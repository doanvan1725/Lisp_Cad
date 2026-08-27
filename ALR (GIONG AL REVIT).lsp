;; ==============================================================================
;;  ALIGN REVIT PRO - FIX TRIỆT ĐỂ LỆCH MÉP BLOCK (100% SÁT MÉP)
;;  Lệnh chạy: 
;;    - ALR    : Dóng 1-Click chuẩn tuyệt đối
;;    - ALRGUI : Bật giao diện điều khiển
;; ==============================================================================

(vl-load-com)

(defun Create_AlignRevit_DCL ( / dclFile f )
  (setq dclFile (vl-filename-mktemp "AlignRevitGUI.dcl"))
  (setq f (open dclFile "w"))
  (write-line "AlignRevitGUI : dialog {" f)
  (write-line "    label = \"Align Revit Pro - Tool Dong Block Auto\";" f)
  (write-line "    :boxed_column {" f)
  (write-line "        label = \"Huong dan su dung\";" f)
  (write-line "        :text { label = \"1. Bam nut 'Bat dau dong' ben duoi.\"; }" f)
  (write-line "        :text { label = \"2. Pick chon CANH MOC (Line/Polyline).\"; }" f)
  (write-line "        :text { label = \"3. Pick chon MEP BLOCK muon hit vao moc!\"; }" f)
  (write-line "    }" f)
  (write-line "    spacer_1;" f)
  (write-line "    :row {" f)
  (write-line "        :button {" f)
  (write-line "            key = \"btn_align\";" f)
  (write-line "            label = \"Bat dau dong (1-Click)\";" f)
  (write-line "            is_default = true;" f)
  (write-line "            width = 22;" f)
  (write-line "        }" f)
  (write-line "        :button {" f)
  (write-line "            key = \"cancel\";" f)
  (write-line "            label = \"Thoat\";" f)
  (write-line "            is_cancel = true;" f)
  (write-line "            width = 12;" f)
  (write-line "        }" f)
  (write-line "    }" f)
  (write-line "}" f)
  (close f)
  dclFile
)

(defun c:ALR ( / *error* doc targetEnt targetEname targetPt targetObj targetAng blkEnt blkObj blkPt snapTargetPt param firstDeriv p1 p2 newBlkPt )
  
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\n[ALR Error]: " msg))
    )
    (vla-EndUndoMark doc)
    (princ)
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  ;; 1. Chọn đường mốc
  (setq targetEnt (entsel "\n1. Chon duong moc / canh moc (Line/Polyline): "))
  (if targetEnt
    (progn
      (setq targetEname (car targetEnt))
      (setq targetPt (cadr targetEnt))
      (setq targetObj (vlax-ename->vla-object targetEname))
      
      ;; Tính góc của đường mốc
      (if (not (vl-catch-all-error-p (setq param (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list targetEname (vlax-curve-getClosestPointTo targetEname targetPt))))))
        (progn
          (setq firstDeriv (vlax-curve-getFirstDeriv targetEname param))
          (setq targetAng (angle '(0 0 0) firstDeriv))
        )
        (cond
          ((= (vla-get-ObjectName targetObj) "AcDbLine")
           (setq p1 (vlax-safearray->list (vlax-variant-value (vla-get-StartPoint targetObj))))
           (setq p2 (vlax-safearray->list (vlax-variant-value (vla-get-EndPoint targetObj))))
           (setq targetAng (angle p1 p2))
          )
          (t (setq targetAng 0.0))
        )
      )

      ;; 2. Chọn điểm mép trên Block
      (setq blkEnt (entsel "\n2. Click vao DUNG MEP CUA BLOCK de hit vao moc: "))
      (if blkEnt
        (progn
          (setq blkPt (cadr blkEnt))
          (setq blkObj (vlax-ename->vla-object (car blkEnt)))

          (if (= (vla-get-ObjectName blkObj) "AcDbBlockReference")
            (progn
              ;; BƯỚC QUAN TRỌNG: Xoay Block trước theo góc mốc
              (vla-put-Rotation blkObj targetAng)

              ;; Sau khi xoay, dùng Osnap 'NEA' lấy lại chính xác điểm mép trên Block đã xoay
              (setq newBlkPt (osnap blkPt "nea"))
              (if (null newBlkPt) (setq newBlkPt blkPt))

              ;; Tính điểm chiếu vuông góc chính xác 100% trên đường mốc
              (if (vl-catch-all-error-p (setq snapTargetPt (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list targetEname newBlkPt))))
                (setq snapTargetPt targetPt)
              )
              
              ;; Di chuyển điểm mép chạm đúng điểm mốc
              (vla-move blkObj (vlax-3d-point newBlkPt) (vlax-3d-point snapTargetPt))
              
              (princ "\n[Thanh cong] Da dong KHANH KHIT mep Block vao duong moc!")
            )
            (princ "\n[Loi] Doi tuong duoc chon khong phai la Block!")
          )
        )
      )
    )
  )

  (vla-EndUndoMark doc)
  (princ)
)

(defun c:ALRGUI ( / dclPath dcl_id status )
  (setq dclPath (Create_AlignRevit_DCL))
  (setq dcl_id (load_dialog dclPath))
  
  (if (not (new_dialog "AlignRevitGUI" dcl_id))
    (progn
      (princ "\n[Loi] Khong the khoi tao giao dien DCL!")
      (exit)
    )
  )

  (action_tile "btn_align" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (setq status (start_dialog))
  (unload_dialog dcl_id)
  
  (if (findfile dclPath) (vl-file-delete dclPath))

  (if (= status 1)
    (c:ALR)
  )
  (princ)
)

(princ "\n>>> DA FIX LOI LECH MEP! Go 'ALR' hoac 'ALRGUI' de dung. <<<")
(princ)