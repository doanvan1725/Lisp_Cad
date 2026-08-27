;;; =====================================================================
;;; BUN.LSP  -  Doi Block Units (don vi cua dinh nghia block)
;;; Version : 1.0  (2026-07-24)
;;; Lenh goi : BUN
;;;
;;; Chuc nang:
;;;   - Liet ke cac block definition trong ban ve kem don vi hien tai
;;;   - Chon 1 hoac nhieu block trong danh sach (multi-select),
;;;     hoac nut Pick de chon block ngoai man hinh
;;;   - Chon don vi dich (Unitless / Millimeters / Meters / Inches...)
;;;   - Apply: doi thuoc tinh Units cua DINH NGHIA block
;;;
;;; QUAN TRONG: thao tac nay KHONG lam thay doi hinh hoc cua block,
;;; KHONG scale cac block da chen trong ban ve. Units chi la "nhan
;;; don vi" duoc CAD dung de tu scale khi CHEN MOI block vao ban ve
;;; co INSUNITS khac (keo tu DesignCenter / tool palette / INSERT).
;;; Doi ve Unitless hoac trung don vi ban ve se het bi tu scale.
;;;
;;; Ghi chu: khong dung dau tieng Viet trong string de tranh loi ANSI.
;;; =====================================================================

(vl-load-com)

(if (not *BUN-Target*) (setq *BUN-Target* 0))   ; don vi dich gan nhat (mac dinh Unitless)

;; ---------------------------------------------------------------------
;; Bang don vi: (ma . ten)  - theo enum INSUNITS cua AutoCAD
;; ---------------------------------------------------------------------
(setq *BUN-UnitTable*
  '((0 . "Unitless")
    (1 . "Inches")
    (2 . "Feet")
    (4 . "Millimeters")
    (5 . "Centimeters")
    (6 . "Meters")
    (7 . "Kilometers")
    (14 . "Decimeters")
   )
)

(defun BUN:UnitName (code / found)
  (setq found (assoc code *BUN-UnitTable*))
  (if found (cdr found) (strcat "Code " (itoa code)))
)

;; ---------------------------------------------------------------------
;; Tien ich chuoi
;; ---------------------------------------------------------------------
(defun BUN:Spaces (n / s)
  (setq s "")
  (repeat (max n 0) (setq s (strcat s " ")))
  s
)

(defun BUN:Pad (str width / s)
  (setq s (if str (vl-princ-to-string str) ""))
  (if (> (strlen s) width)
    (substr s 1 width)
    (strcat s (BUN:Spaces (- width (strlen s))))
  )
)

;; ---------------------------------------------------------------------
;; Lay danh sach block definition: ((ten . unit-code) ...)
;; Bo qua layout, xref, block an danh (*)
;; ---------------------------------------------------------------------
(defun BUN:GetBlocks (/ doc blocks lst nm u chk)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blocks (vla-get-Blocks doc))
  (setq lst '())
  (vlax-for blk blocks
    (setq nm (vla-get-Name blk))
    (if (and (= (vla-get-IsLayout blk) :vlax-false)
             (= (vla-get-IsXRef blk) :vlax-false)
             (/= (substr nm 1 1) "*")
        )
      (progn
        (setq chk (vl-catch-all-apply 'vla-get-Units (list blk)))
        (setq u (if (vl-catch-all-error-p chk) -1 chk))
        (setq lst (cons (cons nm u) lst))
      )
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase (car a)) (strcase (car b)))))
)

;; ---------------------------------------------------------------------
;; Doi Units cua 1 block definition theo ten
;; Tra ve T neu thanh cong
;; ---------------------------------------------------------------------
(defun BUN:SetUnits (blkname target / doc blk chk)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blk (vl-catch-all-apply 'vla-Item
              (list (vla-get-Blocks doc) blkname)))
  (if (vl-catch-all-error-p blk)
    nil
    (progn
      (setq chk (vl-catch-all-apply 'vla-put-Units (list blk target)))
      (not (vl-catch-all-error-p chk))
    )
  )
)

;; ---------------------------------------------------------------------
;; DCL
;; ---------------------------------------------------------------------
(defun BUN:WriteDCL (/ path f)
  (setq path (strcat (getenv "TEMP") "\\bun_" (rtos (getvar "MILLISECS") 2 0) ".dcl"))
  (setq f (open path "w"))
  (write-line "bun_dlg : dialog {" f)
  (write-line "  label = \"BUN v1.0 - Doi Block Units (khong anh huong block da chen)\";" f)
  (write-line "  : list_box {" f)
  (write-line "    key = \"list_blk\";" f)
  (write-line "    label = \"Chon block can doi don vi (giu Ctrl/Shift de chon nhieu):\";" f)
  (write-line "    width = 54;" f)
  (write-line "    height = 15;" f)
  (write-line "    multiple_select = true;" f)
  (write-line "    fixed_width_font = true;" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"btn_pick\"; label = \"Pick block ngoai man hinh <\"; width = 26; }" f)
  (write-line "    : button { key = \"btn_all\"; label = \"Chon tat ca\"; width = 14; }" f)
  (write-line "  }" f)
  (write-line "  : popup_list {" f)
  (write-line "    key = \"list_unit\";" f)
  (write-line "    label = \"Doi thanh don vi:\";" f)
  (write-line "    edit_width = 20;" f)
  (write-line "  }" f)
  (write-line "  : text { label = \"Chi doi NHAN don vi cua dinh nghia block - hinh hoc va\"; }" f)
  (write-line "  : text { label = \"cac block da chen trong ban ve GIU NGUYEN 100%.\"; }" f)
  (write-line "  spacer;" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  path
)

;; ---------------------------------------------------------------------
;; Do danh sach block vao list_box
;; ---------------------------------------------------------------------
(defun BUN:PopList (blks / lst b)
  (setq lst '())
  (foreach b blks
    (setq lst (append lst
      (list (strcat (BUN:Pad (car b) 34) " "
                    (BUN:UnitName (cdr b))))))
  )
  (start_list "list_blk")
  (mapcar 'add_list lst)
  (end_list)
)

;; ---------------------------------------------------------------------
;; Parse chuoi index tu list_box multi-select: "0 2 5" -> (0 2 5)
;; ---------------------------------------------------------------------
(defun BUN:ParseSel (s / res pos tok)
  (setq res '())
  (setq s (vl-string-trim " " s))
  (while (/= s "")
    (setq pos (vl-string-search " " s))
    (if pos
      (progn
        (setq tok (substr s 1 pos))
        (setq s (vl-string-trim " " (substr s (+ pos 2))))
      )
      (progn (setq tok s) (setq s ""))
    )
    (if (/= tok "") (setq res (cons (atoi tok) res)))
  )
  (reverse res)
)

;; ---------------------------------------------------------------------
;; Lenh chinh: BUN
;; ---------------------------------------------------------------------
(defun c:BUN (/ *error* olderr blks selStr selIdx targetIdx unitCodes
                dcl-path dclid code es enm obj nm i ok fail pickName allStr)

  (setq olderr *error*)
  (defun *error* (msg)
    (if (and dclid (> dclid 0)) (unload_dialog dclid))
    (if (and dcl-path (findfile dcl-path)) (vl-file-delete dcl-path))
    (setq *error* olderr)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort"))
      (princ (strcat "\nLoi BUN: " msg))
    )
    (princ)
  )

  (setq code 2 pickName nil)
  (while (or (= code 2) (= code 3))
    (setq dcl-path nil dclid nil)
    (setq blks (BUN:GetBlocks))
    (if (not blks)
      (progn
        (alert "Ban ve khong co block definition nao (ngoai layout/xref/an danh).")
        (setq code 0)
      )
      (progn
        ;; Che do pick ngoai man hinh (tu vong lap truoc)
        (if (= code 3)
          (progn
            (setq es (entsel "\nPick 1 block de chon trong danh sach: "))
            (setq pickName nil)
            (if es
              (progn
                (setq enm (car es))
                (if (= (cdr (assoc 0 (entget enm))) "INSERT")
                  (progn
                    (setq obj (vlax-ename->vla-object enm))
                    (setq pickName
                      (vl-catch-all-apply 'vlax-get-property
                        (list obj 'EffectiveName)))
                    (if (vl-catch-all-error-p pickName) (setq pickName nil))
                  )
                  (princ "\nDoi tuong khong phai block.")
                )
              )
            )
          )
        )
        (setq code 0)

        ;; Danh sach ma don vi theo thu tu bang
        (setq unitCodes (mapcar 'car *BUN-UnitTable*))

        (setq dcl-path (BUN:WriteDCL))
        (setq dclid (load_dialog dcl-path))
        (if (not (new_dialog "bun_dlg" dclid))
          (progn (alert "Khong the tao hop thoai BUN.") (setq code 0))
          (progn
            (BUN:PopList blks)
            ;; danh sach don vi dich
            (start_list "list_unit")
            (mapcar '(lambda (c) (add_list (BUN:UnitName c))) unitCodes)
            (end_list)
            (setq targetIdx
              (cond
                ((vl-position *BUN-Target* unitCodes))
                (0)
              )
            )
            (set_tile "list_unit" (itoa targetIdx))
            ;; neu vua pick -> danh dau san block do
            (setq selStr "")
            (if pickName
              (progn
                (setq i 0)
                (foreach b blks
                  (if (= (strcase (car b)) (strcase pickName))
                    (setq selStr (itoa i))
                  )
                  (setq i (1+ i))
                )
                (if (/= selStr "")
                  (set_tile "list_blk" selStr)
                  (princ (strcat "\nBlock \"" pickName "\" khong co trong danh sach."))
                )
              )
            )
            (action_tile "list_blk" "(setq selStr $value)")
            (action_tile "list_unit" "(setq targetIdx (atoi $value))")
            (action_tile "btn_pick" "(done_dialog 3)")
            (action_tile "btn_all"
              (strcat
                "(setq selStr \""
                (progn
                  (setq allStr "" i 0)
                  (repeat (length blks)
                    (setq allStr (strcat allStr (if (= i 0) "" " ") (itoa i)))
                    (setq i (1+ i))
                  )
                  allStr
                )
                "\")(set_tile \"list_blk\" selStr)"
              )
            )
            (action_tile "accept" "(done_dialog 1)")
            (action_tile "cancel" "(done_dialog 0)")
            (setq code (start_dialog))
            (unload_dialog dclid)
            (setq dclid nil)
            (if (findfile dcl-path) (vl-file-delete dcl-path))
            (setq dcl-path nil)

            (if (= code 1)
              (progn
                (setq selIdx (BUN:ParseSel selStr))
                (if (not selIdx)
                  (progn
                    (alert "Ban chua chon block nao trong danh sach.")
                    (setq code 2)
                  )
                  (progn
                    (setq *BUN-Target* (nth targetIdx unitCodes))
                    (setq ok 0 fail 0)
                    (foreach i selIdx
                      (setq nm (car (nth i blks)))
                      (if (and nm (BUN:SetUnits nm *BUN-Target*))
                        (setq ok (1+ ok))
                        (setq fail (1+ fail))
                      )
                    )
                    (princ (strcat "\nBUN: da doi don vi "
                                   (itoa ok) " block sang "
                                   (BUN:UnitName *BUN-Target*) "."))
                    (if (> fail 0)
                      (princ (strcat "\n(" (itoa fail) " block doi that bai.)"))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (*error* nil)
  (princ)
)

(princ "\nDa nap BUN.LSP v1.0 - Go BUN de doi Block Units (khong anh huong block da chen).")
(princ)
