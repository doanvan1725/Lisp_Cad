;;; =========================================================================
;;; THAYBLOCK.LSP (ban co giao dien)
;;; Lenh: THAYBLOCK  (lenh tat: TBL)
;;;
;;; Quy trinh:
;;;   1) Go TBL -> QUET CHON cac block cu can thay (chon nhieu)
;;;   2) Hop thoai hien ra:
;;;        - Danh sach TAT CA block dong co trong ban ve -> chon block MOI
;;;        - Danh sach tham so dong cua block moi -> chon tham so chieu dai
;;;          can GIU NGUYEN (vd Distance1)
;;;   3) Bam OK -> thay toan bo:
;;;        - Dung vi tri, goc xoay, ty le, layer cua block cu
;;;        - GIU NGUYEN gia tri chieu dai cua block cu
;;;        - Attribute cung Tag (vd NAME) duoc giu lai
;;;        - Cac thu khac theo block moi; block cu bi xoa
;;; =========================================================================

(vl-load-com)

(if (null *tbl-blkname*) (setq *tbl-blkname* ""))
(if (null *tbl-prop*)    (setq *tbl-prop* "Distance1"))

;; ---------- Lay gia tri Dynamic Property theo ten ----------
(defun TBL:GetDynProp (blkObj propName / props i res)
  (setq res nil)
  (if (and (vlax-property-available-p blkObj 'IsDynamicBlock)
           (eq (vla-get-IsDynamicBlock blkObj) :vlax-true))
    (progn
      (setq props (vlax-invoke blkObj 'GetDynamicBlockProperties))
      (setq i 0)
      (while (and (< i (length props)) (not res))
        (if (= (strcase (vla-get-PropertyName (nth i props))) (strcase propName))
          (setq res (vla-get-Value (nth i props)))
        )
        (setq i (1+ i))
      )
      (if (and res (= (type res) 'VARIANT))
        (setq res (vlax-variant-value res))
      )
    )
  )
  res
)

;; ---------- Gan gia tri Dynamic Property theo ten ----------
(defun TBL:SetDynProp (blkObj propName val / props i done)
  (setq done nil)
  (if (and (vlax-property-available-p blkObj 'IsDynamicBlock)
           (eq (vla-get-IsDynamicBlock blkObj) :vlax-true))
    (progn
      (setq props (vlax-invoke blkObj 'GetDynamicBlockProperties))
      (setq i 0)
      (while (and (< i (length props)) (not done))
        (if (= (strcase (vla-get-PropertyName (nth i props))) (strcase propName))
          (progn
            (vl-catch-all-apply
              'vla-put-Value
              (list (nth i props)
                    (vlax-make-variant (float val) vlax-vbDouble))
            )
            (setq done t)
          )
        )
        (setq i (1+ i))
      )
    )
  )
  done
)

;; ---------- Liet ke ten tham so dong (bo qua Origin) ----------
(defun TBL:ListDynProps (blkObj / res nm)
  (setq res nil)
  (if (and (vlax-property-available-p blkObj 'IsDynamicBlock)
           (eq (vla-get-IsDynamicBlock blkObj) :vlax-true))
    (foreach p (vlax-invoke blkObj 'GetDynamicBlockProperties)
      (setq nm (vla-get-PropertyName p))
      (if (/= (strcase nm) "ORIGIN")
        (setq res (cons nm res))
      )
    )
  )
  (reverse res)
)

;; ---------- Ten goc block (ke ca block dong *U...) ----------
(defun TBL:EffName (blkObj)
  (if (vlax-property-available-p blkObj 'EffectiveName)
    (vla-get-EffectiveName blkObj)
    (vla-get-Name blkObj)
  )
)

;; ---------- Doc / gan Attribute ----------
(defun TBL:GetAttList (blkObj / res)
  (setq res nil)
  (if (and (vlax-property-available-p blkObj 'HasAttributes)
           (eq (vla-get-HasAttributes blkObj) :vlax-true))
    (foreach a (vlax-invoke blkObj 'GetAttributes)
      (setq res (cons (cons (strcase (vla-get-TagString a))
                            (vla-get-TextString a))
                      res))
    )
  )
  (reverse res)
)

(defun TBL:PutAttList (blkObj attList / pair)
  (if (and (vlax-property-available-p blkObj 'HasAttributes)
           (eq (vla-get-HasAttributes blkObj) :vlax-true))
    (foreach a (vlax-invoke blkObj 'GetAttributes)
      (setq pair (assoc (strcase (vla-get-TagString a)) attList))
      (if pair (vla-put-TextString a (cdr pair)))
    )
  )
)

;; ---------- Quet toan ban ve, lap danh sach block dong ----------
;; Tra ve list: ((tenblock . ename-tham-chieu) ...) sap xep theo ten
(defun TBL:CollectDynBlocks (/ ss i ent obj nm res)
  (setq res nil)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq i (1+ i))
        (setq obj (vlax-ename->vla-object ent))
        (if (and (vlax-property-available-p obj 'IsDynamicBlock)
                 (eq (vla-get-IsDynamicBlock obj) :vlax-true))
          (progn
            (setq nm (TBL:EffName obj))
            (if (not (assoc nm res))
              (setq res (cons (cons nm ent) res))
            )
          )
        )
      )
    )
  )
  (vl-sort res (function (lambda (a b) (< (strcase (car a)) (strcase (car b))))))
)

;; ---------- DCL ----------
(defun TBL:WriteDCL ( / f fn)
  (setq fn (vl-filename-mktemp "thayblock" nil ".dcl"))
  (setq f (open fn "w"))
  (write-line "thayblock : dialog {" f)
  (write-line "  label = \"Thay block dong - giu nguyen chieu dai\";" f)
  (write-line "  : list_box {" f)
  (write-line "    key = \"blist\";" f)
  (write-line "    label = \"Cac block dong co trong ban ve (chon block MOI de thay vao):\";" f)
  (write-line "    height = 14; width = 46;" f)
  (write-line "  }" f)
  (write-line "  : popup_list {" f)
  (write-line "    key = \"plist\";" f)
  (write-line "    label = \"Tham so chieu dai GIU NGUYEN:\";" f)
  (write-line "    width = 40;" f)
  (write-line "  }" f)
  (write-line "  : text { key = \"info\"; label = \"\"; width = 46; }" f)
  (write-line "  spacer;" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  fn
)

;; ---------- Do danh sach tham so cua block dang chon vao popup ----------
(defun TBL:FillProps (idx / pair obj)
  (setq *tbl-curprops* nil)
  (if (and idx (>= idx 0) (< idx (length *tbl-dynlist*)))
    (progn
      (setq pair (nth idx *tbl-dynlist*))
      (setq obj (vlax-ename->vla-object (cdr pair)))
      (setq *tbl-curprops* (TBL:ListDynProps obj))
    )
  )
  (start_list "plist")
  (if *tbl-curprops*
    (foreach p *tbl-curprops* (add_list p))
    (add_list "(khong co tham so)")
  )
  (end_list)
  ;; Chon lai tham so da dung lan truoc neu co
  (if (and *tbl-curprops* (member *tbl-prop* *tbl-curprops*))
    (set_tile "plist"
      (itoa (- (length *tbl-curprops*)
               (length (member *tbl-prop* *tbl-curprops*)))))
    (set_tile "plist" "0")
  )
)

;; =========================================================================
;; LENH CHINH
;; =========================================================================
(defun c:THAYBLOCK (/ doc ss dclF dclId res idx pidx
                      newName newRefEnt propName
                      i entOld objOld oldName owner
                      insPt rot sx sy sz layer lenVal attList
                      objIns cnt skip startIdx)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; ----- 1. Quet chon cac block cu can thay -----
  (princ "\nQuet chon cac block CU can thay (chon nhieu duoc): ")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (null ss) (progn (princ "\nKhong chon block nao.") (exit)))
  (princ (strcat "\nDa chon " (itoa (sslength ss)) " block."))

  ;; ----- 2. Lap danh sach block dong trong ban ve -----
  (setq *tbl-dynlist* (TBL:CollectDynBlocks))
  (if (null *tbl-dynlist*)
    (progn (princ "\nBan ve khong co block dong nao!") (exit))
  )

  ;; ----- 3. Hop thoai chon block moi + tham so -----
  (setq dclF (TBL:WriteDCL))
  (setq dclId (load_dialog dclF))
  (if (not (new_dialog "thayblock" dclId))
    (progn (princ "\nKhong mo duoc hop thoai!") (exit))
  )

  (start_list "blist")
  (foreach pair *tbl-dynlist* (add_list (car pair)))
  (end_list)

  ;; Chon lai block da dung lan truoc, khong co thi chon dong dau
  (setq startIdx 0)
  (if (assoc *tbl-blkname* *tbl-dynlist*)
    (setq startIdx (- (length *tbl-dynlist*)
                      (length (member (assoc *tbl-blkname* *tbl-dynlist*)
                                      *tbl-dynlist*))))
  )
  (set_tile "blist" (itoa startIdx))
  (TBL:FillProps startIdx)
  (set_tile "info" (strcat "Se thay " (itoa (sslength ss)) " block da quet chon."))

  (action_tile "blist" "(TBL:FillProps (atoi $value))")
  (action_tile "accept"
    "(setq idx (atoi (get_tile \"blist\")) pidx (atoi (get_tile \"plist\")) res 1)(done_dialog 1)")
  (action_tile "cancel" "(setq res nil)(done_dialog 0)")
  (start_dialog)
  (unload_dialog dclId)
  (vl-file-delete dclF)

  (if (not res) (progn (princ "\nDa huy.") (exit)))

  ;; ----- 4. Doc lua chon -----
  (setq newName   (car (nth idx *tbl-dynlist*)))
  (setq newRefEnt (cdr (nth idx *tbl-dynlist*)))
  (setq propName
    (if (and *tbl-curprops* (< pidx (length *tbl-curprops*)))
      (nth pidx *tbl-curprops*)
      *tbl-prop*
    )
  )
  (setq *tbl-blkname* newName
        *tbl-prop*    propName
  )
  (princ (strcat "\n-> Thay bang block: " newName
                 " | Giu nguyen tham so: " propName))

  ;; ----- 5. Thay tung block -----
  (setq cnt 0 skip 0 i 0)
  (repeat (sslength ss)
    (setq entOld (ssname ss i))
    (setq i (1+ i))
    (setq objOld (vlax-ename->vla-object entOld))
    (setq oldName (TBL:EffName objOld))

    ;; Bo qua block da cung loai voi block moi
    (if (= (strcase oldName) (strcase newName))
      (setq skip (1+ skip))
      (progn
        (setq insPt  (vlax-get objOld 'InsertionPoint)
              rot    (vla-get-Rotation objOld)
              layer  (vla-get-Layer objOld)
              lenVal (TBL:GetDynProp objOld propName)
              attList (TBL:GetAttList objOld)
        )
        (setq sx (vl-catch-all-apply 'vla-get-XScaleFactor (list objOld)))
        (if (vl-catch-all-error-p sx) (setq sx 1.0))
        (setq sy (vl-catch-all-apply 'vla-get-YScaleFactor (list objOld)))
        (if (vl-catch-all-error-p sy) (setq sy sx))
        (setq sz (vl-catch-all-apply 'vla-get-ZScaleFactor (list objOld)))
        (if (vl-catch-all-error-p sz) (setq sz sx))

        (setq owner (vla-ObjectIDToObject doc (vla-get-OwnerID objOld)))
        (setq objIns
          (vl-catch-all-apply
            'vla-InsertBlock
            (list owner (vlax-3d-point insPt) newName sx sy sz rot)
          )
        )
        (if (vl-catch-all-error-p objIns)
          (progn
            (princ (strcat "\n!! Khong chen duoc block moi: "
                           (vl-catch-all-error-message objIns)))
            (setq skip (1+ skip))
          )
          (progn
            (vla-put-Layer objIns layer)
            (if (numberp lenVal)
              (if (not (TBL:SetDynProp objIns propName lenVal))
                (princ (strcat "\n!! Block moi khong co tham so \"" propName "\"."))
              )
              (princ (strcat "\n!! 1 block cu khong co tham so \"" propName
                             "\" - block moi giu chieu dai mac dinh."))
            )
            (if attList (TBL:PutAttList objIns attList))
            (vla-Update objIns)
            (vla-Delete objOld)
            (setq cnt (1+ cnt))
          )
        )
      )
    )
  )

  (princ (strcat "\nHoan thanh: da thay " (itoa cnt) " block"
                 (if (> skip 0) (strcat ", bo qua " (itoa skip)) "")
                 ". Block moi: " newName
                 " | Tham so giu nguyen: " propName))
  (princ)
)

(defun c:TBL () (c:THAYBLOCK))

(princ "\n=== THAYBLOCK (GUI) da nap. Go TBL: quet chon block cu -> chon block moi trong danh sach -> OK. ===")
(princ)