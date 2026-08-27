;;; ============================================================
;;; KVP - KHOA / MO KHOA VIEWPORT THEO LAYOUT (giao dien Visual LISP)
;;;
;;; - Liet ke tat ca Layout trong ban ve (tru Model), cho phep chon
;;;   NHIEU layout cung luc (list_box multiple_select).
;;; - Chon Khoa (Lock) hoac Mo khoa (Unlock) toan bo viewport cua
;;;   (cac) layout da chon, roi bam THUC HIEN.
;;; - Dung truc tiep property DisplayLocked cua tung Viewport (giong
;;;   het lenh VPLOCK cua AutoCAD) - KHONG can chuyen qua tung tab
;;;   Layout, xu ly thang tren du lieu, nhanh cho ban ve nhieu layout.
;;; - Tu dong bo qua Viewport ID = 1 (viewport "nen" dai dien cho
;;;   chinh Paper Space, khong phai 1 khung nhin thuc su).
;;; - Nho lai lua chon layout + che do Khoa/Mo khoa giua cac lan chay
;;;   trong cung 1 phien lam viec.
;;;
;;; Lenh: KVP
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Cai dat nho giua cac lan chay trong phien
;; ------------------------------------------------------------
(if (not *KVP:Set*) (setq *KVP:Set* '(("lock" . "1") ("sel" . ""))))
(defun KVP:Get (k) (cdr (assoc k *KVP:Set*)))
(defun KVP:Put (k v)
  (setq *KVP:Set* (subst (cons k v) (assoc k *KVP:Set*) *KVP:Set*)))

;; ------------------------------------------------------------
;; Danh sach ten Layout (tru Model), sap theo dung thu tu Tab
;; ------------------------------------------------------------
(defun KVP:LayoutList (/ doc layouts lay out)
  (setq out '())
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layouts (vla-get-layouts doc))
  (vlax-for lay layouts
    (if (/= (strcase (vla-get-name lay)) "MODEL")
      (setq out (cons (cons (vla-get-name lay) (vla-get-taborder lay)) out))
    )
  )
  (setq out (vl-sort out '(lambda (a b) (< (cdr a) (cdr b)))))
  (mapcar 'car out)
)

;; ------------------------------------------------------------
;; Khoa/Mo khoa TAT CA viewport (AcDbViewport) trong 1 Layout.
;; Khoa CA viewport "nen" dai dien Paper Space (thuong dang TAT/
;; khong hien thi nen khong anh huong gi) - vi ID cua no KHONG
;; luon luon = 1 trong moi truong hop (co the lech sau khi Copy/
;; Move layout), neu loai tru theo ID se de sot viewport that.
;; Tra ve so viewport da xu ly thanh cong.
;; ------------------------------------------------------------
(defun KVP:ProcessLayout (layoutName lockflag / doc layouts lay blk cnt r)
  (setq cnt 0)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layouts (vla-get-layouts doc))
  (setq lay (vl-catch-all-apply 'vla-item (list layouts layoutName)))
  (if (not (vl-catch-all-error-p lay))
    (progn
      (setq blk (vla-get-block lay))
      (vlax-for e blk
        (if (and (vlax-property-available-p e 'objectname)
                 (= (vla-get-objectname e) "AcDbViewport"))
          (progn
            (setq r (vl-catch-all-apply 'vla-put-displaylocked (list e lockflag)))
            (if (not (vl-catch-all-error-p r)) (setq cnt (1+ cnt)))
          )
        )
      )
    )
  )
  cnt
)

;; ------------------------------------------------------------
;; Chuoi "0 1 2 ... n-1" (chon tat ca) danh cho list_box
;; ------------------------------------------------------------
(defun KVP:AllIdxStr (n / i s)
  (setq s "" i 0)
  (repeat n (setq s (strcat s (itoa i) " ")) (setq i (1+ i)))
  s
)

;; Chuoi chi so tra ve tu list_box ("0 2 5") -> list so nguyen
(defun KVP:ParseIdx (selstr)
  (if (or (not selstr) (= selstr "")) nil (read (strcat "(" selstr ")")))
)

;; ------------------------------------------------------------
;; Thuc hien Khoa/Mo khoa theo lua chon hien tai trong hop thoai,
;; roi cap nhat dong trang thai - KHONG dong hop thoai.
;; ------------------------------------------------------------
(defun KVP:DoApply (/ selstr lockmode idxs lockflag totalvp totallayout nm cnt)
  (setq selstr (get_tile "layoutlist"))
  (KVP:Put "sel" selstr)
  (setq lockmode (if (= (get_tile "rlock") "1") "1" "0"))
  (KVP:Put "lock" lockmode)
  (setq idxs (KVP:ParseIdx selstr))
  (if (not idxs)
    (set_tile "status" "*** Chua chon Layout nao! ***")
    (progn
      (setq lockflag (if (= lockmode "1") :vlax-true :vlax-false))
      (setq totalvp 0 totallayout 0)
      (foreach i idxs
        (setq nm (nth i *KVP:Names*))
        (if nm
          (progn
            (setq cnt (KVP:ProcessLayout nm lockflag))
            (setq totalvp (+ totalvp cnt))
            (if (> cnt 0) (setq totallayout (1+ totallayout)))
          )
        )
      )
      (command "_.REGEN")
      (set_tile "status"
        (strcat (if (= lockmode "1") "Da KHOA " "Da MO KHOA ")
                (itoa totalvp) " viewport / " (itoa (length idxs)) " layout da chon."))
    )
  )
  (princ)
)

;; ------------------------------------------------------------
;; Sinh file DCL tam
;; ------------------------------------------------------------
(defun KVP:MakeDCL (/ fname f)
  (setq fname (vl-filename-mktemp "kvp" nil ".dcl"))
  (setq f (open fname "w"))
  (foreach s
   '("kvp : dialog {"
     "  label = \"KHOA / MO KHOA VIEWPORT THEO LAYOUT\";"
     "  : boxed_column {"
     "    label = \"1. Chon Layout (co the chon nhieu)\";"
     "    : list_box { key = \"layoutlist\"; multiple_select = true; height = 14; width = 36; }"
     "    : row {"
     "      : button { key = \"selall\";  label = \"Chon tat\"; width = 14; }"
     "      : button { key = \"selnone\"; label = \"Bo chon\";  width = 14; }"
     "    }"
     "  }"
     "  : boxed_row {"
     "    label = \"2. Thao tac\";"
     "    : radio_column {"
     "      : radio_button { key = \"rlock\";   label = \"Khoa (Lock) tat ca viewport\"; }"
     "      : radio_button { key = \"runlock\"; label = \"Mo khoa (Unlock) tat ca viewport\"; }"
     "    }"
     "  }"
     "  : text { key = \"status\"; label = \" \"; width = 44; }"
     "  spacer;"
     "  : row {"
     "    : button { key = \"apply\"; label = \"<<  THUC HIEN  >>\"; fixed_width = false; }"
     "    : button { key = \"close\"; label = \"Dong\"; is_cancel = true; width = 12; }"
     "  }"
     "}")
    (write-line s f)
  )
  (close f)
  fname
)

;; ============================================================
;; LENH CHINH
;; ============================================================
(defun C:KVP (/ dclfile dclid selstr)
  (if (not *KVP:Set*) (setq *KVP:Set* '(("lock" . "1") ("sel" . ""))))
  (setq *KVP:Names* (KVP:LayoutList))
  (if (not *KVP:Names*)
    (alert "Ban ve khong co Layout nao (ngoai Model) de khoa viewport!")
    (progn
      (setq dclfile (KVP:MakeDCL))
      (setq dclid (load_dialog dclfile))
      (if (< dclid 0)
        (alert "Khong the tao hop thoai DCL!")
        (progn
          (if (not (new_dialog "kvp" dclid))
            (alert "Khong the mo hop thoai!")
            (progn
              ;; --- nap du lieu ---
              (start_list "layoutlist")
              (mapcar 'add_list *KVP:Names*)
              (end_list)
              (setq selstr (KVP:Get "sel"))
              (if (or (not selstr) (= selstr ""))
                (setq selstr (KVP:AllIdxStr (length *KVP:Names*))))
              (set_tile "layoutlist" selstr)
              (set_tile (if (= (KVP:Get "lock") "1") "rlock" "runlock") "1")
              (set_tile "status" " ")

              ;; --- su kien ---
              (action_tile "selall"
                "(set_tile \"layoutlist\" (KVP:AllIdxStr (length *KVP:Names*)))")
              (action_tile "selnone" "(set_tile \"layoutlist\" \"\")")
              (action_tile "apply" "(KVP:DoApply)")

              (start_dialog)
            )
          )
        )
      )
      (unload_dialog dclid)
      (vl-file-delete dclfile)
    )
  )
  (princ)
)

(princ "\n>> Da tai KVP - Khoa/Mo khoa viewport theo Layout. Go KVP de bat dau. <<")
(princ)
