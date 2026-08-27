;;; ================================================================
;;; BET - Mo Block Editor (BEDIT) nhanh cho block thuoc tinh
;;; 2 cach dung (trong cung 1 lenh):
;;;   1. Chi thang vao block tren man hinh -> mo BEDIT block do
;;;      (ho tro dynamic block - tu lay EffectiveName)
;;;   2. Nhan Enter (khong chon) -> hien hop thoai danh sach
;;;      cac block thuoc tinh, chon ten (hoac double-click) -> BEDIT
;;; Cach dung: go lenh BET
;;; ================================================================

(vl-load-com)

;; ---------- Lay ten block cua 1 doi tuong (ho tro dynamic) ----------
(defun be:ename->blkname (ent / obj)
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'EffectiveName)
    (vla-get-EffectiveName obj)
    (vla-get-Name obj)
  )
)

;; ---------- Danh sach block thuoc tinh trong bang block ----------
;; Lay tu block table nen liet ke ca block chua duoc chen vao ban ve
(defun be:att-blknames (/ blk name flags lst)
  (while (setq blk (tblnext "BLOCK" (not blk)))
    (setq name  (cdr (assoc 2 blk))
          flags (cdr (assoc 70 blk)))
    (if (and
          (= 2 (logand 2 flags))          ; co thuoc tinh
          (/= (substr name 1 1) "*")      ; bo block an danh / layout
          (= 0 (logand 4 flags))          ; bo xref
        )
      (setq lst (cons name lst))
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Mo Block Editor ----------
(defun be:open-bedit (name)
  (if (= 1 (getvar "BLOCKEDITOR"))
    (prompt "\n*** Dang o trong Block Editor, hay dong lai truoc! ***")
    (progn
      (prompt (strcat "\nMo Block Editor: " name))
      (command "_.-BEDIT" name)
    )
  )
)

;; ---------- Tao file DCL tam ----------
(defun be:make-dcl (/ fname f)
  (setq fname (vl-filename-mktemp "be" nil ".dcl")
        f     (open fname "w"))
  (foreach line
    '("be : dialog {"
      "  label = \"BE - Chon block thuoc tinh de BEDIT\";"
      "  : list_box {"
      "    key = \"blklist\";"
      "    label = \"Danh sach block thuoc tinh (double-click de mo):\";"
      "    width = 42;"
      "    height = 16;"
      "  }"
      "  spacer;"
      "  ok_cancel;"
      "}")
    (write-line line f)
  )
  (close f)
  fname
)

;; ---------- Hop thoai chon block tu danh sach ----------
(defun be:dialog-pick (/ allnames dclfile dclid idx ret)
  (setq allnames (be:att-blknames))
  (if (not allnames)
    (progn
      (prompt "\n*** Ban ve khong co block thuoc tinh nao! ***")
      nil
    )
    (progn
      (setq dclfile (be:make-dcl)
            dclid   (load_dialog dclfile))
      (if (not (new_dialog "be" dclid))
        (progn (prompt "\n*** Loi: khong mo duoc hop thoai DCL! ***") nil)
        (progn
          ;; Nho lai ten block chon lan truoc
          (setq idx (if (and *be-idx* (< *be-idx* (length allnames))) *be-idx* 0))

          (start_list "blklist")
          (mapcar 'add_list allnames)
          (end_list)
          (set_tile "blklist" (itoa idx))

          ;; Click chon / double-click ($reason = 4) mo luon
          (action_tile "blklist"
            "(setq idx (atoi $value)) (if (= $reason 4) (done_dialog 1))")
          (action_tile "accept" "(done_dialog 1)")
          (action_tile "cancel" "(done_dialog 0)")

          (setq ret (start_dialog))
          (unload_dialog dclid)
          (vl-file-delete dclfile)

          (if (= ret 1)
            (progn
              (setq *be-idx* idx)
              (nth idx allnames)        ; tra ve ten block da chon
            )
            nil
          )
        )
      )
    )
  )
)

;; ================================================================
;; LENH CHINH
;; ================================================================
(defun c:BET (/ sel ent edata name)
  (if (= 1 (getvar "BLOCKEDITOR"))
    (prompt "\n*** Dang o trong Block Editor, hay dong lai truoc! ***")
    (progn
      (setq name nil)
      (prompt "\nChi vao block can sua hoac <Enter = chon tu danh sach>: ")
      (setq sel (vl-catch-all-apply 'entsel))

      (cond
        ;; --- Nguoi dung chi vao 1 doi tuong ---
        ((and (not (vl-catch-all-error-p sel)) sel)
         (setq ent   (car sel)
               edata (entget ent))
         (if (= "INSERT" (cdr (assoc 0 edata)))
           (setq name (be:ename->blkname ent))
           (prompt "\n*** Doi tuong vua chon khong phai la block! ***")
         )
        )
        ;; --- Enter / bo qua -> hien danh sach ---
        (t
         (setq name (be:dialog-pick))
        )
      )

      (if name (be:open-bedit name))
    )
  )
  (princ)
)

(prompt "\nDa nap lenh BET - Chi vao block hoac Enter de chon tu danh sach -> mo BEDIT.")
(princ)
