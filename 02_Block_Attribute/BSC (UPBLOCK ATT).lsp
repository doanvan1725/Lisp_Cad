;;; ================================================================
;;; BSC - Dat block ve DUNG ty le nhap vao (tinh tu ty le goc 1:1)
;;; *** BAN DCL ***
;;; Khac lenh SCALE thong thuong:
;;;   - SCALE nhan them ty le hien tai (block dang 2, scale 3 -> thanh 6)
;;;   - BSC dat TUYET DOI ve ty le nhap (block dang 2, nhap 3 -> thanh 3)
;;; Giao dien DCL:
;;;   - O nhap ty le dich (nho lai lan truoc)
;;;   - 2 che do: Quet chon nhieu block / Tat ca block trong ban ve
;;; Dac diem:
;;;   - Scale quanh diem chen tung block -> vi tri chen khong doi
;;;   - Giu nguyen block bi lat guong (mirror, ty le am)
;;;   - Thuoc tinh (ATT) tu scale theo block
;;; Cach dung: go lenh BSC
;;; ================================================================

(vl-load-com)

(if (null *bsc-scale*) (setq *bsc-scale* 1.0))
(if (null *bsc-mode*)  (setq *bsc-mode*  "m_pick"))

;; ---------- Lay dau (+/-) cua 1 so ----------
(defun bsc:sign (v)
  (if (minusp v) -1.0 1.0)
)

;; ---------- Dat ty le tuyet doi cho 1 block, tra ve T neu OK ----------
(defun bsc:set-scale (ent sc / obj rs)
  (setq obj (vlax-ename->vla-object ent))
  (setq rs
    (vl-catch-all-apply
      '(lambda ()
         ;; Giu dau cu de khong pha block dang lat guong
         (vla-put-XScaleFactor obj (* (bsc:sign (vla-get-XScaleFactor obj)) sc))
         (vla-put-YScaleFactor obj (* (bsc:sign (vla-get-YScaleFactor obj)) sc))
         (vla-put-ZScaleFactor obj (* (bsc:sign (vla-get-ZScaleFactor obj)) sc))
       )
    )
  )
  (not (vl-catch-all-error-p rs))
)

;; ---------- Ap ty le cho ca tap chon ----------
(defun bsc:apply (ss sc / i n-ok n-err)
  (setq i 0 n-ok 0 n-err 0)
  (while (< i (sslength ss))
    (if (bsc:set-scale (ssname ss i) sc)
      (setq n-ok (1+ n-ok))
      (setq n-err (1+ n-err))
    )
    (setq i (1+ i))
  )
  (vl-cmdf "_.REGENALL")
  (prompt (strcat "\n==> Da dat " (itoa n-ok) " block ve ty le "
                  (rtos sc 2 4)
                  (if (> n-err 0)
                    (strcat " (" (itoa n-err)
                            " block loi - co the bi khoa layer hoac rang buoc dynamic).")
                    "."
                  )))
)

;; ---------- Tao file DCL tam ----------
(defun bsc:make-dcl (/ fname f)
  (setq fname (vl-filename-mktemp "bsc" nil ".dcl")
        f     (open fname "w"))
  (foreach line
    '("bsc : dialog {"
      "  label = \"BSC - Dat block ve dung ty le\";"
      "  : edit_box {"
      "    key = \"scale\";"
      "    label = \"Ty le dich (so voi goc 1:1) :\";"
      "    edit_width = 12;"
      "  }"
      "  : text { label = \"VD: block dang ty le 2, nhap 3 -> thanh dung 3 (khong nhan don).\"; }"
      "  spacer;"
      "  : boxed_radio_column {"
      "    label = \"Pham vi ap dung\";"
      "    : radio_button { key = \"m_pick\"; label = \"Quet chon nhieu block tren man hinh\"; }"
      "    : radio_button { key = \"m_all\";  label = \"Tat ca block trong ban ve\"; }"
      "  }"
      "  spacer;"
      "  : text { key = \"err\"; label = \"\"; }"
      "  ok_cancel;"
      "}")
    (write-line line f)
  )
  (close f)
  fname
)

;; ================================================================
;; LENH CHINH
;; ================================================================
(defun c:BSC (/ dclfile dclid mode sc scstr ss ret)
  ;; ----- Nap dialog -----
  (setq dclfile (bsc:make-dcl)
        dclid   (load_dialog dclfile))
  (if (not (new_dialog "bsc" dclid))
    (prompt "\n*** Loi: khong mo duoc hop thoai DCL! ***")
    (progn
      ;; Gia tri mac dinh (nho lai lan truoc)
      (setq mode  *bsc-mode*
            scstr (rtos *bsc-scale* 2 4))
      (set_tile "scale" scstr)
      (set_tile mode "1")
      (mode_tile "scale" 2)   ; focus san vao o ty le

      (action_tile "m_pick" "(setq mode \"m_pick\")")
      (action_tile "m_all"  "(setq mode \"m_all\")")
      (action_tile "scale"  "(setq scstr $value)")
      ;; Kiem tra ty le hop le ngay tren hop thoai
      (action_tile "accept"
        (strcat
          "(setq scstr (get_tile \"scale\"))"
          "(if (and (setq sc (distof scstr)) (> sc 0.0))"
          "  (done_dialog 1)"
          "  (set_tile \"err\" \"*** Ty le phai la so > 0! ***\")"
          ")"
        )
      )
      (action_tile "cancel" "(done_dialog 0)")

      (setq ret (start_dialog))
      (unload_dialog dclid)
      (vl-file-delete dclfile)

      ;; ----- Xu ly -----
      (if (= ret 1)
        (progn
          (setq *bsc-scale* sc
                *bsc-mode*  mode)
          (cond
            ;; 1. Quet chon nhieu block tren man hinh
            ((= mode "m_pick")
             (prompt (strcat "\nQuet chon cac block can dat ve ty le "
                             (rtos sc 2 4) ": "))
             (setq ss (ssget '((0 . "INSERT"))))
             (if ss
               (bsc:apply ss sc)
               (prompt "\n*** Khong chon duoc block nao! ***")
             )
            )
            ;; 2. Tat ca block trong ban ve
            ((= mode "m_all")
             (setq ss (ssget "_X" '((0 . "INSERT"))))
             (if ss
               (bsc:apply ss sc)
               (prompt "\n*** Ban ve khong co block nao! ***")
             )
            )
          )
        )
        (prompt "\n* Da huy lenh. *")
      )
    )
  )
  (princ)
)

(prompt "\nDa nap lenh BSC (ban DCL) - Nhap ty le, quet chon block -> dat ve dung ty le do.")
(princ)
