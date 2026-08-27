;;; DIMAT.LSP - Sua dim thanh dang n@spacing=total
;;; Lenh: BT (nho gia tri KC, tu lam tron so khoang)
(defun C:BT (/ dcl_id kc kcdef ss i ent ed dval n newtxt tmp f)
  ;; --- Lay gia tri lan truoc (mac dinh 3000) ---
  (setq kcdef (getenv "DIMAT_KC"))
  (if (or (not kcdef) (= kcdef "")) (setq kcdef "3000"))

  ;; --- Tao file DCL tam ---
  (setq tmp (vl-filename-mktemp "dimat" nil ".dcl"))
  (setq f (open tmp "w"))
  (write-line "dimat : dialog { label = \"Sua Dim n@KC=Tong\";" f)
  (write-line (strcat "  : edit_box { label = \"Khoang cach (KC):\"; key = \"kc\"; edit_width = 10; value = \"" kcdef "\"; }") f)
  (write-line "  ok_cancel; }" f)
  (close f)

  (setq dcl_id (load_dialog tmp))
  (if (not (new_dialog "dimat" dcl_id)) (exit))
  (setq kc kcdef)
  (action_tile "kc" "(setq kc (get_tile \"kc\"))")
  (action_tile "accept" "(setq kc (get_tile \"kc\")) (done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (if (= (start_dialog) 1)
    (progn
      (unload_dialog dcl_id)
      (vl-file-delete tmp)
      (if (<= (atof kc) 0)
        (princ "\nKhoang cach khong hop le!")
        (progn
          (setenv "DIMAT_KC" kc)
          (setq kc (atof kc))
          (princ "\nChon cac Dimension can sua: ")
          (setq ss (ssget '((0 . "DIMENSION"))))
          (if ss
            (progn
              (setq i 0)
              (repeat (sslength ss)
                (setq ent (ssname ss i)
                      ed  (entget ent)
                      dval (cdr (assoc 42 ed)))
                (setq dval (atof (rtos dval 2 0)))
                ;; lam tron so khoang ve so nguyen gan nhat, toi thieu 1
                (setq n (fix (+ (/ dval kc) 0.5)))
                (if (< n 1) (setq n 1))
                (setq newtxt (strcat (itoa n) "@"
                                     (rtos kc 2 0) "="
                                     (rtos dval 2 0)))
                (entmod (subst (cons 1 newtxt) (assoc 1 ed) ed))
                (entupd ent)
                (setq i (1+ i)))
              (princ (strcat "\nDa xu ly " (itoa (sslength ss)) " dim.")))
            (princ "\nKhong chon dim nao."))))
      )
    (progn (unload_dialog dcl_id) (vl-file-delete tmp)))
  (princ))
(princ "\nGo BT de chay.")
(princ)