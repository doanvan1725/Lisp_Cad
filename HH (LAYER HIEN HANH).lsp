;;; ================================================================
;;; HH - Chon 1 doi tuong -> lay layer cua no lam layer hien hanh
;;; - Pick truot (khong trung doi tuong) thi cho pick lai
;;; - Neu layer dang bi tat/dong bang van set duoc (chi bao them)
;;; Cach dung: go lenh HH -> chon doi tuong
;;; ================================================================

(defun c:HH (/ sel ent lyr)
  ;; Cho pick lai neu truot
  (while
    (progn
      (setq sel (entsel "\nChon doi tuong de lay layer lam hien hanh: "))
      (cond
        ((not sel) (prompt "\n* Pick truot hoac da huy. *") nil) ; Enter/Esc -> thoat
        (t nil)
      )
      (and (not sel) (/= (getvar "ERRNO") 52))  ; ERRNO 52 = nguoi dung nhan Enter
    )
  )
  (if sel
    (progn
      (setq ent (car sel)
            lyr (cdr (assoc 8 (entget ent))))
      (if (= (strcase lyr) (strcase (getvar "CLAYER")))
        (prompt (strcat "\n* Layer \"" lyr "\" da la layer hien hanh. *"))
        (progn
          (setvar "CLAYER" lyr)
          (prompt (strcat "\n==> Da chuyen layer hien hanh sang: \"" lyr "\""))
        )
      )
    )
  )
  (princ)
)

(prompt "\nDa nap lenh HH - Chon doi tuong de lay layer cua no lam layer hien hanh.")
(princ)
