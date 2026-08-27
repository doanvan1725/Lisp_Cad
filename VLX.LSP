(defun c:VLX (/ p1 p2 d s ang h n i x y pt-list local-x local-y global-pt)
  (setq p1 (getpoint "\nChon diem BAT DAU: "))
  (setq p2 (getpoint p1 "\nChon diem CUOI: "))
  (setq d (getdist "\nNhap duong kinh ngoai cua dai (D): "))
  (setq s (getdist "\nNhap buoc vong (Buoc quat p): "))
  
  ; Tinh chieu dai va goc xoay giua 2 diem
  (setq h (distance p1 p2))
  (setq ang (angle p1 p2))
  (setq n (fix (/ h s))) ; Tinh so buoc vong
  
  (command "_.pline")
  (command p1)
  
  (setq i 0)
  (while (< i n)
    ; Tinh diem Zic sang phai (he toa do phat sinh)
    (setq local-x d)
    (setq local-y (+ (* i s) (/ s 2.0)))
    ; Chuyen doi sang toa do thuc cua ban ve (co tinh den goc xoay)
    (setq global-pt (polar (polar p1 ang local-y) (+ ang (/ pi -2.0)) local-x))
    (command global-pt)
    
    ; Tinh diem Zac ve trai
    (setq local-x 0)
    (setq local-y (* (+ i 1) s))
    (command (polar p1 ang local-y))
    
    (setq i (1+ i))
  )
  (command "")
  (princ "\nDa ve xong thep dai xoan theo 2 diem pick!")
  (princ)
)