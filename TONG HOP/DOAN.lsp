;;; ===========================================================================
;;;  DOAN.LSP  -  BO LISP TONG HOP
;;;  Tac gia   : NVDOAN
;;;  Ngay gop  : 27/07/2026
;;;  Noi dung  : Gop 28 file LISP don le trong thu muc "02. MOI" thanh 1 file.
;;;              Cac file LISP don le van duoc GIU NGUYEN, khong bi thay doi.
;;;
;;;  CACH DUNG : APPLOAD file DOAN.lsp (hoac them vao Startup Suite)
;;;              -> co day du cac lenh ben duoi, KHONG can load tung file.
;;;              LUU Y: da load DOAN.lsp thi khong load lai cac file le,
;;;              tranh nap trung dinh nghia.
;;;
;;;  DANH SACH LENH
;;; ---------------------------------------------------------------------------
;;;  NHOM 1 - TIEN ICH CHUNG
;;;      VLX                    : Ve thep dai xoan (vong lo xo) theo 2 diem pick
;;;      HH                     : Chon doi tuong -> lay layer cua no lam layer hien hanh
;;;      BT                     : Sua Dimension thanh dang n@KC=Tong
;;;      UPCHU                  : Up ty le co chu cho Text/Mtext/Dim/Leader/Block
;;;
;;;  NHOM 2 - TEXT & KY TU
;;;      VTT / CVT              : Viet chu nhanh / chuyen doi text
;;;      KTD                    : Chen ky tu Hy Lap va ky tu dac biet
;;;      GOM / GM               : Gom nhieu Text roi thanh 1 MText
;;;
;;;  NHOM 3 - BLOCK: CHEN / VE / THAY THE
;;;      LDB                    : Load toan bo block tu 1 file DWG chi dinh vao ban ve
;;;      BSC                    : Dat block ve dung ty le (co hop thoai)
;;;      ALR / ALRGUI           : Align doi tuong kieu Revit (fix lech mep)
;;;      DBL                    : Chen block hang loat theo toa do tu file
;;;      DYN                    : Ve block dong theo tuyen, co khung xem truoc block
;;;      TBL / THAYBLOCK        : Thay the block cu bang block moi, giu Distance1 (v3.2 - co xem truoc)
;;;
;;;  NHOM 4 - ATTRIBUTE
;;;      TAT                    : Them Attribute (Name / LKDV / Distance1) vao block
;;;      ATTS                   : Dong bo Attribute, nhan ca block chua co att
;;;      BET                    : Sua nhanh Attribute cua block
;;;      SYNCATT / CPATT        : Copy block va tang dan gia tri Attribute
;;;      DTEN                   : Danh so / danh ten Attribute hang loat theo thu tu
;;;      EDA                    : Sua nhanh Name va Distance1 cua block dong
;;;      BUN                    : Doi don vi (Block Units) cua block
;;;
;;;  NHOM 5 - TOA DO / CAO DO / LY TRINH
;;;      DTD / DTDMOVE / DTDCOPY : Danh toa do cho block va diem, tu cap nhat khi REGEN
;;;      CD                     : Danh cao do tuong doi
;;;      TDD                    : Danh ly trinh / cao do tuong doi cho block
;;;      MC                     : Danh ky hieu mat cat (v1.6 - danh sach block ATT MATCAT + xem truoc)
;;;
;;;  NHOM 6 - LAYOUT / VIEWPORT
;;;      SMV                    : Tao Mview theo ty le nhap tu Model
;;;      KVP                    : Khoa / mo khoa viewport theo tung Layout
;;;
;;;  NHOM 7 - GHI CHU & THONG KE KHOI LUONG
;;;      TAG / TAGUPDATE        : Tag ghi chu block, lay Distance1 tu parameter dong hoac ATT
;;;      THKL                   : Tong hop / thong ke khoi luong theo block, xuat CSV
;;;
;;;  GHI CHU KHI GOP
;;; ---------------------------------------------------------------------------
;;;   - File TBV.lsp trong thu muc la BAN CU cua ThayBlock, dinh nghia trung
;;;     hoan toan lenh TBL/THAYBLOCK va cac ham TBL:* voi ban v3.2.
;;;     -> DOAN.lsp chi lay ban MOI (ThayBlock(TBL).lsp v3.2).
;;;     -> File TBV.lsp goc van con nguyen trong thu muc, muon dung ban cu
;;;        thi APPLOAD rieng file do (se ghi de lenh TBL).
;;;   - Toan bo code cua tung file duoc giu NGUYEN VAN, khong sua doi.
;;;   - Cac bien toan cuc cua tung LISP deu co tien to rieng (*tag-, *mc-, ...)
;;;     nen khong xung dot voi nhau.
;;; ===========================================================================

(vl-load-com)


;;; ###########################################################################
;;; ##  NHOM 1 - TIEN ICH CHUNG
;;; ###########################################################################

;;; ---------------------------------------------------------------------------
;;; [01] VLX
;;;      Ve thep dai xoan (vong lo xo) theo 2 diem pick
;;;      Nguon: VLX.LSP
;;; ---------------------------------------------------------------------------
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

;;; --- HET [01] VLX ---

;;; ---------------------------------------------------------------------------
;;; [02] HH
;;;      Chon doi tuong -> lay layer cua no lam layer hien hanh
;;;      Nguon: HH (LAYER HIEN HANH).lsp
;;; ---------------------------------------------------------------------------
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

;;; --- HET [02] HH ---

;;; ---------------------------------------------------------------------------
;;; [03] BT
;;;      Sua Dimension thanh dang n@KC=Tong
;;;      Nguon: BT.lsp
;;; ---------------------------------------------------------------------------
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

;;; --- HET [03] BT ---

;;; ---------------------------------------------------------------------------
;;; [04] UPCHU
;;;      Up ty le co chu cho Text/Mtext/Dim/Leader/Block
;;;      Nguon: UPCHU.lsp
;;; ---------------------------------------------------------------------------
;;; =================================================================
;;; UPCHU.lsp - UP TY LE CO CHU TRONG AUTOCAD (co giao dien)
;;; -----------------------------------------------------------------
;;; Lenh goi: UPCHU
;;; Cong thuc: Co chu moi = Co chu co ban x Ty le up
;;; Ho tro cac loai chu:
;;;   - TEXT (Dtext), MTEXT
;;;   - ATTDEF (dinh nghia thuoc tinh)
;;;   - Thuoc tinh trong block (INSERT co attribute)
;;;   - DIMENSION (chieu cao chu kich thuoc)
;;;   - MULTILEADER (chu tren mleader)
;;; Cho phep chon nhieu doi tuong cung luc; nho gia tri nhap lan truoc.
;;; Cai dat: APPLOAD -> chon file nay (them vao Startup Suite de tu nap)
;;; =================================================================

(vl-load-com)

;; ---------- Bo loc doi tuong chu ----------
(setq *upchu:filter* '((0 . "TEXT,MTEXT,ATTDEF,DIMENSION,MULTILEADER,INSERT")))

;; ---------- Tao file DCL tam ----------
(defun upchu:makedcl (/ fn f)
  (setq fn (vl-filename-mktemp "upchu" nil ".dcl")
        f  (open fn "w"))
  (foreach s
    '("upchu : dialog {"
      "  label = \"UP TY LE CO CHU\";"
      "  spacer;"
      "  : boxed_column { label = \"Thong so\";"
      "    : edit_box { key = \"base\";  label = \"Co chu co ban :\"; edit_width = 12; }"
      "    : edit_box { key = \"scale\"; label = \"Ty le up :\";      edit_width = 12; }"
      "    spacer;"
      "    : text { key = \"result\"; label = \"Co chu sau up : \"; width = 38; }"
      "  }"
      "  spacer;"
      "  : boxed_column { label = \"Doi tuong\";"
      "    : button { key = \"pick\"; label = \"< Chon chu tren man hinh >\"; }"
      "    : text { key = \"count\"; label = \"Da chon: 0 doi tuong\"; width = 38; }"
      "  }"
      "  spacer;"
      "  : row {"
      "    : button { key = \"accept\"; label = \"Ap dung\"; is_default = true; }"
      "    : button { key = \"cancel\"; label = \"Thoat\";   is_cancel  = true; }"
      "  }"
      "}")
    (write-line s f))
  (close f)
  fn)

;; ---------- Cap nhat dong "Co chu sau up" tren hop thoai ----------
(defun upchu:update (/ b s)
  (setq b (distof (get_tile "base"))
        s (distof (get_tile "scale")))
  (if (and b s (> b 0) (> s 0))
    (set_tile "result" (strcat "Co chu sau up : " (rtos (* b s) 2 4)))
    (set_tile "result" "Co chu sau up : (nhap so > 0)")))

;; ---------- Gan chieu cao moi cho 1 doi tuong, tra ve so chu da sua ----------
(defun upchu:seth (obj h / cnt)
  (setq cnt 0)
  (cond
    ;; TEXT / MTEXT / ATTDEF / ATTRIB dung thuoc tinh Height
    ((vlax-property-available-p obj 'Height T)
     (if (not (vl-catch-all-error-p
                (vl-catch-all-apply 'vla-put-Height (list obj h))))
         (setq cnt 1)))
    ;; DIMENSION / MULTILEADER dung thuoc tinh TextHeight
    ((vlax-property-available-p obj 'TextHeight T)
     (if (not (vl-catch-all-error-p
                (vl-catch-all-apply 'vla-put-TextHeight (list obj h))))
         (setq cnt 1))))
  ;; Block co thuoc tinh: sua tung attribute ben trong
  (if (and (= (vla-get-ObjectName obj) "AcDbBlockReference")
           (= (vla-get-HasAttributes obj) :vlax-true))
    (foreach a (vlax-invoke obj 'GetAttributes)
      (if (not (vl-catch-all-error-p
                 (vl-catch-all-apply 'vla-put-Height (list a h))))
          (setq cnt (1+ cnt)))))
  cnt)

;; ---------- Lenh chinh ----------
(defun C:UPCHU (/ doc dcl id done ss base scale newh n i obj)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Gia tri mac dinh = lan nhap truoc (bien toan cuc), lan dau: 2.5 va 1.0
  (setq base  (cond (*upchu:base*)  (2.5))
        scale (cond (*upchu:scale*) (1.0)))

  ;; Neu dang co doi tuong chon san (pickfirst) thi lay luon
  (setq ss (ssget "_I" *upchu:filter*))
  (if ss (sssetfirst nil nil))

  (setq dcl (upchu:makedcl)
        done 2)

  ;; Vong lap hop thoai: 1 = Ap dung, 0 = Thoat, 2 = tam dong de chon chu
  (while (= done 2)
    (setq id (load_dialog dcl))
    (if (not (new_dialog "upchu" id))
      (progn (princ "\nKhong mo duoc hop thoai DCL!") (setq done 0))
      (progn
        (set_tile "base"  (rtos base 2 4))
        (set_tile "scale" (rtos scale 2 4))
        (set_tile "count"
          (strcat "Da chon: " (itoa (if ss (sslength ss) 0)) " doi tuong"))
        (upchu:update)
        (action_tile "base"
          "(setq base (distof (get_tile \"base\"))) (upchu:update)")
        (action_tile "scale"
          "(setq scale (distof (get_tile \"scale\"))) (upchu:update)")
        (action_tile "pick" "(done_dialog 2)")
        (action_tile "accept"
          (strcat
            "(setq base (distof (get_tile \"base\"))"
            "      scale (distof (get_tile \"scale\")))"
            "(if (and base scale (> base 0) (> scale 0))"
            "    (if ss (done_dialog 1)"
            "           (alert \"Chua chon doi tuong chu nao!\\nBam nut < Chon chu tren man hinh >\"))"
            "    (alert \"Co chu co ban va Ty le up phai la so > 0 !\"))"))
        (setq done (start_dialog))
        (unload_dialog id)
        ;; Nguoi dung bam nut chon chu -> ra man hinh quet chon roi quay lai
        (if (= done 2)
          (progn
            (princ "\nChon cac chu can up ty le (quet chon nhieu doi tuong): ")
            (setq ss (ssget *upchu:filter*)))))))

  (vl-file-delete dcl)

  ;; Ap dung
  (if (= done 1)
    (progn
      (setq *upchu:base*  base
            *upchu:scale* scale
            newh (* base scale)
            n 0
            i 0)
      (vla-StartUndoMark doc)
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
              n   (+ n (upchu:seth obj newh))
              i   (1+ i)))
      (vla-EndUndoMark doc)
      (princ (strcat "\nDa up " (itoa n) " chu ve co "
                     (rtos newh 2 4)
                     "  ( = " (rtos base 2 4) " x " (rtos scale 2 4) " )"))))
  (princ))

(princ "\nDa nap UPCHU.lsp - Go lenh UPCHU de up ty le co chu.")
(princ)

;;; --- HET [04] UPCHU ---

;;; ###########################################################################
;;; ##  NHOM 2 - TEXT & KY TU
;;; ###########################################################################

;;; ---------------------------------------------------------------------------
;;; [05] VTT / CVT
;;;      Viet chu nhanh / chuyen doi text
;;;      Nguon: VTT(VIET CHU).lsp
;;; ---------------------------------------------------------------------------
;;; ==========================================================================
;;; LISP VIẾT CHỮ VTT - SỬA LỖI TREO CAD / ĐÃ TỐI ƯU HÓA 100%
;;; Lệnh chính: VTT | Lệnh chuyển đổi chữ cũ: CVT
;;; ==========================================================================

(vl-load-com)

;;; --- BẢNG MÃ CHUYỂN ĐỔI UNICODE -> TCVN3 ---
(defun GetUniToTCVNMap ()
  (list
    (cons "á" (chr 184)) (cons "à" (chr 181)) (cons "ả" (chr 182)) (cons "ã" (chr 183)) (cons "ạ" (chr 185))
    (cons "ă" (chr 168)) (cons "ắ" (chr 190)) (cons "ằ" (chr 187)) (cons "ẳ" (chr 188)) (cons "ẵ" (chr 189)) (cons "ặ" (chr 198))
    (cons "â" (chr 169)) (cons "ấ" (chr 202)) (cons "ầ" (chr 199)) (cons "ẩ" (chr 200)) (cons "ẫ" (chr 201)) (cons "ậ" (chr 203))
    (cons "é" (chr 208)) (cons "è" (chr 204)) (cons "ẻ" (chr 206)) (cons "ẽ" (chr 207)) (cons "ẹ" (chr 209))
    (cons "ê" (chr 170)) (cons "ế" (chr 213)) (cons "ề" (chr 210)) (cons "ể" (chr 211)) (cons "ễ" (chr 212)) (cons "ệ" (chr 214))
    (cons "í" (chr 221)) (cons "ì" (chr 215)) (cons "ỉ" (chr 216)) (cons "ĩ" (chr 220)) (cons "ị" (chr 222))
    (cons "ó" (chr 227)) (cons "ò" (chr 223)) (cons "ỏ" (chr 225)) (cons "õ" (chr 226)) (cons "ọ" (chr 228))
    (cons "ô" (chr 171)) (cons "ố" (chr 232)) (cons "ồ" (chr 230)) (cons "ổ" (chr 231)) (cons "ỗ" (chr 233)) (cons "ộ" (chr 234))
    (cons "ơ" (chr 172)) (cons "ớ" (chr 237)) (cons "ờ" (chr 235)) (cons "ở" (chr 236)) (cons "ỡ" (chr 238)) (cons "ợ" (chr 239))
    (cons "ú" (chr 243)) (cons "ù" (chr 241)) (cons "ủ" (chr 242)) (cons "ũ" (chr 244)) (cons "ụ" (chr 245))
    (cons "ư" (chr 173)) (cons "ứ" (chr 250)) (cons "ừ" (chr 247)) (cons "ử" (chr 248)) (cons "ữ" (chr 249)) (cons "ự" (chr 251))
    (cons "ý" (chr 253)) (cons "ỳ" (chr 252)) (cons "ỷ" (chr 254)) (cons "ỹ" (chr 255))
    (cons "đ" (chr 174)) (cons "Đ" (chr 167))
    ;; Chuyển đổi chữ hoa Unicode sang chữ thường TCVN3 (Tránh lỗi font .VnH)
    (cons "Á" (chr 184)) (cons "À" (chr 181)) (cons "Ả" (chr 182)) (cons "Ã" (chr 183)) (cons "Ạ" (chr 185))
    (cons "Ă" (chr 168)) (cons "Ắ" (chr 190)) (cons "Ằ" (chr 187)) (cons "Ẳ" (chr 188)) (cons "Ẵ" (chr 189)) (cons "Ặ" (chr 198))
    (cons "Â" (chr 169)) (cons "Ấ" (chr 202)) (cons "Ầ" (chr 199)) (cons "Ẩ" (chr 200)) (cons "Ẫ" (chr 201)) (cons "Ậ" (chr 203))
    (cons "É" (chr 208)) (cons "È" (chr 204)) (cons "Ẻ" (chr 206)) (cons "Ẽ" (chr 207)) (cons "Ẹ" (chr 209))
    (cons "Ê" (chr 170)) (cons "Ế" (chr 213)) (cons "Ề" (chr 210)) (cons "Ể" (chr 211)) (cons "Ễ" (chr 212)) (cons "Ệ" (chr 214))
    (cons "Í" (chr 221)) (cons "Ì" (chr 215)) (cons "Ỉ" (chr 216)) (cons "Ĩ" (chr 220)) (cons "Ị" (chr 222))
    (cons "Ó" (chr 227)) (cons "Ò" (chr 223)) (cons "Ỏ" (chr 225)) (cons "Õ" (chr 226)) (cons "Ọ" (chr 228))
    (cons "Ô" (chr 171)) (cons "Ố" (chr 232)) (cons "Ồ" (chr 230)) (cons "Ổ" (chr 231)) (cons "Ỗ" (chr 233)) (cons "Ộ" (chr 234))
    (cons "Ơ" (chr 172)) (cons "Ớ" (chr 237)) (cons "Ờ" (chr 235)) (cons "Ở" (chr 236)) (cons "Ỡ" (chr 238)) (cons "Ợ" (chr 239))
    (cons "Ú" (chr 243)) (cons "Ù" (chr 241)) (cons "Ủ" (chr 242)) (cons "Ũ" (chr 244)) (cons "Ụ" (chr 245))
    (cons "Ư" (chr 173)) (cons "Ứ" (chr 250)) (cons "Ừ" (chr 247)) (cons "Ử" (chr 248)) (cons "Ữ" (chr 249)) (cons "Ự" (chr 251))
    (cons "Ý" (chr 253)) (cons "Ỳ" (chr 252)) (cons "Ỷ" (chr 254)) (cons "Ỹ" (chr 255))
  )
)

(defun GetTCVNToUniMap ()
  (mapcar '(lambda (x) (cons (cdr x) (car x))) (GetUniToTCVNMap))
)

;;; --- DỊCH CHUỖI ---
(defun TranslateString (str mapList / res i len found pair key klen)
  (setq res "" len (strlen str) i 1)
  (while (<= i len)
    (setq found nil)
    (foreach pair mapList
      (if (not found)
        (progn
          (setq key (car pair) klen (strlen key))
          (if (= (substr str i klen) key)
            (progn (setq res (strcat res (cdr pair)) i (+ i klen) found t))
          )
        )
      )
    )
    (if (not found) (progn (setq res (strcat res (substr str i 1)) i (1+ i))))
  )
  res
)

;;; --- KIỂM TRA STYLE PHẢI TCVN3 KHÔNG ---
(defun IsTCVN3Style (styleName / styleEnt fontFile)
  (setq styleEnt (tblsearch "STYLE" styleName))
  (if styleEnt
    (progn
      (setq fontFile (strcase (cdr (assoc 3 styleEnt))))
      (or 
        (= (substr fontFile 1 2) "VN")
        (= (substr fontFile 1 3) ".VN")
        (vl-string-search "TCVN" (strcase styleName))
        (vl-string-search "VN" (strcase styleName))
      )
    )
    nil
  )
)

;;; --- QUÉT STYLE CỰC NHANH KHÔNG SỬ DỤNG ACTIVEX ---
(defun GetStyleList ( / lst style)
  (setq lst nil)
  (setq style (tblnext "STYLE" t))
  (while style
    (setq lst (cons (cdr (assoc 2 style)) lst))
    (setq style (tblnext "STYLE"))
  )
  (acad_strlsort lst)
)

;;; --- TẠO FILE DCL TẠM (BỎ TIẾNG VIỆT CÓ DẤU TRÁNH LỖI ENCODING) ---
(defun CreateDCLFile ( / f dcl_file)
  (setq dcl_file (vl-filename-mktemp "smarttext" "" ".dcl"))
  (if (setq f (open dcl_file "w"))
    (progn
      (write-line "smarttext_dlg : dialog {" f)
      (write-line "    label = \"Smart Text Tool (VTT) - CAD GUI\";" f)
      (write-line "    : row {" f)
      (write-line "        : boxed_column {" f)
      (write-line "            label = \"Thiet lap Text\";" f)
      (write-line "            : popup_list { label = \"Text Style:\"; key = \"style_list\"; width = 28; }" f)
      (write-line "            : edit_box { label = \"Chieu cao:\"; key = \"txt_height\"; edit_width = 12; }" f)
      (write-line "        }" f)
      (write-line "        : boxed_radio_column {" f)
      (write-line "            label = \"Kieu chu\";" f)
      (write-line "            : radio_button { label = \"MText (Da dong)\"; key = \"type_m\"; }" f)
      (write-line "            : radio_button { label = \"DText (Don dong)\"; key = \"type_d\"; }" f)
      (write-line "        }" f)
      (write-line "    }" f)
      (write-line "    spacer;" f)
      (write-line "    ok_cancel;" f)
      (write-line "}" f)
      (close f)
      dcl_file
    )
    nil
  )
)

;;; ==========================================================================
;;; LỆNH CHÍNH: VTT
;;; ==========================================================================
(defun c:VTT ( / dcl_file dcl_id styleList curStyle initIdx status hStr styleEnt fixedHeight pt entMarker entNext oldEcho tmp_err)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; Khởi tạo biến lưu tạm
  (setq dcl_file nil dcl_id nil)

  ;; Hệ thống bẫy lỗi và giải phóng bộ nhớ khi bấm ESC / Hủy bỏ
  (setq tmp_err *error*)
  (defun *error* (msg)
    (if dcl_id (vl-catch-all-apply 'unload_dialog (list dcl_id)))
    (if (and dcl_file (vl-file-exists-p dcl_file)) (vl-file-delete dcl_file))
    (setvar "CMDECHO" oldEcho)
    (setq *error* tmp_err)
    (princ)
  )

  ;; Khởi tạo giá trị mặc định ban đầu nếu chưa có
  (if (not *wtext-height*) (setq *wtext-height* 2.5))
  (if (not *wtext-type*) (setq *wtext-type* "M"))

  ;; 1. Tạo và Load DCL an toàn
  (setq dcl_file (CreateDCLFile))
  (if dcl_file
    (progn
      (setq dcl_id (load_dialog dcl_file))
      (if (and dcl_id (>= dcl_id 0) (new_dialog "smarttext_dlg" dcl_id))
        (progn
          ;; Đưa danh sách Style vào popup
          (setq styleList (GetStyleList))
          (start_list "style_list")
          (mapcar 'add_list styleList)
          (end_list)

          ;; Chọn mặc định Style hiện hành
          (setq curStyle (getvar "TEXTSTYLE"))
          (setq initIdx (vl-position curStyle styleList))
          (if initIdx
            (set_tile "style_list" (itoa initIdx))
            (set_tile "style_list" "0")
          )

          ;; Đồng bộ dữ liệu hiện hành lên giao diện
          (set_tile "txt_height" (rtos *wtext-height* 2 2))
          (if (= *wtext-type* "M")
            (set_tile "type_m" "1")
            (set_tile "type_d" "1")
          )

          ;; Lắng nghe tương tác nút nhấn
          (action_tile "accept"
            "(progn
               (setq *wtext-style* (nth (atoi (get_tile \"style_list\")) styleList))
               (setq hStr (get_tile \"txt_height\"))
               (if (and hStr (/= hStr \"\")) (setq *wtext-height* (distof hStr)))
               (if (= (get_tile \"type_m\") \"1\")
                 (setq *wtext-type* \"M\")
                 (setq *wtext-type* \"D\")
               )
               (done_dialog 1)
             )"
          )
          (action_tile "cancel" "(done_dialog 0)")

          (setq status (start_dialog))
          (unload_dialog dcl_id)
          (setq dcl_id nil)
        )
      )
      (vl-file-delete dcl_file)
      (setq dcl_file nil)
    )
  )

  ;; 2. Bắt đầu viết chữ khi người dùng nhấn OK
  (if (= status 1)
    (progn
      (setvar "TEXTSTYLE" *wtext-style*)
      (setvar "TEXTSIZE" *wtext-height*)
      
      (setq pt (getpoint "\nChọn điểm đặt chữ: "))
      (if pt
        (progn
          (setq entMarker (entlast)) ; Đánh dấu đối tượng cuối cùng trước khi nhập
          
          ;; Khởi chạy lệnh nhập chữ của CAD (nhập lùi ô, Enter xuống dòng thoải mái)
          (if (= *wtext-type* "M")
            (progn
              (command "_.mtext" pt "H" *wtext-height* "W" 0)
              (while (> (getvar "CMDACTIVE") 0) (command pause))
            )
            (progn
              (setq styleEnt (tblsearch "STYLE" *wtext-style*))
              (setq fixedHeight (cdr (assoc 40 styleEnt)))
              (if (= fixedHeight 0.0)
                (command "_.text" pt *wtext-height* 0)
                (command "_.text" pt 0)
              )
              (while (> (getvar "CMDACTIVE") 0) (command pause))
            )
          )
          
          ;; 3. Tự động chuyển đổi mã chữ nếu dùng Font TCVN3
          (if (IsTCVN3Style *wtext-style*)
            (progn
              (princ "\nHệ thống phát hiện Font TCVN3. Đang tự động chuyển đổi mã...")
              (if (null entMarker)
                (setq entNext (entnext))
                (setq entNext (entnext entMarker))
              )
              (while entNext
                (ConvertEntityToTCVN3 entNext)
                (setq entNext (entnext entNext))
              )
              (princ "\nĐã tự động sửa lỗi font!")
            )
          )
        )
      )
    )
  )

  ;; Trả lại hàm lỗi mặc định
  (setq *error* tmp_err)
  (setvar "CMDECHO" oldEcho)
  (princ)
)

;;; --- CONVERT THỰC THỂ SANG TCVN3 ---
(defun ConvertEntityToTCVN3 (ent / obj oldStr newStr)
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'TextString)
    (progn
      (setq oldStr (vla-get-TextString obj))
      (setq newStr (TranslateString oldStr (GetUniToTCVNMap)))
      (vla-put-TextString obj newStr)
    )
  )
)

;;; ==========================================================================
;;; LỆNH PHỤ: CVT (CHUYỂN ĐỔI CHỮ CÓ SẴN UNICODE <-> TCVN3)
;;; ==========================================================================
(defun c:CVT ( / ss i ent obj oldStr newStr choice opMap)
  (initget "1 2")
  (setq choice (getkword "\nChọn chiều chuyển đổi [1: Unicode->TCVN3 / 2: TCVN3->Unicode] <1>: "))
  (if (not choice) (setq choice "1"))
  
  (if (= choice "1")
    (setq opMap (GetUniToTCVNMap))
    (setq opMap (GetTCVNToUniMap))
  )
  
  (princ "\nChọn các Text/MText cần chuyển đổi mã...")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
              oldStr (vla-get-TextString obj)
              newStr (TranslateString oldStr opMap)
              i (1+ i))
        (vla-put-TextString obj newStr)
      )
      (princ (strcat "\nHệ thống đã xử lý xong " (itoa (sslength ss)) " đối tượng!"))
    )
  )
  (princ)
)

;;; --- HET [05] VTT / CVT ---

;;; ---------------------------------------------------------------------------
;;; [06] KTD
;;;      Chen ky tu Hy Lap va ky tu dac biet
;;;      Nguon: KTD (chen ky tu Hy Lap).lsp
;;; ---------------------------------------------------------------------------
(defun c:KTD ( / dcl_file file dcl_id status pt active_val active_height active_style_idx 
               keep_running append_sym get_text_styles styles cur_style sel_style 
               num_height style_data fixed_h *error*)
  (vl-load-com)
  
  ;; Bộ xử lý lỗi tự động dọn dẹp file tạm khi đột ngột thoát
  (defun *error* (msg)
    (if (and dcl_file (vl-file-systime dcl_file))
      (vl-file-delete dcl_file)
    )
    (if dcl_id (unload_dialog dcl_id))
    (princ (strcat "\nChương trình đã dừng: " msg))
    (princ)
  )

  ;; Hàm lấy danh sách Text Styles có trong bản vẽ
  (defun get_text_styles ( / data lst )
    (setq data (tblnext "STYLE" T))
    (while data
      (if (not (vl-string-search "|" (cdr (assoc 2 data))))
        (setq lst (cons (cdr (assoc 2 data)) lst))
      )
      (setq data (tblnext "STYLE"))
    )
    (vl-sort lst '<)
  )

  ;; 1. Tạo file giao diện DCL tạm thời
  (setq dcl_file (vl-filename-mktemp "specchar.dcl"))
  (setq file (open dcl_file "w"))
  
  (write-line "ktd_dialog : dialog {" file)
  (write-line "    label = \"BẢNG CHÈN KÝ TỰ ĐẶC BIỆT & HY LẠP\";" file)
  (write-line "    spacer_1;" file)
  
  ;; Hàng chứa 2 cột chính
  (write-line "    : row {" file)
  
  ;; CỘT 1: Ký tự kỹ thuật (Thay Alpha, Beta bằng Góc và Vô cực)
  (write-line "      : boxed_column {" file)
  (write-line "          label = \" Ký tự Kỹ thuật \";" file)
  (write-line "          : row {" file)
  (write-line "              : button { key = \"b1\"; label = \"° (Độ)\"; width = 12; }" file)
  (write-line "              : button { key = \"b2\"; label = \"± (Cộng/Trừ)\"; width = 12; }" file)
  (write-line "              : button { key = \"b3\"; label = \"Ø (Phi)\"; width = 12; }" file)
  (write-line "              : button { key = \"b4\"; label = \"℄ (Center)\"; width = 12; }" file)
  (write-line "          }" file)
  (write-line "          : row {" file)
  (write-line "              : button { key = \"b5\"; label = \"Δ (Delta)\"; width = 12; }" file)
  (write-line "              : button { key = \"b6\"; label = \"Ω (Ohm)\"; width = 12; }" file)
  (write-line "              : button { key = \"b7\"; label = \"≈ (Xấp xỉ)\"; width = 12; }" file)
  (write-line "              : button { key = \"b8\"; label = \"≠ (Khác)\"; width = 12; }" file)
  (write-line "          }" file)
  (write-line "          : row {" file)
  (write-line "              : button { key = \"b9\"; label = \"≤ (Nhỏ/bằng)\"; width = 12; }" file)
  (write-line "              : button { key = \"b10\"; label = \"≥ (Lớn/bằng)\"; width = 12; }" file)
  (write-line "              : button { key = \"b11\"; label = \"² (Mũ 2)\"; width = 12; }" file)
  (write-line "              : button { key = \"b12\"; label = \"³ (Mũ 3)\"; width = 12; }" file)
  (write-line "          }" file)
  (write-line "          : row {" file)
  (write-line "              : button { key = \"b13\"; label = \"‰ (Phần nghìn)\"; width = 12; }" file)
  (write-line "              : button { key = \"b14\"; label = \"⅊ (Property)\"; width = 12; }" file)
  (write-line "              : button { key = \"b15\"; label = \"∠ (Góc)\"; width = 12; }" file)
  (write-line "              : button { key = \"b16\"; label = \"∞ (Vô cực)\"; width = 12; }" file)
  (write-line "          }" file)
  (write-line "      }" file)

  ;; CỘT 2: Trọn bộ chữ cái Hy Lạp (Lưới 4 hàng x 6 cột)
  (write-line "      : boxed_column {" file)
  (write-line "          label = \" Ký tự Hy Lạp \";" file)
  ;; Hàng 1
  (write-line "          : row {" file)
  (write-line "              : button { key = \"g1\"; label = \"α (Alpha)\"; width = 10; }" file)
  (write-line "              : button { key = \"g2\"; label = \"β (Beta)\"; width = 10; }" file)
  (write-line "              : button { key = \"g3\"; label = \"γ (Gamma)\"; width = 10; }" file)
  (write-line "              : button { key = \"g4\"; label = \"δ (Delta)\"; width = 10; }" file)
  (write-line "              : button { key = \"g5\"; label = \"ε (Epsilon)\"; width = 10; }" file)
  (write-line "              : button { key = \"g6\"; label = \"ζ (Zeta)\"; width = 10; }" file)
  (write-line "          }" file)
  ;; Hàng 2
  (write-line "          : row {" file)
  (write-line "              : button { key = \"g7\"; label = \"η (Eta)\"; width = 10; }" file)
  (write-line "              : button { key = \"g8\"; label = \"θ (Theta)\"; width = 10; }" file)
  (write-line "              : button { key = \"g9\"; label = \"ι (Iota)\"; width = 10; }" file)
  (write-line "              : button { key = \"g10\"; label = \"κ (Kappa)\"; width = 10; }" file)
  (write-line "              : button { key = \"g11\"; label = \"λ (Lambda)\"; width = 10; }" file)
  (write-line "              : button { key = \"g12\"; label = \"μ (Mu)\"; width = 10; }" file)
  (write-line "          }" file)
  ;; Hàng 3
  (write-line "          : row {" file)
  (write-line "              : button { key = \"g13\"; label = \"ν (Nu)\"; width = 10; }" file)
  (write-line "              : button { key = \"g14\"; label = \"ξ (Xi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g15\"; label = \"ο (Omicron)\"; width = 10; }" file)
  (write-line "              : button { key = \"g16\"; label = \"π (Pi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g17\"; label = \"ρ (Rho)\"; width = 10; }" file)
  (write-line "              : button { key = \"g18\"; label = \"σ (Sigma)\"; width = 10; }" file)
  (write-line "          }" file)
  ;; Hàng 4
  (write-line "          : row {" file)
  (write-line "              : button { key = \"g19\"; label = \"τ (Tau)\"; width = 10; }" file)
  (write-line "              : button { key = \"g20\"; label = \"υ (Upsilon)\"; width = 10; }" file)
  (write-line "              : button { key = \"g21\"; label = \"φ (Phi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g22\"; label = \"χ (Chi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g23\"; label = \"ψ (Psi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g24\"; label = \"ω (Omega)\"; width = 10; }" file)
  (write-line "          }" file)
  (write-line "      }" file)
  (write-line "    }" file)
  
  (write-line "    spacer_1;" file)
  
  ;; Thiết lập định dạng chữ (Style & Height)
  (write-line "    : boxed_row {" file)
  (write-line "        label = \" Tùy chỉnh chữ \";" file)
  (write-line "        : popup_list { key = \"cb_style\"; label = \"Font chữ (Style):\"; width = 30; }" file)
  (write-line "        : edit_box { key = \"eb_height\"; label = \"Cỡ chữ (Height):\"; edit_width = 10; }" file)
  (write-line "    }" file)

  ;; Vùng nhập và sửa text
  (write-line "    : row {" file)
  (write-line "        : edit_box {" file)
  (write-line "            key = \"eb_text\";" file)
  (write-line "            label = \"Nội dung chèn:\";" file)
  (write-line "            edit_width = 55;" file)
  (write-line "        }" file)
  (write-line "        : button {" file)
  (write-line "            key = \"btn_clear\";" file)
  (write-line "            label = \"Xóa Trắng\";" file)
  (write-line "            width = 10;" file)
  (write-line "        }" file)
  (write-line "    }" file)
  
  (write-line "    spacer_1;" file)
  
  ;; Hàng nút điều khiển cuối cùng
  (write-line "    : row {" file)
  (write-line "        : button { key = \"btn_ins\"; label = \"Chèn TEXT (DTEXT)\"; is_default = true; width = 20; }" file)
  (write-line "        : button { key = \"btn_ins_mt\"; label = \"Chèn MTEXT\"; width = 20; }" file)
  (write-line "        : cancel_button { label = \"Thoát\"; width = 12; }" file)
  (write-line "    }" file)
  (write-line "}" file)
  (close file)

  ;; 2. Hàm phụ trợ ghép ký tự vào cuối chuỗi
  (defun append_sym (sym / cur)
    (setq cur (get_tile "eb_text"))
    (set_tile "eb_text" (strcat cur sym))
  )

  ;; 3. Khởi tạo dữ liệu ban đầu
  (setq styles (get_text_styles))
  (setq active_val "")
  (setq active_height (rtos (getvar "TEXTSIZE") 2 2))
  (setq cur_style (getvar "TEXTSTYLE"))
  (setq active_style_idx (vl-position cur_style styles))
  (if (not active_style_idx) (setq active_style_idx 0))

  (setq keep_running t)

  ;; 4. Vòng lặp điều khiển giao diện
  (while keep_running
    (setq dcl_id (load_dialog dcl_file))
    (if (not (new_dialog "ktd_dialog" dcl_id))
      (progn (setq keep_running nil) (exit))
    )

    ;; Đưa danh sách Text Styles vào Dropdown
    (start_list "cb_style")
    (mapcar 'add_list styles)
    (end_list)
    
    ;; Cập nhật giao diện từ các biến trạng thái
    (set_tile "cb_style" (itoa active_style_idx))
    (set_tile "eb_height" active_height)
    (set_tile "eb_text" active_val)

    ;; Hành động cho Ký tự kỹ thuật
    (action_tile "b1" "(append_sym \"%%d\")")
    (action_tile "b2" "(append_sym \"%%p\")")
    (action_tile "b3" "(append_sym \"%%c\")")
    (action_tile "b4" "(append_sym \"\\\\U+2104\")")
    (action_tile "b5" "(append_sym \"\\\\U+0394\")")
    (action_tile "b6" "(append_sym \"\\\\U+03A9\")")
    (action_tile "b7" "(append_sym \"\\\\U+2248\")")
    (action_tile "b8" "(append_sym \"\\\\U+2260\")")
    (action_tile "b9" "(append_sym \"\\\\U+2264\")")
    (action_tile "b10" "(append_sym \"\\\\U+2265\")")
    (action_tile "b11" "(append_sym \"\\\\U+00B2\")")
    (action_tile "b12" "(append_sym \"\\\\U+00B3\")")
    (action_tile "b13" "(append_sym \"\\\\U+2030\")")
    (action_tile "b14" "(append_sym \"\\\\U+214A\")")
    (action_tile "b15" "(append_sym \"\\\\U+2220\")") ; Góc
    (action_tile "b16" "(append_sym \"\\\\U+221E\")") ; Vô cực

    ;; Hành động cho Trọn bộ 24 ký tự Hy Lạp
    (action_tile "g1"  "(append_sym \"\\\\U+03B1\")") ; alpha
    (action_tile "g2"  "(append_sym \"\\\\U+03B2\")") ; beta
    (action_tile "g3"  "(append_sym \"\\\\U+03B3\")") ; gamma
    (action_tile "g4"  "(append_sym \"\\\\U+03B4\")") ; delta
    (action_tile "g5"  "(append_sym \"\\\\U+03B5\")") ; epsilon
    (action_tile "g6"  "(append_sym \"\\\\U+03B6\")") ; zeta
    (action_tile "g7"  "(append_sym \"\\\\U+03B7\")") ; eta
    (action_tile "g8"  "(append_sym \"\\\\U+03B8\")") ; theta
    (action_tile "g9"  "(append_sym \"\\\\U+03B9\")") ; iota
    (action_tile "g10" "(append_sym \"\\\\U+03BA\")") ; kappa
    (action_tile "g11" "(append_sym \"\\\\U+03BB\")") ; lambda
    (action_tile "g12" "(append_sym \"\\\\U+03BC\")") ; mu
    (action_tile "g13" "(append_sym \"\\\\U+03BD\")") ; nu
    (action_tile "g14" "(append_sym \"\\\\U+03BE\")") ; xi
    (action_tile "g15" "(append_sym \"\\\\U+03BF\")") ; omicron
    (action_tile "g16" "(append_sym \"\\\\U+03C0\")") ; pi
    (action_tile "g17" "(append_sym \"\\\\U+03C1\")") ; rho
    (action_tile "g18" "(append_sym \"\\\\U+03C3\")") ; sigma
    (action_tile "g19" "(append_sym \"\\\\U+03C4\")") ; tau
    (action_tile "g20" "(append_sym \"\\\\U+03C5\")") ; upsilon
    (action_tile "g21" "(append_sym \"\\\\U+03C6\")") ; phi
    (action_tile "g22" "(append_sym \"\\\\U+03C7\")") ; chi
    (action_tile "g23" "(append_sym \"\\\\U+03C8\")") ; psi
    (action_tile "g24" "(append_sym \"\\\\U+03C9\")") ; omega

    (action_tile "btn_clear" "(set_tile \"eb_text\" \"\")")

    ;; Khi nhấn Chèn: Ghi nhận các tùy biến thiết lập trên bảng
    (action_tile "btn_ins" "(setq active_val (get_tile \"eb_text\") active_height (get_tile \"eb_height\") active_style_idx (atoi (get_tile \"cb_style\"))) (done_dialog 1)")
    (action_tile "btn_ins_mt" "(setq active_val (get_tile \"eb_text\") active_height (get_tile \"eb_height\") active_style_idx (atoi (get_tile \"cb_style\"))) (done_dialog 2)")
    (action_tile "cancel" "(done_dialog 0)")

    (setq status (start_dialog))
    (unload_dialog dcl_id)

    ;; 5. Tiến hành vẽ đối tượng chữ ra AutoCAD
    (if (or (= status 1) (= status 2))
      (progn
        ;; Thiết lập thuộc tính Font chữ được chọn
        (setq sel_style (nth active_style_idx styles))
        (setvar "TEXTSTYLE" sel_style)
        
        ;; Thiết lập cỡ chữ
        (setq num_height (atof active_height))
        (if (<= num_height 0.0) (setq num_height (getvar "TEXTSIZE")))
        (setvar "TEXTSIZE" num_height)
        
        ;; Kiểm tra xem Text Style hiện tại có bị đặt cứng chiều cao không
        (setq style_data (tblsearch "STYLE" sel_style))
        (setq fixed_h (cdr (assoc 40 style_data)))

        (if (/= active_val "")
          (progn
            (setq pt (getpoint "\nChọn điểm chèn chữ trên bản vẽ: "))
            (if pt
              (cond
                ;; Chèn DTEXT
                ((= status 1)
                 (if (and fixed_h (> fixed_h 0.0))
                   (vl-cmdf "_.text" pt "0" active_val)
                   (vl-cmdf "_.text" pt num_height "0" active_val)
                 )
                )
                ;; Chèn MTEXT
                ((= status 2)
                 (vl-cmdf "_-mtext" pt "_H" num_height "_W" "0" active_val "")
                )
              )
            )
          )
          (princ "\n[Lỗi] Bạn chưa nhập nội dung chữ!")
        )
      )
      ;; Nhấn Thoát (Cancel)
      (setq keep_running nil)
    )
  )

  ;; 6. Dọn dẹp file DCL tạm sau khi tắt bảng
  (if (and dcl_file (vl-file-systime dcl_file))
    (vl-file-delete dcl_file)
  )
  (princ "\nĐã đóng bảng công cụ.")
  (princ)
)

(princ "\n[OK] LISP đã cập nhật bộ ký tự Hy Lạp. Gõ lệnh 'KTD' để sử dụng.")
(princ)

;;; --- HET [06] KTD ---

;;; ---------------------------------------------------------------------------
;;; [07] GOM / GM
;;;      Gom nhieu Text roi thanh 1 MText
;;;      Nguon: GM(chuyenMText).lsp
;;; ---------------------------------------------------------------------------
;;; ============================================================
;;; GOM.LSP - GOM NHIEU TEXT / MTEXT THANH 1 MTEXT DUY NHAT - v1
;;; Lenh: GOM
;;;
;;; Cach dung: go GOM -> chinh tuy chon trong hop thoai -> quet chon
;;; ca cum ghi chu (tieu de + cac dong) -> tat ca duoc dong goi thanh
;;; MOT MTEXT duy nhat, giu nguyen vi tri hien thi cua ca cum.
;;;
;;; Tinh nang:
;;;   - Nhan ca TEXT (DTEXT) lan MTEXT trong tap chon.
;;;   - Tu sap xep cac dong theo thu tu TREN -> DUOI.
;;;   - Cac text nam CUNG HANG (lech dung trong dung sai) duoc noi
;;;     thanh 1 dong, theo thu tu trai -> phai (tuy chon).
;;;   - Dong co chieu cao chu KHAC chieu cao chung (vd tieu de to hon)
;;;     duoc giu nguyen bang ma \H<ti le>x trong MTEXT.
;;;   - Gian dong cua MTEXT tu tinh theo khoang cach dong thuc te
;;;     (hoac nhap he so trong hop thoai).
;;;   - Khoang cach giua 2 dong lon bat thuong -> chen them dong trong
;;;     (tuy chon) de giu bo cuc doan.
;;;   - TEXT: tu doi ma %% (%%d %%p %%c %%u %%o %%%%) sang ma MTEXT,
;;;     escape ky tu dac biet \ { }. MTEXT: giu nguyen noi dung.
;;;   - Vi tri MTEXT = goc tren-trai khung bao ca cum -> khong xe dich.
;;;   - Layer / mau / text style lay theo doi tuong tren cung.
;;;
;;; Gioi han: cac text nen co goc xoay 0 (ghi chu thong thuong).
;;; ============================================================

(vl-load-com)

;; Bien luu tuy chon theo session
(if (null *gom-del*)   (setq *gom-del* "1"))   ; xoa doi tuong goc
(if (null *gom-same*)  (setq *gom-same* "1"))  ; gop text cung hang
(if (null *gom-blank*) (setq *gom-blank* "1")) ; chen dong trong neu gap lon
(if (null *gom-hmode*) (setq *gom-hmode* "hcom")) ; hcom | hfirst | hcus
(if (null *gom-hcus*)  (setq *gom-hcus* ""))
(if (null *gom-lsp*)   (setq *gom-lsp* ""))    ; he so gian dong (trong = tu tinh)

;; ---------- Tien ich chuoi ----------
(defun gom:blank-p (s) (or (not s) (= (vl-string-trim " " s) "")))

;; Escape ky tu dac biet cua MTEXT: \ { }
(defun gom:esc (s / r i c)
  (setq r "" i 1)
  (repeat (strlen s)
    (setq c (substr s i 1))
    (cond
      ((= c "\\") (setq r (strcat r "\\\\")))
      ((= c "{")  (setq r (strcat r "\\{")))
      ((= c "}")  (setq r (strcat r "\\}")))
      (t          (setq r (strcat r c)))
    )
    (setq i (1+ i))
  )
  r
)

;; Doi ky hieu %% cua TEXT sang ma MTEXT (goi SAU gom:esc)
(defun gom:pct (s / r i n len c uOn oOn d1 d2 d3)
  (setq r "" i 1 len (strlen s) uOn nil oOn nil)
  (while (<= i len)
    (if (and (<= (+ i 1) len) (= (substr s i 2) "%%"))
      (progn
        (setq c (if (<= (+ i 2) len) (strcase (substr s (+ i 2) 1)) ""))
        (cond
          ((= c "D") (setq r (strcat r "\\U+00B0")) (setq i (+ i 3)))
          ((= c "P") (setq r (strcat r "\\U+00B1")) (setq i (+ i 3)))
          ((= c "C") (setq r (strcat r "\\U+2205")) (setq i (+ i 3)))
          ((= c "U")
           (setq r (strcat r (if uOn "\\l" "\\L")))
           (setq uOn (not uOn)) (setq i (+ i 3)))
          ((= c "O")
           (setq r (strcat r (if oOn "\\o" "\\O")))
           (setq oOn (not oOn)) (setq i (+ i 3)))
          ((= c "%") (setq r (strcat r "%")) (setq i (+ i 3)))
          ((and (<= (+ i 4) len)
                (setq d1 (substr s (+ i 2) 1))
                (setq d2 (substr s (+ i 3) 1))
                (setq d3 (substr s (+ i 4) 1))
                (member d1 '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                (member d2 '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                (member d3 '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                (setq n (atoi (substr s (+ i 2) 3)))
                (> n 0))
           (setq r (strcat r (chr n))) (setq i (+ i 5)))
          (t (setq r (strcat r "%%")) (setq i (+ i 2)))
        )
      )
      (progn (setq r (strcat r (substr s i 1))) (setq i (1+ i)))
    )
  )
  (if uOn (setq r (strcat r "\\l")))
  (if oOn (setq r (strcat r "\\o")))
  r
)

;; ---------- Khung bao WCS cua 1 doi tuong ----------
(defun gom:bbox (ent / obj mn mx r)
  (setq obj (vlax-ename->vla-object ent))
  (setq r (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx)))
  (if (not (vl-catch-all-error-p r))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
    nil
  )
)

;; ---------- Noi dung MTEXT-ready cua 1 doi tuong ----------
(defun gom:content (ent / ed ty)
  (setq ed (entget ent) ty (cdr (assoc 0 ed)))
  (cond
    ((= ty "TEXT")  (gom:pct (gom:esc (cdr (assoc 1 ed)))))
    ((= ty "MTEXT") (vla-get-TextString (vlax-ename->vla-object ent)))
    (t "")
  )
)

;; ---------- Gia tri pho bien nhat trong danh sach so ----------
(defun gom:mode-num (lst / uniq u cnt best bestc)
  (setq uniq '())
  (foreach x lst
    (if (not (vl-some (function (lambda (u) (equal u x 1e-6))) uniq))
      (setq uniq (cons x uniq))
    )
  )
  (setq best (car uniq) bestc 0)
  (foreach u uniq
    (setq cnt (length (vl-remove-if-not
                        (function (lambda (x) (equal x u 1e-6))) lst)))
    (if (> cnt bestc) (setq best u bestc cnt))
  )
  best
)

;; ---------- Trung vi cua danh sach so ----------
(defun gom:median (lst / s n)
  (setq s (vl-sort lst '<) n (length s))
  (cond
    ((= n 0) nil)
    ((= (rem n 2) 1) (nth (/ n 2) s))
    (t (/ (+ (nth (/ n 2) s) (nth (1- (/ n 2)) s)) 2.0))
  )
)

;; ---------- Chuoi pho bien nhat trong danh sach ----------
(defun gom:mode-str (lst / uniq u cnt best bestc)
  (setq uniq '())
  (foreach x lst
    (if (not (member x uniq)) (setq uniq (cons x uniq)))
  )
  (setq best (car uniq) bestc 0)
  (foreach u uniq
    (setq cnt (length (vl-remove-if-not
                        (function (lambda (x) (= x u))) lst)))
    (if (> cnt bestc) (setq best u bestc cnt))
  )
  best
)

;; ---------- Phan tu pho bien nhat (so sanh equal) ----------
(defun gom:mode-eq (lst / uniq u cnt best bestc)
  (setq uniq '())
  (foreach x lst
    (if (not (vl-some (function (lambda (u) (equal u x))) uniq))
      (setq uniq (cons x uniq))
    )
  )
  (setq best (car uniq) bestc 0)
  (foreach u uniq
    (setq cnt (length (vl-remove-if-not
                        (function (lambda (x) (equal x u))) lst)))
    (if (> cnt bestc) (setq best u bestc cnt))
  )
  best
)

;; ---------- Hop thoai ----------
(defun gom:makedcl (/ fn f)
  (setq fn (vl-filename-mktemp "gom" nil ".dcl")
        f  (open fn "w"))
  (write-line "gom : dialog {" f)
  (write-line "  label = \"GOM - Dong goi nhieu Text thanh 1 MText - v1\";" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Tuy chon gop\";" f)
  (write-line "    : toggle { key=\"same\";  label=\"Noi cac text nam cung hang thanh 1 dong\"; }" f)
  (write-line "    : toggle { key=\"blank\"; label=\"Chen dong trong khi khoang cach dong lon bat thuong\"; }" f)
  (write-line "    : toggle { key=\"del\";   label=\"Xoa cac Text goc sau khi gom\"; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_radio_column {" f)
  (write-line "    label = \"Chieu cao chu cua MText\";" f)
  (write-line "    : radio_button { key=\"hcom\";   label=\"Chieu cao PHO BIEN nhat trong cum (khuyen dung)\"; }" f)
  (write-line "    : radio_button { key=\"hfirst\"; label=\"Chieu cao cua dong TREN CUNG\"; }" f)
  (write-line "    : row {" f)
  (write-line "      : radio_button { key=\"hcus\"; label=\"Tu nhap:\"; }" f)
  (write-line "      : edit_box { key=\"hcusval\"; edit_width=10; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : text { label=\"He so gian dong (de trong = tu tinh theo khoang cach thuc te):\"; }" f)
  (write-line "    : edit_box { key=\"lsp\"; edit_width=8; }" f)
  (write-line "  }" f)
  (write-line "  : text { label=\"Dong khac chieu cao chung se duoc giu nguyen bang ma \\\\H..x.\"; width=62; }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key=\"accept\"; label=\"Chon doi tuong >\"; is_default=true; fixed_width=true; width=18; }" f)
  (write-line "    : button { key=\"cancel\"; label=\"Huy\"; is_cancel=true; fixed_width=true; width=12; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  fn
)

;; ============================================================
;; LENH CHINH
;; ============================================================
(defun c:GOM (/ doc dclfile dclid ok del same blank hmode hcus lspin
                basePair baseC62 baseC420 lc62 lc420 codes
                ss n i ent items bb it tol baseH heights firstH
                lines cur curY sorted lineData lh ratio txtLine gaps g normal
                factor content parts insX insY ed0 lay sty lst new cntSrc
                itemsY topEnt)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; ----- Hop thoai tuy chon -----
  (setq dclfile (gom:makedcl) dclid (load_dialog dclfile))
  (if (not (new_dialog "gom" dclid))
    (progn (princ "\nKhong mo duoc hop thoai!") (exit))
  )
  (set_tile "del"   *gom-del*)
  (set_tile "same"  *gom-same*)
  (set_tile "blank" *gom-blank*)
  (set_tile *gom-hmode* "1")
  (set_tile "hcusval" *gom-hcus*)
  (set_tile "lsp" *gom-lsp*)
  (mode_tile "hcusval" (if (= *gom-hmode* "hcus") 0 1))
  (action_tile "hcom"   "(mode_tile \"hcusval\" 1)")
  (action_tile "hfirst" "(mode_tile \"hcusval\" 1)")
  (action_tile "hcus"   "(mode_tile \"hcusval\" 0)")
  (action_tile "accept"
    (strcat
      "(setq *gom-del* (get_tile \"del\")"
      "      *gom-same* (get_tile \"same\")"
      "      *gom-blank* (get_tile \"blank\")"
      "      *gom-hmode* (cond ((= (get_tile \"hfirst\") \"1\") \"hfirst\")"
      "                        ((= (get_tile \"hcus\") \"1\") \"hcus\")"
      "                        (t \"hcom\"))"
      "      *gom-hcus* (get_tile \"hcusval\")"
      "      *gom-lsp* (get_tile \"lsp\"))"
      "(done_dialog 1)"
    )
  )
  (action_tile "cancel" "(done_dialog 0)")
  (setq ok (start_dialog))
  (unload_dialog dclid)
  (vl-file-delete dclfile)
  (if (/= ok 1) (progn (princ "\nDa huy.") (exit)))

  (setq del   (= *gom-del* "1")
        same  (= *gom-same* "1")
        blank (= *gom-blank* "1")
        hmode *gom-hmode*
        hcus  (distof *gom-hcus* 2)
        lspin (distof *gom-lsp* 2)
  )
  (if (and (= hmode "hcus") (or (not hcus) (<= hcus 0)))
    (progn (princ "\nChieu cao tu nhap khong hop le.") (exit))
  )

  ;; ----- Chon doi tuong -----
  (setq ss (ssget "_I" '((0 . "TEXT,MTEXT"))))
  (if (or (null ss) (= (sslength ss) 0))
    (progn
      (princ "\nQuet chon ca cum TEXT/MTEXT can gom thanh 1 MText: ")
      (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    )
  )
  (if (null ss) (progn (princ "\nKhong co doi tuong nao duoc chon.") (exit)))

  ;; ----- Thu thap du lieu tung doi tuong -----
  ;; item = (topY leftX noi-dung chieu-cao ename mau62 mau420)
  ;;   mau62 : mau index (256 = ByLayer, 0 = ByBlock)
  ;;   mau420: true color 24-bit (nil neu khong co)
  (setq items '() n (sslength ss) i 0)
  (while (< i n)
    (setq ent (ssname ss i))
    (setq bb (gom:bbox ent))
    (if (and bb (not (gom:blank-p (gom:content ent))))
      (progn
        (setq ed0 (entget ent))
        (setq items
          (cons (list (cadr (cadr bb))            ; topY  = maxY
                      (car (car bb))              ; leftX = minX
                      (gom:content ent)
                      (cdr (assoc 40 ed0))
                      ent
                      (if (assoc 62 ed0) (cdr (assoc 62 ed0)) 256)
                      (if (assoc 420 ed0) (cdr (assoc 420 ed0)) nil))
                items))
      )
    )
    (setq i (1+ i))
  )
  (if (< (length items) 1)
    (progn (princ "\nKhong doc duoc noi dung doi tuong nao.") (exit))
  )

  ;; ----- Sap xep tren -> duoi -----
  (setq sorted (vl-sort items (function (lambda (a b) (> (car a) (car b))))))

  ;; ----- Chieu cao chung -----
  (setq heights (mapcar (function (lambda (x) (nth 3 x))) sorted))
  (setq firstH (car heights))
  (setq baseH
    (cond
      ((= hmode "hcus") hcus)
      ((= hmode "hfirst") firstH)
      (t (gom:mode-num heights))
    )
  )
  (if (or (not baseH) (<= baseH 0)) (setq baseH firstH))

  ;; ----- Gom cac item cung hang -----
  ;; dung sai theo phuong dung = 0.5 x chieu cao chung
  (setq tol (* 0.5 baseH))
  (setq lines '() cur (list (car sorted)) curY (car (car sorted)))
  (foreach it (cdr sorted)
    (if (and same (<= (abs (- curY (car it))) tol))
      (setq cur (cons it cur))
      (progn
        (setq lines (cons cur lines))
        (setq cur (list it) curY (car it))
      )
    )
  )
  (setq lines (cons cur lines))
  (setq lines (reverse lines))

  ;; ----- Du lieu tung dong: (topY noi-dung chieu-cao mau62 mau420) -----
  (setq lineData '())
  (foreach cur lines
    ;; trong 1 hang: sap trai -> phai roi noi bang khoang trang
    (setq cur (vl-sort cur (function (lambda (a b) (< (cadr a) (cadr b))))))
    (setq txtLine "")
    (foreach it cur
      (setq txtLine
        (if (= txtLine "") (nth 2 it) (strcat txtLine " " (nth 2 it))))
    )
    (setq lh (apply 'max (mapcar (function (lambda (x) (nth 3 x))) cur)))
    (setq itemsY (apply 'max (mapcar 'car cur)))
    (setq lineData (cons (list itemsY txtLine lh
                               (nth 5 (car cur))     ; mau62 cua dong
                               (nth 6 (car cur)))    ; mau420 cua dong
                         lineData))
  )
  (setq lineData (reverse lineData))

  ;; ----- Khoang cach dong chuan (trung vi) -----
  (setq gaps '())
  (setq i 0)
  (while (< i (1- (length lineData)))
    (setq g (- (car (nth i lineData)) (car (nth (1+ i) lineData))))
    (if (> g 1e-8) (setq gaps (cons g gaps)))
    (setq i (1+ i))
  )
  (setq normal (gom:median gaps))

  ;; ----- He so gian dong MTEXT: baseline = factor x 5/3 x chieu cao -----
  (setq factor
    (cond
      ((and lspin (> lspin 0)) lspin)
      ((and normal (> normal 0)) (/ normal (* baseH (/ 5.0 3.0))))
      (t 1.0)
    )
  )
  (if (< factor 0.25) (setq factor 0.25))
  (if (> factor 4.0)  (setq factor 4.0))

  ;; ----- *** v1.3: Mau entity MText LUON la ByLayer -----
  ;; Moi dong co mau rieng (index / true color) deu duoc giu nguyen
  ;; bang ma \C / \c nhung trong noi dung, nen so sanh voi ByLayer.
  (setq basePair (list 256 nil))
  (setq baseC62 (car basePair) baseC420 (cadr basePair))

  ;; ----- Ghep noi dung: \P giua cac dong; giu chieu cao \H..x; giu mau \C.. -----
  (setq content "" i 0)
  (foreach ld lineData
    (setq lh (caddr ld))
    (setq ratio (/ lh baseH))
    (setq lc62 (nth 3 ld) lc420 (nth 4 ld))
    ;; ma dinh dang rieng cua dong (neu khac chieu cao / mau chung)
    (setq codes "")
    (if (> (abs (- ratio 1.0)) 1e-3)
      (setq codes (strcat codes "\\H" (rtos ratio 2 4) "x;"))
    )
    (if (not (equal (list lc62 lc420) basePair))
      (cond
        (lc420
         (setq codes (strcat codes "\\c" (itoa lc420) ";")))
        ((and lc62 (>= lc62 1) (<= lc62 255))
         (setq codes (strcat codes "\\C" (itoa lc62) ";")))
        ;; ByLayer/ByBlock khac mau chung -> khong nhung duoc, de theo mau chung
      )
    )
    (setq txtLine
      (if (= codes "")
        (cadr ld)
        (strcat "{" codes (cadr ld) "}")
      )
    )
    (if (> i 0)
      (progn
        (setq content (strcat content "\\P"))
        ;; chen dong trong neu khoang cach lon bat thuong
        (if (and blank normal
                 (> (- (car (nth (1- i) lineData)) (car ld)) (* 1.6 normal)))
          (setq content (strcat content "\\P"))
        )
      )
    )
    (setq content (strcat content txtLine))
    (setq i (1+ i))
  )

  ;; ----- Diem chen = goc tren-trai khung bao ca cum -----
  (setq insX (apply 'min (mapcar 'cadr sorted)))
  (setq insY (apply 'max (mapcar 'car sorted)))

  ;; ----- Layer / style: GIU LAYER GOC -----
  ;; Lay layer PHO BIEN NHAT trong ca cum lam layer cua MText,
  ;; text style lay theo doi tuong dau tien thuoc layer do.
  ;; Mau cua MText = cap mau PHO BIEN NHAT (baseC62/baseC420) da tinh o tren;
  ;; dong nao khac mau da duoc giu bang ma \C/\c trong noi dung.
  (setq lay (gom:mode-str
              (mapcar (function (lambda (x) (cdr (assoc 8 (entget (nth 4 x))))))
                      sorted)))
  (setq topEnt nil)
  (foreach it sorted
    (if (and (not topEnt)
             (= (cdr (assoc 8 (entget (nth 4 it)))) lay))
      (setq topEnt (nth 4 it))
    )
  )
  (if (not topEnt) (setq topEnt (nth 4 (car sorted))))
  (setq ed0 (entget topEnt))
  (if (not lay) (setq lay (cdr (assoc 8 ed0))))
  (setq sty (if (assoc 7 ed0) (cdr (assoc 7 ed0)) (getvar "TEXTSTYLE")))

  ;; ----- Tao MTEXT -----
  (vla-StartUndoMark doc)
  (setq lst (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 lay)))
  ;; *** v1.3: khong ghi ma 62/420 -> mau entity = ByLayer.
  ;; (baseC62 = 256 nen 2 dieu kien duoi khong bao gio ghi mau)
  (if (and baseC62 (/= baseC62 256))
    (setq lst (append lst (list (cons 62 baseC62))))
  )
  (if baseC420
    (setq lst (append lst (list (cons 420 baseC420))))
  )
  (setq lst (append lst
              (list '(100 . "AcDbMText")
                    (cons 10 (list insX insY 0.0))
                    (cons 40 baseH)
                    (cons 41 0.0)     ; khong gioi han rong -> khong tu xuong dong
                    '(71 . 1)         ; Top-Left
                    (cons 7 sty)
                    (cons 1 content)
              )))
  (if (> (length lineData) 1)
    (setq lst (append lst (list '(73 . 2) (cons 44 factor))))  ; gian dong Exactly
  )
  (setq new (entmakex lst))

  (setq cntSrc (length items))
  (if new
    (progn
      (if del (foreach it items (entdel (nth 4 it))))
      (princ (strcat "\nDa gom " (itoa cntSrc) " doi tuong thanh 1 MText ("
                     (itoa (length lineData)) " dong)"
                     (if del ", da xoa doi tuong goc." ", giu lai doi tuong goc.")))
    )
    (princ "\nLoi: khong tao duoc MText.")
  )
  (vla-EndUndoMark doc)
  (princ)
)

;; ----- Lenh tat -----
(defun c:GM () (c:GOM))

(princ "\n=== GOM v1.3 da nap - Go GOM hoac GM: quet chon ca cum Text -> dong goi thanh 1 MText (mau entity ByLayer, giu mau rieng tung dong bang ma \\C). ===")
(princ)

;;; --- HET [07] GOM / GM ---

;;; ###########################################################################
;;; ##  NHOM 3 - BLOCK: CHEN / VE / THAY THE
;;; ###########################################################################

;;; ---------------------------------------------------------------------------
;;; [08] LDB
;;;      Load toan bo block tu 1 file DWG chi dinh vao ban ve
;;;      Nguon: ldb (load block).lsp
;;; ---------------------------------------------------------------------------
;;; ================================================================
;;; LDB - Load toan bo block tu 1 file DWG chi dinh vao ban ve hien hanh
;;; Nguyen ly: dung ObjectDBX doc file nguon (khong can mo file),
;;;            copy tat ca dinh nghia block sang ban ve dang mo.
;;; Dac diem:
;;;   - Khong can mo file nguon, khong anh huong file nguon
;;;   - Tu bo qua: layout (*Model_Space/*Paper_Space), xref,
;;;     block an danh (*U...), block da ton tai trung ten
;;;   - Bao cao ro: bao nhieu block nap moi, bao nhieu bo qua
;;; Cach dung: go lenh LDB -> chon file DWG
;;; ================================================================

(vl-load-com)

;; ---------- Mo file DWG bang ObjectDBX ----------
;; Tra ve doi tuong dbx neu OK, nil neu loi
(defun ldb:open-dbx (path / acad ver dbx rs)
  (setq acad (vlax-get-acad-object)
        ver  (itoa (fix (atof (getvar "ACADVER"))))
  )
  ;; Thu dang co version truoc (vd ObjectDBX.AxDbDocument.25)
  (setq rs (vl-catch-all-apply
             'vla-GetInterfaceObject
             (list acad (strcat "ObjectDBX.AxDbDocument." ver))))
  (if (vl-catch-all-error-p rs)
    (setq rs (vl-catch-all-apply
               'vla-GetInterfaceObject
               (list acad "ObjectDBX.AxDbDocument")))
  )
  (if (vl-catch-all-error-p rs)
    (progn (prompt "\n*** Khong khoi tao duoc ObjectDBX! ***") nil)
    (progn
      (setq dbx rs
            rs  (vl-catch-all-apply 'vla-Open (list dbx path)))
      (if (vl-catch-all-error-p rs)
        (progn
          (prompt (strcat "\n*** Khong doc duoc file: " path " ***"))
          (vlax-release-object dbx)
          nil
        )
        dbx
      )
    )
  )
)

;; ---------- Kiem tra block dinh nghia "that" (khong phai layout/xref/an danh) ----------
(defun ldb:real-block-p (blkdef / name)
  (setq name (vla-get-Name blkdef))
  (and
    (/= (substr name 1 1) "*")                          ; bo an danh + layout cu
    (= :vlax-false (vla-get-IsLayout blkdef))           ; bo layout
    (= :vlax-false (vla-get-IsXRef blkdef))             ; bo xref
  )
)

;; ================================================================
;; LENH CHINH
;; ================================================================
(defun c:LDB (/ path curdoc dbx blks copylist skiplist name n rs)
  (setq curdoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; ----- Chon file nguon -----
  (setq path (getfiled "Chon file DWG chua block can nap" "" "dwg" 16))
  (cond
    ((not path)
     (prompt "\n* Da huy lenh. *")
    )
    ((= (strcase path) (strcase (vla-get-FullName curdoc)))
     (prompt "\n*** File nguon trung voi ban ve dang mo! ***")
    )
    ((not (setq dbx (ldb:open-dbx path)))
     ;; thong bao loi da in trong ldb:open-dbx
    )
    (t
     ;; ----- Duyet block trong file nguon -----
     (setq blks     (vla-get-Blocks dbx)
           copylist '()
           skiplist '())
     (vlax-for blkdef blks
       (if (ldb:real-block-p blkdef)
         (progn
           (setq name (vla-get-Name blkdef))
           (if (tblsearch "BLOCK" name)
             (setq skiplist (cons name skiplist))      ; da co -> bo qua
             (setq copylist (cons blkdef copylist))    ; chua co -> copy
           )
         )
       )
     )

     ;; ----- Copy sang ban ve hien hanh -----
     (if copylist
       (progn
         (setq rs (vl-catch-all-apply
                    'vlax-invoke
                    (list dbx 'CopyObjects copylist (vla-get-Blocks curdoc))))
         (if (vl-catch-all-error-p rs)
           (prompt (strcat "\n*** Loi khi copy block: "
                           (vl-catch-all-error-message rs) " ***"))
           (progn
             (setq n (length copylist))
             (prompt (strcat "\n==> Da nap " (itoa n) " block tu file:"))
             (foreach b (reverse copylist)
               (prompt (strcat "\n  + " (vla-get-Name b)))
             )
           )
         )
       )
       (prompt "\n* Khong co block moi nao de nap. *")
     )

     ;; ----- Bao cac block bi bo qua vi trung ten -----
     (if skiplist
       (progn
         (prompt (strcat "\n* Bo qua " (itoa (length skiplist))
                         " block da ton tai trong ban ve (giu nguyen ban hien tai):"))
         (foreach s (reverse skiplist) (prompt (strcat "\n  - " s)))
         (prompt "\n  (Muon lay ban moi tu file nguon: xoa het block do trong ban ve, PURGE, roi chay lai LDB)")
       )
     )

     ;; ----- Dong ObjectDBX -----
     (vlax-release-object dbx)
    )
  )
  (princ)
)

(prompt "\nDa nap lenh LDB - Load toan bo block tu file DWG chi dinh vao ban ve hien hanh.")
(princ)

;;; --- HET [08] LDB ---

;;; ---------------------------------------------------------------------------
;;; [09] BSC
;;;      Dat block ve dung ty le (co hop thoai)
;;;      Nguon: BSC (UPBLOCK ATT).lsp
;;; ---------------------------------------------------------------------------
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

;;; --- HET [09] BSC ---

;;; ---------------------------------------------------------------------------
;;; [10] ALR / ALRGUI
;;;      Align doi tuong kieu Revit (fix lech mep)
;;;      Nguon: ALR (GIONG AL REVIT).lsp
;;; ---------------------------------------------------------------------------
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

;;; --- HET [10] ALR / ALRGUI ---

;;; ---------------------------------------------------------------------------
;;; [11] DBL
;;;      Chen block hang loat theo toa do tu file
;;;      Nguon: DBL (CHEN BLOG VAO TOA DO).lsp
;;; ---------------------------------------------------------------------------
;;; ================================================================
;;; DBL - Chen hang loat block theo toa do tu file CSV (BAN DCL v3.1 - Da sua rtos)
;;; *** SUA LOI v3 ***
;;;   - Sua loi khong nhan file: bo kiem tra bang findfile (hay truot
;;;     voi duong dan co dau tieng Viet / khoang trang), thay bang
;;;     mo doc file truc tiep
;;;   - Them BANG XEM TRUOC toa do ngay tren hop thoai:
;;;     chon file xong la thay ngay danh sach diem + tong so diem,
;;;     truoc khi bam OK
;;; Giao dien:
;;;   - Chon block: danh sach so xuong hoac nut "Pick <" ngoai man hinh
;;;   - Chon file CSV: o duong dan + nut "Chon file..."
;;;   - Bang xem truoc: STT, X, Y, Z tung diem + tong so diem
;;;   - Ty le chen + goc xoay (do), kiem tra hop le tai cho
;;;   - Nho lai toan bo lua chon cho lan chay sau
;;; File CSV: moi dong X,Y hoac X,Y,Z - nhan ca "," lan ";"
;;; Cach dung: go lenh DBL
;;; ================================================================

(vl-load-com)

(if (null *dbl-scale*) (setq *dbl-scale* "1"))
(if (null *dbl-rot*)   (setq *dbl-rot*   "0"))
(if (null *dbl-file*)  (setq *dbl-file*  ""))
(if (null *dbl-idx*)   (setq *dbl-idx*   0))
(if (null *dbl-swap*)  (setq *dbl-swap*  "0"))
(if (null *dbl-label*) (setq *dbl-label* "0"))
(if (null *dbl-style*) (setq *dbl-style* ""))
(if (null *dbl-th*)    (setq *dbl-th*    ""))

;; ---------- Tach chuoi theo ky tu phan cach ----------
(defun dbl:str-to-list (str del / pos lst)
  (while (setq pos (vl-string-search del str))
    (setq lst (cons (substr str 1 pos) lst))
    (setq str (substr str (+ pos (strlen del) 1)))
  )
  (reverse (cons str lst))
)

;; ---------- Danh sach ten block "that" trong ban ve ----------
(defun dbl:all-blknames (/ blk name flags lst)
  (while (setq blk (tblnext "BLOCK" (not blk)))
    (setq name  (cdr (assoc 2 blk))
          flags (cdr (assoc 70 blk)))
    (if (and (/= (substr name 1 1) "*")
             (= 0 (logand 4 flags)))
      (setq lst (cons name lst))
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Doc file CSV -> danh sach diem ((x y z) ...) ----------
;; *** v3: mo file truc tiep, khong dung findfile ***
;; Tra ve nil neu khong mo duoc file HOAC file khong co dong toa do nao
(defun dbl:read-csv (path / fh line cells pts)
  (setq pts '())
  (if (and path (/= path "") (setq fh (open path "r")))
    (progn
      (while (setq line (read-line fh))
        (setq line (vl-string-translate ";" "," line))
        (setq cells (mapcar '(lambda (s) (vl-string-trim " \"" s))
                            (dbl:str-to-list line ",")))
        (if (and (>= (length cells) 2)
                 (numberp (distof (nth 0 cells)))
                 (numberp (distof (nth 1 cells))))
          (setq pts
            (cons (list (distof (nth 0 cells))
                        (distof (nth 1 cells))
                        (if (and (nth 2 cells) (numberp (distof (nth 2 cells))))
                          (distof (nth 2 cells))
                          0.0))
                  pts))
        )
      )
      (close fh)
      (reverse pts)
    )
    nil
  )
)

;; ---------- Hoan doi X <-> Y neu bat tuy chon dao cot ----------
(defun dbl:swap-pts (pts swapstr)
  (if (= swapstr "1")
    (mapcar '(lambda (p) (list (cadr p) (car p) (caddr p))) pts)
    pts
  )
)

;; ---------- Do day bang xem truoc toa do tren hop thoai ----------
(defun dbl:fill-preview (pts / i)
  (start_list "preview")
  (setq i 0)
  (foreach p pts
    (setq i (1+ i))
    (add_list (strcat (itoa i) ")   X= " (rtos (car p) 2 3)
                      "    Y= " (rtos (cadr p) 2 3)
                      "    Z= " (rtos (caddr p) 2 3)))
  )
  (end_list)
  (set_tile "cnt"
    (if pts
      (strcat "Tong: " (itoa (length pts)) " diem se duoc chen block.")
      "Chua doc duoc toa do nao - hay chon file CSV."
    )
  )
)

;; ---------- Danh sach Text Style trong ban ve (bo file shape) ----------
(defun dbl:all-styles (/ sty name flags lst)
  (while (setq sty (tblnext "STYLE" (not sty)))
    (setq name  (cdr (assoc 2 sty))
          flags (cdr (assoc 70 sty)))
    (if (and (/= name "")
             (= 0 (logand 1 flags)))    ; bo shape file
      (setq lst (cons name lst))
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Chieu cao chu ghi chu (theo DIMTXT x DIMSCALE) ----------
(defun dbl:text-h (/ h)
  (setq h (* (getvar "DIMTXT")
             (if (> (getvar "DIMSCALE") 0) (getvar "DIMSCALE") 1.0)))
  (if (<= h 0) (setq h 2.5))
  h
)

;; ---------- Ghi nhan toa do co LEADER chi vao diem ----------
;; Leader: tu diem chen keo xien 45 do len tren-phai, them doan ngang,
;; MTEXT "X=... / Y=..." dat cuoi doan ngang (giong ghi chu trac dia)
;; tstyle: ten text style; thstr: cao chu (chuoi rong = tu dong theo DIMTXT)
(defun dbl:make-label (p tstyle thstr / h d hseg gap end1 end2)
  (setq h (if (and (/= thstr "") (distof thstr) (> (distof thstr) 0.0))
            (distof thstr)
            (dbl:text-h))
        d    (* 3.0 h)      ; do dai doan xien 45 do
        hseg (* 1.5 h)      ; do dai doan ngang
        gap  (* 0.4 h)      ; khoang ho tu leader den chu
        end1 (list (+ (car p) d) (+ (cadr p) d) (caddr p))
        end2 (list (+ (car end1) hseg) (cadr end1) (caddr p))
  )
  ;; Leader co mui ten, khong annotation
  (command "_.LEADER" "_none" p "_none" end1 "_none" end2 "" "" "_N")
  ;; MTEXT 2 dong, canh giua-trai, dat sau doan ngang
  ;; *** ĐÃ SỬA: (rtos ... 2 2) thành (rtos ... 2 3) để hiển thị 3 số thập phân ***
  (entmakex
    (append
      (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
            (cons 10 (list (+ (car end2) gap) (cadr end2) (caddr p)))
            (cons 40 h)
            '(71 . 4)                                 ; MiddleLeft
            (cons 1 (strcat "X=" (rtos (car p) 2 3)
                            "\\P" "Y=" (rtos (cadr p) 2 3)))
      )
      (if (and tstyle (/= tstyle "") (tblsearch "STYLE" tstyle))
        (list (cons 7 tstyle))                        ; font theo style da chon
        nil
      )
    )
  )
)

;; ---------- Chen block theo danh sach diem ----------
(defun dbl:insert-pts (blkName pts sc rot labstr tstyle thstr / doc ms count old-cmdecho)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        ms  (vla-get-ModelSpace doc)
        count 0
        old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (vla-StartUndoMark doc)
  (foreach p pts
    (vla-InsertBlock ms (vlax-3d-point p) blkName sc sc sc (* pi (/ rot 180.0)))
    (if (= labstr "1") (dbl:make-label p tstyle thstr)) ; ghi toa do + leader neu tick
    (setq count (1+ count))
  )
  (vla-EndUndoMark doc)
  (setvar "CMDECHO" old-cmdecho)
  count
)

;; ---------- Tao file DCL tam ----------
(defun dbl:make-dcl (/ fname f)
  (setq fname (vl-filename-mktemp "dbl" nil ".dcl")
        f     (open fname "w"))
  (foreach line
    '("dbl : dialog {"
      "  label = \"DBL - Chen block hang loat tu file CSV\";"
      "  : boxed_column {"
      "    label = \"Block can chen\";"
      "    : row {"
      "      : popup_list { key = \"blklist\"; edit_width = 32; }"
      "      : button { key = \"pick\"; label = \"Pick <\"; width = 10; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"File toa do (CSV: X,Y hoac X,Y,Z - nhan ca dau ; )\";"
      "    : row {"
      "      : edit_box { key = \"file\"; edit_width = 38; }"
      "      : button { key = \"browse\"; label = \"Chon file...\"; width = 12; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Xem truoc toa do\";"
      "    : toggle { key = \"swap\"; label = \"Dao cot X <-> Y (file trac dia dang N,E)\"; }"
      "    : toggle { key = \"label\"; label = \"Ghi toa do X=, Y= kem leader chi vao diem\"; }"
      "    : row {"
      "      : popup_list { key = \"tstyle\"; label = \"Font (Text Style) :\"; edit_width = 18; }"
      "      : edit_box { key = \"theight\"; label = \"Cao chu :\"; edit_width = 8; }"
      "    }"
      "    : text { label = \"(De trong Cao chu = tu dong theo DIMTXT x DIMSCALE)\"; }"
      "    : list_box { key = \"preview\"; width = 55; height = 10; }"
      "    : text { key = \"cnt\"; label = \"\"; width = 55; }"
      "  }"
      "  : row {"
      "    : edit_box { key = \"scale\"; label = \"Ty le chen :\"; edit_width = 10; }"
      "    : edit_box { key = \"rot\";   label = \"Goc xoay (do) :\"; edit_width = 10; }"
      "  }"
      "  spacer;"
      "  : text { key = \"err\"; label = \"\"; width = 55; }"
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
(defun c:DBL (/ dclfile dclid allnames done ret idx fpath scstr rotstr
                sc rot sel ent name blkName count pts swapstr labstr
                stylist sidx thstr)
  (setq allnames (dbl:all-blknames))
  (if (not allnames)
    (prompt "\n*** Ban ve khong co block nao! Hay dung lenh LDB nap block truoc. ***")
    (progn
      (setq dclfile (dbl:make-dcl)
            idx     (if (< *dbl-idx* (length allnames)) *dbl-idx* 0)
            fpath   *dbl-file*
            scstr   *dbl-scale*
            rotstr  *dbl-rot*
            swapstr *dbl-swap*
            labstr  *dbl-label*
            thstr   *dbl-th*
            stylist (dbl:all-styles)
            sidx    (cond
                      ((vl-position *dbl-style* stylist))
                      ((vl-position (getvar "TEXTSTYLE") stylist))
                      (0)
                    )
            pts     (dbl:read-csv *dbl-file*)   ; doc san file lan truoc (neu co)
            done    nil)

      ;; Vong lap hop thoai (dong tam de Pick / Browse roi mo lai)
      (while (not done)
        (setq dclid (load_dialog dclfile))
        (if (not (new_dialog "dbl" dclid))
          (progn (prompt "\n*** Loi: khong mo duoc hop thoai DCL! ***")
                 (setq done T ret 0))
          (progn
            (start_list "blklist")
            (mapcar 'add_list allnames)
            (end_list)
            (set_tile "blklist" (itoa idx))
            (set_tile "file"  fpath)
            (set_tile "scale" scstr)
            (set_tile "rot"   rotstr)
            (set_tile "swap"  swapstr)
            (set_tile "label" labstr)
            (start_list "tstyle")
            (mapcar 'add_list stylist)
            (end_list)
            (set_tile "tstyle" (itoa sidx))
            (set_tile "theight" thstr)
            (dbl:fill-preview (dbl:swap-pts pts swapstr))

            (action_tile "blklist" "(setq idx (atoi $value))")
            (action_tile "label" "(setq labstr $value)")
            (action_tile "tstyle" "(setq sidx (atoi $value))")
            (action_tile "theight" "(setq thstr $value)")
            ;; Tick dao cot -> cap nhat xem truoc ngay lap tuc
            (action_tile "swap"
              "(setq swapstr $value) (dbl:fill-preview (dbl:swap-pts pts swapstr))")
            ;; Go duong dan tay + Enter -> doc va xem truoc ngay
            (action_tile "file"
              "(setq fpath $value pts (dbl:read-csv fpath)) (dbl:fill-preview (dbl:swap-pts pts swapstr))")
            (action_tile "scale"   "(setq scstr $value)")
            (action_tile "rot"     "(setq rotstr $value)")
            (action_tile "pick"    "(done_dialog 2)")
            (action_tile "browse"  "(done_dialog 3)")
            ;; *** v3: kiem tra bang cach DOC THAT file, khong dung findfile ***
            (action_tile "accept"
              (strcat
                "(setq fpath (get_tile \"file\")"
                "      scstr (get_tile \"scale\")"
                "      rotstr (get_tile \"rot\")"
                "      thstr (get_tile \"theight\")"
                "      pts (dbl:read-csv fpath))"
                "(dbl:fill-preview (dbl:swap-pts pts swapstr))"
                "(cond"
                "  ((not pts)"
                "   (set_tile \"err\" \"*** Khong doc duoc file hoac file khong co toa do hop le! ***\"))"
                "  ((or (not (distof scstr)) (<= (distof scstr) 0.0))"
                "   (set_tile \"err\" \"*** Ty le phai la so > 0! ***\"))"
                "  ((not (distof rotstr))"
                "   (set_tile \"err\" \"*** Goc xoay phai la so! ***\"))"
                "  ((and (= \"1\" (get_tile \"label\"))"
                "        (/= (get_tile \"theight\") \"\")"
                "        (or (not (distof (get_tile \"theight\")))"
                "            (<= (distof (get_tile \"theight\")) 0.0)))"
                "   (set_tile \"err\" \"*** Cao chu phai la so > 0 (hoac de trong = tu dong)! ***\"))"
                "  (t (done_dialog 1))"
                ")"
              )
            )
            (action_tile "cancel" "(done_dialog 0)")

            (setq ret (start_dialog))
            (unload_dialog dclid)

            (cond
              ;; --- Pick block mau tren man hinh ---
              ((= ret 2)
               (setq sel (entsel "\nChon 1 block mau tren ban ve: "))
               (if (and sel (= "INSERT" (cdr (assoc 0 (entget (car sel))))))
                 (progn
                   (setq ent  (car sel)
                         name (if (vlax-property-available-p
                                    (vlax-ename->vla-object ent) 'EffectiveName)
                                (vla-get-EffectiveName (vlax-ename->vla-object ent))
                                (vla-get-Name (vlax-ename->vla-object ent))))
                   (if (vl-position name allnames)
                     (setq idx (vl-position name allnames))
                   )
                 )
                 (prompt "\n* Khong chon duoc block - giu nguyen lua chon cu. *")
               )
              )
              ;; --- Browse file CSV -> doc luon de xem truoc ---
              ((= ret 3)
               (setq fpath (getfiled "Chon file CSV chua toa do" fpath "csv" 4))
               (if (not fpath) (setq fpath *dbl-file*))
               (setq pts (dbl:read-csv fpath))
              )
              ;; --- OK / Cancel -> thoat vong lap ---
              (t (setq done T))
            )
          )
        )
      )
      (vl-file-delete dclfile)

      ;; ----- Thuc thi -----
      (if (= ret 1)
        (progn
          (setq blkName (nth idx allnames)
                sc      (distof scstr)
                rot     (distof rotstr)
                *dbl-idx*   idx
                *dbl-file*  fpath
                *dbl-scale* scstr
                *dbl-rot*   rotstr
                *dbl-swap*  swapstr
                *dbl-label* labstr
                *dbl-style* (nth sidx stylist)
                *dbl-th*    thstr)
          (setq count (dbl:insert-pts blkName (dbl:swap-pts pts swapstr) sc rot
                                      labstr (nth sidx stylist) thstr))
          (prompt (strcat "\n==> Da chen " (itoa count) " block \"" blkName
                          "\" (ty le " scstr ", xoay " rotstr " do"
                          (if (= swapstr "1") ", DA DAO COT X<->Y" "")
                          (if (= labstr "1") ", co ghi toa do tai diem" "")
                          ")."))
        )
        (prompt "\n* Da huy lenh. *")
      )
    )
  )
  (princ)
)

(prompt "\nDa nap lenh DBL v3.1 - Da cap nhat rtos hien thi 3 so thap phan.")
(princ)

;;; --- HET [11] DBL ---

;;; ---------------------------------------------------------------------------
;;; [12] DYN
;;;      Ve block dong theo tuyen, co khung xem truoc block
;;;      Nguon: DYN (VE BLOG).lsp
;;; ---------------------------------------------------------------------------
;;; =====================================================================
;;; DYNLINE.LSP   -   v3.1
;;; Lenh: DYN
;;; Chuc nang: Chen Block Dong (Dynamic Block) co Linear Parameter,
;;;            thao tac giong nhu ve Line (pick 2 diem) - block se tu
;;;            dong XOAY theo huong va KEO DAI (stretch) theo khoang cach
;;;            giua 2 diem pick.
;;; Giao dien: Dialog cho phep
;;;            (1) Chon Block tu danh sach cac Dynamic Block dang co
;;;                trong ban ve hien tai, hoac
;;;            (2) Bam nut "Pick" de chon truc tiep 1 Block co san
;;;                ngoai ban ve nham lay ten Block do (khong can go ten).
;;; Khong can file .dcl rieng - script tu sinh DCL tam thoi khi chay.
;;; *** v2: Tu nho block da ve lan gan nhat (session + registry)
;;; *** v3.1: KHUNG XEM TRUOC BLOCK nam ben PHAI (bo cuc 2 cot can doi):
;;;   - Nen DEN giong man hinh CAD, net ve giu mau ACI cua doi tuong
;;;   - Tu dong zoom vua khung (fit) khi doi block trong danh sach
;;;   - Ho tro LINE / LWPOLYLINE (co bulge) / POLYLINE / ARC / CIRCLE /
;;;     ELLIPSE / SPLINE / SOLID / TEXT-MTEXT-ATTDEF / INSERT long nhau
;;;   - Sau khi Pick block ngoai ban ve, hop thoai MO LAI de xem truoc
;;;     roi moi bam OK de ve
;;; =====================================================================

(vl-load-com)

;; Luu block ve gan nhat de lan sau dung lai
;;   - *DYNL-LastBlk* : nho trong phien lam viec
;;   - setenv/getenv  : nho ca khi dong CAD mo lai (registry)
(if (not *DYNL-LastBlk*)
  (setq *DYNL-LastBlk* (getenv "DYNL_LASTBLK"))
)

(defun DYNL:save-last (name)
  (if (and name (/= name ""))
    (progn
      (setq *DYNL-LastBlk* name)
      (setenv "DYNL_LASTBLK" name)
    )
  )
)

;; Vi tri cua ten block trong danh sach (nil neu khong co)
(defun DYNL:index-of (name lst / i res)
  (setq i 0 res nil)
  (foreach x lst
    (if (and (not res) (= (strcase x) (strcase name)))
      (setq res i)
    )
    (setq i (1+ i))
  )
  res
)

;; =====================================================================
;; ============  PHAN MOI v3: BO MAY XEM TRUOC BLOCK  ==================
;; =====================================================================
;; tr (transform) = (ox oy sx sy ang bx by)
;;   bx by : base point cua block dinh nghia (tru truoc khi bien doi)
;; =====================================================================

(setq *DYNL-MAXSEG* 4000)                 ; gioi han net ve cho nhe may

;; Bien doi 1 diem tu he toa do block sang he toa do "model" cua preview
(defun DYNL:tp (p tr / x y c s)
  (setq x (* (- (car  p) (nth 5 tr)) (nth 2 tr))
        y (* (- (cadr p) (nth 6 tr)) (nth 3 tr))
        c (cos (nth 4 tr))
        s (sin (nth 4 tr))
  )
  (list (+ (nth 0 tr) (- (* x c) (* y s)))
        (+ (nth 1 tr) (+ (* x s) (* y c)))
  )
)

;; Them 1 doan thang (da bien doi) vao danh sach + cap nhat bao hinh
(defun DYNL:addseg (q1 q2 col)
  (if (< *DYNL-N* *DYNL-MAXSEG*)
    (progn
      (setq *DYNL-SEGS*
        (cons (list (car q1) (cadr q1) (car q2) (cadr q2) col) *DYNL-SEGS*))
      (setq *DYNL-N* (1+ *DYNL-N*))
      (foreach q (list q1 q2)
        (if (null *DYNL-XMIN*)
          (setq *DYNL-XMIN* (car q) *DYNL-XMAX* (car q)
                *DYNL-YMIN* (cadr q) *DYNL-YMAX* (cadr q))
          (progn
            (if (< (car  q) *DYNL-XMIN*) (setq *DYNL-XMIN* (car  q)))
            (if (> (car  q) *DYNL-XMAX*) (setq *DYNL-XMAX* (car  q)))
            (if (< (cadr q) *DYNL-YMIN*) (setq *DYNL-YMIN* (cadr q)))
            (if (> (cadr q) *DYNL-YMAX*) (setq *DYNL-YMAX* (cadr q)))
          )
        )
      )
    )
  )
)

(defun DYNL:seg (p1 p2 tr col)
  (if (and p1 p2) (DYNL:addseg (DYNL:tp p1 tr) (DYNL:tp p2 tr) col))
)

;; Noi chuoi diem thanh cac doan thang
(defun DYNL:chain (pts tr col / p)
  (setq p (car pts))
  (foreach q (cdr pts)
    (DYNL:seg p q tr col)
    (setq p q)
  )
)

;; Sinh chuoi diem cho cung tron
(defun DYNL:arcpts (cen r sa sweep / n i step pts)
  (setq n (max 4 (min 72 (fix (* 12.0 (abs sweep))))))
  (setq step (/ sweep (float n)))
  (setq pts '() i 0)
  (repeat (1+ n)
    (setq pts (cons (polar cen (+ sa (* step i)) r) pts))
    (setq i (1+ i))
  )
  (reverse pts)
)

;; Sinh chuoi diem tu bulge cua LWPOLYLINE
(defun DYNL:barc (p1 p2 b / ang r a cen sa)
  (if (or (null b) (equal b 0.0 1e-9) (equal p1 p2 1e-9))
    (list p1 p2)
    (progn
      (setq ang (* 4.0 (atan b)))
      (setq r   (/ (distance p1 p2) (* 2.0 (sin (/ ang 2.0)))))
      (setq a   (angle p1 p2))
      (setq cen (polar p1 (+ a (- (/ pi 2.0) (/ ang 2.0))) r))
      (setq sa  (angle cen p1))
      (DYNL:arcpts cen (abs r) sa ang)
    )
  )
)

;; Mau ve: uu tien mau doi tuong -> mau layer -> mau block cha
;; Mau 7 doi thanh 255 (trang) de noi ro tren nen den
(defun DYNL:ecolor (ed parentCol / c lay ld)
  (setq c (cdr (assoc 62 ed)))
  (if (or (null c) (= c 256))
    (progn
      (setq lay (cdr (assoc 8 ed)))
      (setq ld  (if lay (tblsearch "LAYER" lay) nil))
      (setq c   (if ld (abs (cdr (assoc 62 ld))) 7))
    )
  )
  (if (= c 0) (setq c parentCol))
  (if (or (null c) (<= c 0) (> c 255)) (setq c 7))
  (if (= c 7) 255 c)
)

;; Lay danh sach (diem . bulge) cua LWPOLYLINE theo dung thu tu
(defun DYNL:lwpts (ed / pt bul res)
  (setq res '() pt nil bul 0.0)
  (foreach x ed
    (cond
      ((= (car x) 10)
       (if pt (setq res (cons (list pt bul) res)))
       (setq pt (cdr x) bul 0.0)
      )
      ((= (car x) 42) (setq bul (cdr x)))
    )
  )
  (if pt (setq res (cons (list pt bul) res)))
  (reverse res)
)

(defun DYNL:lw (ed tr col / lst rest a b cls first)
  (setq lst (DYNL:lwpts ed))
  (setq cls (and (cdr (assoc 70 ed)) (= 1 (logand 1 (cdr (assoc 70 ed))))))
  (setq first (car lst))
  (setq a (car lst) rest (cdr lst))
  (while rest
    (setq b (car rest))
    (DYNL:chain (DYNL:barc (car a) (car b) (cadr a)) tr col)
    (setq a b rest (cdr rest))
  )
  (if (and cls first a (not (equal (car a) (car first) 1e-9)))
    (DYNL:chain (DYNL:barc (car a) (car first) (cadr a)) tr col)
  )
)

;; Diem cuc bo (dx,dy) quay quanh goc p mot goc ang
(defun DYNL:rp (p dx dy ang)
  (list (+ (car  p) (- (* dx (cos ang)) (* dy (sin ang))))
        (+ (cadr p) (+ (* dx (sin ang)) (* dy (cos ang)))))
)

;; Khung bao cua chu (TEXT / MTEXT / ATTDEF) - ve dang hinh chu nhat
(defun DYNL:tbox (ed tr col / p h txt wd ht ang)
  (setq p   (cdr (assoc 10 ed))
        h   (cdr (assoc 40 ed))
        txt (cdr (assoc 1 ed))
        ang (cdr (assoc 50 ed))
  )
  (if (null ang) (setq ang 0.0))
  (if (null txt) (setq txt " "))
  (if (and p h (> h 0.0))
    (progn
      (setq ht h)
      (if (= (cdr (assoc 0 ed)) "MTEXT")
        (progn
          (setq wd (cdr (assoc 41 ed)))
          (if (or (null wd) (<= wd 0.0))
            (setq wd (* 0.7 h (max 1 (strlen txt))))
          )
          (setq ht (* 1.2 h))
        )
        (setq wd (* 0.7 h (max 1 (strlen txt))))
      )
      (DYNL:chain
        (list (DYNL:rp p 0.0 0.0 ang)
              (DYNL:rp p wd  0.0 ang)
              (DYNL:rp p wd  ht  ang)
              (DYNL:rp p 0.0 ht  ang)
              (DYNL:rp p 0.0 0.0 ang))
        tr col)
    )
  )
)

;; Xu ly 1 doi tuong trong dinh nghia block
(defun DYNL:ent (ed tr col depth / typ c cen r sa ea sw pts mv nv rt t1 t2 st i)
  (setq typ (cdr (assoc 0 ed)))
  (setq c   (DYNL:ecolor ed col))
  (cond
    ((= typ "LINE")
     (DYNL:seg (cdr (assoc 10 ed)) (cdr (assoc 11 ed)) tr c))

    ((= typ "LWPOLYLINE")
     (DYNL:lw ed tr c))

    ((= typ "POLYLINE")
     (setq *DYNL-LASTV*  nil
           *DYNL-FIRSTV* nil
           *DYNL-PLCOL*  c
           *DYNL-PLCLS*  (and (cdr (assoc 70 ed))
                              (= 1 (logand 1 (cdr (assoc 70 ed)))))))

    ((= typ "VERTEX")
     (setq pts (cdr (assoc 10 ed)))
     (if pts
       (progn
         (if (null *DYNL-FIRSTV*) (setq *DYNL-FIRSTV* pts))
         (if *DYNL-LASTV* (DYNL:seg *DYNL-LASTV* pts tr (if *DYNL-PLCOL* *DYNL-PLCOL* c)))
         (setq *DYNL-LASTV* pts)
       )
     ))

    ((= typ "SEQEND")
     (if (and *DYNL-PLCLS* *DYNL-FIRSTV* *DYNL-LASTV*)
       (DYNL:seg *DYNL-LASTV* *DYNL-FIRSTV* tr (if *DYNL-PLCOL* *DYNL-PLCOL* c)))
     (setq *DYNL-LASTV* nil *DYNL-FIRSTV* nil *DYNL-PLCLS* nil))

    ((= typ "CIRCLE")
     (setq cen (cdr (assoc 10 ed)) r (cdr (assoc 40 ed)))
     (if (and cen r (> r 0.0))
       (DYNL:chain (DYNL:arcpts cen r 0.0 (* 2.0 pi)) tr c)))

    ((= typ "ARC")
     (setq cen (cdr (assoc 10 ed))
           r   (cdr (assoc 40 ed))
           sa  (cdr (assoc 50 ed))
           ea  (cdr (assoc 51 ed)))
     (if (and cen r (> r 0.0) sa ea)
       (progn
         (setq sa (* (/ pi 180.0) sa)
               ea (* (/ pi 180.0) ea))
         (setq sw (- ea sa))
         (if (<= sw 0.0) (setq sw (+ sw (* 2.0 pi))))
         (DYNL:chain (DYNL:arcpts cen r sa sw) tr c))))

    ((= typ "ELLIPSE")
     (setq cen (cdr (assoc 10 ed))
           mv  (cdr (assoc 11 ed))
           rt  (cdr (assoc 40 ed))
           t1  (cdr (assoc 41 ed))
           t2  (cdr (assoc 42 ed)))
     (if (and cen mv rt)
       (progn
         (if (null t1) (setq t1 0.0))
         (if (null t2) (setq t2 (* 2.0 pi)))
         (if (<= (- t2 t1) 0.0) (setq t2 (+ t2 (* 2.0 pi))))
         (setq nv (list (* (- (cadr mv)) rt) (* (car mv) rt)))
         (setq st (/ (- t2 t1) 48.0) pts '() i 0)
         (repeat 49
           (setq pts
             (cons (list (+ (car cen)
                            (* (car mv) (cos (+ t1 (* st i))))
                            (* (car nv) (sin (+ t1 (* st i)))))
                         (+ (cadr cen)
                            (* (cadr mv) (cos (+ t1 (* st i))))
                            (* (cadr nv) (sin (+ t1 (* st i))))))
                   pts))
           (setq i (1+ i))
         )
         (DYNL:chain (reverse pts) tr c))))

    ((= typ "SPLINE")
     (setq pts '())
     (foreach x ed (if (= (car x) 11) (setq pts (cons (cdr x) pts))))
     (if (null pts)
       (foreach x ed (if (= (car x) 10) (setq pts (cons (cdr x) pts)))))
     (DYNL:chain (reverse pts) tr c))

    ((or (= typ "SOLID") (= typ "TRACE"))
     (DYNL:chain
       (list (cdr (assoc 10 ed)) (cdr (assoc 11 ed))
             (cdr (assoc 13 ed)) (cdr (assoc 12 ed))
             (cdr (assoc 10 ed)))
       tr c))

    ((member typ '("TEXT" "MTEXT" "ATTDEF"))
     (DYNL:tbox ed tr c))

    ((= typ "INSERT")
     (if (< depth 4) (DYNL:nested ed tr c depth)))

    (t nil)
  )
)

;; Duyet toan bo doi tuong trong 1 dinh nghia block
(defun DYNL:scan (e0 tr col depth / e ed)
  (setq e e0)
  (setq *DYNL-LASTV* nil *DYNL-FIRSTV* nil *DYNL-PLCLS* nil *DYNL-PLCOL* col)
  (while (and e (< *DYNL-N* *DYNL-MAXSEG*))
    (setq ed (entget e))
    (if ed (DYNL:ent ed tr col depth))
    (setq e (entnext e))
  )
)

;; Block long trong block
(defun DYNL:nested (ed tr col depth / nm ip sx sy rot np be bp ntr)
  (setq nm  (cdr (assoc 2 ed))
        ip  (cdr (assoc 10 ed))
        sx  (cond ((cdr (assoc 41 ed))) (1.0))
        sy  (cond ((cdr (assoc 42 ed))) (1.0))
        rot (cond ((cdr (assoc 50 ed))) (0.0))
  )
  (setq be (if nm (tblsearch "BLOCK" nm) nil))
  (if (and be ip)
    (progn
      (setq bp (cdr (assoc 10 be)))
      (if (null bp) (setq bp '(0.0 0.0 0.0)))
      (setq np (DYNL:tp ip tr))
      (setq ntr (list (car np) (cadr np)
                      (* (nth 2 tr) sx) (* (nth 3 tr) sy)
                      (+ (nth 4 tr) rot)
                      (car bp) (cadr bp)))
      (DYNL:scan (cdr (assoc -2 be)) ntr col (1+ depth))
    )
  )
)

;; ---------------------------------------------------------------
;; Ve khung xem truoc block vao tile "img_preview" (nen DEN nhu CAD)
;; ---------------------------------------------------------------
(defun DYNL:preview (name / key w h be bp dx dy sc cx cy mg s x1 y1 x2 y2)
  (setq key "img_preview")
  (setq w (dimx_tile key) h (dimy_tile key))
  (setq *DYNL-SEGS* '() *DYNL-N* 0
        *DYNL-XMIN* nil *DYNL-XMAX* nil *DYNL-YMIN* nil *DYNL-YMAX* nil)
  (setq be (if name (tblsearch "BLOCK" name) nil))
  (if be
    (progn
      (setq bp (cdr (assoc 10 be)))
      (if (null bp) (setq bp '(0.0 0.0 0.0)))
      (DYNL:scan (cdr (assoc -2 be))
                 (list 0.0 0.0 1.0 1.0 0.0 (car bp) (cadr bp))
                 7 0)
    )
  )
  ;; --- ve ---
  (start_image key)
  (fill_image 0 0 w h 0)                    ; nen den giong man hinh CAD
  (if (and *DYNL-SEGS* *DYNL-XMIN*)
    (progn
      (setq dx (- *DYNL-XMAX* *DYNL-XMIN*)
            dy (- *DYNL-YMAX* *DYNL-YMIN*))
      (if (< dx 1e-9) (setq dx 1e-9))
      (if (< dy 1e-9) (setq dy 1e-9))
      (setq mg 6)
      (setq sc (min (/ (float (max 1 (- w (* 2 mg)))) dx)
                    (/ (float (max 1 (- h (* 2 mg)))) dy)))
      (setq cx (/ (+ *DYNL-XMIN* *DYNL-XMAX*) 2.0)
            cy (/ (+ *DYNL-YMIN* *DYNL-YMAX*) 2.0))
      (foreach s *DYNL-SEGS*
        (setq x1 (fix (+ (/ w 2.0) (* sc (- (nth 0 s) cx))))
              y1 (fix (- (/ h 2.0) (* sc (- (nth 1 s) cy))))
              x2 (fix (+ (/ w 2.0) (* sc (- (nth 2 s) cx))))
              y2 (fix (- (/ h 2.0) (* sc (- (nth 3 s) cy)))))
        (vector_image x1 y1 x2 y2 (nth 4 s))
      )
    )
  )
  (end_image)
  ;; --- dong thong tin ---
  (cond
    ((null name)     (set_tile "txt_info" "Chua chon block nao."))
    ((null be)       (set_tile "txt_info" (strcat "Khong tim thay block: " name)))
    ((null *DYNL-SEGS*)
     (set_tile "txt_info" (strcat name " : khong co hinh hoc de hien thi")))
    (t (set_tile "txt_info"
         (strcat name " : " (itoa *DYNL-N*) " net ve"
                 (if (>= *DYNL-N* *DYNL-MAXSEG*) " (da rut gon)" ""))))
  )
  (princ)
)

;; ---------------------------------------------------------------
;; Tao file DCL tam thoi
;; ---------------------------------------------------------------
(defun DYNL:write-dcl (/ dclFile f)
  (setq dclFile (vl-filename-mktemp "dynline" nil ".dcl"))
  (setq f (open dclFile "w"))
  (write-line "dynline_dlg : dialog {" f)
  (write-line "  label = \"Ve Block Dong theo Line (Linear Parameter)  -  v3.1\";" f)
  (write-line "  : row {" f)
  ;; ---------- COT TRAI: dieu khien ----------
  (write-line "    : boxed_column {" f)
  (write-line "      label = \"Chon Block\";" f)
  (write-line "      : text { label = \"Block dong trong ban ve:\"; }" f)
  (write-line "      : popup_list {" f)
  (write-line "        key = \"list_blocks\";" f)
  (write-line "        edit_width = 32;" f)
  (write-line "      }" f)
  (write-line "      : text { key = \"txt_last\"; width = 34; value = \"\"; }" f)
  (write-line "      spacer_1;" f)
  (write-line "      : button {" f)
  (write-line "        key = \"btn_pick\";" f)
  (write-line "        label = \"<<< Pick chon Block ngoai ban ve\";" f)
  (write-line "        width = 34;" f)
  (write-line "      }" f)
  (write-line "      spacer_1;" f)
  (write-line "      : text { label = \"Huong dan:\"; }" f)
  (write-line "      : text { label = \"1. Chon block roi bam OK.\"; }" f)
  (write-line "      : text { label = \"2. Pick 2 diem nhu lenh Line,\"; }" f)
  (write-line "      : text { label = \"   block tu xoay va keo dai.\"; }" f)
  (write-line "      : text { label = \"3. Go U de huy doan vua ve.\"; }" f)
  (write-line "      spacer_1;" f)
  (write-line "    }" f)
  ;; ---------- COT PHAI: xem truoc ----------
  (write-line "    : boxed_column {" f)
  (write-line "      label = \"Xem truoc Block\";" f)
  (write-line "      : image {" f)
  (write-line "        key = \"img_preview\";" f)
  (write-line "        width = 38;" f)
  (write-line "        height = 15;" f)
  (write-line "        color = 0;" f)
  (write-line "        alignment = centered;" f)
  (write-line "      }" f)
  (write-line "      : text { key = \"txt_info\"; width = 38; value = \"\"; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  spacer_1;" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  dclFile
)

;; ---------------------------------------------------------------
;; Lay danh sach ten cac Dynamic Block dinh nghia trong ban ve
;; ---------------------------------------------------------------
(defun DYNL:get-dynamic-block-names (/ doc blocks lst nm)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq blocks (vla-get-Blocks doc))
  (setq lst '())
  (vlax-for blk blocks
    (setq nm (vla-get-Name blk))
    (if (and (= (vla-get-IsDynamicBlock blk) :vlax-true)
             (= (vla-get-IsLayout blk) :vlax-false)
             (= (vla-get-IsXRef blk) :vlax-false)
             (/= (substr nm 1 1) "*")
        )
      (setq lst (cons nm lst))
    )
  )
  (acad_strlsort lst)
)

;; ---------------------------------------------------------------
;; Lay EffectiveName (ten hieu dung) cua 1 Block Reference duoc pick
;; ---------------------------------------------------------------
(defun DYNL:get-effective-name (ent / obj)
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (if (= (vla-get-ObjectName obj) "AcDbBlockReference")
        (vlax-get-property obj 'EffectiveName)
        (progn
          (princ "\n[Loi] Doi tuong duoc chon khong phai la Block Reference.")
          nil
        )
      )
    )
  )
)

;; ---------------------------------------------------------------
;; Tim va gan gia tri cho Linear Parameter cua Block vua chen
;; ---------------------------------------------------------------
(defun DYNL:set-linear-value (obj newDist / props targetProp)
  (setq props (vlax-invoke obj 'GetDynamicBlockProperties))
  (setq targetProp nil)
  (foreach p props
    (if (and (not targetProp)
             (= (strcase (vla-get-PropertyName p)) "DISTANCE1"))
      (setq targetProp p)
    )
  )
  (if (not targetProp)
    (foreach p props
      (if (and (not targetProp)
               (= (vla-get-UnitsType p) 1)
               (= (vla-get-ReadOnly p) :vlax-false))
        (setq targetProp p)
      )
    )
  )
  (if targetProp
    (progn (vla-put-Value targetProp newDist) T)
    (progn
      (princ "\n[Canh bao] Block nay khong co Linear Parameter co the keo dai.")
      nil
    )
  )
)

;; ---------------------------------------------------------------
;; Lenh chinh: DYN
;; ---------------------------------------------------------------
(defun C:DYN (/ *error* dclFile dcl_id blkNames selIndex idx result
                     es blkName p1 p2 ang dist doc ms newObj oldOsmode
                     ptStack objStack segCount curName pickName goOn)

  (defun *error* (msg)
    (if oldOsmode (setvar "OSMODE" oldOsmode))
    (if (and dcl_id (> dcl_id 0)) (unload_dialog dcl_id))
    (if (and dclFile (findfile dclFile)) (vl-file-delete dclFile))
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort"))
      (princ (strcat "\n[Loi] " msg))
    )
    (princ)
  )

  (setq oldOsmode (getvar "OSMODE"))
  (setq blkNames (DYNL:get-dynamic-block-names))

  (if (not blkNames)
    (alert
      (strcat
        "Khong tim thay Dynamic Block nao trong ban ve hien tai.\n"
        "Ban van co the dung nut Pick sau khi da chen block do vao ban ve,\n"
        "hoac Insert truoc 1 Dynamic Block bat ky."
      )
    )
  )

  (setq dclFile (DYNL:write-dcl))
  (setq dcl_id (load_dialog dclFile))

  (setq curName *DYNL-LastBlk*)
  (setq blkName nil)
  (setq goOn T)

  (while goOn
    (if (not (new_dialog "dynline_dlg" dcl_id))
      (progn (setq goOn nil) (exit))
    )

    (start_list "list_blocks")
    (mapcar 'add_list blkNames)
    (end_list)

    (setq selIndex 0)
    (set_tile "txt_last" "")
    (if (and curName blkNames)
      (progn
        (setq idx (DYNL:index-of curName blkNames))
        (if idx
          (progn
            (setq selIndex idx)
            (set_tile "txt_last" (strcat "Dang chon: " curName))
          )
          (set_tile "txt_last"
            (strcat "Lan truoc: " curName " (khong co trong ban ve nay)"))
        )
      )
    )

    (if blkNames
      (progn
        (set_tile "list_blocks" (itoa selIndex))
        (DYNL:preview (nth selIndex blkNames))
      )
      (DYNL:preview nil)
    )

    (action_tile "list_blocks"
      "(setq selIndex (atoi $value)) (DYNL:preview (nth selIndex blkNames))")
    (action_tile "btn_pick" "(done_dialog 2)")
    (action_tile "accept"   "(done_dialog 1)")
    (action_tile "cancel"   "(done_dialog 0)")

    (setq result (start_dialog))

    (cond
      ;; --- Pick block ngoai ban ve, sau do MO LAI hop thoai de xem truoc
      ((= result 2)
       (setq es (entsel "\nChon mot Block co san trong ban ve: "))
       (if es
         (progn
           (setq pickName (DYNL:get-effective-name (car es)))
           (if pickName
             (progn
               (if (not (DYNL:index-of pickName blkNames))
                 (setq blkNames (acad_strlsort (cons pickName blkNames)))
               )
               (setq curName pickName)
             )
           )
         )
         (princ "\nKhong chon doi tuong nao.")
       )
      )
      ;; --- OK
      ((= result 1)
       (if blkNames
         (setq blkName (nth selIndex blkNames))
         (princ "\nKhong co Block nao de chon trong danh sach.")
       )
       (setq goOn nil)
      )
      ;; --- Cancel
      (t
       (princ "\nDa huy lenh.")
       (setq goOn nil)
      )
    )
  )

  (if (and dcl_id (> dcl_id 0)) (unload_dialog dcl_id))
  (setq dcl_id nil)
  (if (and dclFile (findfile dclFile)) (vl-file-delete dclFile))

  (if blkName
    (progn
      (DYNL:save-last blkName)
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq ms (vla-get-ModelSpace doc))
      (setq p1 (getpoint "\nChon diem bat dau (giong lenh Line): "))
      (if p1
        (progn
          (vla-StartUndoMark doc)
          (setq ptStack (list p1))
          (setq objStack '())
          (setq segCount 0)
          (while p1
            (initget "Undo")
            (setq p2
              (getpoint p1
                "\nChon diem tiep theo [Undo] (Enter de ket thuc): "
              )
            )
            (cond
              ((= p2 "Undo")
               (if objStack
                 (progn
                   (vla-Delete (car objStack))
                   (setq objStack (cdr objStack))
                   (setq ptStack (cdr ptStack))
                   (setq p1 (car ptStack))
                   (setq segCount (1- segCount))
                   (princ "\nDa huy doan vua ve.")
                 )
                 (princ "\nKhong co doan nao de huy.")
               )
              )
              ((null p2)
               (setq p1 nil)
              )
              (t
               (setq ang (angle p1 p2))
               (setq dist (distance p1 p2))
               (setq newObj
                 (vla-InsertBlock ms (vlax-3d-point p1) blkName 1.0 1.0 1.0 ang)
               )
               (DYNL:set-linear-value newObj dist)
               (vla-Update newObj)
               (setq objStack (cons newObj objStack))
               (setq ptStack (cons p2 ptStack))
               (setq segCount (1+ segCount))
               (setq p1 p2)
              )
            )
          )
          (vla-EndUndoMark doc)
          (princ (strcat "\nHoan tat. Da chen " (itoa segCount) " Block \"" blkName "\"."))
        )
        (princ "\nDa huy chon diem.")
      )
    )
  )

  (setvar "OSMODE" oldOsmode)
  (princ)
)

(princ "\n[DYNLINE v3.1] Da tai. Go DYN de ve - khung XEM TRUOC BLOCK nen den nam ben PHAI hop thoai.")
(princ)

;;; --- HET [12] DYN ---

;;; ---------------------------------------------------------------------------
;;; [13] TBL / THAYBLOCK
;;;      Thay the block cu bang block moi, giu Distance1 (v3.2 - co xem truoc)
;;;      Nguon: ThayBlock(TBL).lsp
;;; ---------------------------------------------------------------------------
;;; =========================================================================
;;; THAYBLOCK.LSP  -  v3
;;; Lenh: THAYBLOCK  (lenh tat: TBL)
;;;
;;; Quy trinh:
;;;   1) Go TBL -> QUET CHON cac block cu can thay (chon nhieu)
;;;   2) Hop thoai hien ra:
;;;        - Danh sach TAT CA block dong co trong ban ve -> chon block MOI
;;;        - O XEM TRUOC hinh dang block (nhu che do Ctrl+2 cua CAD)  [v3]
;;;        - Danh sach tham so dong cua block moi -> chon tham so chieu dai
;;;          can GIU NGUYEN (vd Distance1)
;;;   3) Bam OK (hoac nhay dup vao ten block) -> thay toan bo:
;;;        - Dung vi tri, goc xoay, ty le, layer cua block cu
;;;        - GIU NGUYEN gia tri chieu dai cua block cu
;;;        - Attribute (NAME...) LAY THEO GIA TRI MAC DINH CUA BLOCK MOI
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

;; ---------- Quet toan ban ve, lap danh sach block dong ----------
;; Tra ve list: ((tenblock ename-tham-chieu ten-block-thuc) ...)
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
              (setq res (cons (list nm ent (vla-get-Name obj)) res))
            )
          )
        )
      )
    )
  )
  (vl-sort res (function (lambda (a b) (< (strcase (car a)) (strcase (car b))))))
)

;; ---------- Cap nhat TAG sau khi thay block ----------
(defun TBL:RefreshTagsAfterReplace ( / )
  (if (fboundp 'TAG:UpdateAll)
    (TAG:UpdateAll)
  )
)

;; =========================================================================
;; ===============  XEM TRUOC HINH DANG BLOCK (v3)  ========================
;; =========================================================================

;; --- Bien doi 1 diem theo ty le / goc xoay / diem chen ---
(defun TBL:Tx (p sx sy rot off / x y c s)
  (setq x (* sx (car p))
        y (* sy (cadr p))
        c (cos rot)
        s (sin rot)
  )
  (list (+ (car off) (- (* x c) (* y s)))
        (+ (cadr off) (+ (* x s) (* y c))))
)

;; --- Lay so an toan tu thuoc tinh ActiveX ---
(defun TBL:Num (obj prop def / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (or (vl-catch-all-error-p r) (not (numberp r))) def r)
)

;; --- Mau ve cho 1 doi tuong (tra ve chi so mau DCL) ---
(defun TBL:Col (e / c lay r)
  (setq c (vl-catch-all-apply 'vla-get-Color (list e)))
  (if (or (vl-catch-all-error-p c) (not (numberp c))) (setq c 256))
  ;; BYLAYER -> lay mau cua layer
  (if (or (= c 256) (= c 0))
    (progn
      (setq lay (vl-catch-all-apply 'vla-get-Layer (list e)))
      (if (not (vl-catch-all-error-p lay))
        (progn
          (setq r (tblsearch "LAYER" lay))
          (if r (setq c (abs (cdr (assoc 62 r)))))
        )
      )
    )
  )
  (cond
    ((not (numberp c)) 7)
    ((or (= c 0) (= c 256) (> c 255)) 7)   ; khong ro -> trang, hop voi nen den
    (t c)                                   ; giu nguyen mau ACI nhu ngoai ban ve
  )
)

;; --- Lay chuoi diem cua 1 doi tuong (toa do noi bo block) ---
(defun TBL:EntPts (e / en nm sp ep n i lst r)
  (setq nm (vla-get-ObjectName e)
        en (vlax-vla-object->ename e)
        lst nil
  )
  (cond
    ;; duong thang: chi can 2 diem
    ((= nm "AcDbLine")
     (setq lst (list (vlax-get e 'StartPoint) (vlax-get e 'EndPoint))))
    ;; bo qua chu, hatch, att... cho nhe
    ((member nm '("AcDbText" "AcDbMText" "AcDbAttributeDefinition"
                  "AcDbHatch" "AcDbSolid" "AcDbPoint"))
     (setq lst nil))
    (t
     (setq sp (vl-catch-all-apply 'vlax-curve-getStartParam (list en))
           ep (vl-catch-all-apply 'vlax-curve-getEndParam (list en))
     )
     (if (or (vl-catch-all-error-p sp) (vl-catch-all-error-p ep)
             (not (numberp sp)) (not (numberp ep)) (<= (- ep sp) 1e-12))
       (setq lst nil)
       (progn
         (setq n (fix (* 6.0 (- ep sp))))
         (if (< n 8) (setq n 8))
         (if (> n 100) (setq n 100))
         (setq i 0)
         (repeat (1+ n)
           (setq r (vl-catch-all-apply
                     'vlax-curve-getPointAtParam
                     (list en (+ sp (* (- ep sp) (/ (float i) n))))))
           (if (and (not (vl-catch-all-error-p r)) r)
             (setq lst (cons r lst)))
           (setq i (1+ i))
         )
         (setq lst (reverse lst))
       )
     )
    )
  )
  lst
)

;; --- Duyet dinh nghia block, thu ve cac doan thang (x1 y1 x2 y2 mau) ---
(defun TBL:CollectSegs (bo sx sy rot off depth / segs nm pts col p q i
                        blks bo2 ip co cs2 cr)
  (setq segs nil
        blks (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (vlax-for e bo
    (setq nm (vl-catch-all-apply 'vla-get-ObjectName (list e)))
    (if (not (vl-catch-all-error-p nm))
      (if (= nm "AcDbBlockReference")
        ;; --- block long nhau: de quy ---
        (if (< depth 3)
          (progn
            (setq ip  (vl-catch-all-apply 'vlax-get (list e 'InsertionPoint)))
            (if (not (vl-catch-all-error-p ip))
              (progn
                (setq co  (TBL:Tx ip sx sy rot off)
                      cs2 (* sx (TBL:Num e 'XScaleFactor 1.0))
                      cr  (+ rot (TBL:Num e 'Rotation 0.0))
                )
                (setq bo2 (vl-catch-all-apply
                            'vla-Item
                            (list blks (vl-catch-all-apply 'vla-get-Name (list e)))))
                (if (not (vl-catch-all-error-p bo2))
                  (setq segs (append segs
                               (TBL:CollectSegs bo2 cs2
                                 (* sy (TBL:Num e 'YScaleFactor 1.0))
                                 cr co (1+ depth))))
                )
              )
            )
          )
        )
        ;; --- doi tuong thuong ---
        (progn
          (setq pts (TBL:EntPts e))
          (if (and pts (> (length pts) 1))
            (progn
              (setq col (TBL:Col e) i 0)
              (while (< i (1- (length pts)))
                (setq p (TBL:Tx (nth i pts) sx sy rot off)
                      q (TBL:Tx (nth (1+ i) pts) sx sy rot off)
                )
                (setq segs (cons (list (car p) (cadr p) (car q) (cadr q) col) segs))
                (setq i (1+ i))
              )
            )
          )
        )
      )
    )
  )
  segs
)

;; --- Lay (co cache) cac doan thang cua 1 block theo ten dinh nghia ---
(defun TBL:GetSegs (bname / pr bo segs)
  (setq pr (assoc bname *tbl-segcache*))
  (if pr
    (cdr pr)
    (progn
      (setq bo (vl-catch-all-apply
                 'vla-Item
                 (list (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
                       bname)))
      (setq segs (if (vl-catch-all-error-p bo)
                   nil
                   (TBL:CollectSegs bo 1.0 1.0 0.0 '(0.0 0.0) 0)))
      (setq *tbl-segcache* (cons (cons bname segs) *tbl-segcache*))
      segs
    )
  )
)

;; --- Ve xem truoc vao o image ---
;; QUAN TRONG: moi tinh toan va set_tile phai xong TRUOC khi mo start_image.
;; Goi set_tile giua start_image / end_image se lam mat het vector da ve.
(defun TBL:Clamp (v lo hi) (max lo (min hi (fix v))))

(defun TBL:DrawPrev (idx / item bname segs dx dy minx maxx miny maxy
                     k ox oy s lst txt x1 y1 x2 y2)
  (setq dx  (dimx_tile "prev")
        dy  (dimy_tile "prev")
        lst nil
        txt " "
  )
  (if (and idx (>= idx 0) (< idx (length *tbl-dynlist*)))
    (progn
      (setq item  (nth idx *tbl-dynlist*)
            bname (caddr item)
            segs  (TBL:GetSegs bname)
      )
      (if (null segs)
        (setq txt (strcat (car item) "  -  (khong ve duoc xem truoc)"))
        (progn
          ;; --- khung bao ---
          (setq minx nil)
          (foreach s segs
            (if (null minx)
              (setq minx (min (car s) (caddr s))   maxx (max (car s) (caddr s))
                    miny (min (cadr s) (cadddr s)) maxy (max (cadr s) (cadddr s)))
              (setq minx (min minx (car s) (caddr s))
                    maxx (max maxx (car s) (caddr s))
                    miny (min miny (cadr s) (cadddr s))
                    maxy (max maxy (cadr s) (cadddr s)))
            )
          )
          (if (< (- maxx minx) 1e-9) (setq maxx (+ minx 1.0)))
          (if (< (- maxy miny) 1e-9) (setq maxy (+ miny 1.0)))
          (setq k  (min (/ (float (- dx 10)) (- maxx minx))
                        (/ (float (- dy 10)) (- maxy miny)))
                ox (/ (- dx (* k (- maxx minx))) 2.0)
                oy (/ (- dy (* k (- maxy miny))) 2.0)
          )
          ;; --- doi sang toa do pixel truoc, chua ve voi ---
          (foreach s segs
            (setq x1 (TBL:Clamp (+ ox (* k (- (car s) minx))) 0 (1- dx))
                  y1 (TBL:Clamp (- dy oy (* k (- (cadr s) miny))) 0 (1- dy))
                  x2 (TBL:Clamp (+ ox (* k (- (caddr s) minx))) 0 (1- dx))
                  y2 (TBL:Clamp (- dy oy (* k (- (cadddr s) miny))) 0 (1- dy))
            )
            (setq lst (cons (list x1 y1 x2 y2 (nth 4 s)) lst))
          )
          (setq txt (strcat (car item) "   |   " (itoa (length segs)) " doan ve"))
          (princ (strcat "\n[XemTruoc] " bname
                         "  tile=" (itoa dx) "x" (itoa dy)
                         "  khung X " (rtos minx 2 1) "->" (rtos maxx 2 1)
                         "  Y " (rtos miny 2 1) "->" (rtos maxy 2 1)
                         "  he so=" (rtos k 2 4)))
        )
      )
    )
  )
  ;; --- set_tile XONG HET roi moi mo image ---
  (set_tile "binfo" txt)
  (start_image "prev")
  (fill_image 0 0 dx dy 0)
  ;; khung vien de kiem chung tile co ve duoc khong
  (vector_image 0 0 (1- dx) 0 8)
  (vector_image (1- dx) 0 (1- dx) (1- dy) 8)
  (vector_image (1- dx) (1- dy) 0 (1- dy) 8)
  (vector_image 0 (1- dy) 0 0 8)
  (foreach s lst
    (vector_image (car s) (cadr s) (caddr s) (cadddr s) (nth 4 s))
  )
  (end_image)
)

;; ---------- DCL ----------
(defun TBL:WriteDCL ( / f fn)
  (setq fn (vl-filename-mktemp "thayblock" nil ".dcl"))
  (setq f (open fn "w"))
  (foreach s
    (list
      "thayblock : dialog {"
      "  label = \"Thay block dong - giu nguyen chieu dai (v3.2)\";"
      "  : row {"
      "    : column {"
      "      : list_box {"
      "        key = \"blist\";"
      "        label = \"Block dong trong ban ve (chon block MOI):\";"
      "        height = 18; width = 34;"
      "      }"
      "    }"
      "    : column {"
      "      : boxed_column {"
      "        label = \"Xem truoc\";"
      "        : image { key = \"prev\"; width = 40; height = 16; color = 0; }"
      "        : text { key = \"binfo\"; label = \" \"; width = 40; }"
      "      }"
      "    }"
      "  }"
      "  : popup_list {"
      "    key = \"plist\";"
      "    label = \"Tham so chieu dai GIU NGUYEN:\";"
      "    width = 40;"
      "  }"
      "  : text { key = \"info\"; label = \"\"; width = 70; }"
      "  : text { label = \"Meo: nhay dup vao ten block trong danh sach = chon va chay luon.\"; width = 70; }"
      "  spacer;"
      "  ok_cancel;"
      "}"
    )
    (write-line s f)
  )
  (close f)
  fn
)

;; ---------- Do danh sach tham so cua block dang chon vao popup ----------
(defun TBL:FillProps (idx / item obj)
  (setq *tbl-curprops* nil)
  (if (and idx (>= idx 0) (< idx (length *tbl-dynlist*)))
    (progn
      (setq item (nth idx *tbl-dynlist*))
      (setq obj (vlax-ename->vla-object (cadr item)))
      (setq *tbl-curprops* (TBL:ListDynProps obj))
    )
  )
  (start_list "plist")
  (if *tbl-curprops*
    (foreach p *tbl-curprops* (add_list p))
    (add_list "(khong co tham so)")
  )
  (end_list)
  (if (and *tbl-curprops* (member *tbl-prop* *tbl-curprops*))
    (set_tile "plist"
      (itoa (- (length *tbl-curprops*)
               (length (member *tbl-prop* *tbl-curprops*)))))
    (set_tile "plist" "0")
  )
)

;; ---------- Xu ly khi chon trong danh sach ----------
(defun TBL:OnSel (v r / n)
  (setq n (atoi v))
  (TBL:FillProps n)
  (TBL:DrawPrev n)
  (if (= r 4)    ; nhay dup
    (progn
      (setq idx  n
            pidx (atoi (get_tile "plist"))
            res  1
      )
      (done_dialog 1)
    )
  )
)

;; =========================================================================
;; LENH CHINH
;; =========================================================================
(defun c:THAYBLOCK (/ doc ss dclF dclId res idx pidx
                      newName newRefEnt propName
                      i entOld objOld oldName owner
                      insPt rot sx sy sz layer lenVal
                      objIns cnt skip startIdx)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *tbl-segcache* nil)

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
  (foreach item *tbl-dynlist* (add_list (car item)))
  (end_list)

  (setq startIdx 0)
  (if (assoc *tbl-blkname* *tbl-dynlist*)
    (setq startIdx (- (length *tbl-dynlist*)
                      (length (member (assoc *tbl-blkname* *tbl-dynlist*)
                                      *tbl-dynlist*))))
  )
  (set_tile "blist" (itoa startIdx))
  (TBL:FillProps startIdx)
  (TBL:DrawPrev startIdx)
  (set_tile "info" (strcat "Se thay " (itoa (sslength ss)) " block da quet chon."))

  (action_tile "blist" "(TBL:OnSel $value $reason)")
  (action_tile "accept"
    "(setq idx (atoi (get_tile \"blist\")) pidx (atoi (get_tile \"plist\")) res 1)(done_dialog 1)")
  (action_tile "cancel" "(setq res nil)(done_dialog 0)")
  (start_dialog)
  (unload_dialog dclId)
  (vl-file-delete dclF)

  (if (not res) (progn (princ "\nDa huy.") (exit)))

  ;; ----- 4. Doc lua chon -----
  (setq newName   (car (nth idx *tbl-dynlist*)))
  (setq newRefEnt (cadr (nth idx *tbl-dynlist*)))
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

    (if (= (strcase oldName) (strcase newName))
      (setq skip (1+ skip))
      (progn
        (setq insPt  (vlax-get objOld 'InsertionPoint)
              rot    (vla-get-Rotation objOld)
              layer  (vla-get-Layer objOld)
              lenVal (TBL:GetDynProp objOld propName)
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
  (TBL:RefreshTagsAfterReplace)
  (princ)
)

(defun c:TBL () (c:THAYBLOCK))

(princ "\n=== THAYBLOCK v3.2 da nap (xem truoc block, nen den nhu CAD) - Go TBL de chay ===")
(princ)

;;; --- HET [13] TBL / THAYBLOCK ---

;;; ###########################################################################
;;; ##  NHOM 4 - ATTRIBUTE
;;; ###########################################################################

;;; ---------------------------------------------------------------------------
;;; [14] TAT
;;;      Them Attribute (Name / LKDV / Distance1) vao block
;;;      Nguon: TAT_ThemAttVaoBlock.lsp
;;; ---------------------------------------------------------------------------
;;; ============================================================
;;; TAT v3 - Them ATTRIBUTE vao DINH NGHIA BLOCK bang cach click
;;; Lenh: TAT
;;;
;;; Click vao 1 block bat ky (ke ca block thuong chua co attribute)
;;; -> tu them cac ATTDEF khai bao trong hop thoai vao dinh nghia block
;;; -> chay ATTSYNC de cac block da chen ngoai ban ve nhan attribute moi.
;;;
;;; Mac dinh: Invisible + Preset + Lock position, Text height = 2,
;;;           Justification Left, khong Annotative.
;;; Tag da co san trong block se duoc bo qua, khong tao trung.
;;;
;;; *** MOI (v3):
;;;   - Tag 3 mac dinh la "Distance1"
;;;   - Moi dong Tag co o TICK rieng: bo tick = khong chen dong do
;;;     (mac dinh: Tag 1 + Tag 2 co tick, Tag 3 bo tick)
;;;   - Bo tick dong nao thi o Tag / Default cua dong do bi khoa mo
;;; ============================================================

(vl-load-com)

(setq *TAT-HGT* 2.0)          ; chieu cao chu - co dinh
(setq *TAT-GAP* 3.0)          ; khoang cach dong giua cac ATTDEF

(if (null *tat-tag1*) (setq *tat-tag1* "Name"))
(if (null *tat-tag2*) (setq *tat-tag2* "LKDV"))
(if (or (null *tat-tag3*) (= *tat-tag3* "")) (setq *tat-tag3* "Distance1"))
(if (null *tat-def1*) (setq *tat-def1* ""))
(if (null *tat-def2*) (setq *tat-def2* ""))
(if (null *tat-def3*) (setq *tat-def3* ""))
;; *** v3: tick chon tung dong tag (mac dinh: Tag1 + Tag2 ON, Tag3 OFF)
(if (null *tat-use1*) (setq *tat-use1* T))
(if (null *tat-use2*) (setq *tat-use2* T))
(if (null *tat-use3-init*) (setq *tat-use3-init* T *tat-use3* nil))
(if (null *tat-inv*)  (setq *tat-inv*  T))
(if (null *tat-con*)  (setq *tat-con*  nil))
(if (null *tat-ver*)  (setq *tat-ver*  nil))
(if (null *tat-pre*)  (setq *tat-pre*  T))
(if (null *tat-lok*)  (setq *tat-lok*  T))
(if (null *tat-sty*)  (setq *tat-sty*  "VnArialNarrowH"))
(if (null *tat-syn*)  (setq *tat-syn*  T))

;; --------- Tien ich ---------
(defun tat-blank-p (s) (= (vl-string-trim " " s) ""))

(defun tat-styles (/ n lst r)
  (setq lst nil n (tblnext "STYLE" T))
  (while n
    (setq r (cdr (assoc 2 n)))
    (if (and r (/= r "") (not (wcmatch r "`**"))) (setq lst (cons r lst)))
    (setq n (tblnext "STYLE"))
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

(defun tat-pos (x lst / i n)
  (setq i 0 n nil)
  (foreach it lst
    (if (and (null n) (= (strcase it) (strcase x))) (setq n i))
    (setq i (1+ i))
  )
  n
)

;; --- Ten dinh nghia block that su (dynamic block -> ten goc) ---
(defun tat-bname (en / obj r)
  (setq obj (vlax-ename->vla-object en))
  (setq r (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))
  (if (vl-catch-all-error-p r)
    (vl-catch-all-apply 'vla-get-Name (list obj))
    r
  )
)

;; --- Block da co tag nay chua ---
(defun tat-hastag (bo tag / found)
  (setq found nil)
  (vlax-for e bo
    (if (and (not found)
             (= (vla-get-ObjectName e) "AcDbAttributeDefinition")
             (= (strcase (vla-get-TagString e)) (strcase tag)))
      (setq found T)
    )
  )
  found
)

;; --- Dem so ATTDEF dang co trong block ---
(defun tat-natt (bo / n)
  (setq n 0)
  (vlax-for e bo
    (if (= (vla-get-ObjectName e) "AcDbAttributeDefinition") (setq n (1+ n)))
  )
  n
)

;; --- Tao 1 ATTDEF trong dinh nghia block ---
(defun tat-add (bo pt tag def md sty lok / o)
  (setq o (vl-catch-all-apply
            'vla-AddAttribute
            (list bo *TAT-HGT* md tag (vlax-3d-point pt) tag def)))
  (if (vl-catch-all-error-p o)
    (progn
      (princ (strcat "\n    !! Loi tao tag \"" tag "\": "
                     (vl-catch-all-error-message o)))
      nil
    )
    (progn
      (if (not (tat-blank-p sty))
        (vl-catch-all-apply 'vla-put-StyleName (list o sty)))
      (vl-catch-all-apply 'vla-put-Height (list o *TAT-HGT*))
      (vl-catch-all-apply 'vla-put-Rotation (list o 0.0))
      (if lok (vl-catch-all-apply 'vla-put-LockPosition (list o :vlax-true)))
      o
    )
  )
)

;; --- ATTSYNC cho 1 ten block ---
(defun tat-sync (name / r)
  (if (> (getvar "CMDACTIVE") 0)
    (progn (princ "\n  !! Dang co lenh khac chay - bo qua ATTSYNC.") nil)
    (progn
      (setq r (vl-catch-all-apply 'command-s (list "_.ATTSYNC" "_N" name "_Y")))
      (if (and (vl-catch-all-error-p r) (= 0 (getvar "CMDACTIVE")))
        (setq r (vl-catch-all-apply 'command-s (list "_.ATTSYNC" "_N" name)))
      )
      (if (vl-catch-all-error-p r)
        (progn
          (princ (strcat "\n  !! ATTSYNC that bai cho \"" name
                         "\" - hay chay lenh ATTS cua ban."))
          nil
        )
        (progn (princ (strcat "\n  -> Da ATTSYNC block \"" name "\".")) T)
      )
    )
  )
)

;; --------- Giao dien ---------
(defun tat-makedcl (/ fn f)
  (setq fn (vl-filename-mktemp "tat" nil ".dcl")
        f  (open fn "w")
  )
  (foreach s
    (list
      "tat_dlg : dialog {"
      "  label = \"Them Attribute vao block - TAT v3\";"
      "  : boxed_column {"
      "    label = \"Attribute se them (bo tick hoac de trong Tag = bo qua dong do)\";"
      "    : row {"
      "      : toggle { key=\"use1\"; label=\"Tag 1:\"; }"
      "      : edit_box { key=\"tag1\"; edit_width=16; }"
      "      : edit_box { key=\"def1\"; label=\"Default:\"; edit_width=22; }"
      "    }"
      "    : row {"
      "      : toggle { key=\"use2\"; label=\"Tag 2:\"; }"
      "      : edit_box { key=\"tag2\"; edit_width=16; }"
      "      : edit_box { key=\"def2\"; label=\"Default:\"; edit_width=22; }"
      "    }"
      "    : row {"
      "      : toggle { key=\"use3\"; label=\"Tag 3:\"; }"
      "      : edit_box { key=\"tag3\"; edit_width=16; }"
      "      : edit_box { key=\"def3\"; label=\"Default:\"; edit_width=22; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Mode\";"
      "    : row {"
      "      : toggle { key=\"inv\"; label=\"Invisible\"; }"
      "      : toggle { key=\"con\"; label=\"Constant\"; }"
      "      : toggle { key=\"ver\"; label=\"Verify\"; }"
      "      : toggle { key=\"pre\"; label=\"Preset\"; }"
      "      : toggle { key=\"lok\"; label=\"Lock position\"; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Text settings\";"
      "    : popup_list { key=\"sty\"; label=\"Text style:\"; edit_width=24; }"
      "    : text { label=\"Text height = 2 (co dinh)   |   Justification Left   |   Rotation 0\"; width=62; }"
      "  }"
      "  : toggle { key=\"syn\"; label=\"Chay ATTSYNC sau khi them (cap nhat cac block da chen)\"; }"
      "  : text { label=\"Click vao block bat ky - ke ca block thuong chua co attribute.\"; width=62; }"
      "  : text { label=\"Tag da co san trong block se duoc bo qua, khong tao trung.\"; width=62; }"
      "  spacer;"
      "  ok_cancel;"
      "}"
    )
    (write-line s f)
  )
  (close f)
  fn
)

;; ================= LENH CHINH =================
(defun c:TAT (/ *error* dclfile dclid code doc styles blks
                tags defs md sty lok syn
                en nm bo done cnt nadd nskip pt k i tg df
                donelist undoon)

  (defun *error* (msg)
    (if undoon (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nLoi: " msg))
    )
    (princ "\nDa ket thuc TAT.")
    (princ)
  )

  (vl-load-com)
  (setq doc  (vla-get-ActiveDocument (vlax-get-acad-object))
        blks (vla-get-Blocks doc)
  )
  (if (> (getvar "CMDACTIVE") 0)
    (progn (princ "\nDang co lenh khac chua ket thuc. Bam ESC roi go lai TAT.") (exit))
  )

  (setq styles (tat-styles))
  (setq dclfile (tat-makedcl)
        dclid   (load_dialog dclfile)
  )
  (if (not (new_dialog "tat_dlg" dclid))
    (progn (princ "\nKhong mo duoc hop thoai!") (exit))
  )
  (set_tile "tag1" *tat-tag1*) (set_tile "def1" *tat-def1*)
  (set_tile "tag2" *tat-tag2*) (set_tile "def2" *tat-def2*)
  (set_tile "tag3" *tat-tag3*) (set_tile "def3" *tat-def3*)
  ;; *** v3: tick chon tung dong + mo/khoa o nhap theo tick
  (set_tile "use1" (if *tat-use1* "1" "0"))
  (set_tile "use2" (if *tat-use2* "1" "0"))
  (set_tile "use3" (if *tat-use3* "1" "0"))
  (mode_tile "tag1" (if *tat-use1* 0 1)) (mode_tile "def1" (if *tat-use1* 0 1))
  (mode_tile "tag2" (if *tat-use2* 0 1)) (mode_tile "def2" (if *tat-use2* 0 1))
  (mode_tile "tag3" (if *tat-use3* 0 1)) (mode_tile "def3" (if *tat-use3* 0 1))
  (action_tile "use1" "(mode_tile \"tag1\" (if (= $value \"1\") 0 1))(mode_tile \"def1\" (if (= $value \"1\") 0 1))")
  (action_tile "use2" "(mode_tile \"tag2\" (if (= $value \"1\") 0 1))(mode_tile \"def2\" (if (= $value \"1\") 0 1))")
  (action_tile "use3" "(mode_tile \"tag3\" (if (= $value \"1\") 0 1))(mode_tile \"def3\" (if (= $value \"1\") 0 1))")
  (set_tile "inv" (if *tat-inv* "1" "0"))
  (set_tile "con" (if *tat-con* "1" "0"))
  (set_tile "ver" (if *tat-ver* "1" "0"))
  (set_tile "pre" (if *tat-pre* "1" "0"))
  (set_tile "lok" (if *tat-lok* "1" "0"))
  (set_tile "syn" (if *tat-syn* "1" "0"))
  (start_list "sty")
  (foreach s styles (add_list s))
  (end_list)
  (set_tile "sty" (itoa (cond ((tat-pos *tat-sty* styles))
                              ((tat-pos (getvar "TEXTSTYLE") styles))
                              (0))))
  (action_tile "accept"
    (strcat "(setq *tat-tag1* (get_tile \"tag1\") *tat-def1* (get_tile \"def1\")"
            " *tat-tag2* (get_tile \"tag2\") *tat-def2* (get_tile \"def2\")"
            " *tat-tag3* (get_tile \"tag3\") *tat-def3* (get_tile \"def3\")"
            " *tat-use1* (= (get_tile \"use1\") \"1\")"
            " *tat-use2* (= (get_tile \"use2\") \"1\")"
            " *tat-use3* (= (get_tile \"use3\") \"1\")"
            " *tat-inv* (= (get_tile \"inv\") \"1\")"
            " *tat-con* (= (get_tile \"con\") \"1\")"
            " *tat-ver* (= (get_tile \"ver\") \"1\")"
            " *tat-pre* (= (get_tile \"pre\") \"1\")"
            " *tat-lok* (= (get_tile \"lok\") \"1\")"
            " *tat-syn* (= (get_tile \"syn\") \"1\")"
            " *tat-sty* (nth (atoi (get_tile \"sty\")) '"
            (vl-prin1-to-string styles) "))"
            "(done_dialog 1)"
    )
  )
  (action_tile "cancel" "(done_dialog 0)")
  (setq code (start_dialog))
  (unload_dialog dclid)
  (vl-file-delete dclfile)

  (if (/= code 1)
    (progn (princ "\nDa huy.") (princ))
    (progn
      ;; *** v3: dong khong duoc tick -> coi nhu tag rong -> bo qua
      (setq tags (list (if *tat-use1* *tat-tag1* "")
                       (if *tat-use2* *tat-tag2* "")
                       (if *tat-use3* *tat-tag3* ""))
            defs (list *tat-def1* *tat-def2* *tat-def3*)
            sty  *tat-sty*
            lok  *tat-lok*
            syn  *tat-syn*
            md   (+ (if *tat-inv* 1 0) (if *tat-con* 2 0)
                    (if *tat-ver* 4 0) (if *tat-pre* 8 0))
      )
      (if (and (tat-blank-p (nth 0 tags)) (tat-blank-p (nth 1 tags))
               (tat-blank-p (nth 2 tags)))
        (alert "Chua co Tag nao duoc chon.\n\nHay tick vao dong Tag can them va nhap ten Tag.")
        (progn
          (setq done nil cnt 0 donelist nil)
          (while (not done)
            (setq en (entsel "\nClick vao block can them attribute (Enter de ket thuc): "))
            (if (null en)
              (setq done T)
              (progn
                (setq en (car en))
                (if (/= (cdr (assoc 0 (entget en))) "INSERT")
                  (princ "\n  !! Doi tuong khong phai block, chon lai.")
                  (progn
                    (setq nm (tat-bname en))
                    (cond
                      ((or (null nm) (vl-catch-all-error-p nm))
                       (princ "\n  !! Khong doc duoc ten block."))
                      ((member (strcase nm) donelist)
                       (princ (strcat "\n  Block \"" nm "\" da xu ly roi, bo qua.")))
                      (t
                       (setq bo (vl-catch-all-apply 'vla-Item (list blks nm)))
                       (if (vl-catch-all-error-p bo)
                         (princ (strcat "\n  !! Khong mo duoc dinh nghia block \"" nm "\"."))
                         (progn
                           (princ (strcat "\nBlock \"" nm "\":"))
                           (vla-StartUndoMark doc)
                           (setq undoon T)
                           (setq k (tat-natt bo) nadd 0 nskip 0 i 0)
                           (foreach tg tags
                             (setq df (nth i defs))
                             (if (not (tat-blank-p tg))
                               (if (tat-hastag bo tg)
                                 (progn
                                   (princ (strcat "\n    - Tag \"" tg "\" da co san, bo qua."))
                                   (setq nskip (1+ nskip))
                                 )
                                 (progn
                                   (setq pt (list 0.0 (* (- 0 k) *TAT-GAP*) 0.0))
                                   (if (tat-add bo pt tg df md sty lok)
                                     (progn
                                       (princ (strcat "\n    + Da them tag \"" tg "\""
                                                      (if (tat-blank-p df) ""
                                                        (strcat "  (default: " df ")"))))
                                       (setq nadd (1+ nadd) k (1+ k))
                                     )
                                   )
                                 )
                               )
                             )
                             (setq i (1+ i))
                           )
                           (vla-EndUndoMark doc)
                           (setq undoon nil)
                           (if (> nadd 0)
                             (progn
                               (if syn (tat-sync nm))
                               (setq cnt (1+ cnt))
                             )
                             (princ "\n  Khong co tag moi nao duoc them.")
                           )
                           (setq donelist (cons (strcase nm) donelist))
                         )
                       )
                      )
                    )
                  )
                )
              )
            )
          )
          (vl-catch-all-apply 'vla-Regen (list doc acAllViewports))
          (princ (strcat "\n-----------------------------"
                         "\nDa xu ly " (itoa cnt) " block."))
          (if (not syn)
            (princ "\nChua chay ATTSYNC - hay chay lenh ATTS de cap nhat cac block da chen.")
          )
        )
      )
      (princ)
    )
  )
  (princ)
)

(princ "\n=== TAT v3 da nap: tick chon tung Tag, Tag3 mac dinh Distance1 - Go TAT de chay ===")
(princ)

;;; --- HET [14] TAT ---

;;; ---------------------------------------------------------------------------
;;; [15] ATTS
;;;      Dong bo Attribute, nhan ca block chua co att
;;;      Nguon: ATTS(SYNBLOCKATT).lsp
;;; ---------------------------------------------------------------------------
;;; ================================================================
;;; ATTS - Dong bo thuoc tinh block (ATTSYNC) voi giao dien DCL
;;; *** BAN SUA LOI v4 ***
;;;   - SUA LOI "0 found" khi quet chon: block moi them ATTDEF nhung
;;;     CHUA sync lan nao thi reference chua co ATTRIB -> filter cu
;;;     (66 . 1) loai het. Gio nhan moi INSERT va loc theo DINH NGHIA
;;;     co ATTDEF (du dieu kien sync du ref co att hay chua).
;;;   - Danh sach ten block cung quet tu bang dinh nghia (khong tu ref)
;;; (v3): ATTSYNC _Select tung entity, kiem tra ATTDEF truc tiep,
;;;       in ro [Chi tiet loi]
;;; (v2): bo vong lap "Yes", dung command-s
;;; 3 che do:
;;;   1. Quet chon block tren man hinh (ATTSYNC Select tung block)
;;;   2. Chon block theo ten tu danh sach (ATTSYNC Name)
;;;   3. Ap dung cho tat ca block trong ban ve (ATTSYNC Name)
;;; Cach dung: go lenh ATTS
;;; ================================================================

(vl-load-com)

;; ---------- Lay ten block (ho tro dynamic - EffectiveName) ----------
(defun atts:ename->blkname (ent / obj)
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'EffectiveName)
    (vla-get-EffectiveName obj)
    (vla-get-Name obj)
  )
)

;; ---------- Kiem tra dinh nghia block co thuoc tinh khong ----------
;; *** v3: co bit 2 cua bang BLOCK co the stale voi block dong ->
;;     kiem tra them bang cach quet truc tiep ATTDEF trong dinh nghia
(defun atts:has-attdef (name / blk ent found)
  (setq blk (tblsearch "BLOCK" name))
  (cond
    ((not blk) nil)
    ;; Co bit 2 -> chac chan co thuoc tinh
    ((= 2 (logand 2 (cdr (assoc 70 blk)))) T)
    ;; Co stale? Quet entity trong dinh nghia tim ATTDEF
    (t
     (setq ent (tblobjname "BLOCK" name))
     (setq found nil)
     (if ent
       (progn
         (setq ent (cdr (assoc -2 (entget ent))))  ; entity dau tien
         (while (and ent (not found))
           (if (= (cdr (assoc 0 (entget ent))) "ATTDEF")
             (setq found T)
           )
           (setq ent (entnext ent))
         )
       )
     )
     found
    )
  )
)

;; ---------- Danh sach ten block co thuoc tinh trong ban ve ----------
;; *** v4: quet tu BANG DINH NGHIA block (co ATTDEF) thay vi tu cac
;;     INSERT dang co att - vi block moi them ATTDEF nhung chua sync
;;     lan nao thi INSERT chua co ATTRIB, quet kieu cu se bo sot
(defun atts:all-blknames (/ blk name lst)
  (setq lst '())
  (setq blk (tblnext "BLOCK" T))
  (while blk
    (setq name (cdr (assoc 2 blk)))
    (if (and name
             (/= (substr name 1 1) "*")               ; bo an danh/layout
             (/= 4 (logand 4 (cdr (assoc 70 blk))))   ; bo xref
             (atts:has-attdef name)
        )
      (setq lst (cons name lst))
    )
    (setq blk (tblnext "BLOCK"))
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Lay ten block (khong trung) tu tap chon ----------
(defun atts:ss->blknames (ss / i name lst)
  (setq i 0)
  (while (< i (sslength ss))
    (setq name (atts:ename->blkname (ssname ss i)))
    (if (not (member (strcase name) (mapcar 'strcase lst)))
      (setq lst (cons name lst))
    )
    (setq i (1+ i))
  )
  lst
)

;; ---------- Goi ATTSYNC theo TEN an toan cho 1 block ----------
;; Tra ve T neu thanh cong, nil neu loi (in ro loi that de chan doan)
(defun atts:sync-one (name / rs)
  (setq rs
    (vl-catch-all-apply
      'command-s
      (list "_.ATTSYNC" "_Name" name)
    )
  )
  ;; De phong AutoCAD cu khong co command-s (truoc 2015)
  (if (and (vl-catch-all-error-p rs)
           (wcmatch (strcase (vl-catch-all-error-message rs)) "*COMMAND-S*")
      )
    (progn
      (setq rs (vl-catch-all-apply
                 '(lambda () (command "_.ATTSYNC" "_Name" name))))
      (while (> (getvar "CMDACTIVE") 0) (command))
    )
  )
  ;; *** v3: in ro loi that thay vi im lang
  (if (vl-catch-all-error-p rs)
    (progn
      (prompt (strcat "\n    [Chi tiet loi] " (vl-catch-all-error-message rs)))
      nil
    )
    T
  )
)

;; ---------- *** v3: ATTSYNC theo TUNG ENTITY (che do Select) ----------
;; Danh trung dung reference da pick - khong qua ten, xu ly duoc ca
;; ban the an danh *U cua block dong. Tra ve (n-ok n-fail).
(defun atts:sync-ents (entlist / rs n-ok n-fail e)
  (setq n-ok 0 n-fail 0)
  (foreach e entlist
    (setq rs
      (vl-catch-all-apply
        'command-s
        (list "_.ATTSYNC" "_Select" e "_Yes")
      )
    )
    (if (and (vl-catch-all-error-p rs)
             (wcmatch (strcase (vl-catch-all-error-message rs)) "*COMMAND-S*")
        )
      (progn
        (setq rs (vl-catch-all-apply
                   '(lambda () (command "_.ATTSYNC" "_Select" e "_Yes"))))
        (while (> (getvar "CMDACTIVE") 0) (command))
      )
    )
    (if (vl-catch-all-error-p rs)
      (progn
        (setq n-fail (1+ n-fail))
        (prompt (strcat "\n    [Chi tiet loi] " (vl-catch-all-error-message rs)))
      )
      (setq n-ok (1+ n-ok))
    )
  )
  (list n-ok n-fail)
)

;; ---------- Loc entity: chi giu moi ten block 1 dai dien ----------
;; (ATTSYNC Select tren 1 ref se sync TAT CA ref cung ten, nen moi
;;  ten chi can goi 1 lan - do chay lai thua nhieu lan)
(defun atts:ss->rep-ents (ss / i ent name names ents)
  (setq i 0 names '() ents '())
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq name (strcase (atts:ename->blkname ent)))
    (if (not (member name names))
      (progn
        (setq names (cons name names))
        (setq ents (cons ent ents))
      )
    )
    (setq i (1+ i))
  )
  (reverse ents)
)

;; ---------- Chay ATTSYNC cho danh sach ten block ----------
(defun atts:sync-names (namelist / old-cmdecho n-ok n-skip)
  (setq old-cmdecho (getvar "CMDECHO")
        n-ok   0
        n-skip 0)
  (setvar "CMDECHO" 0)
  (foreach name namelist
    (cond
      ;; Khong tim thay dinh nghia hoac khong co thuoc tinh
      ((not (atts:has-attdef name))
       (setq n-skip (1+ n-skip))
       (prompt (strcat "\n  - Bo qua \"" name
                       "\" (khong tim thay dinh nghia block co thuoc tinh)."))
      )
      ;; Sync thanh cong
      ((atts:sync-one name)
       (setq n-ok (1+ n-ok))
       (prompt (strcat "\n  + Da ATTSYNC block: " name))
      )
      ;; ATTSYNC bao loi
      (t
       (setq n-skip (1+ n-skip))
       (prompt (strcat "\n  - LOI khi ATTSYNC block: " name))
      )
    )
  )
  (setvar "CMDECHO" old-cmdecho)
  (vl-cmdf "_.REGENALL")
  (prompt (strcat "\n==> Hoan thanh: "
                  (itoa n-ok) " block dong bo OK"
                  (if (> n-skip 0)
                    (strcat ", " (itoa n-skip) " block bo qua/loi.")
                    "."
                  )))
)

;; ---------- Tao file DCL tam ----------
(defun atts:make-dcl (/ fname f)
  (setq fname (vl-filename-mktemp "atts" nil ".dcl")
        f     (open fname "w"))
  (foreach line
    '("atts : dialog {"
      "  label = \"ATTS v4 - Dong bo thuoc tinh block (ATTSYNC)\";"
      "  : boxed_radio_column {"
      "    label = \"Pham vi ap dung\";"
      "    : radio_button { key = \"m_pick\"; label = \"Quet chon block tren man hinh\"; }"
      "    : radio_button { key = \"m_name\"; label = \"Chon block theo ten:\"; }"
      "    : row {"
      "      : spacer { width = 2; }"
      "      : popup_list { key = \"blklist\"; edit_width = 34; }"
      "    }"
      "    : radio_button { key = \"m_all\"; label = \"Tat ca block trong ban ve\"; }"
      "  }"
      "  spacer;"
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
(defun c:ATTS (/ dclfile dclid allnames mode idx ss ret ret2 old-cmdecho entlist nskip e)
  (if (= 1 (getvar "BLOCKEDITOR"))
    (prompt "\n*** Dang o trong Block Editor, hay dong lai truoc khi ATTSYNC! ***")
    (progn
      (setq allnames (atts:all-blknames))
      (if (not allnames)
        (prompt "\n*** Ban ve khong co block thuoc tinh nao! ***")
        (progn
          ;; ----- Nap dialog -----
          (setq dclfile (atts:make-dcl)
                dclid   (load_dialog dclfile))
          (if (not (new_dialog "atts" dclid))
            (prompt "\n*** Loi: khong mo duoc hop thoai DCL! ***")
            (progn
              ;; Gia tri mac dinh (nho lai lua chon lan truoc)
              (setq mode (if *atts-mode* *atts-mode* "m_pick")
                    idx  (if *atts-idx* *atts-idx* 0))
              (if (>= idx (length allnames)) (setq idx 0))

              (start_list "blklist")
              (mapcar 'add_list allnames)
              (end_list)
              (set_tile "blklist" (itoa idx))
              (set_tile mode "1")
              (mode_tile "blklist" (if (= mode "m_name") 0 1))

              (action_tile "m_pick" "(setq mode \"m_pick\") (mode_tile \"blklist\" 1)")
              (action_tile "m_name" "(setq mode \"m_name\") (mode_tile \"blklist\" 0)")
              (action_tile "m_all"  "(setq mode \"m_all\")  (mode_tile \"blklist\" 1)")
              (action_tile "blklist" "(setq idx (atoi $value))")
              (action_tile "accept" "(setq idx (atoi (get_tile \"blklist\"))) (done_dialog 1)")
              (action_tile "cancel" "(done_dialog 0)")

              (setq ret (start_dialog))
              (unload_dialog dclid)
              (vl-file-delete dclfile)

              ;; ----- Xu ly theo lua chon -----
              (if (= ret 1)
                (progn
                  (setq *atts-mode* mode
                        *atts-idx*  idx)
                  (cond
                    ;; 1. Quet chon tren man hinh
                    ;; *** v4: nhan MOI INSERT (khong doi (66 . 1)) vi
                    ;; block moi them ATTDEF chua sync thi ref CHUA co
                    ;; att - loc theo DINH NGHIA co ATTDEF la du
                    ((= mode "m_pick")
                     (prompt "\nQuet chon cac block can dong bo thuoc tinh: ")
                     (setq ss (ssget '((0 . "INSERT"))))
                     (if ss
                       (progn
                         (setq entlist '() nskip 0)
                         (foreach e (atts:ss->rep-ents ss)
                           (if (atts:has-attdef (atts:ename->blkname e))
                             (setq entlist (cons e entlist))
                             (setq nskip (1+ nskip))
                           )
                         )
                         (if (not entlist)
                           (prompt "\n*** Cac block da chon deu khong co ATTDEF trong dinh nghia! ***")
                           (progn
                             (setq old-cmdecho (getvar "CMDECHO"))
                             (setvar "CMDECHO" 0)
                             (setq ret2 (atts:sync-ents (reverse entlist)))
                             (setvar "CMDECHO" old-cmdecho)
                             (vl-cmdf "_.REGENALL")
                             (prompt (strcat "\n==> Hoan thanh: "
                                             (itoa (car ret2)) " block dong bo OK"
                                             (if (> (cadr ret2) 0)
                                               (strcat ", " (itoa (cadr ret2)) " loi")
                                               ""
                                             )
                                             (if (> nskip 0)
                                               (strcat ", " (itoa nskip)
                                                       " ten block khong co ATTDEF (bo qua).")
                                               "."
                                             )))
                           )
                         )
                       )
                       (prompt "\n*** Khong chon duoc doi tuong nao! ***")
                     )
                    )
                    ;; 2. Theo ten tu danh sach
                    ((= mode "m_name")
                     (atts:sync-names (list (nth idx allnames)))
                    )
                    ;; 3. Tat ca block trong ban ve
                    ((= mode "m_all")
                     (atts:sync-names allnames)
                    )
                  )
                )
                (prompt "\n* Da huy lenh. *")
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(prompt "\nDa nap lenh ATTS v4 - quet chon nhan ca block CHUA co att (chi can dinh nghia co ATTDEF).")
(princ)

;;; --- HET [15] ATTS ---

;;; ---------------------------------------------------------------------------
;;; [16] BET
;;;      Sua nhanh Attribute cua block
;;;      Nguon: BET(SUANBLOCKATT).lsp
;;; ---------------------------------------------------------------------------
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

;;; --- HET [16] BET ---

;;; ---------------------------------------------------------------------------
;;; [17] SYNCATT / CPATT
;;;      Copy block va tang dan gia tri Attribute
;;;      Nguon: COPYTANGDAN(CPATT).lsp
;;; ---------------------------------------------------------------------------
;;======================================================================
;; BLOCKTOOLS.LSP
;; Gom 2 lenh xu ly Attribute cua Block:
;;   SYNCATT - Dong bo (cap nhat) toan bo Attribute cho Block sau khi
;;             bo sung ATTDEF moi (dua tren -ATTSYNC cua AutoCAD)
;;   CPATT   - Copy/gan gia tri Attribute tang dan cho nhieu Block
;;             cung loai (chon Tag, chon huong, prefix, so bat dau)
;; Cach dung: APPLOAD file nay 1 lan, sau do go SYNCATT hoac CPATT
;;======================================================================

(vl-load-com)

;;----------------------------------------------------------------------
;; LENH 1: SYNCATT
;;----------------------------------------------------------------------
(defun SyncBlockAtt (blkName / )
  (if (tblsearch "BLOCK" blkName)
    (progn
      (command "_.-attsync" "_Name" blkName)
      (princ (strcat "\n>> Da dong bo: " blkName)))
    (princ (strcat "\n>> Khong tim thay block: " blkName)))
  (princ))

(defun c:SYNCATT (/ *error* oldEcho oldAttReq ss i ent obj blkName blkList)
  (defun *error* (msg)
    (setvar "CMDECHO" oldEcho)
    (setvar "ATTREQ" oldAttReq)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nLoi: " msg)))
    (princ))

  (setq oldEcho   (getvar "CMDECHO")
        oldAttReq (getvar "ATTREQ"))
  (setvar "CMDECHO" 0)

  (princ "\nChon 1 hoac nhieu Block-Reference can dong bo (Enter = nhap ten tay): ")
  (setq ss (ssget '((0 . "INSERT"))))

  (setq blkList nil i 0)
  (if ss
    (repeat (sslength ss)
      (setq ent (ssname ss i)
            obj (vlax-ename->vla-object ent)
            blkName (cdr (assoc 2 (entget ent))))
      ;; Block an danh (Dynamic Block) -> lay ten thuc te (EffectiveName)
      (if (wcmatch blkName "`**")
        (setq blkName (vla-get-effectivename obj)))
      (if (not (member blkName blkList))
        (setq blkList (cons blkName blkList)))
      (setq i (1+ i)))
    (progn
      (initget 1)
      (setq blkName (getstring T "\nNhap ten Block can dong bo: "))
      (setq blkList (list blkName))))

  (foreach b blkList (SyncBlockAtt b))

  (setvar "CMDECHO" oldEcho)
  (setvar "ATTREQ" oldAttReq)
  (princ (strcat "\n>> Hoan tat dong bo " (itoa (length blkList)) " block."))
  (princ)
)

;;----------------------------------------------------------------------
;; LENH 2: CPATT
;;----------------------------------------------------------------------

;; Lay list (tag . attref-object) cua 1 block reference
(defun GetAtts (obj / atts a lst)
  (setq lst nil)
  (if (vlax-property-available-p obj 'HasAttributes)
    (if (= (vla-get-hasattributes obj) :vlax-true)
      (progn
        (setq atts (vlax-safearray->list
                     (vlax-variant-value (vla-getattributes obj))))
        (foreach a atts
          (setq lst (cons (cons (strcase (vla-get-tagstring a)) a) lst)))
        (reverse lst)))
    nil)
  lst)

;; Ghi 1 file text ra dia, tra ve path neu thanh cong, nil neu that bai
(defun CPATT-WriteFile (path text / f)
  (setq f nil)
  (if (setq f (open path "w"))
    (progn (write-line text f) (close f) path)
    nil))

;; Tao file DCL chua 2 dialog: chon Che do/Huong (buoc 1) va Tag/Prefix/So (buoc 2)
;; Uu tien ghi vao TEMP, neu that bai (thuong do duong dan TEMP chua ky tu
;; tieng Viet/Unicode gay loi load_dialog) thi fallback sang goc o dia C.
(defun CPATT-WriteDCL ( / dclText p1 p2 result)
  (setq dclText (strcat
    "cpatt_mode_dlg : dialog { label = \"CPATT - Buoc 1: Chon che do\";"
    "  : boxed_radio_column { label = \"Cach lay doi tuong\";"
    "    : radio_button { key = \"mode_pick\"; label = \"Pick - chon lan luot tung block theo thu tu\"; }"
    "    : radio_button { key = \"mode_window\"; label = \"Window - quet chon vung roi tu sap xep\"; }"
    "  }"
    "  : boxed_radio_column { label = \"Huong sap xep (chi dung khi chon Window)\";"
    "    : radio_button { key = \"order_xtang\"; label = \"X tang dan\"; }"
    "    : radio_button { key = \"order_xgiam\"; label = \"X giam dan\"; }"
    "    : radio_button { key = \"order_ytang\"; label = \"Y tang dan\"; }"
    "    : radio_button { key = \"order_ygiam\"; label = \"Y giam dan\"; }"
    "    : radio_button { key = \"order_2diem\"; label = \"Pick 2 diem - huong bat ky\"; }"
    "  }"
    "  spacer;"
    "  ok_cancel;"
    "}"
    "cpatt_data_dlg : dialog { label = \"CPATT - Buoc 2: Thong tin gan Attribute\";"
    "  : popup_list { key = \"taglist\"; label = \"Chon Tag can tang:\"; }"
    "  : edit_box { key = \"prefix\"; label = \"Prefix:\"; edit_width = 15; }"
    "  : edit_box { key = \"start\"; label = \"So bat dau:\"; edit_width = 15; }"
    "  : edit_box { key = \"step\"; label = \"Buoc nhay:\"; edit_width = 15; }"
    "  spacer;"
    "  ok_cancel;"
    "}"))

  (setq p1 (strcat (getenv "TEMP") "\\cpatt_dlg.dcl"))
  (setq result (CPATT-WriteFile p1 dclText))
  (if (not result)
    (progn
      (setq p2 "C:\\cpatt_dlg.dcl")
      (setq result (CPATT-WriteFile p2 dclText))))
  result)

(defun c:CPATT (/ *error* dcl_id dclPath dlgOK mode order
                  ss i n ent obj attList tagList tagChon
                  prefix start step val entObjs idx doneFlag
                  e obj2 pair pt1 pt2 vecX vecY vecLen objPairs)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nLoi: " msg)))
    (princ))

  ;;--- Chuan bi file DCL (dung chung cho ca 2 buoc) ---
  (setq dclPath (CPATT-WriteDCL))
  (if (not dclPath)
    (progn (princ "\n>> Khong the tao file DCL.") (exit)))

  ;;===================================================================
  ;; BUOC 1: hien dialog NGAY KHI VAO LENH - chon Mode + Huong sap xep
  ;;===================================================================
  (setq dcl_id (load_dialog dclPath))
  (if (or (not dcl_id) (< dcl_id 0))
    (progn (princ "\n>> Khong the load dialog (kiem tra duong dan co ky tu dac biet).") (exit)))

  (if (not (new_dialog "cpatt_mode_dlg" dcl_id))
    (progn (unload_dialog dcl_id) (princ "\n>> Khong the mo dialog buoc 1.") (exit)))

  (setq mode "Pick" order "Xtang")
  (set_tile "mode_pick"   "1")
  (set_tile "order_xtang" "1")

  (action_tile "mode_pick"    "(setq mode \"Pick\")")
  (action_tile "mode_window"  "(setq mode \"Window\")")
  (action_tile "order_xtang"  "(setq order \"Xtang\")")
  (action_tile "order_xgiam"  "(setq order \"Xgiam\")")
  (action_tile "order_ytang"  "(setq order \"Ytang\")")
  (action_tile "order_ygiam"  "(setq order \"Ygiam\")")
  (action_tile "order_2diem"  "(setq order \"2Diem\")")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (setq dlgOK (= (start_dialog) 1))
  (unload_dialog dcl_id)
  (if (not dlgOK)
    (progn (princ "\n>> Da huy lenh.") (exit)))

  ;;===================================================================
  ;; BUOC 2: thao tac tren man hinh - chon doi tuong / pick huong
  ;;===================================================================
  (setq entObjs nil)

  (cond
    ;;------------------ CHE DO PICK TUNG CAI THEO THU TU ------------------
    ((= mode "Pick")
     (princ "\nChon lan luot cac Block theo thu tu tang dan (Enter de ket thuc):")
     (setq doneFlag nil idx 0)
     (while (not doneFlag)
       (setq e (car (entsel (strcat "\nChon block thu " (itoa (1+ idx)) ": "))))
       (if e
         (progn
           (setq obj2 (vlax-ename->vla-object e))
           (if (and (= (vla-get-objectname obj2) "AcDbBlockReference")
                    (= (vla-get-hasattributes obj2) :vlax-true))
             (progn
               (setq entObjs (append entObjs (list obj2)))
               (setq idx (1+ idx))
               (princ (strcat " -> OK (" (itoa idx) ")")))
             (princ "\n>> Doi tuong khong phai Block co Attribute, bo qua.")))
         (setq doneFlag T))))

    ;;------------------ CHE DO CHON VUNG + SAP XEP ------------------
    ((= mode "Window")
     (setq ss (ssget '((0 . "INSERT"))))
     (if (not ss) (progn (princ "\nKhong co doi tuong nao duoc chon.") (exit)))
     (setq i 0)
     (repeat (sslength ss)
       (setq ent (ssname ss i)
             obj (vlax-ename->vla-object ent))
       (if (= (vla-get-hasattributes obj) :vlax-true)
         (setq entObjs (append entObjs (list obj))))
       (setq i (1+ i)))

     (if (= order "2Diem")
       ;;--- Sap xep theo huong bat ky bang cach pick 2 diem ---
       (progn
         (setq pt1 (getpoint "\nChon diem dau (moc bat dau tang dan): "))
         (if (not pt1) (progn (princ "\n>> Da huy lenh.") (exit)))
         (setq pt2 (getpoint pt1 "\nChon diem cuoi (huong tang dan): "))
         (if (not pt2) (progn (princ "\n>> Da huy lenh.") (exit)))
         (setq vecX (- (car pt2) (car pt1))
               vecY (- (cadr pt2) (cadr pt1))
               vecLen (sqrt (+ (* vecX vecX) (* vecY vecY))))
         (if (< vecLen 1e-8)
           (progn (princ "\n>> Hai diem trung nhau, huy lenh.") (exit)))

         ;; Tinh truoc gia tri hinh chieu cho tung block roi ghep cap
         ;; (gia_tri_chieu . doi_tuong) - tranh dung ham phu/dong bien
         ;; de khong bi loi tham chieu bien khi vl-sort goi lai.
         (setq objPairs
           (mapcar
             (function
               (lambda (o / p dx dy)
                 (setq p  (vlax-get o 'insertionpoint)
                       dx (- (car p) (car pt1))
                       dy (- (cadr p) (cadr pt1)))
                 (cons (/ (+ (* dx vecX) (* dy vecY)) vecLen) o)))
             entObjs))

         (setq objPairs
           (vl-sort objPairs (function (lambda (p1 p2) (< (car p1) (car p2))))))

         (setq entObjs (mapcar 'cdr objPairs)))

       ;;--- Cac huong truc chuan X/Y ---
       (setq entObjs
         (vl-sort entObjs
           (cond
             ((= order "Xtang") (function (lambda (a b) (< (car (vlax-get a 'insertionpoint)) (car (vlax-get b 'insertionpoint))))))
             ((= order "Xgiam") (function (lambda (a b) (> (car (vlax-get a 'insertionpoint)) (car (vlax-get b 'insertionpoint))))))
             ((= order "Ytang") (function (lambda (a b) (< (cadr (vlax-get a 'insertionpoint)) (cadr (vlax-get b 'insertionpoint))))))
             ((= order "Ygiam") (function (lambda (a b) (> (cadr (vlax-get a 'insertionpoint)) (cadr (vlax-get b 'insertionpoint))))))))))
     ))

  (if (< (length entObjs) 1)
    (progn (princ "\nKhong co Block hop le nao duoc chon.") (exit)))

  ;;===================================================================
  ;; BUOC 3: hien dialog thu 2 - chon Tag + nhap Prefix/Start/Step
  ;;===================================================================
  (setq attList (GetAtts (car entObjs)))
  (if (not attList)
    (progn (princ "\nBlock dau tien khong co Attribute.") (exit)))
  (setq tagList (mapcar 'car attList))

  (setq dcl_id (load_dialog dclPath))
  (if (or (not dcl_id) (< dcl_id 0))
    (progn (princ "\n>> Khong the load dialog buoc 2.") (exit)))

  (if (not (new_dialog "cpatt_data_dlg" dcl_id))
    (progn (unload_dialog dcl_id) (princ "\n>> Khong the mo dialog buoc 2.") (exit)))

  (start_list "taglist")
  (mapcar 'add_list tagList)
  (end_list)
  (set_tile "taglist" "0")
  (set_tile "prefix" "")
  (set_tile "start" "1")
  (set_tile "step" "1")

  (setq dlgOK nil)
  (action_tile "accept"
    "(setq tagChon (nth (atoi (get_tile \"taglist\")) tagList))
     (setq prefix (get_tile \"prefix\"))
     (setq start (atoi (get_tile \"start\")))
     (setq step (atoi (get_tile \"step\")))
     (setq dlgOK T)
     (done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (start_dialog)
  (unload_dialog dcl_id)

  (if (not dlgOK)
    (progn (princ "\n>> Da huy lenh.") (exit)))
  (if (not tagChon)
    (progn (princ "\nLua chon Tag khong hop le.") (exit)))

  ;;===================================================================
  ;; BUOC 4: gan gia tri tang dan
  ;;===================================================================
  (setq i 0)
  (foreach obj entObjs
    (setq attList (GetAtts obj))
    (setq pair (assoc tagChon attList))
    (if pair
      (progn
        (setq val (strcat prefix (itoa (+ start (* i step)))))
        (vla-put-textstring (cdr pair) val)
        (princ (strcat "\n  " (itoa (1+ i)) ". Tag[" tagChon "] = " val)))
      (princ (strcat "\n  >> Block " (itoa (1+ i)) " khong co Tag [" tagChon "], bo qua.")))
    (setq i (1+ i)))

  (command "_.REGEN")
  (princ (strcat "\n\n>> Hoan tat, da cap nhat " (itoa (length entObjs)) " block."))
  (princ)
)

(princ "\n>> Da nap BLOCKTOOLS.LSP - Lenh: SYNCATT, CPATT")
(princ)

;;; --- HET [17] SYNCATT / CPATT ---

;;; ---------------------------------------------------------------------------
;;; [18] DTEN
;;;      Danh so / danh ten Attribute hang loat theo thu tu
;;;      Nguon: DanhTenAtt (DTEN).lsp
;;; ---------------------------------------------------------------------------
;;; ============================================================
;;; DANH TEN TU DONG CHO ATTRIBUTE - CO GIAO DIEN (DCL)
;;; Lenh: DTEN
;;; Dinh dang ten: [Tien to] + [So thu tu] + [Hau to]

;;; ============================================================

(vl-load-com)

;; Bien toan cuc luu gia tri lan truoc
(if (null *dten-tag*)  (setq *dten-tag*  "NAME"))
(if (null *dten-pre*)  (setq *dten-pre*  "KC-"))
(if (null *dten-num*)  (setq *dten-num*  "1"))
(if (null *dten-pad*)  (setq *dten-pad*  "2"))
(if (null *dten-suf*)  (setq *dten-suf*  ""))
(if (null *dten-mode*) (setq *dten-mode* "c_sel"))
(if (null *dten-ord*)  (setq *dten-ord*  "o_xy"))
(if (null *dten-tol*)  (setq *dten-tol*  "1"))
(if (null *dten-rev*)  (setq *dten-rev*  nil))
(if (null *dten-grp*)  (setq *dten-grp*  nil))         ; gom nhom
(if (null *dten-par*)  (setq *dten-par*  "Distance1")) ; ten parameter
(if (null *dten-gtol*) (setq *dten-gtol* "0.1"))       ; dung sai so sanh gia tri

;; --- Kiem tra chuoi rong / toan dau cach ---
(defun dten-blank-p (s) (= (vl-string-trim " " s) ""))

;; --- Tao chuoi so co padding ---
(defun dten-numstr (n p / s)
  (setq s (itoa n))
  (while (< (strlen s) p) (setq s (strcat "0" s)))
  s
)

;; --- Ghep ten day du ; n = nil -> khong danh so ---
(defun dten-fullname (pre n pad suf)
  (if n (strcat pre (dten-numstr n pad) suf) (strcat pre suf))
)

;; --- Diem chen cua block (WCS) ---
(defun dten-ip (en / r)
  (setq r (vl-catch-all-apply
            'vlax-safearray->list
            (list (vlax-variant-value
                    (vla-get-InsertionPoint (vlax-ename->vla-object en))))))
  (if (vl-catch-all-error-p r) (list 0.0 0.0 0.0) r)
)

;; --- Doc gia tri parameter de gom nhom ---
;;  1. Tim trong parameter cua dynamic block
;;  2. Khong co -> tim attribute co tag trung ten
(defun dten-parval (en par / obj r v s)
  (setq obj (vlax-ename->vla-object en) v nil)
  (setq r (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
  (if (not (vl-catch-all-error-p r))
    (foreach p r
      (if (null v)
        (progn
          (setq s (vl-catch-all-apply 'vla-get-PropertyName (list p)))
          (if (and (not (vl-catch-all-error-p s))
                   (= (strcase s) (strcase par)))
            (progn
              (setq v (vl-catch-all-apply
                        'vlax-variant-value (list (vla-get-Value p))))
              (if (vl-catch-all-error-p v) (setq v nil))
            )
          )
        )
      )
    )
  )
  (if (null v)
    (if (= (vla-get-HasAttributes obj) :vlax-true)
      (foreach a (vlax-invoke obj 'GetAttributes)
        (if (and (null v) (= (strcase (vla-get-TagString a)) (strcase par)))
          (setq v (vla-get-TextString a))
        )
      )
    )
  )
  v
)

;; --- So sanh 2 gia tri (so hoac chuoi) ---
(defun dten-veq (a b tol)
  (cond
    ((and (numberp a) (numberp b)) (equal a b tol))
    ((and (= (type a) 'STR) (= (type b) 'STR)) (= (strcase a) (strcase b)))
    (t nil)
  )
)

;; --- Tim ten da gan cho gia tri nay ---
(defun dten-gfind (v glist tol / found)
  (foreach it glist
    (if (and (null found) (dten-veq (car it) v tol)) (setq found (cdr it)))
  )
  found
)

;; --- Hien thi gia tri de in bao cao ---
(defun dten-vstr (v)
  (cond
    ((null v) "(khong doc duoc)")
    ((numberp v) (rtos v 2 3))
    ((= (type v) 'STR) v)
    (t "?")
  )
)

;; --- Tang so dem trong bao cao ---
(defun dten-bump (rep nm)
  (mapcar '(lambda (r)
             (if (= (car r) nm) (list (car r) (cadr r) (1+ (caddr r))) r))
          rep)
)

;; --- Gan ten vao attribute co tag chi dinh ; T neu gan duoc ---
(defun dten-set (en tag str / obj atts found)
  (setq found nil)
  (if (and en (= (cdr (assoc 0 (entget en))) "INSERT"))
    (progn
      (setq obj (vlax-ename->vla-object en))
      (if (= (vla-get-HasAttributes obj) :vlax-true)
        (progn
          (setq atts (vlax-invoke obj 'GetAttributes))
          (foreach att atts
            (if (= (strcase (vla-get-TagString att)) tag)
              (progn
                (vl-catch-all-apply 'vla-put-TextString (list att str))
                (setq found T)
              )
            )
          )
          (if found (vl-catch-all-apply 'vla-Update (list obj)))
        )
      )
    )
  )
  found
)

;; --- Sap xep tap chon theo toa do ---
(defun dten-sort (ss ord tol rev / i en p lst k1 k2 out)
  (setq i 0 lst nil)
  (while (< i (sslength ss))
    (setq en (ssname ss i) p (dten-ip en))
    (cond
      ((= ord "o_yx")
       (setq k1 (if (> tol 0.0) (fix (/ (+ (cadr p) (/ tol 2.0)) tol)) (cadr p))
             k2 (car p)))
      (t
       (setq k1 (if (> tol 0.0) (fix (/ (+ (car p) (/ tol 2.0)) tol)) (car p))
             k2 (cadr p)))
    )
    (setq lst (cons (list k1 k2 i en) lst))
    (setq i (1+ i))
  )
  (setq lst (reverse lst))
  (if (= ord "o_ord")
    (setq out (mapcar 'cadddr lst))
    (setq out
      (mapcar 'cadddr
        (vl-sort lst
          '(lambda (a b)
             (cond
               ((< (car a) (car b)) T)
               ((> (car a) (car b)) nil)
               ((< (cadr a) (cadr b)) T)
               ((> (cadr a) (cadr b)) nil)
               (t (< (caddr a) (caddr b)))
             ))))))
  (if rev (reverse out) out)
)

;; --- Cap nhat dong xem truoc ---
(defun dten-preview (/ numstr n p)
  (setq numstr (get_tile "num") p (atoi (get_tile "pad")))
  (if (< p 1) (setq p 1))
  (if (dten-blank-p numstr)
    (set_tile "preview"
      (strcat "Xem truoc: "
              (dten-fullname (get_tile "pre") nil p (get_tile "suf"))
              "  (khong danh so, ten giong nhau)"))
    (progn
      (setq n (atoi numstr))
      (if (< n 0) (setq n 0))
      (set_tile "preview"
        (strcat "Xem truoc: "
                (dten-fullname (get_tile "pre") n p (get_tile "suf"))
                "  ->  "
                (dten-fullname (get_tile "pre") (1+ n) p (get_tile "suf"))
                "  -> ..."))
    )
  )
)

;; --- Tao file DCL tam ---
(defun dten-makedcl (/ fn f)
  (setq fn (vl-filename-mktemp "danhten" nil ".dcl")
        f  (open fn "w")
  )
  (foreach s
    (list
      "danhten : dialog {"
      "  label = \"Danh ten tu dong Attribute - v5\";"
      "  : boxed_column {"
      "    label = \"Thiet lap ten\";"
      "    : edit_box { key=\"tag\"; label=\"Tag attribute:\"; edit_width=18; }"
      "    : edit_box { key=\"pre\"; label=\"Tien to (prefix):\"; edit_width=18; }"
      "    : row {"
      "      : edit_box { key=\"num\"; label=\"So bat dau:\"; edit_width=6; }"
      "      : edit_box { key=\"pad\"; label=\"So chu so:\"; edit_width=6; }"
      "    }"
      "    : text { label=\"(De trong o So bat dau neu KHONG muon danh so thu tu)\"; width=56; }"
      "    : edit_box { key=\"suf\"; label=\"Hau to (suffix):\"; edit_width=18; }"
      "    : text { key=\"preview\"; label=\" \"; width=56; }"
      "  }"
      "  : boxed_radio_column {"
      "    label = \"Cach chon doi tuong\";"
      "    : radio_button { key=\"c_sel\"; label=\"Quet chon nhieu doi tuong mot luot\"; }"
      "    : radio_button { key=\"c_pick\"; label=\"Chon tung cai lien tiep\"; }"
      "  }"
      "  : boxed_radio_column {"
      "    label = \"Thu tu danh so (khi quet chon nhieu)\";"
      "    : radio_button { key=\"o_xy\"; label=\"X tang dan  (cung cot thi Y tang dan)\"; }"
      "    : radio_button { key=\"o_yx\"; label=\"Y tang dan  (cung hang thi X tang dan)\"; }"
      "    : radio_button { key=\"o_ord\"; label=\"Giu nguyen thu tu chon\"; }"
      "  }"
      "  : row {"
      "    : edit_box { key=\"tol\"; label=\"Dung sai gom hang/cot:\"; edit_width=10; }"
      "    : toggle { key=\"rev\"; label=\"Dao chieu\"; }"
      "  }"
      "  : boxed_column {"
      "    label = \"Gom nhom - cac block cung gia tri se cung mot ten\";"
      "    : toggle { key=\"grp\"; label=\"Gom cac block co cung gia tri parameter\"; }"
      "    : row {"
      "      : edit_box { key=\"par\"; label=\"Ten parameter:\"; edit_width=16; }"
      "      : edit_box { key=\"gtol\"; label=\"Dung sai gia tri:\"; edit_width=10; }"
      "    }"
      "    : text { label=\"(Doc parameter cua dynamic block; khong co thi doc attribute cung ten)\"; width=64; }"
      "  }"
      "  : row {"
      "    : button { key=\"accept\"; label=\"Bat dau chon\"; is_default=true; fixed_width=true; width=16; }"
      "    : button { key=\"cancel\"; label=\"Huy\"; is_cancel=true; fixed_width=true; width=12; }"
      "  }"
      "}"
    )
    (write-line s f)
  )
  (close f)
  fn
)

;; ============================================================
;; LENH CHINH
;; ============================================================
(defun c:DTEN (/ *error* dclfile dclid ok doc tag pre suf num pad usenum
                 mode ord tol rev grp par gtol done ent str ss lst en
                 n nskip undoon glist gv nm reused rep)

  (defun *error* (msg)
    (if undoon (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,*EXIT*")))
      (princ (strcat "\nLoi: " msg))
    )
    (princ "\nDa ket thuc DTEN.")
    (princ)
  )

  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (setq dclfile (dten-makedcl)
        dclid   (load_dialog dclfile)
  )
  (if (not (new_dialog "danhten" dclid))
    (progn (princ "\nKhong mo duoc hop thoai!") (exit))
  )
  (set_tile "tag"  *dten-tag*)
  (set_tile "pre"  *dten-pre*)
  (set_tile "num"  *dten-num*)
  (set_tile "pad"  *dten-pad*)
  (set_tile "suf"  *dten-suf*)
  (set_tile "tol"  *dten-tol*)
  (set_tile "rev"  (if *dten-rev* "1" "0"))
  (set_tile "grp"  (if *dten-grp* "1" "0"))
  (set_tile "par"  *dten-par*)
  (set_tile "gtol" *dten-gtol*)
  (set_tile *dten-mode* "1")
  (set_tile *dten-ord* "1")
  (dten-preview)

  (action_tile "tag" "(dten-preview)")
  (action_tile "pre" "(dten-preview)")
  (action_tile "num" "(dten-preview)")
  (action_tile "pad" "(dten-preview)")
  (action_tile "suf" "(dten-preview)")
  (action_tile "c_sel"  "(setq *dten-mode* \"c_sel\")")
  (action_tile "c_pick" "(setq *dten-mode* \"c_pick\")")
  (action_tile "o_xy"   "(setq *dten-ord* \"o_xy\")")
  (action_tile "o_yx"   "(setq *dten-ord* \"o_yx\")")
  (action_tile "o_ord"  "(setq *dten-ord* \"o_ord\")")

  (action_tile "accept"
    (strcat "(setq *dten-tag* (get_tile \"tag\")"
            "      *dten-pre* (get_tile \"pre\")"
            "      *dten-num* (get_tile \"num\")"
            "      *dten-pad* (get_tile \"pad\")"
            "      *dten-suf* (get_tile \"suf\")"
            "      *dten-tol* (get_tile \"tol\")"
            "      *dten-rev* (= (get_tile \"rev\") \"1\")"
            "      *dten-grp* (= (get_tile \"grp\") \"1\")"
            "      *dten-par* (get_tile \"par\")"
            "      *dten-gtol* (get_tile \"gtol\"))"
            "(done_dialog 1)"
    )
  )
  (action_tile "cancel" "(done_dialog 0)")

  (setq ok (start_dialog))
  (unload_dialog dclid)
  (vl-file-delete dclfile)

  (if (/= ok 1)
    (princ "\nDa huy.")
    (progn
      (setq tag  (strcase *dten-tag*)
            pre  *dten-pre*
            suf  *dten-suf*
            pad  (atoi *dten-pad*)
            mode *dten-mode*
            ord  *dten-ord*
            tol  (atof *dten-tol*)
            rev  *dten-rev*
            grp  *dten-grp*
            par  *dten-par*
            gtol (atof *dten-gtol*)
      )
      (if (= tag "") (setq tag "NAME"))
      (if (< pad 1) (setq pad 1))
      (if (< tol 0.0) (setq tol 0.0))
      (if (< gtol 0.0) (setq gtol 0.0))
      (if (dten-blank-p par) (setq grp nil))

      (if (dten-blank-p *dten-num*)
        (setq usenum nil num nil)
        (progn
          (setq usenum T num (atoi *dten-num*))
          (if (< num 0) (setq num 0))
        )
      )
      (if (not usenum)
        (progn
          (setq grp nil)
          (princ "\n[Che do KHONG danh so: moi block deu gan cung mot ten]")
        )
      )
      (if grp
        (princ (strcat "\n[Gom nhom theo parameter \"" par
                       "\", dung sai " (rtos gtol 2 3) "]"))
      )

      (setq glist nil rep nil n 0 nskip 0)

      (cond
        ;; ============ QUET CHON NHIEU ============
        ((= mode "c_sel")
         (setq ss (ssget "_I" '((0 . "INSERT"))))
         (if (null ss)
           (progn
             (princ "\nQuet chon cac block can danh ten: ")
             (setq ss (ssget '((0 . "INSERT"))))
           )
         )
         (if (null ss)
           (princ "\nKhong chon doi tuong nao.")
           (progn
             (setq lst (if usenum
                         (dten-sort ss ord tol rev)
                         (dten-sort ss "o_ord" 0.0 nil)))
             (vla-StartUndoMark doc)
             (setq undoon T)
             (foreach en lst
               (setq gv     (if grp (dten-parval en par) nil)
                     reused (if (and grp gv) (dten-gfind gv glist gtol) nil)
                     nm     (if reused reused (dten-fullname pre num pad suf))
               )
               (if (dten-set en tag nm)
                 (progn
                   (setq n (1+ n))
                   (if reused
                     (setq rep (dten-bump rep nm))
                     (progn
                       (if (and grp gv) (setq glist (cons (cons gv nm) glist)))
                       (setq rep (cons (list nm gv 1) rep))
                       (if usenum (setq num (1+ num)))
                     )
                   )
                 )
                 (setq nskip (1+ nskip))
               )
             )
             (vla-EndUndoMark doc)
             (setq undoon nil)
             (if usenum (setq *dten-num* (itoa num)))
             (princ "\n----------- KET QUA -----------")
             (foreach r (reverse rep)
               (princ (strcat "\n  " (car r)
                              (if grp
                                (strcat "   <- " par " = " (dten-vstr (cadr r)))
                                "")
                              "   (" (itoa (caddr r)) " block)"))
             )
             (princ (strcat "\n-------------------------------"
                            "\nDa gan ten cho " (itoa n) " block"
                            (if grp
                              (strcat ", gom thanh " (itoa (length rep)) " nhom.")
                              ".")
                            (if (> nskip 0)
                              (strcat "\nBo qua " (itoa nskip)
                                      " block khong co Tag \"" tag "\".")
                              "")))
           )
         )
        )
        ;; ============ CHON TUNG CAI ============
        (t
         (setq done nil)
         (vla-StartUndoMark doc)
         (setq undoon T)
         (while (not done)
           (setq str (dten-fullname pre num pad suf))
           (princ (strcat "\nChon block de gan ten [" str "] (Enter de thoat): "))
           (setq ent (entsel))
           (if (null ent)
             (setq done T)
             (progn
               (setq ent    (car ent)
                     gv     (if grp (dten-parval ent par) nil)
                     reused (if (and grp gv) (dten-gfind gv glist gtol) nil)
                     nm     (if reused reused str)
               )
               (if (dten-set ent tag nm)
                 (progn
                   (princ (strcat "  -> Da gan: " nm
                                  (if reused "  (trung nhom da co)" "")))
                   (if (not reused)
                     (progn
                       (if (and grp gv) (setq glist (cons (cons gv nm) glist)))
                       (if usenum
                         (progn
                           (setq num (1+ num))
                           (setq *dten-num* (itoa num))
                         )
                       )
                     )
                   )
                 )
                 (princ (strcat "  !! Khong phai block co Tag \"" tag "\", bo qua."))
               )
             )
           )
         )
         (vla-EndUndoMark doc)
         (setq undoon nil)
        )
      )
      (if usenum
        (princ (strcat "\nSo tiep theo se la: " (dten-fullname pre num pad suf)))
        (princ "\nKet thuc.")
      )
    )
  )
  (princ)
)

(princ "\n=== DTEN v5 da nap (gom block cung parameter vao 1 ten) - Go DTEN de chay ===")
(princ)

;;; --- HET [18] DTEN ---

;;; ---------------------------------------------------------------------------
;;; [19] EDA
;;;      Sua nhanh Name va Distance1 cua block dong
;;;      Nguon: EDA (THAY DOI DISTANCE1).lsp
;;; ---------------------------------------------------------------------------
;;; ============================================================
;;; GHI DE NAME / DISTANCE1 HANG LOAT - CO GIAO DIEN (DCL)
;;; Lenh: EDA   (Edit Distance1 & Attribute Name)
;;; v2 - dua tren co che cua DTEN (DanhTenAtt).
;;;
;;; *** MOI (v2): Distance1 KHONG bat buoc la Parameter dong (Linear).
;;;     Neu block khong co Dynamic Parameter "Distance1" thi tu dong
;;;     tim THUOC TINH ATT co tag "Distance1" va ghi de vao do.
;;;     - Uu tien Parameter dong neu block co ca hai.
;;;     - Ghi vao ATT: giu nguyen chuoi nguoi dung nhap (vd "8.000").
;;;     - Doc ATT co fallback quet DXF (entnext) nen block nam trong
;;;       ARRAY lien ket cung tim va ghi duoc.
;;;
;;; Diem khac biet so voi DTEN:
;;;   - DTEN: chon TUNG block mot (entsel), tu dong danh so tang dan.
;;;   - EDA : quet chon HANG LOAT (ssget / Window / Crossing / pickfirst),
;;;           ghi de CUNG MOT gia tri cho Name va/hoac Distance1 cho
;;;           tat ca cac block hop le trong tap chon.
;;;   - EDA co them kha nang TU DONG DO SAU vao trong cac block AN DANH
;;;     (anonymous block) do lenh ARRAY (associative array) tao ra, de
;;;     tim ra block dong that su ben trong ma van chua Name/Distance1.
;;;
;;; Name  : gan bang thuoc tinh (ATTRIBUTE), TagString mac dinh = NAME
;;; Distance1: la THAM SO DONG (Dynamic Block Parameter), khong phai
;;;            attribute -> doc/ghi qua GetDynamicBlockProperties.
;;; ============================================================

;; Bien toan cuc luu gia tri lan truoc (session memory)
(if (null *eda-tag*)     (setq *eda-tag* "NAME"))
(if (null *eda-dprop*)   (setq *eda-dprop* "Distance1"))
(if (null *eda-newname*) (setq *eda-newname* ""))
(if (null *eda-newdist*) (setq *eda-newdist* ""))
(if (null *eda-usename*) (setq *eda-usename* "1"))
(if (null *eda-usedist*) (setq *eda-usedist* "1"))

;; --- Kiem tra chuoi rong / toan dau cach ---
(defun eda-blank-p (s)
  (= (vl-string-trim " " s) "")
)

;; --- Lay danh sach attribute cua 1 block (an toan) ---
(defun eda-get-atts (obj)
  (if (and obj
           (vlax-property-available-p obj 'HasAttributes)
           (= (vla-get-HasAttributes obj) :vlax-true)
      )
    (vlax-invoke obj 'GetAttributes)
  )
)

;; --- Lay danh sach dynamic property cua 1 block (an toan) ---
(defun eda-get-dynprops (obj)
  (if (and obj
           (vlax-property-available-p obj 'IsDynamicBlock)
           (= (vla-get-IsDynamicBlock obj) :vlax-true)
      )
    (vlax-invoke obj 'GetDynamicBlockProperties)
  )
)

;; --- Tim attribute theo tag (khong phan biet hoa/thuong) ---
(defun eda-find-att (atts tag / a res)
  (setq res nil)
  (foreach a atts
    (if (and (not res) (= (strcase (vla-get-TagString a)) (strcase tag)))
      (setq res a)
    )
  )
  res
)

;; --- Tim dynamic property theo ten (khong phan biet hoa/thuong) ---
(defun eda-find-dprop (dps nm / p res)
  (setq res nil)
  (foreach p dps
    (if (and (not res) (= (strcase (vla-get-PropertyName p)) (strcase nm)))
      (setq res p)
    )
  )
  res
)

;; --- *** v2: Tim ATTRIB theo tag o muc DXF (entnext) ---
;;     Tra ve ENAME cua ATTRIB. Dung lam fallback khi GetAttributes
;;     cua VLA khong tra ve du thuoc tinh (hay gap voi block long
;;     trong block an danh *U do ARRAY lien ket tao ra).
(defun eda-find-att-dxf (blkEnt tag / e ed ty res)
  (setq res nil)
  (if (and blkEnt (= (cdr (assoc 0 (entget blkEnt))) "INSERT"))
    (progn
      (setq e (entnext blkEnt))
      (while (and e (not res))
        (setq ed (entget e)
              ty (cdr (assoc 0 ed)))
        (if (= ty "ATTRIB")
          (progn
            (if (= (strcase (cdr (assoc 2 ed))) (strcase tag))
              (setq res e)
            )
            (setq e (entnext e))
          )
          (setq e nil)   ; gap SEQEND / het chuoi ATTRIB -> dung
        )
      )
    )
  )
  res
)

;; --- *** v2: Tim ATT theo tag: thu VLA truoc, khong co thi quet DXF ---
;;     Tra ve VLA-OBJECT (tu GetAttributes) hoac ENAME (tu DXF) hoac nil.
(defun eda-find-att-any (obj atts ent tag / a)
  (setq a (eda-find-att atts tag))
  (if (not a)
    (setq a (eda-find-att-dxf ent tag))
  )
  a
)

;; --- *** v2: Ghi gia tri cho ATT, chap nhan ca VLA-OBJECT lan ENAME ---
(defun eda-put-att (a val / ed)
  (cond
    ((= (type a) 'ENAME)
     (setq ed (entget a))
     (entmod (subst (cons 1 val) (assoc 1 ed) ed))
     (entupd a)
     t
    )
    (a
     (vl-catch-all-apply 'vla-put-TextString (list a val))
     t
    )
  )
)

;; --- Quet 1 doi tuong; neu la block co Name/Distance1 -> lay ngay.
;;     Neu khong (vd: block an danh do ARRAY tao ra) -> do sau vao
;;     block definition de tim block that su ben trong (de quy). ---
;; *** v2: moi phan tu ket qua = (obj a p d)
;;     a = ATT Name          (vla-object hoac ename)
;;     p = Dynamic Parameter Distance1 (vla-object)
;;     d = ATT Distance1     (vla-object hoac ename)  <- MOI
;;     Block hop le khi co it nhat 1 trong 3.
(defun eda-scan (ent tag dprop depth doc / obj etype atts dps a p d btrname btr e2 res)
  (setq res '())
  (if (and ent (numberp depth) (>= depth 0) (entget ent))
    (progn
      (setq etype (cdr (assoc 0 (entget ent))))
      (if (= etype "INSERT")
        (progn
          (setq obj  (vlax-ename->vla-object ent)
                atts (eda-get-atts obj)
                dps  (eda-get-dynprops obj)
                a    (eda-find-att-any obj atts ent tag)
                p    (eda-find-dprop dps dprop)
                d    (eda-find-att-any obj atts ent dprop)   ; *** v2: Distance1 dang ATT
          )
          (if (or a p d)
            ;; Da tim thay block that su -> tra ve
            (setq res (list (list obj a p d)))
            ;; Khong co gi khop -> co the la block chua (Array/anonymous)
            (if (> depth 0)
              (progn
                (setq btrname (vla-get-Name obj))
                (setq btr (vl-catch-all-apply
                            'vla-item
                            (list (vla-get-Blocks doc) btrname)
                          )
                )
                (if (not (vl-catch-all-error-p btr))
                  (vlax-for e2 btr
                    (setq res
                      (append res
                        (eda-scan (vlax-vla-object->ename e2) tag dprop (1- depth) doc)
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
  )
  res
)

;; --- Tao file DCL tam ---
(defun eda-makedcl (/ fn f)
  (setq fn (vl-filename-mktemp "eda" nil ".dcl")
        f  (open fn "w")
  )
  (write-line "eda : dialog {" f)
  (write-line "  label = \"Ghi de Name / Distance1 hang loat - v2\";" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Ten Tag / Ten Parameter\";" f)
  (write-line "    : edit_box { key=\"tag\"; label=\"Tag Attribute (Name):\"; edit_width=22; }" f)
  (write-line "    : edit_box { key=\"dprop\"; label=\"Ten Parameter/ATT (Distance1):\"; edit_width=22; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Gia tri ghi de (chon 1 hoac ca 2)\";" f)
  (write-line "    : row {" f)
  (write-line "      : toggle { key=\"usename\"; label=\"Ghi de Name\"; }" f)
  (write-line "      : edit_box { key=\"newname\"; label=\"Gia tri Name moi:\"; edit_width=20; }" f)
  (write-line "    }" f)
  (write-line "    : row {" f)
  (write-line "      : toggle { key=\"usedist\"; label=\"Ghi de Distance1\"; }" f)
  (write-line "      : edit_box { key=\"newdist\"; label=\"Gia tri Distance1 moi:\"; edit_width=20; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : text { label=\"Ho tro: quet chon (Window/Crossing) va block nam trong Array.\"; width=55; }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key=\"accept\"; label=\"Chon doi tuong\"; is_default=true; fixed_width=true; width=16; }" f)
  (write-line "    : button { key=\"cancel\"; label=\"Huy\"; is_cancel=true; fixed_width=true; width=12; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  fn
)

;; ============================================================
;; LENH CHINH
;; ============================================================
(defun c:EDA (/ doc dclfile dclid ok tag dprop usename newname usedist newdist newdiststr
                ss n i ent targets tgt obj a p d did h doneHandles cntN cntD cntSkip)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; --- Hien hop thoai cau hinh ---
  (setq dclfile (eda-makedcl)
        dclid   (load_dialog dclfile)
  )
  (if (not (new_dialog "eda" dclid))
    (progn (princ "\nKhong mo duoc hop thoai!") (exit))
  )

  (set_tile "tag"     *eda-tag*)
  (set_tile "dprop"   *eda-dprop*)
  (set_tile "newname" *eda-newname*)
  (set_tile "newdist" *eda-newdist*)
  (set_tile "usename" *eda-usename*)
  (set_tile "usedist" *eda-usedist*)

  ;; Bat/tat o nhap theo trang thai toggle luc mo dialog
  (mode_tile "newname" (if (= *eda-usename* "1") 0 1))
  (mode_tile "newdist" (if (= *eda-usedist* "1") 0 1))

  (action_tile "usename" "(mode_tile \"newname\" (if (= $value \"1\") 0 1))")
  (action_tile "usedist" "(mode_tile \"newdist\" (if (= $value \"1\") 0 1))")

  (action_tile "accept"
    (strcat
      "(setq *eda-tag* (get_tile \"tag\")"
      "      *eda-dprop* (get_tile \"dprop\")"
      "      *eda-newname* (get_tile \"newname\")"
      "      *eda-newdist* (get_tile \"newdist\")"
      "      *eda-usename* (get_tile \"usename\")"
      "      *eda-usedist* (get_tile \"usedist\"))"
      "(done_dialog 1)"
    )
  )
  (action_tile "cancel" "(done_dialog 0)")

  (setq ok (start_dialog))
  (unload_dialog dclid)
  (vl-file-delete dclfile)

  (if (/= ok 1)
    (progn (princ "\nDa huy.") (exit))
  )

  (setq tag     (strcase *eda-tag*)
        dprop   *eda-dprop*
        usename (= *eda-usename* "1")
        newname *eda-newname*
        usedist (= *eda-usedist* "1")
        newdist *eda-newdist*
  )
  (if (= tag "")   (setq tag "NAME"))
  (if (= dprop "") (setq dprop "Distance1"))

  (if (not (or usename usedist))
    (progn (princ "\nBan chua chon muc nao de ghi de (Name / Distance1).") (exit))
  )
  (if (and usename (eda-blank-p newname))
    (progn (princ "\nGia tri Name moi dang de trong.") (exit))
  )
  (if (and usedist (eda-blank-p newdist))
    (progn (princ "\nGia tri Distance1 moi dang de trong.") (exit))
  )
  ;; *** v2: giu ca 2 dang gia tri Distance1:
  ;;   - so (atof)  -> ghi vao Dynamic Parameter
  ;;   - chuoi goc  -> ghi vao ATT (giu nguyen nhu nguoi dung nhap)
  (if usedist
    (setq newdiststr (vl-string-trim " " newdist)
          newdist    (atof newdist))
  )

  ;; --- Lay tap chon: uu tien doi tuong da pickfirst san, khong co thi ssget ---
  (setq ss (ssget "_I" '((0 . "INSERT"))))
  (if (or (null ss) (= (sslength ss) 0))
    (progn
      (princ "\nQuet chon cac block (Window/Crossing/click), Enter de ket thuc: ")
      (setq ss (ssget '((0 . "INSERT"))))
    )
  )
  (if (null ss)
    (progn (princ "\nKhong co doi tuong nao duoc chon.") (exit))
  )

  ;; --- Duyet qua tung doi tuong trong tap chon, do sau vao Array neu can ---
  (setq targets '() n (sslength ss) i 0)
  (while (< i n)
    (setq ent (ssname ss i))
    (setq targets (append targets (eda-scan ent tag dprop 3 doc)))
    (setq i (1+ i))
  )

  ;; --- Ap dung ghi de, tranh xu ly trung (block an danh dung chung) ---
  (setq doneHandles '() cntN 0 cntD 0 cntSkip 0)
  (foreach tgt targets
    (setq obj (car tgt) a (cadr tgt) p (caddr tgt) d (cadddr tgt))
    (setq h (vla-get-Handle obj))
    (if (not (member h doneHandles))
      (progn
        (setq doneHandles (cons h doneHandles))
        (setq did nil)
        (if (and usename a)
          (progn (eda-put-att a newname) (setq cntN (1+ cntN)) (setq did t))
        )
        ;; *** v2: Distance1 -> uu tien Parameter dong; khong co thi ghi ATT
        (if usedist
          (cond
            (p
             (vl-catch-all-apply 'vla-put-Value (list p newdist))
             (setq cntD (1+ cntD))
             (setq did t)
            )
            (d
             (eda-put-att d newdiststr)
             (setq cntD (1+ cntD))
             (setq did t)
            )
          )
        )
        (if did
          (vl-catch-all-apply 'vla-Update (list obj))
          (setq cntSkip (1+ cntSkip))
        )
      )
    )
  )

  (princ (strcat "\nHoan tat. Da doi Name: " (itoa cntN)
                 " | Da doi Distance1: " (itoa cntD)
                 " | Bo qua (khong khop Name/Distance1): " (itoa cntSkip)))
  (princ)
)

(princ "\n=== EDA v2 da nap - Go EDA de chay (Distance1 nhan ca Parameter dong LAN thuoc tinh ATT, ho tro Array) ===")
(princ)

;;; --- HET [19] EDA ---

;;; ---------------------------------------------------------------------------
;;; [20] BUN
;;;      Doi don vi (Block Units) cua block
;;;      Nguon: BUN(doidonviblog).lsp
;;; ---------------------------------------------------------------------------
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

;;; --- HET [20] BUN ---

;;; ###########################################################################
;;; ##  NHOM 5 - TOA DO / CAO DO / LY TRINH
;;; ###########################################################################

;;; ---------------------------------------------------------------------------
;;; [21] DTD / DTDMOVE / DTDCOPY
;;;      Danh toa do cho block va diem, tu cap nhat khi REGEN
;;;      Nguon: DTD_DanhToaDo.lsp
;;; ---------------------------------------------------------------------------
;;; ============================================================
;;; DTD - DANH TOA DO BLOCK / DIEM  (giao dien Visual LISP)
;;;
;;; 4 CACH CHON DOI TUONG:
;;;   1. Chon 1 block mau -> tu dong lay TAT CA block cung ten
;;;   2. Quet chon block tren man hinh (window/crossing)
;;;   3. Chon theo TEN block (danh sach block trong ban ve)
;;;   4. Pick diem thu cong tren ban ve
;;;
;;; - Thong ke danh sach diem ngay tren giao dien (STT/Ten/X/Y)
;;; - Tien to ten diem: nhap chu cai dau (trong -> chi danh so 01, 02...)
;;; - Huong danh so: Trai>Phai / Phai>Trai / Duoi>Tren / Tren>Duoi /
;;;   theo thu tu chon
;;; - Tuy chon dao X/Y theo he trac dia (X=Bac, Y=Dong)
;;; - Nhan ve gom: vong tron + duong dan + Ten + X= + Y= (layer TOA-DO)
;;; - Chu X=/Y= dung FIELD, TU DONG dong bo (qua reactor) theo dung
;;;   vi tri MOI NHAT cua mui leader - keo grip/Move xong go REGEN
;;;   (RE) la chu cap nhat, khong can lam gi them.
;;; - Lenh DTDMOVE / DTDCOPY: tien ich doi cho / sao chep NHIEU nhan
;;;   cung luc (tu dong REGEN + gan lai reactor cho ban Copy moi).
;;;
;;; Lenh: DTD, DTDMOVE, DTDCOPY
;;; Tac gia: Claude - danh cho BIM TEAM
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Cai dat mac dinh (nho giua cac lan chay trong phien)
;; ------------------------------------------------------------
(if (not *DTD:Set*)
  (setq *DTD:Set* '(("mode" . "0") ("prefix" . "") ("start" . "1")
                    ("dir" . "0") ("swap" . "0")
                    ("hchu" . "2.0") ("dec" . "3") ("fld" . "1")
                    ("sidefirst" . "0") ("sideorder" . "0")
                    ("style" . "")))
)
(defun DTD:Get (k) (cdr (assoc k *DTD:Set*)))
(defun DTD:Put (k v)
  (setq *DTD:Set* (subst (cons k v) (assoc k *DTD:Set*) *DTD:Set*)))

;; ------------------------------------------------------------
;; KHOA THU TU PICK: khi = T, danh sach diem lay tu "Pick diem
;; thu cong" (Cach 4) se LUON giu NGUYEN thu tu nguoi dung da
;; pick, bo qua moi lua chon huong (dir) trong hop thoai.
;; Duoc bat (T) ngay khi pick tay xong; tat (nil) khi dung
;; 3 cach chon con lai (block mau / quet man hinh / theo ten).
;; ------------------------------------------------------------
(if (not *DTD:PickOrderLock*) (setq *DTD:PickOrderLock* nil))

;; ------------------------------------------------------------
;; Ten hieu dung cua block (ho tro ca Dynamic Block) - tu VLA object
;; ------------------------------------------------------------
(defun DTD:EffNameOfVLA (vlaobj / r)
  (setq r (vl-catch-all-apply 'vla-get-effectivename (list vlaobj)))
  (if (vl-catch-all-error-p r) (vla-get-name vlaobj) r)
)

(defun DTD:EffName (e)
  (DTD:EffNameOfVLA (vlax-ename->vla-object e))
)

;; ------------------------------------------------------------
;; Diem goc (base point) cua 1 BLOCK DINH NGHIA (khong phai INSERT)
;; ------------------------------------------------------------
(defun DTD:BlockBasePt (blkname / d)
  (setq d (tblsearch "BLOCK" blkname))
  (if d (cdr (assoc 10 d)) (list 0.0 0.0 0.0))
)

;; ------------------------------------------------------------
;; Ma tran affine 2D: (a b c d e f) sao cho
;;   worldX = a*x + b*y + e ;  worldY = c*x + d*y + f
;; DTD:InsertMatrix: ma tran cua 1 INSERT (dua tren diem chen,
;; ty le, goc xoay va base point cua BLOCK DINH NGHIA no tham chieu)
;; ------------------------------------------------------------
(defun DTD:InsertMatrix (ins scale rot basept / sx sy cr sr a b c d bx by e f)
  (setq sx (car scale) sy (cadr scale))
  (setq cr (cos rot) sr (sin rot))
  (setq a (* sx cr) b (- (* sy sr)) c (* sx sr) d (* sy cr))
  (setq bx (car basept) by (cadr basept))
  (setq e (- (car ins) (+ (* a bx) (* b by))))
  (setq f (- (cadr ins) (+ (* c bx) (* d by))))
  (list a b c d e f)
)

;; Nhan 2 ma tran: ket qua M sao cho M(p) = M2(M1(p))
(defun DTD:MatMul (M2 M1 / a1 b1 c1 d1 e1 f1 a2 b2 c2 d2 e2 f2)
  (setq a1 (nth 0 M1) b1 (nth 1 M1) c1 (nth 2 M1) d1 (nth 3 M1) e1 (nth 4 M1) f1 (nth 5 M1))
  (setq a2 (nth 0 M2) b2 (nth 1 M2) c2 (nth 2 M2) d2 (nth 3 M2) e2 (nth 4 M2) f2 (nth 5 M2))
  (list (+ (* a2 a1) (* b2 c1))
        (+ (* a2 b1) (* b2 d1))
        (+ (* c2 a1) (* d2 c1))
        (+ (* c2 b1) (* d2 d1))
        (+ (* a2 e1) (* b2 f1) e2)
        (+ (* c2 e1) (* d2 f1) f2))
)

(defun DTD:ApplyMat (M pt)
  (list (+ (* (nth 0 M) (car pt)) (* (nth 1 M) (cadr pt)) (nth 4 M))
        (+ (* (nth 2 M) (car pt)) (* (nth 3 M) (cadr pt)) (nth 5 M)))
)

;; ------------------------------------------------------------
;; DE QUY: quet 1 BLOCK DINH NGHIA (theo ten thuc, ke ca bien the
;; an danh cua Dynamic Block) de tim cac INSERT con ben trong co
;; TEN HIEU DUNG trung voi target. M = ma tran doi diem cuc bo
;; cua block nay -> toa do THUC TE (world). Tra ve list (x y) moi.
;; Gioi han do sau de tranh de quy vo han neu file loi.
;; ------------------------------------------------------------
(defun DTD:ScanBlockDef (blkname M target out depth
                          / doc blkObj item rawname effname
                            vpt vscale vrot bp worldPt subM)
  (if (< depth 25)
    (progn
      (setq doc (vla-get-activedocument (vlax-get-acad-object)))
      (setq blkObj (vl-catch-all-apply 'vla-item
                     (list (vla-get-blocks doc) blkname)))
      (if (not (vl-catch-all-error-p blkObj))
        (vlax-for item blkObj
          (if (and (vlax-property-available-p item 'objectname)
                   (= (vla-get-objectname item) "AcDbBlockReference"))
            (progn
              (setq rawname (vla-get-name item))
              (setq effname (DTD:EffNameOfVLA item))
              (setq vpt (vlax-safearray->list
                          (vlax-variant-value (vla-get-insertionpoint item))))
              (setq vscale (list (vla-get-xscalefactor item)
                                 (vla-get-yscalefactor item)))
              (setq vrot (vla-get-rotation item))
              (setq bp (DTD:BlockBasePt rawname))
              (setq worldPt (DTD:ApplyMat M (list (car vpt) (cadr vpt))))
              (if (= (strcase target) (strcase effname))
                (setq out (cons worldPt out))
              )
              ;; De quy tiep - phong khi block con nay lai chua block
              ;; con khac (long nhieu cap)
              (setq subM (DTD:MatMul M (DTD:InsertMatrix vpt vscale vrot bp)))
              (setq out (DTD:ScanBlockDef rawname subM target out (1+ depth)))
            )
          )
        )
      )
    )
  )
  out
)

;; ------------------------------------------------------------
;; Tim TAT CA vi tri block co ten hieu dung = name, o CA 2 muc:
;;  - Dat truc tiep ngoai ban ve (top-level)
;;  - LONG BEN TRONG bat ky block cha nao (de quy toa do that)
;; ------------------------------------------------------------
(defun DTD:AllMatches (name / ss i e out p vobj vpt vscale vrot bp M)
  (setq out '() i 0)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  ;; 1) Cac block dat truc tiep, trung ten
  (if ss
    (repeat (sslength ss)
      (setq e (ssname ss i))
      (if (= (strcase name) (strcase (DTD:EffName e)))
        (progn
          (setq p (cdr (assoc 10 (entget e))))
          (setq out (cons (list (car p) (cadr p)) out))
        )
      )
      (setq i (1+ i))
    )
  )
  ;; 2) De quy vao MOI block dat truc tiep de tim block con
  ;;    cung ten LONG BEN TRONG no (bat ke ten cua block cha la gi)
  (setq i 0)
  (if ss
    (repeat (sslength ss)
      (setq e (ssname ss i))
      (setq vobj (vlax-ename->vla-object e))
      (setq vpt (vlax-safearray->list
                  (vlax-variant-value (vla-get-insertionpoint vobj))))
      (setq vscale (list (vla-get-xscalefactor vobj) (vla-get-yscalefactor vobj)))
      (setq vrot (vla-get-rotation vobj))
      (setq bp (DTD:BlockBasePt (vla-get-name vobj)))
      (setq M (DTD:InsertMatrix vpt vscale vrot bp))
      (setq out (DTD:ScanBlockDef (vla-get-name vobj) M name out 0))
      (setq i (1+ i))
    )
  )
  (if (= (length out) 0)
    (princ (strcat "\nKhong tim thay block ten <" name "> trong ban ve (ke ca long nhau)!"))
  )
  (reverse out)
)

;; ------------------------------------------------------------
;; Lay diem chen + ename cua selection set INSERT
;; -> list ((x y ename) ...)   (ename dung de gan Field)
;; ------------------------------------------------------------
(defun DTD:SSPoints (ss / i e p out)
  (setq out '() i 0)
  (if ss
    (repeat (sslength ss)
      (setq e (ssname ss i))
      (setq p (cdr (assoc 10 (entget e))))
      (setq out (cons (list (car p) (cadr p) e) out))
      (setq i (1+ i))
    )
  )
  (reverse out)
)

;; ------------------------------------------------------------
;; Lay ten block CHINH XAC tu 1 lan pick (ho tro pick THANG VAO
;; block CON nam long trong block CHA, dung nentsel de "xam nhap"
;; vao ben trong thay vi chi lay duoc block cha ngoai cung)
;; ------------------------------------------------------------
(defun DTD:PickChildName (ent / d owner)
  (setq d (entget ent))
  (if (= (cdr (assoc 0 d)) "INSERT")
    (DTD:EffName ent)
    (progn
      (setq owner (cdr (assoc 330 d)))
      (if owner (cdr (assoc 2 (entget owner))) nil)
    )
  )
)

;; ------------------------------------------------------------
;; CACH 1: chon 1 block mau -> tat ca block cung ten
;; (cho phep click THANG VAO block con nam trong block cha)
;; ------------------------------------------------------------
(defun DTD:PickParent (/ es ent name pts)
  (setq es (nentsel "\nChon 1 block mau (co the click thang vao block CON nam trong block CHA): "))
  (if es
    (progn
      (setq ent (car es))
      (setq name (DTD:PickChildName ent))
      (if name
        (progn
          (princ (strcat "\nBlock mau: <" name "> - dang tim tat ca (ke ca long nhau trong block khac)..."))
          (setq pts (DTD:AllMatches name))
          (princ (strcat " tim thay " (itoa (length pts)) " block."))
          pts
        )
        (progn (princ "\nKhong xac dinh duoc ten block tu diem vua chon!") nil)
      )
    )
    (progn (princ "\nKhong chon duoc doi tuong!") nil)
  )
)

;; ------------------------------------------------------------
;; CACH 2: quet chon block tren man hinh
;; ------------------------------------------------------------
(defun DTD:PickScreen ()
  (princ "\nQuet chon cac block tren man hinh: ")
  (DTD:SSPoints (ssget '((0 . "INSERT"))))
)

;; ------------------------------------------------------------
;; CACH 3: theo ten block (name lay tu popup) - QUET CA LONG NHAU
;; ------------------------------------------------------------
(defun DTD:ByName (name)
  (DTD:AllMatches name)
)

;; ------------------------------------------------------------
;; CACH 4: pick diem thu cong
;; ------------------------------------------------------------
(defun DTD:PickPoints (/ p out)
  (setq out '())
  (while (setq p (getpoint (strcat "\nPick diem thu "
                                   (itoa (1+ (length out)))
                                   " (Enter de xong): ")))
    (setq out (cons (list (car p) (cadr p) nil) out))
  )
  (reverse out)
)

;; ------------------------------------------------------------
;; Danh sach ten block trong ban ve (bo block an danh, xref)
;; ------------------------------------------------------------
(defun DTD:BlockNames (/ d out n flags)
  (setq out '())
  (while (setq d (tblnext "BLOCK" (not d)))
    (setq n (cdr (assoc 2 d)))
    (setq flags (cdr (assoc 70 d)))
    (if (and (/= (substr n 1 1) "*")
             (/= (strcase (substr n 1 2)) "A$")   ; bo bien the an danh cua Dynamic Block
             (zerop (logand 29 flags)))            ; bo Anonymous(1) + Xref(4+8+16)
      (setq out (cons n out))
    )
  )
  (vl-sort out '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ------------------------------------------------------------
;; Danh sach Text Style co trong ban ve (bo cac style chi dung
;; lam Shape file - flag bit0 = 1)
;; ------------------------------------------------------------
(defun DTD:StyleNames (/ d out n flags)
  (setq out '())
  (while (setq d (tblnext "STYLE" (not d)))
    (setq n (cdr (assoc 2 d)))
    (setq flags (cdr (assoc 70 d)))
    (if (zerop (logand 1 flags))
      (setq out (cons n out))
    )
  )
  (vl-sort out '(lambda (a b) (< (strcase a) (strcase b))))
)

;; Vi tri (0-based) cua 1 chuoi trong list, hoac nil neu khong co
(defun DTD:PosInList (item lst / i pos)
  (setq i 0 pos nil)
  (foreach x lst
    (if (and (not pos) (equal x item)) (setq pos i))
    (setq i (1+ i))
  )
  pos
)

;; Chi so popup "stylesel" -> ten style thuc te ("" = muc "Mac dinh")
;; Muc 0 luon la "Mac dinh (khong doi)", cac muc sau la ten style
;; lay tu DTD:StyleNames (dung THU TU sap xep giong luc do popup).
(defun DTD:StyleAtIdx (idx / lst)
  (if (<= idx 0)
    ""
    (progn
      (setq lst (DTD:StyleNames))
      (if (and (> idx 0) (<= idx (length lst)))
        (nth (1- idx) lst)
        ""
      )
    )
  )
)

;; Ten style da luu -> chi so popup "stylesel" tuong ung
(defun DTD:StyleIdxOf (name / lst pos)
  (if (or (not name) (= name ""))
    0
    (progn
      (setq lst (DTD:StyleNames))
      (setq pos (DTD:PosInList (strcase name) (mapcar 'strcase lst)))
      (if pos (1+ pos) 0)
    )
  )
)

;; ------------------------------------------------------------
;; Chieu diem len huong da pick (*DTD:DirVec* = (goc (ux uy)))
;; ------------------------------------------------------------
(defun DTD:Proj (p / b v)
  (setq b (car *DTD:DirVec*) v (cadr *DTD:DirVec*))
  (+ (* (- (car p) (car b)) (car v))
     (* (- (cadr p) (cadr b)) (cadr v)))
)
(defun DTD:Perp (p / b v)
  (setq b (car *DTD:DirVec*) v (cadr *DTD:DirVec*))
  (+ (* (- (car p) (car b)) (- (cadr v)))
     (* (- (cadr p) (cadr b)) (car v)))
)

;; ------------------------------------------------------------
;; Sap xep: het toan bo 1 ben (theo huong pick) roi sang ben kia
;; Perp<0 = ben PHAI huong di, Perp>=0 = ben TRAI huong di
;; ------------------------------------------------------------
(defun DTD:SortSideFirst (pts rightFirst / right left)
  (setq right (vl-remove-if '(lambda (p) (>= (DTD:Perp p) 0)) pts))
  (setq left  (vl-remove-if '(lambda (p) (< (DTD:Perp p) 0)) pts))
  (setq right (vl-sort right '(lambda (a b) (< (DTD:Proj a) (DTD:Proj b)))))
  (setq left  (vl-sort left  '(lambda (a b) (< (DTD:Proj a) (DTD:Proj b)))))
  (if rightFirst (append right left) (append left right))
)

;; ------------------------------------------------------------
;; Sap xep diem theo huong danh so
;; 0:Trai>Phai  1:Phai>Trai  2:Duoi>Tren  3:Tren>Duoi
;; 4:Thu tu chon  5:Theo huong pick 2 diem
;; ------------------------------------------------------------
(defun DTD:SortPts (pts dir)
  (cond
    ;; Dang khoa thu tu pick (vua pick tay - Cach 4) -> khong sap
    ;; xep lai, bat ke dir dang chon la gi trong hop thoai.
    (*DTD:PickOrderLock* pts)
    ((and (= dir 5) *DTD:DirVec*)
     (if (= (DTD:Get "sidefirst") "1")
       (DTD:SortSideFirst pts (= (DTD:Get "sideorder") "0"))
       (vl-sort pts
         '(lambda (a b)
            (if (equal (DTD:Proj a) (DTD:Proj b) 1e-6)
              (< (DTD:Perp a) (DTD:Perp b))
              (< (DTD:Proj a) (DTD:Proj b)))))
     ))
    ((= dir 0) (vl-sort pts '(lambda (a b)
                 (if (equal (car a) (car b) 1e-6)
                   (> (cadr a) (cadr b)) (< (car a) (car b))))))
    ((= dir 1) (vl-sort pts '(lambda (a b)
                 (if (equal (car a) (car b) 1e-6)
                   (> (cadr a) (cadr b)) (> (car a) (car b))))))
    ((= dir 2) (vl-sort pts '(lambda (a b)
                 (if (equal (cadr a) (cadr b) 1e-6)
                   (< (car a) (car b)) (< (cadr a) (cadr b))))))
    ((= dir 3) (vl-sort pts '(lambda (a b)
                 (if (equal (cadr a) (cadr b) 1e-6)
                   (< (car a) (car b)) (> (cadr a) (cadr b))))))
    (T pts)
  )
)

;; ------------------------------------------------------------
;; Sinh ten diem thu i: prefix + so (pad 0 theo tong)
;; ------------------------------------------------------------
(defun DTD:PtName (prefix i start total / num w s)
  (setq num (+ start i))
  (setq w (max 2 (strlen (itoa (+ start total -1)))))
  (setq s (itoa num))
  (while (< (strlen s) w) (setq s (strcat "0" s)))
  (if (and prefix (/= prefix "")) (strcat prefix s) s)
)

;; ------------------------------------------------------------
;; VE NHAN TOA DO tai 1 diem
;; ------------------------------------------------------------
(defun DTD:Layer ()
  (if (not (tblsearch "LAYER" "TOA-DO"))
    (entmake '((0 . "LAYER") (100 . "AcDbSymbolTableRecord")
               (100 . "AcDbLayerTableRecord") (2 . "TOA-DO")
               (62 . 2) (70 . 0)))
  )
)

(defun DTD:MkText (pt h str)
  (entmake (list (cons 0 "TEXT") (cons 8 "TOA-DO")
                 (cons 10 (list (car pt) (cadr pt) 0.0))
                 (cons 40 h) (cons 1 str)
                 (cons 7 (getvar "TEXTSTYLE")) (cons 50 0.0)))
)

;; ------------------------------------------------------------
;; Lay ObjectID dang chuoi (chay dung ca CAD 32-bit & 64-bit)
;; ------------------------------------------------------------
(defun DTD:ObjIdStr (ename / vobj util)
  (setq vobj (vlax-ename->vla-object ename))
  (setq util (vla-get-utility
               (vla-get-activedocument (vlax-get-acad-object))))
  (if (vlax-method-applicable-p util 'getobjectidstring)
    (vlax-invoke-method util 'getobjectidstring vobj :vlax-false)
    (itoa (vla-get-objectid vobj))
  )
)

;; ------------------------------------------------------------
;; DIEM MOC AN (LINE dai = 0) dat DUNG TAI MUI LEADER.
;; Day la diem thuc su can do, nen Field se bam vao day
;; (khong bam vao block goc nua). Diem moc nay DUNG YEN, KHONG
;; gom nhom (group) voi mleader -> keo duong leader / doi cho chu
;; se KHONG lam mui leader (va diem moc) bi dich chuyen theo.
;; ------------------------------------------------------------
(defun DTD:MakeAnchor (pt)
  (entmakex
    (list (cons 0 "LINE") (cons 8 "TOA-DO")
          (cons 10 (list (car pt) (cadr pt) 0.0))
          (cons 11 (list (car pt) (cadr pt) 0.0)))
  )
)

;; ------------------------------------------------------------
;; Chuoi FIELD tham chieu diem dau doan LINE an (StartPoint)
;; ptcode: "%pt1" = X cad, "%pt2" = Y cad
;; ------------------------------------------------------------
(defun DTD:FieldExprPt (idstr ptcode dec)
  (strcat "%<\\AcObjProp Object(%<\\_ObjId " idstr
          ">%).StartPoint \\f \"%lu2%pr" (itoa dec) ptcode "\">%")
)

;; ------------------------------------------------------------
;; LIEN KET mleader <-> diem moc BANG XDATA (khong dung GROUP).
;; Dung group code 1005 (Database handle) - day la loai xdata DAC
;; BIET duoc AutoCAD TU DONG "remap" sang handle MOI khi ca 2 doi
;; tuong (mleader + diem moc) duoc COPY cung nhau trong 1 lan.
;; ------------------------------------------------------------
(defun DTD:LinkAnchor (mlE anchorE)
  (if (not (tblsearch "APPID" "DTD-ANCHOR")) (regapp "DTD-ANCHOR"))
  (entmod
    (append (entget mlE)
            (list (list -3 (list "DTD-ANCHOR"
                                  (cons 1005 (cdr (assoc 5 (entget anchorE)))))))
    )
  )
)

;; Tra ve ename cua diem moc lien ket voi 1 mleader (nil neu khong co)
(defun DTD:GetAnchorOf (mlE / d xdblk hs anchorE)
  (setq d (entget mlE '("DTD-ANCHOR")))
  (setq xdblk (assoc -3 d))
  (if xdblk
    (progn
      (setq hs (cdr (assoc 1005 (cdr (cadr xdblk)))))
      (if hs (setq anchorE (handent hs)))
    )
  )
  anchorE
)

;; ------------------------------------------------------------
;; TU DONG DONG BO: moi khi 1 mleader (co gan Field) bi CHINH SUA
;; theo BAT KY cach nao (keo grip landing/chu, keo grip mui ten,
;; MOVE, STRETCH...), reactor se doc lai vi tri HIEN TAI cua MUI
;; LEADER (dinh dau tien cua leader line) va cap nhat DIEM MOC AN
;; cho khop. Nho vay, chi can go REGEN la chu X=/Y= LUON DUNG theo
;; vi tri MOI NHAT cua mui leader - du ban di chuyen bang cach nao.
;; ------------------------------------------------------------
(defun DTD:SyncAnchor (mlE / anchorE vobj arrv pts arrowPt d)
  (setq anchorE (DTD:GetAnchorOf mlE))
  (if anchorE
    (progn
      (setq vobj (vlax-ename->vla-object mlE))
      (setq arrv (vl-catch-all-apply 'vla-getleaderlinevertices (list vobj 0)))
      (if (not (vl-catch-all-error-p arrv))
        (progn
          (setq pts (vlax-safearray->list (vlax-variant-value arrv)))
          (if (>= (length pts) 3)
            (progn
              ;; Dinh dau tien = diem MUI LEADER (theo thu tu tao lap ban dau)
              (setq arrowPt (list (nth 0 pts) (nth 1 pts) (nth 2 pts)))
              (setq d (entget anchorE))
              (if (and d (assoc 10 d) (assoc 11 d))
                (progn
                  (setq d (subst (cons 10 arrowPt) (assoc 10 d) d))
                  (setq d (subst (cons 11 arrowPt) (assoc 11 d) d))
                  (entmod d)
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Callback goi khi mleader bi sua doi (:vlr-modified)
(defun DTD:MLReactorCB (notifier reactor arglist)
  (vl-catch-all-apply
    '(lambda () (DTD:SyncAnchor (vlax-vla-object->ename notifier)))
  )
  (princ)
)

;; Go bo toan bo reactor cu (tranh gan trung lap khi load lai file)
(defun DTD:ClearReactors ()
  (if *DTD:Reactors*
    (foreach r *DTD:Reactors* (vl-catch-all-apply 'vlr-remove (list r)))
  )
  (setq *DTD:Reactors* nil)
)

(defun DTD:AttachReactor (mlobj)
  (setq *DTD:Reactors*
    (cons (vlr-object-reactor (list mlobj) nil
            (list (cons :vlr-modified 'DTD:MLReactorCB)))
          *DTD:Reactors*))
)

;; Quet toan bo ban ve, gan lai reactor cho MOI mleader cua DTD (co
;; xdata DTD-ANCHOR). Goi lai ham nay sau khi tao nhan moi / sau khi
;; Copy, va 1 lan khi vua tai file - de dam bao lenh nao cung duoc
;; dong bo tu dong, ke ca sau khi dong/mo lai ban ve.
(defun DTD:ReattachAll (/ ss n i e)
  (DTD:ClearReactors)
  (setq ss (ssget "_X" '((0 . "MULTILEADER"))))
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (repeat n
        (setq e (ssname ss i))
        (if (assoc -3 (entget e '("DTD-ANCHOR")))
          (DTD:AttachReactor (vlax-ename->vla-object e))
        )
        (setq i (1+ i))
      )
    )
  )
)

;; ------------------------------------------------------------
;; VE NHAN = MLEADER (mui ten chi vao diem, chu: Ten + X= + Y=)
;; ------------------------------------------------------------
(defun DTD:Label (item name h dec swap usefld center style
                  / x y sx sy p1 idstr cx cy str doc ms arr ml
                    dirx diry dlen wtext maxlen anchorE)
  (setq x (car item) y (cadr item))
  ;; Dao X/Y theo trac dia: X = huong Bac (Y cad), Y = huong Dong (X cad)
  (if swap
    (setq sx (rtos y 2 dec) sy (rtos x 2 dec) cx "%pt2" cy "%pt1")
    (setq sx (rtos x 2 dec) sy (rtos y 2 dec) cx "%pt1" cy "%pt2")
  )
  ;; Noi dung chu (3 dong): Ten \P X= \P Y=
  ;; Neu dung Field: tao 1 "diem moc an" (LINE dai=0) NGAY TAI DIEM
  ;; CAN DO (mui leader), Field bam vao diem moc nay - KHONG bam vao
  ;; block goc nua. KHONG gom leader + diem moc thanh GROUP -> keo
  ;; grip duong leader hoac grip chu se KHONG lam mui leader dich
  ;; chuyen (grip mui ten van dung yen dung vi tri toa do that).
  (if usefld
    (progn
      (setq anchorE (DTD:MakeAnchor (list x y)))
      (setq idstr (DTD:ObjIdStr anchorE))
      (setq str (strcat name
                        "\\PX=" (DTD:FieldExprPt idstr cx dec)
                        "\\PY=" (DTD:FieldExprPt idstr cy dec)))
    )
    (setq str (strcat name "\\PX=" sx "\\PY=" sy))
  )
  ;; Huong dat nhan: TOA RA tu tam cum diem (tranh chong cheo khi
  ;; nhieu block xep sat nhau, thay vi luon chech cung 1 huong)
  (setq dirx (- x (car center)) diry (- y (cadr center)) dlen (sqrt (+ (* dirx dirx) (* diry diry))))
  (if (< dlen 1e-6)
    (setq dirx 0.7071 diry 0.7071)          ; diem trung tam -> mac dinh chech 45 do
    (setq dirx (/ dirx dlen) diry (/ diry dlen))
  )
  (setq p1 (list (+ x (* 5.0 h dirx)) (+ y (* 5.0 h diry))))
  ;; Chieu rong chu co dinh (ky tu ~0.6*h) de MTEXT KHONG tu xuong dong
  ;; -> tranh hien tuong vo/de chu khi cac nhan o gan nhau
  (setq maxlen (max (strlen name) (+ 2 (strlen sx)) (+ 2 (strlen sy)) 10))
  (setq wtext (* h 0.65 (+ maxlen 2)))
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq ms  (vla-get-modelspace doc))
  (setq arr (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill arr (list x y 0.0 (car p1) (cadr p1) 0.0))
  (setq ml (vla-addmleader ms arr 0))
  (vla-put-contenttype ml 2)               ; acMTextContent
  (vla-put-textstring ml str)
  (vla-put-textheight ml h)
  (vl-catch-all-apply 'vla-put-textwidth (list ml wtext))
  (vla-put-arrowheadsize ml h)
  (vla-put-landinggap ml (* 0.5 h))
  (vla-put-layer ml "TOA-DO")
  (vl-catch-all-apply 'vla-put-color (list ml 2))   ; ep cung mau 2 (vang)
  ;; Font chu (Text Style) cho nhan - chi doi khi nguoi dung CO chon
  ;; trong hop thoai; neu de "Mac dinh" (style = "") thi KHONG dong
  ;; cham gi, giu nguyen style theo Mleader Style hien tai (nhu truoc).
  (if (and style (/= style ""))
    (vl-catch-all-apply 'vla-put-textstyle (list ml style))
  )
  ;; Lien ket mleader <-> diem moc bang XDATA (khong group) - phuc vu
  ;; lenh DTDMOVE/DTDCOPY sau nay.
  (if usefld
    (DTD:LinkAnchor (vlax-vla-object->ename ml) anchorE)
  )
  ml
)

;; ============================================================
;; GIAO DIEN
;; ============================================================
(defun DTD:MakeDCL (/ fname f)
  (setq fname (vl-filename-mktemp "dtd" nil ".dcl"))
  (setq f (open fname "w"))
  (foreach s
   '("dtd : dialog {"
     "  label = \"DTD - DANH TOA DO BLOCK / DIEM\";"
     "  : row {"
     ;; ----- COT TRAI: cai dat -----
     "    : column {"
     "      : boxed_column {"
     "        label = \"1. Cach chon doi tuong\";"
     "        : row {"
     "          : radio_button { key = \"m0\"; label = \"Chon 1 block mau -> block cung ten\"; width = 33; }"
     "          : button { key = \"b0\"; label = \"Chon >\"; width = 9; }"
     "        }"
     "        : row {"
     "          : radio_button { key = \"m1\"; label = \"Quet chon block tren man hinh\"; width = 33; }"
     "          : button { key = \"b1\"; label = \"Quet >\"; width = 9; }"
     "        }"
     "        : row {"
     "          : radio_button { key = \"m2\"; label = \"Chon theo ten block (o ben duoi):\"; width = 33; }"
     "          : button { key = \"b2\"; label = \"Lay >\"; width = 9; }"
     "        }"
     "        : popup_list   { key = \"bname\"; width = 30; }"
     "        : row {"
     "          : radio_button { key = \"m3\"; label = \"Pick diem thu cong tren ban ve\"; width = 33; }"
     "          : button { key = \"b3\"; label = \"Pick >\"; width = 9; }"
     "        }"
     "        spacer;"
     "        : button { key = \"pick\"; label = \"<<  CHON / PICK TREN BAN VE  >>\"; fixed_width = false; }"
     "        : text   { key = \"count\"; label = \"Chua chon diem nao.\"; width = 36; }"
     "      }"
     "      : boxed_column {"
     "        label = \"2. Dat ten diem & huong danh so\";"
     "        : edit_box { key = \"prefix\"; label = \"Tien to ten diem :\"; edit_width = 12; }"
     "        : text { label = \"   (De trong -> chi danh so: 01, 02, 03...)\"; }"
     "        : edit_box { key = \"start\";  label = \"Bat dau tu so   :\"; edit_width = 6; }"
     "        : popup_list {"
     "          key = \"dir\"; label = \"Huong danh so   :\"; width = 26;"
     "        }"
     "        : text { key = \"dirinfo\"; label = \" \"; width = 36; }"
     "        : row {"
     "          : toggle { key = \"sidefirst\"; label = \"Het 1 ben roi qua ben kia:\"; }"
     "          : popup_list { key = \"sideorder\"; width = 12; }"
     "        }"
     "        : toggle { key = \"swap\"; label = \"Dao X/Y theo he trac dia (X=Bac, Y=Dong)\"; }"
     "      }"
     "      : boxed_column {"
     "        label = \"3. Nhan toa do\";"
     "        : row {"
     "          : edit_box { key = \"hchu\"; label = \"Cao chu :\"; edit_width = 6; }"
     "          : edit_box { key = \"dec\";  label = \"So le thap phan :\"; edit_width = 4; }"
     "        }"
     "        : toggle { key = \"fld\"; label = \"Toa do TU CAP NHAT khi Move/Copy leader (Field+REGEN)\"; }"
     "        : popup_list { key = \"stylesel\"; label = \"Font chu (Text Style):\"; width = 30; }"
     "        : text { label = \"   (De 'Mac dinh' -> giu nguyen nhu hien tai)\"; }"
     "        : text { label = \"Nhan la MLEADER: mui ten -> Ten + X= + Y=\"; }"
     "        : text { label = \"Nhan duoc ve vao layer: TOA-DO (mau vang)\"; }"
     "      }"
     "    }"
     ;; ----- COT PHAI: danh sach diem -----
     "    : boxed_column {"
     "      label = \"4. Danh sach diem se danh toa do\";"
     "      : list_box { key = \"plist\"; height = 22; width = 42; }"
     "      : text { key = \"tongkq\"; label = \" \"; width = 40; }"
     "    }"
     "  }"
     "  spacer;"
     "  : row {"
     "    : button { key = \"accept\"; label = \"DANH TOA DO\"; is_default = true; width = 16; }"
     "    : button { key = \"cancel\"; label = \"Thoat\"; is_cancel = true; width = 12; }"
     "  }"
     "}")
    (write-line s f)
  )
  (close f)
  fname
)

;; ------------------------------------------------------------
;; Can le chuoi (them khoang trang ben phai)
;; ------------------------------------------------------------
(defun DTD:PadR (s w)
  (while (< (strlen s) w) (setq s (strcat s " ")))
  s
)

;; ------------------------------------------------------------
;; Cap nhat danh sach diem tren giao dien theo cai dat hien tai
;; ------------------------------------------------------------
(defun DTD:Refresh (pts / prefix start dir dec sorted i n name p)
  (setq prefix (get_tile "prefix")
        start  (max 1 (atoi (get_tile "start")))
        dir    (atoi (get_tile "dir"))
        dec    (max 0 (atoi (get_tile "dec"))))
  (setq sorted (DTD:SortPts pts dir))
  (setq n (length sorted))
  (start_list "plist")
  (if (> n 0)
    (progn
      (setq i 0)
      (foreach p sorted
        (setq name (DTD:PtName prefix i start n))
        (add_list (strcat (DTD:PadR (itoa (1+ i)) 4)
                          (DTD:PadR name 10)
                          "X=" (DTD:PadR (rtos (car p) 2 dec) 12)
                          "Y=" (rtos (cadr p) 2 dec)))
        (setq i (1+ i))
      )
    )
    (add_list "<< Chua co diem - bam nut CHON / PICK ben trai >>")
  )
  (end_list)
  (set_tile "count"
    (if (> n 0)
      (strcat "Da chon: " (itoa n) " diem.")
      "Chua chon diem nao."))
  (set_tile "tongkq"
    (if (> n 0)
      (strcat "Tong cong: " (itoa n) " diem, ten tu "
              (DTD:PtName prefix 0 start n) " den "
              (DTD:PtName prefix (1- n) start n))
      " "))
  sorted
)

;; ------------------------------------------------------------
;; Radio nam trong row -> tu quan ly loai tru lan nhau
;; ------------------------------------------------------------
(defun DTD:SetMode (m / k)
  (foreach k '("0" "1" "2" "3")
    (set_tile (strcat "m" k) (if (= k m) "1" "0"))
  )
  (DTD:Put "mode" m)
)

;; ------------------------------------------------------------
;; Luu cai dat tu hop thoai vao bo nho
;; ------------------------------------------------------------
(defun DTD:SaveTiles ()
  (DTD:Put "prefix" (get_tile "prefix"))
  (DTD:Put "start"  (get_tile "start"))
  (DTD:Put "dir"    (get_tile "dir"))
  (DTD:Put "swap"   (get_tile "swap"))
  (DTD:Put "hchu"   (get_tile "hchu"))
  (DTD:Put "dec"    (get_tile "dec"))
  (DTD:Put "fld"    (get_tile "fld"))
  (DTD:Put "sidefirst" (get_tile "sidefirst"))
  (DTD:Put "sideorder" (get_tile "sideorder"))
  (DTD:Put "style" (DTD:StyleAtIdx (atoi (get_tile "stylesel"))))
)

;; ============================================================
;; LENH CHINH
;; ============================================================
(defun C:DTD (/ pts loop dclfile dclid kq bnames bidx mode
                prefix start dir swap hchu dec sorted i name n oldos
                p1 p2 dd usefld center cenx ceny snames stylename)
  (setq pts '() loop T kq 0)
  ;; Tat nen xam sau chu Field (chi anh huong hien thi tren man hinh,
  ;; khong in ra ban ve) - de chu nhin sach, khong bi to nen
  (setvar "FIELDDISPLAY" 0)
  (setq bnames (DTD:BlockNames))
  (setq snames (DTD:StyleNames))

  (while loop
    (setq dclfile (DTD:MakeDCL))
    (setq dclid (load_dialog dclfile))
    (if (< dclid 0)
      (progn (alert "Khong the tao hop thoai DCL!") (setq loop nil))
      (progn
        (if (not (new_dialog "dtd" dclid))
          (progn (alert "Khong the mo hop thoai!") (setq loop nil))
          (progn
            ;; --- nap du lieu ---
            (set_tile (strcat "m" (DTD:Get "mode")) "1")
            (start_list "bname")
            (if bnames (mapcar 'add_list bnames) (add_list "<khong co block>"))
            (end_list)
            (if *DTD:Bidx* (set_tile "bname" (itoa *DTD:Bidx*)))
            (set_tile "prefix" (DTD:Get "prefix"))
            (set_tile "start"  (DTD:Get "start"))
            (start_list "dir")
            (mapcar 'add_list
              '("Trai  ->  Phai (X tang)"
                "Phai  ->  Trai (X giam)"
                "Duoi  ->  Tren (Y tang)"
                "Tren  ->  Duoi (Y giam)"
                "Theo thu tu chon / pick"
                "Pick 2 diem chi huong  >>"))
            (end_list)
            (set_tile "dir"  (DTD:Get "dir"))
            (set_tile "swap" (DTD:Get "swap"))
            (set_tile "hchu" (DTD:Get "hchu"))
            (set_tile "dec"  (DTD:Get "dec"))
            (set_tile "fld"  (DTD:Get "fld"))
            (start_list "stylesel")
            (add_list "-- Mac dinh (khong doi) --")
            (if snames (mapcar 'add_list snames))
            (end_list)
            (set_tile "stylesel" (itoa (DTD:StyleIdxOf (DTD:Get "style"))))
            (set_tile "dirinfo"
              (if *DTD:DirVec*
                (strcat "Huong da pick: "
                        (rtos (* 180.0 (/ (angle '(0 0)
                                                 (cadr *DTD:DirVec*)) pi))
                              2 1) " do (so voi truc X)")
                "(Chon 'Pick 2 diem' de tu dinh huong danh so)"))
            ;; Dang khoa thu tu pick (vua Pick diem thu cong) -> khoa
            ;; han popup huong danh so, luon hien dung "Theo thu tu
            ;; chon/pick" de nguoi dung biet ro dang o che do nao.
            (if *DTD:PickOrderLock*
              (progn
                (set_tile "dir" "4")
                (mode_tile "dir" 1)
                (mode_tile "sidefirst" 1)
                (mode_tile "sideorder" 1)
                (set_tile "dirinfo"
                  "*** DANG KHOA: giu NGUYEN thu tu da Pick tay, khong doi huong duoc. ***")
              )
            )
            (set_tile "sidefirst" (DTD:Get "sidefirst"))
            (start_list "sideorder")
            (mapcar 'add_list '("Phai truoc" "Trai truoc"))
            (end_list)
            (set_tile "sideorder" (DTD:Get "sideorder"))
            (DTD:Refresh pts)

            ;; --- su kien ---
            ;; radio + nut chon nhanh cua tung cach chon
            (foreach m '("0" "1" "2" "3")
              (action_tile (strcat "m" m)
                (strcat "(DTD:SetMode \"" m "\")"))
              (action_tile (strcat "b" m)
                (strcat "(DTD:Put \"mode\" \"" m
                        "\") (DTD:SaveTiles) (done_dialog 2)"))
            )
            (action_tile "bname" "(setq *DTD:Bidx* (atoi $value))")

            ;; chon "Pick 2 diem chi huong" -> ra ban ve pick ngay
            (action_tile "dir"
              "(if (= $value \"5\")
                 (progn (DTD:SaveTiles) (done_dialog 6))
                 (DTD:Refresh pts))")

            ;; go/doi cai dat -> cap nhat danh sach ngay
            (foreach k '("prefix" "start")
              (action_tile k "(DTD:Refresh pts)")
            )
            (action_tile "sidefirst"
              "(DTD:Put \"sidefirst\" $value) (DTD:Refresh pts)")
            (action_tile "sideorder"
              "(DTD:Put \"sideorder\" $value) (DTD:Refresh pts)")

            ;; nut chon / pick -> dong tam hop thoai
            (action_tile "pick" "(DTD:SaveTiles) (done_dialog 2)")

            (action_tile "accept" "(DTD:SaveTiles) (done_dialog 1)")
            (action_tile "cancel" "(done_dialog 0)")

            (setq kq (start_dialog))
          )
        )
        (unload_dialog dclid)
        (vl-file-delete dclfile)

        (cond
          ;; --- ra ban ve de chon / pick ---
          ((= kq 2)
           (setq mode (atoi (DTD:Get "mode")))
           (setq pts
             (cond
               ((= mode 0) (setq *DTD:PickOrderLock* nil) (DTD:PickParent))
               ((= mode 1) (setq *DTD:PickOrderLock* nil) (DTD:PickScreen))
               ((= mode 2)
                (setq *DTD:PickOrderLock* nil)
                (if (and bnames *DTD:Bidx* (nth *DTD:Bidx* bnames))
                  (DTD:ByName (nth *DTD:Bidx* bnames))
                  (progn (princ "\nChua chon ten block!") pts)))
               ((= mode 3) (setq *DTD:PickOrderLock* T) (DTD:PickPoints))
               (T pts)
             )
           )
           (if (not pts) (setq pts '()))
          )
          ;; --- pick 2 diem chi huong danh so ---
          ((= kq 6)
           (setq p1 (getpoint "\nPick diem DAU cua huong danh so: "))
           (if p1
             (setq p2 (getpoint p1 "\nPick diem CUOI cua huong danh so: ")))
           (if (and p1 p2 (> (distance p1 p2) 1e-8))
             (progn
               (setq dd (distance p1 p2))
               (setq *DTD:DirVec*
                 (list (list (car p1) (cadr p1))
                       (list (/ (- (car p2) (car p1)) dd)
                             (/ (- (cadr p2) (cadr p1)) dd))))
               (princ "\nDa ghi nhan huong danh so theo 2 diem vua pick.")
             )
             (progn
               (princ "\nChua pick du 2 diem - giu nguyen thu tu chon.")
               (DTD:Put "dir" "4")
             )
           )
          )
          ;; --- thuc hien / thoat ---
          (T (setq loop nil))
        )
      )
    )
  )

  ;; ------ DANH TOA DO ------
  (if (= kq 1)
    (if (> (length pts) 0)
      (progn
        (setq prefix (DTD:Get "prefix")
              start  (max 1 (atoi (DTD:Get "start")))
              dir    (atoi (DTD:Get "dir"))
              swap   (= (DTD:Get "swap") "1")
              usefld (= (DTD:Get "fld") "1")
              hchu   (distof (DTD:Get "hchu") 2)
              dec    (max 0 (atoi (DTD:Get "dec")))
              stylename (DTD:Get "style"))
        (if (or (not hchu) (<= hchu 0)) (setq hchu 2.0))
        (setq oldos (getvar "osmode"))
        (setvar "osmode" 0)
        (DTD:Layer)
        (setq sorted (DTD:SortPts pts dir) n (length sorted) i 0)
        ;; Tam cum diem - de cac nhan toa ra, giam chong cheo khi block sat nhau
        (setq cenx 0.0 ceny 0.0)
        (foreach p sorted (setq cenx (+ cenx (car p)) ceny (+ ceny (cadr p))))
        (setq center (list (/ cenx n) (/ ceny n)))
        (foreach p sorted
          (setq name (DTD:PtName prefix i start n))
          (DTD:Label p name hchu dec swap usefld center stylename)
          (setq i (1+ i))
        )
        (setvar "osmode" oldos)
        (if usefld (DTD:ReattachAll))
        (princ (strcat "\n==> Da danh toa do " (itoa n) " diem: "
                       (DTD:PtName prefix 0 start n) " den "
                       (DTD:PtName prefix (1- n) start n)
                       " (layer TOA-DO)."))
        (if usefld
          (princ "\n    Toa do dung FIELD, gan theo diem moc AN tai mui leader.\n    - Keo grip landing/chu HAY mui ten, hoac Move/Stretch - roi go REGEN (RE):\n      chu X=/Y= se TU DONG cap nhat dung theo vi tri MOI cua mui leader.\n    - Copy sang cho khac: dung lenh DTDCOPY de dam bao ban Copy cung tu dong dong bo."))
      )
      (princ "\nKhong co diem nao de danh toa do!")
    )
    (princ "\n*Da huy lenh.*")
  )
  (princ)
)

;; ============================================================
;; LENH DTDMOVE / DTDCOPY - tien ich doi cho / sao chep NHIEU nhan
;; toa do cung luc. (Rieng le, ban co the Move/keo grip mleader THANG
;; tren ban ve nhu binh thuong - reactor da tu dong dong bo diem moc,
;; chi can go REGEN la chu cap nhat dung). 2 lenh nay tien loi khi
;; can xu ly HANG LOAT nhieu nhan, va dam bao ban COPY moi cung duoc
;; gan reactor de tiep tuc tu dong dong bo ve sau.
;; ------------------------------------------------------------
(defun DTD:BuildLinkedSS (ss / n i e ss2 anchorE)
  (setq ss2 (ssadd) n (sslength ss) i 0)
  (repeat n
    (setq e (ssname ss i))
    (setq ss2 (ssadd e ss2))
    (setq anchorE (DTD:GetAnchorOf e))
    (if (and anchorE (not (ssmemb anchorE ss2))) (setq ss2 (ssadd anchorE ss2)))
    (setq i (1+ i))
  )
  ss2
)

(defun C:DTDMOVE (/ ss ss2)
  (princ "\nDTDMOVE - Chon 1 hay nhieu NHAN TOA DO can doi cho (Enter de ket thuc): ")
  (setq ss (ssget '((0 . "MULTILEADER"))))
  (if ss
    (progn
      (setq ss2 (DTD:BuildLinkedSS ss))
      (command "_.MOVE" ss2 "" PAUSE PAUSE)
      (command "_.REGEN")
      (DTD:ReattachAll)
      (princ "\n==> Da doi cho va tu dong cap nhat lai chu toa do.")
    )
    (princ "\nKhong chon duoc nhan toa do nao!")
  )
  (princ)
)

(defun C:DTDCOPY (/ ss ss2)
  (princ "\nDTDCOPY - Chon 1 hay nhieu NHAN TOA DO can sao chep sang cho MOI (Enter de ket thuc): ")
  (setq ss (ssget '((0 . "MULTILEADER"))))
  (if ss
    (progn
      (setq ss2 (DTD:BuildLinkedSS ss))
      (command "_.COPY" ss2 "" PAUSE PAUSE "")
      (command "_.REGEN")
      (DTD:ReattachAll)
      (princ "\n==> Da sao chep sang toa do moi va tu dong cap nhat lai chu toa do.")
    )
    (princ "\nKhong chon duoc nhan toa do nao!")
  )
  (princ)
)

;; Vua tai file: gan lai reactor cho TAT CA nhan toa do (co Field) da
;; co san trong ban ve hien tai - de tu dong dong bo tiep tuc hoat
;; dong ngay ca sau khi dong/mo lai ban ve hoac nap lai file lisp.
(DTD:ReattachAll)

(princ "\n>> Da tai DTD - Danh toa do block/diem. Go DTD de bat dau. <<")
(princ "\n>> Chinh sua truc tiep tren ban ve (grip/Move) roi go REGEN la chu tu cap nhat. <<")
(princ "\n>> Xu ly hang loat nhieu nhan: go DTDMOVE hoac DTDCOPY. <<")
(princ)

;;; --- HET [21] DTD / DTDMOVE / DTDCOPY ---

;;; ---------------------------------------------------------------------------
;;; [22] CD
;;;      Danh cao do tuong doi
;;;      Nguon: CD (DANH_CAO_DO).lsp
;;; ---------------------------------------------------------------------------
;;; ================================================================
;;; CD - Danh CAO DO TUONG DOI vao block thuoc tinh co ATT "CD" (v2)
;;; *** v2: Them DANH SACH SO XUONG cac block att - lisp tu quet
;;;     bang block, CHI liet ke nhung block co tham so ATT ten "CD",
;;;     ban chon 1 block trong do de lam viec (hoac nut Pick <) ***
;;;
;;; - Pick diem GOC (cao do +0.000), cao do = Y block - Y goc
;;; - Don vi ghi: m  (chia 1000, lam tron 3 so le: +12.500)
;;;              mm (giu nguyen,  lam tron so nguyen: +12500)
;;; - Cao do duong/bang 0 co dau "+", am co dau "-"
;;; - 3 che do:
;;;     + CHEN MOI lien tiep: pick diem -> chen block da chon
;;;       va dien ngay cao do vao ATT CD, Enter ket thuc
;;;     + BAM cap nhat tung block co san (Enter ket thuc)
;;;     + CAP NHAT theo ten block da chon: quet chon vung
;;;       hoac Enter = tat ca block cung ten trong ban ve
;;; Cach dung: go lenh CD
;;; ================================================================

(vl-load-com)

(if (null *cd-org*)  (setq *cd-org*  '(0.0 0.0 0.0)))
(if (null *cd-unit*) (setq *cd-unit* "u_m"))
(if (null *cd-mode*) (setq *cd-mode* "m_ins"))
(if (null *cd-idx*)  (setq *cd-idx*  0))
(if (null *cd-lay*)  (setq *cd-lay*  "(Giu nguyen)"))

;; ---------- Danh sach layer trong ban ve (bo layer xref) ----------
(defun cd:all-layers (/ lay name lst)
  (while (setq lay (tblnext "LAYER" (not lay)))
    (setq name (cdr (assoc 2 lay)))
    (if (not (vl-string-search "|" name))   ; bo layer cua xref
      (setq lst (cons name lst))
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Lay ten block (ho tro dynamic) ----------
(defun cd:ename->blkname (ent / obj)
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'EffectiveName)
    (vla-get-EffectiveName obj)
    (vla-get-Name obj)
  )
)

;; ---------- Dinh nghia block co ATTDEF ten "CD" khong ----------
(defun cd:def-has-cd (name / ent ed found)
  (setq ent (cdr (assoc -2 (tblsearch "BLOCK" name))))
  (while (and ent (not found))
    (setq ed (entget ent))
    (if (and (= "ATTDEF" (cdr (assoc 0 ed)))
             (= "CD" (strcase (cdr (assoc 2 ed)))))
      (setq found T)
    )
    (setq ent (entnext ent))
  )
  found
)

;; ---------- Danh sach block co ATT "CD" trong ban ve ----------
(defun cd:cd-blknames (/ blk name flags lst)
  (while (setq blk (tblnext "BLOCK" (not blk)))
    (setq name  (cdr (assoc 2 blk))
          flags (cdr (assoc 70 blk)))
    (if (and (= 2 (logand 2 flags))         ; co thuoc tinh
             (/= (substr name 1 1) "*")     ; bo an danh / layout
             (= 0 (logand 4 flags))         ; bo xref
             (cd:def-has-cd name))          ; co ATT ten "CD"
      (setq lst (cons name lst))
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Dinh dang cao do kem dau ----------
(defun cd:fmt (v unit / s old-dimzin)
  (if (= unit "u_m") (setq v (/ v 1000.0)))
  (setq old-dimzin (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq s (rtos v 2 (if (= unit "u_m") 3 0)))
  (setvar "DIMZIN" old-dimzin)
  (if (minusp v) s (strcat "+" s))
)

;; ---------- Ghi cao do vao ATT "CD" cua 1 block ----------
;; layname: layer ap cho ATT CD ("(Giu nguyen)" = khong doi)
;; Tra ve chuoi da ghi neu OK, nil neu block khong co ATT CD
(defun cd:set-cd (ent org unit layname / obj ip val res att)
  (setq obj (vlax-ename->vla-object ent)
        ip  (vlax-get obj 'InsertionPoint)
        val (cd:fmt (- (cadr ip) (cadr org)) unit)
        res nil)
  (if (and (vlax-property-available-p obj 'HasAttributes)
           (eq (vla-get-HasAttributes obj) :vlax-true))
    (foreach att (vlax-invoke obj 'GetAttributes)
      (if (= (strcase (vla-get-TagString att)) "CD")
        (progn
          (vla-put-TextString att val)
          ;; Ap layer cho ATT neu nguoi dung chon
          (if (and layname (/= layname "") (/= layname "(Giu nguyen)")
                   (tblsearch "LAYER" layname))
            (vl-catch-all-apply 'vla-put-Layer (list att layname))
          )
          (setq res val)
        )
      )
    )
  )
  res
)

;; ================================================================
;; XEM TRUOC BLOCK TREN HOP THOAI (ve lai net vao image tile)
;; ================================================================

;; ---------- Bien doi diem tu he block con -> he cha ----------
;; scale (sx sy) truoc, xoay ang sau, roi tinh tien ins
(defun cd:xf-pt (p ins sx sy ang / x y xr yr)
  (setq x  (* (car p) sx)
        y  (* (cadr p) sy)
        xr (- (* x (cos ang)) (* y (sin ang)))
        yr (+ (* x (sin ang)) (* y (cos ang))))
  (list (+ xr (car ins)) (+ yr (cadr ins)))
)

;; ---------- Gom cac doan thang tu dinh nghia block (de quy) ----------
;; Tra ve list ((p1 p2) ...) ; ho tro LINE, LWPOLYLINE, CIRCLE, ARC,
;; INSERT long nhau (toi da depth cap). ATTDEF/TEXT bo qua.
(defun cd:blk-segs (name depth / ent ed typ segs verts closed i a1 a2 n da
                     ctr rad p q ins sx sy ang sub)
  (setq segs '())
  (if (and (> depth 0) (tblsearch "BLOCK" name))
    (progn
      (setq ent (cdr (assoc -2 (tblsearch "BLOCK" name))))
      (while ent
        (setq ed (entget ent) typ (cdr (assoc 0 ed)))
        (cond
          ;; --- LINE ---
          ((= typ "LINE")
           (setq segs (cons (list (cdr (assoc 10 ed)) (cdr (assoc 11 ed))) segs))
          )
          ;; --- LWPOLYLINE (bo qua bulge - chi can xem truoc) ---
          ((= typ "LWPOLYLINE")
           (setq verts '())
           (foreach x ed
             (if (= (car x) 10) (setq verts (cons (cdr x) verts))))
           (setq verts (reverse verts)
                 closed (= 1 (logand 1 (cdr (assoc 70 ed))))
                 i 0)
           (while (< i (1- (length verts)))
             (setq segs (cons (list (nth i verts) (nth (1+ i) verts)) segs)
                   i (1+ i)))
           (if (and closed (> (length verts) 2))
             (setq segs (cons (list (last verts) (car verts)) segs)))
          )
          ;; --- CIRCLE (xap xi 24 canh) ---
          ((= typ "CIRCLE")
           (setq ctr (cdr (assoc 10 ed)) rad (cdr (assoc 40 ed)) i 0)
           (while (< i 24)
             (setq p (list (+ (car ctr) (* rad (cos (* i (/ pi 12.0)))))
                           (+ (cadr ctr) (* rad (sin (* i (/ pi 12.0))))))
                   q (list (+ (car ctr) (* rad (cos (* (1+ i) (/ pi 12.0)))))
                           (+ (cadr ctr) (* rad (sin (* (1+ i) (/ pi 12.0))))))
                   segs (cons (list p q) segs)
                   i (1+ i)))
          )
          ;; --- ARC (xap xi 16 doan) ---
          ((= typ "ARC")
           (setq ctr (cdr (assoc 10 ed)) rad (cdr (assoc 40 ed))
                 a1 (cdr (assoc 50 ed)) a2 (cdr (assoc 51 ed)))
           (if (< a2 a1) (setq a2 (+ a2 (* 2 pi))))
           (setq n 16 da (/ (- a2 a1) n) i 0)
           (while (< i n)
             (setq p (list (+ (car ctr) (* rad (cos (+ a1 (* i da)))))
                           (+ (cadr ctr) (* rad (sin (+ a1 (* i da))))))
                   q (list (+ (car ctr) (* rad (cos (+ a1 (* (1+ i) da)))))
                           (+ (cadr ctr) (* rad (sin (+ a1 (* (1+ i) da))))))
                   segs (cons (list p q) segs)
                   i (1+ i)))
          )
          ;; --- INSERT long nhau: lay net block con roi bien doi ---
          ((= typ "INSERT")
           (setq ins (cdr (assoc 10 ed))
                 sx  (cond ((cdr (assoc 41 ed))) (1.0))
                 sy  (cond ((cdr (assoc 42 ed))) (1.0))
                 ang (cond ((cdr (assoc 50 ed))) (0.0))
                 sub (cd:blk-segs (cdr (assoc 2 ed)) (1- depth)))
           (foreach s sub
             (setq segs (cons (list (cd:xf-pt (car s) ins sx sy ang)
                                    (cd:xf-pt (cadr s) ins sx sy ang))
                              segs))
           )
          )
        )
        (setq ent (entnext ent))
      )
    )
  )
  segs
)

;; ---------- Ve xem truoc block len image tile "prev" ----------
(defun cd:draw-preview (name / segs iw ih xs ys minx maxx miny maxy
                          w h sc offx offy px1 py1 px2 py2)
  (setq iw (dimx_tile "prev")
        ih (dimy_tile "prev"))
  (start_image "prev")
  (fill_image 0 0 iw ih -2)                  ; nen giong man hinh CAD
  (setq segs (cd:blk-segs name 3))
  (if segs
    (progn
      ;; Khung bao
      (setq xs (apply 'append (mapcar '(lambda (s) (list (caar s) (car (cadr s)))) segs))
            ys (apply 'append (mapcar '(lambda (s) (list (cadar s) (cadr (cadr s)))) segs))
            minx (apply 'min xs) maxx (apply 'max xs)
            miny (apply 'min ys) maxy (apply 'max ys)
            w (max (- maxx minx) 1e-9)
            h (max (- maxy miny) 1e-9)
            sc (min (/ (* iw 0.85) w) (/ (* ih 0.85) h))
            offx (/ (- iw (* w sc)) 2.0)
            offy (/ (- ih (* h sc)) 2.0))
      ;; Ve tung doan (truc Y cua image huong xuong -> lat lai)
      (foreach s segs
        (setq px1 (fix (+ offx (* (- (caar s) minx) sc)))
              py1 (fix (- ih (+ offy (* (- (cadar s) miny) sc))))
              px2 (fix (+ offx (* (- (car (cadr s)) minx) sc)))
              py2 (fix (- ih (+ offy (* (- (cadr (cadr s)) miny) sc)))))
        (vector_image px1 py1 px2 py2 7)
      )
    )
  )
  (end_image)
)

;; ---------- Tao file DCL tam ----------
(defun cd:make-dcl (/ fname f)
  (setq fname (vl-filename-mktemp "cd" nil ".dcl")
        f     (open fname "w"))
  (foreach line
    '("cd : dialog {"
      "  label = \"CD - Danh cao do tuong doi (ATT: CD)\";"
      "  : boxed_column {"
      "    label = \"Block cao do (chi liet ke block co ATT \\\"CD\\\")\";"
      "    : row {"
      "      : popup_list { key = \"blklist\"; edit_width = 30; }"
      "      : button { key = \"pick\"; label = \"Pick <\"; width = 10; }"
      "    }"
      "    : image {"
      "      key = \"prev\";"
      "      width = 44;"
      "      aspect_ratio = 0.45;"
      "      color = -2;"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Goc cao do (+0.000)\";"
      "    : row {"
      "      : text { key = \"orgtxt\"; label = \"\"; width = 32; }"
      "      : button { key = \"pickorg\"; label = \"Pick goc <\"; width = 12; }"
      "    }"
      "  }"
      "  : boxed_radio_row {"
      "    label = \"Don vi ghi cao do\";"
      "    : radio_button { key = \"u_m\";  label = \"m (chia 1000, 3 so le)\"; }"
      "    : radio_button { key = \"u_mm\"; label = \"mm (so nguyen)\"; }"
      "  }"
      "  : boxed_column {"
      "    label = \"Layer cua ATT CD\";"
      "    : popup_list { key = \"laylist\"; edit_width = 30; }"
      "  }"
      "  : boxed_column {"
      "    label = \"Ty le chen block\";"
      "    : edit_box { key = \"scale\"; label = \"Ty le :\"; edit_width = 10; }"
      "    : text { label = \"(Ap dung khi CHEN MOI; block co san giu nguyen ty le)\"; }"
      "  }"
      "  : boxed_radio_column {"
      "    label = \"Che do\";"
      "    : radio_button { key = \"m_ins\";   label = \"CHEN MOI lien tiep block da chon + danh cao do\"; }"
      "    : radio_button { key = \"m_click\"; label = \"Quet chon / bam chon NHIEU block co san de cap nhat\"; }"
      "    : radio_button { key = \"m_name\";  label = \"Cap nhat block cung ten da chon (quet / Enter = tat ca)\"; }"
      "  }"
      "  spacer;"
      "  ok_cancel;"
      "}")
    (write-line line f)
  )
  (close f)
  fname
)

;; ---------- Chuoi hien thi goc ----------
(defun cd:org-str (org)
  (strcat "Goc: X=" (rtos (car org) 2 3) "  Y=" (rtos (cadr org) 2 3))
)

;; ================================================================
;; LENH CHINH
;; ================================================================
(defun c:CD (/ dclfile dclid allnames done ret unit mode org p doc ms
               sel ent name blkName val ss i count nmiss go idx
               laylist lidx scstr sc)
  (setq allnames (cd:cd-blknames))
  (if (not allnames)
    (prompt "\n*** Ban ve khong co block nao chua ATT \"CD\"! ***")
    (progn
      (setq dclfile (cd:make-dcl)
            unit    *cd-unit*
            mode    *cd-mode*
            org     *cd-org*
            idx     (if (< *cd-idx* (length allnames)) *cd-idx* 0)
            laylist (cons "(Giu nguyen)" (cd:all-layers))
            lidx    (cond ((vl-position *cd-lay* laylist)) (0))
            scstr   "1"    ; ty le luon mac dinh 1 moi lan mo lenh
            done    nil)

      ;; ----- Vong lap hop thoai (dong tam de Pick block / Pick goc) -----
      (while (not done)
        (setq dclid (load_dialog dclfile))
        (if (not (new_dialog "cd" dclid))
          (progn (prompt "\n*** Loi: khong mo duoc hop thoai DCL! ***")
                 (setq done T ret 0))
          (progn
            (start_list "blklist") (mapcar 'add_list allnames) (end_list)
            (set_tile "blklist" (itoa idx))
            (start_list "laylist") (mapcar 'add_list laylist) (end_list)
            (set_tile "laylist" (itoa lidx))
            (set_tile "scale" scstr)
            (set_tile unit "1")
            (set_tile mode "1")
            (set_tile "orgtxt" (cd:org-str org))
            (cd:draw-preview (nth idx allnames))

            (action_tile "blklist"
              "(setq idx (atoi $value)) (cd:draw-preview (nth idx allnames))")
            (action_tile "laylist" "(setq lidx (atoi $value))")
            (action_tile "scale"   "(setq scstr $value)")
            (action_tile "u_m"     "(setq unit \"u_m\")")
            (action_tile "u_mm"    "(setq unit \"u_mm\")")
            (action_tile "m_ins"   "(setq mode \"m_ins\")")
            (action_tile "m_click" "(setq mode \"m_click\")")
            (action_tile "m_name"  "(setq mode \"m_name\")")
            (action_tile "pick"    "(done_dialog 2)")
            (action_tile "pickorg" "(done_dialog 3)")
            (action_tile "accept"
              (strcat
                "(setq scstr (get_tile \"scale\"))"
                "(if (and (setq sc (distof scstr)) (> sc 0.0))"
                "  (done_dialog 1)"
                "  (alert \"Ty le chen block phai la so > 0!\")"
                ")"
              )
            )
            (action_tile "cancel"  "(done_dialog 0)")

            (setq ret (start_dialog))
            (unload_dialog dclid)

            (cond
              ;; --- Pick block mau ngoai man hinh ---
              ((= ret 2)
               (setq sel (entsel "\nChon 1 block cao do (co ATT \"CD\") tren ban ve: "))
               (if (and sel (= "INSERT" (cdr (assoc 0 (entget (car sel))))))
                 (progn
                   (setq name (cd:ename->blkname (car sel)))
                   (if (vl-position name allnames)
                     (setq idx (vl-position name allnames))
                     (prompt "\n* Block vua chon khong co ATT \"CD\" - giu lua chon cu. *")
                   )
                 )
                 (prompt "\n* Khong chon duoc block - giu nguyen lua chon cu. *")
               )
              )
              ;; --- Pick diem goc ---
              ((= ret 3)
               (setq p (getpoint "\nPick diem GOC cao do (+0.000): "))
               (if p (setq org p))
              )
              ;; --- OK / Cancel ---
              (t (setq done T))
            )
          )
        )
      )
      (vl-file-delete dclfile)

      ;; ----- Thuc thi -----
      (if (= ret 1)
        (progn
          (setq blkName (nth idx allnames)
                *cd-unit* unit
                *cd-mode* mode
                *cd-org*  org
                *cd-idx*  idx
                *cd-lay*  (nth lidx laylist)
                doc (vla-get-ActiveDocument (vlax-get-acad-object))
                ms  (vla-get-ModelSpace doc)
                count 0
                nmiss 0)
          (prompt (strcat "\n[Block: " blkName
                          " | Goc Y=" (rtos (cadr org) 2 3)
                          " | Don vi: " (if (= unit "u_m") "m" "mm")
                          " | Ty le chen: " scstr "]"))
          (vla-StartUndoMark doc)
          (cond
            ;; === CHEN MOI lien tiep block da chon ===
            ((= mode "m_ins")
             (while (setq p (getpoint "\nPick diem chen block cao do (Enter ket thuc): "))
               (setq ent (vlax-vla-object->ename
                           (vla-InsertBlock ms (vlax-3d-point p) blkName sc sc sc 0.0)))
               (setq val (cd:set-cd ent org unit (nth lidx laylist)))
               (if val
                 (progn (setq count (1+ count))
                        (prompt (strcat "  ->  CD = " val)))
                 (prompt "\n  * Block khong co ATT \"CD\"?! *")
               )
             )
             (prompt (strcat "\n==> Da chen va danh cao do " (itoa count) " block."))
            )
            ;; === QUET CHON / BAM CHON nhieu block co san ===
            ((= mode "m_click")
             (prompt "\nQuet chon hoac bam chon cac block can danh cao do: ")
             (setq ss (ssget '((0 . "INSERT") (66 . 1))))
             (if ss
               (progn
                 (setq i 0)
                 (while (< i (sslength ss))
                   (setq ent (ssname ss i))
                   (setq val (cd:set-cd ent org unit (nth lidx laylist)))
                   (if val
                     (setq count (1+ count))
                     (setq nmiss (1+ nmiss))
                   )
                   (setq i (1+ i))
                 )
                 (vl-cmdf "_.REGENALL")
                 (prompt (strcat "\n==> Da danh cao do cho " (itoa count) " block"
                                 (if (> nmiss 0)
                                   (strcat " (" (itoa nmiss)
                                           " block bo qua vi khong co ATT \"CD\").")
                                   "."
                                 )))
               )
               (prompt "\n*** Khong chon duoc block nao! ***")
             )
            )
            ;; === CAP NHAT theo ten block da chon trong danh sach ===
            ((= mode "m_name")
             (prompt (strcat "\nQuet chon vung chua block \"" blkName
                             "\" (Enter = TAT CA trong ban ve): "))
             (setq ss (ssget '((0 . "INSERT") (66 . 1))))
             (if (not ss)
               (setq ss (ssget "_X" '((0 . "INSERT") (66 . 1))))
             )
             (if ss
               (progn
                 (setq i 0)
                 (while (< i (sslength ss))
                   (setq ent (ssname ss i))
                   (if (= (strcase (cd:ename->blkname ent)) (strcase blkName))
                     (if (cd:set-cd ent org unit (nth lidx laylist))
                       (setq count (1+ count))
                       (setq nmiss (1+ nmiss))
                     )
                   )
                   (setq i (1+ i))
                 )
                 (vl-cmdf "_.REGENALL")
                 (if (> (+ count nmiss) 0)
                   (prompt (strcat "\n==> Da cap nhat cao do " (itoa count)
                                   " block \"" blkName "\""
                                   (if (> nmiss 0)
                                     (strcat " (" (itoa nmiss) " block bo qua).")
                                     "."
                                   )))
                   (prompt (strcat "\n*** Khong tim thay block \"" blkName
                                   "\" nao trong pham vi chon! ***"))
                 )
               )
               (prompt "\n*** Khong co block thuoc tinh nao! ***")
             )
            )
          )
          (vla-EndUndoMark doc)
        )
        (prompt "\n* Da huy lenh. *")
      )
    )
  )
  (princ)
)

(prompt "\nDa nap lenh CD v2 - Danh cao do tuong doi, co danh sach chon block ATT \"CD\".")
(princ)

;;; --- HET [22] CD ---

;;; ---------------------------------------------------------------------------
;;; [23] TDD
;;;      Danh ly trinh / cao do tuong doi cho block
;;;      Nguon: TDD (LY_TRINH).lsp
;;; ---------------------------------------------------------------------------
;;; ================================================================
;;; TDD - Danh TOA DO TUONG DOI vao block thuoc tinh
;;;       ATT "LYTRINHTD" = X tuong doi ; ATT "CAODOTD" = Y tuong doi
;;;       (tinh tu diem goc do nguoi dung pick, lam tron 3 so le)
;;;
;;; Giao dien DCL:
;;;   - Chon block att: danh sach so xuong hoac nut "Pick <"
;;;   - Nut "Pick goc <": chon diem goc toa do tuong doi tren man hinh
;;;     (goc duoc nho lai cho lan chay sau)
;;;   - Cao chu + Font (Text Style) cho 2 dong thuoc tinh
;;;     (de trong / "(Giu nguyen)" = theo dinh nghia block)
;;;   - 2 che do:
;;;       + CHEN MOI: pick lien tuc nhieu diem, moi diem chen 1 block
;;;         va tu dien ngay LYTRINHTD / CAODOTD
;;;       + CAP NHAT: quet chon cac block da co, tinh lai toa do
;;;         theo goc moi (doi goc xong chay lai la ca loat tu sua)
;;;   - Tien ich them:
;;;       + Dinh dang ly trinh kieu "Km0+123.456" cho LYTRINHTD
;;;       + Doi don vi mm -> m (chia 1000) truoc khi ghi
;;; Cach dung: go lenh TDD
;;; ================================================================

(vl-load-com)

(if (null *tdd-org*)   (setq *tdd-org*   '(0.0 0.0 0.0)))
(if (null *tdd-idx*)   (setq *tdd-idx*   0))
(if (null *tdd-th*)    (setq *tdd-th*    ""))
(if (null *tdd-style*) (setq *tdd-style* "(Giu nguyen)"))
(if (null *tdd-km*)    (setq *tdd-km*    "0"))
(if (null *tdd-mm*)    (setq *tdd-mm*    "0"))
(if (null *tdd-mode*)  (setq *tdd-mode*  "m_ins"))

;; ---------- Lay ten block cua 1 doi tuong (ho tro dynamic) ----------
(defun tdd:ename->blkname (ent / obj)
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'EffectiveName)
    (vla-get-EffectiveName obj)
    (vla-get-Name obj)
  )
)

;; ---------- Danh sach block co thuoc tinh trong bang block ----------
(defun tdd:att-blknames (/ blk name flags lst)
  (while (setq blk (tblnext "BLOCK" (not blk)))
    (setq name  (cdr (assoc 2 blk))
          flags (cdr (assoc 70 blk)))
    (if (and (= 2 (logand 2 flags))
             (/= (substr name 1 1) "*")
             (= 0 (logand 4 flags)))
      (setq lst (cons name lst))
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Danh sach Text Style (bo shape) ----------
(defun tdd:all-styles (/ sty name flags lst)
  (while (setq sty (tblnext "STYLE" (not sty)))
    (setq name  (cdr (assoc 2 sty))
          flags (cdr (assoc 70 sty)))
    (if (and (/= name "") (= 0 (logand 1 flags)))
      (setq lst (cons name lst))
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Dinh dang so 3 so le / kieu ly trinh Km0+123.456 ----------
(defun tdd:fmt (v kmflag / sgn km rem s old-dimzin)
  (setq old-dimzin (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)                       ; giu du so 0 cuoi: 12.500
  (setq s
    (if (= kmflag "1")
      (progn
        (setq sgn (if (minusp v) "-" "")
              v   (abs v)
              km  (fix (/ v 1000.0))
              rem (- v (* km 1000.0)))
        (setq s (rtos rem 2 3))
        ;; dem phan nguyen cua rem du 3 chu so: 5.250 -> 005.250
        (while (< (vl-string-search "." s) 3)
          (setq s (strcat "0" s))
        )
        (strcat sgn "Km" (itoa km) "+" s)
      )
      (rtos v 2 3)
    )
  )
  (setvar "DIMZIN" old-dimzin)
  s
)

;; ---------- Ghi toa do tuong doi vao 2 ATT cua 1 block ----------
;; Tra ve so ATT da ghi duoc (0/1/2)
(defun tdd:set-atts (ent org kmflag mmflag thstr sname / obj atts tag ip dx dy n att)
  (setq obj (vlax-ename->vla-object ent)
        ip  (vlax-get obj 'InsertionPoint)
        dx  (- (car ip) (car org))
        dy  (- (cadr ip) (cadr org))
        n   0)
  (if (= mmflag "1")
    (setq dx (/ dx 1000.0) dy (/ dy 1000.0))   ; mm -> m
  )
  (if (and (vlax-property-available-p obj 'HasAttributes)
           (eq (vla-get-HasAttributes obj) :vlax-true))
    (foreach att (vlax-invoke obj 'GetAttributes)
      (setq tag (strcase (vla-get-TagString att)))
      (cond
        ((= tag "LYTRINHTD")
         (vla-put-TextString att (tdd:fmt dx kmflag))
         (tdd:apply-textprop att thstr sname)
         (setq n (1+ n))
        )
        ((= tag "CAODOTD")
         ;; Cao do luon kem dau: duong -> +12.500, am -> -12.500
         (vla-put-TextString att
           (if (minusp dy)
             (tdd:fmt dy "0")                          ; da co san dau -
             (strcat "+" (tdd:fmt dy "0"))             ; them dau +
           )
         )
         (tdd:apply-textprop att thstr sname)
         (setq n (1+ n))
        )
      )
    )
  )
  n
)

;; ---------- Ap cao chu / font cho 1 attribute ----------
(defun tdd:apply-textprop (att thstr sname)
  (if (and thstr (/= thstr "") (distof thstr) (> (distof thstr) 0.0))
    (vl-catch-all-apply 'vla-put-Height (list att (distof thstr)))
  )
  (if (and sname (/= sname "") (/= sname "(Giu nguyen)")
           (tblsearch "STYLE" sname))
    (vl-catch-all-apply 'vla-put-StyleName (list att sname))
  )
)

;; ---------- Tao file DCL tam ----------
(defun tdd:make-dcl (/ fname f)
  (setq fname (vl-filename-mktemp "tdd" nil ".dcl")
        f     (open fname "w"))
  (foreach line
    '("tdd : dialog {"
      "  label = \"TDD - Danh toa do tuong doi (LYTRINHTD / CAODOTD)\";"
      "  : boxed_column {"
      "    label = \"Block thuoc tinh\";"
      "    : row {"
      "      : popup_list { key = \"blklist\"; edit_width = 30; }"
      "      : button { key = \"pick\"; label = \"Pick <\"; width = 10; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Goc toa do tuong doi\";"
      "    : row {"
      "      : text { key = \"orgtxt\"; label = \"\"; width = 38; }"
      "      : button { key = \"pickorg\"; label = \"Pick goc <\"; width = 12; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Chu thuoc tinh\";"
      "    : row {"
      "      : popup_list { key = \"tstyle\"; label = \"Font :\"; edit_width = 18; }"
      "      : edit_box { key = \"theight\"; label = \"Cao chu :\"; edit_width = 8; }"
      "    }"
      "    : text { label = \"(De trong Cao chu / chon (Giu nguyen) = theo dinh nghia block)\"; }"
      "  }"
      "  : boxed_column {"
      "    label = \"Tuy chon gia tri\";"
      "    : toggle { key = \"mm2m\"; label = \"Doi don vi mm -> m (chia 1000)\"; }"
      "    : toggle { key = \"kmfmt\"; label = \"LYTRINHTD kieu ly trinh: Km0+123.456\"; }"
      "  }"
      "  : boxed_radio_column {"
      "    label = \"Che do\";"
      "    : radio_button { key = \"m_ins\"; label = \"Chen moi: pick nhieu diem lien tuc\"; }"
      "    : radio_button { key = \"m_upd\"; label = \"Cap nhat block da co theo goc moi (quet chon)\"; }"
      "    : radio_button { key = \"m_all\"; label = \"Cap nhat TAT CA block cung ten da chon trong ban ve\"; }"
      "  }"
      "  spacer;"
      "  : text { key = \"err\"; label = \"\"; width = 52; }"
      "  ok_cancel;"
      "}")
    (write-line line f)
  )
  (close f)
  fname
)

;; ---------- Chuoi hien thi goc ----------
(defun tdd:org-str (org)
  (strcat "Goc: X=" (rtos (car org) 2 3) "  Y=" (rtos (cadr org) 2 3))
)

;; ================================================================
;; LENH CHINH
;; ================================================================
(defun c:TDD (/ dclfile dclid allnames stylist done ret idx sidx thstr
                kmflag mmflag mode org sel ent name blkName doc ms p
                ss i n count nmiss)
  (setq allnames (tdd:att-blknames))
  (if (not allnames)
    (prompt "\n*** Ban ve khong co block thuoc tinh nao! ***")
    (progn
      (setq dclfile (tdd:make-dcl)
            stylist (cons "(Giu nguyen)" (tdd:all-styles))
            idx     (if (< *tdd-idx* (length allnames)) *tdd-idx* 0)
            sidx    (cond ((vl-position *tdd-style* stylist)) (0))
            thstr   *tdd-th*
            kmflag  *tdd-km*
            mmflag  *tdd-mm*
            mode    *tdd-mode*
            org     *tdd-org*
            done    nil)

      ;; ----- Vong lap hop thoai -----
      (while (not done)
        (setq dclid (load_dialog dclfile))
        (if (not (new_dialog "tdd" dclid))
          (progn (prompt "\n*** Loi: khong mo duoc hop thoai DCL! ***")
                 (setq done T ret 0))
          (progn
            (start_list "blklist") (mapcar 'add_list allnames) (end_list)
            (set_tile "blklist" (itoa idx))
            (start_list "tstyle") (mapcar 'add_list stylist) (end_list)
            (set_tile "tstyle" (itoa sidx))
            (set_tile "theight" thstr)
            (set_tile "kmfmt" kmflag)
            (set_tile "mm2m"  mmflag)
            (set_tile mode "1")
            (set_tile "orgtxt" (tdd:org-str org))

            (action_tile "blklist" "(setq idx (atoi $value))")
            (action_tile "tstyle"  "(setq sidx (atoi $value))")
            (action_tile "theight" "(setq thstr $value)")
            (action_tile "kmfmt"   "(setq kmflag $value)")
            (action_tile "mm2m"    "(setq mmflag $value)")
            (action_tile "m_ins"   "(setq mode \"m_ins\")")
            (action_tile "m_upd"   "(setq mode \"m_upd\")")
            (action_tile "m_all"   "(setq mode \"m_all\")")
            (action_tile "pick"    "(done_dialog 2)")
            (action_tile "pickorg" "(done_dialog 3)")
            (action_tile "accept"
              (strcat
                "(setq thstr (get_tile \"theight\"))"
                "(if (and (/= thstr \"\")"
                "         (or (not (distof thstr)) (<= (distof thstr) 0.0)))"
                "  (set_tile \"err\" \"*** Cao chu phai la so > 0 (hoac de trong)! ***\")"
                "  (done_dialog 1)"
                ")"
              )
            )
            (action_tile "cancel" "(done_dialog 0)")

            (setq ret (start_dialog))
            (unload_dialog dclid)

            (cond
              ;; --- Pick block mau ---
              ((= ret 2)
               (setq sel (entsel "\nChon 1 block thuoc tinh mau tren ban ve: "))
               (if (and sel (= "INSERT" (cdr (assoc 0 (entget (car sel))))))
                 (progn
                   (setq ent  (car sel)
                         name (if (vlax-property-available-p
                                    (vlax-ename->vla-object ent) 'EffectiveName)
                                (vla-get-EffectiveName (vlax-ename->vla-object ent))
                                (vla-get-Name (vlax-ename->vla-object ent))))
                   (if (vl-position name allnames)
                     (setq idx (vl-position name allnames))
                     (prompt "\n* Block vua chon khong co thuoc tinh - giu lua chon cu. *")
                   )
                 )
                 (prompt "\n* Khong chon duoc block - giu nguyen lua chon cu. *")
               )
              )
              ;; --- Pick diem goc ---
              ((= ret 3)
               (setq p (getpoint "\nPick diem GOC toa do tuong doi: "))
               (if p (setq org p))
              )
              ;; --- OK / Cancel ---
              (t (setq done T))
            )
          )
        )
      )
      (vl-file-delete dclfile)

      ;; ----- Thuc thi -----
      (if (= ret 1)
        (progn
          (setq blkName (nth idx allnames)
                *tdd-idx*   idx
                *tdd-th*    thstr
                *tdd-style* (nth sidx stylist)
                *tdd-km*    kmflag
                *tdd-mm*    mmflag
                *tdd-mode*  mode
                *tdd-org*   org
                doc (vla-get-ActiveDocument (vlax-get-acad-object))
                ms  (vla-get-ModelSpace doc)
                count 0
                nmiss 0)

          (vla-StartUndoMark doc)
          (cond
            ;; === CHEN MOI: pick lien tuc ===
            ((= mode "m_ins")
             (prompt (strcat "\n[Goc: X=" (rtos (car org) 2 3)
                             " Y=" (rtos (cadr org) 2 3) "]"))
             (while (setq p (getpoint "\nPick diem chen block (Enter/Esc de ket thuc): "))
               (setq ent (vlax-vla-object->ename
                           (vla-InsertBlock ms (vlax-3d-point p) blkName 1.0 1.0 1.0 0.0)))
               (setq n (tdd:set-atts ent org kmflag mmflag thstr (nth sidx stylist)))
               (if (< n 2)
                 (prompt "\n  * Canh bao: block khong du 2 ATT LYTRINHTD/CAODOTD! *"))
               (setq count (1+ count))
             )
             (prompt (strcat "\n==> Da chen va danh toa do " (itoa count) " block."))
            )
            ;; === CAP NHAT block da co ===
            ((= mode "m_upd")
             (prompt "\nQuet chon cac block can cap nhat toa do theo goc moi: ")
             (setq ss (ssget '((0 . "INSERT") (66 . 1))))
             (if ss
               (progn
                 (setq i 0)
                 (while (< i (sslength ss))
                   (setq n (tdd:set-atts (ssname ss i) org kmflag mmflag
                                         thstr (nth sidx stylist)))
                   (if (> n 0) (setq count (1+ count)) (setq nmiss (1+ nmiss)))
                   (setq i (1+ i))
                 )
                 (vl-cmdf "_.REGENALL")
                 (prompt (strcat "\n==> Da cap nhat " (itoa count) " block"
                                 (if (> nmiss 0)
                                   (strcat " (" (itoa nmiss)
                                           " block bo qua vi khong co ATT LYTRINHTD/CAODOTD).")
                                   "."
                                 )))
               )
               (prompt "\n*** Khong chon duoc block nao! ***")
             )
            )
            ;; === CAP NHAT TAT CA block cung ten trong ban ve ===
            ((= mode "m_all")
             (setq ss (ssget "_X" '((0 . "INSERT") (66 . 1))))
             (if ss
               (progn
                 (setq i 0)
                 (while (< i (sslength ss))
                   (setq ent (ssname ss i))
                   ;; Chi cap nhat block dung ten da chon (ho tro dynamic block)
                   (if (= (strcase (tdd:ename->blkname ent)) (strcase blkName))
                     (progn
                       (setq n (tdd:set-atts ent org kmflag mmflag
                                             thstr (nth sidx stylist)))
                       (if (> n 0) (setq count (1+ count)) (setq nmiss (1+ nmiss)))
                     )
                   )
                   (setq i (1+ i))
                 )
                 (vl-cmdf "_.REGENALL")
                 (if (> (+ count nmiss) 0)
                   (prompt (strcat "\n==> Da cap nhat " (itoa count)
                                   " block \"" blkName "\" trong toan ban ve"
                                   (if (> nmiss 0)
                                     (strcat " (" (itoa nmiss)
                                             " block bo qua vi khong co ATT LYTRINHTD/CAODOTD).")
                                     "."
                                   )))
                   (prompt (strcat "\n*** Khong tim thay block \"" blkName
                                   "\" nao trong ban ve! ***"))
                 )
               )
               (prompt "\n*** Ban ve khong co block thuoc tinh nao! ***")
             )
            )
          )
          (vla-EndUndoMark doc)
        )
        (prompt "\n* Da huy lenh. *")
      )
    )
  )
  (princ)
)

(prompt "\nDa nap lenh TDD - Danh toa do tuong doi LYTRINHTD/CAODOTD cho block att.")
(princ)

;;; --- HET [23] TDD ---

;;; ---------------------------------------------------------------------------
;;; [24] MC
;;;      Danh ky hieu mat cat (v1.6 - danh sach block ATT MATCAT + xem truoc)
;;;      Nguon: MC.lsp
;;; ---------------------------------------------------------------------------
;;; MC v1.6 - Chen ky hieu mat cat lien tuc (2 diem / 1 lan)
;;; Lenh: MC
;;; MOI o v1.2:
;;;   - Ban ve chua co block -> tu tim file .dwg tren support path
;;;     va nap dinh nghia block truoc khi chen (khong bao loi nua)
;;;   - O "Ten block" nhan ca ten block lan duong dan day du file .dwg
;;;   - Them nut "Tim file .dwg..."
;;; MOI o v1.4:
;;;   - Them toggle LAT BLOCK: bat len la lat guong CA 2 block cua cap,
;;;     dao mui ten sang phia ben kia duong cat.
;;;   - Truc guong nam DOC THEO duong cat, di qua dung diem chen
;;;     -> block chi dao huong, khong xe dich vi tri.
;;;   - Tu dat MIRRTEXT = 0 khi lat de chu/ATT khong bi nguoc.
;;; MOI o v1.5:
;;;   - Them O SO XUONG (popup list) liet ke cac BLOCK CO ATT "MATCAT"
;;;     dang co trong ban ve -> chon thang, khong phai go tay ten block.
;;;   - O "Loc ATT tag" cho phep doi tag can loc (mac dinh MATCAT),
;;;     nhan ca ky tu dai dien (vd: MAT*).
;;;       + De TRONG   -> liet ke moi block CO Attribute (bat ky tag nao)
;;;       + Go dau *   -> liet ke TAT CA block trong ban ve
;;;   - Nut "Loc lai" de quet lai ban ve sau khi vua chen/nap them block.
;;;   - Chon block trong danh sach: tu dien ten vao o "Ten block" va
;;;     tu dien tag vao o "Tag ATT" neu o do dang bo trong.
;;; MOI o v1.6:
;;;   - NHUNG O XEM TRUOC (view CAD) ngay trong hop thoai: nen den, ve lai
;;;     hinh dang block bang vector - giong che do xem truoc cua CAD.
;;;   - Tu dong ve lai khi doi dong trong danh sach hoac go tay ten block.
;;;   - Ve duoc: Line, Circle, Arc, Polyline, Spline, Ellipse, SOLID/TRACE
;;;     (mui ten mat cat), block long nhau (toi da 3 cap).
;;;     Text / ATTDEF ve KHUNG BAO mo (mau xam) de biet cho dat nhan.
;;;   - Giu nguyen mau ACI cua doi tuong nhu ngoai ban ve.
;;;   - Co bo nho dem: block da ve 1 lan thi doi qua doi lai khong bi giat.
;;; ============================================================

(vl-load-com)

;; --------- Bien nho theo session ---------
(if (null *mc-name*)  (setq *mc-name*  "A"))
(if (null *mc-blk*)   (setq *mc-blk*   "MC0"))
(if (null *mc-tag*)   (setq *mc-tag*   ""))
(if (null *mc-step*)  (setq *mc-step*  "1"))
(if (null *mc-auto*)  (setq *mc-auto*  T))
(if (null *mc-rot*)   (setq *mc-rot*   T))
(if (null *mc-dbg*)   (setq *mc-dbg*   nil))
(if (null *mc-flip*)  (setq *mc-flip*  nil))    ; *** v1.4: T = lat ca 2 block
(if (null *mc-ftag*)  (setq *mc-ftag*  "MATCAT")) ; *** v1.5: tag dung de loc block
(if (null *mc-scale*)
  (setq *mc-scale*
    (cond
      ((and (boundp '*tl-scale*) *tl-scale*) *tl-scale*)
      ((and (boundp 'scale) (numberp scale)) (rtos scale 2 4))
      (t "1")
    )
  )
)

;; ================= TIEN ICH TANG TEN =================
(defun mc-digit-p (c) (and (>= c 48) (<= c 57)))
(defun mc-alpha-p (c) (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122))))

(defun mc-pad (n w / s)
  (setq s (itoa n))
  (while (< (strlen s) w) (setq s (strcat "0" s)))
  s
)

(defun mc-a2n (s / n i)
  (setq n 0 i 1)
  (while (<= i (strlen s))
    (setq n (+ (* n 26) (- (ascii (strcase (substr s i 1))) 64)))
    (setq i (1+ i))
  )
  n
)

(defun mc-n2a (n low / s r)
  (setq s "")
  (while (> n 0)
    (setq r (rem n 26))
    (if (= r 0) (setq r 26))
    (setq s (strcat (chr (+ 64 r)) s))
    (setq n (/ (- n r) 26))
  )
  (if low (strcase s T) s)
)

(defun mc-next (s step / i head tail n low)
  (if (or (null s) (= s "") (null step) (< step 1))
    s
    (progn
      (setq i (strlen s))
      (while (and (> i 0) (mc-digit-p (ascii (substr s i 1))))
        (setq i (1- i))
      )
      (setq tail (substr s (1+ i)) head (substr s 1 i))
      (if (/= tail "")
        (strcat head (mc-pad (+ (atoi tail) step) (strlen tail)))
        (progn
          (setq i (strlen s))
          (while (and (> i 0) (mc-alpha-p (ascii (substr s i 1))))
            (setq i (1- i))
          )
          (setq tail (substr s (1+ i)) head (substr s 1 i))
          (if (/= tail "")
            (progn
              (setq low (>= (ascii (substr tail (strlen tail) 1)) 97))
              (setq n (+ (mc-a2n tail) step))
              (strcat head (mc-n2a n low))
            )
            s
          )
        )
      )
    )
  )
)

;; ================= TIM / NAP BLOCK =================
;; Tra ve: (list ten-block duong-dan-file)  hoac nil neu khong tim thay
;;  - Da co trong ban ve         -> (list nm nil)
;;  - Tim thay file .dwg         -> (list nm path)
(defun mc-resolve (s / nm f)
  (setq s (vl-string-trim " \t\"" s))
  (cond
    ((= s "") nil)
    ;; nguoi dung go ca duong dan / ten file .dwg
    ((wcmatch (strcase s) "*.DWG")
     (setq f (findfile s))
     (if f (list (vl-filename-base f) f) nil)
    )
    ;; ten block: uu tien dinh nghia da co trong ban ve
    ((tblsearch "BLOCK" s) (list s nil))
    ;; chua co -> tim file cung ten tren support path
    (t
     (setq f (findfile (strcat s ".dwg")))
     (if f (list s f) nil)
    )
  )
)

;; Nap dinh nghia block tu file vao ban ve (chen tam roi xoa)
(defun mc-define (nm f / e0 e1 ok)
  (if (tblsearch "BLOCK" nm)
    T
    (progn
      (setq e0 (entlast))
      (vl-catch-all-apply
        'command
        (list "_.-INSERT" (strcat nm "=\"" f "\"")
              "_non" (list 0.0 0.0 0.0) 1.0 1.0 0.0)
      )
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (setq e1 (entlast))
      ;; xoa doi tuong tam vua chen (chi xoa khi dung la no)
      (if (and e1 (not (eq e1 e0))
               (= (cdr (assoc 0 (entget e1))) "INSERT")
               (= (strcase (cdr (assoc 2 (entget e1)))) (strcase nm)))
        (entdel e1)
      )
      (setq ok (if (tblsearch "BLOCK" nm) T nil))
      (if ok
        (princ (strcat "\nDa nap dinh nghia block \"" nm "\" tu: " f))
        (princ (strcat "\nKhong nap duoc block tu file: " f))
      )
      ok
    )
  )
)

;; ============ *** v1.5: QUET BLOCK CO ATT THEO TAG ============
;; Kiem tra 1 dinh nghia block co chua ATTDEF khop tag hay khong.
;;   tag = ""  -> chi can block co bat ky ATTDEF nao
;;   tag khac  -> so khop kieu wcmatch (nhan ky tu dai dien: * ? # @ ~ ,)
(defun mc-blk-has-att (bname tag / bn en ed typ tg found)
  (setq found nil)
  (if (setq bn (tblobjname "BLOCK" bname))
    (progn
      (setq en (entnext bn))
      (while (and en (not found))
        (setq ed  (entget en)
              typ (cdr (assoc 0 ed))
        )
        (cond
          ((= typ "ENDBLK") (setq en nil))
          ((= typ "ATTDEF")
           (setq tg (strcase (cdr (assoc 2 ed))))
           (if (or (= tag "") (wcmatch tg (strcase tag)))
             (setq found T)
             (setq en (entnext en))
           )
          )
          (t (setq en (entnext en)))
        )
      )
    )
  )
  found
)

;; Tra ve danh sach ten block (da sap xep A-Z) thoa dieu kien loc.
;;   tag = "*" -> lay TAT CA block (khong can co ATT)
;;   tag = ""  -> block co bat ky ATTDEF nao
;;   nguoc lai -> block co ATTDEF khop tag
;; Da loai: block an danh (*U, *D, *Model_Space...), XREF va block phu thuoc XREF.
(defun mc-list-blocks (tag / rec nm flg lst)
  (setq lst nil
        tag (vl-string-trim " \t" tag)
        rec (tblnext "BLOCK" T)
  )
  (while rec
    (setq nm  (cdr (assoc 2 rec))
          flg (cdr (assoc 70 rec))
    )
    (if (null flg) (setq flg 0))
    (if (and nm
             (/= nm "")
             (/= (substr nm 1 1) "*")        ; bo block an danh / khong gian
             (zerop (logand 1  flg))         ; bo anonymous
             (zerop (logand 4  flg))         ; bo external reference
             (zerop (logand 8  flg))         ; bo block phu thuoc xref
             (zerop (logand 32 flg))         ; bo xref chua resolve
             (or (= tag "*") (mc-blk-has-att nm tag))
        )
      (setq lst (cons nm lst))
    )
    (setq rec (tblnext "BLOCK"))
  )
  (if lst
    (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
    nil
  )
)

;; Co chua ky tu dai dien khong?
(defun mc-has-wild (s)
  (or (vl-string-search "*" s) (vl-string-search "?" s)
      (vl-string-search "#" s) (vl-string-search "@" s)
      (vl-string-search "~" s) (vl-string-search "," s)
      (vl-string-search "[" s)
  )
)

;; Quet lai ban ve va do vao popup_list "blist".
(defun mc-refresh-list (/ cur i idx n)
  (setq *mc-ftag*  (vl-string-trim " \t" (get_tile "ftag"))
        *mc-blist* (mc-list-blocks *mc-ftag*)
        cur        (strcase (vl-string-trim " \t\"" (get_tile "blk")))
  )
  (start_list "blist")
  (if *mc-blist*
    (foreach b *mc-blist* (add_list b))
    (add_list "<khong co block nao thoa dieu kien>")
  )
  (end_list)
  ;; chon lai dung block dang ghi trong o "Ten block" (neu co trong danh sach)
  (setq i 0 idx nil)
  (foreach b *mc-blist*
    (if (and (null idx) (= (strcase b) cur)) (setq idx i))
    (setq i (1+ i))
  )
  (set_tile "blist" (if idx (itoa idx) "0"))
  (setq n (length *mc-blist*))
  (set_tile "binfo"
    (cond
      ((= n 0) (strcat "Khong tim thay block nao co ATT \""
                       (if (= *mc-ftag* "") "<bat ky>" *mc-ftag*) "\" trong ban ve."))
      ((= *mc-ftag* "*") (strcat "Co " (itoa n) " block trong ban ve (khong loc ATT)."))
      ((= *mc-ftag* "")  (strcat "Co " (itoa n) " block co Attribute."))
      (t (strcat "Co " (itoa n) " block co ATT \"" (strcase *mc-ftag*) "\"."))
    )
  )
  ;; *** v1.6: ve lai o xem truoc theo block dang duoc chon
  (mc-draw-prev (if (and idx *mc-blist*) (nth idx *mc-blist*) (get_tile "blk")))
  (princ)
)

;; ======= *** v1.6: NHUNG VIEW CAD - XEM TRUOC HINH DANG BLOCK =======
;; Ve lai hinh dang block bang vector_image tren nen den, giong CAD.

;; --- Bien doi 1 diem theo ty le / goc xoay / diem chen ---
(defun mc-tx (p sx sy rot off / x y c s)
  (setq x (* sx (car p))
        y (* sy (cadr p))
        c (cos rot)
        s (sin rot)
  )
  (list (+ (car off)  (- (* x c) (* y s)))
        (+ (cadr off) (+ (* x s) (* y c))))
)

;; --- Lay so an toan tu thuoc tinh ActiveX ---
(defun mc-num (obj prop def / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (or (vl-catch-all-error-p r) (not (numberp r))) def r)
)

;; --- Mau ve cho 1 doi tuong (chi so mau ACI, hop voi nen den) ---
(defun mc-col (e / c lay r)
  (setq c (vl-catch-all-apply 'vla-get-Color (list e)))
  (if (or (vl-catch-all-error-p c) (not (numberp c))) (setq c 256))
  (if (or (= c 256) (= c 0))            ; BYLAYER / BYBLOCK -> lay mau layer
    (progn
      (setq lay (vl-catch-all-apply 'vla-get-Layer (list e)))
      (if (not (vl-catch-all-error-p lay))
        (progn
          (setq r (tblsearch "LAYER" lay))
          (if (and r (assoc 62 r)) (setq c (abs (cdr (assoc 62 r)))))
        )
      )
    )
  )
  (cond
    ((not (numberp c)) 7)
    ((or (= c 0) (= c 256) (> c 255)) 7) ; khong ro -> trang
    (t c)
  )
)

;; --- Khung bao (dung cho TEXT / MTEXT / ATTDEF) ---
(defun mc-bbox-pts (e / mn mx a b)
  (if (vl-catch-all-error-p
        (vl-catch-all-apply 'vla-GetBoundingBox (list e 'mn 'mx)))
    nil
    (progn
      (setq a (vlax-safearray->list (vlax-variant-value mn))
            b (vlax-safearray->list (vlax-variant-value mx))
      )
      (if (and (> (abs (- (car b) (car a))) 1e-9)
               (> (abs (- (cadr b) (cadr a))) 1e-9))
        (list (list (car a) (cadr a)) (list (car b) (cadr a))
              (list (car b) (cadr b)) (list (car a) (cadr b))
              (list (car a) (cadr a)))
        nil
      )
    )
  )
)

;; --- Chuoi diem cua 1 doi tuong (toa do noi bo block) ---
(defun mc-ent-pts (e / nm en sp ep n i lst r pts)
  (setq nm  (vl-catch-all-apply 'vla-get-ObjectName (list e))
        lst nil
  )
  (if (vl-catch-all-error-p nm)
    nil
    (progn
      (setq en (vlax-vla-object->ename e))
      (cond
        ;; duong thang: 2 diem la du
        ((= nm "AcDbLine")
         (setq lst (list (vlax-get e 'StartPoint) (vlax-get e 'EndPoint))))
        ;; SOLID / TRACE: mui ten mat cat thuong la SOLID -> ve vien 4 goc
        ((member nm '("AcDbSolid" "AcDbTrace"))
         (setq r   (entget en)
               pts (vl-remove nil (list (cdr (assoc 10 r)) (cdr (assoc 11 r))
                                        (cdr (assoc 13 r)) (cdr (assoc 12 r))))
         )
         (if (> (length pts) 2) (setq lst (append pts (list (car pts)))))
        )
        ;; chu / attribute: ve khung bao mo de biet cho dat nhan
        ((member nm '("AcDbText" "AcDbMText" "AcDbAttributeDefinition"))
         (setq lst (mc-bbox-pts e)))
        ;; bo qua cho nhe
        ((member nm '("AcDbHatch" "AcDbPoint")) (setq lst nil))
        ;; con lai: lay mau theo duong cong
        (t
         (setq sp (vl-catch-all-apply 'vlax-curve-getStartParam (list en))
               ep (vl-catch-all-apply 'vlax-curve-getEndParam   (list en))
         )
         (if (or (vl-catch-all-error-p sp) (vl-catch-all-error-p ep)
                 (not (numberp sp)) (not (numberp ep)) (<= (- ep sp) 1e-12))
           (setq lst nil)
           (progn
             (setq n (fix (* 6.0 (- ep sp))))
             (if (< n 8)   (setq n 8))
             (if (> n 100) (setq n 100))
             (setq i 0)
             (repeat (1+ n)
               (setq r (vl-catch-all-apply
                         'vlax-curve-getPointAtParam
                         (list en (+ sp (* (- ep sp) (/ (float i) n))))))
               (if (and (not (vl-catch-all-error-p r)) r)
                 (setq lst (cons r lst)))
               (setq i (1+ i))
             )
             (setq lst (reverse lst))
           )
         )
        )
      )
      lst
    )
  )
)

;; --- Duyet dinh nghia block -> danh sach doan (x1 y1 x2 y2 mau) ---
(defun mc-collect (bo sx sy rot off depth / segs nm pts col p q i
                   blks bo2 ip co cs2 cr nm2)
  (setq segs nil
        blks (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (vlax-for e bo
    (setq nm (vl-catch-all-apply 'vla-get-ObjectName (list e)))
    (if (not (vl-catch-all-error-p nm))
      (if (= nm "AcDbBlockReference")
        ;; ----- block long nhau: de quy toi da 3 cap -----
        (if (< depth 3)
          (progn
            (setq ip (vl-catch-all-apply 'vlax-get (list e 'InsertionPoint)))
            (if (not (vl-catch-all-error-p ip))
              (progn
                (setq co  (mc-tx ip sx sy rot off)
                      cs2 (* sx (mc-num e 'XScaleFactor 1.0))
                      cr  (+ rot (mc-num e 'Rotation 0.0))
                      nm2 (vl-catch-all-apply 'vla-get-Name (list e))
                )
                (setq bo2 (if (vl-catch-all-error-p nm2)
                            nm2
                            (vl-catch-all-apply 'vla-Item (list blks nm2))))
                (if (not (vl-catch-all-error-p bo2))
                  (setq segs (append segs
                               (mc-collect bo2 cs2
                                 (* sy (mc-num e 'YScaleFactor 1.0))
                                 cr co (1+ depth))))
                )
              )
            )
          )
        )
        ;; ----- doi tuong thuong -----
        (progn
          (setq pts (mc-ent-pts e))
          (if (and pts (> (length pts) 1))
            (progn
              ;; chu / ATT ve mau xam mo cho de phan biet
              (setq col (if (member nm '("AcDbText" "AcDbMText"
                                         "AcDbAttributeDefinition"))
                          8
                          (mc-col e))
                    i   0
              )
              (while (< i (1- (length pts)))
                (setq p (mc-tx (nth i pts) sx sy rot off)
                      q (mc-tx (nth (1+ i) pts) sx sy rot off)
                )
                (setq segs (cons (list (car p) (cadr p) (car q) (cadr q) col) segs))
                (setq i (1+ i))
              )
            )
          )
        )
      )
    )
  )
  segs
)

;; --- Lay (co bo nho dem) cac doan ve cua 1 block theo ten ---
(defun mc-get-segs (bname / pr bo segs)
  (setq pr (assoc (strcase bname) *mc-segcache*))
  (if pr
    (cdr pr)
    (progn
      (setq bo (vl-catch-all-apply
                 'vla-Item
                 (list (vla-get-Blocks
                         (vla-get-ActiveDocument (vlax-get-acad-object)))
                       bname)))
      (setq segs (if (vl-catch-all-error-p bo)
                   nil
                   (mc-collect bo 1.0 1.0 0.0 '(0.0 0.0) 0)))
      (setq *mc-segcache* (cons (cons (strcase bname) segs) *mc-segcache*))
      segs
    )
  )
)

(defun mc-clamp (v lo hi) (max lo (min hi (fix v))))

;; --- Ve xem truoc vao o image "vprev" ---
;; LUU Y: moi set_tile phai xong TRUOC khi mo start_image,
;;        goi set_tile giua start_image/end_image se xoa het vector da ve.
(defun mc-draw-prev (bname / segs dx dy minx maxx miny maxy
                     k ox oy s lst txt x1 y1 x2 y2)
  (setq dx  (dimx_tile "vprev")
        dy  (dimy_tile "vprev")
        lst nil
        txt " "
  )
  (setq bname (vl-string-trim " \t\"" (if bname bname "")))
  ;; go ca duong dan .dwg -> lay ten file lam ten block
  (if (wcmatch (strcase bname) "*.DWG") (setq bname (vl-filename-base bname)))
  (cond
    ((= bname "") (setq txt " "))
    ((not (tblsearch "BLOCK" bname))
     (setq txt (strcat "\"" bname "\" chua co trong ban ve")))
    (t
     (setq segs (mc-get-segs bname))
     (if (null segs)
       (setq txt (strcat bname " - khong ve duoc xem truoc"))
       (progn
         ;; --- khung bao ---
         (setq minx nil)
         (foreach s segs
           (if (null minx)
             (setq minx (min (car s) (caddr s))    maxx (max (car s) (caddr s))
                   miny (min (cadr s) (cadddr s))  maxy (max (cadr s) (cadddr s)))
             (setq minx (min minx (car s) (caddr s))
                   maxx (max maxx (car s) (caddr s))
                   miny (min miny (cadr s) (cadddr s))
                   maxy (max maxy (cadr s) (cadddr s)))
           )
         )
         (if (< (- maxx minx) 1e-9) (setq maxx (+ minx 1.0)))
         (if (< (- maxy miny) 1e-9) (setq maxy (+ miny 1.0)))
         (setq k  (min (/ (float (- dx 8)) (- maxx minx))
                       (/ (float (- dy 8)) (- maxy miny)))
               ox (/ (- dx (* k (- maxx minx))) 2.0)
               oy (/ (- dy (* k (- maxy miny))) 2.0)
         )
         ;; --- doi sang toa do pixel TRUOC, chua ve voi ---
         (foreach s segs
           (setq x1 (mc-clamp (+ ox (* k (- (car s)     minx))) 0 (1- dx))
                 y1 (mc-clamp (- dy oy (* k (- (cadr s)   miny))) 0 (1- dy))
                 x2 (mc-clamp (+ ox (* k (- (caddr s)   minx))) 0 (1- dx))
                 y2 (mc-clamp (- dy oy (* k (- (cadddr s) miny))) 0 (1- dy))
           )
           (setq lst (cons (list x1 y1 x2 y2 (nth 4 s)) lst))
         )
         (setq txt (strcat bname "   |   " (itoa (length segs)) " doan ve"))
       )
     )
    )
  )
  ;; --- set_tile xong het roi moi mo image ---
  (set_tile "pinfo" txt)
  (start_image "vprev")
  (fill_image 0 0 dx dy 0)                       ; nen den nhu CAD
  (vector_image 0 0 (1- dx) 0 8)                 ; vien khung
  (vector_image (1- dx) 0 (1- dx) (1- dy) 8)
  (vector_image (1- dx) (1- dy) 0 (1- dy) 8)
  (vector_image 0 (1- dy) 0 0 8)
  (foreach s lst
    (vector_image (car s) (cadr s) (caddr s) (cadddr s) (nth 4 s))
  )
  (end_image)
  (princ)
)

;; Chon 1 dong trong popup_list -> do ten vao o "Ten block".
(defun mc-pick-blk (/ i nm ft)
  (setq i (atoi (get_tile "blist")))
  (if (and *mc-blist* (>= i 0) (< i (length *mc-blist*)))
    (progn
      (setq nm (nth i *mc-blist*))
      (set_tile "blk" nm)
      ;; neu o Tag ATT dang trong va bo loc la 1 tag cu the -> dien luon
      (setq ft (vl-string-trim " \t" (get_tile "ftag")))
      (if (and (= (vl-string-trim " \t" (get_tile "tag")) "")
               (/= ft "")
               (not (mc-has-wild ft)))
        (set_tile "tag" (strcase ft))
      )
      (mc-draw-prev nm)                          ; *** v1.6
    )
  )
  (princ)
)

;; Go tay ten block trong o "blk" -> dong bo danh sach + ve lai xem truoc.
(defun mc-typed-blk (/ cur i idx)
  (setq cur (strcase (vl-string-trim " \t\"" (get_tile "blk")))
        i   0
        idx nil
  )
  (foreach b *mc-blist*
    (if (and (null idx) (= (strcase b) cur)) (setq idx i))
    (setq i (1+ i))
  )
  (if idx (set_tile "blist" (itoa idx)))
  (mc-draw-prev (get_tile "blk"))
  (princ)
)

;; ================= GIAO DIEN =================
(defun mc-preview (/ nm st au s i)
  (setq nm (get_tile "name")
        st (atoi (get_tile "step"))
        au (= (get_tile "m_auto") "1")
  )
  (if (< st 1) (setq st 1))
  (if (not au)
    (set_tile "prev" (strcat "Se dung: " nm " (giu nguyen cho moi lan chen)"))
    (progn
      (setq s nm i 0)
      (while (< i 5)
        (setq nm (mc-next nm st))
        (setq s (strcat s ", " nm))
        (setq i (1+ i))
      )
      (set_tile "prev" (strcat "Se dung: " s ", ..."))
    )
  )
)

(defun mc-makedcl (/ fn f)
  (setq fn (vl-filename-mktemp "mc" nil ".dcl")
        f  (open fn "w")
  )
  (foreach s
    (list
      "mc_dlg : dialog {"
      "  label = \"Chen ky hieu mat cat - MC v1.6\";"
      "  : boxed_column {"
      "    label = \"Chon block co san trong ban ve\";"
      "    : row {"
      "      : column {"
      "        : popup_list { key=\"blist\"; label=\"Danh sach block:\"; width=34; }"
      "        : row {"
      "          : edit_box { key=\"ftag\"; label=\"Loc ATT tag:\"; edit_width=12; }"
      "          : button { key=\"btn_flt\"; label=\"Loc lai\"; width=10; }"
      "        }"
      "        : text { key=\"binfo\"; label=\" \"; width=44; }"
      "        : text { label=\"(De trong = moi block co ATT;  go  *  = tat ca block)\"; width=44; }"
      "      }"
      "      : boxed_column {"
      "        label = \"Xem truoc (view CAD)\";"
      "        : image { key=\"vprev\"; width=34; height=13; color=0; }"
      "        : text { key=\"pinfo\"; label=\" \"; width=34; }"
      "      }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Block va ty le\";"
      "    : row {"
      "      : edit_box { key=\"blk\"; label=\"Ten block / file .dwg:\"; edit_width=28; }"
      "      : button { key=\"btn_file\"; label=\"Tim file .dwg...\"; width=18; }"
      "    }"
      "    : row {"
      "      : edit_box { key=\"tag\"; label=\"Tag ATT (trong = ATT dau tien):\"; edit_width=14; }"
      "      : edit_box { key=\"scale\"; label=\"Ty le chen:\"; edit_width=12; }"
      "    }"
      "    : row {"
      "      : toggle { key=\"rot\"; label=\"Xoay theo huong duong cat\"; }"
      "      : toggle { key=\"dbg\"; label=\"In toa do kiem tra\"; }"
      "    }"
      "    : toggle { key=\"flip\"; label=\"Lat block (dao mui ten sang phia ben kia duong cat)\"; }"
      "  }"
      "  : boxed_column {"
      "    label = \"Ten ky hieu\";"
      "    : row {"
      "      : edit_box { key=\"name\"; label=\"Ten bat dau:\"; edit_width=12; }"
      "      : edit_box { key=\"step\"; label=\"Buoc nhay:\"; edit_width=6; }"
      "    }"
      "    : radio_row {"
      "      : radio_button { key=\"m_auto\"; label=\"Tu dong tang\"; }"
      "      : radio_button { key=\"m_keep\"; label=\"Giu nguyen ten\"; }"
      "    }"
      "    : text { key=\"prev\"; label=\" \"; width=64; }"
      "  }"
      "  : text { label=\"Chen lien tuc: moi lan pick 2 diem. Enter hoac Esc de ket thuc.\"; width=64; }"
      "  spacer;"
      "  ok_cancel;"
      "}"
    )
    (write-line s f)
  )
  (close f)
  fn
)

;; ================= CHEN BLOCK =================
(defun mc-set-att (ent tag val / obj atts done)
  (setq obj (vlax-ename->vla-object ent))
  (setq done nil)
  (if (= (vla-get-HasAttributes obj) :vlax-true)
    (progn
      (setq atts (vlax-invoke obj 'GetAttributes))
      (foreach a atts
        (if (and (not done)
                 (or (= tag "")
                     (= (strcase tag) (strcase (vla-get-TagString a)))))
          (progn
            (vl-catch-all-apply 'vla-put-TextString (list a val))
            (setq done T)
          )
        )
      )
    )
  )
  done
)

(defun mc-insert (blk pt sc ang nm tag / e)
  (command "_.-INSERT" blk "_non" pt sc sc ang)
  (setq e (entlast))
  (if (and e (= (cdr (assoc 0 (entget e))) "INSERT"))
    (progn (mc-set-att e tag nm) e)
    nil
  )
)

;; --- *** v1.4: Lat doi xung block QUA TRUC LA DUONG NOI 2 DIEM PICK ---
;;     pA, pB = 2 diem dat cua cap ky hieu (pt1, pt2) -> truc guong.
;;     Dat MIRRTEXT = 0 tam thoi de chu/ATT khong bi lat nguoc.
;;     Tra ve ename cua block sau khi lat.
(defun mc-flip (e pA pB / oldmt enew)
  (if e
    (progn
      (setq oldmt (getvar "MIRRTEXT"))
      (setvar "MIRRTEXT" 0)
      (vl-catch-all-apply
        'command
        (list "_.MIRROR" e "" "_non" pA "_non" pB "_Y")
      )
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (setvar "MIRRTEXT" oldmt)
      (setq enew (entlast))
      (if (and enew (= (cdr (assoc 0 (entget enew))) "INSERT")) enew e)
    )
    e
  )
)

;; --- In thong tin kiem tra base point ---
(defun mc-check (e pt / obj ip mn mx c d)
  (if e
    (progn
      (setq obj (vlax-ename->vla-object e))
      (setq ip (vlax-safearray->list
                 (vlax-variant-value (vla-get-InsertionPoint obj))))
      (princ (strcat "\n   Diem pick (WCS): "
                     (rtos (car (trans pt 1 0)) 2 3) " , "
                     (rtos (cadr (trans pt 1 0)) 2 3)))
      (princ (strcat "\n   Block chen tai : "
                     (rtos (car ip) 2 3) " , " (rtos (cadr ip) 2 3)))
      (if (not (vl-catch-all-error-p
                 (vl-catch-all-apply 'vla-GetBoundingBox (list obj 'mn 'mx))))
        (progn
          (setq mn (vlax-safearray->list (vlax-variant-value mn))
                mx (vlax-safearray->list (vlax-variant-value mx))
                c  (list (/ (+ (car mn) (car mx)) 2.0)
                         (/ (+ (cadr mn) (cadr mx)) 2.0))
                d  (distance (list (car ip) (cadr ip)) c)
          )
          (princ (strcat "\n   Tam hinh ve    : " (rtos (car c) 2 3) " , " (rtos (cadr c) 2 3)
                         "  -> lech khoi diem chen: " (rtos d 2 3)))
        )
      )
    )
  )
  (princ)
)

;; ================= LENH CHINH =================
(defun c:MC (/ *error* dclfile dclid code doc oldecho oldattdia oldattreq
               res blk file tag sc nm st au rt db fl pt1 pt2 ang1 ang2
               goon cnt undoon e1 e2 tmp)

  (defun *error* (msg)
    (if oldattdia (setvar "ATTDIA" oldattdia))
    (if oldattreq (setvar "ATTREQ" oldattreq))
    (if oldecho   (setvar "CMDECHO" oldecho))
    (if undoon (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*")))
      (princ (strcat "\nLoi: " msg))
    )
    (princ (strcat "\nDa ket thuc MC. Ten tiep theo: " (if *mc-name* *mc-name* "")))
    (princ)
  )

  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *mc-segcache* nil)   ; *** v1.6: xoa bo nho dem xem truoc moi lan chay

  ;; ---------- Hop thoai (lap lai neu bam nut tim file) ----------
  (setq code 2)
  (while (= code 2)
    (setq dclfile (mc-makedcl)
          dclid   (load_dialog dclfile)
    )
    (if (not (new_dialog "mc_dlg" dclid))
      (progn (princ "\nKhong mo duoc hop thoai!") (exit))
    )
    (set_tile "blk"   *mc-blk*)
    (set_tile "tag"   *mc-tag*)
    (set_tile "scale" *mc-scale*)
    (set_tile "name"  *mc-name*)
    (set_tile "step"  *mc-step*)
    (set_tile "rot"   (if *mc-rot* "1" "0"))
    (set_tile "dbg"   (if *mc-dbg* "1" "0"))
    (set_tile "flip"  (if *mc-flip* "1" "0"))   ; *** v1.4
    (set_tile "ftag"  *mc-ftag*)                ; *** v1.5
    (set_tile (if *mc-auto* "m_auto" "m_keep") "1")
    (mc-refresh-list)                           ; *** v1.5: do danh sach block
    (mc-preview)
    (action_tile "blist"    "(mc-pick-blk)")    ; *** v1.5
    (action_tile "ftag"     "(mc-refresh-list)")
    (action_tile "btn_flt"  "(mc-refresh-list)")
    (action_tile "blk"      "(mc-typed-blk)")   ; *** v1.6
    (action_tile "name"   "(mc-preview)")
    (action_tile "step"   "(mc-preview)")
    (action_tile "m_auto" "(mc-preview)")
    (action_tile "m_keep" "(mc-preview)")
    (action_tile "btn_file"
      "(setq *mc-blk* (get_tile \"blk\") *mc-ftag* (get_tile \"ftag\"))(done_dialog 2)"
    )
    (action_tile "accept"
      (strcat "(setq *mc-blk* (get_tile \"blk\") *mc-tag* (get_tile \"tag\")"
              " *mc-scale* (get_tile \"scale\") *mc-name* (get_tile \"name\")"
              " *mc-step* (get_tile \"step\") *mc-rot* (= (get_tile \"rot\") \"1\")"
              " *mc-dbg* (= (get_tile \"dbg\") \"1\")"
              " *mc-auto* (= (get_tile \"m_auto\") \"1\")"
              " *mc-ftag* (get_tile \"ftag\")"
              " *mc-flip* (= (get_tile \"flip\") \"1\"))"
              "(done_dialog 1)"
      )
    )
    (action_tile "cancel" "(done_dialog 0)")
    (setq code (start_dialog))
    (unload_dialog dclid)
    (vl-file-delete dclfile)
    (if (= code 2)
      (progn
        (setq tmp (getfiled "Chon file block (.dwg)" "" "dwg" 8))
        (if tmp (setq *mc-blk* tmp))
      )
    )
  )

  (if (/= code 1)
    (progn (princ "\nDa huy.") (princ))
    (progn
      (setq tag *mc-tag*
            nm  *mc-name*
            sc  (atof *mc-scale*)
            st  (atoi *mc-step*)
            au  *mc-auto*
            rt  *mc-rot*
            db  *mc-dbg*
            fl  *mc-flip*   ; *** v1.4: T = lat ca 2
            res (mc-resolve *mc-blk*)
      )
      (if (< st 1) (setq st 1))
      (cond
        ((null res)
         (alert (strcat "Khong tim thay block \"" *mc-blk* "\".\n\n"
                        "Da tim:\n"
                        " - Dinh nghia block trong ban ve\n"
                        " - File \"" *mc-blk* ".dwg\" tren duong dan support\n\n"
                        "Hay dung nut \"Tim file .dwg...\" de chi dan duong dan file,\n"
                        "hoac them thu muc chua file vao Options > Files > Support File Search Path."))
        )
        ((<= sc 0.0)
         (alert "Ty le chen phai la so > 0."))
        (t
         (setq blk  (car res)
               file (cadr res)
         )
         (setq scale sc)   ; tuong thich cac lisp cu dung bien 'scale'
         (setq oldecho   (getvar "CMDECHO")
               oldattdia (getvar "ATTDIA")
               oldattreq (getvar "ATTREQ")
         )
         (setvar "CMDECHO" 0)
         (setvar "ATTDIA" 0)
         (setvar "ATTREQ" 0)
         (vla-StartUndoMark doc)
         (setq undoon T)
         (if (and file (not (mc-define blk file)))
           (progn
             (vla-EndUndoMark doc)
             (setq undoon nil)
             (setvar "ATTDIA" oldattdia)
             (setvar "ATTREQ" oldattreq)
             (setvar "CMDECHO" oldecho)
             (alert (strcat "Nap block tu file that bai:\n" file))
             (setq blk nil)
           )
           (progn
             (vla-EndUndoMark doc)
             (setq undoon nil)
           )
         )
         (if blk
           (progn
             (setq *mc-blk* blk)   ; lan sau chi can ten block
             (setq goon T cnt 0)
             (while goon
               (setq pt1 (getpoint (strcat "\n[" nm "] Chon diem thu nhat (Enter de ket thuc): ")))
               (if (null pt1)
                 (setq goon nil)
                 (progn
                   (setq pt2 (getpoint pt1 (strcat "\n[" nm "] Chon diem thu hai: ")))
                   (if (null pt2)
                     (setq goon nil)
                     (progn
                       (setq ang1 (/ (* (angle pt1 pt2) 180.0) pi))
                       (if (> ang1 180) (setq ang1 (- ang1 180)))
                       (if (> ang1 0)
                         (setq ang2 (- ang1 90))
                         (setq ang2 (+ ang1 90))
                       )
                       (if (not rt) (setq ang2 0.0))
                       (vla-StartUndoMark doc)
                       (setq undoon T)
                       (setq e1 (mc-insert blk pt1 sc ang2 nm tag))
                       (setq e2 (mc-insert blk pt2 sc ang2 nm tag))
                       ;; *** v1.4: bat "Lat block" -> lat ca 2 qua truc
                       ;; la duong noi 2 diem pick (pt1 - pt2)
                       (if fl
                         (progn
                           (setq e1 (mc-flip e1 pt1 pt2))
                           (setq e2 (mc-flip e2 pt1 pt2))
                         )
                       )
                       (vla-EndUndoMark doc)
                       (setq undoon nil)
                       (if db (progn (mc-check e1 pt1) (mc-check e2 pt2)))
                       (setq cnt (1+ cnt))
                       (princ (strcat "\n  -> Da chen cap ky hieu \"" nm "\"."))
                       (if au (setq nm (mc-next nm st)))
                       (setq *mc-name* nm)
                       (setq #MC nm)
                     )
                   )
                 )
               )
             )
             (setvar "ATTDIA" oldattdia)
             (setvar "ATTREQ" oldattreq)
             (setvar "CMDECHO" oldecho)
             (princ (strcat "\nDa chen " (itoa cnt) " cap ky hieu mat cat. Ten tiep theo: " nm))
           )
         )
        )
      )
      (princ)
    )
  )
  (princ)
)

(princ "\n=== MC v1.6 da nap (danh sach block co ATT MATCAT + o xem truoc view CAD) - Go MC de chay ===")
(princ)

;;; --- HET [24] MC ---

;;; ###########################################################################
;;; ##  NHOM 6 - LAYOUT / VIEWPORT
;;; ###########################################################################

;;; ---------------------------------------------------------------------------
;;; [25] SMV
;;;      Tao Mview theo ty le nhap tu Model
;;;      Nguon: SMV (tao MV).lsp
;;; ---------------------------------------------------------------------------
(defun c:SMV ( / *error* oldOsmd oldCmde pt1_mod pt2_mod width_mod height_mod 
                 scVal width_lay height_lay pt1_lay pt2_lay activeDoc layoutName mviewObj)
  (vl-load-com)
  (setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; 1. Hàm xử lý bẫy lỗi hệ thống
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[Lỗi]: " msg))
    )
    (if oldOsmd (setvar "OSMODE" oldOsmd))
    (if oldCmde (setvar "CMDECHO" oldCmde))
    (vla-EndUndoMark activeDoc)
    (princ "\n[SmartMview] Đã khôi phục cài đặt hệ thống.")
    (princ)
  )

  (vla-StartUndoMark activeDoc)
  (setq oldOsmd (getvar "OSMODE")
        oldCmde (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; 2. Bắt buộc người dùng phải bắt đầu ở Model
  (if (= (getvar "TILEMODE") 0)
    (progn
      (princ "\n[Thông báo]: Vui lòng về không gian MODEL để chọn vùng bản vẽ!")
      (setvar "TILEMODE" 1)
    )
  )

  (princ "\n=============================================")
  (princ "\n   SMART MVIEW - TỰ ĐỘNG TÍNH THEO TỶ LỆ")
  (princ "\n=============================================\n")

  ;; 3. Quét vùng chọn ở Model
  (setvar "OSMODE" 16383) ;; Bật bắt điểm
  (setq pt1_mod (getpoint "\n[Bước 1/3] Click điểm đầu tiên trên MODEL: "))
  
  (if pt1_mod
    (progn
      (setq pt2_mod (getcorner pt1_mod "\n[Bước 2/3] Kéo quét điểm đối diện vùng bản vẽ: "))
      
      (if pt2_mod
        (progn
          ;; Tính kích thước vùng quét tại Model (Đơn vị Model)
          (setq width_mod (abs (- (car pt1_mod) (car pt2_mod)))
                height_mod (abs (- (cadr pt1_mod) (cadr pt2_mod))))
          
          ;; 4. Nhập tỷ lệ bản vẽ trực quan
          (initget 6) ;; Không cho phép nhập số âm hoặc số 0
          (setq scVal (getreal "\n[Bước 3/3] Nhập mẫu số tỷ lệ mong muốn (Ví dụ: 50, 100, 200): "))
          
          (if (null scVal) (setq scVal 100.0)) ;; Mặc định nếu bấm Enter là tỷ lệ 1/100
          
          ;; Tính kích thước Mview thực tế sẽ hiển thị trên Layout giấy (đơn vị mm)
          (setq width_lay (/ width_mod scVal)
                height_lay (/ height_mod scVal))
          
          ;; 5. Chuyển sang Layout hiện hành
          (setq layoutName (getvar "CTAB"))
          (if (= layoutName "Model")
            (setvar "CTAB" (car (layoutlist)))
          )
          (if (= (getvar "CVPORT") 1) (princ) (command "_.pspace"))

          ;; 6. Đặt Mview lên Layout
          (setvar "OSMODE" 0) ;; Tắt bắt điểm để tránh lệch khung trên giấy
          (setq pt1_lay (getpoint (strcat "\n[Hệ thống]: Đã tính toán kích thước Mview (" 
                                          (rtos width_lay 2 1) "x" (rtos height_lay 2 1) 
                                          "mm). \nChọn điểm đặt Mview trên Layout: ")))
          
          (if pt1_lay
            (progn
              (setq pt2_lay (list (+ (car pt1_lay) width_lay) (+ (cadr pt1_lay) height_lay) 0.0))
              
              ;; Tạo Mview bằng lệnh vẽ cơ bản
              (command "_.mview" pt1_lay pt2_lay)
              
              ;; Lấy đối tượng Mview vừa tạo cuối cùng để áp tỷ lệ chính xác
              (setq mviewObj (vlax-ename->vla-object (entlast)))
              
              ;; Nhảy vào mspace để zoom chuẩn tọa độ
              (command "_.mspace")
              (command "_.zoom" "_window" pt1_mod pt2_mod)
              
              ;; Áp đặt tỷ lệ chuẩn (Custom Scale) thông qua ActiveX
              (vla-put-CustomScale mviewObj (/ 1.0 scVal))
              
              ;; Khóa Viewport tự động để bảo vệ tỷ lệ bản vẽ
              (vla-put-DisplayLocked mviewObj :vlax-true)
              
              ;; Trở lại Pspace để người dùng thấy toàn cảnh tờ giấy
              (command "_.pspace")
              
              (princ (strcat "\n[Thành công]: Đã tạo hoàn thành Mview tỷ lệ 1/" (rtos scVal 2 0) " và tự động KHÓA khung!"))
            )
            (princ "\n[Hủy bỏ]: Chưa chọn vị trí đặt trên Layout.")
          )
        )
        (princ "\n[Hủy bỏ]: Chưa quét xong vùng chọn ở Model.")
      )
    )
    (princ "\n[Hủy bỏ]: Chưa chọn điểm đầu tiên.")
  )

  ;; Khôi phục cài đặt gốc
  (*error* nil)
  (princ)
)

(princ "\n[SmartMview] Gõ 'SMV' tại Model để tạo Mview nhập tỷ lệ.\n")
(princ)

;;; --- HET [25] SMV ---

;;; ---------------------------------------------------------------------------
;;; [26] KVP
;;;      Khoa / mo khoa viewport theo tung Layout
;;;      Nguon: KVP_KhoaViewport.lsp
;;; ---------------------------------------------------------------------------
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

;;; --- HET [26] KVP ---

;;; ###########################################################################
;;; ##  NHOM 7 - GHI CHU & THONG KE KHOI LUONG
;;; ###########################################################################

;;; ---------------------------------------------------------------------------
;;; [27] TAG / TAGUPDATE
;;;      Tag ghi chu block, lay Distance1 tu parameter dong hoac ATT
;;;      Nguon: TAG(tag blog).lsp
;;; ---------------------------------------------------------------------------
;;; =========================================================================
;;; TAG.LSP  -  v6.1
;;;   - Duong Leader ngang nam GIUA: NAME o tren gach ngang, L=... o duoi
;;;       MAT CAU1-H400
;;;       --------------   <- duong leader ngang
;;;       L=32m
;;;   - Lua chon don vi mm / m trong hop thoai (m = Length/1000)
;;;   - Duong gach ngang tu keo dai theo do rong chu
;;;   *** (v3): Neu Distance1 de trong (block khong co Distance1
;;;       hoac nguoi dung xoa trang o Distance1 trong hop thoai)
;;;       thi KHONG tao dong "L=..." nua - Tag chi con dong NAME.
;;;       Khi cap nhat (TAGUPDATE/REGEN), neu Distance1 khong con
;;;       gia tri thi dong "L=..." cu cung tu dong bi xoa.
;;;   *** MOI (v4): Ho tro ARRAY lien ket. Pick vao block nam trong
;;;       array -> tu tim block long ben trong tai diem pick (nentselp)
;;;       de lay NAME/Distance1 gan Tag. Diem chi mac dinh (Enter o
;;;       buoc chon diem dau) = diem pick thay vi diem chen block.
;;;       LUU Y: neu sua array (them/bot hang cot) CAD dung lai block
;;;       ben trong -> Tag cu co the mat lien ket, can tao lai Tag.
;;;   *** MOI (v5): Don vi m cho phep cai so chu so sau dau phay
;;;       (o nhap trong hop thoai, mac dinh 3, luu theo session va
;;;       ghi vao XDATA de TAGUPDATE/REGEN format dung nhu luc tao;
;;;       tag cu chua co truong nay tu dong dung 3).
;;;
;;;   *** MOI (v6): Distance1 khong bat buoc phai la Parameter dong nua.
;;;       Neu block KHONG co Dynamic Property ten "Distance1" thi tu
;;;       dong tim THUOC TINH ATT co tag "Distance1"; co gia tri thi
;;;       van cho phep tag binh thuong.
;;;       - ATT la so  -> van doi duoc don vi mm/m va so chu so thap phan
;;;                       ("12500" / "12,5" / "L=12500mm" deu doc duoc)
;;;       - ATT khong phai so -> in y nguyen sau chu "L="
;;;       - Hop thoai hien ro dang lay tu Parameter dong hay tu ATT
;;;       - TAGUPDATE / REGEN cung ap dung quy tac nay (tag cu van chay)
;;;
;;; Lenh:
;;;   TAG        - Tao Tag
;;;   TAGUPDATE  - Cap nhat toan bo Tag (thu cong)
;;;   REGEN      - Tu dong cap nhat tat ca Tag
;;; =========================================================================

(vl-load-com)

(if (null *tag-unit*) (setq *tag-unit* "mm"))
(if (null *tag-mdec*) (setq *tag-mdec* 3))   ; *** v5: so chu so sau dau phay khi don vi m

;; ---------- Doc gia tri Attribute theo Tag name ----------
(defun TAG:GetAttValue (blkObj attTag / atts i res)
  (setq res nil)
  (if (and (vlax-property-available-p blkObj 'HasAttributes)
           (eq (vla-get-HasAttributes blkObj) :vlax-true))
    (progn
      (setq atts (vlax-invoke blkObj 'GetAttributes))
      (setq i 0)
      (while (and (< i (length atts)) (not res))
        (if (= (strcase (vla-get-TagString (nth i atts))) (strcase attTag))
          (setq res (vla-get-TextString (nth i atts)))
        )
        (setq i (1+ i))
      )
    )
  )
  res
)

;; ---------- Doc gia tri Dynamic Block Property theo ten ----------
(defun TAG:GetDynProp (blkObj propName / props i res)
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

;; ---------- Cat so 0 thua: "2.500" -> "2.5" ----------
(defun TAG:TrimZero (s)
  (if (vl-string-search "." s)
    (progn
      (while (= (substr s (strlen s) 1) "0")
        (setq s (substr s 1 (1- (strlen s))))
      )
      (if (= (substr s (strlen s) 1) ".")
        (setq s (substr s 1 (1- (strlen s))))
      )
      s
    )
    s
  )
)

;; ---------- Format so theo don vi ----------
;; *** v5.1: rtos bi bien he thong DIMZIN chi phoi (DIMZIN=8 mac dinh
;;     ban ve metric se cat so 0 thua: (rtos 12.0 2 3) -> "12").
;;     Ep tam DIMZIN=0 quanh luc format de luon hien du so chu so.
(defun TAG:Rtos (val prec / oldz r)
  (setq oldz (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq r (vl-catch-all-apply 'rtos (list val 2 prec)))
  (setvar "DIMZIN" oldz)
  (if (vl-catch-all-error-p r) (rtos val 2 prec) r)
)

;; dec: so chu so sau dau phay khi don vi m (nil -> 3, kep trong 0..8)
;; *** v5.1: khong cat so 0 thua nua -> luon hien du so chu so da cai
;;     (12 voi dec=3 -> "12.000")
(defun TAG:FormatNum (val unit dec)
  (setq dec (if (numberp dec) (min 8 (max 0 (fix dec))) 3))
  (if (numberp val)
    (if (= unit "m")
      (TAG:Rtos (/ val 1000.0) dec)
      (TAG:Rtos val 0)
    )
    (vl-princ-to-string val)
  )
)

;; ---------- Kiem tra chuoi trong/toan khoang trang ----------
(defun TAG:BlankP (s)
  (or (not s) (= (vl-string-trim " " s) ""))
)

;; =========================================================================
;; *** v6: Distance1 co the la THUOC TINH ATT chu khong chi Parameter dong
;; =========================================================================

;; Xoa mot ky tu khoi chuoi
(defun TAG:StrDel (s ch / r i c)
  (setq r "" i 1)
  (repeat (strlen s)
    (setq c (substr s i 1))
    (if (/= c ch) (setq r (strcat r c)))
    (setq i (1+ i))
  )
  r
)

(defun TAG:DigitP (c)
  (member c '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "." "-" "+"))
)

;; Doi chuoi ATT thanh so (nil neu khong phai so)
;;   - Chap nhan dau phay thap phan kieu VN : "12,5"  -> 12.5
;;   - Bo dau phay ngan cach nghin          : "1,234.5" -> 1234.5
;;   - Bo phan chu o dau/cuoi               : "L=12500mm" -> 12500
(defun TAG:Str2Num (s)
  (cond
    ((numberp s) s)
    ((/= (type s) 'STR) nil)
    (t
     (setq s (vl-string-trim " \t" s))
     (if (and (vl-string-search "," s) (vl-string-search "." s))
       (setq s (TAG:StrDel s ","))
       (if (vl-string-search "," s) (setq s (vl-string-translate "," "." s)))
     )
     ;; bo ky tu khong phai so o CUOI chuoi (don vi mm, m, cm...)
     (while (and (> (strlen s) 0)
                 (not (TAG:DigitP (substr s (strlen s) 1))))
       (setq s (substr s 1 (1- (strlen s))))
     )
     ;; bo ky tu khong phai so o DAU chuoi (tien to L=, D=...)
     (while (and (> (strlen s) 0)
                 (not (TAG:DigitP (substr s 1 1))))
       (setq s (substr s 2))
     )
     (if (= s "") nil (distof s 2))
    )
  )
)

;; Lay gia tri Distance1 tu 2 nguon, tra ve (gia-tri nguon):
;;   1. Parameter dong (Linear / Dynamic Block Property)   -> nguon "DYN"
;;   2. Neu khong co  -> Thuoc tinh ATT cung ten           -> nguon "ATT"
;; ATT co so   -> tra ve so   (van doi duoc don vi mm/m)
;; ATT khong so-> tra ve chuoi nguyen ban (in y nguyen sau chu L=)
(defun TAG:GetLenRaw (blkObj propName / v n)
  (setq v (vl-catch-all-apply 'TAG:GetDynProp (list blkObj propName)))
  (if (vl-catch-all-error-p v) (setq v nil))
  (if v
    (list v "DYN")
    (progn
      (setq v (vl-catch-all-apply 'TAG:GetAttValue (list blkObj propName)))
      (if (vl-catch-all-error-p v) (setq v nil))
      (if (TAG:BlankP v)
        nil
        (progn
          (setq n (TAG:Str2Num v))
          (list (if n n (vl-string-trim " " v)) "ATT")
        )
      )
    )
  )
)

;; ---------- *** v4: Tim block long ben trong array tai diem pick ----------
;; Khi entsel tra ve INSERT vo cua array (block an danh *U, khong co att
;; NAME), dung nentselp tai diem pick de lay chuoi block cha long nhau,
;; quet tu trong ra ngoai, tra ve ename cua block dau tien co att NAME.
(defun TAG:FindNestedBlock (pt / r cont e res chk)
  (setq res nil)
  (if pt
    (progn
      (setq r (vl-catch-all-apply 'nentselp (list pt)))
      (if (and (not (vl-catch-all-error-p r)) r)
        (progn
          (setq cont '())
          ;; ban than doi tuong trong cung co the la INSERT long
          (if (and (car r)
                   (= (cdr (assoc 0 (entget (car r)))) "INSERT"))
            (setq cont (list (car r)))
          )
          ;; danh sach cha (phan tu thu 4), thu tu trong -> ngoai
          (if (= (length r) 4)
            (setq cont (append cont (nth 3 r)))
          )
          (foreach e cont
            (if (and (not res)
                     (= (cdr (assoc 0 (entget e))) "INSERT"))
              (progn
                (setq chk (vl-catch-all-apply
                            '(lambda (x)
                               (TAG:GetAttValue (vlax-ename->vla-object x) "NAME"))
                            (list e)))
                (if (and (not (vl-catch-all-error-p chk)) chk)
                  (setq res e)
                )
              )
            )
          )
        )
      )
    )
  )
  res
)

;; ---------- Tim INSERT gan mot diem ----------
;; Dung khi block cu bi thay bang block moi, handle cu khong con hop le.
(defun TAG:FindBlockNearPoint (pt tol / ss i ent obj ip best bestd d nameVal)
  (setq best nil
        bestd nil
  )
  (if (and pt tol (> tol 0.0))
    (progn
      (setq ss (ssget "_X" '((0 . "INSERT"))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (setq i (1+ i))
            (setq obj (vlax-ename->vla-object ent))
            (setq nameVal nil)
            (setq ip (vl-catch-all-apply 'vlax-get (list obj 'InsertionPoint)))
            (if (not (vl-catch-all-error-p ip))
              (progn
                (setq d (distance pt ip))
                ;; uu tien block co ATT NAME va gan diem nhat
                (if (and (<= d tol)
                         (setq nameVal (TAG:GetAttValue obj "NAME"))
                         (or (null bestd) (< d bestd)))
                  (setq best ent
                        bestd d)
                )
              )
            )
          )
        )
      )
    )
  )
  best
)

;; ---------- Thu hoi block theo handle cu / leader / diem chen ----------
(defun TAG:ResolveBlock (blkHandle ldrHandle / blkEnt ldrEnt pts anchor newEnt)
  (setq blkEnt (handent blkHandle))
  (if (and blkEnt (= (cdr (assoc 0 (entget blkEnt))) "INSERT"))
    blkEnt
    (progn
      (setq ldrEnt (handent ldrHandle))
      (if ldrEnt
        (progn
          (setq pts (TAG:LeaderPts ldrEnt))
          ;; diem dau cua leader la diem block / diem pick ban dau
          (setq anchor (car pts))
          ;; dung do le nho de bat block moi tai cung vi tri
          (setq newEnt (TAG:FindBlockNearPoint anchor 1e-3))
        )
      )
      newEnt
    )
  )
)

;; ---------- Chieu cao chu ----------
(defun TAG:TextH ( / h)
  (setq h (* (getvar "DIMTXT")
             (if (> (getvar "DIMSCALE") 0) (getvar "DIMSCALE") 1.0)))
  (if (<= h 0) (setq h 2.5))
  h
)

;; ---------- Tao MTEXT bang entmake, tra ve ename ----------
;; att: 1=TopLeft 3=TopRight 7=BottomLeft 9=BottomRight
(defun TAG:MakeMText (pt h att str)
  (entmakex
    (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
          (cons 10 pt) (cons 40 h) (cons 71 att) (cons 1 str))
  )
)

;; ---------- Do rong bounding box cua 1 doi tuong ----------
(defun TAG:BoxWidth (en / obj mn mx)
  (setq obj (vlax-ename->vla-object en))
  (vla-getboundingbox obj 'mn 'mx)
  (- (car (vlax-safearray->list mx)) (car (vlax-safearray->list mn)))
)

;; ---------- Chieu rong lon nhat cua 2 dong (dong 2 co the khong ton tai) ----------
(defun TAG:MaxW (e1 e2)
  (if e2
    (max (TAG:BoxWidth e1) (TAG:BoxWidth e2))
    (TAG:BoxWidth e1)
  )
)

;; ---------- Doi diem cuoi cua LEADER ----------
(defun TAG:SetLeaderEnd (lent newpt / ed total n res x)
  (setq ed (entget lent) total 0 n 0 res nil)
  (foreach x ed (if (= (car x) 10) (setq total (1+ total))))
  (foreach x ed
    (if (= (car x) 10)
      (progn
        (setq n (1+ n))
        (if (= n total)
          (setq res (cons (cons 10 newpt) res))
          (setq res (cons x res))
        )
      )
      (setq res (cons x res))
    )
  )
  (entmod (reverse res))
)

;; ---------- Lay danh sach dinh cua LEADER ----------
(defun TAG:LeaderPts (lent / res x)
  (setq res nil)
  (foreach x (entget lent)
    (if (= (car x) 10) (setq res (cons (cdr x) res)))
  )
  (reverse res)
)

;; ---------- Ghi / ghi de XDATA TAGLINK2 vao MTEXT NAME ----------
(defun TAG:PutXData (nameEnt blkHandle unit lenHandle ldrHandle dirStr mdec / ed)
  (setq ed (entget nameEnt '("TAGLINK2")))
  ;; bo xdata cu neu co
  (if (assoc -3 ed) (setq ed (vl-remove (assoc -3 ed) ed)))
  (entmod
    (append ed
      (list (list -3
                  (list "TAGLINK2"
                        (cons 1000 blkHandle)     ; handle block
                        (cons 1000 "NAME")        ; ten ATT
                        (cons 1000 "Distance1")   ; ten dyn prop
                        (cons 1000 unit)          ; don vi
                        (cons 1000 lenHandle)     ; handle mtext L= ("NONE" neu khong co)
                        (cons 1000 ldrHandle)     ; handle leader
                        (cons 1000 dirStr)        ; huong
                        (cons 1000 (itoa (if (numberp mdec) mdec 3))) ; *** v5: so chu so thap phan (m)
                  )))
    )
  )
)

;; ---------- DCL ----------
(defun TAG:WriteDCL ( / f fname)
  (setq fname (strcat (getenv "TEMP") "\\tag_dialog.dcl"))
  (setq f (open fname "w"))
  (write-line "tag_dialog : dialog {" f)
  (write-line "  label = \"TAG - Xem truoc noi dung  -  v6.1\";" f)
  (write-line "  : edit_box { key = \"txtname\"; label = \"NAME :\"; edit_width = 30; }" f)
  (write-line "  : edit_box { key = \"txtdist\"; label = \"Distance1 :\"; edit_width = 30; }" f)
  (write-line "  : text { key = \"srcnote\"; width = 50; value = \"\"; }" f)
  (write-line "  : text { key = \"note\"; label = \"(De trong Distance1 = khong tao dong L=...)\"; }" f)
  (write-line "  : boxed_radio_row {" f)
  (write-line "    label = \"Don vi chieu dai\";" f)
  (write-line "    : radio_button { key = \"unit_mm\"; label = \"mm (giu nguyen gia tri)\"; }" f)
  (write-line "    : radio_button { key = \"unit_m\";  label = \"m (gia tri / 1000)\"; }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : text { label = \"So chu so sau dau phay (don vi m) :\"; }" f)
  (write-line "    : edit_box { key = \"mdec\"; edit_width = 4; }" f)
  (write-line "    : button { key = \"mdec_dn\"; label = \"-\"; width = 4; fixed_width = true; }" f)
  (write-line "    : button { key = \"mdec_up\"; label = \"+\"; width = 4; fixed_width = true; }" f)
  (write-line "    : text { label = \"(0 - 8)\"; }" f)
  (write-line "  }" f)
  (write-line "  spacer;" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  fname
)

;; ---------- LENH CHINH: TAG ----------
(defun c:TAG (/ sel ent pkpt nestedEnt obj insPt nameVal distRaw distVal unit mdec dclF dclId res
                lenInfo distSrc
                p1 p2 pn pts corner prev dir h gap hasLen
                nameEnt lenEnt maxw endPt cmdArgs leaderEnt)

  (if (not (tblsearch "APPID" "TAGLINK2")) (regapp "TAGLINK2"))

  (setq sel (entsel "\nChon Block can gan Tag (co ATT NAME, Distance1 la Parameter dong hoac ATT): "))
  (setq ent (car sel) pkpt (cadr sel))
  (cond
    ((null ent) (princ "\nBan chua chon doi tuong nao.") (exit))
    ((/= (cdr (assoc 0 (entget ent))) "INSERT")
     (princ "\nDoi tuong ban chon khong phai la Block (INSERT).") (exit))
  )

  (setq obj   (vlax-ename->vla-object ent))
  (setq insPt (vlax-get obj 'InsertionPoint))

  (setq nameVal (TAG:GetAttValue obj "NAME"))

  ;; *** v4: Khong doc duoc NAME -> co the dang pick vao array.
  ;; Tim block long ben trong tai diem pick.
  (if (not nameVal)
    (progn
      (setq nestedEnt (TAG:FindNestedBlock pkpt))
      (if nestedEnt
        (progn
          (setq ent nestedEnt)
          (setq obj (vlax-ename->vla-object ent))
          (setq nameVal (TAG:GetAttValue obj "NAME"))
          ;; InsertionPoint cua block long nam trong he toa do dinh nghia
          ;; block array -> khong dung lam diem chi mac dinh duoc.
          ;; Dung luon diem pick lam diem chi mac dinh.
          (setq insPt pkpt)
          (princ "\n(Da lay block nam trong array de gan Tag.)")
        )
      )
    )
  )

  ;; *** v6: Distance1 lay tu Parameter dong, neu khong co thi lay tu ATT
  (setq lenInfo (TAG:GetLenRaw obj "Distance1"))
  (setq distRaw (car lenInfo))
  (setq distSrc (cadr lenInfo))
  (if (not nameVal) (setq nameVal ""))
  (setq unit *tag-unit*)
  (setq mdec *tag-mdec*)   ; *** v5: so chu so thap phan (don vi m)
  (setq distVal (if distRaw (TAG:FormatNum distRaw unit mdec) ""))

  ;; ----- Hop thoai -----
  (setq dclF (TAG:WriteDCL))
  (setq dclId (load_dialog dclF))
  (if (not (new_dialog "tag_dialog" dclId))
    (progn (princ "\nKhong the mo hop thoai TAG.") (exit))
  )
  (set_tile "txtname" nameVal)
  (set_tile "txtdist" distVal)
  (set_tile "srcnote"
    (cond
      ((equal distSrc "DYN") "Nguon Distance1: Parameter dong (Linear) cua block.")
      ((equal distSrc "ATT") "Nguon Distance1: Thuoc tinh ATT cua block.")
      (t "Khong tim thay Distance1 (ca Parameter dong lan ATT).")
    )
  )
  (set_tile "mdec" (itoa mdec))
  (if (= unit "m") (set_tile "unit_m" "1") (set_tile "unit_mm" "1"))
  (action_tile "unit_mm"
    (vl-prin1-to-string
      '(progn (setq unit "mm")
              (if distRaw (set_tile "txtdist" (TAG:FormatNum distRaw unit mdec))))))
  (action_tile "unit_m"
    (vl-prin1-to-string
      '(progn (setq unit "m")
              (if distRaw (set_tile "txtdist" (TAG:FormatNum distRaw unit mdec))))))
  ;; *** v5: doi so chu so -> cap nhat preview ngay (chi anh huong don vi m)
  (action_tile "mdec"
    (vl-prin1-to-string
      '(progn (setq mdec (atoi (get_tile "mdec")))
              (if (or (< mdec 0) (> mdec 8)) (setq mdec 3))
              (set_tile "mdec" (itoa mdec))
              (if distRaw (set_tile "txtdist" (TAG:FormatNum distRaw unit mdec))))))
  ;; *** v6.1: nut giam / tang so chu so thap phan
  (action_tile "mdec_dn"
    (vl-prin1-to-string
      '(progn (setq mdec (atoi (get_tile "mdec")))
              (if (or (< mdec 0) (> mdec 8)) (setq mdec 3))
              (setq mdec (max 0 (1- mdec)))
              (set_tile "mdec" (itoa mdec))
              (if distRaw (set_tile "txtdist" (TAG:FormatNum distRaw unit mdec))))))
  (action_tile "mdec_up"
    (vl-prin1-to-string
      '(progn (setq mdec (atoi (get_tile "mdec")))
              (if (or (< mdec 0) (> mdec 8)) (setq mdec 3))
              (setq mdec (min 8 (1+ mdec)))
              (set_tile "mdec" (itoa mdec))
              (if distRaw (set_tile "txtdist" (TAG:FormatNum distRaw unit mdec))))))
  (action_tile "accept"
    "(setq nameVal (get_tile \"txtname\") distVal (get_tile \"txtdist\") mdec (atoi (get_tile \"mdec\")) res 1)(done_dialog)")
  (action_tile "cancel" "(setq res nil)(done_dialog)")
  (start_dialog)
  (unload_dialog dclId)
  (vl-file-delete dclF)
  (if (not res) (progn (princ "\nDa huy lenh TAG.") (exit)))
  (if (or (< mdec 0) (> mdec 8)) (setq mdec 3))
  (setq *tag-unit* unit)
  (setq *tag-mdec* mdec)

  ;; *** v3: Distance1 trong -> khong tao dong L= ***
  (setq hasLen (not (TAG:BlankP distVal)))
  (if hasLen (setq distVal (vl-string-trim " " distVal)))

  ;; ----- Pick cac diem Leader -----
  (setq p1 (getpoint "\nChon diem dau Leader - diem chi vao Block (Enter = diem chen Block): "))
  (if (not p1) (setq p1 insPt))
  (setq p2 (getpoint p1 "\nChon vi tri dat Tag (goc bat dau doan gach ngang): "))
  (if (not p2) (progn (princ "\nDa huy lenh TAG.") (exit)))
  (setq pts (list p1 p2))
  (while (setq pn (getpoint (last pts) "\nChon diem tiep theo (Enter de ket thuc): "))
    (setq pts (append pts (list pn)))
  )

  ;; Diem goc = diem pick cuoi; huong gach ngang theo vi tri diem truoc do
  (setq corner (last pts))
  (setq prev (nth (- (length pts) 2) pts))
  (setq dir (if (>= (car corner) (car prev)) 1.0 -1.0))

  (setq h (TAG:TextH))
  (setq gap (* 0.35 h))

  ;; ----- Tao dong chu: NAME tren duong; L=... duoi duong (neu co) -----
  (if (> dir 0)
    (progn ; chu keo sang phai
      (setq nameEnt (TAG:MakeMText (list (+ (car corner) gap) (+ (cadr corner) gap) 0.0) h 7 nameVal))
      (setq lenEnt
        (if hasLen
          (TAG:MakeMText (list (+ (car corner) gap) (- (cadr corner) gap) 0.0) h 1
                         (strcat "L=" distVal unit))
          nil
        )
      )
    )
    (progn ; chu keo sang trai
      (setq nameEnt (TAG:MakeMText (list (- (car corner) gap) (+ (cadr corner) gap) 0.0) h 9 nameVal))
      (setq lenEnt
        (if hasLen
          (TAG:MakeMText (list (- (car corner) gap) (- (cadr corner) gap) 0.0) h 3
                         (strcat "L=" distVal unit))
          nil
        )
      )
    )
  )

  ;; ----- Do dai doan gach ngang = chieu rong chu lon nhat + le -----
  (setq maxw (+ (TAG:MaxW nameEnt lenEnt) (* 2 gap)))
  (setq endPt (list (+ (car corner) (* dir maxw)) (cadr corner) 0.0))

  ;; ----- Ve Leader (khong annotation) qua cac diem + doan ngang -----
  (setq cmdArgs (list "_.LEADER"))
  (foreach p (append pts (list endPt))
    (setq cmdArgs (append cmdArgs (list "_none" p)))
  )
  (setq cmdArgs (append cmdArgs (list "" "" "_N")))
  (apply (function command) cmdArgs)
  (setq leaderEnt (entlast))

  ;; ----- Ghi XDATA vao MTEXT NAME de tu cap nhat -----
  (TAG:PutXData
    nameEnt
    (cdr (assoc 5 (entget ent)))
    unit
    (if lenEnt (cdr (assoc 5 (entget lenEnt))) "NONE")   ; *** v3 ***
    (cdr (assoc 5 (entget leaderEnt)))
    (if (> dir 0) "R" "L")
    mdec                                                  ; *** v5 ***
  )

  (princ
    (if hasLen
      "\nDa tao Tag (NAME + L=). Go REGEN de tu cap nhat khi Block thay doi."
      "\nDa tao Tag (chi NAME, khong co dong L= vi Distance1 trong)."
    )
  )
  (princ)
)

;; ---------- Cap nhat toan bo Tag ----------
(defun TAG:UpdateAll ( / ss i ent xd lst blkHandle attTagName propName unit
                          lenHandle ldrHandle dirStr dir mdec blkEnt blkObj
                          nameVal distRaw lenEnt ldrEnt
                          vlist corner maxw gap h endPt cnt)
  (setq ss (ssget "_X" '((0 . "MTEXT") (-3 ("TAGLINK2")))))
  (setq cnt 0)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq xd (cdr (assoc -3 (entget ent '("TAGLINK2")))))
        (setq lst (cdr (car xd)))
        (setq blkHandle  (cdr (nth 0 lst))
              attTagName (cdr (nth 1 lst))
              propName   (cdr (nth 2 lst))
              unit       (cdr (nth 3 lst))
              lenHandle  (cdr (nth 4 lst))
              ldrHandle  (cdr (nth 5 lst))
              dirStr     (cdr (nth 6 lst))
        )
        ;; *** v5: truong 8 = so chu so thap phan; tag cu chua co -> 3
        (setq mdec (if (nth 7 lst) (atoi (cdr (nth 7 lst))) 3))
        (if (or (< mdec 0) (> mdec 8)) (setq mdec 3))
        (setq dir (if (= dirStr "L") -1.0 1.0))
        (setq blkEnt (TAG:ResolveBlock blkHandle ldrHandle)
              lenEnt (if (= lenHandle "NONE") nil (handent lenHandle))  ; *** v3 ***
              ldrEnt (handent ldrHandle)
        )
        (if blkEnt
          (progn
            (setq blkObj (vlax-ename->vla-object blkEnt))
            (setq nameVal (TAG:GetAttValue blkObj attTagName))
            ;; *** v6: doc Distance1 tu Parameter dong, khong co thi tu ATT
            (setq distRaw (car (TAG:GetLenRaw blkObj propName)))
            (if nameVal
              (progn
                ;; Neu block da duoc thay va tim lai duoc block moi,
                ;; cap nhat lai handle trong XDATA de lan sau bam vao dung block.
                (if (/= blkHandle (cdr (assoc 5 (entget blkEnt))))
                  (setq blkHandle (cdr (assoc 5 (entget blkEnt))))
                )

                ;; --- Cap nhat dong NAME ---
                (vla-put-TextString (vlax-ename->vla-object ent) nameVal)

                ;; --- Cap nhat dong L= ---
                (cond
                  ;; Co dong L= va Distance1 van co gia tri -> cap nhat binh thuong
                  ((and lenEnt distRaw)
                   (vla-put-TextString (vlax-ename->vla-object lenEnt)
                                       (strcat "L=" (TAG:FormatNum distRaw unit mdec) unit))
                  )
                  ;; *** v3: Co dong L= nhung Distance1 khong con -> XOA dong L= ***
                  ((and lenEnt (not distRaw))
                   (entdel lenEnt)
                   (setq lenEnt nil)
                   (TAG:PutXData ent blkHandle unit "NONE" ldrHandle dirStr mdec)
                  )
                  ;; Khong co dong L= tu dau -> khong lam gi
                )

                ;; --- Keo dai / thu ngan doan gach ngang theo chu ---
                (if ldrEnt
                  (progn
                    (setq vlist (TAG:LeaderPts ldrEnt))
                    (if (>= (length vlist) 2)
                      (progn
                        (setq corner (nth (- (length vlist) 2) vlist))
                        (setq h (cdr (assoc 40 (entget ent))))
                        (setq gap (* 0.35 h))
                        (setq maxw (+ (TAG:MaxW ent lenEnt) (* 2 gap)))
                        (setq endPt (list (+ (car corner) (* dir maxw))
                                          (cadr corner)
                                          (caddr corner)))
                        (TAG:SetLeaderEnd ldrEnt endPt)
                      )
                    )
                  )
                )

                ;; Luu lai XDATA voi handle block hien tai, de TAGUPDATE
                ;; lan sau van bam dung block moi sau khi THAYBLOCK.
                (TAG:PutXData
                  ent
                  (cdr (assoc 5 (entget blkEnt)))
                  unit
                  (if lenEnt (cdr (assoc 5 (entget lenEnt))) "NONE")
                  (if ldrEnt (cdr (assoc 5 (entget ldrEnt))) "NONE")
                  dirStr
                  mdec
                )

                (setq cnt (1+ cnt))
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nDa cap nhat " (itoa cnt) " Tag."))
    )
    (princ "\nKhong tim thay Tag nao trong ban ve.")
  )
  (princ)
)

(defun c:TAGUPDATE () (TAG:UpdateAll))

;; ---------- Reactor: tu dong cap nhat khi REGEN / THAYBLOCK ----------
(if (not *TAG:CmdReactor*)
  (setq *TAG:CmdReactor*
    (vlr-command-reactor "TAGREACTOR"
      '((:vlr-commandEnded . TAG:OnCommandEnded))
    )
  )
)

(defun TAG:OnCommandEnded (calling-reactor cmd-list / cmdName)
  (setq cmdName (strcase (car cmd-list)))
  (if (or (wcmatch cmdName "*REGEN*")
          (wcmatch cmdName "*THAYBLOCK*")
          (wcmatch cmdName "*TBL*"))
    (TAG:UpdateAll)
  )
)

(princ "\n=== TAG.LSP v6.2 da nap: TAG se tu cap nhat sau THAYBLOCK/TBL hoac REGEN; Distance1 tu Parameter dong HOAC ATT. ===")
(princ)

;;; --- HET [27] TAG / TAGUPDATE ---

;;; ---------------------------------------------------------------------------
;;; [28] THKL
;;;      Tong hop / thong ke khoi luong theo block, xuat CSV
;;;      Nguon: THKL(tong_hop_KL).lsp
;;; ---------------------------------------------------------------------------
;;; =====================================================================
;;; TKBD.LSP  -  Thong Ke Block Dong (NAME, KLDV, Distance1, So Luong, KL)
;;; Version : 1.7.1  (2026-07-25)
;;;
;;; v1.7.1: FIX loi "no function definition: TKBD:PARSEKLDV" - ham parse
;;;         KLDV bi go nham thanh THKL:ParseKLDV (sot lai luc doi ten
;;;         lenh TKBD -> THKL), trong khi noi goi van la TKBD:ParseKLDV.
;;;         Da doi lai dung TKBD:ParseKLDV cho khop toan bo file.
;;; v1.7: Cot Tong CD quy doi ra don vi m (chia 1000 khi toggle mm->m bat).
;;;       CD 1 cau kien van giu don vi ve goc (mm).
;;; v1.6: Them cot "CD 1 cau kien" truoc cot "Tong chieu dai":
;;;       Tong CD = CD 1CK x So luong. Dong TONG CONG cong ca Tong CD.
;;; v1.5: Ho tro associative array cua CAD: INSERT khong co att NAME se
;;;       duoc duyet vao dinh nghia block (de quy 3 cap) de tim cac
;;;       block dong long ben trong (array = block an danh *U chua
;;;       cac ban sao). Highlight ap len ca cum array.
;;; v1.4: Block khong co Distance1 nhung co KLDV van duoc thong ke:
;;;       KL = KLDV x So Luong (cau kien dem cai, KLDV = kg/cai).
;;;       Cot Chieu dai hien "---". Gop nhom theo NAME + KLDV.
;;; v1.3: Doi thu tu cot: Ten cau kien - So luong - Chieu dai - KLDV - Tong KL
;;; v1.2: Them cot TONG KL = Distance1 x KLDV x So Luong
;;;       - Toggle "mm -> m": chia Distance1 cho 1000 khi tinh KL
;;;         (Distance1 ve bang mm, KLDV la kg/m). Luu theo session.
;;;       - KLDV parse tu attribute text, chap nhan dau phay VN ("21,7")
;;;       - KLDV khong hop le -> KL hien "---" va co dong canh bao
;;; Lenh goi : TKBD
;;;
;;; Chuc nang:
;;;   - Chon cac block dong (INSERT) trong ban ve
;;;   - Doc attribute "NAME", attribute "KLDV" va dynamic parameter "Distance1"
;;;   - Gop nhom theo (NAME + Distance1): block nao cung ten, cung chieu dai
;;;     thi gop vao 1 dong va cong don So Luong. KLDV chi hien thi (lay gia
;;;     tri dau tien trong nhom, khong cong don).
;;;   - Hien thi bang thong ke trong hop thoai DCL (fixed width font)
;;;   - Click 1 dong trong danh sach -> highlight cac doi tuong tuong ung
;;;   - Nut "Chon lai doi tuong" de chon lai selection set
;;;   - Nut "Xuat CSV..." de xuat file CSV
;;;
;;; Ghi chu: khong dung dau tieng Viet trong chuoi string de tranh loi ANSI.
;;; =====================================================================

(vl-load-com)

;; ---------------------------------------------------------------------
;; Bien toan cuc phien lam viec
;; ---------------------------------------------------------------------
(if (not *TKBD-CurHL*)   (setq *TKBD-CurHL* nil))
(if (not *TKBD-Groups*)  (setq *TKBD-Groups* nil))
(if (not *TKBD-LastCSV*) (setq *TKBD-LastCSV* nil))
(if (not *TKBD-MM2M*)    (setq *TKBD-MM2M* "1"))   ; "1" = doi Distance1 mm->m khi tinh KL

;; ---------------------------------------------------------------------
;; Parse KLDV (attribute text) sang so thuc. Tra ve nil neu khong hop le.
;; Chap nhan dau phay thap phan kieu VN (vd "21,7" -> 21.7).
;; ---------------------------------------------------------------------
(defun TKBD:ParseKLDV (s / s2)
  (if (and s (/= s ""))
    (progn
      (setq s2 (vl-string-translate "," "." s))
      (if (distof s2 2) (distof s2 2) nil)
    )
    nil
  )
)

;; ---------------------------------------------------------------------
;; Tinh KL 1 nhom:
;;   - Co Distance1  : KL = Distance1(quy doi mm->m neu bat) x KLDV x SL
;;   - Khong Distance1: KL = KLDV x SL   (cau kien dem cai, KLDV = kg/cai)
;; Tra ve nil neu KLDV khong parse duoc.
;; ---------------------------------------------------------------------
(defun TKBD:CalcKL (dist kldvStr cnt / kldvNum d)
  (setq kldvNum (TKBD:ParseKLDV kldvStr))
  (if kldvNum
    (if dist
      (progn
        (setq d (if (= *TKBD-MM2M* "1") (/ dist 1000.0) dist))
        (* d kldvNum cnt)
      )
      (* kldvNum cnt)
    )
    nil
  )
)

;; ---------------------------------------------------------------------
;; Ham tien ich: dem khoang trang, can le chuoi
;; ---------------------------------------------------------------------
(defun TKBD:Spaces (n / s)
  (setq s "")
  (repeat (max n 0) (setq s (strcat s " ")))
  s
)

(defun TKBD:Pad (str width / s)
  ;; can trai, cat bot neu qua dai
  (setq s (if str (vl-princ-to-string str) ""))
  (if (> (strlen s) width)
    (substr s 1 width)
    (strcat s (TKBD:Spaces (- width (strlen s))))
  )
)

(defun TKBD:PadNum (str width / s)
  ;; can phai, dung cho so
  (setq s (if str (vl-princ-to-string str) ""))
  (if (> (strlen s) width)
    (substr s 1 width)
    (strcat (TKBD:Spaces (- width (strlen s))) s)
  )
)

(defun TKBD:FormatRow (name kldv dist totdist cnt kl / distStr totStr klStr)
  (setq distStr
    (cond
      ((numberp dist) (rtos dist 2 2))
      ((not dist) "---")
      (t (vl-princ-to-string dist))
    )
  )
  (setq totStr
    (cond
      ((numberp totdist) (rtos totdist 2 2))
      ((not totdist) "---")
      (t (vl-princ-to-string totdist))
    )
  )
  (setq klStr
    (cond
      ((numberp kl) (rtos kl 2 2))
      (t (vl-princ-to-string kl))
    )
  )
  ;; Thu tu cot: Ten - So luong - CD 1 cau kien - Tong CD - KLDV - Tong KL
  (strcat
    (TKBD:Pad name 18) " "
    (TKBD:PadNum (vl-princ-to-string cnt) 6) " "
    (TKBD:PadNum distStr 11) " "
    (TKBD:PadNum totStr 12) " "
    (TKBD:PadNum kldv 8) " "
    (TKBD:PadNum klStr 12)
  )
)

;; ---------------------------------------------------------------------
;; Doc attribute theo tag, an toan voi vl-catch-all-apply
;; ---------------------------------------------------------------------
(defun TKBD:GetAttValue (obj tag / chk atts a res)
  (setq res nil)
  (setq chk (vl-catch-all-apply 'vlax-get (list obj 'HasAttributes)))
  (if (and (not (vl-catch-all-error-p chk)) chk)
    (progn
      (setq atts (vl-catch-all-apply 'vlax-invoke (list obj 'GetAttributes)))
      (if (not (vl-catch-all-error-p atts))
        (foreach a atts
          (if (and (not res)
                   (= (strcase (vlax-get a 'TagString)) (strcase tag)))
            (setq res (vlax-get a 'TextString))
          )
        )
      )
    )
  )
  res
)

;; ---------------------------------------------------------------------
;; Doc dynamic property theo ten (vd: "Distance1")
;; ---------------------------------------------------------------------
(defun TKBD:GetDynProp (obj pname / chk props p res)
  (setq res nil)
  (setq chk (vl-catch-all-apply 'vlax-get (list obj 'IsDynamicBlock)))
  (if (and (not (vl-catch-all-error-p chk)) chk)
    (progn
      (setq props (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
      (if (not (vl-catch-all-error-p props))
        (foreach p props
          (if (and (not res)
                   (= (strcase (vlax-get p 'PropertyName)) (strcase pname)))
            (setq res (vlax-get p 'Value))
          )
        )
      )
    )
  )
  res
)

;; ---------------------------------------------------------------------
;; Xu ly 1 doi tuong INSERT (co the la array / block long nhau):
;;   - Neu doc truc tiep duoc NAME (+ Distance1 hoac KLDV) -> 1 item
;;   - Neu khong: duyet vao dinh nghia block cua no, tim cac INSERT
;;     long ben trong (de quy toi da maxdepth cap) - dung cho
;;     associative array cua CAD (block an danh *U chua cac block con)
;; topent: ename cap cao nhat, dung de highlight ca cum
;; Tra ve: list cac item (name kldv dist topent)
;; ---------------------------------------------------------------------
(defun TKBD:ProcessInsert (obj topent depth / name kldv dist items btrname
                                              btr chk)
  (setq items '())
  (setq name (TKBD:GetAttValue obj "NAME"))
  (setq kldv (TKBD:GetAttValue obj "KLDV"))
  (setq dist (TKBD:GetDynProp obj "Distance1"))
  (cond
    ;; Doc truc tiep duoc -> 1 item
    ((and name (or dist (and kldv (/= kldv ""))))
     (setq items (list (list name (if kldv kldv "") dist topent)))
    )
    ;; Khong doc duoc va con duoc phep de quy -> duyet dinh nghia block
    ((> depth 0)
     (setq btrname (vl-catch-all-apply 'vlax-get (list obj 'Name)))
     (if (not (vl-catch-all-error-p btrname))
       (progn
         (setq btr (vl-catch-all-apply 'vla-Item
                     (list (vla-get-Blocks
                             (vla-get-ActiveDocument (vlax-get-acad-object)))
                           btrname)))
         (if (not (vl-catch-all-error-p btr))
           (vlax-for subent btr
             (setq chk (vl-catch-all-apply 'vla-get-ObjectName (list subent)))
             (if (and (not (vl-catch-all-error-p chk))
                      (= chk "AcDbBlockReference"))
               (setq items
                 (append items
                   (TKBD:ProcessInsert subent topent (1- depth))))
             )
           )
         )
       )
     )
    )
  )
  items
)

;; ---------------------------------------------------------------------
;; Thu thap du lieu tu selection set
;; Tra ve: (list data-list skip-count)
;; data item: (name kldv distance1 ename)
;; ---------------------------------------------------------------------
(defun TKBD:CollectData (ss / n i ent chk obj data skip items)
  (setq data '() skip 0)
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (setq chk (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (vl-catch-all-error-p chk)
          (setq skip (1+ skip))
          (progn
            (setq obj chk)
            ;; De quy toi da 3 cap: bat duoc array, array long array,
            ;; block bao ngoai chua block dong...
            (setq items (TKBD:ProcessInsert obj ent 3))
            (if items
              (setq data (append data items))
              (setq skip (1+ skip))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (list data skip)
)

;; ---------------------------------------------------------------------
;; Gop nhom theo (NAME + Distance1) - cung ten, cung chieu dai -> 1 dong
;; Tra ve list cac group: (name kldv distance count enames-list)
;; KLDV lay gia tri xuat hien dau tien trong nhom (khong cong don).
;; ---------------------------------------------------------------------
(defun TKBD:RoundKey (val)
  ;; lam tron ve 3 chu so thap phan de tranh sai so so thuc khi gop nhom
  (rtos val 2 3)
)

(defun TKBD:GroupData (datalist / groups key found g name kldv dist ent)
  (setq groups '())
  (foreach it datalist
    (setq name (nth 0 it) kldv (nth 1 it) dist (nth 2 it) ent (nth 3 it))
    ;; Block khong co Distance1: gop theo NAME + KLDV (de khong tron
    ;; lan cac cau kien dem cai co don trong khac nhau)
    (setq key
      (if dist
        (strcat (strcase name) "|" (TKBD:RoundKey dist))
        (strcat (strcase name) "|NODIST|" (strcase kldv))
      )
    )
    (setq found (assoc key groups))
    (if found
      (progn
        (setq g (cdr found))
        ;; g = (name kldv dist count enames)
        (setq g (list (nth 0 g)
                      (nth 1 g)
                      (nth 2 g)
                      (1+ (nth 3 g))
                      (cons ent (nth 4 g))))
        (setq groups (subst (cons key g) found groups))
      )
      (setq groups (cons (cons key (list name kldv dist 1 (list ent))) groups))
    )
  )
  (vl-sort (mapcar 'cdr groups)
    '(lambda (a b / da db)
       (setq da (if (nth 2 a) (nth 2 a) -1.0)
             db (if (nth 2 b) (nth 2 b) -1.0))
       (if (= (strcase (nth 0 a)) (strcase (nth 0 b)))
         (< da db)
         (< (strcase (nth 0 a)) (strcase (nth 0 b)))
       )
     )
  )
)

;; ---------------------------------------------------------------------
;; Highlight / bo highlight danh sach ename
;; ---------------------------------------------------------------------
(defun TKBD:HighlightGroup (enames flag / e chk)
  (foreach e enames
    (setq chk (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
    (if (not (vl-catch-all-error-p chk))
      (vl-catch-all-apply 'vla-Highlight (list chk flag))
    )
  )
)

;; ---------------------------------------------------------------------
;; Ghi file DCL ra thu muc TEMP (nham dam bao file .lsp giao duoc doc lap)
;; ---------------------------------------------------------------------
(defun TKBD:WriteDCL (/ path f)
  (setq path (strcat (getenv "TEMP") "\\tkbd_" (rtos (getvar "MILLISECS") 2 0) ".dcl"))
  (setq f (open path "w"))
  (write-line "tkbd_dlg : dialog {" f)
  (write-line "  label = \"THKL v1.7.1 - Tong Hop Khoi Luong Block Dong (Tong CD don vi m)\";" f)
  (write-line "  : list_box {" f)
  (write-line "    key = \"list_data\";" f)
  (write-line "    width = 80;" f)
  (write-line "    height = 18;" f)
  (write-line "    fixed_width_font = true;" f)
  (write-line "  }" f)
  (write-line "  : toggle {" f)
  (write-line "    key = \"tgl_mm2m\";" f)
  (write-line "    label = \"Distance1 don vi mm -> doi sang m khi tinh KL (KLDV = kg/m)\";" f)
  (write-line "  }" f)
  (write-line "  : text {" f)
  (write-line "    key = \"txt_warn\";" f)
  (write-line "    width = 70;" f)
  (write-line "    value = \"\";" f)
  (write-line "  }" f)
  (write-line "  spacer;" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"btn_select\"; label = \"Chon lai doi tuong <\"; width = 22; }" f)
  (write-line "    : button { key = \"btn_export\"; label = \"Xuat CSV...\"; width = 14; }" f)
  (write-line "  }" f)
  (write-line "  spacer;" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  path
)

;; ---------------------------------------------------------------------
;; Dua du lieu vao list_box (header + data + tong cong)
;; ---------------------------------------------------------------------
(defun TKBD:PopulateList (groups / lst g total-cnt total-kl total-cd kl totdist bad)
  (setq lst (list (TKBD:FormatRow "TEN CAU KIEN" "KLDV" "CD 1CK" "TONG CD(m)" "SL" "TONG KL")))
  (setq total-cnt 0 total-kl 0.0 total-cd 0.0 bad 0)
  (foreach g groups
    (setq kl (TKBD:CalcKL (nth 2 g) (nth 1 g) (nth 3 g)))
    (if kl
      (setq total-kl (+ total-kl kl))
      (progn (setq bad (1+ bad)) (setq kl "---"))
    )
    ;; Tong CD quy doi ra m (chia 1000 neu toggle mm->m dang bat)
    (setq totdist
      (if (nth 2 g)
        (if (= *TKBD-MM2M* "1")
          (/ (* (nth 2 g) (nth 3 g)) 1000.0)
          (* (nth 2 g) (nth 3 g))
        )
        nil
      )
    )
    (if totdist (setq total-cd (+ total-cd totdist)))
    (setq lst (append lst
      (list (TKBD:FormatRow (nth 0 g) (nth 1 g) (nth 2 g) totdist (nth 3 g) kl))))
    (setq total-cnt (+ total-cnt (nth 3 g)))
  )
  (setq lst (append lst
    (list (TKBD:FormatRow "TONG CONG" "" "" total-cd total-cnt total-kl))))
  (start_list "list_data")
  (mapcar 'add_list lst)
  (end_list)
  (set_tile "txt_warn"
    (if (> bad 0)
      (strcat "Canh bao: " (itoa bad) " dong co KLDV khong phai so -> KL = ---")
      ""
    )
  )
)

;; ---------------------------------------------------------------------
;; Xu ly khi chon 1 dong trong danh sach: highlight nhom tuong ung
;; Dong 0 = header, dong cuoi = tong cong -> bo qua
;; ---------------------------------------------------------------------
(defun TKBD:OnSelect (idx / n g)
  (if *TKBD-CurHL*
    (progn (TKBD:HighlightGroup *TKBD-CurHL* nil) (setq *TKBD-CurHL* nil))
  )
  (setq n (length *TKBD-Groups*))
  (if (and (>= idx 1) (<= idx n))
    (progn
      (setq g (nth (1- idx) *TKBD-Groups*))
      (setq *TKBD-CurHL* (nth 4 g))
      (TKBD:HighlightGroup *TKBD-CurHL* T)
    )
  )
  (princ)
)

;; ---------------------------------------------------------------------
;; Xuat CSV
;; ---------------------------------------------------------------------
(defun TKBD:CSVField (s)
  (if (or (vl-string-search "," s) (vl-string-search "\"" s))
    (strcat "\"" (vl-string-subst "\"\"" "\"" s) "\"")
    s
  )
)

(defun TKBD:DoExport (groups / fname f g kl total-kl totdist total-cd)
  (setq fname
    (getfiled "Xuat file CSV thong ke Block Dong"
      (strcat (getvar "DWGPREFIX") "ThongKe_BlockDong.csv")
      "csv" 1
    )
  )
  (if fname
    (progn
      (setq f (open fname "w"))
      (write-line "Ten cau kien,So luong,CD 1 cau kien,Tong chieu dai (m),KLDV,Tong KL (kg)" f)
      (setq total-kl 0.0 total-cd 0.0)
      (foreach g groups
        (setq kl (TKBD:CalcKL (nth 2 g) (nth 1 g) (nth 3 g)))
        (if kl (setq total-kl (+ total-kl kl)))
        (setq totdist
          (if (nth 2 g)
            (if (= *TKBD-MM2M* "1")
              (/ (* (nth 2 g) (nth 3 g)) 1000.0)
              (* (nth 2 g) (nth 3 g))
            )
            nil
          )
        )
        (if totdist (setq total-cd (+ total-cd totdist)))
        (write-line
          (strcat
            (TKBD:CSVField (nth 0 g)) ","
            (itoa (nth 3 g)) ","
            (if (nth 2 g) (rtos (nth 2 g) 2 2) "---") ","
            (if totdist (rtos totdist 2 2) "---") ","
            (TKBD:CSVField (nth 1 g)) ","
            (if kl (rtos kl 2 2) "---")
          )
          f
        )
      )
      (write-line
        (strcat "TONG CONG,,," (rtos total-cd 2 2) ",," (rtos total-kl 2 2)) f)
      (close f)
      (setq *TKBD-LastCSV* fname)
      (alert (strcat "Da xuat file:\n" fname))
    )
  )
  (princ)
)

;; ---------------------------------------------------------------------
;; Lenh chinh: TKBD
;; ---------------------------------------------------------------------
(defun c:THKL (/ *error* olderr ss coldata skip groups dcl-path dclid code)

  (setq olderr *error*)
  (defun *error* (msg)
    (if *TKBD-CurHL*
      (progn (TKBD:HighlightGroup *TKBD-CurHL* nil) (setq *TKBD-CurHL* nil))
    )
    (if (and dclid (> dclid 0)) (unload_dialog dclid))
    (if (and dcl-path (findfile dcl-path)) (vl-file-delete dcl-path))
    (setq *error* olderr)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort"))
      (princ (strcat "\nLoi TKBD: " msg))
    )
    (princ)
  )

  (setq *TKBD-CurHL* nil)
  (setq code 2)

  (while (= code 2)
    (setq dcl-path nil dclid nil)
    (princ "\nChon cac block dong can thong ke (NAME, KLDV, Distance1): ")
    (setq ss (ssget '((0 . "INSERT"))))
    (cond
      ((not ss)
       (princ "\nKhong co doi tuong nao duoc chon.")
       (setq code 0)
      )
      (t
       (setq coldata (TKBD:CollectData ss))
       (setq skip (cadr coldata))
       (setq coldata (car coldata))
       (cond
         ((not coldata)
          (alert "Khong tim thay block dong hop le nao co du du lieu (NAME + Distance1).")
          (setq code 0)
         )
         (t
          (if (> skip 0)
            (princ (strcat "\n(Bo qua " (itoa skip) " doi tuong khong hop le.)"))
          )
          (setq groups (TKBD:GroupData coldata))
          (setq *TKBD-Groups* groups)
          (setq dcl-path (TKBD:WriteDCL))
          (setq dclid (load_dialog dcl-path))
          (if (not (new_dialog "tkbd_dlg" dclid))
            (progn
              (alert "Khong the tao hop thoai TKBD.")
              (setq code 0)
            )
            (progn
              (set_tile "tgl_mm2m" *TKBD-MM2M*)
              (TKBD:PopulateList groups)
              (action_tile "list_data" "(TKBD:OnSelect (atoi $value))")
              (action_tile "tgl_mm2m"
                "(setq *TKBD-MM2M* $value) (TKBD:PopulateList *TKBD-Groups*)")
              (action_tile "btn_select" "(done_dialog 2)")
              (action_tile "btn_export" "(TKBD:DoExport groups)")
              (action_tile "accept" "(done_dialog 1)")
              (action_tile "cancel" "(done_dialog 0)")
              (setq code (start_dialog))
              (unload_dialog dclid)
              (setq dclid nil)
              (if (findfile dcl-path) (vl-file-delete dcl-path))
              (setq dcl-path nil)
              (if *TKBD-CurHL*
                (progn (TKBD:HighlightGroup *TKBD-CurHL* nil) (setq *TKBD-CurHL* nil))
              )
             )
          )
         )
       )
      )
    )
  )

  (*error* nil)
  (princ "\nTKBD hoan tat.")
  (princ)
)

(princ "\nDa nap THKL.LSP v1.7.1 (fix loi TKBD:PARSEKLDV) - Go lenh THKL de thong ke block dong (NAME/KLDV/Distance1).")
(princ)

;;; --- HET [28] THKL ---

;;; ===========================================================================
;;;  BANG TONG HOP LENH - in ra sau khi nap xong DOAN.lsp
;;; ===========================================================================
(princ "\n")
(princ "\n===============================================================")
(princ "\n   DOAN.LSP - DA NAP XONG 28 BO LENH LISP")
(princ "\n===============================================================")
(princ "\n-- NHOM 1 - TIEN ICH CHUNG")
(princ "\n   VLX                    Ve thep dai xoan (vong lo xo) theo 2 diem pick")
(princ "\n   HH                     Chon doi tuong -> lay layer cua no lam layer hien hanh")
(princ "\n   BT                     Sua Dimension thanh dang n@KC=Tong")
(princ "\n   UPCHU                  Up ty le co chu cho Text/Mtext/Dim/Leader/Block")
(princ "\n-- NHOM 2 - TEXT & KY TU")
(princ "\n   VTT / CVT              Viet chu nhanh / chuyen doi text")
(princ "\n   KTD                    Chen ky tu Hy Lap va ky tu dac biet")
(princ "\n   GOM / GM               Gom nhieu Text roi thanh 1 MText")
(princ "\n-- NHOM 3 - BLOCK: CHEN / VE / THAY THE")
(princ "\n   LDB                    Load toan bo block tu 1 file DWG chi dinh vao ban ve")
(princ "\n   BSC                    Dat block ve dung ty le (co hop thoai)")
(princ "\n   ALR / ALRGUI           Align doi tuong kieu Revit (fix lech mep)")
(princ "\n   DBL                    Chen block hang loat theo toa do tu file")
(princ "\n   DYN                    Ve block dong theo tuyen, co khung xem truoc block")
(princ "\n   TBL / THAYBLOCK        Thay the block cu bang block moi, giu Distance1 (v3.2 - co xem truoc)")
(princ "\n-- NHOM 4 - ATTRIBUTE")
(princ "\n   TAT                    Them Attribute (Name / LKDV / Distance1) vao block")
(princ "\n   ATTS                   Dong bo Attribute, nhan ca block chua co att")
(princ "\n   BET                    Sua nhanh Attribute cua block")
(princ "\n   SYNCATT / CPATT        Copy block va tang dan gia tri Attribute")
(princ "\n   DTEN                   Danh so / danh ten Attribute hang loat theo thu tu")
(princ "\n   EDA                    Sua nhanh Name va Distance1 cua block dong")
(princ "\n   BUN                    Doi don vi (Block Units) cua block")
(princ "\n-- NHOM 5 - TOA DO / CAO DO / LY TRINH")
(princ "\n   DTD / DTDMOVE / DTDCOPY Danh toa do cho block va diem, tu cap nhat khi REGEN")
(princ "\n   CD                     Danh cao do tuong doi")
(princ "\n   TDD                    Danh ly trinh / cao do tuong doi cho block")
(princ "\n   MC                     Danh ky hieu mat cat (v1.6 - chon block + xem truoc)")
(princ "\n-- NHOM 6 - LAYOUT / VIEWPORT")
(princ "\n   SMV                    Tao Mview theo ty le nhap tu Model")
(princ "\n   KVP                    Khoa / mo khoa viewport theo tung Layout")
(princ "\n-- NHOM 7 - GHI CHU & THONG KE KHOI LUONG")
(princ "\n   TAG / TAGUPDATE        Tag ghi chu block, lay Distance1 tu parameter dong hoac ATT")
(princ "\n   THKL                   Tong hop / thong ke khoi luong theo block, xuat CSV")
(princ "\n===============================================================")
(princ "\n   Go ten lenh de su dung. Chuc ban ve nhanh!")
(princ "\n===============================================================\n")
(princ)
