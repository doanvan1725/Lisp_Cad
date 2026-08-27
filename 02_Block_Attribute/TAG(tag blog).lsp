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
        (setq blkEnt (handent blkHandle)
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

;; ---------- Reactor: tu dong cap nhat khi REGEN ----------
(if (not *TAG:CmdReactor*)
  (setq *TAG:CmdReactor*
    (vlr-command-reactor "TAGREACTOR"
      '((:vlr-commandWillStart . TAG:OnCommand))
    )
  )
)

(defun TAG:OnCommand (calling-reactor cmd-list / cmdName)
  (setq cmdName (strcase (car cmd-list)))
  (if (wcmatch cmdName "*REGEN*")
    (TAG:UpdateAll)
  )
)

(princ "\n=== TAG.LSP v6.1 da nap: Distance1 tu Parameter dong HOAC ATT; o so chu so thap phan co nut +/-. Go TAGUPDATE de cap nhat tag cu. ===")
(princ)