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