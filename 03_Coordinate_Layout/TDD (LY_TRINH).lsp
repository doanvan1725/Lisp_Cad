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