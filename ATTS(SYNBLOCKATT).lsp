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