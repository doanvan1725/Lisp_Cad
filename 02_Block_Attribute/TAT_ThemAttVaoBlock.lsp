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