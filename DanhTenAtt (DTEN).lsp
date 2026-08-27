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