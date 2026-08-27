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