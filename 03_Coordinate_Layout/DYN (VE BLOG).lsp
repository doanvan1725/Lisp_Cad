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