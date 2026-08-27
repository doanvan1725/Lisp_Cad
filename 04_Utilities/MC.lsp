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