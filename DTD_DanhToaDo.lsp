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
