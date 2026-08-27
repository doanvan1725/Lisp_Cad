;;; =========================================================================
;;; THAYBLOCK.LSP  -  v3
;;; Lenh: THAYBLOCK  (lenh tat: TBL)
;;;
;;; Quy trinh:
;;;   1) Go TBL -> QUET CHON cac block cu can thay (chon nhieu)
;;;   2) Hop thoai hien ra:
;;;        - Danh sach TAT CA block dong co trong ban ve -> chon block MOI
;;;        - O XEM TRUOC hinh dang block (nhu che do Ctrl+2 cua CAD)  [v3]
;;;        - Danh sach tham so dong cua block moi -> chon tham so chieu dai
;;;          can GIU NGUYEN (vd Distance1)
;;;   3) Bam OK (hoac nhay dup vao ten block) -> thay toan bo:
;;;        - Dung vi tri, goc xoay, ty le, layer cua block cu
;;;        - GIU NGUYEN gia tri chieu dai cua block cu
;;;        - Attribute (NAME...) LAY THEO GIA TRI MAC DINH CUA BLOCK MOI
;;;        - Cac thu khac theo block moi; block cu bi xoa
;;; =========================================================================

(vl-load-com)

(if (null *tbl-blkname*) (setq *tbl-blkname* ""))
(if (null *tbl-prop*)    (setq *tbl-prop* "Distance1"))

;; ---------- Lay gia tri Dynamic Property theo ten ----------
(defun TBL:GetDynProp (blkObj propName / props i res)
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

;; ---------- Gan gia tri Dynamic Property theo ten ----------
(defun TBL:SetDynProp (blkObj propName val / props i done)
  (setq done nil)
  (if (and (vlax-property-available-p blkObj 'IsDynamicBlock)
           (eq (vla-get-IsDynamicBlock blkObj) :vlax-true))
    (progn
      (setq props (vlax-invoke blkObj 'GetDynamicBlockProperties))
      (setq i 0)
      (while (and (< i (length props)) (not done))
        (if (= (strcase (vla-get-PropertyName (nth i props))) (strcase propName))
          (progn
            (vl-catch-all-apply
              'vla-put-Value
              (list (nth i props)
                    (vlax-make-variant (float val) vlax-vbDouble))
            )
            (setq done t)
          )
        )
        (setq i (1+ i))
      )
    )
  )
  done
)

;; ---------- Liet ke ten tham so dong (bo qua Origin) ----------
(defun TBL:ListDynProps (blkObj / res nm)
  (setq res nil)
  (if (and (vlax-property-available-p blkObj 'IsDynamicBlock)
           (eq (vla-get-IsDynamicBlock blkObj) :vlax-true))
    (foreach p (vlax-invoke blkObj 'GetDynamicBlockProperties)
      (setq nm (vla-get-PropertyName p))
      (if (/= (strcase nm) "ORIGIN")
        (setq res (cons nm res))
      )
    )
  )
  (reverse res)
)

;; ---------- Ten goc block (ke ca block dong *U...) ----------
(defun TBL:EffName (blkObj)
  (if (vlax-property-available-p blkObj 'EffectiveName)
    (vla-get-EffectiveName blkObj)
    (vla-get-Name blkObj)
  )
)

;; ---------- Quet toan ban ve, lap danh sach block dong ----------
;; Tra ve list: ((tenblock ename-tham-chieu ten-block-thuc) ...)
(defun TBL:CollectDynBlocks (/ ss i ent obj nm res)
  (setq res nil)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq i (1+ i))
        (setq obj (vlax-ename->vla-object ent))
        (if (and (vlax-property-available-p obj 'IsDynamicBlock)
                 (eq (vla-get-IsDynamicBlock obj) :vlax-true))
          (progn
            (setq nm (TBL:EffName obj))
            (if (not (assoc nm res))
              (setq res (cons (list nm ent (vla-get-Name obj)) res))
            )
          )
        )
      )
    )
  )
  (vl-sort res (function (lambda (a b) (< (strcase (car a)) (strcase (car b))))))
)

;; =========================================================================
;; ===============  XEM TRUOC HINH DANG BLOCK (v3)  ========================
;; =========================================================================

;; --- Bien doi 1 diem theo ty le / goc xoay / diem chen ---
(defun TBL:Tx (p sx sy rot off / x y c s)
  (setq x (* sx (car p))
        y (* sy (cadr p))
        c (cos rot)
        s (sin rot)
  )
  (list (+ (car off) (- (* x c) (* y s)))
        (+ (cadr off) (+ (* x s) (* y c))))
)

;; --- Lay so an toan tu thuoc tinh ActiveX ---
(defun TBL:Num (obj prop def / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (or (vl-catch-all-error-p r) (not (numberp r))) def r)
)

;; --- Mau ve cho 1 doi tuong (tra ve chi so mau DCL) ---
(defun TBL:Col (e / c lay r)
  (setq c (vl-catch-all-apply 'vla-get-Color (list e)))
  (if (or (vl-catch-all-error-p c) (not (numberp c))) (setq c 256))
  ;; BYLAYER -> lay mau cua layer
  (if (or (= c 256) (= c 0))
    (progn
      (setq lay (vl-catch-all-apply 'vla-get-Layer (list e)))
      (if (not (vl-catch-all-error-p lay))
        (progn
          (setq r (tblsearch "LAYER" lay))
          (if r (setq c (abs (cdr (assoc 62 r)))))
        )
      )
    )
  )
  (cond
    ((not (numberp c)) 7)
    ((or (= c 0) (= c 256) (> c 255)) 7)   ; khong ro -> trang, hop voi nen den
    (t c)                                   ; giu nguyen mau ACI nhu ngoai ban ve
  )
)

;; --- Lay chuoi diem cua 1 doi tuong (toa do noi bo block) ---
(defun TBL:EntPts (e / en nm sp ep n i lst r)
  (setq nm (vla-get-ObjectName e)
        en (vlax-vla-object->ename e)
        lst nil
  )
  (cond
    ;; duong thang: chi can 2 diem
    ((= nm "AcDbLine")
     (setq lst (list (vlax-get e 'StartPoint) (vlax-get e 'EndPoint))))
    ;; bo qua chu, hatch, att... cho nhe
    ((member nm '("AcDbText" "AcDbMText" "AcDbAttributeDefinition"
                  "AcDbHatch" "AcDbSolid" "AcDbPoint"))
     (setq lst nil))
    (t
     (setq sp (vl-catch-all-apply 'vlax-curve-getStartParam (list en))
           ep (vl-catch-all-apply 'vlax-curve-getEndParam (list en))
     )
     (if (or (vl-catch-all-error-p sp) (vl-catch-all-error-p ep)
             (not (numberp sp)) (not (numberp ep)) (<= (- ep sp) 1e-12))
       (setq lst nil)
       (progn
         (setq n (fix (* 6.0 (- ep sp))))
         (if (< n 8) (setq n 8))
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

;; --- Duyet dinh nghia block, thu ve cac doan thang (x1 y1 x2 y2 mau) ---
(defun TBL:CollectSegs (bo sx sy rot off depth / segs nm pts col p q i
                        blks bo2 ip co cs2 cr)
  (setq segs nil
        blks (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
  )
  (vlax-for e bo
    (setq nm (vl-catch-all-apply 'vla-get-ObjectName (list e)))
    (if (not (vl-catch-all-error-p nm))
      (if (= nm "AcDbBlockReference")
        ;; --- block long nhau: de quy ---
        (if (< depth 3)
          (progn
            (setq ip  (vl-catch-all-apply 'vlax-get (list e 'InsertionPoint)))
            (if (not (vl-catch-all-error-p ip))
              (progn
                (setq co  (TBL:Tx ip sx sy rot off)
                      cs2 (* sx (TBL:Num e 'XScaleFactor 1.0))
                      cr  (+ rot (TBL:Num e 'Rotation 0.0))
                )
                (setq bo2 (vl-catch-all-apply
                            'vla-Item
                            (list blks (vl-catch-all-apply 'vla-get-Name (list e)))))
                (if (not (vl-catch-all-error-p bo2))
                  (setq segs (append segs
                               (TBL:CollectSegs bo2 cs2
                                 (* sy (TBL:Num e 'YScaleFactor 1.0))
                                 cr co (1+ depth))))
                )
              )
            )
          )
        )
        ;; --- doi tuong thuong ---
        (progn
          (setq pts (TBL:EntPts e))
          (if (and pts (> (length pts) 1))
            (progn
              (setq col (TBL:Col e) i 0)
              (while (< i (1- (length pts)))
                (setq p (TBL:Tx (nth i pts) sx sy rot off)
                      q (TBL:Tx (nth (1+ i) pts) sx sy rot off)
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

;; --- Lay (co cache) cac doan thang cua 1 block theo ten dinh nghia ---
(defun TBL:GetSegs (bname / pr bo segs)
  (setq pr (assoc bname *tbl-segcache*))
  (if pr
    (cdr pr)
    (progn
      (setq bo (vl-catch-all-apply
                 'vla-Item
                 (list (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
                       bname)))
      (setq segs (if (vl-catch-all-error-p bo)
                   nil
                   (TBL:CollectSegs bo 1.0 1.0 0.0 '(0.0 0.0) 0)))
      (setq *tbl-segcache* (cons (cons bname segs) *tbl-segcache*))
      segs
    )
  )
)

;; --- Ve xem truoc vao o image ---
;; QUAN TRONG: moi tinh toan va set_tile phai xong TRUOC khi mo start_image.
;; Goi set_tile giua start_image / end_image se lam mat het vector da ve.
(defun TBL:Clamp (v lo hi) (max lo (min hi (fix v))))

(defun TBL:DrawPrev (idx / item bname segs dx dy minx maxx miny maxy
                     k ox oy s lst txt x1 y1 x2 y2)
  (setq dx  (dimx_tile "prev")
        dy  (dimy_tile "prev")
        lst nil
        txt " "
  )
  (if (and idx (>= idx 0) (< idx (length *tbl-dynlist*)))
    (progn
      (setq item  (nth idx *tbl-dynlist*)
            bname (caddr item)
            segs  (TBL:GetSegs bname)
      )
      (if (null segs)
        (setq txt (strcat (car item) "  -  (khong ve duoc xem truoc)"))
        (progn
          ;; --- khung bao ---
          (setq minx nil)
          (foreach s segs
            (if (null minx)
              (setq minx (min (car s) (caddr s))   maxx (max (car s) (caddr s))
                    miny (min (cadr s) (cadddr s)) maxy (max (cadr s) (cadddr s)))
              (setq minx (min minx (car s) (caddr s))
                    maxx (max maxx (car s) (caddr s))
                    miny (min miny (cadr s) (cadddr s))
                    maxy (max maxy (cadr s) (cadddr s)))
            )
          )
          (if (< (- maxx minx) 1e-9) (setq maxx (+ minx 1.0)))
          (if (< (- maxy miny) 1e-9) (setq maxy (+ miny 1.0)))
          (setq k  (min (/ (float (- dx 10)) (- maxx minx))
                        (/ (float (- dy 10)) (- maxy miny)))
                ox (/ (- dx (* k (- maxx minx))) 2.0)
                oy (/ (- dy (* k (- maxy miny))) 2.0)
          )
          ;; --- doi sang toa do pixel truoc, chua ve voi ---
          (foreach s segs
            (setq x1 (TBL:Clamp (+ ox (* k (- (car s) minx))) 0 (1- dx))
                  y1 (TBL:Clamp (- dy oy (* k (- (cadr s) miny))) 0 (1- dy))
                  x2 (TBL:Clamp (+ ox (* k (- (caddr s) minx))) 0 (1- dx))
                  y2 (TBL:Clamp (- dy oy (* k (- (cadddr s) miny))) 0 (1- dy))
            )
            (setq lst (cons (list x1 y1 x2 y2 (nth 4 s)) lst))
          )
          (setq txt (strcat (car item) "   |   " (itoa (length segs)) " doan ve"))
          (princ (strcat "\n[XemTruoc] " bname
                         "  tile=" (itoa dx) "x" (itoa dy)
                         "  khung X " (rtos minx 2 1) "->" (rtos maxx 2 1)
                         "  Y " (rtos miny 2 1) "->" (rtos maxy 2 1)
                         "  he so=" (rtos k 2 4)))
        )
      )
    )
  )
  ;; --- set_tile XONG HET roi moi mo image ---
  (set_tile "binfo" txt)
  (start_image "prev")
  (fill_image 0 0 dx dy 0)
  ;; khung vien de kiem chung tile co ve duoc khong
  (vector_image 0 0 (1- dx) 0 8)
  (vector_image (1- dx) 0 (1- dx) (1- dy) 8)
  (vector_image (1- dx) (1- dy) 0 (1- dy) 8)
  (vector_image 0 (1- dy) 0 0 8)
  (foreach s lst
    (vector_image (car s) (cadr s) (caddr s) (cadddr s) (nth 4 s))
  )
  (end_image)
)

;; ---------- DCL ----------
(defun TBL:WriteDCL ( / f fn)
  (setq fn (vl-filename-mktemp "thayblock" nil ".dcl"))
  (setq f (open fn "w"))
  (foreach s
    (list
      "thayblock : dialog {"
      "  label = \"Thay block dong - giu nguyen chieu dai (v3.2)\";"
      "  : row {"
      "    : column {"
      "      : list_box {"
      "        key = \"blist\";"
      "        label = \"Block dong trong ban ve (chon block MOI):\";"
      "        height = 18; width = 34;"
      "      }"
      "    }"
      "    : column {"
      "      : boxed_column {"
      "        label = \"Xem truoc\";"
      "        : image { key = \"prev\"; width = 40; height = 16; color = 0; }"
      "        : text { key = \"binfo\"; label = \" \"; width = 40; }"
      "      }"
      "    }"
      "  }"
      "  : popup_list {"
      "    key = \"plist\";"
      "    label = \"Tham so chieu dai GIU NGUYEN:\";"
      "    width = 40;"
      "  }"
      "  : text { key = \"info\"; label = \"\"; width = 70; }"
      "  : text { label = \"Meo: nhay dup vao ten block trong danh sach = chon va chay luon.\"; width = 70; }"
      "  spacer;"
      "  ok_cancel;"
      "}"
    )
    (write-line s f)
  )
  (close f)
  fn
)

;; ---------- Do danh sach tham so cua block dang chon vao popup ----------
(defun TBL:FillProps (idx / item obj)
  (setq *tbl-curprops* nil)
  (if (and idx (>= idx 0) (< idx (length *tbl-dynlist*)))
    (progn
      (setq item (nth idx *tbl-dynlist*))
      (setq obj (vlax-ename->vla-object (cadr item)))
      (setq *tbl-curprops* (TBL:ListDynProps obj))
    )
  )
  (start_list "plist")
  (if *tbl-curprops*
    (foreach p *tbl-curprops* (add_list p))
    (add_list "(khong co tham so)")
  )
  (end_list)
  (if (and *tbl-curprops* (member *tbl-prop* *tbl-curprops*))
    (set_tile "plist"
      (itoa (- (length *tbl-curprops*)
               (length (member *tbl-prop* *tbl-curprops*)))))
    (set_tile "plist" "0")
  )
)

;; ---------- Xu ly khi chon trong danh sach ----------
(defun TBL:OnSel (v r / n)
  (setq n (atoi v))
  (TBL:FillProps n)
  (TBL:DrawPrev n)
  (if (= r 4)    ; nhay dup
    (progn
      (setq idx  n
            pidx (atoi (get_tile "plist"))
            res  1
      )
      (done_dialog 1)
    )
  )
)

;; =========================================================================
;; LENH CHINH
;; =========================================================================
(defun c:THAYBLOCK (/ doc ss dclF dclId res idx pidx
                      newName newRefEnt propName
                      i entOld objOld oldName owner
                      insPt rot sx sy sz layer lenVal
                      objIns cnt skip startIdx)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *tbl-segcache* nil)

  ;; ----- 1. Quet chon cac block cu can thay -----
  (princ "\nQuet chon cac block CU can thay (chon nhieu duoc): ")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (null ss) (progn (princ "\nKhong chon block nao.") (exit)))
  (princ (strcat "\nDa chon " (itoa (sslength ss)) " block."))

  ;; ----- 2. Lap danh sach block dong trong ban ve -----
  (setq *tbl-dynlist* (TBL:CollectDynBlocks))
  (if (null *tbl-dynlist*)
    (progn (princ "\nBan ve khong co block dong nao!") (exit))
  )

  ;; ----- 3. Hop thoai chon block moi + tham so -----
  (setq dclF (TBL:WriteDCL))
  (setq dclId (load_dialog dclF))
  (if (not (new_dialog "thayblock" dclId))
    (progn (princ "\nKhong mo duoc hop thoai!") (exit))
  )

  (start_list "blist")
  (foreach item *tbl-dynlist* (add_list (car item)))
  (end_list)

  (setq startIdx 0)
  (if (assoc *tbl-blkname* *tbl-dynlist*)
    (setq startIdx (- (length *tbl-dynlist*)
                      (length (member (assoc *tbl-blkname* *tbl-dynlist*)
                                      *tbl-dynlist*))))
  )
  (set_tile "blist" (itoa startIdx))
  (TBL:FillProps startIdx)
  (TBL:DrawPrev startIdx)
  (set_tile "info" (strcat "Se thay " (itoa (sslength ss)) " block da quet chon."))

  (action_tile "blist" "(TBL:OnSel $value $reason)")
  (action_tile "accept"
    "(setq idx (atoi (get_tile \"blist\")) pidx (atoi (get_tile \"plist\")) res 1)(done_dialog 1)")
  (action_tile "cancel" "(setq res nil)(done_dialog 0)")
  (start_dialog)
  (unload_dialog dclId)
  (vl-file-delete dclF)

  (if (not res) (progn (princ "\nDa huy.") (exit)))

  ;; ----- 4. Doc lua chon -----
  (setq newName   (car (nth idx *tbl-dynlist*)))
  (setq newRefEnt (cadr (nth idx *tbl-dynlist*)))
  (setq propName
    (if (and *tbl-curprops* (< pidx (length *tbl-curprops*)))
      (nth pidx *tbl-curprops*)
      *tbl-prop*
    )
  )
  (setq *tbl-blkname* newName
        *tbl-prop*    propName
  )
  (princ (strcat "\n-> Thay bang block: " newName
                 " | Giu nguyen tham so: " propName))

  ;; ----- 5. Thay tung block -----
  (setq cnt 0 skip 0 i 0)
  (repeat (sslength ss)
    (setq entOld (ssname ss i))
    (setq i (1+ i))
    (setq objOld (vlax-ename->vla-object entOld))
    (setq oldName (TBL:EffName objOld))

    (if (= (strcase oldName) (strcase newName))
      (setq skip (1+ skip))
      (progn
        (setq insPt  (vlax-get objOld 'InsertionPoint)
              rot    (vla-get-Rotation objOld)
              layer  (vla-get-Layer objOld)
              lenVal (TBL:GetDynProp objOld propName)
        )
        (setq sx (vl-catch-all-apply 'vla-get-XScaleFactor (list objOld)))
        (if (vl-catch-all-error-p sx) (setq sx 1.0))
        (setq sy (vl-catch-all-apply 'vla-get-YScaleFactor (list objOld)))
        (if (vl-catch-all-error-p sy) (setq sy sx))
        (setq sz (vl-catch-all-apply 'vla-get-ZScaleFactor (list objOld)))
        (if (vl-catch-all-error-p sz) (setq sz sx))

        (setq owner (vla-ObjectIDToObject doc (vla-get-OwnerID objOld)))
        (setq objIns
          (vl-catch-all-apply
            'vla-InsertBlock
            (list owner (vlax-3d-point insPt) newName sx sy sz rot)
          )
        )
        (if (vl-catch-all-error-p objIns)
          (progn
            (princ (strcat "\n!! Khong chen duoc block moi: "
                           (vl-catch-all-error-message objIns)))
            (setq skip (1+ skip))
          )
          (progn
            (vla-put-Layer objIns layer)
            (if (numberp lenVal)
              (if (not (TBL:SetDynProp objIns propName lenVal))
                (princ (strcat "\n!! Block moi khong co tham so \"" propName "\"."))
              )
              (princ (strcat "\n!! 1 block cu khong co tham so \"" propName
                             "\" - block moi giu chieu dai mac dinh."))
            )
            (vla-Update objIns)
            (vla-Delete objOld)
            (setq cnt (1+ cnt))
          )
        )
      )
    )
  )

  (princ (strcat "\nHoan thanh: da thay " (itoa cnt) " block"
                 (if (> skip 0) (strcat ", bo qua " (itoa skip)) "")
                 ". Block moi: " newName
                 " | Tham so giu nguyen: " propName))
  (princ)
)

(defun c:TBL () (c:THAYBLOCK))

(princ "\n=== THAYBLOCK v3.2 da nap (xem truoc block, nen den nhu CAD) - Go TBL de chay ===")
(princ)