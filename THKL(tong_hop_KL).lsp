;;; =====================================================================
;;; TKBD.LSP  -  Thong Ke Block Dong (NAME, KLDV, Distance1, So Luong, KL)
;;; Version : 1.7.1  (2026-07-25)
;;;
;;; v1.7.1: FIX loi "no function definition: TKBD:PARSEKLDV" - ham parse
;;;         KLDV bi go nham thanh THKL:ParseKLDV (sot lai luc doi ten
;;;         lenh TKBD -> THKL), trong khi noi goi van la TKBD:ParseKLDV.
;;;         Da doi lai dung TKBD:ParseKLDV cho khop toan bo file.
;;; v1.7: Cot Tong CD quy doi ra don vi m (chia 1000 khi toggle mm->m bat).
;;;       CD 1 cau kien van giu don vi ve goc (mm).
;;; v1.6: Them cot "CD 1 cau kien" truoc cot "Tong chieu dai":
;;;       Tong CD = CD 1CK x So luong. Dong TONG CONG cong ca Tong CD.
;;; v1.5: Ho tro associative array cua CAD: INSERT khong co att NAME se
;;;       duoc duyet vao dinh nghia block (de quy 3 cap) de tim cac
;;;       block dong long ben trong (array = block an danh *U chua
;;;       cac ban sao). Highlight ap len ca cum array.
;;; v1.4: Block khong co Distance1 nhung co KLDV van duoc thong ke:
;;;       KL = KLDV x So Luong (cau kien dem cai, KLDV = kg/cai).
;;;       Cot Chieu dai hien "---". Gop nhom theo NAME + KLDV.
;;; v1.3: Doi thu tu cot: Ten cau kien - So luong - Chieu dai - KLDV - Tong KL
;;; v1.2: Them cot TONG KL = Distance1 x KLDV x So Luong
;;;       - Toggle "mm -> m": chia Distance1 cho 1000 khi tinh KL
;;;         (Distance1 ve bang mm, KLDV la kg/m). Luu theo session.
;;;       - KLDV parse tu attribute text, chap nhan dau phay VN ("21,7")
;;;       - KLDV khong hop le -> KL hien "---" va co dong canh bao
;;; Lenh goi : TKBD
;;;
;;; Chuc nang:
;;;   - Chon cac block dong (INSERT) trong ban ve
;;;   - Doc attribute "NAME", attribute "KLDV" va dynamic parameter "Distance1"
;;;   - Gop nhom theo (NAME + Distance1): block nao cung ten, cung chieu dai
;;;     thi gop vao 1 dong va cong don So Luong. KLDV chi hien thi (lay gia
;;;     tri dau tien trong nhom, khong cong don).
;;;   - Hien thi bang thong ke trong hop thoai DCL (fixed width font)
;;;   - Click 1 dong trong danh sach -> highlight cac doi tuong tuong ung
;;;   - Nut "Chon lai doi tuong" de chon lai selection set
;;;   - Nut "Xuat CSV..." de xuat file CSV
;;;
;;; Ghi chu: khong dung dau tieng Viet trong chuoi string de tranh loi ANSI.
;;; =====================================================================

(vl-load-com)

;; ---------------------------------------------------------------------
;; Bien toan cuc phien lam viec
;; ---------------------------------------------------------------------
(if (not *TKBD-CurHL*)   (setq *TKBD-CurHL* nil))
(if (not *TKBD-Groups*)  (setq *TKBD-Groups* nil))
(if (not *TKBD-LastCSV*) (setq *TKBD-LastCSV* nil))
(if (not *TKBD-MM2M*)    (setq *TKBD-MM2M* "1"))   ; "1" = doi Distance1 mm->m khi tinh KL

;; ---------------------------------------------------------------------
;; Parse KLDV (attribute text) sang so thuc. Tra ve nil neu khong hop le.
;; Chap nhan dau phay thap phan kieu VN (vd "21,7" -> 21.7).
;; ---------------------------------------------------------------------
(defun TKBD:ParseKLDV (s / s2)
  (if (and s (/= s ""))
    (progn
      (setq s2 (vl-string-translate "," "." s))
      (if (distof s2 2) (distof s2 2) nil)
    )
    nil
  )
)

;; ---------------------------------------------------------------------
;; Tinh KL 1 nhom:
;;   - Co Distance1  : KL = Distance1(quy doi mm->m neu bat) x KLDV x SL
;;   - Khong Distance1: KL = KLDV x SL   (cau kien dem cai, KLDV = kg/cai)
;; Tra ve nil neu KLDV khong parse duoc.
;; ---------------------------------------------------------------------
(defun TKBD:CalcKL (dist kldvStr cnt / kldvNum d)
  (setq kldvNum (TKBD:ParseKLDV kldvStr))
  (if kldvNum
    (if dist
      (progn
        (setq d (if (= *TKBD-MM2M* "1") (/ dist 1000.0) dist))
        (* d kldvNum cnt)
      )
      (* kldvNum cnt)
    )
    nil
  )
)

;; ---------------------------------------------------------------------
;; Ham tien ich: dem khoang trang, can le chuoi
;; ---------------------------------------------------------------------
(defun TKBD:Spaces (n / s)
  (setq s "")
  (repeat (max n 0) (setq s (strcat s " ")))
  s
)

(defun TKBD:Pad (str width / s)
  ;; can trai, cat bot neu qua dai
  (setq s (if str (vl-princ-to-string str) ""))
  (if (> (strlen s) width)
    (substr s 1 width)
    (strcat s (TKBD:Spaces (- width (strlen s))))
  )
)

(defun TKBD:PadNum (str width / s)
  ;; can phai, dung cho so
  (setq s (if str (vl-princ-to-string str) ""))
  (if (> (strlen s) width)
    (substr s 1 width)
    (strcat (TKBD:Spaces (- width (strlen s))) s)
  )
)

(defun TKBD:FormatRow (name kldv dist totdist cnt kl / distStr totStr klStr)
  (setq distStr
    (cond
      ((numberp dist) (rtos dist 2 2))
      ((not dist) "---")
      (t (vl-princ-to-string dist))
    )
  )
  (setq totStr
    (cond
      ((numberp totdist) (rtos totdist 2 2))
      ((not totdist) "---")
      (t (vl-princ-to-string totdist))
    )
  )
  (setq klStr
    (cond
      ((numberp kl) (rtos kl 2 2))
      (t (vl-princ-to-string kl))
    )
  )
  ;; Thu tu cot: Ten - So luong - CD 1 cau kien - Tong CD - KLDV - Tong KL
  (strcat
    (TKBD:Pad name 18) " "
    (TKBD:PadNum (vl-princ-to-string cnt) 6) " "
    (TKBD:PadNum distStr 11) " "
    (TKBD:PadNum totStr 12) " "
    (TKBD:PadNum kldv 8) " "
    (TKBD:PadNum klStr 12)
  )
)

;; ---------------------------------------------------------------------
;; Doc attribute theo tag, an toan voi vl-catch-all-apply
;; ---------------------------------------------------------------------
(defun TKBD:GetAttValue (obj tag / chk atts a res)
  (setq res nil)
  (setq chk (vl-catch-all-apply 'vlax-get (list obj 'HasAttributes)))
  (if (and (not (vl-catch-all-error-p chk)) chk)
    (progn
      (setq atts (vl-catch-all-apply 'vlax-invoke (list obj 'GetAttributes)))
      (if (not (vl-catch-all-error-p atts))
        (foreach a atts
          (if (and (not res)
                   (= (strcase (vlax-get a 'TagString)) (strcase tag)))
            (setq res (vlax-get a 'TextString))
          )
        )
      )
    )
  )
  res
)

;; ---------------------------------------------------------------------
;; Doc dynamic property theo ten (vd: "Distance1")
;; ---------------------------------------------------------------------
(defun TKBD:GetDynProp (obj pname / chk props p res)
  (setq res nil)
  (setq chk (vl-catch-all-apply 'vlax-get (list obj 'IsDynamicBlock)))
  (if (and (not (vl-catch-all-error-p chk)) chk)
    (progn
      (setq props (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
      (if (not (vl-catch-all-error-p props))
        (foreach p props
          (if (and (not res)
                   (= (strcase (vlax-get p 'PropertyName)) (strcase pname)))
            (setq res (vlax-get p 'Value))
          )
        )
      )
    )
  )
  res
)

;; ---------------------------------------------------------------------
;; Xu ly 1 doi tuong INSERT (co the la array / block long nhau):
;;   - Neu doc truc tiep duoc NAME (+ Distance1 hoac KLDV) -> 1 item
;;   - Neu khong: duyet vao dinh nghia block cua no, tim cac INSERT
;;     long ben trong (de quy toi da maxdepth cap) - dung cho
;;     associative array cua CAD (block an danh *U chua cac block con)
;; topent: ename cap cao nhat, dung de highlight ca cum
;; Tra ve: list cac item (name kldv dist topent)
;; ---------------------------------------------------------------------
(defun TKBD:ProcessInsert (obj topent depth / name kldv dist items btrname
                                              btr chk)
  (setq items '())
  (setq name (TKBD:GetAttValue obj "NAME"))
  (setq kldv (TKBD:GetAttValue obj "KLDV"))
  (setq dist (TKBD:GetDynProp obj "Distance1"))
  (cond
    ;; Doc truc tiep duoc -> 1 item
    ((and name (or dist (and kldv (/= kldv ""))))
     (setq items (list (list name (if kldv kldv "") dist topent)))
    )
    ;; Khong doc duoc va con duoc phep de quy -> duyet dinh nghia block
    ((> depth 0)
     (setq btrname (vl-catch-all-apply 'vlax-get (list obj 'Name)))
     (if (not (vl-catch-all-error-p btrname))
       (progn
         (setq btr (vl-catch-all-apply 'vla-Item
                     (list (vla-get-Blocks
                             (vla-get-ActiveDocument (vlax-get-acad-object)))
                           btrname)))
         (if (not (vl-catch-all-error-p btr))
           (vlax-for subent btr
             (setq chk (vl-catch-all-apply 'vla-get-ObjectName (list subent)))
             (if (and (not (vl-catch-all-error-p chk))
                      (= chk "AcDbBlockReference"))
               (setq items
                 (append items
                   (TKBD:ProcessInsert subent topent (1- depth))))
             )
           )
         )
       )
     )
    )
  )
  items
)

;; ---------------------------------------------------------------------
;; Thu thap du lieu tu selection set
;; Tra ve: (list data-list skip-count)
;; data item: (name kldv distance1 ename)
;; ---------------------------------------------------------------------
(defun TKBD:CollectData (ss / n i ent chk obj data skip items)
  (setq data '() skip 0)
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (setq chk (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        (if (vl-catch-all-error-p chk)
          (setq skip (1+ skip))
          (progn
            (setq obj chk)
            ;; De quy toi da 3 cap: bat duoc array, array long array,
            ;; block bao ngoai chua block dong...
            (setq items (TKBD:ProcessInsert obj ent 3))
            (if items
              (setq data (append data items))
              (setq skip (1+ skip))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (list data skip)
)

;; ---------------------------------------------------------------------
;; Gop nhom theo (NAME + Distance1) - cung ten, cung chieu dai -> 1 dong
;; Tra ve list cac group: (name kldv distance count enames-list)
;; KLDV lay gia tri xuat hien dau tien trong nhom (khong cong don).
;; ---------------------------------------------------------------------
(defun TKBD:RoundKey (val)
  ;; lam tron ve 3 chu so thap phan de tranh sai so so thuc khi gop nhom
  (rtos val 2 3)
)

(defun TKBD:GroupData (datalist / groups key found g name kldv dist ent)
  (setq groups '())
  (foreach it datalist
    (setq name (nth 0 it) kldv (nth 1 it) dist (nth 2 it) ent (nth 3 it))
    ;; Block khong co Distance1: gop theo NAME + KLDV (de khong tron
    ;; lan cac cau kien dem cai co don trong khac nhau)
    (setq key
      (if dist
        (strcat (strcase name) "|" (TKBD:RoundKey dist))
        (strcat (strcase name) "|NODIST|" (strcase kldv))
      )
    )
    (setq found (assoc key groups))
    (if found
      (progn
        (setq g (cdr found))
        ;; g = (name kldv dist count enames)
        (setq g (list (nth 0 g)
                      (nth 1 g)
                      (nth 2 g)
                      (1+ (nth 3 g))
                      (cons ent (nth 4 g))))
        (setq groups (subst (cons key g) found groups))
      )
      (setq groups (cons (cons key (list name kldv dist 1 (list ent))) groups))
    )
  )
  (vl-sort (mapcar 'cdr groups)
    '(lambda (a b / da db)
       (setq da (if (nth 2 a) (nth 2 a) -1.0)
             db (if (nth 2 b) (nth 2 b) -1.0))
       (if (= (strcase (nth 0 a)) (strcase (nth 0 b)))
         (< da db)
         (< (strcase (nth 0 a)) (strcase (nth 0 b)))
       )
     )
  )
)

;; ---------------------------------------------------------------------
;; Highlight / bo highlight danh sach ename
;; ---------------------------------------------------------------------
(defun TKBD:HighlightGroup (enames flag / e chk)
  (foreach e enames
    (setq chk (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
    (if (not (vl-catch-all-error-p chk))
      (vl-catch-all-apply 'vla-Highlight (list chk flag))
    )
  )
)

;; ---------------------------------------------------------------------
;; Ghi file DCL ra thu muc TEMP (nham dam bao file .lsp giao duoc doc lap)
;; ---------------------------------------------------------------------
(defun TKBD:WriteDCL (/ path f)
  (setq path (strcat (getenv "TEMP") "\\tkbd_" (rtos (getvar "MILLISECS") 2 0) ".dcl"))
  (setq f (open path "w"))
  (write-line "tkbd_dlg : dialog {" f)
  (write-line "  label = \"THKL v1.7.1 - Tong Hop Khoi Luong Block Dong (Tong CD don vi m)\";" f)
  (write-line "  : list_box {" f)
  (write-line "    key = \"list_data\";" f)
  (write-line "    width = 80;" f)
  (write-line "    height = 18;" f)
  (write-line "    fixed_width_font = true;" f)
  (write-line "  }" f)
  (write-line "  : toggle {" f)
  (write-line "    key = \"tgl_mm2m\";" f)
  (write-line "    label = \"Distance1 don vi mm -> doi sang m khi tinh KL (KLDV = kg/m)\";" f)
  (write-line "  }" f)
  (write-line "  : text {" f)
  (write-line "    key = \"txt_warn\";" f)
  (write-line "    width = 70;" f)
  (write-line "    value = \"\";" f)
  (write-line "  }" f)
  (write-line "  spacer;" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"btn_select\"; label = \"Chon lai doi tuong <\"; width = 22; }" f)
  (write-line "    : button { key = \"btn_export\"; label = \"Xuat CSV...\"; width = 14; }" f)
  (write-line "  }" f)
  (write-line "  spacer;" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  path
)

;; ---------------------------------------------------------------------
;; Dua du lieu vao list_box (header + data + tong cong)
;; ---------------------------------------------------------------------
(defun TKBD:PopulateList (groups / lst g total-cnt total-kl total-cd kl totdist bad)
  (setq lst (list (TKBD:FormatRow "TEN CAU KIEN" "KLDV" "CD 1CK" "TONG CD(m)" "SL" "TONG KL")))
  (setq total-cnt 0 total-kl 0.0 total-cd 0.0 bad 0)
  (foreach g groups
    (setq kl (TKBD:CalcKL (nth 2 g) (nth 1 g) (nth 3 g)))
    (if kl
      (setq total-kl (+ total-kl kl))
      (progn (setq bad (1+ bad)) (setq kl "---"))
    )
    ;; Tong CD quy doi ra m (chia 1000 neu toggle mm->m dang bat)
    (setq totdist
      (if (nth 2 g)
        (if (= *TKBD-MM2M* "1")
          (/ (* (nth 2 g) (nth 3 g)) 1000.0)
          (* (nth 2 g) (nth 3 g))
        )
        nil
      )
    )
    (if totdist (setq total-cd (+ total-cd totdist)))
    (setq lst (append lst
      (list (TKBD:FormatRow (nth 0 g) (nth 1 g) (nth 2 g) totdist (nth 3 g) kl))))
    (setq total-cnt (+ total-cnt (nth 3 g)))
  )
  (setq lst (append lst
    (list (TKBD:FormatRow "TONG CONG" "" "" total-cd total-cnt total-kl))))
  (start_list "list_data")
  (mapcar 'add_list lst)
  (end_list)
  (set_tile "txt_warn"
    (if (> bad 0)
      (strcat "Canh bao: " (itoa bad) " dong co KLDV khong phai so -> KL = ---")
      ""
    )
  )
)

;; ---------------------------------------------------------------------
;; Xu ly khi chon 1 dong trong danh sach: highlight nhom tuong ung
;; Dong 0 = header, dong cuoi = tong cong -> bo qua
;; ---------------------------------------------------------------------
(defun TKBD:OnSelect (idx / n g)
  (if *TKBD-CurHL*
    (progn (TKBD:HighlightGroup *TKBD-CurHL* nil) (setq *TKBD-CurHL* nil))
  )
  (setq n (length *TKBD-Groups*))
  (if (and (>= idx 1) (<= idx n))
    (progn
      (setq g (nth (1- idx) *TKBD-Groups*))
      (setq *TKBD-CurHL* (nth 4 g))
      (TKBD:HighlightGroup *TKBD-CurHL* T)
    )
  )
  (princ)
)

;; ---------------------------------------------------------------------
;; Xuat CSV
;; ---------------------------------------------------------------------
(defun TKBD:CSVField (s)
  (if (or (vl-string-search "," s) (vl-string-search "\"" s))
    (strcat "\"" (vl-string-subst "\"\"" "\"" s) "\"")
    s
  )
)

(defun TKBD:DoExport (groups / fname f g kl total-kl totdist total-cd)
  (setq fname
    (getfiled "Xuat file CSV thong ke Block Dong"
      (strcat (getvar "DWGPREFIX") "ThongKe_BlockDong.csv")
      "csv" 1
    )
  )
  (if fname
    (progn
      (setq f (open fname "w"))
      (write-line "Ten cau kien,So luong,CD 1 cau kien,Tong chieu dai (m),KLDV,Tong KL (kg)" f)
      (setq total-kl 0.0 total-cd 0.0)
      (foreach g groups
        (setq kl (TKBD:CalcKL (nth 2 g) (nth 1 g) (nth 3 g)))
        (if kl (setq total-kl (+ total-kl kl)))
        (setq totdist
          (if (nth 2 g)
            (if (= *TKBD-MM2M* "1")
              (/ (* (nth 2 g) (nth 3 g)) 1000.0)
              (* (nth 2 g) (nth 3 g))
            )
            nil
          )
        )
        (if totdist (setq total-cd (+ total-cd totdist)))
        (write-line
          (strcat
            (TKBD:CSVField (nth 0 g)) ","
            (itoa (nth 3 g)) ","
            (if (nth 2 g) (rtos (nth 2 g) 2 2) "---") ","
            (if totdist (rtos totdist 2 2) "---") ","
            (TKBD:CSVField (nth 1 g)) ","
            (if kl (rtos kl 2 2) "---")
          )
          f
        )
      )
      (write-line
        (strcat "TONG CONG,,," (rtos total-cd 2 2) ",," (rtos total-kl 2 2)) f)
      (close f)
      (setq *TKBD-LastCSV* fname)
      (alert (strcat "Da xuat file:\n" fname))
    )
  )
  (princ)
)

;; ---------------------------------------------------------------------
;; Lenh chinh: TKBD
;; ---------------------------------------------------------------------
(defun c:THKL (/ *error* olderr ss coldata skip groups dcl-path dclid code)

  (setq olderr *error*)
  (defun *error* (msg)
    (if *TKBD-CurHL*
      (progn (TKBD:HighlightGroup *TKBD-CurHL* nil) (setq *TKBD-CurHL* nil))
    )
    (if (and dclid (> dclid 0)) (unload_dialog dclid))
    (if (and dcl-path (findfile dcl-path)) (vl-file-delete dcl-path))
    (setq *error* olderr)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort"))
      (princ (strcat "\nLoi TKBD: " msg))
    )
    (princ)
  )

  (setq *TKBD-CurHL* nil)
  (setq code 2)

  (while (= code 2)
    (setq dcl-path nil dclid nil)
    (princ "\nChon cac block dong can thong ke (NAME, KLDV, Distance1): ")
    (setq ss (ssget '((0 . "INSERT"))))
    (cond
      ((not ss)
       (princ "\nKhong co doi tuong nao duoc chon.")
       (setq code 0)
      )
      (t
       (setq coldata (TKBD:CollectData ss))
       (setq skip (cadr coldata))
       (setq coldata (car coldata))
       (cond
         ((not coldata)
          (alert "Khong tim thay block dong hop le nao co du du lieu (NAME + Distance1).")
          (setq code 0)
         )
         (t
          (if (> skip 0)
            (princ (strcat "\n(Bo qua " (itoa skip) " doi tuong khong hop le.)"))
          )
          (setq groups (TKBD:GroupData coldata))
          (setq *TKBD-Groups* groups)
          (setq dcl-path (TKBD:WriteDCL))
          (setq dclid (load_dialog dcl-path))
          (if (not (new_dialog "tkbd_dlg" dclid))
            (progn
              (alert "Khong the tao hop thoai TKBD.")
              (setq code 0)
            )
            (progn
              (set_tile "tgl_mm2m" *TKBD-MM2M*)
              (TKBD:PopulateList groups)
              (action_tile "list_data" "(TKBD:OnSelect (atoi $value))")
              (action_tile "tgl_mm2m"
                "(setq *TKBD-MM2M* $value) (TKBD:PopulateList *TKBD-Groups*)")
              (action_tile "btn_select" "(done_dialog 2)")
              (action_tile "btn_export" "(TKBD:DoExport groups)")
              (action_tile "accept" "(done_dialog 1)")
              (action_tile "cancel" "(done_dialog 0)")
              (setq code (start_dialog))
              (unload_dialog dclid)
              (setq dclid nil)
              (if (findfile dcl-path) (vl-file-delete dcl-path))
              (setq dcl-path nil)
              (if *TKBD-CurHL*
                (progn (TKBD:HighlightGroup *TKBD-CurHL* nil) (setq *TKBD-CurHL* nil))
              )
             )
          )
         )
       )
      )
    )
  )

  (*error* nil)
  (princ "\nTKBD hoan tat.")
  (princ)
)

(princ "\nDa nap THKL.LSP v1.7.1 (fix loi TKBD:PARSEKLDV) - Go lenh THKL de thong ke block dong (NAME/KLDV/Distance1).")
(princ)