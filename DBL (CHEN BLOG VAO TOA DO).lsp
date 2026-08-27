;;; ================================================================
;;; DBL - Chen hang loat block theo toa do tu file CSV (BAN DCL v3.1 - Da sua rtos)
;;; *** SUA LOI v3 ***
;;;   - Sua loi khong nhan file: bo kiem tra bang findfile (hay truot
;;;     voi duong dan co dau tieng Viet / khoang trang), thay bang
;;;     mo doc file truc tiep
;;;   - Them BANG XEM TRUOC toa do ngay tren hop thoai:
;;;     chon file xong la thay ngay danh sach diem + tong so diem,
;;;     truoc khi bam OK
;;; Giao dien:
;;;   - Chon block: danh sach so xuong hoac nut "Pick <" ngoai man hinh
;;;   - Chon file CSV: o duong dan + nut "Chon file..."
;;;   - Bang xem truoc: STT, X, Y, Z tung diem + tong so diem
;;;   - Ty le chen + goc xoay (do), kiem tra hop le tai cho
;;;   - Nho lai toan bo lua chon cho lan chay sau
;;; File CSV: moi dong X,Y hoac X,Y,Z - nhan ca "," lan ";"
;;; Cach dung: go lenh DBL
;;; ================================================================

(vl-load-com)

(if (null *dbl-scale*) (setq *dbl-scale* "1"))
(if (null *dbl-rot*)   (setq *dbl-rot*   "0"))
(if (null *dbl-file*)  (setq *dbl-file*  ""))
(if (null *dbl-idx*)   (setq *dbl-idx*   0))
(if (null *dbl-swap*)  (setq *dbl-swap*  "0"))
(if (null *dbl-label*) (setq *dbl-label* "0"))
(if (null *dbl-style*) (setq *dbl-style* ""))
(if (null *dbl-th*)    (setq *dbl-th*    ""))

;; ---------- Tach chuoi theo ky tu phan cach ----------
(defun dbl:str-to-list (str del / pos lst)
  (while (setq pos (vl-string-search del str))
    (setq lst (cons (substr str 1 pos) lst))
    (setq str (substr str (+ pos (strlen del) 1)))
  )
  (reverse (cons str lst))
)

;; ---------- Danh sach ten block "that" trong ban ve ----------
(defun dbl:all-blknames (/ blk name flags lst)
  (while (setq blk (tblnext "BLOCK" (not blk)))
    (setq name  (cdr (assoc 2 blk))
          flags (cdr (assoc 70 blk)))
    (if (and (/= (substr name 1 1) "*")
             (= 0 (logand 4 flags)))
      (setq lst (cons name lst))
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Doc file CSV -> danh sach diem ((x y z) ...) ----------
;; *** v3: mo file truc tiep, khong dung findfile ***
;; Tra ve nil neu khong mo duoc file HOAC file khong co dong toa do nao
(defun dbl:read-csv (path / fh line cells pts)
  (setq pts '())
  (if (and path (/= path "") (setq fh (open path "r")))
    (progn
      (while (setq line (read-line fh))
        (setq line (vl-string-translate ";" "," line))
        (setq cells (mapcar '(lambda (s) (vl-string-trim " \"" s))
                            (dbl:str-to-list line ",")))
        (if (and (>= (length cells) 2)
                 (numberp (distof (nth 0 cells)))
                 (numberp (distof (nth 1 cells))))
          (setq pts
            (cons (list (distof (nth 0 cells))
                        (distof (nth 1 cells))
                        (if (and (nth 2 cells) (numberp (distof (nth 2 cells))))
                          (distof (nth 2 cells))
                          0.0))
                  pts))
        )
      )
      (close fh)
      (reverse pts)
    )
    nil
  )
)

;; ---------- Hoan doi X <-> Y neu bat tuy chon dao cot ----------
(defun dbl:swap-pts (pts swapstr)
  (if (= swapstr "1")
    (mapcar '(lambda (p) (list (cadr p) (car p) (caddr p))) pts)
    pts
  )
)

;; ---------- Do day bang xem truoc toa do tren hop thoai ----------
(defun dbl:fill-preview (pts / i)
  (start_list "preview")
  (setq i 0)
  (foreach p pts
    (setq i (1+ i))
    (add_list (strcat (itoa i) ")   X= " (rtos (car p) 2 3)
                      "    Y= " (rtos (cadr p) 2 3)
                      "    Z= " (rtos (caddr p) 2 3)))
  )
  (end_list)
  (set_tile "cnt"
    (if pts
      (strcat "Tong: " (itoa (length pts)) " diem se duoc chen block.")
      "Chua doc duoc toa do nao - hay chon file CSV."
    )
  )
)

;; ---------- Danh sach Text Style trong ban ve (bo file shape) ----------
(defun dbl:all-styles (/ sty name flags lst)
  (while (setq sty (tblnext "STYLE" (not sty)))
    (setq name  (cdr (assoc 2 sty))
          flags (cdr (assoc 70 sty)))
    (if (and (/= name "")
             (= 0 (logand 1 flags)))    ; bo shape file
      (setq lst (cons name lst))
    )
  )
  (vl-sort lst '(lambda (a b) (< (strcase a) (strcase b))))
)

;; ---------- Chieu cao chu ghi chu (theo DIMTXT x DIMSCALE) ----------
(defun dbl:text-h (/ h)
  (setq h (* (getvar "DIMTXT")
             (if (> (getvar "DIMSCALE") 0) (getvar "DIMSCALE") 1.0)))
  (if (<= h 0) (setq h 2.5))
  h
)

;; ---------- Ghi nhan toa do co LEADER chi vao diem ----------
;; Leader: tu diem chen keo xien 45 do len tren-phai, them doan ngang,
;; MTEXT "X=... / Y=..." dat cuoi doan ngang (giong ghi chu trac dia)
;; tstyle: ten text style; thstr: cao chu (chuoi rong = tu dong theo DIMTXT)
(defun dbl:make-label (p tstyle thstr / h d hseg gap end1 end2)
  (setq h (if (and (/= thstr "") (distof thstr) (> (distof thstr) 0.0))
            (distof thstr)
            (dbl:text-h))
        d    (* 3.0 h)      ; do dai doan xien 45 do
        hseg (* 1.5 h)      ; do dai doan ngang
        gap  (* 0.4 h)      ; khoang ho tu leader den chu
        end1 (list (+ (car p) d) (+ (cadr p) d) (caddr p))
        end2 (list (+ (car end1) hseg) (cadr end1) (caddr p))
  )
  ;; Leader co mui ten, khong annotation
  (command "_.LEADER" "_none" p "_none" end1 "_none" end2 "" "" "_N")
  ;; MTEXT 2 dong, canh giua-trai, dat sau doan ngang
  ;; *** ĐÃ SỬA: (rtos ... 2 2) thành (rtos ... 2 3) để hiển thị 3 số thập phân ***
  (entmakex
    (append
      (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
            (cons 10 (list (+ (car end2) gap) (cadr end2) (caddr p)))
            (cons 40 h)
            '(71 . 4)                                 ; MiddleLeft
            (cons 1 (strcat "X=" (rtos (car p) 2 3)
                            "\\P" "Y=" (rtos (cadr p) 2 3)))
      )
      (if (and tstyle (/= tstyle "") (tblsearch "STYLE" tstyle))
        (list (cons 7 tstyle))                        ; font theo style da chon
        nil
      )
    )
  )
)

;; ---------- Chen block theo danh sach diem ----------
(defun dbl:insert-pts (blkName pts sc rot labstr tstyle thstr / doc ms count old-cmdecho)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        ms  (vla-get-ModelSpace doc)
        count 0
        old-cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (vla-StartUndoMark doc)
  (foreach p pts
    (vla-InsertBlock ms (vlax-3d-point p) blkName sc sc sc (* pi (/ rot 180.0)))
    (if (= labstr "1") (dbl:make-label p tstyle thstr)) ; ghi toa do + leader neu tick
    (setq count (1+ count))
  )
  (vla-EndUndoMark doc)
  (setvar "CMDECHO" old-cmdecho)
  count
)

;; ---------- Tao file DCL tam ----------
(defun dbl:make-dcl (/ fname f)
  (setq fname (vl-filename-mktemp "dbl" nil ".dcl")
        f     (open fname "w"))
  (foreach line
    '("dbl : dialog {"
      "  label = \"DBL - Chen block hang loat tu file CSV\";"
      "  : boxed_column {"
      "    label = \"Block can chen\";"
      "    : row {"
      "      : popup_list { key = \"blklist\"; edit_width = 32; }"
      "      : button { key = \"pick\"; label = \"Pick <\"; width = 10; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"File toa do (CSV: X,Y hoac X,Y,Z - nhan ca dau ; )\";"
      "    : row {"
      "      : edit_box { key = \"file\"; edit_width = 38; }"
      "      : button { key = \"browse\"; label = \"Chon file...\"; width = 12; }"
      "    }"
      "  }"
      "  : boxed_column {"
      "    label = \"Xem truoc toa do\";"
      "    : toggle { key = \"swap\"; label = \"Dao cot X <-> Y (file trac dia dang N,E)\"; }"
      "    : toggle { key = \"label\"; label = \"Ghi toa do X=, Y= kem leader chi vao diem\"; }"
      "    : row {"
      "      : popup_list { key = \"tstyle\"; label = \"Font (Text Style) :\"; edit_width = 18; }"
      "      : edit_box { key = \"theight\"; label = \"Cao chu :\"; edit_width = 8; }"
      "    }"
      "    : text { label = \"(De trong Cao chu = tu dong theo DIMTXT x DIMSCALE)\"; }"
      "    : list_box { key = \"preview\"; width = 55; height = 10; }"
      "    : text { key = \"cnt\"; label = \"\"; width = 55; }"
      "  }"
      "  : row {"
      "    : edit_box { key = \"scale\"; label = \"Ty le chen :\"; edit_width = 10; }"
      "    : edit_box { key = \"rot\";   label = \"Goc xoay (do) :\"; edit_width = 10; }"
      "  }"
      "  spacer;"
      "  : text { key = \"err\"; label = \"\"; width = 55; }"
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
(defun c:DBL (/ dclfile dclid allnames done ret idx fpath scstr rotstr
                sc rot sel ent name blkName count pts swapstr labstr
                stylist sidx thstr)
  (setq allnames (dbl:all-blknames))
  (if (not allnames)
    (prompt "\n*** Ban ve khong co block nao! Hay dung lenh LDB nap block truoc. ***")
    (progn
      (setq dclfile (dbl:make-dcl)
            idx     (if (< *dbl-idx* (length allnames)) *dbl-idx* 0)
            fpath   *dbl-file*
            scstr   *dbl-scale*
            rotstr  *dbl-rot*
            swapstr *dbl-swap*
            labstr  *dbl-label*
            thstr   *dbl-th*
            stylist (dbl:all-styles)
            sidx    (cond
                      ((vl-position *dbl-style* stylist))
                      ((vl-position (getvar "TEXTSTYLE") stylist))
                      (0)
                    )
            pts     (dbl:read-csv *dbl-file*)   ; doc san file lan truoc (neu co)
            done    nil)

      ;; Vong lap hop thoai (dong tam de Pick / Browse roi mo lai)
      (while (not done)
        (setq dclid (load_dialog dclfile))
        (if (not (new_dialog "dbl" dclid))
          (progn (prompt "\n*** Loi: khong mo duoc hop thoai DCL! ***")
                 (setq done T ret 0))
          (progn
            (start_list "blklist")
            (mapcar 'add_list allnames)
            (end_list)
            (set_tile "blklist" (itoa idx))
            (set_tile "file"  fpath)
            (set_tile "scale" scstr)
            (set_tile "rot"   rotstr)
            (set_tile "swap"  swapstr)
            (set_tile "label" labstr)
            (start_list "tstyle")
            (mapcar 'add_list stylist)
            (end_list)
            (set_tile "tstyle" (itoa sidx))
            (set_tile "theight" thstr)
            (dbl:fill-preview (dbl:swap-pts pts swapstr))

            (action_tile "blklist" "(setq idx (atoi $value))")
            (action_tile "label" "(setq labstr $value)")
            (action_tile "tstyle" "(setq sidx (atoi $value))")
            (action_tile "theight" "(setq thstr $value)")
            ;; Tick dao cot -> cap nhat xem truoc ngay lap tuc
            (action_tile "swap"
              "(setq swapstr $value) (dbl:fill-preview (dbl:swap-pts pts swapstr))")
            ;; Go duong dan tay + Enter -> doc va xem truoc ngay
            (action_tile "file"
              "(setq fpath $value pts (dbl:read-csv fpath)) (dbl:fill-preview (dbl:swap-pts pts swapstr))")
            (action_tile "scale"   "(setq scstr $value)")
            (action_tile "rot"     "(setq rotstr $value)")
            (action_tile "pick"    "(done_dialog 2)")
            (action_tile "browse"  "(done_dialog 3)")
            ;; *** v3: kiem tra bang cach DOC THAT file, khong dung findfile ***
            (action_tile "accept"
              (strcat
                "(setq fpath (get_tile \"file\")"
                "      scstr (get_tile \"scale\")"
                "      rotstr (get_tile \"rot\")"
                "      thstr (get_tile \"theight\")"
                "      pts (dbl:read-csv fpath))"
                "(dbl:fill-preview (dbl:swap-pts pts swapstr))"
                "(cond"
                "  ((not pts)"
                "   (set_tile \"err\" \"*** Khong doc duoc file hoac file khong co toa do hop le! ***\"))"
                "  ((or (not (distof scstr)) (<= (distof scstr) 0.0))"
                "   (set_tile \"err\" \"*** Ty le phai la so > 0! ***\"))"
                "  ((not (distof rotstr))"
                "   (set_tile \"err\" \"*** Goc xoay phai la so! ***\"))"
                "  ((and (= \"1\" (get_tile \"label\"))"
                "        (/= (get_tile \"theight\") \"\")"
                "        (or (not (distof (get_tile \"theight\")))"
                "            (<= (distof (get_tile \"theight\")) 0.0)))"
                "   (set_tile \"err\" \"*** Cao chu phai la so > 0 (hoac de trong = tu dong)! ***\"))"
                "  (t (done_dialog 1))"
                ")"
              )
            )
            (action_tile "cancel" "(done_dialog 0)")

            (setq ret (start_dialog))
            (unload_dialog dclid)

            (cond
              ;; --- Pick block mau tren man hinh ---
              ((= ret 2)
               (setq sel (entsel "\nChon 1 block mau tren ban ve: "))
               (if (and sel (= "INSERT" (cdr (assoc 0 (entget (car sel))))))
                 (progn
                   (setq ent  (car sel)
                         name (if (vlax-property-available-p
                                    (vlax-ename->vla-object ent) 'EffectiveName)
                                (vla-get-EffectiveName (vlax-ename->vla-object ent))
                                (vla-get-Name (vlax-ename->vla-object ent))))
                   (if (vl-position name allnames)
                     (setq idx (vl-position name allnames))
                   )
                 )
                 (prompt "\n* Khong chon duoc block - giu nguyen lua chon cu. *")
               )
              )
              ;; --- Browse file CSV -> doc luon de xem truoc ---
              ((= ret 3)
               (setq fpath (getfiled "Chon file CSV chua toa do" fpath "csv" 4))
               (if (not fpath) (setq fpath *dbl-file*))
               (setq pts (dbl:read-csv fpath))
              )
              ;; --- OK / Cancel -> thoat vong lap ---
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
                sc      (distof scstr)
                rot     (distof rotstr)
                *dbl-idx*   idx
                *dbl-file*  fpath
                *dbl-scale* scstr
                *dbl-rot*   rotstr
                *dbl-swap*  swapstr
                *dbl-label* labstr
                *dbl-style* (nth sidx stylist)
                *dbl-th*    thstr)
          (setq count (dbl:insert-pts blkName (dbl:swap-pts pts swapstr) sc rot
                                      labstr (nth sidx stylist) thstr))
          (prompt (strcat "\n==> Da chen " (itoa count) " block \"" blkName
                          "\" (ty le " scstr ", xoay " rotstr " do"
                          (if (= swapstr "1") ", DA DAO COT X<->Y" "")
                          (if (= labstr "1") ", co ghi toa do tai diem" "")
                          ")."))
        )
        (prompt "\n* Da huy lenh. *")
      )
    )
  )
  (princ)
)

(prompt "\nDa nap lenh DBL v3.1 - Da cap nhat rtos hien thi 3 so thap phan.")
(princ)