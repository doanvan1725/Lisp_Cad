;;; ============================================================
;;; GOM.LSP - GOM NHIEU TEXT / MTEXT THANH 1 MTEXT DUY NHAT - v1
;;; Lenh: GOM
;;;
;;; Cach dung: go GOM -> chinh tuy chon trong hop thoai -> quet chon
;;; ca cum ghi chu (tieu de + cac dong) -> tat ca duoc dong goi thanh
;;; MOT MTEXT duy nhat, giu nguyen vi tri hien thi cua ca cum.
;;;
;;; Tinh nang:
;;;   - Nhan ca TEXT (DTEXT) lan MTEXT trong tap chon.
;;;   - Tu sap xep cac dong theo thu tu TREN -> DUOI.
;;;   - Cac text nam CUNG HANG (lech dung trong dung sai) duoc noi
;;;     thanh 1 dong, theo thu tu trai -> phai (tuy chon).
;;;   - Dong co chieu cao chu KHAC chieu cao chung (vd tieu de to hon)
;;;     duoc giu nguyen bang ma \H<ti le>x trong MTEXT.
;;;   - Gian dong cua MTEXT tu tinh theo khoang cach dong thuc te
;;;     (hoac nhap he so trong hop thoai).
;;;   - Khoang cach giua 2 dong lon bat thuong -> chen them dong trong
;;;     (tuy chon) de giu bo cuc doan.
;;;   - TEXT: tu doi ma %% (%%d %%p %%c %%u %%o %%%%) sang ma MTEXT,
;;;     escape ky tu dac biet \ { }. MTEXT: giu nguyen noi dung.
;;;   - Vi tri MTEXT = goc tren-trai khung bao ca cum -> khong xe dich.
;;;   - Layer / mau / text style lay theo doi tuong tren cung.
;;;
;;; Gioi han: cac text nen co goc xoay 0 (ghi chu thong thuong).
;;; ============================================================

(vl-load-com)

;; Bien luu tuy chon theo session
(if (null *gom-del*)   (setq *gom-del* "1"))   ; xoa doi tuong goc
(if (null *gom-same*)  (setq *gom-same* "1"))  ; gop text cung hang
(if (null *gom-blank*) (setq *gom-blank* "1")) ; chen dong trong neu gap lon
(if (null *gom-hmode*) (setq *gom-hmode* "hcom")) ; hcom | hfirst | hcus
(if (null *gom-hcus*)  (setq *gom-hcus* ""))
(if (null *gom-lsp*)   (setq *gom-lsp* ""))    ; he so gian dong (trong = tu tinh)

;; ---------- Tien ich chuoi ----------
(defun gom:blank-p (s) (or (not s) (= (vl-string-trim " " s) "")))

;; Escape ky tu dac biet cua MTEXT: \ { }
(defun gom:esc (s / r i c)
  (setq r "" i 1)
  (repeat (strlen s)
    (setq c (substr s i 1))
    (cond
      ((= c "\\") (setq r (strcat r "\\\\")))
      ((= c "{")  (setq r (strcat r "\\{")))
      ((= c "}")  (setq r (strcat r "\\}")))
      (t          (setq r (strcat r c)))
    )
    (setq i (1+ i))
  )
  r
)

;; Doi ky hieu %% cua TEXT sang ma MTEXT (goi SAU gom:esc)
(defun gom:pct (s / r i n len c uOn oOn d1 d2 d3)
  (setq r "" i 1 len (strlen s) uOn nil oOn nil)
  (while (<= i len)
    (if (and (<= (+ i 1) len) (= (substr s i 2) "%%"))
      (progn
        (setq c (if (<= (+ i 2) len) (strcase (substr s (+ i 2) 1)) ""))
        (cond
          ((= c "D") (setq r (strcat r "\\U+00B0")) (setq i (+ i 3)))
          ((= c "P") (setq r (strcat r "\\U+00B1")) (setq i (+ i 3)))
          ((= c "C") (setq r (strcat r "\\U+2205")) (setq i (+ i 3)))
          ((= c "U")
           (setq r (strcat r (if uOn "\\l" "\\L")))
           (setq uOn (not uOn)) (setq i (+ i 3)))
          ((= c "O")
           (setq r (strcat r (if oOn "\\o" "\\O")))
           (setq oOn (not oOn)) (setq i (+ i 3)))
          ((= c "%") (setq r (strcat r "%")) (setq i (+ i 3)))
          ((and (<= (+ i 4) len)
                (setq d1 (substr s (+ i 2) 1))
                (setq d2 (substr s (+ i 3) 1))
                (setq d3 (substr s (+ i 4) 1))
                (member d1 '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                (member d2 '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                (member d3 '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                (setq n (atoi (substr s (+ i 2) 3)))
                (> n 0))
           (setq r (strcat r (chr n))) (setq i (+ i 5)))
          (t (setq r (strcat r "%%")) (setq i (+ i 2)))
        )
      )
      (progn (setq r (strcat r (substr s i 1))) (setq i (1+ i)))
    )
  )
  (if uOn (setq r (strcat r "\\l")))
  (if oOn (setq r (strcat r "\\o")))
  r
)

;; ---------- Khung bao WCS cua 1 doi tuong ----------
(defun gom:bbox (ent / obj mn mx r)
  (setq obj (vlax-ename->vla-object ent))
  (setq r (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx)))
  (if (not (vl-catch-all-error-p r))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
    nil
  )
)

;; ---------- Noi dung MTEXT-ready cua 1 doi tuong ----------
(defun gom:content (ent / ed ty)
  (setq ed (entget ent) ty (cdr (assoc 0 ed)))
  (cond
    ((= ty "TEXT")  (gom:pct (gom:esc (cdr (assoc 1 ed)))))
    ((= ty "MTEXT") (vla-get-TextString (vlax-ename->vla-object ent)))
    (t "")
  )
)

;; ---------- Gia tri pho bien nhat trong danh sach so ----------
(defun gom:mode-num (lst / uniq u cnt best bestc)
  (setq uniq '())
  (foreach x lst
    (if (not (vl-some (function (lambda (u) (equal u x 1e-6))) uniq))
      (setq uniq (cons x uniq))
    )
  )
  (setq best (car uniq) bestc 0)
  (foreach u uniq
    (setq cnt (length (vl-remove-if-not
                        (function (lambda (x) (equal x u 1e-6))) lst)))
    (if (> cnt bestc) (setq best u bestc cnt))
  )
  best
)

;; ---------- Trung vi cua danh sach so ----------
(defun gom:median (lst / s n)
  (setq s (vl-sort lst '<) n (length s))
  (cond
    ((= n 0) nil)
    ((= (rem n 2) 1) (nth (/ n 2) s))
    (t (/ (+ (nth (/ n 2) s) (nth (1- (/ n 2)) s)) 2.0))
  )
)

;; ---------- Chuoi pho bien nhat trong danh sach ----------
(defun gom:mode-str (lst / uniq u cnt best bestc)
  (setq uniq '())
  (foreach x lst
    (if (not (member x uniq)) (setq uniq (cons x uniq)))
  )
  (setq best (car uniq) bestc 0)
  (foreach u uniq
    (setq cnt (length (vl-remove-if-not
                        (function (lambda (x) (= x u))) lst)))
    (if (> cnt bestc) (setq best u bestc cnt))
  )
  best
)

;; ---------- Phan tu pho bien nhat (so sanh equal) ----------
(defun gom:mode-eq (lst / uniq u cnt best bestc)
  (setq uniq '())
  (foreach x lst
    (if (not (vl-some (function (lambda (u) (equal u x))) uniq))
      (setq uniq (cons x uniq))
    )
  )
  (setq best (car uniq) bestc 0)
  (foreach u uniq
    (setq cnt (length (vl-remove-if-not
                        (function (lambda (x) (equal x u))) lst)))
    (if (> cnt bestc) (setq best u bestc cnt))
  )
  best
)

;; ---------- Hop thoai ----------
(defun gom:makedcl (/ fn f)
  (setq fn (vl-filename-mktemp "gom" nil ".dcl")
        f  (open fn "w"))
  (write-line "gom : dialog {" f)
  (write-line "  label = \"GOM - Dong goi nhieu Text thanh 1 MText - v1\";" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Tuy chon gop\";" f)
  (write-line "    : toggle { key=\"same\";  label=\"Noi cac text nam cung hang thanh 1 dong\"; }" f)
  (write-line "    : toggle { key=\"blank\"; label=\"Chen dong trong khi khoang cach dong lon bat thuong\"; }" f)
  (write-line "    : toggle { key=\"del\";   label=\"Xoa cac Text goc sau khi gom\"; }" f)
  (write-line "  }" f)
  (write-line "  : boxed_radio_column {" f)
  (write-line "    label = \"Chieu cao chu cua MText\";" f)
  (write-line "    : radio_button { key=\"hcom\";   label=\"Chieu cao PHO BIEN nhat trong cum (khuyen dung)\"; }" f)
  (write-line "    : radio_button { key=\"hfirst\"; label=\"Chieu cao cua dong TREN CUNG\"; }" f)
  (write-line "    : row {" f)
  (write-line "      : radio_button { key=\"hcus\"; label=\"Tu nhap:\"; }" f)
  (write-line "      : edit_box { key=\"hcusval\"; edit_width=10; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : text { label=\"He so gian dong (de trong = tu tinh theo khoang cach thuc te):\"; }" f)
  (write-line "    : edit_box { key=\"lsp\"; edit_width=8; }" f)
  (write-line "  }" f)
  (write-line "  : text { label=\"Dong khac chieu cao chung se duoc giu nguyen bang ma \\\\H..x.\"; width=62; }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key=\"accept\"; label=\"Chon doi tuong >\"; is_default=true; fixed_width=true; width=18; }" f)
  (write-line "    : button { key=\"cancel\"; label=\"Huy\"; is_cancel=true; fixed_width=true; width=12; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (close f)
  fn
)

;; ============================================================
;; LENH CHINH
;; ============================================================
(defun c:GOM (/ doc dclfile dclid ok del same blank hmode hcus lspin
                basePair baseC62 baseC420 lc62 lc420 codes
                ss n i ent items bb it tol baseH heights firstH
                lines cur curY sorted lineData lh ratio txtLine gaps g normal
                factor content parts insX insY ed0 lay sty lst new cntSrc
                itemsY topEnt)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; ----- Hop thoai tuy chon -----
  (setq dclfile (gom:makedcl) dclid (load_dialog dclfile))
  (if (not (new_dialog "gom" dclid))
    (progn (princ "\nKhong mo duoc hop thoai!") (exit))
  )
  (set_tile "del"   *gom-del*)
  (set_tile "same"  *gom-same*)
  (set_tile "blank" *gom-blank*)
  (set_tile *gom-hmode* "1")
  (set_tile "hcusval" *gom-hcus*)
  (set_tile "lsp" *gom-lsp*)
  (mode_tile "hcusval" (if (= *gom-hmode* "hcus") 0 1))
  (action_tile "hcom"   "(mode_tile \"hcusval\" 1)")
  (action_tile "hfirst" "(mode_tile \"hcusval\" 1)")
  (action_tile "hcus"   "(mode_tile \"hcusval\" 0)")
  (action_tile "accept"
    (strcat
      "(setq *gom-del* (get_tile \"del\")"
      "      *gom-same* (get_tile \"same\")"
      "      *gom-blank* (get_tile \"blank\")"
      "      *gom-hmode* (cond ((= (get_tile \"hfirst\") \"1\") \"hfirst\")"
      "                        ((= (get_tile \"hcus\") \"1\") \"hcus\")"
      "                        (t \"hcom\"))"
      "      *gom-hcus* (get_tile \"hcusval\")"
      "      *gom-lsp* (get_tile \"lsp\"))"
      "(done_dialog 1)"
    )
  )
  (action_tile "cancel" "(done_dialog 0)")
  (setq ok (start_dialog))
  (unload_dialog dclid)
  (vl-file-delete dclfile)
  (if (/= ok 1) (progn (princ "\nDa huy.") (exit)))

  (setq del   (= *gom-del* "1")
        same  (= *gom-same* "1")
        blank (= *gom-blank* "1")
        hmode *gom-hmode*
        hcus  (distof *gom-hcus* 2)
        lspin (distof *gom-lsp* 2)
  )
  (if (and (= hmode "hcus") (or (not hcus) (<= hcus 0)))
    (progn (princ "\nChieu cao tu nhap khong hop le.") (exit))
  )

  ;; ----- Chon doi tuong -----
  (setq ss (ssget "_I" '((0 . "TEXT,MTEXT"))))
  (if (or (null ss) (= (sslength ss) 0))
    (progn
      (princ "\nQuet chon ca cum TEXT/MTEXT can gom thanh 1 MText: ")
      (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    )
  )
  (if (null ss) (progn (princ "\nKhong co doi tuong nao duoc chon.") (exit)))

  ;; ----- Thu thap du lieu tung doi tuong -----
  ;; item = (topY leftX noi-dung chieu-cao ename mau62 mau420)
  ;;   mau62 : mau index (256 = ByLayer, 0 = ByBlock)
  ;;   mau420: true color 24-bit (nil neu khong co)
  (setq items '() n (sslength ss) i 0)
  (while (< i n)
    (setq ent (ssname ss i))
    (setq bb (gom:bbox ent))
    (if (and bb (not (gom:blank-p (gom:content ent))))
      (progn
        (setq ed0 (entget ent))
        (setq items
          (cons (list (cadr (cadr bb))            ; topY  = maxY
                      (car (car bb))              ; leftX = minX
                      (gom:content ent)
                      (cdr (assoc 40 ed0))
                      ent
                      (if (assoc 62 ed0) (cdr (assoc 62 ed0)) 256)
                      (if (assoc 420 ed0) (cdr (assoc 420 ed0)) nil))
                items))
      )
    )
    (setq i (1+ i))
  )
  (if (< (length items) 1)
    (progn (princ "\nKhong doc duoc noi dung doi tuong nao.") (exit))
  )

  ;; ----- Sap xep tren -> duoi -----
  (setq sorted (vl-sort items (function (lambda (a b) (> (car a) (car b))))))

  ;; ----- Chieu cao chung -----
  (setq heights (mapcar (function (lambda (x) (nth 3 x))) sorted))
  (setq firstH (car heights))
  (setq baseH
    (cond
      ((= hmode "hcus") hcus)
      ((= hmode "hfirst") firstH)
      (t (gom:mode-num heights))
    )
  )
  (if (or (not baseH) (<= baseH 0)) (setq baseH firstH))

  ;; ----- Gom cac item cung hang -----
  ;; dung sai theo phuong dung = 0.5 x chieu cao chung
  (setq tol (* 0.5 baseH))
  (setq lines '() cur (list (car sorted)) curY (car (car sorted)))
  (foreach it (cdr sorted)
    (if (and same (<= (abs (- curY (car it))) tol))
      (setq cur (cons it cur))
      (progn
        (setq lines (cons cur lines))
        (setq cur (list it) curY (car it))
      )
    )
  )
  (setq lines (cons cur lines))
  (setq lines (reverse lines))

  ;; ----- Du lieu tung dong: (topY noi-dung chieu-cao mau62 mau420) -----
  (setq lineData '())
  (foreach cur lines
    ;; trong 1 hang: sap trai -> phai roi noi bang khoang trang
    (setq cur (vl-sort cur (function (lambda (a b) (< (cadr a) (cadr b))))))
    (setq txtLine "")
    (foreach it cur
      (setq txtLine
        (if (= txtLine "") (nth 2 it) (strcat txtLine " " (nth 2 it))))
    )
    (setq lh (apply 'max (mapcar (function (lambda (x) (nth 3 x))) cur)))
    (setq itemsY (apply 'max (mapcar 'car cur)))
    (setq lineData (cons (list itemsY txtLine lh
                               (nth 5 (car cur))     ; mau62 cua dong
                               (nth 6 (car cur)))    ; mau420 cua dong
                         lineData))
  )
  (setq lineData (reverse lineData))

  ;; ----- Khoang cach dong chuan (trung vi) -----
  (setq gaps '())
  (setq i 0)
  (while (< i (1- (length lineData)))
    (setq g (- (car (nth i lineData)) (car (nth (1+ i) lineData))))
    (if (> g 1e-8) (setq gaps (cons g gaps)))
    (setq i (1+ i))
  )
  (setq normal (gom:median gaps))

  ;; ----- He so gian dong MTEXT: baseline = factor x 5/3 x chieu cao -----
  (setq factor
    (cond
      ((and lspin (> lspin 0)) lspin)
      ((and normal (> normal 0)) (/ normal (* baseH (/ 5.0 3.0))))
      (t 1.0)
    )
  )
  (if (< factor 0.25) (setq factor 0.25))
  (if (> factor 4.0)  (setq factor 4.0))

  ;; ----- *** v1.3: Mau entity MText LUON la ByLayer -----
  ;; Moi dong co mau rieng (index / true color) deu duoc giu nguyen
  ;; bang ma \C / \c nhung trong noi dung, nen so sanh voi ByLayer.
  (setq basePair (list 256 nil))
  (setq baseC62 (car basePair) baseC420 (cadr basePair))

  ;; ----- Ghep noi dung: \P giua cac dong; giu chieu cao \H..x; giu mau \C.. -----
  (setq content "" i 0)
  (foreach ld lineData
    (setq lh (caddr ld))
    (setq ratio (/ lh baseH))
    (setq lc62 (nth 3 ld) lc420 (nth 4 ld))
    ;; ma dinh dang rieng cua dong (neu khac chieu cao / mau chung)
    (setq codes "")
    (if (> (abs (- ratio 1.0)) 1e-3)
      (setq codes (strcat codes "\\H" (rtos ratio 2 4) "x;"))
    )
    (if (not (equal (list lc62 lc420) basePair))
      (cond
        (lc420
         (setq codes (strcat codes "\\c" (itoa lc420) ";")))
        ((and lc62 (>= lc62 1) (<= lc62 255))
         (setq codes (strcat codes "\\C" (itoa lc62) ";")))
        ;; ByLayer/ByBlock khac mau chung -> khong nhung duoc, de theo mau chung
      )
    )
    (setq txtLine
      (if (= codes "")
        (cadr ld)
        (strcat "{" codes (cadr ld) "}")
      )
    )
    (if (> i 0)
      (progn
        (setq content (strcat content "\\P"))
        ;; chen dong trong neu khoang cach lon bat thuong
        (if (and blank normal
                 (> (- (car (nth (1- i) lineData)) (car ld)) (* 1.6 normal)))
          (setq content (strcat content "\\P"))
        )
      )
    )
    (setq content (strcat content txtLine))
    (setq i (1+ i))
  )

  ;; ----- Diem chen = goc tren-trai khung bao ca cum -----
  (setq insX (apply 'min (mapcar 'cadr sorted)))
  (setq insY (apply 'max (mapcar 'car sorted)))

  ;; ----- Layer / style: GIU LAYER GOC -----
  ;; Lay layer PHO BIEN NHAT trong ca cum lam layer cua MText,
  ;; text style lay theo doi tuong dau tien thuoc layer do.
  ;; Mau cua MText = cap mau PHO BIEN NHAT (baseC62/baseC420) da tinh o tren;
  ;; dong nao khac mau da duoc giu bang ma \C/\c trong noi dung.
  (setq lay (gom:mode-str
              (mapcar (function (lambda (x) (cdr (assoc 8 (entget (nth 4 x))))))
                      sorted)))
  (setq topEnt nil)
  (foreach it sorted
    (if (and (not topEnt)
             (= (cdr (assoc 8 (entget (nth 4 it)))) lay))
      (setq topEnt (nth 4 it))
    )
  )
  (if (not topEnt) (setq topEnt (nth 4 (car sorted))))
  (setq ed0 (entget topEnt))
  (if (not lay) (setq lay (cdr (assoc 8 ed0))))
  (setq sty (if (assoc 7 ed0) (cdr (assoc 7 ed0)) (getvar "TEXTSTYLE")))

  ;; ----- Tao MTEXT -----
  (vla-StartUndoMark doc)
  (setq lst (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 lay)))
  ;; *** v1.3: khong ghi ma 62/420 -> mau entity = ByLayer.
  ;; (baseC62 = 256 nen 2 dieu kien duoi khong bao gio ghi mau)
  (if (and baseC62 (/= baseC62 256))
    (setq lst (append lst (list (cons 62 baseC62))))
  )
  (if baseC420
    (setq lst (append lst (list (cons 420 baseC420))))
  )
  (setq lst (append lst
              (list '(100 . "AcDbMText")
                    (cons 10 (list insX insY 0.0))
                    (cons 40 baseH)
                    (cons 41 0.0)     ; khong gioi han rong -> khong tu xuong dong
                    '(71 . 1)         ; Top-Left
                    (cons 7 sty)
                    (cons 1 content)
              )))
  (if (> (length lineData) 1)
    (setq lst (append lst (list '(73 . 2) (cons 44 factor))))  ; gian dong Exactly
  )
  (setq new (entmakex lst))

  (setq cntSrc (length items))
  (if new
    (progn
      (if del (foreach it items (entdel (nth 4 it))))
      (princ (strcat "\nDa gom " (itoa cntSrc) " doi tuong thanh 1 MText ("
                     (itoa (length lineData)) " dong)"
                     (if del ", da xoa doi tuong goc." ", giu lai doi tuong goc.")))
    )
    (princ "\nLoi: khong tao duoc MText.")
  )
  (vla-EndUndoMark doc)
  (princ)
)

;; ----- Lenh tat -----
(defun c:GM () (c:GOM))

(princ "\n=== GOM v1.3 da nap - Go GOM hoac GM: quet chon ca cum Text -> dong goi thanh 1 MText (mau entity ByLayer, giu mau rieng tung dong bang ma \\C). ===")
(princ)