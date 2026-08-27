;;; =================================================================
;;; UPCHU.lsp - UP TY LE CO CHU TRONG AUTOCAD (co giao dien)
;;; -----------------------------------------------------------------
;;; Lenh goi: UPCHU
;;; Cong thuc: Co chu moi = Co chu co ban x Ty le up
;;; Ho tro cac loai chu:
;;;   - TEXT (Dtext), MTEXT
;;;   - ATTDEF (dinh nghia thuoc tinh)
;;;   - Thuoc tinh trong block (INSERT co attribute)
;;;   - DIMENSION (chieu cao chu kich thuoc)
;;;   - MULTILEADER (chu tren mleader)
;;; Cho phep chon nhieu doi tuong cung luc; nho gia tri nhap lan truoc.
;;; Cai dat: APPLOAD -> chon file nay (them vao Startup Suite de tu nap)
;;; =================================================================

(vl-load-com)

;; ---------- Bo loc doi tuong chu ----------
(setq *upchu:filter* '((0 . "TEXT,MTEXT,ATTDEF,DIMENSION,MULTILEADER,INSERT")))

;; ---------- Tao file DCL tam ----------
(defun upchu:makedcl (/ fn f)
  (setq fn (vl-filename-mktemp "upchu" nil ".dcl")
        f  (open fn "w"))
  (foreach s
    '("upchu : dialog {"
      "  label = \"UP TY LE CO CHU\";"
      "  spacer;"
      "  : boxed_column { label = \"Thong so\";"
      "    : edit_box { key = \"base\";  label = \"Co chu co ban :\"; edit_width = 12; }"
      "    : edit_box { key = \"scale\"; label = \"Ty le up :\";      edit_width = 12; }"
      "    spacer;"
      "    : text { key = \"result\"; label = \"Co chu sau up : \"; width = 38; }"
      "  }"
      "  spacer;"
      "  : boxed_column { label = \"Doi tuong\";"
      "    : button { key = \"pick\"; label = \"< Chon chu tren man hinh >\"; }"
      "    : text { key = \"count\"; label = \"Da chon: 0 doi tuong\"; width = 38; }"
      "  }"
      "  spacer;"
      "  : row {"
      "    : button { key = \"accept\"; label = \"Ap dung\"; is_default = true; }"
      "    : button { key = \"cancel\"; label = \"Thoat\";   is_cancel  = true; }"
      "  }"
      "}")
    (write-line s f))
  (close f)
  fn)

;; ---------- Cap nhat dong "Co chu sau up" tren hop thoai ----------
(defun upchu:update (/ b s)
  (setq b (distof (get_tile "base"))
        s (distof (get_tile "scale")))
  (if (and b s (> b 0) (> s 0))
    (set_tile "result" (strcat "Co chu sau up : " (rtos (* b s) 2 4)))
    (set_tile "result" "Co chu sau up : (nhap so > 0)")))

;; ---------- Gan chieu cao moi cho 1 doi tuong, tra ve so chu da sua ----------
(defun upchu:seth (obj h / cnt)
  (setq cnt 0)
  (cond
    ;; TEXT / MTEXT / ATTDEF / ATTRIB dung thuoc tinh Height
    ((vlax-property-available-p obj 'Height T)
     (if (not (vl-catch-all-error-p
                (vl-catch-all-apply 'vla-put-Height (list obj h))))
         (setq cnt 1)))
    ;; DIMENSION / MULTILEADER dung thuoc tinh TextHeight
    ((vlax-property-available-p obj 'TextHeight T)
     (if (not (vl-catch-all-error-p
                (vl-catch-all-apply 'vla-put-TextHeight (list obj h))))
         (setq cnt 1))))
  ;; Block co thuoc tinh: sua tung attribute ben trong
  (if (and (= (vla-get-ObjectName obj) "AcDbBlockReference")
           (= (vla-get-HasAttributes obj) :vlax-true))
    (foreach a (vlax-invoke obj 'GetAttributes)
      (if (not (vl-catch-all-error-p
                 (vl-catch-all-apply 'vla-put-Height (list a h))))
          (setq cnt (1+ cnt)))))
  cnt)

;; ---------- Lenh chinh ----------
(defun C:UPCHU (/ doc dcl id done ss base scale newh n i obj)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Gia tri mac dinh = lan nhap truoc (bien toan cuc), lan dau: 2.5 va 1.0
  (setq base  (cond (*upchu:base*)  (2.5))
        scale (cond (*upchu:scale*) (1.0)))

  ;; Neu dang co doi tuong chon san (pickfirst) thi lay luon
  (setq ss (ssget "_I" *upchu:filter*))
  (if ss (sssetfirst nil nil))

  (setq dcl (upchu:makedcl)
        done 2)

  ;; Vong lap hop thoai: 1 = Ap dung, 0 = Thoat, 2 = tam dong de chon chu
  (while (= done 2)
    (setq id (load_dialog dcl))
    (if (not (new_dialog "upchu" id))
      (progn (princ "\nKhong mo duoc hop thoai DCL!") (setq done 0))
      (progn
        (set_tile "base"  (rtos base 2 4))
        (set_tile "scale" (rtos scale 2 4))
        (set_tile "count"
          (strcat "Da chon: " (itoa (if ss (sslength ss) 0)) " doi tuong"))
        (upchu:update)
        (action_tile "base"
          "(setq base (distof (get_tile \"base\"))) (upchu:update)")
        (action_tile "scale"
          "(setq scale (distof (get_tile \"scale\"))) (upchu:update)")
        (action_tile "pick" "(done_dialog 2)")
        (action_tile "accept"
          (strcat
            "(setq base (distof (get_tile \"base\"))"
            "      scale (distof (get_tile \"scale\")))"
            "(if (and base scale (> base 0) (> scale 0))"
            "    (if ss (done_dialog 1)"
            "           (alert \"Chua chon doi tuong chu nao!\\nBam nut < Chon chu tren man hinh >\"))"
            "    (alert \"Co chu co ban va Ty le up phai la so > 0 !\"))"))
        (setq done (start_dialog))
        (unload_dialog id)
        ;; Nguoi dung bam nut chon chu -> ra man hinh quet chon roi quay lai
        (if (= done 2)
          (progn
            (princ "\nChon cac chu can up ty le (quet chon nhieu doi tuong): ")
            (setq ss (ssget *upchu:filter*)))))))

  (vl-file-delete dcl)

  ;; Ap dung
  (if (= done 1)
    (progn
      (setq *upchu:base*  base
            *upchu:scale* scale
            newh (* base scale)
            n 0
            i 0)
      (vla-StartUndoMark doc)
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
              n   (+ n (upchu:seth obj newh))
              i   (1+ i)))
      (vla-EndUndoMark doc)
      (princ (strcat "\nDa up " (itoa n) " chu ve co "
                     (rtos newh 2 4)
                     "  ( = " (rtos base 2 4) " x " (rtos scale 2 4) " )"))))
  (princ))

(princ "\nDa nap UPCHU.lsp - Go lenh UPCHU de up ty le co chu.")
(princ)
