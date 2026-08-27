;;======================================================================
;; BLOCKTOOLS.LSP
;; Gom 2 lenh xu ly Attribute cua Block:
;;   SYNCATT - Dong bo (cap nhat) toan bo Attribute cho Block sau khi
;;             bo sung ATTDEF moi (dua tren -ATTSYNC cua AutoCAD)
;;   CPATT   - Copy/gan gia tri Attribute tang dan cho nhieu Block
;;             cung loai (chon Tag, chon huong, prefix, so bat dau)
;; Cach dung: APPLOAD file nay 1 lan, sau do go SYNCATT hoac CPATT
;;======================================================================

(vl-load-com)

;;----------------------------------------------------------------------
;; LENH 1: SYNCATT
;;----------------------------------------------------------------------
(defun SyncBlockAtt (blkName / )
  (if (tblsearch "BLOCK" blkName)
    (progn
      (command "_.-attsync" "_Name" blkName)
      (princ (strcat "\n>> Da dong bo: " blkName)))
    (princ (strcat "\n>> Khong tim thay block: " blkName)))
  (princ))

(defun c:SYNCATT (/ *error* oldEcho oldAttReq ss i ent obj blkName blkList)
  (defun *error* (msg)
    (setvar "CMDECHO" oldEcho)
    (setvar "ATTREQ" oldAttReq)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nLoi: " msg)))
    (princ))

  (setq oldEcho   (getvar "CMDECHO")
        oldAttReq (getvar "ATTREQ"))
  (setvar "CMDECHO" 0)

  (princ "\nChon 1 hoac nhieu Block-Reference can dong bo (Enter = nhap ten tay): ")
  (setq ss (ssget '((0 . "INSERT"))))

  (setq blkList nil i 0)
  (if ss
    (repeat (sslength ss)
      (setq ent (ssname ss i)
            obj (vlax-ename->vla-object ent)
            blkName (cdr (assoc 2 (entget ent))))
      ;; Block an danh (Dynamic Block) -> lay ten thuc te (EffectiveName)
      (if (wcmatch blkName "`**")
        (setq blkName (vla-get-effectivename obj)))
      (if (not (member blkName blkList))
        (setq blkList (cons blkName blkList)))
      (setq i (1+ i)))
    (progn
      (initget 1)
      (setq blkName (getstring T "\nNhap ten Block can dong bo: "))
      (setq blkList (list blkName))))

  (foreach b blkList (SyncBlockAtt b))

  (setvar "CMDECHO" oldEcho)
  (setvar "ATTREQ" oldAttReq)
  (princ (strcat "\n>> Hoan tat dong bo " (itoa (length blkList)) " block."))
  (princ)
)

;;----------------------------------------------------------------------
;; LENH 2: CPATT
;;----------------------------------------------------------------------

;; Lay list (tag . attref-object) cua 1 block reference
(defun GetAtts (obj / atts a lst)
  (setq lst nil)
  (if (vlax-property-available-p obj 'HasAttributes)
    (if (= (vla-get-hasattributes obj) :vlax-true)
      (progn
        (setq atts (vlax-safearray->list
                     (vlax-variant-value (vla-getattributes obj))))
        (foreach a atts
          (setq lst (cons (cons (strcase (vla-get-tagstring a)) a) lst)))
        (reverse lst)))
    nil)
  lst)

;; Ghi 1 file text ra dia, tra ve path neu thanh cong, nil neu that bai
(defun CPATT-WriteFile (path text / f)
  (setq f nil)
  (if (setq f (open path "w"))
    (progn (write-line text f) (close f) path)
    nil))

;; Tao file DCL chua 2 dialog: chon Che do/Huong (buoc 1) va Tag/Prefix/So (buoc 2)
;; Uu tien ghi vao TEMP, neu that bai (thuong do duong dan TEMP chua ky tu
;; tieng Viet/Unicode gay loi load_dialog) thi fallback sang goc o dia C.
(defun CPATT-WriteDCL ( / dclText p1 p2 result)
  (setq dclText (strcat
    "cpatt_mode_dlg : dialog { label = \"CPATT - Buoc 1: Chon che do\";"
    "  : boxed_radio_column { label = \"Cach lay doi tuong\";"
    "    : radio_button { key = \"mode_pick\"; label = \"Pick - chon lan luot tung block theo thu tu\"; }"
    "    : radio_button { key = \"mode_window\"; label = \"Window - quet chon vung roi tu sap xep\"; }"
    "  }"
    "  : boxed_radio_column { label = \"Huong sap xep (chi dung khi chon Window)\";"
    "    : radio_button { key = \"order_xtang\"; label = \"X tang dan\"; }"
    "    : radio_button { key = \"order_xgiam\"; label = \"X giam dan\"; }"
    "    : radio_button { key = \"order_ytang\"; label = \"Y tang dan\"; }"
    "    : radio_button { key = \"order_ygiam\"; label = \"Y giam dan\"; }"
    "    : radio_button { key = \"order_2diem\"; label = \"Pick 2 diem - huong bat ky\"; }"
    "  }"
    "  spacer;"
    "  ok_cancel;"
    "}"
    "cpatt_data_dlg : dialog { label = \"CPATT - Buoc 2: Thong tin gan Attribute\";"
    "  : popup_list { key = \"taglist\"; label = \"Chon Tag can tang:\"; }"
    "  : edit_box { key = \"prefix\"; label = \"Prefix:\"; edit_width = 15; }"
    "  : edit_box { key = \"start\"; label = \"So bat dau:\"; edit_width = 15; }"
    "  : edit_box { key = \"step\"; label = \"Buoc nhay:\"; edit_width = 15; }"
    "  spacer;"
    "  ok_cancel;"
    "}"))

  (setq p1 (strcat (getenv "TEMP") "\\cpatt_dlg.dcl"))
  (setq result (CPATT-WriteFile p1 dclText))
  (if (not result)
    (progn
      (setq p2 "C:\\cpatt_dlg.dcl")
      (setq result (CPATT-WriteFile p2 dclText))))
  result)

(defun c:CPATT (/ *error* dcl_id dclPath dlgOK mode order
                  ss i n ent obj attList tagList tagChon
                  prefix start step val entObjs idx doneFlag
                  e obj2 pair pt1 pt2 vecX vecY vecLen objPairs)
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*")))
      (princ (strcat "\nLoi: " msg)))
    (princ))

  ;;--- Chuan bi file DCL (dung chung cho ca 2 buoc) ---
  (setq dclPath (CPATT-WriteDCL))
  (if (not dclPath)
    (progn (princ "\n>> Khong the tao file DCL.") (exit)))

  ;;===================================================================
  ;; BUOC 1: hien dialog NGAY KHI VAO LENH - chon Mode + Huong sap xep
  ;;===================================================================
  (setq dcl_id (load_dialog dclPath))
  (if (or (not dcl_id) (< dcl_id 0))
    (progn (princ "\n>> Khong the load dialog (kiem tra duong dan co ky tu dac biet).") (exit)))

  (if (not (new_dialog "cpatt_mode_dlg" dcl_id))
    (progn (unload_dialog dcl_id) (princ "\n>> Khong the mo dialog buoc 1.") (exit)))

  (setq mode "Pick" order "Xtang")
  (set_tile "mode_pick"   "1")
  (set_tile "order_xtang" "1")

  (action_tile "mode_pick"    "(setq mode \"Pick\")")
  (action_tile "mode_window"  "(setq mode \"Window\")")
  (action_tile "order_xtang"  "(setq order \"Xtang\")")
  (action_tile "order_xgiam"  "(setq order \"Xgiam\")")
  (action_tile "order_ytang"  "(setq order \"Ytang\")")
  (action_tile "order_ygiam"  "(setq order \"Ygiam\")")
  (action_tile "order_2diem"  "(setq order \"2Diem\")")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (setq dlgOK (= (start_dialog) 1))
  (unload_dialog dcl_id)
  (if (not dlgOK)
    (progn (princ "\n>> Da huy lenh.") (exit)))

  ;;===================================================================
  ;; BUOC 2: thao tac tren man hinh - chon doi tuong / pick huong
  ;;===================================================================
  (setq entObjs nil)

  (cond
    ;;------------------ CHE DO PICK TUNG CAI THEO THU TU ------------------
    ((= mode "Pick")
     (princ "\nChon lan luot cac Block theo thu tu tang dan (Enter de ket thuc):")
     (setq doneFlag nil idx 0)
     (while (not doneFlag)
       (setq e (car (entsel (strcat "\nChon block thu " (itoa (1+ idx)) ": "))))
       (if e
         (progn
           (setq obj2 (vlax-ename->vla-object e))
           (if (and (= (vla-get-objectname obj2) "AcDbBlockReference")
                    (= (vla-get-hasattributes obj2) :vlax-true))
             (progn
               (setq entObjs (append entObjs (list obj2)))
               (setq idx (1+ idx))
               (princ (strcat " -> OK (" (itoa idx) ")")))
             (princ "\n>> Doi tuong khong phai Block co Attribute, bo qua.")))
         (setq doneFlag T))))

    ;;------------------ CHE DO CHON VUNG + SAP XEP ------------------
    ((= mode "Window")
     (setq ss (ssget '((0 . "INSERT"))))
     (if (not ss) (progn (princ "\nKhong co doi tuong nao duoc chon.") (exit)))
     (setq i 0)
     (repeat (sslength ss)
       (setq ent (ssname ss i)
             obj (vlax-ename->vla-object ent))
       (if (= (vla-get-hasattributes obj) :vlax-true)
         (setq entObjs (append entObjs (list obj))))
       (setq i (1+ i)))

     (if (= order "2Diem")
       ;;--- Sap xep theo huong bat ky bang cach pick 2 diem ---
       (progn
         (setq pt1 (getpoint "\nChon diem dau (moc bat dau tang dan): "))
         (if (not pt1) (progn (princ "\n>> Da huy lenh.") (exit)))
         (setq pt2 (getpoint pt1 "\nChon diem cuoi (huong tang dan): "))
         (if (not pt2) (progn (princ "\n>> Da huy lenh.") (exit)))
         (setq vecX (- (car pt2) (car pt1))
               vecY (- (cadr pt2) (cadr pt1))
               vecLen (sqrt (+ (* vecX vecX) (* vecY vecY))))
         (if (< vecLen 1e-8)
           (progn (princ "\n>> Hai diem trung nhau, huy lenh.") (exit)))

         ;; Tinh truoc gia tri hinh chieu cho tung block roi ghep cap
         ;; (gia_tri_chieu . doi_tuong) - tranh dung ham phu/dong bien
         ;; de khong bi loi tham chieu bien khi vl-sort goi lai.
         (setq objPairs
           (mapcar
             (function
               (lambda (o / p dx dy)
                 (setq p  (vlax-get o 'insertionpoint)
                       dx (- (car p) (car pt1))
                       dy (- (cadr p) (cadr pt1)))
                 (cons (/ (+ (* dx vecX) (* dy vecY)) vecLen) o)))
             entObjs))

         (setq objPairs
           (vl-sort objPairs (function (lambda (p1 p2) (< (car p1) (car p2))))))

         (setq entObjs (mapcar 'cdr objPairs)))

       ;;--- Cac huong truc chuan X/Y ---
       (setq entObjs
         (vl-sort entObjs
           (cond
             ((= order "Xtang") (function (lambda (a b) (< (car (vlax-get a 'insertionpoint)) (car (vlax-get b 'insertionpoint))))))
             ((= order "Xgiam") (function (lambda (a b) (> (car (vlax-get a 'insertionpoint)) (car (vlax-get b 'insertionpoint))))))
             ((= order "Ytang") (function (lambda (a b) (< (cadr (vlax-get a 'insertionpoint)) (cadr (vlax-get b 'insertionpoint))))))
             ((= order "Ygiam") (function (lambda (a b) (> (cadr (vlax-get a 'insertionpoint)) (cadr (vlax-get b 'insertionpoint))))))))))
     ))

  (if (< (length entObjs) 1)
    (progn (princ "\nKhong co Block hop le nao duoc chon.") (exit)))

  ;;===================================================================
  ;; BUOC 3: hien dialog thu 2 - chon Tag + nhap Prefix/Start/Step
  ;;===================================================================
  (setq attList (GetAtts (car entObjs)))
  (if (not attList)
    (progn (princ "\nBlock dau tien khong co Attribute.") (exit)))
  (setq tagList (mapcar 'car attList))

  (setq dcl_id (load_dialog dclPath))
  (if (or (not dcl_id) (< dcl_id 0))
    (progn (princ "\n>> Khong the load dialog buoc 2.") (exit)))

  (if (not (new_dialog "cpatt_data_dlg" dcl_id))
    (progn (unload_dialog dcl_id) (princ "\n>> Khong the mo dialog buoc 2.") (exit)))

  (start_list "taglist")
  (mapcar 'add_list tagList)
  (end_list)
  (set_tile "taglist" "0")
  (set_tile "prefix" "")
  (set_tile "start" "1")
  (set_tile "step" "1")

  (setq dlgOK nil)
  (action_tile "accept"
    "(setq tagChon (nth (atoi (get_tile \"taglist\")) tagList))
     (setq prefix (get_tile \"prefix\"))
     (setq start (atoi (get_tile \"start\")))
     (setq step (atoi (get_tile \"step\")))
     (setq dlgOK T)
     (done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (start_dialog)
  (unload_dialog dcl_id)

  (if (not dlgOK)
    (progn (princ "\n>> Da huy lenh.") (exit)))
  (if (not tagChon)
    (progn (princ "\nLua chon Tag khong hop le.") (exit)))

  ;;===================================================================
  ;; BUOC 4: gan gia tri tang dan
  ;;===================================================================
  (setq i 0)
  (foreach obj entObjs
    (setq attList (GetAtts obj))
    (setq pair (assoc tagChon attList))
    (if pair
      (progn
        (setq val (strcat prefix (itoa (+ start (* i step)))))
        (vla-put-textstring (cdr pair) val)
        (princ (strcat "\n  " (itoa (1+ i)) ". Tag[" tagChon "] = " val)))
      (princ (strcat "\n  >> Block " (itoa (1+ i)) " khong co Tag [" tagChon "], bo qua.")))
    (setq i (1+ i)))

  (command "_.REGEN")
  (princ (strcat "\n\n>> Hoan tat, da cap nhat " (itoa (length entObjs)) " block."))
  (princ)
)

(princ "\n>> Da nap BLOCKTOOLS.LSP - Lenh: SYNCATT, CPATT")
(princ)
