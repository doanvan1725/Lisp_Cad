;;; ==========================================================================
;;; LISP VIẾT CHỮ VTT - SỬA LỖI TREO CAD / ĐÃ TỐI ƯU HÓA 100%
;;; Lệnh chính: VTT | Lệnh chuyển đổi chữ cũ: CVT
;;; ==========================================================================

(vl-load-com)

;;; --- BẢNG MÃ CHUYỂN ĐỔI UNICODE -> TCVN3 ---
(defun GetUniToTCVNMap ()
  (list
    (cons "á" (chr 184)) (cons "à" (chr 181)) (cons "ả" (chr 182)) (cons "ã" (chr 183)) (cons "ạ" (chr 185))
    (cons "ă" (chr 168)) (cons "ắ" (chr 190)) (cons "ằ" (chr 187)) (cons "ẳ" (chr 188)) (cons "ẵ" (chr 189)) (cons "ặ" (chr 198))
    (cons "â" (chr 169)) (cons "ấ" (chr 202)) (cons "ầ" (chr 199)) (cons "ẩ" (chr 200)) (cons "ẫ" (chr 201)) (cons "ậ" (chr 203))
    (cons "é" (chr 208)) (cons "è" (chr 204)) (cons "ẻ" (chr 206)) (cons "ẽ" (chr 207)) (cons "ẹ" (chr 209))
    (cons "ê" (chr 170)) (cons "ế" (chr 213)) (cons "ề" (chr 210)) (cons "ể" (chr 211)) (cons "ễ" (chr 212)) (cons "ệ" (chr 214))
    (cons "í" (chr 221)) (cons "ì" (chr 215)) (cons "ỉ" (chr 216)) (cons "ĩ" (chr 220)) (cons "ị" (chr 222))
    (cons "ó" (chr 227)) (cons "ò" (chr 223)) (cons "ỏ" (chr 225)) (cons "õ" (chr 226)) (cons "ọ" (chr 228))
    (cons "ô" (chr 171)) (cons "ố" (chr 232)) (cons "ồ" (chr 230)) (cons "ổ" (chr 231)) (cons "ỗ" (chr 233)) (cons "ộ" (chr 234))
    (cons "ơ" (chr 172)) (cons "ớ" (chr 237)) (cons "ờ" (chr 235)) (cons "ở" (chr 236)) (cons "ỡ" (chr 238)) (cons "ợ" (chr 239))
    (cons "ú" (chr 243)) (cons "ù" (chr 241)) (cons "ủ" (chr 242)) (cons "ũ" (chr 244)) (cons "ụ" (chr 245))
    (cons "ư" (chr 173)) (cons "ứ" (chr 250)) (cons "ừ" (chr 247)) (cons "ử" (chr 248)) (cons "ữ" (chr 249)) (cons "ự" (chr 251))
    (cons "ý" (chr 253)) (cons "ỳ" (chr 252)) (cons "ỷ" (chr 254)) (cons "ỹ" (chr 255))
    (cons "đ" (chr 174)) (cons "Đ" (chr 167))
    ;; Chuyển đổi chữ hoa Unicode sang chữ thường TCVN3 (Tránh lỗi font .VnH)
    (cons "Á" (chr 184)) (cons "À" (chr 181)) (cons "Ả" (chr 182)) (cons "Ã" (chr 183)) (cons "Ạ" (chr 185))
    (cons "Ă" (chr 168)) (cons "Ắ" (chr 190)) (cons "Ằ" (chr 187)) (cons "Ẳ" (chr 188)) (cons "Ẵ" (chr 189)) (cons "Ặ" (chr 198))
    (cons "Â" (chr 169)) (cons "Ấ" (chr 202)) (cons "Ầ" (chr 199)) (cons "Ẩ" (chr 200)) (cons "Ẫ" (chr 201)) (cons "Ậ" (chr 203))
    (cons "É" (chr 208)) (cons "È" (chr 204)) (cons "Ẻ" (chr 206)) (cons "Ẽ" (chr 207)) (cons "Ẹ" (chr 209))
    (cons "Ê" (chr 170)) (cons "Ế" (chr 213)) (cons "Ề" (chr 210)) (cons "Ể" (chr 211)) (cons "Ễ" (chr 212)) (cons "Ệ" (chr 214))
    (cons "Í" (chr 221)) (cons "Ì" (chr 215)) (cons "Ỉ" (chr 216)) (cons "Ĩ" (chr 220)) (cons "Ị" (chr 222))
    (cons "Ó" (chr 227)) (cons "Ò" (chr 223)) (cons "Ỏ" (chr 225)) (cons "Õ" (chr 226)) (cons "Ọ" (chr 228))
    (cons "Ô" (chr 171)) (cons "Ố" (chr 232)) (cons "Ồ" (chr 230)) (cons "Ổ" (chr 231)) (cons "Ỗ" (chr 233)) (cons "Ộ" (chr 234))
    (cons "Ơ" (chr 172)) (cons "Ớ" (chr 237)) (cons "Ờ" (chr 235)) (cons "Ở" (chr 236)) (cons "Ỡ" (chr 238)) (cons "Ợ" (chr 239))
    (cons "Ú" (chr 243)) (cons "Ù" (chr 241)) (cons "Ủ" (chr 242)) (cons "Ũ" (chr 244)) (cons "Ụ" (chr 245))
    (cons "Ư" (chr 173)) (cons "Ứ" (chr 250)) (cons "Ừ" (chr 247)) (cons "Ử" (chr 248)) (cons "Ữ" (chr 249)) (cons "Ự" (chr 251))
    (cons "Ý" (chr 253)) (cons "Ỳ" (chr 252)) (cons "Ỷ" (chr 254)) (cons "Ỹ" (chr 255))
  )
)

(defun GetTCVNToUniMap ()
  (mapcar '(lambda (x) (cons (cdr x) (car x))) (GetUniToTCVNMap))
)

;;; --- DỊCH CHUỖI ---
(defun TranslateString (str mapList / res i len found pair key klen)
  (setq res "" len (strlen str) i 1)
  (while (<= i len)
    (setq found nil)
    (foreach pair mapList
      (if (not found)
        (progn
          (setq key (car pair) klen (strlen key))
          (if (= (substr str i klen) key)
            (progn (setq res (strcat res (cdr pair)) i (+ i klen) found t))
          )
        )
      )
    )
    (if (not found) (progn (setq res (strcat res (substr str i 1)) i (1+ i))))
  )
  res
)

;;; --- KIỂM TRA STYLE PHẢI TCVN3 KHÔNG ---
(defun IsTCVN3Style (styleName / styleEnt fontFile)
  (setq styleEnt (tblsearch "STYLE" styleName))
  (if styleEnt
    (progn
      (setq fontFile (strcase (cdr (assoc 3 styleEnt))))
      (or 
        (= (substr fontFile 1 2) "VN")
        (= (substr fontFile 1 3) ".VN")
        (vl-string-search "TCVN" (strcase styleName))
        (vl-string-search "VN" (strcase styleName))
      )
    )
    nil
  )
)

;;; --- QUÉT STYLE CỰC NHANH KHÔNG SỬ DỤNG ACTIVEX ---
(defun GetStyleList ( / lst style)
  (setq lst nil)
  (setq style (tblnext "STYLE" t))
  (while style
    (setq lst (cons (cdr (assoc 2 style)) lst))
    (setq style (tblnext "STYLE"))
  )
  (acad_strlsort lst)
)

;;; --- TẠO FILE DCL TẠM (BỎ TIẾNG VIỆT CÓ DẤU TRÁNH LỖI ENCODING) ---
(defun CreateDCLFile ( / f dcl_file)
  (setq dcl_file (vl-filename-mktemp "smarttext" "" ".dcl"))
  (if (setq f (open dcl_file "w"))
    (progn
      (write-line "smarttext_dlg : dialog {" f)
      (write-line "    label = \"Smart Text Tool (VTT) - CAD GUI\";" f)
      (write-line "    : row {" f)
      (write-line "        : boxed_column {" f)
      (write-line "            label = \"Thiet lap Text\";" f)
      (write-line "            : popup_list { label = \"Text Style:\"; key = \"style_list\"; width = 28; }" f)
      (write-line "            : edit_box { label = \"Chieu cao:\"; key = \"txt_height\"; edit_width = 12; }" f)
      (write-line "        }" f)
      (write-line "        : boxed_radio_column {" f)
      (write-line "            label = \"Kieu chu\";" f)
      (write-line "            : radio_button { label = \"MText (Da dong)\"; key = \"type_m\"; }" f)
      (write-line "            : radio_button { label = \"DText (Don dong)\"; key = \"type_d\"; }" f)
      (write-line "        }" f)
      (write-line "    }" f)
      (write-line "    spacer;" f)
      (write-line "    ok_cancel;" f)
      (write-line "}" f)
      (close f)
      dcl_file
    )
    nil
  )
)

;;; ==========================================================================
;;; LỆNH CHÍNH: VTT
;;; ==========================================================================
(defun c:VTT ( / dcl_file dcl_id styleList curStyle initIdx status hStr styleEnt fixedHeight pt entMarker entNext oldEcho tmp_err)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; Khởi tạo biến lưu tạm
  (setq dcl_file nil dcl_id nil)

  ;; Hệ thống bẫy lỗi và giải phóng bộ nhớ khi bấm ESC / Hủy bỏ
  (setq tmp_err *error*)
  (defun *error* (msg)
    (if dcl_id (vl-catch-all-apply 'unload_dialog (list dcl_id)))
    (if (and dcl_file (vl-file-exists-p dcl_file)) (vl-file-delete dcl_file))
    (setvar "CMDECHO" oldEcho)
    (setq *error* tmp_err)
    (princ)
  )

  ;; Khởi tạo giá trị mặc định ban đầu nếu chưa có
  (if (not *wtext-height*) (setq *wtext-height* 2.5))
  (if (not *wtext-type*) (setq *wtext-type* "M"))

  ;; 1. Tạo và Load DCL an toàn
  (setq dcl_file (CreateDCLFile))
  (if dcl_file
    (progn
      (setq dcl_id (load_dialog dcl_file))
      (if (and dcl_id (>= dcl_id 0) (new_dialog "smarttext_dlg" dcl_id))
        (progn
          ;; Đưa danh sách Style vào popup
          (setq styleList (GetStyleList))
          (start_list "style_list")
          (mapcar 'add_list styleList)
          (end_list)

          ;; Chọn mặc định Style hiện hành
          (setq curStyle (getvar "TEXTSTYLE"))
          (setq initIdx (vl-position curStyle styleList))
          (if initIdx
            (set_tile "style_list" (itoa initIdx))
            (set_tile "style_list" "0")
          )

          ;; Đồng bộ dữ liệu hiện hành lên giao diện
          (set_tile "txt_height" (rtos *wtext-height* 2 2))
          (if (= *wtext-type* "M")
            (set_tile "type_m" "1")
            (set_tile "type_d" "1")
          )

          ;; Lắng nghe tương tác nút nhấn
          (action_tile "accept"
            "(progn
               (setq *wtext-style* (nth (atoi (get_tile \"style_list\")) styleList))
               (setq hStr (get_tile \"txt_height\"))
               (if (and hStr (/= hStr \"\")) (setq *wtext-height* (distof hStr)))
               (if (= (get_tile \"type_m\") \"1\")
                 (setq *wtext-type* \"M\")
                 (setq *wtext-type* \"D\")
               )
               (done_dialog 1)
             )"
          )
          (action_tile "cancel" "(done_dialog 0)")

          (setq status (start_dialog))
          (unload_dialog dcl_id)
          (setq dcl_id nil)
        )
      )
      (vl-file-delete dcl_file)
      (setq dcl_file nil)
    )
  )

  ;; 2. Bắt đầu viết chữ khi người dùng nhấn OK
  (if (= status 1)
    (progn
      (setvar "TEXTSTYLE" *wtext-style*)
      (setvar "TEXTSIZE" *wtext-height*)
      
      (setq pt (getpoint "\nChọn điểm đặt chữ: "))
      (if pt
        (progn
          (setq entMarker (entlast)) ; Đánh dấu đối tượng cuối cùng trước khi nhập
          
          ;; Khởi chạy lệnh nhập chữ của CAD (nhập lùi ô, Enter xuống dòng thoải mái)
          (if (= *wtext-type* "M")
            (progn
              (command "_.mtext" pt "H" *wtext-height* "W" 0)
              (while (> (getvar "CMDACTIVE") 0) (command pause))
            )
            (progn
              (setq styleEnt (tblsearch "STYLE" *wtext-style*))
              (setq fixedHeight (cdr (assoc 40 styleEnt)))
              (if (= fixedHeight 0.0)
                (command "_.text" pt *wtext-height* 0)
                (command "_.text" pt 0)
              )
              (while (> (getvar "CMDACTIVE") 0) (command pause))
            )
          )
          
          ;; 3. Tự động chuyển đổi mã chữ nếu dùng Font TCVN3
          (if (IsTCVN3Style *wtext-style*)
            (progn
              (princ "\nHệ thống phát hiện Font TCVN3. Đang tự động chuyển đổi mã...")
              (if (null entMarker)
                (setq entNext (entnext))
                (setq entNext (entnext entMarker))
              )
              (while entNext
                (ConvertEntityToTCVN3 entNext)
                (setq entNext (entnext entNext))
              )
              (princ "\nĐã tự động sửa lỗi font!")
            )
          )
        )
      )
    )
  )

  ;; Trả lại hàm lỗi mặc định
  (setq *error* tmp_err)
  (setvar "CMDECHO" oldEcho)
  (princ)
)

;;; --- CONVERT THỰC THỂ SANG TCVN3 ---
(defun ConvertEntityToTCVN3 (ent / obj oldStr newStr)
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'TextString)
    (progn
      (setq oldStr (vla-get-TextString obj))
      (setq newStr (TranslateString oldStr (GetUniToTCVNMap)))
      (vla-put-TextString obj newStr)
    )
  )
)

;;; ==========================================================================
;;; LỆNH PHỤ: CVT (CHUYỂN ĐỔI CHỮ CÓ SẴN UNICODE <-> TCVN3)
;;; ==========================================================================
(defun c:CVT ( / ss i ent obj oldStr newStr choice opMap)
  (initget "1 2")
  (setq choice (getkword "\nChọn chiều chuyển đổi [1: Unicode->TCVN3 / 2: TCVN3->Unicode] <1>: "))
  (if (not choice) (setq choice "1"))
  
  (if (= choice "1")
    (setq opMap (GetUniToTCVNMap))
    (setq opMap (GetTCVNToUniMap))
  )
  
  (princ "\nChọn các Text/MText cần chuyển đổi mã...")
  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
              oldStr (vla-get-TextString obj)
              newStr (TranslateString oldStr opMap)
              i (1+ i))
        (vla-put-TextString obj newStr)
      )
      (princ (strcat "\nHệ thống đã xử lý xong " (itoa (sslength ss)) " đối tượng!"))
    )
  )
  (princ)
)