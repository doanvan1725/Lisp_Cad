(defun c:KTD ( / dcl_file file dcl_id status pt active_val active_height active_style_idx 
               keep_running append_sym get_text_styles styles cur_style sel_style 
               num_height style_data fixed_h *error*)
  (vl-load-com)
  
  ;; Bộ xử lý lỗi tự động dọn dẹp file tạm khi đột ngột thoát
  (defun *error* (msg)
    (if (and dcl_file (vl-file-systime dcl_file))
      (vl-file-delete dcl_file)
    )
    (if dcl_id (unload_dialog dcl_id))
    (princ (strcat "\nChương trình đã dừng: " msg))
    (princ)
  )

  ;; Hàm lấy danh sách Text Styles có trong bản vẽ
  (defun get_text_styles ( / data lst )
    (setq data (tblnext "STYLE" T))
    (while data
      (if (not (vl-string-search "|" (cdr (assoc 2 data))))
        (setq lst (cons (cdr (assoc 2 data)) lst))
      )
      (setq data (tblnext "STYLE"))
    )
    (vl-sort lst '<)
  )

  ;; 1. Tạo file giao diện DCL tạm thời
  (setq dcl_file (vl-filename-mktemp "specchar.dcl"))
  (setq file (open dcl_file "w"))
  
  (write-line "ktd_dialog : dialog {" file)
  (write-line "    label = \"BẢNG CHÈN KÝ TỰ ĐẶC BIỆT & HY LẠP\";" file)
  (write-line "    spacer_1;" file)
  
  ;; Hàng chứa 2 cột chính
  (write-line "    : row {" file)
  
  ;; CỘT 1: Ký tự kỹ thuật (Thay Alpha, Beta bằng Góc và Vô cực)
  (write-line "      : boxed_column {" file)
  (write-line "          label = \" Ký tự Kỹ thuật \";" file)
  (write-line "          : row {" file)
  (write-line "              : button { key = \"b1\"; label = \"° (Độ)\"; width = 12; }" file)
  (write-line "              : button { key = \"b2\"; label = \"± (Cộng/Trừ)\"; width = 12; }" file)
  (write-line "              : button { key = \"b3\"; label = \"Ø (Phi)\"; width = 12; }" file)
  (write-line "              : button { key = \"b4\"; label = \"℄ (Center)\"; width = 12; }" file)
  (write-line "          }" file)
  (write-line "          : row {" file)
  (write-line "              : button { key = \"b5\"; label = \"Δ (Delta)\"; width = 12; }" file)
  (write-line "              : button { key = \"b6\"; label = \"Ω (Ohm)\"; width = 12; }" file)
  (write-line "              : button { key = \"b7\"; label = \"≈ (Xấp xỉ)\"; width = 12; }" file)
  (write-line "              : button { key = \"b8\"; label = \"≠ (Khác)\"; width = 12; }" file)
  (write-line "          }" file)
  (write-line "          : row {" file)
  (write-line "              : button { key = \"b9\"; label = \"≤ (Nhỏ/bằng)\"; width = 12; }" file)
  (write-line "              : button { key = \"b10\"; label = \"≥ (Lớn/bằng)\"; width = 12; }" file)
  (write-line "              : button { key = \"b11\"; label = \"² (Mũ 2)\"; width = 12; }" file)
  (write-line "              : button { key = \"b12\"; label = \"³ (Mũ 3)\"; width = 12; }" file)
  (write-line "          }" file)
  (write-line "          : row {" file)
  (write-line "              : button { key = \"b13\"; label = \"‰ (Phần nghìn)\"; width = 12; }" file)
  (write-line "              : button { key = \"b14\"; label = \"⅊ (Property)\"; width = 12; }" file)
  (write-line "              : button { key = \"b15\"; label = \"∠ (Góc)\"; width = 12; }" file)
  (write-line "              : button { key = \"b16\"; label = \"∞ (Vô cực)\"; width = 12; }" file)
  (write-line "          }" file)
  (write-line "      }" file)

  ;; CỘT 2: Trọn bộ chữ cái Hy Lạp (Lưới 4 hàng x 6 cột)
  (write-line "      : boxed_column {" file)
  (write-line "          label = \" Ký tự Hy Lạp \";" file)
  ;; Hàng 1
  (write-line "          : row {" file)
  (write-line "              : button { key = \"g1\"; label = \"α (Alpha)\"; width = 10; }" file)
  (write-line "              : button { key = \"g2\"; label = \"β (Beta)\"; width = 10; }" file)
  (write-line "              : button { key = \"g3\"; label = \"γ (Gamma)\"; width = 10; }" file)
  (write-line "              : button { key = \"g4\"; label = \"δ (Delta)\"; width = 10; }" file)
  (write-line "              : button { key = \"g5\"; label = \"ε (Epsilon)\"; width = 10; }" file)
  (write-line "              : button { key = \"g6\"; label = \"ζ (Zeta)\"; width = 10; }" file)
  (write-line "          }" file)
  ;; Hàng 2
  (write-line "          : row {" file)
  (write-line "              : button { key = \"g7\"; label = \"η (Eta)\"; width = 10; }" file)
  (write-line "              : button { key = \"g8\"; label = \"θ (Theta)\"; width = 10; }" file)
  (write-line "              : button { key = \"g9\"; label = \"ι (Iota)\"; width = 10; }" file)
  (write-line "              : button { key = \"g10\"; label = \"κ (Kappa)\"; width = 10; }" file)
  (write-line "              : button { key = \"g11\"; label = \"λ (Lambda)\"; width = 10; }" file)
  (write-line "              : button { key = \"g12\"; label = \"μ (Mu)\"; width = 10; }" file)
  (write-line "          }" file)
  ;; Hàng 3
  (write-line "          : row {" file)
  (write-line "              : button { key = \"g13\"; label = \"ν (Nu)\"; width = 10; }" file)
  (write-line "              : button { key = \"g14\"; label = \"ξ (Xi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g15\"; label = \"ο (Omicron)\"; width = 10; }" file)
  (write-line "              : button { key = \"g16\"; label = \"π (Pi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g17\"; label = \"ρ (Rho)\"; width = 10; }" file)
  (write-line "              : button { key = \"g18\"; label = \"σ (Sigma)\"; width = 10; }" file)
  (write-line "          }" file)
  ;; Hàng 4
  (write-line "          : row {" file)
  (write-line "              : button { key = \"g19\"; label = \"τ (Tau)\"; width = 10; }" file)
  (write-line "              : button { key = \"g20\"; label = \"υ (Upsilon)\"; width = 10; }" file)
  (write-line "              : button { key = \"g21\"; label = \"φ (Phi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g22\"; label = \"χ (Chi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g23\"; label = \"ψ (Psi)\"; width = 10; }" file)
  (write-line "              : button { key = \"g24\"; label = \"ω (Omega)\"; width = 10; }" file)
  (write-line "          }" file)
  (write-line "      }" file)
  (write-line "    }" file)
  
  (write-line "    spacer_1;" file)
  
  ;; Thiết lập định dạng chữ (Style & Height)
  (write-line "    : boxed_row {" file)
  (write-line "        label = \" Tùy chỉnh chữ \";" file)
  (write-line "        : popup_list { key = \"cb_style\"; label = \"Font chữ (Style):\"; width = 30; }" file)
  (write-line "        : edit_box { key = \"eb_height\"; label = \"Cỡ chữ (Height):\"; edit_width = 10; }" file)
  (write-line "    }" file)

  ;; Vùng nhập và sửa text
  (write-line "    : row {" file)
  (write-line "        : edit_box {" file)
  (write-line "            key = \"eb_text\";" file)
  (write-line "            label = \"Nội dung chèn:\";" file)
  (write-line "            edit_width = 55;" file)
  (write-line "        }" file)
  (write-line "        : button {" file)
  (write-line "            key = \"btn_clear\";" file)
  (write-line "            label = \"Xóa Trắng\";" file)
  (write-line "            width = 10;" file)
  (write-line "        }" file)
  (write-line "    }" file)
  
  (write-line "    spacer_1;" file)
  
  ;; Hàng nút điều khiển cuối cùng
  (write-line "    : row {" file)
  (write-line "        : button { key = \"btn_ins\"; label = \"Chèn TEXT (DTEXT)\"; is_default = true; width = 20; }" file)
  (write-line "        : button { key = \"btn_ins_mt\"; label = \"Chèn MTEXT\"; width = 20; }" file)
  (write-line "        : cancel_button { label = \"Thoát\"; width = 12; }" file)
  (write-line "    }" file)
  (write-line "}" file)
  (close file)

  ;; 2. Hàm phụ trợ ghép ký tự vào cuối chuỗi
  (defun append_sym (sym / cur)
    (setq cur (get_tile "eb_text"))
    (set_tile "eb_text" (strcat cur sym))
  )

  ;; 3. Khởi tạo dữ liệu ban đầu
  (setq styles (get_text_styles))
  (setq active_val "")
  (setq active_height (rtos (getvar "TEXTSIZE") 2 2))
  (setq cur_style (getvar "TEXTSTYLE"))
  (setq active_style_idx (vl-position cur_style styles))
  (if (not active_style_idx) (setq active_style_idx 0))

  (setq keep_running t)

  ;; 4. Vòng lặp điều khiển giao diện
  (while keep_running
    (setq dcl_id (load_dialog dcl_file))
    (if (not (new_dialog "ktd_dialog" dcl_id))
      (progn (setq keep_running nil) (exit))
    )

    ;; Đưa danh sách Text Styles vào Dropdown
    (start_list "cb_style")
    (mapcar 'add_list styles)
    (end_list)
    
    ;; Cập nhật giao diện từ các biến trạng thái
    (set_tile "cb_style" (itoa active_style_idx))
    (set_tile "eb_height" active_height)
    (set_tile "eb_text" active_val)

    ;; Hành động cho Ký tự kỹ thuật
    (action_tile "b1" "(append_sym \"%%d\")")
    (action_tile "b2" "(append_sym \"%%p\")")
    (action_tile "b3" "(append_sym \"%%c\")")
    (action_tile "b4" "(append_sym \"\\\\U+2104\")")
    (action_tile "b5" "(append_sym \"\\\\U+0394\")")
    (action_tile "b6" "(append_sym \"\\\\U+03A9\")")
    (action_tile "b7" "(append_sym \"\\\\U+2248\")")
    (action_tile "b8" "(append_sym \"\\\\U+2260\")")
    (action_tile "b9" "(append_sym \"\\\\U+2264\")")
    (action_tile "b10" "(append_sym \"\\\\U+2265\")")
    (action_tile "b11" "(append_sym \"\\\\U+00B2\")")
    (action_tile "b12" "(append_sym \"\\\\U+00B3\")")
    (action_tile "b13" "(append_sym \"\\\\U+2030\")")
    (action_tile "b14" "(append_sym \"\\\\U+214A\")")
    (action_tile "b15" "(append_sym \"\\\\U+2220\")") ; Góc
    (action_tile "b16" "(append_sym \"\\\\U+221E\")") ; Vô cực

    ;; Hành động cho Trọn bộ 24 ký tự Hy Lạp
    (action_tile "g1"  "(append_sym \"\\\\U+03B1\")") ; alpha
    (action_tile "g2"  "(append_sym \"\\\\U+03B2\")") ; beta
    (action_tile "g3"  "(append_sym \"\\\\U+03B3\")") ; gamma
    (action_tile "g4"  "(append_sym \"\\\\U+03B4\")") ; delta
    (action_tile "g5"  "(append_sym \"\\\\U+03B5\")") ; epsilon
    (action_tile "g6"  "(append_sym \"\\\\U+03B6\")") ; zeta
    (action_tile "g7"  "(append_sym \"\\\\U+03B7\")") ; eta
    (action_tile "g8"  "(append_sym \"\\\\U+03B8\")") ; theta
    (action_tile "g9"  "(append_sym \"\\\\U+03B9\")") ; iota
    (action_tile "g10" "(append_sym \"\\\\U+03BA\")") ; kappa
    (action_tile "g11" "(append_sym \"\\\\U+03BB\")") ; lambda
    (action_tile "g12" "(append_sym \"\\\\U+03BC\")") ; mu
    (action_tile "g13" "(append_sym \"\\\\U+03BD\")") ; nu
    (action_tile "g14" "(append_sym \"\\\\U+03BE\")") ; xi
    (action_tile "g15" "(append_sym \"\\\\U+03BF\")") ; omicron
    (action_tile "g16" "(append_sym \"\\\\U+03C0\")") ; pi
    (action_tile "g17" "(append_sym \"\\\\U+03C1\")") ; rho
    (action_tile "g18" "(append_sym \"\\\\U+03C3\")") ; sigma
    (action_tile "g19" "(append_sym \"\\\\U+03C4\")") ; tau
    (action_tile "g20" "(append_sym \"\\\\U+03C5\")") ; upsilon
    (action_tile "g21" "(append_sym \"\\\\U+03C6\")") ; phi
    (action_tile "g22" "(append_sym \"\\\\U+03C7\")") ; chi
    (action_tile "g23" "(append_sym \"\\\\U+03C8\")") ; psi
    (action_tile "g24" "(append_sym \"\\\\U+03C9\")") ; omega

    (action_tile "btn_clear" "(set_tile \"eb_text\" \"\")")

    ;; Khi nhấn Chèn: Ghi nhận các tùy biến thiết lập trên bảng
    (action_tile "btn_ins" "(setq active_val (get_tile \"eb_text\") active_height (get_tile \"eb_height\") active_style_idx (atoi (get_tile \"cb_style\"))) (done_dialog 1)")
    (action_tile "btn_ins_mt" "(setq active_val (get_tile \"eb_text\") active_height (get_tile \"eb_height\") active_style_idx (atoi (get_tile \"cb_style\"))) (done_dialog 2)")
    (action_tile "cancel" "(done_dialog 0)")

    (setq status (start_dialog))
    (unload_dialog dcl_id)

    ;; 5. Tiến hành vẽ đối tượng chữ ra AutoCAD
    (if (or (= status 1) (= status 2))
      (progn
        ;; Thiết lập thuộc tính Font chữ được chọn
        (setq sel_style (nth active_style_idx styles))
        (setvar "TEXTSTYLE" sel_style)
        
        ;; Thiết lập cỡ chữ
        (setq num_height (atof active_height))
        (if (<= num_height 0.0) (setq num_height (getvar "TEXTSIZE")))
        (setvar "TEXTSIZE" num_height)
        
        ;; Kiểm tra xem Text Style hiện tại có bị đặt cứng chiều cao không
        (setq style_data (tblsearch "STYLE" sel_style))
        (setq fixed_h (cdr (assoc 40 style_data)))

        (if (/= active_val "")
          (progn
            (setq pt (getpoint "\nChọn điểm chèn chữ trên bản vẽ: "))
            (if pt
              (cond
                ;; Chèn DTEXT
                ((= status 1)
                 (if (and fixed_h (> fixed_h 0.0))
                   (vl-cmdf "_.text" pt "0" active_val)
                   (vl-cmdf "_.text" pt num_height "0" active_val)
                 )
                )
                ;; Chèn MTEXT
                ((= status 2)
                 (vl-cmdf "_-mtext" pt "_H" num_height "_W" "0" active_val "")
                )
              )
            )
          )
          (princ "\n[Lỗi] Bạn chưa nhập nội dung chữ!")
        )
      )
      ;; Nhấn Thoát (Cancel)
      (setq keep_running nil)
    )
  )

  ;; 6. Dọn dẹp file DCL tạm sau khi tắt bảng
  (if (and dcl_file (vl-file-systime dcl_file))
    (vl-file-delete dcl_file)
  )
  (princ "\nĐã đóng bảng công cụ.")
  (princ)
)

(princ "\n[OK] LISP đã cập nhật bộ ký tự Hy Lạp. Gõ lệnh 'KTD' để sử dụng.")
(princ)