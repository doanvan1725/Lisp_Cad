(defun c:SMV ( / *error* oldOsmd oldCmde pt1_mod pt2_mod width_mod height_mod 
                 scVal width_lay height_lay pt1_lay pt2_lay activeDoc layoutName mviewObj)
  (vl-load-com)
  (setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; 1. Hàm xử lý bẫy lỗi hệ thống
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*")))
      (princ (strcat "\n[Lỗi]: " msg))
    )
    (if oldOsmd (setvar "OSMODE" oldOsmd))
    (if oldCmde (setvar "CMDECHO" oldCmde))
    (vla-EndUndoMark activeDoc)
    (princ "\n[SmartMview] Đã khôi phục cài đặt hệ thống.")
    (princ)
  )

  (vla-StartUndoMark activeDoc)
  (setq oldOsmd (getvar "OSMODE")
        oldCmde (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; 2. Bắt buộc người dùng phải bắt đầu ở Model
  (if (= (getvar "TILEMODE") 0)
    (progn
      (princ "\n[Thông báo]: Vui lòng về không gian MODEL để chọn vùng bản vẽ!")
      (setvar "TILEMODE" 1)
    )
  )

  (princ "\n=============================================")
  (princ "\n   SMART MVIEW - TỰ ĐỘNG TÍNH THEO TỶ LỆ")
  (princ "\n=============================================\n")

  ;; 3. Quét vùng chọn ở Model
  (setvar "OSMODE" 16383) ;; Bật bắt điểm
  (setq pt1_mod (getpoint "\n[Bước 1/3] Click điểm đầu tiên trên MODEL: "))
  
  (if pt1_mod
    (progn
      (setq pt2_mod (getcorner pt1_mod "\n[Bước 2/3] Kéo quét điểm đối diện vùng bản vẽ: "))
      
      (if pt2_mod
        (progn
          ;; Tính kích thước vùng quét tại Model (Đơn vị Model)
          (setq width_mod (abs (- (car pt1_mod) (car pt2_mod)))
                height_mod (abs (- (cadr pt1_mod) (cadr pt2_mod))))
          
          ;; 4. Nhập tỷ lệ bản vẽ trực quan
          (initget 6) ;; Không cho phép nhập số âm hoặc số 0
          (setq scVal (getreal "\n[Bước 3/3] Nhập mẫu số tỷ lệ mong muốn (Ví dụ: 50, 100, 200): "))
          
          (if (null scVal) (setq scVal 100.0)) ;; Mặc định nếu bấm Enter là tỷ lệ 1/100
          
          ;; Tính kích thước Mview thực tế sẽ hiển thị trên Layout giấy (đơn vị mm)
          (setq width_lay (/ width_mod scVal)
                height_lay (/ height_mod scVal))
          
          ;; 5. Chuyển sang Layout hiện hành
          (setq layoutName (getvar "CTAB"))
          (if (= layoutName "Model")
            (setvar "CTAB" (car (layoutlist)))
          )
          (if (= (getvar "CVPORT") 1) (princ) (command "_.pspace"))

          ;; 6. Đặt Mview lên Layout
          (setvar "OSMODE" 0) ;; Tắt bắt điểm để tránh lệch khung trên giấy
          (setq pt1_lay (getpoint (strcat "\n[Hệ thống]: Đã tính toán kích thước Mview (" 
                                          (rtos width_lay 2 1) "x" (rtos height_lay 2 1) 
                                          "mm). \nChọn điểm đặt Mview trên Layout: ")))
          
          (if pt1_lay
            (progn
              (setq pt2_lay (list (+ (car pt1_lay) width_lay) (+ (cadr pt1_lay) height_lay) 0.0))
              
              ;; Tạo Mview bằng lệnh vẽ cơ bản
              (command "_.mview" pt1_lay pt2_lay)
              
              ;; Lấy đối tượng Mview vừa tạo cuối cùng để áp tỷ lệ chính xác
              (setq mviewObj (vlax-ename->vla-object (entlast)))
              
              ;; Nhảy vào mspace để zoom chuẩn tọa độ
              (command "_.mspace")
              (command "_.zoom" "_window" pt1_mod pt2_mod)
              
              ;; Áp đặt tỷ lệ chuẩn (Custom Scale) thông qua ActiveX
              (vla-put-CustomScale mviewObj (/ 1.0 scVal))
              
              ;; Khóa Viewport tự động để bảo vệ tỷ lệ bản vẽ
              (vla-put-DisplayLocked mviewObj :vlax-true)
              
              ;; Trở lại Pspace để người dùng thấy toàn cảnh tờ giấy
              (command "_.pspace")
              
              (princ (strcat "\n[Thành công]: Đã tạo hoàn thành Mview tỷ lệ 1/" (rtos scVal 2 0) " và tự động KHÓA khung!"))
            )
            (princ "\n[Hủy bỏ]: Chưa chọn vị trí đặt trên Layout.")
          )
        )
        (princ "\n[Hủy bỏ]: Chưa quét xong vùng chọn ở Model.")
      )
    )
    (princ "\n[Hủy bỏ]: Chưa chọn điểm đầu tiên.")
  )

  ;; Khôi phục cài đặt gốc
  (*error* nil)
  (princ)
)

(princ "\n[SmartMview] Gõ 'SMV' tại Model để tạo Mview nhập tỷ lệ.\n")
(princ)