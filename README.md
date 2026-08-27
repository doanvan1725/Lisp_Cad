# Lisp_Cad

Bộ thư viện Lisp và mã bổ trợ cho AutoCAD.

## Nội dung

- Các file `.lsp` phục vụ thao tác bản vẽ, block, text, tọa độ, viewport và các tiện ích khác.
- Thư mục `THEP HINH/` chứa phần mã C# cho chức năng tra cứu / tính toán thép hình.
- Thư mục `TONG HOP/` chứa các file Lisp tổng hợp.

## Cấu trúc thư mục

```text
Lisp_Cad/
├─ README.md
├─ .gitignore
├─ *.lsp
├─ TONG HOP/
└─ THEP HINH/
```

## Một số file chính

- `ALR (GIONG AL REVIT).lsp`
- `ATTS(SYNBLOCKATT).lsp`
- `BET(SUANBLOCKATT).lsp`
- `BSC (UPBLOCK ATT).lsp`
- `DBL (CHEN BLOG VAO TOA DO).lsp`
- `DYN (VE BLOG).lsp`
- `DTD_DanhToaDo.lsp`
- `MC.lsp`
- `TAG(tag blog).lsp`
- `THKL(tong_hop_KL).lsp`
- `TONG HOP/DOAN.lsp`
- `THEP HINH/VCI.ShapeSteel.csproj`

## Ghi chú

- Tên file và thư mục được giữ nguyên để tương thích với cách gọi Lisp hiện có.
- Thư mục `THEP HINH/bin/` và `THEP HINH/obj/` được bỏ qua trong Git để không đưa file build sinh ra tự động lên repo.

## Sử dụng

1. Tải repo về máy.
2. Copy các file `.lsp` vào nơi bạn thường lưu thư viện AutoCAD.
3. Dùng `APPLOAD` hoặc cơ chế load Lisp bạn đang dùng để nạp file cần thiết.
4. Với phần `THEP HINH`, mở project trong Visual Studio nếu muốn build lại DLL.
