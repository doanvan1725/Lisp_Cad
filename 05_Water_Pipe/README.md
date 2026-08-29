# VCI Water Pipe 2D

Plugin AutoCAD .NET tạo tuyến ống nước 2D có thành ống và cút bo tự động.

## Lệnh

- `ONGNUOC`: mở giao diện nhập đường kính ngoài, chiều dày ống, chiều dày cút và bán kính tim cút; bấm **Chọn tuyến ống**, sau đó pick các điểm đổi hướng.

## Build

Mặc định dùng AutoCAD 2027 x64:

```bat
build.bat
```

Nếu AutoCAD cài ở thư mục khác, sửa `AcadInstallPath` trong file `.csproj`. Trong AutoCAD dùng `NETLOAD` để nạp `bin\Release\VCI.WaterPipe.dll`, sau đó gõ `ONGNUOC`.

Phiên bản nền dùng Line/Arc native của AutoCAD, không tạo Solid3D hay vòng lặp regen nặng; phù hợp bản vẽ mặt bằng 2D. Có thể mở rộng tiếp với block fitting, lưu preset, Undo theo một nhóm và xuất thống kê chiều dài.
