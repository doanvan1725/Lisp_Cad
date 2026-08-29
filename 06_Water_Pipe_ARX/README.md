# VCI Water Pipe — Native ObjectARX C++

Đây là bản native C++ dùng ObjectARX 2025, không dùng C#/.NET. Lõi `PipeGeometry.cpp` chỉ xử lý vector/arc và tạo biên ống; phần AutoCAD chỉ chuyển kết quả thành `AcDbLine`/`AcDbArc`, giúp số lượng entity và thời gian regen thấp.

## Build

1. Cài ObjectARX SDK 2025 và Visual Studio 2022 C++.
2. Mở `VCI.WaterPipe.ARX.vcxproj`.
3. Nếu SDK ở vị trí khác, sửa macro `ARX_SDK` thành thư mục chứa `inc` và `lib-x64`.
4. Build `Release | x64`, sau đó dùng `APPLOAD` nạp file `.arx`/`.dll` đầu ra.
5. Gõ `ONGNUOC`, nhập thông số và pick các điểm tuyến.

Project hiện dùng giao diện command-native để tránh phụ thuộc framework UI. Có thể bổ sung palette native ở lớp UI mà không ảnh hưởng lõi C++.
