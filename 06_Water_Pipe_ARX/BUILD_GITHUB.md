# Build `.arx` bằng GitHub Actions

GitHub runner công khai không có ObjectARX SDK. Autodesk cũng không cho phép đưa SDK vào repository công khai. Vì vậy cần một Windows self-hosted runner của bạn, có cài Visual Studio C++ và ObjectARX SDK 2025.

## Thiết lập một lần

1. Cài **Visual Studio 2022** với workload **Desktop development with C++**.
2. Cài **ObjectARX SDK 2025** vào đúng thư mục:

   ```text
   C:\ObjectARX 2025
   ```

   Thư mục này phải có `inc\` và `lib-x64\`.

3. Vào GitHub repository → **Settings → Actions → Runners → New self-hosted runner → Windows → x64**.
4. Làm theo các lệnh GitHub đưa ra để tải và đăng ký runner.
5. Khi GitHub hỏi label, thêm đúng label:

   ```text
   objectarx-2025
   ```

6. Để cửa sổ runner chạy ở trạng thái **Connected/Idle**.

## Build và tải file

1. Vào tab **Actions**.
2. Chọn workflow **Build native WaterPipe ARX**.
3. Bấm **Run workflow**.
4. Khi workflow xanh, mở run đó → phần **Artifacts** → tải `VCI-WaterPipe-ARX`.
5. Giải nén, lấy file `VCI.WaterPipe.ARX.arx`.

## Nạp vào AutoCAD

Trong AutoCAD:

```text
APPLOAD
→ chọn VCI.WaterPipe.ARX.arx
→ gõ ONGNUOC
```

Workflow cũng tự chạy khi có thay đổi trong `06_Water_Pipe_ARX/`. Nếu SDK nằm ở thư mục khác, sửa cả `ARX_SDK` trong file `.vcxproj` và đường dẫn kiểm tra trong workflow.
