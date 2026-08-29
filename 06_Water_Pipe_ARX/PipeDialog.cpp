#include "PipeGeometry.h"
#include "Resource.h"
#include <windows.h>
#include <cwchar>

namespace VCI::WaterPipe
{
static constexpr INT_PTR ResultNoHollow = 10001;
static double ReadNumber(HWND dialog, int id)
{
    wchar_t text[64]{};
    GetDlgItemTextW(dialog, id, text, 64);
    return std::wcstod(text, nullptr);
}

static INT_PTR CALLBACK DialogProc(HWND dialog, UINT message, WPARAM wParam, LPARAM lParam)
{
    auto* settings = reinterpret_cast<Settings*>(GetWindowLongPtrW(dialog, DWLP_USER));
    if (message == WM_INITDIALOG) {
        settings = reinterpret_cast<Settings*>(lParam);
        SetWindowLongPtrW(dialog, DWLP_USER, lParam);
        SetDlgItemTextW(dialog, IDC_OD, L"90");
        SetDlgItemTextW(dialog, IDC_WALL, L"4.5");
        SetDlgItemTextW(dialog, IDC_ELBOW, L"4.5");
        SetDlgItemTextW(dialog, IDC_RADIUS, L"135");
        CheckDlgButton(dialog, IDC_HOLLOW, BST_CHECKED);
        return TRUE;
    }
    if (message == WM_COMMAND && LOWORD(wParam) == IDOK && settings) {
        settings->outerDiameter = ReadNumber(dialog, IDC_OD);
        settings->wallThickness = ReadNumber(dialog, IDC_WALL);
        settings->elbowWallThickness = ReadNumber(dialog, IDC_ELBOW);
        settings->bendRadius = ReadNumber(dialog, IDC_RADIUS);
        if (!settings->valid()) {
            MessageBoxW(dialog, L"Thong so khong hop le. Kiem tra lai OD, chieu day va ban kinh cut.", L"VCI Water Pipe", MB_ICONWARNING);
            return TRUE;
        }
        EndDialog(dialog, IsDlgButtonChecked(dialog, IDC_HOLLOW) == BST_CHECKED ? IDOK : ResultNoHollow);
        return TRUE;
    }
    if (message == WM_COMMAND && LOWORD(wParam) == IDCANCEL) { EndDialog(dialog, IDCANCEL); return TRUE; }
    return FALSE;
}

bool ShowSettingsDialog(Settings& settings, bool& hollow)
{
    HMODULE module = nullptr;
    GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, reinterpret_cast<LPCWSTR>(&ShowSettingsDialog), &module);
    INT_PTR result = DialogBoxParamW(module, MAKEINTRESOURCEW(IDD_PIPE_DIALOG), GetActiveWindow(), DialogProc, reinterpret_cast<LPARAM>(&settings));
    hollow = result == IDOK;
    return result == IDOK || result == ResultNoHollow;
}
}
