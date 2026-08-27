@echo off
setlocal
echo ====================================================
echo   VCI Shape Steel Library - Build
echo   Target: AutoCAD 2027 / .NET 10
echo   Lenh: THEPHINH
echo ====================================================
echo.
if exist "bin" rmdir /s /q "bin" >nul 2>&1
if exist "obj" rmdir /s /q "obj" >nul 2>&1

where dotnet >nul 2>&1 || (echo [LOI] Khong tim thay .NET SDK. & pause & exit /b 1)
if not exist "C:\Program Files\Autodesk\AutoCAD 2027\AcMgd.dll" (
    echo [LOI] Khong tim thay AutoCAD 2027.
    pause & exit /b 1
)

dotnet build VCI.ShapeSteel.csproj -c Release --nologo
if errorlevel 1 ( echo. & echo [LOI] Build that bai. & pause & exit /b 1 )

echo.
echo ====================================================
echo   BUILD THANH CONG
echo ====================================================
echo   DLL: %CD%\bin\Release\VCI.ShapeSteel.dll
echo.
echo Cach dung:
echo   1. Mo AutoCAD 2027
echo   2. Go: NETLOAD
echo   3. Tro toi: %CD%\bin\Release\VCI.ShapeSteel.dll
echo   4. Go lenh: THEPHINH
echo.
pause
endlocal
