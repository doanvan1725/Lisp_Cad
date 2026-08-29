@echo off
dotnet build VCI.WaterPipe.csproj -c Release
if errorlevel 1 exit /b %errorlevel%
echo Built: bin\Release\VCI.WaterPipe.dll
