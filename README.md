# Lisp_Cad

Bo thu vien Lisp va ma bo tro cho AutoCAD.

## Cach to chuc

Toan bo file duoc gom theo nhom chuc nang de de tim va de quan ly hon, trong khi ten file goc van giu nguyen de khong lam hong cach goi Lisp hien co.

```text
Lisp_Cad/
├─ README.md
├─ .gitignore
├─ 01_Text/
├─ 02_Block_Attribute/
├─ 03_Coordinate_Layout/
├─ 04_Utilities/
├─ THEP HINH/
└─ TONG HOP/
```

## Nhom file

### `01_Text/`
- `GM(chuyenMText).lsp`
- `KTD (chen ky tu Hy Lap).lsp`
- `UPCHU.lsp`
- `VTT(VIET CHU).lsp`

### `02_Block_Attribute/`
- `ATTS(SYNBLOCKATT).lsp`
- `BET(SUANBLOCKATT).lsp`
- `BSC (UPBLOCK ATT).lsp`
- `COPYTANGDAN(CPATT).lsp`
- `DBL (CHEN BLOG VAO TOA DO).lsp`
- `DanhTenAtt (DTEN).lsp`
- `TAT_ThemAttVaoBlock.lsp`
- `TAG(tag blog).lsp`
- `ThayBlock(TBL).lsp`
- `ldb (load block).lsp`

### `03_Coordinate_Layout/`
- `ALR (GIONG AL REVIT).lsp`
- `CD (DANH_CAO_DO).lsp`
- `DYN (VE BLOG).lsp`
- `DTD_DanhToaDo.lsp`
- `EDA (THAY DOI DISTANCE1).lsp`
- `HH (LAYER HIEN HANH).lsp`
- `KVP_KhoaViewport.lsp`
- `TBV.lsp`
- `TDD (LY_TRINH).lsp`

### `04_Utilities/`
- `BT.lsp`
- `BUN(doidonviblog).lsp`
- `DOAN.lsp`
- `MC.lsp`
- `SMV (tao MV).lsp`
- `THKL(tong_hop_KL).lsp`
- `VLX.LSP`

### `THEP HINH/`
- C# project `VCI.ShapeSteel.csproj`
- cac file `ShapeSteel*.cs`
- `SectionCalc.cs`
- `build.bat`

### `TONG HOP/`
- `DOAN.lsp` ban tong hop

## Ghi chu

- Thu muc `THEP HINH/bin/` va `THEP HINH/obj/` duoc bo qua trong Git.
- Cau truc nay chi thay doi cach sap xep trong repo, khong doi ten file goc.

## Su dung

1. Tai repo ve may.
2. Mo dung thu muc theo nhom chuc nang ban can.
3. Dung `APPLOAD` hoac co che load Lisp hien co de nap file can thiet.
4. Neu can build phan thep hinh, mo project trong Visual Studio.

## Plugin ong nuoc 2D

Module `05_Water_Pipe/` cung cap lenh `ONGNUOC` de ve tuyen ong 2D co thanh ong va tu dong tao cung bo tai cac diem doi huong. Xem `05_Water_Pipe/README.md`.

Ban native C++ ObjectARX duoc dat tai `06_Water_Pipe_ARX/`; day la huong build chinh neu can toc do xu ly toi da va khong dung C#.
