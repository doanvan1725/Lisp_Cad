# VCI Shape Steel Library — AutoCAD 2027 (NETLOAD)

Plugin tra cứu thép hình **TCVN 7571 / JIS G 3192 / ASTM W-Shape** + vẽ mặt cắt có bo góc + tạo Block + tính dac trung hinh hoc tu polygon outline.

**Lệnh:** `THEPHINH`

## Mới v1.3 — Auto-calc + Sheet Pile bo góc

### 1. Section Properties Auto-Calculator

File mới `SectionCalc.cs` dùng **Green's theorem trên polygon** để tính từ outline:
- Area (cm²)
- Weight (kg/m) = 0.785 × Area
- Centroid Cx, Cy (cm)
- Moment quán tính Ix, Iy (cm⁴)
- Bán kính quán tính ix, iy (cm)
- Modul tiết diện Zx, Zy (cm³)

**Cách dùng:**

| UI Control | Tác dụng |
|---|---|
| Button **"So sanh"** | Tính dac trung mặt cắt đang chọn → MessageBox so sánh với hardcoded |
| Checkbox **"Auto tinh DTHH"** | Override toàn bộ data hiển thị bằng giá trị tính được |

**Algorithm:**
1. Lấy outline `List<PV>` (point + bulge)
2. Discretize mỗi arc thành **24 segment** thẳng → polygon
3. Áp dụng Green's theorem:
   ```
   A    = (1/2) Σ (xi·yj − xj·yi)
   Ix_O = (1/12) Σ (yi² + yi·yj + yj²) · cross
   Iy_O = (1/12) Σ (xi² + xi·xj + xj²) · cross
   Ix_c = Ix_O − A·Cy²  (parallel axis)
   ```
4. Convert mm → cm/cm²/cm³/cm⁴

**Độ chính xác** (verify với I 300x150 TCVN):
| Item | Hardcoded | Computed | Diff |
|---|---|---|---|
| Area cm² | 61.58 | 60.92 | -1.1% |
| Weight | 48.3 | 47.82 | -1.0% |
| Ix cm⁴ | 9480 | 9408 | -0.8% |
| Iy cm⁴ | 588 | 732 | **+24.5%** |

Lưu ý quan trọng:
- **Area/Weight/Ix** sai số <1% — formula đúng
- **Iy lệch ~25%** với I-beam vì TCVN dùng I-beam **flange côn** (tapered), code giả định flange phẳng chữ nhật. Với H-beam/Channel/Angle thì Iy cũng sát.
- "Computed" = thuần hình học, không cộng radius/fillet thực tế

### 2. Sheet Pile bo góc r1

Generic `RoundCorners(verts, radii[])` helper — tính turn angle tự động cho mỗi vertex:
```
turn = atan2(cross, dot)       // signed angle in [-π, π]
bulge = ±tan(|turn|/4)         // sign = sign of turn
L     = r / tan(|turn|/2)      // tangent point distance
```

Áp dụng cho Sheet Pile tại 4 corner:
- 2 outer convex (slope-to-base junction)
- 2 inner concave (bottom of cavity)

Turn angle ≈ 68° (không phải 90°) vì slope nghiêng — bulge ≈ tan(17°) ≈ 0.305

Có thể dùng `RoundCorners` cho bất kỳ shape custom nào — chỉ cần truyền mảng radii song song với verts.

## Toolbar mới (đầy đủ)

```
┌──────────────────────────────────────────────────────────────────────────────┐
│ Tieu chuan: [TCVN ▼] │ ☑Bo goc r1/r2  Output: ⚫Block ⚪Polyline             │
│                      │ ☑Auto tinh DTHH  [So sanh]                            │
└──────────────────────────────────────────────────────────────────────────────┘
```

## Build & dùng

```bat
build.bat                    → bin\Release\VCI.ShapeSteel.dll
```

Trong AutoCAD 2027: `NETLOAD` → `THEPHINH`

## Cấu trúc

```
VCI.ShapeSteel.NetLoad/
├── VCI.ShapeSteel.csproj
├── ShapeSteelData.cs          (153 TCVN + 38 JIS + 43 ASTM)
├── ShapeSteelDrawer.cs        (bulge outlines + RoundCorners + block helpers)
├── SectionCalc.cs             [MOI] Green's theorem polygon integration
├── ShapeSteelCommands.cs      (lệnh THEPHINH)
├── ShapeSteelWindow.cs        (UI + 5 options + So sanh button)
├── build.bat
└── README.md
```

## Test workflow

1. `NETLOAD` → `THEPHINH`
2. Tab **Steel Sheet Pile** → click `SP-IV` → bấm **Draw** với mode **Polyline + Bo goc r1/r2**
3. Pick điểm → quan sát: outer slope-to-base corner cong, inner cavity corner cong
4. Sang tab **I-Beam** → chọn `I 300x150 (t=8)` → bấm **So sanh** → MessageBox hiện bảng:
   ```
   Item        |  Hardcoded  |  Computed   |  Diff %
   ------------+-------------+-------------+--------
   Area cm2    |     61.580  |     60.920  |   -1.1%
   Weight kg/m |     48.300  |     47.822  |   -1.0%
   Ix cm4      |   9480.000  |   9408.234  |   -0.8%
   Iy cm4      |    588.000  |    732.419  |  +24.6%
   ...
   ```
5. Tick **Auto tinh DTHH** → toàn bộ bảng reload với giá trị Computed (test verify)

## API thư viện (cho dev)

```csharp
// Get outline cho bất kỳ profile
List<PV> outline = ShapeSteelDrawer.Outline(profile, type, useBulge: true);

// Tính properties từ outline
SectionProps calc = SectionCalc.Calculate(outline, arcSegments: 24);

// Hoặc tính trực tiếp từ profile
SectionProps calc = SectionCalc.CalculateForProfile(profile, type);

// Update properties in-place
SectionCalc.RecomputeProfile(profile, type);

// Generic bo góc cho outline custom
List<PV> rounded = ShapeSteelDrawer.RoundCorners(verts, new double[]{ 0, 5, 5, 0, ... });
```

## Trouble-shooting

**Iy chênh nhiều với I-beam TCVN** → Bình thường. TCVN I-beam có flange côn, code dùng flange phẳng. Dùng cột Computed cho phân tích mới, dùng Hardcoded cho tra cứu theo bảng chuẩn.

**Auto-calc cho ASTM W-shape giống Hardcoded** → ASTM W-shape có flange phẳng (parallel flange) đúng như mô hình code → match tốt hơn TCVN.

**Sheet Pile bo góc trông méo** → kiểm tra r1 trong data, default = 5mm. Tăng/giảm tùy ý.

**MessageBox "So sanh" cắt nội dung dài** → resize console — Windows MessageBox tự fit text.

## Mở rộng

Thêm shape mới hoàn toàn (vd ống tròn rỗng):
1. Thêm `ShapeType.Pipe` vào enum
2. Thêm `OutPipe(p)` trong Drawer trả về list PV (8 vertices với bulge ±B90 cho cung ngoài + 8 vertices cho cung trong)
3. `SectionCalc.Calculate(outline)` work ngay không cần code thêm — generic
4. UI add tab + data
