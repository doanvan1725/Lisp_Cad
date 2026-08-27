// ============================================================================
//  VCI Shape Steel - Section Properties Calculator
//
//  Tinh Area, Cx/Cy, Ix/Iy, ix/iy, Zx/Zy, Weight tu outline hinh hoc (mm).
//
//  Phuong phap:
//    1. Discretize cac arc (bulge != 0) thanh N straight segments
//    2. Ap dung Green's theorem cho polygon
//          Area     = (1/2) Sum (xi * yj - xj * yi)
//          Cx       = (1/6A) Sum (xi + xj) * cross
//          Cy       = (1/6A) Sum (yi + yj) * cross
//          Ix_orig  = (1/12) Sum (yi^2 + yi*yj + yj^2) * cross
//          Iy_orig  = (1/12) Sum (xi^2 + xi*xj + xj^2) * cross
//    3. Shift to centroid: Ix_c = Ix_orig - A * Cy^2
//
//  Don vi:
//    - Input: mm (coords)
//    - Output: cm2/cm4/cm3/cm  (don vi xay dung VN)
//    - Weight: kg/m (steel = 7.85 g/cm3 -> 0.785 kg/m per cm2)
// ============================================================================

using System;
using System.Collections.Generic;
using Autodesk.AutoCAD.Geometry;

namespace VCI.ShapeSteel
{
    /// <summary>Ket qua tinh dac trung mat cat.</summary>
    public class SectionProps
    {
        public double Area_cm2;
        public double Weight_kgm;
        public double Cx_cm;
        public double Cy_cm;
        public double Ix_cm4;
        public double Iy_cm4;
        public double Zx_cm3;
        public double Zy_cm3;
        public double ix_cm;
        public double iy_cm;
        public double H_mm;          // bounding height (y range)
        public double B_mm;          // bounding width  (x range)
        public int    VertexCount;   // so dinh polygon sau khi discretize
    }

    public static class SectionCalc
    {
        public const double STEEL_DENSITY_KG_PER_CM3 = 7.85e-3;  // 7.85 g/cm3 = 7.85e-3 kg/cm3
        public const double WEIGHT_FACTOR = 0.785;               // kg/m / cm2

        /// <summary>Tinh properties tu outline (don vi mm), arcSegments cao -> chinh xac hon</summary>
        public static SectionProps Calculate(List<PV> outline, int arcSegments = 24)
        {
            if (outline == null || outline.Count < 3)
                return new SectionProps();

            var pts = DiscretizeOutline(outline, arcSegments);
            return ComputePolygonProperties(pts);
        }

        /// <summary>Helper: tinh truc tiep tu profile + type</summary>
        public static SectionProps CalculateForProfile(SteelProfile p, ShapeType type, bool useBulge = true)
        {
            var outline = ShapeSteelDrawer.Outline(p, type, useBulge);
            return Calculate(outline);
        }

        // ====================================================================
        // DISCRETIZE OUTLINE
        // ====================================================================
        public static List<Point2d> DiscretizeOutline(List<PV> verts, int segmentsPerArc)
        {
            var pts = new List<Point2d>();
            int n = verts.Count;
            for (int i = 0; i < n; i++)
            {
                Point2d p1 = verts[i].P;
                Point2d p2 = verts[(i + 1) % n].P;
                double bulge = verts[i].Bulge;

                pts.Add(p1);

                if (Math.Abs(bulge) < 1e-9) continue;   // straight, just the start point

                // Discretize arc into intermediate points (skip first and last)
                double sweep = 4.0 * Math.Atan(bulge);  // signed sweep angle (positive=CCW)
                double chordLen = p1.GetDistanceTo(p2);
                if (chordLen < 1e-9) continue;
                double absSweep = Math.Abs(sweep);
                if (absSweep < 1e-9) continue;
                double radius = chordLen / (2.0 * Math.Sin(absSweep / 2.0));

                // Midpoint of chord
                double midX = (p1.X + p2.X) / 2.0;
                double midY = (p1.Y + p2.Y) / 2.0;
                // Perpendicular CCW direction to chord
                double dx = (p2.X - p1.X) / chordLen;
                double dy = (p2.Y - p1.Y) / chordLen;
                double perpX = -dy, perpY = dx;  // 90 deg CCW

                // Offset from midpoint to arc center
                //   For sweep < PI, center is on opposite side of arc from chord
                //   offset = radius * cos(sweep/2)
                double offset = radius * Math.Cos(absSweep / 2.0);
                if (absSweep > Math.PI) offset = -offset;   // for major arc

                double cx, cy;
                if (sweep > 0)  // CCW arc: center on LEFT of chord direction = +perpCCW
                {
                    cx = midX + perpX * offset;
                    cy = midY + perpY * offset;
                }
                else            // CW arc: center on RIGHT = -perpCCW
                {
                    cx = midX - perpX * offset;
                    cy = midY - perpY * offset;
                }

                // Start/end angles on circle (atan2 from center)
                double startAng = Math.Atan2(p1.Y - cy, p1.X - cx);

                // Generate intermediate points (k=1..segments-1 to skip start and end)
                for (int k = 1; k < segmentsPerArc; k++)
                {
                    double t = (double)k / segmentsPerArc;
                    double a = startAng + sweep * t;
                    pts.Add(new Point2d(cx + radius * Math.Cos(a),
                                        cy + radius * Math.Sin(a)));
                }
                // p2 will be added as next iteration's p1 - skip here
            }
            return pts;
        }

        // ====================================================================
        // POLYGON PROPERTIES (Green's theorem)
        // ====================================================================
        public static SectionProps ComputePolygonProperties(List<Point2d> pts)
        {
            int n = pts.Count;
            var result = new SectionProps { VertexCount = n };
            if (n < 3) return result;

            // First pass: compute signed area
            double A = 0;
            for (int i = 0; i < n; i++)
            {
                int j = (i + 1) % n;
                A += pts[i].X * pts[j].Y - pts[j].X * pts[i].Y;
            }
            A *= 0.5;

            // If CW, reverse to CCW
            List<Point2d> p = pts;
            if (A < 0)
            {
                p = new List<Point2d>(pts);
                p.Reverse();
                A = -A;
            }

            // Centroid + moments about origin
            double cx = 0, cy = 0;
            double IxO = 0, IyO = 0;
            for (int i = 0; i < n; i++)
            {
                int j = (i + 1) % n;
                double xi = p[i].X, yi = p[i].Y;
                double xj = p[j].X, yj = p[j].Y;
                double cross = xi * yj - xj * yi;
                cx += (xi + xj) * cross;
                cy += (yi + yj) * cross;
                IxO += (yi * yi + yi * yj + yj * yj) * cross;
                IyO += (xi * xi + xi * xj + xj * xj) * cross;
            }
            cx /= (6.0 * A);
            cy /= (6.0 * A);
            IxO /= 12.0;
            IyO /= 12.0;

            // Shift to centroid via parallel axis theorem
            double IxC = IxO - A * cy * cy;
            double IyC = IyO - A * cx * cx;

            // Bounding box for section modulus
            double xMin = double.MaxValue, xMax = double.MinValue;
            double yMin = double.MaxValue, yMax = double.MinValue;
            foreach (var pt in p)
            {
                if (pt.X < xMin) xMin = pt.X;
                if (pt.X > xMax) xMax = pt.X;
                if (pt.Y < yMin) yMin = pt.Y;
                if (pt.Y > yMax) yMax = pt.Y;
            }

            // Distance from centroid to extreme fiber
            double yExtreme = Math.Max(yMax - cy, cy - yMin);
            double xExtreme = Math.Max(xMax - cx, cx - xMin);

            // ===== Convert mm -> standard units =====
            // Area: mm^2 -> cm^2  (divide 100)
            // I:    mm^4 -> cm^4  (divide 10000)
            // r:    mm   -> cm    (divide 10)
            // Z:    mm^3 -> cm^3  (divide 1000)
            result.Area_cm2   = A / 100.0;
            result.Weight_kgm = WEIGHT_FACTOR * result.Area_cm2;
            result.Cx_cm      = cx / 10.0;
            result.Cy_cm      = cy / 10.0;
            result.Ix_cm4     = IxC / 10000.0;
            result.Iy_cm4     = IyC / 10000.0;
            result.Zx_cm3     = (yExtreme > 1e-9) ? (IxC / yExtreme) / 1000.0 : 0;
            result.Zy_cm3     = (xExtreme > 1e-9) ? (IyC / xExtreme) / 1000.0 : 0;
            result.ix_cm      = (A > 1e-9) ? Math.Sqrt(IxC / A) / 10.0 : 0;
            result.iy_cm      = (A > 1e-9) ? Math.Sqrt(IyC / A) / 10.0 : 0;
            result.H_mm       = yMax - yMin;
            result.B_mm       = xMax - xMin;
            return result;
        }

        // ====================================================================
        // FORMAT comparison report (computed vs hardcoded)
        // ====================================================================
        public static string FormatComparison(SteelProfile p, SectionProps calc)
        {
            return
                $"So sanh ket qua TINH so voi du lieu HARDCODED\n" +
                $"=========================================================\n" +
                $"  Profile: {p.Designation}  (H={p.H:F1} x B={p.B:F1} x t={p.t:F1} x tf={p.tf:F1})\n" +
                $"  r1={p.r1:F1} mm, r2={p.r2:F1} mm\n" +
                $"  Polygon vertices sau discretize: {calc.VertexCount}\n" +
                $"\n" +
                $"  Item        |  Hardcoded  |  Computed   |  Diff %\n" +
                $"  ------------+-------------+-------------+--------\n" +
                Row("Area cm2",  p.Area,   calc.Area_cm2) +
                Row("Weight kg/m", p.Weight, calc.Weight_kgm) +
                Row("Ix cm4",    p.Ix,     calc.Ix_cm4) +
                Row("Iy cm4",    p.Iy,     calc.Iy_cm4) +
                Row("ix cm",     p.ix,     calc.ix_cm) +
                Row("iy cm",     p.iy,     calc.iy_cm) +
                Row("Zx cm3",    p.Zx,     calc.Zx_cm3) +
                Row("Zy cm3",    p.Zy,     calc.Zy_cm3) +
                Row("Cx cm",     p.Cx,     calc.Cx_cm) +
                Row("Cy cm",     p.Cy,     calc.Cy_cm) +
                $"\nGhi chu: chenh lech do dac trung TCVN co the tinh\n" +
                $"theo phuong phap chuan hoa (lam tron, vat lieu thuc),\n" +
                $"trong khi 'Computed' chi tinh thuan tuy hinh hoc.";
        }

        private static string Row(string name, double hard, double calc)
        {
            string diff = "—";
            if (Math.Abs(hard) > 1e-6)
            {
                double pct = (calc - hard) / Math.Abs(hard) * 100.0;
                diff = pct >= 0 ? $"+{pct:F1}%" : $"{pct:F1}%";
            }
            return $"  {name,-11} | {hard,11:F3} | {calc,11:F3} | {diff,7}\n";
        }

        // ====================================================================
        // Recompute profile fields IN-PLACE (overwrite hardcoded with calc)
        // ====================================================================
        public static void RecomputeProfile(SteelProfile p, ShapeType type, bool useBulge = true)
        {
            var calc = CalculateForProfile(p, type, useBulge);
            p.Area = calc.Area_cm2;
            p.Weight = calc.Weight_kgm;
            p.Cx = calc.Cx_cm;
            p.Cy = calc.Cy_cm;
            p.Ix = calc.Ix_cm4;
            p.Iy = calc.Iy_cm4;
            p.ix = calc.ix_cm;
            p.iy = calc.iy_cm;
            p.Zx = calc.Zx_cm3;
            p.Zy = calc.Zy_cm3;
            // Don't overwrite Imax/Imin (those need principal axis rotation - more complex)
        }
    }
}
