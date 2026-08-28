// ============================================================================
//  VCI Shape Steel - Drawer (v2: bo goc + block)
//  Don vi ve = mm
// ============================================================================

using System;
using System.Collections.Generic;
using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.EditorInput;
using Autodesk.AutoCAD.Geometry;

namespace VCI.ShapeSteel
{
    /// <summary>1 dinh polyline + bulge toi dinh ke tiep</summary>
    public struct PV
    {
        public Point2d P;
        public double Bulge;
        public PV(double x, double y, double b) { P = new Point2d(x, y); Bulge = b; }
        public PV(Point2d pt, double b) { P = pt; Bulge = b; }
    }

    public class ShapeSteelDrawer
    {
        private static readonly double B90 = Math.Tan(Math.PI / 8.0);   // ~0.41421 cho cung 90 do

        private readonly Document _doc;
        private readonly Database _db;
        private readonly Editor _ed;

        public ShapeSteelDrawer(Document doc) { _doc = doc; _db = doc.Database; _ed = doc.Editor; }

        public Point3d? PickInsertPoint(string designation)
        {
            var r = _ed.GetPoint(new PromptPointOptions(
                $"\n[VCI THEPHINH] Pick diem dat mat cat '{designation}': "));
            return r.Status == PromptStatus.OK ? (Point3d?)r.Value : null;
        }

        /// <summary>Ve mat cat. useBulge=co bo goc r1/r2. asBlock=tao block thay vi polyline.</summary>
        public int DrawSection(SteelProfile p, ShapeType type, Point3d ins,
                                bool useBulge, bool asBlock)
        {
            using (_doc.LockDocument())
            using (Transaction tr = _db.TransactionManager.StartTransaction())
            {
                BlockTableRecord space = (BlockTableRecord)tr.GetObject(_db.CurrentSpaceId, OpenMode.ForWrite);
                string layerName = "VCI_ThepHinh";
                EnsureLayer(tr, layerName, 5);

                List<PV> verts = Outline(p, type, useBulge);

                if (asBlock)
                {
                    ObjectId btrId = EnsureBlockDef(tr, p, verts, layerName);
                    var br = new BlockReference(ins, btrId) { Layer = layerName };
                    space.AppendEntity(br);
                    tr.AddNewlyCreatedDBObject(br, true);
                }
                else
                {
                    Polyline pl = BuildPolyline(verts);
                    pl.Layer = layerName;
                    pl.TransformBy(Matrix3d.Displacement(new Vector3d(ins.X, ins.Y, 0)));
                    space.AppendEntity(pl);
                    tr.AddNewlyCreatedDBObject(pl, true);

                    DBText txt = new DBText
                    {
                        TextString = p.Designation,
                        Position = new Point3d(ins.X, ins.Y - p.H * 0.15, 0),
                        Height = p.H * 0.08,
                        Layer = layerName
                    };
                    space.AppendEntity(txt);
                    tr.AddNewlyCreatedDBObject(txt, true);
                }

                tr.Commit();
                _ed.WriteMessage(
                    $"\n[VCI] {(asBlock ? "Insert block" : "Ve polyline")} '{p.Designation}' " +
                    $"tai ({ins.X:F0},{ins.Y:F0}). Bo goc: {(useBulge ? "co" : "khong")}.");
                return 1;
            }
        }

        // --- Block definition ----------------------------------------------
        private ObjectId EnsureBlockDef(Transaction tr, SteelProfile p, List<PV> verts, string layerName)
        {
            BlockTable bt = (BlockTable)tr.GetObject(_db.BlockTableId, OpenMode.ForRead);
            string blockName = "VCI_" + SafeBlockName(p.Designation);

            if (bt.Has(blockName))
                return bt[blockName];

            bt.UpgradeOpen();
            BlockTableRecord btr = new BlockTableRecord
            {
                Name = blockName,
                Origin = Point3d.Origin
            };
            ObjectId btrId = bt.Add(btr);
            tr.AddNewlyCreatedDBObject(btr, true);

            // Polyline outline
            Polyline pl = BuildPolyline(verts);
            pl.Layer = layerName;
            btr.AppendEntity(pl);
            tr.AddNewlyCreatedDBObject(pl, true);

            // Label text
            DBText txt = new DBText
            {
                TextString = p.Designation,
                Position = new Point3d(0, -p.H * 0.15, 0),
                Height = p.H * 0.08,
                Layer = layerName
            };
            btr.AppendEntity(txt);
            tr.AddNewlyCreatedDBObject(txt, true);

            _ed.WriteMessage($"\n[VCI] Tao block: {blockName}");
            return btrId;
        }

        private static string SafeBlockName(string s)
        {
            var sb = new System.Text.StringBuilder();
            foreach (char c in s)
            {
                if (char.IsLetterOrDigit(c) || c == '_' || c == '-' || c == '.') sb.Append(c);
                else if (c == ' ') sb.Append('_');
                else sb.Append('_');
            }
            return sb.ToString();
        }

        private static Polyline BuildPolyline(List<PV> verts)
        {
            Polyline pl = new Polyline();
            for (int i = 0; i < verts.Count; i++)
                pl.AddVertexAt(i, verts[i].P, verts[i].Bulge, 0, 0);
            pl.Closed = true;
            return pl;
        }

        // ====================================================================
        // OUTLINE GENERATORS - exposed PUBLIC for SectionCalc su dung
        //   Vertices i CCW (counter-clockwise) - quanh contour ngoai cua shape
        //   Bulge dinh thu i = tan(angle/4) cho arc TU dinh i DEN dinh i+1
        //     + dau duong = arc CCW (convex outward khi outline CCW)
        //     + dau am   = arc CW  (concave inward khi outline CCW)
        // ====================================================================
        public static List<PV> Outline(SteelProfile p, ShapeType type, bool useBulge)
        {
            switch (type)
            {
                case ShapeType.EqualAngle:
                case ShapeType.UnequalAngle:
                    return useBulge ? OutAngleR(p) : OutAngleSquare(p);
                case ShapeType.IBeam:
                case ShapeType.HBeam:
                    return useBulge ? OutIBeamR(p) : OutIBeamSquare(p);
                case ShapeType.Channel:
                    return useBulge ? OutChannelR(p) : OutChannelSquare(p);
                case ShapeType.SheetPile:
                    return useBulge ? OutSheetPileR(p) : OutSheetPile(p);
                default:
                    return OutIBeamSquare(p);
            }
        }

        // --- ANGLE ---
        private static List<PV> OutAngleSquare(SteelProfile p) => new List<PV>
        {
            new PV(0, 0, 0), new PV(p.B, 0, 0), new PV(p.B, p.t, 0),
            new PV(p.t, p.t, 0), new PV(p.t, p.H, 0), new PV(0, p.H, 0),
        };

        private static List<PV> OutAngleR(SteelProfile p)
        {
            // r1 (root inner): co the lon den ~min(leg length - t)
            //   - khong duoc lan qua dau canh -> r1 < min(B-t, H-t)
            double maxR1 = Math.Min(p.B - p.t, p.H - p.t) * 0.95;
            double r1 = Math.Max(0.1, Math.Min(p.r1, maxR1));
            // r2 (toe outer): bo dau canh, < leg thickness
            //   - tu (B, t-r2) den (B-r2, t) -> r2 < t
            double r2 = Math.Max(0.1, Math.Min(p.r2, p.t * 0.95));
            return new List<PV>
            {
                new PV(0, 0, 0),
                new PV(p.B, 0, 0),
                new PV(p.B, p.t - r2, +B90),            // toe convex
                new PV(p.B - r2, p.t, 0),
                new PV(p.t + r1, p.t, -B90),            // root concave
                new PV(p.t, p.t + r1, 0),
                new PV(p.t, p.H - r2, +B90),            // toe convex
                new PV(p.t - r2, p.H, 0),
                new PV(0, p.H, 0),
            };
        }

        // --- I-BEAM / H-BEAM ---
        private static List<PV> OutIBeamSquare(SteelProfile p)
        {
            double hb = p.B / 2.0, ht = p.t / 2.0;
            return new List<PV>
            {
                new PV(0, 0, 0), new PV(p.B, 0, 0),
                new PV(p.B, p.tf, 0), new PV(hb + ht, p.tf, 0),
                new PV(hb + ht, p.H - p.tf, 0), new PV(p.B, p.H - p.tf, 0),
                new PV(p.B, p.H, 0), new PV(0, p.H, 0),
                new PV(0, p.H - p.tf, 0), new PV(hb - ht, p.H - p.tf, 0),
                new PV(hb - ht, p.tf, 0), new PV(0, p.tf, 0),
            };
        }

        private static List<PV> OutIBeamR(SteelProfile p)
        {
            // Voi I-beam (TCVN 7571-15) co ca r1 + r2 -> ve 20 vertices
            // Voi H-beam (TCVN 7571-16) chi co 1 r duy nhat (r1=r2) hoac r2=0 -> ve 12 vertices
            //   Phan biet bang viec kiem tra r2 co dang ke khong so voi r1
            //   Neu r2 ~ r1 -> coi nhu khong co toe radius rieng (H-beam style)
            //   Neu r2 < r1 ro rang (vd I-beam: r2 = r1/2) -> ve full 20 vertices
            bool hasToeRadius = (p.r2 > 0.1) && (p.r2 < p.r1 * 0.8);
            return hasToeRadius ? OutIBeamFullR(p) : OutIBeamRootOnly(p);
        }

        /// <summary>I-beam / H-beam voi CHI root radius r1 (12 vertices)</summary>
        private static List<PV> OutIBeamRootOnly(SteelProfile p)
        {
            double hb = p.B / 2.0, ht = p.t / 2.0;
            // FIX: clamp dung theo geometry thuc te
            //   - r khong qua tf (flange thickness)
            //   - r khong qua (B-t)/2 (half flange free width)
            //   KHONG dung ht*0.9 vi ht = t/2 (half WEB thickness) qua nho cho H-beam
            double maxR = Math.Min(p.tf, (p.B - p.t) / 2.0) * 0.95;
            double r = Math.Max(0.1, Math.Min(p.r1, maxR));
            return new List<PV>
            {
                new PV(0, 0, 0),
                new PV(p.B, 0, 0),
                new PV(p.B, p.tf, 0),
                new PV(hb + ht + r, p.tf, -B90),            // 4 root concave
                new PV(hb + ht, p.tf + r, 0),
                new PV(hb + ht, p.H - p.tf - r, -B90),
                new PV(hb + ht + r, p.H - p.tf, 0),
                new PV(p.B, p.H - p.tf, 0),
                new PV(p.B, p.H, 0),
                new PV(0, p.H, 0),
                new PV(0, p.H - p.tf, 0),
                new PV(hb - ht - r, p.H - p.tf, -B90),
                new PV(hb - ht, p.H - p.tf - r, 0),
                new PV(hb - ht, p.tf + r, -B90),
                new PV(hb - ht - r, p.tf, 0),
                new PV(0, p.tf, 0),
            };
        }

        /// <summary>I-beam day du voi ca root r1 + toe r2 (20 vertices)</summary>
        private static List<PV> OutIBeamFullR(SteelProfile p)
        {
            double hb = p.B / 2.0, ht = p.t / 2.0;
            double maxR1 = Math.Min(p.tf, (p.B - p.t) / 2.0) * 0.95;
            double r1 = Math.Max(0.1, Math.Min(p.r1, maxR1));
            double r2 = Math.Max(0.1, Math.Min(p.r2, p.tf * 0.95));
            return new List<PV>
            {
                new PV(0, 0, 0),
                new PV(p.B, 0, 0),
                new PV(p.B, p.tf - r2, +B90),               // 4 toe convex
                new PV(p.B - r2, p.tf, 0),
                new PV(hb + ht + r1, p.tf, -B90),           // 4 root concave
                new PV(hb + ht, p.tf + r1, 0),
                new PV(hb + ht, p.H - p.tf - r1, -B90),
                new PV(hb + ht + r1, p.H - p.tf, 0),
                new PV(p.B - r2, p.H - p.tf, +B90),
                new PV(p.B, p.H - p.tf + r2, 0),
                new PV(p.B, p.H, 0),
                new PV(0, p.H, 0),
                new PV(0, p.H - p.tf + r2, +B90),
                new PV(r2, p.H - p.tf, 0),
                new PV(hb - ht - r1, p.H - p.tf, -B90),
                new PV(hb - ht, p.H - p.tf - r1, 0),
                new PV(hb - ht, p.tf + r1, -B90),
                new PV(hb - ht - r1, p.tf, 0),
                new PV(r2, p.tf, +B90),
                new PV(0, p.tf - r2, 0),
            };
        }

        // --- CHANNEL ---
        private static List<PV> OutChannelSquare(SteelProfile p) => new List<PV>
        {
            new PV(0, 0, 0), new PV(p.B, 0, 0),
            new PV(p.B, p.tf, 0), new PV(p.t, p.tf, 0),
            new PV(p.t, p.H - p.tf, 0), new PV(p.B, p.H - p.tf, 0),
            new PV(p.B, p.H, 0), new PV(0, p.H, 0),
        };

        private static List<PV> OutChannelR(SteelProfile p)
        {
            // FIX: clamp dung theo geometry
            //   r1 (root inner) <= min(tf, B-t)  -- khong qua flange dau, khong qua web
            //   r2 (toe outer)  <= tf / 2        -- bo dau flange tip
            double maxR1 = Math.Min(p.tf, p.B - p.t) * 0.95;
            double r1 = Math.Max(0.1, Math.Min(p.r1, maxR1));
            double r2 = Math.Max(0.1, Math.Min(p.r2, p.tf * 0.5));
            return new List<PV>
            {
                new PV(0, 0, 0),
                new PV(p.B - r2, 0, +B90),                  // toe convex
                new PV(p.B, r2, 0),
                new PV(p.B, p.tf, 0),
                new PV(p.t + r1, p.tf, -B90),               // root concave
                new PV(p.t, p.tf + r1, 0),
                new PV(p.t, p.H - p.tf - r1, 0),
                new PV(p.t + r1, p.H - p.tf, -B90),         // root concave
                new PV(p.B, p.H - p.tf, 0),
                new PV(p.B, p.H - r2, +B90),                // toe convex
                new PV(p.B - r2, p.H, 0),
                new PV(0, p.H, 0),
            };
        }

        // --- SHEET PILE ---
        private static List<PV> OutSheetPile(SteelProfile p)
        {
            // 8 vertices forming U-shape with sloped walls
            //   CCW from top-left:
            //   0: (0, H)              top-left outer
            //   1: (slope, 0)          bottom-left outer (slope-to-base corner)
            //   2: (B-slope, 0)        bottom-right outer (slope-to-base corner)
            //   3: (B, H)              top-right outer
            //   4: (B-t, H)            top-right inner
            //   5: (B-slope-t/2, t)    bottom-right inner
            //   6: (slope+t/2, t)      bottom-left inner
            //   7: (t, H)              top-left inner
            double slope = p.H * 0.4;
            return new List<PV>
            {
                new PV(0, p.H, 0),
                new PV(slope, 0, 0),
                new PV(p.B - slope, 0, 0),
                new PV(p.B, p.H, 0),
                new PV(p.B - p.t, p.H, 0),
                new PV(p.B - slope - p.t * 0.5, p.t, 0),
                new PV(slope + p.t * 0.5, p.t, 0),
                new PV(p.t, p.H, 0),
            };
        }

        private static List<PV> OutSheetPileR(SteelProfile p)
        {
            // Bo goc tai 4 internal junction: 2 outer slope-to-base + 2 inner bottom corners
            double r1 = Math.Max(p.r1, 1.0);  // root radius (min 1mm)
            var square = OutSheetPile(p);
            double[] radii = new double[]
            {
                0,    // 0: top-left outer (sharp)
                r1,   // 1: bottom-left outer  (CONVEX corner, slope meets base)
                r1,   // 2: bottom-right outer (CONVEX corner)
                0,    // 3: top-right outer (sharp)
                0,    // 4: top-right inner (sharp - leg tip)
                r1,   // 5: bottom-right inner (CONCAVE corner of cavity)
                r1,   // 6: bottom-left inner  (CONCAVE corner of cavity)
                0,    // 7: top-left inner (sharp - leg tip)
            };
            return RoundCorners(square, radii);
        }

        // ====================================================================
        // GENERIC CORNER ROUNDING
        //   Replace each sharp vertex by 2 vertices (start/end of arc) + bulge
        //   Bulge sign auto-computed from turn angle:
        //     - turn left (CCW outline, convex out) -> bulge dương
        //     - turn right (CCW outline, concave in) -> bulge âm
        // ====================================================================
        public static List<PV> RoundCorners(List<PV> verts, double[] radii)
        {
            int n = verts.Count;
            if (radii == null || radii.Length != n)
                throw new ArgumentException("radii length must match verts count");

            var result = new List<PV>();
            for (int i = 0; i < n; i++)
            {
                double r = radii[i];
                Point2d pCur = verts[i].P;

                if (r <= 0)
                {
                    result.Add(verts[i]);
                    continue;
                }

                Point2d pPrev = verts[(i - 1 + n) % n].P;
                Point2d pNext = verts[(i + 1) % n].P;

                double dxIn = pCur.X - pPrev.X, dyIn = pCur.Y - pPrev.Y;
                double dxOut = pNext.X - pCur.X, dyOut = pNext.Y - pCur.Y;
                double lenIn = Math.Sqrt(dxIn * dxIn + dyIn * dyIn);
                double lenOut = Math.Sqrt(dxOut * dxOut + dyOut * dyOut);
                if (lenIn < 1e-9 || lenOut < 1e-9)
                {
                    result.Add(verts[i]);
                    continue;
                }
                dxIn /= lenIn; dyIn /= lenIn;
                dxOut /= lenOut; dyOut /= lenOut;

                // Turn angle from vIn to vOut: signed angle in [-PI, PI]
                double crossZ = dxIn * dyOut - dyIn * dxOut;
                double dot = dxIn * dxOut + dyIn * dyOut;
                double turn = Math.Atan2(crossZ, dot);

                if (Math.Abs(turn) < 0.01)
                {
                    result.Add(verts[i]);
                    continue;
                }

                // Distance along each edge from corner to arc tangent point
                double halfTurn = Math.Abs(turn) / 2.0;
                double tanHalf = Math.Tan(halfTurn);
                if (tanHalf < 1e-9) { result.Add(verts[i]); continue; }
                double L = r / tanHalf;

                // Clamp L to fraction of adjacent edge lengths
                L = Math.Min(L, Math.Min(lenIn, lenOut) * 0.45);

                // Tangent points
                double ax = pCur.X - dxIn * L;
                double ay = pCur.Y - dyIn * L;
                double bx = pCur.X + dxOut * L;
                double by = pCur.Y + dyOut * L;

                // Bulge magnitude = tan(turn / 4); sign = sign of turn
                double bulge = Math.Tan(Math.Abs(turn) / 4.0);
                if (turn < 0) bulge = -bulge;

                result.Add(new PV(ax, ay, bulge));
                result.Add(new PV(bx, by, 0));
            }
            return result;
        }

        // ====================================================================
        // Layer helper
        // ====================================================================
        private void EnsureLayer(Transaction tr, string name, short colorIndex)
        {
            LayerTable lt = (LayerTable)tr.GetObject(_db.LayerTableId, OpenMode.ForRead);
            if (lt.Has(name)) return;
            lt.UpgradeOpen();
            LayerTableRecord layer = new LayerTableRecord
            {
                Name = name,
                Color = Autodesk.AutoCAD.Colors.Color.FromColorIndex(
                    Autodesk.AutoCAD.Colors.ColorMethod.ByAci, colorIndex)
            };
            lt.Add(layer);
            tr.AddNewlyCreatedDBObject(layer, true);
        }
    }
}
