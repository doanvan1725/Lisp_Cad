using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.Geometry;
using Autodesk.AutoCAD.ApplicationServices;
using System;
using System.Collections.Generic;

namespace VCI.WaterPipe
{
    internal static class PipeDrawer
    {
        private sealed class Bend
        {
            public Point3d Center, In, Out;
            public double Radius, Turn;
        }

        public static int Draw(Document doc, IReadOnlyList<Point3d> points, PipeSettings s, bool hollow)
        {
            if (points.Count < 2) return 0;
            s.Validate();
            Database db = doc.Database;
            using (doc.LockDocument())
            using (Transaction tr = db.TransactionManager.StartTransaction())
            {
                ObjectId layer = EnsureLayer(db, tr, s.LayerName, s.ColorIndex);
                BlockTable bt = (BlockTable)tr.GetObject(db.BlockTableId, OpenMode.ForRead);
                BlockTableRecord space = (BlockTableRecord)tr.GetObject(db.CurrentSpaceId, OpenMode.ForWrite);
                var bends = BuildBends(points, s.CenterlineRadius);
                double half = s.OuterDiameter / 2.0;
                int count = 0;

                foreach (int side in new[] { -1, 1 })
                {
                    for (int i = 0; i < points.Count - 1; i++)
                    {
                        Point3d a = i == 0 ? Offset(points[0], Direction(points[0], points[1]), side * half) : BendPoint(bends[i - 1], false, side, half);
                        Point3d b = i == points.Count - 2 ? Offset(points[^1], Direction(points[^2], points[^1]), side * half) : BendPoint(bends[i], true, side, half);
                        count += AddLine(space, tr, a, b, layer);
                    }
                    for (int i = 0; i < bends.Count; i++)
                    {
                        Bend x = bends[i];
                        double r = x.Radius - x.Turn * side * half;
                        if (r <= 0) continue;
                        Vector3d va = x.In - x.Center, vb = x.Out - x.Center;
                        double start = Math.Atan2(va.Y, va.X), end = Math.Atan2(vb.Y, vb.X);
                        if (x.Turn < 0) { double t = start; start = end; end = t; }
                        var arc = new Arc(x.Center, r, start, end) { LayerId = layer };
                        space.AppendEntity(arc); tr.AddNewlyCreatedDBObject(arc, true); count++;
                    }
                }
                count += AddLine(space, tr, Offset(points[0], Direction(points[0], points[1]), -half), Offset(points[0], Direction(points[0], points[1]), half), layer);
                count += AddLine(space, tr, Offset(points[^1], Direction(points[^2], points[^1]), -half), Offset(points[^1], Direction(points[^2], points[^1]), half), layer);

                if (hollow)
                {
                    double inner = Math.Max(0.1, half - s.WallThickness);
                    foreach (int side in new[] { -1, 1 })
                    {
                        for (int i = 0; i < points.Count - 1; i++)
                        {
                            Point3d a = i == 0 ? Offset(points[0], Direction(points[0], points[1]), side * inner) : BendPoint(bends[i - 1], false, side, inner);
                            Point3d b = i == points.Count - 2 ? Offset(points[^1], Direction(points[^2], points[^1]), side * inner) : BendPoint(bends[i], true, side, inner);
                            count += AddLine(space, tr, a, b, layer);
                        }
                        foreach (Bend x in bends)
                        {
                            double r = x.Radius - x.Turn * side * inner;
                            Vector3d va = x.In - x.Center, vb = x.Out - x.Center;
                            double start = Math.Atan2(va.Y, va.X), end = Math.Atan2(vb.Y, vb.X);
                            if (x.Turn < 0) { double t = start; start = end; end = t; }
                            var arc = new Arc(x.Center, r, start, end) { LayerId = layer };
                            space.AppendEntity(arc); tr.AddNewlyCreatedDBObject(arc, true); count++;
                        }
                    }
                }
                tr.Commit();
                return count;
            }
        }

        private static List<Bend> BuildBends(IReadOnlyList<Point3d> p, double radius)
        {
            var result = new List<Bend>();
            for (int i = 1; i < p.Count - 1; i++)
            {
                Vector3d u = Direction(p[i - 1], p[i]), v = Direction(p[i], p[i + 1]);
                double turn = u.X * v.Y - u.Y * v.X;
                double theta = Math.Acos(Math.Max(-1, Math.Min(1, -u.DotProduct(v))));
                if (theta < 0.001 || Math.Abs(turn) < 0.001) { result.Add(new Bend { Center = p[i], In = p[i], Out = p[i], Radius = radius, Turn = 1 }); continue; }
                double d = radius * Math.Tan(theta / 2), h = radius / Math.Sin(theta / 2);
                Vector3d bis = ((-u) + v).GetNormal();
                Point3d center = p[i] + bis * h;
                result.Add(new Bend { Center = center, In = p[i] - u * d, Out = p[i] + v * d, Radius = radius, Turn = Math.Sign(turn) });
            }
            return result;
        }

        private static Point3d BendPoint(Bend b, bool outgoing, int side, double offset)
        {
            Point3d t = outgoing ? b.Out : b.In;
            double r = b.Radius - b.Turn * side * offset;
            return b.Center + (t - b.Center).GetNormal() * r;
        }
        private static Vector3d Direction(Point3d a, Point3d b) => (b - a).GetNormal();
        private static Point3d Offset(Point3d p, Vector3d d, double distance) => p + new Vector3d(-d.Y, d.X, 0) * distance;
        private static int AddLine(BlockTableRecord space, Transaction tr, Point3d a, Point3d b, ObjectId layer)
        { var x = new Line(a, b) { LayerId = layer }; space.AppendEntity(x); tr.AddNewlyCreatedDBObject(x, true); return 1; }
        private static ObjectId EnsureLayer(Database db, Transaction tr, string name, short color)
        {
            LayerTable lt = (LayerTable)tr.GetObject(db.LayerTableId, OpenMode.ForRead);
            if (lt.Has(name)) return lt[name];
            lt.UpgradeOpen(); var rec = new LayerTableRecord { Name = name, Color = Autodesk.AutoCAD.Colors.Color.FromColorIndex(Autodesk.AutoCAD.Colors.ColorMethod.ByAci, color) };
            ObjectId id = lt.Add(rec); tr.AddNewlyCreatedDBObject(rec, true); return id;
        }
    }
}
