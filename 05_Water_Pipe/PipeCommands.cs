using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.EditorInput;
using Autodesk.AutoCAD.Geometry;
using Autodesk.AutoCAD.Runtime;
using System.Collections.Generic;
using System.Windows.Interop;

[assembly: CommandClass(typeof(VCI.WaterPipe.PipeCommands))]
namespace VCI.WaterPipe
{
    public class PipeCommands
    {
        private static PipeWindow _window;

        [CommandMethod("ONGNUOC", CommandFlags.Session)]
        public void OpenPipeTool()
        {
            Document doc = Application.DocumentManager.MdiActiveDocument;
            if (doc == null) return;
            if (_window != null) { _window.Activate(); return; }
            _window = new PipeWindow();
            new WindowInteropHelper(_window).Owner = Application.MainWindow.Handle;
            _window.DrawRequested = (settings, hollow) => DrawRoute(doc, settings, hollow);
            _window.Closed += (s, e) => _window = null;
            Application.ShowModelessWindow(_window);
        }

        private static void DrawRoute(Document doc, PipeSettings settings, bool hollow)
        {
            Editor ed = doc.Editor;
            var points = new List<Point3d>();
            PromptPointResult first = ed.GetPoint("\nChọn điểm đầu tuyến ống: ");
            if (first.Status != PromptStatus.OK) return;
            points.Add(first.Value);
            while (true)
            {
                var opt = new PromptPointOptions("\nChọn điểm kế tiếp (Enter để kết thúc): ") { BasePoint = points[^1], UseBasePoint = true, AllowNone = true };
                PromptPointResult next = ed.GetPoint(opt);
                if (next.Status != PromptStatus.OK) break;
                if (next.Value.DistanceTo(points[^1]) > 1e-6) points.Add(next.Value);
            }
            if (points.Count < 2) return;
            int n = PipeDrawer.Draw(doc, points, settings, hollow);
            ed.WriteMessage($"\n[VCI ỐNG NƯỚC] Đã tạo {n} đối tượng, OD={settings.OuterDiameter:g} mm, ID={settings.InnerDiameter:g} mm.");
        }
    }
}
