// ============================================================================
//  VCI Shape Steel - Commands (NETLOAD version)
//  Khong co IExtensionApplication, khong co Ribbon
//  -> Build DLL -> NETLOAD trong AutoCAD -> Go lenh THEPHINH
// ============================================================================

using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.Geometry;
using Autodesk.AutoCAD.Runtime;

[assembly: CommandClass(typeof(VCI.ShapeSteel.ShapeSteelCommands))]

namespace VCI.ShapeSteel
{
    public class ShapeSteelCommands
    {
        private static ShapeSteelWindow _currentWindow;

        [CommandMethod("THEPHINH", CommandFlags.Session)]
        public void ThepHinhCmd()
        {
            Document doc = Application.DocumentManager.MdiActiveDocument;
            if (doc == null) return;
            if (_currentWindow != null) { _currentWindow.Activate(); return; }

            var win = new ShapeSteelWindow();
            _currentWindow = win;
            new System.Windows.Interop.WindowInteropHelper(win).Owner = Application.MainWindow.Handle;

            win.OnDraw = (profile, type, useBulge, asBlock) => DoDraw(doc, profile, type, useBulge, asBlock);
            win.Closed += (s, e) => { _currentWindow = null; };

            Application.ShowModelessWindow(win);
        }

        private int DoDraw(Document doc, SteelProfile profile, ShapeType type, bool useBulge, bool asBlock)
        {
            try
            {
                Application.MainWindow.Focus();
                var drawer = new ShapeSteelDrawer(doc);
                Point3d? p = drawer.PickInsertPoint(profile.Designation);
                if (!p.HasValue) return 0;
                return drawer.DrawSection(profile, type, p.Value, useBulge, asBlock);
            }
            catch (System.Exception ex)
            {
                doc.Editor.WriteMessage($"\n[VCI] Loi ve: {ex.Message}");
                System.Windows.MessageBox.Show("Loi ve mat cat: " + ex.Message);
                return 0;
            }
        }
    }
}
