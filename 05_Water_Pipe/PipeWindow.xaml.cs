using System;
using System.Globalization;
using System.Windows;

namespace VCI.WaterPipe
{
    public partial class PipeWindow : Window
    {
        public Action<PipeSettings, bool> DrawRequested;
        public PipeWindow() { InitializeComponent(); }

        private bool Read(out PipeSettings s)
        {
            s = new PipeSettings();
            try
            {
                s.OuterDiameter = Number(OuterDiameterBox.Text);
                s.WallThickness = Number(WallThicknessBox.Text);
                s.ElbowThickness = Number(ElbowThicknessBox.Text);
                s.BendRadius = Number(BendRadiusBox.Text);
                s.Validate();
                return true;
            }
            catch (Exception ex) { StatusText.Text = ex.Message; return false; }
        }
        private static double Number(string value) => double.Parse(value, CultureInfo.CurrentCulture);
        private void DrawClick(object sender, RoutedEventArgs e) { if (Read(out var s)) DrawRequested?.Invoke(s, HollowBox.IsChecked == true); }
        private void CloseClick(object sender, RoutedEventArgs e) => Close();
    }
}
