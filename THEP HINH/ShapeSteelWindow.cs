// ============================================================================
//  VCI Shape Steel Library - WPF Window (giao dien giong anh mau)
//  Light theme: nen xam nhat, header xanh
// ============================================================================

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace VCI.ShapeSteel
{
    public class ShapeSteelWindow : Window
    {
        // Func: profile, type, useBulge, asBlock -> count drawn
        public Func<SteelProfile, ShapeType, bool, bool, int> OnDraw { get; set; }

        private TabControl _tabs;
        private DataGrid _dgEA, _dgUA, _dgIB, _dgCh, _dgHB, _dgSP;
        private Canvas _previewCanvas;
        private ComboBox _cbManufacturer;
        private CheckBox _chkBulge;
        private CheckBox _chkAutoCalc;
        private RadioButton _rbBlock, _rbPolyline;

        // === Palette light theme (giong anh user upload) ===
        private static readonly Brush BG_WINDOW = B(0xF0, 0xF0, 0xF0);
        private static readonly Brush BG_PANEL  = Brushes.White;
        private static readonly Brush BG_HEADER = B(0xEA, 0xEA, 0xEA);
        private static readonly Brush BG_GRID_HEADER = B(0xDD, 0xDD, 0xDD);
        private static readonly Brush BG_GRID_ALT  = B(0xFA, 0xFA, 0xFA);
        private static readonly Brush BG_HIGHLIGHT = B(0xFF, 0xFF, 0xCC);  // yellow highlight common
        private static readonly Brush BG_BUTTON       = B(0xE0, 0xE0, 0xE0);
        private static readonly Brush BG_BUTTON_HOVER = B(0xCC, 0xE6, 0xFF);
        private static readonly Brush FG_BLACK = Brushes.Black;
        private static readonly Brush FG_MUTED = B(0x66, 0x66, 0x66);
        private static readonly Brush FG_TEAL  = B(0x14, 0x9F, 0xA8);
        private static readonly Brush FG_BLUE  = B(0x14, 0x68, 0x82);
        private static readonly Brush BORDER   = B(0xAA, 0xAA, 0xAA);
        private static Brush B(byte r, byte g, byte b) => new SolidColorBrush(Color.FromRgb(r, g, b));

        public ShapeSteelWindow()
        {
            Title = "Shape Steel Library - VCI";
            Width = 920; Height = 600;
            WindowStartupLocation = WindowStartupLocation.Manual;
            Left = 100; Top = 60;
            Background = BG_WINDOW;
            Foreground = FG_BLACK;
            FontFamily = new FontFamily("Segoe UI");
            FontSize = 12;
            ResizeMode = ResizeMode.CanResize;

            TextOptions.SetTextFormattingMode(this, TextFormattingMode.Display);
            TextOptions.SetTextRenderingMode(this, TextRenderingMode.ClearType);

            Resources.Add(typeof(TextBlock), StyleTextBlock());
            Resources.Add(typeof(Button), StyleButton());
            Resources.Add(typeof(DataGrid), StyleDG());
            Resources.Add(typeof(DataGridColumnHeader), StyleDGHeader());
            Resources.Add(typeof(DataGridCell), StyleDGCell());

            BuildLayout();
            PopulateData();
        }

        private void BuildLayout()
        {
            var root = new Grid();
            root.RowDefinitions.Add(new RowDefinition { Height = GridLength.Auto });
            root.RowDefinitions.Add(new RowDefinition { Height = GridLength.Auto });
            root.RowDefinitions.Add(new RowDefinition { Height = new GridLength(1, GridUnitType.Star) });
            Content = root;

            // ===== HEADER =====
            var header = new Border
            {
                Background = BG_PANEL, BorderBrush = BORDER, BorderThickness = new Thickness(0, 0, 0, 1),
                Padding = new Thickness(12, 10, 12, 10)
            };
            var hgrid = new Grid();
            hgrid.ColumnDefinitions.Add(new ColumnDefinition { Width = GridLength.Auto });
            hgrid.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(1, GridUnitType.Star) });
            hgrid.ColumnDefinitions.Add(new ColumnDefinition { Width = GridLength.Auto });
            hgrid.ColumnDefinitions.Add(new ColumnDefinition { Width = GridLength.Auto });

            // Logo
            var logo = MakeVciLogo(50);
            Grid.SetColumn(logo, 0); hgrid.Children.Add(logo);

            // Company info
            var info = new StackPanel { Margin = new Thickness(12, 0, 0, 0), VerticalAlignment = VerticalAlignment.Center };
            info.Children.Add(new TextBlock
            {
                Text = "VCI - Cong ty Co phan Cong nghe so VCI",
                FontSize = 12, Foreground = FG_BLUE, FontWeight = FontWeights.SemiBold
            });
            info.Children.Add(new TextBlock
            {
                Text = "vcijsc.com  -  Thu vien thep hinh theo TCVN 7571",
                FontSize = 10, Foreground = FG_MUTED
            });
            info.Children.Add(new TextBlock
            {
                Text = "Lenh AutoCAD: THEPHINH",
                FontSize = 10, Foreground = FG_TEAL, FontWeight = FontWeights.SemiBold
            });
            Grid.SetColumn(info, 1); hgrid.Children.Add(info);

            // Draw button
            var btnDraw = new Button
            {
                Content = "Draw", Width = 90, Height = 30, FontWeight = FontWeights.SemiBold,
                Margin = new Thickness(8, 0, 4, 0),
                Background = B(0x14, 0x9F, 0xA8),
                Foreground = Brushes.White
            };
            btnDraw.Click += (s, e) => DoDraw();
            Grid.SetColumn(btnDraw, 2); hgrid.Children.Add(btnDraw);

            var btnClose = new Button
            {
                Content = "Close", Width = 90, Height = 30,
                Margin = new Thickness(4, 0, 0, 0)
            };
            btnClose.Click += (s, e) => Close();
            Grid.SetColumn(btnClose, 3); hgrid.Children.Add(btnClose);

            header.Child = hgrid;
            Grid.SetRow(header, 0); root.Children.Add(header);

            // ===== MANUFACTURER ROW + OPTIONS =====
            var manuRow = new Border
            {
                Background = BG_HEADER, BorderBrush = BORDER,
                BorderThickness = new Thickness(0, 0, 0, 1),
                Padding = new Thickness(12, 6, 12, 6)
            };
            var manuSp = new StackPanel { Orientation = Orientation.Horizontal };

            // Standard dropdown
            manuSp.Children.Add(new TextBlock
            {
                Text = "Tieu chuan:", FontWeight = FontWeights.SemiBold,
                VerticalAlignment = VerticalAlignment.Center, Margin = new Thickness(0, 0, 8, 0)
            });
            _cbManufacturer = new ComboBox { Width = 230, Height = 22 };
            _cbManufacturer.Items.Add("TCVN 7571 (Viet Nam)");
            _cbManufacturer.Items.Add("JIS G 3192 (Nhat)");
            _cbManufacturer.Items.Add("ASTM A6 / W-Shape (My)");
            _cbManufacturer.SelectedIndex = 0;
            _cbManufacturer.SelectionChanged += (s, e) => ReloadDataForStandard();
            manuSp.Children.Add(_cbManufacturer);

            // Separator
            manuSp.Children.Add(new Border
            {
                Width = 1, Background = BORDER,
                Margin = new Thickness(16, 2, 16, 2)
            });

            // Bo goc checkbox
            _chkBulge = new CheckBox
            {
                Content = "Bo goc r1/r2",
                IsChecked = true,
                VerticalAlignment = VerticalAlignment.Center,
                FontWeight = FontWeights.SemiBold,
                Margin = new Thickness(0, 0, 16, 0),
                ToolTip = "Tao polyline voi bulge bo goc theo r1 (root) va r2 (toe)"
            };
            manuSp.Children.Add(_chkBulge);

            // Output mode radio buttons
            manuSp.Children.Add(new TextBlock
            {
                Text = "Output:", FontWeight = FontWeights.SemiBold,
                VerticalAlignment = VerticalAlignment.Center,
                Margin = new Thickness(0, 0, 6, 0)
            });
            _rbBlock = new RadioButton
            {
                Content = "Block", IsChecked = true,
                GroupName = "OutputMode",
                VerticalAlignment = VerticalAlignment.Center,
                Margin = new Thickness(0, 0, 10, 0),
                ToolTip = "Tao block definition VCI_<ten>, insert block reference"
            };
            _rbPolyline = new RadioButton
            {
                Content = "Polyline",
                GroupName = "OutputMode",
                VerticalAlignment = VerticalAlignment.Center,
                ToolTip = "Ve polyline truc tiep, khong nhom vao block"
            };
            manuSp.Children.Add(_rbBlock);
            manuSp.Children.Add(_rbPolyline);

            // Separator
            manuSp.Children.Add(new Border
            {
                Width = 1, Background = BORDER,
                Margin = new Thickness(16, 2, 16, 2)
            });

            // Auto-recalc checkbox
            _chkAutoCalc = new CheckBox
            {
                Content = "Auto tinh DTHH",
                IsChecked = false,
                VerticalAlignment = VerticalAlignment.Center,
                FontWeight = FontWeights.SemiBold,
                Margin = new Thickness(0, 0, 12, 0),
                ToolTip = "Tick = override du lieu hardcoded bang gia tri TINH tu hinh hoc " +
                          "(Area, Weight, Ix/Iy, ix/iy, Zx/Zy, Cx/Cy theo Green's theorem)"
            };
            _chkAutoCalc.Checked   += (s, e) => ReloadDataForStandard();
            _chkAutoCalc.Unchecked += (s, e) => ReloadDataForStandard();
            manuSp.Children.Add(_chkAutoCalc);

            // Recalc selected button
            var btnRecalc = new Button
            {
                Content = "So sanh",
                Width = 75, Height = 22,
                FontSize = 11,
                ToolTip = "Tinh dac trung hinh hoc cua mat cat dang chon va so sanh voi hardcoded"
            };
            btnRecalc.Click += (s, e) => CompareSelected();
            manuSp.Children.Add(btnRecalc);

            manuRow.Child = manuSp;
            Grid.SetRow(manuRow, 1); root.Children.Add(manuRow);

            // ===== TAB CONTROL + PREVIEW =====
            var bodyGrid = new Grid { Margin = new Thickness(8, 8, 8, 8) };
            bodyGrid.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(1, GridUnitType.Star) });
            bodyGrid.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(180) });

            _tabs = new TabControl { Background = BG_PANEL, BorderBrush = BORDER };
            _tabs.Items.Add(MakeTab("Equal Angles",   out _dgEA));
            _tabs.Items.Add(MakeTab("Unequal Angles", out _dgUA));
            _tabs.Items.Add(MakeTab("I-Beam",         out _dgIB));
            _tabs.Items.Add(MakeTab("Channels",       out _dgCh));
            _tabs.Items.Add(MakeTab("H-Beam",         out _dgHB));
            _tabs.Items.Add(MakeTab("Steel Sheet Pile", out _dgSP));
            _tabs.SelectionChanged += (s, e) => UpdatePreview();
            Grid.SetColumn(_tabs, 0);
            bodyGrid.Children.Add(_tabs);

            // Preview panel ben phai
            var previewBorder = new Border
            {
                Background = BG_PANEL, BorderBrush = BORDER, BorderThickness = new Thickness(1),
                Margin = new Thickness(8, 0, 0, 0), Padding = new Thickness(8)
            };
            var previewSp = new StackPanel();
            previewSp.Children.Add(new TextBlock
            {
                Text = "MAT CAT", FontWeight = FontWeights.Bold, FontSize = 11,
                HorizontalAlignment = HorizontalAlignment.Center, Foreground = FG_TEAL,
                Margin = new Thickness(0, 0, 0, 6)
            });
            _previewCanvas = new Canvas
            {
                Width = 160, Height = 160, Background = B(0xFA, 0xFA, 0xFA),
                ClipToBounds = true
            };
            previewSp.Children.Add(_previewCanvas);
            previewSp.Children.Add(new TextBlock
            {
                Text = "Don vi: mm", FontSize = 10, Foreground = FG_MUTED,
                HorizontalAlignment = HorizontalAlignment.Center, Margin = new Thickness(0, 4, 0, 0)
            });
            previewBorder.Child = previewSp;
            Grid.SetColumn(previewBorder, 1);
            bodyGrid.Children.Add(previewBorder);

            Grid.SetRow(bodyGrid, 2); root.Children.Add(bodyGrid);
        }

        private TabItem MakeTab(string title, out DataGrid dg)
        {
            var ti = new TabItem { Header = title, FontWeight = FontWeights.SemiBold };
            dg = new DataGrid
            {
                AutoGenerateColumns = false,
                CanUserAddRows = false, CanUserDeleteRows = false,
                IsReadOnly = true,
                HeadersVisibility = DataGridHeadersVisibility.Column,
                GridLinesVisibility = DataGridGridLinesVisibility.All,
                SelectionMode = DataGridSelectionMode.Single,
                Background = BG_PANEL, AlternatingRowBackground = BG_GRID_ALT,
                RowHeight = 22, FontSize = 11
            };
            dg.SelectionChanged += (s, e) => UpdatePreview();
            // Double-click to draw
            dg.MouseDoubleClick += (s, e) => DoDraw();

            // Columns - dynamic theo loai (se add khi populate)
            ti.Content = dg;
            return ti;
        }

        private Standard CurrentStandard()
        {
            switch (_cbManufacturer?.SelectedIndex ?? 0)
            {
                case 1: return Standard.JIS;
                case 2: return Standard.ASTM;
                default: return Standard.TCVN;
            }
        }

        private void ReloadDataForStandard()
        {
            if (_dgEA == null) return;
            var std = CurrentStandard();
            bool autoCalc = _chkAutoCalc?.IsChecked == true;

            _dgEA.ItemsSource = Prep(ShapeSteelData.Get(ShapeType.EqualAngle,   std), ShapeType.EqualAngle,   autoCalc);
            _dgUA.ItemsSource = Prep(ShapeSteelData.Get(ShapeType.UnequalAngle, std), ShapeType.UnequalAngle, autoCalc);
            _dgIB.ItemsSource = Prep(ShapeSteelData.Get(ShapeType.IBeam,        std), ShapeType.IBeam,        autoCalc);
            _dgCh.ItemsSource = Prep(ShapeSteelData.Get(ShapeType.Channel,      std), ShapeType.Channel,      autoCalc);
            _dgHB.ItemsSource = Prep(ShapeSteelData.Get(ShapeType.HBeam,        std), ShapeType.HBeam,        autoCalc);
            _dgSP.ItemsSource = Prep(ShapeSteelData.Get(ShapeType.SheetPile,    std), ShapeType.SheetPile,    autoCalc);

            var dg = CurrentDataGrid();
            if (dg != null && dg.Items.Count > 0) dg.SelectedIndex = 0;
            UpdatePreview();
        }

        /// <summary>Neu autoCalc=true thi tra ve list moi voi properties tinh tu hinh hoc</summary>
        private static List<SteelProfile> Prep(List<SteelProfile> source, ShapeType type, bool autoCalc)
        {
            if (!autoCalc) return source;
            var result = new List<SteelProfile>(source.Count);
            foreach (var src in source)
            {
                // Clone to avoid mutating original static data
                var p = new SteelProfile
                {
                    Designation = src.Designation,
                    H = src.H, B = src.B, t = src.t, tf = src.tf,
                    r1 = src.r1, r2 = src.r2,
                    IsCommon = src.IsCommon,
                    Imax = src.Imax, Imin = src.Imin,
                    imax = src.imax, imin = src.imin
                };
                SectionCalc.RecomputeProfile(p, type, useBulge: true);
                result.Add(p);
            }
            return result;
        }

        /// <summary>Tinh dac trung mat cat dang chon va hien dialog so sanh</summary>
        private void CompareSelected()
        {
            var (profile, type) = GetSelected();
            if (profile == null)
            {
                MessageBox.Show("Chua chon mat cat nao.", "VCI Thep Hinh");
                return;
            }
            try
            {
                var calc = SectionCalc.CalculateForProfile(profile, type, useBulge: true);
                string report = SectionCalc.FormatComparison(profile, calc);
                MessageBox.Show(report, $"So sanh dac trung mat cat: {profile.Designation}",
                    MessageBoxButton.OK, MessageBoxImage.Information);
            }
            catch (Exception ex)
            {
                MessageBox.Show("Loi tinh: " + ex.Message, "VCI Thep Hinh",
                    MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        private DataGrid CurrentDataGrid()
        {
            switch (_tabs?.SelectedIndex ?? 0)
            {
                case 1: return _dgUA;
                case 2: return _dgIB;
                case 3: return _dgCh;
                case 4: return _dgHB;
                case 5: return _dgSP;
                default: return _dgEA;
            }
        }

        private void PopulateData()
        {
            // Add columns 1 lan - so cot ban kinh bo theo dung tieu chuan TCVN:
            //   - EqualAngle:   1 cot r1 (TCVN 7571-1, r2 = r1/2 mac dinh)
            //   - UnequalAngle: 2 cot r1, r2
            //   - IBeam:        2 cot r1, r2 (TCVN 7571-15)
            //   - Channel:      2 cot r1, r2 (TCVN 7571-11)
            //   - HBeam:        1 cot r duy nhat (TCVN 7571-16)
            //   - SheetPile:    1 cot r
            AddAngleCols(_dgEA, doubleR: false);          // Equal: 1 r
            AddAngleCols(_dgUA, doubleR: true);           // Unequal: r1, r2
            AddBeamCols(_dgIB,  doubleR: true);           // I:  r1, r2
            AddBeamCols(_dgCh,  doubleR: true);           // U:  r1, r2
            AddBeamCols(_dgHB,  doubleR: false);          // H:  chi 1 r theo TCVN 7571-16
            AddBeamCols(_dgSP,  doubleR: false);          // SP: 1 r

            ReloadDataForStandard();
        }

        private void AddAngleCols(DataGrid dg, bool doubleR)
        {
            dg.Columns.Add(NewCol("Ten", nameof(SteelProfile.Designation), 100));
            dg.Columns.Add(NewCol("H/B (mm)", nameof(SteelProfile.H), 60, "F0"));
            dg.Columns.Add(NewCol("t", nameof(SteelProfile.t), 40, "F1"));
            if (doubleR)
            {
                dg.Columns.Add(NewColRadius("r1", nameof(SteelProfile.r1), 40,
                    "Ban kinh bo trong (root) - mm"));
                dg.Columns.Add(NewColRadius("r2", nameof(SteelProfile.r2), 40,
                    "Ban kinh bo dau canh (toe) - mm"));
            }
            else
            {
                dg.Columns.Add(NewColRadius("r", nameof(SteelProfile.r1), 45,
                    "Ban kinh bo goc theo TCVN (mm). r2 = r1/2 (mac dinh)"));
            }
            dg.Columns.Add(NewCol("kg/m", nameof(SteelProfile.Weight), 55, "F2"));
            dg.Columns.Add(NewCol("Area cm2", nameof(SteelProfile.Area), 70, "F3"));
            dg.Columns.Add(NewCol("Cx cm", nameof(SteelProfile.Cx), 55, "F2"));
            dg.Columns.Add(NewCol("Ix cm4", nameof(SteelProfile.Ix), 65, "F1"));
            dg.Columns.Add(NewCol("Imax", nameof(SteelProfile.Imax), 65, "F1"));
            dg.Columns.Add(NewCol("Imin", nameof(SteelProfile.Imin), 65, "F2"));
            dg.Columns.Add(NewCol("ix cm", nameof(SteelProfile.ix), 50, "F2"));
            dg.Columns.Add(NewCol("Zx cm3", nameof(SteelProfile.Zx), 65, "F2"));
            dg.Columns.Add(NewCol("Usual", nameof(SteelProfile.IsCommon), 50));
        }

        private void AddBeamCols(DataGrid dg, bool doubleR)
        {
            dg.Columns.Add(NewCol("Ten", nameof(SteelProfile.Designation), 100));
            dg.Columns.Add(NewCol("H", nameof(SteelProfile.H), 50, "F0"));
            dg.Columns.Add(NewCol("B", nameof(SteelProfile.B), 50, "F0"));
            dg.Columns.Add(NewCol("tw", nameof(SteelProfile.t), 45, "F1"));
            dg.Columns.Add(NewCol("tf", nameof(SteelProfile.tf), 45, "F1"));
            if (doubleR)
            {
                // TCVN 7571-15 (I-beam) / TCVN 7571-11 (Channel) - 2 ban kinh r1 (root) va r2 (toe)
                dg.Columns.Add(NewColRadius("r1", nameof(SteelProfile.r1), 40,
                    "Ban kinh bo trong root (mm) - tai 4 goc giua web va flange"));
                dg.Columns.Add(NewColRadius("r2", nameof(SteelProfile.r2), 40,
                    "Ban kinh bo dau canh toe (mm) - tai dau flange"));
            }
            else
            {
                // TCVN 7571-16 (H-beam) - chi co 1 r duy nhat (root tai 4 web-flange junctions)
                dg.Columns.Add(NewColRadius("r", nameof(SteelProfile.r1), 45,
                    "Ban kinh bo goc trong theo TCVN 7571-16 (mm) - tai 4 goc web-flange"));
            }
            dg.Columns.Add(NewCol("kg/m", nameof(SteelProfile.Weight), 60, "F2"));
            dg.Columns.Add(NewCol("Area cm2", nameof(SteelProfile.Area), 75, "F2"));
            dg.Columns.Add(NewCol("Ix cm4", nameof(SteelProfile.Ix), 80, "F0"));
            dg.Columns.Add(NewCol("Iy cm4", nameof(SteelProfile.Iy), 75, "F0"));
            dg.Columns.Add(NewCol("ix cm", nameof(SteelProfile.ix), 60, "F2"));
            dg.Columns.Add(NewCol("iy cm", nameof(SteelProfile.iy), 60, "F2"));
            dg.Columns.Add(NewCol("Zx cm3", nameof(SteelProfile.Zx), 75, "F1"));
            dg.Columns.Add(NewCol("Zy cm3", nameof(SteelProfile.Zy), 75, "F1"));
            dg.Columns.Add(NewCol("Usual", nameof(SteelProfile.IsCommon), 50));
        }

        /// <summary>Column ban kinh - highlight nen vang nhe + tooltip</summary>
        private DataGridTextColumn NewColRadius(string header, string path, double width, string tooltip)
        {
            var col = new DataGridTextColumn
            {
                Header = header,
                Width = width,
                Binding = new System.Windows.Data.Binding(path) { StringFormat = "F1" }
            };
            // Cell highlight: nen vang nhe de noi bat cot ban kinh
            var cellStyle = new Style(typeof(DataGridCell));
            cellStyle.Setters.Add(new Setter(DataGridCell.BackgroundProperty,
                new SolidColorBrush(Color.FromRgb(0xFF, 0xF8, 0xDC))));   // cornsilk
            cellStyle.Setters.Add(new Setter(DataGridCell.ForegroundProperty, Brushes.Black));
            cellStyle.Setters.Add(new Setter(DataGridCell.FontWeightProperty, FontWeights.Bold));
            cellStyle.Setters.Add(new Setter(DataGridCell.ToolTipProperty, tooltip));
            col.CellStyle = cellStyle;

            // Header tooltip
            var headerStyle = new Style(typeof(System.Windows.Controls.Primitives.DataGridColumnHeader));
            headerStyle.Setters.Add(new Setter(
                System.Windows.Controls.Primitives.DataGridColumnHeader.ToolTipProperty, tooltip));
            col.HeaderStyle = headerStyle;

            return col;
        }

        private DataGridTextColumn NewCol(string header, string path, double width, string fmt = null)
        {
            var col = new DataGridTextColumn
            {
                Header = header,
                Width = width,
                Binding = new System.Windows.Data.Binding(path)
                {
                    StringFormat = fmt
                }
            };
            return col;
        }

        // ====================================================================
        // PREVIEW canvas - ve mat cat hien tai
        // ====================================================================
        private void UpdatePreview()
        {
            if (_previewCanvas == null) return;
            _previewCanvas.Children.Clear();

            var (profile, type) = GetSelected();
            if (profile == null) return;

            // Draw the outline on canvas (scale to fit 140x140)
            double maxDim = Math.Max(profile.H, profile.B);
            if (maxDim < 1) return;
            double scale = 130.0 / maxDim;

            List<System.Windows.Point> outline = GetOutlinePoints(profile, type);
            if (outline == null || outline.Count == 0) return;

            // Center within canvas
            double offsetX = (160 - profile.B * scale) / 2.0;
            double offsetY = (160 - profile.H * scale) / 2.0;

            var poly = new Polygon
            {
                Stroke = FG_BLUE, StrokeThickness = 1.5,
                Fill = B(0x55, 0xB8, 0xFF) // light blue fill
            };
            poly.Fill.Opacity = 0.3;
            foreach (var p in outline)
            {
                // Flip Y (WPF Y goes down, geometry Y goes up)
                poly.Points.Add(new System.Windows.Point(
                    offsetX + p.X * scale,
                    160 - offsetY - p.Y * scale));
            }
            _previewCanvas.Children.Add(poly);

            // Label dimensions
            var lblH = new TextBlock
            {
                Text = $"H={profile.H:F0}", FontSize = 9,
                Foreground = FG_MUTED
            };
            Canvas.SetLeft(lblH, 2);
            Canvas.SetTop(lblH, 2);
            _previewCanvas.Children.Add(lblH);

            var lblB = new TextBlock
            {
                Text = $"B={profile.B:F0}", FontSize = 9,
                Foreground = FG_MUTED
            };
            Canvas.SetLeft(lblB, 2);
            Canvas.SetTop(lblB, 14);
            _previewCanvas.Children.Add(lblB);

            var lblT = new TextBlock
            {
                Text = $"t={profile.t:F1}", FontSize = 9,
                Foreground = FG_MUTED
            };
            Canvas.SetLeft(lblT, 2);
            Canvas.SetTop(lblT, 26);
            _previewCanvas.Children.Add(lblT);
        }

        private List<System.Windows.Point> GetOutlinePoints(SteelProfile p, ShapeType type)
        {
            // Mimics ShapeSteelDrawer.OutlineXxx
            var result = new List<System.Windows.Point>();
            switch (type)
            {
                case ShapeType.EqualAngle:
                case ShapeType.UnequalAngle:
                    result.Add(new System.Windows.Point(0, 0));
                    result.Add(new System.Windows.Point(p.B, 0));
                    result.Add(new System.Windows.Point(p.B, p.t));
                    result.Add(new System.Windows.Point(p.t, p.t));
                    result.Add(new System.Windows.Point(p.t, p.H));
                    result.Add(new System.Windows.Point(0, p.H));
                    break;
                case ShapeType.IBeam:
                case ShapeType.HBeam:
                    double hb = p.B / 2.0, ht = p.t / 2.0;
                    result.Add(new System.Windows.Point(0, 0));
                    result.Add(new System.Windows.Point(p.B, 0));
                    result.Add(new System.Windows.Point(p.B, p.tf));
                    result.Add(new System.Windows.Point(hb + ht, p.tf));
                    result.Add(new System.Windows.Point(hb + ht, p.H - p.tf));
                    result.Add(new System.Windows.Point(p.B, p.H - p.tf));
                    result.Add(new System.Windows.Point(p.B, p.H));
                    result.Add(new System.Windows.Point(0, p.H));
                    result.Add(new System.Windows.Point(0, p.H - p.tf));
                    result.Add(new System.Windows.Point(hb - ht, p.H - p.tf));
                    result.Add(new System.Windows.Point(hb - ht, p.tf));
                    result.Add(new System.Windows.Point(0, p.tf));
                    break;
                case ShapeType.Channel:
                    result.Add(new System.Windows.Point(0, 0));
                    result.Add(new System.Windows.Point(p.B, 0));
                    result.Add(new System.Windows.Point(p.B, p.tf));
                    result.Add(new System.Windows.Point(p.t, p.tf));
                    result.Add(new System.Windows.Point(p.t, p.H - p.tf));
                    result.Add(new System.Windows.Point(p.B, p.H - p.tf));
                    result.Add(new System.Windows.Point(p.B, p.H));
                    result.Add(new System.Windows.Point(0, p.H));
                    break;
                case ShapeType.SheetPile:
                    double slope = p.H * 0.4;
                    result.Add(new System.Windows.Point(0, p.H));
                    result.Add(new System.Windows.Point(slope, 0));
                    result.Add(new System.Windows.Point(p.B - slope, 0));
                    result.Add(new System.Windows.Point(p.B, p.H));
                    result.Add(new System.Windows.Point(p.B - p.t, p.H));
                    result.Add(new System.Windows.Point(p.B - slope - p.t * 0.5, p.t));
                    result.Add(new System.Windows.Point(slope + p.t * 0.5, p.t));
                    result.Add(new System.Windows.Point(p.t, p.H));
                    break;
            }
            return result;
        }

        // ====================================================================
        // DRAW button - cho user pick diem trong AutoCAD + ve
        // ====================================================================
        private void DoDraw()
        {
            var (profile, type) = GetSelected();
            if (profile == null)
            {
                MessageBox.Show("Chua chon mat cat nao trong bang.", "VCI Thep Hinh");
                return;
            }
            if (OnDraw == null) return;

            bool useBulge = _chkBulge?.IsChecked == true;
            bool asBlock = _rbBlock?.IsChecked == true;

            this.Hide();
            int count = 0;
            try { count = OnDraw(profile, type, useBulge, asBlock); }
            finally { this.Show(); this.Activate(); }
            if (count > 0)
            {
                string mode = asBlock ? "Block" : "Polyline";
                string bo = useBulge ? "bo r" : "vuong";
                Title = $"Shape Steel Library - VCI  |  Da ve {mode}/{bo}: {profile.Designation}";
            }
        }

        private (SteelProfile profile, ShapeType type) GetSelected()
        {
            int idx = _tabs.SelectedIndex;
            ShapeType type;
            DataGrid dg;
            switch (idx)
            {
                case 0: type = ShapeType.EqualAngle;   dg = _dgEA; break;
                case 1: type = ShapeType.UnequalAngle; dg = _dgUA; break;
                case 2: type = ShapeType.IBeam;        dg = _dgIB; break;
                case 3: type = ShapeType.Channel;      dg = _dgCh; break;
                case 4: type = ShapeType.HBeam;        dg = _dgHB; break;
                case 5: type = ShapeType.SheetPile;    dg = _dgSP; break;
                default: return (null, ShapeType.IBeam);
            }
            return (dg.SelectedItem as SteelProfile, type);
        }

        // ====================================================================
        // VCI Logo (50x50)
        // ====================================================================
        private UIElement MakeVciLogo(double size)
        {
            var sp = new StackPanel { Orientation = Orientation.Horizontal, VerticalAlignment = VerticalAlignment.Center };
            var iconGrid = new Grid { Width = size, Height = size };
            iconGrid.Children.Add(new Ellipse
            {
                Width = size - 4, Height = size - 4,
                Stroke = FG_TEAL, StrokeThickness = 2.5,
                Fill = B(0xE6, 0xFA, 0xFC),
                HorizontalAlignment = HorizontalAlignment.Center,
                VerticalAlignment = VerticalAlignment.Center
            });
            iconGrid.Children.Add(new TextBlock
            {
                Text = "VCI",
                FontSize = size * 0.32, FontWeight = FontWeights.Black, Foreground = FG_TEAL,
                FontFamily = new FontFamily("Arial Black, Arial"),
                HorizontalAlignment = HorizontalAlignment.Center,
                VerticalAlignment = VerticalAlignment.Center
            });
            sp.Children.Add(iconGrid);
            return sp;
        }

        // ====================================================================
        // STYLES (light theme)
        // ====================================================================
        private Style StyleTextBlock()
        {
            var s = new Style(typeof(TextBlock));
            s.Setters.Add(new Setter(TextBlock.ForegroundProperty, FG_BLACK));
            s.Setters.Add(new Setter(TextOptions.TextFormattingModeProperty, TextFormattingMode.Display));
            return s;
        }
        private Style StyleButton()
        {
            var s = new Style(typeof(Button));
            s.Setters.Add(new Setter(Button.BackgroundProperty, BG_BUTTON));
            s.Setters.Add(new Setter(Button.ForegroundProperty, FG_BLACK));
            s.Setters.Add(new Setter(Button.BorderBrushProperty, BORDER));
            s.Setters.Add(new Setter(Button.BorderThicknessProperty, new Thickness(1)));
            s.Setters.Add(new Setter(Button.PaddingProperty, new Thickness(10, 4, 10, 4)));
            s.Setters.Add(new Setter(Button.CursorProperty, Cursors.Hand));
            return s;
        }
        private Style StyleDG()
        {
            var s = new Style(typeof(DataGrid));
            s.Setters.Add(new Setter(DataGrid.BackgroundProperty, BG_PANEL));
            s.Setters.Add(new Setter(DataGrid.ForegroundProperty, FG_BLACK));
            s.Setters.Add(new Setter(DataGrid.RowBackgroundProperty, BG_PANEL));
            s.Setters.Add(new Setter(DataGrid.AlternatingRowBackgroundProperty, BG_GRID_ALT));
            s.Setters.Add(new Setter(DataGrid.HorizontalGridLinesBrushProperty, B(0xDD, 0xDD, 0xDD)));
            s.Setters.Add(new Setter(DataGrid.VerticalGridLinesBrushProperty, B(0xDD, 0xDD, 0xDD)));
            s.Setters.Add(new Setter(DataGrid.BorderBrushProperty, BORDER));
            return s;
        }
        private Style StyleDGHeader()
        {
            var s = new Style(typeof(DataGridColumnHeader));
            s.Setters.Add(new Setter(DataGridColumnHeader.BackgroundProperty, BG_GRID_HEADER));
            s.Setters.Add(new Setter(DataGridColumnHeader.ForegroundProperty, FG_BLACK));
            s.Setters.Add(new Setter(DataGridColumnHeader.BorderBrushProperty, BORDER));
            s.Setters.Add(new Setter(DataGridColumnHeader.BorderThicknessProperty, new Thickness(0, 0, 1, 1)));
            s.Setters.Add(new Setter(DataGridColumnHeader.PaddingProperty, new Thickness(6, 4, 6, 4)));
            s.Setters.Add(new Setter(DataGridColumnHeader.FontWeightProperty, FontWeights.SemiBold));
            return s;
        }
        private Style StyleDGCell()
        {
            var s = new Style(typeof(DataGridCell));
            s.Setters.Add(new Setter(DataGridCell.BackgroundProperty, Brushes.Transparent));
            s.Setters.Add(new Setter(DataGridCell.ForegroundProperty, FG_BLACK));
            s.Setters.Add(new Setter(DataGridCell.BorderThicknessProperty, new Thickness(0)));
            s.Setters.Add(new Setter(DataGridCell.PaddingProperty, new Thickness(4, 2, 4, 2)));
            var tSel = new Trigger { Property = DataGridCell.IsSelectedProperty, Value = true };
            tSel.Setters.Add(new Setter(DataGridCell.BackgroundProperty, B(0xCC, 0xE6, 0xFF)));
            tSel.Setters.Add(new Setter(DataGridCell.ForegroundProperty, FG_BLACK));
            s.Triggers.Add(tSel);
            return s;
        }
    }
}
