using System;

namespace VCI.WaterPipe
{
    public sealed class PipeSettings
    {
        public double OuterDiameter { get; set; } = 90.0;
        public double WallThickness { get; set; } = 4.5;
        public double ElbowThickness { get; set; } = 4.5;
        public double BendRadius { get; set; } = 135.0;
        public string LayerName { get; set; } = "VCI-ONG-NUOC";
        public short ColorIndex { get; set; } = 4;

        public double InnerDiameter => Math.Max(0.1, OuterDiameter - 2.0 * WallThickness);
        public double CenterlineRadius => Math.Max(OuterDiameter * 1.5, BendRadius);

        public void Validate()
        {
            if (OuterDiameter <= 0) throw new ArgumentException("Đường kính ngoài phải lớn hơn 0.");
            if (WallThickness <= 0 || WallThickness * 2 >= OuterDiameter)
                throw new ArgumentException("Chiều dày ống phải lớn hơn 0 và nhỏ hơn một nửa đường kính ngoài.");
            if (ElbowThickness <= 0) throw new ArgumentException("Chiều dày cút phải lớn hơn 0.");
            if (BendRadius < OuterDiameter) throw new ArgumentException("Bán kính tim cút nên lớn hơn hoặc bằng đường kính ngoài.");
        }
    }
}
