#pragma once

#include "gept3dar.h"
#include "gevec3d.h"
#include <vector>

namespace VCI::WaterPipe
{
struct Settings
{
    double outerDiameter = 90.0;
    double wallThickness = 4.5;
    double elbowWallThickness = 4.5;
    double bendRadius = 135.0;
    double innerDiameter() const { return outerDiameter - 2.0 * wallThickness; }
    bool valid() const;
};

struct LinePiece { AcGePoint3d start, end; };
struct ArcPiece { AcGePoint3d center; double radius, startAngle, endAngle; };
struct PathGeometry
{
    std::vector<LinePiece> lines;
    std::vector<ArcPiece> arcs;
};

// Tạo hai biên song song của ống. Không phụ thuộc AutoCAD database/entity.
PathGeometry BuildWall(const std::vector<AcGePoint3d>& route, const Settings& settings, double straightHalfWidth, double bendHalfWidth);
}
