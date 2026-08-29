#include "PipeGeometry.h"
#include <algorithm>
#include <cmath>

namespace VCI::WaterPipe
{
namespace { constexpr double Eps = 1e-8; }

bool Settings::valid() const
{
    return outerDiameter > 0 && wallThickness > 0 && wallThickness * 2 < outerDiameter &&
           elbowWallThickness > 0 && bendRadius >= outerDiameter;
}

struct Bend { AcGePoint3d center, inPoint, outPoint; double radius, turn; };

static AcGeVector3d Unit(const AcGePoint3d& a, const AcGePoint3d& b) { return (b - a).normal(); }

PathGeometry BuildWall(const std::vector<AcGePoint3d>& p, const Settings& s, double straightHalfWidth, double bendHalfWidth)
{
    PathGeometry out;
    if (p.size() < 2 || !s.valid()) return out;
    std::vector<Bend> bends;
    const double radius = std::max(s.bendRadius, s.outerDiameter * 1.5);

    for (size_t i = 1; i + 1 < p.size(); ++i) {
        AcGeVector3d u = Unit(p[i - 1], p[i]), v = Unit(p[i], p[i + 1]);
        double turn = u.x * v.y - u.y * v.x;
        double theta = std::acos(std::clamp(-u.dotProduct(v), -1.0, 1.0));
        if (theta < Eps || std::abs(turn) < Eps) continue;
        double tangent = radius * std::tan(theta / 2.0);
        double centerDistance = radius / std::sin(theta / 2.0);
        AcGeVector3d bisector = ((-u) + v).normal();
        bends.push_back({ p[i] + bisector * centerDistance,
                          p[i] - u * tangent, p[i] + v * tangent,
                          radius, turn > 0 ? 1.0 : -1.0 });
    }

    auto bendPoint = [&](const Bend& b, bool outgoing, int side) {
        AcGePoint3d tangent = outgoing ? b.outPoint : b.inPoint;
        double r = b.radius - b.turn * side * bendHalfWidth;
        return b.center + (tangent - b.center).normal() * r;
    };
    auto offset = [&](const AcGePoint3d& point, const AcGeVector3d& direction, double d) {
        return point + AcGeVector3d(-direction.y, direction.x, 0.0) * d;
    };

    for (int side : {-1, 1}) {
        for (size_t i = 0; i + 1 < p.size(); ++i) {
            AcGePoint3d a = i == 0 ? offset(p.front(), Unit(p[0], p[1]), side * straightHalfWidth) : bendPoint(bends[i - 1], false, side);
            AcGePoint3d b = i + 1 == p.size() - 1 ? offset(p.back(), Unit(p[p.size() - 2], p.back()), side * straightHalfWidth) : bendPoint(bends[i], true, side);
            out.lines.push_back({a, b});
        }
        for (const Bend& b : bends) {
            double r = b.radius - b.turn * side * halfWidth;
            if (r <= Eps) continue;
            AcGeVector3d va = b.inPoint - b.center, vb = b.outPoint - b.center;
            double start = std::atan2(va.y, va.x), end = std::atan2(vb.y, vb.x);
            if (b.turn < 0) std::swap(start, end);
            out.arcs.push_back({b.center, r, start, end});
        }
    }
    return out;
}
}
