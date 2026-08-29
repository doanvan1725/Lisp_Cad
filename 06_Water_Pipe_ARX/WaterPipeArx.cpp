#include "PipeGeometry.h"
#include "aced.h"
#include "adscodes.h"
#include "acutads.h"
#include "acdocman.h"
#include "dbapserv.h"
#include "dbents.h"
#include "dbsymtb.h"
#include "dbpl.h"
#include "rxregsvc.h"
#include <vector>

using namespace VCI::WaterPipe;

static AcDbObjectId EnsureLayer(AcDbDatabase* db, const ACHAR* name)
{
    AcDbLayerTable* table = nullptr;
    if (acdbOpenObject(table, db->layerTableId(), AcDb::kForRead) != Acad::eOk) return AcDbObjectId::kNull;
    AcDbObjectId id;
    if (table->getAt(name, id) != Acad::eOk) {
        table->upgradeOpen();
        auto* layer = new AcDbLayerTableRecord();
        layer->setName(name);
        table->add(id, layer);
        layer->close();
    }
    table->close();
    return id;
}

static void AddGeometry(AcDbBlockTableRecord* space, const PathGeometry& geometry, const AcDbObjectId& layer)
{
    for (const auto& line : geometry.lines) {
        auto* entity = new AcDbLine(line.start, line.end);
        entity->setLayer(layer);
        space->appendAcDbEntity(entity); entity->close();
    }
    for (const auto& arc : geometry.arcs) {
        auto* entity = new AcDbArc(arc.center, arc.radius, arc.startAngle, arc.endAngle);
        entity->setLayer(layer);
        space->appendAcDbEntity(entity); entity->close();
    }
}

static void DrawWaterPipe()
{
    Settings settings;
    if (acedGetReal(L"\nĐường kính ngoài ống <90>: ", &settings.outerDiameter) == RTNONE) settings.outerDiameter = 90;
    if (acedGetReal(L"\nChiều dày thành ống <4.5>: ", &settings.wallThickness) == RTNONE) settings.wallThickness = 4.5;
    if (acedGetReal(L"\nChiều dày cút nối <4.5>: ", &settings.elbowWallThickness) == RTNONE) settings.elbowWallThickness = 4.5;
    if (acedGetReal(L"\nBán kính tim cút <135>: ", &settings.bendRadius) == RTNONE) settings.bendRadius = 135;
    if (!settings.valid()) { acutPrintf(L"\n[VCI] Thông số ống không hợp lệ."); return; }

    std::vector<AcGePoint3d> route;
    ads_point raw;
    if (acedGetPoint(nullptr, L"\nChọn điểm đầu tuyến ống: ", raw) != RTNORM) return;
    route.emplace_back(raw[0], raw[1], raw[2]);
    while (true) {
        acedInitGet(0, nullptr);
        ads_point base = { route.back().x, route.back().y, route.back().z };
        if (acedGetPoint(base, L"\nChọn điểm tiếp theo (Enter kết thúc): ", raw) != RTNORM) break;
        AcGePoint3d next(raw[0], raw[1], raw[2]);
        if (next.distanceTo(route.back()) > 1e-6) route.push_back(next);
    }
    if (route.size() < 2) return;

    AcDbDatabase* db = acdbHostApplicationServices()->workingDatabase();
    AcDbBlockTableRecord* space = nullptr;
    if (acdbOpenObject(space, db->currentSpaceId(), AcDb::kForWrite) != Acad::eOk) return;
    AcDbObjectId layer = EnsureLayer(db, L"VCI-ONG-NUOC");
    const double half = settings.outerDiameter / 2.0;
    AddGeometry(space, BuildWall(route, settings, half, half), layer);
    AddGeometry(space, BuildWall(route, settings, half - settings.wallThickness, half - settings.elbowWallThickness), layer);
    space->close();
    acutPrintf(L"\n[VCI] Đã tạo tuyến ống 2D, OD=%.3f, ID=%.3f.", settings.outerDiameter, settings.innerDiameter());
}

extern "C" AcRx::AppRetCode acrxEntryPoint(AcRx::AppMsgCode msg, void* pkt)
{
    switch (msg) {
    case AcRx::kInitAppMsg:
        acrxDynamicLinker->unlockApplication(pkt);
        acrxDynamicLinker->registerAppMDIAware(pkt);
        acedRegCmds->addCommand(L"VCI_WATER_PIPE", L"ONGNUOC", L"ONGNUOC", ACRX_CMD_MODAL, DrawWaterPipe);
        break;
    case AcRx::kUnloadAppMsg:
        acedRegCmds->removeGroup(L"VCI_WATER_PIPE");
        break;
    }
    return AcRx::kRetOK;
}
