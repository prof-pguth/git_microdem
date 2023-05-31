unit demmapdraw;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

//{$Define InlineCoreMapping}    //turn off to debug inline functions

{$IFDEF DEBUG}
   {$Define NoParallelFor} //used to debug only
   //{$Define NoParallelLAS}
   //{$Define NoParallelOpenness}
{$ELSE}
   {$Define NoParallelLAS}    //need to check why this is set in Release mode
{$ENDIF}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems

   //{$Define RecordKeyMap}         //don't use if there will be a lot of map drawing

   {$IfDef Debug}
      //{$Define RecordFan}
      //{$Define RecordVAT}
      //{$Define FanDrawProblems)
      //{$Define WorldFileOverlay}
      //{$Define RecordStretchBitmap}
      //{$Define RecordLegendDraw}
      //{$Define RecordDrawGridLines}
      //{$Define RecordSat}
      //{$Define AspectCheck}
      //{$Define RecordShapeFileGroup}
      //{$Define RecordWorldOutline}
      //{$Define Track_f}
      //{$Define RecordMapResize}
      //{$Define RecordTissot}
      //{$Define RecordTargetAreasCoverage}
      //{$Define RecordMapDrawCreation}
      //{$Define RecordPlatCarree}
      //{$Define RecordKeyMap}
      //{$Define ShowProjectedLimits}
      //{$Define RecordElevRange}
      //{$Define RecordMapDraw}
      //{$Define RecordRefMapColors}

      //{$Define RecordMapBlow}        //needs to be off for threaded fan drawing
      //{$Define RecordElevationScaling}
      //{$Define RecordTIGER}
      //{$Define TimePLSS}
      //{$Define RecordZoomIn}
      //{$Define RecordMaximizeMapCoverage}
      //{$Define RecordFullDrawGridLines}
      //{$Define RecordLegend}
      //{$Define RecordMapLayers}
      //{$Define ShowUTMZone}
      //{$Define RecordFullShapeFileGroup}
      //{$Define TigerTiming}
      //{$Define RecordTiming}
      //{$Define RecordLong0}
      //{$Define SameProjection}
      //{$Define RecordLAS}
      //{$Define RecordPixelSize}
      //{$Define RecordShortDefineDatum}
      //{$Define RecordDBPlots}
      //{$Define RecordOverlays}
      //{$Define RecordPLSS}
      //{$Define RecordOffMap}
      //{$Define RecordWMS}
      //{$Define RecordGeology}
      //{$Define RecordAnaglyph}
      //{$Define TimeFigureMultiSensorCoverage}
      //{$Define RecordSatTraining}
      //{$Define RecordConversion}
      //{$Define RecordDatumShift}
      //{$Define RecordCarefulPolyLineDraw}
      //{$Define RecordGetVisiblePoints} //slowdown
      //{$Define RecordUTMZone}
      //{$Define RecordAnaglyph}
      //{$Define RecordIHSmerges}
      //{$Define RecordMapMerge}
      //{$Define SaveGridSequentially}
      //{$Define RecordMapIndex}
      //{$Define RecordFullMapDraw}
      //{$Define RecordGazLabel}
      //{$Define RecordHiResDEM}
      //{$Define RecordMapMargin}
      //{$Define RecordTerrCatOverlays}
      //{$Define RecordMapLimits}
      //{$Define RecordElevColorFromTable}
      //{$Define RecordNLCD}
      //{$Define RecordPrinter}
      //{$Define RecordFullTissot}
      //{$Define RecordGridToScreen} //major slowdown
      //{$Define RecordGeotiff}
      //{$Define RecordContour}
      //{$Define RecordContourLines}
      //{$Define RecordOblique}
      //{$Define RecordTIGERFull}
      //{$Define RecordClosing}
      //{$Define RecordMagDecDiagram}
      //{$Define RecordThreadContour}
      //{$Define RecordRoute}
      //{$Define RecordFanProblemsWithBMP}
      //{$Define GeotiffSave}
      //{$Define RecordAmbush}
      //{$Define RecordFly}
      //{$Define RecordTerrainCategories}
      //{$Define RecordDefineDatum}
   {$Else}
      //{$Define RecordTiming}
   {$EndIf}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseBDETables}
      dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end needed for inline core DB functions

   System.UITypes,System.UIConsts,System.Types,System.SyncObjs, System.IOUtils,
   SysUtils,Classes,  StrUtils,  Math,

   {$IfDef VCL}
      Windows, Winspool,ComCtrls,Forms,Printers,Graphics,Buttons,Controls,
      Dialogs,Grids, ExtCtrls, StdCtrls, Menus,  ToolWin, DBCtrls,
   {$EndIf}

   {$IfDef FMX}
      FMX.Graphics,FMX.Controls, FMX.Types,
   {$EndIf}

   {$IfDef Android}
      Androidapi.NativeActivity,
      Posix.Pthread,
   {$EndIf}

   {$IfDef RecordTime}
      System.Diagnostics,System.TimeSpan,
   {$EndIf}

   {$If Defined(NoParallelFor) and Defined(NoParallelLAS)}
   {$Else}
      System.Threading,
   {$EndIf}

   PETMAR,PETMath,PetImage,BaseMap,Petmar_types,
   DEMDefs,DEMDef_routines;


const
   OutsideBox = false;
type
   tMapOverlays = record
     ovShapeFileGroup : PathStr;
     ovVectorFiles,
     ovTerrainCat    : tStringList;
     ConInt          : int16;
   end;

type
   tMapDraw = class
   private
      CountInStrips : integer;
      {$IfDef VCL}
         procedure DrawElevationMapForMerge(var Bitmap : tMyBitmap);
         procedure ShowVegOnMap(var inBitmap : tMyBitmap; VegLayer : integer = 0);
         procedure PlotMapLibrary(var Bitmap : tMyBitmap);
         procedure DrawVectors(var inBitmap: tMyBitmap);
         procedure SecondGridStrip(Limits : tGridLimits; BMPMemory :  tBMPMemory);
         procedure NewDEMSatelliteMerge(Bitmap : tMyBitmap);
         procedure LabelDataBases(var Bitmap : tMyBitmap);
         procedure SmartUSOutline(var inBitmap : tMyBitmap);
      {$EndIf}

      {$IfDef ExWMS}
      {$Else}
         function CreateWMSRequest : ANSIstring;
         procedure DrawMMSOverlay(var Bitmap : tMyBitmap);
         procedure DownloadWMSforThisMap(var fName : PathStr);
      {$EndIf}

      function LoadExistingBaseMap(var SavedLayer : tMyBitmap; var fName : PathStr) : boolean;
      procedure DrawSlopeMap(var Bitmap : tMyBitmap);
      procedure DrawOpennessMap(var Bitmap : tMyBitmap);
      procedure DrawSlopeMapStrip(Limits : tGridLimits; var BMPMemory :  tBMPMemory);
      procedure DrawElevationMapStrip(Limits : tGridLimits; var BMPMemory :  tBMPMemory);
      procedure DrawOpennessMapStrip(Limits : tGridLimits; var BMPMemory :  tBMPMemory);
      function ElevColorFromZ(zj : float64) : tPlatformColor;
      procedure DrawLASclassMap(var Bitmap: tMyBitmap);
      procedure DrawRGBMap(var Bitmap : tMyBitmap);
   public
     MapOwner    : tMapOwner;
     MapType     : tMapType;
     MapMerge    : tMapMerge;
     ElevStretch : tElevStretch;
     BaseTitle   : ShortString;
     BasicProjection : tBasicProjection;
     VectorIndex,
     ShowVegLayer,
     MultiGridOnMap,
     MonthlyDBArrayOnMap,
     DEMonMap,DEM2onMap,
     FeatureGrid,
     LASclassGrid,
     CHMgrid,
     ChangeGrid,
     SATonMap,
     MapGazDB,
     MergeMap   : integer;
     MinZSizeDEM2,MaxZSizeDEM2,
     MinZColorDEM2,MaxZColorDEM2 : float64;
     TIGERFilter : ShortString;
     OverlayOrder : array[1..MaxOverlays] of tOverlayOrder;
     DBonThisMap :  array[1..MaxDataBase] of boolean;
     NeedToRedraw,
     ClosingMapNow,
     InitialMapDrawn,
     FastMapDraw,
     Log10Elev,
     AllTigerOK,
     NoMapGrids,
     NoDrawingNow,
     DrawScaleBarThisMap,
     DrawLegendsThisMap,
     RedrawDrainageVectors,
     aDRGmap,
     SingleContourColor,
     MapDrawValid,
     DEMShadeImage,
     ShowMapLibraryCoverage,
     DrawFansThisTime,
     LockZColors,
     SubdueWorldOutline,
     GrayscaleWorldOutline,
     SubdueOSM,
     GrayscaleOSM,
     SubdueBase,
     SubdueGrids,
     MakeMapGrayscale,
     GrayscaleSubdueOverlays,
     InterpolateMapColors,
     DrawMarginalia,
     ShowMapMargins,
     AllowDataBaseDrawing,
     FirstMapDrawing,
     Long0_360,
     UsePercentiles,
     TigerSingleOverlayUp,
     {$IfDef ExWMS}
     {$Else}
        WMSOverlayOnMap,
     {$EndIf}
     GroundOnly,
     LASlayerOnMap : boolean;

     IndexFileName,
     GraticuleFName,
     UTMgridFName,
     GraticuleEndsFName,
     UTMgridEndsFName,
     RangeCirclesFName,
     CartoGroupShapesUp,
     OSMShapesUp,
     SectorOutlines,
     FullMapfName,
     CartoDBfName,
     SatTrainingfName,
     SecondGridfName,
     ContourOverlayfName,
     ContourOverlayfName2,
     AllFansCoverageFName,
     TigerOverlayFName,
     BaseMapFName,
     OSMOverlayfName,
     PLSSOverlayfName,
     USOverlayfName,
     GridOverlayFName,
     GazOverlayFName,
     NaturalEarthOverlayFName,
     LegendOverlayfName,
     VectorOverlayFName : PathStr;
     DBOverlayfName : array [1..MaxDataBase] of PathStr;

     PrimMapProj,SecondaryMapProj : tMapProjection;
     MapOverlays     : tMapOverlays;
     MapCorners : CurScreenMapType;
     CurrentZoomLevel : integer;
     MapXSize,MapYSize : int32;

     MinMapElev,MaxMapElev : float32;
     MapLatCent, MapLongCent,
     ScreenPixelSize,
     LongLeftSide,
     LatTickInt,UTMTickInt,NativeTickInt,
     GridTrueAngle,
     MapMagDec        : float64;
     MultiFanMasks,
     FloodLayers : tStringList;
     TerrainShadowsDEM,
     VegDensityLayerInUse       : integer;
     ZColorTable : tColorTableDefinitions;
     CurrentFansTable : Integer;
     MaskColor : tPlatformColor;
     VATColors : tVATColors;

     SlopeCut          : ColorCutArrayType;
     SlopeColors       : tColorArray;

     DEMGridSym : tDrawingSymbol;
     DEMGridSymSize : byte;
     DEMGridSymColor : tPlatformColor;

     DEMGridLabel,
     DEMGridRedraw : boolean;

     {$IfDef ExSat}
     {$Else}
        SatView  : tSatView;
     {$EndIf}

     {$IfDef ExWMS}
     {$Else}
        LastWMSService,
        WMSLayerfName : PathStr;
     {$EndIf}

     {$IfDef ExPointCloud}
     {$Else}
        LasMinAreaZ,
        LasMaxAreaZ : float64;
     {$EndIf}

      constructor Create;
      destructor Destroy;

      procedure DrawMapOnBMP(var Bitmap : tMyBitmap; DrawBaseMap : boolean = true; DrawOverlays : boolean = true);

      procedure ZeroOverlays;

      function DEMMap : boolean;
      procedure ScaleMapElevationsToDEM;
      procedure DeleteMapSavedLayers;
      procedure DeleteSingleMapLayer(var fName : PathStr);
      procedure DeleteDatabaseSavedLayers;
      procedure DeleteMapSavedLasLayers;
      procedure SaveLayerBitmap(Bitmap : tMyBitmap; var FName : PathStr);
      procedure ClearGISLayer(i : integer);
      procedure ZeroTickInt;

      function ScreenPositionToLatLongDegreeString(x,y : integer) : ShortString;
      function MapColorRange(What : shortstring) : shortstring;

      Procedure AssignSecondDEM(DEM : integer; Min : float64 = 99; Max : float64 = -99);

      procedure ScreenBoxToLatLongMinMax(xlo, ylo, xhi, yhi : integer; var CurScrLatLow,CurScrLongLow,CurScrLatHigh,CurScrLongHigh : float64);
      procedure DefineNewDEMMap(CurDEM : integer; mt : tMapType);

      procedure MapAspectCheck;
      function GetMapAspectRatio : float64;
      procedure CheckMapNotTooLarge;
      function MapZRange : float64; inline;

      //function ConicProjection : boolean;

      procedure AdjustProjectionForUTMZone(Why : shortstring; PrimaryDatum: shortstring;  UTMZone : byte; LatHemi : ANSIchar); overload;
      procedure AdjustProjectionForUTMZone(Why : shortstring; PrimaryDatum : shortstring;  Lat,Long : float64); overload;


      procedure ResetMarginalia;

      function RecenterMap(Lat,Long : float64; Percent : integer) : boolean;
      function ResizeMapByPercentage(Percent : integer{; SureAboutDraw : boolean = false}) : boolean;

      procedure MaximizeLatLongMapCoverage(uMinLat,uMinLong,uMaxLat,uMaxLong : float64; xsize: integer = -99; ysize : integer = -99); overload; {all in degrees}
      procedure MaximizeLatLongMapCoverage(bb : sfBoundBox; xsize: integer = -99; ysize : integer = -99); overload; {all in degrees}
      procedure SetFullMapCoverage;

      procedure MapOverlaysToDebugFile(Where : shortstring);

      procedure MapGridToDEMGrid(xg,yg : float32; var xg1,yg1 : float32);

      procedure DataGridToLatLongDegree(xgrid,ygrid : float32; var lat,long : float64);
      procedure DataGridToScreen(XGrid,YGrid : float32; var XPic,YPic : integer; OutsideData : boolean = false);
      procedure DataGridToProjectedCoords(X,Y : float32; var XProj,YProj : float64);
      procedure ScreenToProjectedCoords(XPic,YPic : integer; var XProj,YProj : float64); {$IfDef InlineCoreMapping} inline; {$EndIf}
      function ScreenToSatelliteDataGrid(Band, XPic,YPic : integer; var XGrid,YGrid : integer) : boolean;
      function ScreenToLatLongString(xpic,ypic : integer) : shortstring;
      procedure ProjectedCoordsToScreen(XProj,YProj : float64; var XPic,YPic : integer); {$IfDef InlineCoreMapping} inline; {$EndIf}
      procedure ProjectedCoordsToDataGrid(X,Y : float64; var XProj,YProj : float64);

      function MaxScaleDistortionOnMap : float64;

      procedure BoundingBoxProjToDataGrid;
      procedure BoundingBoxGeoToDataGrid;
      procedure BoundingBoxDataGridToGeo;
      procedure BoundingBoxDataGridToProj;
      procedure BoundingBoxGeoToProjected;
      function GetBoundBoxGeo : sfBoundBox;
      function GetBoundBoxUTM : sfBoundBox;
      procedure FindMapUTMLimits;

      function ScreenToDataGrid(XPic,YPic : integer; var XGrid,YGrid : float32) : boolean;
      procedure ScreenToLatLongDegree(XPic,YPic : integer; var Lat,Long : float64);
      procedure ScreenToUTM(xpic,ypic : integer; var xutm,yutm : float64; ForceSlow : boolean = false);
      function ScreenToElev(xpic,ypic : integer; var z : float32) : boolean;
      procedure ScreenToDEMGrid(xpic,ypic : integer; var XGrid,YGrid : float32); overload; {$IfDef InlineCoreMapping} inline; {$EndIf}
      procedure ScreenToDEMGrid(xpic,ypic : integer; var XGrid,YGrid : integer); overload; {$IfDef InlineCoreMapping} inline; {$EndIf}

      procedure LatLongDegreeToScreen(Lat,Long : float64; var XPic,YPic : integer); {$IfDef InlineCoreMapping} inline; {$EndIf}
      procedure LatLongRadiansToScreen(Lat,Long : float64; var XPic,YPic : integer); {$IfDef InlineCoreMapping} inline; {$EndIf}
      procedure LatLongDegreeToProjectedCoords(Lat,Long : float64; var XProj,YProj : float64);
      procedure LatLongDegreeToDataGrid(Lat,Long : float64; var xg,yg : float32);  overload;
      procedure LatLongDegreeToDataGrid(Lat,Long : float64; var xg,yg : float64);  overload;

      procedure UTMtoScreen(XUTM,YUTM : float64; var XPic,YPic : integer; ReallyUTM : boolean = false); overload;
      procedure UTMtoScreen(XUTM,YUTM : float32; var XPic,YPic : integer; ReallyUTM : boolean = false); overload;
      procedure MGRSToScreen(MGRS : shortString; var XPic,YPic : integer);
      procedure DEMGridToScreen(xg,yg : float32; var xs,ys : integer);

      procedure ClipDataGrid(var bb : sfBoundBox);

      procedure LatLongDegreeToUTM(Lat,Long : float64; var xutm,yutm : float64);
      procedure UTMtoLatLongDegree(xutm,yutm : float64; var Lat,Long : float64);

      function ProjectedCoordinatesStringFromScreen(x,y : integer; LongForm : boolean = true) : shortString;
      function ProjectedCoordinatesStringFromLatLong(lat,long : float64; LongForm : boolean = true) : shortString;

      procedure DrawLatLine(Canvas : tCanvas; Lat : float64); overload;
      procedure DrawLatLine(Canvas : tCanvas; Lat : float64; var LeftY,RightY : integer); overload;
      procedure DrawLongLine(Canvas : tCanvas; Long : float64); overload;
      procedure DrawLongLine(Canvas : tCanvas; Long : float64; var TopX,BottomX : integer); overload;

      function MapPerimeter : integer;

      function ScreenLocStr(x,y : integer) : shortstring;
      function MapZRangeString : shortstring;

      function ColsDisplayed : integer;
      function RowsDisplayed : integer;

      function ValidDEMMap : boolean;
      function ValidDEMonMap : boolean;
      function ValidSatonMap : boolean;
      function IsThisMapUTM : boolean;
      function MGRSValid : boolean;
      function FullDEMMap : boolean;
      function ValidProjectedCoordinates : boolean;

      function GetMapElev(xgrid,ygrid : float64; var Z : float32) : boolean;

      function NativeGridAllowedOnMap : boolean;

      procedure BoxToContainFan(Lat,Long,Range : float64; var uMinLat,uMinLong,uMaxLat,uMaxLong : float64);

      function GetElevColor(x,y : float64) : tPlatformColor;
      function QuickElevColor(x,y : integer) : tPlatformColor;
      procedure SetUpElevationColorTable;
      procedure DefineSlopeColors;
      procedure ColorTintedElevationMap(var inBitmap : tMyBitmap);

      function ZoomableVectorMap : boolean;

      procedure DrawGridLines(var inBitmap : tMyBitmap);
      procedure QuickShapeFileDisplay(Var Bitmap : tMyBitmap; fName : PathStr; ColorFieldPresent : boolean = false; color : tColor = -1);

      procedure DrawWorldFileImageOnMap(var SummaryBitmap : tBitmap; fName : PathStr; Exagerrate : boolean = false);
      procedure WriteMapsWorldFile(fName : PathStr);
      procedure StretchWorldFileMap(var StretchBitmap : tBitmap; fName : PathStr; Color : integer = -99);

      procedure AnaglyphBitmap(var RightImage,LeftImage : PathStr; var Bitmap : tBitmap);

      {$IfDef VCL}
        procedure DrawSecondGrid(var Bitmap : tMyBitmap; Grid2 : integer);
        procedure PlotVectorOverlay(Bitmap : tMyBitmap; FName : PathStr);
        procedure OverlaySRTMWaterBodies(Bitmap : tMyBitmap; WaterMask : boolean = false);

        {$IfDef AllowDEMGeomorph}
           procedure OverlayCategories(Bitmap : tMyBitmap; TerrainCategory : tTerrainCatDefinition);
           function CreateCategoryOverlay(TerrainCategory : tTerrainCatDefinition) : tMyBitmap;
           function TerrainCategoryLegend : tMyBitmap;
        {$EndIf}

        function MakeVATLegend : tMyBitmap;
        function ChangeMapLegend : tMyBitmap;
        function DrawLegendOnBitmap : tMyBitmap;
        procedure DrawLegendsOnMap(var Bitmap : tMyBitmap);
        procedure GazetteerLegend(var Bitmap : tMyBitmap);
        procedure PlotDataBaseLegends(var Bitmap : tMyBitmap);

        function SizeOfPixel(x,y : integer) : float64;

        function DrawScaleBarOnBitmap(PixelSize : float64 = -1) : tMyBitmap;
        procedure DrawScaleBar(Canvas : tCanvas; xstart,Ystart : integer);

        procedure DrawDeclinationDiagramOnBitmap(var Bitmap : tMyBitmap);
        function MapSymbolAtLatLongDegree(Canvas : tCanvas; Lat,Long : float64; Sym : tDrawingSymbol; Size : integer; Color : tPlatformColor) : boolean; overload;
        function MapSymbolAtLatLongDegree(Canvas : tCanvas; Lat,Long : float64; Symbol : tFullSymbolDeclaration) : boolean; overload;

        procedure DrawMapOverlays(var inBitmap : tMyBitmap);
        procedure ShowDEMGrid(Bitmap : tMyBitmap);

        procedure PlotAnIcon(Bitmap : tMyBitmap; Table : tMyData; LatFieldName,LongFieldName : ShortString; FieldName : ShortString = 'ICONNAME'; ScalingFactor : byte = 100); overload;
        procedure PlotAnIcon(Bitmap : tMyBitmap; Lat,Long : float64; fName : PathStr; ScalingFactor : byte = 100); overload;
        procedure PlotDataBases(var Bitmap : tMyBitmap);

        procedure ComputeDatumShifts(Canvas : tCanvas; Lat,Long : float64; var TotalShiftUTM,TotalShiftGeo : float64; Mark : tMarkShift = msNone);

        procedure LabelGazetteerFeatures(var inBitmap : tMyBitmap);

        procedure UTMBox(Bitmap : tMyBitmap; LowLong,HighLong : float64; x1,y1,x2,y2,inc : integer; var xc,yc,xr : integer; UseDatum : tMapProjection);
        procedure LatLongBox(Bitmap : tMyBitmap; Lat1,Long1,Lat2,Long2 : float64; {SW corner first, then NE corner} inc : integer; var xc,yc : integer; Fill : boolean = false);

        procedure SmartWorldOutline(Bitmap : tMyBitmap);
        procedure ThreadContours(Bitmap : tMyBitmap; Contours : tStringList; ExportContourShapeFile : boolean = false);
        procedure DrawContoursInArea(Bitmap : tMyBitmap; ContInt,DEMToUse : integer);
        procedure OverlayContours(Bitmap : tMyBitmap);
        procedure DrawRangeCircleLatLong(Canvas : tCanvas; Lat,Long : float64; RangeCirclesRadius : float64; RangeCircleColor : tColor; RangeCircleWidth : Integer; AddToShape : boolean = false);
        procedure PositionBitmap(var Bitmap : tMyBitmap; var Legend : tMyBitmap; LegendLocation : byte);
        procedure TerrainShadows(Bitmap : tMyBitmap);
        procedure OutlineSectors(Canvas : tCanvas);
        procedure DrawArc(Canvas : tCanvas; Lat,Long : float64; Range,StartAz,EndAz : float64; RangeCircleColor : tColor; RangeCircleWidth : Integer; AddToShape : boolean = false; DrawPie : boolean = false);
        procedure OutlineFans(Canvas : tCanvas; FanTable : tMyData; FanOutLineColor : tColor; FanOutLineWidth : Integer);
        procedure DrawOrientedLine(Bitmap : tMyBitmap; StartLat,StartLong,Bearing : float64);
        procedure CarefulPolyLineDraw(Bitmap : tMyBitmap; var PointsInPolyline : integer;  var PolyLinePoints : tPolyLinePts);
      {$EndIf}

      function BlowUpAtPoint(Lat,Long,Range : float64; Zoom : integer; CopyOverlays : boolean = false) : tMapDraw;

      function OnScreen(x,y : integer) : boolean;
      function DEMGridOnScreen(x,y : float64) : boolean;
      function NearScreen(x,y : integer) : boolean;
      function LatLongOnScreen(Lat,Long : float64) : boolean;  inline;
      function LatLongBoxOnScreen(BoundBoxToTest : sfBoundBox) : boolean;
      procedure SetUTMTickInt(var UTMTickInt : float64);
      function CurrentSatelliteColors : shortstring;

      function MapLimits : tGridLimits;
      function MapAreaDEMGridLimits : tGridLimits;
      function SatLimits : tGridLimits;

      function MapZoomFactor : float64;
      function KMLcompatibleMap : boolean;

      function AFullWorldMap : boolean;
      function UTMGridToTrueNorthAngle(Lat,Long : float64) : float64;
      function MapSizeString : shortstring;
      procedure CopyMapParams(var NewMapDraw : tMapDraw; CopyOverlays : boolean);

      procedure ShowMapGridLimits(Where : shortString);
      function DistanceOffMap(Lat,Long : float64) : shortstring;

      procedure PlotLineShapeFileOnMap(fName : PathStr; var Bitmap : tMyBitmap; LineColor : tPlatformColor; LineWidth : byte);
      procedure PlotShapeFileGroupOnMap(IndexFName : PathStr; Bitmap : tMyBitmap; BaseDir : PathStr = '');
      procedure PlotShapeFileGroup(inBitmap : tMyBitmap; FName : PathStr);
      procedure QuietPlotTIGERShapeFileOnMap(fName : PathStr; var Bitmap : tMyBitmap; RoadsOnly : boolean = false; MaskSize : integer = -1; StreamsOnly : boolean = false);
      procedure PlotDataBase(DBNum : integer; var Bitmap : tMyBitmap);
      procedure PlotDBLegend(DBNum : integer; var Bitmap : tMyBitmap);
      procedure PlotDBOnMapFromRules(DBNum : integer; var Bitmap : tMyBitmap; Rules : PathStr; MaskSize : integer = -1);
      procedure PlotTigerShapeFileOnMap(DBNum : integer; var Bitmap : tMyBitmap; RoadsOnly : boolean = false; MaskSize : integer = -1; StreamsOnly : boolean = false);
      procedure AddDatabaseToMask(DBonTable : integer; var Bitmap : tMyBitmap; UseShapeFile,MaskShapesAreIn : boolean; MaskingDistance : float64; Color : tPlatformColor);

      procedure DrawThreadCheckPlane(Bitmap : tBitmap; xglo,yglo,xghi,yghi : integer; xg1,yg1,Dip,Strike,DipDir  : float64);
      procedure GetMapScaleFactor(var Lat,Long,Maph,Mapk: float64; var Prime : boolean);
      function MapUTMZoneString : shortstring;

      {$IfDef ShowProjectedLimits}
         procedure ShowProjectedLimits(Where : shortstring);
      {$EndIf}

      {$IfDef ExOSM}
      {$Else}
          procedure OSMthread(Layer : integer; fName : PathStr; var Bitmap : tMyBitmap);
      {$EndIf}


    {$IfDef ExViewshed}
    {$Else}
        procedure DrawRadialFan(var WeaponsFan : tWeaponsFan; var ShowName,CoverName : PathStr);
        procedure DrawFanOnMap(WeaponsFan : tWeaponsFan; ShowName,CoverName : PathStr{; ReallyDraw : boolean = true});
        procedure DrawAllFans(Table : tMyData; var MapBitmap : tMyBitmap);
        procedure InsureAllFansDrawn(Table : tMyData; var SummaryBitmap : tMyBitmap; Memo1 : tMemo = nil);
        procedure AddFanToMap(var WeaponsFan : tWeaponsFan);
        procedure FanBitmap(Lat,Long : float64; var Bitmap : tMyBitmap);

        {$IfDef VCL}
        procedure ComputeMultiSensorCoverage(var NumFansCoveringFName : PathStr;  WeaponsTable : tMyData; ViewshedSummary : tStringList = Nil);
        {$EndIf}
     {$EndIf}

      {$IfDef ExMilIcons}
      {$Else}
         procedure PlotMilIconsOnBitmap(Bitmap : tMyBitmap; fName : PathStr);
      {$EndIf}

     {$IfDef ExSat}
     {$Else}
         procedure DefineSatMap(CurSat : integer; mt : tMapType);
     {$EndIf}

     {$IfDef ExPLSS}
     {$Else}
        procedure DrawPLSSGrid(inBitmap : tMyBitmap);
     {$EndIf}

     {$IfDef ExTiger}
     {$Else}
         procedure DrawFullTigerCoverage(Bitmap : tMyBitmap; RoadsOnly : boolean = false; AllRoads : boolean = false; DrawMarginalia : boolean = true; RoadMaskWidth : integer = -1; StreamsOnly : boolean = false);
     {$EndIf}

     {$IfDef ExPointCloud}
     {$Else}
         procedure PlotALasFile(Cloud : integer; fName : PathStr; var BMPMemory : tBMPMemory);
         procedure BackgroundPlotLASFilesOnMap(var inBitmap : tMyBitmap);
     {$EndIf}

     {$IfDef ExTissot}
     {$Else}
         procedure DrawTissotIndicatrixOverlay(var Bitmap : tMyBitmap);
         procedure DrawTissotIndicatrix(Canvas : tCanvas; lat,long : float64);
      {$EndIf}
  end;


function TheHue(MergeHue : float64; i : integer) : float64; inline;

function ContColorFunct(z : integer) : TColor;

procedure ExpandRoute(DEM : integer; var Route : tStringList; PointSep : float64; AddFirst,AddLast,AddTurns : boolean);
function UTMGridLabel(v : float64) : ShortString;

procedure DrawAndDeleteOverlay(var Bitmap,Overlay : tMyBitmap; Opacity : byte = 100; StartX : SmallInt = 0; StartY : SmallInt = 0; TextMode : boolean = false);
procedure DrawOverlayNoDelete(var Bitmap,Overlay : tMyBitmap; Opacity : byte = 100; StartX : SmallInt = 0; StartY : SmallInt = 0; TextMode : boolean = false);

function SameProjection(Map1,Map2 : tMapDraw) : boolean;

function MakeChangeMapLegend(DEMonMap : integer) : tMyBitmap;


var
   AmbushCountMyBitmap : tMyBitmap;
   LocationLabel,
   ConnectPoints,
   SizeIsCorrectThankYou : boolean;
   LocationColor : tColor;
   LocationSymSize,TopCont,BotCont  : integer;
   LocationSymbol : tDrawingSymbol;
   FanCoverPerCent : float64;
   ForceMinElev,ForceMaxElev : float32;
   pPal,seaPal,SlopeColorChoices : tRGBLookUp;
   AspColor : array[0..360] of tPlatformColor;


implementation

uses
   {$IfDef VCL}
      Nevadia_Main,
      DEMMapf,
      Thread_timers,
      drg_anaglyph,
      dem_indexes,
      PetImage_form,
   {$EndIf}

   {$IfDef ExPLSS}
   {$Else}
      DEM_PLSS,
   {$EndIf}

   {$IfDef ExGeoStats}
   {$Else}
      DEMStat,
      NetMainW,
   {$EndIf}

   {$IfDef ExGraphs}
   {$Else}
      BaseGraf,
      PETGraphColors,
   {$EndIf}

   {$IfDef VCL}
      //GetMapF,
      DEMElevOps,
   {$EndIf}

   {$IfDef ExGeotiff}
   {$Else}
      GEOTIFF,
   {$EndIf}

   {$IfDef ExNLCD}
   {$Else}
      DEM_NLCD,
   {$EndIf}

   {$IfDef ExMagVar}
   {$Else}
      DEMMagVar,
   {$EndIf}

   {$IfDef ExMilIcons}
   {$Else}
      dem_milicon,
   {$EndIf}

   {$IfDef ExAdvancedSats}
   {$Else}
      rgb_colors_three_params,
      demsatcontrast,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
      DEMEROS,
   {$EndIf}

   {$IfDef ExTIGER}
   {$Else}
      DEMTiger,
   {$EndIf}

   {$IfDef ExVectorOverlay}
   {$Else}
      DEMTerrC,
   {$EndIf}

   {$IfDef ExViewshed}
   {$Else}
      {$IfDef VCL}
         DEMWeapn,
         DEMfanparams,
      {$EndIf}

      Weapons_fan_thread,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      petdbUtils,
      DataBaseCreate,
      demdatabase,
      DEMESRIShapeFile,
      Make_Tables,
   {$EndIf}

   {$IfDef VCL}
      US_Properties,
   {$EndIf}

   {$IfDef ExPointCloud}
   {$Else}
      LAS_lidar,
      Point_cloud_options,
   {$EndIf}

   DEM_Manager,
   DEMCoord;

const
   SlopeAspectColor : array[1..3, 1..8] of tColor = ((8500632,9480306,11374204,10515852,10582964,9407435,9086405,9027517),
                                                     (5817485,7449405,11958352,10307447,10243520,8024039,7120610,6216662),
                                                     (54916,4500224,12609536,10682476,10223818,6837759,4697087,64244));


   {$IfDef ExViewshed}
   {$Else}
      {$I demmapdraw_viewshed.inc}
   {$EndIf}

   {$IfDef ExTissot}
   {$Else}
      {$I demmapdraw_tissot.inc}
   {$EndIf}

   {$IfDef ExMapGrids}
   {$Else}
      {$I demmapdraw_grids.inc}
   {$EndIf}

   {$IfDef VCL}
      {$I demmapdraw_map_colors.inc}
      {$I demmapdraw_plot_dbs.inc}
      {$I demmapdraw_plot_vcl.inc}
   {$EndIf}


   {$IfDef ExWMS}
   {$Else}
      {$I demmapdraw_wms.inc}
   {$EndIf}

   {$I demmapdraw_coords.inc}
   {$I demmapdraw_map_sizing.inc}


function MakeChangeMapLegend(DEMonMap : integer) : tMyBitmap;
var
   Vat : tStringList;
   fName : PathStr;
   color : tColor;
   TStr : shortstring;
   CatHeight,Cat : integer;
begin
   CreateBitmap(Result,1200,800);

   Result.Canvas.Font.Size := MDDef.LegendFont.Size;
   CatHeight := 6 * Result.Canvas.TextHeight('Wy') div 5;

   Cat := 0;

   ClearBitmap(Result,clNearWhite);
   Result.Canvas.Font.Style := [fsBold];

   Result.Canvas.TextOut(5,1,'       % of area      Category');
   inc(Cat);
   Result.Canvas.Pen.Color := clLime;
   Result.Canvas.Brush.Color := clLime;
   Result.Canvas.Brush.Style := bsSolid;
   Result.Canvas.Rectangle(5,Cat*CatHeight,40,succ(Cat)*CatHeight);
   Result.Canvas.Brush.Style := bsClear;
   Tstr := RealToString(100-DEMGlb[DEMonMap].PercentileOfElevation(MDDef.TopCutLevel),9,2) + '%   Positive Change > ' + RealToString(MDDef.TopCutLevel,-8,-2);
   Result.Canvas.TextOut(45,Cat*CatHeight + 4, TStr);

   inc(Cat);
   Result.Canvas.Pen.Color := clRed;
   Result.Canvas.Brush.Color := clRed;
   Result.Canvas.Brush.Style := bsSolid;
   Result.Canvas.Rectangle(5,Cat*CatHeight,40,succ(Cat)*CatHeight);
   Result.Canvas.Brush.Style := bsClear;
   TStr := RealToString(DEMGlb[DEMonMap].PercentileOfElevation(MDDef.BottomCutLevel),9,2) + '%   Negative Change < ' + RealToString(MDDef.BottomCutLevel,-8,-2);
   Result.Canvas.TextOut(45,Cat*CatHeight + 4,TStr);
   PutBitmapInBox(Result);
end;






function SameProjection(Map1,Map2 : tMapDraw) : boolean;
begin
   {$If Defined(SameProjection)} WriteLineToDebugFile('Same projection,' + Map1.BaseTitle + ' and ' + Map2.BaseTitle); {$EndIf}
   Result := Map1.BasicProjection = Map2.BasicProjection;
   if Result then begin
      if (Map1.BasicProjection = bpUTM) then begin
         {$If Defined(SameProjection)} WriteLineToDebugFile('Same projection UTMs,' + IntToStr(Map1.PrimMapProj.projUTMZone) + ' and ' + IntToStr(Map2.PrimMapProj.projUTMZone) ); {$EndIf}
         Result := Map1.PrimMapProj.projUTMZone = Map2.PrimMapProj.projUTMZone;
      end;
   end;
end;



function tMapDraw.ColsDisplayed : integer;
begin
    Result := round(MapCorners.BoundBoxDataGrid.xmax - MapCorners.BoundBoxDataGrid.xmin);
end;

function tMapDraw.RowsDisplayed : integer;
begin
    Result := round(MapCorners.BoundBoxDataGrid.ymax - MapCorners.BoundBoxDataGrid.ymin);
end;


function tMapDraw.ScreenToLatLongString(xpic,ypic : integer) : shortstring;
var
   Lat,Long : float64;
begin
   ScreenToLatLongDegree(xpic,ypic,Lat,Long);
   Result := LatLongDegreeToString(Lat,Long);
end;


function tMapDraw.MapUTMZoneString : shortstring;
begin
   Result := ' utm=' + IntToStr(PrimMapProj.projUTMZone) + PrimMapProj.LatHemi;
end;


function tMapDraw.MapPerimeter : integer;
var
   csv : tStringList;
   i : integer;
   fName : PathStr;
   x,y : integer;
   Lat,Long : float64;

   procedure AddPoint(x,y : integer);
   begin
      ScreenToLatLongDegree(x,y,Lat,Long);
      csv.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7));
   end;

begin
   csv := tStringList.Create;
   csv.Add('LAT,LONG');

    //top side
   y := pred(MapYSize);
   for i := 0 to 10 do begin
      x :=  i * MapXSize div 10;
      AddPoint(x,y)
   end;

   //right side
   x := MapXSize;
   for i := 9 downto 0 do begin
      y := i * MapYSize div 10;
      AddPoint(x,y)
   end;

   //bottom side
   y := 0;
   for i := 9 downto 0 do begin
      x := i * MapXSize div 10;
      AddPoint(x,y)
   end;

   //left side
   x := 0;
   for i := 1 to 9 do begin
      y := i * MapYSize div 10;
      AddPoint(x,y)
   end;

   fName := MDTempDir + 'outline.csv';
   Result := StringList2CSVtoDB(csv,fName);
end;


function tMapDraw.GetBoundBoxGeo : sfBoundBox;
var
   i : integer;
   x,y : integer;

   procedure AddPoint(x,y : integer);
   var
      Lat,Long : float64;
   begin
      ScreenToLatLongDegree(x,y,Lat,Long);
      CompareValueToExtremes(Lat,Result.ymin,Result.ymax);
      CompareValueToExtremes(Long,Result.xmin,Result.xmax);
   end;

begin
   InitializeBoundBox(Result);
    //top side
   y := pred(MapYSize);
   for i := 0 to 10 do begin
      x :=  i * MapXSize div 10;
      AddPoint(x,y)
   end;

   //right side
   x := MapXSize;
   for i := 9 downto 0 do begin
      y := i * MapYSize div 10;
      AddPoint(x,y)
   end;

   //bottom side
   y := 0;
   for i := 9 downto 0 do begin
      x := i * MapXSize div 10;
      AddPoint(x,y)
   end;

   //left side
   x := 0;
   for i := 1 to 9 do begin
      y := i * MapYSize div 10;
      AddPoint(x,y)
   end;
end;

function TMapDraw.DEMGridOnScreen(x,y : float64) : boolean;
begin
   Result := (x >= MapCorners.BoundBoxDataGrid.xmin) and (x <= MapCorners.BoundBoxDataGrid.xmax) and (y >= MapCorners.BoundBoxDataGrid.ymin) and (y <= MapCorners.BoundBoxDataGrid.ymax);
end;


function tMapDraw.GetBoundBoxUTM : sfBoundBox;
const
  EachSide = 5;
var
   i : integer;
   x,y : integer;

   procedure AddPoint(x,y : integer);
   var
      xutm,yutm : float64;
   begin
      ScreenToUTM(x,y,xutm,yutm);
      {$If Defined(ShowUTMZone)} WriteLineToDebugFile('bbutm: ' + IntToStr(x) + ' ' + IntToStr(y) + ' ' + UTMString(xutm,yutm)); {$EndIf}
      CompareValueToExtremes(yutm,Result.ymin,Result.ymax);
      CompareValueToExtremes(xutm,Result.xmin,Result.xmax);
   end;

begin
   {$If Defined(ShowUTMZone)} WriteLineToDebugFile('');  WriteLineToDebugFile('tMapDraw.GetBoundBoxUTM in for ' + BaseTitle); {$EndIf}
   InitializeBoundBox(Result);
    //top side
   y := pred(MapYSize);
   for i := 0 to EachSide do begin
      x :=  i * MapXSize div eachSide;
      AddPoint(x,y)
   end;

   //right side
   x := MapXSize;
   for i := Pred(EachSide) downto 0 do begin
      y := i * MapYSize div EachSide;
      AddPoint(x,y)
   end;

   //bottom side
   y := 0;
   for i := Pred(EachSide) downto 0 do begin
      x := i * MapXSize div EachSide;
      AddPoint(x,y)
   end;

   //left side
   x := 0;
   for i := 1 to pred(EachSide) do begin
      y := i * MapYSize div EachSide;
      AddPoint(x,y)
   end;
   {$If Defined(ShowUTMZone)} WriteLineToDebugFile('tMapDraw.GetBoundBoxUTM = ' + sfBoundBoxToString(Result,1)); {$EndIf}
end;


{$IfDef ShowProjectedLimits}
   procedure TMapDraw.ShowProjectedLimits(Where : shortstring);
   begin
      WriteLineToDebugFile(Where + ' projected limits, ' + sfBoundBoxToString(MapCorners.BoundBoxProj,4));
   end;
{$EndIf}


function TMapDraw.CurrentSatelliteColors : shortstring;
begin
  if MapType in [mtSatTrueColor,mtSatFalseColor,mtSatPickColor,mtSatFalseVeg] then begin
     //Result := RGBString(SatView.RedBand,SatView.GreenBand,SatView.BlueBand);
     Result := SatImage[SATonMap].BandShortName[SatView.RedBand] + '-' + SatImage[SATonMap].BandShortName[SatView.GreenBand] + '-' +  SatImage[SATonMap].BandShortName[SatView.BlueBand];
     if SatView.PanSharpenImage then Result := Result + ' (pan sharpen)';
  end
  else Result := SatImage[SATonMap].BandShortName[SatView.BandInWindow];
end;


procedure TMapDraw.ClipDataGrid(var bb : sfBoundBox);
begin
   if DEMMap then begin
      DEMGlb[DEMonMap].ClipDEMGrid(bb.xmin,bb.ymin);
      DEMGlb[DEMonMap].ClipDEMGrid(bb.xmax,bb.ymax);
   end
   else if ValidSatOnMap then begin
      SatImage[SatOnMap].ClipSatGrid(SatImage[SatOnMap].BandForSize,bb.xmin,bb.ymin);
      SatImage[SatOnMap].ClipSatGrid(SatImage[SatOnMap].BandForSize,bb.xmax,bb.ymax);
   end;
end;


function tMapDraw.ValidProjectedCoordinates : boolean;
begin
   Result := false;
   if (VectorIndex <> 0) then begin
      Result := (PrimMapProj.PName in [AlbersEqAreaConicalEllipsoid,MercatorEllipsoid,PolarStereographicEllipsoidal,LambertConformalConicEllipse,UK_OS,Finn_GK,GeneralTransverseMercator,LamAzEqAreaEllipsoidal,IrishGrid]);
   end;
   if DEMMap and (DEMGlb[DEMonMap].DEMMapProjection <> Nil) then begin
      Result := true;
   end;

   {$IfDef ExSat}
   {$Else}
      if ValidSatOnMap and (SatImage[SATonMap].RegVars.Registration = RegProjection) then Result := true;
   {$EndIf}
end;



procedure tMapDraw.DrawWorldFileImageOnMap(var SummaryBitmap : tBitmap; fName : PathStr; Exagerrate : boolean = false);
var
   FanBMP : tBitmap;

   {$IfDef VCL}
      i,j : integer;
      Dir : DirStr;
      bName : NameStr;
      Ext : ExtStr;
      FName2 : PathStr;
   {$EndIf}

   {$IfDef FMX}
      Region : tRectF;
   {$EndIf}
begin
   if FileExists(fName) and (SummaryBitmap <> Nil) then begin
      FanBMP := Nil;

      {$IfDef FMX}
         StretchWorldFileMap(FanBMP,fName);
         Region.Create(0,0,pred(SummaryBitmap.Width),pred(SummaryBitmap.Height));
         SummaryBitmap.Canvas.BeginScene;
         SummaryBitmap.Canvas.DrawBitmap(FanBMP,region,region,0.5);
         SummaryBitmap.Canvas.EndScene;
      {$EndIf}

      {$IfDef VCL}
         FSplit(fName,Dir,bName,Ext);
         fName2 := Dir + bName + '_dtm' + Ext;
         if FileExists(fName2) then begin
            {$If Defined(WorldFileOverlay)} WriteLineToDebugFile('tMapDraw.DrawWorldFileImageOnMap stretching with' + fName2); {$EndIf}
            StretchWorldFileMap(FanBMP,fName2,ClGreen);
            SummaryBitmap.Canvas.CopyMode := cmSrcAnd;
            SummaryBitmap.Canvas.Draw(0,0,FanBMP);
            SummaryBitmap.Canvas.CopyMode := cmSrcInvert;
            StretchWorldFileMap(FanBMP,fName,clRed);
         end
         else begin
            {$If Defined(WorldFileOverlay)} WriteLineToDebugFile('tMapDraw.DrawWorldFileImageOnMap stretching ' + fName); {$EndIf}
            SummaryBitmap.Canvas.CopyMode := cmSrcAnd;
            StretchWorldFileMap(FanBMP,fName);
         end;

         SummaryBitmap.Canvas.Draw(0,0,FanBMP);
         if Exagerrate then begin
            {$If Defined(WorldFileOverlay)} WriteLineToDebugFile('tMapDraw.DrawWorldFileImageOnMap exaggerate'); {$EndIf}
            for i := -1 to 1 do
               for j := -1 to 1 do
                  SummaryBitmap.Canvas.Draw(i,j,FanBMP);
         end;
      {$EndIf}

      {$IfDef RecordOverlays}
         FanBMP.SaveToFile(MDTempDir + 'DrawWorldFileImageOnMap_fan' + OverlayFExt);
         SummaryBitMap.SaveToFile(MDTempDir + 'DrawWorldFileImageOnMap_summary' + OverlayFExt);
      {$EndIf}
      FreeAndNil(FanBMP);
   end
   else begin
      {$If Defined(WorldFileOverlay) or Defined(RecordFan) or Defined(RecordOverlays)} WriteLineToDebugFile('File missing in tMapDraw.DrawWorldFileImageOnMap=' + fName); {$EndIf}
   end;
end;




function TMapDraw.NativeGridAllowedOnMap : boolean;
begin
   Result := (BasicProjection = bpOther) or ( (VectorIndex <> 0) and (PrimMapProj.PName in [LamAzEqArea,LambertConformalConicEllipse,UK_OS,IrishGrid]));
end;


function tMapDraw.BlowUpAtPoint(Lat,Long,Range : float64; Zoom : integer; CopyOverlays : boolean = false) : tMapDraw;
const
   ExtraMargin = 250;
var
   uMinLat,uMinLong,uMaxLat,uMaxLong : float64;
   Bmp : tMyBitmap;
begin
   {$IfDef RecordMapBlow} WriteLineToDebugFile('tMapDraw.BlowUpAtPoint '  + MapSizeString);      {$EndIf}
   Result := Nil;
   CopyMapParams(Result,false);
   Result.MapType := mtDEMBlank;
   BoxToContainFan(Lat,Long,Range,uMinLat,uMinLong,uMaxLat,uMaxLong);
   Result.MaximizeLatLongMapCoverage(uMinLat,uMinLong,uMaxLat,uMaxLong);
   Result.ResizeMapByPercentage(Zoom);
   BMP := Nil;
   Result.DrawMapOnBMP(BMP,false,false);
   if (BMP <> Nil) then BMP.Free;
   {$IfDef RecordMapBlow} WriteLineToDebugFile('tMapDraw.BlowUpAtPoint out,  ' + Result.MapSizeString);   {$EndIf}
end;


function TMapDraw.MapZRangeString : shortstring;
begin
   Result := '  z range=' + RealToString(MinMapElev,-12,-2) + ' to ' +  RealToString(MaxMapElev,-12,-2);
end;


procedure tMapDraw.DrawMapOnBMP(var Bitmap : tMyBitmap; DrawBaseMap : boolean = true; DrawOverlays : boolean = true);
label
  CleanUp,RestartWMS;
var
   z,DIP,TI,GV,lat,long : float64;
   Table : tMyData;


          function bpName : shortstring;
          begin
             case BasicProjection of
                bpUTM : Result := 'bpUTM';
                bpLatLong : Result := 'bpLatLong';
                bpOther : Result := 'bpOther';
             end;
          end;


begin
   {$If Defined(RecordMapDraw) or Defined(RecordTiming) or Defined(RecordUTMZone) or Defined(RecordElevationScaling) or Defined(RecordPixelSize) or Defined(RecordMapResize) or Defined(RecordSat)}
       WriteLineToDebugFile('tMapDraw.DrawMapOnBMP in ' + MapSizeString + '  ' + PrimMapProj.GetProjectionName + '  bp=' + bpName);
   {$EndIf}
   {$IfDef ShowProjectedLimits} WriteLineToDebugFile('tMapDraw.DrawMapOnBMP in, proj limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxProj,3)); {$EndIf}
   {$IfDef ShowProjectedLimits} if (VectorIndex <> 0) then WriteLineToDebugFile('tMapDraw.DrawMapOnBMP in, data grid limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxDataGrid,1)); {$EndIf}
   {$If Defined(track_f) or Defined(RecordMapDrawCreation) or Defined(RecordLong0)} PrimMapProj.ShortProjInfo('DrawMapOnBMP in');  {$EndIf}
   {$If Defined(RecordElevRange)} if DEMMap then  WriteLineToDebugFile('elev range ' + RealToString(MinMapElev,-12,-2) + ' to ' + RealToString(MaxMapElev,-12,-2) + ' ' + MapSizeString); {$EndIf}
   {$If Defined(RecordMapDraw)} WriteLineToDebugFile('deltas=' + RealToString(MapCorners.Projdx,-12,-6) +  '/' + RealToString(MapCorners.Projdy,-12,-6) + '  ' + MapSizeString); {$EndIf}

   try
      if (NoDrawingNow) then exit;

      try
         MapDrawValid := true;
         GridTrueAngle := -999;
         if (VectorIndex <> 0) or ValidSatOnMap or (ValidDEMonMap and (DEMGlb[DEMonMap].DEMheader.DigitizeDatum <> Rectangular)) then begin
             ScreenToLatLongDegree(MapXSize div 2, MapYSize div 2, Lat,Long);
             MapMagDec := 0;
            {$IfDef ExMagVar}
            {$Else}
               if ValidDEMonMap then z := 0.0005*(MinMapElev+MaxMapElev)
               else z := 0;
               try
                  MagVr1(z,lat,Long,CurMagYear,MapMagDEC,DIP,TI,GV);
               finally
               end;
            {$EndIf}
         end;
         CreateBitmap(Bitmap,MapXSize,MapYSize);

         if DEMmap or ValidSatImage(SatOnMap) then ClearBitmap(Bitmap,ConvertPlatformColorToTColor(MDdef.MissingDataColor));
         if DrawBaseMap then begin
            {$If Defined(RecordFullMapDraw) or Defined(RecordPixelSize)} WriteLineToDebugFile('tMapDraw.DrawMapOnBMP DrawBaseMap ' + MapSizeString); {$EndIf}

            if (BaseMapFName <> '') and FileExists(BaseMapFName) then begin
               {$If Defined(RecordFullMapDraw) or Defined(RecordKeyMap)} WriteLineToDebugFile('Loading bitmap ' + MapSizeString); {$EndIf}
               Bitmap := LoadBitmapFromFile(BaseMapFName);
            end
            else begin
               {$If Defined(RecordFullMapDraw) or Defined(RecordKeyMap)} WriteLineToDebugFile('Drawing new map=' + MapSizeString); {$EndIf}
               BaseMapFName := NextFileNumber(MDTempDir, 'Base_map_', OverlayFExt);
               if WantShowProgress and ShowSatProgress and (MapType <> mtDEMBlank) then StartProgressAbortOption('Draw map ' + BaseTitle);
               if (VectorIndex <> 0) then begin
                  {$If Defined(RecordFullMapDraw) or Defined(RecordLong0) or Defined(RecordKeyMap)} PrimMapProj.ShortProjInfo('Vector map draw'); {$EndIf}
                  ScreenToLatLongDegree(0,0,Lat,LongLeftSide);
                  ScreenToLatLongDegree(0,Bitmap.Height,Lat,Long);
                  if (Long < LongLeftSide) then LongLeftSide := Long;
               end
               else if (MapType in [mtLASclass]) then begin
                  DrawLASClassMap(BitMap);     //Las_rgb_colors[zi]);
               end
               else if ValidDEMMap then begin
                  if (BaseTitle = '') then begin
                     if DEMGlb[DEMonMap].LandcoverGrid then BaseTitle := 'Land cover'
                     else if DEMGlb[DEMonMap].ElevationGrid then BaseTitle := 'DEM'
                     else BaseTitle := 'Grid';
                     BaseTitle := BaseTitle + ': ' + RemoveUnderscores(DEMGlb[DEMonMap].AreaName);
                     if (MapType = mtDEMContour) then BaseTitle := RemoveUnderscores(DEMGlb[DEMonMap].AreaName) + ' with ' + RealToString(DEMGlb[DEMonMap].ZinMeters(MapOverlays.ConInt),-6,-1) + ' m contours';
                     if isReflectanceMap(MapType) then BaseTitle := RemoveUnderscores(DEMGlb[DEMonMap].AreaName) + ': Sun Az=' + RealToString(MDdef.RefPhi,-4,0) + '°, Elev='+ RealToString(MDDef.RefTheta,-4,0) + DegSym;
                  end;

                  {$If Defined(RecordFullMapDraw) or Defined(RecordKeyMap)} WriteLineToDebugFile('Draw DEM map type=' + IntToStr(MapType) + '  ' + BaseTitle); {$EndIf}
                  if (MapType = mtDEMContour) then begin
                      {$IfDef VCL}
                      DrawContoursInArea(BitMap,MapOverlays.ConInt,DEMonMap);
                      {$EndIf}
                  end
                  else if isSlopeMap(MapType) or (MapType in [mtDEMaspectSlope,mtDEMAspect,mtFlowDir360,mtFlowDirArc,mtFlowDirTau]) or (DEMGlb[DEMonMap].DEMheader.ElevUnits in [AspectDeg]) then begin
                     if isSlopeMap(MapType) then BaseTitle := 'Slope Map (' + SlopeMethodName(MDdef.SlopeAlg) + ')'
                     else BaseTitle := 'Aspect Map';
                     BaseTitle :=  RemoveUnderscores(DEMGlb[DEMonMap].AreaName) + ' ' + BaseTitle;
                     DrawSlopeMap(BitMap);
                     {$If Defined(RecordFullMapDraw)} WriteLineToDebugFile('after DrawSlopeMap, bmp=' + MapSizeString); {$EndIf}
                  end
                  else if (MapType in [mtOpenness]) then begin
                     DrawOpennessMap(BitMap);
                  end
                  else if (MapType in [mtRGB]) then begin
                     DrawRGBMap(BitMap);
                  end
                  else if IsReflectanceMap(MapType) or IsElevationMap(MapType) or (MapType in [mtDEMVATTable,mtLandCover]) then begin
                     {$IfDef ExVegDensity}
                     {$Else}
                     if (DEMGlb[DEMonMap].VegGrid[1] <> 0) and (MDDef.VegOptionMap in [voVeg,voDSM]) then begin
                        if MDDef.VegOptionMap in [voVeg] then BaseTitle := 'Veg Ht ' + RemoveUnderscores(DEMGlb[DEMonMap].AreaName)
                        else BaseTitle := 'DSM ' + RemoveUnderscores(DEMGlb[DEMonMap].AreaName);
                        ShowVegOnMap(Bitmap);
                     end
                     else {$EndIf}
                     begin
                        ColorTintedElevationMap(BitMap);
                        {$If Defined (RecordFullMapDraw) or Defined(RecordTiming)} WriteLineToDebugFile('after ColorTintedElevationMap,  ' + MapSizeString); {$EndIf}
                     end;
                  end
                  else if (MapType in [mtDEMBlank]) then begin
                     {$IfDef VCL}
                        Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.BlankMapColor);
                        Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDDef.BlankMapColor);
                        Bitmap.Canvas.Brush.Style := bsSolid;
                        Bitmap.Canvas.Rectangle(0,0,pred(MapXsize),pred(MapYSize));
                     {$EndIf}
                  end;
               end
               {$IfDef ExSat}
               else begin
                  {$IfDef RecordFullMapDraw} WriteLineToDebugFile('Sat image undefined'); {$EndIf}
               end;
               {$Else}
               else if ValidSatOnMap then begin
                  {$If Defined(RecordMapDraw)  or Defined(RecordKeyMap)} WriteLineToDebugFile('satellite image'); {$EndIf}
                  if (MapType <> mtSatBlank) then begin
                     SatImage[SATonMap].DisplayImage(SatView,Self,Bitmap);
                  end;
                  {$IfDef RecordMapDraw} WriteLineToDebugFile('Sat image drawn OK, ' + MapSizeString); {$EndIf}
               end;
               {$EndIf}

               {$If Defined(RecordFullMapDraw) or Defined(RecordTiming) or Defined(RecordOverlays)} WriteLineToDebugFile('BaseMapFName=' + BaseMapFName + '   ' + MapSizeString); {$EndIf}

               {$IfDef VCL}
                  SaveLayerBitmap(Bitmap,BaseMapFName);
               {$Else}
                  if FirstMapDrawing and ValidDEM(DEMonMap) then begin
                     {$IfDef RecordOverlays} WriteLineToDebugFile('Try Basemap save, ' + MapSizeString); {$EndIf}
                     Bitmap.SaveToFile(BaseMapFName);
                  end;
               {$EndIf}
               {$IfDef Defined(RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('Basemap saved=' + BaseMapFName); {$EndIf}
               if WantShowProgress and ShowSatProgress and (MapType <> mtDEMBlank) then EndProgress;
            end;

            {$If Defined(RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('Basemap drawn, ' + MapSizeString); {$EndIf}
            {$If Defined(RecordLong0)} PrimMapProj.ProjectionParamsToDebugFile('tMapDraw.DrawMapOnBMP basemap drawm, MapDraw.PrimaryMapDatum'); {$EndIf}

            {$IfDef ExWMS}
            {$Else}
                if WMSOverlayOnMap then DrawMMSOverlay(Bitmap);
            {$EndIf}

            {$IfDef FMX}
               DrawGridLines(Bitmap);
            {$EndIf}

            InitialMapDrawn := true;
            if FastMapDraw then goto CleanUp;

            {$If Defined( RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('DrawMapOnBMP not fast map'); {$EndIf}

            if not GrayscaleSubdueOverlays then begin
               if MakeMapGrayscale then MakeTheBitmapGrayScale(Bitmap);
               if SubdueBase then MakeTheBitmapSubdued(Bitmap);
            end;

            {$IfDef VCL}
               if ValidDEM(DEMonMap) then begin
                  if (LASclassGrid <> 0) then DrawSecondGrid(Bitmap,LASClassGrid);
                  if (CHMGrid <> 0) then DrawSecondGrid(Bitmap,CHMGrid);
                  if (ChangeGrid <> 0) then DrawSecondGrid(Bitmap,ChangeGrid);
               end;
               if (MapMerge in [mmSlope,mmElevation,mmAspect]) then NewDEMSatelliteMerge(Bitmap);
               if (TerrainShadowsDEM <> 0) or (MapMerge = mmReflectance) then begin
                  {$IfDef RecordAnaglyph} WriteLineToDebugFile('Call TerrainShadows'); {$EndIf}
                  TerrainShadows(Bitmap);
               end;
            {$EndIf}
         end;

         {$IfDef NoOverlays}
         {$Else}
            if DrawOverlays then begin
               {$If Defined( RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('DrawMapOnBMP DrawOverlays start, ' + MapSizeString); {$EndIf}

               {$IfDef VCL}
                  if DEMGridRedraw then ShowDEMGrid(Bitmap);
               {$EndIf}

               {$IfDef ExPointCloud}
               {$Else}
                  if (pt_cloud_opts_fm <> Nil) and MDdef.AutoRedrawMapLAS then begin
                     if (LasLayerOnMap) then begin
                        {$If Defined(RecordFullMapDraw) or Defined(RecordOverlays) or Defined(RecordLAS)} WriteLineToDebugFile('DrawMapOnBMP, BackgroundPlotLASFilesOnMap'); {$EndIf}
                         BackgroundPlotLASFilesOnMap(Bitmap);
                     end
                     else begin
                        {$If Defined(RecordFullMapDraw) or Defined(RecordOverlays) or Defined(RecordLAS)} WriteLineToDebugFile('DrawMapOnBMP no LasLayerOnMap'); {$EndIf}
                     end;
                  end;
               {$EndIf}

               {$If Defined( RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('DrawMapOnBMP call DrawMapOverlays'); {$EndIf}
               {$If Defined(RecordLong0)} PrimMapProj.ProjectionParamsToDebugFile('tMapDraw.DrawMapOnBMP call draw overlays, MapDraw.PrimaryMapDatum'); {$EndIf}

               DrawMapOverlays(Bitmap);

               {$If Defined(RecordLong0)} PrimMapProj.ProjectionParamsToDebugFile('tMapDraw.DrawMapOnBMP done overlays, MapDraw.PrimaryMapDatum'); {$EndIf}

               {$IfDef ExIndexes}
               {$Else}
                  if (MapOwner in [moMapDataBase]) or ShowMapLibraryCoverage then begin
                     {$IfDef Defined( RecordFullMapDraw)  or Defined(RecordOverlays)} WriteLineToDebugFile('PlotMapLibrary'); {$EndIf}
                     PlotMapLibrary(Bitmap);
                  end;
               {$EndIf}

               {$IfDef VCL}
                  if (RangeCirclesFName <> '') then begin
                     Table := tMyData.Create(RangeCirclesFName);
                     while not Table.Eof do begin
                        DrawRangeCircleLatLong(Bitmap.Canvas,Table.GetFieldByNameAsFloat('LAT'),Table.GetFieldByNameAsFloat('LONG'),Table.GetFieldByNameAsFloat('RANGE'),Table.GetFieldByNameAsInteger('COLOR'),Table.GetFieldByNameAsInteger('LINE_WIDTH'),true);
                        Table.Next;
                     end;
                  end;
                  {$If Defined( RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('tMapDraw.DrawMapOnBMP label dbs'); {$EndIf}
                  LabelDataBases(Bitmap);
                  DrawLegendsOnMap(Bitmap);
               {$EndIf}
            end
            else begin
               {$If Defined( RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('tMapDraw.DrawMapOnBMP No Overlays'); {$EndIf}
            end;
         {$EndIf}
        Cleanup:;
         {$If Defined( RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('tMapDraw.DrawMapOnBMP past cleanup'); {$EndIf}
      finally
         {$IfDef RecordFullMapDraw} WriteLineToDebugFile('tMapDraw.DrawMapOnBMP exit pentultimate finally block'); {$EndIf}
      end;

      if (FullMapfName = '') then FullMapfName := NextFileNumber(MDTempDir, 'Full_map_', OverlayFExt);
      {$If Defined( RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('tMapDraw.DrawMapOnBMP saving  ' + FullMapfName); {$EndIf}
      Bitmap.SaveToFile(FullMapfName);
   finally
      if WantShowProgress and ShowSatProgress and (MapType <> mtDEMBlank) then EndProgress;
      {$If Defined(RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('tMapDraw.DrawMapOnBMP exit last finally block'); {$EndIf}
      FirstMapDrawing := false;
   end;

   {$If Defined(RecordMapDraw) or Defined(RecordFullMapDraw) or Defined(RecordTiming) or Defined(RecordUTMZone) or Defined(RecordElevationScaling)}
      WriteLineToDebugFile('tMapDraw.DrawMapOnBMP out: ' + MapSizeString + MapZRangeString);
   {$EndIf}
   {$If Defined(RecordLong0)} PrimMapProj.ProjectionParamsToDebugFile('tMapDraw.DrawMapOnBMP out, MapDraw.PrimaryMapDatum'); {$EndIf}

   {$If Defined(RecordPlatCarree)}
      if (BasicProjection = bpLatLong) then begin
         WriteLineToDebugFile('tMapDraw.DrawMapOnBMP out, UL=' + RealToString(MapCorners.BoundBoxProj.xmin,-12,-6) + '/' + RealToString(MapCorners.BoundBoxProj.ymax,-12,-6) +
               '  deltas=' + RealToString(MapCorners.Projdx,-12,-6) +  '/' + RealToString(MapCorners.Projdy,-12,-6) + '  ' + MapSizeString);
      end;
   {$EndIf}

end;


procedure DrawOverlayNoDelete(var Bitmap,Overlay : tMyBitmap; Opacity : byte = 100; StartX : SmallInt = 0; StartY : SmallInt = 0; TextMode : boolean = false);
var
   x,y,xe,ye,xs,ys : integer;
   {$IfDef VCL}
      p0,p1 : pRGB;
   {$EndIf}
   {$IfDef FMX}
      Region : tRectF;
   {$EndIf}
begin
   if (Overlay <> Nil) then begin
      {$IfDef RecordOverlays}
         Overlay.SaveToFile(MDTempDir + 'DrawOverlayNoDelete_overlay' + OverlayFExt);
         Bitmap.SaveToFile(MDTempDir + 'DrawOverlayNoDelete_bitmap' + OverlayFExt);
      {$EndIf}

      {$IfDef FMX}
          Bitmap.Canvas.BeginScene;
          Region.Create(0,0,pred(Bitmap.Width),pred(Bitmap.Height));
          Bitmap.Canvas.DrawBitmap(Overlay,region,region,0.01 * Opacity);
          Bitmap.Canvas.EndScene;
      {$EndIf}

      {$IfDef VCL}
         if (StartX = 0) and (StartY = 0) and (Bitmap.Height = Overlay.Height) and (Bitmap.Width = Overlay.Width) then begin
            {$IfDef RecordIHSmerges} WriteLineToDebugFile('IHS merge in DrawAndDeleteOverlay'); {$EndIf}
            if TextMode then begin   //this has problems with the legends and scalebars, where white disappears
               Bitmap.Canvas.CopyMode := cmSrcAnd;
               Bitmap.Canvas.Draw(0,0,Overlay);
            end
            else begin
               BlendBitmapAtop(Bitmap,Overlay,Opacity * 0.01);
            end;
         end
         else begin
            xs := 0;
            while xs + StartX < 0 do inc(xs);
            ys := 0;
            while ys + StartY < 0 do inc(ys);
            ye := pred(Overlay.height);
            while StartY + ye > pred(Bitmap.Height) do dec(ye);
            xe := pred(Overlay.Width);
            while StartX + xe > pred(Bitmap.Width) do dec(xe);
            for y := ys to ye do begin
               p0 := Bitmap.ScanLine[StartY + y];
               p1 := Overlay.ScanLine[y];
               for x := xs to xe do begin
                  if not SameColor(p1[x],rgbTripleWhite) then p0[StartX + x] := p1[x];
               end;
            end;
         end;
      {$EndIf}
   end;
end;


procedure DrawAndDeleteOverlay(var Bitmap,Overlay : tMyBitmap; Opacity : byte = 100; StartX : SmallInt = 0; StartY : SmallInt = 0; TextMode : boolean = false);
begin
   DrawOverlayNoDelete(Bitmap,Overlay,Opacity,StartX,StartY,TextMode);
   Overlay.Free;
   Overlay := Nil;
end;


function tMapDraw.KMLcompatibleMap : boolean;
begin
   Result := (DEMMap and (DEMGlb[DEMonMap].DEMheader.DEMUsed <> UTMbasedDEM)) or ((VectorIndex <> 0) and (PrimMapProj.Pname in [MercatorEllipsoid]))
{$IfDef ExSat}
{$Else}
      or ((SATonMap > 0) and (SatImage[SATonMap].ImageMapProjection.PName = PlateCaree));
{$EndIf}
end;


procedure tMapDraw.BoxToContainFan(Lat,Long,Range : float64; var uMinLat,uMinLong,uMaxLat,uMaxLong : float64);
var
   uLat,ULong : float64;
begin
   VincentyPointAtDistanceBearing(Lat,Long,Range+MDDef.BlowUpExtraMargin,0,uMaxLat,uLong);
   VincentyPointAtDistanceBearing(Lat,Long,Range+MDDef.BlowUpExtraMargin,90,uLat,uMaxLong);
   VincentyPointAtDistanceBearing(Lat,Long,Range+MDDef.BlowUpExtraMargin,180,uMinLat,uLong);
   VincentyPointAtDistanceBearing(Lat,Long,Range+MDDef.BlowUpExtraMargin,270,uLat,uMinLong);
end;


Procedure tMapDraw.AssignSecondDEM(DEM : integer; Min : float64 = 99; Max : float64 = -99);
begin
   if (DEM = 0) then begin
       if (NumDEMDataSetsOpen = 2) and (DEMGlb[1] <> Nil) and (DEMGlb[2] <> Nil) then begin
         if (DEMonMap = 1) then DEM := 2 else DEM := 1;
      end
      else begin
         {$IfDef VCL} GetDEM(DEM,true,'second DEM on map'); {$EndIf}
      end;
   end;

   if (DEM <> 0 ) and (DEMGlb[DEM] <> Nil) then begin
      DEM2onMap := DEM;
      if (Min > Max) then begin
         if false {MDDef.Full2dDEMRange} then begin
            MinZColorDEM2 := DEMGlb[DEM].DEMheader.MinElev;
            MaxZColorDEM2 := DEMGlb[DEM].DEMheader.MaxElev;
         end
         else begin
            MinZColorDEM2 := DEMGlb[DEM].FindPercentileElevation(MDDef.MinElevPercentile);
            MaxZColorDEM2 := DEMGlb[DEM].FindPercentileElevation(MDDef.MaxElevPercentile);
         end;
      end
      else begin
         MinZColorDEM2 := Min;
         MaxZColorDEM2 := Max;
      end;
      MinZSizeDEM2 := DEMGlb[DEM].DEMheader.MinElev;
      MaxZSizeDEM2 := DEMGlb[DEM].DEMheader.MaxElev;
   end
   else DEM2onMap := 0;
end;



function TMapDraw.FullDEMMap : boolean;
begin
   Result := (MapAreaDEMGridLimits.XGridLow = DEMglb[DEMonMap].FullDEMGridLimits.XGridLow) and
             (MapAreaDEMGridLimits.XGridHigh = DEMglb[DEMonMap].FullDEMGridLimits.XGridHigh) and
             (MapAreaDEMGridLimits.YGridLow = DEMglb[DEMonMap].FullDEMGridLimits.YGridLow) and
             (MapAreaDEMGridLimits.YGridHigh = DEMglb[DEMonMap].FullDEMGridLimits.YGridHigh);
end;


function TMapDraw.MapAreaDEMGridLimits : tGridLimits;
var
   xlow,ylow,xHigh,yHigh : float32;
begin
  MapGridToDEMGrid(MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymin,xlow,ylow);
  MapGridToDEMGrid(MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymax,xHigh,yHigh);
  Result.XGridLow := round(xLow);
  Result.XGridHigh := round(xHigh);
  Result.YGridLow := round(yLow);
  Result.YGridHigh := round(yHigh);
end;


procedure tMapDraw.CopyMapParams(var NewMapDraw : tMapDraw; CopyOverlays : boolean);
var
   i : integer;
begin
   if (NewMapDraw = Nil) then begin
      NewMapDraw := tMapDraw.Create;
      NewMapDraw.BaseTitle := 'Copied map';
   end;
   NewMapDraw.PrimMapProj := PrimMapProj;
   NewMapDraw.MapXSize := MapXSize;
   NewMapDraw.MapYSize := MapYSize;
   NewMapDraw.ScreenPixelSize := ScreenPixelSize;

   NewMapDraw.MapCorners := MapCorners;
   NewMapDraw.DEMonMap := DEMonMap;

   {$IfDef ExSat}
   {$Else}
     NewMapDraw.SATonMap := SATonMap;
     NewMapDraw.SATView := SatView;
   {$EndIf}

   NewMapDraw.MapType := MapType;
   NewMapDraw.MapMerge := MapMerge;
   NewMapDraw.TerrainShadowsDEM := TerrainShadowsDEM;
   NewMapDraw.PrimMapProj := PrimMapProj;
   NewMapDraw.SecondaryMapProj := SecondaryMapProj;
   NewMapDraw.MinMapElev := MinMapElev;
   NewMapDraw.MaxMapElev := MaxMapElev;
   NewMapDraw.DEMShadeImage := DEMShadeImage;

   if CopyOverlays then begin
      for i := MaxOverlays downto 1 do NewMapDraw.OverLayOrder[i] := OverlayOrder[i];
      NewMapDraw.CartoGroupShapesUp := CartoGroupShapesUp;
      for i := 1 to MaxDataBase do NewMapDraw.DBonThisMap[i] := DBonThisMap[i];
      {$IfDef RecordDrape} WriteLineToDebugFile('Take care of MapOverlays'); {$EndIf}
      CopyStringList(MapOverlays.ovVectorFiles,NewMapDraw.MapOverlays.ovVectorFiles);
      CopyStringList(MapOverlays.ovTerrainCat,NewMapDraw.MapOverlays.ovTerrainCat);
      NewMapDraw.MapOverlays.ConInt := MapOverlays.ConInt;
   end;

   NewMapDraw.LatTickInt := LatTickInt;
   NewMapDraw.UTMTickInt := UTMTickInt;
   NewMapDraw.PrimMapProj := PrimMapProj;

   NewMapDraw.FindMapUTMLimits;
end;


function TMapDraw.MapZoomFactor : float64;
var
   xfac,yfac : float64;
begin
   xfac := MapXSize / succ(round(MapCorners.BoundBoxDataGrid.xmax- MapCorners.BoundBoxDataGrid.xmin));
   yfac := MapYSize / succ(round(MapCorners.BoundBoxDataGrid.ymax - MapCorners.BoundBoxDataGrid.ymin));
   Result := Min(xfac,yfac);
end;


function TMapDraw.MapSizeString : shortstring;
begin
    Result := ' Map size=' + MapXSize.ToString + 'x' + MapYSize.ToString +  '  Pixel size=' + SmartDistanceMetersFormat(ScreenPixelSize);
end;


function TMapDraw.MGRSValid : boolean;
begin
   Result := false;
   if (ScreenPixelSize > 2500) then exit;
   if (VectorIndex <> 0) then begin
      Result := ZoomableVectorMap;
   end
   else if DEMMap then begin
      if ValidDEMonMap then begin
         if DEMGlb[DEMonMap].DEMheader.DigitizeDatum in [Spherical{,unusedLamAzEqAreaSphere}] then exit;
         Result := true;
      end;
   end
   else begin
      Result := ScreenPixelSize < 500;
   end;
end;


procedure TMapDraw.ScaleMapElevationsToDEM;
begin
   {$IfDef RecordElevationScaling} WriteLineToDebugFile(MapColorRange('TMapDraw.ScaleMapElevationsToDEM in')); {$EndIf}
   if ValidDEMonMap then begin
      MinMapElev := DEMGlb[DEMonMap].DEMheader.MinElev;
      MaxMapElev := DEMGlb[DEMonMap].DEMheader.MaxElev;
      if UsePercentiles or (DEMGlb[DEMonMap].DEMheader.ElevUnits in [Nanotesla]) then begin
         MinMapElev := DEMGLB[DEMonMap].FindPercentileElevation(MDDef.MinElevPercentile);
         MaxMapElev := DEMGLB[DEMonMap].FindPercentileElevation(MDDef.MaxElevPercentile);
         {$IfDef RecordElevationScaling} WriteLineToDebugFile('TMapDraw.ScaleMapElevationsToDEM percentiles' + MapZRangeString); {$EndIf}
      end
      else if (DEMGlb[DEMonMap].DEMheader.ElevUnits in [euImagery]) then begin
         MinMapElev := DEMGLB[DEMonMap].FindPercentileElevation(MDDef.MinImagePercentile);
         MaxMapElev := DEMGLB[DEMonMap].FindPercentileElevation(MDDef.MaxImagePercentile);
         {$IfDef RecordElevationScaling} WriteLineToDebugFile('TMapDraw.ScaleMapElevationsToDEM imagery percentiles' + MapZRangeString); {$EndIf}
      end
      else begin
          if Log10Elev then begin
             if DEMGlb[DEMonMap].DEMheader.MinElev <= 0 then MinMapElev := PetMath.Log10(0.01)
             else MinMapElev := PetMath.Log10(DEMGlb[DEMonMap].DEMheader.MinElev);
             MaxMapElev := PetMath.Log10(DEMGlb[DEMonMap].DEMheader.MaxElev);
          end
          else begin
             MinMapElev := DEMGlb[DEMonMap].DEMheader.MinElev;
             MaxMapElev := DEMGlb[DEMonMap].DEMheader.MaxElev;
          end;
       end;
   end;
   {$IfDef RecordElevationScaling} WriteLineToDebugFile('TMapDraw.ScaleMapElevationsToDEM out' + MapZRangeString); {$EndIf}
end;


function TMapDraw.MapColorRange(What : shortstring) : shortstring;
begin
   Result := What + '  min=' + RealToString(MinMapElev,-12,-2) + '  max=' + RealToString(MaxMapElev,-12,-2);
end;


procedure tMapDraw.ZeroOverlays;
var
   i : integer;
begin
   for i := 1 to MaxOverlays do OverLayOrder[i] := ovoUnused;
end;


procedure tMapDraw.MapOverlaysToDebugFile(Where : shortstring);
{$IfDef RecordProblems}
var
   I : integer;
begin
   WriteLineToDebugFile(Where);
   for i := MaxOverlays downto 1 do begin
      if (OverLayOrder[i] <> ovoUnused) then WriteLineToDebugFile('  Map layer: ' + LayerName[OverLayOrder[i]]);
   end;
{$Else}
begin
{$EndIf}
end;

   procedure tMapDraw.ZeroTickInt;
   begin
      LatTickInt := 0;
      UTMTickInt := 0;
      NativeTickInt := 0;
   end;



procedure tMapDraw.ResetMarginalia;
begin
   if (VectorIndex <> 0) then exit;
   NoMapGrids := (MDdef.MapTicks = tixNone);
end;


{$IfDef ExPointCloud}
{$Else}

   procedure tMapDraw.PlotALasFile(Cloud : integer; fName : PathStr; var BMPMemory : tBMPMemory);
   var
      LasData : Las_Lidar.tLAS_data;
   begin
      LasData := Las_Lidar.tLAS_data.Create(fName);
      LasData.PlotTileOnMap(Cloud,self,BMPMemory,LasMinAreaZ,LasMaxAreaZ);
      LasData.Destroy;
   end;


   procedure tMapDraw.BackgroundPlotLASFilesOnMap(var inBitmap : tMyBitmap);
   var
      fName : PathStr;
      bitmap,cbmp : tMyBitmap;
      LayerName : PathStr;
      Cloud : integer;
      BMPMemory : tBMPMemory;

            procedure PlotNoParallel;
            var
               i : integer;
            begin
               for i := 0 to pred(pt_cloud_opts_fm.LasFiles[Cloud].LAS_fnames.Count) do begin
                   fName := pt_cloud_opts_fm.LasFiles[Cloud].LAS_fnames.Strings[i];
                   {$IfDef RecordLAS} WriteLineToDebugFile('PlotNoParallel call LAS plot ' + ExtractFileName(fName)); {$EndIf}
                   PlotALasFile(Cloud,fName,BMPMemory);
                   if WantOut then break;
               end;
            end;

   begin
      {$If Defined(RecordOverlays) or Defined(RecordLAS)} WriteLineToDebugFile('tMapDraw.BackgroundPlotLASFilesOnMap in display=' + IntToStr(ord(MDDef.ls.ColorCoding))); {$EndIf}
      CreateBitmap(cBMP,inBitmap.width,inBitmap.Height);
      for Cloud := MaxClouds downto 1 do if (pt_cloud_opts_fm.LasFiles[Cloud] <> Nil) and pt_cloud_opts_fm.UsePC[Cloud] then begin
         if (MDDef.ls.ColorCoding = lasccIntensity) and (not pt_cloud_opts_fm.LasFiles[Cloud].HasIntensity) then begin

         end
         else begin
            if (MDDef.ls.ColorCoding = lasccIntensity) then begin
               MDDef.MaxIntensity := pt_cloud_opts_fm.LasFiles[Cloud].INTEN_99;
               MDDef.MinIntensity := pt_cloud_opts_fm.LasFiles[Cloud].INTEN_1;
            end;
            if MDDef.LasAutoThin then begin
               MDDef.CloudMapThinFactor := trunc(pt_cloud_opts_fm.LasFiles[Cloud].PointDensity * ScreenPixelSize / 2);
               if (MDDef.CloudMapThinFactor <= 0) then MDDef.CloudMapThinFactor := 1;
               if (MDDef.CloudMapThinFactor >500) then MDDef.CloudMapThinFactor := 500;
               if (pt_cloud_opts_fm <> Nil) then begin
                  pt_cloud_opts_fm.Edit3.Text := IntToStr(MDDef.CloudMapThinFactor);
               end;
            end
            else begin
               MDDef.CloudMapThinFactor := MDDef.LasThinFactor;
            end;
            {$If Defined(RecordOverlays) or Defined(RecordLAS)} WriteLineToDebugFile('Thin Factor =' + IntToStr(MDDef.CloudMapThinFactor)); {$EndIf}
             LayerName := MDTempDir + 'cloud_' + IntToStr(Cloud) + '_mode_' + IntToStr(ord(MDDef.ls.ColorCoding)) + '.bmp';
             if FileExists(LayerName) then begin
                {$If Defined(RecordOverlays) or Defined(RecordLAS)} WriteLineToDebugFile('Found ' + LayerName); {$EndIf}
                Bitmap := PetImage.LoadBitmapFromFile(LayerName);
             end
             else begin
                CreateBitmap(Bitmap,inBitmap.width,inBitmap.Height);
                BMPMemory := tBMPMemory.Create(bitmap);
                ShowSatProgress := false;
                StartProgressAbortOption('Draw ' + pt_cloud_opts_fm.LASfiles[Cloud].CloudName);
                ThreadsNumDone := 0;
                ThreadsToDo := pt_cloud_opts_fm.LasFiles[Cloud].TotalCloudPts;

                 if (pt_cloud_opts_fm.LasFiles[Cloud].LAS_fnames.Count = 1) then begin
                    PlotNoParallel;
                 end
                 else begin
                     {$If Defined(NoParallelFor) or Defined(NoParallelLAS)}
                        PlotNoParallel;
                    {$Else}
                       BaseStart := 0;
                       ThreadsWorking := true;
                       while (BaseStart < pt_cloud_opts_fm.LasFiles[Cloud].LAS_fnames.Count) do begin
                         TParallel.For(1,MDdef.MaxThreadsForPC,
                               procedure (Value: Integer)
                               begin
                                  if ((BaseStart + Value) <= pt_cloud_opts_fm.LasFiles[Cloud].LAS_fnames.Count) then begin
                                     fName := pt_cloud_opts_fm.LasFiles[Cloud].LAS_fnames.Strings[pred(BaseStart + Value)];
                                     PlotALasFile(Cloud,fName,BMPMemory);
                                  end;
                              end);
                              BaseStart := BaseStart + MDdef.MaxThreadsForPC;
                       end;
                       ThreadsWorking := false;
                    {$EndIf}
                 end;
                EndProgress;
                BMPMemory.Destroy;
                {$If Defined(RecordOverlays) or Defined(RecordLAS)} WriteLineToDebugFile('Saving ' + LayerName); {$EndIf}
                Bitmap.SaveToFile(LayerName);
             end;
             cBmp.Canvas.CopyMode := cmSrcAnd;
             cBmp.Canvas.Draw(0,0,Bitmap);
             Bitmap.Free;
         end;
      end;

      {$If Defined(RecordOverlays) or Defined(RecordLAS)} WriteLineToDebugFile('tMapDraw.BackgroundPlotLASFilesOnMap mid'); {$EndIf}

       if MDDef.SlicerIHSMerge then begin
          DrawAndDeleteOverlay(inBitmap,cBMP,MDDef.LasOpacity);
       end
       else begin
          inBitmap.Canvas.Draw(0,0,cBMP);
          cBMP.Free;
        end;

      {$If Defined(RecordOverlays) or Defined(RecordLAS)} WriteLineToDebugFile('tMapDraw.BackgroundPlotLASFilesOnMap end'); {$EndIf}
   end;
{$EndIf}


procedure tMapDraw.AdjustProjectionForUTMZone(Why : shortstring; PrimaryDatum: shortstring;  UTMZone : byte; LatHemi : ANSIchar);
begin
   {$If Defined(RecordShortDefineDatum) or Defined(ShowUTMZone)} WriteLineToDebugFile('tMapDraw.DefineDatums ' + Why + '  datums=' + PrimaryDatum  + '  UTM zone=' + IntToStr(UTMzone) + LatHemi); {$EndIf}
   {$If Defined(RecordLong0)} PrimMapProj.ShortProjInfo('tMapDraw.AdjustProjectionForUTMZone in'); {$EndIf}
   PrimMapProj.LatHemi := LatHemi;
   PrimMapProj.DefineDatumFromUTMZone(PrimaryDatum,UTMZone,LatHemi,Why + ' tMapDraw.DefineDatums primary');
   {$If Defined(RecordShortDefineDatum) or Defined(ShowUTMZone)} WriteLineToDebugFile('tMapDraw.DefineDatums primary: ' + PrimMapProj.KeyDatumParams); {$EndIf}
   {$If Defined(RecordLong0)} PrimMapProj.ShortProjInfo('tMapDraw.AdjustProjectionForUTMZone out'); {$EndIf}
end;


procedure tMapDraw.AdjustProjectionForUTMZone(Why : shortstring; PrimaryDatum : shortstring;  Lat,Long : float64);
var
   LatHemi : ANSIchar;
   UTMZone : byte;
begin
   {$IfDef RecordShortDefineDatum} WriteLineToDebugFile('tMapDraw.DefineDatums ' + Why + '  datums=' + PrimaryDatum ); {$EndIf}
   if Lat > 0 then LatHemi := 'N' else LatHemi := 'S';
   UTMZone := GetUTMZone(Long);
   AdjustProjectionForUTMZone(Why,PrimaryDatum,UTMZone,LatHemi);
end;


constructor tMapDraw.Create;
var
   i : integer;
begin
   {$IfDef RecordMapDrawCreation} WriteLineToDebugFile('tMapDraw.Create enter'); {$EndIf}
   inherited;
   MapDrawValid := false;
   ZeroOverlays;

   {$IfDef ExWMS}
   {$Else}
      LastWMSService := '';
      WMSLayerfName := '';
      WMSOverlayOnMap := false;
   {$EndIf}

   InitialMapDrawn := false;
   BaseTitle := 'Default map';
   FullMapfName := '';
   RangeCirclesFName := '';
   AllFansCoverageFName := '';
   TigerOverlayFName := '';
   CartoDBfName := '';
   ContourOverlayfName  := '';
   ContourOverlayfName2 := '';
   BaseMapFName := '';
   OSMShapesUp := '';
   OSMOverlayfName := '';
   PLSSOverlayfName := '';
   GazOverlayFName := '';
   NaturalEarthOverlayFName := '';
   GridOverlayFName := '';
   VectorOverlayFName := '';
   SecondGridfName := '';
   SatTrainingFName := '';
   LegendOverlayfName := '';
   USOverlayfName := '';
   VegDensityLayerInUse := 0;
   MapGazDB := 0;

   for i := 1 to MaxDataBase do DBOverlayfName[i] := '';
   for i := 1 to MaxDataBase do DBonThisMap[i] := false;

   CurrentFansTable := 0;
   FeatureGrid := 0;
   LasClassGrid := 0;
   CHMgrid := 0;
   ChangeGrid := 0;

   Long0_360 := false;

   FloodLayers := Nil;
   NeedToRedraw := false;
   FastMapDraw := false;
   UsePercentiles := MDDef.DefElevsPercentile;
   DrawLegendsThisMap := true;
   DrawScaleBarThisMap := true;
   TigerSingleOverlayUp := false;
   Log10Elev := false;
   LASlayerOnMap := false;

   NoDrawingNow := false;
   AllowDataBaseDrawing := true;
   DrawMarginalia := true;
   LockZColors := false;
   MakeMapGrayscale := false;
   SubdueBase := false;
   SubdueGrids := false;
   GrayscaleSubdueOverlays := false;
   SubdueWorldOutline := false;
   GrayscaleWorldOutline := false;
   SubdueOSM := false;
   GrayscaleOSM := false;

   DEMGridRedraw := false;
   DEMGridSym := Box;
   DEMGridSymSize := 3;
   DEMGridSymColor := claRed;
   NoMapGrids := false;
   AllTigerOK := false;
   DrawFansThisTime := true;
   ShowMapLibraryCoverage := false;
   DEMShadeImage := false;
   aDRGMap := false;
   ElevStretch := esNone;
   MapXSize := MDdef.DefaultMapXSize;
   MapYSize := MDdef.DefaultMapYSize;
   CurrentZoomLevel := 100;
   VectorIndex := 0;
   TerrainShadowsDEM := 0;
   FirstMapDrawing := true;
   MapType := mtNone;
   MapMerge := mmNone;
   MapOwner := moNone;
   RedrawDrainageVectors := false;
   ZeroTickInt;
   MapLatCent := -999;
   MapLongCent := -999;
   GridTrueAngle := MaxSmallInt;

   SingleContourColor := false;
   SectorOutlines := '';
   CartoGroupShapesUp := '';
   MultiFanMasks := Nil;
   DEMonMap := 0;
   DEM2onMap := 0;
   SatOnMap := 0;

   MultiGridOnMap := 0;
   MonthlyDBArrayOnMap := 0;

   MinMapElev := 0;
   MaxMapElev := 0;

   MapOverlays.ovShapeFileGroup := '';
   MapOverlays.ovTerrainCat  := tStringList.Create;
   MapOverlays.ovVectorFiles := tStringList.Create;
   MapOverlays.ConInt := MDdef.DefaultContourInterval;

   {$IfDef ExSat}
   {$Else}
      InitializeSatView(SatView);
   {$EndIf}

   {$IfDef RecordMapDrawCreation} WriteLineToDebugFile('tMapDraw.Create out'); {$EndIf}
end;


destructor tMapDraw.Destroy;
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('tMapDraw.Destroy in'); {$EndIf}
   if (MapOverlays.ovTerrainCat <> Nil) then begin
      MapOverlays.ovVectorFiles.Free;
      MapOverlays.ovTerrainCat.Free;
      MapOverlays.ovTerrainCat := Nil;
   end;
   DeleteMapSavedLayers;
   {$IfDef RecordClosing} WriteLineToDebugFile('DeleteMapSavedLayers OK'); {$EndIf}
   if (FloodLayers <> Nil) then FloodLayers.Free;

   if (VectorIndex <> 0) then begin
      {$IfDef RecordClosing} WriteLineToDebugFile('Projection, ' + PrimMapProj.ProjDebugName); {$EndIf}
      try
         PrimMapProj.Destroy;
      finally
         PrimMapProj := nil;
      end;
   end;
   if SecondaryMapProj <> Nil then try
      SecondaryMapProj.Destroy;
   finally
      SecondaryMapProj := nil;
   end;

   {$IfDef RecordClosing} WriteLineToDebugFile('tMapDraw.Destroy out'); {$EndIf}
end;


function tMapDraw.IsThisMapUTM : boolean;
begin
   Result := false;
   {$IfDef ExSat}
   {$Else}
      if ValidSatOnMap then begin
         Result := ( (SatImage[SatOnMap].RegVars.Registration = RegProjection) and (SatImage[SatOnMap].ImageMapProjection.pName = UTMEllipsoidal) );
      end;
   {$EndIf}
   if (VectorIndex <> 0) then Result := (PrimMapProj.PName in [UTMEllipsoidal]);
   if DEMMap then Result := (DEMGlb[DEMonMap].DEMheader.DEMUsed = UTMbasedDEM);
end;


function tMapDraw.ValidDEMonMap: boolean;
begin
   Result := (DEMonMap <> 0) and (DEMGlb[DEMonMap] <> Nil);
end;


function tMapDraw.ValidSatonMap: boolean;
begin
   {$IfDef ExSat}
      Result := false;
   {$Else}
      Result := ValidSatImage(SatOnMap);
   {$EndIf}
end;


function TMapDraw.ScreenToElev(xpic,ypic : integer; var z : float32) : boolean;
var
   xg,yg : float32;
begin
   if ValidDEMonMap then begin
      ScreenToDataGrid(xpic,ypic,xg,yg);
      Result := GetMapElev(xg,yg,Z);
   end
   else begin
      Result := false;
      z := MaxSmallInt;
   end;
end;


function TMapDraw.GetMapElev(xgrid,ygrid : float64; var Z : float32) : boolean;
var
   Lat,Long : float64;
begin
   Result := false;
   if (VectorIndex <> 0) then exit;
   {$IfDef ExSat}
   {$Else}
   if ValidSatImage(SatOnMap) then begin
      SatImage[SatOnMap].SatGridToLatLongDegree(SatImage[SatOnMap].BandForSize,XGrid,YGrid,Lat,Long);
      DEMGlb[DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,XGrid,YGrid);
   end;
   {$EndIf}
   Result := DEMGlb[DEMonMap].GetElevMeters(XGrid,YGrid,z);
end;


function TMapDraw.OnScreen(x,y : integer) : boolean;
begin
   Result := (x >= 0) and (y >= 0) and (x <= pred(MapXSize)) and (y <= pred(MapYSize));
end;


function TMapDraw.NearScreen(x,y : integer) : boolean;
const
   ExtraMargin = 25;
begin
   Result := (x >=0-ExtraMargin) and (y >= -ExtraMargin) and (x < MapXSize+ExtraMargin) and (y < MapYSize+ExtraMargin);
end;


function tMapDraw.DEMMap : boolean;
begin
   Result := ValidDEM(DEMonMap) and (isElevationMap(MapType) or isReflectanceMap(MapType) or isSlopeMap(MapType) or IsOtherGridMap(MapType));
end;


function tMapDraw.ValidDEMMap : boolean;
begin
   Result := DEMMap and ValidDEM(DEMonMap);
end;



procedure TMapDraw.SaveLayerBitmap(Bitmap : tMyBitmap; var FName : PathStr);
begin
   fName := NextFileNumber(MDTempDir, 'Map_layer', OverlayFExt);
   {$IfDef Defined(RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('TMapDraw.SaveLayerBitmap, fname=' + FName); {$EndIf}
   PetImage.SaveBitmap(Bitmap,fName);
   {$IfDef Defined(RecordFullMapDraw) or Defined(RecordOverlays)} WriteLineToDebugFile('  save OK'); {$EndIf}
end;


procedure TMapDraw.DeleteSingleMapLayer(var fName : PathStr);
begin
    if (FName <> '') then begin
       DeleteFileIfExists(FName);
       FName := '';
    end;
end;


procedure TMapDraw.DrawThreadCheckPlane(Bitmap : tBitmap; xglo,yglo,xghi,yghi : integer;  xg1,yg1,Dip,Strike,DipDir  : float64);
begin
   MessageToContinue('Temporarily disabled');
(*
var
   XUTM1,XUTM2,YUTM1,YUTM2,NTol,STol,ETol,WTol,Elv1,
   ContactXC,ContactYC,ContactZC,ContactConstant : float64;
   Normal1   : VectorType;

      procedure IntersectionThreePlanes(var XC1,YC1,ZC1,Con1,XC2,YC2,ZC2,Con2,XC3,YC3,ZC3,Con3 : float64;  var XInt,YInt,ZInt : float64);
      var
         Det : float64;

         Function ThByTh(a1,b1,c1,a2,b2,c2,a3,b3,c3 : float64) : float64;   inline;
         begin
            ThByTh := a1*b2*c3 - a1*b3*c2 - a2*b1*c3+ a2*b3*c1 - a3*b2*c1 + a3*b1*c2;
         end;

      begin {proc IntersectionThreePlanes}
         Det := ThByTh(XC1,YC1,ZC1,XC2,YC2,ZC2,XC3,YC3,ZC3);
         if abs(Det) > 0.001 then begin
            XInt := ThByTh(-Con1,YC1,ZC1,-Con2,YC2,ZC2,-Con3,YC3,ZC3) / Det;
            YInt := ThByTh(XC1,-Con1,ZC1,XC2,-Con2,ZC2,XC3,-Con3,ZC3) / Det;
            ZInt := ThByTh(XC1,YC1,-Con1,XC2,YC2,-Con2,XC3,YC3,-Con3) / Det;
         end;
      end {proc IntersectionThreePlanes};


      procedure CheckGridSquare(XGrid,YGrid : LongInt);
      var
         x1,y1,x2,y2 : integer;
         NXInt,NYInt,EXInt,EYInt,
         SXInt,SYInt,WXInt,WYInt,ZInt  : float64;
         SidesFound                    : integer;
         ElvSq : tElevFloatArray;


         procedure CheckSide(XGr1,YGr1 : LongInt; Elv1 : float64; XGr2,YGr2 : LongInt;
            Elv2 : float64; var XIntSect,YIntSect,ZIntSect,Tol : float64);
         var
            SideXC,SideYC,SideZC,SideConstant,z1,z2,x3,y3,z3,
            IncXC,IncYC,IncZC,IncConstant : float64;
         begin
             if DEMGlb[DEMonMap].IsSurroundedPoint(xgr1,ygr1) then begin
                DEMGlb[DEMonMap].DEMGridToUTM(1.0*XGr1,1.0*YGr1,XUTM1,YUTM1);
                DEMGlb[DEMonMap].DEMGridToUTM(1.0*XGr2,1.0*YGr2,XUTM2,YUTM2);
                {for each side of grid square, find planes whose intersection will be edge}
                {1st inclined plane through side}
                z1 := Elv1;
                z2 := Elv2;
                z3 := 1234.0;
                x3 := XUTM2 + 1000.0;
                y3 := YUTM2 + 1000.0;
                PlaneEquationFromThreePoints(XUTM1,YUTM1,z1,XUTM2,YUTM2,z2,x3,y3,z3,SideXC,SideYC,SideZC,SideConstant);
                {2d inclined plane through side}
                x3 := XUTM2 - 500.0;
                y3 := YUTM2 - 500.0;
                z3 := 2234;
                PlaneEquationFromThreePoints(XUTM1,YUTM1,z1,XUTM2,YUTM2,z2,x3,y3,z3,IncXC,IncYC,IncZC,IncConstant);
                {intersect those two planes with contact plane}
                IntersectionThreePlanes(ContactXC,ContactYC,ContactZC,ContactConstant,SideXC,SideYC,SideZC,SideConstant,IncXC,IncYC,IncZC,IncConstant,XIntSect,YIntSect,ZIntSect);
                {check if intersection point lies on side}
                if ( ((XUTM1 <= XIntSect+0.05) and (XIntSect-0.05 <= XUTM2)) or ((XUTM1 >= XIntSect-0.05) and (XIntSect+0.05 >= XUTM2)) ) and
                  ( ((YUTM1 <= YIntSect+0.05) and (YIntSect-0.05 <= YUTM2)) or ((YUTM1 >= YIntSect-0.05) and (YIntSect+0.05 >= YUTM2)) ) and
                  ( ((Elv1 <= zIntSect+0.05) and (zIntSect-0.05 <= Elv2)) or ((Elv1 >= zIntSect-0.05) and (zIntSect+0.05 >= Elv2)) ) then begin
                       if SidesFound = 0 then UTMToScreen(XIntSect,YIntSect,x2,y2)
                       else UTMToScreen(XIntSect,YIntSect,x1,y1);
                       inc(SidesFound);
                 end;
             end;
         end {function};

      begin {proc CheckGridSquare}
          SidesFound := 0;
          DEMGlb[DEMonMap].GetElevSquareMeters(XGrid,YGrid,ElvSq);
          CheckSide(XGrid,YGrid,ElvSq[1],XGrid,succ(YGrid),ElvSq[4],WXInt,WYInt,ZInt,WTol);
          CheckSide(XGrid,YGrid,ElvSq[1],succ(XGrid),YGrid,ElvSq[2],SXInt,SYInt,ZInt,STol);
          CheckSide(succ(XGrid),succ(YGrid),ElvSq[3],XGrid,succ(YGrid),ElvSq[4],NXInt,NYInt,ZInt,NTol);
          CheckSide(succ(XGrid),succ(YGrid),ElvSq[3],succ(XGrid),YGrid,ElvSq[2],EXInt,EYInt,ZInt,ETol);
          if (SidesFound > 1) then begin
             PetImage.DrawLine(Bitmap,x1,y1,x2,y2);
          end {if};
      end {proc CheckGridSquare};

var
   xg,yg : LongInt;
begin
   {$IfDef RecordGeology} WriteLineToDebugFile('TMapDraw.DrawThreadCheckPlane, dip='+RealToString(Dip,-12,1) + '  strike=' + RealToString(Strike,-4,0) + 'dipdir=' + RealToString(DipDir,-4,0)); {$EndIf}

    DEMGlb[DEMonMap].DEMGridToUTM(xg1,yg1,xutm1,yutm1);
    DEMGlb[DEMonMap].GetElevMeters(xg1,yg1,Elv1);
    FindVectorNormalToPlane(DipDir,Dip,Normal1);
    PlaneEquationFromPointAndNormal(XUTM1,YUTM1,Elv1,Normal1,ContactXC,ContactYC,ContactZC,ContactConstant);

    StartProgress('Trace contact');

    {$IfDef VCL}
    Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.GeoContactColor);
    Bitmap.Canvas.Pen.Width := MDDef.GeoContactWidth;
    {$EndIf}

    {$IfDef FMX}
    Bitmap.Canvas.BeginScene;
    Bitmap.Canvas.Stroke.Color := MDDef.GeoContactColor;
    Bitmap.Canvas.Stroke.Thickness := MDDef.GeoContactWidth;
    {$EndIf}

    for xg := xglo to xghi do begin
       UpDateProgressBar((xg-xglo )/(xghi-xglo ));
       for yg := yglo to yghi do begin
          CheckGridSquare(xg,yg);
       end {for yg};
    end {for xg};

    EndProgress;

   {$IfDef FMX}
   Bitmap.Canvas.EndScene;
   {$EndIf}

   {$IfDef RecordGeology}  Bitmap.SaveToFile(MDTempDir + 'geology.png'); {$EndIf}
*)
end;



procedure TMapDraw.DeleteMapSavedLasLayers;
begin
   DeleteMultipleFiles(MDTempDir,'cloud_*.bmp');
end;


procedure TMapDraw.DeleteMapSavedLayers;
begin
    {$IfDef VCL}
       DeleteSingleMapLayer(BaseMapFName);
    {$EndIf}
   {$IfDef ExWMS}
   {$Else}
       DeleteSingleMapLayer(WMSLayerfName);
    {$EndIf}

    DeleteSingleMapLayer(FullMapfName);
    DeleteSingleMapLayer(AllFansCoverageFName);
    DeleteSingleMapLayer(TigerOverlayFName);
    DeleteSingleMapLayer(CartoDBfName);
    DeleteSingleMapLayer(ContourOverlayfName);
    DeleteSingleMapLayer(ContourOverlayfName2);
    DeleteSingleMapLayer(PLSSOverlayfName);
    DeleteSingleMapLayer(GazOverlayfName);
    DeleteSingleMapLayer(NaturalEarthOverlayFName);
    DeleteSingleMapLayer(GridOverlayFName);
    DeleteSingleMapLayer(SecondGridfName);
    DeleteSingleMapLayer(SatTrainingfName);
    DeleteSingleMapLayer(LegendOverlayfName);
    DeleteSingleMapLayer(USOverlayfName);
    DeleteSingleMapLayer(VectorOverlayfName);
    DeleteSingleMapLayer(OSMOverlayfName);
    DeleteDatabaseSavedLayers;
    DeleteMapSavedLasLayers;
   {$IfDef ExGIS}
   {$Else}
       DEMESRIshapefile.DeleteShapeFile(UTMgridFName);
       DEMESRIshapefile.DeleteShapeFile(GraticuleFName);
   {$EndIf}
end;

procedure TMapDraw.ClearGISLayer(i : integer);
begin
    DeleteSingleMapLayer(DBOverlayfName[i]);
end;

procedure TMapDraw.DeleteDatabaseSavedLayers;
var
   i : integer;
begin
   {$IfDef VCL}
      for i := 1 to MaxDataBase do if (GISDB[i] <> nil) then ClearGISLayer(i);
   {$EndIf}
end;


function TMapDraw.AFullWorldMap : boolean;
begin
   Result := ((VectorIndex <> 0) and (PrimMapProj.FullWorld)) or (DEMMap and (DEMGlb[DEMonMap].LongSizeMap > 300))
           {$IfDef ExSat}
           {$Else}
           or ((SatOnMap > 0) and (SatImage[SatOnMap].FullWorld))
           {$EndIf};
end;


function TheHue(MergeHue : float64; i : integer) : float64;
begin
   Result := (360.0 - (MergeHue/255) * ( i / 255 * 360.0));
end;


function TMapDraw.LatLongBoxOnScreen(BoundBoxToTest : sfBoundBox) : boolean;
var
  ProjBox : sfBoundBox;
begin
   Result := AtLeastPartOfBoxInAnotherBox(BoundBoxToTest,MapCorners.BoundBoxGeo);
   if (not Result) and (BasicProjection = bpOther) then begin
      LatLongDegreeToProjectedCoords(BoundBoxToTest.Ymin,BoundBoxToTest.Xmin,ProjBox.xmin,ProjBox.ymin);
      LatLongDegreeToProjectedCoords(BoundBoxToTest.Ymax,BoundBoxToTest.Xmax,ProjBox.xmax,ProjBox.ymax);
      Result := AtLeastPartOfBoxInAnotherBox(ProjBox,MapCorners.BoundBoxProj);
   end;
end;


function TMapDraw.LatLongOnScreen(Lat,Long : float64) : boolean;
begin
   Result := (Lat < MapCorners.BoundBoxGeo.ymax) and (Lat > MapCorners.BoundBoxGeo.ymin) and (Long < MapCorners.BoundBoxGeo.xmax) and (Long > MapCorners.BoundBoxGeo.xmin);
end;



function tMapDraw.UTMGridToTrueNorthAngle(Lat,Long : float64) : float64;
var
   xutm,yutm,Lat2,Long2,Distance : float64;
begin
   if PrimMapProj.Pname in [Platecaree,WebMercator,MercatorEllipsoid,UTMEllipsoidal] then begin
      PrimMapProj.LatLongDegreetoUTM(Lat,Long,xutm,yutm);
      PrimMapProj.UTMToLatLongDegree(xutm,yutm+1000,Lat2,Long2);
      VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Distance,Result);
      if (Result > 180) then Result := -(360 - Result);
   end;
end;



procedure ExpandRoute(DEM : integer; var Route : tStringList; PointSep : float64;  AddFirst,AddLast,AddTurns : boolean);

      procedure DoExpandRoute(var Bends,Route : tStringList; PointSep : float64);
      var
         CarryDistance,Lat,Long,Lat2,Long2,Azimuth,Distance : float64;
         j       : integer;
         DoneOne : boolean;

               procedure AddPoint(DistAdd : float64);
               var
                  Lat,Long : float64;
                  z : float32;
                  TStr : shortstring;
               begin
                 VincentyPointAtDistanceBearing(Lat2,Long2,DistAdd,Azimuth,Lat,Long);
                 if DoneOne or AddFirst then begin
                    TStr := '';
                    if (DEM <> 0) and DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z) then TStr := RealToString(z,-8,-2);

                    Route.Add(RealToString(Lat,-12,-8) + ',' + RealToString(Long,-14,-8) + ',' + TStr);
                   {$IfDef RecordRoute} WriteLineToDebugFile('Add point, dist=' + RealToString(DistAdd,-12,0) + '   ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
                 end;
                 DoneOne := true;
               end;

      begin
          {$IfDef RecordRoute} WriteLineToDebugFile('Arrived in expand route Original file  Requesting pt sep=' + RealToString(PointSep,-12,2));  WriteStringListToDebugFile(Bends); {$EndIf}

          ReadCoordsLatLongFromStreamProfileResults(Bends,1,Lat2,Long2);
          CarryDistance := 0;

          DoneOne := false;
          for j := 2 to pred(Bends.Count) do begin
              ReadCoordsLatLongFromStreamProfileResults(Bends,j,Lat,Long);
              VincentyCalculateDistanceBearing(Lat2,Long2,Lat,Long,Distance,Azimuth);
              {$IfDef RecordRoute} WriteLineToDebugFile('New segment ' + LatLongDegreeToString(Lat2,Long2);    {$EndIf}

              while (CarryDistance < Distance) do begin
                 AddPoint(CarryDistance);
                 CarryDistance := CarryDistance + PointSep;
              end;
              if AddTurns then AddPoint(Distance);

              CarryDistance := CarryDistance - Distance;
              if (j <> pred(Bends.Count)) then begin
                 Lat2 := Lat;
                 Long2 := Long;
              end;
          end;
          if AddLast and (CarryDistance > PointSep / 100 ) then AddPoint(Distance);

          {$IfDef RecordRoute}
             WriteLineToDebugFile('Done with expand route initial point count: ' + IntToStr((Bends.Count-1)) + '  final point count:   ' + IntToStr((Route.Count)));
             WriteLineToDebugFile('Set with length carry distance left=' + RealToString(CarryDistance,-12,-1));
             WriteStringListToDebugFile(Route);
          {$EndIf}
      end;

var
   NewRoute : tStringList;
   i : integer;
begin
   ShowHourglassCursor;
   NewRoute := tStringList.Create;
   NewRoute.Add('LAT,LONG,Z');
   DoExpandRoute(Route,NewRoute,PointSep);
   Route.Clear;
   For i := 0 to pred(NewRoute.Count) do Route.Add(NewRoute.Strings[i]);
   NewRoute.Free;
   ShowDefaultCursor;
end;


function TMapDraw.LoadExistingBaseMap(var SavedLayer : tMyBitmap; var fName : PathStr) : boolean;
begin
   Result := false;
   if (DEMonMap <> 0) and FullDEMMap then begin
      if FileExists(fName) then begin
         SavedLayer.LoadFromFile(fName);
         if (SavedLayer.Width = MapXSize) and (SavedLayer.Height = MapYSize) then Result := true
         else begin
            {$IfDef RecordTIGER} WriteLineToDebugFile('Saved bitmap ' + BitmapSize(SavedLayer) + '  ' + MapSizeString ); {$EndIf}
            SysUtils.DeleteFile(fName);
            SavedLayer.Width := MapXSize;
            SavedLayer.Height := MapYSize;
         end;
      end;
   end
end;



procedure TMapDraw.WriteMapsWorldFile(fName : PathStr);
var
  xPixelSize,yPixelSize,x1,x2,y1,y2 : float64;
begin
    {$IfDef FMX}
       FixFileNameBackslashes(fName);
    {$EndIf}
    if IsThisMapUTM then begin
       ScreenToUTM(0,0,x1,y1);
       ScreenToUTM(pred( MapXSize),pred(MapYSize),x2,y2);
    end
    else begin
       ScreenToLatLongDegree(0,0,y1,x1);
       ScreenToLatLongDegree(pred(MapXSize),pred(MapYSize),y2,x2);
    end;
    xPixelSize := (x2 - x1) / pred(MapXSize);
    yPixelSize := -(y2 - y1) / pred(MapYSize);
    SaveWorldFile(fName,xPixelSize,yPixelSize,x1,y1,PrimMapProj.h_DatumCode,PrimMapProj.projUTMZone,MDdef.DefaultLatHemi);
end;


procedure tMapDraw.StretchWorldFileMap(var StretchBitmap : tMyBitmap; fName : PathStr; Color : integer = -99);
var
   {$IfDef FMX}
      FromRegion,ToRegion : tRectF;
   {$EndIf}
   x1,y1,x2,y2,i : integer;
   Bitmap : tMyBitmap;
   KMLfile : tStringList;
   aLine : ANSIString;
   FoundGroundOverlay : boolean;
   DeltaX,Xrot,YRot,DeltaY,UpLeftX,UpLeftY,LowRightY,LowRightX : float64;

      procedure GetOverlayCorners;
      begin
         FindMapUTMLimits;    //unclear why this has to be called here, but it does
         if IsThisMapUTM then begin
            {$IfDef RecordStretchBitmap} WriteLineToDebugFile('UTM loadimage'); {$EndIf}
            UTMToScreen(Upleftx,UpleftY,x1,y1);
            UTMtoScreen(LowRightX,LowRightY,x2,y2);
         end
         else begin
            {$IfDef RecordStretchBitmap} WriteLineToDebugFile('Lat/long loadimage'); {$EndIf}
            if (LowRightX - UpLeftX) > 355 then begin
               if LowRightX > 180 then LowRightX := 179.875;
            end;
            {$IfDef RecordStretchBitmap} WriteLineToDebugFile('Adjusted Upper left:  ' + RealToString(UpLeftX,-12,-4) + 'x' + RealToString(UpLeftY,-12,-4) + ' Lower right: ' + RealToString(LowRightX,-12,-4) + 'x' + RealToString(LowRightY,-12,-4)); {$EndIf}
            LatLongDegreeToScreen(UpLeftY,UpLeftX,x1,y1);
            LatLongDegreeToScreen(LowRightY,LowRightX,x2,y2);
            if (abs(x2-x1) < 5) then x1 := 0;
         end;
      end;


      procedure LoadImage;
      begin
         {$IfDef RecordStretchBitmap} WriteLineToDebugFile('Load image in'); {$EndIf}
         GetOverlayCorners;
         {$IfDef RecordStretchBitmap} WriteLineToDebugFile('Stretch corner (UL): ' + IntToStr(x1) + 'x' + IntToStr(y1) + '  (LR): ' + IntToStr(x2) + 'x' + IntToStr(y2)); {$EndIf}

         {$IfDef VCL}
            StretchBitmap.Canvas.CopyMode := cmSrcCopy;
            Bitmap.Canvas.CopyMode := cmSrcCopy;
            Bitmap.TransparentMode := tmFixed;
            StretchBitmap.Canvas.StretchDraw(Rect(x1,y1,x2,y2),Bitmap);
            Bitmap.Free;
         {$EndIf}

         {$IfDef FMX}
            ToRegion.Create(x1,y1,x2,y2);
            FromRegion.Create(0,0,pred(Bitmap.Width),pred(Bitmap.Height));
            StretchBitmap.Canvas.BeginScene;
            StretchBitmap.Canvas.DrawBitmap(Bitmap,FromRegion,ToRegion,1);
            StretchBitmap.Canvas.EndScene;
            Bitmap.Free;
         {$EndIf}

         {$IfDef RecordStretchBitmap} WriteLineToDebugFile('Load image out'); {$EndIf}
      end;


       procedure CheckLine(Side : ShortString; var Location : float64; Load : boolean);
       begin
          if StrUtils.AnsiContainsText(aLine,Side) then begin
              while not (Copy(aline,1,Length(Side)) = Side) do Delete(aLine,1,1);
              Delete(aline,1,Length(Side));
              Location := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(aline,'<',true,true));
          end;
       end;


begin
   {$IfDef RecordStretchBitmap} WriteLineToDebugFile('enter tMapDraw.StretchWorldFileMap for ' + fName); {$EndIf}
   if FileExists(fName) then begin
      CreateBitmap(StretchBitmap,MapXSize,MapYSize);
      if FileExtEquals(fname,'.KML') then begin
         KMLfile := tStringList.Create;
         KMLfile.LoadFromFile(fName);
         FoundGroundOverlay := false;
         for I := 0 to pred(KMLfile.Count) do begin
             aline := KMLfile.Strings[i];
             if StrUtils.AnsiContainsText(aLine,'<GroundOverlay>') then FoundGroundOverlay := true;
             if FoundGroundOverlay and StrUtils.AnsiContainsText(aLine,'<href>') then begin
                 while not (Copy(aline,1,6) = '<href>') do Delete(aLine,1,1);
                 Delete(aline,1,6);
                 {$IfDef RecordStretchBitmap} WriteLineToDebugFile('found image ' + aLine); {$EndIf}
                 fName := ExtractFilePath(fName) + Petmar_types.BeforeSpecifiedCharacterANSI(aline,'<',true,true);
                 FixForwardSlashes(fName);
                 Bitmap := PetImage.LoadBitmapFromFile(fName);
                 PetImage.BitmapWhiteToNearWhite(Bitmap);
                 FoundGroundOverlay := false;
                 {$IfDef RecordStretchBitmap} WriteLineToDebugFile('loaded ' + fName +  BitmapSizeString(Bitmap)); {$EndIf}
                 {$IfDef RecordOverlays}  PetImage.SaveBitmap(Bitmap,MDTempDir + 'kml_overlay.bmp'); {$EndIf}
             end;
             CheckLine('<north>',UpLeftY,false);
             CheckLine('<south>',LowRightY,false);
             CheckLine('<east>',LowRightX,false);
             CheckLine('<west>',UpLeftX,true);
         end;
         KMLFile.Free;
         {$IfDef RecordStretchBitmap} WriteLineToDebugFile('KML done'); {$EndIf}
      end
      else begin
         {$IfDef RecordStretchBitmap} WriteLineToDebugFile('tMapDraw.StretchWorldFileMap,  Map size: ' + IntToStr(MapXSize) + 'x' + IntToStr(MapYSize)); {$EndIf}

         Bitmap := PetImage.LoadBitmapFromFile(fName);
         FindExistingWorldFile(fName);
         ReadWorldFileValues(fName,DeltaX,Xrot,YRot,DeltaY,UpLeftX,UpLeftY);
         LowRightX := UpLeftX + pred(Bitmap.Width) * DeltaX;
         LowRightY := UpLeftY + pred(Bitmap.Height) * DeltaY;

         {$IfDef RecordStretchBitmap} WriteLineToDebugFile('Upper left: ' + RealToString(UpLeftX,-12,-4) + 'x' + RealToString(UpLeftY,-12,-4) + ' Lower right: ' + RealToString(LowRightX,-12,-4) + 'x' + RealToString(LowRightY,-12,-4));      {$EndIf}

         GetOverlayCorners;
         if (not OnScreen(x1,y1)) and (not OnScreen(x2,y2)) and (not OnScreen(x1,y2)) and (not OnScreen(x2,y1)) then begin
            if ((x1 < 0) and (x2 > Bitmap.Width)) or  ((y1 < 0) and (y2 > Bitmap.Height)) then begin
               {$IfDef RecordStretchBitmap} WriteLineToDebugFile('Entire Map Inside the overlay'); {$EndIf}
            end
            else begin
               {$IfDef RecordStretchBitmap} WriteLineToDebugFile('Image offscreen'); {$EndIf}
               Bitmap.Free;
               exit;
            end;
         end;

         {$IfDef VCL}
            if (Color >= 0) then PetImage.RecolorBitmap(Bitmap,ConvertTColorToPlatformColor(Color));
         {$EndIf}
         {$IfDef RecordStretchBitmap} WriteLineToDebugFile('World file map' +  BitmapSizeString(Bitmap)); {$EndIf}
      end;
      LoadImage;
   end;
   {$IfDef RecordStretchBitmap} WriteLineToDebugFile('tMapDraw.StretchWorldFileMap out'); {$EndIf}
end;



function TMapDraw.ZoomableVectorMap : boolean;
begin
   Result := (PrimMapProj.PName in [UTMEllipsoidal,MercatorEllipsoid,LambertConformalConicEllipse,AlbersEqAreaConicalEllipsoid,LamAzEqAreaEllipsoidal,AzimuthalEquidistantEllipsoidal]);
end;

function TMapDraw.MapZRange : float64;
begin
   Result := MaxMapElev - MinMapElev;
end;


function TMapDraw.DistanceOffMap(Lat,Long : float64) : shortstring;
var
   South,North,West,East : boolean;

         procedure DoIt(Direction : ShortString; LatBox,LongBox : float64);
         var
            Dist,Az : float64;
         begin
            {$IfDef RecordOffMap} WriteLineToDebugFile('Direction off=' + Direction + '  measure to ' + LatLongDegreeToString(LatBox,LongBox));   {$EndIf}
            VincentyCalculateDistanceBearing(Lat,Long,LatBox,LongBox, Dist,Az);
            Result := ' off map ' + SmartDistanceMetersFormat(Dist) + ' to ' + Direction;
         end;

begin
    {$IfDef RecordOffMap} WriteLineToDebugFile('RecordOffMap enter, ' + LatLongDegreeToString(Lat,Long));   {$EndIf}
    South := Lat < MapCorners.BoundBoxGeo.ymin;
    North := Lat > MapCorners.BoundBoxGeo.ymax;
    West := Long < MapCorners.BoundBoxGeo.xmin;
    East := Long > MapCorners.BoundBoxGeo.xmax;
    Result := '';
    if North and West then begin
       DoIt('NW',MapCorners.BoundBoxGeo.ymax,MapCorners.BoundBoxGeo.xmin);
    end
    else if North and East then begin
       DoIt('NE',MapCorners.BoundBoxGeo.ymax,MapCorners.BoundBoxGeo.xmax);
    end
    else if South and West then begin
       DoIt('SW',MapCorners.BoundBoxGeo.ymin,MapCorners.BoundBoxGeo.xmin);
    end
    else if South and East then begin
       DoIt('SE',MapCorners.BoundBoxGeo.ymin,MapCorners.BoundBoxGeo.xmax);
    end
    else if North then begin
       DoIt('N',MapCorners.BoundBoxGeo.ymax,Long);
    end
    else if East then begin
       DoIt('E',Lat,MapCorners.BoundBoxGeo.xmax);
    end
    else if South then begin
       DoIt('S',MapCorners.BoundBoxGeo.ymin,Long);
    end
    else if West then begin
       DoIt('W',Lat,MapCorners.BoundBoxGeo.xmin);
    end;
   {$IfDef RecordOffMap} WriteLineToDebugFile('RecordOffMap out, result=' + Result); {$EndIf}
end;


procedure tMapDraw.GetMapScaleFactor(var Lat,Long,Maph,Mapk: float64; var Prime : boolean);
 {$IfDef ExTissot}
 begin
 {$Else}

      procedure DrawDepending(projection : BaseMap.tMapProjection);
      begin
         Projection.GetMapScaleFactor(Lat,Long,Maph,Mapk,Prime);
      end;

 begin
      if ((VectorIndex <> 0) and PrimMapProj.TissotEnabled) then DrawDepending(PrimMapProj);
      if (DEMMap and (DEMGlb[DEMonMap].DEMMapProjection <> Nil) and
         DEMGlb[DEMonMap].DEMMapProjection.TissotEnabled) then DrawDepending(DEMGlb[DEMonMap].DEMMapProjection);
      if (ValidSatOnMap and (SatImage[SatOnMap].ImageMapProjection <> nil)) then DrawDepending(SatImage[SatOnMap].ImageMapProjection);
 {$EndIf}
 end;



{$IfDef AllowDEMGeomorph}

         procedure TMapDraw.OverlayCategories(Bitmap : tMyBitmap; TerrainCategory : tTerrainCatDefinition);
         var
            Overlay : tMyBitmap;
         begin
            {$IfDef RecordTerrainCategories} WriteLineToDebugFile('TMapDraw.OverlayCategories '+  TerrainCategoryToString(TerrainCategory)); {$EndIf}
            Overlay := CreateCategoryOverlay(TerrainCategory);
            DrawAndDeleteOverlay(Bitmap,Overlay);
         end;


         function TMapDraw.CreateCategoryOverlay(TerrainCategory : tTerrainCatDefinition) : tMyBitmap;
         var
            xg1,yg1,x,y     : integer;
            tc      : tRGBTriple;
            BMPMem : tBMPMemory;
         begin
            {$IfDef RecordTerrCatOverlays} WriteLineToDebugFile('TMapDraw.CreateCategoryOverlay: ' +  DEMGlb[DEMonMap].TerrainCategoryLabel(TerrainCategory)); {$EndIf}
            try
               CreateBitmap(Result,MapXSize,MapYSize);
               BMPMem := tBMPMemory.Create(Result);
               tc := TerrainCategory.CatColor;
               if ShowSatProgress then StartProgress('Terrain Category');
               for y := 0 to pred(MapYSize) do begin
                  if (y mod 500 = 0) and ShowSatProgress then UpDateProgressBar(y/MapYSize);
                  for x := 0 to pred(MapXSize) do begin
                     ScreenToDEMGrid(x,y,XG1,YG1);
                     if DEMGlb[DEMonMap].InTerrainCategory(xg1,yg1,TerrainCategory) then BMPMem.SetPixelColor(x,y,tc);
                  end;
               end {for i};
            finally
               EndProgress;
               BMPMem.Destroy;
            end;
         end;


         function TMapDraw.TerrainCategoryLegend : tMyBitmap;
         var
            i,x,y,{xlo,xhi,ylo,yhi,}Num,Tot, MaxWidth,Width : integer;
            TerrainCategory : tTerrainCatDefinition;
            TStr : shortstring;
            CatList : tStringList;
         begin
            {$IfDef RecordTerrainCategories} WriteLineToDebugFile('Terrain categories in TMapDraw.TerrainCategoryLegend'); {$EndIf}
            CatList := tStringList.Create;
            ShowHourglassCursor;
            CreateBitmap(Result,500,15 * MapOverlays.ovTerrainCat.Count);
            Result.Canvas.Brush.Style := bsSolid;
            //DEMGridLimitsOnMap(xlo,ylo,yhi,xhi);

            MaxWidth := 0;
            for i := 0 to pred(MapOverlays.ovTerrainCat.Count) do begin
               {$IfDef RecordTerrainCategories} WriteLineToDebugFile(MapOverlays.ovTerrainCat[i]); {$EndIf}
               TerrainCategory := StringToTerrainCategory(MapOverlays.ovTerrainCat[i]);
               if MDDef.TerrainCatPercentages then begin
                  Num := 0;
                  Tot := 0;
                  for x := round(MapCorners.BoundBoxDataGrid.xmin) to round(MapCorners.BoundBoxDataGrid.xmax) do begin
                     for y := Round(MapCorners.BoundBoxDataGrid.ymin) to round(MapCorners.BoundBoxDataGrid.ymax) do begin
                        if DEMGlb[DEMonMap].InTerrainCategory(x,y,TerrainCategory) then inc(Num);
                        inc(Tot);
                     end;
                  end;
                  TStr := ' (' + RealToString(100*Num/Tot,-8,2) + '%  ' + SmartAreaFormat(Num * DEMGlb[DEMonMap].AverageXSpace * DEMGlb[DEMonMap].AverageYSpace) + ')';
               end
               else TStr := '';
               TStr := DEMGlb[DEMonMap].TerrainCategoryLabel(TerrainCategory) + TStr;
               {$IfDef RecordTerrainCategories} WriteLineToDebugFile('Legend: ' + TStr); {$EndIf}
               CatList.Add(TStr);
               Width := Result.Canvas.TextWidth(TStr);
               if (Width > MaxWidth) then MaxWidth := Width;
            end;
            Result.Width := MaxWidth +30;

            for i := 0 to pred(CatList.Count) do begin
               TerrainCategory := StringToTerrainCategory(MapOverlays.ovTerrainCat[i]);
               Result.Canvas.Brush.Color := clWhite;
               Result.Canvas.TextOut(25,i*15,CatList.Strings[i]);
               Result.Canvas.Brush.Color := ConvertPlatformColorToTColor(TerrainCategory.CatColor);
               Result.Canvas.Rectangle(2,i*15,23,i*15 + 13);
            end;
            CatList.Free;
            ShowDefaultCursor;
         end;
{$EndIf}


initialization
finalization
   {$IfDef RecordPrinter} WriteLineToDebugFile('RecordPrinterProblems in demmapdraw'); {$EndIf}
   {$IfDef RecordOffMap} WriteLineToDebugFile('RecordOffMap  in demmapdraw'); {$EndIf}
   {$IfDef RecordRecordAmbush} WriteLineToDebugFile('RecordAmbushProblems in demmapdraw'); {$EndIf}
   {$IfDef RecordMapDraw} WriteLineToDebugFile('RecordMapDraw in demmapdraw'); {$EndIf}
   {$IfDef RecordRoute} WriteLineToDebugFile('RecordRouteProblems in demmapdraw'); {$EndIf}
   {$IfDef RecordContour} WriteLineToDebugFile('RecordContourProblems in demmapdraw'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosing in demmapdraw'); {$EndIf}
   {$IfDef RecordFly} WriteLineToDebugFile('RecordFlyProblems in demmapdraw'); {$EndIf}
   {$IfDef RecordTIGER} WriteLineToDebugFile('RecordTIGERProblems in demmapdraw'); {$EndIf}
   {$IfDef RecordMapMerge} WriteLineToDebugFile('RecordMapMerge in demmapdraw'); {$EndIf}
   {$IfDef RecordContourLines} WriteLineToDebugFile('RecordContourLines in demmapdraw'); {$EndIf}
   {$IfDef RecordFan} WriteLineToDebugFile('RecordFan in demmapdraw'); {$EndIf}
   {$IfDef RecordElevationScaling} WriteLineToDebugFile('RecordElevationScaling in demmapdraw'); {$EndIf}
   {$IfDef RecordMagDecDiagram} WriteLineToDebugFile('RecordMagDecDiagram in DEMMapDraw'); {$EndIf}
   {$IfDef RecordTissot} WriteLineToDebugFile('RecordTissotProblems in DEMMapDraw'); {$EndIf}
   {$IfDef RecordTargetAreasCoverage} WriteLineToDebugFile('RecordTargetAreasCoverage in DEMMapDraw'); {$EndIf}
   {$IfDef RecordMapMargin} WriteLineToDebugFile('RecordMapMarginProblems in DEMMapDraw'); {$EndIf}
   {$IfDef RecordDrawGridLines} WriteLineToDebugFile('RecordDrawGridLines in DEMMapDraw'); {$EndIf}
   {$IfDef RecordLegend} WriteLineToDebugFile('RecordLegend in DEMMapDraw'); {$EndIf}
   {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('TimeFigureMultiSensorCoverage in DEMMapDraw'); {$EndIf}
   {$IfDef RecordTerrainCategories} WriteLineToDebugFile('RecordTerrainCategories in DEMMapDraw'); {$EndIf}
   {$IfDef RecordPLSS} WriteLineToDebugFile('RecordPLSSProblems in DEMMapDraw'); {$EndIf}
   {$IfDef RecordDBPlots} WriteLineToDebugFile('RecordDBPlots in DEMMapDraw'); {$EndIf}
   {$IfDef RecordIHSmerges} WriteLineToDebugFile('RecordIHSmerges in DEMMapDraw'); {$EndIf}
   {$IfDef RecordStretchBitmap} WriteLineToDebugFile('RecordStretchBitmap in DEMMapDraw'); {$EndIf}
   {$IfDef RecordMapBlow} WriteLineToDebugFile('RecordMapBlow in DEMMapDraw'); {$EndIf}
   {$IfDef RecordTerrCatOverlays} WriteLineToDebugFile('RecordTerrCatOverlays in DEMMapDraw'); {$EndIf}
   {$IfDef RecordDrawSecondGrid} WriteLineToDebugFile('RecordDrawSecondGrid in DEMMapDraw'); {$EndIf}
   {$IfDef RecordMapLimits} WriteLineToDebugFile('RecordMapLimits in DEMMapDraw'); {$EndIf}
   {$IfDef RecordWMS} WriteLineToDebugFile('RecordWMS in DEMMapDraw'); {$EndIf}
   {$IfDef RecordDrawLayers} WriteLineToDebugFile('RecordDrawLayers in DEMMapDraw'); {$EndIf}
   {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('RecordShapeFileGroup in demmapdraw'); {$EndIf}
   {$IfDef RecordQueryGeoBox} WriteLineToDebugFile('RecordQueryGeoBox in demmapdraw'); {$EndIf}
   {$IfDef RecordMapIndex} WriteLineToDebugFile('RecordMapIndex in demmapdraw'); {$EndIf}
   {$IfDef RecordHiResDEM} WriteLineToDebugFile('RecordHiResDEM in DEMMapDraw'); {$EndIf}
   {$IfDef RecordSat} WriteLineToDebugFile('RecordSatProblems in DEMMapDraw'); {$EndIf}
   {$IfDef RecordOverlays} WriteLineToDebugFile('RecordOverlays in DEMMapDraw'); {$EndIf}
   {$IfDef RecordElevColorFromTable} WriteLineToDebugFile('RecordElevColorFromTable in DEMMapDraw'); {$EndIf}
   {$IfDef RecordSatTraining} WriteLineToDebugFile('RecordSatTraining in DEMMapDraw'); {$EndIf}
   {$IfDef RecordCarefulPolyLineDraw} WriteLineToDebugFile('RecordCarefulPolyLineDraw in DEMMapDraw'); {$EndIf}
   {$IfDef RecordGeology} WriteLineToDebugFile('RecordGeology in DEMMapDraw'); {$EndIf}
   {$IfDef RecordNLCD} WriteLineToDebugFile('RecordNLCD in DEMMapDraw'); {$EndIf}
   {$IfDef NoParallelFor} WriteLineToDebugFile('NoParallelFor in demmapdraw'); {$EndIf}
   {$IfDef RecordWorldOutline} WriteLineToDebugFile('RecordWorldOutline in demmapdraw'); {$EndIf}
   {$If Defined(TimePLSS)} WriteLineToDebugFile('TimePLSS in demmapdraw'); {$EndIf}
   {$IfDef RecordFullDrawGridLines} WriteLineToDebugFile('TimePLSS in demmapdraw'); {$EndIf}
end.




