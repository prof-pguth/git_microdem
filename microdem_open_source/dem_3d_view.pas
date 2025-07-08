unit dem_3d_view;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordFindLatLong}
   //{$Define RecordClosing}
   //{$Define RecordSetUpPanoramaView}
   //{$Define RecordPerspLabel}
   //{$Define RecordPerspLabelWrite}
   //{$Define RecordPerspLabelShift}
   //{$Define RecordFindLatLongError}
   //{$Define RecordDrape}
   //{$Define RecordFullDrape}
   //{$Define RecordHorizon}
   //{$Define RecordLocatePointOnPersp}
   //{$Define ShowDrapeExtent}
   //{$Define RecordTrackObserverElevation}
   //{$Define RecordPerspective}
   //{$Define RecordPerspectiveProgress}
   //{$Define RecordSatTimeSeries}
   //{$Define RecordPerspectiveSize}
   //{$Define RecordPerspectiveTime}
   //{$Define RecordOblique}
   //{$Define RecordFlySequence}
   //{$Define RecordShortFlySequence}
   //{$Define RecordElevAndCompare}          //major slowdow
   //{$Define RecordPerspectiveHorizon}     //detailed options
   //{$Define RecordMapDraw}
   //{$Define RecordInnerLoopPerspective}
   //{$Define RecordPerspectivePancake}
   //{$Define RecordDefineDatum}
   //{$Define RecordView3dCreate}
{$EndIf}


interface

uses
//needed for inline of core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}

  {$IfDef VCL}
     Graphics,Controls,Forms, Dialogs,Menus,
     DEMMapF,Windows,
  {$EndIf}

  {$IfDef FMX}
     FMX.Graphics,
  {$EndIf}

  SysUtils,  Classes,
  System.Threading,System.SyncObjs,System.UITypes,
  Petmar_types,PETMAR,PETImage,PETMath,DEMdefs;

const
   MaxHideArray = 2400;

type
   HideArray  = array[0..MaxHideArray] of integer;
   tDrapeDisplay = (QuickDrapeReflectance,DrapeReflectance,DrapeBMP);
   tPersSize = (psPerpsective,psPanorama,psFlying);

type
  tView3D = class
     protected
     private
        function DrapedRGBtriple(xgrid,ygrid : float64; Which : tDrapeDisplay; var Valid : boolean) : tPlatformColor;
        procedure PerspectiveDrawStrip(FirstXStrip,LastXStrip : integer; BMPMemory : tBMPMemory; ShowProgress : boolean);
     public
        MainInCh : AnsiChar;
        DrapeMapUsing,
        RegionalDEM,
        MaxShift,NumRuns,
        DEMOnView : integer;
        RadialPointSpacing : array[0..2] of float64;
        SpecifiedCaption : shortstring;
        PersOpts : tPerspectiveOptions;

        {$IfDef ExFly}
        {$Else}
           FlyOpts : tFlyOptions;
        {$EndIf}

        StraightLineAlgorithm : DemDefs.tStraightAlgorithm;
        PersSize : tPersSize;
        SaveName : shortstring;
        FlightRouteDB : tMyData;
        FlightDBName   : PathStr;
        PointOnView,
        ForceElevAngle,
        RedrapeEachFrame,
        ASingleView,
        FirstFrame,
        FirstDraw,
        TargetRun : boolean;

        MinPitch,MaxPitch,MinRange,MaxRange,
        ScaleFac,DefVertExag,ViewWidth,
        xg3,yg3,xg4,yg4,
        SectLen,
        DistanceAlongRoute,
        LeftAzimuth,RightAzimuth,
        ViewVFOV,ViewHFOV,
        ViewerLat,ViewerLong,
        ViewedLat,ViewedLong,
        FlightAzimuth,ViewAzimuth,ViewDepth,
        tanMinElevAngle,tanMaxElevAngle,LookHeadingRelative,
        Exag,Size,TargetXUTM,TargetYUTM : float64;
        XGridLeft,YGridLeft,LeftRearGridX,LeftRearGridY,RightRearGridX,RightRearGridY,
        XGridRight,YGridRight : float64;  //the viewer's location in the DEM grid
        ObsElev,      //viewer's total elevation
        ElevObs : float32;     //ground elevation under viewer

        {$IfDef VCL}
        DrapingMaps  : array[1..2] of tMapForm;
        DrapedBMP    : array[1..2] of tMyBitmap;
        P0    : array[1..2] of tScreenPRGB;
        {$EndIf}

        DrapeDisplay : tDrapeDisplay;
        xu,yu        : array[1..4] of float64;

     {for oblique, corner point order is:
              Left is left front
              Right is right front
              3 is left rear
              4 is right rear}
        NumPointsOnRay : array[0..2] of integer;
        FanLeft,FanRight,FanRange,
        ViewportHeight,ViewportWidth,
        x,y,NumRadialPts : integer;
        constructor Create;
        destructor Destroy;
        procedure InitHidden;
        procedure CloseHidden;
        procedure SetSize(Depress : float64 = 0; SetFOV : boolean = false);
        procedure InitializeViewParameters;

        {$IfDef VCL}
        procedure CloseDrapingMaps;
        procedure SetUpDrapeMap(WhatStarting : tDEMDoingWhat; MapForm : tMapForm; Distance : float64);
        {$EndIf}

        function PerspectiveDraw(var Bitmap : tMyBitmap; ShowProgress : boolean) : boolean;
        function FindPitch(Elev1,DistOut : float64) : float64;
        procedure SeriesNumberCoord(Prof : integer);
        procedure AzimuthPitchToScreen(Azimuth,Pitch : float32; var x,y : integer);
        procedure ScreenToAzimuthPitch(x,y : integer; var Azimuth,Pitch : float32);
        procedure AzimuthToScreen(Azimuth : float32; var x : integer);
        procedure PitchToScreen(Pitch : float32; var y : integer);
        procedure ScreenToAzimuth(x: integer; var Azimuth : float32);
        procedure ScreenToPitch(y : integer; var Pitch : float32);
        procedure SetAzimuthLimits;
        function VertAngleRange : shortstring;
        function FieldOfView : shortstring;

        procedure BackCorner(LastX : integer;  var Azimuth : float32; var ViewedLat,ViewedLong,LeftRearGridX,LeftRearGridY  : float64);
        procedure FindProfilePoint(DistOut : float64; var xr,yr,Elev1,ThisPitch : float64);
        procedure ViewPortBackCorners(var LeftRearGridX,LeftRearGridY,RightRearGridX,RightRearGridY : float64);
        function ViewWithInData(ModifyElevAngle,SafetyFactor,NewExtremes : boolean) : boolean;
  end;

procedure OpenPanoramaView(var View3D : tView3D; Lat,Long,CamerasElevAboveGround,Depth,inAzimuth,HFOV,VFOV,Depress : float64; DEMUsed : integer;
    ViewCaption : shortstring = ''; DoItDraped : boolean = false; WhatStarting : tDEMDoingWhat = JustWandering; Distance : float64 = 0);


implementation

uses
   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}

   {$IfDef ExPLSS}
   {$Else}
      DEM_PLSS,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
      DEMEros,
   {$EndIf}

   {$IfDef ExStereo}
   {$Else}
      Stereo_viewer,
   {$EndIf}

   Petdbutils,
   BaseMap,
   DEMCoord, Make_tables,
   Demdef_routines;


var
   YMin, YMinNew  : ^HideArray;
   ResizingNow,
   NeedErrorString,
   GetRadial : boolean;
   MovieList     : tStringList;
   {$IfDef VCL}
      AnyPointSet   : boolean;
   {$EndIf}

function tView3d.VertAngleRange : shortstring;
begin
   Result := 'Vert angle range: ' + RealToString(MinPitch,8,3) + '  to ' +  RealToString(MaxPitch,8,3);
end;

function tView3d.FieldOfView : shortstring;
begin
   Result := '  HFOV: ' + RealToString(ViewHFOV,-8,0)+ '  VFOV: ' + RealToString(ViewVFOV,-8,0);
end;


procedure OpenPanoramaView(var View3D : tView3D; Lat,Long,CamerasElevAboveGround,Depth,inAzimuth,HFOV,VFOV,Depress : float64; DEMUsed : integer;
    ViewCaption : shortstring = ''; DoItDraped : boolean = false; WhatStarting : tDEMDoingWhat = JustWandering; Distance : float64 = 0);
begin
   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('OpenPanoramaView MDDef.PerspOpts.PersWidth=' + IntToStr(MDDef.PerspOpts.PersWidth)); {$EndIf}

   View3D := tView3D.Create;
   if (HFOV > 75) then View3D.PersSize := psPanorama
   else View3D.PersSize := psPerpsective;
   View3D.InitializeViewParameters;

   View3D.ViewerLat  := Lat;
   View3D.ViewerLong := Long;
   View3D.ViewAzimuth := inAzimuth;
   View3D.ViewVFOV := VFOV;
   View3D.ViewHFOV := HFOV;

   DEMGlb[DEMused].GetElevFromLatLongDegree(Lat,Long,View3D.ObsElev);
   View3D.PersOpts.PersObsUp := CamerasElevAboveGround;

   View3D.MinPitch := Depress - 0.5 * VFOV;
   View3D.MaxPitch := Depress + 0.5 * VFOV;
   View3D.MaxRange := Depth;
   View3D.MinRange := 0;
   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('OpenPanoramaView before SetAzimuthLimits,' + View3D.FieldOfView); {$EndIf}
   View3D.SetAzimuthLimits;
   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('OpenPanoramaView after InitializeViewParameters,  Viewer: ' + LatLongDegreeToString(Lat,Long) + ' ' + View3D.VertAngleRange + View3D.FieldOfView); {$EndIf}

   View3D.SpecifiedCaption := ViewCaption;
   MDdef.PerspOpts.FlyDepth := round(Depth);
   View3D.DEMonView := DEMUsed;

   View3D.SetSize(Depress,false);
   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('OpenPanoramaView After setsize,' + View3D.FieldOfView); {$EndIf}
   View3D.PersOpts.WhichPerspective := ReflectancePerspective;
   View3D.ViewDepth := Depth;
   View3D.ForceElevAngle := true;
   View3D.ViewWithInData(false,false,false);
   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('OpenPanoramaView exit,' + View3D.FieldOfView); {$EndIf}
end;


{$IfDef VCL}
procedure BresenhamDrawAlgorithm(BitMap : tMyBitmap; x1,y1,x2,y2 : integer; color : TColor;  PointSetter : PETMAR_types.SetPointProcedure);
var
   x,y,DeltaX,DeltaY,XStep,YStep,direction : integer;
begin
   x := x1;
   y := y1;
   if x1 > x2 then XStep := -1 else XStep := 1;
   if y1 > y2 then YStep := -1 else YStep := 1;
   DeltaX := abs(x2 - x1);
   DeltaY := abs(y2 - y1);
   if DeltaX = 0 then direction := -1 else direction := 0;
   while not ((x = x2) and (y = y2)) do begin
      PointSetter(Bitmap,x,y,color);
      if direction < 0 then begin
         inc(y,YStep);
         inc(direction,DeltaX);
      end
      else begin
         inc(x,XStep);
         dec(direction,DeltaY);
      end {if};
   end {while};
end {proc BresenhamDraw};

{$EndIf}


function tView3D.ViewWithInData(ModifyElevAngle,SafetyFactor,NewExtremes : boolean) : boolean;
var
   dxg,dyg,ElevAngle,p,pe0x,pe0y : float64;
   i,j : integer;

      procedure ElevAndCompare(xg,yg : float64; DoMin : boolean);
      var
         Elev1 : float32;
      begin
         if DEMGlb[DEMonView].GetElevMeters(xg,yg,Elev1) then begin
            ElevAngle := ArcTan( (Elev1 - (ElevObs + PersOpts.PersObsUp)) / p) / DegToRad;
            if (ElevAngle > MaxPitch) then MaxPitch := ElevAngle;
            if DoMin and (ElevAngle < MinPitch) then MinPitch := ElevAngle;
            {$IfDef RecordElevAndCompare} WriteLineToDebugFile(RealToString(p,12,0) + RealToString(Elev1,12,1) + RealToString(ElevObs,12,1)); {$EndIf}
         end;
      end;

begin {func ViewWithInData}
   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('ViewWithinData in, ' + VertAngleRange + FieldOfView); {$EndIf}

   DEMGlb[DEMonView].LatLongDegreeToDEMGrid(ViewerLat,ViewerLong,XGridRight,YGridRight);
   if (not ModifyElevAngle) and (not NewExtremes) then exit;

   {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('ViewWithinData 1'); {$EndIf}
   SeriesNumberCoord(0);
   {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('ViewWithinData 1.5'); {$EndIf}
   Result := (ViewDepth > 0.1);
   if (not Result) then exit;

   for i := 0 to 2 do begin
      RadialPointSpacing[i] := (DEMGlb[DEMonView].AverageXSpace);
      {$IfDef ExAdvancedSats}
      {$Else} if (i > 0) and (DrapingMaps[i] <> Nil) and IsSatelliteMap(DrapingMaps[i].MapDraw.MapType) then RadialPointSpacing[i] := (SatImage[DrapingMaps[i].MapDraw.SatOnMap].MetersPerPixel); {$EndIf}
      NumPointsOnRay[i] := round(ViewDepth / RadialPointSpacing[i]);
      if (NumPointsOnRay[i] < 150) then NumPointsOnRay[i] := 150;
   end {for i};

   {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('ViewWithinData 2'); {$EndIf}

   if ModifyElevAngle then begin
      {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('ViewWithinData 2.5'); {$EndIf}
      if NewExtremes then begin
         MinPitch := 90;
         MaxPitch := -90;

         {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('ViewWithinData 3'); {$EndIf}
         ViewPortBackCorners(LeftRearGridX,LeftRearGridY,RightRearGridX,RightRearGridY);

         {$IfDef RecordPerspective}
            WriteLineToDebugFile('View parameters:');
            WriteLineToDebugFile('  Viewer:     ' + DEMGlb[DEMonView].DEMLocationString(XGridRight,YGridRight) + '  Viewer elev:' + RealToString(ObsElev,8,1));
            WriteLineToDebugFile('  Left rear:  ' + DEMGlb[DEMonView].DEMLocationString(LeftRearGridX,LeftRearGridY) + '  Right Rear: ' + DEMGlb[DEMonView].DEMLocationString(RightRearGridX,RightRearGridY));
         {$EndIf}

         i := 0;
         while (i <= ViewportWidth) do begin
            pe0x := LeftRearGridX - i * (LeftRearGridX - RightRearGridX) / ViewportWidth;
            pe0y := LeftRearGridY - i * (LeftRearGridY - RightRearGridY) / ViewportWidth;
            dxg := (pe0x - XGridRight) / NumPointsOnRay[DrapeMapUsing];
            dyg := (pe0y - YGridRight) / NumPointsOnRay[DrapeMapUsing];

            j := 5;
            while (j <= NumPointsOnRay[DrapeMapUsing]) do begin
               p := j / NumPointsOnRay[DrapeMapUsing] * ViewDepth;
               ElevAndCompare(XGridRight + j * dxg,YGridRight + j * dyg,(j=5));
               inc(j,5);
            end;
            inc(i,15);
         end;
      end;

      if SafetyFactor then begin
         MaxPitch := MaxPitch + MDDef.PerspectiveSkyAngle;   //0.10 * p;
      end;
      MinPitch := MinPitch - ViewVFOV;
   end {if};
   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('ViewWithinData in,' + VertAngleRange); {$EndIf}
end {function ViewWithinData};


procedure tView3D.SetAzimuthLimits;
begin
   LeftAzimuth := ViewAzimuth - (0.5 * ViewHFOV);
   RightAzimuth := ViewAzimuth + (0.5 * ViewHFOV);
   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('tView3D.SetAzimuthLimits, az range: ' + RealToString(LeftAzimuth,8,0) + RealToString(RightAzimuth,8,0) + ' HFOV: ' + RealToString(ViewHFOV,8,0) + ' VFOV: ' + RealToString(ViewVFOV,8,0)); {$EndIf}
end;


procedure tView3D.SetSize;
begin
   if SetFOV then begin
      ViewHFOV := MDdef.PersHFOV;
      ViewVFOV := MDdef.PersVFOV;
   end;
   {$IfDef FMX}
      ViewPortWidth := MDDef.PerspOpts.PersWidth;
   {$Else}
      if (PersSize = psPerpsective) then begin
         ViewPortWidth := PersOpts.PersWidth;
         ViewPortHeight := PersOpts.PersHeight;
      end
      else if (PersSize = psPanorama) then begin
         ViewPortWidth := PersOpts.PanWidth;
         ViewPortHeight := PersOpts.PanHeight;
         if SetFOV then begin
            ViewHFOV := MDdef.PanHFOV;
            ViewVFOV := MDdef.PanVFOV;
         end;
      end
      else if (PersSize = psFlying) then begin
        {$IfDef ExFly}
        {$Else}
            ViewPortWidth := FlyOpts.FlyThroughWidth;
            ViewPortHeight := FlyOpts.FlyThroughHeight;
         {$EndIf}
      end;

      if SetFov and (MaxPitch < -99) then begin
         MinPitch := Depress - 0.5 * ViewVFOV;
         MaxPitch := Depress + 0.5 * ViewVFOV;
      end;
   {$EndIf}

   if MDDef.PerspOpts.NoVE then ViewPortHeight := round(ViewPortWidth * ViewVFOV / ViewHFOV);

   {$IfDef RecordPerspective} WriteLineToDebugFile('tView3d.SetSize, width=' + IntToStr(ViewPortWidth) + '    & height=' + IntToStr(ViewPortHeight)); {$EndIf}
end;


{$IfDef VCL}
procedure tView3D.CloseDrapingMaps;
var
   i : integer;
begin
   for i := 1 to 2 do if (DrapingMaps[i] <> Nil) then begin
      {$IfDef RecordClosing} WriteLineToDebugFile('Close perspective/oblique window, drape ' + IntToStr(i)); {$EndIf}
      DrapingMaps[i].Closable := true;
      DrapingMaps[i].Close;
      DrapingMaps[i] := Nil;
      if (DrapedBMP[i] <> Nil) then FreeAndNil(DrapedBMP[i]);
   end;
   DrapeMapUsing := 0;
end;


procedure tView3D.SetUpDrapeMap(WhatStarting : tDEMDoingWhat; MapForm : tMapForm; Distance : float64);
const
   LittleBit = 0.005;
var
   HadToSubsample : boolean;
   SubsampleFactor : float64;
   {$IfDef RecordDrape}
      i : integer;
   {$EndIf}

   procedure SetUpSingleDrape(ThisViewDepth : float64; WhichDrapeMap : integer);
   var
      Lat,Long,LatLow,LatHigh,LongLow,LongHigh,Bearing,sf2 : float64;
      xu1,yu1,xu4,yu4 : float64;
      DrapeForm : tMapForm;
   begin
      with MapForm do begin
         SetAzimuthLimits;
         DrapeForm := Nil;
         DrapeForm := tMapForm.create(Application);
         DrapeDisplay := DrapeBMP;
         {$IfDef RecordFullDrape} WriteLineToDebugFile('calling CopyMap in tView3D.SetUpDrapeMap'); {$EndIf}
         CopyMap(DrapeForm,true,true);
         {$IfDef RecordFullDrape} WriteLineToDebugFile('done CopyMap in tView3D.SetUpDrapeMap'); {$EndIf}

         RedrapeEachFrame := WhatStarting in [SeekingFlyThroughRoute,JustWandering];
         if (WhatStarting = LiveFly2) then begin
            LatLow := MapDraw.MapCorners.BoundBoxGeo.ymin;
            LatHigh := MapDraw.MapCorners.BoundBoxGeo.ymax;
            LongLow := MapDraw.MapCorners.BoundBoxGeo.xmin;
            LongHigh := MapDraw.MapCorners.BoundBoxGeo.xmax;
         end
         else begin
            if (WhatStarting = SeekingThirdCircleFly) then begin
               LeftAzimuth := 0;
               RightAzimuth := 360;
            end;
            LatLow := ViewerLat - LittleBit;
            LatHigh := ViewerLat + LittleBit;
            LongLow := ViewerLong - LittleBit;
            LongHigh := ViewerLong + LittleBit;
            Bearing := LeftAzimuth-5;
            while (Bearing <= RightAzimuth+5) do begin
               VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,1.05*ThisViewDepth,Bearing,Lat,Long);
               {$IfDef ShowDrapeExtent} MapForm.MapDraw.LatLongDegreeToScreen(Lat,Long,xp1,yp1); ScreenSymbol(MapForm.Image1.Canvas,xp1,yp1,FilledBox,3,clRed);  {$EndIf}
               CompareValueToExtremes(Lat,LatLow,LatHigh);
               CompareValueToExtremes(Long,LongLow,LongHigh);
               Bearing := Bearing + 5;
            end;
         end;

         if MapDraw.DEMMap then begin
            DEMGLB[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(LatLow,LongLow,xu1,yu4);
            DEMGLB[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(LatHigh,LongHigh,xu4,yu1);
            DEMGLB[MapDraw.DEMonMap].ClipDEMGrid(xu1,yu4);
            DEMGLB[MapDraw.DEMonMap].ClipDEMGrid(xu4,yu1);
         end
         else begin
            {$IfDef ExSat}
            {$Else}
               SatImage[MapDraw.SATonMap].LatLongDegreeToSatGrid(SatImage[MapDraw.SATonMap].BandForSize,LatLow,LongLow,xu1,yu4);
               SatImage[MapDraw.SATonMap].LatLongDegreeToSatGrid(SatImage[MapDraw.SATonMap].BandForSize,LatHigh,LongHigh,xu4,yu1);
               SatImage[MapDraw.SATonMap].ClipSatGrid(SatImage[MapDraw.SATonMap].BandForSize,xu1,yu4);
               SatImage[MapDraw.SATonMap].ClipSatGrid(SatImage[MapDraw.SATonMap].BandForSize,xu4,yu1);
            {$EndIf}
         end;

         HadToSubSample := ( (abs(xu1-xu4) > MDdef.MaxDrapeXSize) or (abs(yu1-yu4) > MDdef.MaxDrapeYSize));
         if HadToSubsample then begin
            sf2 := MDdef.MaxDrapeYSize / abs(Yu1-yu4);
            SubsampleFactor := MDdef.MaxDrapeXSize / abs(xu1-xu4);
            if (sf2 < SubsampleFactor) then SubsampleFactor := sf2;
         end;
         {$IfDef RecordFullDrape} WriteLineToDebugFile('about to DrapeForm.DrapeMapFromTwoGridCorners'); {$EndIf}

         DrapeForm.DrapeMapFromTwoGridCorners(xu1,yu4,xu4,yu1);   {order is left,bottom,right,top}

         {$IfDef ShowDrapeExtent}
            MapForm.MapDraw.GridToScreen(xu1,yu4,xp1,yp1); MapForm.MapDraw.GridToScreen(xu4,yu1,xp2,yp2);
            MapForm.Image1.Canvas.Brush.Style := bsClear;  MapForm.Image1.Canvas.Rectangle(xp1,yp1,xp2,yp2);
         {$EndIf}

         {$IfDef RecordFullDrape} WriteLineToDebugFile('back from DrapeForm.DrapeMapFromTwoGridCorners'); {$EndIf}

         DrapeMapUsing := WhichDrapeMap;
         DrapingMaps[DrapeMapUsing] := DrapeForm;
         DrapingMaps[DrapeMapUsing].Caption := 'Drape map ' + IntToStr(DrapeMapUsing) +  ' pixel size=' + RealToString(DrapeForm.MapDraw.ScreenPixelSize,-8,1) + ' m';

         {$IfDef RecordFullDrape}
            WriteLineToDebugFile('Set up drape map in tView3D.SetUpDrapeMap, Map draped: ' + MapForm.Caption);
            WriteLineToDebugFile('Grid (UL): x=' + RealToString(xu1,12,2) + '   &y=' + RealToString(yu4,12,2) + '  Grid (LR): x=' + RealToString(xu4,12,2) + '   &y=' + RealToString(yu1,12,2));
            WriteLineToDebugFile('PersViewDepth: ' + RealToString(ViewDepth,-12,0) + '  rapeMapUsing= ' + IntToStr(DrapeMapUsing));
         {$EndIf}

         {$IfDef RecordDrape} WriteLineToDebugFile('Done drape map in tView3D.SetUpDrapeMap'); {$EndIf}
      end;
   end;


begin
   {$IfDef RecordDrape} WriteLineToDebugFile('tView3D.SetUpDrapeMap in'); {$EndIf}
   CloseDrapingMaps;
   SetAzimuthLimits;
   SetUpSingleDrape(ViewDepth,1);

   {$IfDef RecordFullDrape}
      for I := 1 to 2 do begin
         if DrapingMaps[i] = Nil then WriteLineToDebugFile(IntToStr(i) + ' Drape map nil')
         else WriteLineToDebugFile(DrapingMaps[i].Caption);
      end;
   {$EndIf}
   {$IfDef RecordDrape} WriteLineToDebugFile('tView3D.SetUpDrapeMap out'); {$EndIf}
end;

{$F+}
procedure HiddenSetPoint(BitMap : tMyBitmap; x,y : integer; color : TColor);
begin
  if (y < YMin^[x]) then begin
     Bitmap.Canvas.Pixels[x,y] := Color;
     AnyPointSet := True;
  end {if};
  if y < YMinNew^[x] then YMinNew^[x] := y;
end;

{$EndIf}

function TView3D.DrapedRGBtriple(xgrid,ygrid : float64; Which : tDrapeDisplay; var Valid : boolean) : tPlatFormColor;
var
   xp,yp   : integer;
   Lat,Long : float64;
begin
   Valid := DEMGlb[DEMonView].GridInDataSetFloat(xgrid,ygrid);
   if Valid then begin
      PointOnView := true;
      if (Which in [DrapeReflectance,QuickDrapeReflectance]) then Result := DEMGlb[DEMonView].RGBReflectanceColor(MDDef.DefRefMap,round(xgrid),round(ygrid))
      else if (DrapeMapUsing <> 0) then begin
         {$IfDef VCL}
            DrapingMaps[DrapeMapUsing].MapDraw.DEMGridToScreen(xgrid,ygrid,xp,yp);
            if DrapingMaps[DrapeMapUsing].MapDraw.OnScreen(xp,yp) then begin
               Result := P0[DrapeMapUsing][yp]^[xp];
            end
            else begin
               if (DrapeMapUsing = 2) then begin
                  DEMGlb[DEMonView].DEMGridToLatLongDegree(xgrid,ygrid,Lat,Long);
                  DrapingMaps[1].MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
                  if DrapingMaps[1].MapDraw.OnScreen(xp,yp) then Result := P0[1][yp]^[xp]
                  else Result := DEMGlb[DEMonView].RGBReflectanceColor(DrapingMaps[1].MapDraw.MapType, round(xgrid),round(ygrid));
               end
               else Result := DEMGlb[DEMonView].RGBReflectanceColor(DrapingMaps[1].MapDraw.MapType,round(xgrid),round(ygrid));
            end;
         {$EndIf}
      end;
   end;
end;


procedure TView3D.InitHidden;
begin
  New(YMin);
  New(YMinNew);
  FillChar(YMin^,SizeOf(HideArray),chr(127));
  FillChar(YMinNew^,SizeOf(HideArray),chr(127));
end;

procedure TView3D.CloseHidden;
begin
  Dispose(YMin);
  Dispose(YMinNew);
end;


destructor tView3d.Destroy;
begin
   inherited;
end;


procedure tView3d.InitializeViewParameters;
begin
   {$IfDef ExFly}
   {$Else}
      FlyOpts := MDDef.FlyOptions;
   {$EndIf}

   ObsElev := 0;
   DEMOnView := 0;
   TargetXUTM := 0;
   TargetYUTM := 0;
   MaxPitch := -999;
   DefVertExag := -1;
   FirstFrame := true;
   TargetRun := false;
   ForceElevAngle := false;
   PersOpts := MDDef.PerspOpts;
   StraightLineAlgorithm := saVincenty;
   RedrapeEachFrame := true;
   DrapeDisplay := DrapeReflectance;
end;


constructor tView3d.Create;
begin
   {$IfDef RecordView3dCreate} WriteLineToDebugFile('tView3d.Create in'); {$EndIf}
   FirstDraw := true;
   RegionalDEM := 0;

   {$IfDef VCL}
      DrapingMaps[1] := Nil;
      DrapingMaps[2] := Nil;
      DrapedBMP[1] := Nil;
      DrapedBMP[2] := Nil;
   {$EndIf}

   FlightRouteDB := Nil;
   FlightDBName := '';
   SaveName := 'FLYT';
   InitializeViewParameters;
   {$IfDef RecordView3dCreate} WriteLineToDebugFile('tView3d.Create out, perswidth=' + IntToStr(PersOpts.PersWidth) + '  persheight=' + IntToStr(PersOpts.PersHeight)); {$EndIf}
end;



procedure tView3D.SeriesNumberCoord(Prof : integer);
var
   i : integer;
begin
  {$IfDef RecordShortFlySequence}
     if (FlightRouteDB = Nil) then WriteLineToDebugFile('tView3D.SeriesNumberCoord in , no FlightRouteDB')
     else WriteLineToDebugFile('tView3D.SeriesNumberCoord in , FlightRouteDB present');
  {$EndIf}
  if (FlightRouteDB = Nil) then exit;
   if (FlightRouteDB <> Nil) then begin
      ViewerLat := FlightRouteDB.GetFieldByNameAsFloat('LAT');
      ViewerLong := FlightRouteDB.GetFieldByNameAsFloat('LONG');
      ViewAzimuth := FlightRouteDB.GetFieldByNameAsFloat('AZIMUTH');
      MaxPitch := FlightRouteDB.GetFieldByNameAsFloat('PITCH') + 0.5 * ViewVFOV;
      MinPitch := MaxPitch - ViewVFOV;
   end;

   FlightAzimuth := ViewAzimuth;
   SetAzimuthLimits;

   {$IfDef RecordFlySequence}
      WriteLineToDebugFile('View start center: '+ LatLongDegreeToString(ViewerLat,ViewerLong) + '   zg='+RealToString(ElevObs,8,0)+'   zobs='+RealToString(ObserverElevation,8,0));
      WriteLineToDebugFile('Horiz  angle range: ' + RealToString(LeftAzimuth,8,3) + '  to ' +  RealToString(RightAzimuth,8,3)) ;
      WriteLineToDebugFile('SNC 2, viewer=' + LatLongDegreeToString(ViewerLat,ViewerLong,MDDef.OutPutLatLongMethod));
   {$EndIf}

   if (FlightRouteDB <> Nil) and (FlightRouteDB.RecordCount > 1) then begin
      ObsElev := FlightRouteDB.GetFieldByNameAsFloat('ALTITUDE');
      DistanceAlongRoute := FlightRouteDB.GetFieldByNameAsFloat('DISTANCE');
      FlightRouteDB.Next;
   end;

  {$IfDef RecordFlySequence}
     WriteLineToDebugFile('DEMonView: ' + IntToStr(DEMonView) + '   SNC 3, z=' + RealToString(ObserverElevation,-8,1));
     WriteLineToDebugFile('viewer: ' + LatLongDegreeToString(ViewerLat,ViewerLong) + '  viewer depth: ' + RealToString(ViewDepth,-12,1) + '  viewer azimuth: ' + RealToString(ViewAzimuth,-12,1));
     WriteLineToDebugFile('HFOV: ' + RealToString(ViewHFOV,-12,2) + '  VFOV: ' + RealToString(ViewVFOV,-12,2));
  {$EndIf}

   VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,ViewAzimuth,ViewedLat,ViewedLong);

   ViewWidth := ViewDepth * sinDeg(ViewHFOV * 0.5) / cosDeg(ViewHFOV * 0.5);

  {$IfDef RecordFlySequence} WriteLineToDebugFile('SNC 4'); {$EndIf}

   {$IfDef VCL}
   if (DrapeMapUsing <> 0) and (DrapingMaps[DrapeMapUsing] <> Nil) and (DrapingMaps[DrapeMapUsing].MapDraw.ValidSatOnMap) then
      {$IfDef ExSat}
      {$Else}
          with SatImage[DrapingMaps[DrapeMapUsing].MapDraw.SatOnMap] do begin
             for i := 0 to 2 do RadialPointSpacing[i] := MetersPerPixel;
         end
      {$EndIf}
   else {$EndIf} begin
      for i := 0 to 2 do RadialPointSpacing[i] := (DEMGlb[DEMonView].AverageXSpace);
   end;

   {$IfDef RecordFlySequence}
      WriteLineToDebugFile('tView3D.SeriesNumberCoord,  Draw number '+ IntToStr(Prof));
      WriteLineToDebugFile('viewer depth: ' + RealToString(ViewDepth,-12,1) + '  viewer azimuth: ' + RealToString(ViewAzimuth,-12,1));
      WriteLineToDebugFile('Target coordinates:'+RealToString(TargetXUTM,8,0) +RealToString(TargetYUTM,10,0));
      WriteLineToDebugFile('View start center: '+ LatLongDegreeToString(ViewerLat,ViewerLong) + '   zg='+RealToString(ElevObs,8,0)+'   zobs='+RealToString(ObserverElevation,8,0));
      WriteLineToDebugFile('View end center  :'+LatLongDegreeToString(ViewedLat,ViewedLong));
   {$EndIf}
end;


function TView3D.FindPitch(Elev1,DistOut : float64) : float64;
begin
   Result := arcTan( -(ObsElev - (Elev1 - DropEarthCurve(DistOut))) / DistOut) / DegToRad;
end;

procedure TView3D.FindProfilePoint(DistOut : float64; var xr,yr,Elev1,ThisPitch : float64);
var
   z : float32;
begin
   xr := XGridRight + (DistOut/ViewDepth) * (LeftRearGridX - XGridRight);
   yr := YGridRight + (DistOut/ViewDepth) * (LeftRearGridY - YGridRight);
   DEMGlb[DEMonView].GetElevMeters(xr,yr,z);
   Elev1 := z;
   ThisPitch := FindPitch(Elev1,DistOut);
end;


procedure TView3D.BackCorner(LastX : integer;  var Azimuth : float32; var ViewedLat,ViewedLong,LeftRearGridX,LeftRearGridY  : float64);
begin
   if (DEMonView <> 0) and (DEMGlb[DEMonView] <> Nil) then begin
      ScreenToAzimuth(LastX,Azimuth);
      VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,Azimuth,ViewedLat,ViewedLong);
      DEMGlb[DEMonView].LatLongDegreetoDEMGrid(ViewedLat,ViewedLong,LeftRearGridX,LeftRearGridY);
   end;
end;


procedure TView3D.ViewPortBackCorners(var LeftRearGridX,LeftRearGridY,RightRearGridX,RightRearGridY : float64);
var
   Lat,Long : float64;
begin
   VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,LeftAzimuth,Lat,Long);
   DEMGlb[DEMOnView].LatLongDegreeToDEMGrid(Lat,Long,LeftRearGridX,LeftRearGridY);
   VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,RightAzimuth,Lat,Long);
   DEMGlb[DEMOnView].LatLongDegreeToDEMGrid(Lat,Long,RightRearGridX,RightRearGridY);
   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('TView3D.ViewPortBackCorners, Left side az: ' + RealToString(LeftAzimuth,-12,2) + '   Right side az: ' + RealToString(RightAzimuth,-12,2)); {$EndIf}
end;


procedure TView3D.AzimuthToScreen(Azimuth : float32; var x : integer);
begin
   x := round(((Azimuth - LeftAzimuth) / ViewHFOV) * ViewportWidth);
end;


procedure TView3D.PitchToScreen(Pitch : float32; var y : integer);
begin
   y := ViewportHeight - round((ViewportHeight) * (Pitch - MinPitch) / ViewVFOV);
end;


procedure TView3D.AzimuthPitchToScreen(Azimuth,Pitch : float32; var x,y : integer);
begin
   AzimuthToScreen(Azimuth,x);
   PitchToScreen(Pitch,y);
end;


procedure TView3D.ScreenToAzimuth(x: integer; var Azimuth : float32);
begin
   if (X > -1) then begin
      Azimuth := LeftAzimuth + (X / pred(ViewPortWidth) * ViewHFOV);
      Azimuth := CompassAngleInRangeFloat32(Azimuth);
   end;
end;


procedure TView3D.ScreenToPitch(y : integer; var Pitch : float32);
begin
   Pitch := MaxPitch - (Y / pred(ViewPortHeight) * ViewVFOV);
end;


procedure TView3D.ScreenToAzimuthPitch(x,y : integer; var Azimuth,Pitch : float32);
begin
   ScreenToAzimuth(x,Azimuth);
   ScreenToPitch(y,Pitch);
end;


procedure TView3D.PerspectiveDrawStrip(FirstXStrip,LastXStrip : integer; BMPMemory : tBMPMemory; ShowProgress : boolean);
var
   Elev1,BlockPitch,
   LastDistOut : float64;
   ThisPitch,
   Azimuth                 : float32;
   Shift,Runs,
   i,y,ip,iy,
   Min,FirstPt,Pt,PtInc                    : integer;
   Valid,Quicky                 : boolean;
   elevs,fxgrids,fygrids,fdists : ^bfarray32;
   RGBtriple : tPlatformColor;
begin
   {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('TView3D.PerspectiveDrawStrip in, azimuith=' + RealToString(LeftAzimuth,-8,-1) + ' ' + RealToString(RightAzimuth,-8,-1)); {$EndIf}
   new(fxgrids);
   new(fygrids);
   new(fdists);
   new(elevs);
   Quicky := (DrapeDisplay in [QuickDrapeReflectance,DrapeReflectance]) and (not MDdef.AviationDangerColors);

   for i := FirstXStrip to LastXStrip do begin
      {$IfDef RecordInnerLoopPerspective} WriteLineToDebugFile('i: ' + IntToStr(i)); {$EndIf}

      inc(BMPMemory.NumDone);

      TInterlocked.Increment(BMPMemory.NumDone);
      {$IfDef VCL}
         if (BMPMemory.NumDone mod 100 = 0) and ShowProgress then begin
            UpdateProgressBar(BMPMemory.NumDone / BMPMemory.NumToDo);
         end;
      {$EndIf}

      Min := ViewportHeight;
      Azimuth := LeftAzimuth + i * (RightAzimuth - LeftAzimuth) / LastXStrip;
      VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,Azimuth,ViewedLat,ViewedLong);

      {$IfDef RecordPerspective} WriteLineToDebugFile(IntToStr(i) + ' az=' + RealToString(Azimuth,-8,-1) + ' ' + LatLongDegreeToString(ViewedLat,ViewedLong) ); {$EndIf}
      DEMGlb[DEMonView].GetStraightRouteLatLongWithElevs(ViewerLat,ViewerLong,ViewedLat,ViewedLong,NumRadialPts,fxgrids^,fygrids^,fdists^,elevs^);
      FirstPt := 0;
      while (fDists^[FirstPt] < PersOpts.PersFirstProfile) do inc(FirstPt);
      LastDistOut := 50000;
      Pt := FirstPt;
      PtInc := 1;
      BlockPitch := -99;

      while (Pt <= NumRadialPts) do begin
         if (Elevs^[Pt] < 32000) then begin
            ThisPitch := FindPitch(Elevs^[Pt],fDists^[Pt]);
            if ThisPitch > BlockPitch then begin
               BlockPitch := ThisPitch;

               PitchToScreen(ThisPitch,y);

               {$IfDef RecordInnerLoopPerspective}
                  WriteLineToDebugFile('Viewport height: ' + IntToStr(ViewPortHeight) + 'PersMaxVertAngle: ' + RealToString(PersOpts.PersMaxVertAngle,12,4));
                  WriteLineToDebugFile('PersOpts.PersVFOV: ' + RealToString( ViewVFOV,12,4));
                  WriteLineToDebugFile('ThisPitch: ' + RealToString(ThisPitch,12,4) + '  y:' + IntToStr(y));
               {$EndIf}

               if (y < Min) and (y >= 0) then begin
                  RGBtriple := DrapedRGBtriple(fXGrids^[Pt],fYGrids^[Pt],DrapeDisplay,Valid);
                  if Valid then begin
                     if Quicky then begin
                        for ip := y to pred(Min) do begin
                           BMPMemory.SetPixelColor(i,ip,RGBTriple);
                        end;
                     end
                     else begin
                        for ip := y to pred(Min) do begin
                           if ((NumRuns = 1) and (MaxShift = 0)) or (PersOpts.PerpsectiveStereo in [psAnaglyphDual,psStereoPair]) then begin
                              if DrapeDisplay in [QuickDrapeReflectance,DrapeReflectance] then begin
                                 if MDdef.AviationDangerColors then begin
                                    if (Elev1 > ObsElev) then begin
                                       ModifyRGBColor( RGBTriple,-99,0,0);
                                    end
                                    else if (Elev1 > (ObsElev - 500 * FeetToMeters)) then begin
                                       ModifyRGBColor( RGBTriple,-99,-99,0);
                                    end;
                                 end;
                              end;
                              BMPMemory.SetPixelColor(i,ip,RGBTriple);
                           end
                           else begin
                              {$IfDef VCL}
                                   if (NumRuns = 2) then begin
                                      if not SameColor(BMPMemory.p1[ip][i],rgbTripleWhite) then  begin
                                         if (Runs = 1) then  begin
                                            BMPMemory.p1[ip][i].rgbtGreen := RGBtriple.rgbtGreen;
                                            BMPMemory.p1[ip][i].rgbtBlue := RGBtriple.rgbtBlue;
                                         end
                                         else BMPMemory.p1[ip][i].rgbtRed := RGBtriple.rgbtRed;
                                      end;
                                   end
                                   else begin
                                      if (MaxShift > 0) then  begin
                                         Shift := MaxShift - round(fDists^[Pt]/ViewDepth * MaxShift);
                                         if (i + shift <= BMPMemory.BMPHeight) then BMPMemory.p1[ip][i+Shift].rgbtRed := RGBtriple.rgbtRed;
                                      end
                                      else BMPMemory.p1[ip][i].rgbtRed := RGBtriple.rgbtRed;
                                      BMPMemory.p1[ip][i].rgbtGreen := RGBtriple.rgbtGreen;
                                      BMPMemory.p1[ip][i].rgbtBlue := RGBtriple.rgbtBlue;
                                   end;
                              {$EndIf}
                           end;
                        end;
                     end;
                  end;

                  if PersOpts.OutLineCrests and (fDists^[Pt] > LastDistOut + PersOpts.CrestSeparator) and (PersOpts.PerpsectiveStereo in [psNone]) then begin
                     for iy := 0 to pred(PersOpts.CrestLineWidth) do if (i-iy > 0) then begin
                        BMPMemory.SetPixelColor(i,ip-iy,PersOpts.CrestColor);
                     end;
                  end;
                  LastDistOut := fDists^[Pt];
                  Min := y;
               end;
            end;

         end;
         if PersOpts.PersVaryResolutionAlongRadial then begin
            for ip := 1 to 4 do
               if (fDists^[Pt] > PersOpts.PersVaryResRanges[ip]) then PtInc := PersOpts.PersVaryResFactors[ip];
         end;
         inc(Pt,PtInc);
         if (Min = 0) then break;
      end {while};
      if WantOut then break;
   end {for i};

   Dispose(fxgrids);
   Dispose(fygrids);
   Dispose(fdists);
   Dispose(elevs);
end;


function TView3D.PerspectiveDraw(var Bitmap : tMyBitmap; ShowProgress : boolean) : boolean;
var
   Dist,
   SaveViewerLat,SaveViewerLong,SaveViewAzimuth,
   VertExag,
   Azimuth                 : float64;
   LastX,EveryProfileHilite,
   Runs,xinc,x1,x2,i,x,y : integer;
   TStr,ViewCaption : ShortString;
   FastScreen : array[1..2] of tBMPMemory;
   {$IfDef ExStereo}
   {$Else}
      LeftName,RightName : PathStr;
   {$EndIf}


   procedure LabelViewport;
   var
      Az,dAz,j,ElevRange : integer;
   begin
      {$IfDef RecordPerspLabel} WriteLineToDebugFile('LabelViewport left=' + RealToString(LeftAzimuth,-4,0) + '  right=' + RealToString(RightAzimuth,-4,0)); {$EndIf}

      {$IfDef VCL} LoadMyFontIntoWindowsFont(MDDef.PerspFont,Bitmap.Canvas.Font);  {$EndIf}

     {vertical angles}
     ElevRange := round(ViewVFOV);
     j := MDDef.PerspOpts.VertLabelInc;
     i := j * succ(round(MaxPitch) div j);

      while (i mod j) <> 0 do inc(i);

      if (ElevRange > 0) then begin
         while (i > MinPitch) do begin
            y := BitMap.Height - round((BitMap.Height) * (i - MinPitch) / (ViewVFOV));
            TStr := IntToStr(i)+'°';
            {$IfDef RecordPerspLabel} WriteLineToDebugFile('i=' + IntToStr(i) + '  '  + 'x=' + x.ToString  + '  '  + 'y=' + y.ToString + ' ' + TStr);     {$EndIf}
            if (y > 6) then begin
               BitmapTextOut(Bitmap,12,y-5,TStr);
            end;
            PetImage.DrawLine(Bitmap,0,y,8,y);
            PetImage.DrawLine(Bitmap,BitMap.Width,y,BitMap.Width-8,y);
            dec(i,j);
         end {while};
      end;

      dAz := round(ViewVFOV) div (Bitmap.Width div Bitmap.Canvas.TextWidth('360D'));
      if dAz < 1 then dAz := 1;

      LastX := -999;
      Az := dAz * (trunc(LeftAzimuth / dAz));
      while (Az < RightAzimuth) do begin
         x := round(((Az - (LeftAzimuth)) / ViewHFOV) * Bitmap.Width);
         if (Az < 0) then Str(Az+360,TStr)
         else if (Az > 360) then Str(Az-360,TStr)
         else Str(Az,TStr);
         PetImage.DrawLine(Bitmap,x,0,x,8);
         TStr := TStr + '°';
         {$IfDef RecordPerspLabel} WriteLineToDebugFile('x=' + IntToStr(x) + '  '  + 'Az=' + RealToString(Az,-12,1) + '  ' + TStr); {$EndIf}
         if (x > 12) and (x > (LastX + Bitmap.Canvas.TextWidth(TStr))) then begin
            BitmapTextOut(Bitmap,x-8,7,TStr);
            LastX := x + round(Bitmap.Canvas.TextWidth(TStr));
         end;
         Az := Az + dAz;
      end {while};
   end {proc LabelViewPort};


   procedure ComputeCorners;
   var
      Lat,Long : float64;
      {$IfDef RecordMapDraw} i : integer; {$EndIf}
   begin
      {back corners of viewport}
      ViewPortBackCorners(LeftRearGridX,LeftRearGridY,RightRearGridX,RightRearGridY);
      xu[1] := LeftRearGridX;
      yu[1] := LeftRearGridY;

      xu[2] := RightRearGridX;
      yu[2] := RightRearGridY;
      {front corners of viewport}
      VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,MDdef.PerspOpts.PersFirstProfile,RightAzimuth,Lat,Long);
      DEMGlb[DEMonView].LatLongDegreeToDEMGrid(Lat,Long,xu[3],yu[3]);

      VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,MDdef.PerspOpts.PersFirstProfile,LeftAzimuth,Lat,Long);
      DEMGlb[DEMonView].LatLongDegreeToDEMGrid(Lat,Long,xu[4],yu[4]);

      {$IfDef RecordMapDraw} for i := 1 to 4 do WriteLineToDebugFile('Corner ' + IntToStr(i) + ': ' + RealToString(xu[i],8,0) + RealToString(yu[i],8,0)); {$EndIf}
  end;


       {$IfDef VCL}
       procedure DrawFishnetPerspective;
       const
          MaxMesh = 1500;
       type
          MeshArray = array[-MaxMesh..MaxMesh] of integer;
       var
         Elev1 : float32;
         MinProfElev,
         MaxProfElev,xg,yg,ElevRange, pe0x,pe0y,pe1x,pe1y,dxg,dyg,p,
         IncrDist,DistOut,
         NextOut,
         xutm0,yutm0,xutm1,yutm1,
         xgl,ygl,xgr,ygr,
         DropCurve : float64;
         TieColor,RegColor,HiColor,LabelColor,TerCol : TColor;
         CountOut,ip,oxr,oyr,oxl,oyl,j,PixelPerPoint  : integer;
          i : integer;
          LastProfX,LastProfY,ThisProfX,ThisProfY   : ^MeshArray;
       begin
          {$IfDef RecordPerspective} WriteLineToDebugFile('DrawFishnetPerspective in'); {$EndIf}

           {$IfDef VCL}
             RegColor := clLime;
             HiColor := clTeal;
             LabelColor := clSilver;
           {$EndIf}

          New(LastProfX);
          New(LastProfY);
          New(ThisProfX);
          New(ThisProfY);
          InitHidden;
          for i := -MaxMesh to MaxMesh do begin
             LastProfX^[i] := MaxInt;
             LastProfY^[i] := MaxInt;
             ThisProfX^[i] := MaxInt;
             ThisProfY^[i] := MaxInt;
          end;
          if (MainInCh in ['C','V','1','F']) then PixelPerPoint := 1
          else begin
             PixelPerPoint := 4;
             for i := 0 to ViewPortWidth do HiddenSetPoint(Bitmap,i,ViewPortHeight,0);
          end;

          if (PersOpts.WhichPerspective = FishnetChromaPerspective) then begin
             with BitMap.Canvas do begin
                Pen.Color := clYellow;
                Rectangle(0,0,Bitmap.Width,Bitmap.Height);
                Brush.Color := clBlack;
                FloodFill(1,pred(Bitmap.height),clYellow,fsBorder);
                Pen.Color := clBlack;
                Rectangle(0,0,Bitmap.Width,Bitmap.Height);
                Font.Color := clWhite;
             end {with};
          end;
          ip := 1;
          p := PersOpts.PersFirstProfile;
         {loop with multiple profiles, each constant distance from observer}
          {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective pt 10'); {$EndIf}
          while (p <= ViewDepth) do begin
             if ShowProgress then UpdateProgressBar(p/ViewDepth);
             if WantOut then break;
             if (PersOpts.WhichPerspective = FishnetChromaPerspective) then begin
                TerCol := RainbowColorFunct(p,0,ViewDepth);
                tieColor := TerCol;
             end
             else begin
                TerCol := RegColor;
             end;

             pe0x := XGridRight + p / ViewDepth * (LeftRearGridX - XGridRight);
             pe0y := YGridRight + p / ViewDepth * (LeftRearGridY - YGridRight);
             pe1x := XGridRight + p / ViewDepth * (RightRearGridX - XGridRight);
             pe1y := YGridRight + p / ViewDepth * (RightRearGridY - YGridRight);

             dxg := (pe1x - pe0x) / pred(succ(ViewPortWidth) div PixelPerPoint);
             dyg := (pe1y - pe0y) / pred(succ(ViewportWidth) div PixelPerPoint);

             i := 0;
             MinProfElev := ObsElev + sinDeg(MinPitch) * p;
             MaxProfElev := ObsElev + sinDeg(MaxPitch) * p;
             ElevRange := MaxProfElev - MinProfElev;
             xg := pe0x;
             yg := pe0y;

             DEMGlb[DEMonView].DEMGridToUTM(pe0x,pe0y,xutm0,yutm0);
             DEMGlb[DEMonView].DEMGridToUTM(pe1x,pe1y,xutm1,yutm1);
             IncrDist := sqrt( sqr(xutm0 - xutm1) + sqr(yutm0 - yutm1)) / ViewPortWidth * PixelPerPoint;
             DistOut := PersOpts.PersMeshSpace;
             NextOut := 1.5 * PersOpts.PersMeshSpace;
             CountOut := 1;
             pe0x := 0.5 * (pe0x + pe1x);
             pe0y := 0.5 * (pe0y + pe1y);
             xgl := pe0x;
             ygl := pe0y;
             xgr := pe0x;
             ygr := pe0y;
             oxr := 0;
             oyr := 0;
             i := ViewportWidth div 2;
             j := ViewportWidth div 2;
             DropCurve := DropEarthCurve(p);

             repeat   {loop for points on constant distance profile}
                if DEMGlb[DEMonView].GetElevMeters(xgl,ygl,Elev1) then begin
                   Elev1 := Elev1 - DropCurve;
                   y := ViewportHeight - round(ViewportHeight * (Elev1 - MinProfElev) / ElevRange);
                   if (y < 0) then y := 0 else if (y > ViewPortHeight) then y := ViewPortHeight;
                   if (i <> j) then BresenhamDrawAlgorithm(Bitmap,oxl,oyl,i,y,TerCol,HiddenSetPoint);
                   if DistOut > NextOut then begin
                      ThisProfx^[-CountOut] := i;
                      ThisProfY^[-CountOut] := y;
                   end;
                   oxl := i;
                   oyl := y;

                   if DEMGlb[DEMonView].GetElevMeters(xgr,ygr,Elev1) then begin
                       Elev1 := Elev1 - DropCurve;
                       y := ViewportHeight - round(ViewportHeight * (Elev1 - MinProfElev) / ElevRange);
                       if (y < 0) then y := 0 else if (y > ViewPortHeight) then y := ViewPortHeight;
                       if (j <> i) then BresenhamDrawAlgorithm(Bitmap,oxr,oyr,j,y,TerCol,HiddenSetPoint);
                       if DistOut > NextOut then begin
                          ThisProfx[CountOut] := j;
                          ThisProfY[CountOut] := y;
                          NextOut := NextOut + PersOpts.PersMeshSpace;
                          inc(CountOut);
                          if (CountOut > MaxMesh) then begin
                             NextOut := 999999999.99;
                          end;
                       end;
                   end;
                  oxr := j;
                  oyr := y;
               end;

               dec(i,PixelPerPoint);
               xgl := xgl - dxg;
               ygl := ygl - dyg;
               inc(j,PixelPerPoint);
               xgr := xgr + dxg;
               ygr := ygr + dyg;
               DistOut := DistOut + IncrDist;
             until (j > ViewportWidth) or (i < 0);

             for i := -MaxMesh to MaxMesh do
                if LastProfX[i] < MaxInt then BresenhamDrawAlgorithm(Bitmap,LastProfX^[i],LastProfY^[i],ThisProfX^[i],ThisProfY^[i],TieColor,HiddenSetPoint);
             LastProfX^ := ThisProfX^;
             LastProfY^ := ThisProfY^;

             inc(ip);
             p := p + PersOpts.PersDistBetweenProf;
             YMin^ := YMinNew^;
          end {while};
          {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective pt 11'); {$EndIf}
          if PersOpts.TitlePerspectiveViewport then BitmapTextOut(Bitmap,35,BitMap.Height-5-Bitmap.Canvas.TextHeight(ViewCaption),ViewCaption + ',' + RealToString(PersOpts.PersDistBetweenProf,5,0) + ' m per profile');
          Dispose(LastProfX);
          Dispose(LastProfY);
          Dispose(ThisProfX);
          Dispose(ThisProfY);
          CloseHidden;
      end;
     {$EndIf}


    procedure TryDraping;
    var
       i,j : integer;
       Lat,Long,Lat1,Long1 : float64;
    begin
       {$IfDef VCL}
       if (DrapeMapUsing > 0) and (PersOpts.WhichPerspective in [BMPPerspective]) and (DrapingMaps[DrapeMapUsing] <> Nil) then begin
          DrapeDisplay := DrapeBMP;
          if (not MDdef.DrapeExactly) and RedrapeEachFrame then begin
             {find what data must be loaded for drape}
             HeapSort(4,xu);
             HeapSort(4,yu);
             DEMGlb[DrapingMaps[DrapeMapUsing].MapDraw.DEMonMap].DEMGridToLatLongDegree(xu[1],yu[1],Lat,Long);
             DEMGlb[DrapingMaps[DrapeMapUsing].MapDraw.DEMonMap].DEMGridToLatLongDegree(xu[4],yu[4],Lat1,Long1);
             {$IfDef ExSat}
             {$Else}
             if Not DrapingMaps[DrapeMapUsing].MapDraw.DEMMap then begin
                SatImage[DrapingMaps[DrapeMapUsing].MapDraw.SATonMap].LatLongDegreeToSatGrid(SatImage[DrapingMaps[DrapeMapUsing].MapDraw.SATonMap].BandForSize,Lat,Long,xu[1],yu[1]);
                SatImage[DrapingMaps[DrapeMapUsing].MapDraw.SATonMap].LatLongDegreeToSatGrid(SatImage[DrapingMaps[DrapeMapUsing].MapDraw.SATonMap].BandForSize,Lat1,Long1,xu[4],yu[4]);
             end;
             {$EndIf}
             if FirstFrame or (not (DrapingMaps[DrapeMapUsing].MapDraw.LatLongOnScreen(Lat,Long) and DrapingMaps[DrapeMapUsing].MapDraw.LatLongOnScreen(Lat1,Long1))) then
                   DrapingMaps[DrapeMapUsing].DrapeMapFromTwoGridCorners(xu[1],yu[1],xu[4],yu[4]);
             FirstFrame := false;
             {$IfDef RecordMapDraw} WriteLineToDebugFile('Redrape: ' + RealToString(xu[1],8,0) + RealToString(yu[1],8,0) + RealToString(xu[4],8,0) + RealToString(yu[4],8,0)); {$EndIf}
             for i := 1 to 2 do begin
               DrapedBmp[i].Free;
               DrapedBMP[i] := Nil;
             end;
          end;
          for i := 1 to 2 do if (DrapingMaps[i] <> Nil) then begin
             if not CopyImageToBitmap(DrapingMaps[i].Image1,DrapedBMP[i]) then begin
                if AnswerIsYes('Reduce drape map size for next attempt') then
                   GetNewBMPSize(MDDef.MaxDrapeXSize,MDdef.MaxDrapeYSize,'');
                Result := false;
                exit;
             end;
             {$IfDef RecordPerspective} DrapedBMP[i].SaveToFile(MDTempDir + 'drapemap.bmp'); {$EndIf}
             for j := 0 to pred(DrapedBMP[i].Height) do P0[i][j] := DrapedBMP[i].ScanLine[j];
          end;
       end;
       {$EndIf}
    end;

    procedure CloudsOrSky;
    var
       CloudBMP : tMyBitmap;
       i,ip,y : integer;
    begin
       if PersOpts.CloudBackground and FileExists(CloudBitmapName) then begin
          {$IfDef VCL}
            CloudBMP := LoadBitmapFromFile(CloudBitmapName);
            if (CloudBMP.Height < Bitmap.Height) and (CloudBMP.Width < Bitmap.Width) then
               Bitmap.Canvas.StretchDraw(Rect(0,0,pred(Bitmap.Width),pred(Bitmap.Height)),CloudBMP)
            else Bitmap.Canvas.Draw(0,0,CloudBMP);
            CloudBMP.Free;
          {$EndIf}
       end
       else begin
          {$IfDef VCL}
              for i := 0 to pred(Bitmap.Width) do begin
                 for ip := 0 to pred(Bitmap.height) do begin
                    if MDDef.PerspOpts.UsersSky then FastScreen[runs].p1[ip][i] := MDDef.PerspOpts.rgbtSky
                    else begin {blue sky}
                       y := 240 - round(240 - ip/(Bitmap.Height/2)*250 + Random(5));
                       if (y > 240) then y := 240 + Random(5);
                       if (y < 50) then Y := 50 + Random(5);
                       FastScreen[runs].p1[ip][i].rgbtBlue := y;
                       FastScreen[runs].p1[ip][i].rgbtRed := 0;
                       FastScreen[runs].p1[ip][i].rgbtGreen := 0;
                    end;
                 end;
              end;
          {$EndIf}
       end;
    end;


begin
   {$IfDef RecordPerspective}
      WriteLineToDebugFile('Enter TView3D.PerspectiveDraw');
      WriteLineToDebugFile('  Observer: ' +  LatLongDegreeToString(ViewerLat,ViewerLong) + '  Viewed: ' +  LatLongDegreeToString(ViewedLat,ViewedLong) + '  ObsElev=' + RealToString(ObsElev,-8,1));
      WriteLineToDebugFile('  ViewDepth=' + RealToString(ViewDepth,-8,0) + '  HFOV=' + RealToString(ViewHFOV,-8,2) + '  VFOV=' + RealToString(ViewVFOV,-8,2));
      WriteLineToDebugFile('  DrapeMapUsing=' + IntToStr(DrapeMapUsing));
      if ShowProgress then WriteLineToDebugFile('Showing progress');
   {$EndIf}

   DEMDef_Routines.SaveBackupDefaults;

   EveryProfileHilite := 5;

   if MDdef.PerspOpts.PerpsectiveStereo in [psAnaglyphDual,psStereoPair] then NumRuns := 2 else NumRuns := 1;

   if (NumRuns = 2) then begin
      SaveViewerLat := ViewerLat;
      SaveViewerLong := ViewerLong;
      SaveViewAzimuth := ViewAzimuth;
   end;
   DEMGlb[DEMonView].ReflectanceParams;

   tanMinElevAngle := tanDeg(MinPitch);
   tanMaxElevAngle := tanDeg(MaxPitch);

   if (DrapeMapUsing = 0) then NumRadialPts := round(ViewDepth / DEMGlb[DEMonView].AverageSpace)
   else NumRadialPts := round(ViewDepth / RadialPointSpacing[DrapeMapUsing]);
   if (NumRadialPts > MaxFArrayPts) then NumRadialPts := pred(MaxFArrayPts);

   if (FlightRouteDB = Nil) or (FlightRouteDB.RecordCount = 1) then begin
      DEMGlb[DEMonView].GetElevFromLatLongDegree(ViewerLat,ViewerLong,ElevObs);
      if PersOpts.NapEarth then begin
         ObsElev := round(PersOpts.PersObsUp + ElevObs);
      end
      else begin
         ObsElev := PersOpts.PerspAbsElev;
         while (ObsElev < ElevObs) do
            ReadDefault('Ground elev ' + RealToString(ElevObs,-8,1) + '; Observer elev', ObsElev);
      end;
   end;
   {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective pt 1.5'); {$EndIf}

   PointOnView := true;
   if (MDdef.PerspOpts.PersFirstProfile < 1) then MDdef.PerspOpts.PersFirstProfile := 1;

    MaxShift := 0;
    if MDdef.PerspOpts.PerpsectiveStereo in [psAnaglyphShift] then begin
       MaxShift := MDdef.PerspOpts.PerspAnaglyphShift;
    end;

    for Runs := 1 to NumRuns do begin
       {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective, start run=' + IntToStr(Runs)); {$EndIf}
       CreateBitmap(Bitmap, ViewportWidth,ViewportHeight);

       FastScreen[Runs] := tBMPMemory.Create(Bitmap);
       CloudsOrSky;

       if (NumRuns = 2) then begin  //this is a stereo view
          Azimuth := SaveViewAzimuth - 90;
          VincentyPointAtDistanceBearing(SaveViewerLat,SaveViewerLong,0.5*MDdef.PerspOpts.PerspAnaglyphSeperate,Azimuth,ViewerLat,ViewerLong);

          if MDDef.ConvergingViews then begin
             VincentyPointAtDistanceBearing(SaveViewerLat,SaveViewerLong,ViewDepth,SaveViewAzimuth,ViewedLat,ViewedLong);
             VincentyCalculateDistanceBearing(ViewerLat,ViewerLong,ViewedLat,ViewedLong,Dist,ViewAzimuth);
          end;

          {$IfDef RecordPerspective}
             WriteLineToDebugFile('Stereo Run=' + IntToStr(Runs) + '  ViewDepth=' + RealToString(ViewDepth,-8,0) + '  ViewAzimuth=' + RealToString(ViewAzimuth,-8,0));
             WriteLineToDebugFile('  Observer location: ' +  LatLongDegreeToString(ViewerLat,ViewerLong) + '  Viewed location: ' +  LatLongDegreeToString(ViewedLat,ViewedLong));
          {$EndIf}

          {$IfDef VCL}
          DEMGlb[DEMonView].SelectionMap.OutlinePerspectiveView(ViewHFOV,ViewerLat,ViewerLong,ViewDepth,ViewAzimuth,pmCopy);
          {$EndIf}
       end {if (NumRuns = 2)};

       ComputeCorners;

       {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective pt 4'); {$EndIf}

       TryDraping;

       {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective point 8'); {$EndIf}

       if (PersOpts.WhichPerspective in [BMPPerspective,ReflectancePerspective]) then begin
          DEMGlb[DEMonView].ReflectanceParams;
          {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective [BMPPerspective,ReflectancePerspective]'); {$EndIf}
          PerspectiveDrawStrip(0,pred(FastScreen[Runs].BMPWidth),FastScreen[Runs],ShowProgress);

          {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective pt 12.5'); {$EndIf}

          if PersOpts.NapEarth then TStr := RealToString(PersOpts.PersObsUp,6,1) + ' m up'
          else TStr := SmartDistanceMetersFormat(ObsElev) + ' elev';
          VertExag := (ViewHFOV / Bitmap.Width) / (ViewVFOV /Bitmap.Height);
          if (SpecifiedCaption = '') then ViewCaption := DEMGlb[DEMonView].DEMMapProj.PreferLocationString(ViewerLat,ViewerLong) + '    ' + TStr
          else ViewCaption := SpecifiedCaption;

          FastScreen[1].Destroy;

          {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective pt 13'); {$EndIf}

          {$IfDef ExStereo}
          {$Else}
             if (PersOpts.PerpsectiveStereo in [psStereoPair,psAnaglyphDual]) then begin
                LeftName := ImageDir + 'left-pers.bmp';
                RightName := ImageDir + 'right-pers.bmp';
                if (Runs = 1) then begin
                   Bitmap.SaveToFile(LeftName);
                   Bitmap.Free;
                end
                else begin
                   Bitmap.SaveToFile(RightName);
                   if (PersOpts.PerpsectiveStereo in [psStereoPair]) then begin
                      {$IfDef VCL}
                      ShowStereoPair(LeftName,RightName);
                      {$EndIf}
                   end
                   else begin
                      FreeAndNil(Bitmap);
                      Bitmap := AnaglyphFromTwoBitmaps(LeftName,RightName);
                   end;
                   VincentyPointAtDistanceBearing(SaveViewerLat,SaveViewerLong,ViewDepth,SaveViewAzimuth,ViewedLat,ViewedLong);
                   ViewerLat := SaveViewerLat;
                   ViewerLong := SaveViewerLong;
                   ViewAzimuth := SaveViewAzimuth;
                end;
             end;
          {$EndIf}
       end
       else begin  //fishnet perspective
         {$IfDef VCL}
            DrawFishnetPerspective;
         {$EndIf}
       end;
       {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective pt 15'); {$EndIf}
    end;

    {$IfDef FMX}
       Bitmap.Canvas.BeginScene;
       Bitmap.Canvas.Stroke.Color := clBlack;
       Bitmap.canvas.Stroke.Kind := TBrushKind.bkSolid;
       Bitmap.canvas.Stroke.Thickness := 1;
       Bitmap.Canvas.Fill.Color := TAlphaColors.Black;
    {$EndIf}

    if PersOpts.LabelPerspectiveViewport then LabelViewPort;
    {$IfDef VCL}
        if PersOpts.TitlePerspectiveViewport then begin
           x := 20 + Bitmap.Canvas.TextWidth('-200');
           BitmapTextOut(Bitmap,x,BitMap.Height-5-Bitmap.Canvas.TextHeight(ViewCaption),ViewCaption);
        end;
    {$EndIf}

    ComputeCorners;

    {$IfDef VCL}
       if (not MDdef.DrapeExactly) and RedrapeEachFrame then
           for i := 1 to 2 do begin
               DrapedBmp[i].Free;
               DrapedBMP[i] := Nil;
             end;
    {$EndIf}

    DEMDef_Routines.RestoreBackupDefaults;

    Result := true;

    {$IfDef FMX} Bitmap.Canvas.EndScene; {$EndIf}

    {$IfDef RecordPerspectiveProgress} WriteLineToDebugFile('DrawPerspective out'); {$EndIf}
end;


initialization
   {$IfDef MessageStartUpUnit} MessageToContinue('Startup dempersw'); {$EndIf}
   ResizingNow := false;
   NeedErrorString := false;
   GetRadial := false;
   MovieList := Nil;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing dem_3d_view in'); {$EndIf}
end.



