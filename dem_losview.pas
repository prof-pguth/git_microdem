unit dem_losview;


6/4/2019: this is not used any longer

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM freeware GIS   }
{ PETMAR Trilobite Breeding Ranch }
{    verified 7/30/2104           }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordLOSAlgorithm}
   //{$Define RecordPointClouds}
   //{$Define RecordUTMZones}
   {$Define RecordLOSProblems}
   //{$Define RecordLOSPrettyDrawing}
   //{$Define RecordRandomProfiles}
   //{$Define RecordWaveLenghtHeightProblems}
   //{$Define RecordAllLOSProblems}
   //{$Define RecordClosingProblems}
   //{$Define FormStyle}
   //{$Define RecordNearestCrest}
   //{$Define RecordThreadCrest}
   //{$Define RecordMGTProblems}
   //{$Define SaveIntermediateProfiles}
{$Endif}


{$IfDef ExGeology}
   {$Define ExMagAnom}
{$EndIf}


interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,Forms,
  System.UITypes,
  Dialogs,ExtCtrls, StdCtrls,ClipBrd,ComCtrls,Math,Menus, Buttons,
  {$IFDef ExPointClouds}
  {$Else}
  point_cloud_memory,
  {$EndIf}
  Petmar_types,PetDBUtils,PETMAR,DEMDefs,DEMLos_draw,DEMCoord,BaseGraf,PetImage;

type
  Tlosview = class
  private
    { Private declarations }
      procedure LOSExtremeElevationsAndScreenCoordinates(DEMToUse : integer);
      procedure RedrawLOS(WhichDEM,BHeight,BTop : integer; DrawSideTix : boolean = true);
      procedure SetLOSScreenSize(x,y : integer);
      procedure ClearProtractorTool;
      procedure DrawCollar(Bitmap: tMyBitmap);
      procedure DrawTheProfile(var Bitmap : tMyBitmap; FieldName : string12; Color : tColor; Width : integer; SecondName : string12 = ''; Mult : integer = 1);
      procedure RecalculateProfile;
      function IntervisibilitysummaryString(DoPopUp: boolean; var TerrainBlockage : boolean): ANSIstring;
      {$IfDef ExMagAnom}
      {$Else}
      procedure StartMagModel;
      procedure CreateMarineMagneticAnomaliesBitmap(var Bitmap : tMyBitmap; Wide,High : integer);
      {$EndIf}
     {$IFDef ExPointClouds}
     {$Else}
      procedure LoadProfilePointCloud(ShowLoadProgress : boolean = true);
      procedure OverlayProfilePointCloud(var Bitmap : tMyBitmap);
      {$EndIf}
      {$IfDef RecordLOSProblems}
      procedure ScreenDimensions(Where : ANSIString);
      {$EndIf}
      procedure DoIntervisibilityFromDataBase;
    procedure Adjustazimuth1Click(Sender: TObject);
    procedure Adjustrange1Click(Sender: TObject);
    procedure Algorithmanalysis1Click(Sender: TObject);
    procedure AllopenDEMs1Click(Sender: TObject);
    procedure Averageelevation1Click(Sender: TObject);
    procedure Copyimagetoclipboard1Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Copytoclipboard2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Intervisibilitysummary1Click(Sender: TObject);
    procedure Linesizeandcolors1Click(Sender: TObject);
    procedure LOSParameters1Click(Sender: TObject);
    procedure MagneticModel1Click(Sender: TObject);
    procedure Measureslope1Click(Sender: TObject);
    procedure Modify1Click(Sender: TObject);
    procedure OpenGLofPointCloud1Click(Sender: TObject);
    procedure Openprofiledatabase1Click(Sender: TObject);
    procedure Parallelprofiles1Click(Sender: TObject);
    procedure Pastefromclipboard1Click(Sender: TObject);
    procedure Profiledropdown1Click(Sender: TObject);
    procedure Profilelegends1Click(Sender: TObject);
    procedure Profilenames1Click(Sender: TObject);
    procedure ProfilesonotherDEMs1Click(Sender: TObject);
    procedure Protractor1Click(Sender: TObject);
    procedure RandomLOSanalyses1Click(Sender: TObject);
    procedure Saveimage2Click(Sender: TObject);
    procedure Saveimage3Click(Sender: TObject);
    procedure Saveprofileendpoints1Click(Sender: TObject);
    procedure Saveprofileendpoints2Click(Sender: TObject);
    procedure Sensorobserversiteblowup1Click(Sender: TObject);
    procedure SetImagesize1Click(Sender: TObject);
    procedure Spacinganalysis1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure WaveLengthHeight1Click(Sender: TObject);
  public
    { Public declarations }
     {$IFDef ExPointClouds}
     {$Else}
     LOSMemoryPointCloud : tTwoPointClouds;
     PtCldInUse : integer;
     {$EndIf}
     LOSVariety : tLOSVariety;

     {$IFDef ExVegDensity}
     {$Else}
     VegGridUsed : integer;
     {$EndIf}
     FormSectLenMeters : Float;
     RidgeLoc,LOSAzimuth,ObsGroundElev,TargetGroundElev,
     ObsGroundElevLAS,TargetGroundElevLAS,
     LatLeft,LongLeft,LatRight,LongRight,FormVertExag,
     FormScaleFac,DropCurve,ZoomLeft,ZoomRight,
     SliceDx,SliceDy,SliceRotate : float;
     PixLong,PixelsHigh,ProfileBot,StartLOSLeft,StartLOSDown,
     DEMOnView,ProfileDropDown,LOSHeight,LOSWidth,
     FresnelDB,FirstX,FirstY,MinAreaZ,MaxAreaZ : integer;
     CrossTrackProfile : BaseGraf.TThisBaseGraph;
     EraseFresnelDB,DragEdit,ZoomedDiagram,CompareDensityAlongProfile,
     EnvelopeDone,Closable,ActuallyDraw,RespondToResize   : boolean;
     ProtractorTool,Basebmp : tMyBitmap;

     ProfLats,ProfLongs : tLongArray;

     ProfileName : array[1..MaxDEMDataSets] of ShortString;
     TargetName,SensorName : shortstring;
     MultipleProfilesToShow : tStringList;
     p3 : tBMPMemory;

     procedure LineOfSightFromLatLong;
     procedure LOSScreenLocation(Dist,Elev : float; var xp,yp : integer);
     procedure MarineMagneticAnomalies;
     procedure ShowOnMap;
     procedure EnableAzimuthShifts;
     function FindNearestCrest(Distance : float; var Lat,Long,Height,LeftHt,LeftLength,RightHt,RightLength : float) : boolean;
     procedure FindWaveLengthHeight(DisplayIt : boolean; var WavelengthMean,WavelengthMedian,WavelengthStdDev, HeightMean,HeightMedian,HeightStd : float; Memo1 : tMemo = nil);
    constructor Create;
end;


type
   tLOSFormDoing = (losNothing,losFirstSlope,losSecondSlope,losFresnelCrossSection);
var
   BlockIntensity : integer;
   SpreadRidge,TimeLeft,TimeRight,
   MagI,MagAlpha : float;
   MagTimeScale,MagAnomNames : boolean;
   LOSFormDoing : tLOSFormDoing;
   SavedMapImage : tMyBitmap;


implementation

uses
   PetGraphColors,
   {$IfDef ExGIS}
   {$Else}
   demdatabase,
   {$EndIf}
   DEMLOSOp,DEMDef_routines,
   DEMDatum,
   DEMRange,
   Thread_timers,
   Make_tables,  basemap,
   PETMath, DataBaseCreate,

{$IfDef ExPointClouds}
{$Else}
   Point_Cloud_Options, Las_Lidar,slicer_3d,
{$EndIf}

{$IfDef ExOpenGL}
{$Else}
   ogl_DEMOpenGLMain,
{$EndIf}

{$IfDef ExViewshed}
{$Else}
   DEM_Fan_Compare,
{$EndIf}

   {$IfDef ExGeology}
   {$Else}
   GeologicTimeScale,
   mag_prof,
   {$EndIf}

   {$IfDef ExGeoStats}
   {$Else}
   demssocalc, DEMStat,
   {$EndIf}

{Main program MDI window for different programs that use this module}
   Nevadia_Main, petimage_form;
{End of MDI parent declaration}


var
   Symbol : tSymbols15;
   LineColors256 : tColors256;
   LineSize256 :  tbytes256;
   LastX,LastY : integer;

   {$IfDef ExGeology}
   {$Else}
   PickMagProfVars : TPickMagProfVars;
   {$EndIf}




procedure tLOSview.DrawCollar(Bitmap : tMyBitmap);
var
   x,yp1,TickElev,YPic,LastXP,TickInt,xp,yp : integer;
   i,j,Long,Factor : float;
   TStr,TStr2 : shortString;
   First : boolean;
begin
    LoadMyFontIntoWindowsFont(MDDef.LOSfont,Bitmap.Canvas.Font);
   {draw elevation lines across screen}

   {$IfDef RecordLOSProblems}
   ScreenDimensions('tLOSview.DrawCollar in');
   {$EndIf}

   {$IfDef RecordLOSPrettyDrawing}
   WriteLineToDebugFile('tLOSview.DrawCollar, bitmap size= ' + IntToStr(Bitmap.Width) + 'x' + IntToStr(Bitmap.Height),true);
   {$EndIf}

   if MDdef.ElevDisplayMeters then Factor := 1
   else Factor := 1 / FeetToMeters;

   TickInt := round(GetTickInt(PixelsHigh,round((MaxAreaZ-MinAreaZ) * Factor),Bitmap.Canvas.TextHeight('089')));
   if (TickInt = 0) then TickInt := 1;

   {$IfDef RecordLOSPrettyDrawing}
   WriteLineToDebugFile('Tick Elev range: ' + IntToStr(MinAreaZ) + ' to ' + IntToStr(MaxAreaZ));
   WriteLineToDebugFile('TickInt=' + IntToStr(TickInt));
   {$EndIf}

   if not ZoomedDiagram then begin
      ZoomRight := FormSectLenMeters;
      ZoomLeft := 0;
   end;
   Long := ZoomRight - ZoomLeft;

   if MDDef.EnglishDistanceUnits = disEnglish then begin
      Long := Long * 0.001 * Km2Miles;
   end
   else if MDDef.EnglishDistanceUnits = disNautical then begin
      Long := Long * 0.001 * Km2NauticalMiles;
   end
   else begin
      if MDDef.LOSHorizTickSpacing_KM then begin
         if (Long > 2500) then Long := Long * 0.001
         else MDDef.LOSHorizTickSpacing_KM := false;
      end;
   end;

   Bitmap.Canvas.Pen.Color := clBlack;
   Bitmap.Canvas.Pen.Width := MDDef.FrameLineWidth;

   TickElev := round(MaxAreaZ * Factor) - (round(MaxAreaZ * Factor) mod TickInt);
   First := true;
   while (TickElev >= MinAreaZ * Factor) do begin
      {$IfDef RecordLOSPrettyDrawing}
      WriteLineToDebugFile('z=' + IntToStr(TickElev));
      {$EndIf}
      for x := 0 to PixLong do begin
        if (x mod 3 in [0,1]) then begin
            LOSScreenLocation(ZoomLeft + (Long * x/PixLong),TickElev/Factor,xp,yp);
            Bitmap.Canvas.Pixels[xp,yp] := clTeal;
            if (x = 0) then yp1 := yp;
         end;
      end;
     {draw ticks on sides profile}
      {left side}
      Bitmap.Canvas.MoveTo(0,YP1);  Bitmap.Canvas.LineTo(StartLOSLeft,YP1);

      {right side}
      LOSScreenLocation(ZoomRight,TickElev,xp,yp);
      Bitmap.Canvas.MoveTo(xp,YP);   Bitmap.Canvas.LineTo(xp+10,YP);
      {$IfDef RecordLOSPrettyDrawing}
      WriteLineToDebugFile('  right side=' + IntToStr(yp));
      {$EndIf}
      {label right side ticks}
      if not First then Bitmap.Canvas.TextOut(xp+15,Yp-Bitmap.Canvas.TextHeight('8') div 2,IntegerToString(TickElev,4));
      First := false;
      dec(TickElev,TickInt);
   end {while};

   if DEMGlb[DEMOnView].ElevationDEM then begin
      if MDdef.ElevDisplayMeters then TStr := 'meters'
      else TStr := 'feet';
   end
   else TStr := LabelElevUnits[DEMGlb[DEMOnView].HeadRecs.ElevUnits];
   Bitmap.Canvas.TextOut(StartLOSLeft + PixLong + 20,StartLOSDown-20,TStr);

   if MDDef.ForceLOSHorizTickSpacing then j := MDDef.LosHorizTick
   else begin
      if Long < 0.25 then j := 0.01
      else if Long < 0.75 then j := 0.02
      else if Long < 1.5 then j := 0.05
      else if Long < 2.5 then j := 0.1
      else if Long < 5 then j := 0.25
      else if Long < 25 then j := 1
      else if Long <   50.0 then j := 2
      else if Long <  100.0 then j := 5
      else if Long <  250.0 then j := 10
      else if Long <  500.0 then j := 50
      else if Long < 1000.0 then j := 100
      else if Long < 2500.0 then j := 250
      else j := 500;
   end;

   LastXP := -999;

   if ZoomedDiagram then i := ZoomLeft
   else i := 0;
   repeat
      LOSScreenLocation(i,0,xp,yp);
      Bitmap.Canvas.MoveTo(XP,ProfileBot);
      Bitmap.Canvas.LineTo(XP,ProfileBot+8);

      if MDDef.EnglishDistanceUnits  = disEnglish then begin
         TStr := RealToString(0.001 * i * Km2Miles,-8,-2)
      end
      else if MDDef.EnglishDistanceUnits = disNautical then begin
         TStr := RealToString(0.001 * i * Km2NauticalMiles,-8,-2)
      end
      else begin
         if MDDef.LOSHorizTickSpacing_KM then TStr := RealToString(0.001 * i,-8,-2)
         else TStr := RealToString(i,-8,-2);
      end;
      XP := xp - Bitmap.Canvas.TextWidth(TStr) div 2;
      if xp < 2 then xp := 2;

      if MDDef.EnglishDistanceUnits  = disEnglish then begin
         i := i + 1000 * j / Km2Miles;
      end
      else if MDDef.EnglishDistanceUnits  = disNautical then begin
         i := i + 1000 * j / Km2NauticalMiles;
      end
      else begin
         if MDDef.LOSHorizTickSpacing_KM then i := i + 1000 * j
         else i := i + j;
      end;

      if (i > ZoomRight) then begin
         if MDDef.EnglishDistanceUnits = disEnglish then TStr2 := ' miles'
         else if MDDef.EnglishDistanceUnits = disNautical then TStr2 := ' nm'
         else if MDDef.LOSHorizTickSpacing_KM then TStr2 := ' km'
         else TStr2 := ' m';
      end
      else TStr2 := '';
      if (XP < LastXP) then TStr := '';
      Bitmap.Canvas.TextOut(XP+3,ProfileBot + 10,TStr + TStr2);

      if (XP > LastXP) then begin
         LastXP := XP + Bitmap.Canvas.TextWidth(TStr) + 8;
      end;
   until (i > ZoomRight);

    {Draw Left side of profile}
    Bitmap.Canvas.MoveTo(StartLOSLeft,ProfileBot);
    LOSScreenLocation(ZoomLeft,ObsGroundElev,xp,yp);
    Bitmap.Canvas.LineTo(XP,YP);

    LOSScreenLocation(ZoomRight,TargetGroundElev,xp,yp);
    Bitmap.Canvas.MoveTo(XP,Yp);
    Bitmap.Canvas.LineTo(XP,ProfileBot);             //right side
    Bitmap.Canvas.LineTo(StartLOSLeft,ProfileBot);   //bottom of profile
    Bitmap.Canvas.Pen.Width := 1;
end {proc SideTix};



procedure tLOSview.LOSExtremeElevationsAndScreenCoordinates(DEMToUse : integer);
var
   PointElev : float;
   i : integer;
   dists : ^tLongArray;
begin
   {$IfDef RecordLOSProblems}
   WriteLineToDebugFile('tLOSview.LOSExtremeElevationsAndScreenCoordinates,  DEM=' + IntToStr(DEMtoUse),true);
   {$EndIf}
   if (DEMtoUse <> 0) then begin
      new(Dists);
      DEMGlb[DEMtoUse].GetStraightRouteLatLongDegree(LatLeft,LongLeft,LatRight,LongRight,MDDef.wf.StraightAlgorithm,PixLong,ProfLats,ProfLongs,dists^);
      For i := 0 to PixLong do begin
         if DEMGlb[DEMtoUse].GetElevFromLatLongDegree(ProfLats[i],ProfLongs[i],PointElev) then begin
            {$IfDef RecordLOSProblems}
            if (i mod 50 = 0) or (i = PixLong) then begin
               WriteLineToDebugFile( IntToStr(i) + '  ' + LatLongDegreeToString(ProfLats[i],ProfLongs[i],DecDegrees) + RealToString(PointElev,12,1));
            end;
            {$EndIf}
            Petmath.CompareValueToExtremes(round(PointElev),MinAreaZ,MaxAreaZ);
         end;
      end;
      Dropcurve := DropEarthCurve(FormSectLenMeters);
      FormVertExag := (FormSectLenMeters /PixLong)/ ((MaxAreaZ - MinAreaZ + Dropcurve) / PixelsHigh);
      FormScaleFac := 1.0 * (MaxAreaZ - MinAreaZ + Dropcurve) / (PixelsHigh-StartLOSDown);
      {$IfDef RecordLOSProblems}
      WriteLineToDebugFile('PixLong=' + IntToStr(PixLong));
      WriteLineToDebugFile('Dropcurve=' + RealToString(Dropcurve,-12,-2));
      WriteLineToDebugFile('FormVertExag=' + RealToString(FormVertExag,-12,-2));
      WriteLineToDebugFile('FormScaleFac=' + RealToString(FormScaleFac,-12,-2));
      {$EndIf}
      Dispose(Dists);
   end {with};
end;



procedure tLOSview.LineOfSightFromLatLong;
var
   i,j,DivFactor : integer;
   z   : float;
   Bitmap : tMyBitmap;
begin {proc LineOfSight}
  {$IfDef RecordLOSProblems}
   WriteLineToDebugFile('enter tLOSview.LineOfSightFromLatLong ' + TimeToStr(Now),true);
  {$EndIf}

   {$IfDef FormStyle}
   WriteLineToDebugFile('tLOSview.LineOfSightFromLatLong,true');
   if FormStyle = fsMDIChild then WriteLineToDebugFile('fsMDIChild form')
   else if Self.Visible then WriteLineToDebugFile('normal form, visible')
   else WriteLineToDebugFile('normal form, hidden');
   {$EndIf}

   DEMDatum.CalculateDistanceBearing(LatLeft,LongLeft,LatRight,LongRight,FormSectLenMeters,LOSAzimuth);
   if (FormSectLenMeters > 25000) then MDDef.LOSHorizTickSpacing_KM := true;


   if LOSVariety in [losThreeProfiles,losAllDEMs,losMagModel,losAllDEMDropDown,losSimpleMagModel,LOSSimpleOne] then begin
      if LOSVariety in [losAllDEMs,losAllDEMDropDown] then begin
         if (MultipleProfilesToShow = Nil) then begin
            if (MinAreaZ > 32000) then begin
               for j := 1 to NumDEMDataSetsOpen do if (DEMGlb[j] <> Nil) then begin
                  {$IfDef RecordLOSProblems}
                  WriteLineToDebugFile('Get elevations, DEM=' + IntToStr(j));
                  {$EndIf}
                  DEMonView := j;
                  LOSExtremeElevationsAndScreenCoordinates(j);
               end;
               if LOSVariety in [losAllDEMDropDown] then MinAreaZ := MinAreaZ - pred(NumDEMDataSetsOpen) * MDDef.DropDownPerProfile;
            end;
         end;
      end;

      {$IfDef ExMagAnom}
      {$Else}
      if (LOSVariety in [losMagModel,losSimpleMagModel]) then StartMagModel;
      {$EndIf}

      if LOSVariety in [losSimpleOne] then begin
        if DEMGlb[DEMonView].LatLongDegreeInDEM(LatLeft,LongLeft) and DEMGlb[DEMonView].LatLongDegreeInDEM(LatRight,LongRight) then begin
           RecalculateProfile;
           CloseAndNilNumberedDB(FresnelDB);
        end;
      end;


      if LOSVariety in [losAllDEMs,losAllDEMDropDown,losThreeProfiles] then begin
         if (MultipleProfilesToShow = Nil) then begin
            MultipleProfilesToShow := tStringList.Create;
            for i := 1 to MaxDEMDataSets do if (DEMGlb[i] <> Nil) then begin
               DEMonView := i;
               if DEMGlb[i].LatLongDegreeInDEM(LatLeft,LongLeft) and DEMGlb[i].LatLongDegreeInDEM(LatRight,LongRight) then begin
                  {$IfDef RecordLOSProblems}
                  WriteLineToDebugFile('Recalculate profile, DEM=' + IntToStr(i));
                  {$EndIf}
                   RecalculateProfile;
                   MultipleProfilesToShow.Add(GISdb[FresnelDB].DBFullName);
                   CloseAndNilNumberedDB(FresnelDB);
               end;
            end;
         end;
      end;

      if (LOSVariety in [losSimpleMagModel,losMagModel]) then begin
         for i := 1 to MaxDEMDataSets do if (DEMGlb[i] <> Nil) then begin
            if (LOSVariety in [losSimpleMagModel]) then begin
               if (i = 1) then j := DEMonView
               else begin
                   MarineMagneticAnomalies;
                   exit;
               end;
               DivFactor := 2;
            end;

            if (LOSVariety in [losMagModel]) then begin
               {$IfDef ExGeology}
               {$Else}
               case i of
                  1 : j := 1;
                  2 : j := 3;
                  3 : begin
                         MarineMagneticAnomalies;
                         exit;
                      end;
               end;
               DivFactor := 3;
               {$EndIf}
            end;
         end;
      end {for i};

      if (MultipleProfilesToShow <> Nil) then begin
         {$IfDef RecordLOSProblems}
         WriteLineToDebugFile('(MultipleProfilesToShow <> Nil)',true);
         {$EndIf}
         //CreateBitmap(Bitmap,Image1.Width,Image1.Height);
         if (WMDEM <> Nil) then Bitmap.Canvas.Font := wmdem.Font;
         DrawCollar(Bitmap);
         for i := 1 to MultipleProfilesToShow.Count do begin
            OpenNumberedGISDataBase(FresnelDB,MultipleProfilesToShow.Strings[pred(i)]);
            DrawTheProfile(Bitmap,'ELEV_M',LineColors256[i],LineSize256[i]);
            CloseAndNilNumberedDB(FresnelDB);
         end;
        {$IFDef ExPointClouds}
        {$Else}
         LoadProfilePointCloud;
         OverlayProfilePointCloud(Bitmap);
         {$EndIf}
         //Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;
      end;
      {$IfDef RecordLOSProblems}
      WriteLineToDebugFile('Left side:  ' + LatLongDegreeToString(LatLeft,LongLeft,DecDegrees));
      {$EndIf}
   end
   else with DEMGlb[DEMOnView],HeadRecs do begin
      LOSExtremeElevationsAndScreenCoordinates(DEMonView);
      z := MDDef.ObsAboveGround;
      if ( MDDef.TargetAboveGround > z) then z :=  MDDef.TargetAboveGround;

      {$IFDef ExVegDensity}
      {$Else}
      if (VegGrid[1] <> 0) then Z := Z + round(DEMGlb[VegGrid[1]].Headrecs.MaxElev);
      {$EndIf}

      MaxAreaZ := round(MaxAreaZ + z + 5);

      if (not ElevationDEM) or ((FormSectLenMeters > 250000.0) and (MDdef.CurvAlg <> vcNoCurvature) and
         (not AnswerIsYes('Confirm use ' +  CurvAlgName(MDdef.CurvAlg) +
            ' for profile length ' + SmartDistanceMetersFormat(FormSectLenMeters)))) then SetFlatProfile;

      {$IfDef RecordLOSProblems}
      WriteLineToDebugFile('LOS with Sectlen='+ RealToString(FormSectLenMeters,12,1));
      WriteLineToDebugFile('Elev range' + IntegerToString(MinAreaZ,8) + ' to' + IntegerToString(MaxAreaZ,8));
      WriteLineToDebugFile('Sides:  ' + LatLongDegreeToString(LatLeft,LongLeft,DecDegrees) + '  ' + LatLongDegreeToString(LatRight,LongRight,DecDegrees));
      {$EndIf}

      {$IfDef RecordLOSProblems}
      ScreenDimensions('Client size 5');
      {$EndIf}


      {$IfDef RecordLOSProblems}
      ScreenDimensions('Client size 6');
      {$EndIf}

   end; {with}

   {$IfDef FormStyle}
   WriteLineToDebugFile('tLOSview.LineOfSightFromLatLong out', true);
   if FormStyle = fsMDIChild then WriteLineToDebugFile('fsMDIChild form')
   else if Self.Visible then WriteLineToDebugFile('normal form, visible')
   else WriteLineToDebugFile('normal form, hidden');
   {$EndIf}

  {$IfDef RecordLOSProblems}
   WriteLineToDebugFile('exit tLOSview.LineOfSightFromLatLong ' + TimeToStr(Now),true);
  {$EndIf}
end {procedure LineOfSight};



procedure tLOSview.FindWaveLengthHeight(DisplayIt : boolean; var WavelengthMean,WavelengthMedian,WavelengthStdDev,
                  HeightMean,HeightMedian,HeightStd : float; Memo1 : tMemo = nil);
const
   FlatnessCutOff = 0.05;
var
   Findings : tStringList;
   NPts,Missing : integer;
   zs : ^Bfarray;
   ave,adev,sdev,svar,skew,curt,Min,Max,Median : float;
begin
   {$IfDef ExFresnel}
   WavelengthMean := 0;
   WavelengthMedian := 0;
   WavelengthStdDev := 0;
   HeightMean := 0;
   HeightMedian := 0;
   HeightStd := 0;
   {$Else}
   if (FresnelDB = 0) then exit;
   GISdb[FresnelDB].EmpSource.Enabled := false;
   FindWavelengthStats(GISdb[FresnelDB].MyData, WavelengthMean,WavelengthMedian,WavelengthStdDev,
         HeightMean,HeightMedian,HeightStd);

   if DisplayIt then begin
      Findings := tStringList.Create;
      Findings.Add(ptTrim(DEMGlb[DEMonView].AreaName));
      Findings.Add(LatLongDegreeToString(LatLeft,LongLeft,MDDef.OutPutLatLongMethod));
      Findings.Add(LatLongDegreeToString(LatRight,LongRight,MDDef.OutPutLatLongMethod));

      Findings.Add('Regions: ' + IntToStr(MDDef.PeakPitPostings) );  //+ '  (' + RealToString(MDDef.PeakPitPostings *
      Findings.Add('Min Wave Height (m): ' + RealToString(MDDef.MinWaveHeight,-18,2));
      Findings.Add('Computation disatnce (m): ' + IntToStr(MDdef.WavelengthCompDist));
      Findings.Add('Peaks needed: ' + IntToStr(MDDef.WaveHtValuesNeeded));
      Findings.Add(' ');

      AddToWaveSpacingHeightResults(Findings,WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd);
      Findings.Add(' ');

      GISdb[FresnelDB].MyData.ApplyFilter('');
      GISdb[FresnelDB].MyData.TotRecordsInDB := GISdb[FresnelDB].MyData.RecordCount;
      New(zs);

      GISdb[FresnelDB].MyData.ApplyFilter('SLOPE_2D > ' + RealToString(FlatnessCutOff,-18,2));
      Findings.Add('Positive slopes:' + RealToString(100 * GISdb[FresnelDB].MyData.RecordCount / pred(GISdb[FresnelDB].MyData.TotRecordsInDB),-12,2) + '%');
      GetFieldValuesInArrayLinkPossible(GISdb[FresnelDB].MyData,Nil,'','','SLOPE_2D',zs^,Npts,Missing,Min,Max);
      if (NPts > 1)  then begin
         moment(zs^,npts,ave,adev,sdev,svar,skew,curt);
         Median := Petmath.Median(zs^,npts);
         Findings.Add('   average slope:' + RealToString(ave,8,2));
         Findings.Add('   median slope:' + RealToString(Median,8,2));
      end;

      GISdb[FresnelDB].MyData.ApplyFilter('SLOPE_2D < -' + RealToString(FlatnessCutOff,-18,2));
      Findings.Add('Negative slopes:' + RealToString(100 * GISdb[FresnelDB].MyData.RecordCount / pred(GISdb[FresnelDB].MyData.TotRecordsInDB),-12,2) + '%');
      GetFieldValuesInArrayLinkPossible(GISdb[FresnelDB].MyData,Nil,'','','SLOPE_2D',zs^,Npts,Missing,Min,Max);
      if (NPts > 1)  then begin
         moment(zs^,npts,ave,adev,sdev,svar,skew,curt);
         Median := Petmath.Median(zs^,npts);
         Findings.Add('   average slope:' + RealToString(ave,8,2));
         Findings.Add('   median slope:' + RealToString(Median,8,2));
      end;

      Dispose(zs);

      GISdb[FresnelDB].MyData.ApplyFilter('SLOPE_2D > -' + RealToString(FlatnessCutOff,-18,2) + ' AND SLOPE_2D < ' + RealToString(FlatnessCutOff,-18,2));
      Findings.Add('Flat regions:' + RealToString(100 * GISdb[FresnelDB].MyData.RecordCount / pred(GISdb[FresnelDB].MyData.TotRecordsInDB),-12,2) + '%');

      if (Memo1 = Nil) then Petmar.DisplayAndPurgeStringList(Findings,DEMGlb[DEMonView].AreaName + ' Spacing and height')
      else Memo1.Lines := Findings;

      GISdb[FresnelDB].EmpSource.Enabled := true;
      GISdb[FresnelDB].MyData.ApplyFilter('');
   end;
   {$EndIf}
end;


function tLOSview.FindNearestCrest(Distance : float; var Lat,Long,Height,LeftHt,LeftLength,RightHt,RightLength : float) : boolean;
var
   d,dd,LastDD,MinD,z : float;
   Rec,WhereCrest,i,xp,yp : integer;
begin
   {$IfDef RecordNearestCrest}
   WriteLineToDebugFile('Enter tLOSview.FindNearestCrest for distance=' + RealToString(Distance,-12,4),true);
   {$EndIf}

   Result := false;
   if (FresnelDB = 0) then exit;
   GISdb[FresnelDB].EmpSource.Enabled := false;
   GISdb[FresnelDB].MyData.ApplyFilter('PEAK = ' + QuotedStr('Y'));

   LastDD := 99999;
   Rec := 0;
   while not GISdb[FresnelDB].MyData.eof do begin
      d := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
      dd := abs(d  - Distance);
      if (dd < LastDD) then begin
         Lat := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LAT');
         Long := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LONG');
         Height := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
         MinD := d;
         LastDD := dd;
         WhereCrest := Rec;
      end;
      GISdb[FresnelDB].MyData.Next;
      inc(Rec);
   end;

   {$IfDef RecordNearestCrest}
   WriteLineToDebugFile('Crest at dist=' + RealToString(MinD,-12,4) + '  with z=' + RealToString(Height,-12,-2));
   {$EndIf}

   LeftHt := 0;
   RightHt := 0;

   GISdb[FresnelDB].MyData.ApplyFilter('');
   LastDD := 99999;
   Rec := 0;
   while not GISdb[FresnelDB].MyData.eof do begin
      d := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
      dd := abs(d  - Distance);
      if (dd < LastDD) then begin
         LastDD := dd;
         WhereCrest := Rec;
      end;
      GISdb[FresnelDB].MyData.Next;
      inc(Rec);
   end;

   {$IfDef RecordNearestCrest}
   WriteLineToDebugFile('Right side',true);
   {$EndIf}
   GISdb[FresnelDB].MyData.First;
   for i := 0 to succ(WhereCrest) do GISdb[FresnelDB].MyData.Next;
   repeat
      z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
      if (Height - z > RightHt) then RightHt := height - z;
      {$IfDef RecordNearestCrest}
      WriteLineToDebugFile('Dist=' + RealToString(GISDataBase[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM'),-12,4) + '   z=' + RealToString(z,-8,2) + '   RightHt=' + RealToString(RightHt,-8,2));
      {$EndIf}
      GISdb[FresnelDB].MyData.Next;
   until (GISdb[FresnelDB].MyData.GetFieldByNameAsString('PEAK') = 'Y') or (GISdb[FresnelDB].MyData.eof);

   RightLength := abs(GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM') - MinD);

   {$IfDef RecordNearestCrest}
   WriteLineToDebugFile('Left side',true);
   {$EndIf}
   GISdb[FresnelDB].MyData.First;
   for i := 0 to pred(WhereCrest) do GISdb[FresnelDB].MyData.Next;
   repeat
      z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
      if (Height - z > LeftHt) then LeftHt := height - z;
      {$IfDef RecordNearestCrest}
      WriteLineToDebugFile('Dist=' + RealToString(GISDataBase[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM'),-12,4) + '   z=' + RealToString(z,-8,2) + '   RightHt=' + RealToString(LeftHt,-8,2));
      {$EndIf}
      GISdb[FresnelDB].MyData.Prior;
   until (GISdb[FresnelDB].MyData.GetFieldByNameAsString('PEAK') = 'Y') or (GISdb[FresnelDB].MyData.bof);
   LeftLength := abs(MinD - GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM'));
   GISdb[FresnelDB].MyData.ApplyFilter('');
   {$IfDef RecordNearestCrest}
   WriteLineToDebugFile('Lengths, left=' + RealToString(LeftLength,-12,3) +  '   and right=' + RealToString(RightLength,-12,3),true);
   WriteLineToDebugFile('MinD=' + RealToString(MinD,-12,-4));
   {$EndIf}
   Result := true;
   if isNaN(RightLength) or isNaN(LeftLength) or (abs(RightLength) < 0.01) or  (abs(RightLength) > 100) or
     ((abs(LeftLength) < 0.01) or (abs(LeftLength) > 100)) then Result := false;

end;




procedure tLOSview.DrawTheProfile(var Bitmap : tMyBitmap; FieldName : string12; Color : tColor; Width : integer; SecondName : string12 = ''; Mult : integer = 1);
var
   z,Dist: float;
   FirstPoint : boolean;
   xpic,ypic : integer;
   ColorFromDB : boolean;
   {$IfDef SaveIntermediateProfiles}
   fName : PathStr;
   {$EndIf}
begin
  with GISdb[FresnelDB] do begin
      MyData.First;
      FirstPoint := true;
      ColorFromDB := (Color < 0);
      if Color >= 0 then Bitmap.Canvas.Pen.Color := Color;
      Bitmap.Canvas.Pen.Width := Width;
      while not MyData.eof do begin
         Dist := MyData.GetFieldByNameAsFloat('RANGE_KM');
         if MyData.CarefullyGetFieldByNameAsFloat(FieldName,Z) then begin
            if (SecondName <> '') and (not ColorFromDB) then begin
               z := z + Mult * MyData.GetFieldByNameAsFloat(SecondName);
            end;
            LOSScreenLocation(1000 * Dist,Z,xpic,ypic);
            if (XPic < 32000) then begin
               if FirstPoint then begin
                  Bitmap.Canvas.MoveTo(xpic,ypic);
                  FirstPoint := false;
               end
               else begin
                  if (ColorFromDB) then Bitmap.Canvas.Pen.Color := MyData.GetFieldByNameAsInteger(SecondName);
                  Bitmap.Canvas.LineTo(xpic,ypic);
               end;
            end;
         end
         else FirstPoint := true;
         MyData.Next;
      end;
   end;
   {$IfDef SaveIntermediateProfiles}
   fName := NextFileNumber(MDTempDir, 'los_',OverlayFExt);
   Bitmap.SaveToFile(fName);
   {$EndIf}
end;


procedure tLOSview.DoIntervisibilityFromDataBase;
var
   ObserverTotalElev,NewTanAngle,TanAngle,MaxTanAngle,zt,Dist,MaxZ : float;
   PointOnRay : integer;
   VisPoint,
   AboveVeg : boolean;
   Color : tColor;
begin
   MaxTanAngle := -999;
   GISdb[FresnelDB].MyData.First;
   StartProgress('DB intervisibility');
   ObserverTotalElev := ObsGroundElevLAS + MDDef.ObsAboveGround;
   while not GISdb[FresnelDB].MyData.eof do
   begin
      Dist := 1000 * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
      if GISdb[FresnelDB].MyData.CarefullyGetFieldByNameAsFloat('MAXZ_PTCLD',MaxZ) then begin
         zt := MaxZ + MDDef.TargetAboveGround - DropEarthCurve(Dist);
         TanAngle :=  (ObserverTotalElev - zt) / Dist;  {line to target}
         NewTanAngle :=  (ObserverTotalElev - zt - MDDef.TargetAboveGround) / Dist;  {line to target}
         VisPoint := (TanAngle >= MaxTanAngle);
         if (NewTanAngle > MaxTanAngle) and (Dist > MDDef.wf.ClosestBlockingDistance) then MaxTanAngle := NewTanAngle;
         GISdb[FresnelDB].MyData.Edit;
         if VisPoint then Color := MDDef.FanColor else Color := MDDef.MaskColor;
         GISdb[FresnelDB].MyData.SetFieldByNameAsInteger('COLOR_LAS',Color);
      end;
      GISdb[FresnelDB].MyData.Next;
   end;
end;




constructor tLOSview.Create;
var
   j,WhichDEM : integer;
begin
   {$IfDef RecordLOSProblems}
   WriteLineToDebugFile('tLOSview.FormCreate in',true);
   {$EndIf}
   RespondToResize := false;
   if MDDef.SimpleTopoProfilesOnly then begin
       MDDef.LOSVisible := false;
       MDDef.DrawLOS := false;
       MDDef.DrawFresnel := false;
       MDDef.LOSShowPitch := false;
   end;
   if (MDDef.LOSClHt < 200) then MDDef.LOSClHt := 200;

   TargetName := '';
   SensorName := '';
   ProtractorTool := Nil;
   CrossTrackProfile := Nil;
   BaseBMP := Nil;
   MultipleProfilesToShow := Nil;
   LOSVariety := losVanilla;
   LOSFormDoing := losNothing;
   ZoomedDiagram := false;
   CompareDensityAlongProfile := false;
   ActuallyDraw := false;
   EraseFresnelDB := false;
   EnvelopeDone := false;
   Closable := true;
   ProfileDropDown := 0;
   FresnelDB := 0;
   DragEdit := false;

   {$IfDef ExGeology}
   {$Else}
   PickMagProfVars := Nil;
   {$EndIf}

   {$IFDef ExPointClouds}
   {$Else}
   PtCldInUse := 1;
   for j := 1 to 2 do LOSMemoryPointCloud[j] := Nil;
   {$EndIf}

   MinAreaZ := MaxSmallInt;
   MaxAreaZ := -MaxSmallInt;
   TargetGroundElevLAS := -9999;
   ObsGroundElevLAS := -9999;

   for WhichDEM := 1 to NumDEMDataSetsOpen do begin
       if DEMGlb[WhichDEM] <> Nil then ProfileName[WhichDEM] := RemoveUnderScores(DEMGlb[WhichDEM].AreaName)
       else ProfileName[WhichDEM] := '';
   end;

   {$IfDef FormStyle}
   WriteLineToDebugFile('tLOSview.LineOfSightFromLatLong,true');
   if FormStyle = fsMDIChild then WriteLineToDebugFile('fsMDIChild form')
   else if Self.Visible then WriteLineToDebugFile('normal form, visible')
   else WriteLineToDebugFile('normal form, hidden');
   {$EndIf}

   //RespondToResize := true;
   {$IfDef RecordLOSProblems}
   ScreenDimensions('tLOSview.FormCreate out');
   {$EndIf}
end;



function tLOSview.IntervisibilitysummaryString(DoPopUp : boolean; var TerrainBlockage : boolean) : ANSIstring;
var
   Results : tStringList;
   PixSize : float;
   VegPatch,PtsBlocking : integer;
   InVeg : boolean;


      procedure DoVersion(Which : shortstring; AroundField,AboveField : string12);

         procedure ACase(theField : string12);
         var
            SumBlocking : integer;
         begin
               with GISdb[FresnelDB],MyData do
               begin
                   ApplyFilter(theField + ' > 0');
                   if (RecordCount = 0) then
                   begin
                      if DoPopup then Results.Add('   No blockage');
                      Result := Result +  '0,0,0,';
                   end
                   else
                   begin
                      PtsBlocking := RecordCount;
                      EmpSource.Enabled := false;
                      ApplyFilter('');
                      VegPatch := 0;
                      SumBlocking := 0;
                      InVeg := false;
                      while not MyData.Eof do
                      begin
                          if GetFieldByNameAsString(theField) <> '' then
                          begin
                             SumBlocking := SumBlocking + GetFieldByNameAsInteger(theField);
                             if (not InVeg) then inc(VegPatch);
                             InVeg := true;
                          end
                          else InVeg := false;
                          MyData.Next;
                      end;
                      if DoPopup then
                      begin
                         Results.Add('   Blocked distance: ' + RealToString(PixSize * PtsBlocking,-12,1) + ' m');
                         Results.Add('   Total points blocking: ' + IntToStr(SumBlocking));
                         Results.Add('   Vegetation patches: ' + IntToStr(VegPatch));
                      end;
                      Result := Result +  RealToString(PixSize * PtsBlocking,-12,1) + ',' +  IntToStr(SumBlocking) + ',' +
                         IntToStr(VegPatch) + ',';
                   end;
               end;
         end;

      begin
         with GISdb[FresnelDB],MyData do
         begin
             EmpSource.Enabled := false;
             if DoPopup then
             begin
                Results.Add('');
                Results.Add('From ' + Which);
             end;
              if DoPopup then Results.Add('Surrrounding vegetation');
              ACase(AroundField);
              if DoPopup then Results.Add('Overlying vegetation');
              ACase(AboveField);
         end;
      end;

      {$IFDef ExVegDensity}
      {$Else}
      procedure DoHag(Which : integer; aField : string12);
      begin
         with GISdb[FresnelDB],MyData do
         begin
              if DoPopup then Results.Add('From HAG grid: ' + DEMGlb[DEMGlb[DEMonView].VegGrid[Which]].AreaName);
              ApplyFilter(aField + '=' + QuotedStr('Y'));
              TerrainBlockage := RecordCount > 0;
              if (RecordCount = 0) then  begin
                 if DoPopup then Results.Add('No vegetation blockage');
                 Result := Result + 'N,0,0,';
              end
              else
              begin
                 PtsBlocking := RecordCount;
                 if DoPopup then Results.Add('Blocked by vegetation: ' + RealToString(PixSize * PtsBlocking,-12,1) + ' m');
                 ApplyFilter('');
                 VegPatch := 0;
                 InVeg := false;
                 while not MyData.Eof do
                 begin
                     if GetFieldByNameAsString(aField) = 'Y' then
                     begin
                        if not InVeg then inc(VegPatch);
                        InVeg := true;
                     end
                     else InVeg := false;
                     MyData.Next;
                 end;
                 if DoPopup then Results.Add('Vegetation patches: ' + IntToStr(VegPatch));
                 Result := Result + 'Y,' + RealToString(PixSize * PtsBlocking,-12,1) + ','+ IntToStr(VegPatch) + ',';
              end;
          end;
      end;
      {$EndIf}

begin
   with GISdb[FresnelDB],MyData do
   begin
      ApplyFilter('');
      PixSize := FormSectLenMeters / pred(RecordCount);
      if DoPopup then
      begin
         Results := tStringList.Create;
         Results.Add('Total profile length: ' + RealToString(FormSectLenMeters,-12,2) + ' m');
         Results.Add('Analysis bin size: ' + RealToString(PixSize,6,2) + ' m');
         Results.Add('');
      end;

     Result := RealToString(LatLeft,-12,-7) + ',' + RealToString(LongLeft,-12,-7) + ',' +
               RealToString(LatRight,-12,-7) + ',' + RealToString(LongRight,-12,-7) + ',' +
               RealToString(MDDef.ObsAboveGround,-12,-2) + ',' + RealToString(MDDef.TargetAboveGround,-12,-2) + ',' +
               RealToString(FormSectLenMeters,-12,2) + ',';

      {$IFDef ExVegDensity}
      {$Else}
      if (DEMGlb[DEMonView].VegGrid[1] <> 0 )then
      begin
          if DoPopup then Results.Add('From DTM');
          ApplyFilter('BLOCK_TERR=' + QuotedStr('Y'));

          if (RecordCount = 0) then
          begin
             if DoPopup then Results.Add('No terrain blockage');
             Result := Result + 'N,0,';
          end
          else
          begin
             if DoPopup then Results.Add('Blocked by terrain: ' + RealToString(PixSize * RecordCount,-12,1) + ' m');
             Result := Result + 'Y,' + RealToString(PixSize * RecordCount,-12,1) + ',';
          end;
          if DoPopup then Results.Add('');
          if DEMGlb[DEMonView].VegGrid[1] <> 0 then DoHag(1,'BLOCK_VEG');
          if DEMGlb[DEMonView].VegGrid[2] <> 0 then DoHag(2,'BLOCK_VEG2');
      end;
      {$EndIf}


     {$IFDef ExPointClouds}
     {$Else}
      if (LOSMemoryPointCloud[1] <> Nil) then
      begin
         DoVersion('point cloud ' + LOSMemoryPointCloud[1].CloudName  ,'PTS_AROUND','PTS_ABOVE');
      end;
      if (LOSMemoryPointCloud[2] <> Nil) then
      begin
         DoVersion('point cloud ' + LOSMemoryPointCloud[2].CloudName  ,'PTS2_AROUND','PTS2_ABOVE');
      end;
   {$EndIf}


     {$IFDef ExVegDensity}
     {$Else}
      if (DEMGlb[DEMonView].VegDensityLayers[1] <> Nil) then
      begin
         DoVersion('veg voxels ' + DEMGlb[DEMonView].VegDensityLayers[1].VegDensityName,'VPTS_AROUND','VPTS_ABOVE');
      end;
      if (DEMGlb[DEMonView].VegDensityLayers[2] <> Nil) then
      begin
         DoVersion('veg voxels ' + DEMGlb[DEMonView].VegDensityLayers[2].VegDensityName,'VPT2_AROUND','VPT2_ABOVE');
      end;
      {$EndIf}

      System.Delete(Result,length(Result),1);

      if DoPopup then Petmar.DisplayAndPurgeStringList(Results,'LOS summary');
   end;
end;




procedure Tlosview.SetLOSScreenSize(x, y: integer);
begin

end;

procedure tLOSview.ClearProtractorTool;
begin
   if (ProtractorTool <> Nil) then FreeAndNil(ProtractorTool);
   if (BaseBMP <> Nil) then FreeAndNil(Basebmp);
end;




procedure tLOSview.Saveprofileendpoints2Click(Sender: TObject);
begin
   Saveprofileendpoints1Click(Sender);
end;

procedure tLOSview.Copytoclipboard2Click(Sender: TObject);
begin
   Copytoclipboard1Click(Sender);
end;



procedure InitializeDEMLosW;
begin
   SpreadRidge := 30;
   BlockIntensity := 30;
   MagI := 90;
   MagAlpha := 0;
   MagTimeScale := true;
   MagAnomNames := true;
   DefaultGraphColors(Symbol,LineColors256,LineSize256);
   MDDef.ForceCrestComputations := false;
end;



procedure tLOSview.RecalculateProfile;
var
   fName : PathStr;
   i,NumPts : integer;
   Dist,Ht,MinZ,MaxZ : float;
   LOSCalculation : tLOSCalculation;
begin
   with DEMGlb[DEMonView],HeadRecs do begin

     {$IfDef LoadLastLOS}
     LastSavedLOSfName := ProjectDir + 'last_los.csv';
     Saveprofileendpoints1Click(nil);
     {$EndIf}

      if (FresnelDB <> 0) then begin
         CloseAndNilNumberedDB(FresnelDB,EraseFresnelDB);
      end;
      fName := NextFileNumber(MDTempDir, DEMGlb[DEMonView].AreaName + '_los_',DefaultDBExt);
      MakeFresnelTable(fName, MDDef.DrawFresnel,
           {$IFDef ExVegDensity}
           false,
           {$Else}
           (VegGrid[1] <> 0),
           {$EndIf}

           {$IfDef ExPointClouds}
           false,
           {$Else}
           (LOSMemoryPointCloud[1] <> Nil),
           {$EndIf}
           MDDef.DoGrazingFields,
           //CrestComputationsOn or ForceCrestComputations,
           false,

           {$IFDef ExVegDensity}
           false,
           {$Else}
           (VegGrid[2] <> 0),
           {$EndIf}

           {$IfDef ExPointClouds}
           false,
           {$Else}
           (LOSMemoryPointCloud[2] <> Nil),
           {$EndIf}
           {$IFDef ExVegDensity}
           false,false );
           {$Else}
           (VegDensityLayers[1] <> Nil),
           (VegDensityLayers[2] <> Nil)  );
           {$EndIf}


      GISdb[FresnelDB].dbOpts.DBAutoShow := dbasNone;
      GISdb[FresnelDB].EmpSource.Enabled := false;
      LOSCalculation := tLOSCalculation.Create;
      LosCalculation.Execute(GISdb[FresnelDB].MyData,DEMGlb[DEMonView],LatLeft,LongLeft,LatRight,LongRight, MDDef.ObsAboveGround, MDDef.TargetAboveGround,FresnelDB
         {$IfDef ExPointClouds}
         {$Else}
         ,LOSMemoryPointCloud[1],LOSMemoryPointCloud[2]
         {$EndIf}
         );
      LosCalculation.Destroy;

      GISdb[FresnelDB].MyData.First;
      ObsGroundElev := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
      GISdb[FresnelDB].MyData.Last;
      TargetGroundElev := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');

  {$IFDef ExPointClouds}
  {$Else}
     {$EndIf}
   end;
end;


procedure tLOSview.RedrawLOS(WhichDEM,BHeight,BTop : integer; DrawSideTix : boolean = true);


      procedure DrawProfile(bitmap : tMyBitmap);
      var
         LOSCalculation : tLOSCalculation;
         DownLabel,DownSymbol,i,j,XPic,YPic,y,ypic2,
         XPic1,YPic1,Color,Col,Row,LeftY,SymSize : integer;
         Dir : tCompassDirection;
         AspDir,LocalPitch,EarthCurve,Dist,
         n1,n2,n3,l1,l2,l3,Graz2,XGrid,YGrid,xg,yg,nz,Slope2,Slope3,
         HorizDist,PointElev,TanAngle,Pitch,MinZ,MaxZ,z2,az,
         MaxTanAngle,Lat,Long,Fresnel,LOSHt,z,VegHt,lz,
         XGridIncr,YGridIncr : float;
         IsPeak,IsPit,ReCalculateRequired,FirstPoint : boolean;
         TStr,MenuStr : ShortString;
         SavedProfile,Legend : tStringList;
         f1,f2 : string12;
         fName : PathStr;
         Table1 : tMyData;
         rgbColor : tRGBTriple;
         VisPoints : array[0..MaxScreenXMax] of boolean;
         Protractor : tMyBitmap;
         MyFont : tMyFont;
         k : integer;

            procedure DrawPitchLine(Pitch : float);
            label
               Done;
            var
               y2,j,y,xpic,ypic : integer;
               z,z2 : float;
            begin
               with GISdb[FresnelDB], Bitmap do begin
                   Canvas.Pen.Color := MDDef.PitchLineColor;
                   Canvas.Pen.Width := MDDef.PitchLineWidth;
                   MyData.First;
                   FirstPoint := true;
                   z := MyData.GetFieldByNameAsFloat('ELEV_M') + MDDef.ObsAboveGround;
                     while not MyData.eof do begin
                        Dist := MyData.GetFieldByNameAsFloat('RANGE_KM');
                        z2 := z + TanDeg(Pitch) * 1000 * Dist;
                        LOSScreenLocation(1000*Dist,Z2,xpic,ypic);
                        if (yPic >= StartLOSDown + PixelsHigh) or (yPic < 0) then goto Done;
                        if (XPic < 32000) then begin
                           if FirstPoint then begin
                              Bitmap.Canvas.MoveTo(xpic,ypic);
                              FirstPoint := false;
                           end
                           else Bitmap.Canvas.LineTo(xpic,ypic);
                        end;
                        MyData.Next;
                     end;
                   Done:;
               end;
            end;

            procedure ShowDenityAlongProfile(Thename : shortstring; TheField : string12; Offset : integer = 0);
            begin
                if TheName <> '' then Legend.Add(theName);
                GISdb[FresnelDB].MyData.First;
                while not GISdb[FresnelDB].MyData.eof do begin
                   if (GISdb[FresnelDB].MyData.GetFieldByNameAsString(TheField) <> '') then begin
                      Dist := 1000 * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                      z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LOS_HT_M');
                      j := GISdb[FresnelDB].MyData.GetFieldByNameAsInteger(TheField);
                      LOSScreenLocation(Dist,z,xpic,ypic);
                      if (XPic < 32000) then Petmar.ScreenSymbol(Bitmap.Canvas,XPic,YPic + Offset,VertLine,2,VegDenstColors[j]);
                   end;
                   GISdb[FresnelDB].MyData.Next;
                end;
            end;

              procedure ShowBlocksAlongProfile(aField : string12; Grid,TopLabel,Offset : integer);
              begin
                  {$IFDef ExVegDensity}
                  {$Else}
                  Legend.Add(DEMGlb[DEMGlb[DEMonView].VegGrid[Grid]].AreaName);
                  {$EndIf}
                  GISdb[FresnelDB].MyData.ApplyFilter(aField + '=' + QuotedStr('Y'));
                  while not GISdb[FresnelDB].MyData.eof do begin
                      Dist := 1000 * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                      z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LOS_HT_M');
                      LOSScreenLocation(Dist,z,xpic,ypic);
                      if (XPic < 32000) then Petmar.ScreenSymbol(Bitmap.Canvas,XPic,YPic+Offset,VertLine,2,clLime);
                      GISdb[FresnelDB].MyData.Next;
                  end;
                  GISdb[FresnelDB].MyData.ApplyFilter('');
              end;

      begin
        {$IfDef RecordLOSProblems}
        WriteLineToDebugFile('tLOSview.RedrawLOS start draw Profile ' + TimeToStr(Now));
        {$EndIf}
         if (DEMGlb[DEMonView] = Nil) then exit;
         with DEMGlb[DEMonView],HeadRecs do begin

           if (FresnelDB = 0) then RecalculateProfile;

           if ActuallyDraw then with Bitmap.Canvas do begin
              //FillScanlineAddresses(Bitmap,P3);
              p3 := tBMPMemory.Create(Bitmap);
              DrawCollar(Bitmap);
              if (FresnelDB <> 0) and (GISdb[FresnelDB] <> Nil) then GISdb[FresnelDB].EmpSource.Enabled := false;
              if MDDef.DrawFresnel then begin
                 DrawTheProfile(Bitmap,'LOS_HT_M', MDDef.FresnelZone1Color, MDDef.FresnelZone1Width,'FRESNEL1_M',1);
                 DrawTheProfile(Bitmap,'LOS_HT_M', MDDef.FresnelZone1Color, MDDef.FresnelZone1Width,'FRESNEL1_M',-1);
                 DrawTheProfile(Bitmap,'LOS_HT_M', MDDef.FresnelZone2Color, MDDef.FresnelZone2Width,'FRESNEL2_M',1);
                 DrawTheProfile(Bitmap,'LOS_HT_M', MDDef.FresnelZone2Color, MDDef.FresnelZone2Width,'FRESNEL2_M',-1);
              end;

              if MDDef.ShowDEMTerrainProfile then DrawTheProfile(Bitmap,'ELEV_M', MDDef.TerrainProfileColor, MDDef.TerrainProfileWidth);

              if MDDef.ShowMaskedAirspace then DrawTheProfile(Bitmap,'MASK_AIR', MDDef.MaskedAirspaceColor, MDDef.MaskedAirspaceWidth);

              if MDDef.LOSVisible then  begin
                 if MDDef.LOSVisShowLine then begin
                    DrawTheProfile(Bitmap,'ELEV_M',-99,MDDef.LOSVisLineWidth,'COLOR');
                 end
                 else begin
                     GISdb[FresnelDB].MyData.First;
                     while not GISdb[FresnelDB].MyData.eof do begin
                        GISdb[FresnelDB].MyData.Edit;
                        Dist := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                        z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
                        Color := GISdb[FresnelDB].MyData.GetFieldByNameAsInteger('COLOR');
                        LOSScreenLocation(1000 * Dist,Z,xpic,ypic);
                        if (XPic < 32000) then FastSymbolOnBitmap(3,xpic,ypic,Petmar.TColorToRGB(Color),p3);
                        GISdb[FresnelDB].MyData.Next;
                     end;
                 end;
              end;

              {$IfDef ExLasLidar}
              {$Else}
              OverlayProfilePointCloud(Bitmap);

              if MDDef.VegEffectsVoxels and (BaseMap.MapDraw.VegDensityLayerInUse <> 0) and (DEMGlb[DEMonView].VegDensityLayers[BaseMap.MapDraw.VegDensityLayerInUse]<> Nil) then begin
                 if not CompareDensityAlongProfile then begin
                     TStr := 'Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[BaseMap.MapDraw.VegDensityLayerInUse].VegDensityName;
                     GetMyFontFromWindowsFont(MyFont,Bitmap.Canvas.Font);
                     Bitmap.Canvas.Font.Size := 18;
                     Bitmap.Canvas.Font.Color := clLime;
                     Bitmap.Canvas.Font.Style := [fsBold];
                     Bitmap.Canvas.Font.Name := 'Verdana';
                     Bitmap.Canvas.TextOut(Bitmap.Width div 2 - Bitmap.Canvas.TextWidth(TStr),10,TStr);
                     LoadMyFontIntoWindowsFont(MyFont,Bitmap.Canvas.Font);
                 end;

                  Bitmap.Canvas.Pen.Width := 2;
                  SymSize := 1;
                  GISdb[FresnelDB].MyData.First;
                  while not GISdb[FresnelDB].MyData.eof do begin
                      Dist := 1000 * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                      Lat := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LAT');
                      Long := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('LONG');
                      z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');

                      DEMGlb[DEMonView].LatLongDegreeToDEMGrid(Lat,Long,Col,Row);
                      for i := 1 to MaxVegLayers do with DEMGlb[DEMonView],VegDensityLayers[BaseMap.MapDraw.VegDensityLayerInUse] do begin
                         if (VegLayers[i] <> 0) and DEMGlb[VegLayers[i]].GetElevMeters(Col,Row,z2) then begin
                            LOSScreenLocation(Dist,Z + pred(i),xpic,ypic);
                            LOSScreenLocation(Dist,Z+i,xpic,ypic2);
                            Bitmap.Canvas.Pen.Color := VegDenstColors[round(z2)];
                            Bitmap.Canvas.MoveTo(xpic,ypic);
                            Bitmap.Canvas.LineTo(xpic,ypic2);
                         end;
                      end {for i};
                      GISdb[FresnelDB].MyData.Next;
                  end {while};
              end;

               if MDDef.ShowCloudDensity and (FresnelDB <> 0) and (LOSMemoryPointCloud[PtCldInUse] <> Nil) then begin
                  if PtCldInUse = 1 then ShowDenityAlongProfile('','PTS_AROUND');
                  if PtCldInUse = 2 then ShowDenityAlongProfile('','PTS2_AROUND');
               end;
              {$EndIf}

               {$IfDef ExVegDensity}
               {$Else}
               if MDDef.LOSShowVoxelDensity and (FresnelDB <> 0) then begin
                  if (BaseMap.MapDraw.VegDensityLayerInUse = 1) and (DEMGlb[DEMonView].VegDensityLayers[1] <> Nil) then ShowDenityAlongProfile('','VPTS_AROUND');
                  if (BaseMap.MapDraw.VegDensityLayerInUse = 2) and (DEMGlb[DEMonView].VegDensityLayers[2] <> Nil) then ShowDenityAlongProfile('','VPT2_AROUND');
               end;
               {$EndIf}

               if CompareDensityAlongProfile then begin
                  Legend := TStringList.Create;

                  {$IfDef ExVegDensity}
                  {$Else}
                  if (DEMGlb[DEMonView].VegDensityLayers[2] <> Nil) then begin
                     ShowDenityAlongProfile('Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[2].VegDensityName,'VPT2_ABOVE',-32);
                  end;
                  if (DEMGlb[DEMonView].VegDensityLayers[1] <> Nil) then begin
                     ShowDenityAlongProfile('Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[1].VegDensityName,'VPTS_ABOVE',-24);
                  end;
                  {$EndIf}

                 {$IfDef ExPointClouds}
                 {$Else}
                  if (LOSMemoryPointCloud[2] <> Nil) then begin
                     ShowDenityAlongProfile('Cloud: ' + LOSMemoryPointCloud[2].CloudName,'PTS2_ABOVE',-16);
                  end;
                  if (LOSMemoryPointCloud[1] <> Nil) then begin
                     ShowDenityAlongProfile('Cloud: ' + LOSMemoryPointCloud[1].CloudName,'PTS_ABOVE',-8);
                  end;
                  {$EndIf}

                  {$IFDef ExVegDensity}
                  DownSymbol := 0;
                  {$Else}
                  if (VegGrid[1] > 0) then ShowBlocksAlongProfile('BLOCK_VEG',1,30,0);
                  if (VegGrid[2] > 0) then begin
                     ShowBlocksAlongProfile('BLOCK_VEG2',2,45,8);
                     DownSymbol := 8;
                  end
                  else begin
                     DownSymbol := 0;
                  end;
                  {$EndIf}


                 {$IfDef ExPointClouds}
                 {$Else}
                  if (LOSMemoryPointCloud[1] <> Nil) then begin
                     ShowDenityAlongProfile('Cloud: ' + LOSMemoryPointCloud[1].CloudName,'PTS_AROUND',8 + DownSymbol);
                  end;
                  if (LOSMemoryPointCloud[2] <> Nil) then begin
                     ShowDenityAlongProfile('Cloud: ' + LOSMemoryPointCloud[2].CloudName,'PTS2_AROUND',16 + DownSymbol);
                  end;
                  {$EndIf}

                  {$IfDef ExVegDensity}
                  {$Else}
                  if (DEMGlb[DEMonView].VegDensityLayers[1] <> Nil) then begin
                     ShowDenityAlongProfile('Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[1].VegDensityName,'VPTS_AROUND',24 + DownSymbol);
                  end;
                  if (DEMGlb[DEMonView].VegDensityLayers[2] <> Nil) then begin
                     ShowDenityAlongProfile('Voxels: ' + DEMGlb[DEMonView].VegDensityLayers[2].VegDensityName,'VPT2_AROUND',32 + DownSymbol);
                  end;
                  {$EndIf}

                  for k := 0 to pred(Legend.Count) do begin
                     Bitmap.Canvas.TextOut(Bitmap.Width div 2 - 40,10 + k * 15,Legend.Strings[k]);
                  end;
                  Legend.Free;

               end;

                {$IfDef ExVegGrid}
                {$Else}
                if (VegGrid[1] > 0) and MDDef.ShowGridVegEffects then with GISdb[FresnelDB],Bitmap.Canvas do begin
                  if VegGridUsed = 1 then begin
                    f1 := 'VEG_HT';
                    f2 := 'BLOCK_VEG';
                  end
                  else begin
                    f1 := 'VEG2_HT';
                    f2 := 'BLOCK_VEG2';
                  end;

                  Pen.Color := clGreen;
                  Pen.Width := 1;
                  MyData.ApplyFilter(f1 + ' > 0.001');
                  while not MyData.eof do begin
                     Dist := MyData.GetFieldByNameAsFloat('RANGE_KM');
                     z := MyData.GetFieldByNameAsFloat('ELEV_M');
                     VegHt := MyData.GetFieldByNameAsFloat(f1);
                     LOSScreenLocation(1000 * Dist,Z,xpic,ypic);
                     if (XPic < 32000) then begin
                        Bitmap.Canvas.MoveTo(xpic,ypic);
                        LOSScreenLocation(1000 * Dist,Z + VegHt,xpic,ypic);
                        Bitmap.Canvas.LineTo(xpic,ypic);
                     end;
                     MyData.Next;
                  end;

                  Pen.Color := clLime;
                  MyData.ApplyFilter(f2 +  '=' + QuotedStr('Y'));
                  while not MyData.eof do begin
                     Dist := MyData.GetFieldByNameAsFloat('RANGE_KM');
                     z := MyData.GetFieldByNameAsFloat('ELEV_M');
                     VegHt := MyData.GetFieldByNameAsFloat(f1);
                     LOSScreenLocation(1000 * Dist,Z,xpic,ypic);
                     if (XPic < 32000) then begin
                        Bitmap.Canvas.MoveTo(xpic,ypic);
                        LOSScreenLocation(1000 * Dist,Z + VegHt,xpic,ypic);
                        Bitmap.Canvas.LineTo(xpic,ypic);
                     end;
                     MyData.Next;
                  end;
                  MyData.ApplyFilter('');
               end {if veg};
               {$EndIf}

               if (FresnelDB <> 0) and MDDef.PlotCrest and MDDef.ForceCrestComputations then begin
                  GISdb[FresnelDB].MyData.First;
                  while not GISdb[FresnelDB].MyData.eof do begin
                     Dist := 1000 * GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
                     z := GISdb[FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
                     LOSScreenLocation(Dist,z,xpic,ypic);
                     if (XPic < 32000) then begin
                        if GISdb[FresnelDB].MyData.GetFieldByNameAsString('PEAK') = 'Y' then begin
                          Petmar.ScreenSymbol(Bitmap.Canvas,XPic,YPic,FilledBox,3,clLime);
                        end
                        else if GISdb[FresnelDB].MyData.GetFieldByNameAsString('PIT') = 'Y' then begin
                          Petmar.ScreenSymbol(Bitmap.Canvas,XPic,YPic,FilledBox,3,clRed);
                        end;
                     end;
                     GISdb[FresnelDB].MyData.Next;
                  end;
               end;

               if ElevationDEM and (Not (LOSVariety in [losAllDEMs,losAllDEMDropDown])) then with Bitmap.Canvas do begin
                  if MDDef.DrawLOS then begin    {Draw line of sight}
                    Bitmap.Canvas.Pen.Color := MDDef.LosConnectionColor;
                    Bitmap.Canvas.Pen.Width := MDDef.LosConnectionWidth;
                    LOSScreenLocation(0,ObsGroundElev + MDDef.ObsAboveGround,xpic,ypic);
                    Bitmap.Canvas.MoveTo(xpic,ypic);
                    LOSScreenLocation(FormSectLenMeters,TargetGroundElev +  MDDef.TargetAboveGround,xpic,ypic);
                    Bitmap.Canvas.LineTo(xpic,ypic);
                  end {if};

                  if MDDef.LOSShowPitch then begin
                     DrawPitchLine(MDDef.LOSMinPitch);
                     DrawPitchLine(MDDef.LOSMaxPitch);
                  end;
               end {if};

               if MDDef.DrawLOSProtractor then begin
                  Protractor := CreateProtractor(false,false,FormVertExag);
                  Bitmap.Canvas.Draw(30,30,Protractor);
                  Protractor.Free;
               end;

               if MDDef.LOSShowPitch then begin
                  Pitch := arcTan( -((ObsGroundElev +   MDDef.ObsAboveGround) - (TargetGroundElev +  MDDef.TargetAboveGround - Dropcurve)) / FormSectLenMeters) / DegToRad;
                  TStr := '  Pitch= ' + RealToString(Pitch,-8,-2) + DegSym;
               end
               else TStr := '';

               if not (LOSVariety = losThreeProfiles) then Caption := DEMGlb[DEMonView].AreaName + ' LOS: vert exag' + RealToString(FormVertExag,7,1) + TStr;
               if (LOSVariety <> losThreeProfiles) or (WhichDEM = 1) or MDDef.LabelMultipleProf then with BitMap.Canvas do begin

                  if (SensorName = '') and (TargetName = '') then begin
                     TextOut(0,0,PreferLocationString(LatLeft,LongLeft,BaseMap.MapDraw.CurrentMapDatum));
                     TStr := PreferLocationString(LatRight,LongRight,BaseMap.MapDraw.CurrentMapDatum);
                     TextOut(PixLong - TextWidth(TStr),0,TStr);
                  end
                  else begin
                     TextOut(0,0,SensorName);
                     TextOut(PixLong - TextWidth(TargetName),0,TargetName);
                  end;
               end;
               if (FresnelDB <> 0) then GISdb[FresnelDB].EmpSource.Enabled := true;
           end;
         end;
      end {proc DrawProfile};

var
   Pitch,yf : float;
   xi,x,y   : integer;
   Bitmap   : tMyBitmap;
   TStr : ShortString;
begin {proc RedrawLOS}
   {$IfDef RecordLOSProblems}
   ScreenDimensions('tLOSview.RedrawLOS in ');
   {$EndIf}
   ClearProtractorTool;
   if (WhichDEM = 0) or (DEMGlb[WhichDEM] = Nil) then exit;
   try
      ShowHourglassCursor;
      with DEMGlb[WhichDEM],HeadRecs do begin
         {$IfDef RecordLOSProblems}
         WriteLineToDebugFile('LOS with DEM: ' + IntToStr(WhichDEM),true);
         WriteLineToDebugFile(' Left side:  ' + LatLongDegreeToString(LatLeft,LongLeft,DecDegrees),true);
         {$EndIf}

         if LOSVariety in [losThreeProfiles,losAllDEMs,losAllDEMDropDown,losMagModel,losSimpleMagModel] then begin
            DEMOnView := WhichDEM;
            if LOSVariety in [losThreeProfiles,losMagModel,losSimpleMagModel] then begin
               MinAreaZ := 99999;
               MaxAreaZ := -99999;
            end;
         end;
         LOSExtremeElevationsAndScreenCoordinates(WhichDEM);

         {$IfDef RecordLOSProblems}
         WriteLineToDebugFile('tLOSview.RedrawLOS call set screen size',true);
         {$EndIf}
         SetLOSScreenSize(ClientWidth,BHeight);

         if ActuallyDraw then begin
            if (LOSVariety in [losAllDEMs,losAllDEMDropDown]) and (WhichDEM <> 1) then CopyImageToBitmap(Image1,Bitmap)
            else begin
               CreateBitmap(Bitmap,Image1.Width,BHeight);
               Bitmap.Canvas.Pen.Width := 3;
            end;
            if LOSVariety in [losAllDEMs,losAllDEMDropDown] then Bitmap.Canvas.Pen.Width := 2;
            if (WMDEM <> Nil) then Bitmap.Canvas.Font := wmdem.Font;
         end
         else Bitmap := Nil;

        {$IfDef ExLasLidar}
        {$Else}
        LoadProfilePointCloud;
        {$EndIf}

         DrawProfile(BitMap);

         if ActuallyDraw then begin
           {$IfDef RecordLOSProblems}
           WriteLineToDebugFile('tLOSview.RedrawLOS end draw Profile ' + TimeToStr(Now));
           {$EndIf}

           if LOSVariety in [losThreeProfiles,losMagModel,losSimpleMagModel] then Image1.Canvas.Draw(0,bTop,Bitmap)
           else Image1.Picture.Graphic := Bitmap;
           Bitmap.Free;
        end;
     end {with}
   finally
     ShowDefaultCursor;
   end;
   {$IfDef RecordLOSProblems}
   ScreenDimensions('tLOSviewyyy.RedrawLOS out');
   {$EndIf}
end {proc RedrawLOS};




initialization
   InitializeDEMLosW;
finalization
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing demlosw in', true);
   {$EndIf}

{$IfDef RecordLOSProblems}
   WriteLineToDebugFile('RecordLOSProblems active in DEMLOSW');
{$EndIf}
{$IfDef RecordAllLOSProblems}
   WriteLineToDebugFile('RecordAllLOSProblems active in DEMLOSW');
{$EndIf}
{$IfDef RecordClosingProblems}
   WriteLineToDebugFile('RecordClosingProblems active in DEMLOSW');
{$EndIf}
{$IfDef RecordLOSAlgorithm}
   WriteLineToDebugFile('RecordLOSAlgorithm active in DEMLOSW');
{$EndIf}
   {$IfDef FormStyle}
   WriteLineToDebugFile('FormStyle active in DEMLOSW');
   {$EndIf}
   {$IfDef RecordWaveLenghtHeightProblems}
   WriteLineToDebugFile('RecordWaveLenghtHeightProblems active in DEMLOSW');
   {$EndIf}
   {$IfDef RecordNearestCrest}
   WriteLineToDebugFile('RecordNearestCrest active in DEMLOSW');
   {$EndIf}
   {$IfDef RecordThreadCrest}
   WriteLineToDebugFile('RecordThreadCrest active in DEMLOSW');
   {$EndIf}
   {$IfDef RecordPointClouds}
   WriteLineToDebugFile('RecordPointClouds active in DEMLOSW');
   {$EndIf}
   {$IfDef RecordMGTProblems}
   WriteLineToDebugFile('RecordMGTProblems active in DEMLOSW');
   {$EndIf}
   {$IfDef RecordLOSPrettyDrawing}
   WriteLineToDebugFile('RecordLOSPrettyDrawing active in DEMLOSW');
   {$EndIf}
   {$IfDef SaveIntermediateProfiles}
   WriteLineToDebugFile('SaveIntermediateProfiles active in DEMLOSW');
   {$EndIf}



   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing demlosw out');
   {$EndIf}
end.





