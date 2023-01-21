unit demlosw;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM freeware GIS   }
{ PETMAR Trilobite Breeding Ranch }
{    verified 7/30/2104           }
{_________________________________}

{$I nevadia_defines.inc}



//{$Define ExPointCloud}



{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define RecordLOSAlgorithm}
      {$Define RecordLOSDraw}
      //{$Define RecordPointClouds}
      //{$Define RecordClosing}
      //{$Define RecordUTMZones}
      //{$Define RecordSlopeCalc}
      //{$Define RecordLOSLegend}
      //{$Define RecordLOSProblems}
      //{$Define RecordLOSPrettyDrawing}
      //{$Define RecordRandomProfiles}
      //{$Define RecordWaveLenghtHeightProblems}
      //{$Define RecordAllLOSProblems}
      //{$Define RecordClosingProblems}
      //{$Define RecordNearestCrest}
      //{$Define RecordThreadCrest}
      //{$Define RecordMGTProblems}
   {$EndIf}
{$Endif}


interface

uses
  SysUtils, StrUtils,Windows, Messages, Classes, Graphics, Controls,Forms,
  System.UITypes,
  Dialogs,ExtCtrls, StdCtrls,ClipBrd,ComCtrls,Math,Menus, Buttons,


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
//end inline of the core DB functions

  {$IFDef ExPointCloud}
  {$Else}
     point_cloud_memory,
  {$EndIf}

  Petmath,Petmar_types,PetDBUtils,PETMAR,DEMDefs,DEMMapf,DEMLos_draw,DEMCoord,BaseGraf,PetImage;

type
  TDEMLOSF = class(TForm)
    MainMenu1: TMainMenu;
    Modify1: TMenuItem;
    File1: TMenuItem;
    Saveimage1: TMenuItem;
    Close1: TMenuItem;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Copytoclipboard1: TMenuItem;
    PopupMenu1: TPopupMenu;
    LOSParameters1: TMenuItem;
    Saveimage2: TMenuItem;
    Copytoclipboard2: TMenuItem;
    N2: TMenuItem;
    Spacinganalysis1: TMenuItem;
    Algorithmanalysis1: TMenuItem;
    N3: TMenuItem;
    Measureslope1: TMenuItem;
    Parallelprofiles1: TMenuItem;
    Linesizeandcolors1: TMenuItem;
    Profilenames1: TMenuItem;
    Profiledropdown1: TMenuItem;
    AllopenDEMs1: TMenuItem;
    Averageelevation1: TMenuItem;
    Adjustazimuth1: TMenuItem;
    Panel2: TPanel;
    TrackBar1: TTrackBar;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    Adjustrange1: TMenuItem;
    Sensorobserversiteblowup1: TMenuItem;
    Grazingangles1: TMenuItem;
    Protractor1: TMenuItem;
    Magneticmodel1: TMenuItem;
    FrenelZoneencroachment1: TMenuItem;
    Profilelegends1: TMenuItem;
    Pastefromclipboard1: TMenuItem;
    Wavelengthheight1: TMenuItem;
    Openprofiledatabase1: TMenuItem;
    Frese1: TMenuItem;
    N1: TMenuItem;
    ProfilesonotherDEMs1: TMenuItem;
    Grainalongprofile1: TMenuItem;
    OpenGLofPointCloud1: TMenuItem;
    Intervisibilitysummary1: TMenuItem;
    Saveprofileendpoints1: TMenuItem;
    Label1: TLabel;
    BitBtn2: TBitBtn;
    PopupMenu2: TPopupMenu;
    Saveimage3: TMenuItem;
    Copyimagetoclipboard1: TMenuItem;
    Saveprofileendpoints2: TMenuItem;
    Sethorizontalpixelsizem1: TMenuItem;
    Hideprofiles1: TMenuItem;
    procedure Wavelengthheight1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Saveimage1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure LOSParameters1Click(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Modify1Click(Sender: TObject);
    procedure Saveimage2Click(Sender: TObject);
    procedure Copytoclipboard2Click(Sender: TObject);
    procedure Spacinganalysis1Click(Sender: TObject);
    procedure Algorithmanalysis1Click(Sender: TObject);
    procedure Measureslope1Click(Sender: TObject);
    procedure Parallelprofiles1Click(Sender: TObject);
    procedure Profilenames1Click(Sender: TObject);
    procedure Profiledropdown1Click(Sender: TObject);
    procedure AllopenDEMs1Click(Sender: TObject);
    procedure Averageelevation1Click(Sender: TObject);
    procedure Adjustazimuth1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BitBtn1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Adjustrange1Click(Sender: TObject);
    procedure Sensorobserversiteblowup1Click(Sender: TObject);
    procedure Grazingangles1Click(Sender: TObject);
    procedure Protractor1Click(Sender: TObject);
    procedure Magneticmodel1Click(Sender: TObject);
    procedure FrenelZoneencroachment1Click(Sender: TObject);
    procedure Profilelegends1Click(Sender: TObject);
    procedure Pastefromclipboard1Click(Sender: TObject);
    procedure Openprofiledatabase1Click(Sender: TObject);
    procedure OpenGLofPointCloud1Click(Sender: TObject);
    procedure Frese1Click(Sender: TObject);
    procedure ProfilesonotherDEMs1Click(Sender: TObject);
    procedure Grainalongprofile1Click(Sender: TObject);
    procedure Intervisibilitysummary1Click(Sender: TObject);
    procedure Saveprofileendpoints1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Saveimage3Click(Sender: TObject);
    procedure Copyimagetoclipboard1Click(Sender: TObject);
    procedure Saveprofileendpoints2Click(Sender: TObject);
    procedure SetImagesize1Click(Sender: TObject);
    procedure Sethorizontalpixelsizem1Click(Sender: TObject);
    procedure Hideprofiles1Click(Sender: TObject);
  private
    { Private declarations }
      procedure ClearProtractorTool;
      function IntervisibilitysummaryString(DoPopUp: boolean; var TerrainBlockage : boolean): ANSIstring;

      {$IfDef ExMagAnom}
      {$Else}
         procedure StartMagModel;
         procedure CreateMarineMagneticAnomaliesBitmap(var Bitmap : tMyBitmap; Wide,High : integer);
      {$EndIf}
      {$IF Defined(ExPointCloud) or Defined(ExVegDensity)}
      {$Else}
         procedure LoadProfilePointCloud(ShowLoadProgress : boolean = true);
      {$EndIf}
      {$IfDef RecordLOSProblems}
         procedure ScreenDimensions(Where : ANSIString);
      {$EndIf}
  public
    { Public declarations }
     LOSdraw : tLOSDraw;
     BaseMap : tMapForm;
     ProtractorTool,Basebmp : tMyBitmap;
     CrossTrackProfile : BaseGraf.TThisBaseGraph;
     Closable,ActuallyDraw,RespondToResize,DragEdit   : boolean;
     FirstLOSX,FirstLOSY : integer;

     procedure LineOfSightFromLatLong;
     procedure MarineMagneticAnomalies;
     procedure ShowOnMap;
     procedure EnableAzimuthShifts;
     {$IfDef ExWaveLengthHeight}
     {$Else}
        function FindNearestCrest(Distance : float64; var Lat,Long,Height,LeftHt,LeftLength,RightHt,RightLength : float64) : boolean;
        procedure FindWaveLengthHeight(DisplayIt : boolean; var WavelengthMean,WavelengthMedian,WavelengthStdDev, HeightMean,HeightMedian,HeightStd : float64; Memo1 : tMemo = nil);
     {$EndIf}
end;



type
   tLOSFormDoing = (losNothing,losFirstSlope,losSecondSlope,losFresnelCrossSection);
var
   {$IfDef ExMagAnom}
   {$Else}
      BlockIntensity : integer;
      SpreadRidge,TimeLeft,TimeRight,
      MagI,MagAlpha : float64;
      MagTimeScale,MagAnomNames : boolean;
   {$EndIf}
   LOSFormDoing : tLOSFormDoing;
   SavedMapImage : tMyBitmap;


function StartLOS(inActuallyDraw : boolean; WhatType : tDEMDoingWhat; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64; inBaseMap : tMapForm; inEnableAzimuthShifts : boolean = false) : TDEMLOSF;
function AverageProfileGraph(Map : tMapForm; DEM : Integer; LatLeft,LongLeft,LatRight,LongRight : float64) :  TThisBaseGraph;


implementation

{$R *.DFM}

uses
   PetGraphColors,
   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}
   DEMLOSOp,DEMDef_routines,
   DEMRange,
   Thread_timers,
   Make_tables,  basemap,
   DataBaseCreate,
   DEM_Manager,
   Toggle_DB_Use,

   {$IfDef ExPointCloud}
   {$Else}
      Point_Cloud_Options, Las_Lidar,slicer_3d,
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
   LastLOSX,LastLOSY : integer;

   {$IfDef ExGeology}
   {$Else}
      PickMagProfVars : TPickMagProfVars;
   {$EndIf}

{$IfDef ExWaveLengthHeight}
{$Else}
   {$I demlos_crest_wavelength.inc}
{$EndIf}


{$I demlos_marine_mag.inc}


function StartLOS(inActuallyDraw : boolean; WhatType : tDEMDoingWhat; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64; inBaseMap : tMapForm; inEnableAzimuthShifts : boolean = false) : TDEMLOSF;
begin
  {$If Defined(RecordLOSProblems) or Defined(RecordUTMZone)} WriteLineToDebugFile('enter StartLOS, DEM=' + IntToStr(DEMonMap) + '  Map UTM zone=' + IntToStr(inBaseMap.MapDraw.PrimMapProj.projUTMZone)); {$EndIf}

   Result := Nil;
   if True or (DEMGlb[DEMonMap].LatLongDegreeInDEM(Lat1,Long1) and DEMGlb[DEMonMap].LatLongDegreeInDEM(Lat2,Long2)) then begin
      if WhatType = SeekingSecondAverageProfile then begin
         AverageProfileGraph(inBaseMap,DEMonMap,Lat1,Long1,Lat2,Long2);
      end
      else begin
         Result := TDEMLOSF.Create(Application);
         Result.ActuallyDraw := inActuallyDraw;
         if not Result.ActuallyDraw then begin
            Result.Width := 1;
            Result.Height := 1;
         end;
         Result.LOSdraw.LatLeft := Lat1;
         Result.LOSdraw.LongLeft := Long1;
         Result.LOSdraw.LatRight := Lat2;
         Result.LOSdraw.LongRight := Long2;
         {$If Defined(RecordLOSProblems)} WriteLineToDebugFile('Left side=' + LatLongDegreeToString(Lat1,Long1) + '  rightside=' + LatLongDegreeToString(Lat2,Long2) ); {$EndIf}
         if (WhatType = MultipleTopoProfileRight) then Result.LOSdraw.LOSVariety := losAllDEMs
         else if (WhatType = SimpleTopoProfileRight) then Result.LOSdraw.LOSVariety := losSimpleOne
         else if (WhatType = MultipleLOS) then Result.LOSdraw.LOSVariety := losAllDEMDropDown
         {$IfDef ExGeology}
         {$Else}
         else if (WhatType = SeekingRightSideMagModels) then Result.LOSdraw.LOSVariety := losMagModel
         {$EndIf}
         else Result.LOSdraw.LOSVariety := losVanilla;

         Result.LOSdraw.DEMonView := DEMonMap;
         Result.BaseMap := inBaseMap;
         Result.LOSdraw.BaseMapDraw := inBaseMap.MapDraw;

         if inEnableAzimuthShifts then Result.EnableAzimuthShifts;

         if Result.ActuallyDraw then begin
            {$IfDef RecordLOSProblems} WriteLineToDebugFile('StartLOS, ActuallyDraw'); {$EndIf}
            Result.RespondToResize := true;
            Result.FormResize(Nil);
            Result.FormStyle := fsMDIChild;
            Result.ShowOnMap;
            Result.SetFocus;
            wmDEM.FormPlacementInCorner(Result,lpNEMap);
            {$IfDef RecordLOSProblems} WriteLineToDebugFile('StartLOS, Out'); {$EndIf}
         end;
      end;
   end
   else MessageToContinue(NoDEMCovers);

   {$If Defined(RecordUTMZones) or Defined(RecordLOSProblems)} WriteLineToDebugFile('exit StartLOS, Map UTM zone=' + IntToStr(inBaseMap.MapDraw.PrimMapProj.projUTMZone));  {$EndIf}
end;



{$IfDef RecordLOSProblems}
   procedure TDEMLOSF.ScreenDimensions(Where : ANSIString);
   begin
      WriteLineToDebugFile(Where + '  ');
      WriteLineToDebugFile('Form=' + IntToStr(Width) + 'x' + IntToStr(Height) + ' Client=' + IntToStr(ClientWidth) + 'x' + IntToStr(ClientHeight));
      WriteLineToDebugFile('Image=' + ImageSize(Image1) + '  PixLong=' + IntToStr(LOSdraw.PixLong) + ' and PixelsHigh=' + IntToStr(LOSdraw.PixelsHigh));
   end;
{$EndIf}


procedure TDEMLOSF.Sethorizontalpixelsizem1Click(Sender: TObject);
var
   ps : float64;
begin
   ps := LOSDraw.FormSectLenMeters / LOSDraw.PixLong;
   ReadDefault('Profile pixel size (m)',ps);
   LOSDraw.PixLong := round( LOSDraw.FormSectLenMeters / ps);
   ClientWidth := LOSDraw.PixLong + 105;
   FormResize(Sender);
end;

procedure TDEMLOSF.SetImagesize1Click(Sender: TObject);
var
   x : integer;
begin
   X := ClientWidth - 105;
   ReadDefault('Profile length (pixels)',x);
   ClientWidth := x + 105;
   FormResize(Sender);
end;


procedure TDEMLOSF.BitBtn1Click(Sender: TObject);
begin
   Adjustazimuth1Click(Sender);
end;


procedure TDEMLOSF.BitBtn2Click(Sender: TObject);
begin
   Adjustazimuth1Click(Sender);
end;


procedure TDEMLOSF.FrenelZoneencroachment1Click(Sender: TObject);
begin
   {$IfDef ExFresnel}
   {$Else}
      if (LOSdraw.LOSProfileDB <> 0) then begin
         GISdb[LOSdraw.LOSProfileDB].DisplayTable;
         GISdb[LOSdraw.LOSProfileDB].dbOpts.XField := 'RANGE_KM';
         GISdb[LOSdraw.LOSProfileDB].dbOpts.YField := 'INTRUDE_PC';
         GISdb[LOSdraw.LOSProfileDB].DBTablef.N2Dgraph1Click(Nil);
      end;
   {$EndIf}
end;

procedure TDEMLOSF.Frese1Click(Sender: TObject);
begin
   if (LOSdraw.LOSProfileDB <> 0) and (CrossTrackProfile = Nil) then begin
     GISdb[LOSdraw.LOSProfileDB].DisplayTable;
     CrossTrackProfile := BaseGraf.TThisBaseGraph.Create(Application);
     CrossTrackProfile.GraphDraw.HorizLabel := 'Distance from LOS (m)';
     CrossTrackProfile.GraphDraw.VertLabel := 'Elevation (m)';
     CrossTrackProfile.GraphDraw.MaxVertAxis := LOSdraw.MaxAreaZ + MDDef.ObsAboveGround;
     CrossTrackProfile.GraphDraw.MinVertAxis := LOSdraw.MinAreaZ;
     CrossTrackProfile.GraphDraw.MinHorizAxis := -MDDef.LOSSliceBuffer;
     CrossTrackProfile.GraphDraw.MaxHorizAxis := MDDef.LOSSliceBuffer;
     CrossTrackProfile.SetUpGraphForm;
     CrossTrackProfile.Top := Self.Height + 5;
     CrossTrackProfile.Left := 0;
   end;
   LOSFormDoing := losFresnelCrossSection;
   Image1DblClick(Sender);
end;


procedure TDEMLOSF.Grainalongprofile1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      DEMStat.CalculateGrainProfile(BaseMap,LOSdraw.DEMonView,LOSdraw.LatLeft,LOSdraw.LongLeft,LOSdraw.LatRight,LOSdraw.LongRight);
   {$EndIf}
end;

procedure TDEMLOSF.Grazingangles1Click(Sender: TObject);
begin
   {$IfDef ExFresnel}
   {$Else}
      if (LOSdraw.LOSProfileDB <> 0) and MDDef.DoGrazingFields then begin
        GISdb[LOSdraw.LOSProfileDB].DisplayTable;
        GISdb[LOSdraw.LOSProfileDB].dbOpts.XField := 'RANGE_KM';
        GISdb[LOSdraw.LOSProfileDB].dbOpts.YField := 'GRAZING_2D';
        GISdb[LOSdraw.LOSProfileDB].DBTablef.N2Dgraph1Click(Nil);
      end;
   {$EndIf}
end;


procedure TDEMLOSF.Hideprofiles1Click(Sender: TObject);
var
   i : integer;
begin
   {$IfDef SupClassAuxGrids} WriteLineToDebugFile('TSupClassAuxGrids.BitBtn10Click (Pick open grids) in'); {$EndIf}
   GetMultipleDEMsFromList('DEMs to include in profile',LOSDraw.ShowProfile);
   FormResize(Nil);
end;

procedure TDEMLOSF.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,LOSdraw.LOSAzimuth);
end;

procedure TDEMLOSF.Close1Click(Sender: TObject);
begin
   Close;
end;


procedure TDEMLOSF.Saveimage1Click(Sender: TObject);
begin
   PetImage.SaveImageAsBMP(Image1);
end;


procedure TDEMLOSF.FormClose(Sender: TObject; var Action: TCloseAction);
var
   i : integer;
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('TDEMLOSF.FormClose'); {$EndIf}
   {$IfDef ExGeology}
   {$Else}
      ClearProtractorTool;
      if (LOSdraw.LOSProfileDB <> 0) then begin
         {$IfDef RecordClosing} WriteLineToDebugFile('Close Fresnel DB ' + IntToStr(LOSdraw.LOSProfileDB)); {$EndIf}
         CloseAndNilNumberedDB(LOSdraw.LOSProfileDB);
      end
      else begin
         {$IfDef RecordClosing} WriteLineToDebugFile('No Fresnel DB');   {$EndIf}
      end;

      {$IFDef ExPointCloud}
      {$Else}
         for i := 1 to 2 do
            if (LOSdraw.LOSMemoryPointCloud[i] <> Nil) then LOSdraw.LOSMemoryPointCloud[i].Destroy;
      {$EndIf}

      if (PickMagProfVars <> Nil) then begin
         PickMagProfVars.Free;
         PickMagProfVars := Nil;
      end;
   {$EndIf}
   Action := caFree;
   Self := Nil;
end;


procedure TDEMLOSF.FormCreate(Sender: TObject);
begin
   {$IfDef RecordLOSProblems} WriteLineToDebugFile('TDEMLOSF.FormCreate in');  {$EndIf}

   LOSdraw := tLOSDraw.Create;

   if MDDef.SimpleTopoProfilesOnly then begin
       MDDef.LOSVisible := false;
       MDDef.DrawLOS := false;
      {$IfDef ExFresnel}
      {$Else}
          MDDef.DrawFresnel := false;
      {$EndIf}
       MDDef.LOSShowPitch := false;
   end;
   if (MDDef.LOSClHt < 200) then MDDef.LOSClHt := 200;

   ScrollBox1.HorzScrollBar.Visible := false;
   ScrollBox1.VertScrollBar.Visible := false;

   Closable := true;
   RespondToResize := false;
   ActuallyDraw := false;

   ProtractorTool := Nil;
   CrossTrackProfile := Nil;
   BaseBMP := Nil;
   LOSFormDoing := losNothing;
   BaseMap := Nil;
   DragEdit := false;

   {$IfDef ExGeology}
   {$Else}
      PickMagProfVars := Nil;
   {$EndIf}

   Panel2.Height := 0;

   {$IfDef RecordLOSProblems} ScreenDimensions('TDEMLOSF.FormCreate out');  {$EndIf}
end;


procedure TDEMLOSF.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
   Elev : float32;
   CapStr, TStr     : ShortString;
   i                : integer;
   newBMP : tMyBitmap;
begin
   for i := 0 to 3 do WmDEM.StatusBar1.Panels[i].Text := '';

   if DragEdit then begin
       CreateBitmap(newbmp,savedmapImage.width,savedmapImage.height);
       newbmp.Canvas.Draw(0,0,savedmapImage);
       newbmp.Canvas.Draw(x,y,DragBitmap);
       Image1.Picture.Graphic := newbmp;
       newbmp.free;
   end;

   if (LOSFormDoing = losSecondSlope) then begin
      {$IfDef RecordSlopeCalc} WriteLineToDebugFile('MouseMovelosSecondSlope, start draw at x=' + IntToStr(MapScreenX1) + ',  y=' + IntToStr(MapScreenY1)); {$EndIf}
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.Pen.Width := 2;
      Image1.Canvas.MoveTo(FirstLOSX,FirstLOSY);
      Image1.Canvas.LineTo(lastLOSx,lastLOSy);
      Image1.Canvas.MoveTo(FirstLOSX,FirstLOSY);
      Image1.Canvas.LineTo(x,y);
   end;

   if (ProtractorTool <> Nil) then begin
      Image1.Picture.Graphic := Basebmp;
      Image1.Canvas.CopyMode := cmSrcAnd;
      Image1.Canvas.Draw(x-ProtractorTool.Width div 2,y,ProtractorTool);
   end;

   LastLOSX := x;
   LastLOSY := y;
   if (DEMGlb[LOSDraw.DEMonView] = Nil) then exit;
   if (x >= LOSDraw.StartLOSLeft) and (x <= LOSDraw.StartLOSLeft + LOSDraw.PixLong) then begin
      CapStr := '';
      BroadcastLatLong(Self.Handle,LOSdraw.ProfLats[x-LOSdraw.StartLOSLeft],LOSdraw.ProfLongs[x-LOSdraw.StartLOSLeft]);
      if LOSdraw.LOSVariety in [losAllDEMs,losAllDEMDropDown] then begin
         for i := 1 to NumDEMDataSetsOpen do if ValidDEM(i) then  begin
            if DEMGLB[i].GetElevFromLatLongDegree(LOSdraw.ProfLats[x-LOSdraw.StartLOSLeft],LOSdraw.ProfLongs[x-LOSdraw.StartLOSLeft],Elev) then
               CapStr := CapStr + '      ' + DEMGLB[i].AreaName + '  ' + RealToString(Elev,-10,1) + ' m';
         end;
      end
      else  begin
         DEMGlb[LOSdraw.DEMonView].GetElevFromLatLongDegree(LOSdraw.ProfLats[x-LOSdraw.StartLOSLeft],LOSdraw.ProfLongs[x-LOSdraw.StartLOSLeft],Elev);
         if (DEMGlb[LOSdraw.DEMonView].ElevationDEM) then CapStr := CapStr + '  z=' + RealToString(Elev,4,1) + ' m'
         else CapStr := CapStr + '  z=' + RealToString(Elev,7,2) + ElevUnitsAre(DEMGlb[LOSdraw.DEMonView].DEMheader.ElevUnits);
         if MDdef.DualElevs then CapStr := CapStr + '  (' + RealToString(Elev/Petmar_types.FeetToMeters,-5,0) + ' ft)';

         {$IFDef ExVegDensity}
         {$Else}
            if (DEMGlb[LOSdraw.DEMonView].VegGrid[1] <> 0) then begin
               if DEMGlb[DEMGlb[LOSdraw.DEMonView].VegGrid[1]].GetElevFromLatLongDegree(LOSdraw.ProfLats[x-LOSdraw.StartLOSLeft],LOSdraw.ProfLongs[x-LOSdraw.StartLOSLeft],Elev) then
                  TStr := RealToString(Elev,-15,-2) + ' m'
               else TStr := 'no veg';
               CapStr := CapStr + '  (veg=' + TStr + ')';
            end;
         {$EndIf}
      end;
      if (BaseMap <> Nil) then Panel1.Caption := BaseMap.MapDraw.PrimMapProj.PreferLocationString(LOSdraw.ProfLats[x-LOSdraw.StartLOSLeft],LOSdraw.ProfLongs[x-LOSdraw.StartLOSLeft]) + CapStr;
   end
   else Panel1.Caption := '';
end;


procedure TDEMLOSF.Intervisibilitysummary1Click(Sender: TObject);
var
   TerrBlock : boolean;
begin
   IntervisibilitysummaryString(true,TerrBlock);
end;


function TDEMLOSF.IntervisibilitysummaryString(DoPopUp : boolean; var TerrainBlockage : boolean) : ANSIstring;
var
   Results : tStringList;
   PixSize : float64;
   VegPatch,PtsBlocking : integer;
   InVeg : boolean;


  {$IF Defined(ExPointCloud) or Defined(ExVegDensity)}
  {$Else}
      procedure DoVersion(Which : shortstring; AroundField,AboveField : ShortString);

            procedure ACase(theField : ShortString);
            var
               SumBlocking : integer;
            begin
                  with LOSDraw, GISdb[LOSProfileDB] do begin
                      MyData.ApplyFilter(theField + ' > 0');
                      if (MyData.RecordCount = 0) then begin
                         if DoPopup then Results.Add('   No blockage');
                         Result := Result +  '0,0,0,';
                      end
                      else begin
                         PtsBlocking := MyData.RecordCount;
                         EmpSource.Enabled := false;
                         MyData.ApplyFilter('');
                         VegPatch := 0;
                         SumBlocking := 0;
                         InVeg := false;
                         while not MyData.Eof do begin
                             if MyData.GetFieldByNameAsString(theField) <> '' then begin
                                SumBlocking := SumBlocking + MyData.GetFieldByNameAsInteger(theField);
                                if (not InVeg) then inc(VegPatch);
                                InVeg := true;
                             end
                             else InVeg := false;
                             MyData.Next;
                         end;
                         if DoPopup then begin
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
          GISdb[LOSdraw.LOSProfileDB].EmpSource.Enabled := false;
          if DoPopup then begin
             Results.Add('');
             Results.Add('From ' + Which);
          end;
           if DoPopup then Results.Add('Surrrounding vegetation');
           ACase(AroundField);
           if DoPopup then Results.Add('Overlying vegetation');
           ACase(AboveField);
      end;
   {$EndIf}

      {$IFDef ExVegDensity}
      {$Else}
         procedure DoHag(Which : integer; aField : ShortString);
         begin
            with GISdb[LOSdraw.LOSProfileDB] do begin
                 if DoPopup then Results.Add('From HAG grid: ' + DEMGlb[DEMGlb[LOSdraw.DEMonView].VegGrid[Which]].AreaName);
                 MyData.ApplyFilter(aField + '=' + QuotedStr('Y'));
                 TerrainBlockage := MyData.RecordCount > 0;
                 if (MyData.RecordCount = 0) then  begin
                    if DoPopup then Results.Add('No vegetation blockage');
                    Result := Result + 'N,0,0,';
                 end
                 else begin
                    PtsBlocking := MyData.RecordCount;
                    if DoPopup then Results.Add('Blocked by vegetation: ' + RealToString(PixSize * PtsBlocking,-12,1) + ' m');
                    MyData.ApplyFilter('');
                    VegPatch := 0;
                    InVeg := false;
                    while not MyData.Eof do begin
                        if MyData.GetFieldByNameAsString(aField) = 'Y' then begin
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
   GISdb[LOSDraw.LOSProfileDB].MyData.ApplyFilter('');
   PixSize := LOSDraw.FormSectLenMeters / pred(GISdb[LOSDraw.LOSProfileDB].MyData.RecordCount);
   if DoPopup then begin
      Results := tStringList.Create;
      Results.Add('Total profile length: ' + RealToString(LOSDraw.FormSectLenMeters,-12,2) + ' m');
      Results.Add('Analysis bin size: ' + RealToString(PixSize,6,2) + ' m');
      Results.Add('');
   end;

  Result := RealToString(LOSDraw.LatLeft,-12,-7) + ',' + RealToString(LOSDraw.LongLeft,-12,-7) + ',' +
            RealToString(LOSDraw.LatRight,-12,-7) + ',' + RealToString(LOSDraw.LongRight,-12,-7) + ',' +
            RealToString(MDDef.ObsAboveGround,-12,-2) + ',' + RealToString(MDDef.TargetAboveGround,-12,-2) + ',' +
            RealToString(LOSDraw.FormSectLenMeters,-12,2) + ',';

   {$IFDef ExVegDensity}
   {$Else}
      if (DEMGlb[LOSDraw.DEMonView].VegGrid[1] <> 0 )then begin
          if DoPopup then Results.Add('From DTM');
          GISdb[LOSDraw.LOSProfileDB].MyData.ApplyFilter('BLOCK_TERR=' + QuotedStr('Y'));

          if (GISdb[LOSDraw.LOSProfileDB].MyData.RecordCount = 0) then begin
             if DoPopup then Results.Add('No terrain blockage');
             Result := Result + 'N,0,';
          end
          else begin
             if DoPopup then Results.Add('Blocked by terrain: ' + RealToString(PixSize * GISdb[LOSDraw.LOSProfileDB].MyData.RecordCount,-12,1) + ' m');
             Result := Result + 'Y,' + RealToString(PixSize * GISdb[LOSDraw.LOSProfileDB].MyData.RecordCount,-12,1) + ',';
          end;
          if DoPopup then Results.Add('');
          if DEMGlb[LOSDraw.DEMonView].VegGrid[1] <> 0 then DoHag(1,'BLOCK_VEG');
          if DEMGlb[LOSDraw.DEMonView].VegGrid[2] <> 0 then DoHag(2,'BLOCK_VEG2');
      end;
   {$EndIf}


  {$IFDef ExPointCloud}
  {$Else}
      if (LOSDraw.LOSMemoryPointCloud[1] <> Nil) then begin
         DoVersion('point cloud ' + LOSDraw.LOSMemoryPointCloud[1].CloudName  ,'PTS_AROUND','PTS_ABOVE');
      end;
      if (LOSDraw.LOSMemoryPointCloud[2] <> Nil) then begin
         DoVersion('point cloud ' + LOSDraw.LOSMemoryPointCloud[2].CloudName  ,'PTS2_AROUND','PTS2_ABOVE');
      end;
  {$EndIf}

  {$IFDef ExVegDensity}
  {$Else}
      if (DEMGlb[LOSDraw.DEMonView].VegDensityLayers[1] <> Nil) then begin
         DoVersion('veg voxels ' + DEMGlb[LOSDraw.DEMonView].VegDensityLayers[1].VegDensityName,'VPTS_AROUND','VPTS_ABOVE');
      end;
      if (DEMGlb[LOSDraw.DEMonView].VegDensityLayers[2] <> Nil) then begin
         DoVersion('veg voxels ' + DEMGlb[LOSDraw.DEMonView].VegDensityLayers[2].VegDensityName,'VPT2_AROUND','VPT2_ABOVE');
      end;
   {$EndIf}

   System.Delete(Result,length(Result),1);
   if DoPopup then Petmar.DisplayAndPurgeStringList(Results,'LOS summary');
end;


procedure TDEMLOSF.LOSParameters1Click(Sender: TObject);
begin
   if SetLOSOptions(Self) then  begin
      FormResize(Nil);
   end;
end;


procedure TDEMLOSF.Sensorobserversiteblowup1Click(Sender: TObject);
begin
   DEMGlb[LOSDraw.DEMonView].SelectionMap.SiteContourMapBlowUp(LOSDraw.LatLeft,LOSDraw.LongLeft,1000,500,MDDef.DefaultContourInterval,'Sensor/observer location');
end;


procedure TDEMLOSF.ClearProtractorTool;
begin
   if (ProtractorTool <> Nil) then FreeAndNil(ProtractorTool);
   if (BaseBMP <> Nil) then FreeAndNil(Basebmp);
end;


procedure TDEMLOSF.Image1DblClick(Sender: TObject);
var
   anrgbColor : tRGBTriple;
   x,z,i, xi,yi,SymSize : integer;
   LowDist,HighDist : float64;
   Ht,Rad,Bitty,Distance,Bearing,dz,Dist : float64;
   z1,z2 : float32;
   rFile : file;
   v : array[1..3] of float64;

        procedure MakeFresnelFile;
        var
           i : integer;
           rFile : file;
           v : tFloatPoint;
        begin
          CrossTrackProfile.OpenPointFile(rfile,CrossTrackProfile.Symbol);
          for I := 0 to 360 do begin
             v[1] := SinDeg(i) * Rad;
             v[2] := Ht + CosDeg(i) * Rad;
             CrossTrackProfile.AddPointToDataBuffer(rfile,v);
          end;
          CrossTrackProfile.ClosePointDataFile(Rfile);
       end;

begin
with LOSdraw do begin
   ClearProtractorTool;

   if DragEdit then begin
      Image1.Canvas.Pen.Color := clWhite;
      Image1.Canvas.Brush.Color := clWhite;
      Image1.Canvas.Brush.Style := bsSolid;
      Image1.Canvas.Rectangle(LastLOSX,LastLOSY,LastLOSX+DragBitmap.Width,LastLOSY+DragBitmap.Height);
      Image1.Canvas.Draw(LastLOSX,LastLOSY,DragBitmap);
      FreeAndNil(DragBitmap);
      SavedMapImage.Free;
      DragEdit := false;;
   end;

   if (LOSVariety in [losMagModel,losSimpleMagModel]) then begin
      RidgeLoc := 0.001 * FormSectLenMeters * (LastLOSX-StartLOSLeft) / PixLong;
      MarineMagneticAnomalies;
   end;

   if (LOSFormDoing = losSecondSlope) and (FirstX <> LastLOSX) then {with DEMGlb[DEMonView] do} begin
      DEMGlb[DEMonView].GetElevFromLatLongDegree(ProfLats[FirstLOSx-StartLOSLeft],ProfLongs[FirstLOSx-StartLOSLeft],z1);
      DEMGlb[DEMonView].GetElevFromLatLongDegree(ProfLats[LastLOSX-StartLOSLeft],ProfLongs[LastLOSX-StartLOSLeft],z2);
      VincentyCalculateDistanceBearing(ProfLats[FirstLOSx-StartLOSLeft],ProfLongs[FirstLOSx-StartLOSLeft],ProfLats[LastLOSX-StartLOSLeft],ProfLongs[LastLOSX-StartLOSLeft],Distance,Bearing);
      dz := (z1-z2);
      z1 := abs(dz/Distance);
      Petmar.MessageToContinue('Slope: ' + RealToString(100*z1,8,2) + '%' + MessLineBreak + RealToString(arcTan(z1)/DegToRad,12,2) + DegSym + MessLineBreak +
                'elev drop: ' + RealToString(dz,8,2) + ' m' + MessLineBreak + 'over ' + SmartDistanceMetersFormat(Distance),True);
      LOSFormDoing := losNothing;
   end;

   if (LOSFormDoing = losFirstSlope) then begin
      LOSFormDoing := losSecondSlope;
      FirstX := LastLOSX;
      FirstY := LastLOSY;
      {$IfDef RecordSlopeCalc} WriteLineToDebugFile('Start losSecondSlope, at x=' + IntToStr(FirstX) + ',  y=' + IntToStr(FirstY)); {$EndIf}
   end;

   if (LOSFormDoing = losFresnelCrossSection) then begin
       DEMGlb[DEMonView].DrawCrossTrackProfile(CrossTrackProfile,ProfLats[LastLOSX-StartLOSLeft],ProfLongs[LastLOSX-StartLOSLeft],LOSAzimuth,MDDef.LOSSliceBuffer,0.5*DEMGlb[DEMonView].AverageSpace);

       with GISdb[LOSProfileDB],MyData do begin
          Bitty := 0.00001;
          repeat
             MyData.ApplyFilter(PointVeryCloseGeoFilter(LatFieldName,LongFieldName,ProfLats[LastLOSX-StartLOSLeft],ProfLongs[LastLOSX-StartLOSLeft],Bitty));
             Bitty := 2 * Bitty;
          until (MyData.RecordCount >= 1);
          Ht := MyData.GetFieldByNameAsFloat('LOS_HT_M');
          Dist := GetFieldByNameAsFloat('RANGE_KM');

   {$IfDef ExFresnel}
   {$Else}
          if MDDef.DrawFresnel then begin
             Rad := GetFieldByNameAsFloat('FRESNEL1_M');
             MakeFresnelFile;
             CrossTrackProfile.GraphDraw.LineSize256[3] := MDdef.FresnelZone1Width;
             CrossTrackProfile.GraphDraw.FileColors256[3] := MDdef.FresnelZone1Color;
             CrossTrackProfile.GraphDraw.Symbol[3].Size := 0;

             Rad := 0.2 * Rad;
             MakeFresnelFile;
             CrossTrackProfile.GraphDraw.LineSize256[4] := MDdef.FresnelZone1Width;
             CrossTrackProfile.GraphDraw.FileColors256[4] := MDdef.FresnelZone1Color;

             Rad := GetFieldByNameAsFloat('FRESNEL2_M');
             MakeFresnelFile;
             CrossTrackProfile.GraphDraw.LineSize256[5] := MDdef.FresnelZone2Width;
             CrossTrackProfile.GraphDraw.FileColors256[5] := MDdef.FresnelZone2Color;
          end
          else
      {$EndIf}
          with CrossTrackProfile,GraphDraw do begin
              Petmar.ScreenSymbol(Image1.Canvas,GraphX(0),GraphY(Ht),FilledBox,SymSize,claRed);
          end;

          with CrossTrackProfile,GraphDraw do begin
             CrossTrackProfile.Caption := 'Section ' + RealToString(Dist,-12,3) + ' km along profile, V.E=' +
                  RealToString( ((MaxHorizAxis - MinHorizAxis) / (XWindowSize - LeftMargin)) / ((MaxVertAxis - MinVertAxis) / (YWindowSize - BottomMargin)),-12,-2);
          end;

          {$IfDef ExLASlidar}
          {$Else}
             if (LOSMemoryPointCloud[PtCldInUse] <> Nil) then begin
                if (MDdef.ShowPointCloundOnProfile = spcDensity) then begin
                   LOSMemoryPointCloud[PtCldInUse].GetAcrossProfileDensity(1000 * Dist);
                   CrossTrackProfile.OpenXYColorFile(rfile);
                   with LOSMemoryPointCloud[PtCldInUse] do begin
                       for x := -MaxYDensity to MaxYDensity do begin
                          v[1] := CrossTrackProfile.GraphDraw.GraphX(x);
                          for z  := 1 to MaxzDensity do begin
                             if (AcrossProfileDensity^[x,z] > 0) then begin
                                 v[2] := CrossTrackProfile.GraphDraw.GraphY(AcrossProfileGround[x] + z);
                                 v[3] := ConvertPlatformColorToTColor(VegDenstColors[AcrossProfileDensity^[x,z]]);
                                 BlockWrite(rfile,v,1);
                             end {if};
                          end {for z};
                       end {for x};
                       CloseFile(Rfile);
                   end {with};
                end {if};

                if (MDdef.ShowPointCloundOnProfile = spcPoints) then begin
                     LowDist := 1000 * Dist - 0.5 * MDDef.CloudSliceThick;
                     HighDist := 1000 * Dist + 0.5 * MDDef.CloudSliceThick;
                     CrossTrackProfile.GraphDraw.XYColorFilesPlotted.Clear;
                     CrossTrackProfile.OpenXYColorFile(rfile);

                     with LOSMemoryPointCloud[PtCldInUse] do begin
                        anrgbColor := ConvertTColorToPlatformColor(clAqua);
                        for i := 1 to NumMemPts do begin
                          if (LOSMemoryPointCloud[PtCldInUse].xyPts^[i,2] > LowDist) and (xyPts^[i,2] <= HighDist) then begin
                              if (MDdef.ls.ColorCoding = lasccClass) and (PtClass <> Nil) then anrgbcolor := Las_rgb_colors[PtClass^[i]];
                              if (MDdef.ls.ColorCoding = lasccReturnNumber) and (PtReturnNumber <> Nil) then anrgbColor := Las_ret_colors[PtReturnNumber^[i]];
                              with CrossTrackProfile,GraphDraw do begin
                                 xi := GraphX(LOSMemoryPointCloud[PtCldInUse].xyPts^[i,1]);
                                 yi := GraphY(LOSMemoryPointCloud[PtCldInUse].zPts^[i]);
                                 v[1] := LOSMemoryPointCloud[PtCldInUse].xyPts^[i,1];
                                 v[2] := LOSMemoryPointCloud[PtCldInUse].zPts^[i];
                                 if PtOnGraph(v[1],v[2]) then begin
                                    v[3] := ConvertPlatformColorToTColor(anrgbcolor);
                                    BlockWrite(rfile,v,1);
                                 end;
                              end;
                           end {if};
                        end {for i};
                       CloseFile(Rfile);
                   end {with};
                end {if};
             end;
          {$EndIf}
          {$IfDef ExPointCloud}
          {$Else}
             for i := 1 to 15 do CrossTrackProfile.GraphDraw.Symbol[i].Size := MDDef.CloudSymbol[1].Size;
          {$EndIf}
          CrossTrackProfile.RedrawDiagram11Click(Nil);
          CrossTrackProfile.BringToFront;
       end;
   end;
end;
end;


procedure TDEMLOSF.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
{$IfDef RecordClosing} WriteLineToDebugFile('TDEMLOSF.FormCloseQuery, saving size, ' + IntToStr(ClientWidth) + 'x' + IntToStr(ClientHeight)); {$EndIf}
   CanClose := Closable;   // and (DEMMapF.DEMNowDoing <> SeekingSecondLOS);  5/8/2021, unclear why this was here for just one case
   MDDef.LOSClHt := ClientHeight;
   MDDef.LOSClWid := ClientWidth;
end;


procedure TDEMLOSF.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      FirstLOSX := x;
      FirstLOSY := y;
      {$IfDef RecordSlopeCalc} WriteLineToDebugFile('MBrightdown, at x=' + IntToStr(FirstLOSX) + ',  y=' + IntToStr(FirstLOSY)); {$EndIf}
      if (LOSDraw.LOSVariety in [losMagModel,losSimpleMagModel]) then PopupMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y)
      else Modify1Click(Sender);
   end;
end;


procedure TDEMLOSF.FormActivate(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to 3 do WmDEM.StatusBar1.Panels[i].Text := '';
end;


procedure TDEMLOSF.Copyimagetoclipboard1Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TDEMLOSF.Copytoclipboard1Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;


procedure TDEMLOSF.Modify1Click(Sender: TObject);
var
   MultiProf : boolean;
begin
   with LOSDraw do begin
      MultiProf := (LOSVariety in [losAllDEMs,losAllDEMDropDown]) and (NumDEMDataSetsOpen > 1);
      AlgorithmAnalysis1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and  MDDef.ShowMethodCompare and (not MultiProf);
      SpacingAnalysis1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and  MDDef.ShowMethodCompare and (not MultiProf);
      MeasureSlope1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram]);
      Sensorobserversiteblowup1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and (not MultiProf);
      Adjustazimuth1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and (not MultiProf);
      Adjustrange1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and (not MultiProf);
      Grazingangles1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and (not MultiProf) and MDDef.ShowGeomorphometry;
      GrainAlongProfile1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and (not MultiProf);
      Parallelprofiles1.Visible := ((LOSVariety in [losVanilla,losSimpleOne]) or (MDDef.ProgramOption in [GeologyProgram])) and (not MultiProf);
      Magneticmodel1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram]) or (DEMGlb[DEMonView].DEMheader.ElevUnits in [TenthMgal,Milligal,TenthGamma,Gammas,HundredthMGal,Nanotesla]);

      Wavelengthheight1.Visible := (LOSProfileDB <> 0) and MDDef.ShowGeomorphometry and (MDDef.ProgramOption in [ExpertProgram]);
      Linesizeandcolors1.Visible := MultiProf;
      Grazingangles1.Visible := MDDef.DoGrazingFields;
      AllopenDEMs1.Visible := (NumDEMDataSetsOpen > 1);
      ProfilesonotherDEMs1.Visible := (NumDEMDataSetsOpen > 1);
      Hideprofiles1.Visible := (NumDEMDataSetsOpen > 1);
      Profilelegends1.Visible := MultiProf;
      Profiledropdown1.Visible := LOSVariety in [losAllDEMs,losAllDEMDropDown];

      if MDDef.SimpleTopoProfilesOnly then begin
         Intervisibilitysummary1.Visible := false;
      end;

      {$IfDef ExFresnel}
         FrenelZoneencroachment1.Visible := false;
         Averageelevation1.Visible := false;
         Frese1.Visible := false;
      {$Else}
         FrenelZoneencroachment1.Visible := MDDef.DrawFresnel and (not MultiProf);
         Averageelevation1.Visible := MDDef.DrawFresnel and (not MultiProf);
         {$IFDef ExPointCloud}
            Frese1.Visible := false;
         {$Else}
            Frese1.Visible := (LOSMemoryPointCloud[PtCldInUse] <> Nil) or (MDDef.DrawFresnel and (not MultiProf));
         {$EndIf}
      {$EndIf}

      if (MDdef.ProgramOption = DragonPlotProgram) then begin
         Intervisibilitysummary1.Visible := false;
         Protractor1.Visible := false;
         MeasureSlope1.Visible := false;
         ParallelProfiles1.Visible := false;
         ProfileNames1.Visible := false;
      end;

      {$IfDef ExLasLidar}
      OpenGLofPointCloud1.Visible := false;
      {$Else}
      OpenGLofPointCloud1.Visible := (Point_Cloud_Options.pt_cloud_opts_fm <> Nil) and (MDDef.LOSshowPointCloudRangePoints or MDDef.LOSshowPointCloudRangeLines or (MDDef.ShowPointCloundOnProfile in [spcPoints,spcDensity]));
      {$EndIf}

      PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;



procedure TDEMLOSF.EnableAzimuthShifts;
begin
   Panel2.Height := 36;
   Trackbar1.Enabled := true;
end;


procedure TDEMLOSF.Saveimage2Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;


procedure TDEMLOSF.Saveimage3Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;


procedure TDEMLOSF.Saveprofileendpoints1Click(Sender: TObject);
var
   fName : PathStr;
   results : tStringList;
begin
   if (Sender = Nil) then  fName := LastSavedLOSfName
   else begin
      Petmar.GetFileNameDefaultExt('Profile end points','*.csv',fName);
      fName := ExtractFilePath(DEMGlb[LOSDraw.DEMonView].DEMFileName);
   end;
   Results := tStringList.Create;
   Results.Add('LAT,LONG,LAT2,LONG2');
   Results.Add(RealToString(LOSDraw.LatLeft,-18,-8) + ',' + RealToString(LOSDraw.LongLeft,-18,-8) + ',' + RealToString(LOSDraw.LatRight,-18,-8) + ',' + RealToString(LOSDraw.LongRight,-18,-8));
   Results.SaveToFile(fName);
   Results.Free;
end;

procedure TDEMLOSF.Saveprofileendpoints2Click(Sender: TObject);
begin
   Saveprofileendpoints1Click(Sender);
end;

procedure TDEMLOSF.Copytoclipboard2Click(Sender: TObject);
begin
   Copytoclipboard1Click(Sender);
end;


procedure TDEMLOSF.Spacinganalysis1Click(Sender: TObject);
{$IfDef ExViewshed}
begin
{$Else}
var
   xgrids,ygrids,dists,elevs : ^Petmath.bfarray32;
   j,nfiles,VisCount : integer;
   VisPoints : array[0..MaxScreenXMax] of boolean;
   Results : tStringList;
   ThisGraph : TThisBaseGraph;


   procedure FigureForSpacing(Points: integer);
   var
      i : integer;
      rfile : file;
      v : array[1..3] of float64;
   begin
      with LOSDraw, DEMGlb[LOSDraw.DEMonView] do begin
          {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('FigureForSpacing  ' + IntToStr(Points)); {$EndIf}
          wmDEM.SetPanelText(0,'Spacing: ' + IntToStr(Points));
          Points := round(FormSectLenMeters/Points);
          if (Points > MaxScreenXMax) then Points := MaxScreenXMax;

          GetStraightRouteDEMGrid(LatLeft,LongLeft,LatRight,LongRight,MDDef.wf.StraightAlgorithm,Points,xgrids^,ygrids^,dists^);
          GetVisiblePoints(MDDef.ObsAboveGround,MDDef.TargetAboveGround,-89,89,true,true,Points,xgrids^,ygrids^,dists^,elevs^,VisPoints);

          {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile(LOSAlgorithmDescription(MDDef.wf)); {$EndIf}

          VisCount := 0;
          ThisGraph.OpenXYZFile(rfile);
          for i := 0 to Points do begin
             if VisPoints[i] then inc(VisCount);
             v[1] := 0.001 * dists^[i];
             v[2] := elevs^[i] - 50 * Nfiles;
             if VisPoints[i] then v[3] := clLime else v[3] := clRed;
             BlockWrite(rFile,v,1);
             {$IfDef RecordLOSAlgorithm}
             if VisPoints[i] then TStr := 'Visible' else TStr := 'Masked';
             WriteLineToDebugFile(RealToString(xgrids^[i],8,1) + RealToString(ygrids^[i],8,1) +
                 RealToString(dists^[i],8,1) + RealToString(Elevs^[i],8,1) + '  ' + TStr);
             {$EndIf}
          end;
          CloseFile(rFile);
          if Sender = Algorithmanalysis1 then begin
             Results.Add(RealToString(100*VisCount/Points,18,2) + '  ' + LOSAlgorithmDescription(MDDef.wf));
          end
          else begin
             Results.Add( IntegerToString(Points,5) + RealToString(Dists^[Points]/Points,15,1) + RealToString(100*VisCount/Points,18,2));
          end;
          inc(Nfiles);
       end;
   end;


begin
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('TDEMLOSF.Spacinganalysis1Click enter'); {$EndIf}
  ShowHourglassCursor;
  if Sender = Algorithmanalysis1 then GetFanCompareOptions;
  SaveBackupDefaults;
   with DEMGlb[LOSDraw.DEMonView] do begin
      new(xgrids);
      new(ygrids);
      new(dists);
      new(elevs);
      ThisGraph := TThisBaseGraph.Create(Application);
      ThisGraph.GraphDraw.HorizLabel := 'Distance (km)';
      ThisGraph.GraphDraw.VertLabel := 'Elevation (m)';
      ThisGraph.Caption := '';
      nFiles := 0;

      Results := tStringList.Create;

      if (Sender = Algorithmanalysis1) then begin
         for j := 1 to MDDef.MultipleFanAlgorithmsToUse do begin
            MDDef.wf := MDDef.CompareIVA[j];
            FigureForSpacing(round(MDDef.wf.FanDEMSpaceMultiple * DEMGlb[LOSDraw.DEMonView].AverageSpace));
         end;
      end
      else begin
         Results.Add(LOSAlgorithmDescription(MDDef.wf));
         Results.Add('Points     Point Spacing      Coverage');
         Results.Add('               (m)               (%)');
         Results.Add('========================================');
         FigureForSpacing(60);
         FigureForSpacing(45);
         FigureForSpacing(30);
         FigureForSpacing(20);
         FigureForSpacing(15);
         FigureForSpacing(10);
         FigureForSpacing(5);
         FigureForSpacing(4);
         FigureForSpacing(3);
         FigureForSpacing(2);
         FigureForSpacing(1);
      end;

      Petmar.DisplayAndPurgeStringList(Results,'Intervisibility Spacing Sensitivity');

      ThisGraph.AutoScaleAndRedrawDiagram(true,true);
      ThisGraph.Caption := 'Intervisibility Spacing Sensitivity';
      dispose(xgrids);
      dispose(ygrids);
      dispose(dists);
      Dispose(Elevs);
   end;
   RestoreBackupDefaults;
   ShowDefaultCursor;
   WMDEM.StatusBar1.Panels[0].Text := '';
{$EndIf}
end;


procedure TDEMLOSF.TrackBar1Change(Sender: TObject);
begin
   LOSDraw.LosAzimuth := TrackBar1.Position;
   Edit1.Text := IntToStr(TrackBar1.Position);
end;

procedure TDEMLOSF.TrackBar1KeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
   Adjustazimuth1Click(Sender);
end;


procedure TDEMLOSF.WaveLengthHeight1Click(Sender: TObject);
begin
   {$IfDef ExFresnel}
   {$Else}
      CloseAndNilNumberedDB(LOSDraw.LOSProfileDB);
      FormResize(Nil);
      DEMSSOCalc.DrawTopoGrainOverlay(Nil,Self,false,2);
   {$EndIf}
end;


procedure TDEMLOSF.Adjustazimuth1Click(Sender: TObject);
begin
   if (Sender = Adjustazimuth1) then ReadDefault('Azimuth  (°)',LOSDraw.LOSAzimuth);
   if (Sender = Adjustrange1)or (Sender = BitBtn2) then GetDistance(LOSDraw.FormSectLenMeters,'LOS Range');
   BitBtn2.Caption := 'Dist:  ' + SmartDistanceMetersFormat(LOSDraw.FormSectLenMeters);
   VincentyPointAtDistanceBearing(LOSDraw.LatLeft,LOSDraw.LongLeft,LOSDraw.FormSectLenMeters,LOSDraw.LOSAzimuth,LOSDraw.LatRight,LOSDraw.LongRight);
   LOSDraw.LOSExtremeElevationsAndScreenCoordinates(false);   //(DEMonView);
   if (BaseMap <> Nil) then BaseMap.DoFastMapRedraw;
   CloseAndNilNumberedDB(LOSDraw.LOSProfileDB);
   FormResize(Nil);
   ShowOnMap;
end;


procedure TDEMLOSF.ShowOnMap;
var
   XGridLeft,YGridLeft,XGridRight,YGridRight,Distance,BlockDist : float64;
   x,y,Rad : integer;
begin
{this should just plot Fresnal DB}
   DEMGlb[LOSDraw.DEMonView].LatLongDegreeToDEMGrid(LOSDraw.LatLeft,LOSDraw.LongLeft,XGridLeft,YGridLeft);
   DEMGlb[LOSDraw.DEMonView].LatLongDegreeToDEMGrid(LOSDraw.LatRight,LOSDraw.LongRight,XGridRight,YGridRight);
   if MDDef.ShowObserverMaskingCircle then begin
      BaseMap.MapDraw.LatLongDegreeToScreen(LOSDraw.LatLeft,LOSDraw.LongLeft,x,y);
      Rad := round(MDdef.MaskObsRange / BaseMap.MapDraw.ScreenPixelSize);
      Image1.Canvas.Brush.Style := bsClear;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.Ellipse(x-rad,y-rad,x+rad,y+rad);
   end;
   DEMGlb[LOSDraw.DEMonView].GridPointsIntervisible(XGridLeft,YGridLeft,MDDef.ObsAboveGround,XGridRight,YGridRight,MDDef.TargetAboveGround,Distance,BlockDist);
end;


procedure TDEMLOSF.Adjustrange1Click(Sender: TObject);
begin
   Adjustazimuth1Click(Sender);
end;

procedure TDEMLOSF.Algorithmanalysis1Click(Sender: TObject);
begin
   {$IfDef ExViewshed}
   {$Else}
      Spacinganalysis1Click(Sender);
   {$EndIf}
end;


procedure TDEMLOSF.Measureslope1Click(Sender: TObject);
begin
   LOSFormDoing := losSecondSlope;
   {$IfDef RecordSlopeCalc} WriteLineToDebugFile('Start losSecondSlope, at x=' + IntToStr(FirstLOSX) + ',  y=' + IntToStr(FirstLOSY)); {$EndIf}
end;


procedure TDEMLOSF.Parallelprofiles1Click(Sender: TObject);
begin
    AverageProfileGraph(DEMGlb[LOSDraw.DEMonView].SelectionMap,LOSDraw.DEMonView,LOSDraw.LatLeft,LOSDraw.LongLeft,LOSDraw.LatRight,LOSDraw.LongRight);
end;


function AverageProfileGraph(Map : tMapForm; DEM : Integer; LatLeft,LongLeft,LatRight,LongRight : float64) :  TThisBaseGraph;
var
   avdists,avelevs,
   xgrids,ygrids,dists,elevs : ^Petmath.bfarray32;
   i,Points : integer;
   Distance,LOSAzimuth : float64;
   ThisGraph : TThisBaseGraph;
   rfile : file;
   v : array[1..2] of float32;

   procedure FigureForSpacing(Angle,Separation : integer);
   var
      i,x1,y1,x2,y2 : integer;
      LatLeft2,LongLeft2,LatRight2,LongRight2 : float64;
      VisPoints : array[0..MaxScreenXMax] of boolean;
   begin
      {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('  Points=' + IntToStr(Points)); {$EndIf}
       VincentyPointAtDistanceBearing(LatLeft,LongLeft,Separation,LOSAzimuth+Angle,LatLeft2,LongLeft2);
       VincentyPointAtDistanceBearing(LatRight,LongRight,Separation,LOSAzimuth+Angle,LatRight2,LongRight2);

       DEMGlb[DEM].SelectionMap.MapDraw.LatLongDegreeToScreen(LatLeft2,LongLeft2,x1,y1);
       DEMGlb[DEM].SelectionMap.MapDraw.LatLongDegreeToScreen(LatRight2,LongRight2,x2,y2);
       DEMGlb[DEM].SelectionMap.Image1.Canvas.MoveTo(x1,y1);
       DEMGlb[DEM].SelectionMap.Image1.Canvas.LineTo(x2,y2);

       DEMGlb[DEM].GetStraightRouteDEMGrid(LatLeft2,LongLeft2,LatRight2,LongRight2,MDDef.wf.StraightAlgorithm,Points,xgrids^,ygrids^,dists^);
       DEMGlb[DEM].GetVisiblePoints(MDDef.ObsAboveGround,MDDef.TargetAboveGround,-89,89,true,true,Points,xgrids^,ygrids^,dists^,elevs^,VisPoints);

       if MDDef.ShowAllProfiles then ThisGraph.OpenDataFile(rfile);

       for i := 0 to Points do if (elevs^[i] < 32000) then begin
          v[1] := 0.001 * avdists^[i];
          v[2] := elevs^[i];
          if MDDef.ShowAllProfiles then BlockWrite(rFile,v,1);
          if (ThisGraph.GraphDraw.DataFilesPlotted.Count = 1) then begin
             avelevs^[i] := elevs^[i];
          end
          else begin
             avelevs^[i] := (avelevs^[i] * pred(ThisGraph.GraphDraw.DataFilesPlotted.Count) + elevs^[i]) / ThisGraph.GraphDraw.DataFilesPlotted.Count;
          end;
       end;
       if MDDef.ShowAllProfiles then CloseFile(rFile);
       dec(Points);
   end;


begin
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('TDEMLOSF.Parallelprofiles1Click enter'); {$EndIf}
   ShowHourglassCursor;
   new(xgrids);
   new(ygrids);
   new(dists);
   new(elevs);
   New(avdists);
   new(avelevs);
   VincentyCalculateDistanceBearing(LatLeft,LongLeft,LatRight,LongRight,Distance,LOSAzimuth);

   ReadDefault('Profile separation (m)', MDDef.LOSParallelProfileSep);
   ReadDefault('Profiles each side (1-6)', MDDef.NumLOSParallelProfile);
   if (MDDef.NumLOSParallelProfile > 6) then  MDDef.NumLOSParallelProfile := 6;

   ThisGraph := TThisBaseGraph.Create(Application);
   ThisGraph.GraphDraw.HorizLabel := 'Distance (km)';
   ThisGraph.GraphDraw.VertLabel := 'Elevation (m)';
   ThisGraph.BaseCaption := 'Parallel Profiles';
   ThisGraph.VertCompare := 0.001;
   for i := 1 to 255 do ThisGraph.GraphDraw.FileColors256[i] := claSilver;
   for i := 1 to 15 do ThisGraph.GraphDraw.ShowPoints[i] := false;
   for i := 1 to 15 do ThisGraph.GraphDraw.ShowLine[i] := true;
   ThisGraph.GraphDraw.FileColors256[1] := claRed;

   Points := round(MDDef.wf.FanDEMSpaceMultiple * DEMGlb[DEM].AverageSpace);
   if (Points < 1) then Points := 1;
   Points := round(Distance/Points);
   if (Points > MaxScreenXMax) then Points := MaxScreenXMax;
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('First Points=' + IntToStr(Points)); {$EndIf}

   DEMGlb[DEM].GetStraightRouteDEMGrid(LatLeft,LongLeft,LatRight,LongRight,MDDef.wf.StraightAlgorithm,Points,xgrids^,ygrids^,avdists^);
   Dec(Points);

   FigureForSpacing(0,0);

   for i := 1 to MDDef.NumLOSParallelProfile do begin
      FigureForSpacing(90,i * MDDef.LOSParallelProfileSep);
      FigureForSpacing(-90,i * MDDef.LOSParallelProfileSep);
   end;

    ThisGraph.OpenDataFile(rfile);
    ThisGraph.GraphDraw.FileColors256[(ThisGraph.GraphDraw.DataFilesPlotted.Count)] := claLime;
    for i := 0 to Points do begin
       v[1] := 0.001 * avdists^[i];
       v[2] := avelevs^[i];
       BlockWrite(rFile,v,1);
    end;
    CloseFile(rFile);

   ThisGraph.AutoScaleAndRedrawDiagram(true,true);
   dispose(xgrids);
   dispose(ygrids);
   dispose(dists);
   Dispose(Elevs);
   Dispose(avdists);
   Dispose(avelevs);
   ShowDefaultCursor;
   WMDEM.StatusBar1.Panels[0].Text := '';
end;


procedure TDEMLOSF.Pastefromclipboard1Click(Sender: TObject);
begin
   DragBitmap := tMyBitmap.Create;
   DragBitmap.Assign(ClipBoard);
   CopyImageToBitmap(Image1,SavedMapImage);
   DragEdit := true;
end;

procedure TDEMLOSF.Profilenames1Click(Sender: TObject);
var
   WhichDEM : integer;
begin
   {$IfDef RecordLOSProblems} WriteLineToDebugFile('TDEMLOSF.Profilenames1Click');{$EndIf}
   for WhichDEM := 1 to NumDEMDataSetsOpen do begin
      if (DEMGlb[WhichDEM] <> Nil) then begin
         {$IfDef RecordLOSProblems} WriteLineToDebugFile('DEM=' + IntToStr(WhichDEM) + '   ' + LOSDraw.ProfileName[WhichDEM]);  {$EndIf}
         Petmar.GetString('Profile Name',LOSDraw.ProfileName[WhichDEM],false,ReasonableTextChars);
         {$IfDef RecordLOSProblems} WriteLineToDebugFile('   Changed to  ' + LOSDraw.ProfileName[WhichDEM]); {$EndIf}
      end;
   end;
   FormResize(Sender);
end;


procedure TDEMLOSF.ProfilesonotherDEMs1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to MaxDEMDataSets do begin
       if ValidDEM(i) and ((LOSDraw.LOSVariety = losAllDEMs) or (I <> LOSDraw.DEMonView)) then begin
          StartLOS(true,JustWandering,i,LOSDraw.LatLeft,LOSDraw.LongLeft,LOSDraw.LatRight,LOSDraw.LongRight,DEMGlb[i].SelectionMap);
       end;
   end;
end;

procedure TDEMLOSF.Protractor1Click(Sender: TObject);
begin
   if (ProtractorTool = Nil) then ProtractorTool := CreateProtractor(true,false,LOSdraw.FormVertExag);
   if (BaseBMP = Nil) then CopyImageToBitmap(Image1,BaseBMP);
end;


procedure TDEMLOSF.Profiledropdown1Click(Sender: TObject);
begin
   LOSdraw.MinAreaZ := LOSdraw.MinAreaZ + NumDEMDataSetsOpen * MDDef.DropDownPerProfile;
   ReadDefault('Profile drop down',MDDef.DropDownPerProfile);
   LOSdraw.MinAreaZ := LOSdraw.MinAreaZ - NumDEMDataSetsOpen * MDDef.DropDownPerProfile;
   if (MDDef.DropDownPerProfile > 0) then LOSdraw.LOSVariety := losAllDEMDropDown
   else LOSdraw.LOSVariety := losAllDEMs;
   FormResize(Sender);
end;


procedure TDEMLOSF.Profilelegends1Click(Sender: TObject);
var
   y,x,WhichDEM,ItemHigh,LegendItem,DEMshown : integer;
   LegBMP : tMyBitmap;
begin
   {$If Defined(RecordLOSProblems) or Defined(RecordLOSLegend)} WriteLineToDebugFile('TDEMLOSF.Profilelegends1Click'); {$EndIf}
   CreateBitmap(LegBMP,1,1);
   LegBMP.Canvas.Font.Name := MDDef.DefaultGraphFont.Name;
   LegBMP.Canvas.Font.Size := MDDef.DefaultGraphFont.Size;
   LegBMP.Canvas.Font.Style := [fsBold];

   y := 0;
   DEMShown := 0;
   for WhichDEM := 1 to MaxDEMDataSets do begin
      if ValidDEM(WhichDEM) and LOSDraw.ShowProfile[WhichDEM] then begin
         {$IfDef RecordLOSProblems} WriteLineToDebugFile('DEM=' + IntToStr(WhichDEM) + '   ' + LOSDraw.ProfileName[WhichDEM]); {$EndIf}
         inc(DEMShown);
         x := LegBMP.Canvas.TextWidth(LOSdraw.ProfileName[WhichDEM]);
         if (x > y) then y := x;
         ItemHigh := LegBMP.Canvas.TextHeight(LOSdraw.ProfileName[WhichDEM]);
      end;
   end;
   ItemHigh := ItemHigh + 10;
   LegBMP.Width := 40+y;
   LegBMP.Height := 15 + NumDEMDataSetsOpen*(ItemHigh);
   {$If Defined(RecordLOSProblems) or Defined(RecordLOSLegend)}  WriteLineToDebugFile('Legend size: ' + IntToStr(LegBMP.Width) + 'x' + IntToStr(LegBMP.Height) ); {$EndIf}

   LegBMP.Canvas.Brush.Color := clWhite;
   LegBMP.Canvas.Brush.Style := bsSolid;
   LegBMP.Canvas.Pen.Color := clBlack;
   LegBMP.Canvas.Pen.Width := 0;
   LegBMP.Canvas.Rectangle(0,0,pred(LegBMP.Width),pred(LegBMP.Height));
   LegBMP.Canvas.Brush.Style := bsClear;

   LegendItem := 0;
   DEMShown := 0;
   for WhichDEM := 1 to MaxDEMDataSets do begin
      if ValidDEM(WhichDEM) then begin
         inc(DEMshown);
         if LOSDraw.ShowProfile[WhichDEM] then begin
            inc(LegendItem);
            {$If Defined(RecordLOSProblems) or Defined(RecordLOSLegend)}  WriteLineToDebugFile('Item=' + IntToStr(DEMshown) + ' ' +  LOSdraw.ProfileName[WhichDEM] + '  y=' + IntToStr(5+ItemHigh*pred(LegendItem))); {$EndIf}
            LegBMP.Canvas.Pen.Color := ConvertPlatformColorToTColor(LineColors256[DEMshown]);
            LegBMP.Canvas.Pen.Width := LineSize256[DEMshown];
            LegBMP.Canvas.Font.Color := LegBMP.Canvas.Pen.Color;
            LegBMP.Canvas.TextOut(35,5+ItemHigh*pred(LegendItem),LOSdraw.ProfileName[WhichDEM]);
            LegBMP.Canvas.MoveTo(5,ItemHigh*pred(LegendItem) + ItemHigh div 2);
            LegBMP.Canvas.LineTo(30,ItemHigh*pred(LegendItem) + ItemHigh div 2);
         end;
      end;
   end;
   PetImage_form.DisplayBitmap(LegBMP,'Profiles');
   //LegBMP.Free;
end;

procedure TDEMLOSF.AllopenDEMs1Click(Sender: TObject);
begin
   LOSdraw.LOSVariety := losAllDEMs;
   FormResize(Sender);
end;


procedure TDEMLOSF.Averageelevation1Click(Sender: TObject);
begin
   {$IfDef ExFresnel}
   {$Else}
      GISdb[LOSdraw.LOSProfileDB].DisplayFieldStatistics('ELEV_M');
   {$EndIf}
end;


procedure TDEMLOSF.OpenGLofPointCloud1Click(Sender: TObject);
begin
   LoadProfilePointCloud;
   //ObsZ := ObsGroundElev + MDDef.ObsAboveGround;
   //TarZ := TargetGroundElev + MDDef.TargetAboveGround;
   //ogl_DEMOpenGLmain.StartPointCloudViewer;
end;

procedure TDEMLOSF.Openprofiledatabase1Click(Sender: TObject);
begin
  {$IfDef ExFresnel}
  {$Else}
      if (LOSdraw.LOSProfileDB <> 0) then GISdb[LOSdraw.LOSProfileDB].DisplayTable;
  {$EndIf}
end;


{$IfDef ExLASLidar}
{$Else}

   procedure TDEMLOSF.LoadProfilePointCloud(ShowLoadProgress : boolean = true);
   var
      Bitmap : tMyBitmap;
      Lat,Long,Rotate : float64;
      PointsInPolyLine,i : integer;
      BoundBox : sfBoundBox;
      fName : PathStr;
      PolyLinePoints : array[0..4] of tPoint;
      ObsX,ObsY,ObsZ,TarX,TarY,TarZ : float64;
   begin
      with LOSdraw do begin
         if (Point_Cloud_Options.pt_cloud_opts_fm <> Nil) then begin
            if (LOSMemoryPointCloud[PtCldInUse] = Nil) then begin
               CloseAndNilNumberedDB(LOSProfileDB);
               CloneImageToBitmap(BaseMap.Image1,Bitmap);
               PointsInPolyLine := 5;

               {$IfDef RecordPointClouds} WriteLineToDebugFile('TDEMLOSF.LoadProfilePointCloud, Map UTM zone=' + IntToStr(BaseMap.MapDraw.PrimMapProj.projUTMZone)); {$EndIf}

               VincentyPointAtDistanceBearing(LatLeft,LongLeft,1.414 * MDDef.LOSSliceBuffer,LOSAzimuth -135,Lat,Long);
               BaseMap.MapDraw.LatLongDegreeToScreen(Lat,Long,PolyLinePoints[0].x,PolyLinePoints[0].y);
               VincentyPointAtDistanceBearing(LatLeft,LongLeft,1.414 * MDDef.LOSSliceBuffer,LOSAzimuth +135,Lat,Long);
               BaseMap.MapDraw.LatLongDegreeToScreen(Lat,Long,PolyLinePoints[1].x,PolyLinePoints[1].y);
               VincentyPointAtDistanceBearing(LatRight,LongRight,1.414 * MDDef.LOSSliceBuffer,LOSAzimuth +45,Lat,Long);
               BaseMap.MapDraw.LatLongDegreeToScreen(Lat,Long,PolyLinePoints[2].x,PolyLinePoints[2].y);
               VincentyPointAtDistanceBearing(LatRight,LongRight,1.414 * MDDef.LOSSliceBuffer,LOSAzimuth -45,Lat,Long);
               BaseMap.MapDraw.LatLongDegreeToScreen(Lat,Long,PolyLinePoints[3].x,PolyLinePoints[3].y);

               PolyLinePoints[4].x := PolyLinePoints[0].x;
               PolyLinePoints[4].y := PolyLinePoints[0].y;
               Bitmap.Canvas.Pen.Color := clRed;
               Bitmap.Canvas.Brush.Color := clRed;
               Bitmap.Canvas.Brush.Style := bsSolid;
               Bitmap.Canvas.Polygon(slice(PolyLinePoints,PointsInPolyLine));

               BaseMap.MapDraw.LatLongDegreeToUTM(LatLeft,LongLeft,ObsX,ObsY);
               BaseMap.MapDraw.LatLongDegreeToUTM(LatRight,LongRight,TarX,TarY);

               Rotate := HeadingOfLine((TarX-ObsX),(TarY-ObsY));
               BoundBox.XMin := Min(LongLeft,LongRight);
               BoundBox.XMax := Max(LongLeft,LongRight);
               BoundBox.YMin := Min(LatLeft,LatRight);
               BoundBox.YMax := Max(LatLeft,LatRight);

               {$IfDef RecordPointClouds} WriteLineToDebugFile('TDEMLOSF.OpenGLofPointCloud1Click target, x=' + RealToString(ObsX,-18,-2) + ' y=' + RealToString(ObsY,-18,-2) + ' Rot=' + RealToString(LOSAzimuth,-18,-2)); {$EndIf}

               SliceDx := -ObsX;
               SliceDy := -ObsY;
               SliceRotate := Rotate;
               for i := 1 to 2 do begin
                  if (Point_Cloud_Options.pt_cloud_opts_fm.LasFiles[i] <> Nil) then begin
                      Point_Cloud_Options.pt_cloud_opts_fm.LasFiles[i].ShowLASProgress := false;
                      Point_Cloud_Options.pt_cloud_opts_fm.LasFiles[i].OpenGLLasPoints(fName,BaseMap,Bitmap);
                      LOSMemoryPointCloud[i] := tMemoryPointCloud.Create(FName,BaseMap);
                      LOSMemoryPointCloud[i].TranslateAndRotate(Slicedx, Slicedy, SliceRotate);
                  end;
               end;
               Bitmap.Free;
               if ShowLoadProgress then FormResize(Nil);
            end;
         end;
      end;
   end;

{$EndIf}


procedure InitializeDEMLosW;
begin
   {$IfDef ExMagAnom}
   {$Else}
      SpreadRidge := 30;
      BlockIntensity := 30;
      MagI := 90;
      MagAlpha := 0;
      MagTimeScale := true;
      MagAnomNames := true;
   {$EndIf}
   DefaultGraphColors(Symbol,LineColors256,LineSize256);
   MDDef.ForceCrestComputations := false;
end;


procedure TDEMLOSF.LineOfSightFromLatLong;
var
   Bitmap : tMyBitmap;
   TStr : shortstring;
begin
  {$If Defined(RecordLOSProblems) or Defined(RecordLOSDraw)} WriteLineToDebugFile('enter TDEMLOSF.LineOfSightFromLatLong  ' + FormClientSize(Self) ); {$EndIf}

   CreateBitmap(Bitmap,Image1.ClientWidth,Image1.ClientHeight);
   LOSDraw.SetSize(Bitmap,ClientWidth,ClientHeight);

   {$IfDef ExMagAnom}
   {$Else}
        if (LOSdraw.LOSVariety in [losSimpleMagModel,losMagModel]) then begin
           StartMagModel;
           MarineMagneticAnomalies;
           exit;
        end;
    {$EndIf}

   LOSdraw.CreateProfileBMP(Bitmap);

   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;

   TStr := ', V.E.=' + RealToString(LOSDraw.FormVertExag,-12,-2);

   if LOSDraw.LOSVariety in [losAllDEMs,losAllDEMDropDown] then Caption := 'Terrain Profiles' + TStr
   {$IfDef ExMagAnom}{$Else} else if (LOSdraw.LOSVariety in [losMagModel,losSimpleMagModel]) then Caption := 'Magnetic models' {$EndIf}
   else if LOSdraw.LOSVariety in [losSimpleOne] then Caption := 'Terrain profile' + TStr
   else if LOSdraw.LOSVariety in [losVanilla] then Caption := 'LOS' + TStr
   else Caption := 'Multiple Profiles' + TStr;

   Edit1.Text := RealToString(LOSDraw.LOSAzimuth,-12,-3);
   BitBtn2.Caption := 'Dist:  ' + SmartDistanceMetersFormat(LOSDraw.FormSectLenMeters);
   ShowDefaultCursor;

  {$If Defined(RecordLOSProblems) or Defined(RecordLOSDraw)}  WriteLineToDebugFile('exit TDEMLOSF.LineOfSightFromLatLong'); {$EndIf}
end;


procedure TDEMLOSF.FormResize(Sender: TObject);
begin
   if RespondToResize then begin
       {$IfDef RecordLOSProblems}  ScreenDimensions('TDEMLOSF.FormResize in');    {$EndIf}
       RespondToResize := false;
       if ActuallyDraw then FormStyle := fsMDIChild;
       ClearProtractorTool;
       Image1.Align := alClient;
       LineOfSightFromLatLong;
       RespondToResize := true;
       {$IfDef RecordLOSProblems}  ScreenDimensions('TDEMLOSF.FormResize out');  {$EndIf}
   end;
end;


initialization
   InitializeDEMLosW;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demlosw in');   {$EndIf}
   {$IfDef RecordLOSProblems} WriteLineToDebugFile('RecordLOSProblems active in DEMLOSW');{$EndIf}
   {$IfDef RecordAllLOSProblems} WriteLineToDebugFile('RecordAllLOSProblems active in DEMLOSW');{$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosing active in DEMLOSW');{$EndIf}
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('RecordLOSAlgorithm active in DEMLOSW');{$EndIf}
   {$IfDef RecordWaveLenghtHeightProblems} WriteLineToDebugFile('RecordWaveLenghtHeightProblems active in DEMLOSW'); {$EndIf}
   {$IfDef RecordNearestCrest} WriteLineToDebugFile('RecordNearestCrest active in DEMLOSW'); {$EndIf}
   {$IfDef RecordThreadCrest} WriteLineToDebugFile('RecordThreadCrest active in DEMLOSW'); {$EndIf}
   {$IfDef RecordPointClouds} WriteLineToDebugFile('RecordPointClouds active in DEMLOSW'); {$EndIf}
   {$IfDef RecordMGTProblems} WriteLineToDebugFile('RecordMGTProblems active in DEMLOSW'); {$EndIf}
   {$IfDef RecordLOSPrettyDrawing} WriteLineToDebugFile('RecordLOSPrettyDrawing active in DEMLOSW'); {$EndIf}
   {$IfDef RecordSlopeCalc} WriteLineToDebugFile('RecordSlopeCalc active in DEMLOSW'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demlosw out');   {$EndIf}
end.




