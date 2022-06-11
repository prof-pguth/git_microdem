 unit register_view;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of DragonPlot Program      }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/24/2011       }
{_________________________________}


{$I Nevadia_defines.inc}


{$IfDef RecordProblems}
  //{$Define RecordTowerProblems}
  //{$Define RecordCheckCameraParamters}
  {$Define RecordCreateBlowUpMap}
  {$Define RecordDrawSighting}

  //{$Define RecordDefineDatumProblems}
  //{$Define RecordClosingProblems}
  //{$Define RecordPostResultsProblems}
  //{$Define RecordViewshedProblems}
  //{$Define RecordOpenDEM}
  {$Define RecordKML}
  {$Define RecordGazetteer}
  //{$Define RecordTemporaryTowers}
  //{$Define DebugDefaults}
  //{$Define RecordLightningProblems}
  //{$Define RecordFigureViewProblems}
  //{$Define RecordPerspectiveProblems}
  //{$Define CheckAccessibility}
  //{$Define RecordHiResDEM}
{$EndIf}

interface

uses
//needed for inline of the core DB functions
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
// end units for inline of the core DB functions

   Windows,  SysUtils,Classes, Graphics,  Forms,Messages, System.Diagnostics,System.TimeSpan,System.Math,
   Grids, DBGrids,
   {$IfDef ExSats}
   {$Else}
   DEMEros,
   {$EndIf}
   DEMMapf,Petmar_types,DEMLOSW,DEMDataBase,DEMdefs,
   DEMPersw,PetImage,PetImage_form,PETMAR, Vcl.Dialogs, Vcl.StdCtrls,
   Vcl.ExtCtrls, Vcl.Buttons, Vcl.Controls, Vcl.ComCtrls;

type
  TPhotoRegForm = class(TForm)
    Edit5: TEdit;
    Label5: TLabel;
    Label10: TLabel;
    Edit4: TEdit;
    Label4: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Edit1: TEdit;
    Edit2: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    Edit9: TEdit;
    Label3: TLabel;
    Label9: TLabel;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Edit6: TEdit;
    Edit8: TEdit;
    Memo1: TMemo;
    TabSheet4: TTabSheet;
    Label12: TLabel;
    Edit11: TEdit;
    CheckBox2: TCheckBox;
    PanoramaSpeedButton: TSpeedButton;
    Label15: TLabel;
    Label16: TLabel;
    Edit12: TEdit;
    Edit13: TEdit;
    GroupBox1: TGroupBox;
    Edit3: TEdit;
    GroupBox2: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Edit14: TEdit;
    Edit15: TEdit;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label19: TLabel;
    Panel1: TPanel;
    TabSheet6: TTabSheet;
    DBGrid1: TDBGrid;
    SetSightButton: TBitBtn;
    BitBtn6: TBitBtn;
    TabSheet7: TTabSheet;
    ComboBox3: TComboBox;
    GroupBox3: TGroupBox;
    Label20: TLabel;
    Label21: TLabel;
    Edit16: TEdit;
    Edit17: TEdit;
    Label22: TLabel;
    Label23: TLabel;
    CrossShotBitBtn8: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    FontDialog1: TFontDialog;
    TabSheet8: TTabSheet;
    BitBtn12: TBitBtn;
    Label24: TLabel;
    BitBtn13: TBitBtn;
    Label26: TLabel;
    BitBtn14: TBitBtn;
    RadioGroup1: TRadioGroup;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label29: TLabel;
    ComboBox5: TComboBox;
    Edit21: TEdit;
    Label30: TLabel;
    BitBtn4: TBitBtn;
    BitBtn3: TBitBtn;
    RadioGroup3: TRadioGroup;
    Label13: TLabel;
    Label14: TLabel;
    Edit19: TEdit;
    Edit20: TEdit;
    TabSheet5: TTabSheet;
    TabSheet9: TTabSheet;
    ComboBox4: TComboBox;
    BitBtn23: TBitBtn;
    BitBtn24: TBitBtn;
    BitBtn25: TBitBtn;
    BitBtn26: TBitBtn;
    Resotr: TBitBtn;
    BitBtn27: TBitBtn;
    BitBtn28: TBitBtn;
    Pitch: TLabel;
    Edit23: TEdit;
    Label33: TLabel;
    Edit24: TEdit;
    BitBtn29: TBitBtn;
    BitBtn7: TBitBtn;
    CheckBox99: TCheckBox;
    Edit25: TEdit;
    Edit26: TEdit;
    Suze: TLabel;
    Label34: TLabel;
    BitBtn11: TBitBtn;
    CheckBox10: TCheckBox;
    Panel2: TPanel;
    BitBtn18: TBitBtn;
    ComboBox6: TComboBox;
    BitBtn21: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn17: TBitBtn;
    SpeedButton1: TSpeedButton;
    Label28: TLabel;
    Edit27: TEdit;
    BitBtn1: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn22: TBitBtn;
    BitBtn30: TBitBtn;
    CheckBox1: TCheckBox;
    BitBtn31: TBitBtn;
    BitBtn32: TBitBtn;
    BitBtn33: TBitBtn;
    //BitBtn5: TBitBtn;
    RadioGroup2: TRadioGroup;
    CheckBox3: TCheckBox;
    BitBtn35: TBitBtn;
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PanoramaSpeedButtonClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure CrossShotBitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BitBtn10Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure Edit12Change(Sender: TObject);
    procedure Edit13Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit19Change(Sender: TObject);
    procedure Edit20Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure DBGrid2DblClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure Edit24Change(Sender: TObject);
    procedure BitBtn29Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure CheckBox99Click(Sender: TObject);
    procedure Edit25Change(Sender: TObject);
    procedure Edit26Change(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure Edit27Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn30Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtn31Click(Sender: TObject);
    procedure BitBtn32Click(Sender: TObject);
    procedure BitBtn33Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn35Click(Sender: TObject);
  private
    { Private declarations }
     function CheckCameraParameters(LoadBlowup : boolean = true; CrossShot : boolean = false) : boolean;
     procedure GetTowerVariables(TheName : ShortString);
     procedure ClosePerspectives;
     procedure ArchiveShot;
     procedure DisplayMessage(Words : shortString);
     procedure WMBroadcastLatLongMessage(var Msg : TMessage);  message WM_BroadcastLatLongMessage;
     procedure CheckAccessibilityMap(Lat,Long : float; Primary : boolean);
     procedure CreateProfile(var DEMLOSF : TDEMLOSF; TowerLat,TowerLong,PLSSLat,PLSSLong,TowerHeight : float; LabelStr : ShortString);
     procedure ShowOnMaps(Lat, Long: float; Primary: boolean);
     procedure InsureLocationMapOK;
     procedure AddGazetteerToTargetMap;
  public
    { Public declarations }
     DEMPersF2 : TThreeDview;
     DEMPersF  : array[1..3] of TThreeDview;
     {$IfDef ExSats}
     {$Else}
     ImagePersF  : array[1..MaxSatAllowed] of TThreeDview;
     {$EndIf}
     DEMLOSF,DEMLOSF2 : TDEMLOSF;
     DEMPersPan : TThreeDview;
     RegionalDEM,ComputeDEM,
     LightningTable : integer;
     AccessMap,
     TargetMap : tMapForm;
     fNameViewed : PathStr;
     PLSSEntered : boolean;
     PLSSString : shortstring;
     LastGPSLat,LastGPSLong,
     plssLat,plssLong : float;
     LastSighting : tStringList;
     TowerLat,TowerLong,TowerGroundElevation,TowerHeight,TowerRange,CameraElev,Depress,Azimuth,Pitch2,
     TowerLat2,TowerLong2,Tower2GroundElevation,TowerHeight2,CameraElev2,Depress2,Azimuth2,
     Depth,HFOV,VFOV,GroundElevation,Roll : float;
     procedure LoadDEM;
     procedure LoadSatelliteImage;
     procedure CloseWindows;
     procedure LoadTowersIntoComboBoxes;
     procedure ShowAdvancedOptions;
     procedure DrawTargetMap(var UseMap : tMapForm; Lat,Long : float; Primary : boolean);
     procedure LoadTowersOnMap(TheMap : tMapForm);
     procedure DrawSightingsOnMap(var UseMap : tMapForm; Lat,Long : float; Primary : boolean);
  end;


const
  TopWindow2 = 30;
var
   PhotoForm      : TImageFm;

implementation

{$R *.dfm}

uses
   {$IfDef ExGPS}
   {$Else}
   GPSCommo,
   {$EndIf}
   {$IfDef ExSats}
   {$Else}
   DEMEROSM,
   {$EndIf}
   {$IfDef ExKML}
   {$Else}
   kml_creator,
   {$EndIf}
   Dragon_Plot_Init,
   DEM_Indexes,
   GetLatLn,
   DEMDef_routines,
   DEMDatum,
   Make_Tables,
   toggle_db_use,
   Read_DEM,
   DEM_Manager,
   KML_opts,
   US_Properties,
   DEMCoord,
   PetMath,
   PETdbUtils,
   DEM_PLSS,
   Get_PLSS,
   Nevadia_Main, map_overlays, dem_browser, DataBaseCreate;


procedure TPhotoRegForm.DrawSightingsOnMap(var UseMap : tMapForm; Lat,Long : float; Primary : boolean);
var
   x,y : integer;
   Bitmap : tBitmap;
begin
   {$IfDef RecordDrawSighting}
   WriteLineToDebugFile('TPhotoRegForm.DrawSightingsOnMap in, ' + UseMap.MapDraw.MapSizeString);
   {$EndIf}
   UseMap.DoFastMapRedraw;
   {$IfDef RecordDrawSighting}
   WriteLineToDebugFile('TPhotoRegForm.DrawSightingsOnMap FastMapRedraw over');
   {$EndIf}
   UseMap.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
   CopyImageToBitmap(UseMap.Image1,Bitmap);
   ScreenSymbol(Bitmap.Canvas,x,y,DragonPlotDef.ResultSymbol);
   PetImage.PlotOrientedLine(Bitmap,x,y,200,Azimuth,DragonPlotDef.ShotLineColor,DragonPlotDef.ShotLineWidth,true);
   if (not Primary) then begin
     PetImage.PlotOrientedLine(Bitmap,x,y,200,Azimuth2,DragonPlotDef.ShotLineColor,DragonPlotDef.ShotLineWidth,true);
   end;
   UseMap.Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   {$IfDef RecordDrawSighting}
   WriteLineToDebugFile('TPhotoRegForm.DrawSightingsOnMap out');
   {$EndIf}
end;


procedure TPhotoRegForm.LoadTowersOnMap(TheMap : tMapForm);
begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('LoadTowersOnMap, file= ' + DragonPlotDef.TowerFileName);
   {$EndIf}
   OpenNumberedGISDataBase(WMDEM.TowerTable,DragonPlotDef.TowerFileName,false,TheMap);
   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   GISdb[WMDEM.TowerTable].dbOpts.LabelDBPlots := true;
   GISdb[WMDEM.TowerTable].KMLExportable := false;
   Map_Overlays.AddOrSubtractOverlay(TheMap,ovoDatabases,True);
   GISdb[WMDEM.TowerTable].DBOpts.Symbol.DrawingSymbol := DragonPlotDef.TowerSymbol.DrawingSymbol;
   GISdb[WMDEM.TowerTable].dbOpts.Symbol.Color := DragonPlotDef.TowerSymbol.Color;
   GISdb[WMDEM.TowerTable].dbOpts.Symbol.Size := DragonPlotDef.TowerSymbol.Size;
   GISdb[WMDEM.TowerTable].dbOpts.DBAutoShow := dbasDefault;
   GISdb[WMDEM.TowerTable].dbOpts.LabelField := 'NAME';
   GISdb[WMDEM.TowerTable].DoNotTrackDBSettings := true;
   LoadTowersIntoComboBoxes;
end;


procedure TPhotoRegForm.LoadSatelliteImage;
begin
   {$IfDef ExSats}
   {$Else}
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.LoadSatelliteImage ' + LastImageName);
   {$EndIf}
   OpenAndDisplayNewScene(Nil,LastImageName,true,false,true);
   if (SatImage[1] <> Nil) then begin
      SatImage[1].SelectionMap.MapDraw.MapType := mtSATImage;
      SatImage[1].SelectionMap.MapDraw.DrawTerrainShadows := DragonPlotDef.TerrainShadows;
      SatImage[1].SelectionMap.PickBandSpeedButton20Click(Nil);
      SatImage[1].SelectionMap.WindowState := wsMinimized;
      ShowDefaultCursor;
   end;
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.LoadSatelliteImage OK ' + LastImageName);
   {$EndIf}
   {$EndIf}
end;



procedure TPhotoRegForm.LoadDEM;
var
   fName : PathStr;
   Ext : ExtStr;
begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.LoadDEM ' + LastDEMName);
   {$EndIf}
   RegionalDEM := 0;
   ComputeDEM := 0;
   if (MDdef.AutoOpen in [aoDEM,aoNothing]) then begin
      Read_DEM.LoadNewDEM(RegionalDEM,LastDEMName,true,'regional DEM','',false);
   end
   else if (MDdef.AutoOpen = aoProject) then begin
      RestoreSpecifiedDesktop(LastDesktop);
      RegionalDEM := 1;
   end;

   {$IfDef RecordOpenDEM}
   WriteLineToDebugFile('Regional DEM=' + LastDEMName);
   {$EndIf}

   if (NumDEMDataSetsOpen = 0) then begin
      {$IfDef RecordOpenDEM}
      WriteLineToDebugFile('Close because no DEM');
      {$EndIf}
      exit;
   end
   else begin
      {$IfDef DragonPlotDemo}
      if (DEMGlb[WantedDEM].AverageSpace < 100) then begin
          ThinFactor := 2;
          while ThinFactor * DEMGlb[WantedDEM].AverageSpace < 100 do inc(ThinFactor);
          DEMGlb[WantedDEM].ThinThisDEM(ThinDEM,ThinFactor);
          CloseSingleDEM(WantedDEM);
          LastDEMName := MDTempDir + 'thinned_dem.dem';
          DEMGlb[ThinDEM].WriteNewFormatDEM(LastDEMName);
          CloseSingleDEM(ThinDEM);
          WantedDEM := 0;
          Read_DEM.LoadNewDEM(WantedDEM,LastDEMName,true,'','',false);
          MessageToContinue('Demo program can only use 100 m DEMs');
      end;
      {$EndIf}
      DEMGlb[RegionalDEM].SelectionMap.Closable := false;
      DEMGlb[RegionalDEM].SelectionMap.MapDraw.BaseTitle := 'Regional fire map, ' + DEMGlb[RegionalDEM].AreaName;
      DEMGlb[RegionalDEM].SelectionMap.Caption := DEMGlb[RegionalDEM].SelectionMap.MapDraw.BaseTitle;
      DEMGLB[RegionalDEM].SelectionMap.Left := 0;
      DEMGLB[RegionalDEM].SelectionMap.Top := 0;
      Azimuth := Azimuth + DEMGlb[RegionalDEM].SelectionMap.MapDraw.MapMagDec;
      DEMglb[RegionalDEM].GetElevFromLatLongDegree(TowerLat,TowerLong,GroundElevation);
      BitBtn2.Enabled := true;
      {$IfDef RecordOpenDEM}
      WriteLineToDebugFile('DEM loaded');
      {$EndIf}
   end;

   {$IfDef ExPLSS}
   {$Else}
      if MDDef.PLSSDef.AutoDrawPLSS then begin
         {$IfDef RecordProblems}
         WriteLineToDebugFile('Loading PLSS '+ LastPLSSfile);
         {$EndIf}
         TryToOpenPLSS(DEMGlb[RegionalDEM].SelectionMap);
      end;
      BitBtn12.Enabled := (PLSS[1] <> Nil);
   {$EndIf}

   if (DragonPlotDef.DefaultShapeFile <> '') then begin
      {$IfDef RecordProblems}
      WriteLineToDebugFile('Loading default shapefile');
      {$EndIf}
      DEMGlb[RegionalDEM].SelectionMap.OpenDBonMap('',DragonPlotDef.DefaultShapeFile,true,false);
   end;

   if not FileExists(DragonPlotDef.TowerFileName) then DragonPlotDef.TowerFileName := '';
   Ext := UpperCase(ExtractFileExt(DragonPlotDef.TowerFileName));
   if (Ext <> UpperCase(Petmar_types.DefaultDBExt)) then begin
      if (Ext = '.DBF') then DatabaseCreate.ConvertDBFtoCDSFile(DragonPlotDef.TowerFileName);
      DragonPlotDef.TowerFileName := ChangeFileExt(DragonPlotDef.TowerFileName,Petmar_types.DefaultDBExt);
   end;

   LoadTowersOnMap(DEMGlb[RegionalDEM].SelectionMap);

   CheckCameraParameters;
   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   DEMGlb[RegionalDEM].SelectionMap.MapDraw.CurrentFansTable := WMDEM.TowerTable;

   while (DEMGlb[RegionalDEM].SelectionMap.MapDraw.OverLayOrder[1] <> ovoDatabases) do
      MoveOverlayUp(DEMGlb[RegionalDEM].SelectionMap,ovoDatabases,true);

   DEMGlb[RegionalDEM].SelectionMap.DoFastMapRedraw;
   Forms.Screen.Cursor := crDefault;
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.LoadDEM out');
   {$EndIf}
end;


procedure DefineTowerDatum(TowerLong : float);
begin
   if (DEMGlb[1] <> Nil) then begin
      DEMGlb[1].DEMEllipsoidConstants.DefineDatumFromUTMZone(MDdef.PreferPrimaryDatum,GetUTMZone(TowerLong),'');
      DEMGlb[1].DEMEllipsoidConstants.DefineDatumFromUTMZone(MDdef.PreferSecondaryDatum,GetUTMZone(TowerLong),'');
   end;
end;


procedure TPhotoRegForm.CheckAccessibilityMap(Lat,Long : float; Primary : boolean);
begin
   {$IfDef CheckAccessibility}
   WriteLineToDebugFile('TPhotoRegForm.CheckAccessibilityMap in',true);
   {$EndIf}
   if DragonPlotDef.FireAccessMap and DEMGlb[1].LatLongDegreeInDEM(Lat,Long) then begin
      AccessMap := DEMGlb[1].SelectionMap.SiteContourMapBlowUp(Lat,Long,DragonPlotDef.FireAccessMapSize,DragonPlotDef.FireAccessMapBlowUp,
          MDDef.DefaultContourInterval,'Fire location accessibility');
      if (DragonPlotDef.DefaultShapeFile <> '') then begin
         AccessMap.OpenDBonMap('',DragonPlotDef.DefaultShapeFile,true,false);
      end;
      AccessMap.DoFastMapRedraw;
      AccessMap.Closable := false;
      DrawSightingsOnMap(AccessMap,Lat,Long,Primary);
   end;
   {$IfDef CheckAccessibility}
   WriteLineToDebugFile('TPhotoRegForm.CheckAccessibilityMap out');
   {$EndIf}
end;


procedure TPhotoRegForm.DisplayMessage(Words : shortString);
begin
   if (Words <> '') then LastSighting.Add(Words);
   Memo1.Lines.Add(Words);
end;


procedure TPhotoRegForm.ShowOnMaps(Lat,Long : float; Primary : boolean);
begin
  {$IfDef RecordCreateBlowUpMap}
   WriteLineToDebugFile('TPhotoRegForm.ShowOnMaps in');
  {$EndIf}
   if (DragonPlotDef.TargetMapType = 1) then begin
     {$IfDef RecordCreateBlowUpMap}
      WriteLineToDebugFile('Try image map');
     {$EndIf}
      if (SatImage[1].LatLongDegreeInDataSet(Lat-MDDef.BlowUpLatSize,Long-MDDef.BlowUpLatSize)) and
         (SatImage[1].LatLongDegreeInDataSet(Lat+MDDef.BlowUpLatSize,Long+MDDef.BlowUpLatSize)) then begin
         {$IfDef RecordCreateBlowUpMap}
         WriteLineToDebugFile('All on image map');
         {$EndIf}
         TargetMap := SatImage[1].SelectionMap;
      end
      else begin
         {$IfDef RecordCreateBlowUpMap}
         WriteLineToDebugFile('Off image map, use DEM map');
         {$EndIf}
         SatImage[1].SelectionMap.WindowState := wsMinimized;
         TargetMap := CreateANewMapWindow(1,false,MDDef.DefDEMMap,'');
      end;
   end;

   DrawSightingsOnMap(DEMGlb[1].SelectionMap,Lat,Long,Primary);
  {$IfDef RecordCreateBlowUpMap}
   WriteLineToDebugFile('TPhotoRegForm.ShowOnMaps DrawSightingsOnMap done');
  {$EndIf}
   CheckAccessibilityMap(Lat,Long,Primary);
  {$IfDef RecordCreateBlowUpMap}
   WriteLineToDebugFile('TPhotoRegForm.ShowOnMaps CheckAccessibilityMap done');
  {$EndIf}
   DrawTargetMap(TargetMap,Lat,Long,Primary);
  {$IfDef RecordCreateBlowUpMap}
   WriteLineToDebugFile('TPhotoRegForm.ShowOnMaps out');
  {$EndIf}
end;

procedure TPhotoRegForm.AddGazetteerToTargetMap;
begin
  {$IfDef RecordGazetteer}
  WriteLineToDebugFile('TPhotoRegForm.AddGazetteerToTargetMap');
  {$EndIf}
   if (DragonPlotDef.DefGazfName <> '') and FileExists(DragonPlotDef.DefGazfName) then begin
      //TargetMap.OpenGazetterOnMap(DragonPlotDef.DefGazfName,false);
      LastGazFile := DragonPlotDef.DefGazfName;
      AddOrSubtractOverlay(TargetMap,ovoGazetteer,true);

   end
   else begin
     {$IfDef RecordGazetteer}
     WriteLineToDebugFile('Problem loading gazetteer ' +  DragonPlotDef.DefGazfName);
     {$EndIf}
   end;
   TargetMap.Closable := false;
end;

procedure TPhotoRegForm.InsureLocationMapOK;
begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.InsureLocationMapOK in');
   {$EndIf}
   if (TargetMap = Nil) then begin
      if (DragonPlotDef.TargetMapType = 1) then begin
         {$IfDef RecordProblems}
         WriteLineToDebugFile('TPhotoRegForm.InsureLocationMapOK (DragonPlotDef.TargetMap = 1, sat image)');
         {$EndIf}
         if (SatImage[1] = Nil) then LoadSatelliteImage;
         if (SatImage[1] = Nil) then DragonPlotDef.TargetMapType := 0
         else TargetMap := SatImage[1].SelectionMap;
      end;

      if (DragonPlotDef.TargetMapType = 0) then begin
         {$IfDef RecordProblems}
         WriteLineToDebugFile('TPhotoRegForm.InsureLocationMapOK (DragonPlotDef.TargetMap = 0, DEM)');
         {$EndIf}
         TargetMap := CreateANewMapWindow(1,false,MDDef.DefDEMMap,'Target map');
         AddGazetteerToTargetMap;
         TargetMap.Closable := false;
      end;
      if (DragonPlotDef.DefaultShapeFile <> '') then begin
         TargetMap.OpenDBonMap('',DragonPlotDef.DefaultShapeFile,true,false);
         TargetMap.DoFastMapRedraw;
      end;
   end;
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.InsureLocationMapOK out');
   {$EndIf}
end;


procedure TPhotoRegForm.BitBtn2Click(Sender: TObject);
var
   AzTrue,AzUTM,MapAz,Dist,Lat,Long,BearingAngle,
   TargetZ,ReportedDistance,z,
   Pitch1,Pitch2  : float;
   Distance : array[1..3] of float;
   i,j   : integer;
   t1 : string16;
   GeoName,TStr,Mess1,Mess2 : ShortString;
   FoundIntersection : boolean;
   Sym1 : tFullSymbolDeclaration;
   Bitmap : Graphics.tBitmap;


         procedure SetUpPerspectiveForm(Number : integer; var PersF : TThreeDview; Title : ShortString;
              TowersLat,TowersLong,TowerGroundElev,CamerasElevAboveGround,theAzimuth,thePitch : float; DoDrape : boolean; SatNum : integer; MapForm : tMapForm = Nil);
         begin
            {$IfDef RecordPerspectiveProblems}
            WriteLineToDebugFile('SetUpPerspectiveForm, ' + LatLongDegreeToString(TowersLat,TowersLong,DecDegrees),true);
            WriteLineToDebugFile('Tower Ground Elev: ' + RealToString(TowerGroundElev,-12,1));
            WriteLineToDebugFile('Azimuth: ' + RealToString(theAzimuth,-12,1));
            WriteLineToDebugFile('ThePitch: ' + RealToString(thePitch,-12,1));
            WriteLineToDebugFile('Title: ' + Title);
            {$EndIf}

            SetUpPanoramaView(PersF,TowersLat,TowersLong,CamerasElevAboveGround,Depth,theAzimuth,HFOV,VFOV,thePitch,ComputeDEM,'View from ' + Title,
                      DoDrape,SeekingSecondPerspective,MapForm,0);
            PersF.Closable := false;
            PersF.Caption := t1 + 'iew from ' + Title;
            PersF.Top := (pred(Number) * (PersF.Height + 5));
            PersF.Left := Screen.Width - 10 - Self.Width - PersF.Width;
            PersF.SymbolAtPitchAzimith(thePitch,theAzimuth,Sym1,true);
        end;


         procedure SetForVisMasked(ShootToPoint : boolean; TowerName : string35; PersUsed : integer; TowersLat,TowersLong,TowerGroundElev,CamerasElevAboveGround,theAzimuth,thePitch : float; Vis : boolean = false);
         var
            BlockDist : float;
         begin
            {$IfDef RecordProblems}
            WriteLineToDebugFile('SetForVisMasked, Tower=' + LatLongDegreeToString(TowersLat,TowersLong,DecDegrees));
            WriteLineToDebugFile('Tower ground elevation: ' +RealToString(TowerGroundElev,-12,1) + '  Camera up: ' +RealToString(CamerasElevAboveGround,-12,1));
            {$EndIf}

            if ShootToPoint then begin
               DEMDatum.VincentyCalculateDistanceBearing(TowersLat,TowersLong,Lat,Long,BlockDist,BearingAngle);
               if (not Vis) then begin
                  Vis := DEMGlb[ComputeDEM].LatLongDegreePointsIntervisible(TowersLat,TowersLong,CamerasElevAboveGround,Lat,Long,0,Dist,BlockDist);
               end;
               {$IfDef RecordProblems}
               WriteLineToDebugFile('Tower Vis=' + BoolToStr(Vis));
               {$EndIf}
            end;

            if Vis then begin
               TStr := '(visible)';
               Sym1 := DragonPlotDef.VisibleSymbol;
               t1 := 'V';
            end
            else begin
               TStr := '(masked)';   // at ' + SmartDistanceMetersFormat(BlockDist,MDDef.EnglishDistanceUnits)  + ')';
               Sym1 := DragonPlotDef.MaskedSymbol;
               t1 := 'Masked v';
            end;

            DisplayMessage('Tower ' + IntToStr(PersUsed) + ': ' + TowerName);
            DisplayMessage('  Input Azimuth (true)=' + RealToString(theAzimuth,-8,2) + '°   ' + ConvertToDegreesString(theAzimuth,DecMinutes));
            if ShootToPoint then begin
               DisplayMessage('  Calculated distance =' +  SmartDistanceMetersFormat(Dist,MDDef.EnglishDistanceUnits));
               DisplayMessage('  Pitch to target: ' + RealToString(Pitch1,-8,2)+ '°   ' + ConvertToDegreesString(Pitch1,DecMinutes) + '   ' + Tstr);
            end;

            DefineTowerDatum(TowerLong);
            {$IfDef RecordProblems}
            WriteLineToDebugFile('Call SetUpPerspectiveForm');
            {$EndIf}
            SetUpPerspectiveForm(PersUsed,DEMPersF[PersUsed],' Tower ' + IntToStr(PersUsed) + ': ' + TowerName,TowersLat,TowersLong,TowerGroundElev,CamerasElevAboveGround,
               theAzimuth,ThePitch,(MDDef.PerspOpts.WhichPerspective = BMPPerspective),1,DEMglb[ComputeDEM].SelectionMap);
            {$IfDef RecordProblems}
            WriteLineToDebugFile('Done SetUpPerspectiveForm');
            {$EndIf}
         end;


      procedure ShowIntersectionOnMap(Map : tMapForm; Intersection : boolean = true);
      begin
         {$IfDef RecordProblems}
         WriteLineToDebugFile('ShowIntersectionOnMap in, map=' + Map.Caption);
         {$EndIf}
         CopyImageToBitmap(Map.Image1,Bitmap);
         Bitmap.Canvas.Font.Color := clRed;
         Map.MapDraw.DrawOrientedLine(Bitmap,TowerLat,TowerLong,Azimuth);
         {$IfDef RecordProblems}
         WriteLineToDebugFile('Tower 1 line drawn');
         {$EndIf}
         if Intersection then begin
            Map.MapDraw.DrawOrientedLine(Bitmap,TowerLat2,TowerLong2,Azimuth2);
            {$IfDef RecordProblems}
            WriteLineToDebugFile('Tower 2 line drawn');
            {$EndIf}
         end;
         Map.Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;
      end;


      procedure AccuracyCheck(Map : tMapForm; Azimuth,Depress : float);
      var
         DistOut : float;
      begin
          DEMPersF[1].LocatePointOnPerspective(Azimuth,Depress,Lat,Long,DistOut,Mess1,Mess2,true);
          Map.MapDraw.MapSymbolAtLatLongDegree(Map.Image1.Canvas,Lat,Long,DragonPlotDef.AccuracySymbol);
      end;


      procedure PostResult(Lat,Long : float; title,Note : shortString);
      begin
         {$IfDef RecordPostResultsProblems}
         WriteLineToDebugFile('Post results 1',true);
         {$EndIf}
         DEMglb[ComputeDEM].SelectionMap.MapDraw.MapSymbolAtLatLongDegree(DEMglb[ComputeDEM].SelectionMap.Image1.Canvas,Lat,Long,DragonPlotDef.ResultSymbol);
         DEMGlb[ComputeDEM].GetElevFromLatLongDegree(Lat,Long,TargetZ);

         DisplayMessage('  ' + title + '  ' + DEMglb[ComputeDEM].SelectionMap.MapDraw.PrimaryMapDatum.h_DatumCode + '   ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + '   ' + note );
         if DragonPlotDef.ShowUTM then DisplayMessage('  ' + title + '  ' + DEMglb[ComputeDEM].SelectionMap.MapDraw.PrimaryMapDatum.UTMStringFromLatLongDegree(Lat,Long));
         DisplayMessage('Elev: ' + RealToString(TargetZ,-8,1) + ' m  (' + RealToString(TargetZ/FeetToMeters,-8,1) + ' ft)');

         if US_properties.GetUSGSQuadName(Lat,Long,GeoName) then Memo1.Lines.Add('USGS 24K quad: ' + GeoName);
         {$IfDef RecordPostResultsProblems}
         WriteLineToDebugFile('Post results 3');
         {$EndIf}

         if (PLSS[1] <> Nil) then begin
            {$IfDef RecordPostResultsProblems}
            WriteLineToDebugFile('Post results 3.5--getting PLSS');
            {$EndIf}
            TStr := PLSSLocation(Lat,Long);
            DisplayMessage('PLSS location: ' + TStr);
         end;

         if (abs(Lat) > 0.000001) and (abs(Long) > 0.00001) then begin
            LastGPSLat := Lat;
            LastGPSLong := Long;
         end;

         {$IfDef RecordPostResultsProblems}
         WriteLineToDebugFile('Post results out');
         {$EndIf}
      end;

      procedure FigureView(DEMPersF : TThreeDview; DEMUsed : integer; Azimuth,Depress : float; Primary : boolean; var Lat,Long : float);
      var
         BlockLat,BlockLong,DistOut,BlockAngle,BlockDist,xgrid,ygrid : float;
      begin
         if (DEMPersF = Nil) then exit;
        {$IfDef RecordFigureViewProblems}
         WriteLineToDebugFile('Off to LocatePointOnPerspective');
        {$EndIf}
        DEMPersF.LocatePointOnPerspective(Azimuth,Depress,Lat,Long,DistOut,Mess1,Mess2);
        DisplayMessage('  Pitch=' + RealToString(Depress,-8,2) + '°   ' + ConvertToDegreesString(Depress,DecMinutes));

        {$IfDef RecordFigureViewProblems}
        WriteLineToDebugFile('Pitch=' + RealToString(Depress,-8,2));
        {$EndIf}

        if  (Mess1 = '') then begin
            {$IfDef RecordFigureViewProblems}
            WriteLineToDebugFile('Off to HorizonBlocking');
            {$EndIf}
            DEMGlb[DEMUsed].HorizonBlocking(TowerLat,TowerLong,AzTrue,Depth,TowerHeight,BlockAngle,BlockDist,BlockLat,BlockLong,MDDef.wf.StraightAlgorithm);
            {$IfDef RecordFigureViewProblems}
            WriteLineToDebugFile('Horizon pitch=' + RealToString(BlockAngle,-8,2));
            {$EndIf}
        end;

         if (Mess1 = '') then begin
            DisplayMessage('');
            DisplayMessage('Shot does not intersect ground in DEM');
            DisplayMessage('Horizon pitch=' + RealToString(BlockAngle,-8,2)+ '°   ' + ConvertToDegreesString(BlockAngle,DecMinutes));
            DEMGlb[DEMUsed].LatLongDegreetoDEMGrid(BlockLat,BlockLong,xgrid,ygrid);
            DisplayMessage('Horizon at ' + DEMGlb[DEMUsed].DEMLocationString(xgrid,ygrid));
            Lat := BlockLat;
            Long := BlockLong;
            DisplayMessage('');
         end
         else begin
            {$IfDef RecordFigureViewProblems}
            WriteLineToDebugFile('Go to CalculateDistanceBearing');
            {$EndIf}
            DEMDatum.VincentyCalculateDistanceBearing(TowerLat,TowerLong,Lat,Long,Distance[DEMUsed],BearingAngle);
            {$IfDef RecordFigureViewProblems}
            WriteLineToDebugFile('Go to PostResult');
            {$EndIf}
            PostResult(Lat,Long,' Target=','');
            {$IfDef RecordFigureViewProblems}
            WriteLineToDebugFile('Go to CheckAccessibilityMap');
            {$EndIf}
            DisplayMessage('Distance from tower: ' + SmartDistanceMetersFormat(Distance[DEMUsed],MDDef.EnglishDistanceUnits));
            {$IfDef RecordFigureViewProblems}
            WriteLineToDebugFile('Go to CreateBlowUpMap');
            {$EndIf}
            DisplayMessage('');
         end;

        {$IfDef RecordFigureViewProblems}
        WriteLineToDebugFile('Figure view out');
        {$EndIf}
      end;

      procedure DoCrossShot;
      begin
         {$IfDef RecordProblems}
         WriteLineToDebugFile('Cross shot underway');
         WriteLineToDebugFile('Tower 1: ' + ComboBox1.Text);
         WriteLineToDebugFile('    TowerGroundElevation=' + RealToString(TowerGroundElevation,-8,1) + '  up=' + RealToString(TowerHeight,-8,1));
         WriteLineToDebugFile('    Azimuth= ' + RealToString(Azimuth,-18,2));
         {$EndIf}

         GISdb[WMDEM.TowerTable].MyData.ApplyFilter('NAME=' + QuotedStr(Trim(ComboBox2.Text)));
         TowerLat2 := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('LAT');
         TowerLong2 := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('LONG');
         TowerHeight2 := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_UP');
         GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
         if not DEMGlb[ComputeDEM].GetElevFromLatLongDegree(TowerLat2,TowerLong2,Tower2GroundElevation) then begin
            Memo1.Lines.Add('Tower 2: ' + ComboBox2.Text + ' not on DEM');
            exit;
         end;

         CameraElev2 := Tower2GroundElevation + TowerHeight2;
         DEMglb[ComputeDEM].SelectionMap.MapDraw.MapSymbolAtLatLongDegree(DEMglb[ComputeDEM].SelectionMap.Image1.Canvas,TowerLat2,TowerLong2,DragonPlotDef.TowerSymbol);

         {$IfDef RecordProblems}
         WriteLineToDebugFile('Tower 2: ' + ComboBox2.Text);
         WriteLineToDebugFile('   Tower2GroundElevation=' + RealToString(Tower2GroundElevation,-8,1) + '  up=' + RealToString(TowerHeight2,-8,1));
         WriteLineToDebugFile('   Azimuth= ' + RealToString(Azimuth2,-18,2));
         {$EndIf}

         FoundIntersection := DEMglb[ComputeDEM].SelectionMap.Intersection(TowerLat,TowerLong,Azimuth,TowerLat2,TowerLong2,Azimuth2,Lat,Long);

         LastGPSLat := Lat;
         LastGPSLong := Long;

         if FoundIntersection then ShowOnMaps(Lat,Long,false);

         if (not FoundIntersection) or (TargetMap = Nil) then begin
            DisplayMessage('Tower 1: ' + ComboBox1.Text {+ '    ' + LatLongDegreeToString(TowerLat,TowerLong,MDDef.OutPutLatLongMethod)+ '   ' + 'Datum: ' + DEMglb[ComputeDEM].SelectionMap.MapDraw.CurrentMapDatum.MapDefine.h_DatumCode});
            DisplayMessage('  Input Azimuth (true)=' + ConvertToDegreesString(Azimuth,DecDegrees,false) + '   ' + ConvertToDegreesString(Azimuth,DecMinutes,false));

            DisplayMessage('Tower 2: ' + ComboBox2.Text {+ '    ' + LatLongDegreeToString(TowerLat2,TowerLong2,MDDef.OutPutLatLongMethod) '   ' + 'Datum: ' + DEMglb[ComputeDEM].SelectionMap.MapDraw.CurrentMapDatum.MapDefine.h_DatumCode});
            DisplayMessage('  Input Azimuth (true)=' + ConvertToDegreesString(Azimuth2,DecDegrees,false) + '   ' + ConvertToDegreesString(Azimuth2,DecMinutes,false));
            DisplayMessage('');
            LastGPSLat := 999;
            LastGPSLong := 999;
            if FoundIntersection then begin
               DisplayMessage('Intersection beyond DEM data');
            end
            else DisplayMessage('No intersection within ' + SmartDistanceMetersFormat(250000,MDDef.EnglishDistanceUnits));
            GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
            TargetMap.Closable := true;
            TargetMap.Close;
            TargetMap := Nil;
            exit;
         end
         else begin
            TargetMap.MapDraw.BaseTitle := 'Intersection from ' + ComboBox1.Text + ' and ' + ComboBox2.Text;
            TargetMap.Caption := TargetMap.MapDraw.BaseTitle;
         end;

         DEMglb[ComputeDEM].GetElevFromLatLongDegree(Lat,Long,TargetZ);

         {$IfDef RecordProblems}
         WriteLineToDebugFile('Target  z=' + RealToString(TargetZ,-8,1));
         {$EndIf}

         with DEMglb[ComputeDEM].SelectionMap do begin
            Mess1 := '';
             with DEMglb[ComputeDEM].SelectionMap do begin
                if not MapDraw.MapSymbolAtLatLongDegree(Image1.Canvas,Lat,Long,Box,3,claRed) then Mess1 := '  (off map)';
            end;

            VincentyCalculateDistanceBearing(TowerLat,TowerLong,Lat,Long,ReportedDistance,Pitch1);
            Pitch1 := ArcTan((Targetz - CameraElev - DropEarthCurve(ReportedDistance)) /   ReportedDistance) / DegToRad;
            SetForVisMasked(true,ComboBox1.Text,1,TowerLat,TowerLong,TowerGroundElevation,TowerHeight,Azimuth,Pitch1);

            VincentyCalculateDistanceBearing(TowerLat2,TowerLong2,Lat,Long,ReportedDistance,Pitch1);
            Pitch2 := ArcTan((Targetz - CameraElev2 - DropEarthCurve(ReportedDistance)) /   ReportedDistance) / DegToRad;
            SetForVisMasked(true,ComboBox2.Text,2,TowerLat2,TowerLong2,Tower2GroundElevation,TowerHeight2,Azimuth2,Pitch2);
            Memo1.Lines.Add('');
            PostResult(Lat,Long,'Intersection: ',Mess1);
            Memo1.Lines.Add('');
         end;

         if DragonPlotDef.ShowLOS then begin
             MDDef.LOSShowPitch := true;
             MDDef.LOSMinPitch := Pitch1;
             MDDef.LOSMaxPitch := Pitch1;

            CreateProfile(DEMLOSF,TowerLat,TowerLong,Lat,Long,TowerHeight,ComboBox1.Text);
             MDDef.LOSShowPitch := true;
             MDDef.LOSMinPitch := Pitch2;
             MDDef.LOSMaxPitch := Pitch2;
            CreateProfile(DEMLOSF2,TowerLat2,TowerLong2,Lat,Long,TowerHeight2,ComboBox2.Text);
         end;
         GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
      end;

      procedure DoSingleShot;
      var
         i,j : integer;
      begin
         Edit11.Text := RealToString(Depth,-18,0);
         {$IfDef RecordProblems}
         WriteLineToDebugFile('Single shot');
         WriteLineToDebugFile(ComboBox1.Text + '    ' + LatLongDegreeToString(TowerLat,TowerLong,MDDef.OutPutLatLongMethod) + '  Camera z: ' + RealToString(CameraElev,8,2));
         WriteLineToDebugFile('  Input Azimuth (true)=' + RealToString(Azimuth,-8,2) + '°   ' + ConvertToDegreesString(Azimuth,DecMinutes));
         WriteLineToDebugFile('  Pitch=' + RealToString(Depress,-8,2)+ '°   ' + ConvertToDegreesString(Depress,DecMinutes));
         {$EndIf}

         SetForVisMasked(false,ComboBox1.Text,1,TowerLat,TowerLong,TowerGroundElevation,TowerHeight,Azimuth,Depress,true);

         {$IfDef RecordProblems}
         WriteLineToDebugFile('Done SetForVisMasked');
         {$EndIf}
         FigureView(DEMPersF[1],ComputeDEM,Azimuth,Depress,true,Lat,Long);
         {$IfDef RecordProblems}
         WriteLineToDebugFile('Done FigureView');
         {$EndIf}
         ShowOnMaps(Lat,Long,true);
         {$IfDef RecordProblems}
         WriteLineToDebugFile('Done ShowOnMaps');
         {$EndIf}
         if DragonPlotDef.ShowLOS then begin
            MDDef.LOSShowPitch := true;
            MDDef.LOSMinPitch := Depress;
            MDDef.LOSMaxPitch := Depress;
            CreateProfile(DEMLOSF,TowerLat,TowerLong,Lat,Long,TowerHeight,ComboBox1.Text);
         end;

         if DragonPlotDef.PlotAccuracy and (TargetMap <> Nil) then begin
            {$IfDef RecordProblems}
            WriteLineToDebugFile('DragonPlotDef.PlotAccuracy');
            {$EndIf}
            for i := -1 to 1 do
               for j := -1 to 1 do
                  if (i <> 0) or (j <> 0) then
                     AccuracyCheck(TargetMap,Azimuth+i*DragonPlotDef.AzimuthAccuracy,Depress+j*DragonPlotDef.PitchAccuracy);
         end;
      end;


begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('*******TPhotoRegForm.BitBtn2Click----Draw view selected');
   {$EndIf}
   CloseWindows;
   LastSighting.Clear;

   if (Sender = CrossShotBitBtn8) then begin
      ComboBox1.Text := ComboBox3.Text;
      Edit3.Text := Edit16.Text;
      Edit1.Text := Edit17.Text;
      TStr := 'Cross shot ';
   end
   else begin
      ComboBox2.Text := '';
      TStr := 'Single sighting';
   end;
   DisplayMessage(TStr + ' done at ' + DateToStr(Date) + '   ' + CurrentMilitaryTime(true)  );

   if (not CheckCameraParameters(True,(Sender = CrossShotBitBtn8))) then begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Input entry error, cannot proceed.');
      Memo1.Lines.Add('Fix error and retry.');
      exit;
   end;


   InsureLocationMapOK;

   {$IfDef RecordProblems}
   WriteLineToDebugFile('Tower ' + GISdb[WMDEM.TowerTable].MyData.Filter);
   WriteLineToDebugFile('Picked first tower of ' + IntToStr(GISdb[WMDEM.TowerTable].MyData.RecordCount));
   {$EndIf}

   Depth := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
   AzTrue := Azimuth;
   AzUTM := Azimuth-DEMglb[ComputeDEM].SelectionMap.MapDraw.GridTrueAngle;
   MapAz := AzTrue;

   DEMglb[RegionalDEM].SelectionMap.MapDraw.GridTrueAngle := DEMglb[RegionalDEM].SelectionMap.MapDraw.UTMGridToTrueNorthAngle(TowerLat,TowerLong);

   gbLatStart := TowerLat;
   gbLongStart := TowerLong;

   If (Sender = BitBtn2) and (Edit21.Text <> '') then begin
      CheckEditString(Edit21.Text,ReportedDistance);
      DisplayMessage('Tower 1: ' + ComboBox1.Text);
      DisplayMessage('  Input Azimuth (true)=' + ConvertToDegreesString(Azimuth,DecDegrees,false) + '  ' + ConvertToDegreesString(Azimuth,DecMinutes,false));
      DisplayMessage('  Input distance: ' + Edit21.Text + ' miles');

      ReportedDistance := ReportedDistance * 1.61 * 1000;
      DEMDatum.VincentyPointAtDistanceBearing(TowerLat,TowerLong,ReportedDistance,Azimuth,Lat,Long);
      LastGPSLat := Lat;
      LastGPSLong := Long;
      DEMGlb[ComputeDEM].GetElevFromLatLongDegree(Lat,Long,Z);
      Pitch1 := ArcTan((z - CameraElev - DropEarthCurve(ReportedDistance)) /   ReportedDistance) / DegToRad;
      SetForVisMasked(false,ComboBox1.Text,1,TowerLat,z,TowerHeight,CameraElev,Azimuth,Pitch1);
      FigureView(DEMPersF[1],2,Azimuth,Pitch1,false,Lat,Long);
      ShowOnMaps(Lat,Long,true);
      PostResult(Lat,Long,'   Computed location: ','');
   end;

   if (Sender = CrossShotBitBtn8) then begin   //Cross option
      DoCrossShot;
   end;

   if (Sender = BitBtn2) then begin
      DoSingleShot;
   end;
   Pitch2 := -99;

   {$IfDef RecordProblems}
   WriteLineToDebugFile('Off to ArchiveShot');
   {$EndIf}
   ArchiveShot;
   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   Forms.Screen.Cursor := crDefault;
  {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn2Click out');
  {$EndIf}
end;


procedure TPhotoRegForm.ClosePerspectives;
var
   I : integer;

   procedure CloseSinglePerspective(var DEMPersF : TThreeDview);
   begin
      if (DEMPersF <> Nil) then begin
         DEMPersF.Closable := true;
         DEMPersF.Close;
         DEMPersF := Nil;
      end;
   end;

begin
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('TPhotoRegForm.ClosePerspectives in');
   {$EndIf}
   for i := 1 to 3 do begin
      {$IfDef RecordClosingProblems}
      WriteLineToDebugFile('TPhotoRegForm.ClosePerspectives ' + IntToStr(i));
      {$EndIf}
      CloseSinglePerspective(DEMPersF[i]);
   end;
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('TPhotoRegForm.ClosePerspectives mid');
   {$EndIf}
   CloseSinglePerspective(DEMPersF2);
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('TPhotoRegForm.ClosePerspectives out');
   {$EndIf}
end;


procedure TPhotoRegForm.CloseWindows;
var
   i : integer;
begin
    {$IfDef RecordClosingProblems}
    WriteLineToDebugFile('TPhotoRegForm.CloseWindows in');
    {$EndIf}
   if not WMDEM.ProgramClosing then begin
      Memo1.Clear;
      Edit27.Text := '';
      BitBtn1.Enabled := false;
      BitBtn12.Enabled := (PLSS[1] <> Nil);
   end;

   {$IfDef ExGPS}
   {$Else}
   if DragonPlotDef.PauseGPSDuringShot and (GPSCommoForm <> Nil) then begin
      GPSCommoForm.GPSWorking := false;
      GPSCommoForm.Close;
      GPSCommoForm := Nil;
      wmDEM.Start1.Enabled := true;
   end;
  {$EndIf}

   ClosePerspectives;

   {$IfDef ExSats}
   {$Else}
   for i := 1 to MaxSatAllowed do if (ImagePersF[i] <> Nil) then begin
      ImagePersF[i].Closable := true;
      ImagePersF[i].Close;
      ImagePersF[i] := Nil;
   end;
   {$EndIf}

   if (DEMPersPan <> Nil) then begin
      DEMPersPan.Closable := true;
      DEMPersPan.Close;
      DEMPersPan := Nil;
   end;

   if (DEMLOSF <> Nil) then begin
      DEMLOSF.Closable := true;
      DEMLOSF.Close;
      DEMLOSF := Nil;
   end;

   if (DEMLOSF2 <> Nil) then begin
      DEMLOSF2.Closable := true;
      DEMLOSF2.Close;
      DEMLOSF2 := Nil;
   end;

   if (TargetMap <> Nil) then begin
      TargetMap.ShowKeyLocation := false;
      TargetMap.Closable := true;
      TargetMap.Close;
      TargetMap := Nil;
   end;

   if (AccessMap <> Nil) then begin
      AccessMap.Closable := true;
      AccessMap.Close;
      AccessMap := Nil;
   end;


   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('TPhotoRegForm.CloseWindows out');
   {$EndIf}
end;


procedure TPhotoRegForm.ArchiveShot;
var
   TStr : ShortString;
   i : integer;
begin
    {$IfDef RecordProblems}
    WriteLineToDebugFile('ArchiveShot in');
    {$EndIf}
    TStr := DateToStr(now) + '--' + CurrentMilitaryTime(true);
    for i := 1 to length(TStr) do if TStr[i] in ['/',':'] then TStr[i] := '-';
    TStr := ArchiveDirectory + TStr;
    Memo1.Lines.SaveToFile(TStr + '.txt');
    //if (not DragonPlotDef.ShowPerspective) then ClosePerspectives;
    GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
    BitBtn11.Enabled := (TargetMap <> Nil) or (SatImage[1] <> Nil);
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Notes:');
    {$IfDef RecordProblems}
    WriteLineToDebugFile('ArchiveShot out');
    {$EndIf}
end;


    procedure TPhotoRegForm.GetTowerVariables(TheName : ShortString);
    begin
       GISdb[WMDEM.TowerTable].MyData.ApplyFilter('NAME=' + QuotedStr(Trim(TheName)));
       TowerLat := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('LAT');
       TowerLong := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('LONG');
       TowerRange := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
       TowerHeight := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_UP');
       GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
    end;


function TPhotoRegForm.CheckCameraParameters(LoadBlowup : boolean = true; CrossShot : boolean = false) : boolean;
var
   InputError : boolean;

       function GetAngleDegrees(EditDeg,EditMin : tEdit; NegativeAllowed : boolean = false) : float;
       var
          Minutes : float;
       begin
          Result := 0;
          Minutes := 0;
          if not PETMAR.CheckEditString(EditDeg.Text,Result) and (EditDeg.Text <> '') then begin
             Memo1.Lines.Add('Error in degrees entry (' + EditDeg.Text + ')');
             InputError := true;
          end;
          if (not NegativeAllowed) and ((Result > 360) or (Result < 0)) then begin
             Memo1.Lines.Add('Degrees  (' + EditDeg.Text + ') out of range, 0-360.');
             InputError := true;
          end;
          if not PETMAR.CheckEditString(EditMin.Text,Minutes) and (EditMin.Text <> '') then begin
             Memo1.Lines.Add('Error in minutes entry (' + EditMin.Text + ')');
             InputError := true;
          end;
          if (Minutes < 0) and (Result > 0.0001) then begin
             Memo1.Lines.Add('Negative Minutes  (' + EditMin.Text + ') not allowed');
             InputError := true;
          end;
          if (Minutes >= 60) then begin
             Memo1.Lines.Add('Minutes  (' + EditMin.Text + ') out of range, 0-60.');
             InputError := true;
          end;
          Minutes := Minutes / 60;
          if Result >= 0 then Result := Result + Minutes
          else Result := Result - Minutes;
          if (not NegativeAllowed) and (Result > 360.000000001) then begin
             Memo1.Lines.Add('Degrees  > 360.');
             InputError := true;
          end;
       end;

       procedure LoadComputeDEMForTower(fName : PathStr);
       var
          uMinLat,uMinLong,uMaxLat,uMaxLong : float;
       begin
          {$IfDef RecordHiResDEM}
          WriteLineToDebugFile('');
          WriteLineToDebugFile('LoadComputeDEMForTower in ' + fName);
          {$EndIf}
          if FileExists(fName) then begin
             if (ComputeDEM <> 0) and (UpperCase(DEMGlb[ComputeDEM].DEMFileName) = UpperCase(fName)) then begin
                {$IfDef RecordHiResDEM}
                WriteLineToDebugFile('already loaded');
                {$EndIf}
                exit;
             end
             else begin
                {$IfDef RecordHiResDEM}
                WriteLineToDebugFile('load existing');
                {$EndIf}
                if ComputeDEM <> 0 then CloseSingleDEM(ComputeDEM);
                Read_DEM.LoadNewDEM(ComputeDEM,fName);
             end;
          end
          else begin
               {$IfDef RecordHiResDEM}
               WriteLineToDebugFile('create blowup');
               {$EndIf}
               DEMGlb[RegionalDEM].SelectionMap.MapDraw.BoxToContainFan(TowerLat,TowerLong,TowerRange,uMinLat,uMinLong,uMaxLat,uMaxLong);
               DEMGlb[RegionalDEM].LoadFanBlowUpDEM(uMinLat,uMinLong,uMaxLat,uMaxLong);
               if (DEMGlb[RegionalDEM].FanBlowUpDEM <> 0) then begin
                  ComputeDEM := DEMGlb[RegionalDEM].FanBlowUpDEM;
                  {$IfDef RecordHiResDEM}
                  WriteLineToDebugFile('Save new dem=' + fName);
                  {$EndIf}
                  DEMGlb[ComputeDEM].WriteNewFormatDEM(fName);
               end
               else begin
                  ComputeDEM := RegionalDEM;
                  {$IfDef RecordHiResDEM}
                  WriteLineToDebugFile('Creation failed for ' + fName);
                  {$EndIf}
               end;
          end;
          {$IfDef RecordHiResDEM}
          WriteLineToDebugFile('LoadComputeDEMForTower out');
          WriteLineToDebugFile('');
          {$EndIf}
       end;


begin
   {$IfDef RecordHiResDEM}
   WriteLineToDebugFile('TPhotoRegForm.CheckCameraParameters in');
   {$EndIf}
   InputError := false;
   PETMAR.CheckEditString(Edit4.Text,VFOV);
   PETMAR.CheckEditString(Edit5.Text,HFOV);
   MDDef.PersVFOV := VFOV;
   MDDef.PersHFOV := HFOV;

   PETMAR.CheckEditString(Edit11.Text,Depth);
   PETMAR.CheckEditString(Edit12.Text,MDDef.BlowUpLatSize);
   PETMAR.CheckEditString(Edit13.Text,MDDef.BlowUpLongSize);
   PETMAR.CheckEditString(Edit23.Text,DragonPlotDef.PanoramaPitch);

   Azimuth := GetAngleDegrees(Edit3,Edit1);
   Depress := GetAngleDegrees(Edit9,Edit2,true);
   if (Depress > DragonPlotDef.MaxPitch) or (Depress < DragonPlotDef.MinPitch) then begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Pitch angle (' + RealToString(Depress,-9,-2) + ') may be excessive; verify');
      Memo1.Lines.Add('');
   end;

   Azimuth2 := GetAngleDegrees(Edit14,Edit15);

   CheckEditString(Edit6.Text,DragonPlotDef.AzimuthAccuracy);
   CheckEditString(Edit8.Text,DragonPlotDef.PitchAccuracy);

   if (ComboBox1.Text <> '') then begin
      {$IfDef RecordHiResDEM}
      WriteLineToDebugFile('Tower=' + ComboBox1.Text);
      {$EndIf}
      GetTowerVariables(ComboBox1.Text);

      if LoadBlowUp then begin
          {$IfDef ExVegDensity}
           ComputeDEM := RegionalDEM;
          {$Else}
          if (BlowUpDEMSeries = '') then begin
             {$IfDef RecordHiResDEM}
             WriteLineToDebugFile('No blowup series defined');
             {$EndIf}
             ComputeDEM := RegionalDEM;
          end
          else begin
             {$IfDef RecordHiResDEM}
             WriteLineToDebugFile('Check hi res DEM, series =' + BlowUpDEMSeries);
             {$EndIf}
             DEMfName := WriteDEMDir + ComboBox1.Text + '.dem';
             LoadComputeDEMForTower(DEMfName);

             if CrossShot then begin
                {$IfDef RecordHiResDEM}
                WriteLineToDebugFile('In a Crossshot');
                {$EndIf}
                DEMfName2 := WriteDEMDir + Trim(ComboBox2.Text) + '.dem';
                GetTowerVariables(ComboBox2.Text);
                TowerLat2 := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('LAT');
                TowerLong2 := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('LONG');

                if not FileExists(DEMfName2) then begin
                  {$IfDef RecordHiResDEM}
                  WriteLineToDebugFile('Crossshot, no second DEM');
                  {$EndIf}
                   LoadComputeDEMForTower(DEMfName2);
                   CloseSingleDEM(ComputeDEM);
                end;

                GetTowerVariables(ComboBox1.Text);
                {$IfDef RecordHiResDEM}
                WriteLineToDebugFile('Loading first DEM, ' + DEMfName);
                {$EndIf}

                if (DEMGlb[ComputeDEM].AreaName = GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsString('NAME')) then begin
                   {$IfDef RecordHiResDEM}
                   WriteLineToDebugFile('Current compute DEM correct');
                   {$EndIf}
                   if not DEMGlb[ComputeDEM].LatLongDegreeInDEM(TowerLat2,TowerLong2) then begin
                      {$IfDef RecordHiResDEM}
                      WriteLineToDebugFile('Cross shot merge start);
                      {$EndIf}
                      CloseSingleDEM(ComputeDEM);
                      MergeList := TStringList.Create;
                      MergeList.Add(DEMfName);
                      MergeList.Add(DEMfName2);
                      DEM_Indexes.MergeMultipleDEMsHere(ComputeDEM,MergeList,true);
                      DEMGlb[ComputeDEM].WriteNewFormatDEM(DEMfName);
                      DEMGlb[ComputeDEM].AreaName := ComboBox1.Text;
                      DEMGlb[ComputeDEM].SelectionMap.Caption := DEMGlb[ComputeDEM].AreaName;
                      {$IfDef RecordHiResDEM}
                      WriteLineToDebugFile('Merge over, save=' + DEMfName);
                      {$EndIf}
                   end;
                end;
             end;
          end;
          {$EndIf}
      end
      else begin
         if (ComputeDEM = 0) then ComputeDEM := RegionalDEM;
      end;

      if (DEMglb[ComputeDEM] <> Nil) then begin
         if not DEMglb[ComputeDEM].GetElevFromLatLongDegree(TowerLat,TowerLong,TowerGroundElevation) then begin
            Memo1.Lines.Add('Tower 1: ' + ComboBox1.Text + ' not on DEM');
            exit;
         end;
         CameraElev := TowerGroundElevation + TowerHeight;
      end;
      DefineTowerDatum(TowerLong);
   end;
   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   Result := not InputError;

   {$IfDef RecordCheckCameraParamters}
   WriteLineToDebugFile(' Location:  ' + LatLongDegreeToString(TowerLat,TowerLong,decMinutes));
   WriteLineToDebugFile(' TowerGroundElevation=' + RealToString(TowerGroundElevation,-8,1));
   WriteLineToDebugFile(' tower height=' + RealToString(TowerHeight,-8,1));
   WriteLineToDebugFile(' Camera Elev=' + RealToString(CameraElev,-8,1));
   WriteLineToDebugFile(' Depth=' + RealToString(Depth,-8,0));
   WriteLineToDebugFile(' Azimuth=' + RealToString(Azimuth,-8,2));
   WriteLineToDebugFile(' Depress=' + RealToString(DragonPlotDef.PanoramaPitch,-8,2));
   {$EndIf}

   {$IfDef RecordHiResDEM}
   WriteLineToDebugFile('TPhotoRegForm.CheckCameraParameters out');
   {$EndIf}
end;


procedure TPhotoRegForm.DrawTargetMap(var UseMap : tMapForm; Lat,Long : float; Primary : boolean);
begin
  {$IfDef RecordCreateBlowUpMap}
   WriteLineToDebugFile('TPhotoRegForm.DrawTargetMap, ' + UseMap.Caption);
  {$EndIf}
   UseMap.MapDraw.MaximizeLatLongMapCoverage(Lat-MDDef.BlowUpLatSize,Long-MDDef.BlowUpLongSize,Lat+MDDef.BlowUpLatSize,Long+MDDef.BlowupLongSize,DragonPlotDef.TargetMapWidth,DragonPlotDef.TargetMapWidth);
   UseMap.ShowKeyLocation := true;
   UseMap.KeyLocationLat := Lat;
   UseMap.KeyLocationLong := Long;
   UseMap.DoCompleteMapRedraw;
   UseMap.Closable := false;
   UseMap.MapDraw.BaseTitle := 'Estimated fire location: ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod);
   UseMap.Caption := UseMap.MapDraw.BaseTitle;
   LastSighting.Clear;
   LastSighting.Add('LAT,LONG,NAME,TEXT,IMAGE');
   LastSighting.Add(RealToString(UseMap.KeyLocationLat,-12,8) + ',' + RealToString(UseMap.KeyLocationLong,-12,8) + ',' + 'Dragon Plot Fix' + ',' + 'sighting.txt,View_of_shot.png');
   LastSighting.SaveToFile(MDTempDir + 'sighting.csv');

   DrawSightingsOnMap(UseMap,Lat,Long,Primary);

   UseMap.NoScrollBars;
   UseMap.Left := 0;
   UseMap.Top := TopWindow2;
   UseMap.BringToFront;
   UseMap.WindowState := wsNormal;
  {$IfDef RecordCreateBlowUpMap}
   WriteLineToDebugFile('TPhotoRegForm.DrawTargetMap out, ' + TargetMap.MapDraw.MapSizeString);
  {$EndIf}
end;


procedure TPhotoRegForm.ShowAdvancedOptions;
begin
   CheckBox10.Visible := DragonPlotDef.AdvancedOptions;
   CheckBox10.Checked := DragonPlotDef.ShowUTM;
end;


procedure TPhotoRegForm.FormCreate(Sender: TObject);
var
   i  : integer;
begin
   {$IfDef ExSats}
   {$Else}
   for i := 1 to MaxSatAllowed do ImagePersF[i] := Nil;
   {$EndIf}

   for i := 1 to 3 do DEMPersF[i] := Nil;
   DEMPersF2 := Nil;

   TargetMap := Nil;
   DEMLOSF := Nil;
   DEMLOSF2 := Nil;
   Azimuth := 0;

   PageControl1.Visible := true;
   PageControl1.ActivePage := TabSheet1;
   Pitch2 := -99;
   LastSighting := tStringList.Create;

   CheckBox1.Checked := DragonPlotDef.ShowLOS;
   CheckBox2.Checked := DragonPlotDef.PlotAccuracy;
   Edit11.Text := IntToStr(DragonPlotDef.ViewDepth);

   ShowAdvancedOptions;

   RadioGroup1.ItemIndex := DragonPlotDef.ViewLevelIndex;
   RadioGroup1Click(nil);

   RadioGroup2.ItemIndex := DragonPlotDef.TargetMapType;

   RadioGroup3.ItemIndex := DragonPlotDef.MapLevelIndex;
   RadioGroup3Click(nil);
   CheckBox3.Checked := DragonPlotDef.TerrainShadows;

   CheckBox99.Checked := DragonPlotDef.FireAccessMap;

   Edit19.Text := IntToStr(DragonPlotDef.MinPitch);
   Edit20.Text := IntToStr(DragonPlotDef.MaxPitch);
   Edit23.Text := RealToString(DragonPlotDef.PanoramaPitch,-8,-2);

   Edit6.Text := RealToString(DragonPlotDef.AzimuthAccuracy,-8,-2);
   Edit8.Text := RealToString(DragonPlotDef.PitchAccuracy,-8,-2);

   Edit24.Text := RealToString(DragonPlotDef.BlowUpSizeAspect,-12,-4);
   Edit25.Text := IntToStr(DragonPlotDef.FireAccessMapSize);
   Edit26.Text := IntToStr(DragonPlotDef.FireAccessMapBlowup);

   BitBtn4.Visible := TrilobiteComputer;
   Edit21.Visible := TrilobiteComputer;
   Label30.Visible := TrilobiteComputer;
   TabSheet6.TabVisible := TrilobiteComputer;

   SymbolOnButton(BitBtn15,DragonPlotDef.NegLightningSymbol);
   SymbolOnButton(BitBtn21,DragonPlotDef.PosLightningSymbol);
   SymbolOnButton(BitBtn26,DragonPlotDef.MaskedSymbol);
   SymbolOnButton(BitBtn25,DragonPlotDef.VisibleSymbol);
   SymbolOnButton(BitBtn27,DragonPlotDef.AccuracySymbol);
   SymbolOnButton(BitBtn28,DragonPlotDef.TowerSymbol);
   SymbolOnButton(BitBtn29,DragonPlotDef.ResultSymbol);
   SymbolOnButton(BitBtn32,MDDef.VisPtSymbol);
   SymbolOnButton(BitBtn33,MDDef.MaskPtSymbol);

   ColorLineWidthBitBtn(BitBtn35,DragonPlotDef.ShotLineColor,DragonPlotDef.ShotLineWidth);

   {$IfDef DragonPlotDemo}
   BitBtn12.Enabled := false;
   BitBtn17.Enabled := false;
   CheckBox2.Enabled := false;
   RadioGroup1.Enabled := false;
   RadioGroup3.Enabled := false;
   {$EndIf}

   Caption := 'Fire tower information:    ' + ExtractFileNameNoExt(Application.ExeName);
   Petmar.CheckFormPlacement(Self);
end;



procedure TPhotoRegForm.PanoramaSpeedButtonClick(Sender: TObject);
var
   Depth : float;
   NewDEM : integer;
   Bitmap : Graphics.tBitmap;
   wFan : tWeaponsFan;
   fName : PathStr;
begin
   if (ComboBox5.Text = '') then begin
      MessageToContinue('Select tower first');
      exit;
   end;
   CloseWindows;
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.PanoramaSpeedButtonClick (Pan button) in');
   WriteLineToDebugFile('Tower: ' + ComboBox5.Text);
   WriteLineToDebugFile('Towers=' + IntToStr(GISdb[WMDEM.TowerTable].MyData.RecordCount));
   {$EndIf}
   ComboBox1.Text := ComboBox5.Text;

   CheckCameraParameters;
   GetTowerVariables(ComboBox5.Text);

   if not DEMglb[ComputeDEM].LatLongDegreeInDEM(TowerLat,TowerLong) then begin
      MessageToContinue('Tower ' + ComboBox1.Text + ' not located within DEM');
      exit;
   end;

   with DEMglb[ComputeDEM].SelectionMap.MapDraw do begin
      Depth := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
      Edit11.Text := RealToString(Depth,-18,0);

     {$IfDef RecordProblems}
      WriteLineToDebugFile('Tower location: ' + LatLongDegreeToString(TowerLat,TowerLong,DecDegrees));
     {$EndIf}

      if (Sender = BitBtn20) then begin  //how masked
         MDDef.DoReqAntHigh := true;
         MDDef.DoReqFlyHigh := false;
         MDDef.DoGrazingAngle := false;
         NewDEM := MakeRequiredAntennaMap('Antennas',1,TowerLat,TowerLong,TowerHeight,Depth,true,0,360);
         DEMGlb[NewDEM].SelectionMap.MapDraw.MapType := mtElevFromTable;
         DEMGlb[NewDEM].SelectionMap.MapDraw.TerrainShadowsDEM := 1;
         DEMGlb[NewDEM].SelectionMap.MapDraw.DrawTerrainShadows := true;
         DEMGlb[NewDEM].SelectionMap.MapDraw.BaseTitle := ComboBox5.Text + ' Visibility categories';
         DEMGlb[NewDEM].SelectionMap.DoCompleteMapRedraw;
      end;

      if (Sender = BitBtn16) then begin //tower viewshed
         {$IfDef RecordViewshedProblems}
         WriteLineToDebugFile('(Sender = BitBtn16) tower viewshed',true);
         {$EndIf}
         GISdb[WMDEM.TowerTable].MyData.ApplyFilter('NAME=' + QuotedStr(Trim(ComboBox5.Text)));
         fName := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsString('IMAGE');
         {$IfDef RecordViewshedProblems}
         WriteLineToDebugFile('fName=' + fName);
         {$EndIf}
         if (fName = '') or not FileExists(fName) then begin
           wFan := WeaponsTableToFan(DEMglb[ComputeDEM].SelectionMap.MapDraw.PrimaryMapDatum,GISdb[WMDEM.TowerTable].MyData);
           wFan.FanFileName := '';
           {$IfDef RecordViewshedProblems}
           WriteLineToDebugFile('call add to fan');
           {$EndIf}
           DEMglb[ComputeDEM].SelectionMap.MapDraw.AddFanToMap(WFan);
            {$IfDef RecordViewshedProblems}
            WriteLineToDebugFile('Back from add to fan, fName=' + wFan.FanFileName);
            {$EndIf}
            GISdb[WMDEM.TowerTable].MyData.Edit;
            GISdb[WMDEM.TowerTable].MyData.SetFieldByNameAsString('IMAGE',wFan.FanFileName);
            GISdb[WMDEM.TowerTable].MyData.Post;
            {$IfDef RecordProblems}
            WriteLineToDebugFile('Viewshed created ' + wFan.FanFileName);
            {$EndIf}
         end;
         CopyImageToBitmap(DEMglb[ComputeDEM].SelectionMap.Image1,Bitmap);
         DEMglb[ComputeDEM].SelectionMap.MapDraw.DrawWorldFileImageOnMap(Bitmap,FName);
         DEMglb[ComputeDEM].SelectionMap.Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;
         GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
      end;

      if (Sender = PanoramaSpeedButton) then begin
         MDdef.PerspOpts.WhichPerspective := ReflectancePerspective;
         DEMPersPan := nil;
         SetUpPanoramaView(DEMPersPan,TowerLat,TowerLong,TowerHeight,Depth,0,MDDef.PanHFOV,40,DragonPlotDef.PanoramaPitch,1);
         DEMPersPan.Caption := ComboBox1.Text + ' panorama view';
         DEMPersPan.Top := Screen.Height - 100 - DEMPersPan.Height;
         DEMPersPan.Left := Screen.Width - 5 - DEMPersPan.Width;
         DEMPersPan.Closable := false;
         RadioGroup1Click(Nil);  //to reset panorama size
      end;
   end;

   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   DEMglb[ComputeDEM].SelectionMap.Forceredrawlegendsscalebars1Click(Nil);
   {$IfDef RecordProblems}
   WriteLineToDebugFile('Pan button out, Towers=' + IntToStr(GISdb[WMDEM.TowerTable].MyData.RecordCount));
   {$EndIf}
end;


procedure TPhotoRegForm.ComboBox2Change(Sender: TObject);
begin
   GroupBox2.Enabled := true;
   Edit14.Enabled := true;
   Edit15.Enabled := true;
end;

procedure TPhotoRegForm.WMBroadcastLatLongMessage(var Msg : TMessage);
begin
end;


procedure TPhotoRegForm.BitBtn6Click(Sender: TObject);
var
   WantedDEM : integer;
begin
   WantedDEM := 0;
   Read_DEM.LoadNewDEM(WantedDEM,LastDEMName);
   BitBtn6.Enabled := (NumDEMDataSetsOpen < 3);
end;


procedure TPhotoRegForm.BitBtn7Click(Sender: TObject);
var
   Depth : float;
   Lat2,Long2 : float;
begin
   CloseWindows;
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn7Click--LOS button selected');
   WriteLineToDebugFile('Tower: ' + ComboBox5.Text);
   WriteLineToDebugFile('Azimuth: ' + RealToString(Azimuth,-12,0));
   {$EndIf}
   ComboBox1.Text := ComboBox5.Text;
   CheckCameraParameters;
   if not DEMglb[RegionalDEM].LatLongDegreeInDEM(TowerLat,TowerLong) then begin
      MessageToContinue('Tower ' + ComboBox1.Text + ' not located within DEM');
      exit;
   end;

   with DEMglb[RegionalDEM].SelectionMap.MapDraw do begin
      Depth := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
      repeat
         Edit11.Text := RealToString(Depth,-18,0);
         DEMDatum.VincentyPointAtDistanceBearing(TowerLat,TowerLong,Depth,Azimuth,Lat2,Long2);
         Depth := Depth - 5000;
      until DEMglb[RegionalDEM].LatLongDegreeInDEM(Lat2,Long2);
      MDDef.ObsAboveGround := TowerHeight;
      MDDef.TargetAboveGround := 0;
      StartLOS(true,JustWandering,RegionalDEM,TowerLat,TowerLong,Lat2,Long2,DEMglb[ComputeDEM].SelectionMap,true);
   end;
end;


procedure TPhotoRegForm.BitBtn8Click(Sender: TObject);
begin
   If (LightningTable <> 0) then begin
      CloseAndNilNumberedDB(LightningTable);
      BitBtn8.Enabled := false;
      BitBtn15.Enabled := false;
      BitBtn18.Enabled := false;
      BitBtn19.Enabled := false;
      BitBtn21.Enabled := false;
      SpeedButton1.Enabled := false;
   end;
end;

procedure TPhotoRegForm.CrossShotBitBtn8Click(Sender: TObject);
begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.CrossShotBitBtn8Click (cross shot)');
   {$EndIf}
   BitBtn2Click(Sender);
end;


procedure TPhotoRegForm.BitBtn9Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to NumDEMDataSetsOpen do if (DEMGlb[i] <> Nil) then DEMGlb[i].SelectionMap.DoCompleteMapRedraw;
end;


procedure TPhotoRegForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := true;
end;

procedure TPhotoRegForm.BitBtn10Click(Sender: TObject);
var
   CanClose : boolean;
   i : integer;
begin
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn10Click enter--close program button');
   {$EndIf}
   DragonPlotDef.DefaultTowerName := ComboBox1.Text;
   wmdem.Saveconfiguration1Click(Sender);
   WMDem.ProgramClosing := true;
   if (ComputeDEM <> 0) and (DEMglb[ComputeDEM] <> Nil) then DEMglb[ComputeDEM].SelectionMap.Closable := true;

   for I := 1 to MaxDataBase do if (GISdb[i] <> Nil) then GISdb[i].FanCanClose := true;

   DEM_Manager.CloseAllWindowsAndData;
   Halt;


   ClosePerspectives;

   CloseWindows;

  {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn10Click 2');
  {$EndIf}

   FormCloseQuery(Nil,CanClose);

  {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn10Click 3');
  {$EndIf}

   WmDEM.Close;

  {$IfDef RecordClosingProblems}
  WriteLineToDebugFile('TPhotoRegForm.BitBtn10Click done');
  {$EndIf}
end;


procedure TPhotoRegForm.BitBtn11Click(Sender: TObject);
var
   fName : PathStr;
   ts : tStringList;
   i : integer;
   bmp : Graphics.tBitmap;
begin
   {$IfDef RecordKML}
   WriteLineToDebugFile('****** TPhotoRegForm.BitBtn11Click (Record KML) in');
   {$EndIf}

   PetImage.CopyImageToBitmap(DEMPersF[1].Image1,bmp);
   KMLLogo2FileName := MDTempDir + 'View_of_shot.png';
   bmp.SaveToFile(KMLLogo2FileName);
   Bmp.Free;

   CreateBitmap(bmp,800,1200);
   for i := 0 to pred(Memo1.Lines.Count) do bmp.Canvas.TextOut(5,5+i*15,Memo1.Lines[i]);

   PetImage.GetImagePartOfBitmap(bmp);
   KMLLogo1FileName:= MDTempDir + 'Shot_details.gif';
   bmp.SaveToFile(KMLLogo1FileName);
   Bmp.Free;
   {$IfDef RecordKML}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn11Click two logos done');
   {$EndIf}

   fName := Petmar.NextFileNumber(MDTempDir,'sighting_',DefaultDBExt);

   MDDef.KML_DB_tables := false;
   MDDef.KMLOutputOption := 0;
   MDDef.AskAboutKMLExport := false;
   MDDef.KMLOpenGoogleEarth := true;
   MDDef.ZipKMLfiles := false;

   if (TargetMap <> Nil) then begin
      TargetMap.QuickbasemaptoGoogleEarth1Click(Sender);
   end;

   KMLLogo1FileName := '';
   KMLLogo2FileName := '';
   BitBtn11.Enabled := false;

   {$IfDef RecordKML}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn11Click out');
   {$EndIf}
end;


procedure TPhotoRegForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   if (LastSighting <> Nil) then LastSighting.Free;
end;


procedure TPhotoRegForm.LoadTowersIntoComboBoxes;
var
   TStr : shortString;

   procedure AddToBoxes;
   begin
      ComboBox1.Items.Add(TStr);
      ComboBox2.Items.Add(TStr);
      ComboBox3.Items.Add(TStr);
      ComboBox4.Items.Add(TStr);
      ComboBox5.Items.Add(TStr);
      ComboBox6.Items.Add(TStr);
   end;

begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.LoadTowersIntoComboBoxes in');
   {$EndIf}
   with GISdb[WMDEM.TowerTable].MyData do begin
      ComboBox1.Items.Clear;
      ComboBox2.Items.Clear;
      ComboBox3.Items.Clear;
      ComboBox4.Items.Clear;
      ComboBox5.Items.Clear;
      ComboBox6.Items.Clear;

      ApplyFilter('(USE=' + QuotedStr('Y') + ') AND (NAME = ' + QuotedStr('O -*') + ')');

      while not EOF do begin
         TStr := GetFieldByNameAsString('NAME');
         AddToBoxes;
         Next;
      end;

      ApplyFilter('(USE=' + QuotedStr('Y') + ') AND (NAME <>' + QuotedStr('O -*') + ')');
      while not EOF do begin
         TStr := GetFieldByNameAsString('NAME');
         AddToBoxes;
         Next;
      end;
      GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
      ApplyFilter('(USE=' + QuotedStr('Y') + ')');
      BitBtn23.Enabled := (RecordCount > 0);
      {$IfDef RecordProblems}
      WriteLineToDebugFile('Tower Total to Use: ' + IntToStr(RecordCount));
      {$EndIf}
   end;
end;


procedure TPhotoRegForm.CreateProfile(var DEMLOSF : TDEMLOSF; TowerLat,TowerLong,PLSSLat,PLSSLong,TowerHeight : float; LabelStr : ShortString);
var
   Dist,Bearing,extra : float;
begin
   DEMDatum.VincentyCalculateDistanceBearing(TowerLat,TowerLong,PLSSLat,PLSSLong,Dist,Bearing);
   extra := 5000;
   repeat
      DEMDatum.VincentyPointAtDistanceBearing(TowerLat,TowerLong,Dist + extra,Bearing,PLSSLat,PLSSLong);
      Extra := Extra - 500;
   until (Extra < 0) or (DEMglb[ComputeDEM].LatLongDegreeInDEM(PLSSLat,PLSSLong));

   DEMLOSF := StartLOS(true,JustWandering,ComputeDEM,TowerLat,TowerLong,PLSSLat,PLSSLong,DEMglb[ComputeDEM].SelectionMap,true);
   if (DEMLOSf <> Nil) then begin
      DEMLOSF.Closable := false;
      DEMLOSF.Caption := 'Profile from ' +  LabelStr + ' to fire';
   end;
end;


procedure TPhotoRegForm.BitBtn13Click(Sender: TObject);
var
   BlockDist,
   Distance,tz,Pitch : float;
   t1,t2,t3 : shortString;
   vis,OnMap : boolean;
begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn13Click 1, Edit11.text=' + Edit11.Text);
   {$EndIf}
   CheckCameraParameters;
   LastSighting.Clear;
   CloseWindows;
   InsureLocationMapOK;

   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('NAME=' + QuotedStr(Trim(ComboBox4.Text)));
   GISdb[WMDEM.TowerTable].MyData.First;
   GISdb[WMDEM.TowerTable].ValidLatLongFromTable(TowerLat,TowerLong);
   DefineTowerDatum(TowerLong);

   OnMap := DEMglb[ComputeDEM].GetElevFromLatLongDegree(PLSSLat,PLSSLong,tz);

   t1 := ' (computed)';
   t2 := ' (entered)';
   if (Sender = Nil) then begin
      t3 := '';
      if GISdb[LightningTable].MyData.FieldExists('DTG') then begin
         t3 := '(' + GISdb[LightningTable].MyData.GetFieldByNameAsString('DTG') + ')';
      end
      else if GISdb[LightningTable].MyData.FieldExists('DATE') then begin
         t3 := '(' + GISdb[LightningTable].MyData.GetFieldByNameAsString('DATE') + '  ' + GISdb[LightningTable].MyData.GetFieldByNameAsString('TIME') +  ')';
      end;

      t3 := 'Lightning strike' + t3;
      GISdb[LightningTable].ValidLatLongFromTable(PLSSLat,PLSSLong);

      if (PLSS[1] <> Nil) then begin
         PLSSString := PLSSLocation(PLSSLat,PLSSLong);
      end;
   end
   else begin
      if PLSSentered then begin
         t1 := ' (entered)';
         t2 := ' (computed)';
         t3 := 'PLSS';
      end
      else begin
         t3 := 'Lat/Long';
      end;
   end;

   Depth := GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));

   DisplayMessage(t3 + ' done ' + DateToStr(Date) + '  ' + CurrentMilitaryTime(true)  );
   DisplayMessage('  Tower: ' + ComboBox4.Text + ' Elev: ' + RealToString(CameraElev,6,1) + ' m   (' +  RealToString(CameraElev/FeetToMeters,6,1) + ' ft)');
   DisplayMessage('Estimated location:');
   if (PLSSString <> '') then DisplayMessage('   ' + PLSSString + T1);
   DisplayMessage('   ' + DEMglb[ComputeDEM].SelectionMap.MapDraw.PrimaryMapDatum.h_DatumCode + '   ' + LatLongDegreeToString(PLSSLat,PLSSLong,MDDEF.OutPutLatLongMethod) + t2);
   if DragonPlotDef.ShowUTM then DisplayMessage('    ' + DEMglb[ComputeDEM].SelectionMap.MapDraw.PrimaryMapDatum.UTMStringFromLatLongDegree(PLSSLat,PLSSLong));

   if OnMap then DisplayMessage('   Elev: ' + RealToString(tz,6,1) + ' m   (' +  RealToString(tz/FeetToMeters,6,1) + ' ft)')
   else DisplayMessage('   Not on map');
   DEMDatum.VincentyCalculateDistanceBearing(TowerLat,TowerLong,PLSSLat,PLSSLong,Distance,Azimuth);

   DisplayMessage('  Computed bearing angle: ' + RealToString(Azimuth,-8,2) + '°   ' + ConvertToDegreesString(Azimuth,DecMinutes));
   DisplayMessage('  Computed distance: ' + SmartDistanceMetersFormat(Distance,MDDef.EnglishDistanceUnits));
   if OnMap then begin
      Pitch := arctan((tz - CameraElev - DropEarthCurve(Distance)) / Distance) / DegToRad;
      DisplayMessage('  Computed pitch: ' + RealToString(Pitch,-8,2) + '°   ' + ConvertToDegreesString(Pitch,DecMinutes));
   end;

   DisplayMessage('');

   ShowOnMaps(plssLat,plssLong,true);

   SetUpPanoramaView(DEMPersF[1],TowerLat,TowerLong,TowerHeight,Depth,Azimuth,HFOV,VFOV,Pitch,1,'View from ' + Trim(ComboBox4.Text));

   DEMPersF[1].Top := 0;
   DEMPersF[1].Left := Screen.Width - 10 - Self.Width - DEMPersF[1].Width;
   DEMPersF[1].Closable := false;
   Vis := DEMglb[ComputeDEM].LatLongDegreePointsIntervisible(TowerLat,TowerLong,TowerHeight,PLSSLat,PLSSLong,1,Distance,BlockDist);

   if Vis then begin
      t2 := 'L';
      DEMPersF[1].SymbolAtPitchAzimith(Pitch,Azimuth,DragonPlotDef.VisibleSymbol,true);
      DisplayMessage('Location should be visible from tower');
   end
   else begin
      t2 := 'Masked l';
      DEMPersF[1].SymbolAtPitchAzimith(Pitch,Azimuth,DragonPlotDef.MaskedSymbol,true);
      DisplayMessage('Location should be masked from tower'); // at ' + SmartDistanceMetersFormat(BlockDist,MDDef.EnglishDistanceUnits));
   end;
   DEMPersF[1].Caption := t2 + 'ocate from ' +  ComboBox4.Text;
   Edit3.Text := RealToString(Azimuth,-12,-2);
   ArchiveShot;
  {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn13Click end, Edit11.text=' + Edit11.Text);
  {$EndIf}
end;


procedure TPhotoRegForm.BitBtn14Click(Sender: TObject);
var
   Lat2,Long2 : float;
begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn14Click in');
   {$EndIf}
   ComboBox1.Text := ComboBox4.Text;
   CheckCameraParameters(true);

   if (Sender = BitBtn14) then GetLatLong(DEMglb[ComputeDEM].SelectionMap.MapDraw.PrimaryMapDatum,'Smoke sighting (WGS84)',PLSSLat,PLSSLong);
   if (PLSS[1] <> Nil) then begin
      PLSSString := PLSSLocation(PLSSLat,PLSSLong);
      Label24.Caption := PLSSString;
   end;
   MolodenskiyTransformation(PLSSLat,PLSSLong,Lat2,Long2,DEMglb[ComputeDEM].SelectionMap.MapDraw.PrimaryMapDatum,DEMglb[ComputeDEM].SelectionMap.MapDraw.SecondaryMapDatum);
   BitBtn13.Enabled := true;
   PLSSentered := false;
   Label26.Caption := 'WGS84: ' + LatLongDegreeToString(PLSSLat,PLSSLong,MDDEF.OutPutLatLongMethod);
   Forms.Screen.Cursor := crDefault;
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn14Click out');
   {$EndIf}
end;


procedure TPhotoRegForm.BitBtn15Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn15,DragonPlotDef.NegLightningSymbol,'- Lightning strikes');
end;


procedure TPhotoRegForm.BitBtn16Click(Sender: TObject);
var
   SummaryBitmap : tBitmap;
begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn16Click (tower viewshed) in');
   {$EndIf}
   SummaryBitmap := Nil;
   DEMGlb[RegionalDEM].SelectionMap.DoFastMapRedraw;
   PanoramaSpeedButtonClick(Sender);
end;

procedure TPhotoRegForm.BitBtn12Click(Sender: TObject);
begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn12Click in');
   {$EndIf}
   if (PLSS[1] <> Nil) then begin
      {$IfDef RecordProblems}
      WriteLineToDebugFile('PLSS was not nil');
      {$EndIf}
      PLSSentered := true;
      if GetPLSSLocation(PLSSString,PLSSLat,PLSSLong,DEMglb[RegionalDEM].SelectionMap) then begin
         {$IfDef RecordProblems}
         WriteLineToDebugFile('GetPLSSLocation success, ' + Label26.Caption);
         {$EndIf}
         BitBtn13.Enabled := true;
         Label24.Caption := PLSSString;
         Label26.Caption := 'WGS84:  ' + LatLongDegreeToString(PLSSLat,PLSSLong,MDDEF.OutPutLatLongMethod);
         {$IfDef RecordProblems}
         WriteLineToDebugFile('GetPLSSLocation success, ' + Label26.Caption);
         {$EndIf}
      end
      else begin
         {$IfDef RecordProblems}
         WriteLineToDebugFile('GetPLSSLocation failure');
         {$EndIf}
         BitBtn13.Enabled := false;
         Label24.Caption := '';
         Label26.Caption := '';
      end;
   end;
   Forms.Screen.Cursor := crDefault;
   {$IfDef RecordProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn12Click out');
   {$EndIf}
end;


procedure TPhotoRegForm.RadioGroup1Click(Sender: TObject);
begin
   case RadioGroup1.ItemIndex of
      0 : begin
            Edit4.Text := '3';
            Edit5.Text := '4';
          end;
      1 : begin
             Edit4.Text := '9';
             Edit5.Text := '12';
          end;
      2 : begin
             Edit4.Text := '18';
             Edit5.Text := '24';
          end;
      3 : begin
             Edit4.Text := IntToStr(DragonPlotDef.CustomVFOV);
             Edit5.Text := IntToStr(DragonPlotDef.CustomHFOV);
          end;
   end;
   Edit4.Enabled := RadioGroup1.ItemIndex = 3;
   Edit5.Enabled := RadioGroup1.ItemIndex = 3;
   DragonPlotDef.ViewLevelIndex := RadioGroup1.ItemIndex;
end;


procedure TPhotoRegForm.RadioGroup2Click(Sender: TObject);
begin
   DragonPlotDef.TargetMapType := RadioGroup2.ItemIndex;
   if (DragonPlotDef.TargetMapType = 0) then begin
      if (SatImage[1] <> Nil) then SatImage[1].SelectionMap.WindowState := wsMinimized;
   end
   else begin
      if (TargetMap <> Nil) then begin
         TargetMap.Closable := true;
         TargetMap.Destroy;
      end;
   end;
   TargetMap := Nil;
end;

procedure TPhotoRegForm.CheckBox10Click(Sender: TObject);
begin
   DragonPlotDef.ShowUTM := CheckBox10.Checked;
end;

procedure TPhotoRegForm.CheckBox1Click(Sender: TObject);
begin
   DragonPlotDef.ShowLOS := CheckBox1.Checked;
end;

procedure TPhotoRegForm.CheckBox99Click(Sender: TObject);
begin
   DragonPlotDef.FireAccessMap := CheckBox99.Checked;
end;

procedure TPhotoRegForm.BitBtn4Click(Sender: TObject);
begin
   CheckCameraParameters;
   gbLatStart := TowerLat;
   gbLongStart := TowerLong;
   DEMglb[ComputeDEM].SelectionMap.Geodeticbearing1Click(Nil);
   DEMglb[ComputeDEM].SelectionMap.SetFocus;
end;


procedure TPhotoRegForm.BitBtn30Click(Sender: TObject);
var
   SummaryBitmap : tBitmap;
begin
   {$IfDef RecordViewshedProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn30Click (composite viewshed) in');
   {$EndIf}
   CloneImageToBitmap(DEMGlb[RegionalDEM].SelectionMap.Image1,SummaryBitmap);
   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   DEMGlb[RegionalDEM].SelectionMap.MapDraw.InsureAllFansDrawn(GISdb[WMDEM.TowerTable].MyData,SummaryBitmap);
   {$IfDef RecordViewshedProblems}
   SummaryBitmap.SaveToFile(MDTempDir + 'vs.bmp');
   {$EndIf}

   DEMGlb[RegionalDEM].SelectionMap.IHSmergeOntoMap(SummaryBitmap);
end;

procedure TPhotoRegForm.BitBtn31Click(Sender: TObject);
begin
   DragonPlotDef.VisDefPlot := true;
   ComboBox6Change(Sender);
end;

procedure TPhotoRegForm.BitBtn32Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn32,MDDef.VisPtSymbol,'Visible strikes');
end;

procedure TPhotoRegForm.BitBtn33Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn33,MDDef.MaskPtSymbol,'Masked strikes');
end;

procedure TPhotoRegForm.BitBtn35Click(Sender: TObject);
begin
   PickLineSizeAndColor('Shot lines',BitBtn35,DragonPlotDef.ShotLineColor,DragonPlotDef.ShotLineWidth);
end;

procedure TPhotoRegForm.BitBtn3Click(Sender: TObject);
var
   fName : PathStr;
   DefExt : byte;
   Ext : ExtStr;
   TStr : ShortString;
begin
    FName := ArchiveDirectory;
    DefExt := 0;
    if GetFileMultipleMask('Archive to restore','Any archive|*.JPG;*.TXT|Screen capture|*.JPG|Text log|*.TXT',
        FName,DefExt) then begin
        Ext := UpperCase(ExtractFileExt(fName));
        TStr := 'Archived shot ' + ExtractFileName(fName);
        if (Ext = '.JPG') then DisplayBitmap(fName,TStr)
        else if (Ext = '.TXT') then QuickOpenEditWindow(FName,TStr);
    end;
end;


procedure TPhotoRegForm.RadioGroup3Click(Sender: TObject);
var
  ls : float;
begin
   case RadioGroup3.ItemIndex of
      0 : ls := 0.02;
      1 : ls := 0.05;
      2 : ls := 0.1;
      3 : ls := MDDef.BlowUpLatSize;
   end;
   Edit12.Text := RealToString(ls,-8,-6);
   Edit13.Text := RealToString(ls * DragonPlotDef.BlowUpSizeAspect,-8,-6);
   Edit12.Enabled := (RadioGroup3.ItemIndex = 3);
   Edit13.Enabled := (RadioGroup3.ItemIndex = 3);
   DragonPlotDef.MapLevelIndex := RadioGroup3.ItemIndex;
end;


procedure TPhotoRegForm.Edit12Change(Sender: TObject);
begin
   CheckEditString(Edit12.Text,MDDef.BlowUpLatSize);
end;

procedure TPhotoRegForm.Edit13Change(Sender: TObject);
begin
   CheckEditString(Edit13.Text,MDDef.BlowUpLongSize);
end;


procedure TPhotoRegForm.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,DragonPlotDef.CustomVFOV);
end;


procedure TPhotoRegForm.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,DragonPlotDef.CustomHFOV);
end;

procedure TPhotoRegForm.Edit19Change(Sender: TObject);
begin
   CheckEditString(Edit19.Text,DragonPlotDef.MinPitch);
end;

procedure TPhotoRegForm.Edit20Change(Sender: TObject);
begin
   CheckEditString(Edit20.Text,DragonPlotDef.MaxPitch);
end;

procedure TPhotoRegForm.Edit24Change(Sender: TObject);
begin
   CheckEditString(Edit24.Text,DragonPlotDef.BlowUpSizeAspect);
end;

procedure TPhotoRegForm.Edit25Change(Sender: TObject);
begin
   CheckEditString(Edit25.Text,DragonPlotDef.FireAccessMapSize);
end;

procedure TPhotoRegForm.Edit26Change(Sender: TObject);
begin
   CheckEditString(Edit26.Text,DragonPlotDef.FireAccessMapBlowup);
end;

procedure TPhotoRegForm.Edit27Change(Sender: TObject);
begin
   BitBtn1.Enabled := true;
end;

procedure TPhotoRegForm.CheckBox2Click(Sender: TObject);
begin
   DragonPlotDef.PlotAccuracy := CheckBox2.Checked;
end;


procedure TPhotoRegForm.CheckBox3Click(Sender: TObject);
begin
   DragonPlotDef.TerrainShadows := CheckBox3.Checked;
end;

procedure TPhotoRegForm.BitBtn17Click(Sender: TObject);
var
   fName : PathStr;
   Ext : ExtStr;
begin
   {$IfDef RecordLightningProblems}
    WriteLineToDebugFile('TPhotoRegForm.BitBtn17Click in (import lighthing file)',true);
   {$EndIf}
    FName := LightningDirectory;
    if (LightningTable <> 0) then CloseAndNilNumberedDB(LightningTable);
    if not Petmar.GetFileFromDirectory('Lightning data','*.txt;*.dbf;*.csv',fName) then exit;
    Ext := UpperCase(ExtractFileExt(fName));

   {$IfDef RecordLightningProblems}
    WriteLineToDebugFile('lighthing file=' + fName);
   {$EndIf}

    OpenNumberedGISDataBase(LightningTable,fName,false,DEMglb[RegionalDEM].SelectionMap);
    GISdb[LightningTable].MyData.TrimAllStringFields;
    GISdb[LightningTable].KMLExportable := false;
    if GISdb[LightningTable].dbTablef <> Nil then begin
       GISdb[LightningTable].dbTablef.WindowState := wsNormal;
       GISdb[LightningTable].dbTablef.Panel2.Visible := false;
       GISdb[LightningTable].dbTablef.Width := Self.Width;
       GISdb[LightningTable].dbTablef.Height := 300;
       GISdb[LightningTable].dbTablef.Top := Self.Top + Self.Height + 25;
       GISdb[LightningTable].dbTablef.Left := Self.Left;
    end;
    GISdb[LightningTable].AddSymbolToDB;
    GISdb[LightningTable].dbOpts.DBAutoShow := dbasSymbolInDB;
    GISdb[LightningTable].AddSequentialIndex('REC_NO',false);
    Self.BringToFront;
    GISdb[LightningTable].EmpSource.Enabled := false;
    if DragonPlotDef.VisDefPlot then ComboBox6Change(Sender)
    else BitBtn18Click(Sender);
    GISdb[LightningTable].EmpSource.Enabled := true;

    BitBtn8.Enabled := true;
    BitBtn15.Enabled := GISdb[LightningTable].MyData.FieldExists('POLARITY');
    BitBtn18.Enabled := true;
    BitBtn19.Enabled := true;
    BitBtn21.Enabled := true;
    BitBtn31.Enabled := true;
    SpeedButton1.Enabled := true;
    ComboBox6.Enabled := true;
    {$IfDef RecordLightningProblems}
    WriteLineToDebugFile('TPhotoRegForm.BitBtn17Click out (import lighthing file)');
   {$EndIf}
end;


procedure TPhotoRegForm.BitBtn18Click(Sender: TObject);

      procedure PlotStrikes(ch : char; Symbol : tFullSymbolDeclaration);
      var
         i : integer;
      begin
         if (ch = 'X') then GISdb[LightningTable].MyData.ApplyFilter('')
         else GISdb[LightningTable].MyData.ApplyFilter('POLARITY=' + QuotedStr(ch));
         {$IfDef RecordLightningProblems}
         WriteLineToDebugFile('Polarity ' + ch + ' has ' + IntToStr(GISdb[LightningTable].MyData.RecordCount) + ' recs',true);
         {$EndIf}
         i := 0;
         GISdb[LightningTable].MyData.First;
         while not GISdb[LightningTable].MyData.eof do begin
            GISdb[LightningTable].MyData.Edit;
            GISdb[LightningTable].MyData.PostPointSymbol(Symbol.DrawingSymbol,Symbol.Size,Symbol.Color);
            GISdb[LightningTable].MyData.Next;
            inc(i);
         end;
         {$IfDef RecordLightningProblems}
         WriteLineToDebugFile('  symbols updated=' + intToStr(i));
         {$EndIf}
      end;

begin
  {$IfDef RecordLightningProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn18Click (plot lightning) in', true);
  {$EndIf}
   DragonPlotDef.VisDefPlot := false;
  //with GISdb[LightningTable].MyData do begin
      if GISdb[LightningTable].MyData.FieldExists('POLARITY') then begin
         {$IfDef RecordLightningProblems}
         WriteLineToDebugFile('Polarity field exists');
         {$EndIf}
         PlotStrikes('P',DragonPlotDef.PosLightningSymbol);
         PlotStrikes('N',DragonPlotDef.NegLightningSymbol);
      end
      else begin
         {$IfDef RecordLightningProblems}
         WriteLineToDebugFile('No Polarity field');
         {$EndIf}
         PlotStrikes('X',DragonPlotDef.PosLightningSymbol);
      end;
      //GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));

      GISdb[LightningTable].MyData.ApplyFilter('');
      GISdb[LightningTable].RedrawLayerOnMap;
      Forms.Screen.Cursor := crDefault;
      //GISdb[WMDEM.TowerTable].dbOpts.DBAutoShow := dbasDefault;
   //end;
end;


procedure TPhotoRegForm.ComboBox6Change(Sender: TObject);
begin
  {$IfDef RecordLightningProblems}
   WriteLineToDebugFile('TPhotoRegForm.ComboBox6Change in');
  {$EndIf}
   if (LightningTable = 0) or (ComboBox6.Text = '') then begin
      if (ComboBox6.Text = '') then MessageToContinue('Pick a tower');
      exit;
   end;

   ComboBox1.Text := ComboBox6.Text;
   CheckCameraParameters;
   GISdb[LightningTable].IntervisibilityFromPoint(TowerLat,TowerLong,CameraElev,TowerHeight);
   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
  {$IfDef RecordLightningProblems}
   WriteLineToDebugFile('TPhotoRegForm.ComboBox6Change out');
  {$EndIf}
end;


procedure TPhotoRegForm.BitBtn19Click(Sender: TObject);
var
   Lat,Long : float;
begin
  {$IfDef RecordLightningProblems}
   WriteLineToDebugFile('TPhotoRegForm.BitBtn19Click (enter strike)');
  {$EndIf}
    GetLatLong(DEMglb[ComputeDEM].SelectionMap.MapDraw.PrimaryMapDatum,'Lightning strike location',Lat,Long);
    GISdb[LightningTable].MyData.Insert;
    GISdb[LightningTable].MyData.SetFieldByNameAsFloat('LAT', Lat);
    GISdb[LightningTable].MyData.SetFieldByNameAsFloat('LONG',Long);
    GISdb[LightningTable].MyData.SetFieldByNameAsString('POLARITY','N');
    GISdb[LightningTable].MyData.Post;
    ComboBox6Change(Sender);
end;


procedure TPhotoRegForm.BitBtn1Click(Sender: TObject);
begin
   Memo1.Lines.SaveToFile(ArchiveDirectory + 'Incident_' + Edit27.Text + '.txt');
end;

procedure TPhotoRegForm.DBGrid2DblClick(Sender: TObject);
//var
   //Inbounds : boolean;
begin
  {$IfDef RecordLightningProblems}
   WriteLineToDebugFile('TPhotoRegForm.DBGrid2DblClick',true);
  {$EndIf}
   ComboBox4.Text := ComboBox6.Text;
   GISdb[LightningTable].ValidLatLongFromTable(PlssLat,PlssLong);
   if (PLSS[1] <> Nil) then begin
      PLSSString := PLSSLocation(PLSSLat,PLSSLong);
   end;
   BitBtn13Click(Nil);
   GISdb[LightningTable].MyData.ApplyFilter('');
end;


procedure TPhotoRegForm.SpeedButton1Click(Sender: TObject);
begin
  {$IfDef RecordLightningProblems}
   WriteLineToDebugFile('TPhotoRegForm.SpeedButton1Click (ID lightning strike)');
  {$EndIf}
   if (ComputeDEM <> 0) then DEMglb[ComputeDEM].SelectionMap.BringToFront;
   ChangeDEMNowDoing(IDJudomia,JustWandering,'Pick lightning strike for analysis');
   DBEditting := LightningTable;
end;


procedure TPhotoRegForm.ComboBox4Change(Sender: TObject);
begin
   ComboBox1.Text := ComboBox4.Text;
end;


procedure TPhotoRegForm.ComboBox3Change(Sender: TObject);
begin
   ComboBox1.Text := ComboBox3.Text;
end;

procedure TPhotoRegForm.ComboBox5Change(Sender: TObject);
begin
   ComboBox1.Text := ComboBox5.Text;
end;

procedure TPhotoRegForm.BitBtn23Click(Sender: TObject);
begin
   GISdb[WMDEM.TowerTable].MyData.ApplyFilter('(NAME=' + QuotedStr('O -*') + ' OR NAME=' + QuotedStr('S_*') + ') AND USE=' + QuotedStr('Y'));
   //GISdb[WMDEM.TowerTable].TheData.Filtered := true;
   while not GISdb[WMDEM.TowerTable].MyData.eof do begin
      if AnswerIsYes('Delete ' + GISdb[WMDEM.TowerTable].MyData.GetFieldByNameAsString('NAME')) then begin
         GISdb[WMDEM.TowerTable].MyData.Edit;
         GISdb[WMDEM.TowerTable].MyData.SetFieldByNameAsString('USE','N');
         GISdb[WMDEM.TowerTable].MyData.Next;
      end
      else GISdb[WMDEM.TowerTable].MyData.Next;
   end;
   //GISdb[WMDEM.TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   LoadTowersIntoComboBoxes;
   DEMGlb[RegionalDEM].SelectionMap.DoCompleteMapRedraw;
end;


procedure TPhotoRegForm.BitBtn24Click(Sender: TObject);
var
   MenuStr : shortString;
   Lat,Long,z : float;
   wf : tWeaponsFan;
begin
   with GISdb[WMDEM.TowerTable] do begin
     {$IfDef RecordTemporaryTowers}
     WriteLineToDebugFile('TPhotoRegForm.BitBtn24Click in,  towers =' + IntToStr(MyData.RecordCount));
     {$EndIf}
      MenuStr := '';
      GetString('Label',MenuStr,false,ReasonableTextChars);
      z := 2;
      ReadDefault('Observer above ground (m)',z);
      Lat := 41;
      Long := -118;
      GetLatLn.GetLatLongDefault(WGS84DatumConstants,'Sighting location',Lat,Long);
      InitializeWeaponsFan(wf);

      wf.Fan_Name := 'O - ' + MenuStr;
      wf.W_Lat  := Lat;
      wf.W_Long  := Long;
      wf.W_Up := z;

      AddFanToWeaponsTable(TheMapOwner.MapDraw.PrimaryMapDatum,false,true,MyData,wf);

      LoadTowersIntoComboBoxes;
      DEMGlb[RegionalDEM].SelectionMap.DoCompleteMapRedraw;
     {$IfDef RecordTemporaryTowers}
     WriteLineToDebugFile('TPhotoRegForm.BitBtn24Click out, towers =' + IntToStr(MyData.RecordCount));
     {$EndIf}
   end;
end;

procedure TPhotoRegForm.BitBtn20Click(Sender: TObject);
begin
  PanoramaSpeedButtonClick(Sender);
end;

procedure TPhotoRegForm.BitBtn21Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn21,DragonPlotDef.PosLightningSymbol,'+ Lightning strikes');
end;

procedure TPhotoRegForm.BitBtn22Click(Sender: TObject);
begin
   ComboBox1.Text := ComboBox5.Text;
   CheckCameraParameters(true);
   MDDef.HorizonLength := TowerRange;
   MDDef.ObsAboveGround := TowerHeight;
   MDDef.HorizonIncrement := 1;
   //MDDef.HorizonTextTable := false;
   MDDef.HorizonVertAngleGraph := false;
   MDDef.HorizonDistanceGraph := false;
   DEMGlb[ComputeDEM].SelectionMap.DrawHorizon(TowerLat,TowerLong);
end;


procedure TPhotoRegForm.BitBtn25Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then PickSymbol(BitBtn25,DragonPlotDef.VisibleSymbol,'Visible points');
end;

procedure TPhotoRegForm.BitBtn26Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then PickSymbol(BitBtn26,DragonPlotDef.MaskedSymbol,'Masked points');
end;

procedure TPhotoRegForm.BitBtn27Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then PickSymbol(BitBtn27,DragonPlotDef.AccuracySymbol,'Accuracy plots');
end;

procedure TPhotoRegForm.BitBtn28Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then PickSymbol(BitBtn28,DragonPlotDef.TowerSymbol,'Towers');
end;

procedure TPhotoRegForm.BitBtn29Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then begin
      PickSymbol(BitBtn29,DragonPlotDef.ResultSymbol,'Shot results');
      MDDef.KeyLocationSymbol := DragonPlotDef.ResultSymbol;
   end;
end;

initialization
finalization
   {$IfDef RecordProblems}
   WriteLineToDebugFile('register_view finalization');
   {$EndIf}

   {$IfDef RecordKML}
   WriteLineToDebugFile('RecordKML active');
   {$EndIf}

   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing register_view');
   {$EndIf}
end.
