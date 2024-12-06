 unit dp_control;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}



{$I Nevadia_defines.inc}


{$IfDef RecordProblems}
   {$IfDef Debug}
      //{$Define RecordTowerProblems}
      //{$Define RecordCheckCameraParamters}
      //{$Define RecordCreateBlowUpMap}
      //{$Define RecordDrawSighting}
      //{$Define RecordDP}
      //{$Define RecordFigureView}
      //{$Define RecordClosing}
      //{$Define RecordLightning}
      //{$Define RecordKML}
      //{$Define RecordDefineDatum}
      //{$Define RecordClosingPersp}
      //{$Define RecordPostResults}
      //{$Define RecordViewshed}
      //{$Define RecordOpenDEM}
      //{$Define RecordGazetteer}
      //{$Define RecordTemporaryTowers}
      //{$Define DebugDefaults}
      //{$Define RecordPerspective}
      //{$Define CheckAccessibility}
      //{$Define RecordHiResDEM}
   {$Else}
      //{$Define RecordDP}
   {$EndIf}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
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
// end units for inline of the core DB functions

   Windows,  SysUtils,Classes, Graphics,  Forms,Messages, System.Diagnostics,System.TimeSpan,System.Math,
   Winapi.tlHelp32,
   Grids, DBGrids,
   {$IfDef ExSats}
   {$Else}
      DEMEros,
   {$EndIf}
   DEMMapf,Petmar_types,DEMLOSW,DEMDataBase,DEMdefs,
   DEMPersw,PetImage,PetImage_form,PETMAR, Vcl.Dialogs, Vcl.StdCtrls,
   Vcl.ExtCtrls, Vcl.Buttons, Vcl.Controls, Vcl.ComCtrls, Vcl.Menus;

type
   tShotMode = (smSingle,smCross);
type
  TDragonPlotForm = class(TForm)
    Panel1: TPanel;
    FontDialog1: TFontDialog;
    BitBtn3: TBitBtn;
    AdminPopupMenu1: TPopupMenu;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label3: TLabel;
    Label9: TLabel;
    Label19: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit3: TEdit;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit9: TEdit;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    BitBtn5: TBitBtn;
    TabSheet7: TTabSheet;
    Label22: TLabel;
    Label23: TLabel;
    SecondaryTowerComboBox2: TComboBox;
    GroupBox2: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Edit14: TEdit;
    Edit15: TEdit;
    GroupBox3: TGroupBox;
    Label20: TLabel;
    Label21: TLabel;
    Edit16: TEdit;
    Edit17: TEdit;
    CrossShotBitBtn8: TBitBtn;
    TabSheet8: TTabSheet;
    Label24: TLabel;
    Label26: TLabel;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    TabSheet4: TTabSheet;
    GroupBox5: TGroupBox;
    RadioGroup2: TRadioGroup;
    CheckBox3: TCheckBox;
    TabSheet2: TTabSheet;
    Label12: TLabel;
    PanoramaSpeedButton: TSpeedButton;
    Pitch: TLabel;
    Edit11: TEdit;
    Edit23: TEdit;
    BitBtn7: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn22: TBitBtn;
    BitBtn30: TBitBtn;
    TabSheet3: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Edit6: TEdit;
    Edit8: TEdit;
    CheckBox2: TCheckBox;
    BitBtn27: TBitBtn;
    TabSheet5: TTabSheet;
    BitBtn23: TBitBtn;
    BitBtn24: TBitBtn;
    BitBtn25: TBitBtn;
    BitBtn26: TBitBtn;
    RestoreConfigurationButton: TBitBtn;
    BitBtn28: TBitBtn;
    BitBtn29: TBitBtn;
    BitBtn35: TBitBtn;
    TabSheet9: TTabSheet;
    Panel2: TPanel;
    BitBtn18: TBitBtn;
    BitBtn21: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn31: TBitBtn;
    BitBtn32: TBitBtn;
    BitBtn33: TBitBtn;
    Panel3: TPanel;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn11: TBitBtn;
    Instancestoload1: TMenuItem;
    Tabsheet6: TTabSheet;
    GroupBox4: TGroupBox;
    Suze: TLabel;
    Label34: TLabel;
    GroupBox6: TGroupBox;
    CheckBox99: TCheckBox;
    Edit25: TEdit;
    Edit26: TEdit;
    RadioGroup3: TRadioGroup;
    Edit13: TEdit;
    Label16: TLabel;
    Label15: TLabel;
    Edit12: TEdit;
    CheckBox4: TCheckBox;
    GroupBox7: TGroupBox;
    Edit19: TEdit;
    Edit20: TEdit;
    Label14: TLabel;
    Label13: TLabel;
    AdminOptionsButton: TButton;
    BitBtn1: TBitBtn;
    PopupMenu2: TPopupMenu;
    DP11: TMenuItem;
    DP21: TMenuItem;
    DP31: TMenuItem;
    DP41: TMenuItem;
    DP51: TMenuItem;
    DP61: TMenuItem;
    KMLdelaysec1: TMenuItem;
    OpenGoogleEarth1: TMenuItem;
    Button1: TButton;
    ChangeregionalDEM1: TMenuItem;
    PrimaryTowerComboBox7 : TComboBox;
    Memo1: TMemo;
    BitBtn34: TBitBtn;
    BitBtn19: TBitBtn;
    PrepPLSS1: TMenuItem;
    BitBtn36: TBitBtn;
    BitBtn4: TBitBtn;
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PanoramaSpeedButtonClick(Sender: TObject);
    procedure SecondaryTowerComboBox2Change(Sender: TObject);
    procedure CrossShotBitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BitBtn10Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Edit19Change(Sender: TObject);
    procedure Edit20Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure DBGrid2DblClick(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure BitBtn29Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure CheckBox99Click(Sender: TObject);
    procedure Edit25Change(Sender: TObject);
    procedure Edit26Change(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
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
    procedure Button1Click(Sender: TObject);
    procedure ChangeRegionalDEM1Click(Sender: TObject);
    procedure Changeimagemap1Click(Sender: TObject);
    procedure ChangeTowers1Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Instancestoload1Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure Edit12Change(Sender: TObject);
    procedure Edit13Change(Sender: TObject);
    procedure RestoreConfigurationButtonClick(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure Edit23Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure DP11Click(Sender: TObject);
    procedure DP21Click(Sender: TObject);
    procedure DP31Click(Sender: TObject);
    procedure DP41Click(Sender: TObject);
    procedure DP51Click(Sender: TObject);
    procedure DP61Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure KMLdelaysec1Click(Sender: TObject);
    procedure OpenGoogleEarth1Click(Sender: TObject);
    procedure PrimaryTowerComboBox7Change(Sender: TObject);
    procedure BitBtn34Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure PrepPLSS1Click(Sender: TObject);
    procedure BitBtn36Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
  private
    { Private declarations }
     function CheckCameraParameters : boolean;
     function GetTowerSettings(TowerName : shortString; var Lat,Long,Height,GroundElevation,CameraElev,Range : float64) : boolean;
     procedure ArchiveShot;
     procedure DisplayMessage(Words : shortString);
     procedure CheckAccessibilityMap;
     procedure CreateProfile(var DEMLOSF : TDEMLOSF; TowerLat,TowerLong,PLSSLat,PLSSLong,TowerHeight : float64; LabelStr : ShortString);
     procedure ShowOnMaps(FoundPoint : boolean = true);
     procedure InsureLocationMapOK;
     procedure AddGazetteerToTargetMap;
     procedure SetTabSheets;
     procedure ProcessShot(ShotMode : tShotMode);
     procedure SetLastShotTime;
     procedure EndShot;
     procedure GetCurrentAndPolarity(var Polarity : shortstring; var Mag : float64);
     procedure SetLightningSymbols;
  public
    { Public declarations }
     DragonPlotInstance : integer;
     DEMPersF2,DEMPersF,DEMPersPan : TThreeDview;
     DEMLOSF,DEMLOSF2 : TDEMLOSF;
     AccessMap,TargetMap : tMapForm;
     SettingUp,
     FirstKML,
     SecondSighting,
     PLSSEntered : boolean;
     InstanceName,
     PLSSString,
     LastShotTime : shortstring;
     LastSighting : tStringList;
     ShotLat,ShotLong,
     plssLat,plssLong,
     TowerLat,TowerLong,TowerGroundElevation,TowerHeight,TowerRange,CameraElev,Depress,Azimuth,Pitch2,
     TowerLat2,TowerLong2,TowerGroundElevation2,TowerHeight2,TowerRange2,CameraElev2,Depress2,Azimuth2,
     Depth : float64;
     procedure CloseWindows;
     procedure LoadTowersIntoComboBoxes;
     procedure DrawTargetMap(var UseMap : tMapForm);
     procedure DrawSightingsOnMap(Found : boolean; var UseMap : tMapForm);
     procedure CreateSinglePerspective;

     procedure SetUpInstance(i : integer);
  end;


const
  TopWindow2 = 30;

     procedure LoadDEM;
     procedure LoadSatelliteImage;
     procedure LoadTowersOnMap(TheMap : tMapForm);

implementation

{$R *.dfm}

uses
   {$IfDef ExKML}
   {$Else}
      kml_creator,
   {$EndIf}
   Dragon_Plot_Init,
   DEM_Indexes,
   GetLatLn,
   DEMDef_routines,
   Make_Tables,
   toggle_db_use,
   DEM_Manager,
   KML_opts,
   US_Properties,
   DEMCoord,
   PetMath,
   PETdbUtils,
   DEM_PLSS,
   Get_PLSS,
   Petmar_db,
   Petmar_ini_file,
   DEMOptions,
   BaseMap,
   Sun_position,
   Nevadia_Main, map_overlays, //dem_browser,
   DataBaseCreate, DEMTiger;


function IERunningEx : Boolean;
var
   WinHandle : HWND;
   Name: array[0..255] of Char;
   EXEName : string;
begin
  Result := False; // assume no IE window is present
  WinHandle := GetTopWindow(GetDesktopWindow);

  while WinHandle <> 0 do begin// go thru the window list
      GetClassName(WinHandle, @Name[0], 255);
      ExeName := string(Name);
      WritelineToDebugFile(ExeName);
      if (CompareText(string(Name), 'IEFrame') = 0) then begin // IEFrame found
          Result := True;
          Exit;
      end;
      WinHandle := GetNextWindow(WinHandle, GW_HWNDNEXT);
  end;
end;


procedure OpenADP(ItsName : shortstring);
var
  npadhandle: HWnd;
begin
   IERunningEx;
   npadhandle := FindWindow(nil,pChar(ItsName));
   if npadhandle <> 0 then begin
      SetForegroundWindow(npadhandle);
      SendMessage(npadhandle, WM_SYSCOMMAND, SC_RESTORE, 0)
    end
   else Showmessage(ItsName + ' not found.');
end;



procedure LoadSatelliteImage;
begin
   {$IfDef ExSats}
   {$Else}
      {$IfDef RecordDP} WriteLineToDebugFile('LoadSatelliteImage, file=' + DragonPlotDef.DefaultDRGName); {$EndIf}
      OpenAndDisplaySatelliteScene(Nil,DragonPlotDef.DefaultDRGName,true,false,false);
      DragonPlotDef.DefaultDRGName := LastScanMapName;
      if (SatImage[1] <> Nil) then begin
         SatImage[1].SelectionMap.WindowState := wsMinimized;
         ShowDefaultCursor;
      end;
      {$IfDef RecordDP} WriteLineToDebugFile('LoadSatelliteImage OK ' + LastImageName); {$EndIf}
   {$EndIf}
end;


procedure LoadTowersOnMap(TheMap : tMapForm);
begin
   {$IfDef RecordDP} WriteLineToDebugFile('LoadTowersOnMap, file= ' + DragonPlotDef.TowerFileName); {$EndIf}
   if not FileExists(DragonPlotDef.TowerFileName) then DragonPlotDef.TowerFileName := '';
   if OpenNumberedGISDataBase(TowerTable,DragonPlotDef.TowerFileName,false,false,TheMap) then begin
      GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
      GISdb[TowerTable].dbOpts.LabelDBPlots := true;
      GISdb[TowerTable].KMLExportable := false;
      GISdb[TowerTable].DBOpts.Symbol.DrawingSymbol := DragonPlotDef.TowerSymbol.DrawingSymbol;
      GISdb[TowerTable].dbOpts.Symbol.Color := DragonPlotDef.TowerSymbol.Color;
      GISdb[TowerTable].dbOpts.Symbol.Size := DragonPlotDef.TowerSymbol.Size;
      GISdb[TowerTable].dbOpts.DBAutoShow := dbasDefault;
      GISdb[TowerTable].dbOpts.LabelField := 'NAME';
      GISdb[TowerTable].DoNotTrackDBSettings := true;
      Map_Overlays.AddOrSubtractOverlay(TheMap,ovoDatabases,True);
      GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
      DEMGlb[RegionalDEM].SelectionMap.MapDraw.CurrentFansTable := TowerTable;
      while (DEMGlb[RegionalDEM].SelectionMap.MapDraw.OverLayOrder[1] <> ovoDatabases) do MoveOverlayUp(DEMGlb[RegionalDEM].SelectionMap,ovoDatabases,true);
      DEMGlb[RegionalDEM].SelectionMap.DoFastMapRedraw;
   end;
end;


procedure LoadDEM;
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TPhotoRegForm.LoadDEM ' + DragonPlotDef.DefaultDEMName); {$EndIf}
    RegionalDEM := 0;

      if (MDdef.AutoOpen in [aoDEM,aoNothing]) then begin
         LoadNewDEM(RegionalDEM,DragonPlotDef.DefaultDEMName,true,'regional DEM','',true);
      end
      else if (MDdef.AutoOpen = aoProject) then begin
         RestoreSpecifiedDesktop(LastDesktop);
         RegionalDEM := 1;
      end;

      {$IfDef RecordOpenDEM} WriteLineToDebugFile('Regional DEM=' + LastDEMName); {$EndIf}

      if (NumDEMDataSetsOpen = 0) then begin
         {$IfDef RecordOpenDEM} WriteLineToDebugFile('Close because no DEM'); {$EndIf}
         exit;
      end
      else begin
         DEMGlb[RegionalDEM].SelectionMap.Closable := false;
         DEMGlb[RegionalDEM].SelectionMap.MapDraw.BaseTitle := 'Regional fire map, ' + DEMGlb[RegionalDEM].AreaName;
         DEMGlb[RegionalDEM].SelectionMap.Caption := DEMGlb[RegionalDEM].SelectionMap.MapDraw.BaseTitle;
         DEMGLB[RegionalDEM].SelectionMap.Left := 0;
         DEMGLB[RegionalDEM].SelectionMap.Top := 0;
         {$IfDef RecordOpenDEM} WriteLineToDebugFile('DEM loaded'); {$EndIf}
      end;

   {$IfDef ExPLSS}
   {$Else}
      if MDDef.PLSSDef.AutoDrawPLSS then begin
         TryToOpenPLSS(DEMGlb[RegionalDEM].SelectionMap);
      end;
   {$EndIf}

   LoadTowersOnMap(DEMGlb[RegionalDEM].SelectionMap);

   Forms.Screen.Cursor := crDefault;
   RegionalDEM := RegionalDEM;
   {$IfDef RecordDP} WriteLineToDebugFile('TPhotoRegForm.LoadDEM out'); {$EndIf}
end;


procedure TDragonPlotForm.SetTabSheets;
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.SetTabSheets in, advanced=' + TrueOrFalse(DragonPlotDef.AdvancedOptions)); {$EndIf}
   TabSheet2.TabVisible := DragonPlotDef.AdvancedOptions;
   TabSheet3.TabVisible := DragonPlotDef.AdvancedOptions;
   RestoreConfigurationButton.Visible := DragonPlotDef.AdvancedOptions;
   AdminOptionsButton.Visible := (MDDef.ProgramOption = ExpertProgram) or DragonPlotDef.AdvancedOptions;
   CheckBox4.Checked := DragonPlotDef.AdvancedOptions;
   InstancesToLoad1.Visible := DragonPlotDef.AdvancedOptions;

  //lightning tab
    BitBtn8.Enabled := LightningTable <> 0;
    BitBtn34.Enabled := LightningTable <> 0;
//lightning options, not working now with the current lightning files
   BitBtn18.Visible := false;   //GroupBox4.Visible;
   BitBtn31.Visible := false;   //GroupBox4.Visible;
   BitBtn15.Visible := false;   //GroupBox4.Visible;
   BitBtn21.Visible := false;   //GroupBox4.Visible;
   BitBtn32.Visible := false;   //GroupBox4.Visible;
   BitBtn33.Visible := false;   //GroupBox4.Visible;

   //disabled options, but open for a full GIS use case
   GroupBox4.Visible := TrilobiteComputer or (MDDef.ProgramOption = ExpertProgram) or DragonPlotDef.AdvancedOptions;
   ChangeRegionalDEM1.Visible := GroupBox4.Visible;
   TabSheet6.TabVisible := (MDDef.ProgramOption = ExpertProgram) and DragonPlotDef.AdvancedOptions;
   if not TabSheet6.TabVisible then begin
      DragonPlotDef.FireAccessMap := false;
      CheckBox99.Checked := false;
   end;

   BitBtn20.Visible := (MDDef.ProgramOption = ExpertProgram) and DragonPlotDef.AdvancedOptions;

   Button1.Visible := GroupBox4.Visible;
   BitBtn4.Visible := GroupBox4.Visible;
   BitBtn7.Visible := GroupBox4.Visible;

   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.SetTabSheets out'); {$EndIf}
end;


procedure TDragonPlotForm.SetUpInstance(i : integer);

         Procedure SetPanel(Panel : tPanel);
         begin
            Panel.StyleElements := Panel.StyleElements - [seClient];
            if i=1 then Panel.Color := clBtnFace
            else if i=2 then Panel.Color := clMoneyGreen
            else if i=3 then Panel.Color := clSkyBlue
            else if i=4 then Panel.Color := clGradientInactiveCaption;
         end;

begin
   DragonPlotInstance := i;
   Left := WMDEM.ClientWidth - Self.Width - 5;
   Top := 0;
   PrimaryTowerComboBox7.Text := DragonPlotDef.DefaultTowerName[i];
   PrimaryTowerComboBox7Change(nil);
   SecondaryTowerComboBox2.Text := '';
   BitBtn12.Enabled := (PLSS[1] <> Nil);
   BitBtn2.Enabled := true;
   SetPanel(Panel1);
   SetPanel(Panel3);
   if MDDef.ProgramOption = DragonPlotProgram then InstanceName := ExtractFileNameNoExt(Application.EXEName)
   else InstanceName := 'DP' + IntToStr(i);
   LoadTowersIntoComboBoxes;
   Caption := InstanceName + ' Control Window';
end;


procedure TDragonPlotForm.DrawSightingsOnMap(Found : boolean; var UseMap : tMapForm);
var
   x,y : integer;
   Bitmap : tBitmap;
begin
   {$IfDef RecordDrawSighting} WriteLineToDebugFile('TDragonPlotForm.DrawSightingsOnMap in, ' + UseMap.MapDraw.MapSizeString); {$EndIf}
   CopyImageToBitmap(UseMap.Image1,Bitmap);
   if Found then begin
      UseMap.MapDraw.LatLongDegreeToScreen(ShotLat,ShotLong,x,y);
      ScreenSymbol(Bitmap.Canvas,x,y,DragonPlotDef.ResultSymbol);
      PetImage.PlotOrientedLine(Bitmap,x,y,200,Azimuth,DragonPlotDef.ShotLineColor,DragonPlotDef.ShotLineWidth,true);
      if SecondSighting then begin
        PetImage.PlotOrientedLine(Bitmap,x,y,200,Azimuth2,DragonPlotDef.ShotLineColor,DragonPlotDef.ShotLineWidth,true);
      end;
   end
   else begin
      UseMap.MapDraw.LatLongDegreeToScreen(TowerLat,TowerLong,x,y);
      PetImage.PlotOrientedLine(Bitmap,x,y,400,Azimuth,DragonPlotDef.ShotLineColor,DragonPlotDef.ShotLineWidth,true);
      if SecondSighting then begin
        UseMap.MapDraw.LatLongDegreeToScreen(TowerLat2,TowerLong2,x,y);
        PetImage.PlotOrientedLine(Bitmap,x,y,400,Azimuth2,DragonPlotDef.ShotLineColor,DragonPlotDef.ShotLineWidth,true);
      end;
   end;
   UseMap.Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   {$IfDef RecordDrawSighting} WriteLineToDebugFile('TDragonPlotForm.DrawSightingsOnMap out'); {$EndIf}
end;


procedure DefineTowerDatum(TowerLong : float64);
begin
   if (DEMGlb[1] <> Nil) then begin
      DEMGlb[1].DEMMapProj.DefineDatumFromUTMZone(MDdef.PreferPrimaryDatum,GetUTMZone(TowerLong),'N');
      DEMGlb[1].DEMMapProj.DefineDatumFromUTMZone(MDdef.PreferSecondaryDatum,GetUTMZone(TowerLong),'N');
   end;
end;


procedure TDragonPlotForm.ChangeRegionalDEM1Click(Sender: TObject);
begin
   DragonPlotDef.DefaultDEMName := '';
   LastDEMName := '';
   CloseSingleDEM(RegionalDEM);
   LoadDEM;
end;


procedure TDragonPlotForm.Changeimagemap1Click(Sender: TObject);
var
   i : integer;
begin
   if SatImage[1] <> Nil then begin
      LastImageName := '';
      i := 1;
      CloseSingleSatelliteImage(i);
   end;
  DragonPlotDef.DefaultDRGName := '';
  LoadSatelliteImage;
end;


procedure TDragonPlotForm.ChangeTowers1Click(Sender: TObject);
var
   i : integer;
begin
   if Petmar.GetFileFromDirectory('Towers','*.dbf',DragonPlotDef.TowerFileName) then begin
      i := 1;
      LoadTowersOnMap(DEMGlb[RegionalDEM].SelectionMap);
   end;
end;

procedure TDragonPlotForm.CheckAccessibilityMap;
begin
   if DragonPlotDef.FireAccessMap and DEMGlb[1].LatLongDegreeInDEM(ShotLat,ShotLong) then begin
      {$If Defined(CheckAccessibility) or Defined(RecordCreateBlowUpMap)} WriteLineToDebugFile('TDragonPlotForm.CheckAccessibilityMap in'); {$EndIf}
      AccessMap := DEMGlb[1].SelectionMap.SiteContourMapBlowUp(ShotLat,ShotLong,DragonPlotDef.FireAccessMapSize,DragonPlotDef.FireAccessMapBlowUp,MDDef.DefaultContourInterval,'Fire location accessibility');
      AccessMap.DoFastMapRedraw;
      AccessMap.Closable := false;
      DrawSightingsOnMap(true,AccessMap);
      {$If Defined(CheckAccessibility) or Defined(RecordCreateBlowUpMap)} WriteLineToDebugFile('TDragonPlotForm.CheckAccessibilityMap out'); {$EndIf}
   end;
end;


procedure TDragonPlotForm.DisplayMessage(Words : shortString);
begin
   if (Words <> '') then LastSighting.Add(Words);
   Memo1.Lines.Add(Words);
end;


procedure TDragonPlotForm.DP11Click(Sender: TObject);
begin
   OpenADP('DP-1.exe');
end;

procedure TDragonPlotForm.DP21Click(Sender: TObject);
begin
   OpenADP('DP-2.exe');
end;

procedure TDragonPlotForm.DP31Click(Sender: TObject);
begin
   OpenADP('DP-3');
end;

procedure TDragonPlotForm.DP41Click(Sender: TObject);
begin
   OpenADP('DP-4');
end;

procedure TDragonPlotForm.DP51Click(Sender: TObject);
begin
   OpenADP('DP-5');
end;

procedure TDragonPlotForm.DP61Click(Sender: TObject);
begin
   OpenADP('DP-6');
end;


procedure TDragonPlotForm.ShowOnMaps(FoundPoint : boolean = true);
begin
  {$IfDef RecordCreateBlowUpMap} WriteLineToDebugFile('TDragonPlotForm.ShowOnMaps in, TargetType=' + IntToStr(DragonPlotDef.TargetMapType )); {$EndIf}
   DrawSightingsOnMap(FoundPoint,DEMGlb[1].SelectionMap);

   if FoundPoint then begin
      if (DragonPlotDef.TargetMapType = 1) then begin
        {$IfDef RecordCreateBlowUpMap} WriteLineToDebugFile('Try image map'); {$EndIf}
         if (SatImage[1].LatLongDegreeInDataSet(shotLat-MDDef.BlowUpLatSize,ShotLong-MDDef.BlowUpLatSize)) and (SatImage[1].LatLongDegreeInDataSet(ShotLat+MDDef.BlowUpLatSize,ShotLong+MDDef.BlowUpLatSize)) then begin
            {$IfDef RecordCreateBlowUpMap} WriteLineToDebugFile('All on image map'); {$EndIf}
            TargetMap := SatImage[1].SelectionMap;
         end
         else begin
            {$IfDef RecordCreateBlowUpMap} WriteLineToDebugFile('Off image map, use DEM map'); {$EndIf}
            SatImage[1].SelectionMap.WindowState := wsMinimized;
            TargetMap := CreateANewDEMMapWindow(1,false,MDDef.DefDEMMap,'');
         end;
      end;
      DrawTargetMap(TargetMap);
      CheckAccessibilityMap;
   end;
  {$IfDef RecordCreateBlowUpMap} WriteLineToDebugFile('TDragonPlotForm.ShowOnMaps out'); {$EndIf}
end;


procedure TDragonPlotForm.AddGazetteerToTargetMap;
begin
   if (LastGazFile <> '') then begin
      {$IfDef RecordGazetteer} WriteLineToDebugFile('TDragonPlotForm.AddGazetteerToTargetMap'); {$EndIf}
      if FileExists(LastGazFile) then begin
         AddOrSubtractOverlay(TargetMap,ovoGazetteer,true);
      end
      else begin
        {$IfDef RecordGazetteer} WriteLineToDebugFile('Problem loading gazetteer ' +  LastGazFile); {$EndIf}
      end;
   end;
end;

procedure TDragonPlotForm.Instancestoload1Click(Sender: TObject);
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.Instancestoload1Click in'); {$EndIf}
   ReadDefault('Instances to load', DragonPlotDef.DPInstances);
   if (DragonPlotDef.DPInstances > MaxInstances) then DragonPlotDef.DPInstances := MaxInstances;
   if (DragonPlotDef.DPInstances < 1) then DragonPlotDef.DPInstances := 1;
   LoadTheInstances;
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.Instancestoload1Click out'); {$EndIf}
end;


procedure TDragonPlotForm.InsureLocationMapOK;
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.InsureLocationMapOK in'); {$EndIf}
   if (TargetMap = Nil) then begin
      if (DragonPlotDef.TargetMapType = 0) then begin
         {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.InsureLocationMapOK (DragonPlotDef.TargetMap = 0, DEM)'); {$EndIf}
         TargetMap := CreateANewDEMMapWindow(1,false,MDDef.DefDEMMap,'Target map');
         AddGazetteerToTargetMap;
         TargetMap.Closable := false;
      end;
      if (DragonPlotDef.TargetMapType = 1) then begin
         {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.InsureLocationMapOK (DragonPlotDef.TargetMap = 1, sat image)'); {$EndIf}
         if (SatImage[1] = Nil) then LoadSatelliteImage;
         if (SatImage[1] = Nil) then DragonPlotDef.TargetMapType := 0
         else TargetMap := SatImage[1].SelectionMap;
      end;
   end;
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.InsureLocationMapOK out'); {$EndIf}
end;


procedure TDragonPlotForm.KMLdelaysec1Click(Sender: TObject);
begin
   ReadDefault('KML delay (sec)',DragonPlotDef.KMLDelay);
end;


function TDragonPlotForm.GetTowerSettings(TowerName : shortString; var Lat,Long,Height,GroundElevation,CameraElev,Range : float64) : boolean;
var
   z : float32;
begin
   if ValidDB(TowerTable) then begin
      GISdb[TowerTable].MyData.ApplyFilter('NAME=' + QuotedStr(Trim(TowerName)));
      if GISdb[TowerTable].MyData.FiltRecsInDB = 0 then begin
         {$IfDef RecordDP} WriteLineToDebugFile('Tower missing ' + TowerName); {$EndIf}
         Result := false;
      end
      else begin
         Lat := GISdb[TowerTable].MyData.GetFieldByNameAsFloat('LAT');
         Long := GISdb[TowerTable].MyData.GetFieldByNameAsFloat('LONG');
         Height := GISdb[TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_UP');
         Range := GISdb[TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
         GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
         Result := DEMGlb[RegionalDEM].GetElevFromLatLongDegree(Lat,Long,z);;
         GroundElevation := z;
         if Result then CameraElev := GroundElevation + Height
         else begin
            Memo1.Lines.Add('Tower: ' + TowerName + ' not on DEM');
            CameraElev := 0;
         end;
         DefineTowerDatum(TowerLong);
      end;
      GISdb[TowerTable].ClearGISFilter;
   end;
end;


procedure TDragonPlotForm.BitBtn2Click(Sender: TObject);
begin
   ProcessShot(smSingle);
end;


procedure TDragonPlotForm.PrepPLSS1Click(Sender: TObject);
begin
   PrepPLSS;
end;

procedure TDragonPlotForm.PrimaryTowerComboBox7Change(Sender: TObject);
begin
   if not GetTowerSettings(PrimaryTowerComboBox7.Text, TowerLat,TowerLong,TowerHeight,TowerGroundElevation,CameraElev,TowerRange) then begin
      MessageToContinue('Tower ' + PrimaryTowerComboBox7.Text + ' missing or not located within DEM');
   end;
   if (LightningTable <> 0) then GISdb[LightningTable].IntervisibilityFromPoint(false,TowerLat,TowerLong,CameraElev,TowerHeight);
end;


procedure TDragonPlotForm.ProcessShot;
var
   BlockLat,BlockLong,DistOut,BlockAngle,BlockDist : float64;
   TStr : shortstring;
   AzTrue,AzUTM,MapAz,Dist,BearingAngle,TargetZ,ReportedDistance,{z,}Pitch1,Pitch2  : float64;
   GeoName,PointMess1,PLSSMess2 : ShortString;
   FoundIntersection : boolean;
   Sym1 : tFullSymbolDeclaration;
   //Bitmap : Graphics.tBitmap;

      procedure AccuracyCheck(Map : tMapForm; Azimuth,Depress : float64; NeedPLSS : boolean);
      var
         DistOut,Lat,Long : float64;
      begin
          DEMPersF.LocatePointOnPerspective(Azimuth,Depress,Lat,Long,DistOut,PointMess1,PLSSMess2,NeedPLSS);
          Map.MapDraw.MapSymbolAtLatLongDegree(Map.Image1.Canvas,Lat,Long,DragonPlotDef.AccuracySymbol);
      end;


      procedure PostResult(Title,Note : shortString);
      var
         z : float32;
      begin
         {$IfDef RecordPostResults} WriteLineToDebugFile('Post results 1'); {$EndIf}
         DEMglb[RegionalDEM].SelectionMap.MapDraw.MapSymbolAtLatLongDegree(DEMglb[RegionalDEM].SelectionMap.Image1.Canvas,ShotLat,ShotLong,DragonPlotDef.ResultSymbol);
         DEMGlb[RegionalDEM].GetElevFromLatLongDegree(ShotLat,ShotLong,z);
         TargetZ := z;

         DisplayMessage({'  ' +} title + '  ' + DEMglb[RegionalDEM].SelectionMap.MapDraw.PrimMapProj.h_DatumCode + '   ' + LatLongDegreeToString(ShotLat,ShotLong,MDDef.OutPutLatLongMethod) + '   ' + note );
         DisplayMessage('Elev: ' + RealToString(TargetZ,-8,1) + ' m  (' + RealToString(TargetZ/FeetToMeters,-8,1) + ' ft)');

         if US_properties.GetUSGSQuadName(ShotLat,ShotLong,GeoName) then Memo1.Lines.Add('USGS 24K quad: ' + GeoName);
         {$IfDef RecordPostResults} WriteLineToDebugFile('Post results 3'); {$EndIf}

         if (PLSS[1] <> Nil) then begin
            {$IfDef RecordPostResults} WriteLineToDebugFile('Post results 3.5--getting PLSS'); {$EndIf}
            DisplayMessage('PLSS location: ' + PLSSLocation(ShotLat,ShotLong));
         end;
         {$IfDef RecordPostResults} WriteLineToDebugFile('Post results out'); {$EndIf}
      end;



      procedure FigureView(var PersF : TThreeDview; Azimuth,Pitch : float64; Number : integer; TowerName : string35; TowersLat,TowersLong,TowerGroundElev,CamerasElevAboveGround : float64; Vis : boolean = false);

            procedure SetUpPerspectiveForm(Title : ShortString; DoDrape : boolean);
            begin
              {$IfDef RecordDP} WriteLineToDebugFile('SetUpPerspectiveForm in'); {$EndIf}
              {$IfDef RecordPerspective}
                  WriteLineToDebugFile('SetUpPerspectiveForm, ' + LatLongDegreeToString(TowersLat,TowersLong) + '  Tower Ground Elev: ' + RealToString(TowerGroundElev,-12,1));
                  WriteLineToDebugFile('Azimuth=' + RealToString(Azimuth,-12,1) + '  Pitch=' + RealToString(Pitch,-12,1) + ' HFOV=' + IntToStr(DragonPlotDef.CustomHFOV) + '  VFOV=' + IntToStr(DragonPlotDef.CustomVFOV));
                  WriteLineToDebugFile('Title: ' + Title);
               {$EndIf}
               SetUpPanoramaView(PersF,TowersLat,TowersLong,CamerasElevAboveGround,Depth,Azimuth,DragonPlotDef.CustomHFOV,DragonPlotDef.CustomVFOV,Pitch,RegionalDEM,
                              'View from ' + Title,DoDrape,SeekingSecondPerspective,DEMglb[RegionalDEM].SelectionMap);
               PersF.Closable := false;
               PersF.Top := (pred(Number) * (PersF.Height + 5));
               PersF.Left := Screen.Width - 10 - Self.Width - PersF.Width;
               {$IfDef RecordDP} WriteLineToDebugFile('SetUpPerspectiveForm out'); {$EndIf}
           end;


      begin
        {$If Defined(RecordDP) or Defined(RecordFigureView)} WriteLineToDebugFile('FigureView in, shot=' + LatLongDegreeToString(TowerLat,TowerLong)); {$EndIf}
         DefineTowerDatum(TowerLong);
         SetUpPerspectiveForm(TowerName + ' ' + LastShotTime,(MDDef.PerspOpts.WhichPerspective = BMPPerspective));
         if (ShotMode = smSingle) then begin
            PersF.LocatePointOnPerspective(Azimuth,Depress,ShotLat,ShotLong,DistOut,PointMess1,PLSSMess2);
            Vis := true;
         end;
         if (ShotMode = smCross) then  begin
            Vis := DEMGlb[RegionalDEM].LatLongDegreePointsIntervisible(TowersLat,TowersLong,CamerasElevAboveGround,ShotLat,ShotLong,0,Dist,BlockDist);
         end;

         {$IfDef RecordDP} WriteLineToDebugFile('Tower Vis=' + BoolToStr(Vis) + ' shot=' + LatLongDegreeToString(ShotLat,ShotLong)); {$EndIf}
         if Vis then begin
            TStr := '(visible)';
            Sym1 := DragonPlotDef.VisibleSymbol;
            PersF.Caption := InstanceName + ': View from ' + TowerName;
         end
         else begin
            TStr := '(masked)';
            Sym1 := DragonPlotDef.MaskedSymbol;
            PersF.Caption := InstanceName + ': Masked view from ' + TowerName;
         end;
         {$If Defined(RecordFigureView)} WriteLineToDebugFile('Figure view out, TStr=' + TStr); {$EndIf}
         PersF.SymbolAtPitchAzimith(Pitch,Azimuth,Sym1,true);

         DisplayMessage('Tower ' + IntToStr(Number) + ': ' + TowerName);
         DisplayMessage('Input Azimuth (true)=' + RealToString(Azimuth,-8,2) + '°   ' + ConvertToDegreesString(Azimuth,DecMinutes));
         VincentyCalculateDistanceBearing(TowersLat,TowersLong,ShotLat,ShotLong,BlockDist,BearingAngle);
         DisplayMessage('Calculated distance =' +  SmartDistanceMetersFormat(BlockDist));
         if (ShotMode = smCross) then DisplayMessage('Pitch to target: ' + RealToString(Pitch,-8,2)+ '°   ' + ConvertToDegreesString(Pitch,DecMinutes) + '   ' + Tstr);
        {$If Defined(RecordFigureView) or Defined(RecordDP)} WriteLineToDebugFile('Figure view out, shot=' + LatLongDegreeToString(ShotLat,ShotLong)); {$EndIf}
      end;

      procedure DoCrossShot;
      var
         z : float32;
      begin
         {$IfDef RecordDP} WriteLineToDebugFile('Cross shot underway'); {$EndIf}
         if GetTowerSettings(SecondaryTowerComboBox2.Text, TowerLat2,TowerLong2,TowerHeight2,TowerGroundElevation2,CameraElev2,TowerRange2) then begin
            FoundIntersection := DEMglb[RegionalDEM].SelectionMap.Intersection(TowerLat,TowerLong,Azimuth,TowerLat2,TowerLong2,Azimuth2,ShotLat,ShotLong);
            {$IfDef RecordDP} if FoundIntersection then WriteLineToDebugFile('Intersection = ' + LatLongDegreeToString(ShotLat,ShotLong)); {$EndIf}
            SecondSighting := true;
            if (FoundIntersection) then InsureLocationMapOK;
            ShowOnMaps(FoundIntersection);
            if (FoundIntersection) then begin
               TargetMap.MapDraw.MapSymbolAtLatLongDegree(DEMglb[RegionalDEM].SelectionMap.Image1.Canvas,TowerLat,TowerLong,DragonPlotDef.TowerSymbol);
               TargetMap.MapDraw.MapSymbolAtLatLongDegree(DEMglb[RegionalDEM].SelectionMap.Image1.Canvas,TowerLat2,TowerLong2,DragonPlotDef.TowerSymbol);
               TargetMap.MapDraw.BaseTitle := InstanceName + ': Intersection (' + LastShotTime + ') from ' + PrimaryTowerComboBox7.Text + ' and ' + SecondaryTowerComboBox2.Text;
               TargetMap.Caption := TargetMap.MapDraw.BaseTitle;
               DEMglb[RegionalDEM].GetElevFromLatLongDegree(ShotLat,ShotLong,z);
               TargetZ := z;
               {$IfDef RecordDP} WriteLineToDebugFile('Target z=' + RealToString(TargetZ,-8,1)); {$EndIf}
               PointMess1 := '';
               if not DEMglb[RegionalDEM].SelectionMap.MapDraw.MapSymbolAtLatLongDegree(DEMglb[RegionalDEM].SelectionMap.Image1.Canvas,ShotLat,ShotLong,Box,3,claRed) then PointMess1 := '  (off map)';
               VincentyCalculateDistanceBearing(TowerLat,TowerLong,ShotLat,ShotLong,ReportedDistance,Azimuth);
               Pitch1 := ArcTan((Targetz - CameraElev - DropEarthCurve(ReportedDistance)) / ReportedDistance) / DegToRad;
               FigureView(DEMPersF,Azimuth,Pitch1,1,PrimaryTowerComboBox7.Text,TowerLat,TowerLong,TowerGroundElevation,TowerHeight);

               VincentyCalculateDistanceBearing(TowerLat2,TowerLong2,ShotLat,ShotLong,ReportedDistance,Azimuth2);
               Pitch2 := ArcTan((Targetz - CameraElev2 - DropEarthCurve(ReportedDistance)) / ReportedDistance) / DegToRad;
               FigureView(DEMPersF2,Azimuth2,Pitch2,2,SecondaryTowerComboBox2.Text,TowerLat2,TowerLong2,TowerGroundElevation2,TowerHeight2);

               if (FoundIntersection) then begin
                  Memo1.Lines.Add('');
                  PostResult('Intersection: ',PointMess1);
                  Memo1.Lines.Add('');
                  if DragonPlotDef.ShowLOS then begin
                      MDDef.LOSShowPitch := true;
                      MDDef.LOSMinPitch := Pitch1;
                      MDDef.LOSMaxPitch := Pitch1;
                      CreateProfile(DEMLOSF,TowerLat,TowerLong,ShotLat,ShotLong,TowerHeight,PrimaryTowerComboBox7.Text);
                      MDDef.LOSShowPitch := true;
                      MDDef.LOSMinPitch := Pitch2;
                      MDDef.LOSMaxPitch := Pitch2;
                      CreateProfile(DEMLOSF2,TowerLat2,TowerLong2,ShotLat,ShotLong,TowerHeight2,SecondaryTowerComboBox2.Text);
                  end;
               end;
            end
            else begin
               DisplayMessage('Primar Tower: ' + PrimaryTowerComboBox7.Text);
               DisplayMessage(' Input Azimuth (true)=' + ConvertToDegreesString(Azimuth,DecDegrees,false) + '   ' + ConvertToDegreesString(Azimuth,DecMinutes,false));
               DisplayMessage('Cross Tower: ' + SecondaryTowerComboBox2.Text);
               DisplayMessage(' Input Azimuth (true)=' + ConvertToDegreesString(Azimuth2,DecDegrees,false) + '   ' + ConvertToDegreesString(Azimuth2,DecMinutes,false));
               DisplayMessage('');
               ShotLat := 999;
               ShotLong := 999;
               DisplayMessage('No intersection within ' + SmartDistanceMetersFormat(250000));
               GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
            end;
            GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
        end;
      end;

      procedure DoSingleShot;
      var
         i,j : integer;
         FoundPoint : boolean;
      begin
         {$IfDef RecordDP}
            WriteLineToDebugFile('Single shot ' + PrimaryTowerComboBox7.Text + '    ' + LatLongDegreeToString(TowerLat,TowerLong,MDDef.OutPutLatLongMethod) + '  Camera z: ' + RealToString(CameraElev,8,2));
            WriteLineToDebugFile('Input Azimuth (true)=' + RealToString(Azimuth,-8,2) + '°   Pitch=' + RealToString(Depress,-8,2)+ '°');
         {$EndIf}
         FigureView(DEMPersF,Azimuth,Depress,1,PrimaryTowerComboBox7.Text,TowerLat,TowerLong,TowerGroundElevation,TowerHeight);

         DisplayMessage('Pitch=' + RealToString(Depress,-8,2)+ '°   ' + ConvertToDegreesString(Depress,DecMinutes));
         FoundPoint := (PointMess1 <> '');
         if FoundPoint then begin
            InsureLocationMapOK;
            PostResult('Target=','');
            DisplayMessage('');
         end
         else begin
            {$IfDef RecordFigureView} WriteLineToDebugFile('Off to HorizonBlocking'); {$EndIf}
            DEMGlb[RegionalDEM].HorizonBlocking(TowerLat,TowerLong,AzTrue,Depth,TowerHeight,BlockAngle,BlockDist,BlockLat,BlockLong,MDDef.wf.StraightAlgorithm);
            DisplayMessage('');
            DisplayMessage('Shot does not intersect ground in DEM');
            DisplayMessage('Horizon pitch=' + RealToString(BlockAngle,-8,2)+ '°   ' + ConvertToDegreesString(BlockAngle,DecMinutes));
            DisplayMessage('Horizon at ' + LatLongDegreeToString(BlockLat,BlockLong,MDDef.OutPutLatLongMethod));
         end;
         ShowOnMaps(FoundPoint);
         {$IfDef RecordDP} WriteLineToDebugFile('Done ShowOnMaps'); {$EndIf}

         if FoundPoint then begin
            if DragonPlotDef.ShowLOS then begin
               MDDef.LOSShowPitch := true;
               MDDef.LOSMinPitch := Depress;
               MDDef.LOSMaxPitch := Depress;
               CreateProfile(DEMLOSF,TowerLat,TowerLong,ShotLat,ShotLong,TowerHeight,PrimaryTowerComboBox7.Text);
            end;

            if DragonPlotDef.PlotAccuracy and (TargetMap <> Nil) then begin
               {$IfDef RecordDP} WriteLineToDebugFile('DragonPlotDef.PlotAccuracy start'); {$EndIf}
               if DragonPlotDef.AzAccuracy < 0.0001 then begin
                  AccuracyCheck(TargetMap,Azimuth,Depress+DragonPlotDef.PitchAccuracy,false);
                  AccuracyCheck(TargetMap,Azimuth,Depress-DragonPlotDef.PitchAccuracy,false);
               end
               else begin
                  for i := -1 to 1 do
                     for j := -1 to 1 do
                        if (i <> 0) or (j <> 0) then
                           AccuracyCheck(TargetMap,Azimuth+i*DragonPlotDef.AzAccuracy,Depress+j*DragonPlotDef.PitchAccuracy,false);
               end;
               {$IfDef RecordDP} WriteLineToDebugFile('DragonPlotDef.PlotAccuracy end'); {$EndIf}
            end;
         end;
      end;


begin
   {$IfDef RecordDP} WriteLineToDebugFile('*******TDragonPlotForm.BitBtn2Click----Draw view selected'); {$EndIf}
   {$If Defined(CheckAccessibility)} WriteLineToDebugFile('DragonPlotDef.FireAccessMap=' + TrueOrFalse(DragonPlotDef.FireAccessMap)); {$EndIf}
   CloseWindows;
   SecondSighting := false;
   SetLastShotTime;
   if (ShotMode = smCross) then begin
      if SecondaryTowerComboBox2.Text = PrimaryTowerComboBox7.Text then begin
         DisplayMessage('Cross tower must be different than primary tower');
         DisplayMessage('Fix error and retry.');
         SecondaryTowerComboBox2.Text := '';
         exit;
      end;
      Edit3.Text := Edit16.Text;
      Edit1.Text := Edit17.Text;
      DisplayMessage('Cross shot done at ' + LastShotTime );
   end
   else if (ShotMode = smSingle) then begin
      SecondaryTowerComboBox2.Text := '';
      DisplayMessage('Single sighting done at ' + LastShotTime );
   end;

   if (not CheckCameraParameters) then begin
      DisplayMessage('');
      DisplayMessage('Input entry error, cannot proceed.');
      DisplayMessage('Fix error and retry.');
      exit;
   end;

   {$IfDef RecordDP} WriteLineToDebugFile('Tower ' + GISdb[TowerTable].MyData.Filter + ' Picked first tower of ' + IntToStr(GISdb[TowerTable].MyData.RecordCount)); {$EndIf}

   Depth := GISdb[TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
   Edit11.Text := RealToString(Depth,-18,0);
   AzTrue := Azimuth;
   AzUTM := Azimuth-DEMglb[RegionalDEM].SelectionMap.MapDraw.GridTrueAngle;
   MapAz := AzTrue;

   DEMglb[RegionalDEM].SelectionMap.MapDraw.GridTrueAngle := DEMglb[RegionalDEM].SelectionMap.MapDraw.UTMGridToTrueNorthAngle(TowerLat,TowerLong);

   if (ShotMode = smCross) then begin
      DoCrossShot;
   end
   else if (ShotMode = smSingle) then begin
      DoSingleShot;
   end;
   Pitch2 := -99;
   BitBtn11.Enabled := (TargetMap <> Nil) or (SatImage[1] <> Nil);
   BitBtn36.Enabled := (TargetMap <> Nil) or (SatImage[1] <> Nil);
   GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   Forms.Screen.Cursor := crDefault;
   EndShot;
   AssignMapOwnerToPLSS(DEMglb[RegionalDEM].SelectionMap);
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn2Click out'); {$EndIf}
end;



procedure TDragonPlotForm.CloseWindows;

   procedure CloseSinglePerspective(var PersF : TThreeDview);
   begin
      if (PersF <> Nil) then begin
         PersF.Closable := true;
         PersF.Close;
         PersF := Nil;
      end;
   end;

   procedure CloseSingleLOS(var LOSF : TDEMLOSF);
   begin
      if (LOSF <> Nil) then begin
         LOSF.Closable := true;
         LOSF.Close;
         LOSF := Nil;
      end;
   end;

begin
   {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.CloseWindows in'); {$EndIf}
   ArchiveShot;
   LastSighting.Clear;

   if not WMDEM.ProgramClosing then begin
      Memo1.Clear;
      Memo1.Scrollbars := ssNone;
      BitBtn12.Enabled := (PLSS[1] <> Nil);
   end;

   CloseSinglePerspective(DEMPersF);
   CloseSinglePerspective(DEMPersF2);
   CloseSinglePerspective(DEMPersPan);
   {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.CloseWindows perspectives closed'); {$EndIf}

   CloseSingleLOS(DEMLOSF);
   CloseSingleLOS(DEMLOSF2);
   {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.CloseWindows LOS closed'); {$EndIf}

   if (TargetMap <> Nil) then begin
      TargetMap.ShowKeyLocation := false;
      TargetMap.Closable := true;
      TargetMap.Close;
      TargetMap := Nil;
      {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.CloseWindows target map closed'); {$EndIf}
   end;

   if (AccessMap <> Nil) then begin
      AccessMap.Closable := true;
      AccessMap.Close;
      AccessMap := Nil;
      {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.CloseWindows access map closed'); {$EndIf}
   end;

   {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.CloseWindows out'); {$EndIf}
end;


procedure TDragonPlotForm.ArchiveShot;
//var
   //TStr : ShortString;
   //i : integer;
begin
   if (Memo1.Lines.Count > 0) and ValidDB(TowerTable) then begin
       {$IfDef RecordDP} WriteLineToDebugFile('ArchiveShot in, MainMapData=' + MainMapData); {$EndIf}
       //TStr := DateToStr(now) + '--' + CurrentMilitaryTime(true);
       //for i := 1 to length(TStr) do if TStr[i] in ['/',':'] then TStr[i] := '-';
       Memo1.Lines.SaveToFile(ArchiveDirectory + CurrentTimeForFileName + '.txt');
       //GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
       {$IfDef RecordDP} WriteLineToDebugFile('ArchiveShot out'); {$EndIf}
   end;
end;


procedure TDragonPlotForm.DrawTargetMap(var UseMap : tMapForm);
begin
   {$IfDef RecordCreateBlowUpMap} WriteLineToDebugFile('TDragonPlotForm.DrawTargetMap, ' + UseMap.Caption);{$EndIf}
   UseMap.MapDraw.MaximizeLatLongMapCoverage(ShotLat-MDDef.BlowUpLatSize,ShotLong-MDDef.BlowUpLongSize,ShotLat+MDDef.BlowUpLatSize,ShotLong+MDDef.BlowupLongSize,DragonPlotDef.TargetMapWidth,DragonPlotDef.TargetMapHeight);

   UseMap.ShowKeyLocation := true;
   UseMap.KeyLocationLat := ShotLat;
   UseMap.KeyLocationLong := ShotLong;
   UseMap.DoCompleteMapRedraw;
   UseMap.Closable := false;
   UseMap.MapDraw.BaseTitle := 'DP'+ IntToStr(DragonPlotInstance) + ': Estimated fire (' + LastShotTime + '):' + LatLongDegreeToString(ShotLat,ShotLong,MDDef.OutPutLatLongMethod);
   UseMap.Caption := UseMap.MapDraw.BaseTitle + ' ' + LastShotTime;
   LastSighting.Clear;
   LastSighting.Add('LAT,LONG,NAME,TEXT,IMAGE');
   LastSighting.Add(RealToString(UseMap.KeyLocationLat,-12,8) + ',' + RealToString(UseMap.KeyLocationLong,-12,8) + ',' + 'Dragon Plot Fix' + ',' + 'sighting.txt,View_of_shot.png');
   LastSighting.SaveToFile(MDTempDir + 'sighting.csv');
   UseMap.MapDraw.MapXsize := DragonPlotDef.TargetMapWidth;
   UseMap.MapDraw.MapYsize := DragonPlotDef.TargetMapHeight;

   DrawSightingsOnMap(True,UseMap);
   UseMap.NoScrollBars;
   UseMap.Left := 0;
   UseMap.Top := TopWindow2;
   UseMap.BringToFront;
   UseMap.WindowState := wsNormal;
   {$IfDef RecordCreateBlowUpMap} WriteLineToDebugFile('TDragonPlotForm.DrawTargetMap out, ' + TargetMap.MapDraw.MapSizeString); {$EndIf}
end;


procedure TDragonPlotForm.FormCreate(Sender: TObject);
//var
   //i  : integer;
begin
   SettingUp := true;
   FirstKML := true;
   SetTabSheets;

   DEMPersF := Nil;
   DEMPersF2 := Nil;
   DEMPersPan := Nil;
   DEMLOSF := Nil;
   DEMLOSF2 := Nil;
   TargetMap := Nil;
   AccessMap := Nil;

   Azimuth := 0;

   PageControl1.Visible := true;
   PageControl1.ActivePage := TabSheet1;
   Pitch2 := -99;
   LastSighting := tStringList.Create;

   CheckBox1.Checked := DragonPlotDef.ShowLOS;
   CheckBox2.Checked := DragonPlotDef.PlotAccuracy;
   CheckBox3.Checked := DragonPlotDef.TerrainShadows;
   CheckBox99.Checked := DragonPlotDef.FireAccessMap;

   RadioGroup2.ItemIndex := DragonPlotDef.TargetMapType;

   Edit6.Text := RealToString(DragonPlotDef.AzAccuracy,-8,-2);
   Edit8.Text := RealToString(DragonPlotDef.PitchAccuracy,-8,-2);
   Edit19.Text := IntToStr(DragonPlotDef.MinPitch);
   Edit11.Text := IntToStr(DragonPlotDef.ViewDepth);
   Edit20.Text := IntToStr(DragonPlotDef.MaxPitch);
   Edit23.Text := RealToString(DragonPlotDef.PanoramaPitch,-8,-2);
   Edit25.Text := IntToStr(DragonPlotDef.FireAccessMapSize);
   Edit26.Text := IntToStr(DragonPlotDef.FireAccessMapBlowup);

   RadioGroup3.ItemIndex := DragonPlotDef.MapLevelIndex;
   RadioGroup3Click(nil);

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

   if TrilobiteComputer then begin
      Edit3.Text := '234';
      Edit9.Text := '-2';
   end;

   Petmar.CheckFormPlacement(Self);
   SettingUp := false;
end;



procedure TDragonPlotForm.SetLastShotTime;
begin
   LastShotTime := CurrentMilitaryTime(true);
end;


procedure TDragonPlotForm.PanoramaSpeedButtonClick(Sender: TObject);
var
   Depth : float64;
   NewDEM : integer;
   Bitmap : Graphics.tBitmap;
   wFan : tWeaponsFan;
   //CoverName,
   fName : PathStr;
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.PanoramaSpeedButtonClick in'); {$EndIf}
   SetLastShotTime;
   CloseWindows;
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.PanoramaSpeedButtonClick (Pan button) in, Tower: ' + PrimaryTowerComboBox7.Text + '  Towers=' + IntToStr(GISdb[TowerTable].MyData.RecordCount)); {$EndIf}

   CheckCameraParameters;

   with DEMglb[RegionalDEM].SelectionMap.MapDraw do begin
      Depth := GISdb[TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
      Edit11.Text := RealToString(Depth,-18,0);

     {$IfDef RecordDP} WriteLineToDebugFile('Tower location: ' + LatLongDegreeToString(TowerLat,TowerLong)); {$EndIf}

      if (Sender = BitBtn20) then begin  //how masked
         {$IfDef RecordViewshed} WriteLineToDebugFile('(Sender = BitBtn20)'); {$EndIf}
         MDDef.DoReqAntHigh := true;
         MDDef.DoReqFlyHigh := false;
         MDDef.DoGrazingAngle := false;
         NewDEM := MakeRequiredAntennaMap('Antennas',1,TowerLat,TowerLong,TowerHeight,Depth,true,0,360);
         DEMGlb[NewDEM].SelectionMap.MapDraw.MapType := mtElevFromTable;
         DEMGlb[NewDEM].SelectionMap.MapDraw.BaseTitle := PrimaryTowerComboBox7.Text + ' Visibility categories';
         DEMGlb[NewDEM].SelectionMap.DoCompleteMapRedraw;
      end;

      if (Sender = BitBtn16) then begin //tower viewshed
         {$IfDef RecordViewshed} WriteLineToDebugFile('(Sender = BitBtn16) tower viewshed'); {$EndIf}
         GISdb[TowerTable].MyData.ApplyFilter('NAME=' + QuotedStr(Trim(PrimaryTowerComboBox7.Text)));
         fName := GISdb[TowerTable].MyData.GetFieldByNameAsString('IMAGE');
         {$IfDef RecordViewshed} WriteLineToDebugFile('fName=' + fName); {$EndIf}
         if (fName = '') or not FileExists(fName) then begin
            wFan := WeaponsTableToFan(DEMglb[RegionalDEM].SelectionMap.MapDraw.PrimMapProj,GISdb[TowerTable].MyData);
            wFan.FanFileName := '';
            {$IfDef RecordViewshed} WriteLineToDebugFile('call add to fan'); {$EndIf}
            DEMglb[RegionalDEM].SelectionMap.MapDraw.AddFanToMap(WFan);
            {$IfDef RecordViewshed} WriteLineToDebugFile('Back from add to fan, fName=' + wFan.FanFileName); {$EndIf}
            GISdb[TowerTable].MyData.Edit;
            GISdb[TowerTable].MyData.SetFieldByNameAsString('IMAGE',wFan.FanFileName);
            GISdb[TowerTable].MyData.Post;
            {$IfDef RecordDP} WriteLineToDebugFile('Viewshed created ' + wFan.FanFileName); {$EndIf}
         end;
         CopyImageToBitmap(DEMglb[RegionalDEM].SelectionMap.Image1,Bitmap);
         DEMglb[RegionalDEM].SelectionMap.MapDraw.DrawWorldFileImageOnMap(Bitmap,FName);
         DEMglb[RegionalDEM].SelectionMap.Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;
         GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
      end;

      if (Sender = PanoramaSpeedButton) then begin
         {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.PanoramaSpeedButtonClick start pan'); {$EndIf}
         MDdef.PerspOpts.WhichPerspective := ReflectancePerspective;
         DEMPersPan := nil;
         SetUpPanoramaView(DEMPersPan,TowerLat,TowerLong,TowerHeight,Depth,0,400,40,DragonPlotDef.PanoramaPitch,1);
         DEMPersPan.Caption := InstanceName + ': ' + PrimaryTowerComboBox7.Text + ' panorama view ' + LastShotTime;
         DEMPersPan.Top := Screen.Height - 100 - DEMPersPan.Height;
         DEMPersPan.Left := Screen.Width - 5 - DEMPersPan.Width;
         DEMPersPan.Closable := false;
      end;
   end;

   GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   DEMglb[RegionalDEM].SelectionMap.Forceredrawlegendsscalebars1Click(Nil);
   {$IfDef RecordDP} WriteLineToDebugFile('Pan button out, Towers=' + IntToStr(GISdb[TowerTable].MyData.RecordCount)); {$EndIf}
end;


procedure TDragonPlotForm.SecondaryTowerComboBox2Change(Sender: TObject);
begin
   GroupBox2.Enabled := true;
   Edit14.Enabled := true;
   Edit15.Enabled := true;
end;

procedure TDragonPlotForm.BitBtn7Click(Sender: TObject);
var
   Depth : float64;
   Lat2,Long2 : float64;
begin
   CloseWindows;
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn7Click--LOS button selected Tower: ' + PrimaryTowerComboBox7.Text + '  Azimuth: ' + RealToString(Azimuth,-12,0)); {$EndIf}
   CheckCameraParameters;
   MDDef.ObsAboveGround := TowerHeight;
   MDDef.TargetAboveGround := 0;
   Depth := GISdb[TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
   repeat
      Edit11.Text := RealToString(Depth,-18,0);
      VincentyPointAtDistanceBearing(TowerLat,TowerLong,Depth,Azimuth,Lat2,Long2);
      Depth := Depth - 5000;
   until DEMglb[RegionalDEM].LatLongDegreeInDEM(Lat2,Long2);
   StartLOS(true,JustWandering,RegionalDEM,TowerLat,TowerLong,Lat2,Long2,DEMglb[RegionalDEM].SelectionMap,true);
   ShowDefaultCursor;
end;


procedure TDragonPlotForm.BitBtn8Click(Sender: TObject);
begin
   CloseAndNilNumberedDB(LightningTable);
   SetTabSheets;
end;

procedure TDragonPlotForm.CrossShotBitBtn8Click(Sender: TObject);
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.CrossShotBitBtn8Click (cross shot)'); {$EndIf}
   //ShotMode := smCross;
   ProcessShot(smCross);
end;


procedure TDragonPlotForm.BitBtn9Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to NumDEMDataSetsOpen do if ValidDEM(i) then DEMGlb[i].SelectionMap.DoCompleteMapRedraw;
end;


procedure TDragonPlotForm.Button1Click(Sender: TObject);
begin
   AdminPopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TDragonPlotForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := true;
end;

procedure TDragonPlotForm.BitBtn10Click(Sender: TObject);
var
   CanClose : boolean;
   i : integer;
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.BitBtn10Click enter--close program button'); {$EndIf}
   DragonPlotDef.DefaultTowerName[DragonPlotInstance] := PrimaryTowerComboBox7.Text;
   ProcessDragonPlotIniFile(iniWrite);
   SaveMDdefaults;
   CloseWindows;
   {$IfDef RecordClosing} WriteLineToDebugFile('Gaz=' + LastGazFile); {$EndIf}
   {$IfDef RecordClosing} PLSStoDebugFile; {$EndIf}
   if (MDDef.ProgramOption = DragonPlotProgram) then begin
      halt;         //this is ugly, but 9/21/2020 could not figure out what happens without it
      CloseAllPLSS;
     {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.BitBtn10Click PLSS closes'); {$EndIf}
      WMDem.ProgramClosing := true;
      if ValidDEM(RegionalDEM) then DEMglb[RegionalDEM].SelectionMap.Closable := true;
      for I := 1 to MaxDataBase do if ValidDB(i) then GISdb[i].FanCanClose := true;
      DEM_Manager.CloseAllWindowsAndData;
     {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.BitBtn10Click 2'); {$EndIf}
      FormCloseQuery(Nil,CanClose);
     {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.BitBtn10Click 3'); {$EndIf}
      if (MDDef.ProgramOption = DragonPlotProgram) then wmDEM.Close;
     {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.BitBtn10Click done'); {$EndIf}
   end
   else begin
      Close;
   end;
end;


procedure PushPinInGoogleEarth(UseMap : tMapForm; Name : shortstring; Lat,Long : float64);
var
   csv : tstringlist;
   db : integer;
begin
   {$IfDef RecordKML} WriteLineToDebugFile('PushPinInGoogleEarth in'); {$EndIf}
   csv := tstringlist.Create;
   csv.add('NAME,LAT,LONG');
   csv.Add(Name + ',' + RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7));
   db := UseMap.StringListToLoadedDatabase(csv,MDTempDir + 'Shot_Location.dbf',false,true,false);
   GISdb[db].ExportToKML(false,true);
   CloseAndNilNumberedDB(db);
   {$IfDef RecordKML} WriteLineToDebugFile('PushPinInGoogleEarth out'); {$EndIf}
end;


procedure TDragonPlotForm.BitBtn11Click(Sender: TObject);
var
   i : integer;
   bmp : Graphics.tBitmap;
begin
   {$IfDef RecordKML} WriteLineToDebugFile('****** TDragonPlotForm.BitBtn11Click (Record KML) in'); {$EndIf}
   BitBtn11.Enabled := false;
   PetImage.CopyImageToBitmap(DEMPersF.Image1,bmp);
   KMLLogo2FileName := MDTempDir + 'View_of_shot.png';
   bmp.SaveToFile(KMLLogo2FileName);
   Bmp.Free;

   CreateBitmap(bmp,800,1200);
   for i := 0 to pred(Memo1.Lines.Count) do bmp.Canvas.TextOut(5,5+i*15,Memo1.Lines[i]);

   PetImage.GetImagePartOfBitmap(bmp);
   KMLLogo1FileName:= MDTempDir + 'Shot_details.gif';
   bmp.SaveToFile(KMLLogo1FileName);
   Bmp.Free;
   {$IfDef RecordKML} WriteLineToDebugFile('TDragonPlotForm.BitBtn11Click two logos done'); {$EndIf}

   //fName := Petmar.NextFileNumber(MDTempDir,'sighting_',DefaultDBExt);

   MDDef.KML_DB_tables := false;
   MDDef.KMLOutputOption := 0;
   MDDef.AskAboutKMLExport := false;
   MDDef.KMLOpenGoogleEarth := true;
   MDDef.ZipKMLfiles := false;

   if (TargetMap <> Nil) then begin
      PushPinInGoogleEarth(TargetMap,'Shot ' + LastShotTime,ShotLat,ShotLong);
      DrawSightingsOnMap(true,TargetMap);
      {$IfDef RecordKML} WriteLineToDebugFile('TDragonPlotForm.BitBtn11Click call TargetMap.QuickbasemaptoGoogleEarth1Click'); {$EndIf}
      TargetMap.QuickbasemaptoGoogleEarth1Click(Sender);
      {$IfDef RecordKML} WriteLineToDebugFile('TDragonPlotForm.BitBtn11Click TargetMap.QuickbasemaptoGoogleEarth1Click done'); {$EndIf}
   end;

   {$IfDef RecordKML} WriteLineToDebugFile('TDragonPlotForm.BitBtn11Click PushPinInGoogleEarth done'); {$EndIf}
   KMLLogo1FileName := '';
   KMLLogo2FileName := '';
   wmdem.SendToBack;
   {$IfDef RecordKML} WriteLineToDebugFile('TDragonPlotForm.BitBtn11Click (KML) out'); {$EndIf}
end;


procedure TDragonPlotForm.FormActivate(Sender: TObject);
begin
   (*
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.FormActivate, instance=' + IntToStr(DragonPlotInstance)); {$EndIf}
   if (TargetMap <> Nil) then TargetMap.BringToFront;
   if (DEMPersF <> Nil) then DEMPersF.BringToFront;
   if (DEMPersF2 <> Nil) then DEMPersF2.BringToFront;
   if (DEMPersPan <> Nil) then DEMPersPan.BringToFront;
   if (DEMLOSF <> Nil) then DEMLOSF.BringToFront;
   if (DEMLOSF2 <> Nil) then DEMLOSF2.BringToFront;
   if (AccessMap <> Nil) then AccessMap.BringToFront;
   Panel1.Enabled := true;
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.FormActivate out'); {$EndIf}
   *)
end;

procedure TDragonPlotForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.FormClose in'); {$EndIf}
   CloseWindows;
   Action := caFree;
   {$IfDef RecordClosing} WriteLineToDebugFile('TDragonPlotForm.FormClose out'); {$EndIf}
end;


procedure TDragonPlotForm.LoadTowersIntoComboBoxes;
var
   TStr : shortString;

   procedure AddToBoxes;
   begin
      PrimaryTowerComboBox7.Items.Add(TStr);
      SecondaryTowerComboBox2.Items.Add(TStr);
   end;

begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.LoadTowersIntoComboBoxes in'); {$EndIf}
   if (TowerTable <> 0) then with GISdb[TowerTable].MyData do begin
      PrimaryTowerComboBox7.Items.Clear;
      SecondaryTowerComboBox2.Items.Clear;
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
      GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
      ApplyFilter('(USE=' + QuotedStr('Y') + ')');
      BitBtn23.Enabled := (RecordCount > 0);
      {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.LoadTowersIntoComboBoxes out,  Tower Total=' + IntToStr(RecordCount)); {$EndIf}
   end;
end;


procedure TDragonPlotForm.OpenGoogleEarth1Click(Sender: TObject);
begin
   ReadDefault('KML delay (sec)',DragonPlotDef.KMLdelay);
   {$IfDef RecordKML} WriteLineToDebugFile('TDragonPlotForm.OpenGoogleEarth1Click'); {$EndIf}
   ExecuteFile('"C:\Program Files\Google\Google Earth Pro\client\googleearth.exe"');
   Delay(1000*DragonPlotDef.KMLdelay);
end;

procedure TDragonPlotForm.CreateProfile(var DEMLOSF : TDEMLOSF; TowerLat,TowerLong,PLSSLat,PLSSLong,TowerHeight : float64; LabelStr : ShortString);
var
   Dist,Bearing,extra : float64;
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.CreateProfile in'); {$EndIf}
   VincentyCalculateDistanceBearing(TowerLat,TowerLong,PLSSLat,PLSSLong,Dist,Bearing);
   extra := 5000;
   repeat
      VincentyPointAtDistanceBearing(TowerLat,TowerLong,Dist + extra,Bearing,PLSSLat,PLSSLong);
      Extra := Extra - 500;
   until (Extra < 0) or (DEMglb[RegionalDEM].LatLongDegreeInDEM(PLSSLat,PLSSLong));

   DEMLOSF := StartLOS(true,JustWandering,RegionalDEM,TowerLat,TowerLong,PLSSLat,PLSSLong,DEMglb[RegionalDEM].SelectionMap{,true});
   if (DEMLOSf <> Nil) then begin
      DEMLOSF.Closable := false;
      DEMLOSF.Caption := InstanceName + ': Profile from ' +  LabelStr + ' to fire ' + LastShotTime;
   end;
   ShowDefaultCursor;
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.CreateProfile out'); {$EndIf}
end;



procedure TDragonPlotForm.CreateSinglePerspective;
begin
end;



procedure TDragonPlotForm.BitBtn13Click(Sender: TObject);
var
   tz,Pitch : float32;
   BlockDist,Distance : float64;
   t1,t2,t3 : shortString;
   vis,OnMap : boolean;
   Current : float64;
   Polarity : shortstring;
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn13Click 1, Edit11.text=' + Edit11.Text); {$EndIf}
   CheckCameraParameters;
   CloseWindows;
   InsureLocationMapOK;
   SetLastShotTime;

   t1 := ' (computed)';
   t2 := ' (entered)';
   if (Sender = Nil) then begin
      t3 := '';
      if GISdb[LightningTable].MyData.FieldExists('DTG') then begin
         t3 := '(' + GISdb[LightningTable].MyData.GetFieldByNameAsString('DTG') + ')';
      end
      else if GISdb[LightningTable].MyData.FieldExists('DATE') then begin
         t3 := '(' + GISdb[LightningTable].MyData.GetFieldByNameAsString('DATE') + '  ' + GISdb[LightningTable].MyData.GetFieldByNameAsString('TIME') +  ')';
      end
      else if GISdb[LightningTable].MyData.FieldExists('TIMESTAMP') then begin
         t3 := '(' + GISdb[LightningTable].MyData.GetFieldByNameAsString('TIMESTAMP') +  ')';
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

   OnMap := DEMglb[RegionalDEM].GetElevFromLatLongDegree(PLSSLat,PLSSLong,tz);

   Depth := GISdb[TowerTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
   GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   DisplayMessage(t3 + ' done ' + LastShotTime);

   if (Sender = Nil) then begin
      GetCurrentAndPolarity(Polarity,Current);
      DisplayMessage('Current:  ' + RealToString(Current,-12,0));
      DisplayMessage('Polarity: ' + Polarity);
   end;

   DisplayMessage('Tower: ' + PrimaryTowerComboBox7.Text + ' Elev: ' + RealToString(CameraElev,6,1) + ' m   (' +  RealToString(CameraElev/FeetToMeters,6,1) + ' ft)');
   DisplayMessage('Estimated location:');
   if (PLSSString <> '') then DisplayMessage('   ' + PLSSString + T1);
   DisplayMessage('   ' + DEMglb[RegionalDEM].SelectionMap.MapDraw.PrimMapProj.h_DatumCode + '   ' + LatLongDegreeToString(PLSSLat,PLSSLong,MDDEF.OutPutLatLongMethod) + t2);

   if OnMap then DisplayMessage('   Elev: ' + RealToString(tz,6,1) + ' m   (' +  RealToString(tz/FeetToMeters,6,1) + ' ft)')
   else DisplayMessage('   Not on map');
   VincentyCalculateDistanceBearing(TowerLat,TowerLong,PLSSLat,PLSSLong,Distance,Azimuth);

   DisplayMessage('Computed bearing angle: ' + RealToString(Azimuth,-8,2) + '°   ' + ConvertToDegreesString(Azimuth,DecMinutes));
   DisplayMessage('Computed distance: ' + SmartDistanceMetersFormat(Distance));
   if OnMap then begin
      Pitch := arctan((tz - CameraElev - DropEarthCurve(Distance)) / Distance) / DegToRad;
      DisplayMessage('Computed pitch: ' + RealToString(Pitch,-8,2) + '°   ' + ConvertToDegreesString(Pitch,DecMinutes));
   end;

   DisplayMessage('');
   ShotLat := PLSSLat;
   ShotLong := PLSSLong;
   SecondSighting := false;
   ShowOnMaps(OnMap);

   SetUpPanoramaView(DEMPersF,TowerLat,TowerLong,TowerHeight,Depth,Azimuth,DragonPlotDef.CustomHFOV,DragonPlotDef.CustomVFOV,Pitch,1,'View from ' + Trim(PrimaryTowerComboBox7.Text));
   DEMPersF.Top := 0;
   DEMPersF.Left := Screen.Width - 10 - Self.Width - DEMPersF.Width;
   DEMPersF.Closable := false;
   Vis := DEMglb[RegionalDEM].LatLongDegreePointsIntervisible(TowerLat,TowerLong,TowerHeight,PLSSLat,PLSSLong,1,Distance,BlockDist);

   if Vis then begin
      t2 := 'L';
      DEMPersF.SymbolAtPitchAzimith(Pitch,Azimuth,DragonPlotDef.VisibleSymbol,true);
      DisplayMessage('Location should be visible from tower');
   end
   else begin
      t2 := 'Masked l';
      DEMPersF.SymbolAtPitchAzimith(Pitch,Azimuth,DragonPlotDef.MaskedSymbol,true);
      DisplayMessage('Location should be masked from tower'); // at ' + SmartDistanceMetersFormat(BlockDist));
   end;
   DEMPersF.Caption := InstanceName + ': ' + t2 + 'ocate from ' +  PrimaryTowerComboBox7.Text + ' ' + LastShotTime;
   Edit3.Text := RealToString(Azimuth,-12,-2);
   EndShot;
  {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn13Click end, Edit11.text=' + Edit11.Text); {$EndIf}
end;


procedure TDragonPlotForm.EndShot;
begin
   DisplayMessage('');
   DisplayMessage('Notes:');
   BitBtn11.Enabled := (TargetMap <> Nil) or (SatImage[1] <> Nil);
   BitBtn36.Enabled := (TargetMap <> Nil) or (SatImage[1] <> Nil);
end;

procedure TDragonPlotForm.BitBtn14Click(Sender: TObject);
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn14Click in'); {$EndIf}
   CheckCameraParameters;
   if (Sender = BitBtn14) then GetLatLong(DEMglb[RegionalDEM].SelectionMap.MapDraw.PrimMapProj,'Smoke sighting',PLSSLat,PLSSLong);
   if (PLSS[1] <> Nil) then begin
      PLSSString := PLSSLocation(PLSSLat,PLSSLong);
      Label24.Caption := PLSSString;
   end;
   //MolodenskiyTransformation(PLSSLat,PLSSLong,Lat2,Long2,DEMglb[RegionalDEM].SelectionMap.MapDraw.PrimMapProj,DEMglb[RegionalDEM].SelectionMap.MapDraw.SecondaryMapDatum);
   BitBtn13.Enabled := true;
   PLSSentered := false;
   Label26.Caption := LatLongDegreeToString(PLSSLat,PLSSLong,MDDEF.OutPutLatLongMethod);
   Forms.Screen.Cursor := crDefault;
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn14Click out'); {$EndIf}
end;


procedure TDragonPlotForm.BitBtn15Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn15,DragonPlotDef.NegLightningSymbol,'- Lightning strikes');
   SetLightningSymbols;
   GISdb[LightningTable].RedrawLayerOnMap;
end;


procedure TDragonPlotForm.BitBtn16Click(Sender: TObject);
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn16Click (tower viewshed) in'); {$EndIf}
   DEMGlb[RegionalDEM].SelectionMap.DoFastMapRedraw;
   PanoramaSpeedButtonClick(Sender);
end;

procedure TDragonPlotForm.BitBtn12Click(Sender: TObject);
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn12Click in'); {$EndIf}
   if (PLSS[1] <> Nil) then begin
      {$IfDef RecordDP} WriteLineToDebugFile('PLSS was not nil'); {$EndIf}
      PLSSentered := true;
      if GetPLSSLocation(PLSSString,PLSSLat,PLSSLong,DEMglb[RegionalDEM].SelectionMap) then begin
         {$IfDef RecordDP} WriteLineToDebugFile('GetPLSSLocation success, ' + Label26.Caption); {$EndIf}
         BitBtn13.Enabled := true;
         Label24.Caption := PLSSString;
         Label26.Caption := LatLongDegreeToString(PLSSLat,PLSSLong,MDDEF.OutPutLatLongMethod);
         {$IfDef RecordDP} WriteLineToDebugFile('GetPLSSLocation success, ' + Label26.Caption); {$EndIf}
      end
      else begin
         {$IfDef RecordDP} WriteLineToDebugFile('GetPLSSLocation failure'); {$EndIf}
         BitBtn13.Enabled := false;
         Label24.Caption := '';
         Label26.Caption := '';
      end;
   end;
   Forms.Screen.Cursor := crDefault;
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn12Click out'); {$EndIf}
end;


procedure TDragonPlotForm.RadioGroup2Click(Sender: TObject);
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
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.RadioGroup2Click, DragonPlotDef.TargetMapType=' + IntToStr(DragonPlotDef.TargetMapType)); {$EndIf}
end;

procedure TDragonPlotForm.RadioGroup3Click(Sender: TObject);
var
  ls : float32;
begin
   case RadioGroup3.ItemIndex of
      0 : ls := 0.02;
      1 : ls := 0.05;
      2 : ls := 0.1;
      3 : ls := MDDef.BlowUpLatSize;
   end;
   DragonPlotDef.MapLevelIndex := RadioGroup3.ItemIndex;
   Edit12.Text := RealToString(ls,-8,-6);
   if RadioGroup3.ItemIndex = 3 then Edit13.Text := RealToString(MDDef.BlowUpLongSize,-8,-6)
   else Edit13.Text := RealToString(ls * 1.5,-8,-6);
   Edit12.Enabled := (RadioGroup3.ItemIndex = 3);
   Edit13.Enabled := (RadioGroup3.ItemIndex = 3);
end;

procedure TDragonPlotForm.RestoreConfigurationButtonClick(Sender: TObject);

   procedure CustomizeMDDefaultsForDragonPlot;
   begin
      {$IfDef RecordDP} WriteLineToDebugFile('CustomizeMDDefaults in'); {$EndIf}
   //none of these would have to be set
      MDDef.ProgramOption := DragonPlotProgram;
      MDDef.ShowLabs := false;
      MDDef.ShowClimateAndLight := false;
      MDDef.ShowCartography := false;
      MDdef.AutoOpen := aoDEM;
      SetGeologyOptions(false);
      SaveMDdefaults;
      MDDef.KeyLocationSymbol := DragonPlotDef.ResultSymbol;
   end;

begin
   CustomizeMDDefaultsForDragonPlot;
   ProcessDragonPlotIniFile(iniInit);
end;

procedure TDragonPlotForm.CheckBox1Click(Sender: TObject);
begin
   DragonPlotDef.ShowLOS := CheckBox1.Checked;
end;

procedure TDragonPlotForm.CheckBox99Click(Sender: TObject);
begin
   DragonPlotDef.FireAccessMap := CheckBox99.Checked;
   {$If Defined(CheckAccessibility)} WriteLineToDebugFile('TDragonPlotForm.CheckBox99Click, DragonPlotDef.FireAccessMap=' + TrueOrFalse(DragonPlotDef.FireAccessMap)); {$EndIf}
end;

procedure TDragonPlotForm.BitBtn4Click(Sender: TObject);
begin
   CheckCameraParameters;
   gbLatStart := TowerLat;
   gbLongStart := TowerLong;
   DEMglb[RegionalDEM].SelectionMap.Geodeticbearing1Click(Nil);
   wmDEM.SetPanelText(0,PrimaryTowerComboBox7.Text + ' geodetic bearing');
   DEMglb[RegionalDEM].SelectionMap.SetFocus;
end;


procedure TDragonPlotForm.BitBtn6Click(Sender: TObject);
begin
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn6Click (clear) in'); {$EndIf}
   Edit1.Text := '';
   Edit2.Text := '';
   Edit3.Text := '';
   Edit9.Text := '';
   Edit14.Text := '';
   Edit15.Text := '';
   Edit16.Text := '';
   Edit17.Text := '';
   CloseWindows;
   {$IfDef RecordDP} WriteLineToDebugFile('TDragonPlotForm.BitBtn6Click (clear) out'); {$EndIf}
end;

procedure TDragonPlotForm.BitBtn30Click(Sender: TObject);
var
   SummaryBitmap : tBitmap;
begin
   {$If Defined(RecordViewshed) or Defined(RecordDP)} WriteLineToDebugFile('TDragonPlotForm.BitBtn30Click (composite viewshed) in'); {$EndIf}
   CloseWindows;
   DisplayMessage('Start composite viewshed');
   Memo1.Scrollbars := ssVertical;
   CloneImageToBitmap(DEMGlb[RegionalDEM].SelectionMap.Image1,SummaryBitmap);
   GISdb[TowerTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
   DEMGlb[RegionalDEM].SelectionMap.MapDraw.InsureAllFansDrawn(GISdb[TowerTable].MyData,SummaryBitmap,Memo1);
   {$IfDef RecordViewshed} SummaryBitmap.SaveToFile(MDTempDir + 'vs.bmp'); {$EndIf}
   DEMGlb[RegionalDEM].SelectionMap.IHSmergeOntoMap(SummaryBitmap);
   {$If Defined(RecordViewshed) or Defined(RecordDP)} WriteLineToDebugFile('TDragonPlotForm.BitBtn30Click (composite viewshed) out'); {$EndIf}
end;

procedure TDragonPlotForm.BitBtn31Click(Sender: TObject);
begin
   DragonPlotDef.VisDefPlot := true;
end;

procedure TDragonPlotForm.BitBtn32Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn32,MDDef.VisPtSymbol,'Visible strikes');
end;

procedure TDragonPlotForm.BitBtn33Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn33,MDDef.MaskPtSymbol,'Masked strikes');
end;

procedure TDragonPlotForm.BitBtn34Click(Sender: TObject);
begin
   if (LightningTable <> 0) and (GISdb[LightningTable].TheMapOwner <> Nil) then begin
      ChangeDEMNowDoing(IDDBforAction);
      DBEditting := LightningTable;
      GISdb[LightningTable].theMapOwner.SetFocus;
   end;
end;

procedure TDragonPlotForm.BitBtn35Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then PickLineSizeAndColor('Shot lines',BitBtn35,DragonPlotDef.ShotLineColor,DragonPlotDef.ShotLineWidth);
end;

procedure TDragonPlotForm.BitBtn36Click(Sender: TObject);
begin
   SimpleLoadGoogleMaps(ShotLat,ShotLong);
end;

procedure TDragonPlotForm.BitBtn3Click(Sender: TObject);
var
   fName : PathStr;
   DefExt : byte;
   Ext : ExtStr;
   TStr : ShortString;
begin
    FName := ArchiveDirectory;
    DefExt := 0;
    if GetFileMultipleMask('Archive to restore','Any archive|*.JPG;*.TXT|Screen capture|*.JPG|Text log|*.TXT',FName,DefExt) then begin
        Ext := UpperCase(ExtractFileExt(fName));
        TStr := 'Archived shot ' + ExtractFileName(fName);
        if (Ext = '.JPG') then DisplayBitmap(fName,TStr)
        else if (Ext = '.TXT') then QuickOpenEditWindow(FName,TStr);
    end;
end;


procedure TDragonPlotForm.Edit12Change(Sender: TObject);
begin
   CheckEditString(Edit12.Text,MDDef.BlowUpLatSize);
end;

procedure TDragonPlotForm.Edit13Change(Sender: TObject);
begin
   CheckEditString(Edit13.Text,MDDef.BlowUpLongSize);
end;

procedure TDragonPlotForm.Edit19Change(Sender: TObject);
begin
   CheckEditString(Edit19.Text,DragonPlotDef.MinPitch);
end;

procedure TDragonPlotForm.Edit20Change(Sender: TObject);
begin
   CheckEditString(Edit20.Text,DragonPlotDef.MaxPitch);
end;

procedure TDragonPlotForm.Edit23Change(Sender: TObject);
begin
   CheckEditString(Edit23.Text,DragonPlotDef.PanoramaPitch);
end;

procedure TDragonPlotForm.Edit25Change(Sender: TObject);
begin
   CheckEditString(Edit25.Text,DragonPlotDef.FireAccessMapSize);
end;

procedure TDragonPlotForm.Edit26Change(Sender: TObject);
begin
   CheckEditString(Edit26.Text,DragonPlotDef.FireAccessMapBlowup);
end;

procedure TDragonPlotForm.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,DragonPlotDef.AzAccuracy);
end;

procedure TDragonPlotForm.Edit8Change(Sender: TObject);
begin
   CheckEditString(Edit8.Text,DragonPlotDef.PitchAccuracy);
end;


procedure TDragonPlotForm.CheckBox2Click(Sender: TObject);
begin
   DragonPlotDef.PlotAccuracy := CheckBox2.Checked;
end;


procedure TDragonPlotForm.CheckBox3Click(Sender: TObject);
begin
   DragonPlotDef.TerrainShadows := CheckBox3.Checked;
end;

procedure TDragonPlotForm.CheckBox4Click(Sender: TObject);
begin
   if (not SettingUp) then begin
      DragonPlotDef.AdvancedOptions := CheckBox4.Checked;
      SetTabSheets;
   end;
end;


procedure TDragonPlotForm.GetCurrentAndPolarity(var Polarity : shortstring; var Mag : float64);
begin
    Polarity := '';
    Mag := 0;
    if GISdb[LightningTable].MyData.FieldExists('POLARITY') then begin
       Polarity := GISdb[LightningTable].MyData.GetFieldByNameAsString('POLARITY');
    end;
    if GISdb[LightningTable].MyData.FieldExists('PEAK_CURRE') then begin
       Mag := GISdb[LightningTable].MyData.GetFieldByNameAsFloat('PEAK_CURRE');
       if (Mag < 0) then Polarity := 'N' else Polarity := 'P';
    end;
end;

procedure TDragonPlotForm.SetLightningSymbols;
var
   //fName : PathStr;
   Polarity : shortstring;
   Mag : float64;
   Sym : tFullSymbolDeclaration;
begin
   GISdb[LightningTable].AddSymbolToDB;
   GISdb[LightningTable].EmpSource.Enabled := false;
   GISdb[LightningTable].MyData.First;
   while not GISdb[LightningTable].MyData.eof do begin
      GetCurrentAndPolarity(Polarity,Mag);
      (*
      if Polarity = 'N' then Sym := DragonPlotDef.NegLightningSymbol
      else if Polarity = 'P' then Sym := DragonPlotDef.PosLightningSymbol;
      *)
      if Polarity = 'N' then Sym.Color := claBlue  //16740352
      else if Polarity = 'P' then Sym.Color := claRed
      else Sym.Color := claLime;

      if GISdb[LightningTable].MyData.FieldExists('PEAK_CURRE') then begin
          Mag := abs(Mag);
          if Mag < 10000 then Sym.Size := 2
          else if Mag < 25000 then Sym.Size := 3
          else if Mag < 50000 then Sym.Size := 4
          else if Mag < 100000 then Sym.Size := 5
          else Sym.Size := 6;
      end
      else begin
          if Mag < 10 then Sym.Size := 2
          else if Mag < 20 then Sym.Size := 3
          else if Mag < 30 then Sym.Size := 4
          else if Mag < 40 then Sym.Size := 5
          else Sym.Size := 6;
      end;
      Sym.DrawingSymbol := FilledCircle;
      GISdb[LightningTable].MyData.Edit;
      GISdb[LightningTable].MyData.PostPointSymbol(Sym);
      GISdb[LightningTable].MyData.Next;
   end;
   GISdb[LightningTable].ShowStatus;
end;


procedure TDragonPlotForm.BitBtn17Click(Sender: TObject);
var
   fName : PathStr;
begin
   {$IfDef RecordLightning} WriteLineToDebugFile('TDragonPlotForm.BitBtn17Click in (import lighthing file)'); {$EndIf}
    FName := LightningDirectory;
    if (LightningTable <> 0) then CloseAndNilNumberedDB(LightningTable);
    if Petmar.GetFileFromDirectory('Lightning data','*.dbf;*.csv;*.txt',fName) then begin
      {$IfDef RecordLightning} WriteLineToDebugFile('lightning file=' + ExtractFileName(fName)); {$EndIf}
       if OpenNumberedGISDataBase(LightningTable,fName,true,false,DEMglb[RegionalDEM].SelectionMap) then begin
          {$IfDef RecordLightning} WriteLineToDebugFile('Pt 1: ' +  IntToStr(GISdb[LightningTable].MyData.RecordCount) + ' recs for lightning'); {$EndIf}
          GISdb[LightningTable].KMLExportable := false;
          GISdb[LightningTable].dbTablef.WindowState := wsNormal;
          GISdb[LightningTable].dbTablef.Panel2.Visible := DragonPlotDef.AdvancedOptions;
          GISdb[LightningTable].dbTablef.Width := Self.Width;
          GISdb[LightningTable].dbTablef.Height := 300;
          GISdb[LightningTable].dbTablef.Top := Self.Top + Self.Height + 25;
          GISdb[LightningTable].dbTablef.Left := Self.Left;
          {$IfDef RecordLightning} WriteLineToDebugFile('Pt 1.5: ' +  IntToStr(GISdb[LightningTable].MyData.RecordCount) + ' recs for lightning'); {$EndIf}

          if (UpperCase(ExtractFileExt(fName)) <> '.DBF') then begin
             SetLightningSymbols;
             GISdb[LightningTable].IntervisibilityFromPoint(false,TowerLat,TowerLong,CameraElev,TowerHeight);
             GISdb[LightningTable].AddSequentialIndex(RecNoFName,false);
          end;
          GISdb[LightningTable].dbOpts.DBAutoShow := dbasPointsInDB;

          GISdb[LightningTable].RedrawLayerOnMap;
          GISdb[LightningTable].dbTablef.CanCloseIt := false;
          GISdb[LightningTable].dbTablef.WindowState := wsMinimized;
          Self.BringToFront;
          {$IfDef RecordLightning} WriteLineToDebugFile('Pt 3: ' +  IntToStr(GISdb[LightningTable].MyData.RecordCount) + ' recs for lightning'); {$EndIf}
       end;
    end;
    SetTabSheets;
    {$IfDef RecordLightning} WriteLineToDebugFile('TDragonPlotForm.BitBtn17Click out (import lighthing file)'); {$EndIf}
end;


procedure TDragonPlotForm.BitBtn18Click(Sender: TObject);

      procedure PlotStrikes(ch : char; Symbol : tFullSymbolDeclaration);
      var
         i : integer;
      begin
         if (ch = 'X') then GISdb[LightningTable].MyData.ApplyFilter('')
         else GISdb[LightningTable].MyData.ApplyFilter('POLARITY=' + QuotedStr(ch));
         {$IfDef RecordLightning} WriteLineToDebugFile('Polarity ' + ch + ' has ' + IntToStr(GISdb[LightningTable].MyData.RecordCount) + ' recs'); {$EndIf}
         i := 0;
         GISdb[LightningTable].EmpSource.Enabled := false;
         GISdb[LightningTable].MyData.First;
         while not GISdb[LightningTable].MyData.eof do begin
            GISdb[LightningTable].MyData.Edit;
            GISdb[LightningTable].MyData.PostPointSymbol(Symbol);
            GISdb[LightningTable].MyData.Next;
            inc(i);
         end;
         GISdb[LightningTable].EmpSource.Enabled := true;
         {$IfDef RecordLightning} WriteLineToDebugFile('  symbols updated=' + intToStr(i)); {$EndIf}
      end;

begin
   {$IfDef RecordLightning} WriteLineToDebugFile('TDragonPlotForm.BitBtn18Click (plot lightning) in, recs = ' + IntToStr(GISdb[LightningTable].MyData.RecordCount)); {$EndIf}
   DragonPlotDef.VisDefPlot := false;
   if GISdb[LightningTable].MyData.FieldExists('POLARITY') then begin
      {$IfDef RecordLightning} WriteLineToDebugFile('Polarity field exists'); {$EndIf}
      PlotStrikes('P',DragonPlotDef.PosLightningSymbol);
      PlotStrikes('N',DragonPlotDef.NegLightningSymbol);
   end
   else begin
      {$IfDef RecordLightning} WriteLineToDebugFile('No Polarity field'); {$EndIf}
      PlotStrikes('X',DragonPlotDef.PosLightningSymbol);
   end;
   GISdb[LightningTable].MyData.ApplyFilter('');
   GISdb[LightningTable].RedrawLayerOnMap;
   Forms.Screen.Cursor := crDefault;
   {$IfDef RecordLightning} WriteLineToDebugFile('TDragonPlotForm.BitBtn18Click (plot lightning) out, recs = ' + IntToStr(GISdb[LightningTable].MyData.RecordCount)); {$EndIf}
end;


procedure TDragonPlotForm.BitBtn19Click(Sender: TObject);
begin
   ExecuteFile(DragonPlotDef.LightningWWW);
end;

procedure TDragonPlotForm.BitBtn1Click(Sender: TObject);
begin
  PopupMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TDragonPlotForm.DBGrid2DblClick(Sender: TObject);
begin
  {$IfDef RecordLightning} WriteLineToDebugFile('TDragonPlotForm.DBGrid2DblClick'); {$EndIf}
   if GISdb[LightningTable].ValidLatLongFromTable(PlssLat,PlssLong) and (PLSS[1] <> Nil) then begin
      PLSSString := PLSSLocation(PLSSLat,PLSSLong);
   end;
   BitBtn13Click(Nil);
   GISdb[LightningTable].MyData.ApplyFilter('');
end;


procedure TDragonPlotForm.BitBtn23Click(Sender: TObject);
begin
   GISdb[TowerTable].MyData.ApplyFilter('(NAME=' + QuotedStr('O -*') + ' OR NAME=' + QuotedStr('S_*') + ') AND USE=' + QuotedStr('Y'));
   while not GISdb[TowerTable].MyData.eof do begin
      if AnswerIsYes('Delete ' + GISdb[TowerTable].MyData.GetFieldByNameAsString('NAME')) then begin
         GISdb[TowerTable].MyData.Edit;
         GISdb[TowerTable].MyData.SetFieldByNameAsString('USE','N');
         GISdb[TowerTable].MyData.Next;
      end
      else GISdb[TowerTable].MyData.Next;
   end;
   LoadTowersIntoComboBoxes;
   GISdb[TowerTable].RedrawLayerOnMap;
end;


procedure TDragonPlotForm.BitBtn24Click(Sender: TObject);
var
   MenuStr : shortString;
   Lat,Long,z : float64;
   wf : tWeaponsFan;
begin
   with GISdb[TowerTable] do begin
     {$IfDef RecordTemporaryTowers} WriteLineToDebugFile('TPhotoRegForm.BitBtn24Click in,  towers =' + IntToStr(MyData.RecordCount)); {$EndIf}
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

      AddFanToWeaponsTable(TheMapOwner.MapDraw.PrimMapProj,false,true,MyData,wf);

      LoadTowersIntoComboBoxes;
      DEMGlb[RegionalDEM].SelectionMap.DoCompleteMapRedraw;
     {$IfDef RecordTemporaryTowers} WriteLineToDebugFile('TPhotoRegForm.BitBtn24Click out, towers =' + IntToStr(MyData.RecordCount)); {$EndIf}
   end;
end;

procedure TDragonPlotForm.BitBtn20Click(Sender: TObject);
begin
  PanoramaSpeedButtonClick(Sender);
end;

procedure TDragonPlotForm.BitBtn21Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn21,DragonPlotDef.PosLightningSymbol,'+ Lightning strikes');
   SetLightningSymbols;
   GISdb[LightningTable].RedrawLayerOnMap;
end;

procedure TDragonPlotForm.BitBtn22Click(Sender: TObject);
begin
   CheckCameraParameters;
   MDDef.HorizonLength := TowerRange;
   MDDef.ObsAboveGround := TowerHeight;
   MDDef.HorizonIncrement := 1;
   MDDef.HorizonVertAngleGraph := false;
   MDDef.HorizonDistanceGraph := false;
   HorizonBlockingGraph(DEMGlb[RegionalDEM].SelectionMap,TowerLat,TowerLong,false,false);
end;


procedure TDragonPlotForm.BitBtn25Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then PickSymbol(BitBtn25,DragonPlotDef.VisibleSymbol,'Visible points');
end;

procedure TDragonPlotForm.BitBtn26Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then PickSymbol(BitBtn26,DragonPlotDef.MaskedSymbol,'Masked points');
end;

procedure TDragonPlotForm.BitBtn27Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then PickSymbol(BitBtn27,DragonPlotDef.AccuracySymbol,'Accuracy plots');
end;

procedure TDragonPlotForm.BitBtn28Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then PickSymbol(BitBtn28,DragonPlotDef.TowerSymbol,'Towers');
end;

procedure TDragonPlotForm.BitBtn29Click(Sender: TObject);
begin
   if DragonPlotDef.AdvancedOptions then begin
      PickSymbol(BitBtn29,DragonPlotDef.ResultSymbol,'Shot results');
      MDDef.KeyLocationSymbol := DragonPlotDef.ResultSymbol;
   end;
end;


function TDragonPlotForm.CheckCameraParameters : boolean;
var
   InputError : boolean;

       function GetAngleDegrees(EditDeg,EditMin : tEdit; NegativeAllowed : boolean = false) : float64;
       var
          Minutes : float64;
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


begin
   {$IfDef RecordCheckCameraParamters} WriteLineToDebugFile('TDragonPlotForm.CheckCameraParameters in'); {$EndIf}
   InputError := false;
   PETMAR.CheckEditString(Edit11.Text,Depth);
   PETMAR.CheckEditString(Edit23.Text,DragonPlotDef.PanoramaPitch);
   Azimuth := GetAngleDegrees(Edit3,Edit1);
   Depress := GetAngleDegrees(Edit9,Edit2,true);
   if (Depress > DragonPlotDef.MaxPitch) or (Depress < DragonPlotDef.MinPitch) then begin
      Memo1.Lines.Add('');
      Memo1.Lines.Add('Pitch angle (' + RealToString(Depress,-9,-2) + ') may be excessive; verify');
      Memo1.Lines.Add('');
   end;
   Azimuth2 := GetAngleDegrees(Edit14,Edit15);
   Result := not InputError;

   {$IfDef RecordCheckCameraParamters}
      WriteLineToDebugFile(' Location:  ' + LatLongDegreeToString(TowerLat,TowerLong) +  ' TowerGroundElevation=' + RealToString(TowerGroundElevation,-8,1) + ' tower height=' + RealToString(TowerHeight,-8,1));
      WriteLineToDebugFile(' Camera Elev=' + RealToString(CameraElev,-8,1) + ' Depth=' + RealToString(Depth,-8,0) + ' Azimuth=' + RealToString(Azimuth,-8,2) + ' Pitch=' + RealToString(DragonPlotDef.PanoramaPitch,-8,2));
      WriteLineToDebugFile('TDragonPlotForm.CheckCameraParameters out');
   {$EndIf}
end;

var
   i : integer;
initialization
   for i := MaxInstances downto 1 do DragonPlotForm[i] := Nil;
finalization
   {$IfDef RecordDP} WriteLineToDebugFile('register_view finalization'); {$EndIf}
   {$IfDef RecordKML} WriteLineToDebugFile('RecordKML active in register_view'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing register_view'); {$EndIf}
end.
