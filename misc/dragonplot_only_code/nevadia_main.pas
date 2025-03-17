{DragonPlot main form}

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of DragonPlot Program      }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/24/2011       }
{_________________________________}



unit nevadia_main;

{$I Nevadia_defines.inc}

{$IfDef RecordProblems}
   //{$Define RecordClosingProblems}
   //{$Define RecordWebMapsProblems}
   //{$Define RecordFullWebTimerProblems}
   //{$Define RecordTowerLoadProblems}
{$EndIf}

//{$Define DebugDefaults}
//{$Define UseStandardDefaults}

interface


uses
  Windows, Messages,  iniFiles,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, Db,ExtCtrls, StdCtrls,
  DEMDefs, PETMar, Petmar_types;

const
   {$IfDef DragonPlotDemo}
   xxxxxx  not allowed
   ProgramName = 'DragonPlot Demo';
   {$Else}
      {$IfDef Win64}
      ProgramName = 'DragonPlot-64 2019 beta';
      {$EndIf}
   {$EndIf}

type
  Twmdem = class(TForm)
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    Options1: TMenuItem;
    Windows1: TMenuItem;
    Cascade1: TMenuItem;
    Tile1: TMenuItem;
    Help1: TMenuItem;
    Debuglog1: TMenuItem;
    About1: TMenuItem;
    Timer1: TTimer;
    Whatsopen1: TMenuItem;
    Defaults1: TMenuItem;
    Select1: TMenuItem;
    Convert1: TMenuItem;
    WebTimer: TTimer;
    File1: TMenuItem;
    Close1: TMenuItem;
    Memo1: TMemo;
    ChangeDEM1: TMenuItem;
    Saveconfiguration1: TMenuItem;
    Hardware1: TMenuItem;
    Advancedoptions1: TMenuItem;
    IndexTIGER1: TMenuItem;
    ChangeTowers1: TMenuItem;
    Automaticshapefileoverlay1: TMenuItem;
    Windowsizes1: TMenuItem;
    BackupEXE1: TMenuItem;
    Changeimagemap1: TMenuItem;
    Gazetteerfile1: TMenuItem;
    procedure Register1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Cascade1Click(Sender: TObject);
    procedure Tile1Click(Sender: TObject);
    procedure Debuglog1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Whatsopen1Click(Sender: TObject);
    procedure Select1Click(Sender: TObject);
    procedure Defaults1Click(Sender: TObject);
    procedure Convert1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure ChangeDEM1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Hardware1Click(Sender: TObject);
    procedure Advancedoptions1Click(Sender: TObject);
    procedure IndexTIGER1Click(Sender: TObject);
    procedure ChangeTowers1Click(Sender: TObject);
    procedure Automaticshapefileoverlay1Click(Sender: TObject);
    procedure Windowsizes1Click(Sender: TObject);
    procedure BackupEXE1Click(Sender: TObject);
    procedure Changeimagemap1Click(Sender: TObject);
    procedure Saveconfiguration1Click(Sender: TObject);
    procedure Gazetteerfile1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     TowerTable : Integer;
     ProgramClosing : boolean;
     procedure SetPanelText(PanelNum : integer; What : shortString);
     procedure SetMenusForVersion;
     procedure IDJudomia(xpic,ypic : integer; Lat,Long : float64);
     procedure FormPlacementInCorner(TheForm : Forms.tForm; FormPosition : byte = lpSEMap);
  end;


   procedure ToggleShowProgress(ShowIt : boolean);


var
  wmdem : Twmdem;
  LockStatusBar,
  SkipMenuUpdating,
  FirstRun : boolean;
  LightningDirectory,
  ArchiveDirectory : PathStr;
  ShortEXEName : shortstring;


implementation

{$R *.DFM}


uses
   PETMath,
   DEMDatum,
   DEMMapF,
   DEMCoord,
   DEMDef_routines,
   Read_DEM,
   GetLatLn,
   DEMOptions,
   PetdbUtils,
   DEM_Indexes,
   DEM_PLSS,
   DEMDataBase,
   DEM_Manager,
   PLSS_converter,
   Make_tables,
   Dragon_Plot_Init,
   {$IfDef ExRegisterView}
   {$Else}
   register_view,
   {$EndIf}
   DEMTiger;



procedure ToggleShowProgress(ShowIt : boolean);
begin
   ShowSatProgress := ShowIt;
   ShowDEMReadingProgress := ShowIt;
   SkipMenuUpdating := not ShowIt;
   ReportErrors := ShowIt;
end;



procedure Twmdem.SetPanelText(PanelNum : integer; What : shortString);
begin
   if not LockStatusBar then begin
      wmDEM.StatusBar1.Panels[PanelNum].Text := What;
      Application.ProcessMessages;
   end;
end;


procedure tWmDEM.SetMenusForVersion;  //required for map forms
begin
   if ProgramClosing then exit;
   
   {$IfDef ExRegisterView}
   {$Else}
   if (PhotoRegForm <> Nil) then begin
      PhotoRegForm.Caption := 'Fire tower information:    ' + ExtractFileNameNoExt(Application.ExeName);
   end;
   {$EndIf}
end;


procedure Twmdem.FormPlacementInCorner(TheForm : Forms.tForm; FormPosition :  byte = lpSEMap);
begin
   if (FormPosition = lpNEMap) then TheForm.Top := 0
   else TheForm.Top := ClientHeight - TheForm.Height - 5;
   if TheForm.Top < 0 then TheForm.Top := 10;
   TheForm.Left := ClientWidth - TheForm.Width - 10;
   if TheForm.Left < 0 then TheForm.Left := 10;
end;


procedure Twmdem.Gazetteerfile1Click(Sender: TObject);
begin
   Petmar.GetFileFromDirectory('gazetteer','*.dbf', DragonPlotDef.DefGazfName);
end;



procedure Twmdem.FormActivate(Sender: TObject);
var
   TowerFile : PathStr;
begin
   if FirstRun then begin
      SetUpDPsettings;
      if DragonPlotDef.AdvancedOptions then Advancedoptions1Click(sender);
      Select1.Visible := DragonPlotDef.AdvancedOptions;
      Timer1.Enabled := true;
      Changeimagemap1.Checked := (LastImageName <> '');
   end;
   FirstRun := false;
end;



procedure Twmdem.Register1Click(Sender: TObject);
begin
{$IfDef ExRegisterView}
{$Else}
   {$IfDef RecordProblems}  WriteLineToDebugFile('Twmdem.Register1Click in'); {$EndIf}
   StartDragonPlot;
   {$IfDef RecordProblems}  WriteLineToDebugFile('Twmdem.Register1Click out'); {$EndIf}
{$EndIf}
end;


procedure Twmdem.IDJudomia(xpic,ypic : integer; Lat,Long : float64);
{$IfDef ExRegisterView}
begin
{$Else}
var
   RecsFound : integer;
   FeatureName : shortString;
begin
   GISdb[PhotoRegForm.LightningTable].dbOpts.MainFilter := '';
   GISdb[PhotoRegForm.LightningTable].dbOpts.GeoFilter := '';
   GISdb[PhotoRegForm.LightningTable].MyData.ApplyFilter('');
   GISdb[PhotoRegForm.LightningTable].IdentifyRecord(xpic,ypic,Lat,Long,RecsFound,false,false,FeatureName,false,true);
   if (RecsFound > 0) then begin
      PhotoRegForm.ComboBox4.Text := PhotoRegForm.ComboBox6.Text;
      PhotoRegForm.PLSSLat := Lat;
      PhotoRegForm.PLSSLong := Long;
      PhotoRegForm.BitBtn13Click(Nil);
      GISdb[PhotoRegForm.LightningTable].MyData.ApplyFilter('');
   end;
{$EndIf}
end;


procedure Twmdem.IndexTIGER1Click(Sender: TObject);
begin
   DEMTiger.IndexTigerFiles;
end;


procedure Twmdem.Advancedoptions1Click(Sender: TObject);
begin
   Advancedoptions1.Checked := not Advancedoptions1.Checked;
   DragonPlotDef.AdvancedOptions := Advancedoptions1.Checked;
   Select1.Visible := DragonPlotDef.AdvancedOptions;
{$IfDef ExRegisterView}
{$Else}
   if (PhotoRegForm <> Nil) then PhotoRegForm.ShowAdvancedOptions;
{$EndIf}
end;

procedure Twmdem.Automaticshapefileoverlay1Click(Sender: TObject);
begin
   Petmar.GetFileFromDirectory('Shape file for auto overlay','*.shp',DragonPlotDef.DefaultShapeFile);
end;


procedure Twmdem.BackupEXE1Click(Sender: TObject);
begin
   BackupProgramEXE('DP');
end;

procedure Twmdem.Cascade1Click(Sender: TObject);
begin
   Cascade;
end;

procedure Twmdem.Tile1Click(Sender: TObject);
begin
   Tile;
end;

procedure Twmdem.Timer1Timer(Sender: TObject);
begin
   Timer1.Enabled := false;
   Register1Click(Sender);
   WantShowProgress := true;
end;

procedure Twmdem.Debuglog1Click(Sender: TObject);
begin
   {$IfDef RecordProblems}
   StopSplashing;
   ShowInNotepadPlusPlus(DebugFilename,'Dragon Plot Debug log' )
   {$EndIf}
end;

procedure Twmdem.About1Click(Sender: TObject);
begin
   PETMARAboutBox(ProgramName, '');
end;


procedure Twmdem.FormClose(Sender: TObject; var Action: TCloseAction);
var
   j : integer;
begin
   {$IfDef RecordProblems}
   WriteLineToDebugFile('Twmdem.FormClose in');
   {$EndIf}
   Action := caFree;
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Twmdem.FormClose out');
   {$EndIf}
end;


procedure Twmdem.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Twmdem.FormCloseQuery in',true);
   {$EndIf}
   CanClose := true;
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Twmdem.FormCloseQuery out',true);
   {$EndIf}
end;

procedure Twmdem.Whatsopen1Click(Sender: TObject);
var
   OpenData : tStringList;
begin
   OpenData := DEM_Manager.GetWhatsOpen;
   DisplayAndPurgeStringList(OpenData,'Open data sets');
end;


procedure Twmdem.Windowsizes1Click(Sender: TObject);
begin
   ReadDefault('Page control height 1 (suggest 250)',DragonPlotDef.PageControlHeight1);
   ReadDefault('Page control height 2 (suggest 161)',DragonPlotDef.PageControlHeight2);
end;


procedure Twmdem.Hardware1Click(Sender: TObject);
begin
   Petmar.MessageToContinue(HardwareString + #10#13 + 'Window size: ' + IntToStr(Self.Width) + 'x' + IntToStr(Self.Height));
end;

procedure Twmdem.Saveconfiguration1Click(Sender: TObject);
begin
   SaveMDdefaults;
   ProcessDragonPlotIniFile(iniWrite);
end;

procedure Twmdem.Select1Click(Sender: TObject);
var
   OptionsForm : TOptionsForm;
begin
   OptionsForm := TOptionsForm.Create(Application);
   OptionsForm.ShowModal;
   OptionsForm.Free;
   SetMenusForVersion;
{$IfDef ExRegisterView}
{$Else}
   PhotoRegForm.CloseWindows;
{$EndIf}
end;

procedure Twmdem.Defaults1Click(Sender: TObject);
begin
   CustomizeMDDefaultsForDragonPlot;
   ProcessDragonPlotIniFile(iniInit);
end;


procedure Twmdem.Convert1Click(Sender: TObject);
{$IfDef ExPLSS}
begin
{$Else}
var
   PLSSConvertForm : TPLSSConvertForm;
begin
   if TryToOpenPLSS(nil) then begin
      PLSSConvertForm := TPLSSConvertForm.Create(Application);
      PLSSConvertForm.Show;
   end;
{$EndIf}
end;


procedure Twmdem.Close1Click(Sender: TObject);
begin
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Twmdem.Close1Click'); {$EndIf}
   Close;
end;


procedure Twmdem.ChangeDEM1Click(Sender: TObject);
var
   i : integer;
begin
   LastDEMName := '';
   i := 1;
   CloseSingleDEM(i);
{$IfDef ExRegisterView}
{$Else}
   PhotoRegForm.LoadDEM;
{$EndIf}
end;


procedure Twmdem.Changeimagemap1Click(Sender: TObject);
var
   i : integer;
begin
   if Changeimagemap1.Checked then begin
      LastImageName := '';
      i := 1;
      CloseSingleSatelliteImage(i);
   end
   else PhotoRegForm.LoadSatelliteImage;
   Changeimagemap1.Checked := not Changeimagemap1.Checked;
end;

procedure Twmdem.ChangeTowers1Click(Sender: TObject);
var
   i : integer;
begin
   if Petmar.GetFileFromDirectory('Towers','*.dbf',DragonPlotDef.TowerFileName) then begin
      i := 1;
      CloseSingleDEM(i);
      PhotoRegForm.LoadDEM;
   end;
end;


initialization
   LockStatusBar := false;
   FirstRun := true;
finalization
   {$IfDef RecordProblems} WriteLineToDebugFile('nevadia_main finalizaton'); {$EndIf}
   {$IfDef RecordClosingProblems}  WriteLineToDebugFile('RecordClosingProblems active in Nevadia_main'); {$EndIf}
end.

