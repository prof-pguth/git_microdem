unit dem_plss_op;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordPLSSOps}
   //{$Define RecordFont}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  Petmar_types,DEMMapf;

type
  TPLSSform = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    OKBtn: TBitBtn;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    CheckBox6: TCheckBox;
    BitBtn4: TBitBtn;
    Label1: TLabel;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Label2: TLabel;
    BitBtn5: TBitBtn;
    CheckBox10: TCheckBox;
    BitBtn6: TBitBtn;
    Label3: TLabel;
    BitBtn7: TBitBtn;
    Label4: TLabel;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Label5: TLabel;
    Label6: TLabel;
    CheckBox11: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
  private
    { Private declarations }
    procedure CheckChanges;
    procedure GetNewFile(i : integer; theLabel : tLabel);
    procedure ShowSettings;
  public
    { Public declarations }
    ReloadPLSS : boolean;
    MapOwner : tMapForm;
  end;

function SetPLSSPlot(aMapOwner : tMapForm) : boolean;


implementation

{$R *.dfm}

uses
  Nevadia_Main,
  PETMAR,DEMDefs,DEM_PLSS,DEMDef_routines,Petmar_ini_file;


function SetPLSSPlot;
var
  PLSSform: TPLSSform;
begin
  {$IfDef RecordPLSSOps} WriteLineToDebugFile('Start SetPLSSPlot'); PLSStoDebugFile;  {$EndIf}
  PLSSform := TPLSSform.Create(Application);
  with PLSSform, MDDef.PLSSDef do begin
     MapOwner := aMapOwner;
     if (MapOwner <> Nil) then Label2.Caption := 'Current map pixel size: ' + RealToString(MapOwner.MapDraw.ScreenPixelSize,-8,1) + ' m';
     ShowSettings;
     ReloadPLSS := false;
     ShowModal;
     RedrawSpeedButton12Click(nil);
  end;
  {$IfDef RecordPLSSOps} WriteLineToDebugFile('SetPLSSPlot out'); PLSStoDebugFile; {$EndIf}
end;

procedure TPLSSform.ShowSettings;
begin
  with MDDef.PLSSDef do begin
    {$IfDef RecordFont} WriteLineToDebugFile('TPLSSform.ShowSettings, town font: ' + MyFontToString(MDDef.PLSSDef.TownFont)); {$EndIf}
    {$IfDef RecordFont} WriteLineToDebugFile('TPLSSform.ShowSettings, town color: ' + ColorStringFromPlatformColor(MDDef.PLSSDef.PLSSTownColor)); {$EndIf}
    {$IfDef RecordFont} WriteLineToDebugFile('TPLSSform.ShowSettings, sect font: ' +  MyFontToString(MDDef.PLSSDef.SectFont)); {$EndIf}
    {$IfDef RecordFont} WriteLineToDebugFile('TPLSSform.ShowSettings, sect color: ' + ColorStringFromPlatformColor(MDDef.PLSSDef.PLSSSectionColor)); {$EndIf}
     CheckBox1.Checked := PLSSShowQuarters;
     CheckBox2.Checked := PLSSShowSections;
     CheckBox3.Checked := PLSSShowTowns;
     CheckBox4.Checked := PLSSLabelSections;
     CheckBox5.Checked := PLSSLabelTowns;
     CheckBox6.Checked := PLSSsmartScaling;
     CheckBox7.Checked := PLSSFormat = plssTRS;
     CheckBox8.Checked := PLSSQuartersInLabels;
     CheckBox9.Checked := AutoDrawPLSS;
     CheckBox10.Checked := PLSSLotsInLabels;
     CheckBox11.Checked := PLSStoRAM;
     Edit1.Text := IntToStr(PLSSAppearQuarters);
     Edit2.Text := IntToStr(PLSSAppearSections);
     Edit3.Text := IntToStr(PLSSAppearTowns);
     Label1.Caption := PLSSFile[1];
     Label3.Caption := PLSSFile[2];
     Label4.Caption := PLSSFile[3];
     Label5.Caption := PLSSFile[4];
     Label6.Caption := PLSSFile[5];
     LoadMyFontIntoWindowsFont(MDDef.PLSSDef.SectFont,BitBtn10.Font);
     BitBtn10.Caption := '17';
     LoadMyFontIntoWindowsFont(MDDef.PLSSDef.TownFont,BitBtn11.Font);
     BitBtn11.Caption := 'T1S R2W';
   end;
end;

procedure TPLSSform.CheckChanges;
var
   i : integer;
begin
   with MDDef.PLSSDef  do begin
     PLSSShowQuarters := CheckBox1.Checked;
     PLSSShowSections := CheckBox2.Checked;
     PLSSShowTowns := CheckBox3.Checked;
     PLSSLabelSections := CheckBox4.Checked;
     PLSSLabelTowns := CheckBox5.Checked;
     PLSSsmartScaling := CheckBox6.Checked;
     if CheckBox7.Checked then PLSSFormat := plssTRS
     else PLSSFormat := plssSTR;
     PLSSQuartersInLabels := CheckBox8.Checked;
     AutoDrawPLSS := CheckBox9.Checked;
     PLSSLotsInLabels := CheckBox10.Checked;
     PLSStoRAM := CheckBox11.Checked;
     CheckEditString(Edit1.Text,PLSSAppearQuarters);
     CheckEditString(Edit2.Text,PLSSAppearSections);
     CheckEditString(Edit3.Text,PLSSAppearTowns);
     for i := 1 to 5 do
        if (PLSS[i] <> nil) then
           PLSS[i].SetPLSSDBoptions;
   end;
end;


procedure TPLSSform.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TPLSSform.RedrawSpeedButton12Click(Sender: TObject);
begin
   CheckChanges;
   if (MapOwner <> Nil) then MapOwner.DoCompleteMapRedraw;
end;

procedure TPLSSform.FormCreate(Sender: TObject);
begin
   with MDDef.PLSSDef do begin
      ColorLineWidthBitBtn(BitBtn1,PLSSQuarterColor,PLSSQuarterWidth);
      ColorLineWidthBitBtn(BitBtn2,PLSSSectionColor,PLSSSectionWidth);
      ColorLineWidthBitBtn(BitBtn3,PLSSTownColor,PLSSTownWidth);
   end;
   PlaceFormAtMousePosition(Self);
end;


procedure TPLSSform.BitBtn10Click(Sender: TObject);
begin
   EditMyFont(MDDef.PLSSDef.SectFont);
   ShowSettings;
end;

procedure TPLSSform.BitBtn11Click(Sender: TObject);
begin
   EditMyFont(MDDef.PLSSDef.TownFont);
   ShowSettings;
end;

procedure TPLSSform.BitBtn12Click(Sender: TObject);
begin
   {$IfDef RecordFont} WriteLineToDebugFile('TPLSSform.BitBtn12Click, town font: ' + MyFontToString(MDDef.PLSSDef.TownFont)); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('TPLSSform.BitBtn12Click, town color: ' + ColorStringFromPlatformColor(MDDef.PLSSDef.PLSSTownColor)); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('TPLSSform.BitBtn12Click, sect font: ' +  MyFontToString(MDDef.PLSSDef.SectFont)); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('TPLSSform.BitBtn12Click, sect color: ' + ColorStringFromPlatformColor(MDDef.PLSSDef.PLSSSectionColor)); {$EndIf}

   ProcessIniFile(iniInit,'PLSS');
   ShowSettings;
   RedrawSpeedButton12Click(Nil);
end;

procedure TPLSSform.BitBtn1Click(Sender: TObject);
begin
   PickLineSizeAndColor('PLSS 1/4 sections',BitBtn1,MDDef.PLSSDef.PLSSQuarterColor,MDDef.PLSSDef.PLSSQuarterWidth);
end;

procedure TPLSSform.BitBtn2Click(Sender: TObject);
begin
   PickLineSizeAndColor('PLSS sections',BitBtn2,MDDef.PLSSDef.PLSSSectionColor,MDDef.PLSSDef.PLSSSectionWidth);
end;

procedure TPLSSform.BitBtn3Click(Sender: TObject);
begin
   PickLineSizeAndColor('PLSS townships',BitBtn3,MDDef.PLSSDef.PLSSTownColor,MDDef.PLSSDef.PLSSTownWidth);
end;


procedure TPLSSform.GetNewFile(i : integer; theLabel : tLabel);
begin
   if (PLSS[i] <> Nil) then begin
      {$IfDef RecordPLSSOps} WriteLineToDebugFile('GetNewFile, Old PLSS=' + IntToStr(i) + '  ' + PLSSFile[i]); {$EndIf}
      PLSS[i].CloseFiles;
      PLSS[i].Destroy;
      PLSS[i] := Nil;
   end;

   if PETMAR.GetDOSPath('PLSS files',PLSSFile[i]) then begin
      TryToOpenOnePLSS(MapOwner,I,PLSSFile[i],true);
      RedrawSpeedButton12Click(Nil);
   end;
   theLabel.Caption := PLSSFile[i];
   {$IfDef RecordPLSSOps} WriteLineToDebugFile('GetNewFile, New PLSS=' + IntToStr(i) + '  ' + PLSSFile[i]); {$EndIf}
end;


procedure TPLSSform.BitBtn4Click(Sender: TObject);
begin
   GetNewFile(1,Label1);
end;


procedure TPLSSform.BitBtn5Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\plss_opts.htm');
end;


procedure TPLSSform.BitBtn6Click(Sender: TObject);
begin
   GetNewFile(2,Label3);
end;

procedure TPLSSform.BitBtn7Click(Sender: TObject);
begin
   GetNewFile(3,Label4);
end;


procedure TPLSSform.BitBtn8Click(Sender: TObject);
begin
   GetNewFile(4,Label5);
end;

procedure TPLSSform.BitBtn9Click(Sender: TObject);
begin
   GetNewFile(5,Label6);
end;


initialization
finalization
end.
