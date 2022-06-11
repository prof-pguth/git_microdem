unit kml_overlay;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
  //{$Define KMLProblems}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TKML_over_opts = class(TForm)
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    RadioGroup2: TRadioGroup;
    Panel1: TPanel;
    Label1: TLabel;
    Transparent: TLabel;
    TrackBar1: TTrackBar;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    AllMapsCheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    Edit8: TEdit;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Edit9: TEdit;
    CheckBox5: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CheckOptions;
  end;


implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,DEMDefs;


procedure TKML_over_opts.BitBtn3Click(Sender: TObject);
begin
   Petmar.GetFileFromDirectory('Upper left','*.gif;*.png',KMLLogo1FileName);
   FormCreate(Sender);
end;

procedure TKML_over_opts.BitBtn4Click(Sender: TObject);
begin
   Petmar.GetFileFromDirectory('Lower left logo','*.gif;*.png',KMLLogo2FileName);
   FormCreate(Sender);
end;

procedure TKML_over_opts.CheckBox1Click(Sender: TObject);
begin
   CheckOptions;
end;

procedure TKML_over_opts.CheckOptions;
begin
   TrackBar1.Enabled := CheckBox1.Checked and (RadioGroup1.ItemIndex = 1);
   MDDef.TransparentGIF := CheckBox1.Checked;
   MDDef.TransparentPNG := CheckBox3.Checked;
end;

procedure TKML_over_opts.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TKML_over_opts.FormCreate(Sender: TObject);
begin
   {$IfDef KMLProblems} WriteLineToDebugFile('TKML_over_opts.FormCreate in'); {$EndIf}

   Edit8.Text := KMLLogo1FileName;
   Edit9.Text := KMLLogo2FileName;
   if MDDef.UseGif then begin
      RadioGroup1.ItemIndex := 0;
   end
   else begin
      RadioGroup1.ItemIndex := 1;
   end;
   RadioGroup2.ItemIndex := MDDef.KMLOutputOption;

   TrackBar1.Position := MDDef.TransparentLevel;

   CheckOptions;
   CheckBox1.Checked := MDDef.TransparentGIF;
   CheckBox2.Checked := MDDef.KMLExportLayers;
   CheckBox3.Checked := MDDef.TransparentPNG;
   CheckBox4.Checked := MDDef.ZipKMLfiles;
   CheckBox5.Checked := MDDef.ShowKMLFile;
   CheckBox6.Checked := MDDef.KMLCreateWorldFiles;
   CheckBox7.Checked := MDDef.KMLOpenGoogleEarth;
   CheckBox8.Checked := MDDef.KMLExportSeparateFans;
   CheckBox9.Checked := MDDef.KMLTimeAnimations;
   CheckBox10.Checked := MDDef.KMLDefaultDB;
   CheckBox11.Checked := MDDef.CleanUpHTML;
   CheckBox13.Checked := MDDef.KMLLabelGraticule;
   CheckBox14.Checked := MDDef.KMLTopLevelFolder;
   AllMapsCheckBox3.Checked := MDDef.KMLExportAllMaps;
   {$IfDef KMLProblems} WriteLineToDebugFile('TKML_over_opts.FormCreate  out');  {$EndIf}
end;

procedure TKML_over_opts.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html/kml_overlay.htm');
end;

procedure TKML_over_opts.OKBtnClick(Sender: TObject);
begin
   MDDef.KMLExportLayers := CheckBox2.Checked;

   MDDef.ZipKMLfiles := CheckBox4.Checked;
   MDDef.ShowKMLFile:= CheckBox5.Checked;
   MDDef.KMLCreateWorldFiles := CheckBox6.Checked;
   MDDef.KMLOpenGoogleEarth := CheckBox7.Checked;
   MDDef.KMLExportSeparateFans := CheckBox8.Checked;

   MDDef.KMLTimeAnimations := CheckBox9.Checked;
   MDDef.KMLDefaultDB := CheckBox10.Checked;
   MDDef.CleanUpHTML := CheckBox11.Checked;
   MDDef.KML_DB_tables := CheckBox12.Checked;
   MDDef.KMLLabelGraticule := CheckBox13.Checked;
   MDDef.KMLTopLevelFolder := CheckBox14.Checked;

   MDDef.KMLExportAllMaps := AllMapsCheckBox3.Checked;

   {$IfDef KMLProblems} WriteLineToDebugFile('Options set');  {$EndIf}

   MDDef.UseGif := (RadioGroup1.ItemIndex = 0);
   MDDef.KMLOutputOption := RadioGroup2.ItemIndex;
   MDDef.TransparentLevel := TrackBar1.Position;

   Close;
end;

procedure TKML_over_opts.RadioGroup1Click(Sender: TObject);
begin
   CheckOptions;
end;

procedure TKML_over_opts.RadioGroup2Click(Sender: TObject);
begin
   Panel1.Enabled := RadioGroup2.ItemIndex in [0,1];
   MDDef.KMLOutputOption := RadioGroup2.ItemIndex;
end;


initialization
finalization
   {$IfDef KMLProblems} WriteLineToDebugFile('KMLProblems active in kml_overlay'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing kml_overlay');  {$EndIf}
end.
