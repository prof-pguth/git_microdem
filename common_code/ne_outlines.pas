unit ne_outlines;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordNaturalEarth}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  DEMMapf;

type
  TNEOutlineForm = class(TForm)
    OKBtn: TBitBtn;
    Edit2: TEdit;
    Edit3: TEdit;
    BitBtn5: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    CheckBox3: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    Label4: TLabel;
    BitBtn2: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Label5: TLabel;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    procedure BitBtn5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
  private
    { Private declarations }
    MapOwner : tMapForm;
  public
    { Public declarations }
  end;


procedure SetNEOutlines(MapOwner : tMapForm);

implementation

{$R *.dfm}

uses
   DEMDefs,DEMDataBase,
   DEM_Manager,
   PETMAR,Petmar_types;


procedure SetNEOutlines;
var
   USOutlineForm : TNEOutlineForm;
begin
   {$IfDef RecordNaturalEarth} WriteLineToDebugFile('SetNEOutlines in'); {$EndIf}
   USOutlineForm := TNEOutlineForm.Create(Application);
   USOutlineForm.MapOwner := MapOwner;
   USOutlineForm.Edit2.Text := IntToStr(MDDef.SmallScaleWorldOutlinePixelSize);
   USOutlineForm.Edit3.Text := IntToStr(MDDef.MedScaleWorldOutlinePixelSize);
   USOutlineForm.Edit1.Text := IntToStr( MDDef.LargeScaleWorldOutlinePixelSize);
   USOutlineForm.CheckBox1.Checked := MDDef.NEAutoDEM;
   USOutlineForm.CheckBox2.Checked := MDDef.NEAutoSat;
   USOutlineForm.CheckBox3.Checked := MDDef.NEAutoScale;
   if (MapOwner = Nil) then begin
      USOutlineForm.CheckBox4.Enabled := false;
      USOutlineForm.CheckBox5.Enabled := false;
   end
   else begin
      USOutlineForm.CheckBox4.Checked := MapOwner.MapDraw.GrayscaleWorldOutline;
      USOutlineForm.CheckBox5.Checked := MapOwner.MapDraw.SubdueWorldOutline;
   end;

   USOutlineForm.Label1.Caption := SmallScaleWorldOutlines;
   USOutlineForm.Label2.Caption := MedScaleWorldOutlines;
   USOutlineForm.Label3.Caption := LargeScaleWorldOutlines;
   if (MapOwner <> Nil) then USOutlineForm.Label4.Caption := 'Current pixel size ' + SmartDistanceMetersFormat(MapOwner.MapDraw.ScreenPixelSize);
   USOutlineForm.ShowModal;
   USOutlineForm.Free;
   {$IfDef RecordNaturalEarth} WriteLineToDebugFile('SetNEOutlines out'); {$EndIf}
end;


procedure TNEOutlineForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
end;


procedure TNEOutlineForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   MapOwner.MapDraw.DeleteSingleMapLayer(MapOwner.MapDraw.NaturalEarthOverlayFName);
   MapOwner.DoFastMapRedraw;
end;

procedure TNEOutlineForm.BitBtn16Click(Sender: TObject);
begin
   OpenDBForModalEdit(SmallScaleWorldOutlines);
end;

procedure TNEOutlineForm.BitBtn1Click(Sender: TObject);
begin
   GetFileFromDirectory('Small scale vectors','*.dbf',SmallScaleWorldOutlines);
   Label1.Caption := SmallScaleWorldOutlines;
   OpenDBForModalEdit(SmallScaleWorldOutlines);
end;

procedure TNEOutlineForm.BitBtn2Click(Sender: TObject);
begin
   {$IfDef AllowUSNAdataDownloads}
      GetNaturalEarthData(True);
   {$EndIf}
end;

procedure TNEOutlineForm.BitBtn3Click(Sender: TObject);
begin
   OpenDBForModalEdit(MedScaleWorldOutlines);
end;

procedure TNEOutlineForm.BitBtn4Click(Sender: TObject);
begin
   OpenDBForModalEdit(LargeScaleWorldOutlines);
end;

procedure TNEOutlineForm.BitBtn5Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\us_smart_outlines.htm');
end;


procedure TNEOutlineForm.BitBtn8Click(Sender: TObject);
begin
   GetFileFromDirectory('Medium scale vectors','*.dbf',MedScaleWorldOutlines);
   Label2.Caption := MedScaleWorldOutlines;
   OpenDBForModalEdit(MedScaleWorldOutlines);
end;

procedure TNEOutlineForm.BitBtn9Click(Sender: TObject);
begin
   GetFileFromDirectory('Large scale vectors','*.dbf',LargeScaleWorldOutlines);
   Label2.Caption := LargeScaleWorldOutlines;
   OpenDBForModalEdit(LargeScaleWorldOutlines);
end;


procedure TNEOutlineForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.NEAutoDEM := CheckBox1.Checked;
   MDDef.NEAutoSat := CheckBox2.Checked;
end;

procedure TNEOutlineForm.CheckBox3Click(Sender: TObject);
begin
   MDDef.NEAutoScale := CheckBox3.Checked;
end;

procedure TNEOutlineForm.CheckBox4Click(Sender: TObject);
begin
   MapOwner.MapDraw.GrayscaleWorldOutline := CheckBox4.Checked;
end;

procedure TNEOutlineForm.CheckBox5Click(Sender: TObject);
begin
   MapOwner.MapDraw.SubdueWorldOutline := CheckBox5.Checked;
end;

procedure TNEOutlineForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.LargeScaleWorldOutlinePixelSize);
end;

procedure TNEOutlineForm.Edit2Change(Sender: TObject);
begin
    CheckEditString(Edit2.Text,MDDef.SmallScaleWorldOutlinePixelSize );
end;

procedure TNEOutlineForm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.MedScaleWorldOutlinePixelSize);
end;

initialization
finalization
   {$IfDef RecordNaturalEarth} WriteLineToDebugFile('RecordNaturalEarth active in ne_outlines'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing ne_outlines'); {$EndIf}
end.



