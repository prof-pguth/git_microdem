unit usoutlines;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TUSOutlineForm = class(TForm)
    OKBtn: TBitBtn;
    Edit2: TEdit;
    Edit3: TEdit;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn5: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure BitBtn5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure SetUSOutlines;

implementation

{$R *.dfm}

uses
   Nevadia_Main,
   DEMDefs,
   PETMAR,Petmar_types;


procedure SetUSOutlines;
var
   USOutlineForm : TUSOutlineForm;
begin
   USOutlineForm := tUSOutlineForm.Create(Application);
   with USOUtlineForm do begin
      Edit2.Text := IntToStr(MDDef.PixelSizeToShowCounties);
      Edit3.Text := IntToStr(MDDef.PixelSizeToShowStates);
      CheckBox1.Checked := MDDef.USOutlinesOnDEMs;
      CheckBox2.Checked := MDDef.USOutlinesOnImagery;
      ColorLineWidthBitBtn(BitBtn2,MDDef.US_CountyOutline_Color,MDDef.US_CountyOutline_Width);
      ColorLineWidthBitBtn(BitBtn3,MDDef.US_StateOutline_Color,MDDef.US_StateOutline_Width);
      //ColorLineWidthBitBtn(BitBtn4,MDDef.US_FennemanColor,MDDef.US_FennemanWidth);
      ColorLineWidthBitBtn(BitBtn6,MDDef.US_Highway_Color,MDDef.US_Highway_Width);
      ColorLineWidthBitBtn(BitBtn7,MDDef.US_River_Color,MDDef.US_River_Width);

      CheckBox5.Checked := MDDef.US_Show_Counties;
      CheckBox6.Checked := MDDef.US_Show_States;
      CheckBox7.Checked := MDDef.US_Show_Roads;
      CheckBox8.Checked := MDDef.US_Show_Rivers;

      Label1.Caption := CountyGISFileName;
      Label2.Caption := StateGISFileName;
      Label3.Caption := HighwayGISFileName;
      Label5.Caption := RiversGISFileName;

      ShowModal;
      CheckEditString(Edit2.Text,MDDef.PixelSizeToShowCounties);
      CheckEditString(Edit3.Text,MDDef.PixelSizeToShowStates);
      MDDef.USOutlinesOnDEMs := CheckBox1.Checked;
      MDDef.USOutlinesOnImagery := CheckBox2.Checked;

      MDDef.US_Show_Counties := CheckBox5.Checked;
      MDDef.US_Show_States := CheckBox6.Checked;
      MDDef.US_Show_Roads := CheckBox7.Checked;
      MDDef.US_Show_Rivers := CheckBox8.Checked;
      Free;
   end;
end;


procedure TUSOutlineForm.BitBtn10Click(Sender: TObject);
begin
   GetFileFromDirectory('Rivers SHP','*.shp',RiversGISFileName);
   Label5.Caption := RiversGISFileName;
end;

procedure TUSOutlineForm.BitBtn1Click(Sender: TObject);
begin
   GetFileFromDirectory('County SHP','*.shp',CountyGISFileName);
   Label1.Caption := CountyGISFileName;
end;

procedure TUSOutlineForm.BitBtn2Click(Sender: TObject);
begin
   PickLineSizeAndColor('County outlines',BitBtn2,MDDef.US_CountyOutline_Color,MDDef.US_CountyOutline_Width);
end;


procedure TUSOutlineForm.BitBtn3Click(Sender: TObject);
begin
   PickLineSizeAndColor('State outlines',BitBtn3,MDDef.US_StateOutline_Color,MDDef.US_StateOutline_Width);
end;

procedure TUSOutlineForm.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
end;


procedure TUSOutlineForm.BitBtn5Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\us_smart_outlines.htm');
end;


procedure TUSOutlineForm.BitBtn6Click(Sender: TObject);
begin
   PickLineSizeAndColor('Highways',BitBtn6,MDDef.US_Highway_Color,MDDef.US_Highway_Width);
end;

procedure TUSOutlineForm.BitBtn7Click(Sender: TObject);
begin
   PickLineSizeAndColor('Rivers',BitBtn7,MDDef.US_River_Color,MDDef.US_River_Width);
end;

procedure TUSOutlineForm.BitBtn8Click(Sender: TObject);
begin
   GetFileFromDirectory('State SHP','*.shp',StateGISFileName);
   Label2.Caption := StateGISFileName;
end;

procedure TUSOutlineForm.BitBtn9Click(Sender: TObject);
begin
   GetFileFromDirectory('Road SHP','*.shp',HighwayGISFileName);
   Label3.Caption := HighwayGISFileName;
end;

initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing us_outlines'); {$EndIf}
end.



