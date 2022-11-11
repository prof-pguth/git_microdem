unit demxyzim;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
   Windows, SysUtils, Classes, Graphics, Forms,   Buttons,
   Vcl.ExtCtrls,
   Petmar_types, OKCANCL1, Vcl.StdCtrls, Vcl.Controls;

type
  tDataType = (EastingNorthing,LatitudeLongitude,LongitudeLatitude);

  TImportParamsDialog = class(TOKBottomDlg)
    HelpBtn: TButton;
    Memo1: TMemo;
    MultLong1: TCheckBox;
    ZMult1: TCheckBox;
    RadioGroup2: TRadioGroup;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    CheckBox4: TCheckBox;
    RadioGroup1: TRadioGroup;
    Label3: TLabel;
    Edit2: TEdit;
    RadioGroup3: TRadioGroup;
    Label4: TLabel;
    Edit3: TEdit;
    Label5: TLabel;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ZMult1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     DataType : tDataType;
     ZMult : float64;
  end;

var
  ImportParamsDialog : TImportParamsDialog;


implementation

{$R *.DFM}

uses
   DEMDefs,BaseMap,
   PETMAR;

procedure TImportParamsDialog.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\demb7dyr.htm');
end;


procedure TImportParamsDialog.ComboBox1Change(Sender: TObject);
begin
   ZMult1Click(Sender);
end;

procedure TImportParamsDialog.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.DefaultUTMZone);
   Label5.Caption := UTMZoneExtent(MDDef.DefaultUTMZone);
end;

procedure TImportParamsDialog.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.ImportThinFactor);
end;

procedure TImportParamsDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
   Self := Nil;
end;

procedure TImportParamsDialog.FormCreate(Sender: TObject);
begin
   inherited;
   Petmar.PlaceFormAtMousePosition(Self);
   RadioGroup2.ItemIndex := ord(MDDef.XYZImport);
   RadioGroup3.ItemIndex := MDDef.XYZProduct;
   ZMult := 1;
   Edit2.Text := IntToStr(MDDef.DefaultUTMZone);
   Edit3.Text := IntToStr(MDDef.ImportThinFactor);
   Label5.Caption := UTMZoneExtent(MDDef.DefaultUTMZone);
   if (MDDef.DefaultLatHemi = 'N') then RadioGroup1.ItemIndex := 0 else RadioGroup1.ItemIndex := 1;
   {$IfDef HideHelpButtons}  HelpBtn.Visible := false;  {$EndIf}
end;


procedure TImportParamsDialog.RadioGroup1Click(Sender: TObject);
begin
   if (RadioGroup1.ItemIndex = 0) then MDDef.DefaultLatHemi := 'N' else MDDef.DefaultLatHemi := 'S';
end;

procedure TImportParamsDialog.RadioGroup2Click(Sender: TObject);
begin
   MDDef.XYZImport := tXYZImport(RadioGroup2.ItemIndex);
end;

procedure TImportParamsDialog.RadioGroup3Click(Sender: TObject);
begin
   MDDef.XYZProduct := RadioGroup3.ItemIndex;
end;

procedure TImportParamsDialog.ZMult1Click(Sender: TObject);
var
   err : integer;
begin
    Val(ComboBox1.Text,ZMult,err);
    if (err <> 0) then ZMult := 1;
    If ZMult1.Checked then ZMult := -ZMult;
    if CheckBox4.Checked then ZMult := ZMUlt * FeetToMeters;
end;


initialization
   ImportParamsDialog := Nil;
finalization
end.

