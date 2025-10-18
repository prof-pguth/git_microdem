unit sc_colopts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

interface

uses
   Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
   Dialogs, StdCtrls, ExtCtrls, Buttons,
   PETMAR;

type
  TStratOptsForm = class(TForm)
    GroupBox2: TGroupBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    GroupBox3: TGroupBox;
    CheckBox3: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox11: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    CheckBox12: TCheckBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit5: TEdit;
    Label5: TLabel;
    RadioGroup3: TRadioGroup;
    OKBtn: TBitBtn;
    RadioGroup4: TRadioGroup;
    RadioGroup5: TRadioGroup;
    RadioGroup6: TRadioGroup;
    BitBtn1: TBitBtn;
    CheckBox10: TCheckBox;
    RadioGroup7: TRadioGroup;
    RadioGroup8: TRadioGroup;
    Edit6: TEdit;
    Label6: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure SetStratcolOptions;


implementation

{$R *.dfm}

uses
   Nevadia_Main,
   DEMDefs,
   Petmar_types,
   Petmar_ini_file,
   Demdef_routines;


procedure SetStratcolOptions;
var
   StratOptsForm: TStratOptsForm;
begin
   StratOptsForm := TStratOptsForm.Create(Application);
   StratOptsForm.ShowModal;
end;


procedure TStratOptsForm.FormCreate(Sender: TObject);
begin
  {$IfDef ExStratcol}
  {$Else}
   OKbtn.Caption := 'OK';
   PlaceFormAtMousePosition(Self);

    CheckBox1.Checked := MDDef.ColDef.AbsThickness;
    CheckBox2.Checked := MDDef.ColDef.RightSideThickness;
    CheckBox3.Checked := MDDef.ColDef.AutoShortLabels;
    CheckBox4.Checked := MDDef.ColDef.BoundaryTicks;
    CheckBox5.Checked := MDDef.ColDef.RaggedRightMargin;
    CheckBox6.Checked := MDDef.ColDef.VariableResistance;
    CheckBox7.Checked := MDDef.ColDef.OverPrintLithology;
    CheckBox8.Checked := MDDef.ColDef.FancyUnitBases;
    CheckBox9.Checked := MDDef.ColDef.EnterLatLong;
    CheckBox10.Checked := MDDef.ColDef.ColumnVerbiage;
    CheckBox11.Checked := MDDef.ColDef.RapidColEntry;
    CheckBox12.Checked := MDDef.ColDef.ShowAgeBar;
    RadioGroup1.ItemIndex := ord(MDDef.ColDef.ThickLabelUnits);
    RadioGroup2.ItemIndex := ord(MDDef.ColDef.ThickLabelling);
    RadioGroup3.ItemIndex := ord(MDDef.ColDef.ColorAndPatternOptions);
    RadioGroup4.ItemIndex := ord(MDDef.ColDef.TextDirection);
    RadioGroup5.ItemIndex := ord(MDDef.ColDef.TextPlacement);
    RadioGroup6.ItemIndex := ord(MDDef.ColDef.TextLabels);
    RadioGroup7.ItemIndex := ord(MDDef.ColDef.AlignColumns);
    RadioGroup8.ItemIndex := ord(MDDef.ColDef.LocationLabel);

    Edit1.Text := IntToStr(MDDef.ColDef.PixelsColumnWide);
    Edit2.Text := IntToStr(MDDef.ColDef.ColumnSeparation);
    Edit3.Text := RealToString(MDDef.ColDef.DefaultThickness,-8,-2);
    Edit4.Text := IntToStr(MDDef.ColDef.DefaultMyBitmapWidth);
    Edit5.Text := IntToStr(MDDef.ColDef.DefaultMyBitmapHeight);
{$EndIf}
end;


procedure TStratOptsForm.OKBtnClick(Sender: TObject);
begin
     MDDef.ColDef.AbsThickness := CheckBox1.Checked;
     MDDef.ColDef.RightSideThickness := CheckBox2.Checked;
     MDdef.ColDef.AutoShortLabels := CheckBox3.Checked;
     MDdef.ColDef.BoundaryTicks := CheckBox4.Checked;
     MDdef.ColDef.RaggedRightMargin := CheckBox5.Checked;
     MDdef.ColDef.VariableResistance := CheckBox6.Checked;
     MDdef.ColDef.OverPrintLithology := CheckBox7.Checked;
     MDdef.ColDef.FancyUnitBases := CheckBox8.Checked;
     MDdef.ColDef.EnterLatLong := CheckBox9.Checked;
     MDdef.ColDef.ColumnVerbiage := CheckBox10.Checked;
     MDdef.ColDef.RapidColEntry := CheckBox11.Checked;
     MDdef.ColDef.ShowAgeBar := CheckBox12.Checked;
     MDdef.ColDef.ThickLabelUnits := tLabelUnits(RadioGroup1.ItemIndex);
     MDdef.ColDef.ThickLabelling := ThickLabelOption (RadioGroup2.ItemIndex);
     MDdef.ColDef.ColorAndPatternOptions := tColorAndPatternOptions(RadioGroup3.ItemIndex);
     MDdef.ColDef.TextDirection := tTextDirection(RadioGroup4.ItemIndex);
     MDdef.ColDef.TextPlacement := tTextPlacement(RadioGroup5.ItemIndex);
     MDdef.ColDef.TextLabels := tTextLabels(RadioGroup6.ItemIndex);
     MDdef.ColDef.AlignColumns := tAlignColumns(RadioGroup7.ItemIndex);
     MDdef.ColDef.LocationLabel := tLocationLabel(RadioGroup8.ItemIndex);
    CheckEditString(Edit1.Text,MDDef.ColDef.PixelsColumnWide);
    CheckEditString(Edit2.Text,MDDef.ColDef.ColumnSeparation);
    CheckEditString(Edit3.Text,MDDef.ColDef.DefaultThickness);
    CheckEditString(Edit4.Text,MDDef.ColDef.DefaultMyBitmapWidth);
    CheckEditString(Edit5.Text,MDDef.ColDef.DefaultMyBitmapHeight);
    CheckEditString(Edit6.Text,MDDef.ColDef.ScaleLabelOffset);
   Close;
end;


procedure TStratOptsForm.BitBtn1Click(Sender: TObject);
begin
   ProcessIniFile(iniInit,'Stratcol');
   FormCreate(Nil);
end;


initialization
finalization
end.
