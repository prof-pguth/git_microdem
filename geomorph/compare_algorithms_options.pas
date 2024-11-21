unit compare_algorithms_options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TAlgCompareForm = class(TForm)
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    BitBtn38: TBitBtn;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
  private
    { Private declarations }
    procedure CheckSettings;
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
   DEMDefs,DEMdef_routines,Petmar,Petmar_types;

procedure TAlgCompareForm.BitBtn38Click(Sender: TObject);
begin
   CheckSettings;
   SaveMDDefaults;
   close;
end;


procedure TAlgCompareForm.FormCreate(Sender: TObject);
begin
   CheckBox1.Checked := MDDef.CalcR;
   CheckBox2.Checked := MDDef.CalcMAbD;
   CheckBox3.Checked := MDDef.CalcMAvD;
   CheckBox4.Checked := MDDef.CalcScattergrams;
   CheckBox5.Checked := MDDef.CalcHistograms;
   CheckBox6.Checked := MDDef.UseCalculatedOutside;
   CheckBox7.Checked := MDDef.CompareShowMaps;
   CheckBox8.Checked := MDDef.GDAL_SAGA_arcsec;
   CheckBox9.Checked := MDDef.CalcDiffMaps;
   CheckBox10.Checked := MDDef.CalcBoxPlots;
   CheckBox11.Checked := MDDef.CloseGridsAfterComputing;
end;

procedure TAlgCompareForm.CheckSettings;
begin
   MDDef.CalcR := CheckBox1.Checked;
   MDDef.CalcMAbD := CheckBox2.Checked;
   MDDef.CalcMAvD := CheckBox3.Checked;
   MDDef.CalcScattergrams := CheckBox4.Checked;
   MDDef.CalcHistograms := CheckBox5.Checked;
   MDDef.UseCalculatedOutside := CheckBox6.Checked;
   MDDef.CompareShowMaps := CheckBox7.Checked;
   MDDef.GDAL_SAGA_arcsec := CheckBox8.Checked;
   MDDef.CalcDiffMaps := CheckBox9.Checked;
   MDDef.CalcBoxPlots := CheckBox10.Checked;
   MDDef.CloseGridsAfterComputing := CheckBox11.Checked;
end;


procedure TAlgCompareForm.OKBtnClick(Sender: TObject);
begin
   CheckSettings;
   Close;
end;


end.
