unit geomorph_compare_algorithms;

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
    GroupBox2: TGroupBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    BitBtn38: TBitBtn;
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

   Edit1.Text := RealToString(MDDef.PerfectR,-12,-6);
   Edit2.Text := RealToString(MDDef.PerfectMAbD,-12,-6);
   Edit3.Text := RealToString(MDDef.PerfectMAvD,-12,-6);
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

   CheckEditString(Edit1.Text,MDDef.PerfectR);
   CheckEditString(Edit2.Text,MDDef.PerfectMAbD);
   CheckEditString(Edit3.Text,MDDef.PerfectMAvD);
end;


procedure TAlgCompareForm.OKBtnClick(Sender: TObject);
begin
(*
   MDDef.CalcR := CheckBox1.Checked;
   MDDef.CalcMAbD := CheckBox2.Checked;
   MDDef.CalcMAvD := CheckBox3.Checked;
   MDDef.CalcScattergrams := CheckBox4.Checked;
   MDDef.CalcHistograms := CheckBox5.Checked;
   MDDef.UseCalculatedOutside := CheckBox6.Checked;
   MDDef.CompareShowMaps := CheckBox7.Checked;
   MDDef.GDAL_SAGA_arcsec := CheckBox8.Checked;

   CheckEditString(Edit1.Text,MDDef.PerfectR);
   CheckEditString(Edit2.Text,MDDef.PerfectMAbD);
   CheckEditString(Edit3.Text,MDDef.PerfectMAvD);
*)
   CheckSettings;
   Close;
end;

end.
