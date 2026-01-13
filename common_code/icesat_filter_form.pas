unit icesat_filter_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons;

type
  TIcesat_filter = class(TForm)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    RadioGroup5: TRadioGroup;
    RadioGroup6: TRadioGroup;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Edit2: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure GetIcesatFilter;


implementation

{$R *.dfm}

uses
  DEMdefs,Petmar,
  Nevadia_Main;


procedure GetIcesatFilter;
var
   Icesat_filter : TIcesat_filter;
begin
   Icesat_filter := TIcesat_filter.Create(Application);
   InsureFormOnScreenCurrentLocation(Icesat_filter,Mouse.CursorPos.X,Mouse.CursorPos.Y);
   Icesat_filter.ShowModal;
end;

procedure TIcesat_filter.CheckBox9Click(Sender: TObject);
begin
    //MDDef.Icesat2.LoadDEMfromIndex := CheckBox9.Checked;
end;

procedure TIcesat_filter.FormCreate(Sender: TObject);
begin
    CheckBox1.Checked := MDDef.Icesat2.DoFilter;
    CheckBox2.Checked := MDDef.Icesat2.UseBeam[1];
    CheckBox3.Checked := MDDef.Icesat2.UseBeam[2];
    CheckBox4.Checked := MDDef.Icesat2.UseBeam[3];
    CheckBox5.Checked := MDDef.Icesat2.UseBeam[4];
    CheckBox6.Checked := MDDef.Icesat2.UseBeam[5];
    CheckBox7.Checked := MDDef.Icesat2.UseBeam[6];
    Edit1.Text := IntToStr(MDDef.Icesat2.Boxsize);
    Edit2.Text := IntToStr(MDDef.Icesat2.PCPtsRequired);
    RadioGroup1.ItemIndex := MDDef.Icesat2.BeamConfidence[1];
    RadioGroup2.ItemIndex := MDDef.Icesat2.BeamConfidence[2];
    RadioGroup3.ItemIndex := MDDef.Icesat2.BeamConfidence[3];
    RadioGroup4.ItemIndex := MDDef.Icesat2.BeamConfidence[4];
    RadioGroup5.ItemIndex := MDDef.Icesat2.BeamConfidence[5];
    RadioGroup6.ItemIndex := MDDef.Icesat2.BeamConfidence[6];
end;

procedure TIcesat_filter.OKBtnClick(Sender: TObject);
begin
    MDDef.Icesat2.DoFilter := CheckBox1.Checked;
    MDDef.Icesat2.UseBeam[1] := CheckBox2.Checked;
    MDDef.Icesat2.UseBeam[2] := CheckBox3.Checked;
    MDDef.Icesat2.UseBeam[3] := CheckBox4.Checked;
    MDDef.Icesat2.UseBeam[4] := CheckBox5.Checked;
    MDDef.Icesat2.UseBeam[5] := CheckBox6.Checked;
    MDDef.Icesat2.UseBeam[6] := CheckBox7.Checked;
    //MDDef.Icesat2.MustHaveAllThree := CheckBox8.Checked;
    CheckEditString(Edit1.Text,MDDef.Icesat2.Boxsize);
    CheckEditString(Edit2.Text,MDDef.Icesat2.PCPtsRequired);
    MDDef.Icesat2.BeamConfidence[1] := RadioGroup1.ItemIndex;
    MDDef.Icesat2.BeamConfidence[2] := RadioGroup2.ItemIndex;
    MDDef.Icesat2.BeamConfidence[3] := RadioGroup3.ItemIndex;
    MDDef.Icesat2.BeamConfidence[4] := RadioGroup4.ItemIndex;
    MDDef.Icesat2.BeamConfidence[5] := RadioGroup5.ItemIndex;
    MDDef.Icesat2.BeamConfidence[6] := RadioGroup6.ItemIndex;
end;




end.
