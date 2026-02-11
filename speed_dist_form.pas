unit speed_dist_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

interface

uses
   Windows, Messages, SysUtils,  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TSpeedDistanceForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    CheckBox5: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
   Nevadia_Main,
   Petmar,DEMDefs;


procedure TSpeedDistanceForm.FormCreate(Sender: TObject);
begin
    CheckBox1.Checked := MDdef.AddAzimuth;
    CheckBox2.Checked := MDdef.AddSpeed;
    CheckBox3.Checked := MDdef.AddDist;
    CheckBox4.Checked := MDdef.AddCumDist;
    CheckBox5.Checked := MDdef.Add3DDist;
    if MDDef.UseMeters then RadioGroup1.ItemIndex := 0 else RadioGroup1.ItemIndex := 1;
    PlaceFormAtMousePosition(Self);
end;


procedure TSpeedDistanceForm.OKBtnClick(Sender: TObject);
begin
    MDdef.AddAzimuth := CheckBox1.Checked;
    MDdef.AddSpeed := CheckBox2.Checked;
    MDdef.AddDist := CheckBox3.Checked;
    MDdef.AddCumDist := CheckBox4.Checked;
    MDdef.Add3DDist := CheckBox5.Checked;
    MDDef.UseMeters := RadioGroup1.ItemIndex = 0;
    Close;
end;


end.
