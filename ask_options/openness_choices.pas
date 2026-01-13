unit openness_choices;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  DEMmapF;

type
  TOpenOptForm = class(TForm)
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox2: TGroupBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    Label3: TLabel;
    Edit3: TEdit;
    HelpBtn: TBitBtn;
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
      MapOwner : tMapform;
  public
    { Public declarations }
  end;


procedure GetOptionsDrawOpenness(inMapOwner : tMapForm);

implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,
   DEMDEFs,DEMDef_routines,DEMcoord,
   Make_Grid;

procedure GetOptionsDrawOpenness(inMapOwner : tMapForm);
var
   OpenOptForm: TOpenOptForm;
begin
   OpenOptForm := TOpenOptForm.Create(Application);
   OpenOptForm.MapOwner := inMapOwner;
   OpenOptForm.ShowModal;

end;


procedure TOpenOptForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.OpennessBoxRadiusMeters);
end;

procedure TOpenOptForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.OpennessBoxRadiusPixels);
end;


procedure TOpenOptForm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.OpenStartRadialsAtPixel);
end;

procedure TOpenOptForm.FormCreate(Sender: TObject);
begin
   Edit1.Text := IntToStr(MDDef.OpennessBoxRadiusMeters);
   Edit2.Text := IntToStr(MDDef.OpennessBoxRadiusPixels);
   Edit3.Text := IntToStr(MDDef.OpenStartRadialsAtPixel);

   //RadioGroup2.ItemIndex := pred(MDDef.OpenStartRadialsAtPixel);
   RadioGroup1.ItemIndex := MDDef.OpenRadiusUnits;

    CheckBox1.Checked := MDDef.OpennessDirs[1];
    CheckBox2.Checked := MDDef.OpennessDirs[2];
    CheckBox3.Checked := MDDef.OpennessDirs[3];
    CheckBox4.Checked := MDDef.OpennessDirs[4];
    CheckBox5.Checked := MDDef.OpennessDirs[5];
    CheckBox6.Checked := MDDef.OpennessDirs[6];
    CheckBox7.Checked := MDDef.OpennessDirs[7];
    CheckBox8.Checked := MDDef.OpennessDirs[8];
end;

procedure TOpenOptForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html/openness_opts.htm');
end;

procedure TOpenOptForm.RadioGroup1Click(Sender: TObject);
begin
   MDDef.OpenRadiusUnits := RadioGroup1.ItemIndex;
   Label1.Enabled := MDDef.OpenRadiusUnits = 0;
   Edit1.Enabled := MDDef.OpenRadiusUnits = 0;
   Label2.Enabled := MDDef.OpenRadiusUnits = 1;
   Edit2.Enabled := MDDef.OpenRadiusUnits = 1;
end;

procedure TOpenOptForm.RedrawSpeedButton12Click(Sender: TObject);
var
   Upward,Downward,Difference,
   Meters,Pixels : integer;
begin
   MDDef.OpennessDirs[1] := CheckBox1.Checked;
   MDDef.OpennessDirs[2] := CheckBox2.Checked;
   MDDef.OpennessDirs[3] := CheckBox3.Checked;
   MDDef.OpennessDirs[4] := CheckBox4.Checked;
   MDDef.OpennessDirs[5] := CheckBox5.Checked;
   MDDef.OpennessDirs[6] := CheckBox6.Checked;
   MDDef.OpennessDirs[7] := CheckBox7.Checked;
   MDDef.OpennessDirs[8] := CheckBox8.Checked;

   if (RadioGroup1.ItemIndex = 0) then begin
      Meters := MDDef.OpennessBoxRadiusMeters;
      Pixels := -99;
   end
   else begin
      Pixels := MDDef.OpennessBoxRadiusPixels;
      Meters := -99;
   end;

    if CheckBox9.Checked then Upward := -1 else Upward := 0;
    if CheckBox10.Checked then DownWard  := -1 else Downward := 0;
    if CheckBox11.Checked then Difference := -1 else Difference := 0;
    CreateOpennessMap(true,MapOwner.MapDraw.MapAreaDEMGridLimits,MapOwner.MapDraw.DEMonMap,Meters,Pixels,Upward,DownWard,Difference);
    if MapOwner.FullMapSpeedButton.Enabled then begin
        if ValidDEM(Upward) then MatchAnotherMapThisCoverageArea(MapOwner,DEMGlb[Upward].SelectionMap);
        if ValidDEM(Downward) then MatchAnotherMapThisCoverageArea(MapOwner,DEMGlb[Downward].SelectionMap);
        if ValidDEM(Difference) then MatchAnotherMapThisCoverageArea(MapOwner,DEMGlb[Difference].SelectionMap);
    end;
end;



end.
