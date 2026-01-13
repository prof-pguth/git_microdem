unit check_8_dirs;

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
   Windows, Classes, Graphics, Forms, Controls, Buttons,StdCtrls, ExtCtrls, ColorGrd, Dialogs,
   DEMDefs;

type
  TGetDir8 = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetDirectionsToUse(var Aspects : tDirToUse) : boolean;


implementation

{$R *.DFM}

uses
   Nevadia_Main,
   PETMAR,Petmar_types,PETMath;

function GetDirectionsToUse;
var
  GetDir8: TGetDir8;
begin
   GetDir8 := tGetDir8.Create(Application);
   with GetDir8 do begin
      CheckBox1.Checked := Aspects[1];
      CheckBox2.Checked := Aspects[2];
      CheckBox3.Checked := Aspects[3];
      CheckBox4.Checked := Aspects[4];
      CheckBox5.Checked := Aspects[5];
      CheckBox6.Checked := Aspects[6];
      CheckBox7.Checked := Aspects[7];
      CheckBox8.Checked := Aspects[8];
      if (GetDir8.ShowModal = mrCancel) then begin
         Result := false;
      end
      else begin
         Result := true;
         Aspects[1] := CheckBox1.Checked;
         Aspects[2] := CheckBox2.Checked;
         Aspects[3] := CheckBox3.Checked;
         Aspects[4] := CheckBox4.Checked;
         Aspects[5] := CheckBox5.Checked;
         Aspects[6] := CheckBox6.Checked;
         Aspects[7] := CheckBox7.Checked;
         Aspects[8] := CheckBox8.Checked;
      end;
   end;
end;


procedure TGetDir8.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
end;


end.
