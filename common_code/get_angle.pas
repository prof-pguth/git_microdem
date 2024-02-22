unit get_angle;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
  //{$Define RecordGetAngleProblems}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Buttons, ExtCtrls,
  Petmar_types,Petmar;

type
  Tget_angle_form = class(TForm)
    Edit5: TEdit;
    RadioGroup4: TRadioGroup;
    BitBtn5: TBitBtn;
    procedure Edit5Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     Angle : float64;
  end;


implementation

{$R *.dfm}

uses
  DEMDefs;


procedure Tget_angle_form.Edit5Change(Sender: TObject);
begin
   if (Sender = Edit5) then begin
      CheckEditString(Edit5.Text,Angle);
      if abs(Angle) < 0.00000000001 then exit;
      if (RadioGroup4.ItemIndex = 1) then Angle := Angle / 60;
      if (RadioGroup4.ItemIndex = 2) then Angle := Angle / 3600;
   end;
   {$IfDef RecordGetAngleProblems}
   WriteLineToDebugFile('Tget_angle_form.Edit5Change, angle=' + RealToString(Angle,-12,-6));
   {$EndIf}
end;


procedure Tget_angle_form.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   Height := 157;
   Width := 184;
end;


procedure Tget_angle_form.RadioGroup4Click(Sender: TObject);
begin
   Edit5.Text := AngleFormat(Angle,MDDef.GraticuleUnits,false);
   {$IfDef RecordGetAngleProblems}
   WriteLineToDebugFile('Tget_angle_form.RadioGroup4Click, angle=' + RealToString(Angle,-12,-6));
   {$EndIf}
end;


procedure Tget_angle_form.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure Tget_angle_form.BitBtn5Click(Sender: TObject);
begin
   Close;
end;


initialization
finalization
   {$IfDef RecordGetAngleProblems} WriteLineToDebugFile('RecordGetAngleProblems active in GetAngle'); {$EndIf}
end.

