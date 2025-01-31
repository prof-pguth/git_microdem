unit demslopecompare;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DEMDefs,
  StdCtrls, Buttons, ExtCtrls;

type
  TSlopeCompareOptions = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    BitBtn1: TBitBtn;
    CheckBox2: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     SlopeMethod1,SlopeMethod2 : byte;
  end;


implementation

{$R *.DFM}

uses
   DEMDef_routines,PETMAR,DEMCoord;


procedure TSlopeCompareOptions.Button1Click(Sender: TObject);
begin
   PickSlopeAspectMethod('',SlopeMethod1);
   Label1.Caption := SlopeMethodName(SlopeMethod1);
end;


procedure TSlopeCompareOptions.Button2Click(Sender: TObject);
begin
   PickSlopeAspectMethod('',SlopeMethod2);
   Label2.Caption := SlopeMethodName(SlopeMethod2);
end;


procedure TSlopeCompareOptions.FormCreate(Sender: TObject);
begin
   Petmar.CheckFormPlacement(Self);
   SlopeMethod1 := smEvansYoung;
   SlopeMethod2 := smZevenbergenThorne;
   Label1.Caption := SlopeMethodName(SlopeMethod1);
   Label2.Caption := SlopeMethodName(SlopeMethod2);
end;


procedure TSlopeCompareOptions.BitBtn1Click(Sender: TObject);
begin
   Close;
end;

end.
