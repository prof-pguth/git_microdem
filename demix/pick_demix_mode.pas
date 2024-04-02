unit pick_demix_mode;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
{$EndIf}



interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons;

type
  TPickDEMIXmodeForm = class(TForm)
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure PickDEMIXMode(var DEMIX_Mode : integer);


implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,DEMIX_definitions;

procedure PickDEMIXMode(var DEMIX_Mode : integer);
var
  PickDEMIXmodeForm: TPickDEMIXmodeForm;
begin
  {$IfDef RecordDEMIX} WriteLineToDebugFile('PickDEMIXMode in, DEMIX_mode=' + IntToStr(DEMIX_mode)); {$EndIf}
  PickDEMIXmodeForm := TPickDEMIXmodeForm.Create(Application);
  PickDEMIXmodeForm.RadioGroup1.ItemIndex := DEMIX_mode;
  PickDEMIXmodeForm.ShowModal;
  DEMIX_mode := PickDEMIXmodeForm.RadioGroup1.ItemIndex;
  if DEMIX_Mode = dmNotYetDefined then begin
     DEMIX_Mode := dmClassic;
     MessageToContinue('Set to classic; it has to be defined');
  end;
  PickDEMIXmodeForm.Destroy;
  {$IfDef RecordDEMIX} WriteLineToDebugFile('PickDEMIXMode out'); {$EndIf}
end;


procedure TPickDEMIXmodeForm.BitBtn1Click(Sender: TObject);
begin
   Close;
end;



end.
