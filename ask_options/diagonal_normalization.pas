unit diagonal_normalization;


{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons;

type
  TPickDiagNormForm = class(TForm)
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

procedure PickDiagonalNormalization(var Method : byte);
var
  TheForm : TPickDiagNormForm;
begin
  TheForm := TPickDiagNormForm.Create(Application);
  TheForm.RadioGroup1.ItemIndex := Method;
  TheForm.ShowModal;
  Method := TheForm.RadioGroup1.ItemIndex;
  TheForm.Destroy;
end;


{$R *.dfm}

end.
