unit dem_digit_opts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TDEMDigitOptions = class(TForm)
    Label25: TLabel;
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn1: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure SetDigitizingOptions;


implementation

{$R *.dfm}


uses
   Petmar,Petmar_types,DEMDefs;


procedure SetDigitizingOptions;
var
   DEMDigitOptions : TDEMDigitOptions;
begin
   DEMDigitOptions := TDEMDigitOptions.Create(Application);
   DEMDigitOptions.ShowModal;
   DEMDigitOptions.Free;
end;


procedure TDEMDigitOptions.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   RadioGroup1.ItemIndex := ord(MDDef.DigitizeMode);
   Edit1.Text := IntToStr(MDDef.ContDigitizeSeparation);
   ColorLineWidthBitBtn(BitBtn1,MDDef.DigitizeColor,MDDef.DigitizeWidth);
end;


procedure TDEMDigitOptions.OKBtnClick(Sender: TObject);
begin
   MDDef.DigitizeMode := tDigitizeMode(RadioGroup1.ItemIndex);
   CheckEditString(Edit1.Text,MDDef.ContDigitizeSeparation);
   Close;
end;

procedure TDEMDigitOptions.BitBtn1Click(Sender: TObject);
begin
   PickLineSizeAndColor('digitizing',BitBtn1,MDDef.DigitizeColor,MDDef.DigitizeWidth);
end;


initialization
finalization
end.
