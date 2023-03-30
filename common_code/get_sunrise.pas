unit get_sunrise;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  Tsunrisepicker = class(TForm)
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
   DEMDefs;

procedure Tsunrisepicker.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure Tsunrisepicker.FormCreate(Sender: TObject);
var
   wYear,wmonth,wDay : word;
begin
   DecodeDate(now,wYear,wmonth,wDay);
   Edit1.Text := IntToStr(wDay);
   Edit2.Text := IntToStr(wMonth);
   Edit3.Text := IntToStr(wYear);
   Edit4.Text := IntToStr(MDDef.UTCOffset);
end;


procedure Tsunrisepicker.OKBtnClick(Sender: TObject);
begin
   Close;
end;

end.
