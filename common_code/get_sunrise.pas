unit get_sunrise;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

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
begin
   Edit1.Text := IntToStr(UsersFavoriteDay);
   Edit2.Text := IntToStr(UsersFavoriteMonth);
   Edit3.Text := IntToStr(UsersFavoriteYear);
   Edit4.Text := IntToStr(MDDef.UTCOffset);
end;


procedure Tsunrisepicker.OKBtnClick(Sender: TObject);
begin
   Close;
end;


end.
