unit fan_sens_opts;

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
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  Tfan_sens_form = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
   Nevadia_Main,
   Petmar;

procedure Tfan_sens_form.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\test_sensitivity.htm');
end;


procedure Tfan_sens_form.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure Tfan_sens_form.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
end;


end.
