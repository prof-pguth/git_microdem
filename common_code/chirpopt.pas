unit ChirpOpt;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,Buttons, ExtCtrls, OkCancl2;

type
  TC = class(TOKRightDlg)
    HelpBtn: TButton;
    Edit3: TEdit;
    Edit4: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    RadioGroup2: TRadioGroup;
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Edit1: TEdit;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.DFM}

procedure TC.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
   Action := caFree;
end;

procedure TC.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

initialization
finalization
end.

