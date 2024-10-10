unit text_report_options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TReportOptionsForm = class(TForm)
    OKBtn: TBitBtn;
    RadioGroup1: TRadioGroup;
    HelpBtn: TBitBtn;
    Edit1: TEdit;
    Label4: TLabel;
    UpDown1: TUpDown;
    procedure Edit1Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    { Private declarations }
  public
    { Public declarations }
    tf : integer;
  end;


implementation

uses
   petmar;
{$R *.dfm}


procedure TReportOptionsForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,tf);
end;

procedure TReportOptionsForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TReportOptionsForm.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then inc(tf);
  if (Button = btPrev) and (tf > 1) then dec(tf);
  Edit1.Text := IntToStr(tf);
end;

procedure TReportOptionsForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   tf := 1;
end;


procedure TReportOptionsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\dd_text_rprt.htm');
end;

end.
