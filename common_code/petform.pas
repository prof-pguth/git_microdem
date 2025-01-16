unit petform;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Dialogs,
   SysUtils,
   StdCtrls, Buttons, ExtCtrls;

type
  TPETMARCommonForm = class(TForm)
    OpenDialog1: TOpenDialog;
    Edit1: TEdit;
    OKBtn: TBitBtn;
    SaveDialog1: TSaveDialog;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    Panel1: TPanel;
    procedure OKBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PETMARCommonForm : TPETMARCommonForm;

implementation

{$R *.DFM}

uses
   Petmar,
   Petmar_types;


procedure TPETMARCommonForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TPETMARCommonForm.FormActivate(Sender: TObject);
begin
   ActiveControl := Edit1;
   Left := Mouse.CursorPos.X;
   if (Left > Screen.Width - Width - 20) then Left := Screen.Width - Width - 20;
   Top := Mouse.CursorPos.Y;
   if (Top > Screen.Height - Height - 50) then Top := Screen.Height - Height - 50;

   //when running on other than monitor[0], this opens on the wrong monitor
   //OnMonitor := Screen.MonitorFromWindow(FindWindowExtd(BuildString)).MonitorNum;
   //Left := Left + Screen.Monitors[OnMonitor].Left;       //value looks correct 12/5/2017, but program hangs

end;


initialization
   {$IfDef MessageStartUpProblems} MessageToContinue('Open PETMARCommonForm'); {$EndIf}
finalization
end.

