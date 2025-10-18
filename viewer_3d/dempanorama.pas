unit dempanorama;

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

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, OkCancl2;

type
  TPanoramaOps = class(TOKRightDlg)
    HelpBtn: TButton;
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    ComboBox1: TComboBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
   PanoramaOps: TPanoramaOps;

implementation

{$R *.DFM}

uses
   Nevadia_Main,
   PETMAR,DEMDefs;


procedure TPanoramaOps.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme2515.htm');
end;


procedure TPanoramaOps.ComboBox1Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox1.Items.Count) do
      if ComboBox1.Text = ComboBox1.Items[i] then
         MDdef.PerspOpts.WhichPerspective := tPerspectiveType(i);
   ComboBox1.Text := ComboBox1.Items[ord(MDdef.PerspOpts.WhichPerspective)];
end;

procedure TPanoramaOps.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
end;


end.

