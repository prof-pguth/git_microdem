unit demvarop;

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

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, OkCancl2;

type
  TVariogramOptions = class(TOKRightDlg)
    HelpBtn: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Label4: TLabel;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VariogramOptions: TVariogramOptions;

implementation

{$R *.DFM}

uses
   Nevadia_Main,
   Petmar;

procedure TVariogramOptions.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\micr1hv1.htm');
end;

procedure TVariogramOptions.CheckBox6Click(Sender: TObject);
begin
   Label2.Enabled := CheckBox6.Checked;
   Edit2.Enabled := CheckBox6.Checked;
end;

procedure TVariogramOptions.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
   CheckBox6Click(Sender);
end;

end.

