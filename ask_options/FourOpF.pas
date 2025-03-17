unit FourOpF;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
   Buttons, ExtCtrls,
   OKCancl1;

type
  TFourierOptionsForm = class(TOKBottomDlg)
    HelpBtn: TButton;
    GroupBox1: TGroupBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
    RadioButton12: TRadioButton;
    RadioButton13: TRadioButton;
    RadioButton14: TRadioButton;
    RadioButton15: TRadioButton;
    RadioButton4: TRadioButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FourierOptionsForm: TFourierOptionsForm;

implementation

{$R *.DFM}

uses
   Petmar;

   
procedure TFourierOptionsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\four85mb.htm');
end;


end.

