unit pc_options;

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
  Dialogs, StdCtrls, Buttons;

type
  Tpc_opts_form = class(TForm)
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox6: TCheckBox;
    Label2: TLabel;
    Edit2: TEdit;
    procedure HelpBtnClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure SetPrincipalComponentsOptions;

implementation

{$R *.dfm}

uses
   DEMDefs,Petmar,Petmar_types;

procedure SetPrincipalComponentsOptions;
var
   pc_opts_form : Tpc_opts_form;
begin
  pc_opts_form := Tpc_opts_form.Create(Application);
  with pc_opts_form do begin
     CheckBox2.Checked := MDDef.PCCorrelation;
     CheckBox3.Checked := MDDef.PCVarCovar;
     CheckBox4.Checked := MDDef.PCEigenValues;
     CheckBox5.Checked := MDDef.PCResults;
     CheckBox6.Checked := MDDef.LoadPCBands;
     Edit1.Text := IntToStr(MDDef.MaxPCBands);
     Edit2.Text := RealToString(MDDef.MinPCtoShow,-18,-2);
     ShowModal;
     MDDef.PCCorrelation := CheckBox2.Checked;
     MDDef.PCVarCovar := CheckBox3.Checked;
     MDDef.PCEigenValues := CheckBox4.Checked;
     MDDef.PCResults := CheckBox5.Checked;
     CheckEditString(Edit1.Text,MDDef.MaxPCBands);
     CheckEditString(Edit2.Text,MDDef.MinPCtoShow);
     Free;
  end;
end;

procedure Tpc_opts_form.Edit1Change(Sender: TObject);
begin
   MDDef.LoadPCBands := CheckBox6.Checked;
end;

procedure Tpc_opts_form.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\pc_opts.htm');
end;



end.
