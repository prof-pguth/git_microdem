unit edit_dip;

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
     Windows, Classes, Graphics, Forms, Controls, Buttons,StdCtrls, ExtCtrls,  PETMAR, Dialogs;

type
  TGetDipStrike = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.DFM}

uses
   DEMDefs;

procedure TGetDipStrike.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\geology_overlay.htm');
end;


procedure TGetDipStrike.FormCreate(Sender: TObject);
begin
   RadioGroup1.ItemIndex := MdDef.DefaultGeologySymbol;
end;


procedure TGetDipStrike.RadioGroup1Click(Sender: TObject);
begin
   MdDef.DefaultGeologySymbol := RadioGroup1.ItemIndex;
end;


procedure TGetDipStrike.OKBtnClick(Sender: TObject);
begin
   Close;
end;

initialization
finalization
end.
