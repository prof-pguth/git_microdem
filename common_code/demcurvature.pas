unit demcurvature;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TCurvatureForm = class(TForm)
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure GetCurvAlg;


implementation

{$R *.DFM}

uses
   PETMAR,Petmar_types,DEMDefs;


procedure GetCurvAlg;
var
  CurvatureForm: TCurvatureForm;
begin
   CurvatureForm := TCurvatureForm.Create(Application);
   with CurvatureForm do begin
      RadioGroup1.ItemIndex := ord(MDdef.CurvAlg);
     {$IfDef ExFresnel}
        Edit1.Visible := false;
     {$Else}
        Edit1.Text := RealToString(MDdef.RadioK,-18,-4);
        Edit1.Enabled := MDdef.CurvAlg = vcRadioLineOfSight;
     {$EndIf}
      ShowModal;
   end;
end;


procedure TCurvatureForm.OKBtnClick(Sender: TObject);
begin
   MDdef.CurvAlg := tVerticalCurvAlg(RadioGroup1.ItemIndex);
   {$IfDef ExFresnel}
   {$Else}
      CheckEditString(Edit1.Text,MDdef.RadioK);
   {$EndIf}
   Close;
end;

procedure TCurvatureForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TCurvatureForm.RadioGroup1Click(Sender: TObject);
begin
   MDdef.CurvAlg := tVerticalCurvAlg(RadioGroup1.ItemIndex);
   Edit1.Enabled := MDdef.CurvAlg = vcRadioLineOfSight;
end;

procedure TCurvatureForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
end;


procedure TCurvatureForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\demb30q0.thm');
end;
 

end.
