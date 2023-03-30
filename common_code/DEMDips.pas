unit Demdips;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, ExtCtrls, Dialogs,
   PETMAR,Petmar_types;

type
  TStructureOptions = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    LabelValueCheckBox: TCheckBox;
    CheckBox1: TCheckBox;
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
   StructureOptions : TStructureOptions;
   NetSym      : PETMAR_types.tFullSymbolDeclaration;

implementation

{$R *.DFM}

uses
   DEMDefs,PetImage;


procedure TStructureOptions.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\geology_overlay.htm');
end;


procedure TStructureOptions.BitBtn1Click(Sender: TObject);
begin
   PickSymbol(BitBtn1,NetSym);
end;


procedure TStructureOptions.FormCreate(Sender: TObject);
begin
   SymbolOnButton(BitBtn1,NetSym);
   ColorBitBtn(BitBtn2,MDDef.MapGeoSymColor);
   RadioGroup1.ItemIndex := MdDef.DefaultGeologySymbol;
end;


procedure TStructureOptions.BitBtn2Click(Sender: TObject);
begin
   QueryColor(BitBtn2,MDDef.MapGeoSymColor);
end;


procedure TStructureOptions.RadioGroup1Click(Sender: TObject);
begin
   MdDef.DefaultGeologySymbol := RadioGroup1.ItemIndex;
end;


initialization
   StructureOptions := Nil;
   NetSym.Color := ConvertTColorToPlatformColor(clRed);
   NetSym.Size  := 2;
   NetSym.DrawingSymbol := Box;
finalization
end.
