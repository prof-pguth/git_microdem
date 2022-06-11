unit aspect_colors;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{ verified 11/22/2021             }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  DEMMapf;


type
  TAspectMapColors = class(TForm)
    RadioGroup1: TRadioGroup;
    RedrawSpeedButton12: TSpeedButton;
    OKBtn: TButton;
    procedure RadioGroup1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
    TheMap : tMapForm;
  public
    { Public declarations }
  end;


procedure ChangeAspectMap(Map : tMapForm);

implementation

{$R *.dfm}

uses
   DEMDefs;

var
  AspectMapColors : TAspectMapColors;

procedure ChangeAspectMap(Map : tMapForm);
begin
   AspectMapColors := TAspectMapColors.Create(Application);
   AspectMapColors.TheMap := Map;
   AspectMapColors.ShowModal;
end;

procedure TAspectMapColors.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TAspectMapColors.RadioGroup1Click(Sender: TObject);
begin
   MDDef.AspectMapMode := RadioGroup1.ItemIndex;
   TheMap.DoBaseMapRedraw;
end;


end.
