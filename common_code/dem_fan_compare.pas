unit dem_fan_compare;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file created 6/26/2013        }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Grids;

type
  TFanCompareForm = class(TForm)
    StringGrid1: TStringGrid;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
     OnRow : integer;
  public
    { Public declarations }
  end;

procedure GetFanCompareOptions;


implementation

{$R *.dfm}

uses
   PETMAR,
   DEMDefs,DEMDef_routines,
   Petmar_types,
   DEM_fan_algorithm;


procedure GetFanCompareOptions;
var
  FanCompareForm: TFanCompareForm;
begin
   FanCompareForm := TFanCompareForm.Create(Application);
   FanCompareForm.ShowModal;
   FanCompareForm.Free;
end;

procedure TFanCompareForm.FormCreate(Sender: TObject);
var
   i : integer;
begin
   if (Sender <> Nil) then Petmar.CheckFormPlacement(Self);
   RadioGroup1.ItemIndex := pred(MDDef.MultipleFanAlgorithmsToUse);
   StringGrid1.RowCount := succ(MDDef.MultipleFanAlgorithmsToUse);
   StringGrid1.Cells[0,0] := 'Fan algorithm';
   for i := 1 to MDDef.MultipleFanAlgorithmsToUse do
      StringGrid1.Cells[0,i] := IntervisiblityAlgorithmName(MDDef.CompareIVA[i]);
end;

procedure TFanCompareForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TFanCompareForm.RadioGroup1Click(Sender: TObject);
begin
   MDDef.MultipleFanAlgorithmsToUse := succ(RadioGroup1.ItemIndex);
   FormCreate(nil);
end;

procedure TFanCompareForm.StringGrid1Click(Sender: TObject);
begin
   GetFanAlgorithmParameters(MDDef.CompareIVA[OnRow]);
   FormCreate(nil);
end;

procedure TFanCompareForm.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   OnRow := y div StringGrid1.DefaultRowHeight;
end;

initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demfancompare out'); {$EndIf}
end.
