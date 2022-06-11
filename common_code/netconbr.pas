unit netconbr;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons, StdCtrls, ExtCtrls, Grids;

type
  TInputArrays = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    StringGrid1: TStringGrid;
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InputArrays: TInputArrays;

implementation

{$R *.DFM}

uses
   Petmar_types,PETMAR;

var
   LastX,LastY : integer;


procedure TInputArrays.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   LastX := x;
   LastY := y;
end;


procedure TInputArrays.StringGrid1DblClick(Sender: TObject);
var
   Col,Row : longInt;
   x       : float64;
   err     : integer;
begin
   StringGrid1.MouseToCell(LastX,Lasty,Col,Row);
   if ((Col = 0) and (StringGrid1.FixedCols = 1)) or (Col = 1) then begin
      Val(StringGrid1.Cells[Col,Row],x,err);
      ReadDefault('New value',x);
      StringGrid1.Cells[Col,Row] := RealToString(x,-8,-4);
   end;
end;

procedure TInputArrays.StringGrid1Click(Sender: TObject);
begin
   StringGrid1DblClick(Sender);
end;

procedure TInputArrays.HelpBtnClick(Sender: TObject);
begin
   Application.HelpJump('Contour_boundaries');
end;


initialization
   LastX := 0;
   LastY := 0;
finalization
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing netconbr');
   {$EndIf}
end.
