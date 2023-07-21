unit demslped;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define SlopeEditProblems}
{$EndIf}


interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, OkCancl2, Grids,Dialogs,
  DEMDefs, PETMAR,Petmar_types,PETMath;

type
  TSlopeCategoryEditor = class(TOKRightDlg)
    HelpBtn: TButton;
    Image1: TImage;
    ColorDialog1: TColorDialog;
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    StringGrid2: TStringGrid;
    StringGrid1: TStringGrid;
    Button2: TButton;
    procedure HelpBtnClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure StringGrid2MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure StringGrid2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure ShowColors;
  public
    { Public declarations }
     EditSlopeCut     : ColorCutArrayType;
     EditSlopeColors  : tColorArray;
     NumCats          : SmallInt;
     Col,Row,LastX,LastY   : integer;
  end;

function EditSlopeCategories(var inSlopeCut : ColorCutArrayType; var inSlopeColors  : tColorArray) : boolean;

implementation

{$R *.DFM}

uses
   DEMDef_routines;

function EditSlopeCategories(var inSlopeCut : ColorCutArrayType; var inSlopeColors  : tColorArray)  : boolean;
var
  SlopeCategoryEditor : TSlopeCategoryEditor;
  i : integer;
begin
   SlopeCategoryEditor := TSlopeCategoryEditor.Create(Application);
   with SlopeCategoryEditor do begin;
      EditSlopeCut := inSlopeCut;
      EditSlopeColors := inSlopeColors;
      NumCats := MDDef.NumSlopeBands;
      ShowColors;
      if (ShowModal = idCancel) then Result := false
      else begin
         Result := true;
         for i := 1 to pred(NumCats) do CheckEditString(StringGrid1.Cells[0,15-i],EditSlopeCut[i]);
         inSlopeColors := EditSlopeColors;
         inSlopeCut := EditSlopeCut;
         MDDef.NumSlopeBands := NumCats;
      end;
   end;
end;


procedure TSlopeCategoryEditor.ShowColors;
var
   Bitmap : tMyBitmap;
   i      : integer;
begin
   Edit1.Text := IntegerToString(NumCats,-8);
   StringGrid1.Cells[0,0] := '%';
   StringGrid2.Cells[0,0] := 'Degrees (°)';
   for i := 1 to pred(NumCats) do begin
      StringGrid1.Cells[0,15-i] := RealToString(EditSlopeCut[i],-12,-2);
      StringGrid2.Cells[0,15-i] := RealToString(arcTan(0.01*EditSlopeCut[i])/DegToRad,-12,-2);
   end;

   Bitmap := tMyBitmap.Create;
   Bitmap.Height := Image1.Height;
   Bitmap.Width := Image1.Width;
   for i := 0 to pred(NumCats) do with Bitmap.Canvas do begin
      Pen.Color := EditSlopeColors[i];
      Brush.Color := EditSlopeColors[i];
      Rectangle(0,Image1.Height-(i*24),Image1.Width-10,Image1.Height-(succ(i)*24));
      Pen.Color := clBlack;
      MoveTo(Image1.Width-10,Image1.Height-(succ(i)*24));
      LineTo(Image1.Width,Image1.Height-(succ(i)*24));
   end;
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;


procedure TSlopeCategoryEditor.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme0dmb.htm');
end;


procedure TSlopeCategoryEditor.Image1DblClick(Sender: TObject);
var
   i : integer;
begin
   i := (Image1.Height - LastY) div 24;
   ColorDialog1.Color := EditSlopeColors[i];
   if ColorDialog1.Execute then begin
      EditSlopeColors[i] := ColorDialog1.Color;
      ShowColors;
   end;
end;

procedure TSlopeCategoryEditor.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   LastX := x;
   LastY := y;
end;


procedure TSlopeCategoryEditor.Button1Click(Sender: TObject);
begin
   SetSlopedefaultColors(NumCats,EditSlopeCut,EditSlopeColors);
   ShowColors;
end;


procedure TSlopeCategoryEditor.Button2Click(Sender: TObject);
begin
   SetDEMIXSlopeColors(NumCats,EditSlopeCut,EditSlopeColors);
   ShowColors;
end;

procedure TSlopeCategoryEditor.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,NumCats);
   if not (Numcats in [1..15]) then NumCats := 15;
   ShowColors;
end;


procedure TSlopeCategoryEditor.StringGrid1Click(Sender: TObject);
var
   tval : float64;
begin
   if 15-Row < NumCats then begin
      CheckEditString(StringGrid1.Cells[Col,Row],tVal);
      ReadDefault('Slope (%)',tVal);
      StringGrid1.Cells[0,Row] := RealToString(tVal,-12,-2);
      StringGrid2.Cells[0,Row] := RealToString(arcTan(0.01*tVal)/DegToRad,-12,-2);
   end;
end;


procedure TSlopeCategoryEditor.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   StringGrid1.MouseToCell(X,y,Col,Row);
end;


procedure TSlopeCategoryEditor.StringGrid2MouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Integer);
begin
   StringGrid1.MouseToCell(X,y,Col,Row);
end;


procedure TSlopeCategoryEditor.StringGrid2Click(Sender: TObject);
var
   tval : float64;
begin
   if (15-Row < NumCats) then begin
      CheckEditString(StringGrid2.Cells[Col,Row],tVal);
      ReadDefault('Slope (°)',tVal);
      StringGrid2.Cells[0,Row] := RealToString(tVal,-12,-2);
      StringGrid1.Cells[0,Row] := RealToString(100 * TanDeg(tVal),-12,-2);
      CheckEditString(StringGrid1.Cells[0,Row],EditSlopeCut[Row]);
   end;
end;


initialization
finalization
  {$IfDef SlopeEditProblems} WriteLineToDebugFile('SlopeEditProblems active in demslped'); {$EndIf}
  {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing dsmslped'); {$EndIf}
end.



