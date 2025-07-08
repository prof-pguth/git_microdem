unit petgraphcolors;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
    //{$Define RecordGraphColors}
{$EndIf}

interface

uses
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Controls, Vcl.Buttons,Vcl.ExtCtrls,
  System.Classes,
  Windows, Messages, SysUtils, Graphics,  Forms,
  BaseGraf,Petmar_types,PETMAR;

type
  Tgraphcolorsform = class(TForm)
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Panel1: TPanel;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    SymBitBtn: TBitBtn;
    LineBitBtn: TBitBtn;
    BitBtn10: TBitBtn;
    PlotScaledSymbolsButton: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure SymBitBtnClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure PlotScaledSymbolsButtonClick(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
    //procedure ShowSymbols;
    procedure ColorBitButton(Btn : tBitBtn; SymNum : integer);
    procedure GetSymbolization(Btn : tBitBtn; SymNum : integer);
  public
    { Public declarations }
    TheGraph : tThisBaseGraph;
    CurSymNum,Active,LastY     : integer;
  end;


procedure SetGraphColors(inTheGraph : tThisBaseGraph);
//procedure DefaultGraphColors(var Symbol : tSymbols256; var FileColors : tPlatformColors256; var LineSize :  tBytes256);


implementation

uses
  PetImage;

{$R *.DFM}


procedure Tgraphcolorsform.ColorBitButton(Btn : tBitBtn; SymNum : integer);
var
   Bitmap : tMyBitmap;
begin
   CreateBitmap(Bitmap,40,20);
   if TheGraph.GraphDraw.ShowLine[SymNum] then begin
      Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(TheGraph.GraphDraw.FileColors256[SymNum]);
      Bitmap.Canvas.Pen.Width := TheGraph.GraphDraw.LineSize256[SymNum] ;
      DrawLine(Bitmap,1,Bitmap.height div 2, Bitmap.Width-1,Bitmap.height div 2);
   end;
   if TheGraph.GraphDraw.ShowPoints[SymNum] then begin
      ScreenSymbol(Bitmap.Canvas,20,10,TheGraph.GraphDraw.Symbol[SymNum]);
   end;
   PetImage.GetImagePartOfBitmap(Bitmap);
   Btn.Glyph := Bitmap;
   FreeAndNil(Bitmap);
   (*
   if (TheGraph.GraphDraw.LegendList <> Nil) then begin
      if (TheGraph.GraphDraw.LegendList.Count >= SymNum) then Btn.Caption := TheGraph.GraphDraw.LegendList.Strings[pred(SymNum)]
      else Btn.Visible := false;
   end
   else Btn.Caption := '';
   *)
end;


procedure Tgraphcolorsform.GetSymbolization(Btn : tBitBtn; SymNum : integer);
begin
   Panel1.Visible := true;
   Panel1.Top := Btn.Top;
   Panel1.BringToFront;
   CurSymNum := SymNum;
   SymbolOnButton(SymBitBtn,TheGraph.GraphDraw.Symbol[SymNum]);
   ColorLineWidthBitBtn(LineBitBtn,TheGraph.GraphDraw.FileColors256[SymNum],TheGraph.GraphDraw.LineSize256[SymNum]);
   CheckBox3.Checked := TheGraph.GraphDraw.ShowPoints[SymNum];
   CheckBox4.Checked := TheGraph.GraphDraw.ShowLine[SymNum];
end;



procedure SetGraphColors(inTheGraph : tThisBaseGraph);
var
   graphcolorsform: Tgraphcolorsform;
begin
   {$IfDef RecordGraphColors} writeLineToDebugFile('SetGraphColors in'); {$EndIf}
   graphcolorsform := Tgraphcolorsform.Create(Application);
   graphcolorsform.TheGraph := inTheGraph;
   graphcolorsform.Panel1.Visible := false;
   graphcolorsform.ColorBitButton(graphcolorsform.BitBtn4,1);
   graphcolorsform.ColorBitButton(graphcolorsform.BitBtn5,2);
   graphcolorsform.ColorBitButton(graphcolorsform.BitBtn6,3);
   graphcolorsform.ColorBitButton(graphcolorsform.BitBtn7,4);
   graphcolorsform.ColorBitButton(graphcolorsform.BitBtn8,5);
   graphcolorsform.ColorBitButton(graphcolorsform.BitBtn9,6);
   if (inTheGraph.GraphDraw.GraphType = gtTwoVertAxes) then begin
      graphcolorsform.BitBtn6.Visible := false;
      graphcolorsform.BitBtn7.Visible := false;
      graphcolorsform.BitBtn8.Visible := false;
      graphcolorsform.BitBtn9.Visible := false;
   end;
   graphcolorsform.ShowModal;
end;



procedure Tgraphcolorsform.SymBitBtnClick(Sender: TObject);
begin
   PickSymbol(SymBitBtn,TheGraph.GraphDraw.Symbol[CurSymNum],'Graph symbols');
end;

procedure Tgraphcolorsform.BitBtn4Click(Sender: TObject);
begin
   GetSymbolization(BitBtn4,1);
end;

procedure Tgraphcolorsform.BitBtn5Click(Sender: TObject);
begin
   GetSymbolization(BitBtn5,2);
end;

procedure Tgraphcolorsform.BitBtn6Click(Sender: TObject);
begin
   GetSymbolization(BitBtn6,3);
end;

procedure Tgraphcolorsform.BitBtn7Click(Sender: TObject);
begin
   GetSymbolization(BitBtn7,4);
end;

procedure Tgraphcolorsform.BitBtn8Click(Sender: TObject);
begin
   GetSymbolization(BitBtn6,5);
end;

procedure Tgraphcolorsform.BitBtn9Click(Sender: TObject);
begin
   GetSymbolization(BitBtn9,6);
end;

procedure Tgraphcolorsform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   TheGraph.RedrawDiagram11Click(Nil);
end;

procedure Tgraphcolorsform.BitBtn10Click(Sender: TObject);
begin
   Panel1.Visible := false;
end;

procedure Tgraphcolorsform.Image1MouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Integer);
begin
   LastY := y;
end;

procedure Tgraphcolorsform.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\pick_graph_colors.htm');
end;


procedure Tgraphcolorsform.FormCreate(Sender: TObject);
begin
   {$IfDef RecordGraphColors} writeLineToDebugFile('Tgraphcolorsform.FormCreate'); {$EndIf}
end;


procedure Tgraphcolorsform.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure Tgraphcolorsform.PlotScaledSymbolsButtonClick(Sender: TObject);
begin
   TheGraph.RedrawDiagram11Click(Nil);
end;

procedure Tgraphcolorsform.CheckBox1Click(Sender: TObject);
begin
   TheGraph.GraphDraw.SetShowAllPoints(CheckBox1.Checked);
   TheGraph.RedrawDiagram11Click(Nil);
end;

procedure Tgraphcolorsform.CheckBox2Click(Sender: TObject);
begin
   TheGraph.GraphDraw.SetShowAllLines(CheckBox2.Checked);
   TheGraph.RedrawDiagram11Click(Nil);
end;

procedure Tgraphcolorsform.CheckBox3Click(Sender: TObject);
begin
   TheGraph.GraphDraw.ShowPoints[CurSymNum] := CheckBox3.Checked;
end;

procedure Tgraphcolorsform.CheckBox4Click(Sender: TObject);
begin
   TheGraph.GraphDraw.ShowLine[CurSymNum] := CheckBox4.Checked;
end;

initialization
finalization
   {$IfDef RecordGraphColors} WriteLineToDebugFile('RecordGraphColors active in PetGraphColors'); {$EndIf}
end.
