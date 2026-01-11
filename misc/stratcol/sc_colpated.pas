unit sc_colpated;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


interface

uses
  Windows,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls,
  Zipatone,
  StdCtrls, Spin, Menus, Buttons;

type
  TStratcolPatternEditor = class(TForm)
    Image1: TImage;
    Image2: TImage;
    SpinButton1: TSpinButton;
    SpinButton2: TSpinButton;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    Addpattern1: TMenuItem;
    Color1: TMenuItem;
    ColorDialog1: TColorDialog;
    Replacepattern1: TMenuItem;
    HelpBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure SpinButton2DownClick(Sender: TObject);
    procedure SpinButton2UpClick(Sender: TObject);
    procedure Addpattern1Click(Sender: TObject);
    procedure Color1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Replacepattern1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     CurrentPattern : tPatternRecord;
     PatternOn,
     LastX,LastY : integer;
     procedure LoadPattern;
     procedure FillSquareBig(x,y : integer; Color : tColor);
  end;

var
  StratcolPatternEditor: TStratcolPatternEditor;

implementation

{$R *.DFM}

uses
   Nevadia_Main,
   PETMAR,petmar_types,sc_ColLith, PETImage;


procedure TStratcolPatternEditor.FormCreate(Sender: TObject);
var
   Bitmap : tMyBitmap;
   i    : integer;
begin
   CheckFormPlacement(Self);
   PetImage.CreateBitmap(Bitmap,195,195);
   with Bitmap.Canvas do begin
      for i := 1 to 23 do begin
         MoveTo(0,i*8);
         LineTo(195,i*8);
         MoveTo(i*8,0);
         LineTo(i*8,195);
      end;
   end;
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;


procedure TStratcolPatternEditor.FillSquareBig(x,y : integer; Color : tColor);
begin
   Image1.Canvas.Brush.Color := Color;
   Image1.Canvas.Rectangle(x*8,y*8,(succ(x)*8),(succ(y)*8));
end;


procedure TStratcolPatternEditor.LoadPattern;
var
   x,y : integer;
   Bitmap : tMyBitmap;
begin
   CreateBitmap(Bitmap,64,64);
   FillInBox(Bitmap.Canvas,0,0,64,64,CurrentPattern,true);
   Image2.Picture.Graphic := Bitmap;
   Bitmap.Free;

   with CurrentPattern do begin
      for x := 0 to pred(NumCols) do
         for y := 0 to pred(NumRows) do
            if (PatternMasks[y mod NumRows div 8,x mod NumCols] and MaskBit[y mod NumRows mod 8]) > 0 then
               FillSquareBig(x,y,CurrentPattern.WinColor)
            else FillSquareBig(x,y,ClWhite);
       for x := (NumCols) to 23 do
         for y := 0 to 23 do
            FillSquareBig(x,y,clSilver);
       for y := (NumRows) to 23 do
          for x := 0 to 23 do
            FillSquareBig(x,y,clSilver);
    end;
end;


procedure TStratcolPatternEditor.Image1DblClick(Sender: TObject);
var
   x,y : integer;
begin
   x := LastX div 8;
   y := LastY div 8;
   with CurrentPattern do begin
      if x >= NumCols then NumCols := succ(x);
      if y >= NumRows then NumRows := succ(y);
      if (PatternMasks[y mod NumRows div 8,x mod NumCols] and MaskBit[y mod NumRows mod 8]) = 0 then begin
         PatternMasks[y mod NumRows div 8,x mod NumCols] := PatternMasks[y mod NumRows div 8,x mod NumCols] or MaskBit[y mod NumRows mod 8];
      end
      else PatternMasks[y mod NumRows div 8,x mod NumCols] := PatternMasks[y mod NumRows div 8,x mod NumCols] and ReverseMask[y mod NumRows mod 8];
   end;
   LoadPattern;
end;

procedure TStratcolPatternEditor.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   LastX := x;
   LastY := y;
end;

procedure TStratcolPatternEditor.SpinButton1DownClick(Sender: TObject);
begin
   with CurrentPattern do begin
      if NumCols > 0 then dec(NumCols);
      LoadPattern;
   end;
end;

procedure TStratcolPatternEditor.SpinButton1UpClick(Sender: TObject);
begin
   with CurrentPattern do begin
      if NumCols < 24 then inc(NumCols);
      LoadPattern;
   end;
end;

procedure TStratcolPatternEditor.SpinButton2DownClick(Sender: TObject);
begin
   with CurrentPattern do begin
      if NumRows > 0 then dec(NumRows);
      LoadPattern;
   end;
end;

procedure TStratcolPatternEditor.SpinButton2UpClick(Sender: TObject);
begin
   with CurrentPattern do begin
      if NumRows < 24 then inc(NumRows);
      LoadPattern;
   end;
end;

procedure TStratcolPatternEditor.Addpattern1Click(Sender: TObject);
var
   TStr : shortstring;
begin
   if NumStandardPattern >= MaxPatterns then begin
      MessageToContinue('At pattern limit');
      exit;
   end;
   TStr := CurrentPattern.PatternName;
   repeat
      GetString('3 character pattern name', TStr,true,['A'..'Z','0'..'9']);
      CurrentPattern.PatternName := TStr;
   until Length(CurrentPattern.PatternName) = 3;
   PatternF.ChangedPatterns := true;
   inc(NumStandardPattern);
   StandardPattern^[NumStandardPattern] := CurrentPattern;
   PatternF.NewColorMap;
end;

procedure TStratcolPatternEditor.Color1Click(Sender: TObject);
begin
   ColorDialog1.Color := CurrentPattern.WinColor;
   if ColorDialog1.Execute then begin
      CurrentPattern.WinColor := ColorDialog1.Color;
      LoadPattern;
   end;
end;


procedure TStratcolPatternEditor.FormActivate(Sender: TObject);
begin
   CurrentPattern := StandardPattern^[PatternOn];
   LoadPattern;
end;


procedure TStratcolPatternEditor.Replacepattern1Click(Sender: TObject);
var
   Row,Col : integer;
begin
   if AnswerIsYes('Confirm replace pattern') then begin
      StandardPattern^[PatternOn] := CurrentPattern;
      with PatternF do begin
         Row := RowSize * (pred(PatternOn) div PatPerLine);
         Col := ColSize * (pred(PatternOn) mod PatPerLine);
         FillInBox(LithImage1.Canvas,Col,Row,Col+ColSize,Row+RowSize,CurrentPattern,true);
         ChangedPatterns := true;
      end;
   end;

end;

procedure TStratcolPatternEditor.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\map_pattern.htm');
end;


end.
