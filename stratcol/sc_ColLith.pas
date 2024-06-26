unit sc_ColLith;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

interface



uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, ExtCtrls, Menus, Dialogs,  SysUtils,
   PETMAR, petmar_types,ComCtrls;

const
  RowSize = 32;
  ColSize = 32;

type
  Tpatternf = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    EditPattern1: TMenuItem;
    ColorDialog1: TColorDialog;
    Recolor2: TMenuItem;
    LithImage1: TImage;
    Help1: TMenuItem;
    Recolor1: TMenuItem;
    Allpatterns1: TMenuItem;
    Pattern1: TMenuItem;
    Delete1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Recolor3: TMenuItem;
    Eidt1: TMenuItem;
    Rename1: TMenuItem;
    StatusBar1: TStatusBar;
    SaveallpatternsasBMPzipatone1: TMenuItem;
    SaveasZipatoneBMP1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure LithImage1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure LithImage1DblClick(Sender: TObject);
    procedure Recolor2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Exit1Click(Sender: TObject);
    procedure Allpatterns1Click(Sender: TObject);
    procedure Pattern1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure LithImage1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Recolor3Click(Sender: TObject);
    procedure Eidt1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    //procedure Restrictallto8x81Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure SaveallpatternsasBMPzipatone1Click(Sender: TObject);
    procedure SaveasZipatoneBMP1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    GettingAPattern,
    CreatingNAMfile,
    ChangedPatterns   : boolean;
    NamFile         : System.Text;
    PatPerLine,
    LastX,LastY,PatNum : integer;
    WantName : string3;
    procedure NewColorMap;
end;

var
  patternf: Tpatternf;


function GetAPatternName(WhatFor : ShortString) : string3;


implementation

{$R *.DFM}

uses
   Zipatone,
   PetImage,
   sc_colpated;



function GetAPatternName(WhatFor : ShortString) : string3;
begin
   if (PatternF = Nil) then patternf := Tpatternf.Create(Application);
   patternf.GettingAPattern := true;
   PatternF.Caption := 'Pattern for ' + WhatFor;
   patternf.FormStyle := fsNormal;
   patternf.Visible := false;
   patternf.ShowModal;
   Result := PatternF.WantName;
end;


procedure Tpatternf.NewColorMap;
var
   i,Col,FirstCol,Row : integer;
   CurrentPattern : tPatternRecord;
   BitMap         : tMyBitmap;
begin
   PetImage.CreateBitmap(Bitmap,ClientWidth,ClientHeight);
   Col := 0;
   FirstCol := 0;
   Row := 0;
   StartProgress('Patterns');
   for i := 1 to NumStandardPattern do begin
      if i mod 10 = 0 then UpdateProgressBar(i/ NumStandardPattern);
      CurrentPattern := StandardPattern^[i];
      FillInBox(BitMap.Canvas,Col,Row,Col+ColSize,Row+RowSize,CurrentPattern,true);
      inc(Col,ColSize);
      if Col + ColSize > ClientWidth then begin
         Col := FirstCol;
         inc(Row,RowSize);
      end {if};
      if Row > BitMap.Height - RowSize then break;
   end;
   LithImage1.Picture.Graphic := Bitmap;
   BitMap.Free;
   EndProgress;
end;



procedure Tpatternf.FormCreate(Sender: TObject);
begin
   CreatingNAMFile := false;
   GettingAPattern := false;
   ShowHourglassCursor;
   PatPerLine := ClientWidth div ColSize;
   ClientWidth := succ(PatPerLine * ColSize);
   ClientHeight := succ(succ(NumStandardPattern div PatPerLine) * RowSize) + StatusBar1.Height;
   NewColorMap;
   ChangedPatterns := false;
   Petmar.CheckFormPlacement(Self);
   ShowDefaultCursor;
end;


procedure Tpatternf.LithImage1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   StatusBar1.Panels[0].Text := '';
   LastX := x;
   LastY := y;
   PatNum := ((LastY) div RowSize) * (PatPerLine) + succ((Lastx) div ColSize);
   if (PatNum <= NumStandardPattern) then StatusBar1.Panels[0].Text := StandardPattern^[PatNum].PatternName;
end;


procedure Tpatternf.LithImage1DblClick(Sender: TObject);
var
   Col,Row,i : integer;
   TStr : ShortString;
   CurrentPattern : tPatternRecord;
begin
   PatNum := ((LastY) div RowSize) * (PatPerLine) + succ((Lastx) div ColSize);

   CurrentPattern := StandardPattern^[PatNum];
   Row := RowSize * (pred(PatNum) div PatPerLine);
   Col := ColSize * (pred(PatNum) mod PatPerLine);
   if GettingAPattern then begin
      WantName := CurrentPattern.PatternName;
      Close;
      Exit;
   end;

   if CreatingNAMFile then begin
      TStr := '';
      GetString('lithology name for pattern ' +  StandardPattern^[PatNum].PatternName,
         TStr,false,ReasonableTextChars);
      writeln(NAMFile,StandardPattern^[PatNum].PatternName,' ',TStr);
      CurrentPattern.WinColor := clSilver;
      FillInBox(LithImage1.Canvas,Col,Row,Col+ColSize,Row+RowSize,CurrentPattern,true);
   end
   else if Rename1.Checked then begin
      TStr := StandardPattern^[PatNum].PatternName;
      repeat
         GetString('3 character pattern name', TStr,true,['A'..'Z','0'..'9']);
         StandardPattern^[PatNum].PatternName := TStr;
      until Length(StandardPattern^[PatNum].PatternName) = 3;
   end
   else if Delete1.Checked then begin
      if AnswerIsYes('Confirm delete pattern ' + StandardPattern^[PatNum].PatternName) then begin
         for i := PatNum to pred(NumStandardPattern) do
             StandardPattern^[i] := StandardPattern^[succ(i)];
         dec(NumStandardPattern);
         ChangedPatterns := true;
         NewColorMap;
      end;
   end
   else if Recolor2.Checked then begin
      ColorDialog1.Color := StandardPattern^[PatNum].WinColor;
      if ColorDialog1.Execute then begin
         StandardPattern^[PatNum].WinColor := ColorDialog1.Color;
         CurrentPattern := StandardPattern^[PatNum];
         Row := RowSize * (pred(PatNum) div PatPerLine);
         Col := ColSize * (pred(PatNum) mod PatPerLine);
         FillInBox(LithImage1.Canvas,Col,Row,Col+ColSize,Row+RowSize,CurrentPattern,true);
         ChangedPatterns := true;
      end;
   end
   else begin
      CurrentPattern := StandardPattern^[PatNum];
      MessageToContinue('Pattern' + IntegerToString(PatNum,4) + '     ' + CurrentPattern.PatternName);
   end;
end;


procedure Tpatternf.Recolor2Click(Sender: TObject);
begin
   Recolor2.Checked := not Recolor2.Checked;
end;


procedure Tpatternf.FormClose(Sender: TObject; var Action: TCloseAction);
var
   FileName : PathStr;
   y : integer;
   PatternFile : file of tPatternRecord;
begin
   Action := caFree;
   if ChangedPatterns then with PatternF do begin
      if AnswerIsYes('Save changes in pattern file') then begin
         LithImage1.Picture.SaveToFile(ProgramRootDir + 'rocks.pat');
         assignFile(PatternFile,PatternFileName);
         rewrite(PatternFile);
         for y := 1 to NumStandardPattern do write(PatternFile,StandardPattern^[y]);
         closeFile(PatternFile);
      end;
   end;
   if CreatingNAMfile then begin
      CloseFile(NAMfile);
      FileName := ProgramRootDir;
      if GetFileNameDefaultExt('lihtology pattern names','Lithologies|*.NAM',FileName) then SysUtils.RenameFile(MDTempDir + 'qqtempzz.nam',FileName)
      else DeleteFileIfExists(MDTempDir + 'qqtempzz.nam')
   end;
end;


procedure Tpatternf.Exit1Click(Sender: TObject);
begin
   PatternF.Close;
end;


procedure Tpatternf.Allpatterns1Click(Sender: TObject);
var
   i : integer;
begin
   ColorDialog1.Color := clBlack;
   if ColorDialog1.Execute then for i := 1 to NumStandardPattern do
      StandardPattern^[i].WinColor := ColorDialog1.Color;
   ChangedPatterns := true;
   NewColorMap;
end;



procedure Tpatternf.Pattern1Click(Sender: TObject);
begin
  StratcolPatternEditor := TStratcolPatternEditor.Create(Application);
  StratcolPatternEditor.PatternOn := PatNum;
  StratcolPatternEditor.Show;
end;


procedure Tpatternf.Delete1Click(Sender: TObject);
begin
   Delete1.Checked := not Delete1.Checked;
end;


procedure Tpatternf.LithImage1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   PatNum := ((LastY) div RowSize) * (PatPerLine) + succ((Lastx) div ColSize);
   if (Button = mbRight) then PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Tpatternf.Recolor3Click(Sender: TObject);
var
   CurrentPattern :  tPatternRecord;
   Row,Col : integer;
begin
   ColorDialog1.Color := StandardPattern^[PatNum].WinColor;
   if ColorDialog1.Execute then begin
      StandardPattern^[PatNum].WinColor := ColorDialog1.Color;
      CurrentPattern := StandardPattern^[PatNum];
      Row := RowSize * (pred(PatNum) div PatPerLine);
      Col := ColSize * (pred(PatNum) mod PatPerLine);
      FillInBox(LithImage1.Canvas,Col,Row,Col+ColSize,Row+RowSize,CurrentPattern,true);
      ChangedPatterns := true;
   end;
end;


procedure Tpatternf.Eidt1Click(Sender: TObject);
begin
   StratcolPatternEditor := TStratcolPatternEditor.Create(Application);
   StratcolPatternEditor.PatternOn := PatNum;
   StratcolPatternEditor.Show;
end;


procedure Tpatternf.Help1Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\map_pattern.htm');
end;


procedure Tpatternf.SaveallpatternsasBMPzipatone1Click(Sender: TObject);
begin
   SaveZipatonesToBMP;
end;

procedure Tpatternf.SaveasZipatoneBMP1Click(Sender: TObject);
begin
    SaveCurrentPatternasBMP(StandardPattern^[PatNum]);
end;

procedure Tpatternf.Rename1Click(Sender: TObject);
begin
   Rename1.Checked := not ReName1.Checked;
end;


initialization
finalization
end.
