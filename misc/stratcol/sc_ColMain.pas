unit sc_ColMain;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

//{$Define AllowCorrelationFile}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
    //{$Define RecordStratColDBProblems}
    //{$Define RecordStratColProblems}
    //{$Define RecordScaleBarProblems}
    //{$Define RecordStratColUnitProblems}
{$EndIf}


interface

uses
//need for inline of core DB functions
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end core DB functions definitions
  PETMAR,sc_ColLith,Zipatone,DEMDefs,Petmar_types,Petmar_db,
  System.Math,
  //Data.db,
  SysUtils, Windows, Messages, Classes, Graphics, Controls,   //Printers,
  Forms, Dialogs, Menus, StdCtrls, ExtCtrls,Consts,
  Buttons, ToolWin, ComCtrls;

const
   AgeSkip = 45;
   HorizDir = 1;
   VertDir  = 2;
   DrawingColor = clBlack;
   BackColor = clWhite;

   MaxColumns = 25;
   MaxUnitsPerColumn = 250;

type
   tDiagramType = (NothingYet,TimeCorrelation,ThicknessCorrelation);
   PatternPointer = array[1..Zipatone.MaxPatterns] of Zipatone.tPatternRecord;
   BaseType = (Conformable,Unconformity,Channel,Transgressive,Regressive,Questionable,Probable,Jagged);
   UnitRecordType = record
      UnitName   : string10;
      UnitLith   : string3;
      MeterThick : float64;
      BasalAge   : float64;
      Resistance : byte;
      LongDesc   : shortstring;
      UnitBase   : BaseType;
      BaseParam1 : byte;
      BaseParam2 : byte;
      TopUnitOnCanvas : int32;
      BaseUnitOnCanvas : int32;
   end;
   OneColTextType = array[1..5] of shortstring;
   OneColumnType  = record
      ColLat,
      ColLong,
      Blank,
      ColumnTotalThick : float64;
      RockColumn       : array[1..MaxUnitsPerColumn] of UnitRecordType;
      LeftSideColumn,
      RightSideColumn,
      TopOfColumn,
      BottomOfColumn,
      UnitsInColumn    : int32;
      ColumnLocationString,
      ColumnLabel      : ShortString;
      ColumnLongDesc   : OneColTextType;
      ColumnModified   : boolean;
      ColumnFileName   : PathStr;
   end;
   tCorrelationDiagram = record
      AgeScale        : boolean;
      NumColumns      : Int16;
      DiagramThickness,
      DiagramBasalAge,DiagramTopAge  : float64;
      CurrentCorrelationFiles  : array[1..25] of PathStr;
      CurrentCorrelationDirs   : array[1..25] of AnsiChar;
      CurrentCorrelationStart,
      CurrentCorrelationEnd    : array[1..25] of SmallInt;
      CurrentCorrelationFont   : array[1..25] of TFont;
      ScaleBarFont             : tFont;
      TimeLabelFile            : array[1..25] of boolean;
   end;



  TColMainF = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Timecorrelation1: TMenuItem;
    Help1: TMenuItem;
    TimeScalefromFile1: TMenuItem;
    ColumnfromFile1: TMenuItem;
    Newtimelimits1: TMenuItem;
    FontDialog1: TFontDialog;
    Options1: TMenuItem;
    Font1: TMenuItem;
    AddColumn1: TMenuItem;
    SaveImage1: TMenuItem;
    SaveFiles1: TMenuItem;
    CreateNew1: TMenuItem;
    TimeScale1: TMenuItem;
    TimeAxis1: TMenuItem;
    CloseDiagram1: TMenuItem;
    N1: TMenuItem;
    Rockcorrelation1: TMenuItem;
    Labelfile1: TMenuItem;
    Columnfromfile2: TMenuItem;
    N2: TMenuItem;
    EditLithologyPatterns1: TMenuItem;
    OpenDiagram1: TMenuItem;
    Savediagram1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Redraw1: TMenuItem;
    Panel1: TPanel;
    Createnew2: TMenuItem;
    Column1: TMenuItem;
    FossilRanges1: TMenuItem;
    N5: TMenuItem;
    Convertlengthunits1: TMenuItem;
    Timefiles1: TMenuItem;
    Fossilranges2: TMenuItem;
    Present1: TMenuItem;
    Ticksize1: TMenuItem;
    Rescalethickness1: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    Redraw2: TMenuItem;
    Scalebar1: TMenuItem;
    Columns1: TMenuItem;
    PatternNames1: TMenuItem;
    Select1: TMenuItem;
    Create1: TMenuItem;
    Grid1: TMenuItem;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    PopupMenu1: TPopupMenu;
    Tickincrement1: TMenuItem;
    Columnheight1: TMenuItem;
    Display1: TMenuItem;
    Set1: TMenuItem;
    ToolBar1: TToolBar;
    RedrawSpeedButton12: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    CreateGISDB1: TMenuItem;
    ClipboardSpeedButton: TSpeedButton;
    PopupMenu2: TPopupMenu;
    Setverticalpixelsize1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Rescale1Click(Sender: TObject);
    procedure TimeScalefromFile1Click(Sender: TObject);
    procedure Newtimelimits1Click(Sender: TObject);
    procedure ColumnfromFile1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AddColumn1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SaveImage1Click(Sender: TObject);
    procedure SaveFiles1Click(Sender: TObject);
    procedure TimeScale1Click(Sender: TObject);
    procedure CloseDiagram1Click(Sender: TObject);
    procedure Columnfromfile2Click(Sender: TObject);
    procedure Labelfile1Click(Sender: TObject);
    procedure Savediagram1Click(Sender: TObject);
    procedure OpenDiagram1Click(Sender: TObject);
    procedure Redraw1Click(Sender: TObject);
    procedure Column1Click(Sender: TObject);
    procedure FossilRanges1Click(Sender: TObject);
    procedure Timefiles1Click(Sender: TObject);
    procedure Fossilranges2Click(Sender: TObject);
    procedure Ticksize1Click(Sender: TObject);
    procedure Present1Click(Sender: TObject);
    procedure Rescalethickness1Click(Sender: TObject);
    procedure Redraw2Click(Sender: TObject);
    procedure Columns1Click(Sender: TObject);
    procedure Scalebar1Click(Sender: TObject);
    procedure Select1Click(Sender: TObject);
    procedure Create1Click(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure EditLithologyPatterns1Click(Sender: TObject);
    procedure Tickincrement1Click(Sender: TObject);
    procedure Columnheight1Click(Sender: TObject);
    procedure Display1Click(Sender: TObject);
    procedure Set1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure CreateGISDB1Click(Sender: TObject);
    procedure ClipboardSpeedButtonClick(Sender: TObject);
    procedure Setverticalpixelsize1Click(Sender: TObject);
  private
    { Private declarations }
      procedure RedrawCorrelationDiagram;
      procedure RescaleCorrelationDiagram;
      procedure RescaleDiagram;
      procedure InitializeThicknessCorrelation;
      procedure MenuOptionsEnabled;
     function GetPixelSize: float64;
     function AgeToScreenPosition(Age : float64) : integer;
     function ScreenPositionInPixels(Age : float64) : integer;
     function ScreenPositionToAge(y : integer) : float64;
     procedure DrawSingleUnit(Canvas : TCanvas; Column,LithUnit : integer);
     procedure UnitTopAndBottom(NumColumn,NumUnit : integer);
     procedure RedrawUnits(Canvas : TCanvas; Column,TopUnit,BaseUnit : integer);
      procedure CreateNewColumn;
      procedure DrawAColumn(Canvas : TCanvas; Column : integer);
      //function PlotColumn(fName : PathStr) : tMyBitmap;
      procedure DrawAgeBar(Canvas : TCanvas; TickSize : float64);
      procedure InitializeCorrelationDiagram;
      procedure DrawALabelFile(Canvas : TCanvas; Labels : integer);
      procedure WriteStratFile(WhichOne : byte; FileName : PathStr);
      procedure WriteLabelFile(var Fname : PathStr; NumColumns : integer);
      function SaveCOTFile(NumColumns : integer; ColumnFileName : PathStr) : boolean;
      function SaveCOLFile(NumColumns : integer; ColumnFileName : PathStr) : boolean;
      procedure CheckTopDown(FileName : PathStr);
      procedure ReadTimeColumn(Canvas : TCanvas; First : boolean; DirCh : AnsiChar; FileName: PathStr);
      procedure GetRockColumn(Canvas : TCanvas; FileName : PathStr);
      procedure GetBaseAndTop(var Base,Topp : float64);
      procedure SaveDataFiles;
      procedure ZeroVariables;
      procedure PlotTheTime(Canvas : tCanvas; FileNumber : integer; PlotArrows,ShiftRangesRight : boolean;  TimeWidth : integer);
      procedure DrawThicknessScaleBar(Canvas : TCanvas; xstart : integer; Flipped : boolean = false);
      function EditLabelFile(FileNumber,UnitNumber : integer) : boolean;
  public
    { Public declarations }
     Diagram : tDiagramType;
     CorrelationDiagram : tCorrelationDiagram;
     StratColumn : array[1..MaxColumns] of OneColumnType;
     procedure NewFileToDiagram(fName : PathStr);
  end;


var
   NumNamedPatterns,XSkipFactor,YSkipFactor,
   ScreenXMax,ScreenYMax,
   MinUnitWidth : integer;
   NamedPatternShortName : array[0..200] of string3;
   NamedPatternLongNames : TStrings;
   ThisUnitBase,ThisUnitTop         : BaseType;
   ThisBaseParam1,ThisBaseParam2,
   ThisTopParam1,ThisTopParam2      : byte;
   CurrentPattern   : tPatternRecord;
   RowsUsed,TimeXStart : integer;
   MainDefaultFont     : TFont;

procedure FillInPattern(UseCanvas : TCanvas; StartCol,StartRow,TopEndCol,BotEndCol,EndRow,MaxDrift : integer);
procedure WritePatternFile(FileName : PathStr);
procedure MapPatternEditor;


var
   ColMainF : TColMainF;
   TickSize     : float64;




implementation

{$R *.DFM}

uses
   sc_ColEnter,sc_ColLabel,sc_ColHead,sc_Fossil,
   sc_ColOpts,sc_colpated,
   GetLatLn,Make_Tables,
   PETMath,PetImage, petimage_form;

var
   MaxLongDesc,XOffset      : integer;
   WhichOne          : byte;
   TicksPerResist,MaxDrift,
   ZoneWidth : integer;
   ProjectName : PathStr;
   {$IfDef AllowCorrelationFile}
      TFile : file of tCorrelationDiagram;
   {$EndIf}


procedure ModeDraw(UseCanvas : TCanvas; x1,y1,x2,y2 : integer; Incolor : TColor);
begin
   UseCanvas.Pen.Color := InColor;
   UseCanvas.MoveTo(x1,y1);
   UseCanvas.LineTo(x2,y2);
end;

   procedure RaggedDraw(UseCanvas : TCanvas; x1,x2,y1,y2 : integer; color : TColor; MaxDrift : integer);
   var
      LastX,ly,x  : integer;
   begin
      if not MDDef.ColDef.RaggedRightMargin then begin
         ModeDraw(UseCanvas,x1,y1,x2,y2,Color);
         exit;
      end {if};
      LastX := x1;
      for ly := succ(y1) to y2 do begin
         LastX := pred(LastX) + Random(3);
           {Random component can be 0, 1, -1, so shift left with Pred}
         x := x1 + (x2-x1) * (ly - y1) div (y2 - y1);
         if MaxDrift > (y2 - ly) div 2 then dec(MaxDrift);
         if LastX > x + MaxDrift then LastX := x + MaxDrift;
         if LastX < x - MaxDrift then LastX := x - MaxDrift;
         if LastX < x then while (x - LastX) > (y2 - ly) do Inc(LastX)
         else  while (LastX - x) > (y2 - ly) do Dec(LastX);
         UseCanvas.Pixels[LastX,Ly] := Color;
      end {for ly};
   end {proc RaggedDraw};


   procedure UnconformityDraw(UseCanvas : TCanvas; xstart,xend,y,Magnitude,Periods : integer;
      Color : TColor; var ly : integer);
   var
      x,ty : integer;
   begin
      for x := xstart to xend do begin
         ty := y + round(Magnitude * sin((x-xstart) / 200 * periods * Pi));
         if (x > xstart) then ModeDraw(UseCanvas,x,ty,pred(x),ly,Color);
         ly := ty;
      end;
   end;


   procedure DashedDraw(UseCanvas : TCanvas; xstart,xend,y,DrawLength,SkipLength : integer; Color : TColor);
   var
      i,x : integer;
   begin
      x := xstart;
      while x <= xend do begin
         for i := 1 to DrawLength do begin
            UseCanvas.Pixels[x,y] := Color;
            inc(x);
            if (x > xend) then exit;
         end;
         for i := 1 to SkipLength do begin
            UseCanvas.Pixels[x,y] := 0;
            inc(x);
            if (x > xend) then exit;
         end;
      end;
   end;

   procedure ChannelDraw(UseCanvas : TCanvas; xstart,xend,y,Depth,Width : integer; Color : TColor);
   var
      x,x1,ty,ly : integer;
   begin
      x1 := xstart + 4;
      ModeDraw(UseCanvas,xstart,y,x1,y,Color);
      ly := 1;
      for x := x1 to x1 + Width do begin
         ty := y + round(Depth * sin((x-x1) / Width * Pi));
         if x > x1 then ModeDraw(UseCanvas,x,ty,pred(x),ly,Color);
         ly := ty;
      end;
      ModeDraw(UseCanvas,x1 + width,y,xend,y,Color);
   end;


   procedure JaggedDraw(UseCanvas : TCanvas; xstart,xend,y,Depth,Width : integer; Color : Tcolor);
   var
     x1 : integer;
   begin
      ModeDraw(UseCanvas,xStart,y,xStart+Width,y-Depth,Color);
      x1 := xstart + width;
      repeat
         ModeDraw(UseCanvas,x1,y-Depth,x1+Width,y+Depth,Color);
         ModeDraw(UseCanvas,x1+Width,y+Depth,x1+2*Width,y-depth,Color);
         inc(x1,2*width);
      until x1 >= xend - 2*width;
      ModeDraw(UseCanvas,x1,y - depth,xend,y,Color);
   end;



procedure DrawBase(UseCanvas : TCanvas; ThisUnitTop : BaseType;
   StartCol,StartRow,TopEndCol,ThisTopParam1,ThisTopParam2 : integer; DrawingColor : TColor);
begin
   case ThisUnitTop of
      Conformable,
      Questionable,
      Probable     : ModeDraw(UseCanvas,StartCol,StartRow,TopEndCol,StartRow,DrawingColor);
      Channel      : ChannelDraw(UseCanvas,StartCol,TopEndCol,StartRow,ThisTopParam1,
                             ThisTopParam2,DrawingColor);
      Jagged       : JaggedDraw(UseCanvas,StartCol,TopEndCol,StartRow,ThisTopParam1,
                             ThisTopParam2,DrawingColor);
      Unconformity : UnconformityDraw(UseCanvas,StartCol,TopEndCol,StartRow,ThisTopParam1,
                             ThisTopParam2,DrawingColor,StartRow);
      Transgressive : ModeDraw(UseCanvas,StartCol,StartRow + ThisTopParam1,
                             TopEndCol,StartRow,DrawingColor);
      Regressive    : ModeDraw(UseCanvas,StartCol,StartRow - ThisTopParam1,
                             TopEndCol,StartRow,DrawingColor);
  end {case};
end;


procedure FillInPattern(UseCanvas : TCanvas; StartCol,StartRow,TopEndCol,BotEndCol,EndRow,MaxDrift : integer);
var
   EndTop,RealStartRow,RealEndRow,
   XLimit,x,y : integer;
   UseColor : TColor;
begin {proc FillInPattern}
   if (StartRow > EndRow) then begin
      x := StartRow;
      StartRow := EndRow;
      EndRow := x;
      x := TopEndCol;
      TopEndCol := BotEndCol;
      BotEndCol := x;
   end;
   UseColor := CurrentPattern.WinColor;
   with CurrentPattern do case MDDef.ColDef.ColorAndPatternOptions of
      patSolidColors     : begin
                           NumCols := 1;
                           NumRows := 1;
                        end;
      patBlackAndWhitePatterns : UseColor := clBlack;
      patNoPatterns : begin
                      UseColor := clWhite;
                      NumCols := 1;
                      NumRows := 1;
                   end;
   end {case};
   RealStartRow := StartRow;
   RealEndRow := EndRow;
   DrawBase(UseCanvas,ThisUnitTop,StartCol,StartRow,TopEndCol,
         ThisTopParam1,ThisTopParam2,DrawingColor);
   if ThisUnitBase = Transgressive then
      ModeDraw(UseCanvas,StartCol,EndRow + ThisBaseParam1,StartCol,EndRow,DrawingColor);
   if ThisUnitBase = Regressive then
      ModeDraw(UseCanvas,StartCol,EndRow - ThisBaseParam1,StartCol,EndRow,DrawingColor);
   ModeDraw(UseCanvas,StartCol,EndRow,StartCol,StartRow,DrawingColor);       {Left}

   DrawBase(UseCanvas,ThisUnitBase,StartCol,EndRow,BotEndCol, ThisBaseParam1,ThisBaseParam2,DrawingColor);
   if UseColor = BackColor then begin
      RaggedDraw(UseCanvas,TopEndCol,BotEndCol,RealStartRow,RealEndRow,DrawingColor,
          MaxDrift);  {Right side}
   end
   else if (CurrentPattern.NumCols = 0) then begin {covered pattern}
      ModeDraw(UseCanvas,TopEndCol,RealStartRow,BotEndCol,RealEndRow,DrawingColor); {Right}
      ModeDraw(UseCanvas,StartCol,StartRow,TopEndCol,EndRow,UseColor);
      ModeDraw(UseCanvas,StartCol,EndRow,BotEndCol,StartRow,UseColor);
   end
   else begin
      RaggedDraw(UseCanvas,TopEndCol,BotEndCol,RealStartRow,RealEndRow,DrawingColor,MaxDrift);  {Right side}
      if abs(RealEndRow - RealStartRow) > 2 then with CurrentPattern do begin
         if (TopEndCol < BotEndCol) then XLimit := TopEndCol - MaxDrift - 2
         else XLimit := BotEndCol - MaxDrift - 2;
         if not (ThisUnitTop in [Conformable,Probable,Questionable]) then begin
          {then fill in the irregular top}
            inc(StartRow,succ(ThisTopParam1));
            EndTop := succ(StartCol);
            while (UseCanvas.Pixels[EndTop,succ(StartRow)] = BackColor) do inc(EndTop);
            for x := succ(StartCol) to pred(TopEndCol) do begin
               y := StartRow;
               while (UseCanvas.Pixels[x,y] <> DrawingColor) {and (y > 0)} do begin
                  {start with given point, and move up until hitting border}
                  if (PatternMasks[y mod NumRows div 8,x mod NumCols] and MaskBit[y mod NumRows mod 8]) > 0 then
                     UseCanvas.Pixels[x,y] := UseColor;
                  dec(y);
               end {while};
            end {for x};
         end {if};

         if not (ThisUnitBase in [Conformable,Probable,Questionable]) then begin
          {fill in the irregular base}
            if ThisUnitBase = Channel then dec(EndRow,1)
            else dec(EndRow,succ(ThisBaseParam1));
            EndTop := succ(StartCol);
            while (UseCanvas.Pixels[EndTop,pred(EndRow)] = BackColor) do inc(EndTop);
            for x := succ(StartCol) to pred(BotEndCol) do begin
               y := EndRow;
               while (UseCanvas.Pixels[x,y] <> DrawingColor) do begin
                  {start with given point, and move down until hitting border}
                  if (PatternMasks[y mod NumRows div 8,x mod NumCols] and MaskBit[y mod NumRows mod 8]) > 0 then
                     UseCanvas.Pixels[x,y] := UseColor;
                  inc(y);
               end {while};
            end {for x};
         end {if};
         for y := succ(StartRow) to pred(EndRow) do begin
            {first use quick method to fill rectangular part of pattern}
            for x := succ(StartCol) to succ(XLimit) do
               if (PatternMasks[y mod NumRows div 8,x mod NumCols] and MaskBit[y mod NumRows mod 8]) > 0 then
                  UseCanvas.Pixels[x,y] := UseColor;
            {then fill in the irregular margin}
            x := XLimit + 2;

            while (UseCanvas.Pixels[x,y] = clWhite) do begin
                  {start with given point, and move to the right until hitting the
                      border or leaving the screen}
               if (PatternMasks[y mod NumRows div 8,x mod NumCols] and MaskBit[y mod NumRows mod 8]) > 0 then
                  UseCanvas.Pixels[x,y] := UseColor;
               inc(x);
            end {while};

         end {for y};
      end;
   end {if};

   case ThisUnitTop of
      Questionable : DashedDraw(UseCanvas,StartCol,TopEndCol,StartRow,3,3,DrawingColor);
      Probable     : DashedDraw(UseCanvas,StartCol,TopEndCol,StartRow,5,2,DrawingColor);
   end {case};
   case ThisUnitBase of
      Probable     : DashedDraw(UseCanvas,StartCol,BotEndCol,EndRow,5,2,DrawingColor);
      Questionable : DashedDraw(UseCanvas,StartCol,BotEndCol,EndRow,3,3,DrawingColor);
   end {case};
   RowsUsed := EndRow;
end;




procedure WritePatternFile(FileName : PathStr);
var
   y : integer;
   PatternFile : file of tPatternRecord;
begin
   assign(PatternFile,FileName);
   rewrite(PatternFile);
   for y := 1 to NumStandardPattern do write(PatternFile,StandardPattern^[y]);
   close(PatternFile);
end;


function TColMainF.AgeToScreenPosition(Age : float64) : integer;
begin
   with CorrelationDiagram do begin
      if AgeScale then begin
         if (Age < DiagramTopAge) then AgeToScreenPosition := AgeSkip
         else if (Age > DiagramBasalAge) then AgeToScreenPosition := ScreenYMax
         else AgeToScreenPosition := AgeSkip + round( (Age - DiagramTopAge) /(DiagramBasalAge - DiagramTopAge) *(ScreenYMax- AgeSkip));
      end
      else begin
         AgeToScreenPosition := AgeSkip +  round((Age) / DiagramThickness * (ScreenYMax - AgeSkip));
      end;
   end {with};
end;


function TColMainF.ScreenPositionInPixels(Age : float64) : integer;
begin
   with CorrelationDiagram do begin
      if AgeScale then Result := 1
      else begin
         if (MDDef.ColDef.ThickLabelling = StartTop) then begin
            Result := AgeSkip + AgeToScreenPosition(DiagramTopAge)-AgeToScreenPosition(DiagramTopAge-Age);
         end
         else begin
            Result := ScreenYMax + AgeSkip - AgeToScreenPosition(Age);
         end;
      end;
   end {with};
end;


function TColMainF.ScreenPositionToAge(y : integer) : float64;
begin
   with CorrelationDiagram do
      ScreenPositionToAge := DiagramTopAge + (DiagramBasalAge - DiagramTopAge) * (y - AgeSkip) / (ScreenYMax - AgeSkip);
end;


procedure TimeZoneName(Canvas : TCanvas; xoffset,ZoneWidth,yw,yl,Direction : integer;
    TStr : ShortString; DrawLines,XCenter : boolean);
var
   TStr2,TStr1 : AnsiString;
   Split,ForceDraw       : boolean;
   OldSize,
   i,j,spare,XSpare,Spare2 : integer;
begin
   with Canvas,MDDef.ColDef do begin
      if yw > yl then begin
         i := yw;
         yw := yl;
         yl := i;
      end;
      TrimLeft(TStr);
      TrimRight(TStr);
      if DrawLines then begin
         ModeDraw(Canvas,xoffset+1,yw,xoffset+ZoneWidth,yw,DrawingColor);
         ModeDraw(Canvas,xoffset+1,yl,xoffset+ZoneWidth,yl,DrawingColor);
      end;
      if (Direction = VertDir) then begin
         OldSize := Font.Size;
         Spare := abs(yl-yw) - TextWidth(TStr);
         while (Spare < 2) and (Font.Size > 6) do begin
            Font.Size := Font.Size - 1;
            Spare := abs(yl-yw) - TextWidth(TStr);
         end;
         XSpare := ZoneWidth - Font.Size;
         if (Spare > 2) then TextOutVertical(Canvas,xoffset+pred(XSpare div 2)-2, yl - Spare div 2,TStr);
         Font.Size := OldSize;
      end
      else begin {Horizontal direction}
         Font.Size := 12;
         Font.Style := [fsBold];
         ForceDraw := false;

         with Canvas do begin
            OldSize := Font.Size;
            Spare := abs(yl-yw) - Font.Size;
            Spare2 := ZoneWidth - TextWidth(TStr);
            if not ForceDraw then begin
               if ((Spare2 < 2) and (Spare > 2 * Font.Size + 5)) then begin
                   Split := false;
                   for i := Low(TStr) to High(TStr) do if TStr[i] = ' ' then
                      Split := true;
                   if Split then begin
                      j := 0;
                      i := length(TStr) div 2;
                      while TStr[i] <> ' ' do begin
                         if Split then  i := length(TStr) div 2 + j
                         else begin
                            i := length(TStr) div 2 - j;
                            inc(j);
                         end;
                         Split := not Split;
                      end;
                      Spare := abs(yl-yw) - 2 * Font.Size;
                      TStr1 := copy(TStr,1,pred(i));
                      TStr2 := copy(TStr,succ(i),length(TStr) - i);
                      Spare2 := ZoneWidth - TextWidth(TStr1);
                      TextOut(xoffset+ Spare2 div 2,yw + Spare div 2 - 2,TStr1);
                      Spare2 := ZoneWidth - TextWidth(TStr2);
                      TextOut(xoffset+ Spare2 div 2,yw + Spare div 2 + 2 + Font.Size,TStr2);
                      exit;
                   end;
               end;
               while ((Spare < 2) or (Spare2 < 2)) and (Font.Size > 6) do begin
                  Font.Size := Font.Size - 1;
                  Spare := abs(yl-yw) - Font.Size;
                  Spare2 := ZoneWidth - TextWidth(TStr);
               end;
            end;
            if ForceDraw or ((Spare > 2) and (Spare2 > 2)) then begin
               if not XCenter then Spare2 := 0;
               TextOut(xoffset+ Spare2 div 2,yw + Spare div 2,TStr);
            end;
            Font.Size := OldSize;
         end;
      end;
   end {with};
end {proc};


procedure TColMainF.DrawSingleUnit(Canvas : TCanvas; Column,LithUnit : integer);
var
   Th : float64;
   i,TopRight,BottomRight  : integer;
   TStr  : ShortString;
begin
   with StratColumn[Column],RockColumn[LithUnit] do begin
      UnitTopAndBottom(Column,LithUnit);
      CurrentPattern := PatternFromString(UnitLith);
      if CorrelationDiagram.AgeScale then begin
         TopUnitOnCanvas := AgeToScreenPosition(RockColumn[pred(LithUnit)].BasalAge);
         BaseUnitOnCanvas := AgeToScreenPosition(BasalAge);
      end
      else begin
         th := Blank;
         for i := 2 to pred(LithUnit) do th := th + RockColumn[i].MeterThick;
         TopUnitOnCanvas := AgeToScreenPosition(th);
         BaseUnitOnCanvas := AgeToScreenPosition(th + RockColumn[LithUnit].MeterThick);
      end;
      TopRight := RightSideColumn;
      BottomRight := RightSideColumn;
      if MDDef.ColDef.VariableResistance then begin
         TopRight := RightSideColumn - 20 + 4 * Resistance;
         BottomRight := RightSideColumn - 20 + 4 * Resistance;
      end;
      {$IfDef RecordStratColUnitProblems} WriteLineToDebugFile('Left=' + IntToStr(LeftSideColumn) + '  Top=' + IntToStr(TopRight)+ '  Base=' + IntToStr(BaseUnitOnCanvas)); {$EndIf}
      FillInPattern(Canvas,LeftSideColumn,TopUnitOnCanvas,TopRight,BottomRight,BaseUnitOnCanvas,4);
      if (MDDef.ColDef.TextLabels <> textNone) then begin
         if (MDDef.ColDef.TextLabels = textLong) then TStr := ptTrim(LongDesc)
         else TStr := ptTrim(UnitName);
         if (MDDef.ColDef.TextPlacement = textBeside) then
            TimeZoneName(Canvas,LeftSideColumn+MDDef.ColDef.PixelsColumnWide+4,ColMainF.ClientWidth,TopUnitOnCanvas,BaseUnitOnCanvas,
                    HorizDir,TStr,false,false)
         else TimeZoneName(Canvas,LeftSideColumn,MDDef.ColDef.PixelsColumnWide,TopUnitOnCanvas,BaseUnitOnCanvas,HorizDir,TStr,false,true);
      end;
   end {with};
end;


procedure TColMainF.UnitTopAndBottom(NumColumn,NumUnit : integer);
begin
   if NumUnit = 1 then ThisUnitTop := Conformable
   else with StratColumn[NumColumn].RockColumn[pred(NumUnit)] do begin
      ThisUnitTop   := UnitBase;
      ThisTopParam1 := BaseParam1;
      ThisTopParam2 := BaseParam2;
   end;
   with StratColumn[NumColumn].RockColumn[NumUnit] do begin
      ThisUnitBase := UnitBase;
      ThisBaseParam1 := BaseParam1;
      ThisBaseParam2 := BaseParam2;
   end;
end;


procedure TColMainF.RedrawUnits(Canvas : TCanvas; Column,TopUnit,BaseUnit : integer);
var
   Color : TColor;
   i,j   : integer;
   OK    : boolean;
begin
   with MDDef.ColDef,StratColumn[Column] do begin
      if (TopUnit < 2) then TopUnit := 2;
      if (BaseUnit > UnitsInColumn) then BaseUnit := UnitsInColumn;

      i := 0;
      repeat
         inc(i);
         Color := ElevColors[i];
         OK := true;
         for j := TopUnit to BaseUnit do begin
            with RockColumn[j] do
               CurrentPattern := PatternFromString(UnitLith);
            if (CurrentPattern.WinColor = Color) then OK := false;
         end;
      until OK;
      try
         ShowHourglassCursor;
         UnitTopAndBottom(Column,TopUnit);
         if (TopUnit = 2) then with Canvas,RockColumn[TopUnit] do begin
            Pen.Color := clWhite;
            for i := LeftSideColumn to RightSideColumn do begin
               MoveTo(i,0);
               LineTo(i,BaseUnitOnCanvas);
            end;
         end;

         with RockColumn[TopUnit] do DrawBase(Canvas,ThisUnitTop,
             LeftSideColumn,TopUnitOnCanvas,RightSideColumn,ThisTopParam1,ThisTopParam2,Color);

         UnitTopAndBottom(Column,BaseUnit);

         if (TopUnit = UnitsInColumn) then with RockColumn[TopUnit] do begin
            Canvas.Pen.Color := clWhite;
            for i := LeftSideColumn to RightSideColumn do begin
               Canvas.MoveTo(i,BaseUnitOnCanvas);
               Canvas.LineTo(i,ScreenYMax);
            end;
         end;

         with RockColumn[BaseUnit] do DrawBase(Canvas,ThisUnitBase,
             LeftSideColumn,BaseUnitOnCanvas,RightSideColumn,
             ThisBaseParam1,ThisBaseParam2,Color);
         with Canvas do begin
            Pen.Color := Color;
            with RockColumn[TopUnit] do MoveTo(LeftSideColumn,TopUnitOnCanvas);
            with RockColumn[BaseUnit] do LineTo(LeftSideColumn,BaseUnitOnCanvas);
            with RockColumn[TopUnit] do MoveTo(RightSideColumn,TopUnitOnCanvas);
            with RockColumn[BaseUnit] do LineTo(RightSideColumn,BaseUnitOnCanvas);
            Brush.Color := clWhite;
            FloodFill(succ(LeftSideColumn),(RockColumn[TopUnit].TopUnitOnCanvas +
               RockColumn[BaseUnit].BaseUnitOnCanvas) div 2, Color, fsBorder);

            Pen.Color := clWhite;
            with RockColumn[TopUnit] do MoveTo(LeftSideColumn,TopUnitOnCanvas);
            with RockColumn[BaseUnit] do LineTo(LeftSideColumn,BaseUnitOnCanvas);
            with RockColumn[TopUnit] do MoveTo(RightSideColumn,TopUnitOnCanvas);
            with RockColumn[BaseUnit] do LineTo(RightSideColumn,BaseUnitOnCanvas);
            with RockColumn[TopUnit] do DrawBase(Canvas,ThisUnitTop,
                LeftSideColumn,TopUnitOnCanvas,RightSideColumn,ThisTopParam1,ThisTopParam2,ClWhite);
         end;
         for i := TopUnit to BaseUnit do DrawSingleUnit(Canvas,Column,i);
      finally
         ShowDefaultCursor;  {Default}
      end;
   end {with};
end;


procedure ReadTheFile(FileName : PathStr; var ThisColumn : OneColumnType);
var
   i,{code,}OnUnit              : integer;
   OneLine,
   SectionTitle               : shortstring;
  // MeterThickStr,DurationStr  : shortstring;
   //LithStr,ResistStr          : shortstring;
   RockCol                    : System.text;
   ch                         : AnsiChar;

   procedure BaseParams;
   begin
      dec(OnUnit);
      if Length(oneLine) >= 15 then ThisColumn.RockColumn[OnUnit].BaseParam1 := StrToInt(trim(copy(OneLine,11,5)));
      if Length(oneLine) >= 25 then ThisColumn.RockColumn[OnUnit].BaseParam2 := StrToInt(trim(copy(OneLine,16,5)));
   end;


begin
   try
     with ThisColumn do begin
      {$IfDef RecordStratColProblems} WriteLineToDebugFile('ReadTheFile Enter: ' + FileName); {$EndIf}
      ColumnFileName := FileName;
      assign(RockCol,FileName);
      reset(RockCol);
      read(RockCol,ch);
      ColumnLocationString := '';
      if ch = '*' then begin
         {$I-} read(RockCol,ColLat,ColLong); {$I+}
         if IOResult <> 0 then begin
            reset(RockCol);
            read(RockCol,ch);
            readln(RockCol,ColumnLocationString);
         end
         else begin
            if not eoln(rockCol) then readln(RockCol,ColumnLocationString);
         end;
         {$IfDef RecordStratColProblems} WriteLineToDebugFile(LatLongDegreeToString(ColLat,ColLong,DecMinutes)); {$EndIf}
      end
      else begin
         reset(RockCol);
         ColLat := MaxInt;
         ColLong := MaxInt;
      end;
      readln(RockCol,SectionTitle);
      ColumnLabel:= Copy(SectionTitle,1,20);
      {$IfDef RecordStratColProblems} WriteLineToDebugFile('   ' + SectionTitle); {$EndIf}
      while (Length(ColumnLabel) < 20) do ColumnLabel := ColumnLabel + ' ';

      if FileName[Length(FileName)] = 'L' then begin
         OnUnit := 1;
         ColumnTotalThick := 0;
         with RockColumn[1] do begin
            UnitBase := Conformable;
            MeterThick := 0;
        end;
      end
      else OnUnit := 0;
      FillChar(ColumnLongDesc,SizeOf(OneColTextType),chr(0));
      while (not EOF(RockCol)) and (OnUnit < MDDef.ColDef.UsersMaxUnits) do begin
         inc(OnUnit);
         with RockColumn[OnUnit] do begin
            readln(RockCol,OneLine);
            UnitName := trim(copy(OneLine,1,10));
            {$IfDef RecordStratColUnitProblems} WriteLineToDebugFile('  Read Unit: ' + IntToStr(OnUnit) + '   ' + UnitName); {$EndIf}
            if (UnitName = 'BASE') then begin
               BaseParams;
               i := 0;
               while (not EOF(RockCol)) and (i < 5) do begin
                  inc(i);
                  readln(RockCol,ColumnLongDesc[i]);
               end {while};
            end
            else if UnitName = 'UNCONFORM' then begin
                BaseParams;
                RockColumn[OnUnit].UnitBase := Unconformity;
            end
            else if UnitName = 'CHANNEL' then begin
                BaseParams;
                RockColumn[OnUnit].UnitBase := Channel;
            end
            else if UnitName = 'QUESTION' then begin
                BaseParams;
                RockColumn[OnUnit].UnitBase := Questionable;
            end
            else if UnitName = 'JAGGED' then begin
                BaseParams;
                RockColumn[OnUnit].UnitBase := Jagged;
            end
            else if UnitName = 'PROBABLE' then begin
                BaseParams;
                RockColumn[OnUnit].UnitBase := Probable;
            end
            else if UnitName = 'REGRESS' then begin
                BaseParams;
                RockColumn[OnUnit].UnitBase := Regressive;
            end
            else if UnitName = 'TRANSGRESS' then begin
                BaseParams;
                RockColumn[OnUnit].UnitBase := Transgressive;
            end
            else begin
               RockColumn[OnUnit].UnitName := UnitName + '         ';
               RockColumn[OnUnit].MeterThick := StrToFloat(trim(copy(OneLine,11,10)));
               RockColumn[OnUnit].BasalAge := StrToFloat(trim(copy(OneLine,21,10)));
               RockColumn[OnUnit].UnitLith := trim(copy(OneLine,31,5));
               RockColumn[OnUnit].Resistance := StrToInt(trim(copy(OneLine,36,5)));
               ColumnTotalThick := ColumnTotalThick + MeterThick;
               LongDesc := '';
               if Length(OneLine) > 40 then  LongDesc := Copy(OneLine,41,Length(OneLine)-40);
                  TrimLeft(LongDesc);
                  if Length(LongDesc) < MaxLongDesc then MaxLongDesc := Length(LongDesc);
                  while Length(LongDesc) < pred(SizeOf(LongDesc)) do LongDesc := LongDesc + ' ';
               UnitBase := Conformable;
            end {with};
         end;
         if (OnUnit = MDDef.ColDef.UsersMaxUnits) and (not EOF(RockCol)) then
            MessageToContinue('Too many units; Some ignored.');
      end {while};
      UnitsInColumn := OnUnit;
      {$IfDef RecordStratColProblems}   WriteLineToDebugFile('Read: ' + FileName + ' w/ ' + IntToStr(UnitsInColumn) + ' units + thick=' + RealToString(ColumnTotalThick,-12,-2)); {$EndIf}
     end {with};
   except
      on Exception do MessageToContinue('Error reading ' + FileName);
   end;
   close(RockCol);
end {proc ReadTheFile};


procedure TColMainF.CreateNewColumn;
var
   i : integer;
begin
   {$IfDef RecordStratColProblems} WriteLineToDebugFile('CreateNewColumn in'); {$EndIf}
   with CorrelationDiagram do begin
      inc(NumColumns);
      {$IfDef RecordStratColProblems} WriteLineToDebugFile('Newcol=' + IntToStr(NumColumns)); {$EndIf}
      CurrentCorrelationFont[NumColumns] := MainDefaultFont;
      with StratColumn[NumColumns] do begin
         ColumnModified := false;
         ColumnLabel := '';
         ColumnFileName := '';
         TimeLabelFile[NumColumns] := false;
         Blank := 0;
         ColumnTotalThick := 0;
         for i := 1 to 5 do ColumnLongDesc[i] := '';
         for i := 1 to MaxUnitsPerColumn do with RockColumn[i] do begin
            UnitName   := 'Bed'  + IntegerToString(pred(i),5);
            UnitLith := NamedPatternShortName[0];
            MeterThick := 0;
            BasalAge   := 0;
            Resistance := 3;
            LongDesc   := '';
            UnitBase   := Conformable;
            BaseParam1 := 5;
            BaseParam2 := 5;
         end;
      end;
   end;
   {$IfDef RecordStratColProblems} WriteLineToDebugFile('CreateNewColumn out'); {$EndIf}
end;


procedure TColMainF.DrawAColumn(Canvas : TCanvas; Column : integer);
var
   i : integer;
begin
   {$IfDef RecordStratColUnitProblems} WriteLineToDebugFile('DrawAColumn in'); {$EndIf}
   try
      ShowHourglassCursor;
      with CorrelationDiagram,StratColumn[Column] do begin
         if (MDDef.ColDef.AlignColumns = acBase) then Blank := DiagramThickness - ColumnTotalThick
         else Blank := 0;
         Canvas.Font := CurrentCorrelationFont[Column];
         if MDDef.ColDef.ColumnVerbiage then begin
            Canvas.TextOut(LeftSideColumn,0,ColumnLabel);

            case MDDef.ColDef.LocationLabel of
              llLatLong  : if (abs(ColLat) < 90) and (abs(ColLong) < 180) then
                                Canvas.TextOut(LeftSideColumn,20,LatLongDegreeToString(ColLat,ColLong,MDDef.OutPutLatLongMethod));
              llText     : Canvas.TextOut(LeftSideColumn,20,ColumnLocationString);
            end;
         end;
         with RockColumn[1] do BaseUnitOnCanvas := AgeToScreenPosition(BasalAge);
         for i := 2 to UnitsInColumn do begin
            DrawSingleUnit(Canvas,Column,i);
            {$IfDef RecordStratColUnitProblems} WriteLineToDebugFile('  Draw unit ' + IntToStr(i)); {$EndIf}
         end;
         TopOfColumn := RockColumn[1].BaseUnitOnCanvas;
         BottomOfColumn := RockColumn[UnitsInColumn].BaseUnitOnCanvas;
      end {with};
   finally
      ShowDefaultCursor;
   end;
   {$IfDef RecordStratColUnitProblems} WriteLineToDebugFile('DrawAColumn out, TopCol=' + IntToStr(StratColumn[Column].TopOfColumn) + '  BottomCol=' + IntToStr(StratColumn[Column].BottomOfColumn)); {$EndIf}
end;

(*
function TColMainF.PlotColumn(fName : PathStr) : tMyBitmap;
begin
    CreateNewColumn;
    with CorrelationDiagram do begin
       //New(StratColumn[NumColumns]);
       ReadTheFile(fName,StratColumn[NumColumns]);
       PetImage.CreateBitmap(Result,150,500);
       DrawAColumn(Result.Canvas,1);
    end;
end;
*)

procedure InitializeNamedPatterns;
var
   PatFile : text;
   TStr  : ShortString;
begin
   {$IfDef RecordStratColProblems} WriteLineToDebugFile('InitializeNamedPatterns in, ' + LithFileName); {$EndIf}

   NumNamedPatterns := 0;
   if (NamedPatternLongNames <> Nil) then NamedPatternLongNames.Free;
   NamedPatternLongNames := TStringList.Create;
   with MDDef.ColDef do begin
      if FileExists(LithFileName) then begin
          assign(PatFile,LithFileName);
          reset(PatFile);
          while not EOF(PatFile) do begin
             inc(NumNamedPatterns);
             readln(PatFile,NamedPatternShortName[pred(NumNamedPatterns)],TStr);
             TrimLeft(Tstr);
             TrimRight(Tstr);
             NamedPatternLongNames.Add(TStr);
          end {while};
          {$IfDef RecordStratColProblems}
          WriteLineToDebugFile('Number NamedPattern=' + IntToStr(NamedPatternLongNames.Count));
         {$EndIf}
          NamedPatternLongNames.Add('Other');
          close(PatFile);
      end;
   end {with};
end;


var
   SinglePatternColumn : boolean;
   ColumnBaseName      : array[1..MaxColumns] of NameStr;


procedure DisplayDSDPPatterns(LithFileName : PathStr);
const
   BoxSize = 40;
var
   Row,Col, ColInc, MaxCol,FirstCol, NumPat,i,j   : integer;
   CurrentPattern : Zipatone.tPatternRecord;
   Bitmap : tMyBitmap;
   LithNames : tStringList;
   TStr  : ShortString;
begin
   LithNames := tStringList.Create;
   LithNames.LoadFromFile(LithFileName);
   NumPat := LithNames.Count;

   ColInc := 175;
   MaxCol := 550;
   PetImage.CreateBitmap(Bitmap,750,BoxSize * (NumPat + 2) div 3);
   Col := 0;
   Row := 0;
   SinglePatternColumn := false;

   FirstCol := Col;

   for j := 0 to pred(NumPat) do begin
      TStr := ptTrim(LithNames.Strings[j]);
      i := 0;
      CurrentPattern := PatternFromString(TStr);
      FillInBox(Bitmap.Canvas,Col,Row,Col+BoxSize,Row+BoxSize,CurrentPattern,true);
      Delete(Tstr,1,4);
      Bitmap.Canvas.TextOut(Col + BoxSize + 3,Row+BoxSize div 2,TStr);
      inc(Col,ColInc);
      if (Col > MaxCol) then begin
         Col := FirstCol;
         inc(Row,BoxSize);
      end {if};
   end {while};
   PetImage_form.DisplayBitmap(Bitmap,ExtractFileName(LithFileName));
   Bitmap.Free;
end;


   procedure TColMainF.DrawAgeBar(Canvas : TCanvas; TickSize : float64);
   var
      Age : float64;
      y   : integer;
   begin
      {$IfDef RecordScaleBarProblems}
      WriteLineToDebugFile('DrawAgeBar in');
      {$EndIf}
      if MDDef.ColDef.ShowAgeBar then with MDDef,ColDef,Canvas,CorrelationDiagram do begin
         Font := ScaleBarFont;
         ModeDraw(Canvas,0,20,0,ColDef.DefaultMyBitmapHeight,DrawingColor);
         if (TickSize < 0) then begin
            TickSize := 5;
            Age := TickSize * succ(trunc(DiagramBasalAge/TickSize));
         end;
         while Age >= DiagramTopAge do begin
            if abs(Age - DiagramTopAge) > -0.0001 then begin
               Y := ScreenPositionInPixels(Age);
               ModeDraw(Canvas,0,y,10,y,DrawingColor);
               if ColMainF.Grid1.Checked then begin
                  Pen.Style := psDot;
                  MoveTo(XOffset+10+TextWidth(RealToString(Age,-12,-2)),y);
                  LineTo(ColDef.DefaultMyBitmapWidth,y);
                  Pen.Style := psSolid;
                  Pen.Color := clBlack;
               end;
               dec(y,8);
               Canvas.Font.Color := DrawingColor;
               Canvas.TextOut(12,y,RealToString(Age,-12,-2));
            end {if};
            Age := Age - TickSize;
         end;
         inc(XOffset,25+TextWidth(RealToString(DiagramBasalAge,-12,-2)));
      end;
   end;


procedure TColMainF.InitializeCorrelationDiagram;
var
   i : integer;
begin
      CorrelationDiagram.NumColumns := 0;
      CorrelationDiagram.AgeScale := true;

   with CorrelationDiagram do begin
      CorrelationDiagram.ScaleBarFont := tFont.Create;
      for i := 1 to 25 do CorrelationDiagram.CurrentCorrelationFont[i] := tFont.Create;

      with ScaleBarFont do begin
         Name := 'Verdana';
         Size := 10;
      end;
      for i := 1 to 25 do begin
         CurrentCorrelationFont[i] := ScaleBarFont;
         CurrentCorrelationDirs[i] := 'V';
      end;
   end;
end;



procedure TColMainF.DrawALabelFile(Canvas : TCanvas; Labels : integer);
var
   Dir,i : integer;

      procedure Check(var TopUnitOnCanvas : integer);
      begin
         if TopUnitOnCanvas < AgeSkip then TopUnitOnCanvas := AgeSkip;
         if TopUnitOnCanvas > (MDDef.ColDef.DefaultMyBitmapHeight) then TopUnitOnCanvas := MDDef.ColDef.DefaultMyBitmapHeight;
      end;

begin
   try
      ShowHourglassCursor;
      with CorrelationDiagram,StratColumn[Labels] do begin
         Canvas.Font := CurrentCorrelationFont[Labels];
         if CurrentCorrelationDirs[Labels] = 'H' then Dir := HorizDir
         else Dir := VertDir;
         for i := 2 to UnitsInColumn do with RockColumn[i] do begin
            TopUnitOnCanvas := ScreenPositionInPixels(RockColumn[pred(i)].BasalAge);
            BaseUnitOnCanvas := ScreenPositionInPixels(BasalAge);
            Check(TopUnitOnCanvas);
            Check(BaseUnitOnCanvas);
            ModeDraw(Canvas,LeftSideColumn,TopUnitOnCanvas,LeftSideColumn,BaseUnitOnCanvas,DrawingColor);
            ModeDraw(Canvas,RightSideColumn,TopUnitOnCanvas,RightSideColumn,BaseUnitOnCanvas,DrawingColor);
            TimeZoneName(Canvas,LeftSideColumn,RightSideColumn-LeftSideColumn,TopUnitOnCanvas,BaseUnitOnCanvas,Dir,LongDesc,true,true);
         end;
      end {with};
   finally
      ShowDefaultCursor;
   end;
end;



procedure TColMainF.WriteStratFile(WhichOne : byte; FileName : PathStr);
var
   i : integer;
   ColFile : textFile;
begin
with StratColumn[WhichOne] do begin
   assignFile(ColFile,FileName);
   rewrite(ColFile);
   writeln(ColFile,'*',ColLat:9:5,ColLong:12:5);
   writeln(ColFile,ColumnLabel);
   for i := 1 to UnitsInColumn do begin
      with RockColumn[i] do begin
         write(ColFile,UnitName:10,MeterThick:10:3,
            (BasalAge):10:3,UnitLith:5,Resistance:5);
         if (LongDesc <> '') then write(ColFile,'  ',LongDesc);
         writeln(ColFile);
         case UnitBase of
            Unconformity  : write(ColFile,'UNCONFORM   ');
            Channel       : write(ColFile,'CHANNEL     ');
            Transgressive : write(ColFile,'TRANSGRESS  ');
            Regressive    : write(ColFile,'REGRESS     ');
            Questionable  : write(ColFile,'QUESTION    ');
            Probable      : write(ColFile,'PROBABLE    ');
            Jagged        : write(ColFile,'JAGGED      ');
         end;
         if (UnitBase <> Conformable) then
            writeln(ColFile,BaseParam1:5,BaseParam2:10);
      end {with};
   end {for};
   writeln(ColFile,'BASE');
   for i := 1 to 5 do
      if length(ColumnLongDesc[i]) > 0 then
         writeln(ColFile,ColumnLongDesc[i]);
   closeFile(ColFile);
end {with};
end;


procedure TColMainF.WriteLabelFile(var Fname : PathStr; NumColumns : integer);
var
   DFile : textFile;
   i     : integer;
   Dir : DirStr;
   tName : NameStr;
   Ext  : ExtStr;
begin
    FSplit(FName,Dir,tName,Ext);
    Ext := UpperCase(ext);
    if not ( (Ext[2] = 'T') and (Ext[3] = 'M') ) then begin
       FName := Dir + tName + '.TM1';
       while FileExists(FName) do
          FName[Length(FName)] := succ(FName[Length(FName)]);
   end;
   assignFile(DFile,FName);
   rewrite(DFile);
   with StratColumn[NumColumns] do
      for i := 2 to UnitsInColumn do
         with RockColumn[i] do
            writeln(DFile,BasalAge:12:4,RockColumn[pred(i)].BasalAge:12:4,'  ',LongDesc);
   CloseFile(DFile);
end;


function TColMainF.SaveCOTFile(NumColumns : integer; ColumnFileName : PathStr) : boolean;
var
   fName : pathStr;
begin
   fName := MainMapData + 'stratcol\' + ColumnFileName;
   if Petmar.GetFileNameDefaultExt('STRATCOL','.COT',fName) then begin
      WriteStratFile(NumColumns,fName);
      Result := true;
   end
   else Result := true;
end;

function TColMainF.SaveCOLFile(NumColumns : integer; ColumnFileName : PathStr) : boolean;
var
   fName : pathStr;
begin
   fName := MainMapData + 'stratcol\' + ColumnFileName;
   if Petmar.GetFileNameDefaultExt('STRATCOL','.COL',fName) then begin
      WriteStratFile(NumColumns,fName);
      Result := true;
   end
   else Result := true;
end;

procedure TColMainF.CheckTopDown(FileName : PathStr);
var
   inf      : system.text;
   base,top : array[1..500] of float64;
   Descripts : tStringList;
   i         : integer;
   MenuStr  : ShortString;
begin
   Descripts := tStringList.Create;
   assignFile(inf,FileName);
   reset(inf);
   i := 0;
   while not EOF(inf) do begin
      inc(i);
      readln(inf,base[i],top[i],MenuStr);
      Descripts.Add(MenuStr);
   end;
   closeFile(inf);
   if (i > 1) and (base[1] < Base[2]) then begin
      rewrite(inf);
      for i := Descripts.Count downto 1 do
         writeln(inf,base[i]:8:2,top[i]:10:2,Descripts.Strings[pred(i)]);
      closeFile(inf);
   end;
end;


procedure TColMainF.ReadTimeColumn(Canvas : TCanvas; First : boolean; DirCh : AnsiChar; FileName: PathStr);
var
   TimeFile          : textFile;
   bage,tage     : float64;
   TStr  : ShortString;
begin
   with MDDef.COLDef,Canvas,CorrelationDiagram do begin
      Canvas.Font.Size := DefaultFontSize;
      if not FileExists(FileName) then begin
         FileName := ExtractFilePath(LastStratColFile);
         if not GetFileFromDirectory('Time ranges','*.TM*',FileName) then exit;
         CheckTopDown(FileName);
      end;
      CreateNewColumn;
      CurrentCorrelationFiles[NumColumns] := FileName;
      TimeLabelFile[NumColumns] := true;
   try
      ShowHourglassCursor;
         AssignFile(TimeFile,FileName);
         reset(TimeFile);
         CurrentCorrelationDirs[NumColumns] := DirCh;
         case DirCh of
            'H' : begin
                     ZoneWidth := 0;
                     while not EOF(TimeFile) do begin
                        readln(TimeFile,bage,tage,TStr);
                        TrimLeft(TStr);
                        TrimRight(TStr);
                        if Length(TStr) > ZoneWidth then
                           ZoneWidth := Length(TStr);
                     end {while};
                     reset(TimeFile);
                     ZoneWidth := ZoneWidth*Canvas.Font.Size + 2;
                  end;
            'V' : ZoneWidth := Canvas.TextHeight('W') + 2;
         end;
         if First then begin
            readln(TimeFile,tage,DiagramTopAge);
            while not EOF(TimeFile) do begin
               readln(TimeFile,DiagramBasalAge,tage);
               while Eoln(TimeFile) and not EOF(TimeFile) do readln(TimeFile);
            end;
            DrawAgeBar(Canvas,TickSize);
            reset(TimeFile);
         end;
         with StratColumn[NumColumns] do begin
            ColumnFileName := FileName;
            LeftSideColumn := XOffset;
            RightSideColumn := XOffset + ZoneWidth;
            XOffset := RightSideColumn;

            if MDDef.ColDef.TextDirection = TextHorizontal then
               CurrentCorrelationDirs[NumColumns] := 'H'
            else CurrentCorrelationDirs[NumColumns] := 'V';
            UnitsInColumn := 1;
            while not EOF(TimeFile) do begin
               inc(UnitsInColumn);
               with RockColumn[UnitsInColumn] do begin
                  readln(TimeFile,BasalAge,RockColumn[pred(UnitsInColumn)].BasalAge,LongDesc);
                  TrimLeft(LongDesc);
                  TrimRight(LongDesc);
               end;
               while Eoln(TimeFile) and not EOF(TimeFile) do readln(TimeFile);
            end;
         end;
         DrawALabelFile(Canvas,NumColumns);
         closeFile(TimeFile);
   finally
      ShowDefaultCursor;
   end;
   end {with};
end;


procedure TColMainF.GetRockColumn(Canvas : TCanvas; FileName : PathStr);
begin
   TimeXStart := XOffset;
   with MDDef.ColDef,CorrelationDiagram do begin
      {$IfDef RecordStratColProblems} WriteLineToDebugFile('GetRockColumn 1: ' + FileName); {$EndIf}
      if not FileExists(FileName) then begin
         FileName := LastStratColFile;
         if not GetFileFromDirectory('strat column','*.CO*',FileName) then exit;
         LastStratColFile := FileName;
         {$IfDef RecordStratColProblems} WriteLineToDebugFile('GetRockColumn 2: ' + FileName); {$EndIf}
      end;
      CreateNewColumn;
      CurrentCorrelationFiles[NumColumns] := FileName;
      try
         ShowHourglassCursor;
         ReadTheFile(FileName,StratColumn[NumColumns]);
         ColumnBaseName[NumColumns] := ExtractFileNameNoExt(FileName);
         with StratColumn[NumColumns] do begin
            LeftSideColumn := TimeXStart;
            RightSideColumn := TimeXStart + PixelsColumnWide;
            {$IfDef RecordStratColProblems} WriteLineToDebugFile('Col extents: ' + IntToStr(LeftSideColumn) + '--' + IntToStr(RightSideColumn)); {$EndIf}
            Blank := DiagramThickness - ColumnTotalThick;
            if Blank < 0 then begin
               {$IfDef RecordStratColProblems} WriteLineToDebugFile('GetRockColumn 2.5'); {$EndIf}
               DiagramThickness := ColumnTotalThick;
               TickSize := -1;
               ColMainF.RedrawCorrelationDiagram;
            end
            else begin
               {$IfDef RecordStratColProblems} WriteLineToDebugFile('GetRockColumn 3');               {$EndIf}
               DrawAColumn(Canvas,NumColumns);
               {$IfDef RecordStratColProblems} WriteLineToDebugFile('GetRockColumn 4'); {$EndIf}
            end;
         end;
     finally
        inc(XOffset,MDDef.ColDef.PixelsColumnWide+ ColumnSeparation);
        ShowDefaultCursor;
     end;
   end;
end;


procedure TColMainF.GetBaseAndTop(var Base,Topp : float64);
var
   ok : BOOLEAN;
begin
   ColLabF := TColLabF.Create(Application);
   with ColLabF do begin
      TopEdit.Text := RealToString(Topp,-18,-6);
      BaseEdit.Text := RealToString(Base,-18,-6);
      TextEdit.Text := '';
      TextEdit.Visible := false;
      Label2.Visible := false;
      ColLabF.Caption := 'Enter time limits';
      repeat
         OK := true;
         if ShowModal = mrCancel then exit
         else begin
            CheckEditString(TopEdit.Text,Topp);
            CheckEditString(BaseEdit.Text,Base);
            if (Top > Base) then begin
               MessageToContinue('Base must be older than top');
               OK := false;
            end;
         end;
      until OK;
   end;
   ColLabF.Free;
end;


procedure TColMainF.RescaleCorrelationDiagram;
begin
   with CorrelationDiagram do begin
      GetBaseAndTop(DiagramBasalAge,DiagramTopAge);
      RedrawCorrelationDiagram;
   end;
end;


procedure TColMainF.SaveDataFiles;
var
   i   : integer;
begin
   with CorrelationDiagram do begin
      for i := 1 to NumColumns do with StratColumn[i] do begin
         if ColumnModified then begin
            if AnswerIsYes(IntegerToString(i,2) + ' Column ' + ColumnLabel + ' in ' + #10 +
                   ColumnFileName + ' has changed.  Save it') then begin
               if TimeLabelFile[i] then ColumnModified := not SaveCOTFile(NumColumns,ColumnFileName)
               else ColumnModified := not SaveCOLFile(NumColumns,ColumnFileName);;
            end;
         end;
      end;
      ColMainF.SaveFiles1.Enabled := false;
      for i := 1 to NumColumns do  with StratColumn[i] do
         if ColumnModified then ColMainF.SaveFiles1.Enabled := true;
   end {with};
end;


procedure TColMainF.ZeroVariables;
//var
   //i : integer;
begin
   with CorrelationDiagram do begin
      SaveDataFiles;
      NumColumns := 0;
      XOffset := 0;
   end;
end;


procedure TColMainF.PlotTheTime(Canvas : tCanvas; FileNumber : integer; PlotArrows,ShiftRangesRight : boolean;  TimeWidth : integer);
label
   InvalidZone,Bored;
var
   FileName : PathStr;
   StartLabelTime,
   TextXStart,OldSize,
   Spare,StartRow,EndRow : integer;
   Base,Top,tval   : float64;
   Dfile      : system.text;
   TStr       : ShortString;
begin
   StartLabelTime := CorrelationDiagram.CurrentCorrelationStart[FileNumber];
   FileName := CorrelationDiagram.CurrentCorrelationFiles[FileNumber];
   assignFile(Dfile,FileName);
   reset(Dfile);
   while not EOF(Dfile) do with Canvas do begin
      {$I-} readln(Dfile,Base,Top,TStr); {$I+}
      if (IOResult <> 0) then goto Bored;
      if (Base > Top) then begin
         tval := Base;
         Base := Top;
         Top := tval;
      end;
      TrimLeft(TStr);
      TrimRight(TStr);
      if (Base < 0) or (Top < 0) or (Base > CorrelationDiagram.DiagramThickness) or
         (Top < 0) or  (Top > CorrelationDiagram.DiagramThickness) then goto InvalidZone;
      StartRow := ScreenPositionInPixels(Top);
      EndRow := ScreenPositionInPixels(Base);
      if Not ShiftRangesRight then begin  {plot top and bottom lines}
         MoveTo(StartLabelTime,StartRow);
         LineTo(StartLabelTime+TimeWidth,StartRow);
         MoveTo(StartLabelTime,EndRow);
         LineTo(StartLabelTime+TimeWidth,EndRow);
      end;

      OldSize := Font.Size;
      Spare := (abs(StartRow-EndRow) - TextWidth(TStr)) div 2;
      if (TStr[1] = '"') then begin
         Font.Style := [fsItalic];
         Delete(TStr,1,1);
      end
      else Font.Style := [];
      TextXStart := StartLabelTime - TextHeight('E') div 4;
      if PlotArrows then begin
         MoveTo(StartLabelTime+TimeWidth div 2,EndRow);
         LineTo(StartLabelTime+TimeWidth div 2,StartRow);
      end;

      if (Spare > 2) then CanvasTextOutAngle(Canvas,TextXStart,EndRow-Spare,900,TStr)
      else if ShiftRangesRight then begin
         Font.Size := OldSize;
         if TextWidth(TStr)+16 < StartRow then {label at top}
            CanvasTextOutAngle(Canvas,TextXStart,StartRow-8,900,TStr)
         else {label at bottom}
            CanvasTextOutAngle(Canvas,TextXStart,EndRow+8+TextWidth(TStr),900,TStr);
      end;

      Font.Size := OldSize;

      if PlotArrows and ((Spare > 20) or (ShiftRangesRight and (EndRow - StartRow > 20))) then begin
        {top arrow}
         MoveTo(StartLabelTime+TimeWidth div 2,StartRow);
         LineTo(StartLabelTime+TimeWidth div 2-3,StartRow + 8 );
         MoveTo(StartLabelTime+TimeWidth div 2,StartRow);
         LineTo(StartLabelTime+TimeWidth div 2+3,StartRow + 8 );
        {bottom arrow}
         MoveTo(StartLabelTime+TimeWidth div 2,EndRow);
         LineTo(StartLabelTime+TimeWidth div 2+3,EndRow - 8 );
         MoveTo(StartLabelTime+TimeWidth div 2,EndRow);
         LineTo(StartLabelTime+TimeWidth div 2-3,EndRow - 8 );
      end
      else if ShiftRangesRight then begin
        {top arrow, pointing down}
         MoveTo(StartLabelTime+TimeWidth div 2,StartRow);
         LineTo(StartLabelTime+TimeWidth div 2,StartRow - 12 );
         MoveTo(StartLabelTime+TimeWidth div 2,StartRow);
         LineTo(StartLabelTime+TimeWidth div 2-3,StartRow - 8 );
         MoveTo(StartLabelTime+TimeWidth div 2,StartRow);
         LineTo(StartLabelTime+TimeWidth div 2+3,StartRow - 8 );
        {bottom arrow, pointing up}
         MoveTo(StartLabelTime+TimeWidth div 2,EndRow);
         LineTo(StartLabelTime+TimeWidth div 2,EndRow + 12 );
         MoveTo(StartLabelTime+TimeWidth div 2,EndRow);
         LineTo(StartLabelTime+TimeWidth div 2+3,EndRow + 8 );
         MoveTo(StartLabelTime+TimeWidth div 2,EndRow);
         LineTo(StartLabelTime+TimeWidth div 2-3,EndRow + 8 );
      end {if};

      while EOLn(Dfile) and (not EOF(Dfile)) do readln(Dfile);
      if ShiftRangesRight then begin
         inc(StartLabelTime,TextHeight('E'));
         if StartLabelTime + TextHeight('E') > CorrelationDiagram.CurrentCorrelationEnd[FileNumber] then begin
            StartLabelTime := CorrelationDiagram.CurrentCorrelationStart[FileNumber]
                + TextHeight('E');
         end {if};
      end {if};
      if WantOut then goto Bored;
      InvalidZone:;
   end {while};
 Bored:;
  closeFile(Dfile);
end {proc PlotTheTime};


var
   MouseOnBoundary,
   MovingBoundary,
   MouseOnLabel,
   MouseOnUnit : boolean;
   LastY,
   MouseOnColumnNumber,
   MouseOnUnitNumber    : integer;


   procedure TColMainF.DrawThicknessScaleBar(Canvas : TCanvas; xstart : integer; Flipped : boolean = false);
   var
      Age,PlotAge : float64;
      y,ys   : integer;
   begin
      if MDDef.ColDef.ShowAgeBar then begin
         with Canvas,CorrelationDiagram do begin
            if xstart < 0 then begin
               xstart := MDDef.ColDef.DefaultMyBitmapWidth - (30+TextWidth(RealToString(DiagramThickness,-12,-2)));
            end;

            Canvas.Font := CorrelationDiagram.ScaleBarFont;
            Pen.Color := DrawingColor;
            if Flipped then y := 11 else y := 0;
            MoveTo(xstart+1+y,AgeSkip);
            Lineto(xstart+1+y,MDDef.ColDef.DefaultMyBitmapHeight);
            with ColMainF do if TickSize < 0 then begin
               TickSize := GetTickInt(MDDef.ColDef.DefaultMyBitmapHeight,25,DiagramThickness);
            end;

            {$IfDef RecordScaleBarProblems} WriteLineToDebugFile('TickSize: ' + RealToString(TickSize,-8,-2)); {$EndIf}

            Age := 0;
            while Age <= DiagramThickness do begin
               if abs(Age - DiagramTopAge) > -0.0001 then begin
                  y := ScreenPositionInPixels(Age);
                 {$IfDef RecordScaleBarProblems} WriteLineToDebugFile('Age: ' + RealToString(Age,-8,-2) + '/' +  RealToString(DiagramThickness ,-8,-2) + IntegerToString(y,12)); {$EndIf}
                  ModeDraw(Canvas,xstart+1,y,xstart+10,y,DrawingColor);
                  Canvas.Font.Color := DrawingColor;
                  PlotAge := MDDef.ColDef.ScaleLabelOffset + Age;
                  if MDDef.ColDef.AbsThickness then PlotAge := -PlotAge;
                  if Flipped then ys := 2 else ys := 0;
                  Canvas.TextOut(xstart+12+ys,y-8,RealToString(PlotAge,-12,-2));
                  if ColMainF.Grid1.Checked then with Canvas do begin
                     Pen.Style := psDot;
                     MoveTo(xstart+XOffset+20+TextWidth(RealToString(Age,-12,-2)),y);
                     LineTo(xstart+MDDef.ColDef.DefaultMyBitmapWidth,y);
                     Pen.Style := psSolid;
                     Pen.Color := clBlack;
                  end;
               end {if};
               Age := Age + TickSize;
            end;
            inc(XOffset,30+TextWidth(RealToString(Age,-12,-2)));
         end;
      end;
   end;


function TColMainF.GetPixelSize : float64;
begin
   Result := (CorrelationDiagram.DiagramTopAge-CorrelationDiagram.DiagramBasalAge)/
       (ScreenPositionInPixels(CorrelationDiagram.DiagramTopAge) - ScreenPositionInPixels(CorrelationDiagram.DiagramBasalAge));
end;

procedure TColMainF.RedrawCorrelationDiagram;
var
   i     : integer;
   Ext   : ExtStr;
   OffScreenBitmap : tMyBitmap;
begin
  {$If Defined(RecordScaleBarProblems) or Defined(RecordStratColProblems)} WriteLineToDebugFile(' TColMainF.RedrawCorrelationDiagramPixel size: ' + RealToString(GetPixelSize,-12,-2)); {$EndIf}
   try
      ShowHourglassCursor;
      WhichOne := 1;
      XOffset := 0;
      PetImage.CreateBitmap(OffScreenBitmap,MDDef.ColDef.DefaultMyBitmapWidth,MDDef.ColDef.DefaultMyBitmapHeight);
      ScreenXMax := pred(OffScreenBitMap.Width);
      ScreenYMax := pred(OffScreenBitMap.Height) - 25;

      if MDDef.ColDef.ShowAgeBar and (CorrelationDiagram.NumColumns > 0) then begin
         if (Diagram = TimeCorrelation) then DrawAgeBar(OffScreenBitmap.Canvas,TickSize)
         else if (Diagram = ThicknessCorrelation) then begin
            DrawThicknessScaleBar(OffScreenBitmap.Canvas,0);
            if MDDef.ColDef.RightSideThickness then DrawThicknessScaleBar(OffScreenBitmap.Canvas,-200,true);
         end
         else exit;
      end
      else XOffset := 0;

      with CorrelationDiagram do for i := 1 to NumColumns do begin
         Ext := ExtractFileExt(CurrentCorrelationFiles[i]);
         Ext := UpperCase(Copy(Ext,2,2));
         {$IfDef RecordStratColProblems} WriteLineToDebugFile('Drawing: ' + Name); {$EndIf}
         if (Ext = 'CO') then begin
            DrawAColumn(OffScreenBitmap.Canvas,i);
            {$IfDef RecordStratColProblems}          WriteLineToDebugFile('   Xoffset:  ' + IntToStr(Xoffset));          {$EndIf}
            inc(XOffset,MDDef.ColDef.PixelsColumnWide + MDDef.ColDef.ColumnSeparation);
            {$IfDef RecordStratColProblems}            WriteLineToDebugFile('   Xoffset:  ' + IntToStr(Xoffset));          {$EndIf}
         end
         else if (Ext = 'FO') then begin
            inc(XOffset,15);
            PlotTheTime(OffscreenBitmap.Canvas,i,true,true,10);
         end
         else if (Ext = 'TM') then begin
            DrawALabelFile(OffScreenBitmap.Canvas,i);
            inc(Xoffset,ZoneWidth);
         end;
      end {with};
    finally
      ShowDefaultCursor;
      Image1.Height := MDDef.ColDef.DefaultMyBitmapHeight;
      Image1.Width := MDDef.ColDef.DefaultMyBitmapWidth;
      Image1.Picture.Graphic := OffScreenBitmap;
      OffScreenBitmap.Free;
   end;
end;


procedure TColMainF.Exit1Click(Sender: TObject);
begin
   Close;
end;


procedure TColMainF.MenuOptionsEnabled;
begin
   //with ColMainF do begin
      NewTimeLimits1.Enabled := CorrelationDiagram.NumColumns > 0;
      ColumnfromFile1.Enabled := CorrelationDiagram.NumColumns > 0;
      SaveDiagram1.Enabled := CorrelationDiagram.NumColumns > 0;
      CloseDiagram1.Enabled := CorrelationDiagram.NumColumns > 0;
      OpenDiagram1.Enabled := CorrelationDiagram.NumColumns = 0;
      AddColumn1.Enabled := CorrelationDiagram.NumColumns > 0;
      SaveFiles1.Enabled := false;
      Present1.Checked := MDDef.ColDef.ShowAgeBar;
   //end {with};
end;


procedure TColMainF.TimeScalefromFile1Click(Sender: TObject);
var
   DirCh : AnsiChar;
   OffScreenBitmap : tMyBitmap;
begin
   with CorrelationDiagram do begin
      Diagram := TimeCorrelation;
      RockCorrelation1.Enabled := false;
      MDDef.ColDef.RaggedRightMargin := false;
      AgeScale := true;
      if NumColumns = 0 then XOffset := 0;
      if MDDef.ColDef.TextDirection = textVertical then DirCh := 'V' else DirCh := 'H';
      CopyImageToBitmap(Image1,OffScreenBitMap);
      ReadTimeColumn(OffScreenBitMap.Canvas,NumColumns = 0,DirCh,'');
      ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
      OffScreenBitMap.Free;
      MenuOptionsEnabled;
      StopSplashing;
   end {with};
end;


procedure TColMainF.FormCreate(Sender: TObject);
begin
  ReadPatternFile;
  InitializeNamedPatterns;
  Diagram := NothingYet;
  Font.Name := 'Times New Roman';
  Font.Size := MDDef.ColDef.DefaultFontSize;
  MainDefaultFont := Font;
  TickSize := -1;
  ScrollBox1.Height := ClientHeight - 30;
  InitializeCorrelationDiagram;
  CorrelationDiagram.DiagramThickness := MDDef.ColDef.DefaultThickness;
  RedrawCorrelationDiagram;
  with CorrelationDiagram do begin
     NewTimeLimits1.Enabled := NumColumns > 0;
     ColumnfromFile1.Enabled := NumColumns > 0;
     AddColumn1.Enabled := NumColumns > 0;
     SaveFiles1.Enabled := false;
     Present1.Checked := MDDef.ColDef.ShowAgeBar;
   end;
   Petmar.CheckFormPlacement(Self);
end;


procedure TColMainF.RescaleDiagram;
var
   x,y : int32;
begin
    x := ColMainF.Image1.Picture.Graphic.Width;
    y := ColMainF.Image1.Picture.Graphic.Height;
    if GetNewBMPSize(x,y,'') then  begin
       MDDef.ColDef.DefaultMyBitmapWidth := x;
       MDDef.ColDef.DefaultMyBitmapHeight := y;
       RedrawCorrelationDiagram;
    end;
end;

procedure TColMainF.Rescale1Click(Sender: TObject);
begin
   RescaleDiagram;
end;


procedure TColMainF.Newtimelimits1Click(Sender: TObject);
begin
   RescaleCorrelationDiagram;
end;


procedure TColMainF.ColumnfromFile1Click(Sender: TObject);
var
   OffScreenBitmap : tMyBitmap;
begin
   StopSplashing;
   RockCorrelation1.Enabled := false;
   CopyImageToBitmap(Image1,OffscreenBitmap);
   GetRockColumn(OffScreenBitMap.Canvas,'');
   ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
   OffScreenBitmap.Free;
end;


procedure TColMainF.FormDestroy(Sender: TObject);
begin
   NamedPatternLongNames.Free;
   NamedPatternLongNames := Nil;
end;


function TColMainF.EditLabelFile(FileNumber,UnitNumber : integer) : boolean;
var
   t : float64;
   error : integer;
   ok : BOOLEAN;
begin
   ColLabF := TColLabF.Create(Application);
   with CorrelationDiagram,StratColumn[FileNumber],RockColumn[UnitNumber] do begin
      ColLabF.TopEdit.Text := RealToString(RockColumn[pred(UnitNumber)].BasalAge,-18,-6);
      ColLabF.BaseEdit.Text := RealToString(BasalAge,-18,-6);
      ColLabF.TextEdit.Text := LongDesc;
      ColLabF.TextEdit.Visible := true;
      ColLabF.Label2.Visible := true;
      ColLabF.Caption := 'Edit time unit';
      repeat
         OK := true;
         if ColLabF.ShowModal = mrCancel then EditLabelFile := false
         else begin
            EditLabelFile := true;
            Val(ColLabF.TopEdit.Text,t,error);
            if Error = 0 then RockColumn[pred(UnitNumber)].BasalAge := t;
            Val(ColLabF.BaseEdit.Text,t,error);
            if Error = 0 then BasalAge := t;
            if RockColumn[pred(UnitNumber)].BasalAge > BasalAge then begin
               MessageToContinue('Base must be older than top');
               OK := false;
            end;
         end;
      until OK;
      LongDesc := ColLabF.TextEdit.Text;
   end;
   ColLabF.Free;
end;


procedure ClearColumn(MouseOnColumnNumber : integer; Offscreenbitmap : tMyBitmap);
var
   i : integer;
begin
  with ColMainF,StratColumn[MouseOnColumnNumber],OffScreenBitmap do begin
    Canvas.Pen.Color := clWhite;
    for i := succ(LeftSideColumn) to pred(RightSideColumn) do begin
       Canvas.MoveTo(i,0);
       Canvas.LineTo(i,ScreenYMax);
    end {for i};
  end {with};
end;


function EditRockColumnUnit(HighLight : boolean; Column,Bed : integer) : boolean;
var
   i : integer;
   BtnBottomDlg : TFullUnitEntryDlg;
begin
   {$IfDef RecordStratColProblems} WriteLineToDebugFile('EditRockColumnUnit in'); {$EndIf}
   BtnBottomDlg := TFullUnitEntryDlg.Create(Application);
   if MDDef.ColDef.RapidColEntry then BtnBottomDlg.ShortEntry;
   if HighLight then with ColMainF.Image1.Canvas do begin
      BtnBottomDlg.Caption := 'Edit Selected Unit';
      Pen.Color := clRed;
      with ColMainF,CorrelationDiagram,StratColumn[Column],RockColumn[Bed] do begin
         MoveTo(LeftSideColumn,TopUnitOnCanvas);
         LineTo(LeftSideColumn,BaseUnitOnCanvas);
         LineTo(RightSideColumn,BaseUnitOnCanvas);
         LineTo(RightSideColumn,TopUnitOnCanvas);
         LineTo(LeftSideColumn,TopUnitOnCanvas);
      end;
   end
   else begin
      BtnBottomDlg.Caption := 'Add New Unit (start at top)';
      BtnBottomDlg.OKBtn.Enabled := false;
   end;

   with ColMainF,CorrelationDiagram,BtnBottomDlg,StratColumn[Column],RockColumn[Bed] do begin
      if (NamedPatternLongNames <> Nil) and (NamedPatternLongNames.Count > 0) then begin
         LithListBox.Items := NamedPatternLongNames;
         ComboBox1.Items := NamedPatternLongNames;
         ComboBox1.Text := NamedPatternLongNames[0];
      end;
      RedrawPattern;
      ShortNameEdit.Text := UnitName;
      LongNameEdit.Text := LongDesc;
      ThicknessEdit.Text := RealToString(MeterThick,-18,-6);
      BaseAgeEdit.Text := RealToString(BasalAge,-18,-6);
      TopAgeEdit.Text := RealToString(RockColumn[pred(Bed)].BasalAge,-18,-6);
      BaseWidthEdit.Text := IntegerToString(1*BaseParam2,-8);
      BaseDepthEdit.Text := IntegerToString(1*BaseParam1,-8);
      TopWidthEdit.Text := IntegerToString(1*RockColumn[pred(Bed)].BaseParam2,-8);
      TopDepthEdit.Text := IntegerToString(1*RockColumn[pred(Bed)].BaseParam1,-8);

      LithLabel.Caption := 'Other  ' + UnitLith;
      for i := 1 to NumNamedPatterns do
         if UnitLith =  NamedPatternShortName[pred(i)] then begin
            LithListBox.ItemIndex := pred(i);
            LithLabel.Caption := NamedPatternLongNames[pred(i)];
         end;
      WhatIsTop.Caption := TopListBox.Items[ord(RockColumn[pred(Bed)].UnitBase)];
      TopListBox.ItemIndex := ord(RockColumn[pred(Bed)].UnitBase);
      WhatIsBase.Caption := BaseListBox.Items[ord(UnitBase)];
      BaseListBox.ItemIndex := ord(UnitBase);
      RadioGroup1.ItemIndex := succ(Resistance);

      if (ShowModal = mrCancel) then begin
         EditRockColumnUnit := false;
         exit;
      end;
      EditRockColumnUnit := true;

      Resistance := succ(RadioGroup1.ItemIndex);
      UnitBase := BaseType(BaseListBox.ItemIndex);
      RockColumn[pred(Bed)].UnitBase := BaseType(TopListBox.ItemIndex);
      if Copy(LithLabel.Caption,1,5) = 'Other' then UnitLith := Copy(LithLabel.Caption,8,3)
      else UnitLith := NamedPatternShortName[LithListBox.ItemIndex];

      UnitName := ShortNameEdit.Text;
      LongDesc := LongNameEdit.Text;
      CheckEditString(ThicknessEdit.Text,MeterThick);
      CheckEditString(BaseAgeEdit.Text,BasalAge);
      CheckEditString(TopAgeEdit.Text,RockColumn[pred(Bed)].BasalAge);
      CheckEditString(BaseWidthEdit.Text,BaseParam2);
      CheckEditString(BaseDepthEdit.Text,BaseParam1);
      CheckEditString(TopWidthEdit.Text,RockColumn[pred(Bed)].BaseParam2);
      CheckEditString(TopDepthEdit.Text,RockColumn[pred(Bed)].BaseParam1);
   end;
   BtnBottomDlg.Free;
end;


procedure TColMainF.AddColumn1Click(Sender: TObject);
var
   OffscreenBitmap : tMyBitmap;
begin
   StopSplashing;
   CreateNewColumn;
   with CorrelationDiagram,StratColumn[NumColumns] do begin
      CurrentCorrelationFont[NumColumns] := ColMainF.Font;
      GetLatLongNoDatum('Column location',ColLat,ColLong);
      GetString('Column label',ColumnLabel,false,ReasonableTextChars);
      ColumnFileName := 'New' + IntegerToString(NumColumns,-5) + '.COT';
      UnitsInColumn := 2;
      LeftSideColumn := XOffSet;
      RightSideColumn := XOffset + MDDef.ColDef.PixelsColumnWide;
      inc(XOffset,MDDef.ColDef.PixelsColumnWide);
      RockColumn[1].BasalAge := DiagramTopAge;
      CopyImageToBitmap(Image1,OffscreenBitmap);
      repeat
         RockColumn[UnitsInColumn].BasalAge := DiagramBasalAge;
         if not EditRockColumnUnit(false,NumColumns,UnitsInColumn) then break;
         DrawSingleUnit(OffScreenBitmap.Canvas,NumColumns,UnitsInColumn);
         inc(UnitsInColumn);
         ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
         OffScreenBitmap.Free;
      until false;
      dec(UnitsInColumn);
      ColumnModified := not SaveCOTFile(NumColumns,ColumnFileName);
  end {with};
end;

procedure TColMainF.Image1DblClick(Sender: TObject);
var
   OffScreenBitmap : tMyBitmap;
begin
   with CorrelationDiagram,StratColumn[MouseOnColumnNumber] do begin
      if MouseOnUnit then begin
         CopyImageToBitmap(Image1,OffScreenBitmap);
         if CorrelationDiagram.TimeLabelFile[MouseOnColumnNumber] then begin
            EditLabelFile(MouseOnColumnNumber,MouseOnUnitNumber);
            ClearColumn(MouseOnColumnNumber,Offscreenbitmap);
            DrawALabelFile(OffScreenBitmap.Canvas,MouseOnColumnNumber);
         end
         else begin
            EditRockColumnUnit(True,MouseOnColumnNumber,MouseOnUnitNumber);
            if SimpleEdit then RedrawUnits(OffScreenBitmap.Canvas,MouseOnColumnNumber,MouseOnUnitNumber,MouseOnUnitNumber)
            else RedrawUnits(OffScreenBitMap.Canvas,MouseOnColumnNumber,pred(MouseOnUnitNumber),succ(MouseOnUnitNumber));
         end;
         StratColumn[MouseOnColumnNumber].ColumnModified := true;
         SaveFiles1.Enabled := true;
         ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
         OffScreenBitmap.Free;
         exit;
      end;
      if MouseOnLabel then begin
         CopyImageToBitmap(Image1,OffScreenBitmap);
         if ChangeHeaderInfo(TimeLabelFile[MouseOnColumnNumber],ColLat,ColLong,
            CurrentCorrelationFont[MouseOnColumnNumber],
            CurrentCorrelationDirs[MouseOnColumnNumber],ColumnLabel) then begin
            try
               ShowHourglassCursor;
               ClearColumn(MouseOnColumnNumber,Offscreenbitmap);
               if CorrelationDiagram.TimeLabelFile[MouseOnColumnNumber] then
                  DrawALabelFile(OffScreenBitmap.Canvas,MouseOnColumnNumber)
               else DrawAColumn(OffScreenBitMap.Canvas,MouseOnColumnNumber);
               StratColumn[MouseOnColumnNumber].ColumnModified := true;
               SaveFiles1.Enabled := true;
               ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
               OffScreenBitmap.Free;
            finally
               ShowDefaultCursor;
            end;
         end;
      end;
      exit;
   end;
   PopupMenu1.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TColMainF.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
   i,j : integer;
begin
   if TimeCorrelation1.Enabled and (Diagram <> NothingYet) then
      Panel1.Caption := RealToString(ScreenPositionToAge(y),-12,-3) + ' Ma';
   if MovingBoundary then with Image1.Canvas do begin
      with StratColumn[MouseOnColumnNumber],RockColumn[MouseOnUnitNumber] do begin
         Pen.Color := clRed;
         Pen.Mode := pmNotXor;
         MoveTo(LeftSideColumn,Lasty);
         LineTo(RightSideColumn,Lasty);
         MoveTo(LeftSideColumn,y);
         LineTo(RightSideColumn,y);
         LastY := y;
         Pen.Mode := pmCopy;
      end;
   end
   else with CorrelationDiagram do begin
      MouseOnUnit := false;
      MouseOnBoundary := false;
      MouseOnLabel := false;
      for i := 1 to NumColumns do with StratColumn[i] do
         if (X > LeftSideColumn) and (x < RightSideColumn) then begin
            MouseOnColumnNumber := i;
            for j := 1 to UnitsInColumn do with RockColumn[j] do
               if (y >= BaseUnitOnCanvas-2) and (y <= BaseUnitOnCanvas+2) then begin
                   MouseOnBoundary  := true;
                   MouseOnUnitNumber  := j;
                   exit;
               end;
            for j := 2 to UnitsInColumn do with RockColumn[j] do
               if (y > TopUnitOnCanvas) and (Y < BaseUnitOnCanvas) then begin
                   MouseOnUnit := true;
                   MouseOnUnitNumber  := j;
                   exit;
               end;
            if (Y < AgeSkip) then begin
               MouseOnLabel := true;
               exit;
            end;
         end {If};
   end;
end;


procedure TColMainF.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if MouseOnBoundary then with Image1.Canvas do begin
      Forms.Screen.Cursor := crSizeNS;
      MovingBoundary := true;
      with StratColumn[MouseOnColumnNumber],RockColumn[MouseOnUnitNumber] do begin
         Pen.Color := clRed;
         Pen.Mode := pmNotXor;
         MoveTo(LeftSideColumn,y);
         LineTo(RightSideColumn,y);
         LastY := y;
         Pen.Mode := pmCopy;
      end;
   end
   else if Button = mbRight then begin
       PopupMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;

procedure TColMainF.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   OffscreenBitmap : tMyBitmap;
begin
   if MovingBoundary then begin
      ShowHourglassCursor;
      CopyImageToBitmap(Image1,OffscreenBitmap);
      with StratColumn[MouseOnColumnNumber],RockColumn[MouseOnUnitNumber] do begin
          BasalAge := ScreenPositionToAge(y);
          BaseUnitOnCanvas := y;
          if CorrelationDiagram.TimeLabelFile[MouseOnColumnNumber] then begin
             ClearColumn(MouseOnColumnNumber,OffScreenBitmap);
             DrawALabelFile(OffScreenBitmap.Canvas,MouseOnColumnNumber);
          end
          else RedrawUnits(OffScreenBitMap.Canvas,MouseOnColumnNumber,(MouseOnUnitNumber),
             succ(MouseOnUnitNumber));
          ColumnModified := true;
          SaveFiles1.Enabled := true;
      end {with};
      ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
      OffScreenBitmap.Free;
      MouseOnBoundary := false;
      MovingBoundary := false;
      ShowDefaultCursor;
   end;
end;


procedure TColMainF.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   ZeroVariables;
   Action := caFree;
end;


procedure TColMainF.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
   i : integer;
   OffScreenBitmap : tMyBitmap;
begin
   CopyImageToBitmap(Image1,OffscreenBitmap);
   with StratColumn[MouseOnColumnNumber] do case Key of
     vk_Insert : if MouseOnBoundary then begin
                    if MouseOnUnitNumber < UnitsInColumn then begin
                       for i := UnitsInColumn downto succ(MouseOnUnitNumber) do
                          RockColumn[succ(i)] := RockColumn[i];
                      RockColumn[succ(MouseOnUnitNumber)] := RockColumn[MaxUnitsPerColumn];
                    end;
                    if CorrelationDiagram.TimeLabelFile[MouseOnColumnNumber] then
                       EditLabelFile(MouseOnColumnNumber,succ(MouseOnUnitNumber))
                    else EditRockColumnUnit(true,MouseOnColumnNumber,succ(MouseOnUnitNumber));
                    inc(UnitsInColumn);
                    MouseOnBoundary := false;
                    if CorrelationDiagram.TimeLabelFile[MouseOnColumnNumber] then
                        DrawALabelFile(OffScreenBitmap.Canvas,MouseOnColumnNumber)
                    else RedrawUnits(OffScreenBitmap.Canvas,MouseOnColumnNumber,MouseOnUnitNumber,
                       (MouseOnUnitNumber + 2));
                    ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
                    ColumnModified := true;
                    SaveFiles1.Enabled := true;
                 end;
     vk_Delete : if MouseOnUnit then begin
                    if AnswerIsYes('Confirm delete ' +RockColumn[MouseOnUnitNumber].UnitName) then begin
                       for i := MouseOnUnitNumber to pred(UnitsInColumn) do
                          RockColumn[i] := RockColumn[succ(i)];
                       dec(UnitsInColumn);
                       if CorrelationDiagram.TimeLabelFile[MouseOnColumnNumber] then
                           DrawALabelFile(OffScreenBitmap.Canvas,MouseOnColumnNumber)
                       else RedrawUnits(OffScreenBitmap.Canvas,MouseOnColumnNumber,pred(MouseOnUnitNumber),
                          MouseOnUnitNumber);
                       ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
                       ColumnModified := true;
                       SaveFiles1.Enabled := true;
                    end;
                 end;
   end {case};
   OffscreenBitmap.Free;
end;

procedure TColMainF.SaveImage1Click(Sender: TObject);
begin
   SaveImageAsBMP(Image1);
end;


procedure TColMainF.SaveFiles1Click(Sender: TObject);
begin
   SaveDataFiles;
end;


procedure TColMainF.TimeScale1Click(Sender: TObject);
var
   Done : boolean;
   LabelDir,
   i    : integer;
   FName : PathStr;
   OffscreenBitmap : tMyBitmap;
begin
   StopSplashing;
   RockCorrelation1.Enabled := false;
   with CorrelationDiagram do begin
      StopSplashing;
      CopyImageToBitmap(Image1,OffscreenBitmap);
      ColMainF.RockCorrelation1.Enabled := false;
      MDDef.ColDef.RaggedRightMargin := false;
      if NumColumns = 0 then begin
         DiagramBasalAge := 570;
         DiagramTopAge := 505;
         GetBaseAndTop(DiagramBasalAge,DiagramTopAge);
         try
            ShowHourglassCursor;
            XOffset := 0;
            DrawAgeBar(OffScreenBitMap.Canvas,TickSize);
            ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
         finally
            ShowDefaultCursor;
         end;
      end;
      MessageToContinue('Start with top unit and work downward.');
      CreateNewColumn;
      CorrelationDiagram.TimeLabelFile[NumColumns] := true;
      with StratColumn[NumColumns] do begin
         LeftSideColumn := XOffset;
         if MDDef.ColDef.TextDirection = textHorizontal then begin
            LabelDir := HorizDir;
            RightSideColumn := XOffset + MDDef.ColDef.PixelsColumnWide;
         end
         else begin
            LabelDir := VertDir;
            RightSideColumn := XOffset + 2*OffScreenBitMap.Canvas.Font.Size + 2;
         end;
         RockColumn[1].BasalAge := DiagramTopAge;
         for i := 2 to MaxUnitsPerColumn do begin
            with RockColumn[i] do begin
               BasalAge := DiagramBasalAge;
               LongDesc := '';
            end;
         end;
      end;
      StratColumn[NumColumns].UnitsInColumn := 1;
      OffScreenBitMap.Canvas.Font := CurrentCorrelationFont[NumColumns];
      with StratColumn[NumColumns] do repeat
         inc(UnitsInColumn);
         Done := not EditLabelFile(NumColumns,UnitsInColumn);
         if Done then dec(UnitsInColumn)
         else with RockColumn[UnitsInColumn] do begin
            TopUnitOnCanvas := ScreenPositionInPixels(RockColumn[pred(UnitsInColumn)].BasalAge);
            if TopUnitOnCanvas < 0 then TopUnitOnCanvas := 0;
            if UnitsInColumn = 2 then RockColumn[1].BaseUnitOnCanvas := TopUnitOnCanvas;
            BaseUnitOnCanvas := ScreenPositionInPixels(BasalAge);
            if BaseUnitOnCanvas > ScreenYMax then BaseUnitOnCanvas := ScreenYMax;
            TimeZoneName(OffScreenBitMap.Canvas,LeftSideColumn,RightSideColumn-LeftSideColumn,
               TopUnitOnCanvas,BaseUnitOnCanvas,LabelDir,LongDesc,true,true);
            ModeDraw(OffScreenBitMap.Canvas,LeftSideColumn,TopUnitOnCanvas,LeftSideColumn,BaseUnitOnCanvas,DrawingColor);
            ModeDraw(OffScreenBitMap.Canvas,RightSideColumn,TopUnitOnCanvas,RightSideColumn,BaseUnitOnCanvas,DrawingColor);
         end;
         ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
      until Done;
      OffScreenBitmap.Free;
      XOffset := StratColumn[NumColumns].RightSideColumn;

      fName := MainMapData + 'stratcol\';

      if Petmar.GetFileNameDefaultExt('STRATCOL','.COT',fName) then begin
          WriteLabelFile(FName,NumColumns);
          StratColumn[NumColumns].ColumnModified := false;
       end;
   end {with};
end;

procedure TColMainF.ClipboardSpeedButtonClick(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TColMainF.CloseDiagram1Click(Sender: TObject);
begin
   if AnswerIsYes('Confirm close diagram') then begin
      ZeroVariables;
      RescaleDiagram;
      MenuOptionsEnabled;
      TimeCorrelation1.Enabled := true;
      RockCorrelation1.Enabled := true;
   end;
end;

procedure TColMainF.InitializeThicknessCorrelation;
var
   OffscreenBitmap : tMyBitmap;
begin
   Diagram := ThicknessCorrelation;
   TimeCorrelation1.Enabled := false;
   CopyImagetoBitmap(Image1,OffscreenBitmap);
   with CorrelationDiagram do if (NumColumns = 0) then begin
      AgeScale := false;
      XOffset := 0;
      MDDef.ColDef.DefaultThickness := DiagramThickness;
      DiagramTopAge := 0;
      DiagramBasalAge := DiagramThickness;
      DrawThicknessScaleBar(OffScreenBitmap.Canvas,0);
      ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
   end;
   OffscreenBitmap.Free;
   StopSplashing;
end;


procedure TColMainF.Columnfromfile2Click(Sender: TObject);
begin
   NewFileToDiagram('');
end;

procedure TColMainF.NewFileToDiagram(fName : PathStr);
var
   OffscreenBitmap : tMyBitmap;
begin
   {$IfDef RecordStratColProblems} WriteLineToDebugFile('TColMainF.NewFileToDiagram ' + fName);{$EndIf}
   InitializeThicknessCorrelation;
   CopyImagetoBitmap(Image1,OffscreenBitmap);
   GetRockColumn(OffScreenBitMap.Canvas,fName);
   ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
   OffscreenBitmap.Free;
   RedrawCorrelationDiagram;
   MenuOptionsEnabled;
   StopSplashing;
   {$IfDef RecordStratColProblems} WriteLineToDebugFile('TColMainF.NewFileToDiagram out'); {$EndIf}
end;

procedure TColMainF.Labelfile1Click(Sender: TObject);
var
   DirCh : AnsiChar;
   OffscreenBitmap : tMyBitmap;
begin
   CopyImagetoBitmap(Image1,OffscreenBitmap);
   InitializeThicknessCorrelation;
   if (MDDef.ColDef.TextDirection = textVertical) then DirCh := 'V' else DirCh := 'H';
   ReadTimeColumn(OffScreenBitMap.Canvas,false,DirCh,'');
   StopSplashing;
   ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
   OffscreenBitmap.Free;
end;


procedure TColMainF.Savediagram1Click(Sender: TObject);
{$IfDef AllowCorrelationFile}
var
   fName : PathStr;
begin
   fName := MainMapData + 'stratcol\' +  ProjectName;
   with CorrelationDiagram, ColMainF do
     if Petmar.GetFileNameDefaultExt('STRATCOL Project','.CPJ',fName) then begin
      ProjectName := fName;
     {$IfDef RecordStratColProblems} WriteLineToDebugFile('Saving stratcol project in ' + ProjectName); {$EndIf}
      try
         assignFile(TFile,ProjectName);
         rewrite(TFile);
         write(TFile,CorrelationDiagram);
        {$IfDef RecordStratColProblems} WriteLineToDebugFile('Save success'); {$EndIf}
      finally
         closeFile(TFile);
      end;
   end;
{$Else}
begin
{$EndIf}
end;


procedure TColMainF.OpenDiagram1Click(Sender: TObject);
{$IfDef AllowCorrelationFile}
var
   i,j : integer;
   Ext     : ExtStr;
   OffscreenBitmap : tMyBitmap;
begin
   with CorrelationDiagram do begin
      Diagram := ThicknessCorrelation;
      StopSplashing;
      if not GetFileFromDirectory('Correlation Project','*.CPJ',ProjectName) then exit;
      try
        {$IfDef RecordStratColProblems} WriteLineToDebugFile('Open stratcol project in ' + ProjectName); {$EndIf}
         ShowHourglassCursor;

         assignFile(TFile,ProjectName);
         reset(TFile);
         read(Tfile,CorrelationDiagram);
         closeFile(TFile);

         InitializeCorrelationDiagram;
         CopyImagetoBitmap(Image1,OffscreenBitmap);
         DrawThicknessScaleBar(OffScreenBitmap.Canvas,0);
         j := NumColumns;
         NumColumns := 0;
         for i := 1 to j do begin
           {$IfDef RecordStratColProblems}
           WriteLineToDebugFile('Open file ' + CurrentCorrelationFiles[i]);
           {$EndIf}

            Ext := UpperCase(ExtractFileNameNoExt(CurrentCorrelationFiles[i]));
            if ExtEquals(Ext, '.COT') or ExtEquals(Ext, '.COL')  then begin
               GetRockColumn(OffScreenBitMap.Canvas,CurrentCorrelationFiles[i]);
            end
            else if copy(Ext,1,3) = '.FO' then begin
               inc(XOffset,15);
               PlotTheTime(OffscreenBitmap.Canvas,i,true,true,10);
            end
            else begin
               ReadTimeColumn(OffScreenBitMap.Canvas,false,
                  CurrentCorrelationDirs[i],CurrentCorrelationFiles[i]);
               DrawALabelFile(OffScreenBitMap.Canvas,NumColumns);
            end;
         end;
         ColMainF.Image1.Picture.Graphic := OffScreenBitmap;

        {$IfDef RecordStratColProblems}
        WriteLineToDebugFile('Done reading');
        {$EndIf}

         MenuOptionsEnabled;
      finally
         OffscreenBitmap.Free;
         ShowDefaultCursor;
      end;
   end;
{$Else}
begin
{$EndIf}
end;


procedure TColMainF.Redraw1Click(Sender: TObject);
begin
   RedrawCorrelationDiagram;
end;


procedure TColMainF.Column1Click(Sender: TObject);
var
   OffscreenBitmap : tMyBitmap;
begin
   {$IfDef RecordStratColProblems}
   WriteLineToDebugFile('TColMainF.Column1Click enter');
   {$EndIf}
   CopyImagetoBitmap(Image1,OffscreenBitmap);
   TimeCorrelation1.Enabled := false;
   InitializeThicknessCorrelation;
   CreateNewColumn;
   CorrelationDiagram.CurrentCorrelationFont[CorrelationDiagram.NumColumns] := ColMainF.Font;
   if MDDef.ColDef.EnterLatLong then begin
      GetLatLongNoDatum('Column location',StratColumn[CorrelationDiagram.NumColumns].ColLat,StratColumn[CorrelationDiagram.NumColumns].ColLong);
   end;
   GetString('Column label',StratColumn[CorrelationDiagram.NumColumns].ColumnLabel,false,ReasonableTextChars);
   with CorrelationDiagram,StratColumn[NumColumns] do begin
      ColumnFileName := 'New' + IntegerToString(NumColumns,-5) + '.COT';
      UnitsInColumn := 2;
      LeftSideColumn := XOffSet;
      RightSideColumn := XOffset + MDDef.ColDef.PixelsColumnWide;
      inc(XOffset,MDDef.ColDef.PixelsColumnWide + MDDef.ColDef.ColumnSeparation);
      RockColumn[1].BasalAge := DiagramTopAge;
      repeat
         RockColumn[UnitsInColumn].BasalAge := DiagramBasalAge;
         if not EditRockColumnUnit(false,NumColumns,UnitsInColumn) then break;
         if StratColumn[NumColumns].RockColumn[UnitsInColumn].MeterThick > 0.0001 then begin
            DrawSingleUnit(OffScreenBitmap.Canvas,NumColumns,UnitsInColumn);
            inc(UnitsInColumn);
            ColMainF.Image1.Picture.Graphic := OffScreenBitmap;
         end;
      until false;
      dec(UnitsInColumn);

      ColumnModified := not SaveCOTFile(NumColumns,ColumnFileName);
  end {with};
end;


procedure TColMainF.FossilRanges1Click(Sender: TObject);
var
   FileName   : PathStr;
   ThisFile   : system.text;
   NumFoss    : integer;
   OffscreenBitmap : tMyBitmap;
begin
   CopyImagetoBitmap(Image1,OffscreenBitmap);
   GetFileFromDirectory('fossil ranges','*.FO*',FileName);
   if FileName = '' then exit;
   with CorrelationDiagram do begin
      CreateNewColumn;
      CurrentCorrelationFiles[NumColumns] := FileName;
      AssignFile(ThisFile,FileName);
      reset(ThisFile);
      NumFoss := 0;
      while not EOF(ThisFile) do begin
         inc(NumFoss);
         readln(ThisFile);
      end;
      closeFile(ThisFile);
      CurrentCorrelationStart[NumColumns] := XOffset + 15;
      CurrentCorrelationEnd[NumColumns] := CurrentCorrelationStart[NumColumns] +
         NumFoss * OffscreenBitmap.Canvas.TextHeight('E');
      OffscreenBitmap.Canvas.Font := CorrelationDiagram.CurrentCorrelationFont[NumColumns];
      SetFossilRangeOptions(NumFoss,OffscreenBitmap.Canvas.TextHeight('E'),
         OffscreenBitmap.Width,CurrentCorrelationStart[NumColumns],CurrentCorrelationEnd[NumColumns]);
      PlotTheTime(OffscreenBitmap.Canvas,NumColumns,true,true,10);
      XOffset := CurrentCorrelationEnd[NumColumns] + 15;
   end;

   Image1.Picture.Graphic := OffScreenBitmap;
   OffScreenBitmap.Free;
end;


procedure Convert(Fossils : boolean);
var
   FileName : PathStr;
   inf,outf : System.Text;
   Ext      : ExtStr;
   Base,Top : float64;
   Words    : ShortString;
   TStr  : ShortString;
begin
   if Fossils then GetFileFromDirectory('fossil ranges','*.FO?',FileName)
   else GetFileFromDirectory('time zones','*.TM?',FileName);
   if FileName <> '' then begin
     assign(Inf,FileName);
     reset(Inf);
     Ext := ExtractFileExt(FileName);
     Delete(Ext,1,1);
     if Fossils then TStr := 'new metric fossil ranges'
     else TStr := 'new metric time zones';
     GetFileNameDefaultExt(TStr,'Files|*'+Ext,FileName);
     assign(OutF,FileName);
     rewrite(OutF);
     while not EOF(Inf) do begin
        {$I-} readln(Inf,Base,Top,Words);  {$I+}
        if IOresult = 0 then begin
           TrimLeft(Words);
           writeln(OutF,(Base*FeetToMeters):12:4,(Top*FeetToMeters):15:4,'   ',Words)
        end;
     end {while};
     close(Inf);
     close(OutF);
   end;
end;


procedure TColMainF.Timefiles1Click(Sender: TObject);
begin
   Convert(false);
end;

procedure TColMainF.Fossilranges2Click(Sender: TObject);
begin
   Convert(true);
end;

procedure TColMainF.Ticksize1Click(Sender: TObject);
begin
   ReadDefault('Tick interval',TickSize);
   RedrawCorrelationDiagram;
end;

procedure TColMainF.Present1Click(Sender: TObject);
begin
   MDDef.ColDef.ShowAgeBar := not MDDef.ColDef.ShowAgeBar;
   Present1.Checked := MDDef.ColDef.ShowAgeBar;
end;

procedure TColMainF.Rescalethickness1Click(Sender: TObject);
begin
   with CorrelationDiagram do begin
      ReadDefault('Thickness for diagram (m)',DiagramThickness);
      DiagramBasalAge := DiagramThickness;
      MDDef.ColDef.DefaultThickness := DiagramThickness;
      TickSize := -1;
      RedrawCorrelationDiagram;
   end;
end;

procedure TColMainF.Redraw2Click(Sender: TObject);
begin
   RedrawCorrelationDiagram;
end;

procedure TColMainF.Columns1Click(Sender: TObject);
begin
   FontDialog1.Font := MainDefaultFont;
   if FontDialog1.Execute then begin
      MainDefaultFont := FontDialog1.Font;
      MDDef.ColDef.DefaultFontSize := FontDialog1.Font.Size;
   end;
end;


procedure TColMainF.Scalebar1Click(Sender: TObject);
begin
   FontDialog1.Font := CorrelationDiagram.ScaleBarFont;
   if FontDialog1.Execute then begin
      CorrelationDiagram.ScaleBarFont := FontDialog1.Font;
      MDDef.ColDef.DefaultScaleFontSize := FontDialog1.Font.Size;
   end;
end;


procedure TColMainF.Select1Click(Sender: TObject);
begin
   GetFileFromDirectory('pattern names','*.NAM',LithFileName);
   InitializeNamedPatterns;
end;


procedure TColMainF.Create1Click(Sender: TObject);
var
   //NamFile : textfile;
   i : integer;
   tf : tStringList;
begin
   tf := tStringList.Create;

   for i := 1 to NumStandardPattern do begin
      Tf.Add(StandardPattern^[i].PatternName + ' Pattern ' + StandardPattern^[i].PatternName);
   end;
   tf.SaveToFile(ProgramRootDir + 'all_lith_patterns.nam');
   tf.Free;

   PatternF := TPatternF.Create(Application);
   sc_ColLith.PatternF.Visible := false;
   PatternF.CreatingNAMFile := true;
   AssignFile(PatternF.NamFile,ProgramRootDir + 'qqtempzz.nam');
   rewrite(PatternF.NamFile);
   sc_ColLith.PatternF.ShowModal;
   PatternF.Destroy;
end;


procedure TColMainF.Grid1Click(Sender: TObject);
begin
   Grid1.Checked := not Grid1.Checked;
end;

procedure TColMainF.EditLithologyPatterns1Click(Sender: TObject);
begin
   MapPatternEditor;
end;

procedure MapPatternEditor;
begin
   StopSplashing;
   PatternF := TPatternF.Create(Application);
   sc_ColLith.PatternF.Visible := false;
   sc_ColLith.PatternF.ShowModal;
   PatternF.Destroy;
end;

procedure TColMainF.Tickincrement1Click(Sender: TObject);
begin
   ReadDefault('Tick increment',TickSize);
   RedrawCorrelationDiagram;
end;

procedure TColMainF.Columnheight1Click(Sender: TObject);
begin
   Rescalethickness1Click(Sender);
end;


procedure TColMainF.Display1Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := ProgramRootDir;
   if GetFileFromDirectory('pattern names','*.NAM',fName) then DisplayDSDPPatterns(fName);
end;


procedure TColMainF.Set1Click(Sender: TObject);
begin
   {$IfDef MICRODEM}
   sc_ColOpts.SetStratcolOptions;
   {$EndIf}
end;


procedure TColMainF.Setverticalpixelsize1Click(Sender: TObject);
var
   ps : float64;
begin
   ps := GetPixelSize;
   ReadDefault('Pixel size',ps);
   MDDef.ColDef.DefaultMyBitmapHeight := 25 + round(abs(CorrelationDiagram.DiagramThickness)/ps) + AgeSkip;
   RedrawCorrelationDiagram;
end;

procedure TColMainF.Help1Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\stratcol\stra0x4c.htm');
end;


procedure TColMainF.RedrawSpeedButton12Click(Sender: TObject);
begin
   RedrawCorrelationDiagram;
end;


procedure TColMainF.SpeedButton1Click(Sender: TObject);
begin
   Columnfromfile2Click(Sender);
end;


procedure TColMainF.SpeedButton2Click(Sender: TObject);
begin
   Column1Click(Sender);
end;

procedure TColMainF.CreateGISDB1Click(Sender: TObject);
{$IfDef ExStratcol}
begin
{$Else}
var
   fName : PathStr;
   Table1 : tMyData;
   TheFiles : tStringList;
   i : integer;
   ThisColumn : OneColumnType;
begin
   {$IfDef RecordStratColDBProblems} WriteLineToDebugFile('TColMainF.CreateGISDB1Click in'); {$EndIf}
   fName := MainMapData + 'stratcol\stratcol' + DefaultDBExt;
   if not FileExists(fName) then begin
      Make_Tables.MakeStatColGIS(fName);
   end;
   {$IfDef RecordStratColDBProblems} WriteLineToDebugFile('Table made and repaired'); {$EndIf}
   Table1 := tMyData.Create(FName);

   {$IfDef RecordStratColDBProblems} WriteLineToDebugFile('DB opened'); {$EndIf}
   TheFiles := Nil;
   FindMatchingFiles(MainMapData + 'stratcol\','*.CO*',TheFiles,3);
   {$IfDef RecordStratColDBProblems} WriteLineToDebugFile('Found files: ' + IntToStr(TheFiles.Count)); {$EndIf}

   for i := 0 to pred(TheFiles.Count) do begin
      ThisColumn.ColumnTotalThick := 0;
      fName := TheFiles.Strings[i];
      {$IfDef RecordStratColDBProblems} WriteLineToDebugFile('fName'); {$EndIf}
      ReadTheFile(fName,ThisColumn);
      Table1.Insert;
      Table1.SetFieldByNameAsString('COL_NAME',fName);
      Table1.SetFieldByNameAsString('NAME',ThisColumn.ColumnLabel);
      Table1.SetFieldByNameAsFloat('LAT',ThisColumn.ColLat);
      Table1.SetFieldByNameAsFloat('LONG',ThisColumn.ColLong);
      Table1.SetFieldByNameAsFloat('THICK',ThisColumn.ColumnTotalThick);
      Table1.Post;
   end;
   Table1.Destroy;
{$EndIf}
end;


procedure InitialStatColOptions;
//var
  // i : integer;
begin
{$IfDef MessageStartUpUnitProblems} MessageToContinue('start colmain initialization'); {$EndIf}
   NamedPatternLongNames := Nil;
   MouseOnBoundary := false;
   MovingBoundary := false;
   TicksPerResist := 5;
   MaxDrift := 8;
   ProjectName := '';
{$IfDef MessageStartUpUnitProblems}  MessageToContinue('end colmain initialization');{$EndIf}
end;


initialization
   InitialStatColOptions;
finalization
end {unit}.


