unit contgraf;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordDetailedXYZProblems}
   //{$Define RecordXYZProblems}
   //{$Define RecordTINProblems}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseBDETables}
      dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end needed for inline of the core DB functions

  Vcl.Menus, Vcl.Dialogs, Vcl.ToolWin, Vcl.ComCtrls, Vcl.Controls, Vcl.ExtCtrls,
  Windows, Messages, SysUtils, Classes, Graphics,  Forms,  Buttons,

   {$IfDef ExTin}
   {$Else}
      DEM_TIN,
   {$EndIf}

  BASEGRAF,PETMAR,Petmar_types, PETMath,PetDBUtils,DEMDefs;

const
   MaxGridSize  = 100;
type
   DataRecord = Record
      NumCol,NumRow     : Int16;
      XMinGr,YMinGr     : float32;
      XIncGr,YIncGr     : float32;
      zmin,zmax         : float32;
      MinValue,MaxValue : int16;  { extreme z values in ValueArray }
      ValueArray : array[0..MaxGridSize,0..MaxGridSize] of Int16;
   end;

type
  tWhatsOnGraph = (TriangulationContour,GridContour,GridPoints,XYZPoints,NothingSpecial);
  ColorFunctType = function(z : integer) : TColor;
  GetElevColProcType = procedure(x : integer; var z : tElevColPointer);

  TThisContourGraph = class(TThisBaseGraph)
    Recontour1: TMenuItem;
    Contouroptions1: TMenuItem;
    Graphsize1: TMenuItem;
    TINInterpolateButton: TSpeedButton;
    Setoptions1: TMenuItem;
    procedure TINInterpolateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Recontour1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Graphparameters1Click(Sender: TObject);
    procedure Graphsize1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton8Click(Sender: TObject);
    procedure Setoptions1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ContInterval : integer;
    WhatsOnGraph : tWhatsOnGraph;
    cf : ColorFunctionType;
    IntCF : ColorFunctType;
    RoamPossible,
    ColorCodePlot,
    LabelPoints : boolean;
    TINfName : PathStr;
    {$IfDef ExTin}
    {$Else}
       TIN : tTin;
    {$EndIf}
    procedure Contour;
    procedure DefaultContour(FileName : PathStr);
    procedure DrawContours(DrawCanvas : TCanvas; ContInt,MinElev,LeftGrid,BottGrid,RightGrid, TopGrid : integer;  ColorFunction : ColorFunctType; GetElevColProc : GetElevColProcType;  TopCont,BotCont : integer);
    procedure PlotXYZPoints(Color : tPlatformColor; Symbol : tDrawingSymbol);
    procedure PlotGrid(Color : tPlatformColor; Symbol : tDrawingSymbol);
  end;


function IntConstantColor(z : integer) : TPlatformColor;
procedure ProcessXYZFile(ASCIIFileName : PathStr; var xyzTableName : PathStr; ZValues1Checked,XandY1Checked : boolean;  XFieldName,YFieldName,ZFieldName : ShortString;  PointTolerance : float32);
procedure DisplayDataSet;
procedure FigureIncrements;
procedure CreateNewGraph(var PlotForm : TThisContourGraph);

var
   ContourData    : ^DataRecord;
   ScreenXMax,ScreenYMax,Sub   : integer;
   NumDataPoints : integer;
   XRange,YRange,DataX,
   PointTolerance,
   xIncrGrd,YIncrGrd,
   XMax,YMax : float32;
   UserContourColor     : tPlatformColor;
   LabelValues     : boolean;
   ThisContourGraph: TThisContourGraph;
   Pnt   : tPointerPnt;


implementation

{$R *.DFM}


uses
   Nevadia_Main,
   contouroptsform,
   PETImage;


procedure CreateNewGraph(var PlotForm : TThisContourGraph);
var
   xs,ys : float32;
begin
   PlotForm := TThisContourGraph.Create(Application);
   PlotForm.GraphDraw.CorrectScaling := true;
   PlotForm.ScrollGraph := true;
   PlotForm.ClientHeight := MDDef.ClouderYSize;
   PlotForm.ClientWidth := MDDef.ClouderXSize;

   if (ContourData <> Nil) then begin
      PlotForm.GraphDraw.MinHorizAxis := ContourData^.XMinGr;
      PlotForm.GraphDraw.MaxHorizAxis := XMax;
      PlotForm.GraphDraw.MinVertAxis := ContourData^.YMinGr;
      PlotForm.GraphDraw.MaxVertAxis := YMax;
      PadAxis(PlotForm.GraphDraw.MinHorizAxis,PlotForm.GraphDraw.MaxHorizAxis);
      PadAxis(PlotForm.GraphDraw.MinVertAxis,PlotForm.GraphDraw.MaxVertAxis);
      xs := PlotForm.ClientWidth - PlotForm.GraphDraw.LeftMargin;
      ys := PlotForm.ClientHeight - PlotForm.GraphDraw.BottomMargin;
      xs := (PlotForm.GraphDraw.MaxHorizAxis - PlotForm.GraphDraw.MinHorizAxis) / xs;
      ys := (PlotForm.GraphDraw.MaxVertAxis - PlotForm.GraphDraw.MinVertAxis) / ys;
      if (XS/ys < 5) and (xs/ys > 0.2) then begin
         if xs < ys then PlotForm.ClientWidth := round( (PlotForm.GraphDraw.MaxHorizAxis - PlotForm.GraphDraw.MinHorizAxis) / ys + PlotForm.GraphDraw.LeftMargin)
         else PlotForm.ClientHeight := round( (PlotForm.GraphDraw.MaxVertAxis - PlotForm.GraphDraw.MinVertAxis) / xs + PlotForm.GraphDraw.BottomMargin);
      end;
      PlotForm.SetUpGraphForm;
      PlotForm.Rescale1.Visible := false;
      PlotForm.Option1.Visible := false;
   end;
end;


procedure FigureIncrements;
begin
   XRange := XMax - ContourData^.XMinGr;
   YRange := YMax - ContourData^.YMinGr;
   XIncrGrd := ScreenXMax / XRange;
   YIncrGrd := ScreenYMax / YRange;
   if (YMax - ContourData^.YMinGr > XMax - ContourData^.XMinGr) then DataX := YMax - ContourData^.YMinGr
   else DataX := XMax - ContourData^.XMinGr;
end;


procedure DisplayDataSet;
begin
   MessageToContinue('Number Data Points =' + IntegerToString(NumDataPoints,4) + MessLineBreak +
     'X values:' + RealToString(ContourData^.XMinGr,8,1) + ' to' + realToString(XMax,8,1) + MessLineBreak +
     'Y values:' + RealToString(ContourData^.YMinGr,8,1) + ' to' + realToString(YMax,8,1) + MessLineBreak +
     'Z values:' + RealToString(ContourData^.ZMin,8,1) + ' to' + realToString(ContourData^.ZMax,8,1) + MessLineBreak);
end;


procedure ProcessXYZFile(ASCIIFileName : PathStr; var xyzTableName : PathStr; ZValues1Checked,XandY1Checked : boolean; XFieldName,YFieldName,ZFieldName : ShortString; PointTolerance : float32);
label
   AnotherDuplicateImpossible,
   SubsetData;
var
   i,Offset  : integer;
   xyzTable : tMyData;

begin
   {$IfDef RecordXYZProblems} WriteLineToDebugFile('ProcessXYZFile in ' + ASCIIFileName); {$EndIf}

{Offset is where to start indexing the data; triangulation contouring algorithm uses three dummy values in positions 1 to 3 of the data array}
   if (ContourData = Nil) then begin
      New(ContourData);
      for i := 1 to 3 do if Pnt[i] = Nil then New(Pnt[i]);
   end;

   With ContourData^ do begin
      ConvertXYZASCIItoDBF(Nil,ASCIIFileName,xyzTableName,xMinGr,yMinGr,xMax,yMax,zMin,zMax,XFieldName,YFieldName,ZFieldName,true);
      Offset := 3;
      StartProgress('Read triples');
      Sub := Offset;
      xyzTable := tMyData.Create(xyzTableName,dbmCDS);
      while not xyzTable.Eof do begin
         if (ZFieldName = '') or (xyzTable.GetFieldByNameAsString(ZFieldName) <> '') then begin
            inc(Sub);
            if (sub mod 100 = 0) then UpdateProgressBar(sub/XYZTable.RecordCount);
            Pnt[1]^[Sub] := xyzTable.GetFieldByNameAsFloat(XFieldName);
            Pnt[2]^[Sub] := xyzTable.GetFieldByNameAsFloat(YFieldName);
            if (ZFieldName <> '') then Pnt[3]^[Sub] := xyzTable.GetFieldByNameAsFloat(ZFieldName)
            else Pnt[3]^[Sub] := 1;
            if AddDelauneyImage then begin
               Pnt[4]^[Sub] := xyzTable.GetFieldByNameAsInteger('X_IMAGE');
               Pnt[5]^[Sub] := xyzTable.GetFieldByNameAsInteger('Y_IMAGE');
            end;

            {$IfDef RecordDetailedXYZProblems} WriteLineToDebugFile(IntToStr(Sub) + RealToString(Pnt[1]^[Sub],12,3) + RealToString(Pnt[2]^[Sub],12,3) + RealToString(Pnt[3]^[Sub],12,3)); {$EndIf}

            if (Sub = PetMath.MaxContourPoints) then begin
               MessageToContinue('Too many points in file; truncated.');
               break;
            end;
            for i := succ(Offset) to pred(Sub) do begin
               if (abs(Pnt[1]^[Sub] - Pnt[1]^[i]) < PointTolerance) and (abs(Pnt[2]^[Sub] - Pnt[2]^[i]) < PointTolerance) then begin
                 {$IfDef RecordDetailedXYZProblems}
                    WriteLineToDebugFile('Pt retained:                   ' +  RealToString(Pnt[1]^[i],18,6) + RealToString(Pnt[2]^[i],18,6) + RealToString(Pnt[3]^[i],18,6));
                    WriteLineToDebugFile('  Pt < PointTolerance ignored: ' +  RealToString(Pnt[1]^[Sub],18,6) + RealToString(Pnt[2]^[Sub],18,6) + RealToString(Pnt[3]^[Sub],18,6));
                 {$EndIf}
                 dec(Sub);
                 goto AnotherDuplicateImpossible;
              end {if};
            end {for i};
         end;
        AnotherDuplicateImpossible:;
        xyzTable.Next;
      end;

      xyzTable.Destroy;
      EndProgress;
      NumDataPoints := Sub - Offset;
      {$IfDef RecordXYZProblems} WriteLineToDebugFile('Pnts in data set: ' + IntToStr(NumDataPoints)); {$EndIf}

      if ZValues1Checked or XandY1Checked then begin
         for i := Succ(Offset) to Sub do begin
            if XandY1Checked then begin
               Pnt[1]^[i] := Pnt[1]^[i] - XMinGr;
               Pnt[2]^[i] := Pnt[2]^[i] - YMinGr;
            end;
            if ZValues1Checked then Pnt[3]^[i] := Pnt[3]^[i] - ZMin;
         end;
         if ZValues1Checked then begin
            ZMax := ZMax - ZMin;
            ZMin := 0;
         end;
         if XandY1Checked then begin
            XMax := XMax - XMinGr;
            YMax := YMax - YMinGr;
            XMinGr := 0;
            YMinGr := 0;
         end;
      end;
   end {with};
   {$IfDef RecordXYZProblems} WriteLineToDebugFile('ProcessXYZFile out'); {$EndIf}
end;


{$F+}
function IntMapColorFunct(z : integer) : TColor;
begin
   IntMapColorFunct := RainbowColorFunct(z,ContourData^.MinValue,ContourData^.MaxValue);
end {function};


function IntConstantColor(z : integer) : TPlatformColor;
begin
   IntConstantColor := UserContourColor;
end {function};


procedure GetColumnOfElevs(x : integer; var z : tElevColPointer);
var
   i : integer;
begin
   for i := 0 to ContourData^.NumRow do z^[i] := ContourData^.ValueArray[x,i];
end;


procedure TThisContourGraph.Contour;
begin
   RoamPossible := false;
   if (WhatsOnGraph = GridContour) then begin
      with ContourData^ do begin
         ContInterval := (MaxValue - MinValue) div 50;
         if (ContInterval < 1) then ContInterval := 1;
      end {with};
   end;
   {$IfDef ExTin}
   {$Else}
   if (WhatsOnGraph = TriangulationContour) then with ContourData^ do begin
      UserContourInterval := 0.01;
      while UserContourInterval < ((ZMax - ZMin) / pred(PetMath.MaxContours)) do UserContourInterval := UserContourInterval * 10;
      TIN.TinContourInterval := UserContourInterval;
   end;
   {$EndIf}
   DefaultContour(TinfName);
   RoamPossible := true;
end;


procedure TThisContourGraph.DefaultContour(FileName : PathStr);
var
   BotCont,TopCont,i,
   NumContourLines : integer;
   Dir : DirStr;
   bName : NameStr;
   Ext : ExtStr;
   Bitmap : tMyBitmap;
begin
   RoamPossible := false;
   if (WhatsOnGraph = TriangulationContour) then begin
      fSplit(FileName,Dir,bName,Ext);
      TinfName := FileName;

      {$IfDef RecordTINProblems}
         WriteLineToDebugFile('TThisContourGraph.DefaultContour, tinfname='+ TinFName);
         WriteLineToDebugFile('   filename=' + FileName + '   C.I.=' + RealToString(UserContourInterval,-18,-4));
      {$EndIf}

      if LabelPoints then PlotXYZPoints(ConvertTColorToPlatformColor(clRed),FilledBox);

      Caption := FileName + ' Triangulation Contours, CI=' + RealToString(UserContourInterval,-8,-3);
      if (UserContourInterval > 0.0001) then with ContourData^ do begin
         for i := 4 to NumDataPoints + 3 do Pnt[3]^[i] := Pnt[3]^[i] / UserContourInterval;
         NumContourLines := succ(succ(trunc((ZMax - ZMin) / UserContourInterval)));
         PetImage.CopyImageToBitmap(Image1,Bitmap);
         Accord(Bitmap,1,NumContourLines,NumDataPoints,Pnt,XMinGr,YMinGr,ZMin / UserContourInterval,DataX,CF,TINfName);
         Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;
         for i := 4 to NumDataPoints + 3 do Pnt[3]^[i] := Pnt[3]^[i] * UserContourInterval;
      end;
   end;

   if (WhatsOnGraph = GridContour) then begin
      Caption := FileName + ' Grid Contours, CI=' +  IntToStr(ContInterval);
      with ContourData^ do begin
         BotCont := succ(MinValue div ContInterval div 5) * ContInterval * 5;
         TopCont := (MaxValue div ContInterval div 5) * ContInterval * 5;
         StartProgress('Contour');
         DrawContours(Image1.Canvas,ContInterval,MinValue,0,0,NumCol,NumRow,IntCF,GetColumnOfElevs,TopCont,BotCont);
         EndProgress;
      end {with};
   end;
   RoamPossible := true;
end;


procedure TThisContourGraph.FormCreate(Sender: TObject);
begin
   inherited;
   {$IfDef ExTin}
   {$Else}
      TIN := Nil;
   {$EndIf}
   IntCf := IntMapColorFunct;
   GraphDraw.AxisColor := clBlack;
   WhatsOnGraph := NothingSpecial;
   LabelPoints := false;
   ColorCodePlot := true;
   GraphDraw.ZColorLegend := true;
   GraphDraw.BottomMargin := 75;
   RoamPossible := false;
end;


procedure TThisContourGraph.Recontour1Click(Sender: TObject);
begin
   inherited;
   ReadDefault('Contour interval',ContInterval);
   SetUpGraphForm;
   DefaultContour(TinfName);
end;


procedure TThisContourGraph.DrawContours(DrawCanvas : TCanvas; ContInt,MinElev,LeftGrid,BottGrid,RightGrid,
    TopGrid : integer;  ColorFunction : ColorFunctType;
    GetElevColProc : GetElevColProcType;  TopCont,BotCont : integer);

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{ Prototype for routine written by Cadet Karen R. Gorkowski '85 in }
{ UCSD Pascal for the Damn Fine Sage IV.  Modified and translated  }
{ into Turbo Pascal for IBM PC and clones by CPT Peter L. Guth.    }
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Contouring Algorithm:                                 }
{ 1. Each four data points define a rectangle.          }
{ 2. Calculate where contour lines cross each side of   }
{  the rectangle (linear interpolation between corners).}
{ 3. Connect same elevation points across rectangle.    }
{ 4. Move to the next rectangle (start in lower left and}
{   move up, then right, doing one column at a time).   }
{_______________________________________________________}

    label
      NoFullSquare,StartAgain;
    type
       RlSide    = array[1..4,1..MaxContoursPerGrid] of float32;
       IntSide   = array[1..4,1..MaxContoursPerGrid] of integer;
       IntCorner = array[1..4] of integer;
       ElevArray = array[1..5] of integer; {elevations around point: 1=SW, 2=SE, 3=NE, 4=NW, 5=1=SW}
    var
       Elv                      : ElevArray;
       SideZ,XPix,YPix          : IntSide;
       Point                    : RlSide;
       DifElv,ElvPer,Tested     : IntCorner;
       Side,DesiredContInt,
       ElevShift,
       Side4,
       NSGrid,EWGrid,cnt,Num,side1  : integer;
       Found : boolean;
       tzCol,ZLeft : tElevColPointer;


    procedure CompareForContourLines;
    var
       Num : integer;
    begin
       Num := 1;
       while not(Found) and (Num <= ElvPer[Side]) do begin
          if (SideZ[Side1,Cnt] = SideZ[Side,Num]) then begin
             Found := true;
             DrawCanvas.Pen.Color := ColorFunction(SideZ[Side,Num]-ElevShift);
             DrawCanvas.MoveTo(XPix[Side1,Cnt],YPix[Side1,Cnt]);
             DrawCanvas.LineTo(XPix[Side,Num],YPix[Side,Num]);
             SideZ[Side1,Cnt] := 1;
             SideZ[Side,Num] := 1;
             inc(Tested[Side1]);
             inc(Tested[Side]);
          end {if};
          inc(Num);
       end {while};
    end {proc Compare};


begin {proc DrawContours}
   new(tzCol);
   new(ZLeft);
   with DrawCanvas,ContourData^ do begin
     DesiredContInt := ContInt;
         {if user wants too many contour lines, for affected squares it will
            drop down, but will use his interval where possible}
     {4 data points.  Starts in lower left corner, and does 1 column at a time}
     {arrays defined both in terms of corners and sides:}
     {   corners     sides }
     {  4 ----- 3    --3-- }
     {  |       |    4   2 }
     { 1=5 ---- 2    --1-- }

       if MinElev = 0 then ElevShift := 5 * ContInt
       else if MinElev < 0 then ElevShift := 5 * ContInt *
          succ(abs(MinElev) div 5 div ContInt)
       else ElevShift := 0;

       GetElevColProc(LeftGrid,ZLeft);
       for EWGrid := LeftGrid to pred(RightGrid) do begin
          UpdateProgressBar(EWGrid/RightGrid);
          GetElevColProc(succ(EWGrid),tzCol);
          for NSGrid := BottGrid to pred(TopGrid) do begin
             Elv[1] := ZLeft^[NSGrid];
             Elv[4] := ZLeft^[succ(NSGrid)];
             Elv[2] := tzCol^[NSGrid];
             Elv[3] := tzCol^[succ(NSGrid)];

             if (Elv[1] = MaxSmallInt) or
                (Elv[2] = MaxSmallInt) or
                (Elv[3] = MaxSmallInt) or
                (Elv[4] = MaxSmallInt) then goto NoFullSquare;

             For Cnt := 1 to 4 do inc(Elv[Cnt],ElevShift);
             Elv[5] := Elv[1];

             ContInt := DesiredContInt;
            StartAgain:;
             For Cnt := 1 to 4 do begin
                DifElv[Cnt] := Elv[succ(Cnt)] - Elv[Cnt];
                  {difference in elevation along each side of block}
                  { if > 0, higher elevation is Counterclockwise--Cnt+1}
                  { if < 0, higher elevation is clockwise--Cnt}
                ElvPer[Cnt] :=  Abs(Elv[succ(Cnt)] div ContInt - Elv[Cnt] div ContInt);
                {number of contour lines per block side, calculated by figuring # of contour lines between sea level and each corner point  }

                if ElvPer[Cnt] > MaxContoursPerGrid then begin
                   ContInt := ContInt * 5;
                   goto StartAgain;
                end {if};

                if ElvPer[Cnt]  > 0 then begin
                   {    grid units, from Cnt corner}
                   { SideZ = elevation of first contour line from corner}
                   if DifElv[Cnt] > 0 then begin
                      {higher elevation is counterclockwise}
                      Point[Cnt,1] := ( ContInt - Elv[Cnt] mod ContInt) /
                           abs(DifElv[Cnt]);
                      SideZ[Cnt,1] := succ(Elv[Cnt] div ContInt) * ContInt;
                   end
                   else begin
                      {higher elevation is clockwise}
                      Point[Cnt,1] := (Elv[Cnt] mod ContInt)/abs(DifElv[Cnt]);
                      SideZ[Cnt,1] := (Elv[Cnt] div ContInt) * ContInt;
                   end;
                   for Num := 2 to ElvPer[Cnt] do Begin
                      Point[Cnt,Num] := Point[Cnt,pred(Num)] + ContInt /
                            abs(DifElv[Cnt]);
                      if DifElv[Cnt] > 0 then
                            SideZ[Cnt,Num] := SideZ[Cnt,pred(Num)] + ContInt
                      else  SideZ[Cnt,Num] := SideZ[Cnt,pred(Num)] - ContInt;
                   end {for Num};
                 end {if};
             end {Cnt Loop};

        {calculate screen coordinates where contour lines cross grid squares}
              For Num := 1 to ElvPer[1] do begin
                 XPix[1,Num] := GraphDraw.Graphx(XMinGr + XIncGr * (EWGrid+Point[1,Num]));
                 YPix[1,Num] := GraphDraw.GraphY(YMinGr + YIncGr * (NSGrid));
              end;
              For Num := 1 to ElvPer[2] do begin
                 XPix[2,num] := GraphDraw.graphX(XMinGr + XIncGr * (succ(EWGrid)));
                 YPix[2,Num] := GraphDraw.GraphY(YMinGr + YIncGr * (NSGrid + Point[2,Num]));
              end;
              For Num := 1 to ElvPer[3] do begin
                 XPix[3,num] := GraphDraw.GraphX(XMinGr + XIncGr * (succ(EWGrid)-Point[3,Num]));
                 YPix[3,Num] := GraphDraw.GraphY(YMinGr + YIncGr * (succ(NSGrid)));
              end;
              For Num := 1 to ElvPer[4] do begin
                 XPix[4,num] := GraphDraw.GraphX(XMinGr + XIncGr * (EWGrid));
                 YPix[4,Num] := GraphDraw.GraphY(YMinGr + YIncGr * (succ(NSGrid) - Point[4,Num]));
              end;
            For Num := 1 to 4 do Tested[Num] := 0;
               {Tested is number of contour lines drawn to a side}

            If ((Elv[1] > Elv[2]) and (Elv[1] > Elv[4])) or
                 ((Elv[3] > Elv[2]) and (Elv[3] > Elv[4])) then
              begin {**** Algorithm for south to west *******}
                 for Side1 := 1 to 3 do If (ElvPer[Side1] > 0) then
                    While (ElvPer[Side1] > Tested[Side1]) do begin
                       for Cnt := 1 to ElvPer[Side1] do begin
                          Found := (SideZ[Side1,Cnt] = 1);
                          Side4 := succ(Side1);
                          Side := 4;
                          while Not(Found) and (Side >= Side4) and (Side > 0) do begin
                             CompareForContourLines;
                             dec(Side);
                          end {while};
                       End {Cnt loop};
                    end {while};
              end
              else begin  {*** Algorithm for South to East ***}
                 for Side1 := 1 to 3 do If (ElvPer[Side1] > 0) then
                    While (ElvPer[Side1] > Tested[Side1]) do begin
                       for Cnt := 1 to ElvPer[Side1] do begin
                          Found := (SideZ[Side1,Cnt] = 1);
                          Side4 := 4;
                          Side := succ(Side1);
                          while Not(Found) and (Side <= Side4)  and (Side > 0) do begin
                             CompareForContourLines;
                             inc(Side);
                          end {while};
                       End {Cnt loop};
                    end {while};
               end {if};
               NoFullSquare:;  {happens due to jaggies on 1:24K DEM edge}
                               {happens when area selected exends beyond DEM}
                               {happens for water}
            end {NSGrid loop};
            ZLeft^ := tzCol^;
        end {EWGrid loop};
   end {with};
   dispose(tzCol);
   dispose(ZLeft);
end {proc DrawContours};


procedure TThisContourGraph.PlotXYZPoints(Color : tPlatformColor; Symbol : tDrawingSymbol);
var
   i,xp,yp : integer;
   TStr : ShortString;
   sBitmap,Bitmap : tMyBitmap;
begin
   CopyImageToBitmap(Image1,Bitmap);
   Bitmap.Canvas.Font.Color := ConvertPlatformColorToTColor(color);
   StartProgress('Plot');
   for i := 4 to NumDataPoints + 3 do with ContourData^ do begin
      if (i mod 25 = 0) then UpdateProgressBar((i-4)/(NumDataPoints));
      xp := GraphDraw.GraphX(Pnt[1]^[i]);
      Yp := GraphDraw.GraphY(Pnt[2]^[i]);
      if ColorCodePlot then Color := ConvertTColorToPlatformColor(RainbowColorFunct(Pnt[3]^[i],zMin,zmax));
      ScreenSymbol(Bitmap.Canvas,xp,yp,symbol,3,color);
      if LabelPoints then begin
         TStr := RealToString(Pnt[3]^[i],-8,-2);
         Bitmap.Canvas.TextOut(xp+2,yp+2,TStr);
      end;
   end;
   EndProgress;
   if GraphDraw.ZColorLegend then with ContourData^ do SBitmap := DefaultHorizontalLegendOnBitmap(ZMin,ZMax,'','',LegRainbows);
   Bitmap.Canvas.Draw(0,GraphDraw.YWindowSize-50,sBitmap);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   sBitmap.Free;
end;


procedure TThisContourGraph.PlotGrid(Color : tPlatFormColor; Symbol : tDrawingSymbol);
var
   Column,Row,xp,yp : integer;
   x,y              : float32;
   TStr : ShortString;
begin
   with ContourData^ do begin
      Image1.Canvas.Font.Color := ConvertPlatformColorToTColor(color);
      StartProgress('Plot');
      for Column := 0 to NumCol do begin
         UpdateProgressBar(Column / NumCol);
         x := XMinGr + Column * XIncGr;
         for Row := 0 to NumRow do begin
            y := YMinGr + Row * YIncGr;
            xp := GraphDraw.GraphX(x);
            yp := GraphDraw.GraphY(y);
            ScreenSymbol(Image1.Canvas,xp,yp,symbol,3,color);
            TStr := IntegerToString(ValueArray[Column,Row],-8);
            Image1.Canvas.TextOut(xp-5-Image1.Canvas.TextWidth(TStr),yp+2,TStr);
         end {for Row};
      end {for Column};
   end {with};
   EndProgress;
end;


procedure TThisContourGraph.FormResize(Sender: TObject);
begin
   if GraphDraw.GraphDrawn then begin
      inherited;
      RedrawDiagram11Click(Nil);
      if (WhatsOnGraph in [TriangulationContour,GridContour]) then DefaultContour(TinfName);
      if (WhatsOnGraph = XYZPoints) then PlotXYZPoints(ConvertTColorToPlatformColor(clRed),Box);
      if (WhatsOnGraph = GridPoints) then PlotGrid(ConvertTColorToPlatformColor(clMaroon),Cross);
   end;
end;


procedure TThisContourGraph.Setoptions1Click(Sender: TObject);
var
  ContourOptions : TSimpleContourOptions;
begin
   ContourOptions := TSimpleContourOptions.Create(Application);
   with ContourData^ do begin
      ContourOptions.CheckBox1.Checked := LabelPoints;
      ContourOptions.CheckBox2.Checked := ColorCodePlot;
      ContourOptions.CheckBox3.Checked := MDDef.ShowDelauneyTriangles;
      ContourOptions.Edit1.Text := RealToString(UserContourInterval,-18,-4);
      ContourOptions.ShowModal;
      LabelPoints := ContourOptions.CheckBox1.Checked;
      ColorCodePlot := ContourOptions.CheckBox2.Checked;
      MDDef.ShowDelauneyTriangles := ContourOptions.CheckBox3.Checked;
      CheckEditString(ContourOptions.Edit1.Text,UserContourInterval);
      CheckEditString(ContourOptions.Edit2.Text,MDDef.MaxTriSide);
      if UserContourInterval < (ZMax - ZMin) / PetMath.MaxContours then begin
         MessageToContinue('Contour interval small');
         UserContourInterval := (ZMax - ZMin) / PetMath.MaxContours;
      end;
      TIN.TinContourInterval := UserContourInterval;

      FormResize(Sender);
      TIN.UpdateBaseMap;
   end;
   ContourOptions.Free;
end;

procedure TThisContourGraph.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef RecordTINProblems} WriteLineToDebugFile('TThisContourGraph.FormClose'); {$EndIf}
end;

procedure TThisContourGraph.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{$IfDef ExTin}
begin
{$Else}
var
   xf,yf,z : float32;
begin
   if RoamPossible and (TIN <> Nil) then begin
      xf := GraphDraw.InvGraphX(x);
      yf := GraphDraw.InvGraphY(Y);
      z := TIN.InterpolateZ(xf,yf);
      if (z < 32000) then Panel1.Caption := RealToString(z,-12,1)
      else Panel1.Caption := '';
   end;
   inherited;;
{$EndIf}
end;

procedure TThisContourGraph.Graphparameters1Click(Sender: TObject);
begin
   FormResize(Nil);
end;

procedure TThisContourGraph.Graphsize1Click(Sender: TObject);
begin
   Graphparameters1Click(Sender);
end;

procedure TThisContourGraph.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      Setoptions1Click(Sender);
   end
   else inherited;
end;


procedure TThisContourGraph.SpeedButton8Click(Sender: TObject);
begin
   FormResize(Sender);
end;


procedure TThisContourGraph.TINInterpolateButtonClick(Sender: TObject);
begin
   {$IfDef ExTin}
   {$Else}
      Tin.InterpolateDEM;
   {$EndIf}
end;

procedure DoInit;
var
   i : integer;
begin
   PointTolerance := 0.5;
   for i := 1 to 5 do Pnt[i] := Nil;
end;

procedure DoFinal;
var
   i : integer;
begin
   for i := 1 to 5 do if (Pnt[i] <> Nil) then Dispose(Pnt[i]);
end;

initialization
   DoInit;
finalization
   DoFinal;
{$IfDef RecordDetailedXYZProblems} WriteLineToDebugFile('RecordDetailedXYZProblems active in ContGraf'); {$EndIf}
{$IfDef RecordXYZProblems} WriteLineToDebugFile('RecordXYZProblems active in ContGraf'); {$EndIf}
{$IfDef RecordTINProblems} WriteLineToDebugFile('RecordTINProblems active in ContGraf'); {$EndIf}
end.



