unit crosscor;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}


interface

uses
   Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
   Buttons, ExtCtrls, OkCancl2,
   Petmar_types,PETMAR,Petmath,   BaseGraf;

type
   CrossCorType = array[0..MaxContourPoints] of float32;


  TCrossCorrelationForm = class(TOKRightDlg)
    HelpBtn: TButton;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
  private
    { Private declarations }
     procedure InitializeCrossCorrelation;
     procedure DrawLagGraph(Graph : tThisBaseGraph; Lag : integer);
     procedure HighLightTextFile(HighLight : float64);
     procedure LagScatterPlot(Lag: integer);
  public
    { Public declarations }
   TempSeries,
   Series1,Series2 : ^bfarray32;
   PlusCrossCor,
   NegCrossCor     : ^CrossCorType;
   LengthShort,LengthLong,                                          
   Match,i,j       : integer;
   R,HighLight       : float64;
   ValueMin2,ValueMax2,
   ValueMin : float64;
   ValueMax  : float64;
   MaxR          : float64;
   MinR          : float64;
   DataSpacing    : float64;
   DataSpacingUnits : ShortString;
   Lag,
   WhereMaxR     : integer;
   WhereMinR     : integer;
   MinMatches    : integer;
   DrawOriginalGraph,
   DrawResultsTable,
   Auto          : boolean;
   InputFName,
   InputFName2   : PathStr;
   CorrelogramGraph,
   OriginalGraph,
   LagScatterGraph,
   OffsetTimeGraph      : tThisBaseGraph;
  end;


procedure CrossCorrelating(Auto : boolean; fName : PathStr = ''; fName2 : PathStr = ''; DrawOriginalGraph : boolean = true; DrawResultsTable : boolean = true; DataSpacing : float64 = 1; DataSpacingUnits : ShortString = ''; AutoClose : boolean = false;
    Series1Name : string35 = ''; Series2Name : string35 = '');


var
  CrossCorrelationForm: TCrossCorrelationForm;

implementation


{$R *.DFM}


procedure TCrossCorrelationForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\cross_correlation.htm');
end;


procedure CrossCorrelating;
begin
   CrossCorrelationForm := TCrossCorrelationForm.Create(Application);
   CrossCorrelationForm.Auto := Auto;
   CrossCorrelationForm.DrawOriginalGraph := DrawOriginalGraph;
   CrossCorrelationForm.DrawResultsTable := DrawResultsTable;
   CrossCorrelationForm.DataSpacing := DataSpacing;
   CrossCorrelationForm.DataSpacingUnits := DataSpacingUnits;
   if Series1Name = '' then CrossCorrelationForm.Label1.Caption := ''
   else begin
      CrossCorrelationForm.Label1.Font.Color := clRed;
      CrossCorrelationForm.Label1.Caption := Series1Name;
   end;
   if Series2Name = '' then CrossCorrelationForm.Label2.Caption := ''
   else begin
      CrossCorrelationForm.Label2.Font.Color := clBlue;
      CrossCorrelationForm.Label2.Caption := Series2Name;
   end;

   CrossCorrelationForm.InputFName := fName;
   CrossCorrelationForm.InputFName2 := fName2;
   CrossCorrelationForm.InitializeCrossCorrelation;
   if AutoClose then CrossCorrelationForm.Close;
end;


procedure TCrossCorrelationForm.DrawLagGraph(Graph : tThisBaseGraph; Lag : integer);
var
   i : integer;
   rfile,rfile2 : file;
   v            : tGraphPoint32;
begin
   with Graph,GraphDraw do begin
      ClearDataOnGraph;
      if (Lag < 0) then R := NegCrossCor^[-Lag]
      else R := PlusCrossCor^[Lag];
      Caption := 'R=' + RealToString(R,5,3) + '   lag=' +  IntToStr(Lag);
      OpenDataFile(Rfile);
      for I := 0 to pred(LengthLong) do begin
            v[1] := i;
            v[2] := Series1^[i];
            BlockWrite(rfile,v,1);
      end;
      if ((not Auto) or (Lag <> 0)) then begin
         OpenDataFile(RFile2);
         for I := 0 to pred(LengthShort) do begin
            v[1] := i - lag;
            v[2] := Series2^[i];
            BlockWrite(rfile2,v,1);
         end;
         CloseFile(RFile2);
      end;
      closeFile(Rfile);
      AutoScaleAndRedrawDiagram(true,true,false);
   end;
end;


procedure TCrossCorrelationForm.HighLightTextFile(HighLight : float64);
var
   i     : integer;
begin
   Memo1.Clear;
   Memo1.Lines.Add('Max correlation '+RealToString(MaxR,8,2)+' at lag'+ IntegerToString(WhereMaxR,6));
   Memo1.Lines.Add('Min correlation '+RealToString(MinR,8,2)+' at lag'+ IntegerToString(WhereMinR,6));
   Memo1.Lines.Add('');
   Memo1.Lines.Add('  Lag   R');
   Memo1.Lines.Add('-----------');
   for i := -MaxContourPoints to MaxContourPoints do begin
      if i < 0 then R := NegCrossCor^[-i] else R := PlusCrossCor^[i];
      if (R < MaxInt) and  (abs(R) >= HighLight) then Memo1.Lines.Add(IntegerToString(i,5) + RealToString(R,8,3));
   end;
end;


procedure TCrossCorrelationForm.InitializeCrossCorrelation;
label
   ReStart;
var
   CrossCorFile : ShortString;


      procedure ComputeCrossCorrelation(Pt1,Pt2 : integer; var R : float64; var Match : integer);
      var
         SX,SY,SXX,SYY,SXY     : float64;
         NonMissMatch,Match1,Match2,i   : integer;
      begin
         SX  := 0;
         SY  := 0;
         SXX := 0;
         SYY := 0;
         SXY := 0;
         Match1 := succ(LengthShort - Pt1);
         Match2 := succ(LengthLong - Pt2);
         if (Match1 < Match2) then Match := Match1 else Match := Match2;
         if (Match < MinMatches) then exit;
         NonMissMatch := Match;
         for i := 1 to Match do begin
            if (Series1^[Pt1] < pred(MaxInt)) and (Series2^[Pt1] < pred(MaxInt)) then begin
               SX := SX + Series1^[Pt1];
               SY := SY + Series2^[Pt2];
               SXX := SXX + sqr(Series1^[Pt1]);
               SYY := SYY + sqr(Series2^[Pt2]);
               SXY := SXY + Series1^[Pt1] * Series2^[Pt2];
            end
            else dec(NonMissMatch);
            inc(Pt1);
            inc(Pt2);
         end {for i};
         R := (NonMissMatch * SXY - SX * SY) / sqrt( (NonMissMatch * SXX - sqr(SX) ) * (NonMissMatch * SYY - sqr(SY) ) );
      end {ComputeCrossCorrelation};


      procedure DrawCorrelogram;
      var
         rfile     : file;
         i : integer;
         v         : tGraphPoint32;
      begin
         with CorrelogramGraph,GraphDraw do begin
            MinVertAxis := -1;
            MaxVertAxis := 1;
            if Auto then MinHorizAxis := 0
            else MinHorizAxis := (MinMatches-LengthShort) * DataSpacing;
            MaxHorizAxis := (LengthLong-MinMatches) * DataSpacing;
            PadAxis(MinHorizAxis,MaxHorizAxis);
            if abs(DataSpacing - 1) < 0.0001 then HorizLabel := 'Lag (sampling periods)'
            else HorizLabel := 'Lag (' + DataSpacingUnits + ')';
            VertLabel := 'Correlation (r)';

            for i := 0 to MaxContourPoints do begin
               PlusCrossCor^[i] := MaxInt;
               NegCrossCor^[i] := MaxInt;
            end;
            Caption := ChangeFileExt(ExtractFileName(InputfName),'') + ' Correlogram (correlation coefficient vs. lag)';

            SetUpGraphForm;

            StartProgress('Correlations');
            i := LengthShort - MinMatches;
            while (i >= 0) do begin
               if (i mod 25 = 0) then UpdateProgressBar(((LengthShort - MinMatches)-i) / (LengthShort - MinMatches));
               ComputeCrossCorrelation(succ(i),1,R,Match);
               if (Match >= MinMatches) then begin
                  if R > MaxR then begin
                     MaxR := R;
                     WhereMaxR := -i;
                  end;
                  if R < MinR then begin
                     MinR := R;
                     WhereMinR := -i;
                  end;
                  NegCrossCor^[i] := R;
                  if Auto then PlusCrossCor^[i] := R;
               end;
               dec(i);
            end {while};
            EndProgress;

            i := 0;
            if Not Auto then while i <= LengthLong - MinMatches do begin
               ComputeCrossCorrelation(1,succ(i),R,Match);
               if (Match >= MinMatches) then begin
                  if (R > MaxR) then begin
                     MaxR := R;
                     WhereMaxR := i;
                  end;
                  if (R < MinR) then begin
                     MinR := R;
                     WhereMinR := i;
                  end;
                  PlusCrossCor^[i] := R;
               end;
               inc(i);
            end;

            OpenDataFile(rfile);
            if Auto then begin
               for i := 0 to MaxContourPoints do begin
                  R := PlusCrossCor^[i];
                  if (R < MaxInt) then begin
                     v[1] := i * DataSpacing;
                     v[2] := R;
                     BlockWrite(rfile,v,1);
                  end;
               end;
            end
            else begin
               for i := -MaxContourPoints to MaxContourPoints do begin
                  if i < 0 then R := NegCrossCor^[-i] else R := PlusCrossCor^[i];
                  if (R < MaxInt) then begin
                     v[1] := i * DataSpacing;
                     v[2] := R;
                     BlockWrite(rfile,v,1);
                  end;
               end;
            end;

            CloseFile(rfile);
            RedrawDiagram11Click(nil);
         end {with};
      end;


var
   i : integer;
begin
   if Auto then begin
      GetFile('series',LengthShort,Series1^,ValueMin,ValueMax,InputFName);
      LengthLong := LengthShort;
      Series2^ := Series1^;
      CrossCorFile := InputFileName;
   end
   else begin
      GetFile('short series',LengthShort,Series1^,ValueMin,ValueMax,InputFName);
      CrossCorFile := InputFileName;
      GetFile('long series',LengthLong,Series2^,ValueMin2,ValueMax2,InputFName2);
      CrossCorFile := CrossCorFile + ' and ' + InputFileName;
      if ValueMin2 < ValueMin then ValueMin := ValueMin2;
      if ValueMax2 > ValueMax then ValueMax := ValueMax2;
      if (LengthShort = 0) or (LengthLong = 0) then exit;
   end;

   if (LengthShort > LengthLong) then begin
      New(TempSeries);
      TempSeries^ := Series1^;
      Series1^ := Series2^;
      Series2^ := TempSeries^;
      i := LengthShort;
      LengthShort := LengthLong;
      LengthLong := i;
      Dispose(TempSeries);
   end;

   MinMatches := LengthShort div 4;
  ReStart:;

   if (InputFName = '') then repeat
      ReadDefault('Min matches (> n/4, <= n; n= ' +  IntToStr(LengthShort) +')',MinMatches);
   until MinMatches <= LengthShort;

   DrawCorrelogram;

   if DrawOriginalGraph then begin
      DrawLagGraph(OriginalGraph,0);
   end;

   if DrawResultsTable then HighLightTextFile(MaxR - 0.1);
   Lag := WhereMaxR;
   DrawLagGraph(OffsetTimeGraph,WhereMaxR);
   LagScatterPlot(WhereMaxR);
   BringToFront;
end;


procedure TCrossCorrelationForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   Dispose(Series1);
   Dispose(Series2);
   Dispose(PlusCrossCor);
   Dispose(NegCrossCor);
   OffsetTimeGraph.CanCloseGraph := true;
   LagScatterGraph.CanCloseGraph := true;
   CorrelogramGraph.CanCloseGraph := true;
   OriginalGraph.CanCloseGraph := true;
   if CheckBox1.Checked then begin
      OffsetTimeGraph.Close;
      LagScatterGraph.Close;
      CorrelogramGraph.Close;
      OriginalGraph.Close;
   end;
end;


procedure TCrossCorrelationForm.FormCloseQuery(Sender: TObject;   var CanClose: Boolean);
begin
   CanClose := AnswerIsYes('Confirm close auto/cross correlation');
end;

procedure TCrossCorrelationForm.FormCreate(Sender: TObject);
begin
   MaxR       := -1;
   MinR       := 1;
   WhereMaxR  := 0;
   WhereMinR  := 0;
   New(Series1);
   New(Series2);
   New(PlusCrossCor);
   New(NegCrossCor);
   OffsetTimeGraph := tThisBaseGraph.Create(Application);
   LagScatterGraph := tThisBaseGraph.Create(Application);
   CorrelogramGraph := tThisBaseGraph.Create(Application);
   OriginalGraph := tThisBaseGraph.Create(Application);
   OffsetTimeGraph.CanCloseGraph := false;
   LagScatterGraph.CanCloseGraph := false;
   CorrelogramGraph.CanCloseGraph := false;
   OriginalGraph.CanCloseGraph := false;
end;


procedure TCrossCorrelationForm.BitBtn1Click(Sender: TObject);
begin
   LagScatterGraph.BringToFront;

end;

procedure TCrossCorrelationForm.BitBtn2Click(Sender: TObject);
begin
   OffsetTimeGraph.BringToFront;
end;

procedure TCrossCorrelationForm.BitBtn3Click(Sender: TObject);
begin
   CorrelogramGraph.BringToFront;
end;

procedure TCrossCorrelationForm.BitBtn4Click(Sender: TObject);
begin
   OriginalGraph.BringToFront;
end;

procedure TCrossCorrelationForm.Button1Click(Sender: TObject);
var
   Lag : integer;
begin
   CheckEditString(Edit1.Text,Lag);
   DrawLagGraph(OffsetTimeGraph,Lag);
   LagScatterPlot(Lag);
end;


procedure TCrossCorrelationForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TCrossCorrelationForm.CancelBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TCrossCorrelationForm.Button2Click(Sender: TObject);
var
   Corr : float64;
begin
   Corr := MaxR - 0.1;
   ReadDefault('r to list in table',Corr);
   HighLightTextFile(Corr);
end;


procedure TCrossCorrelationForm.LagScatterPlot(Lag : integer);
var
   i : integer;
   rfile : file;
   r : float64;
   v            : tGraphPoint32;
begin
   with LagScatterGraph,GraphDraw do begin
      ClearDataOnGraph;
      if (Lag >= 0) then R := PlusCrossCor^[Lag]
      else R := NegCrossCor^[-Lag];
      Caption := 'Scatter plot for lag=' +  IntToStr(Lag) + '  r=' + RealToString(r,-8,3);
      OpenPointFile(Rfile,LagScatterGraph.Symbol);
      for I := 0 to pred(LengthLong) do begin
         v[1] := Series1^[i];
         if (i+Lag >= 0) and (i <= LengthShort-Lag) then begin
            v[2] := Series2^[i+Lag];
            BlockWrite(rfile,v,1);
         end;
      end;
      closeFile(Rfile);
      AutoScaleAndRedrawDiagram;
   end;
end;


initialization
finalization
end.

