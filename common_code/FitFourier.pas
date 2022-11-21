unit FitFourier;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, OkCancl2, ComCtrls,
  PETMAR,PetMath,Petmar_types;


const
   MaxDataPoints  = 8000;
   MaxHarmonics   = 1500;
type
   OneHarmonicType = array[1..succ(MaxDataPoints)] of float32;
   HarmonicArray = array[1..MaxHarmonics] of ^OneHarmonicType;

  TFitFourierForm = class(TOKRightDlg)
    RichEdit1: TRichEdit;
    HelpBtn: TButton;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
      procedure InitialRun;
      procedure AnalyzeOne;
      procedure PlotData(FitTotal,ShowHarmonics : boolean);
  public
    { Public declarations }
   Z,Cumt            : ^bfarray32;
   DataFile          : PathStr;
   Harmonic          : HarmonicArray;
   A                      : tGraphPoint32;
   Period,Phase,Amp,PctDev   : array[1..MaxHarmonics] of float64;
   OrderComponents           : array[1..MaxHarmonics] of integer;
   k,xd,yd,
   NComp,NumSamples  : integer;
   ch                          : AnsiChar;
   PltInc,XInc,Mean,Varian,
   Dev,SumPct,Orig,T,SumDev,
   ZMin,ZMax,
    ys,Angle,SMA,SMB,ARG,
    Power,SumZ,SSqr      : float64;
  end;


procedure FitFourierCurve(fName : PathStr = '');


var
  FitFourierForm : TFitFourierForm;

implementation

{$R *.DFM}

uses
  BaseGraf;


procedure FitFourierCurve(fName : PathStr = '');
begin
   FitFourierForm := TFitFourierForm.Create(Application);
   FitFourierForm.DataFile := fName;
   FitFourierForm.InitialRun;
end;


procedure TFitFourierForm.PlotData(FitTotal,ShowHarmonics : boolean);
var
   i,j,k,l   : integer;
   SumSq,
   sum : float64;
   ThisGraph : tThisBaseGraph;
   Rfile,RFile2 : file;
   v     : tGraphPoint32;
begin
   ThisGraph := tThisBaseGraph.Create(Application);
   with ThisGraph,GraphDraw do begin
      if ShowHarmonics then begin
         Left := 0;
         Top := 0;
      end;
      if FitTotal then ThisGraph.Caption := InputFileName +  ' Raw data + Fourier fit'  + IntegerToString(NComp,4) + ' terms'
      else ThisGraph.Caption := InputFileName + ' Raw data';
      MaxHorizAxis := NumSamples;
      MinVertAxis := zMin;
      MaxVertAxis := zMax;
      HorizLabel := 'Time sequence';
      PadAxis(MinVertAxis,MaxVertAxis);
      OpenDataFile(Rfile);
      if FitTotal then OpenDataFile(RFile2);
      for I := 1 to NumSamples do begin
         v[1] := i;
         v[2] := z^[i];
         BlockWrite(rfile,v,1);
         if FitTotal then begin
            v[2] := Cumt^[i];
            BlockWrite(rfile2,v,1);
         end;
      end;
      closeFile(Rfile);
      if FitTotal then CloseFile(RFile2);
      SetUpGraphForm;
      RedrawDiagram11Click(nil);
   end {with};


   if ShowHarmonics then with ThisGraph,GraphDraw do begin
      sumSq := 0;
      Image1.Picture.SaveToFile(MDTempDir + 'working.bmp');
      for l := 1 to NComp do begin
         Image1.Picture.LoadFromFile(MDTempDir + 'working.bmp');
         Image1.Canvas.Pen.Width := 2;
         j := OrderComponents[l];
         {plot this harmonic}
         with Image1.Canvas do begin
            Pen.Color := clBlue;
            MoveTo(GraphDraw.GraphX(1), GraphDraw.GraphY(Mean + Harmonic[j]^[1]));
            for I := 2 to NumSamples do begin
               LineTo(GraphDraw.GraphX(i), GraphDraw.GraphY(Mean + Harmonic[j]^[i]));
            end;
         end;

         sum := mean;
         for k := 1 to l do sum := sum + harmonic[OrderComponents[k]]^[1];
         with Image1.Canvas do begin
            Pen.Color := clNavy;
            MoveTo(GraphX(1), GraphY(Sum));
            for I := 2 to NumSamples do begin
               Sum := Mean;
               for k := 1 to l do sum := sum + harmonic[OrderComponents[k]]^[i];
               LineTo(GraphX(i), GraphY(Sum));
            end;
         end;

         ApplicationProcessMessages;

         SumSq := SumSq + PctDev[j];

         if not AnswerIsYes('Harmonic ' + IntegerToString(j,3) + ' (#' +
           IntegerToString(l,-4) + ' largest) explains ' +
           RealToString(PctDev[j],6,2) + '%' +  MessLineBreak +
           'Sum largest' + IntegerToString(l,5) + ' harmonics explain ' +
           RealToString(SumSq,6,2) + '%' + MessLineBreak + MessLineBreak +
           'Show next largest harmonic') then break;
       end;
       //DeleteFile(ScrathBMPFile);
   end;
end;


procedure TFitFourierForm.InitialRun;
{---------------------------------------------------------------}
{ Fourier analysis                                              }
{ from a Basic program by William T. Fox                        }
{ modified by Peter L. Guth                                     }
{---------------------------------------------------------------}
var
   i           : integer;
begin {Program Fourier}
   GetFile('Fourier analysis',NumSamples,Z^,ZMin,ZMax,DataFile);
   if (NumSamples = 0) then begin
      MessageToContinue('Invalid file selected');
      exit;
   end;
   RichEdit1.Lines.Add('Fourier curve fit for ' + InputFileName);
   RichEdit1.Lines.Add('   Samples=' + IntToStr(NumSamples));
   RichEdit1.Lines.Add('');
   SumZ := 0;
   for I := 1 to NumSamples do SumZ := SumZ + Z^[i];
   Mean := SumZ / NumSamples;
   SSQR := 0;
   for I := 1 to NumSamples do SSQR := SSQR + sqr(Z^[I] - Mean);
   Varian := SSQR / NumSamples;
   PlotData(False,false);

   NComp := NumSamples;
   if NComp > MaxHarmonics then NComp := MaxHarmonics;
   AnalyzeOne;
end;


procedure TFitFourierForm.AnalyzeOne;
var
   nx,i,j  : integer;
   Results : tStringList;
begin
   for i := 1 to NComp do GetMem(Harmonic[i],SizeOf(float32) * NumSamples);
   Period[1] := NumSamples;
   RichEdit1.Lines.Add('Number components:' + IntegerToString(NComp,6));
   RichEdit1.Lines.Add('Min period resolved:' + RealToString(NumSamples/NComp,8,2));
   RichEdit1.Lines.Add('');

   for NX := 1 to NComp do Period[NX] := Period[1] / NX;
   for NX := 1 to NComp do begin
       SMA := 0;
       SMB := 0;
       for I := 1 to NumSamples do begin
          ARG := 2 * PI * I / Period[NX];
          SMA := SMA + (Z^[I] - Mean) * COS(ARG);
          SMB := SMB + (Z^[i] - Mean) * sin(ARG);
       end {i};
       A[1] := SMA * 2 / NumSamples;
       A[2] := SMB * 2 / NumSamples;
       POWER := Sqrt(sqr(A[1]) + sqr(A[2]));
       Angle := ATan2(a[1],a[2]);
       AMP[NX] := (A[2]*COS(Angle) + A[1]*sin(Angle));
       Phase[NX] := Angle / DegToRad;
       if Phase[NX]  > 360 then Phase[NX] := Phase[NX] - 360;
    end {NX};
    for I := 1 to succ(NumSamples) do Cumt^[I] := Mean;
    SumPct := 0;

    {compute values  for each harmonic}
    for I := 1 to NComp do begin
       ORIG := Phase[I] * DegToRad;
       SumDev := 0;
       for J := 1 to NumSamples do begin
          Ys := AMP[I]*sin(ORIG + TwoPi * J / Period[I]);
          Harmonic[i]^[j] := Ys;
          Cumt^[J] := Cumt^[J] + Ys;
          DEV := ABS(Z^[J] - Ys - Mean);
          SumDev := SumDev + sqr(DEV);
       end {for j};
       { compute percent sum of squares for each harmonic}
       PctDev[i] := 100.0 * (1 - SumDev / SSQR);
       SumPct := SumPct + PctDev[i];
    end {for i};

   Results := tStringList.Create;
   Results.Add(' Mean  = '+ RealToString(Mean,7,2) + '  Variance  = '+ RealToString(Varian,12,3));
   Results.Add('');
   Results.Add('Component   Period   Phase    Amp   % SS  Cum % SS');
   Results.Add('');

   for I := 1 to NComp do OrderComponents[i] := i;
   for I := 1 to pred(NComp) do begin
      for j := 1 to pred(NComp) do begin
         if PctDev[OrderComponents[j]] < PctDev[OrderComponents[succ(j)]] then begin
            k := OrderComponents[j];
            OrderComponents[j] := OrderComponents[succ(j)];
            OrderComponents[succ(j)] := k;
         end {if};
      end;
   end;

   SumDev := 0;
   for j := 1 to NComp do begin
      i := OrderComponents[j];
      SumDev := SumDev + PctDev[i];
      Results.Add(IntegerToString(I,6) + RealToString(Period[I],12,3)+RealToString(Phase[I],8,1)+
         RealToString(abs(AMP[I]),8,3) +
         RealToString(PctDev[i],8,3)+RealToString(SumDev,8,2));
   end;
   Results.Add('');
   Results.Add(' Total Percent sum of Squares  =  '+RealToString(SumPct,8,4));

   RichEdit1.Lines.Add('Explains Total Percent sum of Squares:'+ RealToString(SumPct,8,4));
   RichEdit1.Lines.Add('');
   RichEdit1.Lines.Add('');
   PlotData(True,false);
   Petmar.DisplayAndPurgeStringList(Results,InputFileName + ' Fourier fit' + IntegerToString(NComp,4) + ' terms');
   BringToFront;
end;


procedure TFitFourierForm.FormClose(Sender: TObject;  var Action: TCloseAction);
var
   i : integer;
begin
  inherited;
   Action := caFree;
   Dispose(Z);
   Dispose(Cumt);
   for i := 1 to NComp do FreeMem(Harmonic[i],SizeOf(float32) * NumSamples);
end;


procedure TFitFourierForm.Button1Click(Sender: TObject);
begin
   PlotData(true,true);
end;


procedure TFitFourierForm.FormCreate(Sender: TObject);
begin
   new(Z);
   new(Cumt);
end;


procedure TFitFourierForm.OKBtnClick(Sender: TObject);
begin
  inherited;
   Close;
end;


procedure TFitFourierForm.CancelBtnClick(Sender: TObject);
begin
  inherited;
   Close;
end;

procedure TFitFourierForm.Button2Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to NComp do FreeMem(Harmonic[i],SizeOf(float32) * NumSamples);
   ReadDefault('Number of samples',NComp);
   if (NComp > MaxHarmonics) then begin
      NComp := MaxHarmonics;
      MessageToContinue('Harmonics limit: ' + IntToStr(NComp));
   end;
   if (NComp > NumSamples) then NComp := NumSamples;
   AnalyzeOne;
end;

end.

