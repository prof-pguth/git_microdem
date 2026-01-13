unit NyqGraph;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


interface

uses
  Windows,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BASEGRAF, Menus, ExtCtrls,
  petmar, Petmar_types, Buttons, ToolWin, ComCtrls;

type
  TNyquistBaseGraph = class(TThisBaseGraph)
    Resample1: TMenuItem;
    Sameinterval1: TMenuItem;
    Newinterval1: TMenuItem;
    Period1: TMenuItem;
    Change1: TMenuItem;
    FFT2: TMenuItem;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Graphsettings2Click(Sender: TObject);
    procedure Graphparameters1Click(Sender: TObject);
    procedure Font1Click(Sender: TObject);
    procedure Sameinterval1Click(Sender: TObject);
    procedure Newinterval1Click(Sender: TObject);
    procedure Change1Click(Sender: TObject);
    procedure Run1Click(Sender: TObject);
    procedure FFT2Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Period,StartTime,
    SamplingInterval : float64;
    procedure DrawSampling;
  end;


implementation

{$R *.DFM}

uses
   PETMath,PetFouri;

const
   Amplitude : float64 = 1.0;

const
   MaxPlotGlb   = 500;
type
   PlotArray = array [1..MaxPlotGlb,1..2] of float64;

(***********************************************************)
(*                                                         *)
(*                TURBO GRAPHIX version 1.05A              *)
(*                   Cubic spline module                   *)
(*                  Module version  1.00A                  *)
(*                  Copyright (C) 1985 by                  *)
(*                  BORLAND International                  *)
(*                                                         *)
(***********************************************************)

procedure spline(A     : PlotArray;    {array for spline fit}
                 N     : integer;      {points in A array}
                 X1,XM : float64;         {x range for spline operation}
                 var B : PlotArray;    {array with spline curve}
                 M     : integer);     {points for B array}
type
   Vector = array [1..MaxPlotGlb] of float64;
var
   x,y,z  : ^Vector;
   i      : integer;
   DeltaX : float64;

    procedure stg(var Vector1,Vector2,Vector3 : Vector; var Vector4 : Vector;
                NPts : integer);
    var
       i : integer;
       Factor : float64;
    begin
       for i := 2 to NPts do begin
          Factor := Vector1[pred(i)] / Vector2[pred(i)];
          Vector2[i] := Vector2[i] - Factor * Vector3[pred(i)];
          Vector4[i] := Vector4[i] - Factor * Vector4[pred(i)];
       end;
       Vector4[NPts] := Vector4[NPts] / Vector2[NPts];
       for i := 1 to pred(NPts) do
         Vector4[NPts-i] := (Vector4[NPts-i]-Vector3[NPts-i]*Vector4[NPts-i+1])
             / Vector2[NPts-i];
    end;

  procedure sc(X,Y:Vector; var Z:Vector; NPts: integer);
  var i: integer;
      D,C: ^Vector;
  begin
      new(d);
      new(c);
      D^[1] := 1.0;
      C^[1] := 0.5;
      Z[1] := 0.5;
      for i := 2 to pred(NPts) do begin
        D^[i] := 2.0*(X[succ(i)] - X[pred(i)]);
        C^[i] := X[i+1]-X[i];
        Z[i] := 6.0*((Y[succ(i)]-Y[i])/(X[succ(i)]-X[i])-(Y[i]-Y[pred(i)])/
               (X[i]-X[pred(i)]));
      end;
      D^[NPts] := 1.0;
      C^[pred(NPts)] := 0.0;
      C^[NPts] := 0.0;
      Z[NPts] := 0.0;
      stg(C^,D^,C^,Z,NPts);
      dispose(c);
      dispose(d);
    end;

  function si(V:float64; X,Y,Z:Vector; NPts: integer):float64;
    var i,j: integer;
        dummy,ai,hi:float64;
    begin
      if (V > X[1]) and (V < X[NPts]) then begin
        j := 1;
        repeat
          j := j+1;
          i := NPts-j;
          dummy := V-X[i];
        until (dummy >= 0.0) or (i=2);
        hi := X[succ(i)] - X[i];
        ai := dummy*(Z[succ(i)]-Z[i])/(6.0*hi)+0.5*Z[i];
        ai := dummy*ai+(Y[succ(i)]-Y[i])/hi-hi*(2.0*Z[i]+Z[succ(i)])/6.0;
        si := dummy*ai+Y[i];
      end
      else if V = X[1] then si := Y[1]
      else si := Y[NPts];
    end;

  procedure sia(X,Y:Vector; NPts: integer; XInt:Vector; var YInt:Vector; N: integer);
    var i: integer;
        V3:Vector;
    begin
      sc(X,Y,V3,NPts);
      for i := 1 to N do YInt[i] := si(XInt[i],X,Y,V3,NPts);
    end;

begin { Spline }
    new(x);
    new(y);
    New(z);
    if (abs(N) >= 2) and (abs(M) >=2 ) then begin
      if ((X1 >= A[1,1]) and (XM <= A[N,1])) and (M>=2) then begin
        DeltaX := (XM - X1) / (M-1);
        for i := 1 to N do begin
          X^[i] := A[i,1];
          Y^[i] := A[i,2];
        end;
        for i := 2 to pred(M) do Z^[i] := X1 + pred(i) * DeltaX;
        Z^[1] := X1;
        Z^[M] := XM;
        sia(X^,Y^,N,Z^,Y^,M);
        for i := 1 to M do begin
          B[i,1] := Z^[i];
          B[i,2] := Y^[i];
         end;
       end;
     end;
    Dispose(x);
    Dispose(y);
    Dispose(z);
  end {procedure spline};


procedure TNyquistBaseGraph.DrawSampling;
var
   NumPts,i,x,y : integer;
   Time,Tide  : float64;
   Sample,Splined : PlotArray;
begin
   Caption := 'Sampling interval:  ' + RealToString(SamplingInterval,-8,-2) + '  from Period:  ' + RealToString(Period,-8,-2);
   with GraphDraw,Image1.Canvas do begin
      Pen.Color := clBlue;
      Pen.Width := 2;
      for i := 0 to 500 do begin
         Time := i / 500 * MaxHorizAxis;
         x := GraphX(Time);
         y := GraphY(Amplitude * cos(Time / Period * TwoPi));
         if (i = 0) then MoveTo(x,y) else LineTo(x,y);
      end;
      Pen.Width := 1;
      StartTime := -Random * SamplingInterval - SamplingInterval;
      Time := StartTime;

      NumPts := 0;
      while Time <= MaxHorizAxis + 2*SamplingInterval do begin
         Tide := Amplitude * cos(Time / Period * TwoPi);
         inc(NumPts);
         Sample[NumPts,1] := Time;
         Sample[NumPts,2] := Tide;

         x := GraphX(Time);
         y := GraphY(Tide);
         if (Time >= MinHorizAxis) and (Time <= MaxHorizAxis) then ScreenSymbol(Image1.Canvas,x,y,FilledBox,3,claRed);
         Time := Time + SamplingInterval;
      end;

      Spline(Sample,NumPts,Sample[2,1],Sample[pred(NumPts),1],Splined,500);
      Pen.Color := clMaroon;
      Pen.Width := 2;
      for i := 1 to 500 do if Splined[i,1] < 0 then Splined[i,1] := 0;
      MoveTo(GraphX(Splined[1,1]),GraphY(Splined[1,2]));
      for i := 2 to 500 do LineTo(GraphX(Splined[i,1]),GraphY(Splined[i,2]));
   end;
end;


procedure TNyquistBaseGraph.FormResize(Sender: TObject);
begin
   inherited;
   if GraphDraw.GraphDrawn then DrawSampling;
end;

procedure TNyquistBaseGraph.FormCreate(Sender: TObject);
begin
   inherited;
   SamplingInterval := 10;
   Period := 24.0;
   GraphDraw.HorizLabel := 'Time';
   GraphDraw.MaxHorizAxis := 250;
   GraphDraw.MaxVertAxis := 1.25 * Amplitude;
   GraphDraw.MinVertAxis := -1.25 * Amplitude;
   SetUpGraphForm;
end;

procedure TNyquistBaseGraph.Graphsettings2Click(Sender: TObject);
begin
   inherited;
   if GraphDraw.GraphDrawn then DrawSampling;
end;

procedure TNyquistBaseGraph.Graphparameters1Click(Sender: TObject);
begin
   inherited;
   if GraphDraw.GraphDrawn then DrawSampling;
end;

procedure TNyquistBaseGraph.Font1Click(Sender: TObject);
begin
   inherited;
   if GraphDraw.GraphDrawn then DrawSampling;
end;

procedure TNyquistBaseGraph.Sameinterval1Click(Sender: TObject);
begin
   inherited;
   FormResize(Sender);
end;



procedure TNyquistBaseGraph.Newinterval1Click(Sender: TObject);
begin
   repeat
      ReadDefault('New sampling interval',SamplingInterval);
   until SamplingInterval > 0.00001;
   FormResize(Sender);
end;

procedure TNyquistBaseGraph.Change1Click(Sender: TObject);
begin
   ReadDefault('New period',Period);
   inherited;
   FormResize(Sender);
end;


procedure TNyquistBaseGraph.Run1Click(Sender: TObject);
var
   tfile : file of float64;
   rfile : file;
   i : integer;
   wave,time   : float64;
   coord : tGraphPoint32;
   ThisGraph    : tThisBaseGraph;
   FFTGraph : TFFTGraph;
begin
   FFTGraph := TFFTGraph.Create(Application);
   FFTGraph.Caption := 'FFT Power spectrum with sampling interval:  ' + RealToString(SamplingInterval,-8,-2) + '  from Period:  ' + RealToString(Period,-8,-2);

   ThisGraph := TThisBaseGraph.Create(Application);
   ThisGraph.Caption := 'Sampled Time Series with sampling interval:  ' + RealToString(SamplingInterval,-8,-2) + '  from Period:  ' + RealToString(Period,-8,-2);

   with FFTGraph do begin
      TotalNumberPoints := 64;
      BinTime := SamplingInterval;
      ReadDefault('Points to sample',TotalNumberPoints);

      with ThisGraph,GraphDraw do begin
         ThisGraph.GraphDraw.MaxHorizAxis := TotalNumberPoints * BinTime;
         ThisGraph.GraphDraw.MaxVertAxis := Amplitude;
         ThisGraph.GraphDraw.MinVertAxis := -MaxVertAxis;
         ThisGraph.SetUpGraphForm;
      end;

      FFTfileName := MDTempDir + 'temp.fft';
      assignFile(tfile,fftfilename);
      rewrite(tfile);
      ThisGraph.OpenDataFile(rfile,'');

      Time := StartTime;

      for i := 1 to TotalNumberPoints do begin
         Wave := Amplitude * cos(Time / Period * TwoPi);
         Time := BinTime * i + StartTime;
         write(tfile,wave);
         coord[1] := time;
         coord[2] := wave;
         Blockwrite(rfile,coord,1);
      end;
      closefile(tfile);
      closeFile(rfile);
      with ThisGraph,Image1.Canvas do begin
         Pen.Color := clRed;
         Pen.Width := 3;
         RedrawDiagram11Click(Nil);
      end;
      FFTGraph.FastFourierTransform;
   end;
end;


procedure TNyquistBaseGraph.FFT2Click(Sender: TObject);
begin
   Run1Click(Sender);
end;

procedure TNyquistBaseGraph.SpeedButton8Click(Sender: TObject);
begin
   FormResize(Sender)
end;

procedure TNyquistBaseGraph.SpeedButton9Click(Sender: TObject);
begin
   Newinterval1Click(Sender);
end;


end.
