unit petfouri;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define FFTGraphProblems}
   //{$Define MemProblems}
{$EndIf}


interface

uses
  Windows,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,Math,
  Menus, ExtCtrls,
  Petmath,Petmar_types,BASEGRAF,PETMAR, Buttons, ToolWin, ComCtrls;

type
  WindowType = (Parzen,Square,Welch);

type
  TFFTGraph = class(TThisBaseGraph)
    Reprocess1: TMenuItem;
    Changeparameters1: TMenuItem;
    SeekPeakButton: TSpeedButton;
    fftbutton: TSpeedButton;
    SlopeSpeedButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Changeparameters1Click(Sender: TObject);
    procedure SeekPeakButtonClick(Sender: TObject);
    procedure fftbuttonClick(Sender: TObject);
    procedure SlopeSpeedButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TotalNumberPoints,
    SegSize             : integer;
    BinTime             : float64;
    FFTfileName         : PathStr;
    GraphPeriod         : boolean;
    ACaption            : ShortString;
    FFTGraphName,
    BinUnits            : string35;
    WindowFunction      : WindowType;
    PowerTables,
    Double,
    NewAxes,
    ZeroPad             : boolean;
    SegExp              : integer;
    function FastFourierTransform : boolean;
    procedure GetSlopeLofOfValues(ShowResults : boolean; var a,b,r : float64);
    function NewOptions : boolean;
    function ComputeAndDraw : boolean;
  end;


procedure PowerSpectrumByMaximumEntropy(MEMPowerDefaults : tMEMPowerDefaults;  var Slope : float32; var FFTGraph : TFFTGraph; Title : ShortString;
    DataFNames : tStringList; NumRecs : integer; BinTime : float32);


implementation

{$R *.DFM}

uses
   FourOpF,

   Nevadia_Main;


PROCEDURE memcof(var data: array of float32; n,m: integer; VAR pm: float64; VAR cof : array of float32);
//Maximum Entropy power spectrum //from Press and others, 1989, Numerical Recipes
//n is the length of the input data array
//m is the number of coefficients to return
var
   k,j,i : integer;
   pneum,p,denom : float64;
   wk1,wk2,wkm : ^bfarray32;  //array[1..12000] of float64;
begin
   New(wk1);
   New(wk2);
   New(wkm);
   p := 0.0;
   for j := 0 to n-1 do p := p+sqr(data[j]);
   pm := p/n;
   wk1^[1] := data[0];
   wk2^[n-1] := data[n-1];

   for j := 2 to n-1 do begin
      wk1^[j] := data[j-1];
      wk2^[j-1] := data[j-1]
   end;

   for k := 1 to m do begin
      pneum := 0.0;
      denom := 0.0;
      for j := 1 to n-k do begin
         pneum := pneum+wk1^[j]*wk2^[j];
         denom := denom+sqr(wk1^[j])+sqr(wk2^[j])
      end;
      cof[k] := 2.0*pneum/denom;
      pm := pm*(1.0-sqr(cof[k]));
      IF (k <> 1) then for i := 1 to k-1 do cof[i] := wkm^[i]-cof[k]*wkm^[k-i];
      if (k <> m) then begin
         for i := 1 to k do wkm[i] := cof[i];
         for j := 1 to n-k-1 do begin
            wk1^[j] := wk1^[j]-wkm^[k]*wk2^[j];
            wk2^[j] := wk2^[j+1]-wkm^[k]*wk1^[j+1]
         end;
      end;
   end;
   Dispose(wk1);
   Dispose(wk2);
   Dispose(wkm);
end;


FUNCTION evlmem(fdt : float32; cof : array of float32; m: integer; pm : float32): float32;
//Maximum Entropy power spectrum  from Press and others, 1989, Numerical Recipes
VAR
   wr,wi,wpr,wpi,wtemp,theta: double;
   sumi,sumr: real;
   i: integer;
BEGIN
   theta := 6.28318530717959*fdt;
   wpr := cos(theta);
   wpi := sin(theta);
   wr := 1.0;
   wi := 0.0;
   sumr := 1.0;
   sumi := 0.0;
   FOR i := 0 to pred(m) DO BEGIN
      wtemp := wr;
      wr := wr*wpr-wi*wpi;
      wi := wi*wpr+wtemp*wpi;
      sumr := sumr-cof[i] * wr;
      sumi := sumi-cof[i] * wi;
   END;
   evlmem := pm/(sqr(sumr)+sqr(sumi));
END;


procedure PowerSpectrumByMaximumEntropy(MEMPowerDefaults : tMEMPowerDefaults; var Slope : float32; var FFTGraph : TFFTGraph; Title : ShortString;
    DataFNames : tStringList; NumRecs : integer; BinTime : float32);
//type
   //bfArray = array[0..12000] of float32;
var
   DataFile : tStringList;
   DataFName: PathStr;
   n,i,j,err : integer;
   MomentVar : tMomentVar;
   MaxPeriod,a,b,r,pm,fdt,MaxPower : float64;
   v         : array[1..2] of float32;
   rFile : file;
   cof : array[0..100] of float32;
   Slopes : ^bfarray32;  //array[0..100] of float32;
   DataArray : ^bfArray32;
   Results : tStringList;
   //DoResults : boolean;
begin
   {$IfDef MemProblems} WriteLineToDebugFile('PowerSpectrumByMaximumEntropy in'); {$EndIf}
   MaxPower := -9999;
   MaxPeriod := NumRecs * BinTime;
   New(DataArray);
   New(Slopes);
   FFTGraph := TFFTGraph.Create(Application);
   with FFTGraph,GraphDraw,MEMPowerDefaults do begin
      if LogLogPlot then begin
         MinVertAxis := log10(0.01);
         MaxVertAxis := log10(100000);
         MaxHorizAxis := log10(LastFreq * MaxPeriod); //BinTime / 0.1;  //0.3;
         MinHorizAxis := log10(FirstFreq * MaxPeriod);  //BinTime / 0.3;  //0.1;
      end
      else begin
         VertAxisFunct := PetMath.Log10;
         VertAxisFunctionType := Log10Axis;
         MinVertAxis := 0.01;
         MaxVertAxis := 100000;
         MaxHorizAxis := LastFreq * MaxPeriod;
         MinHorizAxis := FirstFreq * MaxPeriod;
      end;
      VertLabel := 'Power';
      HorizLabel := Title;
      SetUpGraphForm;
   end;
   cof[0] := 0;

   {$IfDef MemProblems}
      //DoResults := true;
   {$Else}
      //DoResults := (not SkipDrawing);
   {$EndIf}

   //if DoResults then begin
      Results := tStringList.Create;
      Results.Add(Title);
      Results.Add('');
      Results.Add('  slope     r');
      Results.Add('=================');
   //end;

   for j := 0 to pred(DataFNames.Count) do begin
      DataFName := DataFNames.Strings[j];
      DataFile := tStringList.Create;
      DataFile.LoadFromFile(DataFName);
      MomentVar.NPts  := DataFile.Count;
      {$IfDef MemProblems} WriteLineToDebugFile(IntToStr(MomentVar.NPts) + '   ' +  DataFName);  {$EndIf}
      if (MomentVar.NPts  > 3 * NumRecs div 4) then begin
         for i := 0 to pred(MomentVar.NPts ) do DataArray^[i] := StrToFloat(DataFile.Strings[i]);
         //Val(DataFile.Strings[i],DataArray^[i],err);

         memcof(dataarray^,MomentVar.NPts,MEMPowerDefaults.NumPoles,pm,cof);

         with FFTGraph,MEMPowerDefaults do begin
            OpenDataFile(rfile,'');
            ACaption := Title + ' Power spectrum by Maximum Entropy Method';
            {if (not CreateGraphHidden) then} Caption := ACaption;
            fdt := FirstFreq;
            while (fdt <= LastFreq) do begin
               v[1] := fdt * MaxPeriod;
               v[2] := evlmem(fdt,cof,NumPoles,pm);
               if LogLogPlot then begin
                  v[1] := log10(v[1]);
                  v[2] := log10(v[2]);
               end;
               if (v[2] > MaxPower) then MaxPower := v[2];
               fdt := fdt + 0.005;
               BlockWrite(rfile,v,1);
            end;
            CloseFile(RFile);
         end;
      end;
      DataFile.Free;
   end;

   with FFTGraph do begin
      SetUpGraphForm;
      RedrawDiagram11Click(Nil);
      MomentVar.NPts := 0;
      for i := 0 to pred(GraphDraw.DataFilesPlotted.Count) do begin
         FitGraph(true,2,GraphDraw.DataFilesPlotted.Strings[i], a,b,r,n);
         if (abs(b) > 0.01) and (abs(b) < 10) then begin
            Results.Add(RealToString(b,8,3)+ RealToString(r,8,3));
            Slopes^[MomentVar.NPts] := b;
            Inc(MomentVar.NPts);
         end;
      end;
   end;
   Moment(Slopes^,MomentVar,msAll);
   //if DoResults then begin
      Results.Add('');
      Results.Add('Average slope: ' + RealToString(MomentVar.mean,-18,-3));
      Results.Add('Median slope:  ' + RealToString(MomentVar.Median,-18,-3));
      Results.Add('');
      Results.Add('Fractal dimension: ' + RealToString(FracDimFromSlope2(Slope),-18,-3));
      //if (not SkipDrawing)then
         Petmar.DisplayAndPurgeStringList(Results,'MEM Power Spectrum Slopes');
   Dispose(DataArray);
   Dispose(Slopes);
end;


function TFFTGraph.FastFourierTransform : boolean;
begin
   Double := true;
   ZeroPad := true;
   SegExp := 11;
   while Math.IntPower(2,SegExp) > TotalNumberPoints do dec(SegExp);
   SegSize := round(IntPower(2,SegExp));
   WindowFunction := Welch;
   NewAxes := true;
   Result := ComputeAndDraw;
end;


procedure TFFTGraph.FormCreate(Sender: TObject);
begin
   inherited;
   GraphPeriod := true;
   BinTime := 1;
   BinUnits := '';
   NewAxes := true;
   MainMenu1.AutoMerge := Not SkipMenuUpdating;
   Reprocess1.Visible := Not SkipMenuUpdating;
end;


procedure TFFTGraph.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   inherited;
   DeleteFile(FFTfileName);
   DeleteFile(MDTempDir + 'Fourier.txt');
end;


function TFFTGraph.ComputeAndDraw : boolean;
const
   MaxPoints = 4096;
TYPE
   glmarray = array [1..MaxPoints] of float64;
   gl4marray = array [1..4*MaxPoints] of float64;
   gldarray = array [1..2*MaxPoints] of float64;
var
   FFTFile : file of float64;

   PROCEDURE four1(var data: gl4marray; nn,isign: integer);
   var
      ii,jj,n,mmax,m,j,istep,i  : integer;
      wtemp,wr,wpr,wpi,wi,theta,
      tempr,tempi               : float64;
   begin
      n := 2*nn;
      j := 1;
      for ii := 1 to nn DO begin
         i := pred(2*ii);
         IF (j > i) THEN begin
            tempr := data[j];
            tempi := data[succ(j)];
            data[j] := data[i];
            data[succ(j)] := data[succ(i)];
            data[i] := tempr;
            data[succ(i)] := tempi;
         end;
         m := n DIV 2;
         WHILE ((m >= 2) AND (j > m)) DO begin
            dec(j,m);
            m := m DIV 2;
         END;
         inc(j,m);
      end;
      mmax := 2;
      WHILE (n > mmax) DO begin
         istep := 2*mmax;
         theta := TwoPi / (isign*mmax);
         wpr := -2.0*sqr(sin(0.5*theta));
         wpi := sin(theta);
         wr := 1.0;
         wi := 0.0;
         for ii := 1 to (mmax DIV 2) DO begin
            m := pred(2*ii);
            for jj := 0 to ((n-m) DIV istep) DO begin
               i := m + jj*istep;
               j := i+mmax;
               tempr := (wr)*data[j]-(wi)*data[succ(j)];
               tempi := (wr)*data[succ(j)]+(wi)*data[j];
               data[j] := data[i]-tempr;
               data[succ(j)] := data[succ(i)]-tempi;
               data[i] := data[i] + tempr;
               data[succ(i)] := data[succ(i)]+tempi;
            end;
            wtemp := wr;
            wr := wr*wpr-wi*wpi+wr;
            wi := wi*wpr+wtemp*wpi+wi;
         end;
         mmax := istep;
      end
   end;


   procedure spctrm(var p : glmarray; m,k : integer; ovrlap : boolean; WindowFunction : WindowType);
   //based on routine from Press and others, 1999, Numerical recipes
   var
      mm,m44,m43,m4,kk,joffn,joff,j2,j,jj: integer;
      w,sumw,facp,facm,den: float64;
      w1: ^gl4marray;
      w2: ^glmarray;

      FUNCTION window(j : integer; facm,facp : float64): float64;
      begin
         case WindowFunction of
            Parzen : window := (1.0-abs(((pred(j))-facm)*facp));
            Square : window := 1.0;
            Welch  : window := (1.0-sqr(((pred(j))-facm)*facp));
            else Window := 1;
         end {case};
      end;

   begin
      mm := m+m;
      m4 := mm+mm;
      m44 := m4+4;
      m43 := m4+3;
      den := 0.0;
      facm := m-0.5;
      facp := 1.0/(m+0.5);
      sumw := 0.0;
      for j := 1 to mm do sumw := sumw+sqr(window(j,facm,facp));
      for j := 1 to m do p[j] := 0.0;
      GetMem(w2,m*SizeOf(float64));
      GetMem(w1,round(4.0*m*SizeOf(float64)));
      if (ovrlap) then for j := 1 to m do if EOF(FFTfile) then w2^[j] := 0
      else read(FFTfile,w2^[j]);

      if ShowGraphProgress then StartProgress('FFT segments');
      for kk := 1 to k do begin
         if ShowGraphProgress then UpdateProgressBar(kk/k);
         for joff := -1 to 0 do begin
            if (ovrlap) then begin
               for j := 1 to m do w1^[joff+j+j] := w2^[j];
               for j := 1 to m do if EOF(fftfile) then w2^[j] := 0 else read(fftfile,w2^[j]);
               joffn := joff+mm;
               for j := 1 to m do w1^[joffn+j+j] := w2^[j]
            end
            else begin
               for jj := 0 to ((m4-joff-2) div 2) do begin
                  j := joff+2+2*jj;
                  if EOF(FFTfile) then w1^[j] := 0 else read(FFTfile,w1^[j]);
               end
            end
         end;
         for j := 1 to mm do begin
            j2 := j+j;
            w := window(j,facm,facp);
            w1^[j2] := w1^[j2]*w;
            w1^[j2-1] := w1^[j2-1]*w
         end;
         four1(w1^,mm,1);
         p[1] := p[1]+sqr(w1^[1])+sqr(w1^[2]);
         for j := 2 to m do begin
            j2 := j+j;
            p[j] := p[j]+sqr(w1^[j2])+sqr(w1^[j2-1])+sqr(w1^[m44-j2])+ sqr(w1^[m43-j2]);
         end;
         den := den+sumw;
      end {for kk};
      if ShowGraphProgress then EndProgress;
      den := m4*den;
      for j := 1 to m do p[j] := p[j]/den;
      FreeMem(w1,round(4.0 * SizeOf(float64)*m));
      FreeMem(w2,SizeOf(float64)*m);
   end;


var
   p : glmarray;
   NSegs,j : integer;
   MaxPower,
   MinY,MaxY : float64;

   procedure PowerTable(Reorder : boolean);
   var
      t : float64;
      i,j,k       : integer;
      Freq        : float64;
      p2          : ^glmarray;
      Order       : array[1..MaxPoints] of integer;
      TStr,TStr2 : ShortString;
      Results : tStringList;
   begin
      New(p2);
      for i := 2 to SegSize do begin
         Order[pred(i)] := i;
         p2^[Pred(i)] := p[i];
      end {for i};

      if reorder then begin
         if ShowGraphProgress then StartProgress('Order');
         for j := 1 to pred(SegSize) do begin
            if ShowGraphProgress then UpdateProgressBar(j/SegSize);
            for i := 1 to (SegSize-j) do begin
               if (p2^[i] < p2^[succ(i)]) then begin
                  t := p2^[i];
                  p2^[i] := p2^[succ(i)];
                  p2^[succ(i)] := t;
                  k := Order[i];
                  Order[i] := Order[succ(i)];
                  Order[succ(i)] := k;
               end {if};
            end;
         end;
         if ShowGraphProgress then EndProgress;
         TStr2 := ' by power';
      end
      else TStr2 := ' by period';

      if (SegSize <= 32) then K := SegSize
      else k := SegSize div 2;

      if PowerTables then begin
         Results := tStringList.Create;
         Results.Add(FFTGraphName);
         Results.Add('');
         Results.Add('Bin    Frequency        Period              Power');
      end;

      for i := 1 to k do if (Order[i] > 1) then begin
         Freq := pred(Order[i])/(2*SegSize*BinTime);
         if p2^[i] < 0.00001 then TStr := RealToString(p2^[i],18,12)
         else TStr := RealToString(p2^[i],18,6);

         if PowerTables then begin
            if Order[i] = 2 then Results.Add(IntegerToString(pred(order[I]),5) +  '     0.0            infinite'+TStr)
            else Results.Add(IntegerToString(pred(order[I]),5)+ RealToString(Freq,10,5) + RealToString((1/Freq),18,6)+TStr);
         end;
      end;
      if PowerTables then Petmar.DisplayAndPurgeStringList(Results,'FFT statistics: ' + Acaption + TStr2);
      if IsNAN(p2^[1]) then begin
          p2^[1] :=  p2^[2];
      end;
      
      MaxY := p2^[1];
      MinY := p2^[pred(SegSize)];
      Dispose(p2);
   end;


   procedure GraphPowerSpectrum;
   var
      i      : integer;
      rfile  : file;
      v      : array[1..2] of float32;
   begin
      with GraphDraw,Image1.Canvas do begin
         if NewAxes then begin
            MinVertAxis := MinY;
            MaxVertAxis := MaxY;
            if GraphPeriod then begin
               MinHorizAxis := (2*SegSize*BinTime/pred(SegSize));
               MaxHorizAxis := (2*SegSize*BinTime);
               HorizLabel := 'Period ' + BinUnits;
            end
            else begin
               MaxHorizAxis := pred(SegSize)/(2*SegSize*BinTime);
               MinHorizAxis := 1/(2*succ(SegSize)*BinTime); {0.5 / BinTime;}
               HorizLabel := 'Frequency ' + BinUnits + '^-1';
            end;
            HorizAxisFunct := Log10;
            VertAxisFunct := Log10;
            HorizAxisFunctionType := Log10Axis;
            VertAxisFunctionType := Log10Axis;
            VertLabel := 'Power';
            NewAxes := false;
            {if not CreateGraphHidden then} Caption := 'FFT Power Spectrum--' + ACaption;
         end;

         {$IfDef FFTGraphProblems} WriteLineToDebugFile('GraphPowerSpectrum, ' + AxisRange); {$EndIf}

         SetUpGraphForm;
         OpenDataFile(rfile,'');
         for i := 3 to SegSize do begin
            v[2] := p[i];
            v[1] := pred(i) / (2 * SegSize*BinTime);
            if GraphPeriod then v[1] := 1 / v[1];
            BlockWrite(rfile,v,1);
         end;
         CloseFile(RFile);
         RedrawDiagram11Click(Nil);
      end {with};
   end;


begin
   {$IfDef FFTGraphProblems} WriteLineToDebugFile('Compute and draw in'); {$EndIf}
   try
      Result := false;
      assignFile(FFTfile,FFTFileName);
      reset(FFTFile);
      NSegs := TotalNumberPoints div SegSize;
      if (NSegs * SegSize <> TotalNumberPoints) and ZeroPad then inc(NSegs);
      spctrm(p,SegSize,NSegs,Double,WindowFunction);

      MaxPower := 0;
      for j := 1 to SegSize do if (p[j] > MaxPower) then MaxPower := p[j];
      for j := 1 to SegSize do p[j] := 100 * p[j] / MaxPower;

      if (MaxPower > 0.0000001) then begin
         if (Acaption = '') then ACaption := ExtractFilename(FFTFileName);
         PowerTable(false);
         PowerTable(true);
         if (abs(MinY-MaxY) > 0.001)  then begin
            GraphPowerSpectrum;
            Result := true;
         end;
      end;
   finally
      closeFile(FFTfile);
   end;
   {$IfDef FFTGraphProblems} WriteLineToDebugFile('Compute and draw out'); {$EndIf}
end;


function TFFTGraph.NewOptions;
begin
   FourierOptionsForm := tFourierOptionsForm.Create(Self);
   with FourierOptionsForm do begin
      Edit1.Text := RealToString(BinTime,-18,-8);
      Edit2.Text := BinUnits;
      RadioButton1.Checked := WindowFunction = Parzen;
      RadioButton2.Checked := WindowFunction = Square;
      RadioButton3.Checked := WindowFunction = Welch;
      CheckBox4.Checked := Double;
      CheckBox5.Checked := ZeroPad;
      RadioButton6.Checked := SegSize = 16;
      RadioButton7.Checked := SegSize = 32;
      RadioButton8.Checked := SegSize = 64;
      RadioButton9.Checked := SegSize = 128;
      RadioButton10.Checked := SegSize = 256;
      RadioButton11.Checked := SegSize = 512;
      RadioButton12.Checked := SegSize = 1024;
      RadioButton13.Checked := SegSize = 2048;
      RadioButton4.Checked := SegSize = 4096;

      RadioButton6.Enabled := TotalNumberPoints >= 16;
      RadioButton7.Enabled := TotalNumberPoints >= 32;
      RadioButton8.Enabled := TotalNumberPoints >= 64;
      RadioButton9.Enabled := TotalNumberPoints >= 128;
      RadioButton10.Enabled := TotalNumberPoints >= 256;
      RadioButton11.Enabled := TotalNumberPoints >= 512;
      RadioButton12.Enabled := TotalNumberPoints >= 1024;
      RadioButton13.Enabled := TotalNumberPoints >= 2048;
      RadioButton4.Enabled := TotalNumberPoints >= 4096;

      RadioButton14.Checked := GraphPeriod;
      RadioButton15.Checked := not GraphPeriod;
      if FourierOptionsForm.ShowModal = IdCancel then Result := false
      else begin
         Result := true;
         if RadioButton1.Checked then WindowFunction := Parzen;
         if RadioButton2.Checked then WindowFunction := Square;
         if RadioButton3.Checked then WindowFunction := Welch;
         Double := CheckBox4.Checked;
         ZeroPad := CheckBox5.Checked;
         if RadioButton6.Checked then SegSize := 16;
         if RadioButton7.Checked then SegSize := 32;
         if RadioButton8.Checked then SegSize := 64;
         if RadioButton9.Checked then SegSize := 128;
         if RadioButton10.Checked then SegSize := 256;
         if RadioButton11.Checked then SegSize := 512;
         if RadioButton12.Checked then SegSize := 1024;
         if RadioButton13.Checked then SegSize := 2048;
         if RadioButton4.Checked then SegSize := 4096;
         GraphPeriod := RadioButton14.Checked;
         while (SegSize > TotalNumberPoints) do SegSize := SegSize div 2;
         CheckEditString(Edit1.Text,BinTime);
         BinUnits := Edit2.Text;
      end;
      FourierOptionsForm.Free;
   end;
end;


procedure TFFTGraph.Changeparameters1Click(Sender: TObject);
var
   i : integer;
begin
   if NewOptions then begin
      for i := 1 to GraphDraw.DataFilesPlotted.Count do DeleteFile(GraphDraw.DataFilesPlotted.Strings[pred(i)]);
      GraphDraw.DataFilesPlotted.Free;
      GraphDraw.DataFilesPlotted := TStringList.Create;
      NewAxes := true;
      ComputeAndDraw;
   end;
end;

procedure TFFTGraph.SeekPeakButtonClick(Sender: TObject);
const
   ASize = 4096;
type
   CoordArray = array[1..(2*ASize)] of float64;
var
   NumRead,
   i,j : integer;
   yp,ys,rp,rs,Amp,MaxAmp,WhereMax,
   PeakSize,MinPeriod,
   x,y : float64;
   tf    : file;
   Coords : ^CoordArray;
   OFile  : TextFile;
begin
   PeakSize := 200;
   ReadDefault('Peak amplitude',PeakSize);
   MinPeriod := 4;
   ReadDefault('Min period',MinPeriod);
   assignFile(Ofile,MDTempDir + 'fft-peaks.txt');
   rewrite(Ofile);
   assignFile(tf,GraphDraw.DataFilesPlotted.Strings[0]);
   reset(tf,2*SizeOf(float64));
   new(Coords);
   MaxAmp := 0;
   WhereMax := 0;
   try
   while not EOF(tf) do begin
      BlockRead(tf,Coords^,ASize,Numread);
      for i := 3 to pred(NumRead) do begin
         x := Coords^[pred(2*i)];
         y := Coords^[2*i];
         j := i;
         while (y > Coords^[2*pred(j)]) and (j > 2) and (abs(i-j) < 10) do dec(j);
         yp := Coords^[2*pred(j)];

         j := i;
         while (y > Coords^[2*succ(j)]) and (j < NumRead) and (abs(i-j) < 10) do inc(j);
         ys := Coords^[2*succ(j)];

         if (abs(yp) > 1e-9) and (abs(ys) > 1e-9) then begin
            rp := y/yp;
            rs := y/ys;
            if (X >= MinPeriod) and ((rp > PeakSize) and (rs > PeakSize)) then writeln(Ofile,x:8:2,rp:10:2,rs:10:2);
            Amp := 0.5 * (rp + rs);
            if Amp > MaxAmp then begin
               MaxAmp := Amp;
               WhereMax := x;
            end;
         end;
      end;
   end;
   finally
   end;
   Dispose(Coords);
   closeFile(tf);
   writeln(Ofile);
   writeln(Ofile,'Max amplitude peak at x=' + RealToString(WhereMax,-8,2) + '  with amplitude of ' + RealToString(MaxAmp,8,2));
   CloseFile(OFile);
   QuickOpenEditWindow(MDTempDir + 'Fft-peaks.txt','FFT peaks');
   SysUtils.DeleteFile(MDTempDir + 'Fft-peaks.txt');
end;


procedure TFFTGraph.SlopeSpeedButtonClick(Sender: TObject);
var
   a,b,r : float64;
begin
   GetSlopeLofOfValues(True,a,b,r);
end;


procedure TFFTGraph.GetSlopeLofOfValues(ShowResults : boolean; var a,b,r : float64);
var
   infile,outfile : file;
   v       : array[1..2] of float32;
   fName : PathStr;
   n  : integer;
   Results : tStringList;
begin
   assignFile(infile,GraphDraw.DataFilesPlotted.Strings[0]);
   reset(infile,2*SizeOf(float64));

   fName := MDTempDir + 'qq_fft_xyz.tmp';
   assignFile(outfile,fName);
   rewrite(outfile,2*SizeOf(float64));

   while not EOF(infile) do begin
      BlockRead(infile,v,1);
      v[1] := log10(v[1]);
      v[2] := log10(v[2]);
      BlockWrite(outfile,v,1);
   end;
   CloseFile(InFile);
   CloseFile(OutFile);
   FitGraph(true,2,fName,a,b,r, n);
   SysUtils.DeleteFile(fName);

   if ShowResults then begin
      Results := tStringList.Create;
      Results.Add('Intercept: ' + RealToString(a,-18,-8));
      Results.Add('Slope: ' + RealToString(b,-18,-8));
      Results.Add('r:' + RealToString(r,8,4));
      Results.Add('r²:' + RealToString(sqr(r),8,4));
      Results.Add('n=' + IntToStr(n));
      Results.Add('');
      Petmar.DisplayAndPurgeStringList(Results,'');
   end;
end;

procedure TFFTGraph.fftbuttonClick(Sender: TObject);
begin
  Changeparameters1Click(Sender);
end;


initialization
finalization
  {$IfDef FFTGraphProblems} WriteLineToDebugFile('FFTGraphProblems active in petfouri'); {$EndIf}
  {$IfDef MemProblems} WriteLineToDebugFile('MEMProblems active in petfouri'); {$EndIf}
end.



