unit demtrendopt;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}



{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordTrendSurfaceProblems}
   //{$Define RecordAllTrendSurfaceProblems}
{$EndIf}

interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}

//end needed for inline core DB functions

  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  DEMMapF,DEMDefs,Petmar_types;

type
  TTrendPick = class(TForm)
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    OKBtn: TBitBtn;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    HelpBtn: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    BitBtn1: TBitBtn;
    RedrawSpeedButton12: TSpeedButton;
    Label1: TLabel;
    Edit1: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CurrentColorNumber : integer;
    CurDEM,CurDB : integer;
    theMapOwner : tMapForm;
    GridLimits : tGridLimits;
  end;


tTrendSurf = class
   public
      TrendDEM,DevDEM,
      CurDB,CurDEM,CurrentOrderTrendSurface  : integer;
      r2 : float64;
      theMapOwner : tMapForm;
      GridLimits : tGridLimits;
      Dip,Strike,DipDir : float32;
      DipAndStrike,SlopeStr   : string16;

      constructor Create;
      destructor Destroy;
      procedure SetDEM(inDEM : integer; inLimits : tGridLimits);
      procedure SetDB(inDB : integer);
      procedure ComputeTrendSurface;
   end;


procedure ComputeTrendSurface(CurDB : integer; CurDEM : integer; GridLimits : tGridLimits; theMapOwner : tMapForm);
procedure GetTrendOptions(CurDB : integer; CurDEM : integer; GridLimits : tGridLimits; theMapOwner : tMapForm);
procedure RawComputeTrendSurface(CurDB : integer; CurDEM : integer; GridLimits : tGridLimits; theMapOwner : tMapForm);

function CalculateTrendSurface(CurrentOrderTrendSurface : integer; B : tTrendVector; x,y : float64) : float64;  inline;

implementation

{$R *.DFM}

uses
   Nevadia_Main,
   PETMAR, PetMath,PetImage,
   Petmar_geology,
   BaseGraf,
   DEMDataBase,DEM_indexes,
   DEMCoord,BaseMap,DEMStat,
   computations;


procedure TTrendPick.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TTrendPick.RadioGroup1Click(Sender: TObject);
begin
   MDDef.CurOrdTrendSurf := succ(RadioGroup1.ItemIndex);
end;

procedure TTrendPick.RedrawSpeedButton12Click(Sender: TObject);
begin
  if (Edit1.Text <> '') then begin
     CheckEditString(Edit1.Text,MDDef.CurOrdTrendSurf);
  end;
  RawComputeTrendSurface(CurDB,CurDEM,GridLimits,theMapOwner);

  if MDDef.AutoIncGeoColor then begin
     inc(CurrentColorNumber);
     if (CurrentColorNumber > 10) then CurrentColorNumber := 0;
     MDDef.GeoContactColor := LineColors10[CurrentColorNumber];
     ColorLineWidthBitBtn(BitBtn1,MDDef.GeoContactColor,MDDef.GeoContactWidth);
  end;
end;

procedure GetTrendOptions;
var
   TrendPick : TTrendPick;
begin
   TrendPick := TTrendPick.Create(Application);
   TrendPick.CurDEM := CurDEM;
   TrendPick.CurDB := CurDB;
   TrendPick.theMapOwner := theMapOwner;
   TrendPick.GridLimits := GridLimits;
   TrendPick.Show;
end;


procedure RawComputeTrendSurface;
var
   TrendSurf : tTrendSurf;
begin
   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('RawComputeTrendSurface'); {$EndIf}

   TrendSurf := tTrendSurf.Create;
   TrendSurf.theMapOwner := theMapOwner;
   TrendSurf.SetDEM(CurDEM,GridLimits);
   TrendSurf.SetDB(CurDB);
   TrendSurf.CurrentOrderTrendSurface := MDDef.CurOrdTrendSurf;
   TrendSurf.ComputeTrendSurface;
   TrendSurf.Destroy;

   if (TrendSurf.DevDEM <> 0) then begin
      theMapOwner.OverlayContourFromSecondDEM(TrendSurf.DevDEM,0,MDDef.GeoContactColor);
   end;
   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('RawComputeTrendSurface out'); {$EndIf}
end;


procedure ComputeTrendSurface;
begin
   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile(' ComputeTrendSurface'); {$EndIf}
   GetTrendOptions(CurDB,CurDEM,GridLimits,theMapOwner);
   RawComputeTrendSurface(CurDB,CurDEM,GridLimits,theMapOwner);

   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile(' ComputeTrendSurface out'); {$EndIf}
end;


procedure TTrendPick.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\trend_surf_basic.htm');
end;


procedure TTrendPick.FormCreate(Sender: TObject);
begin
   CurrentColorNumber := 0;
   MDDef.GeoContactColor := LineColors10[CurrentColorNumber];
   ColorLineWidthBitBtn(BitBtn1,MDDef.GeoContactColor,MDDef.GeoContactWidth);

   PlaceFormAtMousePosition(Self);
   CheckBox1.Checked := MDDef.TrendDoGraph;
   CheckBox2.Checked := MDDef.TrendMapDev;
   CheckBox3.Checked := MDDef.TrendHistDev;
   CheckBox4.Checked := MDDef.TrendText;
   CheckBox5.Checked := MDDef.TrendSurfaceOverVoids;
   CheckBox6.Checked := MDDef.TrendSurfMap;
   CheckBox7.Checked := MDDef.TrendOpenMaps;

   RadioGroup1.ItemIndex := pred(MDDef.CurOrdTrendSurf);
end;


procedure TTrendPick.BitBtn1Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Geologic contact',BitBtn1,MDDef.GeoContactColor,MDDef.GeoContactWidth);
end;

procedure TTrendPick.CheckBox1Click(Sender: TObject);
begin
   MDDef.TrendDoGraph := CheckBox1.Checked;
end;

procedure TTrendPick.CheckBox2Click(Sender: TObject);
begin
   MDDef.TrendMapDev := CheckBox2.Checked;
end;

procedure TTrendPick.CheckBox3Click(Sender: TObject);
begin
   MDDef.TrendHistDev := CheckBox3.Checked;
end;

procedure TTrendPick.CheckBox4Click(Sender: TObject);
begin
   MDDef.TrendText := CheckBox4.Checked;
end;

procedure TTrendPick.CheckBox5Click(Sender: TObject);
begin
   MDDef.TrendSurfaceOverVoids := CheckBox5.Checked;
end;

procedure TTrendPick.CheckBox6Click(Sender: TObject);
begin
   MDDef.TrendSurfMap := CheckBox6.Checked;
end;


procedure TTrendPick.CheckBox7Click(Sender: TObject);
begin
   MDDef.TrendOpenMaps := CheckBox7.Checked;
end;


procedure TTrendPick.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.CurOrdTrendSurf);
   if (MDDef.CurOrdTrendSurf > 12) then Edit1.Text := '12';  //2/2/2024, largest value currently working

end;

{ tTrendSurf }


    function CalculateTrendSurface(CurrentOrderTrendSurface : integer; B : tTrendVector; x,y : float64) : float64;
    var
       k,l,jb,kb,IORD2 : integer;
       C : tTrendVector;
    begin
       JB := 1;
       C[1] := 1;
       IORD2 := succ(CurrentOrderTrendSurface)*(CurrentOrderTrendSurface+2) div 2;
       for K := 1 to CurrentOrderTrendSurface do begin
          for L := 1 to K do begin
             inc(JB);
             KB := JB - K;
             C[JB] := C[KB] * X;
          end {for L};
          inc(JB);
          C[JB] := C[KB] * y;
       end {for k};
       Result := 0.0;
       for k := 1 to IORD2 do Result := Result + B[K] * C[K];
     end;



procedure tTrendSurf.ComputeTrendSurface;
{from a program in Davis, 1st ed, p.332}
const
   MaxTrendSurfacePoints = 500000;
var
   A    : ^tTrendMatrix;
   B,C  : tTrendVector;
   NumDataPoints : LongInt;
   ResultsFile : System.text;
   NDF1,NDF2,NDF3 : LongInt;
   IOrd2,I,J,TrendSurfInc,Col,Row,rc       : integer;
   SST,SSR,SSD,AMSR,AMSD,SY,SYY,SYC,SYYC : extended;
   xutmoffset,yutmoffset,R,F,xutm,yutm  : float64;
   z,zs,zMeters,zg : float32;
   FName      : PathStr;
   ThisGraph  : tThisBaseGraph;
   rfile      : file;
   v          : array[1..2] of float32;


      procedure AddPointToTrendSurface(xutm,yutm : float64);  //inline;
      var
         j,k,jb,kb : integer;
      begin
         {$IfDef RecordAllTrendSurfaceProblems} WriteLineToDebugFile('x=' + RealToString(xutm,-18,2) +  '  y=' + RealToString(yutm,-18,2) + '  x=' + RealToString(zMeters,-18,2) ); {$EndIf}
          JB := 1;
          C[1] := 1;
          for J := 1 to CurrentOrderTrendSurface do begin
             for k := 1 to J do begin
                inc(JB);
                KB := JB  - J;
                C[JB] := C[KB] * xutm;
             end {for k};
             inc(JB);
             C[JB] := C[KB] * yutm;
          end {for j};
          for j := 1 to IORD2 do begin
             B[J] := B[J] + C[J] * zMeters;
             for K := 1 to IORD2 do A^[J,K] := A^[J,K] + C[J] * C[K];
          end {for j};
      end;


      procedure AddToOutputFile;
      var
         i : integer;
         x,y : float64;
      begin
         writeln(ResultsFile, '  n=',NumDataPoints);
         writeln(ResultsFile);
         writeln(ResultsFile,'Order ', CurrentOrderTrendSurface,' Trend surface');
         if (CurrentOrderTrendSurface = 1) then begin
            DipAndStrikeFromThreePoints(1000,1000,CalculateTrendSurface(CurrentOrderTrendSurface,B,1000,1000),
                                        10000,1000,CalculateTrendSurface(CurrentOrderTrendSurface,B,10000,1000),
                                        1000,10000,CalculateTrendSurface(CurrentOrderTrendSurface,B,1000,10000),
                                        DipAndStrike,SlopeStr,Dip,Strike,DipDir);
            writeln(ResultsFile);
            writeln(ResultsFile,'Orientation of plane: ' + DipAndStrike + '   ' + SlopeStr);
         end;
         (*
         writeln(ResultsFile);
         writeln(ResultsFile,'Goodness of Fit (r²):',R2:13:5);
         writeln(ResultsFile,'Correlation coefficient:',R:10:5);
         *)
         writeln(ResultsFile);
         writeln(ResultsFile,'Coefficients:');
         writeln(ResultsFile,'   z = C1 + C2*x + C3*y');
         if (CurrentOrderTrendSurface > 1) then writeln(ResultsFile,'          + C4*x^2 + C5*x*y + C6*y^2');
         if (CurrentOrderTrendSurface > 2) then writeln(ResultsFile,'          + C7*x^3 + C8*x^2*y + C9*x*y^2 + C10*y^3');
         if (CurrentOrderTrendSurface > 3) then writeln(ResultsFile,'          + C11*x^4 + C12*x^3*y + C13*x^2*y^2 + C14*x*y^3 + C15*y^4');
         if (CurrentOrderTrendSurface > 4) then writeln(ResultsFile,'          + 5th order terms');
         if (CurrentOrderTrendSurface > 6) then writeln(ResultsFile,'          + 6th order terms');
         if (CurrentOrderTrendSurface > 6) then writeln(ResultsFile,'          + 7th order terms');
         if (CurrentOrderTrendSurface > 7) then writeln(ResultsFile,'          + 8th order terms');
         for i := 1 to IORD2 do begin
            write(ResultsFile,B[i]:25:12,'   ',B[i]:15,'    =C',i);
            if (i = 1) or (i > MaxCoefs) then writeln(ResultsFile)
            else writeln(ResultsFile,'*' +  TrendSurMultiples[i]);
         end;
         writeln(ResultsFile);
         writeln(ResultsFile);
         writeln(ResultsFile,'x and y coordinates offset before substition into equation by:');
         writeln(ResultsFile,xutmoffset:12:2,yutmoffset:12:2);
         writeln(ResultsFile);
         writeln(ResultsFile);
         if (CurrentOrderTrendSurface = 2) then begin
            y := (-B[2] + 2 * B[3] * B[4] / B[5]) / (B[5] - 4 * B[4] * B[6] / B[5]);
            x := (-B[3] - 2 * B[6] * Y) / B[5];
            writeln(ResultsFile,'Critical Point:');
            writeln(ResultsFile,'    Partials dz/dx = 0 and dz/dy = 0 at   x=',RealToString(x,-18,-2),' and  y=',RealToString(y,-18,-2));
         end;

         (*
         writeln(ResultsFile);
         writeln(ResultsFile);
         writeln(ResultsFile,'Source of              Sum of  Degrees of   Mean');
         writeln(ResultsFile,'Variation             Squares   Freedom    Squares    F -test');
         writeln(ResultsFile,'-------------------------------------------------------------');
         writeln(ResultsFile,' Regression',SSR:20:2,NDF1:8,AMSR:12:2);
         writeln(ResultsFile,F:61:4);
         writeln(ResultsFile,' Deviation ',SSD:20:2,NDF2:8,AMSD:12:2);
         writeln(ResultsFile);
         writeln(ResultsFile,'Total Variation ',SST:15:2,NDF3:8);
         *)
      end;


begin {procedure tTrendSurf.ComputeTrendSurface}
   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('ComputeTrendSurfaceFromDEM in'); {$EndIf}
   SkipMenuUpdating := true;
   if (CurDEM <> 0) then begin
      DEMGlb[CurDEM].ClipDEMGridInteger(GridLimits.XGridLow,GridLimits.YGridLow);
      DEMGlb[CurDEM].ClipDEMGridInteger(GridLimits.XGridHigh,GridLimits.YGridHigh);
      if MDDef.TrendSurfMap then begin
         if not OpenAndZeroNewDEM(true,DEMGlb[CurDEM].DEMheader,TrendDEM,DEMGlb[CurDEM].AreaName + ' Trend_Order_'+ IntToStr(CurrentOrderTrendSurface),InitDEMmissing) then exit;
      end;
      if MDDef.TrendMapDev then begin
         if not OpenAndZeroNewDEM(true,DEMGlb[CurDEM].DEMheader,DevDEM,'Deviations_from_trend_Surface_' + IntToStr(CurrentOrderTrendSurface),InitDEMmissing) then exit;
      end;
   end;

   New(A);
   C[1] := 1.0;

   IORD2 := succ(CurrentOrderTrendSurface)*(CurrentOrderTrendSurface+2) div 2;
   {ZERO SLE MATRIX }
   for I := 1 to IORD2 do begin
      B[I] := 0;
      for J := 1 to IORD2 do A^[I,J] := 0;
   end;

   if MDDef.TrendText then begin
      FName := NextFileNumber(MDTempDir, 'TRENDSUR_','.DAT');
      assignFile(ResultsFile,fName);
      rewrite(ResultsFile);
      if (CurDEM <> 0) then writeln(ResultsFile,DEMGlb[CurDEM].AreaName);
      if (CurDB <> 0) then writeln(ResultsFile,GISdb[CurDB].DBName);
      writeln(ResultsFile);
   end;

   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('ComputeTrendSurface pt 3'); {$EndIf}

 { CALC. SLE MATRIX }
   if MDDef.TrendOpenMaps then StartProgress('Points for trend surface ' + IntToStr(CurrentOrderTrendSurface));

   if (CurDEM <> 0) then begin
      {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('CurDEM=' + IntToStr(CurDEM)); {$EndIf}
      TrendSurfInc := 1;
      DEMGlb[CurDEM].DEMGridToUTM(GridLimits.XGridLow,GridLimits.YGridLow,xutmoffset,yutmoffset);
      Col := GridLimits.XGridHigh - GridLimits.XGridLow;
      Row := GridLimits.YGridHigh - GridLimits.YGridLow;
      while (Col div TrendSurfInc) * (Row div TrendSurfInc) > MaxTrendSurfacePoints do inc(TrendSurfInc);
      Col := GridLimits.XGridLow;
      while Col <= GridLimits.XGridHigh do begin
         {$IfDef RecordAllTrendSurfaceProblems} WriteLineToDebugFile('Col=' + IntToStr(Col));  {$EndIf}
         if MDDef.TrendOpenMaps and (Col mod 25 = 0) then UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
         Row := GridLimits.YGridLow;
         while Row <= GridLimits.YGridHigh do begin
            if DEMGlb[CurDEM].GetElevMeters(Col,Row,zMeters) then begin
               DEMGlb[CurDEM].DEMGridToUTM(Col,Row,xutm,yutm);
               AddPointToTrendSurface(xutm-xutmoffset,yutm-yutmoffset);
            end;
            inc(Row,TrendSurfInc);
         end {for Row};
         inc(Col,TrendSurfInc);
      end {for Col};
      if MDDef.TrendOpenMaps then EndProgress;
   end;

   if (CurDB <> 0) then begin
      {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('CurDB=' + IntToStr(CurDB)); {$EndIf}
      xutmoffset := 0;
      yutmoffset := 0;
      GISdb[CurDB].MyData.First;
      xutmoffset := GISdb[CurDB].MyData.GetFieldByNameAsFloat(GISdb[CurDB].dbOpts.XField);
      yutmoffset := GISdb[CurDB].MyData.GetFieldByNameAsFloat(GISdb[CurDB].dbOpts.YField);
      GISdb[CurDB].EmpSource.Enabled := false;
      StartProgress('Extract pts');
      rc := GISdb[CurDB].MyData.RecordCount;
      i := 0;
      while not GISdb[CurDB].MyData.eof do begin
         inc(i);
         if (I mod 1000 = 0) then UpdateProgressBar(i/rc);
         zMeters := GISdb[CurDB].MyData.GetFieldByNameAsFloat(GISdb[CurDB].dbOpts.ZField);
         xutm := GISdb[CurDB].MyData.GetFieldByNameAsFloat(GISdb[CurDB].dbOpts.XField);
         yutm := GISdb[CurDB].MyData.GetFieldByNameAsFloat(GISdb[CurDB].dbOpts.YField);
         AddPointToTrendSurface(xutm-xutmoffset,yutm-yutmoffset);
         GISdb[CurDB].MyData.Next;
      end;
   end;

   {SOLVE SLE}
   SolveSLE(A^,B,IORD2,1.0E-08);

   {Initialize ERROR MEASURES }
   SY := 0.0;
   SYY:= 0.0;
   SYC:= 0.0;
   SYYC := 0.0;
   NumDataPoints := 0;

   if (MDDef.TrendSurfMap or MDDef.TrendMapDev) and ValidDEM(CurDEM) then begin
      StartProgress('Calculate trend surface ' + IntToStr(CurrentOrderTrendSurface));
      {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('Calc est val and dev for each obs');  {$EndIf}
      for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
         if (Col mod 25 = 0) then UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
         {$IfDef RecordAllTrendSurfaceProblems}
         WriteLineToDebugFile('Col=' + IntToStr(Col));
         {$EndIf}
         for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
            DEMGlb[CurDEM].DEMGridToUTM(Col,Row,xutm,yutm);

            z := CalculateTrendSurface(CurrentOrderTrendSurface,B,xutm-xutmoffset,yutm-yutmoffset);

            {$IfDef RecordAllTrendSurfaceProblems} WriteLineToDebugFile('x=' + RealToString(xutm,-18,2) +  '  y=' + RealToString(yutm,-18,2) + '  x=' + RealToString(z,-18,2) ); {$EndIf}
            if abs(z) > 32000 then z := MaxSmallInt;

            if (z < MaxSmallInt) and ((not DEMGlb[CurDEM].MissingDataIngrid(Col,Row)) or MDDef.TrendSurfaceOverVoids) then begin
               if MDDef.TrendSurfMap then DEMGlb[TrendDEM].SetGridElevation(Col,Row,z);
               if MDDef.TrendMapDev and (not DEMGlb[CurDEM].MissingDataInGrid(Col,Row)) and DEMGlb[CurDEM].GetElevMeters(Col,Row,zg)then
                  DEMGlb[DevDEM].SetGridElevation(Col,Row,zg-z);

               if (not DEMGlb[CurDEM].MissingDataInGrid(Col,Row)) then begin
                  inc(NumDataPoints);
                  z := z - DEMGlb[CurDEM].DEMheader.MinElev;
                  DEMGlb[CurDEM].GetElevMeters(Col,Row,zs);
                  zs := zs - DEMGlb[CurDEM].DEMheader.MinElev;
                  SY := SY + zs;
                  SYY := SYY + sqr(zs);
                  SYC := SYC + z;
                  SYYC := SYYC + sqr(z);
               end;
            end
            else begin
               if MDDef.TrendSurfMap then DEMGlb[TrendDEM].SetGridMissing(Col,Row);
               if MDDef.TrendMapDev then DEMGlb[DevDEM].SetGridMissing(Col,Row);
            end;

            if MDDef.TrendOpenMaps and MDDef.TrendSurfaceOverVoids and (DEMGlb[CurDEM].MissingDataIngrid(Col,Row)) then begin
               {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('interpolate   x=' + RealToString(xutm,-18,2) +  '  y=' + RealToString(yutm,-18,2) + '  x=' + RealToString(z,-18,2) ); {$EndIf}
               if DEMGlb[CurDEM].MissingDataInGrid(Col,Row) then DEMGlb[CurDEM].SetGridElevation(Col,Row,z)
               else DEMGlb[CurDEM].SetGridMissing(Col,Row);
            end;
         end {for Row};
      end {for Col};
      EndProgress;
   end;

   if (CurDB <> 0) then begin
      GISdb[CurDB].AddFieldToDataBase(ftFloat,'PLANE_DEV',8,2);
      GISdb[CurDB].AddFieldToDataBase(ftFloat,'TREND_Z',9,2);
      GISdb[CurDB].MyData.First;
      GISdb[CurDB].EmpSource.Enabled := false;
      StartProgress('Compute');
      i := 0;
      while not GISdb[CurDB].MyData.eof do begin
         inc(i);
         if (I mod 1000 = 0) then begin
            UpdateProgressBar(i/rc);
         end;
         zMeters := GISdb[CurDB].MyData.GetFieldByNameAsFloat(GISdb[CurDB].dbOpts.ZField);
         xutm := GISdb[CurDB].MyData.GetFieldByNameAsFloat(GISdb[CurDB].dbOpts.XField);
         yutm := GISdb[CurDB].MyData.GetFieldByNameAsFloat(GISdb[CurDB].dbOpts.YField);
         z := CalculateTrendSurface(CurrentOrderTrendSurface,B,xutm-xutmoffset,yutm-yutmoffset);
         GISdb[CurDB].MyData.Edit;
         GISdb[CurDB].MyData.SetFieldByNameAsFloat('PLANE_DEV',z-zMeters);
         GISdb[CurDB].MyData.SetFieldByNameAsFloat('TREND_Z',z);
         GISdb[CurDB].MyData.Next;
         inc(NumDataPoints);

         zs := z-zmeters;
         SY := SY + zs;
         SYY := SYY + sqr(zs);
         SYC := SYC + z;
         SYYC := SYYC + sqr(z);
      end;
      GISdb[CurDB].DBTablef.ShowStatus;
      {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('Complete DB compute'); {$EndIf}
   end;

   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('Calc errors'); {$EndIf}

    {compute error measures}
     SST := SYY - (SY * SY) / NumDataPoints;
     SSR := SYYC - (SYC * SYC) / NumDataPoints;
     SSD := SST - SSR;
     NDF1 := pred(IORD2);
     AMSR := SSR / NDF1;
     NDF2 := NumDataPoints - IORD2;
     AMSD := SSD / NDF2;
     R2 := SSR / SST;
     R := SQRT(R2);
     if (abs(AMSD) < 0.00001) then F := 9e99
     else F := AMSR / AMSD;
     NDF3 := pred(NumDataPoints);
     Dispose(A);

     if (CurrentOrderTrendSurface = 1) then begin
         DipAndStrikeFromThreePoints(1000,1000,CalculateTrendSurface(CurrentOrderTrendSurface,B,1000,1000),
                                     10000,1000,CalculateTrendSurface(CurrentOrderTrendSurface,B,10000,1000),
                                     1000,10000,CalculateTrendSurface(CurrentOrderTrendSurface,B,1000,10000),
                                     DipAndStrike,SlopeStr,Dip,Strike,DipDir);
     end;

    SkipMenuUpdating := false;

   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('Errored'); {$EndIf}
    if MDDef.TrendText then begin
       AddToOutputFile;
       CloseFile(ResultsFile);
       QuickOpenEditWindow(FName,'Statistics Order' + IntegerToString(CurrentOrderTrendSurface,2) + ' Trend Surface ');
       DeleteFileIfExists(FName);
    end;


   if (CurDEM <> 0) then begin
      if MDDef.TrendOpenMaps and MDDef.TrendMapDev then begin
         DEMGlb[DevDEM].SetUpMap(true,mtElevRainbow);
         DEMGlb[DevDEM].SelectionMap.MapDraw.ScaleMapElevationsToDEM;
         DEMGlb[DevDEM].SelectionMap.DoCompleteMapRedraw;
      end;

      if MDDef.TrendOpenMaps and MDDef.TrendSurfMap then begin
         DEMGlb[TrendDEM].SetUpMap(true,mtElevRainbow);
         DEMGlb[TrendDEM].SelectionMap.MapDraw.ScaleMapElevationsToDEM;
         DEMGlb[TrendDEM].SelectionMap.DoCompleteMapRedraw;
      end;

      if MDDef.TrendOpenMaps and MDDef.TrendDoGraph then begin
         StartProgress('Deviations');
         ThisGraph := tThisBaseGraph.Create(Application);
         ThisGraph.Caption := 'Elev vs' + IntegerToString(CurrentOrderTrendSurface,2) + ' Trend Surface';
         ThisGraph.GraphDraw.MaxHorizAxis := DEMGlb[CurDEM].DEMheader.MaxElev;
         ThisGraph.GraphDraw.MinHorizAxis := DEMGlb[CurDEM].DEMheader.MinElev;
         ThisGraph.GraphDraw.MaxVertAxis := DEMGlb[TrendDEM].DEMheader.MaxElev;
         ThisGraph.GraphDraw.MinVertAxis := DEMGlb[TrendDEM].DEMheader.MinElev;
         ThisGraph.GraphDraw.HorizLabel := DEMGlb[CurDEM].AreaName + ' DEM Elevs';
         ThisGraph.GraphDraw.VertLabel := 'Order ' + IntToStr(CurrentOrderTrendSurface) + ' Trend Surface';
         PadAxis(ThisGraph.GraphDraw.MinVertAxis,ThisGraph.GraphDraw.MaxVertAxis);
         PadAxis(ThisGraph.GraphDraw.MinHorizAxis,ThisGraph.GraphDraw.MaxHorizAxis);
         ThisGraph.SetUpGraphForm;
         ThisGraph.OpenPointSymbolFile(rfile,'trend',ThisGraph.MainSymbol);

         for Col := 0 to pred(DEMGlb[CurDEM].DEMheader.NumCol) do begin
            if (Col mod 25 = 0) then UpdateProgressBar(Col/DEMGlb[CurDEM].DEMheader.NumCol);
            for Row := 0 to pred(DEMGlb[CurDEM].DEMheader.NumRow) do begin
               if not DEMGlb[CurDEM].MissingDataInGrid(Col,Row) then with ThisGraph do begin
                  DEMGlb[CurDEM].GetElevMeters(Col,Row,v[1]);
                  DEMGlb[TrendDEM].GetElevMeters(Col,Row,v[2]);
                  BlockWrite(rfile,v,1);
               end {if};
            end {for row};
         end {for Col};
         closeFile(rfile);
         EndProgress;
         ThisGraph.RedrawDiagram11Click(Nil);
      end;
      if MDDef.TrendHistDev then SingleDEMHistogram(DevDEM);
   end;

   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('Trend Surface out'); {$EndIf}
end {procedure tTrendSurf.ComputeTrendSurface};


constructor tTrendSurf.Create;
begin
   inherited;
   CurDB := 0;
   CurDEM := 0;
   CurrentOrderTrendSurface := 1;
end;


destructor tTrendSurf.Destroy;
begin
   inherited;
end;

procedure tTrendSurf.SetDB(inDB: integer);
begin
   CurDB := inDB;
end;

procedure tTrendSurf.SetDEM(inDEM: integer; inLimits: tGridLimits);
begin
   CurDEM := inDEM;
   GridLimits := inLimits;
end;


initialization
finalization
   {$IfDef RecordTrendSurfaceProblems} WriteLineToDebugFile('RecordTrendSurfaceProblems active in demtrendopt'); {$EndIf}
   {$IfDef RecordAllTrendSurfaceProblems} WriteLineToDebugFile('RecordAllTrendSurfaceProblems active in demtrendopt'); {$EndIf}
end.



(*
procedure TrimTrendDeviations;
var
   CutDevs,Col,Row : integer;
   TrimClose : boolean;
begin
   CutDevs := -500;
   ReadDefault('Deviation from trend surface to cut',CutDevs);
   TrimClose := AnswerIsYes('Trim points with deviation from trend surface');
   StartProgress('Trim');
   with DEMGlb[InDEM],HeadRecs do begin
      for Col := 0 to pred(NumCol) do begin
         if (Col mod 25 = 0) then UpdateProgressBar(Col/NumCol);
         for Row := 0 to pred(NumRow) do if not DEMGlb[InDEM].MissingData(Col,Row) then begin
            if TrimClose then begin
               if abs(DEMGlb[DevDEM].GridElevMeters(Col,Row)) <= CutDevs then
                  DEMGlb[InDEM].SetGridMissing(Col,Row);
            end
            else begin
               if abs(DEMGlb[DevDEM].GridElevMeters(Col,Row)) >= CutDevs then
                  DEMGlb[InDEM].SetGridMissing(Col,Row);
            end;
         end;
      end;
      EndProgress;
      DEMGlb[InDEM].CheckMaxMinElev;
      DEMGlb[InDEM].SelectionMap.DrawColoredMap1Click(Nil);
   end;
end;
*)

