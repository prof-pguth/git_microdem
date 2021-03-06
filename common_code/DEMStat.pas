{$F+,O+}

unit DEMStat;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef Debug}
      {$Define NoParallelFor}
      {$Define RecordLag}
      //{$Define RecordStdDef}
      //{$Define RecordElevationSlopePlot}
      //{$Define RecordSSO}
      //{$Define RecordGlobalDEM}
      {$Define RecordElevMoment}
      //{$Define RecordElevationSlopePlotAll}
      {$Define RecordDEMCompare}
      //{$Define RecordStat}
      //{$Define RecordIceSat}
      //{$Define RecordGeoStat}
      //{$Define FullRecordBlockGeostats}
      //{$Define RecordPC}
      //{$Define RecordTraceCrests}
      //{$Define MapTraceCrests}
      //{$Define RecordDetailedTraceCrests}
      //{$Define RecordClustering}
      //{$Define RecordFFT}
      {$Define RecordHistogram}
      //{$Define RecordGridScatterGram}
   {$Else}
   {$EndIf}

{$EndIf}

{$IfDef ExGeoStats}
   {$Define ExSat}
{$EndIf}

interface

uses
//needed for inline of core DB functions
   Petmar_db,
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
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
//end core DB functions definitions

   SysUtils,Forms,Classes,Controls,Graphics,ExtCtrls,Math,StdCtrls,
   System.Threading,System.SyncObjs,System.UITypes,System.IOUtils,
   WinAPI.Windows,

   {$IfDef ExSat}
   {$Else}
     DEMEros,Multigrid,
   {$EndIf}
   DEMLosW,PETMAR,Petmar_types,BaseGraf,DEMDefs,DEMMapf;


const
   MaxPlotGlb = 2500;
type
   CountType = array[-5..2500] of integer;
   PlotArray = array[1..(MaxPlotGlb+2)] of float64;


  {$IfDef ExGeoStats}
  {$Else}
      procedure ElevMomentReport(aTitle : shortstring; Memo1 : tMemo; SamplingCheck : boolean; GridLimits: tGridLimits; CurDEM : integer = 0);
      procedure AspectDistributionBySlope(WhichDEM : Integer; GridLimits : tGridLimits);
      procedure AspectDistributionByAlgorithm(WhichDEM : Integer; GridLimits : tGridLimits);

      procedure StatsFromTwoGrids(iDEM,JDEM : integer; var r,covar : float64; Incr : integer = 1);
      procedure ElevationSlopePlot(WhichDEM : integer; DesiredBinSize : integer = 1);
      procedure DoAnSSODiagram(CurDEM : integer; GridLimits : tGridLimits);
      procedure GridRatio;
      procedure GridMinimum;
      procedure SlopeRegionSize(CurDEM : integer; DoRegionSize : boolean = true);
      procedure MakeElevSlopeAspectDifferenceMap;
      procedure PointSlopesByRegionSize(DEM : integer; RightClickLat,RightClickLong : float64);

      procedure SingleDEMHistogram(WhichDEM : integer; Quick : boolean = false);
      procedure DoElevationHistograms;

      procedure CalculateGrainProfile(MapForm : tMapForm; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64);
      function MakeDifferenceMap(Map1,Map2 : integer; ShowMap,ShowHistogram,ShowScatterPlot : boolean; TheAreaName : ShortString = '') : integer;
      function MakeDifferenceMapOfBoxRegion(Map1,Map2 : integer; GridLimits: tGridLimits; ShowMap,ShowHistogram,ShowScatterplot : boolean; TheAreaName : ShortString = '') : integer;
      function IwashuriPikeColor(Slope,Convexity,Texture : float64; var TerClass : integer) : tPlatformColor;  //inline;
      function IwashuriPikeCat(Slope,Convexity,Texture : float64) : integer; //inline;
      procedure IandPLegend(pc : array of float64);

       procedure ComputeDunecrestspacingheight(MapForm : tMapForm; GridLimits : tGridLimits; Memo1 : tMemo);
       procedure ThreadCrest1Click(MapForm : tMapForm; StartLat,StartLong : float64; var Results : tStringList; CrestNum : integer);
       procedure CrestsAlongProfile(theLOSView : TDEMLOSF; var Results : tStringList; Memo1 : tMemo = Nil);
       function ClusterGrids(StartingGrid,EndingGrid : integer) : integer;

       procedure SemiVariogramOptions;
       procedure SemiVariogram(DEMtoUse : integer; GridLimits: tGridLimits);
       procedure FastFourierTransform(WantedDEM : integer; GridLimits: tGridLimits; var SlopeByColumn,SlopeByRow : float32; CloseGraphs : boolean = false);

       procedure MakeGeomporphDBforPolygons(DBonTable : integer);
       procedure MakeGeomporphDBforPoints(DBonTable : integer);
       procedure NewBlockGeostats(WhatGeomorph : tGeomporphBlock; Table1 : tMyData;  WantedDEM : integer; GridLimits : tGridLimits);

      {$IfDef ExSat}
      {$Else}
         procedure ComputeVarCoVarAndPrincipalComponents(DB : integer; MG : tMultigridArray; UseFields : tStringList);
      {$EndIf}
  {$EndIf}

  {$IfDef ExCompare}
  {$Else}
      procedure GridDiffernces(EntireDEM : boolean);
      procedure MissingPointsInGrids(DEM1 : integer = 0; DEM2 : integer = 0);
      function GridScatterGram(FullGrid : boolean; DEM1 : integer = 0; DEM2 : integer = 0) : TThisBaseGraph;
      procedure GridCoOccurrence(AutoFull : boolean = false; DEM1 : integer = 0; DEM2 : integer = 0; Percentages : boolean = true);
      procedure DBCoOccurrence(Table : tMyData; EmpSource: TDataSource; Field1,Field2 : ShortString; Percentages : boolean);
  {$EndIf}

   function SumDEMs(FirstDEM : integer; Merge : tDEMbooleans; NewName : shortstring) : integer;
   function MakeChangeMap(Map1,Map2 : integer; GridLimits: tGridLimits) : integer;
   procedure GridCorrelationMatrix(Incr : integer = 1);

   procedure HistogramPointCloudAndGlobalDEMs(DB : integer = 0; Title : shortString = '');
   procedure DirtAndAirShots(DB : integer; Title : shortString);
   procedure CloudSummaryGlobalDEMs(DB : integer);
   function FiveSeriesGraph(DB : integer; Lat,Long,Tolerance : float64; DirField : shortstring) : TThisbasegraph;
   procedure IcesatProcessCanopy(dbOnTable : integer; AddDEMs : boolean);
   procedure IcesatPhotonConvert(Memo1 : tMemo);
   procedure AddGlobalDEMs(dbOnTable : integer);
   procedure ElevationSlopePlotCompareDEMs;
   procedure LandCoverSummary;
   procedure AddEGMfields(dbOnTable : integer);
   procedure GridsByRegionSize(CurDEM : integer; GridCh : char);

   procedure AllAspects;

   function FindPeaks(DEM : integer; GridLimits : tGridLimits; var PeakResults : tStringList; Memo1 : tMemo) : integer;
   function FindPits(DEM : integer; GridLimits : tGridLimits; var PitResults : tStringList; Memo1 : tMemo) : integer;

   procedure OneLag(MainDEM,SubDEM : integer; BoxLimits: tGridLimits; var BigResults : tStringList);
   procedure Lag_and_Shift(ColC,RowC : integer; MainDEM,SubDEM : integer; GridLimits : tGridLimits; var NPts,XWhereMaxR,YWhereMaxR : integer; var MaxR,NoLagR,ZRange,AvgSlope,BestA,BestB : float64; CorrelationMatrix : tStringList = Nil);

var
   IPColor : array[1..16] of Graphics.tColor;
   ConvexityMeanCut,TextureMeanCut,
   SlopeMeanCut,SlopeQuarterCut,SlopeEigthCut : float64;
   VariableWeightings : array[1..EdburgMaxVariables] of float64;

implementation

uses
   Nevadia_Main,

   {$IfDef ExGeoStats}
   {$Else}
      PETFouri,
      DEMVarOp,
      Slope_Graph_Opts,
      PC_options,
      MVClusterClientDataSet,
      ClusterOptions,
      Moment_Opts,
      CrossCor,
      DEM_Hist_Opts,
      DEM_grid_diffs,
   {$EndIf}
   {$IfDef ExStereoNet}
   {$Else}
      NetMainW,
   {$EndIf}
   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}

   DEMCoord,DEMOptions,
   PetDBUtils,
   GetLatLn,
   Thread_Timers,
   Make_tables,
   DEM_Indexes,
   DEM_Manager,
   DEMStringGrid,
   Petimage_form,
   DEMDef_routines,
   PetImage,
   Make_grid,
   las_lidar,
   basemap,
   DEM_NLCD,
   icesat_filter_form,
   PETMath;

const
   InsuffDEMrelief = 'Insufficient DEM relief';
   ZRangeTooLarge = 'Elev range too large';
type
   PlotModeType = (Plain,Strahler,Cumulative,NormProb);
var
   Count   : array[1..MaxDEMDataSets] of ^CountType;


{$I demstat_global_dems.inc}

{$I demstat_dune_crest.inc}

{$IfDef ExCompare}
{$Else}
   {$I demstat_grid_compare.inc}
{$EndIf}

var
   NumDone,NumToDo : integer;


procedure Lag_and_Shift(ColC,RowC : integer; MainDEM,SubDEM : integer; GridLimits : tGridLimits; var NPts,XWhereMaxR,YWhereMaxR : integer; var MaxR,NoLagR,ZRange,AvgSlope,BestA,BestB : float64; CorrelationMatrix : tStringList = Nil);
var
   x,y : ^bfarray32;
   xs,ys,Col,Row : integer;
   Lat,Long : float64;
   zMin,zMax, a,b,siga,sigb,r,xg,yg : float32;
   z : array[1..2] of float32;
   MomentVar : tMomentVar;
begin
   {$IfDef RecordLagProblems} WriteLineToDebugFile('Lag_and_Shift in'); {$EndIf}
   New(x);
   New(y);
   MaxR := -1;
   zMin := 99999;
   zMax := -99999;
   if (CorrelationMatrix <> Nil) then CorrelationMatrix.Add('LAT,LONG,XSHIFT,YSHIFT,R2,INTERCEPT,SLOPE');

   for xs := MDDef.ShiftLoX to MDDef.ShiftHighX do begin
      for ys := MDDef.ShiftLoY to MDDef.ShiftHighY do begin
         NPts := 0;
         Col := GridLimits.XGridLow;
         while (Col <= GridLimits.XGridHigh) do begin
            Row := GridLimits.YGridLow;
            while (Row <= GridLimits.YGridHigh) do begin
               if DEMGLB[MainDEM].GetElevMetersOnGrid(Col,Row,z[1]) then begin
                  CompareValueToExtremes(z[1],zMin,zMax);
                  DEMGLB[MainDEM].DEMGridToLatLongDegree(Col+xs,Row+ys,Lat,Long);
                  DEMGLB[SubDEM].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
                  if DEMGLB[SubDEM].GetElevMeters(xg,yg,z[2]) then begin
                     x^[NPts] := z[1];
                     y^[NPts] := z[2];
                     inc(NPts);
                  end;
               end;
               inc(Row);
            end;
            inc(Col);
         end;
         if (Npts > 25) then begin
            fit(x^,y^,NPts,a,b,siga,sigb,r);
            r := r*r;
            if (R > MaxR) then begin
               MaxR := R;
               XWhereMaxR := xs;
               YWhereMaxR := ys;
               BestA := a;
               BestB := b;
            end;
            if (CorrelationMatrix <> Nil) then begin
               DEMGLB[MainDEM].DEMGridToLatLongDegree(ColC+xs,RowC+ys,Lat,Long);
               CorrelationMatrix.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7) + ',' + IntToStr(xs) + ',' + IntToStr(ys) + ',' + RealToString(r,-12,-5) );
            end;
            {$IfDef RecordFullLagProblems} WriteLineToDebugFile(IntToStr(xs) + ',' + IntToStr(ys) + ',' + RealToString(r,-8,-4) + ',' + RealToString(a,-8,-4) + ',' + RealToString(ba,-8,-4));  {$EndIf}
         end;
        if (xs = 0) and (ys = 0) then NoLagR := R;
      end;
   end;
   DEMGlb[MainDEM].SlopeMoments(GridLimits,MomentVar);
   AvgSlope := MomentVar.mean;
   zRange := zmax - zmin;
   Dispose(x);
   Dispose(y);
   ShowDefaultCursor;
   {$IfDef RecordLagProblems} WriteLineToDebugFile('Leaving routine, XWhereMaxR=' + IntToStr(XWhereMaxR) + ' YWhereMaxR=' + IntToStr(YWhereMaxR)); {$EndIf}
end;


procedure DoLagStrip(MainDEM,SubDEM : integer; BoxLimits : tGridLimits; Results : tStringList);
var
   x,y,xlag,ylag,NPts,Buffer : integer;
   Lat,Long,Missing,MaxR,NoLagR,zrange,AvgSlope,xd,yd,heading,BestA,BestB : float64;
   GridLimits : tGridLimits;
begin
     {$If Defined(NoParallelFor)}
        {$IfDef RecordLagProblems} WriteLineToDebugFile('DoStrip in'); {$EndIf}
        StartProgress('Lag strip ' + DEMGlb[SubDEM].AreaName);
     {$EndIf}
     Buffer := MDDef.LagSearchRadius;
     Buffer := 0;
     x := BoxLimits.XGridLow + Buffer;
     while x <= BoxLimits.XGridHigh - Buffer do begin
        {$If Defined(NoParallelFor)}
          UpdateProgressBar( (x-BoxLimits.XGridLow) / (BoxLimits.XGridHigh - BoxLimits.XGridLow));
        {$EndIf}
        y := BoxLimits.YGridLow + Buffer;
        while y <= BoxLimits.YGridHigh - Buffer do begin
           GridLimits.XGridLow := x - MDDef.LagSearchRadius;
           GridLimits.XGridHigh := x + MDDef.LagSearchRadius;
           GridLimits.YGridLow := y - MDDef.LagSearchRadius;
           GridLimits.YGridHigh := y + MDDef.LagSearchRadius;
           DEMGlb[MainDEM].ComputeMissingData(GridLimits,Missing);
           if (Missing < 25) then begin
              Lag_and_Shift(0,0,MainDEM,SubDEM,GridLimits,NPts,xLag,YLag,MaxR,NoLagR,zrange,AvgSlope,BestA,BestB);
              DEMGlb[MainDEM].DEMGridToLatLongDegree(x,y,Lat,Long);
              xd := xLag*DEMGlb[MainDEM].AverageXSpace;
              yd := yLag*DEMGlb[MainDEM].AverageYSpace;
              heading := HeadingOfLine(xd,yd);
              Results.Add(DEMGlb[SubDEM].AreaName + ',' + IntToStr(Npts) + ',' + RealToString(Lat,-18,-8) + ',' + RealToString(Long,-18,-8) + ',' +
                 RealToString(xd,-12,-2) + ',' + RealToString(yd,-12,-2) + ',' + RealToString(sqrt(sqr(xd) + sqr(yd)),-8,2)+ ',' + ',' + RealToString(Heading,-12,-2) + ',' +
                 RealToString(MaxR,-8,4) + ',' + IntToStr(xLag) + ',' + IntToStr(yLag) + ',' + RealToString(sqrt(sqr(xlag) + sqr(yLag)),-8,2)+ ',' + RealToString(NoLagR,-8,4) + ',' +
                 RealToString(zRange,-8,2 ) + ',' + RealToString(AvgSlope,-8,2) + ',' + RealToString(MaxR-NoLagR,-8,4) + ',' + RealToString(BestA,-8,4) + ',' + RealToString(BestB,-8,4));
           end;
           inc(y,MDDef.LagCenterShift);
        end;
        inc(x,MDDef.LagCenterShift);
     end;
     {$IfDef VCL}
        {$If Defined(NoParallelFor)}
           EndProgress;
        {$Else}
           TInterlocked.Increment(NumDone);
           ThreadTimers.UpdateThreadStats(9, round(100 * NumDone /NumToDo));
        {$EndIf}
     {$EndIf}
end;


procedure OneLag(MainDEM,SubDEM : integer; BoxLimits: tGridLimits; var BigResults : tStringList);
const
   fs = 'DEM,NPTS,LAT,LONG,X_SHIFT_M,Y_SHIFT_M,SHIFT_M,SHIFT_DIR,R,X_LAG,Y_LAG,TOTAL_LAG,NO_LAG_R,RELIEF_M,AVG_SLOPE,R_IMPROVE,BEST_A,BEST_B';
var
  i,it,nt,NumStrips : integer;
  Results : array[1..MaxThreadsAllowed] of tStringList;
  StripBoxLimits: tGridLimits;
begin
   {$IfDef RecordLag} WriteLineToDebugFile('One Lag in, maindem=' + IntToStr(MainDEM) + ' subdem=' + IntToStr(SubDEM)); {$EndIf}
   i := 1;
   ShowSatProgress := false;

   {$If Defined(NoParallelFor)}
      Results[1] := tStringList.Create;
      nt := 1;
      DoLagStrip(MainDEM,SubDEM,BoxLimits,Results[1]);
   {$Else}
      StartSingleThreadTimer('Shifting ' + DEMGlb[SubDEM].AreaName);
      NumToDo := succ( (BoxLimits.XGridHigh-BoxLimits.XGridLow) div MDDef.LagCenterShift);
      NumDone := 0;
      for i := 1 to MDdef.MaxThreadsForPC do begin
         Results[i] := tStringList.Create;
      end;
      NumStrips := succ( NumToDo div MDdef.MaxThreadsForPC);
      nt := MDdef.MaxThreadsForPC;
      TParallel.For(1, MDdef.MaxThreadsForPC,
          procedure (Value: Integer)
          begin
             StripBoxLimits := BoxLimits;
             StripBoxLimits.XGridLow  := BoxLimits.XGridLow + pred(Value) * NumStrips * MDDef.LagCenterShift;
             StripBoxLimits.XGridHigh := StripBoxLimits.XGridLow + pred(NumStrips * MDDef.LagCenterShift);
             DoLagStrip(MainDEM,SubDEM,StripBoxLimits,Results[Value]);
          end);
     EndThreadTimers;
  {$EndIf}
  {$IfDef RecordLag} WriteLineToDebugFile('One Lag strips done'); {$EndIf}
  ShowSatProgress := true;
  if (BigResults = Nil) then begin
    BigResults := tStringList.Create;
    BigResults.Add(fs);
    {$IfDef RecordLag} WriteLineToDebugFile('One Lag big results started'); {$EndIf}
  end;

  for it := 1 to nt do begin
     if Results[it].Count > 0 then begin
        for I := 0 to pred(Results[it].Count) do
           BigResults.Add(Results[it].Strings[i]);
     end;
     Results[it].Free;
  end;
  {$IfDef RecordLag} WriteLineToDebugFile('One Lag out'); {$EndIf}
end;




function FindPits(DEM : integer; GridLimits : tGridLimits; var PitResults : tStringList; Memo1 : tMemo) : integer;
label
   NotPit;
var
   x,y,NumPit,Col,Row,db,dx,dy, xp,yp : integer;
   Lat,Long : float64;
   MaxZ,MinZ,z : float32;
begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('FindPits in'); {$EndIf}
   dx := round(MDDef.PitRadius / DEMGlb[DEM].AverageXSpace);
   dy := round(MDDef.PitRadius / DEMGlb[DEM].AverageYSpace);
   NumPit := 0;
  {$IfDef RecordPitsSpires} WriteLineToDebugFile('Start search spires/pits'); {$EndIf}
  for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
     for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
       if DEMGlb[DEM].GetElevMeters(Col,Row,MinZ) then begin
          MaxZ := MinZ;
          for x := Col-dx to Col+dx do begin
             for y := Row-dy to Row+dy do begin
                if (x <> 0) and (y <> 0) then begin
                   if DEMGlb[DEM].GetElevMeters(x,y,z) then begin
                      if z < MinZ then goto NotPit;
                      if z > MaxZ then MaxZ := z;
                   end
                   else GoTo NotPit;
                end;
             end;
          end;
          if ((MaxZ-MinZ) >= MdDef.PitHeight) then begin
             if (PitResults <> Nil) then begin
                DEMGlb[DEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                PitResults.Add(DEMGlb[DEM].AreaName + ',' + RealToString(Lat,-18,8) + ',' + RealToString(Long,-18,8) + ',' + RealToString(MinZ,-18,-2)+ ',' + RealToString(MaxZ-MinZ,-18,-2));
             end;
             inc(NumPit);
          end;
      end;
      NotPit:;
     end {while row};
     if WantOut then break;
  end {while col};
  if (Memo1 <> nil) then Memo1.Lines.Add(DEMGlb[DEM].AreaName + ' Pits in DEM: ' + IntToStr(NumPit));
  Result := NumPit;
  {$IfDef RecordPitsSpires} WriteLineToDebugFile('FindPits out'); {$EndIf}
end;



function FindPeaks(DEM : integer; GridLimits : tGridLimits; var PeakResults : tStringList; Memo1 : tMemo) : integer;
label
   NotPeak;
var
   NumPeak,
   Col,Row,x,y,
   dx,dy : integer;
   z,MinZ,MaxZ : float32;
   Lat,Long : float64;
begin
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('FindPeaks in'); {$EndIf}
   with DEMGlb[DEM] do begin
      dx := round(MDDef.PeakRadius / AverageXSpace);
      dy := round(MDDef.PeakRadius / AverageYSpace);
      NumPeak := 0;
      for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
         for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
             if DEMGlb[DEM].GetElevMeters(Col,Row,MaxZ) then begin
                MinZ := MaxZ;
                for x := Col-dx to Col+dx do begin
                   for y := Row-dy to Row+dy do begin
                      if (x <> 0) and (y <> 0) then begin
                         if GetElevMeters(x,y,z) then begin
                            if z > MaxZ then goto NotPeak;
                            if z < MinZ then MinZ := z;
                         end;
                      end;
                   end;
                end;
                if ((MaxZ-MinZ) >= MdDef.PeakHeight) then begin
                   if PeakResults <> nil then begin
                      DEMGridToLatLongDegree(Col,Row,Lat,Long);
                      PeakResults.Add(DEMGlb[DEM].AreaName + ',' + RealToString(Lat,-18,8) + ',' + RealToString(Long,-18,8) + ',' + RealToString(MaxZ,-18,-2)+ ',' + RealToString(MaxZ-MinZ,-18,-2));
                   end;
                   inc(NumPeak);
                end;
             end;
           NotPeak:;
         end {while row};
      end {while col};

      if (Memo1 <> nil) then begin
         Memo1.Lines.Add('');
         Memo1.Lines.Add(DEMGlb[DEM].AreaName);
         Memo1.Lines.Add('Peaks:  ' + IntToStr(NumPeak));
      end;
   end;
   Result := NumPeak;
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('FindPeaks out'); {$EndIf}
end;




procedure AllAspects;
 var
    rg : tThisBaseGraph;
    fName : PathStr;
    BMPlist : tStringList;
    DEM : integer;
 begin
    BMPlist := tStringList.Create;
    for DEM := 1 to MaxDEMDataSets do begin
       if ValidDEM(DEM) then begin
           if DEMGlb[DEM].DEMHeader.ElevUnits = AspectDeg then begin
              rg := CreateAspectRose(DEM);
           end;
       end;
    end;

   if (BMPlist.Count > 0) then begin
      MakeBigBitmap(BMPlist,'Aspect roses');
   end
   else BMPlist.Free;
end;


procedure PointSlopesByRegionSize(DEM : integer; RightClickLat,RightClickLong : float64);
var
   xDEMg1,yDEMg1 : float64;
   Findings : tStringList;
   TStr,location : shortString;
   i,db : integer;
   graph : tThisBaseGraph;

         procedure MakeSlopeMap(BoxSize : integer);
         var
            SlopeAsp : tSlopeAspectRec;
         begin
            MDDef.SlopeRegionRadius := BoxSize;
            if DEMGlb[DEM].GetSlopeAndAspect(round(xDEMg1),round(yDEMg1),SlopeAsp) then begin
               Findings.Add(Location + ',' + IntToStr(BoxSize) + ',' + RealToString(BoxSize* DEMGlb[DEM].AverageYSpace,-12,-2) + ',' +
                     RealToString(SlopeAsp.SlopePercent,-12,2) + ',' + RealToString(SlopeAsp.SlopeDegree,-12,2) + ',' + RealToString(SlopeAsp.AspectDir,-8,2));
            end;
         end;

begin
   DEMDef_routines.SaveBackupDefaults;
   Findings := tStringList.Create;
   Location := LatLongDegreeToString(RightClickLat,RightClickLong,MDDef.OutPutLatLongMethod);
   Findings.Add('Location,Region,Dist_m,Slope_pc,Slope_deg,Aspect_deg');
   DEMGlb[DEM].LatLongDegreeToDEMGrid(RightClickLat,RightClickLong,xDEMg1,yDEMg1);
   for i := 1 to 25 do MakeSlopeMap(i);
   TStr := NextFileNumber(MDTempDir,DEMGlb[DEM].AreaName + '_Slope_by_region_','.dbf');
   StringList2CSVtoDB(Findings,TStr);
   Graph := GISDB[db].CreateScatterGram('DIST_M','SLOPE_PC',true,'Slopes by region at ' + Location);
   Graph.GraphDraw.MinVertAxis := 0;
   Graph.LLCornerText := Location;
   Graph.RedrawDiagram11Click(Nil);
   DEMDef_routines.RestoreBackupDefaults;
end;


procedure SlopeRegionSize(CurDEM : integer; DoRegionSize : boolean = true);
var
   LegendFiles,ElevFiles : tstringList;
   GridForm : TGridForm;
   Method,RegionsDone : integer;
   MaxSlope : float64;

       procedure MakeSlopeMap(BoxSize : integer);
       var
          zvs : ^Petmath.bfarray32;
          MomentVar : tMomentVar;
       begin
          inc(RegionsDone);
          MDDef.SlopeRegionRadius := BoxSize;
          New(zvs);
          DEMGlb[CurDEM].SlopeMomentsWithArray(DEMGlb[CurDEM].FullDEMGridLimits, MomentVar,zvs^);
          ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
          Dispose(zvs);
          MomentsToStringGrid(GridForm.StringGrid1, 1,RegionsDone,'Slope', MomentVar);
          if (MomentVar.MaxZ > MaxSlope) then MaxSlope := MomentVar.MaxZ;    //for histogram
          if DoRegionSize then begin
             GridForm.StringGrid1.Cells[RegionsDone,0] := 'Region ' + IntToStr(BoxSize) + ' (' + DEMGlb[CurDEM].SimpleHorizontalDEMSpacing(BoxSize) + ')';
             LegendFiles.Add(GridForm.StringGrid1.Cells[RegionsDone,0]);
          end
          else begin
             GridForm.StringGrid1.Cells[RegionsDone,0] := ShortSlopeMethodName(MDdef.SlopeAlg);
             LegendFiles.Add(ShortSlopeMethodName(MDdef.SlopeAlg));
          end;
       end;

begin
   DEMDef_routines.SaveBackupDefaults;   //save slope algorith
   ElevFiles := tStringList.Create;
   LegendFiles := tStringList.Create;

   GridForm := TGridForm.Create(Application);
   GridForm.StringGrid1.RowCount := 10;
   GridForm.Caption := 'Slope Region Size Moment Report';
   GridForm.StringGrid1.Cells[0,0] := DEMGlb[CurDEM].AreaName;
   RegionsDone := 0;
   MaxSlope := 0;

   if DoRegionSize then begin
      MakeSlopeMap(1);
      MakeSlopeMap(2);
      MakeSlopeMap(3);
      MakeSlopeMap(4);
      MakeSlopeMap(5);
      MakeSlopeMap(7);
      MakeSlopeMap(10);
      MakeSlopeMap(15);
      MakeSlopeMap(20);
   end
   else begin
      for Method:= FirstSlopeMethod to LastSlopeMethod do begin
         MDdef.SlopeAlg := Method;
         MakeSlopeMap(0);
      end;
   end;
   CreateMultipleHistograms(MDDef.CountHistograms,ElevFiles,LegendFiles,'Slope','Slope distribution',-99,0,Trunc(MaxSlope + 0.99),0.5);

   GridForm.StringGrid1.ColCount:= succ(RegionsDone);
   DEMDef_routines.RestoreBackupDefaults;  //restore slope algorithm
end;



procedure GridsByRegionSize(CurDEM : integer; GridCh : char);
begin
   var i : integer;
   for i := 1 to 5 do Make_grid.MakeMomentsGrid(CurDEM,GridCh,MDDef.BatchRegionSize[i]);
end;


procedure GetBins(MaxMax,MinMin : integer; var BinSize,StartIndex : integer);
var
   ElevRange : integer;
begin
   ElevRange := MaxMax - MinMin;
   if (ElevRange < 100) then BinSize := 1
   else if (ElevRange < 200) then BinSize := 2
   else if (ElevRange < 500) then BinSize := 5
   else if (ElevRange < 1000) then BinSize := 10
   else if (ElevRange < 2000) then BinSize := 20
   else if (ElevRange < 3000) then BinSize := 25
   else if (ElevRange < 5000) then BinSize := 50
   else if (ElevRange < 10000) then BinSize := 100
   else BinSize := 200;
   StartIndex := MinMin div BinSize;
end;


{$IfDef ExGeostats}
{$Else}

procedure MakeGeomporphDBforPoints(DBonTable : integer);
var
   dbName : PathStr;
   dbTable : tMyData;
   GridLimits : tGridLimits;
   Lat,Long : float64;
   xsize,ysize,xg,yg,
   i,rc : integer;
   TStr : shortString;
begin
   {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPoints in'); {$EndIf}
   if not (GISdb[DBonTable].DEMwithDBsMap) then begin
      MessageToContinue('Operation must be on a map with a DEM');
   end
   else begin
      GetGeomorphBlockOpts(gbPointNeighborhood,GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,GridLimits);

      dbName := ChangeFileExt(GISdb[DBonTable].dbFullName,'_geomorph' + DefaultDBExt);
      Make_tables.CreateGeomorphometryAttributesTable(dbname,false);
      dbTable := tMyData.Create(dbName);

      DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].GetBoxGridSizeDiameter(MDDef.GeomorphBoxSizeMeters,XSize,Ysize,TStr);
      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].AddSequentialIndex(RecNoFName,true);
      {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPoints prelims over'); {$EndIf}
      StartThreadTimers('Geomorph stats',0,true);
      rc := GISdb[DBonTable].MyData.TotRecsInDB;
      for i := 1 to rc do begin
         {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPoints Rec=' + IntToStr(i) + '/' + IntToStr(rc)); {$EndIf}
         ThreadTimers.OverallGauge9.Progress := round(100 * i/rc);
         if GISdb[DBonTable].MyData.ValidLatLongFromTable(Lat,Long) then begin
            DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
            GridLimits.XGridLow := xg - Xsize div 2;
            GridLimits.XGridHigh := xg + Xsize div 2;
            GridLimits.YGridLow := yg - Ysize div 2;
            GridLimits.YGridHigh := yg + Ysize div 2;
            DEMStat.NewBlockGeoStats(gbPointNeighborhood,dbTable,GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,GridLimits);
            {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPoints stats ' + GridLimitsToString(GridLimits)); {$EndIf}
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      GISdb[DBonTable].TheMapOwner.OpenDBonMap('',dbName);
      GISdb[DBonTable].ShowStatus;
      EndThreadTimers;
   end;
   {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPoints out'); {$EndIf}
end;



procedure NewBlockGeostats(WhatGeomorph : tGeomporphBlock; Table1 : tMyData;  WantedDEM : integer; GridLimits : tGridLimits);
var
   StartX,StartY,XBoxGridSize,YBoxGridSize,
   XBoxes,YBoxes,x,y : integer;
   GammaEW,GammaNS,GammaNESW,GammaNWSE : float32;
   UpMoment,DownMoment : tMomentVar;
   Lat,Long : float64;
   NetForm : NetMainw.tNetForm;
   TStr : shortstring;
   ThisGraph : BaseGraf.TThisBaseGraph;
   MomentVar : tMomentVar;
   GoodFabric : boolean;

        procedure ComputeStats;
        var
           eLat1,eLong1,eLat2,eLong2,z,zl,MissingPC,Distance,Bearing : float64;
           xloc,yloc : integer;
           SSOvars : tSSOvars;
           SSOGridLimits : tGridLimits;
        begin
           {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats in'); {$EndIf}
           if DEMGlb[WantedDEM].FilledGridBox(GridLimits) then begin
              DEMGlb[WantedDEM].DEMGridToLatLongDegree((GridLimits.XGridLow+GridLimits.XGridHigh) div 2,(GridLimits.YGridLow+GridLimits.YGridHigh) div 2, Lat,Long);
              MomentVar := DEMGlb[WantedDEM].ElevationMoments(GridLimits);
              Table1.Insert;

              if Table1.FieldExists('NAME') then Table1.SetFieldByNameAsString('NAME',DEMGlb[WantedDEM].AreaName);
              if Table1.FieldExists('AREA') then Table1.SetFieldByNameAsString('AREA',LastSubDir(DEMGlb[WantedDEM].DEMFileName));
              Table1.SetFieldByNameAsFloat('LAT',Lat);
              Table1.SetFieldByNameAsFloat('LONG',Long);
              Table1.SetFieldByNameAsFloat(RecNoFName,succ(Table1.RecordCount));
              Table1.SetFieldByNameAsInteger('NPTS',MomentVar.Npts);

              if EnoughPoints(MomentVar) and (MomentVar.MaxZ - MomentVar.MinZ > 0) then begin
                 {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats got good elevation moments'); {$EndIf}
                 if MDDef.IncludeBasicElevation then begin
                   Table1.SetFieldByNameAsFloat('ELEV_AVG',MomentVar.mean);
                   Table1.CarefullySetFloat('ELEV_STD',MomentVar.sdev,0.001);
                   Table1.CarefullySetFloat('ELEV_SKW',MomentVar.skew,0.001);
                   Table1.CarefullySetFloat('ELEV_KRT',MomentVar.curt,0.001);
                   {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished basic elevation'); {$EndIf}
                 end;

                 if MDDef.IncludeAdvancedElevation then begin
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats Start advanced elevation'); {$EndIf}
                    Table1.SetFieldByNameAsFloat('RELIEF',MomentVar.MaxZ - MomentVar.MinZ);
                    z := (MomentVar.mean-MomentVar.MinZ) / (MomentVar.MaxZ - MomentVar.MinZ);
                    Table1.CarefullySetFloat('ELEV_RELF',z,0.001);

                    if DEMGlb[WantedDEM].FindLocationOfMaximum(GridLimits,xloc,yloc,z) then begin
                       DEMGlb[WantedDEM].DEMGridToLatLongDegree(xloc,yloc,elat1,elong1);
                       Table1.SetFieldByNameAsFloat('LAT_ZMAX',eLat1);
                       Table1.SetFieldByNameAsFloat('LONG_ZMAX',eLong1);
                    end;

                    Table1.SetFieldByNameAsFloat('ELEV_MAX',MomentVar.MaxZ);

                    if DEMGlb[WantedDEM].FindLocationOfMinimum(GridLimits,xloc,yloc,zl) then begin
                       {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats start extremes'); {$EndIf}
                       DEMGlb[WantedDEM].DEMGridToLatLongDegree(xloc,yloc,elat2,elong2);
                       Table1.SetFieldByNameAsFloat('LAT_ZMIN',eLat2);
                       Table1.SetFieldByNameAsFloat('LONG_ZMIN',eLong2);
                       Table1.SetFieldByNameAsFloat('ELEV_MIN',zl);

                       VincentyCalculateDistanceBearing(eLat1,eLong1,eLat2,eLong2,Distance,Bearing);
                       Table1.SetFieldByNameAsFloat('DIST_DROP',0.001 * Distance);
                       Table1.SetFieldByNameAsFloat('AZ_DROP',Bearing);
                       Table1.SetFieldByNameAsFloat('SLOPE_DROP',100 * (z-zl) / Distance);
                    end;
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished advanced elevation'); {$EndIf}
                 end;

                 if MDDef.IncludeSlopeMeasures then begin
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats start slope'); {$EndIf}
                     DEMGlb[WantedDEM].SlopeMoments(GridLimits,MomentVar);
                     if MomentVar.NPts > 1 then begin
                        Table1.CarefullySetFloat('SLOPE_AVG',MomentVar.mean,0.001);
                        Table1.CarefullySetFloat('SLOPE_STD',MomentVar.sdev,0.001);
                        Table1.CarefullySetFloat('SLOPE_SKW',MomentVar.skew,0.001);
                        Table1.CarefullySetFloat('SLOPE_KRT',MomentVar.curt,0.001);
                        Table1.CarefullySetFloat('SLOPE_MAX',MomentVar.MaxZ,0.001);
                     end;
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished slope'); {$EndIf}
                 end;

                 if MDDef.IncludePlanCMeasures then begin
                      DEMGlb[WantedDEM].PlanCMoments(GridLimits,MomentVar);
                      if MomentVar.NPts > 1 then begin
                         Table1.CarefullySetFloat('PLANC_AVG',MomentVar.mean,0.000001);
                         Table1.CarefullySetFloat('PLANC_STD',MomentVar.sdev,0.000001);
                         Table1.CarefullySetFloat('PLANC_SKW',MomentVar.skew,0.000001);
                         Table1.CarefullySetFloat('PLANC_KRT',MomentVar.curt,0.000001);
                      end;
                     {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished plan curvature'); {$EndIf}
                 end;

                 if MDDef.IncludeProfCMeasures then begin
                     DEMGlb[WantedDEM].ProfCMoments(GridLimits,MomentVar);
                     if MomentVar.NPts > 1 then begin
                        Table1.CarefullySetFloat('PROFC_AVG',MomentVar.mean,0.000001);
                        Table1.CarefullySetFloat('PROFC_STD',MomentVar.sdev,0.000001);
                        Table1.CarefullySetFloat('PROFC_SKW',MomentVar.skew,0.000001);
                        Table1.CarefullySetFloat('PROFC_KRT',MomentVar.curt,0.000001);
                     end;
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished profile curvature'); {$EndIf}
                 end;

                 if MDDef.IncludeOpenness then begin
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats start openness'); {$EndIf}
                     DEMGlb[WantedDEM].BothOpennessMoments(GridLimits,UpMoment,DownMoment);
                     if (UpMoment.NPts > 0) then begin
                        Table1.SetFieldByNameAsFloat('OPN_UP_AVG',UpMoment.mean);
                        Table1.SetFieldByNameAsFloat('OPN_UP_STD',UpMoment.sdev);
                        Table1.SetFieldByNameAsFloat('OPN_UP_SKW',UpMoment.Skew);
                        Table1.SetFieldByNameAsFloat('OPN_UP_KRT',UpMoment.Curt);
                        Table1.SetFieldByNameAsFloat('OPN_DN_AVG',DownMoment.mean);
                        Table1.SetFieldByNameAsFloat('OPN_DN_STD',DownMoment.sdev);
                        Table1.SetFieldByNameAsFloat('OPN_DN_SKW',DownMoment.Skew);
                        Table1.SetFieldByNameAsFloat('OPN_DN_KRT',DownMoment.curt);
                     end;
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats end openness'); {$EndIf}
                 end;

                  {$IfDef ExComplexGeostats}
                  {$Else}
                     if MDDef.IncludeGammaMeasures then begin
                         DEMGlb[WantedDEM].VariogramGamma(GridLimits,GammaEW,GammaNS,GammaNESW,GammaNWSE);
                         Table1.CarefullySetFloat32('GAMMA_EW',GammaEW,0.0001);
                         Table1.CarefullySetFloat32('GAMMA_NS',GammaNS,0.0001);
                         Table1.CarefullySetFloat32('GAMMA_NESW',GammaNESW,0.0001);
                         Table1.CarefullySetFloat32('GAMMA_NWSE',GammaNWSE,0.0001);
                     end;
                  {$EndIf}

                 if (MDDef.IncludeFabricMeasures) then begin
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats start SSO'); {$EndIf}
                    NetForm := Nil;
                    ThisGraph := Nil;
                    if (WhatGeomorph in [gbPolygon,gbMapOnly]) then begin
                       SSOGridLimits := GridLimits;
                    end
                    else begin
                       SSOGridLimits.XGridLow := (GridLimits.XGridLow+GridLimits.XGridHigh) div 2 - round(MDDef.SSOBoxSizeMeters /DEMGLB[WantedDEM].AverageXSpace);
                       SSOGridLimits.XGridHigh := (GridLimits.XGridLow+GridLimits.XGridHigh) div 2 + round(MDDef.SSOBoxSizeMeters /DEMGLB[WantedDEM].AverageXSpace);
                       SSOGridLimits.YGridLow := (GridLimits.YGridLow+GridLimits.YGridHigh) div 2 - round(MDDef.SSOBoxSizeMeters /DEMGLB[WantedDEM].AverageYSpace);
                       SSOGridLimits.YGridHigh := (GridLimits.YGridLow+GridLimits.YGridHigh) div 2 + round(MDDef.SSOBoxSizeMeters /DEMGLB[WantedDEM].AverageYSpace);
                    end;

                    GoodFabric := DEMGlb[WantedDEM].SSOComputations(SSOGridLimits,SSOvars,false,'','');
                    Table1.SetFieldByNameAsFloat('ASPECT_PTS',SSOVars.AspPts);
                    Table1.SetFieldByNameAsFloat('SSO_PTS',SSOVars.NumPts);

                    if (SSOVars.AvgAspect > -90) then begin
                       Table1.SetFieldByNameAsFloat('ASPECT_AVG',SSOVars.AvgAspect);
                       Table1.SetFieldByNameAsFloat('ASPECT_STR',SSOVars.AspectStrength);
                    end;

                    if GoodFabric and MDDef.IncludeFabricMeasures and (SSOvars.s1s2 < 32000) then begin
                        Table1.SetFieldByNameAsFloat('S1S2',SSOvars.S1S2);
                        Table1.SetFieldByNameAsFloat('STRENGTH',SSOvars.Strength);
                        Table1.SetFieldByNameAsFloat('S2S3',SSOvars.S2S3);
                        Table1.SetFieldByNameAsFloat('SHAPE',SSOvars.Shape);
                        Table1.CarefullySetFloat('ROUGHNESS',SSOvars.RoughnessFactor,0.0001);
                        if (SSOvars.TheDipDirs[3] > 180) then SSOvars.TheDipDirs[3] := SSOvars.TheDipDirs[3] - 180;
                        Table1.CarefullySetFloat('FABRIC_DIR',SSOvars.TheDipDirs[3],0.1);
                    end;
                   {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats end SSO'); {$EndIf}

                      (*
                       if MDDef.IncludeWavelength then begin
                           if GetPerpendicularLineEnd(Lat,Long,5000,TheDipDirs[3],Lat1,Long1,Lat2,Long2) then begin
                              MDDef.ForceCrestComputations := true;
                              LOSComputeOnly(ProfileData,DEMGLB[WantedDEM],0,lat1,long1,lat2,long2,0,0);
                              FindWavelengthStats(ProfileData,WavelengthMean,WavelengthMedian,WavelengthStdDev,
                                       HeightMean,HeightMedian,HeightStd);
                              if (WaveLengthMean > 0.01) then begin
                                 Table1.SetFieldByNameAsFloat('LEN_MEAN',WavelengthMean);
                                 Table1.SetFieldByNameAsFloat('LEN_MEDIAN',WavelengthMedian);
                                 Table1.SetFieldByNameAsFloat('LEN_STD',WavelengthStdDev);
                                 Table1.SetFieldByNameAsFloat('HT_MEAN',HeightMean);
                                 Table1.SetFieldByNameAsFloat('HT_MEDIAN',HeightMedian);
                                 Table1.SetFieldByNameAsFloat('HT_STD',HeightStd);
                                 Table1.SetFieldByNameAsFloat('HT_STD2AV',HeightStd/HeightMean);
                                 Table1.SetFieldByNameAsFloat('LEN_STD2AV',WavelengthStdDev/WavelengthMean);
                              end;
                              TableName := ProfileData.FullTableName;
                           end;
                        end;
                        *)
                  end;

                 if MDDef.IncludeMissingHoles then begin
                    DEMGlb[WantedDEM].ComputeMissingData(GridLimits,MissingPC);
                    Table1.SetFieldByNameAsFloat('MISSING',MissingPC);
                 end;

                  if MDDef.IncludeBasinID then begin
                    Table1.SetFieldByNameAsString('BASIN_ID',DEMGlb[WantedDEM].AreaName);
                    Table1.SetFieldByNameAsFloat('SLP_OV_30',DEMGlb[WantedDEM].Over30PercentSlope);
                    Table1.SetFieldByNameAsFloat('SLP_OV_50',DEMGlb[WantedDEM].Over50PercentSlope);
                  end;
                 {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished all'); {$EndIf}
               end
               else begin
                 {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats bad good elevation moments'); {$EndIf}
               end;
              Table1.Post;
              {$IfDef VCL}
                 TInterlocked.Increment(ThreadsNumDone);
                 if (ThreadsNumDone mod 100 = 0) and ShowSatProgress then UpdateProgressBar(ThreadsNumDone/ThreadsToDo);
              {$EndIf}
           end;
        end;

begin
  {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('BlockGeostats.Execute in  WantedDEM = ' + IntToStr(WantedDEM)); {$EndIf}
   if not DEMGlb[WantedDEM].FilledGridBox(GridLimits) then exit;
   if (WhatGeomorph in [gbDEM]) then begin
      {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('BlockGeostats.Execute entire DEM'); {$EndIf}
      GridLimits := DEMGlb[WantedDEM].FullDEMGridLimits;
      ComputeStats;
   end
   else if (WhatGeomorph in [gbPolygon,gbMapOnly]) then begin
      {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('BlockGeostats.Execute just map area'); {$EndIf}
      ComputeStats;
   end
   else if (WhatGeomorph in [gbPointNeighborhood]) then begin
      {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('BlockGeostats.Execute just point'); {$EndIf}
      ComputeStats;
   end
   else begin
      {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('BlockGeostats.Execute grid of points'); {$EndIf}
       DEMGlb[WantedDEM].GetBoxGridSizeDiameter(MDDef.GeomorphBoxSizeMeters,XBoxGridSize,YBoxGridSize,TStr);
       XBoxes := (GridLimits.XGridHigh - GridLimits.XGridLow) div XBoxGridSize;
       YBoxes := (GridLimits.YGridHigh - GridLimits.YGridLow) div YBoxGridSize;
       if (xboxes > 0) and (yboxes > 0) then begin
          StartX := GridLimits.XGridLow;
          StartY := GridLimits.YGridLow;
          try
             try
                SkipMenuUpdating := true;
                x := 0;
                while (x <= XBoxes) do  begin
                   GridLimits.XGridLow := StartX + x * XBoxGridSize;
                   GridLimits.XGridHigh := StartX + succ(x) * XBoxGridSize;
                   y := 0;
                   while (y <= YBoxes) do  begin
                       GridLimits.YGridLow := StartY + y * YBoxGridSize;
                       GridLimits.YGridHigh := StartY + succ(Y) * YBoxGridSize;
                       ComputeStats;
                       inc(y,MDDef.StatSampleIncr);
                   end {while y};
                   inc(x,MDDef.StatSampleIncr);
                end {while x};
             except
                 on Exception do begin end;
             end;
          finally
          end;
      end;
   end;
   {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('BlockGeostats.Execute out'); {$EndIf}
end;


procedure MakeGeomporphDBforPolygons(DBonTable : integer);
var
   dbName : PathStr;
   dbTable : tMyData;
   GridLimits : tGridLimits;
   i,rc : integer;
begin
   {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons in'); {$EndIf}
   if not (GISdb[DBonTable].DEMwithDBsMap) then begin
      MessageToContinue('Operation must be on a map with a DEM');
   end
   else begin
      GetGeomorphBlockOpts(gbPolygon,GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,GridLimits);
      MDDef.MaskShapesIn := true;
      dbName := ChangeFileExt(GISdb[DBonTable].dbFullName,'_geomorph' + DefaultDBExt);
      Make_tables.CreateGeomorphometryAttributesTable(dbname,false);
      dbTable := tMyData.Create(dbName);
      {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons db opened'); {$EndIf}
      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].AddSequentialIndex(RecNoFName,true);
      {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons prelims over'); {$EndIf}
      StartThreadTimers('Geomorph stats',0,true);
      rc := GISdb[DBonTable].MyData.TotRecsInDB;
      for i := 1 to rc do begin
         {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons Rec=' + IntToStr(i) + '/' + IntToStr(rc)); {$EndIf}
         ThreadTimers.OverallGauge9.Progress := round(100 * i/rc);
         GISdb[DBonTable].ApplyGISFilter(RecNoFName + '=' + IntToStr(i));

         if AtLeastPartOfBoxInAnotherBox(GISdb[DBonTable].MyData.GetRecordBoundingBox, DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].DEMBoundBoxGeo) then begin
            GISdb[DBonTable].ZoommaptorecordWithBufferMeters(MDDef.MaskDistance);
            {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons zoom ' + GISdb[DBonTable].TheMapOwner.MapDraw.MapSizeString); {$EndIf}
            GISdb[DBonTable].TheMapOwner.ResizeByPercentage(100);
            {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons 100% ' + GISdb[DBonTable].TheMapOwner.MapDraw.MapSizeString + ' mask=' +IntToStr(MDDef.MaskDistance)); {$EndIf}
            MaskDEMFromShapeFile(GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,DBonTable,true,MDDef.MaskShapesIn,i,MDDef.MaskDistance);
            {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons masked from ' + IntToStr(i) + GISdb[DBonTable].TheMapOwner.MapDraw.MapSizeString); {$EndIf}
            DEMStat.NewBlockGeoStats(gbPolygon,dbTable,GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,GISdb[DBonTable].TheMapOwner.MapDraw.MapAreaDEMGridLimits);
            {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons stats'); {$EndIf}
            DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].ReloadDEM(true);
            GISdb[DBonTable].TheMapOwner.FullDEM1Click(Nil);
            {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons reloaded'); {$EndIf}
         end
         else begin
            {$IfDef RecordGeostat} WriteLineToDebugFile('Polygon not on map'); {$EndIf}
         end;
      end;
      {$IfDef RecordGeostat} WriteLineToDebugFile('Recs=' + IntToStr(dbTable.RecordCount) + '  ' + dbName); {$EndIf}
      dbTable.Destroy;
      GISdb[DBonTable].ApplyGISFilter('');
      GISdb[DBonTable].TheMapOwner.ReloadDEMClick(Nil);
      GISdb[DBonTable].TheMapOwner.OpenDBonMap('',dbName);
      GISdb[DBonTable].ShowStatus;
      EndThreadTimers;
   end;
   {$IfDef RecordGeostat} WriteLineToDebugFile('MakeGeomporphDBforPolygons out'); {$EndIf}
end;

{$EndIf}


function SumDEMs(FirstDEM : integer; Merge : tDEMbooleans; NewName : shortstring) : integer;
label
   MissingData;
var
   i,j,Col,Row : integer;
   Lat,Long : float64;
   z,z2 : float32;
begin
   StartProgress('Sum grids');
   if OpenAndZeroNewDEM(true,DEMGlb[FirstDEM].DEMheader,Result,NewName,InitDEMmissing) then begin
      for Col := 0 to pred(DEMGlb[Result].DEMheader.NumCol) do begin
         if (Col mod 100 = 0) then UpdateProgressBar(Col/DEMGlb[Result].DEMheader.NumCol);
         for Row := 0 to pred(DEMGlb[Result].DEMheader.NumRow) do begin
            z := 0;
            for i := 1 to MaxDEMDataSets do if Merge[i] then begin
               if DEMGlb[FirstDEM].SecondGridIdentical(i) then begin
                  if DEMGlb[i].GetElevMeters(Col,Row,z2) then z := z + z2
                  else GoTo MissingData;
               end
               else begin
                  DEMGlb[FirstDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                  if DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,z2) then z := z + z2
                  else Goto MissingData;;
               end;
            end;
            DEMGlb[Result].SetGridElevation(Col,Row,z);
            MissingData:;
         end;
      end;
      EndProgress;
      DEMGlb[Result].SetUpMap(Result,true,DEMGlb[FirstDEM].SelectionMap.MapDraw.MapType);
   end;
end;


procedure IandPLegend(pc : array of float64);
var
   i : integer;
   bmp : tMyBitmap;

            procedure LegBox(i: integer);
            var
               x,y : integer;
            begin
               x := 50 * (pred(i) div 4);
               y := 50 * (pred(i) mod 4);
               bmp.canvas.Brush.Color := IPcolor[i];
               bmp.Canvas.Rectangle(x,y,x+50,Y+50);
               bmp.Canvas.TextOut(x+5,Y+5,IntToStr(i));
               bmp.Canvas.TextOut(x+5,Y+25,RealToString(pc[pred(i)],5,2) + '%');
            end;

begin
   CreateBitmap(bmp,200,200);
   for i := 1 to MDDef.IwashPikeCats do LegBox(i);
   Petimage_form.DisplayBitmap(bmp,'Legend');
end;



function ClusterGrids(StartingGrid,EndingGrid : integer) : integer;
{$IfDef ExGeoStats}
begin
{$Else}
label
   MissingData,MissingData2,ReclassDone,ReachedLimit;
var
   MVClusterClientDataSet : TMVClusterClientDataSet;
   BestClass,Sampler,i,j,x,y,NPts,db : integer;
   Limits : tGridLimits;
   AveDev,aMin,zf : float32;
   FieldsUsed : byte;
   FieldsToUse : array[1..MaxBands] of AnsiString;
   z,Mean,Std : array[1..MaxDEMDataSets] of float32;
   DistArr : array[1..EdburgGeneralFuncsMaxClusters] of float64;
   HistArray : array[1..EdburgGeneralFuncsMaxClusters] of integer;
   NewHeadRecs : tDEMheader;
begin
   {$IfDef RecordClustering} WriteLineToDebugFile('ClusterGrids in'); {$EndIf}
     Limits := DEMGlb[StartingGrid].FullDEMGridLimits;
     Sampler := 1;
     while ((Limits.XGridHigh-Limits.XGridLow) div Sampler) * ((Limits.YGridHigh-Limits.YGridLow) div Sampler) > EdburgGeneralFuncsMaxObservations do inc(Sampler);

     if (Sampler > 1) then begin
        repeat
            dec(Sampler);
            NPts := 0;
            y := Limits.YGridLow;
            while y <= Limits.YGridHigh do begin
               x := Limits.XGridLow;
               while x <= Limits.XGridHigh do begin
                  if (not DEMGlb[2].MissingDataInGrid(x,y)) then inc(Npts);
                  inc(x,sampler);
               end;
               inc(y,sampler);
            end;
        until (NPts > EdburgGeneralFuncsMaxObservations);
        inc(Sampler);
     end;

     if not ClusterOptions.GetClusterOptions(Sampler,false) then exit;

     MVClusterClientDataSet := TMVClusterClientDataSet.Create(Application);
     DefineMICRODEMClusteringOptions(MVClusterClientDataSet);

      FieldsUsed := 0;
      for i := StartingGrid to EndingGrid do begin
          if (DEMGlb[i] <> Nil) then begin
             MVClusterClientDataSet.FieldDefs.Add('DEM_' + IntToStr(i),ftFloat, 0, False);
             inc(FieldsUsed);
             FieldsToUse[FieldsUsed] := 'DEM_' + IntToStr(i);
             DEMGlb[i].ElevationStatistics(Mean[i],Std[i],AveDev,true);
             {$IfDef RecordClustering} WriteLineToDebugFile(DEMGlb[i].AreaName + '   DEM ' + IntToStr(i) + ' mean=' + RealToString(Mean[i],-18,-5) + '  std=' + RealToString(STD[i],-12,-5)); {$EndIf}
          end;
      end;
      MVClusterClientDataSet.CreateDataset;
      MVClusterClientDataSet.Open;

      StartProgress('Load');
      y := Limits.YGridLow;
      while y <= Limits.YGridHigh do begin
         UpdateProgressBar((y-Limits.YGridLow)/(Limits.YGridHigh-Limits.YGridLow));
         x := Limits.XGridLow;
         while x <= Limits.XGridHigh do begin
            for i := StartingGrid to EndingGrid do if (DEMGlb[i] <> Nil) then begin
               if (not DEMGlb[i].GetElevMeters(x,y,z[i])) then goto MissingData;
            end;

            MVClusterClientDataSet.Insert;
            for i := StartingGrid to EndingGrid do begin
               if (DEMGlb[i] <> Nil) then begin
                  MVClusterClientDataSet.FieldByName('DEM_' + IntToStr(i)).AsFloat := VariableWeightings[i] * (z[i] - Mean[i]) / std[i];
               end;
            end;
            MVClusterClientDataSet.Post;
            if (MVClusterClientDataSet.RecordCount > EdburgGeneralFuncsMaxObservations) then goto ReachedLimit;

            MissingData:;
            inc(x,sampler);
         end;
         inc(y,sampler);
      end;
     ReachedLimit:;
      EndProgress;

     {$IfDef RecordClustering}
        WriteLineToDebugFile('Samples used: ' + IntToStr(MVClusterClientDataSet.RecordCount) + '   limit=' + IntToStr(EdburgGeneralFuncsMaxObservations));
        WriteLineToDebugFile('Sample thinning: ' + IntToStr(Sampler));
        WriteLineToDebugFile('Distance power: ' + RealToString(MDDef.ClassDistancePower,4,1));
     {$EndIf}

       with MVClusterClientDataSet do begin
          ShowHourglassCursor;
          wmDEM.SetPanelText(0,'K Means Clustering');
          KMeansClustering(FieldsToUse, FieldsUsed, MDTempDir + 'Clster_Results.HTML');
          wmDEM.SetPanelText(0,'');
       end;

     NewHeadRecs := DEMGlb[StartingGrid].DEMheader;
     NewHeadRecs.DEMPrecision := ByteDEM;

     if not OpenAndZeroNewDEM(true,NewHeadRecs,Result,'Clusters',InitDEMvalue,0) then exit;
        with DEMGlb[Result],DEMheader do begin
           ShortName := 'Clusters';
           DEMheader.ElevUnits := Undefined;
           DefineDEMVariables(true);
           StartProgress('Classify');
           for y := 0 to pred(NumRow) do begin
              if (y mod 100 = 0) then UpdateProgressBar(y/NumRow);

              for x := 0 to pred(NumCol) do begin
                 for i := StartingGrid to EndingGrid do if (DEMGlb[i] <> Nil) and (i <> Result) then begin
                    if (DEMGlb[i].GetElevMeters(x,y,z[i])) then z[i] := VariableWeightings[i] * (z[i] - Mean[i]) / std[i]
                    else goto MissingData2;
                 end;

                 for j := 1 to MVClusterClientDataSet.NClusters do begin
                    DistArr[j] := 0;
                    for i := StartingGrid to EndingGrid do if (DEMGlb[i] <> Nil) and (i <> Result) then
                       DistArr[j] := DistArr[j] + Power(abs(z[i] - MVClusterClientDataSet.ClsCenters[j,i]),MDDef.ClassDistancePower);
                 end; {for j}

                 aMin := DistArr[1];
                 BestClass := 1;
                 for j:= 2 to MVClusterClientDataSet.NClusters do begin
                    if ( DistArr[j] < aMin ) then begin
                       aMin := DistArr[j];
                       BestClass:= j;
                    end; // if ( DistArr[j] < aMin )
                 end; // for j:= 2 to NumberClusters
                 DEMGlb[Result].SetGridElevation(x,y,BestClass);
                 MissingData2:;
              end {for x};
           end {for y};
        end {with};

        for i := 1 to EdburgGeneralFuncsMaxClusters do HistArray[i] := 0;
        for y := 0 to pred(DEMGlb[Result].DEMheader.NumRow) do begin
           for x := 0 to pred(DEMGlb[Result].DEMheader.NumCol) do begin
              if DEMGlb[Result].GetElevMeters(x,y,zf) then inc(HistArray[round(zf)]);
           end;
        end;

        for i := 1 to pred(EdburgGeneralFuncsMaxClusters) do begin
           if (HistArray[i] = 0) then begin
              for j := succ(i) to EdburgGeneralFuncsMaxClusters do begin
                 if (HistArray[j] <> 0)  then begin
                     WriteLineToDebugFile('Reclass ' + IntToStr(j) + ' to ' + IntToStr(i));
                      for y := 0 to pred(DEMGlb[Result].DEMheader.NumRow) do begin
                         for x := 0 to pred(DEMGlb[Result].DEMheader.NumCol) do begin
                            if DEMGlb[Result].GetElevMeters(x,y,zf) and (round(zf) = j)  then
                               DEMGlb[Result].SetGridElevation(x,y,i);
                         end;
                      end;
                    HistArray[i] := HistArray[j];
                    HistArray[j] := 0;
                    goto ReclassDone;
                 end;
              end;
           end;
           ReclassDone:;
        end;

   DEMGlb[Result].DEMFileName := Petmar.NextFileNumber(MDTempDir, 'cluster_', '.dem');
   DEMGlb[Result].WriteNewFormatDEM(DEMGlb[Result].DEMFileName );

   DEMGlb[Result].CreateVATforDEM(false);
   DEMGlb[Result].SetUpMap(Result,true,mtDEMVATTable,DEMGlb[Result].AreaName);
   db := DEMGlb[Result].SelectionMap.LoadDataBaseFile(ChangeFileExt(DEMGlb[Result].DEMFileName,'.vat.dbf'));
   {$IfDef RecordClustering}
      GISdb[db].MyData.First;
      while Not GISdb[db].MyData.eof do begin
         WriteLineToDebugFile('Cluster: ' + IntToStr(GISdb[db].MyData.GetFieldByNameAsInteger('VALUE')) + '  NPTS=' + IntToStr(GISdb[db].MyData.GetFieldByNameAsInteger('N')));
         GISdb[db].MyData.Next;
      end;
   {$EndIf}
   GISdb[db].StartVATEdits;
   MVClusterClientDataSet.Free;
   ShowDefaultCursor;
{$EndIf}
end;


function IwashuriPikeCat(Slope,Convexity,Texture : float64) : integer;
var
   AboveConvexityMean,AboveTextureMean : boolean;
begin
   AboveConvexityMean := Convexity > ConvexityMeanCut;
   AboveTextureMean := Texture > TextureMeanCut;
   if (Slope > SlopeMeanCut) then begin
      if AboveConvexityMean then begin
         if AboveTextureMean then Result := 2 else Result := 1;
      end
      else begin
         if AboveTextureMean then Result := 4 else Result := 3;
      end;
   end
   else begin
      if (MDDef.IwashPikeCats  = 8) then begin
         if AboveConvexityMean then begin
            if AboveTextureMean then Result := 6 else Result := 5;
         end
         else begin
            if AboveTextureMean then Result := 8 else Result := 7;
         end;
      end
      else begin
         if (Slope > SlopeQuarterCut) then begin
            if AboveConvexityMean then begin
               if AboveTextureMean then Result := 6 else Result := 5;
            end
            else begin
               if AboveTextureMean then Result := 8 else Result := 7;
            end;
         end
         else begin
            if (MDDef.IwashPikeCats  = 12) then begin
               if AboveConvexityMean then begin
                  if AboveTextureMean then Result := 10 else Result := 9;
               end
               else begin
                  if AboveTextureMean then Result := 12 else Result := 11;
               end;
            end
            else begin  //(MDDef.IwashPikeCats  = 16)
               if (Slope > SlopeEigthCut) then begin
                  if AboveConvexityMean then begin
                     if AboveTextureMean then Result := 10 else Result := 9;
                  end
                  else begin
                     if AboveTextureMean then Result := 12 else Result := 11;
                  end;
               end
               else begin
                  if AboveConvexityMean then begin
                     if AboveTextureMean then Result := 14 else Result := 13;
                  end
                  else begin
                     if AboveTextureMean then Result := 16 else Result := 15;
                  end;
               end;
            end;
         end;
      end;
   end;
end;


function IwashuriPikeColor(Slope,Convexity,Texture : float64;  var TerClass : integer) : tPlatformColor;
begin
   TerClass := IwashuriPikeCat(Slope,Convexity,Texture);
   Result := ConvertTColorToPlatformColor(IPColor[TerClass]);
end;


{$If Defined(ExGeoStats) or Defined(ExSat)}
{$Else}
procedure ComputeVarCoVarAndPrincipalComponents(DB : integer; MG : tMultigridArray; UseFields : tStringList);
var
   pc : array[1..MaxBands] of float64;
   k,l,
   NewDEM,i,j,n,Band,NRot,NumVars : integer;
   EigenVectors,Correlations,VarCoVar   : tTrendMatrix;
   EigenValues   : tTrendVector;
   Value,Lat,Long    : float64;
   z : float32;
   FName,fName2,MGPath : PathStr;
   NewTable : tMyData;
   LatLongPresent : boolean;
   MainTitle,MenuStr,TStr : ANSIString;
   Findings : tStringList;
   EigenGrid : TGridForm;
   VarTitle : array[1..MaxBands] of string35;
begin
   {$IfDef RecordPC} WriteLineToDebugFile('ComputeVarCoVarAndPrincipalComponents in'); {$EndIf}

   if (MG <> Nil) and (MG.NumGrids > MaxMatrixSize) then begin
      MessageToContinue('Too many bands');
      exit;
   end;

   SetPrincipalComponentsOptions;

   if ValidDB(DB) then begin
      DataBaseCorrelations(DB,UseFields,VarCovar,Correlations,N);
      MainTitle := 'Database';
      NumVars := UseFields.Count;
      for i := 1 to NumVars do VarTitle[i] := UseFields[pred(i)];
      {$IfDef RecordPC} WriteLineToDebugFile('DB options DataBaseCorrelations done'); {$EndIf}
   end;

   if (MG <> Nil) then begin
      {$IfDef RecordPC} WriteLineToDebugFile('MG option');    {$EndIf}

      StartProgress('covariances');
      NumVars := MG.NumGrids;
      for i := 1 to MG.NumGrids do begin
         UpDateProgressBar(i/MG.NumGrids);
         if (MG.Grids[i] <> 0) then begin
            VarTitle[i] := DEMGlb[MG.Grids[i]].AreaName;
            for j := 1 to MG.NumGrids do begin
               if (MG.Grids[j] <> 0) then begin
                  StatsFromTwoGrids(MG.Grids[i],MG.Grids[j],Correlations[i,j],VarCoVar[i,j]);
               end;
            end;
         end;
      end;
      EndProgress;
      {$IfDef RecordPC} WriteLineToDebugFile('MG cov done'); {$EndIf}
   end;


   if MDDef.PCCorrelation then begin
      {$IfDef RecordPC} WriteLineToDebugFile('MDDef.PCCorrelation start'); {$EndIf}
      Findings := tStringList.Create;
      MenuStr := 'Band';
      for i := 1 to NumVars do MenuStr := MenuStr + ',' +  NoCommas(VarTitle[i]);
      Findings.Add(MenuStr);
      for i := 1 to NumVars do begin
         MenuStr := NoCommas(VarTitle[i]);
         for j := 1 to NumVars do MenuStr := MenuStr + ',' +  RealToString(Correlations[j,i],-8,4);
         Findings.Add(MenuStr);
      end;
      fName := MDTempDir + 'sat_r_matrix.csv';
      Findings.SaveToFile(fName);
      Findings.Free;
      DEMStringGrid.OpenCorrelationMatrix('Correlation',fName);

      {$IfDef RecordPC} WriteLineToDebugFile('MDDef.PCCorrelation done'); {$EndIf}
   end;

   if MDDef.PCVarCovar then begin
      {$IfDef RecordPC} WriteLineToDebugFile('MDDef.PCVarianceCovariance start'); {$EndIf}
      Findings := tStringList.Create;
      MenuStr := 'Band';
      for i := 1 to NumVars do MenuStr := MenuStr + ',' +  NoCommas(VarTitle[i]);
      Findings.Add(MenuStr);
      for i := 1 to NumVars do begin
         MenuStr := NoCommas(VarTitle[i]);
         for j := 1 to NumVars do MenuStr := MenuStr + ',' +  RealToString(VarCoVar[i,j],-18,8);
         Findings.Add(MenuStr);
      end;
      fName := MDTempDir + 'sat_covar_matrix.csv';
      Findings.SaveToFile(fName);
      Findings.Free;
      DEMStringGrid.OpenCorrelationMatrix('Covariance',fName);
      {$IfDef RecordPC} WriteLineToDebugFile('MDDef.PCVarianceCovariance done'); {$EndIf}
   end
   else begin
      {$IfDef RecordPC} WriteLineToDebugFile('MDDef.PCVarianceCovariance skipped'); {$EndIf}
   end;

   {$IfDef RecordPC} WriteLineToDebugFile('Call Jacobi'); {$EndIf}
   Jacobi(VarCoVar,NumVars,Eigenvalues,EigenVectors,Nrot);
   {$IfDef RecordPC} WriteLineToDebugFile('Call EigSort'); {$EndIf}
   Eigsrt(EigenValues,EigenVectors,NumVars);

   if MDDef.PCEigenValues then begin
      EigenGrid := TGridForm.Create(Application);
      EigenGrid.Caption := MainTitle + ' eigenvalues and PC loadings';
      EigenGrid.StringGrid1.ColCount := succ(NumVars);
      EigenGrid.StringGrid1.RowCount := 3 + (NumVars);
      EigenGrid.SetFormSize;
      EigenGrid.Top := 10;
      EigenGrid.Left := 10;

      for i := 1 to NumVars do EigenGrid.StringGrid1.Cells[i,0] := 'PC' + IntToStr(i);
      EigenGrid.StringGrid1.Cells[0,1] := 'Eigenvalue';
      EigenGrid.StringGrid1.Cells[0,2] := 'Explains';
      Value := 0;
      for i := 1 to NumVars do Value := Value + Eigenvalues[i];
      for i := 1 to NumVars do begin
         EigenGrid.StringGrid1.Cells[0,2+i] := VarTitle[i];
         EigenGrid.StringGrid1.Cells[i,1] := RealToString(Eigenvalues[i],-18,8);
         pc[i] := 100.0 * Eigenvalues[i] / Value;
         EigenGrid.StringGrid1.Cells[i,2] := RealToString(pc[i],-12,6);
         for Band := 1 to NumVars do EigenGrid.StringGrid1.Cells[i,2+Band] := RealToString(EigenVectors[Band,i],-8,5);
      end;
      EndProgress;
      EigenGrid.Show;
      {$IfDef RecordPC} WriteLineToDebugFile('MDDef.PCEigenValues done'); {$EndIf}
   end
   else begin
      {$IfDef RecordPC} WriteLineToDebugFile('MDDef.PCEigenValues skipped'); {$EndIf}
   end;

   if MDDef.PCResults then begin
      if ValidDB(DB) then begin
         fName2 := MDTempDir + 'pc' + DefaultDBExt;
         MakePCTable(fName2,NumVars);
         NewTable := tMyData.Create(fName2);
         StartProgress('Loadings');
         LatLongPresent := GISDB[DB].MyData.FieldExists('LAT') and GISDB[DB].MyData.FieldExists('LONG');
         i := 0;
         GISDB[DB].MyData.First;
         while not GISDB[DB].MyData.eof do begin
             inc(i);
             if (i mod 100 = 0) then begin
                UpdateProgressBar(i/GISDB[DB].MyData.FiltRecsInDB);
                GISDB[DB].EmpSource.Enabled := false;
             end;
             NewTable.Insert;
             if LatLongPresent and GISDB[DB].MyData.ValidLatLongFromTable(Lat,Long) then begin
                NewTable.SetFieldByNameAsFloat('LAT',Lat);
                NewTable.SetFieldByNameAsFloat('LONG',Long);
             end;
             for k := 1 to NumVars do begin
                Value := 0;
                for l := 1 to NumVars do begin
                    Value := Value + EigenVectors[l,k] * GISDB[DB].MyData.GetFieldByNameAsFloat(UseFields[pred(l)]);
                end {for l};
                NewTable.SetFieldByNameAsFloat('PC' + IntToStr(k),Value);
             end {for k};
             NewTable.Post;
             GISDB[DB].MyData.Next;
         end;
         NewTable.Destroy;
         EndProgress;
         {$IfDef RecordPC} WriteLineToDebugFile('(DB <> Nil) and MDDef.PCResults done'); {$EndIf}
      end;

      if (MG <> Nil) then begin
         {$IfDef RecordPC} WriteLineToDebugFile('MG fill new grids start'); {$EndIf}
         NakedMapOptions;
         TStr := ShortLandsatName(ExtractFileName(MG.BasePath));
         MGPath := ExtractFilePath(MG.BasePath) + '\princ_comps\';
         if not PathIsValid(MGPath) then SafeMakeDir(MGPath);
         MGPath := MGPath + TStr;

         for k := 1 to MDDef.MaxPCBands do begin
            if (K <= MG.NumGrids) and (pc[k] > MDdef.MinPCToShow) then begin
               FName := MGPath + '_pc_' + IntToStr(k) + '.dem';
               NewDEM := DEMGLB[MG.Grids[1]].CloneAndOpenGrid(FloatingPointDEM,fName,DEMGLB[MG.Grids[1]].DEMheader.ElevUnits);
               {$IfDef RecordPC} WriteLineToDebugFile('Create grid: ' + fName); {$EndIf}

               StartProgress('Compute PC ' + IntToStr(k));
               for i := 0 to pred(DEMGLB[MG.Grids[1]].DEMheader.NumCol) do begin
                  if (i mod 100 = 0) then UpdateProgressBar( i / DEMGLB[MG.Grids[1]].DEMheader.NumCol);
                  for j := 0 to pred(DEMGLB[MG.Grids[1]].DEMheader.NumRow) do begin
                        Value := 0;
                        for l := 1 to MG.NumGrids do begin
                           if (MG.Grids[l] <> 0) and DEMGLB[MG.Grids[l]].GetElevMeters(i,j,z) and (abs(z) > 0.00001) then begin
                              Value := Value + EigenVectors[l,k] * z;
                           end;
                        end;
                        if (abs(Value) > 0.0000000001) then begin
                           DEMGLB[NewDEM].SetGridElevation(i,j,Value);
                        end;
                  end {for j};
               end;
               EndProgress;
               DEMGlb[NewDEM].DEMheader.ElevUnits := Undefined;   //Imagery;
               DEMGlb[NewDEM].CheckMaxMinElev;
               DEMGlb[NewDEM].WriteNewFormatDEM(fName);
               if MDDef.LoadPCBands then begin
                  DEMGlb[NewDEM].AreaName := ExtractFileNameNoExt(fName);
                  DEMGlb[NewDEM].SetUpMap(NewDEM,false,mtElevGray,DEMGlb[NewDEM].AreaName);
               end
               else CloseSingleDEM(NewDEM);
            end;
            RestoreBackupDefaults;
         end;
         {$IfDef RecordPC} WriteLineToDebugFile('MG fill new grids end'); {$EndIf}
      end;
   end;
   {$IfDef RecordPC} WriteLineToDebugFile('ComputeVarCoVarAndPC out'); {$EndIf}
end;

{$EndIf}



procedure StatsFromTwoGrids(iDEM,JDEM : integer; var r,covar : float64; Incr : integer = 1);
var
   Col,Row : integer;
   NPts : int64;
   z1,z2 : float32;
   Lat,Long,XGrid,YGrid : float64;
   IdenticalGrids : boolean;
   GridLimits : tGridLimits;
   x,y : ^bfarray32;
begin
   {$IfDef RecordStat} WriteLineToDebugFile('StatsFromTwoGrids in, grids=' + IntToStr(IDEM) + ' and ' + IntToStr(jDEM)); {$EndIf}
   IdenticalGrids := DEMGlb[iDEM].SecondGridIdentical(jDEM);
   GridLimits := DEMGlb[iDEM].FullDEMGridLimits;

   NPts := 0;
   New(x);
   New(y);
   while ( (GridLimits.XGridHigh - GridLimits.XGridLow) div Incr) * ((GridLimits.YGridHigh - GridLimits.YGridLow) div Incr) > Petmath.bfArrayMaxSize do inc(incr);
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if DEMGlb[iDEM].GetElevMeters(Col,Row,z1) then begin
            if IdenticalGrids then begin
               if DEMGlb[jDEM].GetElevMeters(Col,Row,z2) then begin
                  x^[NPts] := z1;
                  y^[NPts] := z2;
                  inc(NPts);
               end;
            end
            else begin
               DEMGlb[iDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
               DEMGlb[jDEM].LatLongDegreetoDEMGrid(Lat,Long,XGrid,YGrid);
               if DEMGlb[jDEM].GetElevMeters(XGrid,YGrid,z2) then begin
                  x^[NPts] := z1;
                  y^[NPts] := z2;
                  inc(NPts);
               end;
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
   varcovar(x^,y^,NPts,r,covar);
   Dispose(x);
   Dispose(y);
end;




{$IfDef ExGeoStats}
{$Else}


procedure ElevationSlopePlot(WhichDEM : integer; DesiredBinSize : integer = 1);
label
   Restart;
const
   MinSlopeValue = 0;
   MaxSlopeValue = 250;
type
   TheArrayType = array[MinSlopeValue..MaxSlopeValue] of float64;
   CountType    = array[MinSlopeValue..MaxSlopeValue] of LongInt;
   GraphType    = (ElevFreq,SlopeFreq,ElevSlope,AspectFreq,CumSlope,AspectRose,ElevSlopeDeg);
var
   Graph : GraphType;
   BinSize,StartIndex,Bin,
   IntSlope,
   MinMin,MaxMax,
   StartDEM,EndDEM,
   j,x,which : LongInt;
   MaxCount,MaxSlopeCount,
   Cum,MinElevZ,MaxElevZ,MaxSlope : float64;
   NumBins  : array[1..MaxDEMDataSets] of integer;
   TotalDEM : array[1..MaxDEMDataSets] of LongInt;
   SlopeTotal,
   MaxDEMSlope,
   SlopeSqrTotal,
   TotalCount : array[1..MaxDEMDataSets] of float64;
   Count,
   SumSquares,
   BinElev,
   SlopeHist,
   Slopes   : array[1..MaxDEMDataSets] of ^TheArrayType;
   PointCount : array[tCompassDirection] of int32;
   ThisGraph : TThisBaseGraph;
   SlopeAspectRec : tSlopeAspectRec;
   GraphList : tStringList;
   AspectStats : tAspectStats;

      function GraphPlot(Graph : GraphType) : TThisBaseGraph;
      var
         Which,x : integer;
         rfile,rfile2,rfile3 : file;
         v       : tGraphPoint32;
         MenuStr,TStr : ShortString;
      begin
         if Graph in [AspectRose] then begin
            Result := AspectStats.CreateRose;
         end
         else begin
             Result := TThisBaseGraph.Create(Application);
             case Graph of
                ElevFreq   : Tstr := ' Elevation Frequency, bin size=' + IntToStr(BinSize) + ' m';
                ElevSlopeDeg : TStr := ' Elevation vs Slope (?), bin size=' + IntToStr(BinSize) + ' m';
                ElevSlope  : TStr := ' Elevation vs Slope (%), bin size=' + IntToStr(BinSize) + ' m';
                SlopeFreq  : TStr := ' Slope Frequency';
                AspectFreq : TStr := ' Aspect Frequency';
                CumSlope   : TStr := ' Cumulative Slope Distribution';
                AspectRose : TStr := ' Aspect Distribution';
             end {case};
             {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('Plot =' + TStr); {$EndIf}
             Result.GraphDraw.VertLabel := 'Elev (m)';
             Result.GraphDraw.MinHorizAxis := 0.0;
             Result.GraphDraw.HorizLabel := 'Percentage of Region';
             Result.GraphDraw.MinVertAxis := MinElevZ;
             Result.GraphDraw.MaxVertAxis := MaxElevZ;
             if (StartDEM = EndDEM) then TStr := DEMGlb[WhichDEM].AreaName + '  ' + TStr;
             if (not BaseGraf.CreateGraphHidden) then Result.Caption := TStr;
             case Graph of
               ElevFreq  : Result.GraphDraw.MaxHorizAxis := MaxCount;
               ElevSlope : begin
                              Result.GraphDraw.MaxHorizAxis := MaxSlope;
                              Result.GraphDraw.HorizLabel := 'Avg Slope ' + SlopeMethodName(MdDef.SlopeAlg)+' (%)';
                           end;
               ElevSlopeDeg : begin
                              Result.GraphDraw.MaxHorizAxis := arcTan(0.01*MaxSlope) / DegToRad;
                              Result.GraphDraw.HorizLabel := 'Avg Slope ' + SlopeMethodName( MDDef.SlopeAlg)+' (?)';
                           end;
               SlopeFreq : begin
                             Result.GraphDraw.MaxHorizAxis := MaxSlopeCount;
                             Result.GraphDraw.MinVertAxis := 0;
                             Result.GraphDraw.MaxVertAxis := MaxSlopeValue;
                             Result.GraphDraw.VertLabel := 'Slope ' + SlopeMethodName(MDDef.SlopeAlg) + ' (%)';
                           end;
               CumSlope   : begin
                               Result.GraphDraw.HorizLabel := 'Slope ' + SlopeMethodName(MDDef.SlopeAlg) + ' (%)';
                               Result.GraphDraw.VertLabel := 'Cumulative Percentage Less Steep';
                               Result.GraphDraw.VertAxisFunct := NInv;
                               Result.GraphDraw.VertAxisFunctionType := CumulativeNormalAxis;
                            end;
             end {case};

            Result.SetUpGraphForm;
            Result.GraphDraw.LegendList := tStringList.Create;
            Result.GraphDraw.SetShowAllLines(true);
            Result.GraphDraw.SetShowAllPoints(false);

            for Which := StartDEM to EndDEM do if ValidDEM(Which) then begin
               Result.GraphDraw.LegendList.Add(DEMGlb[Which].AreaName);
               Result.OpenDataFile(rfile);
               if (Graph in [ElevSlope,ElevSlopeDeg])  then begin
                  Result.GraphDraw.ShowLine[Result.GraphDraw.DataFilesPlotted.Count] := true;
               end;
               if (Graph in [ElevSlope,ElevSlopeDeg]) and (StartDEM = EndDEM) and (MDdef.ShowSDonElevSlope) and (MDdef.ShowGeomorphometry) then begin
                  Result.OpenDataFile(Rfile2);
                  Result.OpenDataFile(Rfile3);
                  Result.GraphDraw.LineSize256[2] := 1;
                  Result.GraphDraw.LineSize256[3] := 1;
               end;
               Result.GraphDraw.FileColors256[Which] := ConvertTColorToPlatformColor(WinGraphColors[Which]);
               if (Graph = ElevFreq) then begin
                  for x := 0 to NumBins[Which] do if Count[Which]^[x] > 0 then begin
                     v[1] := Count[Which]^[x];
                     v[2] := BinElev[Which]^[x];
                     BlockWrite(Rfile,v,1);
                  end;
                  Result.GraphDraw.ShowLine[Result.GraphDraw.DataFilesPlotted.Count] := true;
               end
               else if (Graph = SlopeFreq) then begin
                  for x := 0 to MaxSlopeValue do if SlopeHist[Which]^[x] > 0 then begin
                     v[1] := SlopeHist[Which]^[x];
                     v[2] := x;
                     BlockWrite(Rfile,v,1);
                  end;
               end
               else if Graph in [ElevSlope,ElevSlopeDeg] then begin
                  if (StartDEM = EndDEM) then Result.GraphDraw.FileColors256[1] := claBlue;
                  Result.Image1.Canvas.Pen.Width := 3;
                  for x := 0 to NumBins[Which] do if Count[Which]^[x] > 0 then begin
                     v[1] := Slopes[Which]^[x];
                     v[2] := BinElev[Which]^[x];
                     if (Graph = ElevSlopeDeg) then v[1] := ArcTan(0.01 * v[1]) / DegToRad;
                     {$IfDef RecordStdDef} WriteLineToDebugFile('bin: ' + RealToString(v[2],-12,-1) + '  slope: ' + RealToString(v[1],-12,-1)); {$EndIf}

                     BlockWrite(rfile,v,1);
                     if (StartDEM = EndDEM) and (MDdef.ShowSDonElevSlope) and (MDdef.ShowGeomorphometry) then begin
                        v[1] := Slopes[Which]^[x] - SumSquares[Which]^[x];
                        if (Graph = ElevSlopeDeg) then v[1] := ArcTan(0.01 * v[1]) / DegToRad;
                        {$IfDef RecordStdDef} WriteLineToDebugFile('std: ' + RealToString(SumSquares[Which]^[x],-12,-1) + '  slope - std: ' + RealToString(v[1],-12,-1)); {$EndIf}
                        BlockWrite(rfile2,v,1);
                        v[1] := Slopes[Which]^[x] + SumSquares[Which]^[x];
                        if (Graph = ElevSlopeDeg) then v[1] := ArcTan(0.01 * v[1]) / DegToRad;
                        {$IfDef RecordStdDef} WriteLineToDebugFile('slope + std: ' + RealToString(v[1],-12,-1)); {$EndIf}
                        BlockWrite(rfile3,v,1);
                     end;
                  end;
               end
               else if (Graph = CumSlope) then begin
                  TotalCount[WhichDEM] := 0;
                  for x := 0 to 100 do TotalCount[WhichDEM] := TotalCount[WhichDEM] + SlopeHist[Which]^[x];
                  x := 0;
                  Cum := 0;
                  repeat
                     Cum := SlopeHist[Which]^[x] / TotalCount[WhichDEM] * 100;
                     inc(x);
                  until Cum > 0;
                  v[1] := pred(x);
                  v[2] := Cum;
                  BlockWrite(Rfile,v,1);

                  while (x <= 100) do begin
                     Cum := Cum + SlopeHist[Which]^[x] / TotalCount[WhichDEM] * 100;
                     if SlopeHist[Which]^[x] > 0 then begin
                        v[1] := x;
                        v[2] := Cum;
                        BlockWrite(Rfile,v,1);
                     end;
                     inc(x);
                  end;
               end {if};
               CloseFile(Rfile);
               if (Graph in [ElevSlope,ElevSlopeDeg]) and (StartDEM = EndDEM) and (MDdef.ShowSDonElevSlope) and (MDdef.ShowGeomorphometry) then begin
                  CloseFile(Rfile2);
                  CloseFile(Rfile3);
               end;
               Result.RedrawDiagram11Click(Nil);
            end {for Which};
         end;
      end;

var
   LegendBMP : tMyBitmap;
   LegendY : integer;
   fName : PathStr;
   Dir : tCompassDirection;
begin {proc ElevationSlopePlot}
   {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot in'); {$EndIf}
   MaxCount := 0;
   MaxSlope := 0;
   MinElevZ := MaxSmallInt;
   MaxElevZ := -MaxSmallInt;
   Graph := ElevSlope;
   MaxSlopeCount := 0;
   GraphList := nil;

   if (WhichDEM = 0) then begin
      StartDEM := 1;
      EndDEM := MaxDEMDataSets;
      if MDDef.ShowAspectRose then GraphList := tStringList.Create;
   end
   else begin
      StartDEM := WhichDEM;
      EndDEM := WhichDEM;
   end;

   MinMin := 9999;
   MaxMax := -9999;
   for Which := StartDEM to EndDEM do if ValidDEM(Which) then begin
      if (DEMGlb[Which].DEMheader.MinElev < MinMin) then MinMin := round(DEMGlb[Which].DEMheader.MinElev);
      if (DEMGlb[Which].DEMheader.MaxElev > MaxMax) then MaxMax := round(DEMGlb[Which].DEMheader.MaxElev);
   end;

   GetBins(MaxMax,MinMin,BinSize,StartIndex);
   if BinSize < DesiredBinSize then BinSize := DesiredBinSize;

   StartIndex := MinMin div BinSize;

   if MDDef.ShowColorLegend then begin
      CreateBitmap(LegendBMP,350,25*NumDEMDataSetsOpen+10);
      LegendBMP.Canvas.Font.Name := 'Verdana';
      LegendBMP.Canvas.Font.Size := 14;
      LegendBMP.Canvas.Font.Style := [fsbold];
      LegendY := 5;
   end;

   for Which := StartDEM to EndDEM do if (ValidDEM(Which)) then begin
      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot DEM=' + IntToStr(which)); {$EndIf}
      WhichDEM := Which;
      AspectStats.Create(Which);

      if MDDef.ShowColorLegend then begin
         LegendBMP.Canvas.Font.Color := WinGraphColors[Which];
         LegendBMP.Canvas.TextOut(15,LegendY,DEMGlb[WhichDEM].AreaName);
         inc(LegendY,25);
      end;
      New(Count[WhichDEM]);
      New(SumSquares[WhichDEM]);
      New(BinElev[WhichDEM]);
      New(Slopes[WhichDEM]);
      New(SlopeHist[WhichDEM]);
      for Dir := Low(Dir) to High(Dir) do PointCount[Dir] := 0;

      MaxDEMSlope[Which] := 0;
      for x := MinSlopeValue to MaxSlopeValue do SlopeHist[WhichDEM]^[x] := 0;

      for x := MinSlopeValue to MaxSlopeValue do begin
         Slopes[WhichDEM]^[x] := 0;
         SumSquares[WhichDEM]^[x] := 0;
         Count[WhichDEM]^[x] := -0.0000001;
      end {for x};

      SlopeTotal[WhichDEM] := 0;
      SlopeSqrTotal[WhichDEM] := 0;
      TotalDEM[WhichDEM] := 0;
      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('Setup over, DEM=' + IntToStr(which) + '  increment=' + IntToStr(MDdef.StatSampleIncr)); {$EndIf}

      StartProgress('Stats ' + DEMGlb[Which].AreaName);
      x := 1;
      while x <= (DEMGlb[WhichDEM].DEMheader.NumCol - 2) do begin
         UpDateProgressBar( x / DEMGlb[WhichDEM].DEMheader.NumCol);
         {$IfDef RecordElevationSlopePlotAll} WriteLineToDebugFile('x=' + IntToStr(x)); {$EndIf}
         j := 1;
         while j <= (DEMGlb[WhichDEM].DEMheader.NumRow - 2) do begin
            if DEMGlb[WhichDEM].GetSlopeAndAspect(x,j,SlopeAspectRec) then begin
               if (MDDef.UseSealevel) or (abs(SlopeAspectRec.z) > 0.001) then begin
                  AspectStats.AddPoint(SlopeAspectRec);
               end;
               Bin := (round(SlopeAspectRec.z) div BinSize) - StartIndex;
               Count[WhichDEM]^[Bin] := 1 + Count[WhichDEM]^[Bin];
               inc(TotalDEM[WhichDEM]);

               if MDDef.ShowSlopeFreq or MDDef.ShowElevSlope or MDDef.ShowCumSlope or MDDef.ShowElevSlopeDeg then begin
                  if (SlopeAspectRec.Slope > MaxDEMSlope[WhichDEM]) then MaxDEMSlope[WhichDEM] := SlopeAspectRec.Slope;
                  SlopeTotal[WhichDEM] := SlopeTotal[WhichDEM] + SlopeAspectRec.Slope;
                  SlopeSqrTotal[WhichDEM] := SlopeSqrTotal[WhichDEM] + sqr(SlopeAspectRec.Slope);
                  inc(PointCount[Dir]);
                  Slopes[WhichDEM]^[Bin] := Slopes[WhichDEM]^[Bin] + SlopeAspectRec.Slope;
                  SumSquares[WhichDEM]^[Bin] := SumSquares[WhichDEM]^[Bin] + sqr(SlopeAspectRec.Slope);
                  IntSlope := round(SlopeAspectRec.SlopePercent);
                  if (IntSlope <= MaxSlopeValue) then begin
                     SlopeHist[WhichDEM]^[IntSlope] := 1 + SlopeHist[WhichDEM]^[IntSlope];
                  end;
               end;
            end {if};
            inc(J,MDdef.StatSampleIncr);
         end {while j};
         inc(x,MDdef.StatSampleIncr);
      end {for x};

      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot end first loop'); {$EndIf}

      TotalCount[WhichDEM] := 0;
      for x := MinSlopeValue to MaxSlopeValue do TotalCount[WhichDEM] := TotalCount[WhichDEM] + Count[WhichDEM]^[x];
      for x := MinSlopeValue to MaxSlopeValue do begin
         SlopeHist[WhichDEM]^[x] := 100 * SlopeHist[WhichDEM]^[x] / TotalCount[WhichDEM];
         if Count[WhichDEM]^[x] > 0.5 then begin
            if Count[WhichDEM]^[x]  < 2.5 then SumSquares[WhichDEM]^[x] := 0
            else SumSquares[WhichDEM]^[x] := 100.0 * sqrt(1.0 * (SumSquares[WhichDEM]^[x] * Count[WhichDEM]^[x] - sqr(1.0 * Slopes[WhichDEM]^[x])) / (sqr(1.0 * Count[WhichDEM]^[x])));
            Slopes[WhichDEM]^[x] := 100 * Slopes[WhichDEM]^[x] / Count[WhichDEM]^[x];
            if Slopes[WhichDEM]^[x] > MaxSlope then MaxSlope := Slopes[WhichDEM]^[x];
            if 100 * Count[WhichDEM]^[x] / TotalCount[WhichDEM] > MaxCount then MaxCount := 100 * Count[WhichDEM]^[x] / TotalCount[WhichDEM];
            NumBins[WhichDEM] := x;
         end {if};
         Count[WhichDEM]^[x] := 100 * Count[WhichDEM]^[x] / TotalCount[WhichDEM];
         BinElev[WhichDEM]^[x] := MinMin + (x + 0.5) * BinSize;
      end {for x};
      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot end slope loop'); {$EndIf}

      for x := MinSlopeValue to MaxSlopeValue do begin
         if SlopeHist[WhichDEM]^[x] > MaxSlopeCount then MaxSlopeCount := SlopeHist[WhichDEM]^[x];
      end {for x};

      if MinElevZ > BinElev[WhichDEM]^[0] then MinElevZ := trunc(BinElev[WhichDEM]^[0]);
      if MaxElevZ < BinElev[WhichDEM]^[NumBins[WhichDEM]] then MaxElevZ := succ(Trunc(BinElev[WhichDEM]^[NumBins[WhichDEM]]));

      EndProgress;
      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('End loop, DEM=' + IntToStr(which)); {$EndIf}

      if MDDef.ShowAspectRose then begin
         ThisGraph := GraphPlot(AspectRose);
         if (GraphList <> Nil) then begin
            fName := NextFileNumber(MDTempDir,'rose_','.bmp');
            SaveImageAsBMP(ThisGraph.Image1,fName);
            GraphList.Add(fName);
            ThisGraph.Close;
         end;
      end;
      AspectStats.Destroy;
   end {for Which};

   if MDDef.ShowColorLegend then begin
      PetImage_form.DisplayBitmap(LegendBMP,'DEM colors');
      FreeAndNil(LegendBMP);
   end;

   if (GraphList <> Nil) then begin
      MakeBigBitmap(GraphList,'DEM Aspects');
   end;

   try
      if MDDef.ShowElevFreq then GraphPlot(ElevFreq);
      if MDDef.ShowSlopeFreq then GraphPlot(SlopeFreq);
      if MDDef.ShowElevSlope then GraphPlot(ElevSlope);
      if MDDef.ShowCumSlope then GraphPlot(CumSlope);
      if MDDef.ShowElevSlopeDeg then GraphPlot(ElevSlopeDeg);
   finally
      ShowDefaultCursor;
   end;

   for Which := StartDEM to EndDEM do if ValidDEM(Which) then begin
      Dispose(Count[Which]);
      Dispose(SumSquares[Which]);
      Dispose(BinElev[Which]);
      Dispose(Slopes[Which]);
      Dispose(SlopeHist[Which]);
   end;
   {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot Out'); {$EndIf}
end;


procedure CalculateGrainProfile(MapForm : tMapForm; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64);
const
   MaxBoxes = 10;
var
   xgrid,ygrid,Lat,Long,Distance,Bearing,CurDist,rf : float64;
   f : array[1..MaxBoxes] of file;
   Size : array[1..MaxBoxes] of integer;
   BoxesWanted : tStringList;
   err,
   i,BoxesUsed : integer;
   ThisGraph : tThisBaseGraph;
   Trend,s1s2,s2s3 : float64;
   Vals : tGraphPoint32;
begin
   DEMOptions.GetFabricOptions(DEMonMap);
   VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Distance,Bearing);
   BoxesWanted := tStringList.Create;
   BoxesWanted.LoadFromFile(ProgramRootDir + 'FabricRegionSize.txt');
   ThisGraph := TThisBaseGraph.Create(Application);
   ThisGraph.GraphDraw.LegendList := tStringList.Create;

   for i := 1 to BoxesWanted.Count do begin
      BoxesUsed := i;
      Val(BoxesWanted.Strings[pred(i)],Size[i],err);
      ThisGraph.GraphDraw.LegendList.Add(IntToStr(Size[i]) + ' m');
      if (i = MaxBoxes) then break;
   end;
   BoxesWanted.Free;

   ThisGraph.SetUpGraphForm;
   if (not BaseGraf.CreateGraphHidden) then ThisGraph.Caption := 'Topographic Grain along Profile ' + DEMGlb[DEMonMap].AreaName;
   for i := 1 to BoxesUsed do ThisGraph.OpenDataFile(f[i]);
   ThisGraph.GraphDraw.HorizLabel := 'Distance along profile (km)';
   ThisGraph.GraphDraw.VertLabel := 'Organization Strength';

   CurDist := 0;
   StartProgress('Organization');
   while (CurDist <= Distance) do begin
      UpdateProgressBar(CurDist / Distance);
      VincentyPointAtDistanceBearing(Lat1,Long1,CurDist,Bearing,Lat,Long);
      DEMGlb[DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xgrid,ygrid);

      MapForm.MapDraw.MapSymbolAtLatLongDegree(MapForm.Image1.Canvas,Lat,Long,FilledBox,3,claRed);
      for i := 1 to BoxesUsed do begin
          if DEMGlb[DEMonMap].SimplePointSSOComputations(false,round(xgrid),round(ygrid),Size[i],s1s2,s2s3,Trend,rf) then begin
             Vals[1] := 0.001 * CurDist;
             Vals[2] := s2s3;
             BlockWrite(f[i],Vals,1);
         end;
      end;
      CurDist := CurDist + MDDef.GrainSampleSeparation;
   end;
   EndProgress;
   for i := 1 to BoxesUsed do CloseFile(f[i]);
   ThisGraph.AutoScaleAndRedrawDiagram;
end;


procedure DoAnSSODiagram(CurDEM : integer; GridLimits : tGridLimits);
const
   UseMinSlope : float64 = 0;
   UseMaxSlope : float64 = 90;
var
   SSOvars : tSSOvars;
   i,ng       : Integer;
   MenuStr : ShortString;
   Results,AllResults : tStringList;
   AspName,NetName : PathStr;
   AspSL,NetSL : tstringList;
   AllDEMs : boolean;
   fName : PathStr;
begin
   {$IfDef RecordSSO} WriteLineToDebugFile('DoAnSSODiagram in, DEM=' + IntToStr(CurDEM)); {$EndIf}
   SaveBackupDefaults;
   MDDef.NetDef.NetUsed := Schmidt;
   AllDEMs := (CurDEM = 0);
   if AllDEMs then begin
      AllResults := tStringList.Create;
      AllResults.Add('DEM,AVG_SPACE,GRID_TRUE,AVG_ELEV,AVG_SLOPE,MAX_SLOPE,FABRIC,LN_S1_S2,LN_S2_S3,SHAPE,STRENGTH,ROUGH,AVG_ASPECT,STR_ASPECT');
   end;

   MDDef.NetDef.FormLegend := false;
   MDDef.NetDef.MaxContourConcentration := 10;
   ng := 0;
   if MDDef.GemorphAspectRose then AspSL := tstringList.Create;
   if MDDef.GemorphSSOPoles then NetSL := tstringList.Create;
   for i := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
      if AllDEMs or (i=CurDEM) then begin
         inc(ng);
         if MDDef.GemorphSSOPoles then begin
            NetName := MDtempDir + 'sso_net_' + IntToStr(ng) + '.bmp';
            NetSL.Add(NetName);
         end;
         if MDDef.GemorphAspectRose then begin
            AspName := MDtempDir + 'sso_asp_' + IntToStr(ng) + '.bmp';
            AspSL.Add(AspName);
         end;
         DEMGlb[i].SSOComputations(DEMGlb[i].FullDEMGridLimits,SSOvars,true,NetName,AspName);
         if AllDEMs then AllResults.Add(DEMGlb[i].AreaName + ',' + RealToString(DEMGlb[i].AverageSpace,-8,-2) + ',' + RealToString(DEMGlb[i].AverageGridTrue,-8,-2) + ',' +
            RealToString(SSOvars.AvgElev,8,2) + ',' + RealToString(SSOvars.AvgSlope,8,2) + ',' + RealToString(SSOvars.MaxSlope,8,2) + ',' +
            RealToString(SSOvars.TheDipDirs[3],8,1) + ',' +
            RealToString(SSOvars.s1s2,8,3) + ',' + RealToString(SSOvars.s2s3,8,3) + ',' + RealToString(SSOvars.Shape,6,3) + ',' + RealToString(SSOvars.Strength,6,3)+ ',' +
            RealToString(SSOvars.RoughnessFactor,8,4) + ',' + RealToString(SSOvars.AvgAspect,8,2) + ',' +  RealToString(SSOvars.AspectStrength,8,4) );
      end;
   end;
   if MDDef.GemorphSSOPoles then begin
      NetSL.Add(MDtempDir + 'net_legend.bmp');
      MakeBigBitmap(NetSL,'');
   end;
   if MDDef.GemorphAspectRose then MakeBigBitmap(AspSL,'Apect roses');

   if AllDEMs then begin
      fName := NextFileNumber(MDTempDir,'sso_results_',DefaultDBExt);
      StringList2CSVtoDB(AllResults,fName,false);
   end
   else begin
      if (SSOVars.NumPts > 0) then begin
         Results := tStringList.Create;
         Results.add(DEMGlb[CurDEM].AreaName + '         n=' +  IntToStr(SSOvars.NumPts));
         Results.add('');
         Results.add('Avg elev: '+ RealToString(SSOvars.AvgElev,8,2) + ' m  std dev=' + RealToString(SSOvars.StdDevElev,8,2) );
         Results.add('Avg slope:'+ RealToString(SSOvars.AvgSlope,8,2) + '%   std dev=' + RealToString(SSOvars.StdDevSlope,8,2) );
         Results.add('Max slope:'+ RealToString(SSOvars.MaxSlope,8,2) + '%');
         Results.add('Log Ratios:');
         Results.add('   ln(S1/S2):'+RealToString(SSOvars.s1s2,8,2));
         Results.add('   ln(S2/S3):'+RealToString(SSOvars.s2s3,8,2));
         Results.add('Shape Indicator:'+RealToString(SSOvars.Shape,6,2));
         Results.add('Strength Indicator:'+RealToString(SSOvars.Strength,6,2));
         Results.add('');
         Results.add('Eigen vectors:');
         MenuStr := '';
         for i := 1 to 3 do begin
            Results.add('   V' + IntToStr(i) +  RealToString(SSOvars.TheDips[i],10,4) +  '? toward' + RealToString(SSOvars.TheDipDirs[i],8,1) +  '?');
         end {for i};
         Results.add('Queen''s aspect ratio:' + RealToString(SSOvars.QueensAspect,8,3));
         Petmar.DisplayAndPurgeStringList(Results,'SSO results ' + DEMGlb[CurDEM].AreaName);
      end
      else MessageToContinue('No results');
   end;
   RestoreBackupDefaults;
   {$IfDef RecordSSO} WriteLineToDebugFile('DoAnSSODiagram out'); {$EndIf}
end;


procedure ElevMomentReport(aTitle : shortstring; Memo1 : tMemo; SamplingCheck : boolean; GridLimits: tGridLimits; CurDEM : integer = 0);
var
   LegendFiles,ElevFiles,SlopeFiles,PlanCurvFiles,ProfCurvFiles : tStringList;
   DEMsDone,OnLine : integer;
   GridForm : TGridForm;
   MaxSlope,MinElev,MaxElev : float64;
   ElevDist,SlopeDist,RufDist : tStringList;

      procedure MomentReportForDEM(CurDEM : integer);
      label
         Done;
      var
         Col,Row,Incr  : integer;
         Ruff1 : float32;
         Slope,SlopeCurvature,
         PlanCurvature,crossc,MaxCurve,MinCurve : float64;
         MomentVar : tMomentVar;
         zvs,zvs2 : ^Petmath.bfarray32;


         function MomentResults : shortstring;
         begin
            Result := DEMGlb[CurDEM].AreaName + MomentResultsToString(MomentVar);
         end;


      begin
         {$IfDef RecordElevMoment} WriteLineToDebugFile('Start DEM=' + IntToStr(CurDEM) + '  ' + DEMGlb[CurDEM].AreaName); {$EndIf}
         New(zvs);
         inc(DEMsDone);
         ShowHourglassCursor;
         OnLine := 3;
         if (Memo1 <> Nil) then Memo1.Lines.Add(DEMGlb[CurDEM].AreaName);
         if MDDef.ElevMoments then begin
            {$IfDef RecordElevMoment} WriteLineToDebugFile('Start elev'); {$EndIf}
            if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start elev');
            DEMGlb[CurDEM].ElevationMomentsWithArray({DEMGlb[CurDEM].FullDEM}GridLimits,MomentVar,zvs^);
            MinElev := MomentVar.MinZ;
            MaxElev := MomentVar.MaxZ;
            MomentsToStringGrid(GridForm.StringGrid1,OnLine,DEMsDone,'Elevation',MomentVar);
            inc(Online,11);
            if MDDef.GraphsOfMoments then ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
            ElevDist.Add(MomentResults);
         end;

         if MDDef.SlopeMoments then begin
           {$IfDef RecordElevMoment} WriteLineToDebugFile('Start slope'); {$EndIf}
           if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start slope');
           DEMGlb[CurDEM].SlopeMomentsWithArray({DEMGlb[CurDEM].FullDEM}GridLimits, MomentVar,zvs^);
           MomentsToStringGrid(GridForm.StringGrid1,OnLine,DEMsDone,'Slope',MomentVar);
           inc(Online,11);
           if MomentVar.MaxZ > MaxSlope then MaxSlope := MomentVar.MaxZ;
           if MDDef.GraphsOfMoments then SlopeFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
           Dispose(zvs);
           SlopeDist.Add(MomentResults);
         end;

         if MDDef.RoughnessMoments then begin
           {$IfDef RecordElevMoment} WriteLineToDebugFile('Start roughness'); {$EndIf}
           if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start roughness');
           New(zvs);
           MomentVar.NPts := 0;
           DEMGlb[CurDEM].RoughnessMomentsWithArray({DEMGlb[CurDEM].FullDEM}GridLimits, MomentVar,zvs^);
           MomentsToStringGrid(GridForm.StringGrid1,OnLine,DEMsDone,'Roughness',MomentVar);
           inc(Online,11);
           RufDist.Add(MomentResults);
         end;


         Incr := DEMGlb[CurDEM].GetSamplingSize(GridLimits);
         Incr := Incr * MDDef.StatSampleIncr;
         GridForm.StringGrid1.Cells[DEMsDone,2] := IntToStr(Incr);
         if MDDef.PlanCurvMoments or MDDef.SlopeCurvMoments then begin
             {$IfDef RecordElevMoment} WriteLineToDebugFile('Start curvature'); {$EndIf}
             MomentVar.Npts := 0;
             New(zvs2);
             StartProgress('Curvature');
             Col := GridLimits.XGridLow;
             while (Col <= GridLimits.XGridHigh) do begin
                if (Col mod 25 = 0) then UpdateProgressBar(Col/DEMGlb[CurDEM].DEMheader.NumCol);
                Row := GridLimits.YGridLow;
                while (Row < GridLimits.YGridHigh) do begin
                   if DEMGlb[CurDEM].GetEvansParams(Col,Row,1,Slope,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve) then begin
                      zvs^[MomentVar.Npts] := SlopeCurvature;
                      zvs2^[MomentVar.Npts] := PlanCurvature;
                      inc(MomentVar.NPts);
                   end;
                   inc(Row,Incr);
                end;
                inc(Col,Incr);
             end;
             EndProgress;

             if MDDef.SlopeCurvMoments then begin
                MomentReport('Slope curvature',zvs^,MomentVar.npts,'',GridForm.StringGrid1,OnLine,DEMsDone);
                if MDDef.GraphsOfMoments then ProfCurvFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
                inc(Online,8);
             end;
             if MDDef.PlanCurvMoments then begin
                MomentReport('Plan curvature',zvs2^,MomentVar.npts,'',GridForm.StringGrid1,OnLine,DEMsDone);
                if MDDef.GraphsOfMoments then PlanCurvFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs2^));
                inc(Online,8);
             end;
             Dispose(zvs2);
         end;
         Dispose(zvs);

         GridForm.StringGrid1.Cells[DEMsDone,0] := DEMGlb[CurDEM].AreaName;
         GridForm.StringGrid1.Cells[DEMsDone,1] := RealToString(DEMGlb[CurDEM].AverageSpace,-12,2);
         GridForm.StringGrid1.Cells[0,0] := 'DEM';
         GridForm.StringGrid1.Cells[0,1] := 'Avg Grid Space';
         GridForm.StringGrid1.Cells[0,2] := 'Sampling';
         ShowDefaultCursor;
         GridForm.StringGrid1.ColCount:= succ(DEMsDone);
         GridForm.StringGrid1.RowCount := OnLine;
         GridForm.Caption := aTitle + ' Moment report';
      end;


      procedure DoOne(Incr : integer);
      begin
         if Memo1 <> Nil then Memo1.Lines.Add(TimeToStr(Now) + ' start ' + IntToStr(Incr));
         LegendFiles.Add('Sampling=' + IntToStr(Incr));
         MDDef.StatSampleIncr := Incr;
         MomentReportForDEM(CurDEM);
      end;

var
   j : integer;
begin
   {$IfDef RecordElevMoment} WriteLineToDebugFile('ElevMomentReport in'); {$EndIf}
   SaveBackupDefaults;
   if MDDef.GraphsOfMoments then begin
      ElevFiles := tStringList.Create;
      SlopeFiles := tStringList.Create;
      PlanCurvFiles := tStringList.Create;
      ProfCurvFiles := tStringList.Create;
      LegendFiles := tStringList.Create;
   end;

   GridForm := TGridForm.Create(Application);

   DEMsDone := 0;
   MaxSlope := -1;
   MaxElev := -99999;
   MinElev := 9999999;

   if (CurDEM = 0) then begin
     SlopeDist := tStringList.Create;
     SlopeDist.Add(MomentStr);
     ElevDist := tStringList.Create;
     ElevDist.Add(MomentStr);
     RufDist := tStringList.Create;
     RufDist.Add(MomentStr);

      for j := 1 to MaxDEMDataSets do if ValidDEM(j) then begin
         {$IfDef RecordElevMoment} WriteLineToDebugFile('Start DEM=' + IntToStr(j)); {$EndIf}
         GridLimits := DEMGlb[j].SelectionMap.MapDraw.MapAreaDEMGridLimits;
         MomentReportForDEM(j);
         if MDDef.GraphsOfMoments then LegendFiles.Add(DEMGlb[j].AreaName);
      end;

      if RufDist.Count > 1 then RufDist.SaveToFile(NextFileNumber(MDTempDir,'roughness_raw_','.csv'));
      if SlopeDist.Count > 1 then SlopeDist.SaveToFile(NextFileNumber(MDTempDir,'slope_raw_','.csv'));
      if ElevDist.Count > 1 then ElevDist.SaveToFile(NextFileNumber(MDTempDir,'elev_raw_','.csv'));

      if RufDist.Count > 1 then StringList2CSVtoDB(RufDist,NextFileNumber(MDTempDir,'roughness','.dbf')) else RufDist.Free;
      if SlopeDist.Count > 1 then StringList2CSVtoDB(SlopeDist,NextFileNumber(MDTempDir,'slope','.dbf')) else SlopeDist.Free;
      if ElevDist.Count > 1 then StringList2CSVtoDB(ElevDist,NextFileNumber(MDTempDir,'elev','.dbf')) else ElevDist.Free;

      if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' Processing over');

     {$IfDef RecordElevMoment} WriteLineToDebugFile('DEMs complete'); {$EndIf}
   end
   else begin
      if ValidDEM(CurDEM) then begin
         if SamplingCheck then begin
            MDDef.CountHistograms := false;
            DoOne(1);
            DoOne(2);
            DoOne(3);
            DoOne(4);
            DoOne(5);
            DoOne(7);
            DoOne(10);
         end
         else MomentReportForDEM(CurDEM);
      end;
   end;

   if MDDef.GraphsOfMoments then begin
      {$IfDef RecordElevMoment} WriteLineToDebugFile('Start histograms'); {$EndIf}
      if (ElevFiles.Count > 0) then CreateMultipleHistograms(MDDef.CountHistograms,ElevFiles,LegendFiles,'Elevation/grid','Elevation/grid distribution',-99,MinElev,MaxElev,0.5);
      if (SlopeFiles.Count > 0) then CreateMultipleHistograms(MDDef.CountHistograms,SlopeFiles,LegendFiles,'Slope (%)','Slope distribution',-99,0,Trunc(MaxSlope + 0.99),0.5);
      if (PlanCurvFiles.Count > 0) then CreateMultipleHistograms(MDDef.CountHistograms,PlanCurvFiles,LegendFiles,'Plan curvature','Plan curvature distribution',-99,-10,10,0.1);
      if (ProfCurvFiles.Count > 0) then CreateMultipleHistograms(MDDef.CountHistograms,ProfCurvFiles,LegendFiles,'Profile curvature','Profile curvature distribution',-99,-10,10,0.1);
      {$IfDef RecordElevMoment} WriteLineToDebugFile('Done histograms'); {$EndIf}
      SlopeFiles.Free;
      PlanCurvFiles.Free;
      ProfCurvFiles.Free;
      LegendFiles.Free;
      ElevFiles.Free;
   end;

   RestoreBackupDefaults;
   {$IfDef RecordElevMoment} WriteLineToDebugFile('ElevMomentReport out'); {$EndIf}
end;


procedure AspectDistributionBySlope(WhichDEM : Integer; GridLimits : tGridLimits);
var
   Col,Row,i,j,TotPts  : integer;
   ThisGraph,ThisGraph2 : TThisBaseGraph;
   RoseGraph : array[1..6] of TThisBaseGraph;
   rfile,rfile2 : file;
   v : tGraphPoint32;
   theFiles : tstringlist;
   MenuStr : shortstring;
   fName : PathStr;
   SlopeAspectRec : tSlopeAspectRec;
   AspectStats : array[1..6] of tAspectStats;


   procedure SetUpGraph(var ThisGraph : tThisBaseGraph; VertLabel : shortstring);
   var
      i : integer;
   begin
      ThisGraph := TThisBaseGraph.Create(Application);
      ThisGraph.SetUpGraphForm;
      ThisGraph.Caption := 'Aspect distribution ' + DEMGlb[WhichDEM].AreaName;
      ThisGraph.GraphDraw.HorizLabel := 'Aspect direction';
      ThisGraph.GraphDraw.VertLabel := VertLabel;
      ThisGraph.GraphDraw.LegendList := tStringList.Create;
      for i := 1 to 6 do ThisGraph.GraphDraw.FileColors256[i] := ConvertTColorToPlatformColor(WinGraphColors[i]);
   end;

   procedure FinishGraph(var ThisGraph : tThisBaseGraph);
   begin
      ThisGraph.AutoScaleAndRedrawDiagram;
      ThisGraph.GraphDraw.SetShowAllLines(true);
      ThisGraph.GraphDraw.SetShowAllPoints(false);
      ThisGraph.GraphDraw.MaxHorizAxis := 360;
      ThisGraph.RedrawDiagram11Click(Nil);
   end;


begin
   theFiles := tstringlist.Create;
   for i := 1 to 6 do AspectStats[i].Create(WhichDEM);
   TotPts := 0;
   StartProgress('Aspects');
   for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
      if (Row mod 25 = 0) then UpdateProgressBar((Row-GridLimits.YGridLow)/(GridLimits.YGridHigh-GridLimits.YGridLow));
      for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          if DEMGlb[WhichDEM].GetSlopeAndAspect(Col,Row,SlopeAspectRec) then begin
             if (MDDef.UseSealevel) or (abs(SlopeAspectRec.z) > 0.001) then begin
                if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[1] then AspectStats[1].AddPoint(SlopeAspectRec)
                else if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[2] then AspectStats[2].AddPoint(SlopeAspectRec)
                else if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[3] then AspectStats[3].AddPoint(SlopeAspectRec)
                else if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[4] then AspectStats[4].AddPoint(SlopeAspectRec)
                else AspectStats[5].AddPoint(SlopeAspectRec);
                AspectStats[6].AddPoint(SlopeAspectRec);
                inc(TotPts);
             end;
          end;
      end;
   end;
   EndProgress;

   SetUpGraph(ThisGraph,'Concentration of values');
   SetUpGraph(ThisGraph2,'Number of values');
   for i := 1 to 6 do begin
     ThisGraph.OpenDataFile(rfile);
     ThisGraph2.OpenDataFile(rfile2);
     for j := 0 to 359 do begin
        v[1] := j;
        v[2] := 360 * AspectStats[i].AspectFreqValsTrue[j] / AspectStats[i].NPts;
        BlockWrite(rfile,v,1);
        v[2] := AspectStats[i].AspectFreqValsTrue[j];
        BlockWrite(rfile2,v,1);
     end;
     CloseFile(rfile);
     CloseFile(rfile2);

     case i of
        1 : MenuStr := '0-' + RealToString(MdDef.GeomorphSlopeCut[1],-8,-2) + '%';
        2 : MenuStr := RealToString(MdDef.GeomorphSlopeCut[1],-8,-2) + '-' + RealToString(MdDef.GeomorphSlopeCut[2],-8,-2) + '%';
        3 : MenuStr := RealToString(MdDef.GeomorphSlopeCut[2],-8,-2) + '-' + RealToString(MdDef.GeomorphSlopeCut[3],-8,-2) + '%';
        4 : MenuStr := RealToString(MdDef.GeomorphSlopeCut[3],-8,-2) + '-' + RealToString(MdDef.GeomorphSlopeCut[4],-8,-2) + '%';
        5 : MenuStr := '>' + RealToString(MdDef.GeomorphSlopeCut[4],-8,-2) + '%';
        6 : MenuStr := 'all points';
     end;
     MenuStr := ' Slope ' + MenuStr;
     ThisGraph.GraphDraw.LegendList.Add(MenuStr);
     ThisGraph2.GraphDraw.LegendList.Add(MenuStr);
     RoseGraph[i] := AspectStats[i].CreateRose(MenuStr);
     fName := MDtempDir + 'aspect_' + IntToStr(i) + '.bmp';
     SaveImageAsBMP(RoseGraph[i].Image1,fName);
     TheFiles.Add(fName);
     RoseGraph[i].Close;
     RoseGraph[i].Free;
     AspectStats[i].Destroy;
  end;

  FinishGraph(ThisGraph);
  FinishGraph(ThisGraph2);

  MakeBigBitmap(theFiles,'Aspect: ' + DEMGlb[WhichDEM].AreaName);
end;


procedure AspectDistributionByAlgorithm(WhichDEM : Integer; GridLimits : tGridLimits);
var
   Diagonal,Col,Row,i,j,k : integer;
   QueensAspect : float64;
   AspectFreq : ^CircleFreqType;
   RoseGraph : TThisBaseGraph;
   fName : PathStr;
   TheBitmaps : tStringList;
   Method : byte;
   SlopeAspectRec : tSlopeAspectRec;
begin
   SaveBackupDefaults;
   TheBitmaps := tStringList.Create;
   i := 0;
   for Method:= FirstSlopeMethod to LastSlopeMethod do begin
      MDdef.SlopeAlg := Method;
      New(AspectFreq);
      for j := 0 to 360 do AspectFreq^[j] := 0;
      StartProgress('Aspects');
      for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
         if (Col mod 25 = 0) then UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
         for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
             if DEMGlb[WhichDEM].GetSlopeAndAspect(Col,Row,SlopeAspectRec) then begin
                 if (not MDDef.UseSealevel) or (abs(SlopeAspectRec.z) > 0.001) then inc(AspectFreq^[round(SlopeAspectRec.AspectDir)]);
             end;
         end;
      end;
      EndProgress;
      AspectFreq^[0] := AspectFreq^[0] + AspectFreq^[360];
      AspectFreq^[360] := 0;

      RoseGraph := TThisBaseGraph.Create(Application);
      RoseGraph.ClientHeight := 400;
      RoseGraph.ClientWidth := 400;
      RoseGraph.RoseColor := WinGraphColors[i];
      RoseGraph.DrawAspectRose(AspectFreq^,SlopeMethodName(MDdef.SlopeAlg));

      k := 0;
      for j := 0 to 360 do inc(k,AspectFreq^[j]);
      if (k > 50) then begin
         Diagonal := round(arctan(DEMGlb[WhichDEM].AverageYSpace/DEMGlb[WhichDEM].AverageXSpace) / DegToRad);
         QueensAspect := (AspectFreq^[0] + AspectFreq^[Diagonal] + AspectFreq^[90] + AspectFreq^[180-Diagonal] + AspectFreq^[180] + AspectFreq^[180+Diagonal] + AspectFreq^[270] + AspectFreq^[360-Diagonal]) / (k) * 45;
         RoseGraph.Image1.Canvas.TextOut(5,RoseGraph.Image1.ClientHeight-45,'Queens aspect=' + RealToString(QueensAspect,-8,-2));
      end;
      Dispose(AspectFreq);
      fName := MDtempDir + 'aspect_rose_' + IntToStr(i) + '.bmp';
      SaveImageAsBMP(RoseGraph.Image1,fName);
      TheBitmaps.Add(fName);
      RoseGraph.Close;
      RoseGraph.Free;
      RoseGraph := Nil;
      inc(i);
   end;
   MakeBigBitmap(theBitmaps, 'Aspect: ' + DEMGlb[WhichDEM].AreaName);
   RestoreBackupDefaults;
end;


procedure DoElevationHistograms;
var
   CurDEM,j : integer;
begin
   DEM_Hist_Opts.SetHistogramOptions;
   if MDDef.GeomorphAllDEMs then begin
      for j := 1 to MaxDEMDataSets do if (DEMGlb[j] <> Nil) then SingleDEMHistogram(j);
   end
   else begin
      GetDEM(CurDEM);
      SingleDEMHistogram(CurDEM);
   end;
end;


procedure GridCorrelationMatrix(Incr : integer = 1);
{$IfDef ExGeoStats}
begin
{$Else}
type
  tCorrs = array[1..MaxDEMDataSets,1..MaxDEMDataSets] of float64;
var
  r,covar : float64;
  i: Integer;
  j: Integer;
  Findings : tStringList;
  MenuStr : ansistring;
  fName : PathStr;
  Corrs : ^tCorrs;
begin
//would be good to thread
   MenuStr := 'DEM/Grid';
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         MenuStr := MenuStr + ',' + DEMGlb[i].AreaName;
      end;
   end;
   Findings := tStringList.Create;
   Findings.Add(MenuStr);
   New(Corrs);
   StartProgress('Grid correlations');
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         UpdateProgressBar(i/MaxDEMDataSets);
         for j := i to MaxDEMDataSets do begin
            if ValidDEM(j) then begin
               if (i = j) then begin
                  Corrs^[i,j] := 1;
               end
               else begin
                  StatsFromTwoGrids(i,J,r,covar,Incr);
                  Corrs^[i,j] := r;
                  Corrs^[j,i] := r;
               end;
            end;
         end;
      end;
   end;

   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         UpdateProgressBar(i/NumDEMDataSetsOpen);
         MenuStr := DEMGlb[i].AreaName;
         for j := 1 to MaxDEMDataSets do begin
            if ValidDEM(j) then begin
               MenuStr := MenuStr + ',' + RealToString(Corrs^[i,j],-18,8);
            end;
         end;
         Findings.Add(MenuStr);
      end;
   end;

   EndProgress;
   Dispose(Corrs);
   fName := MDTempDir + 'grid_r_matrix.csv';
   Findings.SaveToFile(fName);
   Findings.Free;
   DEMStringGrid.OpenCorrelationMatrix('Correlation',fName);
{$EndIf}
end;



procedure SingleDEMHistogram(WhichDEM : integer; Quick : boolean = false);
const
   HistRange = 64000;
type
   tHistArray = array[-HistRange..HistRange] of int64;
var
   i,j,x,y,theDB,NumBins : integer;
   TotalPts : int64;
   MinBin,MaxBin,PC,CumPC : float64;
   z : float32;
   fName : PathStr;
   TStr : shortstring;
   GridLimits : tGridLimits;
   Results : tStringList;
   Hist : ^tHistArray;
begin
   if Not ValidDEM(WhichDEM) then exit;
   
   {$IfDef RecordHistogram} WriteLineToDebugFile('SingleDEMHistogram for ' + DEMGlb[WhichDEM].AreaName); {$EndIf}
   if (DEMGlb[WhichDEM].SelectionMap = nil) then GridLimits := DEMGlb[WhichDEM].FullDEMGridLimits

   else GridLimits := DEMGlb[WhichDEM].SelectionMap.GridLimitsForGeomorphOps;
   New(Hist);
   for I := -HistRange to HistRange do Hist^[i] := 0;
   TotalPts := 0;
   x := GridLimits.XGridLow;
   while (x <= GridLimits.XGridHigh) do begin
       y := GridLimits.YGridLow;
       while (y <= GridLimits.YGridHigh) do begin
           if DEMGlb[WhichDEM].GetElevMeters(x,y,z) then begin
              inc(Hist^[round(z / MDDef.HistBinSize)]);
              inc(TotalPts);
           end;
           inc(y,MDDef.StatSampleIncr);
       end {while y};
       inc(x,MDDef.StatSampleIncr);
   end {while x};
   {$IfDef RecordHistogram} WriteLineToDebugFile('Total done'); {$EndIf}
   NumBins := 0;
   MinBin := 99e38;
   for I := -HistRange to HistRange do begin
      if Hist^[i] > 0 then begin
         inc(NumBins);
         if MinBin > 98e38 then MinBin := MDDef.HistBinSize * i;
         MaxBin := MDDef.HistBinSize * i;
      end;
   end;
   Results := tStringList.Create;
   Results.Add('Z,NPTS,PERCENTAGE,CUM_PC,CONCENT,PROP_HGT,PROP_AREA,REC_ID');
   CumPC := 0;
   j := 0;
   for I := -HistRange to HistRange do begin
      if Hist^[i] > 0 then begin
         pc := 100 * Hist^[i] / TotalPts;
         inc(j);
         CumPc := CumPC + PC;
         Results.Add(RealToString(MDDef.HistBinSize * i,-18,-4) + ',' + IntToStr(Hist^[i]) + ',' + RealToString(pc,-12,-5) +
            ',' + RealToString(CumPC,-12,-5)  + ',' + RealToString(Hist^[i] / (TotalPts / NumBins),-12,-4) +
            ',' + RealToString(((MDDef.HistBinSize * i) -MinBin)/(MaxBin - MinBin),-12,-5) +  ',' + RealToString(1 - 0.01 * CumPC,-12,-5) + ',' + IntToStr(j) );
      end;
   end;
   Dispose(Hist);

    fName := Petmar.NextFileNumber(MDTempDir,DEMGlb[WhichDEM].AreaName + '_Hist_',DefaultDBExt);
    {$IfDef RecordHistogram} WriteLineToDebugFile('Convert and load table');    {$EndIf}

    StringList2CSVtoDB(Results,fName,true);
    theDB := DEMGlb[WhichDEM].SelectionMap.LoadDataBaseFile(fName);

   if DEMGlb[WhichDEM].DEMheader.ElevUnits = euImagery then TStr := 'DNs '
   else TStr := 'Elevation ';
   if Quick or MDDef.ShowRegularHistogram then begin
      if MDDef.CountHistograms then GISDB[theDB].CreateScatterGram('CONCENT','Z',true,DEMGlb[WhichDEM].AreaName +  ' ' + TStr + ' Histogram',
           'Concentration (Fraction of Uniform)',  TStr + '(' + LabelElevUnits[DEMGlb[WhichDEM].DEMheader.ElevUnits]  + ')')
      else begin
         if (DEMGlb[WhichDEM].DEMheader.ElevUnits = euImagery) then
              GISDB[theDB].CreateScatterGram('Z','NPTS',true,DEMGlb[WhichDEM].AreaName +  ' ' + TStr + 'Histogram', TStr + '(' + LabelElevUnits[DEMGlb[WhichDEM].DEMheader.ElevUnits]  + ')','Number of Points')
         else GISDB[theDB].CreateScatterGram('NPTS','Z',true,DEMGlb[WhichDEM].AreaName +  ' ' + TStr + 'Histogram','Number of Points', TStr + '(' + LabelElevUnits[DEMGlb[WhichDEM].DEMheader.ElevUnits]  + ')');
      end;
   end;

   if Quick then begin
      CloseAndNilNumberedDB(theDB);
   end
   else begin
      if MDDef.ShowCumulativeHistogram then GISDB[theDB].CreateScatterGram('CUM_PC','Z',true,DEMGlb[WhichDEM].AreaName +  ' Cumulative Elevation Distribution',
          'Cumulative Percentage', 'Elevation (' + LabelElevUnits[DEMGlb[WhichDEM].DEMheader.ElevUnits]  + ')');

      if MDDef.ShowStrahlerHistogram then GISDB[theDB].CreateScatterGram('PROP_AREA','PROP_HGT',true,DEMGlb[WhichDEM].AreaName +  ' Strahler Elevation Distribution',
       'Proportion of Area', 'Proportion of Basin Height');

       if MDDef.ShowNormalHistogram then begin
          GISDB[theDB].CreateScatterGram('Z','CUM_PC',true,DEMGlb[WhichDEM].AreaName +  ' Cumulative ' + TStr + 'Distribution',
            TStr + '(' + LabelElevUnits[DEMGlb[WhichDEM].DEMheader.ElevUnits]  + ')','Cumulative Percentage',true);
       end;

       if MDDef.ShowHistogramText then begin
          Results := tStringList.Create;
          Results.Add(DEMGlb[WhichDEM].AreaName + '  ' + TStr);
          Results.Add('Range: '+ RealToString(DEMGlb[WhichDEM].DEMheader.MinElev,-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].DEMheader.MaxElev,-8,-2));
          Results.Add('Range (0.1 to 99.9%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(0.1),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(99.9),-8,-2));
          Results.Add('Range (0.2 to 99.8%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(0.2),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(99.8),-8,-2));
          Results.Add('Range (0.5 to 99.5%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(0.5),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(99.5),-8,-2));
          Results.Add('Range (1.0 to 99.0%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(1.0),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(99.0),-8,-2));
          Results.Add('Range (2.0 to 98.0%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(2.0),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(98.0),-8,-2));
          Results.Add('Range (5.0 to 95.0%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(5.0),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(95.0),-8,-2));
          PETMAR.DisplayAndPurgeStringList(Results,'Histogram results');
       end;
   end;
  {$IfDef RecordHistogram} WriteLineToDebugFile('SingleDEMHistogram out'); {$EndIf}
end;

{$EndIf}


{$IfDef ExGeoStats}
{$Else}
procedure FastFourierTransform(WantedDEM : integer; GridLimits: tGridLimits; var SlopeByColumn,SlopeByRow : float32; CloseGraphs : boolean = false);
var
   FFTFile : file;
   FName   : PathStr;
   Size,x,y,Good,Bad : integer;
   z : float32;
   a,r : float32;
   Vals    : array[0..MaxElevArraySize] of float32;

         procedure SetUpAnalysis(NumRecs : integer);
         var
            x : integer;
         begin
            {$IfDef RecordFFT} WriteLineToDebugFile('SetUpAnalysis in');     {$EndIf}
            Size := 2;
            While (Size < NumRecs) do Size := Size * 2;
            assignFile(FFTfile,FName);
            rewrite(FFTFile,Size * SizeOf(float64));
            for x := 0 to MaxElevArraySize do Vals[x] := 0;
            Good := 0;
            Bad := 0;
            if ShowSatProgress then StartProgress('Set up FFT');
         end;

         procedure SetUpGraph(Title : ShortString; AverageSpace : float32; var Slope : float32);
         begin
            CloseFile(FFTFile);
            EndProgress;
            Slope := -9999;
            {$IfDef RecordFFT} WriteLineToDebugFile('FFT set up graph (good/bad): ' + IntToStr(Good) + '/' + IntToStr(Bad));   {$EndIf}
            if (Good > 3 * Bad) then begin
               FFTGraph := TFFTGraph.Create(Application);
               with FFTGraph,GraphDraw do begin
                  ACaption :=  'FFT Power spectrum: ' + Title + DEMGlb[WantedDEM].AreaName;
                  if (not BaseGraf.CreateGraphHidden) then FFTGraph.Caption := ACaption;
                  FFTFileName := FName;
                  TotalNumberPoints := Size;
                  Double := false;
                  PowerTables := false;
                  ShowProgress := false;
                  SkipDrawing := CloseGraphs;
                  BinTime := AverageSpace;
                  BinUnits := ' (' + ElevUnitsAre[DEMGlb[WantedDEM].DEMheader.ElevUnits] + ')';
               end;
               if FFTGraph.FastFourierTransform and (FFTGraph.GraphDraw.DataFilesPlotted.Count > 0) then FFTGraph.GetSlope(false,a,Slope,r);
               if CloseGraphs then FFTGraph.Close;
            end;
         end;


begin
   {$IfDef RecordFFT} WriteLineToDebugFile('FastFourierTransform in, WantedDEM=' + IntToStr(WantedDEM)); {$EndIf}
   fName := MDTempDir + 'tempfft1.zzz';
   SetUpAnalysis(succ(GridLimits.XGridHigh-GridLimits.XGridLow));
   y := GridLimits.YGridLow;
   while y < GridLimits.YGridHigh do begin
      if ShowSatProgress then UpdateProgressBar(y/DEMGlb[WantedDEM].DEMheader.NumRow);
      for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
         if DEMGlb[WantedDEM].GetElevMeters(x,y,z) then begin
            Vals[x-GridLimits.XGridLow] := z;
            inc(Good);
         end
         else inc(Bad);
      end;
      BlockWrite(fftFile,Vals,1);
      inc(Y,MDdef.StatSampleIncr);
   end;
   SetUpGraph(' by row ',DEMGlb[WantedDEM].AverageXSpace,SlopeByColumn);

   fName := MDTempDir + 'tempfft2.zzz';
   SetUpAnalysis(succ(GridLimits.YGridHigh-GridLimits.YGridLow));
   x := GridLimits.XGridLow;
   while (x < GridLimits.XGridHigh) do  begin
      if ShowSatProgress then UpdateProgressBar(x/DEMGlb[WantedDEM].DEMheader.NumCol);
      for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
         if DEMGlb[WantedDEM].GetElevMeters(x,y,z) then begin
            Vals[y-GridLimits.YGridLow] := z;
            inc(Good);
         end
         else inc(Bad);
      end;
      BlockWrite(fftFile,Vals,1);
      inc(x,MDdef.StatSampleIncr);
   end;
   SetUpGraph(' by column ',DEMGlb[WantedDEM].AverageYSpace,SlopeByRow);
end;


procedure SemiVariogramOptions;
begin
   VariogramOptions := TVariogramOptions.Create(Application);
   with VariogramOptions,MDDef.VariogramOptionsRecord do begin
      CheckBox1.Checked := LogLog;
      CheckBox2.Checked := SemiVar;
      CheckBox3.Checked := DoGraph;
      CheckBox4.Checked := DoGamma;
      CheckBox5.Checked := ShowTextOutput;
      CheckBox6.Checked := OldMethod;
      CheckBox7.Checked := DoSlopes;
      Edit1.Text := IntToStr(Skip);
      Edit2.Text := IntToStr(GraphSkip);
      Edit3.Text := IntToStr(DistanceOut);
      Edit4.Text := IntToStr(PointsRequired);
      ShowModal;
      LogLog := CheckBox1.Checked;
      SemiVar := CheckBox2.Checked;
      CheckEditString(Edit1.Text,Skip);
      CheckEditString(Edit2.Text,GraphSkip);
      CheckEditString(Edit3.Text,DistanceOut);
      CheckEditString(Edit4.Text,PointsRequired);
      DoGraph := CheckBox3.Checked;
      DoGamma := CheckBox4.Checked;
      ShowTextOutput := CheckBox5.Checked;
      OldMethod := CheckBox6.Checked;
      DoSlopes := CheckBox7.Checked;
   end;
end;

procedure SemiVariogram(DEMtoUse : integer; GridLimits: tGridLimits);
begin
   {$IfDef RecordGridScatterGram} WriteLineToDebugFile('SemiVariogram for DEM=' + intToStr(DEMtoUse) + '  ' + DEMGlb[DEMToUse].AreaName); {$EndIf}
   DEMGlb[DEMToUse].ComputeVariogram(GridLimits,false);
   {$IfDef RecordGridScatterGram} WriteLineToDebugFile('SemiVariogram for DEM=' + intToStr(DEMtoUse) + ' out'); {$EndIf}
end {proc SemiVariogram};
{$EndIf}


function GridScatterGram(FullGrid : boolean; DEM1 : integer = 0; DEM2 : integer = 0) : TThisBaseGraph;
var
   Incr,Col,Row,NPts,Prog : integer;
   Lat,Long,XGrid,YGrid : float64;
   rFile : file;
   v : tFloatPoint;
   IdenticalGrids : boolean;
   GridLimits : tGridLimits;
begin
   if (DEM1 = 0) and (DEM2 = 0) then IdenticalGrids := GetTwoCompatibleGrids('DEM1=x axis, DEM2= y axis',false,DEM1,DEM2,false,true)
   else IdenticalGrids := DEMGlb[DEM1].SecondGridIdentical(DEM2);
   {$IfDef RecordGridScatterGram}
      WriteLineToDebugFile('Two grid scattergram');
      WriteLineToDebugFile('  DEM 1:' + DEMGlb[DEM1].AreaName + '  ' + DEMGlb[DEM1].KeyDEMParams);
      WriteLineToDebugFile('  DEM 2:' + DEMGlb[DEM2].AreaName + '  ' + DEMGlb[DEM2].KeyDEMParams);
      WriteLineToDebugFile('  ll corner :' + RealToString(DEMGlb[DEM2].Headrecs.hdfSWCornerx,-12,-2) + '   ' + RealToString(DEMGlb[DEM2].Headrecs.hdfSWCornery,-12,-2) );
   {$EndIf}

   if FullGrid then GridLimits := DEMGlb[DEM1].FullDEMGridLimits
   else GridLimits := DEMGlb[DEM1].SelectionMap.MapDraw.MapAreaDEMGridLimits;

   {$IfDef RecordGridScatterGram} WriteLineToDebugFile('Grid from DEM 1: ' + GridLimitsToString(GridLimits)); {$EndIf}
   Result := TThisBaseGraph.Create(Application);
   Result.SetUpGraphForm;
   Result.GraphDraw.AutoPointDensity := true;
   Result.Caption := 'Two grid scattergram';
   if DEMGlb[DEM1].ShortName = '' then Result.GraphDraw.HorizLabel := RemoveUnderscores(DEMGlb[DEM1].AreaName)
   else Result.GraphDraw.HorizLabel := RemoveUnderscores(DEMGlb[DEM1].ShortName);
   if DEMGlb[DEM2].ShortName = '' then Result.GraphDraw.VertLabel := RemoveUnderscores(DEMGlb[DEM2].AreaName)
   else Result.GraphDraw.VertLabel := RemoveUnderscores(DEMGlb[DEM2].ShortName);
   Result.OpenPointFile(rfile,Result.Symbol);
   Incr := 1;
   NPts := 0;
   Prog := 0;

   StartProgress('Scatter plot');
   while ( (GridLimits.XGridHigh - GridLimits.XGridLow) div Incr) * ((GridLimits.YGridHigh - GridLimits.YGridLow) div Incr) > Petmath.bfArrayMaxSize do inc(incr);
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin

      Row := GridLimits.YGridLow;
      if (Prog mod 100 = 0) then  begin
         UpdateProgressBar((Col - GridLimits.XGridLow) / (GridLimits.XGridHigh - GridLimits.XGridLow));
         {$IfDef RecordGridScatterGram} WriteLineToDebugFile('Col=' + IntToStr(Col) );    {$EndIf}
      end;
      Inc(Prog);

      while (Row <= GridLimits.YGridHigh) do begin
         if DEMGlb[DEM1].GetElevMeters(Col,Row,v[1]) then begin
            if IdenticalGrids then begin
               if DEMGlb[DEM2].GetElevMeters(Col,Row,v[2]) then begin
                  Result.AddPointToDataBuffer(rfile,v);
                  inc(NPts);
               end;
            end
            else begin
               DEMGlb[DEM1].DEMGridToLatLongDegree(Col,Row,Lat,Long);
               DEMGlb[DEM2].LatLongDegreetoDEMGrid(Lat,Long,XGrid,YGrid);
               {$IfDef RecordGridScatterGram}
                  if (Col mod 100 = 0)  and (Row mod 100 = 0) then begin
                      WriteLineToDebugFile('Col=' + IntToStr(Col) + '  ' + 'Row=' + IntToStr(Row) + '  ' + LatLongDegreeToString(Lat,Long) + '  xgrid=' + IntToStr(Round(xgrid)) +  '  ygrid=' + IntToStr(Round(ygrid)));
                  end;
               {$EndIf}
               if DEMGlb[DEM2].GetElevMeters(XGrid,YGrid,v[2]) then begin
                  Result.AddPointToDataBuffer(rfile,v);
                  inc(NPts);
               end;
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
   Result.ClosePointDataFile(rfile);
   EndProgress;
   if (NPts > 0) then begin
      Result.AutoScaleAndRedrawDiagram;
   end
   else begin
      Result.Close;
      MessageToContinue('No matches');
   end;
end;


procedure InitializeDEMStat;
var
   i : integer;
begin
   for i := 1 to MaxDEMDataSets do Count[i] := Nil;
   for i := 1 to EdburgMaxVariables do VariableWeightings[i] := 1;
   IPColor[1] := RGB(127,70,50);
   IPColor[2] := RGB(255,0,255);
   IPColor[3] := RGB(192,111,44);
   IPColor[4] := RGB(254,151,207);
   IPColor[5] := RGB(250,150,0);
   IPColor[6] := RGB(253,67,168);
   IPColor[7] := RGB(251,207,102);
   IPColor[8] := RGB(249,196,212);
   IPColor[9] := RGB(0,159,115);
   IPColor[10] := RGB(196,183,29);
   IPColor[11] := RGB(0,114,177);
   IPColor[12] := RGB(214,215,0);
   IPColor[13] := RGB(161,252,142);
   IPColor[14] := RGB(234,233,21);
   IPColor[15] := RGB(0,59,91);
   IPColor[16] := RGB(242,254,190);
end;


initialization
   InitializeDEMStat;
finalization
   {$IfDef RecordGeoStat} WriteLineToDebugFile('RecordGeoStat active in demstat'); {$EndIf}
   {$IfDef RecordStat} WriteLineToDebugFile('RecordStatProblems active in demstat'); {$EndIf}
   {$IfDef RecordGridScatterGram} WriteLineToDebugFile('RecordGridScatterGramProblems active in demstat'); {$EndIf}
   {$IfDef RecordHistogram} WriteLineToDebugFile('RecordHistogram active in demstat'); {$EndIf}
   {$IfDef RecordFFT} WriteLineToDebugFile('RecordFFTProblems active in demstat'); {$EndIf}
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('RecordDEMCompareProblems active in demstat'); {$EndIf}
   {$IfDef RecordClustering} WriteLineToDebugFile('RecordClustering active in demstat'); {$EndIf}
   {$IfDef RecordPC} WriteLineToDebugFile('RecordPC active in demstat'); {$EndIf}
   {$IfDef RecordTraceCrests} WriteLineToDebugFile('RecordTraceCrests active in demstat'); {$EndIf}
   {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('RecordDetailedTraceCrests active in demstat'); {$EndIf}
   {$IfDef MapTraceCrests} WriteLineToDebugFile('MapTraceCrests active in demstat'); {$EndIf}
   {$IfDef RecordSSO} WriteLineToDebugFile('RecordSSO active in demstat'); {$EndIf}
   {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('RecordElevationSlopePlot active in demstat'); {$EndIf}
   {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('FullRecordBlockGeostats  active in demstat'); {$EndIf}
   {$IfDef RecordIceSat} WriteLineToDebugFile('FullIceSat active in demstat'); {$EndIf}
end.



