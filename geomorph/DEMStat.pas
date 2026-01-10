{$F+,O+}

unit DEMStat;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$Define NoParallelLag}


{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef Debug }
      //{$Define NoParallelFor}
      {$Define RecordDEMIX}
      //{$Define RecordFUV}
      //{$Define RecordFUVcreate}
      //{$Define RecordFUVcreateFull}
      //{$Define RecordFUVbb}
      //{$Define MultipleLSPFUV}
      //{$Define RecordSensitivity}
      //{$Define RecordDEMIX_colors}
      //{$Define RecordComparisons}
      //{$Define RecordSSIM}
      //{$Define TrackCovariance}
      {$Define TrackCovarianceFull}
      //{$Define RecordCovarianceFail}
      //{$Define RecordMultipleLSP}
      //{$Define RecordSSIMFull}
      //{$Define RecordDEMIXTimeCriterion}
      //{$Define RecordSSO}
      //{$Define RecordHistogram}
      //{$Define RecordGridCorrrelations}
      //{$Define RecordGridScatterGram}
      //{$Define RecordDEMMapProj}
      {$Define RecordSingleGridScatterGram}
      //{$Define RecordFUVsteps}
      //{$Define TimeGridsForArea}
      //{$Define TimeOpenCreateGrids}
      //{$Define RecordMakeSSIMMapFull}
      //{$Define RecordKappa}
      //{$Define RecordDEMIXFull}
      //{$Define TrackPixelIs}
      //{$Define RecordDEMIXSSIMGrid}
      //{$Define RepeatProblematicComputations}  //put in breakpoint, and then follow debugger but may have issues
      //{$Define RecordCovariance}
      //{$Define TrackSWcorner}  //must also be defined in DEMCoord
      //{$Define RecordSSIMNormalization}
      //{$Define RecordLag}
      //{$Define RecordPitsSpires}
      //{$Define RecordMapAlgebra}
      //{$Define RecordHistogramFromVAT}
      //{$Define RecordDiffMap}
      //{$Define RecordStdDef}
      //{$Define RecordElevationSlopePlot}
      //{$Define RecordGlobalDEM}
      //{$Define RecordElevMoment}
      //{$Define RecordElevationSlopePlotAll}
      //{$Define RecordDEMCompare}
      //{$Define RecordFullDEMCompare}
      //{$Define TrackZRange}
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
   {$Else}
      //{$Define RecordDEMIX}
      //{$Define RecordFUVsteps}
   {$EndIf}
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

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end core DB functions definitions

   Forms,Classes,Controls,Graphics,ExtCtrls,Math,StdCtrls,
   System.Threading,System.SyncObjs,System.UITypes,System.IOUtils, StrUtils,System.Diagnostics,System.SysUtils,
   WinAPI.Windows,

   {$IfDef ExSat}
   {$Else}
     DEMEros,Multigrid,
   {$EndIf}
   DEMStringGrid,
   DEMLosW,PETMAR,Petmar_types,BaseGraf,DEMDefs,DEMMapf;


const
   MaxPlotGlb = 2500;
   DoingDEMIXnow : boolean = false;
type
   CountType = array[-5..2500] of integer;
   PlotArray = array[1..(MaxPlotGlb+2)] of float64;

      procedure ElevMomentReport(DEMSWanted : tDEMbooleanArray; aTitle : shortstring; SamplingCheck : boolean = false; CurDEM : integer = 0; Memo1 : tMemo = Nil);
      procedure JustElevationMoments(DEMSWanted : tDEMbooleanArray; aTitle : shortstring; Whiskers : boolean = true; StringGrid : boolean = false);
      procedure JustSlopeMoments(DEMSWanted : tDEMbooleanArray; aTitle : shortstring);
      procedure ElevationAndSlopeMoments(DEMSWanted : tDEMbooleanArray; aTitle : shortstring);

      procedure AspectDistributionBySlope(WhichDEM : Integer; GridLimits : tGridLimits);
      procedure AspectDistributionByAlgorithm(WhichDEM : Integer; GridLimits : tGridLimits);

      function CovariancesFromTwoGrids(GridLimitsDEM1 : tGridLimits; DEM1,DEM2 : integer;  var NPts : int64; var r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff : float64; NoteFailure : boolean = true) : boolean;  inline;
      function CorrelationTwoGrids(GridLimitsDEM1 : tGridLimits; DEM1,DEM2 : integer; TrackFailure : boolean = false) : float64;
      function GetFUVForPairGrids(RefGridLimits : tGridLimits; Grid1,Grid2 : integer; TrackFailure : boolean = false) : float64;

      procedure ElevationSlopePlot(WhichDEMs : tDEMbooleanArray; DesiredBinSize : integer = 1; Memo : tMemo = Nil);
      procedure MultipleElevationSlopePlots;

      procedure DoAnSSODiagram(CurDEM : integer; GridLimits : tGridLimits);
      function GridRatio(Map1Num : integer = 0; Map2Den : integer = 0; inMapType : tMapType = mtDEMBlank) : integer;
      function PercentDifferentTwoGrids(RefDEM,TestDEM : integer; fName : PathStr) : integer;

      procedure GridMinimum;
      procedure SlopeRegionSize(CurDEM : integer; DoRegionSize : boolean = true);
      procedure MakeElevSlopeAspectDifferenceMap;
      procedure PointSlopesByRegionSize(DEM : integer; RightClickLat,RightClickLong : float64);

      procedure SingleDEMHistogram(WhichDEM : integer; Quick : boolean = false);
      procedure DoElevationHistograms;
      function CreatePartDEMHistogram(DEM : integer; GridLimits: tGridLimits) : TThisBaseGraph;
      function CreateWholeDEMHistogram(DEM : integer) : TThisBaseGraph;
      function CreateMultipleHistogram(GraphNumbers : boolean; FileList,LegendList : tStringList; ParamName,TitleBar : ShortString;
          NumBins : integer = 100; Min : float32 = 1; Max : float32 = -1; BinSize : float32 =  -99; TColorList : tStringList = Nil) : TThisBaseGraph;
      procedure MultipleHistogramPrep(Histogram : TThisBaseGraph);

      procedure CalculateGrainProfile(MapForm : tMapForm; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64);
      function IwahashiiPikeColor(Slope,Convexity,Texture : float64; var TerClass : integer) : tPlatformColor;  //inline;
      function IwashuriPikeCat(Slope,Convexity,Texture : float64) : integer; //inline;
      procedure IandPLegend(pc : array of float64);

       function ClusterGrids(StartingGrid,EndingGrid : integer) : integer;

       procedure SemiVariogramOptions;
       procedure SemiVariogram(DEMtoUse : integer; GridLimits: tGridLimits);
       procedure FastFourierTransform(WantedDEM : integer; GridLimits: tGridLimits; var SlopeByColumn,SlopeByRow : float64; CloseGraphs : boolean = false);

       procedure MakeGeomporphDBforPolygons(DBonTable : integer);
       procedure MakeGeomporphDBforPoints(DBonTable : integer);
       procedure NewBlockGeostats(WhatGeomorph : tGeomporphBlock; Table1 : tMyData;  WantedDEM : integer; GridLimits : tGridLimits);

      {$IfDef ExSat}
      {$Else}
         procedure ComputeVarCoVarAndPrincipalComponents(DB : integer; MG : tMultigridArray; UseFields : tStringList);
      {$EndIf}

      {$IfDef ExWaveLengthHeight}
      {$Else}
          procedure ComputeDunecrestspacingheight(MapForm : tMapForm; GridLimits : tGridLimits; Memo1 : tMemo);
          procedure ThreadCrest1Click(MapForm : tMapForm; StartLat,StartLong : float64; var Results : tStringList; CrestNum : integer);
          procedure CrestsAlongProfile(theLOSView : TDEMLOSF; var Results : tStringList; Memo1 : tMemo = Nil);
      {$EndIf}

      procedure MissingPointsInGrids(MapForm : tMapForm; DEM1 : integer = 0; DEM2 : integer = 0);
      function GridScatterGram(GridLimits : tGridLimits; var r : float32; DEM1 : integer = 0; DEM2 : integer = 0) : TThisBaseGraph;
      procedure GridCoOccurrence(AutoFull : boolean = false; DEM1 : integer = 0; DEM2 : integer = 0; Percentages : boolean = true);
      procedure DBCoOccurrence(Table : tMyData; EmpSource: TDataSource; Field1,Field2 : ShortString; Percentages : boolean);

   function SumDEMs(FirstDEM : integer; Merge : tDEMbooleanArray; NewName : shortstring; OpenMap : boolean = true; AllGridsValidZ : boolean = true) : integer;
   procedure AddaDEM(FirstDEM,AddDEM : integer; Mult : integer = 1);

   function MakeChangeMap(Map1,Map2 : integer; GridLimits: tGridLimits) : integer;
   function GridCorrelationMatrix(Which : tGridCorrelationMatrix; DEMsWanted : tDEMbooleanArray; Title : shortstring; Incr : integer = 1; SaveName : PathStr = '') : DEMStringGrid.TGridForm;  overload;
   function GridCorrelationMatrix(Which : tGridCorrelationMatrix; NumDEM : integer; DEMsOrdered : tDEMIntegerArray; Title : shortstring; Incr : integer = 1; SaveName : PathStr = '') : DEMStringGrid.TGridForm; overload;

   procedure HistogramPointCloudAndGlobalDEMs(DB : integer = 0; Title : shortString = '');
{$IfDef ExcludeDirtAirShots}
{$Else}
   procedure DirtAndAirShots(DB : integer; Title : shortString);
{$EndIf}

{$IfDef ExIceSat}
{$Else}
   procedure IcesatProcessCanopy(dbOnTable : integer; AddDEMs : boolean; LimitDecimals : boolean = false);
   procedure IcesatPhotonConvert(Memo1 : tMemo);
   procedure AddGlobalDEMs(dbOnTable : integer);
   procedure AddEGMfields(dbOnTable : integer);
{$EndIf}

   procedure CloudSummaryGlobalDEMs(DB : integer);
   function FiveSeriesGraph(DB : integer; Lat,Long,Tolerance : float64; DirField : shortstring) : TThisbasegraph;
   procedure ElevationSlopePlotCompareDEMs;
   procedure LandCoverSummary;

   procedure GridsByRegionSize(CurDEM : integer; GridCh : char);

   procedure AllAspects;

   function FindPeaks(DEM : integer; GridLimits : tGridLimits; var PeakResults : tStringList; Memo1 : tMemo) : integer;
   function FindPits(DEM : integer; GridLimits : tGridLimits; var PitResults : tStringList; Memo1 : tMemo) : integer;

   procedure OneLag(MainDEM,SubDEM : integer; BoxLimits: tGridLimits; var BigResults : tStringList);
   procedure Lag_and_Shift(ColC,RowC : integer; MainDEM,SubDEM : integer; GridLimits : tGridLimits; var NPts,XWhereMaxR,YWhereMaxR : integer; var MaxR,NoLagR,ZRange,AvgSlope,BestA,BestB : float64; CorrelationMatrix : tStringList = Nil);

   procedure HistogramsFromVATDEM(DEMwithVAT,ElevMap,SlopeMap,RuffMap,AspMap : integer; var Graph1,Graph2,Graph3,Graph4 : tThisBaseGraph);
   function CreateGridHistograms(DEMSwanted : tDEMbooleanArray; ParamName : shortstring = ''; TailCutoff : float32 = 0.0; Min : float32 = 99; Max : float32 = -99) : TThisBaseGraph;

   procedure ComputeKappa(RefGrid,TestGrid : integer; RefGridLimits : tGridLimits; var Kappa,OverallAccuracy,AvgUsers,AvgProd : float32);

//ssim operations
   function ComputeSSIM(DEM1,DEM2 : integer; gl1 : tGridLimits; var SSIM,Luminance,Contrast,Structure : float64) : boolean; //inline;
   procedure AreaSSIMandFUVComputations(Overwrite,AreasInsteadOfTiles : boolean; Areas : tStringList = nil);
   procedure NormalizeDEMforSSIM(DEM : integer; What : shortstring);
   function MakeSSIMMap(OpenMap,AlreadyNormalized : boolean; DEM1,DEM2,NumberOfGrids,WindowSize : integer; ThinFactor : integer = 1; AreaName : shortstring = '') : integer;
   procedure SSIMcheck(DoThinning : boolean);

procedure CompareResamplingFiltering(DEM : integer);
procedure CompareWindowSizesForSlopeMap(DEM : integer);
procedure ScatterGramGrid(ScatterGram : boolean; DEMsWanted : tDEMbooleanArray; RemoveName : shortstring = ''; fName : PathStr = '');

procedure SSOforVATgrid(FeatureDEM,FeaturesDB,ElevDEM : integer);
procedure CompareLSQ(DoSlopes : boolean; DEM : integer);
procedure CompareThreeLSQ(DEM : integer);
procedure CompareLSQSlopePointSelection(DEM : integer);
procedure CompareLSQPointsUsedEffects(DEM : integer);

{$IfDef SlopeWindowEdgeEffect} procedure CompareLSQEdgeEffects(DEM : integer); {$EndIf}


procedure CompareLSPthenupsampletoUpsamplethenLSP;
procedure CompareUpsampling(SlopeOption : boolean = false);

procedure LSP_gridMultipleDEMs(Which : integer; OpenMap : boolean = true);
procedure OpennessSensitivity;
procedure SlopeCurvatureSensitivityWindowSize(WhichLSP : integer);
procedure Compare_one_dem_mult_windows(DEM : integer);




const
   SSIM_fudge : float64 = 1.0;

var
   IPColor : array[1..16] of Graphics.tColor;
   ConvexityMeanCut,TextureMeanCut,
   SlopeMeanCut,SlopeQuarterCut,SlopeEigthCut : float64;
   VariableWeightings : array[1..EdburgMaxVariables] of float64;

implementation

uses
   Nevadia_Main,

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
      NetMainW,
      demdatabase,

   {$IfDef ExDEMIX}
   {$Else}
      demix_definitions,
      DEMIX_Control,
   {$EndIf}

   Geotiff,

   DEMCoord,DEMOptions,
   PetDBUtils,
   GetLatLn,
   Thread_Timers,
   Make_tables,
   Make_grid,
   DEM_Indexes,
   DEM_Manager,
   Petimage_form,
   DEMDef_routines,
   PetImage,
   {$If Defined(ExPointCloud)}
   {$Else}
      icesat_filter_form,
      las_lidar,
   {$EndIf}
   basemap,
   DEM_NLCD,
   GDAL_tools,
   md_use_tools,
   PETMath;

const
   InsuffDEMrelief = 'Insufficient DEM relief';
type
   PlotModeType = (Plain,Strahler,Cumulative,NormProb);
var
   Count : array[1..MaxDEMDataSets] of ^CountType;


{$I demstat_global_dems.inc}

{$I demstat_ssim.inc}

{$I histograms.inc}

{$I demstat_slope_algorithm_compare.inc}

{$IfDef ExWaveLengthHeight}
{$Else}
   {$I demstat_dune_crest.inc}
{$EndIf}

   {$I demstat_grid_compare.inc}


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


procedure ElevationSlopePlot(WhichDEMs : tDEMbooleanArray; DesiredBinSize : integer = 1; Memo : tMemo = nil);
label
   Restart;
const
   MinSlopeValue = 0;
   MaxSlopeValue = 250;
type
   TheArrayType = array[MinSlopeValue..MaxSlopeValue] of float64;
   GraphType    = (ElevFreq,SlopeFreq,ElevSlope,AspectFreq,CumSlope,AspectRose,ElevSlopeDeg,ElevRuff);
var
   Graph : GraphType;
   BinSize,StartIndex,Bin,
   IntSlope,
   MinMin,MaxMax,NumDEMs, CurDEM,
   j,x,which : LongInt;
   MaxCount,MaxSlopeCount,
   Cum,MinElevZ,MaxElevZ,MaxSlope : float64;
   Ruff1,MaxRuff : float32;
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
   Ruffs,
   Slopes   : array[1..MaxDEMDataSets] of ^TheArrayType;
   PointCount : array[tCompassDirection] of int32;
   ThisGraph : TThisBaseGraph;
   SlopeAspectRec : tSlopeAspectRec;
   GraphList : tStringList;
   AspectStats : tAspectStats;

      function GraphPlot(Graph : GraphType) : TThisBaseGraph;
      var
         x,n,CurDEM : integer;
         rfile,rfile2,rfile3 : file;
         v       : tGraphPoint32;
         TStr,bsString : ShortString;
      begin
         if (Graph in [AspectRose]) then begin
            Result := AspectStats.CreateRose;
         end
         else begin
             Result := TThisBaseGraph.Create(Application);
             Result.GraphDraw.LeftMargin := 100;
             Result.GraphDraw.BottomMargin := 65;
             Result.GraphDraw.DrawInsideLines := false;
             bsString := 'bin size=' + IntToStr(BinSize) + ' m';
             case Graph of
                ElevRuff  : TStr := ' Elevation vs Roughness (%), ' + bsString;
                ElevSlope  : TStr := ' Elevation vs Slope (%), ' + bsString;
                ElevFreq   : Tstr := ' Elevation Frequency, ' + bsString;
                ElevSlopeDeg : TStr := ' Elevation vs Slope (°), ' + bsString;
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
             Result.GraphDraw.LLcornerText := bsString;
             case Graph of
               ElevFreq  : Result.GraphDraw.MaxHorizAxis := MaxCount;
               ElevRuff : begin
                              Result.GraphDraw.MaxHorizAxis := MaxRuff + 5;
                              Result.GraphDraw.HorizLabel := 'Avg Roughness (' + IntToStr(MDDef.RoughnessBox) + 'x' + IntToStr(MDDef.RoughnessBox) + ' window std dev slope, %)';
                           end;
               ElevSlope : begin
                              Result.GraphDraw.MaxHorizAxis := MaxSlope + 5;
                              Result.GraphDraw.HorizLabel := 'Avg Slope ' + SlopeMethodName(MDDef.SlopeCompute)+' (%)';
                           end;
               ElevSlopeDeg : begin
                              Result.GraphDraw.MaxHorizAxis := arcTan(0.01*MaxSlope) / DegToRad;
                              Result.GraphDraw.HorizLabel := 'Avg Slope ' + SlopeMethodName( MDDef.SlopeCompute)+' (°)';
                           end;
               SlopeFreq : begin
                             Result.GraphDraw.MaxHorizAxis := MaxSlopeCount;
                             Result.GraphDraw.MinVertAxis := 0;
                             Result.GraphDraw.MaxVertAxis := MaxSlopeValue;
                             Result.GraphDraw.VertLabel := 'Slope ' + SlopeMethodName(MDDef.SlopeCompute) + ' (%)';
                           end;
               CumSlope   : begin
                               Result.GraphDraw.HorizLabel := 'Slope ' + SlopeMethodName(MDDef.SlopeCompute) + ' (%)';
                               Result.GraphDraw.VertLabel := 'Cumulative Percentage Less Steep';
                               Result.GraphDraw.VertAxisFunct := NInv;
                               Result.GraphDraw.VertAxisFunctionType := CumulativeNormalAxis;
                            end;
             end {case};

            Result.GraphDraw.SetShowAllLines(true);
            Result.GraphDraw.SetShowAllPoints(false);
            n := 0;
            for CurDEM := 1 to MaxDEMDataSets do if WhichDEMs[CurDEM] and ValidDEM(CurDEM) then begin
               If (Memo <> nil) then begin
                  Memo.Lines.Add(TimeToStr(Now) + '  ' + DEMGlb[CurDEM].AreaName);
                  Application.ProcessMessages;
               end;
               Result.OpenDataFile(rfile,DEMGlb[CurDEM].AreaName);
               Result.GraphDraw.FileColors256[CurDEM] := ConvertTColorToPlatformColor(WinGraphColors(CurDEM));

               if DoingDEMIXnow then begin
                  LoadDEMIXnames;
                  Result.GraphDraw.FileColors256[CurDEM] := DEMIXColorFromDEMName(DEMGlb[CurDEM].AreaName);
                  {$IfDef RecordDEMIX_colors} WriteLineToDebugFile('DEMStat ' + DEMGlb[CurDEM].AreaName + '  ' + ColorStringFromPlatformColor(Result.GraphDraw.FileColors256[CurDEM])); {$EndIf}
               end;

               if (Graph in [ElevSlope,ElevSlopeDeg,ElevRuff])  then begin
                  Result.GraphDraw.ShowLine[pred(Result.GraphDraw.DataFilesPlotted.Count)] := true;
               end;
               if (Graph in [ElevSlope,ElevSlopeDeg]) and (NumDEMs = 1) and (MDdef.ShowSDonElevSlope) and (MDdef.ShowGeomorphometry) then begin
                  inc(n);
                  Result.OpenDataFile(Rfile2,'');
                  Result.OpenDataFile(Rfile3,'');
                  Result.GraphDraw.LineSize256[2] := 1;
                  Result.GraphDraw.LineSize256[3] := 1;
               end;
               if (Graph = ElevFreq) then begin
                  for x := 0 to NumBins[CurDEM] do if Count[CurDEM]^[x] > 0 then begin
                     v[1] := Count[CurDEM]^[x];
                     v[2] := BinElev[CurDEM]^[x];
                     BlockWrite(Rfile,v,1);
                  end;
                  Result.GraphDraw.ShowLine[pred(Result.GraphDraw.DataFilesPlotted.Count)] := true;
               end
               else if (Graph = SlopeFreq) then begin
                  for x := 0 to MaxSlopeValue do if SlopeHist[CurDEM]^[x] > 0 then begin
                     v[1] := SlopeHist[CurDEM]^[x];
                     v[2] := x;
                     BlockWrite(Rfile,v,1);
                  end;
               end
               else if Graph in [ElevRuff] then begin
                  if (NumDEMs = 1) then Result.GraphDraw.FileColors256[1] := claBlue;
                  Result.Image1.Canvas.Pen.Width := 3;
                  for x := 0 to NumBins[CurDEM] do if Count[CurDEM]^[x] > 0 then begin
                     v[1] := Ruffs[CurDEM]^[x];
                     v[2] := BinElev[CurDEM]^[x];
                     {$IfDef RecordStdDef} WriteLineToDebugFile('bin: ' + RealToString(v[2],-12,-1) + '  slope: ' + RealToString(v[1],-12,-1)); {$EndIf}
                     BlockWrite(rfile,v,1);
                  end;
               end
               else if Graph in [ElevSlope,ElevSlopeDeg] then begin
                  if (NumDEMs = 1) then Result.GraphDraw.FileColors256[1] := claBlue;
                  Result.Image1.Canvas.Pen.Width := 3;
                  for x := 0 to NumBins[CurDEM] do if Count[CurDEM]^[x] > 0 then begin
                     v[1] := Slopes[CurDEM]^[x];
                     v[2] := BinElev[CurDEM]^[x];
                     if (Graph = ElevSlopeDeg) then v[1] := ArcTan(0.01 * v[1]) / DegToRad;
                     {$IfDef RecordStdDef} WriteLineToDebugFile('bin: ' + RealToString(v[2],-12,-1) + '  slope: ' + RealToString(v[1],-12,-1)); {$EndIf}
                     BlockWrite(rfile,v,1);
                     if (NumDEMs = 1) and (MDdef.ShowSDonElevSlope) and (MDdef.ShowGeomorphometry) then begin
                        v[1] := Slopes[CurDEM]^[x] - SumSquares[CurDEM]^[x];
                        if (Graph = ElevSlopeDeg) then v[1] := ArcTan(0.01 * v[1]) / DegToRad;
                        {$IfDef RecordStdDef} WriteLineToDebugFile('std: ' + RealToString(SumSquares[CurDEM]^[x],-12,-1) + '  slope - std: ' + RealToString(v[1],-12,-1)); {$EndIf}
                        BlockWrite(rfile2,v,1);
                        v[1] := Slopes[CurDEM]^[x] + SumSquares[CurDEM]^[x];
                        if (Graph = ElevSlopeDeg) then v[1] := ArcTan(0.01 * v[1]) / DegToRad;
                        {$IfDef RecordStdDef} WriteLineToDebugFile('slope + std: ' + RealToString(v[1],-12,-1)); {$EndIf}
                        BlockWrite(rfile3,v,1);
                     end;
                  end;
               end
               else if (Graph = CumSlope) then begin
                  TotalCount[CurDEM] := 0;
                  for x := 0 to 100 do TotalCount[CurDEM] := TotalCount[CurDEM] + SlopeHist[CurDEM]^[x];
                  x := 0;
                  Cum := 0;
                  repeat
                     Cum := SlopeHist[CurDEM]^[x] / TotalCount[CurDEM] * 100;
                     inc(x);
                  until Cum > 0;
                  v[1] := pred(x);
                  v[2] := Cum;
                  BlockWrite(Rfile,v,1);

                  while (x <= 100) do begin
                     Cum := Cum + SlopeHist[CurDEM]^[x] / TotalCount[CurDEM] * 100;
                     if SlopeHist[CurDEM]^[x] > 0 then begin
                        v[1] := x;
                        v[2] := Cum;
                        BlockWrite(Rfile,v,1);
                     end;
                     inc(x);
                  end;
               end {if};
               CloseFile(Rfile);
               if (Graph in [ElevSlope,ElevSlopeDeg]) and (NumDEMs = 1) and (MDdef.ShowSDonElevSlope) and (MDdef.ShowGeomorphometry) then begin
                  CloseFile(Rfile2);
                  CloseFile(Rfile3);
               end;
               Result.RedrawDiagram11Click(Nil);
            end {for CurDEM};
         end;
      end;

var
   LegendBMP : tMyBitmap;
   LegendY,i : integer;
   fName : PathStr;
   Dir : tCompassDirection;
begin {proc ElevationSlopePlot}
   {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot in'); {$EndIf}
   SetColorForProcessing;
   MaxCount := 0;
   MaxSlope := 0;
   MaxRuff := 0;
   MinElevZ := MaxSmallInt;
   MaxElevZ := -MaxSmallInt;
   Graph := ElevSlope;
   MaxSlopeCount := 0;
   GraphList := nil;

   NumDEMs := 0;
   for i := 1 to MaxDEMDataSets do if WhichDEMs[i] then inc(NumDEMs);
   if (NumDEMs > 1) and  MDDef.ShowAspectRose then GraphList := tStringList.Create;
   MinMin := 9999;
   MaxMax := -9999;
   for Which := 1 to MaxDEMDataSets do if WhichDEMs[Which] and ValidDEM(Which) then begin
      if (DEMGlb[Which].DEMheader.MinElev < MinMin) then MinMin := round(DEMGlb[Which].DEMheader.MinElev);
      if (DEMGlb[Which].DEMheader.MaxElev > MaxMax) then MaxMax := round(DEMGlb[Which].DEMheader.MaxElev);
   end;

   GetBins(MaxMax,MinMin,BinSize,StartIndex);
   if (BinSize < DesiredBinSize) then BinSize := DesiredBinSize;

   StartIndex := MinMin div BinSize;

   if MDDef.ShowColorLegend then begin
      CreateBitmap(LegendBMP,350,25*NumDEMDataSetsOpen+10);
      LegendBMP.Canvas.Font.Name := 'Verdana';
      LegendBMP.Canvas.Font.Size := 14;
      LegendBMP.Canvas.Font.Style := [fsbold];
      LegendY := 5;
   end;

   for CurDEM := 1 to MaxDEMDataSets do if WhichDEMs[CurDEM] and ValidDEM(CurDEM) then begin
      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot DEM=' + IntToStr(CurDEM)); {$EndIf}
      AspectStats.Create(CurDEM);

      if MDDef.ShowColorLegend then begin
         LegendBMP.Canvas.Font.Color := WinGraphColors(CurDEM);
         LegendBMP.Canvas.TextOut(15,LegendY,DEMGlb[CurDEM].AreaName);
         inc(LegendY,25);
      end;
      New(Count[CurDEM]);
      New(SumSquares[CurDEM]);
      New(BinElev[CurDEM]);
      New(Slopes[CurDEM]);
      if MDDef.ShowElevRough then New(Ruffs[CurDEM]);
      New(SlopeHist[CurDEM]);
      for Dir := Low(Dir) to High(Dir) do PointCount[Dir] := 0;

      MaxDEMSlope[CurDEM] := 0;
      for x := MinSlopeValue to MaxSlopeValue do SlopeHist[CurDEM]^[x] := 0;

      for x := MinSlopeValue to MaxSlopeValue do begin
         Slopes[CurDEM]^[x] := 0;
         if MDDef.ShowElevRough then Ruffs[CurDEM]^[x] := 0;
         SumSquares[CurDEM]^[x] := 0;
         Count[CurDEM]^[x] := -0.0000001;
      end {for x};

      SlopeTotal[CurDEM] := 0;
      SlopeSqrTotal[CurDEM] := 0;
      TotalDEM[CurDEM] := 0;
      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('Setup over, DEM=' + IntToStr(CurDEM) + '  increment=' + IntToStr(MDdef.StatSampleIncr)); {$EndIf}

      StartProgress('Stats ' + DEMGlb[CurDEM].AreaName,25,5);
      x := 1;
      while x <= (DEMGlb[CurDEM].DEMheader.NumCol - 2) do begin
         UpDateProgressBar( x / DEMGlb[CurDEM].DEMheader.NumCol);
         {$IfDef RecordElevationSlopePlotAll} WriteLineToDebugFile('x=' + IntToStr(x)); {$EndIf}
         j := 1;
         while j <= (DEMGlb[CurDEM].DEMheader.NumRow - 2) do begin
            if DEMGlb[CurDEM].QuickEvansSlopeAndAspect(x,j,SlopeAspectRec) then begin
               if Graph in [ElevRuff] then DEMGlb[CurDEM].RoughnessFromSlopeSTD(x,j,MDDef.RoughnessBox,Ruff1);
               if (MDDef.UseSealevel) or (abs(SlopeAspectRec.z) > 0.001) then begin
                  AspectStats.AddPoint(SlopeAspectRec);
               end;
               Bin := (round(SlopeAspectRec.z) div BinSize) - StartIndex;
               Count[CurDEM]^[Bin] := 1 + Count[CurDEM]^[Bin];
               inc(TotalDEM[CurDEM]);

               if MDDef.ShowSlopeFreq or MDDef.ShowElevSlope or MDDef.ShowCumSlope or MDDef.ShowElevSlopeDeg then begin
                  if (SlopeAspectRec.Slope > MaxDEMSlope[CurDEM]) then MaxDEMSlope[CurDEM] := SlopeAspectRec.Slope;
                  SlopeTotal[CurDEM] := SlopeTotal[CurDEM] + SlopeAspectRec.Slope;
                  SlopeSqrTotal[CurDEM] := SlopeSqrTotal[CurDEM] + sqr(SlopeAspectRec.Slope);
                  inc(PointCount[Dir]);
                  Slopes[CurDEM]^[Bin] := Slopes[CurDEM]^[Bin] + SlopeAspectRec.Slope;
                  if MDDef.ShowElevRough then Ruffs[CurDEM]^[Bin] := Ruffs[CurDEM]^[Bin] + Ruff1;
                  SumSquares[CurDEM]^[Bin] := SumSquares[CurDEM]^[Bin] + sqr(SlopeAspectRec.Slope);
                  IntSlope := round(SlopeAspectRec.SlopePercent);
                  if (IntSlope <= MaxSlopeValue) then begin
                     SlopeHist[CurDEM]^[IntSlope] := 1 + SlopeHist[CurDEM]^[IntSlope];
                  end;
               end;
            end {if};
            inc(J,MDdef.StatSampleIncr);
         end {while j};
         inc(x,MDdef.StatSampleIncr);
      end {for x};

      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot end first loop'); {$EndIf}

      TotalCount[CurDEM] := 0;
      for x := MinSlopeValue to MaxSlopeValue do TotalCount[CurDEM] := TotalCount[CurDEM] + Count[CurDEM]^[x];
      for x := MinSlopeValue to MaxSlopeValue do begin
         SlopeHist[CurDEM]^[x] := 100 * SlopeHist[CurDEM]^[x] / TotalCount[CurDEM];
         if Count[CurDEM]^[x] > 0.5 then begin
            if Count[CurDEM]^[x]  < 2.5 then SumSquares[CurDEM]^[x] := 0
            else SumSquares[CurDEM]^[x] := 100.0 * sqrt(1.0 * (SumSquares[CurDEM]^[x] * Count[CurDEM]^[x] - sqr(1.0 * Slopes[CurDEM]^[x])) / (sqr(1.0 * Count[CurDEM]^[x])));
            if MDDef.ShowElevRough then begin
               Ruffs[CurDEM]^[x] := Ruffs[CurDEM]^[x] / Count[CurDEM]^[x];
               if Ruffs[CurDEM]^[x] > MaxRuff then MaxRuff := Ruffs[CurDEM]^[x];
            end;
            Slopes[CurDEM]^[x] := 100 * Slopes[CurDEM]^[x] / Count[CurDEM]^[x];
            if Slopes[CurDEM]^[x] > MaxSlope then MaxSlope := Slopes[CurDEM]^[x];
            if (100 * Count[CurDEM]^[x] / TotalCount[CurDEM] > MaxCount) then MaxCount := 100 * Count[CurDEM]^[x] / TotalCount[CurDEM];
            NumBins[CurDEM] := x;
         end {if};
         Count[CurDEM]^[x] := 100 * Count[CurDEM]^[x] / TotalCount[CurDEM];
         BinElev[CurDEM]^[x] := MinMin + (x + 0.5) * BinSize;
      end {for x};
      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot end slope loop'); {$EndIf}

      for x := MinSlopeValue to MaxSlopeValue do begin
         if SlopeHist[CurDEM]^[x] > MaxSlopeCount then MaxSlopeCount := SlopeHist[CurDEM]^[x];
      end {for x};

      if MinElevZ > BinElev[CurDEM]^[0] then MinElevZ := trunc(BinElev[CurDEM]^[0]);
      if MaxElevZ < BinElev[CurDEM]^[NumBins[CurDEM]] then MaxElevZ := succ(Trunc(BinElev[CurDEM]^[NumBins[CurDEM]]));

      EndProgress;
      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('End loop, DEM=' + IntToStr(CurDEM)); {$EndIf}

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
   end {for CurDEM};

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
      if MDDef.ShowElevRough then GraphPlot(ElevRuff);
   finally
      ShowDefaultCursor;
   end;

   for CurDEM := 1 to MaxDEMDataSets do if WhichDEMs[CurDEM] and ValidDEM(CurDEM) then begin
      Dispose(Count[CurDEM]);
      Dispose(SumSquares[CurDEM]);
      Dispose(BinElev[CurDEM]);
      Dispose(Slopes[CurDEM]);
      Dispose(SlopeHist[CurDEM]);
   end;
   SetColorForWaiting;
   {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot Out'); {$EndIf}
end;




procedure MultipleElevationSlopePlots;
begin
   SaveBackupDefaults;
   MDDef.ShowElevSlope := true;
   MDDef.ShowElevFreq := false;
   MDDef.ShowSlopeFreq := false;
   MDDef.ShowCumSlope := false;
   MDDef.ShowAspectRose := false;
   MDDef.ShowElevSlopeDeg := false;
   MDDef.ShowColorLegend := false;
   MDDef.ShowSDonElevSlope := false;
   MDDef.ShowElevRough := false;
   ElevationSlopePlot(GetMultipleDEMsFromList('DEMs for elevation/slope plot'),MDDef.ElevBinSize,Nil);
   RestoreBackupDefaults;
end;


procedure SSOforVATgrid(FeatureDEM,FeaturesDB,ElevDEM : integer);
var
   ID : integer;
   SSOvars : tSSOvars;
   LLtext : shortstring;
   fName : PathStr;
   CloseIt : boolean;
   NetSL : tstringList;
begin
  if ValidDB(FeaturesDB) then begin
     CloseIt := false;
  end
  else if FileExists(DEMGlb[FeatureDEM].VATFileName) then begin
     OpenNumberedGISDataBase(FeaturesDB,DEMGlb[FeatureDEM].VATFileName);
     CloseIt := true;
  end;
  if ValidDB(FeaturesDB) then begin
      if not ValidDEM(ElevDEM) then GetDEM(ElevDEM,false,'elevation DEM');
      {$IfDef RecordSSO} WriteLineToDebugFile('ElevDEM=' + DEMGlb[ElevDEM].AreaName); {$EndIf}
      if ValidDEM(ElevDEM) then begin
         NetSL := tstringList.Create;
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'S2S3',8,3);
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'TREND',5,1);
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'ROUGHNESS',8,3);
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'DOWN_DIP',5,1);
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'RELIEF',10,2);
         StartProgress('Features');
         while not GISdb[FeaturesDB].MyData.eof do begin
            ID := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger('VALUE');
            LLtext := GISdb[FeaturesDB].MyData.GetFieldByNameAsString('NAME');
            {$IfDef RecordSSO} HighlightLineToDebugFile('Feature=' + IntToStr(ID)); {$EndIf}
            fName := NextFileNumber(MDTempDir,'sso_features_' + IntToStr(ID) + '_','.png');
            if DEMGLB[ElevDEM].SSOComputations(DEMGLB[ElevDEM].FullDEMGridLimits,SSOvars,true,fName,'',FeatureDEM,ID,LLtext) then begin
               GISdb[FeaturesDB].MyData.Edit;
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('S2S3',SSOvars.s2s3);
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('TREND',SSOvars.TheDipDirs[3]);
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('DOWN_DIP',SSOvars.TheDipDirs[1]);
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('ROUGHNESS',SSOvars.RoughnessFactor);
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('RELIEF',SSOvars.ElevRange);
               NetSL.Add(fName);
            end;
            GISdb[FeaturesDB].MyData.Next;
         end;
         fName := NextFileNumber(MDTempDir,'sso_entire_dem_','.png');
         DEMglb[ElevDEM].SSOComputations(DEMGLB[ElevDEM].FullDEMGridLimits,SSOvars,true,fName,'',0,0,'All points');
         NetSL.Add(fName);
      end;
      {$IfDef RecordSSO} WriteLineToDebugFile('SSOforVATgrid, BigBitmap Features=' + IntToStr(NetSL.Count)); {$EndIf}
      MakeBigBitmap(NetSL,Copy(GISdb[FeaturesDB].DBName,1,length(GISdb[FeaturesDB].DBName)-6),'',2);
      if CloseIt then CloseAndNilNumberedDB(FeaturesDB);
  end;
end;



procedure ScatterGramGrid(ScatterGram : boolean; DEMsWanted : tDEMbooleanArray; RemoveName : shortstring = ''; fName : PathStr = '');
const
   LeftMargin = 70;
   TopMargin = 75;
   BetweenGraphs = 20;
var
   First : boolean;
   Panelx,PanelY,
   i,j,row,col,x,y : integer;
   theMax,theMin,aMax,aMin : float32;
   BigBitMap,LegendBitmap,Bitmap : tMyBitmap;

         procedure StartBigBitmap(Bitmap : tMyBitmap);
         //wait to do this to have the actual size of each graph's bitmap
         var
            NumGraphs,i,j : integer;
            LabelName : shortstring;
         begin
             PanelX := Bitmap.Width;
             PanelY := Bitmap.Height;
             NumGraphs := 0;
             for i := 1 to MaxDEMDataSets do begin
                if DEMsWanted[i] and ValidDEM(i) then begin
                   inc(NumGraphs);
                end;
             end;
             CreateBitmap(BigBitmap,NumGraphs * (PanelX + BetweenGraphs) + LeftMargin + 25,NumGraphs * (PanelY + BetweenGraphs) + TopMargin + 25);
             BigBitmap.Canvas.Font.Size := 24;
             BigBitmap.Canvas.Font.Style := [fsBold];

             for i := 1 to MaxDEMDataSets do begin
                if DEMsWanted[i] and ValidDEM(i) then begin
                   LabelName := DEMglb[i].AreaName;
                   if (RemoveName <> '') then LabelName := System.SysUtils.StringReplace(LabelName, RemoveName, '', [rfReplaceAll]);
                   LabelName := RemoveUnderscores(LabelName);
                   while BigBitmap.Canvas.TextWidth(LabelName) > PanelX do begin
                      BigBitmap.Canvas.Font.Size := BigBitmap.Canvas.Font.Size - 1;
                   end;
                end;
             end;


             j := 0;
             for i := 1 to MaxDEMDataSets do begin
                if DEMsWanted[i] and ValidDEM(i) then begin
                   LabelName := DEMglb[i].AreaName;
                   if (RemoveName <> '') then LabelName := System.SysUtils.StringReplace(LabelName, RemoveName, '', [rfReplaceAll]);
                   LabelName := RemoveUnderscores(LabelName);
                   BigBitmap.Canvas.TextOut(LeftMargin + j * (PanelX + BetweenGraphs),5,LabelName);
                   inc(j);
                   TextOutVertical(BigBitmap.Canvas,5,j * (PanelY + BetweenGraphs),LabelName);
                end;
             end;
             First := false;
         end;

         function DrawScattergramAndGetGraphImage(i,j : integer) : tMyBitmap;
         var
            Graph : tThisBaseGraph;
            r : float32;
         begin
             {$IfDef RecordGridScattergramFull} WriteLineToDebugFile('Start scattergram, Grids: ' + DEMGlb[i].AreaName + ' and  ' +  DEMGlb[j].AreaName); {$EndIf}
             Graph := Nil;
             Graph := GridScatterGram(DEMGlb[i].FullDEMGridLimits,r,i,j);
             if (Graph <> nil) then begin
                Graph.GraphDraw.MaxHorizAxis := theMax;
                Graph.GraphDraw.MaxVertAxis := theMax;
                Graph.GraphDraw.MinHorizAxis := theMin;
                Graph.GraphDraw.MinVertAxis := theMin;
                Graph.GraphDraw.HorizLabel := '';
                Graph.GraphDraw.VertLabel := '';
                Graph.RedrawDiagram11Click(Nil);
                CopyImageToBitmap(Graph.Image1,Result);
                Graph.Destroy;
             end;
         end;

         function DrawDifferenceMapAndGetGraphImage(i,j : integer) : tMyBitmap;
         var
            DiffMap : integer;
         begin
            {$IfDef RecordGridScattergramFull} WriteLineToDebugFile('Start difference map, Grids: ' + DEMGlb[i].AreaName + ' and  ' +  DEMGlb[j].AreaName); {$EndIf}
            DiffMap := MakeDifferenceMap(i,j,i,0,true,false,false);
            CopyImageToBitmap(DEMGlb[DiffMap].SelectionMap.Image1,Result);
            if First then begin
               LegendBitmap := DEMGlb[DiffMap].SelectionMap.MapDraw.DrawLegendOnBitmap;
            end;
            CloseSingleDEM(DiffMap);
         end;


begin {procedure ScatterGramGrid}
    {$IfDef RecordGridScattergram} WriteLineToDebugFile('ScatterGramGrid in'); {$EndIf}
    NakedMapOptions;
    SetColorForProcessing;
    MDDef.DefaultGraphXSize := 300;
    MDDef.DefaultGraphYSize := 240;
    MDDef.DefaultMapXSize := 300;
    MDDef.DefaultMapYSize := 300;
    MDDef.HighlightDiffMap := 2;
    MDDef.MissingDataColor := claSilver;
    MDDef.DefElevsPercentile := false;
    First := true;
    row := 0;
    theMax := -99e39;
    theMin := 99e39;
    for i := 1 to MaxDEMDataSets do begin
       if DEMsWanted[i] and ValidDEM(i) then begin
          aMin := DEMglb[i].FindPercentileElev(5);
          if aMin < theMin then theMin := aMin;
          aMax := DEMglb[i].FindPercentileElev(95);
          if aMax > theMax then theMax := aMax;
         {$IfDef RecordGridScattergram} WriteLineToDebugFile('Grids: ' + DEMGlb[i].AreaName + ' ' + RealToString(aMin,-12,-6) + ' to ' + RealToString(aMax,-12,-6)); {$EndIf}
       end;
    end;
    {$IfDef RecordGridScattergram} WriteLineToDebugFile('Graphs: ' + RealToString(TheMin,-12,-6) + ' to ' + RealToString(TheMax,-12,-6)); {$EndIf}

    for i := 1 to MaxDEMDataSets do begin
       if DEMsWanted[i] and ValidDEM(i) then begin
         {$IfDef RecordGridScattergram} WriteLineToDebugFile('Start loop, Grids: ' + DEMGlb[i].AreaName); {$EndIf}
          Col := succ(Row);
          for j := succ(i) to MaxDEMDataSets do begin
             if DEMsWanted[j] and ValidDEM(j) then begin
                if ScatterGram then begin
                   Bitmap := DrawScattergramAndGetGraphImage(i,j);
                end
                else begin
                   Bitmap := DrawDifferenceMapAndGetGraphImage(i,j);
                end;

                if First then StartBigBitmap(Bitmap);
                x := LeftMargin + (PanelX + BetweenGraphs) * Row;
                y := TopMargin + (PanelY + BetweenGraphs) * Col;
                BigBitmap.Canvas.Draw(x,y,Bitmap);
                {$IfDef RecordGridScattergramDraw} WriteLineToDebugFile('   Draw at x=' + IntToStr(x) + ' y=' + IntToStr(y)); {$EndIf}
                x := LeftMargin + (PanelX + BetweenGraphs) * Col;
                y := TopMargin + (PanelY + BetweenGraphs) * Row;
                BigBitmap.Canvas.Draw(x,y,Bitmap);
                {$IfDef RecordGridScattergramDraw} WriteLineToDebugFile('   Draw at x=' + IntToStr(x) + ' y=' + IntToStr(y)); {$EndIf}
                BitMap.Free;
             end;
             inc(Col);
          end;
          inc(Row);
       end;
    end;
    if ScatterGram then begin
       if (fName = '') then fName := 'Scattergrams';
       DisplayBitmap(BigBitmap,fName);
    end
    else begin
       if (LegendBitmap <> Nil) then begin
          BigBitmap.Height := BigBitMap.Height + 20 + LegendBitmap.Height;
          BigBitmap.Canvas.Draw(BigBitmap.Width - 25 - LegendBitmap.Width,BigBitmap.Height - LegendBitmap.Height - 2,LegendBitmap);
          LegendBitmap.Destroy;
       end;
       if (fName = '') then fName := 'Difference maps';
       DisplayBitmap(BigBitmap,fName);
    end;
    RestoreBackupDefaults;
    SetColorForWaiting;
end {procedure ScatterGramGrid};



procedure CompareResamplingFiltering(DEM : integer);
begin
   MessageToContinue('Disabled');
(*
const
   NumGeo = 7;
   NumUTM = 6;
   NumFilt = 6;
   Averages : array[1..NumUTM] of integer = (5,10,15,20,25,30);
   GeoSpace : array[1..NumGeo] of float32 = (1/9,0.25,1/3,0.5,0.67,0.75,1);
   Filters : array[1..NumFilt] of integer = (1,3,5,10,20,30);
var
   i,NewGrid : integer;
   fName : PathStr;
   OpenMap : boolean;
   Findings : tStringList;
   Fixed : int64;
   Series : shortstring;
   Space : float32;

   procedure ProcessNewGrid(NewGrid : integer);
   var
      Ruff,Slope : integer;
      n : int64;
      sMean,sStd,rMean,rStd : float32;
   begin
      Slope := 0;   //so slope grid returned
      Ruff := CreateSlopeRoughnessSlopeStandardDeviationMap(NewGrid,5,Slope,false);
      DEMglb[Slope].ElevationStatistics(DEMglb[Slope].FullDEMGridLimits,sMean,sStd,n);
      DEMglb[Ruff].ElevationStatistics(DEMglb[Ruff].FullDEMGridLimits,rMean,rStd,n);
      if (Series <> 'Filter') then Space := DEMglb[NewGrid].AverageSpace;
      Findings.Add(DEMglb[NewGrid].AreaName + ',' + Series + ',' + RealToString(Space,-8,-2) + ',' + RealToString(sMean,-8,-2) + ',' + RealToString(sStd,-8,-2) + ',' + RealToString(rMean,-8,-2) + ',' + RealToString(rStd,-8,-2));
      if (NewGrid <> DEM) then CloseSingleDEM(NewGrid);
      CloseSingleDEM(Slope);
      CloseSingleDEM(Ruff);
   end;


begin
   SetColorForProcessing;
   OpenMap := false;
   Findings := tStringList.Create;
   Findings.Add('NAME,SERIES,AVG_SPACE,AVG_SLOPE,STD_SLOPE,AVG_ROUGH,STD_ROUGH');

   DEMGLb[DEM].MarkBelowMissing(1.0,Fixed,false);

   Series := 'UTM';
   ProcessNewGrid(DEM);
   for i := 1 to NumUTM do begin
      fName := 'utm_' + IntToStr(Averages[i]) + '_m';
      wmdem.SetPanelText(3,'utm ' + IntToStr(i) + '/' + IntToStr(NumUTM));
      NewGrid := DEMGlb[DEM].ThinThisDEM(OpenMap,fName,Averages[i],true);
      ProcessNewGrid(NewGrid);
   end;

   Series := 'Geo';
   for i := 1 to NumGeo do begin
      fName := MDtempDir + 'arc_sec_' + RealToString(GeoSpace[i],-8,-3) + '_sec.tif';
      wmdem.SetPanelText(3,'geo ' + IntToStr(i) + '/' + IntToStr(NumGeo));
      NewGrid := CreateArcSecDEM(false,OpenMap,DEM,PixelIsPoint,GeoSpace[i],GeoSpace[i],fName);
      ProcessNewGrid(NewGrid);
   end;

   Series := 'Filter';
   for i := 1 to NumFilt do begin
      wmdem.SetPanelText(3,'Mean filter ' + IntToStr(i) + '/' + IntToStr(NumFilt));
      NewGrid := DEMGlb[DEM].FilterThisDEM(OpenMap,fcMean,Filters[i]);
      Space := 0.5 * succ(2 * Filters[i]) * DEMglb[NewGrid].AverageSpace;
      ProcessNewGrid(NewGrid);
   end;

   wmdem.SetPanelText(3,'');
   SetColorForWaiting;

   fName := NextFileNumber(MDtempDir,'slope_spacing_sampler_','.dbf');
   Findings.SaveToFile(fName);
   StringList2CSVtoDB(Findings,fName);
*)
end;


procedure Lag_and_Shift(ColC,RowC : integer; MainDEM,SubDEM : integer; GridLimits : tGridLimits; var NPts,XWhereMaxR,YWhereMaxR : integer; var MaxR,NoLagR,ZRange,AvgSlope,BestA,BestB : float64; CorrelationMatrix : tStringList = Nil);
var
   x,y : ^bfarray32;
   xs,ys,Col,Row : integer;
   Lat,Long,xg,yg : float64;
   zMin,zMax, a,b,siga,sigb,r : float64;
   z : array[1..2] of float32;
   MomentVar : tMomentVar;
begin
   {$IfDef RecordLagProblems} WriteLineToDebugFile('Lag_and_Shift in'); {$EndIf}
   New(x);
   New(y);
   MaxR := -1;
   zMin := 99999;
   zMax := -99999;
   if (CorrelationMatrix = Nil) then begin
      CorrelationMatrix := tStringList.Create;
      CorrelationMatrix.Add('LAT,LONG,XSHIFT,YSHIFT,R2,INTERCEPT,SLOPE');
   end;

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
            //if (CorrelationMatrix = Nil) then begin
               DEMGLB[MainDEM].DEMGridToLatLongDegree(ColC+xs,RowC+ys,Lat,Long);
               CorrelationMatrix.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7) + ',' + IntToStr(xs) + ',' + IntToStr(ys) + ',' + RealToString(r,-12,-5) );
            //end;
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
     {$If Defined(NoParallelFor) or Defined(NoParallelLag)}
        {$IfDef RecordLagProblems} WriteLineToDebugFile('DoStrip in'); {$EndIf}
        StartProgress('Lag strip ' + DEMGlb[SubDEM].AreaName);
     {$EndIf}
     Buffer := MDDef.LagSearchRadius;
     Buffer := 0;
     x := BoxLimits.XGridLow + Buffer;
     while x <= BoxLimits.XGridHigh - Buffer do begin
        {$If Defined(NoParallelFor) or Defined(NoParallelLag)}
          UpdateProgressBar( (x-BoxLimits.XGridLow) / (BoxLimits.XGridHigh - BoxLimits.XGridLow));
        {$EndIf}
        y := BoxLimits.YGridLow + Buffer;
        while y <= BoxLimits.YGridHigh - Buffer do begin
           GridLimits.XGridLow := x - MDDef.LagSearchRadius;
           GridLimits.XGridHigh := x + MDDef.LagSearchRadius;
           GridLimits.YGridLow := y - MDDef.LagSearchRadius;
           GridLimits.YGridHigh := y + MDDef.LagSearchRadius;
           Missing := DEMGlb[MainDEM].ComputeMissingDataPercentage(GridLimits);
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
        {$If Defined(NoParallelFor) or Defined(NoParallelLag)}
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
  i,it,nt{,NumStrips} : integer;
  Results : array[1..MaxThreadsAllowed] of tStringList;
  //StripBoxLimits: tGridLimits;
begin
   {$IfDef RecordLag} WriteLineToDebugFile('One Lag in, maindem=' + IntToStr(MainDEM) + ' subdem=' + IntToStr(SubDEM)); {$EndIf}
   i := 1;
   ShowSatProgress := false;

   {$If Defined(NoParallelFor) or Defined(NoParallelLag)}
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
   x,y,NumPit,Col,Row,dx,dy : integer;
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
      StartProgressAbortOption('Peaks');
      for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
         if Col mod 25 = 0 then UpdateProgressBar( (Col - GridLimits.XGridLow) / (GridLimits.XGridHigh - GridLimits.XGridLow));
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
         if WantOut then break;
      end {while col};
      EndProgress;
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
    //fName : PathStr;
    BMPlist : tStringList;
    DEM : integer;
 begin
    BMPlist := tStringList.Create;
    for DEM := 1 to MaxDEMDataSets do begin
       if ValidDEM(DEM) then begin
           if (DEMGlb[DEM].DEMHeader.ElevUnits = euAspectDeg) then begin
              rg := CreateAspectRose(DEM);
           end;
       end;
    end;
   //if (BMPlist.Count > 0) then begin
      MakeBigBitmap(BMPlist,'Aspect roses');
   //end
   //else BMPlist.Free;
end;


procedure PointSlopesByRegionSize(DEM : integer; RightClickLat,RightClickLong : float64);
var
   xDEMg1,yDEMg1 : float64;
   Findings : tStringList;
   xField,yField,aCapt,
   TStr,location : shortString;
   i,db,Col,Row : integer;
   graph : tThisBaseGraph;

         procedure GetTheSlopes(BoxSize : integer);
         var
            SlopeAsp : tSlopeAspectRec;
         begin
            MDDef.SlopeCompute.WindowRadius := BoxSize;
            if DEMGlb[DEM].GetSlopeAndAspect(MDDef.SlopeCompute,Col,Row,SlopeAsp) then begin
               Findings.Add(Location + ',' + IntToStr(BoxSize) + ',' + RealToString(BoxSize* DEMGlb[DEM].AverageYSpace,-12,-2) + ',' +
                     RealToString(SlopeAsp.SlopePercent,-12,2) + ',' + RealToString(SlopeAsp.SlopeDegree,-12,2) + ',' + RealToString(SlopeAsp.AspectDirTrue,-8,2));
            end;
         end;

begin
   DEMDef_routines.SaveBackupDefaults;
   Findings := tStringList.Create;
   Location := LatLongDegreeToString(RightClickLat,RightClickLong,MDDef.OutPutLatLongMethod);

   DEMGlb[DEM].LatLongDegreeToDEMGrid(RightClickLat,RightClickLong,xDEMg1,yDEMg1);
   Col := round(xDEMg1);
   Row := round(yDEMg1);

   Findings.Add('Location,Region,Dist_m,Slope_pc,Slope_deg,Aspect_deg');
   for i := 1 to 25 do GetTheSlopes(i);
   TStr := NextFileNumber(MDTempDir,DEMGlb[DEM].AreaName + '_Slope_by_region_','.dbf');
   xField := 'DIST_M';
   yField := 'SLOPE_PC';
   aCapt := 'Slopes by region at ' + Location;

   StringList2CSVtoDB(Findings,TStr);
   Graph := GISDB[db].CreateScatterGram('test',xField,yField,clRed,true,aCapt);
   Graph.GraphDraw.MinVertAxis := 0;
   Graph.GraphDraw.LLCornerText := Location;
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
          zvs : ^bfarray32;
          MomentVar : tMomentVar;
          Row : integer;
       begin
          inc(RegionsDone);
          MDdef.SlopeCompute.WindowRadius := BoxSize;
          New(zvs);
          DEMGlb[CurDEM].SlopeMomentsWithArray(DEMGlb[CurDEM].FullDEMGridLimits, MomentVar,zvs^);
          ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
          Dispose(zvs);
          Row := 1;
          MomentsToStringGrid(GridForm.StringGrid1, Row,RegionsDone,'Slope', MomentVar);
          if (MomentVar.MaxZ > MaxSlope) then MaxSlope := MomentVar.MaxZ;    //for histogram
          if DoRegionSize then begin
             GridForm.StringGrid1.Cells[RegionsDone,0] := 'Region ' + IntToStr(BoxSize) + ' (' + DEMGlb[CurDEM].SimpleHorizontalDEMSpacing(BoxSize) + ')';
             LegendFiles.Add(GridForm.StringGrid1.Cells[RegionsDone,0]);
          end
          else begin
             GridForm.StringGrid1.Cells[RegionsDone,0] := ShortSlopeMethodName(MDDef.SlopeCompute);
             LegendFiles.Add(ShortSlopeMethodName(MDDef.SlopeCompute));
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
         MDDef.SlopeCompute.AlgorithmName := Method;
         MakeSlopeMap(0);
      end;
   end;
   CreateMultipleHistogram(MDDef.CountHistograms,ElevFiles,LegendFiles,'Slope','Slope distribution',-99,0,Trunc(MaxSlope + 0.99),MDDef.SlopeHistBinSize);

   GridForm.StringGrid1.ColCount:= succ(RegionsDone);
   GridForm.SetFormSize;

   DEMDef_routines.RestoreBackupDefaults;  //restore slope algorithm
end;


procedure GridsByRegionSize(CurDEM : integer; GridCh : char);
begin
   var i : integer;
   for i := 1 to 5 do Make_grid.MakeMomentsGrid(CurDEM,GridCh,MDDef.BatchRegionSize[i]);
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
           eLat1,eLong1,eLat2,eLong2,Distance,Bearing : float64;
           xloc,yloc : integer;
           zl,z : float32;
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
                   Table1.CarefullySetFloat('ELEV_STD',MomentVar.std_dev,0.001);
                   Table1.CarefullySetFloat('ELEV_SKW',MomentVar.skew,0.001);
                   Table1.CarefullySetFloat('ELEV_KRT',MomentVar.curt,0.001);
                   {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished basic elevation'); {$EndIf}
                 end;

                 if MDDef.IncludeAdvancedElevation then begin
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats Start advanced elevation'); {$EndIf}
                    Table1.SetFieldByNameAsFloat('RELIEF',MomentVar.MaxZ - MomentVar.MinZ);
                    z := (MomentVar.mean-MomentVar.MinZ) / (MomentVar.MaxZ - MomentVar.MinZ);
                    Table1.SetFieldByNameAsFloat('ELEV_RELF',z);

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
                        Table1.CarefullySetFloat('SLOPE_STD',MomentVar.std_dev,0.001);
                        Table1.CarefullySetFloat('SLOPE_SKW',MomentVar.skew,0.001);
                        Table1.CarefullySetFloat('SLOPE_KRT',MomentVar.curt,0.001);
                        Table1.CarefullySetFloat('SLOPE_MAX',MomentVar.MaxZ,0.001);
                     end;
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished slope'); {$EndIf}
                 end;
                 {$IfDef MultipleCurvatureMethods}
                    if MDDef.IncludePlanCMeasures then begin
                         DEMGlb[WantedDEM].PlanCMoments(GridLimits,MomentVar);
                         if MomentVar.NPts > 1 then begin
                            Table1.CarefullySetFloat('PLANC_AVG',MomentVar.mean,0.000001);
                            Table1.CarefullySetFloat('PLANC_STD',MomentVar.std_dev,0.000001);
                            Table1.CarefullySetFloat('PLANC_SKW',MomentVar.skew,0.000001);
                            Table1.CarefullySetFloat('PLANC_KRT',MomentVar.curt,0.000001);
                         end;
                        {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished plan curvature'); {$EndIf}
                    end;

                    if MDDef.IncludeProfCMeasures then begin
                        DEMGlb[WantedDEM].ProfCMoments(GridLimits,MomentVar);
                        if MomentVar.NPts > 1 then begin
                           Table1.CarefullySetFloat('PROFC_AVG',MomentVar.mean,0.000001);
                           Table1.CarefullySetFloat('PROFC_STD',MomentVar.std_dev,0.000001);
                           Table1.CarefullySetFloat('PROFC_SKW',MomentVar.skew,0.000001);
                           Table1.CarefullySetFloat('PROFC_KRT',MomentVar.curt,0.000001);
                        end;
                       {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats finished profile curvature'); {$EndIf}
                    end;
                 {$EndIf}

                 if MDDef.IncludeOpenness then begin
                    {$IfDef FullRecordBlockGeostats} WriteLineToDebugFile('ComputeStats start openness'); {$EndIf}
                     DEMGlb[WantedDEM].BothOpennessMoments(GridLimits,UpMoment,DownMoment);
                     if (UpMoment.NPts > 0) then begin
                        Table1.SetFieldByNameAsFloat('OPN_UP_AVG',UpMoment.mean);
                        Table1.SetFieldByNameAsFloat('OPN_UP_STD',UpMoment.std_dev);
                        Table1.SetFieldByNameAsFloat('OPN_UP_SKW',UpMoment.Skew);
                        Table1.SetFieldByNameAsFloat('OPN_UP_KRT',UpMoment.Curt);
                        Table1.SetFieldByNameAsFloat('OPN_DN_AVG',DownMoment.mean);
                        Table1.SetFieldByNameAsFloat('OPN_DN_STD',DownMoment.std_dev);
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
                    Table1.SetFieldByNameAsFloat('MISSING',DEMGlb[WantedDEM].ComputeMissingDataPercentage(GridLimits));
                 end;

                  if MDDef.IncludeBasinID then begin
                    Table1.SetFieldByNameAsString('BASIN_ID',DEMGlb[WantedDEM].AreaName);
                    //Table1.SetFieldByNameAsFloat('SLP_OV_30',DEMGlb[WantedDEM].Over30PercentSlope);
                    //Table1.SetFieldByNameAsFloat('SLP_OV_50',DEMGlb[WantedDEM].Over50PercentSlope);
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
             SkipMenuUpdating := false;
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


procedure AddaDEM(FirstDEM,AddDEM : integer; Mult : integer = 1);
var
   x,y  : integer;
   Lat,Long : float64;
   z,z2,nz : float32;
begin
    StartProgress('DEM add ' + DEMGlb[AddDEM].AreaName + ' to ' + DEMGlb[FirstDEM].AreaName);
    for x := 0 to pred(DEMGlb[FirstDEM].DEMheader.NumCol) do begin
       if (x mod 50 = 0) then UpDateProgressBar(x/pred(DEMGlb[FirstDEM].DEMheader.NumCol));
       for y := 0 to pred(DEMGlb[FirstDEM].DEMheader.NumRow) do begin
          if DEMGlb[FirstDEM].GetElevMeters(x,y,z) then begin
             DEMGlb[FirstDEM].DEMGridToLatLongDegree(x,y,Lat,Long);
             if DEMGlb[AddDEM].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                nz := z + (Mult * z2);
                DEMGlb[FirstDEM].SetGridElevation(x,y,nz);
             end
             else begin
                DEMGlb[FirstDEM].SetGridMissing(x,y);
             end;
          end
          else DEMGlb[FirstDEM].SetGridMissing(x,y);
       end;
    end;
    EndProgress;
    DEMGlb[FirstDEM].CheckMaxMinElev;
end;


function SumDEMs(FirstDEM : integer; Merge : tDEMbooleanArray; NewName : shortstring; OpenMap : boolean = true; AllGridsValidZ : boolean = true) : integer;
label
   MissingData;
var
   i,{j,}Col,Row : integer;
   Lat,Long : float64;
   z,z2 : float32;
   //IdenticalGrids : boolean;
begin
   {$IfDef RecordMapAlgebra} WriteLineToDebugFile('SumDEMs in'); {$EndIf}
   if OpenAndZeroNewDEM(true,DEMGlb[FirstDEM].DEMheader,Result,NewName,InitDEMmissing) then begin
      StartProgress('Sum grids');
      for Col := 0 to pred(DEMGlb[Result].DEMheader.NumCol) do begin
         if (Col mod 100 = 0) then begin
            UpdateProgressBar(Col/DEMGlb[Result].DEMheader.NumCol);
            {$IfDef RecordMapAlgebra} WriteLineToDebugFile('Col=' + IntToStr(Col)); {$EndIf}
         end;
         for Row := 0 to pred(DEMGlb[Result].DEMheader.NumRow) do begin
            z := 0;
            for i := 1 to MaxDEMDataSets do if Merge[i] then begin
               if DEMGlb[FirstDEM].SecondGridIdentical(i) then begin
                  if DEMGlb[i].GetElevMeters(Col,Row,z2) then z := z + z2
                  else if AllGridsValidZ then GoTo MissingData;
               end
               else begin
                  DEMGlb[FirstDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                  if DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,z2) then z := z + z2
                  else if AllGridsValidZ then Goto MissingData;
               end;
            end;
            DEMGlb[Result].SetGridElevation(Col,Row,z);
            MissingData:;
         end;
      end;
      EndProgress;
      if OpenMap then DEMGlb[Result].SetupMap(true,DEMGlb[FirstDEM].SelectionMap.MapDraw.MapType);
   end;
   {$IfDef RecordMapAlgebra} WriteLineToDebugFile('SumDEMs out'); {$EndIf}
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
   BestClass,Sampler,i,j,x,y,db : integer;
   Limits : tGridLimits;
   aMin,zf : float32;
   NPts : int64;
   FieldsUsed : byte;
   FieldsToUse : array[1..MaxBands] of AnsiString;
   z,Mean,Std : array[1..MaxDEMDataSets] of float32;
   DistArr : array[1..EdburgGeneralFuncsMaxClusters] of float64;
   HistArray : array[1..EdburgGeneralFuncsMaxClusters] of integer;
   NewHeadRecs : tDEMheader;
   sl : tStringList;
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
          if ValidDEM(i) then begin
             MVClusterClientDataSet.FieldDefs.Add('DEM_' + IntToStr(i),ftFloat, 0, False);
             inc(FieldsUsed);
             FieldsToUse[FieldsUsed] := 'DEM_' + IntToStr(i);
             DEMGlb[i].ElevationStatistics(DEMGlb[i].FullDEMGridLimits,Mean[i],Std[i],NPts);
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
            for i := StartingGrid to EndingGrid do if ValidDEM(i) then begin
               if (not DEMGlb[i].GetElevMeters(x,y,z[i])) then goto MissingData;
            end;

            MVClusterClientDataSet.Insert;
            for i := StartingGrid to EndingGrid do begin
               if ValidDEM(i) then begin
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
          sl := tStringList.Create;
          KMeansClustering(sl,FieldsToUse, FieldsUsed, MDTempDir + 'Clster_Results.HTML');
          sl.destroy;
          wmDEM.SetPanelText(0,'');
       end;

     NewHeadRecs := DEMGlb[StartingGrid].DEMheader;
     NewHeadRecs.DEMPrecision := ByteDEM;

     if not OpenAndZeroNewDEM(true,NewHeadRecs,Result,'Clusters',InitDEMvalue,0) then exit;
        with DEMGlb[Result],DEMheader do begin
           ShortName := 'Clusters';
           DEMheader.ElevUnits := euUndefined;
           DefineDEMVariables(true);
           StartProgress('Classify');
           for y := 0 to pred(NumRow) do begin
              if (y mod 100 = 0) then UpdateProgressBar(y/NumRow);

              for x := 0 to pred(NumCol) do begin
                 for i := StartingGrid to EndingGrid do if ValidDEM(i) and (i <> Result) then begin
                    if (DEMGlb[i].GetElevMeters(x,y,z[i])) then z[i] := VariableWeightings[i] * (z[i] - Mean[i]) / std[i]
                    else goto MissingData2;
                 end;

                 for j := 1 to MVClusterClientDataSet.NClusters do begin
                    DistArr[j] := 0;
                    for i := StartingGrid to EndingGrid do if ValidDEM(i) and (i <> Result) then
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
   DEMGlb[Result].SetupMap(true,mtDEMVATTable);
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


function IwahashiiPikeColor(Slope,Convexity,Texture : float64;  var TerClass : integer) : tPlatformColor;
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
   NPts : int64;
   Value,Lat,Long,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff    : float64;
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
      {$IfDef RecordPC} WriteLineToDebugFile('MG option'); {$EndIf}

      StartProgress('covariances');
      NumVars := MG.NumGrids;
      for i := 1 to MG.NumGrids do begin
         UpDateProgressBar(i/MG.NumGrids);
         if (MG.Grids[i] <> 0) then begin
            VarTitle[i] := DEMGlb[MG.Grids[i]].AreaName;
            for j := 1 to MG.NumGrids do begin
               if (MG.Grids[j] <> 0) then begin
                  CovariancesFromTwoGrids(DEMGlb[MG.Grids[i]].FullDEMGridLimits,MG.Grids[i],MG.Grids[j],Npts,Correlations[i,j],VarCoVar[i,j],Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff);
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
         if not ValidPath(MGPath) then SafeMakeDir(MGPath);
         MGPath := MGPath + TStr;

         for k := 1 to MDDef.MaxPCBands do begin
            if (K <= MG.NumGrids) and (pc[k] > MDdef.MinPCToShow) then begin
               FName := MGPath + '_pc_' + IntToStr(k) + '.dem';
               NewDEM := DEMGLB[MG.Grids[1]].CloneAndOpenGridSetMissing(FloatingPointDEM,fName,DEMGLB[MG.Grids[1]].DEMheader.ElevUnits);
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
               DEMGlb[NewDEM].DEMheader.ElevUnits := euUndefined;   //Imagery;
               DEMGlb[NewDEM].CheckMaxMinElev;
               DEMGlb[NewDEM].WriteNewFormatDEM(fName);
               if MDDef.LoadPCBands then begin
                  DEMGlb[NewDEM].AreaName := ExtractFileNameNoExt(fName);
                  DEMGlb[NewDEM].SetUpMap(false,mtElevGray);
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

//{$EndIf}


function GetFUVForPairGrids(RefGridLimits : tGridLimits; Grid1,Grid2 : integer; TrackFailure : boolean = false) : float64;
//if the grids do not match exactly, Grid2 is reinterpolated to match Grid1
begin
  {$IfDef TrackCovarianceFull} if TrackFailure then WriteLineToDebugFile('GetFUVForPairGrids in, ' + DEMGlb[Grid1].AreaName + '  ' + DEMGlb[Grid2].AreaName); {$EndIf}
   if ValidDEM(Grid1) and ValidDEM(Grid2) then begin
      Result := 1-sqr(CorrelationTwoGrids(RefGridLimits,Grid1,Grid2,TrackFailure));
   end
   else begin
      Result := NaN;
      {$IfDef TrackCovariance} WriteLineToDebugFile('Invalid grid, GetFUVForPairGrids Fail for ' + ' ' + DEMglb[Grid1].AreaName + ' ' + DEMglb[Grid2].AreaName); {$EndIf}
   end;
   {$IfDef TrackCovariance} if TrackFailure then WriteLineToDebugFile('GetFUVForPairGrids out, ' + DEMGlb[Grid1].AreaName + '  ' + DEMGlb[Grid2].AreaName + ' FUV=' + RealToString(Result,-8,-4)); {$EndIf}
end;


function CorrelationTwoGrids(GridLimitsDEM1 : tGridLimits; DEM1,DEM2 : integer; TrackFailure : boolean = false) : float64;
//if the grids do not match exactly, DEM2 is reinterpolated to match DEM1
var
   Col,Row,xoff,yoff : integer;
   NPts : int64;
   Lat,Long,a,b,siga,sigb : float64;
   z1,z2 : float32;
   xs,ys : ^BfArray32;
   IdenticalOffsetGrids,MatchPt : boolean;
begin
   {$IfDef TrackCovarianceFull} if TrackFailure then WriteLineToDebugFile('CorrelationTwoGrids in, ' + DEMGlb[dem1].AreaName + '  ' + DEMGlb[dem2].AreaName); {$EndIf}
   New(xs);
   New(ys);
   NPts := 0;
   IdenticalOffsetGrids := DEMGlb[DEM1].SecondGridJustOffset(DEM2,xoff,yoff,true);
   {$If Defined(TrackCovarianceFull) or Defined(TrackCovariance)}
      if TrackFailure then begin
         WriteLineToDebugFile('xoff=' + IntToStr(Xoff) + '   yoff=' + IntToStr(yoff));
      end;
   {$EndIf}

   Col := GridLimitsDEM1.XGridLow;
   while (Col <= GridLimitsDEM1.XGridHigh) do begin
      Row := GridLimitsDEM1.YGridLow;
      while (Row <= GridLimitsDEM1.YGridHigh) do begin
         if DEMGlb[DEM1].GetElevMeters(Col,Row,z1) then begin
            if IdenticalOffsetGrids then begin
               MatchPt := DEMGlb[DEM2].GetElevMeters(Col+xoff,Row+yoff,z2);
            end
            else begin
               DEMGlb[DEM1].DEMGridToLatLongDegree(Col,Row,Lat,Long);
               MatchPt := DEMGlb[DEM2].GetElevFromLatLongDegree(Lat,Long,z2);
            end;
            if MatchPt then begin

               xs^[Npts] := z1;
               ys^[Npts] := z2;
               inc(NPts);
            end;
         end;
         inc(row);
      end;
      inc(Col);
   end;
   {$IfDef TrackCovarianceFull} if TrackFailure then WriteLineToDebugFile('CorrelationTwoGrids call fit, NPTs=' + IntToStr(NPts)); {$EndIf}
   if (NPts = 0) then Result := Nan
   else fit(xs^,ys^,NPts,a,b,siga,sigb,Result);
   Dispose(xs);
   Dispose(ys);
   {$IfDef TrackCovariance} if TrackFailure then WriteLineToDebugFile('CorrelationTwoGrids out, ' + DEMGlb[dem1].AreaName + '  ' + DEMGlb[dem2].AreaName + '  r=' + RealToString(Result,-8,-4)); {$EndIf}
end;


function CovariancesFromTwoGrids(GridLimitsDEM1 : tGridLimits; DEM1,DEM2 : integer; var NPts : int64; var r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff : float64; NoteFailure : boolean = true) : boolean;
var
   Col,Row,xoff,yoff,i : integer;
   z1,z2 : float32;
   Lat,Long : float64;
   IdenticalGrids,MatchPt : boolean;
   sum, sp : array[1..2] of float64;
   spc : float64;
begin
   {$If Defined(RecordCovariance)} HighlightLineToDebugFile('CovariancesFromTwoGrids in, grids=' + IntToStr(DEM1) + ' and ' + IntToStr(DEM2)); {$EndIf}
   IdenticalGrids := DEMGlb[DEM1].SecondGridJustOffset(DEM2,xoff,yoff,true);
   NPts := 0;
   spc := 0;
   for i := 1 to 2 do begin
      sum[i] := 0;
      sp[i] := 0;
   end;
   MeanDiff := 0;
   MeanAbsDiff := 0;

   {$If Defined(RecordCovariance)}
      WriteLineToDebugFile('DEM1, ' + DEMglb[DEM1].KeyParams(true));
      WriteLineToDebugFile('DEM2, ' + DEMglb[DEM2].KeyParams(true));
      if IdenticalGrids then  WriteLineToDebugFile('offset, x=' + IntToStr(xoff) + '  y=' + IntToStr(yoff));
      WriteLineToDebugFile('DEM1 grid limits ' + GridLimitsToString(GridLimitsDEM1));
   {$EndIf}

 (*
   sx := 0.0;
   sy := 0.0;
   sxy := 0.0;
   st2 := 0.0;
   sty2 := 0;
   b := 0.0;
   for i := pred(FirstPoint) to pred(LastPoint) do begin
      sy := sy + y[i];
      sx := sx + x[i];
   END;
   {$IfDef RecordFitProblems} WriteLineToDebugFile('sx=' + RealToString(sx,-18,-8) + '  sy=' + RealToString(sy,-18,-8)); {$EndIf}
   //ss := ndata;

   sxoss := sx / ndata;
   syoss := sy / ndata;
   {$IfDef RecordFitProblems} WriteLineToDebugFile('sxoss=' + RealToString(sxoss,18,8) +'  syoss=' + RealToString(syoss,18,8)); {$EndIf}

   for i := pred(FirstPoint) to pred(LastPoint) do begin
      t := x[i] - sxoss;
      ty := y[i] - syoss;
      st2 := st2 + sqr(t);
      sty2 := sty2 + sqr(ty);
      b := b + t * y[i];
      sxy := sxy + t * ty;
   END;
   b := b/st2;
   a := (sy-sx*b) / Ndata;
   siga := sqrt((1.0 + sqr(sx) / (NData * st2)) / NData);
// siga := sqrt((1.0+sx*sx/(ss*st2))/ss);

   sigb := sqrt(1.0 / st2);
   r := sxy / sqrt(st2 * sty2);
*)

   Col := GridLimitsDEM1.XGridLow;
   while (Col <= GridLimitsDEM1.XGridHigh) do begin
      Row := GridLimitsDEM1.YGridLow;
      while (Row <= GridLimitsDEM1.YGridHigh) do begin
         if DEMGlb[DEM1].GetElevMeters(Col,Row,z1) then begin
            if IdenticalGrids then begin
               MatchPt := DEMGlb[DEM2].GetElevMeters(Col+xoff,Row+yoff,z2);
            end
            else begin
               DEMGlb[DEM1].DEMGridToLatLongDegree(Col,Row,Lat,Long);
               MatchPt := DEMGlb[DEM2].GetElevFromLatLongDegree(Lat,Long,z2);
            end;
            if MatchPt then begin
               Sum[1] := Sum[1] + z1;       //sx
               Sum[2] := Sum[2] + z2;       //sy
               sp[1] := sp[1] + z1 * z1;
               sp[2] := sp[2] + z2 * z2;
               SPc := SPc + z1 * z2;
               inc(NPts);
               MeanDiff := MeanDiff + (z1 - z2);
               MeanAbsDiff := MeanAbsDiff + abs(z1 - z2);
            end;
         end;
         inc(Row);
      end;
      inc(Col);
   end;
   Result := (NPts > 0);
   if Result then begin
      Mean1 := (Sum[1] / NPts);
      StdDev1 := sqrt( (NPts * SP[1] - sqr(sum[1]) ) / pred(NPts) / Npts );
      Mean2 := (Sum[2] / NPts);
      StdDev2 := sqrt( (NPts * SP[2] - sqr(sum[2]) ) / pred(NPts) / Npts );
      Covar := (NPts * SPc - Sum[1] * Sum[2]) / NPts / pred(NPts);
      r := Covar / StdDev1 / StdDev2;
      MeanDiff := MeanDiff / NPts;
      MeanAbsDiff := MeanAbsDiff / Npts;

      if IsNAN(r) then begin
         {$If Defined(RecordCovarianceFail)} HighLightLineToDebugFile('CovariancesFromTwoGrids is Nan'); {$EndIf}
         r := 0;
      end;

      {$If Defined(RecordCovariance)}
         WriteLineToDebugFile('npts=' + IntToStr(NPts));
         WriteLineToDebugFile('Mean1=' + RealToString(Mean1,8,2) + ' std dev1=' + RealToString(StdDev1,8,2) + '  ' + DEMglb[DEM1].AreaName);
         WriteLineToDebugFile('Mean2=' + RealToString(Mean2,8,2) + ' std dev2=' + RealToString(StdDev2,8,2) + '  ' + DEMglb[DEM2].AreaName);
      {$EndIf}
      {$If Defined(RecordStat) or Defined(RecordCovariance)}  WriteLineToDebugFile('CovariancesFromTwoGrids out covar=' + RealToString(covar,-12,-4) + '  r=' + RealToString(r,-12,-6)); {$EndIf}
   end
   else begin
      r := -999;
      {$If Defined(RecordCovarianceFail)} HighLightLineToDebugFile('CovariancesFromTwoGrids fail, npts=' + IntToStr(NPts) + ' DEM1=' + DEMglb[DEM1].AreaName +  ' DEM2=' + DEMglb[DEM2].AreaName); {$EndIf}
      {$If Defined(RecordStat) or Defined(RecordCovariance) or Defined(RecordCovarianceFail)}
         if NoteFailure then WriteLineToDebugFile('CovariancesFromTwoGrids failed, npts=' + IntToStr(NPts) + ' DEM1=' + DEMglb[DEM1].AreaName +  ' DEM2=' + DEMglb[DEM2].AreaName );
      {$EndIf}
   end;
end;

(*
function MeanAbsoluteDeviationFromTwoGrids(GridLimitsDEM1 : tGridLimits; DEM1,DEM2 : integer; var NPts : int64; var MAD : float64; NoteFailure : boolean = true) : boolean;
var
   Col,Row,xoff,yoff,i : integer;
   z1,z2 : float32;
   Lat,Long : float64;
   IdenticalGrids,MatchPt : boolean;
   Sum : float64;
begin
   {$If Defined(RecordCovariance)} HighlightLineToDebugFile('CovariancesFromTwoGrids in, grids=' + IntToStr(DEM1) + ' and ' + IntToStr(DEM2)); {$EndIf}
   IdenticalGrids := DEMGlb[DEM1].SecondGridJustOffset(DEM2,xoff,yoff,true);
   NPts := 0;
   Sum := 0;

   {$If Defined(RecordCovariance)}
      WriteLineToDebugFile('DEM1, ' + DEMglb[DEM1].KeyParams(true));
      WriteLineToDebugFile('DEM2, ' + DEMglb[DEM2].KeyParams(true));
      if IdenticalGrids then  WriteLineToDebugFile('offset, x=' + IntToStr(xoff) + '  y=' + IntToStr(yoff));
      WriteLineToDebugFile('DEM1 grid limits ' + GridLimitsToString(GridLimitsDEM1));
   {$EndIf}

   Col := GridLimitsDEM1.XGridLow;
   while (Col <= GridLimitsDEM1.XGridHigh) do begin
      Row := GridLimitsDEM1.YGridLow;
      while (Row <= GridLimitsDEM1.YGridHigh) do begin
         if DEMGlb[DEM1].GetElevMeters(Col,Row,z1) then begin
            if IdenticalGrids then begin
               MatchPt := DEMGlb[DEM2].GetElevMeters(Col+xoff,Row+yoff,z2);
            end
            else begin
               DEMGlb[DEM1].DEMGridToLatLongDegree(Col,Row,Lat,Long);
               MatchPt := DEMGlb[DEM2].GetElevFromLatLongDegree(Lat,Long,z2);
            end;
            if MatchPt then begin
               Sum := Sum + abs(z1-z2);
               inc(NPts);
            end;
         end;
         inc(Row);
      end;
      inc(Col);
   end;
   Result := (NPts > 0);
   if Result then begin
      MAD := Sum / Npts;
   end
   else begin
      MAD := -999;
      {$If Defined(RecordCovarianceFail)} HighLightLineToDebugFile('CovariancesFromTwoGrids fail, npts=' + IntToStr(NPts) + ' DEM1=' + DEMglb[DEM1].AreaName +  ' DEM2=' + DEMglb[DEM2].AreaName); {$EndIf}
      {$If Defined(RecordStat) or Defined(RecordCovariance) or Defined(RecordCovarianceFail)}
         if NoteFailure then WriteLineToDebugFile('CovariancesFromTwoGrids failed, npts=' + IntToStr(NPts) + ' DEM1=' + DEMglb[DEM1].AreaName +  ' DEM2=' + DEMglb[DEM2].AreaName );
      {$EndIf}
   end;
end;
*)




procedure CalculateGrainProfile(MapForm : tMapForm; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64);
const
   MaxBoxes = 10;
var
   xgrid,ygrid,Trend,s1s2,s2s3 : float64;
   Lat,Long,Distance,Bearing,CurDist,rf : float64;
   f : array[1..MaxBoxes] of file;
   Size : array[1..MaxBoxes] of integer;
   BoxesWanted : tStringList;
   err,
   i,BoxesUsed : integer;
   ThisGraph : tThisBaseGraph;
   Vals : tGraphPoint32;
begin
   DEMOptions.GetFabricOptions(DEMonMap);
   VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Distance,Bearing);
   BoxesWanted := tStringList.Create;
   BoxesWanted.LoadFromFile(ProgramRootDir + 'FabricRegionSize.txt');
   ThisGraph := TThisBaseGraph.Create(Application);
   //ThisGraph.GraphDraw.LegendList := tStringList.Create;

   for i := 1 to BoxesWanted.Count do begin
      BoxesUsed := i;
      Val(BoxesWanted.Strings[pred(i)],Size[i],err);
      //ThisGraph.GraphDraw.LegendList.Add(IntToStr(Size[i]) + '_m');
      if (i = MaxBoxes) then break;
   end;
   BoxesWanted.Free;

   ThisGraph.Caption := 'Topographic Grain along Profile ' + DEMGlb[DEMonMap].AreaName;
   for i := 1 to BoxesUsed do ThisGraph.OpenDataFile(f[i],IntToStr(Size[i]) + '_m');
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
   //Results,
   AllResults : tStringList;
   AspName,NetName : PathStr;
   AspSL,NetSL : tstringList;
   Success : boolean;
   fName : PathStr;
begin
   {$IfDef RecordSSO} WriteLineToDebugFile('DoAnSSODiagram in, DEM=' + IntToStr(CurDEM)); {$EndIf}
   SetColorForProcessing;
   SaveBackupDefaults;
   MDDef.NetDef.NetUsed := Schmidt;
   AllResults := tStringList.Create;
   AllResults.Add('DEM,AVG_SPACE,GRID_TRUE,AVG_ELEV,AVG_SLOPE,MAX_SLOPE,FABRIC,LN_S1_S2,LN_S2_S3,SHAPE,STRENGTH,ROUGH,AVG_ASPECT,STR_ASPECT,QUEENS');

   MDDef.NetDef.FormLegend := false;
   MDDef.NetDef.MaxContourConcentration := 10;
   ng := 0;
   if MDDef.GemorphAspectRose then AspSL := tstringList.Create;
   if MDDef.GemorphSSOPoles then NetSL := tstringList.Create;
   Success := false;
   for i := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
      if (i=CurDEM) then begin
         inc(ng);
         if MDDef.GemorphSSOPoles then begin
            NetName := NextFileNumber(MDtempDir,DEMglb[i].AreaName + '_sso_net_' + IntToStr(ng) + '_','.bmp');
            NetSL.Add(NetName);
         end;
         if MDDef.GemorphAspectRose then begin
            AspName := NextFileNumber(MDtempDir,DEMglb[i].AreaName + '_sso_asp_' + IntToStr(ng) + '_','.bmp');
            AspSL.Add(AspName);
         end;
         if DEMGlb[i].SSOComputations(DEMGlb[i].FullDEMGridLimits,SSOvars,true,NetName,AspName) then begin
            Success := true;
            AllResults.Add(DEMGlb[i].AreaName + ',' + RealToString(DEMGlb[i].AverageSpace,-8,-2) + ',' + RealToString(DEMGlb[i].AverageGridTrue,-8,-2) + ',' +
               RealToString(SSOvars.AvgElev,8,2) + ',' + RealToString(SSOvars.AvgSlope,8,2) + ',' + RealToString(SSOvars.MaxSlope,8,2) + ',' +
               RealToString(SSOvars.TheDipDirs[3],8,1) + ',' +
               RealToString(SSOvars.s1s2,8,3) + ',' + RealToString(SSOvars.s2s3,8,3) + ',' + RealToString(SSOvars.Shape,6,3) + ',' + RealToString(SSOvars.Strength,6,3)+ ',' +
               RealToString(SSOvars.RoughnessFactor,8,4) + ',' + RealToString(SSOvars.AvgAspect,8,2) + ',' +  RealToString(SSOvars.AspectStrength,8,4) + ',' +
               RealToString(SSOvars.QueensAspect,8,3));
         end;
      end;
   end;
   if Success then begin
      if MDDef.GemorphSSOPoles then begin
         //NetSL.Add(MDtempDir + 'net_legend.bmp');
         MakeBigBitmap(NetSL,'');
      end;
      if MDDef.GemorphAspectRose then begin
         MakeBigBitmap(AspSL,'Aspect roses');
      end;
      fName := NextFileNumber(MDTempDir,'sso_results_',DefaultDBExt);
      StringList2CSVtoDB(AllResults,fName,false);
   end
   else begin
      MessageToContinue('No results');
   end;
   RestoreBackupDefaults;
   SetColorForWaiting;
   {$IfDef RecordSSO} WriteLineToDebugFile('DoAnSSODiagram out'); {$EndIf}
end;

procedure JustElevationMoments(DEMSWanted : tDEMbooleanArray; aTitle : shortstring; Whiskers : boolean = true; StringGrid : boolean = false);
begin
   SaveBackupDefaults;
   MDDef.ElevMoments := Whiskers;
   MDDef.GraphsOfMoments := false;
   MDDef.SlopeMoments := false;
   MDDef.RoughnessMoments := false;
   MDDef.StringGridWithMoments := StringGrid;
   ElevMomentReport(DEMSWanted,aTitle);
   RestoreBackupDefaults;
end;

procedure ElevationAndSlopeMoments(DEMSWanted : tDEMbooleanArray; aTitle : shortstring);
begin
   SaveBackupDefaults;
   MDDef.ElevMoments := true;
   MDDef.GraphsOfMoments := false;
   MDDef.SlopeMoments := true;
   MDDef.RoughnessMoments := false;
   MDDef.StringGridWithMoments := true;
   ElevMomentReport(DEMSWanted,aTitle);
   RestoreBackupDefaults;
end;

procedure JustSlopeMoments(DEMSWanted : tDEMbooleanArray; aTitle : shortstring);
begin
   SaveBackupDefaults;
   MDDef.ElevMoments := false;
   MDDef.GraphsOfMoments := false;
   MDDef.SlopeMoments := true;
   MDDef.RoughnessMoments := false;
   MDDef.StringGridWithMoments := true;
   ElevMomentReport(DEMSWanted,aTitle);
   RestoreBackupDefaults;
end;


procedure ElevMomentReport(DEMSWanted : tDEMbooleanArray; aTitle : shortstring; SamplingCheck : boolean = false; CurDEM : integer = 0; Memo1 : tMemo = Nil);
var
   {$IfDef AllowCurvatureStatistics} PlanCurvFiles,ProfCurvFiles, {$EndIf}
   LegendFiles,ElevFiles,SlopeFiles,RoughFiles : tStringList;
   DEMsDone,OnLine : integer;
   GridForm : TGridForm;
   MaxSlope,MinElev,MaxElev,MaxRough : float64;
   ElevDist,SlopeDist,RufDist : tStringList;
   GridLimits: tGridLimits;

      procedure MomentReportForDEM(CurDEM : integer);
      label
         Done;
      var
         Incr : integer;
         MomentVar : tMomentVar;
         zvs : ^bfarray32;

               function MomentResults : shortstring;
               begin
                  Result := DEMGlb[CurDEM].AreaName + MomentResultsToString(MomentVar);
               end;

      begin
         {$IfDef RecordElevMoment} WriteLineToDebugFile('Start DEM=' + DEMGlb[CurDEM].AreaName); {$EndIf}
         New(zvs);
         inc(DEMsDone);
         ShowHourglassCursor;
         OnLine := 3;
         GridLimits := DEMGlb[CurDEM].FullDEMGridLimits;
         if (Memo1 <> Nil) then Memo1.Lines.Add(DEMGlb[CurDEM].AreaName);
         if MDDef.ElevMoments then begin
            if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start elev');
            DEMGlb[CurDEM].ElevationMomentsWithArray(GridLimits,MomentVar,zvs^);
            if (MinElev < MomentVar.MinZ) then MinElev := MomentVar.MinZ;
            if (MaxElev < MomentVar.MaxZ) then MaxElev := MomentVar.MaxZ;
            if MDDef.StringGridWithMoments then MomentsToStringGrid(GridForm.StringGrid1,OnLine,DEMsDone,ZUnitCategory(DEMGlb[CurDEM].DEMHeader.ElevUnits),MomentVar);
            if MDDef.GraphsOfMoments then ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
            ElevDist.Add(MomentResults);
            {$IfDef RecordElevMoment} WriteLineToDebugFile('Elev done, range=' + RealToString(MomentVar.MinZ,-12,1) + ' to ' + RealToString(MomentVar.MaxZ,-12,1)); {$EndIf}
         end;

         if MDDef.SlopeMoments then begin
           if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start slope');
           DEMGlb[CurDEM].SlopeMomentsWithArray(GridLimits, MomentVar,zvs^);
           if MDDef.StringGridWithMoments then MomentsToStringGrid(GridForm.StringGrid1,OnLine,DEMsDone,'Slope',MomentVar);
           if (MomentVar.MaxZ > MaxSlope) then MaxSlope := MomentVar.MaxZ;
           if MDDef.GraphsOfMoments then SlopeFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
           SlopeDist.Add(MomentResults);
           {$IfDef RecordElevMoment} WriteLineToDebugFile('Slope done, max=' + RealToString(MomentVar.MaxZ,-12,1)); {$EndIf}
         end;

         if MDDef.RoughnessMoments then begin
           if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start roughness');
           DEMGlb[CurDEM].RoughnessMomentsWithArray(GridLimits, MomentVar,zvs^);
           if MDDef.StringGridWithMoments then MomentsToStringGrid(GridForm.StringGrid1,OnLine,DEMsDone,'Roughness',MomentVar);
           if (MomentVar.MaxZ > MaxRough) then MaxRough := MomentVar.MaxZ;
           if MDDef.GraphsOfMoments then RoughFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
           RufDist.Add(MomentResults);
           {$IfDef RecordElevMoment} WriteLineToDebugFile('Roughness done, max=' + RealToString(MomentVar.MaxZ,-12,1)); {$EndIf}
         end;

         Incr := DEMGlb[CurDEM].GetSamplingSize(GridLimits);
         Incr := Incr * MDDef.StatSampleIncr;

         Dispose(zvs);

         if MDDef.StringGridWithMoments then begin
            GridForm.StringGrid1.Cells[DEMsDone,0] := DEMGlb[CurDEM].AreaName;
            GridForm.StringGrid1.Cells[DEMsDone,1] := RealToString(DEMGlb[CurDEM].AverageSpace,-12,2);
            GridForm.StringGrid1.Cells[DEMsDone,2] := IntToStr(Incr);
            GridForm.StringGrid1.Cells[0,0] := 'DEM';
            GridForm.StringGrid1.Cells[0,1] := 'Avg Grid Space';
            GridForm.StringGrid1.Cells[0,2] := 'Sampling';
            GridForm.StringGrid1.ColCount:= succ(DEMsDone);
            GridForm.StringGrid1.RowCount := OnLine;
            GridForm.Caption := aTitle + ' Moment report';
         end;
         ShowDefaultCursor;
      end;

      procedure DoOneSampling(Incr : integer);
      begin
         if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start ' + IntToStr(Incr));
         LegendFiles.Add('Sampling=' + IntToStr(Incr));
         MDDef.StatSampleIncr := Incr;
         MomentReportForDEM(CurDEM);
      end;

var
   j,Done,ToDo,db,db2 : integer;
   gr : tThisBaseGraph;
begin {procedure ElevMomentReport}
   {$IfDef RecordElevMoment} WriteLineToDebugFile('ElevMomentReport in'); {$EndIf}
   SetColorForProcessing;
   SaveBackupDefaults;
   if MDDef.GraphsOfMoments then begin
      ElevFiles := tStringList.Create;
      SlopeFiles := tStringList.Create;
      RoughFiles := tStringList.Create;
      LegendFiles := tStringList.Create;
   end;

   if MDDef.StringGridWithMoments then GridForm := TGridForm.Create(Application);

   DEMsDone := 0;
   MaxSlope := -1;
   MaxRough := -1;
   MaxElev := -99999;
   MinElev := 9999999;

   ElevDist := tStringList.Create;
   ElevDist.Add(MomentStr);
   SlopeDist := tStringList.Create;
   SlopeDist.Add(MomentStr);
   RufDist := tStringList.Create;
   RufDist.Add(MomentStr);

   if (CurDEM = 0) then begin
      Done := 0;
      ToDo := 0;
      for j := 1 to MaxDEMDataSets do if DEMsWanted[j] then inc(ToDo);

      for j := 1 to MaxDEMDataSets do if DEMsWanted[j] and ValidDEM(j) then begin
         inc(Done);
         if (Memo1 <> Nil) then Memo1.Lines.Add('DEM ' + IntToStr(Done) + '/' + IntToStr(ToDo) );
         MomentReportForDEM(j);
         if MDDef.GraphsOfMoments then LegendFiles.Add(DEMGlb[j].AreaName);
      end;

      if (RufDist.Count > 1) then begin
         StringList2CSVtoDB(RufDist,NextFileNumber(MDTempDir,'roughness_','.dbf'));
         if MDDef.GraphsOfMoments then CreateMultipleHistogram(MDDef.CountHistograms,RoughFiles,LegendFiles,'Roughness (%)','Roughness distribution',200,0,Trunc(MaxRough + 0.99),0.25);
      end
      else RufDist.Free;

      if (SlopeDist.Count > 1) then begin
         if MDDef.GraphsOfMoments then CreateMultipleHistogram(MDDef.CountHistograms,SlopeFiles,LegendFiles,'Slope (%)','Slope distribution',200,0,Trunc(MaxSlope + 0.99),MDDef.SlopeHistBinSize);
         db := StringList2CSVtoDB(SlopeDist,NextFileNumber(MDTempDir,'slope_','.dbf'));
         DB2 := SortDataBase(db,false,true,'STD_DEV');
         CloseAndNilNumberedDB(db);
         gr := StartBoxPlot(DB2,'','Slope (%)');
         SaveImageAsBMP(Gr.Image1,NextFileNumber(MDTempDir,aTitle + '_boxplot_','.png'));
      end
      else SlopeDist.Free;

      if (ElevDist.Count > 1) then begin
         if MDDef.GraphsOfMoments then CreateMultipleHistogram(MDDef.CountHistograms,ElevFiles,LegendFiles,'Elevation/grid','Elevation/grid distribution',200,MinElev,MaxElev,MDDef.ElevHistBinSize);
         db := StringList2CSVtoDB(ElevDist,NextFileNumber(MDTempDir,aTitle + '_','.dbf'));
         DB2 := SortDataBase(db,false,true,'STD_DEV');
         CloseAndNilNumberedDB(db);
         gr := StartBoxPlot(DB2,'','Elevation (m)');
         SaveImageAsBMP(Gr.Image1,NextFileNumber(MDTempDir,aTitle + '_boxplot_','.png'));
      end
      else ElevDist.Free;
      if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' Processing over');
      {$IfDef RecordElevMoment} WriteLineToDebugFile('DEMs complete'); {$EndIf}
   end
   else begin
      if ValidDEM(CurDEM) then begin
         {$IfDef RecordElevMoment} WriteLineToDebugFile('Just DEM=' + IntToStr(CurDEM)); {$EndIf}
         if SamplingCheck then begin
            MDDef.CountHistograms := false;
            DoOneSampling(1);
            DoOneSampling(2);
            DoOneSampling(3);
            DoOneSampling(4);
            DoOneSampling(5);
            DoOneSampling(7);
            DoOneSampling(10);
         end
         else MomentReportForDEM(CurDEM);
      end;
   end;

   if MDDef.StringGridWithMoments then GridForm.SetFormSize;
   RestoreBackupDefaults;
   SetColorForWaiting;
   ShowDefaultCursor;
   {$IfDef RecordElevMoment} WriteLineToDebugFile('ElevMomentReport out'); {$EndIf}
end {procedure ElevMomentReport};


   procedure SetUpGraph(WhichDEM : integer; var ThisGraph : tThisBaseGraph; VertLabel : shortstring);
   var
      i : integer;
   begin
      ThisGraph := TThisBaseGraph.Create(Application);
      ThisGraph.Caption := 'Aspect distribution ' + DEMGlb[WhichDEM].AreaName;
      ThisGraph.GraphDraw.HorizLabel := 'Aspect direction';
      ThisGraph.GraphDraw.VertLabel := VertLabel;
      //ThisGraph.GraphDraw.LegendList := tStringList.Create;
      for i := 1 to 6 do ThisGraph.GraphDraw.FileColors256[i] := ConvertTColorToPlatformColor(WinGraphColors(i));
   end;

   procedure FinishGraph(var ThisGraph : tThisBaseGraph);
   begin
      ThisGraph.AutoScaleAndRedrawDiagram;
      ThisGraph.GraphDraw.SetShowAllLines(true);
      ThisGraph.GraphDraw.SetShowAllPoints(false);
      ThisGraph.GraphDraw.MaxHorizAxis := 360;
      ThisGraph.RedrawDiagram11Click(Nil);
   end;


(*
procedure AspectDistributionByVATCategory(WhichDEM,AspDEM : Integer; GridLimits : tGridLimits);
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
begin {AspectDistributionByVATCategory}
   theFiles := tstringlist.Create;
   for i := 1 to 6 do AspectStats[i].Create(WhichDEM);
   TotPts := 0;
   StartProgress('Aspects');
   for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
      if (Row mod 25 = 0) then UpdateProgressBar((Row-GridLimits.YGridLow)/(GridLimits.YGridHigh-GridLimits.YGridLow));
      for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          if DEMGlb[WhichDEM].GetSlopeAndAspect(MDDef.SlopeCompute,Col,Row,SlopeAspectRec) then begin
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

   SetUpGraph(WhichDEM,ThisGraph,'Concentration of values');
   SetUpGraph(WhichDEM,ThisGraph2,'Number of values');
   for i := 1 to 6 do begin
     MenuStr := GetSlopeLabel(i);
     ThisGraph.OpenDataFile(rfile,MenuStr);
     ThisGraph2.OpenDataFile(rfile2,MenuStr);
     for j := 0 to 359 do begin
        v[1] := j;
        v[2] := 360 * AspectStats[i].AspectFreqValsTrue[j] / AspectStats[i].NPts;
        BlockWrite(rfile,v,1);
        v[2] := AspectStats[i].AspectFreqValsTrue[j];
        BlockWrite(rfile2,v,1);
     end;
     CloseFile(rfile);
     CloseFile(rfile2);


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
  MakeBigBitmap(theFiles,'','',3);
end {AspectDistributionByVATCategory};
*)


procedure AspectDistributionBySlope(WhichDEM : Integer; GridLimits : tGridLimits);
var
   Col,Row,i,j,TotPts  : integer;
   ThisGraph,ThisGraph2 : TThisBaseGraph;
   RoseGraph : array[1..8] of TThisBaseGraph;
   rfile,rfile2 : file;
   v : tGraphPoint32;
   theFiles : tstringlist;
   MenuStr : shortstring;
   fName : PathStr;
   SlopeAspectRec : tSlopeAspectRec;
   AspectStats : array[1..8] of tAspectStats;

      function GetSlopeLabel(i : integer) : shortstring;
      begin
         case i of
            1 : Result := '0-' + RealToString(MdDef.GeomorphSlopeCut[1],-8,-2) + '%';
            2 : Result := RealToString(MdDef.GeomorphSlopeCut[1],-8,-2) + '-' + RealToString(MdDef.GeomorphSlopeCut[2],-8,-2) + '%';
            3 : Result := RealToString(MdDef.GeomorphSlopeCut[2],-8,-2) + '-' + RealToString(MdDef.GeomorphSlopeCut[3],-8,-2) + '%';
            4 : Result := RealToString(MdDef.GeomorphSlopeCut[3],-8,-2) + '-' + RealToString(MdDef.GeomorphSlopeCut[4],-8,-2) + '%';
            5 : Result := RealToString(MdDef.GeomorphSlopeCut[4],-8,-2) + '-' + RealToString(MdDef.GeomorphSlopeCut[5],-8,-2) + '%';
            6 : Result := RealToString(MdDef.GeomorphSlopeCut[5],-8,-2) + '-' + RealToString(MdDef.GeomorphSlopeCut[6],-8,-2) + '%';
            7 : Result := '>' + RealToString(MdDef.GeomorphSlopeCut[6],-8,-2) + '%';
            8 : Result := 'all points';
         end;
         Result := ' Slope ' + Result;
      end;


begin
   theFiles := tstringlist.Create;
   for i := 1 to 8 do AspectStats[i].Create(WhichDEM);
   TotPts := 0;
   StartProgress('Aspects');
   for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
      if (Row mod 25 = 0) then UpdateProgressBar((Row-GridLimits.YGridLow)/(GridLimits.YGridHigh-GridLimits.YGridLow));
      for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          if DEMGlb[WhichDEM].GetSlopeAndAspect(MDDef.SlopeCompute,Col,Row,SlopeAspectRec) then begin
             if (MDDef.UseSealevel) or (abs(SlopeAspectRec.z) > 0.001) then begin
                if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[1] then AspectStats[1].AddPoint(SlopeAspectRec)
                else if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[2] then AspectStats[2].AddPoint(SlopeAspectRec)
                else if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[3] then AspectStats[3].AddPoint(SlopeAspectRec)
                else if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[4] then AspectStats[4].AddPoint(SlopeAspectRec)
                else if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[5] then AspectStats[5].AddPoint(SlopeAspectRec)
                else if SlopeAspectRec.SlopePercent < MdDef.GeomorphSlopeCut[6] then AspectStats[6].AddPoint(SlopeAspectRec)
                else AspectStats[7].AddPoint(SlopeAspectRec);
                AspectStats[8].AddPoint(SlopeAspectRec);
                inc(TotPts);
             end;
          end;
      end;
   end;
   EndProgress;

   SetUpGraph(WhichDEM,ThisGraph,'Concentration of values');
   SetUpGraph(WhichDEM,ThisGraph2,'Number of values');
   for i := 1 to 8 do begin
     ThisGraph.OpenDataFile(rfile,GetSlopeLabel(i));
     ThisGraph2.OpenDataFile(rfile2,GetSlopeLabel(i));
     for j := 0 to 359 do begin
        v[1] := j;
        v[2] := 360 * AspectStats[i].AspectFreqValsTrue[j] / AspectStats[i].NPts;
        BlockWrite(rfile,v,1);
        v[2] := AspectStats[i].AspectFreqValsTrue[j];
        BlockWrite(rfile2,v,1);
     end;
     CloseFile(rfile);
     CloseFile(rfile2);
     MenuStr := GetSlopeLabel(i);
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

  MakeBigBitmap(theFiles,'','',3);
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
      MDDef.SlopeCompute.AlgorithmName := Method;
      New(AspectFreq);
      for j := 0 to 360 do AspectFreq^[j] := 0;
      StartProgress('Aspects');
      for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
         if (Col mod 25 = 0) then UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
         for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
             if DEMGlb[WhichDEM].GetSlopeAndAspect(MDDef.SlopeCompute,Col,Row,SlopeAspectRec) then begin
                 if (not MDDef.UseSealevel) or (abs(SlopeAspectRec.z) > 0.001) then inc(AspectFreq^[round(SlopeAspectRec.AspectDirTrue)]);
             end;
         end;
      end;
      EndProgress;
      AspectFreq^[0] := AspectFreq^[0] + AspectFreq^[360];
      AspectFreq^[360] := 0;

      RoseGraph := TThisBaseGraph.Create(Application);
      RoseGraph.ClientHeight := 400;
      RoseGraph.ClientWidth := 400;
      RoseGraph.RoseColor := WinGraphColors(i);
      RoseGraph.DrawAspectRose(AspectFreq^,SlopeMethodName(MDDef.SlopeCompute));

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
      inc(i);
   end;
   MakeBigBitmap(theBitmaps, 'Aspect: ' + DEMGlb[WhichDEM].AreaName);
   RestoreBackupDefaults;
end;


function GridCorrelationMatrix(Which : tGridCorrelationMatrix; DEMsWanted : tDEMbooleanArray; Title : shortstring; Incr : integer = 1; SaveName : PathStr = '') : DEMStringGrid.TGridForm;
var
   i,NumDEMs : integer;
   DEMsOrdered : tDEMIntegerArray;
begin
   NumDEMs := 0;
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) and DEMsWanted[i] then begin
         inc(NumDEMs);
         DEMsOrdered[NumDEMs] := i;
      end;
   end;
   Result := GridCorrelationMatrix(Which,NumDEMs,DEMsOrdered,Title,Incr);
end;



function GridCorrelationMatrix(Which : tGridCorrelationMatrix; NumDEM : integer; DEMsOrdered : tDEMIntegerArray; Title : shortstring; Incr : integer = 1; SaveName : PathStr = '') : DEMStringGrid.TGridForm;
type
  tCorrs = array[1..MaxDEMDataSets,1..MaxDEMDataSets] of float64;
var
  r2,MeanDiff2,MeanAbsDiff2,
  r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff : float64;
  i,n,j,xoffset,yoffset : Integer;
  NPts : int64;
  Symmetrical : boolean;
  Findings : tStringList;
  MenuStr : ansistring;
  Corrs : ^tCorrs;
  Metrics : tStringList;
  Mean,Std : float32;
  TStr : shortstring;
  Graph : tThisBaseGraph;
begin
  {$IfDef RecordGridCorrrelations} WriteLineToDebugFile('GridCorrelationMatrix in ' + Title); {$EndIf}

//would be good to thread
   MenuStr := 'DEM/Grid';
   for i := 1 to NumDEM do begin
      if ValidDEM(DEMsOrdered[i]) then begin
         {$IfDef RecordGridCorrrelationsFull} WriteLineToDebugFile(IntToStr(i) + '  ' + DEMGlb[DEMsOrdered[i]].AreaName); {$EndIf}
         MenuStr := MenuStr + ',' + DEMGlb[DEMsOrdered[i]].AreaName;
      end;
   end;
   Findings := tStringList.Create;
   Findings.Add(MenuStr);
   New(Corrs);

   StartProgress('Grid correlations');
   n := 0;
   for i := 1 to NumDEM do begin
      if ValidDEM(DEMsOrdered[i]) then begin
         inc(n);
         UpdateProgressBar(n/NumDEMDataSetsOpen);
         for j := i to NumDEM do begin
            if ValidDEM(DEMsOrdered[j]) then begin
               //if the grids do not have points at the same locations, we will use each in turn to interpolate
               Symmetrical := DEMglb[DEMsOrdered[i]].SecondGridJustOffset(DEMsOrdered[j],xoffset,yoffset);
               TStr := 'Compute Grids ' + IntToStr(DEMsOrdered[i]) + ' and ' + IntToStr(DEMsOrdered[J]);
               wmDEM.SetPanelText(1,TStr,true);
               if (i = j) then begin
                  if (Which = gcmR) then Corrs^[i,j] := 1
                  else Corrs^[i,j] := -999;
               end
               else begin
                  {$IfDef RecordGridCorrrelationsFull} WriteLineToDebugFile(TStr); {$EndIf}
                  if (not Symmetrical) then CovariancesFromTwoGrids(DEMGlb[DEMsOrdered[j]].FullDEMGridLimits,DEMsOrdered[j],DEMsOrdered[i],NPts,r2,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff2,MeanAbsDiff2);
                  if CovariancesFromTwoGrids(DEMGlb[DEMsOrdered[i]].FullDEMGridLimits,DEMsOrdered[i],DEMsOrdered[j],NPts,r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff) then begin
                     if (Which = gcmR) then begin
                        Corrs^[i,j] := r;
                        if Symmetrical then Corrs^[j,i] := r
                        else begin
                           Corrs^[j,i] := r2;
                           WriteLineToDebugFile(DEMglb[DEMsOrdered[i]].AreaName + ',' + DEMglb[DEMsOrdered[j]].AreaName + ',' + RealToString(r,-12,-6) + ',' + RealToString(r2,-12,-6));
                        end;
                     end
                     else if (Which = gcmMAbD) then begin
                        Corrs^[i,j] := MeanAbsDiff;
                        if Symmetrical then Corrs^[j,i] := MeanAbsDiff
                        else Corrs^[j,i] := MeanAbsDiff2;
                     end
                     else begin
                        Corrs^[i,j] := MeanDiff;
                        if Symmetrical then Corrs^[j,i] := -MeanDiff
                        else Corrs^[j,i] := MeanDiff2;
                     end;
                  end;
               end;
            end;
         end;
      end;
   end;

   for i := 1 to NumDEM do begin
      if ValidDEM(DEMsOrdered[i]) then begin
         MenuStr := DEMGlb[DEMsOrdered[i]].AreaName;
         for j := 1 to NumDEM do begin
            if ValidDEM(DEMsOrdered[j]) then begin
               MenuStr := MenuStr + ',' + RealToString(Corrs^[i,j],-18,8);
            end;
         end;
         Findings.Add(MenuStr);
      end;
   end;

   EndProgress;
   Dispose(Corrs);
   if (SaveName = '') then SaveName := Petmar.NextFileNumber(MDTempDir,'grid_r_matrix_', '.csv');
   Findings.SaveToFile(SaveName);
   Findings.Free;
   Result := DEMStringGrid.OpenCorrelationMatrix(Title,SaveName);
   Result.DoR := Which;

   if (Which = gcmMAbD) then Result.LegUnits := 'MAbD'
   else if (Which = gcmMAvD) then Result.LegUnits := 'MAvD';
   (*
   for i := 1 to NumDEM do begin
      if ValidDEM(DEMsOrdered[i]) then begin
         if (DEMGlb[DEMsOrdered[i]].DEMHeader.DEMUsed = ArcSecDEM) then begin
            Result.URstring := RealToString(DEMGlb[DEMsOrdered[i]].DEMHeader.DEMxSpacing * 3600, -8,-2) + '" DEM';
         end
         else begin
            Result.URstring := RealToString(DEMGlb[DEMsOrdered[i]].DEMHeader.DEMxSpacing, -8,-1) + ' m DEM';
         end;
      end;
   end;
   *)
   wmDEM.SetPanelText(1,'Matrix',true);
   Result.BitBtn6Click(Nil);

   Metrics := tStringList.Create;
   Metrics.Add('GRID,MINIMUM,MEAN,MAXIMUM,STD_DEV,NUM_PIXELS');
   for i := 1 to NumDEM do begin
      if ValidDEM(DEMsOrdered[i])  then begin
         DEMGlb[DEMsOrdered[i]].ElevationStatistics(DEMGlb[DEMsOrdered[i]].FullDEMGridLimits,Mean,Std,NPts);
         Metrics.Add(DEMGlb[DEMsOrdered[i]].AreaName + ',' + RealToString(DEMGlb[DEMsOrdered[i]].DEMHeader.MinElev,-12,-6) + ',' + RealToString(Mean,-12,-6) + ',' +
                                RealToString(DEMGlb[DEMsOrdered[i]].DEMHeader.MaxElev,-12,-6) + ',' + RealToString(Std,-12,-6) + ',' + IntToStr(NPts));
      end;
   end;
   SaveName := Petmar.NextFileNumber(MDTempDir,'grid_statistics_', '.dbf');
   StringList2CSVtoDB(Metrics,SaveName);
   wmDEM.ClearStatusBarPanelText;
end;

{$EndIf}


{$IfDef ExGeoStats}
{$Else}
procedure FastFourierTransform(WantedDEM : integer; GridLimits: tGridLimits; var SlopeByColumn,SlopeByRow : float64; CloseGraphs : boolean = false);
var
   FFTFile : file;
   FName   : PathStr;
   Size,x,y,Good,Bad : integer;
   z : float32;
   a,r : float64;
   Vals    : array[0..MaxElevArraySize] of float32;

         procedure SetUpAnalysis(NumRecs : integer);
         var
            x : integer;
         begin
            {$IfDef RecordFFT} WriteLineToDebugFile('SetUpAnalysis in, NumRecs=' + IntToStr(NumRecs));     {$EndIf}
            Size := 2;
            While (Size < NumRecs) do Size := Size * 2;
            assignFile(FFTfile,FName);
            rewrite(FFTFile,Size * SizeOf(float64));
            for x := 0 to MaxElevArraySize do Vals[x] := 0;
            Good := 0;
            Bad := 0;
            if ShowSatProgress then StartProgress('Set up FFT');
         end;

         procedure SetUpGraph(Title : ShortString; AverageSpace : float32; var Slope : float64);
         var
            FFTGraph : TFFTGraph;
         begin
            CloseFile(FFTFile);
            EndProgress;
            Slope := -9999;
            {$IfDef RecordFFT} WriteLineToDebugFile('FFT set up graph (good/bad): ' + IntToStr(Good) + '/' + IntToStr(Bad)); {$EndIf}
            if (Good > 3 * Bad) then begin
               FFTGraph := TFFTGraph.Create(Application);
               with FFTGraph,GraphDraw do begin
                  ACaption :=  'FFT Power spectrum: ' + Title + DEMGlb[WantedDEM].AreaName;
                  {if (not BaseGraf.CreateGraphHidden) then} FFTGraph.Caption := ACaption;
                  FFTFileName := FName;
                  TotalNumberPoints := Size;
                  Double := false;
                  PowerTables := false;
                  ShowGraphProgress := false;
                  BinTime := AverageSpace;
                  BinUnits := ' (' + ElevUnitsAre(DEMGlb[WantedDEM].DEMheader.ElevUnits) + ')';
               end;
               {$IfDef RecordFFT} WriteLineToDebugFile('FFT set up graph initialized'); {$EndIf}
               if FFTGraph.FastFourierTransform then begin
                  {$IfDef RecordFFT} WriteLineToDebugFile('FFTGraph.FastFourierTransform done'); {$EndIf}
                  if (FFTGraph.GraphDraw.DataFilesPlotted.Count > 0) then FFTGraph.GetSlopeLofOfValues(false,a,Slope,r);
               end;
               if CloseGraphs then FFTGraph.Close;
            end;
         end;


begin
   {$IfDef RecordFFT} WriteLineToDebugFile('FastFourierTransform in, WantedDEM=' + IntToStr(WantedDEM) + '  '  + DEMGlb[WantedDEM].AreaName); {$EndIf}
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
   {$IfDef RecordFFT} WriteLineToDebugFile('FastFourierTransform out'); {$EndIf}
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
   DEMGlb[DEMToUse].ComputeVariogram(GridLimits);
   {$IfDef RecordGridScatterGram} WriteLineToDebugFile('SemiVariogram for DEM=' + intToStr(DEMtoUse) + ' out'); {$EndIf}
end {proc SemiVariogram};

{$EndIf}


procedure ComputeKappa(RefGrid,TestGrid : integer; RefGridLimits : tGridLimits; var Kappa,OverallAccuracy,AvgUsers,AvgProd : float32);
//results verified with WbT
const
   MaxClass = 16;
var
   ConfusionMatrix : array[1..MaxClass,1..MaxClass] of int64;
   RowTotal,ColumnTotal : array[1..MaxClass] of int64;
   Users,Producers : array[1..MaxClass] of float64;
   i,j,n,Col,Row,Ref,Test,xoffset,yoffset : integer;
   z1,z2 : float32;
   SumProducts : float64;
   TotalN,SumCorrect : int64;
begin
   for i := 1 to MaxClass do begin
      RowTotal[i] := 0;
      ColumnTotal[i] := 0;
      for j := 1 to MaxClass do
         ConfusionMatrix[i,j] := 0;
   end;

   DEMglb[refGrid].SecondGridJustOffset(TestGrid,xoffset,yoffset);
   TotalN := 0;
   for Col := RefGridLimits.XGridLow to RefGridLimits.XGridHigh do begin
      for Row := RefGridLimits.YGridLow to RefGridLimits.YGridHigh do begin
          if DEMGlb[RefGrid].GetElevMetersOnGrid(Col,Row,z1) and DEMGlb[TestGrid].GetElevMetersOnGrid(Col+xoffset,Row+yoffset,z2) then begin
             Ref := round(z1);
             inc(ColumnTotal[Ref]);
             Test := round(z2);
             inc(RowTotal[Test]);
             inc(ConfusionMatrix[Test,Ref]);
             inc(TotalN);
          end;
      end;
   end;

   SumCorrect := 0;
   for i := 1 to MaxClass do
      SumCorrect := SumCorrect + ConfusionMatrix[i,i];

   SumProducts := 0;
   for i := 1 to MaxClass do
      SumProducts := SumProducts + 1.0 * RowTotal[i] * ColumnTotal[i];

   Kappa := ((TotalN * SumCorrect) - SumProducts) / (sqr(TotalN) - SumProducts);
   OverallAccuracy := SumCorrect / TotalN;

   AvgUsers := 0;
   n := 0;
   for i := 1 to MaxClass do begin
       if (RowTotal[i] <> 0) then begin
          Users[i] := ConfusionMatrix[i,i] / RowTotal[i];
          AvgUsers := AvgUsers + Users[i];
          inc(n);
       end;
   end;
   AvgUsers := AvgUsers / n;

   AvgProd := 0;
   n := 0;
   for j := 1 to MaxClass do begin
       if ColumnTotal[j] <> 0 then begin
          Producers[j] := ConfusionMatrix[j,j] / ColumnTotal[j];
          AvgProd := AvgProd + Producers[j];
          inc(n);
       end;
   end;
   AvgProd := AvgProd / n;
   {$IfDef RecordKappa} WriteLineToDebugFile('Ref=' + DEMGlb[RefGrid].AreaName + ' Test=' + DEMGlb[TestGrid].AreaName + ' KAPPA=' + RealToString(1-Kappa,-12,-4)); {$EndIf}
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
end.



