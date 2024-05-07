{$F+,O+}

unit DEMStat;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$Define NoParallelLag}


{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef Debug }
      //{$Define NoParallelFor}
      //{$Define RecordDEMIX_colors}
      //{$Define RecordSSIM}
      //{$Define RecordSSIMFull}
      {$Define RecordDEMIX}
      {$Define RecordFUVsteps}
      //{$Define RecordDEMIXFull}
      //{$Define TrackPixelIs}
      //{$Define RecordCovarianceFail}
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
      //{$Define RecordSSO}
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
      //{$Define RecordHistogram}
      //{$Define RecordGridScatterGram}
   {$Else}
      {$Define RecordDEMIX}
      {$Define RecordFUVsteps}
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

   SysUtils,Forms,Classes,Controls,Graphics,ExtCtrls,Math,StdCtrls,
   System.Threading,System.SyncObjs,System.UITypes,System.IOUtils, StrUtils,System.Diagnostics,
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
      procedure ElevMomentReport(DEMSWanted : tDEMbooleanArray; aTitle : shortstring; Memo1 : tMemo; SamplingCheck : boolean; GridLimits: tGridLimits; CurDEM : integer = 0);
      procedure AspectDistributionBySlope(WhichDEM : Integer; GridLimits : tGridLimits);
      procedure AspectDistributionByAlgorithm(WhichDEM : Integer; GridLimits : tGridLimits);

      procedure GridDiffernces(EntireDEM : boolean);
      function MakeDifferenceMap(Map1,Map2,GridResultionToUse,GridToMergeShading : integer; ShowMap,ShowHistogram,ShowScatterPlot : boolean; TheAreaName : ShortString = '') : integer;
      function MakeDifferenceMapOfBoxRegion(Map1,Map2,GridResultionToUse,GridToMergeShading : integer; GridLimits: tGridLimits; ShowMap,ShowHistogram,ShowScatterplot : boolean; TheAreaName : ShortString = '') : integer;

      function CovariancesFromTwoGrids(GridLimitsDEM1 : tGridLimits; DEM1,DEM2 : integer; var r,covar,Mean1,Mean2,StdDev1,StdDev2 : float64; NoteFailure : boolean = true) : boolean;    inline;
      procedure ElevationSlopePlot(WhichDEMs : tDEMbooleanArray; DesiredBinSize : integer = 1; Memo : tMemo = Nil);

      procedure DoAnSSODiagram(CurDEM : integer; GridLimits : tGridLimits);
      function GridRatio(Map1Num : integer = 0; Map2Den : integer = 0; inMapType : tMapType = mtDEMBlank) : integer;
      function PercentDifferentTwoGrids(RefDEM,TestDEM : integer; fName : PathStr) : integer;

      procedure GridMinimum;
      procedure SlopeRegionSize(CurDEM : integer; DoRegionSize : boolean = true);
      procedure MakeElevSlopeAspectDifferenceMap;
      procedure PointSlopesByRegionSize(DEM : integer; RightClickLat,RightClickLong : float64);

      procedure SingleDEMHistogram(WhichDEM : integer; Quick : boolean = false);
      procedure DoElevationHistograms;

      procedure CalculateGrainProfile(MapForm : tMapForm; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64);
      function IwashuriPikeColor(Slope,Convexity,Texture : float64; var TerClass : integer) : tPlatformColor;  //inline;
      function IwashuriPikeCat(Slope,Convexity,Texture : float64) : integer; //inline;
      procedure IandPLegend(pc : array of float64);

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
      {$IfDef ExWaveLengthHeight}
      {$Else}
          procedure ComputeDunecrestspacingheight(MapForm : tMapForm; GridLimits : tGridLimits; Memo1 : tMemo);
          procedure ThreadCrest1Click(MapForm : tMapForm; StartLat,StartLong : float64; var Results : tStringList; CrestNum : integer);
          procedure CrestsAlongProfile(theLOSView : TDEMLOSF; var Results : tStringList; Memo1 : tMemo = Nil);
      {$EndIf}

  {$EndIf}

  {$IfDef ExCompare}
  {$Else}
      procedure MissingPointsInGrids(DEM1 : integer = 0; DEM2 : integer = 0);
      function GridScatterGram(GridLimits : tGridLimits; DEM1 : integer = 0; DEM2 : integer = 0) : TThisBaseGraph;
      procedure GridCoOccurrence(AutoFull : boolean = false; DEM1 : integer = 0; DEM2 : integer = 0; Percentages : boolean = true);
      procedure DBCoOccurrence(Table : tMyData; EmpSource: TDataSource; Field1,Field2 : ShortString; Percentages : boolean);
  {$EndIf}

   function SumDEMs(FirstDEM : integer; Merge : tDEMbooleans; NewName : shortstring; OpenMap : boolean = true; AllGridsValidZ : boolean = true) : integer;
   procedure AddaDEM(FirstDEM,AddDEM : integer; Mult : integer = 1);

   function MakeChangeMap(Map1,Map2 : integer; GridLimits: tGridLimits) : integer;
   procedure GridCorrelationMatrix(Incr : integer = 1);

   procedure HistogramPointCloudAndGlobalDEMs(DB : integer = 0; Title : shortString = '');
   procedure DirtAndAirShots(DB : integer; Title : shortString);
   procedure CloudSummaryGlobalDEMs(DB : integer);
   function FiveSeriesGraph(DB : integer; Lat,Long,Tolerance : float64; DirField : shortstring) : TThisbasegraph;
   procedure IcesatProcessCanopy(dbOnTable : integer; AddDEMs : boolean; LimitDecimals : boolean = false);
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

   procedure HistogramsFromVATDEM(DEMwithVAT,ElevMap,SlopeMap,RuffMap,AspMap : integer; var Graph1,Graph2,Graph3,Graph4 : tThisBaseGraph);
   procedure CreateGridHistograms(DEMSwanted : tDEMbooleanArray; TailCutoff : float32 = 0.5);


//ssim operations
   function ComputeSSIM(DEM1,DEM2 : integer; gl1,gl2 : tGridLimits; var SSIM,Luminance,Contrast,Structure : float64) : boolean; //inline;
   procedure AreaSSIMandFUVComputations(Overwrite : boolean; Areas : tStringList = nil);
   procedure NormalizeDEMforSSIM(DEM : integer; What : shortstring);
   function MakeSSIMMap(OpenMap,AlreadyNormalized : boolean; DEM1,DEM2,NumberOfGrids,WindowSize : integer; ThinFactor : integer = 1; AreaName : shortstring = '') : integer;
   procedure SSIMcheck(DoThinning : boolean);


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
   md_use_tools,
   icesat_filter_form,
   PETMath;

const
   InsuffDEMrelief = 'Insufficient DEM relief';
type
   PlotModeType = (Plain,Strahler,Cumulative,NormProb);
var
   Count : array[1..MaxDEMDataSets] of ^CountType;


{$I demstat_global_dems.inc}

{$I demstat_ssim.inc}

{$IfDef ExWaveLengthHeight}
{$Else}
   {$I demstat_dune_crest.inc}
{$EndIf}

{$IfDef ExCompare}
{$Else}
   {$I demstat_grid_compare.inc}
{$EndIf}



procedure CreateGridHistograms(DEMSwanted : tDEMbooleanArray; TailCutoff : float32 = 0.5);
var
   j : integer;
   Distributions,Legends : tStringList;
   Values : ^Petmath.bfarray32;
   Max,Min,BinSize,ThisMin,ThisMax : float32;
   NPts : int64;
   fName : PathStr;
begin
   if (TailCutoff < 0) then ReadDefault('Tail cutoff (%)',TailCutoff);
   Distributions := tStringList.Create;
   Legends := tStringList.Create;
   Max := -99e39;
   Min := 99e39;
   for j := 1 to MaxDEMDataSets do begin
      if DEMsWanted[j] then begin
         New(Values);
         DEMGlb[j].GetElevationsInLongArray(DEMGlb[j].FullDEMGridLimits,NPts,Values^);

         ThisMax := Values^[round(NPts * (100 - TailCutoff) / 100)];
         ThisMin := Values^[round(NPts * TailCutoff / 100)];

         if (ThisMax > Max) then Max := ThisMax;
         if (ThisMin < Min) then Min := ThisMin;
         fName := Petmar.NextFileNumber(MDtempDir,DEMGlb[j].AreaName + '_' ,'.z');
         Distributions.Add(SaveSingleValueSeries(npts,Values^,fName));
         Legends.Add(DEMGlb[j].AreaName);
         Dispose(Values);
      end;
   end;
   BinSize := (Max - Min) / 200;
   CreateMultipleHistogram(MDDef.CountHistograms,Distributions,Legends,'', 'DEM/Grid histograms',200,Min,Max,BinSize);
end;


procedure HistogramsFromVATDEM(DEMwithVAT,ElevMap,SlopeMap,RuffMap,AspMap : integer; var Graph1,Graph2,Graph3,Graph4 : tThisBaseGraph);
//creates histograms of elevation, slope, roughness, and aspect for each category in VAT grid
const
   MaxCodes = 100;
Type
   tDistCount = array[1..MaxCodes] of integer;
   pbfarray32 = ^bfarray32;
   tDistArray = array[1..MaxCodes] of pbfarray32;
   tNames = array[1..MaxCodes] of shortstring;
var
   VAT : integer;
   i,j,NumCodes : integer;
   Codes : array[1..MaxCodes] of integer;
   Names : tNames;
   Colors : tStringList;
   CountField : shortstring;
   ElevDist,SlopeDist,RuffDist,AspDist : tDistArray;
   ElevDistCount,SlopeDistCount,RuffDistCount,AspDistCount : tDistCount;

   procedure CreateDistribution;
   var
      Col,Row,ThisCode,i : integer;
      zf : float32;
      Lat,Long : float64;

            function MakeOneHistogram(DEM : integer; BinSize : float32; Names : tNames;  DistCount : tDistCount; Dist : tDistArray; aTitle : shortstring; Max : float32) : tThisBaseGraph;
            var
               i : integer;
               fName : PathStr;
               ElevFiles,LegendFiles : tStringList;
            begin
               if ValidDEM(DEM) then begin
                  {$IfDef RecordHistogramFromVAT} HighlightLineToDebugFile(aTitle + '  MakeOneHistogram in, dem=' + IntToStr(DEM)); {$EndIf}
                  ElevFiles := tStringList.Create;
                  LegendFiles := tStringList.Create;
                  for i := 1 to NumCodes do begin
                     if (DistCount[i] > 0) then begin
                        fName := NextFileNumber(MDTempDir,'dist_file_','.z');
                        SaveSingleValueSeries(DistCount[i],Dist[i]^,fName);
                        ElevFiles.Add(fName);
                        LegendFiles.Add(Names[i]);
                        {$IfDef RecordHistogramFromVAT} HighlightLineToDebugFile(Names[i] + '  ' + Colors[pred(i)] + '  ' + fName); {$EndIf}
                     end;
                  end;
                  Result := CreateMultipleHistogram(MDDef.CountHistograms,ElevFiles,LegendFiles,BeforeSpecifiedCharacterANSI(aTitle,' '),aTitle,100,DEMglb[DEM].DEMheader.MinElev,Max,BinSize,Colors);
               end;
            end;

   begin
      {$IfDef RecordHistogramFromVAT} WriteLineToDebugFile('Create Distribution in'); {$EndIf}
      for Col := 0 to pred(DEMGlb[DEMwithVAT].DEMheader.NumCol) do begin
         for Row := 0 to pred(DEMGlb[DEMwithVAT].DEMheader.NumRow) do begin
            if DEMGlb[DEMwithVAT].GetElevMetersOnGrid(Col,Row,zf) then begin
              ThisCode := round(zf);
               for I := 1 to succ(NumCodes) do begin
                  if (i = succ(NumCodes)) then ThisCode := 0;
                  
                  if ThisCode = Codes[i] then begin
                     ThisCode := i;
                     break;
                  end;
               end;

               if (ThisCode > 0) then begin
                  DEMGlb[DEMwithVAT].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                  if ValidDEM(ElevMap) and (DEMGlb[ElevMap].GetElevFromLatLongDegree(Lat,long,zf)) then begin
                     inc(ElevDistCount[ThisCode]);
                     ElevDist[ThisCode]^[ElevDistCount[ThisCode]] := zf;
                  end;
                  if ValidDEM(SlopeMap) and (DEMGlb[SlopeMap].GetElevFromLatLongDegree(Lat,long,zf)) then begin
                     inc(SlopeDistCount[ThisCode]);
                     SlopeDist[ThisCode]^[SlopeDistCount[ThisCode]] := zf;
                  end;
                  if ValidDEM(RuffMap) and (DEMGlb[RuffMap].GetElevFromLatLongDegree(Lat,long,zf)) then begin
                     inc(RuffDistCount[ThisCode]);
                     RuffDist[ThisCode]^[RuffDistCount[ThisCode]] := zf;
                  end;
                  if ValidDEM(AspMap) and (DEMGlb[AspMap].GetElevFromLatLongDegree(Lat,long,zf)) then begin
                     inc(AspDistCount[ThisCode]);
                     AspDist[ThisCode]^[AspDistCount[ThisCode]] := zf;
                  end;
               end;
            end;
         end;
      end;

      MDDef.HistElevBinSize := 20;
      MDDef.HistSlopeBinSize := 1;
      MDDef.HistRuffBinSize  := 0.5;
      MDDef.HistAspectBinSize := 2;

      MDDef.DoElevHist := true;
      MDDef.DoSlopeHist := true;
      MDDef.DoRuffHist := true;
      MDDef.DoAspectHist := true;

      if MDDef.DoElevHist then Graph1 := MakeOneHistogram(ElevMap,MDDef.HistElevBinSize,Names,ElevDistCount,ElevDist,'Elevation (m)',DEMglb[ElevMap].DEMheader.MaxElev);
      if MDDef.DoSlopeHist then Graph2 := MakeOneHistogram(SlopeMap,MDDef.HistSlopeBinSize,Names,SlopeDistCount,SlopeDist,'Slope (%)',100);
      if MDDef.DoRuffHist then Graph3 := MakeOneHistogram(RuffMap,MDDef.HistRuffBinSize,Names,RuffDistCount,RuffDist,'Roughness (%)',100);
      if MDDef.DoAspectHist then Graph4 := MakeOneHistogram(AspMap,MDDef.HistAspectBinSize,Names,AspDistCount,AspDist,'Aspect (' + DegSym + ')',360);
   end;


const
   PtRequiredInCat = 2500;
var
   Sum : float64;
begin
   {$IfDef RecordHistogramFromVAT} WriteLineToDebugFile('HistogramsFromVATDEM in, DEM=' + IntToStr(DEMwithVAT)); {$EndIf}
   if not ValidDEM(DEMwithVAT) then exit;
   if OpenNumberedGISDataBase(VAT,DEMGlb[DEMwithVAT].VATFileName) then begin
      if (not GISdb[VAT].MyData.FieldExists(GISdb[VAT].dbOpts.LabelField)) then GISdb[VAT].dbOpts.LabelField := GISdb[VAT].PickField('label field',[ftstring],true);
      CountField := 'COUNT';
      if (not GISdb[VAT].MyData.FieldExists(CountField)) then CountField := 'N';
      Sum := GISdb[VAT].MyData.FieldSum(CountField);
      GISdb[VAT].ApplyGISFilter(CountField + '>=' + IntToStr(PtRequiredInCat) );
      NumCodes := 0;
      Colors := tStringList.Create;
      while not GISdb[VAT].MyData.eof do begin
         inc(NumCodes);
         Codes[NumCodes] := GISdb[VAT].MyData.GetFieldByNameAsInteger('VALUE');
         Colors.Add(IntToStr(GISdb[VAT].MyData.TColorFromTable));
         Names[NumCodes] := RealToString(100 * GISdb[VAT].MyData.GetFieldByNameAsInteger(CountField) / Sum, 8,2) + '%   ' + GISdb[VAT].MyData.GetFieldByNameAsString(GISdb[VAT].dbOpts.LabelField);
        {$IfDef RecordHistogramFromVAT} WriteLineToDebugFile(IntToStr(NumCodes) + IntegerToString(Codes[NumCodes],8) + '  ' + Names[NumCodes]); {$EndIf}
         GISdb[VAT].MyData.Next;
         if (NumCodes >= MaxCodes) then begin
            MessageToContinue('Reached number of codes limit=' + IntToStr(MaxCodes));
            Break;
         end;
      end;
      GISdb[VAT].MyData.Destroy;

      for i := 1 to MaxCodes do begin
         if ValidDEM(ElevMap) then new(ElevDist[i]);
         if ValidDEM(SlopeMap) then new(SlopeDist[i]);
         if ValidDEM(RuffMap) then new(RuffDist[i]);
         if ValidDEM(AspMap) then new(AspDist[i]);
         ElevDistCount[i] := 0;
         SlopeDistCount[i] := 0;
         RuffDistCount[i] := 0;
         AspDistCount[i] := 0;
      end;

      CreateDistribution;

      for i := 1 to MaxCodes do begin
         if ValidDEM(ElevMap) then Dispose(ElevDist[i]);
         if ValidDEM(SlopeMap) then Dispose(SlopeDist[i]);
         if ValidDEM(RuffMap) then Dispose(RuffDist[i]);
         if ValidDEM(AspMap) then Dispose(AspDist[i]);
      end;
      Colors.Destroy;
   end;
   {$IfDef RecordHistogramFromVAT} WriteLineToDebugFile('HistogramsFromVATDEM out'); {$EndIf}
end;



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
  i,it,nt,NumStrips : integer;
  Results : array[1..MaxThreadsAllowed] of tStringList;
  StripBoxLimits: tGridLimits;
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
    fName : PathStr;
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
   if (BMPlist.Count > 0) then begin
      MakeBigBitmap(BMPlist,'Aspect roses');
   end
   else BMPlist.Free;
end;


procedure PointSlopesByRegionSize(DEM : integer; RightClickLat,RightClickLong : float64);
var
   xDEMg1,yDEMg1 : float32;
   Findings : tStringList;
   xField,yField,aCapt,
   TStr,location : shortString;
   i,db,Col,Row : integer;
   graph : tThisBaseGraph;

         procedure GetTheSlopes(BoxSize : integer);
         var
            SlopeAsp : tSlopeAspectRec;
         begin
            MDDef.SlopeRegionRadius := BoxSize;
            if DEMGlb[DEM].GetSlopeAndAspect(Col,Row,SlopeAsp) then begin
               Findings.Add(Location + ',' + IntToStr(BoxSize) + ',' + RealToString(BoxSize* DEMGlb[DEM].AverageYSpace,-12,-2) + ',' +
                     RealToString(SlopeAsp.SlopePercent,-12,2) + ',' + RealToString(SlopeAsp.SlopeDegree,-12,2) + ',' + RealToString(SlopeAsp.AspectDir,-8,2));
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
   Graph := GISDB[db].CreateScatterGram(xField,yField,true,aCapt);
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
          zvs : ^Petmath.bfarray32;
          MomentVar : tMomentVar;
          Row : integer;
       begin
          inc(RegionsDone);
          MDDef.SlopeRegionRadius := BoxSize;
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
           eLat1,eLong1,eLat2,eLong2,MissingPC,Distance,Bearing : float64;
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


function SumDEMs(FirstDEM : integer; Merge : tDEMbooleans; NewName : shortstring; OpenMap : boolean = true; AllGridsValidZ : boolean = true) : integer;
label
   MissingData;
var
   i,j,Col,Row : integer;
   Lat,Long : float64;
   z,z2 : float32;
   IdenticalGrids : boolean;
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
      if OpenMap then DEMGlb[Result].SetUpMap(Result,true,DEMGlb[FirstDEM].SelectionMap.MapDraw.MapType);
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
   BestClass,Sampler,i,j,x,y,NPts,db : integer;
   Limits : tGridLimits;
   aMin,zf : float32;
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
             DEMGlb[i].ElevationStatistics(DEMGlb[i].FullDEMGridLimits,Mean[i],Std[i]);
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
   DEMGlb[Result].SetUpMap(Result,true,mtDEMVATTable);
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
   Value,Lat,Long,Mean1,Mean2,StdDev1,StdDev2    : float64;
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
                  CovariancesFromTwoGrids(DEMGlb[MG.Grids[i]].FullDEMGridLimits,MG.Grids[i],MG.Grids[j],Correlations[i,j],VarCoVar[i,j],Mean1,Mean2,StdDev1,StdDev2);
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
                  DEMGlb[NewDEM].SetUpMap(NewDEM,false,mtElevGray);
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



function CovariancesFromTwoGrids(GridLimitsDEM1 : tGridLimits; DEM1,DEM2 : integer; var r,covar,Mean1,Mean2,StdDev1,StdDev2 : float64; NoteFailure : boolean = true) : boolean;
var
   Col,Row,xoff,yoff,i : integer;
   NPts : int64;
   z1,z2,XGrid,YGrid : float32;
   Lat,Long : float64;
   IdenticalGrids : boolean;
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

   {$If Defined(RecordCovariance)}
      WriteLineToDebugFile('DEM1, ' + DEMglb[DEM1].KeyDEMParams(true));
      WriteLineToDebugFile('DEM2, ' + DEMglb[DEM2].KeyDEMParams(true));
      if IdenticalGrids then  WriteLineToDebugFile('offset, x=' + IntToStr(xoff) + '  y=' + IntToStr(yoff));
      WriteLineToDebugFile('DEM1 grid limits ' + GridLimitsToString(GridLimitsDEM1));
   {$EndIf}

   Col := GridLimitsDEM1.XGridLow;
   while (Col <= GridLimitsDEM1.XGridHigh) do begin
      Row := GridLimitsDEM1.YGridLow;
      while (Row <= GridLimitsDEM1.YGridHigh) do begin
         if DEMGlb[DEM1].GetElevMeters(Col,Row,z1) then begin
            if IdenticalGrids then begin
               if DEMGlb[DEM2].GetElevMeters(Col+xoff,Row+yoff,z2) then begin
                  Sum[1] := Sum[1] + z1;
                  Sum[2] := Sum[2] + z2;
                  sp[1] := sp[1] + z1 * z1;
                  sp[2] := sp[2] + z2 * z2;
                  SPc := SPc + z1 * z2;
                  inc(NPts);
               end;
            end
            else begin
               DEMGlb[DEM1].DEMGridToLatLongDegree(Col,Row,Lat,Long);
               if DEMGlb[DEM2].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                  Sum[1] := Sum[1] + z1;
                  Sum[2] := Sum[2] + z2;
                  sp[1] := sp[1] + z1 * z1;
                  sp[2] := sp[2] + z2 * z2;
                  SPc := SPc + z1 * z2;
                  inc(NPts);
               end;
            end;
         end;
         inc(Row);
      end;
      inc(Col);
   end;
   Result := (NPts > 0);
   if Result then begin
      Mean1 := (Sum[1] / NPts);
      StdDev1 := sqrt( (NPts * SP[1] - (sum[1] * sum[1]) ) / (NPts-1) / Npts );
      Mean2 := (Sum[2] / NPts);
      StdDev2 := sqrt( (NPts * SP[2] - (sum[2] * sum[2]) ) / (NPts-1) / Npts );
      Covar := (NPts * SPc - Sum[1] * Sum[2]) / NPts / pred(NPts);
      r := Covar / StdDev1 / StdDev2;
      {$If Defined(RecordCovariance)}
         WriteLineToDebugFile('npts=' + IntToStr(NPts));
         WriteLineToDebugFile('Mean1=' + RealToString(Mean1,8,2) + ' std dev1=' + RealToString(StdDev1,8,2) + '  ' + DEMglb[DEM1].AreaName);
         WriteLineToDebugFile('Mean2=' + RealToString(Mean2,8,2) + ' std dev2=' + RealToString(StdDev2,8,2) + '  ' + DEMglb[DEM2].AreaName);
      {$EndIf}
      {$If Defined(RecordStat) or Defined(RecordCovariance)}  WriteLineToDebugFile('CovariancesFromTwoGrids out covar=' + RealToString(covar,-12,-4) + '  r=' + RealToString(r,-12,-6)); {$EndIf}
   end
   else begin
      {$If Defined(RecordStat) or Defined(RecordCovariance) or Defined(RecordCovarianceFail)}
         if NoteFailure then WriteLineToDebugFile('CovariancesFromTwoGrids failed, npts=' + IntToStr(NPts) + ' DEM1=' + DEMglb[DEM1].AreaName +  ' DEM2=' + DEMglb[DEM2].AreaName );
      {$EndIf}
   end;
end;



{$IfDef ExGeoStats}
{$Else}


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
         Which,x,CurDEM : integer;
         rfile,rfile2,rfile3 : file;
         v       : tGraphPoint32;
         MenuStr,TStr,bsString : ShortString;
      begin
         if Graph in [AspectRose] then begin
            Result := AspectStats.CreateRose;
         end
         else begin
             Result := TThisBaseGraph.Create(Application);
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
             if (NumDEMs = 1) then TStr := DEMGlb[CurDEM].AreaName + '  ' + TStr;
             if (not BaseGraf.CreateGraphHidden) then Result.Caption := TStr;
             case Graph of
               ElevFreq  : Result.GraphDraw.MaxHorizAxis := MaxCount;
               ElevRuff : begin
                              Result.GraphDraw.MaxHorizAxis := MaxRuff + 5;
                              Result.GraphDraw.HorizLabel := 'Avg Roughness (' + IntToStr(MDDef.RoughnessBox) + 'x' + IntToStr(MDDef.RoughnessBox) + ' window std dev slope, %)';
                           end;
               ElevSlope : begin
                              Result.GraphDraw.MaxHorizAxis := MaxSlope + 5;
                              Result.GraphDraw.HorizLabel := 'Avg Slope ' + SlopeMethodName(MdDef.SlopeAlg)+' (%)';
                           end;
               ElevSlopeDeg : begin
                              Result.GraphDraw.MaxHorizAxis := arcTan(0.01*MaxSlope) / DegToRad;
                              Result.GraphDraw.HorizLabel := 'Avg Slope ' + SlopeMethodName( MDDef.SlopeAlg)+' (°)';
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

            for CurDEM := 1 to MaxDEMDataSets do if WhichDEMs[CurDEM] and ValidDEM(CurDEM) then begin
               //CurDEM := Which;
               If (Memo <> nil) then begin
                  Memo.Lines.Add(TimeToStr(Now) + '  ' + DEMGlb[CurDEM].AreaName);
                  Application.ProcessMessages;
               end;
               Result.GraphDraw.LegendList.Add(DEMGlb[CurDEM].AreaName);
               Result.OpenDataFile(rfile);
               Result.GraphDraw.FileColors256[CurDEM] := ConvertTColorToPlatformColor(WinGraphColors[CurDEM mod 16]);


               LoadDEMIXnames;
               Result.GraphDraw.FileColors256[CurDEM] := DEMIXColorFromDEMName(DEMGlb[CurDEM].AreaName);
               {$IfDef RecordDEMIX_colors} WriteLineToDebugFile('DEMStat ' + DEMGlb[CurDEM].AreaName + '  ' + ColorStringFromPlatformColor(Result.GraphDraw.FileColors256[CurDEM])); {$EndIf}


               if (Graph in [ElevSlope,ElevSlopeDeg,ElevRuff])  then begin
                  Result.GraphDraw.ShowLine[Result.GraphDraw.DataFilesPlotted.Count] := true;
               end;
               if (Graph in [ElevSlope,ElevSlopeDeg]) and (NumDEMs = 1) and (MDdef.ShowSDonElevSlope) and (MDdef.ShowGeomorphometry) then begin
                  Result.OpenDataFile(Rfile2);
                  Result.OpenDataFile(Rfile3);
                  Result.GraphDraw.LineSize256[2] := 1;
                  Result.GraphDraw.LineSize256[3] := 1;
               end;
               if (Graph = ElevFreq) then begin
                  for x := 0 to NumBins[CurDEM] do if Count[CurDEM]^[x] > 0 then begin
                     v[1] := Count[CurDEM]^[x];
                     v[2] := BinElev[CurDEM]^[x];
                     BlockWrite(Rfile,v,1);
                  end;
                  Result.GraphDraw.ShowLine[Result.GraphDraw.DataFilesPlotted.Count] := true;
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
         LegendBMP.Canvas.Font.Color := WinGraphColors[CurDEM];
         LegendBMP.Canvas.TextOut(15,LegendY,DEMGlb[CurDEM].AreaName);
         inc(LegendY,25);
      end;
      New(Count[CurDEM]);
      New(SumSquares[CurDEM]);
      New(BinElev[CurDEM]);
      New(Slopes[CurDEM]);
      New(Ruffs[CurDEM]);
      New(SlopeHist[CurDEM]);
      for Dir := Low(Dir) to High(Dir) do PointCount[Dir] := 0;

      MaxDEMSlope[CurDEM] := 0;
      for x := MinSlopeValue to MaxSlopeValue do SlopeHist[CurDEM]^[x] := 0;

      for x := MinSlopeValue to MaxSlopeValue do begin
         Slopes[CurDEM]^[x] := 0;
         Ruffs[CurDEM]^[x] := 0;
         SumSquares[CurDEM]^[x] := 0;
         Count[CurDEM]^[x] := -0.0000001;
      end {for x};

      SlopeTotal[CurDEM] := 0;
      SlopeSqrTotal[CurDEM] := 0;
      TotalDEM[CurDEM] := 0;
      {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('Setup over, DEM=' + IntToStr(CurDEM) + '  increment=' + IntToStr(MDdef.StatSampleIncr)); {$EndIf}

      StartProgress('Stats ' + DEMGlb[CurDEM].AreaName);
      x := 1;
      while x <= (DEMGlb[CurDEM].DEMheader.NumCol - 2) do begin
         UpDateProgressBar( x / DEMGlb[CurDEM].DEMheader.NumCol);
         {$IfDef RecordElevationSlopePlotAll} WriteLineToDebugFile('x=' + IntToStr(x)); {$EndIf}
         j := 1;
         while j <= (DEMGlb[CurDEM].DEMheader.NumRow - 2) do begin
            if DEMGlb[CurDEM].GetSlopeAndAspect(x,j,SlopeAspectRec) and DEMGlb[CurDEM].RoughnessFromSlopeSTD(x,j,MDDef.RoughnessBox,Ruff1) then begin
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
                  Ruffs[CurDEM]^[Bin] := Ruffs[CurDEM]^[Bin] + Ruff1;
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
            Ruffs[CurDEM]^[x] := Ruffs[CurDEM]^[x] / Count[CurDEM]^[x];
            if Ruffs[CurDEM]^[x] > MaxRuff then MaxRuff := Ruffs[CurDEM]^[x];
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
   {$IfDef RecordElevationSlopePlot} WriteLineToDebugFile('ElevationSlopePlot Out'); {$EndIf}
end;


procedure CalculateGrainProfile(MapForm : tMapForm; DEMonMap : integer; Lat1,Long1,Lat2,Long2 : float64);
const
   MaxBoxes = 10;
var
   xgrid,ygrid : float32;
   Trend,s1s2,s2s3 : float64;
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
            Results.add('   V' + IntToStr(i) +  RealToString(SSOvars.TheDips[i],10,4) +  '° toward' + RealToString(SSOvars.TheDipDirs[i],8,1) +  '°');
         end {for i};
         Results.add('Queen''s aspect ratio:' + RealToString(SSOvars.QueensAspect,8,3));
         Petmar.DisplayAndPurgeStringList(Results,'SSO results ' + DEMGlb[CurDEM].AreaName);
      end
      else MessageToContinue('No results');
   end;
   RestoreBackupDefaults;
   {$IfDef RecordSSO} WriteLineToDebugFile('DoAnSSODiagram out'); {$EndIf}
end;


procedure ElevMomentReport(DEMSWanted : tDEMbooleanArray; aTitle : shortstring; Memo1 : tMemo; SamplingCheck : boolean; GridLimits: tGridLimits; CurDEM : integer = 0);
var
   {$IfDef AllowCurvatureStatistics} PlanCurvFiles,ProfCurvFiles, {$EndIf}
   LegendFiles,ElevFiles,SlopeFiles,RoughFiles : tStringList;
   DEMsDone,OnLine,LinesPer : integer;
   GridForm : TGridForm;
   MaxSlope,MinElev,MaxElev,MaxRough : float64;
   ElevDist,SlopeDist,RufDist : tStringList;

      procedure MomentReportForDEM(CurDEM : integer);
      label
         Done;
      var
         Col,Row,Incr,RuffGrid  : integer;
         Ruff1 : float32;
         Slope : float64;
         {$IfDef AllowCurvatureStatistics}
            SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve : float64;
         {$EndIf}
         MomentVar : tMomentVar;
         zvs,zvs2 : ^Petmath.bfarray32;


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
         if (Memo1 <> Nil) then Memo1.Lines.Add(DEMGlb[CurDEM].AreaName);
         if MDDef.ElevMoments then begin
            if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start elev');
            DEMGlb[CurDEM].ElevationMomentsWithArray(GridLimits,MomentVar,zvs^);
            if (MinElev < MomentVar.MinZ) then MinElev := MomentVar.MinZ;
            if (MaxElev < MomentVar.MaxZ) then MaxElev := MomentVar.MaxZ;
            MomentsToStringGrid(GridForm.StringGrid1,OnLine,DEMsDone,ZUnitCategory(DEMGlb[CurDEM].DEMHeader.ElevUnits),MomentVar);
            if MDDef.GraphsOfMoments then ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
            ElevDist.Add(MomentResults);
            {$IfDef RecordElevMoment} WriteLineToDebugFile('Elev done, range=' + RealToString(MomentVar.MinZ,-12,1) + ' to ' + RealToString(MomentVar.MaxZ,-12,1)); {$EndIf}
         end;

         if MDDef.SlopeMoments then begin
           if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start slope');
           DEMGlb[CurDEM].SlopeMomentsWithArray(GridLimits, MomentVar,zvs^);
           MomentsToStringGrid(GridForm.StringGrid1,OnLine,DEMsDone,'Slope',MomentVar);
           if (MomentVar.MaxZ > MaxSlope) then MaxSlope := MomentVar.MaxZ;
           if MDDef.GraphsOfMoments then SlopeFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
           Dispose(zvs);
           SlopeDist.Add(MomentResults);
            {$IfDef RecordElevMoment} WriteLineToDebugFile('Slope done, max=' + RealToString(MomentVar.MaxZ,-12,1)); {$EndIf}
         end;

         if MDDef.RoughnessMoments then begin
           if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start roughness');
           New(zvs);
           MomentVar.NPts := 0;
           DEMGlb[CurDEM].RoughnessMomentsWithArray(GridLimits, MomentVar,zvs^);
           MomentsToStringGrid(GridForm.StringGrid1,OnLine,DEMsDone,'Roughness',MomentVar);
           if (MomentVar.MaxZ > MaxRough) then MaxRough := MomentVar.MaxZ;
           if MDDef.GraphsOfMoments then RoughFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs^));
           RufDist.Add(MomentResults);
           {$IfDef RecordElevMoment} WriteLineToDebugFile('Roughness done, max=' + RealToString(MomentVar.MaxZ,-12,1)); {$EndIf}
         end;


         Incr := DEMGlb[CurDEM].GetSamplingSize(GridLimits);
         Incr := Incr * MDDef.StatSampleIncr;
         GridForm.StringGrid1.Cells[DEMsDone,2] := IntToStr(Incr);

         {$IfDef AllowCurvatureStatistics}
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
                   inc(Online,LinesPer);
                end;
                if MDDef.PlanCurvMoments then begin
                   MomentReport('Plan curvature',zvs2^,MomentVar.npts,'',GridForm.StringGrid1,OnLine,DEMsDone);
                   if MDDef.GraphsOfMoments then PlanCurvFiles.Add(SaveSingleValueSeries(MomentVar.npts,zvs2^));
                   inc(Online,LinesPer);
                end;
                Dispose(zvs2);
            end;
         {$EndIf}


         Dispose(zvs);

         GridForm.StringGrid1.Cells[DEMsDone,0] := DEMGlb[CurDEM].AreaName;
         GridForm.StringGrid1.Cells[DEMsDone,1] := RealToString(DEMGlb[CurDEM].AverageSpace,-12,2);
         GridForm.StringGrid1.Cells[0,0] := 'DEM';
         GridForm.StringGrid1.Cells[0,1] := 'Avg Grid Space';
         GridForm.StringGrid1.Cells[0,2] := 'Sampling';
         GridForm.StringGrid1.ColCount:= succ(DEMsDone);
         GridForm.StringGrid1.RowCount := OnLine;
         GridForm.Caption := aTitle + ' Moment report';
         ShowDefaultCursor;
      end;


      procedure DoOne(Incr : integer);
      begin
         if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' start ' + IntToStr(Incr));
         LegendFiles.Add('Sampling=' + IntToStr(Incr));
         MDDef.StatSampleIncr := Incr;
         MomentReportForDEM(CurDEM);
      end;

var
   j,Done,ToDo : integer;
begin
   {$IfDef RecordElevMoment} WriteLineToDebugFile('ElevMomentReport in'); {$EndIf}
   SaveBackupDefaults;
   if MDDef.GraphsOfMoments then begin
      ElevFiles := tStringList.Create;
      SlopeFiles := tStringList.Create;
      RoughFiles := tStringList.Create;
      LegendFiles := tStringList.Create;
      {$IfDef AllowCurvatureStatistics}
         PlanCurvFiles := tStringList.Create;
         ProfCurvFiles := tStringList.Create;
      {$EndIf}
   end;

   GridForm := TGridForm.Create(Application);

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

      for j := 1 to MaxDEMDataSets do if DEMsWanted[j] then begin
         inc(Done);
         if (Memo1 <> Nil) then Memo1.Lines.Add('DEM ' + IntToStr(Done) + '/' + IntToStr(ToDo) );
         GridLimits := DEMGlb[j].SelectionMap.MapDraw.MapAreaDEMGridLimits;
         MomentReportForDEM(j);
         if MDDef.GraphsOfMoments then LegendFiles.Add(DEMGlb[j].AreaName);
      end;

      if (RufDist.Count > 1) then begin
         RufDist.SaveToFile(NextFileNumber(MDTempDir,'roughness_raw_','.csv'));
         StringList2CSVtoDB(RufDist,NextFileNumber(MDTempDir,'roughness','.dbf'));
      end
      else RufDist.Free;

      if (SlopeDist.Count > 1) then begin
         SlopeDist.SaveToFile(NextFileNumber(MDTempDir,'slope_raw_','.csv'));
         StringList2CSVtoDB(SlopeDist,NextFileNumber(MDTempDir,'slope','.dbf'));
      end
      else SlopeDist.Free;

      if (ElevDist.Count > 1) then begin
         ElevDist.SaveToFile(NextFileNumber(MDTempDir,'elev_raw_','.csv'));
         StringList2CSVtoDB(ElevDist,NextFileNumber(MDTempDir,'elev','.dbf'));
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
      CreateMultipleHistogram(MDDef.CountHistograms,ElevFiles,LegendFiles,'Elevation/grid','Elevation/grid distribution',200,MinElev,MaxElev,MDDef.ElevHistBinSize);
      CreateMultipleHistogram(MDDef.CountHistograms,SlopeFiles,LegendFiles,'Slope (%)','Slope distribution',200,0,Trunc(MaxSlope + 0.99),MDDef.SlopeHistBinSize);
      CreateMultipleHistogram(MDDef.CountHistograms,RoughFiles,LegendFiles,'Roughness (%)','Roughness distribution',200,0,Trunc(MaxRough + 0.99),0.25);
      {$IfDef AllowCurvatureStatistics}
         if (PlanCurvFiles.Count > 0) then CreateMultipleHistograms(MDDef.CountHistograms,PlanCurvFiles,LegendFiles,'Plan curvature','Plan curvature distribution',200,-10,10,0.1);
         if (ProfCurvFiles.Count > 0) then CreateMultipleHistograms(MDDef.CountHistograms,ProfCurvFiles,LegendFiles,'Profile curvature','Profile curvature distribution',200,-10,10,0.1);
         PlanCurvFiles.Free;
         ProfCurvFiles.Free;
      {$EndIf}
      {$IfDef RecordElevMoment} WriteLineToDebugFile('Done histograms'); {$EndIf}
   end;
   GridForm.SetFormSize;
   RestoreBackupDefaults;
   {$IfDef RecordElevMoment} WriteLineToDebugFile('ElevMomentReport out'); {$EndIf}
end;


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
  r,covar,Mean1,Mean2,StdDev1,StdDev2 : float64;
  i,n: Integer;
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
   n := 0;
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         inc(n);
         UpdateProgressBar(n/NumDEMDataSetsOpen);
         for j := i to MaxDEMDataSets do begin
            if ValidDEM(j) then begin
               if (i = j) then begin
                  Corrs^[i,j] := 1;
               end
               else begin
                  CovariancesFromTwoGrids(DEMGlb[i].FullDEMGridLimits,i,J,r,covar,Mean1,Mean2,StdDev1,StdDev2);
                  Corrs^[i,j] := r;
                  Corrs^[j,i] := r;
               end;
            end;
         end;
      end;
   end;

   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
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
   fName := Petmar.NextFileNumber(MDTempDir,'grid_r_matrix_', '.csv');
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
    {$IfDef RecordHistogram} WriteLineToDebugFile('Convert and load table'); {$EndIf}

    StringList2CSVtoDB(Results,fName,true);
    theDB := DEMGlb[WhichDEM].SelectionMap.LoadDataBaseFile(fName);

   if DEMGlb[WhichDEM].DEMheader.ElevUnits = euImagery then TStr := 'DNs '
   else TStr := 'Elevation ';
   if Quick or MDDef.ShowRegularHistogram then begin
      if MDDef.CountHistograms then GISDB[theDB].CreateScatterGram('CONCENT','Z',true,DEMGlb[WhichDEM].AreaName +  ' ' + TStr + ' Histogram',
           'Concentration (Fraction of Uniform)',  TStr + '(' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')')
      else begin
         if (DEMGlb[WhichDEM].DEMheader.ElevUnits = euImagery) then
              GISDB[theDB].CreateScatterGram('Z','NPTS',true,DEMGlb[WhichDEM].AreaName +  ' ' + TStr + 'Histogram', TStr + '(' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')','Number of Points')
         else GISDB[theDB].CreateScatterGram('NPTS','Z',true,DEMGlb[WhichDEM].AreaName +  ' ' + TStr + 'Histogram','Number of Points', TStr + '(' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')');
      end;
   end;

   if Quick then begin
      CloseAndNilNumberedDB(theDB);
   end
   else begin
      if MDDef.ShowCumulativeHistogram then GISDB[theDB].CreateScatterGram('CUM_PC','Z',true,DEMGlb[WhichDEM].AreaName +  ' Cumulative Elevation Distribution',
          'Cumulative Percentage', 'Elevation (' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')');

      if MDDef.ShowStrahlerHistogram then GISDB[theDB].CreateScatterGram('PROP_AREA','PROP_HGT',true,DEMGlb[WhichDEM].AreaName +  ' Strahler Elevation Distribution',
       'Proportion of Area', 'Proportion of Basin Height');

       if MDDef.ShowNormalHistogram then begin
          GISDB[theDB].CreateScatterGram('Z','CUM_PC',true,DEMGlb[WhichDEM].AreaName +  ' Cumulative ' + TStr + 'Distribution',
            TStr + '(' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')','Cumulative Percentage',true);
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
   z,a,r : float32;
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

         procedure SetUpGraph(Title : ShortString; AverageSpace : float32; var Slope : float32);
         var
            FFTGraph : TFFTGraph;
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
                  ShowGraphProgress := false;
                  BinTime := AverageSpace;
                  BinUnits := ' (' + ElevUnitsAre(DEMGlb[WantedDEM].DEMheader.ElevUnits) + ')';
               end;
               {$IfDef RecordFFT} WriteLineToDebugFile('FFT set up graph initialized');   {$EndIf}
               if FFTGraph.FastFourierTransform then begin
                  {$IfDef RecordFFT} WriteLineToDebugFile('FFTGraph.FastFourierTransform done');   {$EndIf}
                  if (FFTGraph.GraphDraw.DataFilesPlotted.Count > 0) then FFTGraph.GetSlope(false,a,Slope,r);
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


function GridScatterGram(GridLimits : tGridLimits; DEM1 : integer = 0; DEM2 : integer = 0) : TThisBaseGraph;
var
   Incr,Col,Row,NPts,Prog : integer;
   Lat,Long : float64;
   XGrid,YGrid : float32;
   rFile : file;
   v : tFloatPoint;
   IdenticalGrids : boolean;
begin
   if (DEM1 = 0) and (DEM2 = 0) then begin
      IdenticalGrids := GetTwoCompatibleGrids('DEM1=x axis, DEM2= y axis',false,DEM1,DEM2,false,true);
      GridLimits := DEMGlb[DEM1].SelectionMap.MapDraw.MapAreaDEMGridLimits;
   end
   else IdenticalGrids := DEMGlb[DEM1].SecondGridIdentical(DEM2);
   if ValidDEM(DEM1) and ValidDEM(DEM2) then begin
      {$IfDef RecordGridScatterGram}
         WriteLineToDebugFile('Two grid scattergram');
         WriteLineToDebugFile('  DEM 1:' + DEMGlb[DEM1].AreaName + '  ' + DEMGlb[DEM1].KeyDEMParams);
         WriteLineToDebugFile('  DEM 2:' + DEMGlb[DEM2].AreaName + '  ' + DEMGlb[DEM2].KeyDEMParams);
         WriteLineToDebugFile('  ll corner :' + RealToString(DEMGlb[DEM2].Headrecs.hdfSWCornerx,-12,-2) + '   ' + RealToString(DEMGlb[DEM2].Headrecs.hdfSWCornery,-12,-2) );
         WriteLineToDebugFile('Grid from DEM 1: ' + GridLimitsToString(GridLimits));
      {$EndIf}
      SetReasonableGraphSize;

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
         if (Prog mod 100 = 0) then UpdateProgressBar((Col - GridLimits.XGridLow) / (GridLimits.XGridHigh - GridLimits.XGridLow));
         Inc(Prog);
         Row := GridLimits.YGridLow;
         while (Row <= GridLimits.YGridHigh) do begin
            if DEMGlb[DEM1].GetElevMeters(Col,Row,v[1]) then begin
               if DEMGlb[DEM1].GetElevMetersFromSecondDEM(IdenticalGrids,Dem2,Col,Row,v[2]) then begin
                  Result.AddPointToDataBuffer(rfile,v);
                  inc(NPts);
               end;
               (*
               //changed 10/22/2023
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
               *)
            end;
            inc(Row,Incr);
         end;
         inc(Col,Incr);
      end;
      Result.ClosePointDataFile(rfile);
      EndProgress;
      if (NPts > 0) then begin
         Result.AutoScaleAndRedrawDiagram;
         Result.AddCorrelationToCorner;
      end
      else begin
         Result.Close;
         MessageToContinue('No scattergram matches ' + DEMGlb[DEM1].AreaName + ' and ' + DEMGlb[DEM2].AreaName);
      end;
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



