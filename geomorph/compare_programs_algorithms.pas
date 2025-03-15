unit compare_programs_algorithms;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IFDEF DEBUG}
   {$IfDef RecordProblems}   //normally only defined for debugging specific problems
      {$Define RecordCompareLSPs}
   {$EndIf}
{$ELSE}
   //{$Define NoParallelFor}
{$ENDIF}

interface

 uses
//needed for inline of core DB functions
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
//end DB declarations
   SysUtils, Windows, Classes, Graphics, Controls,JPEG,DBCtrls,Math,
   StrUtils,
   System.Types,

   Nevadia_main,
   DEMDefs,
   Petmar_types,PETMAR,PETMath;


procedure CompareSlopeMaps(DEM : integer);
procedure CompareProfileCurvatures(DEM : integer);
procedure ComparePlanCurvatures(DEM : integer);
procedure CompareTangentialCurvature(DEM : integer);
procedure CompareSlopeAndThreeCurvature(DEM : integer);
procedure CompareHillshadeMaps(DEM : integer; OpenMap : boolean = true);
procedure ComparePartialDerivatives(DEM : integer; OpenMap : boolean = true);
procedure CompareTRI(DEM : integer; OpenMap : boolean = true);
procedure CompareAspectMaps(DEM : integer; OpenMap : boolean = true);
procedure CompareContourTorsion(DEM : integer);

procedure CompareMICRODEMslopes(DEM,How : integer; OpenMap : boolean = false);
procedure CompareMICRODEMSlopeMaps(DEM : integer);
procedure CompareMICRODEM_filtered_slopes(DEM : integer; OpenMap : boolean = false);


procedure CompareGDAL_ScaleFactorsForSlope(DEM : integer; OpenMap : boolean = true);


implementation

uses
   DEMCoord,
   DEM_Manager,DEMOptions,
   PetDBUtils,
   DEMDef_routines,Petimage,
   DEMStat,DEMMapf,
   MD_use_tools,
   gdal_tools,
   DEMStringGrid,
   PetImage_Form,
   BaseMap,
   BaseGraf,
   DEMIX_control,
   Make_grid;

const
   //lsp_calc_3d_dir = 'J:\aa_new_zealand\lsp_calc_ponui_30m_3d\';
   //lsp_calc_4d_dir = 'J:\aa_new_zealand\lsp_calc_ponui_30m_4d\';
   lsp_calc_3d_dir = 'J:\aa_new_zealand\shifted_ponui_3d\';
   lsp_calc_4d_dir = 'J:\aa_new_zealand\shifted_ponui_4d\';
   //arc_utm_dir = 'J:\aa_new_zealand\GeomorphometryChapter_ArcGISPro_SlopeCurvature_Output\ponui_island_dtm_utm_egm2008_30m\';
   //arc_geo_dir = 'J:\aa_new_zealand\GeomorphometryChapter_ArcGISPro_SlopeCurvature_Output\ponui_island_dtm_utm_egm2008_1sec\';
   arc_utm_dir =  'J:\aa_new_zealand\GeomorphometryChapter_ArcGISPro_SlopeCurvature_Output\datums_shifted_ponui_island_dtm_ref_30m\';
   arc_geo_dir = 'J:\aa_new_zealand\GeomorphometryChapter_ArcGISPro_SlopeCurvature_Output\datums_shifted_ponui_island_dtm_ref_1sec\';

const
   SAGA_algorithms : array[1..4] of char = ('2','3','6','8');
   SAGA_aNames : array[1..4] of shortstring = ('horn','evans','ZT','flor');

const
   Use5x5 = false;


      procedure PairCompareSSOforFeatures(OpenMap : boolean; DEM,DEM1,DEM2 : integer; Name : shortstring);
      var
         Diff,DiffCat : integer;
      begin
         Diff := MakeDifferenceMap(DEM1,DEM2,DEM1,0,OpenMap,false,false,Name);
         DiffCat := DifferenceCategoryMap(Diff,Name,OpenMap);
         MDDef.SSObyPole := true;
         SSOforVATgrid(DiffCat,0,DEM);
         MDDef.SSObyPole := false;
         SSOforVATgrid(DiffCat,0,DEM);
      end;




procedure CompareGDAL_ScaleFactorsForSlope(DEM : integer; OpenMap : boolean = true);
var
   NewMap1,NewMap2,NewMap3,NewMap4 : integer;
   TStr : shortstring;
   DistanceNS,DistanceEW,DistanceAVG : float64;
   DEMList : tDEMBooleanArray;
   Graph : tThisBaseGraph;
begin
   if DEMglb[DEM].DEMheader.DEMused = ArcSecDEM then begin
      MetersPerDegree(DEMGlb[DEM].DEMSWcornerLat + 0.5 * DEMGlb[DEM].LatSizeMap,DEMGlb[DEM].DEMSWcornerLong + 0.5 * DEMGlb[DEM].LongSizeMap,DistanceNS,DistanceEW,DistanceAVG);
      SaveBackupDefaults;

      InitializeDEMsWanted(DEMList,false);
      MDDef.SlopeCompute.AlgorithmName := smHorn;
      NewMap1 := CreateSlopeMapPercent(OpenMap,DEM,'md_horn_slope_2_spacing');
      DEMList[NewMap1] := true;
      GDAL_testScaleFactor := ' -s ' + RealToString(DistanceAVG,-12,2);
      NewMap2 := GDAL_SlopeMap_Horn(OpenMap,DEM,MDTempDir + 'gdal_horn_slope_average.tif');
      DEMList[NewMap2] := true;

      GDAL_testScaleFactor := ' -s ' + RealToString(DistanceEW,-12,2);
      NewMap3 := GDAL_SlopeMap_Horn(OpenMap,DEM,MDTempDir + 'gdal_horn_slope_x_space.tif');
      DEMList[NewMap3] := true;
      GDAL_testScaleFactor := ' -s ' + RealToString(DistanceNS,-12,2);
      NewMap4 := GDAL_SlopeMap_Horn(OpenMap,DEM,MDTempDir + 'gdal_horn_slope_y_space.tif');
      DEMList[NewMap4] := true;

(*
      NewMap := MakeDifferenceMap(NewMap2,NewMap1,NewMap2,0,true,false,false,'GDAL_average_compared_MICRODEM_Horn');
      DifferenceCategoryMap(NewMap,'GDAL_average_compared_MICRODEM_Horn');

      NewMap := MakeDifferenceMap(NewMap3,NewMap1,NewMap3,0,true,false,false,'GDAL_x_space_compared_MICRODEM_Horn');
      DifferenceCategoryMap(NewMap,'GDAL_x_space_compared_MICRODEM_Horn');

      NewMap := MakeDifferenceMap(NewMap4,NewMap1,NewMap4,0,true,false,false,'GDAL_y_space_compared_MICRODEM_Horn');
      DifferenceCategoryMap(NewMap,'GDAL_y_space_compared_MICRODEM_Horn');
*)

      PairCompareSSOforFeatures(OpenMap,NewMap2,NewMap1,NewMap1,'GDAL_average_compared_MICRODEM_Horn');
      PairCompareSSOforFeatures(OpenMap,NewMap3,NewMap1,NewMap1,'GDAL_x_space_compared_MICRODEM_Horn');
      PairCompareSSOforFeatures(OpenMap,NewMap4,NewMap1,NewMap1,'GDAL_y_space_compared_MICRODEM_Horn');

      TStr := DEMglb[DEM].AreaName + '_horn_geo_slope_histograms';
      Graph := CreateGridHistograms(DEMList,TStr,0);
      SaveBitmap(Graph.AddLegendBesideGraph,MDTempDir + TStr + '.png');


      (*
      InitializeDEMsWanted(DEMList,false);
      MDDef.SlopeAlgorithm := smZevenbergenThorne;
      NewMap := CreateSlopeMapPercent(OpenMap,DEM,'md_zt_slope_2_spacing');
      DEMList[NewMap] := true;
      GDAL_testScaleFactor := ' -s ' + RealToString(DistanceAVG,-12,2);
      NewMap := GDAL_SlopeMap_ZT(OpenMap,DEM,MDTempDir + 'gdal_zt_slope_average.tif');
      DEMList[NewMap] := true;
      GDAL_testScaleFactor := ' -s ' + RealToString(DistanceEW,-12,2);
      NewMap := GDAL_SlopeMap_ZT(OpenMap,DEM,MDTempDir + 'gdal_zt_slope_x_space.tif');
      DEMList[NewMap] := true;
      GDAL_testScaleFactor := ' -s ' + RealToString(DistanceNS,-12,2);
      NewMap := GDAL_SlopeMap_ZT(OpenMap,DEM,MDTempDir + 'gdal_zt_slope_y_space.tif');
      DEMList[NewMap] := true;
      TStr := DEMglb[DEM].AreaName + '_zt_geo_slope_histograms';
      Graph := CreateGridHistograms(DEMList,TStr,0);
      SaveBitmap(Graph.AddLegendBesideGraph,MDTempDir + TStr + '.png');
      *)


      RestoreBackupDefaults;
   end
   else begin
      MessageToContinue('Not arc second DEM');
   end;
end;



procedure CompareMICRODEMslopes(DEM,How : integer; OpenMap : boolean = false);

   procedure OpenOneWindowSize(Which : shortstring; FilterSize : integer);
   var
      Evans,Horn,ZT,LSQ{,Diff,DiffCat} : integer;
      DEMList : tDEMBooleanArray;
      TStr : shortstring;
      //Graph : tThisBaseGraph;
   begin
      InitializeDEMsWanted(DEMList,false);
      MDDef.SlopeCompute.AlgorithmName := smEvansYoung;
      MDDef.SlopeCompute.WindowRadius := FilterSize;
      Evans := CreateSlopeMapPercent(OpenMap,DEM,'md_evans_slope_' + Which);
      DEMlist[Evans] := true;
      MDDef.SlopeCompute.AlgorithmName := smHorn;
      Horn := CreateSlopeMapPercent(OpenMap,DEM,'md_horn_slope_' + Which);
      DEMlist[Horn] := true;
      MDDef.SlopeCompute.AlgorithmName := smZevenbergenThorne;
      ZT := CreateSlopeMapPercent(OpenMap,DEM,'md_zt_slope_' + Which);
      DEMlist[ZT] := true;

      MDDef.SlopeCompute.AlgorithmName := smLSQ;
      LSQ := CreateSlopeMapPercent(OpenMap,DEM,'md_lsq_slope_' + Which);
      DEMlist[LSQ] := true;

      TStr := DEMglb[DEM].AreaName + '_md_slope';
      CreateGridHistograms(DEMList,TStr,0);
      JustElevationMoments(DEMlist,TStr,true,true);

      (*
      PairCompareSSOforFeatures(OpenMap,DEM,Evans,Horn,DEMglb[DEM].AreaName + '_Slope_Evans_Compared_Horn');
      PairCompareSSOforFeatures(OpenMap,DEM,Evans,ZT,DEMglb[DEM].AreaName + '_Slope_Evans_Compared_ZT');
      PairCompareSSOforFeatures(OpenMap,DEM,Horn,ZT,DEMglb[DEM].AreaName + '_Slope_Horn_Compared_ZT');
      *)
   end;

begin
   SaveBackupDefaults;
   if How in [3,99] then OpenOneWindowSize('3x3',1);
   if How in [5,99] then OpenOneWindowSize('5x5',2);
   RestoreBackupDefaults;
end;


procedure CompareMICRODEM_filtered_slopes(DEM : integer; OpenMap : boolean = false);
var
   DEMList : tDEMBooleanArray;
   TStr : shortstring;
   Graph : tThisBaseGraph;
   FeaturePreserve,ParamIso : integer;

   procedure OpenOneFilter(DEM : integer; Which : shortstring; FilterSize : integer);
   var
      Evans,Horn,ZT{,Diff,DiffCat} : integer;
   begin
      MDDef.SlopeCompute.WindowRadius := FilterSize;
      MDDef.SlopeCompute.AlgorithmName := smEvansYoung;
      Evans := CreateSlopeMapPercent(OpenMap,DEM,'md_evans_slope_' + Which);
      DEMlist[Evans] := true;
      MDDef.SlopeCompute.AlgorithmName := smHorn;
      Horn := CreateSlopeMapPercent(OpenMap,DEM,'md_horn_slope_' + Which);
      DEMlist[Horn] := true;
      MDDef.SlopeCompute.AlgorithmName := smZevenbergenThorne;
      ZT := CreateSlopeMapPercent(OpenMap,DEM,'md_zt_slope_' + Which);
      DEMlist[ZT] := true;
   end;

begin
   SaveBackupDefaults;
   FeaturePreserve := WBT_FeaturePreserveSmooth(false, DEMglb[DEM].GeotiffDEMName,euMeters);
   ParamIso := DEMGlb[DEM].FilterThisDEM(false,fcParamIsotrop);
   InitializeDEMsWanted(DEMList,false);

   OpenOneFilter(DEM,'no_filter',1);
   OpenOneFilter(FeaturePreserve,'feature_preserve',1);
   OpenOneFilter(ParamIso,'parametric_isotropic',1);

   TStr := DEMglb[DEM].AreaName + '_md_slope';
   Graph := CreateGridHistograms(DEMList,TStr,0);
   JustElevationMoments(DEMlist,TStr,true,true);
   RestoreBackupDefaults;
end;


procedure ReCaptionMaps(DEMList : tDEMBooleanArray);
var
   i : integer;
begin
   for i := 1 to MaxDEMdataSets do begin
      if DEMList[i] and (DEMGlb[i].SelectionMap <> Nil) then begin
         DEMGlb[i].SelectionMap.MakeAreaNameTheCaption;
      end;
   end;
end;


procedure FilterToFullAnalysisWindow(DEM : integer; DEMList : tDEMBooleanArray);
const
   fil = 3;
var
   i,x,y,OldPts,{NumPts,}MaskDEM : integer;
   //z : float32;
begin
   {$IfDef RecordCompareLSPs} WriteLineToDebugFile('FilterToFullAnalysisWindow using ' + DEMglb[DEM].AreaName + '  pts=' + IntToStr(DEMglb[DEM].ValidElevsInDEM)); {$EndIf}
(*
   MaskDEM := DEMglb[DEM].CloneAndOpenGridSetMissing(byteDEM,'Full_window_mask',euUndefined);
   NumPts := 0;
   for x := 0 to pred(DEMglb[DEM].DEMHeader.NumCol) do begin
      for y := 0 to pred(DEMglb[DEM].DEMHeader.NumRow) do begin
         if DEMglb[DEM].FullAnalysisWindow(x,y,Fil) then begin
            inc(NumPts);
            DEMGlb[MaskDEM].SetGridElevation(x,y,1);
         end
         else DEMGlb[MaskDEM].SetGridMissing(x,y);
      end;
   end;
   DEMglb[MaskDEM].CheckMaxMinElev;
   {$IfDef RecordCompareLSPs}
      WriteLineToDebugFile('FilterToFullAnalysisWindow using ' + DEMglb[DEM].AreaName + '  pts=' + IntToStr(DEMglb[DEM].ValidElevsInDEM) + ' filled=' + IntToStr(NumPts));
      WriteLineToDebugFile('Mask DEM=' + IntToStr(DEMglb[MaskDEM].ValidElevsInDEM));
   {$EndIf}
*)
   MaskDEM := MakeGridFullNeighborhoods(DEM,false,fil);

   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) and DEMList[i] then begin
         OldPts := DEMglb[i].ValidElevsInDEM;
         DEMglb[i].SetGridMissingOutsideBox(fil,fil,(DEMglb[i].DEMHeader.NumCol - fil),(DEMglb[DEM].DEMHeader.NumRow - fil)  );

         for x := fil to (DEMglb[DEM].DEMHeader.NumCol-fil) do begin
            for y := fil to (DEMglb[DEM].DEMHeader.NumRow-fil) do begin
               if (DEMglb[MaskDEM].MissingDataInGrid(x,y)) then begin
                  DEMglb[i].SetGridMissing(x,y);
               end;
            end;
         end;
         DEMglb[i].CheckMaxMinElev;
         {$IfDef RecordCompareLSPs}
            WriteLineToDebugFile('Filtered ' + DEMglb[i].AreaName + ' old pts=' + IntToStr(OldPts) + '  now=' + IntToStr(DEMglb[i].ValidElevsInDEM));
         {$EndIf}
      end;
   end;
   CloseSingleDEM(MaskDEM);
end;


procedure StartComparisonProcess(DEM : integer; var DEMList : tDEMBooleanArray);
var
   Fixed : int64;
begin
   GetDEMIXpaths;
   InitializeDEMsWanted(DEMList,false);
   SaveBackupDefaults;
   DEMGLb[DEM].MarkBelowMissing(1.0,Fixed,false);
end;


procedure EndComparison(DEM : integer; var DEMList : tDEMBooleanArray;  aCaption : shortstring);
var
   CorrelationMatrix : DEMStringGrid.TGridForm;
   fName : PathStr;
   TStr : shortstring;
   List : tStringList;
   DEMsOrdered : tDEMIntegerArray;
   Graph : TThisBaseGraph;
   i,j,NumDEMs : integer;
   Table : tMyData;

   procedure DEMProjections(What : shortstring);
   var
      i : integer;
   begin
      HighLightLineToDebugFile(What);
      for i := 1 to MaxDEMDataSets do begin
         if DEMlist[i] and ValidDEM(i) then WriteLineToDebugFile(DEMGlb[i].AreaName + ' ' + DEMGlb[i].DEMMapProj.GetProjName);
      end;
   end;


begin
   {$IfDef RecordCompareLSPs} WriteLineToDebugFile(aCaption + ' EndComparison in'); {$EndIf}
   {$IfDef RecordCompareLSPs} DEMProjections('Entering EndComparison'); {$EndIf}
   FilterToFullAnalysisWindow(DEM,DEMList);
   ReCaptionMaps(DEMList);
   RestoreBackupDefaults;
   fName := ProgramRootDir + 'algorithm_comparisons.dbf';
   Table := tMyData.Create(fName);
   Table.ApplyFilter('SHORT=' + QuotedStr(aCaption));
   if Table.FiltRecsInDB = 1 then begin
      MDDef.PerfectR := Table.GetFieldByNameAsFloat('R');
      MDDef.PerfectMAbD := Table.GetFieldByNameAsFloat('MABD');
      MDDef.PerfectMAvD := Table.GetFieldByNameAsFloat('MAVD');
      MDDef.DivergenceRange := Table.GetFieldByNameAsFloat('DIFF_MAP');
   end;
   Table.Destroy;
   {$IfDef RecordCompareLSPs} DEMProjections('Setup Done'); {$EndIf}


   if (aCaption = 'slope') and StrUtils.AnsiContainsText(DEMGlb[DEM].AreaName,'ponui')  then begin
      if (DEMGlb[DEM].DEMheader.DEMUsed = ArcSecDEM) then fName := 'J:\aa_new_zealand\geo_slope.txt'
      else fName := 'J:\aa_new_zealand\utm_slope.txt';
      List := tStringList.Create;
      List.LoadFromFile(fName);
      NumDEMs := 0;
      for i := 1 to List.Count do begin
          for j := 1 to MaxDEMDataSets do begin
             if ValidDEM(j) and StrUtils.AnsiContainsText(DEMGlb[j].AreaName,List.Strings[pred(i)]) then begin
                inc(NumDems);
                DEMsOrdered[NumDEMs] := j;
                break;
             end;
          end;
      end;
      {$IfDef RecordCompareLSPs} WriteLineToDebugFile(aCaption + ' EndComparison, NumDEMs=' + IntToStr(NumDEMs)); {$EndIf}
      if MDDef.CalcR then CorrelationMatrix := GridCorrelationMatrix(gcmR,NumDEMs,DEMsOrdered,DEMglb[DEM].AreaName + '_' + aCaption + '_correlation_matrix');
      if MDDef.CalcMAbD then CorrelationMatrix := GridCorrelationMatrix(gcmMAbD,NumDEMs,DEMsOrdered,DEMglb[DEM].AreaName + '_' + aCaption + '_mean_absolute_difference_matrix');
      if MDDef.CalcMAvD then CorrelationMatrix := GridCorrelationMatrix(gcmMAvD,NumDEMs,DEMsOrdered,DEMglb[DEM].AreaName + '_' + aCaption + '_mean_average_difference_matrix');
   end
   else begin
      if MDDef.CalcR then CorrelationMatrix := GridCorrelationMatrix(gcmR,DEMList,DEMglb[DEM].AreaName + '_' + aCaption + '_correlation_matrix');
      if MDDef.CalcMAbD then CorrelationMatrix := GridCorrelationMatrix(gcmMAbD,DEMList,DEMglb[DEM].AreaName + '_' + aCaption + '_mean_absolute_difference_matrix');
      if MDDef.CalcMAvD then CorrelationMatrix := GridCorrelationMatrix(gcmMAvD,DEMList,DEMglb[DEM].AreaName + '_' + aCaption + '_mean_average_difference_matrix');
   end;
   {$IfDef RecordCompareLSPs} DEMProjections('Done correlation matrices'); {$EndIf}

   if MDDef.CalcBoxPlots then JustElevationMoments(DEMlist,aCaption);
   if MDDef.CalcScatterGrams then ScatterGramGrid(True,DEMList,aCaption,DEMglb[DEM].AreaName + '_' + aCaption + '_scattergrams');
   if MDDef.CalcDiffMaps then ScatterGramGrid(false,DEMList,aCaption,DEMglb[DEM].AreaName + '_' + aCaption + '_difference_maps');
   if MDDef.CalcHistoGrams then begin
      TStr := DEMglb[DEM].AreaName + '_' + aCaption + '_histograms';
      if aCaption = 'slope' then Graph := CreateGridHistograms(DEMList,TStr,0)
      else Graph := CreateGridHistograms(DEMList,TStr,0,-MDDef.DivergenceRange,MDDef.DivergenceRange);
      SaveBitmap(Graph.AddLegendBesideGraph,MDTempDir + TStr + '.png');
   end;
   {$IfDef RecordCompareLSPs} DEMProjections('Done all calc graphics'); {$EndIf}
   UpdateMenusForAllMaps;
   EndDEMIXProcessing(0,false);
   (*
   if MDDef.CloseGridsAfterComputing then begin
      for i := 1 to MaxDEMdataSets do begin
         if DEMList[i] then CloseSingleDEM(i);
      end;
   end;
   *)
   {$IfDef RecordCompareLSPs} DEMProjections('Leaving EndComparison'); {$EndIf}
   {$IfDef RecordCompareLSPs} WriteLineToDebugFile(aCaption + ' EndComparison out'); {$EndIf}
end;


procedure CompareTRI(DEM : integer; OpenMap : boolean = true);
var
   DEMList : tDEMBooleanArray;
   Grid : integer;
   //Fixed : int64;
   //CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   StartComparisonProcess(DEM,DEMList);

   MDDef.SlopeCompute.AlgorithmName := smEvansYoung;
   Grid := CreateSlopeMapPercent(OpenMap,DEM,'md_slope');
   DEMlist[Grid] := true;

   if DEMGlb[DEM].DEMheader.DEMUsed = ArcSecDEM then begin
      Grid := MakeTRIGrid(DEM,nmEastWest,OpenMap,'md_tri_ew');
      DEMlist[Grid] := true;
      Grid := MakeTRIGrid(DEM,nmNorthSouth,OpenMap,'md_tri_ns');
      DEMlist[Grid] := true;
      Grid := MakeTRIGrid(DEM,nm30m,OpenMap,'md_tri_30m');
      DEMlist[Grid] := true;
   end
   else begin
      Grid := MakeTRIGrid(DEM,nmEastWest,OpenMap,'md_tri_spacing');
      DEMlist[Grid] := true;
   end;

   Grid := MakeTRIGrid(DEM,nmInterpolate,OpenMap,'md_tri_interpolate');
   DEMlist[Grid] := true;

   Grid := MakeTRIGrid(DEM,nmNone,OpenMap,'md_tri_none');
   DEMlist[Grid] := true;

   Grid := WBT_TRI(OpenMap,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'wbt_tri.tif');
   DEMlist[Grid] := true;

   Grid := GDAL_TRI_Riley(OpenMap,DEM,MDtempDir + 'gdal_tri_riley.tif');
   DEMlist[Grid] := true;
   Grid := GDAL_TRI_Wilson(OpenMap,DEM,MDtempDir + 'gdal_tri_wilson.tif');
   DEMlist[Grid] := true;

   Grid := SagaTRIMap(OpenMap,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'saga_tri.tif');
   DEMlist[Grid] := true;

   (*
   Grid := GRASSTRIMap(OpenMap,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'grass_tri.tif');
   DEMlist[Grid] := true;
   *)
   EndComparison(DEM,DEMList,'TRI');
end;


procedure CompareHillshadeMaps(DEM : integer; OpenMap : boolean = true);
begin
   WBT_HillshadeMap(OpenMap,DEM,MDtempDir + 'wbt_hillshade.tif');
   CreateHillshadeMap(OpenMap,DEM,MDtempDir + 'md_hillshade.tif');
   GDAL_HillshadeMap_Horn(OpenMap,DEM,MDtempDir + 'gdal_hillshade.tif');
end;


procedure MakeMDcurvatures(OpenMap : boolean; DEM,Curvature : integer; var DEMList : tDEMBooleanArray);

      procedure MICRODEMCurvature(Algorithm : integer; CD2 : boolean; Name : ShortString);
      var
         Grid : integer;
      begin
         MDDef.SlopeCompute.AlgorithmName := Algorithm;
         MDDef.CD2 := CD2;
         case Curvature of
            1 : Grid := CreateCurvatureMap(1,OpenMap,DEM,'md_' + Name + '_prof_curv');
            2 : Grid := CreateCurvatureMap(2,OpenMap,DEM,'md_' + Name + '_tang_curv');
            3 : Grid := CreateCurvatureMap(3,OpenMap,DEM,'md_' + Name + '_plan_curv');
            5 : Grid := CreateCurvatureMap(5,OpenMap,DEM,'md_' + Name + '_cont_tors');
         end;
         DEMlist[Grid] := true;
         DEMGlb[Grid].DEMHeader.VerticalCSTypeGeoKey := VertCSUndefined;
      end;

begin
   MICRODEMCurvature(smHorn,False,'horn_cd1');
   MICRODEMCurvature(smHorn,True,'horn_cd2');
   MICRODEMCurvature(smEvansYoung,False,'evans_cd1');
   MICRODEMCurvature(smEvansYoung,True,'evans_cd2');
   MICRODEMCurvature(smZevenbergenThorne,False,'zt_cd1');
   MICRODEMCurvature(smZevenbergenThorne,True,'zt_cd2');
   MICRODEMCurvature(smlsq,True,'lsq');
end;


procedure LoadLSPgrids(OpenMap,UTMGrid : boolean; Res : shortstring; var DEMlist : tDEMBooleanArray; ParamCode,ParamName : shortstring);
var
   //fName : PathStr;
   NewMap : integer;

   procedure LoadOne(fName : PathStr; sName : shortstring);
   var
      mt : tMapType;
      Invalid : integer;
   begin
      if FileExists(fName) then begin
         NewMap := OpenNewDEM(fName,false);
         if ValidDEM(NewMap) then begin
            mt := mtElevRainbow;
            DEMGlb[NewMap].DEMHeader.VerticalCSTypeGeoKey := VertCSUndefined;
            if (ParamCode = 'slope') then begin
               if (DEMGlb[NewMap].DEMHeader.ElevUnits <> euPercentSlope) then begin
                  PerformSingleGridArithmetic(NewMap,sgaSlopedegreestopercent1,Invalid,false);
               end;
               DEMGlb[NewMap].DEMHeader.ElevUnits := euPercentSlope;
               mt := MDDef.DefSlopeMap;
            end;
            if (ParamCode = 'aspect') and (DEMGlb[NewMap].DEMHeader.ElevUnits <> euAspectDeg) then begin
               DEMGlb[NewMap].DEMHeader.ElevUnits := euAspectDeg;
               mt := mtDEMAspect;
            end;
            if StrUtils.AnsiContainsText(ParamName,'curv') then begin
               DEMGlb[NewMap].DEMHeader.ElevUnits := euPerMeter;
               mt := mtCurvature;
            end;
            DEMlist[NewMap] := true;
            DEMGlb[NewMap].AreaName := SName  + ParamName;
            if OpenMap and (DEMGlb[NewMap].SelectionMap = Nil) then CreateDEMSelectionMap(NewMap,true,MDDef.DefElevsPercentile,mt);
         end;
      end
      else begin
         WriteLineToDebugFile('Missing: ' + fName);
      end;
   end;

begin
   if MDDef.UseCalculatedOutside then begin
      if ValidPath(lsp_calc_3d_dir) then begin
         LoadOne(lsp_calc_3d_dir + 'shifted_ponui_' + Res + '_3d_' + ParamCode + '.tif', 'lsp_calc_3d_');
      end
      else begin
         WriteLineToDebugFile('Invalid path: ' + lsp_calc_3d_dir);
      end;
      LoadOne(lsp_calc_4d_dir + 'shifted_ponui_' + Res + '_4d_' + ParamCode + '.tif','lsp_calc_4d_' );
   end;
end;


procedure LoadArcGrids(OpenMap,UTMGrid : boolean; var DEMlist : tDEMBooleanArray; ParamCode : shortstring);
var
   fName : PathStr;
   NewMap : integer;
   DataDir : PathStr;

   procedure LoadOne(Algorithm,sName : shortstring);
   var
      mt : tMapType;
      Invalid : integer;
   begin
      fName := DataDir + Algorithm + ParamCode + '.tif';
      if FileExists(fName) then begin
         NewMap := OpenNewDEM(fName,false);
         if ValidDEM(NewMap) then begin
            DEMGlb[NewMap].DEMHeader.VerticalCSTypeGeoKey := VertCSUndefined;
            DEMlist[NewMap] := true;
            DEMGlb[NewMap].AreaName := Algorithm  + ParamCode;
            if (ParamCode = 'slope') then begin
               mt := MDDef.DefSlopeMap;
               if (DEMGlb[NewMap].DEMHeader.ElevUnits <> euPercentSlope) then begin
                  PerformSingleGridArithmetic(NewMap,sgaSlopedegreestopercent1,Invalid,false);
               end;
               DEMGlb[NewMap].DEMHeader.ElevUnits := euPercentSlope;
            end
            else if (ParamCode = 'aspect') and (DEMGlb[NewMap].DEMHeader.ElevUnits <> euAspectDeg) then begin
               DEMGlb[NewMap].DEMHeader.ElevUnits := euAspectDeg;
               mt := mtDEMAspect;
            end
            else if StrUtils.AnsiContainsText(ParamCode,'curv') then begin
               DEMGlb[NewMap].DEMHeader.ElevUnits := euPerMeter;
               mt := mtCurvature;
            end
            else mt := mtElevRainbow;
            if OpenMap and (DEMGlb[NewMap].SelectionMap = Nil) then CreateDEMSelectionMap(NewMap,true,MDDef.DefElevsPercentile,mt);
         end;
      end
      else begin
         WriteLineToDebugFile('File missing: ' + fName);
      end;
   end;

begin
   if MDDef.UseCalculatedOutside then begin
      if UTMgrid then DataDir := arc_utm_dir else DataDir := arc_geo_dir;
      LoadOne('Arc_Old_',ParamCode);
      LoadOne('ArcSP_q_3x3_',ParamCode);
      LoadOne('ArcSP_bq_3x3_',ParamCode);
      if Use5x5 then LoadOne('ArcSP_q_5x5_',ParamCode);
      if Use5x5 then LoadOne('ArcSP_bq_5x5_',ParamCode);
   end;
end;


procedure CompareAspectMaps(DEM : integer; OpenMap : boolean = true);
var
   DEMList : tDEMBooleanArray;
   Fixed : int64;
   //fName : PathStr;
   Grid : integer;
  //CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   StartComparisonProcess(DEM,DEMList);
   SaveBackupDefaults;
   InitializeDEMsWanted(DEMList,false);
   DEMGLb[DEM].MarkBelowMissing(1.0,Fixed,false);

   Grid := MakeAspectMap(OpenMap,DEM);
   DEMGlb[Grid].AreaName := 'md_aspect.tif';
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   Grid := WBT_AspectMap(OpenMap,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'wbt_aspect.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      //for i := 1 to 4 do begin
         Grid := SAGA_Aspect(OpenMap,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'saga_'  + '_aspect.tif');
         if ValidDEM(Grid) then DEMlist[Grid] := true;
      //end;
   end;

   Grid := GrassAspectMap(DEMGlb[DEM].GeotiffDEMName, OpenMap,MDtempDir + 'grass_aspect.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   Grid := GDAL_AspectMap_Horn(true,DEM,MDtempDir + 'gdal_horn_aspect.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;
   Grid := GDAL_AspectMap_ZT(true,DEM,MDtempDir + 'gdal_zt_aspect.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      LoadLSPgrids(OpenMap,true,'30m',DEMlist,'aspect','aspect');
   end
   else begin
      LoadLSPgrids(OpenMap,false,'1sec',DEMlist,'aspect','aspect');
   end;
   ReCaptionMaps(DEMList);
   RestoreBackupDefaults;
end;


procedure CompareSlopeMaps(DEM : integer);
var
   NewMap : integer;
   DEMList : tDEMBooleanArray;
   //CorrelationMatrix : DEMStringGrid.TGridForm;
   OutPath: PathStr;
   // PathStr;

      procedure MatchAndSave(AreaName : shortstring; SaveIt : boolean = true);
      var
         fName : PathStr;
      begin
         if ValidDEM(NewMap) then begin
            DEMlist[NewMap] := true;
            DEMGlb[NewMap].AreaName := AreaName;
            MatchAnotherDEMMap(NewMap,DEM);
            if SaveIt then begin
               fName := OutPath + DEMGlb[NewMap].AreaName + '.tif';
               DEMGlb[NewMap].SaveAsGeotiff(fName);
            end;
         end;
      end;

begin
   if ValidDEM(DEM) then begin
      OutPath := MDtempDir;
      StartComparisonProcess(DEM,DEMList);

      MDDef.SlopeCompute.AlgorithmName := smEvansYoung;
      NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM);
      MatchAndSave('md_evans_slope');
      if (MDDef.GDAL_SAGA_arcsec) or (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
         NewMap := SAGA_Slope_percent(MDDef.CompareShowMaps,'3',DEMGlb[DEM].GeotiffDEMName,OutPath + 'saga_evans_slope.tif');
         MatchAndSave('saga_evans_slope');
      end;

      MDDef.SlopeCompute.AlgorithmName := smHorn;
      NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM);
      MatchAndSave('md_horn_slope');
      if (MDDef.GDAL_SAGA_arcsec) or (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
         NewMap := SAGA_Slope_percent(MDDef.CompareShowMaps,'2',DEMGlb[DEM].GeotiffDEMName,OutPath + 'saga_horn_slope.tif');
         MatchAndSave('saga_horn_slope');
         NewMap := GDAL_SlopeMap_Horn(MDDef.CompareShowMaps,DEM,'');
         MatchAndSave('gdal_horn_slope');
      end;

      {$IfDef ExUseGrass}
      {$Else}
         NewMap := GRASSSlopeMap(DEMGlb[DEM].GeotiffDEMName,MDDef.CompareShowMaps);
         if ValidDEM(NewMap) then MatchAndSave('grass_horn_slope');
      {$EndIf}

      MDDef.SlopeCompute.AlgorithmName := smZevenbergenThorne;
      NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM);
      MatchAndSave('md_zt_slope');
      if (MDDef.GDAL_SAGA_arcsec) or (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
         NewMap := SAGA_Slope_percent(MDDef.CompareShowMaps,'6',DEMGlb[DEM].GeotiffDEMName,OutPath + 'saga_zt_slope.tif');
         MatchAndSave('saga_zt_slope');
         NewMap := GDAL_SlopeMap_ZT(MDDef.CompareShowMaps,DEM,'');
         MatchAndSave('gdal_zt_slope');
      end;

      MDDef.SlopeCompute.AlgorithmName := smShary;
      NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM);
      MatchAndSave('md_shary_slope');

      if (MDDef.GDAL_SAGA_arcsec) or (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
         NewMap := SAGA_Slope_percent(MDDef.CompareShowMaps,'8',DEMGlb[DEM].GeotiffDEMName,OutPath + 'saga_zt_slope.tif');
         MatchAndSave('saga_flor_slope');
      end;
      NewMap := WBT_SlopeMap(MDDef.CompareShowMaps,DEMGlb[DEM].GeotiffDEMName);
      if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then MatchAndSave('wbt_flor_slope')
      else MatchAndSave('wbt_evans_slope');

      if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
         LoadLSPgrids(MDDef.CompareShowMaps,true,'30m',DEMlist,'slope','slope');
         LoadArcGrids(MDDef.CompareShowMaps,true,DEMlist,'slope');
      end
      else begin
         LoadLSPgrids(MDDef.CompareShowMaps,false,'1sec',DEMlist,'slope','slope');
         LoadArcGrids(MDDef.CompareShowMaps,false,DEMlist,'slope');
      end;

      if Use5x5 then begin
         MDDef.SlopeCompute.WindowRadius := 2;
         MDDef.SlopeCompute.AlgorithmName := smEvansYoung;
         NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM,'');
         MatchAndSave('md_evans_slope_5x5');
         MDDef.SlopeCompute.AlgorithmName := smHorn;
         NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM,'');
         MatchAndSave('md_horn_slope_5x5');
         MDDef.SlopeCompute.AlgorithmName := smZevenbergenThorne;
         NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM,'');
         MatchAndSave('md_zt_slope_5x5');
      end;

      EndComparison(DEM,DEMList,'slope');
   end;
end {procedure CompareSlopeMaps};



procedure CompareMICRODEMSlopeMaps(DEM : integer);
var
   NewMap : integer;
   DEMList : tDEMBooleanArray;
   //CorrelationMatrix : DEMStringGrid.TGridForm;
   OutPath : PathStr;
   //fName : PathStr;

      procedure MatchAndSave(AreaName : shortstring; SaveIt : boolean = true);
      var
         fName : PathStr;
      begin
         if ValidDEM(NewMap) then begin
            DEMlist[NewMap] := true;
            DEMGlb[NewMap].AreaName := AreaName;
            MatchAnotherDEMMap(NewMap,DEM);
            if SaveIt then begin
               fName := OutPath + DEMGlb[NewMap].AreaName + '.tif';
               DEMGlb[NewMap].SaveAsGeotiff(fName);
            end;
            {$IfDef RecordCompareLSPs} WriteLineToDebugFile('MatchAndSave done, ' + AreaName); {$EndIf}

         end;
      end;

begin
   if ValidDEM(DEM) then begin
      {$IfDef RecordCompareLSPs} WriteLineToDebugFile('CompareMICRODEMSlopeMaps in'); {$EndIf}
      OutPath := MDtempDir;
      StartComparisonProcess(DEM,DEMList);
      MDDef.SlopeCompute.AlgorithmName := smEvansYoung;
      NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM);
      MatchAndSave('md_evans_slope');
      MDDef.SlopeCompute.AlgorithmName := smHorn;
      NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM);
      MatchAndSave('md_horn_slope');
      MDDef.SlopeCompute.AlgorithmName := smLSQ;
      NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM);
      MatchAndSave('md_lsq_slope');
      {$IfDef ExUseGrass}
      {$Else}
         NewMap := GRASSSlopeMap(DEMGlb[DEM].GeotiffDEMName,MDDef.CompareShowMaps);
         if ValidDEM(NewMap) then MatchAndSave('grass_horn_slope');
      {$EndIf}
      MDDef.SlopeCompute.AlgorithmName := smZevenbergenThorne;
      NewMap := CreateSlopeMapPercent(MDDef.CompareShowMaps,DEM);
      MatchAndSave('md_zt_slope');
      NewMap := WBT_SlopeMap(MDDef.CompareShowMaps,DEMGlb[DEM].GeotiffDEMName);
      if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then MatchAndSave('wbt_flor_slope')
      else MatchAndSave('wbt_evans_slope');
      EndComparison(DEM,DEMList,'slope');
      {$IfDef RecordCompareLSPs} WriteLineToDebugFile('CompareMICRODEMSlopeMaps out'); {$EndIf}
   end;
end {procedure CompareSlopeMaps};


procedure CompareProfileCurvatures(DEM : integer);
var
   DEMList : tDEMBooleanArray;
   //Fixed,
   i : int64;
   //fName : PathStr;
   Grid : integer;
   //CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   {$IfDef RecordCompareLSPs} WriteLineToDebugFile('CompareProfileCurvatures in'); {$EndIf}

   StartComparisonProcess(DEM,DEMList);

   MakeMDcurvatures(MDDef.CompareShowMaps,DEM,1,DEMList);

   if (MDDef.GDAL_SAGA_arcsec) or (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      for i := 2 to 3 do begin
         Grid := SAGA_ProfileCurvature(MDDef.CompareShowMaps,DEMGlb[DEM].GeotiffDEMName,SAGA_algorithms[i],MDtempDir + 'saga_' + SAGA_aNames[i] + '_prof_curv.tif');
         if ValidDEM(Grid) then DEMlist[Grid] := true;
      end;
   end;

   Grid := GrassProfileCurvatureMap(DEMGlb[DEM].GeotiffDEMName, MDDef.CompareShowMaps,MDtempDir + 'grass_prof_curv.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   Grid := WBT_ProfileCurvature(MDDef.CompareShowMaps,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'wbt_prof_curv.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      LoadLSPgrids(MDDef.CompareShowMaps,true,'30m',DEMlist,'kns','prof_curv');
      LoadArcGrids(MDDef.CompareShowMaps,true,DEMlist,'prof_curv');
   end
   else begin
      LoadLSPgrids(MDDef.CompareShowMaps,false,'1sec',DEMlist,'kns','prof_curv');
      LoadArcGrids(MDDef.CompareShowMaps,false,DEMlist,'prof_curv');
   end;

   EndComparison(DEM,DEMList,'prof_curv');
   {$IfDef RecordCompareLSPs} WriteLineToDebugFile('CompareProfileCurvatures out'); {$EndIf}
end;


procedure ComparePlanCurvatures(DEM : integer);
var
   DEMList : tDEMBooleanArray;
   //Fixed,
   i : int64;
 //fName : PathStr;
   Grid : integer;
   //CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   StartComparisonProcess(DEM,DEMList);

   MakeMDcurvatures(MDDef.CompareShowMaps,DEM,3,DEMList);

   if (MDDef.GDAL_SAGA_arcsec) or (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      for i := 2 to 3 do begin  //skip Horn, Flor method, which fail
         Grid := SAGA_PlanCurvature(MDDef.CompareShowMaps,DEMGlb[DEM].GeotiffDEMName,SAGA_algorithms[i],MDtempDir + 'saga_' + SAGA_aNames[i] + '_plan_curv.tif');
         if ValidDEM(Grid) then DEMlist[Grid] := true;
      end;
   end;

   Grid := WBT_PlanCurvature(MDDef.CompareShowMaps,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'wbt_plan_curv.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      LoadLSPgrids(MDDef.CompareShowMaps,true,'30m',DEMlist,'kpc','plan_curv');
      LoadArcGrids(MDDef.CompareShowMaps,true,DEMlist,'plan_curv');
   end
   else begin
      LoadLSPgrids(MDDef.CompareShowMaps,false,'1sec',DEMlist,'kpc','plan_curv');
      LoadArcGrids(MDDef.CompareShowMaps,false,DEMlist,'plan_curv');
   end;
   EndComparison(DEM,DEMList,'plan_curv');
end;


procedure CompareTangentialCurvature(DEM : integer);
var
   DEMList : tDEMBooleanArray;
   //Fixed : int64;
   Grid,i : integer;
   //fName : PathStr;
   //CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   StartComparisonProcess(DEM,DEMList);

   MakeMDcurvatures(MDDef.CompareShowMaps,DEM,2,DEMList);

   Grid := GrassTangentialCurvatureMap(DEMGlb[DEM].GeotiffDEMName,MDDef.CompareShowMaps,MDtempDir + 'grass_tang_curv.tif');
   DEMlist[Grid] := true;
   if (MDDef.GDAL_SAGA_arcsec) or (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      for i := 2 to 3 do begin  //skip Horn, Flor method, which fail
         Grid := SAGA_TangentialCurvature(MDDef.CompareShowMaps,DEMGlb[DEM].GeotiffDEMName,SAGA_algorithms[i],MDtempDir + 'saga_' + SAGA_aNames[i] + '_tang_curv.tif');
         if ValidDEM(Grid) then DEMlist[Grid] := true;
      end;
   end;
   Grid := WBT_TangentialCurvature(MDDef.CompareShowMaps,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'wbt_tang_curv.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      LoadLSPgrids(MDDef.CompareShowMaps,true,'30m',DEMlist,'knc','tang_curv');
      LoadArcGrids(MDDef.CompareShowMaps,true,DEMlist,'tang_curv');
   end
   else begin
      LoadLSPgrids(MDDef.CompareShowMaps,false,'1sec',DEMlist,'knc','tang_curv');
      LoadArcGrids(MDDef.CompareShowMaps,false,DEMlist,'tang_curv');
   end;
   EndComparison(DEM,DEMList,'tang_curv');
end;


procedure CompareContourTorsion(DEM : integer);
var
   DEMList : tDEMBooleanArray;
   //Fixed : int64;
   //Grid,
   //i : integer;
   //fName : PathStr;
   //CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   StartComparisonProcess(DEM,DEMList);
   MakeMDcurvatures(MDDef.CompareShowMaps,DEM,5,DEMList);
   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      LoadLSPgrids(MDDef.CompareShowMaps,true,'30m',DEMlist,'tc','cont_tors');
   end
   else begin
      LoadLSPgrids(MDDef.CompareShowMaps,false,'1sec',DEMlist,'tc','cont_tors');
   end;
   EndComparison(DEM,DEMList,'cont_tors');
end;


procedure CompareSlopeAndThreeCurvature(DEM : integer);
begin
   CompareSlopeMaps(DEM);
   CompareProfileCurvatures(DEM);
   CompareTangentialCurvature(DEM);
   ComparePlanCurvatures(DEM);
   CombineAllPanelGraphs(2);
end;


procedure ComparePartialDerivatives(DEM : integer; OpenMap : boolean = true);
var
   DEMList : tDEMBooleanArray;
   i : integer;
   GrassPartials,MD_evanspartials,MD_ZTpartials,MD_HornPartials : tPartialGrids;
   CorrelationMatrix : DEMStringGrid.TGridForm;

   procedure Rename(var Partials : tPartialGrids; What : shortstring);
   var
      i : integer;
   begin
      for i := 1 to 5 do begin
         DEMglb[Partials[i]].AreaName := What + '_' + PartialNames[i];
         DEMglb[Partials[i]].SelectionMap.MapDraw.BaseTitle := DEMglb[Partials[i]].AreaName;
         DEMglb[Partials[i]].SelectionMap.DoFastMapRedraw;
      end;
   end;

begin
   SaveBackupDefaults;
   GRASS_partialDerivatives(DEM,GrassPartials);
   Rename(GrassPartials,'grass_horn');

   MDDef.SlopeCompute.AlgorithmName := smHorn;
   MICRODEM_partialDerivatives(DEM,MD_HornPartials);
   Rename(MD_hornPartials,'md_horn');

   MDDef.SlopeCompute.AlgorithmName := smEvansYoung;
   MICRODEM_partialDerivatives(DEM,MD_evansPartials);
   Rename(MD_evansPartials,'md_evans');

   MDDef.SlopeCompute.AlgorithmName := smZevenbergenThorne;
   MICRODEM_partialDerivatives(DEM,MD_ZTpartials);
   Rename(MD_ztPartials,'md_zt');
   ReCaptionMaps(DEMList);

   for i := 1 to 5 do begin
      InitializeDEMsWanted(DEMList,false);
      DEMList[GrassPartials[i]] := true;
      DEMList[MD_Hornpartials[i]] := true;
      DEMList[MD_evanspartials[i]] := true;
      DEMList[MD_ZTpartials[i]] := true;
      CorrelationMatrix := GridCorrelationMatrix(gcmR,DEMlist,DEMglb[DEM].AreaName + ' partial derivative correlation matrix');
   end;
   CombineAllPanelGraphs(3);
   RestoreBackupDefaults;
end;


end.
