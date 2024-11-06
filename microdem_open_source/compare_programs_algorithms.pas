unit compare_programs_algorithms;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


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
   SysUtils, Windows, Classes, Graphics, Controls,JPEG,DBCtrls,Math,dbClient,
   System.Threading,System.SyncObjs,System.Diagnostics,System.TimeSpan,
   StrUtils,
   db,
   System.Types,

   Nevadia_main,
   DEMDefs,
   Petmar_types,PETMAR,PETMath;


procedure CompareSlopeMaps(CurDEM : integer; OpenMap : boolean = true);
procedure CompareProfileCurvatures(DEM : integer; OpenMap : boolean = true);
procedure ComparePlanCurvatures(DEM : integer; OpenMap : boolean = true);
procedure CompareTangentialCurvature(DEM : integer; OpenMap : boolean = true);
procedure CompareThreeCurvature(DEM : integer; OpenMap : boolean = true);
procedure CompareHillshadeMaps(DEM : integer; OpenMap : boolean = true);
procedure ComparePartialDerivatives(DEM : integer; OpenMap : boolean = true);
procedure CompareTRI(DEM : integer; OpenMap : boolean = true);
procedure CompareAspectMaps(DEM : integer; OpenMap : boolean = true);

procedure CompareMICRODEMslopes(DEM : integer; OpenMap : boolean = true);
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



procedure CompareGDAL_ScaleFactorsForSlope(DEM : integer; OpenMap : boolean = true);
var
   NewMap : integer;
   DistanceNS,DistanceEW,DistanceAVG : float64;
begin
    GDAL_testScaleFactor := '';

   SaveBackupDefaults;
   MDDef.SlopeAlgorithm := smHorn;
   NewMap := CreateSlopeMapPercent(OpenMap,DEM,'md_horn_slope_2_space');

(*
   ExploreSingleGridSpacing := true;
   SingleSetSpacing := DEMGlb[DEM].AverageSpace;
   NewMap := CreateSlopeMapPercent(OpenMap,DEM,'md_horn_slope_average');
   SingleSetSpacing := DEMGlb[DEM].AverageXSpace;
   NewMap := CreateSlopeMapPercent(OpenMap,DEM,'md_horn_slope_x_space');
   SingleSetSpacing := DEMGlb[DEM].AverageYSpace;
   NewMap := CreateSlopeMapPercent(OpenMap,DEM,'md_horn_slope_y_space');
   ExploreSingleGridSpacing := false;
*)

   MetersPerDegree(DEMGlb[DEM].DEMSWcornerLat + 0.5 * DEMGlb[DEM].LatSizeMap,DEMGlb[DEM].DEMSWcornerLong + 0.5 * DEMGlb[DEM].LongSizeMap,DistanceNS,DistanceEW,DistanceAVG);

   NewMap := GDAL_SlopeMap_Horn(OpenMap,DEM,'gdal_horn_slope_average');
   GDAL_testScaleFactor := ' -s ' + RealToString(DistanceEW,-12,2);
   NewMap := GDAL_SlopeMap_Horn(OpenMap,DEM,'gdal_horn_slope_x_space');
   GDAL_testScaleFactor := ' -s ' + RealToString(DistanceNS,-12,2);
   NewMap := GDAL_SlopeMap_Horn(OpenMap,DEM,'gdal_horn_slope_y_space');

   RestoreBackupDefaults;
end;


procedure CompareMICRODEMslopes(DEM : integer; OpenMap : boolean = true);
var
   NewMap : integer;

   procedure OpenOneWindowSize(Which : shortstring; FilterSize : integer);
   begin
      MDDef.SlopeAlgorithm := smEvansYoung;
      NewMap := CreateSlopeMapPercent(OpenMap,DEM,'md_evans_slope_' + Which,FilterSize);
      MDDef.SlopeAlgorithm := smHorn;
      NewMap := CreateSlopeMapPercent(OpenMap,DEM,'md_horn_slope_' + Which,FilterSize);
      MDDef.SlopeAlgorithm := smZevenbergenThorne;
      NewMap := CreateSlopeMapPercent(OpenMap,DEM,'md_zt_slope_' + Which,FilterSize);
   end;

begin
   SaveBackupDefaults;
   OpenOneWindowSize('3x3',0);
   OpenOneWindowSize('5x5',2);
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
var
   i,x,y : integer;
begin
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) and DEMList[i] then begin
         for x := 0 to pred(DEMglb[DEM].DEMHeader.NumCol) do begin
            for y := 0 to pred(DEMglb[DEM].DEMHeader.NumRow) do begin
               if not DEMglb[DEM].FullAnalysisWindow(x,y,2) then begin
                  DEMglb[i].SetGridMissing(x,y);
               end;
            end;
         end;
         DEMglb[i].CheckMaxMinElev;
      end;
   end;
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
   List : tStringList;
   DEMsOrdered : tDEMIntegerArray;
   i,j,NumDEMs : integer;
begin
   {$IfDef RecordCompareLSPs} WriteLineToDebugFile(aCaption + ' EndComparison in'); {$EndIf}
   FilterToFullAnalysisWindow(DEM,DEMList);
   ReCaptionMaps(DEMList);
   RestoreBackupDefaults;

   if aCaption = 'slope' then begin
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

      CorrelationMatrix := GridCorrelationMatrix(gcmR,NumDEMs,DEMsOrdered,DEMglb[DEM].AreaName + ' ' + aCaption + ' correlation matrix');
      CorrelationMatrix := GridCorrelationMatrix(gcmMAbD,NumDEMs,DEMsOrdered,DEMglb[DEM].AreaName + ' ' + aCaption + ' mean absolute difference matrix');
      CorrelationMatrix := GridCorrelationMatrix(gcmMAvD,NumDEMs,DEMsOrdered,DEMglb[DEM].AreaName + ' ' + aCaption + ' mean average difference matrix');
   end
   else begin
      CorrelationMatrix := GridCorrelationMatrix(gcmR,DEMList,DEMglb[DEM].AreaName + ' ' + aCaption + ' correlation matrix');
      CorrelationMatrix := GridCorrelationMatrix(gcmMAbD,DEMList,DEMglb[DEM].AreaName + ' ' + aCaption + ' mean absolute difference matrix');
      CorrelationMatrix := GridCorrelationMatrix(gcmMAvD,DEMList,DEMglb[DEM].AreaName + ' ' + aCaption + ' mean average difference matrix');
   end;
   EndDEMIXProcessing;
   {$IfDef RecordCompareLSPs} WriteLineToDebugFile(aCaption + ' EndComparison in'); {$EndIf}
end;


procedure CompareTRI(DEM : integer; OpenMap : boolean = true);
var
   DEMList : tDEMBooleanArray;
   Grid : integer;
   Fixed : int64;
   CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   StartComparisonProcess(DEM,DEMList);

   MDDef.SlopeAlgorithm := smEvansYoung;
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
         MDDef.SlopeAlgorithm := Algorithm;
         MDDef.CD2 := CD2;
         case Curvature of
            1 : Grid := CreateProfileCurvature(OpenMap,DEM,1,'md_' + Name + '_prof_curv');
            2 : Grid := CreateTangentialCurvature(OpenMap,DEM,1,'md_' + Name + '_tang_curv');
            3 : Grid := CreatePlanCurvature(OpenMap,DEM,1,'md_' + Name + '_plan_curv');
         end;
         DEMlist[Grid] := true;
      end;

begin
   MICRODEMCurvature(smHorn,False,'horn_cd1');
   MICRODEMCurvature(smHorn,True,'horn_cd2');
   MICRODEMCurvature(smEvansYoung,False,'evans_cd1');
   MICRODEMCurvature(smEvansYoung,True,'evans_cd2');
   MICRODEMCurvature(smZevenbergenThorne,False,'zt_cd1');
   MICRODEMCurvature(smZevenbergenThorne,True,'zt_cd2');
end;


procedure LoadLSPgrids(OpenMap,UTMGrid : boolean; Res : shortstring; var DEMlist : tDEMBooleanArray; ParamCode,ParamName : shortstring);
var
   fName : PathStr;
   NewMap : integer;

   procedure LoadOne(fName : PathStr; sName : shortstring);
   var
      mt : tMapType;
   begin
      if FileExists(fName) then begin
         NewMap := OpenNewDEM(fName,false);
         if ValidDEM(NewMap) then begin
            mt := mtElevRainbow;
            if (ParamCode = 'slope') then begin
               if (DEMGlb[NewMap].DEMHeader.ElevUnits <> euPercentSlope) then begin
                  CreateDEMSelectionMap(NewMap,true,MDDef.DefElevsPercentile,mt);
                  DEMGlb[NewMap].SelectionMap.SingleGridArithmetic(sgaSlopedegreestopercent1,false);
               end;
               DEMGlb[NewMap].DEMHeader.ElevUnits := euPercentSlope;
               mt := MDDef.DefSlopeMap;
               //DEMGlb[NewMap].SaveAsGeotiff(DEMGlb[NewMap].DEMFileName);
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
            if OpenMap and (DEMGlb[NewMap].SelectionMap <> Nil) then CreateDEMSelectionMap(NewMap,true,MDDef.DefElevsPercentile,mt);
         end;
      end
      else begin
         WriteLineToDebugFile('Missing: ' + fName);
      end;
   end;

begin
   if ValidPath(lsp_calc_3d_dir) then begin
      LoadOne(lsp_calc_3d_dir + 'shifted_ponui_' + Res + '_3d_' + ParamCode + '.tif', 'lsp_calc_3d_');
   end
   else begin
      WriteLineToDebugFile('Invalid path: ' + lsp_calc_3d_dir);
   end;
   LoadOne(lsp_calc_4d_dir + 'shifted_ponui_' + Res + '_4d_' + ParamCode + '.tif','lsp_calc_4d_' );
end;


procedure LoadArcGrids(OpenMap,UTMGrid : boolean; var DEMlist : tDEMBooleanArray; ParamCode : shortstring);
var
   fName : PathStr;
   NewMap : integer;
   DataDir : PathStr;

   procedure LoadOne(Algorithm,sName : shortstring);
   var
      mt : tMapType;
   begin
      fName := DataDir + Algorithm + ParamCode + '.tif';
      if FileExists(fName) then begin
         NewMap := OpenNewDEM(fName,false);
         if ValidDEM(NewMap) then begin
            DEMlist[NewMap] := true;
            DEMGlb[NewMap].AreaName := Algorithm  + ParamCode;
            if (ParamCode = 'slope') then begin
               mt := MDDef.DefSlopeMap;
               if (DEMGlb[NewMap].DEMHeader.ElevUnits <> euPercentSlope) then begin
                  CreateDEMSelectionMap(NewMap,true,MDDef.DefElevsPercentile,mt);
                  DEMGlb[NewMap].SelectionMap.SingleGridArithmetic(sgaSlopedegreestopercent1,false);
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
            if OpenMap and (DEMGlb[NewMap].SelectionMap <> Nil) then CreateDEMSelectionMap(NewMap,true,MDDef.DefElevsPercentile,mt);
         end;
      end
      else begin
         WriteLineToDebugFile('File missng: ' + fName);
      end;
   end;

begin
   if UTMgrid then DataDir := arc_utm_dir else DataDir := arc_geo_dir;
   LoadOne('Arc_Old_',ParamCode);
   LoadOne('ArcSP_q_3x3_',ParamCode);
   LoadOne('ArcSP_bq_3x3_',ParamCode);
   if Use5x5 then LoadOne('ArcSP_q_5x5_',ParamCode);
   if Use5x5 then LoadOne('ArcSP_bq_5x5_',ParamCode);
end;


procedure CompareAspectMaps(DEM : integer; OpenMap : boolean = true);
var
   DEMList : tDEMBooleanArray;
   Fixed,i : int64;
   fName : PathStr;
   Grid : integer;
   CorrelationMatrix : DEMStringGrid.TGridForm;
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
         Grid := SAGA_Aspect(OpenMap,DEMGlb[DEM].GeotiffDEMName,{SAGA_algorithms[i],}MDtempDir + 'saga_' {+ SAGA_aNames[i]} + '_aspect.tif');
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


procedure CompareSlopeMaps(CurDEM : integer; OpenMap : boolean = true);
var
   NewMap : integer;
   DEMList : tDEMBooleanArray;
   CorrelationMatrix : DEMStringGrid.TGridForm;
   OutPath,
   fName : PathStr;

      procedure MatchAndSave(AreaName : shortstring; SaveIt : boolean = true);
      var
         fName : PathStr;
      begin
         if ValidDEM(NewMap) then begin
            DEMlist[NewMap] := true;
            DEMGlb[NewMap].AreaName := AreaName;
            MatchAnotherDEMMap(NewMap,CurDEM);
            if SaveIt then begin
               fName := OutPath + DEMGlb[NewMap].AreaName + '.tif';
               DEMGlb[NewMap].SaveAsGeotiff(fName);
            end;
         end;
      end;

begin
   if ValidDEM(CurDEM) then begin
      OutPath := MDtempDir;
      StartComparisonProcess(CurDEM,DEMList);

      MDDef.SlopeAlgorithm := smEvansYoung;
      NewMap := CreateSlopeMapPercent(OpenMap,CurDEM);
      MatchAndSave('md_evans_slope');
      NewMap := SAGA_Slope_percent(OpenMap,'3',DEMGlb[CurDEM].GeotiffDEMName,OutPath + 'saga_evans_slope.tif');
      MatchAndSave('saga_evans_slope');

      MDDef.SlopeAlgorithm := smHorn;
      NewMap := CreateSlopeMapPercent(OpenMap,CurDEM);
      MatchAndSave('md_horn_slope');
      NewMap := SAGA_Slope_percent(OpenMap,'2',DEMGlb[CurDEM].GeotiffDEMName,OutPath + 'saga_horn_slope.tif');
      MatchAndSave('saga_horn_slope');
      NewMap := GDAL_SlopeMap_Horn(true,CurDEM,'');
      MatchAndSave('gdal_horn_slope');

      {$IfDef ExUseGrass}
      {$Else}
         NewMap := GRASSSlopeMap(DEMGlb[CurDEM].GeotiffDEMName);
         if ValidDEM(NewMap) then MatchAndSave('grass_horn_slope');
      {$EndIf}

      MDDef.SlopeAlgorithm := smZevenbergenThorne;
      NewMap := CreateSlopeMapPercent(OpenMap,CurDEM);
      MatchAndSave('md_zt_slope');
      NewMap := SAGA_Slope_percent(OpenMap,'6',DEMGlb[CurDEM].GeotiffDEMName,OutPath + 'saga_zt_slope.tif');
      MatchAndSave('saga_zt_slope');
      NewMap := GDAL_SlopeMap_ZT(true,CurDEM,'');
      MatchAndSave('gdal_zt_slope');

      NewMap := SAGA_Slope_percent(OpenMap,'8',DEMGlb[CurDEM].GeotiffDEMName,OutPath + 'saga_zt_slope.tif');
      MatchAndSave('saga_flor_slope');
      NewMap := WBT_SlopeMap(OpenMap,DEMGlb[CurDEM].GeotiffDEMName);
      if (DEMGlb[CurDEM].DEMheader.DEMUsed = UTMbasedDEM) then MatchAndSave('wbt_flor_slope')
      else MatchAndSave('wbt_evans_slope');

      if (DEMGlb[CurDEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
         LoadLSPgrids(OpenMap,true,'30m',DEMlist,'slope','slope');
         LoadArcGrids(OpenMap,true,DEMlist,'slope');
      end
      else begin
         LoadLSPgrids(OpenMap,false,'1sec',DEMlist,'slope','slope');
         LoadArcGrids(OpenMap,false,DEMlist,'slope');
      end;

      if Use5x5 then begin
         MDDef.SlopeAlgorithm := smEvansYoung;
         NewMap := CreateSlopeMapPercent(OpenMap,CurDEM,'',2);
         MatchAndSave('md_evans_slope_5x5');
         MDDef.SlopeAlgorithm := smHorn;
         NewMap := CreateSlopeMapPercent(OpenMap,CurDEM,'',2);
         MatchAndSave('md_horn_slope_5x5');
         MDDef.SlopeAlgorithm := smZevenbergenThorne;
         NewMap := CreateSlopeMapPercent(OpenMap,CurDEM,'',2);
         MatchAndSave('md_zt_slope_5x5');
      end;

      EndComparison(CurDEM,DEMList,'slope');
   end;
end {procedure CompareSlopeMaps};


procedure CompareProfileCurvatures(DEM : integer; OpenMap : boolean = true);
var
   DEMList : tDEMBooleanArray;
   Fixed,i : int64;
   fName : PathStr;
   Grid : integer;
   CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   {$IfDef RecordCompareLSPs} WriteLineToDebugFile('CompareProfileCurvatures in'); {$EndIf}

   StartComparisonProcess(DEM,DEMList);

   MakeMDcurvatures(OpenMap,DEM,1,DEMList);

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      for i := 2 to 3 do begin
         Grid := SAGA_ProfileCurvature(OpenMap,DEMGlb[DEM].GeotiffDEMName,SAGA_algorithms[i],MDtempDir + 'saga_' + SAGA_aNames[i] + '_prof_curv.tif');
         if ValidDEM(Grid) then DEMlist[Grid] := true;
      end;
   end;

   Grid := GrassProfileCurvatureMap(DEMGlb[DEM].GeotiffDEMName, OpenMap,MDtempDir + 'grass_prof_curv.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   Grid := WBT_ProfileCurvature(OpenMap,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'wbt_prof_curv.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      LoadLSPgrids(OpenMap,true,'30m',DEMlist,'kns','prof_curv');
      LoadArcGrids(OpenMap,true,DEMlist,'prof_curv');
   end
   else begin
      LoadLSPgrids(OpenMap,false,'1sec',DEMlist,'kns','prof_curv');
      LoadArcGrids(OpenMap,false,DEMlist,'prof_curv');
   end;

   EndComparison(DEM,DEMList,'Profile curvature');
   {$IfDef RecordCompareLSPs} WriteLineToDebugFile('CompareProfileCurvatures out'); {$EndIf}
end;



procedure ComparePlanCurvatures(DEM : integer; OpenMap : boolean = true);
var
   DEMList : tDEMBooleanArray;
   Fixed,i : int64;
   fName : PathStr;
   Grid : integer;
   CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   StartComparisonProcess(DEM,DEMList);

   MakeMDcurvatures(OpenMap,DEM,3,DEMList);

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      for i := 2 to 3 do begin  //skip Horn, Flor method, which fail
         Grid := SAGA_PlanCurvature(OpenMap,DEMGlb[DEM].GeotiffDEMName,SAGA_algorithms[i],MDtempDir + 'saga_' + SAGA_aNames[i] + '_plan_curv.tif');
         if ValidDEM(Grid) then DEMlist[Grid] := true;
      end;
   end;
   //DEMlist[Grid] := true;
   Grid := WBT_PlanCurvature(OpenMap,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'wbt_plan_curv.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      LoadLSPgrids(OpenMap,true,'30m',DEMlist,'kpc','plan_curv');
      LoadArcGrids(OpenMap,true,DEMlist,'plan_curv');
   end
   else begin
      LoadLSPgrids(OpenMap,false,'1sec',DEMlist,'kpc','plan_curv');
      LoadArcGrids(OpenMap,false,DEMlist,'plan_curv');
   end;
   EndComparison(DEM,DEMList,'Plan curvature');
end;


procedure CompareTangentialCurvature(DEM : integer; OpenMap : boolean = true);
var
   DEMList : tDEMBooleanArray;
   Fixed : int64;
   Grid,i : integer;
   fName : PathStr;
   CorrelationMatrix : DEMStringGrid.TGridForm;
begin
   StartComparisonProcess(DEM,DEMList);

   MakeMDcurvatures(OpenMap,DEM,2,DEMList);

   Grid := GrassTangentialCurvatureMap(DEMGlb[DEM].GeotiffDEMName,OpenMap,MDtempDir + 'grass_tang_curve.tif');
   DEMlist[Grid] := true;
   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      for i := 2 to 3 do begin  //skip Horn, Flor method, which fail
         Grid := SAGA_TangentialCurvature(OpenMap,DEMGlb[DEM].GeotiffDEMName,SAGA_algorithms[i],MDtempDir + 'saga_' + SAGA_aNames[i] + '_tang_curv.tif');
         if ValidDEM(Grid) then DEMlist[Grid] := true;
      end;
   end;
   Grid := WBT_TangentialCurvature(OpenMap,DEMGlb[DEM].GeotiffDEMName,MDtempDir + 'wbt_tang_curve.tif');
   if ValidDEM(Grid) then DEMlist[Grid] := true;

   if (DEMGlb[DEM].DEMheader.DEMUsed = UTMbasedDEM) then begin
      LoadLSPgrids(OpenMap,true,'30m',DEMlist,'knc','tang_curv');
      LoadArcGrids(OpenMap,true,DEMlist,'tang_curv');
   end
   else begin
      LoadLSPgrids(OpenMap,false,'1sec',DEMlist,'knc','tang_curv');
      LoadArcGrids(OpenMap,false,DEMlist,'tang_curv');
   end;
   EndComparison(DEM,DEMList,'Tangential curvatur');
end;


procedure CompareThreeCurvature(DEM : integer; OpenMap : boolean = true);
begin
   CompareSlopeMaps(DEM,OpenMap);
   CompareProfileCurvatures(DEM,OpenMap);
   CompareTangentialCurvature(DEM,OpenMap);
   ComparePlanCurvatures(DEM,OpenMap);
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

   MDDef.SlopeAlgorithm := smHorn;
   MICRODEM_partialDerivatives(DEM,MD_HornPartials);
   Rename(MD_hornPartials,'md_horn');

   MDDef.SlopeAlgorithm := smEvansYoung;
   MICRODEM_partialDerivatives(DEM,MD_evansPartials);
   Rename(MD_evansPartials,'md_evans');

   MDDef.SlopeAlgorithm := smZevenbergenThorne;
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
