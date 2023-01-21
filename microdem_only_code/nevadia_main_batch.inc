{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}


{ include file for nevadia_main}


procedure Sentinel2BatchOps;
var
   Paths : tStringList;
   fName,sName : PathStr;
   i,Sat,NDVIgrid,NBRgrid,db : integer;
begin
   Paths := tStringList.Create;
   Paths.Add(LastSatDir);
   fName := 'H:\mid_projects\debuse\calwood.shp';
   if GetMultipleDirectories('Landsat or Sentinel-2 image',Paths) then begin
      for i := 0 to pred(Paths.Count) do begin
         LastSatDir := Paths[i];
         Sat := OpenSatImageFromDirectory(LastSatDir);
         db := SatImage[Sat].SelectionMap.LoadDataBaseFile(fName, false,true);
         NDVIgrid := SatImage[Sat].SelectionMap.NewSatWindow(nsbNDVI);
         MaskDEMFromShapeFile(NDVIgrid,db,true,true,0,MDDef.MaskDistance);
         sName := GetParentDirectory(LastSatDir) + 'ndvi\';
         SafeMakeDir(sName);
         sName := sName + SatImage[Sat].SceneBaseName + '_ndvi.dem';
         DEMGlb[NDVIgrid].SavePartOfDEMWithData(sName);

         NBRgrid := SatImage[Sat].SelectionMap.NewSatWindow(nsbNBRNormalizedBurnIndex);
         MaskDEMFromShapeFile(NBRgrid,db,true,true,0,MDDef.MaskDistance);
         sName := GetParentDirectory(LastSatDir) + 'nbr\';
         SafeMakeDir(sName);
         sName := sName + SatImage[Sat].SceneBaseName + '_nbr.dem';
         DEMGlb[NDVIgrid].SavePartOfDEMWithData(sName);

         CloseSingleSatelliteImage(Sat);
      end;
   end;
   Paths.Free;
end;



procedure MergeDEMIXCSV;
var
   fName : PathStr;
   i     : byte;
   DEM,db : integer;
   FilesWanted : tStringList;
   Tiles,ElevDiff,SlopeDiff,RuffDiff,TransposeNames : tStringList;


      function TransposeDEMIXcriteria(DBonTable : integer) : PathStr;
      const
         MaxDEMs = 10;
         MaxCriteria = 50;
      var
         Headers,DEMs,Criteria,Output,Tiles : tStringList;
         Line : ANSIString;
         fName,CriteriaFile : PathStr;
         value,ThisDEM,Missing : shortstring;
         Cycles,Done,
         i,j,  Tile, Ref, aLandType,DEM,Criterion : Integer;
         Values : array[0..MaxDEMs,0..MaxCriteria] of shortstring;
      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile(''); WriteLineToDebugFile('TransposeDEMIXcriteria in, ' + GISdb[DBonTable].dbName); {$EndIf}
         StopSplashing;
         Headers := tStringList.Create;
         Headers.LoadFromFile(DEMIXSettingsDir + 'demix_headers.txt');
         DEMs := tStringList.Create;
         DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');

         CriteriaFile := DEMIXSettingsDir + 'demix_criteria_with_signed.txt';
         Criteria := tStringList.Create;
         Criteria.LoadFromFile(CriteriaFile);

         for Criterion := pred(Criteria.Count) downto 0 do begin
            if not GISdb[DBonTable].MyData.FieldExists(Criteria.Strings[Criterion]) then begin
               Criteria.Delete(Criterion);
            end;
         end;

         Output := tStringList.Create;
         Line := '';
         for i := 0 to pred(Headers.Count) do Line := Line + Headers.Strings[i] + ',';
         Line := Line + 'TOLERANCE,DEM_LOW_SC,COP_ALOS,';
         for i := 0 to pred(DEMs.Count) do begin
            Line := Line + DEMs.Strings[i] + '_SCR,';
         end;
         Line := Line + 'REC_ID,CRITERION,';
         for i := 0 to pred(DEMs.Count) do begin
            Line := Line + DEMs.Strings[i];
            if i < pred(DEMs.Count) then Line := Line + ',';
         end;
         Output.Add(Line);

         GISdb[DBonTable].ClearGISFilter;
         Tiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria tiles=' + IntToStr(Tiles.Count)); {$EndIf}

         Cycles := Tiles.Count * 2 * MaxLandType;
         Done := 0;
         StartProgress('Transposing ' + GISdb[DBontable].DBName);
         for Tile := 0 to Pred(Tiles.Count) do begin
            UpdateProgressBar(Done/Cycles);
            for Ref := 1 to 2 do begin
               Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefDEMType[Ref]);
               GISdb[DBonTable].ApplyGISFilter(Line);
               GISdb[DBonTable].EmpSource.Enabled := false;
               if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin  //there are DSMs or DTMs for this tile
                  for aLandType := 1 to MaxLandType do begin
                     Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefDEMType[Ref])  + ' AND LAND_TYPE=' + QuotedStr(LandType[aLandType]);
                     GISdb[DBonTable].ApplyGISFilter(Line);
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     if (GISdb[DBonTable].MyData.FiltRecsInDB = DEMs.Count) then begin
                        for i := 0 to MaxDems do
                           for j := 0 to MaxCriteria do
                               Values[i,j] := '-9999';

                        while not GISdb[DBonTable].MyData.eof do begin
                           GISdb[DBonTable].EmpSource.Enabled := false;
                           ThisDEM := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));
                           DEM := DEMs.IndexOf(ThisDEM);
                           for Criterion := 0 to pred(Criteria.Count) do begin
                              value := GISdb[DBonTable].MyData.GetFieldByNameAsString(Criteria[Criterion]);
                              if (value <> '') then Values[DEM,Criterion] := value;
                           end;
                           GISdb[DBonTable].MyData.Next;
                        end;

                        for Criterion := 0 to pred(Criteria.Count) do begin
                           Line := '';
                           for i := 0 to pred(Headers.Count) do Line := Line + GISdb[DBonTable].MyData.GetFieldByNameAsString(Headers[i]) + ',';
                           Line := Line + '99.99,Not_yet_done,Not4,';
                           for DEM := 0 to pred(DEMs.Count) do Line := Line + '9.9,';
                           Line := Line + '123456,' + Criteria[Criterion] + ',';

                           for DEM := 0 to pred(DEMs.Count) do Line := Line + Values[DEM,Criterion] + ',';
                           Delete(Line,Length(Line),1);
                           Output.Add(Line);
                        end;
                     end
                     else begin
                        {$If Defined(RecordDEMIX)}  //this is to track down why some tiles do not have 6 records
                           if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin  //if it was 0, it was something like CLIFF in the flatlands
                              Missing := '  Present: ';
                              GISdb[DBonTable].MyData.First;
                              while not GISdb[DBonTable].MyData.eof do begin
                                 Missing := Missing + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM') + ', ';
                                 GISdb[DBonTable].MyData.Next;
                              end;
                              WriteLineToDebugFile('filter=' + GISdb[DBonTable].MyData.Filter + '   Matches=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + Missing);
                           end;
                        {$EndIf}
                     end;
                  end;
               end;
            end;
         end;
         EndProgress;
         GISdb[DBonTable].ClearGISFilter;
         Headers.Free;
         Criteria.Free;
         DEMs.Free;
         Result := ChangeFileExt(GISdb[DBonTable].dbFullName, '_transpose_' + ExtractFileNameNoExt(CriteriaFile) + '.dbf');
         DeleteFileIfExists(Result);
         Result := ChangeFileExt(Result,'.csv');
         OutPut.SaveToFile(Result);
         OutPut.Free;
         GISdb[DBonTable].ShowStatus;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria out, created ' + Result); {$EndIf}
      end;



   procedure MergeAndTranspose(var Diffs : tStringList; fName : PathStr);
   //var
      //DEM : integer;
      //DEMs : tStringList;
   begin
      if (Diffs.Count > 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeAndTranspose ' + fName + ' with regions=' + IntToStr(Diffs.Count)); {$EndIf}
         MergeCSVFiles(Diffs,fName);
         db := OpenDataBase('',fName);
         GISdb[db].AddSequentialIndex('REC_ID',false);
         fName := TransposeDEMIXcriteria(DB);
         TransposeNames.Add(fName);
         CloseAndNilNumberedDB(db);
      end
      else begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No MergeAndTranspose ' + fName + '  with regions=' + IntToStr(Diffs.Count)); {$EndIf}
         Diffs.Free;
      end;
   end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV in'); {$EndIf}
   FilesWanted := tStringList.Create;
   FilesWanted.Add(DEMIXresultsDir);
   i := 1;
   if not GetMultipleFiles('DEMIX CSV files to process','*.csv',FilesWanted,i) then exit;

   try
      HeavyDutyProcessing := true;
      Tiles := tStringList.Create;
      ElevDiff := tStringList.Create;
      SlopeDiff := tStringList.Create;
      RuffDiff := tStringList.Create;
      TransposeNames := tStringList.Create;
      for i := 0 to pred(FilesWanted.Count) do begin
        fName := uppercase(FilesWanted.Strings[i]);
        if StrUtils.AnsiContainsText(fname,'DEMIX_TILES_USED_') then Tiles.Add(fName);
        if StrUtils.AnsiContainsText(fname,'ELEV_DIFF_STATS_') then ElevDiff.Add(fName);
        if StrUtils.AnsiContainsText(fname,'SLOPE_DIFF_STATS_') then SlopeDiff.Add(fName);
        if StrUtils.AnsiContainsText(fname,'ROUGHNESS_DIFF_STATS_') then RuffDiff.Add(fName);
        if (ExtractFileExt(fName) = '.DBF') then DeleteFile(fName);  //so not have to confirm overwrite later
      end;
      if (Tiles.Count > 1) then MergeCSVFiles(Tiles,ExtractFilePath(fName) + 'DEMIX_TILES_USED_SUMMARY.csv')
      else Tiles.Free;

      MergeAndTranspose(ElevDiff,ExtractFilePath(fName) + 'elev_merge_difference_ref_dem.csv');
      MergeAndTranspose(SlopeDiff,ExtractFilePath(fName) + 'slope_merge_difference_ref_dem.csv');
      MergeAndTranspose(RuffDiff,ExtractFilePath(fName) + 'ruff_merge_difference_ref_dem.csv');

      fName := ExtractFilePath(fName) + 'all_merge_difference_ref_dem.csv';

      MergeCSVFiles(TransposeNames,fName);
      db := OpenDataBase('',fName);

      RankDEMS(DB);
      CloseAndNilNumberedDB(db);
   finally
      HeavyDutyProcessing := false;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV out'); {$EndIf}
end;



procedure ComputeDEMIXstats(FilesWanted : tStringList = Nil);
//a number of options are disabled, and might require changes to the code to get working again
const
   MaxRefStore = 100;
var
   DoHorizontalShift,
   ElevDiffHists : boolean;
   MomentVar,FlatMomentVar,SteepMomentVar,GentleMomentVar,CliffMomentVar,ForestMomentVar,UrbanMomentVar,BarrenMomentVar : tMomentVar;
   SlopeMomentVar,ElevMomentVar : array[1..MaxRefStore] of tMomentVar;
   NumPits,NumPeaks : array[1..MaxRefStore] of integer;
   DEMIXtileDB,

   LandCoverGrid : integer;
   bbgrid : tGridLimits;

   GeodeticFName,LandCoverFName,

   cfName,fName,IceSatFName,OutputDir : PathStr;
   TestAreaName,{WantSeries,}DEMIXtile,LandTypeMask,TileHeader : shortstring;
   LatCent,LongCent,
   GridFull,Lat,Long : float64;
   zRef{,z,z2} : float32;
   TileStats,
   ICESatStats,
   LagStats,
   SlopeMoments,
   ElevMoments,
   ElevDiffStats,
   RufDiffStats,
   SlopeDiffStats,
   PitsPeaks,
   CorrelationCoefficientsStats,
   ElevMomentDiffStats,SlopeMomentDiffStats,
   GeodeticStats : tStringList;
   zs,zsSteep,ZSFlat,zsGentle,zsCliff,zsForest,zsUrban,zsBarren : ^bfarray32;
   Rules,
   GeodeticTable,
   IceSatTable : tMyData;
   SlopeAsp,SlopeAspectTest : tSlopeAspectRec;
   ElevFiles,LegendFiles : tStringList;


         procedure InitializeStringLists;
         const
            AreaString = ',DEM,REF_DEM,REF_TYPE,GRID_FULL,LAND_TYPE,LANDTYP_PC';
            SlopeDiffStatsString = ',SLPD_MIN,SLPD_Max,SLPD_Mean,SLPD_AVD,SLPD_STD,SLPD_MED,SLPD_RMSE,SLPD_MAE,SLPD_N,SLPD_LE90';
            ElevDiffStatsString = ',ELVD_MIN,ELVD_Max,ELVD_Mean,ELVD_AVD,ELVD_STD,ELVD_MED,ELVD_RMSE,ELVD_MAE,ELVD_N,ELVD_LE90';
            RufDiffStatsString = ',RUFD_MIN,RUFD_Max,RUFD_Mean,RUFD_AVD,RUFD_STD,RUFD_MED,RUFD_RMSE,RUFD_MAE,RUFD_N,RUFD_LE90';

            IcePointsStatsString = 'DICE_MIN,DICE_Max,DICE_Mean,DICE_AVD,DICE_STD,DICE_MEDI,DICE_RMSE,DICE_MAE,DICE_N,DICE_AMED';
            BMPointsStatsString = 'DBM_MIN,DBM_Max,DBM_Mean,DBM_AVD,DBM_STD,DBM_MEDI,DBM_RMSE,DBM_MAE,DBM_N,DBM_AMED';

            ElevMomentsString = 'ELEV_MIN,ELEV_Max,ELEV_Mean,ELEV_ADEV,ELEV_STD,ELEV_SKEW,ELEV_KURT,ELEV_MED,N';
            SlopeMomentsString = 'SLP_MIN,SLP_Max,SLP_Mean,SLP_ADEV,SLP_STDV,SLP_SKEW,SLP_KURT,SLP_MED,SLP_N';

            SlopeMomentsDiffString = 'SDIFF_MIN,SDIFF_Max,SDIFF_Mean,SDIFF_STD,SDIFF_SKEW,SDIFF_KURT,SDIFF_MED,SDIFF_N';
            ElevMomentsDiffString = 'EDIFF_MIN,EDIFF_Max,EDIFF_Mean,EDIFF_STD,EDIFF_SKEW,EDIFF_KURT,EDIFF_MED,EDIFF_N';

            LagStatsString = 'Lag_Max,Lag_Mean,Lag_STD,LAG_MED,LAG_LE90,LAG_NPTS';
            TileStatsString = 'AREA,DEMIX_TILE,LAT,LONG,AVG_ELEV,AVG_SLOPE,AVG_ROUGH,RELIEF,FOREST_PC,URBAN_PC,BARREN_PC';
         begin
            TileStats := tStringList.Create;
            TileStats.Add(TileStatsString);

            ElevDiffStats := tStringList.Create;
            ElevDiffStats.Add(TileStatsString + AreaString + ElevDiffStatsString);

            RufDiffStats := tStringList.Create;
            RufDiffStats.Add(TileStatsString + AreaString + RufDiffStatsString);

            SlopeDiffStats := tStringList.Create;
            SlopeDiffStats.Add(TileStatsString + AreaString + SlopeDiffStatsString);

            (*
            LagStats := tStringList.Create;
            LagStats.Add(TileStatsString + AreaString + LagStatsString);

            ElevMomentDiffStats := tStringList.Create;
            ElevMomentDiffStats.Add(TileStatsString + AreaString + ElevMomentsDiffString);

            SlopeMomentDiffStats := tStringList.Create;
            SlopeMomentDiffStats.Add(TileStatsString + AreaString + SlopeMomentsDiffString);

            SlopeMoments := tStringList.Create;
            SlopeMoments.Add(MomentStr + ',' + SlopeMomentsString);

            ElevMoments := tStringList.Create;
            ElevMoments.Add(MomentStr + ',' + ElevMomentsString);

            PitsPeaks := tStringList.Create;
            PitsPeaks.Add(TileStatsString + AreaString + 'NUM_PITS,NUM_PEAKS,DIFF_PITS,DIFF_PEAKS');

            ICESatStats := tStringList.Create;
            ICESATStats.Add(TileStatsString + AreaString + IcePointsStatsString);

            GeodeticStats := tStringList.Create;
            GeodeticStats.Add(TileStatsString + AreaString + BMPointsStatsString);

            CorrelationCoefficientsStats := tStringList.Create;
            CorrelationCoefficientsStats.Add(TileStatsString + AreaString + 'R2,INTERCEPT,SLOPE');
            *)

            {$IfDef RecordDEMIXFull} writeLineToDebugFile('InitializeStringLists out, string lists created'); {$EndIf}
         end;


         function LineHeader(DEM,RefDEM : integer; RefType : shortstring) : shortstring;
         var
            t1,t2{,t3} : shortstring;
         begin
             if (DEM = 0) then t1 := 'xxx' else t1 := DEMGlb[DEM].AreaName;
             if (RefDEM = 0) then begin
                t2 := 'xxx';
                //t3 := 'xxx';
             end
             else begin
                t2 := DEMGlb[RefDEM].AreaName;
                //if (RefDEM = RefDSMpoint) or (RefDEM = RefDSMarea) then t3 := 'DSM'
                //else t3 := 'DTM';
             end;
             Result := t1 + ',' + t2 + ',' + RefType + ',' + RealToString(GridFull,-8,2) + ',' + LandTypeMask + ',';
         end;



procedure SortListWithHeader(var sl : tStringList);
var
   Header : ANSIString;
begin
   Header := sl.Strings[0];
   sl.Delete(0);
   sl.Duplicates := dupIgnore;
   sl.Sort;
   sl.Insert(0,Header);
end;

         procedure FinalizeStringLists(FinalSaveResults : boolean = true);

               procedure ProcessStringList(fName : PathStr; var sl : tStringList; DoStats : boolean = false);
               //var
                  //db : integer;
               begin
                  {$IfDef RecordFullDEMIX} writeLineToDebugFile(fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                  if (sl.count > 1) then begin
                     fName := OutputDir + fname + '_' + TestAreaName + '.csv';
                     sl.SaveToFile(fName);
                  end
                  else fName := '';
                  sl.Destroy;
               end;

         begin
            {$IfDef RecordFullDEMIX} writeLineToDebugFile('DEMIX start string list processing'); {$EndIf}
            ProcessStringList('DEMIX_tiles_used',TileStats);

            ProcessStringList('Elev_diff_stats',ElevDiffStats,true);
            ProcessStringList('Roughness_diff_stats',RufDiffStats,true);
            ProcessStringList('Slope_diff_stats',SlopeDiffStats,true);

            (*
            ProcessStringList('ICESat_best_diff_stats',ICESATStats);
            ProcessStringList('Geodetic_control_stats',GeodeticStats);

            SortListWithHeader(ElevMoments);
            ProcessStringList('Elevation_moments',ElevMoments);
            SortListWithHeader(SlopeMoments);
            ProcessStringList('Slope_moments',SlopeMoments);

            ProcessStringList('Elevation_moments_diff',ElevMomentDiffStats);
            ProcessStringList('Slope_moments_diff',SlopeMomentDiffStats);
            ProcessStringList('Pits_and_peaks',PitsPeaks);
            ProcessStringList('Horizonatal_lags',LagStats);
            ProcessStringList('Full_tile_correlation',CorrelationCoefficientsStats);
            *)
         end;



   procedure ProcessDEMIXtestarea;



         function GridBoundingBox(DEM,RefDEM : integer; Clip : boolean = false) : tGridLimits;
         var
            bb : sfBoundBox;
         begin
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;

            if Clip and (RefDEM <> 0) then begin
               if bb.xMin < DEMGlb[RefDEM].DEMBoundBoxGeo.xmin then bb.xMin := DEMGlb[RefDEM].DEMBoundBoxGeo.xmin;
               if bb.xMax > DEMGlb[RefDEM].DEMBoundBoxGeo.xmax then bb.xMax := DEMGlb[RefDEM].DEMBoundBoxGeo.xmax;
               if bb.yMin < DEMGlb[RefDEM].DEMBoundBoxGeo.ymin then bb.yMin := DEMGlb[RefDEM].DEMBoundBoxGeo.ymin;
               if bb.yMax > DEMGlb[RefDEM].DEMBoundBoxGeo.ymax then bb.yMax := DEMGlb[RefDEM].DEMBoundBoxGeo.ymax;
            end;

            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,Result.xgridlow,Result.ygridlow);
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,Result.xgridhigh,Result.ygridhigh);
         end;

         procedure WriteTileDetails(bb: tGridLimits);
         var
            cols,rows : integer;
         begin
             cols := succ(bbgrid.xgridhigh-bbgrid.xgridLow);
             rows := succ(bbgrid.ygridhigh-bbgrid.ygridLow);
             WriteLineToDebugFile(DEMIXTile + RealToString(GridFull,8,2) + '%  ' + GridLimitstoString(bbgrid) + ' cols=' + IntToStr(cols) +  ' rows=' + IntToStr(rows) + ' points=' + IntToStr(cols*rows));
         end;

         function UseThisTile : boolean;
         var
            bb : sfBoundBox;
         begin
            GridFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
            Result := GridFull >= MDDef.DEMIX_Full;
            DEMIXtile := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            LatCent := 0.5 * (bb.ymax + bb.ymin);
            LongCent := 0.5 * (bb.xmax + bb.xmin);
         end;

         function MomentStatsString(MomentVar : tMomentVar) : shortstring;
         begin
            Result := RealToString(MomentVar.MinZ,-8,2) + ',' + RealToString(MomentVar.MaxZ,-8,2) + ',' + RealToString(MomentVar.Mean,-8,2) + ',' +
                RealToString(MomentVar.avg_dev,-8,2) + ',' + RealToString(MomentVar.sdev,-8,2) + ',' + RealToString(MomentVar.median,-8,2) + ',' + RealToString(MomentVar.rmse,-8,2)  + ',' +
                RealToString(MomentVar.mae,-8,2) + ',' + IntToStr(MomentVar.NPts);
         end;


         function MomentDifferenceString(aMomentVar,RefMomentVar : tMomentVar) : shortstring;
         begin
             Result := RealToString(aMomentVar.MinZ-RefMomentVar.minZ,-8,2) + ',' + RealToString(aMomentVar.MaxZ-RefMomentVar.MaxZ,-8,2) + ',' +
                            RealToString(aMomentVar.Mean-RefMomentVar.mean,-8,2) + ',' + RealToString(aMomentVar.sdev-RefMomentVar.sdev,-8,2) + ',' +
                            RealToString(aMomentVar.skew-RefMomentVar.skew,-8,2) + ',' + RealToString(aMomentVar.curt-RefMomentVar.Curt,-8,2) + ',' +
                            RealToString(aMomentVar.median-RefMomentVar.median,-8,2) + ',' + IntToStr(aMomentVar.NPts);
         end;


(*
         procedure DoICESat(DEM : integer);
         var
            Lat,Long,MedianAbs : float64;
            i : integer;
            z,z2 : float32;
            MomentVar : tMomentVar;
         begin
            if (IceSatFName = '') then exit;
            {$IfDef RecordDEMIX} writeLineToDebugFile('ICESat-2 ' + DEMGLB[DEM].AreaName); {$EndIf}
            GridFull := 9999;
            IceSatTable.First;
            MomentVar.Npts := 0;
            while not ICESatTable.eof do begin
               Lat := IceSatTable.GetFieldByNameAsFloat('LAT');
               Long := IceSatTable.GetFieldByNameAsFloat('LONG');
               Z := IceSatTable.GetFieldByNameAsFloat('ICESAT_GRD');
               if DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                  zs^[MomentVar.NPts] := z2-z;
                  inc(MomentVar.Npts);
               end;
               ICESatTable.Next;
            end;
            moment(zs^,MomentVar,msAll);
            for i := 0 to pred(MomentVar.NPts) do zs^[i] := abs(zs^[i]);
            MedianAbs := Median(zs^,MomentVar.NPts,false);
            ICESatStats.Add(LineHeader(DEM,0) + MomentStatsString(MomentVar) + ',' + RealToString(MedianAbs,-8,2));
         end;


         procedure DoGeodetic(DEM : integer);
         var
            Lat,Long,MedianAbs : float64;
            i : integer;
            z,z2 : float32;
            MomentVar : tMomentVar;
         begin
            if (GeodeticFName = '') then exit;
            {$IfDef RecordDEMIX} writeLineToDebugFile('geodetic ' + DEMGLB[DEM].AreaName); {$EndIf}
            GridFull := 9999;
            GeodeticTable.First;
            MomentVar.Npts := 0;
            while not GeodeticTable.eof do begin
               Lat := GeodeticTable.GetFieldByNameAsFloat('LAT');
               Long := GeodeticTable.GetFieldByNameAsFloat('LONG');
               Z := GeodeticTable.GetFieldByNameAsFloat('Z_EGM2008');
               if DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                  zs^[MomentVar.NPts] := z2-z;
                  inc(MomentVar.Npts);
               end;
               GeodeticTable.Next;
            end;
            if MomentVar.NPts > 0 then begin
               moment(zs^,MomentVar,msAll);
               for i := 0 to pred(MomentVar.NPts) do zs^[i] := abs(zs^[i]);
               MedianAbs := Median(zs^,MomentVar.NPts,false);
               GeodeticStats.Add(LineHeader(DEM,0) + MomentStatsString(MomentVar) + ',' + RealToString(MedianAbs,-8,2));
            end;
         end;
*)


            procedure ZeroMomentVar;
            begin
               MomentVar.Npts := 0;
               FlatMomentVar.Npts := 0;
               GentleMomentVar.Npts := 0;
               SteepMomentVar.Npts := 0;
               CliffMomentVar.Npts := 0;

               UrbanMomentVar.Npts := 0;
               ForestMomentVar.Npts := 0;
               BarrenMomentVar.Npts := 0;
            end;


            procedure LandCover(Lat,Long : float32; Difference : float32);
            //this is hard coded for a particular land cover data set
            var
               z : float32;
            begin
               if (LandCoverGrid <> 0) and DEMGlb[LandCoverGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
                  if round(z) in [111..126] then begin
                     zsForest^[ForestMomentVar.NPts] := Difference;
                     inc(ForestMomentVar.Npts);
                  end
                  else if round(z) in [30,60,70] then begin
                     zsBarren^[BarrenMomentVar.NPts] := Difference;
                     inc(BarrenMomentVar.Npts);
                  end
                  else if round(z) in [50] then begin
                     zsUrban^[UrbanMomentVar.NPts] := Difference;
                     inc(UrbanMomentVar.Npts);
                  end;
               end;
            end;


            procedure LandTypeDiff(Difference : float32);
            begin
               zs^[MomentVar.NPts] := Difference;
               inc(MomentVar.Npts);
               if SlopeAsp.SlopePercent < MDDef.SlopeFlatBoundary then begin
                  zsflat^[FlatMomentVar.NPts] := Difference;
                  inc(FlatMomentVar.Npts);
               end
               else if SlopeAsp.SlopePercent < MDDef.SlopeGentleBoundary then begin
                  zsGentle^[GentleMomentVar.NPts] := Difference;
                  inc(GentleMomentVar.Npts);
               end
               else if SlopeAsp.SlopePercent < MDDef.SlopeSteepBoundary then begin
                  zsSteep^[SteepMomentVar.NPts] := Difference;
                  inc(SteepMomentVar.Npts);
               end
               else begin
                  zsCliff^[CliffMomentVar.NPts] := Difference;
                  inc(CliffMomentVar.Npts);
               end;
            end;



           function WriteDifferenceResults(DEM,REFDEM : integer; RefType : shortstring; var WhichStats : tStringList) : integer;

                     procedure WriteDifferenceResult(var sl : tStringList; DEM,RefDEM : integer; var thezs : bfarray32; var theMoments : tMomentVar; Percent : float32);
                     var
                        i : integer;
                        aLine : shortstring;
                     begin
                        if (theMoments.NPts > MDDef.LandTypePointsNeeded) then begin
                           moment(thezs,theMoments,msAll);
                           for i := 0 to pred(theMoments.NPts) do thezs[i] := abs(thezs[i]);
                           theMoments.LE90 := Percentile(90,thezs,theMoments.NPts,false);
                           aline := TileHeader + LineHeader(DEM,RefDEM,RefType)  + RealToString(Percent,-12,-2) + ',' + MomentStatsString(theMoments) + ',' + RealToString(theMoments.LE90,-12,-2);
                           sl.Add(aLine);
                           inc(Result);
                        end
                        else begin
                           {$If Defined(RecordDEMIXGridCompare)} WriteLineToDebugFile('WriteDifferenceResult failed for ' + LandTypeMask + ' theMoments.NPts=' + IntToStr(theMoments.NPts) ); {$EndIf}
                        end;
                     end;

           begin
               Result := 0;
               LandTypeMask := 'ALL';
               WriteDifferenceResult(WhichStats,DEM,RefDEM,zs^,MomentVar,100);
               LandTypeMask := 'FLAT';
               WriteDifferenceResult(WhichStats,DEM,RefDEM,zsFlat^,FlatMomentVar,(100 * FlatMomentVar.NPts/MomentVar.NPts));
               LandTypeMask := 'GENTLE';
               WriteDifferenceResult(WhichStats,DEM,RefDEM,zsGentle^,GentleMomentVar,(100 * GentleMomentVar.NPts/MomentVar.NPts));
               LandTypeMask := 'STEEP';
               WriteDifferenceResult(WhichStats,DEM,RefDEM,zsSteep^,SteepMomentVar,(100 * SteepMomentVar.NPts/MomentVar.NPts));
               LandTypeMask := 'CLIFF';
               WriteDifferenceResult(WhichStats,DEM,RefDEM,zsCliff^,CliffMomentVar,(100 * CliffMomentVar.NPts/MomentVar.NPts));

               if (LandCoverGrid <> 0) then begin
                  LandTypeMask := 'FOREST';
                  WriteDifferenceResult(WhichStats,DEM,RefDEM,zsForest^,ForestMomentVar,(100 *ForestMomentVar.NPts/MomentVar.NPts));
                  LandTypeMask := 'URBAN';
                  WriteDifferenceResult(WhichStats,DEM,RefDEM,zsUrban^,UrbanMomentVar,(100 * UrbanMomentVar.NPts/MomentVar.NPts));
                  LandTypeMask := 'BARREN';
                  WriteDifferenceResult(WhichStats,DEM,RefDEM,zsBarren^,BarrenMomentVar,(100 * BarrenMomentVar.NPts/MomentVar.NPts));
               end;
               LandTypeMask := 'ALL';
           end;


           procedure CompareDifferencesToReferenceDEM(DEM,RefDEM : integer; RefType : shortstring);
           //do full DEM pixel by pixel comparison to reference DEM
           var
              Ruff1,Ruff2,z,Difference : float32;
              NumRuff,NumSlope,NumElev,
              xg,yg,Col,Row : integer;
            begin
               if (RefDEM = 0) then begin
                  {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXGridCompare)} writeLineToDebugFile('Fail (RefDEM=0), comparison to reference, ' + DEMGLB[DEM].AreaName); {$EndIf}
                  exit;
               end;
               {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXGridCompare)} writeLineToDebugFile('Comparison to reference, ' + DEMGLB[DEM].AreaName); {$EndIf}
               bbgrid := GridBoundingBox(DEM,RefDEM,true);

               NumElev := 0;
               NumSlope := 0;
               NumRuff := 0;

               ZeroMomentVar;
               {$IfDef RecordFullDEMIX} WriteTileDetails(bbgrid); {$EndIf}
               for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                  for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                     if DEMGlb[DEM].GetElevMetersOnGrid(col,row,z) then begin
                        DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                        DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                        if DEMGlb[RefDEM].GetElevMetersOnGrid(xg,yg,zref) then begin
                           Difference := z-zref;
                           if DEMGlb[RefDEM].GetSlopeAndAspect(xg,yg,SlopeAsp) then begin
                              LandTypeDiff(Difference);
                           end;
                           LandCover(Lat,long,Difference);
                        end;
                     end;
                  end;
               end;

               if (MomentVar.NPts > 0) then begin
                  if ElevDiffHists then begin
                     fName := OutputDir + 'elev_diff_hists\' + DEMGLB[DEM].AreaName  + '_to_' + RefType + '-' + DEMIXtile + '.z';
                     ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zs^,fName));
                     LegendFiles.Add(ExtractFileNameNoExt(fName));
                  end;
                  NumElev := WriteDifferenceResults(DEM,REFDEM,RefType,ElevDiffStats);
               end;

               ZeroMomentVar;
               for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                  for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                     if DEMGlb[DEM].GetSlopeAndAspect(col,row,SlopeAspectTest) then begin
                        DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                        if DEMGlb[RefDEM].GetSlopeAndAspectFromLatLong(Lat,Long,SlopeAsp) then begin
                           Difference := SlopeAspectTest.SlopePercent - SlopeAsp.SlopePercent;
                           LandTypeDiff(Difference);
                           LandCover(Lat,Long,Difference);
                        end;
                     end;
                  end;
               end;
               if (MomentVar.NPts > 1) then begin
                  if ElevDiffHists then begin
                     fName := OutputDir + 'slope_diff_hists\' + DEMGLB[DEM].AreaName  + '_to_' + RefType + '-' + DEMIXtile + '.z';
                     SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                  end;
                  NumSlope := WriteDifferenceResults(DEM,REFDEM,RefType,SlopeDiffStats);
               end;

               ZeroMomentVar;
               for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                  for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                     if DEMGlb[DEM].RoughnessFromSlopeSTD(Col,Row,MDDef.RoughnessBox,Ruff1) then begin
                        DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                        DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                        if DEMGlb[RefDEM].RoughnessFromSlopeSTD(xg,yg,MDDef.RoughnessBox,Ruff2) then begin
                           Difference := Ruff1 - Ruff2;
                           if DEMGlb[RefDEM].GetSlopeAndAspect(xg,yg,SlopeAsp) then begin
                              LandTypeDiff(Difference);
                           end;
                           LandCover(Lat,long,Difference);
                        end;
                     end;
                  end;
               end;
               if (MomentVar.NPts > 1) then begin
                  if ElevDiffHists then begin
                     fName := OutputDir + 'ruff_diff_hists\' + DEMGLB[DEM].AreaName  + '_to_' + RefType + '-' + DEMIXtile + '.z';
                     SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                  end;
                  NumRuff := WriteDifferenceResults(DEM,REFDEM,RefType,RufDiffStats);
               end;

               {$IfDef RecordDEMIX}  WriteLineToDebugFile(TestAreaName + '   ' + DEMIXTile + '  ' + RefType + '  elev=' + IntToStr(NumElev) +  '  slope=' + IntToStr(NumSlope) +  '  ruff=' + IntToStr(NumRuff)  +
                     '  total=' + IntToStr(NumElev+NumSlope+NumRuff)); {$EndIf}
            end;


         procedure GetTileStatistics(RefDEM : integer);
            //'DEMIX_TILE,LAT,LONG,AVG_ELEV,AVG_SLOPE,RELIEF,FOREST_PC,URBAN_PC');
         var
           ElevMomentVar,SlopeMomentVar,RoughMomentVar : tMomentVar;
           Col,Row,NPts : integer;
           ForestPC,UrbanPC,BarrenPC : float32;
         begin
            if UseThisTile then begin
               bbgrid := GridBoundingBox(RefDEM,0);
               ZeroMomentVar;
               ElevMomentVar.Npts := 0;
               SlopeMomentVar.Npts := 0;
               RoughMomentVar.Npts := 0;
               NPts := 0;
               DEMGlb[RefDEM].ElevationMomentsWithArray(bbgrid,ElevMomentVar,zs^);
               DEMGlb[RefDEM].SlopeMomentsWithArray(bbgrid,SlopeMomentVar,zs^);

               DEMGlb[RefDEM].GetRoughnessInLongArray(bbgrid,RoughMomentVar.NPts,zs^);
               Moment(zs^,RoughMomentVar,msAll);

               if (LandCoverGrid <> 0) then begin
                  for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                     for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                        DEMGlb[RefDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                        LandCover(Lat,long,Difference);
                        inc(NPts);
                     end;
                  end;
                  ForestPC := 100 * ForestMomentVar.Npts / NPts;
                  UrbanPC := 100 * UrbanMomentVar.Npts / NPts;
                  BarrenPC := 100 * BarrenMomentVar.Npts / NPts;
               end
               else begin
                  ForestPC := -9999;
                  UrbanPC := -9999;
                  BarrenPC := -9999;
               end;

               TileHeader := TestAreaName + ',' + DEMIXTile + ',' + RealToString(LatCent,-12,-6) + ',' + RealToString(LongCent,-12,-6) + ',' + RealToString(ElevMomentVar.Mean,-12,-2)   + ','  +
                  RealToString(SlopeMomentVar.Mean,-12,-2)  + ',' + RealToString(RoughMomentVar.Mean,-12,-2)  + ',' +
                  RealToString(ElevMomentVar.Maxz - ElevMomentVar.Minz,-12,-2) + ',' + RealToString(ForestPC,-12,-2)  + ',' + RealToString(UrbanPC,-12,-2) + ',' + RealToString(BarrenPC,-12,-2);
               TileStats.Add(TileHeader);
               TileHeader := TileHeader + ',';
            end;
         end;


         procedure DoReferenceCriteria;

               procedure ReferenceDEMcriteria(DEM : integer);
               begin
                  if (DEM <> 0) then begin
                     (*
                     //DEMIXTile := '';
                     DoSlopeMoments(DEM,0);
                     DoElevMoments(DEM,0);
                     DoICESat(DEM);
                     DoGeodetic(DEM);
                     DoPeakPits(DEM,0);
                     *)
                  end;
               end;

         begin
            //do critera for reference DEMs
            ReferenceDEMcriteria(RefDTMpoint);
            ReferenceDEMcriteria(RefDTMarea);
            ReferenceDEMcriteria(RefDSMpoint);
            ReferenceDEMcriteria(RefDSMarea);
            ReferenceDEMcriteria(COPRefDTM);
         end;

         procedure CompareToReference(DEM,RefDEM : integer; RefType : shortstring);
         begin
            if (RefDEM = 0) then begin
               {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXGridCompare)}
                  WriteLineToDebugFile('Failure, missing RefDEM: ' + TestAreaName + '  ' + DEMIXTile  + '  ' + RefType + ' DEM=' + DEMGlb[DEM].AreaName);
               {$EndIf}
            end
            else begin
               {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXGridCompare)}
                  WriteLineToDebugFile(TestAreaName + '  ' + DEMIXTile  + '  ' + RefType + '  CompareToReference, DEM=' + DEMGlb[DEM].AreaName + '   ref=' + DEMGlb[RefDEM].AreaName);
               {$EndIf}

               CompareDifferencesToReferenceDEM(DEM,RefDEM,RefType);

               (*
               //GetTileStatistics(RefDEM);
               if DoHorizontalShift then DoHorizontalShift(DEM,RefDEM);
               DoSlopeMoments(DEM,RefDEM);
               DoElevMoments(DEM,RefDEM);
               ComputeCorrelationCoefficient(DEM,RefDEM);
               DoPeakPits(DEM,RefDEM);
               *)
            end;
         end;


   var
     // WantDEM,j,
      Ser,i,UseDSM,UseDTM : integer;
      //AllDEMs,
      TStr : shortstring;
      NoneMissing : boolean;
   begin
      {$If Defined(RecordDEMIXFull)} HighlightLineToDebugFile('ProcessDEMIXtestarea in ' + TestAreaName); {$EndIf}

      ReportErrors := false;

      DEMIXtileDB := 0;
      AddLocalVDatum := 0;
      SubLocalVDatum := 0;
      LandCoverGrid := 0;

      for I := 1 to MaxRefStore do begin
        InitializeMomentVar(SlopeMomentVar[i]);
        InitializeMomentVar(ElevMomentVar[i]);
        NumPits[i] := 0;
        NumPeaks[i] := 0;
      end;

      if LoadDEMIXReferenceDEMs(DEMIXRefDEM) then begin
         DEMIXtileDB := DEMIXtileFill(DEMIXRefDEM,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo);
         GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>=' + IntToStr(MDDef.DEMIX_Full));

         if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
            TStr := 'No filled DEMIX tiles on ' + TestAreaName;
            MessageToContinue(TStr);
            {$IfDef RecordDEMIX} HighlightLineToDebugFile(TStr); {$EndIf}
         end
         else begin
         (*
            if (IceSatFName <> '') then IceSatTable := tMyData.Create(IceSatFName);
            if (GeodeticFName <> '') then GeodeticTable := tMyData.Create(GeodeticFName);
        *)

            {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('DEMs start loading'); {$EndIf}

            DoReferenceCriteria;
            GISdb[DEMIXtileDB].MyData.First;  //placed here to see where in the tile processing the program is at

            NoneMissing := LoadDEMIXCandidateDEMs(0);


            if NoneMissing then begin
               if (LandCoverFName <> '') then begin
                  LandCoverGrid := GDALsubsetimageandopen(DEMGlb[DEMIXRefDEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,true,LandCoverFName);
                  if ValidDEM(LandCoverGrid) then begin
                     New(zsForest);
                     New(zsUrban);
                     New(zsBarren);
                  end;
               end;

               new(zs);
               New(zssteep);
               New(zsflat);
               New(zsGentle);
               New(zsCliff);
               {$IfDef RecordDEMIX} writeLineToDebugFile(TestAreaName + '  ProcessDEMIXtestarea in, zs created'); {$EndIf}

               if ElevDiffHists then begin
                  ElevFiles := tStringList.Create;
                  LegendFiles := tStringList.Create;
               end;

               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  if UseThisTile then begin
                     GetTileStatistics(DEMIXRefDEM);
                     for i := 1 to Ser do begin
                        {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoops)}
                           WriteLineToDebugFile(DEMIXTile + ' Start tests for DEM=' + IntToStr(TestDEM[i]) + '/' + IntToStr(Ser) + '  Series=' + TestSeries[i] + '  DEM=' + DEMGlb[TestDEM[i]].AreaName);
                        {$EndIf}
                        GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                        CompareToReference(TestDEM[i],UseDTM,'DTM');
                        CompareToReference(TestDEM[i],UseDSM,'DSM');

(*
                        if Copy(DEMGlb[TestDEM[i]].AreaName,1,4) = 'ALOS' then begin
                           {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoops)} WriteLineToDebugFile('ALOS'); {$EndIf}
                           CompareToReference(TestDEM[i],RefDTMArea,'DTM');
                           CompareToReference(TestDEM[i],RefDSMArea,'DSM');
                        end
                        else if (Copy(DEMGlb[TestDEM[i]].AreaName,1,3) = 'COP') then begin
                           {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoops)} WriteLineToDebugFile('COP'); {$EndIf}
                           if (COPRefDTM <> 0) then CompareToReference(TestDEM[i],COPRefDTM,'DTM')
                           else CompareToReference(TestDEM[i],RefDTMpoint,'DTM');

                           if (COPRefDSM <> 0) then CompareToReference(TestDEM[i],COPRefDSM,'DSM')
                           else CompareToReference(TestDEM[i],RefDSMpoint,'DSM');
                        end
                        else begin
                           {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoops)} WriteLineToDebugFile('other'); {$EndIf}
                           CompareToReference(TestDEM[i],RefDTMPoint,'DTM');
                           CompareToReference(TestDEM[i],RefDSMPoint,'DSM');
                        end;
*)
                        (*
                        DoICESat(TestDEM[i]);
                        DoGeodetic(TestDEM[i]);
                        *)
                        {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('All tests done for ' + TestSeries[i]); {$EndIf}
                     end;
                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               {$If Defined(RecordFullDEMIX)} for i := 1 to 2 do writeLineToDebugFile(''); {$EndIf}
               Dispose(zs);
               Dispose(zssteep);
               Dispose(zsflat);
               Dispose(zsGentle);
               Dispose(zsCliff);
               if ValidDEM(LandCoverGrid) then begin
                  Dispose(zsForest);
                  Dispose(zsUrban);
                  Dispose(zsBarren);
               end;
               if ElevDiffHists then begin
                  ElevFiles.Destroy;
                  LegendFiles.Destroy;
               end;
            end;
            {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('call CloseAndNilNumberedDB(DEMIXtileDB)'); {$EndIf}
            CloseAndNilNumberedDB(DEMIXtileDB);
         end;
      end
      else begin
         {$If Defined(RecordDEMIX)} HighlightLineToDebugFile(TestAreaName + ',  did not load reference data'); {$EndIf}
      end;
      ReportErrors := true;
      {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('closing all DEMs'); {$EndIf}
      CloseAllDEMs;
    end;


var
   DataPath : pathStr;
   i : integer;
   DefaultFilter : byte;
   TStr : shortstring;
begin
   {$IfDef RecordDEMIX} writeLineToDebugFile('Start ComputeDEMIXstats'); {$EndIf}
try
   HeavyDutyProcessing := true;
   SaveBackupDefaults;
   MDdef.DefaultMapXSize := 800;
   MDdef.DefaultMapYSize := 800;

   //settings that can be changed
   ElevDiffHists := false;
   DoHorizontalShift := false;
   MDDef.DEMIX_Full := 75;
   MDDef.SlopeFlatBoundary := 12.5;
   MDDef.SlopeGentleBoundary := 25;
   MDDef.SlopeSteepBoundary := 50;
   MDDef.LandTypePointsNeeded := 100;
   LandTypeMask := 'ALL';

   if (FilesWanted = Nil) then begin
      DefaultFilter := 1;
      FilesWanted := tStringList.Create;
      FilesWanted.Add(ExtractFilePath(DEMIXrefDataDir));
      if not GetMultipleFiles('DEMIX control file','*.dbf:*.csv',FilesWanted,DefaultFilter) then exit;
   end;


   OutPutDir := DEMIXresultsDir;
   SafeMakeDir(OutputDir);

   (*
   TStr :=  DateToStr(now);  // + '-' + CurrentMilitaryTime(true);
   for i := 1 to length(TStr) do if TStr[i] in ['/',':'] then TStr[i] := '-';
   OutputDir := OutPutDir + TStr + '\';
   SafeMakeDir(OutputDir);
   *)

   if DoHorizontalShift then SafeMakeDir(OutputDir + 'lags\' );
   if ElevDiffHists then begin
      SafeMakeDir(OutputDir + 'elev_diff_hists\' );
      SafeMakeDir(OutputDir + 'slope_diff_hists\' );
      SafeMakeDir(OutputDir + 'ruff_diff_hists\' );
   end;

   {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIX output to ' + OutputDir); {$EndIf}

   for i := 0 to pred(FilesWanted.Count) do begin
      cFName := FilesWanted.Strings[i];
      TestAreaName := ExtractFileNameNoExt(cfName);
      if FileExists(OutPutDir +'Elev_diff_stats_' + TestAreaName + '.csv')  then begin
         {$IfDef RecordDEMIX} writeLineToDebugFile(TestAreaName + ' already processed'); {$EndIf}
      end
      else begin
         InitializeStringLists;

         GeodeticFName := '';
         IceSatFName := '';
         LocalDatumAddFName := '';
         LocalDatumSubFName := '';
         RefDSMPointFName := '';
         RefDSMareaFName := '';
         RefDTMPointFName := '';
         RefDTMareaFName := '';
         COPRefDTMFName := '';
         COPRefDSMFName := '';
         LandCoverFName := '';

         {$IfDef RecordDEMIX} writeLineToDebugFile('File ' + IntToStr(i) + '  Read rules ' + cfName); {$EndIf}
         Rules := tMyData.Create(cfName);
         if Rules.FieldExists('DATA_PATH') then begin
            {$IfDef RecordDEMIXFull} writeLineToDebugFile('DATA_PATH option'); {$EndIf}

            while (not Rules.eof) do begin
               DataPath := Rules.GetFieldByNameAsString('DATA_PATH');
               RefDTMPointFName := DataPath + Rules.GetFieldByNameAsString('DTM_POINT');
               RefDTMareaFName :=  DataPath + Rules.GetFieldByNameAsString('DTM_AREA');
               RefDSMPointFName := DataPath + Rules.GetFieldByNameAsString('DSM_POINT');
               RefDSMareaFName := DataPath + Rules.GetFieldByNameAsString('DSM_AREA');
               LandCoverFName := Rules.GetFieldByNameAsString('LANDCOVER');
               ProcessDEMIXtestarea;
               Rules.Next;
            end;
         end
         else begin
            while (not Rules.eof) do begin
               fName := Rules.GetFieldByNameAsString('FILENAME');
               if (fName <> '') then begin
                  if Not FileExists(fName) then fName[1] := cfName[1];   //fix for external hard drive which moves around
                  //if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'GEODETIC' then GeodeticFName := fName;
                  //if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'ICESAT2' then IceSatFName := fName;
                  if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_POINT' then RefDTMPointFName := fName;
                  if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_AREA' then RefDTMareaFName := fName;
                  if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_POINT' then RefDSMPointFName := fName;
                  if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_AREA' then RefDSMareaFName := fName;
                  if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'COP_REF_DTM' then COPRefDTMFName := fName;
                  if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'COP_REF_DSM' then COPRefDSMFName := fName;
                  if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_ADD' then LocalDatumAddFName := fName;
                  if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_SUB' then LocalDatumSubFName := fName;
                  if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'LANDCOVER' then LandCoverFName := fName;
               end;
               Rules.Next;
            end;
            {$IfDef RecordDEMIXFull} writeLineToDebugFile('ProcessDEMIXtestarea in, rules read ' + ExtractFilePath(fName)); {$EndIf}
             ProcessDEMIXtestarea;
         end;
         Rules.Destroy;


         FinalizeStringLists(false);
      end;
   end;
   {$IfDef RecordDEMIXFull} WriteLineToDebugFile('All processing done'); {$EndIf}
   FilesWanted.Destroy;
finally;
   HeavyDutyProcessing := false;
   RestoreBackupDefaults;
end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('End ComputeDEMIXstats'); {$EndIf}
end;


procedure BatchResampleForDEMIX(FilesWanted : tStringList = Nil);
//aggregates to 1" DEMs
var
   fName : PathStr;
   i,DEM : integer;
begin
   {$If Defined(RecordIniMemoryOverwrite) or Defined(TimeLoadDEM)} IniMemOverwriteCheck('start OpenNewDEM'); {$EndIf}
   if (FilesWanted = Nil) then begin
      FilesWanted := tStringList.Create;
      FilesWanted.Add(LastDEMName);
      if not GetMultipleFiles('DEMs for aggregating',DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then exit;
   end;

   try
      HeavyDutyProcessing := true;
      StartSingleThreadTimer('Create DEMIX reference DEMs');
      for i := 0 to pred(FilesWanted.Count) do begin
        ThreadTimers.UpdateThreadStats(9, round(100 * (i / FilesWanted.Count)));
        fName := FilesWanted.Strings[i];
        LoadNewDEM(DEM,fName,true);
        if ValidDEM(DEM) then begin
            ResampleForDEMIXOneSecDEMs(DEM);
            CloseSingleDEM(DEM);
        end;
      end;
   finally
      EndThreadTimers;
      HeavyDutyProcessing := false;
   end;
end;


procedure DEMIX_elevation_histograms;
var
   ElevFiles,LegendFiles : tStringList;
   DefaultFilter : byte;
   TStr : string;
   i : integer;
begin
   {$IfDef RecordDEMIX} writeLineToDebugFile('Twmdem.DEMIXelevationhistograms1Click in'); {$EndIf}
   StopSplashing;
   ElevFiles := tStringList.Create;
   ElevFiles.Add('H:\demix_wine_contest\wine_results\05-06-22--0904-50_elev_diffs\elev_diff_hists\');
   DefaultFilter := 1;
   if GetMultipleFiles('Files with elevation distributions','Files|*.z',ElevFiles,DefaultFilter) then begin
      LegendFiles := tStringList.Create;
      for I := 0 to pred(ElevFiles.Count) do begin
         TStr := ExtractFileNameNoExt(ElevFiles[i]);
         LegendFiles.Add(BeforeSpecifiedCharacter(TStr,'-'));
         {$IfDef RecordDEMIX} writeLineToDebugFile('Twmdem.DEMIXelevationhistograms1Click ' + Tstr); {$EndIf}
      end;
      CreateMultipleHistograms(MDDef.CountHistograms,ElevFiles,LegendFiles,'Elevation difference','Elevation difference distribution');
   end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('Twmdem.DEMIXelevationhistograms1Click out'); {$EndIf}
end;


//some of these have the file names hard coded
//this was designed for a particular purpose, and might not be generally applicable

procedure DEMIXtilesStats;
//  11-18-22 removed since it does not appear very useful
//used DEM=1 to mask all other open DEMs (say loaded from indexed map library)
//found all DEMIX tiles, and did moments report
//each tile and DEM in a separate window
var
   DEMIXtiles,i,Valid : integer;
   bb : sfBoundBox;
begin
   if ValidDEM(1) then begin
      {$IfDef RecordBatch} WriteLineToDebugFile('Twmdem.DEMIXtiles1Click in'); {$EndIf}
      DemixTiles := DEMGlb[1].SelectionMap.LoadDEMIxTileOutlines(DEMGlb[1].SelectionMap.MapDraw.MapCorners.BoundBoxGeo);
      Pick_Geostats.DoGeoStatAnalysis;

      for i := 2 to MaxDEMDataSets do if ValidDEM(i) then begin
         if ExtractFilePath(UpperCase(DEMGlb[i].DEMFileName)) = UpperCase(MDTempDir) then begin
            DEMGlb[i].CutOutGeoBox(DEMGlb[1].DEMBoundBoxGeo);
            DEMGlb[i].SelectionMap.MaskFromSecondGrid(1, msSecondMissing);
            DEMGlb[i].CheckMaxMinElev;
            DEMGlb[i].SavePartOfDEMWithData(DEMGlb[i].DEMFileName);
            DEMGlb[i].DEMAlreadyDefined := false;
            DEMGlb[i].SelectionMap.ReloadDEMClick(Nil);
            {$IfDef RecordBatch} WriteLineToDebugFile(DEMGlb[i].KeyDEMParams(true)); {$EndIf}
         end;
      end;

      GISdb[DemixTiles].LayerIsOn := false;
      GISdb[DemixTiles].MyData.First;
      while not GISdb[DemixTiles].MyData.eof do begin
         bb := GISdb[DemixTiles].MyData.GetRecordBoundingBox;
         PickGeoStat.GroupTitle := GISdb[DemixTiles].MyData.GetFieldByNameAsString('NAME');
         {$IfDef RecordBatch} Valid := DEMGlb[1].ValidElevsInDEM; WriteLineToDebugFile(IntToStr(1) + ' full DEM valid=' + IntToStr(Valid)); {$EndIf}
         DEMGlb[1].CutOutGeoBox(bb);
         DEMGlb[1].SelectionMap.RespondToChangedDEM;
         Valid := DEMGlb[1].ValidElevsInDEM;
         {$IfDef RecordBatch} WriteLineToDebugFile('Tile=' + PickGeoStat.GroupTitle + ' valid=' + IntToStr(Valid)); {$EndIf}
         if (Valid > 5000) then begin
            for i := 2 to MaxDEMDataSets do if ValidDEM(i) then begin
               DEMGlb[i].SelectionMap.OutlineGeoBox(bb,clRed,3);
               DEMGlb[i].CutOutGeoBox(bb);
               DEMGlb[i].SelectionMap.RespondToChangedDEM;
               DEMGlb[i].SelectionMap.BringToFront;
            end;
            PickGeoStat.DifferenClick(Nil);
            for i := 2 to MaxDEMDataSets do if ValidDEM(i) then begin
               DEMGlb[i].SelectionMap.ReloadDEMClick(Nil);
            end;
         end;
         DEMGlb[1].SelectionMap.ReloadDEMClick(Nil);
         GISdb[DemixTiles].MyData.Next;
      end;
      {$IfDef RecordBatch} WriteLineToDebugFile('Twmdem.DEMIXtiles1Click out'); {$EndIf}
   end;
end;




//from this point on, experimental routines not likely to work fully or correctly


procedure BatchGlobalDEMs;

     procedure OneDir(Lidar,Output : PathStr);
     begin
         {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.LidarandglobalDEMs1Click in, ' + Lidar); {$EndIf}
          OvelayPointClouds(Nil,Lidar);
         {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.LidarandglobalDEMs1Click opened'); {$EndIf}
          pt_cloud_opts_fm.CreateGridOrDBfromStats(dbpcDataBase,50,Output);
         {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.LidarandglobalDEMs1Click db created, ' + Output); {$EndIf}
         pt_cloud_opts_fm_Close;
         {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.LidarandglobalDEMs1Click out'); {$EndIf}
     end;


begin
   StopSplashing;
   //OneDir('H:\00-tgis_paper\lidar\Icod\','H:\00-tgis_paper\Icod_lidar.csv');
   //OneDir('H:\00-tgis_paper\lidar\Oahu\','H:\00-tgis_paper\Oahu_lidar.csv');
   //OneDir('H:\00-tgis_paper\lidar\Bled\','H:\00-tgis_paper\Bled_lidar.csv');
   //OneDir('H:\00-tgis_paper\lidar\Canyon_Mtns\','H:\00-tgis_paper\Canyon_Mtns_lidar.csv');
   //OneDir('H:\00-tgis_paper\lidar\Blue_Ridge\','H:\00-tgis_paper\Blue_Ridge_lidar.csv');
   OneDir('H:\00-tgis_paper\lidar\El_Hiero\','H:\00-tgis_paper\El_Hiero_lidar.csv');
   OneDir('H:\00-tgis_paper\lidar\Redwoods\','H:\00-tgis_paper\RedWoods_lidar.csv');
end;



procedure CreateDEMsfromLidar;
var
   CloudFNames,DEMsToCreate : PathStr;
   Clouds : tStringList;
   I : Integer;
begin
   {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.CreateDEMsfromlidar1Click in'); {$EndIf}
   StopSplashing;
   CloudFNames := 'c:\microdem\clouds.txt';
   DEMsToCreate := 'c:\microdem\match_tandemx_dems.dbf';
   Clouds := tStringList.Create;
   Clouds.LoadFromFile(CloudFNames);
   for I := 0 to pred(Clouds.Count) do begin
      {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Clouds.Strings[i]'); {$EndIf}
       if PathIsValid(Clouds.Strings[i]) then begin
          OvelayPointClouds(Nil,Clouds.Strings[i]);
          if (pt_cloud_opts_fm.LasFiles[1].CloudName <> '') then begin
             pt_cloud_opts_fm.AutoSaveDir := 'c:\temp\';
             pt_cloud_opts_fm.DEMrulesFName := DEMstoCreate;
             pt_cloud_opts_fm.BitBtn50Click(Nil);
          end
          else begin
             {$IfDef RecordProblems} HighlightLineToDebugFile('No LAS files in ' + Clouds.Strings[i]); {$EndIf}
          end;
          pt_cloud_opts_fm_Close;
          CloseAllWindowsAndData;
       end
       else begin
          {$IfDef RecordProblems} HighlightLineToDebugFile('Invalid dirctory  ' + Clouds.Strings[i]); {$EndIf}
       end;
   end;
   {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.CreateDEMsfromlidar1Click out'); {$EndIf}
end;


procedure DEMIXreferenceDEMtiles;
var
   RefDataPath,DEMIXRefData,fName, RefDEMDir,RefDEMtiles : PathStr;
   aTable : Petmar_db.tMyData;
   Areas,FoundFiles : tStringList;
   i,WantedDEM,db : integer;
   TStr : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMtiles in'); {$EndIf}
   HeavyDutyProcessing := true;
   StopSplashing;
   RefDataPath := 'H:\new_demix_reference_dems\';
   RefDEMDir := RefDataPath+ '1sec_ref_dems\';
   RefDEMtiles := RefDataPath+ '1sec_ref_demix_tiles\';
   SafeMakeDir(RefDEMtiles);
   FoundFiles := Nil;
   Areas := tStringList.Create;
   Areas.Add('AREA,REF_TYPE,DEMIX_TILE');

   FindMatchingFiles(RefDEMDir,'*.tif',FoundFiles,2);
   for I := 0 to pred(FoundFiles.Count) do begin
      wmdem.SetPanelText(1, IntToStr(i) + '/' + IntToStr(FoundFiles.Count));
      fName := FoundFiles.Strings[i];
      WantedDEM := OpenNewDEM(fName);
      //db := DEMGlb[WantedDEM].SelectionMap.LoadDEMIXtileOutlines(DEMGlb[WantedDEM].DEMBoundBoxGeo,true);
      DB := DEMIXtileFill(WantedDEM,DEMGlb[WantedDEM].DEMBoundBoxGeo);
      GISdb[db].SaveCurrentDBaseSubset(RefDEMtiles + DEMGlb[WantedDEM].AreaName + '_demix_tiles.dbf');
      GISdb[db].ApplyGISFilter('GRID_FULL>=' + IntToStr(MDDef.DEMIX_Full));
      if StrUtils.AnsiContainsText(DEMGlb[WantedDEM].AreaName,'_dsm') then TStr := 'DSM' else TStr := 'DTM';
      TStr := DEMGlb[WantedDEM].AreaName + ',' + TStr + ',' + IntToStr(GISdb[DB].MyData.FiltRecsInDB);
      Areas.Add(TStr);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMtiles, ' + Tstr); {$EndIf}
      CloseSingleDEM(WantedDEM);
   end;

   StringList2CSVtoDB(Areas);
   wmdem.SetPanelText(1,'');
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMtiles in'); {$EndIf}
end;


procedure DEMIXreferenceDEMs;
var
   RefDataPath,DEMIXRefData,fName, RefDEMDir : PathStr;
   aTable : Petmar_db.tMyData;
   Areas,FoundFiles : tStringList;
   i,WantedDEM : integer;
   TStr : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs in'); {$EndIf}
   try
      HeavyDutyProcessing := true;
      StopSplashing;

      RefDataPath := 'H:\new_demix_reference_dems\';
      DEMIXRefData := RefDataPath + 'ref_dems.dbf';
      aTable := Petmar_db.tMyData.Create(DEMIXRefData);
      RefDEMDir := RefDataPath+ '1sec_ref_dems\';
      SafeMakeDir(RefDEMDir);

      Areas := aTable.UniqueEntriesInDB('SERIES');
      for i := 0 to pred(Areas.Count) do begin
         wmdem.SetPanelText(1, IntToStr(i) + '/' + IntToStr(FoundFiles.Count));
         fName := RefDEMDir + 'md_' + Areas[i] + '_1sec_area.tif';
         tstr := ', area=' + Areas.Strings[i] + '  ' + IntToStr(i) + '/' + IntToStr(Areas.Count);
         if FileExists(fName) then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs alread done' + TStr); {$EndIf}
         end
         else begin
            {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXreferenceDEMs' + Tstr); {$EndIf}
            aTable.ApplyFilter('SERIES=' + QuotedStr(Areas.Strings[i]));
            FoundFiles := aTable.UniqueEntriesInDB('FILENAME');
            if (FoundFiles.Count > 0) then begin
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs, area=' + Areas.Strings[i] + ' merge=' + IntToStr(FoundFiles.Count)); {$EndIf}
               MergeMultipleDEMsHere(WantedDEM,FoundFiles,true,false);
               if (WantedDEM <> 0) then begin
                  {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs call resample'); {$EndIf}
                  ResampleForDEMIXOneSecDEMs(WantedDEM,RefDEMDir);
                  CloseSingleDEM(WantedDEM);
               end;
               CleanUpTempDirectory;  //might be many tiled or compressed DEMs expanded
            end
            else begin
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs, area=' + Areas.Strings[i] + ' no dems'); {$EndIf}
            end;
         end;
      end;
   finally
      Areas.Free;
      aTable.Destroy;
      HeavyDutyProcessing := false;
      wmdem.SetPanelText(1, '');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs out'); {$EndIf}
end;


procedure IndexDEMIXreferenceDEMs;
var
   RefDataPath,DEMIXRefData,fName : PathStr;
   aTable : Petmar_db.tMyData;
   FoundFiles : tStringList;
   i : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('IndexDEMIXreferenceDEMs in'); {$EndIf}
   RefDataPath := 'H:\new_demix_reference_dems\';
   DEMIXRefData := RefDataPath + 'ref_dems.dbf';
   if not FileExists(DEMIXRefData) then CreateIntegratedDataBaseTable(DEMIXRefData);
   aTable := Petmar_db.tMyData.Create(DEMIXRefData);
   VerifyMapLibraryFilesExist(aTable);

   FoundFiles := Nil;
   FindMatchingFiles(RefDataPath,'*.*',FoundFiles,2);
   for I := 0 to pred(FoundFiles.Count) do begin
      wmdem.SetPanelText(1, IntToStr(i) + '/' + IntToStr(FoundFiles.Count));
      fName := FoundFiles.Strings[i];
      if StrUtils.AnsiContainsText(fName,'md_') and (StrUtils.AnsiContainsText(fName,'point') or StrUtils.AnsiContainsText(fName,'area')) then begin
         //this is a reference 1" DEM
      end
      else begin
         aTable.ApplyFilter('FILENAME=' + QuotedStr(fName));
         if (aTable.FiltRecsInDB = 0) and ValidDEMExt(ExtractFileExt(fName)) then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile(fName); {$EndIf}
            CheckDEMFileForMapLibrary(aTable,LastSubDir(ExtractFilePath(fName)),fName);
            CleanUpTempDirectory;
         end;
      end;
   end;
   wmdem.SetPanelText(1, '');
   aTable.Destroy;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('IndexDEMIXreferenceDEMs out'); {$EndIf}
end;





(*
            procedure ComputeCorrelationCoefficient(DEM,RefDEM : integer);
            var
               Col,Row,Npts : integer;
               x,y : ^bfarray32;
               Lat,Long : float64;
               a,b,siga,sigb,r,z,zref : float32;
            begin
               if (RefDEM = 0) then exit;
               New(x);
               New(y);
               {$IfDef RecordDEMIX} writeLineToDebugFile('DoCorrelationCoefficient DEM=' + DEMGLB[DEM].AreaName); {$EndIf}
               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  if UseThisTile then begin
                     Npts := 0;
                     bbgrid := GridBoundingBox(DEM,RefDEM,true);
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[DEM].GetElevMetersOnGrid(col,row,z) then begin
                              DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                              if DEMGlb[RefDEM].GetElevFromLatLongDegree(Lat,Long,zref) then begin
                                 x^[NPts] := zref;
                                 y^[NPts] := z;
                                 inc(NPts);
                              end;
                           end;
                        end;
                     end;
                     fit(x^,y^,NPts,a,b,siga,sigb,r);
                     r := r*r;
                     CorrelationCoefficientsStats.Add(LineHeader(DEM,RefDEM) + RealToString(r,-12,-5) + ',' + RealToString(a,-8,-4) + ',' + RealToString(b,-8,-4) );
                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               Dispose(x);
               Dispose(y);
            end;


         procedure DoElevMoments(DEM,RefDEM : integer);
         var
            aMomentVar : tMomentVar;
            gbb : tGridLimits;
         begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('DoElevMoments, test DEM ' + DEMGLB[DEM].AreaName); {$EndIf}
            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               if UseThisTile then begin
                  aMomentVar.NPts := 0;
                  gbb := GridBoundingBox(DEM,RefDEM,true);
                  {$IfDef RecordFullDEMIX} WriteTileDetails(gbb); {$EndIf}
                  aMomentVar := DEMGlb[DEM].ElevationMoments(gbb);
                  ElevMoments.Add(DEMIXtile + ' ' + DEMGlb[DEM].AreaName +  MomentResultsToString(aMomentVar) + ',' + MomentStatsString(aMomentVar));
                  if (RefDEM = 0) then begin //save to use for differences
                     ElevMomentVar[DEM] := aMomentVar;
                  end
                  else begin
                     ElevMomentDiffStats.Add(LineHeader(DEM,RefDEM) + ',' +  MomentDifferenceString(aMomentVar,ElevMomentVar[RefDEM]));
                  end;
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
         end;

         procedure DoSlopeMoments(DEM,RefDEM : integer);
         var
            aMomentVar : tMomentVar;
            gbb : tGridLimits;
         begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('DoSlopeMoments, test DEM ' + DEMGlb[DEM].AreaName); {$EndIf}
            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               if UseThisTile then begin
                  aMomentVar.NPts := 0;
                  gbb := GridBoundingBox(DEM,RefDEM,true);
                  DEMGlb[DEM].SlopeMoments(gbb,aMomentVar);
                  if (aMomentVar.Npts > 1) then begin
                     {$IfDef RecordFullDEMIX} writeLineToDebugFile('Slope moments, ' + LineHeader(DEM,RefDEM)); {$EndIf}
                     {$IfDef RecordFullDEMIX} writeLineToDebugFile(DEMIXtile + ' ' + DEMGlb[DEM].AreaName + MomentStatsString(aMomentVar) + ',' + MomentResultsToString(aMomentVar)); {$EndIf}
                     SlopeMoments.Add(DEMIXtile + ' ' + DEMGlb[DEM].AreaName  + MomentResultsToString(aMomentVar) + ',' + MomentStatsString(aMomentVar));
                     if (RefDEM = 0) then begin //save to use for differences
                        SlopeMomentVar[DEM] := aMomentVar;
                     end
                     else begin
                        SlopeMomentDiffStats.Add(LineHeader(DEM,RefDEM) + MomentDifferenceString(aMomentVar,SlopeMomentVar[RefDEM]));
                     end;
                  end;
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
         end;


         procedure DoPeakPits(DEM,RefDEM : integer);
         var
            DummyResults : tStringList;
            Pits,Peaks : integer;
         begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('Peak and pits, full DEM'); {$EndIf}
            DummyResults := tStringList.Create;
            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               if UseThisTile then begin
                  Pits := FindPits(DEM,GridBoundingBox(DEM,RefDEM),DummyResults,Nil);
                  Peaks := FindPeaks(DEM,GridBoundingBox(DEM,RefDEM),DummyResults,Nil);
                  if (RefDEM = 0) then begin
                     NumPits[DEM] := Pits;
                     NumPeaks[DEM] := Peaks;
                  end
                  else begin
                     PitsPeaks.Add(LineHeader(DEM,RefDEM) + IntToStr(Pits) + ',' + IntToStr(Peaks) + ',' + IntToStr(Pits-NumPits[RefDEM]) + ',' + IntToStr(Peaks-NumPeaks[RefDEM]) );
                  end;
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
         end;
*)

(*
         procedure DoHorizontalShift(DEM,RefDEM : integer);
         var
            BigResults : tStringList;
            fName : PathStr;
            AllLags,db : integer;
            aLine : shortstring;
            aMomentVar : tMomentVar;
         begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('DoHorizontalShift ' + DEMGLB[DEM].AreaName); {$EndIf}
            //the correlation window
            MDDef.LagSearchRadius := 10;

            //the sampling factor
            MDDef.LagCenterShift := 10;

            //the exploration window
            AllLags := 10;
            MDDef.ShiftLoX := -AllLags;
            MDDef.ShiftHighX := AllLags;
            MDDef.ShiftLoY := -AllLags;
            MDDef.ShiftHighY := AllLags;

            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               if UseThisTile then begin
                  BigResults := Nil;
                  OneLag(RefDEM,DEM,GridBoundingBox(RefDEM,0),BigResults);
                  fname := OutputDir + 'lags\' + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_' + DEMGLB[RefDEM].AreaName +  '.dbf';
                  {$IfDef RecordFullDEMIX} writeLineToDebugFile('Output=' + fName); {$EndIf}
                  db := StringList2CSVtoDB(BigResults, fName);
                  if (db <> 0) then begin
                     aMomentVar := GISdb[db].GetFieldStatistics('TOTAL_LAG');
                     aMomentVar.LE90 := Percentile(90,zs^,aMomentVar.NPts,false);
                     aLine := RealToString(aMomentVar.MaxZ,-8,2) + ',' + RealToString(aMomentVar.Mean,-8,2) + ',' + RealToString(aMomentVar.sdev,-8,2) + ',' + RealToString(aMomentVar.median,-8,2) +
                         ',' + RealToString(aMomentVar.LE90,-8,2) + ',' + IntToStr(aMomentVar.NPts);
                     {$IfDef RecordFullDEMIX} writeLineToDebugFile(aline); {$EndIf}
                     LagStats.Add(LineHeader(DEM,RefDEM) + aLine);
                     CloseAndNilNumberedDB(db);
                  end;
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
         end;
*)



(*
procedure DEMIXreferenceDEMs;
var
   DirsToProcess,DEMstoMerge : tStringList;
   i,WantedDEM : integer;
   fName,ThisDir,MergeDEMDir,MergeDEMName,MergefName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs in'); {$EndIf}
   try
      HeavyDutyProcessing := true;
      StopSplashing;
      MergeDEMDir := 'H:\new_reference_dems\merged_ref_dems\';
      DirsToProcess := tStringList.Create;
      DirsToProcess.Add(MergeDEMDir);
      if GetMultipleDirectories('High res DEMs',DirsToProcess) then begin
         for i := 0 to pred(DirsToProcess.Count) do begin
            ThisDir := DirsToProcess.Strings[i];
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs process ' + ThisDir); {$EndIf}
            MergeDEMName := LastSubDir(ThisDir);
            MergefName := MergeDEMDir + MergeDEMname + '.dem';
            if FileExists(MergefName) then begin
               //WantedDEM := OpenNewDEM(MergefName);
            end
            else begin
               DEMsToMerge := tStringList.Create;
               FindMatchingFiles(ThisDir,'*.tif',DEMstoMerge,2);
               if (DEMstoMerge.Count = 0) then FindMatchingFiles(ThisDir,'*.asc',DEMstoMerge,2);
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs found dems=' + IntToStr(DEMstoMerge.Count)); {$EndIf}
               if (DEMstoMerge.Count > 1) then begin
                  MergeMultipleDEMsHere(WantedDEM,DEMsToMerge,true);
                  {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs merged new dem=' + IntToStr(WantedDEM)); {$EndIf}
                  DEMGlb[WantedDEM].AreaName := MergeDEMName;
                  DEMGlb[WantedDEM].WriteNewFormatDEM(MergefName);
               end;
               //DEMstoMerge.Free;
            end;
            if (WantedDEM <> 0) then begin
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs call resample'); {$EndIf}
               ResampleForDEMIX(WantedDEM);
               CloseSingleDEM(WantedDEM);
            end;
            CleanUpTempDirectory;  //might be many tiled or compressed DEMs expanded
         end;
      end;
   finally
      DirsToProcess.Free;
      HeavyDutyProcessing := false;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs out'); {$EndIf}
end;
*)






