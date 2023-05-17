{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
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
      CreateMultipleHistogram(MDDef.CountHistograms,ElevFiles,LegendFiles,'Elevation difference','Elevation difference distribution');
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






