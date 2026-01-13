{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for demix_definitions   }
{________________________________________}

procedure InventoryAreasAndTilesByCountry(DB : integer);
var
   Countries,Findings : tStringList;
   i : integer;
   BaseFilter : shortstring;
   fName : PathStr;
begin
   if GISdb[db].MyData.FieldExists('DEMIX_TILE') and GISdb[db].MyData.FieldExists('AREA') and GISdb[db].MyData.FieldExists('COUNTRY') then begin
      ShowHourglassCursor;
      GISdb[db].EmpSource.Enabled := false;
      BaseFilter := GISdb[db].MyData.Filter;
      Countries := GISdb[db].MyData.ListUniqueEntriesInDB('COUNTRY');
      Findings := tStringList.Create;
      Findings.Add('COUNTRY,AREAS,DEMIX_TILES');
      for i := 0 to pred(Countries.Count) do begin
         GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'COUNTRY=' + QuotedStr(Countries[i]));
         Findings.Add(Countries[i] + ',' + IntToStr(GISdb[db].NumUniqueEntriesInDB('AREA')) + ',' + IntToStr(GISdb[db].NumUniqueEntriesInDB('DEMIX_TILE')));
      end;
      fName := NextFileNumber(MDTempDir,'country-areas-tiles','.dbf');
      PetdbUtils.StringList2CSVtoDB(Findings,fName);
      Countries.Destroy;
      GISdb[db].ApplyGISFilter(BaseFilter);
      GISdb[db].ShowStatus;
   end
   else begin
      MessageToContinue('Required fields missing');
   end;
end;


procedure MakeTerrainGridsFromMICRODEM(DataDir : PathStr; DEMIndex : integer; IsPoint : boolean);
var
   TestDEMs : tStringList;
   i,DEM,ad : integer;
   AreaName : shortstring;
   fName,fName2 : PathStr;
   PointGrids : array[1..7] of integer;
begin
   try
      GetDEMIXPaths;
      TestDEMs := Nil;
      FindMatchingFiles(DataDir, '*.tif',TestDEMs,1);
      wmdem.SetPanelText(0, 'Started ' + TimeToStr(Now),true);
      for i := 0 to pred(TestDEMs.Count) do begin
          fName := TestDEMs.Strings[i];
          DEM := OpenNewDEM(fName,false);
          AreaName := DEMglb[DEM].AreaName;
          wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(TestDEMs.Count) + '  ' + AreaName,true);

          if (NumFilledDEMIXtilesOnDEM(DEM) = 0) then begin
             {$If Defined(RecordDEMMIX)} HighlightLineToDebugFile('No filled tiles in ' + ExtractFileName(fName)); {$EndIf}
          end
          else begin
             {$If Defined(RecordDEMIX)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             //BoxSize := round(3 * DEMglb[PointGrids[1]].AverageSpace);
             wmdem.SetPanelText(3,DEMIXModeName + ' Hillshade',true);
             fName := ExternalProgramOutPutFile(DEMIndex,'MD','HILL_',AreaName,IsPoint);
             PointGrids[1] := CreateHillshadeMap(false,DEM);
             DEMglb[PointGrids[1]].SaveAsGeotiff(fName);

             wmdem.SetPanelText(3,DEMIXModeName + ' Slope/Ruff',true);
             fName2 := ExternalProgramOutPutFile(DEMIndex,'MD','SLOPE_',AreaName,IsPoint);
             fName := ExternalProgramOutPutFile(DEMIndex,'MD','ROUGH_',AreaName,IsPoint);
             PointGrids[3] := 0;
             PointGrids[2] := CreateSlopeRoughnessSlopeStandardDeviationMap(false,DEM,5,PointGrids[3]);
             DEMglb[PointGrids[3]].SaveAsGeotiff(fName2);
             DEMglb[PointGrids[2]].SaveAsGeotiff(fName);

             wmdem.SetPanelText(3,DEMIXModeName + ' RRI',true);
             fName := ExternalProgramOutPutFile(DEMIndex,'MD','RRI_',AreaName,IsPoint);
             PointGrids[4] := MakeTRIGrid(DEM,nmRRI,false);
             DEMglb[PointGrids[4]].SaveAsGeotiff(fName);

             wmdem.SetPanelText(3,DEMIXModeName + ' TPI',true);
             fName := ExternalProgramOutPutFile(DEMIndex,'MD','TPI_',AreaName,IsPoint);
             PointGrids[5] := BoxCarDetrendDEM(MDDef.OpenSavedMapsFUVSSIM,DEM,{DEMGlb[DEM].FullDEMGridLimits,}3);
             DEMglb[PointGrids[5]].SaveAsGeotiff(fName);

             wmdem.SetPanelText(3,DEMIXModeName + ' Openness',true);
             fName2 := ExternalProgramOutPutFile(DEMIndex,'MD','OPEND_',AreaName,IsPoint);
             fName := ExternalProgramOutPutFile(DEMIndex,'MD','OPENU_',AreaName,IsPoint);
             PointGrids[6] := -1;
             PointGrids[7] := -1;
             ad := 0;   //we don't want openness difference
             CreateOpennessMap(False,DEMGlb[DEM].FullDEMGridLimits,DEM,-99,3,PointGrids[6],PointGrids[7],ad);
             DEMglb[PointGrids[7]].SaveAsGeotiff(fName2);
             DEMglb[PointGrids[6]].SaveAsGeotiff(fName);
             CloseAllDEMs;
           	 {$If Defined(RecordDEMIX)} WriteLineToDebugFile(AreaName + ' MD grids created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
          end;
       end;
   finally
      TestDEMs.Destroy;
      EndDEMIXProcessing;
   end;
end;



procedure InventoryAllDEMIXdata;
var
   Areas,Results,SSIMareas,FUVareas,RefDEMs,TestDEMs,DiffDist : tStringList;
   aline,Area : shortstring;
   j : integer;
   fName : PathStr;
begin
   try
      GetDEMIXPaths;
      Areas := DEMIX_AreasWanted;
      Results := tStringList.Create;
      Results.Add('AREA,REF_DEM,TEST_DEM,FUV,SSIM,DIFF_DIST');

      SSIMareas := tStringList.Create;
      FindMatchingFiles(SSIMresultsDir, '*.csv',SSIMareas,1);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('InventoryAllDEMIXdata, csv in ' + SSIMresultsDir + ': ' + IntToStr(SSIMareas.count)); {$EndIf}
      RemoveFilesThatDoNotHaveString(SSIMareas,'ssim');
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('InventoryAllDEMIXdata, with SSIM ' + SSIMresultsDir + ': ' + IntToStr(SSIMareas.count)); {$EndIf}
      FUVareas := tStringList.Create;
      FindMatchingFiles(SSIMresultsDir, '*.csv',FUVareas,1);
      RemoveFilesThatDoNotHaveString(FUVareas,'fuv');

      RefDEMs := tStringList.Create;
      FindMatchingFiles(DEMIX_Ref_1sec, '*.tif',REFDEMs,1);
      TestDEMs := tStringList.Create;
      FindMatchingFiles(DEMIX_test_dems, '*.tif',TestDEMs,1);
      DiffDist := tStringList.Create;
      FindMatchingFiles(Diff_dist_results_dir, '*.csv',DiffDist,1);

      for j := 0 to pred(Areas.Count) do begin
         Area := Areas.Strings[j];
         aline := Area + ',' + IntToStr(NumberOfStringsWithSubstring(RefDEMs,Area)) + ',' +
                               IntToStr(NumberOfStringsWithSubstring(TestDEMs,Area)) + ',' +
                               IntToStr(NumberOfStringsWithSubstring(FUVareas,Area)) + ',' +
                               IntToStr(NumberOfStringsWithSubstring(SSIMareas,Area)) + ',' +
                               IntToStr(NumberOfStringsWithSubstring(DiffDist,Area));
         Results.Add(aline);
      end;
     fName := NextFileNumber(MDTempDir,'DEMIX_data_inventory','.dbf');
     PetdbUtils.StringList2CSVtoDB(Results,fName);
     Areas.Destroy;
     SSIMareas.Destroy;
     FUVareas.Destroy;
     RefDEMs.Destroy;
     TestDEMs.Destroy;
     DiffDist.Destroy;
   finally
      EndDEMIXProcessing;
   end;
end;



procedure InventoryCriteriaEachDEMIXtile(DB : integer);
var
   Tiles,Results,Criteria : tStringList;
   Tile,Criterion,Aline,IsGood : shortstring;
   i,j,TileIndex,CriterionIndex : integer;
   fName : PathStr;
   Stats : array[0..MaxTiles,0..MaxCriteria] of integer;
begin
   if GISdb[db].MyData.FieldExists('DEMIX_TILE') and GISdb[db].MyData.FieldExists('CRITERION') then begin
      GISdb[db].EmpSource.Enabled := false;
      Tiles := GISdb[db].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
      GISdb[db].EmpSource.Enabled := false;
      Criteria := GISdb[db].MyData.ListUniqueEntriesInDB('CRITERION');
      GISdb[db].ClearGISFilter;
      GISdb[db].EmpSource.Enabled := false;
      for i := 0 to MaxTiles do
         for J := 0 to MaxCriteria do
           Stats[i,j] := 0;
      GISdb[db].MyData.First;
      while not GISdb[db].MyData.eof do begin
         Tile := GISdb[db].MyData.GetFieldByNameAsString('DEMIX_TILE');
         Criterion := GISdb[db].MyData.GetFieldByNameAsString('CRITERION');
         TileIndex := Tiles.IndexOf(Tile);
         CriterionIndex := Criteria.IndexOf(Criterion);
         if (TileIndex <> -1) then begin
            inc(Stats[TileIndex,0]);
            inc(Stats[TileIndex,succ(CriterionIndex)]);
         end;
         GISdb[db].MyData.Next;
      end;
      Results := tStringList.Create;
      aline := 'DEMIX_TILE,TOTAL';
      for I := 1 to Criteria.Count do begin
         aline := aline + ',' + Criteria.Strings[pred(i)];
      end;
      aLine := aline + ',' + 'RESULTS';
      Results.Add(aline);
      for I := 0 to pred(Tiles.Count) do begin
         aline := Tiles.Strings[i] + ',' + IntToStr(Stats[i,0]);
         IsGood := 'Good';
         for j := 1 to Criteria.Count do begin
            aline := aline + ',' + IntToStr(Stats[i,j]);
            if Stats[i,j] = 0 then IsGood := 'Missing'
            else if Stats[i,j] > 1 then IsGood := 'Duplicates';
         end;
         Results.Add(aline + ',' + IsGood);
      end;
      fName := NextFileNumber(MDTempDir,'Criteria_each_cluster','.dbf');
      PetdbUtils.StringList2CSVtoDB(Results,fName);

      GISdb[db].EmpSource.Enabled := true;
      Tiles.Destroy;
      Criteria.Destroy;
   end
   else begin
      MessageToContinue('Requires fields DEMIX_TILE and CRITERION');
   end;
end;


procedure InventoryPercentileByCriterionEachDEMIXtile(DB : integer);
var
   Tiles,Results,Criteria : tStringList;
   Tile,Aline : shortstring;
   i,j,k,n,tileIndex : integer;
   fName : PathStr;
   Stats : array[0..MaxTiles,0..MaxCriteria] of float32;
begin
   if GISdb[db].MyData.FieldExists('DEMIX_TILE') and GISdb[db].MyData.FieldExists('CRITERION') then begin
      GISdb[db].EmpSource.Enabled := false;
      Tiles := GISdb[db].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
      GISdb[db].EmpSource.Enabled := false;
      Criteria := GISdb[db].MyData.ListUniqueEntriesInDB('CRITERION');
      GISdb[db].ClearGISFilter;
      GISdb[db].EmpSource.Enabled := false;
      for i := 0 to MaxTiles do
         for J := 0 to MaxCriteria do
           Stats[i,j] := 0;
      for k := 0 to pred(Criteria.Count) do begin
         GISdb[db].ApplyGISfilter('CRITERION=' + QuotedStr(Criteria.Strings[k]));
         GISdb[db].EmpSource.Enabled := false;
         GISdb[db].MyData.First;
         n := 0;
         while not GISdb[db].MyData.eof do begin
            Tile := GISdb[db].MyData.GetFieldByNameAsString('DEMIX_TILE');
            TileIndex := Tiles.IndexOf(Tile);
            inc(n);
            Stats[TileIndex,k] := GISdb[db].MyData.GetFieldByNameAsFloat('COP');
            GISdb[db].MyData.Next;
         end;
      end;
      Results := tStringList.Create;
      aline := 'DEMIX_TILE,DEM';
      for I := 1 to Criteria.Count do begin
         aline := aline + ',' + Criteria.Strings[pred(i)];
      end;
      Results.Add(aline);
      for I := 0 to pred(Tiles.Count) do begin
         aline := Tiles.Strings[i] + ',COP';
         for j := 0 to pred(Criteria.Count) do begin
            aline := aline + ',' + RealToString(Stats[i,j],-12,-6);
         end;
         Results.Add(aline);
      end;
      fName := NextFileNumber(MDTempDir,'Percentile_criteria_each_cluster','.dbf');
      PetdbUtils.StringList2CSVtoDB(Results,fName);

      GISdb[db].EmpSource.Enabled := true;
      GISdb[db].ClearGISfilter;
      Tiles.Destroy;
      Criteria.Destroy;
   end
   else begin
      MessageToContinue('Requires fields DEMIX_TILE and CRITERION');
   end;
end;


procedure AreasInClusters(DB : integer);
var
   Areas,Results,Clusters : tStringList;
   Area,Aline : shortstring;
   i,j,Cluster,AreaIndex,ClusterIndex : integer;
   fName : PathStr;
   Stats : array[0..MaxAreas,0..MaxClusters] of integer;
begin
   if GISdb[db].MyData.FieldExists('AREA') and GISdb[db].MyData.FieldExists('CLUSTER') then begin
      GISdb[db].EmpSource.Enabled := false;
      Areas := GISdb[db].MyData.ListUniqueEntriesInDB('AREA');
      GISdb[db].EmpSource.Enabled := false;
      Clusters := GISdb[db].MyData.ListUniqueEntriesInDB('CLUSTER');
      GISdb[db].ClearGISFilter;
      GISdb[db].EmpSource.Enabled := false;
      for i := 0 to MaxAreas do
         for J := 0 to MaxClusters do
           Stats[i,j] := 0;
      GISdb[db].MyData.First;
      while not GISdb[db].MyData.eof do begin
         Area := GISdb[db].MyData.GetFieldByNameAsString('AREA');
         Cluster := GISdb[db].MyData.GetFieldByNameAsInteger('CLUSTER');
         AreaIndex := Areas.IndexOf(Area);
         if (AreaIndex <> -1) then begin
            inc(Stats[AreaIndex,0]);
            inc(Stats[AreaIndex,Cluster]);
         end;
         GISdb[db].MyData.Next;
      end;
      Results := tStringList.Create;
      aline := 'AREA,TOTAL';
      for I := 1 to MaxClusters do begin
         ClusterIndex := Clusters.IndexOf(IntToStr(I));
         if ClusterIndex <> -1 then
            aline := aline + ',CLUSTER_' + IntToStr(I);
      end;
      Results.Add(aline);
      for I := 0 to pred(Areas.Count) do begin
         aline := Areas.Strings[i] + ',' + IntToStr(Stats[i,0]);
         for j := 1 to MaxClusters do begin
            ClusterIndex := Clusters.IndexOf(IntToStr(j));
            if ClusterIndex <> -1 then
               aline := aline + ',' + IntToStr(Stats[i,j]);
          end;
          Results.Add(aline);
      end;
      fName := NextFileNumber(MDTempDir,'Area_cluster_composition','.dbf');
      PetdbUtils.StringList2CSVtoDB(Results,fName);

      GISdb[db].EmpSource.Enabled := true;
      Areas.Destroy;
      Clusters.Destroy;
   end
   else begin
      MessageToContinue('Requires fields AREA and CLUSTER');
   end;
end;


procedure OneDegreeTilesToCoverTestAreas;
var
   Areas,Tiles,Results : tStringList;
   DEM,j,i : integer;
   fName : PathStr;
begin
   GetDEMIXPaths;
   Areas := DEMIX_AreasWanted;
   Results := tStringList.Create;
   Results.Add('AREA,DEG_1_TILE');
   for j := 0 to pred(Areas.Count) do begin
      wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(j)) + '/' + IntToStr(Areas.Count) + '  ' + Areas.Strings[j]);
      fName := DEMIX_test_dems + Areas.Strings[j] + '_cop.tif';
      DEM := OpenNewDEM(fName,false);
      Tiles := GetListOfDataInBoxInSeries('SRTM_GL1_srtm',DEMGlb[DEM].DEMBoundBoxGeo);
      for i := 0 to pred(Tiles.Count) do Results.Add(Areas.Strings[j] + ',' + ExtractFileNameNoExt(Tiles.strings[i]));
      CloseSingleDEM(DEM);
      Tiles.Destroy;
   end;
   Areas.Destroy;
   wmdem.SetPanelText(2,'');
   fName := NextFileNumber(MDTempDir,'One_degree_tiles_needed','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   EndDEMIXProcessing;
end;


procedure FixFilesWith42112;
var
   TheBytes : array[0..4000] of byte;
   FilesWanted,Results,Paths : tStringList;
   Tag : shortstring;
   fName,fName2,{NewName,JustName,}BaseDir : PathStr;
   //Num1m,NumEGM,Num1secArea,Num1secPoint,
   i,j,DTMs,DSMs,DEM : Integer;
   Tag42112Offset,Tag42112Length : int64;
   TiffHandle : THandle;
   TStr : ansistring;
begin
   try
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs getting data paths'); {$EndIf}
      GetDosPath('3DEP files',BaseDir);
      FilesWanted := tStringList.Create;
      FindMatchingFiles(BaseDir,'*.tif',FilesWanted,3);
      RemoveFilesThatDoHaveString(FilesWanted,'_egm2008');  //only check the original files
      StartProgress('Checking Geotiffs');
      for I := 0 to pred(FilesWanted.Count) do begin
         if (i mod 25 = 0) then UpdateProgressBar(i/FilesWanted.Count);
         fName := FilesWanted.Strings[i];
         if GetGeotiffTag42112(fName,Tag,Tag42112Offset,Tag42112Length) then begin
            if StrUtils.ANSIContainsText(Tag,'foot') then begin
               TiffHandle := FileOpen(fName,fmOpenReadWrite);
               FileSeek(TiffHandle,Tag42112Offset,0);
               FileRead(TiffHandle,TheBytes[1],Tag42112Length);
               TStr := ByteArrayToString(TheBytes,Tag42112Length);
               TStr := StringReplace(TStr,'US survey foot</Item>','metre</Item>         ',[rfReplaceAll, rfIgnoreCase]);
               StringToByteArray(TStr,TheBytes);
               FileSeek(TiffHandle,Tag42112Offset,0);
               FileWrite(TiffHandle,TheBytes[1],Tag42112Length);
               FileClose(Tiffhandle);
               //now delete any files that had bad elevation conversion in GDAL
               fName2 := ExtractFilePath(fName) + extractFileNameNoExt(fName) + '_egm2008.tif';
               DeleteFileIfExists(fName2);
               fName2 := ExtractFilePath(fName) + extractFileNameNoExt(fName) + '_egm2008' + Ref1SecPointStr + '.tif';
               DeleteFileIfExists(fName2);
               fName2 := ExtractFilePath(fName) + extractFileNameNoExt(fName) + '_egm2008' + Ref1SecAreaStr + '.tif';
               DeleteFileIfExists(fName2);
            end;
         end;
      end;
   finally
      EndProgress;
   end;
end;


procedure FindFilesWith42112;
var
   FilesWanted,Results{,Paths} : tStringList;
   Tag : shortstring;
   fName,{NewName,JustName}BaseDir : PathStr;
   i{,j,DTMs,DSMs} : Integer;
   Tag42112Offset,Tag42112Length : int64;
begin
   try
      Results := tStringList.Create;
      Results.Add('FILE,TAG42114');
      GetDosPath('3DEP files',BaseDir);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('FindFilesWith42112s getting data path ' + BaseDir); {$EndIf}
      FilesWanted := tStringList.Create;
      FindMatchingFiles(BaseDir,'*.tif',FilesWanted,3);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('FindFilesWith42112s DEM tiles=' + IntToStr(FilesWanted.Count)); {$EndIf}
      RemoveFilesThatDoHaveString(FilesWanted,'_egm2008');
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('FindFilesWith42112s no egm2008=' + IntToStr(FilesWanted.Count)); {$EndIf}
      RemoveFilesThatDoHaveString(FilesWanted,'_ref_1sec');
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('FindFilesWith42112s no ref_1sec=' + IntToStr(FilesWanted.Count)); {$EndIf}
      StartProgress('Checking Geotiffs');
      for I := 0 to pred(FilesWanted.Count) do begin
         if (i mod 25 = 0) then UpdateProgressBar(i/FilesWanted.Count);
         fName := FilesWanted.Strings[i];
         if GetGeotiffTag42112(fName,Tag,Tag42112Offset,Tag42112Length) then begin
            if StrUtils.ANSIContainsText(Tag,'foot') then begin
               Results.Add(fName + ',' + Tag);
            end;
         end;
      end;
      if Results.Count = 1 then begin
         Results.Free;
         MessageToContinue('No problematic DEMs with TIFF tag 42112');
      end
      else begin
         fName := NextFileNumber(MDTempDir,'files_with_tag_42112','.dbf');
         PetdbUtils.StringList2CSVtoDB(Results,fName);
      end;
   finally
      EndProgress;
   end;
end;


procedure DeleteFilesForATestArea;
var
   TestArea : shortstring;
   Areas : tStringList;
   j : integer;

         procedure ClearOutFolder(Dir : PathStr);
         var
            FilesWanted : tstringlist;
            i,Deletes : integer;
            fName : PathStr;
         begin
            wmdem.SetPanelText(3,Dir,true);
            FilesWanted := nil;
            FindMatchingFiles(Dir,'*.*',FilesWanted,2);
            Deletes := 0;
            for I := 0 to pred(FilesWanted.Count) do begin
               fName := UpperCase(FilesWanted.Strings[i]);
               if StrUtils.ANSIContainsText(fName,TestArea) then begin
                  DeleteFileIfExists(fName);
                  inc(Deletes);
               end;
            end;
            FilesWanted.Destroy;
            {$If Defined(RecordDEMIX)} if (Deletes > 0) then WriteLineToDebugFile(TestArea + ' ' +IntToStr(Deletes) + ' files deleted in ' + Dir); {$EndIf}
         end;

begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DeleteFilesForATestArea in'); {$EndIf}
   MDDef.DEMIX_mode := dmFull;
   GetDEMIXPaths;
   Areas := DEMIX_AreasWanted;
   for j := 0 to pred(Areas.Count) do begin
      TestArea := UpperCase(Areas.Strings[j]);
      wmdem.SetPanelText(2,'Clear ' + IntToStr(succ(j)) + '/' + IntToStr(Areas.Count) + '  ' +  TestArea,true);
      ShowHourglassCursor;

      if AnswerIsYes('Delete area DEMs and Land cover') then begin
         ClearOutFolder(DEMIX_area_lc100);
         ClearOutFolder(DEMIX_Ref_1sec);
         ClearOutFolder(DEMIX_test_dems);
         //ClearOutFolder(DEMIX_diluvium_dems);
         //ClearOutFolder(DEMIX_delta_dems);
         //ClearOutFolder(DEMIX_coastal_dems);
      end;
      ClearOutFolder(Diff_dist_results_dir);
      ClearOutFolder(DEMIX_diff_dist);
      ClearOutFolder(DEMIX_diff_maps_dir);
      ClearOutFolder(DEMIX_profile_test_dir);
      ClearOutFolder(DEMIX_distrib_graph_dir);

      {$IfDef DEMIX_SAGA_channels}
         ClearOutFolder(DEMIX_test_DEMs_no_sink);
         ClearOutFolder(DEMIX_ref_DEMs_no_sink);
         ClearOutFolder(DEMIX_test_DEMs_channels);
         ClearOutFolder(DEMIX_ref_DEMs_channels);
         ClearOutFolder(DEMIX_test_DEMs_channel_grids);
         ClearOutFolder(DEMIX_ref_DEMs_channel_grids);
      {$EndIf}

      {$IfDef ExternalProgramsSaveCriteria}
          ClearOutFolder(MD_out_ref_dir);
          ClearOutFolder(MD_out_test_dir);
          ClearOutFolder(wbt_out_ref_dir);
          ClearOutFolder(wbt_out_test_dir);
          ClearOutFolder(saga_out_ref_dir);
          ClearOutFolder(saga_out_test_dir);
      {$EndIf}


      ClearOutFolder(SSIMresultsDir);

      ClearOutFolder(FUVresultsDir);
      ClearOutFolder(PartialsResultsDir);
      ClearOutFolder(CurvaturesResultsDir);
   end;
   EndDEMIXProcessing;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DeleteFilesForATestArea out'); {$EndIf}
end;


procedure CriteriaInSSIM_FUV_db(db : integer);
var
   Areas,Results : tStringList;
   fName : PathStr;
   TStr : shortstring;
   i,Tiles,Criteria,Records : integer;
begin
   SetColorForProcessing;
   GISdb[db].EmpSource.Enabled := false;
   Areas := GISdb[db].MyData.ListUniqueEntriesInDB('AREA');
   Results := tStringList.Create;
   Results.Add('AREA,DEMIX_TILES,CRITERIA,RECORDS,COMPLETE');
   for I := 0 to pred(Areas.Count) do begin
      GISdb[db].ApplyGISFilter('AREA=' + QuotedStr(Areas.Strings[i]));
      GISdb[db].EmpSource.Enabled := false;
      Tiles := GISdb[db].MyData.NumUniqueEntriesInDB('DEMIX_TILE');
      Criteria := GISdb[db].MyData.NumUniqueEntriesInDB('CRITERION');
      Records := GISdb[db].MyData.FiltRecsInDB;
      if (Records = Tiles * Criteria) then TStr := 'Yes'
      else begin
         if (Records < Tiles * Criteria) then TStr := 'NO--Missing' else TStr := 'NO--Duplicates';
      end;
      Results.Add(Areas.Strings[i] + ',' + IntToStr(Tiles) + ',' + IntToStr(Criteria) + ',' + IntToStr(Records) + ',' + TStr);
   end;
   Areas.Free;
   fName := NextFileNumber(MDTempDir,'ssim-fuv_inventory','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   GISdb[db].ClearGISFilter;
   GISdb[db].EmpSource.Enabled := true;
   SetColorForWaiting;
end;


procedure InventoryDEMIXdifferenceStats;
var
   TheFiles,Results,Areas : tStringList;
   i,j,n : integer;
   fName : PathStr;
begin
   Areas := DEMIX_AreasWanted;
   n := Areas.Count;
   TheFiles := Nil;
   FindMatchingFiles(Diff_dist_results_dir,'*.csv',TheFiles);
   Results := tStringList.Create;
   for j := pred(Areas.Count) downto 0 do begin
      for i := 0 to pred(TheFiles.Count) do begin
         fName := UpperCase(ExtractFileNameNoExt(TheFiles.Strings[i]));
         if StrUtils.AnsiContainsText(fName,UpperCase(Areas.Strings[j])) then begin
            Areas.Delete(j);
            break;
         end;
      end;
   end;
   if (Results.Count = 0) then MessageToContinue('Difference stats done for all areas, n=' + IntToStr (N))
   else DisplayAndPurgeStringList(Results,'Areas Missing DEMIX difference stats (n=' + IntToStr(Results.Count) + ')');
   TheFiles.Free;
end;



procedure InventoryDEMIX_SSIM_FUV_Stats;
var
   TheFiles,Results,Areas : tStringList;
   i,j,n : integer;
   fName : PathStr;
begin
   GetDEMIXPaths;
   Areas := DEMIX_AreasWanted;
   n := Areas.Count;
   TheFiles := Nil;
   FindMatchingFiles(SSIMresultsDir,'*.csv',TheFiles);
   Results := tStringList.Create;
   for j := pred(Areas.Count) downto 0 do begin
      for i := 0 to pred(TheFiles.Count) do begin
         fName := UpperCase(ExtractFileNameNoExt(TheFiles.Strings[i]));
         if StrUtils.AnsiContainsText(fName,UpperCase(Areas.Strings[j])) then begin
            Areas.Delete(j);
            break;
         end;
      end;
   end;
   if (Results.Count = 0) then MessageToContinue('SSIM/FUV stats done for all areas, n=' + IntToStr (N))
   else DisplayAndPurgeStringList(Results,'Areas Missing SSIM/FUV stats (n=' + IntToStr(Results.Count) + ')');
   TheFiles.Free;
end;




procedure InventoryDBwithDSMandDTMbyArea;
//this should be redone now that the DSMs are in a different directory
var
   FilesWanted,Areas,Results : tStringList;
   AreaName : shortstring;
   fName,NewName,JustName : PathStr;
   i,j,DTMs,DSMs : Integer;
begin
   GetDEMIXpaths(false);
   Areas := tStringList.Create;
   Areas.LoadFromFile(AreaListFName);
   Results := tStringList.Create;
   Results.Add('AREA,REF_DTMs,REF_DSMs');
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',FilesWanted,1);
   for i := 0 to pred(Areas.Count) do begin
      AreaName := Areas.Strings[i];
      DSMs := 0;
      DTMs := 0;
      for j := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[j];
         if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) then begin
            JustName := ExtractFileName(fname);
            if StrUtils.AnsiContainsText(UpperCase(fname),'DSM') then inc(DSMs)
            else inc(DTMs);
         end;
      end;
      Results.Add(AreaName + ',' + IntToStr(DTMs) + ',' + IntToStr(DSMs));
   end;
   fName := NextFileNumber(MDTempDir,'ref_dem_status','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   EndDEMIXProcessing;
end;


function DoAllFilesHaveSetEGMandPixelIs(FilesWanted : tStringList; var Results : tstringList) : boolean;
var
   Tiles75,Tiles50,Tiles25,
   j,WantedDEM,DEMIXtileDB : Integer;
   fName : PathStr;
   Lat,Long : float64;
begin
   Result := true;
   Results := tStringList.Create;
   Results.Add('AREA_DEM,TILES_75,TILES_50,TILES_25,LAT_CENT,LONG_CENT,PIXEL_IS,VERT_DATUM');
   for j := 0 to pred(FilesWanted.Count) do begin
      fName := FilesWanted.Strings[j];
      wmdem.SetPanelText(2, 'File: ' + IntToStr(succ(j)) + '/' + IntToStr(FilesWanted.Count) + '  ' + ExtractFileNameNoExt(fName));
      WantedDEM := 0;
      if NewArea(true,WantedDEM,'',FName,WantedDEM) and ValidDEM(WantedDEM) then begin
         DEMGlb[WantedDEM].DEMCenterPoint(Lat,Long);
         DEMIXtileDB := DEMIXtileFill(WantedDEM,DEMGlb[WantedDEM].DEMBoundBoxGeo,true);
         GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>75');
         Tiles75 := GISdb[DEMIXtileDB].MyData.FiltRecsinDB;
         GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>50');
         Tiles50 := GISdb[DEMIXtileDB].MyData.FiltRecsinDB;
         GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>25');
         Tiles25 := GISdb[DEMIXtileDB].MyData.FiltRecsinDB;

         Results.Add(DEMGlb[WantedDEM].AreaName + ',' + IntToStr(Tiles75)  + ',' + IntToStr(Tiles50) + ',' + IntToStr(Tiles25) + ',' + RealToString(Lat,-12,-3) + ',' + RealToString(Long,-12,-3) + ',' +
            IntToStr(DEMGlb[WantedDEM].DEMHeader.RasterPixelIsGeoKey1025) + ',' + IntToStr(DEMGlb[WantedDEM].DEMHeader.VerticalCSTypeGeoKey));
         if (DEMGlb[WantedDEM].DEMHeader.RasterPixelIsGeoKey1025 = 0) or (DEMGlb[WantedDEM].DEMHeader.VerticalCSTypeGeoKey = 0) then begin
            File2Trash(fName);
            Result := false;
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Recycle due to DEM header issues: ' + fName); {$EndIf}
         end;
         CloseSingleDEM(WantedDEM);
      end;
   end;
end;


procedure CheckLowAltitudeDEMs(Dir : PathStr; Name : shortstring);
var
   FilesWanted,Results : tStringList;
   fName : PathStr;
   db : integer;
begin
   GetDEMIXpaths(false);
   FilesWanted := tStringList.Create;
   FindMatchingFiles(Dir,'*.tif',FilesWanted,1);
   DoAllFilesHaveSetEGMandPixelIs(FilesWanted,Results);
   FilesWanted.Free;
   fName := NextFileNumber(MDTempDir,Name + '_area_status_','.dbf');
   db := PetdbUtils.StringList2CSVtoDB(Results,fName);
   EndDEMIXProcessing;
end;



function TestDEMsAllPresent(AreaName : shortstring; var DEMs : integer; var Missing : shortstring) : boolean;
var
   FilesWanted : tStringList;
   fName : PathStr;
   Found : boolean;
   j,k : integer;
begin
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_test_dems,'*.tif',FilesWanted,1);
   RemoveFilesThatDoNotHaveString(FilesWanted,AreaName);
   DEMs := 0;
   Missing := '-';
   for k := 1 to NumDEMIXtestDEM do begin
      Found := false;
      for j := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[j];
         if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) and StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(DEMIXShort[k])) then begin
            inc(DEMs);
            Found := true;
         end;
      end;
      if not Found then Missing := Missing + DEMIXShort[k] + '-';
   end;
  Result := (DEMS = NumDEMIXtestDEM);
  if not Result then begin

  end;
  FilesWanted.Free;
  EndDEMIXProcessing;
end;


procedure CheckTestDEMs;
var
   ProblemAreas,FilesWanted,Areas,Results : tStringList;
   AreaName,Missing,Found : shortstring;
   fName,NewName,JustName : PathStr;
   i,j,k,DEMs,Refs : Integer;
begin
   GetDEMIXpaths(false);
   ProblemAreas := tStringList.Create;
   Areas := tStringList.Create;
   Areas.LoadFromFile(AreaListFName);
   Results := tStringList.Create;
   Results.Add('AREA,REF_DEMS,TEST_DEMs,MISS_DEM');
   for i := 0 to pred(Areas.Count) do begin
      AreaName := Areas.Strings[i];
      if TestDEMsAllPresent(AreaName,DEMs,Missing) then ProblemAreas.Add(AreaName);
      FilesWanted := tStringList.Create;
      FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',FilesWanted,1);
      RemoveFilesThatDoNotHaveString(FilesWanted,AreaName);
      Refs := FilesWanted.Count;
      FilesWanted.Free;
      Results.Add(AreaName + ',' + IntToStr(Refs) + ',' + IntToStr(DEMs) + ',' + Missing);
   end;
   fName := NextFileNumber(MDTempDir,'test_dem_status_','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   ProblemAreas.Free;
   Areas.Free;
   EndDEMIXProcessing;
end;



procedure VerifyTestDEMcoverages;
var
   Areas,Results : tStringList;
   AreaName : shortstring;
   fName : PathStr;
   i,j,k,DEMs : Integer;
   Lat,Long,Lat0,long0,distance,Bearing : float64;
begin
   GetDEMIXpaths(false);
   Areas := DEMIX_AreasWanted;
   Results := tStringList.Create;
   Results.Add('AREA,DEM,DISTANCE');
   for j := 0 to pred(Areas.Count) do begin
      AreaName := Areas.Strings[j];
      wmdem.SetPanelText(3,'Area ' + IntToStr(j) + '/' + IntToStr(Areas.Count) + '  ' + AreaName);
      if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,false) then begin
         DEMGlb[PointDEMs[0]].DEMCenterPoint(Lat0,Long0);
         for i := 1 to NumPtDEMs do begin
            DEMGlb[PointDEMs[i]].DEMCenterPoint(Lat,Long);
            VincentyCalculateDistanceBearing(Lat,Long,Lat0,Long0,Distance,Bearing);
            Results.Add(AreaName + ',' + DEMGlb[PointDEMs[i]].AreaName + ',' + RealToString(0.001 * Distance,-12,-4));
            if (Distance > 250) then DeleteFileIfExists(DEMGlb[PointDEMs[i]].DEMFileName);
         end;
         DEMGlb[AreaDEMs[0]].DEMCenterPoint(Lat0,Long0);
         for i := 1 to NumAreaDEMs do begin
            DEMGlb[AreaDEMs[i]].DEMCenterPoint(Lat,Long);
            VincentyCalculateDistanceBearing(Lat,Long,Lat0,Long0,Distance,Bearing);
            Results.Add(AreaName + ',' + DEMGlb[AreaDEMs[i]].AreaName + ',' + RealToString(0.001 * Distance,-12,-4));
            if (Distance > 250) then DeleteFileIfExists(DEMGlb[AreaDEMs[i]].DEMFileName);
         end;
         CloseAllDEMs;
      end;
   end;
   fName := NextFileNumber(MDTempDir,'test_dem_center_locations_','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   Areas.Free;
   EndDEMIXProcessing;
end;


procedure TrimReferenceDEMsToDEMIXtiles;
var
   Areas,Results : tStringList;
   AreaName : shortstring;
   fName : PathStr;
   i,j,k,DEMs,DEM : Integer;
   Lat,Long,Lat0,long0,distance,Bearing : float64;
begin
   GetDEMIXpaths(false);
   Areas := DEMIX_AreasWanted;
   for j := 0 to pred(Areas.Count) do begin
      AreaName := Areas.Strings[j];
      wmdem.SetPanelText(3,'Area ' + IntToStr(j) + '/' + IntToStr(Areas.Count) + '  ' + AreaName);
      LoadDEMIXReferenceDEMs(AreaName,DEMIXRefDEM,true);
      for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) then begin
            ClipTheDEMtoFullDEMIXTiles(i);
            DEMGlb[i].SaveAsGeotiff(DEMGlb[i].DEMFileName);
         end;
         DEM := i;
         CloseSingleDEM(DEM);
      end;
   end;
   Areas.Free;
   EndDEMIXProcessing;
end;


procedure PruneMisnamedReferenceDTMs;
var
   RefDEMfiles : tStringList;
   i,j : integer;
   fName : PathStr;


   function GetTiffFiles(aDir : PathStr; Ref : boolean = false) : tStringList;
   var
      i : integer;
      fName : PathStr;
   begin
      Result := tStringList.Create;
      FindMatchingFiles(aDir,'*.tif',Result);
      for i := pred(Result.Count) downto 0 do begin
         fName := Result.Strings[i];
         if StrUtils.ANSIcontainsText(fName,'_dsm') then begin
         end
         else begin
            if Ref and (not StrUtils.ANSIcontainsText(fName,'_dtm')) then begin
               DeleteFileIfExists(fName);
            end;
         end;
      end;
   end;


begin
   GetDEMIXPaths;
   ShowHourglassCursor;
   RefDEMfiles := GetTiffFiles(DEMIX_Ref_1sec,true);
   RefDEMfiles.Free;
   EndDEMIXProcessing;
end;


procedure MoveReferenceDSMs;
//segregates reference DSMs in their own directory
var
   RefDEMfiles : tStringList;
   i,j : integer;
   fName : PathStr;

   function GetTiffFiles(aDir : PathStr; Ref : boolean = false) : tStringList;
   var
      i : integer;
      fName : PathStr;
   begin
      Result := tStringList.Create;
      FindMatchingFiles(aDir,'*.tif',Result);
      for i := pred(Result.Count) downto 0 do begin
         fName := Result.Strings[i];
         if StrUtils.ANSIcontainsText(fName,'_dsm') then begin
            Petmar.MoveFile(fName,DEMIX_Ref_dsm_1sec + ExtractFileName(fName));
         end;
      end;
   end;


begin
   SafeMakeDir(DEMIX_Ref_dsm_1sec);
   GetDEMIXPaths;
   ShowHourglassCursor;
   RefDEMfiles := GetTiffFiles(DEMIX_Ref_1sec,true);
   RefDEMfiles.Free;
   EndDEMIXProcessing;
end;






