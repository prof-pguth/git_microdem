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
         Findings.Add(Countries[i] + ',' + IntToStr(GISdb[db].NumUniqueEntriesInDBLinkPossible('AREA')) + ',' + IntToStr(GISdb[db].NumUniqueEntriesInDBLinkPossible('DEMIX_TILE')));
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


procedure InventoryCriteriaEachDEMIXtile(DB : integer);
var
   Tiles,Results,Criteria : tStringList;
   Tile,Criterion,Aline,IsGood : shortstring;
   i,j,TileIndex,CriterionIndex : integer;
   fName : PathStr;
   Stats : array[0..MaxTiles,0..MaxCriteria] of integer;
begin
   if GISdb[db].MyData.FieldExists('DEMIX_TILE') and GISdb[db].MyData.FieldExists('CRITERION') then begin
      GISdb[db].ClearGISFilter;
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
      aline := 'DEMIX_TILE,AREA,TOTAL';
      for I := 1 to Criteria.Count do begin
         aline := aline + ',' + Criteria.Strings[pred(i)];
      end;
      aLine := aline + ',' + 'RESULTS';
      Results.Add(aline);
      for I := 0 to pred(Tiles.Count) do begin
         GISdb[db].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tiles.Strings[i]));
         aline := Tiles.Strings[i] + ',' + GISdb[db].MyData.GetFieldByNameAsString('AREA') + ',' + IntToStr(Stats[i,0]);
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
      GISdb[db].ClearGISFilter;
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
      end;
      ClearOutFolder(Diff_dist_results_dir);
      ClearOutFolder(DEMIX_diff_dist);

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


procedure CriteriaInFUVdb(db : integer);
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
      wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count),true);
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
   fName := NextFileNumber(MDTempDir,'criteria-fuv_inventory','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   GISdb[db].ClearGISFilter;
   GISdb[db].EmpSource.Enabled := true;
   SetColorForWaiting;
   wmdem.SetPanelText(1,'',true);
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


procedure TrimReferenceDEMsToDEMIXtiles;
var
   Areas{,Results} : tStringList;
   AreaName : shortstring;
   //fName : PathStr;
   i,j,{k,DEMs,}DEM : Integer;
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






