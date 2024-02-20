{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{ include file for demix_definitions }
{____________________________________}



procedure InventoryChannelDataByArea;
var
   Areas,Results : tStringList;
   AreaName : shortstring;
   i : integer;
   fName : PathStr;
   FileList : array[1..7] of tStringList;

   function CountOfFiles(ID : integer; Area : shortString) : integer;
   var
      j : integer;
      fName : PathStr;
   begin
      Result := 0;
      for j := 0 to pred(FileList[ID].Count) do begin
         fName := UpperCase(FileList[ID].Strings[j]);
         if (StrUtils.AnsiContainsText(fName,UpperCase(Area) )) then inc(Result);
      end;
   end;

   procedure LoadFiles(ID : integer; Dir : PathStr; Ext : shortString);
   begin
      FileList[ID] := Nil;
      FindMatchingFiles(Dir,Ext,FileList[ID]);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile(Dir + '  ' + Ext + '  files=' + IntToStr(FileList[ID].Count)); {$EndIf}
   end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('InventoryChannelDataByArea in'); {$EndIf}
   GetDEMIXPaths;
   Areas := tStringList.Create;
   Areas.LoadFromFile(DEMIXSettingsDir + 'areas_list.txt');

   Results := tStringList.Create;
   Results.Add('AREA,REF_NOS,TEST_NOS,REF_SHP,TEST_SHP,REF_GRID,TEST_GRID,MISS_CSV');
   LoadFiles(1,DEMIX_ref_DEMs_no_sink,'*.tif');
   LoadFiles(2,DEMIX_test_DEMs_no_sink,'*.tif');
   LoadFiles(3,DEMIX_ref_DEMs_channels,'*.shp');
   LoadFiles(4,DEMIX_test_DEMs_channels,'*.shp');
   LoadFiles(5,DEMIX_ref_DEMs_channel_grids,'*.tif');
   LoadFiles(6,DEMIX_test_DEMs_channel_grids,'*.tif');
   LoadFiles(7,ChannelMissesDir,'*.csv');
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('InventoryChannelDataByArea loaded'); {$EndIf}

   for i := 0 to pred(Areas.Count) do begin
      AreaName := Areas.Strings[i];
      Results.Add(AreaName + ',' + IntToStr(CountOfFiles(1,AreaName)) + ',' +
                                   IntToStr(CountOfFiles(2,AreaName)) + ',' +
                                   IntToStr(CountOfFiles(3,AreaName)) + ',' +
                                   IntToStr(CountOfFiles(4,AreaName)) + ',' +
                                   IntToStr(CountOfFiles(5,AreaName)) + ',' +
                                   IntToStr(CountOfFiles(6,AreaName)) + ',' +
                                   IntToStr(CountOfFiles(7,AreaName)));

   end;


   Areas.Destroy;
   fName := NextFileNumber(MDTempDir,'status_channel_files_','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('InventoryChannelDataByArea out'); {$EndIf}
end;

procedure InventoryListRefereneDTMbyArea;
var
   TheFiles,Results : tStringList;
   i : integer;
   fName : PathStr;
begin
   TheFiles := Nil;
   FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',TheFiles);
   Results := tStringList.Create;
   Results.Sorted := true;
   for i := 0 to pred(TheFiles.Count) do begin
      fName := theFiles.Strings[i];
      if (StrUtils.AnsiContainsText(fName,'_point')) and (not StrUtils.AnsiContainsText(fName,'_dsm'))then begin
         fName := AreaNameFromRefDEMName(fName);
         fName := StringReplace(fName,'_dtm','',[rfReplaceAll, rfIgnoreCase]);
         Results.Add(fName);
      end;
   end;
   DisplayAndPurgeStringList(Results,'DEMIX test areas with reference DTM (point; n=' + IntToStr(Results.Count) + ')');
end;


procedure InventoryDEMIXdifferenceStats;
var
   TheFiles,Results,Areas : tStringList;
   i,j,n : integer;
   fName : PathStr;
begin
   Areas := DEMIX_AreasWanted(true);
   n := Areas.Count;
   TheFiles := Nil;
   FindMatchingFiles(DEMIXresultsDir,'*.csv',TheFiles);
   Results := tStringList.Create;
   //Results.Sorted := true;
   for j := pred(Areas.Count) downto 0 do begin
      for i := 0 to pred(TheFiles.Count) do begin
         fName := UpperCase(ExtractFileNameNoExt(TheFiles.Strings[i]));
         if StrUtils.AnsiContainsText(fName,UpperCase(Areas.Strings[j])) then begin
            Areas.Delete(j);
            break
         end;
      end;
   end;
   if (Results.Count = 0) then MessageToContinue('Difference stats done for all areas, n=' + IntToStr (N))
   else DisplayAndPurgeStringList(Results,'Areas Missing DEMIX difference stats (n=' + IntToStr(Results.Count) + ')');
   TheFiles.Free;
end;


procedure Inventory3DEPtiles;
//table with the number of tiffs in four categories for the source 3DEP
//can show if processing is incomplete, or if there are broken or damaged files
var
   FilesWanted,Results,Paths : tStringList;
   AreaName,DirStatus : shortstring;
   fName,NewName,JustName : PathStr;
   Num1m,NumEGM,Num1secArea,Num1secPoint,
   i,j,DTMs,DSMs : Integer;
begin
   try
      GetDEMIXPaths;
      ReportErrors := false;
      LockStatusBar := true;
      Results := tStringList.Create;
      Results.Add('AREA,TILES_1M,TILES_EGM,AREA_1SEC,POINT_1SEC');

         {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs getting data paths'); {$EndIf}
         Get3DEPDataDirs(Paths);
         {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs paths=' + IntToStr(Paths.Count)); {$EndIf}
         for I := 0 to pred(Paths.Count) do begin
            Num1m := 0;
            NumEGM := 0;
            Num1secArea := 0;
            Num1secPoint := 0;
            AreaName := LastSubDir(Paths.Strings[i]);
            FilesWanted := tStringList.Create;
            FindMatchingFiles(Paths[i],'*.tif',FilesWanted,1);
            for j := 0 to pred(FilesWanted.Count) do begin
                fName := ExtractFileNameNoExt(FilesWanted.Strings[j]);
                JustName := UpperCase(fName);
                if StrUtils.AnsiContainsText(JustName,'REF_1SEC_POINT') then inc(Num1secPoint)
                else if StrUtils.AnsiContainsText(JustName,'REF_1SEC_AREA') then inc(Num1secArea)
                else if StrUtils.AnsiContainsText(JustName,'EGM2008') then inc(NumEGM)
                else inc(Num1m);
            end;
            FilesWanted.Free;
            Results.Add(AreaName + ',' + IntToStr(Num1m) + ',' + IntToStr(NumEGM) + ',' + IntToStr(Num1secArea) + ',' + IntToStr(Num1secPoint));
         end;
         Paths.Free;
      fName := NextFileNumber(MDTempDir,'status_source_3DEP_','.dbf');
      PetdbUtils.StringList2CSVtoDB(Results,fName);
   finally
      EndDEMIXProcessing;
   end;
end;


procedure InventoryDBwithDSMandDTMbyArea;
var
   FilesWanted,Areas,Results : tStringList;
   AreaName : shortstring;
   fName,NewName,JustName : PathStr;
   i,j,DTMs,DSMs : Integer;
begin
   GetDEMIXpaths(false);
   Areas := tStringList.Create;
   Areas.LoadFromFile(DEMIXSettingsDir + 'areas_list.txt');
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
   fName := NextFileNumber(MDTempDir,'ref_dem_status_','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   EndDEMIXProcessing;
end;


function DoAllFilesHaveSetEGMandPixelIs(FilesWanted : tStringList; Results : tstringList = nil) : boolean;
var
   j,WantedDEM,DEMIXtileDB : Integer;
   fName : PathStr;
   Lat,Long : float64;
begin
   Result := true;
   for j := 0 to pred(FilesWanted.Count) do begin
      fName := FilesWanted.Strings[j];
      wmdem.SetPanelText(2, 'File: ' + IntToStr(succ(j)) + '/' + IntToStr(FilesWanted.Count) + '  ' + ExtractFileNameNoExt(fName));
      if NewArea(true,WantedDEM,'',FName,WantedDEM) and ValidDEM(WantedDEM) then begin
         if (Results <> nil) then begin
            DEMGlb[WantedDEM].DEMCenterPoint(Lat,Long);
            DEMIXtileDB := DEMIXtileFill(WantedDEM,DEMGlb[WantedDEM].DEMBoundBoxGeo,true);
            GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>' + IntToStr(MDDef.DEMIX_Full));
            Results.Add(DEMGlb[WantedDEM].AreaName + ',' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsinDB) + ',' + RealToString(Lat,-12,-3) + ',' + RealToString(Long,-12,-3) + ',' +
               IntToStr(DEMGlb[WantedDEM].DEMHeader.RasterPixelIsGeoKey1025) + ',' + IntToStr(DEMGlb[WantedDEM].DEMHeader.VerticalCSTypeGeoKey));
         end;
         if (DEMGlb[WantedDEM].DEMHeader.RasterPixelIsGeoKey1025 = 0) or (DEMGlb[WantedDEM].DEMHeader.VerticalCSTypeGeoKey = 0) then begin
            File2Trash(fName);
            Result := false;
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Recycle due to DEM header issues: ' + fName); {$EndIf}
         end;
      end;
   end;
end;



procedure CheckDiluviumAreas;
var
   FilesWanted,Results : tStringList;
   fName : PathStr;
begin
   GetDEMIXpaths(false);
   Results := tStringList.Create;
   Results.Add('AREA_DEM,DEMIX_TILES,LAT_CENT,LONG_CENT,PIXEL_IS,VERT_DATUM');
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_diluvium_dems,'*.tif',FilesWanted,1);
   FilesWanted.Sort;
   DoAllFilesHaveSetEGMandPixelIs(FilesWanted,Results);
   FilesWanted.Free;
   fName := NextFileNumber(MDTempDir,'diluvium_area_status_','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   EndDEMIXProcessing;
end;



procedure CheckReferenceDEMsAreEGMandPixelIs;
var
   FilesWanted,Results : tStringList;
   fName : PathStr;
begin
   GetDEMIXpaths(false);
   Results := tStringList.Create;
   Results.Add('AREA_DEM,DEMIX_TILES,LAT_CENT,LONG_CENT,PIXEL_IS,VERT_DATUM');
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',FilesWanted,1);
   FilesWanted.Sort;
   RemoveFilesThatDoHaveString(FilesWanted,'_dsm_');
   RemoveFilesThatDoHaveString(FilesWanted,'_area');
   DoAllFilesHaveSetEGMandPixelIs(FilesWanted,Results);
   FilesWanted.Free;
   fName := NextFileNumber(MDTempDir,'ref_dem_status_','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   //ReallyReadDEM := true;
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
   for k := 1 to NumDemixDEM do begin
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
  Result := (DEMS = RequiredTestDEMs);
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
   Areas.LoadFromFile(DEMIXSettingsDir + 'areas_list.txt');
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
   (*
   if (ProblemAreas.Count > 0) then begin
      if AnswerIsYes('Create test area DEMs for those not complete') then begin
         for I := 0 to pred(ProblemAreas.Count) do begin
            AreaName := ProblemAreas.Strings[i];
            //LoadDEMsForCurrentArea(AreaName,true,false);
         end;
      end;
   end;
   *)
   ProblemAreas.Free;
   Areas.Free;
   EndDEMIXProcessing;
end;


procedure DEMIXTileSummary(DBonTable : integer);
var
   DEMsPresent,LandTypesPresent,Report,CriteriaPresent : tStringList;
   j,k : integer;
   aLine : shortstring;
   fName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;

   DEMsPresent := tStringList.Create;
   DEMsPresent.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');

   GISdb[DBonTable].EmpSource.Enabled := false;
   LandTypesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   GISdb[DBonTable].EmpSource.Enabled := false;
   CriteriaPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
   {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('Land_Types present=' + IntToStr(LandTypesPresent.Count) + '  Criteria present=' + IntToStr(CriteriaPresent.Count)); {$EndIf}

   Report := tStringList.Create;
   aline := 'REF_TYPE,AREA';
   for j := 0 to pred(LandTypesPresent.Count) do aLine := aLine + ',' + LandTypesPresent.strings[j];
   Report.Add(Aline);

   {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}

   for j := 2 downto 1 do begin
      aline := RefDEMType[j];
      GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefDEMType[j]));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA'));
      GISdb[DBonTable].EmpSource.Enabled := false;
      for k := 0 to pred(LandTypesPresent.Count) do begin
         wmdem.SetPanelText(3,'DEM type=' + RefDEMType[j] + '  land type=' + LandTypesPresent[k]);
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefDEMType[j]) + ' AND LAND_TYPE=' + QuotedStr(LandTypesPresent[k]));
         aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB div CriteriaPresent.Count);
      end;
      {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}
      Report.Add(aline);
   end;

   DEMsPresent.Free;
   LandTypesPresent.Free;
   CriteriaPresent.Free;
   fName := NextFileNumber(MDTempDir,'demix_tile_summary_','.dbf');
   StringList2CSVtoDB(Report,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   GISdb[DBonTable].ShowStatus;
   wmdem.SetPanelText(3,'');
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary out'); {$EndIf}
end {DEMIXTileSummary};


procedure DEMIXtile_inventory(DBonTable : integer);
var
   TileList,Findings,DEMs : tStringList;
   TStr : shortstring;
   fName : PathStr;
   i : Integer;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
   Findings := tStringList.Create;
   Findings.Add('COUNTRY,AREA,DEMIX_TILE,DEMS');
   StartProgress('Inventory tiles');
   for i := 0 to pred(TileList.Count) do begin
      wmdem.SetPanelText(3,'Tile ' + IntToStr(i) + '/' + IntToStr(TileList.Count));
      GISdb[DBonTable].EmpSource.Enabled := false;
      if (i mod 10 = 0) then UpdateProgressBar(i/TileList.Count);
      GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(TileList.Strings[i]),false);
      DEMs := GISdb[DBonTable].MyData.UniqueEntriesInDB('REF_TYPE');
      TStr := DEMs[0];
      if (DEMs.Count > 1) then TStr := TStr + ' + ' +  DEMs[1];
      Findings.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('COUNTRY') + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA') + ',' +  TileList.Strings[i] + ',' + TStr);
   end;
   GISdb[DBonTable].ClearGISFilter;
   fName := NextFileNumber(MDTempDir,'DEMIX_tile_inventory','.dbf');
   StringList2CSVtoDB(Findings,fName);
   GISdb[DBonTable].ShowStatus;
   wmdem.SetPanelText(3,'');
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
   Areas := DEMIX_AreasWanted(true);
   Results := tStringList.Create;
   Results.Add('AREA,DEM,DISTANCE');
   for j := 0 to pred(Areas.Count) do begin
      AreaName := Areas.Strings[j];
      wmdem.SetPanelText(3,'Area ' + IntToStr(j) + '/' + IntToStr(Areas.Count) + '  ' + AreaName);
      if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,false) then begin
         DEMGlb[PointDEMs[0]].DEMCenterPoint(Lat0,Long0);
         for i := 1 to NumPt do begin
            DEMGlb[PointDEMs[i]].DEMCenterPoint(Lat,Long);
            VincentyCalculateDistanceBearing(Lat,Long,Lat0,Long0,Distance,Bearing);
            Results.Add(AreaName + ',' + DEMGlb[PointDEMs[i]].AreaName + ',' + RealToString(0.001 * Distance,-12,-4));
            if (Distance > 250) then DeleteFileIfExists(DEMGlb[PointDEMs[i]].DEMFileName);
         end;
         DEMGlb[AreaDEMs[0]].DEMCenterPoint(Lat0,Long0);
         for i := 1 to NumArea do begin
            DEMGlb[AreaDEMs[i]].DEMCenterPoint(Lat,Long);
            VincentyCalculateDistanceBearing(Lat,Long,Lat0,Long0,Distance,Bearing);
            Results.Add(AreaName + ',' + DEMGlb[AreaDEMs[i]].AreaName + ',' + RealToString(0.001 * Distance,-12,-4));
            if (Distance > 250) then DeleteFileIfExists(DEMGlb[AreaDEMs[i]].DEMFileName);
         end;
         CloseAllDEMs;
      end
      else begin
         //MessageToContinue('DEMs missing: ' + AreaName);
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
   Areas := DEMIX_AreasWanted(true);
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


procedure VerifyAllMapsReadyForSSIM;
const
   NCrits = 6;
   Crits : array[1..NCrits] of shortstring = ('ELEV_','RRI_','SLOPE_','HILL_','RUFF_','TPI_');
   NDEMs = 9;
   DEMs : array[1..NDEMs] of shortstring = ('COP','FABDEM','NASA','SRTM','ASTER','ALOS','DILUV','dtm_ref_area','dtm_ref_point');
var
   Dirs,GoodTiles,BadTiles,TheFiles : tStringList;
   i,j,k : Integer;
   fName,fName2 : PathStr;
   Error,Tile : shortstring;
begin
   Dirs := tStringList.Create;
   GoodTiles := tStringList.Create;
   BadTiles := tStringList.Create;
   fName2 := 'C:\temp\ssim_global_norm\';
   Dirs := GetSubDirsInDirectory(fName2);
   StartProgress('Verify');
   for i := 0 to pred(Dirs.Count) do begin
      UpdateProgressBar(i/Dirs.Count);
      Tile := Dirs[i];
      if (Tile[1] = 'N') or (Tile[1] = 'S') then begin //in case other folders selected
         Error := '';
         TheFiles := Nil;
         FindMatchingFiles(fName2 + Tile,'*.tif',TheFiles);
         for k := 1 to NCrits do begin
            for j := 1 to NDEMs do begin
               fName := Crits[k] + DEMs[j] + '_norm.tif';
               if TheFiles.IndexOf(fName2 + Tile + '\' + fName) < 0 then begin
                  if (Error = '') then Error := 'files=' + IntToStr(TheFiles.Count) + '  ';
                  Error := Error + fName + '  ';
               end;
            end;
         end;
         TheFiles.Free;
         if (Error = '') then GoodTiles.Add(Tile)
         else begin
            BadTiles.Add(Tile + '  ' + Error);
            WriteLineToDebugFile(Tile + '  ' + Error);
         end;
      end;
   end;
   EndProgress;
   Dirs.Free;
   DisplayAndPurgeStringList(GoodTiles,'Good tiles=' + IntToStr(GoodTiles.Count));
   DisplayAndPurgeStringList(BadTiles,'Problem tiles=' + IntToStr(BadTiles.Count));
end;



