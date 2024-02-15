{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{    include file for demix_control  }
{____________________________________}


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


procedure CheckReferenceDEMs;
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

            //when sure all the file names are harmonized, this can be removed
            if (not StrUtils.AnsiContainsText(UpperCase(JustName),'REF')) then begin
               NewName := '';
               if StrUtils.AnsiContainsText(UpperCase(JustName),'1sec') then NewName := StringReplace(JustName, '1sec', 'ref_1sec',[rfIgnoreCase]);
               if StrUtils.AnsiContainsText(UpperCase(JustName),'1.5') then NewName := StringReplace(JustName, '1.5', 'ref_1.5',[rfIgnoreCase]);
               if (NewName <> '') then begin
                  NewName := ExtractFilePath(fName) + NewName;
                  RenameFile(fName,NewName);
                  fName := NewName;
               end;
            end;

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
   FindMatchingFiles(DEMIX_test_dems,'*.dem',FilesWanted,1);
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
   i,j,k,DEMs : Integer;
begin
   GetDEMIXpaths(false);
   ProblemAreas := tStringList.Create;
   Areas := tStringList.Create;
   Areas.LoadFromFile(DEMIXSettingsDir + 'areas_list.txt');
   Results := tStringList.Create;
   Results.Add('AREA,TEST_DEMs,MISS_DEM');
   for i := 0 to pred(Areas.Count) do begin
      AreaName := Areas.Strings[i];
      if TestDEMsAllPresent(AreaName,DEMs,Missing) then ProblemAreas.Add(AreaName);
      Results.Add(AreaName + ',' + IntToStr(DEMs) + ',' + Missing);
(*
      DEMs := 0;
      Found := '';
      for j := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[j];
         if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) then begin
            inc(DEMs);
            Found := Found + UpperCase(ExtractFileNameNoExt(fName));
         end;
      end;
      Missing := '-';
      if (DEMs <> RequiredTestDEMs) then begin
         for k := 1 to NumDemixDEM do begin
            if not StrUtils.AnsiContainsText(Found,UpperCase(DEMIXShort[k])) then Missing := Missing + DEMIXShort[k] + '-';
         end;
         ProblemAreas.Add(AreaName);
      end;

      if True then
  *)
      Results.Add(AreaName + ',' + IntToStr(DEMs) + ',' + Missing);
   end;
   //FilesWanted.Free;

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
   i,j,k,DEMs : Integer;
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
         CloseSingleDEM(i);
      end;
   end;
   Areas.Free;
   EndDEMIXProcessing;
end;


