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

