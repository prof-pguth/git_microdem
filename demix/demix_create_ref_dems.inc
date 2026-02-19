{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{  include file for demix_definitions    }
{________________________________________}


procedure CreateLandCoverGrids;
var
   Areas : tStringList;
   DEM,grid,j : integer;
   fName,GridName : PathStr;
   Area : shortstring;
   bb : sfBoundBox;
begin
   try
      GetDEMIXPaths;
      LockStatusBar := true;
      Areas := DEMIX_AreasWanted;
      for j := 0 to pred(Areas.Count) do begin
         Area := Areas.Strings[j];
         wmdem.SetPanelText(3,'Land cover grids ' + IntToStr(succ(j)) + '/  ' + IntToStr(Areas.Count) + '  ' + Area,true);
         fName := DEMIX_Ref_1sec + Area + '_dtm' + Ref1SecPointStr + '.tif';
         GridName := DEMIX_area_lc100 + Area + '.tif';
         if FileExists(fName) then begin
            DEM := OpenNewDEM(fName,false);
            bb := DEMGlb[DEM].DEMBoundBoxGeo;
            Grid := LoadLC100LandCover(GridName,bb,false);
         end
         else begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Ref DTM missing=' + fName); {$EndIf}
         end;
         CloseAllDEMs;
      end;
  finally
     EndDEMIXProcessing
  end;
end;


procedure Get3DEPDataDirs(var DataDirs : tStringList);
begin
   DataDirs := tStringList.Create;
   //DataDirs.Add(DEMIX_3DEP_Dir);
   GetMultipleDirectories('3DEP DEMs', DataDirs);
end;


procedure MaskWaterInReferenceAndTestDEMs;
var
   Areas : tStringList;
   i,j,lc : integer;
   Fixed1,Fixed2 : int64;
   AreaName : shortstring;
begin
   try
      GetDEMIXPaths;
      Areas := DEMIX_AreasWanted;
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         wmdem.SetPanelText(3,'Load: ' + IntToStr(i) + '/' + IntToStr(Areas.Count) + '  ' + AreaName,true);
         OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,false);
         lc := LoadLandcoverForDEMIXarea(AreaName,false);
         if ValidDEM(lc) then begin
            wmdem.SetPanelText(3,'Mask: ' + IntToStr(i) + '/' + IntToStr(Areas.Count) + '  ' + AreaName,true);
            DEMglb[lc].MarkInRangeMissing(79.99,80.01,Fixed1,false);  //80 is the open water value
            DEMglb[lc].MarkInRangeMissing(199.99,200.01,Fixed2,false);  //200 is the ocean value
            if (Fixed1 > 0) or (Fixed2 > 0) then begin
               for j := dmxFirstPoint to NumPtDEMs do begin
                  MaskGridFromSecondGrid(PointDEMs[j],lc,msSecondMissing);
                  DEMglb[PointDEMs[j]].CheckMaxMinElev;
                  DEMglb[PointDEMs[j]].SaveAsGeotiff(DEMglb[PointDEMs[j]].DEMFileName);
               end;

               for j := dmxFirstArea to NumAreaDEMs do begin
                  MaskGridFromSecondGrid(AreaDEMs[j],lc,msSecondMissing);
                  DEMglb[AreaDEMs[j]].CheckMaxMinElev;
                  DEMglb[AreaDEMs[j]].SaveAsGeotiff(DEMglb[AreaDEMs[j]].DEMFileName);
               end;
            end;
         end
         else begin
            {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('LC missing for ' + AreaName); {$EndIf}
         end;
      end;
      CloseAllDEMs;
   finally
      EndDEMIXProcessing;
   end;
end;




procedure ShiftDEMsto_UTM_WGS84_EGM2008;
var
   AreaProjectionInfo : tMyData;
var
   VertCode,HorizCode,UTMzone : shortstring;
   Files : tStringList;
   i,j : integer;
   AreaName,TStr : ShortString;
   LocalList : boolean;
   DirName,fName : PathStr;
begin {ShiftDEMsto_UTM_WGS84_EGM2008}
   try
      GetDEMIXPaths;
      LocalList := (DataDirs = nil);
      if LocalList then Get3DEPDataDirs(DataDirs);
      AreaProjectionInfo := tMyData.Create(DEMIX_area_dbName);
      ShowHourglassCursor;
      {$If Defined(Record3DEPX)} HighlightLineToDebugFile('ShiftDEMsto_UTM_WGS84_EGM2008 paths=' + IntToStr(DataDirs.Count)); {$EndIf}
      for I := 0 to pred(DataDirs.Count) do begin
         DirName := DataDirs.Strings[i];
         AreaName := LastSubDir(ExtractFilePath(DirName));
         TStr := 'Datum shift, dir: ' + IntToStr(succ(i)) + '/' + IntToStr(DataDirs.Count) + ' ' + AreaName;
         wmdem.SetPanelText(3,TStr,true);
         AreaProjectionInfo.ApplyFilter('AREA=' + QuotedStr(AreaName));
         if (AreaProjectionInfo.FiltRecsInDB = 1) then begin
            {$If Defined(Record3DEPX)} WriteLineToDebugFile('Shirt ' + TStr); {$EndIf}
            VertCode := AreaProjectionInfo.GetFieldByNameAsString('VERT_CS');
            HorizCode := AreaProjectionInfo.GetFieldByNameAsString('HORIZ_CS');
            UTMZone := AreaProjectionInfo.GetFieldByNameAsString('UTM_ZONE');
            if length(UTMzone) = 1 then UTMZone := '0' + UTMZone;

            if (VertCode = '') then begin
               MessageToContinue('Area ' + AreaName + ' missing VERT_CS in ' + DEMIX_area_dbName);
            end
            else begin
               Files := Nil;
               Petmar.FindMatchingFiles(DirName,'*.tif',Files,0);
               RemoveFilesThatDoHaveString(Files,'EGM2008');  //cannot use DEMs with datum shift already, which is what we are creating
               RemoveFilesThatDoHaveString(Files,'REF_1sec_');  //cannot use DEMs already reference, which is what we are creating

               for j := 0 to pred(Files.Count) do begin
                   fName := Files.Strings[j];
                   wmdem.SetPanelText(2, 'File: ' + IntToStr(succ(j)) + '/' + IntToStr(Files.Count),true);
                   ShiftAFile_UTM_WGS84_EGM2008(Fname,ChangeFileExt(fName,'_egm2008.tif'),VertCode,HorizCode,UTMzone);
               end;
               Files.Free;
            end;
         end
         else begin
            MessageToContinue('Area ' + AreaName + ' missing in ' + DEMIX_area_dbName);
         end;
      end;
   finally
      AreaProjectionInfo.Destroy;
      if LocalList then DataDirs.Free;
      EndDEMIXProcessing;
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('ShiftDEMsto_UTM_WGS84_EGM2008 out'); {$EndIf}
   end;
end {ShiftDEMsto_UTM_WGS84_EGM2008};



procedure DEMIX_MergeReferenceDEMs(Overwrite : boolean; DataDirs : tStringList = Nil);
const
   ShowMapWhileMerging = false;   //help track down problematic data sets
var
   DirStatus : shortstring;
   i : integer;
   OutPath,adir : PathStr;
   LocalList : boolean;
   AreaName : shortstring;


         procedure OnePixelIs(di : integer; which : shortstring);
         var
            DoneName : PathStr;
            Files : tStringList;
            i,DEM : integer;
         begin
            DoneName := Outpath + AreaName + '_dtm' + which + '.tif';
            if Overwrite or (not FileExists(DoneName)) then begin
               DirStatus := 'Merge Ref DEMs: ' + IntToStr(succ(di)) + '/' + IntToStr(DataDirs.Count) + '  ' + AreaName;
               wmdem.SetPanelText(3,DirStatus,true);
               wmdem.SetPanelText(2,Which,true);
               Files := nil;
               Petmar.FindMatchingFiles(aDir, '*.tif',Files,0);
               {$If Defined(Record3DEPX)} WriteLineToDebugFile('OnePixelIs ' + aDir + ' Found DEMs=' + IntToStr(Files.Count)); {$EndIf}
               RemoveFilesThatDoNotHaveString(Files,which);  //can only use DEMs with aggregated to 1 arc sec, for correct pixel-is
               {$If Defined(Record3DEPX)} WriteLineToDebugFile(Which + ' aggregated ref DEMs=' + IntToStr(Files.Count)); {$EndIf}
               if (Files.Count = 0) then begin
                  Files.Destroy;
               end
               else begin
                  DEM := MergeMultipleDEMsHere(Files,true,ShowMapWhileMerging);
                  {$If Defined(Record3DEPX)} WriteLineToDebugFile('Merged for ' + DoneName); {$EndIf}
                  if ShowMapWhileMerging then MessageToContinue('Merged');
                  if ClipTheDEMtoFullDEMIXTiles(DEM,DoneName) then begin
                     {$If Defined(Record3DEPX)} WriteLineToDebugFile('Clipped'); {$EndIf}
                     if ShowMapWhileMerging then MessageToContinue('Clipped to DEM tiles');
                     (*
                     MarkWaterMissingInThisDEM(DEM);
                     {$If Defined(Record3DEPX)} WriteLineToDebugFile('Water masked'); {$EndIf}
                     *)
                     if ShowMapWhileMerging then MessageToContinue('Mask water');
                     DEMGlb[DEM].SavePartOfDEMWithDataGeotiff(DoneName);
                     {$If Defined(Record3DEPX)} WriteLineToDebugFile('Saved, ' + DoneName); {$EndIf}
                  end
                  else begin
                     {$If Defined(RecordProblems)} HighLightLineToDebugFile('No filled DEMIX tiles in ' + AreaName); {$EndIf}
                     DEMGlb[DEM].SavePartOfDEMWithDataGeotiff(DoneName);
                  end;
                  CloseAllDEMs;
               end;
            end;
         end;


begin {DEMIX_MergeReferenceDEMs}
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_MergeReferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_MergeReferenceDEMs getting data paths'); {$EndIf}
      LocalList := (DataDirs = nil);
      if LocalList then begin
         Get3DEPDataDirs(DataDirs);
      end;
         {$If Defined(Record3DEPX)} HighlightLineToDebugFile('DEMIX_MergeReferenceDEMs paths=' + IntToStr(DataDirs.Count)); {$EndIf}
         for I := 0 to pred(DataDirs.Count) do begin
            AreaName := LastSubDir(DataDirs.Strings[i]);
            aDir := DataDirs.Strings[i];
            OutPath := DEMIX_Ref_1sec;
            OnePixelIs(i,Ref1SecPointStr);
            OnePixelIs(i,Ref1SecAreaStr);
            OnePixelIs(i,Ref1_5SecPointStr);
         end;
      if LocalList then DataDirs.Free;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_MergeReferenceDEMs out'); {$EndIf}
end {DEMIX_MergeReferenceDEMs};


procedure DEMIX_CreateReferenceDEMsFromSource(Overwrite : boolean; DataDirs : tStringList = Nil);
var
   DirStatus : shortstring;
   i : integer;
   LocalList : boolean;


   procedure MakeRefDEMsForOneDir(aDir : PathStr);
   var
      fName,fName2,fName3,DoneName : PathStr;
      Files : tStringList;
      i,WantedDEM : integer;
      AreaName : ShortString;
   begin
      Files := nil;
      ShowHourglassCursor;
      Petmar.FindMatchingFiles(aDir,'*.tif',Files,0);
      {$If Defined(Record3DEPX)} HighlightLineToDebugFile('MakeRefDEMsForOneDir ' + aDir + ' Found TIFs=' + IntToStr(Files.Count)); {$EndIf}
      RemoveFilesThatDoNotHaveString(Files,'EGM2008');  //can only use DEMs with datum shift(*

      {$If Defined(Record3DEPX)} WriteLineToDebugFile('MakeRefDEMsForOneDir Found EGM2008 TIFs=' + IntToStr(Files.Count)); {$EndIf}
      RemoveFilesThatDoHaveString(Files,Ref1SecPointStr); //cannot use DEMs already reference
      RemoveFilesThatDoHaveString(Files,Ref1SecAreaStr); //cannot use DEMs already reference
      RemoveFilesThatDoHaveString(Files,Ref1_5SecPointStr); //cannot use DEMs already reference
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('MakeRefDEMsForOneDir Found EGM2008 TIFs, REF not done yet=' + IntToStr(Files.Count)); {$EndIf}
      for i := 0 to pred(Files.Count) do begin
          fName := Files.Strings[i];
          AreaName := LastSubDir(ExtractFilePath(fName));
          fName2 := ChangeFileExt(fName,Ref1SecPointStr + '.tif');
          fName3 := ChangeFileExt(fName, Ref1SecAreaStr + '.tif');
          if FileExists(fName3) and FileExists(fName2) and (not Overwrite) then begin
          end
          else begin
             {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('MakeRefDEMsForOneDir file=' + fName ); {$EndIf}
             WantedDEM := OpenNewDEM(fName,false);
             if ValidDEM(WantedDEM) then begin
                wmdem.SetPanelText(2, 'DEM: ' + IntToStr(succ(i)) + '/' + IntToStr(Files.Count),true);
                ResampleForDEMIXOneSecDEMs(Overwrite,true,WantedDEM,false,aDir,ResampleModeOneSec);
             end;
          end;
          wmdem.SetPanelText(2, '',true);
      end;
      CleanUpTempDirectory(True);
   end;


begin {procedure DEMIX_Create3DEPReferenceDEMs}
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      LockStatusBar := true;
      LocalList := (DataDirs = Nil);
      if LocalList then begin
         {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs getting data paths'); {$EndIf}
         Get3DEPDataDirs(DataDirs);
      end;
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs paths=' + IntToStr(DataDirs.Count)); {$EndIf}
      for I := 0 to pred(DataDirs.Count) do begin
         DirStatus := 'Ref DEM aggregation Area: ' + IntToStr(succ(i)) + '/' + IntToStr(DataDirs.Count) + '  ' + LastSubDir(DataDirs.Strings[i]);
         wmdem.SetPanelText(3,DirStatus,true);
         {$If Defined(Record3DEPX)} WriteLineToDebugFile(DirStatus); {$EndIf}
         MakeRefDEMsForOneDir(DataDirs.Strings[i]);
       end;
      if LocalList then DataDirs.Free;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs out'); {$EndIf}
end {procedure DEMIX_Create3DEPReferenceDEMs};



procedure DEMIX_GDAL_Ref_DEM_datum_shift(Overwrite : boolean; DataDirs : tStringList = Nil);
//currently only does NAD83 to WGS84 for the horizontal datum
//this was initially for US 3DEP, but now works with CGVD2013 for the vertical datum

   procedure OneDirectory(DataDir : PathStr);
   var
      FilesWanted : tStringList;
      DEM,i  : integer;
      fName : PathStr;
      InName,SaveName : PathStr;
      UTMZone,s_SRSstring,t_srsstring : shortstring;
   begin
      {$If Defined(Record3DEPX)} HighlightLineToDebugFile('BatchGDAL_3DEP_shift in, DataDir=' + DataDir); {$EndIf}
      FilesWanted := tStringList.Create;
      Petmar.FindMatchingFiles(DataDir,'*.tif',FilesWanted,0);
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('TIF files found=' + IntToStr(FilesWanted.Count)); {$EndIf}
      if (FilesWanted.Count> 0) then begin
        for I := pred(FilesWanted.Count) downto 0 do begin
           fName := FilesWanted.Strings[i];
           if StrUtils.AnsiContainsText(fName,'egm2008_egm2008') then begin
               //this was processed earlier by mistake, and the check will be taken out eventually
               FilesWanted.Delete(I);
               File2Trash(fName);
           end
           else if StrUtils.AnsiContainsText(fName,'ref_1sec') then begin
               FilesWanted.Delete(I);
           end
           else if (StrUtils.AnsiContainsText(fName,'egm2008')) then begin
               FilesWanted.Delete(I);
           end
           else if (Not OverWrite) and FileExists(ChangeFileExt(fName,'_egm2008.tif')) then begin
               FilesWanted.Delete(I);
           end;
        end;
        {$If Defined(Record3DEPX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift TIF files to process=' + IntToStr(FilesWanted.Count)); {$EndIf}
        for I := 0 to pred(FilesWanted.Count) do begin
            wmdem.SetPanelText(2,'File: ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count));
            fName := FilesWanted.Strings[i];
            {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('Try to open ' + fName); {$EndIf}
            DEM := OpenNewDEM(fName,false);
            if ValidDEM(DEM) then begin
               InName := DEMGlb[DEM].GeotiffDEMName;
               SaveName := ChangeFileExt(fName,'_egm2008.tif');
               UTMZone := AddDayMonthLeadingZero(DEMGlb[DEM].DEMHeader.UTMzone);
               s_SRSstring := ' -s_srs EPSG:269' + UTMzone + '+' + IntToStr(DEMIX_vert_datum_code);
               t_srsstring := ' -t_srs EPSG:326' + UTMzone + '+3855';
               {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('Try composite shift ' + fName); {$EndIf}
               CompositeDatumShiftWithGDAL(InName,SaveName,s_SRSstring,t_srsstring);
               CloseSingleDEM(DEM);
            end
            else begin
               {$If Defined(Record3DEPXFull)} HighlightLineToDebugFile('Error trying to create ' + SaveName); {$EndIf}
            end;
            {$If Defined(Record3DEPX)} WriteLineToDebugFile('BatchGDAL_3DEP_shifted, ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count) + ' ' + ExtractFileName(SaveName)); {$EndIf}
         end;
      end;
      FilesWanted.Free;
      CleanUpTempDirectory(True);
   end;


var
   I : Integer;
   LocalList : boolean;
begin {procedure DEMIX_GDAL_3DEP_datum_shift}
   try
      GetDEMIXpaths;
      ReportErrors := false;
      LocalList := (DataDirs = Nil);
      if LocalList then begin
         Get3DEPDataDirs(DataDirs);
      end;

      {$If Defined(Record3DEPX)} HighlightLineToDebugFile('BatchGDAL_3DEP_shift paths=' + IntToStr(DataDirs.Count)); {$EndIf}

     if (DEMIX_vert_datum_code = 0) then begin
        ReadDefault('EPSG vertical datum code for input DEMs (5703 for NAVD, 6647 for CGVD2013, 0 to exit)',DEMIX_vert_datum_code);
        if (DEMIX_vert_datum_code = 0) then exit;
     end;
     for I := 0 to pred(DataDirs.Count) do begin
         wmdem.SetPanelText(1,'Dir: ' + IntToStr(succ(i)) + '/' + IntToStr(DataDirs.Count) + ' ' + LastSubDir(DataDirs.Strings[i]));
         OneDirectory(DataDirs.Strings[i]);
      end;
     if LocalList then DataDirs.Free;
   finally
      EndDEMIXProcessing;
      DEMIX_vert_datum_code := 0;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift out'); {$EndIf}
end {procedure DEMIX_GDAL_3DEP_datum_shift};


procedure DEMIX_Ref_DEM_full_chain(overwrite : boolean);
var
   DataDirs : tStringList;
begin
   GetDEMIXpaths;
   Get3DEPDataDirs(DataDirs);
   ShiftDEMsto_UTM_WGS84_EGM2008(Overwrite,DataDirs);
   DEMIX_CreateReferenceDEMsFromSource(Overwrite,DataDirs);
   DEMIX_MergeReferenceDEMs(Overwrite,DataDirs);
end;


procedure ConvertASCtoGeotiffDEMs(aPath : PathStr);
var
   ASCIIDEMs : tStringList;
   i,NewDEM : integer;
begin
   ASCIIDEMs := tStringList.Create;
   FindMatchingFiles(aPath,'*.asc',ASCIIDEMs,5);
   if (ASCIIDEMs.Count > 0) then begin
      //convert ASC files to Tiff, there must be a single WKT projection file in the directory
      for I := 0 to pred(ASCIIDEMs.Count) do begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Convert ASC file= ' + ASCIIDEMs.Strings[i]); {$EndIf}
         NewDEM := OpenNewDEM(ASCIIDEMs.Strings[i],false);
         CloseSingleDEM(NewDEM);
      end;
   end;
   ASCIIDEMs.Free;
end;


procedure DEMIX_merge_source(Areas : tStringList = Nil);
var
   DEMs : tStringList;
   Fixed : int64;
   VDatumCode,NewDEM,AnArea : integer;
   AreaInventoryName,AreaMergeName, AreaPath,DEMName,OutVRT : PathStr;
   AreaName,TStr : shortstring;
   AreaInventory : tMyData;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source in'); {$EndIf}
   try
      GetDEMIXPaths(true);
      if (Areas = nil) then begin
         Areas := tStringList.Create;
         Areas.Add(DEMIX_3DEP_Dir);
         GetMultipleDirectories('Non-3DEP DEMs', Areas);
      end;
      if (Areas.Count = 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No areas selected'); {$EndIf}
      end
      else begin
         AreaInventoryName := DEMIX_area_dbName;
         if FileExists(AreaInventoryName) or GetFileFromDirectory('New DEMIX area inventory','*.dbf',AreaInventoryName) then begin
            AreaInventory := tMyData.Create(AreaInventoryName);
            for anArea := 0 to pred(Areas.Count) do begin
               AreaName := LastSubDir(Areas.Strings[AnArea]);
               AreaName := RemoveDTMorDSMfromAreaName(AreaName);
               AreaPath := ExtractFilePath(Areas.Strings[AnArea]);
               wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(anArea)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName);
               AreaMergeName := AreaPath + AreaName + '.tif';
               if FileExists(AreaMergeName) then begin
                  {$If Defined(RecordDEMIX)} WriteLineToDebugFile('File Existed ' + AreaMergeName); {$EndIf}
               end
               else begin
                  AreaInventory.ApplyFilter('AREA=' + QuotedStr(AreaName));
                  if (AreaInventory.FiltRecsInDB = 1) then begin
                     VDatumCode := AreaInventory.GetFieldByNameAsInteger('VERT_CS');
                  end
                  else begin
                     VDatumCode := 0;
                  end;

                  wmdem.SetPanelText(3, 'Convert ASC to Geotiff');
                  //   6/1/2024, this is not working (for French DEMs)
                  //ConvertASCtoGeotiffDEMs(Areas.Strings[AnArea]);
                  DEMs := Nil;
                  FindMatchingFiles(AreaPath,'*.tif',DEMs,5);
                  if (DEMs.Count = 0) then begin
                     FindMatchingFiles(AreaPath,'*.asc',DEMs,5);
                  end;
                  if (DEMs.Count > 1) then begin
                     wmdem.SetPanelText(3, 'Merge DEMs=' + IntToStr(DEMs.Count));
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files= ' + IntToStr(DEMs.Count) + ' for ' + AreaMergeName); {$EndIf}
                     UseGDAL_VRT_to_merge(AreaMergeName,OutVRT,DEMs);
                     CleanUpTempDirectory;  //might be many tiled or compressed DEMs expanded
                  end
                  else begin
                     TStr := 'No TIF files found in ' + AreaPath;
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                  end;
                  DEMs.Free;
               end;
            end {for area};
         end;
         AreaInventory.Destroy;
      end;
      Areas.Free;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source out'); {$EndIf}
end;


procedure DEMIX_CreateReferenceDEMs(Overwrite : boolean; ResampleMode : byte; Areas : tStringList = Nil);
var
   fName,fName2,AreaPath,AreaInventoryName : PathStr;
   ErrorLog : tStringList;
   i,WantedDEM : integer;
   TStr,AreaName : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      ErrorLog := tStringList.Create;

      AreaInventoryName := DEMIX_area_dbName;
      if FileExists(AreaInventoryName) or GetFileFromDirectory('New DEMIX area inventory','*.dbf',AreaInventoryName) then begin
         if (Areas = nil) then begin
            Areas := tStringList.Create;
            Areas.Add(DEMIX_3DEP_Dir);
            GetMultipleDirectories('Source DEMs', Areas);
         end;

         if (Areas.Count > 0) then begin
           for i := 0 to pred(Areas.Count) do begin
              AreaName := LastSubDir(Areas.Strings[i]);
              AreaPath := ExtractFilePath(Areas.Strings[i]);
              AreaName := LastSubDir(AreaPath);
              wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + AreaName);
              fName := AreaPath + AreaName + '_dtm_' + Ref1SecAreaStr + '.tif';
              fName2 := AreaPath + AreaName + '_dtm_' + Ref1SecPointStr + '.tif';
              if (not Overwrite) and FileExists(fName) and FileExists(fName2) then begin
                 {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs already done ' + fName ); {$EndIf}
              end
              else begin
                 {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXreferenceDEMs for ' + AreaName); {$EndIf}
                 fName := AreaPath + AreaName + '.tif';
                 if FileExists(fName) then begin
                    WantedDEM := OpenNewDEM(fName,false);
                    if (DEMGlb[WantedDEM].DEMheader.VerticalCSTypeGeoKey = VertCSEGM2008) then begin
                       {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('Call ResampleForDEMIXOneSecDEMs ' + AreaName); {$EndIf}
                       ResampleForDEMIXOneSecDEMs(Overwrite,true,WantedDEM,false,AreaPath,ResampleMode);
                    end
                    else CloseSingleDEM(WantedDEM);
                 end
                 else begin
                    TStr := AreaName  + ' no merged EGM2008 DEM';
                    {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                    ErrorLog.Add(TStr);
                 end;
              end;
           end;
         end;
      end;
   finally
      Areas.Free;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_CreateReferenceDEMs Problems');
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs out'); {$EndIf}
end;


procedure DEMIXCreateHalfSecRefDEMs(AreaName : shortstring = '');
var
   fName : PathStr;
   ErrorLog,Areas : tStringList;
   i,WantedDEM : integer;
   TStr : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXCreateHalfSecRefDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      Areas := tStringList.Create;
      ErrorLog := tStringList.Create;
      Areas := DEMIX_AreasWanted;
      if (Areas.Count > 0) then begin
        for i := 0 to pred(Areas.Count) do begin
           AreaName := Areas.Strings[i];
           wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Areas.Count));
           wmdem.SetPanelText(3,'Half sec ref DEM ' +  AreaName);
           {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXreferenceDEMs for ' + AreaName); {$EndIf}
           fName := DEMIX_Ref_Half_sec + AreaName + '_ref_0.5sec.tif';
           if not FileExists(fName) then begin
              fName := DEMIX_Ref_Merge + AreaName + '.dem';
              if FileExists(fName) then begin
                 WantedDEM := OpenNewDEM(fName,true);   //need to open map to create the subset
                 if (DEMGlb[WantedDEM].DEMheader.VerticalCSTypeGeoKey = VertCSEGM2008) then begin
                    {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('Call ResampleForDEMIXOneSecDEMs ' + AreaName); {$EndIf}
                    ResampleForDEMIXOneSecDEMs(True,true,WantedDEM,false,DEMIX_Ref_1sec,ResampleModeHalfSec);
                 end
                 else begin
                    TStr := DEMGlb[WantedDEM].AreaName  + ' not EGM2008; vert datum=' + IntToStr(DEMGlb[WantedDEM].DEMheader.VerticalCSTypeGeoKey);
                    if AnswerIsYes(TStr + '  Proceed anyway') then begin
                       ResampleForDEMIXOneSecDEMs(true,true,WantedDEM,false,DEMIX_Ref_1sec,ResampleModeHalfSec);
                    end
                    else begin
                       {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                       ErrorLog.Add(TStr);
                       CloseSingleDEM(WantedDEM);
                    end;
                 end;
              end
              else begin
                 TStr := AreaName  + ' no merged source DEMs';
                 {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                 ErrorLog.Add(TStr);
              end;
           end;
        end;
      end;
   finally
      Areas.Free;
      DisplayAndPurgeStringList(ErrorLog,'DEMIXCreateHalfSecRefDEMs Problems');
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXCreateHalfSecRefDEMs out'); {$EndIf}
end;



procedure ResampleForDEMIXOneSecDEMs(OverWrite : boolean; CloseAfter : boolean; DEM : integer; OpenMap : boolean = false; OutPath : PathStr = ''; ResampleMode : byte = 1);
var
   NewDEM : integer;
   BaseName : PathStr;
   Ext : ExtStr;
   ps : float32;
begin
   {$If Defined(Record3DEPXFull)} WriteLineToDebugFile(''); WriteLineToDebugFile('ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
   if (DEMGlb[DEM].DEMFileName = '') then begin
      DEMGlb[DEM].AskAndWriteNewFormatDEM(DEMGlb[DEM].DEMFileName,' save DEM before resampling');
   end;
   {$If Defined(TrackDEMCorners)} DEMGlb[DEM].WriteDEMCornersToDebugFile('ResampleForDEMIXOneSecDEMs, starting DEM'); {$EndIf}
   try
      SaveBackupDefaults;
      HeavyDutyProcessing := true;
      MDDef.DefElevMap := mtDEMBlank;
      Ext := '.tif';

      if (OutPath = '') then OutPath := ExtractFilePath(DEMGlb[DEM].DEMFileName);
      BaseName := DEMGlb[DEM].AreaName;

      if ResampleMode in [ResampleModeBoth,ResampleModeOneSec] then begin
         {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('Start one sec'); {$EndIf}
         CreateArcSecDEM(Overwrite,OpenMap,DEM,PixelIsPoint,1,1,OutPath + BaseName + Ref1SecPointStr + Ext);
         CreateArcSecDEM(Overwrite,OpenMap,DEM,PixelIsArea,1,1,OutPath + BaseName + Ref1SecAreaStr + Ext);

         if (DEMGlb[DEM].DEMSWcornerLat > 50) and (DEMGlb[DEM].DEMSWcornerLat < 60) then begin
            CreateArcSecDEM(Overwrite,OpenMap,DEM,PixelIsPoint,1.5,1,OutPath + BaseName + Ref1_5SecPointStr + Ext);
         end;
      end;

      if ResampleMode in [ResampleModeRange] then begin
         {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('Range scales'); {$EndIf}
         ps := 0.5;
         while ps <= 5 do begin
            CreateArcSecDEM(Overwrite,OpenMap,DEM,PixelIsPoint,ps,ps,DEMIX_Ref_Half_sec + BaseName + RealToString(ps,-5,1) + 'sec' + Ext);
            ps := ps + 0.5;
         end;
      end;

      if ResampleMode in [ResampleModeBoth,ResampleModeHalfSec] then begin
         {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('Half sec'); {$EndIf}
         NewDEM := CreateArcSecDEM(Overwrite,OpenMap,DEM,PixelIsPoint,0.5,0.5,OutPath + BaseName + '0.5sec' + Ext);
      end;
   finally
      if CloseAfter then CloseAllDEMs;
      RestoreBackupDefaults;
      HeavyDutyProcessing := false;
   end;
end;


