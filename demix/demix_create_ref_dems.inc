{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{ include file for demix_definitions }
{____________________________________}


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
         wmdem.SetPanelText(3,IntToStr(succ(j)) + '/  ' + IntToStr(Areas.Count) + '  ' + Area,true);
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
   DataDirs.Add(DEMIX_3DEP_Dir);
   GetMultipleDirectories('3DEP DEMs', DataDirs);
end;


procedure MaskWaterInReferenceDEMs;
var
   TheFiles,Results : tStringList;
   i,DEM : integer;
   fName : PathStr;
begin
   TheFiles := Nil;
   FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',TheFiles);
   for i := 0 to pred(TheFiles.Count) do begin
      fName := theFiles.Strings[i];
      DEM := OpenNewDEM(fName,false);
      MarkWaterMissingInThisDEM(DEM);
      DEMglb[DEM].SaveAsGeotiff(DEMglb[DEM].DEMFileName);
      CloseSingleDEM(DEM);
   end;
   TheFiles.Free;
end;


procedure ShiftToUTM_WGS84_EGM2008(inName,SaveName : PathStr; SourceEPSG,TargetEPSG : shortString);
var
   s_SRSstring,t_srsstring : shortstring;
begin
   s_SRSstring := ' -s_srs EPSG:' + SourceEPSG;
   t_srsstring := ' -t_srs EPSG:' + TargetEPSG;
   CompositeDatumShiftWithGDAL(InName,SaveName,s_SRSstring,t_srsstring);
end;


procedure ShifDEMsto_UTM_WGS84_EGM2008;
var
   AreaProjectionInfo : tMyData;

         procedure ShiftAFile(fName : PathStr; VertCode,HorizCode,UTMzone : shortstring);
         var
            InName,SaveName : PathStr;
            s_SRSstring,t_srsstring : shortstring;

                  procedure OpenAndCheck(Starter : shortstring);
                  var
                    zone : integer;
                  begin
                     zone := Geotiff_UTMzone(fName);
                     UTMZone := AddDayMonthLeadingZero(zone);
                     HorizCode := Starter + UTMZone;
                     AreaProjectionInfo.Edit;
                     AreaProjectionInfo.SetFieldByNameAsString('UTM_ZONE',UTMzone);
                     AreaProjectionInfo.Post;
                  end;

         begin
            SaveName := ChangeFileExt(fName,'_egm2008.tif');
            if (not FileExists(SaveName)) or Overwrite then begin
               {$If Defined(Record3DEPX)} WriteLineToDebugFile('ShiftAFile=' + fName + '  horiz code=' + HorizCode); {$EndIf}
               InName := fName;
               if not FileExists(SaveName) then begin
                  if HorizCode = 'NAD83' then begin
                     if (UTMZone = '') then OpenAndCheck('269')
                     else HorizCode := '269' + UTMZone;
                  end
                  else if HorizCode = 'WGS84' then begin
                     if (UTMZone = '') then OpenAndCheck('268')
                     else HorizCode := '269' + UTMZone;
                  end
                  else if HorizCode = 'ETRS89' then begin
                     if (UTMZone = '') then OpenAndCheck('258')
                     else HorizCode := '258' + UTMZone;
                  end
                  else if (HorizCode = 'GCS_WGS') then begin
                     HorizCode := '4326'
                  end
                  else if (HorizCode = '') then begin
                     {$If Defined(Record3DEPX)} HighlightLineToDebugFile('ShiftAFile=' + fName + ' undefined or unsupported ' + HorizCode); {$EndIf}
                     exit;
                  end;
               end;
               s_SRSstring := HorizCode + '+' + VertCode;
               t_srsstring := '326' + UTMzone + '+3855';          //WGS84 + EGM2008
               ShiftToUTM_WGS84_EGM2008(inName,SaveName,s_SRSstring,t_srsstring);
            end
            else begin
               {$If Defined(Record3DEPXfull)} WriteLineToDebugFile('Already shifted=' + fName); {$EndIf}
            end;
         end;


var
   VertCode,HorizCode,UTMzone : shortstring;
   Files : tStringList;
   i,j : integer;
   AreaName,TStr : ShortString;
   LocalList : boolean;
   DirName,fName : PathStr;
begin {ShifDEMsto_UTM_WGS84_EGM2008}
   try
      GetDEMIXPaths;
      LocalList := (DataDirs = nil);
      if LocalList then Get3DEPDataDirs(DataDirs);
      AreaProjectionInfo := tMyData.Create(DEMIX_area_dbName);
      ShowHourglassCursor;
      {$If Defined(Record3DEPX)} HighlightLineToDebugFile('ShifDEMsto_UTM_WGS84_EGM2008 paths=' + IntToStr(DataDirs.Count)); {$EndIf}
      for I := 0 to pred(DataDirs.Count) do begin
         DirName := DataDirs.Strings[i];
         AreaName := LastSubDir(ExtractFilePath(DirName));
         TStr := 'Dir: ' + IntToStr(succ(i)) + '/' + IntToStr(DataDirs.Count) + ' ' + AreaName;
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
                   ShiftAFile(Fname,VertCode,HorizCode,UTMzone);
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
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('ShifDEMsto_UTM_WGS84_EGM2008 out'); {$EndIf}
   end;
end {ShifDEMsto_UTM_WGS84_EGM2008};



procedure DEMIX_MergeReferenceDEMs(Overwrite : boolean; DataDirs : tStringList = Nil);
const
   ShowMapWhileMerging = false;   //help track down problematic data sets
var
   DirStatus : shortstring;
   i : integer;
   OutPath,adir : PathStr;
   LocalList : boolean;
   AreaName : shortstring;


         procedure OnePixelIs(which : shortstring);
         var
            DoneName : PathStr;
            Files : tStringList;
            i,DEM : integer;
         begin
            DoneName := Outpath + AreaName + '_dtm' + which + '.tif';
            if Overwrite or (not FileExists(DoneName)) then begin
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
            DirStatus := 'Dir: ' + IntToStr(succ(i)) + '/' + IntToStr(DataDirs.Count) + '  ' + AreaName;
            wmdem.SetPanelText(3,DirStatus);
            aDir := DataDirs.Strings[i];
            OutPath := DEMIX_Ref_1sec;
            OnePixelIs(Ref1SecPointStr);
            OnePixelIs(Ref1SecAreaStr);
            OnePixelIs(Ref1_5SecPointStr);
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
      RemoveFilesThatDoNotHaveString(Files,'EGM2008');  //can only use DEMs with datum shift
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('MakeRefDEMsForOneDir Found EGM2008 TIFs=' + IntToStr(Files.Count)); {$EndIf}
      RemoveFilesThatDoHaveString(Files,Ref1SecPointStr); //cannot use DEMs already reference
      RemoveFilesThatDoHaveString(Files,Ref1SecAreaStr); //cannot use DEMs already reference
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('MakeRefDEMsForOneDir Found EGM2008 TIFs, REF not done yet=' + IntToStr(Files.Count)); {$EndIf}

      for i := 0 to pred(Files.Count) do begin
          fName := Files.Strings[i];
          AreaName := LastSubDir(ExtractFilePath(fName));
          wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Files.Count) + '  ' + AreaName);
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
      //ReportErrors := false;
      LockStatusBar := true;
      LocalList := (DataDirs = Nil);
      if LocalList then begin
         {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs getting data paths'); {$EndIf}
         Get3DEPDataDirs(DataDirs);
      end;
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs paths=' + IntToStr(DataDirs.Count)); {$EndIf}
      for I := 0 to pred(DataDirs.Count) do begin
         DirStatus := 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(DataDirs.Count) + '  ' + LastSubDir(DataDirs.Strings[i]);
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


(*
procedure DEMIXRef_DEM_create_full_chain(overwrite : boolean);
var
   DataDirs : tStringList;
begin
   GetDEMIXpaths;
   Get3DEPDataDirs(DataDirs);
   DEMIX_vert_datum_code := 5703;
   DEMIX_GDAL_Ref_DEM_datum_shift(Overwrite,DataDirs);
   DEMIX_CreateReferenceDEMsFromSource(Overwrite,DataDirs);
   DEMIX_MergeReferenceDEMs(Overwrite,DataDirs);
end;
*)


procedure DEMIX_Ref_DEM_full_chain(overwrite : boolean);
var
   DataDirs : tStringList;
begin
   GetDEMIXpaths;
   Get3DEPDataDirs(DataDirs);
   ShifDEMsto_UTM_WGS84_EGM2008(Overwrite,DataDirs);
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

(*
procedure DEMIX_merge_Visioterra_source(AreaName : shortstring = '');
var
   Areas,ErrorLog : tStringList;
   Fixed : int64;
   NewDEM,AnArea : integer;
   AreaMergeName,AreaPath,TStr : shortstring;


        procedure Merge(AreaPath : PathStr; SearchName,AreaMergeName : shortstring);
        var
           DEMs : tStringList;
        begin
            DEMs := Nil;
            FindMatchingFiles(AreaPath,SearchName,DEMs,5);
            if (DEMs.Count > 0) then begin
               wmdem.SetPanelText(3, 'Merge DEMs=' + IntToStr(DEMs.Count));
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files= ' + IntToStr(DEMs.Count) + ' for ' + AreaMergeName); {$EndIf}
               if (DEMs.Count = 1) then NewDEM := OpenNewDEM(DEMs.Strings[0],false)
               else NewDEM := MergeMultipleDEMsHere(DEMs,false,false);  //Frees DEMs
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files over, MinZ=' + RealToString(DEMGlb[NewDEM].DEMheader.MinElev,-12,-2)); {$EndIf}
               DEMGlb[NewDEM].DEMheader.VerticalCSTypeGeoKey := VertCSEGM2008;
               ShowHourglassCursor;
               wmdem.SetPanelText(3, 'Write DEM ' + AreaMergeName);
               DEMGlb[NewDEM].CheckMaxMinElev;
               DEMGlb[NewDEM].WriteNewFormatDEM(AreaMergeName);
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge saved to ' + AreaMergeName); {$EndIf}
               CloseSingleDEM(NewDEM);
               CleanUpTempDirectory;  //might be many tiled or compressed DEMs expanded
            end
            else begin
               TStr := 'No ' + SearchName + ' files found in ' + AreaPath;
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
               ErrorLog.Add(TStr);
            end;
       end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_Visioterra_source'); {$EndIf}
   try
      GetDEMIXPaths(true);
      ErrorLog := tStringList.Create;
      Areas := DEMIX_AreasWanted;
      if (Areas.Count = 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No areas selected'); {$EndIf}
      end
      else begin
         for anArea := 0 to pred(Areas.Count) do begin
            wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(anArea)) + '/' + IntToStr(Areas.Count));
            AreaName := Areas.Strings[AnArea];
            AreaPath := DEMIX_Ref_Source + AreaName + '\';
            AreaMergeName := DEMIX_Ref_1sec + AreaName + Ref1SecAreaStr + '.tif';
            Merge(AreaPath,'*_pia.tif',AreaMergeName);
            AreaMergeName := DEMIX_Ref_1sec + AreaName + Ref1SecPointStr + '.tif';
            Merge(AreaPath,'*_pip.tif',AreaMergeName);
          end {for area};
      end;
      Areas.Free;
   finally
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_merge_Visioterra_source');
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_Visioterra_source'); {$EndIf}
end;
*)

procedure DEMIX_merge_source(Areas : tStringList = Nil);
var
   DEMs,ErrorLog : tStringList;
   Fixed : int64;
   VDatumCode,NewDEM,AnArea : integer;
   AreaInventoryName,AreaMergeName, AreaPath,DEMName,OutVRT : PathStr;
   AreaName,TStr : shortstring;
   AreaInventory : tMyData;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source in'); {$EndIf}
   try
      GetDEMIXPaths(true);
      ErrorLog := tStringList.Create;
      if (Areas = nil) then begin
         Areas := tStringList.Create;
         Areas.Add(DEMIX_3DEP_Dir);
         GetMultipleDirectories('Non-3DEP DEMs', Areas);
      end;

//put the black box back up for this since it takes so long


      if (Areas.Count = 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No areas selected'); {$EndIf}
      end
      else begin
         //LocalToWGS84 := 0;
         //WGS84toEGM2008 := 0;
         AreaInventoryName := DEMIX_area_dbName;
         if FileExists(AreaInventoryName) or GetFileFromDirectory('New DEMIX area inventory','*.dbf',AreaInventoryName) then begin
            AreaInventory := tMyData.Create(AreaInventoryName);
            for anArea := 0 to pred(Areas.Count) do begin
               AreaName := LastSubDir(Areas.Strings[AnArea]);
               AreaName := RemoveDTMorDSMfromAreaName(AreaName);
               AreaPath := ExtractFilePath(Areas.Strings[AnArea]);
               wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(anArea)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName);
               AreaMergeName := AreaPath + AreaName + '.tif';
               if {DEMIXSkipFilesAlreadyDone and} FileExists(AreaMergeName) then begin
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
                  ConvertASCtoGeotiffDEMs(Areas.Strings[AnArea]);

                  (*
                  if (VDatumCode = 0) then begin
                     ErrorLog.Add(AreaName + ' undefined VDatumCode');
                     ReadDefault('Vertical datum code for ' + AreaName,VDatumCode);
                  end
                  else begin
                  *)
                     DEMs := Nil;
                     FindMatchingFiles(AreaPath,'*.tif',DEMs,5);
                     if (DEMs.Count > 1) then begin
                        wmdem.SetPanelText(3, 'Merge DEMs=' + IntToStr(DEMs.Count));
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files= ' + IntToStr(DEMs.Count) + ' for ' + AreaMergeName); {$EndIf}
                        //if (DEMs.Count = 1) then NewDEM := OpenNewDEM(DEMs.Strings[0],false)
                        //else NewDEM := MergeMultipleDEMsHere(DEMs,false,false);  //Frees DEMs
                       UseGDAL_VRT_to_merge(AreaMergeName,OutVRT,DEMs);

(*
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files over, MinZ=' + RealToString(DEMGlb[NewDEM].DEMheader.MinElev,-12,-2)); {$EndIf}
                        DEMGlb[NewDEM].DEMheader.VerticalCSTypeGeoKey := VDatumCode;
                        DEMGlb[NewDEM].CheckMaxMinElev;
                        DEMGlb[NewDEM].WriteNewFormatDEM(AreaMergeName);
                        if (VDatumCode <> VertCSEGM2008) then begin //shift to EGM2008
                           if (AreaInventory.FiltRecsInDB = 1) then begin
                              wmdem.SetPanelText(3, 'Vertical datum shift ' + AreaMergeName);
                              GeoidWGS84ellipsoidToLocalVDatum := AreaInventory.GetFieldByNameAsString('VERT_SUB');
                              FindDriveWithFile(GeoidWGS84ellipsoidToLocalVDatum);
                              LoadDatumShiftGrids(LocalToWGS84,WGS84toEGM2008);
                              DEMGlb[NewDEM].MoveToEGM2008(WGS84toEGM2008,LocalToWGS84);
                              {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Shifted to EGM2008 using ' + DEMGLB[LocalToWGS84].AreaName + ' and ' + DEMGLB[WGS84toEGM2008].AreaName); {$EndIf}
                              CloseSingleDEM(LocalToWGS84);
                           end;
                           ShowHourglassCursor;
                           wmdem.SetPanelText(3, 'Write DEM ' + AreaMergeName);
                           DEMGlb[NewDEM].CheckMaxMinElev;
                           DEMGlb[NewDEM].WriteNewFormatDEM(AreaMergeName);
                           {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge saved to ' + AreaMergeName); {$EndIf}
                        end;
                        CloseSingleDEM(NewDEM);
*)
                        CleanUpTempDirectory;  //might be many tiled or compressed DEMs expanded
                     end
                     else begin
                        TStr := 'No TIF files found in ' + AreaPath;
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                        ErrorLog.Add(TStr);
                     end;
                  //end;
               end;
            end {for area};
         end;
         AreaInventory.Destroy;
      end;
      Areas.Free;
   finally
      //CloseSingleDEM(WGS84toEGM2008);
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_merge_source problems');
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
           wmdem.SetPanelText(3, AreaName);
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
   fName,BaseName : PathStr;
   Ext : ExtStr;
   ps : float32;

      function CreateOneRefDEM(PixelIs : byte; xgridsize,ygridsize : float32; fName : PathStr) : integer;
      begin
         if (not Overwrite) and FileExists(fName) then begin
            Result := OpenNewDEM(fName);
         end
         else begin
            MDdef.DefLidarGeoGridSizeX := xgridsize;
            MDdef.DefLidarGeoGridSizeY := ygridsize;
            MDDef.LasDEMPixelIs := PixelIs;
            {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('CreateOneRefDEM, ' + fName); {$EndIf}
            Result := DEMGlb[DEM].ResampleByAveraging(OpenMap,false,fName);
            {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('ResampleForDEMIXOneSecDEMs, new DEM=' + IntToStr(Result)); {$EndIf}
            {$If Defined(TrackDEMCorners)} DEMGlb[Result].WriteDEMCornersToDebugFile('ResampleForDEMIXOneSecDEMs, new DEM=' + IntToStr(Result)); {$EndIf}
         end;
      end;

begin
   {$If Defined(Record3DEPXFull)} WriteLineToDebugFile(''); WriteLineToDebugFile('ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
   if (DEMGlb[DEM].DEMFileName = '') then begin
      DEMGlb[DEM].WriteNewFormatDEM(DEMGlb[DEM].DEMFileName,' save DEM before resampling');
   end;
   {$If Defined(TrackDEMCorners)} DEMGlb[DEM].WriteDEMCornersToDebugFile('ResampleForDEMIXOneSecDEMs, starting DEM'); {$EndIf}
   try
      SaveBackupDefaults;
      HeavyDutyProcessing := true;
      MDDef.DefDEMMap := mtDEMBlank;
      MDDef.LidarGridProjection := ArcSecDEM;
      Ext := '.tif';

      if (OutPath = '') then OutPath := ExtractFilePath(DEMGlb[DEM].DEMFileName);
      BaseName := DEMGlb[DEM].AreaName;

      if ResampleMode in [ResampleModeBoth,ResampleModeOneSec] then begin
         {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('Start one sec'); {$EndIf}
         CreateOneRefDEM(PixelIsPoint,1,1,OutPath + BaseName + Ref1SecPointStr + Ext);
         CreateOneRefDEM(PixelIsArea,1,1,OutPath + BaseName + Ref1SecAreaStr + Ext);

         if (DEMGlb[DEM].DEMSWcornerLat > 50) and (DEMGlb[DEM].DEMSWcornerLat < 60) then begin
            CreateOneRefDEM(PixelIsPoint,1.5,1,OutPath + BaseName + Ref1_5SecPointStr + Ext);
         end;
      end;

      if ResampleMode in [ResampleModeRange] then begin
         {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('Range scales'); {$EndIf}
         ps := 0.5;
         while ps <= 5 do begin
            CreateOneRefDEM(PixelIsPoint,ps,ps,DEMIX_Ref_Half_sec + BaseName + RealToString(ps,-5,1) + 'sec' + Ext);
            ps := ps + 0.5;
         end;
      end;

      if ResampleMode in [ResampleModeBoth,ResampleModeHalfSec] then begin
         {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('Half sec'); {$EndIf}
         NewDEM := CreateOneRefDEM(PixelIsPoint,0.5,0.5,OutPath + BaseName + '0.5sec' + Ext);
      end;
   finally
      if CloseAfter then CloseAllDEMs;
      RestoreBackupDefaults;
      HeavyDutyProcessing := false;
   end;
end;


