{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{include file for demix_control.pas  }
{____________________________________}



procedure ResampleForDEMIXOneSecDEMs(DEM : integer; OpenMap : boolean = false; OutPath : PathStr = ''; ResampleMode : byte = 1);
var
   NewDEM : integer;
   fName,BaseName : PathStr;
   Ext : ExtStr;

      function CreateOneRefDEM(PixelIs : byte; xgridsize,ygridsize : float32; fName : PathStr) : integer;
      begin
         if FileExists(fName) then begin
            Result := OpenNewDEM(fName);
         end
         else begin
            MDdef.DefLidarGeoGridSizeX := xgridsize;
            MDdef.DefLidarGeoGridSizeY := ygridsize;
            MDDef.LasDEMPixelIs := PixelIs;
            {$If Defined(Record3DEPX)} WriteLineToDebugFile('CreateOneRefDEM, ' + fName); {$EndIf}
            Result := DEMGlb[DEM].ResampleByAveraging(OpenMap,false,fName);
            {$If Defined(Record3DEPX)} WriteLineToDebugFile('ResampleForDEMIXOneSecDEMs, new DEM=' + IntToStr(Result)); {$EndIf}
            {$If Defined(TrackDEMCorners)} DEMGlb[Result].WriteDEMCornersToDebugFile('ResampleForDEMIXOneSecDEMs, new DEM=' + IntToStr(Result)); {$EndIf}
         end;
      end;


begin
   {$If Defined(RecordDEMIX) or Defined(Record3DEPX)} WriteLineToDebugFile(''); WriteLineToDebugFile('ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
   if (DEMGlb[DEM].DEMFileName = '') then begin
      DEMGlb[DEM].WriteNewFormatDEM(DEMGlb[DEM].DEMFileName,' save DEM before resampling');
   end;
   {$If Defined(TrackDEMCorners)} DEMGlb[DEM].WriteDEMCornersToDebugFile('ResampleForDEMIXOneSecDEMs, starting DEM'); {$EndIf}
   try
      SaveBackupDefaults;
      MDDef.DefDEMMap := mtDEMBlank;
      MDDef.LidarGridProjection := ArcSecDEM;
      Ext := '.tif';

      if (OutPath = '') then OutPath := ExtractFilePath(DEMGlb[DEM].DEMFileName);
      BaseName := DEMGlb[DEM].AreaName + '_ref_';

      if ResampleMode in [ResampleModeBoth,ResampleModeHalfSec] then begin
         {$If Defined(Record3DEPX)} WriteLineToDebugFile('Half sec'); {$EndIf}
         NewDEM := CreateOneRefDEM(PixelIsPoint,0.5,0.5,DEMIX_Ref_Half_sec + BaseName + '0.5sec' + Ext);
      end;

   (*
      if true {MDDef.UseHalfPixelAggregation} then begin
         if DEMGlb[NewDEM].PixelCenterOnFullSecond then begin
            //sw corner is on a full seccond
            PointOffset := 0;
            AreaOffset := 1;
         end
         else begin
            //sw corner is on a half seccond
            PointOffset := 1;
            AreaOffset := 0;
         end;
         fName := OutPath + BaseName + '1sec_point_v2' + Ext;
         New2 := DEMGlb[NewDEM].HalfPixelAggregation(fName,PixelIsPoint,true,PointOffset);
         //this one does not have the correct shift
         fName := OutPath + BaseName + '1sec_area_v2' + Ext;
         New3 := DEMGlb[NewDEM].HalfPixelAggregation(fName,PixelIsArea,true,AreaOffset);
      end;
   *)
      if ResampleMode in [ResampleModeBoth,ResampleModeOneSec] then begin
         {$If Defined(Record3DEPX)} WriteLineToDebugFile('Start one sec'); {$EndIf}
         CreateOneRefDEM(PixelIsPoint,1,1,OutPath + BaseName + '1sec_point' + Ext);
         CreateOneRefDEM(PixelIsArea,1,1,OutPath + BaseName + '1sec_area' + Ext);

         if (DEMGlb[DEM].DEMSWcornerLat > 50) and (DEMGlb[DEM].DEMSWcornerLat < 60) then begin
            CreateOneRefDEM(PixelIsPoint,1.5,1,OutPath + BaseName + '1.5x1sec_point' + Ext);
         end;
      end;
   finally
      CloseAllDEMs;
      RestoreBackupDefaults;
   end;
end;




procedure DEMIX_Merge3DEPReferenceDEMs;
var
   ErrorLog,Paths : tStringList;
   DirStatus : shortstring;
   i : integer;
   OutPath : PathStr;


   procedure MergeRefDEMsForOneDir(aDir : PathStr);

         procedure OnePixelIs(which : shortstring);
         var
            DoneName : PathStr;
            Files : tStringList;
            i,DEM : integer;
            AreaName : shortstring;
         begin
            AreaName := LastSubDir(aDir);
            DoneName := Outpath + AreaName + which + '.tif';
            if DEMIXSkipFilesAlreadyDone and FileExists(DoneName) then begin
               {$If Defined(Record3DEPX)} WriteLineToDebugFile('MakeRefDEMsForOneDir already done ' + DoneName ); {$EndIf}
            end
            else begin
               Files := nil;
               Petmar.FindMatchingFiles(aDir,'*.dem',Files,0);
               {$If Defined(Record3DEPX)} WriteLineToDebugFile('OnePixelIs ' + aDir + ' Found DEMs=' + IntToStr(Files.Count)); {$EndIf}
               RemoveFilesThatDoNotHaveString(Files,which);  //can only use DEMs with aggregated to 1 arc sec, for correct pixel-is
               {$If Defined(Record3DEPX)} WriteLineToDebugFile('OnePixelIs Found aggregated ref DEMs=' + IntToStr(Files.Count)); {$EndIf}
               if (Files.Count = 0) then begin
                  Files.Destroy;
               end
               else begin
                  wmdem.SetPanelText(2, DirStatus + '  DEM: ' + IntToStr(succ(i)) + '/' + IntToStr(Files.Count));
                  DEM := MergeMultipleDEMsHere(Files,false,false);
                  DEMGlb[DEM].SavePartOfDEMWithData(DoneName);
                end;
            end;
            CloseAllDEMs;
         end;
   begin
      OnePixelIs('_ref_1sec_point');
      OnePixelIs('_ref_1sec_area');
   end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      OutPath := 'j:\us\';
      ErrorLog := tStringList.Create;
      if PathIsValid(DataDir) then begin
         MergeRefDEMsForOneDir(DataDir);
      end
      else begin
         {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs getting data paths'); {$EndIf}
         Paths := tStringList.Create;
         Paths.Add(DEMIX_3DEP_Dir);
         if GetMultipleDirectories('3DEP DEMs', Paths) then begin
            {$If Defined(Record3DEPX)} HighlightLineToDebugFile('DEMIX_Create3DEPReferenceDEMs paths=' + IntToStr(Paths.Count)); {$EndIf}
            for I := 0 to pred(Paths.Count) do begin
               DirStatus := 'Dir: ' + IntToStr(succ(i)) + '/' + IntToStr(Paths.Count);
               wmdem.SetPanelText(3,DirStatus);
               MergeRefDEMsForOneDir(Paths.Strings[i]);
            end;
         end;
         Paths.Free;
      end;
   finally
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_Create3DEPReferenceDEMs Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs out'); {$EndIf}
end;



procedure DEMIX_Create3DEPReferenceDEMs(DataDir : PathStr = '');
var
   ErrorLog,Paths : tStringList;
   DirStatus : shortstring;
   i : integer;


   procedure MakeRefDEMsForOneDir(aDir : PathStr);
   var
      fName,DoneName : PathStr;
      Files : tStringList;
      i,WantedDEM : integer;
   begin
      Files := nil;
      Petmar.FindMatchingFiles(aDir,'*.tif',Files,0);
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('MakeRefDEMsForOneDir ' + aDir + ' Found TIFs=' + IntToStr(Files.Count)); {$EndIf}
      RemoveFilesThatDoNotHaveString(Files,'EGM2008');  //can only use DEMs with datum shift

      RemoveFilesThatDoHaveString(Files,'_REF_'); //cannot use DEMs already reference

      {$If Defined(Record3DEPX)} WriteLineToDebugFile('MakeRefDEMsForOneDir Found EGM2008 TIFs, not ref=' + IntToStr(Files.Count)); {$EndIf}
      for i := 0 to pred(Files.Count) do begin
         fName := Files.Strings[i];
         wmdem.SetPanelText(2, DirStatus + '  DEM: ' + IntToStr(succ(i)) + '/' + IntToStr(Files.Count));
         DoneName := ChangeFileExt(fName,'_ref_1sec_point.tif');
         if DEMIXSkipFilesAlreadyDone and FileExists(DoneName) then begin
            {$If Defined(Record3DEPX)} WriteLineToDebugFile('MakeRefDEMsForOneDir already done ' + fName ); {$EndIf}
         end
         else begin
            {$If Defined(Record3DEPX)} WriteLineToDebugFile('MakeRefDEMsForOneDir file=' + fName ); {$EndIf}
            WantedDEM := OpenNewDEM(fName,true);   //need to open map to create the subset
            if ValidDEM(WantedDEM) then begin
               wmdem.SetPanelText(3, DEMGlb[WantedDEM].AreaName);
               ResampleForDEMIXOneSecDEMs(WantedDEM,false,aDir,ResampleModeOneSec);
            end;
            CloseSingleDEM(WantedDEM);
          end;
      end;
   end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      ErrorLog := tStringList.Create;
      if PathIsValid(DataDir) then begin
         MakeRefDEMsForOneDir(DataDir);
      end
      else begin
         {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs getting data paths'); {$EndIf}
         Paths := tStringList.Create;
         Paths.Add(DEMIX_3DEP_Dir);
         if GetMultipleDirectories('3DEP DEMs', Paths) then begin
            {$If Defined(Record3DEPX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs paths=' + IntToStr(Paths.Count)); {$EndIf}
            for I := 0 to pred(Paths.Count) do begin
               DirStatus := 'Dir: ' + IntToStr(succ(i)) + '/' + IntToStr(Paths.Count);
               wmdem.SetPanelText(3,DirStatus);
               MakeRefDEMsForOneDir(Paths.Strings[i]);
            end;
         end;
         Paths.Free;
      end;
   finally
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_Create3DEPReferenceDEMs Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs out'); {$EndIf}
end;



procedure BatchGDAL_3DEP_shift(DataDir : PathStr = '');

   procedure DatumShiftforUS3DEParea(DEM : integer = 0);
   //for a single DEM, called by routine above for many
   var
      InName,SaveName : PathStr;
      UTMZone,s_SRSstring,t_srsstring : shortstring;
   begin
      try
         GetDEMIXpaths;
         if not ValidDEM(DEM) then begin
            DEM := OpenNewDEM('',false);
         end;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DatumShiftforUS3DEParea in, ' + DEMGlb[DEM].AreaName); {$EndIf}
         InName := DEMGlb[DEM].GeotiffDEMName;
         SaveName := ChangeFileExt(InName,'_egm2008.tif');
         UTMZone := AddDayMonthLeadingZero(DEMGlb[DEM].DEMHeader.UTMzone);
         s_SRSstring := ' -s_srs EPSG:269' + UTMzone + '+5703';
         t_srsstring := ' -t_srs EPSG:326' + UTMzone + '+3855';
         CompositeDatumShiftWithGDAL(InName,SaveName,s_SRSstring,t_srsstring);
      finally
          EndDEMIXProcessing;
      end;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DatumShiftforUS3DEParea out'); {$EndIf}
   end;


   procedure OneDirectory(DataDir : PathStr);
   var
      FilesWanted : tStringList;
      DEM,i  : integer;
      fName : PathStr;
      TStr : shortstring;
   begin
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift in, DataDir=' + DataDir); {$EndIf}
      FilesWanted := tStringList.Create;
      Petmar.FindMatchingFiles(DataDir,'*.tif',FilesWanted,0);
      if (FilesWanted.Count = 0) then begin
         {$If Defined(Record3DEPX)} WriteLineToDebugFile('No DEMs found in ' + DataDir); {$EndIf}
      end
      else begin
         for I := 0 to pred(FilesWanted.Count) do begin
            TStr := 'GDAL 3DEP datum shift: ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count);
            wmdem.SetPanelText(2,TStr);
            fName := FilesWanted.Strings[i];
            if (FileExists(ChangeFileExt(fName,'_egm2008.tif'))) or StrUtils.AnsiContainsText(UpperCase(fname),'EGM2008') then begin
               {$If Defined(Record3DEPX)} WriteLineToDebugFile('Already done ' + ExtractFileName(fName)); {$EndIf}
               //already done EGM2008 shift
            end
            else begin
               wmdem.SetPanelText(3,ExtractFileName(fName));
               DEM := OpenNewDEM(fName,false);
               DatumShiftforUS3DEParea(DEM);
               CloseSingleDEM(DEM);
            end;
         end;
      end;
      FilesWanted.Free;
   end;


var
   Paths : tStringList;
   I : Integer;
begin
   try
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift in, DataDir=' + DataDir); {$EndIf}
      GetDEMIXpaths;
      {$If Defined(Record3DEPX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift done get DEMIX paths'); {$EndIf}
      if PathIsValid(DataDir) then begin
         OneDirectory(DataDir);
      end
      else begin
         {$If Defined(Record3DEPX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift getting data paths'); {$EndIf}
         Paths := tStringList.Create;
         Paths.Add(DEMIX_3DEP_Dir);
         if GetMultipleDirectories('3DEP DEMs', Paths) then begin
             {$If Defined(Record3DEPX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift paths=' + IntToStr(Paths.Count)); {$EndIf}
            for I := 0 to pred(Paths.Count) do begin
               wmdem.SetPanelText(1,'Dir: ' + IntToStr(succ(i)) + '/' + IntToStr(Paths.Count) + ' ' + LastSubDir(Paths.Strings[i]));
               OneDirectory(Paths.Strings[i]);
            end;
         end;
         Paths.Free;
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift out'); {$EndIf}
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
               if FilterOutSeaLevel then begin
                  if (abs(DEMGlb[NewDEM].DEMheader.MinElev) < 0.001) then begin
                     //mark sea level as missing for analysis along coast
                     DEMGlb[NewDEM].MarkInRangeMissing(-0.001,0.001,Fixed);
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Sea level missing done, pts removed=' + IntToStr(Fixed)); {$EndIf}
                  end;
               end;
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
      Areas := DEMIX_AreasWanted (AreaName);
      if (Areas.Count = 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No areas selected'); {$EndIf}
      end
      else begin
         for anArea := 0 to pred(Areas.Count) do begin
            wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(anArea)) + '/' + IntToStr(Areas.Count));
            AreaName := Areas.Strings[AnArea];
            AreaPath := DEMIX_Ref_Source + AreaName + '\';
            AreaMergeName := DEMIX_Ref_1sec + AreaName + '_ref_1sec_area.tif';
            Merge(AreaPath,'*_pia.tif',AreaMergeName);
            AreaMergeName := DEMIX_Ref_1sec + AreaName + '_ref_1sec_point.tif';
            Merge(AreaPath,'*_pip.tif',AreaMergeName);
          end {for area};
      end;
      Areas.Free;
   finally
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_merge_Visioterra_source');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_Visioterra_source'); {$EndIf}
end;


procedure DEMIX_merge_source(Areas : tStringList = Nil);
var
   DEMs,ErrorLog : tStringList;
   Fixed : int64;
   VDatumCode,NewDEM,AnArea,LocalToWGS84,WGS84toEGM2008 : integer;
   AreaInventoryName,AreaMergeName, AreaPath : PathStr;
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

      if (Areas.Count = 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No areas selected'); {$EndIf}
      end
      else begin
         LocalToWGS84 := 0;
         WGS84toEGM2008 := 0;
         AreaInventoryName := 'G:\v3_new_dems\v3_areas.dbf';
         if FileExists(AreaInventoryName) or GetFileFromDirectory('New DEMIX area inventory','*.dbf',AreaInventoryName) then begin
            AreaInventory := tMyData.Create(AreaInventoryName);
            for anArea := 0 to pred(Areas.Count) do begin
               AreaName := LastSubDir(Areas.Strings[AnArea]);
               AreaPath := ExtractFilePath(Areas.Strings[AnArea]);
               wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(anArea)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName);
               AreaMergeName := AreaPath + AreaName + '.dem';
               if DEMIXSkipFilesAlreadyDone and FileExists(AreaMergeName) then begin
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

                  if (VDatumCode = 0) then begin
                     ErrorLog.Add(AreaName + ' undefined VDatumCode');
                  end
                  else begin
                     DEMs := Nil;
                     FindMatchingFiles(AreaPath,'*.tif',DEMs,5);
                     if (DEMs.Count > 0) then begin
                        wmdem.SetPanelText(3, 'Merge DEMs=' + IntToStr(DEMs.Count));
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files= ' + IntToStr(DEMs.Count) + ' for ' + AreaMergeName); {$EndIf}
                        if (DEMs.Count = 1) then NewDEM := OpenNewDEM(DEMs.Strings[0],false)
                        else NewDEM := MergeMultipleDEMsHere(DEMs,false,false);  //Frees DEMs
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files over, MinZ=' + RealToString(DEMGlb[NewDEM].DEMheader.MinElev,-12,-2)); {$EndIf}
                        if FilterOutSeaLevel then begin
                           if (abs(DEMGlb[NewDEM].DEMheader.MinElev) < 0.001) then begin
                              //mark sea level as missing for analysis along coast
                              DEMGlb[NewDEM].MarkInRangeMissing(-0.001,0.001,Fixed);
                              {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Sea level missing done, pts removed=' + IntToStr(Fixed)); {$EndIf}
                           end;
                        end;
                        DEMGlb[NewDEM].DEMheader.VerticalCSTypeGeoKey := VDatumCode;
                        DEMGlb[NewDEM].CheckMaxMinElev;
                        DEMGlb[NewDEM].WriteNewFormatDEM(AreaMergeName);
                        (*
                        if (VDatumCode = VertCSNAVD88)  then begin
                           //no longer doing this, but using GDAL
                           fName := vd_path + AreaName + '.csv';
                           if not FileExists(fName) then begin
                              DEMGlb[NewDEM].CSVforVDatum(0.2,fName);
                              TStr := AreaName + ' VDatumCSV created for NAVD88; run through NOAA VDATUM';
                              {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                              ErrorLog.Add(TStr);
                           end;
                        end
                        else
                        *)
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
                        CleanUpTempDirectory;  //might be many tiled or compressed DEMs expanded
                     end
                     else begin
                        TStr := 'No TIF files found in ' + AreaPath;
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                        ErrorLog.Add(TStr);
                     end;
                  end;
               end;
            end {for area};
         end;
         AreaInventory.Destroy;
      end;
      Areas.Free;
   finally
      EndDEMIXProcessing;
      CloseSingleDEM(WGS84toEGM2008);
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_merge_source problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source out'); {$EndIf}
end;


procedure DEMIX_CreateReferenceDEMs(Areas : tStringList = Nil);
var
   fName,AreaPath,AreaInventoryName,OutPath  : PathStr;
   ErrorLog : tStringList;
   i,WantedDEM : integer;
   TStr,AreaName : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      ErrorLog := tStringList.Create;

      AreaInventoryName := 'G:\v3_new_dems\v3_areas.dbf';
      if FileExists(AreaInventoryName) or GetFileFromDirectory('New DEMIX area inventory','*.dbf',AreaInventoryName) then begin
         OutPath := ExtractFilePath(AreaInventoryName);

         if (Areas = nil) then begin
            Areas := tStringList.Create;
            Areas.Add(DEMIX_3DEP_Dir);
            GetMultipleDirectories('Non-3DEP DEMs', Areas);
         end;

         if (Areas.Count > 0) then begin
           for i := 0 to pred(Areas.Count) do begin
              AreaPath := Areas.Strings[i];
              AreaName := LastSubDir(AreaPath);
              wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + AreaName);
              fName := OutPath + AreaName + '_ref_1sec_area.tif';
              if DEMIXSkipFilesAlreadyDone and FileExists(fName) then begin
                 {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs already done ' + fName ); {$EndIf}
              end
              else begin
                 {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXreferenceDEMs for ' + AreaName); {$EndIf}
                 fName := AreaPath + AreaName + '.dem';
                 if FileExists(fName) then begin
                    WantedDEM := OpenNewDEM(fName,true);   //need to open map to create the subset
                    if (DEMGlb[WantedDEM].DEMheader.VerticalCSTypeGeoKey = VertCSEGM2008) then begin
                       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call ResampleForDEMIXOneSecDEMs ' + AreaName); {$EndIf}
                       ResampleForDEMIXOneSecDEMs(WantedDEM,false,OutPath,ResampleModeOneSec);
                    end;
                    CloseSingleDEM(WantedDEM);
                 end
                 else begin
                    TStr := AreaName  + ' no merged EGM2008 DEM';
                    {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                    ErrorLog.Add(TStr);
                 end;
              end;
           end;
         end
         else begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs, no areas ' + DEMIX_Ref_Merge); {$EndIf}
         end;
      end;
   finally
      Areas.Free;
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_CreateReferenceDEMs Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs out'); {$EndIf}
end;

