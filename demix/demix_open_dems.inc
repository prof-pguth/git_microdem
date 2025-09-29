{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{     include file for demix_control     }
{________________________________________}



function OpenBothPixelIsDEMs(Area,Prefix : shortstring; RefDir,TestDir : PathStr; OpenMaps : boolean) : boolean;
//opens reference DTMs, for both pixel-is-point and pixel-is-area
//0 in the array is the reference data, -1 is the 1.5 sec data
//if this is for U120, U80, or U10 computations, all DEMs are masked to the area in question
const
   Ext = '.tif';
var
   fName : PathStr;
   i,MaskDEM : integer;

         procedure TrimDEM(DEM : integer);
         var
            Limits : tGridLimits;
         begin
            Limits := DEMGlb[DEM].FullDEMGridLimits;
            DEMGlb[DEM].FilledGridBox(Limits);
            fname := NextFileNumber(MDtempDir,DEMGlb[DEM].AreaName + '_trim','.dem');
            DEMGlb[DEM].WriteNewFormatDEMWithLimits(Limits,fName);
            DEMGlb[DEM].DEMFileName := fName;
            DEMGlb[DEM].ReloadDEM(true);
            if OpenMaps then DEMGlb[DEM].SelectionMap.DoBaseMapRedraw;
         end;

         procedure MaskToMatchUnderDEM(DEM : integer);
         begin
            if (DEM <> MaskDEM) then begin
               EditsDone := 0;
               MaskGridFromSecondGrid(DEM,MaskDEM,msSecondMissing);
               DEMGlb[DEM].CheckMaxMinElev;
               {$IfDef ShowOpenBothPixelIsDEMs} if OpenMaps then DEMGlb[DEM].SelectionMap.DoBaseMapRedraw; {$EndIf};
            end;
         end;

         procedure TryToOpen(fName : PathStr; var DEM : integer);
         begin
            if FileExists(fName) then begin
               DEM := OpenNewDEM(fName,OpenMaps);
               if ValidDEM(DEM) then begin
                  {$If Defined(RecordDEMIXopenGrids)} WriteLineToDebugFile(DEMglb[DEM].AreaName + IntegerToString(DEM,4) + '  ' + DEMGlb[DEM].GridCornerModel ); {$EndIf}

                  {$IfDef TrackSWcorner} DEMGlb[DEM].WriteToDebugSWCornerForComputations('OpenBothPixelIsDEMs'); {$EndIf}
               end
               else begin
                  Result := false;
               end;
            end
            else begin
               {$IfDef RecordDEMIX} HighlightLineToDebugFile('Missing file ' + fName); {$EndIf};
               Result := false;
            end;
         end;


       procedure MaskAndTrimCoastalDEMs;
       var
          i : integer;
       begin
          if MDDef.DEMIX_mode in [dmU120] then begin
             for i := 1 to NumAreaDEMs do begin
                 if (DEMGlb[AreaDEMs[i]].AreaName = 'COAST') then MaskDEM := AreaDEMs[i];
             end;
          end;
          if MDDef.DEMIX_mode in [dmU80] then begin
             for i := 1 to NumAreaDEMs do begin
                 if (DEMGlb[AreaDEMs[i]].AreaName = 'DILUV') then MaskDEM := AreaDEMs[i];
             end;
          end;
          if MDDef.DEMIX_mode in [dmU10] then begin
             for i := 1 to NumPtDEMs do begin
                 if (DEMGlb[PointDEMs[i]].AreaName = 'DILUV') then MaskDEM := PointDEMs[i];
             end;
          end;

          if (NumFilledDEMIXtilesOnDEM(MaskDEM) = 0) then begin
             Result := false;
             {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('No filled Delta DEM tiles in ' + Area); {$EndIf}
          end;
          if Result and ValidDEM(MaskDEM) then begin
             for i := dmxFirstPoint to NumPtDEMs do MaskToMatchUnderDEM(PointDEMs[i]);
             for i := dmxFirstArea to NumAreaDEMs do MaskToMatchUnderDEM(AreaDEMs[i]);
             {$IfDef ShowOpenBothPixelIsDEMs} wmdem.Tile; MessageToContinue('After masking'); {$EndIf};

             for i := dmxFirstPoint to NumPtDEMs do TrimDEM(PointDEMs[i]);
             for i := dmxFirstArea to NumAreaDEMs do TrimDEM(AreaDEMs[i]);
             {$IfDef ShowOpenBothPixelIsDEMs} if OpenMaps then wmdem.Tile; MessageToContinue('After trimming'); {$EndIf};
          end;
       end;

   procedure OpenNeoMode;
   var
      DataDir : PathStr;
   begin
      {$If Defined(RecordDEMIXneo)} WriteLineToDebugFile('Neo OpenBothPixelIsDEMs in, currently DEMs open=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf};
      GISdb[NewFormatDEMIXDB].ApplyGISfilter('AREA=' + QuotedStr(Area));
      if (GISdb[NewFormatDEMIXdb].MyData.FiltRecsInDB = 1) then begin
         fName := GISdb[NewFormatDEMIXdb].MyData.GetFieldByNameAsString('REF_DTM');
         if (fName = '') then begin
            Result := false;
            exit;
         end
         else begin
            PointDEMs[0] := OpenNewDEM(fName,OpenMaps);
            DataDir := ExtractFilePath(fName);
            fName := GISdb[NewFormatDEMIXdb].MyData.GetFieldByNameAsString('DTM');
            PointDEMs[1] := OpenNewDEM(fName,OpenMaps);
            DEMglb[PointDEMs[1]].AreaName := 'NEODTM';
            fName := StringReplace(fName, 'DTM', 'DSM',[rfIgnoreCase,rfReplaceAll]);
            PointDEMs[2] := OpenNewDEM(fName,OpenMaps);
            DEMglb[PointDEMs[2]].AreaName := 'NEODSM';
            fName := DataDir + 'COP.tif';
            PointDEMs[3] := OpenNewDEM(fName,OpenMaps);
            fName := DataDir + 'ALOS.tif';
            PointDEMs[4] := OpenNewDEM(fName,OpenMaps);
            fName := DataDir + 'FABDEM.tif';
            PointDEMs[5] := OpenNewDEM(fName,OpenMaps);
            NumPtDEMs := 5;
            NumAreaDEMs := 0;
            {$If Defined(RecordDEMIXneo)} ShowDEMIXgrids('OpenBothPixelIsDEMs ' + Area, PointDEMs,AreaDEMs); {$EndIf};
         end;
      end
      else begin
         Result := false;
      end;
   end;


var
   DataDir : PathStr;
   ThisDEM : shortstring;
   DEMList : tStringList;
begin {function OpenBothPixelIsDEMs}
    {$IfDef ShowOpenBothPixelIsDEMs} OpenMaps := true; {$EndIf};
    {$If Defined(RecordDEMIXopenGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
    Result := true;
    InitializePointAndAreaGrids(PointDEMs,AreaDEMs);

    if (DEMIXanalysismode = DEMIXtraditional) then begin;
       DEMList := GetListOfTestDEMsinUse('SRTM');
       NumPtDEMs := DEMList.Count;

       if (NumPtDEMs >= 1) then begin
         TryToOpen(RefDir + Prefix + area + '_dtm' + Ref1SecPointStr + Ext,PointDEMs[0]);
         if ValidDEM(PointDEMs[0]) and (DEMGlb[PointDEMs[0]].DEMSWcornerLat > 49.999) then begin
            //with 1 sec open, see if 1.5 sec needed
            TryToOpen(RefDir + Prefix + area + '_dtm' + Ref1_5SecPointStr + Ext,PointDEMs[-1]);
         end;
         {$If Defined(RecordDEMIXopenGrids)} WriteLineToDebugFile('Start load point test DEMs=' + IntToStr(NumPtDEMs)); {$EndIf}
         for i := 1 to DEMList.Count do begin
            ThisDEM := DEMlist[pred(i)];
            if (ThisDEM = 'DELTA') then DataDir := DEMIX_Under_ref_dtm
            else DataDir := TestDir;
            fName := DataDir + Prefix + Area + '_' + ThisDEM + Ext;
            TryToOpen(fName,PointDEMs[i]);
            if ValidDEM(PointDEMs[i]) then begin
               if (DEMGlb[PointDEMs[i]].GridCornerModel <> 'SRTM') then MessageToContinue(DEMGlb[PointDEMs[i]].AreaName + ' not geometric model SRTM');
            end
            else begin
               {$If Defined(RecordDEMIXopenGrids)} HighLightLineToDebugFile('Failure opening ' + fName); {$EndIf}
            end;
         end;
         if ValidDEM(PointDEMs[-1]) then dmxFirstPoint := -1 else dmxFirstPoint := 0;
         {$If Defined(RecordDEMIXopenGrids)} WriteLineToDebugFile('Point DEMs=' + IntToStr(NumPtDEMs)); {$EndIf}
       end;

       DEMList := GetListOfTestDEMsinUse('ALOS');
       NumAreaDEMs := DEMList.Count;
       if (NumAreaDEMs >= 1) then begin
         TryToOpen(RefDir + Prefix + Area + '_dtm' + Ref1SecAreaStr + Ext,AreaDEMs[0]);
         for i := 1 to NumAreaDEMs do begin
            ThisDEM := DEMlist[pred(i)];
            if (ThisDEM = 'COAST') or (ThisDEM = 'DILUV') then DataDir := DEMIX_Under_ref_dtm
            else DataDir := TestDir;
            TryToOpen(DataDir + Prefix + Area + '_' + ThisDEM + Ext,AreaDEMs[i]);
            if ValidDEM(AreaDEMs[i]) and (DEMGlb[AreaDEMs[i]].GridCornerModel <> 'ALOS') then
               MessageToContinue(DEMGlb[AreaDEMs[i]].AreaName + ' not geometric model ALOS');
         end;
         if ValidDEM(AreaDEMs[-1]) then dmxFirstArea := -1 else dmxFirstArea := 0;  //this will not happen yet since we have not used high-lat ALOS
         {$If Defined(RecordDEMIXopenGrids)} WriteLineToDebugFile('Area DEMs=' + IntToStr(NumAreaDEMs)); {$EndIf}
       end;

      {$IfDef ShowOpenBothPixelIsDEMs} if OpenMaps then wmdem.Tile; MessageToContinue('After loading'); {$EndIf};

       if MDdef.DEMIX_AllowCoastal and (MDDef.DEMIX_mode in [dmU80,dmU10,dmU120]) then begin
          MaskAndTrimCoastalDEMs;
       end;

       Result := true;
       {$If Defined(RecordDEMIXopenGrids)} WriteLineToDebugFile('OpenBothPixelIsDEMs for ' + Area + '  ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}

       {$IfDef TrackPixelIs} ShowDEMIXgrids('OpenBothPixelIsDEMs', PointDEMs,AreaDEMs); {$EndIf};
       {$IfDef ShowOpenBothPixelIsDEMs} if OpenMaps then wmdem.Tile; MessageToContinue('Check maps'); {$EndIf};
   end
   else if (DEMIXanalysismode = DEMIXneo) then begin
      OpenNeoMode;
   end
   else begin
      Result := false;
      MessageToContinue('Invalid DEMIXanalysismode');
   end;
end {function OpenBothPixelIsDEMs};


procedure OpenDEMIXAreaMaps;
var
   Areas : tStringList;
   AreaName : shortstring;
   i : integer;
begin
   try
      {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('OpenDEMIXAreaMaps in; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + ' OpenMaps=' + IntToStr(NumOpenMaps)); {$EndIf}
      GetDEMIXPaths;
      SkipMenuUpdating := false;
      Areas := DEMIX_AreasWanted;
      for I := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,true);
         LoadLandcoverForDEMIXarea(AreaName);
         {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('Opened ' + Areas.Strings[i] + ' Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + ' OpenMaps=' + IntToStr(NumOpenMaps)); {$EndIf}
      end;
   finally
      {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('OpenDEMIXAreaMaps out; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + ' OpenMaps=' + IntToStr(NumOpenMaps)); {$EndIf}
      Areas.Destroy;
      EndDEMIXProcessing;
   end;
end;


procedure CompareConvergenceIndex(AreaName : ShortString);


      procedure TryToOpen(fName : PathStr; NewName : shortstring);
      var
         DEM : integer;
      begin
         DEM := OpenNewDEM(fName);
         if ValidDEM(DEM) then begin
            DEMglb[DEM].AreaName := NewName;
            DEMglb[DEM].SelectionMap.Caption := NewName;
         end;
      end;

begin
   GetDEMIXpaths(false);
   TryToOpen(wbt_out_ref_dir + 'CONIN_' + AreaName + '_dtm' + Ref1SecPointStr + '.tif', 'wbt_ref_conin_'+ AreaName);
   TryToOpen(wbt_out_test_dir + 'CONIN_' + AreaName + '_cop.tif', 'wbt_COP_conin_'+ AreaName);
   TryToOpen(saga_out_ref_dir + 'CONIN_' + AreaName + '_dtm' + Ref1SecPointStr + '.tif','saga_ref_conin_'+ AreaName);
   TryToOpen(saga_out_test_dir + 'CONIN_' + AreaName + '_cop.tif', 'saga_COP_conin_'+ AreaName);
end;


procedure OpenCopDEMandLandcoverForArea(CopLand : boolean = true);
var
   Areas : tStringList;
   i : integer;
begin
   try
      GetDEMIXPaths;
      SkipMenuUpdating := false;
      Areas := DEMIX_AreasWanted;
      for I := 0 to pred(Areas.Count) do begin
         if CopLand then LoadCopAndLancoverForDEMIXTile(Areas.Strings[i])
         else CompareConvergenceIndex(Areas.Strings[i]);
      end;
   finally
      Areas.Destroy;
      EndDEMIXProcessing;
   end;
end;


function LoadDEMIXCandidateDEMs(AreaName : ShortString; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
var
   i,Ser : integer;
   fName : Pathstr;
begin
   {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('LoadDEMIXCandidateDEMs in; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
   Result := false;
   //LoadDEMIXnames;
   for I := 1 to MaxDEMIXDEM do begin
      TestDEMs[i] := 0;
      TestSeries[i] := '';
   end;
   Ser := 0;
   for I := 1 to NumDEMIXtestDEM do begin
      fName := DEMIX_test_dems + AreaName + '_' + DEMIXShort[i] + '.tif';
      if FileExists(fname) then begin
         inc(Ser);
         TestDEMs[Ser] := OpenNewDEM(fName,OpenMaps);
         TestSeries[Ser] := DEMIXShort[i];
      end
      else begin
         {$If Defined(RecordDEMIXFull)} HighlightLineToDebugFile('Missing test DEM= ' + fName); {$EndIf}
      end;
   end;

   Result := (Ser = NumDEMIXtestDEM);
   {$If Defined(RecordDEMIXLoad)} if not Result then writeLineToDebugFile('Reload LoadDEMIXCandidateDEMs in; Loaded only DEMs=, ' + IntToStr(Ser)); {$EndIf}
end;



function LoadDEMIXReferenceDEMs(AreaName : shortstring; var RefDEM : integer; OpenMaps : boolean = true) : boolean;
var
   NumRefDEMs : integer;

         procedure ReferenceFileOpen(var DEM : integer; fName : PathStr; What : shortString = '');
         begin
            if (fName <> '') then begin
               if FileExists(fName) then begin
                  DEM := OpenNewDEM(FName,OpenMaps);  //must load map for DEMIX tile computation
                  if ValidDEM(DEM) then begin
                     if (RefDEM = 0) then RefDEM := DEM;
                     inc(NumRefDEMs);
                     RefDEMs[NumRefDEMs] := DEM;
                     {$If Defined(RecordDEMIXRefLoad)} writeLineToDebugFile('RefDEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
                  end;
               end
               else begin
                  DEM := 0;
                  {$If Defined(RecordDEMIX)} writeLineToDebugFile(What + ' ReferenceFileOpen missing=' + fName); {$EndIf}
               end;
            end
            else DEM := 0;
         end;

begin
   {$If Defined(RecordDEMIX)} writeLineToDebugFile('ProcessDEMIXtestarea in, open DEMs=' + IntToStr(NumDEMDataSetsOpen) + '  AreaName=' + AreaName); {$EndIf}
   RefDEM := 0;
   NumRefDEMs := 0;
   GetAreaDEMNames(AreaName);
   {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('LoadDEMIXReferenceDEMs need DTM_point=' + RefDTMpointFName +'  DTM_area=' + RefDTMareaFName); {$EndIf}

   ReferenceFileOpen(RefDTMpoint,RefDTMpointFName,'Ref DTM Point ');
   ReferenceFileOpen(RefDTMarea,RefDTMareaFName,'Ref DTM area ');
   ReferenceFileOpen(COPRefDTM,COPRefDTMFName,'Ref DTM COP ');
   {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('ProcessDEMIXtestarea, open ref DTMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
   if MDDef.DEMIX_open_ref_DSM and ValidDEM(RefDTMpoint) then begin
      {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('ProcessDEMIXtestarea start DSM, open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
      ReferenceFileOpen(RefDSMpoint,RefDSMpointFName);
      ReferenceFileOpen(RefDSMarea,RefDSMareaFName);
      ReferenceFileOpen(COPRefDSM,COPRefDSMFName);
   end;
   Result := ValidDEM(RefDEM);
   if Result then begin
      {$If Defined(RecordDEMIXload)} writeLineToDebugFile('ProcessDEMIXtestarea out, ref DEMs open with RefDEM=' + IntToStr(RefDEM) + ' and open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
   end
   else begin
      {$IfDef RecordDEMIXload} HighlightLineToDebugFile('Failure, to open ref DEMs'); {$EndIf}
   end;
end;



function LoadLandcoverForDEMIXarea(AreaName : shortstring; OpenMap : boolean = true) : integer;
var
   fName : PathStr;
begin
   fName := DEMIX_area_lc100 + AreaName + '.tif';
   if FileExists(fname) then begin
      Result := OpenNewDEM(fName,OpenMap);
      DEMglb[Result].AreaName := 'LC100_' + AreaName;
      if OpenMap then DEMglb[Result].SelectionMap.Caption := DEMglb[Result].AreaName;

      {$If Defined(Rec_DEMIX_Landcover)} WriteLineToDebugFile('Land cover loaded ' + fName); {$EndIf}
   end
   else begin
      {$If Defined(Rec_DEMIX_Landcover)} WriteLineToDebugFile('LoadLandcoverForDEMIXarea, but missing ' + fName); {$EndIf}
      Result := 0;
   end;
end;

procedure LoadCopAndLancoverForDEMIXTile(AreaName : shortstring; TileName : shortstring = '');
var
   bb : sfBoundBox;
   grid : integer;

   procedure LoadAndZoom(fName : PathStr);
   begin
      if FileExists(fName) then begin
         grid := OpenNewDEM(fName);
         if ValidDEM(grid) and (TileName <> '') then DEMglb[grid].SelectionMap.SubsetAndZoomMapFromGeographicBounds(bb);
      end;
   end;

begin
   GetDEMIXpaths(false);
   if (TileName <> '') then bb := DEMIXtileBoundingBox(TileName);
   LoadAndZoom(DEMIX_test_dems + AreaName + '_cop.tif');
   grid := LoadLandcoverForDEMIXarea(AreaName);
   if ValidDEM(grid) and (TileName <> '') then DEMglb[grid].SelectionMap.SubsetAndZoomMapFromGeographicBounds(bb);
end;


procedure LoadThisDEMIXTile(AreaName : shortstring; TileName : shortstring; OpenMaps : boolean = true);
var
   i : integer;
   TileDir,fName : PathStr;
   Grids : tStringList;
   DEMs : array[0..25] of integer;
begin
   Grids := Nil;
   TileDir := MDDef.DEMIX_BaseDir + AreaName + '\' + TileName + '_ref_test_dem\';
   FindMatchingFiles(TileDir,'*.tif',Grids,0);
   if (Grids.Count = 0) then begin
      GetDOSPath('DEMIX base directory',MDDef.DEMIX_BaseDir);
      TileDir := MDDef.DEMIX_BaseDir + AreaName + '\' + TileName + '_ref_test_dem\';
      FindMatchingFiles(TileDir,'*.tif',Grids,0);
   end;

   for i := 0 to pred(Grids.Count) do begin
      DEMs[i] := 0;
      fName := Grids.Strings[i];
      LoadNewDEM(DEMs[i],fName,true);
   end;
   Grids.Destroy;
end;




