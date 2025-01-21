{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{ include file for demix_definitions }
{____________________________________}



procedure TilesInEachElevRangeForTestAreas;
//creates table with tiles in each for for FULL, U120, U80, and U10
//deletes and DEMs with no tiles
var
   Results,Areas : tStringList;
   AreaName,aLine : Shortstring;
   //NumPts : int64;
   fName : PathStr;
   Lat,Long : float64;
   //Local : boolean;
   i,nt,NewDEM : Integer;

         procedure CheckSeries(fName : PathStr);
         begin
            if FileExists(fName) then begin
               NewDEM := OpenNewDEM(fName,false);
               if ValidDEM(NewDEM) then begin
                  nt := NumFilledDEMIXtilesOnDEM(NewDEM);
                  if (nt = 0) then begin
                     File2Trash(fName);
                  end;
               end
               else nt := 0;
               if (aLine = '') then begin
                  DEMGlb[NewDEM].DEMCenterPoint(Lat,Long);
                  aLine := AreaName + ',' + RealToString(Lat,-12,4) +  ',' + RealToString(Long,-12,4);
               end;
               aLine := aline + ',' + IntToStr(nt);
               CloseSingleDEM(NewDEM);
            end
            else aLine := aline + ',0';
         end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TilesInEachElevRangeForTestAreas in'); {$EndIf}
   try
      GetDEMIXpaths(true);
      SafeMakeDir(DEMIX_Under_ref_dtm);
      Areas := DEMIX_AreasWanted;
      Results := tStringList.Create;
      Results.Add('AREA,LAT,LONG,FULL_TILES,U120_TILES,U80_TILES,U10_TILES');
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('Start area: ' + AreaName); {$EndIf}
         wmdem.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' +  AreaName);
         aLine := '';
         CheckSeries(DEMIX_Ref_1sec + AreaName + '_dtm' + Ref1SecPointStr + '.tif');
         CheckSeries(DEMIX_coastal_dtms + AreaName + '_COAST' + '.tif');
         CheckSeries(DEMIX_diluvium_dtms + AreaName + '_DILUV' + '.tif');
         CheckSeries(DEMIX_delta_dtms + AreaName + '_DELTA' + '.tif');
         Results.Add(aLine);
      end;
      if (Results.Count > 1) then begin
         fName := NextFileNumber(MDTempDir,'tiles_in_areas_and_series','.dbf');
         PetdbUtils.StringList2CSVtoDB(Results,fName);
      end
      else begin
         Results.Destroy;
         {$If Defined(RecordDiluvium) or Defined(RecordDEMIX)} WriteLineToDebugFile('No Areas with filled DEMIX tiles'); {$EndIf}
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TilesInEachElevRangeForTestAreas out'); {$EndIf}
end;




procedure AllHallucinatingDTMsforCoastalAreas(Overwrite : boolean);
var
   Areas : tStringList;
begin
   GetDEMIXpaths(true);
   Areas := DEMIX_AreasWanted;
   Overwrite := true;

   //must go in this order, as the Areas list will get progressively smaller
   CoastalDTMforTestAreas(Overwrite,Areas);
   DiluviumDTMforTestAreas(Overwrite,Areas);
   DeltaDTMforTestAreas(Overwrite,Areas);
end;

procedure RemoveHighDEMs(var Areas : tStringList; ElevMin : float32);
var
   Table : tMyData;
   fName : PathStr;
   i : integer;
   Area : shortstring;
   AreaMin : float32;
begin
   fName := DEMIXSettingsDir + 'DEMIX_area_statistics.dbf';
   Table := tMyData.Create(fName);
   for i := pred(Areas.Count) downto 0 do begin
      Area := Areas.Strings[i];
      Table.ApplyFilter('AREA=' + QuotedStr(Area));
      AreaMin := Table.GetFieldByNameAsFloat('MIN_ELEV');
      if (AreaMin > ElevMin) then begin
         Areas.Delete(i);
      end;
   end;
   Table.Destroy;
end;

procedure CoastalDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
var
   TheFiles,nt,Results : tStringList;
   AreaName,TStr : Shortstring;
   fName,SaveName : PathStr;
   Lat,Long : float64;
   Local : boolean;
   i,RefDEM,NewDEM,WantImage : Integer;
begin
   {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('CoastalDEMforTestAreas in'); {$EndIf}
   try
      GetDEMIXpaths(true);
      SafeMakeDir(DEMIX_coastal_dtms);
      Local := (Areas = Nil);
      if Local then Areas := DEMIX_AreasWanted;
      RemoveHighDEMs(Areas,120);
      Results := tStringList.Create;
      Results.Add('AREA,LAT,LONG,NUM_TILES');
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('Start area: ' + AreaName); {$EndIf}
         wmdem.SetPanelText(2,'Coastal ' + IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' +  AreaName);
         SaveName := DEMIX_coastal_dtms + AreaName + '_COAST' + '.tif';
         if (not FileExists(SaveName)) or Overwrite then begin
            fName := DEMIX_Ref_1sec + AreaName + '_dtm' + Ref1SecPointStr + '.tif';
            RefDEM := OpenNewDEM(fName,false);
            if ValidDEM(RefDEM) and (DEMGlb[RefDEM].DEMHeader.MinElev < 120) then begin
               {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('RefDEM=' + DEMGlb[RefDEM].AreaName); {$EndIf}
               NewDEM := LoadMapLibraryBox(true,DEMGlb[RefDEM].DEMBoundBoxGeo,'COAST',false);
               if ValidDEM(NewDEM) then begin
                  DEMGlb[NewDEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
                  {$If Defined(RecordDiluviumFull)} WriteLineToDebugFile('Loaded: ' + DEMGlb[NewDEM].KeyParams(true)); {$EndIf}
                  EditsDone := 0;
                  MaskStripFromSecondGrid(NewDEM,RefDEM,msSecondMissing);
                  nt := DEMIXtilesOnDEM(NewDEM);
                  if (nt.Count = 0) then begin
                     {$If Defined(RecordDiluviumFull)} WriteLineToDebugFile('No full DEMIX tiles in ' + AreaName); {$EndIf}
                  end
                  else begin
                     DEMGlb[NewDEM].DEMCenterPoint(Lat,Long);
                     TStr := AreaName + ',' + RealToString(Lat,-12,4) +  ',' + RealToString(Long,-12,4) + ',' + IntToStr(nt.Count);
                     Results.Add(AreaName);
                     {$If Defined(RecordDiluviumFull)} WriteLineToDebugFile(TStr); {$EndIf}
                     DEMGlb[NewDEM].SavePartOfDEMWithDataGeotiff(SaveName);
                  end;
                  CloseSingleDEM(NewDEM);
                  nt.Destroy;
               end
               else begin
                  {$If Defined(RecordDiluvium)} WriteLineToDebugFile('No coastal DEM for ' + AreaName); {$EndIf}
               end;
            end;
            CloseSingleDEM(RefDEM);
         end;
      end;
      if Local then Areas.Free;
      if (Results.Count > 1) then begin
         fName := NextFileNumber(MDTempDir,'coast_dem_status','.dbf');
         PetdbUtils.StringList2CSVtoDB(Results,fName);
      end
      else begin
         Results.Destroy;
         {$If Defined(RecordDiluvium) or Defined(RecordDEMIX)} WriteLineToDebugFile('No Areas with filled COASTAL tiles'); {$EndIf}
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CoastalDEMforTestAreas out'); {$EndIf}
end;


procedure DiluviumDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
var
   TheFiles,nt,Results : tStringList;
   AreaName,TStr : Shortstring;
   fName,SaveName : PathStr;
   Lat,Long : float64;
   Local : boolean;
   i,RefDEM,NewDEM : Integer;
begin
   {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('DiluviumDEMforTestAreas in'); {$EndIf}
   try
      GetDEMIXpaths(true);
      SafeMakeDir(DEMIX_diluvium_dtms);

      Local := (Areas = Nil);
      if Local then Areas := DEMIX_AreasWanted;
      RemoveHighDEMs(Areas,80);

      Results := tStringList.Create;
      Results.Add('AREA,LAT,LONG,NUM_TILES');
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('Start area: ' + AreaName); {$EndIf}
         wmdem.SetPanelText(2,'Diluvium ' + IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' +  AreaName);
         SaveName := DEMIX_diluvium_dtms + AreaName + '_DILUV' + '.tif';
         if (not FileExists(SaveName)) or Overwrite then begin
            fName := DEMIX_Ref_1sec + AreaName + '_dtm' + Ref1SecPointStr + '.tif';
            RefDEM := OpenNewDEM(fName,false);
            if ValidDEM(RefDEM) and (DEMGlb[RefDEM].DEMHeader.MinElev < 80) then begin
               {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('RefDEM=' + DEMGlb[RefDEM].AreaName); {$EndIf}
               NewDEM := LoadMapLibraryBox(true,DEMGlb[RefDEM].DEMBoundBoxGeo,'DILUV',false);
               if ValidDEM(NewDEM) then begin
                  DEMGlb[NewDEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
                  {$If Defined(RecordDiluviumFull)} WriteLineToDebugFile('Loaded: ' + DEMGlb[NewDEM].KeyParams(true)); {$EndIf}
                  nt := DEMIXtilesOnDEM(NewDEM);
                  if (nt.Count = 0) then begin
                     {$If Defined(RecordDiluviumFull)} WriteLineToDebugFile('No full DEMIX tiles in ' + AreaName); {$EndIf}
                  end
                  else begin;
                     EditsDone := 0;
                     MaskStripFromSecondGrid(NewDEM,RefDEM,msSecondMissing);
                     DEMGlb[NewDEM].DEMCenterPoint(Lat,Long);
                     TStr := AreaName + ',' + RealToString(Lat,-12,4) +  ',' + RealToString(Long,-12,4) + ',' + IntToStr(nt.Count);
                     Results.Add(AreaName);
                     {$If Defined(RecordDiluviumFull)} WriteLineToDebugFile(TStr); {$EndIf}
                     DEMGlb[NewDEM].SavePartOfDEMWithDataGeotiff(SaveName);
                  end;
                  CloseSingleDEM(NewDEM);
                  nt.Destroy;
               end
               else begin
                  {$If Defined(RecordDiluvium)} WriteLineToDebugFile('No diluvium DEM for ' + AreaName); {$EndIf}
               end;
            end;
            CloseSingleDEM(RefDEM);
         end;
      end;
      if Local then Areas.Free;
      if (Results.Count > 1) then begin
         fName := NextFileNumber(MDTempDir,'diluv_dem_status','.dbf');
         PetdbUtils.StringList2CSVtoDB(Results,fName);
      end
      else begin
         Results.Destroy;
         {$If Defined(RecordDiluvium) or Defined(RecordDEMIX)} WriteLineToDebugFile('No Areas with filled DEMIX tiles'); {$EndIf}
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DiluviumDEMforTestAreas out'); {$EndIf}
end;



procedure DeltaDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
var
   TheFiles,nt,Results: tStringList;
   AreaName,TStr : Shortstring;
   NumPts : int64;
   fName,SaveName : PathStr;
   Lat,Long : float64;
   Local : boolean;
   i,RefDEM,NewDEM : Integer;
begin
   {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('DeltaDTMforTestAreas in'); {$EndIf}
   try
      GetDEMIXpaths(true);
      SafeMakeDir(DEMIX_delta_dtms);
      Local := (Areas = Nil);
      if Local then Areas := DEMIX_AreasWanted;
      RemoveHighDEMs(Areas,10);
      Results := tStringList.Create;
      Results.Add('AREA,LAT,LONG,NUM_TILES');
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('Start area: ' + AreaName); {$EndIf}
         wmdem.SetPanelText(2,'Delta ' + IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' +  AreaName);
         SaveName := DEMIX_delta_dtms + AreaName + '_DELTA' + '.tif';
         if (not FileExists(SaveName)) or Overwrite then begin
            fName := DEMIX_Ref_1sec + AreaName + '_dtm' + Ref1SecPointStr + '.tif';
            RefDEM := OpenNewDEM(fName,false);
            if ValidDEM(RefDEM) and (DEMGlb[RefDEM].DEMHeader.MinElev < 10) then begin
               {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('RefDEM=' + DEMGlb[RefDEM].AreaName); {$EndIf}
               NewDEM := LoadMapLibraryBox(true,DEMGlb[RefDEM].DEMBoundBoxGeo,'DELTA',false);
               if ValidDEM(NewDEM) then begin
                  DEMGlb[NewDEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
                  DEMGlb[NewDEM].MarkAboveMissing(9.9999,NumPts);
                  {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Removed 10 m points: ' + IntToStr(NumPts)); {$EndIf}
                  {$If Defined(RecordDiluviumFull)} WriteLineToDebugFile('Loaded: ' + DEMGlb[NewDEM].KeyParams(true)); {$EndIf}
                  EditsDone := 0;
                  MaskStripFromSecondGrid(NewDEM,RefDEM,msSecondMissing);
                  nt := DEMIXtilesOnDEM(NewDEM);
                  if (nt.Count = 0) then begin
                     {$If Defined(RecordDiluviumFull)} WriteLineToDebugFile('No full DEMIX tiles in ' + AreaName); {$EndIf}
                  end
                  else begin;
                     DEMGlb[NewDEM].DEMCenterPoint(Lat,Long);
                     TStr := AreaName + ',' + RealToString(Lat,-12,4) +  ',' + RealToString(Long,-12,4) +  IntToStr(nt.Count);
                     Results.Add(AreaName);
                     {$If Defined(RecordDiluviumFull)} WriteLineToDebugFile(TStr); {$EndIf}
                     DEMGlb[NewDEM].SavePartOfDEMWithDataGeotiff(SaveName);
                  end;
                  nt.Destroy;
                  CloseSingleDEM(NewDEM);
               end
               else begin
                  {$If Defined(RecordDiluvium)} WriteLineToDebugFile('No DeltaDTMforTestAreas for ' + AreaName); {$EndIf}
               end;
            end;
            CloseSingleDEM(RefDEM);
         end;
      end;
      if Local then Areas.Free;
      if (Results.Count > 1) then begin
         fName := NextFileNumber(MDTempDir,'delta_dtm_status','.dbf');
         PetdbUtils.StringList2CSVtoDB(Results,fName);
      end
      else begin
         Results.Destroy;
         {$If Defined(RecordDiluvium) or Defined(RecordDEMIX)} WriteLineToDebugFile('No Areas with filled DEMIX tiles'); {$EndIf}
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DeltaDTMforTestAreas out'); {$EndIf}
end;



function CreateDEMIXTestDEMsForArea(Overwrite : boolean; AreaName : ShortString; AreaRefDEM,PointRefDEM : integer) : boolean;
var
   {$IfDef RecordDEMIX} AllDEMs, {$EndIf}
   WantSeries,ShortName : shortstring;
   IndexSeriesTable : tMyData;
   NumPts : int64;
   WantDEM,Ser,i,GeoidGrid,aRefDEM : integer;
   fName,SaveName : Pathstr;


         procedure MoveFromEGM96toEGM2008(var DEM : integer);
         //Reproject vertical datum to EGM2008 if required because DEM is EGM96
         var
           Col,Row,NewDEM : integer;
           z,z2 : float32;
           Lat,Long : float64;
         begin
            {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift in, DEM=' + IntToStr(DEM)  + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            if ValidDEM(DEM) {and (DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey = VertCSEGM96)} then begin
               if not ValidDEM(GeoidGrid) then begin
                  GeoidGrid := OpenNewDEM(GeoidDiffFName,false,'geoid difference from EGM96 to EGM2008');  //to move DEMs from EGM96 to EGM2008
                  GeoidDiffFName := DEMGlb[GeoidGrid].DEMFileName;
               end;

               NewDEM := DEMGlb[DEM].ResaveNewResolution(fcSaveFloatingPoint); //have to resave because input DEMs are all integer resolution
               DEMGlb[NewDEM].AreaName := DEMGlb[DEM].AreaName;  // + '_egm2008';
               DEMGlb[NewDEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
               {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift with shift ' + DEMGlb[DEM].AreaName); {$EndIf}
               z2 := 0;
               for Col := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumCol) do begin
                  for Row := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumRow) do begin
                      if DEMGlb[NewDEM].GetElevMetersOnGrid(Col,Row,z) then begin
                         DEMGlb[NewDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                         if DEMGlb[GeoidGrid].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                            DEMGlb[NewDEM].SetGridElevation(Col,Row,z+z2);
                         end;
                      end;
                  end;
               end;
               CloseSingleDEM(DEM);
               {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('Closed DEM; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               DEMGlb[NewDEM].CheckMaxMinElev;
               DEM := NewDEM;
               {$IfDef RecordDEMIXVDatum} WriteLineToDebugFile('CheckVerticalDatumShift out, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            end
            else begin
               {$IfDef RecordDEMIXVDatum} WriteLineToDebugFile('CheckVerticalDatumShift out, not EGM96, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            end;
         end;


begin {CreateDEMIXTestDEMsForArea}
   {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('CreateDEMIXCandidateDEMs in; AreaName=' + AreaName + '  Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
   Result := false;
   if (ValidDEM(AreaRefDEM) and ValidDEM(PointRefDEM)) then begin
      {$IfDef RecordDEMIX} AllDEMs := ''; {$EndIf}

      for I := 1 to MaxDEMIXDEM do begin
         TestDEMs[i] := 0;
         TestSeries[i] := '';
      end;
      GeoidGrid := 0;

      Ser := 0;
      for I := 1 to NumDEMIXtestDEM do begin
         WantSeries := DEMIXDEMTypeName[i];
         ShortName := DEMIXShort[i];
         SaveName := DEMIX_test_dems + AreaName + '_' + shortname + '.tif';
         wmdem.SetPanelText(3,'Load candidate DEM ' + ShortName + '  ' + IntToStr(i) + '/' + IntToStr(NumDEMIXtestDEM));
         if Overwrite or (not FileExists(SaveName)) then begin
            {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoad)} writeLineToDebugFile('Try ' + WantSeries + ' ' + ShortName + '  ' + IntToStr(i) + '/' + IntToStr(NumDEMIXDEM)); {$EndIf}
            {$If Defined(RecordFullDEMIX)} WriteLineRoDebugFile('Ref DEM=' + DEMGlb[PointRefDEM].AreaName + '  ' + sfBoundBoxToString(DEMGlb[PointRefDEM].DEMBoundBoxGeo,6)); {$EndIf}
            WantDEM := LoadMapLibraryBox(true,DEMGlb[PointRefDEM].DEMBoundBoxGeo,WantSeries,false);
            if ValidDEM(WantDEM) then begin
               {$If Defined(RecordDEMIXLoad)} WriteLineRoDebugFile('CreateDEMIXCandidateDEMs done LoadMapLib; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               {$If Defined(TrackPixelIs)} WriteLineRoDebugFile('Loaded from Map Library, ' + DEMglb[WantedDEM].AreaName + '  ' + GridCornerModelAndPixelIsString); {$EndIf}
               inc(Ser);
               TestDEMs[Ser] := WantDEM;
               TestSeries[Ser] := ShortName;
               if (ShortName = 'ALOS') then aRefDEM := AreaRefDEM else aRefDEM := PointRefDEM;

               {$IfDef RecordDEMIX}
                  if not AllOfBoxInAnotherBox(DEMGlb[aRefDEM].DEMBoundBoxGeo,DEMGlb[WantDEM].DEMBoundBoxGeo) then begin
                     AllDEMs := AllDEMs + TestSeries[Ser] + ' (partial ' + sfBoundBoxToString(DEMGlb[WantDEM].DEMBoundBoxGeo) + ' and need ' + sfBoundBoxToString(DEMGlb[aRefDEM].DEMBoundBoxGeo) + ')';
                  end;
               {$EndIf}
               DEMGlb[TestDEMs[Ser]].AreaName := TestSeries[Ser];
               DEMGlb[TestDEMs[Ser]].DEMFileName := SaveName;   //NextFileNumber(MDTempDir, DEMGlb[TestDEMs[Ser]].AreaName + '_', '.dem');

               {$IfDef RecordDEMIXLoad} WriteLineRoDebugFile('Opened:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               if (ShortName = 'COP') or (ShortName = 'TANDEM') or (ShortName = 'FABDEM') or (ShortName = 'DILUV') then DEMGlb[TestDEMs[Ser]].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008
               else begin
                  MoveFromEGM96toEGM2008(TestDEMs[Ser]);
                  {$If Defined(TrackPixelIs) or Defined(RecordDEMIXLoad)} WriteLineRoDebugFile('Moved to EGM2008, ' + DEMglb[WantedDEM].AreaName + '  ' + GridCornerModelAndPixelIsString); {$EndIf}
               end;
               EditsDone := 0;
               MaskStripFromSecondGrid(TestDEMs[Ser],aRefDEM, msSecondMissing);
               DEMGlb[TestDEMs[Ser]].SavePartOfDEMWithDataGeotiff(SaveName);
               {$If Defined(TrackPixelIs)} WriteLineRoDebugFile('Masked and saved, ' + DEMglb[WantedDEM].AreaName + '  ' + GridCornerModelAndPixelIsString); {$EndIf}
               Result := true;
            end
            else begin
               {$IfDef RecordDEMIX} AllDEMs := AllDEMs + WantSeries + ' (missing) '; {$EndIf}
            end;
         end;
      end;
      CloseSingleDEM(GeoidGrid);
      {$IfDef RecordDEMIX} if (AllDEMs <> '') then HighlightLineToDebugFile(AreaName + ' DEM problem, ' + AllDEMs); {$EndIf}
      {$IfDef RecordDEMIXLoad} WriteLineRoDebugFile('CreateDEMIXCandidateDEMs out; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
//end;
   end
   else begin
      {$IfDef RecordDEMIX} HighlightLineToDebugFile(AreaName + ' CreateDEMIXCandidateDEMs fails, no reference DEMs'); {$EndIf}
   end;
end {CreateDEMIXTestDEMsForArea};


procedure CreateTestAreaDEMs(Overwrite : boolean);
var
   i,RefDEM : integer;
   Areas : tStringList;
   AreaName : shortstring;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateTestAreaDEMs in, dir=' + DEMIX_test_dems); {$EndIf}
   try
      GetDEMIXPaths(true);
      SafeMakeDir(DEMIX_test_dems);
      Areas := DEMIX_AreasWanted;
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + AreaName);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Call LoadDEMIXReferenceDEMs for ' + AreaName); {$EndIf}
         LoadDEMIXReferenceDEMs(AreaName,RefDEM,false);
         if ValidDEM(RefDTMarea) and ValidDEM(RefDTMpoint) then begin
             CreateDEMIXTestDEMsForArea(Overwrite,AreaName,RefDTMarea,RefDTMpoint);
         end
         else begin
            {$IfDef RecordDEMIX}
               HighlightLineToDebugFile(AreaName + ' CreateDEMIXCandidateDEMs fails, reference DEMs');
               WriteLineToDebugFile('REFDTMpoint=' + IntToStr(RefDTMpoint) + '  ' + RefDTMpointFName);
               WriteLineToDebugFile('REFDTMarea =' + IntToStr(RefDTMArea) + '  ' + RefDTMareaFName);
            {$EndIf}
         end;
         CloseAllDEMs;
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateTestAreaDEMs out'); {$EndIf}
end;

