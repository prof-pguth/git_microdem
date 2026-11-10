{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for  demix_definitions  }
{________________________________________}


procedure TilesInEachElevRangeForTestAreas;
//creates table with tiles in each for for FULL, U120, U80, and U10
//deletes and DEMs with no tiles
var
   Results,Areas : tStringList;
   AreaName,aLine : Shortstring;
   fName : PathStr;
   Lat,Long : float64;
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
      aLine := 'AREA,LAT,LONG,FULL_TILES';
      if MDdef.DEMIX_AllowCoastal then aLine := aLine + ',U120_TILES,U80_TILES,U10_TILES';
      Results.Add(aLine);
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('Start area: ' + AreaName); {$EndIf}
         wmdem.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' +  AreaName);
         aLine := '';
         CheckSeries(DEMIX_Ref_1sec + AreaName + '_dtm' + Ref1SecPointStr + '.tif');
         {$IfDef IncludeCoastalDEMs}
             if MDdef.DEMIX_AllowCoastal then begin
                 CheckSeries(DEMIX_coastal_dtms + AreaName + '_COAST' + '.tif');
                 CheckSeries(DEMIX_diluvium_dtms + AreaName + '_DILUV' + '.tif');
                 CheckSeries(DEMIX_delta_dtms + AreaName + '_DELTA' + '.tif');
             end;
         {$EndIf}
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
   nt,Results : tStringList;
   AreaName,TStr : Shortstring;
   fName,SaveName : PathStr;
   Lat,Long : float64;
   Local : boolean;
   i,RefDEM,NewDEM : Integer;
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
                  MaskGridFromSecondGrid(NewDEM,RefDEM,msSecondMissing);
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


{$IfDef IncludeCoastalDEMs}

    procedure DiluviumDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
    var
       nt,Results : tStringList;
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
                         MaskGridFromSecondGrid(NewDEM,RefDEM,msSecondMissing);
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
       nt,Results: tStringList;
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
                      MaskGridFromSecondGrid(NewDEM,RefDEM,msSecondMissing);
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
{$EndIf}




procedure CreateDEMIXTestAreaDEMs(Overwrite : boolean);

      function CreateDEMIXTestDEMsForArea(Overwrite : boolean; AreaName : ShortString; AreaRefDEM,PointRefDEM : integer) : boolean;
      var
         {$IfDef RecordDEMIX} AllDEMs, {$EndIf}
         WantSeries,ShortName : shortstring;
         WantDEM,Ser,i,GeoidGrid,aRefDEM : integer;
         SaveName : Pathstr;


               procedure MoveFromEGM96toEGM2008(var DEM : integer);
               //Reproject vertical datum to EGM2008 if required because DEM is EGM96
               var
                 Col,Row,NewDEM : integer;
                 z,z2 : float32;
                 Lat,Long : float64;
               begin
                  {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift in, DEM=' + IntToStr(DEM)  + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
                  if ValidDEM(DEM) then begin
                     if not ValidDEM(GeoidGrid) then begin
                        GeoidGrid := OpenNewDEM(GeoidDiffFName,false,'geoid difference from EGM96 to EGM2008');  //to move DEMs from EGM96 to EGM2008
                        GeoidDiffFName := DEMGlb[GeoidGrid].DEMFileName;
                     end;

                     NewDEM := DEMGlb[DEM].ResaveNewElevationPrecision(fcSaveFloatingPoint); //have to resave because input DEMs are all integer resolution
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
                  {$If Defined(RecordDEMIXFull) or Defined(RecordDEMIXLoad)} writeLineToDebugFile('Try ' + WantSeries + ' ' + ShortName + '  ' + IntToStr(i) + '/' + IntToStr(NumDEMIXDEM)); {$EndIf}
                  {$If Defined(RecordDEMIXFull)} WriteLineRoDebugFile('Ref DEM=' + DEMGlb[PointRefDEM].AreaName + '  ' + sfBoundBoxToString(DEMGlb[PointRefDEM].DEMBoundBoxGeo,6)); {$EndIf}
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
                     DEMGlb[TestDEMs[Ser]].DEMFileName := SaveName;

                     {$IfDef RecordDEMIXLoad} WriteLineRoDebugFile('Opened:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
                     if (ShortName = 'COP') or (ShortName = 'TANDEM') or (ShortName = 'FABDEM') or (ShortName = 'DILUV') then DEMGlb[TestDEMs[Ser]].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008
                     else begin
                        MoveFromEGM96toEGM2008(TestDEMs[Ser]);
                        {$If Defined(TrackPixelIs) or Defined(RecordDEMIXLoad)} WriteLineRoDebugFile('Moved to EGM2008, ' + DEMglb[WantedDEM].AreaName + '  ' + GridCornerModelAndPixelIsString); {$EndIf}
                     end;
                     EditsDone := 0;
                     MaskGridFromSecondGrid(TestDEMs[Ser],aRefDEM, msSecondMissing);
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

var
   i,RefDEM : integer;
   Areas : tStringList;
   AreaName : shortstring;
begin {procedure CreateDEMIXTestAreaDEMs}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateDEMIXTestAreaDEMs in, dir=' + DEMIX_test_dems); {$EndIf}
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
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateDEMIXTestAreaDEMs out'); {$EndIf}
end {procedure CreateDEMIXTestAreaDEMs};



procedure ComputeDEMIX_Summary_stats_DB(GeoTiles : boolean = true);
var
   MomentVar,ForestMomentVar,UrbanMomentVar,BarrenMomentVar,WaterMomentVar : tMomentVar;
   DEMIXtileDB,LandCoverGrid : integer;
   bbgrid : tGridLimits;
   fName : PathStr;
   Country,AreaName,DEMIXtile,LandTypeMask,TileHeader : shortstring;
   {$IfDef AllowAreaStats}
      AreaGridFull,AreaAveElev,AreaAveSlope,AreaAveRuff,AreaAveRelief,
      AreaForestPC, AreaUrbanPC,AreaBarrenPC,AreaWaterPC,AreaMinElev,AreaMaxElev : float64;
      AreaStats : tStringList;
   {$EndIf}
   LatCent,LongCent,
   GridFull,Lat,Long : float64;
   AreasWanted,TileStats : tStringList;
   zs : ^bfarray32;

         procedure InitializeStringLists;
         const
            TileStatsString = ',COUNTRY,LAT,LONG,GRID_FULL,AVG_ELEV,AVG_SLOPE,AVG_ROUGH,RELIEF,FOREST_PC,URBAN_PC,BARREN_PC,WATER_PC,MIN_ELEV,MAX_ELEV';
         begin
            TileStats := tStringList.Create;
            TileStats.Add('AREA,DEMIX_TILE' + TileStatsString);
            {$IfDef AllowAreaStats}
               AreaStats := tStringList.Create;
               AreaStats.Add('AREA,NUM_TILE' + TileStatsString);
            {$EndIf}
         end;


         procedure FinalizeStringLists(FinalSaveResults : boolean = true);

               function ProcessStringList(fName : PathStr; var sl : tStringList) : integer;
               begin
                  if (sl.count > 1) then begin
                     fName := DEMIXSettingsDir + fname + '.dbf';
                     DeleteFileIfExists(fName);
                     fName := ChangeFileExt(fName,'.csv');
                     DeleteFileIfExists(fName);
                     {$IfDef RecordDEMIX} HighLightInDebugFile('Summary stats written to ' + fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                     Result := StringList2CSVtoDB(sl,fName);
                  end
                  else begin
                     {$IfDef RecordDEMIX} HighLightInDebugFile('No Summary stats for ' + fName); {$EndIf}
                     fName := '';
                     sl.Destroy;
                     Result := 0;
                  end;
               end;
         var
            db : integer;
         begin
            {$IfDef TrackAverageStats} TileStats.SaveToFile(DEMIXSettingsDir + 'tilestats.csv'); {$EndIf}
            db := ProcessStringList('DEMIX_tiles_statistics',TileStats);
            if ValidDB(db) then AddPercentPrimaryData(DB);
            {$IfDef AllowAreaStats}
               ProcessStringList('DEMIX_area_statistics',AreaStats);
            {$EndIf}
         end;


   procedure ProcessDEMIXtestarea;


         function GridBoundingBox(DEM : integer) : tGridLimits;
         var
            bb : sfBoundBox;
         begin
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,Result.xgridlow,Result.ygridlow);
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,Result.xgridhigh,Result.ygridhigh);
         end;

         function UseThisTile : boolean;
         var
            bb : sfBoundBox;
         begin
            GridFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
            DEMIXtile := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            LatCent := 0.5 * (bb.ymax + bb.ymin);
            LongCent := 0.5 * (bb.xmax + bb.xmin);
            Result := GridFull >= MDDef.DEMIX_Tile_Full;
            {$IfDef RecordUseTile} if not Result then WriteLineToDebugFile('Not doing tile=' + DEMIXTile + '  fill=' + IntToStr(round(GridFull))); {$EndIf}
         end;

            procedure LandCover(LandCoverGrid : integer; Lat,Long : float32; Value : float32);
            var
               slc : integer;
            begin
               slc := SimplifiedLandCover(LandCoverGrid,Lat,Long,Value);
               if (slc = slcForest) then inc(ForestMomentVar.Npts)
               else if (slc = slcBarren) then inc(BarrenMomentVar.Npts)
               else if (slc = slcUrban) then inc(UrbanMomentVar.Npts)
               else if (slc = slcWater) then inc(WaterMomentVar.Npts);
            end;


         procedure GetTileStatistics(RefDEM : integer);
         var
           ElevMomentVar,SlopeMomentVar,RoughMomentVar : tMomentVar;
           Col,Row,NPts : integer;
           ForestPC,UrbanPC,BarrenPC,WaterPC,Relief : float32;
         begin
            if UseThisTile then begin
               bbgrid := GridBoundingBox(RefDEM);
               InitializeMomentVar(ElevMomentVar);
               InitializeMomentVar(SlopeMomentVar);
               InitializeMomentVar(RoughMomentVar);
               NPts := 0;
               New(zs);
               DEMGlb[RefDEM].ElevationMomentsWithArray(bbgrid,ElevMomentVar,zs^);
               Relief := ElevMomentVar.Maxz - ElevMomentVar.Minz;
               if (Relief > 1) then begin
                  DEMGlb[RefDEM].SlopeMomentsWithArray(bbgrid,SlopeMomentVar,zs^);
                  DEMGlb[RefDEM].GetRoughnessInLongArray(bbgrid,RoughMomentVar.NPts,zs^);
                  Moment(zs^,RoughMomentVar,msAll);
                  {$IfDef TrackAverageStats} WriteLineToDebugFile(AreaName + '  ' + DEMIXTile +  '  ' + ShortMomentResultsToString(RoughMomentVar));  {$EndIf}
                  if (LandCoverGrid <> 0) then begin
                     InitializeMomentVar(UrbanMomentVar);
                     InitializeMomentVar(ForestMomentVar);
                     InitializeMomentVar(BarrenMomentVar);
                     InitializeMomentVar(WaterMomentVar);

                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           DEMGlb[RefDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                           LandCover(LandCoverGrid,Lat,long,Difference);
                           inc(NPts);
                        end;
                     end;
                     ForestPC := 100 * ForestMomentVar.Npts / NPts;
                     UrbanPC := 100 * UrbanMomentVar.Npts / NPts;
                     BarrenPC := 100 * BarrenMomentVar.Npts / NPts;
                     WaterPC := 100 * WaterMomentVar.Npts / NPts;
                  end
                  else begin
                     ForestPC := -999;
                     UrbanPC := -999;
                     BarrenPC := -999;
                     WaterPC := -999;
                  end;
                  {$IfDef AllowAreaStats}
                      AreaGridFull := AreaGridFull + GridFull;
                      AreaAveElev := AreaAveElev + ElevMomentVar.Mean;
                      AreaAveSlope := AreaAveSlope + SlopeMomentVar.Mean;
                      AreaAveRuff := AreaAveRuff + RoughMomentVar.Mean;
                      AreaAveRelief := AreaAveRelief + ElevMomentVar.Maxz - ElevMomentVar.Minz;
                      AreaForestPC := AreaForestPC + ForestPC;
                      AreaUrbanPC := AreaUrbanPC + UrbanPC;
                      AreaBarrenPC := AreaBarrenPC + BarrenPC;
                      AreaWaterPC := AreaWaterPC + WaterPC;
                      if AreaMinElev > ElevMomentVar.MinZ then AreaMinElev := ElevMomentVar.MinZ;
                      if AreaMaxElev < ElevMomentVar.MaxZ then AreaMaxElev := ElevMomentVar.MaxZ;
                  {$EndIf}

                  TileHeader := AreaName + ',' + DEMIXTile + ',' + Country + ',' + RealToString(LatCent,-12,-2) + ',' + RealToString(LongCent,-12,-2) + ',' +
                     RealToString(GridFull,-6,-2)   + ','  + RealToString(ElevMomentVar.Mean,-12,-2) + ',' +
                     RealToString(SlopeMomentVar.Mean,-12,-2)  + ',' + RealToString(RoughMomentVar.Mean,-12,-2)  + ',' +
                     RealToString(Relief,-12,-2) + ',' + RealToString(ForestPC,-12,-2)  + ',' + RealToString(UrbanPC,-12,-2) + ','
                     + RealToString(BarrenPC,-12,-2) + ',' + RealToString(WaterPC,-12,-2)  + ','  + RealToString(ElevMomentVar.MinZ,-12,-2)  + ','  + RealToString(ElevMomentVar.MaxZ,-12,-2);
                  TileStats.Add(TileHeader);
               end;
               Dispose(zs);
            end
            else begin
               {$IfDef RecordTileStats} WriteLineToDebugFile('Not doing tile=' + DEMIXTile); {$EndIf}
            end;
         end;

   label
      NoLandCover;
   var
      Ser,i,j,UseDSM,UseDTM : integer;
      TStr : shortstring;
      LandCoverFName : PathStr;
      CandidateBoundBoxGeo,bb : sfBoundBox;
   begin {ProcessDEMIXtestarea}
      {$If Defined(RecordDEMIXFull)} HighLightInDebugFile('ProcessDEMIXtestarea in ' + TestAreaName); {$EndIf}
      ReportErrors := false;
      DEMIXtileDB := 0;
      LandCoverGrid := 0;
      {$IfDef AllowAreaStats}
          AreaGridFull := 0;
          AreaAveElev := 0;
          AreaAveSlope := 0;
          AreaAveRuff := 0;
          AreaAveRelief := 0;
          AreaForestPC := 0;
          AreaUrbanPC := 0;
          AreaBarrenPC := 0;
          AreaWaterPC := 0;
      {$EndIf}

      wmdem.SetPanelText(3,'Load reference DEM');
      DEMIXRefDEM := OpenNewDEM(RefDTMpointFName,false);
      if ValidDEM(DEMIXRefDEM) then begin
         DEMIXtileDB := DEMIXtileFill(DEMIXRefDEM,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,false);
         GISdb[DEMIXtileDB].ApplyGISFilter(FilterForDEMIXtilesToUse);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('DTM tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}
         {$IfDef AllowAreaStats}
            AreaMinElev := 99e38;
            AreaMaxElev := -99e38;
         {$EndIf}
         if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
            TStr := 'No filled DEMIX tiles on ' + AreaName;
            {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
         end
         else begin
            {$If Defined(RecordDEMIXFull) or Defined(TrackDEMIX_DEMs) or Defined(Rec_DEMIX_Landcover)} OpenDEMsToDebugFile('Land cover loading'); {$EndIf}
            LandCoverGrid := LoadLandcoverForDEMIXarea(AreaName,false);
            j := 0;
            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               HeavyDutyProcessing := true;
               inc(j);
               wmdem.SetPanelText(2,'Tile: ' + IntToStr(j) + '/' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsinDB));
               if UseThisTile then begin
                  GetTileStatistics(DEMIXRefDEM);
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
            {$IfDef AllowAreaStats}
                j := GISdb[DEMIXtileDB].MyData.FiltRecsInDB;
                DEMGlb[DEMIXRefDEM].DEMCenterPoint(LatCent,LongCent);
                TileHeader := AreaName + ',' + IntToStr(j) + ',' + Country + ',' + RealToString(LatCent,-12,-2) + ',' + RealToString(LongCent,-12,-2) + ',' +
                   RealToString(AreaGridFull / j,-6,-2)   + ','  + RealToString(AreaAveElev / j,-12,-2) + ',' +
                   RealToString(AreaAveSlope / j,-12,-2)  + ',' + RealToString(AreaAveRuff / j,-12,-2)  + ',' +
                   RealToString(AreaAveRelief / j,-12,-2) + ',' + RealToString(AreaForestPC / j,-12,-2)  + ',' + RealToString(AreaUrbanPC / j,-12,-2) + ',' +
                   RealToString(AreaBarrenPC / j,-12,-2) + ',' + RealToString(AreaWaterPC / j,-12,-2) + ',' +
                   RealToString(AreaMinElev,-12,-2) + ',' + RealToString(AreaMaxElev,-12,-2);
                AreaStats.Add(TileHeader);
            {$EndIf}
            {$If Defined(RecordDEMIXFull)} for i := 1 to 2 do writeLineToDebugFile(''); {$EndIf}
         end;
         {$If Defined(RecordCriteriaEvaluation)} WriteLineToDebugFile('call CloseSingleDB(DEMIXtileDB)'); {$EndIf}
         CloseSingleDB(DEMIXtileDB);
      end;
      ReportErrors := true;
      {$If Defined(RecordDEMIXFull) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('closing all DEMs'); {$EndIf}
      if DEMIXCloseMap then CloseAllDEMs;
    end {ProcessDEMIXtestarea};

var
   i : integer;
begin {procedure ComputeDEMIX_Summary_stats}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('Start ComputeDEMIXstats'); {$EndIf}
   try
      GetDEMIXpaths;
      MDDef.DEMIX_mode := dmFull;
      SetParamsForDEMIXmode;

      if GeoTiles then begin
          AreasWanted := DEMIX_AreasWanted;
      end
      else begin

      end;

      {$IfDef RecordDEMIX} WriteLineToDebugFile('Areas to process in selected list=' + IntToStr(AreasWanted.Count)); {$EndIf}

      InitializeStringLists;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         Country := GetCountryForArea(AreaName);
         wmdem.SetPanelText(1,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + '  ' + AreaName);
         GetAreaDEMNames(AreaName);
         if FileExists(RefDTMPointFName) then begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Start process for ' + AreaName); {$EndIf}
            ProcessDEMIXtestarea;
         end
         else begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile(AreaName + ' missing 1 sec reference DEMs'); {$EndIf}
         end;
      end;
      {$IfDef RecordDEMIX} WriteLineToDebugFile('All areas done'); {$EndIf}
      FinalizeStringLists(false);
      {$IfDef RecordDEMIX} WriteLineToDebugFile('All string lists processed'); {$EndIf}
      AreasWanted.Destroy;
   finally;
      RestoreBackupDefaults;
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('End ComputeDEMIXstats'); {$EndIf}
end {procedure ComputeDEMIX_Summary_stats};


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
   AreaInventoryName,AreaMergeName, AreaPath,DEMName{,OutVRT} : PathStr;
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
                     UseGDAL_VRT_to_merge(AreaMergeName,{OutVRT,}DEMs);
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





