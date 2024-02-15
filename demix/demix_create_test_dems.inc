{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{    include file for demix_control  }
{____________________________________}



procedure DiluviumDEMforTestAreas(Overwrite : boolean = true);
var
   TheFiles,Areas : tStringList;
   AreaName : Shortstring;
   fName,SaveName : PathStr;
   i,RefDEM,NewDEM,WantImage : Integer;
begin
   {$If Defined(RecordDEMIX) or Defined(RecordDiluvium)} WriteLineToDebugFile('DiluviumDEMforTestAreas in'); {$EndIf}
   try
      GetDEMIXpaths(true);
      HeavyDutyProcessing := true;
      SafeMakeDir(DEMIX_diluvium_dems);
      Areas := DEMIX_AreasWanted(true,AreaName);
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         wmdem.SetPanelText(2,IntToStr(i) + '/' + IntToStr(Areas.Count) + '  ' +  AreaName);
         SaveName := DEMIX_diluvium_dems + AreaName + '_DILUV' + '.tif';
         if (not FileExists(SaveName)) or Overwrite then begin
            if LoadDEMIXReferenceDEMs(Areas.Strings[i],RefDEM,false) then begin
               if LoadMapLibraryBox(NewDEM,WantImage,true,DEMGlb[RefDEM].DEMBoundBoxGeo,'DILUV',true) and ValidDEM(NewDEM) then begin
                  DEMGlb[NewDEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
                  {$If Defined(RecordDiluvium)} WriteLineToDebugFile('Loaded: ' + DEMGlb[NewDEM].KeyDEMParams(true)); {$EndIf}
                  DEMGlb[NewDEM].SelectionMap.ClipDEMtoregion(DEMGlb[RefDEM].DEMBoundBoxGeo);
                  {$If Defined(RecordDiluvium)} WriteLineToDebugFile('Clipped 1: ' + DEMGlb[NewDEM].KeyDEMParams(true)); {$EndIf}
                  MaskStripFromSecondGrid(NewDEM,RefDEM,msSecondMissing);
                  MarkWaterMissingInThisDEM(NewDEM);
                  {$If Defined(RecordDiluvium)} WriteLineToDebugFile('Water mask: ' + DEMGlb[NewDEM].KeyDEMParams(true)); {$EndIf}
                  ClipTheDEMtoFullDEMIXTiles(NewDEM);
                  {$If Defined(RecordDiluvium)} WriteLineToDebugFile('Clipped 2: ' + DEMGlb[NewDEM].KeyDEMParams(true)); {$EndIf}
                  DEMGlb[NewDEM].SaveAsGeotiff(SaveName);
               end;
            end;
         CloseAllDEMs;
         end;
      end;
      Areas.Free;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DiluviumDEMforTestAreas out'); {$EndIf}
end;


function CreateDEMIXTestDEMs(Overwrite : boolean; AreaName : ShortString; AreaRefDEM,PointRefDEM : integer) : boolean;
var
   {$IfDef RecordDEMIX} AllDEMs, {$EndIf}
   WantSeries,ShortName : shortstring;
   IndexSeriesTable : tMyData;
   NumPts : int64;
   WantDEM,WantImage,Ser,i,GeoidGrid,aRefDEM : integer;
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
               {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Closed DEM; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               DEMGlb[NewDEM].CheckMaxMinElev;
               DEM := NewDEM;
               {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift out, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            end
            else begin
               {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift out, not EGM96, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            end;
         end;


begin {CreateDEMIXTestDEMs}
   {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('CreateDEMIXCandidateDEMs in; AreaName=' + AreaName + '  Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
   Result := false;
   if (ValidDEM(AreaRefDEM) and ValidDEM(PointRefDEM)) then begin

      {$IfDef RecordDEMIX} AllDEMs := ''; {$EndIf}

      for I := 1 to MaxDEMIXDEM do begin
         TestDEMs[i] := 0;
         TestSeries[i] := '';
      end;
      GeoidGrid := 0;

      Ser := 0;
      for I := 1 to NumDEMIXDEM do begin
         WantSeries := DEMIXDEMTypeName[i];
         ShortName := DEMIXShort[i];
         SaveName := DEMIX_test_dems + AreaName + '_' + shortname + '.tif';
         wmdem.SetPanelText(3,'Load candidate DEM ' + ShortName + '  ' + IntToStr(i) + '/' + IntToStr(NumDEMIXDEM));
         if Overwrite or (not FileExists(SaveName)) then begin
            {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoad)} writeLineToDebugFile('Try ' + WantSeries + ' ' + ShortName + '  ' + IntToStr(i) + '/' + IntToStr(NumDEMIXDEM)); {$EndIf}
            {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('Ref DEM=' + DEMGlb[PointRefDEM].AreaName + '  ' + sfBoundBoxToString(DEMGlb[PointRefDEM].DEMBoundBoxGeo,6)); {$EndIf}
            if LoadMapLibraryBox(WantDEM,WantImage,true,DEMGlb[PointRefDEM].DEMBoundBoxGeo,WantSeries,false) and ValidDEM(WantDEM) then begin
               {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('CreateDEMIXCandidateDEMs done LoadMapLib; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
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

               {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Opened:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               if (ShortName = 'COP') or (ShortName = 'TANDEM') or (ShortName = 'FABDEM') or (ShortName = 'DILUV') then DEMGlb[TestDEMs[Ser]].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008
               else begin
                  MoveFromEGM96toEGM2008(TestDEMs[Ser]);
               end;
               {$IfDef RecordDEMIXLoad} if (TestSeries[Ser] = 'ASTER') then writeLineToDebugFile('EGM2003:  ' + SaveName + ' ' + DEMGlb[TestDEMs[Ser]].PixelIsString); {$EndIf}
               MaskStripFromSecondGrid(TestDEMs[Ser],aRefDEM, msSecondMissing);
               DEMGlb[TestDEMs[Ser]].SavePartOfDEMWithDataGeotiff(SaveName);
               Result := true;
            end
            else begin
               {$IfDef RecordDEMIX} AllDEMs := AllDEMs + WantSeries + ' (missing)'; {$EndIf}
            end;
         end;
      end;
      CloseSingleDEM(GeoidGrid);
      {$IfDef RecordDEMIX} if (AllDEMs <> '') then HighlightLineToDebugFile(AreaName + ' DEM problem, ' + AllDEMs); {$EndIf}
      {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('CreateDEMIXCandidateDEMs out; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
   end
   else begin
      {$IfDef RecordDEMIX} HighlightLineToDebugFile(AreaName + ' CreateDEMIXCandidateDEMs fails, no reference DEMs'); {$EndIf}
   end;
end {CreateDEMIXTestDEMs};


procedure CreateTestAreaDEMs(Overwrite : boolean);
var
   i,RefDEM : integer;
   Areas : tStringList;
   AreaName : shortstring;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateTestAreaDEMs in'); {$EndIf}
   try
      HeavyDutyProcessing := true;
      GetDEMIXPaths(true);
      SafeMakeDir(DEMIX_test_dems);
      Areas := DEMIX_AreasWanted(true,AreaName);
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + AreaName);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Call LoadDEMIXReferenceDEMs for ' + AreaName); {$EndIf}
         LoadDEMIXReferenceDEMs(AreaName,RefDEM,false);
         if ValidDEM(RefDTMarea) and ValidDEM(RefDTMpoint) then begin
             CreateDEMIXTestDEMs(Overwrite,AreaName,RefDTMarea,RefDTMpoint);
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
      HeavyDutyProcessing := false;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateTestAreaDEMs out'); {$EndIf}
end;


