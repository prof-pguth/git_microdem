
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for dem_indexes         }
{________________________________________}




    procedure CheckLowElevationAreas;
    var
       Areas,Results,RefDEMs,TestDEMs,
       COASTAL,DILUVIUM,DELTA : tStringList;
       Area : shortstring;
       fName : PathStr;
       i,db : Integer;

             procedure WriteAreaFile(DEM,fName : Shortstring);
             var
                sl : tStringList;
             begin
                GISdb[db].ApplyGISFilter(DEM + '=1');
                sl := GISdb[db].MyData.ListUniqueEntriesInDB('AREA');
                sl.SaveToFile(DEMIXSettingsDir + fname);
                sl.Destroy;
             end;

    begin
       GetDEMIXpaths(false);
       Areas := tStringList.Create;
       Areas.LoadFromFile(AreaListFName);

       COASTAL := tStringList.Create;
       FindMatchingFiles(DEMIX_coastal_dtms,'*.tif',Coastal,1);
       DILUVIUM := tStringList.Create;
       FindMatchingFiles(DEMIX_diluvium_dtms,'*.tif',Diluvium,1);
       DELTA := tStringList.Create;
       FindMatchingFiles(DEMIX_delta_dtms,'*.tif',Delta,1);
       RemoveFilesThatDoNotHaveString(Delta,'_DELTA');   //some areas have "DELTA" in area name

       RefDEMs := tStringList.Create;
       FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',RefDEMs,1);
       TestDEMs := tStringList.Create;
       FindMatchingFiles(DEMIX_test_dems,'*.tif',TestDEMs,1);

       Results := tStringList.Create;
       Results.Add('AREA,REF_DEMS,TEST_DEMs,COASTAL,DILUVIUM,DELTA_DEM');
       for i := 0 to pred(Areas.Count) do begin
          Area := Areas.Strings[i];
          Results.Add(Area + ',' + IntToStr(NumberOfStringsWithSubstring(RefDEMs,Area)) + ',' +
                                       IntToStr(NumberOfStringsWithSubstring(TestDEMs,Area)) + ',' +
                                       IntToStr(NumberOfStringsWithSubstring(Coastal,Area)) + ',' +
                                       IntToStr(NumberOfStringsWithSubstring(Diluvium,Area)) + ',' +
                                       IntToStr(NumberOfStringsWithSubstring(Delta,Area)));
       end;


       fName := NextFileNumber(MDTempDir,'area_dem_status_','.dbf');
       db := PetdbUtils.StringList2CSVtoDB(Results,fName);
       WriteAreaFile('COASTAL','areas_coastal.txt');
       WriteAreaFile('DILUVIUM','areas_diluvium.txt');
       WriteAreaFile('DELTA_DEM','areas_delta.txt');
       GISdb[db].ClearGISFilter;
       Areas.Free;
       COASTAL.Free;
       DILUVIUM.Free;
       DELTA.Free;
       RefDEMs.Free;
       TestDEMs.Free;

       EndDEMIXProcessing;
       GISdb[db].ShowStatus;
    end;



      procedure DiluviumDEMIXtileReport;
	  
      procedure DiluviumDEMIXtileFill(DEM : integer; var sl : tStringList);
      var
         Col,Row,db : integer;
         Pts,Coastal,Ocean,Highlands,DeepWater : int64;
         bbgrid : tGridLimits;
         z : float32;
         bb : sfBoundBox;
         fName : PathStr;
      begin
         {$IfDef RecordDEMIXTiles} writeLineToDebugFile('DEMIXtileFill in, DEM=' + IntToStr(DEM)); {$EndIf}
         if ValidDEM(DEM) then try
            db := LoadDEMIXtileOutlinesNoMap(fName,DEMGlb[DEM].DEMBoundBoxGeo,false,false,false);
            if ValidDB(db) then try
               {$IfDef RecordDEMIXTiles} writeLineToDebugFile('DEMIXtileFill, db created recs=' + IntToStr(GISdb[Result].MyData.TotRecsInDB)); {$EndIf}
               GISdb[db].MyData.First;
               GISdb[db].EmpSource.Enabled := false;
               while not GISdb[db].MyData.eof do begin
                  Pts := 0;
                  Coastal := 0;
                  Ocean := 0;
                  Highlands := 0;
                  DeepWater := 0;
                  bb := GISdb[db].MyData.GetRecordBoundingBox;
                  DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,bbgrid.xgridlow,bbgrid.ygridlow);
                  DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,bbgrid.xgridhigh,bbgrid.ygridhigh);
                  for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                     //UpdateProgressBar((bbgrid.xgridhigh - Col) / (bbgrid.xgridhigh - bbgrid.xgridlow));
                     for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                        inc(Pts);
                        if DEMGlb[DEM].MissingDataInGrid(Col,Row) then inc(Highlands)
                        else begin
                           if DEMGlb[DEM].GetElevMetersOnGrid(Col,Row,z) then begin
                              if z < -0.01 then inc(DeepWater)
                              else if z < 0.01 then inc(Ocean)
                              else inc(Coastal);
                           end;
                        end;
                     end;
                  end;

                  if (Coastal > 1) then begin
                     sl.Add(GISdb[db].MyData.GetFieldByNameAsString('NAME') + ',' +
                            RealToString(0.5 * (bb.YMax + bb.YMin),-12,-3)+ ',' +
                            RealToString(0.5 * (bb.xMax + bb.xMin),-12,-3)+ ',' +
                            RealToString(100 * Ocean / Pts,-12,-3)+ ',' +
                            RealToString(100 * DeepWater / Pts,-12,-3)+ ',' +
                            RealToString(100 * Coastal / Pts,-12,-3)+ ',' +
                            RealToString(100 * Highlands / Pts,-12,-3) );
                  end;
                  {$IfDef RecordDEMIXTiles} writeLineToDebugFile('File boundary, ' + GridLimitsToString(bbgrid) + RealToString(Full,8,2)); {$EndIf}
                  GISdb[db].MyData.Next;
               end;
            finally
            end;
         finally
            CloseAndNilNumberedDB(db);
         end;
         {$IfDef RecordDEMIXTiles} writeLineToDebugFile('DEM contains parts of tiles=' + IntToStr(GISdb[Result].MyData.TotalResInDB)); {$EndIf}
      end;

	  
	  
	  
	  
      var
         sl : tStringList;
         DEM,i : integer;
         MapLib : tMyData;
         fName : PathStr;
      begin
         if FileExists(MapLibraryFName) then begin
            try
               HeavyDutyProcessing := true;
               sl := tStringList.Create;
               sl.Add('DEMIX_TILE,LAT,LONG,OCEAN,DEEPWATER,COASTAL,HIGHLANDS');
               fName := MapLibraryFName;
               MapLib := tMyData.Create(fName);
               MapLib.ApplyFilter('SERIES=' + QuotedStr('DILUV'));
               //StartProgress('DEMIX tiles summary for DILUV');
               i := 0;
               while not MapLib.eof do begin
                  wmDEM.SetPanelText(2,IntToStr(i) + '/' + IntToStr(MapLib.FiltRecsInDB));
                  inc(i);
                  DEM := OpenNewDEM(MapLib.GetFieldByNameAsString('FILENAME'),false);
                  DiluviumDEMIXtileFill(DEM,sl);
                  sl.SaveToFile(mdTempDir + 'floods.csv');
                  CleanUpTempDirectory(false);
                  MapLib.Next;
               end;
               fname := NextFileNumber(MDTempDir,'diluvium_dem_report_','.dbf');
               StringList2CSVtoDB(sl,fName);
            finally
               HeavyDutyProcessing := false;
            end;
         end;
      end;
