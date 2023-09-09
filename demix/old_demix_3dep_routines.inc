{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{include file for demix_control.pas  }
{____________________________________}



      procedure SummarizeVDatumShifts;
      var
         Files,Summary : tstringlist;
         fName : PathStr;
         I : integer;
         db : tMyData;
         TStr : shortstring;
      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SummarizeVDatumShifts in'); {$EndIf}
         GetDEMIXpaths(false);
         Files := nil;
         Petmar.FindMatchingFiles(vd_path,'*.dbf',Files,0);
         Summary := tstringlist.create;
         Summary.Add('AREA,LAT,LONG,MEAN_DX,MEAN_DY,MEAN_DZ,MEDIAN_DZ,STD_DZ,MAX_DX,MAX_DY,MAX_DZ,MIN_DX,MIN_DY,MIN_DZ');
         if (Files.Count > 0) then for I := 0 to pred(Files.Count) do begin
            fName := Files.Strings[i];
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SummarizeVDatumShifts ' + fName); {$EndIf}
            db := tMyData.Create(fName);
            if db.FieldExists('X_SHIFT') and db.FieldExists('Y_SHIFT') and db.FieldExists('VERT_SHIFT') then begin
               TStr := ExtractFileNameNoExt(db.TableName)  + ',' ;
               TStr := TStr + RealToString(db.FieldAverage('LAT'),-12,-3) + ',' +  RealToString(db.FieldAverage('LONG'),-12,-3) + ',';
               TStr := TStr + RealToString(db.FieldAverage('X_SHIFT'),-8,-2) + ',' + RealToString(db.FieldAverage('Y_SHIFT'),-8,-2) + ',' +
                 RealToString(db.FieldAverage('VERT_SHIFT'),-8,-2) + ',' + RealToString(db.FieldMedian('VERT_SHIFT'),-8,-2) + ',' +
                 RealToString(db.FieldStdDev('VERT_SHIFT'),-8,-2) + ',';
               TStr := TStr + RealToString(db.FindFieldMax('X_SHIFT'),-8,-2) + ',' + RealToString(db.FindFieldMax('Y_SHIFT'),-8,-2) + ',' + RealToString(db.FindFieldMax('VERT_SHIFT'),-8,-2) + ',';
               TStr := TStr + RealToString(db.FindFieldMin('X_SHIFT'),-8,-2) + ',' + RealToString(db.FindFieldMin('Y_SHIFT'),-8,-2) + ',' + RealToString(db.FindFieldMin('VERT_SHIFT'),-8,-2);
               Summary.Add(TStr);
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
            end
            else begin
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Shift files problem ' + fName); {$EndIf}
            end;
            db.Destroy;
         end;
         Files.Destroy;
         fName := NextFileNumber(MDTempDir,'VDATUM_shift_summary','.dbf');
         StringList2CSVtoDB(Summary,fName);
      end;


      procedure DEMIX_VDatum_shifts;
      //processes any csv files created by VDATUM, which are then converted to dbf files and the DEM shifted in x, y, and z
      var
        fName{,fName2} : PathStr;
        AreaName{,TStr} : shortstring;
        Shifts,ErrorLog : tStringList;
        i,{j,}db{,DEM}  : Integer;
      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_VDatum_shifts in'); {$EndIf}
         try
            GetDEMIXPaths;
            ErrorLog := tStringList.Create;
            Shifts := tStringList.Create;
            FindMatchingFiles(vd_path,'*.csv',Shifts,0);
            for I := 0 to pred(Shifts.Count) do begin
               AreaName := ExtractFileNameNoExt(Shifts.Strings[i]);
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Start ' + AreaName); {$EndIf}
               wmdem.SetPanelText(2,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Shifts.Count));
               wmdem.SetPanelText(3,AreaName);
               fName := DEMIX_Ref_Merge + AreaName + '.dem';
               if not FileExists(fname) then begin
                  ErrorLog.Add('Merged DEM missing: ' + fName);
               end
               else begin
                  db := AnalyzeVDatumShift(Shifts.Strings[i],ErrorLog);
                  VerticalDatumShiftWithVDATUM(AreaName,0,db,fName,ErrorLog);
                  (*
                  if ValidDB(db) then begin
                     dx := GISdb[db].MyData.FieldAverage('X_SHIFT');
                     dy := GISdb[db].MyData.FieldAverage('Y_SHIFT');
                     dz := GISdb[db].MyData.FieldAverage('VERT_SHIFT');
                     if (abs(dx) < 0.01) and (abs(dy) < 0.01) and (abs(dz) < 0.01) then begin
                        ErrorLog.Add(AreaName + ' no change in dx, dy, and dz');
                     end
                     else begin
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Load ' + fName); {$EndIf}
                        DEM := OpenNewDEM(fName,false);
                        if DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey = VertCSEGM2008 then begin
                           {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEM already EGM2008=' + IntToStr(DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey)); {$EndIf}
                        end
                        else begin
                           {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEM was ' + IntToStr(DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey)); {$EndIf}
                           DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
                           DEMGlb[DEM].DEMHeader.DEMSWCornerX := DEMGlb[DEM].DEMHeader.DEMSWCornerX + dx;
                           DEMGlb[DEM].DEMHeader.DEMSWCornerY := DEMGlb[DEM].DEMHeader.DEMSWCornerY + dy;
                           {$If Defined(RecordDEMIX)} WriteLineToDebugFile('For EGM2008 added dz =' + RealToString(dz,-8,-2)); {$EndIf}
                           DEMGlb[DEM].AddConstantToGrid(dz);
                           {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Writing DEM format ' + fName); {$EndIf}
                           DEMGlb[DEM].WriteNewFormatDEM(DEMGlb[DEM].DEMFileName);
                        end;
                        CloseSingleDEM(DEM);
                     end;
                     CloseAndNilNumberedDB(db);
                  end;
                  *)
               end;
            end;
         finally
             EndDEMIXProcessing;
             Shifts.Free;
             DisplayAndPurgeStringList(ErrorLog,'DEMIX_VDatum_shifts Problems');
         end;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_VDatum_shifts out'); {$EndIf}
      end;


      procedure SubsetLargeUS3DEParea(DEM : integer = 0);
      const
         Extra = 0.0051;
      var
         db,SubArea : integer;
         bb : sfBoundBox;
         filter : shortstring;
         fName : PathStr;
         JustOne : boolean;
         GridLimits : tGridLimits;

               procedure CornerFilter(ExtraSW,ExtraNE : float32; What : shortstring);
               begin
                  GISdb[db].MyData.First;
                  bb.xmin := GISdb[db].MyData.GetFieldByNameAsFloat('LONG_LOW');
                  bb.ymin := GISdb[db].MyData.GetFieldByNameAsFloat('LAT_LOW');
                  {$If Defined(Record3DEPX)} WriteLineToDebugFile(What + ' ' + LatLongDegreeToString(bb.ymin,bb.xmin,DecDegrees)); {$EndIf}
                  //limit to no more than 2 tiles in each direction
                  bb.xmax := bb.xmin + 0.2;
                  bb.ymax := bb.ymin + 0.2;
                  //move so we are either just inside or just outside the tiles we want
                  bb.XMax := bb.xmax + ExtraNE;
                  bb.yMax := bb.ymax + ExtraNE;
                  bb.xmin := bb.xmin + ExtraSW;
                  bb.ymin := bb.ymin + ExtraSW;
                  Filter := MakeCornersGeoFilter(bB);
                  GISdb[db].ApplyGISFilter(Filter);
                  {$If Defined(Record3DEPX)} WriteLineToDebugFile(What + ' ' + Filter + ' bb=' + sfBoundBoxToString(bb,2) + ' tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
               end;

      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SubsetLargeUS3DEParea in'); {$EndIf}
         try
            GetDEMIXpaths;
            if ValidDEM(DEM) then begin
               JustOne := false;
            end
            else begin
               DEM := OpenNewDEM;
               JustOne := true;
            end;
            if ValidDEM(DEM) then begin
               SubArea := 0;
               fName := DEMIX_3DEP_Dir + DEMGlb[DEM].AreaName + '_sub_1' + '.tif';
               if FileExists(fName) then begin
                  db := 0;
               end
               else begin
                  db := DEMIXtileFill(DEM,DEMGlb[DEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo);
                  {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea partial tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
                  Filter := 'GRID_FULL<' + RealToString(MDDef.DEMIX_Full,-8,-1);
                  GISdb[db].ApplyGISFilter(Filter);
                  {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea too empty tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
                  GISdb[db].DeleteAllSelectedRecords(true);
                  GISdb[db].ClearGIsfilter;
                  {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea exporting tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
                  wmdem.SetPanelText(3,DEMGlb[DEM].AreaName + ': tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB));

                  repeat
                     //the first tile in the db is at the lower left
                     //we want to merge no more than 4 tiles due to issues with larger Geotiffs
                     //use slight buffer for UTM grid rotation
                     CornerFilter(-Extra,+Extra, 'SubsetLargeUS3DEParea to export ');

                     DEMGlb[DEM].SelectionMap.SubsetAndZoomMapFromGeographicBounds(bb,true);
                     inc(SubArea);
                     GridLimits := DEMGlb[DEM].SelectionMap.MapDraw.MapAreaDEMGridLimits;
                     fName := DEMIX_3DEP_Dir + DEMGlb[DEM].AreaName + '_sub_' + IntToStr(SubArea) + '.tif';
                     {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea save tiff=' + '  ' + GridLimitsToString(GridLimits) + ' ' + fName); {$EndIf}
                     DEMGlb[DEM].SaveGridSubsetGeotiff(GridLimits,fName);
                     HeavyDutyProcessing := true;

                     //Adjust to only get tiles that were 100% exported, so make box just a bit smaller
                     CornerFilter(+Extra,-Extra,'SubsetLargeUS3DEParea to delete');
                     GISdb[db].DeleteAllSelectedRecords(true);
                     GISdb[db].ClearGIsfilter;
                     {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea remaining tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
                     wmdem.SetPanelText(3,DEMGlb[DEM].AreaName + ': remaining tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB));
                  until (GISdb[db].MyData.FiltRecsInDB = 0);
               end;
            end;
         finally
             CloseAndNilNumberedDB(db);
             if JustOne then begin
                DEMGlb[DEM].SelectionMap.RestoreFullMap;
                EndDEMIXProcessing;
             end;
         end;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SubsetLargeUS3DEParea out'); {$EndIf}
      end;



      procedure BatchSubset_3DEP_DEMs;
      var
         FilesWanted : tStringList;
         DefaultFilter : byte;
         DEM,i  : integer;
         fName : PathStr;
      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BatchSubset_3DEP_DEMs in'); {$EndIf}
         try
            GetDEMIXpaths;
            FilesWanted := tStringList.Create;
            FilesWanted.Add(DEMIX_3DEP_Dir);
            DefaultFilter := 1;
            if GetMultipleFiles('for 3DEP subset','*.dem',FilesWanted,DefaultFilter) then begin
               for I := 0 to pred(FilesWanted.Count) do begin
                  wmdem.SetPanelText(2,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count));
                  fName := FilesWanted.Strings[i];
                  DEM := OpenNewDEM(fName,true);
                  SubsetLargeUS3DEParea(DEM);
                  CloseSingleDEM(DEM);
               end;
            end;
         finally
            EndDEMIXProcessing;
         end;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BatchSubset_3DEP_DEMs out'); {$EndIf}
      end;


