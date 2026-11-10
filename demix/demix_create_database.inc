{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{   include file for demix_definitions   }
{________________________________________}


{$Define UseOldDifferenceDistributions}


//{$Define RecordBest}


function DecsForCriterion(Criterion : shortString) : integer;
begin
   if AnsiContainsText(Criterion,'FUV') then Result := -8 else Result := -5;
end;



procedure DSM_gt_DTM_1(db : integer);
//adds field DSM_PC with percentage of DEM where DSM and DTM should be different, urban and forest landcovers
var
   x,y,Total,Higher,Count,RefDSM,RefDTM : integer;
   AreaName : shortstring;
   DTMdir,DSMdir : PathStr;
   z1,z2 : float32;
begin
   SetColorForProcessing;
   ShowHourglassCursor;
   WantShowProgress := false;
   GISdb[DB].ClearGISFilter;
   GISdb[DB].MyData.InsureFieldPresentAndAdded(ftFloat,'DSM_G1_DTM',8,2);

   Count := 0;
   while not GISdb[db].MyData.eof do begin
       inc(Count);
       wmDEM.SetPanelText(1,IntToStr(Count) + '/' + IntToStr(GISdb[db].MyData.FiltRecsInDB),true);
       GISdb[DB].EmpSource.Enabled := false;
       AreaName := GISdb[db].MyData.GetFieldByNameAsString('AREA');

       DTMDir := ExtractFilePath(GISdb[db].DBFullName) + AreaName  + '\' + GISdb[db].MyData.GetFieldByNameAsString('DTM_NAME')  + RefTestStr + '\';
       RefDTM := OpenNewDEM(DTMDir + 'ref_DTM_srtm.tif',false);

       DSMDir := ExtractFilePath(GISdb[db].DBFullName) + AreaName  + '\' + GISdb[db].MyData.GetFieldByNameAsString('DSM_NAME')  + RefTestStr + '\';
       RefDSM := OpenNewDEM(DSMDir + 'ref_DSM_srtm.tif',false);

       Total := 0;
       Higher := 0;
       for x := 0 to pred(DEMglb[RefDSM].DEMheader.NumCol) do begin
          for y := 0 to pred(DEMglb[RefDSM].DEMheader.NumRow) do begin
             if DEMglb[RefDTM].GetElevMetersOnGrid(x,y,z1) and DEMglb[RefDSM].GetElevMetersOnGrid(x,y,z2) then begin
                 inc(Total);
                 if (z2 - z1 > 1) then inc(Higher);
             end;
          end;
       end;
       CloseSingleDEM(RefDTM);
       CloseSingleDEM(RefDSM);
       GISdb[DB].MyData.Edit;
       GISdb[DB].MyData.SetFieldByNameAsFloat('DSM_G1_DTM',100 * Higher / Total);
       GISdb[DB].MyData.Next;
   end;
    SetColorForWaiting;
   ShowDefaultCursor;
   WantShowProgress := true;
   wmDEM.StatusBar1.Panels[1].Text := '';
end;


procedure DSM_DTM_DifferFromLandcover(dbOnTable : integer);
//adds field DSM_PC with percentage of DEM where DSM and DTM should be different, urban and forest landcovers
var
   Urban,Forest : float32;
begin
   ShowHourglassCursor;
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftFloat,'DSM_PC',8,2);
   while not GISdb[DBonTable].MyData.eof do begin
       GISdb[DBonTable].EmpSource.Enabled := false;
       Urban := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('URBAN_PC');
       Forest := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('FOREST_PC');
       GISdb[DBonTable].MyData.Edit;
       GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DSM_PC',Urban+Forest);
       GISdb[DBonTable].MyData.Next;
   end;
   ShowDefaultCursor;
end;


procedure IdentifyDSMorDTM(dbOnTable : integer);
//adds REF_DEM field to database
var
   Tile,TStr,fName : shortstring;

begin
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'REF_DEM',3);
   GISdb[DBonTable].MyData.First;
   GISdb[DBonTable].EmpSource.Enabled := false;
   while not GISdb[DBonTable].MyData.eof do begin
      Tile := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetFieldByNameAsString('REF_DEM',IdentifyDEMIXtileAsDTMorDSM(Tile));
      GISdb[DBonTable].MyData.Next;
   end;
end;


procedure IdentifyDSM_DTMpair(DBonTable : integer);
var
   Lat,Long : float64;
   DTMName,DSMName : PathStr;
   fl : integer;
   Tile,Tstr : shortstring;
begin
   GISdb[DBonTable].ClearGISFilter;
   SetColorForProcessing;
   fl := GISdb[DBonTable].MyData.GetFieldLength('DEMIX_TILE');
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DEM_PAIR',3);
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DSM_NAME',fl+3);
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DTM_NAME',fl+3);
   GISdb[DBonTable].ApplyGISFilter('DEM_PAIR=' + QuotedStr('NO'));
   GISdb[DBonTable].FillFieldWithValue('DEM_PAIR','',false);

   repeat
       GISdb[DBonTable].ApplyGISFilter('DEM_PAIR=' + QuotedStr(''));
       GISdb[DBonTable].EmpSource.Enabled := false;
       if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
           wmdem.SetPanelText(1,'Tiles to go: ' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB),true);
           {$IfDef RecordDEMIX} WriteLineToDebugFile(Tile + '  Tiles to go: ' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
           Tile := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
           GISdb[DBonTable].MyData.ValidLatLongFromTable(Lat,Long);
           if GetDSMandDTMTileNamesFromLatLong(DBonTable,Lat,Long,DTMName,DSMName) then TStr := 'YES' else TStr := 'NO';
           GISdb[DBonTable].FillFieldWithValue('DEM_PAIR',TStr,false);
           GISdb[DBonTable].FillFieldWithValue('DSM_NAME',DSMname,false);
           GISdb[DBonTable].FillFieldWithValue('DTM_NAME',DTMname,false);
           GISdb[DBonTable].ApplyGISFilter('DEM_PAIR=' + QuotedStr(''));
       end;
   until (GISdb[DBonTable].MyData.FiltRecsInDB = 0);
   SetColorForWaiting;
   GISdb[DBonTable].ClearGISFilter;
end;


procedure MakeCSVforGDEMslopes(DB : integer);
const
   NumDEMs = 10;
var
   Areas,Results,TheTiles : tStringList;
   DEMIX_Tile,DSM_tile,aLine,Country,
   Area,BaseFilter,TStr,theFilter,TileStats : shortstring;
   i,DEM,Thin,DEMthin : integer;
   AvgSlope : array[1..10] of float64;
   fName : PathStr;

       function TryOpenDEM(DEM : shortString; TileName : PathStr) : integer;
       var
          fName : PathStr;
       begin
           fName := ExtractFilePath(GISdb[DB].DBFullName) + Area + '\' + TileName + RefTestStr +'\' + DEM + '.tif';
           if FileExists(fName) then Result := OpenNewDEM(fName,false)
           else Result := -999;
       end;

var
   DEMs : array[1..10] of integer;
begin {procedure MakeCSVforGDEMslopes}
  {$IfDef TrackAverageStats} WriteLineToDebugFile('MakeCSVforGDEMslopes in'); {$EndIf}
    SetColorForProcessing;
    WantShowProgress := false;
    HeavyDutyProcessing := true;
    wmDEM.SetPanelText(0,TimeToStr(Now) + ' started',true);

    theFilter := AddAndIfNeeded(BaseFilter) + 'REF_DEM=' + QuotedStr('DTM') + ' AND DEM_PAIR=' + QuotedStr('YES');
    GISdb[db].ApplyGISFilter(theFilter);
    GISdb[db].EmpSource.Enabled := false;
    TheTiles := GISdb[DB].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
    GISdb[DB].MyData.First;
    i := 0;
    while not GISdb[DB].MyData.eof do begin
       inc(i);
       Area := GISdb[DB].MyData.GetFieldByNameAsString('AREA');
       DEMIX_TILE := GISdb[DB].MyData.GetFieldByNameAsString('DEMIX_TILE');
       DSM_TILE := GISdb[DB].MyData.GetFieldByNameAsString('DSM_NAME');
       wmDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(TheTiles.Count) + ' ' + Area,true);
       TileStats := Area + ',' + DEMIX_TILE + ',' + GISdb[DB].MyData.GetFieldByNameAsString('LAT') + ',' + GISdb[db].MyData.GetFieldByNameAsString('LONG') + ',' +
                    CommonTileStats(db);
       DEMs[1] := TryOpenDEM('cop',DEMIX_TILE);
       DEMs[2] := TryOpenDEM('alos',DEMIX_TILE);
       DEMs[3] := TryOpenDEM('fathom',DEMIX_TILE);
       DEMs[4] := TryOpenDEM('gedtm',DEMIX_TILE);
       DEMs[5] := TryOpenDEM('ref_dtm_alos',DEMIX_TILE);
       DEMs[6] := TryOpenDEM('ref_dsm_alos',DSM_TILE);
       DEMs[7] := TryOpenDEM('ref_dtm_srtm',DEMIX_TILE);
       DEMs[8] := TryOpenDEM('ref_dsm_srtm',DSM_TILE);
       DEMs[9] := TryOpenDEM('ref_dtm_cop',DEMIX_TILE);
       DEMs[10] := TryOpenDEM('ref_dsm_cop',DSM_TILE);
       for Thin := 1 to 6 do begin
           aline := TileStats + ',' + IntToStr(Thin);
           for DEM := 1 to 10 do begin
              if ValidDEM(DEMs[DEM]) then begin
                 if (Thin = 1) then begin
                    AvgSlope[DEM] := DEMglb[DEMs[DEM]].AverageDEMslope(DEMglb[DEMs[DEM]].FullDEMGridLimits);
                 end
                 else begin
                    DEMthin := DEMglb[DEMs[DEM]].ThinThisDEM(false,'',Thin,true);
                    AvgSlope[DEM] := DEMglb[DEMthin].AverageDEMslope(DEMglb[DEMs[DEM]].FullDEMGridLimits);
                    CloseSingleDEM(DEMthin);
                 end;
                 aline := aline + ',' + RealToString(AvgSlope[DEM],-8,-2);
              end
              else aline := aline + ',-9999';  //required because the COP reference DEMs only occur at high latitudes
           end;
           Results.Add(aline);
       end;
       CloseAllDEMs;
       GISdb[DB].MyData.Next;
    end;

   fName := ExtractFilePath(GISdb[db].DBFullName) + 'GDEM_slope_grid_size_' + CurrentTimeForFileName(false) + '.dbf';
   StringList2CSVtoDB(Results,fName);
   {$IfDef TrackAverageStats} WriteLineToDebugFile('MakeCSVforGDEMcompareDSMtoDTM out, saved ' + fName); {$EndIf}
   wmDEM.ClearStatusBarPanelText;
   GISdb[db].ClearGISFilter;
   SetColorForWaiting;
   ShowDefaultCursor;
   HeavyDutyProcessing := false;
end {procedure MakeCSVforGDEMslopes};


procedure MakeCSVforGDEM_average_tile_slope(DBonTable : integer);
var
   Areas,Results,Tiles : tStringList;
   AreaName,BaseFilter,TStr,theFilter,TileStats : shortstring;
   aLine : array[1..6] of shortstring;
   DEMs : array[1..6] of integer;
   i,j,DEM,Thin : integer;
   AllValid : boolean;
   RefALOS_DSM,RefALOS_DTM,AlosName,BaseDir,CSVdir,
   fName,RefDSMName,RefDTMName,CopNAME,FathomName,cName,tName : PathStr;

          procedure TryOpening(DEM : integer; fName : PathStr);
          begin
             if FileExists(fName) then begin
                DEMs[DEM] := OpenNewDEM(fName,false);
             end
             else AllValid := false;
             if Not ValidDEM(DEMs[DEM]) then AllValid := false;
          end;

   procedure AddAThinLevel(thin : integer);
   var
      DEM,ThinDEM,aDEM : integer;
      Mean,Std : float32;
   begin
      aLine[thin] := aline[thin] + ',' + IntToStr(Thin);
      for DEM := 1 to 5 do begin
          aDEM := DEMs[DEM];
          if (thin = 1) then begin
             DEMglb[aDEM].GetSlopeMeanStd(DEMglb[aDEM].FullDEMGridLimits,Mean,Std);
             aLine[thin] := aline[thin] + ',' + RealToString(Mean,-8,-2);
          end
          else begin
              ThinDEM := DEMglb[aDEM].ThinThisDEM(false,'',Thin,true);
              DEMglb[ThinDEM].GetSlopeMeanStd(DEMglb[ThinDEM].FullDEMGridLimits,Mean,Std);
              aLine[thin] := aLine[thin] + ',' + RealToString(Mean,-8,-2);
              CloseSingleDEM(ThinDEM);
          end;
      end;
   end;


begin {procedure MakeCSVforGDEM_average_tile_slope}
  {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} HighLightInDebugFile('MakeCSVforGDEMcompareDSMtoDTM in ' + GISdb[dbOnTable].dbName); {$EndIf}
   try
       SetColorForProcessing;
       WantShowProgress := false;
       HeavyDutyProcessing := true;
       if (MDDef.DEMIX_BaseDir = '') then MDDef.DEMIX_BaseDir := ExtractFilePath(GISdb[DBonTable].dbFullName);
       BaseFilter := 'DEM_PAIR=' + QuotedStr('YES') + ' AND REF_DEM=' + QuotedStr('DTM') ;
       GISdb[DBonTable].ApplyGISFilter(BaseFilter );
       Areas := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('AREA');
       //BaseDir := ExtractFilePath(GISdb[DBonTable].DBFullName) + 'aa_gdem_compare_dsm_dtm\';
       CSVdir := ExtractFilePath(GISdb[dbOnTable].DBfullName) + 'aa_gdem_slope_by_dem_resolution\';
       SafeMakeDir(CSVDir);
       for I := 0 to pred(Areas.Count) do begin
          AreaName := Areas.Strings[i];
          fName := CSVdir + AreaName + '_gdem_compare_tile_slope.csv';
          if FileExists(fName) then begin

          end
          else begin
            TStr := IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName;
            wmDEM.SetPanelText(1,TStr,true);
            Results := tStringList.Create;
            Results.Add('DEMIX_TILE,AREA,COUNTRY,LAT,LONG,CRITERION,AVG_SLOPE,BARREN_PC,FOREST_PC,GRID_SEC,REF_DSM,REF_DTM,COP,ALOS,FATHOM');

            theFilter := AddAndIfNeeded(BaseFilter) + 'AREA=' + QuotedStr(AreaName);
            GISdb[DBonTable].ApplyGISFilter(theFilter);
            {$IfDef RecordDTM_DSM} WriteLineToDebugFile(IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName + '  filter=' + theFilter + '  tiles=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
            j := 0;
            while not GISdb[DBonTable].MyData.eof do begin
                inc(j);
                wmDEM.SetPanelText(0,TimeToStr(Now),true);
                wmDEM.SetPanelText(2,IntToStr(j) + '/' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ' ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME'),true);
                cname := ExtractFilePath(GISdb[DBonTable].DBFullName) + AreaName + '\' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DSM_NAME') + RefTestStr +'\';
                RefDSMName := cName + 'ref_dsm_cop.tif';
                if not FileExists(RefDSMname) then RefDSMName := cName + 'ref_dsm_srtm.tif';;

                RefALOS_DSM := cName + 'ref_dsm_alos.tif';

                cname := ExtractFilePath(GISdb[DBonTable].DBFullName) + AreaName + '\' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME') + RefTestStr +'\';
                RefDTMName := cName + 'ref_dtm_cop.tif';
                if not FileExists(RefDTMname) then RefDTMName := cName + 'ref_dtm_srtm.tif';;
                RefALOS_DTM := cName + 'ref_dtm_alos.tif';

                CopName := cName + 'cop.tif';
                ALOSname := CName + 'alos.tif';
                Fathomname := cName + 'fathom.tif';

                AllValid := true;
                TryOpening(1,RefDSMName);
                TryOpening(2,RefDTMName);
                TryOpening(3,CopName);
                TryOpening(4,alosName);
                TryOpening(5,FathomName);

                if AllValid then begin //loop processes a tile
                    for thin := 1 to 6 do begin
                       aLine[thin] := GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME') + ',' + AreaName + ',' +  GISdb[DBonTable].MyData.GetFieldByNameAsString('COUNTRY') + ',' +
                           GISdb[DBonTable].MyData.GetFieldByNameAsString('LAT') + ',' +
                           GISdb[DBonTable].MyData.GetFieldByNameAsString('LONG') + ',RES_SLOPE,' + CommonTileStats(DBonTable);
                    end;
                    for thin := 1 to 6 do begin
                        AddAThinLevel(thin);
                    end;
                    for thin := 1 to 6 do begin
                       Results.Add(aline[thin]);
                       aline[thin] := '';
                    end;
                end
                else begin
                   {$If Defined(RecordDTM_DSM)} WriteLineToDebugFile('MissingDEMs, ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME')); {$EndIf}
                end;

                for DEM := 1 to 5 do begin
                   CloseSingleDEM(DEMS[DEM]);
                end;
                GISdb[DBonTable].MyData.Next;
             end {while};
             Results.SaveToFile(fName);
             Results.Destroy;
          end;
       end {for i};
   finally
       wmDEM.ClearStatusBarPanelText;
       GISdb[DBonTable].ClearGISFilter;
       SetColorForWaiting;
       ShowDefaultCursor;
       HeavyDutyProcessing := false;
   end;
   {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} WriteLineToDebugFile('MakeCSVforGDEMcompareDSMtoDTM out, saved ' + fName); {$EndIf}
end {procedure MakeCSVforGDEM_average_tile_slope};


procedure MakeCSVforGDEMcompareDSMtoDTM(DBonTable : integer);
var
   Areas,Results,Tiles : tStringList;
   AreaName,BaseFilter,TStr,theFilter,TileStats : shortstring;
   Success,Failures,
   i,j : integer;
   RefALOS_DSM,RefALOS_DTM,AlosName,BaseDir,
   fName,RefDSMName,RefDTMName,CopNAME,FathomName,cName,tName : PathStr;
begin
  {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} HighLightInDebugFile('MakeCSVforGDEMcompareDSMtoDTM in ' + GISdb[dbOnTable].dbName); {$EndIf}
  if not (GISdb[DBonTable].MyData.FieldExists('DEM_PAIR') and GISdb[DBonTable].MyData.FieldExists('REF_DEM')) then begin
      if AnswerIsYes('DB requires fields DEM_PAIR and REF_DEM; add now') then IdentifyDSM_DTMpair(DBonTable)
      else exit;
   end;
   try
       SetColorForProcessing;
       WantShowProgress := false;
       HeavyDutyProcessing := true;
       if (MDDef.DEMIX_BaseDir = '') then MDDef.DEMIX_BaseDir := ExtractFilePath(GISdb[DBonTable].dbFullName);
       BaseFilter := 'DEM_PAIR=' + QuotedStr('YES') + ' AND REF_DEM=' + QuotedStr('DTM') ;
       GISdb[DBonTable].ApplyGISFilter(BaseFilter );
       Areas := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('AREA');
       BaseDir := ExtractFilePath(GISdb[DBonTable].DBFullName) + 'aa_gdem_compare_dsm_dtm\';
       //SafeMakeDir(fName);
       for I := 0 to pred(Areas.Count) do begin
          AreaName := Areas.Strings[i];
          TStr := IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName;
          wmDEM.SetPanelText(1,TStr,true);
          fName := BaseDir + AreaName + '_gdem_compare_dsm_dtm.csv';
          if FileExists(fName) then begin
              {$IfDef RecordRecordDTM_DSM} WriteLineToDebugFile(IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName + '  exists already'); {$EndIf}
          end
          else begin
              Results := nil;
              theFilter := AddAndIfNeeded(BaseFilter) + 'AREA=' + QuotedStr(AreaName);
              {$IfDef RecordDTM_DSM} WriteLineToDebugFile(IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName + '  filter=' + theFilter); {$EndIf}
              GISdb[DBonTable].ApplyGISFilter(theFilter);
              Success := 0;
              Failures := 0;
              j := 0;
              while not GISdb[DBonTable].MyData.eof do begin
                  inc(j);
                  wmDEM.SetPanelText(0,TimeToStr(Now),true);
                  cname := ExtractFilePath(GISdb[DBonTable].DBFullName) + AreaName + '\' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DSM_NAME') + RefTestStr +'\';
                  RefDSMName := cName + 'ref_dsm_cop.tif';
                  if not FileExists(RefDSMname) then RefDSMName := cName + 'ref_dsm_srtm.tif';;

                  RefALOS_DSM := cName + 'ref_dsm_alos.tif';

                  cname := ExtractFilePath(GISdb[DBonTable].DBFullName) + AreaName + '\' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME') + RefTestStr +'\';
                  RefDTMName := cName + 'ref_dtm_cop.tif';
                  if not FileExists(RefDTMname) then RefDTMName := cName + 'ref_dtm_srtm.tif';;
                  RefALOS_DTM := cName + 'ref_dtm_alos.tif';

                  CopName := cName + 'cop.tif';
                  ALOSname := CName + 'alos.tif';
                  Fathomname := cName + 'fathom.tif';

                  if FileExists(RefDSMName) and FileExists(RefDTMname) and FileExists(CopName) then begin
                      TStr := IntToStr(j) + '/' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ' ' + ExtractFileNameNoExt(RefDTMname);
                      wmDEM.SetPanelText(2,TStr,true);
                      inc(Success);
                      {$IfDef RecordDTM_DSM} WriteLineToDebugFile(AreaName + ' ' + TStr); {$EndIf}
                      TileStats := CommonTileStats(dbOnTable);
                      MakeSingleAreaDSMDTMcomparison(false,0,CopName,RefDSMname,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,0,Copname,RefDTMName,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,0,FathomName,RefDSMname,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,0,FathomName,RefDTMName,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,0,RefDSMname,RefDTMName,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,0,ALOSName,RefALOS_DSM,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,0,ALOSName,RefALOS_DTM,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                  end
                  else begin
                     inc(Failures);
                     TStr := 'Files missing: ';
                     if not FileExists(RefDSMName) then TStr := TStr + RefDSMName + '  ';
                     if not FileExists(RefDTMName) then TStr := TStr + RefDTMName + '  ';
                     if not FileExists(CopName) then TStr := TStr + CopName + '  ';
                     HighLightInDebugFile(TStr);
                  end;
                  GISdb[DBonTable].MyData.Next;
              end {while};
              if (Results <> nil) and (Results.Count > 1) then begin
                 Results.SaveToFile(fName);
                 Results.Destroy;
              end;
             {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} WriteLineToDebugFile(AreaName + '  good=' + IntToStr(Success) + '  fail=' + IntToStr(Failures)); {$EndIf}
          end;
       end;
   finally
       wmDEM.ClearStatusBarPanelText;
       GISdb[DBonTable].ClearGISFilter;
       SetColorForWaiting;
       ShowDefaultCursor;
       HeavyDutyProcessing := false;
   end;
   {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} WriteLineToDebugFile('MakeCSVforGDEMcompareDSMtoDTM out, saved ' + fName); {$EndIf}
end {procedure MakeCSVforGDEMcompareDSMtoDTM};


procedure ChangeDBtoPrivateVersionForMultithreading(var db : integer);
var
   fName : PathStr;
begin
   fName := NextFileNumber(MDtempdir,'multithread_working_db_','.dbf');
   GISdb[db].SaveCurrentDBaseSubset(fName);
   OpenNumberedGISDataBase(db,fName,false);
end;


procedure MakeCSVforHRDEMComparison(DB : integer);
//input the tile stats database
var
   Areas,Results : tStringList;
   AreaName,BaseFilter,TStr,theFilter,TileStats,Common,Country,IncomingFilter : shortstring;
   i,j,inDB,LC : integer;
   fName,DSMName,DTMName,AreaLock,BaseDir : PathStr;
begin
   {$IfDef RecordDEMIX} HighLightInDebugFile('MakeCSVforHRDEMComparison in ' + GISdb[db].DBName); {$EndIf}
   if not(GISdb[db].MyData.FieldExists('DEM_PAIR') and GISdb[db].MyData.FieldExists('REF_DEM')) then begin
       {$IfDef RecordDEMIX} HighLightInDebugFile('MakeCSVforHRDEMComparison need to ID pairs'); {$EndIf}
       IdentifyDSM_DTMpair(DB);
   end;
   if GISdb[db].MyData.FieldExists('DEM_PAIR') and GISdb[db].MyData.FieldExists('REF_DEM') then begin
       inDB := DB;
       WantShowProgress := false;
       HeavyDutyProcessing := true;
       SetColorForProcessing;
       IncomingFilter := GISdb[db].MyData.Filter;
       BaseFilter := AddAndIfNeeded(IncomingFilter) + 'DEM_PAIR=' + QuotedStr('YES') + ' AND REF_DEM=' + QuotedStr('DTM') ;
       GISdb[db].ApplyGISFilter(BaseFilter );
       Areas := GISdb[db].MyData.ListUniqueEntriesInDB('AREA');
       BaseDir := ExtractFilePath(GISdb[db].DBFullName) + 'aa_hrdem_compare_dsm_dtm\';
       SafeMakeDir(BaseDir);
       {$IfDef RecordDEMIX} HighLightInDebugFile('Areas=' + IntToStr(Areas.Count)); {$EndIf}
       if (Areas.Count = 0) then begin
          MessageToContinue('No areas in ' + GISdb[db].dbName);
       end
       else begin
           ChangeDBtoPrivateVersionForMultithreading(db);
           for I := pred(Areas.Count) downto 0 do begin
              AreaName := Areas.Strings[i];
              fName := BaseDir + AreaName + '_compare_dsm_dtm' + '.csv';
              if FileExists(fName) then Areas.Delete(i);
           end;
           for I := 0 to pred(Areas.Count) do begin
              AreaName := Areas.Strings[i];
              fName := BaseDir + AreaName + '_compare_dsm_dtm' + '.csv';
              AreaLock := ExtractFilePath(GISdb[indb].DBFullName) + AreaName + '\aa_arealocked.txt';
              if not FileExists(AreaLock) then begin
                Areas.SaveToFile(AreaLock);
                if not FileExists(fName) then begin
                  TStr := IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName;
                  wmDEM.SetPanelText(1,TStr,true);
                  theFilter := AddAndIfNeeded(BaseFilter) + 'AREA=' + QuotedStr(AreaName);
                  {$IfDef RecordDTM_DSM} WriteLineToDebugFile(TStr + '  creating with ' + theFilter); {$EndIf}
                  GISdb[db].ApplyGISFilter(theFilter);
                  if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
                      Results := nil;
                      j := 0;
                      while not GISdb[db].MyData.eof do begin
                          wmDEM.SetPanelText(0,TimeToStr(Now),true);
                          inc(j);
                          Country := UpperCase(GISdb[db].MyData.GetFieldByNameAsString('COUNTRY'));

                          Common := ExtractFilePath(GISdb[inDB].DBFullName) + AreaName + '\wgs_egm\';
                          DSMName := Common + GISdb[db].MyData.GetFieldByNameAsString('DSM_NAME') + '.tif';
                          DTMName := Common + GISdb[db].MyData.GetFieldByNameAsString('DTM_NAME') + '.tif';

                          TStr := IntToStr(j) + '/' + IntToStr(GISdb[db].MyData.FiltRecsInDB) + ' ' + ExtractFileNameNoExt(DTMname) + ' ' + AvailPhysMemStr;
                          wmDEM.SetPanelText(2,TStr,true);
                          {$IfDef RecordDTM_DSM} WriteLineToDebugFile(AreaName + ' ' + TStr); {$EndIf}
                          MakeSingleAreaDSMDTMcomparison(true,LC,DSMname,DTMName,'',AreaName,Country,CommonTileStats(db),Results);
                          CloseSingleDEM(LC);
                          GISdb[db].MyData.Next;
                      end {while};
                     {$IfDef RecordDTM_DSM} if (Results <> Nil) then WriteLineToDebugFile('Tiles=' + IntToStr(Results.Count) + '  ' + fName); {$EndIf}
                     Results.SaveToFile(fName);
                     Results.Destroy;
                  end;
                  CleanUpTempDirectory;
              end;
              DeleteFileIfExists(AreaLock);
           end;
       end;
       CloseSingleDB(db);
       wmDEM.ClearStatusBarPanelText;
       GISdb[indb].ApplyGISFilter(IncomingFilter);
       SetColorForWaiting;
       HeavyDutyProcessing := false;
       ShowDefaultCursor;
       {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeCSVforHRDEMComparison out'); {$EndIf}
     end
   end
   else begin
      MessageToContinue('DB missing required fields DEM_PAIR and REF_DEM');
      {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeCSVforHRDEMComparison fail, missing fields'); {$EndIf}
   end;
end {procedure MakeCSVforHRDEMComparison};

procedure ComputePowerFitForAllTiles(db : integer; PowerLaw : boolean = true);
//input is GDEM or HRDEM DB for grid spacings in meters or arc seconds; uses only those spacings currently
//makes DB with statistics for a power law fit for the DEMs hard coded into theDEMs string list
var
   TileList,Results,theDEMs : tStringList;
   DTMName,aline,BaseFilter,SpacingField,TileField,ResName,TStr,af,aDEM : shortstring;
   fName : PathStr;
   i,j : integer;

         procedure OneKindDEM(theDEM : shortString);
         var
            x,y : array[0..25] of float32;
            a,b,siga,sigb,r : float64;
            n : integer;
         begin
            n := -1;
            GISdb[db].MyData.First;
            while not GISdb[db].MyData.eof do begin
               inc(n);
               x[n] := GISdb[db].MyData.GetFieldByNameAsFloat(SpacingField);
               y[n] := GISdb[db].MyData.GetFieldByNameAsFloat(theDEM);
               GISdb[db].MyData.Next;
            end;
            if PowerLaw then begin
               PowerFit(x,y,n,a,b,siga,sigb,r);
               aline := aLine + ',' + RealToString(sqr(r),-12,-4) + ',' + RealToString(a,-12,-4) + ',' + RealToString(b,-12,-4);
            end
            else begin
               LinearFit(x,y,n,a,b,siga,sigb,r);
               aline := aLine + ',' + RealToString(sqr(r),-12,-4) + ',' + RealToString(a,-12,-4) + ',' + RealToString(b,-12,-4);
            end;
         end;

begin {procedure ComputePowerFitForAllTiles}
   if (GISdb[db].MyData.FieldExists('GRID_M') or GISdb[db].MyData.FieldExists('GRID_SEC')) then begin
      {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('ComputePowerFitForAllTiles in'); {$EndIf}
       theDEMs := CreatePowerLawList(db);
       if GISdb[db].MyData.FieldExists('GRID_M') then begin
          SpacingField := 'GRID_M';
          ResName := 'HRDEM';
          TileField := 'DTM_NAME';
          aLine := 'DTM_NAME,DSM_NAME,';
       end
       else if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
          SpacingField := 'GRID_SEC';
          ResName := 'GDEM';
          TileField := 'DEMIX_TILE';
          aLine := 'DEMIX_TILE,';
       end;
       aline := aLine + 'AREA,COUNTRY,LAT,LONG,' + CommonTileStatsNames(DB);
       if PowerLaw then begin
          for i := 0 to pred(theDEMs.Count) do begin
             aLine := aline + ',' + theDEMs.strings[i] + '_R2'  + ',' + theDEMs.strings[i] + '_A'  + ',' + theDEMs.strings[i] + '_B';
          end;
       end
       else begin
          for i := 0 to pred(theDEMs.Count) do begin
             aLine := aline + ',' + theDEMs.strings[i] + '_R2'  + ',' + theDEMs.strings[i] + '_B'  + ',' + theDEMs.strings[i] + '_M';
          end;
       end;
       Results := tStringList.Create;
       Results.Add(aLine);

       SetColorForProcessing;
       BaseFilter := GISdb[db].MyData.Filter;
       GISdb[db].EmpSource.Enabled := false;
       TileList := GISdb[db].MyData.ListUniqueEntriesInDB(TileField);
       for I := 0 to pred(TileList.Count) do begin
          DTMname := TileList.Strings[i];
          wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(TileList.Count) + ' ' + DTMname,true);
          af := AddAndIfNeeded(BaseFilter) + TileField + '=' + QuotedStr(DTMname) + HRDEMresolutionFilter(db);
          GISdb[db].ApplyGISFilter(af);
          GISdb[db].EmpSource.Enabled := false;
          if (TileField = 'DTM_NAME') then TStr := GISdb[db].MyData.GetFieldByNameAsString('DSM_NAME')  + ','
          else TStr := '';
          aLine := DTMName + ',' + TStr + GISdb[db].MyData.GetFieldByNameAsString('AREA') + ',' + GISdb[db].MyData.GetFieldByNameAsString('COUNTRY') + ',' +
             GISdb[db].MyData.GetFieldByNameAsString('LAT')  + ',' + GISdb[db].MyData.GetFieldByNameAsString('LONG') + ',' + CommonTileStats(db);
          for j := 0 to pred(theDEMs.Count) do begin
             aDEM := theDEMs.Strings[j];
             if not GISdb[db].MyData.FieldExists(aDEM) then aDEM := aDEM + '_SLOPE';   //for the HRDEM case
             OneKindDEM(aDEM);
          end;
          Results.Add(aLine);
       end;
       theDEMs.Destroy;
       if PowerLaw then TStr := 'power_law_' else TStr := 'linear_fit_';
       fName := ExtractFilePath(GISdb[db].dbFullName) + ResName + '_' + TStr + DEMIXcreationString(db) + '.dbf';
       Results.SaveToFile(MDtempDir + 'power_law_fit_results.csv');
       StringList2CSVtoDB(Results,fName);
       wmdem.ClearStatusBarPanelText;
       GISdb[db].ApplyGISFilter(BaseFilter);
       SetColorForWaiting;
       {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('ComputePowerFitForAllTiles out'); {$EndIf}
   end
   else begin
      MessageToContinue('Required spacing field missing');
   end;
end {procedure ComputePowerFitForAllTiles};


procedure ComputePowerBySlopeBins(db : integer);
//input is GDEM or HRDEM DB for grid spacings in meters or arc seconds; uses only those spacings currently
//makes DB with statistics for a power law fit for the DEMs hard coded into theDEMs string list
var
   TileList,Results,theDEMs : tStringList;
   DTMName,aline,BaseFilter,SpacingField,TileField,ResName,TStr,af,sf : shortstring;
   fName : PathStr;
   i,j,n,MaxSlope,MinSlope : integer;

         procedure OneKindDEM;
         var
            x,y : array[0..2500] of float32;
            a,b,siga,sigb,r : float64;
            n : integer;
         begin
            n := -1;
            GISdb[db].MyData.First;
            while not GISdb[db].MyData.eof do begin
               inc(n);
               x[n] := GISdb[db].MyData.GetFieldByNameAsFloat(SpacingField);
               y[n] := GISdb[db].MyData.GetFieldByNameAsFloat('DTM_SLOPE');
               GISdb[db].MyData.Next;
            end;
               PowerFit(x,y,n,a,b,siga,sigb,r);
               aline := aLine + ',' + RealToString(sqr(r),-12,-4) + ',' + RealToString(a,-12,-4) + ',' + RealToString(b,-12,-4);
         end;

begin {procedure ComputePowerBySlopeBins}
   if (GISdb[db].MyData.FieldExists('GRID_M') or GISdb[db].MyData.FieldExists('GRID_SEC')) then begin
      {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('ComputePowerFitForAllTiles in'); {$EndIf}
       theDEMs := CreatePowerLawList(db);
       if GISdb[db].MyData.FieldExists('GRID_M') then begin
          SpacingField := 'GRID_M';
          ResName := 'HRDEM';
          TileField := 'DTM_NAME';
       end
       else if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
          SpacingField := 'GRID_SEC';
          ResName := 'GDEM';
          TileField := 'DEMIX_TILE';
          aLine := 'DEMIX_TILE,';
       end;
       aLine := 'SLOPES,MIN_SLOPE,MAX_SLOPE,N,DTM_R2,DTM_A,DTM_B';

       Results := tStringList.Create;
       Results.Add(aLine);

       SetColorForProcessing;
       BaseFilter := GISdb[db].MyData.Filter;
       GISdb[db].EmpSource.Enabled := false;
       MaxSlope := 75;
       repeat
          MinSlope := MaxSlope - 5;
          sf := 'AVG_SLOPE<' + IntToStr(MaxSlope) + ' AND AVG_SLOPE>' + IntToStr(MinSlope);
          wmDEM.SetPanelText(2,sf,true);
          af := AddAndIfNeeded(BaseFilter) + sf + HRDEMresolutionFilter(db);
          GISdb[db].ApplyGISFilter(af);
          n := GISdb[db].MyData.NumUniqueEntriesInDB(TileField);
          aLine := sf + ',' + IntToStr(MinSlope) + ',' + IntToStr(MaxSlope) + ',' +IntToStr(n);
          GISdb[db].EmpSource.Enabled := false;
          OneKindDEM;
          Results.Add(aLine);
          dec(MaxSlope,5);
       until MaxSlope <= 1;

       fName := ExtractFilePath(GISdb[db].dbFullName) + RESName + '_power_law_by_slope_bins_' + DEMIXcreationString(db) + '.dbf';
       //Results.SaveToFile(MDtempDir + 'power_law_fit_results.csv');
       StringList2CSVtoDB(Results,fName);
       wmdem.ClearStatusBarPanelText;
       GISdb[db].ApplyGISFilter(BaseFilter);
       SetColorForWaiting;
       {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('ComputePowerFitForAllTiles out'); {$EndIf}
   end
   else begin
      MessageToContinue('Required spacing field missing');
   end;
end {procedure ComputePowerBySlopeBins};


const
   DoLandTypes = false;
   DoDSM = false;

procedure MakeDBForParamStats(Option,DBonTable : integer);
var
   Results,UsingFields,UniqueEntries : tStringList;
   i,j : integer;
   aLine,MaxLine,MeanLine,MedianLine,MinLine,stdLine,Common : shortstring;
   MomentVar : tMomentVar;
   aField : shortstring;
   fName : PathStr;
   Color : tColor;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   if (Option = opByCluster) then aField := 'CLUSTER'
   else if (Option = opByDEM) then aField := 'DEM';

   if not GISdb[DBonTable].MyData.FieldExists(aField) then begin
      MessageToContinue('Option requires field in DB:' + aField);
      exit;
   end;

   if (Option = opByCluster) then begin
      UniqueEntries := GISdb[DBonTable].MyData.ListUniqueEntriesInDB(aField);
      SortStringListNumerically(UniqueEntries);
   end
   else if (Option = opByDEM) then begin
      UniqueEntries := GISdb[DBonTable].MyData.ListUniqueEntriesInDB(aField);
   end;

   UsingFields := GISdb[DBonTable].GetAnalysisFields;

   Results := tStringList.Create;
   aline := aField + ',COLOR,N,METRIC';
   for j := 0 to pred(UsingFields.Count) do begin
      aline := aline + ',' + UsingFields.Strings[j];
   end;
   Results.Add(aLine);
   for i := 0 to pred(UniqueEntries.Count) do begin
      if Option = opByCluster then begin
         GISdb[DBonTable].MyData.ApplyFilter(aField + '=' + UniqueEntries.Strings[i]);
         Color := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('COLOR');
      end;
      if Option = opByDEM then begin
         GISdb[DBonTable].MyData.ApplyFilter(aField + '=' + QuotedStr(UniqueEntries.Strings[i]));
         Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(UniqueEntries.Strings[i]));
      end;
      GISdb[DBonTable].Empsource.Enabled := false;
      Common := UniqueEntries.Strings[i] + ',' + IntToStr(Color) + ','  + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ',' ;
      MaxLine := Common + 'MAX' ;
      MeanLine := Common + 'MEAN' ;
      MedianLine := Common + 'MEDIAN';
      MinLine := Common + 'MIN';
      STDline := Common + 'StdDev';

      for j := 0 to pred(UsingFields.Count) do begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         MomentVar := GISdb[DBonTable].GetFieldStatistics(UsingFields.Strings[j]);
         GISdb[DBonTable].Empsource.Enabled := false;
         MaxLine := MaxLine + ',' + RealToString(MomentVar.MaxZ,-18,-4);
         MeanLine := MeanLine + ',' + RealToString(MomentVar.Mean,-18,-6);
         MedianLine := MedianLine + ','  + RealToString(MomentVar.Median,-18,-6);
         MinLine := MinLine + ',' +  RealToString(MomentVar.MinZ,-18,-4);
         STDLine := STDLine + ',' +  RealToString(MomentVar.std_dev,-18,-6);
      end;
      Results.Add(MaxLine);
      Results.Add(MeanLine);
      Results.Add(MedianLine);
      Results.Add(MinLine);
      Results.Add(STDLine);
   end;
   fName := Petmar.NextFileNumber(MDTempDir, 'stats_by_' + aField + '_' ,DefaultDBExt);
   {$IfDef RecordClustering} WriteLineToDebugFile(fName); {$EndIf}
   GISdb[DBonTable].theMapOwner.StringListToDBonMap(Results,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end {procedure MakeDBForParamStats};


procedure DEMIX_SSIM_FUV_transpose_kmeans_new_db(DBonTable : integer);
//transpose DB so it has one TILE/DEM pair on each line, Criteria as columns
var
   values : array[1..9,0..25] of float32;
   Tile,Crit,aline : shortstring;
   i,j,k,N : integer;
   theTiles,sl,Criteria : tStringList;
   fName: PathStr;
begin
   {$If Defined(RecordDEMIX)} HighLightInDebugFile('DEMIX_SSIM_FUV_transpose_kmeans_new_db in'); {$EndIf}
   try
      if GISdb[DBonTable].MyData.Filtered then begin
         if not AnswerIsYes('Transpose filtered DB') then GISdb[DBonTable].ClearGISFilter;
      end;

      GetDEMIXpaths;
      GISdb[DBonTable].EmpSource.Enabled := false;
      theTiles := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
      GISdb[DBonTable].EmpSource.Enabled := false;
      Criteria := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('CRITERION');
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('   tiles=' + IntToStr(TheTiles.Count)); {$EndIf}
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('   criteria=' + IntToStr(Criteria.Count)); {$EndIf}

      sl := tStringList.Create;
      aline := 'DEMIX_TILE,DEM';
      for I := 0 to pred(Criteria.Count) do aline := aline + ',' + Criteria.Strings[i];
      sl.Add(Aline);

      LoadDEMIXnames;
      StartProgress('SSIM/FUV summary');
      for k := 0 to pred(theTiles.Count) do begin
         if (k mod 10 = 0) then wmdem.SetPanelText(1,IntToStr(k) + '/' + IntToStr(TheTiles.Count) + ' tiles transpose');
         Tile := theTiles[k];
         GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tile));
         GISdb[DBonTable].EmpSource.Enabled := false;
         if GISdb[DBonTable].MyData.FiltRecsInDB > 5 then begin
            for i := 1 to 9 do
               for j := 0 to 25 do
                  values[i,j] := -999;
            while not GISdb[DBonTable].MyData.eof do begin
               Crit := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION'));
               N := Criteria.IndexOf(Crit);
               for I := 1 to NumDEMIXtestDEM do begin
                  Values[i,N] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
               end;
               GISdb[DBonTable].MyData.Next;
            end;

            for I := 1 to NumDEMIXtestDEM do begin
               aline := Tile + ',' + DEMIXShort[i];
               for j := 0 to  pred(Criteria.Count) do aline := aline + ',' + RealToString(Values[i,j],-18,6);
               sl.Add(Aline);
            end;
         end;
      end;
      fName := DEMIX_final_DB_dir + 'transpose_' + GISdb[DBonTable].dbName + '.csv';  //NextFileNumber(MDTempDir,MDTempDir + 'ssim_FUV_clustering_','.dbf');
      StringList2CSVtoDB(sl,fName);
   finally
      EndDEMIXProcessing;
      GISdb[DBonTable].ClearGISFilter;
   end;
   {$If Defined(RecordDEMIX)} HighLightInDebugFile('DEMIX_SSIM_FUV_transpose_kmeans_new_db out, ' + fName); {$EndIf}
end;



procedure CreateFinalDiffDistDB;
var
   Areas,Merges,csv : tStringList;
   AreaName : shortstring;
   MergeName : PathStr;
   j,db : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CreateFinalDB in'); {$EndIf}
   GetDEMIXpaths;
   ShowHourglassCursor;
   Areas := DEMIX_AreasWanted;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV picked areas=' + IntToStr(Areas.Count)); {$EndIf}
   try
      Merges := tStringList.Create;
      for j := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[j];
         MergeName := Diff_dist_results_dir + AreaName + '_merge_transpose.csv';
         if FileExists(MergeName) then Merges.Add(MergeName);
      end;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Files to merge=' + IntToStr(Merges.Count)); {$EndIf}
      MergeName := DEMIX_final_DB_dir + DEMIXmodeName + '_diff_dist_demix_db_' + CurrentTimeForFileName(false) + '.csv';
      MergeCSVFiles(Merges,MergeName);
      OpenNumberedGISDataBase(db,MergeName,true);
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CreateFinalDB out, created ' + MergeName); {$EndIf}
end;


procedure MergeCSVtoCreateFinalDB(CSVdir : PathStr; FileDescription,dbName : shortstring);
var
   Areas,Merges,Missing : tStringList;
   AreaName : shortstring;
   MergeName : PathStr;
   j,db : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CreateFinalDB in for ' + CSVDir); {$EndIf}
   try
      ShowHourglassCursor;
      Merges := tStringList.Create;
      FindMatchingFiles(CSVDir,'*.csv',Merges,0);
      if (Merges.Count > 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CreateFinalDB out, created ' + MergeName + ' from areas=' + IntToStr(Merges.Count)); {$EndIf}
         MergeName := DEMIX_final_DB_dir + DEMIXmodeName + dbName + CurrentTimeForFileName(false) + '.csv';
         MergeCSVFiles(Merges,MergeName);
         OpenNumberedGISDataBase(db,MergeName,true);
         DeleteFileIfExists(MergeName);
         FilterTableForDEMIXevaluation(db,0);
         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
            if AnswerIsYes('Missing records in DB=' + IntToStr(GISdb[db].MyData.FiltRecsInDB) + '; Delete them') then
               FilterTableForDEMIXevaluation(DB,0);
         end
         else GISdb[db].ClearGISFilter;
      end
      else begin
         Merges.Destroy;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No FinalDB created'); {$EndIf}
      end;
   finally
      EndDEMIXProcessing;
   end;
end;


procedure MergeCSV(Mode : integer);
begin
   MDDef.DEMIX_all_areas := true;
   SetParamsForDEMIXmode;
   GetDEMIXpaths;
   case Mode of
      1 : MergeCSVtoCreateFinalDB(FUVresultsDir,'_fuv_results.csv','_fuv_demix_db_');
      2 : MergeCSVtoCreateFinalDB(SSIMresultsDir,'_ssim_results.csv','_ssim_demix_db_');
      {$IfDef IncludeVectorCriteria}
         3 : MergeCSVtoCreateFinalDB(ChannelMissesDir,'_Channel_Misses.csv','_channel_misses_' );
         5 : MergeCSVtoCreateFinalDB(Stream_valley_dir,'_ridges_valleys.csv','_ridges_valleys_' );
      {$EndIf}
      4 : MergeCSVtoCreateFinalDB(GeomorphonsDir,'_geomorphons.csv','_pt_class_demix_db_');
      6 : MergeCSVtoCreateFinalDB(PartialsResultsDir,'_partials_results.csv','_partials_demix_db_');
      7 : MergeCSVtoCreateFinalDB(CurvaturesResultsDir,'_curvatures_results.csv','_curvatures_demix_db_');
   end;
end;



procedure ModeOfDifferenceDistributions;
var
   FilesWanted,Modes,TileList : tStringList;
   fName,fName2 : PathStr;
   i,j,NPts,db,atile : integer;
   BinSize,Mode : float32;
   Tile,param,Ref,aLine,Area : shortstring;
   Values : ^bfarray32;
   MomentVar : tMomentVar;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ModeOfDifferenceDistributions in'); {$EndIf}
   try
      if not Petmar.GetExistingFileName('Ordered list of tiles','*.txt',fName) then exit;
      GetDEMIXpaths;
      Modes := tStringList.Create;
      aLine := 'AREA,TILE,REF_TYPE,CRITERION';
      for I := 1 to NumDEMIXtestDEM do begin
         aline := aline + ',' + DEMIXshort[i] + '_MODE' + ',' + DEMIXshort[i] + '_MEAN' + ',' + DEMIXshort[i] + '_MEDN' + ',' + DEMIXshort[i] + '_STD';
      end;
      Modes.Add(aLine);
      TileList := tStringList.Create;
      TileList.LoadFromFile(fName);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('tiles=' + IntToStr(TileList.Count)); {$EndIf}
      FilesWanted := tStringList.Create;
      FindMatchingFiles(DEMIX_diff_dist,'*.z',FilesWanted,0);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('z files=' + IntToStr(FilesWanted.Count)); {$EndIf}

      StartProgress('Modes');
      for atile := 0 to pred(TileList.Count) do begin
         UpdateProgressBar(aTile/TileList.Count);
         for i := 0 to pred(FilesWanted.Count) do begin
            fName := upperCase(FilesWanted.Strings[i]);
            if StrUtils.AnsiContainsText(fname,TileList[aTile]) then begin
               if StrUtils.AnsiContainsText(fname,'COP') then begin
                  Area := BeforeSpecifiedString(ExtractFileName(FilesWanted.Strings[i]),'_COP');
                  Tile := ExtractFileName(fName);
                  Tile := AfterSpecifiedString(Tile,'COP_');
                  Tile := Copy(Tile,1,9);
                  if StrUtils.AnsiContainsText(fname,'DSM') then Ref := 'DSM' else Ref := 'DTM';

                  if StrUtils.AnsiContainsText(fname,'ELEV') then begin
                     BinSize := 0.10;
                     Param := 'elvd_mode';
                  end;

                  if StrUtils.AnsiContainsText(fname, 'SLOPE') then begin
                     BinSize := 0.25;
                     Param := 'slpd_mode';
                  end;
                  if StrUtils.AnsiContainsText(fname, 'RUFF') then begin
                     BinSize := 0.15;
                     Param := 'rufd_mode';
                  end;
                  aLine := Area + ',' + Tile + ',' + Ref + ',' + Param ;

                  for j := 1 to NumDEMIXtestDEM do begin
                     fName2 := StringReplace(fName,'COP',DEMIXDEMTypeName[j],[rfReplaceAll, rfIgnoreCase]);
                     if FileExists(fName2) then begin
                        New(Values);
                        LoadBFarray32(fName2,Values^,npts);
                        Mode := PetMath.Mode(Values^,npts,binsize);
                        InitializeMomentVar(MomentVar);
                        MomentVar.NPts := Npts;
                        moment(Values^,MomentVar,msAll);
                        Dispose(Values);
                     end
                     else Mode := -999;
                     aLine := aLine + ',' + RealToString(Mode,-8,-2) + ',' + RealToString(MomentVar.Mean,-8,-2) + ',' + RealToString(MomentVar.Median,-8,-2) + ',' + RealToString(MomentVar.std_dev,-8,-2);
                  end;
                  Modes.Add(aline);
               end;
            end;
         end;
      end;
      fName := NextFileNumber(MDTempDir,'distrib_diff_modes_','.dbf');
      db := StringList2CSVtoDB(Modes,fName);
   finally
      TileList.Free;
      FilesWanted.Free;
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ModeOfDifferenceDistributions out'); {$EndIf}
end;



procedure FindTilesInAreaForCoast;
const
   FilterHeight : array[2..4] of float64  = (120,80,10);
var
   i,j,DEM,DB : integer;
   Fixed : int64;
   AreasWanted : tStringList;
   Findings : tStringList;
   aLine,AreaName : shortstring;
   fName : PathStr;
begin {FindTilesInAreaForCoast}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('Start FindTilesInAreaForCoast'); {$EndIf}
   try
      GetDEMIXpaths;
      AreasWanted := DEMIX_AreasWanted;
      Findings := tStringList.Create;
      Findings.Add('AREA,COUNTRY,ALL_75,ALL_50,ALL_25,U120_75,U120_50,U120_25,U80_75,U80_50,U80_25,U10_75,U10_50,U10_25');
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         aLine := AreaName + ',' + GetCountryForArea(AreaName);
         wmdem.SetPanelText(1,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + '  ' + AreaName);
         fName := DEMIX_Ref_1sec + AreaName + '_dtm' + Ref1SecPointStr + '.tif';
         DEM := OpenNewDEM(fName);
         for j := 1 to 4 do begin
            if (j > 1) then DEMGlb[DEM].MarkAboveMissing(FilterHeight[j],Fixed,false);
            DB := DEMIXtileFill(DEM,DEMGlb[DEM].DEMBoundBoxGeo,false);
            GISdb[DB].ApplyGISFilter('GRID_FULL>' + IntToStr(75));
            aline := aLine + ',' + IntToStr(GISdb[DB].MyData.FiltRecsInDB);
            GISdb[DB].ApplyGISFilter('GRID_FULL>' + IntToStr(50));
            aline := aLine + ',' + IntToStr(GISdb[DB].MyData.FiltRecsInDB);
            GISdb[DB].ApplyGISFilter('GRID_FULL>' + IntToStr(25));
            aline := aLine + ',' + IntToStr(GISdb[DB].MyData.FiltRecsInDB);
            CloseSingleDB(db);
         end;
         Findings.Add(aline);
         CloseSingleDEM(DEM);
      end;
      fName := MDtempDir + 'estimated_area_tiles_by_elevation.dbf';
      StringList2CSVtoDB(Findings,fName);
      AreasWanted.Destroy;
   finally;
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('End FindTilesInAreaForCoast'); {$EndIf}
end;


procedure AddCountryColorToDB(DBonTable : integer);
var
   OneTile : shortstring;
   theFields : tStringList;
   Table : tMyData;
   i : integer;
   fName : PathStr;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddCountryColorToDB'); {$EndIf}
   fName := DEMIXSettingsDir + 'country_colors.dbf';
   GISdb[DBonTable].ClearLinkTable(true);
   theFields := tStringList.Create;
   theFields.Add('COLOR');
   GISdb[DBonTable].dbOpts.LinkTableName := fname;
   GISdb[DBonTable].dbOpts.LinkFieldThisDB := 'COUNTRY';
   GISdb[DBonTable].dbOpts.LinkFieldOtherDB := 'COUNTRY';
   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   GISdb[DBonTable].LinkSecondaryTable(GISdb[DBonTable].dbOpts.LinkTableName);
   GISdb[DBonTable].FillFieldsFromJoinedTable(TheFields,true);
   GISDb[DBonTable].ShowStatus;
   GISdb[DBonTable].ClearLinkTable(true);
end;


procedure AddTileCharacteristicsToDB(DBonTable : integer);
var
   OneTile : shortstring;
   theFields : tStringList;
   i : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddTileCharacteristics'); {$EndIf}

   GetDEMIXpaths(true);
   GISdb[DBonTable].ClearLinkTable(true);
   theFields := tStringList.Create;
   for I := 1 to NumTileCharacters do theFields.Add(TileCharacters[i]);
   theFields.Add('COUNTRY');

   OneTile := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');

   if not FileExists(MDdef.DEMIX_tile_chars_fname) then begin
      MDdef.DEMIX_tile_chars_fname := DEMIX_final_DB_dir;
      if not GetExistingFileName('DEMIX tile characteristics','*.dbf',MDdef.DEMIX_tile_chars_fname) then exit;
   end;

   GISdb[DBonTable].dbOpts.LinkTableName := MDdef.DEMIX_tile_chars_fname;

   GISdb[DBonTable].dbOpts.LinkFieldThisDB := 'DEMIX_TILE';
   GISdb[DBonTable].dbOpts.LinkFieldOtherDB := 'DEMIX_TILE';
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddFieldsToDEMIXDB, fields=' + IntToStr(TheFields.Count) + ' link=' + GISdb[DBonTable].dbOpts.LinkTableName); {$EndIf}

   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddFieldsToDEMIXDB point 2'); {$EndIf}
   GISdb[DBonTable].LinkSecondaryTable(GISdb[DBonTable].dbOpts.LinkTableName);

   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddFieldsToDEMIXDB point 4'); {$EndIf}
   GISdb[DBonTable].FillFieldsFromJoinedTable(TheFields,true);
   GISDb[DBonTable].ShowStatus;
   GISdb[DBonTable].ClearLinkTable(true);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddFieldsToDEMIXDB done'); {$EndIf}
   EndDEMIXProcessing;
end;


procedure AddPercentPrimaryData(DBonTable : integer);
var
   Cop,ALOS : integer;
   Lat,Long : float64;
   z : float32;
   Tile : shortstring;
   fName : PathStr;
   bb : sfBoundBox;
begin
   try
      ShowHourglassCursor;
      SetColorForProcessing;
      fName := 'J:\visioterra_primary_data_fraction\COP-DEM_GLO-30-DGED_v2019_1_A03_Primary_data_fraction.tif';
      Cop := OpenNewDEM(fName,false,'COP-PDF');
      fName := 'J:\visioterra_primary_data_fraction\AW3D30_v2003_A03_Primary_data_fraction.tif';
      ALOS := OpenNewDEM(fName,false,'ALOS-PDF');
      GISdb[dbOnTable].AddFieldToDataBase(ftFloat,'COP_PDF',6,2);
      GISdb[dbOnTable].AddFieldToDataBase(ftFloat,'ALOS_PDF',6,2);
      GISdb[dbOnTable].EmpSource.Enabled := false;
      GISdb[dbOnTable].MyData.First;
      ShowHourglassCursor;
      while not GISdb[dbOnTable].MyData.eof do begin
         GISdb[dbOnTable].MyData.Edit;
         Tile := GISdb[dbOnTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
         bb := DEMIXtileBoundingBox(Tile);
         if DEMglb[Cop].GetElevFromLatLongDegree(bb.ymax,bb.xmin,z) then GISdb[dbOnTable].MyData.SetFieldByNameAsFloat('COP_PDF',z);
         if DEMglb[ALOS].GetElevFromLatLongDegree(bb.ymax,bb.xmin,z) then GISdb[dbOnTable].MyData.SetFieldByNameAsFloat('ALOS_PDF',z);
         GISdb[dbOnTable].MyData.Next;
      end;
      GISdb[dbOnTable].EmpSource.Enabled := true;
      CloseSingleDEM(Cop);
      CloseSingleDEM(ALOS);
   finally
      ShowDefaultCursor;
      SetColorForWaiting;
   end;
end;


procedure EvalRangeAndStatsByCriterion(DBonTable : integer; aField : shortstring = '');
var
   Criteria : tstringlist;
   i,db : integer;
   fName : PathStr;
   MomentVar : tMomentVar;
   Findings : tStringList;
   Tol : float32;
   BaseFilter : shortstring;
begin
   try
      if (aField = '') then begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         aField := GISdb[DBonTable].PickField('Field statistics by criterion',NumericFieldTypes);
      end;
      SetColorForProcessing;
      GISdb[DBonTable].EmpSource.Enabled := false;
      BaseFilter := GISdb[DBonTable].MyData.Filter;
      Criteria := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('CRITERION');
      Findings := tStringList.Create;
      Findings.Add('CRITERION,MIN,PC5,PC25,MEDIAN,MEAN,PC75,PC95,MAX,N,TOLERANCE');
      for i := 0 to pred(Criteria.Count) do begin
          GISdb[DBonTable].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'CRITERION=' + QuotedStr(Criteria.Strings[i]));
          GISdb[DBonTable].EmpSource.Enabled := false;
          MomentVar := GISdb[DBonTable].GetFieldStatistics(aField);
          //Tol := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('TOLERANCE');
          Tol := CriterionTieTolerance('TOLERANCE');
          Findings.Add(Criteria.Strings[i] + ',' + RealToString(MomentVar.MinZ,-12,-6)  + ',' +
                                                   RealToString(MomentVar.PC5,-12,-6)  + ',' +
                                                   RealToString(MomentVar.Q1,-12,-6)  + ',' +
                                                   RealToString(MomentVar.Median,-12,-6)  + ',' +
                                                   RealToString(MomentVar.Mean,-12,-6)  + ',' +
                                                   RealToString(MomentVar.Q3,-12,-6)  + ',' +
                                                   RealToString(MomentVar.PC95,-12,-6)  + ',' +
                                                   RealToString(MomentVar.MaxZ,-12,-6)  + ',' +
                                                   IntToStr(MomentVar.NPts)  + ',' +
                                                   RealToString(Tol,-12,-6) );
      end;
      fName := NextFileNumber(MDTempDir,'Evaluation_range_' + GISdb[DBonTable].dbName + '_','.dbf');
      db := StringList2CSVtoDB(Findings,fName);
      GISdb[DBonTable].ApplyGISFilter(BaseFilter);
   finally
      ShowDefaultCursor;
      SetColorForWaiting;
   end;
end;


procedure EvalRangeAndBestEvalForCriterion(DBonTable : integer);
//adds field with the range between the best and worst evaluations
var
   i,j : integer;
   Eval,MaxEval,MinEval : float32;
   Criterion,DEM : shortstring;
begin
   if GISdb[DBonTable].MyData.FieldExists('EVAL_RANGE') and GISdb[DBonTable].MyData.FieldExists('BEST_EVAL') then begin
      if not AnswerIsYes('Redo') then exit;
   end;
   SetColorForProcessing;
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,'EVAL_RANGE',12,6);
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,'BEST_EVAL',12,6);
   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   j := 0;
   while not GISDb[DBonTable].MyData.eof do begin
      if (j mod 50 = 0) then wmdem.SetPanelText(3,'Evaluation ranges ' + IntToStr(j) + '/' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB));
      inc(j);
      Criterion := GISdb[dbOnTable].MyData.GetFieldByNameAsString('CRITERION');
      if not IsDEMIX_signedCriterion(Criterion) then begin
      //if (GISDb[DBonTable].MyData.GetFieldByNameAsString('EVAL_RANGE') = '') or (GISDb[DBonTable].MyData.GetFieldByNameAsString('BEST_EVAL') = '') then begin
         MaxEval := -99e39;
         MinEval := 99e39;
         for I := 1 to MaxDEMIXDEM do begin
            DEM := DEMIXshort[i];
            if GISdb[DBonTable].MyData.FieldExists(DEM) then begin
               Eval := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEM);
               if (Eval > 0.000001) then PetMath.CompareValueToExtremes(Eval,MinEval,MaxEval);
            end;
         end;
         GISDb[DBonTable].MyData.Edit;
         GISDb[DBonTable].MyData.SetFieldByNameAsFloat('BEST_EVAL',MinEval);
         GISDb[DBonTable].MyData.SetFieldByNameAsFloat('EVAL_RANGE',MaxEval-MinEval);
      end;
      GISDb[DBonTable].MyData.Next;
   end;
   GISDb[DBonTable].ShowStatus;
   wmdem.SetPanelText(3,'');
   SetColorForWaiting;
end;




