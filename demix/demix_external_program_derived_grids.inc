{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{ include file for demix_definitions }
{____________________________________}



procedure ClearDerivedGrids;
var
   i : integer;
begin
    if (not MDDef.OpenMapsFUVSSIM) then begin
       for i := -1 to NumPtDEMs do CloseSingleDEM(PointGrids[i]);
       for i := -1 to NumAreaDEMs do CloseSingleDEM(AreaGrids[i]);
       for i := dmxFirstPoint to NumPtDEMs do CloseSingleDEM(PointGrids2[i]);
       for i := dmxFirstArea to NumAreaDEMs do CloseSingleDEM(AreaGrids2[i]);
    end;
    wmdem.SetPanelText(2, '',true);
end;


function ExternalProgramOutPutFile(i : integer; aProgram,Param,AreaName : shortstring; IsPoint : boolean) : PathStr;
//aProgram should not have a trailing "_", but Param should have a trailing "_"
var
  RefAreaDir,TestAreaDir : PathStr;
begin
   //FindPath(Aprogram + ' files for reference DEMs',':\wine_contest\' + aProgram + '_out_ref_areas\',RefAreaDir);
   //FindPath(Aprogram + ' files for test DEMs',':\wine_contest\' + aProgram + '_out_test_areas\',TestAreaDir);
   RefAreaDir := DEMIX_Base_DB_Path + aProgram + '_out_ref_areas\';
   TestAreaDir := DEMIX_Base_DB_Path + aProgram + '_out_test_areas\';
   if isPoint then begin
       if i = -1 then Result := RefAreaDir + Param + AreaName + '_dtm_ref_1.5x1sec_point.tif'
       else if i = 0 then Result := RefAreaDir + Param + AreaName + '_dtm_ref_1sec_point.tif'
       else begin
          if StrUtils.AnsiContainsText(AreaName,SRTM_centroid_names[i]) then Result := TestAreaDir + Param + AreaName + '.tif'
          else Result := TestAreaDir + Param + AreaName + '_' + SRTM_centroid_names[i] + '.tif';
       end;
   end
   else begin
       if i = 0 then Result := RefAreaDir + Param + AreaName + '_dtm_ref_1sec_area.tif'
       else  begin
          //Result := TestAreaDir + Param + AreaName + '_' + ALOS_centroid_names[i] + '.tif';
          if StrUtils.AnsiContainsText(AreaName,ALOS_centroid_names[i]) then Result := TestAreaDir + Param + AreaName + '.tif'
          else Result := TestAreaDir + Param + AreaName + '_' + ALOS_centroid_names[i] + '.tif';
       end;
   end;
end;


function RefGridForThisPointGrid(WhatGroup : tDEM_int_array; i : integer) : integer;
var
   SpaceSec : float64;
begin
    if ValidDEM(WhatGroup[i]) then begin
       SpaceSec := 3600 * DEMglb[WhatGroup[i]].DEMHeader.DEMxSpacing;
       {$IfDef RecordDEMIXRefDEM} WriteLineToDebugFile(DEMGlb[WhatGroup[i]].AreaName + ' ' + RealToString(SpaceSec,-8,-2) + 'sec'); {$EndIf}
       if (SpaceSec > 1.05) then Result := WhatGroup[-1]
       else Result := WhatGroup[0];
       {$IfDef RecordDEMIXRefDEM} WriteLineToDebugFile('Ref DEM for ' + DEMGlb[WhatGroup[i]].AreaName + ' is ' + DEMGlb[Result].AreaName); {$EndIf}
    end
    else begin
       {$IfDef RecordDEMIX} WriteLineToDebugFile('Invaild DEM=' + IntToStr(i) + ' in RefGridForThisPointGrid'); {$EndIf}
    end;
end;



procedure ZeroPointAndAreaGrids(var PointGrids,AreaGrids : tDEM_int_array; InitValue : integer = 0);
var
   i : integer;
begin
   for i := -1 to MaxPossGrids do PointGrids[i] := InitValue;
   for i := -1 to MaxPossGrids do AreaGrids[i] := InitValue;
end;


procedure ShowDEMIXgrids(WhatFor : shortstring; PointGrids,AreaGrids : tDEM_int_array);
var
   i : integer;
begin
    HighlightLineToDebugFile(WhatFor);
    for i := -1 to NumPtDEMs do
       if ValidDEM(PointGrids[i]) then
          WriteLineToDebugFile(IntegerToString(PointGrids[i],3) + '  ' + DEMglb[PointGrids[i]].AreaName + '  ' + DEMglb[PointGrids[i]].GridCornerModelAndPixelIsString);
    for i := -1 to NumAreaDEMs do
       if ValidDEM(AreaGrids[i]) then
          WriteLineToDebugFile(IntegerToString(AreaGrids[i],3) + '  ' + DEMglb[AreaGrids[i]].AreaName + '  ' + DEMglb[AreaGrids[i]].GridCornerModelAndPixelIsString);
end;


function OpenGridsCreatedByExternalProgram(OpenMaps : boolean; aProgram,AreaName,Param : shortString; var PointGrids,AreaGrids : tDEM_int_array) : boolean;
var
   i : integer;
   fName2 : PathStr;
begin
    Result := false;
    ZeroPointAndAreaGrids(PointGrids,AreaGrids);
    {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
    for i := dmxFirstPoint to NumPtDEMs do begin
       fName2 := ExternalProgramOutPutFile(i,aProgram,Param,AreaName,true);
       if FileExists(fName2) then LoadNewDEM(PointGrids[i],fName2,OpenMaps)
       else begin
          {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('External program open, Missing file: ' + fName2); {$EndIf}
          exit;
       end;
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       fName2 := ExternalProgramOutPutFile(i,aProgram,Param,AreaName,false);
       if FileExists(fName2) then LoadNewDEM(AreaGrids[i],fName2,OpenMaps)
       else begin
          {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('External program open, Missing file: ' + fName2); {$EndIf}
          exit;
       end;
    end;
    Result := true;
    {$If Defined(RecordFUVsteps)} WriteLineToDebugFile(Param + ' files opened   ' + RealToString(StopWatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
    {$If Defined(RecordOpenExternalProgramGrids)} ShowDEMIXgrids(Param,PointGrids,AreaGrids); {$EndIf}
end;


procedure CreateDEMIXhillshadeGrids(AreaName : shortstring; var HowDone : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
var
   i : integer;
   fName : PathStr;
begin
    HowDone := ' opened ';
    ZeroPointAndAreaGrids(PointGrids,AreaGrids);
    for i := dmxFirstPoint to NumPtDEMs do begin
       fName := ExternalProgramOutPutFile(i,'MD','HILL_',AreaName,true);
       if FileExists(fName) then begin
          LoadNewDEM(PointGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          PointGrids[i] := CreateHillshadeMap(OpenMaps,PointDEMs[i]);
          HowDone := ' created ';
          if SaveMaps then DEMglb[PointGrids[i]].SaveAsGeotiff(fName);
       end;
       {$If Defined(RecordDEMIXFull) or Defined(RecordDEMIXhillshades)}
          WriteLineToDebugFile('Grid=' + IntToStr(PointGrids[i]) + ' Hillshade for ' + DEMGlb[PointDEMs[i]].AreaName + ' ' + DEMGlb[PointGrids[i]].KeyDEMParams(true));
       {$EndIf}
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       fName := ExternalProgramOutPutFile(i,'MD','HILL_',AreaName,false);
       if FileExists(fName) then begin
          LoadNewDEM(AreaGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          AreaGrids[i] := CreateHillshadeMap(OpenMaps,AreaDEMs[i]);
          HowDone := 'created';
          if SaveMaps then DEMglb[AreaGrids[i]].SaveAsGeotiff(fName);
       end;
       {$If Defined(RecordDEMIXFull) or Defined(RecordDEMIXhillshades)}
          WriteLineToDebugFile('Grid=' + IntToStr(AreaGrids[i]) + ' Hillshade for ' + DEMGlb[PointDEMs[i]].AreaName + ' ' + DEMGlb[AreaGrids[i]].KeyDEMParams(true));
       {$EndIf}
    end;
end;


procedure CreateDEMIXSlopeRoughnessGrids(AreaName : shortstring; var HowDone : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
var
   i : integer;
   fName,fName2 : PathStr;
begin
    ZeroPointAndAreaGrids(PointGrids2,AreaGrids2, 0);
    ZeroPointAndAreaGrids(PointGrids,AreaGrids, 0);
    for i := dmxFirstPoint to NumPtDEMs do begin
       fName2 := ExternalProgramOutPutFile(i,'MD','SLOPE_',AreaName,true);
       fName := ExternalProgramOutPutFile(i,'MD','ROUGH_',AreaName,true);
       if FileExists(fName) and FileExists(fName2) then begin
          LoadNewDEM(PointGrids2[i],fName2,MDDef.OpenMapsFUVSSIM);
          LoadNewDEM(PointGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          PointGrids[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(PointDEMs[i],5,PointGrids2[i],OpenMaps);
          if SaveMaps then begin
             DEMglb[PointGrids2[i]].SaveAsGeotiff(fName2);
             DEMglb[PointGrids[i]].SaveAsGeotiff(fName);
          end;
       end;
    end;

    for i := dmxFirstArea to NumAreaDEMs do begin
       fName2 := ExternalProgramOutPutFile(i,'MD','SLOPE_',AreaName,false);
       fName := ExternalProgramOutPutFile(i,'MD','ROUGH_',AreaName,false);
       if FileExists(fName) and FileExists(fName2) then begin
          LoadNewDEM(AreaGrids2[i],fName2,MDDef.OpenMapsFUVSSIM);
          LoadNewDEM(AreaGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          AreaGrids[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(AreaDEMs[i],5,AreaGrids2[i],OpenMaps);
          if SaveMaps then begin
             DEMglb[AreaGrids2[i]].SaveAsGeotiff(fName2);
             DEMglb[AreaGrids[i]].SaveAsGeotiff(fName);
          end;
       end;
    end;
end;


procedure CreateDEMIXRRIgrids(AreaName : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
var
   i : integer;
   fName : PathStr;
begin
    ZeroPointAndAreaGrids(PointGrids,AreaGrids);
    for i := dmxFirstPoint to NumPtDEMs do begin
       fName := ExternalProgramOutPutFile(i,'MD','RRI_',AreaName,true);
       if FileExists(fName) then begin
          LoadNewDEM(PointGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          PointGrids[i] := MakeTRIGrid(PointDEMs[i],nmRRI,MDDef.OpenMapsFUVSSIM);
          if SaveMaps then DEMglb[PointGrids[i]].SaveAsGeotiff(fName);
       end;
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       fName := ExternalProgramOutPutFile(i,'MD','RRI_',AreaName,false);
       if FileExists(fName) then begin
          LoadNewDEM(AreaGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          AreaGrids[i] := MakeTRIGrid(AreaDEMs[i],nmRRI,MDDef.OpenMapsFUVSSIM);
          if SaveMaps then DEMglb[AreaGrids[i]].SaveAsGeotiff(fName);
       end;
    end;
end;


procedure CreateDEMIXTPIGrids(AreaName : shortstring; var HowDone : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
var
   i : integer;
   fName : PathStr;
begin
    HowDone := ' opened ';
    ZeroPointAndAreaGrids(PointGrids,AreaGrids);
    for i := dmxFirstPoint to NumPtDEMs do begin
       fName := ExternalProgramOutPutFile(i,'MD','TPI_',AreaName,true);
       if FileExists(fName) then begin
          LoadNewDEM(PointGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          PointGrids[i] := BoxCarDetrendDEM(MDDef.OpenMapsFUVSSIM,PointDEMs[i],DEMGlb[PointDEMs[i]].FullDEMGridLimits,3);
          HowDone := ' created ';
          if SaveMaps then begin
             DEMglb[PointGrids[i]].SaveAsGeotiff(fName);
          end;
       end;
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       fName := ExternalProgramOutPutFile(i,'MD','TPI_',AreaName,false);
       if FileExists(fName) then begin
          LoadNewDEM(AreaGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          AreaGrids[i] := BoxCarDetrendDEM(MDDef.OpenMapsFUVSSIM,AreaDEMs[i],DEMGlb[AreaDEMs[i]].FullDEMGridLimits,3);
          HowDone := ' created ';
          if SaveMaps then begin
             DEMglb[AreaGrids[i]].SaveAsGeotiff(fName);
          end;
       end;
    end;
end;


procedure CreateDEMIXOpennessGrids(AreaName : shortstring; var HowDone : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
var
   i,ad : integer;
   fName,fName2 : PathStr;
begin
    HowDone := ' opened ';
    ZeroPointAndAreaGrids(PointGrids,AreaGrids,-1);
    ZeroPointAndAreaGrids(PointGrids,AreaGrids,-1);
    ad := 0;
    for i := dmxFirstPoint to NumPtDEMs do begin
       fName2 := ExternalProgramOutPutFile(i,'MD','OPEND_',AreaName,true);
       fName := ExternalProgramOutPutFile(i,'MD','OPENU_',AreaName,true);
       if FileExists(fName) and FileExists(fName2) then begin
          LoadNewDEM(PointGrids2[i],fName2,MDDef.OpenMapsFUVSSIM);
          LoadNewDEM(PointGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          PointGrids[i] := -1;
          PointGrids2[i] := -1;
          CreateOpennessMap(OpenMaps,PointDEMs[i],250,PointGrids[i],PointGrids2[i],ad);
          HowDone := ' created ';
          if SaveMaps then begin
             DEMglb[PointGrids2[i]].SaveAsGeotiff(fName2);
             DEMglb[PointGrids[i]].SaveAsGeotiff(fName);
          end;
       end;
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       fName2 := ExternalProgramOutPutFile(i,'MD','OPEND_',AreaName,false);
       fName := ExternalProgramOutPutFile(i,'MD','OPENU_',AreaName,false);
       if FileExists(fName) and FileExists(fName2) then begin
          LoadNewDEM(AreaGrids2[i],fName2,MDDef.OpenMapsFUVSSIM);
          LoadNewDEM(AreaGrids[i],fName,MDDef.OpenMapsFUVSSIM);
       end
       else begin
          AreaGrids[i] := -1;
          AreaGrids2[i] := -1;
          CreateOpennessMap(OpenMaps,AreaDEMs[i],250,AreaGrids[i],AreaGrids2[i],ad);
          HowDone := ' created ';
          if SaveMaps then begin
             DEMglb[AreaGrids2[i]].SaveAsGeotiff(fName2);
             DEMglb[AreaGrids[i]].SaveAsGeotiff(fName);
          end;
       end;
    end;
end;


function SAGACreateDEMIX_ConIn_Grids(OpenMaps : boolean; AreaName,aParam : shortstring) : boolean;
//these are automatically saved
var
   i : integer;
   fName : PathStr;
begin
    {$IfDef RecordDEMIX_CONIN} WriteLineToDebugFile('SAGACreateDEMIX_ConIn_Grids in'); {$EndIf}
    for i := dmxFirstPoint to NumPtDEMs do begin
       fName := ExternalProgramOutPutFile(i,'SAGA',aParam,AreaName,true);
       {$IfDef RecordDEMIX_CONIN} WriteLineToDebugFile(fname + '  start'); {$EndIf}
       if FileExists(fName) then LoadNewDEM(PointGrids[i],fName,MDDef.OpenMapsFUVSSIM)
       else begin
          wmdem.SetPanelText(2,DEMglb[PointDEMs[i]].AreaName,true);
          PointGrids[i] := SAGA_ConvergenceIndex(MDDef.OpenMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName,fName);
       end;
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       fName := ExternalProgramOutPutFile(i,'SAGA',aParam,AreaName,false);
       {$IfDef RecordDEMIX_CONIN} WriteLineToDebugFile(fname + '  start'); {$EndIf}
       if FileExists(fName) then LoadNewDEM(AreaGrids[i],fName,MDDef.OpenMapsFUVSSIM)
       else begin
          wmdem.SetPanelText(2,DEMglb[AreaDEMs[i]].AreaName,true);
          AreaGrids[i] := SAGA_ConvergenceIndex(MDDef.OpenMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName,fName);
       end;
    end;
    Result := true;
    {$IfDef RecordDEMIX_CONIN} WriteLineToDebugFile('SAGACreateDEMIX_ConIn_Grids out'); {$EndIf}
end;


function SAGACreateDEMIX_LS_Grids(AreaName,aParam : shortstring; OpenMaps : boolean = false) : boolean;
var
   i : integer;
begin
    for i := dmxFirstPoint to NumPtDEMs do begin
       PointGrids[i] := SAGA_LSFactor(MDDef.OpenMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);   //,ExternalProgramOutPutFile(i,'SAGA',aParam,AreaName,true));
       if ValidDEM(PointGrids[i]) then begin
          {$IfDef RecordDEMIXLSgrids} WriteLineToDebugFile(DEMGlb[PointDEMs[i]].AreaName); {$EndIf}
       end
       else begin
          Result := false;
          ClearDerivedGrids;
          exit;
       end;
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       AreaGrids[i] := SAGA_LSFactor(MDDef.OpenMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);  //,ExternalProgramOutPutFile(i,'SAGA',aParam,AreaName,false));
       if ValidDEM(AreaGrids[i]) then begin
          {$IfDef RecordDEMIXLSgrids} WriteLineToDebugFile(DEMGlb[PointDEMs[i]].AreaName); {$EndIf}
       end
       else begin
          Result := false;
          ClearDerivedGrids;
          exit;
       end;
    end;
    Result := true;
end;


procedure WBT_CreateDEMIX_HANDGrids(OpenMaps : boolean = false);
var
   i : integer;
   BreachName,FlowAccumulationName,StreamName,HANDName  : PathStr;
begin
   for i := dmxFirstPoint to NumPtDEMs do begin
      BreachName := '';
      FlowAccumulationName  := '';
      StreamName := '';
      HANDName := '';
      PointGrids[i] := WBT_ElevAboveStream(OpenMaps,DEMGlb[PointDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName,StreamName,HANDName);
   end;
   for i := dmxFirstArea to NumAreaDEMs do begin
      BreachName := '';
      FlowAccumulationName  := '';
      StreamName := '';
      HANDName := '';
      AreaGrids[i] := WBT_ElevAboveStream(OpenMaps,DEMGlb[AreaDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName,StreamName,HandName);
   end;
end;


procedure WBT_CreateDEMIX_Flow_AccumulationGrids(Log : boolean; OpenMaps : boolean = false);
var
   i : integer;
   BreachName,FlowAccumulationName,StreamName,HANDName  : PathStr;
begin
    //using Log, and using D8
    for i := dmxFirstPoint to NumPtDEMs do begin
      BreachName := '';
      FlowAccumulationName  := '';
      StreamName := '';
      HANDName := '';
      PointGrids[i] := WBT_FlowAccumulation(OpenMaps,Log,True,DEMGlb[PointDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName);
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       BreachName := '';
       FlowAccumulationName  := '';
       StreamName := '';
       HANDName := '';
       AreaGrids[i] := WBT_FlowAccumulation(OpenMaps,Log,True,DEMGlb[AreaDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName);
    end;
    {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Flow accumulation grids created'); {$EndIf}
end;


procedure WBT_CreateDEMIX_GeomorphonGrids(OpenMaps : boolean = false);
var
   i : integer;
begin
   for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_Geomorphons(OpenMaps,DEMGlb[PointDEMs[i]].GeotiffDEMName);
   for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_Geomorphons(OpenMaps,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
end;



