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
begin
   if isPoint then begin
       if i = -1 then Result := 'J:\wine_contest\' + aProgram + '_out_ref_areas\' + Param + AreaName + '_dtm_ref_1.5x1sec_point.tif'
       else if i = 0 then Result := 'J:\wine_contest\' + aProgram + '_out_ref_areas\' + Param + AreaName + '_dtm_ref_1sec_point.tif'
       else Result := 'J:\wine_contest\' + aProgram + '_out_test_areas\' + Param + AreaName + '_' + DEMIXshort[i] + '.tif';
   end
   else begin
       if i = 0 then Result := 'J:\wine_contest\' + aProgram + '_out_ref_areas\' + Param + AreaName + '_dtm_ref_1sec_area.tif'
       else Result := 'J:\wine_contest\' + aProgram + '_out_test_areas\' + Param + AreaName + '_' + DEMIXshort[i] + '.tif';
   end;
end;


function OpenGridsCreatedByExternalProgram(aProgram,AreaName,Param : shortString) : boolean;
var
   i : integer;
   fName2 : PathStr;
begin
    {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
    for i := dmxFirstPoint to NumPtDEMs do begin
       fName2 := ExternalProgramOutPutFile(i,aProgram,Param,AreaName,true);
       if FileExists(fName2) then LoadNewDEM(PointGrids[i],fName2,MDDef.OpenMapsFUVSSIM)
       else begin
          Result := false;
          exit;
       end;
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       fName2 := ExternalProgramOutPutFile(i,aProgram,Param,AreaName,false);
       if FileExists(fName2) then LoadNewDEM(AreaGrids[i],fName2,MDDef.OpenMapsFUVSSIM)
       else begin
          Result := false;
          exit;
       end;
    end;
    Result := true;
    {$If Defined(RecordFUVsteps)} WriteLineToDebugFile(Param + ' files opened   ' + RealToString(StopWatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
end;


function SAGACreateDEMIX_ConIn_Grids(OpenMaps : boolean; AreaName,aParam : shortstring) : boolean;
var
   i : integer;
begin
    for i := dmxFirstPoint to NumPtDEMs do begin
       PointGrids[i] := SAGA_ConvergenceIndex(MDDef.OpenMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName,ExternalProgramOutPutFile(i,'SAGA',aParam,AreaName,true));
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       AreaGrids[i] := SAGA_ConvergenceIndex(MDDef.OpenMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName,ExternalProgramOutPutFile(i,'SAGA',aParam,AreaName,false));
    end;
    Result := true;
end;



function CreateDEMIX_LS_Grids(AreaName,aParam : shortstring; OpenMaps : boolean = false) : boolean;
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


procedure CreateDEMIX_HANDGrids(OpenMaps : boolean = false);
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


procedure CreateDEMIX_Flow_AccumulationGrids(OpenMaps : boolean = false);
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
      PointGrids[i] := WBT_FlowAccumulation(OpenMaps,True,True,DEMGlb[PointDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName);
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       BreachName := '';
       FlowAccumulationName  := '';
       StreamName := '';
       HANDName := '';
       AreaGrids[i] := WBT_FlowAccumulation(OpenMaps,True,True,DEMGlb[AreaDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName);
    end;
    {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Flow accumulation grids created'); {$EndIf}
end;


procedure CreateDEMIX_GeomorphonGrids(OpenMaps : boolean = false);
var
   i : integer;
begin
   for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_Geomorphons(OpenMaps,DEMGlb[PointDEMs[i]].GeotiffDEMName);
   for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_Geomorphons(OpenMaps,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
end;



