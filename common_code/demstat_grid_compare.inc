{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{ include file for demstat          }
{___________________________________}

type
   tIntVector256 = array[0..255] of integer;
   tIntMatrix256 = array[0..255,0..255] of integer;


procedure AddSubtractGrid(Map1,Multiple : integer);
var
   Map2,Col,Row  : integer;
   z1,z2 : float32;
   //ThisGraph : TThisBaseGraph;
   IdenticalGrids,OK : boolean;
   rFile : file;
begin
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('AddSubtractGrid in'); {$EndIf}
   IdenticalGrids := DEMGlb[Map1].SecondGridIdentical(Map2);
   StartProgress('Grid Differences');
   for Col := 0 to pred(DEMGlb[Map1].DEMheader.NumCol) do begin
      for Row := 0 to pred(DEMGlb[Map1].DEMheader.NumRow) do begin
         if ((GridMaskDEM = 0) or MaskValidPoint(Col,Row)) then begin
            if DEMGlb[Map1].GetElevMeters(Col,Row,z1) and DEMGlb[Map1].GetElevMetersFromSecondDEM(IdenticalGrids,Map2,Col,Row,z2) then begin
               DEMGlb[Map1].SetGridElevation(Col,Row,z2 - z1);
            end
            else begin
               DEMGlb[Map1].SetGridMissing(Col,Row);
            end;
         end;
      end {for Row};
      if (Col mod 25 = 0) then UpdateProgressBar(Col / pred(DEMGlb[Map1].DEMheader.NumCol));
   end {for Col};
   EndProgress;
   DEMGlb[Map1].SelectionMap.RespondToChangedDEM;
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('AddSubtractGrid out'); {$EndIf}
end;


procedure ZeroCoOccurrence(var Dist1,Dist2 : tIntVector256; var CoOccur  : tIntMatrix256);
var
   Col,Row : integer;
begin
   for col := 0 to 255 do begin
      Dist1[col] := 0;
      Dist2[col] := 0;
      for Row := 0 to 255 do begin
         CoOccur[col,row] := 0;
      end;
   end;
end;


procedure FillInCoOccurrence(HName,VName : ShortString; Percentages : boolean; Total : int64; var Dist1,Dist2 : tIntVector256; var CoOccur  : tIntMatrix256);
var
   Col,Row,OnCol,OnRow : integer;
   VarGrid : TGridForm;

         function ShowValue(Number : integer) : shortString;
         begin
            if Percentages then Result := RealToString(100.0 * Number / Total,-12,4) + '%'
            else Result := IntToStr(Number);
         end;

begin
   VarGrid := TGridForm.Create(Application);
   VarGrid.Caption := 'Co-occurrence ' + hname + ' (h) by ' + vName + ' (v)';
   VarGrid.HideCorrelationControls;
   VarGrid.SetFormSize;

   OnCol := 1;
   for Col := 0 to 255 do begin
      if Dist1[Col] > 0 then begin
          OnRow := 1;
          VarGrid.StringGrid1.Cells[OnCol,0] := IntToStr(Col);
          for Row := 0 to 255 do  begin
             if Dist2[Row] > 0 then begin
                VarGrid.StringGrid1.Cells[OnCol,OnRow] := ShowValue(CoOccur[Col,Row]);
                VarGrid.StringGrid1.Cells[0,OnRow] := IntToStr(Row);
                inc(OnRow);
             end;
          end;
          VarGrid.StringGrid1.Cells[OnCol,OnRow] := ShowValue(Dist1[Col]);
          inc(OnCol);
      end;
   end;
   VarGrid.StringGrid1.ColCount := succ(OnCol);
   VarGrid.StringGrid1.RowCount := succ(OnRow);
   VarGrid.StringGrid1.Cells[0,OnRow] := 'Total';
   VarGrid.StringGrid1.Cells[OnCol,0] := 'Total';
   VarGrid.StringGrid1.Cells[OnCol,OnRow] := ShowValue(Total);
    OnRow := 1;
    for Row := 0 to 255 do begin
       if (Dist2[Row] > 0) then  begin
          VarGrid.StringGrid1.Cells[OnCol,OnRow] := ShowValue(Dist2[Row]);
          inc(OnRow);
       end;
    end;
end;


procedure DBCoOccurrence(Table : tMyData; EmpSource: TDataSource; Field1,Field2 : ShortString; Percentages : boolean);
var
   Dist1,Dist2 : tIntVector256;
   z1,z2,k,Total : integer;
   CoOccur  : tIntMatrix256;
begin
   {$IfDef RecordClustering} WriteLineToDebugFile('DBCoOccurrence in'); {$EndIf}
   Total := 0;
   ZeroCoOccurrence(Dist1,Dist2,CoOccur);
   Table.First;
   k := 0;
   StartProgress('co-occurrence');
   while not Table.eof do begin
      if (k mod 1000 = 0) then begin
         UpdateProgressBar(k/Table.RecordCount);
         EmpSource.Enabled := false;
      end;
      inc(k);
      z1 := round(Table.GetFieldByNameAsFloat(Field1));
      z2 := round(Table.GetFieldByNameAsFloat(Field2));
      inc(Dist1[z1]);
      inc(Dist2[z2]);
      inc(CoOccur[z1,z2]);
      inc(Total);
      Table.Next;
   end;
   EndProgress;
   FillInCoOccurrence(Field1,Field2,Percentages,Total,Dist1,Dist2,CoOccur);
   {$IfDef RecordClustering} WriteLineToDebugFile('DBCoOccurrence in'); {$EndIf}
end;


procedure MissingPointsInGrids(DEM1 : integer = 0; DEM2 : integer = 0);
var
   Incr,Col,Row : integer;
   Total : int64;
   Lat,Long,XGrid,YGrid : float64;
   z1,z2 : float32;
   IdenticalGrids : boolean;
   GridLimits : tGridLimits;
   //Dist1,Dist2 : tIntVector256;
   //CoOccur  : tIntMatrix256;
   sl : tStringList;
   Name1,Name2 : shortstring;
begin
   if (DEM1 = 0) and (DEM2 = 0) then GetTwoCompatibleGrids('DEM1, DEM2',false,DEM1,DEM2,false);
   IdenticalGrids := DEMGlb[DEM1].SecondGridIdentical(DEM2);

   if IdenticalGrids then begin
      Name1 := DEMGlb[DEM1].AreaName;

      while Length(Name1) > 10 do begin
         GetValidDBfieldname(Name1);
      end;
      Name2 := DEMGlb[DEM2].AreaName;
      while Length(Name2) > 10 do begin
         GetValidDBfieldname(Name2);
      end;


      sl := tStringList.Create;
      sl.add('LAT,LONG,' + Name1 + ',' + Name2);

      Col := GridLimits.XGridLow;
      for Col := 0 to pred(DEMGlb[DEM1].DEMHeader.NumCol) do begin
         for Row := 0 to pred(DEMGlb[DEM1].DEMHeader.NumRow) do begin
            DEMGlb[DEM1].DEMGridToLatLongDegree(Col,Row,Lat,Long);
            if DEMGlb[DEM1].GetElevMeters(Col,Row,z1) then begin
               if DEMGlb[DEM2].GetElevMeters(Col,Row,z2) then begin
               end
               else sl.add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' + RealToString(z1,-8,2) + ',  ');
            end
            else begin
               if DEMGlb[DEM2].GetElevMeters(Col,Row,z2) then begin
                  sl.add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',  ,' + RealToString(z2,-8,2));
               end;
            end;
         end;
      end;
      if sl.Count = 1 then begin
         sl.Destroy;
         MessageToContinue('No points missing in just one grid');
      end
      else DEMGlb[DEM1].SelectionMap.StringListToLoadedDatabase(sl, Petmar.NextFileNumber(MDTempDir, 'Missing_Data_Compare_', DefaultDBExt),true,false,true);
   end
   else begin
      MessageToContinue('Requires two idential grids');
   end;
end;


procedure GridCoOccurrence(AutoFull : boolean = false; DEM1 : integer = 0; DEM2 : integer = 0; Percentages : boolean = true);
var
   Incr,Col,Row : integer;
   Total : int64;
   Lat,Long : float64;
   z1,z2,XGrid,YGrid : float32;
   IdenticalGrids : boolean;
   GridLimits : tGridLimits;
   Dist1,Dist2 : tIntVector256;
   CoOccur  : tIntMatrix256;

         procedure Increment;
         begin
            inc(Dist1[round(z1)]);
            inc(Dist2[round(z2)]);
            inc(CoOccur[round(z1),round(z2)]);
            inc(Total);
         end;

begin
   if (DEM1 = 0) and (DEM2 = 0) then IdenticalGrids := GetTwoCompatibleGrids('DEM1=x axis, DEM2= y axis',false,DEM1,DEM2,false)
   else IdenticalGrids := DEMGlb[DEM1].SecondGridIdentical(DEM2);
   Total := 0;
   ZeroCoOccurrence(Dist1,Dist2,CoOccur);

   if (not AutoFull) and DEMGlb[DEM1].SelectionMap.FullMapSpeedButton.Enabled and AnswerIsYes('Current ' + DEMGlb[DEM1].AreaName + ' map subset') then begin
      GridLimits := DEMGlb[DEM1].SelectionMap.MapDraw.MapAreaDEMGridLimits;
   end
   else begin
      GridLimits := DEMGlb[DEM1].FullDEMGridLimits;
   end;

   Incr := 1;
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if MaskValidPoint(Col,Row) and DEMGlb[DEM1].GetElevMeters(Col,Row,z1) then begin
            if IdenticalGrids then begin
               if DEMGlb[DEM2].GetElevMeters(Col,Row,z2) then Increment;
            end
            else begin
               DEMGlb[DEM1].DEMGridToLatLongDegree(Col,Row,Lat,Long);
               DEMGlb[DEM2].LatLongDegreetoDEMGrid(Lat,Long,XGrid,YGrid);
               if DEMGlb[DEM2].GetElevMeters(round(XGrid),round(YGrid),z2) then increment;
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
   FillInCoOccurrence(DEMGlb[DEM1].AreaName,DEMGlb[DEM2].AreaName,Percentages,Total,Dist1,Dist2,CoOccur);
end;


procedure GridDiffernces(EntireDEM : boolean);
var
   DEM1,DEM2,DiffGrid,GridForResult : integer;
   RegionUsed : tGridLimits;
   aName : shortstring;
begin
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridDifference in'); {$EndIf}
   GetTwoCompatibleGrids('Result = DEM1 - DEM2)',true,DEM2,DEM1,false,true);
   if (DEM1 <> 0) and (DEM2 <> 0) then begin
      aName := 'Difference_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName;
      {$IfDef RecordDEMCompare} WriteLineToDebugFile(aName); {$EndIf}
      SetGridDiffernceProperties(DEM1,DEM2,GridForResult);
      if EntireDEM then RegionUsed := DEMGlb[DEM1].FullDEMGridLimits
      else RegionUsed := DEMGlb[DEM1].SelectionMap.MapDraw.MapAreaDEMGridLimits;
      DiffGrid := MakeDifferenceMapOfBoxRegion(DEM1,DEM2,DEM1,0,RegionUsed,MDDef.ShowGridDiffMap,MDDef.ShowGridDiffHistogram,MDDef.ShowScatterPlot,aName);
      {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridDifference out'); {$EndIf}
   end
   else begin
      {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridDifference fail, not 2 DEMs'); {$EndIf}
   end;
end;


function MakeDifferenceMap(Map1,Map2,GridResultionToUse,GridToMergeShading : integer; ShowMap,ShowHistogram,ShowScatterPlot : boolean; TheAreaName : ShortString = '') : integer;
begin
   Result := MakeDifferenceMapOfBoxRegion(Map1,Map2,GridResultionToUse,GridToMergeShading,DEMGlb[GridResultionToUse].FullDEMGridLimits,ShowMap,ShowHistogram,ShowScatterplot,TheAreaName);
   {$If Defined(TrackZRange)} WriteLineToDebugFile('MakeDifferenceMap ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].ZRange); {$EndIf}
end;


function MakeDifferenceMapOfBoxRegion(Map1,Map2,GridResultionToUse,GridToMergeShading : integer; GridLimits: tGridLimits; ShowMap,ShowHistogram,ShowScatterplot : boolean; TheAreaName : ShortString = '') : integer;
var
   Col,Row,mt,OtherGrid,ThisGrid,xoff,yoff  : integer;
   z1,z2 : float32;
   fName : PathStr;
   ThisGraph : TThisBaseGraph;
   IdenticalGrids : boolean;
   rFile : file;
begin
   if ValidDEM(Map1) and ValidDEM(Map2) then begin
      try
         {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap)}
            WriteLineToDebugFile('MakeDifferenceMapOfBoxRegion in, ' + DEMGlb[Map1].AreaName + ' ' + DEMGlb[Map1].zRange + ' minus ' + DEMGlb[Map2].AreaName + ' ' + DEMGlb[Map2].zRange);
         {$EndIf}
         HeavyDutyProcessing := true;
         Result := 0;
         OpenDEMDataStructures(Result);
         if (TheAreaName = '') then DEMGlb[Result].AreaName := 'Elev_Diff_' + DEMGlb[Map2].AreaName + '_minus_' + DEMGlb[Map1].AreaName
         else DEMGlb[Result].AreaName := TheAreaName;
         DEMGlb[Result].DEMheader := DEMGlb[GridResultionToUse].DEMheader;
         DEMGlb[Result].DEMheader.VerticalCSTypeGeoKey := 0;  //difference not referred to a datum
         DEMGlb[Result].DEMMapProj := DEMGlb[GridResultionToUse].DEMMapProj;
         DEMGlb[Result].DEMMapProj.ProjectionSharedWithDataset := true;
         if DEMGlb[Result].DEMheader.DEMPrecision = ByteDEM then DEMGlb[Result].DEMheader.DEMPrecision := SmallIntDEM
         else DEMGlb[Result].DEMheader.DEMPrecision := FloatingPointDEM;;
         if DEMGlb[Map1].ElevationDEM and DEMGlb[Map2].ElevationDEM then DEMGlb[Result].DEMheader.ElevUnits := euElevDiff else DEMGlb[Result].DEMheader.ElevUnits := euDifference;

         DEMGlb[Result].DEMstatus := dsUnsaved;
         if (GridResultionToUse = Map1) then begin
            ThisGrid := Map1;
            OtherGrid := Map2;
         end
         else begin
            ThisGrid := Map2;
            OtherGrid := Map1;
         end;

         if not DEMGlb[Result].AllocateDEMMemory(InitDEMMissing) then exit;

         if ShowScatterPlot then begin
            ThisGraph := TThisBaseGraph.Create(Application);
            ThisGraph.SetUpGraphForm;
            ThisGraph.Caption := 'Grid comparison';
            ThisGraph.OpenPointFile(rfile,ThisGraph.Symbol);
            ThisGraph.GraphDraw.HorizLabel := DEMGlb[Map1].AreaName;
            ThisGraph.GraphDraw.VertLabel := DEMGlb[Map2].AreaName;
         end;
         IdenticalGrids := DEMGlb[Map1].SecondGridJustOffset(Map2,xoff,yoff);
         StartProgress('Grid Differences');
         for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
            if (Col mod 25 = 0) then UpdateProgressBar(Col / pred(DEMGlb[Map1].DEMheader.NumCol));
            for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
               if IdenticalGrids then begin
                  if DEMGlb[ThisGrid].GetElevMeters(Col,Row,z1) and DEMGlb[OtherGrid].GetElevMeters(Col+xoff,Row+yoff,z2) then begin
                     DEMGlb[Result].SetGridElevation(Col,Row,z1 - z2);
                     if ShowScatterPlot then ThisGraph.AddPointToDataBuffer(rfile,z1,z2);
                  end;
               end
               else begin
                  if DEMGlb[ThisGrid].GetElevMetersFromThisAndSecondDEM(OtherGrid,Col,Row,z1,z2) then begin
                     DEMGlb[Result].SetGridElevation(Col,Row,z1 - z2);
                     if ShowScatterPlot then ThisGraph.AddPointToDataBuffer(rfile,z1,z2);
                  end;
               end;
            end {for Row};
         end {for Col};
         EndProgress;
         DEMGlb[Result].CheckMaxMinElev;
         {$If Defined(TrackZRange)} WriteLineToDebugFile('MakeDifferenceMapOfBoxRegion did check: ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange); {$EndIf}
         {$IfDef RecordFullDEMCompare}
            WriteLineToDebugFile('');  WriteLineToDebugFile('DEM1: ' + DEMGlb[Map1].FullDEMParams);
            WriteLineToDebugFile('');  WriteLineToDebugFile('DEM2: ' + DEMGlb[Map2].FullDEMParams);
            WriteLineToDebugFile('');  WriteLineToDebugFile('Diff: ' + DEMGlb[Result].FullDEMParams);
            WriteLineToDebugFile('');
         {$EndIf}

         if ShowMap then begin
            {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap) or Defined(TrackZRange)}
               WriteLineToDebugFile('MakeDifferenceMapofBoxRegion call setupmap ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange); {$EndIf}
            {$EndIf}
            if MDDef.HighlightDiffMap = 0 then mt := mtElevSpectrum
            else if MDDef.HighlightDiffMap = 1 then mt := mtGGRReflect
            else begin
               mt := 8;
               MDDef.ElevPalName := 'CET-D08';
            end;
            DEMGlb[Result].SetUpMap(Result,true,mt);
            if MDDef.AutoMergeStartDEM and ValidDEM(GridToMergeShading) then begin
               {$If Defined(TrackZRange)} WriteLineToDebugFile('MakeDifferenceMapOfBoxRegion call MergeAnotherDEM: ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange); {$EndIf}
               DEMGlb[Result].SelectionMap.MergeAnotherDEMreflectance(GridToMergeShading,true);
               {$If Defined(TrackZRange)} WriteLineToDebugFile('MakeDifferenceMapOfBoxRegion done MergeAnotherDEM: ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange); {$EndIf}
            end;
            {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap) or Defined(TrackZRange)}
               WriteLineToDebugFile('MakeDifferenceMapofBoxRegion done ShowMap ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange);
            {$EndIf}
         end;
         DEMGlb[Result].CheckMaxMinElev;  //because it is reset somewhere in the ShowMapBlock

         if ShowScatterPlot then begin
            CloseFile(rfile);
            ThisGraph.AutoScaleAndRedrawDiagram;
         end;
         if ShowHistogram then DEMGlb[Result].CreateWholeDEMHistogram;  //SingleDEMHistogram(Result);
         {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap) or Defined(TrackZRange)} WriteLineToDebugFile('MakeDifferenceMapofBoxRegion out, ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange); {$EndIf}
      finally
         HeavyDutyProcessing := false;
      end;
   end
   else begin
      {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap)} WriteLineToDebugFile('MakeDifferenceMapofBoxRegion called with invalid grid'); {$EndIf}
   end;
end;


procedure GridMinimum;
var
   Map2,Map1,
   Col,Row,DiffMap  : integer;
   z1,z2 : float32;
   NewHeader : tDEMheader;
begin
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridMinimun in'); {$EndIf}
   GetTwoCompatibleGrids('Minimum of two grids',true,Map1,Map2,false);
   NewHeader := DEMGlb[Map1].DEMheader;
   if OpenAndZeroNewDEM(true,NewHeader,DiffMap,'Minimum: ' + DEMGlb[Map2].AreaName + ' / ' + DEMGlb[Map1].AreaName,InitDEMmissing) then begin
      StartProgress('Minimum');
       for Col := 0 to pred(DEMGlb[Map1].DEMheader.NumCol) do begin
          for Row := 0 to pred(DEMGlb[Map1].DEMheader.NumRow) do begin
             if DEMGlb[Map1].GetElevMeters(Col,Row,z1) and DEMGlb[Map2].GetElevMeters(Col,Row,z2) then begin
                DEMGlb[DiffMap].SetGridElevation(Col,Row,Petmath.MinFloat(z1,z2));
             end;
          end {for Row};
          UpdateProgressBar(Col/ pred(DEMGlb[Map1].DEMheader.NumCol));
       end {for Col};
      DEMGlb[DiffMap].SetUpMap(Diffmap,true);
   end;
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridMinimum out'); {$EndIf}
end;


function PercentDifferentTwoGrids(RefDEM,TestDEM : integer; fName : PathStr) : integer;
var
   Col,Row  : integer;
   zRef,z1 : float32;
   NewHeader : tDEMheader;
   IdenticalGrids : boolean;
begin
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('PercentDifferentTwoGrids in'); {$EndIf}
   IdenticalGrids := DEMGlb[RefDEM].SecondGridIdentical(TestDEM);

   NewHeader := DEMGlb[RefDEM].DEMheader;
   NewHeader.ElevUnits := euUndefined;
   NewHeader.DEMPrecision := FloatingPointDEM;
   if OpenAndZeroNewDEM(true,NewHeader,Result,fName,InitDEMmissing) then begin
      StartProgress('Percent difference');
       for Col := 0 to pred(DEMGlb[RefDEM].DEMheader.NumCol) do begin
          for Row := 0 to pred(DEMGlb[RefDEM].DEMheader.NumRow) do begin
             if DEMGlb[RefDEM].GetElevMeters(Col,Row,zref) and (abs(zref) > 0.000001) then begin
                if DEMGlb[RefDEM].GetElevMetersFromSecondDEM(IdenticalGrids,TestDEM,Col,Row,z1) then begin
                   DEMGlb[Result].SetGridElevation(Col,Row,(z1-zref)/zRef);
                end;
             end;
          end {for Row};
          UpdateProgressBar(Col/ pred(DEMGlb[RefDEM].DEMheader.NumCol));
       end {for Col};
      {$IfDef RecordDEMCompare} WriteLineToDebugFile('Diff DEM: ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].KeyDEMParams); {$EndIf}
      DEMGlb[Result].SetUpMap(Result,true);
   end;
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('PercentDifferentTwoGrids out'); {$EndIf}
end;


function GridRatio(Map1Num : integer = 0; Map2Den : integer = 0; inMapType : tMapType = mtDEMBlank) : integer;
var
   Col,Row : integer;
   z2Den,z1Num : float32;
   NewHeader : tDEMheader;
   IdenticalGrids : boolean;
begin
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridRatio in'); {$EndIf}
   if not (ValidDEM(Map1Num) and ValidDEM( Map2Den)) then GetTwoCompatibleGrids('grid ratio=DEM1/DEM2',true,Map1Num,Map2Den,false,true);
   IdenticalGrids := DEMGlb[Map1Num].SecondGridIdentical(Map2Den);

   NewHeader := DEMGlb[Map2Den].DEMheader;
   NewHeader.ElevUnits := euUndefined;
   NewHeader.DEMPrecision := FloatingPointDEM;
   if OpenAndZeroNewDEM(true,NewHeader,Result,'Ratio: ' + DEMGlb[Map1Num].AreaName + ' / ' + DEMGlb[Map2Den].AreaName,InitDEMmissing) then begin
      StartProgress('Ratio');
       for Col := 0 to pred(DEMGlb[Map2Den].DEMheader.NumCol) do begin
          for Row := 0 to pred(DEMGlb[Map2Den].DEMheader.NumRow) do begin
             if DEMGlb[Map2Den].GetElevMeters(Col,Row,z2Den) and (abs(z2Den) > 0.000001) then begin
                if DEMGlb[Map2Den].GetElevMetersFromSecondDEM(IdenticalGrids,Map1Num,Col,Row,z1Num) then begin
                   DEMGlb[Result].SetGridElevation(Col,Row,z1Num/z2Den);
                end;
             end;
          end {for Row};
          UpdateProgressBar(Col/ pred(DEMGlb[Map2Den].DEMheader.NumCol));
       end {for Col};
      {$IfDef RecordDEMCompare} WriteLineToDebugFile('Diff DEM: ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].KeyDEMParams); {$EndIf}
      DEMGlb[Result].SetUpMap(Result,true,inMapType);
   end;
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridRatio out'); {$EndIf}
end;


function MakeChangeMap(Map1,Map2 : integer; GridLimits: tGridLimits) : integer;
var
   Col,Row,Change,NoChange  : integer;
   z1,z2,tolerance : float32;
   IdenticalGrids : boolean;
begin
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('MakeChangeMap in'); {$EndIf}
   GetTwoCompatibleGrids('change in grids',true,Map1,Map2,false);
   IdenticalGrids := DEMGlb[Map1].SecondGridIdentical(Map2);

   Result := 0;
   OpenDEMDataStructures(Result);
   DEMGlb[Result].AreaName := 'Change map: ' + DEMGlb[Map2].AreaName + ' to ' + DEMGlb[Map1].AreaName;
   DEMGlb[Result].DEMheader := DEMGlb[Map1].DEMheader;
   DEMGlb[Result].DEMheader.DEMPrecision := ByteDEM;
   if DEMGlb[Map1].ElevationDEM and DEMGlb[Map2].ElevationDEM then DEMGlb[Result].DEMheader.ElevUnits := euElevDiff else DEMGlb[Result].DEMheader.ElevUnits := euDifference;

   DEMGlb[Result].DEMstatus := dsUnsaved;

   if not DEMGlb[Result].AllocateDEMMemory(InitDEMMissing) then exit;
   Tolerance := 0.25;
   if DEMGlb[Map1].DEMheader.DEMPrecision = FloatingPointDEM then ReadDefault('Max tolerance for no change',Tolerance);
   Change := 0;
   NoChange := 0;
   if ShowSatProgress then StartProgress('Change');
   for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
      for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
         if MaskValidPoint(Col,Row) and DEMGlb[Map1].GetElevMeters(Col,Row,z1) and DEMGlb[Map1].GetElevMetersFromSecondDEM(IdenticalGrids,Map2,Col,Row,z2) then begin
            if abs(z2-z1) <= Tolerance then begin
               z1 := 0;
               inc(NoChange);
            end
            else begin
               z1 := 1;
               inc(Change);
            end;
            DEMGlb[Result].SetGridElevation(Col,Row,z1);
         end;
      end {for Row};
      if ShowSatProgress  and (Col mod 25 = 0) then UpdateProgressBar(Col / pred(DEMGlb[Map1].DEMheader.NumCol));
   end {for Col};
   MessageToContinue('Changed: ' + IntToStr(Change) + ' pixels, ' + RealToString ( 100 * Change / (Change + NoChange), -8,2) + '% of grid');
   if ShowSatProgress then EndProgress;
   DEMGlb[Result].SetUpMap(Result,true);
end;


procedure MakeElevSlopeAspectDifferenceMap;
const
   NumDiffs = 5;
var
   Col,Row,Col2,Row2  : integer;
   DEM1,DEM2,i : integer;
   Diffs : array[1..NumDiffs] of integer;
   n11,n12,n13,n21,n22,n23,
   dp,mag1,mag2,
   z1,z2,fc,fr : float32;
   Lat,Long : float64;
   IdenticalGrids : boolean;
   ThisGraph : TThisBaseGraph;
   SlopeAspect1,SlopeAspect2 : tSlopeAspectRec;

            function OpenOneResult(NewName : Shortstring; gt : tElevUnit) : integer;
            begin
               Result := 0;
               OpenDEMDataStructures(Result);
               DEMGlb[Result].AreaName := NewName;
               DEMGlb[Result].DEMheader := DEMGlb[DEM1].DEMheader;
               DEMGlb[Result].DEMMapProj := DEMGlb[DEM1].DEMMapProj;
               DEMGlb[Result].DEMheader.DEMPrecision := FloatingPointDEM;
               DEMGlb[Result].DEMheader.ElevUnits := gt;
               DEMGlb[Result].DEMstatus := dsUnsaved;
               DEMGlb[Result].AllocateDEMMemory(InitDEMmissing);
            end;

begin
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('MakeElevSlopeAspectDifferenceMap in'); {$EndIf}
   GetTwoCompatibleGrids('Grids to compare, DEM2-DEM1',true,DEM2,DEM1,false,true);  // then begin
   IdenticalGrids := DEMGlb[DEM1].SecondGridIdentical(DEM2);

   Diffs[1] := OpenOneResult( 'Elev_Diff_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName,euMeters);
   Diffs[2] := OpenOneResult( 'Slope_Diff_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName,euPercentSlope);
   Diffs[3] := OpenOneResult( 'Aspect_Diff_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName,euzDegrees);
   Diffs[4] := OpenOneResult( 'Vector_Normal_Diff_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName,euzDegrees);
   Diffs[5] := OpenOneResult( 'Vector_Downhill_Diff_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName,euzDegrees);
   //Diffs[6] := OpenOneResult( 'Roughness_Diff_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName,zDegrees);
   StartProgress('Diffs');
   for Col := 0 to pred(DEMGlb[DEM1].DEMheader.NumCol) do begin
      for Row := 0 to pred(DEMGlb[DEM1].DEMheader.NumRow) do begin
         if IdenticalGrids then begin
            Col2 := Col;
            Row2 := Row;
         end
         else begin
            DEMGlb[DEM1].DEMgridToLatLongDegree(Col,Row,Lat,Long);
            DEMGlb[DEM2].LatLongDegreeToDEMGrid(Lat,Long,fc,fr);
            Col2 := round(fc);
            Row2 := round(fr);
         end;
         if DEMGlb[DEM1].GetElevMeters(Col,Row,z1) and DEMGlb[DEM2].GetElevMeters(Col2,Row2,z2) then begin
            DEMGlb[Diffs[1]].SetGridElevation(Col,Row,z2 - z1);
            if DEMGlb[DEM1].GetSlopeAndAspect(Col,Row,SlopeAspect1) and DEMGlb[DEM2].GetSlopeAndAspect(Col2,Row2,SlopeAspect2) then begin
               DEMGlb[Diffs[2]].SetGridElevation(Col,Row,(SlopeAspect2.SlopePercent - SlopeAspect1.SlopePercent));
               z1 := SlopeAspect2.AspectDir - SlopeAspect1.AspectDir;
               if abs(z1) < 999 then begin  //aspect undefined if slope is 0
                  if (z1 > 180) then z1 := z1 - 360
                  else  if (z1 < -180) then z1 := z1 + 360;
                  DEMGlb[Diffs[3]].SetGridElevation(Col,Row,abs(z1));
               end;
               if DEMGlb[DEM1].NormalAtPoint(Col,Row, n11,n12,n13) and DEMGlb[DEM2].NormalAtPoint(Col2,Row2, n21,n22,n23) then begin
                   dp := (n11 * n21 + n12 * n22 + n13 * n23);
                   //because these are unit vectors, we can use just the dot product
                   //mag1 := sqrt(sqr(n11) + sqr(n12) + sqr(n13));
                   //mag2 := sqrt(sqr(n21) + sqr(n22) + sqr(n23));
                   //z1 :=  dp / mag1 / mag2;
                   //z1 := arccos(z1) / DegToRad;
                   //DEMGlb[Diffs[4]].SetGridElevation(Col,Row,z1);
                   DEMGlb[Diffs[4]].SetGridElevation(Col,Row,abs(arccos(dp) / DegToRad));
               end;

               if DEMGlb[DEM1].DownhillVectorAtPoint(Col,Row,n11,n12,n13) and DEMGlb[DEM2].DownhillVectorAtPoint(Col2,Row2, n21,n22,n23) then begin
                   dp := (n11 * n21 + n12 * n22 + n13 * n23);
                   DEMGlb[Diffs[5]].SetGridElevation(Col,Row,abs(arccos(dp) / DegToRad));
               end;
            end;
         end;
      end {for Row};
      if (Col mod 25 = 0) then UpdateProgressBar(Col / pred(DEMGlb[DEM1].DEMheader.NumCol));
   end {for Col};
   EndProgress;
   for i := 1 to NumDiffs do begin
      DEMGlb[Diffs[i]].SetUpMap(Diffs[i],true,mtElevSpectrum);
      DEMGlb[Diffs[i]].SelectionMap.MergeAnotherDEMreflectance(DEM2,true);
   end;
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('MakeElevSlopeAspectDifferenceMap out'); {$EndIf}
end;



