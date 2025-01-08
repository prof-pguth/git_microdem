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


procedure MissingPointsInGrids(MapForm : tMapForm; DEM1 : integer = 0; DEM2 : integer = 0);
var
   Incr,Col,Row : integer;
   Total : int64;
   Lat,Long,XGrid,YGrid : float64;
   z1,z2 : float32;
   GridLimits : tGridLimits;
   sl : tStringList;
   Name1,Name2 : shortstring;
begin
   if (DEM1 = 0) and (DEM2 = 0) then GetTwoCompatibleGrids('DEM1, DEM2',false,DEM1,DEM2,false);
   if DEMGlb[DEM1].SecondGridIdentical(DEM2) then begin
      {$If Defined(RecordProblems) or Defined(RecordDEMMapProj)}
         WriteLineToDebugFile('MissingPointsInGrids, ' + DEMGlb[DEM1].AreaName + ' ' + DEMGlb[DEM1].DEMMapProj.GetProjName + '   ' + DEMGlb[DEM2].AreaName + ' ' + DEMGlb[DEM2].DEMMapProj.GetProjName);
      {$EndIf}
      Name1 := DEMGlb[DEM1].AreaName;
      while Length(Name1) > 10 do begin
         GetValidDBfieldname(Name1);
      end;
      Name2 := DEMGlb[DEM2].AreaName;
      while Length(Name2) > 10 do begin
         GetValidDBfieldname(Name2);
      end;

      sl := tStringList.Create;
      sl.add('COL,ROW,LAT,LONG,' + Name1 + ',' + Name2);

      Col := GridLimits.XGridLow;
      for Col := 0 to pred(DEMGlb[DEM1].DEMHeader.NumCol) do begin
         for Row := 0 to pred(DEMGlb[DEM1].DEMHeader.NumRow) do begin
            DEMGlb[DEM1].DEMGridToLatLongDegree(Col,Row,Lat,Long);
            if DEMGlb[DEM1].GetElevMeters(Col,Row,z1) then begin
               if DEMGlb[DEM2].GetElevMeters(Col,Row,z2) then begin
               end
               else sl.add(IntToStr(Col) + ',' + IntToStr(Row) + ',' + RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' + RealToString(z1,-8,2) + ',-9999');
            end
            else begin
               if DEMGlb[DEM2].GetElevMeters(Col,Row,z2) then begin
                  sl.add(IntToStr(Col) + ',' + IntToStr(Row) + ',' + RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',-9999,' + RealToString(z2,-8,2));
               end;
            end;
         end;
      end;
      if (sl.Count = 1) then begin
         sl.Destroy;
         MessageToContinue('No points missing in just one grid');
      end
      else MapForm.StringListToLoadedDatabase(sl, Petmar.NextFileNumber(MDTempDir, 'Missing_Data_Compare_', DefaultDBExt),true,false,true);
   end
   else begin
      MessageToContinue('Requires two idential grids');
   end;
end;


procedure GridCoOccurrence(AutoFull : boolean = false; DEM1 : integer = 0; DEM2 : integer = 0; Percentages : boolean = true);
var
   Incr,Col,Row : integer;
   Total : int64;
   Lat,Long,XGrid,YGrid : float64;
   z1,z2: float32;
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
      DEMGlb[DiffMap].SetUpMap(true);
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
      DEMGlb[Result].SetupMap(true);
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
      DEMGlb[Result].SetupMap(true,inMapType);
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
   DEMGlb[Result].SetupMap(true);
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
   z1,z2 : float32;
   Lat,Long,fc,fr : float64;
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
   Diffs[3] := OpenOneResult( 'Aspect_Diff_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName,euDegrees);
   Diffs[4] := OpenOneResult( 'Vector_Normal_Diff_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName,euDegrees);
   Diffs[5] := OpenOneResult( 'Vector_Downhill_Diff_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName,euDegrees);
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
               z1 := SlopeAspect2.AspectDirTrue - SlopeAspect1.AspectDirTrue;
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
      DEMGlb[Diffs[i]].SetUpMap(true,mtElevSpectrum);
      DEMGlb[Diffs[i]].SelectionMap.MergeAnotherDEMreflectance(DEM2,true);
   end;
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('MakeElevSlopeAspectDifferenceMap out'); {$EndIf}
end;



