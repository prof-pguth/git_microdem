{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{     include file for demix_graphs      }
{________________________________________}


procedure GraphTwoParameterGridsByAverageSlopeAndResolution(db : integer; DEMIXtileFieldName : shortstring; MultSeries : tStringList);
//
var
   Resolutions,Filters1,Labels1,Filters2,Labels2 : tStringList;
   TheFigureFilter,TStr,BaseFilter,ResString: shortstring;
   fName : PathStr;
   f1,f2,xsize,ysize : integer;
   MaxVertAxis : float32;
   BigBitmap,Bitmap : tMyBitmap;
   gr : array[0..10,0..10] of tThisBaseGraph;
   MultTiles : tStringList;
begin {procedure GraphTwoParameterGridsByAverageSlopeAndResolution}
   SetColorForProcessing;
   BaseFilter := GISdb[db].MyData.Filter;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphTwoParameterGridsByAverageSlopeAndResolution'); {$EndIf}

   MDDef.DEMIX_filter1_fName:= DEMIXSettingsDir + 'filters_avg_slope_3_cat.dbf';
   MDDef.DEMIX_filter2_fName:= DEMIXSettingsDir + 'filters_forest_pc.dbf';
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName,Filters1,Labels1);
   ImportLandParamFilters(MDDef.DEMIX_filter2_fName,Filters2,Labels2);

   GISdb[db].EmpSource.Enabled := false;
   if MDDef.DEMIX_MultiGraphCommonScaling then MaxVertAxis := GISdb[db].MyData.FindFieldMax('DSM_SLOPE');

   GISdb[db].EmpSource.Enabled := false;
   if GISdb[db].MyData.FieldExists('GRID_SEC') then ResString := 'GRID_SEC' else ResString := 'GRID_M';
   Resolutions := Nil;
   Resolutions := GISdb[db].MyData.ListUniqueEntriesInDB(ResString);
   SortStringListNumerically(Resolutions);
   GISdb[db].dbOpts.XField := ResString;

   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Resolutions=' + IntToStr(Resolutions.Count)); {$EndIf}
   xsize := 0;
   ySize := 0;
   for f1 := 0 to pred(Filters1.Count) do begin
      wmDEM.SetPanelText(1,IntToStr(succ(f1)) + '/' + IntToStr(Filters1.Count),true);
      for f2 := 0 to pred(Filters2.Count) do begin
         TheFigureFilter := PETdbUtils.AddAndIfNeeded(BaseFilter) + Filters1.strings[f1] + ' AND ' + Filters2.strings[f2];
         GISdb[db].ApplyGISFilter(TheFigureFilter);
         GISdb[db].EmpSource.Enabled := false;
         {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Filter=' + TheFigureFilter + '  n=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
            wmDEM.SetPanelText(2,IntToStr(succ(f2)) + '/' + IntToStr(Filters2.Count) + ' ' + TheFigureFilter,true);
            MultTiles := GISdb[db].MyData.ListUniqueEntriesInDB('DTM_NAME');
            gr[f1,f2] := GISdb[db].ActuallyDrawGraph(dbgtN2DgraphMultDEMs, MultSeries,MultTiles);
            if (gr[f1,f2].GraphDraw.XWindowSize > xsize) then xsize := gr[f1,f2].GraphDraw.XWindowSize;
            if (gr[f1,f2].GraphDraw.YWindowSize > ysize) then Ysize := gr[f1,f2].GraphDraw.YWindowSize;
         end
         else gr[f1,f2] := Nil;
      end;
   end;

   CreateBitmap(BigBitmap,Filters1.Count * (xsize + 20), Filters2.Count * (ysize + 20));
   for f1 := 0 to pred(Filters1.Count) do begin
      for f2 := 0 to pred(Filters2.Count) do begin
         if (gr[f1,f2] <> Nil) and CopyImageToBitmap(gr[f1,f2].Image1,Bitmap) then begin
            BigBitMap.Canvas.Draw(f2 * (xsize + 20), F1 * (ysize + 20),Bitmap);
            Bitmap.Free;
         end;
      end;
   end;
   GetImagePartOfBitmap(BigBitmap);
   DisplayBitmap(BigBitmap,'Graph grid');
   GISdb[db].ApplyGISFilter(BaseFilter);
   MultSeries.Destroy;
   SetColorForWaiting;
   wmdem.ClearStatusBarPanelText;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphTwoParameterGridsByDEMresolution out'); {$EndIf}
end {procedure GraphTwoParameterGridsByAverageSlopeAndResolution};



procedure GraphTwoParameterGridsByDEMresolution(db : integer; Criterion,DEMIXtileFieldName : shortstring; Comparisons : tStringList);
var
   Resolutions,Filters1,Labels1,Filters2,Labels2 : tStringList;
   TheFigureFilter,TStr,BaseFilter,ResString: shortstring;
   fName : PathStr;
   f1,f2,xsize,ysize : integer;
   MaxVertAxis : float32;
   BigBitmap,Bitmap : tMyBitmap;
   gr : array[0..10,0..10] of tThisBaseGraph;


   function MakeGraph : tThisBaseGraph;
   var
      i,j : integer;
      rfile : file;
      SeriesFilter,TStr : shortstring;
      v : array[1..2] of float32;
   begin
      Result := tThisBaseGraph.Create(Application);
      GISdb[db].EmpSource.Enabled := false;
      if MDDEF.DEMIX_UseMedian then TStr := 'Median' else TStr := 'Mean';
      Result.GraphDraw.VertLabel := TStr + ' ' + Criterion;
      if GISdb[db].MyData.FieldExists('GRID_SEC') then TStr := 'arc second' else TStr := 'meters';
      Result.GraphDraw.HorizLabel := 'Grid spacing (' + TStr + ')';
      Result.GraphDraw.LRcornerText := TheFigureFilter + '  (n=' + IntToStr(GISdb[db].MyData.NumUniqueEntriesInDB(DEMIXtileFieldName)) + ')';
      {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile(Result.GraphDraw.LRcornerText); {$EndIf}

      for i := 0 to pred(Comparisons.Count) do begin
         {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile(Comparisons.Strings[i]); {$EndIf}
         Result.OpenPointSymbolFile(rfile,Comparisons.Strings[i],Result.GraphDraw.Symbol[i]);
         Result.GraphDraw.Symbol[i].DrawingSymbol := FilledBox;
         Result.GraphDraw.Symbol[i].Size := 4;
         Result.GraphDraw.Symbol[i].Color := ConvertTColorToPlatformColor(WinGraphColors(i));
         Result.GraphDraw.LineSize256[i] := 3;
         Result.GraphDraw.FileColors256[i] := ConvertTColorToPlatformColor(WinGraphColors(i));
         Result.GraphDraw.ShowLine[i] := true;
         Result.GraphDraw.ShowPoints[i] := false;

         for j := 0 to pred(Resolutions.Count) do begin
           TStr := ' AND COMPARE=' + QuotedStr(Comparisons.Strings[i]);
           SeriesFilter := TheFigureFilter + ' AND ' + ResString + '=' + Resolutions[j] + TStr;
           GISdb[db].ApplyGISFilter(SeriesFilter);
           {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile(SeriesFilter); {$EndIf}
           GISdb[db].EmpSource.Enabled := false;
           v[1] := GISdb[db].MyData.GetFieldByNameAsFloat(ResString);
           if (GISdb[db].MyData.FiltRecsInDB = 1) then begin
              v[2] := GISdb[db].MyData.GetFieldByNameAsFloat(Criterion);
           end
           else begin
              if MDDEF.DEMIX_UseMedian then v[2] := GISdb[db].MyData.FieldMedian(Criterion)
              else v[2] := GISdb[db].MyData.FieldAverage(Criterion);
           end;
           BlockWrite(Rfile,v,1);
         end;
         CloseFile(rfile);
      end;
       Result.AutoScaleAndRedrawDiagram;
       Result.GraphDraw.MinVertAxis := 0.0;
       if MDDef.DEMIX_MultiGraphCommonScaling then Result.GraphDraw.MaxVertAxis := MaxVertAxis;
       Result.GraphDraw.LeftMargin := 90;
       Result.GraphDraw.BottomMargin := 90;
       Result.GraphDraw.LLlegend := false;
       Result.GraphDraw.InsideMarginLegend := lpNEMap;
       Result.RedrawDiagram11Click(Nil);
   end;

begin {procedure GraphTwoParameterGridsByDEMresolution}
   SetColorForProcessing;
   BaseFilter := GISdb[db].MyData.Filter;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphTwoParameterGridsByDEMresolution in'); {$EndIf}
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName,Filters1,Labels1);
   ImportLandParamFilters(MDDef.DEMIX_filter2_fName,Filters2,Labels2);
   GISdb[db].EmpSource.Enabled := false;
   if MDDef.DEMIX_MultiGraphCommonScaling then MaxVertAxis := GISdb[db].MyData.FindFieldMax(Criterion);
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Comparisons=' + IntToStr(Comparisons.Count)); {$EndIf}

   GISdb[db].EmpSource.Enabled := false;
   Resolutions := Nil;
   if GISdb[db].MyData.FieldExists('GRID_SEC') then ResString := 'GRID_SEC' else ResString := 'GRID_M';
   Resolutions := GISdb[db].MyData.ListUniqueEntriesInDB(ResString);
   SortStringListNumerically(Resolutions);

   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Resolutions=' + IntToStr(Resolutions.Count)); {$EndIf}
   xsize := 0;
   ySize := 0;
   for f1 := 0 to pred(Filters1.Count) do begin
      wmDEM.SetPanelText(1,IntToStr(succ(f1)) + '/' + IntToStr(Filters1.Count),true);
      for f2 := 0 to pred(Filters2.Count) do begin
         TheFigureFilter := PETdbUtils.AddAndIfNeeded(BaseFilter) + Filters1.strings[f1] + ' AND ' + Filters2.strings[f2];
         GISdb[db].ApplyGISFilter(TheFigureFilter);
         GISdb[db].EmpSource.Enabled := false;
         {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Filter=' + TheFigureFilter + '  n=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
            wmDEM.SetPanelText(2,IntToStr(succ(f2)) + '/' + IntToStr(Filters2.Count) + ' ' + TheFigureFilter,true);
            gr[f1,f2] := MakeGraph;
            if (gr[f1,f2].GraphDraw.XWindowSize > xsize) then xsize := gr[f1,f2].GraphDraw.XWindowSize;
            if (gr[f1,f2].GraphDraw.YWindowSize > ysize) then Ysize := gr[f1,f2].GraphDraw.YWindowSize;
         end
         else gr[f1,f2] := Nil;
      end;
   end;

   CreateBitmap(BigBitmap,Filters1.Count * (xsize + 20), Filters2.Count * (ysize + 20));
   for f1 := 0 to pred(Filters1.Count) do begin
      for f2 := 0 to pred(Filters2.Count) do begin
         if (gr[f1,f2] <> Nil) and CopyImageToBitmap(gr[f1,f2].Image1,Bitmap) then begin
            BigBitMap.Canvas.Draw(f2 * (xsize + 20), F1 * (ysize + 20),Bitmap);
            Bitmap.Free;
         end;
      end;
   end;
   GetImagePartOfBitmap(BigBitmap);
   DisplayBitmap(BigBitmap,'graph_grid');
   GISdb[db].ApplyGISFilter(BaseFilter);
   SetColorForWaiting;
   wmdem.ClearStatusBarPanelText;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphTwoParameterGridsByDEMresolution out'); {$EndIf}
end {procedure GraphTwoParameterGridsByDEMresolution};



procedure GridOfCriterionForMultipleDEMcomparisons(DBonTable : integer; ColorBy : shortstring);
var
   Compares,FileList : tStringList;
   i : integer;
   fName : PathStr;
   gr : array[0..10] of tThisBaseGraph;
begin
   GISdb[DBonTable].ApplyGISFilter('GRID_SEC=1');
   GISdb[DBonTable].EmpSource.Enabled := false;
   Compares := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('COMPARE');
   GISdb[DBonTable].dbOpts.XField := 'BARREN_PC';
   GISdb[DBonTable].dbOpts.YField := 'AVG_SLOPE';
   GISdb[DBonTable].dbOpts.NumericColorField := ColorBy;
   FileList := tStringList.Create;
   GISdb[DBonTable].dbOpts.Symbol.Size := 8;
   for I := 0 to pred(Compares.Count) do begin
      GISdb[DBonTable].ApplyGISFilter('GRID_SEC=1' + ' AND COMPARE=' + QuotedStr(Compares.Strings[i]));
      gr[i] := GISdb[DBonTable].MakeGraph(dbgtN2DgraphcolorNumericField,false);
      gr[i].GraphDraw.Symbol[0].Size := 8;
      gr[i].GraphDraw.LRcornerText := Compares.Strings[i];
      gr[i].GraphDraw.InsideMarginLegend := lpNWMap;
      if StrUtils.AnsiContainsText(ColorBy,'_FUV') then begin
         gr[i].MinZ := 0;
         gr[i].MaxZ := 0.5;
      end;
      gr[i].GraphDraw.MinVertAxis := -2;
      gr[i].GraphDraw.MaxVertAxis := 90;
      gr[i].GraphDraw.MinHorizAxis := -2;
      gr[i].GraphDraw.MaxHorizAxis := 70;
      gr[i].ZUnits := GISdb[DBonTable].dbOpts.NumericColorField;
      gr[i].RedrawDiagram11Click(Nil);
      fName := NextFileNumber(MDtempdir,'fuv_grid_','.bmp');
      SaveImageAsBMP(Gr[i].Image1,fName);
      FileList.Add(fName);
   end;
   MakeBigBitmap(FileList,'',fName,2);
   GISdb[DBonTable].ClearGISFilter;
   Compares.Destroy;
   GISdb[DBonTable].ShowStatus;
end;


function GraphDEMIX_CompareDSMandDTMslopes(db : integer; DEMIX_tile,TileStats : shortstring; UseArcSecSpacing : boolean = true) : tThisBaseGraph;
var
   MultSeries,Comparisons : tStringList;
   BaseFilter,XField : shortstring;
   i : integer;
   rfile : file;
   v : array[1..2] of float32;
begin
   {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphDEMIX_CompareDSMandDTMslopes in'); {$EndIf}
    Result := nil;
    BaseFilter := GISdb[db].MyData.Filter;
    GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'TILE=' + QuotedStr(DEMIX_tile));
    if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
      Result := tThisBaseGraph.Create(Application);
      Result.GraphDraw.LRcornerText := DEMIX_tile;
      Result.GraphDraw.LLcornerText := TileStats;
      Result.GraphDraw.SetShowAllLines(true,3);
      Result.GraphDraw.SetShowAllPoints(false);
      Result.GraphDraw.VertLabel := 'Slope difference FUV';
      if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
         XField := 'GRID_SEC';
         Result.GraphDraw.HorizLabel := 'Grid spacing (arc second)';
      end
      else begin
         XField := 'GRID_M';
         Result.GraphDraw.HorizLabel := 'Average Grid spacing (m)';
      end;

      Comparisons := Nil;
      Comparisons := GISdb[db].MyData.ListUniqueEntriesInDB('COMPARE');
      for i := 0 to pred(Comparisons.Count) do begin
          GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'TILE=' + QuotedStr(DEMIX_tile) + ' AND COMPARE=' + QuotedStr(Comparisons.Strings[i]));
          Result.OpenDataFile(rfile,Comparisons.Strings[i],WinGraphColors(i));
          while not GISdb[db].MyData.eof do begin
              v[1] := GISdb[DB].MyData.GetFieldByNameAsFloat(XField);
              v[2] := GISdb[DB].MyData.GetFieldByNameAsFloat('SLPD_FUV');
              BlockWrite(rfile,v,1);
              GISdb[db].MyData.Next;
          end;
          closeFile(rFile);
      end;
       GISdb[db].ApplyGISFilter(BaseFilter);
       Result.AutoScaleAndRedrawDiagram;
       Result.GraphDraw.MinVertAxis := 0.0;
       Result.GraphDraw.LeftMargin := 90;
       Result.GraphDraw.BottomMargin := 90;
       Result.GraphDraw.InsideMarginLegend := lpNEMap;
       Result.RedrawDiagram11Click(Nil);
   end;
   {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphDEMIX_CompareDSMandDTMslopes out'); {$EndIf}
end;


function GraphCompareDSMandDTMslopes(db : integer; DTMName : shortstring) : tThisBaseGraph;
var
   MultSeries : tStringList;
   BaseFilter : shortstring;
begin
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphCompareDSMandDTMslopes in'); {$EndIf}
  Result := nil;
  if GISdb[db].MyData.FieldExists('GRID_THIN') and GISdb[db].MyData.FieldExists('DSM_NAME') and GISdb[db].MyData.FieldExists('DTM_NAME') then begin
      BaseFilter := GISdb[db].MyData.Filter;
      GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'DTM_NAME=' + QuotedStr(DTMName));
      if GISdb[db].MyData.FiltRecsinDB > 0 then begin
         if GISdb[db].MyData.FieldExists('GRID_SEC') then GISdb[db].dbOpts.XField := 'GRID_SEC' else GISdb[db].dbOpts.XField := 'GRID_M' ;
          MultSeries := tStringList.Create;
          MultSeries.Add('DSM_SLOPE');
          MultSeries.Add('DTM_SLOPE');
          (*
          MultSeries.Add('ELVD_R2');
          MultSeries.Add('SLPD_R2');
          MultSeries.Add('RRID_R2');
          *)
          Result := GISdb[db].ActuallyDrawGraph(dbgtN2DgraphMultSeries,MultSeries);
            Result.GraphDraw.MinHorizAxis := 0;
            Result.GraphDraw.MinVertAxis := 0;
            Result.GraphDraw.LeftMargin := 90;
            Result.GraphDraw.BottomMargin := 80;
            Result.GraphDraw.InsideMarginLegend := lpNEMap;
            Result.GraphDraw.LRcornerText := DTMName;
            Result.RedrawDiagram11Click(Nil);
      end;
      GISdb[db].ApplyGISFilter(BaseFilter);
  end
  else MessageToContinue('Required fields missing');
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphCompareDSMandDTMslopes out'); {$EndIf}
end;


function GraphDSMandDTMdifferences(db : integer; DTMName,LSP : shortstring): tThisBaseGraph;
var
   MultSeries : tStringList;
   BaseFilter : shortstring;

   procedure CheckLSP(LSP : shortstring);
   begin
      if GISdb[db].MyData.FieldExists(LSP) then MultSeries.Add(LSP);
   end;

begin
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphDSMandDTMdifferences LSP=' + LSP + '  ' + DTMName); {$EndIf}
  Result := nil;
  if GISdb[db].MyData.FieldExists('GRID_THIN') and GISdb[db].MyData.FieldExists('DSM_NAME') and GISdb[db].MyData.FieldExists('DTM_NAME') then begin
      StringReplace(LSP,'_FUV','',[rfReplaceAll, rfIgnoreCase]);
      BaseFilter := GISdb[db].MyData.Filter;
      GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'DTM_NAME=' + QuotedStr(DTMName));
      if GISdb[db].MyData.FiltRecsInDB > 0 then begin
          //GISdb[db].dbOpts.XField := 'GRID_SIZE';
          if GISdb[db].MyData.FieldExists('GRID_SEC') then GISdb[db].dbOpts.XField := 'GRID_SEC' else GISdb[db].dbOpts.XField := 'GRID_M' ;
          MultSeries := tStringList.Create;

          MultSeries := tStringList.Create;
          CheckLSP(LSP + '_LE90');
          CheckLSP(LSP + '_RMSE');
          CheckLSP(LSP + '_MAE');
          CheckLSP(LSP + '_MED');
          CheckLSP(LSP + '_MEAN');
          if (MultSeries.Count = 0) then begin
             MultSeries.Destroy;
             {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphDSMandDTMdifferences no series found'); {$EndIf}
          end
          else begin
              Result := GISdb[db].ActuallyDrawGraph(dbgtN2DgraphMultSeries, MultSeries);
              Result.GraphDraw.MinHorizAxis := 0;
              Result.GraphDraw.MinVertAxis := -2;
              Result.GraphDraw.LeftMargin := 90;
              Result.GraphDraw.BottomMargin := 80;
              Result.GraphDraw.InsideMarginLegend := lpNEMap;
              Result.GraphDraw.LRcornerText := DTMName;
              Result.RedrawDiagram11Click(Nil);
              GISdb[db].ApplyGISFilter(BaseFilter);
          end;
      end;
      //MultSeries.Destroy;
  end
  else MessageToContinue('Required fields missing');
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphDSMandDTMdifferences out'); {$EndIf}
end {function GraphDSMandDTMdifferences};


procedure MakeDBtoCompareDSMandDTM(HRDEM : boolean; DSMName,DTMname,OutName : PathStr; Area,Tile,TileStats : shortstring; var Results : tStringList);
//shortens for legends, and avoids duplicates with CopDEM when spacing changes
var
   db,DTM,DSM,DTMthin,DSMthin : integer;
   MultSeries : tStringList;
   CompareName,TStr,CountryStr,TileAverages : shortstring;

         procedure ComparePair(DEM1,DEM2,thin : integer);
         var
            Grid1,Grid2,
            slope1,slope2 : integer;
            aline : shortstring;
            Lat,Long : float64;

                function GridMean(DEM : integer) : float64;
                var
                   Col,Row,N : int64;
                   z : float32;
                begin
                   n := 0;
                   Result := 0;
                   for col := 0 to pred(DEMglb[DEM].DEMheader.NumCol) do begin
                      for Row := 0 to pred(DEMglb[DEM].DEMheader.NumRow) do begin
                         if DEMglb[DEM].GetElevMetersOnGrid(Col,Row,z) then begin
                            Result := Result + z;
                            inc(n);
                         end;
                      end;
                   end;
                   Result := Result / n;
                end;

                procedure ComputePair(DEM1,DEM2 : integer);
                var
                   r2 : float64;
                   mv : tMomentVar;
                begin
                   r2 := CorrelationTwoGrids(DEMglb[DEM1].FullDEMgridLimits,DEM1,DEM2);
                   mv := GridDifferenceStatsTwoGrids(DEMglb[DEM1].FullDEMgridLimits,DEM1,DEM2);
                   aLine := aLine + ',' + RealToString(1-r2,-12,-8) + ',' + RealToString(mv.Mean,-8,-4) +  ',' + RealToString(mv.Median,-8,-2) +  ',' + RealToString(mv.Std_Dev,-8,-2) +  ',' +
                       RealToString(mv.MAE,-8,-2) +  ',' + RealToString(mv.RMSE,-8,-2) +  ',' + RealToString(mv.LE90,-8,-2);
                end;

         begin  {procedure ComparePair}
            DEMglb[DEM1].DEMCenterPoint(Lat,Long);
            slope1 := CreateEvansSlopeMapPercent(false,DEM1);
            slope2 := CreateEvansSlopeMapPercent(false,DEM2);
            if HRDEM then begin
               CompareName := 'DTM_to_DSM';
               CountryStr := Tile + '_';
            end
            else begin
               CompareName := GeneralizeReferenceName(ExtractFileNameNoExt(DSMname)) + '_to_' + GeneralizeReferenceName(ExtractFileNameNoExt(DTMname));
               CountryStr := '';
            end;
            TStr := RealToString(DEMglb[DEM1].AverageSpace,-8,-1) + ',';

            aline := CountryStr + ExtractFileNameNoExt(DTMname) + ',' + CountryStr + ExtractFileNameNoExt(DSMname) + ',' + CompareName + ',' +
                 Area + ',' + Tile + ',' + TStr + IntToStr(Thin) + ',' +RealToString(Lat,-12,-6) + ',' +
                 RealToString(Long,-12,-6) + ',' + RealToString(GridMean(Slope2),-12,-2) + ',' + RealToString(GridMean(Slope1),-12,-2) + ',' +
                 TileStats;

            ComputePair(DEM1,DEM2);   //for elevation

            ComputePair(Slope1,Slope2);
            CloseSingleDEM(Slope1);
            CloseSingleDEM(Slope2);

        (*
            grid1 := MakeRRIGrid(DEM1,false);
            grid2 := MakeRRIGrid(DEM2,false);
            ComputePair(Grid1,Grid2);
            CloseSingleDEM(Grid1);
            CloseSingleDEM(Grid2);
        *)

            Results.Add(aline);
         end {procedure ComparePair};


         procedure Thinned(i : integer);
         begin
            DTMthin := DEMglb[DTM].ThinThisDEM(false,'',i,true);
            DSMthin := DEMglb[DSM].ThinThisDEM(false,'',i,true);
            ComparePair(DSMthin,DTMthin,i);
            CloseSingleDEM(DTMthin);
            CloseSingleDEM(DSMthin);
         end;

         function LSPparamsString(LSP : shortstring) : shortstring;
         begin
            Result := ',' + LSP + '_FUV,' + LSP  + '_MEAN,' +  LSP + '_MED,' +  LSP + '_STD,' + LSP + '_MAE,' + LSP + '_RMSE,' + LSP + '_LE90';
         end;

begin {procedure CompareDSMandDTM}
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('CompareDSMandDTM ' + DSMname + '  ' + DTMName); {$EndIf}

  SetColorForProcessing;

  if (Results = Nil) then begin
     Results := tStringList.Create;
     TStr :=  LSPparamsString('ELVD') + LSPparamsString('SLPD');  //+ LSPparamsString('RRID');
     TileAverages := ',AVG_SLOPE,BARREN_PC,FOREST_PC';
     if HRDEM then begin
        Results.Add('DTM_NAME,DSM_NAME,COMPARE,AREA,COUNTRY,GRID_THIN,GRID_M,LAT,LONG,DTM_SLOPE,DSM_SLOPE' + TileAverages + TStr);
     end
     else begin
        Results.Add('DEM_1,DEM_2,COMPARE,AREA,DEMIX_TILE,GRID_THIN,GRID_SEC,LAT,LONG,DEM1_SLOPE,DEM2_SLOPE' + TileAverages + TStr);

     end;
  end;
  DTM := OpenNewDEM(DTMName,false);
  DSM := OpenNewDEM(DSMName,false);
  ComparePair(DSM,DTM,1);

  if HRDEM then begin  //this is an HRDEM comparison in meters
     Thinned(2);
     Thinned(3);
     Thinned(4);
     Thinned(5);
     Thinned(8);
     Thinned(10);
     Thinned(12);
     Thinned(15);
     Thinned(20);
     Thinned(25);
     Thinned(30);
     if DEMglb[DTM].AverageSpace < 1.25 then begin
        Thinned(45);
        Thinned(60);
     end;
   end
   else begin    //this is an arc second GDEM comparison
      Thinned(2);
      Thinned(3);
      Thinned(4);
      Thinned(5);
      Thinned(6);
      Thinned(8);
      Thinned(10);
      Thinned(12);
   end;
  if (OutName <> '') then db := StringList2CSVtoDB(Results,OutName);
end {procedure CompareDSMandDTM};


procedure MakeGraphComparingFUVandArcSecondSpacing(db : integer; Criterion,DEMIXtileFieldName : shortstring);
var
   Comparisons,Resolutions,DEMIX_Tiles,Filters1,Labels1,Filters2,Labels2,FileList : tStringList;
   TheFigureFilter,BaseFilter,XField : shortstring;
   f1,f2 : integer;
   fName : PathStr;
   MaxVertAxis : float32;


   function MakeGraph : tThisBaseGraph;
   var
      i,j,k : integer;
      rfile : file;
      SeriesFilter,Comparison,Tile : shortstring;
      v : array[1..2] of float32;
   begin
      Result := tThisBaseGraph.Create(Application);
      GISdb[db].EmpSource.Enabled := false;
      Result.GraphDraw.LRcornerText := TheFigureFilter + '  (n=' + IntToStr(GISdb[db].MyData.NumUniqueEntriesInDB(DEMIXtileFieldName)) + ')';
      Result.GraphDraw.VertLabel := Criterion;
      if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
         Result.GraphDraw.HorizLabel := 'Grid spacing (arc second)';
      end
      else begin
         Result.GraphDraw.HorizLabel := 'Grid spacing (meters)';
     end;

       i := -1;
       for k := 0 to pred(DEMIX_Tiles.Count) do begin
         comparison := 'cop to ref_dtm';
         Tile := DEMIX_Tiles.Strings[k];
         SeriesFilter := TheFigureFilter + ' AND COMPARE=' + QuotedStr(Comparison) + ' AND TILE=' + QuotedStr(tile);
         GISdb[db].ApplyGISFilter(SeriesFilter);
         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
             inc(i);
             Result.GraphDraw.Symbol[i].DrawingSymbol := FilledBox;
             Result.GraphDraw.Symbol[i].Size := 4;
             Result.GraphDraw.Symbol[i].Color := ConvertTColorToPlatformColor(WinGraphColors(i));
             Result.GraphDraw.ShowLine[i] := true;
             Result.GraphDraw.ShowPoints[i] := false;
             Result.GraphDraw.LineSize256[i] := 3;
             Result.OpenPointSymbolFile(rfile,Tile,Result.GraphDraw.Symbol[i]);

             for j := 0 to pred(Resolutions.Count) do begin
               SeriesFilter := TheFigureFilter + ' AND GRID_SEC=' + Resolutions[j] + ' AND COMPARE=' + QuotedStr(Comparison) + ' AND TILE=' + QuotedStr(tile);
               GISdb[db].ApplyGISFilter(SeriesFilter);
               {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile(SeriesFilter); {$EndIf}
               GISdb[db].EmpSource.Enabled := false;
               v[1] := GISdb[db].MyData.GetFieldByNameAsFloat(XField);
               v[2] := GISdb[db].MyData.GetFieldByNameAsFloat(Criterion);
               BlockWrite(Rfile,v,1);
             end;
             CloseFile(rfile);
         end;
      end;

      Result.AutoScaleAndRedrawDiagram;
      Result.GraphDraw.MinVertAxis := 0.0;
      Result.GraphDraw.MaxVertAxis := MaxVertAxis;
      Result.GraphDraw.LeftMargin := 90;
      Result.GraphDraw.BottomMargin := 90;
      Result.GraphDraw.InsideMarginLegend := lpNone;
      Result.RedrawDiagram11Click(Nil);
      fName := NextFileNumber(MDtempdir,'fuv_grid_lines','.bmp');
      SaveImageAsBMP(Result.Image1,fName);
   end;

begin {procedure MakeGraphComparingFUVandArcSecondSpacing};
   SetColorForProcessing;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TCompareDSM_DTMform.BitBtn9Click in'); {$EndIf}
   BaseFilter := GISdb[db].MyData.Filter;
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName,Filters1,Labels1);
   ImportLandParamFilters(MDDef.DEMIX_filter2_fName,Filters2,Labels2);
   GISdb[db].EmpSource.Enabled := false;
   Comparisons := GISdb[db].MyData.ListUniqueEntriesInDB('COMPARE');
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Comparisons=' + IntToStr(Comparisons.Count)); {$EndIf}

    if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
       XField := 'GRID_SEC';
    end
    else begin
       XField := 'GRID_M';
   end;

   GISdb[db].EmpSource.Enabled := false;
   Resolutions := GISdb[db].MyData.ListUniqueEntriesInDB(XField);
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Resolutions=' + IntToStr(Resolutions.Count)); {$EndIf}

   DEMIX_Tiles := GISdb[db].MyData.ListUniqueEntriesInDB(DEMIXtileFieldName);
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Resolutions=' + IntToStr(Resolutions.Count)); {$EndIf}

   MaxVertAxis := GISdb[db].MyData.FindFieldMax('SLPD_FUV');
   FileList := tStringList.Create;

   for f1 := 0 to pred(Filters1.Count) do begin
      for f2 := 0 to pred(Filters2.Count) do begin
         TheFigureFilter := PETdbUtils.AddAndIfNeeded(BaseFilter) + Filters1.strings[f1] + ' AND ' + Filters2.strings[f2];
         GISdb[db].ApplyGISFilter(TheFigureFilter);
         {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Filter=' + TheFigureFilter + '  n=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
            MakeGraph;
            FileList.Add(fName);
         end
         else FileList.Add('');
      end;
   end;
   GISdb[db].ApplyGISFilter(BaseFilter);
   SetColorForWaiting;
   MakeBigBitmap(FileList,'',fName,Filters2.Count);
   GISdb[db].ApplyGISFilter(BaseFilter);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('TCompareDSM_DTMform.BitBtn9Click out'); {$EndIf}
end {procedure MakeGraphComparingFUVandArcSecondSpacing};



