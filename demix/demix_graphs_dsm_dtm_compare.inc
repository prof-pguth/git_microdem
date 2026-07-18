{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{     include file for demix_graphs      }
{________________________________________}


function XSpacingField(db : integer; var xLabel : shortstring) : shortstring;
var
   TStr : shortstring;
begin
    if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
       Result := 'GRID_SEC';
       TStr := 'arc second';
    end
    else begin
       Result := 'GRID_M';
       TStr := 'meters';
   end;
   xLabel := 'Grid spacing (' + TStr + ')';
end;


function StartIndividualGraph(db : integer; Resolutions : tStringList) : tThisBaseGraph;
var
   Extra : float32;
begin
   Result := tThisBaseGraph.Create(Application);
   if (XSpacingField(db,Result.GraphDraw.HorizLabel) = 'GRID_SEC') then begin
      Extra := 0.5;
   end
   else begin
      Extra:= 5;
   end;
   if Resolutions <> Nil then Result.GraphDraw.MaxHorizAxis := StrToInt(Resolutions[pred(Resolutions.Count)]) + Extra;
   Result.GraphDraw.MaxVertAxis := -99e38;
   Result.GraphDraw.MinVertAxis :=  99e38;
end;


procedure EndSingleGraph(var Graph : tThisBaseGraph);
var
   VertRange : float64;
begin
   VertRange := Graph.GraphDraw.MaxVertAxis - Graph.GraphDraw.MinVertAxis;
   {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile('EndSingleGraph, VertRange='+ RealToString(VertRange,-12,-4)); {$EndIf}
   Graph.GraphDraw.MinVertAxis := Graph.GraphDraw.MinVertAxis - 0.025 * VertRange;
   Graph.GraphDraw.MaxVertAxis := Graph.GraphDraw.MaxVertAxis + 0.025 * VertRange;
   Graph.GraphDraw.LeftMargin := 100;
   Graph.GraphDraw.BottomMargin := 90;
   Graph.GraphDraw.SetShowAllPoints(false);
   Graph.GraphDraw.SetShowAllLines(true,4);
   Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
   Graph.DefaultClientWidth := MDDef.DEMIX_xsize;
   Graph.DefaultClientHeight := MDDef.DEMIX_ysize;
   Graph.GraphDraw.ForceNewSize := true;
   Graph.FormResize(Nil);
   {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile('EndSingleGraph out');  {$EndIf}
end;

procedure GridScatterPlotByDEMResolution(db : integer; Criterion,Coloring : shortstring);

   function MakeGraphByResolution(db : integer; FigureLabel,TheFigureFilter,ResString,Criterion : shortstring) : tThisBaseGraph;
   var
      i,j,k : integer;
      rfile : file;
      SeriesFilter,TStr : shortstring;
      v : array[1..3] of float32;
   begin
      Result := StartIndividualGraph(db,Nil);
      Result.GraphDraw.LRcornerText := {'Criterion=' + Criterion + '  ' +} FigureLabel;
      Result.GraphDraw.RainbowColors := true;
      Result.GraphDraw.VertLabel := Criterion;
      {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile(Result.GraphDraw.LRcornerText); {$EndIf}
      Result.OpenXYZFile(rfile);
      while not GISdb[db].MyData.eof do begin
         if GISdb[db].GetFloat32FromTableLink(ResString,v[1]) and GISdb[db].GetFloat32FromTableLink(Criterion,v[2]) and GISdb[db].GetFloat32FromTableLink(Coloring,v[3]) then begin
            BlockWrite(Rfile,v,1);
            Petmath.CompareValueToExtremes(v[1],Result.GraphDraw.MinHorizAxis,Result.GraphDraw.MaxHorizAxis);
            Petmath.CompareValueToExtremes(v[2],Result.GraphDraw.MinVertAxis,Result.GraphDraw.MaxVertAxis);
            Petmath.CompareValueToExtremes(v[3],Result.MinZ,Result.MaxZ);
         end;
         GISdb[db].MyData.Next;
      end;
      Result.GraphDraw.InsideMarginLegend := lpNEMap;
      EndSingleGraph(Result);
      for i := 0 to 15 do Result.GraphDraw.Symbol[i].Size := MDDef.DEMIXsymSize;
      Result.GraphDraw.ForceNewSize := true;
      Result.FormResize(Nil);
   end {function MakeGraphByResolution};


var
   Filters1,Labels1,Filters2,Labels2 : tStringList;
   TheFigureFilter,FigureLabel,TStr,BaseFilter,ResString: shortstring;
   fName : PathStr;
   f1,f2 : integer;
   gr : t2Dgrapharray;
begin {procedure GridScatterPlotByDEMResolution}
   if TileCharacteristicsInDB(DB) then begin
       GISdb[db].EmpSource.Enabled := false;
       StartGraphGrid(db,BaseFilter,ResString,Filters1,Labels1,Filters2,Labels2);
       {$IfDef RecordDEMIX} WriteLineToDebugFile('GridScatterPlotByDEMResolution in, Criterion=' + Criterion); {$EndIf}
       GISdb[db].EmpSource.Enabled := false;
       for f1 := 0 to pred(Filters1.Count) do begin
          wmDEM.SetPanelText(1,IntToStr(succ(f1)) + '/' + IntToStr(Filters1.Count),true);
          for f2 := 0 to pred(Filters2.Count) do begin
             TheFigureFilter := PETdbUtils.AddAndIfNeeded(BaseFilter) + Filters1.strings[f1] + ' AND ' + Filters2.strings[f2] + HRDEMresolutionFilter;
             GISdb[db].ApplyGISFilter(TheFigureFilter);
             GISdb[db].EmpSource.Enabled := false;
             {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('TheFigureFilter=' + TheFigureFilter + '  n=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
             if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
                 FigureLabel := PETdbUtils.AddAndIfNeeded(BaseFilter) + ' tiles=' + IntToStr(GISdb[db].MyData.NumUniqueEntriesInDB('DTM_NAME'));
                 wmDEM.SetPanelText(2,IntToStr(succ(f2)) + '/' + IntToStr(Filters2.Count) + ' ' + TheFigureFilter,true);
                 gr[f1,f2] := MakeGraphByResolution(db,FigureLabel,TheFigureFilter,ResString,Criterion);
             end
             else gr[f1,f2] := Nil;
          end;
       end;
       EndGraphGrid(gr,db, BaseFilter,Filters1,Labels1,Filters2,Labels2);
       {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphMultipleParamsByDEMResolution out'); {$EndIf}
   end;
end {procedure GridScatterPlotByDEMResolution};





procedure GraphMultParamsByAvgSlope_DEMresolution(db : integer; DEMIXtileFieldName : shortstring; Resolutions,MultSeries : tStringList; AllTiles : boolean = false);
//
var
   Filters1,Labels1,Filters2,Labels2 : tStringList;
   TheFigureFilter,FigureLabel,TStr,BaseFilter,ResString: shortstring;
   fName : PathStr;
   f1,f2 : integer;
   RowMinZ,RowMaxZ : float64;
   gr : t2Dgrapharray;
   MultTiles,Colors : tStringList;


   function MakeGraph : tThisBaseGraph;
   var
      i,j,k,Onfile : integer;
      rfile : file;
      LegendEntries : tStringList;
      SeriesFilter,TStr,Criterion : shortstring;
      v : array[1..2] of float32;
      amin,amax : float64;
      bmp : tMyBitmap;
      Symbol : tFullSymbolDeclaration;
   begin {function MakeGraph}
      Result := StartIndividualGraph(db,Resolutions);
         if AllTiles then TStr := ''
         else begin
            if MDDEF.DEMIX_UseMedian then TStr := 'Median' else TStr := 'Mean';
         end;
         Result.GraphDraw.VertLabel := TStr + ' ' + MultSeries.Strings[0];
         for i := 1 to pred(MultSeries.Count) do begin
             Result.GraphDraw.VertLabel := Result.GraphDraw.VertLabel + '/' + MultSeries.Strings[i];
         end;

         if (FigureLabel <> '') then Result.GraphDraw.LRCornerText := FigureLabel
         else Result.GraphDraw.LRCornerText := BaseFilter + ' (n=' + IntToStr(MultTiles.Count) + ')';

        if AllTiles then begin
           for k := 0 to pred(MultTiles.Count) do begin
             Symbol.DrawingSymbol := FilledBox;
             Symbol.Size := 5;
             if (MultSeries.Count > 1) then Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors(j))
             else Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors(k));
             GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'DTM_NAME=' + QuotedStr(MultTiles.Strings[k]) + HRDEMresolutionFilter);
             GISdb[db].EmpSource.Enabled := false;
             OnFile := Result.OpenPointSymbolFile(rfile,MultTiles.Strings[k],Symbol,4);
             GISdb[db].MyData.First;
             repeat
                if GISdb[db].GetFloat32FromTableLink(GISdb[db].dbOpts.XField,v[1]) and GISdb[db].GetFloat32FromTableLink(MultSeries.Strings[0],v[2]) then begin
                   CompareValueToExtremes(v[2],Result.GraphDraw.MinVertAxis,Result.GraphDraw.MaxVertAxis);
                   BlockWrite(Rfile,v,1);
                end;
                GISdb[db].MyData.Next;
             until GISdb[db].MyData.eof;
             CloseFile(rfile);
           end;
            if (MultTiles.Count <= MDDef.DEMIX_MaxTilesInLegend) then Result.GraphDraw.InsideMarginLegend := lpNEMap;
        end
        else begin
            if (MultSeries.Count > 1) then begin
                LegendEntries := tStringList.Create;
                Colors := tStringList.Create;
                for j := 0 to pred(MultSeries.Count) do begin
                   LegendEntries.Add(MultSeries.Strings[j]);
                   Colors.Add(IntToStr(WinGraphColors(j)));
                   {$If Defined(LegendColors)} WriteLineToDebugFile('Legend:  ' + IntToStr(j) + '  ' + MultSeries.Strings[j] + '  ' + ColorToString(WinGraphColors(j))); {$EndIf}
                end;
                MDDef.DEMIXlegendFontSize := 12;
                bmp := MakeLegend(LegendEntries,Colors);
                Result.GraphDraw.BottomLegendFName := NextFileNumber(MDTempDir,'series_label_','.bmp');
                SaveBitmap(bmp,Result.GraphDraw.BottomLegendFName);
                Colors.Destroy;
                LegendEntries.Destroy;
                bmp.Destroy;
                Result.GraphDraw.InsideMarginLegend := lpNEMap;
            end;

         for j := 0 to pred(MultSeries.Count) do begin
             Symbol.DrawingSymbol := FilledBox;
             Symbol.Size := 5;
             Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors(j));
             Criterion := MultSeries.Strings[j];
             Result.OpenPointSymbolFile(rfile,MultSeries.Strings[j],Symbol,4);
             for k := 0 to pred(Resolutions.Count) do begin
               if (StrToFloat(Resolutions[k]) > 1.9) then begin
                   SeriesFilter := TheFigureFilter + ' AND ' + ResString + '=' + Resolutions.Strings[k];
                   GISdb[db].ApplyGISFilter(SeriesFilter);
                   GISdb[db].EmpSource.Enabled := false;
                   if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
                       repeat
                            v[1] := GISdb[db].MyData.GetFieldByNameAsFloat(ResString);
                            if (GISdb[db].MyData.FiltRecsInDB = 1) then begin
                               v[2] := GISdb[db].MyData.GetFieldByNameAsFloat(Criterion);
                            end
                            else begin
                               if MDDEF.DEMIX_UseMedian then v[2] := GISdb[db].MyData.FieldMedian(Criterion)
                               else v[2] := GISdb[db].MyData.FieldAverage(Criterion);
                            end;
                            if v[2] < Result.GraphDraw.MinVertAxis then Result.GraphDraw.MinVertAxis := v[2];
                            if v[2] > Result.GraphDraw.MaxVertAxis then Result.GraphDraw.MaxVertAxis := v[2];
                            BlockWrite(Rfile,v,1);
                            GISdb[db].MyData.Next;
                       until GISdb[db].MyData.eof;
                   end;
               end;
             end;
             CloseFile(rfile);
         end;
       end;
       {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('MakeGraph, data files created '+ Result.GraphDraw.AxisRange); {$EndIf}
       //Result.GraphDraw.MinVertAxis := RowMinZ;
       //Result.GraphDraw.MaxVertAxis := RowMaxZ;
       (*
        Result.MinZ := RowMinZ;
        Result.MaxZ := RowMaxZ;
        Result.MinZShow := Result.MinZ;
       *)
       EndSingleGraph(result);
   end {function MakeGraph};


begin {procedure GraphTwoParameterGridsByAverageSlopeAndResolution}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphTwoParameterGridsByAverageSlopeAndResolution'); {$EndIf}
   StartGraphGrid(db,BaseFilter,ResString,Filters1,Labels1,Filters2,Labels2);
   GISdb[db].EmpSource.Enabled := false;
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Resolutions=' + IntToStr(Resolutions.Count)); {$EndIf}
   for f1 := 0 to pred(Filters1.Count) do begin
       wmDEM.SetPanelText(1,IntToStr(succ(f1)) + '/' + IntToStr(Filters1.Count),true);
       TheFigureFilter := PETdbUtils.AddAndIfNeeded(BaseFilter) + Filters1.strings[f1];
       GISdb[db].ApplyGISFilter(TheFigureFilter);
       GISdb[db].EmpSource.Enabled := false;
       RowMinZ := 99e38;
       RowMaxZ := -99e38;
       for f2 := 0 to pred(Filters2.Count) do begin
         TheFigureFilter := PETdbUtils.AddAndIfNeeded(BaseFilter) + Filters1.strings[f1] + ' AND ' + Filters2.strings[f2];
         GISdb[db].ApplyGISFilter(TheFigureFilter);
         FigureLabel := PETdbUtils.AddAndIfNeeded(BaseFilter) + {Labels1.strings[f1] + ' & ' + Labels2.strings[f2] +} ' n=' + IntToStr(GISdb[db].MyData.NumUniqueEntriesInDB(DEMIXtileFieldName));
         GISdb[db].EmpSource.Enabled := false;
         {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Filter=' + TheFigureFilter + ' ' + FigureLabel); {$EndIf}

         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
            wmDEM.SetPanelText(2,IntToStr(succ(f2)) + '/' + IntToStr(Filters2.Count) + ' ' + TheFigureFilter,true);
            MultTiles := GISdb[db].MyData.ListUniqueEntriesInDB(DEMIXtileFieldName);
            gr[f1,f2] := MakeGraph;
            if gr[f1,f2].GraphDraw.MaxVertAxis > RowMaxz then RowMaxz := gr[f1,f2].GraphDraw.MaxVertAxis;
            if gr[f1,f2].GraphDraw.MinVertAxis < RowMinz then RowMinz := gr[f1,f2].GraphDraw.MinVertAxis;
         end
         else gr[f1,f2] := Nil;
      end;
       for f2 := 0 to pred(Filters2.Count) do begin
          if gr[f1,f2] <> Nil then begin
             gr[f1,f2].GraphDraw.MaxVertAxis := RowMaxz;
             gr[f1,f2].GraphDraw.MinVertAxis := RowMinz;
             gr[f1,f2].RedrawDiagram11Click(Nil);
          end;
       end;
   end;
   EndGraphGrid(gr,db, BaseFilter,Filters1,Labels1,Filters2,Labels2);
   MultSeries.Destroy;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('procedure GraphTwoParameterGridsByAverageSlopeAndResolution out'); {$EndIf}
end {procedure GraphTwoParameterGridsByAverageSlopeAndResolution};



procedure GraphMultipleParamsByDEMResolution(db : integer; Criterion,DEMIXtileFieldName : shortstring; Resolutions,Comparisons : tStringList);

   function MakeGraphByResolution(db : integer; FigureLabel,TheFigureFilter,ResString,Criterion : shortstring; Comparisons,Resolutions : tStringList) : tThisBaseGraph;
   var
      i,j,k : integer;
      rfile : file;
      SeriesFilter,TStr : shortstring;
      v : array[1..2] of float32;
   begin
      Result := StartIndividualGraph(db,Resolutions);
      Result.GraphDraw.LRcornerText := {'Criterion=' + Criterion + '  ' +} FigureLabel;
      {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile(Result.GraphDraw.LRcornerText); {$EndIf}
      for i := 0 to pred(Comparisons.Count) do begin
         {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile('Comparison=' + Comparisons.Strings[i]); {$EndIf}
         Result.OpenPointSymbolFile(rfile,Comparisons.Strings[i],Result.GraphDraw.Symbol[i]);
         Result.GraphDraw.Symbol[i].DrawingSymbol := FilledBox;
         Result.GraphDraw.Symbol[i].Size := 4;
         Result.GraphDraw.Symbol[i].Color := ConvertTColorToPlatformColor(WinGraphColors(i));
         Result.GraphDraw.FileColors256[i] := ConvertTColorToPlatformColor(WinGraphColors(i));
         if MDDEF.DEMIX_UseMedian then TStr := 'Median ' else TStr := 'Mean ';
         Result.GraphDraw.Vertlabel := TStr + Criterion;

         for k := 0 to pred(Resolutions.Count) do begin
            {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile(IntToStr(succ(k)) + '/' + IntToStr(Resolutions.Count) + ' resolution=' + Resolutions.Strings[k]); {$EndIf}
            SeriesFilter := TheFigureFilter + ' AND ' + ResString + '=' + Resolutions.Strings[k] + ' AND COMPARE=' + QuotedStr(Comparisons.Strings[i]);
            GISdb[db].ApplyGISFilter(SeriesFilter);
            GISdb[db].EmpSource.Enabled := false;
            if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
                v[1] := GISdb[db].MyData.GetFieldByNameAsFloat(ResString);
                if (GISdb[db].MyData.FiltRecsInDB = 1) then begin
                   v[2] := GISdb[db].MyData.GetFieldByNameAsFloat(Criterion);
                end
                else begin
                   if MDDEF.DEMIX_UseMedian then v[2] := GISdb[db].MyData.FieldMedian(Criterion)
                   else v[2] := GISdb[db].MyData.FieldAverage(Criterion);
                end;
                if v[2] < Result.GraphDraw.MinVertAxis then Result.GraphDraw.MinVertAxis := v[2];
                if v[2] > Result.GraphDraw.MaxVertAxis then Result.GraphDraw.MaxVertAxis := v[2];
                BlockWrite(Rfile,v,1);
            end;
            GISdb[db].MyData.Next;
         end;
         CloseFile(rfile);
      end;
      {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile('MakeGraphByResolution, Files completed'); {$EndIf}
       Result.GraphDraw.LLlegend := false;
       Result.GraphDraw.InsideMarginLegend := lpNEMap;
       EndSingleGraph(Result);
   end {function MakeGraphByResolution};


var
   Filters1,Labels1,Filters2,Labels2 : tStringList;
   TheFigureFilter,FigureLabel,TStr,BaseFilter,ResString: shortstring;
   fName : PathStr;
   f1,f2 : integer;
   gr : t2Dgrapharray;
begin {procedure GraphMultipleParamsByDEMResolution}
   if TileCharacteristicsInDB(DB) then begin

       GISdb[db].EmpSource.Enabled := false;
       StartGraphGrid(db,BaseFilter,ResString,Filters1,Labels1,Filters2,Labels2);
       {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphMultipleParamsByDEMResolution in, Criterion=' + Criterion + '  Comparisons=' + IntToStr(Comparisons.Count)); {$EndIf}

       GISdb[db].EmpSource.Enabled := false;
       {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Resolutions=' + IntToStr(Resolutions.Count)); {$EndIf}
       for f1 := 0 to pred(Filters1.Count) do begin
          wmDEM.SetPanelText(1,IntToStr(succ(f1)) + '/' + IntToStr(Filters1.Count),true);
          for f2 := 0 to pred(Filters2.Count) do begin
             TheFigureFilter := PETdbUtils.AddAndIfNeeded(BaseFilter) + Filters1.strings[f1] + ' AND ' + Filters2.strings[f2];
             GISdb[db].ApplyGISFilter(TheFigureFilter);
             GISdb[db].EmpSource.Enabled := false;
             {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('TheFigureFilter=' + TheFigureFilter + '  n=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
             if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
                 FigureLabel := PETdbUtils.AddAndIfNeeded(BaseFilter) + ' tiles=' + IntToStr(GISdb[db].MyData.NumUniqueEntriesInDB(DEMIXtileFieldName));
                 wmDEM.SetPanelText(2,IntToStr(succ(f2)) + '/' + IntToStr(Filters2.Count) + ' ' + TheFigureFilter,true);
                 gr[f1,f2] := MakeGraphByResolution(db,FigureLabel,TheFigureFilter,ResString,Criterion,Comparisons,Resolutions);
             end
             else gr[f1,f2] := Nil;
          end;
       end;
       EndGraphGrid(gr,db, BaseFilter,Filters1,Labels1,Filters2,Labels2);
       {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphMultipleParamsByDEMResolution out'); {$EndIf}
   end;
end {procedure GraphMultipleParamsByDEMResolution};



(*
procedure MakeGraphComparingFUVandArcSecondSpacing(db : integer; Criterion,DEMIXtileFieldName : shortstring);
var
   Comparisons,Resolutions,Filters1,Labels1,Filters2,Labels2,FileList : tStringList;
   TheFigureFilter,FigureLabel,BaseFilter,XField,xLabel : shortstring;
   f1,f2 : integer;
   fName : PathStr;
   MaxVertAxis : float32;


   function MakeGraph : tThisBaseGraph;
   var
      i,j,k : integer;
      rfile : file;
      DEMIX_Tiles : tStringList;
      SeriesFilter,Comparison,Tile,SpacingField: shortstring;
      v : array[1..2] of float32;
   begin
      DEMIX_Tiles := GISdb[db].MyData.ListUniqueEntriesInDB(DEMIXtileFieldName);
      {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('MakeGraph in, DEMIX_Tiles=' + IntToStr(DEMIX_Tiles.Count)); {$EndIf}
      Result := tThisBaseGraph.Create(Application);
      GISdb[db].EmpSource.Enabled := false;
      Result.GraphDraw.LRcornerText := FigureLabel + ' (n=' + IntToStr(GISdb[db].MyData.NumUniqueEntriesInDB(DEMIXtileFieldName)) + ')';
      Result.GraphDraw.VertLabel := Criterion;
      XField := XSpacingField(db,Result.GraphDraw.HorizLabel);

       i := -1;
       for k := 0 to pred(DEMIX_Tiles.Count) do begin
         comparison := Comparisons[0];
         Tile := DEMIX_Tiles.Strings[k];
         SeriesFilter := TheFigureFilter + ' AND COMPARE=' + QuotedStr(Comparison) + ' AND ' + DEMIXtileFieldName + '=' + QuotedStr(tile);
         GISdb[db].EmpSource.Enabled := false;
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
               SeriesFilter := TheFigureFilter + ' AND ' + SpacingField + '=' + Resolutions[j] + ' AND COMPARE=' + QuotedStr(Comparison) + ' AND ' + DEMIXtileFieldName + '=' + QuotedStr(tile);
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
      DEMIX_Tiles.Destroy;
      {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeGraph out'); {$EndIf}
   end;

begin {procedure MakeGraphComparingFUVandArcSecondSpacing};
   SetColorForProcessing;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeGraphComparingFUVandArcSecondSpacing in'); {$EndIf}
   BaseFilter := GISdb[db].MyData.Filter;
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName,Filters1,Labels1);
   ImportLandParamFilters(MDDef.DEMIX_filter2_fName,Filters2,Labels2);
   GISdb[db].EmpSource.Enabled := false;
   Comparisons := GISdb[db].MyData.ListUniqueEntriesInDB('COMPARE');
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Comparisons=' + IntToStr(Comparisons.Count)); {$EndIf}

   XField := XSpacingField(db,xLabel);

   GISdb[db].EmpSource.Enabled := false;
   Resolutions := GISdb[db].MyData.ListUniqueEntriesInDB(XField);
   SortStringListNumerically(Resolutions);
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile(XField + '=' + IntToStr(Resolutions.Count)); {$EndIf}

   MaxVertAxis := GISdb[db].MyData.FindFieldMax(Criterion);
   FileList := tStringList.Create;

   for f1 := 0 to pred(Filters1.Count) do begin
      for f2 := 0 to pred(Filters2.Count) do begin
         FigureLabel := {PETdbUtils.AddAndIfNeeded(BaseFilter) +} Labels1.strings[f1] + ' & ' + Labels2.strings[f2];
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
*)


function GraphCompareInterpolation(db : integer) : tThisBaseGraph;
begin
   Result := CreateMultipleSeriesGraph(db);
end;


function GraphDEMIX_CompareDSMandDTMslopes(db : integer; DEMIX_tile,TileStats : shortstring; Resolutions : tStringList) : tThisBaseGraph;
var
   MultSeries,Comparisons : tStringList;
   BaseFilter,XField,xlabel : shortstring;
   i : integer;
   rfile : file;
   v : array[1..2] of float32;
begin
   {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphDEMIX_CompareDSMandDTMslopes in'); {$EndIf}
    Result := nil;
    XField := XSpacingField(db,xlabel);
    BaseFilter := GISdb[db].MyData.Filter;
    GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'TILE=' + QuotedStr(DEMIX_tile));
    GISdb[db].EmpSource.Enabled := false;
    if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
      Result := StartIndividualGraph(db,Resolutions);
      Result.GraphDraw.LRcornerText := DEMIX_tile;
      Result.GraphDraw.LLcornerText := TileStats;
      Result.GraphDraw.VertLabel := 'Slope difference FUV';
      Comparisons := Nil;
      Comparisons := GISdb[db].MyData.ListUniqueEntriesInDB('COMPARE');
      for i := 0 to pred(Comparisons.Count) do begin
          GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'TILE=' + QuotedStr(DEMIX_tile) + ' AND COMPARE=' + QuotedStr(Comparisons.Strings[i]));
          GISdb[db].EmpSource.Enabled := false;
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
end {function GraphDEMIX_CompareDSMandDTMslopes};


procedure ComputePowerFitForAllTiles(db : integer);
//input is HRDEM DB for grid spacings in meters; uses only those >= 2 m spacing
//makes DB with the statistics for a power law fit for the DSM and DTM
var
   TileList,Results : tStringList;
   DTMName,aline,BaseFilter : shortstring;
   fName : PathStr;
   i : integer;

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
         x[n] := GISdb[db].MyData.GetFieldByNameAsFloat('GRID_M');
         y[n] := GISdb[db].MyData.GetFieldByNameAsFloat(theDEM);
         GISdb[db].MyData.Next;
      end;
      PowerFit(x,y,n,a,b,siga,sigb,r);
      aline := aLine + ',' + RealToString(sqr(r),-12,-4) + ',' + RealToString(a,-12,-4) + ',' + RealToString(b,-12,-4);
   end;


begin {procedure ComputePowerFitForAllTiles}
   if GISdb[db].MyData.FieldExists('GRID_M') and GISdb[db].MyData.FieldExists('DSM_NAME') and GISdb[db].MyData.FieldExists('DTM_NAME') then begin
      {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('ComputePowerFitForAllTiles in'); {$EndIf}
       BaseFilter := GISdb[db].MyData.Filter;
       GISdb[db].EmpSource.Enabled := false;
       TileList := GISdb[db].MyData.ListUniqueEntriesInDB('DTM_NAME');
       Results := tStringList.Create;
       Results.Add('DEMIX_TILE,AREA,LAT,LONG,AVG_SLOPE,BARREN_PC,FOREST_PC,DSM_R2,DSM_A,DSM_B,DTM_R2,DTM_A,DTM_B');
       for I := 0 to pred(TileList.Count) do begin
          DTMname := TileList.Strings[i];
          wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(TileList.Count) + ' ' + DTMname,true);
          aLine := DTMName + ',' + GISdb[db].MyData.GetFieldByNameAsString('AREA') + ',' +
             GISdb[db].MyData.GetFieldByNameAsString('LAT')  + ',' + GISdb[db].MyData.GetFieldByNameAsString('LONG') + ',' +
             GISdb[db].MyData.GetFieldByNameAsString('AVG_SLOPE') + ',' + GISdb[db].MyData.GetFieldByNameAsString('BARREN_PC') + ',' +
             GISdb[db].MyData.GetFieldByNameAsString('FOREST_PC') ;
          GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'DTM_NAME=' + QuotedStr(DTMName) + ' AND GRID_M>=2');
          GISdb[db].EmpSource.Enabled := false;
          OneKindDEM('DSM_SLOPE');
          OneKindDEM('DTM_SLOPE');
          Results.Add(aLine);
       end;
       fName := MDtempDir + 'slope_power_law.dbf';
       StringList2CSVtoDB(Results,fName);
       wmdem.ClearStatusBarPanelText;
       GISdb[db].ApplyGISFilter(BaseFilter);
       {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('ComputePowerFitForAllTiles out'); {$EndIf}
   end
   else begin
      MessageToContinue('Required DSM/DTM fields missing');
   end;
end {procedure ComputePowerFitForAllTiles};


function GraphCompareDSMandDTMslopes(db : integer; DTMName : shortstring) : tThisBaseGraph;
//graph for single DEMIX tile, with grid spacing on x-axis and average slope on y-axis, for DSM and DTM as two series
var
   MultSeries : tStringList;
   BaseFilter,xlabel : shortstring;
begin
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphCompareDSMandDTMslopes in'); {$EndIf}
  Result := nil;
  if GISdb[db].MyData.FieldExists('GRID_THIN') and GISdb[db].MyData.FieldExists('DSM_NAME') and GISdb[db].MyData.FieldExists('DTM_NAME') then begin
      BaseFilter := GISdb[db].MyData.Filter;
      GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'DTM_NAME=' + QuotedStr(DTMName));
      if (GISdb[db].MyData.FiltRecsinDB > 0) then begin
         //if GISdb[db].MyData.FieldExists('GRID_SEC') then GISdb[db].dbOpts.XField := 'GRID_SEC' else GISdb[db].dbOpts.XField := 'GRID_M' ;
          GISdb[db].dbOpts.XField := XSpacingField(db,xLabel);
          MultSeries := tStringList.Create;
          MultSeries.Add('DSM_SLOPE');
          MultSeries.Add('DTM_SLOPE');
          Result := GISdb[db].ActuallyDrawGraph(dbgtN2DgraphMultSeries,MultSeries);
          Result.GraphDraw.MinHorizAxis := 0;
          Result.GraphDraw.MinVertAxis := 0;
          Result.GraphDraw.LeftMargin := 90;
          Result.GraphDraw.BottomMargin := 80;
          Result.GraphDraw.InsideMarginLegend := lpNEMap;
          Result.GraphDraw.LRcornerText := DTMName;
          Result.RedrawDiagram11Click(Nil);
          Result.CurveFit(fitPower);
      end;
      GISdb[db].ApplyGISFilter(BaseFilter);
  end
  else MessageToContinue('Required DSM/DTM fields missing');
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphCompareDSMandDTMslopes out'); {$EndIf}
end;


function OneGraphSlopeVersusResolutionManyTiles(db : integer; Parameter : shortstring) : tThisBaseGraph;
//graph for multiple DEMIX tiles, with grid spacing on x-axis and average slope on y-axis,
var
   MultSeries,TheTiles : tStringList;
   BaseFilter,Tile,XField : shortstring;
   i : integer;
   rfile : file;
   v : array[1..2] of float32;
begin
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphCompareDSMandDTMslopes in'); {$EndIf}
  Result := nil;
  if GISdb[db].MyData.FieldExists('GRID_THIN') and GISdb[db].MyData.FieldExists('DSM_NAME') and GISdb[db].MyData.FieldExists('DTM_NAME') then begin
      BaseFilter := GISdb[db].MyData.Filter;
      TheTiles := GISdb[db].MyData.ListUniqueEntriesInDB('DTM_NAME');
      Result := tThisBaseGraph.Create(Application);
      XField := XSpacingField(db,Result.GraphDraw.HorizLabel);
      GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'GRID_M >= 2');
      GISdb[db].MyData.FindFieldRange(XField,Result.GraphDraw.MinHorizAxis,Result.GraphDraw.MaxHorizAxis);
      GISdb[db].MyData.FindFieldRange(Parameter,Result.GraphDraw.MinVertAxis,Result.GraphDraw.MaxVertAxis);
      for i := 0 to pred(TheTiles.Count) do begin
          Tile := TheTiles.Strings[i];
          GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'DTM_NAME=' + QuotedStr(Tile) + HRDEMresolutionFilter);
          if (GISdb[db].MyData.FiltRecsinDB > 0) then begin
             Result.OpenDataFile(rfile,Tile);
             GISdb[db].MyData.First;
             repeat
                v[1] := GISdb[db].MyData.GetFieldByNameAsFloat(XField);
                v[2] := GISdb[db].MyData.GetFieldByNameAsFloat(Parameter);
                CompareValueToExtremes(v[2],Result.GraphDraw.MinVertAxis,Result.GraphDraw.MaxVertAxis);
                BlockWrite(Rfile,v,1);
                GISdb[db].MyData.Next;
             until GISdb[db].MyData.eof;
             CloseFile(rfile);
          end;
      end;
      Result.GraphDraw.VertLabel := Parameter;
      Result.GraphDraw.LeftMargin := 90;
      Result.GraphDraw.BottomMargin := 80;
      Result.GraphDraw.SetShowAllLines(true);
      Result.GraphDraw.SetShowAllPoints(false);

      //Result.GraphDraw.InsideMarginLegend := lpNEMap;
      Result.GraphDraw.LRcornerText := ' Tiles=' + IntToStr(TheTiles.Count);
      Result.RedrawDiagram11Click(Nil);
     GISdb[db].ApplyGISFilter(BaseFilter);
  end
  else MessageToContinue('Required DSM/DTM fields missing');
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphCompareDSMandDTMslopes out'); {$EndIf}
end {function OneGraphSlopeVersusResolutionManyTiles};



procedure ManyGraphsSlopeVersusResolutionManyTiles(db : integer; Parameter : shortstring);
var
   Filters1,Labels1,Filters2,Labels2 : tStringList;
   f1,f2 : integer;
   gr : t2Dgrapharray;
   BaseFilter,ResString,TheFigureFilter,FigureLabel : shortstring;
begin
   StartGraphGrid(db,BaseFilter,ResString,Filters1,Labels1,Filters2,Labels2);
   GISdb[db].EmpSource.Enabled := false;
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('OneGraphSlopeVersusResolutionManyTiles'); {$EndIf}
   for f1 := 0 to pred(Filters1.Count) do begin
      wmDEM.SetPanelText(1,IntToStr(succ(f1)) + '/' + IntToStr(Filters1.Count),true);
      GISdb[db].EmpSource.Enabled := false;
      for f2 := 0 to pred(Filters2.Count) do begin
         TheFigureFilter := PETdbUtils.AddAndIfNeeded(BaseFilter) + Filters1.strings[f1] + ' AND ' + Filters2.strings[f2];
         GISdb[db].ApplyGISFilter(TheFigureFilter);
         GISdb[db].EmpSource.Enabled := false;
         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
            wmDEM.SetPanelText(2,IntToStr(succ(f2)) + '/' + IntToStr(Filters2.Count) + ' ' + TheFigureFilter,true);
            gr[f1,f2] := OneGraphSlopeVersusResolutionManyTiles(db,Parameter);
         end
         else gr[f1,f2] := Nil;
      end;
   end;
   EndGraphGrid(gr,db, BaseFilter,Filters1,Labels1,Filters2,Labels2);
end;



function GraphDSMandDTMdifferences(db : integer; DTMName,LSP : shortstring): tThisBaseGraph;
//makes graph for single tile, for five statistics for the different distribution of an LSP between the DSM and DTM
var
   MultSeries : tStringList;
   BaseFilter,xlabel : shortstring;

         procedure CheckLSPinDB(LSP : shortstring);
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
      if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
          //if GISdb[db].MyData.FieldExists('GRID_SEC') then GISdb[db].dbOpts.XField := 'GRID_SEC' else GISdb[db].dbOpts.XField := 'GRID_M' ;
          GISdb[db].dbOpts.XField := XSpacingField(db,xlabel);

          MultSeries := tStringList.Create;
          MultSeries := tStringList.Create;
          CheckLSPinDB(LSP + '_LE90');
          CheckLSPinDB(LSP + '_RMSE');
          CheckLSPinDB(LSP + '_MAE');
          CheckLSPinDB(LSP + '_MED');
          CheckLSPinDB(LSP + '_MEAN');
          if (MultSeries.Count = 0) then begin
             MultSeries.Destroy;
             {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphDSMandDTMdifferences no series found'); {$EndIf}
          end
          else begin
              Result := GISdb[db].ActuallyDrawGraph(dbgtN2DgraphMultSeries, MultSeries);
              Result.GraphDraw.HorizLabel := xLabel;
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
  end
  else MessageToContinue('Required fields missing');
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GraphDSMandDTMdifferences out'); {$EndIf}
end {function GraphDSMandDTMdifferences};


procedure MakeSingleAreaDSMDTMcomparison(HRDEM : boolean; DSMName,DTMname,OutName : PathStr; Area,Tile,TileStats : shortstring; Resolutions : tStringList; var Results : tStringList);
//shortens for legends, and avoids duplicates with CopDEM when spacing changes
var
   db,DTM,DSM,DTMthin,DSMthin : integer;
   MultSeries : tStringList;
   CompareName,TStr,CountryStr,TileAverages : shortstring;
   Lat,Long,Lat2,Long2 : float64;

         function ComparePair(DEM1,DEM2,thin : integer) : boolean;
         var
            Grid1,Grid2,slope1,slope2,Dec : integer;
            aline,f1,f2 : shortstring;
            ms1,ms2 : float64;

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

                function ComputePair(DEM1,DEM2 : integer) : boolean;
                var
                   r2 : float64;
                   mv : tMomentVar;
                begin
                   r2 := CorrelationTwoGrids(DEMglb[DEM1].FullDEMgridLimits,DEM1,DEM2);
                   mv := GridDifferenceStatsTwoGrids(DEMglb[DEM1].FullDEMgridLimits,DEM1,DEM2);
                   aLine := aLine + ',' + RealToString(1-r2,-12,-8) + ',' + RealToString(mv.Mean,-8,-4) +  ',' + RealToString(mv.Median,-8,-2) +  ',' + RealToString(mv.Std_Dev,-8,-2) +  ',' +
                       RealToString(mv.MAE,-8,-2) +  ',' + RealToString(mv.RMSE,-8,-2) +  ',' + RealToString(mv.LE90,-8,-2);
                   Result := true;
                end;

         begin  {procedure ComparePair}
            DEMglb[DEM1].DEMCenterPoint(Lat,Long);
            slope1 := CreateEvansSlopeMapPercent(false,DEM1);
            slope2 := CreateEvansSlopeMapPercent(false,DEM2);
            ms1 := GridMean(Slope1);
            ms2 := GridMean(Slope2);
            Result := true;
            if (Thin = 1) and HRDEM then begin
               if (ms1 - ms2) < -0.5 then begin
                  Result := false;
                  CloseSingleDEM(Slope1);
                  CloseSingleDEM(Slope2);
                  exit;
               end;
            end;

            if HRDEM then begin
               CompareName := 'DTM_to_DSM';
               CountryStr := Tile + '_';
            end
            else begin
               CompareName := GeneralizeReferenceName(ExtractFileNameNoExt(DSMname)) + '_to_' + GeneralizeReferenceName(ExtractFileNameNoExt(DTMname));
               CountryStr := '';
            end;

            if HRDEM then begin
               if Thin < 5 then Dec := -1 else Dec := 0;

               aline := CountryStr + ExtractFileNameNoExt(DTMname) + ',' + CountryStr + ExtractFileNameNoExt(DSMname) + ',' + CompareName + ',' +
                    Area + ',' + Copy(CountryStr,1,2) + ',' + IntToStr(Thin) + ',' + RealToString(DEMglb[DEM1].AverageSpace,-8,Dec) + ',' +RealToString(Lat,-12,-6) + ',' +
                    RealToString(Long,-12,-6) + ',' + RealToString(ms1,-12,-2) + ',' + RealToString(ms2,-12,-2) + ',' +  RealToString(ms1-ms2,-12,-2) + ',' + TileStats;
            end
            else begin
               //'DEM_1,DEM_2,COMPARE,AREA,DEMIX_TILE,GRID_THIN,GRID_SEC,LAT,LONG,DEM1_SLOPE,DEM2_SLOPE';
               f1 := GeneralizeReferenceName(ExtractFileNameNoExt(DTMname));
               f2 := GeneralizeReferenceName(ExtractFileNameNoExt(DSMname));
               aline := f1 + ',' + f2 + ',' + CompareName + ',' + Area + ',' + Tile + ',' + IntToStr(Thin) + ',' +
                     RealToString(3600*DEMglb[DEM1].DEMHeader.DEMySpacing,-8,-1) + ',' +
                    RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' + RealToString(ms1,-12,-2) + ',' + RealToString(ms2,-12,-2) + ',' + TileStats;
            end;

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


         function Thinned(Thin : integer) : boolean;
         begin
            DTMthin := DEMglb[DTM].ThinThisDEM(false,'',Thin,true);
            DSMthin := DEMglb[DSM].ThinThisDEM(false,'',Thin,true);
            Result := ComparePair(DSMthin,DTMthin,Thin);
            CloseSingleDEM(DTMthin);
            CloseSingleDEM(DSMthin);
         end;

         function LSPparamsString(LSP : shortstring) : shortstring;
         begin
            Result := ',' + LSP + '_FUV,' + LSP  + '_MEAN,' +  LSP + '_MED,' +  LSP + '_STD,' + LSP + '_MAE,' + LSP + '_RMSE,' + LSP + '_LE90';
         end;
var
   aLine : shortstring;
   Mult : integer;
   DistanceMeters,Bearing : float64;
begin {procedure MakeSingleAreaDSMDTMcomparison}
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('CompareDSMandDTM ' + DSMname + '  ' + DTMName); {$EndIf}
  if not (FileExists(DTMName) and FileExists(DSMname)) then begin
     HighlightLineToDebugFile('Missing file: ' + DSMname + '  ' + DTMName);
     exit;
  end;
  SetColorForProcessing;
  DTM := OpenNewDEM(DTMName,false);
  DSM := OpenNewDEM(DSMName,false);
  DEMglb[DTM].DEMCenterPoint(Lat,Long);
  DEMglb[DSM].DEMCenterPoint(Lat2,Long2);
  VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,DistanceMeters,Bearing);
  if (DistanceMeters > 1000) then begin
     {$IfDef RecordCompareDSMandDTM} HighlightLineToDebugFile(RealToString(0.001 * DistanceMeters,-8,-2) + 'km apart  ' + DSMname + '  ' + DTMName); {$EndIf}
     //this should not be happening, but still trying to figure out why it does
  end
  else begin
      if (Results = Nil) then begin
         Results := tStringList.Create;
         TStr :=  LSPparamsString('ELVD') + LSPparamsString('SLPD');  //+ LSPparamsString('RRID');
         TileAverages := ',AVG_SLOPE,BARREN_PC,FOREST_PC';
         if HRDEM then begin
            aLine := 'DTM_NAME,DSM_NAME,COMPARE,AREA,COUNTRY,GRID_THIN,GRID_M,LAT,LONG,DSM_SLOPE,DTM_SLOPE,DIFF_SLOPE';
         end
         else begin
            aLine := 'DEM_1,DEM_2,COMPARE,AREA,DEMIX_TILE,GRID_THIN,GRID_SEC,LAT,LONG,DEM1_SLOPE,DEM2_SLOPE';
         end;
         Results.Add(aLine + TileAverages + TStr);
      end;
      if ComparePair(DSM,DTM,1) then begin
        if HRDEM then begin  //HRDEM comparison in meters
           if (DEMglb[DTM].AverageSpace < 0.75) then begin //0.5 m HRDEM
              Mult := 4;
              Thinned(2);
              Thinned(4);
           end
           else if (DEMglb[DTM].AverageSpace < 1.25) then begin //1 m HRDEM
              Mult := 2;
              Thinned(2);
           end
           else Mult := 1;
           Thinned(2*Mult);
           Thinned(3*Mult);
           Thinned(4*Mult);
           Thinned(5*Mult);
           Thinned(8*Mult);
           Thinned(10*Mult);
           Thinned(12*Mult);
           Thinned(15*Mult);
           Thinned(20*Mult);
           Thinned(25*Mult);
           Thinned(30*Mult);
         end
         else begin    //arc second GDEM comparison
            Thinned(1);
            Thinned(2);
            Thinned(3);
            Thinned(4);
            Thinned(5);
            Thinned(6);
            Thinned(8);
            Thinned(10);
            Thinned(12);
         end;
      end;
      if (OutName <> '') then db := StringList2CSVtoDB(Results,OutName);
  end;
  CloseSingleDEM(DTM);
  CloseSingleDEM(DSM);
end {procedure MakeSingleAreaDSMDTMcomparison};






