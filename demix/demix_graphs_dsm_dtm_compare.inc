{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{     include file for demix_graphs      }
{________________________________________}



procedure EndGraphGrid(gr : t2Dgrapharray; db : integer; BaseFilter : shortstring; var Filters1,Labels1,Filters2,Labels2 : tStringList);
const
   StartX = 75;
   StartY = 75;
var
   BigBitmap,Bitmap : tMyBitmap;
   xsize,ysize,f1,f2,XPanel,YPanel,Offset : integer;
begin
   xsize := 0;
   ySize := 0;
   for f1 := 0 to pred(Filters1.Count) do begin
      for f2 := 0 to pred(Filters2.Count) do begin
         if gr[f1,f2] <> Nil then begin
            if (gr[f1,f2].GraphDraw.XWindowSize > xsize) then xsize := gr[f1,f2].GraphDraw.XWindowSize;
            if (gr[f1,f2].GraphDraw.YWindowSize > ysize) then Ysize := gr[f1,f2].GraphDraw.YWindowSize;
         end;
      end;
   end;

   XPanel := XSize + 20;
   YPanel := YSize + 20;

   CreateBitmap(BigBitmap,StartX + Filters2.Count * XPanel, StartY + Filters1.Count * YPanel);
   BigBitmap.Canvas.Font.Size := 24;
   BigBitmap.Canvas.Font.Style := [fsBold];
   for f1 := 0 to pred(Filters1.Count) do begin
      Offset := (YPanel - BigBitmap.Canvas.TextWidth(Labels1.Strings[f1])) div 2;
      TextOutVertical(BigBitmap.Canvas,2,StartY + succ(F1) * YPanel - Offset,Labels1.Strings[f1]);
   end;

   for f2 := 0 to pred(Filters2.Count) do begin
      Offset := (XPanel - BigBitmap.Canvas.TextWidth(Labels1.Strings[f2])) div 2;
      BigBitmap.Canvas.TextOut(StartX + f2 * XPanel + Offset,2,Labels2.Strings[f2]);
   end;

   for f1 := 0 to pred(Filters1.Count) do begin
      for f2 := 0 to pred(Filters2.Count) do begin
         if (gr[f1,f2] <> Nil) and CopyImageToBitmap(gr[f1,f2].Image1,Bitmap) then begin
            BigBitMap.Canvas.Draw(StartX + f2 * XPanel, StartY + F1 * YPanel,Bitmap);
            Bitmap.Free;
         end;
      end;
   end;
   GetImagePartOfBitmap(BigBitmap);
   DisplayBitmap(BigBitmap,'graph_grid');
   Filters1.Destroy;
   Labels1.Destroy;
   Filters2.Destroy;
   Labels2.Destroy;

   GISdb[db].ApplyGISFilter(BaseFilter);
   SetColorForWaiting;
   wmdem.ClearStatusBarPanelText;
end;


procedure StartGraphGrid(db : integer; var BaseFilter,ResString : shortstring; var Resolutions,Filters1,Labels1,Filters2,Labels2 : tStringList);
begin
   SetColorForProcessing;
   BaseFilter := GISdb[db].MyData.Filter;
   ImportLandParamFilters(MDDef.DEMIX_filter1_fName,Filters1,Labels1);
   ImportLandParamFilters(MDDef.DEMIX_filter2_fName,Filters2,Labels2);
   if GISdb[db].MyData.FieldExists('GRID_SEC') then ResString := 'GRID_SEC' else ResString := 'GRID_M';
   GISdb[db].dbOpts.XField := ResString;
   Resolutions := GISdb[db].MyData.ListUniqueEntriesInDB(ResString);
   SortStringListNumerically(Resolutions);
end;


function StartIndividualGraph(db : integer) : tThisBaseGraph;
var
   TStr : shortstring;
begin
   Result := tThisBaseGraph.Create(Application);
   if (GISdb[db].dbOpts.XField = 'GRID_SEC') then begin
      TStr := 'arc second';
      Result.GraphDraw.MaxHorizAxis := GISdb[db].MyData.FindFieldMax('GRID_SEC') + 2;
   end
   else begin
      TStr := 'meters';
      Result.GraphDraw.MaxHorizAxis := GISdb[db].MyData.FindFieldMax('GRID_M') + 5;
   end;
   Result.GraphDraw.HorizLabel := 'Grid spacing (' + TStr + ')';

   Result.GraphDraw.MaxVertAxis := -99e38;
   Result.GraphDraw.MinVertAxis :=  99e38;
end;


procedure EndSingleGraph(var Graph : tThisBaseGraph);
var
   VertRange : float64;
begin
   VertRange := Graph.GraphDraw.MaxVertAxis - Graph.GraphDraw.MinVertAxis;
   Graph.GraphDraw.MinVertAxis := Graph.GraphDraw.MinVertAxis - 0.05 * Graph.GraphDraw.MinVertAxis;
   Graph.GraphDraw.MaxVertAxis := Graph.GraphDraw.MaxVertAxis - 0.05 * Graph.GraphDraw.MaxVertAxis;

   Graph.GraphDraw.LeftMargin := 100;
   Graph.GraphDraw.BottomMargin := 90;
   Graph.GraphDraw.SetShowAllPoints(false);
   Graph.GraphDraw.SetShowAllLines(true,4);
   Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
   Graph.DefaultClientWidth := MDDef.DEMIX_xsize;
   Graph.DefaultClientHeight := MDDef.DEMIX_ysize;
   Graph.GraphDraw.ForceNewSize := true;
   Graph.FormResize(Nil);
end;

procedure GraphTwoParameterGridsByAverageSlopeAndResolution(db : integer; DEMIXtileFieldName : shortstring; MultSeries : tStringList; AllTiles : boolean = false);
//
var
   Resolutions,Filters1,Labels1,Filters2,Labels2 : tStringList;
   TheFigureFilter,FigureLabel,TStr,BaseFilter,ResString: shortstring;
   fName : PathStr;
   f1,f2 : integer;
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
   begin
      Result := StartIndividualGraph(db);
      (*
      Result := tThisBaseGraph.Create(Application);
         if (GISdb[db].dbOpts.XField = 'GRID_SEC') then begin
            TStr := 'arc second';
            Result.GraphDraw.MaxHorizAxis := GISdb[db].MyData.FindFieldMax(ResString) + 2;
         end
         else begin
            TStr := 'meters';
            Result.GraphDraw.MaxHorizAxis := GISdb[db].MyData.FindFieldMax(ResString) + 5;
         end;
         Result.GraphDraw.HorizLabel := 'Grid spacing (' + TStr + ')';

         Result.GraphDraw.MaxVertAxis := -99e38;
         Result.GraphDraw.MinVertAxis :=  99e38;
      *)
         (*
         for k := 0 to pred(MultSeries.Count) do begin
            GISdb[db].MyData.FindFieldRange(MultSeries.Strings[k],aMin,aMax);
            if amin < Result.GraphDraw.MinVertAxis then Result.GraphDraw.MinVertAxis := amin;
            if amax > Result.GraphDraw.MaxVertAxis then Result.GraphDraw.MaxVertAxis := amax;
         end;
         *)

         if MDDEF.DEMIX_UseMedian then TStr := 'Median' else TStr := 'Mean';
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
             GISdb[db].EmpSource.Enabled := false;
             GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'DTM_NAME=' + QuotedStr(MultTiles.Strings[k]));
             OnFile := Result.OpenPointSymbolFile(rfile,MultTiles.Strings[k],Symbol,4);
             GISdb[db].MyData.First;
             repeat
                if GISdb[db].GetFloat32FromTableLinkPossible(GISdb[db].dbOpts.XField,v[1]) and GISdb[db].GetFloat32FromTableLinkPossible(MultSeries.Strings[0],v[2]) then begin
                   if v[2] < Result.GraphDraw.MinVertAxis then Result.GraphDraw.MinVertAxis := v[2];
                   if v[2] > Result.GraphDraw.MaxVertAxis then Result.GraphDraw.MaxVertAxis := v[2];
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
               SeriesFilter := TheFigureFilter + ' AND ' + ResString + '=' + Resolutions.Strings[k];
               GISdb[db].ApplyGISFilter(SeriesFilter);
               if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
                   GISdb[db].EmpSource.Enabled := false;
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
             CloseFile(rfile);
         end;
       end;

        {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('MakeGraph, data files created '+ Result.GraphDraw.AxisRange); {$EndIf}

       //Result.GraphDraw.LLlegend := false;
       EndSingleGraph(result);
       (*
       Result.GraphDraw.SetShowAllPoints(false);
       Result.GraphDraw.SetShowAllLines(true,4);
       Result.AutoScaleAndRedrawDiagram(false,false,false);
       Result.DefaultClientWidth := MDDef.DEMIX_xsize;
       Result.DefaultClientHeight := MDDef.DEMIX_ysize;
       Result.GraphDraw.ForceNewSize := true;
       Result.FormResize(Nil);
       *)
   end {function MakeGraph};



begin {procedure GraphTwoParameterGridsByAverageSlopeAndResolution}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphTwoParameterGridsByAverageSlopeAndResolution'); {$EndIf}
   StartGraphGrid(db,BaseFilter,ResString,Resolutions,Filters1,Labels1,Filters2,Labels2);
   GISdb[db].EmpSource.Enabled := false;
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Resolutions=' + IntToStr(Resolutions.Count)); {$EndIf}
   for f1 := 0 to pred(Filters1.Count) do begin
      wmDEM.SetPanelText(1,IntToStr(succ(f1)) + '/' + IntToStr(Filters1.Count),true);
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
         end
         else gr[f1,f2] := Nil;
      end;
   end;
   EndGraphGrid(gr,db, BaseFilter,Filters1,Labels1,Filters2,Labels2);
   MultSeries.Destroy;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphTwoParameterGridsByDEMresolution out'); {$EndIf}
end {procedure GraphTwoParameterGridsByAverageSlopeAndResolution};



procedure GraphTwoParameterGridsByDEMresolution(db : integer; Criterion,DEMIXtileFieldName : shortstring; Comparisons : tStringList);
var
   Resolutions,Filters1,Labels1,Filters2,Labels2 : tStringList;
   TheFigureFilter,FigureLabel,TStr,BaseFilter,ResString: shortstring;
   fName : PathStr;
   f1,f2 : integer;
   gr : t2Dgrapharray;


   function MakeGraph : tThisBaseGraph;
   var
      i,j,k : integer;
      rfile : file;
      SeriesFilter,TStr : shortstring;
      v : array[1..2] of float32;
   begin
      Result := StartIndividualGraph(db);
(*
      Result := tThisBaseGraph.Create(Application);
      GISdb[db].EmpSource.Enabled := false;

      if MDDEF.DEMIX_UseMedian then TStr := 'Median' else TStr := 'Mean';
      Result.GraphDraw.VertLabel := TStr + ' ' + Criterion;

       if (GISdb[db].dbOpts.XField = 'GRID_SEC') then begin
          TStr := 'arc second';
          Result.GraphDraw.MaxHorizAxis := GISdb[db].MyData.FindFieldMax(ResString) + 2;
       end
       else begin
          TStr := 'meters';
          Result.GraphDraw.MaxHorizAxis := GISdb[db].MyData.FindFieldMax(ResString) + 5;
       end;
       Result.GraphDraw.HorizLabel := 'Grid spacing (' + TStr + ')';

         Result.GraphDraw.MaxVertAxis := -99e38;
         Result.GraphDraw.MinVertAxis :=  99e38;
*)

      Result.GraphDraw.LRcornerText := FigureLabel;
      {$IfDef RecordDSM_DTM_CompareFull} WriteLineToDebugFile(Result.GraphDraw.LRcornerText); {$EndIf}
      for i := 0 to pred(Comparisons.Count) do begin
         Result.OpenPointSymbolFile(rfile,Comparisons.Strings[i],Result.GraphDraw.Symbol[i]);
         Result.GraphDraw.Symbol[i].DrawingSymbol := FilledBox;
         Result.GraphDraw.Symbol[i].Size := 4;
         Result.GraphDraw.Symbol[i].Color := ConvertTColorToPlatformColor(WinGraphColors(i));
         //Result.GraphDraw.LineSize256[i] := 3;
         Result.GraphDraw.FileColors256[i] := ConvertTColorToPlatformColor(WinGraphColors(i));
         //Result.GraphDraw.ShowLine[i] := true;
         //Result.GraphDraw.ShowPoints[i] := false;

         for k := 0 to pred(Resolutions.Count) do begin
            SeriesFilter := TheFigureFilter + ' AND ' + ResString + '=' + Resolutions.Strings[k] + ' AND COMPARE=' + QuotedStr(Comparisons.Strings[i]);
            GISdb[db].ApplyGISFilter(SeriesFilter);
            GISdb[db].EmpSource.Enabled := false;

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
         end;
         CloseFile(rfile);
      end;
       //Result.GraphDraw.LeftMargin := 100;
       //Result.GraphDraw.BottomMargin := 90;
       Result.GraphDraw.LLlegend := false;
       Result.GraphDraw.InsideMarginLegend := lpNEMap;
       EndSingleGraph(Result);
       (*
       Result.AutoScaleAndRedrawDiagram(false,false,false,false);
       Result.DefaultClientWidth := MDDef.DEMIX_xsize;
       Result.DefaultClientHeight := MDDef.DEMIX_ysize;
       Result.GraphDraw.ForceNewSize := true;
       Result.FormResize(Nil);
       *)
   end;


begin {procedure GraphTwoParameterGridsByDEMresolution}
   StartGraphGrid(db,BaseFilter,ResString,Resolutions,Filters1,Labels1,Filters2,Labels2);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GraphTwoParameterGridsByDEMresolution in, Criterion=' + Criterion + '  Comparisons=' + IntToStr(Comparisons.Count)); {$EndIf}

   GISdb[db].EmpSource.Enabled := false;
   {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Resolutions=' + IntToStr(Resolutions.Count)); {$EndIf}
   for f1 := 0 to pred(Filters1.Count) do begin
      wmDEM.SetPanelText(1,IntToStr(succ(f1)) + '/' + IntToStr(Filters1.Count),true);
      for f2 := 0 to pred(Filters2.Count) do begin
         TheFigureFilter := PETdbUtils.AddAndIfNeeded(BaseFilter) + Filters1.strings[f1] + ' AND ' + Filters2.strings[f2];
         GISdb[db].ApplyGISFilter(TheFigureFilter);
         GISdb[db].EmpSource.Enabled := false;
         {$IfDef RecordDSM_DTM_Compare} WriteLineToDebugFile('Filter=' + TheFigureFilter + '  n=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
            FigureLabel := PETdbUtils.AddAndIfNeeded(BaseFilter) + ' n=' + IntToStr(GISdb[db].MyData.NumUniqueEntriesInDB(DEMIXtileFieldName));
            wmDEM.SetPanelText(2,IntToStr(succ(f2)) + '/' + IntToStr(Filters2.Count) + ' ' + TheFigureFilter,true);
            gr[f1,f2] := MakeGraph;
         end
         else gr[f1,f2] := Nil;
      end;
   end;
   EndGraphGrid(gr,db, BaseFilter,Filters1,Labels1,Filters2,Labels2);
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
      Result := StartIndividualGraph(db);

      (*
      Result := tThisBaseGraph.Create(Application);
      if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
         XField := 'GRID_SEC';
         Result.GraphDraw.HorizLabel := 'Grid spacing (arc second)';
      end
      else begin
         XField := 'GRID_M';
         Result.GraphDraw.HorizLabel := 'Average Grid spacing (m)';
      end;
      Result.GraphDraw.SetShowAllLines(true,3);
      Result.GraphDraw.SetShowAllPoints(false);
      *)

      Result.GraphDraw.LRcornerText := DEMIX_tile;
      Result.GraphDraw.LLcornerText := TileStats;
      Result.GraphDraw.VertLabel := 'Slope difference FUV';


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


procedure MakeSingleAreaDSMDTMcomparison(HRDEM : boolean; DSMName,DTMname,OutName : PathStr; Area,Tile,TileStats : shortstring; var Results : tStringList);
//shortens for legends, and avoids duplicates with CopDEM when spacing changes
var
   db,DTM,DSM,DTMthin,DSMthin : integer;
   MultSeries : tStringList;
   CompareName,TStr,CountryStr,TileAverages : shortstring;

         function ComparePair(DEM1,DEM2,thin : integer) : boolean;
         var
            Grid1,Grid2,
            slope1,slope2 : integer;
            aline,f1,f2 : shortstring;
            Lat,Long,ms1,ms2 : float64;

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
            ms1 := GridMean(Slope1);
            ms2 := GridMean(Slope2);
            Result := true;
            if (Thin = 1) and HRDEM then begin
               if (ms1 - ms2) < - 0.5 then begin
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
               aline := CountryStr + ExtractFileNameNoExt(DTMname) + ',' + CountryStr + ExtractFileNameNoExt(DSMname) + ',' + CompareName + ',' +
                    Area + ',' + Copy(CountryStr,1,2) + ',' + IntToStr(Thin) + ',' + RealToString(DEMglb[DEM1].AverageSpace,-8,-1) + ',' +RealToString(Lat,-12,-6) + ',' +
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


         procedure Thinned(Thin : integer);
         begin
            DTMthin := DEMglb[DTM].ThinThisDEM(false,'',Thin,true);
            DSMthin := DEMglb[DSM].ThinThisDEM(false,'',Thin,true);
            ComparePair(DSMthin,DTMthin,Thin);
            CloseSingleDEM(DTMthin);
            CloseSingleDEM(DSMthin);
         end;

         function LSPparamsString(LSP : shortstring) : shortstring;
         begin
            Result := ',' + LSP + '_FUV,' + LSP  + '_MEAN,' +  LSP + '_MED,' +  LSP + '_STD,' + LSP + '_MAE,' + LSP + '_RMSE,' + LSP + '_LE90';
         end;
var
   aLine : shortstring;
begin {procedure CompareDSMandDTM}
  {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('CompareDSMandDTM ' + DSMname + '  ' + DTMName); {$EndIf}

  SetColorForProcessing;

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
  DTM := OpenNewDEM(DTMName,false);
  DSM := OpenNewDEM(DSMName,false);
  if ComparePair(DSM,DTM,1) then begin

    if HRDEM then begin  //HRDEM comparison in meters
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
     else begin    //arc second GDEM comparison
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
end {procedure CompareDSMandDTM};



procedure MakeGraphComparingFUVandArcSecondSpacing(db : integer; Criterion,DEMIXtileFieldName : shortstring);
var
   Comparisons,Resolutions,Filters1,Labels1,Filters2,Labels2,FileList : tStringList;
   TheFigureFilter,FigureLabel,BaseFilter,XField : shortstring;
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
      if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
         Result.GraphDraw.HorizLabel := 'Grid spacing (arc second)';
         SpacingField := 'GRID_SEC';
      end
      else begin
         Result.GraphDraw.HorizLabel := 'Grid spacing (meters)';
         SpacingField := 'GRID_M';
     end;

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

    if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
       XField := 'GRID_SEC';
    end
    else begin
       XField := 'GRID_M';
   end;

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



