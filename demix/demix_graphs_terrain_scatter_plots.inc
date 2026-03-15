{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{     include file for demix_graphs      }
{________________________________________}


procedure SetScatterPlotParameters(db : integer; var Graph : tThisBaseGraph);
begin
    GISdb[db].dbOpts.XField := 'BARREN_PC';
    GISdb[db].dbOpts.YField := 'AVG_SLOPE';
    Graph.MinZ := 99e38;
    Graph.MaxZ := -99e38;
    Graph.GraphDraw.MinVertAxis := -2;
    Graph.GraphDraw.MaxVertAxis := 90;
    Graph.GraphDraw.MinHorizAxis := -2;
    Graph.GraphDraw.MaxHorizAxis := 70;
    Graph.GraphDraw.HorizLabel := GISdb[db].dbOpts.XField;
    Graph.GraphDraw.VertLabel := GISdb[db].dbOpts.YField;
    Graph.GraphDraw.BottomMargin := 75;
    Graph.GraphDraw.LeftMargin := 90;
    Graph.ZUnits := GISdb[DB].dbOpts.NumericColorField;
    GISdb[DB].EmpSource.Enabled := false;
end;

procedure FinishScatterPlotParameters(db : integer; var Graph : tThisBaseGraph);
begin
   Graph.DefaultClientWidth := MDDef.DEMIX_xsize;
   Graph.DefaultClientHeight := MDDef.DEMIX_ysize;
   Graph.GraphDraw.ForceNewSize := true;
   Graph.FormResize(Nil);
end;


procedure BestDEMonScatterPlotTwoParameters(db : integer; Criteria,DEMs : tstringList);
var
   Compares,FileList : tStringList;
   i : integer;
   aDEM,Criterion,BaseFilter : shortstring;
   fName,LegfName : PathStr;
   gr : tThisBaseGraph;
   Tolerance : float32;
   bmp : tMyBitmap;

   function DoGraph : tThisBaseGraph;
   const
      MaxDEMs = 10;
   var
      j,k,NumTies,NumSolo : integer;
      rfile : array[0..MaxDEMs ] of file;
      v : array[1..3] of float32;
      ThisWinners : shortstring;
      NumWins : array[0..MaxDEMs ] of integer;
      Sym : tFullSymbolDeclaration;
   begin
      for j := 0 to MaxDEMs do NumWins[j] := 0;
       GISdb[db].EmpSource.Enabled := false;
       GISdb[db].MyData.First;
       repeat
          ThisWinners := WinnerAndTies(db,DEMs,Tolerance);
          if MDdef.DEMIX_IgnoreTies and StrUtils.AnsiContainsText(ThisWinners,'-') then begin
          end
          else begin
              for k := 0 to pred(DEMs.Count) do begin
                 aDEM := UpperCase(DEMs.Strings[k]);
                 if StrUtils.AnsiContainsText(ThisWinners,aDEM) then begin
                    inc(NumWins[k]);
                 end;
              end;
          end;
          GISdb[db].MyData.Next;
       until GISdb[db].MyData.eof;

      Result := TThisbasegraph.Create(Application);
      SetScatterPlotParameters(db,Result);

       for k := 0 to pred(DEMs.Count) do begin
          fName := UpperCase(DEMs.Strings[k]) + '_(n=' + IntToStr(NumWins[k]) + ')';
          Result.OpenDataFile(rfile[k],fName);
          Result.GraphDraw.ShowPoints[k] := true;
          Result.GraphDraw.ShowLine[k] := false;
          Result.GraphDraw.Symbol[k].DrawingSymbol := FilledBox;
          Result.GraphDraw.Symbol[k].Size := 5;
          Result.GraphDraw.Symbol[k].Color := DEMIXColorFromDEMName(DEMs.Strings[k]);
       end;
       //GISdb[db].EmpSource.Enabled := false;
       GISdb[db].MyData.First;
       NumTies := 0;
       NumSolo := 0;
       GISdb[db].MyData.First;
       repeat
          if GISdb[db].GetFloat32FromTableLink(GISdb[db].dbOpts.XField,v[1]) and
              GISdb[db].GetFloat32FromTableLink(GISdb[db].dbOpts.YField,v[2]) then begin
            ThisWinners := WinnerAndTies(db,DEMs,Tolerance);
            if MDdef.DEMIX_IgnoreTies and StrUtils.AnsiContainsText(ThisWinners,'-') then begin
            end
            else begin
                for k := pred(DEMs.Count) downto 0 do begin
                   aDEM := UpperCase(DEMs.Strings[k]);
                   if StrUtils.AnsiContainsText(ThisWinners,aDEM) then begin
                      BlockWrite(Rfile[k],v,1);
                   end;
                end;
            end;
          end;
          GISdb[db].MyData.Next;
       until GISdb[db].MyData.eof;
       for k := 0 to pred(DEMs.Count) do CloseFile(rfile[k]);
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile(Criterion + ' ties=' + IntToStr(NumTies) + ' solo=' + IntToStr(NumSolo)); {$EndIf}
      Result.GraphDraw.LRcornerText := GISdb[db].MyData.Filter + '  n=' + IntToStr(GISdb[db].MyData.FiltRecsInDB);
      Result.GraphDraw.InsideMarginLegend := lpNWMap;
      FinishScatterPlotParameters(db,Result);
   end;

begin  {procedure BestDEMonScatterPlotTwoParameter}
   if TileCharacteristicsInDB(DB,true) then begin
      BaseFilter := GISdb[db].MyData.Filter;
      for I := 0 to pred(Criteria.Count) do begin
         Criterion := Criteria.Strings[i];
         GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'Criterion=' + QuotedStr(Criterion));
         Tolerance := CriterionTieTolerance(Criterion);
         GISdb[db].dbOpts.Symbol.Size := 8;
         gr := DoGraph;
      end;
     //MakeBigBitmap(FileList,'',fName,2);
     GISdb[db].ApplyGISFilter(BaseFilter);
     GISdb[db].ShowStatus;
  end;
end {procedure BestDEMonScatterPlotTwoParameter};


procedure GridOfTerrainScatterPlots(DB : integer; ColorBy,Compares,DEMs : tStringList);
//ColorBy has the criteria to color the points for each test tile in the scatter plot
//Compares has the 2 DEM pairs used to create graphs
var
   FileList : tStringList;
   i,j,Ncols : integer;
   fName : PathStr;
   BaseFilter,SpaceFilter : shortstring;
   gr : t2Dgrapharray;
   RowMinZ,RowMaxZ : float64;


   function CreateGraph(LRText : shortstring) : tThisBaseGraph;
   var
      rfile : file;
      v : array[1..3] of float32;
   begin
        Result := tThisBaseGraph.Create(Application);
        SetScatterPlotParameters(db,Result);
        Result.GraphDraw.RainBowColors := true;
        Result.OpenXYZFile(rfile);
        GISdb[DB].MyData.First;
        while not GISdb[DB].MyData.eof  do begin
            if GISdb[DB].GetFloat32FromTableLink(GISdb[DB].dbOpts.XField,v[1]) and
               GISdb[DB].GetFloat32FromTableLink(GISdb[DB].dbOpts.YField,v[2]) and
               GISdb[DB].GetFloat32FromTableLink(GISdb[DB].dbOpts.NumericColorField,v[3]) then begin
                  BlockWrite(Rfile,v,1);
                  CompareValueToExtremes(v[3],Result.MinZ, Result.MaxZ);
            end
            else begin
                {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('Missing point ');  {$EndIf}
            end;
            GISdb[DB].MyData.Next;
        end;
        closeFile(rFile);
        Result.MinZ := RowMinZ;
        Result.MaxZ := RowMaxZ;
        Result.MinZShow := Result.MinZ;

        {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('coloring from '+ RealToString(Result.Minz,-12,-2) + ' to ' + RealToString(Result.Maxz,-12,-2) ); {$EndIf}
        Result.GraphDraw.Symbol[0].Size := 5;
        Result.GraphDraw.LRcornerText := LRText;
        Result.GraphDraw.InsideMarginLegend := lpNEMap;
  (*
        if StrUtils.AnsiContainsText(GISdb[DB].dbOpts.NumericColorField,'_FUV') then begin
           Result.MinZ := 0;
           Result.MaxZ := 0.5;
        end;
  *)
        FinishScatterPlotParameters(db,Result);
        fName := NextFileNumber(MDtempdir,'fuv_grid_','.bmp');
        SaveImageAsBMP(Result.Image1,fName);
        FileList.Add(fName);
   end;

begin {procedure GridOfTerrainScatterPlots}
   {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('GridOfCriterionForMultipleDEMcomparisons in'); {$EndIf}
   BaseFilter := GISdb[DB].MyData.Filter;
   if GISdb[DB].MyData.FieldExists('GRID_SEC') then SpaceFilter := 'GRID_SEC=1'
   else if GISdb[DB].MyData.FieldExists('GRID_M') then SpaceFilter := 'GRID_M=30'
   else begin
       SpaceFilter := '';
   end;
   FileList := tStringList.Create;
   for j := 0 to pred(ColorBy.Count) do begin
       {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('color=' + GISdb[DB].dbOpts.NumericColorField); {$EndIf}
       if (DEMs=Nil) then begin
         GISdb[DB].dbOpts.NumericColorField := ColorBy.Strings[j];
         if (Compares = Nil) then begin
            gr[j,0] := CreateGraph('n=' + IntToStr(GISdb[DB].MyData.FiltRecsInDB));   //ColorBy.Strings[i]);
         end
         else begin
             for I := 0 to pred(Compares.Count) do begin
                GISdb[DB].ApplyGISFilter(AddAndIfNeeded(SpaceFilter) + 'COMPARE=' + QuotedStr(Compares.Strings[i]));
                {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('filter=' + GISdb[DB].MyData.Filter); {$EndIf}
                gr[j,i] := CreateGraph(GISdb[DB].MyData.Filter +  ' (n=' + IntToStr(GISdb[DB].MyData.FiltRecsInDB) + ')');
             end;
         end;
         Ncols := Compares.Count;
       end
       else begin
           GISdb[DB].ApplyGISFilter('CRITERION=' + QuotedStr(ColorBy.Strings[j]));
           GISdb[DB].EmpSource.Enabled := false;
           GISdb[DB].MyData.FindMultipleFieldsRange(DEMs,RowMinZ,RowMaxZ);

           for I := 0 to pred(DEMs.Count) do begin
              GISdb[DB].dbOpts.NumericColorField := DEMs.Strings[i];
              {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('filter=' + GISdb[DB].MyData.Filter); {$EndIf}
              gr[j,i] := CreateGraph(GISdb[DB].MyData.Filter + '  (n=' + IntToStr(GISdb[DB].MyData.FiltRecsInDB) + ')');
           end;
           Ncols := DEMs.Count;
       end;
    end;
    fName := NextFileNumber(MDTempDir,'grid_terrain_scatter_','.bmp');
    MakeBigBitmap(FileList,'',fName,Ncols);
    GISdb[DB].ShowStatus;
end {procedure GridOfTerrainScatterPlots};



