{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{ include file for demix_definitions }
{____________________________________}


procedure ClusterCompositionByDBfield(DBonTable : integer);
var
   i,j : integer;
   WantedField,aLine : ShortString;
   UniqueEntries1,UniqueEntries2,Results : tStringList;
   fName : PathStr;
begin
   {$IfDef RecordClustering} WriteLineToDebugFile('Tdbtablef.ClusterComposition1Click'); {$EndIf}
   if GISdb[DBonTable].MyData.FieldExists('CLUSTER') then begin
      if GISdb[DBonTable].MyData.Filtered then MessageToContinue('Results apply to entire database');
      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].EmpSource.Enabled := false;
      UniqueEntries1 := GISdb[DBonTable].MyData.UniqueEntriesInDB('CLUSTER');
      SortStringListNumerically(UniqueEntries1);
      GISdb[DBonTable].EmpSource.Enabled := false;
      WantedField := GISdb[DBonTable].PickField('cluster membership statistics' ,[ftString],true);
      UniqueEntries2 := GISdb[DBonTable].MyData.UniqueEntriesInDB(WantedField);

      Results := tStringList.Create;
      aLine := 'CLUSTER';
      for I := 0 to pred(UniqueEntries2.Count) do aLine := aLine + ',' + UniqueEntries2.Strings[i];
      Results.Add(aline);

      for j := 0 to pred(UniqueEntries1.Count) do begin
         aLine := 'Cluster ' + UniqueEntries1.Strings[j];
         for I := 0 to pred(UniqueEntries2.Count) do begin
            GISdb[DBonTable].ApplyGISFilter('CLUSTER=' + UniqueEntries1.Strings[j] + ' AND ' + WantedField + '=' + QuotedStr(UniqueEntries2.Strings[i]));
            aLine := aLine + ',' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB);
         end;
         Results.Add(aline);
      end;

      fName := Petmar.NextFileNumber(MDTempDir, 'Cluster_freq_',DefaultDBExt);
      {$IfDef RecordClustering} WriteLineToDebugFile('Created: ' + fName); {$EndIf}
      StringList2CSVtoDB(Results,fName);
   end;
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].dbTablef.UnHideColumns;
   GISdb[DBonTable].ShowStatus;
end;



procedure ClusterFrequencyForSelectedField(DBonTable : integer);
var
   MinX,MaxX : float64;
   i,j : integer;
   WantedField : ShortString;
   UniqueEntries,Results : tStringList;
   TStr,f1 : AnsiString;
   fName : PathStr;
begin
   {$IfDef RecordClustering} WriteLineToDebugFile('Tdbtablef.Clusterfrequency1Click'); {$EndIf}
   if GISdb[DBonTable].MyData.Filtered then MessageToContinue('Results apply to entire database');
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].dbTableF.SaveHiddenColumns;
   GISdb[DBonTable].dbTablef.UnHideColumns;
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.FindFieldRange('CLUSTER',MinX,MaxX);
   WantedField := GISdb[DBonTable].PickField('unique values' ,[ftString,ftInteger]);
   GISdb[DBonTable].EmpSource.Enabled := false;
   UniqueEntries := GISdb[DBonTable].MyData.UniqueEntriesInDB(WantedField);
   Results := tStringList.Create;
   TStr := WantedField;
   for i := round(MinX) to round(MaxX) do TStr := TStr + ',CLUSTER_' + IntToStr(i);
   Results.Add(TStr);
   for j := 0 to pred(UniqueEntries.Count) do begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      TStr := UniqueEntries.Strings[j];
      for i := round(MinX) to round(MaxX) do  begin
         f1 := WantedField + '=' + QuotedStr(UniqueEntries.Strings[j]) + ' AND CLUSTER=' + IntToStr(i);
         GISdb[DBonTable].MyData.ApplyFilter(f1);
         {$IfDef RecordClustering} WriteLineToDebugFile(f1); {$EndIf}
         TStr := TStr + ',' + IntToStr(GISdb[DBonTable].MyData.RecordCount);
      end;
      Results.Add(TStr);
      {$IfDef RecordClustering} WriteLineToDebugFile(TStr); {$EndIf}
   end;
   fName := Petmar.NextFileNumber(MDTempDir, 'Cluster_freq_',DefaultDBExt);
   {$IfDef RecordClustering} WriteLineToDebugFile(fName); {$EndIf}
   GISdb[DBonTable].theMapOwner.StringListToLoadedDatabase(Results,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].dbTableF.RestoreHiddenColumns;
   GISdb[DBonTable].ShowStatus;
end;



procedure TileCharateristicsWhiskerPlotsByCluster(DBonTable : integer; NoFiltering : boolean; SingleCriterion : shortstring = '');
//makes 7 graphs, one per tile characteristic, for all of the clusters
//if no filtering, does for all the tiles in the DB
var
   GraphImages : tStringList;
   FieldWanted,BaseFilter : shortstring;

      function OneGraph(Param : shortstring)  : tThisBaseGraph;
      var
         MomentVar : tMomentVar;
         i,db : integer;
         Stats : tStringList;
         fName : PathStr;
         aFilter : shortstring;

         procedure DoOneCluster(aFilter : shortstring; ClusterName : shortstring);
         begin
            GISdb[DBonTable].ApplyGISFilter(aFilter);
            GISdb[DBonTable].EmpSource.Enabled := false;
            if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
               MomentVar := GISdb[DBonTable].GetFieldStatistics(Param);
               Stats.Add(ClusterName + ' (n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ')' +  MomentResultsToString(MomentVar));
            end;
         end;

      begin
         if GISdb[DBonTable].MyData.FieldExists(Param) then begin
            Stats := tStringList.Create;
            Stats.Add(MomentStr);
            if NoFiltering then begin
               DoOneCluster(BaseFilter,DEMIX_mode_abbreviation(DEMIX_mode) + ' tiles');
            end
            else begin
               for i := 1 to 15 do begin  //loop through clusters, quantiles
                  aFilter := FieldWanted + '=' + IntToStr(i);
                  if (BaseFilter <> '') then aFilter := BaseFilter + ' AND ' + aFilter;
                  DoOneCluster(aFilter,FieldWanted + IntegerToString(i,3));
               end;
            end;
            fName := NextFileNumber(MDTempDir,DEMIX_mode_abbreviation(DEMIX_mode) + '_' + SingleCriterion + '_' + Param,'.dbf');
            db := StringList2CSVtoDB(Stats,fName);
            Result := StartBoxPlot(db);
            fName := MDTempDir + SingleCriterion + Param + '.bmp';
            SaveImageAsBMP(Result.Image1,fName);
            GraphImages.Add(fName);
         end
         else Result := Nil;
      end;

var
   Graph : array[1..7] of  tThisBaseGraph;
   i : integer;
   fName : PathStr;
begin
   if (SingleCriterion <> '') then BaseFilter := 'CRITERION=' + QuotedStr(SingleCriterion)
   else BaseFilter := GISdb[DBonTable].MyData.Filter;
   if not NoFiltering then begin
      if GISdb[DBonTable].MyData.FieldExists('CLUSTER') then FieldWanted := 'CLUSTER'
      else if GISdb[DBonTable].MyData.FieldExists('NUM_CLUSTR') then FieldWanted := 'NUM_CLUSTR'
      else if GISdb[DBonTable].MyData.FieldExists('QUARTILE') then FieldWanted := 'QUARTILE'
      else FieldWanted := '';
   end;
   GraphImages := tStringList.Create;
   for i := 1 to 7 do Graph[i] := OneGraph(TileCharacters[i]);
   fName := MDTempDir + DEMIX_mode_abbreviation(DEMIX_mode) + SingleCriterion + '_tile_characters.png';
   if (GraphImages.Count = 0) then begin
      MessageToContinue('Add tile characteristics to DB');
      GraphImages.Destroy;
   end
   else MakeBigBitmap(GraphImages,'',fName,3);
   GISdb[DBonTable].ApplyGISFilter(BaseFilter);
end;


procedure DEMIX_clusters_per_tile(DBonTable : integer);
//makes table with number of clusters in each DEMIX tile
//    large numbers means the test DEMs are very different
//    small number means the test DEMs are similar
var
   i : integer;
   Tiles,Results : tstringList;
   fName : PathStr;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   LoadDEMIXnames;
   ShowHourglassCursor;
   Results := tStringList.Create;
   Results.Add('DEMIX_TILE,NUM_CLUSTR');
   Tiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
   for I := 0 to pred(Tiles.Count) do begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tiles.Strings[i]));
      Results.Add(Tiles.Strings[i] + ',' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('CLUSTER')));
   end;
   fName := NextFileNumber(MDTempDir,MDTempDir + 'clusters_per_tile','.dbf');
   StringList2CSVtoDB(Results,fName);
   Tiles.Destroy;
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;


procedure DEMIX_COP_clusters_tile_stats(DBonTable : integer);
// for COP DEM, for each cluster finds mean of the tile characteristics and creates a table
var
   Crit,aline : shortstring;
   i,j,k,NCrit,NDEM,Cluster : integer;
   rfile : file;
   Clusters,Results : tstringList;
   fName : PathStr;
   MomentVar : tMomentVar;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   LoadDEMIXnames;
   ShowHourglassCursor;
   Results := tStringList.Create;
   aline := 'CLUSTER,N';
   Clusters := GISdb[DBonTable].MyData.UniqueEntriesInDB('CLUSTER');
   for I := 1 to NumTileCharacters do aline := aline + ',' + TileCharacters[i];
   Results.Add(aline);

   for I := 0 to pred(Clusters.Count) do begin
      GISdb[DBonTable].ApplyGISFilter('DEM=' + QuotedStr('COP') + ' AND CLUSTER=' + Clusters.Strings[i]);
      GISdb[DBonTable].EmpSource.Enabled := false;
      aline := Clusters.Strings[i] + ',' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB);
      for j := 1 to NumTileCharacters do begin
         MomentVar := GISdb[DBonTable].GetFieldStatistics(TileCharacters[j]);
         aline := aline + ',' + RealToString(MomentVar.Mean,-12,-6);
      end;
      Results.Add(aline);
   end;
   fName := NextFileNumber(MDTempDir,MDTempDir + 'cop_tile_clusters_','.dbf');
   StringList2CSVtoDB(Results,fName);
   Clusters.Destroy;
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;


procedure DEMIX_SSIM_FUV_clusters_diversity_graphs(DBonTable : integer; ColorByDEM : boolean = true);
//Makes one graph per cluster of the evaluation for each criterion, with every Tile/DEM that belongs to the cluster
var
   Tile,Crit,aline : shortstring;
   NCrit,NDEM : integer;
   rfile : file;
   fName : PathStr;
   Clusters,DEMs : tStringList;
   Color : tColor;
   Graph : array[1..15] of tThisBaseGraph;

(*
      procedure StartGraph(i : integer);
      var
         J : integer;
      begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         Graph[i] := tThisBaseGraph.Create(Application);
         Graph[i].GraphDraw.GraphLeftLabels := tStringList.Create;
         for j := 1 to NCrits do Graph[i].GraphDraw.GraphLeftLabels.Add(IntToStr(j) + ',' + Crits[j]);
         Graph[i].GraphDraw.SingleGraphSymbology := true;
      end;

      procedure EndGraph(i : integer);
      begin
         Graph[i].GraphDraw.SetShowAllLines(true, 2);
         Graph[i].GraphDraw.SetShowAllPoints(false);
         Graph[i].GraphDraw.FileColors256[1] := ConvertTColorToPlatformColor(Color);
         Graph[i].RedrawDiagram11Click(Nil);
      end;

   procedure GraphByDEMs;
   var
      i,j : integer;
   begin
      for i := 1 to DEMs.Count do begin
         StartGraph(i);
         DEMIX_SSIM_FUV_GraphSettings(Graph[i],'DEM= ' + DEMs.Strings[pred(i)],NCrits);

         GISdb[DBonTable].ApplyGISFilter('DEM=' + QuotedStr(DEMs.Strings[pred(i)]));
         Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM')));
         GISdb[DBonTable].EmpSource.Enabled := false;
         while not GISdb[DBonTable].MyData.eof do begin
            Graph[i].OpenDataFile(rfile,Color);
            for j := 1 to NCrits do begin
               Graph[i].AddPointToDataBuffer(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Crits[j]),j);
            end;
            Graph[i].ClosePointDataFile(rfile);
            GISdb[DBonTable].MyData.Next;
         end;
         EndGraph(i);
      end;
   end;


   procedure GraphByClusters;
   var
      i,j,k : integer;
   begin
      for I := 1 to Clusters.Count do begin
         StartGraph(i);
         DEMIX_SSIM_FUV_GraphSettings(Graph[i],'Cluster ' + Clusters.Strings[pred(i)] + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB),NCrits);

         UpdateProgressBar(i/Clusters.Count);
         GISdb[DBonTable].ApplyGISFilter('CLUSTER=' + Clusters.Strings[pred(i)]);
         if ColorByDEM then begin
            Graph[i].GraphDraw.SingleGraphSymbology := false;
            for k := 1 to DEMs.Count do begin
               GISdb[DBonTable].ApplyGISFilter('CLUSTER=' + Clusters.Strings[pred(i)] + ' AND DEM=' + QuotedStr(DEMs.Strings[pred(k)]));
               Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM')));
               GISdb[DBonTable].EmpSource.Enabled := false;
               while not GISdb[DBonTable].MyData.eof do begin
                  Graph[i].OpenDataFile(rfile,Color);
                  for j := 1 to NCrits do begin
                     Graph[i].AddPointToDataBuffer(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Crits[j]),j);
                  end;
                  Graph[i].ClosePointDataFile(rfile);
                  GISdb[DBonTable].MyData.Next;
               end;
            end;
         end
         else begin
            while not GISdb[DBonTable].MyData.eof do begin
               Color := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('COLOR');
               Graph[i].OpenDataFile(rfile,Color);
               for j := 1 to NCrits do begin
                  Graph[i].AddPointToDataBuffer(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Crits[j]),j);
               end;
               Graph[i].ClosePointDataFile(rfile);
               GISdb[DBonTable].MyData.Next;
            end;
         end;
         EndGraph(i);
      end;
   end;
*)


begin
   MessageToContinue('Disabled for now');

(*
   ColorByDEM := false;

   GISdb[DBonTable].EmpSource.Enabled := false;
   Clusters := GISdb[DBonTable].MyData.UniqueEntriesInDB('CLUSTER');
   SortStringListNumerically(Clusters);
   GISdb[DBonTable].EmpSource.Enabled := false;
   DEMs := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEM');
   LoadDEMIXnames;
   StartProgress('Cluster');
   GraphByDEMs;
   GraphByClusters;
   Clusters.Destroy;
   DEMs.Destroy;

   EndProgress;
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
*)
end;


function DEMIX_SSIM_FUV_cluster_sensitivity_graph(DBonTable : integer) : tThisBaseGraph;
//graphs showing how the number of clusters changes results
(*
         function OneGraph(MaxCluster : integer): tThisBaseGraph;
         var
            Crit,aline : shortstring;
            i,j,k,NCrit,NDEM,Cluster : integer;
            rfile : file;
         begin
            GISdb[DBonTable].ApplyGISFilter('MAX_CLUSTR=' + IntToStr(MaxCluster));
            if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
               inc(i);
               Result:= tThisBaseGraph.Create(Application);
               Result.GraphDraw.GraphLeftLabels := tStringList.Create;
               Result.GraphDraw.SetShowAllPoints(false);
               Result.GraphDraw.LegendList := tStringList.Create;
               DEMIX_SSIM_FUV_GraphSettings(Result,'Cluster ' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + '/' + IntToStr(MaxCluster),Ncrits);
               Result.GraphDraw.LeftMargin := 175;
               GISdb[DBonTable].EmpSource.Enabled := false;
               while not GISdb[DBonTable].MyData.eof do begin
                  Cluster := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('CLUSTER');
                  Result.GraphDraw.LegendList.Add('Cluster ' + IntToStr(Cluster) + ' n=' + IntToStr(GISdb[DBonTable].MyData.GetFieldByNameAsInteger('N')));
                  Result.OpenDataFile(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsInteger('COLOR'));
                  for j := 1 to NCrits do begin
                     Result.AddPointToDataBuffer(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Crits[j]),j);
                  end;
                  Result.ClosePointDataFile(rfile);
                  GISdb[DBonTable].MyData.Next;
               end;
               for j := 1 to NCrits do Result.GraphDraw.GraphLeftLabels.Add(IntToStr(j) + ',' + Crits[j]);
               Result.RedrawDiagram11Click(Nil);
            end;
         end;
*)

var
   i : integer;
begin
   MessageToContinue('Temporarily disabled');
(*
   GISdb[DBonTable].EmpSource.Enabled := false;
   LoadDEMIXnames;
   ShowHourglassCursor;

   for i := MDDef.ClustSensitiveMin to MDDef.ClustSensitiveMax do begin
      OneGraph(i);
   end;

   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
*)
end;


function DEMIX_SSIM_FUV_clusters_graph(DBonTable : integer) : tThisBaseGraph;
//makes graph of each cluster with criteria on y axis and evaluation on x axis
//compare with https://plotly.com/python/parallel-coordinates-plot/
var
   Crit,aline,aField : shortstring;
   j,k,NCrit,NDEM,Cluster : integer;
   rfile : file;
   fName : PathStr;
   NumCrits : integer;
   TheCrits : array[1..100] of shortstring;

         function OneGraph(What : shortstring): tThisBaseGraph;
         var
            j,n : integer;
         begin
            GISdb[DBonTable].ApplyGISFilter('METRIC=' + QuotedStr(UpperCase(What)));
            if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
               Result:= tThisBaseGraph.Create(Application);
               Result.GraphDraw.GraphLeftLabels := tStringList.Create;
               Result.GraphDraw.BottomMargin := 75;
               Result.GraphDraw.SetShowAllPoints(false);
               Result.GraphDraw.LegendList := tStringList.Create;
               Result.GraphDraw.HorizLabel := 'Dissimilarity/Fraction variance unexplained';
               if (aField = '') then DEMIX_SSIM_FUV_GraphSettings(Result,What,NumCrits)
               else DEMIX_SSIM_FUV_GraphSettings(Result,aField + ' ' + What,NumCrits);
               GISdb[DBonTable].EmpSource.Enabled := false;
               while not GISdb[DBonTable].MyData.eof do begin
                  if (aField = 'CLUSTER') or (aField = '') then begin
                     Cluster := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('CLUSTER');
                     n := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('N');
                     Result.GraphDraw.LegendList.Add('Cluster ' + IntToStr(Cluster) + ' (n=' + IntToStr(n) + ')');
                  end;
                  if (aField = 'DEM') then begin
                     Result.GraphDraw.LegendList.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));
                  end;

                  Result.OpenDataFile(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsInteger('COLOR'));
                  if (aField = '') then begin
                     for j := 1 to NumCrits do begin
                        Result.AddPointToDataBuffer(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsFloat(TheCrits[j]),j);
                     end;
                  end
                  else begin
                     (*
                     for j := 1 to NumCrits do begin
                        Result.AddPointToDataBuffer(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Crits[j]),j);
                     end;
                     *)
                  end;
                  Result.ClosePointDataFile(rfile);
                  GISdb[DBonTable].MyData.Next;
               end;
               if (aField = '') then begin
                  for j := 1 to NumCrits do Result.GraphDraw.GraphLeftLabels.Add(IntToStr(j) + ',' + TheCrits[j]);
               end
               else begin
                  (*
                  for j := 1 to NCrits do Result.GraphDraw.GraphLeftLabels.Add(IntToStr(j) + ',' + Crits[j]);
                  *)
               end;
               Result.RedrawDiagram11Click(Nil);
            end;
         end;

begin
   if GISdb[DBonTable].MyData.FieldExists('METRIC') then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      (*
      if GISdb[DBonTable].MyData.FieldExists('CLUSTER') and GISdb[DBonTable].MyData.FieldExists(Crits[1]) then aField := 'CLUSTER'
      else if GISdb[DBonTable].MyData.FieldExists('DEM') then aField := 'DEM'
      else *) begin
         aField := '';
         NumCrits := 0;
         for j := 0 to pred(GISdb[DBonTable].MyData.FieldCount) do begin
            if (GISdb[DBonTable].MyData.GetFieldType(j) = ftFloat) then begin
               inc(NumCrits);
               TheCrits[NumCrits] := GISdb[DBonTable].MyData.GetFieldName(j);
            end;
         end;
      end;
      LoadDEMIXnames;
      ShowHourglassCursor;
      Result := OneGraph('Mean');
      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].ShowStatus;
   end
   else MessageToContinue('Create cluster metrics table first');
end;



