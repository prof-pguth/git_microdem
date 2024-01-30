{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{    include file for demix_control  }
{____________________________________}


procedure MergeSSIMandR2DB;
var
   db1,db2 : integer;
begin
   //db1 :=
end;


procedure SlopeRoughnessWhiskerPlots(DBonTable : integer);
var
   GraphImages : tStringList;
   FieldWanted : shortstring;

      function OneGraph(Param : shortstring)  : tThisBaseGraph;
      var
         MomentVar : tMomentVar;
         i,db : integer;
         Stats : tStringList;
         fName : PathStr;
      begin
         if GISdb[DBonTable].MyData.FieldExists(Param) then begin
            Stats := tStringList.Create;
            Stats.Add(MomentStr);
            for i := 1 to 15 do begin
               GISdb[DBonTable].ApplyGISFilter(FieldWanted + '=' + IntToStr(i));
               GISdb[DBonTable].EmpSource.Enabled := false;
               if GISdb[DBonTable].MyData.FiltRecsInDB > 0 then begin
                  MomentVar := GISdb[DBonTable].GetFieldStatistics(Param);
                  Stats.Add(FieldWanted + IntegerToString(i,3) + ' (n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ')' +  MomentResultsToString(MomentVar));
               end;
            end;
            fName := NextFileNumber(MDTempDir,Param + '_','.dbf');
            db := StringList2CSVtoDB(Stats,fName);
            Result := StartBoxPlot(db);
            fName := NextFileNumber(MDTempDir,Param + '_','.bmp');
            SaveImageAsBMP(Result.Image1,fName);
            GraphImages.Add(fName);
         end
         else Result := Nil;
      end;
var
   Graph : array[1..7] of  tThisBaseGraph;
begin
   if GISdb[DBonTable].MyData.FieldExists('CLUSTER') then FieldWanted := 'CLUSTER'
   else if GISdb[DBonTable].MyData.FieldExists('NUM_CLUSTR') then FieldWanted := 'NUM_CLUSTR';
   GraphImages := tStringList.Create;
   Graph[1] := OneGraph('AVG_SLOPE');
   Graph[2] := OneGraph('AVG_ROUGH');
   Graph[3] := OneGraph('AVG_ELEV');
   Graph[4] := OneGraph('RELIEF');
   Graph[5] := OneGraph('BARREN_PC');
   Graph[6] := OneGraph('URBAN_PC');
   Graph[7] := OneGraph('FOREST_PC');
   MakeBigBitmap(GraphImages,'Cluster tile characteristics','',3);
   GISdb[DBonTable].ClearGISFilter;
end;


procedure DEMIX_clusters_per_tile(DBonTable : integer);
//makes table with the number of clusters in each DEMIX tile
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
// for the COP DEM, for each cluster finds the mean of the tile characteristics and creates a table
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
   if (Option = opByCluster) then begin
      aField := 'CLUSTER';
      UniqueEntries := GISdb[DBonTable].MyData.UniqueEntriesInDB(aField);
      SortStringListNumerically(UniqueEntries);
   end
   else if (Option = opByDEM) then begin
      aField := 'DEM';
      UniqueEntries := GISdb[DBonTable].MyData.UniqueEntriesInDB(aField);
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
   GISdb[DBonTable].theMapOwner.StringListToLoadedDatabase(Results,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;


procedure DEMIX_SSIM_R2_GraphSettings(var Graph : tThisBaseGraph; lltext : shortstring; Ncrits : integer);
begin
   Graph.GraphDraw.GraphAxes := XPartGridOnly;
   Graph.GraphDraw.LeftMargin := 175;
   Graph.GraphDraw.MaxVertAxis := NCrits + 0.5;
   Graph.GraphDraw.MinVertAxis := 0.5;
   Graph.GraphDraw.MaxHorizAxis := 1.0;
   Graph.GraphDraw.MinHorizAxis := -0.01;
   Graph.GraphDraw.SetShowAllLines(true);
   Graph.GraphDraw.LLcornerText := llText;
end;


procedure DEMIX_SSIM_R2_clusters_diversity_graphs(DBonTable : integer; ColorByDEM : boolean = true);
//Makes one graph per cluster of the evaluation for each criterion, with every Tile/DEM that belongs to the cluster
var
   Tile,Crit,aline : shortstring;
   NCrit,NDEM : integer;
   rfile : file;
   fName : PathStr;
   Clusters,DEMs : tStringList;
   Color : tColor;
   Graph : array[1..15] of tThisBaseGraph;


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
         DEMIX_SSIM_R2_GraphSettings(Graph[i],'DEM= ' + DEMs.Strings[pred(i)],NCrits);

         GISdb[DBonTable].ApplyGISFilter('DEM=' + QuotedStr(DEMs.Strings[pred(i)]));
         Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM')));
         GISdb[DBonTable].EmpSource.Enabled := false;
         while not GISdb[DBonTable].MyData.eof do begin
            Graph[i].OpenDataFile(rfile,true,Color);
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
         DEMIX_SSIM_R2_GraphSettings(Graph[i],'Cluster ' + Clusters.Strings[pred(i)] + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB),NCrits);

         UpdateProgressBar(i/Clusters.Count);
         GISdb[DBonTable].ApplyGISFilter('CLUSTER=' + Clusters.Strings[pred(i)]);
         if ColorByDEM then begin
            Graph[i].GraphDraw.SingleGraphSymbology := false;
            for k := 1 to DEMs.Count do begin
               GISdb[DBonTable].ApplyGISFilter('CLUSTER=' + Clusters.Strings[pred(i)] + ' AND DEM=' + QuotedStr(DEMs.Strings[pred(k)]));
               Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM')));
               GISdb[DBonTable].EmpSource.Enabled := false;
               while not GISdb[DBonTable].MyData.eof do begin
                  Graph[i].OpenDataFile(rfile,true,Color);
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
               Graph[i].OpenDataFile(rfile,true,Color);
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

begin
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
end;


function DEMIX_SSIM_R2_cluster_sensitivity_graph(DBonTable : integer) : tThisBaseGraph;
//graphs showing how the number of cluster changes results

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
               DEMIX_SSIM_R2_GraphSettings(Result,'Cluster ' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + '/' + IntToStr(MaxCluster),Ncrits);
               Result.GraphDraw.LeftMargin := 175;
               GISdb[DBonTable].EmpSource.Enabled := false;
               while not GISdb[DBonTable].MyData.eof do begin
                  Cluster := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('CLUSTER');
                  Result.GraphDraw.LegendList.Add('Cluster ' + IntToStr(Cluster) + ' n=' + IntToStr(GISdb[DBonTable].MyData.GetFieldByNameAsInteger('N')));
                  Result.OpenDataFile(rfile,true,GISdb[DBonTable].MyData.GetFieldByNameAsInteger('COLOR'));
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

var
   i : integer;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   //SafeMakeDir(OutPath);
   LoadDEMIXnames;
   ShowHourglassCursor;

   for i := MDDef.ClustSensitiveMin to MDDef.ClustSensitiveMax do begin
      OneGraph(i);
   end;

   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;


function DEMIX_SSIM_R2_clusters_graph(DBonTable : integer) : tThisBaseGraph;
//makes graph of each cluster with criteria on y axis and evaluation on x axis
var
   Crit,aline,aField : shortstring;
   j,k,NCrit,NDEM,Cluster : integer;
   rfile : file;
   fName,OutPath : PathStr;
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
               Result.GraphDraw.SetShowAllPoints(false);
               Result.GraphDraw.LegendList := tStringList.Create;
               Result.GraphDraw.HorizLabel := 'Dissimilarity/Fraction variance unexplained';
               if (aField = '') then DEMIX_SSIM_R2_GraphSettings(Result,What,NumCrits)
               else DEMIX_SSIM_R2_GraphSettings(Result,aField + ' ' + What,NCrits);
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

                  Result.OpenDataFile(rfile,true,GISdb[DBonTable].MyData.GetFieldByNameAsInteger('COLOR'));
                  if (aField = '') then begin
                     for j := 1 to NumCrits do begin
                        Result.AddPointToDataBuffer(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsFloat(TheCrits[j]),j);
                     end;
                  end
                  else begin
                     for j := 1 to NCrits do begin
                        Result.AddPointToDataBuffer(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Crits[j]),j);
                     end;
                  end;
                  Result.ClosePointDataFile(rfile);
                  GISdb[DBonTable].MyData.Next;
               end;
               if (aField = '') then begin
                  for j := 1 to NumCrits do Result.GraphDraw.GraphLeftLabels.Add(IntToStr(j) + ',' + TheCrits[j]);
               end
               else begin
                  for j := 1 to NCrits do Result.GraphDraw.GraphLeftLabels.Add(IntToStr(j) + ',' + Crits[j]);
               end;
               Result.RedrawDiagram11Click(Nil);
            end;
         end;

begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   if GISdb[DBonTable].MyData.FieldExists('CLUSTER') and GISdb[DBonTable].MyData.FieldExists(Crits[1]) then aField := 'CLUSTER'
   else if GISdb[DBonTable].MyData.FieldExists('DEM') then aField := 'DEM'
   else begin
      aField := '';
      NumCrits := 0;
      for j := 0 to pred(GISdb[DBonTable].MyData.FieldCount) do begin
         if GISdb[DBonTable].MyData.GetFieldType(j) = ftFloat then begin
            inc(NumCrits);
            TheCrits[NumCrits] := GISdb[DBonTable].MyData.GetFieldName(j);
         end;
      end;
   end;

   SafeMakeDir(OutPath);
   LoadDEMIXnames;
   ShowHourglassCursor;

   Result := OneGraph('Mean');
   //OneGraph('Median');

   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;



function DEMIX_SSIM_R2_single_tile_graph(DBonTable : integer; tile : shortstring) :tThisBaseGraph;
var
   DEM : shortstring;
   i,j,k,NCrit,NDEM : integer;
   rfile : file;
begin
   GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tile));
   if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
      GISdb[DBonTable].EmpSource.Enabled := false;

      Result := tThisBaseGraph.Create(Application);
      Result.GraphDraw.LegendList := tStringList.Create;
      Result.GraphDraw.GraphLeftLabels := tStringList.Create;
      for j := 1 to NCrits do Result.GraphDraw.GraphLeftLabels.Add(IntToStr(j) + ',' + Crits[j]);

      while not GISdb[DBonTable].MyData.eof do begin
         DEM := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM');
         Result.GraphDraw.LegendList.Add(DEM);
         Result.OpenDataFile(rfile,true,ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM)));

         for j := 1 to NCrits do begin
            Result.AddPointToDataBuffer(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Crits[j]),j);
         end;
         Result.ClosePointDataFile(rfile);
         GISdb[DBonTable].MyData.Next;
      end;

      DEMIX_SSIM_R2_GraphSettings(Result,Tile,NCrits);
      Result.RedrawDiagram11Click(Nil);
   end;
end;


(*

function DEMIX_SSIM_R2_tile_graph(DBonTable : integer) :tThisBaseGraph;
var
   //values : array[1..9,1..NCrits] of float32;
   Tile,Crit,aline : shortstring;
   i,j,k,NCrit,NDEM : integer;
   rfile : file;
   theTiles,sl : tStringList;
   fName,OutPath : PathStr;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   theTiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
   OutPath := 'c:\temp\ssim_tile_summary\';
   SafeMakeDir(OutPath);
   StartProgress('SSIM summary');
   for k := 0 to pred(theTiles.Count) do begin
      if (k mod 10 = 0) then UpdateProgressBar(k/TheTiles.Count);
      Tile := theTiles[k];
      LoadDEMIXnames;
      Result := DEMIX_SSIM_R2_single_tile_graph(DBonTable, tile);
         fName := OutPath + Tile + '.png';
         SaveImageAsBMP(Result.Image1,fName);
      end;
   end;

   EndProgress;
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;
*)


procedure DEMIX_evaluations_graph(DBonTable : integer);
//for each criterion, graph of evaluations sorted by parameter, and with tile names
var
   Criteria : tStringList;
   rfile : file;
   pn : integer;
   v : array[1..3] of float32;
   aMinVal,aMaxVal : float64;


      function OpenGraph(theCrit : shortstring) : tThisBaseGraph;
      var
         i : integer;
      begin
         GISdb[DBonTable].ApplyGISFilter('CRITERION=' + QuotedStr(theCrit));
         Result:= tThisBaseGraph.Create(Application);
         Result.Width := 750;
         Result.Height := 1500;
         Result.GraphDraw.BottomMargin := 75;
         Result.GraphDraw.ShowHorizAxis1 := true;
         Result.OpenXYColorFile(rfile);
         Result.GraphDraw.HorizLabel := theCrit;
         Result.Caption := GISdb[DBonTable].DBName + ' ' + theCrit;
         Result.GraphDraw.MaxHorizAxis := -99999;
         Result.GraphDraw.MinHorizAxis := 99999;
         for i := 1 to NumDEMIXDEM do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].MyData.FindFieldRange(DEMIXShort[i],aMinVal,aMaxVal);
            if (aminVal < Result.GraphDraw.MinHorizAxis - 0.05) then Result.GraphDraw.MinHorizAxis := aMinVal - 0.05;
            if (amaxval > Result.GraphDraw.MaxHorizAxis + 0.05) then Result.GraphDraw.MaxHorizAxis := aMaxVal + 0.05;
         end;
         GISdb[DBonTable].EmpSource.Enabled := false;
      end;


      function SortedGraphForOneCriterion(theCrit,TheSort : shortstring) : tThisBaseGraph;
      var
        i,DEM : integer;
      begin
         Result := OpenGraph(theCrit);
         Result.GraphDraw.VertLabel := TheSort;
         Result.GraphDraw.GraphLeftLabels := tStringList.Create;
         Result.GraphDraw.MinVertAxis := 0;
         Result.GraphDraw.MaxVertAxis := 10 * GISdb[DBonTable].MyData.FiltRecsInDB + 10;
         Result.GraphDraw.ShowHorizAxis1 := true;
         Result.GraphDraw.BottomMargin := 75;

         GISdb[DBonTable].MyData.FindFieldRange(theSort,aMinVal,aMaxVal);
         Result.GraphDraw.MinVertAxis := aMinVal;
         Result.GraphDraw.MaxVertAxis := aMaxVal;

         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.eof  do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            for I := 1 to NumDEMIXDEM do begin
                v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXDEMTypeName[i]);
                v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(theSort);
                v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXDEMTypeName[i]));
                BlockWrite(rfile,v,1);
            end;
            GISdb[DBonTable].MyData.Next;
         end;
         CloseFile(rfile);
         Result.AutoScaleAndRedrawDiagram(true,false,false,true);
      end;


      function GraphForOneCriterionWithTileNames(theCrit : shortstring) : tThisBaseGraph;
      var
        y,i,DEM : integer;
      begin
         Result := OpenGraph(theCrit);
         Result.GraphDraw.VertLabel := '';
         Result.GraphDraw.GraphLeftLabels := tStringList.Create;

         Result.GraphDraw.MinVertAxis := 0;
         Result.GraphDraw.MaxVertAxis := 10 * GISdb[DBonTable].MyData.FiltRecsInDB + 10;
         Result.GraphDraw.GraphAxes := XPartGridOnly;
         Result.GraphDraw.LeftMargin := 150;

         y := 5;
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.eof do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            Result.GraphDraw.GraphLeftLabels.Add(IntToStr(y) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'));
            for I := 1 to NumDEMIXDEM do begin
                v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
                v[2] := y;  //y + i;
                v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[i]));
                BlockWrite(rfile,v,1);
            end;
            y := y + 10;
            GISdb[DBonTable].MyData.Next;
         end;
         CloseFile(rfile);
         Result.RedrawDiagram11Click(Nil);
      end;

      procedure MakeAllGraphs(TheSort : shortstring; GraphName : shortstring);
      var
         j : integer;
         Graph : tThisBaseGraph;
         BigGraph : tstringList;
         fName : PathStr;
      begin
         if (TheSort = '') or GISdb[DBonTable].MyData.FieldExists(TheSort) then begin
            BigGraph := tstringList.Create;
            for j := 0 to pred(Criteria.Count) do begin
               if (TheSort = '') then Graph := GraphForOneCriterionWithTileNames(Criteria.Strings[j])
               else Graph := SortedGraphForOneCriterion(Criteria.Strings[j],TheSort);
               fName := NextFileNumber(MDTempDir,'graph_','.png');
               SaveImageAsBMP(Graph.Image1,fName);
               BigGraph.Add(fName);
            end;
            fName := NextFileNumber(MDTempDir,GraphName + '_','.png');
            MakeBigBitmap(BigGraph,'',fName,Criteria.Count);
         end;
      end;

begin
   try
      LoadDEMIXnames;
      GISdb[DBonTable].ClearGISfilter;
      GISdb[DBonTable].EmpSource.Enabled := false;
      Criteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
      if MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
         MakeAllGraphs('','Tile_Names');
         MakeAllGraphs('AVG_SLOPE','Sort_by_slope');
         MakeAllGraphs('RELIEF','Sort_by_relief');
      end;
   finally
      GISdb[DBonTable].ClearGISfilter;
      GISdb[DBonTable].ShowStatus;
   end;
end;


procedure MultipleBestByParametersSortByValue(DBonTable,Option : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing,TileParameters : tStringList; ByPointFilters : boolean = false);
//Option = 0 Sort by tile parameters, DEM type, graph for each criterion
//Option = 1 Sort by tile parameters, graph by DEM type and Land Type, merged all 15 criteria
//Option = 2 Sort by tile paramaters, each criterion, and graph by DEM type and Land type
//Option = 3 Sort by tile parameters, graph by criterion and DEM type
var
   BigBitmap : tMyBitmap;
   fName : PathStr;
   UsefulWidth,
   GraphPanelsWide,
   LeftStart : integer;
   UseMax : float32;
   aLine : shortstring;
   Statistics : tStringList;


         function OnlyInTieForBestByParameterSorting(DBonTable : integer; UseMax : float32; TileParam,Criterion,DEMtype,LandType,HorizLabel,VertLabel : shortstring) : tThisBaseGraph;
         var
            TileValue : float32;
            rfile: file;
            i,TotalPoss : integer;
            v : array[1..3] of float32;
            Winners,CriterionFilter : shortstring;
            BestCount : array[1..MaxDEMIXDEM] of int32;
         begin
             if (Criterion = '') then CriterionFilter := CreateFilterOutSignedCriteria(DBonTable)
             else CriterionFilter := 'CRITERION=' + QuotedStr(Criterion);
             for i := 1 to NumDEMIXDEM do BestCount[i] := 0;
             TotalPoss := 0;
             GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(DEMType) + ' AND LAND_TYPE=' + QuotedStr(LandType) + ' AND ' + CriterionFilter);
             {$If Defined(RecordDEMIXSortGraph)}
                WriteLineToDebugFile('BestByParameterSorting, param=' + TileParam + ' recs=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + '  filter=' + GISdb[DBonTable].MyData.Filter);
             {$EndIf}
             GISdb[DBonTable].EmpSource.Enabled := false;
             Result := tThisBaseGraph.Create(Application);
             Result.GraphDraw.HorizLabel := RemoveUnderscores(HorizLabel);
             Result.GraphDraw.VertLabel := RemoveUnderscores(VertLabel);
             Result.GraphDraw.RighJustifyHorizLabel := true;

             Result.Caption := GISdb[DBonTable].DBName;
             Result.OpenXYColorFile(rfile);

             SetDirtAirballBackground(Result,DEMType);

             while not GISdb[DBonTable].MyData.eof do begin
                TileValue := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(TileParam);
                Winners := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC');
                inc(TotalPoss);
                for i := 1 to NumDEMIXDEM do begin
                   if StrUtils.AnsiContainsText(Winners,DEMIXDEMTypeName[i]) then begin
                      v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXDEMTypeName[i]));
                      v[2] := TileValue;
                      v[1] := i;
                      BlockWrite(rfile,v,1);
                      inc(BestCount[i]);
                   end;
                end;
                GISdb[DBonTable].MyData.Next;
             end;
             CloseFile(rfile);
             Result.GraphDraw.MinHorizAxis := 0.5;
             Result.GraphDraw.MaxHorizAxis := 6.5;
             Result.GraphDraw.GraphAxes := YFullGridOnly;
             Result.AutoScaleAndRedrawDiagram(true,false);
             Result.Height := 1200;
             Result.Width := 275;
             Result.GraphDraw.MinVertAxis := 0;
             Result.GraphDraw.MaxVertAxis := UseMax;
             Result.RedrawDiagram11Click(Nil);

             Winners := VertLabel + ',' + DEMType + ',' + LandType + ',' + IntToStr(TotalPoss);
             for i := 1 to NumDEMIXDEM do Winners := Winners + ',' + RealToString(100 * BestCount[i] / TotalPoss,-8,-2);
             Statistics.Add(Winners);
         end;


    procedure MakeGraph(aP : integer; TheCriteria,TheParameter,TheDEM,LandType,HorizLabel,VertLabel : shortstring);
    var
       Bitmap : tMyBitmap;
       FullWidth  : integer;
       SourceRect, DestRect : TRect;
       Gr : TThisBaseGraph;
    begin
      {$If Defined(RecordDEMIXSortGraph)} WriteLineToDebugFile('MakeGraph,   labels: ' + HorizLabel + '  ' + VertLabel); {$EndIf}
      GISdb[DBonTable].EmpSource.Enabled := false;
      if (aP = 1) then UseMax := 1.025 * GISdb[DBonTable].MyData.FindFieldMax(TheParameter);
      Gr := OnlyInTieForBestByParameterSorting(DBonTable,UseMax,TheParameter,TheCriteria,theDEM,LandType,HorizLabel,VertLabel);
      CopyImageToBitmap(gr.Image1,Bitmap);
      {$If Defined(RecordDEMIXSortGraph)}
         fName := Petmar.NextFileNumber(MDTempDir,'frame_' + TheParameter + '_' + TheCriteria + '_' + theDEM + '_','.bmp');
         Bitmap.SaveToFile(fName);
      {$EndIf}
      if (ap = 1) then begin
         UseFulWidth := Bitmap.Width - Gr.GraphDraw.LeftMargin;
         FullWidth := Bitmap.Width + pred(GraphPanelsWide) * (UsefulWidth + 10);
         CreateBitmap(BigBitmap,FullWidth, Bitmap.Height);
         BigBitmap.Canvas.Draw(0,0,Bitmap);
         LeftStart := Bitmap.Width + 10;
      end
      else begin
         SourceRect := Rect(Gr.GraphDraw.LeftMargin,0,Bitmap.Width,Bitmap.Height);
         DestRect := Rect(LeftStart,0,LeftStart + UsefulWidth,Bitmap.Height);
         BigBitmap.Canvas.CopyRect(DestRect,Bitmap.Canvas,SourceRect);
         LeftStart := LeftStart + UsefulWidth + 10;
      end;
      Gr.Destroy;
      Bitmap.Free;
      {$If Defined(RecordDEMIXSortGraph)} WriteLineToDebugFile('MakeGraph out, ap=' + IntToStr(ap) + '  LeftStart=' + IntToStr(LeftStart) + ' ' + BitmapSizeString(BigBitmap)); {$EndIf}
    end;

         procedure FinishBigMap(aName : shortstring);
         var
            BMP : tMyBitmap;
         begin
            fName := MDTempDir + 'Supp_fig_' + aName + '.png';
            BMP := DEMIXTestDEMLegend;
            BigBitmap.Height := BigBitmap.Height + BMP.Height;
            BigBitmap.Canvas.Draw((BigBitmap.Width div 2) - (bmp.Width div 2), BigBitmap.Height - bmp.Height,BMP);
            SaveBitmap(BigBitmap,fName);
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MultipleBestByParameters done graph=' + aName  + ' ' + BitmapSizeString(BigBitmap)); {$EndIf}
            DisplayBitmap(BigBitmap);
            BigBitmap.Free;
         end;


var
   aName : shortstring;
   ap,i : integer;
   aCriteria,aTileParameter,k,Sl : integer;
begin {procedure MultipleBestByParametersSortByValue}
   {$If Defined(RecordDEMIXSortGraph)} WriteLineToDebugFile('MultipleBestByParameters in, table=' + IntToStr(DBontable)); {$EndIf}
   Statistics := tStringList.Create;
   aline := 'GRAPH,REF_TYPE,LAND_TYPE,OPINIONS';
   for i := 1 to NumDEMIXDEM do aLine := aLine + ',' + DEMIXDEMTypeName[i];
   Statistics.Add(aLine);

   if (Option = 0) then begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graph option 0'); {$EndIf}
      for aTileParameter := 0 to pred(TileParameters.Count) do begin
         wmdem.SetPanelText(1,TileParameters[aTileParameter]);
         GraphPanelsWide := CriteriaUsing.Count;
         for k := 0 to pred(DEMsTypeUsing.Count) do begin
            wmdem.SetPanelText(3,DEMsTypeUsing[k]);
            aName :=  'criteria_ref_' + DEMsTypeUsing[k] + '_results_sort_by_' + TileParameters[aTileParameter];
            ap := 1;
            for i := 0 to pred(CriteriaUsing.Count) do begin
               wmdem.SetPanelText(2,CriteriaUsing[i]);
               MakeGraph(ap,CriteriaUsing[i],TileParameters[aTileParameter],DEMsTypeUsing[k],'ALL',CriteriaUsing[i],aName);
               inc(ap);
            end;
            FinishBigMap(aName);
         end;
      end
   end
   else if (Option = 1) then begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graph option 1'); {$EndIf}
      for aTileParameter := 0 to pred(TileParameters.Count) do begin
         wmdem.SetPanelText(1,TileParameters[aTileParameter]);
         aName := 'all_15_criteria_sort_by_' + TileParameters[aTileParameter];
         ap := 1;
         GraphPanelsWide := LandTypesUsing.Count * 2;
         for k := 0 to pred(DEMsTypeUsing.Count) do begin
            wmdem.SetPanelText(2,DEMsTypeUsing[k]);
            for sl := 0 to pred(LandTypesUsing.Count) do begin
               wmdem.SetPanelText(3,LandTypesUsing[sl]);
               MakeGraph(ap,'',TileParameters[aTileParameter],DEMsTypeUsing[k],LandTypesUsing[sl],LandTypesUsing[sl],aName);
               inc(ap);
            end;
         end;
         FinishBigMap(aName);
      end
   end
   else if (Option = 2) then begin
      for aTileParameter := 0 to pred(TileParameters.Count) do begin
         for aCriteria := 0 to pred(CriteriaUsing.Count) do begin
            aName := CriteriaUsing[aCriteria] + '_sort_by_' + TileParameters[aTileParameter];
            ap := 1;
            GraphPanelsWide := LandTypesUsing.Count * 2;
            for k := 0 to pred(DEMsTypeUsing.Count) do begin
               for sl := 0 to pred(LandTypesUsing.Count) do begin
                  MakeGraph(ap,CriteriaUsing[aCriteria],TileParameters[aTileParameter],DEMsTypeUsing[k],LandTypesUsing[sl],LandTypesUsing[sl],aName);
                  inc(ap);
               end;
            end;
            FinishBigMap(aName);
         end;
      end
   end
   else if (Option = 3) then begin
      for aTileParameter := 0 to pred(TileParameters.Count) do begin
         wmdem.SetPanelText(1,TileParameters[aTileParameter]);
         aName := 'criterion_sort_by_' + TileParameters[aTileParameter];
         ap := 1;
         GraphPanelsWide := CriteriaUsing.Count * 2;
         for aCriteria := 0 to pred(CriteriaUsing.Count) do begin
            for k := 0 to pred(DEMsTypeUsing.Count) do begin
               MakeGraph(ap,CriteriaUsing[aCriteria],TileParameters[aTileParameter],DEMsTypeUsing[k],'ALL',CriteriaUsing[aCriteria],TileParameters[aTileParameter]);
               inc(ap);
            end;
         end;
         FinishBigMap(aName);
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MultipleBestByParameters done graph=' + IntToStr(aTileParameter)); {$EndIf}
      end;
   end;

   fName := NextFileNumber(MDTempDir,MDTempDir + 'dem_batting_average_','.dbf');
   StringList2CSVtoDB(Statistics,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end {procedure MultipleBestByParametersSortByValue};


procedure DEMIXMeanMedianHistograms(db : integer);
var
   Min,Max,BinSize : float32;
   Corner : byte;


   procedure MakeOne(DEMType,Param : shortString);
   var
      zvs : array[1..MaxDEMIXDEM] of ^Petmath.bfarray32;
      i,npts,DEM : integer;
      Dist,Legs : tStringList;
      Graph : tThisBaseGraph;
      TStr : shortstring;
   begin
      Dist := tStringList.Create;
      Legs := tStringList.Create;
      for DEM := 1 to NumDEMIXDEM  do begin
         Legs.Add(DEMIXDEMTypeName[DEM]);
      end;
      GISdb[DB].ApplyGISFilter('REF_TYPE=' + QuotedStr(DEMtype) + ' AND LAND_TYPE=' + QuotedStr('ALL') + ' AND CRITERION=' + QuotedStr(Param));
      GISdb[DB].EmpSource.Enabled := false;
      for I := 1 to NumDEMIXDEM  do New(zvs[i]);
      npts := 0;
       GISdb[DB].MyData.First;
       while not GISdb[DB].MyData.eof do begin
          for DEM := 1 to NumDEMIXDEM  do begin
             zvs[DEM]^[npts] := GISdb[DB].MyData.GetFieldByNameAsFloat(DEMIXDEMTypeName[DEM]);
          end;
          inc(Npts);
          GISdb[DB].MyData.Next;
       end;

       for I := 1 to NumDEMIXDEM  do begin
          Dist.Add(SaveSingleValueSeries(npts,zvs[i]^));
          Dispose(zvs[i]);
       end;
       TStr := Param + ' compared to reference ' + DEMtype;
       Graph := CreateMultipleHistogram(MDDef.CountHistograms,Dist,Legs,TStr,TStr,100,Min,Max,BinSize);

      for I := 1 to NumDEMIXDEM  do begin
         Graph.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Graph.GraphDraw.LegendList[pred(i)]);
      end;
      Graph.GraphDraw.InsideMarginLegend := Corner;
      Graph.RedrawDiagram11Click(Nil);
   end;

var
   i,j : integer;
begin
   Min := -4;
   Max := 4;
   BinSize := 0.5;
   Corner := lpNWMap;
   for j := 1 to 2 do begin
      for i := 1 to 3 do begin
         MakeOne(RefDEMtype[j],MeanParams[i]);
      end;
   end;

   for j := 1 to 2 do begin
      for i := 1 to 3 do begin
         MakeOne(RefDEMtype[j],MedianParams[i]);
      end;
   end;

   Min := 0;
   Max := 6;
   BinSize := 0.25;
   Corner := lpNEMap;
   for j := 1 to 2 do begin
      for i := 1 to 3 do begin
         MakeOne(RefDEMtype[j],StdDevParams[i]);
      end;
   end;
end {DEMIXMeanMedianHistograms};


function MakeHistogramOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;
var
   FilesWanted,Distributions,Legends : tStringList;
   fName : PathStr;
   UseTile : boolean;
   i,j : integer;
   Min,Max,BinSize : float32;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution in, ' + Tile + '  ' + Param + '  ' + Ref); {$EndIf}
   MDDef.DefaultGraphXSize := 1000;
   MDDef.DefaultGraphYSize := 600;
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_diff_dist,'*.z',FilesWanted,0);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution dist file=' + IntToStr(FilesWanted.Count)); {$EndIf}

   Distributions := tStringList.Create;
   Legends := tStringList.Create;
   for i := 0 to pred(FilesWanted.Count) do begin
      fName := upperCase(FilesWanted.Strings[i]);
      if StrUtils.AnsiContainsText(fname,UpperCase(Tile)) and StrUtils.AnsiContainsText(fname,UpperCase(Param)) then begin
         if (UpperCase(Ref) = 'DSM') then UseTile := StrUtils.AnsiContainsText(fname,'DSM')
         else UseTile := not StrUtils.AnsiContainsText(fname,'DSM');
         if UseTile then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution use ' + fName); {$EndIf}
            Distributions.Add(fName);
            for j := 1 to NumDEMIXDEM do begin
               if StrUtils.AnsiContainsText(fname,UpperCase(DEMIXDEMTypeName[j])) then begin
                  Legends.Add(DEMIXDEMTypeName[j]);
               end;
            end;

            if StrUtils.AnsiContainsText(fname,'ELEV') then begin
               Min := -5;
               Max := 5;
               BinSize := 0.10;
            end;
            if StrUtils.AnsiContainsText(fname, 'SLOPE') then begin
               Min := -10;
               Max := 10;
               BinSize := 0.25;
            end;
            if StrUtils.AnsiContainsText(fname, 'RUFF') then begin
               Min := -10;
               Max := 10;
               BinSize := 0.15;
            end;
         end;
      end;
   end;

   if (Distributions.Count = 6) and (Legends.Count = 6) then begin
      Result := CreateMultipleHistogram(MDDef.CountHistograms,Distributions,Legends,Tile + '  ' + param + ' difference distribution to reference ' + Ref,
        Tile + '  ' + param + ' difference',100,Min,Max,BinSize);
      for I := 1 to Result.GraphDraw.LegendList.Count do begin
         Result.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Result.GraphDraw.LegendList[pred(i)]);
      end;
      Result.GraphDraw.InsideMarginLegend := lpNWMap;
      SetDirtAirballBackground(Result,Ref);

      Result.RedrawDiagram11Click(Nil);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution out'); {$EndIf}
   end
   else begin
      Result := nil;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution fail, Dist=' + IntToStr(Distributions.Count) + '  Leg=' + IntToStr(Legends.Count) ); {$EndIf}
   end;
   FilesWanted.Free;
end;


procedure DEMIX_graph_best_in_Tile(DBonTable : integer; SortByArea : boolean);
var
   RefFilter : shortstring;
   CriteriaTable : tMyData;
   BigBMPFiles : tStringList;
   fName,TileSortName : PathStr;
   ch : ANSIchar;


    procedure OneLoop;
    const
       InterAreaSkip = 2;
    var
       NumAreas,NumTiles : integer;
       TileList,AreaList : tStringList;
       i,j,CurrentY,DEM,Center,NumTies : integer;
       Criterion,Best,TStr,AreaFilter : shortstring;
       aDEM : array[1..10] of shortstring;
       Graph : tThisBaseGraph;
       rfile : array[1..10] of file;
       v : array[1..2] of float32;
       Symbol : tFullSymbolDeclaration;
       fName : PathStr;
       TieTolerance : float32;

                procedure LocateBest(Best : shortstring; Center : float32);
                var
                   DEM : integer;
                begin
                    for DEM := 1 to NumDEMIXDEM do begin
                       if (Best = DEMIXDEMTypeName[DEM]) then begin
                          v[2] := CurrentY;
                          v[1] := Center;
                          BlockWrite(rfile[DEM],v,1);
                       end;
                    end;
                end;


    begin
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('One loop in, ' + RefFilter); {$EndIf}
       ShowHourglassCursor;
       GISdb[DBonTable].ApplyGISFilter(RefFilter);
       GISdb[DBonTable].EmpSource.Enabled := false;
       NumTiles := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE');
       GISdb[DBonTable].EmpSource.Enabled := false;

       AreaList := tStringList.Create;
       if SortByArea then begin
          NumAreas := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA');
          GISdb[DBonTable].EmpSource.Enabled := false;
          fName := DEMIXSettingsDir +  'demix_areas_sorted_by_lat.txt';
          AreaList.LoadFromFile(fName);
       end
       else begin
          AreaList.Add('by sort');
          fName := TileSortName;
          TileList := tStringList.Create;
          TileList.LoadFromFile(fName);
          AreaFilter := '';
          NumAreas := 1;
       end;

       MDDef.DefaultGraphXSize := 1200;
       MDDef.DefaultGraphYSize := NumTiles * 20 + 50;

      Graph := tThisBaseGraph.Create(Application);

      Graph.GraphDraw.LegendList := tStringList.Create;
      for DEM := 1 to NumDEMIXDEM do begin
         Symbol := SymbolFromDEMName(DEMIXDEMTypeName[DEM]);
         Symbol.DrawingSymbol := FilledBox;
         Graph.OpenPointFile(rfile[DEM],Symbol);
         Graph.GraphDraw.LegendList.Add(DEMIXDEMTypeName[DEM]);
      end;

      Graph.GraphDraw.TopLabel := RefFilter  + '  n=' + IntToStr(NumTiles);
      Graph.GraphDraw.HorizLabel := '';
      Graph.Caption := Graph.GraphDraw.TopLabel;
      Graph.GraphDraw.GraphAxes := NoGrid;
      Graph.GraphDraw.MinHorizAxis := 0;
      Graph.GraphDraw.MaxHorizAxis := 20;
      Graph.GraphDraw.MaxVertAxis := NumTiles + InterAreaSkip * NumAreas;
      Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
      Graph.GraphDraw.GraphBottomLabels := tStringList.Create;
      Graph.GraphDraw.TopLeftLabel := ExtractFileNameNoExt(fName);

      CriteriaTable.ApplyFilter('');
      while not CriteriaTable.eof do begin
         TStr := CriteriaTable.GetFieldByNameAsString('CRITERION');
         TieTolerance := CriterionTieTolerance(TStr);
         TStr := IntToStr(CriteriaTable.GetFieldByNameAsInteger('AXIS_VALUE')) + ',' + TStr + ' (' + RealToString(TieTolerance,-8,-2) + ')';
         Graph.GraphDraw.GraphBottomLabels.Add(TStr);
         CriteriaTable.next;
      end;

       CurrentY := NumTiles + InterAreaSkip * NumAreas;

       for i := 0 to pred(AreaList.Count) do begin
          {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile(AreaList.Strings[i]); {$EndIf}
          WMDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(AreaList.Count) );
          WMDEM.SetPanelText(1,RefFilter);
          if SortByArea then AreaFilter := ' AND AREA=' + QuotedStr(AreaList.Strings[i]);
          GISdb[DBonTable].ApplyGISFilter(RefFilter + AreaFilter);
          TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');

          if SortByArea and (TileList.Count > 0) then begin
             TStr := IntToStr(CurrentY-TileList.Count div 2) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA');
             Graph.GraphDraw.GraphLeftLabels.Add(TStr);
          end;

          for j := 0 to pred(TileList.Count) do begin
             dec(CurrentY);
             if not SortByArea then begin
                TStr := IntToStr(CurrentY) + ',' + TileList.Strings[j];
                Graph.GraphDraw.GraphLeftLabels.Add(TStr);
                {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile('Left label=' + TStr); {$EndIf}
             end;

             TStr := RefFilter + AreaFilter + ' AND DEMIX_TILE=' + QuotedStr(TileList.Strings[j]);
             GISdb[DBonTable].ApplyGISFilter(TStr);
             {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
             GISdb[DBonTable].EmpSource.Enabled := false;
             while not GISdb[DBonTable].MyData.eof do begin
                Criterion := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
                Best := uppercase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC'));
                {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile(Criterion + '   ' + Best); {$EndIf}

                CriteriaTable.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
                if (CriteriaTable.FiltRecsInDB = 1) then begin
                   Center := CriteriaTable.GetFieldByNameAsInteger('AXIS_VALUE');
                   ch := 'X';
                   if StrUtils.AnsiContainsText(Best,',') then ch := ',';
                   if StrUtils.AnsiContainsText(Best,';') then ch := ';';
                   if StrUtils.AnsiContainsText(Best,'-') then ch := '-';

                   if (ch in [',',';','-']) then begin //this is a tie for best
                      NumTies := 0;
                      repeat
                         inc(NumTies);
                         aDEM[NumTies] := BeforeSpecifiedCharacterANSI(Best,ch,true,true);
                      until Best = '';
                      for DEM := 1 to NumTies do begin
                         LocateBest(aDEM[DEM],Center-0.3 + DEM * 0.15);
                      end;
                   end
                   else begin //single best
                      LocateBest(Best,Center);
                   end;
                end;
                GISdb[DBonTable].MyData.Next;
             end;
          end;
          if (TileList.Count > 0) then dec(CurrentY,InterAreaSkip);  //add a blank line between areas, but only if this area had tiles
       end;

       for DEM := 1 to NumDEMIXDEM do begin
          CloseFile(rfile[DEM]);
       end;

       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
       Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
       Graph.GraphDraw.BottomMargin := 150;
       Graph.GraphDraw.TopMargin := 50;
       Graph.Height := 1250;
       Graph.GraphDraw.LLlegend := true;
       Graph.GraphDraw.MinVertAxis := CurrentY;
       Graph.RedrawDiagram11Click(Nil);
       fName := NextFileNumber(MDTempDir,'best_dem_graph_','.png');
       SaveImageAsBMP(Graph.Image1,fName);
       BigBMPfiles.Add(fName);
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('One loop out'); {$EndIf}
    end;


var
   i,j : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_graph_best_in_Tile in'); {$EndIf}
   try
      SaveBackupDefaults;
      fName := DEMIXSettingsDir + 'demix_criteria_order_with_gaps.dbf';
      CriteriaTable := tMyData.Create(fName);

      if (not SortByArea) then begin
         if (not GetExistingFileName('sorting order for tiles','*.txt',TileSortName)) then exit;
      end;

      for i := 1 to 2 do begin  //ref_type, dsm or dtm
         BigBMPfiles := tStringList.Create;
         if SortByArea then begin
            for j := 1 to NumLandTypes do begin
               RefFilter := 'REF_TYPE=' + QuotedStr(RefDEMType[i]) + ' AND LAND_TYPE=' + QuotedStr(LandTypes[j]);
               OneLoop;
            end;
         end
         else begin
            RefFilter := 'REF_TYPE=' + QuotedStr(RefDEMType[i]) + ' AND LAND_TYPE=' + QuotedStr('ALL');
            OneLoop;
         end;
         fName := NextFileNumber(MDtempDir,'criteria_by_tile_','.png');
         MakeBigBitmap(BigBMPfiles,'',fName,4);
         DisplayBitmap(fName,'');
      end;
   finally
      CriteriaTable.Destroy;
      GISdb[DBonTable].EmpSource.Enabled := true;
      GISdb[DBonTable].ClearGISFilter;
      WMDEM.SetPanelText(0,'');
      WMDEM.SetPanelText(1,'');
      EndProgress;
      RestoreBackupDefaults;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_graph_best_in_Tile out'); {$EndIf}
end;


function DEMIXwineContestScoresGraph(DBonTable : integer; XScalelabel : shortstring; MinHoriz : float32 = 0.5; MaxHoriz : float32 = 5.5) : tThisBaseGraph;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph in, table=' + IntToStr(DBontable)); {$EndIf}
   BestDEMSbyCategory(DBonTable);
end;


procedure BestDEMSbyCategory(DBonTable : integer);
var
   DEMs,Criteria,Besties,Findings,Findings2,AverageScore,FiltersTooFewOpinions : tStringList;
   i : integer;
   MaxSum : float64;
   theSums : array[0..10] of float64;
   fName : PathStr;
   aLine : shortstring;


   procedure DoOne(Header,theFilter : shortstring);
   var
      Total,DEM,Ties,ThisCat,Opinions : integer;
   begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DO-ONE  ' + theFilter); {$EndIf}
      WMDEM.SetPanelText(1,theFilter);
      GISdb[DBonTable].ApplyGISFilter(theFilter);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Opinions := GISdb[DBonTable].MyData.FiltRecsInDB;
      if (Opinions >= 10) then begin
         Ties := Opinions;
         Total := Opinions;
         aline := Header + '  (n=' + IntToStr(Opinions) + ')' + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr(DEMs.Strings[DEM]) );
            ThisCat := GISdb[DBonTable].MyData.FiltRecsInDB;
            aLine := aLine + ',' + IntToStr(ThisCat);
            Ties := Ties - ThisCat;
         end;
         Besties.Add(aLine + ',' + IntToStr(Ties) );

         GISdb[DBonTable].ApplyGISFilter(theFilter);
         MaxSum := 0;
         for DEM := 0 to pred(DEMs.Count) do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
            theSums[DEM] := GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
            if theSums[DEM] > maxSum then MaxSum := theSums[DEM];
            Findings.Add(Header + ',' + DEMs.Strings[DEM] + ',' + RealToString(theSums[DEM],12,-4));
         end;

         aline := Header + ' (n=' + IntToStr(Opinions) + ')' + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            aLine := aLine + ',' + RealToString(theSums[DEM] / MaxSum,12,-4);
         end;
         Findings2.Add(aLine);

         aline := Header + '  (n=' + IntToStr(Opinions) + ')' + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            aLine := aLine + ',' +  RealToString(theSums[DEM] / Opinions,12,-4);
         end;
         AverageScore.Add(aLine);
      end
      else begin
         FiltersTooFewOpinions.Add(theFilter + ',' + IntToStr(Opinions));
      end;
   end;


var
   Header,aFilter,RefFilter : ShortString;
   j,ScoresDB : integer;
   Graph : tThisBaseGraph;
   bmp : tMyBitmap;
   LegendfName : PathStr;
   BigBitmap : tStringList;
   anImage : TImageDisplayForm;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory in, ' + GISdb[DBonTable].dbName); {$EndIf}
   GetDEMIXPaths;
   Criteria := tStringList.Create;
   Criteria.LoadFromFile(DEMIXSettingsDir + 'criteria_all.txt');

   DEMs := tStringList.Create;
   DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems_all_six.txt');
   ShowHourglassCursor;
   if not GISdb[DBonTable].MyData.FieldExists(DEMs.Strings[0] + '_SCR') then begin
      RankDEMS(DBonTable);
   end;

   FiltersTooFewOpinions := tStringList.Create;
   FiltersTooFewOpinions.Add('FILTER,OPINIONS');

   Besties := tStringList.Create;
   aLine := 'FILTER,OPINIONS';
   for i := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[i];
   Besties.Add(aLine + ',TIES');

   Findings2 := tStringList.Create;
   aLine := 'FILTER,OPINIONS';
   for i := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[i];
   Findings2.Add(aLine);

   Findings := tStringList.Create;
   Findings.Add('FILTER,DEM,SCORE_SUM');

   for i := 1 to 2 do begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory DEM=' + RefDEMType[i] ); {$EndIf}
      AverageScore := tStringList.Create;
      aLine := 'FILTER,OPINIONS';
      for j := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[j];
      AverageScore.Add(aLine);
      BigBitmap := tStringList.Create;
      RefFilter := ' AND REF_TYPE=' + QuotedStr(RefDEMType[i]);
      for j := 1 to NumLandTypes do begin
         DoOne(RefDEMType[i] + ' ' + LandTypes[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandTypes[j]) + RefFilter);
      end;
      AverageScore.Add('SKIP');

      for J := 1 to 9 do begin
         GetFilterAndHeader(i,j, Header,aFilter);
         DoOne(Header,aFilter);
      end;
      AverageScore.Add('SKIP');

      for j := 0 to pred(Criteria.Count) do begin
         DoOne(RefDEMType[i] + ' ALL pixels  ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
      end;

      fName := NextFileNumber(MDTempDir,RefDEMType[i] + '_average_scores_','.dbf');
      ScoresDB := StringList2CSVtoDB(AverageScore,fName);
      Graph := DEMIXwineContestScoresGraph(ScoresDB,'Average score',0.5, 6.25);
      if (Graph.GraphDraw.LegendList <> nil) and (Graph.GraphDraw.LegendList.Count > 0) then begin
         LegendfName := NextFileNumber(MDTempDir,'legend_','.png');
         bmp := Graph.MakeLegend(Graph.GraphDraw.LegendList,false);
         SaveBitmap(bmp,LegendfName);
         Bmp.free;
      end;

      fName := NextFileNumber(MDTempDir,'best_graph_','.png');
      SaveImageAsBMP(Graph.Image1,fName);
      BigBitmap.Add(fName);
      BigBitmap.Add(LegendfName);
      fName := NextFileNumber(MDTempDir,RefDEMType[i] + '_best_graph_with_legend_','.png');
      anImage := MakeBigBitmap(BigBitmap,'',fName,2);
      anImage.Destroy;

      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].ShowStatus;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Done BestDEMSbyCategory DEM=' + RefDEMType[i] ); {$EndIf}
   end;

      //unclear if these are really needed
         fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + 'criteria_best_count.dbf';
         StringList2CSVtoDB(Besties,fName);

         fName := NextFileNumber(MDTempDir, 'multi_line_scores_','.dbf');
         StringList2CSVtoDB(Findings,fName);

         fName := NextFileNumber(MDTempDir,'single_line_scores_','.dbf');
         StringList2CSVtoDB(Findings2,fName);

         fName := NextFileNumber(MDTempDir,'filters_too_few_opinions_','.dbf');
         StringList2CSVtoDB(FiltersTooFewOpinions,fName);

   DEMs.Destroy;
   Criteria.Destroy;
   EndDEMIXProcessing;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory out, ' + GISdb[DBonTable].dbName); {$EndIf}
end;


procedure ModeSTDPlot(DBonTable : integer);
const
   NumParams = 3;
   Params : array[1..NumParams] of shortstring = ('elvd_mode','slpd_mode','rufd_mode');
var
   i,j : integer;
   ThisGraph : array[1..NumParams] of TThisbasegraph;
   rfile : array[1..NumParams] of file;
   v : array[1..3] of float32;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ModeSTDPlot in, ' + GISdb[DBonTable].dbName); {$EndIf}
   with GISdb[DBonTable] do begin
      if Not MyData.FieldExists(DEMIXshort[1] + '_MODE') then begin
         MessageToContinue('Requires DB with modes');
      end
      else begin
         for I := 1 to NumParams do begin
            MyData.First;
            ApplyGISFilter('CRITERION='+ QuotedStr(Params[i]) + ' AND REF_TYPE=' + QuotedStr('DTM'));
            EmpSource.Enabled := false;
            ThisGraph[i] := TThisbasegraph.Create(Application);
            ThisGraph[i].GraphDraw.HorizLabel := Params[i];
            ThisGraph[i].GraphDraw.VertLabel := 'Standard Deviation';
            ThisGraph[i].OpenXYColorFile(rfile[i]);
            while not MyData.eof do begin
               for j := 1 to NumDEMIXDEM do begin
                   v[1] := MyData.GetFieldByNameAsFloat(DEMIXshort[j] + '_MODE');
                   v[2] := MyData.GetFieldByNameAsFloat(DEMIXshort[j] + '_STD');
                   v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXDEMTypeName[j]));
                   BlockWrite(rfile[i],v,1);
               end;
               MyData.Next;
            end;
            CloseFile(rfile[i]);
            ThisGraph[i].AutoScaleAndRedrawDiagram(true,true);
            ThisGraph[i].RedrawDiagram11Click(Nil);
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Done graph ' + IntToStr(i)); {$EndIf}
         end;
         EmpSource.Enabled := true;
         ClearGISFilter;
      end;
   end;
end;


procedure DEMIXMeanMedianModeHistograms(db : integer);
const
(*
   NumParams = 4;
   NumStats = 2;
   Params : array[1..NumParams] of shortstring = ('elev_','slope_','ruff_','rri_');
   ParamsLong : array[1..NumParams] of shortstring = ('Elevation','Slope','Roughness','RRI');
   Stats : array[1..NumStats] of shortstring = ('ssim','wang');
   StatsLong : array[1..NumStats] of shortstring = ('ssim','wang');
*)
   NumParams = 3;
   NumStats = 4;
   Params : array[1..NumParams] of shortstring = ('elvd_','slpd_','rufd_');
   ParamsLong : array[1..NumParams] of shortstring = ('Elevation','Slope','Roughness');
   Stats : array[1..NumStats] of shortstring = ('mean','medn','mode','std');
   StatsLong : array[1..NumStats] of shortstring = ('Mean','Median','Mode','Std dev');
var
   Min,Max,BinSize : float32;
   Corner : byte;


   procedure MakeOne(j2,i2 : integer);
   var
      zvs : array[1..MaxDEMIXDEM] of ^Petmath.bfarray32;
      Param,Stat : shortString;
      i,npts,DEM : integer;
      Dist,Legs : tStringList;
      Graph : tThisBaseGraph;
      TStr : shortstring;
   begin
      Param := Params[j2];
      Stat := Stats[i2];
      Dist := tStringList.Create;
      Legs := tStringList.Create;
      for DEM := 1 to NumDEMIXDEM do begin
         Legs.Add(DEMIXDEMTypeName[DEM]);
      end;
      GISdb[DB].ApplyGISFilter('REF_TYPE=' + QuotedStr('DTM') + ' AND CRITERION=' + QuotedStr(Param + Stat));
      GISdb[DB].EmpSource.Enabled := false;
      if GISdb[db].MyData.FiltRecsInDB = 0 then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Nothing for ' + GISdb[db].MyData.Filter); {$EndIf}
      end
      else begin
         for I := 1 to NumDEMIXDEM do New(zvs[i]);
         npts := 0;
          GISdb[DB].MyData.First;
          while not GISdb[DB].MyData.eof do begin
             for DEM := 1 to NumDEMIXDEM do begin
                zvs[DEM]^[npts] := GISdb[DB].MyData.GetFieldByNameAsFloat(DEMIXshort[DEM]);   //  + '_' + Stat );
             end;
             inc(Npts);
             GISdb[DB].MyData.Next;
          end;

          for I := 1 to NumDEMIXDEM do begin
             Dist.Add(SaveSingleValueSeries(npts,zvs[i]^));
             Dispose(zvs[i]);
          end;
          TStr := StatsLong[i2] + '_' + Paramslong[j2] + '_difference_compared_to_reference_DTM';
          Graph := CreateMultipleHistogram(MDDef.CountHistograms,Dist,Legs,TStr,TStr,100,Min,Max,BinSize);

         for I := 1 to NumDEMIXDEM do begin
            Graph.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Graph.GraphDraw.LegendList[pred(i)]);
         end;
         Graph.GraphDraw.InsideMarginLegend := Corner;
         Graph.RedrawDiagram11Click(Nil);
      end;
   end;

var
   i,j : integer;
begin
   LoadDEMIXnames;
   Min := -4;
   Max := 4;
   BinSize := 0.5;
   Corner := lpNWMap;
   for i := 1 to NumStats do begin
      if (i = 4) then begin
         Min := 0;
         Max := 8;
         Corner := lpNEMap;
      end;
      for j := 1 to NumParams do begin
         MakeOne(j,i);  //Params[j],Stats[i]);
      end;
   end;
   GISdb[DB].ClearGISFilter;
end;


procedure DEMIXwineContestCriterionGraph(What,DBonTable : integer; AreaList : tStringList = nil; CriteriaUsed : tStringList = nil; LandTypePresent : tStringList = nil; DEMsPresent : tStringList = nil);
//includes a number of options that have not been used recently, and may not currently work
const
   InterAreaSkip = 4;
var
   AllTiles : tStringList;
   BigGraph : tstringList;
   PickParam,ThisTile : shortstring;
   WhatNow : integer;
   LegendfName,fName : PathStr;


         procedure MakeGraphForFilter(Param,LandType,DEMType : shortstring);
         var
            Graph : tThisBaseGraph;
            Symbol : tFullSymbolDeclaration;
            DEM,CurrentY,DEMis,i,j,k,NumAreas,NumTiles : integer;
            rfile : array[1..10] of file;
            TileList : tStringList;
            v : array[1..2] of float32;
            Max : float32;
            aMinVal,aMaxVal : float64;
            RefFilter,aFilter : shortstring;
            bmp : tMyBitmap;

               procedure OnePercentage(aFilter : shortstring);
               var
                  Total,i : integer;
               begin
                  GISdb[DBonTable].ApplyGISFilter(aFilter);
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  Total := GISdb[DBonTable].MyData.FiltRecsInDB;
                  dec(CurrentY);
                  Graph.GraphDraw.GraphLeftLabels.Add(aFilter);
                  for i := 0 to pred(DEMsPresent.Count) do begin
                     GISdb[DBonTable].ApplyGISFilter(aFilter + ' AND DEM_LOW_SC=' + QuotedStr(DEMsPresent.Strings[i]));
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     v[2] := CurrentY;
                     v[1] := 100 * GISdb[DBonTable].MyData.FiltRecsInDB / Total;
                     BlockWrite(rfile[succ(i)],v,1);
                  end;
               end;

         begin {MakeGraphForFilter}
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphForFilter in, ' + Param + ' ' + Landtype + ' ' + DEMtype); {$EndIf}
            WMDEM.SetPanelText(1,Param + ' ' + Landtype + ' ' + DEMtype);
            if (What in [dgAllValues,dgAllScores,dgPercentBest]) then begin
            end
            else if (What in [dgArea]) then begin
               RefFilter := 'LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
            end
            else if (What in [dgPick,dg7Params,dgNormalizedDiff ]) then begin
               RefFilter := 'CRITERION=' + QuotedStr(Param)  + ' AND LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
            end;

            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].ApplyGISFilter(RefFilter);
            if (GISdb[DBonTable].MyData.FiltRecsInDB = 0) then begin //primarily needed if there are no DSM opinions
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No Recs for ' + GISdb[DBonTable].MyData.Filter); {$EndIf}
               exit;
            end;

            GISdb[DBonTable].EmpSource.Enabled := false;
            NumTiles := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE');
            GISdb[DBonTable].EmpSource.Enabled := false;
            NumAreas := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA');

            CurrentY := NumTiles + (InterAreaSkip * NumAreas);

            Graph := tThisBaseGraph.Create(Application);
            Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
            Graph.GraphDraw.LegendList := tStringList.Create;
            Graph.GraphDraw.VertLabel := '';
            Graph.Caption := GISdb[DBonTable].DBName;
            Graph.GraphDraw.MinVertAxis := -1;
            Graph.GraphDraw.MaxVertAxis := CurrentY;
            Graph.GraphDraw.ShowHorizAxis0 := true;
            Graph.GraphDraw.GraphAxes := XPartGridOnly;

            GISdb[DBonTable].EmpSource.Enabled := false;
            Graph.GraphDraw.MaxHorizAxis := -99999;
            Graph.GraphDraw.MinHorizAxis := 99999;
            for DEM := 1 to DEMsPresent.Count do begin
               GISdb[DBonTable].MyData.FindFieldRange(DEMsPresent.Strings[pred(DEM)],aMinVal,aMaxVal);
               if (aminVal < Graph.GraphDraw.MinHorizAxis) then Graph.GraphDraw.MinHorizAxis := aMinVal;
               if (amaxval > Graph.GraphDraw.MaxHorizAxis) then Graph.GraphDraw.MaxHorizAxis := aMaxVal;
            end;

            for DEM := 1 to DEMsPresent.Count do begin
               Symbol := SymbolFromDEMName(DEMsPresent.Strings[Pred(DEM)]);
               Graph.OpenPointFile(rfile[DEM],Symbol);
               Graph.GraphDraw.LegendList.Add(DEMsPresent.Strings[Pred(DEM)]);
            end;

            for i := 0 to pred(AreaList.Count) do begin
             GISdb[DBonTable].EmpSource.Enabled := false;
                if (RefFilter <> '') then RefFilter := RefFilter + ' AND ';
                GISdb[DBonTable].ApplyGISFilter(RefFilter + 'AREA=' + QuotedStr(AreaList.Strings[i]));
                WMDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(AreaList.Count) );
                WMDEM.SetPanelText(1,RefFilter);
                if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
                   TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
                   Graph.GraphDraw.GraphLeftLabels.Add(IntToStr(CurrentY-TileList.Count div 2) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA'));
                   for j := 0 to pred(TileList.Count) do begin
                      dec(CurrentY);
                      GISdb[DBonTable].ApplyGISFilter(RefFilter + ' AND AREA=' + QuotedStr(AreaList.Strings[i]) + ' AND DEMIX_TILE=' + QuotedStr(TileList.Strings[j]));
                      GISdb[DBonTable].EmpSource.Enabled := false;

                     if What in [dgArea] then begin
                        for DEM := 0 to pred(DEMsPresent.Count) do begin
                           v[2] := CurrentY;
                           v[1] := GISdb[DBonTable].MyData.FieldSum(DEMsPresent.Strings[DEM] + '_SCR') / GISdb[DBonTable].MyData.FiltRecsInDB;
                           BlockWrite(rfile[succ(DEM)],v,1);
                        end;
                        GISdb[DBonTable].MyData.Next;
                     end
                     else if What in [dgAllValues,dgAllScores] then begin
                        (*
                        Graph.GraphDraw.VertLabel := '';
                        GISdb[DBonTable].MyData.First;
                        while not GISdb[DBonTable].MyData.eof do begin
                           GISdb[DBonTable].EmpSource.Enabled := false;
                           inc(OnTile);
                           Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION') + '  ' + LandType + '  ' + DEMType);
                           for DEM := 0 to pred(DEMsPresent.Count) do begin
                              v[2] := Ontile;
                              if What in [dgAllValues] then v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM])
                              else v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM] + '_SCR');
                              BlockWrite(rfile[succ(DEM)],v,1);
                           end;
                           GISdb[DBonTable].MyData.Next;
                        end;
                        *)
                     end
                     else if What in [dgPercentBest] then begin
                        (*
                        for i := 1 to 2 do OnePercentage('REF_TYPE=' + QuotedStr(RefDEMType[i]));
                        inc(OnTile);
                        Graph.GraphDraw.GraphLeftLabels.Add('-----------');

                        for i := 1 to MaxLandType do OnePercentage('LAND_TYPE=' + QuotedStr(LandType[i]));
                        inc(OnTile);
                        Graph.GraphDraw.GraphLeftLabels.Add('-----------');
                        for i := 1 to AllCriteria.Count do OnePercentage('CRITERION=' + QuotedStr(AllCriteria.Strings[pred(i)]));
                        *)
                     end
                     else if (What in [dg7Params,dgPick,dgNormalizedDiff]) then begin
                        if  (What in [dgNormalizedDiff]) then begin
                           Max := 0;
                           for DEM := 0 to pred(DEMsPresent.Count) do begin
                              v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                              if v[1] > Max then Max := v[1];
                           end;
                        end
                        else Max := 1;
                        for DEM := 0 to pred(DEMsPresent.Count) do begin
                           v[2] := CurrentY;
                           v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]) / Max;
                           BlockWrite(rfile[succ(DEM)],v,1);
                        end;
                     end
                     else begin
                        for DEM := 0 to pred(DEMsPresent.Count) do begin
                           v[2] := CurrentY;
                           v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                           BlockWrite(rfile[succ(DEM)],v,1);
                        end;
                      end;
                   end;
                   Dec(currentY,InterAreaSkip);
                end;
             end;

            for DEM := 1 to DEMsPresent.Count do begin
               CloseFile(rfile[DEM]);
            end;

            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
            Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
            Graph.Width := 1200;
            Graph.Height := 2400;
            Graph.GraphDraw.BottomMargin := 75;

            if What in [dgArea] then begin
               Graph.GraphDraw.HorizLabel := 'Average Ranking compared to reference ' + DEMType;
               Graph.GraphDraw.MinHorizAxis := 0.8;
               Graph.GraphDraw.MaxHorizAxis := 6.2;
            end;

            if (What in [dgPercentBest]) then begin
               Graph.GraphDraw.HorizLabel := 'Percentage of Criteria Where DEM is Best';
               Graph.GraphDraw.MinHorizAxis := 0;
               Graph.GraphDraw.MaxHorizAxis := 100;
            end;

            if (What in [dgAllValues]) then begin
               if WhatNow in [dgAllValues] then begin
                  Graph.GraphDraw.HorizLabel := 'Criterion Difference';
                  Graph.GraphDraw.MinHorizAxis := 0;
               end;
               Graph.GraphDraw.HorizLabel := LandType + '  ' + DEMType + '  ' + Graph.GraphDraw.HorizLabel;
            end;

            if What in [dgPick,dg7Params,dgNormalizedDiff] then begin
               Graph.GraphDraw.HorizLabel := RefFilter;
               if What in [dgNormalizedDiff] then begin
                  Graph.GraphDraw.MinHorizAxis := 0;
                  Graph.GraphDraw.MaxHorizAxis := 1.1;
               end
               else if Graph.GraphDraw.MinHorizAxis > 0 then Graph.GraphDraw.MinHorizAxis := 0;;
            end
            else if What in [dgAllValues] then begin
               Graph.GraphDraw.HorizLabel := ThisTile + ' Criterion Difference';
               Graph.GraphDraw.MinHorizAxis := 0;
            end
            else if What in [dgAllScores] then begin
               Graph.GraphDraw.HorizLabel := ThisTile + ' Criterion Ranking';
               Graph.GraphDraw.MinHorizAxis := 0.8;
               Graph.GraphDraw.MaxHorizAxis := 6.2;
            end;
            Graph.RedrawDiagram11Click(Nil);
            fName := NextFileNumber(MDTempDir,'graph_','.png');
            SaveImageAsBMP(Graph.Image1,fName);
            BigGraph.Add(fName);
            if (LegendfName = '') then begin
               LegendfName := NextFileNumber(MDTempDir,'legend_','.png');
               bmp := Graph.MakeLegend(Graph.GraphDraw.LegendList,false);
               SaveBitmap(bmp,LegendfName);
               Bmp.free;
            end;
         end {MakeGraphForFilter};

var
   J,k,ColsInBigGraph : integer;
begin {DEMIXwineContestMeanMedianGraph}
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestMeanMedianGraph in, table=' + IntToStr(DBontable) + '  what='  + IntToStr(What)); {$EndIf}

   if (CriteriaUsed = Nil) and (What in [dg7Params,dgNormalizedDiff,dgPick]) then begin
      fName := DEMIXSettingsDir;
      if not GetExistingFileName('criteria to plot','*.txt',fName) then exit;
      CriteriaUsed := tStringList.Create;
      CriteriaUsed.LoadFromFile(fName);
   end;

   if (DEMsPresent = Nil) then begin
      DEMsPresent := tStringList.Create;
      DEMsPresent.LoadFromFile(DEMIXSettingsDir + 'demix_dems_all_six.txt');
   end;

   if (AreaList = Nil) then begin
      AreaList := tStringList.Create;
      AreaList.LoadFromFile(DEMIXSettingsDir + 'demix_areas_sorted_by_lat.txt');
   end;

   if (LandTypePresent = Nil) then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      LandTypePresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   end;

   GISdb[DBonTable].EmpSource.Enabled := false;
   ColsInBigGraph := 3;

   if What in [dgAllValues,dgAllScores] then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      AllTiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
      ThisTile := GetFromList('DEMIX tile to plot',AllTiles);
      FreeAndNil(AllTiles);
   end;

   BigGraph := tstringList.Create;
   LegendfName := '';
   if What in [dgPercentBest] then begin
      MakeGraphForFilter('','','');
   end
   else begin
      for j := 1 to DEMsPresent.Count do begin
         if (What in [dgPick,dg7Params,dgNormalizedDiff]) then begin
            for k := 0 to pred(CriteriaUsed.Count) do MakeGraphForFilter(CriteriaUsed.Strings[k],LandTypePresent[1],DEMsPresent[pred(j)]);
         end
         else if (What in [dgAllValues,dgAllScores]) then begin
            what := dgAllValues; MakeGraphForFilter('',LandTypePresent[1],DEMsPresent[pred(j)]);
            what := dgAllScores; MakeGraphForFilter('',LandTypePresent[1],DEMsPresent[pred(j)]);
         end
         else if (What in [dgArea]) then begin
            MakeGraphForFilter('',LandTypePresent[1],DEMsPresent[pred(j)]);
         end;
      end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click graphs all made'); {$EndIf}

   if (LegendFName <> '') then BigGraph.Add(LegendfName);
   fName := NextFileNumber(MDtempDir,'demix_big_graph_','.png');
   MakeBigBitmap(BigGraph,'',fName,ColsInBigGraph);
   WMDEm.SetPanelText(1,'');
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}
end {DEMIXwineContestMeanMedianGraph};







