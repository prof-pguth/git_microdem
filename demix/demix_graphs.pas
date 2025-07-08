unit demix_graphs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

//{$Define ExDEMIXexperimentalOptions}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordDEMIX}
   //{$Define RecordDEMIX_OpenGraph}
   {$Define RecordDEMIX_evaluations_graph}
   {$Define TrackCriteriaList}
   //{$Define RecordDEMIX_criteria_colors}
   //{$Define RecordDEMIXWins}
   {$Define TrackColors}
   {$Define RecordAverageScoresGraph}
   {$Define RecordDEMIXGraph}
{$EndIf}


interface

uses
//needed for inline of core DB functions
   Petmar_db,
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end core DB functions definitions

    System.SysUtils,System.Classes,System.UITypes,
    VCL.ExtCtrls,VCL.Forms, VCL.Graphics, VCL.Controls,
    StrUtils,dbGrids,
    WinAPI.Windows,
    Petmar,Petmar_types,BaseGraf,
    DEMDefs;

procedure DEMIX_evaluations_graph(DBonTable,YAxisWhat : integer; DEMs,Criteria : tStringList; Evaluations : boolean = true);

procedure DEMIX_SSIM_FUV_GraphSettings(var Graph : tThisBaseGraph; DBonTable : integer; lltext : shortstring; Ncrits : integer);
procedure SSIM_FUV_scatterplot(db : integer; theCrit : shortstring);
procedure HistogramsAllCriteria(db : integer);
function WinningPercentagesComparedToCOP(db : integer; CompareDEM : shortstring; Criteria,inUseDEMs : tStringList; HLabel : shortstring = '') : tThisBaseGraph;
//procedure WinningPercentagesComparedToCOPFilteredBySlope(db : integer; CompareDEM : shortstring; Criteria,UseDEMs,GeomorphFilters : tStringList);

procedure WhiskerPlotsByCluster(DB : integer);

procedure BestBySlopeRough(dbOnTable : integer; Criteria : tStringList; Winners : boolean; Param1,Param2,ValueParam : shortstring);
procedure MultipleScatterPlotsForCluster(dbOnTable : integer);
procedure ScatterPlotForClusters(DBonTable : integer;  Field1,Field2 : shortstring);
function AverageScoresGraph(db : integer; DEMs : tStringList; HL : shortstring; aDEMIXLegend : boolean; MinHoriz,MaxHoriz : float64) : tThisBaseGraph;

procedure AddTopLabelToGraph(var Graph : tThisBaseGraph; DBonTable : integer);
function PlotBestEvalVersusPercentileMultipleCriteria(DBonTable : integer; Criteria : tStringList; Evaluations : boolean = true;
           TopLabel : shortstring = ''; HL : shortstring = ''; VertAxisField : shortstring = '') : tThisBaseGraph;
procedure FilterJustOneGraph(DBonTable : integer; Criteria,GeomorphFilters : tStringList; Evaluations : boolean = true; HL : shortstring = '');
procedure WinningComparedToBaseDEM(db : integer; BaseDEM : shortstring; GeomorphFilters,Criteria,DEms : tStringList);
procedure BestEvalGraphPerCriterionMultipleFilters(db : integer; GeomorphFilters,Criteria : tStringList; CriteriaFamily : shortstring);

procedure DEMIX_Area_ind_criteria_graph(DBonTable : integer; DEMs : tStringList);

function NumTilesString(DB : integer) : shortstring;


{$IfDef ExDEMIXexperimentalOptions}
{$Else}
   procedure DEMIX_AreaAverageScores_graph(DBonTable : integer; DEMs : tStringList; DoScores : boolean = true);

   function MakeHistogramOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;
   function GraphAverageScoresByTile(DB : integer; DEMs,TileList,CriteriaList : tStringList): tThisBaseGraph;
   procedure MultipleBestByParametersSortByValue(DBonTable,Option : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing,TileParameters : tStringList; ByPointFilters : boolean = false);
   //procedure BestDEMSbyCategory(DBonTable : integer);
   function DEMIX_SSIM_FUV_single_tile_graph(DBonTable : integer; tile : shortstring) :tThisBaseGraph;
   //function DEMIXwineContestScoresGraph(DBonTable : integer; XScalelabel : shortstring; MinHoriz : float32 = 0.5; MaxHoriz : float32 = 5.5) : tThisBaseGraph;

   procedure DEMIXMeanMedianHistograms(db : integer);
   procedure DEMIX_graph_best_in_Tile(DBonTable : integer; SortByArea : boolean);
   procedure ModeSTDPlot(DBonTable : integer);
   procedure DEMIXMeanMedianModeHistograms(db : integer);
   procedure DEMIXwineContestCriterionGraph(What,DBonTable : integer; AreaList : tStringList = nil; CriteriaUsed : tStringList = nil; LandTypePresent : tStringList = nil; DEMsPresent : tStringList = nil);
{$EndIf}


procedure MainGraphOptions(DBonTable : integer; UseDEMs,UseLSPs : tStringList; DesiredOption : integer = 0;  Tiles : tStringList = nil);
function FractionOfWinnersGraph(db : integer; TheDEMs,Criteria : tStringList) : tThisBaseGraph;

procedure FinishGraph(var Graph : tThisBaseGraph; var Findings : tStringList; AddLegends : boolean = false);

const
   TheLandtypes : array[1..5] of shortstring = ('AVG_SLOPE','AVG_ROUGH','BARREN_PC','FOREST_PC','URBAN_PC');
   TheBins : array[1..5] of integer = (10,3,0,0,0);


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   DEMIX_control,DEMIX_Definitions;


     procedure StartGraph(DBonTable : integer; var Graph : tThisBaseGraph; GraphWidth : integer = 700);
     begin
        Graph := tThisBaseGraph.Create(Application);
        Graph.Width := GraphWidth;
        Graph.GraphDraw.LLcornerText := GISdb[DBonTable].dbName;
        Graph.GraphDraw.VertGraphBottomLabels := false;
        Graph.GraphDraw.SetShowAllLines(true);
        Graph.GraphDraw.SetShowAllPoints(false);
     end;


     procedure FinishGraph(var Graph : tThisBaseGraph; var Findings : tStringList; AddLegends : boolean = false);
     var
        fName : PathStr;
        Bitmap : tMyBitmap;
     begin
        //{$IfDef TrackColors} Graph.GraphColorsRecord('FinishGraph enter'); {$EndIf}
        Graph.GraphDraw.ResetMargins := true;
        Graph.RedrawDiagram11Click(Nil);
        if AddLegends then Bitmap := Graph.AddLegendBesideGraph
        else CopyImageToBitmap(Graph.Image1,Bitmap);
        fName := NextFileNumber(MDtempDir,'finishgraph_','.bmp');
        Bitmap.SaveToFile(fName);
        if (Findings <> Nil) then Findings.Add(fName);
     end;


function FractionOfWinnersGraph(db : integer; TheDEMs,Criteria : tStringList) : tThisBaseGraph;
var
   Findings : tStringList;
   j,k,Tiles : integer;
   Criterion : shortstring;
   rfile : file;
   v : array[1..2] of float32;
begin
   StartGraph(db,Result);

   Result.GraphDraw.GraphLeftLabels := tStringList.Create;
   Result.GraphDraw.ShowGraphLeftLabels := true;
   for j := 1 to Criteria.Count do begin
      Criterion := NoSuffixCriterion(Criteria.Strings[pred(j)]);
     // Criterion := StringReplace(Criterion, '_FUV', '',[rfIgnoreCase]);
      Result.GraphDraw.GraphLeftLabels.Add(IntToStr(j) + ',' + Criterion);
   end;

   for j := 0 to pred(TheDEMs.Count) do begin
      Result.OpenDataFile(rfile,TheDEMs.Strings[j],ConvertPlatformColorToTColor(DEMIXColorFromDEMName(TheDEMs.Strings[j])));
      k := 0;
      GISdb[DB].MyData.First;
      while not GISdb[DB].MyData.eof do begin
         inc(k);
         Tiles := GISdb[DB].MyData.GetFieldByNameAsInteger('TILES');
         v[2] := k;
         v[1] := 100 * GISdb[DB].MyData.GetFieldByNameAsFloat(TheDEMs.Strings[j]) / Tiles;
         BlockWrite(rfile,v,1);
         GISdb[DB].MyData.Next;
      end;
      closeFile(rFile);
   end;

   Result.GraphDraw.HorizLabel := 'Winners (percent of tiles)';
   Result.AutoScaleAndRedrawDiagram;
   Result.GraphDraw.MinHorizAxis := -0.25;
   Result.GraphDraw.MaxHorizAxis := 100.25;
   Result.GraphDraw.MinVertAxis := 0.5;
   Result.GraphDraw.MaxVertAxis := Criteria.Count  + 0.5;
   Result.GraphDraw.LeftMargin := 90;
   Findings := nil;
   FinishGraph(Result,Findings);
end;


procedure MainGraphOptions(DBonTable : integer; UseDEMs,UseLSPs : tStringList; DesiredOption : integer = 0; Tiles : tStringList = nil);
var
   Findings : tStringlist;
   Criteria,Areas,Resolutions : tStringList;
   NumBigGraphCols,i,Tile : integer;
   ThisParam,LandType: shortstring;
   rfile : file;
   v : array[1..2] of float32;
   color : tColor;
   gr : array[1..25] of tThisBaseGraph;
   //OrderedFUVParams : tStringList;


     function DoMultipleResolutionGraph : tThisBaseGraph;
     const
        VertFUV : boolean = true;
     var
        i,j : integer;
        Criterion,TStr,TheDEM : shortstring;
        Evals : array[1..8] of float32;
        Color : tColor;
        DoIt : boolean;
     begin
         StartGraph(DBonTable,Result);
         Result.Width := 800;

         if VertFUV then begin
           Result.GraphDraw.MinVertAxis := -0.05;
           Result.GraphDraw.MaxVertAxis := 1.05;
           Result.GraphDraw.VertLabel := 'FUV';
           Result.GraphDraw.MinHorizAxis := 0.5;
           Result.GraphDraw.MaxHorizAxis := UseLSPs.Count  + 0.5;
           Result.GraphDraw.ShowGraphBottomLabels  := true;
           Result.GraphDraw.GraphBottomLabels := tStringList.Create;
           for j := 0 to pred(UseLSPs.Count) do begin
              Result.GraphDraw.GraphBottomLabels.Add(IntToStr(succ(j)) + ',' + UseLSPs[j]);
           end;
         end
         else begin
            Result.GraphDraw.MinHorizAxis := -0.05;
            Result.GraphDraw.MaxHorizAxis := 1.05;
            Result.GraphDraw.HorizLabel := 'FUV';
            Result.GraphDraw.MinVertAxis := 0.5;
            Result.GraphDraw.MaxVertAxis := UseLSPs.Count  + 0.5;
            Result.GraphDraw.ShowGraphLeftLabels := true;
            Result.GraphDraw.GraphLeftLabels := tStringList.Create;
            for j := 0 to pred(UseLSPs.Count) do begin
               Result.GraphDraw.GraphLeftLabels.Add(IntToStr(succ(j)) + ',' + UseLSPs[j]);
            end;
         end;

         GISdb[DBonTable].MyData.First;
         i := 0;
         while not GISdb[DBonTable].MyData.eof do begin
            inc(i);
            if GISdb[DBonTable].MyData.FieldExists('RESOLUTION') then begin
               TStr := 'RESOLUTION';
               Color := WinGraphColors(i);
               DoIt := true;
            end
            else begin
                TStr := 'DEM';
                TheDEM := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM');
                Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(TheDEM));
                DoIt := false;
                for i := 1 to NumDEMIXtestDEM do begin
                   if UpperCase(TheDEM) = DEMIXshort[i]  then DoIt := true;
                end;
                {$IfDef RecordDEMIX_criteria_colors} WriteLineToDebugFile(TheDEM + '  ' + Colorstring(Color)); {$EndIf}
            end;
            if DoIt then begin
                Result.OpenDataFile(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsString(TStr),Color);
                for j := 0 to pred(UseLSPs.Count) do begin
                   if GISdb[DBonTable].MyData.FieldExists(UseLSPs[j] + '_FUV') then begin
                     if VertFUV then begin
                        v[1] := succ(j);
                        v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(UseLSPs[j] + '_FUV');
                     end
                     else begin
                        v[2] := succ(j);
                        v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(UseLSPs[j] + '_FUV');
                     end;
                     BlockWrite(rfile,v,1);
                   end;
                end;
                closeFile(rFile);
            end;
            GISdb[DBonTable].MyData.Next;
         end;
         FinishGraph(Result,Findings);
     end;


    procedure FinishDifferenceDistributionGraph(Graph : tThisBaseGraph);
    var
       i : integer;
    begin
         Graph.GraphDraw.ShowGraphBottomLabels  := true;
         Graph.GraphDraw.GraphBottomLabels.Add('1,Mean');
         Graph.GraphDraw.GraphBottomLabels.Add('2,Median');
         for I := 1 to 5 do Graph.GraphDraw.GraphBottomLabels.Add(IntToStr(2+i) + ',' + ParamSuffixes[i] );
         Graph.GraphDraw.ShowHorizAxis0 := true;
         Graph.AutoScaleAndRedrawDiagram;
         Graph.GraphDraw.MinHorizAxis := 0.5;
         Graph.GraphDraw.MaxHorizAxis := 7.5;
         FinishGraph(Graph,Findings);
    end;


    function DoDifferenceDistributionGraph : tThisBaseGraph;
    var
        DEM : shortString;
        i : integer;
        rfile2 : file;
     begin
         StartGraph(DBonTable,Result);
         Result.GraphDraw.GraphBottomLabels := tStringList.Create;
         Result.GraphDraw.VertLabel := ThisParam + ' Difference';
         while not GISdb[DBonTable].MyData.eof do begin
            DEM := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM');
            Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM));
            //separate file for mean, median, to show break and emphasize their difference
            Result.OpenDataFile(rfile,'',Color);
            v[1] := 1;
            v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(ThisParam + '_MEAN');
            BlockWrite(rfile,v,1);
            v[1] := 2;
            v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(ThisParam + '_MED');
            BlockWrite(rfile,v,1);
            closeFile(rFile);
            Result.OpenDataFile(rfile2,'',Color);
            for I := 1 to 5 do begin
               v[1] := i + 2;
               v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(ThisParam + ParamSuffixes[i] );
               BlockWrite(rfile2,v,1);
            end;
            closeFile(rFile2);
            GISdb[DBonTable].MyData.Next;
         end;
         FinishDifferenceDistributionGraph(Result);
     end;


    function DoDifferenceDistributionGraphType2(ThisParam : shortstring) : tThisBaseGraph;
    var
        rfile2 : file;
        gName : PathStr;
        InitialFilter : shortstring;
        LandCovers : tStringList;

           function MakeOneGraph : tThisBaseGraph;
           var
              j,i : integer;
           begin
               StartGraph(DBonTable,Result);
               Result.GraphDraw.GraphBottomLabels := tStringList.Create;
               j := 0;
               while not GISdb[DBonTable].MyData.eof do begin
                  //separate file for mean, median, to show break and emphasize their difference
                  inc(j);
                  if (ThisParam = '') then begin
                     gName := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM');
                     Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(gName));
                     Result.GraphDraw.VertLabel := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION') + ' Difference';
                  end
                  else begin
                     gName := GISdb[DBonTable].MyData.GetFieldByNameAsString('LANDCOVER');
                     Color := WinGraphColors(j);
                     Result.GraphDraw.VertLabel := ThisParam + ' Difference';
                  end;
                  Result.OpenDataFile(rfile,'',Color);  //no name so no legend entry
                  v[1] := 1;
                  v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('MEAN');
                  BlockWrite(rfile,v,1);
                  v[1] := 2;
                  v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('MED');
                  BlockWrite(rfile,v,1);
                  closeFile(rFile);
                  GISdb[DBonTable].MyData.Next;
               end;

               j := 0;
               GISdb[DBonTable].MyData.First;
               while not GISdb[DBonTable].MyData.eof do begin
                  //separate file for unsigned differences
                  inc(j);
                  if (ThisParam = '') then begin
                     gName := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM');
                     Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(gName));
                  end
                  else begin
                     gName := GISdb[DBonTable].MyData.GetFieldByNameAsString('LANDCOVER');
                     Color := WinGraphColors(j);
                  end;
                  Result.OpenDataFile(rfile2,gname,Color);
                  for I := 1 to 5 do begin
                     v[1] := i + 2;
                     v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(ShortParamSuffixes[i]);
                     WriteLineToDebugFile(ThisParam + ' ' + gName + ' ' +  ShortParamSuffixes[i]  + RealToString(v[2],6,2));
                     BlockWrite(rfile2,v,1);
                  end;
                  closeFile(rFile2);
                  GISdb[DBonTable].MyData.Next;
               end;
               Result.Caption := 'Difference distributions';
               Result.GraphDraw.LLcornerText := GISdb[DBonTable].MyData.Filter;
               FinishDifferenceDistributionGraph(Result);
           end;

     const
        TheCrits : array[1..3] of shortstring = ('ELEV','SLOPE','RUFF');
     var
        I : integer;
     begin {DoDifferenceDistributionGraphType2}
        InitialFilter := GISdb[DBonTable].MyData.Filter;
        if true then begin
            for i := 1 to 3 do begin
             ThisParam := TheCrits[i];
             GISdb[DBonTable].ApplyGISFilter('DEM=' + QuotedStr('NEO_DTM') + ' AND CRITERION=' + QuotedStr(TheCrits[i]));
              MakeOneGraph;
            end;
         end
         else begin
             if (ThisParam <> '') then GISdb[DBonTable].ApplyGISFilter('PARAMETER=' + QuotedStr(ThisParam));
             Landcovers := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('LANDTYPE');
             for I := 0 to pred(LandCovers.Count) do begin
                GISdb[DBonTable].ApplyGISFilter(AddAndIfNeeded(InitialFilter) + 'LANDTYPE=' + QuotedStr(LandCovers.Strings[i]));
                MakeOneGraph;
             end;
             LandCovers.Destroy;

         end;
         GISdb[DBonTable].ApplyGISFilter(InitialFilter);
     end {DoDifferenceDistributionGraphType2};


     procedure DoWindowSensitivityGraph;
     var
        theWindows : tStringList;
        i : integer;

        function MakeGraph(LSP : shortstring) : tThisBaseGraph;
        var
           j,k : integer;
           rfile : file;
           af : shortstring;
           Graph : tThisBaseGraph;
           v : array[1..2] of float32;
        begin
           StartGraph(DBonTable,Graph);
           Graph.GraphDraw.GraphBottomLabels := tStringList.Create;
           Graph.GraphDraw.ShowGraphBottomLabels := true;
           for j := 1 to theWindows.Count do Graph.GraphDraw.GraphBottomLabels.Add(IntToStr(j) + ',' + theWindows.Strings[pred(j)]);

           for j := 0 to pred(UseDEMs.Count) do begin
              Graph.OpenDataFile(rfile,UseDEMs.Strings[j],ConvertPlatformColorToTColor(DEMIXColorFromDEMName(UseDEMs.Strings[j])));
              af := 'LSP=' + QuotedStr(LSP) + ' AND DEM=' + QuotedStr(UseDEMs.Strings[j]);
              GISdb[DBonTable].ApplyGISFilter(af);
              k := 0;
              while not GISdb[DBonTable].MyData.eof do begin
                 inc(k);
                 v[1] := k;
                 v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('FUV');
                 BlockWrite(rfile,v,1);
                 GISdb[DBonTable].MyData.Next;
              end;
              closeFile(rFile);
           end;
           Graph.GraphDraw.VertLabel := 'FUV';
           Graph.GraphDraw.LLcornerText := LSP;
           Graph.AutoScaleAndRedrawDiagram;
           Graph.GraphDraw.MinVertAxis := -0.05;
           Graph.GraphDraw.MaxVertAxis := 1.05;
           Graph.GraphDraw.MinHorizAxis := 0.5;
           Graph.GraphDraw.MaxHorizAxis := TheWindows.Count  + 0.5;
           {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile(LSP + ' MakeGraph Point 4: ' + Graph.GraphDraw.AxisRange); {$EndIf}
           FinishGraph(Graph,Findings);
           {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile(LSP + ' MakeGraph Point 5: ' + Graph.GraphDraw.AxisRange); {$EndIf}
        end;

     begin
        {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile('DoWindowSensitivityGraph in'); {$EndIf}
        for i := 0 to pred(UseLSPs.Count) do begin
           GISdb[DBonTable].ApplyGISFilter('LSP=' + QuotedStr(UseLSPs.Strings[i]));
           theWindows := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('WINDOW',false);
           MakeGraph(UseLSPs.Strings[i]);
           theWindows.Destroy;
        end;
        GISdb[DBonTable].ClearGISFilter;
        {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile('DoWindowSensitivityGraph out'); {$EndIf}
     end;

const
   goLandcover = 1;
   goTileGeomorph = 2;
   goOther = 3;

     function DoFUVgraph(GraphOption : integer) : tThisBaseGraph;
     var
        i,j,n : integer;
        Criterion,DEM,aFilter,TStr: shortstring;
        Evals : array[1..25] of float32;
        AverageFUV : float64;
        Labels,GeomorphFilters,
        LandCovers : tstringList;
        Legend : tMyBitmap;
        fName : PathStr;
        BigBig : tStringList;


            procedure EndThisFUVGraph(Graph : tThisBaseGraph; Findings : tStringList; AddLegend : boolean = false);
            begin
               Graph.GraphDraw.VertLabel := 'FUV';
               if (Graph.GraphDraw.LLcornerText = '') then Graph.GraphDraw.LLcornerText := 'DEMIX tile: ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
               Graph.AutoScaleAndRedrawDiagram;
               Graph.GraphDraw.MinVertAxis := -0.05;
               Graph.GraphDraw.MaxVertAxis := 1.05;
               Graph.GraphDraw.MinHorizAxis := 0.5;
               Graph.GraphDraw.MaxHorizAxis := UseLSPs.Count  + 0.5;
               FinishGraph(Graph,Findings,AddLegend);
            end;

            function InsureFUVinLSPname(aName : shortstring) : shortstring;
            begin
                if StrUtils.AnsiContainsText(aName,'_FUV') then Result := aName
                else Result := aName + '_FUV';
            end;

            function MakeFUVgraphOneLandCover : tThisBaseGraph;
            var
               i,j : integer;
            begin
               StartGraph(DBonTable,Result,MDdef.DEMIX_FUV_graph_width);
               Result.GraphDraw.GraphBottomLabels := tStringList.Create;
               Result.GraphDraw.ShowGraphBottomLabels := true;
               GISdb[DBonTable].MyData.First;
               i := 0;
               while not GISdb[DBonTable].MyData.eof do begin
                  inc(i);
                  if (UseDEMs <> nil) then begin
                     DEM := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM');
                     Result.OpenDataFile(rfile,DEM,ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM)));
                  end
                  else begin
                     Result.OpenDataFile(rfile,DEM,WinGraphColors(i));
                  end;
                  for j := 1 to UseLSPs.Count do begin
                     v[1] := j;
                     v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(InsureFUVinLSPname(UseLSPs.Strings[pred(j)]));
                     BlockWrite(rfile,v,1);
                     if (i = 1) then begin
                        Result.GraphDraw.GraphBottomLabels.Add(IntToStr(j) + ',' + UseLSPs.Strings[pred(j)]);
                     end;
                  end;
                  GISdb[DBonTable].MyData.Next;
                  closeFile(rFile);
               end;
               Result.GraphDraw.LLCornerText := GISdb[DBonTable].MyData.Filter;
               EndThisFUVGraph(Result,Findings);
            end;


         function TileGeomorphGraph(LandType,DEM : shortstring; Bin : integer = 0) : tThisBaseGraph;
         //x-axis will be the criteria, y-axis the FUV
         //colors will be for the geomorph filters which are set for the landtype
         //will be done for one DEM
         var
            i,j : integer;
         begin
            if not GISdb[DBonTable].MyData.FieldExists(LandType) then begin
               MessageToContinue('Missing field ' + LandType);
               exit;
            end;
            {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile('TileGeomorphGraph in, DEM= ' + DEM + '  terrain=' + LandType); {$EndIf}
            StartGraph(DBonTable,Result,MDdef.DEMIX_FUV_graph_width);
            Result.GraphDraw.GraphBottomLabels := tStringList.Create;
            Result.GraphDraw.ShowGraphBottomLabels := true;
            Result.GraphDraw.LegendList := tStringList.Create;
            Result.GraphDraw.LLcornerText := 'DEM=' + DEM + ' Terrain=' + LandType;
            MakeLandParamFilters(LandType,GeomorphFilters,Labels,Nil,Bin);
            for i := 0 to pred(GeomorphFilters.Count) do begin
               Color := WinGraphColors(i);
               Result.OpenDataFile(rfile,GeomorphFilters[i],Color);
               wmdem.SetPanelText(2, IntToStr(succ(i)) + '/' + IntToStr(GeomorphFilters.Count) + '  ' + GeomorphFilters[i],true);
               aFilter := AddAndIfNeeded(GeomorphFilters[i]) + 'CRITERION=' + QuotedStr(InsureFUVinLSPname(UseLSPs.Strings[0]));
               GISdb[DBonTable].ApplyGISFilter(aFilter);
               GISdb[DBonTable].EmpSource.Enabled := false;
               TStr := Labels.Strings[i] + NumTilesString(DBonTable);
               Result.GraphDraw.LegendList.Add(TStr);
               {$If Defined(RecordDEMIXGraphFull)} WriteLineToDebugFile(TStr); {$EndIf}
               for j := 0 to pred(UseLSPs.Count) do begin
                  wmdem.SetPanelText(1, IntToStr(succ(j)) + '/' + IntToStr(UseLSPs.Count) + '  ' + UseLSPs[j],true);
                  aFilter := AddAndIfNeeded(GeomorphFilters[i]) + 'CRITERION=' + QuotedStr(InsureFUVinLSPname(UseLSPs.Strings[j]));
                  GISdb[DBonTable].ApplyGISFilter(aFilter);
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
                     if MDDef.DEMIX_UseMedian then AverageFUV := GISdb[DBonTable].MyData.FieldMedian(DEM)
                     else AverageFUV := GISdb[DBonTable].MyData.FieldAverage(DEM);
                     {$If Defined(RecordDEMIXGraphFull)} WriteLineToDebugFile(RealToString(AverageFUV,6,4) + '  ' + aFilter); {$EndIf}
                     v[1] := succ(j);
                     v[2] := AverageFUV;
                     BlockWrite(rfile,v,1);
                     Result.GraphDraw.GraphBottomLabels.Add(IntToStr(succ(j)) + ',' + UseLSPs.Strings[j]);
                  end
                  else begin
                     {$If Defined(RecordDEMIXGraphFull)} WriteLineToDebugFile('Nothing: ' + aFilter); {$EndIf}
                  end;
               end;
               CloseFile(rFile);
            end;
            {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile(DEM + '  ' + LandType + ' call EndThisFUVGraph'); {$EndIf}
            EndThisFUVGraph(Result,Findings,false);
            Result.Caption := 'Criteria with slope categories for ' + DEM;
            {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile(DEM + '  ' + LandType + ' out'); {$EndIf}
         end {TileGeomorphGraph};


         function TileGeomorphGraphByDEM(LandType : shortstring; TheDEMs : tStringList; Bin : integer = 0) : tThisBaseGraph;
         //x-axis will be the criteria, y-axis the FUV
         //colors will be for the DEMs
         //will do graphs for each value of the LandType

             function GraphForOneLandTypeFilter(GeomorphFilter,aLabel : shortstring; Findings : tStringList) : tThisBaseGraph;
             var
                j,k : integer;
             begin
                {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile(' Enter GraphForOneLandTypeFilter, label= ' + aLabel); {$EndIf}
                StartGraph(DBonTable,Result,MDdef.DEMIX_FUV_graph_width);
                Result.GraphDraw.GraphBottomLabels := tStringList.Create;
                Result.GraphDraw.ShowGraphBottomLabels := true;
                Result.GraphDraw.LegendList := tStringList.Create;
                Result.GraphDraw.LLcornerTextAtEdge := false;
                Result.GraphDraw.VertGraphBottomLabels := (UseLSPs.Count > 15);
                if Result.GraphDraw.VertGraphBottomLabels then Result.GraphDraw.BottomMargin := 110;

                for j := 0 to pred(UseLSPs.Count) do begin
                    Result.GraphDraw.GraphBottomLabels.Add(IntToStr(succ(j)) + ',' + UseLSPs.Strings[j]);
                end;
                GISdb[DBonTable].ApplyGISFilter(GeomorphFilter);
                GISdb[DBonTable].EmpSource.Enabled := false;
                Result.GraphDraw.LLcornerText := aLabel + NumTilesString(DBonTable);

                for k := 0 to pred(theDEMs.Count) do begin
                  Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(theDEMs.strings[k]));
                  Result.OpenDataFile(rfile,theDEMs.strings[k],Color);
                  wmdem.SetPanelText(2, IntToStr(succ(k)) + '/' + IntToStr(theDEMs.Count) + '  ' + theDEMs[k],true);
                  for j := 0 to pred(UseLSPs.Count) do begin
                     aFilter := AddAndIfNeeded(GeomorphFilter) + 'CRITERION=' + QuotedStr(InsureFUVinLSPname(UseLSPs.Strings[j]));
                     GISdb[DBonTable].ApplyGISFilter(aFilter);
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile(aFilter + '  ' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
                     if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
                        if MDDef.DEMIX_UseMedian then AverageFUV := GISdb[DBonTable].MyData.FieldMedian(theDEMs.strings[k])
                        else AverageFUV := GISdb[DBonTable].MyData.FieldAverage(theDEMs.strings[k]);
                       //AverageFUV := GISdb[DBonTable].MyData.FieldAverage(theDEMs.strings[k]);
                       {$If Defined(RecordDEMIXGraphFull)} WriteLineToDebugFile(RealToString(AverageFUV,6,4) + '  ' + aFilter); {$EndIf}
                       v[1] := succ(j);
                       v[2] := AverageFUV;
                       BlockWrite(rfile,v,1);
                     end
                     else begin
                       {$If Defined(RecordDEMIXGraphFull)} WriteLineToDebugFile('Nothing: ' + aFilter); {$EndIf}
                     end;
                 end;
                 CloseFile(rFile);
                 Result.Caption := 'Criteria with terrain categories for ' + DEM;
                end;
                EndThisFUVGraph(Result,Findings,false);
             end;

         var
            i,j,k,Start,PixWide : integer;
            gr : tGraphArray;
            fName : PathStr;
            aFilter,aLabel : ShortString;
            Bitmap,BigBitmap : tMyBitmap;
            Filters1,Filters2,Labels1,Labels2 : tStringList;
         begin {TileGeomorphGraphByDEM}
            {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile('TileGeomorphGraph in, DEM= ' + DEM + '  terrain=' + LandType); {$EndIf}
            if (DesiredOption = 1) then begin
                MakeLandParamFilters(LandType,GeomorphFilters,Labels,Nil,Bin);
                for i := 0 to pred(GeomorphFilters.Count) do begin
                   wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(GeomorphFilters.Count) + '  ' + Labels[i],true);
                   gr[i] := GraphForOneLandTypeFilter(GeomorphFilters.strings[i],Labels[i],Nil);
                end;
                Findings.Add(MergeGraphPanelsHorizontal(GeomorphFilters.Count,gr));
            end
            else if (DesiredOption = 3) then begin
               k := 0;
               ImportLandParamFilters(DEMIXSettingsDir + 'avg_slope_filters.dbf',Filters1,Labels1);
               ImportLandParamFilters(DEMIXSettingsDir + 'barren_pc_filters.dbf',Filters2,Labels2);
               for I := 0 to pred(Filters1.count) do begin
                  k := 0;
                  for j := 0 to pred(Filters2.count) do begin
                     aFilter := Filters1.Strings[i] + ' AND ' + Filters2.Strings[j];
                     aLabel := Labels1.Strings[i] + ' AND ' + Labels2.Strings[j];
                     wmdem.SetPanelText(3, IntToStr(succ(k)) + '/' + IntToStr(Filters1.Count * Filters2.Count) + '  ' + aLabel,true);
                     gr[k] := GraphForOneLandTypeFilter(aFilter,aLabel,Nil);
                     inc(k);
                  end;
                  Findings.Add(MergeGraphPanelsHorizontal(Filters2.Count,gr));
               end;
               Filters1.Destroy;
               Filters2.Destroy;
               Labels1.Destroy;
               Labels2.Destroy;
            end;
            {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile(DEM + '  ' + LandType + ' out'); {$EndIf}
         end {TileGeomorphGraphByDEM};


         procedure AddLegend;
         var
            Legend,bmp : tMyBitmap;
         begin
             Legend := gr[1].MakeLegend;
             CreateBitmap(bmp,gr[1].Width,Legend.Height + 10);
             bmp.Canvas.Draw((bmp.Width - Legend.Width) div 2,5,Legend);
             fName := NextFileNumber(MDtempDir,'gotilegeomorph_legend_','.bmp');
             bmp.SaveToFile(fName);
             Findings.Add(fName);
             Legend.Destroy;
             bmp.Destroy;
             //BigBig.Add(fName);
         end;

     var
        bmp : tMyBitmap;
        lName : PathStr;
     begin {DoFUVgraph}
         {$If Defined(RecordDEMIXGraph)} writeLineToDebugFile('DoFUVGraph in, graphoption=' + IntToStr(GraphOption)); {$EndIf}
         if (GraphOption = goLandcover) then begin
            //{$IfDef TrackColors} Graph.GraphColorsRecord('ItsLandcover start'); {$EndIf}
            GISdb[DBonTable].EmpSource.Enabled := false;
            if GISdb[DBonTable].MyData.FieldExists('LANDCOVER') then Landcovers := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('LANDCOVER');
            if GISdb[DBonTable].MyData.FieldExists('DEM') and (UseDEMs.Count > 1) then begin
               for i := 0 to pred(LandCovers.Count) do begin
                  GISdb[DBonTable].ApplyGISFilter('LANDCOVER=' + QuotedStr(LandCovers.Strings[i]));
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  MakeFUVgraphOneLandCover;
               end;
            end
            else begin
                n := 0;
                StartGraph(DBonTable,Result);
                Result.GraphDraw.GraphBottomLabels := tStringList.Create;
                Result.GraphDraw.ShowGraphBottomLabels := true;
                Result.Width := 800;
                Result.GraphDraw.LegendList := tStringList.Create;
                for i := 0 to pred(UseLSPs.Count) do
                   Result.GraphDraw.LegendList.Add(UseLSPs.Strings[i]);

                Result.GraphDraw.DataFilesPlotted := tStringList.Create;
                GISdb[DBonTable].MyData.First;
                while not GISdb[DBonTable].MyData.eof do begin
                   inc(n);
                   Color := WinGraphColors(n);
                   Result.OpenDataFile(rfile,GISdb[DBonTable].MyData.GetFieldByNameAsString('LANDCOVER'),Color);
                   for j := 1 to UseLSPs.Count do begin
                      Result.GraphDraw.GraphBottomLabels.Add(IntToStr(j) + ',' + UseLSPs.Strings[pred(j)]);
                      v[1] := j;
                      v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(InsureFUVinLSPname(UseLSPs.Strings[pred(j)]));
                      BlockWrite(rfile,v,1);
                   end;
                   GISdb[DBonTable].MyData.Next;
                   closeFile(rFile);
                end;
                EndThisFUVGraph(Result,Findings);
            end;
            LandCovers.Destroy;
            //{$IfDef TrackColors} Graph.GraphColorsRecord('ItsLandcover done'); {$EndIf}
         end
         else if (GraphOption = goOther) then begin
            StartGraph(DBonTable,Result,MDdef.DEMIX_FUV_graph_width);
            Result.GraphDraw.GraphBottomLabels := tStringList.Create;
            Result.GraphDraw.ShowGraphBottomLabels := true;
            Result.GraphDraw.LLcornerTextAtEdge := false;
            //Result.Width := 800;

            for i := 1 to NumDEMIXtestDEM do begin
               if GISdb[DBonTable].MyData.FieldExists(DEMIXShort[i]) then begin
                  {$If Defined(RecordDEMIXGraph)} writeLineToDebugFile(DEMIXShort[i]); {$EndIf}
                  Color := ConvertPlatformColorToTColor(DEMIXDEMcolors[i]);
                  Result.OpenDataFile(rfile,DEMIXShort[i],Color);
                  GISdb[DBonTable].MyData.First;
                  while not GISdb[DBonTable].MyData.eof do begin
                     Criterion := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
                     Criterion := StringReplace(Criterion, '_FUV', '',[rfIgnoreCase]);
                     for j := 1 to UseLSPs.Count do begin
                        if (Criterion = UseLSPs.Strings[pred(j)]) then begin
                           //writeLineToDebugFile('i=' + IntToStr(i) +  '  j=' + IntToStr(j));
                           Evals[j] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
                           {$If Defined(RecordDEMIXGraph)} writeLineToDebugFile(Criterion + '  ' + RealToString(Evals[j],-12,-4)); {$EndIf}
                        end;
                     end;
                     GISdb[DBonTable].MyData.Next;
                  end;

                  for j := 1 to UseLSPs.Count do begin
                      if (i = 1) then begin
                         Result.GraphDraw.GraphBottomLabels.Add(IntToStr(j) + ',' + UseLSPs.Strings[pred(j)]);
                      end;
                      v[1] := j;
                      v[2] := Evals[j];
                      BlockWrite(rfile,v,1);
                  end;
                  closeFile(rFile);
               end;
            end;
         end
         else if (GraphOption = goTileGeomorph) then begin
            NumBigGraphCols := 1;
            BigBig := tStringList.Create;
            if (DesiredOption = 2) then begin
                {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile('Start GraphOption = goTileGeomorph, Loop terrain, then DEMs'); {$EndIf}
                for j := 1 to 5 do begin  //theLandTypes
                   for i := 1 to UseDEMs.Count do begin
                      wmdem.SetPanelText(3,IntToStr(i) + '/' + IntToStr(UseDEMs.Count) + '  ' + UseDEMs[pred(i)],true);
                      gr[i] := TileGeomorphGraph(TheLandTypes[j],UseDEMs[pred(i)],TheBins[j]);
                   end;
                   AddLegend;
                   fName := NextFileNumber(MDtempDir,TheLandTypes[j] + '_landtypes_criteria_DEM','.png');
                   MakeBigBitmap(Findings,'',fName,1);
                   BigBig.Add(fName);
                   Findings := tStringList.Create;
                end;
                fName := NextFileNumber(MDtempDir,'fuv_terrain_dems_','.png');
                MakeBigBitmap(BigBig,'',fName,12);
            end
            else if (DesiredOption in [1,3]) then begin
                TileGeomorphGraphByDEM('AVG_SLOPE',UseDEMs,15);
                fName := NextFileNumber(MDtempDir,'fuv_dems_','.png');
                if (DesiredOption = 3) then j := 1 else j := 4;
                bmp := DEMIXTestDEMLegend(true,UseDEMs);
                lName := NextFileNumber(MDtempDir,'dem_legend_','.bmp');
                bmp.SaveToFile(lName);
                bmp.Destroy;
                MakeBigBitmap(Findings,'',fName,j,lName);
                Findings := tStringList.Create;
            end;
            {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile('Done GraphOption = goTileGeomorph'); {$EndIf}
         end;
     end {DoFUVgraph};


var
   Criteria2 : tStringList;
   Criterion : shortstring;
begin {procedure MainGraphOptions}
   if ValidDB(DBonTable) then begin
      if not TileCharacteristicsInDB(DBOnTable) then begin
         MessageToContinue('Add tile characteristics to DB');
         exit;
      end;
      {$If Defined(TrackCriteriaList)} TrackCriteriaList(UseLSPs,'Enter MainGraphOptions'); {$EndIf}

      //OrderedFUVParams := OpenDEMIXOrderedCriteria(dbOnTable);

      if (UseDEMs = Nil) then begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         if GISdb[DBonTable].MyData.FieldExists('DEM') then UseDEMs := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEM')
         else UseDEMs := GetListOfTestDEMsinUse;
      end;
      if (UseLSPs = Nil) then begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         UseLSPs := OpenDEMIXOrderedCriteria(DBonTable);
      end
      else begin
          if GISdb[DBonTable].MyData.FieldExists('CRITERION') then begin
             GISdb[DBonTable].EmpSource.Enabled := false;
             Criteria2 := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('CRITERION');
             for i := pred(UseLSPs.Count) downto 0 do begin
                Criterion := UseLSPs.Strings[i] + '_FUV';
                if Criteria2.IndexOf(Criterion) = -1 then begin
                   UseLSPs.Delete(i);
                end;
             end;
             Criteria2.Destroy;
          end
          else begin
            for i := pred(UseLSPs.Count) downto 0 do begin
              Criterion := UseLSPs.Strings[i] + '_FUV';
              if not GISdb[DBonTable].MyData.FieldExists(Criterion) then begin
                 UseLSPs.Delete(i);
              end;
            end;
          end;
      end;
     if (UseLSPs.Count = 0) then begin
        MessageToContinue('No matching criteria in file');
        exit;
     end
     else begin
        {$If Defined(TrackCriteriaList)} TrackCriteriaList(UseLSPs,'after checks, MainGraphOptions'); {$EndIf}
     end;

      {$If Defined(RecordDEMIXGraph)} HighlightLineToDebugFile('MainGraphOptions in, DesiredOption=' + IntToStr(DesiredOption)); {$EndIf}
      GISdb[DBonTable].EmpSource.Enabled := false;
      Findings := tStringList.Create;
      GetDEMIXpaths(True);
      NumBigGraphCols := 4;
      if (DesiredOption > 0) then begin
         if (DesiredOption in [1,2,3]) then begin
           DoFUVGraph(goTileGeomorph);
         end;
      end
      else begin
          if GISdb[DBonTable].MyData.FieldExists('RMSE') then begin
             //this is difference distribution file
             for i := 1 to 3 do begin
                //find out which parameter it is
                ThisParam := DiffParams[i];
                if GISdb[DBonTable].MyData.FieldExists(ThisParam + ParamSuffixes[1]) then break
                else ThisParam := '';
             end;

             if (ThisParam = '') then begin
                //this is a new DB, with multiple parameters
                DoDifferenceDistributionGraphType2('');
             end
             else begin
                Tiles := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
                for Tile := 0 to pred(Tiles.Count) do begin
                   GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tiles.Strings[Tile]));
                   DoDifferenceDistributionGraph;
                end;
                Tiles.Destroy;
             end;
          end
          else if GISdb[DBonTable].MyData.FieldExists('LSP') and GISdb[DBonTable].MyData.FieldExists('WINDOW') then begin
             DoWindowSensitivityGraph;
          end
          else if GISdb[DBonTable].MyData.FieldExists('RESOLUTION') then begin
             //This is range of scales file
             Areas := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('AREA');
             Resolutions := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('RESOLUTION');
             DoMultipleResolutionGraph;
             Areas.Destroy;
          end
          else if GISdb[DBonTable].MyData.FieldExists('CRITERION') then begin //this is FUV file
             Tiles := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
             for Tile := 0 to pred(Tiles.Count) do begin
                GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tiles.Strings[Tile]));
                DoFUVGraph(goOther);
             end;
          end
          else if GISdb[DBonTable].MyData.FieldExists('LANDCOVER') then begin //this is FUV file based on landcover
             DoFUVGraph(goLandcover);
          end
          else if StrUtils.AnsiContainsText(UpperCase(GISdb[DBonTable].dbName),'FUV') then begin
             DoMultipleResolutionGraph;
          end
          else if GISdb[DBonTable].MyData.FieldExists('CRITERION') then begin
             DoDifferenceDistributionGraphType2('ELVD');
             DoDifferenceDistributionGraphType2('SLPD');
             DoDifferenceDistributionGraphType2('RUFD');
          end;
      end;

      if (Findings.Count = 0) then Findings.Destroy
      else begin
         MakeBigBitmap(Findings,'','',NumBigGraphCols);
      end;
      EndDEMIXProcessing(dbOnTable);
      {$If Defined(RecordDEMIXGraph)} HighlightLineToDebugFile('MainGraphOptions '); {$EndIf}
   end;
   UseDEMs.Destroy;
   UseLSPs.Destroy;
   //UseLSPs.Destroy;

end {procedure MainGraphOptions};



function NumTilesString(DB : integer) : shortstring;
begin
   Result := ' (tiles=' + IntToStr(GISdb[DB].NumUniqueEntriesInDB('DEMIX_TILE')) + ')';
end;


procedure AddTopLabelToGraph(var Graph : tThisBaseGraph; DBonTable : integer);
begin
(*
   Graph.GraphDraw.TopLabel := GISdb[DBontable].dbName  +  ' (' + DateToStr(Now) + ')';
   if (GISdb[DBonTable].MyData.Filter <> '') then begin
      Graph.GraphDraw.TopLabel := Graph.GraphDraw.TopLabel  + '  ' +  GISdb[DBontable].MyData.Filter + ' (n=' + IntToStr(GISdb[DBontable].MyData.FiltRecsInDB) +
                       '/' + IntToStr(GISdb[DBontable].MyData.TotRecsInDB) + ')';
   end;
   Graph.GraphDraw.TopMargin := 45;
*)
end;


procedure BestEvalGraphPerCriterionMultipleFilters(db : integer; GeomorphFilters,Criteria : tStringList; CriteriaFamily : shortstring);
var
   i,j : integer;
   HL,TopLabel : shortstring;
   fName : PathStr;
   gr :  tGraphArray;
   Legend,BigBitmap : tMyBitmap;
begin
   if TileCharacteristicsInDB(DB) then begin
      {$If Defined(RecordDEMIXGraph) or Defined(RecordBestEval)} WriteLineToDebugFile('BestEvalGraphPerCriterionMultipleFilters in'); {$EndIf}
      GetDEMIXpaths(true);
      try
         for i := 0 to pred(GeomorphFilters.Count) do begin //filters to tile characteristics
            {$If Defined(RecordBestEval)} WriteLineToDebugFile(GeomorphFilters.Strings[i]); {$EndIf}
            if GeomorphFilters.Strings[i] = '(None)' then GISdb[db].ClearGISFilter
            else GISdb[db].ApplyGISFilter(GeomorphFilters.Strings[i]);
            HL := CriteriaFamily + '_Evaluation';
            if MDDef.DEMIX_AllowCoastal then HL := DEMIXModeName + '_' + HL;
            TopLabel := GISdb[db].MyData.Filter + NumTilesString(DB);
            gr[i] := PlotBestEvalVersusPercentileMultipleCriteria(DB,Criteria,true,TopLabel,HL);
            //if MDDef.DEMIX_combined_graph then AddGraphToBigBitmap(succ(i),GeomorphFilters.Count,1,gr[i],BigBitmap);
         end;
         //if MDDef.DEMIX_combined_graph then begin
      BigBitmap := tMyBitmap.Create;
      BigBitmap.LoadFromFile(MergeGraphPanelsHorizontal(GeomorphFilters.Count,gr));
      Legend := gr[0].MakeLegend;
      Fname := NextFileNumber(MDtempDir,'FilterJustOneGraph_','.png');
      FinishBigBitMapWithLegend(BigBitmap,Legend,fName);



(*
      BigBitmap := tMyBitmap.Create;
      BigBitmap.LoadFromFile(MergeGraphPanels(GeomorphFilters.Count,gr));
      //if MDDef.DEMIX_combined_graph then begin
         Legend := gr[0].MakeLegend;
         Fname := NextFileNumber(MDtempDir,'FilterJustOneGraph_','.png');
         FinishBigBitMapWithLegend(BigBitmap,Legend,fName);
      //end;

             {$If Defined(RecordBestEval)} WriteLineToDebugFile('Combined graph'); {$EndIf}
(*
            Legend := gr[0].MakeLegend;
            fName := NextFileNumber(MDtempDir,'BestEvalGraphPerCriterionMultipleFilters_','.png');
            FinishBigBitMapWithLegend(BigBitmap,Legend,fName,true);
         //end;
*)
      finally
         EndDEMIXProcessing(db);
      end;
      {$If Defined(RecordDEMIXGraph)} WriteLineToDebugFile('BestEvalGraphPerCriterionMultipleFilters out'); {$EndIf}
   end
   else MessageToContinue('Add tile characters to DB');
end;


procedure WinningComparedToBaseDEM(db : integer; BaseDEM : shortstring; GeomorphFilters,Criteria,DEms : tStringList);
var
   i,j : integer;
   HL : shortstring;
   fName : PathStr;
   gr :  tGraphArray;
   Legend,BigBitmap : tMyBitmap;
begin
   if TileCharacteristicsInDB(DB) then begin
      try
         GetDEMIXpaths(False);
         j := 0;
         for i := 0 to pred(GeomorphFilters.Count) do begin
            GISdb[db].ApplyGISFilter(GeomorphFilters.Strings[i]);
            if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
               HL := GISdb[DB].MyData.Filter + NumTilesString(DB);
               inc(j);
               gr[j] := WinningPercentagesComparedToCOP(db,BaseDEM,Criteria,DEMs,HL);
               //if MDDef.DEMIX_combined_graph then AddGraphToBigBitmap(j,GeomorphFilters.Count,1,gr[j],BigBitmap);
            end;
         end;

         BigBitmap.LoadFromFile(MergeGraphPanelsHorizontal(GeomorphFilters.Count,gr));
         Legend := gr[0].MakeLegend;
         Fname := NextFileNumber(MDtempDir,'FilterJustOneGraph_','.png');
         FinishBigBitMapWithLegend(BigBitmap,Legend,fName);


(*
         if MDDef.DEMIX_combined_graph then begin
            Legend := gr[1].MakeLegend;
            fName := NextFileNumber(MDtempDir,'Winning_compared_','.png');
            FinishBigBitMapWithLegend(BigBitmap,Legend,fname);
         end;
*)
      finally
         EndDEMIXProcessing(db);
         //GISdb[db].ClearGISFilter;
         //GISdb[db].ShowStatus;
      end;
   end
   else MessageToContinue('Add tile characters to DB');
end;


procedure WhiskerPlotsByCluster(DB : integer);
var
   Criteria : tStringList;
   Filters : tStringList;
   pn,i : integer;
begin
   try
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('WhiskerPlotsByCluster in'); {$EndIf}
      GetDEMIXpaths(False);
      GISdb[DB].EmpSource.Enabled := false;
      Criteria := GISdb[DB].MyData.ListUniqueEntriesInDB('CRITERION');
      if (Criteria.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
         for i := 0 to Pred(Criteria.Count) do begin
            GISdb[DB].EmpSource.Enabled := false;
            GISdb[db].ApplyGISFilter('CRITERION=' + QuotedStr(Criteria.Strings[i]));
            GISdb[db].PutInQuartilesBasedOnExistingSort(4);
            TileCharateristicsWhiskerPlotsByCluster(DB,false,Nil,Criteria.Strings[i]);
         end;
      end;
   finally
      //GISdb[db].ClearGISFilter;
      EndDEMIXProcessing(db);
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('WhiskerPlotsByCluster out'); {$EndIf}
end;


procedure AddDemixLegend(var Graph :  tThisBaseGraph; DEMsUsed : tStringList = nil);
var
   bmp : tMyBitmap;
begin
   Graph.GraphDraw.BottomLegendFName := MDtempDir + 'DEM_DEM_Legend.bmp';
   Graph.GraphDraw.BottomMargin := 125;
   bmp := DEMIXTestDEMLegend(true,DEMsUsed,Graph.Width);
   bmp.SaveToFile(Graph.GraphDraw.BottomLegendFName);
   bmp.Destroy;
end;


function AdjustFilterForDTMandAll(DB : integer; Base : shortstring) : shortstring;
begin
   Result := Base;
   if GISdb[DB].MyData.FieldExists('REF_TYPE') then Result:= Result+  ' AND REF_TYPE=' + QuotedStr('DTM');
   if GISdb[DB].MyData.FieldExists('LAND_TYPE') then Result:= Result+  ' AND LAND_TYPE=' + QuotedStr('ALL');
end;


procedure SetHorizAxisForDEMIXscores(var Graph : tThisBaseGraph);
begin
    Graph.GraphDraw.MinHorizAxis := 0.5;
    Graph.GraphDraw.MaxHorizAxis := NumDEMIXtestDEM + 0.5;
end;


procedure DEMIX_SSIM_FUV_GraphSettings(var Graph : tThisBaseGraph; DBonTable : integer; lltext : shortstring; Ncrits : integer);
begin
   Graph.GraphDraw.GraphAxes := XPartGridOnly;
   Graph.GraphDraw.LeftMargin := 175;
   Graph.GraphDraw.MaxVertAxis := NCrits + 0.5;
   Graph.GraphDraw.MinVertAxis := 0.5;
   Graph.GraphDraw.MaxHorizAxis := 1.0;
   Graph.GraphDraw.MinHorizAxis := -0.01;
   Graph.GraphDraw.SetShowAllLines(true);
   Graph.GraphDraw.LLcornerText := llText;
   AddTopLabelToGraph(Graph,DBonTable);
end;

procedure SetHorizAxisForSSIM(dbOnTable : integer; var Graph : tThisBaseGraph; theCrit : shortstring);
var
   Table : tMyData;
   fName : PathStr;
   i : integer;
   aMinVal,aMaxVal,Range : float64;
begin
   if StrUtils.AnsiContainsText(theCrit,'SSIM') then begin
      fName := DEMIXSettingsDir + 'ssim_graph_limits.dbf';
      Table := tMyData.Create(fName);
      Table.ApplyFilter('CRITERION=' + QuotedStr(theCrit));
      if (Table.FiltRecsInDB = 1) then begin
         Graph.GraphDraw.MinHorizAxis := Table.GetFieldByNameAsFloat('X_MIN');
         Graph.GraphDraw.MaxHorizAxis := Table.GetFieldByNameAsFloat('X_MAX');
      end
      else begin
         Graph.GraphDraw.MinHorizAxis := 0;
         Graph.GraphDraw.MaxHorizAxis := 1;
      end;
      Table.Destroy;
   end
   else begin
      for i := 1 to NumDEMIXtestDEM do begin
         if GISdb[DBonTable].MyData.FieldExists(DEMIXShort[i]) then begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].MyData.FindFieldRange(DEMIXShort[i],aMinVal,aMaxVal);
            if (aminVal < Graph.GraphDraw.MinHorizAxis) then Graph.GraphDraw.MinHorizAxis := aMinVal;
            if (amaxval > Graph.GraphDraw.MaxHorizAxis) then Graph.GraphDraw.MaxHorizAxis := aMaxVal;
         end;
      end;
      Range := Graph.GraphDraw.MaxHorizAxis - Graph.GraphDraw.MinHorizAxis;
      Graph.GraphDraw.MinHorizAxis := Graph.GraphDraw.MinHorizAxis - 0.05 * Range;
      Graph.GraphDraw.MaxHorizAxis := Graph.GraphDraw.MaxHorizAxis + 0.05 * Range;
   end;
end;


function OpenGraphForCriterionScoresOrEvaluations(DBonTable : integer; DEMs : tStringList; HorizAxisLabel,VertAxisField : shortstring;
   GraphEvaluation : boolean; BestCriteria : boolean = true) : tThisBaseGraph;
var
   i : integer;
   aMinVal,aMaxVal,Range : float64;
begin
   {$If Defined(RecordDEMIX_OpenGraph)} WriteLineToDebugFile('OpenGraphForCriterionScoresOrEvaluations, Criterion=' + HorizAxisLabel + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
   HorizAxisLabel := RemoveUnderscores(HorizAxisLabel);
   Result:= tThisBaseGraph.Create(Application);
   Result.Width := MDDef.DEMIX_xsize;
   Result.Height := MDDef.DEMIX_ysize;
   Result.GraphDraw.ShowHorizAxis1 := true;
   Result.GraphDraw.HorizLabel := HorizAxisLabel;
   Result.Caption := GISdb[DBonTable].DBName + ' ' + HorizAxisLabel;
   if (VertAxisField <> '') then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.FindFieldRange(VertAxisField,aMinVal,aMaxVal);
      Result.GraphDraw.MinVertAxis := aMinVal;
      Result.GraphDraw.MaxVertAxis := aMaxVal;
   end
   else begin
      Result.GraphDraw.MinVertAxis := 0;
      Result.GraphDraw.MaxVertAxis := 100;
   end;

   if BestCriteria then begin
      Result.GraphDraw.MaxHorizAxis := 1;
      Result.GraphDraw.MinHorizAxis := 0;
      Result.GraphDraw.LeftMargin := 45;
   end
   else begin
      AddDEMIXLegend(Result,DEMs);
      Result.GraphDraw.MaxHorizAxis := -99999;
      Result.GraphDraw.MinHorizAxis := 99999;
      if GraphEvaluation then begin
         SetHorizAxisForSSIM(dbOnTable,Result,HorizAxisLabel);
         Result.GraphDraw.LeftMargin := 100;
      end
      else begin
         Result.GraphDraw.MinHorizAxis := 0.5;
         Result.GraphDraw.MaxHorizAxis := NumDEMIXtestDEM + 0.5;
         if (GISdb[DBonTable].MyData.FiltRecsInDB > 200) then begin
            Result.GraphDraw.LeftMargin := 45;
            Result.GraphDraw.ShowGraphLeftLabels := false;
         end
         else begin
            Result.GraphDraw.ShowGraphLeftLabels := true;
            Result.GraphDraw.LeftMargin := 290;
         end;
      end;
   end;
   GISdb[DBonTable].EmpSource.Enabled := false;
   {$If Defined(RecordDEMIX_OpenGraph)} WriteLineToDebugFile('OpenGraphForCriterionScoresOrEvaluations out ' + Result.GraphDraw.AxisRange); {$EndIf}
end;


var
   rfile : array[0..MaxDEMs] of File;
   demix_bfa : array[1..MaxDEMs,1..2] of float32;


procedure ZeroBFA;
var
   i,j,k : integer;
begin
   //for I := 0 to MaxTiles do
      for j := 1 to MaxDEMs do
         for k := 1 to 2 do
            demix_bfa[j,k] := 0;
end;



function WinningPercentagesComparedToCOP(db : integer; CompareDEM : shortstring; Criteria,inUseDEMs : tStringList; HLabel : shortstring = '') : tThisBaseGraph;
const
   MaxCrit = 25;
   CompDEM = 10;
var
   Criterion,TStr,Outcome,DEM1,DEM2 : shortstring;
   i,j,k,CritID,Total,NumDEMs : integer;
   bfa : array[0..MaxCrit,0..CompDEM,1..3] of int16;
   GraphDB,UseDEMs : tStringList;
   lowx,highx,y,Eval1,Eval2 : float32;
   Color : tColor;
   fName : PathStr;
   bs,NumTiles,oc : integer;
   aField,aFilter,aLine : shortstring;
   Wins,Loss,Tie,
   DEM : Integer;


         function MakeLine(StartPC,EndPC,y : float32; Color : tColor; Criterion : shortstring; bs : integer) : shortstring;
         begin
            Result := RealToString(100*StartPC,-8,-2) + ',' + RealToString(100 * EndPC,-8,2) + ',' + RealToString(y,5,0) + ',' + IntToStr(Color) + ',' + Criterion + ',' + IntToStr(bs);
         end;

         procedure ProcessOne(CritIM,DEM : integer);
         begin
            Wins := bfa[CritID,DEM,1];
            Tie  := bfa[CritID,DEM,2];
            Loss := bfa[CritID,DEM,3];
            Total := Wins+Loss+Tie;

            Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM1));
            if (Wins > Loss) then bs := 0 {bsSolid} else bs := 1 {bsDiagCross};
            aline := MakeLine(0,Wins/Total,y,Color,Criteria[CritID],bs);
            GraphDB.Add(aline);
            aline := MakeLine(Wins/Total,(Wins + Tie)/Total,y,clWhite,Criteria[CritID],0);
            GraphDB.Add(aline);
            if (Wins < Loss) then bs := 0 {bsSolid} else bs := 1 {bsDiagCross};
            Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM2));
            aline := MakeLine((Wins+Tie)/Total,1,y,Color,Criteria[CritID],bs);
            GraphDB.Add(aline);
            y := y + 1;
         end;


begin
     {$IfDef RecordDEMIXWins} WriteLineToDebugFile('WinningPercentagesComparedToCOP in, DEMs='+ IntToStr(UseDEMs.Count)); {$EndIf};
      DEM1 := CompareDEM;
      GraphDB := tStringList.Create;
      GraphDB.Add('X1,X2,Y,COLOR,LABEL,BS_STYLE,OUTCOME');
      UseDEMs := tStringList.Create;
      for i := 0 to pred(inUseDEMs.Count) do
         if GISdb[DB].MyData.FieldExists(inUseDEMs[i]) then UseDEMs.Add(InUseDEMs[i]);
     {$IfDef RecordDEMIXWins} WriteLineToDebugFile('WinningPercentagesComparedToCOP Checked, DEMs='+ IntToStr(UseDEMs.Count)); {$EndIf};

      NumTiles := GISdb[DB].NumUniqueEntriesInDB('DEMIX_TILE');
      for i := 0 to pred(Criteria.Count) do
         for j := 0 to pred(UseDEMs.Count) do
            for k := 1 to 3 do bfa[i,j,k] := 0;

      {$IfDef RecordDEMIXWins} WriteLineToDebugFile('Start winning records'); {$EndIf};
      StartProgress('Winning records versus ' + DEM1);
      j := 0;
      GISdb[DB].EmpSource.Enabled := false;
      GISdb[DB].MyData.First;
      while not GISdb[DB].MyData.eof do begin
         if (j mod 25 = 0) then UpdateProgressBar(j/GISdb[DB].MyData.FiltRecsInDB);
         inc(j);
         GISdb[DB].EmpSource.Enabled := false;
         Criterion := GISdb[DB].MyData.GetFieldByNameAsString('CRITERION');
         CritID := Criteria.IndexOf(Criterion);
         if (CritID > -1) then begin
            for DEM := 0 to pred(UseDEMs.Count) do begin
               DEM2 := UseDEMs.Strings[DEM];
               if (DEM1 <> DEM2) then begin
                  OutCome := GetWinner(db,DEM1,DEM2);
                     if (Outcome = DEM1) then oc := 1
                     else if (Outcome = 'TIE') then oc := 2
                     else oc := 3;
                     inc(bfa[CritID,DEM,oc]);
               end;
            end;
         end;
         GISdb[DB].MyData.Next;
      end;
      EndProgress;

      y := 1;
      if (MDDef.DEMIX_groupWonLost = 1) then begin
         for CritID := 0 to pred(Criteria.Count) do begin
            for DEM := 0 to pred(UseDEMs.Count) do begin
               DEM2 := UseDEMs[DEM];
               if (DEM2 <> DEM1) then begin
                  ProcessOne(CritID,DEM);
               end;
            end;
            y := y + 1;
         end;
      end
      else begin
         for DEM := 0 to pred(UseDEMs.Count) do begin
            DEM2 := UseDEMs[DEM];
            if (DEM2 <> DEM1) then begin
               for CritID := 0 to pred(Criteria.Count) do begin
                  ProcessOne(CritID,DEM);
               end;
            end;
            y := y + 1;
         end;
      end;

      fName := NextFileNumber(MDTempDir,DEMIXmodeName + '_winning_graph_','.dbf');
      StringList2CSVtoDB(GraphDB,fName,false);

      {$IfDef RecordDEMIXWins} WriteLineToDebugFile('Start Graph'); {$EndIf};
      Result := tThisBaseGraph.Create(Application);
      Result.Width := 500;
      Result.Height := 300 + 25 * succ(Criteria.Count) * (UseDEMs.Count);
      {$IfDef RecordDEMIXWns} WriteLineToDebugFile('Graph=' + IntToStr(Result.Width) + 'x' + IntToStr(Result.Height)); {$EndIf};
      AddTopLabelToGraph(Result,db);
      GISdb[DB].EmpSource.Enabled := false;
      if (HLabel <> '') then begin
         Result.GraphDraw.TopLabel := HLabel;
         Result.GraphDraw.TopMargin := 45;
      end;
      Result.GraphDraw.BottomMargin := 65;
      Result.GraphDraw.HorizLabel := 'Win/tie/loss (percent)';
      Result.GraphDraw.MaxHorizAxis := 100;
      Result.GraphDraw.MaxVertAxis := y - 1;
      Result.GraphDraw.GraphAxes := XPartGridOnly;
      Result.GraphDraw.LeftMargin := 160;
      Result.BarGraphDBName := fName;
      if (not MDDef.DEMIX_combined_graph) then AddDemixLegend(Result,UseDEMs);
      Result.RedrawDiagram11Click(Nil);
      UseDEMs.Destroy;
     {$IfDef RecordDEMIXWins} WriteLineToDebugFile('WinningPercentagesComparedToCOP out'); {$EndIf};
end;


function AverageScoresGraph(db : integer; DEMs : tStringList; HL : shortstring; aDEMIXLegend : boolean; MinHoriz,MaxHoriz : float64) : tThisBaseGraph;
//graphs database with the Graphs from averaging
//   FILTER field has the filter used
//   NUM_TILES has the number of tiles that met the filter
//   Field for each DEM has the score or evaluation
var
   i,j,k,y,CritID,Total,NumDEMs,tw,MaxTW : integer;
   fName : PathStr;
   GrafFile : tStringList;
   v : array[1..3] of float32;
   aField,aLabel : shortstring;
   DEMsPresent : tStringList;
   Color : array[0..15] of tColor;
   MinF,MaxF : float64;

      function TheLabel : shortstring;
      begin
          Result := GISdb[DB].MyData.GetFieldByNameAsString('FILTER') + ' (n=' + IntToStr(GISdb[DB].MyData.GetFieldByNameAsInteger('NUM_TILES')) + ')';
      end;

begin
   {$IfDef RecordAverageScoresGraph} WriteLineToDebugFile('AverageScoresGraph in, db=' + GISdb[db].dbName + ' HL=' + HL); {$EndIf};
   Result := tThisBaseGraph.Create(Application);
   Result.Canvas.Font.Style := [fsBold];
   Result.Canvas.Font.Name := Result.FontDialog1.Font.Name;
   Result.Canvas.Font.Size := Result.FontDialog1.Font.Size;
   Result.Caption := HL;

   DEMsPresent := tStringList.Create;
   j := 0;
   for i := 0 to pred(DEMs.Count) do begin
      if GISdb[DB].MyData.FieldExists(DEMs.Strings[i]) then begin
         DEMsPresent.Add(DEMs.Strings[i]);
         Color[j] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMs.Strings[i]));
         inc(j);
      end;
   end;

   if (MinHoriz < -9998) and (MaxHoriz < -9998) then begin
      MinHoriz := 9999;
      for i := 0 to pred(DEMs.Count) do begin
         {$IfDef RecordAverageScoresGraph} WriteLineToDebugFile('DEM=' + DEMs.strings[i]); {$EndIf};
         if GISdb[DB].MyData.FieldExists(DEMs.Strings[i]) then begin
            if GISdb[DB].MyData.FindFieldRange(DEMsPresent.Strings[i],MinF,MaxF) then begin
               if (MinF < MinHoriz) then MinHoriz := round(MinF-1);
               if (MaxF > MaxHoriz) then MaxHoriz := round(MaxF + 1);
            end;
         end;
      end;
      Result.GraphDraw.ShowVertAxis0 := true;
   end;

   MaxTW := 0;
   GISdb[DB].MyData.First;
   while not GISdb[DB].MyData.eof do begin
      GISdb[DB].EmpSource.Enabled := false;
      TW := Result.Canvas.TextWidth(theLabel);
      if (TW > MaxTW) then MaxTw := Tw;
      GISdb[DB].MyData.Next;
   end;
   Result.Width := MaxTW + 450;
   Result.Height := 75 + 25 * GISdb[DB].MyData.FiltRecsInDB;
   Result.GraphDraw.LeftMargin := MaxTW + 20;
   AddTopLabelToGraph(Result,db);
   Result.GraphDraw.HorizLabel := HL;
   Result.GraphDraw.MinHorizAxis := 1;
   Result.GraphDraw.MaxHorizAxis := DEMs.Count;
   Result.GraphDraw.MinHorizAxis := MinHoriz;
   Result.GraphDraw.MaxHorizAxis := MaxHoriz;
   Result.GraphDraw.MaxVertAxis := GISdb[db].MyData.FiltRecsInDB + 1;
   Result.GraphDraw.GraphAxes := XPartGridOnly;
   Result.GraphDraw.GraphLeftLabels := tStringList.Create;
   {$IfDef RecordAverageScoresGraph} WriteLineToDebugFile('AverageScoresGraph setup done, Caption=' + Result.Caption); {$EndIf};

   GrafFile := tStringList.Create;
   GrafFile.Add('X,Y,COLOR');

   GISdb[DB].MyData.First;
   y := 1;
   while not GISdb[DB].MyData.eof do begin
      GISdb[DB].EmpSource.Enabled := false;
      v[2] := y;
      Result.GraphDraw.GraphLeftLabels.Add(IntToStr(y) + ',' + theLabel);
      for i := 0 to pred(DEMsPresent.Count) do begin
         v[1] := GISdb[DB].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[i]);
         GrafFile.Add(RealToString(v[1],-12,-4) + ',' + IntToStr(y) + ',' + IntToStr(Color[i]));
      end;
      GISdb[DB].MyData.Next;
      y := y + 1;
   end;
   Result.XYColorDBName := NextFileNumber(MDTempDir,'graph_','.dbf');
   StringList2CSVtoDB(GrafFile,Result.XYColorDBName,true,false,false);
   Result.GraphDraw.ShowGraphLeftLabels := true;
   if aDEMIXLegend then begin
      Result.Height := Result.Height + 100;
      AddDemixLegend(Result,DEMsPresent);
   end;
   DEMsPresent.Destroy;
   Result.RedrawDiagram11Click(Nil);
   {$IfDef RecordAverageScoresGraph} WriteLineToDebugFile('AverageScoresGraph out, Caption=' + Result.Caption); {$EndIf};
end;


procedure BestBySlopeRough(dbOnTable : integer; Criteria : tStringList; Winners : boolean; Param1,Param2,ValueParam : shortstring);

   function CreateGraph(Criterion,DEM1,DEM2 : shortstring) : tThisBaseGraph;
   const
      MaxPossBins = 10;
   type
      tBinRec = record
         N : int16;
         Avg : float32;
         Win,Loss,Tie : int16;
      end;
   var
      i,j,db,bin : integer;
      Rank,LocStr : shortString;
      Color : tColor;
      rfile : file;
      fName : PathStr;
      MaxEval,BestEval,XS,YS : float32;
      v : array[1..3] of float32;
      Bins : array[1..MaxPossBins,1..MaxPossBins] of tBinRec;
      GraphData : tStringList;
      DiffDist : boolean;
   begin
      //Result := nil;
         if (MDDef.DEMIXUseBins > MaxPossBins) then MDDef.DEMIXUseBins := MaxPossBins;
         DiffDist := StrUtils.AnsiContainsText(ValueParam,'ELVD') or StrUtils.AnsiContainsText(ValueParam,'SLPD') or StrUtils.AnsiContainsText(ValueParam,'RUFD');
         if DiffDist then begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            MaxEval := GISdb[DBonTable].MyData.FindFieldMax(ValueParam);
         end;

         for I := 1 to MaxPossBins do begin
            for j := 1 to MaxPossBins do begin
               Bins[i,j].n := 0;
               Bins[i,j].avg := 0;
               Bins[i,j].Win := 0;
               Bins[i,j].Loss := 0;
               Bins[i,j].Tie := 0;
            end;
         end;
         ShowHourglassCursor;
         Result := tThisBaseGraph.Create(Application);
         Result.Width := MDDef.DEMIX_xsize;
         Result.Height := MDDef.DEMIX_ysize;
         Result.GraphDraw.MaxVertAxis := 80;
         Result.GraphDraw.VertLabel := 'Tile Average Slope (%)';
         Result.GraphDraw.HorizLabel := 'Tile Percent Barren (%)';
         Result.GraphDraw.LLCornerText := Criterion;
         Result.OpenXYColorFile(rfile);
         GISdb[DBonTable].ApplyGISFilter('CRITERION=' + QuotedStr(Criterion));
         GISdb[DBonTable].EmpSource.Enabled := false;
         while not GISdb[DBonTable].MyData.eof do begin
            v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Param1);
            v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Param2);
            i := succ(trunc(MDDef.DEMIXUseBins * (v[1] / Result.GraphDraw.MaxHorizAxis)));
            if (i > MDDef.DEMIXUseBins) then i := MDDef.DEMIXUseBins;
            j := succ(trunc(MDDef.DEMIXUseBins * (v[2] / Result.GraphDraw.MaxVertAxis)));
            if (j > MDDef.DEMIXUseBins) then j := MDDef.DEMIXUseBins;
            if (v[1] < 0) or (v[2] < 0) then begin
               //unclear why this happens, but it creates problems
            end
            else begin
               inc(Bins[i,j].n);
               if Winners then begin
                  Rank := GetWinner(dbOnTable,DEM1,DEM2);
                  //Rank := GISdb[DBonTable].MyData.GetFieldByNameAsString(cf);
                  if (Rank = DEM1) then begin
                     v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM1));
                     inc(Bins[i,j].Win);
                  end
                  else if (Rank = 'TIE') then begin
                     v[3] := clSilver;
                     inc(Bins[i,j].Tie);
                  end
                  else begin
                     v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM2));
                     inc(Bins[i,j].Loss);
                  end;
               end
               else begin
                  BestEval := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(ValueParam);
                  if DiffDist then BestEval := BestEval / MaxEval;

                  v[3] := RainbowColorFunct(1-BestEval,0,1);
                  Bins[i,j].avg := Bins[i,j].avg + BestEval;
               end;
               BlockWrite(rfile,v,1);
            end;
            GISdb[DBonTable].MyData.Next;
         end;

         AddTopLabelToGraph(Result,DBonTable);
         CloseFile(rfile);

         if (MDDef.TwoParameterVisualization = 0) then begin  //Pie charts
            xs := Result.GraphDraw.MaxHorizAxis / MDDef.DEMIXUseBins;
            ys := Result.GraphDraw.MaxVertAxis / MDDef.DEMIXUseBins;
            GraphData := tStringList.Create;
            if Winners then begin
               GraphData.Add('X,Y,X_SIZE,Y_SIZE,BIN,NTOTAL,N,COLOR');
               Bin := 0;
               for I := 1 to MDDef.DEMIXUseBins do begin
                  for j := 1 to MDDef.DEMIXUseBins do begin
                     if (Bins[i,j].n > 0) then begin
                       inc(Bin);
                       LocStr := RealToString((pred(i) + 0.5) * xs,-12,-4) + ',' + RealToString((pred(j) + 0.5) * ys,-12,-4) + ',' +
                            RealToString(xs * 0.5,-12,-4) + ',' + RealToString(ys* 0.5,-12,-4) + ',' + IntToStr(Bin) + ',' + IntToStr(Bins[i,j].n) + ',' ;
                       GraphData.Add(LocStr + IntToStr(Bins[i,j].Win) + ','  + IntToStr(ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM1))));
                       GraphData.Add(LocStr + IntToStr(Bins[i,j].Tie) + ','  + IntToStr(clSilver));
                       GraphData.Add(LocStr + IntToStr(Bins[i,j].Loss) + ','  + IntToStr(ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM2))));
                     end;
                  end;
               end;
               Result.GraphDraw.GraphType := gtScaledPieCharts;
            end
            else begin
               GraphData.Add('X,Y,X_SIZE,Y_SIZE,N,AVG,COLOR');
               for I := 1 to MDDef.DEMIXUseBins do begin
                  for j := 1 to MDDef.DEMIXUseBins do begin
                     if Bins[i,j].n > 0 then begin
                       LocStr := RealToString((pred(i) + 0.5) * xs,-12,-4) + ',' + RealToString((pred(j) + 0.5) * ys,-12,-4) + ',' +
                            RealToString(xs * 0.5,-12,-4) + ',' + RealToString(ys* 0.5,-12,-4) + ',';
                       Bins[i,j].avg := Bins[i,j].avg / Bins[i,j].n;
                       GraphData.Add(LocStr + IntToStr(Bins[i,j].n) + ',' + RealToString(Bins[i,j].avg ,-12,-4) + ','  + IntToStr(RainbowColorFunct(1-Bins[i,j].avg,0,1))  );
                     end;
                  end;
               end;
               Result.GraphDraw.GraphType := gtScaledColorSymbols;
            end;
            fName := NextFileNumber(MDTempDir,Criterion + '_two_param_graph_','.dbf');
            StringList2CSVtoDB(GraphData,fName,true,false,false);
            Result.BarGraphDBName := fName;
         end;
         Result.AutoScaleAndRedrawDiagram(false,false,false,false);
      //end;
   end;

var
   i : integer;
   Bitmap : tMyBitmap;
begin
   if GISdb[DBonTable].MyData.FieldExists(Param1) and GISdb[DBonTable].MyData.FieldExists(Param2) then begin
      try
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestBySlopeRough in,  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
         GetDEMIXpaths(False);
         GISdb[DBonTable].EmpSource.Enabled := false;
         for i := 0 to pred(Criteria.Count) do begin
            CreateGraph(Criteria.Strings[i],'COP','ALOS');
            (*
            CreateGraph(Criteria.Strings[i],'COP','TANDEM');
            CreateGraph(Criteria.Strings[i],'COP','FABDEM');
            CreateGraph(Criteria.Strings[i],'COP','DILUV');
            CreateGraph(Criteria.Strings[i],'COP','DELTA');
            *)
         end;
         if not Winners then begin
            Bitmap := DefaultHorizontalLegendOnBitmap(0,1,'','',LegRainbows);
            DisplayBitmap(Bitmap,'Legend');
            Bitmap.Free;
         end;
      finally
         //GISdb[DBonTable].ClearGISFilter;
         //GISdb[DBonTable].ShowStatus;
         EndDEMIXProcessing(dbOnTable);
      end;
   end
   else begin
      MessageToContinue('Fields missing: ' + Param1 + ' and ' + Param2);
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestBySlopeRough out'); {$EndIf}
end;


procedure OpenGraphDEMIXDEMColorFiles(Graph : tThisBaseGraph; DEMs : tStringList; fName : shortstring);
var
   i : integer;
   DEM : shortstring;
   Color : tColor;
begin
   for i := 0 to pred(DEMs.Count) do begin
      DEM := DEMs.Strings[i];
      Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM));
      Graph.OpenDataFile(rfile[i],DEM + '_' + fName + '_',Color);
      Graph.GraphDraw.Symbol[i].DrawingSymbol := FilledBox;
      Graph.GraphDraw.Symbol[i].Size := MDDef.DEMIXsymsize;
   end;
   {$IfDef TrackColors} Graph.GraphColorsRecord('OpenGraphDEMIXDEMColorFiles'); {$EndIf}
end;


procedure AddDEM_DrawingLayers(DBonTable : integer; DEMs : tStringList; Graph : tThisBaseGraph; Evaluations : boolean; theSort : shortstring);
//Evaluations plots the criteria evaluations, otherwise the scores (opinions)
//if theSort is blank, the tiles or areas will be evenly spaced so they do not overprint
//if theSort is a field in the DB, the tiles or areas will be spaced on a numerical axis by the field value
var
   NPts,i,j,TileID : integer;
   v : array[1..2] of float32;
   Tile : shortstring;
   Tiles : tStringList;
   val,y : float32;
begin
   if (DEMs <> Nil) then begin
      y := 0;
      GISdb[DBonTable].EmpSource.Enabled := false;
      Tiles := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
      OpenGraphDEMIXDEMColorFiles(Graph,DEMs,'eval');
      StartProgress('Graph');
      GISdb[DBonTable].MyData.First;
      NPts := 0;
      while not GISdb[DBonTable].MyData.eof do begin
         if (NPts mod 25 = 0) then UpdateProgressBar(j/Tiles.Count);
         inc(NPts);
         GISdb[DBonTable].EmpSource.Enabled := false;
         for i := 0 to pred(DEMs.Count) do begin
            v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMs.Strings[i]);
            if (theSort = '') then v[2] := 100 * y / GISdb[DBonTable].MyData.FiltRecsInDB
            else v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(theSort);
            BlockWrite(rfile[i],V,1);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      for i := 0 to pred(DEMs.Count) do CloseFile(rfile[i]);
      Graph.RedrawDiagram11Click(Nil);
      Tiles.Destroy;
      EndProgress;
      GISdb[DBonTable].EmpSource.Enabled := true;
      Graph.GraphDraw.HorizLabel := RemoveUnderscores(Graph.GraphDraw.HorizLabel) +  '  (n=' + IntToStr(NPts) + ')';
   end;
end;


function GraphForOneCriterion(var PanelsName : PathStr; DBonTable : integer; DEMs : tStringList; Evaluations : boolean = true; HorizAxisLabel: shortstring = '';
     VertAxisField : shortstring = ''; VertAxisLabel : shortstring = ''; TopLabel : shortstring = '') : tThisBaseGraph;
var
  y,i,j,DEM : integer;
var
   Bitmap : tMyBitmap;
begin
   {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('Enter GraphForOneCriterion, ' + HorizAxisLabel + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
   Result := OpenGraphForCriterionScoresOrEvaluations(dbOnTable,DEMs,HorizAxisLabel,VertAxisField,Evaluations);

   if (VertAxisLabel = '') then VertAxisLabel := 'Best tile evaluation (%)';
   Result.GraphDraw.VertLabel := RemoveUnderscores(VertAxisLabel);
   Result.GraphDraw.HorizLabel := RemoveUnderscores(HorizAxisLabel);

   Result.GraphDraw.MinHorizAxis := -0.05;
   Result.GraphDraw.MaxHorizAxis := 1.05;
   Result.GraphDraw.ShowHorizAxis0 := true;
   Result.GraphDraw.ShowHorizAxis1 := true;
   Result.GraphDraw.LeftMargin := 75;

   if (TopLabel <> '') then begin
      Result.GraphDraw.TopLabel := TopLabel;
      Result.GraphDraw.TopMargin := 45;
   end;

   Result.GraphDraw.GraphLeftLabels := tStringList.Create;
   Result.GraphDraw.DrawInsideLines := false;
   AddDEM_DrawingLayers(DBonTable,DEMs,Result,Evaluations,VertAxisField);
   {$IfDef TrackColors} Result.GraphColorsRecord('GraphForOneCriterion after add layers'); {$EndIf}
   PanelsName := Result.AnimateGraph(false,false);
   {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('Exit GraphForOneCriterion'); {$EndIf}
end;


function PlotBestEvalVersusPercentileMultipleCriteria(DBonTable : integer; Criteria : tStringList; Evaluations : boolean = true;
     TopLabel : shortstring = ''; HL : shortstring = ''; VertAxisField : shortstring = '') : tThisBaseGraph;
var
   i,j, y : integer;
   v : array[1..2] of float32;
   rfile : file;
   Color : tColor;
   Symbol : tFullSymbolDeclaration;
   Criterion,BaseFilter : shortstring;
   PanelName : PathStr;
begin
   {$If Defined(RecordDEMIX) or Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('PlotBestEvalVersusPercentileMultipleCriteria in, Criteria=' + IntToStr(Criteria.Count)); {$EndIf}
    if GISdb[DBonTable].MyData.FieldExists('BEST_EVAL') then begin
       GISdb[DBonTable].EmpSource.Enabled := false;
       BaseFilter :=  GISdb[DBonTable].MyData.Filter;
       Result := nil;
       Result := GraphForOneCriterion(PanelName,DBonTable,Nil,Evaluations,HL,VertAxisField,'',TopLabel);
       for j := 0 to pred(Criteria.Count) do begin
          Criterion := Criteria.Strings[j];
          GISdb[DBonTable].ApplyGISFilter(PetDBUtils.AddAndIfNeeded(BaseFilter) + 'CRITERION=' + QuotedStr(Criterion));
          Color := WinGraphColors(succ(j));
          {$If Defined(RecordDEMIX_criteria_colors)} WriteLineToDebugFile(Criterion + ' color=' + IntToStr(Color) + '  ' + ColorString(Color)); {$EndIf}
          Symbol.Color := ConvertTColorToPlatformColor(Color);
          Symbol.DrawingSymbol := FilledBox;
          Symbol.Size := MDDef.DemixSymSize;
          Result.OpenDataFile(rfile,Criteria.Strings[j],Color);
          y := 0;
          GISdb[DBonTable].EmpSource.Enabled := false;
          while not GISdb[DBonTable].MyData.eof do begin
             v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('BEST_EVAL');
             v[2] := 100 * y / GISdb[DBonTable].MyData.FiltRecsInDB;
             BlockWrite(rfile,v,1);
             inc(y);
             GISdb[DBonTable].MyData.Next;
          end;
          CloseFile(rfile);
       end;

       Result.GraphDraw.SetShowAllPoints(false);
       Result.GraphDraw.SetShowAllLines(true);
       for i := 1 to MaxGraphSeries do Result.GraphDraw.LineSize256[i] := MDDef.DemixSymSize;
       Result.GraphDraw.MaxHorizAxis := 1.1;
       Result.AutoScaleAndRedrawDiagram(false,false,false,false);
    end
    else MessageToContinue('Add BEST_EVAL to DB');
   {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('PlotBestEvalVersusPercentileMultipleCriteria out'); {$EndIf}
end;


procedure FilterJustOneGraph(DBonTable : integer; Criteria,GeomorphFilters : tStringList; Evaluations : boolean = true; HL : shortstring = '');
var
   PanelName : PathStr;

      function AGraph(Crit : integer) : tThisBaseGraph;
      var
         k,y,i : integer;
         v : array[1..2] of float32;
         rfile : file;
         aFilter,TStr,Criterion,HL : shortstring;
         //Symbol : tFullSymbolDeclaration;
         BaseFilter : shortstring;
         PanelName : PathStr;
      begin
           {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('Agraph in, Crit=' + IntToStr(Crit)); {$EndIf}
           BaseFilter :=  GISdb[DBonTable].MyData.Filter;
           HL := Petmar_types.BeforeSpecifiedString(GISdb[DBonTable].dbName,'demix');
           Result := nil;
           Result := GraphForOneCriterion(PanelName,DBonTable,Nil,Evaluations,HL,'');
           AddTopLabelToGraph(Result,DBonTable);
           for k := 0 to pred(GeomorphFilters.Count) do begin
              Criterion := Criteria.Strings[Crit];
              aFilter := PetDBUtils.AddAndIfNeeded(BaseFilter) + 'CRITERION=' + QuotedStr(Criterion);
              if (GeomorphFilters[k] <> '') then aFilter := aFilter + ' AND ' + GeomorphFilters[k];

              GISdb[DBonTable].ApplyGISFilter(aFilter);
              if (GeomorphFilters[k] <> '') then TStr := GeomorphFilters[k]
              else TStr := 'All slopes';
              Result.GraphDraw.HorizLabel := Criterion;
              GISdb[DBonTable].EmpSource.Enabled := false;
              //Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors(k));
              //Symbol.DrawingSymbol := FilledBox;
              //Symbol.Size := MDDef.DemixSymSize;
              Result.OpenDataFile(rfile,TStr + '__' + NumTilesString(DBonTable),WinGraphColors(k));
              y := 0;
              while not GISdb[DBonTable].MyData.eof do begin
                 v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('BEST_EVAL');
                 v[2] := 100 * y / GISdb[DBonTable].MyData.FiltRecsInDB;
                 BlockWrite(rfile,v,1);
                 inc(y);
                 GISdb[DBonTable].MyData.Next;
              end;
              CloseFile(rfile);
           end;
           Result.GraphDraw.MaxHorizAxis := 1.1;
           Result.GraphDraw.SetShowAllPoints(false);
           Result.GraphDraw.SetShowAllLines(true);
           for i := 1 to MaxGraphSeries do Result.GraphDraw.LineSize256[i] := MDDef.DemixSymSize;
           Result.AutoScaleAndRedrawDiagram(false,false,false,false);
           GISdb[DBonTable].ApplyGISFilter(BaseFilter);
          {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('Agraph out, Crit=' + IntToStr(Crit)); {$EndIf}
      end;

var
   j : integer;
   Legend,BigBitmap : tMyBitmap;
   fName : PathStr;
   gr : tGraphArray;
begin {FilterJustOneGraph}
   if TileCharacteristicsInDB(DBonTable) and GISdb[DBonTable].MyData.FieldExists('BEST_EVAL') then begin
     {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('FilterJustOneGraph in'); {$EndIf}
        GetDEMIXpaths(False);
      for j := 0 to pred(Criteria.Count) do begin
         gr[j] := AGraph(j);
      end;
      {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('FilterJustOneGraph start merge'); {$EndIf}
      BigBitmap := tMyBitmap.Create;
      BigBitmap.LoadFromFile(MergeGraphPanelsHorizontal(Criteria.Count,gr));
      (*
         Legend := gr[0].MakeLegend;
         Fname := NextFileNumber(MDtempDir,'FilterJustOneGraph_','.png');
         FinishBigBitMapWithLegend(BigBitmap,Legend,fName);
      *)
      ShowDefaultCursor;
     {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('FilterJustOneGraph out'); {$EndIf}

      EndDEMIXProcessing(DBonTable);
   end
   else begin
      MessageToContinue('Add tile characteristics and best eval to DB');
   end;
end {FilterJustOneGraph};


procedure DEMIX_evaluations_graph(DBonTable,YAxisWhat : integer; DEMs,Criteria : tStringList; Evaluations : boolean = true);
//for each criterion, graph of evaluations sorted by parameter, and with tile names
var
   BaseFilter : shortstring;

      procedure MakeAllGraphs(Numerical : boolean; TheSort : shortstring; GraphName : shortstring);
      var
         Legend,BigBitmap,Bitmap : tMyBitmap;
         j : integer;
         gr : tGraphArray;
         BigGraph,Findings : tstringList;
         fName,PanelsName : PathStr;
         aFilter : shortstring;
      begin {procedure MakeAllGraphs}
         {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('MakeAllGraphs, Criteria=' + IntToStr(Criteria.Count) + ' n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
         if (TheSort = '') or GISdb[DBonTable].MyData.FieldExists(TheSort) then begin
            Findings := tStringList.Create;
            for j := 0 to pred(Criteria.Count) do begin
               aFilter := 'CRITERION=' + QuotedStr(Criteria.Strings[j]);
               if (BaseFilter <> '') then aFilter := BaseFilter + ' AND ' + aFilter;

               GISdb[DBonTable].ApplyGISFilter(aFilter);
               if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
                  gr[j] := GraphForOneCriterion(PanelsName,DBonTable,DEMs,Evaluations,NoSuffixCriterion(Criteria.Strings[j]),theSort,GraphName);
                  Findings.Add(PanelsName);
               end;
            end;
            BigBitmap := tMyBitmap.Create;
            BigBitmap.LoadFromFile(MergeBitmapsHorizontal(Findings));
            Fname := NextFileNumber(MDtempDir,'FilterJustOneGraph_','.png');
            bitmap := DEMIXTestDEMLegend(true,DEMs);
            FinishBigBitMapWithLegend(BigBitmap, bitmap,fName);
         end
         else begin
            MessageToContinue('Field missing for sort ' + TheSort);
         end;
      end {procedure MakeAllGraphs};


      procedure GraphsColoredByTile;
      //   7/6/2025, this is not very effective and does not provide a very good graphic
      var
         i,j : integer;
         y : integer;
         Slope : float32;
         gr : tGraphArray;
         v : array[1..3] of float32;
         rfile : file;
         BigBitmap : tMyBitmap;
      begin
          {$If Defined(RecordDEMIX_evaluations_graph)}  WriteLineToDebugFile('GraphsColoredByTile in yasBestEvalColoredBySlope'); {$EndIf}
         if GISdb[DBonTable].MyData.FieldExists('BEST_EVAL') then begin
             for j := 0 to pred(Criteria.Count) do begin
                Gr[j] := OpenGraphForCriterionScoresOrEvaluations(dbOnTable,DEMs,'','BEST_EVAL',Evaluations);
                Gr[j].GraphDraw.VertLabel := 'Best evaluation (any DEM)';
                Gr[j].GraphDraw.HorizLabel := Criteria.Strings[j];
                Gr[j].GraphDraw.BottomMargin := 65;
                Gr[j].GraphDraw.LeftMargin := 75;
                GISdb[DBonTable].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'CRITERION=' + QuotedStr(Criteria.Strings[j]));
                {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile(Criteria.Strings[j] + '  recs=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
                GISdb[DBonTable].EmpSource.Enabled := false;
                Gr[j].OpenXYColorFile(rfile,Criteria.Strings[j]);
                y := 0;
                while not GISdb[DBonTable].MyData.eof do begin
                   Slope := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('AVG_SLOPE');
                   if (Slope < MDDef.SlopeFlatBoundary) then v[3] := clBlue
                   else if (Slope < MDDef.SlopeGentleBoundary) then v[3] := clLime
                   else if (Slope < MDDef.SlopeSteepBoundary) then v[3] := clGreen
                   else v[3] := clRed;
                   v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('BEST_EVAL');
                   v[2] := 100 * y / GISdb[DBonTable].MyData.FiltRecsInDB;
                   BlockWrite(rfile,v,1);
                   inc(y);
                   GISdb[DBonTable].MyData.Next;
                end;
                CloseFile(rfile);
                Gr[j].AutoScaleAndRedrawDiagram;
             end;
             BigBitmap := tMyBitmap.Create;
             BigBitmap.LoadFromFile(MergeGraphPanelsHorizontal(Criteria.Count,gr));
         end
         else MessageToContinue('Add BEST_EVAL to DB');
      end {procedure GraphsColoredByTile};


begin {DEMIX_evaluations_graph}
   if TileCharacteristicsInDB(DBonTable) then begin
     try
        BaseFilter := GISdb[DBonTable].MyData.Filter;
        //if BaseFilter[1] <> '(' then then BaseFilter := '(' + BaseFilter + ')';

        {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('DEMIX_evaluations_graph in,  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
        GetDEMIXpaths(False);
        GISdb[DBonTable].EmpSource.Enabled := false;
        if (YAxisWhat = yasBestEvalColoredBySlope) then GraphsColoredByTile
        else if (YAxisWhat = yasSlope) then MakeAllGraphs(true,'AVG_SLOPE','Tile average slope (%)')
        else if (YAxisWhat = yasRuff) then MakeAllGraphs(true,'AVG_ROUGH','Tile average roughness (%)')
        else if (YAxisWhat = yasRelief) then MakeAllGraphs(true,'RELIEF','Tile average relief (m)')
        else if (YAxisWhat = yasBarren) then MakeAllGraphs(true,'BARREN_PC','Tile barren (%)')
        else if (YAxisWhat = yasForest) then MakeAllGraphs(true,'FOREST_PC','Tile forest (%)')
        else if (YAxisWhat = yasLatitude) then MakeAllGraphs(true,'LAT','Tile latitude')
        else if (YAxisWhat in [yasBestEval]) then MakeAllGraphs(false,'','Tile best evaluation percentile');
     finally
        EndDEMIXProcessing(DBonTable);
        GISdb[DBonTable].ApplyGISFilter(BaseFilter);
     end;
   end
   else MessageToContinue('Add tile characteristics');
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph out'); {$EndIf}
end {DEMIX_evaluations_graph};



procedure SSIM_FUV_scatterplot(db : integer; theCrit : shortstring);
var
   Criterion1,Criterion2,crit,Tile : shortstring;
   Tiles : tStringList;
   i,j,k,TileID,CritID,NPts : integer;
   Graph : tThisBaseGraph;
   val : float32;
   v : array[1..2] of float32;
begin
   MessageToContinue('Disabled');
(*
   Criterion1 := TheCrit + '_SSIM';
   Criterion2 := TheCrit + '_FUV';
   GISdb[DB].EmpSource.Enabled := false;
   Tiles := GISdb[DB].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
   Graph := tThisBaseGraph.Create(Application);
   Graph.GraphDraw.HorizLabel := Criterion1;
   Graph.GraphDraw.VertLabel := Criterion2;
   SetHorizAxisForSSIM(db,Graph,Criterion1);
   Graph.GraphDraw.MinVertAxis := 0;
   Graph.GraphDraw.MaxVertAxis := 1;
   Graph.GraphDraw.LeftMargin := 125;
   Graph.GraphDraw.BottomMargin := 75;

   ZeroBFA;

   StartProgress('Scatter plot');
   j := 0;
   GISdb[DB].ApplyGISFilter('CRITERION=' + QuotedStr(Criterion1) + ' OR CRITERION=' + QuotedStr(Criterion2));
   while not GISdb[DB].MyData.eof do begin
      if (j mod 25 = 0) then UpdateProgressBar(j/Tiles.Count);
      inc(j);
      GISdb[DB].EmpSource.Enabled := false;
      Tile := GISdb[DB].MyData.GetFieldByNameAsString('DEMIX_TILE');
      TileID := Tiles.IndexOf(Tile);
      if TileID > -1 then begin
         Crit := GISdb[DB].MyData.GetFieldByNameAsString('CRITERION');
         if (Crit = Criterion1) then CritID := 1 else CritID := 2;
         for i := 1 to NumDEMIXtestDEM do begin
            val := GISdb[DB].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
            demix_bfa[TileID,i,CritID] := val;
         end;
      end;
      GISdb[DB].MyData.Next;
   end;

   OpenColorFiles(Graph,DEMs,'All_dems_' + Criterion1 + '_' + Criterion2);

   NPts := 0;
   for I := 0 to MaxTiles do
      for j := 1 to NumDEMIXtestDEM do begin
         if (demix_bfa[i,j,1] > 0) and (demix_bfa[i,j,2] > 0) then begin
             v[1] := demix_bfa[i,j,1];
             v[2] := demix_bfa[i,j,2];
             BlockWrite(rfile[j],V,1);
             BlockWrite(rfile[0],V,1);
             if (j = 1) then inc(NPts);
         end;
      end;
   for i := 0 to NumDEMIXtestDEM do CloseFile(rfile[i]);
   Graph.GraphDraw.LLcornerText := 'n=' + IntToStr(NPts);
   Graph.RedrawDiagram11Click(Nil);
   Tiles.Destroy;
   EndProgress;
   GISdb[db].ClearGISFilter;
   GISdb[DB].EmpSource.Enabled := true;
*)
end;


procedure HistogramsAllCriteria(db : integer);
var
   Criteria,DEMs : tStringList;
   i,j : integer;
   Graph : tThisBaseGraph;
begin
   GISdb[DB].EmpSource.Enabled := false;
   Criteria := GISdb[DB].MyData.ListUniqueEntriesInDB('CRITERION');
   DEMs := tStringList.Create;
   for I := 1 to NumDEMIXtestDEM do DEMs.Add(DEMIXshort[i]);

   for I := 0 to pred(Criteria.Count) do begin
      GISdb[DB].ApplyGISFilter('CRITERION=' + QuotedStr(Criteria.Strings[i]));
      Graph := GISdb[DB].CreateHistogramFromDataBase(true,DEMs,false);

      for j := 1 to NumDEMIXtestDEM do begin
         Graph.GraphDraw.Symbol[j].Color := (DEMIXColorFromDEMName(DEMIXShort[j]));
         Graph.GraphDraw.FileColors256[j] := (DEMIXColorFromDEMName(DEMIXShort[j]));
      end;
      Graph.RedrawDiagram11Click(Nil);
   end;
   Criteria.Destroy;
   DEMs.Destroy;
   GISdb[DB].ClearGISFilter;
   GISdb[DB].EmpSource.Enabled := true;
end;


  {$I demix_graphs.inc}


procedure ScatterPlotForClusters(DBonTable : integer;  Field1,Field2 : shortstring);
var
   i : integer;
   Color : tColor;
   fName : shortstring;
   Graph : tThisBaseGraph;
   RealLegend : tStringList;
begin
   if GISdb[DBonTable].MyData.FieldExists('CLUSTER') and GISdb[DBonTable].MyData.FieldExists(Field1) then begin
       for I := 1 to 15 do begin
         GISdb[DBonTable].ApplyGISFilter('DEM=' + QuotedStr('COP') + ' AND CLUSTER=' + IntToStr(i));
         if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            Color := GISdb[DBonTable].MyData.TColorFromTable;
            fName := GISdb[DBonTable].MyData.Filter + '_(n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ')';
            if (i=1) then begin
               Graph := GISdb[DBonTable].CreateScatterGram(fName,Field1,Field2,Color);
               RealLegend := tStringList.Create;
            end
            else GISdb[DBonTable].AddSeriesToScatterGram(fName,Graph,Color,Field1,Field2);
         end;
      end;
      AddTopLabelToGraph(Graph,DBonTable);
      Graph.AnimateGraph(False,false);
   end;
   GISdb[DBonTable].EmpSource.Enabled := true;
   GISdb[DBonTable].ClearGISFilter;
end;

procedure MultipleScatterPlotsForCluster(dbOnTable : integer);
begin
   ScatterPlotForClusters(DBonTable,'COP_PDF', 'ALOS_PDF');
   ScatterPlotForClusters(DBonTable,'BARREN_PC', 'AVG_SLOPE');
   ScatterPlotForClusters(DBonTable,'FOREST_PC', 'AVG_SLOPE');
   CombineAllPanelGraphs;
end;



end.
