unit demix_graphs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

//{$Define ExDEMIXexperimentalOptions}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
   {$Define RecordDEMIX_OpenGraph}
   {$Define RecordDEMIX_evaluations_graph}
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
    StrUtils,dbGrids,
    VCL.ExtCtrls,VCL.Forms, VCL.Graphics, VCL.Controls,
    WinAPI.Windows,
    Petmar,Petmar_types,BaseGraf,
    DEMDefs;

//DEMIX graphs

   procedure DEMIX_evaluations_graph(DBonTable : integer; Evaluations : boolean = true);


{$IfDef ExDEMIXexperimentalOptions}
{$Else}


   procedure DEMIX_AreaAverageScores_graph(DBonTable : integer);



   function MakeHistogramOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;
   function GraphAverageScoresByTile(DB : integer; TileList,CriteriaList : tStringList): tThisBaseGraph;
   procedure MultipleBestByParametersSortByValue(DBonTable,Option : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing,TileParameters : tStringList; ByPointFilters : boolean = false);
   procedure BestDEMSbyCategory(DBonTable : integer);
   function DEMIX_SSIM_FUV_single_tile_graph(DBonTable : integer; tile : shortstring) :tThisBaseGraph;
   function DEMIXwineContestScoresGraph(DBonTable : integer; XScalelabel : shortstring; MinHoriz : float32 = 0.5; MaxHoriz : float32 = 5.5) : tThisBaseGraph;

   procedure DEMIXMeanMedianHistograms(db : integer);
   procedure DEMIX_graph_best_in_Tile(DBonTable : integer; SortByArea : boolean);
   procedure ModeSTDPlot(DBonTable : integer);
   procedure DEMIXMeanMedianModeHistograms(db : integer);
   procedure DEMIXwineContestCriterionGraph(What,DBonTable : integer; AreaList : tStringList = nil; CriteriaUsed : tStringList = nil; LandTypePresent : tStringList = nil; DEMsPresent : tStringList = nil);
{$EndIf}

procedure DEMIX_SSIM_FUV_GraphSettings(var Graph : tThisBaseGraph; lltext : shortstring; Ncrits : integer);
procedure SSIM_FUV_scatterplot(db : integer; theCrit : shortstring);
procedure HistogramsAllCriteria(db : integer);
procedure WinningPercentages(db : integer);
procedure HistogramsByQuantile(DB : integer);

procedure BestBySlopeRough(dbOnTable : integer);


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   DEMIX_control,DEMIX_Definitions;


procedure HistogramsByQuantile(DB : integer);
var
   Criteria : tStringList;
   pn,i : integer;
begin
   try
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph in'); {$EndIf}
      GetDEMIXpaths(False);
      GISdb[DB].EmpSource.Enabled := false;
      Criteria := GISdb[DB].MyData.UniqueEntriesInDB('CRITERION');
      if (Criteria.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
         for i := 0 to Pred(Criteria.Count) do begin
            GISdb[DB].EmpSource.Enabled := false;
            GISdb[db].ApplyGISFilter('CRITERION=' + QuotedStr(Criteria.Strings[i]));
            GISdb[db].PutInQuartilesBasedOnExistingSort(10);
            TileCharateristicsWhiskerPlotsByCluster(DB,false,Criteria.Strings[i]);
         end;
      end;
   finally
      //GISdb[DB].ShowStatus;
      GISdb[db].ClearGISFilter;
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph out'); {$EndIf}

end;


function AdjustFilterForDTMandAll(DB : integer; Base : shortstring) : shortstring;
begin
   Result := Base;
   if GISdb[DB].MyData.FieldExists('REF_TYPE') then Result := Result +  ' AND REF_TYPE=' + QuotedStr('DTM');
   if GISdb[DB].MyData.FieldExists('LAND_TYPE') then Result := Result +  ' AND LAND_TYPE=' + QuotedStr('ALL');
end;


procedure SetHorizAxisForDEMIXscores(var Graph : tThisBaseGraph);
begin
    Graph.GraphDraw.MinHorizAxis := 0.5;
    Graph.GraphDraw.MaxHorizAxis := NumDEMIXtestDEM + 0.5;
end;


procedure DEMIX_SSIM_FUV_GraphSettings(var Graph : tThisBaseGraph; lltext : shortstring; Ncrits : integer);
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


function OpenGraphForCriterionScoresOrEvaluations(DBonTable : integer; theCrit,theSort : shortstring; VertAxisNumerical,GraphEvaluation : boolean) : tThisBaseGraph;
var
   i : integer;
   aMinVal,aMaxVal,Range : float64;
   bmp : tMyBitmap;
   Extra : shortstring;
begin
   {$If Defined(RecordDEMIX_OpenGraph)} WriteLineToDebugFile('OpenGraphForCriterionScoresOrEvaluations, Criterion=' + TheCrit + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}

   Result := tThisBaseGraph.Create(Application);
   Result.Width := MDDef.DEMIX_xsize;
   Result.Height := MDDef.DEMIX_ysize;
   Result.GraphDraw.ShowHorizAxis1 := true;
   Result.GraphDraw.HorizLabel := DEMIX_mode_abbreviation(DEMIX_mode) + '_' + theCrit;
   Result.Caption := GISdb[DBonTable].DBName + ' ' + theCrit;
   if VertAxisNumerical then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.FindFieldRange(theSort,aMinVal,aMaxVal);
      Result.GraphDraw.MinVertAxis := aMinVal;
      Result.GraphDraw.MaxVertAxis := aMaxVal;
   end
   else begin
      Result.GraphDraw.MinVertAxis := 0;
      Result.GraphDraw.MaxVertAxis := 10 * GISdb[DBonTable].MyData.FiltRecsInDB + 10;
   end;
   Result.GraphDraw.BottomLegendFName := MDtempDir + 'DEM_DEM_Legend.bmp';
   Result.GraphDraw.BottomMargin := 125;
   bmp := DEMIXTestDEMLegend;
   bmp.SaveToFile(Result.GraphDraw.BottomLegendFName);
   bmp.Destroy;

   Result.GraphDraw.MaxHorizAxis := -99999;
   Result.GraphDraw.MinHorizAxis := 99999;
   if GraphEvaluation then Extra := '' else Extra := '_SCR';

   if GraphEvaluation then begin
      SetHorizAxisForSSIM(dbOnTable,Result,TheCrit);
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
   GISdb[DBonTable].EmpSource.Enabled := false;
   {$If Defined(RecordDEMIX_OpenGraph)} WriteLineToDebugFile('OpenGraphForCriterionScoresOrEvaluations out ' + Result.GraphDraw.AxisRange); {$EndIf}
end;


const
   MaxDEMs = 10;
   MaxTiles = 2000;
   NumGridParams = 10;
var
   rfile : array[0..MaxDEMs] of File;
   bfa : array[0..MaxTiles,1..MaxDEMs,1..2] of float32;


const
   GridParams : array[1..NumGridParams] of shortstring = ('ELEV','HILL','SLOPE','RUFF','RRI','TPI','ACCUM','WETIN','HAND','LS');


procedure ZeroBFA;
var
   i,j,k : integer;
begin
   for I := 0 to MaxTiles do
      for j := 1 to MaxDEMs do
         for k := 1 to 2 do
            bfa[i,j,k] := 0;
end;



procedure WinningPercentages(db : integer);
const
   MaxCrit = 25;
   CompDEM = 5;
   TheDEMs : array[1..5] of shortstring = ('ALOS','FABDEM','TANDEM','DILUV','DELTA');
var
   Criteria : tStringList;
   Criterion,Outcome,TStr : shortstring;
   i,j,k,CritID,Total,NumDEMs : integer;
   bfa : array[0..MaxCrit,1..CompDEM,1..3] of int16;
   GraphDB : tStringList;
   lowx,highx,y : float32;
   Color : tColor;
   fName : PathStr;
   Graph : tThisBaseGraph;


         procedure AddOutcome(aContest : shortstring; DEM : integer);
         var
            oc : integer;
         begin
            if GISdb[DB].MyData.FieldExists(aContest) then begin
               Outcome := GISdb[DB].MyData.GetFieldByNameAsString(aContest);
               if Outcome = 'COP' then oc := 1
               else if Outcome = 'TIE' then oc := 2
               else oc := 3;
               inc(bfa[CritID,DEM,oc]);
               NumDEMs := DEM;
            end;
         end;


begin
   GISdb[DB].EmpSource.Enabled := false;
   Criteria := GISdb[DB].MyData.UniqueEntriesInDB('Criterion');

   for i := 0 to MaxCrit do
      for j := 1 to CompDEM do
         for k := 1 to 3 do bfa[i,j,k] := 0;

   StartProgress('Winning records');
   j := 0;
   GISdb[DB].MyData.First;
   while not GISdb[DB].MyData.eof do begin
      if (j mod 25 = 0) then UpdateProgressBar(j/GISdb[DB].MyData.FiltRecsInDB);
      inc(j);
      GISdb[DB].EmpSource.Enabled := false;
      Criterion := GISdb[DB].MyData.GetFieldByNameAsString('CRITERION');
      CritID := Criteria.IndexOf(Criterion);
      if (CritID > -1) then begin
         AddOutcome('COP_ALOS',1);
         AddOutcome('COP_FABDEM',2);
         AddOutcome('COP_TANDEM',3);
         AddOutcome('COP_DILUV',4);
         AddOutcome('COP_DELTA',5);
      end;
      GISdb[DB].MyData.Next;
   end;
   EndProgress;

   GraphDB := tStringList.Create;
   GraphDB.Add('X1,X2,Y,COLOR,LABEL');

   y := 1;
   for i := 0 to pred(Criteria.Count) do begin
      for j := 1 to NumDEMs do begin
         Total := 0;
         for k := 1 to 3 do Total := Total + bfa[i,j,k];
         lowx := 0;
         for k := 1 to 3 do begin
            Highx := LowX + 100 * bfa[i,j,k] / Total;
            if (j = 2) and (k = 2) then TStr := Criteria[i] else TStr := '';
            if k=1  then Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName('COP'))
            else if k = 3 then Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(theDEMs[j]))
            else Color := clWhite;
            GraphDB.Add(RealToString(lowx,-8,-2) + ',' + RealToString(Highx,-8,-2) + ',' + RealToString(y,-8,-2) + ',' + IntToStr(Color) + ',' + TStr);
            LowX := Highx;
         end;
         y := y + 1;
      end;
      y := y + 1;
   end;
   //GraphDB.SaveToFile('c:\temp\results.CSV');
   fName := NextFileNumber(MDTempDir,'bar_graph_','.dbf');
   StringList2CSVtoDB(GraphDB,fName,true);
   Graph := tThisBaseGraph.Create(Application);
   Graph.GraphDraw.MaxHorizAxis := 100;
   Graph.GraphDraw.MaxVertAxis := y - 1;
   Graph.GraphDraw.GraphAxes := XPartGridOnly;
   Graph.GraphDraw.LeftMargin := 160;
   Graph.BarGraphDBName := fName;
   Graph.RedrawDiagram11Click(Nil);
end;



procedure BestBySlopeRough(dbOnTable : integer);
var
   Graph : tThisBaseGraph;
   Criteria : tStringList;
   pn : integer;
   Rank,cf : shortString;
   Color : tColor;
   rfile : file;
   v : array[1..3] of float32;

   function CreateGraph(Criterion,DEM1,DEM2 : shortstring) : tThisBaseGraph;
   begin
      Result := nil;
      cf := DEM1 + '_' + DEM2;
      if GISdb[DBonTable].MyData.FieldExists(cf) then begin
         Result := tThisBaseGraph.Create(Application);
         Result.GraphDraw.VertLabel := 'Tile Average Slope (%)';
         Result.GraphDraw.HorizLabel := 'Tile Percent Barren (%)';
         Result.GraphDraw.LLCornerText := Criterion;
         Result.OpenXYColorFile(rfile);
         GISdb[DBonTable].ApplyGISFilter('CRITERION=' + QuotedStr(Criterion));
         GISdb[DBonTable].EmpSource.Enabled := false;
         while not GISdb[DBonTable].MyData.eof do begin
            v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('BARREN_PC');
            v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('AVG_SLOPE');
            Rank := GISdb[DBonTable].MyData.GetFieldByNameAsString(cf);
            if Rank = DEM1 then v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM1))
            else if Rank = 'TIE' then v[3] := clSilver
            else v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM2));
            BlockWrite(rfile,v,1);
            GISdb[DBonTable].MyData.Next;
         end;
         CloseFile(rfile);
         Result.AutoScaleAndRedrawDiagram(true,true);
      end;
   end;

begin
   try
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestBySlopeRough in,  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
      GetDEMIXpaths(False);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Criteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
      Criteria.Sort;
      if (Criteria.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
         for pn := 0 to pred(Criteria.Count) do begin
            CreateGraph(Criteria.Strings[pn],'COP','ALOS');
            CreateGraph(Criteria.Strings[pn],'COP','TANDEM');
            CreateGraph(Criteria.Strings[pn],'COP','FABDEM');
            CreateGraph(Criteria.Strings[pn],'COP','DILUV');
            CreateGraph(Criteria.Strings[pn],'COP','DELTA');
         end;
      end;
   finally
      GISdb[DBonTable].ClearGISFilter;
      Criteria.Destroy;
      GISdb[DBonTable].ShowStatus;
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestBySlopeRough out'); {$EndIf}

end;


procedure OpenColorFiles(Graph : tThisBaseGraph; fName : shortstring);
var
   i : integer;
begin
   for i := 0 to NumDEMIXtestDEM do begin
      if (i = 0) then Graph.OpenDataFile(rfile[i],clSilver, fName + '_graph_data_')
      else Graph.OpenDataFile(rfile[i],ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[i])),fName + '_graph_data_');
      Graph.GraphDraw.Symbol[succ(i)].DrawingSymbol := FilledBox;
      Graph.GraphDraw.Symbol[succ(i)].Size := DEMIXsymsize;
   end;
end;


procedure AddDrawingLayers(DBonTable : integer; Graph : tThisBaseGraph; ConstantColor,Evaluations : boolean; theSort : shortstring);
//Contant color puts everything in the same color, instead of colored by DEM
//Evaluations plots the criteria evaluations, otherwise the scores (opinions)
//if theSort is blank, the tiles or areas will be evenly spaced so they do not overprint
//if theSort is a field in the DB, the tiles or areas will be spaced on a numerical axis by the field value
var
   NPts,i,j,TileID : integer;
   v : array[1..2] of float32;
   Extra,Tile : shortstring;
   Tiles : tStringList;
   val,y : float32;
begin
   y := 5;
   if Evaluations then Extra := '' else Extra := '_SCR';
   Tiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');

   OpenColorFiles(Graph,'eval');
   StartProgress('Graph');
   GISdb[DBonTable].MyData.First;
   j := 0;
   NPts := 0;
   while not GISdb[DBonTable].MyData.eof do begin
      if (j mod 25 = 0) then UpdateProgressBar(j/Tiles.Count);
      inc(j);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Tile := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
      TileID := Tiles.IndexOf(Tile);
      for i := 1 to NumDEMIXtestDEM do begin
         if GISdb[DBonTable].MyData.FieldExists(DEMIXShort[i]) then begin
            val := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
            bfa[TileID,i,1] := val;
            if (theSort = '') then bfa[TileID,i,2] := y
            else bfa[TileID,i,2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(theSort);
         end;
      end;
      y := y + 10;
      GISdb[DBonTable].MyData.Next;
   end;

   for I := 0 to pred(Tiles.Count) do begin
      for j := 1 to NumDEMIXtestDEM do begin
         if GISdb[DBonTable].MyData.FieldExists(DEMIXShort[j]) and (bfa[i,j,1] > 0) then begin
             v[1] := bfa[i,j,1];
             v[2] := bfa[i,j,2];
             BlockWrite(rfile[j],V,1);
             BlockWrite(rfile[0],V,1);
             if (j = 1) then inc(NPts);
         end;
      end;
   end;
   for i := 0 to NumDEMIXtestDEM do {if DEMIXDEMinUse[i] then} CloseFile(rfile[i]);
   Graph.RedrawDiagram11Click(Nil);
   Tiles.Destroy;
   EndProgress;
   GISdb[DBonTable].EmpSource.Enabled := true;
   //Graph.GraphDraw.LLcornerText := 'n=' + IntToStr(NPts);
   Graph.GraphDraw.HorizLabel := DEMIX_mode_abbreviation(DEMIX_mode) + ' ' + Graph.GraphDraw.HorizLabel +  '  (n=' + IntToStr(NPts) + ')';
end;


procedure DEMIX_evaluations_graph(DBonTable : integer; Evaluations : boolean = true);
//for each criterion, graph of evaluations sorted by parameter, and with tile names
var
   Criteria : tStringList;
   pn : integer;


      function GraphForOneCriterionWithTileNames(Numerical : boolean; theCrit,TheSort,GraphName : shortstring) : tThisBaseGraph;
      var
        y,i,j,DEM : integer;

            procedure StartGraph;
            begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               Result := OpenGraphForCriterionScoresOrEvaluations(dbOnTable,theCrit,TheSort,Numerical,Evaluations);
               if (DEMIXVertAxisLabel = '') then Result.GraphDraw.VertLabel := GraphName
               else Result.GraphDraw.VertLabel := RemoveUnderscores(DEMIXVertAxisLabel);  // + '  (n=' + IntToStr(GISdb[dbOnTable].MyData.FiltRecsInDB) + ')';
               Result.GraphDraw.ShowHorizAxis1 := false;
               Result.GraphDraw.GraphLeftLabels := tStringList.Create;
               if (theSort = '') then Result.GraphDraw.GraphAxes := XPartGridOnly;
            end;

      var
         Bitmap : tMyBitmap;
      begin {GraphForOneCriterionWithTileNames}
         {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('GraphForOneCriterionWithTileNames, Criterion=' + TheCrit + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
         StartGraph;
         (*
         GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr('N39PW113H'));
         GISdb[DBonTable].ApplyGISFilter('AREA=' + QuotedStr('TRENTINO'));
         GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr('N28VW018B') + ' OR DEMIX_TILE=' + QuotedStr('N39RW113J')  +
              ' OR DEMIX_TILE=' + QuotedStr('N39UW098F') + ' OR DEMIX_TILE=' + QuotedStr('N46XE012B'));
         *)
         AddDrawingLayers(DBonTable,Result,true,Evaluations,TheSort);
         Result.RedrawDiagram11Click(Nil);
      end {GraphForOneCriterionWithTileNames};


      procedure MakeAllGraphs(Numerical : boolean; TheSort : shortstring; GraphName : shortstring);
      var
         j : integer;
         Graph : tThisBaseGraph;
         BigGraph : tstringList;
         fName : PathStr;
      begin
         {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('MakeAllGraphs, Criteria=' + IntToStr(Criteria.Count) + ' n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
         if (TheSort = '') or GISdb[DBonTable].MyData.FieldExists(TheSort) then begin
            for j := 0 to pred(Criteria.Count) do begin
               if (Criteria.Count > 0) then GISdb[DBonTable].ApplyGISFilter('CRITERION=' + QuotedStr(Criteria.Strings[j]));
               AddFilteredRankID(DBonTable);
               Graph := nil;
               Graph := GraphForOneCriterionWithTileNames(Numerical,Criteria.Strings[j],theSort,GraphName);
               fName := DEMIX_mode_abbreviation(DEMIX_mode) + '_' + Criteria.Strings[j] + '_' + theSort;
               if MovieByTestDEM then begin
                  Graph.AnimateGraph(true,fName);
               end;
               if PanelsByTestDEM then begin
                  Graph.AnimateGraph(false,fName);
               end;
               if (Criteria.Count > 1) then begin
                  if DEMIX_combined_graph then begin
                     fName := NextFileNumber(MDTempDir,GraphName,'.png');
                     MakeBigBitmap(BigGraph,'',fName,Criteria.Count);
                  end;
                  GISdb[DBonTable].ClearGISFilter;
               end;
            end;
         end
         else begin
            MessageToContinue('Field missing for sort ' + TheSort);
         end;
      end;

begin {DEMIX_evaluations_graph}
   try
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph in,  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
      GetDEMIXpaths(False);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Criteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
      Criteria.Sort;
      if (Criteria.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
         if (YAxisWhat = yasSlope) then MakeAllGraphs(true,'AVG_SLOPE','Tile average slope (%)')
         else if (YAxisWhat = yasBarren) then MakeAllGraphs(true,'BARREN_PC','Tile barren (%)')
         else if (YAxisWhat = yasRuff) then MakeAllGraphs(true,'AVG_ROUGH','Tile average roughness (%)')
         else if (YAxisWhat = yasRelief) then MakeAllGraphs(true,'RELIEF','Tile average relief (m)')
         else if (YAxisSort in [yasName,yasBestEval]) then MakeAllGraphs(false,'','Tile best evaluation');
      end;
   finally
      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].ShowStatus;
      EndDEMIXProcessing;
   end;
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
   Criterion1 := TheCrit + '_SSIM';
   Criterion2 := TheCrit + '_FUV';
   GISdb[DB].EmpSource.Enabled := false;
   Tiles := GISdb[DB].MyData.UniqueEntriesInDB('DEMIX_TILE');
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
            bfa[TileID,i,CritID] := val;
         end;
      end;
      GISdb[DB].MyData.Next;
   end;

   OpenColorFiles(Graph,'All_dems_' + Criterion1 + '_' + Criterion2);

   NPts := 0;
   for I := 0 to MaxTiles do
      for j := 1 to NumDEMIXtestDEM do begin
         if (bfa[i,j,1] > 0) and (bfa[i,j,2] > 0) then begin
             v[1] := bfa[i,j,1];
             v[2] := bfa[i,j,2];
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
end;


procedure HistogramsAllCriteria(db : integer);
var
   Criteria,DEMs : tStringList;
   i,j : integer;
   Graph : tThisBaseGraph;
begin
   GISdb[DB].EmpSource.Enabled := false;
   Criteria := GISdb[DB].MyData.UniqueEntriesInDB('CRITERION');
   DEMs := tStringList.Create;
   for I := 1 to NumDEMIXtestDEM do DEMs.Add(DEMIXshort[i]);

   for I := 0 to pred(Criteria.Count) do begin
      GISdb[DB].ApplyGISFilter('CRITERION=' + QuotedStr(Criteria.Strings[i]));
      Graph := GISdb[DB].CreateHistogramFromDataBase(true,DEMs,false);
      for j := 1 to NumDEMIXtestDEM do begin
         //Graph.GraphDraw.    := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[i]));
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


end.
