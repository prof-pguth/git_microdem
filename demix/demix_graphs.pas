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
   //{$Define RecordDEMIX}
   //{$Define RecordDEMIX_OpenGraph}
   //{$Define RecordDEMIX_evaluations_graph}
   //{$Define RecordDEMIX_criteria_colors}
   //{$Define RecordDEMIXWins}
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

   procedure DEMIX_evaluations_graph(DBonTable,YAxisWhat : integer; DEMs,Criteria : tStringList; Evaluations : boolean = true);

procedure DEMIX_SSIM_FUV_GraphSettings(var Graph : tThisBaseGraph; DBonTable : integer; lltext : shortstring; Ncrits : integer);
procedure SSIM_FUV_scatterplot(db : integer; theCrit : shortstring);
procedure HistogramsAllCriteria(db : integer);
function WinningPercentagesComparedToCOP(db : integer; CompareDEM : shortstring; Criteria,inUseDEMs : tStringList; HLabel : shortstring = '') : tThisBaseGraph;
procedure WinningPercentagesComparedToCOPFilteredBySlope(db : integer; CompareDEM : shortstring; Criteria,UseDEMs,GeomorphFilters : tStringList);

procedure WhiskerPlotsByCluster(DB : integer);
procedure DEMWinningPercentageGraph(DBonTable : integer; DEMs : tStringList; aField : shortstring);

procedure BestBySlopeRough(dbOnTable : integer; Criteria : tStringList; Winners : boolean; Param1,Param2,ValueParam : shortstring);
procedure MultipleScatterPlotsForCluster(dbOnTable : integer);
procedure ScatterPlotForClusters(DBonTable : integer;  Field1,Field2 : shortstring);
function AverageScoresGraph(db : integer; DEMs : tStringList; HL : shortstring; aDEMIXLegend : boolean; MinHoriz,MaxHoriz : integer) : tThisBaseGraph;

procedure AddTopLabelToGraph(var Graph : tThisBaseGraph; DBonTable : integer);
function PlotBestEvalVersusPercentileMultipleCriteria(DBonTable : integer; Criteria : tStringList; Evaluations : boolean = true;
           TopLabel : shortstring = ''; HL : shortstring = ''; VertAxisField : shortstring = '') : tThisBaseGraph;
procedure FilterJustOneGraph(DBonTable : integer; Criteria,GeomorphFilters : tStringList; Evaluations : boolean = true; HL : shortstring = '');
procedure WinningComparedToBaseDEM(db : integer; BaseDEM : shortstring; GeomorphFilters,Criteria,DEms : tStringList);
procedure BestEvalGraphPerCriterionMultipleFilters(db : integer; GeomorphFilters,Criteria : tStringList; CriteriaFamily : shortstring);


function NumTilesString(DB : integer) : shortstring;


{$IfDef ExDEMIXexperimentalOptions}
{$Else}
   procedure DEMIX_AreaAverageScores_graph(DBonTable : integer; DEMs : tStringList);

   function MakeHistogramOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;
   function GraphAverageScoresByTile(DB : integer; DEMs,TileList,CriteriaList : tStringList): tThisBaseGraph;
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


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   DEMIX_control,DEMIX_Definitions;


function NumTilesString(DB : integer) : shortstring;
begin
   Result :=  ' (tiles=' + IntToStr(GISdb[DB].NumUniqueEntriesInDB('DEMIX_TILE')) + ')';
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
   gr : array[0..25] of tThisBaseGraph;
   Legend,BigBitmap : tMyBitmap;
begin
   if GISdb[db].MyData.FieldExists('AVG_SLOPE') then begin
      try
         GetDEMIXpaths(False);
         for i := 0 to pred(GeomorphFilters.Count) do begin //filters to tile characteristics
            SetColorForWaiting;
            if GeomorphFilters.Strings[i] = '(None)' then GISdb[db].ClearGISFilter
            else GISdb[db].ApplyGISFilter(GeomorphFilters.Strings[i]);
            HL := DEMIXModeName + '_' + CriteriaFamily + '_Evaluation';
            TopLabel := GISdb[db].MyData.Filter + NumTilesString(DB);
            gr[i] := PlotBestEvalVersusPercentileMultipleCriteria(DB,Criteria,true,TopLabel,HL);
            if MDDef.DEMIX_combined_graph then AddGraphToBigBitmap(succ(i),GeomorphFilters.Count,1,gr[i],BigBitmap);
         end;
         if MDDef.DEMIX_combined_graph then begin
            Legend := gr[0].MakeLegend(gr[0].GraphDraw.LegendList,false);
            FinishBigMap(BigBitmap,Legend,'',true);
         end;
      finally
         EndDEMIXProcessing;
         GISdb[db].ClearGISFilter;
         GISdb[db].ShowStatus;
      end;
   end
   else MessageToContinue('Add tile characters to DB');
end;




procedure WinningComparedToBaseDEM(db : integer; BaseDEM : shortstring; GeomorphFilters,Criteria,DEms : tStringList);
var
   i,j : integer;
   HL : shortstring;
   gr : array[0..25] of tThisBaseGraph;
   Legend,BigBitmap : tMyBitmap;
begin
   if GISdb[db].MyData.FieldExists('AVG_SLOPE') then begin
      try
         GetDEMIXpaths(False);
         j := 0;
         for i := 0 to pred(GeomorphFilters.Count) do begin
            GISdb[db].ApplyGISFilter(GeomorphFilters.Strings[i]);
            if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
               HL := GISdb[DB].MyData.Filter + ' (tiles=' + IntToStr(GISdb[DB].NumUniqueEntriesInDB('DEMIX_TILE')) + ')';
               inc(j);
               gr[j] := WinningPercentagesComparedToCOP(db,BaseDEM,Criteria,DEMs,HL);
               if MDDef.DEMIX_combined_graph then AddGraphToBigBitmap(j,GeomorphFilters.Count,1,gr[j],BigBitmap);
            end;
         end;
         if MDDef.DEMIX_combined_graph then begin
            Legend := gr[1].MakeLegend(gr[1].GraphDraw.LegendList,false);
            FinishBigMap(BigBitmap,Legend);
         end;
      finally
         EndDEMIXProcessing;
         GISdb[db].ClearGISFilter;
         GISdb[db].ShowStatus;
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
      GISdb[db].ClearGISFilter;
      EndDEMIXProcessing;
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
   if GISdb[DB].MyData.FieldExists('REF_TYPE') then Result := Result +  ' AND REF_TYPE=' + QuotedStr('DTM');
   if GISdb[DB].MyData.FieldExists('LAND_TYPE') then Result := Result +  ' AND LAND_TYPE=' + QuotedStr('ALL');
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


function OpenGraphForCriterionScoresOrEvaluations(DBonTable : integer; DEMs : tStringList; HorizAxisLabel,VertAxisField : shortstring; GraphEvaluation,BestCriteria : boolean) : tThisBaseGraph;
var
   i : integer;
   aMinVal,aMaxVal,Range : float64;
begin
   {$If Defined(RecordDEMIX_OpenGraph)} WriteLineToDebugFile('OpenGraphForCriterionScoresOrEvaluations, Criterion=' + HorizAxisLabel + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
   Result := tThisBaseGraph.Create(Application);
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



procedure DEMWinningPercentageGraph(DBonTable : integer; DEMs : tStringList; aField : shortstring);
var
   Graph : tThisBaseGraph;
   i,j,nt : integer;
   rfile : file;
   v : array[1..3] of float32;
begin
   MessageToContinue('DEMWinningPercentageGraph disabled');
(*
   if GISdb[DBonTable].MyData.FieldExists('AREA') then aField := 'AREA';
   if GISdb[DBonTable].MyData.FieldExists('CRITERION') then aField := 'CRITERION';
   for j := 1 to NumDEMIXtestDEM do UseRetiredDEMs[j] := true;

   Graph := OpenGraphForCriterionScoresOrEvaluations(DBonTable,DEMs,'','',true,false);
   Graph.GraphDraw.ShowGraphLeftLabels := true;
   Graph.GraphDraw.MaxHorizAxis := 100;
   Graph.GraphDraw.MinHorizAxis := -2;
   Graph.GraphDraw.HorizLabel := 'DEM winnning percentage by ' + aField;

   Graph.OpenXYColorFile(rfile);
   Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
   i := 1;
   nt := GISdb[DBonTable].MyData.FiltRecsInDB;
   Graph.GraphDraw.MaxVertAxis := succ(nt);
   while not GISdb[DBonTable].MyData.eof do begin
      v[2] := i;
      Graph.GraphDraw.GraphLeftLabels.Add(IntToStr(i) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString(aField));
      for j := 0 to DEMs.Count do begin
          v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMs.Strings[j] + '_WIN');
          v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMs.Strings[j]));
          BlockWrite(rfile,v,1);
      end;
      inc(i);
      GISdb[DBonTable].MyData.Next;
   end;
   CloseFile(rfile);
   Graph.GraphDraw.GraphAxes := XPartGridOnly;
   Graph.GraphDraw.ShowHorizAxis0 := false;
   Graph.GraphDraw.BottomMargin := 125;
   Graph.GraphDraw.LeftMargin := 290;
   Graph.GraphDraw.ShowGraphLeftLabels := true;
   Graph.RedrawDiagram11Click(Nil);
*)
end;


var
   rfile : array[0..MaxDEMs] of File;
   demix_bfa : array[0..MaxTiles,1..MaxDEMs,1..2] of float32;


procedure ZeroBFA;
var
   i,j,k : integer;
begin
   for I := 0 to MaxTiles do
      for j := 1 to MaxDEMs do
         for k := 1 to 2 do
            demix_bfa[i,j,k] := 0;
end;


procedure WinningPercentagesComparedToCOPFilteredBySlope(db : integer; CompareDEM : shortstring; Criteria,UseDEMs,GeomorphFilters : tStringList);
var
   i : integer;
   BigBitMap,Legend : tMyBitmap;
   gr : tThisBaseGraph;
begin
   if GISdb[db].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE') then begin
      for i := 0 to pred(GeomorphFilters.Count) do begin
          {$IfDef RecordDEMIX} WriteLineToDebugFile('WinningPercentagesComparedToCOPFilteredBySlope ' + IntToStr(i) + '  ' + GeomorphFilters[i]); {$EndIf};
          GISdb[db].ApplyGISFilter(GeomorphFilters[i]);
          gr := WinningPercentagesComparedToCOP(db,CompareDEM,Criteria,UseDEMs);
          if MDDef.DEMIX_combined_graph then AddGraphToBigBitmap(succ(i),GeomorphFilters.Count,1,gr,BigBitmap);
      end;
      Legend := Nil;
      if MDDef.DEMIX_combined_graph then FinishBigMap(BigBitmap,Legend);
   end
   else begin
      MessageToContinue('Add tile characteristics to DB');
   end;
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


function AverageScoresGraph(db : integer; DEMs : tStringList; HL : shortstring; aDEMIXLegend : boolean; MinHoriz,MaxHoriz : integer) : tThisBaseGraph;
var
   i,j,k,y,CritID,Total,NumDEMs,tw,MaxTW : integer;
   fName : PathStr;
   GrafFile : tStringList;
   v : array[1..3] of float32;
   aField,aLabel : shortstring;
   DEMsPresent : tStringList;
   Color : array[0..15] of tColor;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AverageScoresGraph in, db=' + GISdb[db].dbName + ' HL=' + HL); {$EndIf};
   Result := tThisBaseGraph.Create(Application);
   Result.Canvas.Font.Style := [fsBold];
   Result.Canvas.Font.Name := Result.FontDialog1.Font.Name;
   Result.Canvas.Font.Size := Result.FontDialog1.Font.Size;

   DEMsPresent := tStringList.Create;
   j := 0;
   for i := 0 to pred(DEMs.Count) do begin
      if GISdb[DB].MyData.FieldExists(DEMs.Strings[i]) then begin
         DEMsPresent.Add(DEMs.Strings[i]);
         Color[j] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMs.Strings[i]));
         inc(j);
      end;
   end;

   MaxTW := 0;
   GISdb[DB].MyData.First;
   while not GISdb[DB].MyData.eof do begin
      GISdb[DB].EmpSource.Enabled := false;
      aLabel := GISdb[DB].MyData.GetFieldByNameAsString('FILTER') + ' (n=' + IntToStr(GISdb[DB].MyData.GetFieldByNameAsInteger('NUM_TILES')) + ')';
      TW := Result.Canvas.TextWidth(aLabel);
      if TW > MaxTW then MaxTw := Tw;
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
   //Result.OpenXYColorFile(rfile);
   Result.GraphDraw.GraphLeftLabels := tStringList.Create;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AverageScoresGraph setup done'); {$EndIf};

   GrafFile := tStringList.Create;
   GrafFile.Add('X,Y,COLOR');

   GISdb[DB].MyData.First;
   y := 1;
   while not GISdb[DB].MyData.eof do begin
      GISdb[DB].EmpSource.Enabled := false;
      v[2] := y;
      Result.GraphDraw.GraphLeftLabels.Add(IntToStr(y) + ',' + GISdb[DB].MyData.GetFieldByNameAsString('FILTER') + ' (n=' + IntToStr(GISdb[DB].MyData.GetFieldByNameAsInteger('NUM_TILES')) + ')');
      for i := 0 to pred(DEMsPresent.Count) do begin
         v[1] := GISdb[DB].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[i]);
         GrafFile.Add(RealToString(v[1],-12,-4) + ',' + IntToStr(y) + ',' + IntToStr(Color[i]));
      end;

      GISdb[DB].MyData.Next;
      y := y + 1;
   end;
   //CloseFile(rfile);

   Result.XYColorDBName := NextFileNumber(MDTempDir,'graph_','.dbf');
   StringList2CSVtoDB(GrafFile,Result.XYColorDBName,true,false,false); 
   Result.GraphDraw.ShowGraphLeftLabels := true;
   if aDEMIXLegend then begin
      Result.Height :=  Result.Height + 100;
      AddDemixLegend(Result,DEMsPresent);
   end;
   DEMsPresent.Destroy;
   Result.RedrawDiagram11Click(Nil);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AverageScoresGraph out'); {$EndIf};
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
      Rank,{cf,}LocStr : shortString;
      Color : tColor;
      rfile : file;
      fName : PathStr;
      MaxEval,BestEval,XS,YS : float32;
      v : array[1..3] of float32;
      Bins : array[1..MaxPossBins,1..MaxPossBins] of tBinRec;
      GraphData : tStringList;
      DiffDist : boolean;
   begin
      Result := nil;
      //cf := DEM1 + '_' + DEM2;
      //if GISdb[DBonTable].MyData.FieldExists(cf) then begin
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
         GISdb[DBonTable].ClearGISFilter;
         GISdb[DBonTable].ShowStatus;
         EndDEMIXProcessing;
      end;
   end
   else begin
      MessageToContinue('Fields missing: ' + Param1 + ' and ' + Param2);
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestBySlopeRough out'); {$EndIf}
end;


procedure OpenColorFiles(Graph : tThisBaseGraph; DEMs : tStringList; fName : shortstring);
var
   i : integer;
begin
   for i := 0 to DEMs.Count do begin
      if (i = 0) then Graph.OpenDataFile(rfile[i],clSilver, fName + '_graph_data_')
      else Graph.OpenDataFile(rfile[i],ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMs.Strings[pred(i)])),fName + '_graph_data_');
      Graph.GraphDraw.Symbol[succ(i)].DrawingSymbol := FilledBox;
      Graph.GraphDraw.Symbol[succ(i)].Size := MDDef.DEMIXsymsize;
   end;
end;


procedure AddDrawingLayers(DBonTable : integer; DEMs : tStringList; Graph : tThisBaseGraph; Evaluations : boolean; theSort : shortstring);
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

      Graph.GraphDraw.LegendList := tStringList.Create;
      Graph.GraphDraw.LegendList.Add('Other');
      for i := 0 to pred(DEMs.Count) do Graph.GraphDraw.LegendList.Add(DEMs[i]);
      OpenColorFiles(Graph,DEMs,'eval');
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
         for i := 1 to DEMs.Count do begin
            val := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMs.Strings[pred(i)]);
            demix_bfa[TileID,i,1] := val;
            if (theSort = '') then demix_bfa[TileID,i,2] := 100 * y / GISdb[DBonTable].MyData.FiltRecsInDB
            else demix_bfa[TileID,i,2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(theSort);
         end;
         y := y + 1;
         GISdb[DBonTable].MyData.Next;
      end;

      for I := 0 to pred(Tiles.Count) do begin
         for j := 1 to DEMs.Count do begin
            if GISdb[DBonTable].MyData.FieldExists(DEMs[pred(j)]) and (demix_bfa[i,j,1] > 0) then begin
                v[1] := demix_bfa[i,j,1];
                v[2] := demix_bfa[i,j,2];
                BlockWrite(rfile[j],V,1);
                BlockWrite(rfile[0],V,1);
                if (j = 1) then inc(NPts);
            end;
         end;
      end;
      for i := 0 to DEMs.Count do CloseFile(rfile[i]);
      Graph.RedrawDiagram11Click(Nil);
      Tiles.Destroy;
      EndProgress;
      GISdb[DBonTable].EmpSource.Enabled := true;
      Graph.GraphDraw.HorizLabel := RemoveUnderscores(Graph.GraphDraw.HorizLabel) +  '  (n=' + IntToStr(NPts) + ')';
   end;
end;


function GraphForOneCriterion(DBonTable : integer; DEMs : tStringList; AllCriteria : boolean; Evaluations : boolean = true; HorizAxisLabel: shortstring = ''; VertAxisField : shortstring = '';
     VertAxisLabel : shortstring = ''; TopLabel : shortstring = '') : tThisBaseGraph;
var
  y,i,j,DEM : integer;
var
   Bitmap : tMyBitmap;
begin
   {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('GraphForOneCriterionWithTileNames, ' + HorizAxisLabel + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   Result := OpenGraphForCriterionScoresOrEvaluations(dbOnTable,DEMs,HorizAxisLabel,VertAxisField,Evaluations,AllCriteria);
   if (VertAxisLabel = '') then VertAxisLabel := 'Best tile evaluation (%)';
   Result.GraphDraw.VertLabel := VertAxisLabel;
   Result.GraphDraw.HorizLabel := HorizAxisLabel;

   Result.GraphDraw.MinHorizAxis := -0.075;
   Result.GraphDraw.MaxHorizAxis := 1.05;

   if (TopLabel <> '') then begin
      Result.GraphDraw.TopLabel := TopLabel;
      Result.GraphDraw.TopMargin := 45;
   end;

   Result.GraphDraw.ShowHorizAxis1 := false;
   Result.GraphDraw.GraphLeftLabels := tStringList.Create;
   Result.GraphDraw.DrawInsideLines := false;
   if not AllCriteria then begin
      AddDrawingLayers(DBonTable,DEMs,Result,Evaluations,VertAxisField);
      Result.RedrawDiagram11Click(Nil);
   end;
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


      procedure AllColors;
      var
         j : integer;
         AllCrits : tStringList;
      begin
         AllCrits := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('CRITERION');
         for j := 0 to pred(AllCrits.Count) do begin
            Criterion := AllCrits.Strings[j];
            GISdb[DBonTable].ApplyGISFilter(PetDBUtils.AddAndIfNeeded(BaseFilter) + 'CRITERION=' + QuotedStr(Criterion));
            Result.GraphDraw.LegendList.Add(AllCrits.Strings[j]);
            Symbol.Color := ConvertTColorToPlatformColor(clSilver);
            Symbol.DrawingSymbol := FilledBox;
            Symbol.Size := 1;
            Result.OpenDataFile(rfile,clSilver);
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
         AllCrits.Free;
      end;


begin
   {$If Defined(RecordDEMIX) or Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('PlotBestEvalVersusPercentileMultipleCriteria in, Criteria=' + IntToStr(Criteria.Count)); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   BaseFilter :=  GISdb[DBonTable].MyData.Filter;
   Result := nil;
   Result := GraphForOneCriterion(DBonTable,Nil,true,Evaluations,HL,VertAxisField,'',TopLabel);
   Result.GraphDraw.LegendList := tStringList.Create;

   //AllColors;

   for j := 0 to pred(Criteria.Count) do begin
      Criterion := Criteria.Strings[j];
      GISdb[DBonTable].ApplyGISFilter(PetDBUtils.AddAndIfNeeded(BaseFilter) + 'CRITERION=' + QuotedStr(Criterion));
      Result.GraphDraw.LegendList.Add(Criteria.Strings[j]);
      Color := DEMIXColorForCriterion(Criterion);
      {$If Defined(RecordDEMIX_criteria_colors)} WriteLineToDebugFile(Criterion + ' color=' + IntToStr(Color) + '  ' + ColorString(Color)); {$EndIf}

      Symbol.Color := ConvertTColorToPlatformColor(Color);
      Symbol.DrawingSymbol := FilledBox;
      Symbol.Size := MDDef.DemixSymSize;

      Result.OpenDataFile(rfile,Color);
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
   {$If Defined(RecordDEMIX) or Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('PlotBestEvalVersusPercentileMultipleCriteria out, LegendCount=' + IntToStr(Result.GraphDraw.LegendList.Count)); {$EndIf}
end;


procedure FilterJustOneGraph(DBonTable : integer; Criteria,GeomorphFilters : tStringList; Evaluations : boolean = true; HL : shortstring = '');

      function AGraph(Crit : integer) : tThisBaseGraph;
      var
         k,y,i : integer;
         v : array[1..2] of float32;
         rfile : file;
         aFilter,TStr,Criterion,HL : shortstring;
         Symbol : tFullSymbolDeclaration;
         BaseFilter : shortstring;
      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Agraph in, Crit=' + IntToStr(Crit)); {$EndIf}
         ShowHourglassCursor;
         BaseFilter :=  GISdb[DBonTable].MyData.Filter;
         HL := Petmar_types.BeforeSpecifiedString(GISdb[DBonTable].dbName,'demix');
         Result := nil;
         Result := GraphForOneCriterion(DBonTable,Nil,false,Evaluations,HL,'');
         AddTopLabelToGraph(Result,DBonTable);
         Result.GraphDraw.LegendList := tStringList.Create;
         for k := 0 to pred(GeomorphFilters.Count) do begin
            Criterion := Criteria.Strings[Crit];
            aFilter := PetDBUtils.AddAndIfNeeded(BaseFilter) + 'CRITERION=' + QuotedStr(Criterion);
            if (GeomorphFilters[k] <> '') then aFilter := aFilter + ' AND ' + GeomorphFilters[k];

            GISdb[DBonTable].ApplyGISFilter(aFilter);
            if (GeomorphFilters[k] <> '') then TStr := GeomorphFilters[k]
            else TStr := 'All slopes';
            Result.GraphDraw.HorizLabel := Criterion;
            GISdb[DBonTable].EmpSource.Enabled := false;
            Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors[k mod 15]);
            Symbol.DrawingSymbol := FilledBox;
            Symbol.Size := MDDef.DemixSymSize;
            Result.OpenPointFile(rfile,Symbol);
            y := 0;
            while not GISdb[DBonTable].MyData.eof do begin
               v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('BEST_EVAL');
               v[2] := 100 * y / GISdb[DBonTable].MyData.FiltRecsInDB;
               BlockWrite(rfile,v,1);
               inc(y);
               GISdb[DBonTable].MyData.Next;
            end;
            CloseFile(rfile);
            Result.GraphDraw.LegendList.Add(TStr + '  ' + NumTilesString(DBonTable));
         end;
         Result.GraphDraw.MaxHorizAxis := 1.1;
         Result.GraphDraw.SetShowAllPoints(false);
         Result.GraphDraw.SetShowAllLines(true);
         for i := 1 to MaxGraphSeries do Result.GraphDraw.LineSize256[i] := MDDef.DemixSymSize;
         Result.AutoScaleAndRedrawDiagram(false,false,false,false);
         GISdb[DBonTable].ApplyGISFilter(BaseFilter);
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Agraph out, Crit=' + IntToStr(Crit)); {$EndIf}
      end;

var
   j : integer;
   Legend,BigBitmap : tMyBitmap;
   gr : array[0..25] of tThisBaseGraph;
begin
   if GISdb[DBonTable].MyData.FieldExists('BARREN_PC') and GISdb[DBonTable].MyData.FieldExists('AVG_SLOPE') then begin
     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('FilterJustOneGraph in'); {$EndIf}
      for j := 0 to pred(Criteria.Count) do begin
         gr[j] := AGraph(j);
         if MDDef.DEMIX_combined_graph then AddGraphToBigBitmap(succ(j),Criteria.Count,1,gr[j],BigBitmap);
      end;
      if MDDef.DEMIX_combined_graph then begin
         Legend := gr[0].MakeLegend(gr[0].GraphDraw.LegendList,false);
         FinishBigMap(BigBitmap,Legend);
      end;
      ShowDefaultCursor;
     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('FilterJustOneGraph out'); {$EndIf}
   end
   else begin
      MessageToContinue('Add tile characteristics to DB');
   end;
end;


procedure DEMIX_evaluations_graph(DBonTable,YAxisWhat : integer; DEMs,Criteria : tStringList; Evaluations : boolean = true);
//for each criterion, graph of evaluations sorted by parameter, and with tile names
var
   BaseFilter,RestoreFilter : shortstring;

      procedure MakeAllGraphs(Numerical : boolean; TheSort : shortstring; GraphName : shortstring);
      var
         Legend,BigBitmap : tMyBitmap;
         j : integer;
         gr : array[0..25] of tThisBaseGraph;
         BigGraph : tstringList;
         fName : PathStr;
         aFilter : shortstring;
      begin
         {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('MakeAllGraphs, Criteria=' + IntToStr(Criteria.Count) + ' n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
         if (TheSort = '') or GISdb[DBonTable].MyData.FieldExists(TheSort) then begin
            if (Criteria.Count > 1) and MDDef.DEMIX_combined_graph then BigGraph := tstringList.Create;
            for j := 0 to pred(Criteria.Count) do begin
               aFilter := BaseFilter + 'CRITERION=' + QuotedStr(Criteria.Strings[j]);
               GISdb[DBonTable].ApplyGISFilter(aFilter);
               if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
                  gr[j] := nil;
                  gr[j]  := GraphForOneCriterion(DBonTable,DEMs,Numerical,Evaluations,Criteria.Strings[j],theSort,GraphName);
                  fName := DEMIXmodeName + '_' + Criteria.Strings[j] + '_' + theSort;
                  if MDDef.DEMIX_combined_graph then AddGraphToBigBitmap(succ(j),Criteria.Count,1,gr[j],BigBitmap);


                  if MovieByTestDEM then begin
                     gr[j].AnimateGraph(true,true,fName);
                  end;
                  if MDDef.PanelsByTestDEM then begin
                     gr[j].AnimateGraph(false,true,fName);
                  end;
                  if (Criteria.Count > 1) and MDDef.DEMIX_combined_graph then begin
                     BigGraph.Add(fName);
                  end;
               end;
            end;
            if MDDef.DEMIX_combined_graph then begin
               Legend := gr[0].MakeLegend(gr[0].GraphDraw.LegendList,false);
               FinishBigMap(BigBitmap,Legend);
            end;

            if (Criteria.Count > 1) then begin
               if MDDef.DEMIX_combined_graph then begin
                  fName := NextFileNumber(MDTempDir,GraphName,'.png');
                  MakeBigBitmap(BigGraph,'',fName,Criteria.Count);
               end;
               GISdb[DBonTable].ClearGISFilter;
            end;
         end
         else begin
            MessageToContinue('Field missing for sort ' + TheSort);
         end;
      end;


      procedure GraphsColoredByTile;
      var
         i,j : integer;
         y : integer;
         Slope : float32;
         Graph : tThisBaseGraph;
         v : array[1..3] of float32;
         rfile : file;

            procedure SetColor;
            begin
               Slope := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('AVG_SLOPE');
               if (Slope < MDDef.SlopeFlatBoundary) then v[3] := clBlue
               else if (Slope < MDDef.SlopeGentleBoundary) then v[3] := clLime
               else if (Slope < MDDef.SlopeSteepBoundary) then v[3] := clGreen
               else v[3] := clRed;
            end;

      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('GraphsColoredByTile in yasBestEvalColoredBySlope'); {$EndIf}
         for j := 0 to pred(Criteria.Count) do begin
            Graph := nil;
            Graph := GraphForOneCriterion(DBonTable,DEMs,true,true,Criteria.Strings[j],'');

            Graph.GraphDraw.BottomMargin := 100;
            Graph.GraphDraw.LegendList := tStringList.Create;
            GISdb[DBonTable].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'CRITERION=' + QuotedStr(Criteria.Strings[j]));
            Graph.GraphDraw.LegendList.Add(Criteria.Strings[j]);
            GISdb[DBonTable].EmpSource.Enabled := false;
            Graph.OpenXYColorFile(rfile);
            y := 0;
            while not GISdb[DBonTable].MyData.eof do begin
               v[2] := 100 * y / GISdb[DBonTable].MyData.FiltRecsInDB;
               SetColor;
               v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('BEST_EVAL');
               BlockWrite(rfile,v,1);
               inc(y);
               GISdb[DBonTable].MyData.Next;
            end;
            CloseFile(rfile);
            Graph.AutoScaleAndRedrawDiagram;

         end;
      end;



begin {DEMIX_evaluations_graph}
   try
      BaseFilter := GISdb[DBonTable].MyData.Filter;

      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph in,  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
      GetDEMIXpaths(False);
      GISdb[DBonTable].EmpSource.Enabled := false;
      if (YAxisWhat = yasBestEvalColoredBySlope) then GraphsColoredByTile
      else if (YAxisWhat = yasSlope) then MakeAllGraphs(true,'AVG_SLOPE','Tile average slope (%)')
      else if (YAxisWhat = yasRuff) then MakeAllGraphs(true,'AVG_ROUGH','Tile average roughness (%)')
      else if (YAxisWhat = yasRelief) then MakeAllGraphs(true,'RELIEF','Tile average relief (m)')
      else if (YAxisWhat = yasBarren) then MakeAllGraphs(true,'BARREN_PC','Tile barren (%)')
      else if (YAxisWhat = yasForest) then MakeAllGraphs(true,'FOREST_PC','Tile forest (%)')
      else if (YAxisWhat in [yasName,yasBestEval]) then MakeAllGraphs(false,'','Tile best evaluation');
   finally
      EndDEMIXProcessing(DBonTable);
      GISdb[DBonTable].ApplyGISFilter(BaseFilter);
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
   Graph : tThisBaseGraph;
   RealLegend : tStringList;
begin
   if GISdb[DBonTable].MyData.FieldExists('CLUSTER') and GISdb[DBonTable].MyData.FieldExists(Field1) then begin
       for I := 1 to 15 do begin
         GISdb[DBonTable].ApplyGISFilter('DEM=' + QuotedStr('COP') + ' AND CLUSTER=' + IntToStr(i));
         if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            Color := GISdb[DBonTable].MyData.TColorFromTable;
            if (i=1) then begin
               Graph := GISdb[DBonTable].CreateScatterGram(Field1,Field2,Color);
               Graph.GraphDraw.LegendList := tStringList.Create;
               RealLegend := tStringList.Create;
            end
            else GISdb[DBonTable].AddSeriesToScatterGram(Graph,Color,Field1,Field2);
            RealLegend.Add(GISdb[DBonTable].MyData.Filter + ' (n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ')');
         end;
      end;
      Graph.GraphDraw.LegendList.Clear;
      for i := 0 to pred(RealLegend.Count) do Graph.GraphDraw.LegendList.Add(RealLegend.Strings[i]);
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
