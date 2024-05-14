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

   procedure DEMIX_evaluations_graph(DBonTable : integer; Criteria : tStringList; Evaluations : boolean = true);


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
procedure WinningPercentagesComparedToCOP(db : integer; Criteria : tStringList);
procedure WinningPercentagesComparedToCOPFilteredBySlope(db : integer; Criteria : tStringList);

procedure WhiskerPlotsByCluster(DB : integer);
procedure DEMWinningPercentageGraph(DBonTable : integer; aField : shortstring);

procedure BestBySlopeRough(dbOnTable : integer; Criteria : tStringList; Winners : boolean; Param1,Param2,ValueParam : shortstring);


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   DEMIX_control,DEMIX_Definitions;

const
   nFilter = 5;
   theFilters : array[1..5] of shortstring = ('BARREN_PC>50','AVG_SLOPE>30','AVG_SLOPE>5','','AVG_SLOPE<5');


procedure WhiskerPlotsByCluster(DB : integer);
var
   Criteria : tStringList;
   pn,i : integer;
begin
   try
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph in'); {$EndIf}
      GetDEMIXpaths(False);
      GISdb[DB].EmpSource.Enabled := false;
      Criteria := GISdb[DB].MyData.ListUniqueEntriesInDB('CRITERION');
      if (Criteria.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
         for i := 0 to Pred(Criteria.Count) do begin
            GISdb[DB].EmpSource.Enabled := false;
            GISdb[db].ApplyGISFilter('CRITERION=' + QuotedStr(Criteria.Strings[i]));
            GISdb[db].PutInQuartilesBasedOnExistingSort(10);
            TileCharateristicsWhiskerPlotsByCluster(DB,false,Criteria.Strings[i]);
         end;
      end;
   finally
      GISdb[db].ClearGISFilter;
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph out'); {$EndIf}

end;

procedure AddTopLabelToGraph(var Graph : tThisBaseGraph; DBonTable : integer);
begin
   Graph.GraphDraw.TopLabel := GISdb[DBontable].dbName  +  ' (' + DateToStr(Now) + ')';
   if (GISdb[DBonTable].MyData.Filter <> '') then Graph.GraphDraw.TopLabel := Graph.GraphDraw.TopLabel  + '  ' +  GISdb[DBontable].MyData.Filter;
   Graph.GraphDraw.TopMargin := 45;
end;

procedure AddDemixLegend(var Graph :  tThisBaseGraph; DEMsUsed : tStringList = nil);
var
   bmp : tMyBitmap;
begin
   Graph.GraphDraw.BottomLegendFName := MDtempDir + 'DEM_DEM_Legend.bmp';
   Graph.GraphDraw.BottomMargin := 125;
   bmp := DEMIXTestDEMLegend(true,DEMsUsed);
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


function OpenGraphForCriterionScoresOrEvaluations(DBonTable : integer; theCrit,theSort : shortstring; VertAxisNumerical,GraphEvaluation,BestCriteria : boolean) : tThisBaseGraph;
var
   i : integer;
   aMinVal,aMaxVal,Range : float64;
begin
   {$If Defined(RecordDEMIX_OpenGraph)} WriteLineToDebugFile('OpenGraphForCriterionScoresOrEvaluations, Criterion=' + TheCrit + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
   Result := tThisBaseGraph.Create(Application);
   Result.Width := MDDef.DEMIX_xsize;
   Result.Height := MDDef.DEMIX_ysize;
   Result.GraphDraw.ShowHorizAxis1 := true;
   Result.GraphDraw.HorizLabel := DEMIX_mode_abbreviation(MDDef.DEMIX_mode) + '_' + theCrit;
   Result.Caption := GISdb[DBonTable].DBName + ' ' + theCrit;
   if VertAxisNumerical then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.FindFieldRange(theSort,aMinVal,aMaxVal);
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
      AddDEMIXLegend(Result);
      Result.GraphDraw.MaxHorizAxis := -99999;
      Result.GraphDraw.MinHorizAxis := 99999;
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
   end;
   GISdb[DBonTable].EmpSource.Enabled := false;
   {$If Defined(RecordDEMIX_OpenGraph)} WriteLineToDebugFile('OpenGraphForCriterionScoresOrEvaluations out ' + Result.GraphDraw.AxisRange); {$EndIf}
end;



procedure DEMWinningPercentageGraph(DBonTable : integer; aField : shortstring);
var
   Graph : tThisBaseGraph;
   i,j,nt : integer;
   rfile : file;
   v : array[1..3] of float32;
begin
   if GISdb[DBonTable].MyData.FieldExists('AREA') then aField := 'AREA';
   if GISdb[DBonTable].MyData.FieldExists('CRITERION') then aField := 'CRITERION';
   for j := 1 to NumDEMIXtestDEM do UseRetiredDEMs[j] := true;

   Graph := OpenGraphForCriterionScoresOrEvaluations(DBonTable,'','',false,true,false);
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
      for j := 1 to NumDEMIXtestDEM do begin
          v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXshort[j] + '_WIN');
          v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[j]));
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
end;


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


procedure WinningPercentagesComparedToCOPFilteredBySlope(db : integer; Criteria : tStringList);
var
   i : integer;
begin
   if GISdb[db].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE') then begin
      for i := 1 to nFilter do begin
          {$IfDef RecordDEMIX} WriteLineToDebugFile('WinningPercentagesComparedToCOPFilteredBySlope ' + IntToStr(i) + '  ' + theFilters[i]); {$EndIf};
          GISdb[db].ApplyGISFilter(theFilters[i]);
          WinningPercentagesComparedToCOP(db,Criteria);
      end;
   end
   else begin
      MessageToContinue('Add tile characteristics to DB');
   end;
end;



procedure WinningPercentagesComparedToCOP(db : integer; Criteria : tStringList);
const
   MaxCrit = 25;
   DEM1 = 'COP';

   CompDEM = 9;
   //TheDEMs : array[1..CompDEM] of shortstring = ('ALOS','FABDEM','TANDEM','COAST','DILUV','DELTA','SRTM','NASA','ASTER');
var
   Criterion,TStr,Outcome : shortstring;
   i,j,k,CritID,Total,NumDEMs : integer;
   bfa : array[0..MaxCrit,1..CompDEM,1..3] of int16;
   GraphDB : tStringList;
   lowx,highx,y : float32;
   Color : tColor;
   fName : PathStr;
   Graph : tThisBaseGraph;
   bs,NumTiles,oc : integer;
   aField : shortstring;
   ComparingDEMs : tStringList;

         procedure ProcessBFA(i,j : integer);
         //allows the control loops to be either order (DEM, the Criterion, or Criterion then DEM)
         var
            k : integer;
         begin
            Total := 0;
            for k := 1 to 3 do Total := Total + bfa[i,j,k];
            if (Total > 0) then begin
               lowx := 0;
               for k := 1 to 3 do begin
                  Highx := LowX + 100 * bfa[i,j,k] / Total;
                  if MDDef.DEMIX_groupWonLost = 1 then begin
                     if (j = NumDEMs div 2) and (k = 2) then TStr := Criteria[i] else TStr := '';
                  end
                  else TStr := Criteria[i];
                  if (k=1) then begin //COP wins this one
                     Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName('COP'));
                     if bfa[i,j,1] > bfa[i,j,3] then bs := 0 {bsSolid}
                     else bs := 1 {bsDiagCross};
                     OutCome := 'COP wins over ' + ComparingDEMs.Strings[pred(j)] + RealToString(Highx,8,2) + '%';
                  end
                  else if (k = 3) then begin  //COP loses
                     Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(ComparingDEMs.Strings[pred(j)]));
                     if bfa[i,j,1] < bfa[i,j,3] then bs := 0 {bsSolid}
                     else bs := 1 {bsDiagCross};
                     OutCome := 'COP loses to ' +  ComparingDEMs.Strings[pred(j)] + RealToString(HighX-LowX,8,2) + '%';
                  end
                  else begin
                     Color := clWhite;
                     OutCome := 'COP ties ' + ComparingDEMs.Strings[pred(j)] + RealToString(HighX-LowX,8,3) + '%';
                  end;
                  GraphDB.Add(RealToString(lowx,-8,-2) + ',' + RealToString(Highx,-8,-2) + ',' + RealToString(y,-8,-2) + ',' + IntToStr(Color) + ',' + TStr + ',' + IntToStr(bs) + ',' + Outcome);
                  LowX := Highx;
               end;
               y := y + 1;
            end;
         end;


begin
   if GISdb[DB].MyData.FieldExists('COP_ALOS') then begin
      GISdb[DB].EmpSource.Enabled := false;
      NumTiles := GISdb[DB].MyData.NumUniqueEntriesInDB('DEMIX_TILE');
      for i := 0 to MaxCrit do
         for j := 1 to CompDEM do
            for k := 1 to 3 do bfa[i,j,k] := 0;

      NumDEMs := 0;
      ComparingDEMs := tStringList.Create;
      if MDDef.DEMIX_graph_Retired_DEMs then begin
        for i := 1 to MaxDEMIXDEM do begin
           if AllDEMIXTheDEMs[i] <> DEM1 then begin
               aField := 'COP' + '_' + AllDEMIXTheDEMs[i];
               if GISdb[DB].MyData.FieldExists(aField) then begin
                  inc(NumDEMs);
                  ComparingDEMs.Add(AllDEMIXTheDEMs[i]);
               end;
           end;
        end;
      end
      else begin
        for i := 1 to MaxUnretiredDEM do begin
           if AllunretiredDEMs[i] <> DEM1 then begin
               aField := 'COP' + '_' + AllunretiredDEMs[i];
               if GISdb[DB].MyData.FieldExists(aField) then begin
                  inc(NumDEMs);
                  ComparingDEMs.Add(AllunretiredDEMs[i]);
               end;
           end;
        end;
      end;

      {$IfDef RecordDEMIX} WriteLineToDebugFile('Start winning records'); {$EndIf};
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
            for I := 1 to NumDEMs do begin
               aField := 'COP' + '_' + ComparingDEMs.Strings[pred(i)];
               if GISdb[DB].MyData.FieldExists(aField) then begin
                  Outcome := GISdb[DB].MyData.GetFieldByNameAsString(aField);
                  if Outcome = 'COP' then oc := 1
                  else if Outcome = 'TIE' then oc := 2
                  else oc := 3;
                  inc(bfa[CritID,i,oc]);
               end;
            end;
         end;
         GISdb[DB].MyData.Next;
      end;
      EndProgress;

      {$IfDef RecordDEMIX} WriteLineToDebugFile('Start GraphDB string list'); {$EndIf};

      GraphDB := tStringList.Create;
      GraphDB.Add('X1,X2,Y,COLOR,LABEL,BS_STYLE,OUTCOME');

      y := 1;
      if MDDef.DEMIX_groupWonLost = 1 then begin
         for i := 0 to pred(Criteria.Count) do begin
            for j := 1 to NumDEMs do begin
               ProcessBFA(i,j);
            end;
            y := y + 1;
         end;
      end
      else begin
         for j := 1 to NumDEMs do begin
            for i := 0 to pred(Criteria.Count) do begin
               ProcessBFA(i,j);
            end;
            y := y + 1;
         end;
      end;

      fName := NextFileNumber(MDTempDir,DEMIX_mode_abbreviation(MDDef.DEMIX_mode) + '_winning_graph_','.dbf');
      StringList2CSVtoDB(GraphDB,fName,true);

      {$IfDef RecordDEMIX} WriteLineToDebugFile('Start Graph'); {$EndIf};
      Graph := tThisBaseGraph.Create(Application);
      Graph.Width := MDDef.DEMIX_xsize;
      Graph.Height := MDDef.DEMIX_ysize;
      AddTopLabelToGraph(Graph,db);
      GISdb[DB].EmpSource.Enabled := false;
      Graph.GraphDraw.HorizLabel := 'CopDEM won/tie/loss record (tiles=' + IntToStr(GISdb[DB].MyData.NumUniqueEntriesInDB('DEMIX_TILE')) + ')';
      Graph.GraphDraw.MaxHorizAxis := 100;
      Graph.GraphDraw.MaxVertAxis := y - 1;
      Graph.GraphDraw.GraphAxes := XPartGridOnly;
      Graph.GraphDraw.LeftMargin := 160;
      Graph.BarGraphDBName := fName;
      AddDemixLegend(Graph,ComparingDEMs);
      Graph.RedrawDiagram11Click(Nil);
   end
   else MessageToContinue('COP winning results required');
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
      Rank,cf,LocStr : shortString;
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
      cf := DEM1 + '_' + DEM2;
      if (MDDef.DEMIXUseBins > MaxPossBins) then MDDef.DEMIXUseBins := MaxPossBins;
      DiffDist := StrUtils.AnsiContainsText(ValueParam,'ELVD') or StrUtils.AnsiContainsText(ValueParam,'SLPD') or StrUtils.AnsiContainsText(ValueParam,'RUFD');
      if DiffDist then begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         MaxEval := GISdb[DBonTable].MyData.FindFieldMax(ValueParam);
      end;

      if GISdb[DBonTable].MyData.FieldExists(cf) then begin
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
                  Rank := GISdb[DBonTable].MyData.GetFieldByNameAsString(cf);
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
            StringList2CSVtoDB(GraphData,fName,true);
            Result.BarGraphDBName := fName;
         end
         else begin

         end;
         Result.AutoScaleAndRedrawDiagram(false,false,false,false);
      end;
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
        //if (Criteria.Count <> 1) {or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true)} then begin
            for i := 0 to pred(Criteria.Count) do begin
               CreateGraph(Criteria.Strings[i],'COP','ALOS');
               (*
               CreateGraph(Criteria.Strings[i],'COP','TANDEM');
               CreateGraph(Criteria.Strings[i],'COP','FABDEM');
               CreateGraph(Criteria.Strings[i],'COP','DILUV');
               CreateGraph(Criteria.Strings[i],'COP','DELTA');
               *)
            end;
         //end;
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


procedure OpenColorFiles(Graph : tThisBaseGraph; fName : shortstring);
var
   i : integer;
begin
   for i := 0 to NumDEMIXtestDEM do begin
      if (i = 0) then Graph.OpenDataFile(rfile[i],clSilver, fName + '_graph_data_')
      else if UseRetiredDEMs[i] then Graph.OpenDataFile(rfile[i],ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[i])),fName + '_graph_data_');
      Graph.GraphDraw.Symbol[succ(i)].DrawingSymbol := FilledBox;
      Graph.GraphDraw.Symbol[succ(i)].Size := DEMIXsymsize;
   end;
end;


procedure AddDrawingLayers(DBonTable : integer; Graph : tThisBaseGraph; Evaluations : boolean; theSort : shortstring);
//Evaluations plots the criteria evaluations, otherwise the scores (opinions)
//if theSort is blank, the tiles or areas will be evenly spaced so they do not overprint
//if theSort is a field in the DB, the tiles or areas will be spaced on a numerical axis by the field value
var
   NPts,i,j,TileID : integer;
   v : array[1..2] of float32;
   Tile : shortstring;
   Tiles : tStringList;
   val,y : float32;

   function MeetsTileFilter : boolean;
   var
      AvgSlope : float32;
   begin
      (*
      AvgSlope := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('AVG_SLOPE');
      Result := AvgSlope > 5;
      *)
      Result := true;
   end;

begin
   y := 0;
   Tiles := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEMIX_TILE');

   Graph.GraphDraw.LegendList := tStringList.Create;
   Graph.GraphDraw.LegendList.Add('Other');
   for i := 1 to NumDEMIXtestDEM do if UseRetiredDEMs[i] then Graph.GraphDraw.LegendList.Add(DEMIXShort[i]);

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
      if MeetsTileFilter then begin
         for i := 1 to NumDEMIXtestDEM do begin
            if UseRetiredDEMs[i] and GISdb[DBonTable].MyData.FieldExists(DEMIXShort[i]) then begin
               val := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
               bfa[TileID,i,1] := val;
               if (theSort = '') then bfa[TileID,i,2] := 100 * y / GISdb[DBonTable].MyData.FiltRecsInDB
               else bfa[TileID,i,2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(theSort);
            end;
         end;
      end;
      y := y + 1;
      GISdb[DBonTable].MyData.Next;
   end;

   for I := 0 to pred(Tiles.Count) do begin
      for j := 1 to NumDEMIXtestDEM do if UseRetiredDEMs[j] then begin
         if GISdb[DBonTable].MyData.FieldExists(DEMIXShort[j]) and (bfa[i,j,1] > 0) then begin
             v[1] := bfa[i,j,1];
             v[2] := bfa[i,j,2];
             BlockWrite(rfile[j],V,1);
             BlockWrite(rfile[0],V,1);
             if (j = 1) then inc(NPts);
         end;
      end;
   end;
   CloseFile(rfile[0]);
   for i := 1 to NumDEMIXtestDEM do if UseRetiredDEMs[i] then CloseFile(rfile[i]);
   Graph.RedrawDiagram11Click(Nil);
   Tiles.Destroy;
   EndProgress;
   GISdb[DBonTable].EmpSource.Enabled := true;
   Graph.GraphDraw.HorizLabel := {DEMIX_mode_abbreviation(MDDef.DEMIX_mode) + ' ' +} RemoveUnderscores(Graph.GraphDraw.HorizLabel) +  '  (n=' + IntToStr(NPts) + ')';
end;


procedure DEMIX_evaluations_graph(DBonTable : integer; Criteria : tStringList; Evaluations : boolean = true);
//for each criterion, graph of evaluations sorted by parameter, and with tile names
var
   BaseFilter,RestoreFilter : shortstring;
   pn : integer;


      function GraphForOneCriterion(Numerical,AllCriteria : boolean; theCrit,TheSort,GraphName : shortstring) : tThisBaseGraph;
      var
        y,i,j,DEM : integer;
      var
         Bitmap : tMyBitmap;
      begin {GraphForOneCriterionWithTileNames}
         {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('GraphForOneCriterionWithTileNames, Criterion=' + TheCrit + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
         GISdb[DBonTable].EmpSource.Enabled := false;
         Result := OpenGraphForCriterionScoresOrEvaluations(dbOnTable,theCrit,TheSort,Numerical,Evaluations,AllCriteria);
         if (DEMIXVertAxisLabel = '') then Result.GraphDraw.VertLabel := GraphName
         else Result.GraphDraw.VertLabel := RemoveUnderscores(DEMIXVertAxisLabel) + '  ' + GISdb[dbOnTable].MyData.Filter;
         Result.GraphDraw.ShowHorizAxis1 := false;
         Result.GraphDraw.GraphLeftLabels := tStringList.Create;
         Result.GraphDraw.DrawInsideLines := false;
         if not AllCriteria then begin
            AddDrawingLayers(DBonTable,Result,Evaluations,TheSort);
            Result.RedrawDiagram11Click(Nil);
         end;
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
            if (Criteria.Count > 1) and DEMIX_combined_graph then BigGraph := tstringList.Create;
            for j := 0 to pred(Criteria.Count) do begin
               if (Criteria.Count > 0) then GISdb[DBonTable].ApplyGISFilter(BaseFilter + 'CRITERION=' + QuotedStr(Criteria.Strings[j]));
               AddFilteredRankID(DBonTable);
               Graph := nil;
               Graph := GraphForOneCriterion(Numerical,false,Criteria.Strings[j],theSort,GraphName);
               fName := DEMIX_mode_abbreviation(MDDef.DEMIX_mode) + '_' + Criteria.Strings[j] + '_' + theSort;
               if MovieByTestDEM then begin
                  Graph.AnimateGraph(true,fName);
               end;
               if PanelsByTestDEM then begin
                  Graph.AnimateGraph(false,fName);
               end;
               if (Criteria.Count > 1) and DEMIX_combined_graph then begin
                  BigGraph.Add(fName);
               end;
            end;
            if (Criteria.Count > 1) then begin
               if DEMIX_combined_graph then begin
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

      procedure JustOneGraph;
      var
         j, y : integer;
         Graph : tThisBaseGraph;
         v : array[1..2] of float32;
         rfile : file;
         Symbol : tFullSymbolDeclaration;
         Criterion : shortstring;
      begin
         Graph := nil;
         Graph := GraphForOneCriterion(false,true,' Critera FUV','','Best evaluation sort (%) ' + RestoreFilter + ' Tiles=' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE')));
         AddTopLabelToGraph(Graph,DBonTable);

         Graph.GraphDraw.LegendList := tStringList.Create;
         for j := 0 to pred(Criteria.Count) do begin
            Criterion := Criteria.Strings[j];
            GISdb[DBonTable].ApplyGISFilter(BaseFilter + 'CRITERION=' + QuotedStr(Criterion));
            Graph.GraphDraw.LegendList.Add(Criteria.Strings[j]);
            //Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors[j mod 15]);
            Symbol.Color := ConvertTColorToPlatformColor(DEMIXColorForCriterion(Criterion));
            Symbol.DrawingSymbol := FilledBox;
            Symbol.Size := DemixSymSize;
            Graph.OpenPointFile(rfile,Symbol);
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
         Graph.AutoScaleAndRedrawDiagram;
      end;


      procedure FilterJustOneGraph;


         procedure AGraph(Crit : integer);
         var
            k,y : integer;
            Graph : tThisBaseGraph;
            v : array[1..2] of float32;
            rfile : file;
            aFilter,TStr,Criterion : shortstring;
            Symbol : tFullSymbolDeclaration;
         begin
            Graph := nil;
            Graph := GraphForOneCriterion(false,true,' Critera FUV','','Best evaluation sort (%) ' + Criteria.Strings[Crit]);
            AddTopLabelToGraph(Graph,DBonTable);
            Graph.GraphDraw.LegendList := tStringList.Create;
            for k := 1 to nFilter do begin
               Criterion := Criteria.Strings[Crit];
               aFilter := BaseFilter + 'CRITERION=' + QuotedStr(Criterion);
               if (theFilters[k] <> '') then aFilter := aFilter + ' AND ' + theFilters[k];

               GISdb[DBonTable].ApplyGISFilter(aFilter);
               if (theFilters[k] <> '') then TStr := theFilters[k]
               else TStr := 'All slopes';
               Graph.GraphDraw.LegendList.Add(TStr + '  (n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ')');
               Graph.GraphDraw.HorizLabel := Criterion;
               GISdb[DBonTable].EmpSource.Enabled := false;
               Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors[k mod 15]);
               //Symbol.Color := ConvertTColorToPlatformColor(DEMIXColorForCriterion(Criterion));
               Symbol.DrawingSymbol := FilledBox;
               Symbol.Size := DemixSymSize;
               Graph.OpenPointFile(rfile,Symbol);
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
            Graph.AutoScaleAndRedrawDiagram;
         end;


      var
         j : integer;
      begin
         if GISdb[DBonTable].MyData.FieldExists('BARREN_PC') and GISdb[DBonTable].MyData.FieldExists('AVG_SLOPE') then begin
            for j := 0 to pred(Criteria.Count) do begin
               AGraph(j);
            end;
         end
         else begin
            MessageToContinue('Add tile characteristics to DB');
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
         for j := 0 to pred(Criteria.Count) do begin
            Graph := nil;
            Graph := GraphForOneCriterion(false,true,Criteria.Strings[j],'','Best evaluation sort colored by slope (%)');
            Graph.GraphDraw.LegendList := tStringList.Create;
            GISdb[DBonTable].ApplyGISFilter(BaseFilter + 'CRITERION=' + QuotedStr(Criteria.Strings[j]));
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
      RestoreFilter := GISdb[DBonTable].MyData.Filter;
      if RestoreFilter = '' then BaseFilter := ''
      else BaseFilter := RestoreFilter + ' AND ';

      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph in,  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
      GetDEMIXpaths(False);
      GISdb[DBonTable].EmpSource.Enabled := false;
      //Criteria := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('CRITERION');
      //if (Criteria.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
         if (YAxisWhat = yasBestEvalByCriterion) then JustOneGraph
         else if (YAxisWhat = yasBestEvalFilteredBySlope) then FilterJustOneGraph
         else if (YAxisWhat = yasBestEvalColoredBySlope) then GraphsColoredByTile
         else if (YAxisWhat = yasSlope) then MakeAllGraphs(true,'AVG_SLOPE','Tile average slope (%)')
         else if (YAxisWhat = yasRuff) then MakeAllGraphs(true,'AVG_ROUGH','Tile average roughness (%)')
         else if (YAxisWhat = yasRelief) then MakeAllGraphs(true,'RELIEF','Tile average relief (m)')
         else if (YAxisWhat = yasBarren) then MakeAllGraphs(true,'BARREN_PC','Tile barren (%)')
         else if (YAxisWhat = yasForest) then MakeAllGraphs(true,'FOREST_PC','Tile forest (%)')
         else if (YAxisSort in [yasName,yasBestEval]) then MakeAllGraphs(false,'','Tile best evaluation');
      //end;
   finally
      EndDEMIXProcessing(DBonTable);
      GISdb[DBonTable].ApplyGISFilter(RestoreFilter);
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
   Criteria := GISdb[DB].MyData.ListUniqueEntriesInDB('CRITERION');
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
