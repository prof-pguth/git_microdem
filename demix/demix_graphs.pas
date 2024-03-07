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


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   DEMIX_control,DEMIX_Definitions;


function AdjustFilterForDTMandAll(DB : integer; Base : shortstring) : shortstring;
begin
   Result := Base;
   if GISdb[DB].MyData.FieldExists('REF_TYPE') then Result := Result +  ' AND REF_TYPE=' + QuotedStr('DTM');
   if GISdb[DB].MyData.FieldExists('LAND_TYPE') then Result := Result +  ' AND LAND_TYPE=' + QuotedStr('ALL');
end;


procedure SetHorizAxisForDEMIXscores(var Graph : tThisBaseGraph);
begin
    Graph.GraphDraw.MinHorizAxis := 0.5;
    Graph.GraphDraw.MaxHorizAxis := NumDEMIXDEM + 0.5;
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
         for i := 1 to NumDEMIXDEM do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].MyData.FindFieldRange(DEMIXShort[i],aMinVal,aMaxVal);
            if (aminVal < Graph.GraphDraw.MinHorizAxis) then Graph.GraphDraw.MinHorizAxis := aMinVal;
            if (amaxval > Graph.GraphDraw.MaxHorizAxis) then Graph.GraphDraw.MaxHorizAxis := aMaxVal;
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
   Result.GraphDraw.HorizLabel := theCrit;
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
   end
   else begin
      Result.GraphDraw.MinHorizAxis := 0.5;
      Result.GraphDraw.MaxHorizAxis := NumDEMIXDEM + 0.5;
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

procedure OpenColorFiles(Graph : tThisBaseGraph; fName : shortstring);
var
   i : integer;
begin
   for i := 0 to NumDEMIXDEM do begin
      if i=0 then Graph.OpenDataFile(rfile[i],clSilver, fName + '_graph_data_')
      else Graph.OpenDataFile(rfile[i],ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[i])),fName + '_graph_data_');
      Graph.GraphDraw.Symbol[succ(i)].DrawingSymbol := FilledBox;
      Graph.GraphDraw.Symbol[succ(i)].Size := 3;
   end;
end;


   procedure AddDrawingLayers(DBonTable : integer; Graph : tThisBaseGraph; ConstantColor,Evaluations : boolean; theSort : shortstring);
   //First and Last determine DEMs to add, for instance to put only one in color on a base with all in gray
   //Contant color puts everything in the same color, instead of colored by DEM
   //Evaluations plots the criteria evaluations, otherwise the scores (opinions)
   //if theSort is blank, the tiles or areas will be evenly spaced so they do not overprint
   //if theSort is a field in the DB, the tiles or areas will be spaced on a numerical axis by the field value
   var
      NPts,i,j,TileID : integer;
      v : array[1..2] of float32;
      //rfile : file;
      Extra,Tile : shortstring;
      Tiles : tStringList;
      val,y : float32;
   begin
      y := 5;
      if Evaluations then Extra := '' else Extra := '_SCR';
      Tiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');

      //Graph.OpenXYColorFile(rfile);
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
         for i := 1 to NumDEMIXDEM do begin
            val := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
            bfa[TileID,i,1] := val;
            bfa[TileID,i,2] := y;
         end;
         y := y + 10;
         GISdb[DBonTable].MyData.Next;
      end;

   for I := 0 to pred(Tiles.Count) do begin
      for j := 1 to NumDEMIXDEM do begin
         if (bfa[i,j,1] > 0) then begin
             v[1] := bfa[i,j,1];
             v[2] := bfa[i,j,2];
             BlockWrite(rfile[j],V,1);
             BlockWrite(rfile[0],V,1);
             inc(NPts);
         end;
      end;
   end;
   for i := 0 to NumDEMIXDEM do CloseFile(rfile[i]);
   Graph.RedrawDiagram11Click(Nil);
   Tiles.Destroy;
   EndProgress;
   GISdb[DBonTable].EmpSource.Enabled := true;
   Graph.GraphDraw.LLcornerText := 'n=' + IntToStr(NPts);


(*

      while not GISdb[DBonTable].MyData.eof do begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         for I := First to Last do begin
             v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i] + Extra);
             if v[1] > Graph.GraphDraw.MaxHorizAxis then v[1] := Graph.GraphDraw.MaxHorizAxis;
             if (theSort = '') then v[2] := 5 + pred(GISdb[DBonTable].MyData.GetFieldByNameAsInteger('FILT_ID')) * 10 //y + ExtraToSpreadDEMs(DEMIXShort[i], 1.5)
             else v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(theSort);
             if ConstantColor then v[3] := clSilver else v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[i]));
             if (v[1] > -999) then BlockWrite(rfile,v,1);
         end;
         y := y + 10;
         GISdb[DBonTable].MyData.Next;
      end;
*)
      //CloseFile(rfile);
   end;


procedure DEMIX_evaluations_graph(DBonTable : integer; Evaluations : boolean = true);
//for each criterion, graph of evaluations sorted by parameter, and with tile names
var
   Criteria : tStringList;
   pn : integer;


      function GraphForOneCriterionWithTileNames(theCrit,TheSort,GraphName : shortstring) : tThisBaseGraph;
      var
        y,i,j,DEM : integer;

            procedure StartGraph;
            begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               Result := OpenGraphForCriterionScoresOrEvaluations(dbOnTable,theCrit,TheSort,(TheSort <> ''),Evaluations);
               if (DEMIXVertAxisLabel = '') then Result.GraphDraw.VertLabel := GraphName
               else Result.GraphDraw.VertLabel :=  RemoveUnderscores(DEMIXVertAxisLabel) + '  (n=' + IntToStr(GISdb[dbOnTable].MyData.FiltRecsInDB) + ')';
               Result.GraphDraw.ShowHorizAxis1 := false;
               Result.GraphDraw.GraphLeftLabels := tStringList.Create;
               if (theSort = '') then Result.GraphDraw.GraphAxes := XPartGridOnly;
            end;

      var
         Bitmap : tMyBitmap;
      begin {GraphForOneCriterionWithTileNames}
         {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('GraphForOneCriterionWithTileNames, Criterion=' + TheCrit + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
         if PanelsByTestDEM then begin
            StartGraph;
            AddDrawingLayers(DBonTable,Result,true,Evaluations,TheSort);
            Result.RedrawDiagram11Click(Nil);
         end
         else begin
            StartGraph;
            AddDrawingLayers(DBonTable,Result,true,true,TheSort);

            (*
            GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr('N39PW113H'));
            GISdb[DBonTable].ApplyGISFilter('AREA=' + QuotedStr('TRENTINO'));
            GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr('N28VW018B') + ' OR DEMIX_TILE=' + QuotedStr('N39RW113J')  +
                 ' OR DEMIX_TILE=' + QuotedStr('N39UW098F') + ' OR DEMIX_TILE=' + QuotedStr('N46XE012B'));
            *)
            Result.RedrawDiagram11Click(Nil);
         end;
         {GraphForOneCriterionWithTileNames}
      end;


      procedure MakeAllGraphs(TheSort : shortstring; GraphName : shortstring);
      var
         j : integer;
         Graph : tThisBaseGraph;
         BigGraph : tstringList;
         fName : PathStr;
      begin
         {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('MakeAllGraphs, Criteria=' + IntToStr(Criteria.Count) + ' n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
         if (TheSort = '') or GISdb[DBonTable].MyData.FieldExists(TheSort) then begin
            //if (Criteria.Count > 1) then BigGraph := tstringList.Create;
            for j := 0 to pred(Criteria.Count) do begin
               if (Criteria.Count > 0) then GISdb[DBonTable].ApplyGISFilter('CRITERION=' + QuotedStr(Criteria.Strings[j]));
               AddFilteredRankID(DBonTable);
               Graph := nil;
               Graph := GraphForOneCriterionWithTileNames(Criteria.Strings[j],theSort,GraphName);
            end;
            if (Criteria.Count > 1) then begin
               if DEMIX_combined_graph then begin
                  fName := NextFileNumber(MDTempDir,GraphName + '_','.png');
                  MakeBigBitmap(BigGraph,'',fName,Criteria.Count);
               end;
               GISdb[DBonTable].ClearGISFilter;
            end;
         end
         else begin
            MessageToContinue('Field missing for sort ' + TheSort);
         end;
      end;

begin {DEMIX_evaluations_graph}
   try
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph in,  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
      GetDEMIXpaths;
      GISdb[DBonTable].EmpSource.Enabled := false;
      Criteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
      Criteria.Sort;
      if (Criteria.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
         if (YAxisSort in [yasName,yasBestEval]) then MakeAllGraphs('','Tile_Names');
         if (YAxisSort = yasSlope) then MakeAllGraphs('AVG_SLOPE','Tiles_sort_by_slope');
         if (YAxisSort = yasRuff) then MakeAllGraphs('AVG_ROUGH','Tiles_sort_by_roughness');
         if (YAxisSort = yasRelief) then MakeAllGraphs('RELIEF','Tiles_sort_by_relief');
      end;
   finally
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
   //Vals : array[1..MaxDEMs] of array[1..3] of float32;
begin
   //Criterion1 := 'RUFF_SSIM';
   //Criterion2 := 'RUFF_FUV';
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
         if Crit = Criterion1 then CritID := 1 else CritID := 2;

         for i := 1 to NumDEMIXDEM do begin
            val := GISdb[DB].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
            bfa[TileID,i,CritID] := val;
         end;
      end;
      GISdb[DB].MyData.Next;
   end;

   OpenColorFiles(Graph,'All_dems_' + Criterion1 + '_' + Criterion2);

   NPts := 0;
   for I := 0 to MaxTiles do
      for j := 1 to NumDEMIXDEM do begin
         if (bfa[i,j,1] > 0) and (bfa[i,j,2] > 0) then begin
             v[1] := bfa[i,j,1];
             v[2] := bfa[i,j,2];
             BlockWrite(rfile[j],V,1);
             BlockWrite(rfile[0],V,1);
             if (j = 1) then inc(NPts);
         end;
      end;
   for i := 0 to NumDEMIXDEM do CloseFile(rfile[i]);
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
   for I := 1 to NumDEMIXDEM do DEMs.Add(DEMIXshort[i]);

   for I := 0 to pred(Criteria.Count) do begin
      GISdb[DB].ApplyGISFilter('CRITERION=' + QuotedStr(Criteria.Strings[i]));
      Graph := GISdb[DB].CreateHistogramFromDataBase(true,DEMs,false);
      for j := 1 to NumDEMIXDEM do begin
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
