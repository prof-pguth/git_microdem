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

   procedure DEMIX_evaluations_graph(DBonTable : integer);


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


function OpenGraphForCriterionScoresOrEvaluations(DBonTable : integer; theCrit,theSort : shortstring; VertAxisNumerical,GraphEvaluation : boolean) : tThisBaseGraph;
var
   i : integer;
   aMinVal,aMaxVal,Range : float64;
   bmp : tMyBitmap;
begin
   Result:= tThisBaseGraph.Create(Application);
   Result.Width := 800;
   Result.Height := 1500;
   //Result.GraphDraw.BottomMargin := 75;
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
   if GraphEvaluation then begin
      for i := 1 to NumDEMIXDEM do begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].MyData.FindFieldRange(DEMIXShort[i],aMinVal,aMaxVal);
         if (aminVal < Result.GraphDraw.MinHorizAxis) then Result.GraphDraw.MinHorizAxis := aMinVal;
         if (amaxval > Result.GraphDraw.MaxHorizAxis) then Result.GraphDraw.MaxHorizAxis := aMaxVal;
      end;
      Range := Result.GraphDraw.MaxHorizAxis - Result.GraphDraw.MinHorizAxis;
      Result.GraphDraw.MinHorizAxis := Result.GraphDraw.MinHorizAxis - 0.05 * Range;
      Result.GraphDraw.MaxHorizAxis := Result.GraphDraw.MaxHorizAxis + 0.05 * Range;
   end
   else begin
      if (GISdb[DBonTable].MyData.FiltRecsInDB > 200) then begin
         Result.GraphDraw.LeftMargin := 5;
         Result.GraphDraw.ShowGraphLeftLabels := false;
      end
      else begin
         Result.GraphDraw.ShowGraphLeftLabels := true;
         Result.GraphDraw.LeftMargin := 290;
      end;
   end;
   GISdb[DBonTable].EmpSource.Enabled := false;
end;


procedure DEMIX_evaluations_graph(DBonTable : integer);
//for each criterion, graph of evaluations sorted by parameter, and with tile names
var
   Criteria : tStringList;
   rfile : file;
   pn : integer;
   v : array[1..3] of float32;


      function GraphForOneCriterionWithTileNames(theCrit,TheSort : shortstring) : tThisBaseGraph;
      var
        y,i,j,DEM : integer;
        fName : PathStr;
        bmp : tMyBitmap;
        Movie : tStringlist;

            procedure AddDrawingLayer(First,Last : integer; ConstantColor : boolean);
            var
               i : integer;
            begin
               y := 5;
               Result.OpenXYColorFile(rfile);
               GISdb[DBonTable].MyData.First;
               while not GISdb[DBonTable].MyData.eof do begin
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  for I := First to Last do begin
                      v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
                      if (theSort = '') then v[2] := y + ExtraToSpreadDEMs(DEMIXShort[i], 1.5)
                      else v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(theSort);
                      if ConstantColor then v[3] := clSilver else v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[i]));
                      if (v[1] > -999) then BlockWrite(rfile,v,1);
                  end;
                  y := y + 10;
                  GISdb[DBonTable].MyData.Next;
               end;
               CloseFile(rfile);
            end;

            procedure StartGraph;
            begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               Result := OpenGraphForCriterionScoresOrEvaluations(dbOnTable,theCrit,TheSort,TheSort <> '',true);
               Result.GraphDraw.VertLabel := '';
               Result.GraphDraw.ShowHorizAxis1 := false;
               Result.GraphDraw.GraphLeftLabels := tStringList.Create;
               if (theSort = '') then Result.GraphDraw.GraphAxes := XPartGridOnly;
            end;

      var
         Bitmap : tMyBitmap;
      begin {GraphForOneCriterionWithTileNames}
         {$If Defined(RecordDEMIX_evaluations_graph)} WriteLineToDebugFile('GraphForOneCriterionWithTileNames, Criterion=' + TheCrit + '  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
         if PanelsByTestDEM then begin
            if MovieByTestDEM then Movie := tStringList.Create;
            for j := 1 to NumDEMIXDEM do begin
               StartGraph;
               AddDrawingLayer(1,NumDEMIXDEM,true);
               AddDrawingLayer(j,j,false);
               Result.RedrawDiagram11Click(Nil);
               if MovieByTestDEM then begin
                  CopyImageToBitmap(Result.Image1,Bitmap);
                  fName := NextFileNumber(MDtempDir,'demix_movie_frame_','.bmp');
                  Bitmap.SaveToFile(fName);
                  Movie.Add(fName);
                  Bitmap.Free;
               end;
            end;
            fName := NextFileNumber(MDtempDir,'demix_movie_','.mov');
            Movie.SaveToFile(fName);
            CreateNewMovie(fName);
         end
         else begin
            StartGraph;
            AddDrawingLayer(1,NumDEMIXDEM,true);
            for I := 1 to NumDEMIXDEM do AddDrawingLayer(i,i,false);
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
            if (Criteria.Count > 1) then BigGraph := tstringList.Create;
            for j := 0 to pred(Criteria.Count) do begin
               if (Criteria.Count > 1) then GISdb[DBonTable].ApplyGISFilter('CRITERION=' + QuotedStr(Criteria.Strings[j]));
               Graph := nil;
               Graph := GraphForOneCriterionWithTileNames(Criteria.Strings[j],theSort);
               //if (TheSort = '') then Graph := GraphForOneCriterionWithTileNames(Criteria.Strings[j,theSort)
               //else Graph := SortedGraphForOneCriterion(Criteria.Strings[j],TheSort);
               if (Graph <> Nil) and (Criteria.Count > 1) and DEMIX_combined_graph then begin
                  fName := NextFileNumber(MDTempDir,'graph_','.png');
                  SaveImageAsBMP(Graph.Image1,fName);
                  BigGraph.Add(fName);
               end;
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
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph in,  n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
   try
      GetDEMIXpaths;
      //LoadDEMIXnames;
      GISdb[DBonTable].EmpSource.Enabled := false;
      Criteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
      Criteria.Sort;
      if (Criteria.Count = 1) or MultiSelectSingleColumnStringList('Criteria to graph',pn,Criteria,true,true) then begin
         if (YAxisSort = yasName) then MakeAllGraphs('','Tile_Names');
         if (YAxisSort = yasSlope) then MakeAllGraphs('AVG_SLOPE','Sort_by_slope');
         if (YAxisSort = yasRuff) then MakeAllGraphs('AVG_ROUGH','Sort_by_roughness');
         if (YAxisSort = yasRelief) then MakeAllGraphs('RELIEF','Sort_by_relief');
      end;
   finally
      GISdb[DBonTable].ShowStatus;
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_evaluations_graph out'); {$EndIf}
end {DEMIX_evaluations_graph};



    {$I demix_graphs.inc}


end.
