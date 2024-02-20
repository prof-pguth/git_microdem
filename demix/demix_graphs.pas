unit demix_graphs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$Define ExDEMIXexperimentalOptions}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
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



{$IfDef ExDEMIXexperimentalOptions}
{$Else}


   procedure DEMIX_evaluations_graph(DBonTable : integer);
   procedure DEMIX_AreaAverageScores_graph(DBonTable : integer);



   function MakeHistogramOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;
   function GraphAverageScoresByTile(DB : integer; TileList,CriteriaList : tStringList): tThisBaseGraph;
   procedure MultipleBestByParametersSortByValue(DBonTable,Option : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing,TileParameters : tStringList; ByPointFilters : boolean = false);
   procedure BestDEMSbyCategory(DBonTable : integer);
   function DEMIX_SSIM_R2_single_tile_graph(DBonTable : integer; tile : shortstring) :tThisBaseGraph;
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

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{ include file for demix_definitions }
{____________________________________}


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

function OpenGraphForCriterionScoresOrEvaluations(DBonTable : integer; theCrit,theSort : shortstring; VertAxisNumerical,GraphEvaluation : boolean) : tThisBaseGraph;
var
   i : integer;
   aMinVal,aMaxVal,Range : float64;
   bmp : tMyBitmap;
begin
   Result:= tThisBaseGraph.Create(Application);
   Result.Width := 800;
   Result.Height := 1500;
   Result.GraphDraw.BottomMargin := 75;
   Result.GraphDraw.ShowHorizAxis1 := true;
   Result.GraphDraw.HorizLabel := theCrit;
   Result.Caption := GISdb[DBonTable].DBName + ' ' + theCrit;
   if VertAxisNumerical then begin
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
   end;
   GISdb[DBonTable].EmpSource.Enabled := false;
end;


procedure ComputeAverageScoresForSelectedCriteria(db : integer; CriteriaList : tStringList; var Scores : tDEMIXfloats; var NumTies : integer; var WinnerString : shortstring);
var
   i,Opinions : integer;
   Criterion : shortstring;
   LowScore : float32;
begin
   GISdb[DB].EmpSource.Enabled := false;
   for i := 1 to NumDEMIXDEM do Scores[i] := 0;
   Opinions := 0;
   while not GISdb[DB].MyData.eof do begin
      Criterion := GISdb[DB].MyData.GetFieldByNameAsString('CRITERION');
      if (CriteriaList.IndexOf(Criterion) <> -1) then begin
         inc(Opinions);
         for i := 1 to NumDEMIXDEM do begin
            Scores[i] := Scores[i] + GISdb[DB].MyData.GetFieldByNameAsFloat(DEMIXShort[i] + '_SCR');
         end;
      end;
      GISdb[DB].MyData.Next;
   end;
   for I := 1 to NumDEMIXDEM do Scores[i] := Scores[i] / Opinions;
   LowScore := 999;
   WinnerString := '';
   for I := 1 to NumDEMIXDEM do begin
      if Scores[i] < LowScore - 0.001 then begin
         LowScore := Scores[i];
         WinnerString := DEMIXShort[i];
         NumTies := 1;
      end
      else if Scores[i] < LowScore + 0.001 then begin
         WinnerString := WinnerString + ';' + DEMIXShort[i];
         inc(NumTies);
      end;
   end;
end;


{$IfDef ExDEMIXexperimentalOptions}
{$Else}
    {$I demix_graphs.inc}
{$EndIf}




end.
