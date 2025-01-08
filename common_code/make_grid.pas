unit make_grid;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IFDEF DEBUG}
   //{$Define NoParallelFor} //used to debug only
   {$Define NoParallelMoments} // added 4/25/2022 to track down bug, removed 8/5/2022 but immediately returned since SSO normals might not be thread safe

   {$IfDef RecordProblems}   //normally only defined for debugging specific problems
      //$Define CreateAspectMap}
      //{$Define RecordDEMIX_VAt}
      //{$Define DEMIXmaps}
      //{$Define CreateSlopeMap}
      //{$Define TrackMapRange}
      {$Define CreateGeomorphMaps}
      //{$Define RecordTimeGridCreate}
      //{$Define RecordPointClass}
      //{$Define RecordDEMCompare}
      //{$Define NewVATgrids}
      //{$Define RecordMapSteps}
   {$EndIf}
{$ELSE}
   //{$Define NoParallelFor}
{$ENDIF}

interface

 uses
   SysUtils, Windows, Classes, Graphics, Controls,JPEG,DBCtrls,Math,dbClient,
   System.Threading,System.SyncObjs,System.Diagnostics,System.TimeSpan,
   Forms, Dialogs, ExtCtrls, StdCtrls,ComCtrls,ClipBrd, Menus, Buttons, ToolWin,StrUtils,db,
   System.Types,

   {$IfDef MSWindows}
      ShlObj, URLMon,Messages,ShellAPI,
   {$EndIf}

   {$IfDef ExStereoNet}
   {$Else}
     NetMainW,
   {$EndIf}

   Nevadia_main,
   DEMDefs,
   Petmar_types,PETMAR,PETMath;

const
   MaxGrids = 12;

const
   UpOpenDEM = 1;
   DownOpenDEM = 2;
   DiffOpenDEM = 3;

const
   PartialNames : array[1..5] of shortstring = ('dzdx','dzdy','dxx','dyy','dxy');

type
   tListOfDEMs = array[1..MaxGrids] of integer;
   tNormalMethod = (nmNone,nmEastWest,nmNorthSouth,nmInterpolate,nm30m,nmTRIK,nmRRI,nmMAD2K);
   tPartialGrids = array[1..5] of integer;

//new, faster (hopefully)
   function CreateHillshadeMap(OpenMap : boolean; DEM : integer; Outname : PathStr = '') : integer;
   function CreateSlopeMapPercent(OpenMap : boolean; DEM : integer; OutName : PathStr = ''; Radius : integer = 0) : integer;
   procedure CreateOpennessMap(OpenMap : boolean; GridLimits : tGridLimits; DEM,BoxSizeMeters : integer; var Upward,DownWard,Difference : integer);

function BoxCarDetrendDEM(OpenMap : boolean; DEM : integer; GridLimits : tGridLimits; FilterRadius : integer = 2) : integer;

function ShortDerivativeMapName(ch : AnsiChar; SampleBoxSize : integer = 0) : ShortString;
function DerivativeMapName(ch : AnsiChar; SampleBoxSize : integer = 0) : ShortString;

function MakeSingleNewDerivativeMap(ch : AnsiChar; CurDEM : integer = 0; SampleBoxSize : integer = 0; ShowMap : boolean = true) : integer;

function CreateRidgeMap(WhichDEM : integer; GridLimits : tGridLimits;  RidgeTypeMap : tRidgeTypemap; Memo1 : tMemo = Nil) : integer;
function CreateIwashishiPikeMap(NewName : ShortString; BaseGrid,SlopeGrid,RoughGrid,ConvexGrid : integer) : integer;
function AspectDifferenceMap(WhichDEM,RegionRadius : integer; GridLimits : tGridLimits) : integer;

function MakeMomentsGrid(CurDEM : integer; What : char; BoxSizeRadiusMeters : integer = -99; OpenMaps : boolean = true) : integer;

function CreateProfileConvexityMap(WhichDEM : integer; OpenMap : boolean = true) : integer;
function CreateSlopeMap(WhichDEM : integer; OpenMap : boolean = true; Components : boolean = false) : integer;
function MakeAspectMap(OpenMap : boolean; DEM : integer; OutName : PathStr = '') : integer;

function CreateStandardDeviationMap(OpenMap : boolean; DEM,BoxSize : integer; fName : PathStr = '') : integer;

procedure ModeFilterDEM(DEM,BufferSize : integer; JustDoHoles : boolean);

function MakeTRIGrid(CurDEM : integer; Normalize : tNormalMethod; OpenMap : boolean = true; OutName : PathStr = '') : integer;
function MakeTPIGrid(CurDEM : integer; Normalize : tNormalMethod; OpenMap : boolean = true) : integer;
function MakeSpecifiedTPIGrid(CurDEM : integer; GridLimits : tGridLimits; Normalize : tNormalMethod; OpenMap : boolean = true) : integer;
function MakeMAD2KGrid(CurDEM : integer; OpenMap : boolean = true) : integer;

function CreateSpecifiedRoughnessMap(DEM : integer; GridLimits : tGridLimits;OpenMap : boolean = true; SaveSlopeMap : boolean = true) : integer;

function CreateRoughnessMap(WhichDEM : integer; OpenMap : boolean = true) : integer;
function CreateRoughnessMap2(DEM : integer; OpenMap : boolean = true; SaveSlopeMap : boolean = true) : integer;
function CreateRoughnessMapAvgVector(WhichDEM : integer; OpenMap : boolean = true) : integer;

function CreateRoughnessSlopeStandardDeviationMap(DEM,DiameterMustBeOdd : integer; OpenMap : boolean = true) : integer;
function CreateSlopeRoughnessSlopeStandardDeviationMap(DEM,DiameterMustBeOdd : integer; var SlopeMap : integer; OpenMap : boolean = true) : integer;

procedure MakeGammaGrids(CurDEM,BoxSize : integer);

function DifferenceCategoryMap(DEMonMap : integer; fName : PathStr = ''; OpenMap : boolean = true) : integer;

function MakeDNBRMap(PreNBR,PostNBR : integer) : integer;

function CreateSecondOrderPartialDerivatives(OpenMap : boolean; DEM : integer; Radius : integer = 0) : integer;
function CreateProfileCurvature(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
function CreateTangentialCurvature(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
function CreatePlanCurvature(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
function CreateFlowLineCurvature(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
function CreateContourTorsion(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;


procedure GRASS_partialDerivatives(DEM : integer; var Grids : tPartialGrids; OpenMap : boolean = true);

procedure MICRODEM_partialDerivatives(DEM : integer; var Grids : tPartialGrids; OpenMap : boolean = true);

function MakeNEneighborGrid(CurDEM : integer; Normalize : tNormalMethod; OpenMap : boolean = true; OutName : PathStr = '') : integer;


{$IfDef ExExoticMaps}
{$Else}
    function CreateAnOrganizationMap(WhichDEM : integer; OpenMap : boolean = true) : integer;
{$EndIf}


function CreateArcSecDEM(OverWrite,OpenMap : boolean; DEM : integer; PixelIs : byte; xgridsize,ygridsize : float32; fName : PathStr) : integer;
function CreateUTMDEM(OverWrite,OpenMap : boolean; DEM : integer; PixelIs : byte; xgridsize,ygridsize : float32; fName : PathStr) : integer;
function MakeGridFullNeighborhoods(DEM : integer; OpenMap : boolean; NeighborhoodRadius : integer) : integer;

function GridDiffernces(PercentDifference : boolean = false) : integer;
procedure GridDifferncesBothInterpolationBasis(PercentDifference : boolean = false);
function MakeDifferenceMap(Map1,Map2,GridResultionToUse,GridToMergeShading : integer; ShowMap,ShowHistogram,ShowScatterPlot : boolean; TheAreaName : ShortString = '';
       PercentDifference : boolean = false) : integer;

procedure PickAspectDifferenceMap(DEM,Window : integer);



var
   NewTrendDirGrid : integer;
   MomentDEMs : tListOfDEMs;

implementation

uses
   DEMCoord,Check_8_Dirs,DEM_Manager,DEMOptions,
   PetDBUtils,
   DEMDef_routines,Petimage,DEMRefOp,DEMterrC,
   DEMStat,DEMMapf,
   Geomorph_point_class,
   MD_use_tools,
   gdal_tools,
   DEMEros,
   BaseGraf,
   dem_grid_diffs,
   DEMStringGrid,
   PetImage_Form,
   DEMweapn;

var
   CountInStrips : integer;

procedure PickAspectDifferenceMap(DEM,Window : integer);
var
   AspectDEM : integer;
begin
   if DEMglb[DEM].DEMheader.ElevUnits = euAspectDeg then AspectDEM := DEM
   else AspectDEM := MakeAspectMap(true,DEM);
   if Window in [1,99] then AspectDifferenceMap(AspectDEM,1,DEMGlb[AspectDEM].FullDEMGridLimits);
   if Window in [2,99] then AspectDifferenceMap(AspectDEM,2,DEMGlb[AspectDEM].FullDEMGridLimits);
   if Window in [3,99] then AspectDifferenceMap(AspectDEM,3,DEMGlb[AspectDEM].FullDEMGridLimits);
end;



function GridDiffernces(PercentDifference : boolean = false) : integer;
var
   DEM1,DEM2,GridForResult : integer;
   aName : shortstring;
begin
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridDifference in'); {$EndIf}
   GetTwoCompatibleGrids('Result = DEM1 - DEM2)',true,DEM2,DEM1,false,true);
   if (DEM1 <> 0) and (DEM2 <> 0) then begin
      aName := 'Difference_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName;
      if PercentDifference then aName := 'Percent ' + aName;

      {$IfDef RecordDEMCompare} WriteLineToDebugFile(aName); {$EndIf}
      SetGridDiffernceProperties(DEM1,DEM2,GridForResult);
      Result := MakeDifferenceMap(DEM1,DEM2,DEM1,0,MDDef.ShowGridDiffMap,MDDef.ShowGridDiffHistogram,MDDef.ShowScatterPlot,aName,PercentDifference);
      {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridDifference out'); {$EndIf}
   end
   else begin
      {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridDifference fail, not 2 different DEMs'); {$EndIf}
   end;
end;

procedure GridDifferncesBothInterpolationBasis(PercentDifference : boolean = false);
var
   DEM1,DEM2,Diff1,Diff2 : integer;
   aName : shortstring;
begin
   {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridDifferncesBothInterpolationBasis in'); {$EndIf}
   GetTwoCompatibleGrids('Result = DEM1 - DEM2)',true,DEM2,DEM1,false,true);
   if (DEM1 <> 0) and (DEM2 <> 0) then begin
      MDDef.HighlightDiffMap := 2;
      aName := 'Difference_' + DEMGlb[DEM2].AreaName + '-' + DEMGlb[DEM1].AreaName;
      {$IfDef RecordDEMCompare} WriteLineToDebugFile(aName); {$EndIf}
      //SetGridDiffernceProperties(DEM1,DEM2,GridForResult);

      aName := 'Difference_' + DEMGlb[DEM2].AreaName + '_minus_' + DEMGlb[DEM1].AreaName + '_projection';
      if PercentDifference then aName := 'Percent ' + aName;
      Diff1 := MakeDifferenceMap(DEM1,DEM2,DEM1,0,true,false,false,aName,PercentDifference);

      aName := 'Difference_' + DEMGlb[DEM2].AreaName+ '_projection_minus_' + DEMGlb[DEM1].AreaName;
      if PercentDifference then aName := 'Percent ' + aName;
      Diff2 := MakeDifferenceMap(DEM1,DEM2,DEM2,0,true,false,false,aName,PercentDifference);
      //DEMGlb[GridForResult].MultiplyGridByConstant(-1);
      //DEMGlb[GridForResult].SelectionMap.DoBaseMapRedraw;

      //MDDef.DivergenceRange := MaxFloat(-DEMglb[Diff1].DEMheader.MinElev,DEMglb[Diff1].DEMheader.MaxElev,-DEMglb[Diff2].DEMheader.MinElev,DEMglb[Diff2].DEMheader.MaxElev);
      MDDef.DivergenceRange := MaxFloat(-DEMglb[Diff1].FindPercentileElev(5),DEMglb[Diff1].FindPercentileElev(95),-DEMglb[Diff2].FindPercentileElev(5),DEMglb[Diff2].FindPercentileElev(95));
      DEMGlb[Diff1].SelectionMap.DoBaseMapRedraw;
      DEMGlb[Diff2].SelectionMap.DoBaseMapRedraw;

      {$IfDef RecordDEMCompare} WriteLineToDebugFile('GridDifferncesBothInterpolationBasis out'); {$EndIf}
   end;
end;



function MakeDifferenceMap(Map1,Map2,GridResultionToUse,GridToMergeShading : integer; ShowMap,ShowHistogram,ShowScatterPlot : boolean; TheAreaName : ShortString = ''; PercentDifference : boolean = false) : integer;
var
   Col,Row,mt,OtherGrid,ThisGrid,xoff,yoff  : integer;
   z1,z2 : float32;
   fName : PathStr;
   ThisGraph : TThisBaseGraph;
   IdenticalGrids : boolean;
   TStr : shortstring;
   rFile : file;


         procedure SetPoint(DEM,Col,Row : integer; z1,z2: float32);
         const
            DiffTol = 0.1;
         begin
            if PercentDifference then begin
               if (abs(z1-z2) < DiffTol) then DEMGlb[Result].SetGridElevation(Col,Row,0)
               else DEMGlb[DEM].SetGridElevation(Col,Row,100 * (z1 - z2)/ (0.5 * (z1 + z2) ));
            end
            else begin
               DEMGlb[DEM].SetGridElevation(Col,Row,z1 - z2);
            end;
         end;


begin
   if ValidDEM(Map1) and ValidDEM(Map2) then begin
      try
         {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap)}
            WriteLineToDebugFile('MakeDifferenceMapOfBoxRegion in, ' + DEMGlb[Map1].AreaName + ' ' + DEMGlb[Map1].zRange + ' minus ' + DEMGlb[Map2].AreaName + ' ' + DEMGlb[Map2].zRange);
         {$EndIf}
         HeavyDutyProcessing := true;
         Result := 0;
         OpenDEMDataStructures(Result);

         if (TheAreaName = '') then begin
            if PercentDifference then TStr := 'percent_';
            DEMGlb[Result].AreaName := 'Elev_Diff_' + TStr + DEMGlb[Map2].AreaName + '_minus_' + DEMGlb[Map1].AreaName
         end
         else DEMGlb[Result].AreaName := TheAreaName;

         DEMGlb[Result].DEMheader := DEMGlb[GridResultionToUse].DEMheader;
         DEMGlb[Result].DEMheader.VerticalCSTypeGeoKey := 0;  //difference not referred to a datum
         if DEMGlb[Result].DEMheader.DEMPrecision = ByteDEM then DEMGlb[Result].DEMheader.DEMPrecision := SmallIntDEM
         else DEMGlb[Result].DEMheader.DEMPrecision := FloatingPointDEM;

         if DEMGlb[Map1].ElevationDEM and DEMGlb[Map2].ElevationDEM then DEMGlb[Result].DEMheader.ElevUnits := euElevDiff else DEMGlb[Result].DEMheader.ElevUnits := euDifference;

         DEMGlb[Result].DEMstatus := dsUnsaved;
         if (GridResultionToUse = Map1) then begin
            ThisGrid := Map1;
            OtherGrid := Map2;
         end
         else begin
            ThisGrid := Map2;
            OtherGrid := Map1;
         end;

         if not DEMGlb[Result].AllocateDEMMemory(InitDEMMissing) then exit;

         if ShowScatterPlot then begin
            ThisGraph := TThisBaseGraph.Create(Application);
            ThisGraph.SetUpGraphForm;
            ThisGraph.Caption := 'Grid comparison';
            ThisGraph.OpenPointFile(rfile,ThisGraph.Symbol);
            ThisGraph.GraphDraw.HorizLabel := DEMGlb[Map1].AreaName;
            ThisGraph.GraphDraw.VertLabel := DEMGlb[Map2].AreaName;
         end;
         IdenticalGrids := DEMGlb[ThisGrid].SecondGridJustOffset(OtherGrid,xoff,yoff);

         StartProgress('Grid Differences');
         for Col := 0 to pred(DEMGlb[ThisGrid].DEMHeader.NumCol) do begin
            if (Col mod 25 = 0) then UpdateProgressBar(Col / pred(DEMGlb[ThisGrid].DEMheader.NumCol));
            for Row := 0 to pred(DEMGlb[ThisGrid].DEMHeader.NumRow) do begin
               if IdenticalGrids then begin
                  if DEMGlb[ThisGrid].GetElevMeters(Col,Row,z1) and DEMGlb[OtherGrid].GetElevMeters(Col+xoff,Row+yoff,z2) then begin
                     SetPoint(Result,Col,Row,z1,z2);
                     if ShowScatterPlot then ThisGraph.AddPointToDataBuffer(rfile,z1,z2);
                  end;
               end
               else begin
                  if DEMGlb[ThisGrid].GetElevMetersFromThisAndSecondDEM(OtherGrid,Col,Row,z1,z2) then begin
                     SetPoint(Result,Col,Row,z1,z2);
                     if ShowScatterPlot then ThisGraph.AddPointToDataBuffer(rfile,z1,z2);
                  end;
               end;
            end {for Row};
         end {for Col};
         EndProgress;
         if (GridResultionToUse = Map2) then begin
            DEMGlb[Result].MultiplyGridByConstant(-1);
         end;
         DEMGlb[Result].CheckMaxMinElev;
         {$If Defined(TrackZRange)} WriteLineToDebugFile('MakeDifferenceMapOfBoxRegion did check: ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange); {$EndIf}
         {$IfDef RecordFullDEMCompare}
            WriteLineToDebugFile('');  WriteLineToDebugFile('DEM1: ' + DEMGlb[Map1].FullDEMParams);
            WriteLineToDebugFile('');  WriteLineToDebugFile('DEM2: ' + DEMGlb[Map2].FullDEMParams);
            WriteLineToDebugFile('');  WriteLineToDebugFile('Diff: ' + DEMGlb[Result].FullDEMParams);
            WriteLineToDebugFile('');
         {$EndIf}

         if ShowMap then begin
            {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap) or Defined(TrackZRange)}
               WriteLineToDebugFile('MakeDifferenceMapofBoxRegion call setupmap ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange);
            {$EndIf}
            if PercentDifference then mt := mtDifferenceDiverge
            else if MDDef.HighlightDiffMap = 0 then mt := mtElevSpectrum
            else if MDDef.HighlightDiffMap = 1 then mt := mtGGRReflect
            else if MDDef.HighlightDiffMap = 2 then mt := mtDifferenceDiverge;
            DEMGlb[Result].SetupMap(true,mt);
            if MDDef.AutoMergeStartDEM and ValidDEM(GridToMergeShading) then begin
               DEMGlb[Result].SelectionMap.MergeAnotherDEMreflectance(GridToMergeShading,true);
            end;
            {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap) or Defined(TrackZRange)}
               WriteLineToDebugFile('MakeDifferenceMapofBoxRegion done ShowMap ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange);
            {$EndIf}
         end;
         DEMGlb[Result].CheckMaxMinElev;  //because it is reset somewhere in the ShowMapBlock

         if ShowScatterPlot then begin
            CloseFile(rfile);
            ThisGraph.AutoScaleAndRedrawDiagram;
         end;
         if ShowHistogram then CreateWholeDEMHistogram(Result);  //SingleDEMHistogram(Result);
         {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap) or Defined(TrackZRange)} WriteLineToDebugFile('MakeDifferenceMapofBoxRegion out, ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange); {$EndIf}
      finally
         HeavyDutyProcessing := false;
         ShowDefaultCursor;
      end;
   end
   else begin
      {$If Defined(RecordDEMCompare) or Defined(RecordDiffMap)} WriteLineToDebugFile('MakeDifferenceMapofBoxRegion called with invalid grid'); {$EndIf}
   end;
end;



function MakeGridFullNeighborhoods(DEM : integer; OpenMap : boolean; NeighborhoodRadius : integer) : integer;
var
   x,y,NumPts : integer;
begin
   Result := DEMglb[DEM].CloneAndOpenGridSetMissing(byteDEM,'Full_window_mask',euUndefined);
   NumPts := 0;
   for x := 0 to pred(DEMglb[DEM].DEMHeader.NumCol) do begin
      for y := 0 to pred(DEMglb[DEM].DEMHeader.NumRow) do begin
         if DEMglb[DEM].FullAnalysisWindow(x,y,NeighborhoodRadius) then begin
            inc(NumPts);
            DEMGlb[Result].SetGridElevation(x,y,1);
         end
         else DEMGlb[Result].SetGridMissing(x,y);
      end;
   end;
   DEMglb[Result].CheckMaxMinElev;
   DEMglb[Result].DEMHeader.ElevUnits := euIntCode;
   DEMglb[Result].AreaName := DEMglb[DEM].AreaName + '_full_neighborhood_' + FilterSizeStr(succ(2*NeighborhoodRadius));
   if OpenMap then DEMglb[Result].SetUpMap(false);
   {$IfDef RecordCompareLSPs}
      WriteLineToDebugFile('FilterToFullAnalysisWindow using ' + DEMglb[DEM].AreaName + '  pts=' + IntToStr(DEMglb[DEM].ValidElevsInDEM) + ' filled=' + IntToStr(NumPts));
      WriteLineToDebugFile('Mask DEM=' + IntToStr(DEMglb[Result].ValidElevsInDEM));
   {$EndIf}
end;



function CreateArcSecDEM(OverWrite,OpenMap : boolean; DEM : integer; PixelIs : byte; xgridsize,ygridsize : float32; fName : PathStr) : integer;
begin
   if (not Overwrite) and FileExists(fName) then begin
      Result := OpenNewDEM(fName);
   end
   else begin
      MDDef.LidarGridProjection := ArcSecDEM;
      MDdef.DefLidarGeoGridSizeX := xgridsize;
      MDdef.DefLidarGeoGridSizeY := ygridsize;
      MDDef.LasDEMPixelIs := PixelIs;
      {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('CreateOneRefDEM, ' + fName); {$EndIf}
      Result := DEMGlb[DEM].ResampleByAveraging(OpenMap,fName);
      {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('ResampleForDEMIXOneSecDEMs, new DEM=' + IntToStr(Result)); {$EndIf}
      {$If Defined(TrackDEMCorners)} DEMGlb[Result].WriteDEMCornersToDebugFile('ResampleForDEMIXOneSecDEMs, new DEM=' + IntToStr(Result)); {$EndIf}
   end;
end;


function CreateUTMDEM(OverWrite,OpenMap : boolean; DEM : integer; PixelIs : byte; xgridsize,ygridsize : float32; fName : PathStr) : integer;
begin
   if (not Overwrite) and FileExists(fName) then begin
      Result := OpenNewDEM(fName);
   end
   else begin
      MDDef.LidarGridProjection := UTMbasedDEM;
      MDdef.DefLidarGeoGridSizeX := xgridsize;
      MDdef.DefLidarGeoGridSizeY := ygridsize;
      MDDef.LasDEMPixelIs := PixelIs;
      {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('CreateOneRefDEM, ' + fName); {$EndIf}
      Result := DEMGlb[DEM].ResampleByAveraging(OpenMap,fName);
      {$If Defined(Record3DEPXFull)} WriteLineToDebugFile('ResampleForDEMIXOneSecDEMs, new DEM=' + IntToStr(Result)); {$EndIf}
      {$If Defined(TrackDEMCorners)} DEMGlb[Result].WriteDEMCornersToDebugFile('ResampleForDEMIXOneSecDEMs, new DEM=' + IntToStr(Result)); {$EndIf}
   end;
end;



procedure GRASS_partialDerivatives(DEM : integer; var Grids : tPartialGrids;  OpenMap : boolean = true);
var
   i : integer;
begin
   grids[1] := Grass_dx_partial(DEMGlb[DEM].GeotiffDEMName);
   grids[2] := Grass_dy_partial(DEMGlb[DEM].GeotiffDEMName);
   grids[3] := Grass_dxx_partial(DEMGlb[DEM].GeotiffDEMName);
   grids[4] := Grass_dyy_partial(DEMGlb[DEM].GeotiffDEMName);
   grids[5] := Grass_dxy_partial(DEMGlb[DEM].GeotiffDEMName);
   for i := 1 to 5 do begin
      DEMglb[Grids[i]].MultiplyGridByConstant(-1);
      DEMglb[Grids[i]].CheckMaxMinElev;
   end;
end;

procedure MICRODEM_partialDerivatives(DEM : integer; var Grids : tPartialGrids;  OpenMap : boolean = true);
var
   i,x,y : integer;
   AreaName : shortstring;
   SlopeAspectRec : tSlopeAspectRec;
begin
   for i := 1 to 5 do begin
      grids[i] := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,PartialNames[i] + '_' + DEMGlb[DEM].AreaName,euUndefined);
   end;
   if ShowSatProgress then StartProgressAbortOption('Slope');
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      if ShowSatProgress and (x mod 100 = 0) then UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if DEMGlb[DEM].GetSlopeAndAspect(x,y,SlopeAspectRec) then begin
            DEMGlb[grids[1]].SetGridElevation(x,y,SlopeAspectRec.dzdx);
            DEMGlb[grids[2]].SetGridElevation(x,y,SlopeAspectRec.dzdy);
            DEMGlb[grids[3]].SetGridElevation(x,y,SlopeAspectRec.dxx);
            DEMGlb[grids[4]].SetGridElevation(x,y,SlopeAspectRec.dyy);
            DEMGlb[grids[5]].SetGridElevation(x,y,SlopeAspectRec.dxy);
         end;
      end;
   end;
   for i := 1 to 5 do DEMglb[Grids[i]].CheckMaxMinElev;
   if ShowSatProgress then EndProgress;
   if OpenMap then for i := 1 to 5 do DEMglb[Grids[i]].SetUpMap(false);
end;



function BoxCarDetrendDEM(OpenMap : boolean; DEM : integer; GridLimits : tGridLimits; FilterRadius : integer = 2) : integer;
var
   x,y,xr,yr,n : integer;
   Sum,zt,z : float32;
begin
   if ValidDEM(DEM) then with DEMGlb[DEM] do begin
      Result := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_detrend_residual',DEMHeader.ElevUnits);
      if (FilterRadius < 1) then ReadDefault('radius to filter (pixels)',FilterRadius);
      StartProgress('Detrend/residual ' + AreaName);
      for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
         if (x mod 50 = 0) then UpDateProgressBar(x/pred(DEMheader.NumCol));
         for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
            Sum := 0;
            n := 0;
            if GetElevMetersOnGrid(x,y,zt) then begin
               for xr := (x - FilterRadius) to (x + FilterRadius) do begin
                  for yr := (y - FilterRadius) to (y + FilterRadius) do begin
                     if GetElevMetersOnGrid(xr,yr,z) then begin
                        Sum := Sum + z;
                        inc(n);
                     end;
                  end;
               end;
               if (n > 0) then begin
                  zt := zt - (sum / n);
                  DEMGlb[Result].SetGridElevation(x,y,zt);
               end;
            end;
         end;
      end;
      DEMGlb[Result].CheckMaxMinElev;
      if OpenMap then DEMGlb[Result].SetupMap(false,mtElevSpectrum);
   end;
end;


function MakeDNBRMap(PreNBR,PostNBR : integer) : integer;
var
   PreFire,PostFire : integer;
   AreaName : shortstring;
begin
   {$IfDef RecordSat} WriteLineToDebugFile('TMapForm.dNBRNBRbeforeandafterfire1Click in'); {$EndIf}
    if ValidDEM(PreNBR) and ValidDEM(PostNBR) then begin
    end
    else begin
       if GetImage(PreFire,true,'Pre fire image') and GetImage(PostFire,true,'Post fire image') then begin
          PreNBR := SatImage[PreFire].SelectionMap.NewSatWindow(nsbNBRNormalizedBurnIndex);
          PostNBR := SatImage[PostFire].SelectionMap.NewSatWindow(nsbNBRNormalizedBurnIndex);
       end
       else begin
          Result := 0;
          exit;
       end;
    end;
    Result := MakeDifferenceMap(PreNBR,PostNBR,PreNBR,0,true,false,false,AreaName);
    DEMGlb[Result].DEMHeader.ElevUnits := euDNBR;
    DEMGlb[Result].SelectionMap.MapDraw.MapType := mtElevFromTable;
    ElevationFixedPalette := 'dNBR';
    DEMGlb[Result].SelectionMap.DoBaseMapRedraw;
   {$IfDef RecordSat} WriteLineToDebugFile('TMapForm.dNBRNBRbeforeandafterfire1Click out'); {$EndIf}
end;


function MakeAspectMap(OpenMap : boolean; DEM : integer; OutName : PathStr = '') : integer;
var
   x,y : integer;
   SlopeAspectRec : tSlopeAspectRec;
begin
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('MakeAspectMap in, method=' + IntToStr(MDDef.SlopeAlgorithm)); {$EndIf}
   Result := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'aspect_' + DEMGlb[DEM].AreaName,euAspectDeg);
   if (OutName <> '') then DEMGlb[Result].AreaName := ExtractFileNameNoExt(OutName);
   StartProgress('Aspect difference');
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if DEMGlb[DEM].GetSlopeAndAspect(x,y,SlopeAspectRec) then begin
            if (SlopeAspectRec.Dir <> cdFlat) then begin
               DEMGlb[Result].SetGridElevation(x,y,SlopeAspectRec.AspectDirTrue);
            end;
         end;
      end;
   end;
   DEMglb[Result].CheckMaxMinElev;
   EndProgress;
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('MakeAspectMap, ' + DEMglb[Result].ZRange); {$EndIf}
   if OpenMap then begin
       DEMglb[Result].SetupMap(false,mtDEMAspect);
       {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('MakeAspectMap out'); {$EndIf}
   end;
end;


function DifferenceCategoryMap(DEMonMap : integer; fName : PathStr = ''; OpenMap : boolean = true) : integer;
//currently no way to get to 5 cat, which is hard coded not possible
const
   HighTolerance = 10.51;
   MediumTolerance = 5.51;
   SimpleTolerance = 2.51;
   ThreeCat = true;
   FiveCat = false;
var
   i : integer;
   z2,What : float32;
   x,y : integer;
   VAT : tStringList;
   //TStr : shortstring;
   Hist : array[1..7] of int64;
begin
   Result := 0;
   if ValidDEM(DEMonMap) then begin
      for i := 1 to 7 do Hist[i] := 0;
      if (fName = '') then fName := 'high_low_' + DEMGlb[DEMonMap].AreaName;
      Result := DEMGlb[DEMonMap].CloneAndOpenGridSetMissing(ByteDEM,fName,euIntCode);
      //StartProgressAbortOption('DEM Difference category');
      for x := 0 to pred(DEMGlb[DEMonMap].DEMheader.NumCol) do begin
         //UpdateProgressBar(x/DEMGlb[DEMonMap].DEMheader.NumCol);
         for y := 0 to pred(DEMGlb[DEMonMap].DEMheader.NumRow) do begin
            if DEMGlb[DEMonMap].GetElevMetersOnGrid(x,y,z2) then begin
               if ThreeCat then begin
                  if z2 > MDDef.TopCutLevel then What := 5
                  else if z2 < MDDef.BottomCutLevel then What := 1
                  else What := 3;
               end
               else if FiveCat then begin
                  if z2 > HighTolerance then What := 1
                  else if z2 > MediumTolerance then What := 2
                  else if z2 > -SimpleTolerance then What := 3
                  else if z2 > -MediumTolerance then What := 4
                  else What := 5;
               end;
               DEMglb[Result].SetGridElevation(x,y,what);
               inc(Hist[round(what)]);
            end;
         end;
      end;
      Vat := tStringList.Create;
      Vat.add('VALUE,NAME,N,USE,COLOR');

      if ThreeCat then begin
         if (Hist[5] > 0) then Vat.add('5,High,' + IntToStr(Hist[5]) + ',Y,' + IntToStr(clGreen));
         if (Hist[3] > 0) then Vat.add('3,Equal ± ' + RealToString(SimpleTolerance,-5,1)  + ',' + IntToStr(Hist[3]) + ',Y,' + IntToStr(clWhite));
         if (Hist[1] > 0) then Vat.add('1,Low,' + IntToStr(Hist[1]) + ',Y,' + IntToStr(clRed));
      end
      else if FiveCat then begin
         if (Hist[1] > 0) then Vat.add('1,High > ' + RealToString(HighTolerance,-5,-1) + ',' + IntToStr(Hist[1]) + ',Y,' + IntToStr(clGreen));
         if (Hist[2] > 0) then Vat.add('2,Medium High,' + IntToStr(Hist[2]) + ',Y,' + IntToStr(clYellow));
         if (Hist[3] > 0) then Vat.add('3,Equal ± ' + RealToString(SimpleTolerance,-5,1)  + ',' + IntToStr(Hist[3]) + ',Y,' + IntToStr(clWhite));
         if (Hist[4] > 0) then Vat.add('4,Medium Low,' + IntToStr(Hist[4]) + ',Y,' + IntToStr(clMagenta));
         if (Hist[5] > 0) then Vat.add('5,Low < ' + RealToString(-HighTolerance,-6,-1) + ',' + IntToStr(Hist[5]) + ',Y,' + IntToStr(clRed));
      end;
      fName := NextFileNumber(MDTempDir,fName + '_','.vat.dbf');
      StringList2CSVtoDB(vat,fName,true);
      DEMGlb[Result].VATFileName := fName;
      DEMglb[Result].CheckMaxMinElev;
      if OpenMap then DEMglb[Result].SetupMap(false,mtDEMVATTable);
   end;
end;


function ComputeCurvature(DEM,CurveType,x,y,Radius : integer; var Curvature : float64) : boolean;  inline;
var
   SlpAsp : tSlopeAspectRec;

         function Denominator1(SlpAsp : tSlopeAspectRec) : float64; inline;
         begin
            Result := (sqr(SlpAsp.dzdx) + sqr(SlpAsp.dzdy)) * sqrt(1 + sqr(SlpAsp.dzdx) + sqr(SlpAsp.dzdy));
         end;

         function Denominator2(SlpAsp : tSlopeAspectRec) : float64; inline;
         begin
            Result := sqrt( Math.Power(sqr(SlpAsp.dzdx) + sqr(SlpAsp.dzdy), 3) ) ;
         end;

         function LSQ1(SlpAsp : tSlopeAspectRec) : float64; inline;
         begin
            Result := sqr(SlpAsp.d) + sqr(SlpAsp.e);
         end;

         function LSQ2(SlpAsp : tSlopeAspectRec) : float64; inline;
         begin
            Result := 1 + sqr(SlpAsp.d) + sqr(SlpAsp.e);
         end;

         function LSQ3(SlpAsp : tSlopeAspectRec) : float64; inline;
         begin
            Result := Math.Power(LSQ2(SlpAsp),1.5);
         end;


begin
   Result := DEMGlb[DEM].GetSlopeAndAspect(x,y,SlpAsp,true,Radius);
   if Result then begin
      with SlpAsp do begin
         if (MDDef.SlopeAlgorithm = smLSQ) then begin
            case CurveType of
               1 : Curvature := -2 * (a * sqr(d) + c * d * e + b * sqr(e)) / (LSQ1(SlpAsp) * LSQ3(SlpAsp)) ;     //Profile
               2 : Curvature :=  0;  //Tangential, not yet coded
               3 : Curvature := -2 * (a * sqr(e) - c * d * e + b * sqr(d)) / (LSQ1(SlpAsp) * sqrt(LSQ2(SlpAsp))); //Plan
               4 : Curvature :=  0;   //flow line, not yet coded
               5 : Curvature := (2 * d * (a - b) - c * (sqr(d) - sqr(e))) / (LSQ1(SlpAsp) * LSQ2(SlpAsp));    //contour torsion
            end;

         end
         else begin
            case CurveType of
               1 : Curvature := -(dxx * sqr(dzdx) + 2 * dxy * dzdx * dzdy + dyy * sqr(dzdy) ) / Denominator1(SlpAsp);    //Profile
               2 : Curvature := -(dxx * sqr(dzdy) - 2 * dxy * dzdx * dzdy + dyy * sqr(dzdx) ) / Denominator1(SlpAsp);    //Tangential
               3 : Curvature := -(dxx * sqr(dzdy) - 2 * dxy * dzdx * dzdy + dyy * sqr(dzdy) ) / Denominator2(SlpAsp);    //Plan
               4 : Curvature := (dzdx * dzdy * (dxx - dyy) - dxy * (sqr(dzdx) - sqr(dzdy) ) ) / Denominator2(SlpAsp);    //flow line
               5 : Curvature := (dzdx * dzdy * (dxx - dyy) - dxy * sqr(dzdx) * sqr(dzdy) ) / Denominator1(SlpAsp);       //contour torsion
            end;
         end;
      end;
   end;
end;


function CreateCurvatureMap(Which : integer; OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
var
   x,y : integer;
   Curvature : float64;
   SlpAsp : tSlopeAspectRec;
   aName : shortstring;
begin
   case Which of
      1 : aName := 'profile_curvature_';
      2 : aName := 'tangential_curvature_';
      3 : aName := 'plan_curvature_';
      4 : aName := 'flow_line_curvature_';
      5 : aName := 'contour_torsion_';
      //6 : aName := 'min_curvature_';
      //7 : aName := 'max_curvature_';
      //8 : aName := 'cross_curvature_';
   end;

   if (OutName = '') then OutName := aName + DEMGlb[DEM].AreaName;
   Result := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,OutName,euPerMeter);
   {$IfDef CreateGeomorphMaps} if (not DEMGlb[Result].DEMAlreadyDefined) then WriteLineToDebugFile(Outname + 'not yet defined at step 1'); {$EndIf}
   if ShowSatProgress then StartProgressAbortOption(aName);
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      if ShowSatProgress and (x mod 100 = 0) then UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if ComputeCurvature(DEM,Which,x,y,Radius,Curvature) then begin
            DEMGlb[Result].SetGridElevation(x,y,Curvature);
         end;
      end;
   end;
   {$IfDef CreateGeomorphMaps}  if (not DEMGlb[Result].DEMAlreadyDefined) then WriteLineToDebugFile(Outname + 'not yet defined at step 2'); {$EndIf}
   DEMglb[Result].CheckMaxMinElev;
   if ShowSatProgress then EndProgress;
   if OpenMap then begin
      {$IfDef CreateGeomorphMaps}  if (not DEMGlb[Result].DEMAlreadyDefined) then WriteLineToDebugFile(Outname + 'not yet defined at step 3'); {$EndIf}
      DEMglb[Result].SetupMap(false,mtCurvature);
   end;
   {$IfDef CreateGeomorphMaps}  if (not DEMGlb[Result].DEMAlreadyDefined) then WriteLineToDebugFile(Outname + 'not yet defined at step 4'); {$EndIf}
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateCurvatureMap, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProj.ProjDebugName); {$EndIf}
 end;


function CreateProfileCurvature(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
begin
   Result := CreateCurvatureMap(1,OpenMap,DEM,Radius,Outname);
end;

function CreateTangentialCurvature(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
begin
   Result := CreateCurvatureMap(2,OpenMap,DEM,Radius,Outname);
end;

function CreatePlanCurvature(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
begin
   Result := CreateCurvatureMap(3,OpenMap,DEM,Radius,Outname);
end;


function CreateFlowLineCurvature(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
begin
   Result := CreateCurvatureMap(4,OpenMap,DEM,Radius,Outname);
end;

function CreateContourTorsion(OpenMap : boolean; DEM : integer; Radius : integer = 1; Outname : PathStr = '') : integer;
begin
   Result := CreateCurvatureMap(5,OpenMap,DEM,Radius,Outname);
end;


function CreateSecondOrderPartialDerivatives(OpenMap : boolean; DEM : integer; Radius : integer = 0) : integer;
var
   x,y : integer;
   pxx,pxy,pyy : integer;
   SlpAsp : tSlopeAspectRec;
begin
   pxx := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'dxx_' + DEMGlb[DEM].AreaName,euMperM);
   pxy := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'dxy_' + DEMGlb[DEM].AreaName,euMperM);
   pyy := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'dyy_' + DEMGlb[DEM].AreaName,euMperM);
   if ShowSatProgress then StartProgressAbortOption('2d order partials');
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      if ShowSatProgress and (x mod 100 = 0) then UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if DEMGlb[DEM].GetSlopeAndAspect(x,y,SlpAsp,true,1) then begin
            DEMGlb[pxx].SetGridElevation(x,y,SlpAsp.dxx);
            DEMGlb[pxy].SetGridElevation(x,y,SlpAsp.dxy);
            DEMGlb[pyy].SetGridElevation(x,y,SlpAsp.dyy);
         end;
      end;
   end;
   DEMglb[pxx].CheckMaxMinElev;
   DEMglb[pxy].CheckMaxMinElev;
   DEMglb[pyy].CheckMaxMinElev;
   if ShowSatProgress then EndProgress;
   if OpenMap then begin
      DEMglb[pxx].SetUpMap(false);
      DEMglb[pxy].SetUpMap(false);
      DEMglb[pyy].SetUpMap(false);
   end;
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateRoughnessMap2, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProj.ProjDebugName); {$EndIf}
end;



function CreateSlopeMapPercent(OpenMap : boolean; DEM : integer; OutName : PathStr = ''; Radius : integer = 0) : integer;
var
   x,y : integer;
   Slope : float64;
begin
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateSlopeMapPercent, method=' + IntToStr(MDDef.SlopeAlgorithm)); {$EndIf}
   Result := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'slope_' + DEMGlb[DEM].AreaName,euPercentSlope);
   if (OutName <> '') then DEMGlb[Result].AreaName := ExtractFileNameNoExt(OutName);
   if ShowSatProgress then StartProgressAbortOption('Slope');
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      if ShowSatProgress and (x mod 100 = 0) then UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if DEMGlb[DEM].SlopePercent(x,y,Slope,Radius) then begin
            DEMGlb[Result].SetGridElevation(x,y,Slope);
         end;
      end;
   end;
   DEMglb[Result].CheckMaxMinElev;
   if ShowSatProgress then EndProgress;
   if OpenMap then DEMglb[Result].SetupMap(false,MDDef.DefSlopeMap);
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateSlopeMapPercent, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProj.ProjDebugName); {$EndIf}
end;


function CreateHillshadeMap(OpenMap : boolean; DEM : integer; Outname : PathStr = '') : integer;
var
   x,y : integer;
   z : float32;
begin
   if (OutName = '') then OutName := 'hillshade_' + DEMGlb[DEM].AreaName;
   Result := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,OutName,euUndefined);
   DEMGlb[DEM].ReflectanceParams;
   if ShowSatProgress then StartProgress('Hillshade');
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      if ShowSatProgress and (x mod 100 = 0) then UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if DEMGlb[DEM].ReflectanceValueFloat(x,y,z) then begin
            DEMGlb[Result].SetGridElevation(x,y,z);
         end;
      end;
   end;
   if ShowSatProgress then EndProgress;
   DEMglb[Result].CheckMaxMinElev;
   if OpenMap then DEMglb[Result].SetupMap(false,mtElevGray);
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateRoughnessMap2, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProj.ProjDebugName); {$EndIf}
end;


procedure CreateOpennessMap(OpenMap : boolean; GridLimits : tGridLimits; DEM,BoxSizeMeters : integer; var Upward,DownWard,Difference : integer);
var
   i,x,y : integer;
   UpO,DownO : float64;

      procedure Finalize(which : integer; mt : byte);
      begin
         if ValidDEM(Which) then begin
            DEMglb[Which].CheckMaxMinElev;
            if OpenMap then DEMglb[Which].SetUpMap(false,mt);
         end;
      end;

begin
   if (Upward <> 0) then Upward := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'Upward_Openness_' + IntToStr(BoxSizeMeters) + '_m_' + DEMGlb[DEM].AreaName,euDegrees);
   if (Downward <> 0) then Downward := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'Downward_Openness_' + IntToStr(BoxSizeMeters) + '_m_' + DEMGlb[DEM].AreaName,euDegrees);
   if (Difference <> 0) then Difference := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'Difference_Openness_' + IntToStr(BoxSizeMeters) + '_m_' + DEMGlb[DEM].AreaName,euDegrees);

   if ShowSatProgress then StartProgressAbortOption('Openness');
   for x := GridLimits.XgridLow to GridLimits.XGridHigh do begin
      if ShowSatProgress and (x mod 100 = 0) then UpdateProgressBar(x/GridLimits.XGridHigh);
      for y := GridLimits.YgridLow to GridLimits.YGridHigh  do begin
         if not DEMGlb[DEM].MissingDataInGrid(x,y) then begin
            DEMGlb[DEM].FigureOpenness(x,y,MDDef.OpenBoxSizeMeters,UpO,DownO);
            if (Upward <> 0) then DEMGlb[Upward].SetGridElevation(x,y,UpO);
            if (Downward <> 0) then DEMGlb[Downward].SetGridElevation(x,y,DownO);
            if (Difference <> 0) then DEMGlb[Difference].SetGridElevation(x,y,UpO-DownO);
         end;
      end;
   end;
   if ShowSatProgress then EndProgress;
   Finalize(Upward,mtElevGray);
   Finalize(Downward,mtElevGrayReversed);
   Finalize(Difference,mtElevGray);
   UpdateMenusForAllMaps;
end;


function CreateRoughnessSlopeStandardDeviationMap(DEM,DiameterMustBeOdd : integer; OpenMap : boolean = true) : integer;
var
   SlopeMap : integer;
begin
   SlopeMap := -1;
   CreateSlopeRoughnessSlopeStandardDeviationMap(DEM,DiameterMustBeOdd,SlopeMap,OpenMap);
end;


function CreateStandardDeviationMap(OpenMap : boolean; DEM,BoxSize : integer; fName : PathStr = '') : integer;
var
   x,y,i,j,Radius,NPts : integer;
   sl : array[1..500] of float32;
   MomentVar : tMomentVar;
   s,s2,svar,Mean,std_dev : float64;
   z : float32;
begin
   if (fName = '') then fName := 'md_elev_std_' + FilterSizeStr(BoxSize) + '_' + DEMGlb[DEM].AreaName;
   Result := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,fName,DEMGlb[DEM].DEMheader.ElevUnits);
   Radius := BoxSize div 2;
   if ShowSatProgress then StartProgressAbortOption('std dev grid');
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      if ShowSatProgress and (x mod 100 = 0) then UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         Npts := 0;
         s := 0;
         s2 := 0;
         for I := x-Radius to x+Radius do begin
            for J := y-Radius to y+Radius do begin
               if DEMGlb[DEM].GetElevMetersOnGrid(i,j,z) then begin
                  inc(Npts);
                  sl[Npts] := z;
                  s := s + z;
                  s2 := s2 + sqr(z);
               end;
            end;
         end;
         if (NPts > 5) then begin
            Mean := s / Npts;
            for j := 1 to Npts do begin
               s := sl[j] - Mean;
               svar := svar + s*s;
            end;
            svar := svar / (Npts-1);
            std_dev := sqrt(svar);
            DEMglb[Result].SetGridElevation(x,y,std_dev);
         end;
      end;
   end;
   DEMglb[Result].CheckMaxMinElev;
   if OpenMap then DEMglb[Result].SetupMap(false,mtElevSpectrum);
   if ShowSatProgress then EndProgress;
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateRoughnessMap2, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProj.ProjDebugName); {$EndIf}
end;


function CreateSlopeRoughnessSlopeStandardDeviationMap(DEM,DiameterMustBeOdd : integer; var SlopeMap : integer; OpenMap : boolean = true) : integer;
//to return slope map, the input value should be 0; otherwise it will be destroyed here
var
   fName : PathStr;
   ReturnSlopeMap : boolean;
   {$If Defined(RecordMapSteps)} MapStopwatch : TStopwatch; {$EndIf}
begin
   ReturnSlopeMap := (SlopeMap = 0);
   {$If Defined(RecordMapSteps)} MapStopwatch := TStopwatch.StartNew; {$EndIf}
   SlopeMap := CreateSlopeMapPercent(OpenMap, DEM);
   {$If Defined(RecordMapSteps)} WriteLineToDebugFile('slope map created   ' + RealToString(MapStopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}

   {$If Defined(RecordMapSteps)} MapStopwatch := TStopwatch.StartNew; {$EndIf}
   fName := 'md_ruff_slope_std_' + FilterSizeStr(DiameterMustBeOdd) + '_' + DEMGlb[DEM].AreaName;
   Result := CreateStandardDeviationMap(OpenMap,SlopeMap,DiameterMustBeOdd,fName);
   {$If Defined(RecordMapSteps)} WriteLineToDebugFile('ruff map created   ' + RealToString(MapStopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   if (not ReturnSlopeMap) then begin
      CloseSingleDEM(SlopeMap);
      SlopeMap := 0;
   end;
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateRoughnessMap2, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProj.ProjDebugName); {$EndIf}
end;


function CreateRoughnessMapAvgVector(WhichDEM : integer; OpenMap : boolean = true) : integer;
begin
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateRoughness in'); {$EndIf}
   SaveBackupDefaults;
   SetAllOrganization(false);
   MDDef.FabricCalcThin := 1;
   MDDef.DoAvgVectStrength := true;
   Result := CreateAnOrganizationMap(WhichDEM,OpenMap);
   RestoreBackupDefaults;
end;


function CreateRoughnessMap(WhichDEM : integer; OpenMap : boolean = true) : integer;
begin
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateRoughness in'); {$EndIf}
   SaveBackupDefaults;
   SetAllOrganization(false);
   MDDef.FabricCalcThin := 1;
   MDDef.DoRoughness := true;
   Result := CreateAnOrganizationMap(WhichDEM,OpenMap);
   RestoreBackupDefaults;
end;


function CreateRoughnessMap2(DEM : integer; OpenMap : boolean = true; SaveSlopeMap : boolean = true) : integer;
begin
    Result := CreateSpecifiedRoughnessMap(DEM,DEMGlb[DEM].FullDEMGridLimits,OpenMap,SaveSlopeMap);
end;


function CreateSpecifiedRoughnessMap(DEM : integer; GridLimits : tGridLimits; OpenMap : boolean = true; SaveSlopeMap : boolean = true) : integer;
var
   SlopeGrid,x,y : integer;
   sl : array[1..9] of float32;
   MomentVar : tMomentVar;
begin
   SlopeGrid := CreateSlopeMap(DEM,OpenMap);
   Result := DEMGlb[DEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'md_roughness_elev_std_3x3_' + DEMGlb[DEM].AreaName,DEMGlb[DEM].DEMheader.ElevUnits);  //,false,1);
   MomentVar.Npts := 9;
   StartProgressAbortOption('roughness');
   for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
      UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
         if DEMGlb[DEM].SurroundedPointElevs(x,y,sl[1],sl[2],sl[3],sl[4],sl[5],sl[6],sl[7],sl[8],sl[9]) then begin
            moment(sl,MomentVar,msAfterStdDev);
            DEMglb[Result].SetGridElevation(x,y,MomentVar.std_dev);
         end;
      end;
   end;
   DEMglb[Result].CheckMaxMinElev;
   if OpenMap then DEMglb[Result].SetupMap(false,mtElevSpectrum);
   if (not SaveSlopeMap) then CloseSingleDEM(SlopeGrid);
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateSpecifiedRoughnessMap out, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProj.ProjDebugName); {$EndIf}
end;


procedure ModeFilterDEM(DEM,BufferSize : integer; JustDoHoles : boolean);
var
   i,x,y,x2,y2,MaxC,MaxL,NewDEM,Missing : integer;
   Hist : array[0..255] of integer;
   z : float32;
begin
   StartProgress('Mode filter');
   NewDEM := DEMGlb[DEM].CloneAndOpenGridSetMissing(ByteDEM,DEMGlb[DEM].AreaName +'_mode_filter',DEMGlb[DEM].DEMheader.ElevUnits);
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if JustDoHoles and DEMGlb[DEM].GetElevMeters(x,y,z) then begin
            DEMGlb[NewDEM].SetGridElevation(x,y,round(z));
         end
         else begin
            for i := 0 to 255 do Hist[i] := 0;
            Missing := 0;
            for x2 := x-BufferSize to x+BufferSize do begin
               for y2 := y-BufferSize to y+BufferSize do
                  if DEMGlb[DEM].GetElevMeters(x2,y2,z) then inc(Hist[round(z)])
                  else inc(Missing);
            end;
            MaxL := 0;
            MaxC := Hist[0];
            for i := 1 to 255 do begin
               if Hist[i] > MaxC then begin
                  MaxL := i;
                  MaxC := Hist[i];
               end;
            end;
            if (MaxC > 0) and (MaxC > Missing) then begin
               DEMGlb[NewDEM].SetGridElevation(x,y,MaxL);
            end
            else begin
               DEMGlb[NewDEM].SetGridMissing(x,y);
            end;
         end;
      end;
   end;
   EndProgress;
   DEMGlb[NewDEM].SetUpMap(true,mtElevSpectrum);
end;


function TheDesiredLimits(CurDEM : integer) : tGridLimits;
begin
   if MDDef.GeomorphMapsFullDEM or (DEMGlb[CurDEM].SelectionMap = Nil) then begin
       Result := DEMGlb[CurDEM].FullDEMGridLimits;
    end
    else begin
       Result := DEMGlb[CurDEM].SelectionMap.MapDraw.MapAreaDEMGridLimits;
    end;
end;


function MakeTPIGrid(CurDEM : integer; Normalize : tNormalMethod; OpenMap : boolean = true) : integer;
begin
   Result := MakeSpecifiedTPIGrid(CurDEM,TheDesiredLimits(CurDEM),Normalize,OpenMap);
end;


procedure GetSpacingMultiples(CurDEM,Row : integer; Normalize : tNormalMethod; var FactorNS,FactorEW,FactorDiag : float32); inline;
//inline for speed and code maintence so multiple uses share the exact same code
var
   DiagonalSpace : float32;
begin
    DiagonalSpace := sqrt(sqr(DEMGlb[CurDEM].XSpaceByDEMrow[Row]) + sqr(DEMGlb[CurDEM].AverageYSpace));
    if (Normalize in [nmEastWest,nmRRI,nmMAD2K]) then begin
       FactorNS := DEMGlb[CurDEM].XSpaceByDEMrow[Row] / DEMGlb[CurDEM].AverageYSpace;
       FactorEW := 1;
       FactorDiag := DEMGlb[CurDEM].XSpaceByDEMrow[Row] / DiagonalSpace;
    end
    else if (Normalize = nmNorthSouth) then begin
       FactorNS := 1;
       FactorEW := DEMGlb[CurDEM].AverageYSpace / DEMGlb[CurDEM].XSpaceByDEMrow[Row];
       FactorDiag := DEMGlb[CurDEM].AverageYSpace / DiagonalSpace;
    end
    else if (Normalize = nm30m) then begin
       FactorNS := 30 / DEMGlb[CurDEM].AverageYSpace;
       FactorEW := 30 / DEMGlb[CurDEM].XSpaceByDEMrow[Row];
       FactorDiag := 30 / DiagonalSpace;
    end
    else if (Normalize in [nmNone,nmTRIK]) then begin
       //Factors remain 1
    end;
end;


function MakeSpecifiedTPIGrid(CurDEM : integer; GridLimits : tGridLimits; Normalize : tNormalMethod; OpenMap : boolean = true) : integer;
var
   Col,Row : integer;
   znw,zw,zsw,zn,z,zs,zne,ze,zse,z1,z11,z21,z3,z23,z5,z15,z25,FactorEW,FactorDiag,FactorNS,DiagonalSpace : float32;
   sum : float64;
   TStr,NormStr : shortstring;
begin
    {$IfDef RecordTimeGridCreate} Stopwatch1 := TStopwatch.StartNew; {$EndIf}
    if (Normalize = nmNorthSouth) then NormStr := '_norm_NS'
    else if (Normalize = nmEastWest) then NormStr := '_norm_EW'
    else if (Normalize = nmInterpolate) then NormStr := '_norm_interpolate'
    else if (Normalize = nm30m) then NormStr := '_norm_30m'
    else if (Normalize = nmTRIK) then NormStr := 'K'
    else if (Normalize = nmNone) then NormStr := '_no_norm';

    TStr := 'TPI_' + NormStr;

    Result := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM, 'MD_' + TStr + '_' + DEMGlb[CurDem].AreaName,euMeters);
    FactorNS := 1;
    FactorEW := 1;
    FactorDiag := 1;

    if ShowSatProgress then StartProgressAbortOption(TStr + ' ' + DEMGlb[CurDEM].AreaName);

    //you could set the normalization factors here, using the average diagonal spacing for the DEM
    //it would not be much faster, and the approximation would be increasingly in error for geographic DEMs as the latitude or size of the DEM increased
    for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
       if (Row mod 25 = 0) and ShowSatProgress then UpdateProgressBar((Row-GridLimits.YGridLow) / (GridLimits.YGridHigh-GridLimits.YGridLow));
       GetSpacingMultiples(CurDEM,Row,Normalize,FactorNS,FactorEW,FactorDiag);

       for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          if DEMGLB[CurDEM].SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
             if (Normalize in [nmNorthSouth,nmEastWest,nm30m]) then begin
                //calculate the elevation in the direction of the neighbor, assuming linear slope in that direction
                znw := z + (zNW-z) * FactorDiag;
                zne := z + (zNE-z) * FactorDiag;
                zsw := z + (zSW-z) * FactorDiag;
                zse := z + (zSE-z) * FactorDiag;
                ze := z + (ze-z) * FactorEW;
                zw := z + (zw-z) * FactorEW;
                zn := z + (zn-z) * FactorNS;
                zs := z + (zs-z) * FactorNS;
             end;
             if (Normalize = nmInterpolate) then begin
                //precise only for UTM grids, but it's so close to the others it's not worth adjusting for geo grids
                DEMGLB[CurDEM].GetElevMeters(Col-0.707,Row+0.707,znw);
                DEMGLB[CurDEM].GetElevMeters(Col+0.707,Row+0.707,zne);
                DEMGLB[CurDEM].GetElevMeters(Col-0.707,Row-0.707,zsw);
                DEMGLB[CurDEM].GetElevMeters(Col+0.707,Row-0.707,zse);
             end;
             Sum := z - (zn+zne+ze+zse+zs+zsw+zw+znw) / 8;
             DEMGlb[Result].SetGridElevation(Col,Row,sum);
          end;
       end;
    end;

    DEMGlb[Result].CheckMaxMinElev;
    if ShowSatProgress then EndProgress;
    if OpenMap then DEMGlb[Result].SetupMap(false,mtElevSpectrum);
    {$IfDef RecordTimeGridCreate} WriteLineToDebugFile('Make TRIGrid took ' + RealToString(Stopwatch1.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
end;


function InterpolateElevs(CurDEM,Col,Row : integer; delta : float32; var znw,zne,zsw,zse : float32) : boolean; inline;
begin
    Result := DEMGLB[CurDEM].GetElevMeters(Col-delta,Row+delta,znw) and DEMGLB[CurDEM].GetElevMeters(Col+delta,Row+delta,zne) and
              DEMGLB[CurDEM].GetElevMeters(Col-delta,Row-delta,zsw) and DEMGLB[CurDEM].GetElevMeters(Col+delta,Row-delta,zse);
end;


function MakeTRIGrid(CurDEM : integer; Normalize : tNormalMethod; OpenMap : boolean = true; OutName : PathStr = '') : integer;
var
   Col,Row : integer;
   GridLimits : tGridLimits;
   znw,zw,zsw,zn,z,zs,zne,ze,zse,z1,z11,z21,z3,z23,z5,z15,z25,FactorEW,FactorDiag,FactorNS,Median : float32;
   sum : float64;
   TStr,NormStr : shortstring;
   zees : array[1..12] of float32;
begin
    {$IfDef RecordTimeGridCreate} Stopwatch1 := TStopwatch.StartNew; {$EndIf}
    if (Normalize = nmNorthSouth) then NormStr := '_norm_NS'
    else if (Normalize = nmEastWest) then NormStr := '_norm_EW'
    else if (Normalize = nmInterpolate) then NormStr := '_norm_interpolate'
    else if (Normalize = nm30m) then NormStr := '_norm_30m'
    else if (Normalize = nmTRIK) then NormStr := 'K'
    else NormStr := '_no_norm';

    if (Normalize = nmRRI) then TStr := 'RRI'
    else TStr := 'TRI_' + NormStr;
    if (OutName = '') then OutName := 'MD_' + TStr + '_' + DEMGlb[CurDem].AreaName;

    Result := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM, OutName,euMeters);

    GridLimits := TheDesiredLimits(CurDEM);
    FactorNS := 1;
    FactorEW := 1;
    FactorDiag := 1;

    if ShowSatProgress then StartProgressAbortOption(TStr + ' ' + DEMGlb[CurDEM].AreaName);

    //you could set the normalization factors here, using the average diagonal spacing
    //it would not be much faster, and the approximation would be increasingly in error for geographic DEMs as the latitude or size of the DEM increased
       (*
       if (Normalize = nmNorthSouth) then begin
          FactorNS := 1;
          FactorEW := DEMGlb[CurDEM].AverageYSpace / DEMGlb[CurDEM].AverageXSpace;
          FactorDiag := DEMGlb[CurDEM].AverageYSpace / DEMGlb[CurDEM].AverageDiaSpace;
       end;
       if (Normalize = nmEastWest) then begin
          FactorNS := DEMGlb[CurDEM].AverageXSpace / DEMGlb[CurDEM].AverageYSpace;
          FactorEW := 1;
          FactorDiag := DEMGlb[CurDEM].AverageXSpace / DEMGlb[CurDEM].AverageDiaSpace;
       end;
       if (Normalize = nmInterpolate) then begin
          //you should get the idea
       end;
       *)
    for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
       if (Row mod 25 = 0) and ShowSatProgress then UpdateProgressBar((Row-GridLimits.YGridLow) / (GridLimits.YGridHigh-GridLimits.YGridLow));
       if not (Normalize in [nmTRIK,nmRRI]) then GetSpacingMultiples(CurDEM,Row,Normalize,FactorNS,FactorEW,FactorDiag);

       for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          if DEMGLB[CurDEM].SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
             if (Normalize in [nmTRIK,nmRRI]) then begin
                InterpolateElevs(CurDEM,Col,Row,0.707,znw,zne,zsw,zse);
                if InterpolateElevs(CurDEM,Col,Row,1.414,z1,z21,z5,z25) and
                   DEMGLB[CurDEM].GetElevMetersOnGrid(Col,Row+2,z11) and
                   DEMGLB[CurDEM].GetElevMetersOnGrid(Col-2,Row,z3) and
                   DEMGLB[CurDEM].GetElevMetersOnGrid(Col+2,Row,z23) and
                   DEMGLB[CurDEM].GetElevMetersOnGrid(Col,Row-2,z15) then begin
                      //outer ring, 5x5 window
                      zees[1] := abs(-z1 + 2 * zNW - z); //point to NW, and its two neighbors
                      zees[2] := abs(-z11 + 2 * zN - z); //point to N, and its two neighbors
                      zees[3] := abs(-z21 + 2 * zNE - z); //point to NE, and its two neighbors
                      zees[4] := abs(-z23 + 2 * zE - z);  //point to E, and its two neighbors
                      zees[5] := abs(-z25 + 2 * zSE - z); //point to SE, and its two neighbors
                      zees[6] := abs(-z15 + 2 * zS - z);  //point to S, and its two neighbors
                      zees[7] := abs(-z5 + 2 * zSW - z);  //point to SW, and its two neighbors
                      zees[8] := abs(-z3 + 2 * zW - z);   //point to W, and its two neighbors
                      //inner ring, 3x3 window
                      zees[9] := abs(-zNW + 2 * z - zSE); //central point, and its two neighbors to NW-SE
                      zees[10] := abs(-zN + 2 * z - zS);  //central point, and its two neighbors to N-S
                      zees[11] := abs(-zNE + 2 * z - zSW); //central point, and its two neighbors to NE-SW
                      zees[12] := abs(-zE + 2 * z - zW);   //central point, and its two neighbors to W-E
                      Median := Petmath.Median(zees,12);
                      DEMGlb[Result].SetGridElevation(Col,Row,Median);
                      (*

                      Sum := abs(-z1 + 2 * zNW - z) * FactorDiag +      //zNW=z7,z=z13
                             abs(-z11 + 2 * zN - z) * FactorNS +        //zN=z12
                             abs(-z21 + 2 * zNE - z) * FactorDiag +     //zNE=z17
                             abs(-z23 + 2 * zE - z) * FactorEW +        //zE=z18
                             abs(-z25 + 2 * zSE - z) * FactorDiag +     //zSE=z19
                             abs(-z15 + 2 * zS - z) * FactorNS +        //zS=z14
                             abs(-z5 + 2 * zSW - z) * FactorDiag +      //zSW=z9
                             abs(-z3 + 2 * zW - z) * FactorEW +         //zW=z8
                             abs(-zNW + 2 * z - zSE) * FactorDiag +     //zNW=z7,zSE=z19
                             abs(-zN + 2 * z - zS) * FactorNS +         //zN=z12,zS=z14
                             abs(-zNE + 2 * z - zSW) * FactorDiag +     //zNE=z17,zSW=z9
                             abs(-zE + 2 * z - zW) * FactorEW;          //zE=z18,zW=z8
                      DEMGlb[Result].SetGridElevation(Col,Row,sum/12);
                      *)
                end;
             end
             else begin
                if (Normalize in [nmNorthSouth,nmEastWest,nm30m]) then begin
                   //calculate elevation in direction of the neighbor, assuming linear slope in that direction
                   znw := z + (zNW-z) * FactorDiag;
                   zne := z + (zNE-z) * FactorDiag;
                   zsw := z + (zSW-z) * FactorDiag;
                   zse := z + (zSE-z) * FactorDiag;
                   ze := z + (ze-z) * FactorEW;
                   zw := z + (zw-z) * FactorEW;
                   zn := z + (zn-z) * FactorNS;
                   zs := z + (zs-z) * FactorNS;
                end;
                if (Normalize = nmInterpolate) then begin
                   //this is precise only for UTM grids, but it's so close to the others it's not worth adjusting for geo grids
                   //DEMGLB[CurDEM].GetElevMeters(Col-0.707,Row+0.707,znw);
                   //DEMGLB[CurDEM].GetElevMeters(Col+0.707,Row+0.707,zne);
                   //DEMGLB[CurDEM].GetElevMeters(Col-0.707,Row-0.707,zsw);
                   //DEMGLB[CurDEM].GetElevMeters(Col+0.707,Row-0.707,zse);
                   InterpolateElevs(CurDEM,Col,Row,0.707,znw,zne,zsw,zse);
                end;

                Sum := sqr(z-zn) + sqr(z-zne) + sqr(z-ze) + sqr(z-zse)+ sqr(z-zs) + sqr(z-zsw) + sqr(z-zw) + sqr(z-znw);
                sum := sqrt(sum/8);
                DEMGlb[Result].SetGridElevation(Col,Row,sum);
             end;
          end;
       end;
    end;
    if ShowSatProgress then EndProgress;
    DEMGlb[Result].CheckMaxMinElev;
    if OpenMap then begin
       DEMGlb[Result].SetupMap(false,mtElevSpectrum);
    end;
    {$IfDef RecordTimeGridCreate} WriteLineToDebugFile('Make TRIGrid took ' + RealToString(Stopwatch1.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
end;


function MakeMAD2KGrid(CurDEM : integer; OpenMap : boolean = true) : integer;
var
   Col,Row : integer;
   GridLimits : tGridLimits;
   fSW,fSE,fNE,fNW : float32;
   znw,zw,zsw,zn,z,zs,zne,ze,zse,z1,z11,z21,z3,z23,z5,z15,z25,{FactorEW,FactorDiag,FactorNS,}Median : float32;
   sum : float64;
   TStr,NormStr : shortstring;
   zees : array[1..8] of float32;
begin
    {$IfDef RecordTimeGridCreate} Stopwatch1 := TStopwatch.StartNew; {$EndIf}
    Result := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM, 'MD_' + 'MAD2K' + '_' + DEMGlb[CurDem].AreaName,euMeters);
    GridLimits := TheDesiredLimits(CurDEM);
    //FactorNS := 1;
    //FactorEW := 1;
    //FactorDiag := 1;

    {$IfDef RecordProblems}
       DEMGLB[CurDEM].GetBilinearWeights(0.707, 0.707, fSW,fSE,fNE,fNW);
       WriteLineToDebugFile('Interpolation weights');
       WriteLineToDebugFile(RealToString(fNW,8,3) + RealToString(fNE,8,3));
       WriteLineToDebugFile(RealToString(fSW,8,3) + RealToString(fSE,8,3));
    {$EndIf}


    if ShowSatProgress then StartProgressAbortOption(TStr + ' ' + DEMGlb[CurDEM].AreaName);
    for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
       if (Row mod 25 = 0) and ShowSatProgress then UpdateProgressBar((Row-GridLimits.YGridLow) / (GridLimits.YGridHigh-GridLimits.YGridLow));

       for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          if DEMGLB[CurDEM].SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
             InterpolateElevs(CurDEM,Col,Row,0.707,znw,zne,zsw,zse);
             zees[1] := abs(zNW - z); // * FactorDiag;
             zees[2] := abs(zN - z); // * FactorNS;
             zees[3] := abs(zE - z); // * FactorEW;
             zees[4] := abs(zNE - z); // * FactorDiag;
             zees[5] := abs(zSW - z); // * FactorDiag;
             zees[6] := abs(zS - z); // * FactorNS;
             zees[7] := abs(zW - z); // * FactorEW;
             zees[8] := abs(zSE - z); // * FactorDiag;
             Median := 0.5 * Petmath.Median(zees,8);
             (*
             zees[1] := abs(zNW - zSE) * FactorDiag;
             zees[2] := abs(zN - zS) * FactorNS;
             zees[3] := abs(zE - zW) * FactorEW;
             zees[4] := abs(zNE - zSW) * FactorDiag;
             Median := 0.25* Petmath.Median(zees,4);
             *)
             DEMGlb[Result].SetGridElevation(Col,Row,Median);
          end;
       end;
    end;
    if ShowSatProgress then EndProgress;
    DEMGlb[Result].CheckMaxMinElev;
    if OpenMap then begin
       DEMGlb[Result].SetupMap(false,mtElevSpectrum);
    end;
    {$IfDef RecordTimeGridCreate} WriteLineToDebugFile('Make TRIGrid took ' + RealToString(Stopwatch1.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
end;


procedure MakeGammaGrids(CurDEM,BoxSize : integer);
const
  gn : array[1..4] of shortstring = ('EW','NS','NESW','NWSE');
var
   NewDEM : array[1..4] of integer;
   z : array[1..4] of float32;
   i,x,y : integer;
   GridLimits : tGridLimits;
begin
   StartProgress('Gamma grids');
   for i := 1 to 4 do
      NewDEM[i] := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM,DEMGlb[CurDEM].AreaName +'_gamma_'+gn[i],DEMGlb[CurDEM].DEMheader.ElevUnits);

   for x := 0 to pred(DEMGlb[CurDEM].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[CurDEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[CurDEM].DEMheader.NumRow) do begin
         GridLimits.XGridLow := x - Boxsize;
         GridLimits.XGridHigh := x + Boxsize;
         GridLimits.YGridLow := y - Boxsize;
         GridLimits.YGridHigh := y + Boxsize;
         DEMGlb[CurDEM].VariogramGamma(GridLimits,z[1],z[2],z[3],z[4]);
         for i := 1 to 4 do begin
            DEMGlb[NewDEM[i]].SetGridElevation(x,y,z[i]);
         end;
      end;
   end;
   EndProgress;
   for i := 1 to 4 do
      DEMGlb[NewDEM[i]].SetUpMap(true,mtElevSpectrum);
end;


{$IfDef ExGeostats}
{$Else}


procedure MomentsGridStrip(CompLimits : tGridLimits; What : char; GridInc,XBoxGridSize,YBoxGridSize,CurDEM : integer; DEMs : tListOfDEMs);
var
   x,y : integer;
   Limits : tGridLimits;
   SlopeDeg,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve,
   Upward,Downward : float64;
   MaxBox,MaxDir,
   MaxOrg,ElevStdDev,PCLower, zr,zsummit,zbase,Dropoff,GeoRelief,AvgElev,Elev_relief,Relief,TPI : float32;
   MomentVar : tMomentVar;
   Findings : tStringList;
   SlopeAspectRec : tSlopeAspectRec;
   SSOvars : tSSOvars;

         procedure PostResults(DEM,x,y,Gridinc : integer; zr : float32); inline;
         begin
            if ValidDEM(DEM) then DEMGlb[DEM].SetGridElevation(x div GridInc,y div GridInc,zr);
         end;

begin
    Findings := nil;
    y := CompLimits.YGridLow;
    while y <= CompLimits.YGridHigh do begin
       TInterlocked.Increment(CountInStrips);
       if (CountInStrips mod 50 = 0) and ShowSatProgress then UpdateProgressBar(CountInStrips*GridInc/DEMGlb[CurDEM].DEMheader.NumRow);
       Limits.YGridLow := (y - YBoxGridSize div 2);
       Limits.YGridHigh := (y + YBoxGridSize div 2);
       x := CompLimits.XGridLow;
       while x <= CompLimits.XGridHigh do begin
          Limits.XGridLow := (x - XBoxGridSize div 2);
          Limits.XGridHigh := (x + XBoxGridSize div 2);

          if (What = 'S') then  begin
               if DEMGlb[CurDEM].GetSlopeAndAspect(x,y,SlopeAspectRec) then begin
                 PostResults(DEMs[1],x,y,GridInc,SlopeAspectRec.SlopePercent);
                 PostResults(DEMs[2],x,y,GridInc,SlopeAspectRec.SlopeDegree);
                 PostResults(DEMs[3],x,y,GridInc,SinDeg(SlopeAspectRec.SlopeDegree));
                 PostResults(DEMs[4],x,y,GridInc,log10(TanDeg(SlopeAspectRec.SlopeDegree)));
                 PostResults(DEMs[5],x,y,GridInc,sqrt(SinDeg(SlopeAspectRec.SlopeDegree)));
                 PostResults(DEMs[9],x,y,GridInc,ln(TanDeg(SlopeAspectRec.SlopeDegree)));
                 PostResults(DEMs[10],x,y,GridInc,SinDeg(SlopeAspectRec.SlopeDegree)/CosDeg(SlopeAspectRec.SlopeDegree));
                 PostResults(DEMs[11],x,y,GridInc,SlopeAspectRec.dzdy);
                 PostResults(DEMs[12],x,y,GridInc,SlopeAspectRec.dzdx);

                 if (SlopeAspectRec.AspectDirTrue < 365) then begin
                    PostResults(DEMs[6],x,y,GridInc,round(SlopeAspectRec.AspectDirTrue));
                    PostResults(DEMs[7],x,y,GridInc,sinDeg(SlopeAspectRec.AspectDirTrue));
                    PostResults(DEMs[8],x,y,GridInc,cosDeg(SlopeAspectRec.AspectDirTrue));
                 end;
              end;
          end
          else if (What = 'G') then begin
             if DEMGlb[CurDEM].QuickRelief(x,y,Limits,zr,zsummit,zbase,GeoRelief,Dropoff,elev_relief) then begin
               PostResults(DEMs[1],x,y,Gridinc,zr);
               PostResults(DEMs[2],x,y,Gridinc,zsummit);
               PostResults(DEMs[3],x,y,Gridinc,zbase);
               PostResults(DEMs[4],x,y,Gridinc,Georelief);
               PostResults(DEMs[5],x,y,Gridinc,Dropoff);
               PostResults(DEMs[6],x,y,Gridinc,elev_relief);
             end;
          end
          else if (What = 'Q') then  begin
             if DEMGlb[CurDEM].SSOByRegionSize(x,y,MaxOrg,MaxBox,MaxDir,Relief,Findings) then begin
                PostResults(DEMs[1],x,y,GridInc,MaxBox);
                PostResults(DEMs[2],x,y,GridInc,MaxOrg);
                PostResults(DEMs[3],x,y,GridInc,MaxDir);
                PostResults(DEMs[4],x,y,GridInc,Relief);
             end;
          end
          {$IfDef MultipleCurvatureMethods}
             else if (What = 'A') then  begin
                if DEMGlb[CurDEM].GetCurvature(x,y,SlopeCurvature,PlanCurvature) then  begin
                   PostResults(DEMs[1],x,y,Gridinc,SlopeCurvature);
                   PostResults(DEMs[2],x,y,Gridinc,PlanCurvature);
                end;
             end
             else if (What = 'C') then  begin
                if DEMGlb[CurDEM].GetEvansParams(x,y,MDDef.WoodRegionRadiusPixels,SlopeDeg,SlopeCurvature,PlanCurvature,CrossC,MaxCurve,MinCurve) then  begin
                   PostResults(DEMs[1],x,y,Gridinc,SlopeCurvature);
                   PostResults(DEMs[2],x,y,Gridinc,PlanCurvature);
                   PostResults(DEMs[3],x,y,Gridinc,CrossC);
                   PostResults(DEMs[4],x,y,Gridinc,MinCurve);
                   PostResults(DEMs[5],x,y,GridInc,MaxCurve);
                end;
             end
          {$EndIf}
          else if (What = 'F') then begin
           // if DEMGlb[CurDEM].SimplePointSSOComputations(false,x,y,MDDef.SSOBoxSizeMeters, s1s2,s2s3,Trend,rf) then begin
           // function tDEMDataSet.SimplePointSSOComputations(PlotResults : boolean; Col,Row,FullBoxSizeMeters : integer; var s1s2,s2s3,Trend,RoughnessFactor : float64) : boolean;

           if DEMGlb[CurDEM].PointSSOComputations(x,y,MDDef.SSOBoxSizeMeters,SSOvars,false,false,false) then begin

(*
         var
         begin
            {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.SimplePointSSOComputations in: ' + IntToStr(Col) + ' & ' + IntToStr(Row) + ' rad=' + IntToStr(FullBoxSize)); {$EndIf}
            Result := PointSSOComputations(Col,Row,FullBoxSizeMeters,SSOvars,PlotResults,false,false);
            Trend := SSOvars.TheDipDirs[3];
            s1s2 := SSOvars.s1s2;
            s2s3 := SSOvars.s2s3;
            RoughnessFactor := SSOvars.RoughnessFactor;
*)
                PostResults(DEMs[1],x,y,GridInc,SSOvars.s2s3);
                if (DEMs[2] <> 0) then begin
                   zr := SSOvars.TheDipDirs[3];
                   if (zr > 180) then zr := zr - 180;
                   if (zr > 90) then zr := zr - 90;
                   PostResults(DEMs[2],x,y,GridInc,zr);
                end;
                PostResults(DEMs[3],x,y,GridInc,SSOvars.s1s2);
                PostResults(DEMs[4],x,y,GridInc,SSOvars.RoughnessFactor);
                PostResults(DEMs[6],x,y,GridInc,SSOvars.TheDipDirs[3]);
                if (DEMs[5] <> 0) then begin
                   zr := SSOvars.TheDipDirs[3];
                   if (zr > 180) then zr := zr - 180;
                   PostResults(DEMs[5],x,y,GridInc,zr);
                end;
                PostResults(DEMs[7],x,y,GridInc,1 - SSOVars.AspectStrength);
            end;
          end
          (*
          else if (What = 'O') then begin
             if DEMGlb[CurDEM].FigureOpenness(x,y,MDDef.OpenBoxSizeMeters,Upward,Downward) then begin
                PostResults(DEMs[1],x,y,GridInc,Upward);
                PostResults(DEMs[2],x,y,GridInc,Downward);
                PostResults(DEMs[3],x,y,GridInc,Upward-Downward);
             end;
          end
          *)
          else if (What = 'R') then begin
             if DEMGlb[CurDem].GetRelief(x,y,MDDef.ReliefBoxSizeMeters,AvgElev,Relief,ElevStdDev,PCLower,TPI) then begin
                PostResults(DEMs[1],x,y,GridInc,Relief);
                PostResults(DEMs[2],x,y,GridInc,AvgElev);
                PostResults(DEMs[3],x,y,GridInc,ElevStdDev);
                PostResults(DEMs[4],x,y,GridInc,PCLower);
                PostResults(DEMs[5],x,y,GridInc,TPI);
             end;
          end
          else begin
             if What = 'e' then MomentVar := DEMGlb[CurDEM].ElevationMoments(Limits)
             {$IfDef MultipleCurvatureMethods}
                else if What = 'l' then DEMGlb[CurDEM].PlanCMoments(Limits,MomentVar)
                else if What = 'r' then DEMGlb[CurDEM].ProfCMoments(Limits,MomentVar)
             {$EndIf}
             else if What = 's' then DEMGlb[CurDEM].SlopeMoments(Limits,MomentVar);
             if (MomentVar.NPts > 3) then  begin
                PostResults(DEMs[1],x,y,GridInc,MomentVar.mean);
                PostResults(DEMs[2],x,y,GridInc,MomentVar.std_dev);
                PostResults(DEMs[3],x,y,GridInc,MomentVar.skew);
                PostResults(DEMs[4],x,y,GridInc,MomentVar.curt);
             end;
          end;
          inc(x,GridInc);
          if WantOut then break;
       end;
       inc(y,GridInc);
       if WantOut then break;
    end;
end;


function MakeMomentsGrid(CurDEM : integer; What : char; BoxSizeRadiusMeters : integer = -99; OpenMaps : boolean = true) : integer;
var
   XBoxGridSize,YBoxGridSize,ThinFactor,i : integer;
   TStr : ShortString;
   WantMapType : tMapType;
   fName,pName : PathStr;
   {$If Defined(NoParallelFor) or Defined(NoParallelMoments)} {$Else} PartLimits : tGridLimits;  {$EndIf}

       procedure NewGrid(var DEM : integer; Gridname : shortstring; ElevUnits : tElevUnit);
       begin
          Petmar.ReplaceCharacter(GridName,' ','_');
          if (ThinFactor > 1) then DEM := DEMGlb[CurDEM].ThinAndOpenGridSetMissing(ThinFactor,FloatingPointDEM,'md_' + GridName + '_' + DEMGlb[CurDEM].AreaName,ElevUnits)
          else DEM := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'md_' + GridName + '_' + DEMGlb[CurDEM].AreaName,ElevUnits);
          {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('Created DEM ' + IntToStr(DEM) + GridName + ' proj=' + DEMGlb[DEM].DEMMapProj.ProjDebugName); {$EndIf}
       end;


begin {MakeMomentsGrid}
   {$If Defined(CreateGeomorphMaps) or Defined(CreateAspectMap)} WriteLineToDebugFile('MakeMomentsGrid in, dem=' + IntToStr(CurDEM) + '  what=' + what); {$EndIf}
   Result := 0;
   DEMGlb[CurDEM].GetBoxGridSizeDiameter(BoxSizeRadiusMeters,XBoxGridSize,YBoxGridSize,TStr);

   WantMapType := mtElevSpectrum;
   ThinFactor := 1;

   for i := 1 to MaxGrids do MomentDEMs[i] := 0;
   if (What = 'G') then begin
      ThinFactor := MDDef.ReliefCalcThin;
      if MDDef.DoRelief2 then NewGrid(MomentDEMs[1], 'Relief_m' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoSummit  then NewGrid(MomentDEMs[2], 'Summit_level_m' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoBaseLevel then NewGrid(MomentDEMs[3], 'Erosion_base_level_m' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoGeophysical then NewGrid(MomentDEMs[4], 'Geophysical_relief_m' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoDropoff then NewGrid(MomentDEMs[5], 'Dropoff_m' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoElevRelief then NewGrid(MomentDEMs[6], 'Elev_relief' + TStr,euUndefined);
   end
   else if (What = 'R') then  begin
      ThinFactor := MDDef.ReliefCalcThin;
      if MDDef.DoRelief1 then NewGrid(MomentDEMs[1], 'Relief_m' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoAvgElev then NewGrid(MomentDEMs[2], 'Average_Elev' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoElevStd then NewGrid(MomentDEMs[3], 'Std_Dev_Elev' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoREL then NewGrid(MomentDEMs[4], 'REL' + TStr,euUndefined);
      if MDDef.DoTPI then NewGrid(MomentDEMs[5], 'TPI' + TStr,euUndefined);
   end
   {$IfDef MultipleCurvatureMethods}
   else if (What = 'A') then begin
      TStr := '_curvature--' + CurvatureMethodName[MDDef.CurvatureMethod] +' (' + IntToStr(MDDef.CurvRegionSize) + ')';
      NewGrid(DEMs[1], '_slope' + TStr,euUndefined);
      NewGrid(DEMs[2], '_plan' + TStr,euUndefined);
   end
   else if (What = 'C') then  begin
      if MDDef.DoSlopeCurve then NewGrid(MomentDEMs[1], 'Slope_curvature',euUndefined);
      if MDDef.DoPlanCurve then NewGrid(MomentDEMs[2], 'Plan_curvature',euUndefined);
      if MDDef.DoCrossCurve then NewGrid(MomentDEMs[3], 'Cross_sectional_curvature',euUndefined);
      if MDDef.DoMinCurve then NewGrid(MomentDEMs[4], 'Min_curvature',euUndefined);
      if MDDef.DoMaxCurve then NewGrid(MomentDEMs[5], 'Max_curvature',euUndefined);
   end
   {$EndIf}
   else if (What = 'O') then begin
      WantMapType := mtElevGray;
      ThinFactor := MDDef.OpennessCalcThin;
      if MDDef.DoUpOpen then NewGrid(MomentDEMs[1], 'Upward_openness' + TStr,euDegrees);
      if MDDef.DoDownOpen then NewGrid(MomentDEMs[2], 'Downward_openness' + TStr,euDegrees);
      if MDDef.DoDiffOpen then NewGrid(MomentDEMs[3], 'Difference_openness' + TStr,euDegrees);
   end
   else if (What in ['F']) then begin
      if MDDef.DoS2S3 then NewGrid(MomentDEMs[1], 'Organization_Strength' + Tstr,euUndefined);
      if MDDef.DoFabDir90 then NewGrid(MomentDEMs[2], 'Organization_Direction_90' + TStr,euDegrees);
      if MDDef.DoS1S2 then NewGrid(MomentDEMs[3], 'Flatness_Strength' + TStr,euUndefined);
      if MDDef.DoRoughness then NewGrid(MomentDEMs[4], 'Roughness' + TStr,euUndefined);
      if MDDef.DoFabDir180 then NewGrid(MomentDEMs[5], 'Organization_Direction_180' + TStr,euDegrees);
      if MDDef.DoFabDir360 then NewGrid(MomentDEMs[6], 'Organization_Direction_360' + TStr,euDegrees);
      if MDDEF.DoAvgVectStrength then NewGrid(MomentDEMs[7], 'avg_aspect_strength' + TStr,euUndefined);
      Result := MomentDEMs[4];
   end
   else if (What = 'Q') then  begin
      ThinFactor := MDDef.FabricCalcThin;
      NewGrid(MomentDEMs[1],'Most organized region (m)' + TStr, DEMGlb[CurDEM].DEMheader.ElevUnits);
      NewGrid(MomentDEMs[2],'Largest S2S3 ' + TStr, euUndefined);
      NewGrid(MomentDEMs[3],'Organization Direction 360 ' + TStr,euDegrees);
      NewGrid(MomentDEMs[4],'Relief (m) ' + TStr, DEMGlb[CurDEM].DEMheader.ElevUnits);
   end
   else if (What = 'S') then begin
       if MDDef.DoSlopePC then begin
          NewGrid(MomentDEMs[1], ShortSlopeMethodName(MDDef.SlopeAlgorithm) +'_Slope_percent',euPercentSlope);
          WantMapType := MDDef.DefSlopeMap;
       end;
       if MDDef.DoSlopeDeg then NewGrid(MomentDEMs[2],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_Slope_degree)',euDegreeSlope);
       if MDDef.DoSlopeSin then NewGrid(MomentDEMs[3],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_Sin_slope',euUndefined);
       if MDDef.DoSlopeLogTan then NewGrid(MomentDEMs[4],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_log_tan_slope',euUndefined);
       if MDDef.DoSlopeSqrtSin then NewGrid(MomentDEMs[5],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_sqrt_sin_slope',euUndefined);
       if MDDef.DoAspect then NewGrid(MomentDEMs[6],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_Aspect_degree',euAspectDeg);
       if MDDef.DoAspectNS then NewGrid(MomentDEMs[7],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_Aspect_NS',euUndefined);
       if MDDef.DoAspectEW then NewGrid(MomentDEMs[8],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_Aspect_EW',euUndefined);
       if MDDef.DoSlopeLnTan then NewGrid(MomentDEMs[9],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_ln_tan_slope',euUndefined);
       if MDDef.DoSlopeMperM then NewGrid(MomentDEMs[10],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_m_per_m',euMperM);
       if MDDef.DoNSSlope then NewGrid(MomentDEMs[11],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_NS_comp',euPercentSlope);
       if MDDef.DoEWSlope then NewGrid(MomentDEMs[12],ShortSlopeMethodName(MDDef.SlopeAlgorithm) + '_EW_comp',euPercentSlope);
   end
   else begin
      ThinFactor := MDDef.MomentCalcThin;
      if What = 'e' then TStr := 'elev_' + TStr
      else if What = 's' then TStr := 'slope_' + TStr
      else if What = 'l' then TStr := 'plan_curv_' + TStr
      else if What = 'r' then TStr := 'prof_curv_'+ TStr;
      TStr := TStr + '_';

      if MDDef.DoMean then NewGrid(MomentDEMs[1],'avg' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoSTD then NewGrid(MomentDEMs[2],'stddev' + Tstr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoSkew then NewGrid(MomentDEMs[3],'skew' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoKurt then NewGrid(MomentDEMs[4],'kurt' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
   end;

    if ShowSatProgress then StartProgressAbortOption('Multiple geomorphometry (' + What + ')');
    {$If Defined(CreateGeomorphMaps) or Defined(CreateAspectMap)} WriteLineToDebugFile('MakeMomentsGrid compute'); for i := 1 to 12 do if ValidDEM(MomentDEMs[i]) then WriteLineToDebugFile(IntToStr(i) + '  ' + DEMGlb[MomentDEMs[i]].AreaName); {$EndIf}

    CountInStrips := 0;
    {$IfDef RecordTimeGridCreate} Stopwatch1 := TStopwatch.StartNew; {$EndIf}

   {$If Defined(NoParallelFor) or Defined(NoParallelMoments)}
       {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('No parallel moments');    {$EndIf}
       MomentsGridStrip(DEMGlb[CurDEM].FullDEMGridLimits,What,ThinFactor,XBoxGridSize,YBoxGridSize,CurDEM,MomentDEMs);
   {$Else}
      {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('Moments strips='+IntToStr(MDdef.MaxThreadsForPC)); {$EndIf}
      PartLimits := GetLimitsForParallelLoops(DEMGlb[CurDEM].FullDEMGridLimits);
      TParallel.For(1,MDdef.MaxThreadsForPC,
         procedure (Value: Integer)
         begin
            MomentsGridStrip(PartLimits[Value],What,ThinFactor,XBoxGridSize,YBoxGridSize,CurDEM,MomentDEMs);
         end);
      ThreadsWorking := false;
   {$EndIf}

    {$IfDef RecordTimeGridCreate} WriteLineToDebugFile('MakeMomentsGrid took ' + RealToString(Stopwatch1.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
    if ShowSatProgress then EndProgress;

    {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('Open maps'); {$EndIf}
    for i := 1 to MaxGrids do begin
       if ValidDEM(MomentDEMs[i]) then begin
          DEMGlb[MomentDEMs[i]].CheckMaxMinElev;
          {$IfDef CreateGeomorphMaps} WriteLineToDebugFile(DEMglb[MomentDEMs[i]].AreaName + '  ' + DEMglb[MomentDEMs[i]].zRange); {$EndIf}
          if OpenMaps then begin
             {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('Create map for DEM ' + IntToStr(MomentDEMs[i]) + ' ' + DEMGlb[MomentDEMs[i]].KeyDEMParams); {$EndIf}
             DEMGlb[MomentDEMs[i]].SetUpMap(false,WantMapType,What in ['A','C','F']);
             MatchAnotherDEMMap(MomentDEMs[i],CurDEM);
             {$IfDef CreateGeomorphMaps} DEMGlb[CurDEM].SelectionMap.MapDraw.PrimMapProj.ProjectionParamsToDebugFile('Original grid',true); {$EndIf}
             {$IfDef CreateGeomorphMaps} DEMGlb[MomentDEMs[i]].SelectionMap.MapDraw.PrimMapProj.ProjectionParamsToDebugFile('New grid',true); {$EndIf}
          end;
       end;
    end;

   if (Result = 0) then begin
       for i := 1 to 12 do if ValidDEM(MomentDEMs[i]) then begin
          Result := MomentDEMs[i];
          break;
       end;
   end;

    if MDDef.AutoSaveGeomorphGrids then begin
       {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('Autosave'); {$EndIf}
       pName := ExtractFilePath(DEMGlb[CurDEM].DEMFileName) + 'geomorph_grids\';
       SafeMakeDir(pName);
       for i := 1 to MaxGrids do if ValidDEM(MomentDEMs[i]) then begin
          Fname := pName + DEMGlb[MomentDEMs[i]].AreaName + '.dem';
          DEMGlb[MomentDEMs[i]].WriteNewFormatDEM(fName);
       end;
    end;
    {$If Defined(CreateGeomorphMaps) or Defined(CreateAspectMap)}  WriteLineToDebugFile('MakeMomentsGrid out, Result=' + IntToStr(Result)); {$EndIf}
end {MakeMomentsGrid};

{$EndIf}


function DerivativeMapName(ch : AnsiChar; SampleBoxSize : integer = 0) : ShortString;
begin
   case ch of
      '-' : Result := 'Minimum curvature';
      '+' : Result := 'Maximum curvature';
      '1' : Result := 'Profile convexity';
      '2' : Result := 'Plan convexity';
      '3' : Result := 'Relief (' + IntToStr(SampleBoxSize) + ' m)';
      '4' : Result := 'Summit level (m) (' + IntToStr(SampleBoxSize) + ' m)';
      '5' : Result := 'Erosion base level (m) (' + IntToStr(SampleBoxSize) + ' m)';
      '6' : Result := 'Organization (s2s3)(L=' + IntToStr(SampleBoxSize) + ')';
      '7' : Result := 'Fabric direction (L=' + IntToStr(SampleBoxSize) + ')';
      '8' : Result := 'Flatness (L=' + IntToStr(SampleBoxSize) + ')';
      '9' : Result := 'Roughness factor';
      'A' : Result := 'Aspect';
      'B' : Result := 'Building';
      'C' : Result := 'Cross sectional curvature';
      'c' : Result := 'Convergence index';
      'd' : Result := 'Neighborhood drop (' + IntToStr(SampleBoxSize) + ' m)';
      'E' : Result := 'Ln trans';
      'F' : Result := 'Planar fit';
      'G' : Result := 'Geophysical Relief (' + IntToStr(SampleBoxSize) + ' m)';
      'g' : Result := 'Rugosity';
      'i' : Result := 'Immediate dropoff';
      'L' : Result := 'LogTrans';
      'M' : Result := 'Meter elevations';
      'N' : Result := 'NS Aspect';
      'n' : Result := 'Normalized grid';
      'o' : Result := 'Slope (°)';
      'P' : Result := 'Percentile';
      'p' : Result := 'Point density';
      'r' : Result := 'Ridges';
      's' : Result := 'Sin of slope';
      'S' : Result := 'Slope (%)';
      'T' : Result := 'Terrain';
      'V','v' : Result := 'Viewshed';
      'W' : Result := 'EW Aspect';
      'X' : Result := 'log tan slope';
      'Y' : Result := 'sqrt sin slope';
      'Z' : Result := 'ln tan slope';
   end;
end;

function ShortDerivativeMapName(ch : AnsiChar; SampleBoxSize : integer = 0) : ShortString;
begin
   case ch of
      '-' : Result := 'MIN_CURVE';
      '+' : Result := 'MAX_CURVE';
      '1' : Result := 'PROF_CONV';
      '2' : Result := 'PLAN_CONV';
      '3' : Result := 'Relief_' + IntToStr(SampleBoxSize);
      '4' : Result := 'SUMMIT_LEV';
      '5' : Result := 'BASE_LEVEL';
      '6' : Result := 'S2S3_' + IntToStr(SampleBoxSize);
      '7' : Result := 'FAB_DIR_' + IntToStr(SampleBoxSize);
      '8' : Result := 'FLAT_' + IntToStr(SampleBoxSize);
      '9' : Result := 'ROUGH_FAC';
      'A' : Result := 'Aspect';
      'B' : Result := 'Building';
      'C' : Result := 'XS_CURVE';
      'c' : Result := 'CONV_INDX';
      'D' : Result := 'DN_OP_' + IntToStr(SampleBoxSize);
      'd' : Result := 'DROP_' + IntToStr(SampleBoxSize);
      'E' : Result := 'Ln_trans';
      'F' : Result := 'Plane_fit';
      'G' : Result := 'GEO_RELF' + IntToStr(SampleBoxSize);
      'g' : Result := 'Rugosity';
      'i' : Result := 'Immed_drop';
      'L' : Result := 'LogTrans';
      'M' : Result := 'Meter_elev';
      'N' : Result := 'NS_ASPECT';
      'n' : Result := 'Norm_grid';
      'o' : Result := 'Slope_deg';
      'O' : Result := 'UP_OP_' + IntToStr(SampleBoxSize);
      'P' : Result := 'Percentile';
      'p' : Result := 'Pt_density';
      'r' : Result := 'RIDGES';
      's' : Result := 'Sin_slope';
      'S' : Result := 'SLOPE_PC';
      'T' : Result := 'Terrain';
      'V','v' : Result := 'Viewshed';
      'W' : Result := 'EW_ASPECT';
      'X' : Result := 'l_tan_slope';
      'Y' : Result := 's_sin_slope';
      'Z' : Result := 'ln_t_slope';
   end;
end;


function CreateProfileConvexityMap(WhichDEM : integer; OpenMap : boolean = true) : integer;
begin
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateProfileConvexityMap in'); {$EndIf}
   SaveBackupDefaults;
   SetAllCurvatures(false);
   MDDef.DoSlopeCurve := true;
   Result := MakeMomentsGrid(WhichDEM,'C',-99,OpenMap);
   RestoreBackupDefaults;
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateProfileConvexityMap, InGrid=' + IntToStr(WhichDEM) + '  NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProj.ProjDebugName); {$EndIf}
end;


function CreateSlopeMap(WhichDEM : integer; OpenMap : boolean = true; Components : boolean = false) : integer;
begin
   {$If Defined(CreateGeomorphMaps)} WriteLineToDebugFile('CreateSlopeMap in'); {$EndIf}
   SaveBackupDefaults;
   SetAllSlopes(false);
   MDDef.DoSlopePC := true;
   MDDef.DoNSSlope := Components;
   MDDef.DoEWSlope := Components;
   Result := MakeMomentsGrid(WhichDEM,'S',-99,OpenMap);
   RestoreBackupDefaults;
   {$If Defined(CreateSlopeMap)} WriteLineToDebugFile('CreateSlopeMap=' + IntToStr(Result) + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].zRange); {$EndIf}
   {$IfDef  Defined(CreateGeomorphMaps)} WriteLineToDebugFile('CreateSlopeMap, InGrid=' + IntToStr(WhichDEM) + '  NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProjection.ProjDebugName); {$EndIf}
end;


function CreateAnOrganizationMap(WhichDEM : integer; OpenMap : boolean = true) : integer;
{$IfDef ExGeoStats}
begin
{$Else}
begin
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateAnOrganizationMap in'); {$EndIf}
    Result := MakeMomentsGrid(WhichDEM,'F',MDDef.SSOBoxSizeMeters,OpenMap);
{$EndIf}
end;


function MakeSingleNewDerivativeMap(ch : AnsiChar; CurDEM : integer = 0; SampleBoxSize : integer = 0; ShowMap : boolean = true) : integer;
{$IfDef ExGeostats}
begin
{$Else}
var
   znw,zw,zsw,zn,zs,zne,ze,zse,z,zr,Sum : float32;
   MomentVar : tMomentVar;
   CumDone,TotalNumDone,
   NumDone,Col,Row : integer;
   GridLimits :  tGridLimits;
   mt : tMapType;
   TerrainCat : tTerrainCatDefinition;
   Bitmap : tMyBitmap;
   AllMissing,UsePC,MadeAChange : boolean;
   NewHeadRecs : tDEMheader;
   fName,OutName : PathStr;


          function AddtoCI(Col,row : integer; Inward : float64) : boolean;
          var
             Angle : float64;
             SlopeAspectRec : tSlopeAspectRec;
          begin
             Result := DEMGlb[CurDEM].GetSlopeAndAspect(Col,Row,SlopeAspectRec) and (SlopeAspectRec.AspectDirTrue < 32000);
             if Result then begin
                Angle := abs(SlopeAspectRec.AspectDirTrue - Inward);
                if (Angle > 180) then Angle := Angle - 180;
                Sum := Sum +  Angle;
             end;
          end;

begin
   {$IfDef RecordMakeNewMapProblems} WriteLineToDebugFile('MakeANewMap ' + DerivativeMapName(ch,SampleBoxSize)); {$EndIf}
   if not ValidDEM(CurDEM) then GetDEM(CurDEM);
   Result := 0;
   if (ch = 'r') then begin
      {$IfDef ExGeostats}
      {$Else}
         Result := CreateRidgeMap(CurDEM,DEMGlb[CurDEM].FullDEMGridLimits, rtmRidge);
      {$EndIf}
   end
   else begin
     Bitmap := Nil;
     OutName := '';

     NewHeadRecs := DEMGlb[CurDEM].DEMheader;
     if (ch = 'B') then NewHeadRecs.DEMPrecision := ByteDEM
     else NewHeadRecs.DEMPrecision := FloatingPointDEM;

     MDDef.StatSampleIncr := 1;
     AllMissing := ch in ['g'];
     if OpenAndZeroNewDEM(true,NewHeadRecs,Result,'',InitDEMMissing,0) then begin
         DEMGlb[CurDEM].ReflectanceParams;
         if (SampleBoxSize = 0) and (ch in ['B']) then begin
            GetSampleBoxSize(CurDEM,MDDef.GeomorphBoxSizeMeters);
            SampleBoxSize := MDDef.GeomorphBoxSizeMeters;
         end;
         if (ch = 'i') then SampleBoxSize := round(2 * DEMGlb[CurDEM].AverageSpace);
         if (ch = 'B') then begin
            ReadDefault('Minimum height (m)',MDDef.BuildingMinHeight);
            ReadDefault('Sampling interval ',MDDef.StatSampleIncr);
         end;

         GridLimits := TheDesiredLimits(CurDEM);

         if (ch = 'C') then begin
            MDDef.WoodRegionRadiusPixels := round(SampleBoxSize / DEMGlb[CurDEM].AverageSpace);
            if (MDDef.WoodRegionRadiusPixels  < 1) then MDDef.WoodRegionRadiusPixels := 1;
         end;

         if (ch in ['1','2','o','s','+','-']) then MDDef.WoodRegionRadiusPixels := 1;

         if (ch = 'E') then DEMGlb[Result].DEMheader.ElevUnits := euLnElev
         else if (ch = 'L') then DEMGlb[Result].DEMheader.ElevUnits := euLogElev
         else if (ch in ['R','B','n','C','+','-','P','p','1','2','s','N','W','X','Y','Z']) then DEMGlb[Result].DEMheader.ElevUnits := euUndefined
         else if (ch = 'S') then DEMGlb[Result].DEMheader.ElevUnits := euPercentSlope
         else if (ch in ['H','M','g','i']) then DEMGlb[Result].DEMheader.ElevUnits := euMeters
         else if (ch in ['A','o','c']) then DEMGlb[Result].DEMheader.ElevUnits := euDegrees
         else if (ch = 'T') then  begin
            DEMGlb[CurDEM].InitializeTerrainCategory(TerrainCat);
            GetTerrainCategory(tcNormal,DEMGlb[CurDEM].SelectionMap,CurDEM,TerrainCat,DEMGlb[Result].ElevationDEM);
            DEMGlb[Result].AreaName := 'Terrain';
         end;

         if (ch = 'n') then begin
            MomentVar := DEMGlb[CurDEM].ElevationMoments(DEMGlb[CurDEM].FullDEMGridLimits);
         end;
         if (ch = 'p') then MDdef.UseSealevel := true;

         DEMGlb[Result].ShortName := ShortDerivativeMapName(ch,SampleBoxSize);
         DEMGlb[Result].AreaName := DEMGlb[CurDEM].AreaName + '_' + DerivativeMapName(ch,SampleBoxSize);
         if ch = 'g' then DEMGlb[Result].AreaName := 'md_rugosity_(m per '+ RealToString(DEMGlb[Result].AverageSpace,-8,-1) + 'm)_' +  DEMGlb[Result].AreaName;

         DEMGlb[Result].DefineDEMVariables(true);

         if ShowSatProgress then StartProgressAbortOption(DEMGlb[Result].AreaName);
         TotalNumDone := 0;
         CumDone := 0;
         Col := GridLimits.XGridLow;
         while (Col <= GridLimits.XGridHigh) do begin
            if ShowSatProgress then UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
            NumDone := 0;
            MadeAChange := false;
            Row := GridLimits.YGridLow;
            while (Row <= GridLimits.YGridHigh) do begin
               inc(NumDone);
               inc(TotalNumDone);
               inc(CumDone);
               MadeAChange := true;
               if DEMGlb[CurDEM].GetElevMeters(Col,Row,z) and ( (ch in ['n']) or ((abs(z) > 0.01) or MDdef.UseSealevel)) then begin
                  if (ch in ['E','L']) then  begin
                      if DEMGlb[CurDEM].GetElevMeters(Col,Row,zr) and (zr > 0) then begin
                         if (ch = 'E') then DEMGlb[Result].SetGridElevation(Col,Row,ln(zr))
                         else DEMGlb[Result].SetGridElevation(Col,Row,log10(zr));
                      end;
                  end
                  else if (ch in ['P']) then begin
                      if DEMGlb[CurDEM].GetElevMeters(Col,Row,zr) then
                         DEMGlb[result].SetGridElevation(Col,Row,DEMGlb[CurDEM].PercentileOfElevation(zr));
                  end
                  else if (ch in ['p']) then begin
                      if DEMGlb[CurDEM].GetElevMeters(Col,Row,zr) then
                         DEMGlb[result].SetGridElevation(Col,Row,zr/DEMGlb[Result].XSpaceByDEMrow^[Row]/DEMGlb[Result].AverageYSpace);
                  end
                  else if (ch in ['M']) then begin
                      if DEMGlb[CurDEM].GetElevMeters(Col,Row,zr) then
                         DEMGlb[result].SetGridElevation(Col,Row,zr);
                  end
                  else if (ch in ['n']) then begin
                      if DEMGlb[CurDEM].GetElevMeters(Col,Row,zr) then begin
                         DEMGlb[result].SetGridElevation(Col,Row,(zr-MomentVar.mean)/MomentVar.std_dev);
                      end;
                  end
                  else if (ch in ['c']) then begin
                      Sum := 0;
                      //we should adjust angles for arc second data
                      if AddtoCI(pred(Col),succ(row),135) and AddtoCI(pred(Col),succ(row),90) and AddtoCI(succ(Col),succ(row),235) and
                         AddtoCI(pred(Col),row,90) and  AddtoCI(succ(Col),row,270) and
                         AddtoCI(pred(Col),pred(row),45) and AddtoCI(pred(Col),pred(row),0) and AddtoCI(succ(Col),pred(row),315) then
                              DEMGlb[result].SetGridElevation(Col,Row,sum/8 - 90);
                  end
                  else if DEMGlb[CurDEM].IsSurroundedPoint(Col,Row) then  begin
                     if (ch = 'T') then begin
                        if DEMGlb[CurDEM].InTerrainCategory(Col,Row,TerrainCat) and DEMGlb[CurDEM].GetElevMeters(Col,Row,z) then
                           DEMGlb[Result].SetGridElevation(Col,Row,z);
                     end
                     else if (ch in ['B']) then begin
                        if DEMGlb[CurDEM].PointHasSpecifiedRelief(Col,Row,SampleBoxSize,MDDef.StatSampleIncr,MDDef.BuildingMinHeight) then begin
                           DEMGlb[Result].SetGridElevation(Col,Row,1);
                        end;
                     end
                     else if (ch in ['g']) then begin
                        DEMGlb[CurDEM].GetNineElevMeters(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse);
                        z := (abs(znw-z) + abs(zn-z) + abs(zne-z) + abs(zw-z) + abs(ze-z) + abs(zsw-z) + abs(zs-z) + abs(zsw-z)) / 8;
                        DEMGlb[Result].SetGridElevation(Col,Row,z);
                     end;
                  end;
               end;
               inc(Row,MDDef.StatSampleIncr);
            end;
            if (OutName <> '') and MadeAChange and (TotalNumDone > 5000) then begin
               DEMGlb[Result].WriteNewFormatDEM(OutName);
               if (DEMGlb[Result].SelectionMap <> Nil) then DEMGlb[Result].SelectionMap.DoBaseMapRedraw;
               TotalNumDone := 0;
            end;
            if WantOut then break;
            inc(Col,MDDef.StatSampleIncr);
         end;
         if ShowSatProgress then EndProgress;

         {$IfDef RecordMakeNewMapProblems} WriteLineToDebugFile('  compute over'); {$EndIf}
         DEMGlb[Result].CheckMaxMinElev;
         if ShowMap then begin
             UsePC := false;
             if (ch in ['R']) then begin
                mt := mtElevGray;
             end
             else if (ch in ['B','E','L','A','C','c','i','V','P','p','+','-','1','2','n','o','s','S','g','Z','X','Y']) then  begin
                mt := mtElevSpectrum;
                UsePC := true;
             end
             else mt := MDdef.DefElevMap;

             DEMGlb[Result].SetupMap(false,mt,UsePC);

             if DEMGlb[CurDEM].SelectionMap.FullMapSpeedButton.Enabled and MDDef.GeomorphMapsFullDEM then begin
                DEMGlb[Result].SelectionMap.MapDraw.NoDrawingNow := true;
                DEMGlb[Result].SelectionMap.RedrawMapForDataGrid(GridLimits.XGridLow,GridLimits.YGridHigh,GridLimits.XGridHigh,GridLimits.YGridLow,
                DEMGlb[CurDEM].SelectionMap.MapDraw.MapXSize,DEMGlb[CurDEM].SelectionMap.MapDraw.MapYSize);
                DEMGlb[Result].SelectionMap.MapDraw.NoDrawingNow := false;
                DEMGlb[Result].SelectionMap.N11view1Click(Nil);
             end;

             DEMGlb[Result].SelectionMap.SaveDEM1.Visible := false;
             DEMGlb[Result].SelectionMap.CheckProperTix;
         end;

         ShowSatProgress := true;
         fName := '';
         if (OutName <> '') then DEMGlb[Result].WriteNewFormatDEM(OutName)
         else if MDdef.PromptToSaveNewDEMs then DEMGlb[Result].WriteNewFormatDEM(fName);
    end;
   end;
   if (DEMGlb[CurDEM].SelectionMap <> Nil) then DEMGlb[CurDEM].SelectionMap.CheckProperTix;
   ShowDEMReadingProgress := true;
   {$IfDef RecordMakeNewMapProblems} WriteLineToDebugFile('MakeANewMap out'); {$EndIf}
{$EndIf}
end;


procedure AspectDifferenceMapStrip(WhichDEM,ResultDEM,RegionRadius : integer; GridLimits : tGridLimits);
var
   i,j : integer;
   z1,z2,z3,z4,znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;

   function ProcessPair(z1,z2 : float64) : float32;  inline;
   begin
      MinOfPairFirst(z1,z2);
      Result := z2-z1;
      while (Result > 180) do Result := Result-180;
   end;


begin
    for j := GridLimits.YGridLow to GridLimits.YGridHigh do begin
       TInterlocked.Increment(CountInStrips);
       if (CountInStrips mod 50 = 0) and ShowSatProgress then UpdateProgressBar(CountInStrips/DEMGlb[WhichDEM].DEMheader.NumRow);
       for i := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          if DEMGlb[WhichDEM].SurroundedPointElevs(i,j,znw,zw,zsw,zn,z,zs,zne,ze,zse,RegionRadius) then begin
             z1 := ProcessPair(zn,zs);
             z2 := ProcessPair(ze,zw);
             z3 := ProcessPair(zne,zsw);
             z4 := ProcessPair(znw,zse);
             z4 := MaxFloat(z1,z2,z3,z4);
             DEMGlb[ResultDEM].SetGridElevation(i,j,round(z4));
          end;
       end;
    end;
end;


function AspectDifferenceMap(WhichDEM,RegionRadius : integer; GridLimits : tGridLimits) : integer;
{$IfDef ExGeology}
begin
{$Else}
   {$IfDef NoParallelFor}
   {$Else}
    var
      PartLimits : tGridLimitsArray;
      ResultDEM : integer;
   {$EndIf}
begin
   {$IfDef RecordPointClass} WriteLineToDebugFile('CreateAspectDifferenceMap in'); {$EndIf}
     Result := DEMGlb[WhichDEM].CloneAndOpenGridSetMissing(ByteDEM,'Aspect_difference_' + IntToStr(succ(2*RegionRadius)) + 'x' + IntToStr(succ(2*RegionRadius)) +
          '_' + DEMGlb[WhichDem].AreaName,euDegrees);
     {$IfDef RecordPointClass} WriteLineToDebugFile('New grid created'); {$EndIf}
     StartProgress('Aspect difference');
     CountInStrips := 0;
    {$IfDef NoParallelFor}
        AspectDifferenceMapStrip(WhichDEM,Result,RegionRadius,GridLimits);
    {$Else}
        PartLimits := GetLimitsForParallelLoops(GridLimits);
        ResultDEM := Result;
        TParallel.For(1,MDdef.MaxThreadsForPC,
           procedure (Value: Integer)
           begin
             AspectDifferenceMapStrip(WhichDEM,ResultDEM,RegionRadius,PartLimits[Value]);
           end);
        ThreadsWorking := false;
    {$EndIf}
    EndProgress;

     {$IfDef RecordPointClass} WriteLineToDebugFile('New map created'); {$EndIf}
    DEMGlb[Result].CheckMaxMinElev;
    DEMGlb[Result].SetupMap(false,mtElevSpectrum);
    {$IfDef RecordPointClass} WriteLineToDebugFile('CreateAspectDifferenceMa out'); {$EndIf}

{$EndIf}
end;


procedure RidgeMapStrip(WhichDEM,ResultDEM : integer; GridLimits : tGridLimits; RidgeTypeMap : tRidgeTypemap);
var
   i,j : integer;
   z : float32;
   PointType : tPointType;
begin
    for j := GridLimits.YGridLow to GridLimits.YGridHigh do begin
       TInterlocked.Increment(CountInStrips);
       if (CountInStrips mod 50 = 0) and ShowSatProgress then UpdateProgressBar(CountInStrips/DEMGlb[WhichDEM].DEMheader.NumRow);
       for i := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          {if (MDDef.RidgePointClassify = raWood) then DEMGlb[WhichDEM].WoodPointClassify(i,j,PointType)
          else} PointType := DEMGlb[WhichDEM].ClassifyAPoint(i,j);
          if (RidgeTypeMap = rtmRidge) and (PointType in [RidgePoint,PeakPoint]) then begin
             DEMGlb[WhichDEM].GetElevMeters(i,j,z);
             DEMGlb[ResultDEM].SetGridElevation(i,j,z);
          end;
          if (RidgeTypeMap = rtmStream) and (PointType in [PitPoint,ValleyPoint]) then begin
             DEMGlb[WhichDEM].GetElevMeters(i,j,z);
             DEMGlb[ResultDEM].SetGridElevation(i,j,z);
          end;
          if (RidgeTypeMap = rtmAllPoints) then begin
             DEMGlb[ResultDEM].SetGridElevation(i,j,ord(PointType));
          end;
       end;
    end;
end;


function CreateRidgeMap(WhichDEM : integer; GridLimits : tGridLimits;  RidgeTypeMap : tRidgeTypemap; Memo1 : tMemo = Nil) : integer;
var
   ResultDEM,i,j,NPts,NRidge,NValley  : integer;
   TStr : ShortString;
   VAT : tStringList;
   z  : float32;
   ElevUnits : tElevUnit;
   Pt,PointType : tPointType;
   DEMPrecision : tDEMprecision;
   ClassNPts : tDEMPointClassIntArray;
   ClassPC   : tDEMPointClassFloatArray;
   {$IfDef NoParallelFor}
   {$Else}
      PartLimits : tGridLimitsArray;
   {$EndIf}
begin
   {$IfDef RecordPointClass} WriteLineToDebugFile('CreateRidgeMap in'); {$EndIf}
   VAT := Nil;
    case RidgeTypeMap of
       rtmRidge : TStr := 'Ridges ' + RidgeAlgorithmName;
       rtmStream : TStr := 'Valleys';
       rtmAllPoints :  TStr := 'All_point_classification';
    end;

    if (RidgeTypeMap = rtmAllPoints) then begin
       DEMPrecision := ByteDEM;
       ElevUnits := euIntCode;
    end
    else begin
       DEMPrecision := FloatingPointDEM;
       ElevUnits := euMeters;
    end;

     for Pt := EdgePoint to OtherPoint do ClassNPts[Pt] := 0;
     NPts := 0;
     NRidge := 0;
     NValley := 0;

     Result := DEMGlb[WhichDEM].CloneAndOpenGridSetMissing(DEMPrecision,DEMGlb[WhichDem].AreaName + '_' + TStr,ElevUnits);

     {$IfDef RecordPointClass} WriteLineToDebugFile('New grid created'); {$EndIf}

    if ShowSatProgress then StartProgressAbortOption(Tstr);

    CountInStrips := 0;
    {$IfDef NoParallelFor}
       RidgeMapStrip(WhichDEM,Result,GridLimits,RidgeTypeMap);
    {$Else}
      PartLimits := GetLimitsForParallelLoops(GridLimits);
      ResultDEM := Result;
      TParallel.For(1,MDdef.MaxThreadsForPC,
         procedure (Value: Integer)
         begin
           RidgeMapStrip(WhichDEM,ResultDEM,PartLimits[Value],RidgeTypeMap);
         end);
      ThreadsWorking := false;
    {$EndIf}
    if ShowSatProgress then EndProgress;

     {$IfDef RecordPointClass} WriteLineToDebugFile('New map created');{$EndIf}

    if (RidgeTypeMap = rtmAllPoints) then begin
       for i := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          for j := GridLimits.YGridLow to GridLimits.YGridHigh do begin
              if DEMGlb[Result].GetElevMeters(i,j,z) then inc(ClassNPts[tPointType(round(z))]);
          end;
       end;
       for PointType := EdgePoint to PassPoint do NPts := ClassNPts[PointType] + NPts;
       if (RidgeTypeMap = rtmAllPoints) then begin
          for PointType := EdgePoint to PassPoint do ClassPC[PointType] := 100 * ClassNPts[PointType] / NPts;
       end;
      {$IfDef RecordPointClass} WriteLineToDebugFile('Histogram created'); {$EndIf}
    end;

    if (Memo1 <> Nil) then begin
        Memo1.Lines.Add('Total points=' + IntToStr(NPTs));
        Memo1.Lines.Add('');
        if (RidgeTypeMap = rtmRidge) then begin
           Memo1.Lines.Add('Ridge points=' + IntToStr(NRidge));
        end;
        if (RidgeTypeMap = rtmStream) then begin
           Memo1.Lines.Add('Valley points=' + IntToStr(NValley));
        end;

        if (RidgeTypeMap = rtmAllPoints) then begin
            if (Npts > 0) then begin
                for PointType := EdgePoint to PassPoint do begin
                   Memo1.Lines.Add(RealToString(ClassPC[PointType],8,3) + '%    ' + PointTypeName(PointType));
                end;
                Memo1.Lines.Add('');
                Memo1.Lines.Add('   n=' + IntToStr(NPts));
            end;
        end;
        Memo1.Lines.Add('');
        Memo1.Lines.Add('');
       {$IfDef RecordPointClass} WriteLineToDebugFile('Memo filled in'); {$EndIf}
    end;

    {$IfDef RecordPointClass} WriteLineToDebugFile('Elev range done'); {$EndIf}

    if MDDef.MaskMapShow in [0,1,2] then begin
       if (RidgeTypeMap = rtmAllPoints) then begin
         VAT := tStringList.Create;
         VAT.Add('N,PERCENT,VALUE,NAME,COLOR');
         for Pt := EdgePoint to OtherPoint do begin
            VAT.Add(IntToStr(ClassNPts[pt]) + ',' + RealToString(ClassPC[pt],-18,2) + ',' + IntToStr(ord(PT)) + ',' + PointTypeName(pt) + ',' + IntToStr(PointTypeColor(pt)));
         end;
         DEMGlb[Result].VATFileName := NextFileNumber(MDTempDir,'class_map_1','.dbf');
         PetDBUtils.StringList2CSVtoDB(VAT,DEMGlb[Result].VATFileName,true);
         DEMGlb[Result].SetupMap(true,mtDEMVATTable);
         {$IfDef RecordPointClass} WriteLineToDebugFile('Call base map redraw'); {$EndIf}
          DEMGlb[Result].Selectionmap.DoBaseMapRedraw;
       end;
    end;
    {$IfDef RecordPointClass} WriteLineToDebugFile('Ridge map out'); {$EndIf}
end;


function CreateIwashishiPikeMap(NewName : ShortString; BaseGrid,SlopeGrid,RoughGrid,ConvexGrid : integer) : integer;
var
   i,y,x,TotPts,Cat : integer;
   VAT : tStringList;
   Slope,Convex,Rough : float32;
   PartLimits : tGridLimits;
   ClassNPts : array[1..16] of int32;
   ClassPC : array[1..16] of float64;
begin
   {$IfDef RecordPointClass} WriteLineToDebugFile('CreateIwashishiPikeMap in'); {$EndIf}
   Result := DEMGlb[SlopeGrid].CloneAndOpenGridSetMissing(ByteDEM,NewName + '_IandPClass',euIntCode);
   DEMGlb[Result].AreaName := NewName + '_IandPClass';

   {$IfDef RecordPointClass} WriteLineToDebugFile('New grid created'); {$EndIf}
   if ShowSatProgress then StartProgressAbortOption('I&P class');
   CountInStrips := 0;
   ConvexityMeanCut := DEMGLB[ConvexGrid].FindPercentileElev(MDDef.ConvexCut);
   TextureMeanCut := DEMGLB[RoughGrid].FindPercentileElev(MDDef.RoughnessCut);
   SlopeMeanCut := DEMGLB[SlopeGrid].FindPercentileElev(MDDef.SlopeCut1);
   SlopeQuarterCut := DEMGLB[SlopeGrid].FindPercentileElev(MDDef.SlopeCut2);
   SlopeEigthCut := DEMGLB[SlopeGrid].FindPercentileElev(MDDef.SlopeCut3);

   PartLimits := DEMGlb[SlopeGrid].FullDEMGridLimits;

   StartProgress('Classify');
   for i := 1 to MDDef.IwashPikeCats do ClassNPts[i] := 0;
   for y := PartLimits.YGridLow to PartLimits.YGridHigh do begin
      if (CountInStrips mod 50 = 0) and ShowSatProgress then UpdateProgressBar(y/DEMGlb[SlopeGrid].DEMheader.NumRow);
      for x := PartLimits.XGridLow to PartLimits.XGridHigh do begin
         if DEMGlb[SlopeGrid].GetElevMeters(x,y,Slope) then begin
            if DEMGlb[ConvexGrid].GetElevMeters(x,y,Convex) then begin
              if DEMGlb[RoughGrid].GetElevMeters(x,y,Rough) then begin
                  Cat := DEMStat.IwashuriPikeCat(Slope,Convex,Rough);
                  DEMGlb[Result].SetGridElevation(x,y,Cat);
                  inc(ClassNPts[Cat]);
               end;
            end;
         end;
      end;
   end;
   DEMGlb[Result].CheckMaxMinElev;
   EndProgress;

   TotPts := 0;
   for i := 1 to MDDef.IwashPikeCats do TotPts := TotPts + ClassNPts[i];
   for i := 1 to MDDef.IwashPikeCats do ClassPC[i] := 100 * ClassNPts[i]/TotPts;

   VAT := tStringList.Create;
   VAT.Add('N,PERCENT,VALUE,COLOR');
   for i := 1 to MDDef.IwashPikeCats do begin
      VAT.Add( IntToStr(ClassNPts[i]) + ',' + RealToString(ClassPC[i],-18,2) + ','  + IntToStr(i) + ',' + IntToStr(IPColor[i]) );
   end;

    DEMGlb[Result].VATFileName := MDTempDir + DEMGlb[Result].AreaName + '.vat.dbf';
    PetDBUtils.StringList2CSVtoDB(VAT,DEMGlb[Result].VATFileName,true);

    {$IfDef RecordPointClass} WriteLineToDebugFile('Call base map redraw'); {$EndIf}
    if DEMGlb[Result].Selectionmap = Nil then CreateDEMSelectionMap(Result,true,false,mtDEMVATTable)
    else DEMGlb[Result].Selectionmap.DoBaseMapRedraw;
    IandPLegend(ClassPC);
    if MDDef.MaskMapShow in [1,2] then DEMGlb[BaseGrid].SelectionMap.GridpointsfromsecondDEMAssignAndDraw(Result);
end;


function MakeNEneighborGrid(CurDEM : integer; Normalize : tNormalMethod; OpenMap : boolean = true; OutName : PathStr = '') : integer;
var
   Col,Row : integer;
   GridLimits : tGridLimits;
   znw,zw,zsw,zn,z,zs,zne,ze,zse,FactorDiag,FactorNS,FactorEW : float32;
   TStr,NormStr : shortstring;
begin
    {$IfDef RecordTimeGridCreate} Stopwatch1 := TStopwatch.StartNew; {$EndIf}
    if (OutName = '') then begin
       if (Normalize = nmNorthSouth) then NormStr := '_norm_NS'
       else if (Normalize = nmEastWest) then NormStr := '_norm_EW'
       else if (Normalize = nm30m) then NormStr := '_norm_30m'
       else if (Normalize = nmInterpolate) then NormStr := '_norm_interpolate'
       else if (Normalize = nmNone) then NormStr := '_no_norm';
       OutName := 'MD_' + TStr + '_' + DEMGlb[CurDem].AreaName;
    end;

    Result := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM, OutName,euMeters);

    GridLimits := TheDesiredLimits(CurDEM);
    FactorDiag := 1;

    if ShowSatProgress then StartProgressAbortOption(TStr + ' ' + DEMGlb[CurDEM].AreaName);

    for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
       if (Row mod 25 = 0) and ShowSatProgress then UpdateProgressBar((Row-GridLimits.YGridLow) / (GridLimits.YGridHigh-GridLimits.YGridLow));
       GetSpacingMultiples(CurDEM,Row,Normalize,FactorNS,FactorEW,FactorDiag);
       for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          if DEMGLB[CurDEM].SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
             if (Normalize in [nmNorthSouth,nmEastWest,nm30m]) then begin
                //calculate elevation in direction of the neighbor, assuming linear slope in that direction
                zne := z + (zNE-z) * FactorDiag;
             end
             else if (Normalize = nmInterpolate) then begin
                DEMGLB[CurDEM].GetElevMeters(Col+0.707,Row+0.707,zne)
             end;
             DEMGlb[Result].SetGridElevation(Col,Row,zne);
          end;
       end;
    end;
    if ShowSatProgress then EndProgress;
    DEMGlb[Result].CheckMaxMinElev;
    DEMGlb[Result].AreaName := ExtractFileNameNoExt(OutName);
    if OpenMap then begin
       DEMGlb[Result].SetupMap(false,mtElevSpectrum);
    end;
    {$IfDef RecordTimeGridCreate} WriteLineToDebugFile('Make TRIGrid took ' + RealToString(Stopwatch1.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
end;


initialization
finalization
end.
