unit Least_cost_path;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordFullPath}            {big slowdown}
   //{$Define RecordSetAccumPathPoints}  {big slowdown}
   //{$Define RecordBasicsAccumPath}
{$EndIf}

interface


uses
   Forms,SysUtils,Graphics,Controls,Vcl.ComCtrls,
   Petmar,Petmar_types,DEMMapf;


procedure AccumulatedCostSurface(Lat,Long : float64; CostSurfacefName,fName,fName3 : PathStr);
procedure FigureLeastCostPath(MapOwner : tMapForm; Lat,Long: float64; var PathDistance,PathCost : float64);
procedure OpenLeastCostGrids;
procedure LeastCostFromCurrentRecord(GISNum : integer);
procedure CreateCostPathSurface(StatusBar1 : tStatusBar);

function LCP_AccumCostfName(GISNum : integer) : PathStr;
function LCP_AccumDistancefName(GISNum : integer) : PathStr;

function LDP_AccumCostfName(GISNum : integer) : PathStr;
function LDP_AccumDistancefName(GISNum : integer) : PathStr;

function LCP_CostSurface : PathStr;
function LDP_CostSurface : PathStr;


var
   FinalAccumCostDEM,
   DownCostDEM : integer;
   LCPJustDistance : boolean;

implementation


uses
//need for inline of core DB functions
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
   DEMDefs,BaseMap,DEMCoord,DEM_Manager,DEMDatabase,
   Make_tables,Petmar_db,PetMath,PetDBUtils,PetImage;


function LCP_CostSurface : PathStr;
begin
   Result := ChangeFileExt(MDDef.LCPRoadfName,'_lcp_cost_surface.dem');
end;


function LDP_CostSurface : PathStr;
begin
   Result := ChangeFileExt(MDDef.LCPRoadfName,'_ldp_cost_surface.dem');
end;

(*
function MakeSafeFileName(fName : ANSIString) : ANSIstring;
var
  i: Integer;
begin
   Result := fName;
   for i := 1 to length(fName) do
      if not (fName[i] in ['0'..'9','a'..'z','A'..'Z']) then Result[i] := '_';
end;
*)

function LCP_AccumCostfName(GISNum : integer) : PathStr;
begin
   Result := '';
   if GISdb[GISnum].MyData.FieldExists(MDDef.PrecintField) then begin
      Result := ExtractFilePath(GISdb[GISnum].dbFullName) + GISdb[GISnum].MyData.GetFieldByNameAsString(MDDef.PrecintField) + '_lcp_accum_cost_surf.dem';
      CleanUpFileName(Result);
   end;
end;

function LCP_AccumDistancefName(GISNum : integer) : PathStr;
begin
   Result := '';
   if GISdb[GISnum].MyData.FieldExists(MDDef.PrecintField) then begin
      Result := ExtractFilePath(GISdb[GISnum].dbFullName) + GISdb[GISnum].MyData.GetFieldByNameAsString(MDDef.PrecintField) + '_lcp_accum_dist.dem';
      CleanUpFileName(Result);
   end;
end;

function LDP_AccumCostfName(GISNum : integer) : PathStr;
begin
   Result := '';
   if GISdb[GISnum].MyData.FieldExists(MDDef.PrecintField) then begin
      Result := ExtractFilePath(GISdb[GISnum].dbFullName) + GISdb[GISnum].MyData.GetFieldByNameAsString(MDDef.PrecintField) + '_ldp_accum_cost_surf.dem';
      CleanUpFileName(Result);
   end;
end;

function LDP_AccumDistancefName(GISNum : integer) : PathStr;
begin
   Result := '';
   if GISdb[GISnum].MyData.FieldExists(MDDef.PrecintField) then begin
      Result := ExtractFilePath(GISdb[GISnum].dbFullName) + GISdb[GISnum].MyData.GetFieldByNameAsString(MDDef.PrecintField) + '_ldp_accum_dist.dem';
      CleanUpFileName(Result);
   end;
end;


procedure CreateCostPathSurface(StatusBar1 : tStatusBar);
const
   CostCats = 10;
var
   NewDEM,BufferDEM,Buffer2DEM,NumCats,Cost,x,y,i,StartMap : integer;
   Colors : array[1..CostCats] of tPlatformColor;
   Costs,AllCostEqual : array[1..CostCats] of int32;
   z : float32;
   Table : Petmar_db.tMyData;
   BMPMemory : tBMPMemory;
   bmp : tMyBitmap;
   Color : tPlatFormColor;
   fName : PathStr;
begin
   {$IfDef RecordBasicsAccumPath} WritelineToDebugFile('Enter CreateCostPathSurface, ' + MDDef.LCPRoadfName); {$EndIf}
   StartMap := LoadBlankVectorMapAndOverlay(false,false,MDDef.LCPRoadfName);

   MDDef.ShiftMercToUTM := true;
   StatusBar1.Panels[0].Text := 'Enter CreateCostPathSurface';
   NewDEM := VectorMap[StartMap].CreateGridToMatchMap(cgUTM,true,SmallIntDEM,MDDef.CostSurfResolution);
   StatusBar1.Panels[0].Text := 'N11view1Click';
   DEMGlb[NewDEM].SelectionMap.N11view1Click(Nil);
   DEMGlb[NewDEM].SelectionMap.LoadDataBaseFile(MDDef.LCPRoadfName);
   StatusBar1.Panels[0].Text := 'DB Loaded';
   NumCats := 0;
   Table := Petmar_db.tMyData.Create(TigerShapeRules);
   while not Table.Eof do begin
      Cost := Table.GetFieldByNameAsInteger('COST');
      if (Cost > 0) and (Cost < 9999) then begin
         inc(NumCats);
         Colors[NumCats] := ConvertTColorToPlatformColor(Table.GetFieldByNameAsInteger('LINE_COLOR'));
         Costs[NumCats] := Cost;
         AllCostEqual[NumCats] := 1;
      end;
      Table.Next;
   end;
   Table.Destroy;

   CopyImageToBitmap(DEMGlb[NewDEM].SelectionMap.Image1,Bmp);
   StartProgress('Create cost surface');
   StatusBar1.Panels[0].Text := 'Create cost surface';
   BMPMemory := tBmpMemory.Create(bmp);
   for x := 0 to pred(Bmp.Width) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/bmp.Width);
      for y := 0 to pred(bmp.Height) do begin
         Color := BMPMemory.GetPixelColor(x,y);
         for i := 1 to NumCats do begin
            if BMPMemory.SameColor(x,y,Colors[i]) then begin
               DEMGlb[NewDEM].SetGridElevation(x,DEMGlb[NewDEM].DEMHeader.NumRow-y,Costs[i]);
               break;
            end;
         end;
      end;
   end;
   BMPMemory.Destroy;
   BMP.Destroy;

   {$IfDef RecordBasicsAccumPath} WritelineToDebugFile('Start buffer CreateCostPathSurface'); {$EndIf}
   StatusBar1.Panels[0].Text := 'Buffering';
   for I := 1 to MDDef.BufferRounds do begin
      if MDDef.LCP_LeastCost then BufferDEM := DEMGlb[NewDEM].CloneAndOpenGridSetMissing(SmallIntDEM,'Buffered',euUndefined);
      if MDDef.LCP_ShortestDistance then Buffer2DEM := DEMGlb[NewDEM].CloneAndOpenGridSetMissing(SmallIntDEM,'Buffered',euUndefined);
      StartProgress('Buffer cost surface');
      for x := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumCol) do begin
         if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[NewDEM].DEMHeader.NumCol);
         for y := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumRow) do begin
            if DEMGlb[NewDEM].GetElevMeters(x,y,z) then begin
               if MDDef.LCP_LeastCost then DEMGlb[BufferDEM].SetGridElevation(x,y,z);
               if MDDef.LCP_ShortestDistance then DEMGlb[Buffer2DEM].SetGridElevation(x,y,1);
            end
            else begin
               if DEMGlb[NewDEM].ValidNeighborsInBox(X,Y,1) > 0 then begin
                  if MDDef.LCP_LeastCost then DEMGlb[BufferDEM].SetGridElevation(x,y,MDDef.BufferCost);
                  if MDDef.LCP_ShortestDistance then DEMGlb[Buffer2DEM].SetGridElevation(x,y,MDDef.BufferCost);
               end;
            end;
         end;
      end;
   end;
   EndProgress;
   StatusBar1.Panels[0].Text := 'Call write new DEM';
   if MDDef.LCP_LeastCost then begin
      DEMGlb[BufferDEM].CheckMaxMinElev;
      fName := LCP_CostSurface;
      DEMGlb[BufferDEM].WriteNewFormatDEM(fName);
      CloseSingleDEM(BufferDEM);
   end;
   if MDDef.LCP_ShortestDistance then begin
      DEMGlb[Buffer2DEM].CheckMaxMinElev;
      fName := LDP_CostSurface;
      DEMGlb[Buffer2DEM].WriteNewFormatDEM(fName);
      CloseSingleDEM(Buffer2DEM);
   end;
   StatusBar1.Panels[0].Text := 'Close DEMs';
   CloseSingleDEM(NewDEM);
   CloseSingleVectorMap(StartMap);
   StatusBar1.Panels[0].Text := 'Exit CreateCostPathSurface';
  {$IfDef RecordBasicsAccumPath} WritelineToDebugFile('Exit CreateCostPathSurface'); {$EndIf}
end;


procedure OpenLeastCostGrids;
var
   fName : PathStr;
begin
   fName := '';
   LoadNewDEM(FinalAccumCostDEM,fName,false,'Accumulated cost surface grid for end point');
   fName := ChangeFileExt(fname,'-dir.dem');
   LoadNewDEM(DownCostDEM,fName,false,'Downhill direction grid');
end;


procedure FigureLeastCostPath(MapOwner : tMapForm; Lat,Long: float64; var PathDistance,PathCost : float64);
var
   MinCost,z : float32;
   Points,x,y,pts,i : integer;
   Diagonal : boolean;
   fName,fName2 : PathStr;
   Table : tMyData;
   Dir : tCompassDirection;
   Count : array[1..10000] of int64;


      procedure ProcessPoint;
      begin
         DEMGlb[FinalAccumCostDEM].GetElevMeters(X,Y,MinCost);
         DEMGlb[FinalAccumCostDEM].DEMGridToLatLongDegree(x,y,Lat,Long);
         if Pts > 0 then begin
            if Diagonal then PathDistance := PathDistance + DEMGlb[FinalAccumCostDEM].DiagSpaceByDEMrow[y]
            else PathDistance := PathDistance + DEMGlb[FinalAccumCostDEM].XSpaceByDEMrow[y];
            PathCost := PathCost + MinCost;
         end;
         inc(Pts);
         Table.Insert;
         Table.SetFieldByNameAsFloat('LAT',Lat);
         Table.SetFieldByNameAsFloat('LONG',Long);
         Table.SetFieldByNameAsFloat('Z',MinCost);
         inc(Count[round(MinCost)]);
         Table.Post;
      end;


begin
   fName2 := ChangeFileExt(DEMGlb[FinalAccumCostDEM].DEMFileName,'-dir.dem');
   if not FileExists(fname2) then begin
      MessageToContinue('Downhill direction file missing');
      exit;
   end;
   DEMGlb[FinalAccumCostDEM].LatLongDegreeToDEMGridInteger(Lat,Long,x,y);
   Pts := 0;
   fName := Petmar.NextFileNumber(ExtractFilePath(DEMGlb[FinalAccumCostDEM].DEMFileName), 'lcp_',DefaultDBExt);
   ShowHourglassCursor;
   Make_Tables.CreateLatLongZTable(fName);
   Table := tMyData.Create(fName);
   Dir :=  cdPit;
   Points := 0;
   PathDistance := 0;
   PathCost := 0;
   for I := 1 to 10000 do Count[i] := 0;

   ProcessPoint;
   repeat
      inc(Points);
      if (Dir <> cdFlat) and DEMGlb[DownCostDEM].GetElevMeters(x,y,z) then begin
         Dir := tCompassDirection(round(z));
         Diagonal := Dir in [cdSW,cdSE,cdNE,cdNW,cdNE];
         if Dir in [cdW,cdSW,cdNW] then dec(x);
         if Dir in [cdE,cdSE,cdNE] then inc(x);
         if Dir in [cdN,cdNW,cdNE] then inc(y);
         if Dir in [cdS,cdSE,cdSW] then dec(y);
         ProcessPoint;
      end;
   until (Dir = cdFlat) or (Points > 100000);

   WriteLineToDebugFile('');
   WriteLineToDebugFile(RealToString(Lat,-12,-6) + '  ' + RealToString(Long,-12,-6));
   DEMGlb[DownCostDEM].DEMGridToLatLongDegree(x,y,Lat,Long);
   WriteLineToDebugFile(RealToString(Lat,-12,-6) + '  ' + RealToString(Long,-12,-6));
   WriteLineToDebugFile('Path distance: ' + RealToString(PathDistance,-12,-2) + '  Path cost: ' + RealToString(PathCost,-12,-2));
   for I := 1 to 10000 do if (Count[i] > 0) then WriteLineToDebugFile(IntegerToString(i,8) + IntegerToString(Count[i],15));
   WriteLineToDebugFile('');

   Table.Destroy;
   if (MapOwner <> Nil) then MapOwner.LoadDataBaseFile(fName);
   ShowDefaultCursor;
end;


procedure LeastCostFromCurrentRecord(GISNum : integer);
var
   Lat,long : float64;
begin
   if GISdb[GISnum].ValidLatLongFromTable(Lat,Long) then begin
      if MDDef.LCP_LeastCost then AccumulatedCostSurface(Lat,Long,LCP_CostSurface,LCP_AccumCostfName(GISNum),LCP_AccumDistancefName(GISNum));
      if MDDef.LCP_ShortestDistance then AccumulatedCostSurface(Lat,Long,LDP_CostSurface,LDP_AccumCostfName(GISNum),LDP_AccumDistancefName(GISNum));
   end;
end;


procedure AccumulatedCostSurface(Lat,Long : float64; CostSurfacefName,fName,fName3 : PathStr);
const
   MaxInStack = 2500;
   MissingGridPoint = 1e39;
   SetGridPoint = 1e38;
   PointNotYetSet = 1e37;
type
   tStackItem = record
      xg,yg : int32;
      DownDir : tCompassDirection;
      Cost,Dist  : float64;
   end;
   tStack = array[1..MaxInStack] of tStackItem;
var
   CostStack : tStack;
   NumInStack : integer;
   NewHeadRecs : tDEMheader;
   NewMinFound : boolean;
   AccumCostDEM,
   CostPassageDEM,
   FinalDistanceDEM,
   AccumDistanceDEM,
   x,y,Found,
   Cycles : integer;
   z,z2,zf : float32;
   fName2 : PathStr;


          procedure CheckStack(var CostStack : tStack; x,y : integer; z,Dist : float64; Dir : tCompassDirection);   //inline;
          var
             StartBump,i : integer;
          begin
             if (NumInStack = 0) then begin
                NumInStack := 1;
                CostStack[1].xg := x;
                CostStack[1].yg := y;
                CostStack[1].Cost := z;
                CostStack[1].DownDir := Dir;
                CostStack[1].Dist := Dist;
             end
             else begin
                if (z < CostStack[NumInStack].Cost) then begin
                   StartBump := pred(NumInStack);
                   if (NumInStack > 1) then begin
                      while (StartBump > 0) and (z < CostStack[StartBump].Cost) do dec(StartBump);
                   end;
                   inc(StartBump);
                   if (NumInStack < MaxInStack) then inc(NumInStack);

                   for i := NumInStack downto succ(StartBump) do begin
                      CostStack[i] := CostStack[pred(i)];
                   end;
                   CostStack[StartBump].xg := x;
                   CostStack[StartBump].yg := y;
                   CostStack[StartBump].Cost := z;
                   CostStack[StartBump].DownDir := Dir;
                   CostStack[StartBump].Dist := Dist;
                end;
             end;
          end;

          procedure FastCheckNeighborCost(var CostStack : tStack; x,y : integer; z,Dist : float64; Dir : tCompassDirection);
          var
             z2,z3,dz : float32;
          begin
             if (DEMGlb[FinalAccumCostDEM].MissingDataInGrid(X,Y)) then begin
                if DEMGlb[AccumCostDEM].GetElevMeters(X,Y,z2) then begin
                   if (z2 < SetGridPoint) then begin
                      DEMGlb[CostPassageDEM].GetElevMeters(x,y,z3);
                      if MDDef.LCPdiagonals and (Dir in [cdNE,cdSE,cdNW,cdSW]) then dz := sqrt_2
                      else dz := 1;

                      z3 := z + dz * z3;
                      if (z3 < z2) then begin
                         DEMGlb[AccumCostDEM].SetGridElevation(X,Y,z3);
                         DEMGlb[DownCostDEM].SetGridElevation(X,Y,ord(Dir));
                         if MDDef.LCPdist then begin
                            Dist := Dist + dz * DEMGlb[AccumDistanceDEM].AverageSpace;
                         end
                         else begin
                            Dist := Dist + dz;
                         end;
                         Dist := round(Dist);
                         DEMGlb[AccumDistanceDEM].SetGridElevation(X,Y,Dist);
                         CheckStack(CostStack,x,y,z3,Dist,Dir);
                      end;
                   end;
                end;
             end;
          end;

          procedure SetPointAndFigureNeighbors(X,Y : integer; z,Dist : float64; Dir : tCompassDirection);
          var
             z2 : float32;
          begin
             if DEMGlb[AccumCostDEM].GridInDataSet(x,y) then begin
               if DEMGlb[AccumCostDEM].GetElevMetersOnGrid(x,y,z2) and (z2 < SetGridPoint) then begin
                  DEMGlb[FinalAccumCostDEM].SetGridElevation(X,Y,z);
                  DEMGlb[FinalDistanceDEM].SetGridElevation(X,Y,Dist);
                  DEMGlb[AccumCostDEM].SetGridElevation(X,Y,SetGridPoint);
                  DEMGlb[DownCostDEM].SetGridElevation(X,Y,ord(Dir));

                  //directions back
                  //SE   S   SW
                  //E   xxx  W
                  //NE   N   NW

                  FastCheckNeighborCost(CostStack,pred(x),y,z,Dist,cdE);
                  FastCheckNeighborCost(CostStack,pred(x),succ(y),z,Dist,cdSE);
                  FastCheckNeighborCost(CostStack,x,pred(y),z,Dist,cdN);
                  FastCheckNeighborCost(CostStack,pred(x),pred(y),z,Dist,cdNE);
                  FastCheckNeighborCost(CostStack,x,succ(y),z,Dist,cdS);
                  FastCheckNeighborCost(CostStack,succ(x),pred(y),z,Dist,cdNW);
                  FastCheckNeighborCost(CostStack,succ(x),y,z,Dist,cdW);
                  FastCheckNeighborCost(CostStack,succ(x),succ(y),z,Dist,cdSW);
                  inc(Found);
               end;
             end;
          end;

          procedure ClearStack(var CostStack : tStack);
          var
             i : integer;
          begin
             while (NumInStack > 0) do begin
                SetPointAndFigureNeighbors(CostStack[1].xg,CostStack[1].yg,CostStack[1].Cost,CostStack[1].Dist,CostStack[1].DownDir);
                for i := 1 to pred(NumInStack) do begin
                   CostStack[i] := CostStack[succ(i)];
                end;
                dec(NumInStack);
             end;
          end;

var
   Dir : tCompassDirection;
   DEMGridLimits :  tGridLimits;
   npts : int64;
begin
   {$IfDef RecordBasicsAccumPath} WritelineToDebugFile('Enter AccumulatedCostSurface, fName =' + fName); {$EndIf}

   LoadNewDEM(CostPassageDEM,CostSurfacefName,false,'Cost surface grid');

   if (fName = '') then begin
      fName := ChangeFileExt(DEMGlb[CostPassageDEM].DEMFileName,'-lcp.dem');
      Petmar.GetFileNameDefaultExt('Accumulated cost path','MICRODEM DEM|*.dem',fname);
   end;
   fName2 := ChangeFileExt(fname,'-dir.dem');
   DeleteFileIfExists(fName2);
   DeleteFileIfExists(fName3);

   DEMGlb[CostPassageDEM].LatLongDegreeToDEMGridInteger(Lat,Long,StartX,StartY);

   {$IfDef RecordBasicsAccumPath} WritelineToDebugFile('Start point neighborhood for ' + ExtractFileNameNoExt(fName)); {$EndIf}
   for x := StartX-MDDef.StartFree to StartX+MDDef.StartFree do begin
      for y := StartY-MDDef.StartFree to StartY+MDDef.StartFree do
         if DEMGlb[CostPassageDEM].GridInDataSet(x,y) then begin
            if DEMGlb[CostPassageDEM].MissingDataInGrid(x,y) then begin
               DEMGlb[CostPassageDEM].SetGridElevation(x,y,MDDef.BufferCost);
            end;
         end;
   end;
   DEMGlb[CostPassageDEM].SetGridElevation(StartX,StartY,0);

   DEMGlb[CostPassageDEM].MissingDataToConstantVelue(MDDef.ImpossibleCost);
   {$IfDef RecordBasicsAccumPath} WritelineToDebugFile('Grid prepped'); {$EndIf}

   {$IfDef RecordBasicsAccumPath} WritelineToDebugFile('Create new grids ' + fName); {$EndIf}

   NewHeadrecs := DEMGlb[CostPassageDEM].DEMheader;
   NewHeadRecs.ElevUnits := euUndefined;

   NewHeadRecs.DEMPrecision := ByteDEM;
   if not OpenAndZeroNewDEM(true,NewHeadRecs,DownCostDEM,'',InitDEMvalue,0) then exit;

   NewHeadRecs.DEMPrecision := FloatingPointDEM;
   if (not OpenAndZeroNewDEM(true,NewHeadRecs,AccumCostDEM,'',InitDEMvalue,0)) or  (not OpenAndZeroNewDEM(true,NewHeadRecs,FinalAccumCostDEM,'',InitDEMvalue,0)) or
      (not OpenAndZeroNewDEM(true,NewHeadRecs,AccumDistanceDEM,'',InitDEMvalue,0)) or (not OpenAndZeroNewDEM(true,NewHeadRecs,FinalDistanceDEM,'',InitDEMvalue,0)) then exit;

  //make sure all points have positive cost value--no free pixels
   for x := 0 to pred(DEMGlb[AccumCostDEM].DEMheader.NumCol) do begin
      for y := 0 to pred(DEMGlb[AccumCostDEM].DEMheader.NumRow) do begin
         DEMGlb[AccumDistanceDEM].SetGridElevation(X,Y,0);
         if DEMGlb[CostPassageDEM].GetElevMeters(x,y,z) then begin
            DEMGlb[AccumCostDEM].SetGridElevation(X,Y,PointNotYetSet);
         end
         else begin
            DEMGlb[AccumCostDEM].SetGridElevation(X,Y,MissingGridPoint);
         end;
      end;
   end;

   NumInStack := 0;
   SetPointAndFigureNeighbors(StartX,StartY,0,0,cdFlat);
   ClearStack(CostStack);
   {$IfDef RecordBasicsAccumPath} WriteLineToDebugFile('Neighbors done'); {$EndIf}

   Found := 0;
   StartProgress('Make ' + ExtractFileNameNoExt(fName));
   DEMGridLimits := DEMGlb[AccumCostDEM].FullDEMGridLimits;
   Cycles := 0;
   {$IfDef RecordBasicsAccumPath} WriteLineToDebugFile('Start loop'); {$EndIf}
   repeat
      inc(Cycles);
      for x := DEMGridLimits.XGridLow to DEMGridLimits.XGridHigh do begin
         for y := DEMGridLimits.YGridLow to DEMGridLimits.YGridHigh do begin
            if DEMGlb[FinalAccumCostDEM].MissingDataInGrid(x,y) then begin
               DEMGlb[AccumCostDEM].GetElevMetersOnGrid(x,y,z);
               DEMGlb[AccumDistanceDEM].GetElevMetersOnGrid(x,y,z2);
               if (z < PointNotYetSet) then begin
                  DEMGlb[DownCostDEM].GetElevMetersOnGrid(x,y,zf);
                  Dir := tCompassDirection(round(zf));
                  CheckStack(CostStack,x,y,z,z2,Dir);
               end;
            end;
         end;
      end;
      UpDateProgressBar(Found/(DEMGridLimits.XGridHigh*DEMGridLimits.YGridHigh));
      NewMinFound := (NumInStack > 0);
      if NewMinFound then ClearStack(CostStack);
   until (not NewMinFound);
   EndProgress;
   {$IfDef RecordBasicsAccumPath} WriteLineToDebugFile('cycles done=' + IntToStr(Cycles)); {$EndIf}

   if MDDef.LCPsavecost then begin
      DEMGlb[FinalAccumCostDEM].MarkAboveMissing(MDDef.ImpossibleCost*MDDef.MaxImpossibleInPath,npts);
      DEMGlb[FinalAccumCostDEM].CheckMaxMinElev;
      DEMGlb[FinalAccumCostDEM].WriteNewFormatDEM(fname);
     {$IfDef RecordBasicsAccumPath} WriteLineToDebugFile('FinalAccumCostDEM Saved'); {$EndIf}
   end;

   if MDDef.LCPsavedir then begin
      DEMGlb[DownCostDEM].CheckMaxMinElev;
      DEMGlb[DownCostDEM].WriteNewFormatDEM(fName2);
   end;

   if MDDef.LCPsavedist then begin
      DEMGlb[FinalDistanceDEM].MultiplyGridByConstant(0.001);
      DEMGlb[FinalDistanceDEM].CheckMaxMinElev;
      DEMGlb[FinalDistanceDEM].WriteNewFormatDEM(fname3);
     {$IfDef RecordBasicsAccumPath} WriteLineToDebugFile('FinalDistanceDEM Saved'); {$EndIf}
   end;
   CloseSingleDEm(AccumCostDEM);
   CloseSingleDEm(FinalAccumCostDEM);
   CloseSingleDEm(AccumDistanceDEM);
   CloseSingleDEm(FinalDistanceDEM);
   CloseSingleDEm(DownCostDEM);
   CloseSingleDEM(CostPassageDEM);
   {$IfDef RecordBasicsAccumPath}   WritelineToDebugFile('Exit AccumulatedCostSurface'); {$EndIf}
end;


initialization
   FinalAccumCostDEM := 0;
   DownCostDEM := 0;
   LCPJustDistance := false;
finalization
   {$IfDef RecordSetAccumPathPoints} WriteLineToDebugFile('RecordFullPath active in least_cost_path (big slowdown)'); {$EndIf}
   {$IfDef RecordFullPath} WriteLineToDebugFile('RecordFullPath active in least_cost_path (big slowdown)'); {$EndIf}
   {$IfDef RecordBasicsAccumPath} WriteLineToDebugFile('RecordBasicsAccumPath active in least_cost_path (big slowdown)'); {$EndIf}
end.


