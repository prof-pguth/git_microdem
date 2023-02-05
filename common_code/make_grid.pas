unit make_grid;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IFDEF DEBUG}
   //{$Define NoParallelFor} //used to debug only
   {$Define NoParallelMoments} // added 4/25/2022 to track down bug, removed 8/5/2022 but immediately returned since SSO normals might not be thread safe

   {$IfDef RecordProblems}   //normally only defined for debugging specific problems
      //$Define CreateAspectMap}
      //{$Define CreateGeomorphMaps}
      //{$Define RecordTimeGridCreate}
      //{$Define RecordPointClass}
      //{$Define RecordDEMCompare}
      //{$Define NewVATgrids}
   {$EndIf}
{$ELSE}
    //{$Define NoParallelFor}
{$ENDIF}


interface

 uses
   SysUtils, Windows, Classes, Graphics, Controls,JPEG,DBCtrls,Math,dbClient,
   System.Threading,System.SyncObjs,
   Forms, Dialogs, ExtCtrls, StdCtrls,Printers,ComCtrls,ClipBrd, Menus, Buttons, ToolWin,StrUtils,db,
   System.Types,

   {$IfDef RecordTime}
      System.Diagnostics,System.TimeSpan,
   {$EndIf}

   {$IfDef MSWindows}
      ShlObj, ActiveX,URLMon,Registry,Messages,ShellAPI,
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
     FireDAC.Comp.Client,
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

type
   tListOfDEMs = array[1..MaxGrids] of integer;


function ShortDerivativeMapName(ch : AnsiChar; SampleBoxSize : integer = 0) : ShortString;
function DerivativeMapName(ch : AnsiChar; SampleBoxSize : integer = 0) : ShortString;

function MakeSingleNewDerivativeMap(ch : AnsiChar; CurDEM : integer = 0; SampleBoxSize : integer = 0; ShowMap : boolean = true) : integer;

procedure CreateOpennessMap(WhichDEM : integer; DoUpward,DoDownWard : boolean);
function CreateRidgeMap(WhichDEM : integer; GridLimits : tGridLimits;  RidgeTypeMap : tRidgeTypemap; Memo1 : tMemo = Nil) : integer;
function CreateIwashishiPikeMap(NewName : ShortString; BaseGrid,SlopeGrid,RoughGrid,ConvexGrid : integer) : integer;
function AspectDifferenceMap(WhichDEM : integer; GridLimits : tGridLimits) : integer;

function MakeMomentsGrid(CurDEM : integer; What : char; BoxSizeRadiusMeters : integer = -99; OpenMaps : boolean = true) : integer;

function CreateProfileConvexityMap(WhichDEM : integer; OpenMap : boolean = true) : integer;
function CreateSlopeMap(WhichDEM : integer; OpenMap : boolean = true; Components : boolean = false) : integer;
function MakeAspectMap(ElevMap : integer) : integer;


function CreateStandardDeviationMap(DEM,Radius : integer) : integer;

procedure ModeFilterDEM(DEM,BufferSize : integer; JustDoHoles : boolean);
procedure RGBFilterDEM(DEM,BufferSize : integer; JustDoHoles : boolean);

function MakeTRIGrid(CurDEM : integer; Normalize : boolean = false; DoTPI : boolean = true) : integer;

function CreateRoughnessMap(WhichDEM : integer; OpenMap : boolean = true) : integer;
function CreateRoughnessMap2(DEM : integer; OpenMap : boolean = true; SaveSlopeMap : boolean = true) : integer;
function CreateRoughnessMapAvgVector(WhichDEM : integer; OpenMap : boolean = true) : integer;

function CreateRoughnessSlopeStandardDeviationMap(DEM,RadiusMustBeOdd : integer) : integer;
function CreateSlopeRoughnessSlopeStandardDeviationMap(DEM,RadiusMustBeOdd : integer; var SlopeMap : integer) : integer;

procedure MakeGammaGrids(CurDEM,BoxSize : integer);

function AirBallDirtBallMap(DEMonMap,DSM,DTM : integer; fName : PathStr = '') : integer;
function TwoDEMHighLowMap(RefDEM,DEM1,DEM2 : integer; DEMtype : shortstring; FourCats : boolean; fName2 : PathStr = '') : integer;


{$IfDef ExExoticMaps}
{$Else}
    function CreateAnOrganizationMap(WhichDEM : integer; OpenMap : boolean = true) : integer;
{$EndIf}

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
   DEMweapn;

var
   CountInStrips : integer;


function MakeAspectMap(ElevMap : integer) : integer;
begin
   {$If Defined(CreateGeomorphMaps) or Defined(CreateAspectMap)} WriteLineToDebugFile('MakeAspectMap in, dem=' + IntToStr(ElevMap)); {$EndIf}
   SaveBackupDefaults;
   SetAllSlopes(false);
   MDDef.DoAspect := true;
   Result := MakeMomentsGrid(ElevMap,'S');
   RestoreBackupDefaults;
   {$If Defined(CreateGeomorphMaps) or Defined(CreateAspectMap)}  WriteLineToDebugFile('MakeAspectMap out, dem=' + IntToStr(ElevMap) + '  aspect map=' + IntToStr(Result)); {$EndIf}
end;


function TwoDEMHighLowMap(RefDEM,DEM1,DEM2 : integer; DEMtype : shortstring; FourCats : boolean; fName2 : PathStr = '') : integer;
const
   SimpleTolerance = 1.51;
   MaxHist = 9;
   LongCatName : array[1..9] of shortstring = ('Both high','ALOS high/COP good','ALOS high/COP low',
                                                'ALOS good/COP high','Both good','ALOS good/COP low',
                                                'ALOS low/COP high','ALOS low/COP good','Both low');
   LongCatColor : array[1..9] of tColor = (clLime,clGreen,clBlue,
                                                clAqua,clYellow,clTeal,
                                                clPurple,clFuchsia,clRed);
var
   i : integer;
   zDEM2,zRefDEM,zDEM1,What : float32;
   Lat,Long : float64;
   x,y : integer;
   VAT : tStringList;
   TStr : shortstring;
   Hist : array[1..MaxHist] of int64;
   DEM1high, DEM1low, DEM1good, DEM2high, DEM2low, DEM2good : boolean;
begin
   {$If Defined(RecordDEMCompare) or Defined(NewVATgrids)} WriteLineToDebugFile('TwoDEMHighLowMap in, REFDEM=' + IntToStr(RefDEM) + '   DEM1=' + IntToStr(DEM1) + ' and DEM2=' + IntToStr(DEM2));  {$EndIf}
   Result := 0;
   if ValidDEM(RefDEM)then begin
      for i := 1 to MaxHist do Hist[i] := 0;
      if (fName2 = '') then fName2 := 'two_dems_to_ref_' + DEMGlb[RefDEM].AreaName;
      Result := DEMGlb[RefDEM].CloneAndOpenGrid(ByteDEM,fName2,euIntCode);
      DEMglb[Result].SetEntireGridMissing;
      StartProgressAbortOption('Air or dirt');
      for x := 0 to pred(DEMGlb[RefDEM].DEMheader.NumCol) do begin
         UpdateProgressBar(x/DEMGlb[RefDEM].DEMheader.NumCol);
         for y := 0 to pred(DEMGlb[RefDEM].DEMheader.NumRow) do begin
            if DEMGlb[RefDEM].GetElevMetersOnGrid(x,y,zRefDEM) then begin
               DEMGlb[RefDEM].DEMGridToLatLongDegree(x,y,Lat,Long);
                  if DEMGlb[DEM1].GetElevFromLatLongDegree(Lat,Long,zDEM1) and DEMGlb[DEM2].GetElevFromLatLongDegree(Lat,Long,zDEM2) then begin
                     DEM1high := (zDEM1 > zRefDEM + SimpleTolerance);
                     DEM1low :=  (zDEM1 < zRefDEM - SimpleTolerance);
                     DEM1good :=  (not DEM1high) and (not DEM1Low);
                     DEM2high := (zDEM2 > zRefDEM + SimpleTolerance);
                     DEM2low :=  (zDEM2 < zRefDEM - SimpleTolerance);
                     DEM2good := (not DEM2high) and (not DEM2Low);

                     if FourCats then begin
                        if DEM1high and DEM2high then What := 5
                        else if DEM1low and DEM2Low then What := 1
                        else if DEM1good and DEM2good then What := 3
                        else What := 6;
                     end
                     else begin
                         if DEM1high then begin
                            if DEM2high then What := 1
                            else if DEM2good then What := 2
                            else what := 3;
                         end
                         else if DEM1good then begin
                            if DEM2high then What := 4
                            else if DEM2good then What := 5
                            else what := 6;
                         end
                         else if DEM1Low then begin
                            if DEM2high then What := 7
                            else if DEM2good then What := 8
                            else what := 9;
                         end;
                     end;

                     if (What <> 0) then begin
                        DEMglb[Result].SetGridElevation(x,y,what);
                        inc(Hist[round(what)]);
                     end;
                  end;
            end;
         end;
      end;
      Vat := tStringList.Create;
      Vat.add('VALUE,NAME,N,USE,COLOR');
      if FourCats then begin
         if (Hist[5] > 0) then Vat.add('5,Both high,' + IntToStr(Hist[5]) + ',Y,' + IntToStr(clBlue));
         if (Hist[3] > 0) then Vat.add('3,Both ' + DEMtype +' ± ' + RealToString(SimpleTolerance,-5,1) + ',' + IntToStr(Hist[3]) + ',Y,' + IntToStr(clLime));
         if (Hist[1] > 0) then Vat.add('1,Both low,' + IntToStr(Hist[1]) + ',Y,' + IntToStr(clBrown));
         if (Hist[6] > 0) then Vat.add('6,Complex,' + IntToStr(Hist[6]) + ',Y,' + IntToStr(clYellow));
      end
      else begin
         for i := 1 to 9 do if (Hist[i] > 0) then Vat.add(IntToStr(i) + ',' + LongCatName[i] + ',' + IntToStr(Hist[i]) + ',Y,' + IntToStr(LongCatColor[i]));
      end;
      //{$If Defined(RecordDEMCompare)} WriteLineToDebugFile(''); WriteLineToDebugFile('TwoDEMHighLowMap VAT'); WriteStringListToDebugFile(VAT); WriteLineToDebugFile(''); {$EndIf}
      fName2 := MDTempDir + fName2 + '.vat.dbf';
      StringList2CSVtoDB(vat,fName2,true);
      DEMGlb[Result].VATFileName := fName2;
      DEMglb[Result].CheckMaxMinElev;
      DEMglb[Result].SetUpMap(Result,true,mtDEMVATTable);
      {$If Defined(RecordDEMCompare) or Defined(NewVATgrids)} WriteLineToDebugFile('TwoDEMHighLowMap out, new grid=' + IntToStr(Result));  {$EndIf}
   end
   else begin
      {$If Defined(RecordDEMCompare) or Defined(NewVATgrids)} HighlightLineToDebugFile('TwoDEMHighLowMap invalid input, DEM=' + IntToStr(RefDEM));  {$EndIf}
   end;
end;


function AirBallDirtBallMap(DEMonMap,DSM,DTM : integer; fName : PathStr = '') : integer;
const
   Tolerance = 0.51;
   HighTolerance = 2.51;
   SimpleTolerance = 0.51;
var
   i : integer;
   z1,z2,z3,What : float32;
   Lat,Long : float64;
   x,y : integer;
   VAT : tStringList;
   TStr : shortstring;
   Hist : array[1..5] of int64;
begin
   Result := 0;
   if ValidDEM(DEMonMap) and (ValidDEM(DSM) or ValidDEM(DTM)) then begin
      for i := 1 to 5 do Hist[i] := 0;
      if (fName = '') then fName := 'air_dirt_' + DEMGlb[DEMonMap].AreaName;
      Result := DEMGlb[DEMonMap].CloneAndOpenGrid(ByteDEM,fName,euIntCode);
      DEMglb[Result].SetEntireGridMissing;
      StartProgressAbortOption('Air or dirt');
      for x := 0 to pred(DEMGlb[DEMonMap].DEMheader.NumCol) do begin
         UpdateProgressBar(x/DEMGlb[DEMonMap].DEMheader.NumCol);
         for y := 0 to pred(DEMGlb[DEMonMap].DEMheader.NumRow) do begin
            if DEMGlb[DEMonMap].GetElevMetersOnGrid(x,y,z2) then begin
               DEMGlb[DEMonMap].DEMGridToLatLongDegree(x,y,Lat,Long);
                  if ((DSM = 0) or DEMGlb[DSM].GetElevFromLatLongDegree(Lat,Long,z3)) and DEMGlb[DTM].GetElevFromLatLongDegree(Lat,Long,z1) then begin
                     if (DSM = 0) then z3 := z1;
                     if (DTM = 0) then z1 := z3;

                     if z2 > z3 + SimpleTolerance then What := 5
                     else if z2 < z1 - SimpleTolerance then What := 1
                     else What := 3;
                     (*
                     if z2 > z3 + HighTolerance then What := 5
                     else if z2 > z3 + Tolerance then What := 4
                     else if z2 > z1 - Tolerance then What := 3
                     else if z2 > z1 - HighTolerance then What := 2
                     else What := 1;
                     *)
                     DEMglb[Result].SetGridElevation(x,y,what);
                     inc(Hist[round(what)]);
                  end;
            end;
         end;
      end;
      Vat := tStringList.Create;
      Vat.add('VALUE,NAME,N,USE,COLOR');

      if (DSM = 0) then TStr := 'DTM' else if (DTM = 0) then TStr := 'DSM' else TStr := 'Canopy';
      if (Hist[5] > 0) then Vat.add('5,Air ball,' + IntToStr(Hist[5]) + ',Y,' + IntToStr(clBlue));
      if (Hist[3] > 0) then Vat.add('3,' + TStr + ' ± ' + RealToString(SimpleTolerance,-5,1)  + ',' + IntToStr(Hist[3]) + ',Y,' + IntToStr(clLime));
      if (Hist[1] > 0) then Vat.add('1,Ground ball,' + IntToStr(Hist[1]) + ',Y,' + IntToStr(clBrown));  //238,138,248)));

      (*
      if (Hist[5] > 0) then Vat.add('5,High Air ball,' + IntToStr(Hist[5]) + ',Y,' + IntToStr(clBlue));
      if (Hist[4] > 0) then Vat.add('4,Low Air ball + ' + RealToString(HighTolerance,-5,1) + ',' + IntToStr(Hist[4]) + ',Y,' + IntToStr(RGB(0,255,255)));
      if (Hist[3] > 0) then Vat.add('3,Canopy ± ' + RealToString(Tolerance,-5,1) + ',' + IntToStr(Hist[3]) + ',Y,' + IntToStr(clLime));
      if (Hist[2] > 0) then Vat.add('2,Shallow Ground ball -' + RealToString(HighTolerance,-5,1) + ',' + IntToStr(Hist[2]) + ',Y,' + IntToStr(RGB(240,134,80)));
      if (Hist[1] > 0) then Vat.add('1,Deep Ground ball,' + IntToStr(Hist[1]) + ',Y,' + IntToStr(RGB(238,138,248)));
      *)
      fName := MDTempDir + fName + '.vat.dbf';
      StringList2CSVtoDB(vat,fName,true);
      DEMGlb[Result].VATFileName := fName;
      DEMglb[Result].CheckMaxMinElev;
      DEMglb[Result].SetUpMap(Result,true,mtDEMVATTable);
   end;
end;


function CreateStandardDeviationMap(DEM,Radius : integer) : integer;
var
   SlopeGrid,x,y,i,j : integer;
   sl : array[1..500] of float32;
   MomentVar : tMomentVar;
   z : float32;
begin
   Radius := Radius div 2;
   Result := DEMGlb[DEM].CloneAndOpenGrid(FloatingPointDEM,'md_elev_std_' + FilterSizeStr(Radius) + '_' + DEMGlb[DEM].AreaName,DEMGlb[DEM].DEMheader.ElevUnits);   //,false,1);
   Radius := Radius div 2;
   StartProgressAbortOption('std dev grid');
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         MomentVar.NPts := 0;
         for I := -Radius to Radius do begin
            for J := -Radius to Radius do begin
               if DEMGlb[DEM].GetElevMetersOnGrid(x+i,y+j,z) then begin
                  inc(MomentVar.Npts);
                  sl[MomentVar.Npts] := z;
               end;
            end;
         end;
         if (MomentVar.NPts > 5) then begin
            moment(sl,MomentVar,msAfterStdDev);
            DEMglb[Result].SetGridElevation(x,y,MomentVar.sdev);
         end;
      end;
   end;
   DEMglb[Result].CheckMaxMinElev;
   DEMglb[Result].SetUpMap(Result,true,mtElevSpectrum);
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateRoughnessMap2, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProjection.ProjDebugName); {$EndIf}
end;


function CreateRoughnessSlopeStandardDeviationMap(DEM,RadiusMustBeOdd : integer) : integer;
var
   SlopeMap : integer;
begin
   SlopeMap := 0;
   CreateSlopeRoughnessSlopeStandardDeviationMap(DEM,RadiusMustBeOdd,SlopeMap);
end;


function CreateSlopeRoughnessSlopeStandardDeviationMap(DEM,RadiusMustBeOdd : integer; var SlopeMap : integer) : integer;
var
   x,y,i,j,Radius : integer;
   Slope : float32;
   fName : PathStr;
   MomentVar : tMomentVar;
   ReturnSlopeMap : boolean;
   sl : array[1..100] of float32;
begin
   ReturnSlopeMap := (SlopeMap <> 0);
   SlopeMap := CreateSlopeMap(DEM,ReturnSlopeMap);
   fName := 'md_ruff_slope_std_' + FilterSizeStr(RadiusMustBeOdd) + '_' + DEMGlb[DEM].AreaName;
   Result := DEMGlb[DEM].CloneAndOpenGrid(FloatingPointDEM,fName,PercentSlope);
   Radius := RadiusMustBeOdd div 2;
   StartProgressAbortOption(fName);
   for x := Radius to pred(DEMGlb[DEM].DEMheader.NumCol - Radius) do begin
      UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := Radius to pred(DEMGlb[DEM].DEMheader.NumRow - Radius) do begin
         MomentVar.Npts := 0;
         for I := x-Radius to x+Radius do begin
            for J := y-Radius to y+Radius do begin
               if DEMGlb[SlopeMap].GetElevMetersOnGrid(i,j,Slope) then begin
                  inc(MomentVar.Npts);
                  sl[MomentVar.Npts] := Slope;
               end;
            end;
         end;
         if (MomentVar.NPts > 5) then begin
            moment(sl,MomentVar,msAfterStdDev);
            DEMglb[Result].SetGridElevation(x,y,MomentVar.sdev);
         end;
      end;
   end;
   DEMglb[Result].CheckMaxMinElev;
   DEMglb[Result].SetUpMap(Result,true,mtElevSpectrum);
   if not ReturnSlopeMap then begin
      CloseSingleDEM(SlopeMap);
      SlopeMap := 0;
   end;
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateRoughnessMap2, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProjection.ProjDebugName); {$EndIf}
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
var
   SlopeGrid,x,y : integer;
   Roughness : float32;
   sl : array[1..9] of float32;
   MomentVar : tMomentVar;
begin
   SlopeGrid := CreateSlopeMap(DEM,OpenMap);
   Result := DEMGlb[DEM].CloneAndOpenGrid(FloatingPointDEM,'md_roughness_elev_std_3x3_' + DEMGlb[DEM].AreaName,DEMGlb[DEM].DEMheader.ElevUnits);  //,false,1);
   MomentVar.Npts := 9;

   StartProgressAbortOption('roughness');
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if DEMGlb[DEM].SurroundedPointElevs(x,y,sl[1],sl[2],sl[3],sl[4],sl[5],sl[6],sl[7],sl[8],sl[9]) then begin
            moment(sl,MomentVar,msAfterStdDev);
            DEMglb[Result].SetGridElevation(x,y,MomentVar.sdev);
         end;
      end;
   end;

   DEMglb[Result].CheckMaxMinElev;
   if OpenMap then DEMglb[Result].SetUpMap(Result,OpenMap,mtElevSpectrum);
   if (not SaveSlopeMap) then CloseSingleDEM(SlopeGrid);
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateRoughnessMap2, NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProjection.ProjDebugName); {$EndIf}
end;


procedure RGBFilterDEM(DEM,BufferSize : integer; JustDoHoles : boolean);
var
   x,y,x2,y2,NewDEM : integer;
   r,g,b : byte;
   z : LongWord;
   rsum,gsum,bsum,Npts : int32;
begin
   StartProgress('RGB filter');
   NewDEM := DEMGlb[DEM].CloneAndOpenGrid(LongWordDEM,DEMGlb[DEM].AreaName +'_rgb_filter',DEMGlb[DEM].DEMheader.ElevUnits);
   for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
         if JustDoHoles and (not DEMGlb[DEM].MissingDataInGrid(x,y)) then begin
            DEMGlb[NewDEM].LongWordElevations^[x]^[y] := DEMGlb[DEM].LongWordElevations^[x]^[y];
         end
         else begin
            rsum := 0;
            gsum := 0;
            bsum := 0;
            NPts := 0;
            for x2 := x-BufferSize to x+BufferSize do begin
               for y2 := y-BufferSize to y+BufferSize do
                  if DEMGlb[DEM].RGBfromLongWord(x2,y2,r,g,b) then begin
                     rsum := rsum + r;
                     gsum := gsum + g;
                     bsum := bsum + b;
                     inc(NPts);
                  end;
            end;
            if (Npts > 0) then begin
               z := (rsum div NPts) + 256 * (gsum div NPts) + 256 * 256 * (bsum div NPts);
               DEMGlb[NewDEM].LongWordElevations^[x]^[y] := z;
            end;
         end;
      end;
   end;
   EndProgress;
   DEMGlb[NewDEM].SetUpMap(NewDEM,true,mtRGB);
end;


procedure ModeFilterDEM(DEM,BufferSize : integer; JustDoHoles : boolean);
var
   i,x,y,x2,y2,MaxC,MaxL,NewDEM,Missing : integer;
   Hist : array[0..255] of integer;
   z : float32;
begin
   StartProgress('Mode filter');
   NewDEM := DEMGlb[DEM].CloneAndOpenGrid(ByteDEM,DEMGlb[DEM].AreaName +'_mode_filter',DEMGlb[DEM].DEMheader.ElevUnits);
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
   DEMGlb[NewDEM].SetUpMap(NewDEM,true,mtElevSpectrum);
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


function MakeTRIGrid(CurDEM : integer; Normalize : boolean = false; DoTPI : boolean = true) : integer;
var
   i,j,TPIGrid : integer;
   GridLimits : tGridLimits;
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
   sum,FactorEW,FactorDiag : float64;
   TStr : shortstring;
begin
    if Normalize then TStr := '_norm' else TStr := '';
    Result := DEMGlb[CurDEM].CloneAndOpenGrid(FloatingPointDEM, 'MD_TRI' + TStr + '_' + DEMGlb[CurDem].AreaName,euMeters);

    if DoTPI then TPIgrid := DEMGlb[CurDEM].CloneAndOpenGrid(FloatingPointDEM,'MD_TPI' + TStr + '_' + DEMGlb[CurDem].AreaName,euMeters);
    GridLimits := TheDesiredLimits(CurDEM);
    if ShowSatProgress then StartProgressAbortOption('TRI');
    for j := GridLimits.YGridLow to GridLimits.YGridHigh do begin
       if (j mod 25 = 0) and ShowSatProgress then UpdateProgressBar((j-GridLimits.YGridLow) / (GridLimits.YGridHigh-GridLimits.YGridLow));
       if Normalize then begin
          FactorEW := DEMGlb[CurDEM].AverageYSpace / DEMGlb[CurDEM].AverageXSpace;
          FactorDiag := DEMGlb[CurDEM].AverageYSpace / DEMGlb[CurDEM].AverageDiaSpace;
       end;
       for i := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          if DEMGLB[CurDEM].SurroundedPointElevs(i,j, znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
             if Normalize then begin
                znw := z + (zNW-z) * FactorDiag;
                zne := z + (zNE-z) * FactorDiag;
                zsw := z + (zSW-z) * FactorDiag;
                zse := z + (zSE-z) * FactorDiag;
                ze := z + (ze-z) * FactorEW;
                zw := z + (zw-z) * FactorEW;
             end;
             Sum := sqr(z-zn) + sqr(z-zne) + sqr(z-ze)+ sqr(z-zse)+ sqr(z-zs) + sqr(z-zsw)+ sqr(z-zw)+ sqr(z-znw);
             //changed 9/18/21 to remove the averaging; now matches GDAL     changed back 4/22/2022 to match GRASS
             sum := sqrt(sum/8);
             DEMGlb[Result].SetGridElevation(i,j,sum);
             if DoTPI then begin
                Sum := z-(zn+zne+ze+zse+zs+zsw+zw+znw)/8;
                DEMGlb[TPIGrid].SetGridElevation(i,j,sum);
             end;
          end;
       end;
    end;
    if ShowSatProgress then EndProgress;
    DEMGlb[Result].SetUpMap(Result,true,mtElevSpectrum);
    if DoTPI then DEMGlb[TPIGrid].SetUpMap(TPIGrid,true,mtElevSpectrum);
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
      NewDEM[i] := DEMGlb[CurDEM].CloneAndOpenGrid(FloatingPointDEM,DEMGlb[CurDEM].AreaName +'_gamma_'+gn[i],DEMGlb[CurDEM].DEMheader.ElevUnits);

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
      DEMGlb[NewDEM[i]].SetUpMap(NewDEM[i],true,mtElevSpectrum);
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

          if (What = 'G') then begin
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
          {$EndIf}
          else if (What = 'C') then  begin
             if DEMGlb[CurDEM].GetEvansParams(x,y,MDDef.WoodRegionSize,SlopeDeg,SlopeCurvature,PlanCurvature,CrossC,MaxCurve,MinCurve) then  begin
                PostResults(DEMs[1],x,y,Gridinc,SlopeCurvature);
                PostResults(DEMs[2],x,y,Gridinc,PlanCurvature);
                PostResults(DEMs[3],x,y,Gridinc,CrossC);
                PostResults(DEMs[4],x,y,Gridinc,MinCurve);
                PostResults(DEMs[5],x,y,GridInc,MaxCurve);
             end;
          end
          else if (What = 'S') then  begin
               if DEMGlb[CurDEM].GetSlopeAndAspect(x,y,SlopeAspectRec) then begin
                 PostResults(DEMs[1],x,y,GridInc,SlopeAspectRec.SlopePercent);
                 PostResults(DEMs[2],x,y,GridInc,SlopeAspectRec.SlopeDegree);
                 PostResults(DEMs[3],x,y,GridInc,SinDeg(SlopeAspectRec.SlopeDegree));
                 PostResults(DEMs[4],x,y,GridInc,log10(TanDeg(SlopeAspectRec.SlopeDegree)));
                 PostResults(DEMs[5],x,y,GridInc,sqrt(SinDeg(SlopeAspectRec.SlopeDegree)));
                 PostResults(DEMs[9],x,y,GridInc,ln(TanDeg(SlopeAspectRec.SlopeDegree)));
                 PostResults(DEMs[10],x,y,GridInc,SinDeg(SlopeAspectRec.SlopeDegree)/CosDeg(SlopeAspectRec.SlopeDegree));
                 if MDDef.SignedSlopeComponents then begin
                    PostResults(DEMs[11],x,y,GridInc, 100 * SlopeAspectRec.dzdy);
                    PostResults(DEMs[12],x,y,GridInc, 100 * SlopeAspectRec.dzdx);
                 end
                 else begin
                    PostResults(DEMs[11],x,y,GridInc, abs(100 * SlopeAspectRec.dzdy));
                    PostResults(DEMs[12],x,y,GridInc, abs(100 * SlopeAspectRec.dzdx));
                 end;

                 if (SlopeAspectRec.AspectDir < 365) then begin
                    PostResults(DEMs[6],x,y,GridInc,round(SlopeAspectRec.AspectDir));
                    PostResults(DEMs[7],x,y,GridInc,sinDeg(SlopeAspectRec.AspectDir));
                    PostResults(DEMs[8],x,y,GridInc,cosDeg(SlopeAspectRec.AspectDir));
                 end;
              end;
          end
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
          else if (What = 'O') then begin
             if DEMGlb[CurDEM].FigureOpenness(x,y,MDDef.OpenBoxSizeMeters,Upward,Downward) then begin
                PostResults(DEMs[1],x,y,GridInc,Upward);
                PostResults(DEMs[2],x,y,GridInc,Downward);
                PostResults(DEMs[3],x,y,GridInc,Upward-Downward);
             end;
          end
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
             else if What = 's' then DEMGlb[CurDEM].SlopeMoments(Limits,MomentVar)
             else if What = 'l' then DEMGlb[CurDEM].PlanCMoments(Limits,MomentVar)
             else if What = 'r' then DEMGlb[CurDEM].ProfCMoments(Limits,MomentVar);
             if (MomentVar.NPts > 3) then  begin
                PostResults(DEMs[1],x,y,GridInc,MomentVar.mean);
                PostResults(DEMs[2],x,y,GridInc,MomentVar.sdev);
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
   PartLimits :  tGridLimitsArray;
   TStr : ShortString;
   WantMapType : tMapType;
   fName,pName : PathStr;
   Stopwatch1 : TStopwatch;


       procedure NewGrid(var DEM : integer; Gridname : shortstring; ElevUnits : tElevUnit);
       begin
          Petmar.ReplaceCharacter(GridName,' ','_');
          DEM := DEMGlb[CurDEM].CloneAndOpenGrid(FloatingPointDEM,'md_' + GridName + '_' + DEMGlb[CurDEM].AreaName,ElevUnits);
          {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('Created DEM ' + IntToStr(DEM) + GridName + ' proj=' + DEMGlb[DEM].DEMMapProjection.ProjDebugName); {$EndIf}
       end;


begin
   {$If Defined(CreateGeomorphMaps) or Defined(CreateAspectMap)} WriteLineToDebugFile('MakeMomentsGrid in, dem=' + IntToStr(CurDEM) + '  what=' + what); {$EndIf}

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
      if MDDef.DoElevRelief then NewGrid(MomentDEMs[6], 'Elev_relief' + TStr,Undefined);
   end
   else if (What = 'R') then  begin
      ThinFactor := MDDef.ReliefCalcThin;
      if MDDef.DoRelief1 then NewGrid(MomentDEMs[1], 'Relief' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoAvgElev then NewGrid(MomentDEMs[2], 'Average_Elev' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoElevStd then NewGrid(MomentDEMs[3], 'Std Dev Elev' + TStr,DEMGlb[CurDEM].DEMheader.ElevUnits);
      if MDDef.DoREL then NewGrid(MomentDEMs[4], 'REL' + TStr,Undefined);
      if MDDef.DoTPI then NewGrid(MomentDEMs[5], 'TPI' + TStr,Undefined);
   end
   {$IfDef MultipleCurvatureMethods}
   else if (What = 'A') then begin
      TStr := '_curvature--' + CurvatureMethodName[MDDef.CurvatureMethod] +' (' + IntToStr(MDDef.CurvRegionSize) + ')';
      NewGrid(DEMs[1], '_slope' + TStr,UnDefined);
      NewGrid(DEMs[2], '_plan' + TStr,UnDefined);
   end
   {$EndIf}
   else if (What = 'C') then  begin
      if MDDef.DoSlopeCurve then NewGrid(MomentDEMs[1], 'Slope_curvature',UnDefined);
      if MDDef.DoPlanCurve then NewGrid(MomentDEMs[2], 'Plan_curvature',UnDefined);
      if MDDef.DoCrossCurve then NewGrid(MomentDEMs[3], 'Cross_sectional_curvature',UnDefined);
      if MDDef.DoMinCurve then NewGrid(MomentDEMs[4], 'Min_curvature',UnDefined);
      if MDDef.DoMaxCurve then NewGrid(MomentDEMs[5], 'Max_curvature',UnDefined);
      Result := MomentDEMs[1];
   end
   else if (What = 'O') then begin
      WantMapType := mtElevGray;
      ThinFactor := MDDef.OpennessCalcThin;
      if MDDef.DoUpOpen then NewGrid(MomentDEMs[1], 'Upward openness' + TStr,zDegrees);
      if MDDef.DoDownOpen then NewGrid(MomentDEMs[2], 'Downward openness' + TStr,zDegrees);
      if MDDef.DoDiffOpen then NewGrid(MomentDEMs[3], 'Difference openness' + TStr,zDegrees);
   end
   else if (What in ['F','Q']) then begin
      ThinFactor := MDDef.FabricCalcThin;
      if (What = 'F') then  begin
         if MDDef.DoS2S3 then NewGrid(MomentDEMs[1], 'Organization_Strength' + Tstr,UnDefined);
         if MDDef.DoFabDir90 then NewGrid(MomentDEMs[2], 'Organization_Direction_90' + TStr,zDegrees);
         if MDDef.DoS1S2 then NewGrid(MomentDEMs[3], 'Flatness_Strength' + TStr,Undefined);
         if MDDef.DoRoughness then NewGrid(MomentDEMs[4], 'Roughness' + TStr,Undefined);
         if MDDef.DoFabDir180 then NewGrid(MomentDEMs[5], 'Organization_Direction_180' + TStr,zDegrees);
         if MDDef.DoFabDir360 then NewGrid(MomentDEMs[6], 'Organization_Direction_360' + TStr,zDegrees);
         if MDDEF.DoAvgVectStrength then NewGrid(MomentDEMs[7], 'avg_aspect_strength' + TStr,Undefined);
         Result := MomentDEMs[4];
      end
      else if (What = 'Q') then  begin
         NewGrid(MomentDEMs[1],'Most organized region (m)' + TStr, DEMGlb[CurDEM].DEMheader.ElevUnits);
         NewGrid(MomentDEMs[2],'Largest S2S3 ' + TStr, Undefined);
         NewGrid(MomentDEMs[3],'Organization Direction 360 ' + TStr,zDegrees);
         NewGrid(MomentDEMs[4],'Relief (m) ' + TStr, DEMGlb[CurDEM].DEMheader.ElevUnits);
      end
   end
   else if (What = 'S') then begin
       if MDDef.DoSlopePC then NewGrid(MomentDEMs[1], ShortSlopeMethodName(MDDef.SlopeAlg) +'_Slope_percent',PercentSlope);
       if MDDef.DoSlopeDeg then NewGrid(MomentDEMs[2],ShortSlopeMethodName(MDDef.SlopeAlg) + '_Slope_degree)',zDegrees);
       if MDDef.DoSlopeSin then NewGrid(MomentDEMs[3],ShortSlopeMethodName(MDDef.SlopeAlg) + '_Sin_slope',Undefined);
       if MDDef.DoSlopeLogTan then NewGrid(MomentDEMs[4],ShortSlopeMethodName(MDDef.SlopeAlg) + '_log_tan_slope',Undefined);
       if MDDef.DoSlopeSqrtSin then NewGrid(MomentDEMs[5],ShortSlopeMethodName(MDDef.SlopeAlg) + '_sqrt_sin_slope',Undefined);
       if MDDef.DoAspect then NewGrid(MomentDEMs[6],ShortSlopeMethodName(MDDef.SlopeAlg) + '_Aspect_degree',AspectDeg);
       if MDDef.DoAspectNS then NewGrid(MomentDEMs[7],ShortSlopeMethodName(MDDef.SlopeAlg) + '_Aspect_NS',Undefined);
       if MDDef.DoAspectEW then NewGrid(MomentDEMs[8],ShortSlopeMethodName(MDDef.SlopeAlg) + '_Aspect_EW',Undefined);
       if MDDef.DoSlopeLnTan then NewGrid(MomentDEMs[9],ShortSlopeMethodName(MDDef.SlopeAlg) + '_ln_tan_slope',Undefined);
       if MDDef.DoSlopeMperM then NewGrid(MomentDEMs[10],ShortSlopeMethodName(MDDef.SlopeAlg) + '_m_per_m',zMperM);
       if MDDef.DoNSSlope then NewGrid(MomentDEMs[11],ShortSlopeMethodName(MDDef.SlopeAlg) + '_NS_comp',PercentSlope);
       if MDDef.DoEWSlope then NewGrid(MomentDEMs[12],ShortSlopeMethodName(MDDef.SlopeAlg) + '_EW_comp',PercentSlope);
       for i := 1 to 12 do if ValidDEM(i) then begin
          Result := MomentDEMs[i];
          break;
       end;
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
          fName := MDTempDir + DEMGlb[MomentDEMs[i]].AreaName + '.dem';
          DEMGlb[MomentDEMs[i]].WriteNewFormatDEM(fName,'difference map');
          CloseSingleDEM(MomentDEMs[i]);
          MomentDEMs[i] := OpenNewDEM(fName,false);

          if OpenMaps then begin
             {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('Create map for DEM ' + IntToStr(MomentDEMs[i]) + ' ' + DEMGlb[MomentDEMs[i]].KeyDEMParams); {$EndIf}
             DEMGlb[MomentDEMs[i]].SetUpMap(MomentDEMs[i],true,WantMapType,What in ['A','C','F']);
             DEMGlb[MomentDEMs[i]].SelectionMap.MapDraw.PrimMapProj.ProjectionSharedWithDataset := true;
             MatchAnotherDEMMap(MomentDEMs[i],CurDEM);
             {$IfDef CreateGeomorphMaps} DEMGlb[CurDEM].SelectionMap.MapDraw.PrimMapProj.ProjectionParamsToDebugFile('Original grid',true); {$EndIf}
             {$IfDef CreateGeomorphMaps} DEMGlb[MomentDEMs[i]].SelectionMap.MapDraw.PrimMapProj.ProjectionParamsToDebugFile('New grid',true); {$EndIf}
          end;
       end;
    end;

   if (What = 'S') then begin
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
end;

{$EndIf}


procedure CreateOpennessMap(WhichDEM : integer; DoUpward,DoDownWard : boolean);
var
   i : integer;
begin
    GetSampleBoxSize(WhichDEM,MDDef.OpenBoxSizeMeters);
    if MDDef.ConfirmOpennesDirections then Check_8_Dirs.GetDirectionsToUse(MDDef.OpennessDirs)
    else for I := 1 to 8 do MDDef.OpennessDirs[i] := true;
    MDDef.DoUpOpen := DoUpward;
    MDDef.DoDownOpen := DoDownWard;
    MakeMomentsGrid(WhichDEM,'O',MDDef.OpenBoxSizeMeters);
end;


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
      'D' : Result := 'Openness (- downward, L=' + IntToStr(SampleBoxSize) + ' m)';
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
      'O' : Result := 'Openness (+ upward, L=' + IntToStr(SampleBoxSize) + ' m)';
      'o' : Result := 'Slope (°)';
      'P' : Result := 'Percentile';
      'p' : Result := 'Point density';
      'R' : Result := 'Reflect';
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
      'R' : Result := 'Reflect';
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
   {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateProfileConvexityMap, InGrid=' + IntToStr(WhichDEM) + '  NewGrid=' + IntToStr(Result) + '  proj=' + DEMGlb[Result].DEMMapProjection.ProjDebugName); {$EndIf}
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
             Result := DEMGlb[CurDEM].GetSlopeAndAspect(Col,Row,SlopeAspectRec) and (SlopeAspectRec.AspectDir < 32000);
             if Result then begin
                Angle := abs(SlopeAspectRec.AspectDir - Inward);
                if (Angle > 180) then Angle := Angle - 180;
                Sum := Sum +  Angle;
             end;
          end;

begin
   {$IfDef RecordMakeNewMapProblems} WriteLineToDebugFile('MakeANewMap ' + DerivativeMapName(ch,SampleBoxSize)); {$EndIf}
   if (CurDEM = 0) or (DEMGlb[CurDEM] = Nil) then GetDEM(CurDEM);
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
            MDDef.WoodRegionSize := round(SampleBoxSize / DEMGlb[CurDEM].AverageSpace);
            if (MDDef.WoodRegionSize  < 1) then MDDef.WoodRegionSize := 1;
         end;

         if (ch in ['1','2','o','s','+','-']) then MDDef.WoodRegionSize := 1;

         if (ch = 'E') then DEMGlb[Result].DEMheader.ElevUnits := LnElev
         else if (ch = 'L') then DEMGlb[Result].DEMheader.ElevUnits := LogElev
         else if (ch in ['B','n','C','+','-','P','p','1','2','s','N','W','X','Y','Z']) then DEMGlb[Result].DEMheader.ElevUnits := Undefined
         else if (ch = 'R') then begin
            DEMGlb[Result].DEMheader.ElevUnits := Undefined;
            ChangeReflectanceOptions(DEMGlb[CurDEM].SelectionMap);
         end
         else if (ch = 'S') then DEMGlb[Result].DEMheader.ElevUnits := PercentSlope
         else if (ch in ['H','M','g','i']) then DEMGlb[Result].DEMheader.ElevUnits := euMeters
         else if (ch in ['A','o','c']) then DEMGlb[Result].DEMheader.ElevUnits := zDegrees
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
         DEMGlb[Result].AreaName := DEMGlb[CurDEM].AreaName + ' ' + DerivativeMapName(ch,SampleBoxSize);
         if ch = 'g' then DEMGlb[Result].AreaName := 'md_rugosity_(m per '+ RealToString(DEMGlb[Result].AverageSpace,-8,-1) + 'm)_' +  DEMGlb[Result].AreaName;

         DEMGlb[Result].DefineDEMVariables(true);

         StartProgressAbortOption(DEMGlb[Result].AreaName);
         TotalNumDone := 0;
         CumDone := 0;
         Col := GridLimits.XGridLow;
         while (Col <= GridLimits.XGridHigh) do begin
            UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
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
                         DEMGlb[result].SetGridElevation(Col,Row,(zr-MomentVar.mean)/MomentVar.SDev);
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
                     if (ch = 'R') then DEMGlb[Result].SetGridElevation(Col,Row,DEMGlb[CurDEM].ReflectanceValue(Col,Row))
                     else if (ch = 'T') then begin
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
         EndProgress;

         {$IfDef RecordMakeNewMapProblems} WriteLineToDebugFile('  compute over');   {$EndIf}

         if ShowMap then begin
             UsePC := false;
             if (ch in ['R']) then begin
                mt := mtElevGray;
             end
             else if (ch in ['B','E','L','A','C','c','i','V','P','p','+','-','1','2','n','o','s','S','g','Z','X','Y']) then  begin
                mt := mtElevSpectrum;
                UsePC := true;
             end
             else mt := MDdef.DefDEMMap;

             DEMGlb[Result].SetUpMap(Result,true,mt,UsePC);

             if DEMGlb[CurDEM].SelectionMap.FullMapSpeedButton.Enabled and MDDef.GeomorphMapsFullDEM then begin
                DEMGlb[Result].SelectionMap.MapDraw.NoDrawingNow := true;
                DEMGlb[Result].SelectionMap.RedrawMapForDataGrid(GridLimits.XGridLow,GridLimits.YGridHigh,GridLimits.XGridHigh,GridLimits.YGridLow,
                DEMGlb[CurDEM].SelectionMap.MapDraw.MapXSize,DEMGlb[CurDEM].SelectionMap.MapDraw.MapYSize);
                DEMGlb[Result].SelectionMap.MapDraw.NoDrawingNow := false;
                DEMGlb[Result].SelectionMap.N11view1Click(Nil);
             end;

             DEMGlb[Result].SelectionMap.SaveDEM1.Visible := false;
             DEMGlb[Result].SelectionMap.CheckProperTix;
         end
         else
         begin
            DEMGlb[Result].CheckMaxMinElev;
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


procedure AspectDifferenceMapStrip(WhichDEM,ResultDEM : integer; GridLimits : tGridLimits);
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
          if DEMGlb[WhichDEM].SurroundedPointElevs(i,j,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
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



function AspectDifferenceMap(WhichDEM : integer; GridLimits : tGridLimits) : integer;
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
     Result := DEMGlb[WhichDEM].CloneAndOpenGrid(ByteDEM,DEMGlb[WhichDem].AreaName + '_aspect_difference',zDegrees);

     {$IfDef RecordPointClass} WriteLineToDebugFile('New grid created'); {$EndIf}

     StartProgressAbortOption('Aspect difference');
     CountInStrips := 0;
    {$IfDef NoParallelFor}
        AspectDifferenceMapStrip(WhichDEM,Result,GridLimits);
    {$Else}
        PartLimits := GetLimitsForParallelLoops(GridLimits);
        ResultDEM := Result;
        TParallel.For(1,MDdef.MaxThreadsForPC,
           procedure (Value: Integer)
           begin
             AspectDifferenceMapStrip(WhichDEM,ResultDEM,PartLimits[Value]);
           end);
        ThreadsWorking := false;
    {$EndIf}
    EndProgress;

     {$IfDef RecordPointClass} WriteLineToDebugFile('New map created'); {$EndIf}

    DEMGlb[Result].SetUpMap(Result,true);
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
          if (MDDef.RidgePointClassify = raWood) then DEMGlb[WhichDEM].WoodPointClassify(i,j,PointType)
          else PointType := DEMGlb[WhichDEM].ClassifyAPoint(i,j);
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
{$IfDef ExGeology}
begin
{$Else}
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

     Result := DEMGlb[WhichDEM].CloneAndOpenGrid(DEMPrecision,DEMGlb[WhichDem].AreaName + '_' + TStr,ElevUnits);

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
       {$IfDef RecordPointClass} WriteLineToDebugFile('Memo filled in');   {$EndIf}
    end;

    {$IfDef RecordPointClass} WriteLineToDebugFile('Elev range done'); {$EndIf}

    if MDDef.MaskMapShow in [0,1,2] then begin
       DEMGlb[Result].SetUpMap(Result,true);

       if (RidgeTypeMap = rtmAllPoints) then begin
         VAT := tStringList.Create;
         VAT.Add('N,PERCENT,VALUE,NAME,COLOR');
         for Pt := EdgePoint to OtherPoint do begin
            VAT.Add(IntToStr(ClassNPts[pt]) + ',' + RealToString(ClassPC[pt],-18,2) + ',' + IntToStr(ord(PT)) + ',' + PointTypeName(pt) + ',' + IntToStr(PointTypeColor(pt)));
         end;

         DEMGlb[Result].Selectionmap.MapDraw.MapType := mtDEMVATTable;
         DEMGlb[Result].VATFileName := MDTempDir + 'class_map.csv';
         PetDBUtils.StringList2CSVtoDB(VAT,DEMGlb[Result].VATFileName,true);

         {$IfDef RecordPointClass} WriteLineToDebugFile('Call base map redraw'); {$EndIf}
          DEMGlb[Result].Selectionmap.DoBaseMapRedraw;
       end;
    end;
    if MDDef.MaskMapShow in [1,2] then DEMGlb[WhichDEM].SelectionMap.GridpointsfromsecondDEMAssignAndDraw(Result);

    {$IfDef RecordPointClass} WriteLineToDebugFile('Ridge map out'); {$EndIf}
{$EndIf}
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
   Result := DEMGlb[SlopeGrid].CloneAndOpenGrid(ByteDEM,NewName + '_IandPClass',euIntCode);
   DEMGlb[Result].AreaName := NewName + '_IandPClass';

   {$IfDef RecordPointClass} WriteLineToDebugFile('New grid created'); {$EndIf}
   if ShowSatProgress then StartProgressAbortOption('I&P class');
   CountInStrips := 0;
   ConvexityMeanCut := DEMGLB[ConvexGrid].FindPercentileElevation(MDDef.ConvexCut);
   TextureMeanCut := DEMGLB[RoughGrid].FindPercentileElevation(MDDef.RoughnessCut);
   SlopeMeanCut := DEMGLB[SlopeGrid].FindPercentileElevation(MDDef.SlopeCut1);
   SlopeQuarterCut := DEMGLB[SlopeGrid].FindPercentileElevation(MDDef.SlopeCut2);
   SlopeEigthCut := DEMGLB[SlopeGrid].FindPercentileElevation(MDDef.SlopeCut3);

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


initialization
finalization
    {$IfDef CreateGeomorphMaps} WriteLineToDebugFile('CreateGeomorphMaps active in make_grid'); {$EndIf}
    {$IfDef RecordPointClass} WriteLineToDebugFile('RecordPointClass active in make_grid'); {$EndIf}
    {$IfDef NoParallelFor} WriteLineToDebugFile('NoParallelFor active in make_grid'); {$EndIf}
end.
