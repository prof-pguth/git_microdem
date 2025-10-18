{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for demix_control       }
{________________________________________}



procedure NumHighLowNeighborsMaps(DEM,Radius : integer; Tolerance : float32; var HighNeigh,LowNeigh : integer);
type
   tHist = array[0..8] of int64;
const
   Colors : array[0..8] of tColor = (clBlue,clGreen,clAqua,
                                           clYellow,clLime,clTeal,
                                           clPurple,clFuchsia,clBrown);
 var
   i,Higher,Lower : integer;
   x,y : integer;
   HighHist,LowHist : tHist;


         procedure Finalize(Neigh : integer; Hist : tHist; aLabel : shortstring; ReverseColors : boolean);
         var
            VAT : tStringList;
            fName2 : PathStr;
            i : integer;
            aColor : tColor;
         begin
            Vat := tStringList.Create;
            Vat.add('VALUE,NAME,N,USE,COLOR');
            for i := 8 downto 0 do if (Hist[i] > 0) then begin
               if ReverseColors then aColor := Colors[8-i] else aColor := Colors[i];
               Vat.add(IntToStr(i) + ',' + IntToStr(i) + aLabel + ',' + IntToStr(Hist[i]) + ',Y,' + IntToStr(aColor));
            end;
            fName2 := NextFileNumber(MDTempDir,DEMGlb[Neigh].AreaName,'.vat.dbf');
            StringList2CSVtoDB(vat,fName2,true);
            DEMGlb[Neigh].VATFileName := fName2;
            DEMglb[Neigh].CheckMaxMinElev;
            DEMglb[Neigh].SetUpMap(true,mtDEMVATTable);
         end;


begin
   {$If Defined(RecordDEMCompare) or Defined(NewVATgrids)} WriteLineToDebugFile('TwoDEMHighLowMap in, DEM=' + IntToStr(DEM)); {$EndIf}
   if ValidDEM(DEM)then begin
      for i := 0 to 8 do begin
          HighHist[i] := 0;
          LowHist[i] := 0;
      end;
      HighNeigh := DEMGlb[DEM].CloneAndOpenGridSetMissing(ByteDEM,'higher_neighors_' + DEMGlb[DEM].AreaName + '_rad='+ IntToStr(Radius) + '_tol=' + RealToString(Tolerance,-4,-1),euIntCode);
      LowNeigh := DEMGlb[DEM].CloneAndOpenGridSetMissing(ByteDEM,'lower_neighors_' + DEMGlb[DEM].AreaName+ '_rad='+ IntToStr(Radius) + '_tol=' + RealToString(Tolerance,-4,-1),euIntCode);
      StartProgressAbortOption('High/low neighbors');
      for x := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
         UpdateProgressBar(x/DEMGlb[DEM].DEMheader.NumCol);
         for y := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
            if DEMGlb[DEM].HighLowPointNeighbors(x,y,Radius,Tolerance,Higher,Lower) then begin
               DEMglb[HighNeigh].SetGridElevation(x,y,Higher);
               inc(HighHist[Higher]);
               DEMglb[LowNeigh].SetGridElevation(x,y,Lower);
               inc(LowHist[Lower]);
            end;
         end;
      end;
      Finalize(HighNeigh,HighHist,' higher neighbors',True);
      Finalize(LowNeigh,LowHist,' lower neighbors',false);
      EndProgress;
   end;
end;


function RGBBestOfThreeMap(RefDEM,ALOS,Cop,Fab,Merge : integer; Tolerance : float32; AName : shortString) : integer;
type
   tHist = array[1..8] of int64;
var
   i,What,ALOSBest,COPbest,FABbest : integer;
   RefZ,CopZ,ALOSZ,FabZ,Best : float32;
   Lat,Long : float64;
   x,y : integer;
   fName : PathStr;
   VAT : tStringList;
   Maps : TStringList;
   Hist,ALOSHist,COPhist,FabHist : tHist;

   procedure FinalizeMap(DEM : integer; aHist : tHist; Extra : shortString = ' (or tied)');
   begin
      Vat := tStringList.Create;
      Vat.add('VALUE,NAME,N,USE,COLOR');
      if (aHist[1] > 0) then Vat.add('1,ALOS best' + Extra + ',' + IntToStr(aHist[1]) + ',Y,' + IntToStr(RGB(255,0,0)));
      if (aHist[2] > 0) then Vat.add('2,COP best' + Extra + ',' + IntToStr(aHist[2]) + ',Y,' + IntToStr(RGB(0,255,0)));
      if (aHist[3] > 0) then Vat.add('3,ALOS/COP best,' + IntToStr(aHist[3]) + ',Y,' + IntToStr(RGB(255,255,0)));
      if (aHist[4] > 0) then Vat.add('4,FABDEM best' + Extra + ',' + IntToStr(aHist[4]) + ',Y,' + IntToStr(RGB(0,0,255)));
      if (aHist[5] > 0) then Vat.add('5,ALOS/FABDEM best,' + IntToStr(aHist[5]) + ',Y,' + IntToStr(RGB(255,0,255)));
      if (aHist[6] > 0) then Vat.add('6,COP/FABDEM best,' + IntToStr(aHist[6]) + ',Y,' + IntToStr(RGB(0,255,255)));
      if (aHist[7] > 0) then Vat.add('7,ALOS/COP/FABDEM best,' + IntToStr(aHist[7]) + ',Y,' + IntToStr(RGB(255,255,255)));
      if (aHist[8] > 0) then Vat.add('8,Other best,' + IntToStr(aHist[8]) + ',Y,' + IntToStr(RGB(255,255,255)));

      DEMGlb[DEM].VATFileName := MDtempDir + DEMGlb[DEM].AreaName + '.vat.dbf';
      DeleteFileIfExists(DEMGlb[DEM].VATFileName);
      StringList2CSVtoDB(vat,DEMGlb[DEM].VATFileName,true);
      DEMglb[DEM].CheckMaxMinElev;
      DEMglb[DEM].SetUpMap(true,mtDEMVATTable);
      {$IfDef RecordDEMIX_VAT}
         WriteLineToDebugFile('RGBBestOfThreeMap FinalizeMap=' + IntToStr(DEMglb[DEM].SelectionMap.MapDraw.MapType) + '  DEM=' + IntToStr(DEM) + '   ' + DEMGlb[DEM].AreaName + '  ' + DEMGlb[DEM].VATFileName);
      {$EndIf}

      if ValidDEM(DEM) then begin
         if MDDef.AutoMergeStartDEM then begin
            DEMGlb[DEM].SelectionMap.MergeAnotherDEMreflectance(Merge,true);
         end;
         Maps.Add(DEMGlb[DEM].SelectionMap.Caption)
      end
      else begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeRGBBestMap failed, ' + fName); {$EndIf}
      end;
   end;


begin
   {$IfDef DEMIXmaps} WriteLineToDebugFile('RGBBestOfThreeMap in'); {$EndIf}
   for i := 1 to 8 do begin
      Hist[i] := 0;
      COPHist[i] := 0;
      FABHist[i] := 0;
      ALOSHist[i] := 0;
   end;
   Result := DEMGlb[RefDEM].CloneAndOpenGridSetMissing(ByteDEM,AName,euIntCode);
   if MDDef.RGBbestSeparates then begin
      COPbest := DEMGlb[RefDEM].CloneAndOpenGridSetMissing(ByteDEM,'COP_' + AName,euIntCode);
      ALOSBest := DEMGlb[RefDEM].CloneAndOpenGridSetMissing(ByteDEM,'ALOS_' + AName,euIntCode);
      FABbest := DEMGlb[RefDEM].CloneAndOpenGridSetMissing(ByteDEM,'FAB_' + AName,euIntCode);
   end
   else begin
      COPbest := 0;
      ALOSBest := 0;
      FABbest := 0;
   end;
   StartProgressAbortOption('Best on map');
   for x := 0 to pred(DEMGlb[RefDEM].DEMheader.NumCol) do begin
      UpdateProgressBar(x/DEMGlb[RefDEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[RefDEM].DEMheader.NumRow) do begin
         if DEMGlb[RefDEM].GetElevMetersOnGrid(x,y,RefZ) then begin
            DEMGlb[RefDEM].DEMGridToLatLongDegree(x,y,Lat,Long);
            if DEMGlb[ALOS].GetElevFromLatLongDegree(Lat,Long,ALOSz) and
                  DEMGlb[Cop].GetElevFromLatLongDegree(Lat,Long,CopZ) and
                  DEMGlb[FAB].GetElevFromLatLongDegree(Lat,Long,FabZ) then begin
               ALOSz := abs(ALOSz - RefZ);
               Copz := abs(COPz - RefZ);
               Fabz := abs(Fabz - RefZ);
               Best := PetMath.MinFloat(ALOSz,COPZ,FabZ);
               What := 0;
               if (ALOSZ - Best <= Tolerance) then begin
                  What := What + 1;
                  if ALOSBest <> 0 then DEMglb[ALOSbest].SetGridElevation(x,y,1);
                  inc(ALOSHist[1]);
               end
               else begin
                  inc(ALOShist[8]);
                  if ALOSBest <> 0 then DEMglb[ALOSbest].SetGridElevation(x,y,8);
               end;
               if (CopZ - Best <= Tolerance) then begin
                  What := What + 2;
                  if COPBest <> 0 then DEMglb[COPbest].SetGridElevation(x,y,2);
                  inc(COPHist[2]);
               end
               else begin
                  inc(COPhist[8]);
                  if COPBest <> 0 then DEMglb[COPbest].SetGridElevation(x,y,8);
               end;
               if (FabZ - Best <= Tolerance) then begin
                  What := What + 4;
                  if FABBest <> 0 then DEMglb[Fabbest].SetGridElevation(x,y,4);
                  inc(FabHist[4]);
               end
               else begin
                  inc(Fabhist[8]);
                  if FABBest <> 0 then DEMglb[Fabbest].SetGridElevation(x,y,8);
               end;
               if (What > 0) then begin
                  DEMglb[Result].SetGridElevation(x,y,what);
                  inc(Hist[round(what)]);
               end;
            end;
         end;
      end;
   end;

   SaveBackupDefaults;
   MDDef.MapNameBelowComposite := false;
   Maps := tStringList.Create;
   if ALOSBest <> 0 then FinalizeMap(ALOSbest,ALOSHist);
   if COPBest <> 0 then FinalizeMap(COPbest,COPHist);
   if FABBest <> 0 then FinalizeMap(FABbest,FABHist);
   FinalizeMap(Result,Hist,'');
   fName := NextFileNumber(MDtempDir,AName + '_rgb3_','.png');
   Bigimagewithallmaps(2,fName,Maps);
   RestoreBackupDefaults;
   {$IfDef DEMIXmaps} WriteLineToDebugFile('BestCopOrALOSmap out'); {$EndIf}
end;


unction BestCopOrALOSmap(RefDEM,ALOS,Cop : integer; Tolerance : float32; AName : shortString) : integer;
var
   i,What : integer;
   RefZ,CopZ,ALOSZ : float32;
   Lat,Long : float64;
   x,y : integer;
   VAT : tStringList;
   Hist : array[1..3] of int64;
begin
   {$IfDef DEMIXmaps} WriteLineToDebugFile('BestCopOrALOSmap in'); {$EndIf}
   for i := 1 to 3 do Hist[i] := 0;
   Result := DEMGlb[RefDEM].CloneAndOpenGridSetMissing(ByteDEM,AName,euIntCode);
   DEMglb[Result].SetEntireGridMissing;
   DEMglb[Result].AreaName := aName;
   StartProgressAbortOption('Best on map');
   for x := 0 to pred(DEMGlb[RefDEM].DEMheader.NumCol) do begin
      UpdateProgressBar(x/DEMGlb[RefDEM].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[RefDEM].DEMheader.NumRow) do begin
         if DEMGlb[RefDEM].GetElevMetersOnGrid(x,y,RefZ) then begin
            DEMGlb[RefDEM].DEMGridToLatLongDegree(x,y,Lat,Long);
            if DEMGlb[ALOS].GetElevFromLatLongDegree(Lat,Long,ALOSz) and DEMGlb[Cop].GetElevFromLatLongDegree(Lat,Long,CopZ) then begin
               ALOSz := abs(ALOSz - RefZ);
               Copz := abs(COPz - RefZ);
               if (ALOSZ <= Tolerance) and (COPz <= Tolerance) then What := 2
               else if ALOSZ < CopZ then What := 3
               else What := 1;
               DEMglb[Result].SetGridElevation(x,y,what);
               inc(Hist[round(what)]);
            end;
         end;
      end;
   end;
   Vat := tStringList.Create;
   Vat.add('VALUE,NAME,N,USE,COLOR');

   if (Hist[3] > 0) then Vat.add('3,ALOS best,' + IntToStr(Hist[3]) + ',Y,' + IntToStr(clBlue));
   if (Hist[2] > 0) then Vat.add('2,Both ± ' + RealToString(Tolerance,-5,1)  + ',' + IntToStr(Hist[2]) + ',Y,' + IntToStr(clYellow));
   if (Hist[1] > 0) then Vat.add('1,COP best,' + IntToStr(Hist[1]) + ',Y,' + IntToStr(clLime));
   StringList2CSVtoDB(vat,AName,true);
   DEMGlb[Result].VATFileName := AName;
   DEMglb[Result].CheckMaxMinElev;
   DEMglb[Result].SetupMap(true,mtDEMVATTable);
   {$IfDef DEMIXmaps} WriteLineToDebugFile('BestCopOrALOSmap out'); {$EndIf}
end;


function TwoDEMHighLowMap(RefDEM,ALOS,COP : integer; SimpleTolerance : float32; FourCats : boolean; fName2 : PathStr; ShowMap : boolean = true) : integer;
const
   MaxHist = 9;
   LongCatName : array[1..9] of shortstring = ('Both high','ALOS high/COP good','ALOS high/COP low',
                                                'ALOS good/COP high','Both good','ALOS good/COP low',
                                                'ALOS low/COP high','ALOS low/COP good','Both low');
   LongCatColor : array[1..9] of tColor = (clGreen,clBlue,clAqua,
                                           clYellow,clLime,clTeal,
                                           clPurple,clFuchsia,clRed);
var
   i : integer;
   zDEM2,zRefDEM,zDEM1,What : float32;
   Lat,Long : float64;
   x,y : integer;
   VAT : tStringList;
   Hist : array[1..MaxHist] of int64;
   DEM1high, DEM1low, DEM1good, DEM2high, DEM2low, DEM2good : boolean;
begin
   {$If Defined(RecordDEMCompare) or Defined(NewVATgrids) or Defined(DEMIXmaps)}
     WriteLineToDebugFile('TwoDEMHighLowMap in, REFDEM=' + IntToStr(RefDEM) + ' DEM1=' + IntToStr(ALOS) + ' and DEM2=' + IntToStr(COP) + '  ' + fName2);
   {$EndIf}
   Result := 0;
   if ValidDEM(RefDEM) and ValidDEM(ALOS) and ValidDEM(COP) then begin
      for i := 1 to MaxHist do Hist[i] := 0;
      Result := DEMGlb[RefDEM].CloneAndOpenGridSetMissing(ByteDEM,fName2,euIntCode);
      DEMGlb[Result].AreaName := fName2;
      StartProgressAbortOption('DEMs high/low');
      for x := 0 to pred(DEMGlb[RefDEM].DEMheader.NumCol) do begin
         UpdateProgressBar(x/DEMGlb[RefDEM].DEMheader.NumCol);
         for y := 0 to pred(DEMGlb[RefDEM].DEMheader.NumRow) do begin
            if DEMGlb[RefDEM].GetElevMetersOnGrid(x,y,zRefDEM) then begin
               DEMGlb[RefDEM].DEMGridToLatLongDegree(x,y,Lat,Long);
               if DEMGlb[ALOS].GetElevFromLatLongDegree(Lat,Long,zDEM1) and DEMGlb[COP].GetElevFromLatLongDegree(Lat,Long,zDEM2) then begin
                  DEM1high := (zDEM1 > zRefDEM + SimpleTolerance);
                  DEM1low :=  (zDEM1 < zRefDEM - SimpleTolerance);
                  DEM1good := abs(zDEM1 - zRefDEM) <= SimpleTolerance;     //(not DEM1high) and (not DEM1Low);

                  DEM2high := (zDEM2 > zRefDEM + SimpleTolerance);
                  DEM2low :=  (zDEM2 < zRefDEM - SimpleTolerance);
                  DEM2good := abs(zDEM2 - zRefDEM) <= SimpleTolerance;     //(not DEM2high) and (not DEM2Low);

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
      Vat.Add('VALUE,CATEGORY,N,USE,COLOR');
      if FourCats then begin
         if (Hist[5] > 0) then Vat.add('5,Both high,' + IntToStr(Hist[5]) + ',Y,' + IntToStr(clGreen));
         if (Hist[3] > 0) then Vat.add('3,Both reference ± ' + RealToString(SimpleTolerance,-5,1) + ',' + IntToStr(Hist[3]) + ',Y,' + IntToStr(clWhite));
         if (Hist[1] > 0) then Vat.add('1,Both low,' + IntToStr(Hist[1]) + ',Y,' + IntToStr(clRed));
         if (Hist[6] > 0) then Vat.add('6,Complex,' + IntToStr(Hist[6]) + ',Y,' + IntToStr(clYellow));
      end
      else begin
         LongCatName[5] := 'Both good ± ' + RealToString(SimpleTolerance,-5,1);
         for i := 1 to 9 do if (Hist[i] > 0) then Vat.add(IntToStr(i) + ',' + LongCatName[i] + ',' + IntToStr(Hist[i]) + ',Y,' + IntToStr(LongCatColor[i]));
      end;
      fName2 := NextFileNumber(MDTempDir,fName2 + '_','.vat.dbf');
      StringList2CSVtoDB(vat,fName2,true);
      DEMGlb[Result].VATFileName := fName2;
      DEMglb[Result].CheckMaxMinElev;
      if ShowMap then DEMglb[Result].SetupMap(true,mtDEMVATTable);
      {$If Defined(RecordDEMCompare) or Defined(NewVATgrids)} WriteLineToDebugFile('TwoDEMHighLowMap out, new grid=' + IntToStr(Result) + ' ' + fName2); {$EndIf}
   end
   else begin
      {$If Defined(RecordDEMCompare) or Defined(NewVATgrids)} HighlightLineToDebugFile('TwoDEMHighLowMap invalid input, ref=' + IntToStr(RefDEM) + ' cop=' + IntToStr(COP) + ' alos=' + IntToStr(ALOS)); {$EndIf}
   end;
end;


function AirBallDirtBallMap(DEMonMap,DSM,DTM : integer; fName : PathStr = '') : integer;
const
   Tolerance = 0.51;
   HighTolerance = 2.51;
   SimpleTolerance = 0.51;
   ThreeCat = true;
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
      Result := DEMGlb[DEMonMap].CloneAndOpenGridSetMissing(ByteDEM,fName,euIntCode);
      StartProgressAbortOption('Air or dirt');
      for x := 0 to pred(DEMGlb[DEMonMap].DEMheader.NumCol) do begin
         UpdateProgressBar(x/DEMGlb[DEMonMap].DEMheader.NumCol);
         for y := 0 to pred(DEMGlb[DEMonMap].DEMheader.NumRow) do begin
            if DEMGlb[DEMonMap].GetElevMetersOnGrid(x,y,z2) then begin
               DEMGlb[DEMonMap].DEMGridToLatLongDegree(x,y,Lat,Long);
                  if ((DSM = 0) or DEMGlb[DSM].GetElevFromLatLongDegree(Lat,Long,z3)) and DEMGlb[DTM].GetElevFromLatLongDegree(Lat,Long,z1) then begin
                     if (DSM = 0) then z3 := z1;
                     if (DTM = 0) then z1 := z3;
                     if ThreeCat then begin
                        if z2 > z3 + SimpleTolerance then What := 5
                        else if z2 < z1 - SimpleTolerance then What := 1
                        else What := 3;
                     end
                     else begin
                        if z2 > z3 + HighTolerance then What := 5
                        else if z2 > z3 + Tolerance then What := 4
                        else if z2 > z1 - Tolerance then What := 3
                        else if z2 > z1 - HighTolerance then What := 2
                        else What := 1;
                     end;
                     DEMglb[Result].SetGridElevation(x,y,what);
                     inc(Hist[round(what)]);
                  end;
            end;
         end;
      end;
      Vat := tStringList.Create;
      Vat.add('VALUE,NAME,N,USE,COLOR');

      if (DSM = 0) then TStr := 'DTM' else if (DTM = 0) then TStr := 'DSM' else TStr := 'Canopy';
      if ThreeCat then begin
         if (Hist[5] > 0) then Vat.add('5,Air ball,' + IntToStr(Hist[5]) + ',Y,' + IntToStr(clBlue));
         if (Hist[3] > 0) then Vat.add('3,' + TStr + ' ± ' + RealToString(SimpleTolerance,-5,1)  + ',' + IntToStr(Hist[3]) + ',Y,' + IntToStr(clLime));
         if (Hist[1] > 0) then Vat.add('1,Ground ball,' + IntToStr(Hist[1]) + ',Y,' + IntToStr(clBrown));  //238,138,248)));
      end
      else begin
         if (Hist[5] > 0) then Vat.add('5,High Air ball,' + IntToStr(Hist[5]) + ',Y,' + IntToStr(clBlue));
         if (Hist[4] > 0) then Vat.add('4,Low Air ball + ' + RealToString(HighTolerance,-5,1) + ',' + IntToStr(Hist[4]) + ',Y,' + IntToStr(RGB(0,255,255)));
         if (Hist[3] > 0) then Vat.add('3,Canopy ± ' + RealToString(Tolerance,-5,1) + ',' + IntToStr(Hist[3]) + ',Y,' + IntToStr(clLime));
         if (Hist[2] > 0) then Vat.add('2,Shallow Ground ball -' + RealToString(HighTolerance,-5,1) + ',' + IntToStr(Hist[2]) + ',Y,' + IntToStr(RGB(240,134,80)));
         if (Hist[1] > 0) then Vat.add('1,Deep Ground ball,' + IntToStr(Hist[1]) + ',Y,' + IntToStr(RGB(238,138,248)));
      end;
      fName := MDTempDir + fName + '.vat.dbf';
      StringList2CSVtoDB(vat,fName,true);
      DEMGlb[Result].VATFileName := fName;
      DEMglb[Result].CheckMaxMinElev;
      DEMglb[Result].SetupMap(true,mtDEMVATTable);
   end;
end;







