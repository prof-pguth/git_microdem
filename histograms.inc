{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for demstat        }
{___________________________________}



function CreateGridHistograms(DEMSwanted : tDEMbooleanArray; ParamName : shortstring = ''; TailCutoff : float32 = 0.0) : TThisBaseGraph;
var
   j : integer;
   Distributions,Legends : tStringList;
   Values : ^bfarray32;
   Max,Min,BinSize,ThisMin,ThisMax : float32;
   NPts : int64;
   fName : PathStr;
begin
   {$IfDef RecordHistogram} WriteLineToDebugFile('CreateGridHistograms in, Tail cutoff=' + RealToString(TailCutoff,-8,-2) + '%'); {$EndIf}
   if (TailCutoff < 0) then ReadDefault('Tail cutoff (%)',TailCutoff);
   Distributions := tStringList.Create;
   Legends := tStringList.Create;
   Max := -99e39;
   Min := 99e39;
   for j := 1 to MaxDEMDataSets do begin
      if DEMsWanted[j] and ValidDEM(j) then begin
         New(Values);
         DEMGlb[j].GetElevationsInLongArray(DEMGlb[j].FullDEMGridLimits,NPts,Values^);
         ThisMax := Values^[pred(round(NPts * (100 - TailCutoff) / 100))];
         ThisMin := Values^[round(NPts * TailCutoff / 100)];
         {$IfDef RecordHistogram}
            WriteLineToDebugFile('DEM header, Min=' + RealToString(DEMglb[j].DEMheader.MinElev,-12,-6) + '  max=' + RealToString(DEMglb[j].DEMheader.MaxElev,-12,-6) + ' DEM=' + DEMglb[j].AreaName);
            WriteLineToDebugFile('Grid, Min=' + RealToString(Values^[0],-12,-6) + '  max=' + RealToString(Values^[pred(Npts)],-12,-6) + ' DEM=' + DEMglb[j].AreaName);
            WriteLineToDebugFile('Less tails, Min=' + RealToString(ThisMin,-12,-6) + '  max=' + RealToString(ThisMax,-12,-6) + ' DEM=' + DEMglb[j].AreaName);
            //WriteLineToDebugFile('Percentile, Min=' + RealToString(DEMglb[j].FindPercentileElevation(TailCutoff),-12,-6) + '  max=' + RealToString(DEMglb[j].FindPercentileElevation(100-TailCutoff),-12,-6) + ' DEM=' + DEMglb[j].AreaName);
         {$EndIf}
         if (ThisMax > Max) then Max := ThisMax;
         if (ThisMin < Min) then Min := ThisMin;
         {$IfDef RecordHistogram}
            WriteLineToDebugFile('Cumulative, Min=' + RealToString(Min,-12,-6) + '  max=' + RealToString(Max,-12,-6)) ;
            WriteLineToDebugFile('');
         {$EndIf}
         fName := Petmar.NextFileNumber(MDtempDir,DEMGlb[j].AreaName + '_' ,'.z');
         Distributions.Add(SaveSingleValueSeries(npts,Values^,fName));
         Legends.Add(DEMGlb[j].AreaName);
         Dispose(Values);
      end;
   end;
   BinSize := (Max - Min) / 200;
   {$IfDef RecordHistogram} WriteLineToDebugFile('Min=' + RealToString(Min,-12,-6) + '  max=' + RealToString(Max,-12,-6) + ' DEMs=' + IntToStr(Distributions.Count)); {$EndIf}
   Result := CreateMultipleHistogram(MDDef.CountHistograms,Distributions,Legends,ParamName, 'DEM/Grid histograms',200,Min,Max,BinSize);
   {$IfDef RecordHistogram} WriteLineToDebugFile('CreateGridHistograms out'); {$EndIf}
end;


procedure HistogramsFromVATDEM(DEMwithVAT,ElevMap,SlopeMap,RuffMap,AspMap : integer; var Graph1,Graph2,Graph3,Graph4 : tThisBaseGraph);
//creates histograms of elevation, slope, roughness, and aspect for each category in VAT grid
const
   MaxCodes = 100;
Type
   tDistCount = array[1..MaxCodes] of integer;
   pbfarray32 = ^bfarray32;
   tDistArray = array[1..MaxCodes] of pbfarray32;
   tNames = array[1..MaxCodes] of shortstring;
var
   VAT : integer;
   i,j,NumCodes : integer;
   Codes : array[1..MaxCodes] of integer;
   Names : tNames;
   Colors : tStringList;
   CountField : shortstring;
   ElevDist,SlopeDist,RuffDist,AspDist : tDistArray;
   ElevDistCount,SlopeDistCount,RuffDistCount,AspDistCount : tDistCount;

   procedure CreateDistribution;
   var
      Col,Row,ThisCode,i : integer;
      zf : float32;
      Lat,Long : float64;

            function MakeOneHistogram(DEM : integer; BinSize : float32; Names : tNames;  DistCount : tDistCount; Dist : tDistArray; aTitle : shortstring; Max : float32) : tThisBaseGraph;
            var
               i : integer;
               fName : PathStr;
               ElevFiles,LegendFiles : tStringList;
            begin
               if ValidDEM(DEM) then begin
                  {$IfDef RecordHistogramFromVAT} HighlightLineToDebugFile(aTitle + '  MakeOneHistogram in, dem=' + IntToStr(DEM)); {$EndIf}
                  ElevFiles := tStringList.Create;
                  LegendFiles := tStringList.Create;
                  for i := 1 to NumCodes do begin
                     if (DistCount[i] > 0) then begin
                        fName := NextFileNumber(MDTempDir,'dist_file_','.z');
                        SaveSingleValueSeries(DistCount[i],Dist[i]^,fName);
                        ElevFiles.Add(fName);
                        LegendFiles.Add(Names[i]);
                        {$IfDef RecordHistogramFromVAT} HighlightLineToDebugFile(Names[i] + '  ' + Colors[pred(i)] + '  ' + fName); {$EndIf}
                     end;
                  end;
                  Result := CreateMultipleHistogram(MDDef.CountHistograms,ElevFiles,LegendFiles,BeforeSpecifiedCharacterANSI(aTitle,' '),aTitle,100,DEMglb[DEM].DEMheader.MinElev,Max,BinSize,Colors);
               end;
            end;

   begin
      {$IfDef RecordHistogramFromVAT} WriteLineToDebugFile('Create Distribution in'); {$EndIf}
      for Col := 0 to pred(DEMGlb[DEMwithVAT].DEMheader.NumCol) do begin
         for Row := 0 to pred(DEMGlb[DEMwithVAT].DEMheader.NumRow) do begin
            if DEMGlb[DEMwithVAT].GetElevMetersOnGrid(Col,Row,zf) then begin
              ThisCode := round(zf);
               for I := 1 to succ(NumCodes) do begin
                  if (i = succ(NumCodes)) then ThisCode := 0;
                  
                  if ThisCode = Codes[i] then begin
                     ThisCode := i;
                     break;
                  end;
               end;

               if (ThisCode > 0) then begin
                  DEMGlb[DEMwithVAT].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                  if ValidDEM(ElevMap) and (DEMGlb[ElevMap].GetElevFromLatLongDegree(Lat,long,zf)) then begin
                     inc(ElevDistCount[ThisCode]);
                     ElevDist[ThisCode]^[ElevDistCount[ThisCode]] := zf;
                  end;
                  if ValidDEM(SlopeMap) and (DEMGlb[SlopeMap].GetElevFromLatLongDegree(Lat,long,zf)) then begin
                     inc(SlopeDistCount[ThisCode]);
                     SlopeDist[ThisCode]^[SlopeDistCount[ThisCode]] := zf;
                  end;
                  if ValidDEM(RuffMap) and (DEMGlb[RuffMap].GetElevFromLatLongDegree(Lat,long,zf)) then begin
                     inc(RuffDistCount[ThisCode]);
                     RuffDist[ThisCode]^[RuffDistCount[ThisCode]] := zf;
                  end;
                  if ValidDEM(AspMap) and (DEMGlb[AspMap].GetElevFromLatLongDegree(Lat,long,zf)) then begin
                     inc(AspDistCount[ThisCode]);
                     AspDist[ThisCode]^[AspDistCount[ThisCode]] := zf;
                  end;
               end;
            end;
         end;
      end;

      MDDef.HistElevBinSize := 20;
      MDDef.HistSlopeBinSize := 1;
      MDDef.HistRuffBinSize  := 0.5;
      MDDef.HistAspectBinSize := 2;

      MDDef.DoElevHist := true;
      MDDef.DoSlopeHist := true;
      MDDef.DoRuffHist := true;
      MDDef.DoAspectHist := true;

      if MDDef.DoElevHist then Graph1 := MakeOneHistogram(ElevMap,MDDef.HistElevBinSize,Names,ElevDistCount,ElevDist,'Elevation (m)',DEMglb[ElevMap].DEMheader.MaxElev);
      if MDDef.DoSlopeHist then Graph2 := MakeOneHistogram(SlopeMap,MDDef.HistSlopeBinSize,Names,SlopeDistCount,SlopeDist,'Slope (%)',100);
      if MDDef.DoRuffHist then Graph3 := MakeOneHistogram(RuffMap,MDDef.HistRuffBinSize,Names,RuffDistCount,RuffDist,'Roughness (%)',100);
      if MDDef.DoAspectHist then Graph4 := MakeOneHistogram(AspMap,MDDef.HistAspectBinSize,Names,AspDistCount,AspDist,'Aspect (°)',360);
   end;


const
   PtRequiredInCat = 2500;
var
   Sum : float64;
begin
   {$IfDef RecordHistogramFromVAT} WriteLineToDebugFile('HistogramsFromVATDEM in, DEM=' + IntToStr(DEMwithVAT)); {$EndIf}
   if not ValidDEM(DEMwithVAT) then exit;
   if OpenNumberedGISDataBase(VAT,DEMGlb[DEMwithVAT].VATFileName) then begin
      if (not GISdb[VAT].MyData.FieldExists(GISdb[VAT].dbOpts.LabelField)) then GISdb[VAT].dbOpts.LabelField := GISdb[VAT].PickField('label field',[ftstring],true);
      CountField := 'COUNT';
      if (not GISdb[VAT].MyData.FieldExists(CountField)) then CountField := 'N';
      Sum := GISdb[VAT].MyData.FieldSum(CountField);
      GISdb[VAT].ApplyGISFilter(CountField + '>=' + IntToStr(PtRequiredInCat) );
      NumCodes := 0;
      Colors := tStringList.Create;
      while not GISdb[VAT].MyData.eof do begin
         inc(NumCodes);
         Codes[NumCodes] := GISdb[VAT].MyData.GetFieldByNameAsInteger('VALUE');
         Colors.Add(IntToStr(GISdb[VAT].MyData.TColorFromTable));
         Names[NumCodes] := RealToString(100 * GISdb[VAT].MyData.GetFieldByNameAsInteger(CountField) / Sum, 8,2) + '%   ' + GISdb[VAT].MyData.GetFieldByNameAsString(GISdb[VAT].dbOpts.LabelField);
        {$IfDef RecordHistogramFromVAT} WriteLineToDebugFile(IntToStr(NumCodes) + IntegerToString(Codes[NumCodes],8) + '  ' + Names[NumCodes]); {$EndIf}
         GISdb[VAT].MyData.Next;
         if (NumCodes >= MaxCodes) then begin
            MessageToContinue('Reached number of codes limit=' + IntToStr(MaxCodes));
            Break;
         end;
      end;
      GISdb[VAT].MyData.Destroy;

      for i := 1 to MaxCodes do begin
         if ValidDEM(ElevMap) then new(ElevDist[i]);
         if ValidDEM(SlopeMap) then new(SlopeDist[i]);
         if ValidDEM(RuffMap) then new(RuffDist[i]);
         if ValidDEM(AspMap) then new(AspDist[i]);
         ElevDistCount[i] := 0;
         SlopeDistCount[i] := 0;
         RuffDistCount[i] := 0;
         AspDistCount[i] := 0;
      end;

      CreateDistribution;

      for i := 1 to MaxCodes do begin
         if ValidDEM(ElevMap) then Dispose(ElevDist[i]);
         if ValidDEM(SlopeMap) then Dispose(SlopeDist[i]);
         if ValidDEM(RuffMap) then Dispose(RuffDist[i]);
         if ValidDEM(AspMap) then Dispose(AspDist[i]);
      end;
      Colors.Destroy;
   end;
   {$IfDef RecordHistogramFromVAT} WriteLineToDebugFile('HistogramsFromVATDEM out'); {$EndIf}
end;



procedure DoElevationHistograms;
var
   CurDEM,j : integer;
begin
   DEM_Hist_Opts.SetHistogramOptions;
   if MDDef.GeomorphAllDEMs then begin
      for j := 1 to MaxDEMDataSets do if (DEMGlb[j] <> Nil) then SingleDEMHistogram(j);
   end
   else begin
      GetDEM(CurDEM);
      SingleDEMHistogram(CurDEM);
   end;
end;



procedure SingleDEMHistogram(WhichDEM : integer; Quick : boolean = false);
const
   HistRange = 64000;
type
   tHistArray = array[-HistRange..HistRange] of int64;
var
   i,j,x,y,theDB,NumBins : integer;
   TotalPts : int64;
   MinBin,MaxBin,PC,CumPC : float64;
   z : float32;
   fName : PathStr;
   TStr : shortstring;
   GridLimits : tGridLimits;
   Results : tStringList;
   Hist : ^tHistArray;
begin
   if Not ValidDEM(WhichDEM) then exit;

   {$IfDef RecordHistogram} WriteLineToDebugFile('SingleDEMHistogram for ' + DEMGlb[WhichDEM].AreaName); {$EndIf}
   if (DEMGlb[WhichDEM].SelectionMap = nil) then GridLimits := DEMGlb[WhichDEM].FullDEMGridLimits

   else GridLimits := DEMGlb[WhichDEM].SelectionMap.GridLimitsForGeomorphOps;
   New(Hist);
   for I := -HistRange to HistRange do Hist^[i] := 0;
   TotalPts := 0;
   x := GridLimits.XGridLow;
   while (x <= GridLimits.XGridHigh) do begin
       y := GridLimits.YGridLow;
       while (y <= GridLimits.YGridHigh) do begin
           if DEMGlb[WhichDEM].GetElevMeters(x,y,z) then begin
              inc(Hist^[round(z / MDDef.HistBinSize)]);
              inc(TotalPts);
           end;
           inc(y,MDDef.StatSampleIncr);
       end {while y};
       inc(x,MDDef.StatSampleIncr);
   end {while x};
   {$IfDef RecordHistogram} WriteLineToDebugFile('Total done'); {$EndIf}
   NumBins := 0;
   MinBin := 99e38;
   for I := -HistRange to HistRange do begin
      if Hist^[i] > 0 then begin
         inc(NumBins);
         if MinBin > 98e38 then MinBin := MDDef.HistBinSize * i;
         MaxBin := MDDef.HistBinSize * i;
      end;
   end;
   Results := tStringList.Create;
   Results.Add('Z,NPTS,PERCENTAGE,CUM_PC,CONCENT,PROP_HGT,PROP_AREA,REC_ID');
   CumPC := 0;
   j := 0;
   for I := -HistRange to HistRange do begin
      if Hist^[i] > 0 then begin
         pc := 100 * Hist^[i] / TotalPts;
         inc(j);
         CumPc := CumPC + PC;
         Results.Add(RealToString(MDDef.HistBinSize * i,-18,-4) + ',' + IntToStr(Hist^[i]) + ',' + RealToString(pc,-12,-5) +
            ',' + RealToString(CumPC,-12,-5)  + ',' + RealToString(Hist^[i] / (TotalPts / NumBins),-12,-4) +
            ',' + RealToString(((MDDef.HistBinSize * i) -MinBin)/(MaxBin - MinBin),-12,-5) +  ',' + RealToString(1 - 0.01 * CumPC,-12,-5) + ',' + IntToStr(j) );
      end;
   end;
   Dispose(Hist);

    fName := Petmar.NextFileNumber(MDTempDir,DEMGlb[WhichDEM].AreaName + '_Hist_',DefaultDBExt);
    {$IfDef RecordHistogram} WriteLineToDebugFile('Convert and load table'); {$EndIf}

    StringList2CSVtoDB(Results,fName,true);
    theDB := DEMGlb[WhichDEM].SelectionMap.LoadDataBaseFile(fName);

   if DEMGlb[WhichDEM].DEMheader.ElevUnits = euImagery then TStr := 'DNs '
   else TStr := 'Elevation ';
   if Quick or MDDef.ShowRegularHistogram then begin
      if MDDef.CountHistograms then GISDB[theDB].CreateScatterGram('CONCENT','Z',clRed,true,DEMGlb[WhichDEM].AreaName +  ' ' + TStr + ' Histogram',
           'Concentration (Fraction of Uniform)',  TStr + '(' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')')
      else begin
         if (DEMGlb[WhichDEM].DEMheader.ElevUnits = euImagery) then
              GISDB[theDB].CreateScatterGram('Z','NPTS',clRed,true,DEMGlb[WhichDEM].AreaName +  ' ' + TStr + 'Histogram', TStr + '(' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')','Number of Points')
         else GISDB[theDB].CreateScatterGram('NPTS','Z',clRed,true,DEMGlb[WhichDEM].AreaName +  ' ' + TStr + 'Histogram','Number of Points', TStr + '(' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')');
      end;
   end;

   if Quick then begin
      CloseAndNilNumberedDB(theDB);
   end
   else begin
      if MDDef.ShowCumulativeHistogram then GISDB[theDB].CreateScatterGram('CUM_PC','Z',clRed,true,DEMGlb[WhichDEM].AreaName +  ' Cumulative Elevation Distribution',
          'Cumulative Percentage', 'Elevation (' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')');

      if MDDef.ShowStrahlerHistogram then GISDB[theDB].CreateScatterGram('PROP_AREA','PROP_HGT',clRed,true,DEMGlb[WhichDEM].AreaName +  ' Strahler Elevation Distribution',
       'Proportion of Area', 'Proportion of Basin Height');

       if MDDef.ShowNormalHistogram then begin
          GISDB[theDB].CreateScatterGram('Z','CUM_PC',clRed,true,DEMGlb[WhichDEM].AreaName +  ' Cumulative ' + TStr + 'Distribution',
            TStr + '(' + ElevUnitsAre(DEMGlb[WhichDEM].DEMheader.ElevUnits)  + ')','Cumulative Percentage',true);
       end;

       if MDDef.ShowHistogramText then begin
          Results := tStringList.Create;
          Results.Add(DEMGlb[WhichDEM].AreaName + '  ' + TStr);
          Results.Add('Range: '+ RealToString(DEMGlb[WhichDEM].DEMheader.MinElev,-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].DEMheader.MaxElev,-8,-2));
          Results.Add('Range (0.1 to 99.9%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(0.1),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(99.9),-8,-2));
          Results.Add('Range (0.2 to 99.8%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(0.2),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(99.8),-8,-2));
          Results.Add('Range (0.5 to 99.5%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(0.5),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(99.5),-8,-2));
          Results.Add('Range (1.0 to 99.0%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(1.0),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(99.0),-8,-2));
          Results.Add('Range (2.0 to 98.0%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(2.0),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(98.0),-8,-2));
          Results.Add('Range (5.0 to 95.0%): '+ RealToString(DEMGlb[WhichDEM].FindPercentileElevation(5.0),-8,-2) + ' to ' +  RealToString(DEMGlb[WhichDEM].FindPercentileElevation(95.0),-8,-2));
          PETMAR.DisplayAndPurgeStringList(Results,'Histogram results');
       end;
   end;
  {$IfDef RecordHistogram} WriteLineToDebugFile('SingleDEMHistogram out'); {$EndIf}
end;

function CreatePartDEMHistogram(DEM : integer; GridLimits: tGridLimits) : TThisBaseGraph;
var
   Distributions,Legends : tStringList;
   Values : ^bfarray32;
   Max,Min,BinSize : float32;
   NPts : int64;
   fName : PathStr;
begin
   {$IfDef RecordDEMstats} WriteLineToDebugFile('tDEMDataSet.CreatePartDEMHistogramn in'); {$EndIf}
   Distributions := tStringList.Create;
   Legends := tStringList.Create;
   New(Values);
   DEMGlb[DEM].GetElevationsInLongArray(GridLimits,NPts,Values^);
   Max := Values^[pred(NPts)];
   Min := Values^[0];
   fName := Petmar.NextFileNumber(MDtempDir,DEMGlb[DEM].AreaName + '_' ,'.z');
   Distributions.Add(SaveSingleValueSeries(npts,Values^,fName));
   Legends.Add(DEMGlb[DEM].AreaName);
   Dispose(Values);
   BinSize := (Max - Min) / 200;
   Result := CreateMultipleHistogram(MDDef.CountHistograms,Distributions,Legends,ElevUnitsAre(DEMGlb[DEM].DEMHeader.ElevUnits),DEMGlb[DEM].AreaName + ' histogram',200,Min,Max,BinSize);
   {$IfDef RecordDEMstats} WriteLineToDebugFile('tDEMDataSet.CreatePartDEMHistogramn out'); {$EndIf}
end;


function CreateWholeDEMHistogram(DEM : integer) : TThisBaseGraph;
begin
   Result := CreatePartDEMHistogram(DEM,DEMGlb[DEM].FullDEMGridLimits);
end;
