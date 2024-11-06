{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{   include file for demcoord.pas    }
{____________________________________}





         procedure tDEMDataSet.DoSSOStrip(Limits : tGridLimits; var Results : tStringList);
         var
            s1,s2,S3 : ShortString;
            aLine : ANSIString;
            SSOvars : tSSOvars;
            Col,Row,xsize,ysize,ColInc,RowInc,Done : integer;
            {$IfDef FabricWavelengthAmplitude} WavelengthMean,WavelengthMedian,WavelengthStdDev,Lat1,Long1,Lat2,Long2,aMinVal,aMaxVal, {$EndIf}
            Lat,Long : float64;
            z : float32;
            TableName : PathStr;
         begin
             xsize := round(0.5 * MDDef.SSOBoxSizeMeters / AverageXSpace);
             ysize := round(0.5 * MDDef.SSOBoxSizeMeters / AverageYSpace);

             ColInc := round(MDDef.PointSeparation / AverageXSpace);
             RowInc := round(MDDef.PointSeparation / AverageYSpace);
             Results := tStringList.Create;
             {$IfDef FabricWavelengthAmplitude}
                if MDdef.DoFabricAmplitude then s1 := ',ELEV_M,FABRIC_AMP' else s1 := '';
                if MDDef.DoWavelength then s2 := ',LEN_MEAN,LEN_MEDIAN,LEN_STD,HT_MEAN,HT_MEDIAN,HT_STD' else s2 := '';
                if MDdef.DoFabricAmplitude then S3 := ',FABRIC_AMP' else s3 := '';
             {$EndIf}
             Results.Add('LAT,LONG,S1S2,S2S3,NPTS,RELIEF,FABRIC_DIR' + s1 + s2 + S3);
             Col := Limits.xGridLow;
             while Col <= Limits.xGridHigh do begin
                 Row := Limits.YGridLow;
                 while (Row <= Limits.YGridHigh) do begin
                    if (GetElevMeters(Col,Row,z)) and SSOComputations(SpecifyDEMGridLimits(Col-xsize,Row-ysize,Col+xsize,Row+Ysize),SSOvars,false,'','') then begin
                       if (SSOvars.s1s2 >= MDdef.FlatnessCutoff) then System.SyncObjs.TInterlocked.Increment(SSOfailFlat);
                       if (SSOvars.s2s3 < MDDef.OrganizationCutoff) then System.SyncObjs.TInterlocked.Increment(SSOfailorg);

                       if (MDDef.SSOallInTable) or ((SSOvars.s1s2 <= MDdef.FlatnessCutoff) and (SSOvars.s2s3 >= MDDef.OrganizationCutoff)) then begin
                           DEMGridToLatLongDegree(Col,Row,Lat,Long);
                           aline := RealToString(Lat,-12,-7) + ',' +
                                    RealToString(Long,-12,-7)+ ',' +
                                    RealToString(SSOvars.s1s2,-12,-3)+ ',' +
                                    RealToString(SSOvars.s2s3,-12,-3)+ ',' +
                                    IntToStr(SSOvars.NumPts)+ ',' +
                                    RealToString(SSOvars.ElevRange,-12,-2)+ ',' +
                                    RealToString(SSOvars.TheDipDirs[3],-12,-1);

                           {$IfDef FabricWavelengthAmplitude}
                              if (MDdef.DoFabricAmplitude or MDDef.DoWavelength) and GetPerpendicularLineEnd(Lat,Long,0.5*MDDef.FabricAmplitudeDistance,SSOvars.TheDipDirs[3],Lat1,Long1,Lat2,Long2) then begin
                                 MDDef.ForceCrestComputations := true;
                                 LOSComputeOnly(0,lat1,long1,lat2,long2,0,0);
                                 WantShowProgress := false;
                                 if MDdef.DoFabricAmplitude then begin
                                    if ProfileData.FindFieldRange('ELEV_M',aMinVal,aMaxVal) then
                                       aLine := aLine + ',' + RealToString(aMaxVal - aMinVal,-12,-2);
                                 end;
                                 if MDDef.DoWavelength then begin
                                   FindWavelengthStats(ProfileData,WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd);
                                   if (WaveLengthMean > 0.01) then  begin
                                      aline := aLine + ',' + RealToString(WavelengthMean,-12,-3)+ ',' +
                                               RealToString(WavelengthMedian,-12,-3)+ ',' +
                                               RealToString(WavelengthStdDev,-12,-3)+ ',' +
                                               RealToString(HeightMean,-12,-3)+ ',' +
                                               RealToString(HeightMedian,-12,-3)+ ',' +
                                               RealToString(HeightStd,-12,-3);
                                   end;
                                 end;
                                 WantShowProgress := true;
                              end;
                          {$EndIf}

                           Results.Add(aLine);
                       end;
                    end
                    else begin
                       System.SyncObjs.TInterlocked.Increment(SSOFailPts);
                    end;
                    inc(Row,RowInc);
                    if ShowSatProgress and WantOut then break;
                 end {while Row};
                inc(Col,ColInc);
                inc(Done);
                if ShowSatProgress and WantOut then break;
             end {while Col};
             if MDdef.DoWaveLength then begin
                DeleteFileIfExists(TableName);
             end;
         end;


         function tDEMDataSet.OrientationTable(fName : PathStr; UseMap : tMapForm) : integer;
         var
            Limits :  tGridLimits;
            Results : array[1..MaxThreadsAllowed] of tStringList;
            {$IfDef ParallelForOrientationTable}
               PartLimits : tGridLimitsArray;
               i,it : integer;
            {$EndIf}
         begin
            {$If Defined(ShowDEMSSOCalc) or Defined(RecordTime)} WriteLineToDebugFile('tDEMDataSet.OrientationTable in'); Stopwatch := TStopwatch.StartNew; {$EndIf}
             SetColorForProcessing;
             if (UseMap = Nil) or MDDef.GeomorphMapsFullDEM then Limits := FullDEMGridLimits
             else Limits := UseMap.MapDraw.MapAreaDEMGridLimits;

             SSOFailPts := 0;
             SSOFailFlat := 0;
             SSOfailOrg := 0;
            {$IfDef ParallelForOrientationTable}
                PartLimits := GetLimitsForParallelLoops(Limits);
                TParallel.For(1,MDdef.MaxThreadsForPC,
                      procedure (Value: Integer)
                      begin
                        DoSSOStrip(PartLimits[Value],Results[Value]);
                      end);

                 for it := 2 to MDdef.MaxThreadsForPC do begin
                    for I := 1 to pred(Results[it].Count) do
                       Results[1].Add(Results[it].Strings[i]);
                    Results[it].Free;
                 end;
                 ThreadsWorking := false;
            {$Else}
                {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('No parallel for, DOSSOstrip'); {$EndIf}
                DoSSOStrip(limits,Results[1]);
             {$EndIf}
             Result := UseMap.StringListToLoadedDatabase(Results[1],fName,false,true);
             GISdb[Result].dbOpts.dbAutoShow := dbasTerrainFabric;
             GISdb[Result].RedrawLayerOnMap;
             SetColorForWaiting;
            {$If Defined(ShowDEMSSOCalc) or Defined(RecordTime)} Elapsed := Stopwatch.Elapsed; WriteLineToDebugFile('OrientationTable, out: ' + RealToString(Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
         end;


         function tDEMDataSet.PointSSOComputations(Col,Row,FullBoxSizeMeters : integer; var SSOvars : tSSOvars; PlotResults,Complete,PlotAspectFreq : boolean) : boolean;
         var
            xsize,ysize : integer;
         begin
            {$IfDef ShowFullDEMSSOCalc)} WriteLineToDebugFile('tDEMDataSet.PointSSOComputations in: ' + IntToStr(Col) + ' & ' + IntToStr(Row)+ ' rad=' + IntToStr(FullBoxSizeMeters)); {$EndIf}
            xsize := round(0.5 * FullBoxSizeMeters / AverageXSpace);
            ysize := round(0.5 * FullBoxSizeMeters / AverageYSpace);
            Result := SSOComputations(SpecifyDEMGridLimits(Col-xsize,Row-ysize,Col+xsize,Row+Ysize),SSOvars,PlotResults,'','');
         end;


         function tDEMDataSet.SimplePointSSOComputations(PlotResults : boolean; Col,Row,FullBoxSizeMeters : integer; var s1s2,s2s3,Trend,RoughnessFactor : float64) : boolean;
         var
             SSOvars : tSSOvars;
         begin
            {$IfDef ShowFullDEMSSOCalc)} WriteLineToDebugFile('tDEMDataSet.SimplePointSSOComputations in: ' + IntToStr(Col) + ' & ' + IntToStr(Row) + ' rad=' + IntToStr(FullBoxSizeMeters)); {$EndIf}
            Result := PointSSOComputations(Col,Row,FullBoxSizeMeters,SSOvars,PlotResults,false,false);
            Trend := SSOvars.TheDipDirs[3];
            s1s2 := SSOvars.s1s2;
            s2s3 := SSOvars.s2s3;
            RoughnessFactor := SSOvars.RoughnessFactor;
         end;


         function tDEMDataSet.FeatureSSOComputations(inFeatureDEM,inFeatureID,LoX,LoY,HiX,HiY : integer; var s1s2,s2s3,Trend,RoughnessFactor,DownDip : float64) : boolean;
         var
             SSOvars : tSSOvars;
         begin
             {$IfDef ShowDEMSSOCalcFull} WriteLineToDebugFile('tDEMDataSet.FeatureSSOComputations in, Feature=: ' + IntToStr(inFeatureID)); {$EndIf}
             FilterGrid := inFeatureDEM;
             FilterGridValue := inFeatureID;
             Result := SSOComputations(SpecifyDEMGridLimits(LoX,LoY,HiX,HiY),SSOvars,true,'','');
             Trend := SSOvars.TheDipDirs[3];
             DownDip := SSOvars.TheDipDirs[1];
             s1s2 := SSOvars.s1s2;
             s2s3 := SSOvars.s2s3;
             RoughnessFactor := SSOvars.RoughnessFactor;
             FilterGrid := 0;
             FilterGridValue := 0;
         end;


         function tDEMDataSet.SSOByRegionSize(Col,Row : integer; var Maxs2s3,BoxSizeForMax,DirForMax,Relief : float32; var Results : tStringList) : boolean;
         var
            i,j,k,NRot,PtsForMax,NumPts : integer;
            DipDirect,Dip,Strike : float32;
            MaxZ,MinZ,s2s3,s1s2,StepRelief : float64;
            V : tTrendVector;
            M,S,tm  : ^tTrendMatrix;
            TStr : shortString;
            TheDipDirs : VectorType;


                  procedure PerformPointComputations(aCol,aRow : integer);
                  var
                     j,k : integer;
                     s2 : VectorType;
                     SlopAsp : tSlopeAspectRec;
                  begin
                     if GetSlopeAndAspect(aCol,aRow,SlopAsp) then begin
                        PetMath.CompareValueToExtremes(SlopAsp.z,MinZ,MaxZ);
                        inc(NumPts);
                        S2[1] := sinDeg(SlopAsp.SlopeDegree) * sinDeg(SlopAsp.AspectDir);
                        S2[2] := sinDeg(SlopAsp.SlopeDegree) * cosDeg(SlopAsp.AspectDir);
                        S2[3] := cosDeg(SlopAsp.SlopeDegree);
                        for j := 1 to 3 do
                           for k := 1 to 3 do
                              M^[j,k] := M^[j,k] + S2[j] * S2[k];
                     end;
                  end;

         var
            x,y,MinX,MaxX,MinY,MaxY,NewMinX,NewMaxX,NewMinY,NewMaxY,RegionM : integer;
            First : boolean;
            s2    : VectorType;
         begin
            {$If Defined(ShowDEMSSOCalc) or Defined(ShowFullDEMSSOCalc)} WriteLineToDebugFile('tDEMDataSet.SSOByRegionSize in, col=' + IntToStr(Col) + ' row'+IntToStr(Row)); {$EndIf}
            Result := false;
            First := true;
            try
               New(S);
               New(M);
               for j := 1 to 3 do
                  for k := 1 to 3 do
                     M^[j,k] := 0.0;

               New(tM);
               NumPts := 0;
               MinZ := MaxSmallInt;
               MaxZ := -MaxSmallInt;
               TStr := AreaName + TStr;
               MaxS2S3 := -99;

               if (Results <> Nil) then Results.Add('REGION_M,S2S3,FABRIC_DIR,FLATNESS,RELIEF,NPTS');

               RegionM := MDDef.FirstBoxSize;
               while (RegionM < MDDef.LastBoxSize) do begin
                  if First then begin
                     Minx := Col - round(0.5*MDDef.FirstBoxSize / AverageXSpace);
                     MaxX := Col + round(0.5*MDDef.FirstBoxSize / AverageXSpace);
                     MinY := Row - round(0.5*MDDef.FirstBoxSize / AverageYSpace);
                     MaxY := Row + round(0.5*MDDef.FirstBoxSize / AverageYSpace);
                     First := false;
                        x := Minx;
                        while x <= MaxX do begin
                           y := MinY;
                           while Y <= MaxY do begin
                              PerformPointComputations(Col,Row);
                              inc(y);
                           end {while Row};
                           inc(x);
                        end {while Col};
                  end
                  else begin
                     //strip on left of analysis region
                     NewMinX := Col - round(0.5*RegionM / AverageXSpace);
                     for y := MinY to MaxY do PerformPointComputations(NewMinX,y);
                     //new strip on right of analysis region
                     NewMaxX := Col + round(0.5*RegionM / AverageXSpace);
                     for y := MinY to MaxY do PerformPointComputations(NewMAxX,y);

                     //new strips on Bottom
                     NewMinY := Row - round(0.5*RegionM / AverageYSpace);
                     if NewMinY <> MinY then begin
                        for y := NewMinY to pred(MinY) do
                           for x := NewMinX to NewMaxX do PerformPointComputations(x,y);
                     end;
                     //new strips on top
                     NewMaxY := Row + round(0.5*RegionM / AverageYSpace);
                     if NewMaxY <> MaxY then begin
                        for y := NewMinY to pred(MinY) do
                           for x := NewMinX to NewMaxX do PerformPointComputations(x,y);
                     end;
                     Minx := NewMinX;
                     MaxX := NewMaxX;
                     MinY := NewMinY;
                     MaxY := NewMaxY;
                  end;

                  if (NumPts >= MDDef.MinPointsForSSO) then begin
                     Result := true;
                     for j := 1 to 3 do
                        for k := 1 to 3 do
                           tM^[j,k] := M^[j,k];

                     Jacobi(tM^,3,V,s^,NRot);
                     Eigsrt(V,S^,3);
                     if abs(v[3]) < 0.00001 then begin
                        s2s3 := 0;
                     end
                     else begin
                        s2s3 := ln(v[2]/v[3]);
                        s1s2 := ln(v[1]/v[2]);
                     end;
                     for i := 1 to 3 do begin
                        S2[1] := s[1,i];
                        S2[2] := s[2,i];
                        S2[3] := s[3,i];
                        CartesianToDipStrike(s2,Dip,Strike);
                        if (Dip < 0) then begin
                          Dip := abs(Dip);
                          DipDirect := Strike - 90;
                        end
                        else DipDirect := Strike + 90;
                        DipDirect := FindCompassAngleInRange(DipDirect);
                        TheDipDirs[i] := DipDirect;
                     end {for i};

                     if (s2s3 > Maxs2s3) and (s2s3 > 0.001) then begin
                        BoxSizeForMax := RegionM;
                        DirForMax := TheDipDirs[3];
                        Maxs2s3 := s2s3;
                        PtsForMax := NumPts;
                        Relief := MaxZ - MinZ;
                     end;
                     if (s2s3 > 0.001) then begin
                        StepRelief := MaxZ - MinZ;
                        TStr := IntToStr(round(RegionM)) + ',' + RealToString(s2s3,-8,-3) + ',' + RealToString(TheDipDirs[3],-8,-1) + ',' + RealToString(s1s2,-8,-3) + ',' + RealToString(StepRelief,-8,-2);
                        if (Results <> Nil) then Results.Add(TStr);
                        {$If Defined(ShowDEMSSOCalc) or Defined(ShowFullDEMSSOCalc)} WriteLineToDebugFile(TStr + '  NPTS=' + IntToStr(NumPts)); {$EndIf}
                     end;
                  end;
                  RegionM := RegionM + round(2 * AverageXSpace);
               end;
            finally
              Dispose(M);
              Dispose(tm);
              Dispose(S);
            end;

            {$If Defined(ShowDEMSSOCalc) or Defined(ShowFullDEMSSOCalc)}
               if Result then WriteLineToDebugFile('tDEMDataSet.SSObyregionsize out, Boxsize=' + IntToStr(round(BoxSizeForMax)) + '  org=' +  RealToString(Maxs2s3,-8,-3) +  '   PTs used=' + IntToStr(PtsForMax))
               else WriteLineToDebugFile('tDEMDataSet.SSObyregionsize out, No result');
            {$EndIf}
         end;


         function tDEMDataSet.SSOComputations(GridLimits : tGridLimits; var SSOvars : tSSOvars; PlotResults : boolean; NetName,AspName : PathStr; UseMinSlope : float64 = 0; UseMaxSlope : float64 = 9999) : boolean;
         var
            xd,yd,y,Col,Row  : integer;
            MaxZ,MinZ,SumElev,SumSlope,
            SumSqSlope,SumSqElev : float64;
            zf : float32;
            Symbol : tFullSymbolDeclaration;
            TStr : shortString;
            SlopeAspectRec : tSlopeAspectRec;
            SSOVarAnalysis : tSSOVarAnalysis;
            AspectStats : tAspectStats;
         begin
            {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.SSOComputations in, ' + GridLimitsToString(GridLimits) + ' plot=' + IntToStr(Ord(MDDef.SSObyPole))); {$EndIf}
            Result := false;
            try
               AspectStats.Create(ThisDEM);
               SSOVars.AspPts := 0;
               SSOvars.NumPts := 0;
               SSOVars.MaxSlope := 0;
               SumSlope := 0;
               SumElev := 0;
               SumSqSlope := 0;
               SumSqElev := 0;
               MinZ := MaxSmallInt;
               MaxZ := -MaxSmallInt;

               if (UseMaxSlope - UseMinSlope) < 90 then TStr := ' slopes ' + round(UseMinSlope).ToString + ' to ' + round(UseMaxSlope).ToString
               else TStr := '';
               TStr := AreaName + TStr;

               if PlotResults and MDDef.GemorphSSOPoles then begin
                  MDDef.NetDef.NetScreenMult := 2;
                  NetForm := TNetForm.Create(Application);
                  NetForm.Caption := 'SSO Diagram, ' + TStr;
                  NetForm.nd.ReallyPlot := false;
               end;
               {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('SSO setup done'); {$EndIf}

               SSOVarAnalysis.Create(ThisDEM);
               SSOVarAnalysis.SSOVars.NumPts := 0;
               SSOVarAnalysis.NormalsInBox(GridLimits);
               SSOVarAnalysis.ComputeEigenVectors;
               SSOVars := SSOVarAnalysis.SSOVars;
               SSOVarAnalysis.Destroy;

               {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('SSOVarAnalysis done'); {$EndIf}

               Col := GridLimits.XGridLow;
               while Col <= GridLimits.XGridHigh do begin
                  Row := GridLimits.YGridLow;
                  while Row <= GridLimits.YGridHigh do begin
                     if (FilterGrid = 0) or (GetElevMetersFromSecondDEM(false,FilterGrid,Col,Row,zf) and (round(zf) = FilterGridValue)) then begin
                        if GetSlopeAndAspect(Col,Row,SlopeAspectRec) then begin
                           if (SlopeAspectRec.SlopeDegree> SSOvars.MaxSlope) then SSOvars.MaxSlope := SlopeAspectRec.SlopeDegree;
                           if (SlopeAspectRec.SlopeDegree >= UseMinSlope) and (SlopeAspectRec.SlopeDegree <= UseMaxSlope) then begin
                              PetMath.CompareValueToExtremes(SlopeAspectRec.z,MinZ,MaxZ);
                              SumSlope := SumSlope + SlopeAspectRec.Slope;
                              SumSqSlope := SumSqSlope + Sqr(SlopeAspectRec.Slope);
                              SumElev := SumElev + SlopeAspectRec.z;
                              SumSqElev := SumSqElev + Sqr(SlopeAspectRec.z);
                              AspectStats.AddPoint(SlopeAspectRec);
                              inc(SSOVars.AspPts);
                              inc(SSOvars.NumPts);
                              if PlotResults and MDDef.GemorphSSOPoles then begin
                                 Symbol.DrawingSymbol := Dot;
                                 Symbol.Size := 3;
                                 Symbol.Color := claRed;
                                 if MDDef.SSObyPole then NetForm.nd.PlotPointOnNet(PolePlot,SlopeAspectRec.SlopeDegree,SlopeAspectRec.AspectDir,Symbol,xd,yd)
                                 else NetForm.nd.PlotPointOnNet(LinePlot,SlopeAspectRec.SlopeDegree,SlopeAspectRec.AspectDir,Symbol,xd,yd);
                              end;
                           end;
                        end;
                     end;
                     inc(Row,MDDef.SSOSampleIncr);
                  end {while Row};
                  inc(Col,MDDef.SSOSampleIncr);
                  if WantOut then break;
               end {while Col};
               {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('Vectors done, npts=' + IntToStr(SSOvars.NumPts)); {$EndIf}

               SSOvars.ElevRange := MaxZ - MinZ;

               if (SSOvars.NumPts > 8) then begin
                  SSOvars.StdDevSlope := SumSqSlope * SSOvars.NumPts - SumSlope * SumSlope;
                  if SSOvars.StdDevSlope < 0 then SSOvars.StdDevSlope := 0
                  else SSOvars.StdDevSlope := 100 * (sqrt(SSOvars.StdDevSlope / (1.0 * SSOvars.NumPts * pred(SSOvars.NumPts))));
                  SSOvars.AvgElev := SumElev / SSOvars.NumPts;
                  SSOvars.StdDevElev := (SumSqElev * SSOvars.NumPts - SumElev * SumElev);
                  if SSOvars.StdDevElev < 0 then SSOvars.StdDevElev := 0
                  else SSOvars.StdDevElev := sqrt(SSOvars.StdDevElev / (1.0 * SSOvars.NumPts * pred(SSOvars.NumPts)));
               end;

               if (SSOVars.AspPts > 1) then begin
                  AspectStats.VectorAverage;
                  SSOVars.AvgAspect := AspectStats.AvgAspectDir;
                  SSOVars.AspectStrength := AspectStats.AvgAspectMag;
               end
               else begin
                  SSOVars.AvgAspect := -99;
                  SSOVars.AspectStrength := -99;
               end;

               if (SSOvars.NumPts >= MDDef.MinPointsForSSO) then begin
                  Result := true;
                  SSOvars.QueensAspect := AspectStats.QueensAspect;
                  SSOvars.AvgSlope := (100*SumSlope/SSOvars.NumPts);
                  SSOvars.RoughnessFactor := 1 - sqrt(sqr(SSOvars.x1sq) + sqr(SSOvars.y1sq) + sqr(SSOvars.z1sq)) / SSOvars.NumPts;   //Mark, 1975; Etzelmuller, 2000;

                  if PlotResults then begin
                     if MDDef.GemorphSSOPoles then begin
                        NetForm.nd.ReallyPlot := true;
                        NetForm.nd.ContourPoles(false);
                        Netform.nd.WorkingBitmap.Canvas.Font.Size := 15;
                        y := Netform.nd.WorkingBitmap.Height-Netform.nd.WorkingBitmap.Canvas.TextHeight(AreaName)-3;
                        //Netform.nd.WorkingBitmap.Canvas.TextOut(1,y,RemoveUnderScores(AreaName));
                        Netform.nd.WorkingBitmap.Canvas.Font.Size := 12;
                        Netform.nd.WorkingBitmap.Canvas.TextOut(1,1,'ln(s1/s2)=' + RealToString(SSOVars.s1s2,-5,2));
                        Netform.nd.WorkingBitmap.Canvas.TextOut(1,18,'ln(s2/23)=' + RealToString(SSOVars.s2s3,-5,2) );
                        Netform.nd.WorkingBitmap.Canvas.TextOut(1,35,'Trend:' + RealToString(SSOVars.TheDipDirs[3],6,1) + '°' );
                        Netform.nd.WorkingBitmap.Canvas.Font.Size := 10;
                        Netform.nd.WorkingBitmap.Canvas.TextOut(1,53,'n=' + IntToStr(Netform.nd.NumPlottedPoints));
                        Netform.nd.NetOutline;

                        if (NetName <> '') then begin
                           PetImage.SaveBitmap(Netform.nd.WorkingBitmap,NetName);
                           NetForm.Close;
                           NetForm := Nil;
                        end;
                     end
                     else begin
                        NetForm.Close;
                        NetForm := Nil;
                     end;

                     if MDDef.GemorphAspectRose then begin
                        RoseGraph := AspectStats.CreateRose;
                        if (AspName <> '') then begin
                           PetImage.SaveImageAsBMP(RoseGraph.Image1,AspName);
                           RoseGraph.Close;
                           RoseGraph := Nil;
                        end;
                     end {if};
                  end;
               end
               else if PlotResults then begin
                  NetForm.Close;
                  NetForm := Nil;
               end;
            finally
              AspectStats.Destroy;
            end;
            {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.SSOComputations out'); {$EndIf}
         end;

