{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{   include file for demcoord.pas    }
{____________________________________}


{$Define MakePartialGrids}


         procedure tDEMDataSet.DoSSOStrip(Limits : tGridLimits; var Results : tStringList);
         var
            s1,s2,S3 : ShortString;
            aLine : ANSIString;
            SSOvars : tSSOvars;
            Col,Row,xsize,ysize,ColInc,RowInc,Done : integer;
            WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd,
            Lat,Long,Lat1,Long1,Lat2,Long2,aMinVal,aMaxVal : float64;
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
            {$IfDef NoParallelFor}
            {$Else}
               PartLimits : tGridLimitsArray;
               i,it : integer;
            {$EndIf}
         begin
            {$If Defined(ShowDEMSSOCalc) or Defined(RecordTime)} WriteLineToDebugFile('tDEMDataSet.OrientationTable in'); Stopwatch := TStopwatch.StartNew; {$EndIf}

             if (UseMap <> Nil) or MDDef.GeomorphMapsFullDEM then Limits := UseMap.MapDraw.MapAreaDEMGridLimits
             else Limits := FullDEMGridLimits;

             SSOFailPts := 0;
             SSOFailFlat := 0;
             SSOfailOrg := 0;

             StartProgress('SSO computations');

            {$IfDef NoParallelFor}
                DoSSOStrip(limits,Results[1]);
            {$Else}
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
             {$EndIf}
             EndProgress;
             Result := UseMap.StringListToLoadedDatabase(Results[1],fName,false,true);
             GISdb[Result].dbOpts.dbAutoShow := dbasTerrainFabric;
             GISdb[Result].RedrawLayerOnMap;
            {$If Defined(ShowDEMSSOCalc) or Defined(RecordTime)} Elapsed := Stopwatch.Elapsed; WriteLineToDebugFile('OrientationTable, out: ' + RealToString(Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
         end;

         function tDEMDataSet.PointSSOComputations(Col,Row,FullBoxSizeMeters : integer; var SSOvars : tSSOvars; PlotResults,Complete,PlotAspectFreq : boolean) : boolean;
         var
            xsize,ysize : integer;
         begin
            {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.PointSSOComputations in: ' + IntToStr(Col) + ' & ' + IntToStr(Row)+ ' rad=' + IntToStr(FullBoxSize)); {$EndIf}
            xsize := round(0.5 * FullBoxSizeMeters / AverageXSpace);
            ysize := round(0.5 * FullBoxSizeMeters / AverageYSpace);
            Result := SSOComputations(SpecifyDEMGridLimits(Col-xsize,Row-ysize,Col+xsize,Row+Ysize),SSOvars,PlotResults,'','');
         end;


         function tDEMDataSet.SimplePointSSOComputations(PlotResults : boolean; Col,Row,FullBoxSizeMeters : integer; var s1s2,s2s3,Trend,RoughnessFactor : float64) : boolean;
         var
             SSOvars : tSSOvars;
         begin
            {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.SimplePointSSOComputations in: ' + IntToStr(Col) + ' & ' + IntToStr(Row) + ' rad=' + IntToStr(FullBoxSize)); {$EndIf}
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
             {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.FeatureSSOComputations in, Feature=: ' + IntToStr(inFeatureID)); {$EndIf}
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


         function tDEMDataSet.SSOComputations(GridLimits : tGridLimits; var SSOvars : tSSOvars; PlotResults : boolean; NetName,AspName : PathStr;
              UseMinSlope : float64 = 0; UseMaxSlope : float64 = 9999) : boolean;
         var
            xd,yd,y,Col,Row,k  : integer;
            MaxZ,MinZ,SumElev,SumSlope,
            SumSqSlope,SumSqElev : float64;
            zf : float32;
            Symbol : tFullSymbolDeclaration;
            TStr : shortString;
            SlopeAspectRec : tSlopeAspectRec;
            SSOVarAnalysis : tSSOVarAnalysis;
            AspectStats : tAspectStats;
         begin
            {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.SSOComputations in, ' + GridLimitsToString(GridLimits) + ' plot=' + IntToStr(Ord(MDDef.SSObyPole))); {$EndIf}
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
               end {if PlotResults};
               {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('SSO setup done'); {$EndIf}

               SSOVarAnalysis.Create(ThisDEM);
               SSOVarAnalysis.SSOVars.NumPts := 0;
               SSOVarAnalysis.NormalsInBox(GridLimits);
               SSOVarAnalysis.ComputeEigenVectors;
               SSOVars := SSOVarAnalysis.SSOVars;
               SSOVarAnalysis.Destroy;

               {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('SSOVarAnalysis done'); {$EndIf}

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
               {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('Vectors done, npts=' + IntToStr(SSOvars.NumPts)); {$EndIf}

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
                        Netform.nd.WorkingBitmap.Canvas.TextOut(1,y,RemoveUnderScores(AreaName));
                        Netform.nd.WorkingBitmap.Canvas.Font.Size := 12;
                        Netform.nd.WorkingBitmap.Canvas.TextOut(1,1,'ln(s1/s2)=' + RealToString(SSOVars.s1s2,-5,2));
                        Netform.nd.WorkingBitmap.Canvas.TextOut(1,18,'ln(s2/23)=' + RealToString(SSOVars.s2s3,-5,2) );
                        Netform.nd.WorkingBitmap.Canvas.TextOut(1,35,'Trend:' + RealToString(SSOVars.TheDipDirs[3],6,1) + '°' );
                        Netform.nd.WorkingBitmap.Canvas.Font.Size := 10;
                        Netform.nd.WorkingBitmap.Canvas.TextOut(1,53,'n=' + IntToStr(k));
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
            {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.SSOComputations out'); {$EndIf}
         end;



function tDEMDataSet.RoughnessFromSlopeSTD(x,y,Radius : integer; var Roughness : float32) : boolean;
var
   i,j : integer;
   z : float32;
   MomentVar : tMomentVar;
   SlopeAsp : tSlopeAspectRec;
   sl : array[1..100] of float32;
begin
   MomentVar.Npts := 0;
   Radius := Radius div 2;
   for I := -Radius to Radius do begin
      for J := -Radius to Radius do begin
         if GetSlopeAndAspect(x+i,y+j,SlopeAsp) then begin
            inc(MomentVar.Npts);
            sl[MomentVar.Npts] := SlopeAsp.SlopePercent;
         end;
      end;
   end;
   Result := MomentVar.NPts > 5;
   if Result then begin
      moment(sl,MomentVar,msAfterStdDev);
      Roughness := MomentVar.sdev;
   end;
end;



procedure tDEMDataSet.RichardsonExtrapolationSlopeMaps(Save : boolean = false);
//Extrapolated Evans-Young method (López-Vázquez, 2022), using Richardson extrapolation
//Carlos López-Vázquez (2022) Uncertainty interval estimates for computing slope and aspect from a gridded digital elevation model,
//   International Journal of Geographical Information Science, 36:8, 1601-1628, DOI: 10.1080/13658816.2022.2063294


//the DEM is entirely in memory, and could be stored as bytes, short integers, long integers, unsigned integers, or floats
//while some of these do not make sense for a DEM, they do for other grids, where it does not make sense to store bytes as floats
//calls to get elevations sort out the storage issues and return floats for all the internal computations

//there is error trapping code, with a conditional compilation directive, to store intermediate values for debugging with the partial derivatives
//this creates a lot of grids, which are not necessarily easy to interpret or immediately useful

var
  bmplist : tStringList;

    procedure MapAndSave(DEM : integer);
    begin
        if ValidDEM(DEM) then begin
           if Save then DEMGlb[DEM].SaveAsGeotiff(ExtractFilePath(DEMFileName) + DEMGlb[DEM].AreaName + '.tif')
           else begin
              DEMGlb[DEM].SetUpMap(DEM,true,mtElevSpectrum);
           end;
        end;
    end;


var
   {$IfDef MakePartialGrids}
      dxgrid,dygrid,dx2grid,dy2grid,dx3grid,dy3grid,
   {$EndIf}

   Aspect_3x3Grid,Aspect_5x5Grid,Aspect_7x7Grid,Aspect4thGrid,Aspect6thGrid,
   Slope3x3Grid,slope5x5Grid,Slope4thgrid,slope7x7Grid,Slope6thgrid,
   SlopeErrorBoundGrid,SlopeUncertaintyGrid,TruncationErrorGrid,
   x,y : integer;

   dzdx_3x3,dzdy_3x3,dzdx_5x5,dzdy_5x5,dzdx_7x7,dzdy_7x7,dzdx_4th,dzdy_4th,dzdx_6th,dzdy_6th,

   slope_3x3,slope_5x5,slope_4th,slope_7x7,slope_6th,

   Aspect_3x3, Aspect_5x5,Aspect4th,Aspect_7x7,Aspect6th,
   SlopeErrBound,uncertainty_bndX, uncertainty_bndY,TruncationError,
   UB_fx,LB_fx,UB_fy,LB_fy,LB_fx2,UB_fx2,LB_fy2, UB_fy2,LB_m,UB_m,UB_slope,LB_slope,uncertainty_slope,
   Lat,Long : float64;
   znw,zw,zsw,zn,z,zs,zne,ze,zse,dx,dy,GridTrueAngle : float32;
begin
   {$IfDef RecordProblems} WriteLineToDebugFile('tDEMDataSet.CarlosLopezSlopeMaps in'); {$EndIf}

   Slope3x3Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope3x3',PercentSlope);
   slope5x5Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope5x5',PercentSlope);
   slope7x7Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope7x7',PercentSlope);
   Slope4thGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope_4th',PercentSlope);
   Slope6thGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope_6th',PercentSlope);
   SlopeErrorBoundGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope_error',PercentSlope);
   SlopeUncertaintyGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_slope_uncertainty',PercentSlope);
   TruncationErrorGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_truncation_error',Undefined);

   Aspect_3x3Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect3x3',AspectDeg);
   Aspect_5x5Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect5x5',AspectDeg);
   Aspect4thGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect_4th',AspectDeg);
   Aspect_7x7grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect7x7',AspectDeg);
   Aspect6thGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_aspect_6th',AspectDeg);

   {$IfDef MakePartialGrids}
      dxGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdx3x3',PercentSlope);
      dyGrid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdy3x3',PercentSlope);
      dx2Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdx5x5',PercentSlope);
      dy2Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdy5x5',PercentSlope);
      dx3Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdx7x7',PercentSlope);
      dy3Grid := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_dzdy7x7',PercentSlope);
   {$EndIf}

   //grid containing DEM is zero based, 0,0 at the SW corner
   for x := 1 to (DEMHeader.NumCol - 2) do begin   //don't worry about edge and corner cases, leave them missing (merge with neighbor if needed)
      for y := 1 to (DEMHeader.NumRow - 2) do begin
         {$IfDef CarlosErrorTrap} if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                WriteLineToDebugFile('Point to check: ' + IntToStr(CarlosXRecord) + '/' + IntToStr(CarlosYRecord) + '  ' + DEMLocationString(CarlosXRecord,CarlosYRecord));
            end;
         {$EndIf}
         if SurroundedPointElevs(x,y,znw,zw,zsw,zn,z,zs,zne,ze,zse,1) then begin //get elevations in first tier surrounding point
            PixelSpacingAndRotation(x,y,Lat,Long,dx,dy,GridTrueAngle,true);

            {$IfDef CarlosErrorTrap} if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                  WriteLineToDebugFile('Pixel spacing, x=' + RealToString(dx,-8,-2) + '  y=' + RealToString(dy,-8,-2));
                  WriteLineToDebugFile('znw=' + RealToString(znw,-8,-2) + '  zn=' + RealToString(zn,-8,-2) + '  zne=' + RealToString(zne,-8,-2));
                  WriteLineToDebugFile('zw=' + RealToString(zw,-8,-2) + '  z=' + RealToString(z,-8,-2) + '  ze=' + RealToString(zne,-8,-2));
                  WriteLineToDebugFile('zsw=' + RealToString(zsw,-8,-2) + '  zs=' + RealToString(zs,-8,-2) + '  zse=' + RealToString(zse,-8,-2));
               end;
            {$EndIf}

            dzdx_3x3 := 1 / dx / 6 * (+zne+ze+zse-znw-zw-zsw);
            dzdy_3x3 := 1 / dy / 6 * (+znw+zn+zne-zsw-zs-zse);
            slope_3x3 := 100 * sqrt(sqr(dzdx_3x3) + sqr(dzdy_3x3));
            DEMGlb[Slope3x3Grid].SetGridElevation(x,y,slope_3x3);
            if (slope_3x3 > 0.01) then begin
               Aspect_3x3 := FindCompassAngleInRange(HeadingOfLine(dzdx_3x3,dzdy_3x3)+180);
               DEMGlb[Aspect_3x3Grid].SetGridElevation(x,y,Aspect_3x3);
            end;

            {$IfDef MakePartialGrids}
               DEMGlb[dxGrid].SetGridElevation(x,y,100 * dzdx_3x3);
               DEMGlb[dyGrid].SetGridElevation(x,y,100 * dzdy_3x3);
            {$EndIf}

            {$IfDef CarlosErrorTrap} if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                  WriteLineToDebugFile('slope_3x3=' + RealToString(slope_3x3,-8,-2) + '  dzdx=' + RealToString(100*dzdx_3x3,-8,-2) + '  dzdy=' + RealToString(100*dzdy_3x3,-8,-2) + '  aspect=' + RealToString(Aspect_3x3,-8,-2));
               end;
            {$EndIf}

            //don't worry about second row from edges
            if (x > 1) and (x < (DEMHeader.NumCol - 3)) and (y > 1) and (y < (DEMHeader.NumRow - 3)) then begin
               if SurroundedPointElevs(x,y,znw,zw,zsw,zn,z,zs,zne,ze,zse,2) then begin //get elevations in second tier surrounding point
                  {$IfDef CarlosErrorTrap} if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                        WriteLineToDebugFile('znw=' + RealToString(znw,-8,-2) + '  zn=' + RealToString(zn,-8,-2) + '  zne=' + RealToString(zne,-8,-2));
                        WriteLineToDebugFile('zw=' + RealToString(zw,-8,-2) + '  z=' + RealToString(z,-8,-2) + '  ze=' + RealToString(zne,-8,-2));
                        WriteLineToDebugFile('zsw=' + RealToString(zsw,-8,-2) + '  zs=' + RealToString(zs,-8,-2) + '  zse=' + RealToString(zse,-8,-2));
                     end;
                  {$EndIf}
                  dzdx_5x5 := 1 / (2*dx) / 6 * (+zne+ze+zse-znw-zw-zsw);
                  dzdy_5x5 := 1 / (2*dy) / 6 * (+znw+zn+zne-zsw-zs-zse);
                  slope_5x5 := 100 * sqrt(sqr(dzdx_5x5) + sqr(dzdy_5x5));
                  DEMGlb[slope5x5Grid].SetGridElevation(x,y,slope_5x5);

                  {$IfDef MakePartialGrids}
                     DEMGlb[dx2Grid].SetGridElevation(x,y,100 * dzdx_5x5);
                     DEMGlb[dy2Grid].SetGridElevation(x,y,100 * dzdy_5x5);
                  {$EndIf}

                  if (slope_5x5 > 0.01) then begin
                     Aspect_5x5 := FindCompassAngleInRange(HeadingOfLine(dzdx_5x5,dzdy_5x5)+180);
                     DEMGlb[Aspect_5x5Grid].SetGridElevation(x,y,Aspect_5x5);
                  end;

                  {$IfDef CarlosErrorTrap} if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                        WriteLineToDebugFile('slope_5x5=' + RealToString(slope_5x5,-8,-2) + '  dzdx=' + RealToString(100*dzdx_5x5,-8,-2) + '  dzdy=' + RealToString(100*dzdy_5x5,-8,-2) +
                             '  aspect=' + RealToString(Aspect_5x5,-8,-2));
                     end;
                  {$EndIf}

                  dzdx_4th := 4/3*dzdx_3x3-1/3*dzdx_5x5;
                  dzdy_4th := 4/3*dzdy_3x3-1/3*dzdy_5x5;

                  slope_4th := 100 * sqrt(sqr(dzdx_4th) + sqr(dzdy_4th));

                  DEMGlb[Slope4thGrid].SetGridElevation(x,y,slope_4th);
                  if (slope_4th > 0.01) then begin
                     Aspect4th := FindCompassAngleInRange(HeadingOfLine(dzdx_4th,dzdy_4th)+180);
                     DEMGlb[Aspect4thGrid].SetGridElevation(x,y,Aspect4th);
                  end;

                  if (x > 2) and (x < (DEMHeader.NumCol - 4)) and (y > 2) and (y < (DEMHeader.NumRow - 4)) then begin
                     if SurroundedPointElevs(x,y,znw,zw,zsw,zn,z,zs,zne,ze,zse,3) then begin //get elevations in third tier surrounding point
                        dzdx_7x7 := 1 / (3*dx) / 6 * (+zne+ze+zse-znw-zw-zsw);
                        dzdy_7x7 := 1 / (3*dy) / 6 * (+znw+zn+zne-zsw-zs-zse);
                        slope_7x7 := 100 * sqrt(sqr(dzdx_7x7) + sqr(dzdy_7x7));

                        DEMGlb[slope7x7Grid].SetGridElevation(x,y,slope_7x7);
                        if (slope_7x7 > 0.01) then begin
                           Aspect_7x7 := FindCompassAngleInRange(HeadingOfLine(dzdx_7x7,dzdy_7x7)+180);
                           DEMGlb[Aspect_7x7Grid].SetGridElevation(x,y,Aspect_7x7);
                        end;

                        dzdx_6th := 3/2*dZdX_3x3 - 3/5*dZdX_5x5 + 1/10*dZdX_7x7;
                        dzdy_6th := 3/2*dZdy_3x3 - 3/5*dZdy_5x5 + 1/10*dZdy_7x7;

                        {$IfDef MakePartialGrids}
                           DEMGlb[dx3Grid].SetGridElevation(x,y,100 * dzdx_7x7);
                           DEMGlb[dy3Grid].SetGridElevation(x,y,100 * dzdy_7x7);
                        {$EndIf}

                        slope_6th := 100 * sqrt(sqr(dzdx_6th) + sqr(dzdy_6th));

                        DEMGlb[Slope6thGrid].SetGridElevation(x,y,slope_6th);
                        if (slope_6th > 0.01) then begin
                           Aspect6th := FindCompassAngleInRange(HeadingOfLine(dzdx_6th,dzdy_6th)+180);
                           DEMGlb[Aspect6thGrid].SetGridElevation(x,y,Aspect6th);
                        end;
                     end;
                  end;

                  uncertainty_bndX := abs(dzdx_3x3-dzdx_5x5) / 3;  //will be needed later
                  uncertainty_bndY := abs(dzdy_3x3-dzdy_5x5) / 3;

                  dzdx_4th := dzdx_3x3 + (dzdx_3x3-dzdx_5x5) / 3;  //Now this is 4th order. It has been using Richardson Extrapolation after a 2nd order estimate
                  dzdy_4th := dzdy_3x3 + (dzdy_3x3-dzdy_5x5) / 3;

                  //The uncertainty bound for the slope should be computed using Interval Arithmetic. To make it simple I will introduce some extra variables
                  UB_fx := dzdx_4th + uncertainty_bndX;  //This stands for Upper Bound
                  LB_fx := dzdx_4th - uncertainty_bndX;  //This stands for Lower Bound
                  UB_fy := dzdy_4th + uncertainty_bndY;  //same for y
                  LB_fy := dzdy_4th - uncertainty_bndY;  //same for y
                  if (LB_fx > 0) then begin
                     LB_fx2 := LB_fx* LB_fx;
                     UB_fx2 := UB_fx* UB_fx;
                  end
                  else if (UB_fx < 0) then begin
                     LB_fx2 := UB_fx * UB_fx;
                     UB_fx2 := LB_fx * LB_fx;
                  end
                  else begin
                     LB_fx2 := 0;
                     UB_fx2 := max(LB_fx * LB_fx, UB_fx * UB_fx);
                  end;
                  if (LB_fy > 0) then begin
                     LB_fy2 := LB_fy* LB_fy;
                     UB_fy2 := UB_fy* UB_fy;
                  end
                  else if (UB_fy < 0) then begin
                     LB_fy2 := UB_fy* UB_fy;
                     UB_fy2 := LB_fy* LB_fy;
                  end
                  else begin
                     LB_fy2 := 0;
                     UB_fy2 := max(LB_fy* LB_fy, UB_fy* UB_fy);
                  end;

                  //Now we should find bounds for the sum of intervals
                  LB_m := LB_fx2 + LB_fy2;
                  UB_m := UB_fx2 + UB_fy2; //m stands for the sum of squares of the partial derivatives
                  UB_slope := 100*sqrt(UB_m);
                  LB_slope := 100*sqrt(LB_m); //slope should belong to the interval [LB_slope, UB_slope]
                  uncertainty_slope := (UB_slope-LB_slope);  //must be positive. equivalent to what denoted as SlopeErrBound
                  SlopeErrBound := abs(slope_3x3 - slope_5x5) / 3;
                  TruncationError := 1/3*(dZdX_3x3-dZdX_5x5);
                  DEMGlb[SlopeErrorBoundGrid].SetGridElevation(x,y,SlopeErrBound);
                  DEMGlb[SlopeUncertaintyGrid].SetGridElevation(x,y,uncertainty_slope);
                  DEMGlb[TruncationErrorGrid].SetGridElevation(x,y,TruncationError);

                  {$IfDef CarlosErrorTrap}
                     if (x=CarlosXRecord) and (y=CarlosYRecord) then begin
                        WriteLineToDebugFile('slope_4th=' + RealToString(slope_4th,-8,-2) + '  dzdx_4th=' + RealToString(dzdx_4th,-8,-2) + '  dzdy_4th =' + RealToString(dzdy_4th ,-8,-2));
                        WriteLineToDebugFile('uncertainty_bndX=' + RealToString(uncertainty_bndX,-8,-2) + '  uncertainty_bndY=' + RealToString(uncertainty_bndY,-8,-2));
                        WriteLineToDebugFile('UB_slope=' + RealToString(UB_slope,-8,-2) + '  LB_slope=' + RealToString(LB_slope,-8,-2));
                        WriteLineToDebugFile('uncertainty_slope=' + RealToString(uncertainty_slope,-8,-2) + '  SlopeErrBound=' + RealToString(SlopeErrBound,-8,-2));
                     end;
                  {$EndIf}
               end;
            end;
         end;
      end;
   end;
   MapAndSave(Slope3x3Grid);
   MapAndSave(slope5x5Grid);
   MapAndSave(slope7x7Grid);
   MapAndSave(Slope4thGrid);
   MapAndSave(Slope6thGrid);

   MapAndSave(Aspect_3x3Grid);
   MapAndSave(Aspect_5x5Grid);
   MapAndSave(Aspect_7x7Grid);
   MapAndSave(Aspect4thGrid);
   MapAndSave(Aspect6thGrid);

   MapAndSave(SlopeErrorBoundGrid);
   MapAndSave(SlopeUncertaintyGrid);
   MapAndSave(TruncationErrorGrid);

   {$IfDef MakePartialGrids}
      MapAndSave(dxGrid);
      MapAndSave(dx2Grid);
      MapAndSave(dx3Grid);
      MapAndSave(dyGrid);
      MapAndSave(dy2Grid);
      MapAndSave(dy3Grid);
   {$EndIf}

    if not Save then AllAspects;  //create aspect roses

   {$IfDef RecordProblems} WriteLineToDebugFile('tDEMDataSet.CarlosLopezSlopeMaps out'); {$EndIf}
end;



function tDEMDataSet.GetSlopeAndAspect(Col,Row : integer; var SlopeAsp : tSlopeAspectRec) : boolean;
//based on coordinates in the DEM grid, which start in the SW corner
var
   sl     : array[1..4] of float64;
   AspDir : array[2..4] of tCompassDirection;
   UseDiaSpace : float64;


         procedure AddSlope(z,z1,z2,Dist : float64);
         //used only for average neighbors method, not recommended
         var
            d1,d2 : float64;
         begin
            d1 := (z - z1);
            d2 := (z - z2);
            d1 := abs(d1);
            SlopeAsp.Slope := SlopeAsp.Slope + d1 / Dist;
            d2 := abs(d2);
            SlopeAsp.Slope := SlopeAsp.Slope + d2 / Dist;
         end;


         procedure MaxDownHill(z,z1,z2,Dist : float64; var Max : float64; Dir1,Dir2 : tCompassDirection; var AspDir : tCompassDirection);    inline;
         //used only for MaxDownhillSlope, not recommended
         var
            d1,d2 : float64;
         begin {proc MaxDownHill}
            d1 := (z - z1);
            d2 := (z - z2);
            if (d1 = d2) then if Odd(Random(100)) then d1 := 0 else d2 := 0;
            if (d1 > d2) and (d1 > 0) then begin
               AspDir := Dir1;
               Max := d1 / Dist;
            end
            else if (d2 > d1) and (d2 > 0) then begin
               AspDir := Dir2;
               Max := d2 / Dist;
            end
            else begin
               Max := 0;
               AspDir := cdPit;
            end;
         end {proc MaxSlope};


         procedure MaxSlopeComputations;
         //used only for SteepestNeighbor,AverageNeighbor; not recommended

                  procedure MaxSlope(z,z1,z2,Dist : float64; var Max : float64; Dir1,Dir2 : tCompassDirection; var AspDir : tCompassDirection); inline;
                  const
                     Opp : array[tCompassDirection] of tCompassDirection = (cdS,cdSW,cdW,cdNW,cdN,cdNE,cdE,cdSE,cdFlat,cdPit);
                  var
                     d1,d2 : float64;
                  begin {proc MaxSlope}
                     d1 := (z - z1);
                     d2 := (z - z2);
                     if (d1 = d2) then if Odd(Random(100)) then d1 := 0 else d2 := 0;

                     if abs(d1) > abs(d2) then begin
                        if d1 < 0 then AspDir := Opp[Dir1] else AspDir := Dir1;
                        d1 := abs(d1);
                        Max := d1 / Dist;
                     end
                     else begin
                        if d2 < 0 then AspDir := Opp[Dir2] else AspDir := Dir2;
                        d2 := abs(d2);
                        Max := d2 / Dist;
                     end;
                  end {proc MaxSlope};

         begin
             MaxSlope(SlopeAsp.z,SlopeAsp.zs,SlopeAsp.zn,SlopeAsp.dy,sl[1],cdS,cdN,SlopeAsp.Dir);
             MaxSlope(SlopeAsp.z,SlopeAsp.zw,SlopeAsp.ze,SlopeAsp.dx,sl[2],cdW,cdE,AspDir[2]);
             MaxSlope(SlopeAsp.z,SlopeAsp.zsw,SlopeAsp.znw,UseDiaSpace,sl[3],cdSW,cdNW,AspDir[3]);
             MaxSlope(SlopeAsp.z,SlopeAsp.zse,SlopeAsp.zne,UseDiaSpace,sl[4],cdSE,cdNE,AspDir[4]);
         end;


         function IsPit(var SlopeAsp : tSlopeAspectRec) : boolean; inline;
         begin
            Result := (SlopeAsp.z < SlopeAsp.zne) and (SlopeAsp.z < SlopeAsp.znw) and (SlopeAsp.z < SlopeAsp.zn) and (SlopeAsp.z < SlopeAsp.ze) and
                  (SlopeAsp.z < SlopeAsp.zw) and (SlopeAsp.z < SlopeAsp.zse) and (SlopeAsp.z < SlopeAsp.zsw) and (SlopeAsp.z < SlopeAsp.zs);
         end;

         procedure GetAspect(var SlopeAsp : tSlopeAspectRec); //inline;
         begin
            if (abs(SlopeAsp.dzdx) < 0.001) and (abs(SlopeAsp.dzdy) < 0.001) then begin
               SlopeAsp.AspectDir := MaxSmallInt;
               SlopeAsp.AspectDirGrid := MaxSmallInt;
               SlopeAsp.Dir := cdFlat;
            end
            else begin
               //modified atan2 function
               //   standard math convention puts 0 on the x axis and angles increases counterclockwise,
               //   use geographic conventions, where N (0) is on the y axis, and angles increase clockwise
               SlopeAsp.AspectDirGrid := HeadingOfLine(SlopeAsp.dzdx,SlopeAsp.dzdy) + 180;
               if (SlopeAsp.AspectDirGrid > 360) then SlopeAsp.AspectDirGrid := SlopeAsp.AspectDirGrid - 360;
               if (SlopeAsp.AspectDirGrid < 0) then SlopeAsp.AspectDirGrid := SlopeAsp.AspectDirGrid + 360;

               SlopeAsp.AspectDir := SlopeAsp.AspectDirGrid + SlopeAsp.GridTrueAngle;
               if (SlopeAsp.AspectDir > 360) then SlopeAsp.AspectDir := SlopeAsp.AspectDir - 360;
               if (SlopeAsp.AspectDir < 0) then SlopeAsp.AspectDir := SlopeAsp.AspectDir + 360;

               if IsPit(SlopeAsp) then begin
                  SlopeAsp.Dir := cdPit;
               end
               else begin
                  (*
                  if (SlopeAsp.AspectDir < 22.5) or (SlopeAsp.AspectDir > 337.5) then SlopeAsp.Dir := cdN
                  else if (SlopeAsp.AspectDir < 67.5) then SlopeAsp.Dir := cdNE
                  else if (SlopeAsp.AspectDir < 112.5) then SlopeAsp.Dir := cdE
                  else if (SlopeAsp.AspectDir < 157.5) then SlopeAsp.Dir := cdSE
                  else if (SlopeAsp.AspectDir < 202.5) then SlopeAsp.Dir := cdS
                  else if (SlopeAsp.AspectDir < 247.5) then SlopeAsp.Dir := cdSW
                  else if (SlopeAsp.AspectDir < 292.5) then SlopeAsp.Dir := cdW
                  else SlopeAsp.Dir := cdNW;
                  *)
                  SlopeAsp.Dir := AspectDir8FromAspect(SlopeAsp.AspectDir);
               end;
            end;
         end;


var
   j : integer;
   Lat,Long : float64;
begin
   Result := false;
   SlopeAsp.AspectDir := MaxSmallInt;
   SlopeAsp.Slope := 0;
   if SurroundedPointElevs(Col,Row,SlopeAsp.znw,SlopeAsp.zw,SlopeAsp.zsw,SlopeAsp.zn,SlopeAsp.z,SlopeAsp.zs,SlopeAsp.zne,SlopeAsp.ze,SlopeAsp.zse,MDDef.SlopeRegionRadius) then begin
      Result := true;
      PixelSpacingAndRotation(Col,Row,Lat,Long,SlopeAsp.dx,SlopeAsp.dy,SlopeAsp.GridTrueAngle,MDDef.QuickSlopeSpacings);
      SlopeAsp.dx := SlopeAsp.dx * MDDef.SlopeRegionRadius;
      SlopeAsp.dy := SlopeAsp.dy * MDDef.SlopeRegionRadius;

      if (MDDef.SlopeAlg in [smEightNeighborsUnweighted,smFourNeighbors,smEightNeighborsWeighted,smEightNeighborsWeightedByDistance,smFrameFiniteDifference,smSimpleDifference,smONeillAndMark]) then begin
         if (MDDef.SlopeAlg in [smEightNeighborsUnweighted]) then begin //this is the preferred method, Evans
            SlopeAsp.dzdx := (+SlopeAsp.zne+SlopeAsp.ze+SlopeAsp.zse-SlopeAsp.znw-SlopeAsp.zw-SlopeAsp.zsw) / SlopeAsp.dx / 6;
            SlopeAsp.dzdy := (+SlopeAsp.znw+SlopeAsp.zn+SlopeAsp.zne-SlopeAsp.zsw-SlopeAsp.zs-SlopeAsp.zse) / SlopeAsp.dy / 6;
         end
         else if (MDdef.SlopeAlg = smEightNeighborsWeighted) then begin  //Horn method
            SlopeAsp.dzdx := 0.125 * ( SlopeAsp.zne + (2 * SlopeAsp.ze) + SlopeAsp.zse  -SlopeAsp.znw - (2 * SlopeAsp.zw) - SlopeAsp.zsw) / SlopeAsp.dx;
            SlopeAsp.dzdy := 0.125 * ( SlopeAsp.znw + (2 * SlopeAsp.zn) + SlopeAsp.zne  -SlopeAsp.zsw - (2 * SlopeAsp.zs) - SlopeAsp.zse) / SlopeAsp.dy;
         end
         else if (MDdef.SlopeAlg = smEightNeighborsWeightedByDistance) then begin  //Unwin, 1981
            SlopeAsp.dzdy := 1 / (4 + 2 * sqrt_2) * ((SlopeAsp.znw + Sqrt_2 * SlopeAsp.zn + SlopeAsp.zne) - (SlopeAsp.zsw + Sqrt_2 * SlopeAsp.zs + SlopeAsp.zse)) / SlopeAsp.dy;
            SlopeAsp.dzdx := 1 / (4 + 2 * sqrt_2) * ((SlopeAsp.zne + Sqrt_2 * SlopeAsp.ze + SlopeAsp.zse) - (SlopeAsp.znw + Sqrt_2 * SlopeAsp.zw + SlopeAsp.zsw)) / SlopeAsp.dx;
         end
         else if (MDDef.SlopeAlg = smFrameFiniteDifference) then begin
            SlopeAsp.dzdy := (SlopeAsp.znw - SlopeAsp.zsw + SlopeAsp.zne - SlopeAsp.zse) * 0.25 / SlopeAsp.dy;
            SlopeAsp.dzdx := (SlopeAsp.zse - SlopeAsp.zsw + SlopeAsp.zne - SlopeAsp.znw) * 0.25 / SlopeAsp.dx;
         end
         else if (MDDef.SlopeAlg = smFourNeighbors) then begin  //Fleming and Hoffer, 1979; Ritter, 1987; Zevenbergen and Thorne, 1987; O'Neill & Mark, 1987
            SlopeAsp.dzdy := (SlopeAsp.zn - SlopeAsp.zs) * 0.5 / SlopeAsp.dy;
            SlopeAsp.dzdx := (SlopeAsp.ze - SlopeAsp.zw) * 0.5 / SlopeAsp.dx;
         end
         else if (MDDef.SlopeAlg = smSimpleDifference) then begin
            SlopeAsp.dzdy := (SlopeAsp.z - SlopeAsp.zs) * 0.5 / SlopeAsp.dy;
            SlopeAsp.dzdx := (SlopeAsp.z - SlopeAsp.zw) * 0.5 / SlopeAsp.dx;
         end
         else if (MDDef.SlopeAlg in [smONeillAndMark]) then begin
            SlopeAsp.dzdy := (SlopeAsp.zn - SlopeAsp.z) / SlopeAsp.dy;
            SlopeAsp.dzdx := (SlopeAsp.ze - SlopeAsp.z) / SlopeAsp.dx;
         end;
         SlopeAsp.Slope := sqrt(sqr(SlopeAsp.dzdx) + sqr(SlopeAsp.dzdy));
         GetAspect(SlopeAsp);
      end
      else begin //these methods probably have no good reasons to recommend them any longer
         UseDiaSpace := DiagSpaceByDEMrow^[Row] * MDDef.SlopeRegionRadius;
         if (MDDef.SlopeAlg in [smGuthHybrid]) then begin  //no longer recommended
             MaxSlopeComputations;
             for j := 2 to 4 do if sl[j] > sl[1] then sl[1] := sl[j];
             SlopeAsp.Slope := Sl[1];
             SlopeAsp.dzdx := (SlopeAsp.zne + SlopeAsp.ze + SlopeAsp.zse - SlopeAsp.zsw - SlopeAsp.zw - SlopeAsp.znw) / 6 / SlopeAsp.dx;
             SlopeAsp.dzdy := (SlopeAsp.znw + SlopeAsp.zn + SlopeAsp.zne - SlopeAsp.zsw - SlopeAsp.zs - SlopeAsp.zse) / 6 / SlopeAsp.dy;
             GetAspect(SlopeAsp);
         end
         else if (MDDef.SlopeAlg in [smSteepestNeighbor,smAverageNeighbor]) then begin
            MaxSlopeComputations;
            for j := 2 to 4 do if sl[j] > sl[1] then begin
                sl[1] := sl[j];
                SlopeAsp.Dir := AspDir[j];
            end {for j};
            SlopeAsp.Slope := Sl[1];
            SlopeAsp.AspectDir := 45.0 * Ord(SlopeAsp.Dir);
            if (MDDef.SlopeAlg = smAverageNeighbor) then begin
               SlopeAsp.Slope := 0;
               AddSlope(SlopeAsp.z,SlopeAsp.zs,SlopeAsp.zn,SlopeAsp.dy);
               AddSlope(SlopeAsp.z,SlopeAsp.zw,SlopeAsp.ze,SlopeAsp.dx);
               AddSlope(SlopeAsp.z,SlopeAsp.zsw,SlopeAsp.znw,UseDiaSpace);
               AddSlope(SlopeAsp.z,SlopeAsp.zse,SlopeAsp.zne,UseDiaSpace);
               SlopeAsp.Slope := SlopeAsp.Slope / 8;
            end;
         end
         else if (MDDef.SlopeAlg = smMaxDownhillSlope) then begin
            MaxDownHill(SlopeAsp.z,SlopeAsp.zs,SlopeAsp.zn,SlopeAsp.dy,sl[1],cdS,cdN,SlopeAsp.Dir);
            MaxDownHill(SlopeAsp.z,SlopeAsp.zw,SlopeAsp.ze,SlopeAsp.dx,sl[2],cdW,cdE,AspDir[2]);
            MaxDownHill(SlopeAsp.z,SlopeAsp.zsw,SlopeAsp.znw,UseDiaSpace,sl[3],cdSW,cdNW,AspDir[3]);
            MaxDownHill(SlopeAsp.z,SlopeAsp.zse,SlopeAsp.zne,UseDiaSpace,sl[4],cdSE,cdNE,AspDir[4]);
            for j := 2 to 4 do if sl[j] > sl[1] then begin
                sl[1] := sl[j];
                SlopeAsp.Dir := AspDir[j];
            end {for j};
            SlopeAsp.Slope := Sl[1];
            SlopeAsp.AspectDir := 45.0 * Ord(SlopeAsp.Dir);
         end;
         if IsPit(SlopeAsp) then SlopeAsp.Dir := cdPit;
      end;
      SlopeAsp.SlopePercent := 100 * SlopeAsp.Slope;
      SlopeAsp.SlopeDegree := ArcTan(SlopeAsp.Slope) / DegToRad;
   end;
end {proc GetSlopeAndAspect};


procedure tDEMDataSet.GetSlopesInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
var
   Col,Row,Incr : integer;
   SlopeAspectRec : tSlopeAspectRec;
begin
   NPts := 0;
   Incr := GetSamplingSize(GridLimits);
   Incr := Incr * MDDef.StatSampleIncr;
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if (IncludeSeaLevel) or (not SeaLevelCell(Col,Row)) then begin
            if GetSlopeAndAspect(Col,Row,SlopeAspectRec) then begin
               Values[Npts] := SlopeAspectRec.SlopePercent;
               inc(NPts);
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
end;


function tDEMDataSet.SlopePercent(XGrid,YGrid : integer) : float64;
var
  SlpAsp : tSlopeAspectRec;
begin
   GetSlopeAndAspect(xgrid,ygrid,SlpAsp);
   Result := SlpAsp.SlopePercent;
end;


function tDEMDataSet.GetSlopeAndAspectFromLatLong(Lat,Long : float64; var SlopeAspectRec : tSlopeAspectRec) : boolean;
var
   xg,yg  : integer;
begin
    LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
    Result := GetSlopeAndAspect(xg,yg,SlopeAspectRec);
end;


function tDEMDataSet.SlopePercentFromLatLong(Lat,Long : float64) : float64;
var
   xgrid,ygrid : float32;
   SlopeAsp : tSlopeAspectRec;
begin
   LatLongDegreeToDEMGrid(Lat,Long,xgrid,ygrid);
   GetSlopeAndAspect(round(xgrid),round(ygrid),SlopeAsp);
   Result := SlopeAsp.SlopePercent;
end;



function tDEMDataSet.GetEvansParams(Col,Row,RegionSize : integer; var SlopeDeg,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve : float64) : boolean;
const
   Tolerance = 0.000000000001;
var
   SqED,z1,z2,z3,z4,z6,z7,z8,z9,a,b,c,d,e,SqABC,
   xsp,ysp,asp : float64;
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
begin
   if SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse,RegionSize) then begin
      Result := true;
      ysp := AverageYSpace * RegionSize;
      xsp := XSpaceByDEMrow^[Row] * RegionSize;

      Asp := 0.5 * (xsp + ysp);
      z7 := zsw - z;
      z8 := zs - z;
      z9 := zse - z;
      z4 := zw - z;
      //z5 := 0;
      z6 := ze - z;
      z1 := znw - z;
      z2 := zn - z;
      z3 := zne - z;

      A := ((z1 + z3 + z4 + z6 + z7 + z9) / 6 - (z2 + {z5 +} z8) / 3) / XSp / YSp;
      B := ((Z1 + z2 + z3 + z7 + z8 + z9) / 6 - (z4 + {z5 +} z6) / 3) / XSp / YSp;
      C := (z3 + z7 -z1 - z9) / 4  / XSp / YSp;
      D := (z3 + z6 + z9 - z1 - z4 - z7) / 6 / XSp;
      E := (z1 + z2 + z3 - z7 - z8 - z9) / 6 / YSp;
      //F := (2 * ( z2 + z4 + z6 - z8) - (z1 + z3 + z7 + z9) {+ 5 * z5} ) / 9;

      SqABC := sqrt(sqr(a-b) + sqr(c));
      MaxCurve := 20 * (ASp)*(-a - b + SqABC);
      MinCurve := 20 * (ASp)*(-a - b - SqABC);
      SqED := (sqr(E) + sqr(D));
      if (SqED > Tolerance) then begin
         SlopeCurvature {Profile convexity} := -200 * (A * sqr(D) + B * sqr(E) + C * D * E) / SqED / Math.Power(1 + SqED, 1.5);
         PlanCurvature {Plan convexity} := 200 * (B * sqr(D) + A * sqr(E) - C * D * E) / Math.Power(1 +SqED, 1.5);
         crossc := -20 * (ASp)*(b*d*d + a*e*e - c*d*e)/  SqED;
      end
      else begin
         SlopeCurvature := 0;
         PlanCurvature := 0;
         CrossC := 0;
      end;
      SlopeDeg := arctan(sqrt(sqED)) / DegToRad;
   end
   else Result := false;
end;


function tDEMDataSet.QuickRelief(Col,Row,BoxSize : integer; var Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_Relf : float32) : boolean;
var
   Limits : tGridLimits;
begin
   SimpleBoxAroundPoint(Col,Row,BoxSize,Limits.XGridLow,Limits.YGridLow,Limits.XGridHigh,Limits.YGridHigh);
   Result := QuickRelief(Col,Row,Limits,Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_relf);
end;


function tDEMDataSet.QuickRelief(Col,Row : integer; Limits : tGridLimits; var Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_Relf : float32) : boolean;
var
   z,ave : float32;
begin
   Result := false;
   if GetElevMeters(Col,Row,z) then begin
      BoxAreaExtremeElevations(Limits,BaseLevel,Summit,ave);
      Relief := Summit - Baselevel;
      GeoRelief := Summit - z;
      Dropoff := z - BaseLevel;
      Elev_relf := (ave-BaseLevel) / (Summit - BaseLevel);
      Result := true;
   end;
end;


function tDEMDataSet.PointHasSpecifiedRelief(Col,Row,BoxSize,SampleFactor : integer; Relief : float64) : boolean;
var
   x1,x2,y1,y2,x,y : integer;
   z,zt : float32;
begin
   SimpleBoxAroundPoint(Col,Row,BoxSize,x1,y1,x2,y2);
   if GetElevMeters(Col,Row,zt) then begin
      x := x1;
      while x <= x2 do begin
        y := y1;
        while y <= y2 do begin
           if GetElevMeters(x,y,z) and (zt > z + Relief) then begin
             Result := true;
             exit;
            end;
            inc(y,SampleFactor);
        end;
        inc(x,SampleFactor);
      end;
   end;
   Result := false;
end;


function tDEMDataSet.GetRelief(Col,Row,BoxSize : integer; var AvgElev,Relief,ElevStdDev,PClower,TPI : float32) : boolean;
var
   z,MinZ,MaxZ,zc : float32;
   NumLower,NumInBox,x1,x2,y1,y2,x,y : integer;
   zs : ^bfarray32;
   MomentVar : tMomentVar;
begin
   MinZ := MaxSmallInt;
   MaxZ := -MaxSmallInt;
   SimpleBoxAroundPoint(Col,Row,BoxSize,x1,y1,x2,y2);
   MomentVar.NPts := 0;
   NumLower := 0;
   NumInBox := 0;
   if GetElevMeters(Col,Row,zc) then begin
      New(zs);
      for x := x1 to x2 do begin
         for y := y1 to y2 do begin
            inc(NumInBox);
            if GetElevMeters(x,y,z) then  begin
               if (z > MaxZ) then MaxZ := z;
               if (z < MinZ) then MinZ := z;
               if (z < zc) then inc(NumLower);
               zs^[MomentVar.NPts] := z;
               inc(MomentVar.NPts);
             end
             else inc(MomentVar.Missing);
         end;
      end;
      Relief := MaxZ - MinZ;
      if EnoughPoints(MomentVar) then begin
         Moment(zs^,MomentVar,msAfterStdDev);
         Result := true;
         PCLower := 100 * NumLower / MomentVar.NPts;
         TPI:= (zc - AvgElev);
      end
      else Result := false;
      Dispose(zs);
      AvgElev := MomentVar.mean;
      ElevStdDev := MomentVar.SDev;
   end
   else Result := false;
end;


procedure tDEMDataSet.PlanCMoments(GridLimits: tGridLimits; var PlanCMoment : tMomentVar);
var
   zvs : ^bfarray32;
begin
   New(zvs);
   GetPlanCInLongArray(GridLimits,PlancMoment.NPts,zvs^,false);
   moment(zvs^,PlanCMoment,msBeforeMedian);
   Dispose(zvs);
end;

procedure tDEMDataSet.ProfCMoments(GridLimits: tGridLimits;  var ProfCMoment : tMomentVar);
var
   zvs : ^bfarray32;
begin
   New(zvs);
   GetProfCInLongArray(GridLImits,ProfCMoment.NPts,zvs^,false);
   moment(zvs^,ProfCMoment,msBeforeMedian);
   Dispose(zvs);
end;


procedure tDEMDataSet.BothOpennessMoments(GridLimits: tGridLimits; var UpOpenMoment,DownOpenMoment : tMomentVar);
var
   ups, downs : ^bfarray32;
begin
   New(ups);
   New(downs);
   GetBothOpennessInLongArray(GridLImits,UpOpenMoment.NPts,ups^,downs^,false);
   DownOpenMoment := UpOpenMoment;
   moment(ups^,UpOpenMoment,msBeforeMedian);
   moment(downs^,DownOpenMoment,msBeforeMedian);
   Dispose(ups);
   Dispose(downs);
end;


procedure tDEMDataSet.SlopeMoments(GridLimits: tGridLimits; var SlopeMoment : tMomentVar);
var
   zvs : ^bfarray32;
begin
   New(zvs);
   SlopeMomentsWithArray(GridLimits,SlopeMoment,zvs^);
   Dispose(zvs);
end;


procedure tDEMDataSet.RoughnessMomentsWithArray(GridLimits: tGridLimits; var MomentVar : tMomentVar; var zvs : bfarray32);
begin
   InitializeMomentVar(MomentVar);
   GetRoughnessInLongArray(GridLimits,MomentVar.Npts,zvs);
   moment(zvs,MomentVar,msAll);
end;


procedure tDEMDataSet.GetRoughnessInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32);
var
   Col,Row,Incr : integer;
   SlopeAspectRec : tSlopeAspectRec;
   Ruff1 : float32;
begin
   NPts := 0;
   Incr := GetSamplingSize(GridLimits);
   Incr := Incr * MDDef.StatSampleIncr;
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
          if RoughnessFromSlopeSTD(Col,Row,MDDef.RoughnessBox,Ruff1) then begin
            if GetSlopeAndAspect(Col,Row,SlopeAspectRec) then begin
               Values[Npts] := Ruff1;
               inc(NPts);
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
end;


procedure tDEMDataSet.SlopeMomentsWithArray(GridLimits: tGridLimits; var SlopeMoment : tMomentVar; var zvs : bfarray32);
var
   Nover30,Nover50 : integer;
   I: Integer;
begin
   nOver30 := 0;
   nOver50 := 0;
   InitializeMomentVar(SlopeMoment);
   GetSlopesInLongArray(GridLimits,SlopeMoment.NPts,zvs,false);
   for I := 0 to pred(SlopeMoment.NPts) do begin
      if (zvs[i] >= 30) then begin
         inc(nOver30);
         if zvs[i] >= 50 then inc(nOver50);
      end;
   end;
   Over30PercentSlope := 100.0 * nOver30 / SlopeMoment.NPts;
   Over50PercentSlope := 100.0 * nOver50 / SlopeMoment.NPts;
   Moment(zvs,SlopeMoment,msAll);
end;


function tDEMDataSet.ElevationMoments(GridLimits: tGridLimits) : tMomentVar;
var
   zvs : ^bfarray32;
begin
   New(zvs);
   ElevationMomentsWithArray(GridLimits,Result,zvs^);
   Dispose(zvs);
end;


procedure tDEMDataSet.ElevationMomentsWithArray(GridLimits: tGridLimits; var MomentVar : tMomentVar; var zvs : bfarray32);
begin
   InitializeMomentVar(MomentVar);
   GetElevationsInLongArray(GridLimits,MomentVar.NPts,zvs,false);
   moment(zvs,MomentVar,msAll);
end;

procedure tDEMDataSet.ElevationStatistics(var Mean,Std,AveDev : float32; UseZero : boolean);
var
   MomentVar : tMomentVar;
begin
   MomentVar := ElevationMoments(FullDEMGridLimits);
   Mean := MomentVar.mean;
   Std := MomentVar.sdev;
   AveDev := MomentVar.avg_dev;
end;


procedure tDEMDataSet.GetDEMMeanStd;
var
   AveDev : float32;
begin
   ElevationStatistics(Z_Mean,Z_Std,AveDev,true);
end;


function tDEMDataSet.CreatePartDEMHistogram(GridLimits: tGridLimits) : TThisBaseGraph;
var
   Distributions,Legends : tStringList;
   Values : ^Petmath.bfarray32;
   Max,Min,BinSize : float32;
   NPts : int64;
   fName : PathStr;
begin
   Distributions := tStringList.Create;
   Legends := tStringList.Create;
   New(Values);
   GetElevationsInLongArray(GridLimits,NPts,Values^);
   Max := DEMHeader.MaxElev;
   Min := DEMHeader.MinElev;
   fName := Petmar.NextFileNumber(MDtempDir,AreaName + '_' ,'.z');
   Distributions.Add(SaveSingleValueSeries(npts,Values^,fName));
   Legends.Add(AreaName);
   Dispose(Values);
   BinSize := (Max - Min) / 200;
   //if (Max-Min > 25) then BinSize := round(BinSize);
   Result := CreateMultipleHistogram(MDDef.CountHistograms,Distributions,Legends,'', '',200,Min,Max,BinSize);
end;


function tDEMDataSet.CreateWholeDEMHistogram : TThisBaseGraph;
begin
   Result := CreatePartDEMHistogram(FullDEMGridLimits);
end;

{$EndIf}


function tDEMDataSet.ContourLineCrossing(x, y: integer; z : float64): boolean;
const
   b = 0.001;
var
  zs : array[0..8] of float32;
  i : integer;
begin
   Result := false;
   GetNineElevMeters(x,y,zs[1],zs[2],zs[3],zs[4],zs[0],zs[5],zs[6],zs[7],zs[8]);
   for i := 0 to 8 do zs[i] := zs[i] - z;
   if abs(zs[0]) < b then begin
      for i := 1 to 8 do begin
          if abs(zs[i] - zs[0]) > b then begin
             Result := true;
             exit;
          end;
      end;
   end
   else if zs[0] > 0 then begin
      for i := 1 to 8 do begin
          if zs[i] < 0 then begin
             Result := true;
             exit;
          end;
      end;
   end
   else if zs[0] < 0 then begin
      for i := 1 to 8 do begin
          if zs[i] > 0 then begin
             Result := true;
             exit;
          end;
      end;
   end;
end;


function tDEMDataSet.FigureEntropy : float64;
type
   tCountArray = array[0..5000] of LongInt;
   tCountReals = array[0..5000] of float64;
var
   CountArray : ^tCountArray;
   CountReals : ^tCountReals;
   NumValues,
   i,j,z : integer;
begin
   if (DEMheader.MaxElev - DEMheader.MinElev > 5000) or (DEMheader.DEMPrecision = FloatingPointDEM) then begin
      Result := -1;
      exit;
   end;
   New(CountArray);
   New(CountReals);
   for i := 0 to 5000 do begin
      CountArray^[i] := 0;
      CountReals^[i] := 0;
   end;

   NumValues := 0;
   for i := 1 to (DEMheader.NumCol-2) do begin
      for j := 1 to (DEMheader.NumRow-2) do begin
         z := SmallIntElevations[i]^[j];
         if (z <> MaxSmallInt) then begin
            inc(CountArray^[z-round(DEMheader.MinElev)]);
            inc(NumValues);
         end {if};
      end {for j};
   end {for i};

   Result := 0;
   for z := round(DEMheader.MaxElev) downto round(DEMheader.MinElev) do begin
      i := z - round(DEMheader.MinElev);
      if CountArray^[i] > 0 then Result := Result - CountArray^[i]/NumValues * ln(CountArray^[i]/NumValues);
   end;
   Dispose(CountArray);
   Dispose(CountReals);
end;



procedure tDEMDataSet.DisposeNormals;
var
   Col     : integer;
begin
   if (Normals <> Nil) then begin
      {$If Defined(RecordDEMMemoryAllocations) or Defined(RecordNormalInit) or Defined(RecordSSO)} WriteLineToDebugFile('tDEMDataSet.DisposeNormals in'); {$EndIf}
      for Col := 0 to pred(DEMheader.NumCol) do begin
         FreeMem(Normals^[Col],DEMheader.NumRow * SizeOf(VectorType));
      end;
      Dispose(Normals);
      {$If Defined(RecordDEMMemoryAllocations) or Defined(RecordNormalInit) or Defined(RecordSSO)} WriteLineToDebugFile('tDEMDataSet.DisposeNormals out'); {$EndIf}
   end;
end;


function tDEMDataSet.NormalAtPoint(Col,Row : integer; var n1,n2,n3 : float32) : boolean;
var
  SlopAsp : tSlopeAspectRec;
begin
   Result := GetSlopeAndAspect(Col,Row,SlopAsp);
   if Result then begin
      if (SlopAsp.SlopeDegree < 0.0001) then SlopAsp.AspectDir := 0;
      n1 := sinDeg(SlopAsp.SlopeDegree) * sinDeg(SlopAsp.AspectDir);
      n2 := sinDeg(SlopAsp.SlopeDegree) * cosDeg(SlopAsp.AspectDir);
      n3 := cosDeg(SlopAsp.SlopeDegree);
   end;
end;

function tDEMDataSet.DownhillVectorAtPoint(Col,Row : integer; var n1,n2,n3 : float32) : boolean;
var
  SlopAsp : tSlopeAspectRec;
begin
   Result := GetSlopeAndAspect(Col,Row,SlopAsp);
   if Result then begin
      if (SlopAsp.SlopeDegree < 0.0001) then SlopAsp.AspectDir := 0;
      SlopAsp.SlopeDegree := 90 - SlopAsp.SlopeDegree;
      n1 := sinDeg(SlopAsp.SlopeDegree) * sinDeg(SlopAsp.AspectDir);
      n2 := sinDeg(SlopAsp.SlopeDegree) * cosDeg(SlopAsp.AspectDir);
      n3 := cosDeg(SlopAsp.SlopeDegree);
   end;
end;



procedure tDEMDataSet.InitializeNormals(var NumPts : Integer);
var
   j,Col,Row     : integer;
   MissingVector : VectorType32;
begin
   if (Normals = Nil) then begin
      {$If Defined(RecordDEMMemoryAllocations) or Defined(RecordNormalInit) or Defined(RecordSSO)} WriteLineToDebugFile('allocate tDEMDataSet.InitializeNormals, pointer size=' + IntToStr(DEMheader.NumRow * SizeOf(VectorType))); {$EndIf}
      for j := 1 to 3 do MissingVector[j] := MaxSmallint;
      New(Normals);
      NumPts := 0;
      for Col := 0 to pred(DEMheader.NumCol) do begin
         GetMem(Normals^[Col],DEMheader.NumRow * SizeOf(VectorType));
         for Row := 0 to pred(DEMheader.NumRow) do begin
            if NormalAtPoint(Col,Row,Normals^[Col]^[Row][1],Normals^[Col]^[Row][2],Normals^[Col]^[Row][3]) then
               inc(NumPts)
            else Normals^[Col]^[Row] := MissingVector;
         end;
      end;
      {$If Defined(RecordDEMMemoryAllocations) or Defined(RecordNormalInit)or Defined(RecordSSO)} WriteLineToDebugFile('allocated tDEMDataSet.InitializeNormals'); {$EndIf}
   end;
end;


function tDEMDataSet.FindReliefInflectionGraph(xg,yg : integer; var Distance,Relief : float64) : boolean;
var
   i,Plat : integer;
   Summit,BaseLevel,GeoRelief,ThisRelief,ThisDistance,Dropoff,AvgElev : float32;
begin
    i := Round(MDDef.FirstBoxSize / AverageSpace);
    Result := false;
    Relief := -12;
    repeat
       ThisDistance := i * AverageSpace;
       QuickRelief(xg,yg,round(ThisDistance),ThisRelief,Summit,BaseLevel,GeoRelief,Dropoff,AvgElev);
       if (ThisRelief > Relief) then begin
          Plat := 0;
          Relief := ThisRelief;
          Distance := ThisDistance;
       end
       else begin
          inc(Plat);
          if (Plat > MDDef.PlateauTolerance) then begin
             Result := true;
          end;
       end;
       inc(i);
    until Result or (i > DEMheader.NumCol) or (i > DEMheader.NumRow) or (ThisDistance > MDdef.LastBoxSize);
end;




procedure tDEMDataSet.FractalBox(GridLimits: tGridLimits; var FracDim,r : float32; SkipDraw : boolean = false; CloseGraph : boolean = false);
var
   Results : tStringList;
   DoResults : boolean;

   function DoBox(Col,Row,Step : integer; var area : float64) : boolean;
   var
      a,b,c,d : float32;
      e, w,x,y,z, o,p,q,r, sa,sb,sc,sd, aa,ab,ac,ad : float64;
      XSpace,YSpace,DiaSpace : float64;
   begin
      Result := false;
      //     d        c
      //     a        b
      try
         if GetElevMeters(Col,Row,a) and GetElevMeters(Col+Step,Row,b) and
            GetElevMeters(Col+Step,Row+Step,c) and GetElevMeters(Col,Row+Step,d) then begin
                e := 0.25 * (a + b + c + d);
                XSpace := Step;
                YSpace := Step;
                DiaSpace := 0.5 * sqrt_2 * Step;

                w := sqrt( sqr(a-b) + sqr(XSpace));
                x := sqrt( sqr(b-c) + sqr(YSpace));
                y := sqrt( sqr(c-d) + sqr(XSpace));
                z := sqrt( sqr(a-d) + sqr(YSpace));

                o := sqrt( sqr(a-e) + sqr(DiaSpace));
                p := sqrt( sqr(b-e) + sqr(DiaSpace));
                q := sqrt( sqr(c-e) + sqr(DiaSpace));
                r := sqrt( sqr(d-e) + sqr(DiaSpace));

                sa := 0.5 * (w + p + o);
                sb := 0.5 * (x + p + q);
                sc := 0.5 * (y + q + r);
                sd := 0.5 * (z + o + r);

                aa := sqrt( abs( sa * (sa - w) * (sa - p) * (sa - o)));
                ab := sqrt( abs( sb * (sb - x) * (sb - p) * (sb - q)));
                ac := sqrt( abs( sc * (sc - y) * (sc - q) * (sc - r)));
                ad := sqrt( abs( sd * (sd - z) * (sd - o) * (sd - r)));

                area := aa + ab + ac + ad;
                Result := true;
         end;
      except
         on Exception do begin
            Result := false;
            {$IfDef TriPrismErrors} MessageToContinue('TriPrism DoBox error'); {$EndIf}
         end;
      end;
   end;

   function DoStepArea(Step : integer) : float64;
   var
      Col,Row,Total,Missing : integer;
      MissingMult,EdgeMult,
      AreaInc    : float64;
   begin
      try
         Result := 0;
         Missing := 0;
         Total := 0;
         Col := GridLimits.XGridLow;
         while (Col < pred(GridLimits.XGridHigh-Step)) do begin
            Row := GridLimits.YGridLow;
            while (Row < pred(GridLimits.YGridHigh-Step))  do begin
               if DoBox(Col,Row,Step,AreaInc) then Result := Result + AreaInc
               else inc(Missing);
               inc(Total);
               inc(Row,Step);
            end;
            inc(Col,Step);
         end;
         if (Result > 0.00001) and (Total > Missing) then begin
            if (Missing > 0) then begin
               MissingMult := Total / (Total - Missing);
            end
            else MissingMult := 1;
            EdgeMult := (GridLimits.XGridHigh-GridLimits.XGridLow) * (GridLimits.YGridHigh-GridLimits.YGridLow) /  (Total * Step * Step);
            if DoResults then Results.Add(IntegerToString(Total,8) + IntegerToString(Missing,8) + RealToString(Step * AverageSpace,8,1) + RealToString(MissingMult,10,2)
                 + RealToString(EdgeMult,8,2) + RealToString(Result,18,2) + RealToString(Result * MissingMult * EdgeMult,18,2));
            Result := Result * MissingMult * EdgeMult;
         end;
      except
         on Exception do begin
            Result := 0.0;
            {$IfDef TriPrismErrors} MessageToContinue('TriPrism DoStepArea error'); {$EndIf}
         end;
      end;
   end;

var
   Step,n,Valid : integer;
   a,b,Area : float32;
   Graph :  TThisBaseGraph;
   v         : array[1..2] of float32;
   TStr      : ShortString;
   rFile : file;
begin
   {$IfDef TriPrismResults}
      DoResults := true;
      WriteLineToDebugFile('');
   {$Else}
      DoResults := (not SkipDraw);
   {$EndIf}
   ShowHourglassCursor;
    try
      try
         Graph := TThisBaseGraph.Create(Application);
         //Graph.GraphDraw.SkipDrawing := SkipDraw;
         if DoResults then begin
            Results := tStringList.Create;
            Results.Add('Triangular prism fractal dimension, ' + AreaName);
            Results.Add('');
            Results.Add('  Boxes   Holes   Size (m)   Hole    Edge     Raw area         Corrected Area');
            Results.Add('==============================================================================');
         end;

         with Graph,GraphDraw do begin
            VertLabel := 'log(surface area)';
            HorizLabel := 'log(resolution area)';
            OpenDataFile(rfile);
            Valid := 0;
            Step := 1;
            repeat
               Area := DoStepArea(Step);
               if (Area > 0.0001) then begin
                  v[1] := ln(sqr(Step));
                  v[2] := ln(area);
                  BlockWrite(rfile,v,1);
                  inc(Valid);
               end;
               Step := Step * 2;
            until (Step > DEMheader.NumCol) or (Step > DEMheader.NumRow);
            CloseFile(RFile);
         end;

         Graph.AutoScaleAndRedrawDiagram;
         if (Valid >= 4) then begin
            Graph.FitGraph(true,2,Graph.GraphDraw.DataFilesPlotted.Strings[0], a,b,r,n);
            FracDim := (2 - b);
            TStr := RealToString(FracDim,-6,2) + ' ';
            if DoResults then begin
               Results.Add('');
               Results.Add('Fractal dimension: ' + TStr);
            end;
         end
         else begin
            FracDim := -999;
            TStr := '';
            {$IfDef TriPrismErrors} MessageToContinue('Not enough valid (' + IntToStr(Valid) + ')'); {$EndIf}
         end;

         if DoResults then begin
            if not SkipDraw then begin
               Graph.Caption := 'Triangular prism fractal, ' + TStr + AreaName;
               Petmar.DisplayAndPurgeStringList(Results,'Triangular Prism Fractal, ' + AreaName);
            end;
            {$IfDef TriPrismResults}
               WriteStringListToDebugFile(Results);
               Results.Free;
            {$EndIf}
         end;
      except
          on Exception do begin
            FracDim := -999;
            {$IfDef TriPrismErrors} MessageToContinue('TriPrism overall exception'); {$EndIf}
          end;
      end;
   finally
      if CloseGraph and (Graph <> Nil) then begin
         Graph.Close;
         Graph.Free;
      end;
   end;
   ShowDefaultCursor;
end;


procedure tDEMDataSet.EntireDEMFractalBox;
var
   FracDim,r  : float32;
begin
   FractalBox(FullDEMGridLimits,FracDim,r);
end;


procedure tDEMDataSet.VariogramGamma(GridLimits: tGridLimits; var EastWest,NorthSouth,NESW,NWSE : float32);
var
   Num,Col,Row : integer;
   Elev  : tElevFloatArray;
begin
   ClipDEMGrid(GridLimits.XGridLow,GridLimits.YGridLow);
   ClipDEMGrid(GridLimits.XGridHigh,GridLimits.YGridHigh);
   Num := 0;
   EastWest := 0;
   NorthSouth := 0;
   NESW := 0;
   NWSE := 0;
   for Col := GridLimits.XGridLow to pred(GridLimits.XGridHigh) do begin
      for Row := GridLimits.YGridLow to pred(GridLimits.YGridHigh) do begin
          if GetElevSquareMeters(Col + 0.5, Row - 0.5,Elev) then begin
             inc(Num);
             EastWest := EastWest + sqr(Elev[4]-Elev[3]);
             NorthSouth := NorthSouth + sqr(Elev[4]-Elev[1]);
             NESW := NESW + sqr(Elev[3]-Elev[1]);
             NWSE := NWSE + sqr(Elev[4]-Elev[2]);
          end;
      end;
   end;
   EastWest := EastWest / Num / sqr(AverageXSpace);
   NorthSouth := NorthSouth / Num / sqr(AverageYSpace);
   NESW := NESW / Num / sqr(AverageDiaSpace);
   NWSE := NWSE / Num / sqr(AverageDiaSpace);
end;


procedure tDEMDataSet.ComputeVariogram(GridLimits: tGridLimits);  //SkipDrawing : boolean);
const
   MaxDist = 5000;
type
   SVarrayType = array[0..MaxDist] of float32;
var
   ThisGraph : tThisBaseGraph;
   SVArray,DistX : array[1..4] of SVArrayType;
   Dist,MaxHoriz,n,Col,Row,i,j : integer;
   NumPts : array[1..4,0..MaxDist] of LongInt;
   v1,v2,v3,MaxSemiVar,dFactor, a,b,r,EastWest,NorthSouth,NESW,NWSE,
   SlopeEastWest,SlopeNorthSouth,SlopeNESW,SlopeNWSE : float32;
   z,zp   : float32;
   Rfile  : file;
   v      : array[1..2] of float32;
   TStr   : ShortString;
   Results : tStringList;


    procedure CheckNeighbor(DoCol,DoRow,i,DirInt : integer);
    begin
       if GetElevMeters(DoCol,DoRow,z) then begin
          SVArray[DirInt][i] := SVArray[DirInt][i] + sqr(zp - z);
          inc(NumPts[DirInt,i]);
       end;
    end {proc};


    procedure DistanceArray;
    var
       i : integer;
    begin
      if MDDef.VariogramOptionsRecord.OldMethod then begin
         for i := 1 to Dist do begin
            DistX[1][i] := dFactor * i * AverageYSpace;
            DistX[2][i] := dFactor * i * AverageXSpace;
            DistX[3][i] := dFactor * i * AverageDiaSpace;
            DistX[4][i] := dFactor * i * AverageDiaSpace;
         end;
      end;
    end;


begin
   {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram in for ' + AreaName);{$EndIf}
   with MDDef.VariogramOptionsRecord do if DoGraph or DoSlopes then begin
      Dist := round(DistanceOut / AverageXSpace);

      for j := 1 to 4 do begin
         for i := 0 to Dist do begin
            SVArray[j][i] := 0;
            NumPts[j,i] := 0;
         end {for i};
      end {for j};

      OldMethod := true;

      Col := GridLimits.XGridLow;
      if ShowSatProgress then StartProgress('Variogram ' + AreaName);
      while Col <= GridLimits.XGridHigh do begin
         if ShowSatProgress then UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
         Row := GridLimits.YGridLow;
         while Row <= GridLImits.YGridHigh do begin
            if GetElevMeters(Col,Row,zp) then begin
               if OldMethod then begin
                  i := 1;
                  while (i <= Dist) do begin
                     CheckNeighbor(Col,Row+i,i,1);
                     CheckNeighbor(Col,Row-i,i,1);
                     CheckNeighbor(Col+i,Row,i,2);
                     CheckNeighbor(Col-i,Row,i,2);
                     CheckNeighbor(Col+i,Row+i,i,3);
                     CheckNeighbor(Col-i,Row-i,i,3);
                     CheckNeighbor(Col-i,Row+i,i,4);
                     CheckNeighbor(Col+i,Row-i,i,4);
                     inc(i,GraphSkip);
                  end {for i};
               end;
            end {if};
            inc(Row,Skip);
         end {while};
         inc(Col,Skip);
      end {while};
      EndProgress;

      dFactor := 1;
      DistanceArray;

     {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram normalize'); {$EndIf}
      MaxSemiVar := 0;
      MaxHoriz := 0;
      for j := 1 to 4 do begin
         i := 1;
         while (i <= Dist) do begin
            if (NumPts[j,i] < PointsRequired) then SVArray[j][i] := MaxInt
            else begin
               {$IfDef RecordVariogramFull} WriteLineToDebugFile(IntegerToString(i,6) + IntegerToString(j,6) + RealToString(SVArray[j][i],18,2) + IntegerToString(NumPts[j,i],8)); {$EndIf}
               v1 := SVArray[j][i];
               v2 := NumPts[j,i];
               v3 := DistX[j][i];
               SVArray[j][i] := 0.5 * SVArray[j][i] / NumPts[j,i] / DistX[j][i];
               if SemiVar then SVArray[j][i] := sqrt(SVArray[j][i]);
               if (SVArray[j][i] > MaxSemiVar) then MaxSemiVar := SVArray[j][i];
               if (i > MaxHoriz) then MaxHoriz := 1;
            end;
            inc(i,GraphSkip);
         end {while};
      end {for j};
      {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram graph'); {$EndIf}
      ThisGraph := TThisBaseGraph.Create(Application);
      //ThisGraph.GraphDraw.SkipDrawing := SkipDrawing;
      TStr := AreaName;
      ThisGraph.GraphDraw.VertLabel := 'Variance (m)';
      if SemiVar then ThisGraph.GraphDraw.VertLabel := 'Semi ' + ThisGraph.GraphDraw.VertLabel;
      if LogLog then begin
         ThisGraph.GraphDraw.HorizLabel := 'Log Distance (m)';
         DFactor := 1;
         ThisGraph.GraphDraw.VertLabel := 'Log ' + ThisGraph.GraphDraw.VertLabel;
      end
      else begin
         DFactor := 0.001;
         ThisGraph.GraphDraw.HorizLabel := 'Distance (km)';
      end;
      DistanceArray;

      if (not BaseGraf.CreateGraphHidden) then ThisGraph.Caption := TStr + ' Semivariograms 4 directions';
      ThisGraph.GraphDraw.LegendList := tStringList.Create;
      ThisGraph.GraphDraw.LegendList.Add('N-S');
      ThisGraph.GraphDraw.LegendList.Add('E-W');
      ThisGraph.GraphDraw.LegendList.Add('NE-SW');
      ThisGraph.GraphDraw.LegendList.Add('NW-SE');

      {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram create files'); {$EndIf}
      for i := 1 to 4 do begin
         ThisGraph.GraphDraw.FileColors256[1] := ConvertTColorToPlatformColor(WinGraphColors[i]);
         ThisGraph.OpenDataFile(Rfile);
         j := 1;
         while (j <= Dist) do begin
            v[2] := SVArray[i][j];
            if v[2] < pred(MaxInt) then begin
               v[1] := DistX[i][j];
               if v[1] < dFactor * DistanceOut then begin
                  if LogLog then begin
                     v[1] := log10(v[1]);
                     v[2] := log10(v[2]);
                  end;
                  BlockWrite(rfile,v,1);
               end;
            end;
            inc(j,GraphSkip);
         end;
         CloseFile(Rfile);
      end;
      {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram autodraw'); {$EndIf}
      ThisGraph.AutoScaleAndRedrawDiagram;
   end;

   Results := Nil;
   if MDDef.VariogramOptionsRecord.DoSlopes or MDDef.VariogramOptionsRecord.DoGamma then begin
      Results := tStringList.Create;
   end;

   if MDDef.VariogramOptionsRecord.DoSlopes then begin
      SlopeNorthSouth := -999;
      SlopeEastWest   := -999;
      SlopeNESW       := -999;
      SlopeNWSE       := -999;
      for i := 0 to pred(ThisGraph.GraphDraw.DataFilesPlotted.Count) do begin
         ThisGraph.BestSegmentFitGraph(2,ThisGraph.GraphDraw.DataFilesPlotted.Strings[i], a,b,r,n,3,0.10);
         if (b > 0.01) and (b < 10) then case i  of
            0 : SlopeNorthSouth := b;
            1 : SlopeEastWest   := b;
            2 : SlopeNESW       := b;
            3 : SlopeNWSE       := b;
         end;
      end;
      Results.Add('Variogram Slopes and fractal Dimensions, ' + AreaName);
      Results.Add('N-S:'+ RealToString(SlopeNorthSouth,10,4) + RealToString(FracDimFromSlope2(SlopeNorthSouth),10,3));
      Results.Add('NE-SW:'+ RealToString(SlopeNESW,8,4) + RealToString(FracDimFromSlope2(SlopeNESW),10,3));
      Results.Add('E-W:'+ RealToString(SlopeEastWest,10,4) + RealToString(FracDimFromSlope2(SlopeEastWest),10,3));
      Results.Add('SE-NW:'+ RealToString(SlopeNWSE,8,4) + RealToString(FracDimFromSlope2(SlopeNWSE),10,3));
      Results.Add('');
      {$IfDef RecordVariogram} WriteStringListToDebugFile(Results); {$EndIf}
      if not MDDef.VariogramOptionsRecord.DoGraph then ThisGraph.Close;
   end;

   if MDDef.VariogramOptionsRecord.DoGamma then begin
      VariogramGamma(GridLimits,EastWest,NorthSouth,NESW,NWSE);
      Results.Add('Variogram Gammas, ' + AreaName);
      Results.Add('N-S:'+ RealToString(NorthSouth,10,4));
      Results.Add('NE-SW:'+ RealToString(NESW,8,4));
      Results.Add('E-W:'+ RealToString(EastWest,10,4));
      Results.Add('SE-NW:'+ RealToString(NWSE,8,4));
   end;
   if (Results <> Nil) then begin
      if MDDef.VariogramOptionsRecord.ShowTextOutput then Petmar.DisplayAndPurgeStringList(Results,'Variogram ' + AreaName)
      else Results.Free;
   end;
end;



{$IfDef ExDEMReports}
{$Else}
procedure tDEMDataSet.PointParameters(xgrid,ygrid : float64);
var
   Results,Results2 : tStringList;
   i : integer;
   SSOvars : tSSOvars;
   {$IfDef MultipleCurvatureMethods} PlanCurv,SlopeCurv : float64; {$EndIf}
   SlopeCurvature, PlanCurvature,crossc,MaxCurve,MinCurve,MaxSlp,Upward,Downward : float64;
   z : float32;
   SlopeAspectRec : tSlopeAspectRec;
begin
   Results := tStringList.Create;
   Results.Add(DEMLocationString(xgrid,ygrid));
   Results.Add('');
   if GetElevMeters(XGrid,YGrid,z) then Results.Add('Elev: ' + RealToString(z,8,2) + ' m');

   if GetSlopeAndAspect(round(xgrid),round(ygrid),SlopeAspectRec) then begin
      Results.Add('Slope:' + RealToString(SlopeAspectRec.SlopePercent,7,2) + '%' + RealToString(SlopeAspectRec.SlopeDegree,8,2) + '°');
      Results.Add('Aspect: ' + RealToString(SlopeAspectRec.AspectDir,6,1) + '°');
   end;

   {$IfDef MultipleCurvatureMethods}
      if GetCurvature(round(xgrid),round(ygrid),PlanCurv,SlopeCurv) then begin
         Results.Add('Plan curvature: ' + RealToString(PlanCurv,12,6));
         Results.Add('Slope curvature:' + RealToString(SlopeCurv,12,6));
      end;
   {$EndIf}

   {$IfDef ExGeostats}
   {$Else}
      if GetEvansParams(round(xgrid),round(ygrid),MDDef.WoodRegionRadiusPixels,MaxSlp,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve) then begin
         Results.Add('');
         Results.Add('Evans curvature (Region size=' + IntToStr(MDDef.WoodRegionRadiusPixels) + '):');
         Results.Add('   Plan curvature: ' + RealToString(PlanCurvature,13,6));
         Results.Add('   Slope curvature:' + RealToString(SlopeCurvature,12,6));
         Results.Add('   Cross curvature:' + RealToString(CrossC,12,6));
         Results.Add('   Max curvature: ' + RealToString(MaxCurve,14,6));
         Results.Add('   Min curvature:' + RealToString(MinCurve,14,6));
      end;

      Results2 := tStringList.Create;
      if FigureOpenness(round(xgrid),round(ygrid),MDDef.OpenGridBoxSize,Upward,Downward,Results2) then begin
         Results.Add('');
         Results.Add('Openness parameters (' + IntToStr(MDDef.OpenGridBoxSize) + ' m)');
         Results.Add('  Upward openness: ' + RealToString(Upward,12,2) + '°');
         Results.Add('  Downward openness:' + RealToString(Downward,12,2) + '°');
         Results.Add('');
         for i := 0 to pred(Results2.Count) do Results.Add('      ' + Results2.Strings[i]);
         Results.Add('');
      end;
      Results2.Free;

      if PointSSOComputations(round(xgrid),round(ygrid),MDDef.SSOBoxSizeMeters, SSOvars,false,true,false) then  begin
         Results.Add('SSO parameters (' + IntToStr(MDDef.SSOBoxSizeMeters) + ' m)');
         Results.Add('  Points in box: ' + IntToStr(SSOvars.NumPts));
         Results.Add('  Organization ln(s2/s3): ' + RealToString(SSOvars.s2s3,12,2));
         Results.Add('  Organization direction: ' + RealToString(SSOvars.TheDipDirs[3],6,1) + '°');
         Results.Add('  Relief: ' + RealToString(SSOvars.ElevRange,12,2) + ' m');
         Results.Add('  Region average elev: ' + RealToString(SSOvars.AvgElev,12,2) + ' m');
         Results.Add('  Region elev std dev: ' + RealToString(SSOvars.StdDevElev,12,2));
         Results.Add('  Region max slope: ' + RealToString(100*SSOvars.MaxSlope,12,2) + '%');
         Results.Add('  Roughness factor: ' + RealToString(SSOvars.RoughnessFactor,8,3));
      end;
   {$EndIf}
   ModalStringListDisplay(Results,'Point parameters');
end;


procedure tDEMDataSet.SlopeMethodsReportFromLatLong(lat,Long : float64; Title : shortstring = '');
var
   Col,Row : integer;
begin
   if LatLongDegreeToDEMGridInteger(Lat,Long,Col,Row) then SlopeMethodsReport(Col,Row,Title);
end;


procedure tDEMDataSet.SlopeMethodsReport(xp,yp : integer; Title : shortstring = '');
{$IfDef ExGeostats}
begin
{$Else}
var
   Results : tStringList;
   CurMethod : byte;
   SlopeAspectRec : tSlopeAspectRec;
begin
   SaveBackupDefaults;
   Results := tStringList.Create;
   Results.Add(AreaName);
   if (Title <> '') then Results.Add(Title);
   Results.Add('Point: ' + DEMLocationString(xp,yp));
   Results.Add('Classified as: ' + PointTypeName(ClassifyAPoint(xp,yp)));
   Results.Add('Col=' + IntToStr(xp) + '  & row=' + IntToStr(yp));
   Results.Add('');
   Results.Add('Neighborhood elevations (m)');
   GetSlopeAndAspect(xp,yp,SlopeAspectRec);
   Results.Add(  RealToString( SlopeAspectRec.znw,13,2) + RealToString( SlopeAspectRec.zn,13,2) + RealToString( SlopeAspectRec.zne,13,2) +  MessLineBreak +
                 RealToString( SlopeAspectRec.zw,13,2)  + RealToString( SlopeAspectRec.z,13,2)   + RealToString( SlopeAspectRec.ze,13,2)  +  MessLineBreak +
                 RealToString( SlopeAspectRec.zsw,13,2) + RealToString( SlopeAspectRec.zs,13,2)  + RealToString( SlopeAspectRec.zse,13,2) +  MessLineBreak + '  ' + MessLineBreak );
   Results.Add('');
   Results.Add('         Slope             Aspect         Method');
   for CurMethod := FirstSlopeMethod to LastSlopeMethod do begin
      MDdef.SlopeAlg := CurMethod;
      GetSlopeAndAspect(xp,yp,SlopeAspectRec);
      Results.Add(RealToString(SlopeAspectRec.SlopePercent,8,2) + '%   ' + RealToString(SlopeAspectRec.SlopeDegree,8,2) + '°   ' + RealToString(SlopeAspectRec.AspectDir,8,1) + '°    ' + SlopeMethodName(MDdef.SlopeAlg));
   end;
   DisplayAndPurgeStringList(Results,'Point Slope Algorithms');
   RestoreBackupDefaults
{$EndIf}
end;


procedure tDEMDataSet.DirectionalSlopesReport(Lat,Long : float64);
var
   MenuStr : AnsiString;
   x,y : integer;
   xgf,ygf : float32;
   SlopeAspectRec : tSlopeAspectRec;
begin
   LatLongDegreeToDEMGrid(Lat,Long,xgf,ygf);
   x := round(xgf);
   y := round(ygf);

   GetSlopeAndAspect(x,y,SlopeAspectRec);
   MenuStr := 'Point: ' + DEMLocationString(x,y) +  MessLineBreak +  MessLineBreak + 'Elevations: ' +  MessLineBreak +
              RealToString( SlopeAspectRec.znw,13,2) + RealToString( SlopeAspectRec.zn,13,2) + RealToString( SlopeAspectRec.zne,13,2) +  MessLineBreak +
              RealToString( SlopeAspectRec.zw,13,2)  + RealToString( SlopeAspectRec.z,13,2)   + RealToString( SlopeAspectRec.ze,13,2)  +  MessLineBreak +
              RealToString( SlopeAspectRec.zsw,13,2) + RealToString( SlopeAspectRec.zs,13,2)  + RealToString( SlopeAspectRec.zse,13,2) +  MessLineBreak + '  ' + MessLineBreak +
              'Slopes: ' +  MessLineBreak  +
              RealToString( 100*(SlopeAspectRec.znw-SlopeAspectRec.z)/AverageDiaSpace,12,2) + '%' + RealToString( 100*(SlopeAspectRec.zn-SlopeAspectRec.z)/AverageYSpace,12,2) + '%' + RealToString( 100*(SlopeAspectRec.zne-SlopeAspectRec.z)/AverageDiaSpace,12,2) + '%' +  MessLineBreak +
              RealToString( 100*(SlopeAspectRec.zw-SlopeAspectRec.z)/AverageXSpace,12,2) + '%' + '             ' + RealToString( 100*(SlopeAspectRec.ze-SlopeAspectRec.z)/AverageXSpace,12,2) + '%' +  MessLineBreak +
              RealToString( 100*(SlopeAspectRec.zsw-SlopeAspectRec.z)/AverageDiaSpace,12,2) + '%' + RealToString( 100*(SlopeAspectRec.zs-SlopeAspectRec.z)/AverageYSpace,12,2) + '%' + RealToString( 100*(SlopeAspectRec.zse-SlopeAspectRec.z)/AverageDiaSpace,12,2) + '%'+  MessLineBreak + '  ' +
              MessLineBreak +
              'Point slope:' + MessLineBreak +   RealToString(SlopeAspectRec.SlopePercent,8,2) + '%   ' + RealToString(SlopeAspectRec.SlopeDegree,8,2) + '°   aspect ' + RealToString(SlopeAspectRec.AspectDir,8,1) + '°    ' +
                   SlopeMethodName(MDdef.SlopeAlg);
   MessageToContinue(MenuStr);
end;

{$EndIf}

procedure tDEMDataSet.BoxStatsDB(BoxSize : integer = 0);
const
   MaxSize = 50000;
var
   Results : tStringList;
   Col,Row,x,y : integer;
   z1: float32;
   Lat,Long : float64;
   MomentVar : tMomentVar;
   fName : PathStr;
   xs : array[0..MaxSize] of float32;
begin
   if (BoxSize = 0) then ReadDefault('Box size (grid cells)',BoxSize);

   Results := tStringList.Create;
   Results.Add('LAT,LONG,MEAN,STD_DEV,MEDIAN,N');

   Col := 0;
   StartProgress('DB create');
   while Col <= DEMheader.NumCol do begin
      UpdateProgressBar(Col/DEMheader.NumCol);
      Row := 0;
      while Row <= DEMheader.NumRow do begin
         MomentVar.NPts := 0;
         for x := Col to Col + pred(BoxSize) do begin
            for y := Row to Row + pred(BoxSize) do begin
               if GetElevMeters(x,y,z1) then begin
                  xs[MomentVar.NPts] := z1;
                  inc(MomentVar.NPts);
               end;
            end;
         end;
         if (MomentVar.NPts > 0) then begin
            Moment(xs,MomentVar,msAll);
            DEMGridToLatLongDegree(Col + succ(Boxsize div 2), Row + succ(Boxsize div 2),Lat,Long);
            Results.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7) + ',' + RealToString(MomentVar.mean,-12,-2) + ',' + RealToString(MomentVar.SDev,-12,-2) + ',' + RealToString(MomentVar.Median,-12,-2) + ',' + IntToStr(MomentVar.NPts));
         end;
         inc(Row,BoxSize);
      end;
      inc(Col,BoxSize);
   end;
   EndProgress;
   fName := System.IOUtils.TPath.Combine(MDTempDir, AreaName + '_stats.csv');

   {$IfDef NoMapOptions}
   {$Else}
      SelectionMap.StringListToLoadedDatabase(Results,fName);
   {$EndIf}
end;


{$IfDef AllowDEMGeomorph}
   procedure tDEMDataSet.GetBoxGridSizeDiameter(BoxSizeMeters : integer; var XBoxGridSize,YBoxGridSize : integer; var BoxSizeString : shortstring);
   begin
       XBoxGridSize := round(BoxSizeMeters / AverageXSpace);
       YBoxGridSize := round(BoxSizeMeters / AverageYSpace);
       BoxSizeString := ' (' +  IntToStr(BoxSizeMeters) + ' m)' ;
   end;
{$EndIf}


function tDEMDataSet.InTerrainCategory(x,Y : integer; TerrainCategory : tTerrainCatDefinition) : boolean;
var
   Relief,Summit,BaseLevel,GeoRelief,Dropoff,AvgElev,z : float32;
   SlopeAspectRec : tSlopeAspectRec;
begin
   Result := false;
   if GetElevMeters(x,y,z) then with TerrainCategory do begin
      if (not ElevationDEM) then Result := (z >= CatMinElev) and (z <= CatMaxElev)
      else begin
         Result := true;
         if UseElevation then Result := (z >= CatMinElev) and (z <= CatMaxElev);
         if UseSlope or UseAspect then begin
            if not GetSlopeAndAspect(x,y,SlopeAspectRec) then begin
               Result := false;
               exit;
            end;
         end;
         if UseSlope then Result := Result and (SlopeAspectRec.SlopePercent >= CatMinSlope) and (SlopeAspectRec.SlopePercent <= CatMaxSlope);
         if UseRelief then begin
            QuickRelief(x,y,CatReliefRadius,Relief,Summit,BaseLevel,GeoRelief,Dropoff,AvgElev);
            Result := Result and (Relief >= CatMinRelief) and (Relief <= CatMaxRelief);
         end;
         if UseAspect then Result := Result and ((SlopeAspectRec.Dir in CatAspects) or (SlopeAspectRec.SlopePercent < 0.0001));
      end;
   end;
end;


function tDEMDataSet.IsSpire(Col,Row,dx,dy : integer; var SpireHeightM : float32; var NumLower : integer) : boolean;
var
   z,zp : float32;
   Npts : integer;

      function ElevMeters(x,y : integer) : float32;
      begin
         if GetElevMeters(x,y,z) then begin
             SpireheightM := SpireHeightM + z;
             Inc(Npts);
         end;
      end;

begin
    Result := false;
    if GetElevMeters(Col,Row,z) then begin
       NumLower := 0;
       if GetElevMeters(Col+dx,Row,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col-dx,Row,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col,Row+dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col,Row-dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col+dx,Row+dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col-dx,Row+dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col-dx,Row+dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col+dx,Row-dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       Result := NumLower >= MDDef.SpireNeighborTolerance;
       if Result then begin
          SpireHeightM := 0;
          SpireHeightM := ElevMeters(Col+dx,Row) + ElevMeters(Col-dx,Row) + ElevMeters(Col,Row+dy) + ElevMeters(Col,Row-dy) + ElevMeters(Col+dx,Row+dy) + ElevMeters(Col-dx,Row+dy) + ElevMeters(Col-dx,Row+dy) + ElevMeters(Col+dx,Row-dy);
          SpireHeightM := SpireHeightM / NPts;
          SpireHeightM := z - SpireHeightM;
       end;
    end;
end;


{$IfDef MultipleCurvatureMethods}

function tDEMDataSet.GetCurvature(Col,Row : integer; var PlanCurvature,SlopeCurvature : float64) : boolean;
const
   Tolerance = 0.000000000001;
var
   xsp,ysp,
   znw,zw,zsw,zn,z,zs,zne,ze,zse,SqED,SqPQ,SqGH,P,Q,R,S,T,
   a,b,c,d,e,f,g,h,z1,z2,z3,z4,z6,z7,z8,z9,v1,v2,v3,v4,v5 : float64;
begin
   {all algorithms use the original notation; have the elevations normalized to the central point, z5; and retain but comment out any computations involving z5}
   //only works if all 9 neighbors are defined, so undefined near data voids and on edge of the grid
   if SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse,MDDef.CurvRegionSize) then begin
      GetCurvature := true;
      SlopeCurvature := 0;
      PlanCurvature := 0;

      ysp := AverageYSpace * MDDef.CurvRegionSize;
      xsp := XSpaceByDEMrow^[Row] * MDDef.CurvRegionSize;

      if (MDDef.CurvatureMethod = cmEvans) or (MDDef.CurvatureMethod = cmZevenbergenAndThorne) then begin
         {   1      2      3}
         {   4      5      6}
         {   7      8      9}
         z7 := zsw - z;
         z8 := zs - z;
         z9 := zse - z;
         z4 := zw - z;
         //z5 := 0;
         z6 := ze - z;
         z1 := znw - z;
         z2 := zn - z;
         z3 := zne - z;
         if (MDDef.CurvatureMethod = cmEvans) then begin
           {Evans, 1979, as defined in Woods PhD thesis}
            A := ((z1 + z3 + z4 + z6 + z7 + z9) / 6 - (z2 + {z5 +} z8) / 3) / xsp / ysp;
            B := ((Z1 + z2 + z3 + z7 + z8 + z9) / 6 - (z4 + {z5 +} z6) / 3) / xsp / ysp;
            C := (z3 + z7 -z1 - z9) / 4  / xsp / ysp;
            D := (z3 + z6 + z9 - z1 - z4 - z7) / 6 / xsp;
            E := (z1 + z2 + z3 - z7 - z8 - z9) / 6 / xsp;
            //F := (2 * ( z2 + z4 + z6 - z8) - (z1 + z3 + z7 + z9) {+ 5 * z5} ) / 9;

            SqED := (sqr(E) + sqr(D));
            if (SqED > Tolerance) then begin
               SlopeCurvature {Profile convexity} := -200 * (A * sqr(D) + B * sqr(E) + C * D * E) / SqED / Math.Power(1 + SqED, 1.5);
               PlanCurvature {Plan convexity} := 200 * (B * sqr(D) + A * sqr(E) - C * D * E) / Math.Power(1 + SqED, 1.5);
            end;
         end
         else if (MDDef.CurvatureMethod = cmZevenbergenAndThorne) then begin
            //A := ( (z1 + z3 + z7 + z9) / 4 - (z2 + z4 + z6 + z8) / 2 {+ z5}) / sqr(AverageXSpace) / sqr(AverageYSpace);
            //B := ( (Z1 + z3 - z7 - z9) / 4 - (z2 - z8) / 2 ) / sqr(AverageXSpace) / (AverageYSpace);
            //C := ( (-z1 + z3  - z7 + z9) / 4 + (z4 - z6) / 2 ) / (AverageXSpace) / sqr(AverageYSpace);
            D := ( (z4 + z6) / 2 {- z5}) / sqr(xsp);
            E := ( (z2 + z8) / 2 {- z5})  / sqr(ysp);
            F := ( -z1 + z3 + z7 - z9) / (4 * xsp * ysp);
            G := (-z4 + z6)  / (2 * xsp);
            H := (z2 - z8)  / (2 * ysp);
            //I := z5;
            SqGH := (sqr(G) + sqr(H));
            If (SqGH > Tolerance) then begin
               SlopeCurvature := 100 * -2 * (D * sqr(G) + E * sqr(H) + F * G * H) / SqGH;
               PlanCurvature := 100 * 2 * (D * sqr(H) + E * sqr(G) - F * G * H) / SqGH;
               //LaplacianCurvature := 100 * 2 * E + D;
            end
         end;
      end
      else begin
         {   7      8      9}
         {   4      5      6}
         {   1      2      3}
         z1 := zsw - z;
         z2 := zs - z;
         z3 := zse - z;
         z4 := zw - z;
         z6 := ze - z;
         z7 := znw - z;
         z8 := zn - z;
         z9 := zne - z;

         if (MDDef.CurvatureMethod = cmHeerdegenAndBeran) then begin
            {after Heerdegen, R.C., and Beran, M.A., 1982, Quantifying source areas through land surface curvature and shape: Journal of Hydrology, vol.57, p.359-373; modified to
             --work with variable x and y spacing;
             --have same sign for Plan curvature as others;
             --multiply both curvatures by 100;}
            v1 := z1 + z3 + z4 + z6 + z7 + z9;
            v2 := z1 + z2 + z3 + z7 + z8 + z9;
            v3 := z1 - z3 - z7 + z9;
            v4 := -z1 + z3 - z4 + z6 - z7 + z9;
            v5 := -z1 - z2 - z3 + z7 + z8 + z9;

            a := (0.3 * v1 - 0.2 * v2) / sqr(xsp);
            b := (0.3 * v2 - 0.2 * v1) / sqr(ysp);
            c := v3 / 4 / xsp / ysp;
            d := v4 / 6 / xsp;
            e := v5 / 6 / ysp;

            SqED := (sqr(E) + sqr(D));

            if SqED > Tolerance then  begin
               PlanCurvature := -200 * e * (c * d - 2 * a * e) / Math.Power(SqED,1.5);
               SlopeCurvature := -200 * (a * sqr(d) + b * sqr(e) + c * d * e) / (SqED * Math.Power((1.0 + SqED), 1.5));
            end;
         end
         else if (MDDef.CurvatureMethod = cmShary) then begin
            {after Shary, 1991, as given in Florinsky, 1998;  --multiply both curvatures by 100;}
            //r = d2z/dx2
            r := ( (z1 + z3 + z4 + z6 + z7 + z9) - 2 * (z2 + {z5 +} z8) ) / (3 * sqr(xsp));
            //r = d2z/dy2
            t := ( (z1 + z2 + z3 + z7 + z8 + z9) - 2 * (z4 + {z5 +} z6) ) / (3 * sqr(ysp));
            //s=d2z/dxdy
            s := (z3 + z7 - z1 - z9) / (4 * xsp * ysp);
            //p=dz/dx
            p := (z3 + z6 + z9 - z1 - z4 - z7) / (6 * xsp);
            //q=dz/dy
            q := (z1 + z2 + z3 - z7 - z8 - z9) / (6 * ysp);

            SqPQ := (sqr(P) + sqr(Q));
            if SqPQ > Tolerance then begin
               SqPQ := SqPQ * sqrt ( 1 + SqPQ);
               SlopeCurvature := -100 * (p * p * r  + 2 * P * q * s + q * q * t) / SqPQ;
               PlanCurvature := -100 * (q * q * r  - 2 * P * q * s + p * p * t) / SqPQ;
            end;
         end;
      end;
   end
   else GetCurvature := false;
end;
{$EndIf}

procedure tDEMDataSet.GetPlanCInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
var
   Col,Row,Incr : integer;
   SlopeDeg,crossc,MaxCurve,MinCurve,
   PlanCurvature,SlopeCurvature : float64;
 begin
   NPts := 0;
   Incr := GetSamplingSize(GridLimits);
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if (IncludeSeaLevel) or (not SeaLevelCell(Col,Row)) then begin
            if GetEvansParams(Col,Row,1,SlopeDeg,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve)  then begin
               Values[Npts] := PlanCurvature;
               inc(NPts);
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
end;


procedure tDEMDataSet.GetProfCInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
var
   Col,Row,Incr : integer;
   SlopeDeg,crossc,MaxCurve,MinCurve,
   PlanCurvature,SlopeCurvature : float64;
 begin
   NPts := 0;
   Incr := GetSamplingSize(GridLimits);
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if (IncludeSeaLevel) or (not SeaLevelCell(Col,Row)) then begin
            if GetEvansParams(Col,Row,1,SlopeDeg,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve)  then begin
               Values[Npts] := SlopeCurvature;
               inc(NPts);
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
end;

{$IfDef ExGeostats}
{$Else}

procedure tDEMDataSet.GetBothOpennessInLongArray(GridLimits: tGridLimits; var NPts : int64;   var UpValues,DownValues : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
var
   Col,Row,Incr : integer;
   UpOpen,DownOpen : float64;
begin
   NPts := 0;
   Incr := GetSamplingSize(GridLimits);
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if (IncludeSeaLevel) or (not SeaLevelCell(Col,Row)) then begin
            if FigureOpenness(Col,Row,MDDef.OpenGridBoxSize,UpOpen,DownOpen) then begin
               UpValues[Npts] := UpOpen;
               DownValues[Npts] := DownOpen;
               inc(NPts);
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
end;


function tDEMDataSet.FindLocationOfMaximum(GridLimits: tGridLimits; var xloc,yloc : integer; var LocMax : float32) : boolean;
var
   z   : float32;
   x,y : integer;
begin
   LocMax := -99e39;
   for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
      for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
         if GetElevMeters(x,y,z) then begin
            if (z > LocMax) then begin
               LocMax := z;
               xloc := x;
               yloc := y;
            end;
         end;
      end;
   end;
   Result := (LocMax > -99e39);
end;


function tDEMDataSet.FindLocationOfMinimum(GridLimits: tGridLimits; var xloc,yloc : integer; var LocMin : float32) : boolean;
var
   z   : float32;
   x,y : integer;
begin
   LocMin := 99e39;
   for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
      for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
         if GetElevMeters(x,y,z) then begin
            if (z < LocMin) then begin
               LocMin := z;
               xloc := x;
               yloc := y;
            end;
         end;
      end;
   end;
   Result := (LocMin < 99e39);
end;


function tDEMDataSet.FindLocationOfMultipleMaxima(GridLimits: tGridLimits; var NPts : integer; var Locations : array of tGridZ) : boolean;
var
   i : integer;

      procedure ReEnterPoints(NPts : integer);
      var
         i : integer;
      begin
         for i := 0 to pred(Npts) do begin
            SetGridElevation(Locations[i].x,Locations[i].y,Locations[i].z);
         end;
      end;

begin
   for i := 0 to pred(Npts) do begin
      if not FindLocationOfMaximum(GridLimits,Locations[i].x,Locations[i].y,Locations[i].z) then begin
         Result := false;
         ReEnterPoints(i);
         NPts := i;
         exit;
      end;
      SetGridMissing(Locations[i].x,Locations[i].y);
   end;
   ReEnterPoints(NPts);
   Result := true;
end;






