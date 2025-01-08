{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{ include file for demcoord              }
{________________________________________}



procedure tDEMDataSet.InitializeNormals;
var
   j,Col,Row     : integer;
   ns : array[1..3] of float32;
begin
   if ValidDEM(Normals[1]) and ValidDEM(Normals[2]) and ValidDEM(Normals[3]) then begin
      {$If Defined(RecordSSOCalc)or Defined(RecordNormalInit)} WriteLineToDebugFile('tDEMDataSet.InitializeNormals normals already created'); {$EndIf}
   end
   else begin
      {$If Defined(RecordDEMMemoryAllocations) or Defined(ShowDEMSSOCalc)} WriteLineToDebugFile('allocate tDEMDataSet.InitializeNormals, pointer size=' + IntToStr(DEMheader.NumRow * SizeOf(VectorType))); {$EndIf}
      for j := 1 to 3 do begin
         if not ValidDEM(Normals[j]) then begin
            Normals[j] := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_normals_' + IntToStr(j),euUndefined);
         end;
      end;
      {$If Defined(RecordDEMMemoryAllocations) or Defined(ShowDEMSSOCalc)} WriteLineToDebugFile('tDEMDataSet.InitializeNormals grids created'); {$EndIf}
      for Col := 0 to pred(DEMheader.NumCol) do begin
         for Row := 0 to pred(DEMheader.NumRow) do begin
            if NormalAtPoint(Col,Row,ns[1],ns[2],ns[3]) then begin
               for j := 1 to 3 do DEMGlb[Normals[j]].SetGridElevation(Col,Row,ns[j]);
            end;
         end;
      end;
      {$If Defined(RecordDEMMemoryAllocations) or Defined(ShowDEMSSOCalc)} WriteLineToDebugFile('tDEMDataSet.InitializeNormals normals defined'); {$EndIf}
   end;
   //{$If Defined(RecordDEMMemoryAllocations) or Defined(ShowDEMSSOCalc)} WriteLineToDebugFile('allocated tDEMDataSet.InitializeNormals'); {$EndIf}
end;


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


function tDEMDataSet.SSOComputations(GridLimits : tGridLimits; var SSOvars : tSSOvars; Plot : boolean; NetName,AspName : PathStr;
                 FilterGrid : integer = 0; FilterValue : integer = 0; LLText : shortstring = '') : boolean;
var
   xd,yd,Col,Row  : integer;
   MaxZ,MinZ,SumElev,SumSlope,
   SumSqSlope,SumSqElev : float64;
   zf : float32;
   fName : PathStr;
   Symbol : tFullSymbolDeclaration;
   SlopeAspectRec : tSlopeAspectRec;
   AspectStats : tAspectStats;
   M,S  : ^tTrendMatrix;
   s2   : VectorType;
   TStr : shortstring;

         function ComputeEigenVectors: boolean;
         var
            i,NRot : integer;
            Dip,Strike,DipDirect : float32;
         begin
            {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('ComputeEigenVectors in'); {$EndIf}
            Result := true;
            Jacobi(M^,3,SSOVars.V,S^,NRot);
            Eigsrt(SSOVars.V,S^,3);
            if abs(SSOVars.v[2]) < 0.00001 then SSOVars.v[2] := 0.00001;
            if abs(SSOVars.v[3]) < 0.000012 then SSOVars.v[3] := 0.000012;

            {Shape/strength statistics after Fisher, Lewis, and Embleton, p.159}
            if abs(SSOvars.v[3]) < 0.00001 then begin
               SSOvars.s2s3 := 0;
               SSOvars.Shape := 0;
               SSOvars.s1s2 := MaxSmallInt;
               SSOvars.Strength := MaxSmallInt;
            end
            else begin
               SSOvars.s1s2 := ln(SSOvars.v[1]/ SSOvars.v[2]);
               SSOvars.s2s3 := ln(SSOvars.v[2]/ SSOvars.v[3]);
               SSOvars.Shape := ln(SSOvars.v[1]/ SSOvars.v[2])/ln( SSOvars.v[2]/ SSOvars.v[3]);
               SSOvars.Strength := ln(SSOvars.v[1]/ SSOvars.v[3]);
            end;

            for i := 1 to 3 do begin //compute eigenvectors
               S2[1] := s[1,i];
               S2[2] := s[2,i];
               S2[3] := s[3,i];
               CartesianToDipStrike(s2,Dip,Strike);
               if (Dip < 0) then begin
                 Dip := abs(Dip);
                 DipDirect := Strike - 90;
               end
               else DipDirect := Strike + 90;
               DipDirect := CompassAngleInRangeFloat32(DipDirect);
               if DipDirect > 180 then DipDirect := DipDirect - 180;
               SSOVars.TheDips[i] := Dip;
               SSOVars.TheDipDirs[i] := DipDirect;
            end {for i};
            {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('ComputeEigenVectors out'); {$EndIf}
         end;


         procedure NormalsInBox(GridLimits: tGridLimits);
         var
            Col,Row,j,k : integer;
            zf : float32;
            z : array[1..3] of float32;
         begin
            {$If Defined(RecordSSOCalc)} WriteLineToDebugFile('tSSOVarAnalysis.NormalsInBox in'); {$EndIf}
            SSOVars.x1sq := 0;
            SSOVars.y1sq := 0;
            SSOVars.z1sq := 0;
            SSOVars.NumPts := 0;
            SSOVars.NumMissing := 0;
            for j := 1 to 3 do
               for k := 1 to 3 do
                  M^[j,k] := 0.0;

            for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
               for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
                  if (FilterGrid = 0) or (GetElevMetersFromSecondDEM(false,FilterGrid,Col,Row,zf) and (round(zf) = FilterValue)) then begin
                     if DEMGlb[Normals[1]].GetElevMetersOnGrid(Col,Row,z[1]) and
                        DEMGlb[Normals[2]].GetElevMetersOnGrid(Col,Row,z[2]) and
                        DEMGlb[Normals[3]].GetElevMetersOnGrid(Col,Row,z[3]) then begin
                        inc(SSOVars.NumPts);
                        SSOVars.x1sq := SSOVars.x1sq + sqr(z[1]);
                        SSOVars.y1sq := SSOVars.y1sq + sqr(z[2]);
                        SSOVars.z1sq := SSOVars.z1sq + sqr(z[3]);
                        for j := 1 to 3 do
                           for k := 1 to 3 do
                              M^[j,k] := M^[j,k] + z[j] * z[k];
                     end
                     else inc(SSOVars.NumMissing);
                  end;
               end {for Row};
            end {for Col};
            {$If Defined(RecordSSOCalc)} WriteLineToDebugFile('tSSOVarAnalysis.NormalsInBox out'); {$EndIf}
         end;



begin {tDEMDataSet.SSOComputations}
   {$IfDef ShowDEMSSOCalc}
      WriteLineToDebugFile('tDEMDataSet.SSOComputations DEM=' + IntToStr(ThisDEM) + ' ' + GridLimitsToString(GridLimits) + ' plot=' + IntToStr(Ord(MDDef.SSObyPole)));
      if (FilterGrid <> 0) then WriteLineToDebugFile('Filter grid=' + IntToStr(FilterGrid) + ' value=' + IntToStr(FilterValue));
   {$EndIf}
   Result := false;
   try
      ClipDEMGridInteger(GridLimits.XGridLow,GridLimits.YGridLow);
      ClipDEMGridInteger(GridLimits.XGridHigh,GridLimits.YGridHigh);
      {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.SSOComputations back clipped limits'); {$EndIf}

      New(M);
      New(s);
      InitializeNormals;
      {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.SSOComputations back from InitNormals'); {$EndIf}
      NormalsInBox(GridLimits);
      ComputeEigenVectors;
      Dispose(M);
      Dispose(s);

      {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('SSO setup done'); {$EndIf}

      AspectStats.Create(ThisDEM);

      if Plot and MDDef.GemorphSSOPoles then begin
         MDDef.NetDef.NetScreenMult := 2;
         NetForm := TNetForm.Create(Application);
         NetForm.Caption := 'SSO Diagram, ' + AreaName;
         NetForm.nd.ReallyPlot := false;
      end;

      {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('SSOVarAnalysis done'); {$EndIf}

      SumSlope := 0;
      SumElev := 0;
      SumSqSlope := 0;
      SumSqElev := 0;
      MinZ := MaxSmallInt;
      MaxZ := -MaxSmallInt;

      Symbol.DrawingSymbol := Dot;
      Symbol.Size := 3;
      Symbol.Color := claRed;
      Col := GridLimits.XGridLow;
      while Col <= GridLimits.XGridHigh do begin
         Row := GridLimits.YGridLow;
         while Row <= GridLimits.YGridHigh do begin
            if (FilterGrid = 0) or (GetElevMetersFromSecondDEM(false,FilterGrid,Col,Row,zf) and (round(zf) = FilterValue)) then begin
               if GetSlopeAndAspect(Col,Row,SlopeAspectRec) then begin
                  if (SlopeAspectRec.SlopeDegree> SSOvars.MaxSlope) then SSOvars.MaxSlope := SlopeAspectRec.SlopeDegree;
                  PetMath.CompareValueToExtremes(SlopeAspectRec.z,MinZ,MaxZ);
                  SumSlope := SumSlope + SlopeAspectRec.Slope;
                  SumSqSlope := SumSqSlope + Sqr(SlopeAspectRec.Slope);
                  SumElev := SumElev + SlopeAspectRec.z;
                  SumSqElev := SumSqElev + Sqr(SlopeAspectRec.z);
                  AspectStats.AddPoint(SlopeAspectRec);
                  inc(SSOVars.AspPts);
                  inc(SSOvars.NumPts);
                  if Plot and MDDef.GemorphSSOPoles then begin
                     if MDDef.SSObyPole then NetForm.nd.PlotPointOnNet(PolePlot,SlopeAspectRec.SlopeDegree,SlopeAspectRec.AspectDirTrue,Symbol,xd,yd)
                     else NetForm.nd.PlotPointOnNet(LinePlot,SlopeAspectRec.SlopeDegree,SlopeAspectRec.AspectDirTrue,Symbol,xd,yd);
                  end;
               end;
            end;
            inc(Row,MDDef.SSOSampleIncr);
         end {while Row};
         inc(Col,MDDef.SSOSampleIncr);
         if WantOut then break;
      end {while Col};
      SSOvars.ElevRange := MaxZ - MinZ;
      {$IfDef ShowDEMSSOCalc} WriteLineToDebugFile('Vectors done, npts=' + IntToStr(SSOvars.NumPts)); {$EndIf}

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

         if Plot then begin
            if MDDef.GemorphSSOPoles then begin
               //NetForm.nd.LLcornerText := LLtext;
               NetForm.nd.ReallyPlot := true;
               NetForm.nd.ContourPoles;
               {$IfDef SaveNetDrawingSteps} fName := NextFileNumber(MDtempDir,'back_home_','.bmp'); Netform.nd.WorkingBitmap.SaveToFile(fName); {$EndIf}
               Netform.nd.WorkingBitmap.Canvas.Font.Size := 15;
               Netform.nd.WorkingBitmap.Canvas.Font.Size := 12;
               Netform.nd.WorkingBitmap.Canvas.TextOut(1,1,'ln(s1/s2)=' + RealToString(SSOVars.s1s2,-5,2));
               Netform.nd.WorkingBitmap.Canvas.TextOut(1,18,'ln(s2/23)=' + RealToString(SSOVars.s2s3,-5,2) );
               Netform.nd.WorkingBitmap.Canvas.TextOut(1,35,'Trend:' + RealToString(SSOVars.TheDipDirs[3],6,1) + '°' );
               Netform.nd.WorkingBitmap.Canvas.Font.Size := 10;
               Netform.nd.WorkingBitmap.Canvas.TextOut(1,53,'n=' + IntToStr(Netform.nd.NumPlottedPoints));
               Netform.nd.WorkingBitmap.Canvas.TextOut(1,Netform.nd.MainDiagramSize - Netform.nd.WorkingBitmap.Canvas.TextHeight(LLtext)-5,LLText);

               TStr := HemisphereName[MDDef.NetDef.HemisphereUsed] + ' Hemisphere';
               Netform.nd.WorkingBitmap.Canvas.TextOut(Netform.nd.MainDiagramSize - Netform.nd.WorkingBitmap.Canvas.TextWidth(Tstr)-5,1,TStr);

               TStr := 'Equal ' + NetEquality[MDDef.NetDef.NetUsed] + ' net';
               Netform.nd.WorkingBitmap.Canvas.TextOut(Netform.nd.MainDiagramSize - Netform.nd.WorkingBitmap.Canvas.TextWidth(Tstr)-5,18,TStr);

               if MDDef.SSObyPole then TStr := 'Poles' else TStr := 'Dip directions';
               Netform.nd.WorkingBitmap.Canvas.TextOut(Netform.nd.MainDiagramSize - Netform.nd.WorkingBitmap.Canvas.TextWidth(Tstr)-5,35,TStr);

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

            if (AspName <> '') then begin
               RoseGraph := AspectStats.CreateRose;
               PetImage.SaveImageAsBMP(RoseGraph.Image1,AspName);
               RoseGraph.Close;
               RoseGraph := Nil;
            end {if};
         end;
      end
      else if (not Plot) then begin
         NetForm.Close;
         NetForm := Nil;
      end;
   finally
     AspectStats.Destroy;
   end;
   {$IfDef ShowFullDEMSSOCalc} WriteLineToDebugFile('tDEMDataSet.SSOComputations out'); {$EndIf}
end {tDEMDataSet.SSOComputations};



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
                        S2[1] := sinDeg(SlopAsp.SlopeDegree) * sinDeg(SlopAsp.AspectDirTrue);
                        S2[2] := sinDeg(SlopAsp.SlopeDegree) * cosDeg(SlopAsp.AspectDirTrue);
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
                        DipDirect := CompassAngleInRangeFloat32(DipDirect);
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



