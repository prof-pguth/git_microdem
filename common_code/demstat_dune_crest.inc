{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 4/20/2020       }
{                                 }
{ include file for demstat        }
{_________________________________}


procedure ThreadCrest1Click(MapForm : tMapForm; StartLat,StartLong : float64; var Results : tStringList; CrestNum : integer);
var
   i : integer;
   NewResults : tStringList;

         procedure DoCrest(TrendMult : integer);
         var
            LOSF : TDEMLOSF;
            Lat1,Long1,Lat2,Long2,
            Lat,Long,DuneHeight, s1s2,s2s3,Trend,LastTrend,FirstTrend : float64;
            Col,Row,x,y,Gap,xinc,yinc : integer;
            LeftHt,LeftLength,RightHt,RightLength,CrestHeight,rf : float64;
            Point : ShortString;
            xg,yg : integer;
         begin
            {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('DoCrest in    TrendMult=' + IntToStr(TrendMult)); {$EndIf}
             Lat := StartLat;
             Long := StartLong;
             LastTrend := -999;
             Gap := -1;
             with MapForm do repeat
                DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(Lat,Long,Col,Row);

                if DEMGlb[MapDraw.DEMonMap].SimplePointSSOComputations(false,Col,Row,MDdef.WavelengthCompDist,s1s2,s2s3,Trend,rf) then begin
                   if (TrendMult = 1) then begin
                      if (LastTrend > -990) then begin
                         if (abs(LastTrend - Trend) > 90) and (abs(LastTrend - Trend) < 270) then Trend := Trend - 180;
                      end
                      else FirstTrend := Trend;
                   end
                   else begin
                      if (FirstTrend > -990) then begin
                         Trend := Trend + 180;
                         FirstTrend := -999;
                      end
                      else begin
                         if (LastTrend > -990) then begin
                            if (abs(LastTrend - Trend) > 90) and (abs(LastTrend - Trend) < 270) then Trend := Trend - 180;
                         end
                      end;
                   end;
                   Trend := PetMath.FindCompassAngleInRange(Trend);

                   {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile(LatLongDegreeToString(Lat,Long) + ' Trend=' + RealToString(Trend,-6,1)); {$EndIf}

                   if DEMGlb[MapDraw.DEMonMap].GetPerpendicularLineEnd(Lat,Long,MDdef.WavelengthCompDist,Trend,Lat1,Long1,Lat2,Long2,true) then begin
                       {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('GotPerp'); {$EndIf}
                       MDDef.ForceCrestComputations := true;
                       LOSF := StartLOS(false,JustWandering,MapDraw.DEMonMap,Lat1,Long1,Lat2,Long2,DEMGlb[MapDraw.DEMonMap].SelectionMap);
                       MDDef.ForceCrestComputations := true;

                       {$IfDef RecordDetailedTraceCrests}   WriteLineToDebugFile('LOSF'); {$EndIf}

                       if LOSF.FindNearestCrest(MDdef.WavelengthCompDist * 0.001,Lat,Long,DuneHeight,LeftHt,LeftLength,RightHt,RightLength) then begin

                          {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('Found NC'); {$EndIf}

                          if (ScratchDEM <> 0) and (DEMGlb[ScratchDEM] <> Nil) then begin
                             {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('Scratching'); {$EndIf}
                             DEMGlb[ScratchDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                             if (not DEMGlb[ScratchDEM].MissingDataInGrid(xg,yg)) then begin
                                LOSF.Close;
                                {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('Break out');  {$EndIf}
                                exit;
                             end;
                          end;

                          {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('Next'); {$EndIf}

                          CrestHeight := (LeftHt + RightHt) * 0.5;
                          if (CrestHeight > MDDef.MinWaveHeight) then begin
                             {$IfDef RecordDetailedTraceCrests}
                                WriteLineToDebugFile('Assemble');
                                WriteLineToDebugFile('right len=' + RealToString((RightLength),-18,-6));
                                WriteLineToDebugFile('trend=' + RealToString(Trend,-8,-1));
                                WriteLineToDebugFile('left len=' + RealToString((LeftLength),-18,-6));
                             {$EndIf}
                             Point := IntToStr(CrestNum) + ',' + RealToString(Lat,-18,-8) + ',' + RealToString(Long,-18,-8) + ',' + RealToString(DuneHeight,-18,-2) + ','+
                                 RealToString(CrestHeight,-18,-2) + ',' + RealToString((LeftLength + RightLength) * 0.5,-18,-6) + ',' + RealToString(Trend,-8,-1);
                             {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile(Point); {$EndIf}
                             if (TrendMult = 1) or (NewResults.Count = 0) then NewResults.Add(Point)
                             else NewResults.Insert(1,Point);

                             if (ScratchDEM <> 0) and (DEMGlb[ScratchDEM] <> Nil) then begin
                                {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('Scratch DEM'); {$EndIf}
                                xinc := round(0.5 * MDDef.DuneSamplingInterval / DEMGlb[ScratchDEM].AverageXSpace);
                                yinc := round(0.5 * MDDef.DuneSamplingInterval / DEMGlb[ScratchDEM].AverageYSpace);

                                for x := xg - xinc to xg + xinc do begin
                                   for y := yg - yinc to yg + yinc do begin
                                      DEMGlb[ScratchDEM].SetGridElevation(xg,yg,1);
                                   end;
                                end;
                             end;
                             LastTrend := trend;
                             Gap := 0;
                             {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('End SetGrid'); {$EndIf}
                          end
                          else begin
                             {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('Gap'); {$EndIf}
                             if (Gap < 0) then Gap := 255
                             else inc(Gap);
                          end;
                       end
                       else begin
                          LOSF.Close;
                          {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('No crest'); {$EndIf}
                          exit;
                       end;
                       LOSF.Close;
                    end
                    else begin
                        {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('X no Perp'); {$EndIf}
                        exit;
                    end;
               end
               else begin
                  {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('X no SSO'); {$EndIf}
                  exit;
               end;

               {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('Next start'); {$EndIf}
               VincentyPointAtDistanceBearing(Lat,Long,MDDef.CrestThreadInterval,Trend,Lat,Long);
               {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('Found start'); {$EndIf}
             until (not MapDraw.LatLongOnScreen(Lat,Long)) or (Gap > MDDef.CrestMaxGap);
            {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('DoCrest out'); {$EndIf}
         end;

begin
   {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('ThreadCrest in'); {$EndIf}
   ShowHourglassCursor;
   NewResults := tStringList.Create;
   DoCrest(1);
   DoCrest(-1);
   {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('Pts found=' + IntToStr(NewResults.Count)); {$EndIf}

   for i := 0 to pred(NewResults.Count) do Results.Add(NewResults.Strings[i]);
   NewResults.Free;
   ShowDefaultCursor;

   {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('ThreadCrest out'); {$EndIf}
end;


procedure CrestsAlongProfile(theLOSView : TDEMLOSF; var Results : tStringList; Memo1 : tMemo = Nil);
var
   Lat,Long : float64;
   CrestNum : integer;
begin
   {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('CrestsAlongProfile in'); {$EndIf}
   if (theLOSView <> Nil) then begin
       theLOSView.BaseMap.MapRedrawsAllowed := false;
       MDDef.ForceCrestComputations := true;
       CloseAndNilNumberedDB(theLOSView.LOSdraw.FresnelDB);
       theLOSView.FormResize(Nil);
       if (Results = Nil) then begin
          Results := tStringList.Create;
          Results.Add('CREST_ID,LAT,LONG,Z,HEIGHT,LENGTH,TREND');
       end;
       CrestNum := 0;

       GISdb[theLOSView.LOSdraw.FresnelDB].EmpSource.Enabled := false;
       GISdb[theLOSView.LOSdraw.FresnelDB].MyData.ApplyFilter('PEAK = ' + QuotedStr('Y'));
       {$IfDef RecordDetailedTraceCrests}  WriteLineToDebugFile('Peaks = ' + IntToStr(GISDataBase[theLOSView.FresnelDB].MyData.RecordCount)); {$EndIf}
       while not GISdb[theLOSView.LOSdraw.FresnelDB].MyData.eof do begin
          inc(CrestNum);
          if (Memo1 <> Nil) then Memo1.Lines.Add('Crest ' + IntToStr(CrestNum)  + '/' + IntToStr(GISdb[theLOSView.LOSdraw.FresnelDB].MyData.RecordCount));
          if GISdb[theLOSView.LOSdraw.FresnelDB].ValidLatLongFromTable(Lat,Long) then Threadcrest1Click(theLOSView.BaseMap,Lat,Long,Results,CrestNum);
          if (Memo1 <> Nil) and (Results.Count > 0) then Memo1.Lines.Add('   Crest pts=' + IntToStr(pred(Results.Count)));
          GISdb[theLOSView.LOSdraw.FresnelDB].MyData.Next;
       end;

       if (Memo1 <> Nil) then Memo1.Lines.Add('Done');
       ApplicationProcessMessages;
       theLOSView.BaseMap.MapRedrawsAllowed := true;
   end;
end;


procedure ComputeDunecrestspacingheight(MapForm : tMapForm; GridLimits : tGridLimits; Memo1 : tMemo);
var
   Results : tStringList;
   s1s2,s2s3,Trend,rf,   Lat,Long,Lat1,Long1,Lat2,Long2 : float64;
   x,y,xinc,yinc,CrestNum : integer;
   TStr : shortString;
   NewHeadRecs : tDEMheader;
   LOSF : TDEMLOSF;
begin
    with MapForm do begin
       {$IfDef RecordTraceCrests} WriteLineToDebugFile('ComputeDunecrestspacingheight in open databases=' + IntToStr(TotalNumOpenDatabase));   {$EndIf}
       NewHeadRecs := DEMGlb[MapDraw.DEMonMap].DEMheader;
       NewHeadRecs.DEMPrecision := ByteDEM;
       if not OpenAndZeroNewDEM(true,NewHeadRecs,ScratchDEM,'',false,0) then exit;

       Results := Nil;
       CrestNum := 0;

       with DEMGlb[MapDraw.DEMonMap] do begin
          xinc := round(MDDef.DuneSamplingInterval / AverageXSpace);
          yinc := round(MDDef.DuneSamplingInterval / AverageYSpace);
          Results := tStringList.Create;
          Results.Add('CREST_ID,LAT,LONG,Z,HEIGHT,LENGTH,TREND');
          {$IfDef NoFanThreads}
          {$Else}
             StartThreadTimers('Thread Crests');
          {$EndIf}
          x := GridLimits.XGridLow + xinc;
          while (x < GridLimits.XGridHigh - xinc) do  begin
             {$IfDef NoFanThreads}
             {$Else}
                ThreadTimers.OverallGauge9.Progress := round(100 * (x - GridLimits.XGridLow) / (GridLimits.XGridHigh - GridLimits.XGridLow));
             {$EndIf}
             y := GridLimits.YGridLow + yinc;
             while (y < GridLimits.YGridHigh  - yinc) do begin
                DEMGridToLatLongDegree(x,y,Lat,Long);
                MapDraw.MapSymbolAtLatLongDegree(Image1.Canvas,Lat,Long,FilledBox,2,claRed);
                inc(CrestNum);


                if DEMGlb[MapDraw.DEMonMap].SimplePointSSOComputations(false,x,y,MDdef.WavelengthCompDist,s1s2,s2s3,Trend,rf) then begin
                   if DEMGlb[MapDraw.DEMonMap].GetPerpendicularLineEnd(Lat,Long,MDdef.WavelengthCompDist,Trend,Lat1,Long1,Lat2,Long2,true) then begin
                       MDDef.ForceCrestComputations := true;
                       LOSF := StartLOS(false,JustWandering,MapDraw.DEMonMap,Lat1,Long1,Lat2,Long2,DEMGlb[MapDraw.DEMonMap].SelectionMap);
                       MDDef.ForceCrestComputations := true;
                       //LOSF.LOSdraw.EraseFresnelDB := true;
                       {$IfDef RecordDetailedTraceCrests} WriteLineToDebugFile('LOSF'); {$EndIf}
                       CrestsAlongProfile(LOSF, Results,Nil);   //Memo1);
                       LOSF.Close;
                   end;
                end;
                inc(Y,yinc);
                ///if CeaseThreads then break;
             end;
             TStr := IntToStr(x) + '/' + IntToStr(round(GridLimits.XGridHigh)) + ' ' + TimeToStr(Now) + '   Pts=' + IntToStr(pred(Results.Count));;
             if (Memo1 <> Nil) then Memo1.Lines.Add(TStr);
             {$IfDef RecordTraceCrests} WriteLineToDebugFile(TStr); {$EndIf}
             inc(x,xinc);
             Results.SaveToFile(ExtractFilePath(DEMFileName) + AreaName + '_crests_working.csv');
             //if CeaseThreads then break;
          end;
          {$IfDef NoFanThreads}
          {$Else}
             EndThreadTimers;
          {$EndIf}
          if (Results.Count > 1) then begin
             StringListToLoadedDatabase(Results,NextFileNumber(ExtractFilePath(DEMFileName), AreaName + '_crests-','.csv'));
          end
          else begin
             Results.Free;
             MessageToContinue('Nothing found');
          end;
       end;

       CloseSingleDEM(ScratchDEM);
       {$IfDef RecordTraceCrests} WriteLineToDebugFile('ComputeDunecrestspacingheight out'); {$EndIf}
    end;
end;



