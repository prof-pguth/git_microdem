


procedure TDEMLOSF.FindWaveLengthHeight(DisplayIt : boolean; var WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd : float64; Memo1 : tMemo = nil);
const
   FlatnessCutOff = 0.05;
var
   Findings : tStringList;
   NPts,Missing : int64;
   zs : ^bfarray32;
   MomentVar : tMomentVar;
   Min,Max,Median : float64;
begin
   {$IfDef ExFresnel}
      WavelengthMean := 0;
      WavelengthMedian := 0;
      WavelengthStdDev := 0;
      HeightMean := 0;
      HeightMedian := 0;
      HeightStd := 0;
   {$Else}
   if (LOSdraw.FresnelDB = 0) then exit;
   GISdb[LOSdraw.FresnelDB].EmpSource.Enabled := false;

   FindWavelengthStats(GISdb[LOSdraw.FresnelDB].MyData, WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd);

   if DisplayIt then begin
      Findings := tStringList.Create;
      Findings.Add(ptTrim(DEMGlb[LOSdraw.DEMonView].AreaName));
      Findings.Add(LatLongDegreeToString(LOSdraw.LatLeft,LOSdraw.LongLeft,MDDef.OutPutLatLongMethod));
      Findings.Add(LatLongDegreeToString(LOSdraw.LatRight,LOSdraw.LongRight,MDDef.OutPutLatLongMethod));

      Findings.Add('Regions: ' + IntToStr(MDDef.PeakPitPostings) );  //+ '  (' + RealToString(MDDef.PeakPitPostings *
      Findings.Add('Min Wave Height (m): ' + RealToString(MDDef.MinWaveHeight,-18,2));
      Findings.Add('Computation disatnce (m): ' + IntToStr(MDdef.WavelengthCompDist));
      Findings.Add('Peaks needed: ' + IntToStr(MDDef.WaveHtValuesNeeded));
      Findings.Add(' ');

      AddToWaveSpacingHeightResults(Findings,WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd);
      Findings.Add(' ');

      GISdb[LOSdraw.FresnelDB].MyData.ApplyFilter('');
      GISdb[LOSdraw.FresnelDB].MyData.TotRecsInDB := GISdb[LOSdraw.FresnelDB].MyData.RecordCount;
      New(zs);

      GISdb[LOSdraw.FresnelDB].MyData.ApplyFilter('SLOPE_2D > ' + RealToString(FlatnessCutOff,-18,2));
      Findings.Add('Positive slopes:' + RealToString(100 * GISdb[LOSdraw.FresnelDB].MyData.RecordCount / pred(GISdb[LOSdraw.FresnelDB].MyData.TotRecsInDB),-12,2) + '%');
      GetFieldValuesInArrayLinkPossible(GISdb[LOSdraw.FresnelDB].MyData,Nil,'','','SLOPE_2D',zs^,MomentVar.Npts,Missing,Min,Max);
      if (NPts > 1)  then begin
         moment(zs^,MomentVar,msAll);
         Findings.Add('   average slope:' + RealToString(MomentVar.Mean,8,2));
         Findings.Add('   median slope:' + RealToString(MomentVar.Median,8,2));
      end;

      GISdb[LOSdraw.FresnelDB].MyData.ApplyFilter('SLOPE_2D < -' + RealToString(FlatnessCutOff,-18,2));
      Findings.Add('Negative slopes:' + RealToString(100 * GISdb[LOSdraw.FresnelDB].MyData.RecordCount / pred(GISdb[LOSdraw.FresnelDB].MyData.TotRecsInDB),-12,2) + '%');
      GetFieldValuesInArrayLinkPossible(GISdb[LOSdraw.FresnelDB].MyData,Nil,'','','SLOPE_2D',zs^,MomentVar.Npts,Missing,Min,Max);
      if (NPts > 1)  then begin
         moment(zs^,MomentVar,msAll);
         Findings.Add('   average slope:' + RealToString(MomentVar.Mean,8,2));
         Findings.Add('   median slope:' + RealToString(MomentVar.Median,8,2));
      end;

      Dispose(zs);

      GISdb[LOSdraw.FresnelDB].MyData.ApplyFilter('SLOPE_2D > -' + RealToString(FlatnessCutOff,-18,2) + ' AND SLOPE_2D < ' + RealToString(FlatnessCutOff,-18,2));
      Findings.Add('Flat regions:' + RealToString(100 * GISdb[LOSdraw.FresnelDB].MyData.RecordCount / pred(GISdb[LOSdraw.FresnelDB].MyData.TotRecsInDB),-12,2) + '%');

      if (Memo1 = Nil) then Petmar.DisplayAndPurgeStringList(Findings,DEMGlb[LOSdraw.DEMonView].AreaName + ' Spacing and height')
      else Memo1.Lines := Findings;

      GISdb[LOSdraw.FresnelDB].EmpSource.Enabled := true;
      GISdb[LOSdraw.FresnelDB].MyData.ApplyFilter('');
   end;
   {$EndIf}
end;


function TDEMLOSF.FindNearestCrest(Distance : float64; var Lat,Long,Height,LeftHt,LeftLength,RightHt,RightLength : float64) : boolean;
var
   d,dd,LastDD,MinD,z : float64;
   Rec,WhereCrest,i : integer;
begin
   {$IfDef RecordNearestCrest} WriteLineToDebugFile('Enter TDEMLOSF.FindNearestCrest for distance=' + RealToString(Distance,-12,4),true); {$EndIf}

   Result := false;
   if (LOSdraw.FresnelDB = 0) then exit;
   GISdb[LOSdraw.FresnelDB].EmpSource.Enabled := false;
   GISdb[LOSdraw.FresnelDB].MyData.ApplyFilter('PEAK = ' + QuotedStr('Y'));

   LastDD := 99999;
   Rec := 0;
   while not GISdb[LOSdraw.FresnelDB].MyData.eof do begin
      d := GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
      dd := abs(d  - Distance);
      if (dd < LastDD) then begin
         Lat := GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('LAT');
         Long := GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('LONG');
         Height := GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
         MinD := d;
         LastDD := dd;
         WhereCrest := Rec;
      end;
      GISdb[LOSdraw.FresnelDB].MyData.Next;
      inc(Rec);
   end;

   {$IfDef RecordNearestCrest} WriteLineToDebugFile('Crest at dist=' + RealToString(MinD,-12,4) + '  with z=' + RealToString(Height,-12,-2));{$EndIf}

   LeftHt := 0;
   RightHt := 0;

   GISdb[LOSdraw.FresnelDB].MyData.ApplyFilter('');
   LastDD := 99999;
   Rec := 0;
   while not GISdb[LOSdraw.FresnelDB].MyData.eof do begin
      d := GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM');
      dd := abs(d  - Distance);
      if (dd < LastDD) then begin
         LastDD := dd;
         WhereCrest := Rec;
      end;
      GISdb[LOSdraw.FresnelDB].MyData.Next;
      inc(Rec);
   end;

   {$IfDef RecordNearestCrest} WriteLineToDebugFile('Right side');  {$EndIf}
   GISdb[LOSdraw.FresnelDB].MyData.First;
   for i := 0 to succ(WhereCrest) do GISdb[LOSdraw.FresnelDB].MyData.Next;
   repeat
      z := GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
      if (Height - z > RightHt) then RightHt := height - z;
      {$IfDef RecordNearestCrest}
      WriteLineToDebugFile('Dist=' + RealToString(GISDataBase[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM'),-12,4) + '   z=' + RealToString(z,-8,2) + '   RightHt=' + RealToString(RightHt,-8,2));
      {$EndIf}
      GISdb[LOSdraw.FresnelDB].MyData.Next;
   until (GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsString('PEAK') = 'Y') or (GISdb[LOSdraw.FresnelDB].MyData.eof);

   RightLength := abs(GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM') - MinD);

   {$IfDef RecordNearestCrest}  WriteLineToDebugFile('Left side'); {$EndIf}
   GISdb[LOSdraw.FresnelDB].MyData.First;
   for i := 0 to pred(WhereCrest) do GISdb[LOSdraw.FresnelDB].MyData.Next;
   repeat
      z := GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('ELEV_M');
      if (Height - z > LeftHt) then LeftHt := height - z;
      {$IfDef RecordNearestCrest}
      WriteLineToDebugFile('Dist=' + RealToString(GISDataBase[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM'),-12,4) + '   z=' + RealToString(z,-8,2) + '   RightHt=' + RealToString(LeftHt,-8,2));
      {$EndIf}
      GISdb[LOSdraw.FresnelDB].MyData.Prior;
   until (GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsString('PEAK') = 'Y') or (GISdb[LOSdraw.FresnelDB].MyData.bof);
   LeftLength := abs(MinD - GISdb[LOSdraw.FresnelDB].MyData.GetFieldByNameAsFloat('RANGE_KM'));
   GISdb[LOSdraw.FresnelDB].MyData.ApplyFilter('');
   {$IfDef RecordNearestCrest} WriteLineToDebugFile('Lengths, left=' + RealToString(LeftLength,-12,3) +  '   and right=' + RealToString(RightLength,-12,3) + ' MinD=' + RealToString(MinD,-12,-4)); {$EndIf}
   Result := true;
   if isNaN(RightLength) or isNaN(LeftLength) or (abs(RightLength) < 0.01) or  (abs(RightLength) > 100) or
     ((abs(LeftLength) < 0.01) or (abs(LeftLength) > 100)) then Result := false;
end;


