{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}

//include file for demdatbase


const
   MaxRefCat = 7;
   NumDEMIXDEM = 6;
   RefType : array[1..2] of shortstring = ('DTM','DSM');
   SlopeType : array[1..MaxRefCat] of shortstring = ('ALL','CLIFF','STEEP','GENTLE','FLAT','URBAN','FOREST');
   DEMType : array[1..NumDEMIXDEM] of shortstring = ('FABDEM','COP','ALOS','NASA','SRTM','ASTER');
const
   SignedParams : array[1..6] of shortstring = ('ELD_MEAN','ELD_MEDIAN', 'SMD_MEAN','SMD_MEDIAN', 'RUFD_MEAN','RUFD_MED');


procedure FilterOutSignedCriteria(DBonTable : integer);
var
   aFilter : ANSIstring;
   i : integer;
begin
    aFilter := '';
    for I := 1 to 6 do begin
       aFilter := AddAndIfNeeded(aFilter) + 'CRITERION<>' + QuotedStr(SignedParams[i]);
    end;
    GISdb[DBOntable].ApplyGISFilter(aFilter);
end;



function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
var
   DEM : integer;
begin
   Result.Size := 4;
   for DEM := 1 to NumDEMIXDEM do begin
      if DEMName = DEMType[DEM] then Result.Color := ConvertTColorToPlatformColor(WinGraphColors[DEM]);
   end;
   if DEMName = 'FABDEM' then Result.DrawingSymbol := FilledBox
   else if DEMName = 'COP' then Result.DrawingSymbol := FilledCircle
   else if DEMName = 'ALOS' then Result.DrawingSymbol := FilledDiamond
   else if DEMName = 'NASA' then Result.DrawingSymbol := FilledDownTri
   else if DEMName = 'SRTM' then Result.DrawingSymbol := FilledUpTri
   else if DEMName = 'ASTER' then Result.DrawingSymbol := Box
   else Result.DrawingSymbol := FilledBox;
end;


function TransposeDEMIXcriteria(DBonTable : integer; CriteriaFile : PathStr = '') : PathStr;
const
   MaxDEMs = 10;
   MaxCriteria = 50;
var
   Headers,DEMs,Criteria,Output,Tiles : tStringList;
   Line : ANSIString;
   fName : PathStr;
   value,ThisDEM,Missing : shortstring;
   Cycles,Done,
   i,j,  Tile, Ref, Slope,DEM,Criterion : Integer;
   Values : array[0..MaxDEMs,0..MaxCriteria] of shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile(''); WriteLineToDebugFile('TransposeDEMIXcriteria in, ' + GISdb[DBonTable].dbName); {$EndIf}
   StopSplashing;
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DEM_LOW_SC',48);

   Headers := tStringList.Create;
   Headers.LoadFromFile(ProgramRootDir + 'demix_headers.txt');
   DEMs := tStringList.Create;
   DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');

   if not FileExists(CriteriaFile) then CriteriaFile := ProgramRootDir + 'demix_criteria.txt';
   Criteria := tStringList.Create;
   Criteria.LoadFromFile(CriteriaFile);

   for Criterion := pred(Criteria.Count) downto 0 do begin
      if not GISdb[DBonTable].MyData.FieldExists(Criteria.Strings[Criterion]) then begin
         Criteria.Delete(Criterion);
      end;
   end;

   Output := tStringList.Create;
   Line := '';
   for i := 0 to pred(Headers.Count) do Line := Line + Headers.Strings[i] + ',';
   Line := Line + 'CRITERION,';
   for i := 0 to pred(DEMs.Count) do begin
      Line := Line + DEMs.Strings[i];
      if i < pred(DEMs.Count) then Line := Line + ',';
   end;
   Output.Add(Line);

   GISdb[DBonTable].ClearGISFilter;
   Tiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria tiles=' + IntToStr(Tiles.Count)); {$EndIf}

   Cycles := Tiles.Count * 2 * 7;
   Done := 0;
   StartProgress('Transposing ' + GISdb[DBontable].DBName);
   for Tile := 0 to Pred(Tiles.Count) do begin
      UpdateProgressBar(Done/Cycles);
      for Ref := 1 to 2 do begin
         Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefType[Ref]);
         GISdb[DBonTable].ApplyGISFilter(Line);
         GISdb[DBonTable].EmpSource.Enabled := false;
         if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin  //there are DSMs or DTMs for this tile
            for Slope := 1 to 7 do begin
               Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefType[Ref])  + ' AND LAND_TYPE=' + QuotedStr(SlopeType[Slope]);
               GISdb[DBonTable].ApplyGISFilter(Line);
               GISdb[DBonTable].EmpSource.Enabled := false;
               if (GISdb[DBonTable].MyData.FiltRecsInDB = DEMs.Count) then begin
                  for i := 0 to MaxDems do
                     for j := 0 to MaxCriteria do
                         Values[i,j] := '-9999';

                  while not GISdb[DBonTable].MyData.eof do begin
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     ThisDEM := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));
                     DEM := DEMs.IndexOf(ThisDEM);
                     for Criterion := 0 to pred(Criteria.Count) do begin
                        value := GISdb[DBonTable].MyData.GetFieldByNameAsString(Criteria[Criterion]);
                        if (value <> '') then Values[DEM,Criterion] := value;
                     end;
                     GISdb[DBonTable].MyData.Next;
                  end;

                  for Criterion := 0 to pred(Criteria.Count) do begin
                     Line := '';
                     for i := 0 to pred(Headers.Count) do Line := Line + GISdb[DBonTable].MyData.GetFieldByNameAsString(Headers[i]) + ',';
                     Line := Line + Criteria[Criterion] + ',';

                     for DEM := 0 to pred(DEMs.Count) do Line := Line + Values[DEM,Criterion] + ',';
                     Delete(Line,Length(Line),1);
                     Output.Add(Line);
                  end;
               end
               else begin
                  {$If Defined(RecordDEMIX)}  //this is to track down why some tiles do not have 6 records
                     if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin  //if it was 0, it was something like CLIFF in the flatlands
                        Missing := '  Present: ';
                        GISdb[DBonTable].MyData.First;
                        while not GISdb[DBonTable].MyData.eof do begin
                           Missing := Missing + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM') + ', ';
                           GISdb[DBonTable].MyData.Next;
                        end;
                        WriteLineToDebugFile('filter=' + GISdb[DBonTable].MyData.Filter + '   Matches=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + Missing);
                     end;
                  {$EndIf}
               end;
            end;
         end;
      end;
   end;
   EndProgress;
   GISdb[DBonTable].ClearGISFilter;
   Headers.Free;
   Criteria.Free;
   DEMs.Free;
   Result := ChangeFileExt(GISdb[DBonTable].dbFullName, '_transpose_' + ExtractFileNameNoExt(CriteriaFile) + '.dbf');
   DeleteFileIfExists(Result);
   Result := ChangeFileExt(Result,'.csv');
   OutPut.SaveToFile(Result);
   OutPut.Free;
   GISdb[DBonTable].ShowStatus;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria out, created ' + Result); {$EndIf}
end;



procedure RankDEMS(DBonTable : integer);
var
   DEMsRanked,DEMs,Scores : tStringList;
   DEM,Value,N,i,Score : integer;
   Sum,fValue,BestValue,TieTolerance : float32;
   fName : PathStr;
   BestValueString,aLine : shortstring;
   Values,Sorted : array[0..10] of float32;
   Results : tStringList;
   TieToleranceTable : tMyData;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS in, ' + GISdb[DBonTable].dbName); {$EndIf}
    DEMs := tStringList.Create;
    DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
    for DEM := 0 to pred(DEMs.Count) do begin
       GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftFloat,DEMs.Strings[DEM] + '_SCR',8,2);
    end;

    DEMsRanked := tStringList.Create;
    DEMsRanked.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
    i := 0;
    if GetFromListZeroBased('DEMs to use in ranking',i,DEMsRanked,true,true) then begin
       fName := ProgramRootDir + 'demix_criterion_tolerance.dbf';
       TieToleranceTable := tMyData.Create(fName);

       GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DEM_LOW_SC',48);
       if GISdb[DBonTable].MyData.GetFieldLength('DEM_LOW_SC') < 48 then GISdb[DBonTable].MyData.TrimField('DEM_LOW_SC',48);

       StartProgress('Rank DEMs');
       i := 0;
       GISdb[DBonTable].MyData.First;
       while not GISdb[DBonTable].MyData.eof do begin
          inc(i);
          UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
          GISdb[DBonTable].EmpSource.Enabled := false;

          for DEM := 0 to pred(DEMs.Count) do begin
             if (DEMsRanked.IndexOf(DEMs.Strings[DEM]) <> -1) then Values[DEM] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMs[DEM])
             else Values[DEM] := 9999;
             Sorted[DEM] := Values[DEM];
          end;

          HeapSort(DEMs.Count, Sorted);
          TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION')));
          TieTolerance := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');

          GISdb[DBonTable].MyData.Edit;
          for DEM := 0 to pred(DEMs.Count) do begin
             if (DEMsRanked.IndexOf(DEMs.Strings[DEM]) <> -1) then begin
                Sum := 0;
                N := 0;
                for value := pred(DEMs.Count) downto 0 do begin
                   if (abs(Values[DEM]-Sorted[Value]) < TieTolerance) then begin
                      Sum := Sum + succ(Value);
                      inc(n);
                   end;
                end;
                Sum := Sum/N;
             end
             else Sum := DEMs.Count;
             GISdb[DBonTable].MyData.SetFieldByNameAsFloat(DEMs.Strings[DEM] + '_SCR',Sum);
          end;
          GISdb[DBonTable].MyData.Next;
       end;

       StartProgress('Find best DEMs');

       i := 0;
       GISdb[DBonTable].MyData.First;
       while not GISdb[DBonTable].MyData.eof do begin
          inc(i);
          UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
          GISdb[DBonTable].EmpSource.Enabled := false;

          TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION')));
          TieTolerance := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');

          BestValue := 99e38;
          for DEM := 0 to pred(DEMs.Count) do begin
             if (DEMsRanked.IndexOf(DEMs.Strings[DEM]) <> -1) then begin
                fValue := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMs[DEM]);
                if (fValue > 0) then begin
                   if (abs(fValue - BestValue) < TieTolerance) then begin
                      BestValueString := BestValueString + '-' + DEMs[DEM];
                   end;
                   if (fValue < BestValue) then begin
                      BestValue := fValue;
                      BestValueString := DEMs[DEM];
                   end;
                end;
             end;
          end;
          GISdb[DBonTable].MyData.Edit;
          GISdb[DBonTable].MyData.SetFieldByNameAsString('DEM_LOW_SC',BestValueString);  //DEMs[BestDEM]);
          GISdb[DBonTable].MyData.Next;
       end;

       Results := tStringList.Create;
       Results.Add('DEM,SUM_SCORES');
       for DEM := 0 to pred(DEMs.Count) do begin
          Results.Add(DEMs.Strings[DEM] + ',' + RealToString(GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR'),-12,-2));
       end;

       fName := NextFileNumber(MDTempDir,'sum_scores_','.dbf');
       StringList2CSVtoDB(Results,fName);

       Scores := tStringList.Create;
       Scores.LoadFromFile(ProgramRootDir + 'demix_scores.txt');

       StartProgress('Histogram Rank DEMs');
       Results := tStringList.Create;
       aline := 'DEM';
       for Score := 0 to pred(Scores.Count) do aLine := aLine + ',' + Scores[Score];
       Results.Add(aLine);

       for DEM := 0 to pred(DEMs.Count) do begin
          UpdateProgressBar(DEM/DEMs.Count);
          aLine := DEMs.Strings[DEM];
          fValue := 1;
          for Score := 0 to pred(Scores.Count) do begin
             GISdb[DBonTable].ApplyGISFilter(DEMs.Strings[DEM] + '_SCR < ' + RealToString(fValue + 0.1,-6,-1) + ' AND ' + DEMs.Strings[DEM] + '_SCR > ' + RealToString(fValue - 0.1,-6,-1)  );
             aLine := aLine + ',' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB);
             fValue :=fValue + 0.5;
          end;
          Results.Add(aLine);
       end;

       fName := NextFileNumber(MDTempDir,'score_histograms_','.dbf');
       Results.SaveToFile(mdtempdir + 'test.csv');
       StringList2CSVtoDB(Results,fName);
       Scores.Destroy;
    end;
    GISdb[DBonTable].ClearGISFilter;
    DEMs.Destroy;
    DEMsRanked.Destroy;
    GISdb[DBonTable].ShowStatus;
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS out, ' + GISdb[DBonTable].dbName); {$EndIf}
end;


procedure SumsOfRankDEMS(DBonTable : integer);
var
   DEMs : tStringList;
   DEM  : integer;
   Findings : tStringList;
   fName : PathStr;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXScoreDEMs in, ' + GISdb[DBonTable].dbName); {$EndIf}
    StopSplashing;
    DEMs := tStringList.Create;
    DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
    Findings := tStringList.Create;
    Findings.Add('DEM,SCORE_SUM');
    for DEM := 0 to pred(DEMs.Count) do begin
       GISdb[DBonTable].EmpSource.Enabled := false;
       GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
       Findings.Add(DEMs.Strings[DEM] + ',' + RealToString(GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR'),12,-2));
    end;
    DEMs.Destroy;
    GISdb[DBonTable].ShowStatus;
    fName := NextFileNumber(MDTempDir,'scores_','.dbf');
    StringList2CSVtoDB(Findings,fName);
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXScoreDEMs out, ' + GISdb[DBonTable].dbName); {$EndIf}
end;



procedure BestDEMSbyCategory(DBonTable : integer);
var
   DEMs,Criteria,Besties,Findings,Findings2,AverageScore,FiltersTooFewOpinions : tStringList;
   i : integer;
   theSum,MaxSum : float64;
   theSums : array[0..10] of float64;
   fName : PathStr;
   aLine : shortstring;


   procedure DoOne(Header,theFilter : shortstring);
   var
      Total,DEM,Ties,ThisCat,Opinions : integer;
   begin
      GISdb[DBonTable].ApplyGISFilter(theFilter);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Opinions := GISdb[DBonTable].MyData.FiltRecsInDB;
      if Opinions >= 10 then begin
         Ties := Opinions;
         Total := Opinions;
         aline := Header + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr(DEMs.Strings[DEM]) );
            ThisCat := GISdb[DBonTable].MyData.FiltRecsInDB;
            aLine := aLine + ',' + IntToStr(ThisCat);
            Ties := Ties - ThisCat;
         end;
         Besties.Add(aLine + ',' + IntToStr(Ties) );

         GISdb[DBonTable].ApplyGISFilter(theFilter);
         MaxSum := 0;
         for DEM := 0 to pred(DEMs.Count) do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
            theSums[DEM] := GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
            if theSums[DEM] > maxSum then MaxSum := theSums[DEM];
            Findings.Add(Header + ',' + DEMs.Strings[DEM] + ',' + RealToString(theSums[DEM],12,-4));
         end;

         aline := Header + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            aLine := aLine + ',' + RealToString(theSums[DEM] / MaxSum,12,-4);
         end;
         Findings2.Add(aLine);

         aline := Header + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            aLine := aLine + ',' + RealToString(theSums[DEM] / Opinions,12,-4);
         end;
         AverageScore.Add(aLine);
      end
      else begin
         FiltersTooFewOpinions.Add(theFilter + ',' + IntToStr(Opinions));
      end;
   end;


var
   RefFilter : ShortString;
   j,ScoresDB : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory in, ' + GISdb[DBonTable].dbName); {$EndIf}
   Criteria := tStringList.Create;
   Criteria.LoadFromFile(ProgramRootDir + 'demix_criteria.txt');
   DEMs := tStringList.Create;
   DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');

   if not GISdb[DBonTable].MyData.FieldExists(DEMs.Strings[0] + '_SCR') then begin
      RankDEMS(DBonTable);
   end;

   FiltersTooFewOpinions := tStringList.Create;
   FiltersTooFewOpinions.Add('FILTER,OPINIONS');

   Besties := tStringList.Create;
   aLine := 'FILTER,OPINIONS';
   for i := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[i];
   Besties.Add(aLine + ',TIES');

   Findings2 := tStringList.Create;
   aLine := 'FILTER,OPINIONS';
   for i := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[i];
   Findings2.Add(aLine);

   AverageScore := tStringList.Create;
   aLine := 'FILTER,OPINIONS';
   for i := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[i];
   AverageScore.Add(aLine);

   Findings := tStringList.Create;
   Findings.Add('FILTER,DEM,SCORE_SUM');

   for i := 1 to 2 do begin
      RefFilter := ' AND REF_TYPE=' + QuotedStr(RefType[i]);
      for j := 1 to MaxRefCat do begin
         DoOne(RefType[i] + ' ' + SlopeType[j] + ' pixels','LAND_TYPE=' + QuotedStr(SlopeType[j]) + RefFilter );
      end;
      AverageScore.Add('SKIP');
      RefFilter := ' AND REF_TYPE=' + QuotedStr(RefType[i]) + ' AND LAND_TYPE=' + QuotedStr('ALL');

      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile avg slope > 18%','AVG_SLOPE > 18' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile avg slope < 18%','AVG_SLOPE < 18' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile relief < 25m','RELIEF < 25' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile relief > 500m','RELIEF > 500' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile avg rough > 10%','AVG_ROUGH > 10' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile avg rough < 5%','AVG_ROUGH < 5' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile urban > 25%','URBAN_PC > 25' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile forest > 50%','FOREST_PC > 50' + RefFilter );
      AverageScore.Add('SKIP');

      for j := 0 to pred(Criteria.Count) do begin
         DoOne(RefType[i] + ' ALL pixels and ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
      end;
      if i=1 then AverageScore.Add('SKIP');

   end;

   fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + 'criteria_best_count.dbf';
   StringList2CSVtoDB(Besties,fName);

   fName := NextFileNumber(MDTempDir,'multi_line_scores_','.dbf');
   StringList2CSVtoDB(Findings,fName);

   fName := NextFileNumber(MDTempDir,'single_line_scores_','.dbf');
   StringList2CSVtoDB(Findings2,fName);

   fName := NextFileNumber(MDTempDir,'filgers_too_few_opinions_','.dbf');
   StringList2CSVtoDB(FiltersTooFewOpinions,fName);

   fName := NextFileNumber(MDTempDir,'average_scores_','.dbf');
   ScoresDB := StringList2CSVtoDB(AverageScore,fName);
   DEMIXwineContestScoresGraph(ScoresDB);

   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
   GISdb[DBonTable].EmpSource.Enabled := true;

   DEMs.Destroy;
   Criteria.Destroy;

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory out, ' + GISdb[DBonTable].dbName); {$EndIf}
end;




procedure TransposeDEMIXwinecontestGraph(DBonTable : integer);
var
   BaseFilter,BaseFilterAnd : shortstring;

      procedure OneField;
      var
         Graph : tThisBaseGraph;
         Symbol : tFullSymbolDeclaration;
         DEM,OnTile : integer;
         rfile : array[1..10] of file;
         v : array[1..2] of float32;


         procedure DoRefType(aFilterElement : shortstring; Title : shortString);
         var
            DEM : integer;
         begin
            GISdb[DBonTable].MyData.First;
            GISdb[DBonTable].ApplyGISFilter(BaseFilterAnd + aFilterElement);
            if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
               inc(OnTile);
               Graph.GraphDraw.GraphLeftLabels.Add('');
               while not GISdb[DBonTable].MyData.eof do begin
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  if GISdb[DBonTable].MyData.FieldExists('DEMIX_TILE') then begin
                      Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE') + '  ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('LAND_TYPE'));
                  end
                  else if GISdb[DBonTable].MyData.FieldExists('FILTER') then begin
                      Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('FILTER'));
                  end;
                  inc(OnTile);

                  for DEM := 1 to NumDEMIXDEM do begin
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Filter= ' + GISdb[DBonTable].MyData.Filter + '  recs=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}

                     if (GISdb[DBonTable].MyData.GetFieldByNameAsString(DEMType[DEM]) <> '') then begin
                        v[2] := Ontile;
                        v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMType[DEM]);
                        if v[1] > Graph.GraphDraw.MaxHorizAxis then Graph.GraphDraw.MaxHorizAxis := v[1] + 1;
                        if v[1] < Graph.GraphDraw.MinHorizAxis then Graph.GraphDraw.MinHorizAxis := v[1] - 1;
                        BlockWrite(rfile[DEM],v,1);
                     end;
                  end;
                  GISdb[DBonTable].MyData.Next;
               end;
               if (Title <> '') then begin
                  inc(OnTile);
                  Graph.GraphDraw.GraphLeftLabels.Add(Title);
               end;
            end;
         end;

      var
         RefTypes : tStringList;
         i : integer;
      begin
         //{$If Defined(RecordDEMIX)} WriteLineToDebugFile('OneField in, ' + aField); {$EndIf}
         GISdb[DBonTable].EmpSource.Enabled := false;
         Graph := tThisBaseGraph.Create(Application);
         Graph.GraphDraw.LegendList := tStringList.Create;
         if GISdb[DBonTable].MyData.FieldExists('CRITERION') then Graph.GraphDraw.HorizLabel := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
         Graph.Caption := GISdb[DBonTable].DBName;

         for DEM := 1 to NumDEMIXDEM do begin
            Symbol := SymbolFromDEMName(DEMType[DEM]);
            Graph.OpenPointFile(rfile[DEM],Symbol);
            Graph.GraphDraw.LegendList.Add(DEMType[DEM]);
         end;

         Graph.GraphDraw.GraphAxes := XPartGridOnly;
         Graph.GraphDraw.MinHorizAxis := 0;
         Graph.GraphDraw.MaxHorizAxis := 0;
         Graph.GraphDraw.GraphLeftLabels := tStringList.Create;

         OnTile := 0;

         RefTypes := GISdb[DBonTable].MyData.UniqueEntriesInDB('REF_TYPE');
         for i := 0 to RefTypes.Count  do begin
            DoRefType('REF_TYPE=' + QuotedStr(RefTypes.Strings[i]),RefTypes.Strings[i]);
         end;
         RefTypes.Free;
         for DEM := 1 to NumDEMIXDEM  do begin
            CloseFile(rfile[DEM]);
         end;

         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
         Graph.GraphDraw.MinVertAxis := 0;
         Graph.GraphDraw.MaxVertAxis := OnTile;
         Graph.GraphDraw.ShowHorizAxis0 := true;
         Graph.AutoScaleAndRedrawDiagram(false,false);
         Graph.GraphDraw.LeftMargin := 225;
         Graph.Height := 130 + 26 * OnTile;
         Graph.RedrawDiagram11Click(Nil);

         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}
         GISdb[DBonTable].ApplyGISFilter(BaseFilter);
      end;

begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXwinecontestGraph in'); {$EndIf}
   BaseFilter := GISdb[DBonTable].MyData.Filter;
   BaseFilterAnd := '';
   if (BaseFilter <> '') then BaseFilterAnd := BaseFilter + ' AND ';

   OneField;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXwinecontestGraph out'); {$EndIf}
end;



procedure DEMIXwineContestScoresGraph(DBonTable : integer);

      var
         DEMsPresent: tstringList;
         Graph : tThisBaseGraph;
         Symbol : tFullSymbolDeclaration;
         DEM,OnTile : integer;
         rfile : array[1..10] of file;
         v : array[1..2] of float32;


         procedure DoRefType;
         var
            Tile,DEM,SlopeCat : integer;
            TStr : shortstring;
         begin
            GISdb[DBonTable].MyData.First;
            while not GISdb[DBonTable].MyData.eof do begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('FILTER');
               inc(OnTile);
               if (TStr = 'SKIP') then begin
                  Graph.GraphDraw.GraphLeftLabels.Add('------------------------');
               end
               else begin
                  Graph.GraphDraw.GraphLeftLabels.Add(TStr);
                  for DEM := 0 to pred(DEMsPresent.Count) do begin
                     v[2] := Ontile;
                     v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                     BlockWrite(rfile[succ(DEM)],v,1);
                  end;
               end;
               GISdb[DBonTable].MyData.Next;
            end;
         end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph  in, table=' + IntToStr(DBontable)); {$EndIf}

   if GISdb[DBonTable].MyData.FieldExists('FILTER') then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      DEMsPresent := tStringList.Create;
      DEMsPresent.LoadFromFile(ProgramRootDir + 'demix_dems.txt');

      Graph := tThisBaseGraph.Create(Application);
      Graph.GraphDraw.LegendList := tStringList.Create;
      Graph.GraphDraw.HorizLabel := 'Average ranking';
      Graph.Caption := GISdb[DBonTable].DBName;

      for DEM := 1 to DEMsPresent.Count do begin
         Symbol := SymbolFromDEMName(DEMsPresent.Strings[Pred(DEM)]);
         Graph.OpenPointFile(rfile[DEM],Symbol);
         Graph.GraphDraw.LegendList.Add(DEMsPresent.Strings[Pred(DEM)]);
      end;

      Graph.GraphDraw.GraphAxes := XPartGridOnly;
      Graph.GraphDraw.MinHorizAxis := 0.5;
      Graph.GraphDraw.MaxHorizAxis := 5.5;
      Graph.GraphDraw.GraphLeftLabels := tStringList.Create;

      OnTile := 0;

      DoRefType;

      for DEM := 1 to DEMsPresent.Count do begin
         CloseFile(rfile[DEM]);
      end;

      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
      Graph.GraphDraw.MinVertAxis := 0;
      Graph.GraphDraw.MaxVertAxis := OnTile;
      Graph.GraphDraw.ShowHorizAxis0 := true;
      Graph.AutoScaleAndRedrawDiagram(false,false);
      Graph.GraphDraw.LeftMargin := 475;
      Graph.Height := 130 + 26 * OnTile;
      Graph.RedrawDiagram11Click(Nil);

      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}
      DEMsPresent.Free;
      GISdb[DBonTable].ApplyGISFilter('');
      GISdb[DBonTable].EmpSource.Enabled := true;
   end
   else begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph in, requires BestDEMSbyCategory'); {$EndIf}
      //MessageToContinue('Select this option on the Scores Filter DB after ranking finishes');
      BestDEMSbyCategory(DBonTable);
   end;
end;


procedure DEMIXwineContestMeanMedianGraph(What,DBonTable : integer);
const
   SimpleTiles : array[1..4] of shortstring = ('N43PW002B','N37YW076F','N28VW018B','S24TW047D');
   SimpleParams : array[1..3] of shortstring = ('RUFD_AVG','SMD_MAE','ELD_RMSE');
var
   AllTiles,AllCriteria,DEMsPresent,SlopesPresent,AllAreas : tstringList;
   PickParam : shortstring;
   ThisTile : shortstring;
   WhatNow : integer;


         procedure MakeGraphForFilter(Param,LandType,DEMType : shortstring);
         var
            Graph : tThisBaseGraph;
            Symbol : tFullSymbolDeclaration;
            DEM,OnTile,DEMis,i : integer;
            rfile : array[1..10] of file;
            v : array[1..2] of float32;
            aMinVal,aMaxVal : float64;
            Filter1,Filter2,aFilter : shortstring;


               procedure OnePercentage(aFilter : shortstring);
               var
                  Total,i : integer;
               begin
                  GISdb[DBonTable].ApplyGISFilter(aFilter);
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  Total := GISdb[DBonTable].MyData.FiltRecsInDB;
                  inc(OnTile);
                  Graph.GraphDraw.GraphLeftLabels.Add(aFilter);
                  for i := 0 to pred(DEMsPresent.Count) do begin
                     GISdb[DBonTable].ApplyGISFilter(aFilter + ' AND DEM_LOW_SC=' + QuotedStr(DEMsPresent.Strings[i]));
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     v[2] := Ontile;
                     v[1] := 100 * GISdb[DBonTable].MyData.FiltRecsInDB / Total;
                     BlockWrite(rfile[succ(i)],v,1);
                  end;
               end;


         begin
            if What = dgSimpleExample then begin
               Filter1 := 'LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
               aFilter := '';
               for I := 1 to 4 do aFilter := AddOrIfNeeded(aFilter) + 'DEMIX_TILE=' + QuotedStr(SimpleTiles[i]);
               Filter1 := Filter1 + ' AND (' + aFilter + ')';
               aFilter := '';
               for I := 1 to 3 do aFilter := AddOrIfNeeded(aFilter) + 'CRITERION=' + QuotedStr(SimpleParams[i]);
               Filter1 := Filter1 + ' AND (' + aFilter + ')';

               WriteLineToDebugFile(Filter1);

            end
            else if What in [dgAllValues,dgAllScores,dgPercentBest] then begin
               GISdb[DBonTable].EmpSource.Enabled := false;
            end
            else if What = dgArea then begin
               Filter1 := 'LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
            end
            else begin
               Filter1 := 'CRITERION=' + QuotedStr(Param)  + ' AND LAND_TYPE=' + QuotedStr(LandType);
               Filter2 := Filter1 + ' AND REF_TYPE=' + QuotedStr(DEMType);
            end;
            GISdb[DBonTable].ApplyGISFilter(Filter1);
            if GISdb[DBonTable].MyData.FiltRecsInDB = 0 then exit;   //primarily needed if there are no DSM opinions

            Graph := tThisBaseGraph.Create(Application);
            Graph.GraphDraw.LegendList := tStringList.Create;
            Graph.GraphDraw.HorizLabel := Filter2;
            Graph.GraphDraw.VertLabel := 'DEMIX Tiles';
            Graph.Caption := GISdb[DBonTable].DBName;
            Graph.GraphDraw.MaxHorizAxis := -99999;
            Graph.GraphDraw.MinHorizAxis := 99999;

            GISdb[DBonTable].EmpSource.Enabled := false;
            for DEM := 1 to DEMsPresent.Count do begin
               GISdb[DBonTable].MyData.FindFieldRange(DEMsPresent.Strings[pred(DEM)],aMinVal,aMaxVal);
               if (aminVal < Graph.GraphDraw.MinHorizAxis) then Graph.GraphDraw.MinHorizAxis := aMinVal;
               if (amaxval > Graph.GraphDraw.MaxHorizAxis) then Graph.GraphDraw.MaxHorizAxis := aMaxVal;
            end;
            Graph.GraphDraw.GraphAxes := XPartGridOnly;

            for DEM := 1 to DEMsPresent.Count do begin
               Symbol := SymbolFromDEMName(DEMsPresent.Strings[Pred(DEM)]);
               Graph.OpenPointFile(rfile[DEM],Symbol);
               Graph.GraphDraw.LegendList.Add(DEMsPresent.Strings[Pred(DEM)]);
            end;
            OnTile := 0;

            Graph.GraphDraw.GraphLeftLabels := tStringList.Create;

            if What in [dgArea] then begin
               for i := 1 to AllAreas.Count do begin
                  GISdb[DBonTable].ApplyGISFilter(Filter1 + 'AND AREA=' + QuotedStr(AllAreas.Strings[pred(i)]));
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  Graph.GraphDraw.GraphLeftLabels.Add(AllAreas.Strings[pred(i)] + '  ' + LandType + '  ' + DEMType);
                  inc(OnTile);
                  for DEM := 0 to pred(DEMsPresent.Count) do begin
                     v[2] := Ontile;
                     v[1] := GISdb[DBonTable].MyData.FieldSum(DEMsPresent.Strings[DEM] + '_SCR') / GISdb[DBonTable].MyData.FiltRecsInDB;
                     BlockWrite(rfile[succ(DEM)],v,1);
                  end;
                  GISdb[DBonTable].MyData.Next;
               end;
            end
            else if What in [dgAllValues,dgAllScores] then begin
               Graph.GraphDraw.VertLabel := '';
               GISdb[DBonTable].MyData.First;
               while not GISdb[DBonTable].MyData.eof do begin
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  inc(OnTile);
                  Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION') + '  ' + LandType + '  ' + DEMType);
                  for DEM := 0 to pred(DEMsPresent.Count) do begin
                     v[2] := Ontile;
                     if What in [dgAllValues] then v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM])
                     else v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM] + '_SCR');
                     BlockWrite(rfile[succ(DEM)],v,1);
                  end;
                  GISdb[DBonTable].MyData.Next;
               end;
            end
            else if What in [dgPercentBest] then begin
               for i := 1 to 2 do OnePercentage('REF_TYPE=' + QuotedStr(RefType[i]));
               inc(OnTile);
               Graph.GraphDraw.GraphLeftLabels.Add('-----------');

               for i := 1 to MaxRefCat do OnePercentage('LAND_TYPE=' + QuotedStr(SlopeType[i]));
               inc(OnTile);
               Graph.GraphDraw.GraphLeftLabels.Add('-----------');
               for i := 1 to AllCriteria.Count do OnePercentage('CRITERION=' + QuotedStr(AllCriteria.Strings[pred(i)]));
            end
            else if What in [dgSimpleExample] then begin
               Graph.GraphDraw.VertLabel := '';
               GISdb[DBonTable].MyData.First;
               while not GISdb[DBonTable].MyData.eof do begin
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  inc(OnTile);
                  Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION') + '  ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'));
                  for DEM := 0 to pred(DEMsPresent.Count) do begin
                     v[2] := Ontile;
                     if WhatNow in [dgAllValues] then v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM])
                     else v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM] + '_SCR');
                     BlockWrite(rfile[succ(DEM)],v,1);
                  end;
                  GISdb[DBonTable].MyData.Next;
               end;
            end
            else begin
               GISdb[DBonTable].ApplyGISFilter(Filter2);
               //GISdb[DBonTable].MyData.First;
               while not GISdb[DBonTable].MyData.eof do begin
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  inc(OnTile);
                  Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'));
                  for DEM := 0 to pred(DEMsPresent.Count) do begin
                     v[2] := Ontile;
                     v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                     BlockWrite(rfile[succ(DEM)],v,1);
                  end;
                  GISdb[DBonTable].MyData.Next;
               end;
            end;

            for DEM := 1 to DEMsPresent.Count do begin
               CloseFile(rfile[DEM]);
            end;

            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
            Graph.GraphDraw.MinVertAxis := -1;
            Graph.GraphDraw.MaxVertAxis := OnTile + 2;
            Graph.GraphDraw.ShowHorizAxis0 := true;
            Graph.Width := 1200;
            Graph.Height := 700;

            Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
            if What in [dgArea] then begin
               Graph.GraphDraw.VertLabel := '';
               Graph.GraphDraw.LeftMargin := 350;
               Graph.GraphDraw.BottomMargin := 75;
               Graph.GraphDraw.HorizLabel := 'Average Ranking';
               Graph.GraphDraw.MinHorizAxis := 0.8;
               Graph.GraphDraw.MaxHorizAxis := 6.2;
               Graph.RedrawDiagram11Click(Nil);
            end;
            if (What in [dgPercentBest]) then begin
               Graph.GraphDraw.VertLabel := '';
               Graph.GraphDraw.LeftMargin := 300;
               Graph.GraphDraw.BottomMargin := 75;
               Graph.GraphDraw.HorizLabel := 'Percentage of Criteria Where DEM is Best';
               Graph.GraphDraw.MinHorizAxis := 0;
               Graph.GraphDraw.MaxHorizAxis := 100;
               Graph.RedrawDiagram11Click(Nil);
            end;

            if (What in [dgSimpleExample]) then begin
               Graph.GraphDraw.VertLabel := '';
               Graph.GraphDraw.LeftMargin := 125;
               Graph.GraphDraw.BottomMargin := 75;
               if WhatNow in [dgAllValues] then begin
                  Graph.GraphDraw.HorizLabel := 'Criterion Difference';
                  Graph.GraphDraw.MinHorizAxis := 0;
               end
               else begin
                  Graph.GraphDraw.HorizLabel := 'Criterion Ranking';
                  Graph.GraphDraw.MinHorizAxis := 0.8;
                  Graph.GraphDraw.MaxHorizAxis := 6.2;
               end;
               Graph.GraphDraw.HorizLabel := LandType + '  ' + DEMType + '  ' + Graph.GraphDraw.HorizLabel;
               Graph.RedrawDiagram11Click(Nil);
            end;

            if (What in [dgAllValues,dgAllScores,dgPick]) then begin
               Graph.GraphDraw.VertLabel := '';
               Graph.GraphDraw.LeftMargin := 275;
               Graph.GraphDraw.BottomMargin := 75;
               if What in [dgPick] then begin
                  Graph.GraphDraw.LeftMargin := 150;
                  Graph.Height := 40 * OnTile + Graph.GraphDraw.BottomMargin;
               end
               else if What in [dgAllValues] then begin
                  Graph.GraphDraw.HorizLabel := ThisTile + ' Criterion Difference';
                  Graph.GraphDraw.MinHorizAxis := 0;
               end
               else begin
                  Graph.GraphDraw.HorizLabel := ThisTile + ' Criterion Ranking';
                  Graph.GraphDraw.MinHorizAxis := 0.8;
                  Graph.GraphDraw.MaxHorizAxis := 6.2;
               end;
               Graph.RedrawDiagram11Click(Nil);
            end;
         end;

var
   j : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestMeanMedianGraph in, table=' + IntToStr(DBontable)); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   DEMsPresent := tStringList.Create;
   DEMsPresent.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
   SlopesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   GISdb[DBonTable].EmpSource.Enabled := false;
   AllCriteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');

   GISdb[DBonTable].EmpSource.Enabled := false;
   if What = dgPick then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      PickParam := GetFromList('Criterion to plot',AllCriteria);
   end;
   if What in [dgAllValues,dgAllScores] then begin
      AllTiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
      ThisTile := GetFromList('DEMIX tile to plot',AllTiles);
      FreeAndNil(AllTiles);
   end;

   if What in [dgArea] then begin
      AllAreas := GISdb[DBonTable].MyData.UniqueEntriesInDB('AREA');
   end;

   if What in [dgSimpleExample] then begin
      WhatNow := dgAllValues;
      MakeGraphForFilter('','ALL','DTM');
      WhatNow := dgAllScores;
      MakeGraphForFilter('','ALL','DTM');
   end
   else if What in [dgPercentBest] then begin
      MakeGraphForFilter('','','');
   end
   else begin
      for j := 2 downto 1 do begin
         if What = dgMean then begin
            MakeGraphForFilter(SignedParams[1],SlopeType[1],RefType[j]);
            MakeGraphForFilter(SignedParams[3],SlopeType[1],RefType[j]);
            MakeGraphForFilter(SignedParams[5],SlopeType[1],RefType[j]);
         end
         else if What = dgMedian then begin
            MakeGraphForFilter(SignedParams[2],SlopeType[1],RefType[j]);
            MakeGraphForFilter(SignedParams[4],SlopeType[1],RefType[j]);
            MakeGraphForFilter(SignedParams[6],SlopeType[1],RefType[j]);
         end
         else if What in [dgAllValues,dgAllScores] then begin
            what := dgAllValues; MakeGraphForFilter('',SlopeType[1],RefType[j]);
            what := dgAllScores; MakeGraphForFilter('',SlopeType[1],RefType[j]);
         end
         else if What in [dgArea] then begin
            MakeGraphForFilter('',SlopeType[1],RefType[j]);
         end
         else begin
            MakeGraphForFilter(PickParam,SlopeType[1],RefType[j]);
         end;
      end;
   end;

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}
   DEMsPresent.Free;
   SlopesPresent.Free;
   AllCriteria.Free;
   FreeAndNil(AllAreas);

   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
end;


procedure DEMIXTileSummary(DBonTable : integer);
var
   DEMsPresent,SlopesPresent,Report,CriteriaPresent : tStringList;
   j,k : integer;
   aLine : shortstring;
   fName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;

   DEMsPresent := tStringList.Create;
   DEMsPresent.LoadFromFile(ProgramRootDir + 'demix_dems.txt');

   GISdb[DBonTable].EmpSource.Enabled := false;
   SlopesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   GISdb[DBonTable].EmpSource.Enabled := false;
   CriteriaPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');

   Report := tStringList.Create;
   aline := 'REF_TYPE,AREA,DEMIX_TILE';
   for j := 0 to pred(SlopesPresent.Count) do aLine := aLine + ',' + SlopesPresent.strings[j];
   Report.Add(Aline);

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}

   for j := 2 downto 1 do begin
      aline := RefType[j];
      GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefType[j]));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA'));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE'));
      for k := 0 to pred(SlopesPresent.Count) do begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefType[j]) + ' AND LAND_TYPE=' + QuotedStr(SlopesPresent[k]));
         aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB div CriteriaPresent.Count);
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}
      end;
      Report.Add(aline);
   end;

   DEMsPresent.Free;
   SlopesPresent.Free;
   CriteriaPresent.Free;
   fName := NextFileNumber(MDTempDir,'demix_tile_summary_','.dbf');
   StringList2CSVtoDB(Report,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary out'); {$EndIf}

end;




(*
procedure DEMIXwinecontestGraph(DBonTable : integer);

      procedure OneField(aField : shortstring; Extra : shortString = '');
      var
         TilesPresent,DEMsPresent,SlopesPresent : tstringList;
         Graph : tThisBaseGraph;
         Symbol : tFullSymbolDeclaration;
         DEM,OnTile : integer;
         rfile : array[1..10] of file;
         v : array[1..2] of float32;


         procedure DoRefType(aFilterElement,Title : shortString);
         var
            Tile,DEM,SlopeCat : integer;
         begin
            GISdb[DBonTable].MyData.First;
            GISdb[DBonTable].ApplyGISFilter(aFilterElement);
            if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
               inc(OnTile);
               Graph.GraphDraw.GraphLeftLabels.Add('');
               for Tile := 1 to TilesPresent.Count do begin
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Tile ' + TilesPresent.Strings[pred(Tile)]); {$EndIf}
                  for SlopeCat := 1 to SlopesPresent.Count do begin
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SlopeCat=' + SlopesPresent.Strings[Pred(SlopeCat)]); {$EndIf}
                     Graph.GraphDraw.GraphLeftLabels.Add(TilesPresent.Strings[pred(Tile)] + '  ' + SlopesPresent.Strings[Pred(SlopeCat)]);
                     inc(OnTile);
                     for DEM := 1 to DEMsPresent.Count do begin
                        GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(TilesPresent.Strings[pred(Tile)]) + ' AND ' + aFilterElement +
                           ' AND DEM=' + QuotedStr(DEMsPresent.Strings[Pred(DEM)]) + ' AND LAND_TYPE=' + QuotedStr(SlopesPresent.Strings[Pred(SlopeCat)]));
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Filter= ' + GISdb[DBonTable].MyData.Filter + '  recs=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
                        if (GISdb[DBonTable].MyData.GetFieldByNameAsString(aField) <> '') then begin
                           v[2] := Ontile;
                           v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(aField);
                           if v[1] > Graph.GraphDraw.MaxHorizAxis then Graph.GraphDraw.MaxHorizAxis := v[1] + 1;
                           if v[1] < Graph.GraphDraw.MinHorizAxis then Graph.GraphDraw.MinHorizAxis := v[1] - 1;
                           BlockWrite(rfile[DEM],v,1);
                        end;
                        GISdb[DBonTable].MyData.Next;
                     end;
                  end;
               end;
               inc(OnTile);
               Graph.GraphDraw.GraphLeftLabels.Add(Title);
            end;
         end;


      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('OneField in, ' + aField); {$EndIf}
         GISdb[DBonTable].EmpSource.Enabled := false;
         TilesPresent := nil;
         DEMsPresent := nil;
         TilesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
         DEMsPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEM');
         SlopesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
         Graph := tThisBaseGraph.Create(Application);
         Graph.GraphDraw.LegendList := tStringList.Create;
         Graph.GraphDraw.HorizLabel := aField + ' ' + Extra;
         Graph.Caption := GISdb[DBonTable].DBName;

         for DEM := 1 to DEMsPresent.Count do begin
            Symbol := SymbolFromDEMName(DEMsPresent.Strings[Pred(DEM)]);
         end;

         Graph.GraphDraw.GraphAxes := XPartGridOnly;
         Graph.GraphDraw.MinHorizAxis := 0;
         Graph.GraphDraw.MaxHorizAxis := 0;
         Graph.GraphDraw.GraphLeftLabels := tStringList.Create;

         OnTile := 0;

         DoRefType('REF_TYPE=' + QuotedStr('DTM'),'DTM');
         DoRefType('REF_TYPE=' + QuotedStr('DSM'),'DSM');

         for DEM := 1 to DEMsPresent.Count do begin
            CloseFile(rfile[DEM]);
         end;

         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
         Graph.GraphDraw.MinVertAxis := 0;
         Graph.GraphDraw.MaxVertAxis := OnTile;
         Graph.GraphDraw.ShowHorizAxis0 := true;
         Graph.AutoScaleAndRedrawDiagram(false,false);
         Graph.GraphDraw.LeftMargin := 225;
         Graph.Height := 130 + 26 * OnTile;
         Graph.RedrawDiagram11Click(Nil);

         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}
         TilesPresent.Free;
         DEMsPresent.Free;
         GISdb[DBonTable].ApplyGISFilter('');
      end;

begin
   if (GISdb[DBonTable].DBName = 'elev_merge_difference_ref_dem') then begin
      OneField('ELD_MEAN');
      OneField('ELD_MEDIAN');
      OneField('ELD_RMSE');
      OneField('ELD_LE90');
      //OneField('ELD_STD');
   end
   else if (GISdb[DBonTable].DBName = 'slope_merge_difference_ref_dem') then begin
      OneField('SMD_MEDIAN');
      OneField('SMD_MEAN');
      OneField('SMD_RMSE');
      //OneField('SMD_STD');
   end
   else if (GISdb[DBonTable].DBName = 'ruff_merge_difference_ref_dem') then begin
      OneField('RUFD_MED');
      OneField('RUFD_MEAN');
      OneField('RUFD_RMSE');
      //OneField('RUFD_STD');
   end
   else if GISdb[DBonTable].DBName = 'Slope_moments_diff' then begin
      OneField('SDIFF_MEAN');
      OneField('SDIFF_MED');
      OneField('SDIFF_STD');
      OneField('SDIFF_SKEW');
      OneField('SDIFF_KURT');
   end
   else if GISdb[DBonTable].DBName = 'Elevation_moments_diff' then begin
      OneField('EDIFF_MEAN');
      OneField('EDIFF_MED');
      OneField('EDIFF_STD');
      OneField('EDIFF_SKEW');
      OneField('EDIFF_KURT');
   end
   else MessageToContinue('Not DEMIX results table');
   GISdb[DBonTable].EmpSource.Enabled := true;
end;
*)


