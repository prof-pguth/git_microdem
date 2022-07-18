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
   value,ThisDEM : shortstring;
   Cycles,Done,
   i,j,  Tile, Ref, Slope,DEM,Criterion : Integer;
   Values : array[0..MaxDEMs,0..MaxCriteria] of shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('');  WriteLineToDebugFile('TransposeDEMIXcriteria in, ' + GISdb[DBonTable].dbName); {$EndIf}
   StopSplashing;
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
                  {$If Defined(RecordDEMIX)} WriteLineToDebugFile('filter=' + GISdb[DBonTable].MyData.Filter + '   Matches=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
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
const
   TieTolerance = 0.01;
var
   DEMs,Scores : tStringList;
   DEM,Value,N,i,Score : integer;
   Sum,fValue : float32;
   fName : PathStr;
   aLine : shortstring;
   Values,Sorted : array[0..10] of float32;
   Results : tStringList;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS in, ' + GISdb[DBonTable].dbName); {$EndIf}
    DEMs := tStringList.Create;
    DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
    for DEM := 0 to pred(DEMs.Count) do begin
       GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftFloat,DEMs.Strings[DEM] + '_SCR',8,2);
    end;

    StartProgress('Rank DEMs');
    i := 0;
    GISdb[DBonTable].MyData.First;
    while not GISdb[DBonTable].MyData.eof do begin
       inc(i);
       UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
       GISdb[DBonTable].EmpSource.Enabled := false;
       for DEM := 0 to pred(DEMs.Count) do begin
          Values[DEM] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMs[DEM]);
          Sorted[DEM] := Values[DEM];
       end;

       HeapSort(DEMs.Count, Sorted);

       GISdb[DBonTable].MyData.Edit;
       for DEM := 0 to pred(DEMs.Count) do begin
          Sum := 0;
          N := 0;
          for value := pred(DEMs.Count) downto 0 do begin
             if (abs(Values[DEM]-Sorted[Value]) < TieTolerance) then begin
                Sum := Sum + succ(Value);
                inc(n);
             end;
          end;
          GISdb[DBonTable].MyData.SetFieldByNameAsFloat(DEMs.Strings[DEM] + '_SCR',Sum/N);
       end;
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
    GISdb[DBonTable].ClearGISFilter;

    DEMs.Destroy;
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
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND BEST_DEM=' + QuotedStr(DEMs.Strings[DEM]) );
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

      RefFilter := ' AND REF_TYPE=' + QuotedStr(RefType[i]) + ' AND LAND_TYPE=' + QuotedStr('ALL');

      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile avg slope > 18%','AVG_SLOPE > 18' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile avg slope < 18%','AVG_SLOPE < 18' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile relief < 25m','RELIEF < 25' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile relief > 500m','RELIEF > 500' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile avg rough > 10%','AVG_ROUGH > 10' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile avg rough < 5%','AVG_ROUGH < 5' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile urban > 25%','URBAN_PC > 25' + RefFilter );
      DoOne(RefType[i] + ' ' + 'ALL pixels and Tile forest > 50%','FOREST_PC > 50' + RefFilter );

      for j := 0 to pred(Criteria.Count) do begin
         DoOne(RefType[i] + ' ALL pixels and ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
      end;
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


procedure ScoreDEMS(DBonTable : integer);
var
   DEMs : tStringList;
   DEM : integer;
   Value,BestValue : float32;
   BestValueString : shortstring;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXScoreDEMs in, ' + GISdb[DBonTable].dbName); {$EndIf}
    StopSplashing;
    GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'BEST_DEM',32);
    DEMs := tStringList.Create;
    DEMs.LoadFromFile(ProgramRootDir + 'demix_dems.txt');
    GISdb[DBonTable].MyData.First;
    while not GISdb[DBonTable].MyData.eof do begin
       GISdb[DBonTable].EmpSource.Enabled := false;
       BestValue := 99e38;
       for DEM := 0 to pred(DEMs.Count) do begin
          Value := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMs[DEM]);
          if (Value > 0) then begin
             if (abs(Value - BestValue) < MDDef.DEMIXTieTolerance) then begin
                BestValueString := BestValueString + '-' + DEMs[DEM];
             end;
             if (Value < BestValue) then begin
                BestValue := Value;
                BestValueString := DEMs[DEM];
             end;
          end;
       end;
       GISdb[DBonTable].MyData.Edit;
       GISdb[DBonTable].MyData.SetFieldByNameAsString('BEST_DEM',BestValueString);  //DEMs[BestDEM]);
       GISdb[DBonTable].MyData.Next;
    end;
    GISdb[DBonTable].ShowStatus;
    DEMs.Destroy;
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXScoreDEMs out, ' + GISdb[DBonTable].dbName); {$EndIf}
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
               if Title <> '' then begin
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
   if (GISdb[DBonTable].DBName = 'Elev_diff_stats') then begin
      OneField('ELD_MEAN');
      OneField('ELD_MEDIAN');
      OneField('ELD_RMSE');
      OneField('ELD_LE90');
      //OneField('ELD_STD');
   end
   else if (GISdb[DBonTable].DBName = 'Slope_diff_stats') then begin
      OneField('SMD_MEDIAN');
      OneField('SMD_MEAN');
      OneField('SMD_RMSE');
      //OneField('SMD_STD');
   end
   else if (GISdb[DBonTable].DBName = 'Roughness_diff_stats') then begin
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
         begin
            GISdb[DBonTable].MyData.First;
            while not GISdb[DBonTable].MyData.eof do begin
               GISdb[DBonTable].EmpSource.Enabled := false;
              // {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SlopeCat=' + SlopesPresent.Strings[Pred(SlopeCat)]); {$EndIf}
               Graph.GraphDraw.GraphLeftLabels.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('FILTER'));
               inc(OnTile);

               for DEM := 0 to pred(DEMsPresent.Count) do begin
                     v[2] := Ontile;
                     v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                     BlockWrite(rfile[succ(DEM)],v,1);
               end;
               GISdb[DBonTable].MyData.Next;
            end;
         end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph  in, table=' + IntToStr(DBontable)); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   DEMsPresent := nil;
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
end;

