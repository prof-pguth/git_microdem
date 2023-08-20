{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{    include file for demdatbase     }
{____________________________________}


procedure DEMIXMeanMedianHistograms(db : integer);
var
   Min,Max,BinSize : float32;
   Corner : byte;


   procedure MakeOne(DEMType,Param : shortString);
   var
      zvs : array[1..MaxTestDEM] of ^Petmath.bfarray32;
      i,npts,DEM : integer;
      Dist,Legs : tStringList;
      Graph : tThisBaseGraph;
      TStr : shortstring;
   begin
      Dist := tStringList.Create;
      Legs := tStringList.Create;
      for DEM := 1 to MaxTestDEM do begin
         Legs.Add(DEMIXDEMTypeName[DEM]);
      end;
      GISdb[DB].ApplyGISFilter('REF_TYPE=' + QuotedStr(DEMtype) + ' AND LAND_TYPE=' + QuotedStr('ALL') + ' AND CRITERION=' + QuotedStr(Param));
      GISdb[DB].EmpSource.Enabled := false;
      for I := 1 to MaxTestDEM do New(zvs[i]);
      npts := 0;
       GISdb[DB].MyData.First;
       while not GISdb[DB].MyData.eof do begin
          for DEM := 1 to MaxTestDEM do begin
             zvs[DEM]^[npts] := GISdb[DB].MyData.GetFieldByNameAsFloat(DEMIXDEMTypeName[DEM]);
          end;
          inc(Npts);
          GISdb[DB].MyData.Next;
       end;

       for I := 1 to MaxTestDEM do begin
          Dist.Add(SaveSingleValueSeries(npts,zvs[i]^));
          Dispose(zvs[i]);
       end;
       TStr := Param + ' compared to reference ' + DEMtype;
       Graph := CreateMultipleHistogram(MDDef.CountHistograms,Dist,Legs,TStr,TStr,100,Min,Max,BinSize);

      for I := 1 to MaxTestDEM do begin
         Graph.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Graph.GraphDraw.LegendList[pred(i)]);
      end;
      Graph.GraphDraw.InsideMarginLegend := Corner;
      Graph.RedrawDiagram11Click(Nil);
   end;

var
   i,j : integer;
begin
   Min := -4;
   Max := 4;
   BinSize := 0.5;
   Corner := lpNWMap;
   for j := 1 to 2 do begin
      for i := 1 to 3 do begin
         MakeOne(RefDEMtype[j],MeanParams[i]);
      end;
   end;

   for j := 1 to 2 do begin
      for i := 1 to 3 do begin
         MakeOne(RefDEMtype[j],MedianParams[i]);
      end;
   end;

   Min := 0;
   Max := 6;
   BinSize := 0.25;
   Corner := lpNEMap;
   for j := 1 to 2 do begin
      for i := 1 to 3 do begin
         MakeOne(RefDEMtype[j],StdDevParams[i]);
      end;
   end;
end;



procedure FilterInSignedCriteria(DBonTable : integer);
var
   aFilter : ANSIstring;
   i : integer;
begin
    aFilter := '';
    for I := 1 to 3 do begin
       aFilter := AddOrIfNeeded(aFilter) + 'CRITERION=' + QuotedStr(MeanParams[i]);
       aFilter := AddOrIfNeeded(aFilter) + 'CRITERION=' + QuotedStr(MedianParams[i]);
    end;
    GISdb[DBOntable].ApplyGISFilter(aFilter);
    GISdb[DBonTable].ShowStatus;
end;


function CreateFilterOutSignedCriteria(DBonTable : integer) : shortstring;
var
   i : integer;
begin
    Result := '' ;
    for I := 1 to 3 do begin
       Result := AddAndIfNeeded(Result) + 'CRITERION<>' + QuotedStr(MeanParams[i]);
       Result := AddAndIfNeeded(Result) + 'CRITERION<>' + QuotedStr(MedianParams[i]);
    end;
    Result := '(' + Result + ')';
end;


procedure FilterOutSignedCriteria(DBonTable : integer);
begin
    GISdb[DBOntable].ApplyGISFilter(CreateFilterOutSignedCriteria(DBonTable));
    GISdb[DBonTable].ShowStatus;
end;


procedure RankDEMS(DBonTable : integer; UseAll : boolean = false);
const
   IttyBitty = 0.00001;  //added for floating point funny business between Delphi and Python
   DoTrio = false;
var
   DEMsRanked,Scores : tStringList;
   DEM,Value,N,i,j,Score,BestScore : integer;
   Sum,fValue,BestValue,TieTolerance,Cop,alos,fab : float32;
   fName : PathStr;
   BestValueString,aLine,Which : shortstring;
   Values : array[0..10] of float32;
   RankDEMs : array[0..10] of shortstring;
   Results,Scoring : tStringList;
   TieToleranceTable : tMyData;
   GridForm : TGridForm;

       procedure FindTies(StartDEM : integer; var EndDEM : integer);
       var
          i,N : integer;
       begin
          {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Find ties in, DEM=' + IntToStr(StartDEM)); {$EndIf}
          BestValueString := RankDEMs[StartDEM];
          EndDEM := succ(StartDEM);
          BestScore := succ(StartDEM);
          N := 1;
          while (Values[EndDEM] < (Values[StartDEM] + TieTolerance + IttyBitty)) and (EndDEM < DEMsRanked.Count) do begin
             BestValueString := BestValueString + ',' + RankDEMs[EndDEM];
             inc(EndDEM);
             inc(N);
             BestScore := BestScore + EndDEM;
          end;
          if (StartDEM = 0) then GISdb[DBonTable].MyData.SetFieldByNameAsString('DEM_LOW_SC',BestValueString);
          {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Find ties =' + BestValueString); {$EndIf}

          for i := StartDEM to pred(EndDEM) do begin
             GISdb[DBonTable].MyData.SetFieldByNameAsFloat(RankDEMs[i] + '_SCR',BestScore/N);
             {$If Defined(RecordDEMIXties)} WriteLineToDebugFile(RankDEMs[i] + RealToString(BestScore/N,64,-2) + RealToString(Values[i],8,2)); {$EndIf}
          end;
          {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Find ties out, EndDEM=' + IntToStr(EndDEM)); {$EndIf}
       end;

var
   FirstDEM : integer;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS in, ' + GISdb[DBonTable].dbName); {$EndIf}
    if (not UseAll) and (not GetFileFromDirectory('DEMIX criterial tolerances','*.dbf',MDDef.DEMIX_criterion_tolerance_fName)) then exit;
    if GISdb[DBonTable].MyData.GetFieldLength('DEM_LOW_SC') < 48 then GISdb[DBonTable].MyData.TrimField('DEM_LOW_SC',48);

    if DoTrio then begin
       GISdb[DBonTable].AddFieldToDataBase(ftString,'COP_FAB',8);
       GISdb[DBonTable].AddFieldToDataBase(ftString,'ALOS_FAB',8);
    end;

    DEMsRanked := tStringList.Create;
    DEMsRanked.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');
    i := 0;
    if UseAll or GetFromListZeroBased('DEMs to use in ranking',i,DEMsRanked,true,true) then begin
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Load criterion tolerances, ' + MDDef.DEMIX_criterion_tolerance_fName); {$EndIf}
       if (DEMsRanked.Count < 3) then begin
          MessageToContinue('Must pick 3 or more DEMs');
       end
       else begin
          TieToleranceTable := tMyData.Create(MDDef.DEMIX_criterion_tolerance_fName);
          if TieToleranceTable.FieldExists('CRITERION') and TieToleranceTable.FieldExists('TOLERANCE') then begin

             GridForm := TGridForm.Create(Application);
             StartProgress('Rank DEMs');
             j := 0;
             GISdb[DBonTable].MyData.First;
             while not GISdb[DBonTable].MyData.eof do begin
                {$If Defined(RecordDEMIXties)} HighLightLineToDebugFile('Loop =' + IntToStr(j)); {$EndIf}
                inc(j);
                UpdateProgressBar(j/GISdb[DBonTable].MyData.FiltRecsInDB);
                GISdb[DBonTable].EmpSource.Enabled := false;

                Scoring := tStringList.Create;
                Scoring.Add('DEM,VALUE,SCORE');
                for DEM := 0 to pred(DEMsRanked.Count) do begin
                   aLine := DEMsRanked.Strings[DEM] + ',';
                   aLine := aLine + RealToString(abs(GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsRanked.Strings[DEM])),-12,-2) + ',';
                   aLine := aLine + RealToString(GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsRanked.Strings[DEM] + '_SCR'),-12,-2);
                   Scoring.Add(aline);
                   {$If Defined(RecordDEMIXties)} WriteLineToDebugFile(aLine); {$EndIf}
                end;
                fName := MDTempDir + 'ranking.csv';
                Scoring.SaveToFile(fName);
                GridForm.ReadCSVFile(fName);
                Scoring.Destroy;
                SortGrid(GridForm.StringGrid1,0,2,true);

                {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Now sorted'); {$EndIf}
                for DEM := 0 to pred(DEMsRanked.Count) do begin
                   RankDEMs[DEM] := GridForm.StringGrid1.Cells[0,succ(DEM)];
                   Values[DEM] := StrToFloat(GridForm.StringGrid1.Cells[1,succ(DEM)]);
                   {$If Defined(RecordDEMIXties)} WriteLineToDebugFile(RankDEMs[DEM] + RealToString(Values[DEM],8,-2)); {$EndIf}
                end;

                TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION')));
                if (TieToleranceTable.FiltRecsInDB = 0) then TieTolerance := 0.25
                else TieTolerance := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');

                GISdb[DBonTable].MyData.Edit;
                GISdb[DBonTable].MyData.SetFieldByNameAsFloat('TOLERANCE',TieTolerance);

                if DoTrio then begin
                   cop := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('COP');
                   alos := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('ALOS');
                   fab := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('FABDEM');

                   if (abs(cop-alos) <= (TieTolerance + IttyBitty)) then which := 'tie'
                   else if (cop < alos) then which := 'cop'
                   else which := 'alos';
                   GISdb[DBonTable].MyData.SetFieldByNameAsString('COP_ALOS',which);

                   if (abs(fab-alos) <= (TieTolerance + IttyBitty)) then which := 'tie'
                   else if (fab < alos) then which := 'fab'
                   else which := 'alos';
                   GISdb[DBonTable].MyData.SetFieldByNameAsString('ALOS_FAB',which);

                   if (abs(cop-fab) <= (TieTolerance + IttyBitty)) then which := 'tie'
                   else if (cop < fab) then which := 'cop'
                   else which := 'fabdem';
                   GISdb[DBonTable].MyData.SetFieldByNameAsString('COP_FAB',which);
                end;

                FirstDEM := 0;
                repeat
                   FindTies(FirstDEM,DEM);
                   FirstDEM := DEM;
                until (DEM = DEMsRanked.Count);
                GISdb[DBonTable].MyData.Next;
             end;
             GridForm.Destroy;
          end
          else begin
              MessageToContinue('Table must have fields "CRITERION" and "TOLERANCE"');
          end;
       end;
    end;
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
   Sum,Average : float64;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXScoreDEMs in, ' + GISdb[DBonTable].dbName); {$EndIf}
    StopSplashing;
    DEMs := tStringList.Create;
    DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');
    Findings := tStringList.Create;
    Findings.Add('DEM,SCORE_SUM,SCORE_AVG');
    for DEM := 0 to pred(DEMs.Count) do begin
       GISdb[DBonTable].EmpSource.Enabled := false;
       GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
       Sum := GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
       Average := Sum / GISdb[DBonTable].MyData.FiltRecsInDB;
       Findings.Add(DEMs.Strings[DEM] + ',' + RealToString(Sum,12,-2) + ',' + RealToString(Average,12,-2));
    end;
    DEMs.Destroy;
    GISdb[DBonTable].ShowStatus;
    fName := NextFileNumber(MDTempDir,'scores_','.dbf');
    StringList2CSVtoDB(Findings,fName);
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

                     if (GISdb[DBonTable].MyData.GetFieldByNameAsString(DEMIXDEMTypeName[DEM]) <> '') then begin
                        v[2] := Ontile;
                        v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXDEMTypeNAME[DEM]);
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
         if GISdb[DBonTable].MyData.FieldExists('CRITERION') then Graph.GraphDraw.HorizLabel := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
         Graph.Caption := GISdb[DBonTable].DBName;

         Graph.GraphDraw.LegendList := tStringList.Create;
         for DEM := 1 to NumDEMIXDEM do begin
            Symbol := SymbolFromDEMName(DEMIXDEMTypeName[DEM]);
            Graph.OpenPointFile(rfile[DEM],Symbol);
            Graph.GraphDraw.LegendList.Add(DEMIXDEMTypeName[DEM]);
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




procedure DEMIXwineContestMeanMedianGraph(What,DBonTable : integer; AreaList : tStringList = nil; CriteriaUsed : tStringList = nil; LandTypePresent : tStringList = nil; DEMsPresent : tStringList = nil);
//this includes a number of options that have not been used recently, and may not currently work
const
   InterAreaSkip = 4;
var
   AllTiles : tStringList;
   BigGraph : tstringList;
   PickParam,ThisTile : shortstring;
   WhatNow : integer;
   LegendfName,fName : PathStr;


         procedure MakeGraphForFilter(Param,LandType,DEMType : shortstring);
         var
            Graph : tThisBaseGraph;
            Symbol : tFullSymbolDeclaration;
            DEM,CurrentY,DEMis,i,j,k,NumAreas,NumTiles : integer;
            rfile : array[1..10] of file;
            TileList : tStringList;
            v : array[1..2] of float32;
            Max : float32;
            aMinVal,aMaxVal : float64;
            RefFilter,aFilter : shortstring;
            bmp : tMyBitmap;

               procedure OnePercentage(aFilter : shortstring);
               var
                  Total,i : integer;
               begin
                  GISdb[DBonTable].ApplyGISFilter(aFilter);
                  GISdb[DBonTable].EmpSource.Enabled := false;
                  Total := GISdb[DBonTable].MyData.FiltRecsInDB;
                  dec(CurrentY);
                  Graph.GraphDraw.GraphLeftLabels.Add(aFilter);
                  for i := 0 to pred(DEMsPresent.Count) do begin
                     GISdb[DBonTable].ApplyGISFilter(aFilter + ' AND DEM_LOW_SC=' + QuotedStr(DEMsPresent.Strings[i]));
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     v[2] := CurrentY;
                     v[1] := 100 * GISdb[DBonTable].MyData.FiltRecsInDB / Total;
                     BlockWrite(rfile[succ(i)],v,1);
                  end;
               end;

         begin {MakeGraphForFilter}
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphForFilter in, ' + Param + ' ' + Landtype + ' ' + DEMtype); {$EndIf}
            WMDEM.SetPanelText(1,Param + ' ' + Landtype + ' ' + DEMtype);
            if (What in [dgAllValues,dgAllScores,dgPercentBest]) then begin
            end
            else if (What in [dgArea]) then begin
               RefFilter := 'LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
            end
            else if (What in [dgPick,dg7Params,dgNormalizedDiff ]) then begin
               RefFilter := 'CRITERION=' + QuotedStr(Param)  + ' AND LAND_TYPE=' + QuotedStr(LandType) + ' AND REF_TYPE=' + QuotedStr(DEMType);
            end;

            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].ApplyGISFilter(RefFilter);
            if (GISdb[DBonTable].MyData.FiltRecsInDB = 0) then begin //primarily needed if there are no DSM opinions
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No Recs for ' + GISdb[DBonTable].MyData.Filter); {$EndIf}
               exit;
            end;

            GISdb[DBonTable].EmpSource.Enabled := false;
            NumTiles := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE');
            GISdb[DBonTable].EmpSource.Enabled := false;
            NumAreas := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA');

            CurrentY := NumTiles + (InterAreaSkip * NumAreas);

            Graph := tThisBaseGraph.Create(Application);
            Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
            Graph.GraphDraw.LegendList := tStringList.Create;
            Graph.GraphDraw.VertLabel := '';
            Graph.Caption := GISdb[DBonTable].DBName;
            Graph.GraphDraw.MaxHorizAxis := -99999;
            Graph.GraphDraw.MinHorizAxis := 99999;
            Graph.GraphDraw.MinVertAxis := -1;
            Graph.GraphDraw.MaxVertAxis := CurrentY;
            Graph.GraphDraw.ShowHorizAxis0 := true;

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

            for i := 0 to pred(AreaList.Count) do begin
             GISdb[DBonTable].EmpSource.Enabled := false;
                if (RefFilter <> '') then RefFilter := RefFilter + ' AND ';
                GISdb[DBonTable].ApplyGISFilter(RefFilter + 'AREA=' + QuotedStr(AreaList.Strings[i]));
                WMDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(AreaList.Count) );
                WMDEM.SetPanelText(1,RefFilter);
                if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
                   TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
                   Graph.GraphDraw.GraphLeftLabels.Add(IntToStr(CurrentY-TileList.Count div 2) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA'));
                   for j := 0 to pred(TileList.Count) do begin
                      dec(CurrentY);
                      GISdb[DBonTable].ApplyGISFilter(RefFilter + ' AND AREA=' + QuotedStr(AreaList.Strings[i]) + ' AND DEMIX_TILE=' + QuotedStr(TileList.Strings[j]));
                      GISdb[DBonTable].EmpSource.Enabled := false;

                     if What in [dgArea] then begin
                        for DEM := 0 to pred(DEMsPresent.Count) do begin
                           v[2] := CurrentY;
                           v[1] := GISdb[DBonTable].MyData.FieldSum(DEMsPresent.Strings[DEM] + '_SCR') / GISdb[DBonTable].MyData.FiltRecsInDB;
                           BlockWrite(rfile[succ(DEM)],v,1);
                        end;
                        GISdb[DBonTable].MyData.Next;
                     end
                     else if What in [dgAllValues,dgAllScores] then begin
                        (*
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
                        *)
                     end
                     else if What in [dgPercentBest] then begin
                        (*
                        for i := 1 to 2 do OnePercentage('REF_TYPE=' + QuotedStr(RefDEMType[i]));
                        inc(OnTile);
                        Graph.GraphDraw.GraphLeftLabels.Add('-----------');

                        for i := 1 to MaxLandType do OnePercentage('LAND_TYPE=' + QuotedStr(LandType[i]));
                        inc(OnTile);
                        Graph.GraphDraw.GraphLeftLabels.Add('-----------');
                        for i := 1 to AllCriteria.Count do OnePercentage('CRITERION=' + QuotedStr(AllCriteria.Strings[pred(i)]));
                        *)
                     end
                     else if (What in [dg7Params,dgPick,dgNormalizedDiff]) then begin
                        if  (What in [dgNormalizedDiff]) then begin
                           Max := 0;
                           for DEM := 0 to pred(DEMsPresent.Count) do begin
                              v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                              if v[1] > Max then Max := v[1];
                           end;
                        end
                        else Max := 1;
                        for DEM := 0 to pred(DEMsPresent.Count) do begin
                           v[2] := CurrentY;
                           v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]) / Max;
                           BlockWrite(rfile[succ(DEM)],v,1);
                        end;
                     end
                     else begin
                        for DEM := 0 to pred(DEMsPresent.Count) do begin
                           v[2] := CurrentY;
                           v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                           BlockWrite(rfile[succ(DEM)],v,1);
                        end;
                      end;
                   end;
                   Dec(currentY,InterAreaSkip);
                end;
             end;

            for DEM := 1 to DEMsPresent.Count do begin
               CloseFile(rfile[DEM]);
            end;

            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
            Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
            Graph.Width := 1200;
            Graph.Height := 2400;
            Graph.GraphDraw.BottomMargin := 75;

            if What in [dgArea] then begin
               Graph.GraphDraw.HorizLabel := 'Average Ranking compared to reference ' + DEMType;
               Graph.GraphDraw.MinHorizAxis := 0.8;
               Graph.GraphDraw.MaxHorizAxis := 6.2;
            end;

            if (What in [dgPercentBest]) then begin
               Graph.GraphDraw.HorizLabel := 'Percentage of Criteria Where DEM is Best';
               Graph.GraphDraw.MinHorizAxis := 0;
               Graph.GraphDraw.MaxHorizAxis := 100;
            end;

            if (What in [dgAllValues]) then begin
               if WhatNow in [dgAllValues] then begin
                  Graph.GraphDraw.HorizLabel := 'Criterion Difference';
                  Graph.GraphDraw.MinHorizAxis := 0;
               end;
               Graph.GraphDraw.HorizLabel := LandType + '  ' + DEMType + '  ' + Graph.GraphDraw.HorizLabel;
            end;

            if What in [dgPick,dg7Params,dgNormalizedDiff] then begin
               Graph.GraphDraw.HorizLabel := RefFilter;
               if What in [dgNormalizedDiff] then begin
                  Graph.GraphDraw.MinHorizAxis := 0;
                  Graph.GraphDraw.MaxHorizAxis := 1.1;
               end
               else if Graph.GraphDraw.MinHorizAxis > 0 then Graph.GraphDraw.MinHorizAxis := 0;;
            end
            else if What in [dgAllValues] then begin
               Graph.GraphDraw.HorizLabel := ThisTile + ' Criterion Difference';
               Graph.GraphDraw.MinHorizAxis := 0;
            end
            else if What in [dgAllScores] then begin
               Graph.GraphDraw.HorizLabel := ThisTile + ' Criterion Ranking';
               Graph.GraphDraw.MinHorizAxis := 0.8;
               Graph.GraphDraw.MaxHorizAxis := 6.2;
            end;
            Graph.RedrawDiagram11Click(Nil);
            fName := NextFileNumber(MDTempDir,'graph_','.png');
            SaveImageAsBMP(Graph.Image1,fName);
            BigGraph.Add(fName);
            if (LegendfName = '') then begin
               LegendfName := NextFileNumber(MDTempDir,'legend_','.png');
               bmp := Graph.MakeLegend(Graph.GraphDraw.LegendList,false);
               SaveBitmap(bmp,LegendfName);
               Bmp.free;
            end;
         end {MakeGraphForFilter};

var
   J,k,ColsInBigGraph : integer;
begin {DEMIXwineContestMeanMedianGraph}
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestMeanMedianGraph in, table=' + IntToStr(DBontable) + '  what='  + IntToStr(What)); {$EndIf}

   if (CriteriaUsed = Nil) and (What in [dg7Params,dgNormalizedDiff,dgPick]) then begin
      fName := DEMIXSettingsDir;
      if not GetExistingFileName('criteria to plot','*.txt',fName) then exit;
      CriteriaUsed := tStringList.Create;
      CriteriaUsed.LoadFromFile(fName);
   end;

   if (DEMsPresent = Nil) then begin
      DEMsPresent := tStringList.Create;
      DEMsPresent.LoadFromFile(DEMIXSettingsDir + 'demix_dems_all_six.txt');
   end;

   if (AreaList = Nil) then begin
      AreaList := tStringList.Create;
      AreaList.LoadFromFile(DEMIXSettingsDir + 'demix_areas_sorted_by_lat.txt');
   end;

   if (LandTypePresent = Nil) then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      LandTypePresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   end;

   GISdb[DBonTable].EmpSource.Enabled := false;
   ColsInBigGraph := 3;

   if What in [dgAllValues,dgAllScores] then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      AllTiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
      ThisTile := GetFromList('DEMIX tile to plot',AllTiles);
      FreeAndNil(AllTiles);
   end;

   BigGraph := tstringList.Create;
   LegendfName := '';
   if What in [dgPercentBest] then begin
      MakeGraphForFilter('','','');
   end
   else begin
      for j := 1 to DEMsPresent.Count do begin
         if (What in [dgPick,dg7Params,dgNormalizedDiff]) then begin
            for k := 0 to pred(CriteriaUsed.Count) do MakeGraphForFilter(CriteriaUsed.Strings[k],LandTypePresent[1],DEMsPresent[pred(j)]);
         end
         else if (What in [dgAllValues,dgAllScores]) then begin
            what := dgAllValues; MakeGraphForFilter('',LandTypePresent[1],DEMsPresent[pred(j)]);
            what := dgAllScores; MakeGraphForFilter('',LandTypePresent[1],DEMsPresent[pred(j)]);
         end
         else if (What in [dgArea]) then begin
            MakeGraphForFilter('',LandTypePresent[1],DEMsPresent[pred(j)]);
         end;
      end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click graphs all made'); {$EndIf}


   if (LegendFName <> '') then BigGraph.Add(LegendfName);
   fName := NextFileNumber(MDtempDir,'demix_big_graph_','.png');
   MakeBigBitmap(BigGraph,'',fName,ColsInBigGraph);
   WMDEm.SetPanelText(1,'');
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}
end {DEMIXwineContestMeanMedianGraph};


procedure DEMIXTileSummary(DBonTable : integer);
var
   DEMsPresent,LandTypesPresent,Report,CriteriaPresent : tStringList;
   j,k : integer;
   aLine : shortstring;
   fName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;

   DEMsPresent := tStringList.Create;
   DEMsPresent.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');

   GISdb[DBonTable].EmpSource.Enabled := false;
   LandTypesPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('LAND_TYPE');
   GISdb[DBonTable].EmpSource.Enabled := false;
   CriteriaPresent := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
   {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('Land_Types present=' + IntToStr(LandTypesPresent.Count) + '  Criteria present=' + IntToStr(CriteriaPresent.Count)); {$EndIf}

   Report := tStringList.Create;
   aline := 'REF_TYPE,AREA';
   for j := 0 to pred(LandTypesPresent.Count) do aLine := aLine + ',' + LandTypesPresent.strings[j];
   Report.Add(Aline);

   {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}

   for j := 2 downto 1 do begin
      aline := RefDEMType[j];
      GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefDEMType[j]));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA'));
      GISdb[DBonTable].EmpSource.Enabled := false;
      for k := 0 to pred(LandTypesPresent.Count) do begin
         wmdem.SetPanelText(3,'DEM type=' + RefDEMType[j] + '  land type=' + LandTypesPresent[k]);
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(RefDEMType[j]) + ' AND LAND_TYPE=' + QuotedStr(LandTypesPresent[k]));
         aLine := aline + ',' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB div CriteriaPresent.Count);
      end;
      {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('DEMIXTileSummary ' + aline); {$EndIf}
      Report.Add(aline);
   end;

   DEMsPresent.Free;
   LandTypesPresent.Free;
   CriteriaPresent.Free;
   fName := NextFileNumber(MDTempDir,'demix_tile_summary_','.dbf');
   StringList2CSVtoDB(Report,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   GISdb[DBonTable].ShowStatus;
   wmdem.SetPanelText(3,'');
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXTileSummary out'); {$EndIf}
end {DEMIXTileSummary};



