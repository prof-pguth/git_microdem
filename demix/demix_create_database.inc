{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{    include file for demix_control  }
{____________________________________}


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
   DEM_list_fname : pathStr;
begin
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS in, ' + GISdb[DBonTable].dbName); {$EndIf}
    if (not UseAll) and (not GetFileFromDirectory('DEMIX criterial tolerances','*.dbf',MDDef.DEMIX_criterion_tolerance_fName)) then exit;
    GISdb[DBonTable].AddFieldToDataBase(ftString,'DEM_LOW_SC',48);
    if GISdb[DBonTable].MyData.GetFieldLength('DEM_LOW_SC') < 48 then GISdb[DBonTable].MyData.TrimField('DEM_LOW_SC',48);
    if (not GetFileFromDirectory('DEMIX DEMs','*.txt',DEM_list_fname)) then exit;

    if DoTrio then begin
       GISdb[DBonTable].AddFieldToDataBase(ftString,'COP_FAB',8);
       GISdb[DBonTable].AddFieldToDataBase(ftString,'ALOS_FAB',8);
    end;

    DEMsRanked := tStringList.Create;
    DEMsRanked.LoadFromFile(DEM_list_fname);

    for DEM := 0 to pred(DEMsRanked.Count) do begin
       GISdb[DBonTable].AddFieldToDataBase(ftFloat,DEMsRanked.Strings[DEM] + '_SCR',6,2);
    end;
    GISdb[DBonTable].AddFieldToDataBase(ftFloat,'TOLERANCE',9,6);

    i := 0;
    {if UseAll or GetFromListZeroBased('DEMs to use in ranking',i,DEMsRanked,true,true) then} begin
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
                   aLine := aLine + RealToString(abs(GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsRanked.Strings[DEM])),-12,-6) + ',';
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


procedure DEMIXtile_inventory(DBonTable : integer);
var
   TileList,Findings,DEMs : tStringList;
   TStr : shortstring;
   fName : PathStr;
   i : Integer;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
   Findings := tStringList.Create;
   Findings.Add('COUNTRY,AREA,DEMIX_TILE,DEMS');
   StartProgress('Inventory tiles');
   for i := 0 to pred(TileList.Count) do begin
      wmdem.SetPanelText(3,'Tile ' + IntToStr(i) + '/' + IntToStr(TileList.Count));
      GISdb[DBonTable].EmpSource.Enabled := false;
      if (i mod 10 = 0) then UpdateProgressBar(i/TileList.Count);
      GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(TileList.Strings[i]),false);
      DEMs := GISdb[DBonTable].MyData.UniqueEntriesInDB('REF_TYPE');
      TStr := DEMs[0];
      if (DEMs.Count > 1) then TStr := TStr + ' + ' +  DEMs[1];
      Findings.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('COUNTRY') + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA') + ',' +  TileList.Strings[i] + ',' + TStr);
   end;
   GISdb[DBonTable].ClearGISFilter;
   fName := NextFileNumber(MDTempDir,'DEMIX_tile_inventory','.dbf');
   StringList2CSVtoDB(Findings,fName);
   GISdb[DBonTable].ShowStatus;
   wmdem.SetPanelText(3,'');
end;




procedure ModeOfDifferenceDistributions;
var
   FilesWanted,Modes,TileList : tStringList;
   fName,fName2 : PathStr;
   i,j,NPts,db,atile : integer;
   BinSize,Mode : float32;
   Tile,param,Ref,aLine,Area : shortstring;
   Values : ^Petmath.bfarray32;
   MomentVar : tMomentVar;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ModeOfDifferenceDistributions in'); {$EndIf}
   try
      if not Petmar.GetExistingFileName('Ordered list of tiles','*.txt',fName) then exit;
      GetDEMIXpaths;
      Modes := tStringList.Create;
      aLine := 'AREA,TILE,REF_TYPE,CRITERION';
      for I := 1 to NumDEMIXDEM do begin
         aline := aline + ',' + DEMIXshort[i] + '_MODE' + ',' + DEMIXshort[i] + '_MEAN' + ',' + DEMIXshort[i] + '_MEDN' + ',' + DEMIXshort[i] + '_STD';
      end;
      Modes.Add(aLine);
      TileList := tStringList.Create;
      TileList.LoadFromFile(fName);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('tiles=' + IntToStr(TileList.Count)); {$EndIf}
      FilesWanted := tStringList.Create;
      FindMatchingFiles(DEMIX_diff_dist,'*.z',FilesWanted,0);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('z files=' + IntToStr(FilesWanted.Count)); {$EndIf}

      StartProgress('Modes');
      for atile := 0 to pred(TileList.Count) do begin
         UpdateProgressBar(aTile/TileList.Count);
         for i := 0 to pred(FilesWanted.Count) do begin
            fName := upperCase(FilesWanted.Strings[i]);
            if StrUtils.AnsiContainsText(fname,TileList[aTile]) then begin
               if StrUtils.AnsiContainsText(fname,'COP') then begin
                  Area := BeforeSpecifiedString(ExtractFileName(FilesWanted.Strings[i]),'_COP');

                  Tile := ExtractFileName(fName);
                  Tile := AfterSpecifiedString(Tile,'COP_');
                  Tile := Copy(Tile,1,9);
                  if StrUtils.AnsiContainsText(fname,'DSM') then Ref := 'DSM' else Ref := 'DTM';

                  if StrUtils.AnsiContainsText(fname,'ELEV') then begin
                     BinSize := 0.10;
                     Param := 'elvd_mode';
                  end;

                  if StrUtils.AnsiContainsText(fname, 'SLOPE') then begin
                     BinSize := 0.25;
                     Param := 'slpd_mode';
                  end;
                  if StrUtils.AnsiContainsText(fname, 'RUFF') then begin
                     BinSize := 0.15;
                     Param := 'rufd_mode';
                  end;
                  aLine := Area + ',' + Tile + ',' + Ref + ',' + Param ;

                  for j := 1 to NumDEMIXDEM do begin
                     fName2 := StringReplace(fName,'COP',DEMIXDEMTypeName[j],[rfReplaceAll, rfIgnoreCase]);
                     if FileExists(fName2) then begin
                        New(Values);
                        LoadBFarray32(fName2,Values^,npts);
                        Mode := PetMath.Mode(Values^,npts,binsize);
                        InitializeMomentVar(MomentVar);
                        MomentVar.NPts := Npts;
                        moment(Values^,MomentVar,msAll);
                        Dispose(Values);
                     end
                     else Mode := -9999;
                     aLine := aLine + ',' + RealToString(Mode,-8,-2) + ',' + RealToString(MomentVar.Mean,-8,-2) + ',' + RealToString(MomentVar.Median,-8,-2) + ',' + RealToString(MomentVar.Sdev,-8,-2);
                  end;
                  Modes.Add(aline);
               end;
            end;
         end;
      end;
      fName := NextFileNumber(MDTempDir,'distrib_diff_modes_','.dbf');
      db := StringList2CSVtoDB(Modes,fName);
   finally
      TileList.Free;
      FilesWanted.Free;
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ModeOfDifferenceDistributions out'); {$EndIf}
end;


procedure CreateDEMIX_GIS_database(AreaName : shortstring = '');
var
   fName : PathStr;
   i     : byte;
   db : integer;
   FilesWanted : tStringList;
   Tiles,ElevDiff,SlopeDiff,RuffDiff,TransposeNames,ErrorLog : tStringList;


      function TransposeDEMIXcriteria(DBonTable : integer) : PathStr;
      const
         MaxDEMs = 10;
         MaxCriteria = 25;
      var
         Headers,DEMs,Criteria,Output,Tiles : tStringList;
         Line : ANSIString;
         CriteriaFile : PathStr;
         value,ThisDEM,Missing : shortstring;
         Cycles,Done,
         i,j,  Tile, Ref, aLandType,DEM,Criterion : Integer;
         Values : array[0..MaxDEMs,0..MaxCriteria] of shortstring;
      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile(''); WriteLineToDebugFile('TransposeDEMIXcriteria in, ' + GISdb[DBonTable].dbName); {$EndIf}
         Headers := tStringList.Create;
         Headers.LoadFromFile(DEMIXSettingsDir + 'demix_headers.txt');
         DEMs := tStringList.Create;
         DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');

         CriteriaFile := DEMIXSettingsDir + 'demix_criteria_with_signed.txt';
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
         Line := Line + 'TOLERANCE,DEM_LOW_SC,COP_ALOS,';
         for i := 0 to pred(DEMs.Count) do begin
            Line := Line + DEMs.Strings[i] + '_SCR,';
         end;
         Line := Line + 'REC_ID,CRITERION,';
         for i := 0 to pred(DEMs.Count) do begin
            Line := Line + DEMs.Strings[i];
            if i < pred(DEMs.Count) then Line := Line + ',';
         end;
         Output.Add(Line);

         GISdb[DBonTable].ClearGISFilter;
         GISdb[DBonTable].EmpSource.Enabled := false;
         Tiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria tiles=' + IntToStr(Tiles.Count)); {$EndIf}

         Cycles := Tiles.Count * 2 * NumLandTypes;
         Done := 0;
         StartProgress('Transposing ' + GISdb[DBontable].DBName);
         for Tile := 0 to Pred(Tiles.Count) do begin
            wmdem.SetPanelText(2,IntToStr(succ(Tile)) + '/' + IntToStr(Tiles.Count));
            wmdem.SetPanelText(3,Tiles.Strings[Tile]);
            UpdateProgressBar(Done/Cycles);
            for Ref := 1 to 2 do begin
               Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefDEMType[Ref]);
               GISdb[DBonTable].ApplyGISFilter(Line);
               GISdb[DBonTable].EmpSource.Enabled := false;
               if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin  //there are DSMs or DTMs for this tile
                  for aLandType := 1 to NumLandTypes do begin
                     Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefDEMType[Ref])  + ' AND LAND_TYPE=' + QuotedStr(LandTypes[aLandType]);
                     GISdb[DBonTable].ApplyGISFilter(Line);
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     if (GISdb[DBonTable].MyData.FiltRecsInDB = DEMs.Count) then begin
                        for i := 0 to MaxDems do
                           for j := 0 to MaxCriteria do
                               Values[i,j] := '-9999';

                        while not GISdb[DBonTable].MyData.eof do begin
                           GISdb[DBonTable].EmpSource.Enabled := false;
                           ThisDEM := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));

                           //clean up problem entries; no time to track down where these crept in
                           ThisDEM := StringReplace(ThisDEM,'_2','',[rfReplaceAll, rfIgnoreCase]);     //unclear where this is from
                           ThisDEM := StringReplace(ThisDEM,'_9','',[rfReplaceAll, rfIgnoreCase]);     //unclear where this is from
                           ThisDEM := StringReplace(ThisDEM,'_2022','',[rfReplaceAll, rfIgnoreCase]);  //poor choice for the name of an area
                           if StrUtils.AnsiContainsText(ThisDEM,'_') then begin
                              i := Length(ThisDEM);
                              while ThisDEM[i] <> '_' do dec(i);
                              Delete(ThisDEM,1,i);
                           end;

                           DEM := DEMs.IndexOf(ThisDEM);
                           if (DEM = -1) then begin
                              ErrorLog.Add(Line + '  -1 index for ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));
                           end
                           else begin
                              for Criterion := 0 to pred(Criteria.Count) do begin
                                 value := GISdb[DBonTable].MyData.GetFieldByNameAsString(Criteria[Criterion]);
                                 if (value <> '') then Values[DEM,Criterion] := value;
                              end;
                           end;
                           GISdb[DBonTable].MyData.Next;
                        end;

                        for Criterion := 0 to pred(Criteria.Count) do begin
                           Line := '';
                           for i := 0 to pred(Headers.Count) do Line := Line + GISdb[DBonTable].MyData.GetFieldByNameAsString(Headers[i]) + ',';
                           Line := Line + '99.99,Not_yet_done,Not4,';
                           for DEM := 0 to pred(DEMs.Count) do Line := Line + '9.9,';
                           Line := Line + '123456,' + Criteria[Criterion] + ',';

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



   procedure MergeAndTranspose(var Diffs : tStringList; fName : PathStr);
   begin
      DeleteFileIfExists(fName);
      DeleteFileIfExists(ChangeFileExt(fName,'.dbf'));

      if (Diffs.Count > 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeAndTranspose ' + fName + ' with regions=' + IntToStr(Diffs.Count)); {$EndIf}
         MergeCSVFiles(Diffs,fName);
         db := OpenMultipleDataBases('',fName);
         GISdb[db].AddSequentialIndex('REC_ID',false);
         fName := TransposeDEMIXcriteria(DB);
         TransposeNames.Add(fName);
         CloseAndNilNumberedDB(db);
      end
      else begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No MergeAndTranspose ' + fName + '  with regions=' + IntToStr(Diffs.Count)); {$EndIf}
         Diffs.Free;
      end;
   end;

var
   Areas : tStringList;
   KeepThisOne : boolean;
   j : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV in'); {$EndIf}
   GetDEMIXpaths;
   ShowHourglassCursor;
   Areas := DEMIX_AreasWanted(AreaName);
   Areas.Sorted := false;
   for i := pred(Areas.Count) downto 0 do begin
      fName := Areas.Strings[i];
      if StrUtils.AnsiContainsText(UpperCase(fname),'_DTM') then Areas.Strings[i] := Copy(fName,1,length(fName)-4);
      if StrUtils.AnsiContainsText(UpperCase(fname),'_DSM') then Areas.Delete(i);
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV picked areas=' + IntToStr(Areas.Count)); {$EndIf}
   FilesWanted := Nil;
   FindMatchingFiles(DEMIXresultsDir,'*.csv',FilesWanted,0);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CSV total results=' + IntToStr(FilesWanted.Count)); {$EndIf}
   for j := pred(FilesWanted.Count) downto 0 do begin
      KeepThisOne := false;
      for i := pred(Areas.Count) downto 0 do begin
         if StrUtils.AnsiContainsText(FilesWanted.Strings[j],Areas.Strings[i]) then begin
            KeepThisOne := true;
         end;
      end;
      if Not KeepThisOne then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Not Using: ' + FilesWanted.Strings[j]); {$EndIf}
         FilesWanted.Delete(j);
      end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CSV in these areas=' + IntToStr(FilesWanted.Count)); {$EndIf}

   try
      Tiles := tStringList.Create;
      ElevDiff := tStringList.Create;
      SlopeDiff := tStringList.Create;
      RuffDiff := tStringList.Create;
      TransposeNames := tStringList.Create;
      ErrorLog := tStringList.Create;
      for i := 0 to pred(FilesWanted.Count) do begin
         fName := uppercase(FilesWanted.Strings[i]);
         if StrUtils.AnsiContainsText(fname,'DEMIX_TILES_USED') then Tiles.Add(fName);
         if StrUtils.AnsiContainsText(fname,'ELEV_DIFF_STATS') then ElevDiff.Add(fName);
         if StrUtils.AnsiContainsText(fname,'SLOPE_DIFF_STATS') then SlopeDiff.Add(fName);
         if StrUtils.AnsiContainsText(fname,'RUFF_DIFF_STATS') then RuffDiff.Add(fName);
      end;
      if (Tiles.Count > 1) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Tiles=' + IntToStr(Tiles.Count)); {$EndIf}
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Elev diff=' + IntToStr(ElevDiff.Count)); {$EndIf}
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Slope diff=' + IntToStr(SlopeDiff.Count)); {$EndIf}
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Ruff diff=' + IntToStr(RuffDiff.Count)); {$EndIf}
         fName := ExtractFilePath(fName) + 'DEMIX_TILES_USED_SUMMARY.csv';
         fName := StringReplace(fName,'_dtm','',[rfReplaceAll, rfIgnoreCase]);
         MergeCSVFiles(Tiles,fName);
      end
      else Tiles.Free;

      DeleteFileIfExists(DEMIX_data_base + 'latest_demix_database.dbf');
      fName := DEMIX_data_base + 'latest_demix_database.csv';

      wmdem.SetPanelText(1,'1/3  Elev');
      MergeAndTranspose(ElevDiff,DEMIX_data_base + 'elev_merge_difference_ref_dem.csv');
      wmdem.SetPanelText(1,'2/3  Slope');
      MergeAndTranspose(SlopeDiff,DEMIX_data_base + 'slope_merge_difference_ref_dem.csv');
      wmdem.SetPanelText(1,'3/3  Ruff');
      MergeAndTranspose(RuffDiff,DEMIX_data_base + 'ruff_merge_difference_ref_dem.csv');

      MergeCSVFiles(TransposeNames,fName);
      db := OpenMultipleDataBases('',fName);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV merged and db opened'); {$EndIf}
      wmdem.ClearStatusBarPanelText;
      AddCountryToDB(DB);
      RankDEMS(DB,true);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV DEMs ranked'); {$EndIf}
      //CloseAndNilNumberedDB(db);
   finally
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX Create Database Problems');
      CloseAllDatabases;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV out, created ' + fName); {$EndIf}
end;


procedure ComputeDEMIX_tile_stats(AreaName : shortstring = '');
//a number of experimental options are disabled, and would require changes to the code to get working again
const
   MaxRefStore = 100;
var
   MomentVar,FlatMomentVar,SteepMomentVar,GentleMomentVar,CliffMomentVar,ForestMomentVar,UrbanMomentVar,BarrenMomentVar : tMomentVar;
   SlopeMomentVar,ElevMomentVar : array[1..MaxRefStore] of tMomentVar;
   NumPits,NumPeaks : array[1..MaxRefStore] of integer;
   DEMIXtileDB,LandCoverGrid : integer;
   bbgrid : tGridLimits;

   //IceSatFName,GeodeticFName,

   cfName,fName : PathStr;
   TestAreaName,DEMIXtile,LandTypeMask,TileHeader : shortstring;
   LatCent,LongCent,
   GridFull,Lat,Long : float64;
   zRef : float32;
   TileStats,
   ElevDiffStats,
   RufDiffStats,
   SlopeDiffStats  : tStringList;
   zs,zsSteep,ZSFlat,zsGentle,zsCliff,zsForest,zsUrban,zsBarren : ^bfarray32;
   SlopeAsp,SlopeAspectTest : tSlopeAspectRec;
   ErrorLog : tStringList;
   ElevFiles,LegendFiles : tStringList;

   {$IfDef ExperimentalCriteria} {$I experiment_demix_criteria.inc}  {$EndIf}

         procedure InitializeStringLists;
         const
            AreaString = ',DEM,REF_TYPE,GRID_FULL,LAND_TYPE,LANDTYP_PC';
            SlopeDiffStatsString = ',SLPD_MIN,SLPD_Max,SLPD_Mean,SLPD_AVD,SLPD_STD,SLPD_MED,SLPD_RMSE,SLPD_MAE,SLPD_LE90,SLPD_N';
            ElevDiffStatsString = ',ELVD_MIN,ELVD_Max,ELVD_Mean,ELVD_AVD,ELVD_STD,ELVD_MED,ELVD_RMSE,ELVD_MAE,ELVD_LE90,ELVD_N';
            RufDiffStatsString = ',RUFD_MIN,RUFD_Max,RUFD_Mean,RUFD_AVD,RUFD_STD,RUFD_MED,RUFD_RMSE,RUFD_MAE,RUFD_LE90,RUFD_N';

            ElevMomentsString = 'ELEV_MIN,ELEV_Max,ELEV_Mean,ELEV_ADEV,ELEV_STD,ELEV_SKEW,ELEV_KURT,ELEV_MED,N';
            SlopeMomentsString = 'SLP_MIN,SLP_Max,SLP_Mean,SLP_ADEV,SLP_STDV,SLP_SKEW,SLP_KURT,SLP_MED,SLP_N';

            SlopeMomentsDiffString = 'SDIFF_MIN,SDIFF_Max,SDIFF_Mean,SDIFF_STD,SDIFF_SKEW,SDIFF_KURT,SDIFF_MED,SDIFF_N';
            ElevMomentsDiffString = 'EDIFF_MIN,EDIFF_Max,EDIFF_Mean,EDIFF_STD,EDIFF_SKEW,EDIFF_KURT,EDIFF_MED,EDIFF_N';

            TileStatsString = 'AREA,DEMIX_TILE,LAT,LONG,AVG_ELEV,AVG_SLOPE,AVG_ROUGH,RELIEF,FOREST_PC,URBAN_PC,BARREN_PC';
         begin
            TileStats := tStringList.Create;
            TileStats.Add(TileStatsString);

            ElevDiffStats := tStringList.Create;
            ElevDiffStats.Add(TileStatsString + AreaString + ElevDiffStatsString);

            RufDiffStats := tStringList.Create;
            RufDiffStats.Add(TileStatsString + AreaString + RufDiffStatsString);

            SlopeDiffStats := tStringList.Create;
            SlopeDiffStats.Add(TileStatsString + AreaString + SlopeDiffStatsString);

            {$IfDef ExperimentalCriteria} CreateExperimentalStringLists; {$EndIf}

            {$IfDef RecordDEMIXFull} writeLineToDebugFile('InitializeStringLists out, string lists created'); {$EndIf}
         end;


         function LineHeader(DEM : integer; RefType : shortstring) : shortstring;
         var
            t1 : shortstring;
         begin
             if (DEM = 0) then t1 := 'xxx' else t1 := DEMGlb[DEM].AreaName;
             Result := t1 + ',' + RefType + ',' + RealToString(GridFull,-8,2) + ',' + LandTypeMask + ',';
         end;


         procedure SortListWithHeader(var sl : tStringList);
         var
            Header : ANSIString;
         begin
            Header := sl.Strings[0];
            sl.Delete(0);
            sl.Duplicates := dupIgnore;
            sl.Sort;
            sl.Insert(0,Header);
         end;

         procedure FinalizeStringLists(FinalSaveResults : boolean = true);

               procedure ProcessStringList(fName : PathStr; var sl : tStringList; DoStats : boolean = false);
               begin
                  {$IfDef RecordFullDEMIX} writeLineToDebugFile(fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                  if (sl.count > 1) then begin
                     fName := DEMIXresultsDir + TestAreaName + fname + '.csv';
                     sl.SaveToFile(fName);
                  end
                  else fName := '';
                  sl.Destroy;
               end;

         begin
            {$IfDef RecordFullDEMIX} writeLineToDebugFile('DEMIX start string list processing'); {$EndIf}
            ProcessStringList('_DEMIX_tiles_used',TileStats);
            ProcessStringList('_elev_diff_stats',ElevDiffStats,true);
            ProcessStringList('_ruff_diff_stats',RufDiffStats,true);
            ProcessStringList('_slope_diff_stats',SlopeDiffStats,true);

            {$IfDef ExperimentalCriteria} ProcessExperimentalStringLists; {$EndIf}
         end;



   procedure ProcessDEMIXtestarea;


         function GridBoundingBox(DEM,RefDEM : integer; Clip : boolean = false) : tGridLimits;
         var
            bb : sfBoundBox;
         begin
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;

            if Clip and (RefDEM <> 0) then begin
               if bb.xMin < DEMGlb[RefDEM].DEMBoundBoxGeo.xmin then bb.xMin := DEMGlb[RefDEM].DEMBoundBoxGeo.xmin;
               if bb.xMax > DEMGlb[RefDEM].DEMBoundBoxGeo.xmax then bb.xMax := DEMGlb[RefDEM].DEMBoundBoxGeo.xmax;
               if bb.yMin < DEMGlb[RefDEM].DEMBoundBoxGeo.ymin then bb.yMin := DEMGlb[RefDEM].DEMBoundBoxGeo.ymin;
               if bb.yMax > DEMGlb[RefDEM].DEMBoundBoxGeo.ymax then bb.yMax := DEMGlb[RefDEM].DEMBoundBoxGeo.ymax;
            end;

            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,Result.xgridlow,Result.ygridlow);
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,Result.xgridhigh,Result.ygridhigh);
         end;

         procedure WriteTileDetails(bb: tGridLimits);
         var
            cols,rows : integer;
         begin
             cols := succ(bbgrid.xgridhigh-bbgrid.xgridLow);
             rows := succ(bbgrid.ygridhigh-bbgrid.ygridLow);
             WriteLineToDebugFile(DEMIXTile + RealToString(GridFull,8,2) + '%  ' + GridLimitstoString(bbgrid) + ' cols=' + IntToStr(cols) +  ' rows=' + IntToStr(rows) + ' points=' + IntToStr(cols*rows));
         end;

         function UseThisTile : boolean;
         var
            bb : sfBoundBox;
         begin
            GridFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
            Result := GridFull >= MDDef.DEMIX_Full;
            DEMIXtile := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            LatCent := 0.5 * (bb.ymax + bb.ymin);
            LongCent := 0.5 * (bb.xmax + bb.xmin);
            {$IfDef RecordUseTile} if not Result then WriteLineToDebugFile('Not doing tile=' + DEMIXTile + '  fill=' + IntToStr(round(GridFull))); {$EndIf}
         end;

         function MomentStatsString(MomentVar : tMomentVar) : shortstring;
         begin
            Result := RealToString(MomentVar.MinZ,-8,2) + ',' + RealToString(MomentVar.MaxZ,-8,2) + ',' + RealToString(MomentVar.Mean,-8,2) + ',' +
                RealToString(MomentVar.avg_dev,-8,2) + ',' + RealToString(MomentVar.sdev,-8,2) + ',' + RealToString(MomentVar.median,-8,2) + ',' + RealToString(MomentVar.rmse,-8,2)  + ',' +
                RealToString(MomentVar.mae,-8,2)  + ',' + RealToString(MomentVar.LE90,-12,-2) + ',' + IntToStr(MomentVar.NPts);
         end;


         function MomentDifferenceString(aMomentVar,RefMomentVar : tMomentVar) : shortstring;
         begin
             Result := RealToString(aMomentVar.MinZ-RefMomentVar.minZ,-8,2) + ',' + RealToString(aMomentVar.MaxZ-RefMomentVar.MaxZ,-8,2) + ',' +
                            RealToString(aMomentVar.Mean-RefMomentVar.mean,-8,2) + ',' + RealToString(aMomentVar.sdev-RefMomentVar.sdev,-8,2) + ',' +
                            RealToString(aMomentVar.skew-RefMomentVar.skew,-8,2) + ',' + RealToString(aMomentVar.curt-RefMomentVar.Curt,-8,2) + ',' +
                            RealToString(aMomentVar.median-RefMomentVar.median,-8,2) + ',' + IntToStr(aMomentVar.NPts);
         end;


            procedure ZeroMomentVar;
            begin
               MomentVar.Npts := 0;
               FlatMomentVar.Npts := 0;
               GentleMomentVar.Npts := 0;
               SteepMomentVar.Npts := 0;
               CliffMomentVar.Npts := 0;

               UrbanMomentVar.Npts := 0;
               ForestMomentVar.Npts := 0;
               BarrenMomentVar.Npts := 0;
            end;


            procedure LandCover(Lat,Long : float32; Difference : float32);
            //this is hard coded for a particular land cover data set, LC100 from Copernicus
            var
               z : float32;
            begin
               if (LandCoverGrid <> 0) and DEMGlb[LandCoverGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
                  if round(z) in [111..126] then begin
                     zsForest^[ForestMomentVar.NPts] := Difference;
                     inc(ForestMomentVar.Npts);
                  end
                  else if round(z) in [30,60,70] then begin
                     zsBarren^[BarrenMomentVar.NPts] := Difference;
                     inc(BarrenMomentVar.Npts);
                  end
                  else if round(z) in [50] then begin
                     zsUrban^[UrbanMomentVar.NPts] := Difference;
                     inc(UrbanMomentVar.Npts);
                  end;
               end;
            end;


            procedure LandTypeDiff(Difference : float32);
            begin
               zs^[MomentVar.NPts] := Difference;
               inc(MomentVar.Npts);
               if (SlopeAsp.SlopePercent < MDDef.SlopeFlatBoundary) then begin
                  zsflat^[FlatMomentVar.NPts] := Difference;
                  inc(FlatMomentVar.Npts);
               end
               else if (SlopeAsp.SlopePercent < MDDef.SlopeGentleBoundary) then begin
                  zsGentle^[GentleMomentVar.NPts] := Difference;
                  inc(GentleMomentVar.Npts);
               end
               else if (SlopeAsp.SlopePercent < MDDef.SlopeSteepBoundary) then begin
                  zsSteep^[SteepMomentVar.NPts] := Difference;
                  inc(SteepMomentVar.Npts);
               end
               else begin
                  zsCliff^[CliffMomentVar.NPts] := Difference;
                  inc(CliffMomentVar.Npts);
               end;
            end;



           function WriteDifferenceResults(DEM,REFDEM : integer; RefType : shortstring; var WhichStats : tStringList) : integer;

                     procedure WriteDifferenceResult(ltMask : shortString; var sl : tStringList; DEM,RefDEM : integer; var thezs : bfarray32; var theMoments : tMomentVar; Percent : float32);
                     var
                        i : integer;
                        aLine : shortstring;
                     begin
                        if (theMoments.NPts > MDDef.LandTypePointsNeeded) then begin
                           LandTypeMask := ltMask;
                           moment(thezs,theMoments,msAll);
                           for i := 0 to pred(theMoments.NPts) do thezs[i] := abs(thezs[i]);
                           theMoments.LE90 := Percentile(90,thezs,theMoments.NPts,false);
                           aline := TileHeader + LineHeader(DEM,RefType)  + RealToString(Percent,-12,-2) + ',' + MomentStatsString(theMoments);
                           sl.Add(aLine);
                           inc(Result);
                        end
                        else begin
                           {$If Defined(RecordGridCompare)} WriteLineToDebugFile('WriteDifferenceResult failed for ' + LandTypeMask + ' theMoments.NPts=' + IntToStr(theMoments.NPts) ); {$EndIf}
                        end;
                     end;

           begin
               Result := 0;
               WriteDifferenceResult('ALL',WhichStats,DEM,RefDEM,zs^,MomentVar,100);
               WriteDifferenceResult('FLAT',WhichStats,DEM,RefDEM,zsFlat^,FlatMomentVar,(100 * FlatMomentVar.NPts/MomentVar.NPts));
               WriteDifferenceResult('GENTLE',WhichStats,DEM,RefDEM,zsGentle^,GentleMomentVar,(100 * GentleMomentVar.NPts/MomentVar.NPts));
               WriteDifferenceResult('STEEP',WhichStats,DEM,RefDEM,zsSteep^,SteepMomentVar,(100 * SteepMomentVar.NPts/MomentVar.NPts));
               WriteDifferenceResult('CLIFF',WhichStats,DEM,RefDEM,zsCliff^,CliffMomentVar,(100 * CliffMomentVar.NPts/MomentVar.NPts));

               if (LandCoverGrid <> 0) then begin
                  WriteDifferenceResult('FOREST',WhichStats,DEM,RefDEM,zsForest^,ForestMomentVar,(100 *ForestMomentVar.NPts/MomentVar.NPts));
                  WriteDifferenceResult('URBAN',WhichStats,DEM,RefDEM,zsUrban^,UrbanMomentVar,(100 * UrbanMomentVar.NPts/MomentVar.NPts));
                  WriteDifferenceResult('BARREN',WhichStats,DEM,RefDEM,zsBarren^,BarrenMomentVar,(100 * BarrenMomentVar.NPts/MomentVar.NPts));
               end;
               LandTypeMask := 'ALL';
           end;


           procedure LogError(TStr : shortstring);
           begin
              {$If Defined(RecordFullDEMIX) or Defined(RecordGridCompare)} writeLineToDebugFile(TStr); {$EndIf}
              ErrorLog.Add(TStr);
           end;

           procedure CompareDifferencesToReferenceDEM(DEM,RefDEM : integer; RefType : shortstring);
           //do full DEM pixel by pixel comparison to reference DEM
           var
              NumRuff,NumSlope,NumElev,
              xg,yg : integer;
              TStr : shortstring;
              z,Difference : float32;

                  procedure DoElevations;
                  var
                     Col,Row : integer;
                  begin
                     NumElev := 0;
                     ZeroMomentVar;
                     {$IfDef RecordFullDEMIX} WriteTileDetails(bbgrid); {$EndIf}
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[DEM].GetElevMetersOnGrid(col,row,z) then begin
                              DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                              DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                              if DEMGlb[RefDEM].GetElevMetersOnGrid(xg,yg,zref) then begin
                                 Difference := z-zref;
                                 if DEMGlb[RefDEM].GetSlopeAndAspect(xg,yg,SlopeAsp) then begin
                                    LandTypeDiff(Difference);
                                 end;
                                 LandCover(Lat,long,Difference);
                              end;
                           end;
                        end;
                     end;

                     if (MomentVar.NPts > 0) then begin
                        if ElevDiffHists then begin
                           fName := DEMIX_diff_dist + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_elev_to_' + RefType  + '.z';
                           ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zs^,fName));
                           LegendFiles.Add(ExtractFileNameNoExt(fName));
                        end;
                        NumElev := WriteDifferenceResults(DEM,REFDEM,RefType,ElevDiffStats);
                     end
                     else begin
                        LogError('No points for elevation difference ' + DEMGLB[DEM].AreaName);
                     end;
                  end;


                  procedure DoSlopes;
                  var
                     Col,Row : integer;
                  begin
                     NumSlope := 0;
                     ZeroMomentVar;
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[DEM].GetSlopeAndAspect(col,row,SlopeAspectTest) then begin
                              DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                              if DEMGlb[RefDEM].GetSlopeAndAspectFromLatLong(Lat,Long,SlopeAsp) then begin
                                 Difference := SlopeAspectTest.SlopePercent - SlopeAsp.SlopePercent;
                                 LandTypeDiff(Difference);
                                 LandCover(Lat,Long,Difference);
                              end;
                           end;
                        end;
                     end;

                     if (MomentVar.NPts > 1) then begin
                        if ElevDiffHists then begin
                           fName := DEMIX_diff_dist + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_slope_to_' + RefType + '.z';
                           SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                        end;
                        NumSlope := WriteDifferenceResults(DEM,REFDEM,RefType,SlopeDiffStats);
                     end
                     else begin
                        LogError('No points for slope differences ' + DEMGLB[DEM].AreaName);
                     end;
                  end;

                  procedure DoRoughness;
                  var
                     Col,Row : integer;
                     Ruff1,Ruff2 : float32;
                  begin
                     NumRuff := 0;
                     ZeroMomentVar;
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[DEM].RoughnessFromSlopeSTD(Col,Row,MDDef.RoughnessBox,Ruff1) then begin
                              DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                              DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                              if DEMGlb[RefDEM].RoughnessFromSlopeSTD(xg,yg,MDDef.RoughnessBox,Ruff2) then begin
                                 Difference := Ruff1 - Ruff2;
                                 if DEMGlb[RefDEM].GetSlopeAndAspect(xg,yg,SlopeAsp) then begin
                                    LandTypeDiff(Difference);
                                 end;
                                 LandCover(Lat,long,Difference);
                              end;
                           end;
                        end;
                     end;
                     if (MomentVar.NPts > 1) then begin
                        if ElevDiffHists then begin
                           fName := DEMIX_diff_dist + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_ruff_to_' + RefType + '.z';
                           SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                        end;
                        NumRuff := WriteDifferenceResults(DEM,REFDEM,RefType,RufDiffStats);
                     end
                     else begin
                        LogError('No points for roughness difference ' + DEMGLB[DEM].AreaName);
                     end;
                  end;


            begin
               if (RefDEM = 0) then begin
                  //this is probably for DSM when we don't have one, so we won't log
               end
               else begin
                  {$If Defined(RecordFullDEMIX) or Defined(RecordGridCompare)} WriteLineToDebugFile('Comparison to reference=' + DEMGLB[RefDEM].AreaName + '  test=' + DEMGLB[DEM].AreaName); {$EndIf}
                  bbgrid := GridBoundingBox(DEM,RefDEM,true);
                  DoElevations;
                  DoSlopes;
                  DoRoughness;
                  TStr := TestAreaName + ' ' + DEMIXTile + ' ' + RefType + ' elev=' + IntToStr(NumElev) +  ' slope=' + IntToStr(NumSlope) + ' ruff=' + IntToStr(NumRuff)  + ' total=' + IntToStr(NumElev+NumSlope+NumRuff);
                  wmdem.SetPanelText(3,TStr);
                  {$If Defined(RecordTileProcessing) or Defined(RecordFullDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
               end;
            end;


         procedure GetTileStatistics(RefDEM : integer);
         var
           ElevMomentVar,SlopeMomentVar,RoughMomentVar : tMomentVar;
           Col,Row,NPts : integer;
           ForestPC,UrbanPC,BarrenPC : float32;
         begin
            if UseThisTile then begin
               bbgrid := GridBoundingBox(RefDEM,0);
               ZeroMomentVar;
               ElevMomentVar.Npts := 0;
               SlopeMomentVar.Npts := 0;
               RoughMomentVar.Npts := 0;
               NPts := 0;
               DEMGlb[RefDEM].ElevationMomentsWithArray(bbgrid,ElevMomentVar,zs^);
               DEMGlb[RefDEM].SlopeMomentsWithArray(bbgrid,SlopeMomentVar,zs^);
               DEMGlb[RefDEM].GetRoughnessInLongArray(bbgrid,RoughMomentVar.NPts,zs^);
               Moment(zs^,RoughMomentVar,msAll);

               if (LandCoverGrid <> 0) then begin
                  for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                     for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                        DEMGlb[RefDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                        LandCover(Lat,long,Difference);
                        inc(NPts);
                     end;
                  end;
                  ForestPC := 100 * ForestMomentVar.Npts / NPts;
                  UrbanPC := 100 * UrbanMomentVar.Npts / NPts;
                  BarrenPC := 100 * BarrenMomentVar.Npts / NPts;
               end
               else begin
                  ForestPC := -9999;
                  UrbanPC := -9999;
                  BarrenPC := -9999;
               end;

               TileHeader := ShortTestAreaName(TestAreaName) + ',' + DEMIXTile + ',' + RealToString(LatCent,-12,-6) + ',' + RealToString(LongCent,-12,-6) + ',' + RealToString(ElevMomentVar.Mean,-12,-2)   + ','  +
                  RealToString(SlopeMomentVar.Mean,-12,-2)  + ',' + RealToString(RoughMomentVar.Mean,-12,-2)  + ',' +
                  RealToString(ElevMomentVar.Maxz - ElevMomentVar.Minz,-12,-2) + ',' + RealToString(ForestPC,-12,-2)  + ',' + RealToString(UrbanPC,-12,-2) + ',' + RealToString(BarrenPC,-12,-2);
               TileStats.Add(TileHeader);
               TileHeader := TileHeader + ',';
            end
            else begin
               {$IfDef RecordTileStats} WriteLineToDebugFile('Not doing tile=' + DEMIXTile); {$EndIf}
            end;
         end;


   label
      NoLandCover;
   var
      Ser,i,j,UseDSM,UseDTM : integer;
      TStr : shortstring;
      LandCoverFName : PathStr;
      CandidateBoundBoxGeo,bb : sfBoundBox;
   begin
      {$If Defined(RecordDEMIXFull)} HighlightLineToDebugFile('ProcessDEMIXtestarea in ' + TestAreaName); {$EndIf}

      ReportErrors := false;
      GetDEMIXPaths;
      DEMIXtileDB := 0;
      LandCoverGrid := 0;

      for I := 1 to MaxRefStore do begin
        InitializeMomentVar(SlopeMomentVar[i]);
        InitializeMomentVar(ElevMomentVar[i]);
        NumPits[i] := 0;
        NumPeaks[i] := 0;
      end;

      wmdem.SetPanelText(3,'Load reference DEMs');

      if LoadDEMIXReferenceDEMs(DEMIXRefDEM,true) then begin
         DEMIXtileDB := DEMIXtileFill(DEMIXRefDEM,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,false);

         //option below does not work; it would mean the maps did not have to be opened if we could get it working
         //DEMIXtileDB := LoadDEMIXtileOutlinesNoMap(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,true);

         GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL<' + IntToStr(MDDef.DEMIX_Full));
         {$IfDef RecordDEMIX} WriteLineToDebugFile('DTM tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}
         GISdb[DEMIXtileDB].DeleteAllSelectedRecords;

         if ValidDEM(RefDSMpoint) then begin
            CandidateBoundBoxGeo.XMin := MinFloat(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo.XMin,DEMGlb[RefDTMarea].DEMBoundBoxGeo.XMin);
            CandidateBoundBoxGeo.yMin := MinFloat(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo.YMin,DEMGlb[RefDTMarea].DEMBoundBoxGeo.YMin);
            CandidateBoundBoxGeo.Xmax := MaxFloat(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo.Xmax,DEMGlb[RefDTMarea].DEMBoundBoxGeo.Xmax);
            CandidateBoundBoxGeo.ymax := MaxFloat(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo.Ymax,DEMGlb[RefDTMarea].DEMBoundBoxGeo.Ymax);
         end
         else begin
            CandidateBoundBoxGeo := DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo
         end;

         if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
            TStr := 'No filled DEMIX tiles on ' + TestAreaName;
            ErrorLog.Add(Tstr);
            {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
         end
         else begin
            {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('DEMs start loading'); {$EndIf}
            GISdb[DEMIXtileDB].MyData.First;  //placed here to see where in the tile processing the program is at

            if LoadDEMIXCandidateDEMs(TestAreaName,DEMIXRefDEM,true) then begin
               fName := DEMIX_area_lc100 + TestAreaName + '.tif';
               if FileExists(fname) then begin
                  LandCoverGrid := OpenNewDEM(fName);
                  if DEMGlb[LandCoverGrid].DEMHeader.ElevUnits <> GLCS_LC100 then begin
                     DEMGlb[LandCoverGrid].DEMHeader.ElevUnits := GLCS_LC100;
                     DEMGlb[LandCoverGrid].WriteNewFormatDEM(fName);
                  end;
               end
               else begin
                  bb := DEMGlb[DEMIXRefDEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo;
                  Lat := 0.5 * (bb.YMax + bb.YMin);
                  Long := 0.5 * (bb.XMax + bb.XMin);
                  LandCoverFName := GetLC100_fileName(Lat,Long);
                  {$IfDef RecordDEMIX} writeLineToDebugFile('Landcover=' + LandCoverfName); {$EndIf}
                  if FileExists(LandCoverFName) then begin
                     LandCoverGrid := GDALsubsetGridAndOpen(bb,true,LandCoverFName);
                     if ValidDEM(LandCoverGrid) then begin
                        DEMGlb[LandCoverGrid].DEMHeader.ElevUnits := GLCS_LC100;
                        DEMGlb[LandCoverGrid].WriteNewFormatDEM(fName);
                     end
                     else begin
                        TStr := 'Load landcover error on ' + TestAreaName + ' ' + LandCoverFName;
                        ErrorLog.Add(Tstr);
                        {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
                        goto NoLandCover;
                     end;
                  end
                  else begin
                     TStr := 'Missing landcover file on ' + TestAreaName + ' ' + LandCoverFName;
                     ErrorLog.Add(Tstr);
                     {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
                     goto NoLandCover;
                  end;
               end;
               if ValidDEM(LandCoverGrid) then begin
                  New(zsForest);
                  New(zsUrban);
                  New(zsBarren);
               end;

               new(zs);
               New(zssteep);
               New(zsflat);
               New(zsGentle);
               New(zsCliff);
               {$IfDef RecordDEMIX} writeLineToDebugFile(TestAreaName + '  ProcessDEMIXtestarea in, zs created'); {$EndIf}

               if ElevDiffHists then begin
                  ElevFiles := tStringList.Create;
                  LegendFiles := tStringList.Create;
               end;

               j := 0;
               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  HeavyDutyProcessing := true;
                  inc(j);
                  wmdem.SetPanelText(2,'Tile: ' + IntToStr(j) + '/' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsinDB));
                  if UseThisTile then begin
                     GetTileStatistics(DEMIXRefDEM);
                     for i := 1 to MaxDEMIXDEM do begin
                        if ValidDEM(TestDEM[i]) then begin
                           {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoops) or Defined(RecordCriteriaEvaluation)}
                              WriteLineToDebugFile(DEMIXTile + ' Start tests for DEM=' + IntToStr(TestDEM[i]) + '/' + IntToStr(Ser) +
                                  '  Series=' + TestSeries[i] + '  DEM=' + DEMGlb[TestDEM[i]].AreaName);
                           {$EndIf}
                           GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                           CompareDifferencesToReferenceDEM(TestDEM[i],UseDTM,'DTM');
                           CompareDifferencesToReferenceDEM(TestDEM[i],UseDSM,'DSM');
                           {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('All tests done for ' + TestSeries[i]); {$EndIf}
                        end
                        else begin
                           TStr := 'Missing test dem=' + IntToStr(i) + ' for ' + TestAreaName;
                           ErrorLog.Add(Tstr);
                           {$If Defined(RecordDEMIX) or Defined(RecordCriteriaEvaluation)} WriteLineToDebugFile(TStr); {$EndIf}
                        end;
                     end;
                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               {$If Defined(RecordFullDEMIX)} for i := 1 to 2 do writeLineToDebugFile(''); {$EndIf}
               Dispose(zs);
               Dispose(zssteep);
               Dispose(zsflat);
               Dispose(zsGentle);
               Dispose(zsCliff);
               if ValidDEM(LandCoverGrid) then begin
                  Dispose(zsForest);
                  Dispose(zsUrban);
                  Dispose(zsBarren);
               end;
               if ElevDiffHists then begin
                  ElevFiles.Destroy;
                  LegendFiles.Destroy;
               end;
            end
            else begin
               TStr := 'Missing reference DEM on ' + TestAreaName;
               ErrorLog.Add(Tstr);
               {$If Defined(RecordDEMIX) or Defined(RecordCriteriaEvaluation)}  WriteLineToDebugFile(TStr); {$EndIf}
            end;
            {$If Defined(RecordDEMIX) or Defined(RecordCriteriaEvaluation)}  writeLineToDebugFile('call CloseAndNilNumberedDB(DEMIXtileDB)'); {$EndIf}
            CloseAndNilNumberedDB(DEMIXtileDB);
            NoLandCover:;
         end;
      end
      else begin
         {$If Defined(RecordDEMIX)} HighlightLineToDebugFile(TestAreaName + ',  did not load reference data'); {$EndIf}
      end;
      ReportErrors := true;
      {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('closing all DEMs'); {$EndIf}
      CloseAllDEMs;
    end;


var
   DTMArea : pathStr;
   i : integer;
   FilesWanted : tStringList;
begin {procedure ComputeDEMIX_tile_stats}
   {$IfDef RecordDEMIX} writeLineToDebugFile('Start ComputeDEMIXstats'); {$EndIf}
   try
      GetDEMIXpaths;
      ErrorLog := tStringList.Create;
      SaveBackupDefaults;
      MDdef.ConfirmDBEdits := false;
      MDdef.DefaultMapXSize := 800;
      MDdef.DefaultMapYSize := 800;

      LandTypeMask := 'ALL';

      //if DoHorizontalShift then SafeMakeDir(DEMIXresultsDir + 'lags\' );

      FilesWanted := DEMIX_AreasWanted(AreaName);

      {$IfDef RecordDEMIX} writeLineToDebugFile('Areas to process=' + IntToStr(FilesWanted.Count)); {$EndIf}

      for i := 0 to pred(FilesWanted.Count) do begin
         wmdem.SetPanelText(1,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count));
         cFName := FilesWanted.Strings[i];
         if StrUtils.AnsiContainsText(UpperCase(cfName),'_DSM') then begin
            //this will be the done same time as its DTM
         end
         else begin
            TestAreaName := ShortTestAreaName(ExtractFileNameNoExt(cfName));
            DTMArea := ExtractFileNameNoExt(cfName);
            if DEMIXSkipFilesAlreadyDone and FileExists(DEMIXresultsDir + TestAreaName + '_Elev_diff_stats.csv') and
                  FileExists(DEMIXresultsDir + TestAreaName + '_Slope_diff_stats.csv') and
                  FileExists(DEMIXresultsDir + TestAreaName + '_Ruff_diff_stats.csv') then begin
               {$IfDef RecordDEMIX} writeLineToDebugFile(TestAreaName + ' already processed'); {$EndIf}
            end
            else begin
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Elev_diff_stats.csv');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Slope_diff_stats.csv');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Ruff_diff_stats.csv');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Elev_diff_stats.dbf');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Slope_diff_stats.dbf');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Ruff_diff_stats.dbf');

               RefDTMPointFName := DEMIX_Ref_1sec + DTMArea + '_ref_1sec_point.tif';
               RefDTMareaFName := StringReplace(RefDTMPointFName, 'point', 'area',[rfIgnoreCase]);
               COPRefDTMFName := StringReplace(RefDSMPointFName, '1sec', '1.5x1sec',[rfIgnoreCase]);
               COPRefDSMFName := StringReplace(COPRefDTMFName, 'dtm', 'dsm',[rfIgnoreCase]);

               if not FileExists(RefDTMPointFName) then RefDTMPointFName := '';
               if not FileExists(RefDTMareaFName) then RefDTMareaFName  := '';
               if not FileExists(COPRefDTMFName) then COPRefDTMFName := '';
               if not FileExists(COPRefDSMFName) then COPRefDSMFName := '';

               if StrUtils.AnsiContainsText(DTMArea,'_dtm') then begin
                  RefDSMpointFName := StringReplace(RefDTMpointFName, 'dtm', 'dsm',[rfIgnoreCase]);
                  RefDSMareaFName := StringReplace(RefDTMareaFName, 'dtm', 'dsm',[rfIgnoreCase]);
                  if not FileExists(RefDSMPointFName) then RefDSMPointFName := '';
                  if not FileExists(RefDSMareaFName) then RefDSMareaFName := '';
               end
               else begin
                  RefDSMPointFName := '';
                  RefDSMareaFName := '';
               end;

               if FileExists(RefDTMPointFName) and FileExists(RefDTMareaFName) then begin
                  {$IfDef RecordDEMIX} writeLineToDebugFile('Start process for ' + TestAreaName); {$EndIf}
                  InitializeStringLists;
                  ProcessDEMIXtestarea;
                  FinalizeStringLists(false);
               end
               else begin
                  {$IfDef RecordDEMIX} writeLineToDebugFile(TestAreaName + ' missing 1 sec reference DEMs'); {$EndIf}
               end;
            end;
         end;
      end;
      {$IfDef RecordDEMIXFull} WriteLineToDebugFile('All processing done'); {$EndIf}
      FilesWanted.Destroy;
   finally;
      EndDEMIXProcessing;
      RestoreBackupDefaults;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_VDatum_shifts Problems');
   end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('End ComputeDEMIXstats'); {$EndIf}
end {ComputeDEMIX_tile_stats};






