{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{   include file for demix_definitions   }
{________________________________________}


{$Define UseOldDifferenceDistributions}


//{$Define RecordBest}


function DecsForCriterion(Criterion : shortString) : integer;
begin
   if AnsiContainsText(Criterion,'FUV') then Result := -8 else Result := -5;
end;


procedure FindBestReferenceDEM(db : integer; DEM : shortstring);
const
   NumCompared = 6;
   NumCriteria = 5;
   NumBests = 4;
type
   tCompareNames = array[1..NumCompared] of shortstring;
   tBestNames = array[1..NumBests] of shortstring;
const
   CompareCriteria : array[1..NumCriteria] of shortstring = ('ALL','ELEV_FUV','ELEV_MAE','SLOPE_FUV','SLOPE_MAE');

   AlosCompareNames : tCompareNames = ('DTM_ALOS','DTM_SRTM','REV_DTM_S','DSM_ALOS','REV_SRTM','REV_DSM_S');  //,'DTM_POINT');
   AlosBestNames : tBestNames = ('BEST_ALL','BEST_ALOS','BEST_DTM','BEST_DSM');
   CopCompareNames : tCompareNames = ('DTM_SRTM','DTM_ALOS','REV_DTM_A','DSM_SRTM','DSM_ALOS','REV_DSM_A');  //,'DTM_POINT');
   CopBestNames : tBestNames = ('BEST_ALL','BEST_COP','BEST_DTM','BEST_DSM');

type
   tCompare64 = array[1..NumCompared] of float64;
var
   CompareNames : tCompareNames;
   BestNames : tBestNames;


    procedure FindBestComparison(Criterion : shortstring; v : tCompare64; var Best1,Best2,Best3,Best4,Diff12,Ties,WhoTies,CloserDSMorDTM : shortstring);
    //Ties and Diff12 only done for the first criterion, all of them
    const
       AllOfThem : array[1..7] of byte = (1,2,3,4,5,6,7);
       JustCop : array[1..2] of byte = (1,4);
       JustDTM : array[1..3] of byte = (1,2,3);
       JustDSM : array[1..3] of byte = (4,5,6);
    var
       Which,i,Test,NumTie : integer;
       vt,vb,Diff,Tolerance : float64;
       aline : shortstring;
       ToSort : array[1..6] of float64;
    begin
        GetToleranceForCriterion('ELEV_FUV',Tolerance);
        if abs(v[1] - v[4]) < Tolerance then CloserDSMorDTM := 'TIE'
        else if v[1] < v[4] then CloserDSMorDTM := 'DTM'
        else CloserDSMorDTM  := 'DSM';

        for i := 1 to 6 do ToSort[i] := v[AllOfThem[i]];
        HeapSort(6,ToSort);
        Diff := ToSort[2] - ToSort[1];
        Diff12 := RealToString(Diff,-12,DecsForCriterion(Criterion));
        WhoTies := '';
        for i := 1 to 6 do begin
           Test := AllOfThem[i];
           if v[Test] < ToSort[1] + Tolerance then begin
              Best1 := CompareNames[Test];
              break;
           end;
        end;
        NumTie := 0;
        WhoTies := '';
        for i := 1 to 6 do begin
           Test := AllOfThem[i];
           if v[i] < ToSort[1] + Tolerance then begin
              inc(NumTie);
              WhoTies := WhoTies + CompareNames[Test] + '+';
           end;
        end;
        Delete(WhoTies,length(WhoTies),1);
        if NumTie = 1 then begin
           Ties := '';
        end
        else Ties := IntToStr(NumTie) + ' ties';

        {$IfDef RecordBest} WriteLineToDebugFile(BestNames[1] + '  ' + Best1 + '  ' + RealToString(ToSort[1],-12,6)); {$EndIf}

        for i := 1 to 2 do ToSort[i] := v[JustCOP[i]];
        HeapSort(2,ToSort);
        for i := 1 to 2 do begin
           Test := JustCop[i];
           if v[Test] < ToSort[1] + Tolerance then begin
              Best2 := CompareNames[Test];
              break;
           end;
        end;
        {$IfDef RecordBest} WriteLineToDebugFile(BestNames[2] + '  ' + Best2 + '  ' + RealToString(ToSort[1],-12,6)); {$EndIf}

        for i := 1 to 3 do ToSort[i] := v[JustDTM[i]];
        HeapSort(3,ToSort);
        for i := 1 to 3 do begin
           Test := JustDTM[i];
           if v[Test] < ToSort[1] + Tolerance then begin
              Best3 := CompareNames[Test];
              break;
           end;
        end;
        {$IfDef RecordBest} WriteLineToDebugFile(BestNames[3] + '  ' + Best3 + '  ' + RealToString(ToSort[1],-12,6)); {$EndIf}

        for i := 1 to 3 do ToSort[i] := v[JustDSM[i]];
        HeapSort(3,ToSort);
        for i := 1 to 3 do begin
           Test := JustDSM[i];
           if v[Test] < ToSort[1] + Tolerance then begin
              Best4 := CompareNames[Test];
              break;
           end;
        end;
        {$IfDef RecordBest} WriteLineToDebugFile(BestNames[4] + '  ' + Best4 + '  ' + RealToString(ToSort[1],-12,6)); {$EndIf}
    end {procedure FindBestComparison};


const
   OpenMaps = false;
var
   DSMDir,fName,EGMDir,DTMDir : PathStr;
   AreaName,Country,TileName,LatStr,LongStr,Criterion,aLine,Best1,Best2,Best3,Best4,Diff12,BestTies,WhoTies,CloserDSMorDTM,DSMTile : shortstring;
   Results : tstringlist;
   NewDB,i,Count : integer;
   v : tCompare64;
   mv : array[1..NumCompared] of tMomentVar;
   MainDEM,MainDTMRef,RevDTMRef,MainDSMRef,RevDSMRef,
   DSM_SRTM,DSM_ALOS,
   COP,ALOS,RefSRTM,RefALOS : integer;

   function OpenaDEM(fName : PathStr; NewAreaName : shortString = '') : integer; //allows checking file name during debug, and option to record in debug log
   begin
      Result := OpenNewDEM(fName,OpenMaps);
      if (NewAreaName <> '') then DEMglb[Result].AreaName := NewAreaName;
      {$IfDef RecordDEMIXFull}
         HighLightInDebugFile('Opened ' + DEMglb[Result].AreaName + ' ' + DEMglb[Result].KeyParams + ' '  + DEMglb[Result].SWcornerString);
      {$EndIf}
   end;

    procedure WriteLine(Criterion : shortString);
    var
       i : integer;
    begin
       aline := TileName + ',' + DSMTile  + ',' + AreaName + ',' + Country + ',' + LatStr + ',' + LongStr +',' + CommonTileStats(db) + ',' + Criterion;
       for i := 1 to NumCompared do aLine := aline + ',' + RealToString(v[i],12,DecsForCriterion(Criterion));
       aLine := aline + ',' + Best1  + ',' + Best2 + ',' + Best3 + ',' + Best4;
       aLine := aline + ',' + BestTies + ',' + Diff12 + ',' + WhoTies + ',' + CloserDSMorDTM;
       {$IfDef RecordBest} WriteLineToDebugFile(aline); {$EndIf}
       Results.Add(aline);
       WriteLineToDebugFile(IntToStr(Results.Count));
    end;

   procedure DoComparisons(Criterion1,Criterion2 : shortstring);
   var
      i : integer;
   begin
        v[1] := GetFUVForPairGrids(DEMglb[MainDEM].FullDEMGridLimits,MainDEM,MainDTMref);
        v[2] := GetFUVForPairGrids(DEMglb[MainDEM].FullDEMGridLimits,MainDEM,RevDTMref);
        v[3] := GetFUVForPairGrids(DEMglb[RevDTMref].FullDEMGridLimits,RevDTMref,MainDEM);
        v[4] := GetFUVForPairGrids(DEMglb[MainDEM].FullDEMGridLimits,MainDEM,MainDSMref);
        v[5] := GetFUVForPairGrids(DEMglb[MainDEM].FullDEMGridLimits,MainDEM,RevDTMRef);
        v[6] := GetFUVForPairGrids(DEMglb[RevDTMRef].FullDEMGridLimits,RevDTMRef,MainDEM);
        FindBestComparison(Criterion1,v,Best1,Best2,Best3,Best4,Diff12,BestTies,WhoTies,CloserDSMorDTM);
        WriteLine(Criterion1);

        mv[1] := GridDifferenceStatsTwoGrids(DEMglb[MainDEM].FullDEMGridLimits,MainDEM,MainDTMref);
        mv[2] := GridDifferenceStatsTwoGrids(DEMglb[MainDEM].FullDEMGridLimits,MainDEM,RevDTMref);
        mv[3] := GridDifferenceStatsTwoGrids(DEMglb[RevDTMref].FullDEMGridLimits,RevDTMref,MainDEM);
        mv[4] := GridDifferenceStatsTwoGrids(DEMglb[MainDEM].FullDEMGridLimits,MainDEM,MainDSMref);
        mv[5] := GridDifferenceStatsTwoGrids(DEMglb[MainDEM].FullDEMGridLimits,MainDEM,RevDTMRef);
        mv[6] := GridDifferenceStatsTwoGrids(DEMglb[RevDTMRef].FullDEMGridLimits,RevDTMRef,MainDEM);
        for i := 1 to NumCompared do v[i] := mv[i].mae;
        FindBestComparison(Criterion2,v,Best1,Best2,Best3,Best4,Diff12,BestTies,WhoTies,CloserDSMorDTM);
        WriteLine(Criterion2);
   end;


      procedure ResultsBestReferenceDEM(db : integer; DEMName : shortstring);
      var
         Results : tStringList;
         aline,cf,BaseFilter : shortstring;
         fName : PathStr;
         i,Criterion,Best,Compare : integer;
      begin
         SetColorForWaiting;
         BaseFilter := GISdb[db].MyData.Filter;

         Results := tStringList.Create;
         aline := 'CRITERION,BEST';
         for i := 1 to NumCompared do aline := aline + ',' + CompareNames[i];
         Results.Add(aline);

          for Criterion := 1 to NumCriteria do begin
             if Criterion = 1 then cf := ''
             else cf := ' AND CRITERION=' + QuotedStr(CompareCriteria[Criterion]);
             for Best := 1 to NumBests do begin
                aline := CompareCriteria[Criterion] + ',' + BestNames[Best];
                for Compare := 1 to NumCompared do begin
                    GISdb[db].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + BestNames[Best] + '=' + QuotedStr(CompareNames[Compare]) + cf);
                    GISdb[db].EmpSource.Enabled := false;
                    aline := aline + ',' + IntToStr(GISdb[db].MyData.FiltRecsInDB);
                end;
                Results.Add(aline);
             end;
          end;
          GISdb[db].ClearGISFilter;
          GISdb[db].EmpSource.Enabled := true;
          fName := ExtractFilePath(GISdb[db].dbFullName) + DEMName + '_summary_two_best_reference_' + DEMIXcreationString(db)  + '.dbf';
          StringList2CSVtoDB(Results, fName);

         Results := tStringList.Create;
         aline := 'CRITERION,REF_DEM';
         for i := 1 to NumBests do aline := aline + ',' + BestNames[i];
         Results.Add(aline);
          for Criterion := 1 to NumCriteria do begin
             if Criterion = 1 then cf := ''
             else cf := ' AND CRITERION=' + QuotedStr(CompareCriteria[Criterion]);
             for Compare := 1 to NumCompared do begin
                aline := CompareCriteria[Criterion] + ',' + CompareNames[Compare];
                for Best := 1 to NumBests do begin
                    GISdb[db].ApplyGISFilter(BestNames[Best] + '=' + QuotedStr(CompareNames[Compare]) + cf);
                    GISdb[db].EmpSource.Enabled := false;
                    aline := aline + ',' + IntToStr(GISdb[db].MyData.FiltRecsInDB);
                end;
                Results.Add(aline);
             end;
          end;
          GISdb[db].ApplyGISFilter(BaseFilter);
          GISdb[db].EmpSource.Enabled := true;
          fName := ExtractFilePath(GISdb[db].dbFullName) + DEMName + '_summary_one_best_reference_' + DEMIXcreationString(db)  + '.dbf';
          StringList2CSVtoDB(Results, fName);
          SetColorForWaiting;
      end {procedure ResultsBestReferenceDEM};



begin {procedure FindBestReferenceDEMforALOS}
   SetColorForProcessing;
   HeavyDutyProcessing := true;
   WantShowProgress := false;
   GISdb[db].ApplyGISFilter('REF_DEM=' + QuotedStr('DTM') + ' AND LAT<50');
   GISdb[db].EmpSource.Enabled := false;

    if DEM = 'ALOS' then begin
       CompareNames := ALOSCompareNames;
       BestNames :=  ALOSBestNames;
    end
    else if DEM = 'COP' then begin
       CompareNames := CopCompareNames;
       BestNames := CopBestNames;
    end;

   Results := tStringList.Create;
   aLine := 'DEMIX_TILE,DSM_NAME,AREA,COUNTRY,LAT,LONG,' + CommonTileStatsNames(db) + ',CRITERION';
   for i := 1 to NumCompared do aline := aline + ',' + CompareNames[i];
   for i := 1 to NumBests do aline := aline + ',' + BestNames[i];
   Results.Add(aline + ',BEST_TIES,DBEST1_2,WHO_TIES,DSM_OR_DTM');
   {$IfDef RecordBest} WriteLineToDebugFile(aline); {$EndIf}

   Count := 0;
   while not GISdb[db].MyData.eof do begin
       inc(Count);
       wmDEM.SetPanelText(1,IntToStr(Count) + '/' + IntToStr(GISdb[db].MyData.FiltRecsInDB),true);

       Country := GISdb[db].MyData.GetFieldByNameAsString('COUNTRY');
       AreaName := GISdb[db].MyData.GetFieldByNameAsString('AREA');
       TileName := GISdb[db].MyData.GetFieldByNameAsString('DEMIX_TILE');
       DSMTile := GISdb[db].MyData.GetFieldByNameAsString('DSM_NAME');
       LatStr := GISdb[db].MyData.GetFieldByNameAsString('LAT');
       LongStr := GISdb[db].MyData.GetFieldByNameAsString('LONG');

       DTMDir := ExtractFilePath(GISdb[db].DBFullName) + AreaName  + '\' + TileName  + RefTestStr + '\';
       COP := OpenaDEM(DTMDir + 'cop.tif','COP');
       ALOS := OpenaDEM(DTMDir + 'alos.tif','COP');
       RefSRTM := OpenaDEM(DTMDir + 'ref_DTM_srtm.tif','DTM_srtm');
       RefALOS := OpenaDEM(DTMDir + 'ref_DTM_alos.tif','DTM_alos');

       DSMDir := ExtractFilePath(GISdb[db].DBFullName) + AreaName  + '\' + DSMTile + RefTestStr + '\';
       DSM_SRTM := OpenaDEM(DSMDir + 'ref_DSM_srtm.tif','DSM_srtm');
       DSM_ALOS := OpenaDEM(DSMDir + 'ref_DSM_alos.tif','DSM_alos');

       if DEM = 'ALOS' then begin
           MainDEM := ALOS;
           MainDTMRef := RefALOS;
           RevDTMRef := RefSRTM;
           MainDSMRef := DSM_ALOS;
           RevDSMRef := DSM_SRTM;
       end
       else if DEM = 'COP' then begin
           MainDEM := Cop;
           MainDTMRef := RefSRTM;
           RevDTMRef := RefALOS;
           MainDSMRef := DSM_SRTM;
           RevDSMRef := DSM_ALOS;
       end;
        DoComparisons('ELEV_FUV','ELEV_MAE');

        MainDEM := CreateEvansSlopeMapPercent(false,MainDEM);
        MainDTMRef := CreateEvansSlopeMapPercent(false,MainDTMref);
        RevDTMRef := CreateEvansSlopeMapPercent(false,MainDTMref);
        MainDSMRef := CreateEvansSlopeMapPercent(false,MainDSMref);
        RevDSMRef := CreateEvansSlopeMapPercent(false,DSM_ALOS);
        DoComparisons('SLOPE_FUV','SLOPE_MAE');
        CloseAllDEMs;
        GISdb[db].MyData.Next;
   end;

   fName := ExtractFilePath(GISdb[db].dbFullName) + DEM + '_best_reference_' + DEMIXcreationString(db) + '.csv';
   Results.SaveToFile(fName);
   fName := ChangeFileExt(fName, '.dbf');
   aline := 'saved ' + IntToStr(Results.Count) + ' to ' + fName;
   newDB := StringList2CSVtoDB(Results, fName);
   wmDEM.StatusBar1.Panels[0].Text := '';
   GISdb[db].EmpSource.Enabled := true;

   GISdb[NewDB].ApplyGISFilter(CompareNames[1] + '< 0.000001');
   if GISdb[NewDB].MyData.FiltRecsinDB > 0 then begin
      GISdb[NewDB].DeleteAllSelectedRecords;
   end;
   ResultsBestReferenceDEM(Newdb,DEM);
   HeavyDutyProcessing := false;

    SetColorForWaiting;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('Out ' + aline); {$EndIf}
end {procedure FindBestReferenceDEMforALOS};


procedure DSM_gt_DTM_1(db : integer);
//adds field DSM_PC with percentage of DEM where DSM and DTM should be different, urban and forest landcovers
var
   x,y,Total,Higher,Count,RefDSM,RefDTM : integer;
   AreaName : shortstring;
   DTMdir,DSMdir : PathStr;
   z1,z2 : float32;
begin
   SetColorForProcessing;
   ShowHourglassCursor;
   WantShowProgress := false;
   GISdb[DB].ClearGISFilter;
   GISdb[DB].MyData.InsureFieldPresentAndAdded(ftFloat,'DSM_G1_DTM',8,2);

   Count := 0;
   while not GISdb[db].MyData.eof do begin
       inc(Count);
       wmDEM.SetPanelText(1,IntToStr(Count) + '/' + IntToStr(GISdb[db].MyData.FiltRecsInDB),true);
       GISdb[DB].EmpSource.Enabled := false;
       AreaName := GISdb[db].MyData.GetFieldByNameAsString('AREA');

       DTMDir := ExtractFilePath(GISdb[db].DBFullName) + AreaName  + '\' + GISdb[db].MyData.GetFieldByNameAsString('DTM_NAME')  + RefTestStr + '\';
       RefDTM := OpenNewDEM(DTMDir + 'ref_DTM_srtm.tif',false);

       DSMDir := ExtractFilePath(GISdb[db].DBFullName) + AreaName  + '\' + GISdb[db].MyData.GetFieldByNameAsString('DSM_NAME')  + RefTestStr + '\';
       RefDSM := OpenNewDEM(DSMDir + 'ref_DSM_srtm.tif',false);

       Total := 0;
       Higher := 0;
       for x := 0 to pred(DEMglb[RefDSM].DEMheader.NumCol) do begin
          for y := 0 to pred(DEMglb[RefDSM].DEMheader.NumRow) do begin
             if DEMglb[RefDTM].GetElevMetersOnGrid(x,y,z1) and DEMglb[RefDSM].GetElevMetersOnGrid(x,y,z2) then begin
                 inc(Total);
                 if (z2 - z1 > 1) then inc(Higher);
             end;
          end;
       end;
       CloseSingleDEM(RefDTM);
       CloseSingleDEM(RefDSM);
       GISdb[DB].MyData.Edit;
       GISdb[DB].MyData.SetFieldByNameAsFloat('DSM_G1_DTM',100 * Higher / Total);
       GISdb[DB].MyData.Next;
   end;
    SetColorForWaiting;
   ShowDefaultCursor;
   WantShowProgress := true;
   wmDEM.StatusBar1.Panels[1].Text := '';
end;


procedure DSM_DTM_DifferFromLandcover(dbOnTable : integer);
//adds field DSM_PC with percentage of DEM where DSM and DTM should be different, urban and forest landcovers
var
   Urban,Forest : float32;
begin
   ShowHourglassCursor;
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftFloat,'DSM_PC',8,2);
   while not GISdb[DBonTable].MyData.eof do begin
       GISdb[DBonTable].EmpSource.Enabled := false;
       Urban := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('URBAN_PC');
       Forest := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('FOREST_PC');
       GISdb[DBonTable].MyData.Edit;
       GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DSM_PC',Urban+Forest);
       GISdb[DBonTable].MyData.Next;
   end;
   ShowDefaultCursor;
end;


procedure IdentifyDSMorDTM(dbOnTable : integer);
//adds REF_DEM field to database
var
   Tile,TStr : shortstring;
begin
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'REF_DEM',3);
   GISdb[DBonTable].MyData.First;
   GISdb[DBonTable].EmpSource.Enabled := false;
   while not GISdb[DBonTable].MyData.eof do begin
      Tile := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetFieldByNameAsString('REF_DEM',IdentifyDEMIXtileAsDTMorDSM(Tile));
      GISdb[DBonTable].MyData.Next;
   end;
end;


procedure IdentifyDSM_DTMpair(DBonTable : integer);
var
   Lat,Long : float64;
   DTMName,DSMName : PathStr;
   fl : integer;
   Tile,Tstr : shortstring;
begin
   GISdb[DBonTable].ClearGISFilter;
   SetColorForProcessing;
   fl := GISdb[DBonTable].MyData.GetFieldLength('DEMIX_TILE');
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DEM_PAIR',3);
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DSM_NAME',fl+3);
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,'DTM_NAME',fl+3);
   GISdb[DBonTable].ApplyGISFilter('DEM_PAIR=' + QuotedStr('NO'));
   GISdb[DBonTable].FillFieldWithValue('DEM_PAIR','',false);

   repeat
       GISdb[DBonTable].ApplyGISFilter('DEM_PAIR=' + QuotedStr(''));
       GISdb[DBonTable].EmpSource.Enabled := false;
       if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
           wmdem.SetPanelText(1,'Tiles to go: ' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB),true);
           {$IfDef RecordDEMIX} WriteLineToDebugFile(Tile + '  Tiles to go: ' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
           Tile := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
           GISdb[DBonTable].MyData.ValidLatLongFromTable(Lat,Long);
           if GetDSMandDTMTileNamesFromLatLong(DBonTable,Lat,Long,DTMName,DSMName) then TStr := 'YES' else TStr := 'NO';
           GISdb[DBonTable].FillFieldWithValue('DEM_PAIR',TStr,false);
           GISdb[DBonTable].FillFieldWithValue('DSM_NAME',DSMname,false);
           GISdb[DBonTable].FillFieldWithValue('DTM_NAME',DTMname,false);
           GISdb[DBonTable].ApplyGISFilter('DEM_PAIR=' + QuotedStr(''));
       end;
   until (GISdb[DBonTable].MyData.FiltRecsInDB = 0);
   SetColorForWaiting;
   GISdb[DBonTable].ClearGISFilter;
end;



procedure MakeCSVforGDEMslopes(DB : integer);
var
   Areas,Results,TheTiles : tStringList;
   DEMIX_Tile,DSM_tile,aLine,Country,
   Area,BaseFilter,TStr,theFilter,TileStats : shortstring;
   i,j,Thin,DEMthin : integer;
   AvgSlope : array[1..10] of float64;
   fName : PathStr;

       function TryOpenDEM(DEM : shortString; TileName : PathStr) : integer;
       var
          fName : PathStr;
       begin
           fName := ExtractFilePath(GISdb[DB].DBFullName) + Area + '\' + TileName + RefTestStr +'\' + DEM + '.tif';
           if FileExists(fName) then Result := OpenNewDEM(fName,false)
           else Result := -999;
       end;

var
   DEMs : array[1..10] of integer;
begin {procedure MakeCSVforGDEMslopes}
  {$IfDef TrackAverageStats} WriteLineToDebugFile('MakeCSVforGDEMslopes in'); {$EndIf}
    SetColorForProcessing;
    WantShowProgress := false;
    HeavyDutyProcessing := true;
    wmDEM.SetPanelText(0,TimeToStr(Now) + ' started',true);

    theFilter := AddAndIfNeeded(BaseFilter) + 'REF_DEM=' + QuotedStr('DTM') + ' AND DEM_PAIR=' + QuotedStr('YES');
    GISdb[db].ApplyGISFilter(theFilter);
    GISdb[db].EmpSource.Enabled := false;
    TheTiles := GISdb[DB].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
    GISdb[DB].MyData.First;
    i := 0;
    while not GISdb[DB].MyData.eof do begin
       inc(i);
       Area := GISdb[DB].MyData.GetFieldByNameAsString('AREA');
       DEMIX_TILE := GISdb[DB].MyData.GetFieldByNameAsString('DEMIX_TILE');
       DSM_TILE := GISdb[DB].MyData.GetFieldByNameAsString('DSM_NAME');
       wmDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(TheTiles.Count) + ' ' + Area,true);
       TileStats := Area + ',' + DEMIX_TILE + ',' + GISdb[DB].MyData.GetFieldByNameAsString('LAT') + ',' + GISdb[db].MyData.GetFieldByNameAsString('LONG') + ',' +
                    CommonTileStats(db);
       DEMs[1] := TryOpenDEM('cop',DEMIX_TILE);
       DEMs[2] := TryOpenDEM('alos',DEMIX_TILE);
       DEMs[3] := TryOpenDEM('fathom',DEMIX_TILE);
       DEMs[4] := TryOpenDEM('gedtm',DEMIX_TILE);
       DEMs[5] := TryOpenDEM('ref_dtm_alos',DEMIX_TILE);
       DEMs[6] := TryOpenDEM('ref_dsm_alos',DSM_TILE);
       DEMs[7] := TryOpenDEM('ref_dtm_srtm',DEMIX_TILE);
       DEMs[8] := TryOpenDEM('ref_dsm_srtm',DSM_TILE);
       DEMs[9] := TryOpenDEM('ref_dtm_cop',DEMIX_TILE);
       DEMs[10] := TryOpenDEM('ref_dsm_cop',DSM_TILE);
       for Thin := 1 to 6 do begin
           aline := TileStats + ',' + IntToStr(Thin);
           for j := 1 to 10 do begin
              if ValidDEM(DEMs[j]) then begin
                 if (Thin = 1) then begin
                    AvgSlope[j] := DEMglb[DEMs[j]].AverageDEMslope(DEMglb[DEMs[j]].FullDEMGridLimits);
                 end
                 else begin
                    DEMthin := DEMglb[DEMs[j]].ThinThisDEM(false,'',Thin,true);
                    AvgSlope[j] := DEMglb[DEMthin].AverageDEMslope(DEMglb[DEMs[j]].FullDEMGridLimits);
                    CloseSingleDEM(DEMthin);
                 end;
                 aline := aline + ',' + RealToString(AvgSlope[j],-8,-2);
              end
              else aline := aline + ',-9999';  //required because the COP reference DEMs only occur at high latitudes
           end;

           Results.Add(aline);
       end;
       CloseAllDEMs;
       GISdb[DB].MyData.Next;
    end;

   fName := ExtractFilePath(GISdb[db].DBFullName) + 'GDEM_slope_grid_size_' + CurrentTimeForFileName(false) + '.dbf';
   StringList2CSVtoDB(Results,fName);
   {$IfDef TrackAverageStats} WriteLineToDebugFile('MakeCSVforGDEMcompareDSMtoDTM out, saved ' + fName); {$EndIf}
   wmDEM.ClearStatusBarPanelText;
   GISdb[db].ClearGISFilter;
   SetColorForWaiting;
   ShowDefaultCursor;
   HeavyDutyProcessing := false;
end;


procedure MakeCSVforGDEM_average_tile_slope(DBonTable : integer);
var
   Areas,Results,Tiles : tStringList;
   AreaName,BaseFilter,TStr,theFilter,TileStats : shortstring;
   aLine : array[1..6] of shortstring;
   DEMs : array[1..6] of integer;
   i,j,DEM,Thin : integer;
   AllValid : boolean;
   RefALOS_DSM,RefALOS_DTM,AlosName,BaseDir,CSVdir,
   fName,RefDSMName,RefDTMName,CopNAME,FathomName,cName,tName : PathStr;

          procedure TryOpening(DEM : integer; fName : PathStr);
          begin
             if FileExists(fName) then begin
                DEMs[DEM] := OpenNewDEM(fName,false);
             end
             else AllValid := false;
             if Not ValidDEM(DEMs[DEM]) then AllValid := false;
          end;

   procedure AddAThinLevel(thin : integer);
   var
      DEM,ThinDEM,aDEM : integer;
      Mean,Std : float32;
   begin
      aLine[thin] := aline[thin] + ',' + IntToStr(Thin);
      for DEM := 1 to 5 do begin
          aDEM := DEMs[DEM];
          if (thin = 1) then begin
             DEMglb[aDEM].GetSlopeMeanStd(DEMglb[aDEM].FullDEMGridLimits,Mean,Std);
             aLine[thin] := aline[thin] + ',' + RealToString(Mean,-8,-2);
          end
          else begin
              ThinDEM := DEMglb[aDEM].ThinThisDEM(false,'',Thin,true);
              DEMglb[ThinDEM].GetSlopeMeanStd(DEMglb[ThinDEM].FullDEMGridLimits,Mean,Std);
              aLine[thin] := aLine[thin] + ',' + RealToString(Mean,-8,-2);
              CloseSingleDEM(ThinDEM);
          end;
      end;
   end;


begin
  {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} HighLightInDebugFile('MakeCSVforGDEMcompareDSMtoDTM in ' + GISdb[dbOnTable].dbName); {$EndIf}
   try
       SetColorForProcessing;
       WantShowProgress := false;
       HeavyDutyProcessing := true;
       if (MDDef.DEMIX_BaseDir = '') then MDDef.DEMIX_BaseDir := ExtractFilePath(GISdb[DBonTable].dbFullName);
       BaseFilter := 'DEM_PAIR=' + QuotedStr('YES') + ' AND REF_DEM=' + QuotedStr('DTM') ;
       GISdb[DBonTable].ApplyGISFilter(BaseFilter );
       Areas := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('AREA');
       //BaseDir := ExtractFilePath(GISdb[DBonTable].DBFullName) + 'aa_gdem_compare_dsm_dtm\';
       CSVdir := ExtractFilePath(GISdb[dbOnTable].DBfullName) + 'aa_gdem_slope_by_dem_resolution\';
       SafeMakeDir(CSVDir);
       for I := 0 to pred(Areas.Count) do begin
          AreaName := Areas.Strings[i];
          fName := CSVdir + AreaName + '_gdem_compare_tile_slope.csv';
          if FileExists(fName) then begin

          end
          else begin

            TStr := IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName;
            wmDEM.SetPanelText(1,TStr,true);
            Results := tStringList.Create;
            Results.Add('DEMIX_TILE,AREA,COUNTRY,LAT,LONG,CRITERION,AVG_SLOPE,BARREN_PC,FOREST_PC,GRID_SEC,REF_DSM,REF_DTM,COP,ALOS,FATHOM');

            theFilter := AddAndIfNeeded(BaseFilter) + 'AREA=' + QuotedStr(AreaName);
            GISdb[DBonTable].ApplyGISFilter(theFilter);
            {$IfDef RecordDTM_DSM} WriteLineToDebugFile(IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName + '  filter=' + theFilter + '  tiles=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB)); {$EndIf}
            j := 0;
            while not GISdb[DBonTable].MyData.eof do begin
                inc(j);
                wmDEM.SetPanelText(0,TimeToStr(Now),true);
                wmDEM.SetPanelText(2,IntToStr(j) + '/' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ' ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME'),true);
                cname := ExtractFilePath(GISdb[DBonTable].DBFullName) + AreaName + '\' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DSM_NAME') + RefTestStr +'\';
                RefDSMName := cName + 'ref_dsm_cop.tif';
                if not FileExists(RefDSMname) then RefDSMName := cName + 'ref_dsm_srtm.tif';;

                RefALOS_DSM := cName + 'ref_dsm_alos.tif';

                cname := ExtractFilePath(GISdb[DBonTable].DBFullName) + AreaName + '\' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME') + RefTestStr +'\';
                RefDTMName := cName + 'ref_dtm_cop.tif';
                if not FileExists(RefDTMname) then RefDTMName := cName + 'ref_dtm_srtm.tif';;
                RefALOS_DTM := cName + 'ref_dtm_alos.tif';

                CopName := cName + 'cop.tif';
                ALOSname := CName + 'alos.tif';
                Fathomname := cName + 'fathom.tif';

                AllValid := true;
                TryOpening(1,RefDSMName);
                TryOpening(2,RefDTMName);
                TryOpening(3,CopName);
                TryOpening(4,alosName);
                TryOpening(5,FathomName);

                if AllValid then begin //loop processes a tile
                    for thin := 1 to 6 do begin
                       aLine[thin] := GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME') + ',' + AreaName + ',' +  GISdb[DBonTable].MyData.GetFieldByNameAsString('COUNTRY') + ',' +
                           GISdb[DBonTable].MyData.GetFieldByNameAsString('LAT') + ',' +
                           GISdb[DBonTable].MyData.GetFieldByNameAsString('LONG') + ',RES_SLOPE,' + CommonTileStats(DBonTable);
                    end;
                    for thin := 1 to 6 do begin
                        AddAThinLevel(thin);
                    end;
                    for thin := 1 to 6 do begin
                       Results.Add(aline[thin]);
                       aline[thin] := '';
                    end;
                end
                else begin
                   {$If Defined(RecordDTM_DSM)} WriteLineToDebugFile('MissingDEMs, ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME')); {$EndIf}
                end;

                for DEM := 1 to 5 do begin
                   CloseSingleDEM(DEMS[DEM]);
                end;
                GISdb[DBonTable].MyData.Next;
             end {while};
             Results.SaveToFile(fName);
             Results.Destroy;
          end;
       end {for i};
   finally
       wmDEM.ClearStatusBarPanelText;
       GISdb[DBonTable].ClearGISFilter;
       SetColorForWaiting;
       ShowDefaultCursor;
       HeavyDutyProcessing := false;
   end;
   {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} WriteLineToDebugFile('MakeCSVforGDEMcompareDSMtoDTM out, saved ' + fName); {$EndIf}
end;


procedure MakeCSVforGDEMcompareDSMtoDTM(DBonTable : integer);
var
   Areas,Results,Tiles : tStringList;
   AreaName,BaseFilter,TStr,theFilter,TileStats : shortstring;
   Success,Failures,
   i,j : integer;
   RefALOS_DSM,RefALOS_DTM,AlosName,BaseDir,
   fName,RefDSMName,RefDTMName,CopNAME,FathomName,cName,tName : PathStr;
begin
  {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} HighLightInDebugFile('MakeCSVforGDEMcompareDSMtoDTM in ' + GISdb[dbOnTable].dbName); {$EndIf}
  if not (GISdb[DBonTable].MyData.FieldExists('DEM_PAIR') and GISdb[DBonTable].MyData.FieldExists('REF_DEM')) then begin
      if AnswerIsYes('DB requires fields DEM_PAIR and REF_DEM; add now') then IdentifyDSM_DTMpair(DBonTable)
      else exit;
   end;
   try
       SetColorForProcessing;
       WantShowProgress := false;
       HeavyDutyProcessing := true;
       if (MDDef.DEMIX_BaseDir = '') then MDDef.DEMIX_BaseDir := ExtractFilePath(GISdb[DBonTable].dbFullName);
       BaseFilter := 'DEM_PAIR=' + QuotedStr('YES') + ' AND REF_DEM=' + QuotedStr('DTM') ;
       GISdb[DBonTable].ApplyGISFilter(BaseFilter );
       Areas := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('AREA');
       BaseDir := ExtractFilePath(GISdb[DBonTable].DBFullName) + 'aa_gdem_compare_dsm_dtm\';
       //SafeMakeDir(fName);
       for I := 0 to pred(Areas.Count) do begin
          AreaName := Areas.Strings[i];
          TStr := IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName;
          wmDEM.SetPanelText(1,TStr,true);
          fName := BaseDir + AreaName + '_gdem_compare_dsm_dtm.csv';
          if FileExists(fName) then begin
              {$IfDef RecordRecordDTM_DSM} WriteLineToDebugFile(IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName + '  exists already'); {$EndIf}
          end
          else begin
              Results := nil;
              theFilter := AddAndIfNeeded(BaseFilter) + 'AREA=' + QuotedStr(AreaName);
              {$IfDef RecordDTM_DSM} WriteLineToDebugFile(IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName + '  filter=' + theFilter); {$EndIf}
              GISdb[DBonTable].ApplyGISFilter(theFilter);
              Success := 0;
              Failures := 0;
              j := 0;
              while not GISdb[DBonTable].MyData.eof do begin
                  inc(j);
                  wmDEM.SetPanelText(0,TimeToStr(Now),true);
                  cname := ExtractFilePath(GISdb[DBonTable].DBFullName) + AreaName + '\' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DSM_NAME') + RefTestStr +'\';
                  RefDSMName := cName + 'ref_dsm_cop.tif';
                  if not FileExists(RefDSMname) then RefDSMName := cName + 'ref_dsm_srtm.tif';;

                  RefALOS_DSM := cName + 'ref_dsm_alos.tif';

                  cname := ExtractFilePath(GISdb[DBonTable].DBFullName) + AreaName + '\' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DTM_NAME') + RefTestStr +'\';
                  RefDTMName := cName + 'ref_dtm_cop.tif';
                  if not FileExists(RefDTMname) then RefDTMName := cName + 'ref_dtm_srtm.tif';;
                  RefALOS_DTM := cName + 'ref_dtm_alos.tif';

                  CopName := cName + 'cop.tif';
                  ALOSname := CName + 'alos.tif';
                  Fathomname := cName + 'fathom.tif';

                  if FileExists(RefDSMName) and FileExists(RefDTMname) and FileExists(CopName) then begin
                      TStr := IntToStr(j) + '/' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ' ' + ExtractFileNameNoExt(RefDTMname);
                      wmDEM.SetPanelText(2,TStr,true);
                      inc(Success);
                      {$IfDef RecordDTM_DSM} WriteLineToDebugFile(AreaName + ' ' + TStr); {$EndIf}
                      TileStats := CommonTileStats(dbOnTable);
                      MakeSingleAreaDSMDTMcomparison(false,CopName,RefDSMname,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,Copname,RefDTMName,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,FathomName,RefDSMname,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,FathomName,RefDTMName,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,RefDSMname,RefDTMName,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,ALOSName,RefALOS_DSM,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                      MakeSingleAreaDSMDTMcomparison(false,ALOSName,RefALOS_DTM,'',AreaName,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'),TileStats,Results);
                  end
                  else begin
                     inc(Failures);
                     TStr := 'Files missing: ';
                     if not FileExists(RefDSMName) then TStr := TStr + RefDSMName + '  ';
                     if not FileExists(RefDTMName) then TStr := TStr + RefDTMName + '  ';
                     if not FileExists(CopName) then TStr := TStr + CopName + '  ';
                     HighLightInDebugFile(TStr);
                  end;
                  GISdb[DBonTable].MyData.Next;
              end {while};
              if (Results <> nil) and (Results.Count > 1) then begin
                 Results.SaveToFile(fName);
                 Results.Destroy;
              end;
             {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} WriteLineToDebugFile(AreaName + '  good=' + IntToStr(Success) + '  fail=' + IntToStr(Failures)); {$EndIf}
          end;
       end;
   finally
       wmDEM.ClearStatusBarPanelText;
       GISdb[DBonTable].ClearGISFilter;
       SetColorForWaiting;
       ShowDefaultCursor;
       HeavyDutyProcessing := false;
   end;
   {$If Defined(RecordDEMIX) or Defined(RecordDTM_DSM)} WriteLineToDebugFile('MakeCSVforGDEMcompareDSMtoDTM out, saved ' + fName); {$EndIf}
end;


procedure ChangeDBtoPrivateVersionForMultithreading(var db : integer);
var
   fName : PathStr;
begin
   fName := NextFileNumber(MDtempdir,'multithread_working_db_','.dbf');
   GISdb[db].SaveCurrentDBaseSubset(fName);
   OpenNumberedGISDataBase(db,fName,false);
end;


procedure MakeCSVforHRDEMComparison(DB : integer);
//input the tile stats database
var
   Areas,Results : tStringList;
   AreaName,BaseFilter,TStr,theFilter,TileStats,Common,Country,IncomingFilter : shortstring;
   i,j,inDB : integer;
   fName,DSMName,DTMName,AreaLock,BaseDir : PathStr;
begin
   {$IfDef RecordDEMIX} HighLightInDebugFile('MakeCSVforHRDEMComparison in ' + GISdb[db].DBName); {$EndIf}
   if not(GISdb[db].MyData.FieldExists('DEM_PAIR') and GISdb[db].MyData.FieldExists('REF_DEM')) then begin
       IdentifyDSM_DTMpair(DB);
   end;
   if GISdb[db].MyData.FieldExists('DEM_PAIR') and GISdb[db].MyData.FieldExists('REF_DEM') then begin
       inDB := DB;
       WantShowProgress := false;
       HeavyDutyProcessing := true;
       SetColorForProcessing;
       IncomingFilter := GISdb[db].MyData.Filter;
       BaseFilter := AddAndIfNeeded(IncomingFilter) + 'DEM_PAIR=' + QuotedStr('YES') + ' AND REF_DEM=' + QuotedStr('DTM') ;
       GISdb[db].ApplyGISFilter(BaseFilter );
       Areas := GISdb[db].MyData.ListUniqueEntriesInDB('AREA');
       BaseDir := ExtractFilePath(GISdb[db].DBFullName) + 'aa_hrdem_compare_dsm_dtm\';
       SafeMakeDir(BaseDir);
       {$IfDef RecordDEMIX} HighLightInDebugFile('Areas=' + IntToStr(Areas.Count)); {$EndIf}
       if (Areas.Count = 0) then begin
          MessageToContinue('No areas in ' + GISdb[db].dbName);
       end
       else begin
           ChangeDBtoPrivateVersionForMultithreading(db);
           for I := pred(Areas.Count) downto 0 do begin
              AreaName := Areas.Strings[i];
              fName := BaseDir + AreaName + '_compare_dsm_dtm' + {CurrentTimeForFileName(false) +} '.csv';
              if FileExists(fName) then Areas.Delete(i);
           end;
           for I := 0 to pred(Areas.Count) do begin
              wmDEM.SetPanelText(0,TimeToStr(Now) + ' HRDEM',true);
              AreaName := Areas.Strings[i];
              fName := BaseDir + AreaName + '_compare_dsm_dtm' + '.csv';
              AreaLock := ExtractFilePath(GISdb[indb].DBFullName) + AreaName + '\aa_arealocked.txt';
              if not FileExists(AreaLock) then begin
                Areas.SaveToFile(AreaLock);
                if not FileExists(fName) then begin
                  TStr := IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + ' ' + AreaName;
                  wmDEM.SetPanelText(1,TStr,true);
                  theFilter := AddAndIfNeeded(BaseFilter) + 'AREA=' + QuotedStr(AreaName);
                  {$IfDef RecordDTM_DSM} WriteLineToDebugFile(TStr + '  creating with ' + theFilter); {$EndIf}
                  GISdb[db].ApplyGISFilter(theFilter);
                  if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
                      Results := nil;
                      j := 0;
                      while not GISdb[db].MyData.eof do begin
                          wmDEM.SetPanelText(0,TimeToStr(Now),true);
                          inc(j);
                          Country := UpperCase(GISdb[db].MyData.GetFieldByNameAsString('COUNTRY'));

                          Common := ExtractFilePath(GISdb[inDB].DBFullName) + AreaName + '\wgs_egm\';
                          DSMName := Common + GISdb[db].MyData.GetFieldByNameAsString('DSM_NAME') + '.tif';
                          DTMName := Common + GISdb[db].MyData.GetFieldByNameAsString('DTM_NAME') + '.tif';

                          TStr := IntToStr(j) + '/' + IntToStr(GISdb[db].MyData.FiltRecsInDB) + ' ' + ExtractFileNameNoExt(DTMname);
                          wmDEM.SetPanelText(2,TStr,true);
                          {$IfDef RecordDTM_DSM} WriteLineToDebugFile(AreaName + ' ' + TStr); {$EndIf}
                          MakeSingleAreaDSMDTMcomparison(true,DSMname,DTMName,'',AreaName,Country,CommonTileStats(db),Results);
                          GISdb[db].MyData.Next;
                      end {while};
                     {$IfDef RecordDTM_DSM} if Results <> Nil then WriteLineToDebugFile('Tiles=' + IntToStr(Results.Count) + '  ' + fName); {$EndIf}
                     Results.SaveToFile(fName);
                     Results.Destroy;
                  end;
                  DeleteFileIfExists(AreaLock);
                //end;
              end;
           end;
       end;
       CloseSingleDB(db);
       wmDEM.ClearStatusBarPanelText;
       GISdb[indb].ApplyGISFilter(IncomingFilter);
       SetColorForWaiting;
       HeavyDutyProcessing := false;
       ShowDefaultCursor;
       {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeCSVforHRDEMComparison out'); {$EndIf}
     end
   end
   else begin
      MessageToContinue('DB missing required fields DEM_PAIR and REF_DEM');
      {$IfDef RecordDEMIX} WriteLineToDebugFile('MakeCSVforHRDEMComparison fail, missing fields'); {$EndIf}
   end;
end;

procedure ComputePowerFitForAllTiles(db : integer; PowerLaw : boolean = true);
//input is GDEM or HRDEM DB for grid spacings in meters or arc seconds; uses only those spacings currently
//makes DB with statistics for a power law fit for the DEMs hard coded into theDEMs string list
var
   TileList,Results,theDEMs : tStringList;
   DTMName,aline,BaseFilter,SpacingField,TileField,ResName,TStr,af,aDEM : shortstring;
   fName : PathStr;
   i,j : integer;

         procedure OneKindDEM(theDEM : shortString);
         var
            x,y : array[0..25] of float32;
            a,b,siga,sigb,r : float64;
            n : integer;
         begin
            n := -1;
            GISdb[db].MyData.First;
            while not GISdb[db].MyData.eof do begin
               inc(n);
               x[n] := GISdb[db].MyData.GetFieldByNameAsFloat(SpacingField);
               y[n] := GISdb[db].MyData.GetFieldByNameAsFloat(theDEM);
               GISdb[db].MyData.Next;
            end;
            if PowerLaw then begin
               PowerFit(x,y,n,a,b,siga,sigb,r);
               aline := aLine + ',' + RealToString(sqr(r),-12,-4) + ',' + RealToString(a,-12,-4) + ',' + RealToString(b,-12,-4);
            end
            else begin
               LinearFit(x,y,n,a,b,siga,sigb,r);
               aline := aLine + ',' + RealToString(sqr(r),-12,-4) + ',' + RealToString(a,-12,-4) + ',' + RealToString(b,-12,-4);
            end;
         end;

begin {procedure ComputePowerFitForAllTiles}
   if (GISdb[db].MyData.FieldExists('GRID_M') or GISdb[db].MyData.FieldExists('GRID_SEC')) then begin
      {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('ComputePowerFitForAllTiles in'); {$EndIf}
       theDEMs := CreatePowerLawList(db);
       if GISdb[db].MyData.FieldExists('GRID_M') then begin
          SpacingField := 'GRID_M';
          ResName := 'HRDEM';
          TileField := 'DTM_NAME';
          aLine := 'DTM_NAME,DSM_NAME,';
       end
       else if GISdb[db].MyData.FieldExists('GRID_SEC') then begin
          SpacingField := 'GRID_SEC';
          ResName := 'GDEM';
          TileField := 'DEMIX_TILE';
          aLine := 'DEMIX_TILE,';
       end;
       aline := aLine + 'AREA,COUNTRY,LAT,LONG,' + CommonTileStatsNames(DB);
       if PowerLaw then begin
          for i := 0 to pred(theDEMs.Count) do begin
             aLine := aline + ',' + theDEMs.strings[i] + '_R2'  + ',' + theDEMs.strings[i] + '_A'  + ',' + theDEMs.strings[i] + '_B';
          end;
       end
       else begin
          for i := 0 to pred(theDEMs.Count) do begin
             aLine := aline + ',' + theDEMs.strings[i] + '_R2'  + ',' + theDEMs.strings[i] + '_B'  + ',' + theDEMs.strings[i] + '_M';
          end;
       end;
       Results := tStringList.Create;
       Results.Add(aLine);

       SetColorForProcessing;
       BaseFilter := GISdb[db].MyData.Filter;
       GISdb[db].EmpSource.Enabled := false;
       TileList := GISdb[db].MyData.ListUniqueEntriesInDB(TileField);
       for I := 0 to pred(TileList.Count) do begin
          DTMname := TileList.Strings[i];
          wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(TileList.Count) + ' ' + DTMname,true);
          af := AddAndIfNeeded(BaseFilter) + TileField + '=' + QuotedStr(DTMname) + HRDEMresolutionFilter(db);
          GISdb[db].ApplyGISFilter(af);
          GISdb[db].EmpSource.Enabled := false;
          if (TileField = 'DTM_NAME') then TStr := GISdb[db].MyData.GetFieldByNameAsString('DSM_NAME')  + ','
          else TStr := '';
          aLine := DTMName + ',' + TStr + GISdb[db].MyData.GetFieldByNameAsString('AREA') + ',' + GISdb[db].MyData.GetFieldByNameAsString('COUNTRY') + ',' +
             GISdb[db].MyData.GetFieldByNameAsString('LAT')  + ',' + GISdb[db].MyData.GetFieldByNameAsString('LONG') + ',' + CommonTileStats(db);
          for j := 0 to pred(theDEMs.Count) do begin
             aDEM := theDEMs.Strings[j];
             if not GISdb[db].MyData.FieldExists(aDEM) then aDEM := aDEM + '_SLOPE';   //for the HRDEM case
             OneKindDEM(aDEM);
          end;
          Results.Add(aLine);
       end;
       theDEMs.Destroy;
       if PowerLaw then TStr := 'power_law_' else TStr := 'linear_fit_';
       fName := ExtractFilePath(GISdb[db].dbFullName) + ResName + '_' + TStr + 'DEMIX_DB_4.3_' + CurrentTimeForFileName(false) + '.dbf';
       Results.SaveToFile(MDtempDir + 'power_law_fit_results.csv');
       StringList2CSVtoDB(Results,fName);
       wmdem.ClearStatusBarPanelText;
       GISdb[db].ApplyGISFilter(BaseFilter);
       SetColorForWaiting;
       {$IfDef RecordCompareDSMandDTM} WriteLineToDebugFile('ComputePowerFitForAllTiles out'); {$EndIf}
   end
   else begin
      MessageToContinue('Required spacing field missing');
   end;
end {procedure ComputePowerFitForAllTiles};


const
   DoLandTypes = false;
   DoDSM = false;

procedure MakeDBForParamStats(Option,DBonTable : integer);
var
   Results,UsingFields,UniqueEntries : tStringList;
   i,j : integer;
   aLine,MaxLine,MeanLine,MedianLine,MinLine,stdLine,Common : shortstring;
   MomentVar : tMomentVar;
   aField : shortstring;
   fName : PathStr;
   Color : tColor;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   if (Option = opByCluster) then aField := 'CLUSTER'
   else if (Option = opByDEM) then aField := 'DEM';

   if not GISdb[DBonTable].MyData.FieldExists(aField) then begin
      MessageToContinue('Option requires field in DB:' + aField);
      exit;
   end;

   if (Option = opByCluster) then begin
      UniqueEntries := GISdb[DBonTable].MyData.ListUniqueEntriesInDB(aField);
      SortStringListNumerically(UniqueEntries);
   end
   else if (Option = opByDEM) then begin
      UniqueEntries := GISdb[DBonTable].MyData.ListUniqueEntriesInDB(aField);
   end;

   UsingFields := GISdb[DBonTable].GetAnalysisFields;

   Results := tStringList.Create;
   aline := aField + ',COLOR,N,METRIC';
   for j := 0 to pred(UsingFields.Count) do begin
      aline := aline + ',' + UsingFields.Strings[j];
   end;
   Results.Add(aLine);
   for i := 0 to pred(UniqueEntries.Count) do begin
      if Option = opByCluster then begin
         GISdb[DBonTable].MyData.ApplyFilter(aField + '=' + UniqueEntries.Strings[i]);
         Color := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('COLOR');
      end;
      if Option = opByDEM then begin
         GISdb[DBonTable].MyData.ApplyFilter(aField + '=' + QuotedStr(UniqueEntries.Strings[i]));
         Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(UniqueEntries.Strings[i]));
      end;
      GISdb[DBonTable].Empsource.Enabled := false;
      Common := UniqueEntries.Strings[i] + ',' + IntToStr(Color) + ','  + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ',' ;
      MaxLine := Common + 'MAX' ;
      MeanLine := Common + 'MEAN' ;
      MedianLine := Common + 'MEDIAN';
      MinLine := Common + 'MIN';
      STDline := Common + 'StdDev';

      for j := 0 to pred(UsingFields.Count) do begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         MomentVar := GISdb[DBonTable].GetFieldStatistics(UsingFields.Strings[j]);
         GISdb[DBonTable].Empsource.Enabled := false;
         MaxLine := MaxLine + ',' + RealToString(MomentVar.MaxZ,-18,-4);
         MeanLine := MeanLine + ',' + RealToString(MomentVar.Mean,-18,-6);
         MedianLine := MedianLine + ','  + RealToString(MomentVar.Median,-18,-6);
         MinLine := MinLine + ',' +  RealToString(MomentVar.MinZ,-18,-4);
         STDLine := STDLine + ',' +  RealToString(MomentVar.std_dev,-18,-6);
      end;
      Results.Add(MaxLine);
      Results.Add(MeanLine);
      Results.Add(MedianLine);
      Results.Add(MinLine);
      Results.Add(STDLine);
   end;
   fName := Petmar.NextFileNumber(MDTempDir, 'stats_by_' + aField + '_' ,DefaultDBExt);
   {$IfDef RecordClustering} WriteLineToDebugFile(fName); {$EndIf}
   GISdb[DBonTable].theMapOwner.StringListToDBonMap(Results,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;


procedure DEMIX_SSIM_FUV_transpose_kmeans_new_db(DBonTable : integer);
//transpose DB so it has one TILE/DEM pair on each line, Criteria as columns
var
   values : array[1..9,0..25] of float32;
   Tile,Crit,aline : shortstring;
   i,j,k,N : integer;
   theTiles,sl,Criteria : tStringList;
   fName: PathStr;
begin
   {$If Defined(RecordDEMIX)} HighLightInDebugFile('DEMIX_SSIM_FUV_transpose_kmeans_new_db in'); {$EndIf}
   try
      if GISdb[DBonTable].MyData.Filtered then begin
         if not AnswerIsYes('Transpose filtered DB') then GISdb[DBonTable].ClearGISFilter;
      end;

      GetDEMIXpaths;
      GISdb[DBonTable].EmpSource.Enabled := false;
      theTiles := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
      GISdb[DBonTable].EmpSource.Enabled := false;
      Criteria := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('CRITERION');
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('   tiles=' + IntToStr(TheTiles.Count)); {$EndIf}
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('   criteria=' + IntToStr(Criteria.Count)); {$EndIf}

      sl := tStringList.Create;
      aline := 'DEMIX_TILE,DEM';
      for I := 0 to pred(Criteria.Count) do aline := aline + ',' + Criteria.Strings[i];
      sl.Add(Aline);

      LoadDEMIXnames;
      StartProgress('SSIM/FUV summary');
      for k := 0 to pred(theTiles.Count) do begin
         if (k mod 10 = 0) then wmdem.SetPanelText(1,IntToStr(k) + '/' + IntToStr(TheTiles.Count) + ' tiles transpose');
         Tile := theTiles[k];
         GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tile));
         GISdb[DBonTable].EmpSource.Enabled := false;
         if GISdb[DBonTable].MyData.FiltRecsInDB > 5 then begin
            for i := 1 to 9 do
               for j := 0 to 25 do
                  values[i,j] := -999;
            while not GISdb[DBonTable].MyData.eof do begin
               Crit := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION'));
               N := Criteria.IndexOf(Crit);
               for I := 1 to NumDEMIXtestDEM do begin
                  Values[i,N] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
               end;
               GISdb[DBonTable].MyData.Next;
            end;

            for I := 1 to NumDEMIXtestDEM do begin
               aline := Tile + ',' + DEMIXShort[i];
               for j := 0 to  pred(Criteria.Count) do aline := aline + ',' + RealToString(Values[i,j],-18,6);
               sl.Add(Aline);
            end;
         end;
      end;
      fName := DEMIX_final_DB_dir + 'transpose_' + GISdb[DBonTable].dbName + '.csv';  //NextFileNumber(MDTempDir,MDTempDir + 'ssim_FUV_clustering_','.dbf');
      StringList2CSVtoDB(sl,fName);
   finally
      EndDEMIXProcessing;
      GISdb[DBonTable].ClearGISFilter;
   end;
   {$If Defined(RecordDEMIX)} HighLightInDebugFile('DEMIX_SSIM_FUV_transpose_kmeans_new_db out, ' + fName); {$EndIf}
end;


procedure ComputeDEMIX_Diff_Dist_tile_stats(Overwrite : boolean; AreasWanted : tStringList = Nil);
//a number of experimental options for criteria are disabled, and would require changes to the code to get working again
//this has one DEM per record, with all the criteria
//in hindsight, it might have been better to create this is the desired final format, as is done for the SSIM and FUV criteria;
//but this works, so why change it?
//also, if we want to try clustering the tiles, we need it this way
const
   MaxRefStore = 100;
var
   MomentVar,FlatMomentVar,SteepMomentVar,GentleMomentVar,CliffMomentVar,ForestMomentVar,UrbanMomentVar,BarrenMomentVar,WaterMomentVar : tMomentVar;
   NumPits,NumPeaks : array[1..MaxRefStore] of integer;
   ElevDiffMap,SlopeDiffMap,RuffDiffMap,
   DEMIXtileDB,LandCoverGrid : integer;
   bbgrid : tGridLimits;
   fName : PathStr;
   NewDEMName : shortstring;
   AreaName,DEMIXtile,LandTypeMask,TileHeader : shortstring;
   LatCent,LongCent,
   GridFull,Lat,Long : float64;
   zRef : float32;
   TileStats,
   ElevDiffStats,
   RufDiffStats,
   SlopeDiffStats  : tStringList;
   zs : ^bfarray32;
   {$IfDef AllowLandTypes} zsSteep,ZSFlat,zsGentle,zsCliff,zsForest,zsUrban,zsBarren,zsWater : ^bfarray32; {$EndIf}
   SlopeAsp,SlopeAspectTest : tSlopeAspectRec;
   ErrorLog : tStringList;
   ElevFiles,LegendFiles : tStringList;

   {$IfDef ExperimentalDEMIXcriteria} {$I experiment_demix_criteria.inc}  {$EndIf}


         procedure OpenDataStructures;
         begin
            new(zs);
            {$IfDef AllowLandTypes}
                if DoLandTypes then begin
                   New(zssteep);
                   New(zsflat);
                   New(zsGentle);
                   New(zsCliff);
                   if ValidDEM(LandCoverGrid) then begin
                      New(zsForest);
                      New(zsUrban);
                      New(zsBarren);
                      New(zsWater);
                   end;
                end;
            {$EndIf}
            {$IfDef RecordDEMIX} WriteLineToDebugFile(AreaName + '  ProcessDEMIXtestarea in, zs created'); {$EndIf}
            if ElevDiffHists then begin
               ElevFiles := tStringList.Create;
               LegendFiles := tStringList.Create;
            end;
         end;

         procedure CloseDataStructures;
         begin
            Dispose(zs);
            {$IfDef AllowLandTypes}
                if DoLandTypes then begin
                   Dispose(zssteep);
                   Dispose(zsflat);
                   Dispose(zsGentle);
                   Dispose(zsCliff);
                   if ValidDEM(LandCoverGrid) then begin
                      Dispose(zsForest);
                      Dispose(zsUrban);
                      Dispose(zsBarren);
                      Dispose(zsWater);
                   end;
                end;
            {$EndIf}
            if ElevDiffHists then begin
               ElevFiles.Destroy;
               LegendFiles.Destroy;
            end;
         end;


         procedure InitializeStringLists;
         {$IfDef ExperimentalDEMIXcriteria}
            const
               ElevMomentsString = 'ELEV_MIN,ELEV_Max,ELEV_Mean,ELEV_ADEV,ELEV_STD,ELEV_SKEW,ELEV_KURT,ELEV_MED,N';
               SlopeMomentsString = 'SLP_MIN,SLP_Max,SLP_Mean,SLP_ADEV,SLP_STDV,SLP_SKEW,SLP_KURT,SLP_MED,SLP_N';

               SlopeMomentsDiffString = 'SDIFF_MIN,SDIFF_Max,SDIFF_Mean,SDIFF_STD,SDIFF_SKEW,SDIFF_KURT,SDIFF_MED,SDIFF_N';
               ElevMomentsDiffString = 'EDIFF_MIN,EDIFF_Max,EDIFF_Mean,EDIFF_STD,EDIFF_SKEW,EDIFF_KURT,EDIFF_MED,EDIFF_N';
         {$EndIf}
         begin
            TileStats := tStringList.Create;
            TileStats.Add(TileStatsString);

            ElevDiffStats := tStringList.Create;
            ElevDiffStats.Add(TileStatsString + AreaString + ElevDiffStatsString);

            RufDiffStats := tStringList.Create;
            RufDiffStats.Add(TileStatsString + AreaString + RufDiffStatsString);

            SlopeDiffStats := tStringList.Create;
            SlopeDiffStats.Add(TileStatsString + AreaString + SlopeDiffStatsString);

            {$IfDef ExperimentalDEMIXcriteria} CreateExperimentalStringLists; {$EndIf}

            {$IfDef RecordDEMIXFull} WriteLineToDebugFile('InitializeStringLists out, string lists created'); {$EndIf}
         end;


         function LineHeader(DEM : integer; RefType : shortstring) : shortstring;
         var
            t1 : shortstring;
         begin
             if ValidDB(NewFormatDEMIXDB) then t1 := NewDEMName
             else begin
                if (DEM = 0) then t1 := 'xxx' else t1 := WhatTestDEMisThis(DEMGlb[DEM].AreaName);
             end;
             Result := t1 + ',' + RefType + ',' + LandTypeMask + ',';
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
                  fName := Diff_dist_results_dir + AreaName + fname + '.csv';
                  if (sl.count > 1) then begin
                     {$IfDef RecordComputeDEMIX_Diff_Dist} WriteLineToDebugFile(fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                     sl.SaveToFile(fName);
                  end
                  else begin
                     {$IfDef RecordComputeDEMIX_Diff_Dist} HighLightInDebugFile(fName + ' has no lines'); {$EndIf}
                     fName := '';
                  end;
                  sl.Destroy;
               end;

         begin
            {$IfDef RecordComputeDEMIX_Diff_Dist} WriteLineToDebugFile('DEMIX start string list processing'); {$EndIf}
            ProcessStringList('_DEMIX_tiles_used',TileStats);
            ProcessStringList('_elev_diff_stats',ElevDiffStats,true);
            ProcessStringList('_ruff_diff_stats',RufDiffStats,true);
            ProcessStringList('_slope_diff_stats',SlopeDiffStats,true);
            {$IfDef ExperimentalDEMIXcriteria} ProcessExperimentalStringLists; {$EndIf}
         end;


           procedure CompareDifferencesToReferenceDEM(aTestDEM,RefDEM : integer; RefType : shortstring; bbgrid : tGridLimits);
           //do full DEM pixel by pixel comparison to reference DEM
           var
              NumRuff,NumSlope,NumElev,
              xg,yg : integer;
              TStr : shortstring;
              z,Difference : float32;
              zs : ^bfarray32;

                  procedure LogError(TStr : shortstring);
                  begin
                     {$If Defined(RecordFullDEMIX) or Defined(RecordGridCompare)} WriteLineToDebugFile(TStr); {$EndIf}
                     ErrorLog.Add(TStr);
                  end;

                  procedure ZeroMomentVar;
                  begin
                     InitializeMomentVar(MomentVar);
                     {$IfDef AllowLandTypes}
                         if DoLandTypes then begin
                            InitializeMomentVar(FlatMomentVar);
                            InitializeMomentVar(GentleMomentVar);
                            InitializeMomentVar(SteepMomentVar);
                            InitializeMomentVar(CliffMomentVar);
                            InitializeMomentVar(UrbanMomentVar);
                            InitializeMomentVar(ForestMomentVar);
                            InitializeMomentVar(BarrenMomentVar);
                            InitializeMomentVar(WaterMomentVar);
                         end;
                     {$EndIf}
                  end;


                  function MomentDifferenceString(aMomentVar,RefMomentVar : tMomentVar) : shortstring;
                  begin
                      Result := RealToString(aMomentVar.MinZ-RefMomentVar.minZ,-8,2) + ',' + RealToString(aMomentVar.MaxZ-RefMomentVar.MaxZ,-8,2) + ',' +
                                     RealToString(aMomentVar.Mean-RefMomentVar.mean,-8,2) + ',' + RealToString(aMomentVar.std_dev-RefMomentVar.std_dev,-8,2) + ',' +
                                     RealToString(aMomentVar.skew-RefMomentVar.skew,-8,2) + ',' + RealToString(aMomentVar.curt-RefMomentVar.Curt,-8,2) + ',' +
                                     RealToString(aMomentVar.median-RefMomentVar.median,-8,2) + ',' + IntToStr(aMomentVar.NPts);
                  end;


                 function WriteDifferenceResults(aTestDEM,RefDEM : integer; RefType : shortstring; var WhichStats : tStringList) : integer;

                           procedure WriteDifferenceResult(ltMask : shortString; var sl : tStringList; theTestDEM,RefDEM : integer; var thezs : bfarray32; var theMoments : tMomentVar; Percent : float32);
                           var
                              i : integer;
                              aLine : shortstring;
                           begin
                              if (theMoments.NPts > MDDef.LandTypePointsNeeded) then begin
                                 LandTypeMask := ltMask;
                                 moment(thezs,theMoments,msAll);
                                 for i := 0 to pred(theMoments.NPts) do thezs[i] := abs(thezs[i]);
                                 //theMoments.LE90 := Percentile(90,thezs,theMoments.NPts,false);
                                 aline := TileHeader + LineHeader(aTestDEM,RefType)  + RealToString(Percent,-12,-2) + ',' + DEMIXMomentStatsString(theMoments);
                                 sl.Add(aLine);
                                 inc(Result);
                                 {$If Defined(RecordNeoDEMIX)} WriteLineToDebugFile(aLine); {$EndIf}
                              end
                              else begin
                                 {$If Defined(RecordGridCompare)} WriteLineToDebugFile('WriteDifferenceResult failed for ' + LandTypeMask + ' theMoments.NPts=' + IntToStr(theMoments.NPts) ); {$EndIf}
                              end;
                           end;

                 begin {WriteDifferenceResults}
                     Result := 0;
                     WriteDifferenceResult('ALL',WhichStats,aTestDEM,RefDEM,zs^,MomentVar,100);
                     {$IfDef AllowLandTypes}
                       if DoLandTypes then begin
                          WriteDifferenceResult('FLAT',WhichStats,aTestDEM,RefDEM,zsFlat^,FlatMomentVar,(100 * FlatMomentVar.NPts/MomentVar.NPts));
                          WriteDifferenceResult('GENTLE',WhichStats,aTestDEM,RefDEM,zsGentle^,GentleMomentVar,(100 * GentleMomentVar.NPts/MomentVar.NPts));
                          WriteDifferenceResult('STEEP',WhichStats,aTestDEM,RefDEM,zsSteep^,SteepMomentVar,(100 * SteepMomentVar.NPts/MomentVar.NPts));
                          WriteDifferenceResult('CLIFF',WhichStats,aTestDEM,RefDEM,zsCliff^,CliffMomentVar,(100 * CliffMomentVar.NPts/MomentVar.NPts));

                          if (LandCoverGrid <> 0) then begin
                             WriteDifferenceResult('FOREST',WhichStats,aTestDEM,RefDEM,zsForest^,ForestMomentVar,(100 * ForestMomentVar.NPts/MomentVar.NPts));
                             WriteDifferenceResult('URBAN',WhichStats,aTestDEM,RefDEM,zsUrban^,UrbanMomentVar,(100 * UrbanMomentVar.NPts/MomentVar.NPts));
                             WriteDifferenceResult('BARREN',WhichStats,aTestDEM,RefDEM,zsBarren^,BarrenMomentVar,(100 * BarrenMomentVar.NPts/MomentVar.NPts));
                             WriteDifferenceResult('WATER',WhichStats,aTestDEM,RefDEM,zsWater^,WaterMomentVar,(100 * WaterMomentVar.NPts/MomentVar.NPts));
                          end;
                       end;
                     {$EndIf}
                     LandTypeMask := 'ALL';
                 end {WriteDifferenceResults};

            {$IfDef UseOldDifferenceDistributions}

                  {$IfDef AllowLandTypes}
                      procedure LandCover(LandCoverGrid : integer; Lat,Long : float32; Value : float32);
                      //hard coded for a particular land cover data set, LC100 from Copernicus
                      var
                         slc : integer;
                      begin
                         slc := SimplifiedLandCover(LandCoverGrid,Lat,Long,Value);
                         if slc = slcForest then begin
                            zsForest^[ForestMomentVar.NPts] := Value;
                            inc(ForestMomentVar.Npts);
                         end
                         else if slc = slcBarren then begin
                            zsBarren^[BarrenMomentVar.NPts] := Value;
                            inc(BarrenMomentVar.Npts);
                         end
                         else if slc = slcUrban then begin
                            zsUrban^[UrbanMomentVar.NPts] := Value;
                            inc(UrbanMomentVar.Npts);
                         end
                         else if slc = slcWater then begin
                            zsWater^[WaterMomentVar.NPts] := Value;
                            inc(WaterMomentVar.Npts);
                         end;
                      end;
                  {$EndIf}


                  procedure LandTypeDiff(Difference : float32);
                  begin
                     zs^[MomentVar.NPts] := Difference;
                     inc(MomentVar.Npts);
                     {$IfDef AllowLandTypes}
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
                     {$EndIf}
                  end;

                  function DoElevations(aTestDEM,Ref : integer) : boolean;
                  var
                     Col,Row : integer;
                  begin
                     ZeroMomentVar;
                     {$If Defined(RecordDEMIXFull) or Defined(RecordNeoDEMIX)} WriteLineToDebugFile('DoElev: ' + GridLimitsToString(bbgrid) + ' test=' + DEMglb[aTestDEM].AreaName + ' ref=' + DEMglb[RefDEM].AreaName); {$EndIf}
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[aTestDEM].GetElevMetersOnGrid(col,row,z) then begin
                              DEMGlb[aTestDEM].DEMGridToLatLongDegree(col,row,lat,long);
                              DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                              if DEMGlb[RefDEM].GetElevMetersOnGrid(xg,yg,zref) then begin
                                 Difference := z-zref;
                                 inc(MomentVar.NPts);
                                 zs^[MomentVar.NPts] := Difference;
                                 {$IfDef AllowLandTypes}
                                   if DoLandTypes and DEMGlb[RefDEM].GetSlopeAndAspect(MDDef.SlopeCompute,xg,yg,SlopeAsp) then begin
                                      LandTypeDiff(Difference);
                                      LandCover(LandCoverGrid,Lat,long,Difference);
                                   end;
                                 {$EndIf}
                              end;
                           end;
                        end;
                     end;
                     moment(zs^,MomentVar,msAll);
                     if (MomentVar.NPts > 0) and ((MomentVar.Maxz - MomentVar.Minz) > 1) then begin
                        if ElevDiffHists then begin
                           fName := DEMIX_diff_dist + DEMGLB[aTestDEM].AreaName + '_' + DEMIXtile + '_elev_to_' + RefType  + '.z';
                           ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zs^,fName));
                           LegendFiles.Add(ExtractFileNameNoExt(fName));
                        end;
                        NumElev := WriteDifferenceResults(aTestDEM,RefDEM,RefType,ElevDiffStats);
                        Result := true;
                     end
                     else begin
                        LogError('No points for elevation difference ' + DEMGLB[aTestDEM].AreaName);
                        NumElev := 0;
                        Result := false;
                     end;
                  end;

                  procedure DoSlopes(aTestDEM,Ref : integer);
                  var
                     Col,Row,xg,yg : integer;
                  begin
                     {$If Defined(RecordDEMIXFull) or Defined(RecordNeoDEMIX)} WriteLineToDebugFile('DoSlope: ' + GridLimitsToString(bbgrid) + ' test=' + DEMglb[aTestDEM].AreaName + ' ref=' + DEMglb[RefDEM].AreaName); {$EndIf}
                     ZeroMomentVar;
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[aTestDEM].QuickEvansSlopeAndAspect(col,row,SlopeAspectTest) then begin
                              DEMGlb[aTestDEM].DEMGridToLatLongDegree(col,row,lat,long);
                              DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                              if DEMGlb[RefDEM].QuickEvansSlopeAndAspect(xg,yg,SlopeAsp) then begin
                                 Difference := SlopeAspectTest.SlopePercent - SlopeAsp.SlopePercent;
                                 inc(MomentVar.NPts);
                                 zs^[MomentVar.NPts] := Difference;
                                 {$IfDef AllowLandTypes}
                                     if DoLandTypes and DEMGlb[RefDEM].GetSlopeAndAspect(MDDef.DEMIXSlopeCompute,xg,yg,SlopeAsp) then begin
                                        LandTypeDiff(Difference);
                                        LandCover(LandCoverGrid,Lat,Long,Difference);
                                     end;
                                 {$EndIf}
                              end;
                           end;
                        end;
                     end;
                     moment(zs^,MomentVar,msAll);
                     if (MomentVar.NPts > 1) then begin
                        if ElevDiffHists then begin
                           fName := DEMIX_diff_dist + DEMGLB[aTestDEM].AreaName + '_' + DEMIXtile + '_slope_to_' + RefType + '.z';
                           SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                        end;
                        NumSlope := WriteDifferenceResults(aTestDEM,RefDEM,RefType,SlopeDiffStats);
                     end
                     else begin
                        NumSlope := 0;
                        LogError('No points for slope differences ' + DEMGLB[aTestDEM].AreaName);
                     end;
                  end;

                  procedure DoRoughness(aTestDEM,Ref : integer);
                  var
                     Col,Row : integer;
                     Ruff1,Ruff2 : float32;
                  begin
                     {$If Defined(RecordDEMIXFull) or Defined(RecordNeoDEMIX)} WriteLineToDebugFile('DoRuff: ' + GridLimitsToString(bbgrid) + ' test=' + DEMglb[aTestDEM].AreaName + ' ref=' + DEMglb[RefDEM].AreaName); {$EndIf}
                     ZeroMomentVar;
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[aTestDEM].RoughnessFromSlopeSTD(Col,Row,MDDef.RoughnessBox,Ruff1) then begin
                              DEMGlb[aTestDEM].DEMGridToLatLongDegree(col,row,lat,long);
                              DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                              if DEMGlb[RefDEM].RoughnessFromSlopeSTD(xg,yg,MDDef.RoughnessBox,Ruff2) then begin
                                 Difference := Ruff1 - Ruff2;
                                 inc(MomentVar.NPts);
                                 zs^[MomentVar.NPts] := Difference;
                                 {$IfDef AllowLandTypes}
                                     if DoLandTypes and DEMGlb[RefDEM].GetSlopeAndAspect(MDDef.SlopeCompute,xg,yg,SlopeAsp) then begin
                                        LandTypeDiff(Difference);
                                        LandCover(LandCoverGrid,Lat,long,Difference);
                                     end;
                                 {$EndIf}
                              end;
                           end;
                        end;
                     end;
                     moment(zs^,MomentVar,msAll);
                     if (MomentVar.NPts > 1) then begin
                        if ElevDiffHists then begin
                           fName := DEMIX_diff_dist + DEMGLB[aTestDEM].AreaName + '_' + DEMIXtile + '_ruff_to_' + RefType + '.z';
                           SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                        end;
                        NumRuff := WriteDifferenceResults(aTestDEM,RefDEM,RefType,RufDiffStats);
                     end
                     else begin
                        NumRuff := 0;
                        LogError('No points for roughness difference ' + DEMGLB[aTestDEM].AreaName);
                     end;
                  end;

             {$Else}

                 procedure GetDifferences(DiffMap : integer; GridLimits : tGridLimts);
                 var
                    MomentVar : tMomentVar;
                    Min,Max : float32;
                 begin
                    InitializeMomentVar(MomentVar);
                    DEMglb[DiffMap].GetElevationsInLongArray(GridLimits, MomentVar.NPts,zs^,Min,Max);
                    moment(zs^,MomentVar,msAll);
                    CloseSingleDEM(DiffMap);
                 end;
           {$EndIf}

            begin {procedure CompareDifferencesToReferenceDEM}
               {$If Defined(RecordDEMIXFull) or Defined(RecordGridCompare) or Defined(RecordTestDEMstart)}
                  WriteLineToDebugFile('Comparison to reference=' + DEMGLB[RefDEM].AreaName + '  test=' + DEMGLB[aTestDEM].AreaName);
               {$EndIf}
               TStr := 'DEM=' + IntToStr(aTestDEM) + ' '  + RefType;
               wmdem.SetPanelText(3,TStr);
               {$If Defined(RecordTileProcessing) or Defined(RecordDEMIXFull)} WriteLineToDebugFile(TStr); {$EndIf}
               New(zs);

               {$IfDef UseOldDifferenceDistributions}
                   if DoElevations(aTestDEM,RefDEM) then begin
                      DoSlopes(aTestDEM,RefDEM);
                      DoRoughness(aTestDEM,RefDEM);
                   end;
               {$Else}
                  ElevDiffMap := MakeDifferenceMap(aTestDEM,RefDEM,RefDEM,0,false,false,false);
                  GetDifferences(ElevDiffMap,bb);
               {$EndIf}

               Dispose(zs);
            end {procedure CompareDifferencesToReferenceDEM};



   procedure ProcessDEMIXtestarea;


         function GridBoundingBox(DEM,RefDEM : integer; Clip : boolean = false) : tGridLimits;
         var
            bb : sfBoundBox;
         begin
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            if Clip and ValidDEM(RefDEM) then begin
               if bb.xMin < DEMGlb[RefDEM].DEMBoundBoxGeo.xmin then bb.xMin := DEMGlb[RefDEM].DEMBoundBoxGeo.xmin;
               if bb.xMax > DEMGlb[RefDEM].DEMBoundBoxGeo.xmax then bb.xMax := DEMGlb[RefDEM].DEMBoundBoxGeo.xmax;
               if bb.yMin < DEMGlb[RefDEM].DEMBoundBoxGeo.ymin then bb.yMin := DEMGlb[RefDEM].DEMBoundBoxGeo.ymin;
               if bb.yMax > DEMGlb[RefDEM].DEMBoundBoxGeo.ymax then bb.yMax := DEMGlb[RefDEM].DEMBoundBoxGeo.ymax;
            end;
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,Result.xgridlow,Result.ygridlow);
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,Result.xgridhigh,Result.ygridhigh);
         end;


         function UseThisTile : boolean;
         var
            bb : sfBoundBox;
         begin
            GridFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
            Result := GridFull >= MDDef.DEMIX_Tile_Full;
            DEMIXtile := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            LatCent := 0.5 * (bb.ymax + bb.ymin);
            LongCent := 0.5 * (bb.xmax + bb.xmin);
            {$IfDef RecordUseTile} if (not Result) then WriteLineToDebugFile('Not doing tile=' + DEMIXTile + '  fill=' + IntToStr(round(GridFull))); {$EndIf}
         end;


         procedure OpenDataStructures;
         begin
            new(zs);
            {$IfDef AllowLandTypes}
              if DoLandTypes then begin
                 New(zssteep);
                 New(zsflat);
                 New(zsGentle);
                 New(zsCliff);
                 if ValidDEM(LandCoverGrid) then begin
                    New(zsForest);
                    New(zsUrban);
                    New(zsBarren);
                    New(zsWater);
                 end;
              end;
            {$EndIf}
            {$IfDef RecordDEMIXFull} WriteLineToDebugFile(AreaName + '  ProcessDEMIXtestarea in, zs created'); {$EndIf}
            if ElevDiffHists then begin
               ElevFiles := tStringList.Create;
               LegendFiles := tStringList.Create;
            end;
         end;

         procedure CloseDataStructures;
         begin
            Dispose(zs);
            {$IfDef AllowLandTypes}
                if DoLandTypes then begin
                   Dispose(zssteep);
                   Dispose(zsflat);
                   Dispose(zsGentle);
                   Dispose(zsCliff);
                   if ValidDEM(LandCoverGrid) then begin
                      Dispose(zsForest);
                      Dispose(zsUrban);
                      Dispose(zsBarren);
                      Dispose(zsWater);
                   end;
                end;
            {$EndIf}
            if ElevDiffHists then begin
               ElevFiles.Destroy;
               LegendFiles.Destroy;
            end;
         end;

   label
      NoLandCover;
   var
      i,j,UseDSM,UseDTM : integer;
      TStr : shortstring;
      CandidateBoundBoxGeo,bb : sfBoundBox;
   begin {ProcessDEMIXtestarea}
      {$If Defined(RecordDEMIXFull)} HighLightInDebugFile('ProcessDEMIXtestarea in ' + TestAreaName); {$EndIf}
      ReportErrors := false;
      DEMIXtileDB := 0;
      LandCoverGrid := 0;
      wmdem.SetPanelText(3,'Load reference DEMs',true);

//Change this for the new method used for FUV
      if LoadDEMIXReferenceDEMs(AreaName,DEMIXRefDEM,DEMIXOpenMap) then begin
         if LoadDEMIXCandidateDEMs(AreaName,DEMIXOpenMap) then begin
            {$IfDef RecordDEMIXFull} HighLightInDebugFile('Start ' + AreaName); {$EndIf}

            DEMIXtileDB := DEMIXtileFill(DEMIXRefDEM,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,false);
            {$IfDef RecordDEMIXFull} WriteLineToDebugFile('DTM on map tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}
            GISdb[DEMIXtileDB].ApplyGISFilter(FilterForDEMIXtilesToAvoid);
            {$IfDef RecordDEMIXFull} WriteLineToDebugFile('DTM filled tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}
            GISdb[DEMIXtileDB].DeleteAllSelectedRecords;

            if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
               TStr := 'No filled DEMIX tiles ' + AreaName;
               ErrorLog.Add(Tstr);
               {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
            end
            else begin
               {$If Defined(RecordDEMIXFull) or Defined(TrackDEMIX_DEMs) or Defined(Rec_DEMIX_Landcover)} OpenDEMsToDebugFile('Land cover loading'); {$EndIf}
               if DoLandTypes then LoadLandcoverForDEMIXarea(AreaName,false);
               {$If Defined(Rec_DEMIX_Landcover)} if ValidDEM(LandCoverGrid) then WriteLineToDebugFile('Land cover area=' + sfBoundBoxToString(DEMGlb[LandCoverGrid].DEMBoundBoxGeo,2)); {$EndIf}
               {$If Defined(RecordDEMIXFull) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('comparing'); {$EndIf}
               OpenDataStructures;
               j := 0;
               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  HeavyDutyProcessing := true;
                  inc(j);
                  DEMIXtile := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
                  wmdem.SetPanelText(2,'Tile: ' + IntToStr(j) + '/' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsinDB) + '  ' + DEMIXtile);
                  bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
                  LatCent := 0.5 * (bb.ymax + bb.ymin);
                  LongCent := 0.5 * (bb.xmax + bb.xmin);
                  TileHeader := AreaName + ',' + DEMIXTile + ',' + RealToString(LatCent,-12,-2) + ',' + RealToString(LongCent,-12,-2) + ',';

//change this for the new one, with the separate loops for area and point
                  for i := 1 to NumDEMIXtestDEM do begin
                     if ValidDEM(TestDEMs[i]) then begin
                        {$If Defined(RecordDEMIXFull) or Defined(RecordDEMIXLoops) or Defined(RecordCriteriaEvaluation)}
                           WriteLineToDebugFile(DEMIXTile + ' Start tests DEM=' + IntToStr(TestDEMs[i]) + '/' + IntToStr(Ser) + ' Series=' + TestSeries[i] + DEMGlb[TestDEMs[i]].AreaName);
                        {$EndIf}
                        GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                        bbgrid := GridBoundingBox(TestDEMs[i],UseDTM,true);
                        CompareDifferencesToReferenceDEM(TestDEMs[i],UseDTM,'DTM',bbgrid);
                        if DoDSM and ValidDEM(UseDSM) then CompareDifferencesToReferenceDEM(TestDEMs[i],UseDSM,'DSM',bbgrid);
                        {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('All tests done for ' + TestSeries[i]); {$EndIf}
                     end;
                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               {$If Defined(RecordDEMIXFull)} for i := 1 to 2 do writeLineToDebugFile(''); {$EndIf}
               CloseDataStructures;
            end;
            {$If Defined(RecordCriteriaEvaluation)} WriteLineToDebugFile('call CloseAndNilNumberedDB(DEMIXtileDB)'); {$EndIf}
            CloseSingleDB(DEMIXtileDB);
         end
         else begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Error loading test DEMs, ' + AreaName); {$EndIf}
         end;
      end
      else begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Error loading reference DEMs, ' + AreaName); {$EndIf}
      end;
      ReportErrors := true;
      {$If Defined(RecordDEMIXFull) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('closing all DEMs'); {$EndIf}
      if DEMIXCloseMap then CloseAllDEMs;
    end {ProcessDEMIXtestarea};


      procedure ProcessAnArea(AreaName: shortstring);
      begin
         GetAreaDEMNames(AreaName);
         if FileExists(RefDTMPointFName) and FileExists(RefDTMareaFName) then begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Start process for ' + AreaName); {$EndIf}
            InitializeStringLists;
            ProcessDEMIXtestarea;
            FinalizeStringLists(false);
            CloseAllDataBases;
            CleanUpTempDirectory(false);
         end
         else begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile(AreaName + ' missing 1 sec reference DEMs'); {$EndIf}
         end;
      end;


      function TransposeDEMIXcriteria(csvFile : PathStr) : PathStr;
      const
         MaxDEMs = 10;
         MaxCriteria = 25;
      var
         Headers,DEMs,Criteria,Output,Tiles : tStringList;
         Line : ANSIString;
         Table : tMyData;
         Gooddata : boolean;
         CriteriaFile,fName : PathStr;
         value,ThisDEM,Missing : shortstring;
         Cycles,Done,dbOnTable,
         i,j,  Tile, Ref, aLandType,DEM,Criterion : Integer;
         Values : array[0..MaxDEMs,0..MaxCriteria] of shortstring;
      begin
         {$If Defined(RecordDEMIXFull) or Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria in, ' + csvFile); {$EndIf}
         wmdem.SetPanelText(3,'TransposeDEMIXcriteria, ' + csvFile,true);

         fName := MDtempDir + ExtractFileName(csvFile);
         CopyFile(csvFile,fName);
         Table := tMyData.Create(fName);
         DEMs := Table.ListUniqueEntriesInDB('DEM');

         CriteriaFile := DEMIXSettingsDir + 'criteria_all.txt';
         Criteria := tStringList.Create;
         Criteria.LoadFromFile(CriteriaFile);

         for Criterion := pred(Criteria.Count) downto 0 do begin
            if not Table.FieldExists(Criteria.Strings[Criterion]) then begin
               Criteria.Delete(Criterion);
            end;
         end;

         Output := tStringList.Create;
         Line := 'AREA,DEMIX_TILE,CRITERION,REF_TYPE,LAND_TYPE';
         for i := 0 to pred(DEMs.Count) do begin
            Line := Line + ',' + DEMs.Strings[i];
         end;
         Output.Add(Line);

         Tiles := Table.ListUniqueEntriesInDB('DEMIX_TILE');

         Cycles := Tiles.Count * 2 * NumLandTypes;
         Done := 0;
         StartProgress('Transposing ' + Table.ShortTableName);
         for Tile := 0 to Pred(Tiles.Count) do begin
            {$If Defined(RecordDEMIXFull)} if (Tile mod 25 = 0) then WriteLineToDebugFile(IntToStr(succ(Tile)) + '/' + IntToStr(Tiles.Count)); {$EndIf}
            wmdem.SetPanelText(2,IntToStr(succ(Tile)) + '/' + IntToStr(Tiles.Count));
            wmdem.SetPanelText(3,Tiles.Strings[Tile]);
            UpdateProgressBar(Done/Cycles);
            for Ref := 1 to 2 do begin
               for aLandType := 1 to NumLandTypes do begin
                  Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefDEMType[Ref])  + ' AND LAND_TYPE=' + QuotedStr(LandTypes[aLandType]);
                  Table.ApplyFilter(Line);
                     if (Table.FiltRecsInDB = DEMs.Count) then begin
                        for i := 0 to MaxDems do
                           for j := 0 to MaxCriteria do
                               Values[i,j] := '-999';

                        while not Table.eof do begin
                           ThisDEM := UpperCase(Table.GetFieldByNameAsString('DEM'));
                           DEM := DEMs.IndexOf(ThisDEM);
                           if (DEM = -1) then begin
                              ErrorLog.Add(Line + '  -1 index for ' + Table.GetFieldByNameAsString('DEM'));
                           end
                           else begin
                              for Criterion := 0 to pred(Criteria.Count) do begin
                                 value := Table.GetFieldByNameAsString(Criteria[Criterion]);
                                 if (value <> '') then Values[DEM,Criterion] := value;
                              end;
                           end;
                           Table.Next;
                        end;

                        for Criterion := 0 to pred(Criteria.Count) do begin
                           Line := AreaName + ',' + Tiles[Tile] + ',' + Criteria[Criterion] + ',' + RefDEMType[Ref]  + ',' + LandTypes[aLandType];
                           GoodData := true;
                           for DEM := 0 to pred(DEMs.Count) do begin
                              Line := Line + ',' + Values[DEM,Criterion];
                              if (Values[DEM,Criterion] = '-999') or (Values[DEM,Criterion] = 'NaN') then GoodData := false;
                           end;
                           if GoodData then Output.Add(Line);
                        end;
                     end;
                  //end;
               end;
            end;
         end;
         EndProgress;
         Table.Destroy;
         Criteria.Free;
         DEMs.Free;
         Result := MDTempDir + ExtractFileNameNoExt(fName) + '_transpose.csv';
         OutPut.SaveToFile(Result);
         OutPut.Free;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria out, created ' + Result); {$EndIf}
      end {function TransposeDEMIXcriteria};


var
   i : integer;
   MergeName : PathStr;
   TransposeNames : tStringList;
begin {procedure ComputeDEMIX_Diff_Dist_tile_stats}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('Start ComputeDEMIX_Diff_Dist_tile_stats'); {$EndIf}
   try
      ErrorLog := tStringList.Create;
      SaveBackupDefaults;
      MDdef.ConfirmDBEdits := false;
      MDdef.DefaultMapXSize := 800;
      MDdef.DefaultMapYSize := 800;
      GetDEMIXpaths;
      LandTypeMask := 'ALL';

      if ElevDiffHists then SafeMakeDir(DEMIX_diff_dist);
      SafeMakeDir(Diff_dist_results_dir);

      if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;

      {$IfDef RecordDEMIX} WriteLineToDebugFile('Areas to process in selected list=' + IntToStr(AreasWanted.Count)); {$EndIf}

      if not Overwrite then begin
         for i := pred(AreasWanted.Count) downto 0 do begin
            AreaName := AreasWanted.Strings[i];
            if FileExists(Diff_dist_results_dir + AreaName + '_Elev_diff_stats.csv') and
               FileExists(Diff_dist_results_dir + AreaName + '_Slope_diff_stats.csv') and
               FileExists(Diff_dist_results_dir + AreaName + '_Ruff_diff_stats.csv') then begin
               AreasWanted.Delete(i);
            end;
         end;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Areas not yet processed=' + IntToStr(AreasWanted.Count)); {$EndIf}
      end;

      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         wmdem.SetPanelText(1,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + '  ' + AreaName);
         ProcessAnArea(AreaName);
         MergeName := Diff_dist_results_dir + AreaName + '_merge_transpose.csv';
         if Overwrite or (not FileExists(MergeName)) then begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Transposing'); {$EndIf}
            TransposeNames := tStringList.Create;
            fName := TransposeDEMIXcriteria(Diff_dist_results_dir + AreaName + '_ELEV_DIFF_STATS.csv');
            if FileExists(fName) then TransposeNames.Add(fName);
            fName := TransposeDEMIXcriteria(Diff_dist_results_dir + AreaName + '_SLOPE_DIFF_STATS.csv');
            if FileExists(fName) then TransposeNames.Add(fName);
            fName := TransposeDEMIXcriteria(Diff_dist_results_dir + AreaName + '_RUFF_DIFF_STATS.csv');
            if FileExists(fName) then TransposeNames.Add(fName);
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Merging'); {$EndIf}
            if (TransposeNames.Count > 1) then MergeCSVFiles(TransposeNames,MergeName);
            TransposeNames.Destroy;
         end;
      end;
      {$IfDef RecordDEMIXFull} WriteLineToDebugFile('All processing done'); {$EndIf}
      AreasWanted.Destroy;
   finally;
      RestoreBackupDefaults;
      DisplayAndPurgeStringList(ErrorLog,'ComputeDEMIXstats_Problems');
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('End ComputeDEMIX_Diff_Dist_tile_stats'); {$EndIf}
end {ComputeDEMIX_Diff_Dist_tile_stats};


procedure CreateFinalDiffDistDB;
var
   Areas,Merges,csv : tStringList;
   AreaName : shortstring;
   MergeName : PathStr;
   j,db : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CreateFinalDB in'); {$EndIf}
   GetDEMIXpaths;
   ShowHourglassCursor;
   Areas := DEMIX_AreasWanted;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV picked areas=' + IntToStr(Areas.Count)); {$EndIf}
   try
      Merges := tStringList.Create;
      for j := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[j];
         MergeName := Diff_dist_results_dir + AreaName + '_merge_transpose.csv';
         if FileExists(MergeName) then Merges.Add(MergeName);
      end;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Files to merge=' + IntToStr(Merges.Count)); {$EndIf}
      MergeName := DEMIX_final_DB_dir + DEMIXmodeName + '_diff_dist_demix_db_' + CurrentTimeForFileName(false) + '.csv';
      MergeCSVFiles(Merges,MergeName);
      OpenNumberedGISDataBase(db,MergeName,true);
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CreateFinalDB out, created ' + MergeName); {$EndIf}
end;


procedure MergeCSVtoCreateFinalDB(CSVdir : PathStr; FileDescription,dbName : shortstring);
var
   Areas,Merges,Missing : tStringList;
   AreaName : shortstring;
   MergeName : PathStr;
   j,db : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CreateFinalDB in for ' + CSVDir); {$EndIf}
   try
      ShowHourglassCursor;
      Merges := tStringList.Create;
      FindMatchingFiles(CSVDir,'*.csv',Merges,0);
      if (Merges.Count > 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CreateFinalDB out, created ' + MergeName + ' from areas=' + IntToStr(Merges.Count)); {$EndIf}
         MergeName := DEMIX_final_DB_dir + DEMIXmodeName + dbName + CurrentTimeForFileName(false) + '.csv';
         MergeCSVFiles(Merges,MergeName);
         OpenNumberedGISDataBase(db,MergeName,true);
         DeleteFileIfExists(MergeName);
         FilterTableForDEMIXevaluation(db,0);
         if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
            if AnswerIsYes('Missing records in DB=' + IntToStr(GISdb[db].MyData.FiltRecsInDB) + '; Delete them') then
               FilterTableForDEMIXevaluation(DB,0);
         end
         else GISdb[db].ClearGISFilter;
      end
      else begin
         Merges.Destroy;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No FinalDB created'); {$EndIf}
      end;
   finally
      EndDEMIXProcessing;
   end;
end;


procedure MergeCSV(Mode : integer);
begin
   MDDef.DEMIX_all_areas := true;
   SetParamsForDEMIXmode;
   GetDEMIXpaths;
   case Mode of
      1 : MergeCSVtoCreateFinalDB(FUVresultsDir,'_fuv_results.csv','_fuv_demix_db_');
      2 : MergeCSVtoCreateFinalDB(SSIMresultsDir,'_ssim_results.csv','_ssim_demix_db_');
      {$IfDef IncludeVectorCriteria}
         3 : MergeCSVtoCreateFinalDB(ChannelMissesDir,'_Channel_Misses.csv','_channel_misses_' );
         5 : MergeCSVtoCreateFinalDB(Stream_valley_dir,'_ridges_valleys.csv','_ridges_valleys_' );
      {$EndIf}
      4 : MergeCSVtoCreateFinalDB(GeomorphonsDir,'_geomorphons.csv','_pt_class_demix_db_');
      6 : MergeCSVtoCreateFinalDB(PartialsResultsDir,'_partials_results.csv','_partials_demix_db_');
      7 : MergeCSVtoCreateFinalDB(CurvaturesResultsDir,'_curvatures_results.csv','_curvatures_demix_db_');
   end;
end;



procedure ModeOfDifferenceDistributions;
var
   FilesWanted,Modes,TileList : tStringList;
   fName,fName2 : PathStr;
   i,j,NPts,db,atile : integer;
   BinSize,Mode : float32;
   Tile,param,Ref,aLine,Area : shortstring;
   Values : ^bfarray32;
   MomentVar : tMomentVar;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ModeOfDifferenceDistributions in'); {$EndIf}
   try
      if not Petmar.GetExistingFileName('Ordered list of tiles','*.txt',fName) then exit;
      GetDEMIXpaths;
      Modes := tStringList.Create;
      aLine := 'AREA,TILE,REF_TYPE,CRITERION';
      for I := 1 to NumDEMIXtestDEM do begin
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

                  for j := 1 to NumDEMIXtestDEM do begin
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
                     else Mode := -999;
                     aLine := aLine + ',' + RealToString(Mode,-8,-2) + ',' + RealToString(MomentVar.Mean,-8,-2) + ',' + RealToString(MomentVar.Median,-8,-2) + ',' + RealToString(MomentVar.std_dev,-8,-2);
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


procedure ComputeDEMIX_Summary_stats_DB(GeoTiles : boolean = true);
var
   MomentVar,ForestMomentVar,UrbanMomentVar,BarrenMomentVar,WaterMomentVar : tMomentVar;
   DEMIXtileDB,LandCoverGrid : integer;
   bbgrid : tGridLimits;
   fName : PathStr;
   Country,AreaName,DEMIXtile,LandTypeMask,TileHeader : shortstring;
   {$IfDef AllowAreaStats}
      AreaGridFull,AreaAveElev,AreaAveSlope,AreaAveRuff,AreaAveRelief,
      AreaForestPC, AreaUrbanPC,AreaBarrenPC,AreaWaterPC,AreaMinElev,AreaMaxElev : float64;
      AreaStats : tStringList;
   {$EndIf}
   LatCent,LongCent,
   GridFull,Lat,Long : float64;
   AreasWanted,TileStats : tStringList;
   zs : ^bfarray32;

         procedure InitializeStringLists;
         const
            TileStatsString = ',COUNTRY,LAT,LONG,GRID_FULL,AVG_ELEV,AVG_SLOPE,AVG_ROUGH,RELIEF,FOREST_PC,URBAN_PC,BARREN_PC,WATER_PC,MIN_ELEV,MAX_ELEV';
         begin
            TileStats := tStringList.Create;
            TileStats.Add('AREA,DEMIX_TILE' + TileStatsString);
            {$IfDef AllowAreaStats}
               AreaStats := tStringList.Create;
               AreaStats.Add('AREA,NUM_TILE' + TileStatsString);
            {$EndIf}
         end;


         procedure FinalizeStringLists(FinalSaveResults : boolean = true);

               function ProcessStringList(fName : PathStr; var sl : tStringList) : integer;
               begin
                  if (sl.count > 1) then begin
                     fName := DEMIXSettingsDir + fname + '.dbf';
                     DeleteFileIfExists(fName);
                     fName := ChangeFileExt(fName,'.csv');
                     DeleteFileIfExists(fName);
                     {$IfDef RecordDEMIX} HighLightInDebugFile('Summary stats written to ' + fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                     Result := StringList2CSVtoDB(sl,fName);
                  end
                  else begin
                     {$IfDef RecordDEMIX} HighLightInDebugFile('No Summary stats for ' + fName); {$EndIf}
                     fName := '';
                     sl.Destroy;
                     Result := 0;
                  end;
               end;
         var
            db : integer;
         begin
            {$IfDef TrackAverageStats} TileStats.SaveToFile(DEMIXSettingsDir + 'tilestats.csv'); {$EndIf}
            db := ProcessStringList('DEMIX_tiles_statistics',TileStats);
            if ValidDB(db) then AddPercentPrimaryData(DB);
            {$IfDef AllowAreaStats}
               ProcessStringList('DEMIX_area_statistics',AreaStats);
            {$EndIf}
         end;


   procedure ProcessDEMIXtestarea;


         function GridBoundingBox(DEM : integer) : tGridLimits;
         var
            bb : sfBoundBox;
         begin
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,Result.xgridlow,Result.ygridlow);
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,Result.xgridhigh,Result.ygridhigh);
         end;

         function UseThisTile : boolean;
         var
            bb : sfBoundBox;
         begin
            GridFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
            DEMIXtile := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            LatCent := 0.5 * (bb.ymax + bb.ymin);
            LongCent := 0.5 * (bb.xmax + bb.xmin);
            Result := GridFull >= MDDef.DEMIX_Tile_Full;
            {$IfDef RecordUseTile} if not Result then WriteLineToDebugFile('Not doing tile=' + DEMIXTile + '  fill=' + IntToStr(round(GridFull))); {$EndIf}
         end;

            procedure LandCover(LandCoverGrid : integer; Lat,Long : float32; Value : float32);
            var
               slc : integer;
            begin
               slc := SimplifiedLandCover(LandCoverGrid,Lat,Long,Value);
               if (slc = slcForest) then inc(ForestMomentVar.Npts)
               else if (slc = slcBarren) then inc(BarrenMomentVar.Npts)
               else if (slc = slcUrban) then inc(UrbanMomentVar.Npts)
               else if (slc = slcWater) then inc(WaterMomentVar.Npts);
            end;


         procedure GetTileStatistics(RefDEM : integer);
         var
           ElevMomentVar,SlopeMomentVar,RoughMomentVar : tMomentVar;
           Col,Row,NPts : integer;
           ForestPC,UrbanPC,BarrenPC,WaterPC,Relief : float32;
         begin
            if UseThisTile then begin
               bbgrid := GridBoundingBox(RefDEM);
               InitializeMomentVar(ElevMomentVar);
               InitializeMomentVar(SlopeMomentVar);
               InitializeMomentVar(RoughMomentVar);
               NPts := 0;
               New(zs);
               DEMGlb[RefDEM].ElevationMomentsWithArray(bbgrid,ElevMomentVar,zs^);
               Relief := ElevMomentVar.Maxz - ElevMomentVar.Minz;
               if (Relief > 1) then begin
                  DEMGlb[RefDEM].SlopeMomentsWithArray(bbgrid,SlopeMomentVar,zs^);
                  DEMGlb[RefDEM].GetRoughnessInLongArray(bbgrid,RoughMomentVar.NPts,zs^);
                  Moment(zs^,RoughMomentVar,msAll);
                  {$IfDef TrackAverageStats} WriteLineToDebugFile(AreaName + '  ' + DEMIXTile +  '  ' + ShortMomentResultsToString(RoughMomentVar));  {$EndIf}
                  if (LandCoverGrid <> 0) then begin
                     InitializeMomentVar(UrbanMomentVar);
                     InitializeMomentVar(ForestMomentVar);
                     InitializeMomentVar(BarrenMomentVar);
                     InitializeMomentVar(WaterMomentVar);

                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           DEMGlb[RefDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                           LandCover(LandCoverGrid,Lat,long,Difference);
                           inc(NPts);
                        end;
                     end;
                     ForestPC := 100 * ForestMomentVar.Npts / NPts;
                     UrbanPC := 100 * UrbanMomentVar.Npts / NPts;
                     BarrenPC := 100 * BarrenMomentVar.Npts / NPts;
                     WaterPC := 100 * WaterMomentVar.Npts / NPts;
                  end
                  else begin
                     ForestPC := -999;
                     UrbanPC := -999;
                     BarrenPC := -999;
                     WaterPC := -999;
                  end;
                  {$IfDef AllowAreaStats}
                      AreaGridFull := AreaGridFull + GridFull;
                      AreaAveElev := AreaAveElev + ElevMomentVar.Mean;
                      AreaAveSlope := AreaAveSlope + SlopeMomentVar.Mean;
                      AreaAveRuff := AreaAveRuff + RoughMomentVar.Mean;
                      AreaAveRelief := AreaAveRelief + ElevMomentVar.Maxz - ElevMomentVar.Minz;
                      AreaForestPC := AreaForestPC + ForestPC;
                      AreaUrbanPC := AreaUrbanPC + UrbanPC;
                      AreaBarrenPC := AreaBarrenPC + BarrenPC;
                      AreaWaterPC := AreaWaterPC + WaterPC;
                      if AreaMinElev > ElevMomentVar.MinZ then AreaMinElev := ElevMomentVar.MinZ;
                      if AreaMaxElev < ElevMomentVar.MaxZ then AreaMaxElev := ElevMomentVar.MaxZ;
                  {$EndIf}

                  TileHeader := AreaName + ',' + DEMIXTile + ',' + Country + ',' + RealToString(LatCent,-12,-2) + ',' + RealToString(LongCent,-12,-2) + ',' +
                     RealToString(GridFull,-6,-2)   + ','  + RealToString(ElevMomentVar.Mean,-12,-2) + ',' +
                     RealToString(SlopeMomentVar.Mean,-12,-2)  + ',' + RealToString(RoughMomentVar.Mean,-12,-2)  + ',' +
                     RealToString(Relief,-12,-2) + ',' + RealToString(ForestPC,-12,-2)  + ',' + RealToString(UrbanPC,-12,-2) + ','
                     + RealToString(BarrenPC,-12,-2) + ',' + RealToString(WaterPC,-12,-2)  + ','  + RealToString(ElevMomentVar.MinZ,-12,-2)  + ','  + RealToString(ElevMomentVar.MaxZ,-12,-2);
                  TileStats.Add(TileHeader);
               end;
               Dispose(zs);
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
   begin {ProcessDEMIXtestarea}
      {$If Defined(RecordDEMIXFull)} HighLightInDebugFile('ProcessDEMIXtestarea in ' + TestAreaName); {$EndIf}
      ReportErrors := false;
      DEMIXtileDB := 0;
      LandCoverGrid := 0;
      {$IfDef AllowAreaStats}
          AreaGridFull := 0;
          AreaAveElev := 0;
          AreaAveSlope := 0;
          AreaAveRuff := 0;
          AreaAveRelief := 0;
          AreaForestPC := 0;
          AreaUrbanPC := 0;
          AreaBarrenPC := 0;
          AreaWaterPC := 0;
      {$EndIf}

      wmdem.SetPanelText(3,'Load reference DEM');
      DEMIXRefDEM := OpenNewDEM(RefDTMpointFName,false);
      if ValidDEM(DEMIXRefDEM) then begin
         DEMIXtileDB := DEMIXtileFill(DEMIXRefDEM,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,false);
         GISdb[DEMIXtileDB].ApplyGISFilter(FilterForDEMIXtilesToUse);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('DTM tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}
         {$IfDef AllowAreaStats}
            AreaMinElev := 99e38;
            AreaMaxElev := -99e38;
         {$EndIf}
         if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
            TStr := 'No filled DEMIX tiles on ' + AreaName;
            {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
         end
         else begin
            {$If Defined(RecordDEMIXFull) or Defined(TrackDEMIX_DEMs) or Defined(Rec_DEMIX_Landcover)} OpenDEMsToDebugFile('Land cover loading'); {$EndIf}
            LandCoverGrid := LoadLandcoverForDEMIXarea(AreaName,false);
            j := 0;
            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               HeavyDutyProcessing := true;
               inc(j);
               wmdem.SetPanelText(2,'Tile: ' + IntToStr(j) + '/' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsinDB));
               if UseThisTile then begin
                  GetTileStatistics(DEMIXRefDEM);
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
            {$IfDef AllowAreaStats}
                j := GISdb[DEMIXtileDB].MyData.FiltRecsInDB;
                DEMGlb[DEMIXRefDEM].DEMCenterPoint(LatCent,LongCent);
                TileHeader := AreaName + ',' + IntToStr(j) + ',' + Country + ',' + RealToString(LatCent,-12,-2) + ',' + RealToString(LongCent,-12,-2) + ',' +
                   RealToString(AreaGridFull / j,-6,-2)   + ','  + RealToString(AreaAveElev / j,-12,-2) + ',' +
                   RealToString(AreaAveSlope / j,-12,-2)  + ',' + RealToString(AreaAveRuff / j,-12,-2)  + ',' +
                   RealToString(AreaAveRelief / j,-12,-2) + ',' + RealToString(AreaForestPC / j,-12,-2)  + ',' + RealToString(AreaUrbanPC / j,-12,-2) + ',' +
                   RealToString(AreaBarrenPC / j,-12,-2) + ',' + RealToString(AreaWaterPC / j,-12,-2) + ',' +
                   RealToString(AreaMinElev,-12,-2) + ',' + RealToString(AreaMaxElev,-12,-2);
                AreaStats.Add(TileHeader);
            {$EndIf}
            {$If Defined(RecordDEMIXFull)} for i := 1 to 2 do writeLineToDebugFile(''); {$EndIf}
         end;
         {$If Defined(RecordCriteriaEvaluation)} WriteLineToDebugFile('call CloseSingleDB(DEMIXtileDB)'); {$EndIf}
         CloseSingleDB(DEMIXtileDB);
      end;
      ReportErrors := true;
      {$If Defined(RecordDEMIXFull) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('closing all DEMs'); {$EndIf}
      if DEMIXCloseMap then CloseAllDEMs;
    end {ProcessDEMIXtestarea};

var
   i : integer;
begin {procedure ComputeDEMIX_Summary_stats}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('Start ComputeDEMIXstats'); {$EndIf}
   try
      GetDEMIXpaths;
      MDDef.DEMIX_mode := dmFull;
      SetParamsForDEMIXmode;

      if GeoTiles then begin
          AreasWanted := DEMIX_AreasWanted;
      end
      else begin

      end;

      {$IfDef RecordDEMIX} WriteLineToDebugFile('Areas to process in selected list=' + IntToStr(AreasWanted.Count)); {$EndIf}

      InitializeStringLists;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         Country := GetCountryForArea(AreaName);
         wmdem.SetPanelText(1,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + '  ' + AreaName);
         GetAreaDEMNames(AreaName);
         if FileExists(RefDTMPointFName) then begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Start process for ' + AreaName); {$EndIf}
            ProcessDEMIXtestarea;
         end
         else begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile(AreaName + ' missing 1 sec reference DEMs'); {$EndIf}
         end;
      end;
      {$IfDef RecordDEMIX} WriteLineToDebugFile('All areas done'); {$EndIf}
      FinalizeStringLists(false);
      {$IfDef RecordDEMIX} WriteLineToDebugFile('All string lists processed'); {$EndIf}
      AreasWanted.Destroy;
   finally;
      RestoreBackupDefaults;
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('End ComputeDEMIXstats'); {$EndIf}
end {procedure ComputeDEMIX_Summary_stats};


procedure FindTilesInAreaForCoast;
const
   FilterHeight : array[2..4] of float64  = (120,80,10);
var
   i,j,DEM,DB : integer;
   Fixed : int64;
   AreasWanted : tStringList;
   Findings : tStringList;
   aLine,AreaName : shortstring;
   fName : PathStr;
begin {FindTilesInAreaForCoast}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('Start FindTilesInAreaForCoast'); {$EndIf}
   try
      GetDEMIXpaths;
      AreasWanted := DEMIX_AreasWanted;
      Findings := tStringList.Create;
      Findings.Add('AREA,COUNTRY,ALL_75,ALL_50,ALL_25,U120_75,U120_50,U120_25,U80_75,U80_50,U80_25,U10_75,U10_50,U10_25');
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         aLine := AreaName + ',' + GetCountryForArea(AreaName);
         wmdem.SetPanelText(1,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + '  ' + AreaName);
         fName := DEMIX_Ref_1sec + AreaName + '_dtm' + Ref1SecPointStr + '.tif';
         DEM := OpenNewDEM(fName);
         for j := 1 to 4 do begin
            if (j > 1) then DEMGlb[DEM].MarkAboveMissing(FilterHeight[j],Fixed,false);
            DB := DEMIXtileFill(DEM,DEMGlb[DEM].DEMBoundBoxGeo,false);
            GISdb[DB].ApplyGISFilter('GRID_FULL>' + IntToStr(75));
            aline := aLine + ',' + IntToStr(GISdb[DB].MyData.FiltRecsInDB);
            GISdb[DB].ApplyGISFilter('GRID_FULL>' + IntToStr(50));
            aline := aLine + ',' + IntToStr(GISdb[DB].MyData.FiltRecsInDB);
            GISdb[DB].ApplyGISFilter('GRID_FULL>' + IntToStr(25));
            aline := aLine + ',' + IntToStr(GISdb[DB].MyData.FiltRecsInDB);
            CloseSingleDB(db);
         end;
         Findings.Add(aline);
         CloseSingleDEM(DEM);
      end;
      fName := MDtempDir + 'estimated_area_tiles_by_elevation.dbf';
      StringList2CSVtoDB(Findings,fName);
      AreasWanted.Destroy;
   finally;
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('End FindTilesInAreaForCoast'); {$EndIf}
end;


procedure AddCountryColorToDB(DBonTable : integer);
var
   OneTile : shortstring;
   theFields : tStringList;
   Table : tMyData;
   i : integer;
   fName : PathStr;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddCountryColorToDB'); {$EndIf}
   fName := DEMIXSettingsDir + 'country_colors.dbf';
   GISdb[DBonTable].ClearLinkTable(true);
   theFields := tStringList.Create;
   theFields.Add('COLOR');
   GISdb[DBonTable].dbOpts.LinkTableName := fname;
   GISdb[DBonTable].dbOpts.LinkFieldThisDB := 'COUNTRY';
   GISdb[DBonTable].dbOpts.LinkFieldOtherDB := 'COUNTRY';
   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   GISdb[DBonTable].LinkSecondaryTable(GISdb[DBonTable].dbOpts.LinkTableName);
   GISdb[DBonTable].FillFieldsFromJoinedTable(TheFields,true);
   GISDb[DBonTable].ShowStatus;
   GISdb[DBonTable].ClearLinkTable(true);
end;


procedure AddTileCharacteristicsToDB(DBonTable : integer);
var
   OneTile : shortstring;
   theFields : tStringList;
   i : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddTileCharacteristics'); {$EndIf}

   GetDEMIXpaths(true);
   GISdb[DBonTable].ClearLinkTable(true);
   theFields := tStringList.Create;
   for I := 1 to NumTileCharacters do theFields.Add(TileCharacters[i]);
   theFields.Add('COUNTRY');

   OneTile := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');

   if not FileExists(MDdef.DEMIX_tile_chars_fname) then begin
      MDdef.DEMIX_tile_chars_fname := DEMIX_final_DB_dir;
      if not GetExistingFileName('DEMIX tile characteristics','*.dbf',MDdef.DEMIX_tile_chars_fname) then exit;
   end;

   GISdb[DBonTable].dbOpts.LinkTableName := MDdef.DEMIX_tile_chars_fname;

   GISdb[DBonTable].dbOpts.LinkFieldThisDB := 'DEMIX_TILE';
   GISdb[DBonTable].dbOpts.LinkFieldOtherDB := 'DEMIX_TILE';
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddFieldsToDEMIXDB, fields=' + IntToStr(TheFields.Count) + ' link=' + GISdb[DBonTable].dbOpts.LinkTableName); {$EndIf}

   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddFieldsToDEMIXDB point 2'); {$EndIf}
   GISdb[DBonTable].LinkSecondaryTable(GISdb[DBonTable].dbOpts.LinkTableName);

   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddFieldsToDEMIXDB point 4'); {$EndIf}
   GISdb[DBonTable].FillFieldsFromJoinedTable(TheFields,true);
   GISDb[DBonTable].ShowStatus;
   GISdb[DBonTable].ClearLinkTable(true);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddFieldsToDEMIXDB done'); {$EndIf}
   EndDEMIXProcessing;
end;


procedure AddPercentPrimaryData(DBonTable : integer);
var
   Cop,ALOS : integer;
   Lat,Long : float64;
   z : float32;
   Tile : shortstring;
   fName : PathStr;
   bb : sfBoundBox;
begin
   try
      ShowHourglassCursor;
      SetColorForProcessing;
      fName := 'J:\visioterra_primary_data_fraction\COP-DEM_GLO-30-DGED_v2019_1_A03_Primary_data_fraction.tif';
      Cop := OpenNewDEM(fName,false,'COP-PDF');
      fName := 'J:\visioterra_primary_data_fraction\AW3D30_v2003_A03_Primary_data_fraction.tif';
      ALOS := OpenNewDEM(fName,false,'ALOS-PDF');
      GISdb[dbOnTable].AddFieldToDataBase(ftFloat,'COP_PDF',6,2);
      GISdb[dbOnTable].AddFieldToDataBase(ftFloat,'ALOS_PDF',6,2);
      GISdb[dbOnTable].EmpSource.Enabled := false;
      GISdb[dbOnTable].MyData.First;
      ShowHourglassCursor;
      while not GISdb[dbOnTable].MyData.eof do begin
         GISdb[dbOnTable].MyData.Edit;
         Tile := GISdb[dbOnTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
         bb := DEMIXtileBoundingBox(Tile);
         if DEMglb[Cop].GetElevFromLatLongDegree(bb.ymax,bb.xmin,z) then GISdb[dbOnTable].MyData.SetFieldByNameAsFloat('COP_PDF',z);
         if DEMglb[ALOS].GetElevFromLatLongDegree(bb.ymax,bb.xmin,z) then GISdb[dbOnTable].MyData.SetFieldByNameAsFloat('ALOS_PDF',z);
         GISdb[dbOnTable].MyData.Next;
      end;
      GISdb[dbOnTable].EmpSource.Enabled := true;
      CloseSingleDEM(Cop);
      CloseSingleDEM(ALOS);
   finally
      ShowDefaultCursor;
      SetColorForWaiting;
   end;
end;


procedure EvalRangeAndStatsByCriterion(DBonTable : integer; aField : shortstring = '');
var
   Criteria : tstringlist;
   i,db : integer;
   fName : PathStr;
   MomentVar : tMomentVar;
   Findings : tStringList;
   Tol : float32;
   BaseFilter : shortstring;
begin
   try
      if (aField = '') then begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         aField := GISdb[DBonTable].PickField('Field statistics by criterion',NumericFieldTypes);
      end;
      SetColorForProcessing;
      GISdb[DBonTable].EmpSource.Enabled := false;
      BaseFilter := GISdb[DBonTable].MyData.Filter;
      Criteria := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('CRITERION');
      Findings := tStringList.Create;
      Findings.Add('CRITERION,MIN,PC5,PC25,MEDIAN,MEAN,PC75,PC95,MAX,N,TOLERANCE');
      for i := 0 to pred(Criteria.Count) do begin
          GISdb[DBonTable].ApplyGISFilter(AddAndIfNeeded(BaseFilter) + 'CRITERION=' + QuotedStr(Criteria.Strings[i]));
          GISdb[DBonTable].EmpSource.Enabled := false;
          MomentVar := GISdb[DBonTable].GetFieldStatistics(aField);
          //Tol := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('TOLERANCE');
          Tol := CriterionTieTolerance('TOLERANCE');
          Findings.Add(Criteria.Strings[i] + ',' + RealToString(MomentVar.MinZ,-12,-6)  + ',' +
                                                   RealToString(MomentVar.PC5,-12,-6)  + ',' +
                                                   RealToString(MomentVar.Q1,-12,-6)  + ',' +
                                                   RealToString(MomentVar.Median,-12,-6)  + ',' +
                                                   RealToString(MomentVar.Mean,-12,-6)  + ',' +
                                                   RealToString(MomentVar.Q3,-12,-6)  + ',' +
                                                   RealToString(MomentVar.PC95,-12,-6)  + ',' +
                                                   RealToString(MomentVar.MaxZ,-12,-6)  + ',' +
                                                   IntToStr(MomentVar.NPts)  + ',' +
                                                   RealToString(Tol,-12,-6) );
      end;
      fName := NextFileNumber(MDTempDir,'Evaluation_range_' + GISdb[DBonTable].dbName + '_','.dbf');
      db := StringList2CSVtoDB(Findings,fName);
      GISdb[DBonTable].ApplyGISFilter(BaseFilter);
   finally
      ShowDefaultCursor;
      SetColorForWaiting;
   end;
end;


procedure EvalRangeAndBestEvalForCriterion(DBonTable : integer);
//adds field with the range between the best and worst evaluations
var
   i,j : integer;
   Eval,MaxEval,MinEval : float32;
   Criterion,DEM : shortstring;
begin
   if GISdb[DBonTable].MyData.FieldExists('EVAL_RANGE') and GISdb[DBonTable].MyData.FieldExists('BEST_EVAL') then begin
      if not AnswerIsYes('Redo') then exit;
   end;
   SetColorForProcessing;
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,'EVAL_RANGE',12,6);
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,'BEST_EVAL',12,6);
   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   j := 0;
   while not GISDb[DBonTable].MyData.eof do begin
      if (j mod 50 = 0) then wmdem.SetPanelText(3,'Evaluation ranges ' + IntToStr(j) + '/' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB));
      inc(j);
      Criterion := GISdb[dbOnTable].MyData.GetFieldByNameAsString('CRITERION');
      if not IsDEMIX_signedCriterion(Criterion) then begin
      //if (GISDb[DBonTable].MyData.GetFieldByNameAsString('EVAL_RANGE') = '') or (GISDb[DBonTable].MyData.GetFieldByNameAsString('BEST_EVAL') = '') then begin
         MaxEval := -99e39;
         MinEval := 99e39;
         for I := 1 to MaxDEMIXDEM do begin
            DEM := DEMIXshort[i];
            if GISdb[DBonTable].MyData.FieldExists(DEM) then begin
               Eval := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEM);
               if (Eval > 0.000001) then PetMath.CompareValueToExtremes(Eval,MinEval,MaxEval);
            end;
         end;
         GISDb[DBonTable].MyData.Edit;
         GISDb[DBonTable].MyData.SetFieldByNameAsFloat('BEST_EVAL',MinEval);
         GISDb[DBonTable].MyData.SetFieldByNameAsFloat('EVAL_RANGE',MaxEval-MinEval);
      end;
      GISDb[DBonTable].MyData.Next;
   end;
   GISDb[DBonTable].ShowStatus;
   wmdem.SetPanelText(3,'');
   SetColorForWaiting;
end;




