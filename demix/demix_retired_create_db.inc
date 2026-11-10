{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{   include file for demix_definitions   }
{________________________________________}



procedure FindBestReferenceDEM(db : integer; DEM : shortstring);
const
   NumCompared = 6;
   NumCriteria = 5;
   NumBests = 4;
type
   tCompareNames = array[1..NumCompared] of shortstring;
   tBestNames = array[1..NumBests] of shortstring;
   tCompare64 = array[1..NumCompared] of float64;
const
   CompareCriteria : array[1..NumCriteria] of shortstring = ('ALL','ELEV_FUV','ELEV_MAE','SLOPE_FUV','SLOPE_MAE');
   AlosCompareNames : tCompareNames = ('DTM_ALOS','DTM_SRTM','REV_DTM_S','DSM_ALOS','REV_SRTM','REV_DSM_S');  //,'DTM_POINT');
   AlosBestNames : tBestNames = ('BEST_ALL','BEST_ALOS','BEST_DTM','BEST_DSM');
   CopCompareNames : tCompareNames = ('DTM_SRTM','DTM_ALOS','REV_DTM_A','DSM_SRTM','DSM_ALOS','REV_DSM_A');  //,'DTM_POINT');
   CopBestNames : tBestNames = ('BEST_ALL','BEST_COP','BEST_DTM','BEST_DSM');
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

    if (DEM = 'ALOS') then begin
       CompareNames := ALOSCompareNames;
       BestNames :=  ALOSBestNames;
    end
    else if (DEM = 'COP') then begin
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

procedure TiesByOpinions(DBonTable : integer);
var
   entry : shortstring;
   i,nTies,j : integer;

         procedure DoOne(DEM : shortString);
         var
            n : integer;
         begin
            if ANSIcontainsStr(entry,DEM) then n := succ(nTies) else n := 0;
            GISdb[DBonTable].MyData.SetFieldByNameAsInteger(DEM + '_TIE',n);
         end;

begin
   {$IfDef RecordDEMIX} HighLightInDebugFile('Tdbtablef.iesbyopinions1Click in'); {$EndIf}
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'ALOS_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'ASTER_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'COP_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'SRTM_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'NASA_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'FABDEM_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'TANDEM_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'TIES',4);
    GISdb[DBonTable].AddFieldToDataBase(ftString,'CRIT_CAT',4);
    GISdb[DBonTable].EmpSource.Enabled := false;
    GISdb[DBonTable].MyData.First;
    StartProgress('Tie numbers');
    j := 0;
    while not GISdb[DBonTable].MyData.eof do begin
       inc(j);
       UpdateProgressBar(j/GISdb[DBonTable].MyData.FiltRecsInDB);
       entry := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC'));
       GISdb[DBonTable].MyData.Edit;
       GISdb[DBonTable].MyData.SetFieldByNameAsString('DEM_LOW_SC',entry);
       nTies := 0;
       for i := 1 to Length(Entry) do if (Entry[i] = ',') then inc(NTies);
       GISdb[DBonTable].MyData.SetFieldByNameAsInteger('TIES',succ(nTies));
       GISdb[DBonTable].MyData.SetFieldByNameAsString('CRIT_CAT',Copy(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION'),1,4));
       DoOne('ASTER');
       DoOne('ALOS');
       DoOne('NASA');
       DoOne('SRTM');
       DoOne('COP');
       DoOne('FABDEM');
       DoOne('TANDEM');
       GISdb[DBonTable].MyData.Next;
    end;
    GISdb[DBonTable].ShowStatus;
   {$IfDef RecordDEMIX} HighLightInDebugFile('Tdbtablef.iesbyopinions1Click out'); {$EndIf}
end {procedure TiesByOpinions};



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



