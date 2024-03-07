{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{ include file for demix_definitions }
{____________________________________}


procedure MergeSSIMandFUVDB;
var
   db1,db2 : integer;
begin
   //db1 :=
end;



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
   if (Option = opByCluster) then begin
      aField := 'CLUSTER';
      UniqueEntries := GISdb[DBonTable].MyData.UniqueEntriesInDB(aField);
      SortStringListNumerically(UniqueEntries);
   end
   else if (Option = opByDEM) then begin
      aField := 'DEM';
      UniqueEntries := GISdb[DBonTable].MyData.UniqueEntriesInDB(aField);
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
   GISdb[DBonTable].theMapOwner.StringListToLoadedDatabase(Results,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;



 procedure DEMIX_SSIM_FUV_transpose_kmeans_new_db(DBonTable : integer);
//transpose the DB so it has one TILE/DEM pair on each line, the Criteria as columns
var
   values : array[1..9,1..NCrits] of float32;
   Tile,Crit,aline : shortstring;
   i,j,k,N,NDEM : integer;
   rfile : file;
   theTiles,sl : tStringList;
   fName,OutPath : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_SSIM_FUV_transpose_kmeans_new_db in'); {$EndIf}
   try
      GetDEMIXpaths;
      GISdb[DBonTable].EmpSource.Enabled := false;
      theTiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
      OutPath := 'c:\temp\ssim_tile_summary\';

      sl := tStringList.Create;
      aline := 'DEMIX_TILE,DEM';
      for I := 1 to NCrits do aline := aline + ',' + Crits[i];
      sl.Add(Aline);

      LoadDEMIXnames;
      SafeMakeDir(OutPath);
      StartProgress('SSIM summary');
      for k := 0 to pred(theTiles.Count) do begin
         if (k mod 10 = 0) then wmdem.SetPanelText(1,IntToStr(k) + '/' + IntToStr(TheTiles.Count) + ' tiles transpose');
         Tile := theTiles[k];
         GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tile));
         GISdb[DBonTable].EmpSource.Enabled := false;
         if (GISdb[DBonTable].MyData.FiltRecsinDB = NCrits) then begin
            while not GISdb[DBonTable].MyData.eof do begin
               Crit := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION'));
               N := 1;
               while (N < succ(NCrits)) and (Crit <> Crits[N]) do inc(N);
               if (N <= NCrits) then begin
                  for I := 1 to NumDEMIXDEM do begin
                      Values[i,N] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXDEMTypeName[i]);
                  end;
               end;
               GISdb[DBonTable].MyData.Next;
            end;

            for I := 1 to NumDEMIXDEM do begin
               aline := Tile + ',' + DEMIXDEMTypeName[i];
               for j := 1 to NCrits do aline := aline + ',' + RealToString(Values[i,j],-10,6);
               sl.Add(Aline);
            end;
         end;
      end;

      fName := NextFileNumber(MDTempDir,MDTempDir + 'ssim_FUV_clustering_','.dbf');
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_SSIM_FUV_transpose_kmeans_new_db creating ' + fName); {$EndIf}

      StringList2CSVtoDB(sl,fName);
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_SSIM_FUV_transpose_kmeans_new_db out'); {$EndIf}
end;



procedure ComputeDEMIX_tile_stats(Overwrite : boolean);
//a number of experimental options for criteria are disabled, and would require changes to the code to get working again
//this has one DEM per record, with all the criteria
//in hindsight, it might have been better to create this is the desired final format, as is done for the SSIM and FUV criteria;
//but this works, so why change it?
//also, if we want to try clustering the tiles, we need it this way
const
   MaxRefStore = 100;
var
   MomentVar,FlatMomentVar,SteepMomentVar,GentleMomentVar,CliffMomentVar,ForestMomentVar,UrbanMomentVar,BarrenMomentVar,WaterMomentVar : tMomentVar;
   SlopeMomentVar,ElevMomentVar : array[1..MaxRefStore] of tMomentVar;
   NumPits,NumPeaks : array[1..MaxRefStore] of integer;
   DEMIXtileDB,LandCoverGrid : integer;
   bbgrid : tGridLimits;
   fName : PathStr;
   AreaName,DEMIXtile,LandTypeMask,TileHeader : shortstring;
   LatCent,LongCent,
   GridFull,Lat,Long : float64;
   zRef : float32;
   TileStats,
   ElevDiffStats,
   RufDiffStats,
   SlopeDiffStats  : tStringList;
   zs,zsSteep,ZSFlat,zsGentle,zsCliff,zsForest,zsUrban,zsBarren,zsWater : ^bfarray32;
   SlopeAsp,SlopeAspectTest : tSlopeAspectRec;
   ErrorLog : tStringList;
   ElevFiles,LegendFiles : tStringList;

   {$IfDef ExperimentalCriteria} IceSatFName,GeodeticFName : PathStr; {$EndIf}
   {$IfDef ExperimentalCriteria} {$I experiment_demix_criteria.inc}  {$EndIf}

         procedure InitializeStringLists;
         const
            AreaString = ',DEM,REF_TYPE,LAND_TYPE,LANDTYP_PC';
            SlopeDiffStatsString = ',SLPD_MIN,SLPD_Max,SLPD_Mean,SLPD_AVD,SLPD_STD,SLPD_MED,SLPD_RMSE,SLPD_MAE,SLPD_LE90,SLPD_N';
            ElevDiffStatsString = ',ELVD_MIN,ELVD_Max,ELVD_Mean,ELVD_AVD,ELVD_STD,ELVD_MED,ELVD_RMSE,ELVD_MAE,ELVD_LE90,ELVD_N';
            RufDiffStatsString = ',RUFD_MIN,RUFD_Max,RUFD_Mean,RUFD_AVD,RUFD_STD,RUFD_MED,RUFD_RMSE,RUFD_MAE,RUFD_LE90,RUFD_N';

            {$IfDef ExperimentalCriteria}
               ElevMomentsString = 'ELEV_MIN,ELEV_Max,ELEV_Mean,ELEV_ADEV,ELEV_STD,ELEV_SKEW,ELEV_KURT,ELEV_MED,N';
               SlopeMomentsString = 'SLP_MIN,SLP_Max,SLP_Mean,SLP_ADEV,SLP_STDV,SLP_SKEW,SLP_KURT,SLP_MED,SLP_N';

               SlopeMomentsDiffString = 'SDIFF_MIN,SDIFF_Max,SDIFF_Mean,SDIFF_STD,SDIFF_SKEW,SDIFF_KURT,SDIFF_MED,SDIFF_N';
               ElevMomentsDiffString = 'EDIFF_MIN,EDIFF_Max,EDIFF_Mean,EDIFF_STD,EDIFF_SKEW,EDIFF_KURT,EDIFF_MED,EDIFF_N';
            {$EndIf}

            //TileStatsString = 'AREA,DEMIX_TILE,LAT,LONG,GRID_FULL,AVG_ELEV,AVG_SLOPE,AVG_ROUGH,RELIEF,FOREST_PC,URBAN_PC,BARREN_PC,WATER_PC';
            TileStatsString = 'AREA,DEMIX_TILE,LAT,LONG';
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

            {$IfDef RecordDEMIXFull} WriteLineToDebugFile('InitializeStringLists out, string lists created'); {$EndIf}
         end;


         function LineHeader(DEM : integer; RefType : shortstring) : shortstring;
         var
            t1 : shortstring;
         begin
             if (DEM = 0) then t1 := 'xxx' else t1 := WhatTestDEMisThis(DEMGlb[DEM].AreaName);
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
                  {$IfDef RecordFullDEMIX} WriteLineToDebugFile(fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                  if (sl.count > 1) then begin
                     fName := DEMIXresultsDir + AreaName + fname + '.csv';
                     sl.SaveToFile(fName);
                  end
                  else fName := '';
                  sl.Destroy;
               end;

         begin
            {$IfDef RecordFullDEMIX} WriteLineToDebugFile('DEMIX start string list processing'); {$EndIf}
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
                RealToString(MomentVar.avg_dev,-8,2) + ',' + RealToString(MomentVar.std_dev,-8,2) + ',' + RealToString(MomentVar.median,-8,2) + ',' + RealToString(MomentVar.rmse,-8,2)  + ',' +
                RealToString(MomentVar.mae,-8,2)  + ',' + RealToString(MomentVar.LE90,-12,-2) + ',' + IntToStr(MomentVar.NPts);
         end;


         function MomentDifferenceString(aMomentVar,RefMomentVar : tMomentVar) : shortstring;
         begin
             Result := RealToString(aMomentVar.MinZ-RefMomentVar.minZ,-8,2) + ',' + RealToString(aMomentVar.MaxZ-RefMomentVar.MaxZ,-8,2) + ',' +
                            RealToString(aMomentVar.Mean-RefMomentVar.mean,-8,2) + ',' + RealToString(aMomentVar.std_dev-RefMomentVar.std_dev,-8,2) + ',' +
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
               WaterMomentVar.Npts := 0;
            end;


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
                  WriteDifferenceResult('FOREST',WhichStats,DEM,RefDEM,zsForest^,ForestMomentVar,(100 * ForestMomentVar.NPts/MomentVar.NPts));
                  WriteDifferenceResult('URBAN',WhichStats,DEM,RefDEM,zsUrban^,UrbanMomentVar,(100 * UrbanMomentVar.NPts/MomentVar.NPts));
                  WriteDifferenceResult('BARREN',WhichStats,DEM,RefDEM,zsBarren^,BarrenMomentVar,(100 * BarrenMomentVar.NPts/MomentVar.NPts));
                  WriteDifferenceResult('WATER',WhichStats,DEM,RefDEM,zsWater^,WaterMomentVar,(100 * WaterMomentVar.NPts/MomentVar.NPts));
               end;
               LandTypeMask := 'ALL';
           end;


           procedure LogError(TStr : shortstring);
           begin
              {$If Defined(RecordFullDEMIX) or Defined(RecordGridCompare)} WriteLineToDebugFile(TStr); {$EndIf}
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
                                 LandCover(LandCoverGrid,Lat,long,Difference);
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
                                 LandCover(LandCoverGrid,Lat,Long,Difference);
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
                                 LandCover(LandCoverGrid,Lat,long,Difference);
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
                  TStr := AreaName + ' ' + DEMIXTile + ' ' + RefType + ' elev=' + IntToStr(NumElev) +  ' slope=' + IntToStr(NumSlope) + ' ruff=' + IntToStr(NumRuff)  + ' total=' + IntToStr(NumElev+NumSlope+NumRuff);
                  wmdem.SetPanelText(3,TStr);
                  {$If Defined(RecordTileProcessing) or Defined(RecordFullDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
               end;
            end;


         procedure GetTileStatistics(RefDEM : integer);
         //var
           //ElevMomentVar,SlopeMomentVar,RoughMomentVar : tMomentVar;
           //Col,Row,NPts : integer;
           //ForestPC,UrbanPC,BarrenPC,WaterPC : float32;
         begin
            TileHeader := AreaName + ',' + DEMIXTile + ',' + RealToString(LatCent,-12,-2) + ',' + RealToString(LongCent,-12,-2) + ',';

            if UseThisTile then begin
               bbgrid := GridBoundingBox(RefDEM,0);
               (*
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
                  UrbanMomentVar.Npts := 0;
                  ForestMomentVar.Npts := 0;
                  BarrenMomentVar.Npts := 0;
                  WaterMomentVar.Npts := 0;

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
                  ForestPC := -9999;
                  UrbanPC := -9999;
                  BarrenPC := -9999;
                  WaterPC := -9999;
               end;

               TileHeader := AreaName + ',' + DEMIXTile + ',' + RealToString(LatCent,-12,-2) + ',' + RealToString(LongCent,-12,-2) + ',' +
                  RealToString(GridFull,-6,-2)   + ','  + RealToString(ElevMomentVar.Mean,-12,-2) + ',' +
                  RealToString(SlopeMomentVar.Mean,-12,-2)  + ',' + RealToString(RoughMomentVar.Mean,-12,-2)  + ',' +
                  RealToString(ElevMomentVar.Maxz - ElevMomentVar.Minz,-12,-2) + ',' + RealToString(ForestPC,-12,-2)  + ',' + RealToString(UrbanPC,-12,-2) + ','
                  + RealToString(BarrenPC,-12,-2) + ',' + RealToString(WaterPC,-12,-2);
               TileStats.Add(TileHeader);
               if (not JustTileStats) then TileHeader := TileHeader + ',';
               *)
            end
            else begin
               {$IfDef RecordTileStats} WriteLineToDebugFile('Not doing tile=' + DEMIXTile); {$EndIf}
            end;
         end;



         procedure OpenDataStructures;
         begin
            if ValidDEM(LandCoverGrid) then begin
               New(zsForest);
               New(zsUrban);
               New(zsBarren);
               New(zsWater);
            end;

            new(zs);
            New(zssteep);
            New(zsflat);
            New(zsGentle);
            New(zsCliff);
            {$IfDef RecordDEMIX} WriteLineToDebugFile(AreaName + '  ProcessDEMIXtestarea in, zs created'); {$EndIf}

            if ElevDiffHists then begin
               ElevFiles := tStringList.Create;
               LegendFiles := tStringList.Create;
            end;
         end;

          procedure CloseDataStructures;
          begin
               Dispose(zs);
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
               if ElevDiffHists then begin
                  ElevFiles.Destroy;
                  LegendFiles.Destroy;
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
      {$If Defined(RecordDEMIXFull)} HighlightLineToDebugFile('ProcessDEMIXtestarea in ' + TestAreaName); {$EndIf}
      ReportErrors := false;
      DEMIXtileDB := 0;
      LandCoverGrid := 0;

      for I := 1 to MaxRefStore do begin
        InitializeMomentVar(SlopeMomentVar[i]);
        InitializeMomentVar(ElevMomentVar[i]);
        NumPits[i] := 0;
        NumPeaks[i] := 0;
      end;

      wmdem.SetPanelText(3,'Load reference DEMs');

      if LoadDEMIXReferenceDEMs(AreaName,DEMIXRefDEM,DEMIXOpenMap) then begin
         if LoadDEMIXCandidateDEMs(AreaName,DEMIXOpenMap) then begin
            //if MaskForDiluvium then MaskAllDEMsToDiluvium;
            DEMIXtileDB := DEMIXtileFill(DEMIXRefDEM,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,false);
            GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL<' + IntToStr(MDDef.DEMIX_Full));
            {$IfDef RecordDEMIX} WriteLineToDebugFile('DTM tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}
            GISdb[DEMIXtileDB].DeleteAllSelectedRecords;

            if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
               TStr := 'No filled DEMIX tiles on ' + AreaName;
               ErrorLog.Add(Tstr);
               {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
            end
            else begin
               {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs) or Defined(Rec_DEMIX_Landcover)} OpenDEMsToDebugFile('Land cover loading'); {$EndIf}
               //GISdb[DEMIXtileDB].MyData.First;  //placed here to see where in the tile processing the program is at
               fName := DEMIX_area_lc100 + AreaName + '.tif';
               if FileExists(fname) then begin
                  LandCoverGrid := OpenNewDEM(fName,false);
                  if (DEMGlb[LandCoverGrid].DEMHeader.ElevUnits <> GLCS_LC100) then begin
                     DEMGlb[LandCoverGrid].DEMHeader.ElevUnits := GLCS_LC100;
                     DEMGlb[LandCoverGrid].WriteNewFormatDEM(fName);
                  end;
                  {$If Defined(Rec_DEMIX_Landcover)} OpenDEMsToDebugFile('Land cover already loaded ' + fName); {$EndIf}
               end
               else begin
                  bb := DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo;
                  LandCoverGrid := LoadLC100LandCover(fName,bb,Tstr,DEMIXopenMap);
                  if ValidDEM(LandCoverGrid) then begin
                     {$If Defined(Rec_DEMIX_Landcover)} OpenDEMsToDebugFile('Land cover just loaded ' + fName); {$EndIf}
                  end
                  else begin
                     ErrorLog.Add(TStr);
                     {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
                     //goto NoLandCover;
                  end;
               end;
               {$If Defined(Rec_DEMIX_Landcover)} if ValidDEM(LandCoverGrid) then WriteLineToDebugFile('Land cover area=' + sfBoundBoxToString(DEMGlb[LandCoverGrid].DEMBoundBoxGeo,2)); {$EndIf}
               {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('comparing'); {$EndIf}
               OpenDataStructures;
               j := 0;
               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  HeavyDutyProcessing := true;
                  inc(j);
                  wmdem.SetPanelText(2,'Tile: ' + IntToStr(j) + '/' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsinDB));
                  if UseThisTile then begin
                     GetTileStatistics(DEMIXRefDEM);
                     //if (not JustTileStats) then begin
                        for i := 1 to NumDEMIXDEM do begin
                           if ValidDEM(TestDEMs[i]) then begin
                              {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoops) or Defined(RecordCriteriaEvaluation)}
                                 WriteLineToDebugFile(DEMIXTile + ' Start tests for DEM=' + IntToStr(TestDEMs[i]) + '/' + IntToStr(Ser) +
                                     '  Series=' + TestSeries[i] + ' DEM=' + DEMGlb[TestDEMs[i]].AreaName);
                              {$EndIf}
                              GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                              CompareDifferencesToReferenceDEM(TestDEMs[i],UseDTM,'DTM');
                              CompareDifferencesToReferenceDEM(TestDEMs[i],UseDSM,'DSM');
                              {$If Defined(RecordFullDEMIX)} WriteLineToDebugFile('All tests done for ' + TestSeries[i]); {$EndIf}
                           end;
                        end;
                     //end;
                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               {$If Defined(RecordFullDEMIX)} for i := 1 to 2 do writeLineToDebugFile(''); {$EndIf}
               CloseDataStructures;
            end;
            {$If Defined(RecordCriteriaEvaluation)} WriteLineToDebugFile('call CloseAndNilNumberedDB(DEMIXtileDB)'); {$EndIf}
            CloseAndNilNumberedDB(DEMIXtileDB);
            //NoLandCover:;
         end;
      end;
      ReportErrors := true;
      {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('closing all DEMs'); {$EndIf}
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
         end
         else begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile(AreaName + ' missing 1 sec reference DEMs'); {$EndIf}
         end;
      end;


var
   i : integer;
   AreasWanted : tStringList;
begin {procedure ComputeDEMIX_tile_stats}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('Start ComputeDEMIXstats'); {$EndIf}
   try
      GetDEMIXpaths;
      ErrorLog := tStringList.Create;
      SaveBackupDefaults;
      MDdef.ConfirmDBEdits := false;
      MDdef.DefaultMapXSize := 800;
      MDdef.DefaultMapYSize := 800;
      LandTypeMask := 'ALL';

      if ElevDiffHists then SafeMakeDir(DEMIX_diff_dist);
      SafeMakeDir(DEMIXresultsDir);
      //if DoHorizontalShift then SafeMakeDir(DEMIXresultsDir + 'lags\' );

      AreasWanted := DEMIX_AreasWanted;

      {$IfDef RecordDEMIX} WriteLineToDebugFile('Areas to process in selected list=' + IntToStr(AreasWanted.Count)); {$EndIf}

      if not Overwrite {and (not JustTileStats)} then begin
         for i := pred(AreasWanted.Count) downto 0 do begin
            AreaName := AreasWanted.Strings[i];

            if FileExists(DEMIXresultsDir + AreaName + '_Elev_diff_stats.csv') and
               FileExists(DEMIXresultsDir + AreaName + '_Slope_diff_stats.csv') and
               FileExists(DEMIXresultsDir + AreaName + '_Ruff_diff_stats.csv') then begin
               AreasWanted.Delete(i);
               {$IfDef RecordDEMIX} WriteLineToDebugFile(AreaName + ' already processed'); {$EndIf}
            end;
         end;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Areas not yet processed=' + IntToStr(AreasWanted.Count)); {$EndIf}
      end;

      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         wmdem.SetPanelText(1,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + '  ' + AreaName);
         (*
         if JustTileStats then begin
            ProcessAnArea(AreaName);
         end
         else begin
         *)
            DeleteFileIfExists(DEMIXresultsDir + AreaName + '_Elev_diff_stats.csv');
            DeleteFileIfExists(DEMIXresultsDir + AreaName + '_Slope_diff_stats.csv');
            DeleteFileIfExists(DEMIXresultsDir + AreaName + '_Ruff_diff_stats.csv');
            DeleteFileIfExists(DEMIXresultsDir + AreaName + '_Elev_diff_stats.dbf');
            DeleteFileIfExists(DEMIXresultsDir + AreaName + '_Slope_diff_stats.dbf');
            DeleteFileIfExists(DEMIXresultsDir + AreaName + '_Ruff_diff_stats.dbf');
            ProcessAnArea(AreaName);
         //end;
      end;
      {$IfDef RecordDEMIXFull} WriteLineToDebugFile('All processing done'); {$EndIf}
      AreasWanted.Destroy;
   finally;
      RestoreBackupDefaults;
      DisplayAndPurgeStringList(ErrorLog,'ComputeDEMIXstats_Problems');
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('End ComputeDEMIXstats'); {$EndIf}
end {ComputeDEMIX_tile_stats};



procedure CreateDEMIX_GIS_database;
var
   fName : PathStr;
   db : integer;
   FilesWanted : tStringList;
   AreaName : shortstring;
   ElevDiff,SlopeDiff,RuffDiff,TransposeNames,ErrorLog : tStringList;


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
         {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile(''); WriteLineToDebugFile('TransposeDEMIXcriteria in, ' + GISdb[DBonTable].dbName); {$EndIf}
         Headers := tStringList.Create;
         Headers.LoadFromFile(DEMIXSettingsDir + 'demix_headers.txt');
         DEMs := tStringList.Create;
         DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');

         CriteriaFile := DEMIXSettingsDir + 'criteria_all.txt';
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
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria, ' + GISdb[DBonTable].dbName + ' tiles=' + IntToStr(Tiles.Count)); {$EndIf}

         Cycles := Tiles.Count * 2 * NumLandTypes;
         Done := 0;
         StartProgress('Transposing ' + GISdb[DBontable].DBName);
         for Tile := 0 to Pred(Tiles.Count) do begin
            {$If Defined(RecordDEMIX)} if (Tile mod 25 = 0) then WriteLineToDebugFile(IntToStr(succ(Tile)) + '/' + IntToStr(Tiles.Count)); {$EndIf}
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
                        {$If Defined(RecordDEMIXFull)}  //this is to track down why some tiles do not have 6 records
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
   var
      csv : tStringList;
   begin
      DeleteFileIfExists(fName);
      DeleteFileIfExists(ChangeFileExt(fName,'.dbf'));

      if (Diffs.Count > 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeAndTranspose ' + fName + ' with regions=' + IntToStr(Diffs.Count)); {$EndIf}
         MergeCSVFiles(Diffs,fName);
         csv := tStringList.Create;
         csv.LoadFromFile(fName);
         db := StringList2CSVtoDB(csv, ChangeFileExt(fName,'.dbf'),false,false,false);
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
   i,j : integer;
begin {procedure CreateDEMIX_GIS_database}
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV in'); {$EndIf}
   GetDEMIXpaths;
   ShowHourglassCursor;
   Areas := DEMIX_AreasWanted;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV picked areas=' + IntToStr(Areas.Count)); {$EndIf}
   FilesWanted := Nil;
   FindMatchingFiles(DEMIXresultsDir,'*.csv',FilesWanted,0);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CSV total results=' + IntToStr(FilesWanted.Count)); {$EndIf}
   for j := pred(FilesWanted.Count) downto 0 do begin
      KeepThisOne := false;
      for i := pred(Areas.Count) downto 0 do begin
         if StrUtils.AnsiContainsText(UpperCase(FilesWanted.Strings[j]),UpperCase(Areas.Strings[i])) then begin
            KeepThisOne := true;
         end;
      end;
      if (Not KeepThisOne) then begin
         {$If Defined(RecordDEMIX)} if StrUtils.AnsiContainsText(UpperCase(FilesWanted.Strings[j]),'RUFF') then WriteLineToDebugFile('Not Using (not in area list): ' + FilesWanted.Strings[j]); {$EndIf}
         FilesWanted.Delete(j);
      end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CSV in these areas=' + IntToStr(FilesWanted.Count)); {$EndIf}

   try
      ElevDiff := tStringList.Create;
      SlopeDiff := tStringList.Create;
      RuffDiff := tStringList.Create;
      TransposeNames := tStringList.Create;
      ErrorLog := tStringList.Create;
      for i := 0 to pred(FilesWanted.Count) do begin
         fName := uppercase(FilesWanted.Strings[i]);
         //if StrUtils.AnsiContainsText(fname,'DEMIX_TILES_USED') then Tiles.Add(fName);
         if StrUtils.AnsiContainsText(fname,'ELEV_DIFF_STATS') then ElevDiff.Add(fName);
         if StrUtils.AnsiContainsText(fname,'SLOPE_DIFF_STATS') then SlopeDiff.Add(fName);
         if StrUtils.AnsiContainsText(fname,'RUFF_DIFF_STATS') then RuffDiff.Add(fName);
      end;
      if (ElevDiff.Count > 1) then begin
         //{$If Defined(RecordDEMIX)} WriteLineToDebugFile('Tiles=' + IntToStr(Tiles.Count)); {$EndIf}
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Elev diff=' + IntToStr(ElevDiff.Count)); {$EndIf}
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Slope diff=' + IntToStr(SlopeDiff.Count)); {$EndIf}
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Ruff diff=' + IntToStr(RuffDiff.Count)); {$EndIf}
         //Name := ExtractFilePath(fName) + 'DEMIX_TILES_USED_SUMMARY.csv';
         //fName := StringReplace(fName,'_dtm','',[rfReplaceAll, rfIgnoreCase]);
         //MergeCSVFiles(Tiles,fName);
      end;
      //else Tiles.Free;

      wmdem.SetPanelText(1,'1/3  Elev');
      {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('elev'); {$EndIf}
      MergeAndTranspose(ElevDiff,DEMIXresultsDir + 'elev_merge_difference_ref_dem.csv');
      wmdem.SetPanelText(1,'2/3  Slope');
      {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('slope'); {$EndIf}
      MergeAndTranspose(SlopeDiff,DEMIXresultsDir + 'slope_merge_difference_ref_dem.csv');
      wmdem.SetPanelText(1,'3/3  Ruff');
      {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('ruff'); {$EndIf}
      MergeAndTranspose(RuffDiff,DEMIXresultsDir + 'ruff_merge_difference_ref_dem.csv');
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Three MergeAndTranspose done'); {$EndIf}

      DeleteFileIfExists(DEMIXresultsDir + 'latest_demix_database.dbf');
      fName := DEMIXresultsDir + 'latest_demix_database.csv';
      MergeCSVFiles(TransposeNames,fName);
      db := OpenMultipleDataBases('',fName);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeCSVFiles merged and db opened'); {$EndIf}
      AddCountryToDB(DB);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('AddCountryToDB done'); {$EndIf}
      RankDEMS(DB);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMs ranked'); {$EndIf}
      //CloseAndNilNumberedDB(db);
   finally
      DisplayAndPurgeStringList(ErrorLog,'DEMIX Create Database Problems');
      EndDEMIXProcessing;
      //CloseAllDatabases;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV out, created ' + fName); {$EndIf}
end {procedure CreateDEMIX_GIS_database};


procedure RankDEMS(DBonTable : integer);
const
   IttyBitty = 0.00001;  //added for floating point funny business between Delphi and Python
var
   Scores : tStringList;
   DEM,Value,N,i,j,Score,BestScore : integer;
   Sum,fValue,BestValue,TieTolerance,Cop,alos,fab : float32;
   fName : PathStr;
   BestValueString,aLine,Which : shortstring;
   Values : array[1..10] of float32;
   RankDEMs : array[1..10] of shortstring;
   Results,Scoring : tStringList;
   TieToleranceTable : tMyData;

       procedure FindTies(StartDEM : integer; var EndDEM : integer);
       var
          i,NumTies : integer;
       begin
          {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Find ties in, DEM=' + IntToStr(StartDEM)); {$EndIf}
          BestValueString := RankDEMs[StartDEM];
          BestScore := StartDEM;
          EndDEM := succ(StartDEM);
          NumTies := 1;
          while (Values[EndDEM] < (Values[StartDEM] + TieTolerance + IttyBitty)) and (EndDEM < NumDEMIXDEM) do begin
             if (StartDEM = 1) then begin
                BestValueString := BestValueString + '-' + RankDEMs[EndDEM];
             end;
             inc(NumTies);
             inc(EndDEM);
             BestScore := BestScore + EndDEM;
          end;
          if (StartDEM = 1) then begin
             GISdb[DBonTable].MyData.SetFieldByNameAsString('DEM_LOW_SC',BestValueString);
             GISdb[DBonTable].MyData.SetFieldByNameAsInteger('NUM_TIES',NumTies);
             {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Find ties =' + BestValueString); {$EndIf}
          end;
          for i := StartDEM to pred(EndDEM) do begin
             GISdb[DBonTable].MyData.SetFieldByNameAsFloat(RankDEMs[i] + '_SCR',BestScore / NumTies);
             {$If Defined(RecordDEMIXties)} WriteLineToDebugFile(RankDEMs[i] + RealToString(BestScore/NumTies,6,-2) + RealToString(Values[i],8,2)); {$EndIf}
          end;
          {$If Defined(RecordDEMIXties)} WriteLineToDebugFile('Find ties out, EndDEM=' + IntToStr(EndDEM)); {$EndIf}
       end;

var
   FirstDEM,l1,l2 : integer;
   ts,Criterion : shortstring;
   tv : float32;
begin {procedure RankDEMS}
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS in, ' + GISdb[DBonTable].dbName); {$EndIf}
    LoadDEMIXnames;
    if (not GetFileFromDirectory('DEMIX criterial tolerances','*.dbf',MDDef.DEMIX_criterion_tolerance_fName)) then exit;

    GISdb[DBonTable].AddFieldToDataBase(ftString,'DEM_LOW_SC',48);
    if GISdb[DBonTable].MyData.GetFieldLength('DEM_LOW_SC') < 48 then GISdb[DBonTable].MyData.TrimField('DEM_LOW_SC',48);

    for DEM := 1 to NumDEMIXDEM do begin
       fName := DEMIXshort[DEM] + '_SCR';
       GISdb[DBonTable].AddFieldToDataBase(ftFloat,fName,6,2);
    end;
    GISdb[DBonTable].AddFieldToDataBase(ftFloat,'TOLERANCE',9,6);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'NUM_TIES',2);

    i := 0;
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Load criterion tolerances, ' + MDDef.DEMIX_criterion_tolerance_fName); {$EndIf}
    TieToleranceTable := tMyData.Create(MDDef.DEMIX_criterion_tolerance_fName);
    if TieToleranceTable.FieldExists('CRITERION') and TieToleranceTable.FieldExists('TOLERANCE') then begin
       StartProgress('Rank DEMs');
       j := 0;
       GISdb[DBonTable].MyData.First;
       while not GISdb[DBonTable].MyData.eof do begin
          {$If Defined(RecordDEMIXties)} HighLightLineToDebugFile('Loop =' + IntToStr(j)); {$EndIf}
          inc(j);
          UpdateProgressBar(j/GISdb[DBonTable].MyData.FiltRecsInDB);
          GISdb[DBonTable].EmpSource.Enabled := false;

          //extract scores
          for DEM := 1 to NumDEMIXDEM do begin
             RankDEMs[DEM] := DEMIXShort[DEM];
             Values[DEM] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[DEM]);
             {$If Defined(RecordDEMIXties)} WriteLineToDebugFile(aLine); {$EndIf}
          end;

          //Bubble sort scores
          for l1 := 1 to (NumDEMIXDEM - 1) do begin
             for l2 := 1 to (NumDEMIXDEM - 1) do begin
                if Values[l2] > Values[succ(l2)] then begin
                   tv := Values[l2];
                   ts := RankDEMs[l2];
                   Values[l2] := Values[succ(l2)];
                   RankDEMs[l2] := RankDEMs[succ(l2)];
                   Values[succ(l2)] := tv;
                   RankDEMs[succ(l2)] := ts;
                end;
             end;
          end;

          GISdb[DBonTable].MyData.Edit;
          Criterion := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
          TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
          if (TieToleranceTable.FiltRecsInDB = 0) then begin
             TieTolerance := 0.01;
             ReadDefault('Tolerance for ties for ' + Criterion,TieTolerance);
             TieToleranceTable.Insert;
             TieToleranceTable.SetFieldByNameAsFloat('TOLERANCE',TieTolerance);
             TieToleranceTable.SetFieldByNameAsString('CRITERION',Criterion);
             TieToleranceTable.Post;
          end
          else TieTolerance := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');
          GISdb[DBonTable].MyData.SetFieldByNameAsFloat('TOLERANCE',TieTolerance);

          FirstDEM := 1;
          repeat
             {$If Defined(RecordDEMIXties)} WriteLineToDebugFile(''); {$EndIf}
             FindTies(FirstDEM,DEM);
             FirstDEM := DEM;
          until (DEM = NumDEMIXDEM);
          GISdb[DBonTable].MyData.Next;
       end;
    end
    else begin
        MessageToContinue('Table must have fields "CRITERION" and "TOLERANCE"');
    end;
    //DEMsRanked.Destroy;
    TieToleranceTable.Destroy;
    GISdb[DBonTable].ShowStatus;
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('RankDEMS out, ' + GISdb[DBonTable].dbName); {$EndIf}
end {procedure RankDEMS};


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


procedure ComputeDEMIX_Summary_stats_DB;
const
   MaxRefStore = 100;
var
   MomentVar,ForestMomentVar,UrbanMomentVar,BarrenMomentVar,WaterMomentVar : tMomentVar;
   DEMIXtileDB,LandCoverGrid : integer;
   bbgrid : tGridLimits;
   fName : PathStr;
   AreaName,DEMIXtile,LandTypeMask,TileHeader : shortstring;
   AreaGridFull,
   AreaAveElev,AreaAveSlope,AreaAveRuff,AreaAveRelief,
   AreaForestPC, AreaUrbanPC,AreaBarrenPC,AreaWaterPC,
   LatCent,LongCent,
   GridFull,Lat,Long : float64;
   TileStats,AreaStats : tStringList;
   zs : ^bfarray32;

         procedure InitializeStringLists;
         const
            TileStatsString = ',LAT,LONG,GRID_FULL,AVG_ELEV,AVG_SLOPE,AVG_ROUGH,RELIEF,FOREST_PC,URBAN_PC,BARREN_PC,WATER_PC';
         begin
            TileStats := tStringList.Create;
            TileStats.Add('AREA,DEMIX_TILE' + TileStatsString);
            AreaStats := tStringList.Create;
            AreaStats.Add('AREA,NUM_TILE' + TileStatsString);
         end;

         procedure FinalizeStringLists(FinalSaveResults : boolean = true);

               procedure ProcessStringList(fName : PathStr; var sl : tStringList);
               begin
                  {$IfDef RecordDEMIX} WriteLineToDebugFile(fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                  if (sl.count > 1) then begin
                     fName := DEMIXresultsDir + 'Summary' + fname + '.csv';
                     StringList2CSVtoDB(sl,fName);
                     {$IfDef RecordDEMIX} HighlightLineToDebugFile('Summary stats written to ' + fName); {$EndIf}
                  end
                  else fName := '';
                  sl.Destroy;
               end;

         begin
            {$IfDef RecordFullDEMIX} WriteLineToDebugFile('DEMIX start string list processing'); {$EndIf}
            ProcessStringList('_DEMIX_tiles_used',TileStats);
            ProcessStringList('_DEMIX_areas_used',AreaStats);
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


         function UseThisTile : boolean;
         var
            bb : sfBoundBox;
         begin
            GridFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
            DEMIXtile := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            LatCent := 0.5 * (bb.ymax + bb.ymin);
            LongCent := 0.5 * (bb.xmax + bb.xmin);
            Result := GridFull >= MDDef.DEMIX_Full;
            {$IfDef RecordUseTile} if not Result then WriteLineToDebugFile('Not doing tile=' + DEMIXTile + '  fill=' + IntToStr(round(GridFull))); {$EndIf}
         end;

            procedure LandCover(LandCoverGrid : integer; Lat,Long : float32; Value : float32);
            var
               slc : integer;
            begin
               slc := SimplifiedLandCover(LandCoverGrid,Lat,Long,Value);
               if slc = slcForest then inc(ForestMomentVar.Npts)
               else if slc = slcBarren then inc(BarrenMomentVar.Npts)
               else if slc = slcUrban then inc(UrbanMomentVar.Npts)
               else if slc = slcWater then inc(WaterMomentVar.Npts);
            end;

         procedure GetTileStatistics(RefDEM : integer);
         var
           ElevMomentVar,SlopeMomentVar,RoughMomentVar : tMomentVar;
           Col,Row,NPts : integer;
           ForestPC,UrbanPC,BarrenPC,WaterPC : float32;
         begin
            if UseThisTile then begin
               bbgrid := GridBoundingBox(RefDEM,0);
               ElevMomentVar.Npts := 0;
               SlopeMomentVar.Npts := 0;
               RoughMomentVar.Npts := 0;
               NPts := 0;
               New(zs);
               DEMGlb[RefDEM].ElevationMomentsWithArray(bbgrid,ElevMomentVar,zs^);
               DEMGlb[RefDEM].SlopeMomentsWithArray(bbgrid,SlopeMomentVar,zs^);
               DEMGlb[RefDEM].GetRoughnessInLongArray(bbgrid,RoughMomentVar.NPts,zs^);
               Moment(zs^,RoughMomentVar,msAll);
               Dispose(zs);

               if (LandCoverGrid <> 0) then begin
                  UrbanMomentVar.Npts := 0;
                  ForestMomentVar.Npts := 0;
                  BarrenMomentVar.Npts := 0;
                  WaterMomentVar.Npts := 0;

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
                  ForestPC := -9999;
                  UrbanPC := -9999;
                  BarrenPC := -9999;
                  WaterPC := -9999;
               end;

               AreaGridFull := AreaGridFull + GridFull;
               AreaAveElev := AreaAveElev + ElevMomentVar.Mean;
               AreaAveSlope := AreaAveSlope + SlopeMomentVar.Mean;
               AreaAveRuff := AreaAveRuff + RoughMomentVar.Mean;
               AreaAveRelief := AreaAveRelief + ElevMomentVar.Maxz - ElevMomentVar.Minz;
               AreaForestPC := AreaForestPC + ForestPC;
               AreaUrbanPC := AreaUrbanPC + UrbanPC;
               AreaBarrenPC := AreaBarrenPC + BarrenPC;
               AreaWaterPC := AreaWaterPC + WaterPC;

               TileHeader := AreaName + ',' + DEMIXTile + ',' + RealToString(LatCent,-12,-2) + ',' + RealToString(LongCent,-12,-2) + ',' +
                  RealToString(GridFull,-6,-2)   + ','  + RealToString(ElevMomentVar.Mean,-12,-2) + ',' +
                  RealToString(SlopeMomentVar.Mean,-12,-2)  + ',' + RealToString(RoughMomentVar.Mean,-12,-2)  + ',' +
                  RealToString(ElevMomentVar.Maxz - ElevMomentVar.Minz,-12,-2) + ',' + RealToString(ForestPC,-12,-2)  + ',' + RealToString(UrbanPC,-12,-2) + ','
                  + RealToString(BarrenPC,-12,-2) + ',' + RealToString(WaterPC,-12,-2);
               TileStats.Add(TileHeader);
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
      {$If Defined(RecordDEMIXFull)} HighlightLineToDebugFile('ProcessDEMIXtestarea in ' + TestAreaName); {$EndIf}
      ReportErrors := false;
      DEMIXtileDB := 0;
      LandCoverGrid := 0;
      AreaGridFull := 0;
      AreaAveElev := 0;
      AreaAveSlope := 0;
      AreaAveRuff := 0;
      AreaAveRelief := 0;
      AreaForestPC := 0;
      AreaUrbanPC := 0;
      AreaBarrenPC := 0;
      AreaWaterPC := 0;

      wmdem.SetPanelText(3,'Load reference DEM');
      DEMIXRefDEM := OpenNewDEM(RefDTMpointFName,false);
      if ValidDEM(DEMIXRefDEM) then begin
         DEMIXtileDB := DEMIXtileFill(DEMIXRefDEM,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,false);
         GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>' + IntToStr(MDDef.DEMIX_Full));
         {$IfDef RecordDEMIX} WriteLineToDebugFile('DTM tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}

         if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
            TStr := 'No filled DEMIX tiles on ' + AreaName;
            //ErrorLog.Add(Tstr);
            {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
         end
         else begin
            {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs) or Defined(Rec_DEMIX_Landcover)} OpenDEMsToDebugFile('Land cover loading'); {$EndIf}
            fName := DEMIX_area_lc100 + AreaName + '.tif';
            if FileExists(fname) then begin
               LandCoverGrid := OpenNewDEM(fName,false);
               if (DEMGlb[LandCoverGrid].DEMHeader.ElevUnits <> GLCS_LC100) then begin
                  DEMGlb[LandCoverGrid].DEMHeader.ElevUnits := GLCS_LC100;
                  DEMGlb[LandCoverGrid].WriteNewFormatDEM(fName);
               end;
               {$If Defined(Rec_DEMIX_Landcover)} OpenDEMsToDebugFile('Land cover already loaded ' + fName); {$EndIf}
            end
            else begin
               bb := DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo;
               LandCoverGrid := LoadLC100LandCover(fName,bb,Tstr,DEMIXopenMap);
               if ValidDEM(LandCoverGrid) then begin
                  {$If Defined(Rec_DEMIX_Landcover)} OpenDEMsToDebugFile('Land cover just loaded ' + fName); {$EndIf}
               end
               else begin
                  //ErrorLog.Add(TStr);
                  {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
               end;
            end;
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
               j := GISdb[DEMIXtileDB].MyData.FiltRecsInDB;
               DEMGlb[DEMIXRefDEM].DEMCenterPoint(LatCent,LongCent);
               TileHeader := AreaName + ',' + IntToStr(j) + ',' + RealToString(LatCent,-12,-2) + ',' + RealToString(LongCent,-12,-2) + ',' +
                  RealToString(AreaGridFull / j,-6,-2)   + ','  + RealToString(AreaAveElev / j,-12,-2) + ',' +
                  RealToString(AreaAveSlope / j,-12,-2)  + ',' + RealToString(AreaAveRuff / j,-12,-2)  + ',' +
                  RealToString(AreaAveRelief / j,-12,-2) + ',' + RealToString(AreaForestPC / j,-12,-2)  + ',' + RealToString(AreaUrbanPC / j,-12,-2) + ','
                  + RealToString(AreaBarrenPC / j,-12,-2) + ',' + RealToString(AreaWaterPC / j,-12,-2);
               AreaStats.Add(TileHeader);

            {$If Defined(RecordFullDEMIX)} for i := 1 to 2 do writeLineToDebugFile(''); {$EndIf}
         end;
         {$If Defined(RecordCriteriaEvaluation)} WriteLineToDebugFile('call CloseAndNilNumberedDB(DEMIXtileDB)'); {$EndIf}
         CloseAndNilNumberedDB(DEMIXtileDB);
      end;
      ReportErrors := true;
      {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('closing all DEMs'); {$EndIf}
      if DEMIXCloseMap then CloseAllDEMs;
    end {ProcessDEMIXtestarea};


var
   i : integer;
   AreasWanted : tStringList;
begin {procedure ComputeDEMIX_Summary_stats}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('Start ComputeDEMIXstats'); {$EndIf}
   try
      GetDEMIXpaths;
      SafeMakeDir(DEMIXresultsDir);
      AreasWanted := DEMIX_AreasWanted;

      {$IfDef RecordDEMIX} WriteLineToDebugFile('Areas to process in selected list=' + IntToStr(AreasWanted.Count)); {$EndIf}

      InitializeStringLists;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
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


procedure AddFilteredRankID(DBonTable : integer);
var
   i,ID : integer;
begin
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'FILT_ID',8);

   GISDb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   GISdb[DBonTable].MyData.First;
   ID := 0;
   while not GISdb[DBonTable].MyData.eof do begin
      GISdb[DBonTable].MyData.Edit;
      inc(ID);
      GISdb[DBonTable].MyData.SetFieldByNameAsInteger('FILT_ID',ID);
      GISdb[DBonTable].MyData.Next;
   end;
end;



procedure AddTileCharacteristics(DBonTable : integer);
var
   theFields : tStringList;
   i : integer;
begin
   if not FileExists(GISdb[DBonTable].dbOpts.LinkTableName) then begin
      GISdb[DBonTable].dbOpts.LinkTableName := DEMIXSettingsDir + 'demix_tiles_characteristics.dbf';
      GISdb[DBonTable].dbOpts.LinkFieldThisDB := 'DEMIX_TILE';
      GISdb[DBonTable].dbOpts.LinkFieldOtherDB := 'DEMIX_TILE';
   end;

   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   GISdb[DBonTable].ClearLinkTable(true);
   GISdb[DBonTable].LinkSecondaryTable(GISdb[DBonTable].dbOpts.LinkTableName);
   theFields := tStringList.Create;

   theFields.Add('LAT');
   theFields.Add('LONG');
   for I := 1 to NumTileCharacters do theFields.Add(TileCharacters[i]);
   GISdb[DBonTable].FillFieldsFromJoinedTable(TheFields,true);
   GISDb[DBonTable].ShowStatus;
end;


(*
//removed 3/5/2024 since we do the 1 minus when we create the DB
procedure SwitchSSIMorFUVScoring(DBonTable : integer);
//for 0-1 values with high score wins, reverses so low score wins
var
   i,j : integer;
   Eval : float32;
begin
   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   StartProgress('Switch scoring');
   j := 0;
   while not GISDb[DBonTable].MyData.eof do begin
      if (j mod 100 = 0) then begin
         UpdateProgressBar(j/GISDb[DBonTable].MyData.FiltRecsInDB);
         GISDb[DBonTable].EmpSource.Enabled := false;
      end;
      inc(j);
      GISDb[DBonTable].MyData.Edit;
      for I := 1 to NumDEMIXDEM do begin
         Eval := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
         GISDb[DBonTable].MyData.SetFieldByNameAsFloat(DEMIXShort[i],1-Eval);
      end;
      GISDb[DBonTable].MyData.Next;
   end;
   GISDb[DBonTable].ShowStatus;
end;
*)

procedure EvaluationRangeForCriterion(DBonTable : integer);
//adds field with the range between the best and worst evaluations
var
   i,j : integer;
   Eval,Max,Min : float32;
begin
   if GISdb[DBonTable].MyData.FieldExists('EVAL_RANGE') and GISdb[DBonTable].MyData.FieldExists('BEST_EVAL') then begin
   end
   else begin
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'EVAL_RANGE',12,6);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'BEST_EVAL',12,6);
      GISDb[DBonTable].ClearGISFilter;
      GISDb[DBonTable].EmpSource.Enabled := false;
      ShowHourglassCursor;
      StartProgress('Evaluation ranges');
      j := 0;
      while not GISDb[DBonTable].MyData.eof do begin
         if (j mod 50 = 0) then UpdateProgressBar(j / GISDb[DBonTable].MyData.FiltRecsInDb);
         inc(j);

         Max := -99e39;
         Min := 99e39;
         for I := 1 to NumDEMIXDEM do begin
            Eval := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXShort[i]);
            PetMath.CompareValueToExtremes(Eval,Min,Max);
         end;
         GISDb[DBonTable].MyData.Edit;
         GISDb[DBonTable].MyData.SetFieldByNameAsFloat('EVAL_RANGE',Max-Min);
         GISDb[DBonTable].MyData.SetFieldByNameAsFloat('BEST_EVAL',Min);
         GISDb[DBonTable].MyData.Next;
      end;
      GISDb[DBonTable].ShowStatus;
   end;
end;



