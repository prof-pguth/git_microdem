{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2015 Peter L. Guth   }
{____________________________________}


{ include file for nevadia_main}


procedure BatchResampleForDEMIX(FilesWanted : tStringList = Nil);
var
   fName : PathStr;
   i,DEM : integer;
begin
   {$If Defined(RecordIniMemoryOverwrite) or Defined(TimeLoadDEM)} IniMemOverwriteCheck('start OpenNewDEM'); {$EndIf}
   if (FilesWanted = Nil) then begin
      FilesWanted := tStringList.Create;
      FilesWanted.Add(LastDEMName);
      if not GetMultipleFiles('DEMs for aggregating',DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then exit;
   end;

   try
      HeavyDutyProcessing := true;
      StartSingleThreadTimer('Create DEMIX reference DEMs');
      for i := 0 to pred(FilesWanted.Count) do begin
        ThreadTimers.UpdateThreadStats(9, round(100 * (i / FilesWanted.Count)));

        fName := FilesWanted.Strings[i];
        LoadNewDEM(DEM,fName,true);
        if ValidDEM(DEM) then begin
            ResampleForDEMIX(DEM);
            CloseSingleDEM(DEM);
        end;
      end;
   finally
      EndThreadTimers;
      HeavyDutyProcessing := false;
   end;
end;




procedure ComputeDEMIXstats(FilesWanted : tStringList = Nil);
const
   AreaString = 'AREA,DEMIX_TILE,DEM,REF_DEM,REF_TYPE,REF_SLOPE,GRID_FULL,';
   MaxRefStore = 100;
   MaxTestDEM = 10;
var
   DoHorizontalShift,
   ElevDiffHists : boolean;

   SlopeMomentVar,ElevMomentVar : array[1..MaxRefStore] of tMomentVar;
   NumPits,NumPeaks : array[1..MaxRefStore] of integer;

   TestDEM : array[1..MaxTestDEM] of integer;
   TestSeries : array[1..MaxTestDEM] of shortstring;
   MomentVar,FlatMomentVar,SteepMomentVar,ForestMomentVar,UrbanMomentVar : tMomentVar;

   DEMIXtileDB,AddLocalVDatum,SubLocalVDatum,RefDTMpoint,RefDTMarea,RefDSMpoint,RefDSMarea,GeoidGrid,  COPRefDTM,
   RefDEM,LandCoverGrid,
   WantImage,NewDEM : integer;
   bbgrid : tGridLimits;
   GeodeticFName,LocalDatumAddFName,LocalDatumSubFName,LandCoverFName,
   RefDSMPointFName,RefDSMareaFName,RefDTMPointFName,RefDTMareaFName, COPRefDTMFName,
   fName,IceSatFName,OutputDir : PathStr;
   IndexSeriesTable : tMyData;
   TestAreaName,WantSeries,DEMIXtile,SlopeMask : shortstring;
   GridFull,Lat,Long : float64;
   zRef,z,z2 : float32;
   ICESatStats,
   LagStats,
   SlopeMoments,
   ElevMoments,
   ElevDiffStats,
   RufDiffStats,
   SlopeDiffStats,
   PitsPeaks,
   CorrelationCoefficientsStats,
   ElevMomentDiffStats,SlopeMomentDiffStats,
   GeodeticStats : tStringList;
   zs,zsSteep,ZSFlat,zsForest,zsUrban : ^bfarray32;
   Rules,
   GeodeticTable,
   IceSatTable : tMyData;
   SlopeAspectRef,SlopeAspectTest : tSlopeAspectRec;
   ElevFiles,LegendFiles : tStringList;


         procedure InitializeStringLists;
         const
            SlopeDiffStatsString = 'SMD_MIN,SMD_Max,SMD_Mean,SMD_AVG,SMD_STD,SMD_MEDIAN,SMD_RMSE,SMD_MAE,SMD_AB_MD,SMD_AB_MN,SMD_N,SMD_LE90';
            ElevDiffStatsString =  'ELD_MIN,ELD_Max,ELD_Mean,ELD_AVG,ELD_STD,ELD_MEDIAN,ELD_RMSE,ELD_MAE,ELD_AB_MD,ELD_AB_MN,ELD_N,ELD_LE90';
            RufDiffStatsString =  'RUFD_MIN,RUFD_Max,RUFD_Mean,RUFD_AVG,RUFD_STD,RUFD_MED,RUFD_RMSE,RUFD_MAE,RUFD_AB_MD,RUFD_AB_MN,RUFD_N,RUFD_LE90';

            IcePointsStatsString = 'DICE_MIN,DICE_Max,DICE_Mean,DICE_AVG,DICE_STD,DICE_MEDI,DICE_RMSE,DICE_MAE,DICE_N,DICE_AMED';
            BMPointsStatsString = 'DBM_MIN,DBM_Max,DBM_Mean,DBM_AVG,DBM_STD,DBM_MEDI,DBM_RMSE,DBM_MAE,DBM_N,DBM_AMED';

            ElevMomentsString = 'ELEV_MIN,ELEV_Max,ELEV_Mean,ELEV_ADEV,ELEV_STD,ELEV_SKEW,ELEV_KURT,ELEV_MED,N';
            SlopeMomentsString = 'SLP_MIN,SLP_Max,SLP_Mean,SLP_ADEV,SLP_STDV,SLP_SKEW,SLP_KURT,SLP_MED,SLP_N';

            SlopeMomentsDiffString = 'SDIFF_MIN,SDIFF_Max,SDIFF_Mean,SDIFF_STD,SDIFF_SKEW,SDIFF_KURT,SDIFF_MED,SDIFF_N';
            ElevMomentsDiffString = 'EDIFF_MIN,EDIFF_Max,EDIFF_Mean,EDIFF_STD,EDIFF_SKEW,EDIFF_KURT,EDIFF_MED,EDIFF_N';

            LagStatsString = 'Lag_Max,Lag_Mean,Lag_STD,LAG_MEDIAN,LAG_LE90,LAG_NPTS';
         begin
            LagStats := tStringList.Create;
            LagStats.Add(AreaString + LagStatsString);

            ElevDiffStats := tStringList.Create;
            ElevDiffStats.Add(AreaString + ElevDiffStatsString + ',SLOPE_PC');

            RufDiffStats := tStringList.Create;
            RufDiffStats.Add(AreaString + RufDiffStatsString + ',SLOPE_PC');

            SlopeDiffStats := tStringList.Create;
            SlopeDiffStats.Add(AreaString + SlopeDiffStatsString + ',SLOPE_PC');

            ElevMomentDiffStats := tStringList.Create;
            ElevMomentDiffStats.Add(AreaString + ElevMomentsDiffString);

            SlopeMomentDiffStats := tStringList.Create;
            SlopeMomentDiffStats.Add(AreaString + SlopeMomentsDiffString);

            SlopeMoments := tStringList.Create;
            SlopeMoments.Add(MomentStr + ',' + SlopeMomentsString);

            ElevMoments := tStringList.Create;
            ElevMoments.Add(MomentStr + ',' + ElevMomentsString);

            PitsPeaks := tStringList.Create;
            PitsPeaks.Add(AreaString + 'NUM_PITS,NUM_PEAKS,DIFF_PITS,DIFF_PEAKS');

            ICESatStats := tStringList.Create;
            ICESATStats.Add(AreaString + IcePointsStatsString);

            GeodeticStats := tStringList.Create;
            GeodeticStats.Add(AreaString + BMPointsStatsString);

            CorrelationCoefficientsStats := tStringList.Create;
            CorrelationCoefficientsStats.Add(AreaString + 'R2,INTERCEPT,SLOPE');

            {$IfDef RecordDEMIX} writeLineToDebugFile('InitializeStringLists out, string lists created'); {$EndIf}
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
               var
                  db : integer;
               begin
                  {$IfDef RecordFullDEMIX} writeLineToDebugFile(fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                  if (sl.count > 1) then begin
                     fName := OutputDir {+ TStr} + fname + '.csv';
                     sl.SaveToFile(fName);
                     if DoStats and FinalSaveResults then begin
                        {$IfDef RecordDEMIX} writeLineToDebugFile(fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                        db := OpenDataBase('',fName);
                        //GISdb[db].dbtablef.Graphsforwinecontest1Click(Nil);
                        GISdb[db].dbtablef.ransposeforwinecontest1Click(Nil);
                     end;
                  end
                  else fName := '';
                  if FinalSaveResults then sl.Destroy;
               end;

         begin
            {$IfDef RecordFullDEMIX} writeLineToDebugFile('DEMIX start string list processing'); {$EndIf}
            ProcessStringList('ICESat_best_diff_stats',ICESATStats);
            ProcessStringList('Geodetic_control_stats',GeodeticStats);

            ProcessStringList('Elev_diff_stats',ElevDiffStats,true);
            ProcessStringList('Roughness_diff_stats',RufDiffStats,true);
            ProcessStringList('Slope_diff_stats',SlopeDiffStats,true);

            SortListWithHeader(ElevMoments);
            ProcessStringList('Elevation_moments',ElevMoments);
            SortListWithHeader(SlopeMoments);
            ProcessStringList('Slope_moments',SlopeMoments);

            ProcessStringList('Elevation_moments_diff',ElevMomentDiffStats);
            ProcessStringList('Slope_moments_diff',SlopeMomentDiffStats);
            ProcessStringList('Pits_and_peaks',PitsPeaks);
            ProcessStringList('Horizonatal_lags',LagStats);
            ProcessStringList('Full_tile_correlation',CorrelationCoefficientsStats);
         end;



   procedure ProcessDEMIXtestarea;


         function LineHeader(DEM,RefDEM : integer) : shortstring;
         var
            t1,t2,t3 : shortstring;
         begin
             if (DEM = 0) then t1 := 'xxx' else t1 := DEMGlb[DEM].AreaName;
             if (RefDEM = 0) then begin
                t2 := 'xxx';
                t3 := 'xxx';
             end
             else begin
                t2 := DEMGlb[RefDEM].AreaName;
                if (RefDEM = RefDSMpoint) or (RefDEM = RefDSMarea) then t3 := 'DSM'
                else t3 := 'DTM';
             end;
             Result := TestAreaName + ',' + DEMIXtile + ',' + t1 + ',' + t2 + ',' + t3 + ',' + SlopeMask + ',' + RealToString(GridFull,-8,2) + ',';
         end;


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
         begin
            GridFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
            Result := GridFull >= MDDef.DEMIX_Full;
            DEMIXtile := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
         end;

         function MomentStatsString(MomentVar : tMomentVar) : shortstring;
         begin
            Result := RealToString(MomentVar.MinZ,-8,2) + ',' + RealToString(MomentVar.MaxZ,-8,2) + ',' + RealToString(MomentVar.Mean,-8,2) + ',' +
                RealToString(MomentVar.adev,-8,2) + ',' + RealToString(MomentVar.sdev,-8,2) + ',' + RealToString(MomentVar.median,-8,2) + ',' + RealToString(MomentVar.rmse,-8,2)  + ',' +
                RealToString(MomentVar.mae,-8,2) + ',' + IntToStr(MomentVar.NPts);
         end;

         function MomentStatsStringWithABS(MomentVar : tMomentVar; var bfArray32) : shortstring;
         var
            MedianAbs : float64;
            i : integer;
         begin
            for i := 0 to pred(MomentVar.NPts) do zs^[i] := abs(zs^[i]);
            MedianAbs := Median(zs^,MomentVar.NPts,false);
            Result := RealToString(MomentVar.MinZ,-8,2) + ',' +
                           RealToString(MomentVar.MaxZ,-8,2) + ',' + RealToString(MomentVar.Mean,-8,2) + ',' + RealToString(MomentVar.adev,-8,2) + ',' + RealToString(MomentVar.sdev,-8,2) + ',' +
                           RealToString(MomentVar.median,-8,2) + ',' + RealToString(MomentVar.rmse,-8,2) + ',' + RealToString(MomentVar.mae,-8,2)  + ',' +
                           RealToString(MedianAbs,-8,2) + ',' + RealToString(abs(MomentVar.Mean),-8,2) + ',' + IntToStr(MomentVar.NPts);
         end;


         function MomentDifferenceString(aMomentVar,RefMomentVar : tMomentVar) : shortstring;
         begin
             Result := RealToString(aMomentVar.MinZ-RefMomentVar.minZ,-8,2) + ',' + RealToString(aMomentVar.MaxZ-RefMomentVar.MaxZ,-8,2) + ',' +
                            RealToString(aMomentVar.Mean-RefMomentVar.mean,-8,2) + ',' + RealToString(aMomentVar.sdev-RefMomentVar.sdev,-8,2) + ',' +
                            RealToString(aMomentVar.skew-RefMomentVar.skew,-8,2) + ',' + RealToString(aMomentVar.curt-RefMomentVar.Curt,-8,2) + ',' +
                            RealToString(aMomentVar.median-RefMomentVar.median,-8,2) + ',' + IntToStr(aMomentVar.NPts);
         end;


procedure WriteDifferenceResult(var sl : tStringList; DEM,RefDEM : integer; var thezs : bfarray32; var theMoments : tMomentVar; Percent : float32);
var
   i : integer;
begin
   if (theMoments.NPts > 0) then begin
      moment(thezs,theMoments,msAll);
      for i := 0 to pred(theMoments.NPts) do thezs[i] := abs(thezs[i]);
      theMoments.LE90 := Percentile(90,thezs,theMoments.NPts,false);
      sl.Add(LineHeader(DEM,RefDEM) + MomentStatsStringWithABS(theMoments,thezs) + ',' + RealToString(theMoments.LE90,-12,-2) + ',' + RealToString(Percent,-12,-2));
   end;
end;



         procedure DoICESat(DEM : integer);
         var
            Lat,Long,MedianAbs : float64;
            i : integer;
            z,z2 : float32;
            MomentVar : tMomentVar;
         begin
            if (IceSatFName = '') then exit;
            {$IfDef RecordDEMIX} writeLineToDebugFile('ICESat-2 ' + DEMGLB[DEM].AreaName); {$EndIf}
            GridFull := 9999;
            IceSatTable.First;
            MomentVar.Npts := 0;
            while not ICESatTable.eof do begin
               Lat := IceSatTable.GetFieldByNameAsFloat('LAT');
               Long := IceSatTable.GetFieldByNameAsFloat('LONG');
               Z := IceSatTable.GetFieldByNameAsFloat('ICESAT_GRD');
               if DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                  zs^[MomentVar.NPts] := z2-z;
                  inc(MomentVar.Npts);
               end;
               ICESatTable.Next;
            end;
            moment(zs^,MomentVar,msAll);
            for i := 0 to pred(MomentVar.NPts) do zs^[i] := abs(zs^[i]);
            MedianAbs := Median(zs^,MomentVar.NPts,false);
            ICESatStats.Add(LineHeader(DEM,0) + MomentStatsString(MomentVar) + ',' + RealToString(MedianAbs,-8,2));
         end;


         procedure DoGeodetic(DEM : integer);
         var
            Lat,Long,MedianAbs : float64;
            i : integer;
            z,z2 : float32;
            MomentVar : tMomentVar;
         begin
            if (GeodeticFName = '') then exit;
            {$IfDef RecordDEMIX} writeLineToDebugFile('geodetic ' + DEMGLB[DEM].AreaName); {$EndIf}
            GridFull := 9999;
            GeodeticTable.First;
            MomentVar.Npts := 0;
            while not GeodeticTable.eof do begin
               Lat := GeodeticTable.GetFieldByNameAsFloat('LAT');
               Long := GeodeticTable.GetFieldByNameAsFloat('LONG');
               Z := GeodeticTable.GetFieldByNameAsFloat('Z_EGM2008');
               if DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                  zs^[MomentVar.NPts] := z2-z;
                  inc(MomentVar.Npts);
               end;
               GeodeticTable.Next;
            end;
            if MomentVar.NPts > 0 then begin
               moment(zs^,MomentVar,msAll);
               for i := 0 to pred(MomentVar.NPts) do zs^[i] := abs(zs^[i]);
               MedianAbs := Median(zs^,MomentVar.NPts,false);
               GeodeticStats.Add(LineHeader(DEM,0) + MomentStatsString(MomentVar) + ',' + RealToString(MedianAbs,-8,2));
            end;
         end;


           procedure CheckVerticalDatumShift(var DEM : integer);
           //Reproject vertical datum to EGM2008 if required because DEM is EGM96
           var
              Col,Row : integer;
              VertDatum : shortstring;
           begin
               if ValidDEM(DEM) then begin
                  VertDatum := IndexSeriesTable.GetFieldByNameAsString('VERT_DATUM');
                  if (VertDatum = '5773') then begin
                     DEMGlb[DEM].ResaveNewResolution(fcSaveFloatingPoint,NewDEM);
                     DEMGlb[NewDEM].AreaName := DEMGlb[DEM].AreaName;  // + '_egm2008';
                     for Col := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumCol) do begin
                        for Row := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumRow) do begin
                            if DEMGlb[NewDEM].GetElevMetersOnGrid(Col,Row,z) then begin
                               DEMGlb[NewDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                               if DEMGlb[GeoidGrid].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                                  DEMGlb[NewDEM].SetGridElevation(Col,Row,z+z2);
                               end;
                            end;
                        end;
                     end;
                     CloseSingleDEM(DEM);
                     DEMGlb[NewDEM].CheckMaxMinElev;
                     WriteLineToDebugFile(DEMGlb[NewDEM].AreaName + '  ' + DEMGlb[NewDEM].ZRange);
                     DEM := NewDEM;
                  end;
               end;
            end;

                  procedure ZeroMomentVar;
                  begin
                     MomentVar.Npts := 0;
                     FlatMomentVar.Npts := 0;
                     SteepMomentVar.Npts := 0;
                     UrbanMomentVar.Npts := 0;
                     ForestMomentVar.Npts := 0;
                  end;



           procedure CompareElevationsToReferenceDEM(DEM,RefDEM : integer; RefType : shortstring);
           //do full DEM pixel by pixel comparison to reference DEM
           var
              //i,
              Col,Row : integer;
              xg,yg,Difference : float32;
              SlopeAsp : tSlopeAspectRec;
            begin
               if (RefDEM = 0) then exit;
               {$IfDef RecordFullDEMIX} writeLineToDebugFile('Elevation comparison to reference, full DEM ' + DEMGLB[DEM].AreaName); {$EndIf}
               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  if UseThisTile then begin
                    ZeroMomentVar;

                     bbgrid := GridBoundingBox(DEM,RefDEM,true);
                     {$IfDef RecordFullDEMIX} WriteTileDetails(bbgrid); {$EndIf}
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[DEM].GetElevMetersOnGrid(col,row,z) then begin
                              DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                              DEMGlb[RefDEM].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
                              if DEMGlb[RefDEM].GetElevMetersOnGrid(round(xg),round(yg),zref) then begin
                                 Difference := z-zref;
                                 zs^[MomentVar.NPts] := Difference;
                                 inc(MomentVar.Npts);

                                 if DEMGlb[RefDEM].GetSlopeAndAspect(round(xg),round(yg),SlopeAsp) then begin
                                    if SlopeAsp.SlopePercent < MDDef.SlopeSteepBoundary then begin
                                       zsflat^[FlatMomentVar.NPts] := Difference;
                                       inc(FlatMomentVar.Npts);
                                    end
                                    else begin
                                       zsSteep^[SteepMomentVar.NPts] := Difference;
                                       inc(SteepMomentVar.Npts);
                                    end;
                                 end;

                                 if (LandCoverGrid <> 0) and DEMGlb[LandCoverGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
                                    if round(z) in [111..126] then begin
                                       zsForest^[ForestMomentVar.NPts] := Difference;
                                       inc(ForestMomentVar.Npts);
                                    end
                                    else if round(z) in [50] then begin
                                       zsUrban^[UrbanMomentVar.NPts] := Difference;
                                       inc(UrbanMomentVar.Npts);
                                    end;
                                 end;
                              end;
                           end;
                        end;
                     end;

                     if (MomentVar.NPts > 0) and ElevDiffHists then begin
                           fName := OutputDir + 'elev_diff_hists\' + DEMGLB[DEM].AreaName  + '_to_' + RefType + '-' + DEMIXtile + '.z';
                           ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zs^,fName));
                           LegendFiles.Add(ExtractFileNameNoExt(fName));
                     end;


                     SlopeMask := 'ALL';
                     WriteDifferenceResult(ElevDiffStats,DEM,RefDEM,zs^,MomentVar,100);
                     SlopeMask := 'FLAT';
                     WriteDifferenceResult(ElevDiffStats,DEM,RefDEM,zsFlat^,FlatMomentVar,(100 *FlatMomentVar.NPts/MomentVar.NPts));
                     SlopeMask := 'STEEP';
                     WriteDifferenceResult(ElevDiffStats,DEM,RefDEM,zsSteep^,SteepMomentVar,(100 *SteepMomentVar.NPts/MomentVar.NPts));
                     SlopeMask := 'FOREST';
                     if (LandCoverGrid <> 0) then WriteDifferenceResult(ElevDiffStats,DEM,RefDEM,zsForest^,ForestMomentVar,(100 *ForestMomentVar.NPts/MomentVar.NPts));
                     SlopeMask := 'URBAN';
                     if (LandCoverGrid <> 0) then WriteDifferenceResult(ElevDiffStats,DEM,RefDEM,zsUrban^,UrbanMomentVar,(100 *UrbanMomentVar.NPts/MomentVar.NPts));
                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               SlopeMask := 'ALL';
            end;

            procedure CompareSlopesToReferenceDEM(DEM,RefDEM : integer; RefType : shortstring);
            //do full DEM pixel by pixel comparison to reference DEM
            var
               //i,
               Col,Row,xg,yg : integer;
               SlopeAsp : tSlopeAspectRec;
               Difference : float32;
            begin
               if RefDEM = 0 then exit;
               {$IfDef RecordFullDEMIX} writeLineToDebugFile('Slope comparison to reference, full DEM ' + DEMGLB[DEM].AreaName); {$EndIf}
               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  if UseThisTile then begin
                     ZeroMomentVar;
                     bbgrid := GridBoundingBox(DEM,RefDEM,true);
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[DEM].GetSlopeAndAspect(col,row,SlopeAspectRef) then begin
                              DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                              if DEMGlb[RefDEM].GetSlopeAndAspectFromLatLong(Lat,Long,SlopeAspectTest) then begin
                                 zs^[MomentVar.NPts] := SlopeAspectTest.SlopePercent - SlopeAspectRef.SlopePercent;
                                 inc(MomentVar.Npts);
                              end;
                              DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                              if DEMGlb[RefDEM].GetSlopeAndAspect(xg,yg,SlopeAsp) then begin
                                 Difference := SlopeAspectTest.SlopePercent - SlopeAspectRef.SlopePercent;
                                 if SlopeAsp.SlopePercent < MDDef.SlopeSteepBoundary then begin
                                    zsflat^[FlatMomentVar.NPts] := difference;
                                    inc(FlatMomentVar.Npts);
                                 end
                                 else begin
                                    zsSteep^[SteepMomentVar.NPts] := Difference;
                                    inc(SteepMomentVar.Npts);
                                 end;

                                 if (LandCoverGrid <> 0) and DEMGlb[LandCoverGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
                                    if round(z) in [111..126] then begin
                                       zsForest^[ForestMomentVar.NPts] := Difference;
                                       inc(ForestMomentVar.Npts);
                                    end
                                    else if round(z) in [50] then begin
                                       zsUrban^[UrbanMomentVar.NPts] := Difference;
                                       inc(UrbanMomentVar.Npts);
                                    end;
                                 end;
                              end;
                           end;
                        end;
                     end;
                     if (MomentVar.NPts > 1) and ElevDiffHists then begin
                        fName := OutputDir + 'slope_diff_hists\' + DEMGLB[DEM].AreaName  + '_to_' + RefType + '-' + DEMIXtile + '.z';
                        SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                     end;

                     SlopeMask := 'ALL';
                     WriteDifferenceResult(SlopeDiffStats,DEM,RefDEM,zs^,MomentVar,100);
                     SlopeMask := 'FLAT';
                     WriteDifferenceResult(SlopeDiffStats,DEM,RefDEM,zsFlat^,FlatMomentVar,(100 *FlatMomentVar.NPts/MomentVar.NPts));
                     SlopeMask := 'STEEP';
                     WriteDifferenceResult(SlopeDiffStats,DEM,RefDEM,zsSteep^,SteepMomentVar,(100 *SteepMomentVar.NPts/MomentVar.NPts));
                     SlopeMask := 'FOREST';
                     if (LandCoverGrid <> 0) then WriteDifferenceResult(SlopeDiffStats,DEM,RefDEM,zsForest^,ForestMomentVar,(100 *ForestMomentVar.NPts/MomentVar.NPts));
                     SlopeMask := 'URBAN';
                     if (LandCoverGrid <> 0) then WriteDifferenceResult(SlopeDiffStats,DEM,RefDEM,zsUrban^,UrbanMomentVar,(100 *UrbanMomentVar.NPts/MomentVar.NPts));

                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               SlopeMask := 'ALL';
            end;


            procedure CompareRoughNessToReferenceDEM(DEM,RefDEM : integer; RefType : shortstring);
            //do full DEM pixel by pixel comparison to reference DEM
            var
               //i,
               Col,Row,xg,yg : integer;
               SlopeAsp : tSlopeAspectRec;
               Ruff1,Ruff2,z,Difference : float32;
            begin
               if RefDEM = 0 then exit;
               {$IfDef RecordFullDEMIX} writeLineToDebugFile('Roughness comparison to reference, full DEM ' + DEMGLB[DEM].AreaName); {$EndIf}
               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  if UseThisTile then begin
                     ZeroMomentVar;
                     bbgrid := GridBoundingBox(DEM,RefDEM,true);
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[DEM].RoughnessFromSlopeSTD(Col,Row,MDDef.RoughnessBox,Ruff1) then begin
                              DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                              DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                              if DEMGlb[RefDEM].RoughnessFromSlopeSTD(xg,yg,MDDef.RoughnessBox,Ruff2) then begin
                                 Difference := Ruff1 - Ruff2;
                                 zs^[MomentVar.NPts] := Difference;
                                 inc(MomentVar.Npts);

                                 if DEMGlb[RefDEM].GetSlopeAndAspect(round(xg),round(yg),SlopeAsp) then begin
                                    if SlopeAsp.SlopePercent < MDDef.SlopeSteepBoundary then begin
                                       zsflat^[FlatMomentVar.NPts] := Difference;
                                       inc(FlatMomentVar.Npts);
                                    end
                                    else begin
                                       zsSteep^[SteepMomentVar.NPts] := Difference;
                                       inc(SteepMomentVar.Npts);
                                    end;
                                 end;

                                 if (LandCoverGrid <> 0) and DEMGlb[LandCoverGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
                                    if round(z) in [111..126] then begin
                                       zsForest^[ForestMomentVar.NPts] := Difference;
                                       inc(ForestMomentVar.Npts);
                                    end
                                    else if round(z) in [50] then begin
                                       zsUrban^[UrbanMomentVar.NPts] := Difference;
                                       inc(UrbanMomentVar.Npts);
                                    end;
                                 end;
                              end;
                           end;
                        end;
                     end;
                     if (MomentVar.NPts > 1) then begin
                        if ElevDiffHists then begin
                           fName := OutputDir + 'ruff_diff_hists\' + DEMGLB[DEM].AreaName  + '_to_' + RefType + '-' + DEMIXtile + '.z';
                           SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                        end;

                        SlopeMask := 'ALL';
                        WriteDifferenceResult(RufDiffStats,DEM,RefDEM,zs^,MomentVar,100);
                        SlopeMask := 'FLAT';
                        WriteDifferenceResult(RufDiffStats,DEM,RefDEM,zsFlat^,FlatMomentVar,(100 *FlatMomentVar.NPts/MomentVar.NPts));
                        SlopeMask := 'STEEP';
                        WriteDifferenceResult(RufDiffStats,DEM,RefDEM,zsSteep^,SteepMomentVar,(100 *SteepMomentVar.NPts/MomentVar.NPts));
                        SlopeMask := 'FOREST';
                        if (LandCoverGrid <> 0) then WriteDifferenceResult(RufDiffStats,DEM,RefDEM,zsForest^,ForestMomentVar,(100 *ForestMomentVar.NPts/MomentVar.NPts));
                        SlopeMask := 'URBAN';
                        if (LandCoverGrid <> 0) then WriteDifferenceResult(RufDiffStats,DEM,RefDEM,zsUrban^,UrbanMomentVar,(100 *UrbanMomentVar.NPts/MomentVar.NPts));
                     end;
                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               SlopeMask := 'ALL';
            end;


            procedure ComputeCorrelationCoefficient(DEM,RefDEM : integer);
            var
               Col,Row,Npts : integer;
               x,y : ^bfarray32;
               //xs,ys : integer;
               Lat,Long : float64;
               a,b,siga,sigb,r,{xg,yg,}z,zref : float32;
            begin
               if (RefDEM = 0) then exit;
               New(x);
               New(y);
               {$IfDef RecordDEMIX} writeLineToDebugFile('DoCorrelationCoefficient DEM=' + DEMGLB[DEM].AreaName); {$EndIf}
               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  if UseThisTile then begin
                     Npts := 0;
                     bbgrid := GridBoundingBox(DEM,RefDEM,true);
                     for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                        for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                           if DEMGlb[DEM].GetElevMetersOnGrid(col,row,z) then begin
                              DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                              if DEMGlb[RefDEM].GetElevFromLatLongDegree(Lat,Long,zref) then begin
                                 x^[NPts] := zref;
                                 y^[NPts] := z;
                                 inc(NPts);
                              end;
                           end;
                        end;
                     end;
                     fit(x^,y^,NPts,a,b,siga,sigb,r);
                     r := r*r;
                     CorrelationCoefficientsStats.Add(LineHeader(DEM,RefDEM) + RealToString(r,-12,-5) + ',' + RealToString(a,-8,-4) + ',' + RealToString(b,-8,-4) );
                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               Dispose(x);
               Dispose(y);
            end;


         procedure DoElevMoments(DEM,RefDEM : integer);
         var
            aMomentVar : tMomentVar;
            gbb : tGridLimits;
         begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('DoElevMoments, test DEM ' + DEMGLB[DEM].AreaName); {$EndIf}
            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               if UseThisTile then begin
                  aMomentVar.NPts := 0;
                  gbb := GridBoundingBox(DEM,RefDEM,true);
                  {$IfDef RecordFullDEMIX} WriteTileDetails(gbb); {$EndIf}
                  aMomentVar := DEMGlb[DEM].ElevationMoments(gbb);
                  ElevMoments.Add(DEMIXtile + ' ' + DEMGlb[DEM].AreaName +  MomentResultsToString(aMomentVar) + ',' + MomentStatsString(aMomentVar));
                  if (RefDEM = 0) then begin
                     //save to use for differences
                     ElevMomentVar[DEM] := aMomentVar;
                  end
                  else begin
                     ElevMomentDiffStats.Add(LineHeader(DEM,RefDEM) + ',' +  MomentDifferenceString(aMomentVar,ElevMomentVar[RefDEM]));
                  end;
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
         end;

         procedure DoSlopeMoments(DEM,RefDEM : integer);
         var
            aMomentVar : tMomentVar;
            gbb : tGridLimits;
         begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('DoSlopeMoments, test DEM ' + DEMGlb[DEM].AreaName); {$EndIf}
            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               if UseThisTile then begin
                  aMomentVar.NPts := 0;
                  gbb := GridBoundingBox(DEM,RefDEM,true);
                  DEMGlb[DEM].SlopeMoments(gbb,aMomentVar);
                  if (aMomentVar.Npts > 1) then begin
                     {$IfDef RecordFullDEMIX} writeLineToDebugFile('Slope moments, ' + LineHeader(DEM,RefDEM)); {$EndIf}
                     {$IfDef RecordFullDEMIX} writeLineToDebugFile(DEMIXtile + ' ' + DEMGlb[DEM].AreaName + MomentStatsString(aMomentVar) + ',' + MomentResultsToString(aMomentVar)); {$EndIf}
                     SlopeMoments.Add(DEMIXtile + ' ' + DEMGlb[DEM].AreaName  + MomentResultsToString(aMomentVar) + ',' + MomentStatsString(aMomentVar));
                     if (RefDEM = 0) then begin
                        //save to use for differences
                        SlopeMomentVar[DEM] := aMomentVar;
                     end
                     else begin
                        SlopeMomentDiffStats.Add(LineHeader(DEM,RefDEM) + MomentDifferenceString(aMomentVar,SlopeMomentVar[RefDEM]));
                     end;
                  end;
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
         end;


         procedure DoPeakPits(DEM,RefDEM : integer);
         var
            DummyResults : tStringList;
            Pits,Peaks : integer;
         begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('Peak and pits, full DEM'); {$EndIf}
            DummyResults := tStringList.Create;
            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               if UseThisTile then begin
                  Pits := FindPits(DEM,GridBoundingBox(DEM,RefDEM),DummyResults,Nil);
                  Peaks := FindPeaks(DEM,GridBoundingBox(DEM,RefDEM),DummyResults,Nil);
                  if (RefDEM = 0) then begin
                     NumPits[DEM] := Pits;
                     NumPeaks[DEM] := Peaks;
                  end
                  else begin
                     PitsPeaks.Add(LineHeader(DEM,RefDEM) + IntToStr(Pits) + ',' + IntToStr(Peaks) + ',' + IntToStr(Pits-NumPits[RefDEM]) + ',' + IntToStr(Peaks-NumPeaks[RefDEM]) );
                  end;
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
         end;


         procedure DoHorizontalShift(DEM,RefDEM : integer);
         var
            BigResults : tStringList;
            fName : PathStr;
            AllLags,db : integer;
            aLine : shortstring;
            aMomentVar : tMomentVar;
         begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('DoHorizontalShift ' + DEMGLB[DEM].AreaName); {$EndIf}
            //the correlation window
            MDDef.LagSearchRadius := 10;

            //the sampling factor
            MDDef.LagCenterShift := 10;

            //the exploration window
            AllLags := 10;
            MDDef.ShiftLoX := -AllLags;
            MDDef.ShiftHighX := AllLags;
            MDDef.ShiftLoY := -AllLags;
            MDDef.ShiftHighY := AllLags;

            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               if UseThisTile then begin
                  BigResults := Nil;
                  OneLag(RefDEM,DEM,GridBoundingBox(RefDEM,0),BigResults);
                  fname := OutputDir + 'lags\' + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_' + DEMGLB[RefDEM].AreaName +  '.dbf';
                  {$IfDef RecordFullDEMIX} writeLineToDebugFile('Output=' + fName); {$EndIf}
                  db := StringList2CSVtoDB(BigResults, fName);
                  if (db <> 0) then begin
                     aMomentVar := GISdb[db].GetFieldStatistics('TOTAL_LAG');
                     aMomentVar.LE90 := Percentile(90,zs^,aMomentVar.NPts,false);
                     aLine := RealToString(aMomentVar.MaxZ,-8,2) + ',' + RealToString(aMomentVar.Mean,-8,2) + ',' + RealToString(aMomentVar.sdev,-8,2) + ',' + RealToString(aMomentVar.median,-8,2) +
                         ',' + RealToString(aMomentVar.LE90,-8,2) + ',' + IntToStr(aMomentVar.NPts);
                     {$IfDef RecordFullDEMIX} writeLineToDebugFile(aline); {$EndIf}
                     LagStats.Add(LineHeader(DEM,RefDEM) + aLine);
                     CloseAndNilNumberedDB(db);
                  end;
               end;
               GISdb[DEMIXtileDB].MyData.Next;
            end;
         end;


         procedure ReferenceDEMcriteria(DEM : integer);
         begin
            if (DEM <> 0) then begin
               DEMIXTile := '';
               (*
               DoSlopeMoments(DEM,0);
               DoElevMoments(DEM,0);
               DoICESat(DEM);
               DoGeodetic(DEM);
               DoPeakPits(DEM,0);
               *)
            end;
         end;


         procedure CompareToReference(DEM,RefDEM : integer; RefType : shortstring);
         begin
            if (RefDEM <> 0) then begin
               {$IfDef RecordDEMIX} writeLineToDebugFile('CompareToReference, DEM=' + IntToStr(DEM) + '   ref=' + IntToStr(RefDEM)); {$EndIf}
               CompareRoughNessToReferenceDEM(DEM,RefDEM,RefType);
               CompareElevationsToReferenceDEM(DEM,RefDEM,RefType);
               CompareSlopesToReferenceDEM(DEM,RefDEM,RefType);

               (*
               DoSlopeMoments(DEM,RefDEM);
               DoElevMoments(DEM,RefDEM);
               if DoHorizontalShift then DoHorizontalShift(DEM,RefDEM);
               ComputeCorrelationCoefficient(DEM,RefDEM);
               DoPeakPits(DEM,RefDEM);
               *)
            end;
         end;



         function LoadReferenceDEMs : boolean;

                  procedure ReferenceFileOpen(var DEM : integer; fName : PathStr);
                  begin
                     if (FName <> '') and FileExists(fName) then begin
                        DEM := OpenNewDEM(FName);   //must load map for the DEMIX tile computation
                        if (DEM <> 0) then begin
                           //move to EGM2008
                           if AddLocalVDatum <> 0 then DEMGlb[DEM].AddaDEM(AddLocalVDatum);
                           if SubLocalVDatum <> 0 then DEMGlb[DEM].SubtractaDEM(AddLocalVDatum);
                           TestAreaName := LastSubDir(ExtractFilePath(fName));
                           if (RefDEM = 0) then RefDEM := DEM;
                        end;
                     end
                     else DEM := 0;
                  end;

         begin
            if (LocalDatumAddFName <> '') and FileExists(LocalDatumAddFName) then AddLocalVDatum := OpenNewDEM(LocalDatumAddFName, false);
            if (LocalDatumSubFName <> '') and FileExists(LocalDatumSubFName) then SubLocalVDatum := OpenNewDEM(LocalDatumSubFName, false);

            ReferenceFileOpen(RefDTMpoint,RefDTMpointFName);
            ReferenceFileOpen(RefDTMarea,RefDTMareaFName);
            ReferenceFileOpen(RefDSMpoint,RefDSMpointFName);
            ReferenceFileOpen(RefDSMarea,RefDSMareaFName);
            ReferenceFileOpen(COPRefDTM,COPRefDTMFName);
            Result := ValidDEM(RefDEM);
            if Result then begin

               {$IfDef RecordDEMIX} writeLineToDebugFile('ProcessDEMIXtestarea in, ref DEMs open'); {$EndIf}
               if (AddLocalVDatum <> 0) then CloseSingleDEM(AddLocalVDatum);
               if (SubLocalVDatum <> 0) then CloseSingleDEM(SubLocalVDatum);

               GeoidGrid := OpenNewDEM(GeoidDiffFName,false,'geoid difference from EGM96 to EGM2008');  //to move DEMs from EGM96 to EGM2008
               GeoidDiffFName := DEMGlb[GeoidGrid].DEMFileName;
               {$IfDef RecordDEMIX} writeLineToDebugFile('ProcessDEMIXtestarea in, geoid grid opened, REFDEM=' + IntToStr(RefDEM)); {$EndIf}
            end
            else begin
               {$IfDef RecordDEMIX} writeLineToDebugFile('Failure, ref DEMs open'); {$EndIf}
            end;
         end;



   var
      WantDEM,Ser,i,j : integer;
   begin
      {$IfDef RecordDEMIX} writeLineToDebugFile(''); writeLineToDebugFile(''); writeLineToDebugFile('ProcessDEMIXtestarea in'); writeLineToDebugFile(''); {$EndIf}

      ReportErrors := false;

      DEMIXtileDB := 0;
      RefDEM := 0;
      AddLocalVDatum := 0;
      SubLocalVDatum := 0;
      LandCoverGrid := 0;

      for I := 1 to MaxRefStore do begin
        InitializeMomentVar(SlopeMomentVar[i]);
        InitializeMomentVar(ElevMomentVar[i]);
        NumPits[i] := 0;
        NumPeaks[i] := 0;
      end;

      for I := 1 to MaxTestDEM do begin
         TestDEM[i] := 0;
         TestSeries[i] := '';
      end;

      if LoadReferenceDEMs then begin
         DEMIXtileDB := DEMIXtileFill(RefDEM,DEMGlb[RefDEM].DEMBoundBoxGeo);
         GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>=' + IntToStr(MDDef.DEMIX_Full));

         if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
            MessageToContinue('No filled DEMIX tiles on map');
            {$IfDef RecordDEMIX} writeLineToDebugFile('No filled DEMIX tiles on map'); {$EndIf}
         end
         else begin
            if (IceSatFName <> '') then IceSatTable := tMyData.Create(IceSatFName);
            if (GeodeticFName <> '') then GeodeticTable := tMyData.Create(GeodeticFName);

            if (LandCoverFName <> '') then LandCoverGrid := GDALsubsetimageandopen(DEMGlb[RefDEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,true,LandCoverFName);

            new(zs);
            New(zssteep);
            New(zsflat);
            if ValidDEM(LandCoverGrid) then begin
               New(zsForest);
               New(zsUrban);
            end;
            {$IfDef RecordFullDEMIX} writeLineToDebugFile('ProcessDEMIXtestarea in, zs created'); {$EndIf}

            //do critera for reference DEMs
            ReferenceDEMcriteria(RefDTMpoint);
            ReferenceDEMcriteria(RefDTMarea);
            ReferenceDEMcriteria(RefDSMpoint);
            ReferenceDEMcriteria(RefDSMarea);
            ReferenceDEMcriteria(COPRefDTM);

            {$IfDef RecordFullDEMIX} OpenDEMsToDebugFile('DEMs start loading');  {$EndIf}

            OpenIndexedSeriesTable(IndexSeriesTable);
            IndexSeriesTable.ApplyFilter('USE=' + QuotedStr('Y'));
            Ser := 0;
            while not IndexSeriesTable.eof do begin
               WantSeries := IndexSeriesTable.GetFieldByNameAsString('SERIES');
               {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Try ' + WantSeries + ' ' + IntToStr(Ser) + '/' + IntToStr(IndexSeriesTable.FiltRecsInDB)); {$EndIf}
               wmdem.SetPanelText(0,WantSeries);
               if LoadMapLibraryBox(WantDEM,WantImage,true,DEMGlb[RefDEM].DEMBoundBoxGeo,WantSeries,false) and ValidDEM(WantDEM) then begin
                  {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Opened:' + WantSeries + '  DEM=' + IntToStr(WantDEM)); {$EndIf}
                  inc(Ser);
                  CheckVerticalDatumShift(WantDEM);
                  TestDEM[Ser] := WantDEM;
                  TestSeries[Ser] := WantSeries;
                  {$IfDef RecordDEMIXLoad} OpenDEMsToDebugFile('Opened ' + WantSeries); for i := 1 to 2 do WriteLineToDebugFile('+++++++++++++');  {$EndIf}
               end
               else begin
                  {$IfDef RecordDEMIXLoad} OpenDEMsToDebugFile('Did not open ' + WantSeries); {$EndIf}
               end;
               IndexSeriesTable.Next;
            end;
            {$IfDef RecordFullDEMIX} OpenDEMsToDebugFile('DEMs were loaded');  {$EndIf}


            if ElevDiffHists then begin
               ElevFiles := tStringList.Create;
               LegendFiles := tStringList.Create;
            end;

            for i := 1 to Ser do begin
               {$IfDef RecordDEMIX} for j := 1 to 2 do writeLineToDebugFile(''); writeLineToDebugFile('Start tests for DEM=' + IntToStr(TestDEM[i]) + '/' + IntToStr(Ser) + '  ' + TestSeries[i]); {$EndIf}
               if Copy(DEMGlb[TestDEM[i]].AreaName,1,4) = 'ALOS' then begin
                  CompareToReference(TestDEM[i],RefDTMArea,'DTM');
                  CompareToReference(TestDEM[i],RefDSMArea,'DSM');
               end
               else begin
                  if (Copy(DEMGlb[TestDEM[i]].AreaName,1,3) = 'COP') and (COPRefDTM <> 0) then begin
                     CompareToReference(TestDEM[i],COPRefDTM,'DTM');
                  end
                  else begin
                     CompareToReference(TestDEM[i],RefDTMpoint,'DTM');
                  end;
                  CompareToReference(TestDEM[i],RefDSMpoint,'DSM');
               end;
               DEMIXTile := '';
               DoICESat(TestDEM[i]);
               DoGeodetic(TestDEM[i]);
               {$IfDef RecordDEMIX} writeLineToDebugFile('All tests done for ' + TestSeries[i]); {$EndIf}
            end;

            {$IfDef RecordDEMIX} for i := 1 to 2 do writeLineToDebugFile(''); {$EndIf}
            Dispose(zs);
            Dispose(zssteep);
            Dispose(zsflat);
            if ValidDEM(LandCoverGrid) then begin
               Dispose(zsForest);
               Dispose(zsUrban);
            end;

            IndexSeriesTable.Destroy;
            {$IfDef RecordDEMIX} writeLineToDebugFile('call CloseAndNilNumberedDB(DEMIXtileDB)'); {$EndIf}
            CloseAndNilNumberedDB(DEMIXtileDB);
         end;
         ReportErrors := true;
         CloseAllDEMs;

         if ElevDiffHists then begin
            ElevFiles.Destroy;
            LegendFiles.Destroy;
         end;
      end;
    end;


    procedure ZeroNames;
    begin
      GeodeticFName := '';
      IceSatFName := '';
      LocalDatumAddFName := '';
      LocalDatumSubFName := '';
      RefDSMPointFName := '';
      RefDSMareaFName := '';
      RefDTMPointFName := '';
      RefDTMareaFName := '';
      COPRefDTMFName := '';
      LandCoverFName := '';
    end;

var
   cfName,DataPath : pathStr;
   i : integer;
   DefaultFilter : byte;
   TStr : shortstring;
begin
   {$IfDef RecordDEMIX} writeLineToDebugFile('Start ComputeDEMIXstats'); {$EndIf}

   if (FilesWanted = Nil) then begin
      DefaultFilter := 1;
      FilesWanted := tStringList.Create;
      FilesWanted.Add(ExtractFilePath(LastDataBase));
      if not GetMultipleFiles('DEMIX control file','*.dbf',FilesWanted,DefaultFilter) then exit;
   end;

   //settings that can be changed
   HeavyDutyProcessing := true;
   ElevDiffHists := false;
   DoHorizontalShift := false;
   MDDef.DEMIX_Full := 75;
   MDDef.SlopeSteepBoundary := 18;
   SlopeMask := 'ALL';

   TStr := DateToStr(now) + '-' + CurrentMilitaryTime(true);
   for i := 1 to length(TStr) do if TStr[i] in ['/',':'] then TStr[i] := '-';

   OutputDir := ExtractFilePath(FilesWanted.Strings[0]) + 'wine_results\' + TStr + '\';
   SafeMakeDir(OutputDir);

   if DoHorizontalShift then SafeMakeDir(OutputDir + 'lags\' );
   if ElevDiffHists then begin
      SafeMakeDir(OutputDir + 'elev_diff_hists\' );
      SafeMakeDir(OutputDir + 'slope_diff_hists\' );
      SafeMakeDir(OutputDir + 'ruff_diff_hists\' );
   end;

   {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIX output to ' + OutputDir); {$EndIf}

   InitializeStringLists;

   for i := 0 to pred(FilesWanted.Count) do begin
      cFName := FilesWanted.Strings[i];
      {$IfDef RecordDEMIX} writeLineToDebugFile('File ' + IntToStr(i) + '  Read rules ' + cfName); {$EndIf}
      Rules := tMyData.Create(cfName);
      ZeroNames;
      if Rules.FieldExists('DATA_PATH') then begin
         {$IfDef RecordDEMIX} writeLineToDebugFile('DATA_PATH option'); {$EndIf}
         while not Rules.eof do begin
            DataPath := Rules.GetFieldByNameAsString('DATA_PATH');
            RefDTMPointFName := DataPath + Rules.GetFieldByNameAsString('DTM_POINT');
            RefDTMareaFName :=  DataPath + Rules.GetFieldByNameAsString('DTM_AREA');
            RefDSMPointFName := DataPath + Rules.GetFieldByNameAsString('DSM_POINT');
            RefDSMareaFName := DataPath + Rules.GetFieldByNameAsString('DSM_AREA');
            ProcessDEMIXtestarea;
            Rules.Next;
         end;
      end
      else begin
         while not Rules.eof do begin
            fName := Rules.GetFieldByNameAsString('FILENAME');
            if (fName <> '') then begin
               if Not FileExists(fName) then fName[1] := cfName[1];   //fix for external hard drive which moves around
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'GEODETIC' then GeodeticFName := fName;
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'ICESAT2' then IceSatFName := fName;
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_POINT' then RefDTMPointFName := fName;
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_AREA' then RefDTMareaFName := fName;
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_POINT' then RefDSMPointFName := fName;
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_AREA' then RefDSMareaFName := fName;
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'COP_REF_DTM' then COPRefDTMFName := fName;
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_ADD' then LocalDatumAddFName := fName;
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_SUB' then LocalDatumSubFName := fName;
               if UpperCase(Rules.GetFieldByNameAsString('DATA')) = 'LANDCOVER' then LandCoverFName := fName;
            end;
            Rules.Next;
         end;
         {$IfDef RecordDEMIX} writeLineToDebugFile('ProcessDEMIXtestarea in, rules read ' + ExtractFilePath(fName)); {$EndIf}
          ProcessDEMIXtestarea;
      end;
      FinalizeStringLists(false);
   end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('All processing done'); {$EndIf}
   Rules.Destroy;
   FinalizeStringLists;

   FilesWanted.Destroy;
   HeavyDutyProcessing := false;
   {$IfDef RecordDEMIX} writeLineToDebugFile('End ComputeDEMIXstats'); {$EndIf}
end;


//some of these have the file names hard coded
//this was designed for a particular purpose, and might not be generally applicable

procedure DEMIXtilesStats;
var
   DEMIXtiles,i,Valid : integer;
   bb : sfBoundBox;
begin
   if ValidDEM(1) then begin
      {$IfDef RecordBatch} WriteLineToDebugFile('Twmdem.DEMIXtiles1Click in'); {$EndIf}
      DemixTiles := DEMGlb[1].SelectionMap.LoadDEMIxTileOutlines(DEMGlb[1].SelectionMap.MapDraw.MapCorners.BoundBoxGeo);
      Pick_Geostats.DoGeoStatAnalysis;

      for i := 2 to MaxDEMDataSets do if ValidDEM(i) then begin
         if ExtractFilePath(UpperCase(DEMGlb[i].DEMFileName)) = UpperCase(MDTempDir) then begin
            DEMGlb[i].CutOutGeoBox(DEMGlb[1].DEMBoundBoxGeo);
            DEMGlb[i].SelectionMap.MaskFromSecondGrid(1, msSecondMissing);
            DEMGlb[i].CheckMaxMinElev;
            DEMGlb[i].SavePartOfDEMWithData(DEMGlb[i].DEMFileName);
            DEMGlb[i].DEMAlreadyDefined := false;
            DEMGlb[i].SelectionMap.ReloadDEMClick(Nil);
            {$IfDef RecordBatch} WriteLineToDebugFile(DEMGlb[i].KeyDEMParams(true)); {$EndIf}
         end;
      end;

      GISdb[DemixTiles].LayerIsOn := false;
      GISdb[DemixTiles].MyData.First;
      while not GISdb[DemixTiles].MyData.eof do begin
         bb := GISdb[DemixTiles].MyData.GetRecordBoundingBox;
         PickGeoStat.GroupTitle := GISdb[DemixTiles].MyData.GetFieldByNameAsString('NAME');

         {$IfDef RecordBatch} Valid := DEMGlb[1].ValidElevsInDEM; WriteLineToDebugFile(IntToStr(1) + '  full DEM valid=' + IntToStr(Valid)); {$EndIf}

         DEMGlb[1].CutOutGeoBox(bb);
         DEMGlb[1].SelectionMap.RespondToChangedDEM;
         Valid := DEMGlb[1].ValidElevsInDEM;
         {$IfDef RecordBatch} WriteLineToDebugFile('Tile=' + PickGeoStat.GroupTitle + ' valid=' + IntToStr(Valid)); {$EndIf}
         if (Valid > 5000) then begin
            for i := 2 to MaxDEMDataSets do if ValidDEM(i) then begin
               DEMGlb[i].SelectionMap.OutlineGeoBox(bb,clRed,3);
               DEMGlb[i].CutOutGeoBox(bb);
               DEMGlb[i].SelectionMap.RespondToChangedDEM;
               DEMGlb[i].SelectionMap.BringToFront;
            end;
            PickGeoStat.DifferenClick(Nil);

            for i := 2 to MaxDEMDataSets do if ValidDEM(i) then begin
               DEMGlb[i].SelectionMap.ReloadDEMClick(Nil);
            end;
         end;
         DEMGlb[1].SelectionMap.ReloadDEMClick(Nil);
         GISdb[DemixTiles].MyData.Next;
      end;
      {$IfDef RecordBatch} WriteLineToDebugFile('Twmdem.DEMIXtiles1Click out'); {$EndIf}
   end;
end;

procedure BatchGlobalDEMs;


     procedure OneDir(Lidar,Output : PathStr);
     begin
         {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.LidarandglobalDEMs1Click in, ' + Lidar); {$EndIf}
          OvelayPointClouds(Nil,Lidar);
         {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.LidarandglobalDEMs1Click opened'); {$EndIf}
          pt_cloud_opts_fm.CreateGridOrDBfromStats(dbpcDataBase,50,Output);
         {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.LidarandglobalDEMs1Click db created, ' + Output); {$EndIf}
         pt_cloud_opts_fm_Close;
         {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.LidarandglobalDEMs1Click out'); {$EndIf}
     end;


begin
   StopSplashing;
   //OneDir('H:\00-tgis_paper\lidar\Icod\','H:\00-tgis_paper\Icod_lidar.csv');
   //OneDir('H:\00-tgis_paper\lidar\Oahu\','H:\00-tgis_paper\Oahu_lidar.csv');
   //OneDir('H:\00-tgis_paper\lidar\Bled\','H:\00-tgis_paper\Bled_lidar.csv');
   //OneDir('H:\00-tgis_paper\lidar\Canyon_Mtns\','H:\00-tgis_paper\Canyon_Mtns_lidar.csv');
   //OneDir('H:\00-tgis_paper\lidar\Blue_Ridge\','H:\00-tgis_paper\Blue_Ridge_lidar.csv');
   OneDir('H:\00-tgis_paper\lidar\El_Hiero\','H:\00-tgis_paper\El_Hiero_lidar.csv');
   OneDir('H:\00-tgis_paper\lidar\Redwoods\','H:\00-tgis_paper\RedWoods_lidar.csv');
end;



procedure CreateDEMsfromLidar;
var
   CloudFNames,DEMsToCreate : PathStr;
   Clouds : tStringList;
   I : Integer;
begin
   {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.CreateDEMsfromlidar1Click in'); {$EndIf}
   StopSplashing;
   CloudFNames := 'c:\microdem\clouds.txt';
   DEMsToCreate := 'c:\microdem\match_tandemx_dems.dbf';
   Clouds := tStringList.Create;
   Clouds.LoadFromFile(CloudFNames);
   for I := 0 to pred(Clouds.Count) do begin
      {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Clouds.Strings[i]'); {$EndIf}
       if PathIsValid(Clouds.Strings[i]) then begin
          OvelayPointClouds(Nil,Clouds.Strings[i]);
          if (pt_cloud_opts_fm.LasFiles[1].CloudName <> '') then begin
             pt_cloud_opts_fm.AutoSaveDir := 'c:\temp\';
             pt_cloud_opts_fm.DEMrulesFName := DEMstoCreate;
             pt_cloud_opts_fm.BitBtn50Click(Nil);
          end
          else begin
             {$IfDef RecordProblems} HighlightLineToDebugFile('No LAS files in ' + Clouds.Strings[i]); {$EndIf}
          end;
          pt_cloud_opts_fm_Close;
          CloseAllWindowsAndData;
       end
       else begin
          {$IfDef RecordProblems} HighlightLineToDebugFile('Invalid dirctory  ' + Clouds.Strings[i]); {$EndIf}
       end;
   end;
   {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.CreateDEMsfromlidar1Click out'); {$EndIf}
end;




