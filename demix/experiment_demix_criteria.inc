//as of 9/15/2023, these are not called


const
   IcePointsStatsString = 'DICE_MIN,DICE_Max,DICE_Mean,DICE_AVD,DICE_STD,DICE_MEDI,DICE_RMSE,DICE_MAE,DICE_N,DICE_AMED';
   BMPointsStatsString = 'DBM_MIN,DBM_Max,DBM_Mean,DBM_AVD,DBM_STD,DBM_MEDI,DBM_RMSE,DBM_MAE,DBM_N,DBM_AMED';
   LagStatsString = 'Lag_Max,Lag_Mean,Lag_STD,LAG_MED,LAG_LE90,LAG_NPTS';



(*
function LoadDEMIXareaDefinitions(cfName : pathStr = '') : boolean;
var
   fName : PathStr;
   db : integer;
begin
   GeodeticFName := '';
   IceSatFName := '';
   LandCoverFName := '';
   LocalDatumAddFName := '';
   LocalDatumSubFName := '';
   RefDSMPointFName := '';
   RefDSMareaFName := '';
   RefDTMPointFName := '';
   RefDTMareaFName := '';
   COPRefDTMFName := '';
   COPRefDSMFName := '';

   DEMIXRefDEM := 0;
   //AddLocalVDatum := 0;
   //SubLocalVDatum := 0;
   RefDTMpoint := 0;
   RefDTMarea := 0;
   RefDSMpoint := 0;
   RefDSMarea := 0;
   //GeoidGrid := 0;
   COPRefDTM := 0;
   COPRefDSM := 0;

   {$IfDef RecordDEMIX} writeLineToDebugFile('File Read rules ' + cfName); {$EndIf}
   //Rules := tMyData.Create(cfName);
   db := OpenMultipleDataBases('DEMIX area rules',cfName, false);
   Result := GISdb[db].MyData.FieldExists('DATA') and GISdb[db].MyData.FieldExists('FILENAME');
   if Result then begin
      while (not GISdb[db].MyData.eof) do begin
         fName := GISdb[db].MyData.GetFieldByNameAsString('FILENAME');
         if (fName <> '') then begin
            if Not FileExists(fName) then fName[1] := cfName[1];   //fix for external hard drive which moves around
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'GEODETIC' then GeodeticFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'ICESAT2' then IceSatFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_POINT' then RefDTMPointFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_AREA' then RefDTMareaFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_POINT' then RefDSMPointFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_AREA' then RefDSMareaFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'COP_REF_DTM' then COPRefDTMFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'COP_REF_DSM' then COPRefDSMFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_ADD' then LocalDatumAddFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_SUB' then LocalDatumSubFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'LANDCOVER' then LandCoverFName := fName;
         end;
         GISdb[db].MyData.Next;
      end;
      {$IfDef RecordDEMIXFull} writeLineToDebugFile('ProcessDEMIXtestarea in, rules read ' + ExtractFilePath(fName)); {$EndIf}
   end
   else begin
      MessageToContinue('Invalid file: ' + cfName);
   end;
   CloseAndNilNumberedDB(db);
end;
*)

(*
var
   UseDSM,UseDTM,chm,
   RefSlopeMap,RefRuffMap,RefAspectMap,
   COP_ALOS_Diff,CopSlope,CopRuff,ALOSslope,ALOSruff,
   BestCOP_ALOS_elev, BestCOP_ALOS_slope,BestCOP_ALOS_ruff,
   COP_ALOS_DSM4,COP_ALOS_DTM4,COP_ALOS_DSM9,COP_ALOS_DTM9,
   DTMElevDiffMapALOS,DTMElevDiffMapCOP,DTMSlopeDiffMapALOS, DTMSlopeDiffMapCOP,DTMRuffDiffMapALOS,DTMRuffDiffMapCOP : integer;
   HalfSec,AirOrDirt,AirOrDirt2,AirOrDirt3 : array[1..10] of integer;


procedure ZeroDEMs;
var
   i : integer;
begin
   UseDSM := 0;
   UseDTM := 0;
   HalfSecRefDSM := 0;
   HalfSecRefDTM := 0;
   HalfSecALOS := 0;
   HalfSecCOP := 0;
   RefSlopeMap := 0;
   RefRuffMap := 0;
   COP_ALOS_DSM4 := 0;
   COP_ALOS_DTM4 := 0;
   COP_ALOS_DSM9 := 0;
   COP_ALOS_DTM9 := 0;
   COP_ALOS_Diff := 0;

   CopSlope := 0;
   CopRuff := 0;
   ALOSslope := 0;
   ALOSruff := 0;

   BestCOP_ALOS_elev := 0;
   BestCOP_ALOS_slope := 0;
   BestCOP_ALOS_ruff := 0;
   DTMElevDiffMapALOS := 0;
   DTMElevDiffMapCOP := 0;
   DTMSlopeDiffMapALOS := 0;
   DTMSlopeDiffMapCOP := 0;
   DTMRuffDiffMapALOS := 0;
   DTMRuffDiffMapCOP := 0;

   CHM := 0;
   for I := 1 to 10 do begin
      HalfSec[i] := 0;
      AirOrDirt[i] := 0;
      AirOrDirt2[i] := 0;
      AirOrDirt3[i] := 0;
   end;
end;
*)






{$IfDef AllowEDTM}

      procedure ExtractEDTMforTestAreas;
      //because these are extracted from a single monster Geotiff, they are handled differently than others in the map library
      var
         TheFiles : tStringList;
         AreaName : Shortstring;
         fName,EDTM_fName : PathStr;
         i,DEM,NewDEM : Integer;
      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ExtractEDTMforTestAreas in'); {$EndIf}
         try
            GetDEMIXpaths(true);
            EDTM_fName := 'j:\dtm.bareearth_ensemble_p10_30m_s_2018_go_epsg4326_v20230221.tif';
            TheFiles := Nil;
            FindMatchingFiles(DEMIX_test_dems,'*.DEM',TheFiles);
            RemoveFilesThatDoHaveString(TheFiles,'DSM');  //if there is a DSM, there is also a DTM
            for i := 0 to pred(TheFiles.Count) do begin
               wmdem.SetPanelText(1,'Area ' + IntToStr(i) + '/' + IntToStr(TheFiles.Count));
               fName := TheFiles.Strings[i];
               if StrUtils.AnsiContainsText(UpperCase(fname),'COP') then begin
                  AreaName := Petmar_Types.BeforeSpecifiedString(ExtractFileName(fName),'_COP');
                  fName := DEMIX_test_dems + AreaName + '_EDTM.dem';
                  if not FileExists(fName) then begin
                     wmdem.SetPanelText(2,AreaName);
                     DEM := OpenNewDEM(fName,false);
                     NewDEM := GDALsubsetGridAndOpen(DEMGlb[DEM].DEMBoundBoxGeo,true,EDTM_fName);
                     DEMGlb[NewDEM].MultiplyGridByConstant(0.1);
                     DEMGlb[NewDEM].WriteNewFormatDEM(fName);
                     CloseAllDEMs;
                  end;
               end;
            end;
         finally
            EndDEMIXProcessing;
         end;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ExtractEDTMforTestAreas out');
      end;

{$EndIf}




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



 var
   ICESatStats,
   LagStats,
   SlopeMoments,
   PitsPeaks,
   CorrelationCoefficientsStats,
   ElevMomentDiffStats,SlopeMomentDiffStats,
   GeodeticStats,
   ElevMoments : tstringlist;
 


		procedure CreateExperimentalStringLists;
         begin
            LagStats := tStringList.Create;
            LagStats.Add(TileStatsString + AreaString + LagStatsString);

            ElevMomentDiffStats := tStringList.Create;
            ElevMomentDiffStats.Add(TileStatsString + AreaString + ElevMomentsDiffString);

            SlopeMomentDiffStats := tStringList.Create;
            SlopeMomentDiffStats.Add(TileStatsString + AreaString + SlopeMomentsDiffString);

            SlopeMoments := tStringList.Create;
            SlopeMoments.Add(MomentStr + ',' + SlopeMomentsString);

            ElevMoments := tStringList.Create;
            ElevMoments.Add(MomentStr + ',' + ElevMomentsString);

            PitsPeaks := tStringList.Create;
            PitsPeaks.Add(TileStatsString + AreaString + 'NUM_PITS,NUM_PEAKS,DIFF_PITS,DIFF_PEAKS');

            ICESatStats := tStringList.Create;
            ICESATStats.Add(TileStatsString + AreaString + IcePointsStatsString);

            GeodeticStats := tStringList.Create;
            GeodeticStats.Add(TileStatsString + AreaString + BMPointsStatsString);

            CorrelationCoefficientsStats := tStringList.Create;
            CorrelationCoefficientsStats.Add(TileStatsString + AreaString + 'R2,INTERCEPT,SLOPE');
         end;
		 
		 
         procedure ProcessExperimentalStringLists;
         begin

            ProcessStringList('ICESat_best_diff_stats',ICESATStats);
            ProcessStringList('Geodetic_control_stats',GeodeticStats);

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




(*
procedure MakeCriteriaGraph(DB : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing : tStringList);
//3 Aug 2023, not working and not clear it is worth revising
var
   aFilter,DEMstr : shortstring;
   i,j,k,m,DEM : integer;
   Graph : tThisBaseGraph;
   rfile : array[0..10] of file;
   Symbol : tFullSymbolDeclaration;
   v : array[1..2] of float32;
   AllGraphs : tStringList;
   fName : PathStr;
begin
   {$IfDef RecordFullDEMIX} WriteLineToDebugFile('Start TDemixFilterForm.DoCriteriaGraph'); {$EndIf}
   GISdb[DB].EmpSource.Enabled := false;
   AllGraphs := tStringList.Create;
   for i := 0 to pred(DEMsTypeUsing.Count) do begin
      for j := 0 to pred(TilesUsing.Count) do begin
         for k :=  0 to pred(LandTypesUsing.Count) do begin
            aFilter := 'REF_TYPE=' + QuotedStr(DEMsTypeUsing[i]) + ' AND DEMIX_TILE=' + QuotedStr(TilesUsing[j])+ ' AND LAND_TYPE=' + QuotedStr(LandTypesUsing[k]);
            GISdb[db].ApplyGISFilter(aFilter);
            if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
               {$IfDef RecordFullDEMIX} WriteLineToDebugFile(aFilter); {$EndIf}
               Graph := tThisBaseGraph.Create(Application);

               Graph.GraphDraw.LegendList := tStringList.Create;
               for DEM := 0 to pred(CandidateDEMsUsing.Count) do begin
                  Symbol := SymbolFromDEMName(CandidateDEMsUsing[DEM]);
                  Symbol.DrawingSymbol := FilledBox;
                  Graph.OpenPointFile(rfile[DEM],Symbol);
                  Graph.GraphDraw.LegendList.Add(CandidateDEMsUsing[DEM]);
                  Graph.GraphDraw.LineSize256[DEM] := 2;
               end;

               Graph.Caption := aFilter;
               Graph.GraphDraw.GraphAxes := YFullGridOnly;
               Graph.GraphDraw.MinVertAxis := 999;
               Graph.GraphDraw.MaxVertAxis := -999;
               Graph.GraphDraw.HorizLabel := aFilter;
               Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
               Graph.GraphDraw.GraphBottomLabels := tStringList.Create;
               Graph.GraphDraw.GraphBottomLabels := tStringList.Create;

               GISdb[db].MyData.first;
               while not GISdb[db].MyData.eof do begin
                  DEMstr := GISdb[db].MyData.GetFieldByNameAsString('DEM');
                  DEM := CandidateDEMsUsing.IndexOf(DEMstr);
                  if (DEM <> -1) then begin
                     for m := 0 to pred(CriteriaUsing.Count) do begin
                        Graph.GraphDraw.GraphBottomLabels.Add(IntToStr(m) + ',' + CriteriaUsing[m]);
                        v[1] := m;
                        v[2] := GISdb[db].MyData.GetFieldByNameAsFloat(CriteriaUsing[m]);
                        CompareValueToExtremes(v[2],Graph.GraphDraw.MinVertAxis,Graph.GraphDraw.MaxVertAxis);
                        BlockWrite(rfile[DEM],v,1);
                     end;
                  end;
                  GISdb[db].MyData.Next;
               end;
               Graph.GraphDraw.MinHorizAxis := -0.5;
               Graph.GraphDraw.MaxHorizAxis := CriteriaUsing.Count - 0.5;
               Graph.GraphDraw.MinVertAxis := Graph.GraphDraw.MinVertAxis - 1;
               Graph.GraphDraw.MaxVertAxis := Graph.GraphDraw.MaxVertAxis + 1;
               Graph.GraphDraw.SetShowAllLines(true,2);
               Graph.GraphDraw.VertGraphBottomLabels := false;
               Graph.GraphDraw.ShowVertAxis0 := true;
               Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
               Graph.Height := MDDef.DEMIX_ysize;
               Graph.Width := MDDef.DEMIX_xsize;
               Graph.RedrawDiagram11Click(Nil);

               Graph.Image1.Canvas.Draw(Graph.GraphDraw.LeftMargin+15,Graph.GraphDraw.TopMargin+10,Graph.MakeLegend(Graph.GraphDraw.LegendList,false));
               fName := NextFileNumber(MDTempDir,'big_graph_','.png');
               SaveImageAsBMP(Graph.Image1,fName);
               AllGraphs.Add(fName);
            end;
         end;
      end;
   end;
   fName := NextFileNumber(MDtempDir,'criteria_by_tile_','.png');
   MakeBigBitmap(AllGraphs,'',fName,4);
   DisplayBitmap(fName,'');

   GISdb[DB].ClearGISFilter;
   GISdb[DB].EmpSource.Enabled := true;
   {$IfDef RecordFullDEMIX} WriteLineToDebugFile('End TDemixFilterForm.DoCriteriaGraph'); {$EndIf}
end;
*)





