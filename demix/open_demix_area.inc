{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{-----------------------------------}
{  include file for demix_control   }
{___________________________________}


removed 9/13/2023, as it had not been used in some time


procedure OpenDEMIXArea(fName : PathStr = '');
//these are currently hard wired in the code, but eventually might be options enabled at run time
const
   DEMIX_Movie = false;
   OverwriteFiles = false;
   DEMIXhistograms = false;
   DEMIX_SaveDEMs = true;
   DEMIX_HalfSecondCompareMaps = true;
   DEMIX_GeomorphMapsBestDEM = true;
   DEMIX_MakeDifferenceMaps = true;
   DEMIX_GeomorphMaps = true;
var
   AreaName : shortstring;
   //HalfSec,
   AirOrDirt,AirOrDirt2,AirOrDirt3 : array[1..10] of integer;
   i,Cols : integer;
   BigMap,Movie : tStringList;
   Graph1,Graph2,Graph3,Graph4 : tThisBaseGraph;
   SaveDir : PathStr;
   Spacing : float32;

                  procedure TryHalfSecCompares;
                  var
                     i,HalfSec : integer;
                  begin
                     if MDDef.DEMIX_DoHalfSecDEMs then begin
                        {$IfDef RecordDEMIX} writeLineToDebugFile('Start creation Half sec dems; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
                        for I := 1 to MaxDEMIXDEM do begin
                           if ValidDEM(TestDEM[i]) then begin
                              {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' Initial DEM=' + IntToStr(TestDEM[i]) + '  ' + DEMGlb[TestDEM[i]].KeyDEMParams(true)); {$EndIf}
                              ReinterpolateTestDEMtoHalfSec(TestDEM[i],true);
                              {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' now TestDEM=' + IntToStr(TestDEM[i])); {$EndIf}
                              {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' Half sec DEM=' + IntToStr(TestDEM[i]) + '  ' + DEMGlb[TestDEM[i]].KeyDEMParams(true)); {$EndIf}
                              if MDDef.DEMIX_DoAirOrDirt then begin
                                 AirOrDirt[i] := AirBallDirtBallMap(TestDEM[i],HalfSecRefDSM ,HalfSecRefDTM, DEMGlb[TestDEM[i]].AreaName + '_canopy' ); //checks if point within canopy
                                 AirOrDirt2[i] := AirBallDirtBallMap(TestDEM[i],HalfSecRefDSM ,0,DEMGlb[TestDEM[i]].AreaName + 'air_dirt_dsm');     //compares to DSM
                                 AirOrDirt3[i] := AirBallDirtBallMap(TestDEM[i],0,HalfSecRefDTM,DEMGlb[TestDEM[i]].AreaName + 'air_dirt_dtm');     //compares to DTM
                                 {$IfDef RecordDEMIX} writeLineToDebugFile('air or dirt done, canopy=' + IntToStr(AirOrDirt[i]) + '  dsm=' + IntToStr(AirOrDirt2[i])   + '  dtm=' + IntToStr(AirOrDirt3[i]) ); {$EndIf}
                              end;
                            end;
                        end;
                        {$IfDef RecordDEMIX} writeLineToDebugFile('Done creation Half sec dems; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

                        if DEMIX_GeomorphMapsBestDEM then begin
                           {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIX_GeomorphMapsBestDEM begin'); {$EndIf}
                           ALOSslope := -1;   //forces creation of slope and roughness maps
                           ALOSruff := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEM[1],3,ALOSslope);
                           DEMGlb[ALOSSlope].AreaName := 'alos_slope_%';
                           DEMGlb[ALOSRuff].AreaName := 'alos_roughness_%';

                           COPslope := -1;   //forces creation of slope and roughness maps
                           COPruff := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEM[2],3,copSlope);
                           DEMGlb[COPSlope].AreaName := 'cop_slope_%';
                           DEMGlb[COPRuff].AreaName := 'cop_roughness_%';

                           BestCOP_ALOS_elev := BestCopOrALOSmap(HalfSecRefDTM,TestDEM[1],TestDEM[2],1.5,'best_dem_elevation');
                           BestCOP_ALOS_slope := BestCopOrALOSmap(RefSlopeMap,ALOSslope,COPslope,1.5,'best_dem_slope');
                           BestCOP_ALOS_ruff := BestCopOrALOSmap(RefRuffMap,ALOSruff,COPruff,1.5,'best_dem_roughness');

                           if DEMIX_MakeDifferenceMaps then begin
                              DTMElevDiffMapALOS := MakeDifferenceMap(HalfSecRefDTM,TestDEM[1],HalfSecRefDTM,HalfSecRefDTM,true,false,false);
                              DTMElevDiffMapCOP := MakeDifferenceMap(HalfSecRefDTM,TestDEM[2],HalfSecRefDTM,HalfSecRefDTM,true,false,false);
                              DTMSlopeDiffMapALOS := MakeDifferenceMap(RefSlopeMap,ALOSslope,RefSlopeMap,HalfSecRefDTM,true,false,false);
                              DTMSlopeDiffMapCOP := MakeDifferenceMap(RefSlopeMap,COPSlope,RefSlopeMap,HalfSecRefDTM,true,false,false);
                              DTMRuffDiffMapALOS := MakeDifferenceMap(RefRuffMap,ALOSRuff,RefRuffMap,HalfSecRefDTM,true,false,false);
                              DTMRuffDiffMapCOP := MakeDifferenceMap(RefRuffMap,COPRuff,RefRuffMap,HalfSecRefDTM,true,false,false);
                           end;
                           {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIX_GeomorphMapsBestDEM done'); {$EndIf}
                        end;

                        if DEMIX_HalfSecondCompareMaps then begin
                           {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec candidates done, Make difference map'); {$EndIf}
                           COP_ALOS_Diff := MakeDifferenceMap(TestDEM[1],TestDEM[2],TestDEM[1],TestDEM[1],true,false,false,'COP-ALOS_difference');
                           (*
                           {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DSM COP-ALOS'); {$EndIf}
                           COP_ALOS_DSM4 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],HalfSecRefDSM, MDDef.DEMIXSimpleTolerance,true,'COP-ALOS_compare_DSM-4');
                           COP_ALOS_DSM9 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],HalfSecRefDSM, MDDef.DEMIXSimpleTolerance,false,'COP-ALOS_compare_DSM-9');

                           {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DTM COP-ALOS'); {$EndIf}
                           COP_ALOS_DTM4 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],HalfSecRefDSM, MDDef.DEMIXSimpleTolerance,true,'COP-ALOS_compare_DTM-4');
                           COP_ALOS_DTM9 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],HalfSecRefDSM, MDDef.DEMIXSimpleTolerance,false,'COP-ALOS_compare_DTM-9');
                          *)
                           if DEMIXhistograms then begin
                              HistogramsFromVATDEM(COP_ALOS_DSM4,HalfSecRefDSM,RefSlopeMap,RefRuffMap,RefAspectMap,Graph1,Graph2,Graph3,Graph4);
                              HistogramsFromVATDEM(COP_ALOS_DTM4,HalfSecRefDTM,RefSlopeMap,RefRuffMap,RefAspectMap,Graph1,Graph2,Graph3,Graph4);
                           end;
                        end;
                     end;

                     {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec dems done; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
                  end;



         procedure AddImage(GraphImage : tImage);
         var
           Bitmap : tMyBitmap;
         begin
            if MDDef.DEMIXCompositeImage then begin
               CopyImageToBitmap(GraphImage,Bitmap);
               Bitmap.Height := Bitmap.Height + 25;
               fName := NextFileNumber(MDtempDir,'graph_4_biggie_','.bmp');
               Bitmap.SaveToFile(fName);
               BigMap.Add(fName);
            end;
         end;


         procedure AddFrame(DEM : integer; What : ShortString);
         begin
            if ValidDEM(DEM) then begin
               {$IfDef RecordDEMIXMovies} writeLineToDebugFile('Add frame, dem=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName + '  ' + DEMGlb[DEM].SelectionMap.MapDraw.FullMapfName); {$EndIf}
               Movie.Add(DEMGlb[DEM].SelectionMap.MapDraw.FullMapfName);
            end
            else begin
               {$IfDef RecordDEMIXMovies} writeLineToDebugFile('Could not find map for DEM=' + IntToStr(DEM) + '  ' + What); {$EndIf}
            end;
         end;

         procedure SaveDEM(DEM : integer; fName : PathStr);
         begin
            if ValidDEM(DEM) then begin
               if OverwriteFiles or (not FileExists(fName)) then begin
                  {$IfDef RecordDEMIXsave} writeLineToDebugFile('Save ' + fName + ' elev units=' + ElevUnitsAre(DEMGlb[DEM].DEMheader.ElevUnits)); {$EndIf}
                  DEMGlb[DEM].WriteNewFormatDEM(fName);
               end;
               fName := ChangeFileExt(fName,'.png');
               SaveImageAsBMP(DEMGlb[DEM].SelectionMap.Image1,fName);
            end;
         end;


begin
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea ' + fName); {$EndIf}
      AreaName := ExtractFileNameNoExt(fName);
      if DEMIX_SaveDEMs then begin
         SaveDir := fName[1] + ':\aa_half_sec_test\' + AreaName + '\';
         //if PathIsValid(SaveDir) then exit;
         SafeMakeDir(SaveDir);
      end;
      try
         HeavyDutyProcessing := true;
         AutoOverwriteDBF := true;
         NakedMapOptions;  //also does SaveBackupDefaults;
         MDDef.ScaleBarLocation.DrawItem := true;
         MDDef.GIFfileLabels := false;
         MDdef.DefaultMapXSize := 1000;
         MDdef.DefaultMapYSize := 1000;
         MDDef.MapNameLocation.DrawItem := true;
         MDDef.MapNameLocation.MapPosition := lpNEmap;
         MDDef.MapNameLocation.DrawItem := true;
         MDDef.GridLegendLocation.DrawItem := true;

         ZeroDEMs;

         if true {LoadDEMIXareaDefinitions(fName)} then begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('LoadDEMIXarea complete'); {$EndIf}
            if LoadDEMIXReferenceDEMs(DEMIXRefDEM) then begin
               {$IfDef RecordDEMIX} writeLineToDebugFile('LoadDEMIXReferenceDEMs complete; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + 'DEMIXRefDEM=' + IntToStr(DEMIXRefDEM)); {$EndIf}

               if MDDef.DEMIX_DoHalfSecDEMs then begin
                  //HalfSecRefDSM := CreateHalfSecRefDEM(RefDSMPoint,RefDSMArea);
                  //HalfSecRefDTM := CreateHalfSecRefDEM(RefDTMPoint,RefDTMArea);
exit;
                  //if FileExists(HalfSecRefDSMfName) then HalfSecRefDSM := OpenNewDEM(HalfSecRefDSMfName) else HalfSecRefDSM := CreateHalfSecRefDEM(RefDSMPoint,RefDSMArea);
                  //if FileExists(HalfSecRefDTMfName) then HalfSecRefDTM := OpenNewDEM(HalfSecRefDTMfName) else HalfSecRefDTM := CreateHalfSecRefDEM(RefDTMPoint,RefDTMArea);
                  DEMIXRefDEM := HalfSecRefDTM;
                  {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec ref dems created; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               end;

               (*
               if DEMIX_GeomorphMaps or DEMIX_GeomorphMapsBestDEM then begin
                  if FileExists(RuffMapfName) then begin
                     RefSlopeMap := OpenNewDEM(SlopeMapfName);
                     RefRuffMap := OpenNewDEM(RuffMapfName);
                  end
                  else begin
                     RefSlopeMap := -1;   //forces creation of slope and roughness maps
                     RefRuffMap := CreateSlopeRoughnessSlopeStandardDeviationMap(HalfSecRefDTM,3,RefSlopeMap);
                  end;
                  DEMGlb[RefSlopeMap].AreaName := 'ref_dtm_slope_%';
                  DEMGlb[RefRuffMap].AreaName := 'ref_dtm_roughness_%';
                  if FileExists(AspectMapfName) then HalfSecRefDSM := OpenNewDEM(AspectMapfName) else RefAspectMap := MakeAspectMap(HalfSecRefDTM);
               end;
               *)

               {$IfDef RecordDEMIX} writeLineToDebugFile('Slope and Ruff dems created; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

               if (MDDef.DEMIX_DoCHM) then begin
                  if ValidDEM(HalfSecRefDSM) and ValidDEM(HalfSecRefDTM) then begin
                     chm := MakeDifferenceMapOfBoxRegion(HalfSecRefDTM,HalfSecRefDSM,HalfSecRefDTM,0,DEMGlb[HalfSecRefDTM].FullDEMGridLimits,true,false,false,AreaName + '_half_sec_chm');
                     //DEMglb[chm].SelectionMap.Elevationpercentiles1Click(Nil);
                     {$IfDef RecordDEMIX} writeLineToDebugFile('CHM created; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
                  end;
               end;

               {$IfDef RecordDEMIX} writeLineToDebugFile('Ref dems done; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

               {$IfDef ShowDEMIXWhatsOpen} writeStringListToDebugFile(GetWhatsOpen); {$EndIf}

               if MDDef.DEMIX_DoHalfSecDEMs or MDDef.DEMIX_DoElevParamGraphs or MDDef.DEMIX_DoAirOrDirt or MDDef.DEMIX_DoElevDiff or MDDef.DEMIX_DoSlopeDiff or MDDef.DEMIX_DoRuffDiff or DEMIX_GeomorphMapsBestDEM then begin
                  if LoadDEMIXCandidateDEMs('',DEMIXRefDEM,true,false) then begin
                     {$IfDef RecordDEMIX} HighlightLineToDebugFile('Candidate DEMs loaded'); {$EndIf}
                     {$IfDef ShowDEMIXWhatsOpen} writeLineToDebugFile('candidates loaded');  writeStringListToDebugFile(GetWhatsOpen); {$EndIf}
                     TryHalfSecCompares;
                     if MDDef.DEMIX_DoAirOrDirt then begin
                        if MDDef.DEMIXCompositeImage then begin
                           BigMap := tStringList.Create;
                           BigMap.Add(DEMGlb[RefDTMPoint].SelectionMap.MapDraw.FullMapfName);
                        end;
                        for I := 1 to MaxDEMIXDEM do begin
                           if ValidDEM(TestDEM[i]) then begin
                              GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                              AirOrDirt[i] := AirBallDirtBallMap(TestDEM[i],UseDSM,UseDTM);
                              AirOrDirt2[i] := AirBallDirtBallMap(TestDEM[i],UseDSM,UseDSM);
                              if MDDef.DEMIXCompositeImage then begin
                                 BigMap.Add(DEMGlb[AirOrDirt[i]].SelectionMap.MapDraw.FullMapfName);
                                 if (AirOrDirt2[i] <> 0) then BigMap.Add(DEMGlb[AirOrDirt2[i]].SelectionMap.MapDraw.FullMapfName);
                              end;
                           end;
                        end;
                        DEMGlb[TestDEM[1]].SelectionMap.Allsamepixelsizeasthismap1Click(Nil);
                        if MDDef.DEMIXCompositeImage then MakeBigBitmap(BigMap,'');
                     end;

                     if MDDef.DEMIX_DoElevParamGraphs then ElevationSlopePlot(DEMListForAllOpenDEM);

                     if MDDef.DEMIX_DoElevDiff or MDDef.DEMIX_DoSlopeDiff or MDDef.DEMIX_DoRuffDiff then begin
                        if MDDef.DEMIXCompositeImage then BigMap := tStringList.Create;    //actually this will be graphs, not maps
                        if MDDef.DEMIX_DoElevDiff then begin
                           DoDEMIX_DifferenceMaps(AreaName,'elvd','Elevation',Graph1,Graph2);
                           AddImage(Graph1.Image1);
                           if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                        end;
                        if MDDef.DEMIX_DoSlopeDiff then begin
                           DoDEMIX_DifferenceMaps(AreaName,'slpd','Slope',Graph1,Graph2);
                           AddImage(Graph1.Image1);
                           if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                        end;
                        if MDDef.DEMIX_DoRuffDiff then begin
                           DoDEMIX_DifferenceMaps(AreaName,'rufd','Roughness',Graph1,Graph2);
                           AddImage(Graph1.Image1);
                           if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                        end;
                        if MDDef.DEMIXCompositeImage then begin
                           if (BigMap.Count > 0) then begin
                              fName := NextFileNumber(DEMIXtempFiles,AreaName + '_difference_histograms_','.png');
                              if (RefDSMArea = 0) then Cols := 1 else Cols := 2;
                              MakeBigBitmap(BigMap,'',fName,Cols);
                              DisplayBitmap(fName);
                           end
                           else BigMap.Free;
                        end;
                     end;
                  end;
               end;
            end;
         end;

         {$IfDef ShowDEMIXWhatsOpen} writeStringListToDebugFile(GetWhatsOpen); {$EndIf}

         if DEMIX_Movie then begin
            Movie := tStringList.Create;
            fName := NextFileNumber(MDtempDir,'map_4_movie_','.mov');
            {$IfDef RecordDEMIXMovies} WriteLineToDebugFile('Making movie, ' + fName); {$EndIf}
            AddFrame(HalfSecRefDTM,'Ref DTM');
            AddFrame(HalfSecRefDSM,'Ref DSM');
            for i := 1 to 2 do begin
               AddFrame(AirOrDirt[i],'AirDirt');
               AddFrame(AirOrDirt2[i],'AirDirt2');
               AddFrame(AirOrDirt3[i],'AirDirt3');
            end;

            AddFrame(RefSlopeMap,'Ref slope');
            AddFrame(RefRuffMap,'Ref ruff');
            AddFrame(COP_ALOS_DSM4,'COP_ALOS_DSM4');
            AddFrame(COP_ALOS_DTM4,'COP_ALOS_DTM4');
            AddFrame(COP_ALOS_DSM9,'COP_ALOS_DSM9');
            AddFrame(COP_ALOS_DTM9,'COP_ALOS_DTM9');

            Movie.SaveToFile(fName);
            CreateNewMovie(fName);
            Movie.Free;
         end;

         (*
         if DEMIX_SaveDEMs then begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea start saving DEMs'); {$EndIf}
            SaveDEM(TestDEM[1],SaveDir + ALOSHalfSecfName);
            SaveDEM(TestDEM[2],SaveDir + CopHalfSecfName);

            SaveDEM(HalfSecRefDSM,SaveDir + HalfSecRefDSMfName);
            SaveDEM(HalfSecRefDTM,SaveDir + HalfSecRefDTMfName);

            if DEMIX_HalfSecondCompareMaps then begin
               SaveDEM(COP_ALOS_Diff,SaveDir + COP_ALOS_DifffName);
               SaveDEM(COP_ALOS_DSM4,SaveDir + COP_ALOS_DSM4fName);
               SaveDEM(COP_ALOS_DTM4,SaveDir + COP_ALOS_DTM4fName);
               SaveDEM(COP_ALOS_DSM9,SaveDir + COP_ALOS_DSM9fName);
               SaveDEM(COP_ALOS_DTM9,SaveDir + COP_ALOS_DTM9fName);
            end;

            if DEMIX_GeomorphMaps then begin
               SaveDEM(RefSlopeMap,SaveDir + SlopeMapfName);
               SaveDEM(RefRuffMap,SaveDir + RuffMapfName);
               SaveDEM(RefAspectMap,SaveDir + AspectMapfName);
            end;

            if DEMIX_GeomorphMapsBestDEM then begin
               SaveDEM(BestCOP_ALOS_slope,SaveDir + BestCOP_ALOS_slopefName);
               SaveDEM(BestCOP_ALOS_ruff,SaveDir + BestCOP_ALOS_rufffName);
               SaveDEM(BestCOP_ALOS_elev,SaveDir + BestCOP_ALOS_elevfName);

               if DEMIX_MakeDifferenceMaps then begin
                  SaveDEM(DTMElevDiffMapALOS,SaveDir + DTMElevDiffMapALOSfName);
                  SaveDEM(DTMElevDiffMapCOP,SaveDir + DTMElevDiffMapCOPfName);
                  SaveDEM(DTMSlopeDiffMapALOS,SaveDir + DTMSlopeDiffMapALOSfName);
                  SaveDEM(DTMSlopeDiffMapCOP,SaveDir +  DTMSlopeDiffMapCOPfName );
                  SaveDEM(DTMRuffDiffMapALOS,SaveDir + DTMRuffDiffMapALOSfName);
                  SaveDEM(DTMRuffDiffMapCOP,SaveDir + DTMRuffDiffMapCOPfName);
               end;
            end;
         end;
         *)
      finally
         EndDEMIXProcessing;
         AutoOverwriteDBF := false;
         RestoreBackupDefaults;
         {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea out'); {$EndIf}
      end;
   end;
end;

