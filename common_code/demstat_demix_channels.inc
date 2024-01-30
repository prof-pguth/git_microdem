{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for demstat        }
{___________________________________}


procedure NormalizeDEMforSSIM(DEM : integer; What : shortstring);
var
   Min,Max : float32;
begin
   if ValidDEM(DEM) then begin
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM in for ' + DEMGlb[DEM].AreaName); {$EndIf};
      What := UpperCase(What);
      if (What = 'ELEV') then begin
         Min := -5;
         Max := 4000;
      end
      else if (What = 'RRI') then begin
         Min := 0;
         Max := 200;
      end
      else if (What = 'HILL') then begin
         Min := 0;
         Max := 255;
      end
      else if (What = 'SLOPE') then begin
         Min := 0;
         Max := 1000;
      end
      else if (What = 'TPI') then begin
         Min := -50;
         Max := 50;
      end
      else if (What = 'RUFF') then begin
         Min := 0;
         Max := 125;
      end;
      WantShowProgress := false;
      DEMGlb[DEM].ClipToVerticalRange(Min,Max);
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM z range clipped'); {$EndIf};
      DEMGlb[DEM].AddConstantToGrid(-Min);
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM min set to 0'); {$EndIf};
      DEMGlb[DEM].MultiplyGridByConstant(1/(Max-Min));
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM range set to 1'); {$EndIf};
      DEMGlb[DEM].CheckMaxMinElev;
      WantShowProgress := true;
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM out'); {$EndIf};
   end;
end;



const
   NumPt = 5;
   NumArea = 1;
   PointNames : array[0..NumPt] of shortstring = ('REF_POINT','ASTER','COP','FABDEM','NASA','SRTM');
   AreaNames : array[0..NumArea] of shortstring = ('REF_AREA','ALOS');
type
   tDEM_int_array = array [0..NumPt] of integer;
var
   PointDEMs : tDEM_int_array;   //0 is the reference, rest the test DEMs
   AreaDEMs : tDEM_int_array;  //0 is the reference, rest the test DEMs



function OpenBothPixelIsDEMs(Area,Prefix : shortstring; RefDir,TestDir : PathStr; OpenMaps : boolean) : boolean;
const
   Ext = '.tif';
var
   fName : PathStr;
   i : integer;

   procedure TryToOpen(fName : PathStr; var DEM : integer);
   begin
      if FileExists(fName) then begin
         DEM := OpenNewDEM(fName,OpenMaps);
      end
      else begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Missing file ' + fName); {$EndIf};
         Result := false;
      end;
   end;

begin
    Result := true;

    fName := RefDir + Prefix + area + '_dtm_ref_1sec_point' + Ext;
    if not FileExists(fName) then fName := RefDir + Prefix + area + '_ref_1sec_point' + Ext;
    TryToOpen(fName,PointDEMs[0]);
    for i := 1 to NumPt do begin
       fName := TestDir + Prefix + Area + '_' + PointNames[i]  + Ext;
       if not FileExists(fName) then fName := ChangeFileExt(fName,'.dem');
       TryToOpen(fName,PointDEMs[i]);
    end;

    fName := RefDir + Prefix + area + '_dtm_ref_1sec_area' + Ext;
    if not FileExists(fName) then fName := RefDir + Prefix + area + '_ref_1sec_area' + Ext;
    TryToOpen(fName,AreaDEMs[0]);
    for i := 1 to NumArea do begin
       fName := TestDir + Prefix + Area + '_' + AreaNames[i]  + Ext;
       if not FileExists(fName) then fName := ChangeFileExt(fName,'.dem');
       TryToOpen(fName,AreaDEMs[i]);
    end;

    //for i := 0 to NumPt do WriteLineToDebugFile('Point=' + IntToStr(i) + '  DEM=' + IntToStr(PointDEMs[i]) + '  ' + DEMglb[PointDEMs[i]].AreaName + '  ' + DEMglb[PointDEMs[i]].ColsRowsString);
    //for i := 0 to NumArea do WriteLineToDebugFile(' Area=' + IntToStr(i) + '  DEM=' + IntToStr(AreaDEMs[i]) + '  ' + DEMglb[AreaDEMs[i]].AreaName + '  ' + DEMglb[AreaDEMs[i]].ColsRowsString);

end;



procedure AreaSSIMComputations;
var
   DEMIXtileDB,NumRef,j : integer;
   Results : tStringList;
   fName,SavefName : PathStr;
   aLine,TileName : shortstring;
   bb : sfBoundBox;
   MissingFiles : boolean;
   AreaGrids,PointGrids,AreaGrids2,PointGrids2 : tDEM_int_array;
   RefGridLimits,TestGridLimits : tGridLimits;


   function ProcessDEMTile(Ref,Test : integer) : float32;
   var
      SSIM,Luminance,Contrast,Structure : float64;
   begin
       if ValidDEM(Ref) and ValidDEM(Test) then begin
          RefGridLimits := DEMglb[Ref].sfBoundBox2tGridLimits(bb);
          TestGridLimits := DEMglb[Test].sfBoundBox2tGridLimits(bb);

          //WriteLineToDebugFile(DEMGlb[Ref].AreaName + '  ' + GridLimitsToString(RefGridLimits) + '  ' + GridLimistsSizeToString(RefGridLimits));
          //WriteLineToDebugFile(DEMGlb[Test].AreaName + '  ' + GridLimitsToString(TestGridLimits) + '  ' + GridLimistsSizeToString(TestGridLimits));

          ComputeSSIM(Ref,Test,RefGridLimits,TestGridLimits,SSIM,Luminance,Contrast,Structure);
          Result := 1-SSIM;
       end
       else Result := -9999;
   end;

   procedure DoAnArea(AreaName : shortstring);
   var
      i : integer;


      procedure DoCriterion(Criterion : ANSIString; PointGrids,AreaGrids : tDEM_int_array);
      var
         i : integer;
         Criterion2 : shortstring;
         r,covar : float64;
         What : shortstring;

             function DoCovarianceForPair(Grid1,Grid2 : integer) : float64;
             var
               Mean1,Mean2,StdDev1,StdDev2 : float64 ;
             begin
                CovariancesFromTwoGrids(RefGridLimits,Grid1,Grid2, r,covar,Mean1,Mean2,StdDev1,StdDev2);
                Result := 1-sqr(r);
                //aLine := aline + ',' + RealToString(1-sqr(r),-12,6);
             end;

      begin
          {$IfDef RecordDEMIX} HighLightLineToDebugFile('Criterion=' + Criterion); {$EndIf}
          wmdem.SetPanelText(3, Criterion);
          GISdb[DEMIXtileDB].MyData.First;
          while not GISdb[DEMIXtileDB].MyData.eof do begin
            //do the correlation coefficients before normalizing the grids
             bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
             TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
             wmdem.SetPanelText(2, TileName + ' R2');
             Criterion2 := StringReplace(Criterion,'SSIM','R2',[rfReplaceAll, rfIgnoreCase]);
             aLine := TileName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + Criterion2;

             RefGridLimits := DEMglb[PointGrids[0]].sfBoundBox2tGridLimits(bb);
             for i := 1 to NumPt do aLine := aline + ',' + RealToString(DoCovarianceForPair(PointGrids[0],PointGrids[i]),-12,6);

             RefGridLimits := DEMglb[AreaGrids[0]].sfBoundBox2tGridLimits(bb);
             for i := 1 to NumArea do aLine := aline + ',' + RealToString(DoCovarianceForPair(AreaGrids[0],AreaGrids[i]),-12,6);
             Results.Add(aline);

             GISdb[DEMIXtileDB].MyData.Next;
          end;

           //normalize grids for SSIM
           What := BeforeSpecifiedCharacterAnsi(Criterion,'_');
           for i := 0 to NumPt do NormalizeDEMforSSIM(PointGrids[i],What);
           for i := 0 to NumArea do NormalizeDEMforSSIM(AreaGrids[i],What);

          GISdb[DEMIXtileDB].MyData.First;
          while not GISdb[DEMIXtileDB].MyData.eof do begin
             //do the SSIM
             bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
             TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
             wmdem.SetPanelText(2, TileName + ' SSIM');

             aLine := TileName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + Criterion;
             for i := 1 to NumPt do aLine := aline + ',' + RealToString(ProcessDEMTile(PointGrids[0],PointGrids[i]),-12,6);
             for i := 1 to NumArea do aLine := aline + ',' + RealToString(ProcessDEMTile(AreaGrids[0],AreaGrids[i]),-12,6);
             Results.Add(aline);

             GISdb[DEMIXtileDB].MyData.Next;
          end;


      end;

      procedure ClearDerivedGrids;
      var
         i : integer;
      begin
          for i := 0 to NumPt do CloseSingleDEM(PointGrids[i]);
          for i := 0 to NumArea do CloseSingleDEM(AreaGrids[i]);
      end;

   begin {DoAnArea}
       {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMComputations area=' + AreaName); {$EndIf}
       SaveFName := SSIMresultsDir + AreaName + '_ssim_results.csv';
       if FileExists(SaveFName) then begin
          {$IfDef RecordDEMIX} HighLightLineToDebugFile('Area=' + AreaName + '  file exists=' + SaveFName); {$EndIf}
       end
       else begin
          {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMComputations area=' + AreaName); {$EndIf}
          if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,false) then begin
             Results := tStringList.Create;
             aline := 'DEMIX_TILE,LAT,LONG,CRITERION';
             for i := 1 to NumPt do aline := aLine + ',' + PointNames[i];
             for i := 1 to NumArea do aline := aLine + ',' + AreaNames[i];
             Results.Add(aLine);

             DEMIXtileDB := DEMIXtileFill(PointDEMs[0],DEMGlb[PointDEMs[0]].DEMBoundBoxGeo,true);
             GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>' + IntToStr(MDDef.DEMIX_Full));


             for i := 0 to NumPt do PointGrids[i] := DEMGlb[PointDEMs[i]].BoxCarDetrendDEM(false,DEMGlb[PointDEMs[i]].FullDEMGridLimits,3);
             for i := 0 to NumArea do AreaGrids[i] := DEMGlb[AreaDEMs[i]].BoxCarDetrendDEM(false,DEMGlb[AreaDEMs[i]].FullDEMGridLimits,3);
             DoCriterion('TPI_SSIM',PointGrids,AreaGrids);
             ClearDerivedGrids;

             for i := 0 to NumPt do PointGrids2[i] := 0;    //so slope grids are returned
             for i := 0 to NumArea do AreaGrids2[i] := 0;  //so slope grids are returned
             for i := 0 to NumPt do PointGrids[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(PointDEMs[i],5,PointGrids2[i],false);
             for i := 0 to NumArea do AreaGrids[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(AreaDEMs[i],5,AreaGrids2[i],false);
             DoCriterion('SLOPE_SSIM',PointGrids2,AreaGrids2);
             DoCriterion('RUFF_SSIM',PointGrids,AreaGrids);
             ClearDerivedGrids;
             for i := 0 to NumPt do CloseSingleDEM(PointGrids2[i]);
             for i := 0 to NumArea do CloseSingleDEM(AreaGrids2[i]);

             for i := 0 to NumPt do PointGrids[i] := MakeSingleNewDerivativeMap('R',PointDEMs[i],0,false);
             for i := 0 to NumArea do AreaGrids[i] := MakeSingleNewDerivativeMap('R',AreaDEMs[i],0,false);
             DoCriterion('HILL_SSIM',PointGrids,AreaGrids);
             ClearDerivedGrids;

             for i := 0 to NumPt do PointGrids[i] := MakeTRIGrid(PointDEMs[i],nmRRI,false);
             for i := 0 to NumArea do AreaGrids[i] := MakeTRIGrid(AreaDEMs[i],nmRRI,false);
             DoCriterion('RRI_SSIM',PointGrids,AreaGrids);
             ClearDerivedGrids;

             //Elevation is done last, so we no longer need elevation to create derived grids, and can normalize it
             DoCriterion('ELEV_SSIM',PointDEMs,AreaDEMs);

             Results.SaveToFile(SavefName);
             Results.Free;
             CloseAllDEMs;
             CloseAndNilNumberedDB(DEMIXtileDB);
          end
          else begin
             {$IfDef RecordDEMIX} HighLightLineToDebugFile('Missing files for area ' + AreaName); {$EndIf}
          end;
       end;
   end {DoAnArea};

var
   Areas : tStringList;
   Area : shortstring;
   i : integer;
begin {procedure AreaSSIMComputations}
   {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMComputations in'); {$EndIf}
   try
      GetDEMIXPaths(true);
      SafeMakeDir(SSIMresultsDir);
      Areas := DEMIX_AreasWanted(true,Area);
      for i := 0 to pred(Areas.Count) do begin
         Area := Areas.Strings[i];
         wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + Area);
         DoAnArea(Area);
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMComputations out'); {$EndIf}
(*



   Run(DEMIX_Ref_1sec,DEMIX_ref_DEMs_no_sink,'*.tif',' ref DEMs');
   Run(DEMIX_test_dems,DEMIX_test_DEMs_no_sink,'*.dem',' test DEMs');

    SafeMakeDir(ChannelMissesDir);
    SavefName := ChannelMissesDir + Area + '_Channel_Misses.csv';
    if not FileExists(savefName) then begin
       MissingFiles := false;

       fName := DEMIX_ref_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + area + '_dtm_ref_1sec_area.dem';
       if not FileExists(fName) then fName := DEMIX_ref_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + area + '_ref_1sec_area.dem';
       TryToOpen(fName,AreaDEMs[0]);
       for i := 1 to NumArea do begin
          fName := DEMIX_test_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + Area + '_' + AreaNames[i]  + '.dem';
          TryToOpen(fName,AreaDEMs[i]);
       end;

       fName := DEMIX_ref_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + area + '_dtm_ref_1sec_point.dem';
       if not FileExists(fName) then fName := DEMIX_ref_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + area + '_ref_1sec_point.dem';
       TryToOpen(fName,PointDEMs[0]);
       for i := 1 to NumPt do begin
          fName := DEMIX_test_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + Area + '_' + PointNames[i]  + '.dem';
          TryToOpen(fName,PointDEMs[i]);
       end;

          //need a reference DEM to get the percentage full of the tiles
             fName := DEMIX_ref_DEMs_no_sink + 'saga_sinks_removed_' + Area +'_dtm_ref_1sec_area.tif';
             if not FileExists(fName) then fName := DEMIX_ref_DEMs_no_sink + 'saga_sinks_removed_' + Area + '_ref_1sec_area.tif';


       if MissingFiles then begin
          {$IfDef RecordDEMIX} WriteLineToDebugFile('Missing files for area ' + Area); {$EndIf}
       end
       else begin
          TryToOpen(fName,DEM);
          Results := tStringList.Create;
          Results.Add('DEMIX_TILE,LAT,LONG,ALOS,ASTER,COP,FABDEM,NASA,SRTM,CHAN_PIX');

          DEMIXtileDB := DEMIXtileFill(DEM,DEMGlb[DEM].DEMBoundBoxGeo,true);
          GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>' + IntToStr(MDDef.DEMIX_Full));

          GISdb[DEMIXtileDB].MyData.First;
          while not GISdb[DEMIXtileDB].MyData.eof do begin
             bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
             DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,GridLimits.xgridlow,GridLimits.ygridlow);
             DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,GridLimits.xgridhigh,GridLimits.ygridhigh);
             if GridLimits.xgridlow < 0 then GridLimits.xgridlow := 0;
             if GridLimits.xgridhigh > pred(DEMGlb[AreaDEMs[0]].DEMheader.NumCol) then GridLimits.xgridhigh := pred(DEMGlb[AreaDEMs[0]].DEMheader.NumCol);
             if GridLimits.ygridlow < 0 then GridLimits.ygridlow := 0;
             if GridLimits.ygridhigh > pred(DEMGlb[AreaDEMs[0]].DEMheader.NumRow) then GridLimits.ygridhigh := pred(DEMGlb[AreaDEMs[0]].DEMheader.NumRow);
             TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
             wmdem.SetPanelText(2, TileName);
             aLine := TileName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4);
             for i := 1 to NumArea do aLine := aline + ',' + RealToString(ProcessDEMTile(AreaDEMs[0],AreaDEMs[i]),-12,2);
             for i := 1 to NumPt do aLine := aline + ',' + RealToString(ProcessDEMTile(PointDEMs[0],PointDEMs[i]),-12,2);
             aLine := aline + ',' + IntToStr(NumRef);
             Results.Add(aline);
             GISdb[DEMIXtileDB].MyData.Next;
          end;
          Results.SaveToFile(SavefName);
          Results.Free;
          CloseAllDEMs;
       end;
    end;
*)
end {procedure AreaSSIMComputations};



procedure CompareChannelNetworks(Area : shortstring);
(*
const
   NumPt = 5;
   NumArea = 1;
   PointNames : array[0..NumPt] of shortstring = ('REF_POINT','ASTER','COP','FABDEM','NASA','SRTM');
   AreaNames : array[0..NumArea] of shortstring = ('REF_AREA','ALOS');
*)
var
   DEM,DEMIXtileDB,NumRef,i,j : integer;
   Results : tStringList;
   fName,SavefName : PathStr;
   aLine,TileName : shortstring;
   bb : sfBoundBox;
   MissingFiles : boolean;
   GridLimits : tGridLimits;
   (*
   PointDEMs : array [0..NumPt] of integer;
   AreaDEMs : array [0..NumArea] of integer;
   *)

   function ProcessDEMTile(Ref,Test : integer) : float32;
   var
      x,y : integer;
      ValidRef,ValidTest : boolean;
      TestOnRef,TestOffRef,NumTest : int64;
   begin
       if ValidDEM(Ref) and ValidDEM(Test) then begin
          TestOnRef := 0;
          TestOffRef := 0;
          NumRef := 0;
          NumTest := 0;
          for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
             for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
                ValidRef := (not DEMGlb[Ref].MissingDataInGrid(x,y));
                if ValidRef then inc(NumRef);
                ValidTest := (not DEMGlb[Test].MissingDataInGrid(x,y));
                if ValidTest then begin
                   inc(NumTest);
                   if ValidRef then begin
                      inc(TestOnRef);
                   end
                   else begin
                      inc(TestOffRef);
                   end;
                end;
             end;
          end;
          Result := 100 - (100 * TestOnRef / NumRef);
       end
       else Result := -999;
   end;


begin
    SafeMakeDir(ChannelMissesDir);
    SavefName := ChannelMissesDir + Area + '_Channel_Misses.csv';
    if not FileExists(savefName) then begin

       if OpenBothPixelIsDEMs(Area,'saga_channels_saga_sinks_removed_',DEMIX_ref_DEMs_channel_grids,DEMIX_test_DEMs_channel_grids,false) then begin
          MissingFiles := false;
          //need a reference DEM to get the percentage full of the tiles
             fName := DEMIX_ref_DEMs_no_sink + 'saga_sinks_removed_' + Area +'_dtm_ref_1sec_area.tif';
             if not FileExists(fName) then fName := DEMIX_ref_DEMs_no_sink + 'saga_sinks_removed_' + Area + '_ref_1sec_area.tif';
             if FileExists(fName) then DEM := OpenNewDEM(fName,false)
             else MissingFiles := true;
       end;

       if MissingFiles then begin
          {$IfDef RecordDEMIX} WriteLineToDebugFile('Missing files for area ' + Area); {$EndIf}
       end
       else begin
          //TryToOpen(fName,DEM);
          Results := tStringList.Create;
          Results.Add('DEMIX_TILE,LAT,LONG,ALOS,ASTER,COP,FABDEM,NASA,SRTM,CHAN_PIX');

          DEMIXtileDB := DEMIXtileFill(DEM,DEMGlb[DEM].DEMBoundBoxGeo,true);
          GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>' + IntToStr(MDDef.DEMIX_Full));

          GISdb[DEMIXtileDB].MyData.First;
          while not GISdb[DEMIXtileDB].MyData.eof do begin
             bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
             DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,GridLimits.xgridlow,GridLimits.ygridlow);
             DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,GridLimits.xgridhigh,GridLimits.ygridhigh);
             if GridLimits.xgridlow < 0 then GridLimits.xgridlow := 0;
             if GridLimits.xgridhigh > pred(DEMGlb[AreaDEMs[0]].DEMheader.NumCol) then GridLimits.xgridhigh := pred(DEMGlb[AreaDEMs[0]].DEMheader.NumCol);
             if GridLimits.ygridlow < 0 then GridLimits.ygridlow := 0;
             if GridLimits.ygridhigh > pred(DEMGlb[AreaDEMs[0]].DEMheader.NumRow) then GridLimits.ygridhigh := pred(DEMGlb[AreaDEMs[0]].DEMheader.NumRow);
             TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
             wmdem.SetPanelText(2, TileName);
             aLine := TileName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4);
             for i := 1 to NumArea do aLine := aline + ',' + RealToString(ProcessDEMTile(AreaDEMs[0],AreaDEMs[i]),-12,2);
             for i := 1 to NumPt do aLine := aline + ',' + RealToString(ProcessDEMTile(PointDEMs[0],PointDEMs[i]),-12,2);
             aLine := aline + ',' + IntToStr(NumRef);
             Results.Add(aline);
             GISdb[DEMIXtileDB].MyData.Next;
          end;
          Results.SaveToFile(SavefName);
          Results.Free;
          CloseAllDEMs;
       end;
    end;
end;


procedure ChannelNetworkMissPercentages;
var
   Areas : tStringList;
   Area : shortstring;
   i : integer;
begin
   try
      GetDEMIXPaths(true);
      Areas := DEMIX_AreasWanted(true,Area);
      for i := 0 to pred(Areas.Count) do begin
         Area := Areas.Strings[i];
         wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + Area);
         CompareChannelNetworks(Area);
      end;
   finally
      EndDEMIXProcessing;
   end;
end;


procedure BatchChannelNetworkComparison;
begin

end;



procedure ChannelNetworkMapComparison(AreaName,TestDEMName : shortstring);
const
   Color1 = clLime;   //Green: channel reference, not test
   Color2 = clRed;    //Red: channel test, not reference
   Color3 = clBlue;   //Blue channel test and reference
var
   BaseDEM,RefDEM, TestDEM,CompDEM,x,y : integer;
   GridLimits : tGridLimits;
   ValidRef,ValidTest : boolean;
   Lat,Long : float64;
   Legend,VectorResults : tStringList;
   fName : PathStr;
   Color : tColor;
begin
   MDdef.DBsOnAllMaps := false;

    TestDEM := OpenNewDEM(DEMIX_test_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + AreaName + '_' + TestDEMName + '.dem',false);

//need to deal with alos
    fName := DEMIX_ref_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + AreaName + '_dtm_ref_1sec_point.dem';
    if Not FileExists(fName) then fName := DEMIX_ref_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + AreaName + '_ref_1sec_point.dem';
    RefDEM := OpenNewDEM(fName,false);

    BaseDEM := OpenNewDEM(DEMIX_test_dems + areaName + '_' + TestDEMName + '.dem');

    (*
    RefDEM := OpenNewDEM('G:\madrid\ref_point_no_sink_channels.dem',true);
    TestDEM := OpenNewDEM('G:\madrid\COP_no_sink_channels.dem',true);
    BaseDEM := OpenNewDEM('G:\madrid\cop.tif',true);

    RefDEM := OpenNewDEM('G:\madrid\ref_point_no_sink_channels.dem',true);
    TestDEM := OpenNewDEM('G:\madrid\aster_no_sink_channels.dem',true);
    BaseDEM := OpenNewDEM('G:\madrid\aster.tif',true);
    *)

    CompDEM := DEMGlb[BaseDEM].SelectionMap.CreateGridToMatchMap(cgLatLong,true,ByteDEM,3600 * DEMGlb[RefDEM].DEMheader.DEMxSpacing,3600 * DEMGlb[RefDEM].DEMheader.DEMySpacing,-99,
         DEMGlb[RefDEM].DEMheader.RasterPixelIsGeoKey1025);
    GridLimits := DEMGlb[BaseDEM].FullDEMGridLimits;
    VectorResults := tStringList.Create;
    VectorResults.Add('LAT,LONG,COLOR');
    for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
       for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
          ValidRef := (not DEMGlb[RefDEM].MissingDataInGrid(x,y));
          ValidTest := (not DEMGlb[TestDEM].MissingDataInGrid(x,y));
          if ValidTest or ValidRef then begin
             DEMGlb[RefDEM].DEMGridToLatLongDegree(x,y,Lat,Long);
             if ValidTest and ValidRef then begin
                DEMGlb[CompDEM].SetGridElevation(x,y,3);
                Color := Color3;
             end
             else if ValidTest then begin
                DEMGlb[CompDEM].SetGridElevation(x,y,2);
                Color := Color2;
             end
             else if ValidRef then begin
                DEMGlb[CompDEM].SetGridElevation(x,y,1);
                Color := Color1;
             end;
             VectorResults.Add(RealToString(Lat,-12,-6)+ ',' + RealToString(Long,-12,-6) + ',' + IntToStr(Color));
          end;
       end;
    end;
    DEMGlb[CompDEM].CheckMaxMinElev;
    fName := MDtempDir + 'channels.dem';
    DEMGlb[CompDEM].WriteNewFormatDEM(fName,'');

    Legend := tStringList.Create;
    Legend.Add('COUNT,VALUE,NAME,COLOR');
    Legend.Add('99999,1,Under predictions (miss),' + IntToStr(Color2));
    Legend.Add('99999,2,Over predictions (false positive),' + IntToStr(Color1));
    Legend.Add('99999,3,Correct channels (hit),' + IntToStr(Color3));

    fName := ChangeFileExt(fName,'.vat.dbf');
    StringList2CSVtoDB(Legend,fName,true,false,false);
    DEMGlb[CompDEM].SelectionMap.MapDraw.MapType := mtDEMVATTable;
    DEMGlb[CompDEM].SelectionMap.DoBaseMapRedraw;

    fName := MDtempDir + 'channels.dbf';
    DEMGlb[BaseDEM].SelectionMap.StringListToLoadedDatabase(VectorResults,fName);
end;




function ChannelSHPtoGrid(DEM,db : integer; OutDir : PathStr; PlotOrder : integer = 1) : integer;
var
   grid : integer;
   Bitmap : tMyBitmap;
   fName : PathStr;

   procedure DoDatabase(LineWidth : integer = 1);
   var
      NewExt : shortstring;
   begin
      GISdb[db].LayerIsOn := true;
      GISdb[db].dbOpts.LineWidth := LineWidth;
      GISdb[db].dbOpts.LineColor := claBlack;
      GISdb[db].ApplyGISFilter('PLOT_ORDER > ' + IntToStr(PlotOrder));
      Grid := DEMGlb[DEM].SelectionMap.CreateGridToMatchMap(cgLatLong,true,ByteDEM,3600 * DEMGlb[DEM].DEMheader.DEMxSpacing,3600 * DEMGlb[DEM].DEMheader.DEMySpacing,-99,
         DEMGlb[DEM].DEMheader.RasterPixelIsGeoKey1025);
      DEMGlb[Grid].AreaName := GISdb[db].dbName;
      DEMGlb[Grid].SelectionMap.MapDraw.UseDistortedRawLatLongScaling := true;
      DEMGlb[Grid].SelectionMap.MapDraw.ResizeMapByPercentage(100);
      DEMGlb[Grid].SelectionMap.DoCompleteMapRedraw;
      DEMGlb[Grid].SelectionMap.MapDraw.MapType := mtElevRainbow;
      CopyImageToBitmap(DEMGlb[Grid].SelectionMap.Image1,Bitmap);
      GISdb[db].PlotDefaultSymbols(DEMGlb[Grid].SelectionMap.MapDraw,Bitmap);
      DEMGlb[Grid].SelectionMap.Image1.Picture.Graphic := Bitmap;

      WriteLineToDebugFile(DEMGlb[Grid].AreaName + ',' + IntToStr(PixelsWithThisColor(Bitmap,claBlack)));
      Bitmap.Free;
      DEMGlb[Grid].SelectionMap.EditGridViaColor(emvcSetElevation,clBlack,1,false,false);
      DEMGlb[Grid].SelectionMap.MaskFromSecondGrid(DEM, msSecondMissing);
      if (LineWidth > 1) then NewExt := '_buffer_' + IntToStr(LineWidth) else NewExt := '';
      fName := OutDir + ExtractFileNameNoExt(GISdb[db].DBFullName) + NewExt + '.dem';
      DEMGlb[Grid].WriteNewFormatDEM(fName);
      GISdb[db].LayerIsOn := false;
      Result := Grid;
   end;


begin
    NakedMapOptions;
    DoDataBase;
end;




procedure CreateChannelNetworkGridsFromVectors;


   procedure Run(BaseFolder,InFolder,OutFolder : PathStr; what : shortstring);
   var
      TheFiles : tStringList;
      InName, dbName,GridName : PathStr;
      i,DEM,db,NewGrid : integer;
   begin
      TheFiles := nil;
      Petmar.FindMatchingFiles(BaseFolder,'*.tif',TheFiles,1);
      {$IfDef RecordDEMIX} WriteLineToDebugFile(What + ' file=' + IntToStr(TheFiles.Count)); {$EndIf}
      if (TheFiles.Count > 0) then begin
         SafeMakeDir(OutFolder);
         for i := 0 to pred(TheFiles.Count) do begin
             wmdem.SetPanelText(1,What + ' ' + IntToStr(i) + '/' + IntToStr(TheFiles.Count));
             InName := TheFiles.Strings[i];
             dbName := InFolder + 'saga_channels_' + ExtractFileNameNoExt(InName) + '.shp';
             GridName := OutFolder + ExtractFileNameNoExt(dbName) + '.dem';

             if (IsDEMaDSMorDTM(InName) = DEMisDSM) then begin
                File2Trash(InName);
             end
             else if not FileExists(dbName) then begin
                {$IfDef RecordDEMIX} WriteLineToDebugFile('Missing file=' + dbName); {$EndIf}
             end
             else if (not FileExists(GridName)) and FileExists(dbName) {and (IsDEMaDSMorDTM(InName) = DEMisDTM)} then begin
                //TemporaryNewGeotiff := false;
                //RewriteGeotiffIfRequired(InName);
                DEM := OpenNewDEM(InName,true);
                db := DEMGlb[DEM].SelectionMap.OpenDBonMap('',dbName,true,false);
                NewGrid := ChannelSHPtoGrid(DEM,db,OutFolder);
                CloseSingleDEM(DEM);
                CloseSingleDEM(NewGrid);
                CleanUpTempDirectory(false);
             end;
         end;
         TheFiles.Free;
      end;
   end;

begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateChannelNetworkGridsFromVectors in'); {$EndIf}
   try
      HeavyDutyProcessing := true;
      Run(DEMIX_ref_DEMs_no_sink,DEMIX_ref_DEMs_channels,DEMIX_ref_DEMs_channel_grids,'Ref DEMs');
      Run(DEMIX_test_DEMs_no_sink,DEMIX_test_DEMs_channels,DEMIX_test_DEMs_channel_grids,'Test DEMs');
   finally
      wmdem.SetPanelText(1,'');
      HeavyDutyProcessing := false;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateChannelNetworkGridsFromVectors out'); {$EndIf}
end;



procedure BatchCreateVectorChannelNewtwork;

   procedure Run(InFolder,OutFolder : PathStr; what : shortstring);
   var
      TheFiles : tStringList;
      InName, OutName : PathStr;
      i,NumDone : integer;
   begin
      TheFiles := nil;
      Petmar.FindMatchingFiles(InFolder,'*.tif',TheFiles,1);
      {$IfDef RecordDEMIX} WriteLineToDebugFile(What + ' file=' + IntToStr(TheFiles.Count)); {$EndIf}
      if (TheFiles.Count > 0) then begin
         SafeMakeDir(OutFolder);
         NumDone := 0;
         StartProgress('Channel network for ' + what);
         for i := 0 to pred(TheFiles.Count) do begin
             if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);
             InName := TheFiles.Strings[i];
             OutName := OutFolder + 'saga_channels_' + ExtractFileNameNoExt(InName) + '.shp';
             if (not FileExists(OutName)) and (IsDEMaDSMorDTM(InName) = DEMisDTM) then begin
                SagaChannelShapefile(InName,OutName);
                CleanUpTempDirectory(false);
                inc(NumDone);
             end;
         end;
         TheFiles.Free;
         EndProgress;
         {$IfDef RecordDEMIX} WriteLineToDebugFile(What + ' shp file created=' + IntToStr(NumDone) + ' in ' + OutFolder); {$EndIf}
      end;
   end;


begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchCreateVectorChannelNewtwork in'); {$EndIf}
   HeavyDutyProcessing := true;
   Run(DEMIX_ref_DEMs_no_sink,DEMIX_ref_DEMs_channels,' ref DEMs');
   Run(DEMIX_test_dems_no_sink,DEMIX_test_DEMs_channels,' test DEMs');
   HeavyDutyProcessing := false;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchCreateVectorChannelNewtwork out'); {$EndIf}
end;




procedure BatchFillHolesInDEMIX_DEMS;

   procedure Run(InFolder,OutFolder : PathStr; Ext,what : shortstring);
   var
      TheFiles : tStringList;
      InName, OutName : PathStr;
      i : integer;
   begin
      TheFiles := nil;
      SafeMakeDir(OutFolder);
      StartProgress('Remove sinks for ' + what);
      Petmar.FindMatchingFiles(InFolder,Ext,TheFiles,1);
      {$IfDef RecordDEMIX} WriteLineToDebugFile(What + ' file=' + IntToStr(TheFiles.Count)); {$EndIf}
      for i := 0 to pred(TheFiles.Count) do begin
          if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);
          InName := TheFiles.Strings[i];
          OutName := OutFolder + 'saga_sinks_removed_' + ExtractFileNameNoExt(InName) + '.tif';
          if (not FileExists(OutName)) and (IsDEMaDSMorDTM(InName) = DEMisDTM) then SagaSinkRemoval(InName,OutName);
      end;
      TheFiles.Free;
      EndProgress;
   end;


begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchFillHolesInDEMIX_DEMS in'); {$EndIf}
   HeavyDutyProcessing := true;
   Run(DEMIX_Ref_1sec,DEMIX_ref_DEMs_no_sink,'*.tif',' ref DEMs');
   Run(DEMIX_test_dems,DEMIX_test_DEMs_no_sink,'*.dem',' test DEMs');
   HeavyDutyProcessing := false;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchFillHolesInDEMIX_DEMS Out'); {$EndIf}
end;




