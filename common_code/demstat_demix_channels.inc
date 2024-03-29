{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for demstat        }
{___________________________________}


procedure ChannelNetworkMissPercentages(Overwrite : boolean; AreasWanted : tstringlist = nil);
var
   AreaName : shortstring;
   i : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('ChannelNetworkMissPercentages in'); {$EndIf}
   try
      GetDEMIXPaths(true);
      if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         wmdem.SetPanelText(3, 'Channel misses: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + '  ' + AreaName,true);
         CompareChannelNetworks(AreaName);
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('ChannelNetworkMissPercentages out'); {$EndIf}
end;


procedure ChannelNetworkMapComparison(AreaName,TestDEMName : shortstring);
const
   MakeGrid = false;
   Color1 = clLime;   //Green: channel reference, not test
   Color2 = clRed;    //Red: channel test, not reference
   Color3 = clBlue;   //Blue channel test and reference
var
   db,BaseDEM,RefDEM, TestDEM,CompDEM,x,y : integer;
   GridLimits : tGridLimits;
   ValidRef,ValidTest : boolean;
   Lat,Long : float64;
   Legend,VectorResults : tStringList;
   fName : PathStr;
   Color : tColor;
   TStr : shortstring;
begin
   MDdef.DBsOnAllMaps := false;
    TestDEM := OpenNewDEM(DEMIX_test_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + AreaName + '_' + TestDEMName + '.tif',false);
    if UpperCase(TestDEMName) = 'ALOS' then TStr := 'area' else TStr := 'point';
    fName := DEMIX_ref_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + AreaName + '_dtm_ref_1sec_' + Tstr + '.tif';
    //if Not FileExists(fName) then fName := DEMIX_ref_DEMs_channel_grids + 'saga_channels_saga_sinks_removed_' + AreaName + Ref1SecPointStr + '.dem';
    RefDEM := OpenNewDEM(fName,false);

    BaseDEM := OpenNewDEM(DEMIX_test_dems + areaName + '_' + TestDEMName + '.tif',false);
    CreateDEMSelectionMap(BaseDEM,true,true,mtGrayReflect);

    if MakeGrid then CompDEM := DEMGlb[BaseDEM].SelectionMap.CreateGridToMatchMap(cgLatLong,true,ByteDEM,3600 * DEMGlb[RefDEM].DEMheader.DEMxSpacing,3600 * DEMGlb[RefDEM].DEMheader.DEMySpacing,-99,
         DEMGlb[RefDEM].DEMheader.RasterPixelIsGeoKey1025)
    else CompDEM := 0;
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
                if (CompDEM <> 0) then DEMGlb[CompDEM].SetGridElevation(x,y,3);
                Color := Color3;
             end
             else if ValidTest then begin
                if (CompDEM <> 0) then DEMGlb[CompDEM].SetGridElevation(x,y,2);
                Color := Color2;
             end
             else if ValidRef then begin
                if (CompDEM <> 0) then DEMGlb[CompDEM].SetGridElevation(x,y,1);
                Color := Color1;
             end;
             VectorResults.Add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' + IntToStr(Color));
          end;
       end;
    end;
    if (CompDEM <> 0) then begin
       DEMGlb[CompDEM].CheckMaxMinElev;
       fName := NextFileNumber(MDtempDir,TestDEMName + '_' + AreaName + '_channels_', '.tif');
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
    end;

    fName := NextFileNumber(MDtempDir,TestDEMName + '_' + AreaName + '_channels_', '.dbf');
    db := DEMGlb[BaseDEM].SelectionMap.StringListToLoadedDatabase(VectorResults,fName,false);
    GISdb[db].dbOpts.dbAutoShow := dbasColorField;
    GISdb[db].dbOpts.Symbol.Size := 1;
    GISdb[db].RedrawLayerOnMap;
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
      fName := OutDir + ExtractFileNameNoExt(GISdb[db].DBFullName) + NewExt + '.tif';
      DEMGlb[Grid].WriteNewFormatDEM(fName);
      GISdb[db].LayerIsOn := false;
      Result := Grid;
   end;


begin
    NakedMapOptions;
    DoDataBase;
end;

procedure CompareChannelNetworks(Area : shortstring);
var
   DEM,DEMIXtileDB,NumRef,i,j : integer;
   Results : tStringList;
   fName,SavefName : PathStr;
   aLine,TileName : shortstring;
   bb : sfBoundBox;
   MissingFiles : boolean;
   GridLimits : tGridLimits;

   function ProcessDEMTile(Ref,Test,Buffer : integer) : float32;
   var
      x,y,xl,yl : integer;
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
                if Buffer = 0 then begin
                   ValidTest := (not DEMGlb[Test].MissingDataInGrid(x,y));
                end
                else begin
                   ValidTest := false;
                   for xl := -Buffer to Buffer do
                      for yl := -Buffer to Buffer do
                         if (not DEMGlb[Test].MissingDataInGrid(x+xl,y+yl)) then ValidTest := true;
                end;
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
          Result := 1 - (TestOnRef / NumRef);
       end
       else Result := -999;
   end;

       procedure DoTolerance(Criterion : shortstring; Buffer : integer);
       var
          i : integer;
       begin
          aLine := TileName + ',' + Area + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + Criterion;
          for i := 1 to NumPt do aLine := aline + ',' + RealToString(ProcessDEMTile(PointDEMs[0],PointDEMs[i],Buffer),-12,6);
          for i := 1 to NumArea do aLine := aline + ',' + RealToString(ProcessDEMTile(AreaDEMs[0],AreaDEMs[i],Buffer),-12,6);
          aLine := aline + ',' + IntToStr(NumRef);
          Results.Add(aline);
       end;


const
   OpenMaps = false;  //available for debugging
begin
    SafeMakeDir(ChannelMissesDir);
    SavefName := ChannelMissesDir + Area + '_Channel_Misses.csv';
    if not FileExists(savefName) then begin
       wmdem.SetPanelText(2, 'Load DEMs',true);

       if OpenBothPixelIsDEMs(Area,'saga_channels_saga_sinks_removed_',DEMIX_ref_DEMs_channel_grids,DEMIX_test_DEMs_channel_grids,OpenMaps) then begin
          MissingFiles := false;
          //need a reference DEM to get the percentage full of the tiles
             fName := DEMIX_ref_DEMs_no_sink + 'saga_sinks_removed_' + Area +'_dtm' + Ref1SecAreaStr + '.tif';
             if FileExists(fName) then DEM := OpenNewDEM(fName,OpenMaps)
             else MissingFiles := true;
       end
       else MissingFiles := true;

       if MissingFiles then begin
          {$IfDef RecordDEMIX} WriteLineToDebugFile('Missing files for area ' + Area); {$EndIf}
       end
       else begin
          Results := tStringList.Create;
          aline := 'DEMIX_TILE,AREA,LAT,LONG,CRITERION';
          for i := 1 to NumPt do aLine := aline + ',' + PointNames[i];
          for i := 1 to NumArea do aLine := aline + ',' + AreaNames[i];
          Results.Add(aLine + ',CHAN_PIX');

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
             wmdem.SetPanelText(2, TileName,true);

             DoTolerance('CHAN_MISS1',0);
             DoTolerance('CHAN_MISS3',1);
             GISdb[DEMIXtileDB].MyData.Next;
          end;
          Results.SaveToFile(SavefName);
          Results.Free;
          if OpenMaps then begin
             wmdem.tile;
             //MessageToContinue('Look at maps');
          end;

          CloseAllDEMs;
          //wmdem.SetPanelText(2, '');
       end;
    end;
end;




procedure CreateChannelNetworkGridsFromVectors(Overwrite : boolean; AreasWanted : tstringlist = nil);
var
   AreaName : shortstring;
   i : integer;

   procedure Run(BaseFolder,InFolder,OutFolder : PathStr; what : shortstring);
   var
      TheFiles : tStringList;
      InName, dbName,GridName : PathStr;
      i,DEM,db,NewGrid : integer;
   begin
      wmdem.SetPanelText(3, 'Channel grids ' + What,true);
      TheFiles := nil;
      Petmar.FindMatchingFiles(BaseFolder,'*.tif',TheFiles,1);
      RemoveFilesThatDoNotHaveString(TheFiles,AreaName);

      {$IfDef RecordDEMIX} WriteLineToDebugFile(What + ' file=' + IntToStr(TheFiles.Count) + '  ' + AreaName); {$EndIf}
      if (TheFiles.Count > 0) then begin
         SafeMakeDir(OutFolder);
         for i := 0 to pred(TheFiles.Count) do begin
             wmdem.SetPanelText(2,What + ' ' + IntToStr(i) + '/' + IntToStr(TheFiles.Count),true);
             InName := TheFiles.Strings[i];
             dbName := InFolder + 'saga_channels_' + ExtractFileNameNoExt(InName) + '.shp';
             GridName := OutFolder + ExtractFileNameNoExt(dbName) + '.tif';

             if (IsDEMaDSMorDTM(InName) = DEMisDSM) then begin
                File2Trash(InName);
             end
             else if not FileExists(dbName) then begin
                {$IfDef RecordDEMIX} WriteLineToDebugFile('Missing file=' + dbName); {$EndIf}
             end
             else if ((not FileExists(GridName)) or Overwrite) then begin
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
      if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         Run(DEMIX_ref_DEMs_no_sink,DEMIX_ref_DEMs_channels,DEMIX_ref_DEMs_channel_grids,'Ref DEMs');
         Run(DEMIX_test_DEMs_no_sink,DEMIX_test_DEMs_channels,DEMIX_test_DEMs_channel_grids,'Test DEMs');
      end;
   finally
     // wmdem.SetPanelText(1,'');
      //HeavyDutyProcessing := false;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateChannelNetworkGridsFromVectors out'); {$EndIf}
end;



procedure BatchCreateVectorChannelNewtwork(Overwrite : boolean; AreasWanted : tstringList = nil);
var
   AreaName : shortstring;

   procedure Run(InFolder,OutFolder : PathStr; what : shortstring);
   var
      TheFiles : tStringList;
      InName, OutName : PathStr;
      i,NumDone : integer;
   begin
      wmdem.SetPanelText(3, 'Vector channels ' + What,true);
      TheFiles := nil;
      Petmar.FindMatchingFiles(InFolder,'*.tif',TheFiles,1);
      RemoveFilesThatDoNotHaveString(TheFiles,AreaName);

      {$IfDef RecordDEMIX} WriteLineToDebugFile(What + ' file=' + IntToStr(TheFiles.Count)); {$EndIf}
      if (TheFiles.Count > 0) then begin
         SafeMakeDir(OutFolder);
         NumDone := 0;
         //StartProgress('Channel network for ' + what);
         for i := 0 to pred(TheFiles.Count) do begin
             //if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);
             wmdem.SetPanelText(2, IntToStr(succ(i)) + '/' + IntToStr(TheFiles.Count),true);
             InName := TheFiles.Strings[i];
             OutName := OutFolder + 'saga_channels_' + ExtractFileNameNoExt(InName) + '.shp';
             if (IsDEMaDSMorDTM(InName) = DEMisDTM) then begin
                if (not FileExists(OutName)) or Overwrite then begin
                   SagaChannelShapefile(InName,OutName);
                   CleanUpTempDirectory(false);
                   inc(NumDone);
                end;
             end;
         end;
         TheFiles.Free;
         {$IfDef RecordDEMIX} WriteLineToDebugFile(What + ' shp file created=' + IntToStr(NumDone) + ' in ' + OutFolder); {$EndIf}
      end;
   end;

var
   i : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchCreateVectorChannelNewtwork in'); {$EndIf}
   //HeavyDutyProcessing := true;
   if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;
   for i := 0 to pred(AreasWanted.Count) do begin
      AreaName := AreasWanted.Strings[i];
      Run(DEMIX_ref_DEMs_no_sink,DEMIX_ref_DEMs_channels,' ref DEMs');
      Run(DEMIX_test_dems_no_sink,DEMIX_test_DEMs_channels,' test DEMs');
   end;
   //HeavyDutyProcessing := false;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchCreateVectorChannelNewtwork out'); {$EndIf}
end;



procedure BatchRemoveSinksInDEMIX_DEMS(Overwrite : boolean; AreasWanted : tStringList);
var
   AreaName : ShortString;
   i : integer;

   procedure Run(InFolder,OutFolder : PathStr; Ext,what : shortstring);
   var
      TheFiles : tStringList;
      InName, OutName : PathStr;
      i : integer;
   begin
      wmdem.SetPanelText(3, 'Remove sinks ' + What + ' ' + AreaName,true);
      TheFiles := nil;
      SafeMakeDir(OutFolder);
      Petmar.FindMatchingFiles(InFolder,Ext,TheFiles,1);
      RemoveFilesThatDoNotHaveString(TheFiles,AreaName);
      {$IfDef RecordDEMIXFull} WriteLineToDebugFile(What + ' file=' + IntToStr(TheFiles.Count)); {$EndIf}
      for i := 0 to pred(TheFiles.Count) do begin
          wmdem.SetPanelText(2, IntToStr(succ(i)) + '/' + IntToStr(TheFiles.Count),true);
          InName := TheFiles.Strings[i];
          if (IsDEMaDSMorDTM(InName) = DEMisDTM) then begin
             OutName := OutFolder + 'saga_sinks_removed_' + ExtractFileNameNoExt(InName) + '.tif';
             if (not FileExists(OutName)) or Overwrite then begin
                SagaSinkRemoval(InName,OutName);
             end;
          end;
      end;
      TheFiles.Free;
      EndProgress;
   end;


begin
   if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;
   for i := 0 to pred(AreasWanted.Count) do begin
      AreaName := AreasWanted.Strings[i];
      {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchRemoveSinksInDEMIX_DEMS start test DEMs'); {$EndIf}
      Run(DEMIX_test_dems,DEMIX_test_DEMs_no_sink,'*.tif',' test DEMs');
      {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchRemoveSinksInDEMIX_DEMS start ref DEMs'); {$EndIf}
      Run(DEMIX_Ref_1sec,DEMIX_ref_DEMs_no_sink,'*.tif',' ref DEMs');
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchRemoveSinksInDEMIX_DEMS Out'); {$EndIf}
end;


procedure MultistepChannelNetworks(Overwrite : boolean);
var
   AreasWanted : tStringList;
   AreaName : shortstring;
begin
   try
      GetDEMIXpaths;
      HeavyDutyProcessing := true;
      LockStatusBar := true;
      AreasWanted := DEMIX_AreasWanted;
      BatchRemoveSinksInDEMIX_DEMS(Overwrite,AreasWanted);
      BatchCreateVectorChannelNewtwork(Overwrite,AreasWanted);
      CreateChannelNetworkGridsFromVectors(Overwrite,AreasWanted);
      ChannelNetworkMissPercentages(Overwrite,AreasWanted);
   finally
      HeavyDutyProcessing := false;
      LockStatusBar := false;
   end;
end;




