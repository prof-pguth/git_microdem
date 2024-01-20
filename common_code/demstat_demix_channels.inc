{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for demstat        }
{___________________________________}



procedure ChannelSHPtoGrid(DEM,db : integer; OutDir : PathStr; PlotOrder : integer = 1);
//need to have a DEM loaded, and the shape files with the channel network opened on it
var
   i,grid : integer;
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
      DEMGlb[Grid].AreaName := GISdb[i].dbName;
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
   end;


begin
    NakedMapOptions;
    DoDataBase;
    (*
    MDdef.DBsOnAllMaps := false;
    for i := 1 to MaxDataBase do begin
       if ValidDB(i) then begin
          GISdb[i].LayerIsOn := false;
       end;
    end;

    for i := 1 to MaxDataBase do begin
       if ValidDB(i) then begin
          DoDatabase(i,1);
          //if StrUtils.AnsiContainsText(UpperCase(GISdb[i].dbName),'REF_') then DoDatabase(i,3);   //creates grid with buffer of 1 pixel from 3 pixel line width
       end;
    end;
    *)
end;




procedure CreateChannelNetworkGridsFromVectors;


   procedure Run(BaseFolder,InFolder,OutFolder : PathStr; what : shortstring);
   var
      TheFiles : tStringList;
      InName, dbName,GridName : PathStr;
      i,DEM,db,NewGrid : integer;
   begin
      TheFiles := nil;
      SafeMakeDir(OutFolder);
      StartProgress('Make raster channels for ' + what);
      Petmar.FindMatchingFiles(BaseFolder,'*.tif',TheFiles,1);
      for i := 0 to pred(TheFiles.Count) do begin
          if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);
          InName := TheFiles.Strings[i];
          dbName := InFolder + 'saga_channels_' + ExtractFileNameNoExt(InName) + '.shp';
          GridName := OutFolder + ExtractFileNameNoExt(dbName) + '.dem';

          if (not FileExists(GridName)) {and FileExists(inName)} and FileExists(dbName) and (IsDEMaDSMorDTM(InName) = DEMisDTM) then begin
             DEM := OpenNewDEM(InName,true);
             db := DEMGlb[DEM].SelectionMap.OpenDBonMap('',dbName,true,false);
             ChannelSHPtoGrid(DEM,db,OutFolder);
             CloseSingleDEM(DEM);
             //CloseSingleDEM(NewGrid);
             CleanUpTempDirectory(false)
          end;
      end;
      TheFiles.Free;
      EndProgress;
   end;

begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateChannelNetworkGridsFromVectors in'); {$EndIf}
   Run(DEMIX_ref_DEMs_no_sink,DEMIX_ref_DEMs_channels,DEMIX_ref_DEMs_channel_grids,'Ref DEMs');
   Run(DEMIX_test_DEMs_no_sink,DEMIX_test_DEMs_channels,DEMIX_test_DEMs_channel_grids,'Ref DEMs');
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateChannelNetworkGridsFromVectors out'); {$EndIf}
end;



procedure BatchCreateVectorChannelNewtwork;


   procedure Run(InFolder,OutFolder : PathStr; what : shortstring);
   var
      TheFiles : tStringList;
      InName, OutName : PathStr;
      i : integer;
   begin
      TheFiles := nil;
      SafeMakeDir(OutFolder);
      StartProgress('Channel network for ' + what);
      Petmar.FindMatchingFiles(InFolder,'*.tif',TheFiles,1);
      for i := 0 to pred(TheFiles.Count) do begin
          if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);
          InName := TheFiles.Strings[i];
          OutName := OutFolder + 'saga_channels_' + ExtractFileNameNoExt(InName) + '.shp';
          if not FileExists(OutName) then SagaChannelShapefile(InName,OutName);
      end;
      TheFiles.Free;
      EndProgress;
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
      for i := 0 to pred(TheFiles.Count) do begin
          if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);
          InName := TheFiles.Strings[i];
          OutName := OutFolder + 'saga_sinks_removed_' + ExtractFileNameNoExt(InName) + '.tif';
          if not FileExists(OutName) then SagaSinkRemoval(InName,OutName);
      end;
      TheFiles.Free;
      EndProgress;
   end;


begin
   HeavyDutyProcessing := true;
   Run(DEMIX_Ref_1sec,DEMIX_ref_DEMs_no_sink,'*.tif',' ref DEMs');
   Run(DEMIX_test_dems,DEMIX_test_DEMs_no_sink,'*.dem',' test DEMs');
   HeavyDutyProcessing := false;
end;




procedure ChannelNetworkMapComparison(BaseDEM,RefDEM, TestDEM : integer);
const
   Color1 = clLime;   //Green: channel reference, not test
   Color2 = clRed;    //Red: channel test, not reference
   Color3 = clBlue;   //Blue channel test and reference
var
   CompDEM,x,y : integer;
   GridLimits : tGridLimits;
   ValidRef,ValidTest : boolean;
   Lat,Long : float64;
   Legend,VectorResults : tStringList;
   fName : PathStr;
   Color : tColor;
begin
    RefDEM := OpenNewDEM('G:\madrid\ref_point_no_sink_channels.dem',true);
    TestDEM := OpenNewDEM('G:\madrid\COP_no_sink_channels.dem',true);
    BaseDEM := OpenNewDEM('G:\madrid\cop.tif',true);

    (*
    RefDEM := OpenNewDEM('G:\madrid\ref_point_no_sink_channels.dem',true);
    TestDEM := OpenNewDEM('G:\madrid\aster_no_sink_channels.dem',true);
    BaseDEM := OpenNewDEM('G:\madrid\aster.tif',true);
    *)

    CompDEM := DEMGlb[RefDEM].SelectionMap.CreateGridToMatchMap(cgLatLong,true,ByteDEM,3600 * DEMGlb[RefDEM].DEMheader.DEMxSpacing,3600 * DEMGlb[RefDEM].DEMheader.DEMySpacing,-99,
         DEMGlb[RefDEM].DEMheader.RasterPixelIsGeoKey1025);
    GridLimits := DEMGlb[RefDEM].FullDEMGridLimits;
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
    fName := MDtempDir + 'channels.dbf';
    DEMGlb[BaseDEM].SelectionMap.StringListToLoadedDatabase(VectorResults,fName);
    DEMGlb[CompDEM].SelectionMap.MapDraw.MapType := mtDEMVATTable;
    DEMGlb[CompDEM].SelectionMap.DoBaseMapRedraw;
end;




procedure CompareChannelNetworks;
const
   NumPt = 5;
   NumArea = 1;
   PointNames : array[0..NumPt] of shortstring = ('REF_POINT','ASTER','COP','FABDEM','NASA','SRTM');
   AreaNames : array[0..NumArea] of shortstring = ('REF_AREA','ALOS');
var
   DEMIXtileDB,NumRef,i,j,DEM : integer;
   Results : tStringList;
   fName : PathStr;
   aLine : shortstring;
   bb : sfBoundBox;
   GridLimits : tGridLimits;
   PointDEMs : array [0..NumPt] of integer;
   AreaDEMs : array [0..NumArea] of integer;

   function ProcessDEM(Ref,Test : integer) : float32;
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
    Results := tStringList.Create;
    Results.Add('DEMIX_TILE,LAT,LONG,ALOS,ASTER,COP,FABDEM,NASA,SRTM,CHAN_PIX');
    for j := 1 to MaxDEMDataSets do if ValidDEM(j) then begin
       for i := 0 to NumPt do if StrUtils.AnsiContainsText(UpperCase(DEMGlb[j].AreaName),PointNames[i]) then PointDEMs[i] := j;
       for i := 0 to NumArea do if StrUtils.AnsiContainsText(UpperCase(DEMGlb[j].AreaName),AreaNames[i]) then AreaDEMs[i] := j;
    end;

    DEM := OpenNewDEM('G:\madrid\ref_point.tif',true);

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

       aLine := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME') + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4);
       for i := 1 to NumArea do aLine := aline + ',' + RealToString(ProcessDEM(AreaDEMs[0],AreaDEMs[i]),-12,2);
       for i := 1 to NumPt do aLine := aline + ',' + RealToString(ProcessDEM(PointDEMs[0],PointDEMs[i]),-12,2);
       aLine := aline + ',' + IntToStr(NumRef);
       Results.Add(aline);
       GISdb[DEMIXtileDB].MyData.Next;
    end;

    fName := MDTempDir + 'Channel_stats.dbf';
    DEMglb[DEM].SelectionMap.StringListToLoadedDatabase(Results,fname);
end;


