{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{include file for demix_definitiions}
{___________________________________}

function GetDEMgridLimitsFromBB(DEM,DEMIXtileDB : integer; var TileName : shortstring; var bb : sfBoundBox) : tGridLimits;
begin
   TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
   wmdem.SetPanelText(2, TileName,true);
    bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
    DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,Result.xgridlow,Result.ygridlow);
    DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,Result.xgridhigh,Result.ygridhigh);
    if (Result.xgridlow < 0) then Result.xgridlow := 0;
    if (Result.xgridhigh > pred(DEMGlb[AreaDEMs[0]].DEMheader.NumCol)) then Result.xgridhigh := pred(DEMGlb[AreaDEMs[0]].DEMheader.NumCol);
    if (Result.ygridlow < 0) then Result.ygridlow := 0;
    if (Result.ygridhigh > pred(DEMGlb[AreaDEMs[0]].DEMheader.NumRow)) then Result.ygridhigh := pred(DEMGlb[AreaDEMs[0]].DEMheader.NumRow);
end;

function ProcessDEMTile(Ref,Test,Buffer : integer; GridLimits : tGridLimits) : float32;
var
   x,y,xl,yl,xoffset,yoffset : integer;
   ValidRef,ValidTest : boolean;
   NumRef,TestOnRef,TestOffRef,RefWithoutTest,NumTest : int64;
begin
    Result := -999;
    if ValidDEM(Ref) and ValidDEM(Test) then begin
       if DEMGlb[Ref].SecondGridJustOffset(Test,xoffset,yoffset) then begin
          TestOnRef := 0;
          TestOffRef := 0;
          RefWithoutTest := 0;
          NumRef := 0;
          NumTest := 0;
          for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
             for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
                ValidRef := (not DEMGlb[Ref].MissingDataInGrid(x,y));
                if ValidRef then inc(NumRef);

                if (Buffer = 0) then begin
                   ValidTest := (not DEMGlb[Test].MissingDataInGrid(x+Xoffset,y+Yoffset));
                end
                else begin
                   ValidTest := false;
                   for xl := -Buffer to Buffer do
                      for yl := -Buffer to Buffer do
                         if (not DEMGlb[Test].MissingDataInGrid(x+xl+xoffset,y+yl+Yoffset)) then ValidTest := true;
                end;

                if ValidTest then begin
                   inc(NumTest);
                   if ValidRef then begin
                      inc(TestOnRef);
                   end
                   else begin
                      inc(TestOffRef);
                   end;
                end
                else if ValidRef then inc(RefWithoutTest);
             end;
          end;
          Result := 1 - (TestOnRef / NumRef);
       end
       else begin
          {$IfDef RecordDEMIXchannels} WriteLineToDebugFile('ProcessDEMTile fail invalide offset, ref=' + IntToStr(Ref) + '  test=' + IntToStr(Test)); {$EndIf}
       end;
    end
    else begin
       {$IfDef RecordDEMIXchannels} WriteLineToDebugFile('ProcessDEMTile fail invalid DEM, ref=' + IntToStr(Ref) + '  test=' + IntToStr(Test)); {$EndIf}
    end;
end;



procedure DEMIX_CreateGridsFromVectors(Overwrite : boolean);
//this is now set for valleys and ridges created by WbW
var
   AreaName,TileName : shortstring;
   i : integer;
   bb : sfBoundBox;
   GridLimits : tGridLimits;
   Results : tStringList;


      function MakeGridFromVectors(VD : PathStr; DEM : integer; fType : shortstring) : integer;
      var
         db : integer;
         Bitmap : tMyBitmap;
      var
         fName : PathStr;
      begin
         FName := VD + ftype + DEMglb[DEM].AreaName + '.tif.shp';
         if FileExists(fName) then begin
            AddBBtoShapeFiles := false;
            if OpenNumberedGISDataBase(db,fName) then begin
               {$IfDef RecordGridFromVector} WriteLineToDebugFile(FName); {$EndIf}
               Result := DEMGlb[DEM].SelectionMap.CreateGridToMatchMap(cgLatLong,true,ByteDEM,3600 * DEMGlb[DEM].DEMheader.DEMxSpacing,3600 * DEMGlb[DEM].DEMheader.DEMySpacing,-99,
                  DEMGlb[DEM].DEMheader.RasterPixelIsGeoKey1025,ftype + DEMglb[DEM].AreaName);
               {$IfDef RecordGridFromVector} WriteLineToDebugFile('Map created: ' + DEMGlb[Result].AreaName); {$EndIf}
               DEMGlb[Result].SelectionMap.MapDraw.UseDistortedRawLatLongScaling := true;
               DEMGlb[Result].SelectionMap.MapDraw.ResizeMapByPercentage(100);
               DEMGlb[Result].SelectionMap.DoCompleteMapRedraw;
               DEMGlb[Result].SelectionMap.MapDraw.MapType := mtElevRainbow;
               CopyImageToBitmap(DEMGlb[Result].SelectionMap.Image1,Bitmap);
               GISdb[db].LayerIsOn := true;
               GISdb[db].dbOpts.LineWidth := 1;
               GISdb[db].dbOpts.LineColor := claBlack;
               GISdb[db].PlotDefaultSymbols(DEMGlb[Result].SelectionMap.MapDraw,Bitmap);
               DEMGlb[Result].SelectionMap.Image1.Picture.Graphic := Bitmap;
               {$IfDef RecordGridFromVector} WriteLineToDebugFile('Map plotted: ' + DEMGlb[Result].AreaName); {$EndIf}
               Bitmap.Free;
               DEMGlb[Result].SelectionMap.EditGridViaColor(emvcSetElevation,clBlack,1,false,false);
               DEMGlb[Result].SelectionMap.MaskFromSecondGrid(DEM, msSecondMissing);
               {$IfDef RecordGridFromVector} WriteLineToDebugFile('Map maskeded: ' + DEMGlb[Result].AreaName); {$EndIf}
               //fName := 'c:\temp\' + AreaName + '.dem';
               //DEMGlb[Result].WriteNewFormatDEM(fName);
               GISdb[db].LayerIsOn := false;
            end
            else begin
               {$IfDef RecordDEMIX} WriteLineToDebugFile('DB open fails ' + fName); {$EndIf}
               Result := 0;
            end;
         end
         else begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Missing ' + fName); {$EndIf}
            Result := 0;
         end;
         //CloseSingleDEM(DEM);
      end;


       procedure DoTolerance(Criterion : shortstring; Buffer : integer; PointGrids,AreaGrids : tDEM_int_array);
       var
          i : integer;
          aline : shortString;
       begin
          aLine := TileName + ',' + AreaName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + Criterion;
          for i := 1 to NumPtDEMs do begin
             aLine := aline + ',' + RealToString(ProcessDEMTile(PointGrids[dmxFirstPoint],PointGrids[i],Buffer,GridLimits),-12,6);
          end;
          for i := 1 to NumAreaDEMs do aLine := aline + ',' + RealToString(ProcessDEMTile(AreaGrids[0],AreaGrids[i],Buffer,GridLimits),-12,6);
          //aLine := aline + ',' + IntToStr(NumRef);
          Results.Add(aline);
       end;


   procedure RunForAnArea(AreaName : shortString);
   var
      i,DEMIXtileDB : integer;
      aLine : shortstring;
      SaveFName,VD : PathStr;
   begin
       SavefName := Stream_valley_dir + AreaName + '_ridges_valleys.csv';
       if Overwrite and FileExists(saveFName) then exit;
       {$IfDef RecordDEMIX} WriteLineToDebugFile('Start ' + AreaName); {$EndIf}
       ZeroPointAndAreaGrids(PointGrids,AreaGrids);
       ZeroPointAndAreaGrids(PointGrids2,AreaGrids2);
       if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,true) then begin  //open the maps becasue we draw on them
          for i := dmxFirstArea to NumAreaDEMs do begin
             if (i <= 0) then VD := wbt_out_ref_dir else VD := wbt_out_test_dir;
             AreaGrids[i] := MakeGridFromVectors(VD,AreaDEMs[i],'valleys_');
             AreaGrids2[i] := MakeGridFromVectors(VD,AreaDEMs[i],'ridges_');
          end;
          for i := dmxFirstPoint to NumPtDEMs do begin
             if (i <= 0) then VD := wbt_out_ref_dir else VD := wbt_out_test_dir;
             PointGrids[i] := MakeGridFromVectors(VD,PointDEMs[i],'valleys_');
             PointGrids2[i] := MakeGridFromVectors(VD,PointDEMs[i],'ridges_');
          end;

          Results := tStringList.Create;
          aline := 'DEMIX_TILE,AREA,LAT,LONG,CRITERION';
          for i := 1 to NumPtDEMs do aLine := aline + ',' + SRTM_centroid_names[i];
          for i := 1 to NumAreaDEMs do aLine := aline + ',' + ALOS_centroid_names[i];
          Results.Add(aLine);

          DEMIXtileDB := DEMIXtileFill(PointDEMs[0],DEMGlb[PointDEMs[0]].DEMBoundBoxGeo,false);
          GISdb[DEMIXtileDB].ApplyGISFilter(FilterForDEMIXtilesToUse);
          GISdb[DEMIXtileDB].EmpSource.Enabled := false;

          GISdb[DEMIXtileDB].MyData.First;
          while not GISdb[DEMIXtileDB].MyData.eof do begin
             GridLimits := GetDEMgridLimitsFromBB(PointDEMs[0],DEMIXtileDB,TileName,bb);
             DoTolerance('VALLEYS_1',1,PointGrids,AreaGrids);
             DoTolerance('VALLEYS_3',3,PointGrids,AreaGrids);
             DoTolerance('RIDGES_1',1,PointGrids2,AreaGrids2);
             DoTolerance('RIDGES_3',3,PointGrids2,AreaGrids2);
             GISdb[DEMIXtileDB].MyData.Next;
          end;
          if (Results.Count > 1) then Results.SaveToFile(SavefName);
          Results.Destroy;
          {$IfDef RecordDEMIX} WriteLineToDebugFile('Done ' + AreaName); {$EndIf}
       end;
   end;

var
   AreasWanted : tStringList;
begin {DEMIX_CreateGridsFromVectors}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMIX_CreateGridsFromVectors in'); {$EndIf}
   try
      GetDEMIXpaths;
      AreasWanted := DEMIX_AreasWanted;
      LockStatusBar := true;
      SafeMakeDir(Stream_valley_dir);
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         wmdem.SetPanelText(1, 'Create channel grids: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + ' ' + AreaName,true);
         RunForAnArea(AreaName);
         CloseAllDEMs;
         CloseAllDataBases;
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMIX_CreateGridsFromVectors out'); {$EndIf}
end {DEMIX_CreateGridsFromVectors};



procedure CompareChannelNetworks(Overwrite : boolean; Area : shortstring);
var
   DEM,DEMIXtileDB,{NumRef,}i,j : integer;
   Results : tStringList;
   fName,SavefName : PathStr;
   aLine,TileName : shortstring;
   bb : sfBoundBox;
   MissingFiles : boolean;
   GridLimits : tGridLimits;



       procedure DoTolerance(Criterion : shortstring; Buffer : integer);
       var
          i : integer;
          aline : shortString;
       begin
          aLine := TileName + ',' + Area + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + Criterion;
          for i := 1 to NumPtDEMs do begin
             aLine := aline + ',' + RealToString(ProcessDEMTile(PointDEMs[dmxFirstPoint],PointDEMs[i],Buffer,GridLimits),-12,6);
          end;
          for i := 1 to NumAreaDEMs do aLine := aline + ',' + RealToString(ProcessDEMTile(AreaDEMs[0],AreaDEMs[i],Buffer,GridLimits),-12,6);
          //aLine := aline + ',' + IntToStr(NumRef);
          Results.Add(aline);
       end;


const
   OpenMaps = false;  //available for debugging
begin {procedure CompareChannelNetworks}
    {$IfDef RecordDEMIX} WriteLineToDebugFile('CompareChannelNetworks in'); {$EndIf}
    SafeMakeDir(ChannelMissesDir);
    SavefName := ChannelMissesDir + Area + '_Channel_Misses.csv';

    if Overwrite or (not FileExists(savefName)) then begin
       wmdem.SetPanelText(3, 'Load DEMs',true);
       if OpenBothPixelIsDEMs(Area,'streams_',wbt_out_ref_dir,wbt_out_test_dir,OpenMaps) then begin
          MissingFiles := false;
          //need reference DEM to get percentage full of the tiles
             fName := DEMIX_Ref_1sec + Area +'_dtm' + Ref1SecAreaStr + '.tif';
             if FileExists(fName) then DEM := OpenNewDEM(fName,OpenMaps)
             else MissingFiles := true;
       end
       else MissingFiles := true;
       if MissingFiles then begin
          {$IfDef RecordDEMIX} WriteLineToDebugFile('CompareChannelNetworks missing for area ' + Area); {$EndIf}
       end
       else begin
          Results := tStringList.Create;
          aline := 'DEMIX_TILE,AREA,LAT,LONG,CRITERION';
          for i := 1 to NumPtDEMs do aLine := aline + ',' + SRTM_centroid_names[i];
          for i := 1 to NumAreaDEMs do aLine := aline + ',' + ALOS_centroid_names[i];
          Results.Add(aLine);   // + ',VECTOR_PIX');

          DEMIXtileDB := DEMIXtileFill(DEM,DEMGlb[DEM].DEMBoundBoxGeo,true);
          GISdb[DEMIXtileDB].ApplyGISFilter(FilterForDEMIXtilesToUse);

          GISdb[DEMIXtileDB].MyData.First;
          GISdb[DEMIXtileDB].EmpSource.Enabled := false;
          while not GISdb[DEMIXtileDB].MyData.eof do begin
             GridLimits := GetDEMgridLimitsFromBB(DEM,DEMIXtileDB,TileName,bb);
             DoTolerance('CHAN_MISS1',0);
             DoTolerance('CHAN_MISS3',1);
             GISdb[DEMIXtileDB].MyData.Next;
          end;
          if (Results.Count > 1) then Results.SaveToFile(SavefName)
          else begin
             {$IfDef RecordDEMIX} WriteLineToDebugFile('CompareChannelNetworks difference fail for area ' + Area); {$EndIf}
          end;
          Results.Free;
          if OpenMaps then begin
             wmdem.tile;
             //MessageToContinue('Look at maps');
          end;
          CloseAllDEMs;
          CloseAllDataBases;
       end;
    end;
end {procedure CompareChannelNetworks};



procedure ChannelNetworkMissPercentages(Overwrite : boolean; AreasWanted : tstringlist = nil);
var
   AreaName : shortstring;
   i : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('ChannelNetworkMissPercentages in'); {$EndIf}
   try
      PickDEMIXMode;
      SetParamsForDEMIXmode;
      GetDEMIXPaths(true);
      if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         wmdem.SetPanelText(1, 'Channel misses: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + '  ' + AreaName,true);
         CompareChannelNetworks(Overwrite,AreaName);
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('ChannelNetworkMissPercentages out'); {$EndIf}
end;


procedure ReclassifyByteGrid(Grid : integer; Reclass : tBytes256);
var
   Col,Row,zi : integer;
   z : float32;
begin
   for Col := 0 to pred(DEMglb[Grid].DEMheader.NumCol) do begin
      for Row := 0 to pred(DEMglb[Grid].DEMheader.NumRow) do begin
         if DEMglb[Grid].GetElevMetersOnGrid(Col,Row,z) then begin
            zi := round(z);
            if zi in [1..255] then begin
                DEMglb[Grid].SetGridElevation(Col,Row,Reclass[zi]);
            end
            else begin
               MessageToContinue('Reclass out of range: ' + IntToStr(zi) + '  Original: ' + RealToString(z,-8,0));
            end;
         end;
      end;
   end;
   DEMglb[Grid].CheckMaxMinElev;
end;


procedure CompareGeomorphons(Overwrite : boolean; AreaName : shortstring; Cat : shortstring = 'GEOM');
const
   OpenMaps = false;  //available for debugging
   cName : array[1..4] of shortstring = ('KAPPA','OVACC','USERS','PROD');
var
   DEM,DEMIXtileDB,NumRef,i,j{,UsingRef} : integer;
   Results : tStringList;
   fName,SavefName : PathStr;
   aLine,TileName : shortstring;
   alines : array[1..4] of shortstring;
   bb : sfBoundBox;
   //MissingFiles,
   Continue : boolean;
   GridLimits : tGridLimits;
   IP12Table : tMyData;
   Reclass : tBytes256;


   procedure GeomorphDifferences(Ref,Test : integer);
   var
      Kappa,OverallAccuracy,AvgUsers,AvgProd,z1,z2 : float32;
      HTMLname : PathStr;
   begin
       if ValidDEM(Ref) and ValidDEM(Test) then begin
          ComputeKappa(Ref,Test,GridLimits, Kappa,OverallAccuracy,AvgUsers,AvgProd);
          alines[1] := alines[1] + ',' + RealToString(1 - Kappa,-12,-8);
          alines[2] := alines[2] + ',' + RealToString(1 - OverallAccuracy,-12,-8);
          alines[3] := alines[3] + ',' + RealToString(1 - AvgUsers,-12,-8);
          alines[4] := alines[4] + ',' + RealToString(1 - AvgProd,-12,-8);
       end;
   end;


begin {procedure CompareGeomorphons}
    {$IfDef RecordDEMIX} WriteLineToDebugFile('CompareGeomorphons in ' + AreaName); {$EndIf}
    SafeMakeDir(GeomorphonsDir);
    if (Cat = 'GEOM') then SavefName := GeomorphonsDir + AreaName + '_geomorphons.csv'
    else SavefName := GeomorphonsDir + AreaName + '_' + Cat + '.csv';
    if Overwrite or (not FileExists(savefName)) then begin
      wmdem.SetPanelText(3, 'Load DEMs',true);
      if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,false) then begin
         wmdem.SetPanelText(3,'Load + ' + Cat,true);
         if (Cat = 'GEOM') then begin
            WBT_CreateDEMIX_GeomorphonGrids(false);
            Continue := true;
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Geomorphons created'); {$EndIf}
         end
         else begin
            Continue := OpenGridsCreatedByExternalProgram(false,'SAGA',AreaName,Cat + '_',PointGrids,AreaGrids);
            if Continue then begin
               {$IfDef RecordDEMIX} WriteLineToDebugFile('Grids read'); {$EndIf}
               for i := 0 to 255 do Reclass[i] := 255;
               fName := ProgramRootDir + 'saga_iwahashi_colors_reclassify.dbf';
               IP12Table := tMyData.Create(fName);
               IP12Table.ApplyFilter('CATEGORIES=12');
               while not IP12Table.eof do begin
                  Reclass[IP12Table.GetFieldByNameAsInteger('CATEGORY')] := IP12Table.GetFieldByNameAsInteger('SIMPLIFY');
                  IP12Table.Next;
               end;
               IP12Table.Destroy;
               {$IfDef RecordDEMIX} WriteLineToDebugFile('Reclass read'); {$EndIf}

               for i := dmxFirstPoint to NumPtDEMs do ReclassifyByteGrid(PointGrids[i],Reclass);
               for i := dmxFirstArea to NumAreaDEMs do ReclassifyByteGrid(AreaGrids[i],Reclass);
               {$IfDef RecordDEMIX} WriteLineToDebugFile('Grids reclassified'); {$EndIf}
            end
            else begin
               {$IfDef RecordDEMIX} WriteLineToDebugFile('Failure in grids read'); {$EndIf}
            end;
         end;

         wmdem.SetPanelText(3,'Compute + ' + Cat,true);
         if Continue then begin
             Results := tStringList.Create;
             aline := 'DEMIX_TILE,AREA,LAT,LONG,CRITERION';
             for i := 1 to NumPtDEMs do aLine := aline + ',' + SRTM_centroid_names[i];
             for i := 1 to NumAreaDEMs do aLine := aline + ',' + ALOS_centroid_names[i];
             Results.Add(aLine);

             DEMIXtileDB := DEMIXtileFill(PointDEMs[0],DEMGlb[PointDEMs[0]].DEMBoundBoxGeo,false);
             GISdb[DEMIXtileDB].ApplyGISFilter(FilterForDEMIXtilesToUse);

             GISdb[DEMIXtileDB].MyData.First;
             while not GISdb[DEMIXtileDB].MyData.eof do begin
                bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
                TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
                wmdem.SetPanelText(2, TileName,true);

                aLine := TileName  + ',' + AreaName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',';
                for i := 1 to 4 do alines[i] := aline + Cat + '_' + CName[i];

                for i := 1 to NumPtDEMs do begin
                   //UsingRef := RefGridForThisPointGrid(PointGrids,i);
                   GridLimits := DEMglb[PointGrids[dmxFirstPoint]].sfBoundBox2tGridLimits(bb);
                   GeomorphDifferences(PointGrids[dmxFirstPoint],PointGrids[i])
                end;

                GridLimits := DEMglb[AreaGrids[0]].sfBoundBox2tGridLimits(bb);
                for i := 1 to NumAreaDEMs do begin
                   GeomorphDifferences(AreaGrids[0],AreaGrids[i])
                end;
                for i := 1 to 4 do Results.Add(alines[i]);

                GISdb[DEMIXtileDB].MyData.Next;
             end;
             if (Results.Count > 1) then Results.SaveToFile(SavefName)
             else begin
                {$IfDef RecordDEMIX} WriteLineToDebugFile('CompareGeomorphons difference fail for area ' + AreaName); {$EndIf}
             end;

             Results.Free;
          end;
          CloseAllDEMs;
          CloseAllDatabases;
          wmdem.SetPanelText(2,'',true);
       end;
    end;
end {procedure CompareGeomorphons};



procedure ClassificationAgreement(Overwrite : boolean; AreasWanted : tstringlist = nil);
var
   AreaName : shortstring;
   i : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GeomorphonsPercentages in'); {$EndIf}
   try
      PickDEMIXMode;
      SetParamsForDEMIXmode;
      GetDEMIXPaths(true);
      if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         wmdem.SetPanelText(1, 'Classification Percentages: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + '  ' + AreaName,true);
         CompareGeomorphons(Overwrite,AreaName);
         CompareGeomorphons(Overwrite,AreaName,'IP12');
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('GeomorphonsPercentages out'); {$EndIf}
end;



{$IfDef DEMIX_SAGA_channels}
      procedure ChannelNetworkMapComparison(Overwrite : boolean; AreaName,TestDEMName : shortstring);


      //should use bits 1-2-4-8-16-32 and then could compare multiple easily (except of course ALOS mode geometry)

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



procedure CreateChannelNetworkGridsFromVectors(Overwrite : boolean; AreasWanted : tstringlist = nil);
var
   AreaName : shortstring;
   i : integer;


      function ChannelSHPtoGrid(DEM,db : integer; fName : PathStr; PlotOrder : integer = 1) : integer;
      var
         grid : integer;
         Bitmap : tMyBitmap;

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
            DEMGlb[Grid].WriteNewFormatDEM(fName);
            GISdb[db].LayerIsOn := false;
            Result := Grid;
         end;


      begin
          if ValidDEM(DEM) and ValidDB(db) then begin
             NakedMapOptions;
             DoDataBase;
          end
          else Result := 0;
      end;


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
      if (TheFiles.Count > 0) then begin
         SafeMakeDir(OutFolder);
         for i := 0 to pred(TheFiles.Count) do begin
             wmdem.SetPanelText(2,What + ' ' + IntToStr(i) + '/' + IntToStr(TheFiles.Count),true);
             InName := TheFiles.Strings[i];
             dbName := InFolder + 'saga_channels_' + ExtractFileNameNoExt(InName) + '.shp';
             if FileExists(InName) and FileExists(dbName) then begin
                GridName := OutFolder + ExtractFileNameNoExt(dbName) + '.tif';
                if ((not FileExists(GridName)) or Overwrite) then begin
                   if (IsDEMaDSMorDTM(InName) = DEMisDSM) then begin
                      File2Trash(InName);
                   end
                   else begin
                      DEM := OpenNewDEM(InName,true);
                      db := DEMGlb[DEM].SelectionMap.OpenDBonMap('',dbName,true,false);
                      NewGrid := ChannelSHPtoGrid(DEM,db,GridName);
                      CloseSingleDEM(DEM);
                      CloseSingleDEM(NewGrid);
                      CleanUpTempDirectory(false);
                   end;
                end;
            end
            else begin
               {$IfDef RecordDEMIX} if not FileExists(dbName) then WriteLineToDebugFile('CreateChannelNetworkGridsFromVectors Missing=' + dbName); {$EndIf}
               {$IfDef RecordDEMIX} if not FileExists(InName) then WriteLineToDebugFile('CreateChannelNetworkGridsFromVectors Missing=' + InName); {$EndIf}
            end;
         end;
         TheFiles.Free;
      end
      else begin
         {$IfDef RecordDEMIX} if not FileExists(dbName) then WriteLineToDebugFile('CreateChannelNetworkGridsFromVectors .tif Missing for ' + BaseFolder); {$EndIf}
      end;
   end;

begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateChannelNetworkGridsFromVectors in'); {$EndIf}
   try
      GetDEMIXpaths;
      if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;
      LockStatusBar := true;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         wmdem.SetPanelText(1, 'Channel grids: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + ' ' + AreaName,true);
         Run(DEMIX_test_DEMs_no_sink,DEMIX_test_DEMs_channels,DEMIX_test_DEMs_channel_grids,'Test DEMs');
         Run(DEMIX_ref_DEMs_no_sink,DEMIX_ref_DEMs_channels,DEMIX_ref_DEMs_channel_grids,'Ref DEMs');
      end;
   finally
      EndDEMIXProcessing;
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
      if (TheFiles.Count = 0) then begin
         {$IfDef RecordDEMIX} HighlightLineToDebugFile('BatchCreateVectorChannelNewtwork no tif files for ' + AreaName + ' in ' + InFolder); {$EndIf}
      end
      else begin
         SafeMakeDir(OutFolder);
         NumDone := 0;
         for i := 0 to pred(TheFiles.Count) do begin
             wmdem.SetPanelText(2, 'DEM: ' + IntToStr(succ(i)) + '/' + IntToStr(TheFiles.Count) + ' ' + AreaName,true);
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
         {$IfDef RecordDEMIX} if (NumDone > 0) then WriteLineToDebugFile(What + ' shp file created=' + IntToStr(NumDone) + ' in ' + OutFolder + ' for ' + AreaName); {$EndIf}
      end;
   end;

var
   i : integer;
begin
   try
      GetDEMIXpaths;
      {$IfDef RecordDEMIX} WriteLineToDebugFile('BatchCreateVectorChannelNewtwork in'); {$EndIf}
      if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;
      LockStatusBar := true;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         wmdem.SetPanelText(1, 'Areas: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + ' ' + AreaName,true);
         Run(DEMIX_ref_DEMs_no_sink,DEMIX_ref_DEMs_channels,' ref DEMs');
         Run(DEMIX_test_dems_no_sink,DEMIX_test_DEMs_channels,' test DEMs');
      end;
   finally
      EndDEMIXProcessing;
   end;
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
            i,NumDone : integer;
         begin
            wmdem.SetPanelText(3, 'Remove sinks ' + What + ' ' + AreaName,true);
            TheFiles := nil;
            SafeMakeDir(OutFolder);
            Petmar.FindMatchingFiles(InFolder,Ext,TheFiles,1);
            RemoveFilesThatDoNotHaveString(TheFiles,AreaName);
            {$IfDef RecordDEMIXFull} WriteLineToDebugFile(What + ' file=' + IntToStr(TheFiles.Count)); {$EndIf}
            NumDone := 0;
            for i := 0 to pred(TheFiles.Count) do begin
                wmdem.SetPanelText(1, 'DEM: ' + IntToStr(succ(i)) + '/' + IntToStr(TheFiles.Count),true);
                InName := TheFiles.Strings[i];
                if (IsDEMaDSMorDTM(InName) = DEMisDTM) then begin
                   OutName := OutFolder + 'saga_sinks_removed_' + ExtractFileNameNoExt(InName) + '.tif';
                   if (not FileExists(OutName)) or Overwrite then begin
                      SagaSinkRemoval(InName,OutName);
                      inc(NumDone);
                   end;
                end;
            end;
            {$IfDef RecordDEMIX} if NumDone > 0 then WriteLineToDebugFile(AreaName + ' ' + what + ' files done=' + IntToStr(NumDone)); {$EndIf}
            TheFiles.Free;
            EndProgress;
         end;


begin
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('BatchRemoveSinksInDEMIX_DEMS Out'); {$EndIf}
   if (AreasWanted = Nil) then AreasWanted := DEMIX_AreasWanted;
   try
      GetDEMIXpaths;
      LockStatusBar := true;
      for i := 0 to pred(AreasWanted.Count) do begin
         AreaName := AreasWanted.Strings[i];
         wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(AreasWanted.Count) + ' ' + AreaName,true);
         {$IfDef RecordDEMIXFull} WriteLineToDebugFile('BatchRemoveSinksInDEMIX_DEMS start test DEMs'); {$EndIf}
         Run(DEMIX_test_dems,DEMIX_test_DEMs_no_sink,'*.tif',' test DEMs');
         Run(DEMIX_Ref_1sec,DEMIX_ref_DEMs_no_sink,'*.tif',' ref DEMs');
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$IfDef RecordDEMIX} HighLightLineToDebugFile('BatchRemoveSinksInDEMIX_DEMS Out'); {$EndIf}
end;


procedure MultistepChannelNetworks(Overwrite : boolean);
var
   AreasWanted : tStringList;
   AreaName : shortstring;
begin
   try
      GetDEMIXpaths;
      LockStatusBar := true;
      AreasWanted := DEMIX_AreasWanted;
      BatchRemoveSinksInDEMIX_DEMS(Overwrite,AreasWanted);
      BatchCreateVectorChannelNewtwork(Overwrite,AreasWanted);
      CreateChannelNetworkGridsFromVectors(Overwrite,AreasWanted);
      ChannelNetworkMissPercentages(Overwrite,AreasWanted);
   finally
      EndDEMIXProcessing;
   end;
end;



{$EndIf}


