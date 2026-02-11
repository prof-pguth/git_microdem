
var
   AreaDir,SourceDir,WGS_EGMdir,MergesDirFName : PathStr;
   AreaProgress : shortstring;
const
   udFUVCalc = 2;
   udPixelGeometrySlope = 4;
   udTileStats = 5;
   udFileFillReport = 31;
   udDeleteResultsCSVsForAreas = 32;
   udInvalidTiles = 33;
   udCSVReportByArea = 34;
   udClearAreaLocks = 35;
   udAverageTileElevations = 36;
   udSourceFileSurvey = 37;
   udCompareLandCover = 38;
   udAverageSlopeMap = 39;
   udLandcoverCats = 40;
   udTileBoundingBoxes = 41;
   udZipSourceFiles = 42;
   udTileCatBinMap = 43;
   udMergeDTMDSMcompare = 44;
   udMergeCopDTMDSMcompare = 45;

function AvailablePhysicalMemoryString : shortstring;
var
  vGlobalMemoryStatus : TMemoryStatus;
begin
  GlobalMemoryStatus( vGlobalMemoryStatus );
  Result := ' Available RAM: ' + SmartMemorySizeBytes(vGlobalMemoryStatus.dwAvailPhys);
end;

function SkipDEM(aDEM : shortstring) : boolean;
begin
   Result := (aDEM = 'GEDTMV0') or (aDEM = 'EDTM') or (aDEM = 'GEDTMV1_1');
end;

function WebExtractDEM(aDEm : shortstring) : boolean;
begin
   Result := (aDEM = 'GEDTMV0') or (aDEM = 'EDTM') or (aDEM = 'GEDTMV1_1') or (aDEM = 'GEDTMV1_2');
end;

function AreaWithMergingFiles(AreaName : shortstring) : boolean;
var
   CountryCode : shortstring;
begin
   CountryCode := UpperCase(copy(AreaName,1,3));
   Result := (CountryCode = 'CH_') or (CountryCode = 'FR_') or (CountryCode = 'NZ_');
end;


procedure AreaDirectoryNames(AreaName : shortstring);
begin
   AreaDir := MDDef.DEMIX_BaseDir + AreaName + '\';
   SourceDir := AreaDir + 'source\';
   WGS_EGMdir := AreaDir + 'wgs_egm\';
   MergesDirFName := AreaDir + 'merges\';
end;


function GetTileNameFromFileName(AreaName : shortstring; fName : PathStr) : shortstring;

         function NZ_thirds(value : integer) : ANSIchar;
         begin
            if Value in [1..17] then Result := 'a'
            else if Value in [18..34] then Result := 'b'
            else Result := 'c';
         end;

var
   Easting,Northing,CountryCode : shortstring;
   East,North : Integer;
begin
   if AreaWithMergingFiles(AreaName) then begin
       fName := ExtractFileNameNoExt(fName);
       CountryCode := copy(AreaName,1,3);
       if (CountryCode = 'ch_') then begin
          {
          swissalti3d_2023_2762-1154_2_2056_5728.tif
          }
          Easting := Copy(fName,18,3);
          Northing := Copy(fName,23,3);
          Result := AreaName + '_' + Easting + '-' + Northing;
       end;

       if (CountryCode = 'fr_') then begin
          {
          LHD_FXX_0419_6703_MNT_O_0M50_LAMB93_IGN69.tif
          }
          if Copy(fName,1,3) = 'LHD' then begin
             Easting := Copy(fName,9,3);
             Northing := Copy(fName,14,3);
             Result := AreaName + '_' + Easting + '-' + Northing;
          end
          else if Copy(fName,1,7) = 'RGEALTI' then begin
             Easting := Copy(fName,13,3);
             Northing := Copy(fName,18,3);
             Result := AreaName + '_' + Easting + '-' + Northing;
          end;
          {
          RGEALTI_MYT_0532_8588_MNT_RGM04UTM38S_MAYO53.asc
          RGEALTI_MTQ_0690_1638_MNT_WGS84UTM20_MART87.asc
          RGEALTI_MTQ_0690_1638_MNT_WGS84UTM20_MART87.asc
          }
       end;

       if (CountryCode = 'nz_') then begin //this is not yet running
          {
          DEM_BQ27_2025_1000_1701.tif
          //BQ27 is 1:50K map quad, which covers about 860 square km
          //tile names at the four corners of 50K quads
          //  CE11_1000_5050     CE12_1000_5001
          //  CF11_1000_0150     CF12_1000_0101
          }
          East := StrToInt(Copy(fName,22,2));
          North := StrToInt(Copy(fName,20,2));
          Result := AreaName + '_' + Nz_thirds(East) + '-' + Nz_thirds(North);
       end;
   end
   else Result := fName;
end;



function DEMIX_GetListOfTileSubDirsInArea(AreaName : shortstring; ShouldHaveBeenCreated : boolean = true) : tStringList;
//these have '_ref_test_dem' plus the tile name
var
  i : integer;
  TiffsThere : tStringList;
  TileDir : PathStr;
begin
   AreaDirectoryNames(AreaName);
   Result := GetSubDirsInDirectory(AreaDir);
   TiffsThere := tStringList.Create;
   for i := pred(Result.Count) downto 0 do begin
      if (UpperCase(Copy(Result.Strings[i],1,3)) = 'AA_') or  (UpperCase(Copy(Result.Strings[i],1,3)) = 'DB_') or (Result.Strings[i] = 'source') or (Result.Strings[i] = 'wgs_egm')  or (Result.Strings[i] = 'merges') then Result.Delete(i)
      else if ShouldHaveBeenCreated then begin
         //this is due to some data directories requiring admin permission to delete, and Windows won't allow that
         TileDir := AreaDir + Result.Strings[i];
         Petmar.FindMatchingFiles(TileDir,'*.tif',TiffsThere,1);
         if (TiffsThere.Count = 0) then Result.Delete(i);
      end;
   end;
   TiffsThere.Destroy;
end;


function DEMIX_TileNameFromSubDir(Tile : shortstring) : shortstring;
begin
   Result := StringReplace(Tile,'_ref_test_dem','',[rfReplaceAll, rfIgnoreCase]);
end;


function DEMIX_GetListOfTileNamesInArea(AreaName : shortstring; ShouldHaveBeenCreated : boolean = true) : tStringList;
var
  i : integer;
begin
   Result := DEMIX_GetListOfTileSubDirsInArea(AreaName,ShouldHaveBeenCreated);
   for i := pred(Result.Count) downto 0 do begin
      Result.Strings[i] := DEMIX_TileNameFromSubDir(Result.Strings[i]);
   end;
end;


procedure TileCharacterization;
//hard coded for the three versions of the DEMIX DB
const
   MaxPossBins = 9;     //in each direction
   DB2 = 2;
   DB3 = 3;
   DB4 = 4;
   World = 1;
   IncludeDBVersions = false;
var
   Bins : array[1..4,1..MaxPossBins,1..MaxPossBins] of int64;
   Filters1,Labels1,Lowers1,Uppers1,Filters2,Labels2,Lowers2,Uppers2 : tStringList;
   NumThere : array[1..4] of int32;
   Param1,Param2 : shortstring;
   i,j,k,db,PieSlices : integer;


   procedure UpdataBin(Sample : integer; Value1,Value2 : float32);
   var
      k : integer;
   begin
      for k := 0 to pred(Lowers1.Count) do begin
         if (Value1 >= StrToFloat(Lowers1.strings[k])) and (Value1 < StrToFloat(Uppers1.strings[k])) then i := succ(k);
      end;
      for k := 0 to pred(Lowers2.Count) do begin
         if (Value2 >= StrToFloat(Lowers2[k])) and (Value2 < StrToFloat(Uppers2[k])) then j := succ(k);
      end;
      inc(Bins[Sample,i,j]);
   end;


   procedure ProcessTable(Sample,db : integer);
   begin
     NumThere[Sample] := GISdb[DB].MyData.FiltRecsInDB;
     GISdb[DB].EmpSource.Enabled := false;
     GISdb[DB].MyData.First;
     while not GISdb[DB].MyData.eof do begin
        UpdataBin(Sample,GISdb[DB].MyData.GetFieldByNameAsFloat(Param1),GISdb[DB].MyData.GetFieldByNameAsFloat(Param2));
        GISdb[DB].MyData.Next;
     end;
   end;


   procedure ProcessGrid(Grid1,Grid2 : integer);
   var
      x,y,NewGrid : integer;
      z1,z2 : float32;
      fName : PathStr;
   begin
       fName := Param1 + '_' + Param2;
       NewGrid := DEMglb[Grid1].CloneAndOpenGridSetMissing(ByteDEM, fName, euIntCode);
       for x := 0 to pred(DEMglb[Grid1].DEMheader.NumCol) do begin
          for y := 0 to pred(DEMglb[Grid1].DEMheader.NumRow) do begin
             if DEMglb[Grid1].GetElevMetersOnGrid(x,y,z1) then begin
                if DEMglb[Grid2].GetElevMetersOnGrid(x,y,z2) then begin
                   inc(NumThere[World]);
                   UpdataBin(World,z1,z2);
                   DEMglb[NewGrid].SetGridElevation(x,y,i + 10 * j);
                end;
             end;
          end;
       end;
       DEMglb[NewGrid].CheckMaxMinElev;
       fName := MDtempDir + fName + '.tif';
       DEMglb[NewGrid].SaveAsGeotiff(fName);
   end;

var
   aLine : shortstring;
   lc,grid_slope,grid_lc, db_v2,db_v3,db_v4 : integer;
   fName,BottomLegendfName : PathStr;
   bmp : tMyBitmap;
   Findings,Colors,LegendEntries : tStringList;
begin {procedure TileCharacterization;}
   ImportLandParamFiltersLong('C:\microdem\demix_settings\filters_avg_slope_6_cat.dbf',Filters1,Labels1,Lowers1,Uppers1);

   if IncludeDBVersions then begin
       fName := 'J:\wine_contest\wine_contest_database\demix_database_diff_distrib_v2.dbf';
       OpenNumberedGISDataBase(db_v2,fName,true);
       GISdb[db_v2].ApplyGISFilter('CRITERION=' + QuotedStr('ELVD_RMSE') + ' AND LAND_TYPE=' + QuotedStr('ALL'));
       fName := 'J:\wine_contest\wine_contest_database\Geo_fuv_db_3.dbf';
       OpenNumberedGISDataBase(db_v3,fName,true);
       GISdb[db_v3].ApplyGISFilter('CRITERION=' + QuotedStr('ELEV'));
       fName := 'J:\wine_contest\wine_contest_database\UTM_diff_distrib_DB_10_02_25.dbf';
       OpenNumberedGISDataBase(db_v4,fName,true);
       GISdb[db_v4].ApplyGISFilter('CRITERION=' + QuotedStr('ELVD_RMSE'));
   end;

   fName := 'J:\wine_contest\wine_contest_database\average_tile_slope.tif';
   grid_slope := OpenNewDEM(fName,false);
   Param1 := 'AVG_SLOPE';

   for lc := 2 to 2 do begin
       NumThere[4] := 0;
        for k := 1 to 4 do begin
           for I := 1 to MaxPossBins do begin
              for j := 1 to MaxPossBins do begin
                 Bins[k,i,j] := 0;
              end;
           end;
        end;

       if (lc = 2)  then begin
          Param2 := 'BARREN_PC';
          fName := 'J:\wine_contest\wine_contest_database\barren_percent.tif';
          ImportLandParamFiltersLong('C:\microdem\demix_settings\filters_barren_pc.dbf',Filters2,Labels2,Lowers2,Uppers2);
       end
       else if (lc = 3)  then begin
          Param2 := 'FOREST_PC';
          fName := 'J:\wine_contest\wine_contest_database\forest_percent.tif';
          ImportLandParamFiltersLong('C:\microdem\demix_settings\filters_forest_pc.dbf',Filters2,Labels2,Lowers2,Uppers2);
       end
       else if (lc = 4)  then begin
          Param2 := 'URBAN_PC';
          fName := 'J:\wine_contest\wine_contest_database\urban_percent.tif';
          ImportLandParamFiltersLong('C:\microdem\demix_settings\filters_urban_pc.dbf',Filters2,Labels2,Lowers2,Uppers2);
       end;

       grid_lc := OpenNewDEM(fName,false);

       if IncludeDBVersions then begin
           ProcessTable(DB2,db_v2);
           ProcessTable(DB3,db_v3);
           ProcessTable(DB4,db_v4);
       end;

       ProcessGrid(Grid_slope,Grid_lc);

       Findings := tStringList.Create;
       if IncludeDBVersions then begin
          Findings.Add('Label1,Label2,FILTER1,FILTER2,X,Y,DB_V2_N,DB_V3_N,DB_v4_N,WORLD_N,DB_V2_PC,DB_V3_PC,DB_v4_PC,WORLD_PC');
       end
       else begin
          Findings.Add('Label1,Label2,FILTER1,FILTER2,X,Y,WORLD_N,WORLD_PC');
       end;

       if IncludeDBVersions then PieSlices := 4 else PieSlices := 1;

       for I := 0 to pred(Labels1.Count) do begin
          for j := 0 to pred(Labels2.Count)  do begin
             aline := Labels1[i] + ',' + Labels2[j] + ',' + Filters1[i] + ',' + Filters2[j] + ',' + IntToStr(succ(i)) +  ',' + IntToStr(succ(j));
             for k := 1 to PieSlices do begin
                 Aline := aLine + ',' + RealToString(Bins[k,succ(i),succ(j)],-12,-4);
              end;
             for k := 1 to PieSlices do begin
                 Aline := aLine + ',' + RealToString(100 * Bins[k,succ(i),succ(j)] / NumThere[k],-12,-4);
              end;
              Findings.Add(aline);
           end;
        end;

        Colors := tStringList.Create;
        Colors.Add(IntToStr(clBlue));
        Colors.Add(IntToStr(clRed));
        Colors.Add(IntToStr(clMaroon));
        Colors.Add(IntToStr(clLime));

        LegendEntries := tStringList.Create;
        LegendEntries.Add('World land area (n=~1.5 million)');
        if IncludeDBVersions then begin
           LegendEntries.Add('DEMIX DB v2 (n=216)');
           LegendEntries.Add('DEMIX DB v3 (n=3462)');
           LegendEntries.Add('DEMIX DB v4 (n=1510)');
        end;

        bmp := MakeLegend(LegendEntries,Colors);
        BottomLegendFName := MDTempDir + 'tile_characteristics.bmp';
        SaveBitmap(bmp, BottomLegendFName);

        fName := MDTempDir + Param2 + 'Tile_characteristics.dbf';
        db := PetDBUtils.StringList2CSVtoDB(Findings,fName);
        if IncludeDBVersions then begin
            DEMIX_graphs.TileCharacteristicsPieGraph(db,Labels1,Labels2,1,true,BottomLegendFName);
            DEMIX_graphs.TileCharacteristicsPieGraph(db,Labels1,Labels2,2.5,true,BottomLegendFName);
            DEMIX_graphs.TileCharacteristicsPieGraph(db,Labels1,Labels2,25,false,BottomLegendFName);
        end
        else begin
            DEMIX_graphs.TileCharacteristicsPieGraph(db,Labels1,Labels2,round(GISdb[db].MyData.FindFieldMax('WORLD_PC')),false,BottomLegendFName);
        end;
        CloseSingleDEM(grid_lc);
   end;
end {procedure TileCharacterization};



procedure RemovePartialTilesBefore10kMergesDownload;
//takes a text file with DEMs that will be downloaded, and removes those that will in a partially full 10x10 km tile
//intended for French data, with very slow downloads
var
   ListOfFiles,ThisTileFiles,FullTilesFiles : tStringList;
   fName : PathStr;
   TileName,{Download,}CheckTileName,AreaName : ANSIstring;
   //i,
   j,LastFile,FilesNeeded : integer;


   function GetTileNameForThisTile(i : integer) : shortstring;
   begin
      Result := Petmar_types.AfterSpecifiedStringANSI(ListOfFiles.Strings[i],'FILENAME=');
      Result := GetTileNameFromFileName(AreaName,Result);
   end;

begin
   if Petmar.GetFileFromDirectory('list of files','*.txt',fname) then begin
      AreaName := ExtractFileNameNoExt(fName);
      ListOfFiles := tStringList.Create;
      ThisTileFiles := tStringList.Create;
      FullTilesFiles := tStringList.Create;
      ListOfFiles.LoadFromFile(fName);
      HighLightLineToDebugFile('At start files=' + IntToStr(ListOfFiles.Count));
      FilesNeeded := 90;
      ReadDefault('Files needed in tile',FilesNeeded);
      repeat
         LastFile := pred(ListOfFiles.Count);
         TileName := GetTileNameForThisTile(LastFile);
         for j := LastFile downto 0 do begin
           CheckTileName := GetTileNameForThisTile(j);
           if TileName = CheckTileName then begin
              ThisTileFiles.Add(ListOfFiles.Strings[j]);
           end;
         end;
         for j := LastFile downto 0 do begin
           CheckTileName := GetTileNameForThisTile(j);
           if (TileName = CheckTileName) then begin
              ListOfFiles.Delete(j);
           end;
         end;
         if (ThisTileFiles.Count >= FilesNeeded) then begin
            for j := 0 to pred(ThisTileFiles.Count) do begin
               FullTilesFiles.Add(ThisTileFiles.Strings[j]);
            end;
         end;
         WriteLineToDebugFile(TileName + '  files=' + IntToStr(ThisTileFiles.Count));
         ThisTileFiles.Clear;
      until (ListOfFiles.Count = 0);
      ListOfFiles.Clear;
      for j := 0 to pred(FullTilesFiles.Count) do begin
          ListOfFiles.Add(FullTilesFiles.Strings[j]);
      end;
      ListOfFiles.SaveToFile(fName);
      HighLightLineToDebugFile('Final files=' + IntToStr(ListOfFiles.Count));
      ListOfFiles.Destroy;
      ThisTileFiles.Destroy;
      FullTilesFiles.Destroy;
   end;
end;



procedure InventoryFilesIn10kMerges(OptionToDelete : boolean = false);
var
   thePath,fName,{TilePath,SourceDir,}TestDir : PathStr;
   theFiles,TheResults,Areas,TileList,TheTests : tStringList;
   DeleteBelow{,NumTest} : integer;

   procedure RunThrough(DeleteBelow : integer);
   var
      CountryCode, AreaName,wgs_egm : shortstring;
      j,k : integer;
   begin
      TheResults := tStringList.Create;
      TheResults.Add('AREA,DEMIX_TILE,MERGE_FILES,WGS_EGM,REF_TEST');
       for j := 0 to pred(Areas.Count) do begin
           AreaName := Areas.Strings[j];
           {$IfDef RecordDEMIX} WriteLineToDebugFile(AreaName); {$EndIf};
           AreaDirectoryNames(AreaName);
           CountryCode := copy(AreaName,1,3);
           if AreaWithMergingFiles(AreaName) then begin
              TileList := tStringList.Create;
              TileList := GetSubDirsInDirectory(MergesDirFName);
              {$IfDef RecordDEMIX} WriteLineToDebugFile('Tiles=' + IntToStr(TileList.Count) + ' in ' + MergesDirFName); {$EndIf};
              if (TileList.Count > 0) then begin
                  for k := 0 to pred(TileList.Count) do begin
                     thePath := MergesDirFName + TileList.Strings[k] + '\';
                     {$IfDef RecordDEMIX} WriteLineToDebugFile(thePath); {$EndIf};
                     if ValidPath(thePath) then begin
                         TheFiles := tStringList.Create;
                         Petmar.FindMatchingFiles(ThePath,'*.tif',TheFiles,2);
                         if (TheFiles.Count = 0) then Petmar.FindMatchingFiles(ThePath,'*.asc',TheFiles,2);
                         {$IfDef RecordDEMIX} WriteLineToDebugFile('Files=' + IntToStr(TheFiles.Count) + ' in ' + thePath); {$EndIf};
                         if OptionToDelete and (TheFiles.Count < DeleteBelow) then begin
                            //thePath := MDDef.DEMIX_BaseDir + AreaName + TileList.Strings[k];
                            System.IOUtils.TDirectory.Delete(ThePath + '\',true);
                            //System.IOUtils.TDirectory.Delete(ThePath + '\',false);
                            //System.IOUtils.TDirectory.Delete(ThePath,false);
                         end
                         else begin
                            fName := wgs_egmDir + TileList.Strings[k] + '.tif';
                            if FileExists(fName) then wgs_egm := 'Yes' else wgs_egm := 'NO';
                            TestDir := AreaDir + TileList.Strings[k] + '_ref_test_dem\';
                            TheTests := tStringList.Create;
                            if ValidPath(TestDir) then begin
                               Petmar.FindMatchingFiles(TestDir,'*.tif',TheTests,1);
                            end;
                            TheResults.Add(AreaName + ',' + TileList.Strings[k] + ',' + IntToStr(TheFiles.Count) + ',' + wgs_egm + ',' + IntToStr(TheTests.Count));
                            TheTests.Destroy;
                         end;
                         TheFiles.Destroy;
                     end;
                  end {for k};
              end {if (TileList.Count > 0)};
              TileList.Destroy;
           end {if CountryCod};
        end {for j};
        fName := NextFileNumber(MDtempDir,'MergeFileCount','.dbf');
        PetDBUtils.StringList2CSVtoDB(TheResults,fName);
   end;


begin
   GetDOSPath('Base data directory',MDDef.DEMIX_BaseDir);
   {$IfDef RecordDEMIX} WriteLineToDebugFile('InventoryFilesIn10kMerges in ' + MDDef.DEMIX_BaseDir); {$EndIf};
   Areas := DEMIX_GetListOfAreas;
   if Areas.Count > 0 then begin
       {$IfDef RecordDEMIX} WriteLineToDebugFile('Areas=' + IntToStr(Areas.Count)); {$EndIf};
       RunThrough(0);
       if OptionToDelete then begin
          DeleteBelow := 50;
          ReadDefault('Delete tiles with fewer files to merge',DeleteBelow);
          if DeleteBelow > 0 then RunThrough(DeleteBelow);
       end;
   end;
   Areas.Destroy;
end;


procedure MoveSmallTilesInto10KTiles;
//tiles must be in area folder
//must be from a country that is covered
//moves files into folders in the merges directory
var
   thePath,fName,TilePath : PathStr;
   theFiles : tStringList;
   AreaName : shortstring;
   i : Integer;
begin
   repeat
       if not GetDosPath('Area with small tiles to put into 10x10 tile files',ThePath) then exit;
       ShowHourglassCursor;
       AreaName := LastSubDir(ThePath);
       if AreaWithMergingFiles(AreaName) then begin
         TheFiles := tStringList.Create;
         Petmar.FindMatchingFiles(ThePath,'*.tif',TheFiles,1);
         if (TheFiles.Count = 0) then Petmar.FindMatchingFiles(ThePath,'*.asc',TheFiles,1);
         if (TheFiles.Count > 0) then begin
           StartProgress('Segregate 10x10 tiles');
           MergesDirFName := ThePath + 'merges\';
           SafeMakeDir(MergesDirFName);
           for i := 0 to pred(TheFiles.Count) do begin
              if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);
              fName := theFiles.Strings[i];
              TilePath := MergesDirFName + GetTileNameFromFileName(AreaName,fName) + '\';
              SafeMakeDir(TilePath);
              Petmar.MoveFile(fName,TilePath + ExtractFileName(fName));
           end;
           EndProgress;
         end;
         TheFiles.Destroy;
       end
       else begin
          MessageToContinue('Not implemented yet for country with ' + AreaName);
       end;
       ShowDefaultCursor;
   until not AnswerIsYes('Another area');
end {procedure MoveSmallTilesInto10KTiles};



procedure GetNamesForMode(FUVMode : integer; AreaName : shortstring; var ResultsDir,ResultsfName : PathStr);
begin
   case FUVmode of
      fuvmMixed :    ResultsDir := MDDef.DEMIX_BaseDir + 'aa_fuv_results\';
      fuvmPartials : ResultsDir := MDDef.DEMIX_BaseDir + 'aa_partials_results\';
      fuvmCurves :   ResultsDir := MDDef.DEMIX_BaseDir + 'aa_curvatures_results\';
      fuvmDiffDist : ResultsDir := MDDef.DEMIX_BaseDir + 'aa_diff_dist_results\';
   end;
   case FUVmode of
      fuvmMixed :    ResultsfName := ResultsDir + AreaName + '_fuv_results.csv';
      fuvmPartials : ResultsfName := ResultsDir + AreaName + '_fuv_partials.csv';
      fuvmCurves :   ResultsfName := ResultsDir + AreaName + '_fuv_curvatures.csv';
      fuvmDiffDist : ResultsfName := ResultsDir + AreaName + '_diff_dist.csv';
   end;
end;


function GetFUVresultsSpreadAcrossHardDrives(subdir : shortstring) : tStringList;
var
   i,j : integer;
   thefiles : tStringList;
begin
    Result := tStringList.Create;
    for I := 1 to 3 do begin
       if i=1 then MDDef.DEMIX_BaseDir := 'f:\demix_utm_tiles\';
       if i=2 then MDDef.DEMIX_BaseDir := 'g:\demix_utm_tiles\';
       if i=3 then MDDef.DEMIX_BaseDir := 'h:\demix_utm_tiles\';
       if ValidPath(MDDef.DEMIX_BaseDir) then begin
          theFiles := nil;
          Petmar.FindMatchingFiles(MDDef.DEMIX_BaseDir + subdir + '\','*.csv',TheFiles,2);
          if (TheFiles.Count > 0) then for j := 0 to pred(theFiles.Count) do Result.Add(theFiles.Strings[j]);
          theFiles.Destroy;
       end;
    end;
end;


procedure PrepList10x10kmTiles;
//source code must be changed for every data set
//this prepares a list of tiles from the mapping agency that will make up a 10x10 km tile
//baseline is the name of one of the small tiles that is within the 10x10 km area desired
//listname is the name for the tile, which must start with NN_, with NN the country code
//for Germany, follow NN_ with a code for the province
//Pos1 and Pos2 are the character postions to change to move the tile by 1 km
//after creation, download all files and place them in a folder named ListName
var
   BaseLink : shortstring;
   Pos1,Pos2 : integer;
   ListName : PathStr;

         procedure DoOne;
         var
            i,j : AnsiChar;
            sl : tStringList;
         begin
            sl := tStringList.Create;
            for i := '0' to '9' do begin
               for j := '0' to '9' do begin
                  BaseLink[Pos1] := i;
                  BaseLink[Pos2] := j;
                  sl.add(BaseLink);
               end;
            end;
            SafeMakeDir('c:\temp\');
            sl.SaveToFile('c:\temp\' + ListName);
            sl.Destroy;
         end;

         procedure RhinelandPalatinate;
         begin
            { Rhineland-Palatinate
            https://geobasis-rlp.de/data/dgm1/current/tif/dgm01_32_323_5516_1_rp.tif'}
            // 32 is UTM zone
            //323nnn is easting
            //5516nnn is northing
               BaseLink := 'https://geobasis-rlp.de/data/dgm1/current/tif/dgm01_32_323_5516_1_rp.tif';
               ListName := 'de_rh_dgm01_32_323_5516.txt';
               Pos1 := 58;
               Pos2 := 63;
         end;

         procedure Saxony;
         begin
            {Saxony
            https://geocloud.landesvermessung.sachsen.de/index.php/s/9JHEbMApfuHxkiF/download?path=%2F&files=dgm1_33321_5598_1_sn_tiff.zip       }
               //33440 is zone 33, 440nnn easting
               //5633 is 5630nnn northing
               //BaseLink := 'https://geocloud.landesvermessung.sachsen.de/index.php/s/9JHEbMApfuHxkiF/download?path=%2F&files=dgm1_33440_5633_1_sn_tiff.zip';
            { older download location
            https://geocloud.landesvermessung.sachsen.de/index.php/s/9JHEbMApfuHxkiF/download?path=%2F&files=dgm1_33440_5633_1_sn_tiff.zip}
               //Pos1 := 107;
               //Pos2 := 112;

            {
            https://geocloud.landesvermessung.sachsen.de/public.php/dav/files/fMpooL3MQMsZdTk/dgm1_33402_5655_1_sn_tiff.zip}
               //BaseLink := 'https://geocloud.landesvermessung.sachsen.de/public.php/dav/files/fMpooL3MQMsZdTk/dgm1_33402_5655_1_sn_tiff.zip';
               BaseLink := 'https://geocloud.landesvermessung.sachsen.de/public.php/dav/files/fMpooL3MQMsZdTk/dgm1_33327_5694_1_sn_tiff.zip';
               ListName := 'de_sx_dgm1_3332_569.txt';
               Pos1 := 92;
               Pos2 := 97;
         end;

         procedure Bavaria;
         begin
        {Bavaria
        https://download1.bayernwolke.de/a/dgm/dgm1/654_5260.tif  }
        //654 is 654nnn easting
        //5260 is 5260nnn northing
           //BaseLink := 'https://download1.bayernwolke.de/a/dgm/dgm1/654_5260.tif';
           BaseLink := 'https://download1.bayernwolke.de/a/dgm/dgm1/693_5334.tif';
           ListName := 'de_bv_dgm1_693_5334.txt';
           Pos1 := 47;
           Pos2 := 52;
         end;

begin
   //Saxony;
   //Bavaria;
   RhinelandPalatinate;
   DoOne;
end {procedure PrepList10x10kmTiles};




 procedure MakeTestDEM(aDEM: shortstring; var TestDEM : integer; SourceEGMName,TestName : PathStr);


        function DatumShiftAlos(DEM : integer; aDEM : shortstring; gName : PathStr) : integer;
        var
           s_SRSstring,t_srsstring : shortstring;
           tName: PathStr;
        begin
           if (aDEM = 'ALOS') and (DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey <> VertCSEGM2008) then begin
               tName := MDtempDir + aDEM + '.tif';
               DEMglb[DEM].SaveAsGeotiff(tName);
               CloseSingleDEM(DEM);
               s_SRSstring := ' -s_srs EPSG:4326+' + IntToStr(VertCSEGM96);
               t_srsstring := ' -t_srs EPSG:4326+' + IntToStr(VertCSEGM2008);
               CompositeDatumShiftWithGDAL(tName,gName,s_SRSstring,t_srsstring);
               LoadNewDEM(Result,gName,false);
               DEMglb[Result].SaveAsGeotiff(gName);
               {$IfDef TrackVerticalDatum} WriteLineToDebugFile('DatumShiftALOS ' + aDEM + ' vdatum=' + IntToStr(DEMglb[Result].DEMheader.VerticalCSTypeGeoKey)); {$EndIf}
          end
          else Result := DEM;
        end;



 var
    {$If Defined(TimeMakeMaps)} Stopwatch : TStopwatch; {$EndIf}
    bb : sfBoundBox;
 begin
    if FileExists(SourceEGMName) then begin
        {$IfDef RecordDEMIXMakeTestFull} WriteLineToDebugFile('Create test DEM ' + TestName); {$EndIf};
        wmDEM.SetPanelText(0,TimeToStr(Now),true);
        GeotiffBoundingBoxGeo(SourceEGMName,bb);

        {$If Defined(TimeMakeMaps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
        if WebExtractDEM(aDEM) then begin
            {$IfDef RecordDEMIXMakeTest} WriteLineToDebugFile('Web extract ' + TestName); {$EndIf};
            TestDEM := WebExtractGEDTMorEDTM(aDEM,bb,TestName,false);
        end
        else begin
           {$IfDef RecordDEMIXMakeTest} WriteLineToDebugFile('Map library ' + TestName); {$EndIf};
           TestDEM := LoadMapLibraryBox(true,bb,GetTestDEMLongName(aDEM),false);
           if ValidDEM(TestDEM) then begin
               if (aDEM = 'ALOS') and (DEMGlb[TestDEM].DEMHeader.VerticalCSTypeGeoKey <> VertCSEGM2008) then begin
                  TestDEM := DatumShiftAlos(TestDEM,aDEM,TestName);
               end;
               DEMGlb[TestDEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
               DEMGlb[TestDEM].SaveGridSubsetGeotiff(DEMGlb[TestDEM].SpecifyDEMGridLimitsFromGeoBoundBox(bb),TestName);
           end;
           wmDEM.SetPanelText(1,AreaProgress,true);  //since library load might overwrite
        end;
        {$If Defined(TimeMakeMaps)} WriteLineToDebugFile('Make test DEM ' + TestName + '   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
        {$IfDef RecordDEMIXMakeTest} if not ValidDEM(TestDEM) then WriteLineToDebugFile('Failure to create ' + TestName); {$EndIf};
    end
    else begin
        {$IfDef RecordDEMIXMakeTest} WriteLineToDebugFile('No valid source DEM for ' + TestName); {$EndIf};
    end;
 end {procedure MakeTestDEM};


procedure MakeReferenceDEM(aDEM : shortstring; var TestDEM,RefDEM,SourceDEM : integer; TestName,RefName,TestDir,SourceEGMName : PathStr; DEMProgress : shortstring = '');
var
   SpacingArcSec : float32;
   NewHeader : tDEMHeader;
   Mode,SecondMode : shortstring;
   {$If Defined(TimeMakeMaps)} Stopwatch : TStopwatch; {$EndIf}

   function InsureTestDEMOpen : boolean;
   begin
      if not ValidDEM(TestDEM) then begin
         if FileExists(TestName) then begin
            {$IfDef RecordDEMIXMakeRef} WriteLineToDebugFile('Need to load test DEM ' + TestName); {$EndIf};
            LoadNewDEM(TestDEM,TestName,false);
            {$IfDef RecordDEMIXMakeRef} if not ValidDEM(TestDEM) then WriteLineToDebugFile('Ref DEM creation, problem loading ' + TestName); {$EndIf};
         end
         else begin
            {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Ref DEM creation, missing' + TestName); {$EndIf};
         end;
      end;
      Result := ValidDEM(TestDEM);
   end;

    procedure MakeThisReferenceDEM;
    var
       NPts : int64;
    begin
        if (not FileExists(RefName)) and ValidDEM(SourceDEM) and ValidDEM(TestDEM) then begin
            {$IfDef RecordDEMIXMakeRef} WriteLineToDebugFile('Source ' + DEMglb[SourceDEM].AreaName + '  ' + DEMglb[SourceDEM].zRange); {$EndIf};
            RefDEM := DEMGlb[TestDEM].ResaveNewElevationPrecision(fcSaveFloatingPoint);
            if ValidDEM(RefDEM) then begin
               DEMglb[RefDEM].MarkAboveMissing(-9999,NPts,false);            //entire DEM missing to start with
               DEMGlb[SourceDEM].ResampleByAveraging(false,RefName,RefDEM);  //mean aggregate new reference DEM
               {$IfDef RecordDEMIXMakeRef} WriteLineToDebugFile('Source ' + DEMglb[RefDEM].AreaName + '  ' + DEMglb[RefDEM].zRange); {$EndIf};
            end
            else begin
               {$IfDef RecordDEMIXMakeRef} WriteLineToDebugFile('Missing ref DEM for' + RefName); {$EndIf};
            end;
        end;
    end;

begin  {procedure MakeReferenceDEM}
   if (aDEM = 'COP') or (aDEM= 'ALOS') or (aDEM = 'FATHOM') {or (aDEM = 'GEDTMV1_1')} then begin
      SecondMode := '';
      if (aDEM = 'FATHOM') then Mode := 'srtm'
      else if (aDEM = 'COP') then begin
         Mode := 'cop';
         if not InsureTestDEMOpen then exit;
         SpacingArcSec := DEMGlb[TestDEM].DEMHeader.DEMxSpacing * 3600;
         if (SpacingArcSec < 1.01) then exit;
      end
      else if (aDEM= 'ALOS') then begin
         Mode := 'alos';
         if not InsureTestDEMOpen then exit;
         SpacingArcSec := DEMGlb[TestDEM].DEMHeader.DEMxSpacing * 3600;
         if (SpacingArcSec > 1.5) then SecondMode := 'alos1';
      end
      else begin
         //if (aDEM = 'GEDTMV1_1') then mode := 'gedtm';
      end;

      RefName := TestDir + 'ref_dtm_' + Mode + '.tif';
      if (not FileExists(RefName)) then begin
          {$If Defined(TimeMakeMaps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
          if not InsureTestDEMOpen then exit;
          if not ValidDEM(SourceDEM) then begin
             if FileExists(SourceEGMName) then begin
                {$IfDef RecordDEMIXMakeRefFull} WriteLineToDebugFile('Need to load source DEM ' + SourceEGMName); {$EndIf};
                LoadNewDEM(SourceDEM,SourceEGMName,false);
             end;
          end;
          if ValidDEM(SourceDEM) and ValidDEM(TestDEM) then begin
              wmDEM.SetPanelText(3,DEMProgress + ' Ref DEM ' + Mode,true);
              {$IfDef RecordDEMIXMakeRef} WriteLineToDebugFile('Create ref DEM ' + RefName); {$EndIf};
              SpacingArcSec := DEMGlb[TestDEM].DEMHeader.DEMxSpacing * 3600;
              if (SecondMode <> '') then begin
                  //first make high latitude version
                 RefName := TestDir + 'ref_dtm_' + Mode + '.tif';
                 MakeThisReferenceDEM;
                 NewHeader := DEMglb[TestDEM].DEMheader;
                 CloseSingleDEM(TestDEM);
                 //now make 1 sec low latitude, for the edit DTMs that don't change the x spacing
                 NewHeader.NumCol := 2 * NewHeader.NumCol;
                 NewHeader.DEMxSpacing := 0.5 * NewHeader.DEMxSpacing;
                 RefName := TestDir + 'ref_dtm_' + SecondMode + '.tif';
                 OpenAndZeroNewDEM(true,NewHeader,TestDEM,'ALOS_geometry_1sec',InitDEMmissing);
                 MakeThisReferenceDEM;
              end
              else begin
                 MakeThisReferenceDEM;
              end;
          end
          else begin
             {$IfDef RecordDEMIXMakeRef} if not ValidDEM(SourceDEM) then  WriteLineToDebugFile('Ref DEM creation, missing' + SourceEGMName); {$EndIf};
             {$IfDef RecordDEMIXMakeRef} if not ValidDEM(TestDEM) then  WriteLineToDebugFile('Ref DEM creation, missing' + TestName); {$EndIf};
          end;
         {$If Defined(TimeMakeMaps)} WriteLineToDebugFile('Make ref DEM ' + RefName + '   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
      end;
   end;
   wmDEM.SetPanelText(3,'',true);
end {procedure MakeReferenceDEM};


procedure UTMbasedtilescreation(AreaName : shortstring; MissingLandCover : tStringList);
var
   TestDir,SourceEGMName,fName,TestName,LCname,RefName : PathStr;
   UTM_Tile,aDEM,TStr,DEMProgress : shortstring;
   TheFiles,TheDEMs,TileSubDirs : tStringList;

   i,j,SourceDEM,TestDEM,RefDEM,lc,TilesInArea : integer;
   Lat,Long : float64;
   bb : sfBoundBox;


   procedure MoveDEMstoSourceDir;
   var
      i : integer;
      fName : PathStr;
      TileSubDirs : tStringList;
   begin
       TileSubDirs := DEMIX_GetListOfTileSubDirsInArea(AreaName);
       if (TileSubDirs.Count = 0) then begin
           {$IfDef RecordDEMIX} WriteLineToDebugFile('Start file movement for ' + AreaName); {$EndIf};
           if not(ValidPath(SourceDir)) then begin
              //to make it easier to download data, it does not have to be manually put into SourceDir, just into the AreaDir
              //only works if the files are close to the 10x10 km target size for tiles
              AreaDirectoryNames(AreaName);

              theFiles := TstringList.Create;
              Petmar.FindMatchingFiles(AreaDir,'*.tif',TheFiles,1);
              if (theFiles.Count > 0) then begin
                 SafeMakeDir(SourceDir);
                 for i := 0 to pred(TheFiles.Count) do begin
                    fName := theFiles.Strings[i];
                    if StrUtils.AnsiContainsText(fName,'(1).tif') then begin
                       DeleteFileIfExists(fName);
                    end
                    else begin
                       Petmar.MoveFile(fName,SourceDir + ExtractFileName(theFiles.Strings[i]));
                    end;
                 end;
              end;
              TheFiles.Destroy;
              TileSubDirs.Destroy;
           end;

           TheFiles := nil;
           Petmar.FindMatchingFiles(AreaDir + 'source\','*.tif',TheFiles,1);
           if (theFiles.Count = 0) then begin
              MessageToContinue('UTMbasedtilescreation, No TIF files in ' + AreaDir + 'source\');
              exit;
           end;
           TheFiles.Destroy;
       end
       else TileSubDirs.Destroy;
   end;


   procedure MakeLandCoverGridForTile;
   var
      i : integer;
      TileSubDirs : tStringList;
   begin
       TileSubDirs := DEMIX_GetListOfTileSubDirsInArea(AreaName);
       for i := 0 to pred(TileSubDirs.Count) do begin
          TestDir := MDDef.DEMIX_BaseDir + AreaName + '\' + TileSubDirs.Strings[i] + '\';
          LCname := TestDir + 'landcover.tif';
          if not FileExists(LCname) then begin
             wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(TileSubDirs.Count),true);
             fName := TestDir + 'cop.tif';
             if FileExists(fName) then begin
                 wmDEM.SetPanelText(3,'Land cover ' + TileSubDirs.Strings[i],true);
                 LoadNewDEM(SourceDEM,fName,false);
                 if ValidDEM(SourceDEM) then begin
                     bb := DEMglb[SourceDEM].DEMBoundBoxGeo;
                     lc := LoadLC10LandCover(LCname,bb,false,true);
                     if ValidDEM(lc) then begin
                        CloseSingleDEM(lc);
                     end
                     else begin
                        DEMglb[SourceDEM].DEMCenterPoint(Lat,Long);
                        TStr := 'Land cover not available for ' + AreaName + ' ' + LatLongDegreeToString(Lat,Long) + '  ' + GetLC10_fileName(Lat,Long);
                        {$IfDef RecordDEMIX} HighLightLineToDebugFile(TStr); {$EndIf};
                        MissingLandCover.Add(TStr);
                     end;
                    {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Finished land cover: ' + UTM_Tile); {$EndIf};
                 end;
             end
             else begin
                {$IfDef RecordDEMIXFull} WriteLineToDebugFile('COP DEM missing: ' + UTM_Tile); {$EndIf};
             end;
          end;
       end;
       TileSubDirs.Destroy;
   end {procedure MakeLandCoverGridForTile};

      procedure MergeTo10x10andPerformDatumShift(AreaName : shortstring; TilesInArea : integer; TheFiles : tStringList);
      var
         i : integer;
         UTM_Tile,VertEPSG,HorizEPSG,UTMzone,ProjName : shortstring;
         ThisTileFiles : tStringList;
         MergefName,OutVRT,TestDir,SourceEGMName,fName,MergeDir : PathStr;
      begin
           {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('MergeTo10x10andPerformDatumShift in, ' + AreaName); {$EndIf};
           for i := 0 to pred(TilesInArea) do begin
              wmDEM.SetPanelText(0,TimeToStr(Now),true);
              wmDEM.SetPanelText(1,AreaProgress,true);
              HeavyDutyProcessing := true;
              if DEMIX_SpecialCaseRequiringMerge(AreaName) then UTM_Tile := LastSubDir(TheFiles.Strings[i])    //AreaName
              else UTM_Tile := DEMIX_UTM_tileName(AreaName,TheFiles.Strings[i]);
              wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(TilesInArea) + '  ' + UTM_Tile,true);
              TestDir := AreaDir + UTM_Tile + '_ref_test_dem\';
              SafeMakeDir(TestDir);
              SourceEGMName := WGS_EGMdir + UTM_Tile + '.tif';
              {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('Goal=' + SourceEGMName); {$EndIf}
              SafeMakeDir(WGS_EGMdir);
              if FileExists(SourceEGMName) then begin
                {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('Datum shift already done: ' + UTM_Tile); {$EndIf};
              end
              else begin
                 if DEMIX_SpecialCaseRequiringMerge(AreaName) then begin
                     wmDEM.SetPanelText(3,'Merge for 10x10 km tile',true);
                     MergefName := MDtempDir + AreaName + '_merge.tif';
                     ThisTileFiles := tStringList.Create;
                     MergeDir := SourceDir + UTM_Tile;
                     Petmar.FindMatchingFiles(MergeDir,'*.tif',ThisTileFiles,2);
                     if (ThisTileFiles.Count = 0) then Petmar.FindMatchingFiles(MergeDir,'*.asc',ThisTileFiles,2);
                     if (ThisTileFiles.Count = 0) then begin
                        {$IfDef RecordDEMIXDatumShift} HighlightLineToDebugFile('Could not find files to merge in ' + MergeDir); {$EndIf};
                     end
                     else begin
                         {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('Merging Tiles=' + IntToStr(ThisTileFiles.Count) + ' in ' + MergeDir); {$EndIf}
                         ProjName := '';
                         {$If Defined(RecordMerge) or Defined(RecordTimeMerge) or Defined(MergeSummary)} WriteLineToDebugFile('ASC reprojection'); {$EndIf}
                         ProjName := FindSingleWKTinDirectory(SourceDir + 'merges\');
                         if (ProjName <> '') then ProjName := '-a_srs ' + ProjName;

                         UseGDAL_VRT_to_merge(MergefName,OutVRT,ThisTileFiles,ProjName);
                         ThisTileFiles.Destroy;
                         if not FileExists(MergefName) then begin
                            {$IfDef RecordDEMIXDatumShift} HighlightLineToDebugFile('Could not merge ' + MergeFName); {$EndIf};
                         end
                         else begin
                             {$IfDef RecordDEMIXDatumShift} WriteLineToDebugFile('Created ' + MergeFName); {$EndIf};
                             AreaName := UpperCase(AreaName);
                             wmDEM.SetPanelText(3,'Datum shift source ref DTM',true);
                             if (Copy(AreaName,1,3) = 'MX_') then begin
                                ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5703','NAD83',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));
                             end
                             else if (Copy(AreaName,1,3) = 'DK_') then begin
                                ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5799','ETRS89',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));
                             end
                             else if (Copy(AreaName,1,3) = 'EE_') then begin
                               ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '','3301','34');    //true vertical datum 9663 not currently supported by GDAL
                             end
                             else if (Copy(AreaName,1,3) = 'FI_') then begin
                                ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '3900','ETRS89',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));
                             end
                             else if (Copy(AreaName,1,3) = 'DE_') then begin
                                ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '7837','ETRS89',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));   //vertical datum DHHN2016
                             end
                             else if (Copy(AreaName,1,3) = 'UK_') or (Copy(AreaName,1,3) = 'GB_') then begin
                                ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5701','27700','30');
                             end
                             else if (Copy(AreaName,1,3) = 'CH_') then begin
                                ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5728','2056','32');
                             end
                             else if (Copy(AreaName,1,3) = 'FR_') then begin
                                VertEPSG := '';
                                if StrUtils.AnsiContainsText(AreaName,'MARTINIQUE') then begin
                                   //VertEPSG := '5154';
                                   HorizEPSG := '32620';
                                   UTMzone := '20';
                                end
                                else if StrUtils.AnsiContainsText(AreaName,'REUNION') then begin
                                   //VertEPSG := '5156';
                                   HorizEPSG := '2975';
                                   UTMzone := '39S';
                                end
                                else if StrUtils.AnsiContainsText(AreaName,'MAYOTTE') then begin
                                   //VertEPSG := '5191';
                                   HorizEPSG := '4471';
                                   UTMzone := '38S';
                                end
                                else if StrUtils.AnsiContainsText(AreaName,'GUYANNA') then begin
                                   HorizEPSG := '2972';
                                   UTMzone := '22';
                                end
                                else begin
                                   VertEPSG := '5720';
                                   HorizEPSG := '2154';
                                   UTMzone := AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName));
                                end;
                                ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, VertEPSG,HorizEPSG,UTMzone);
                             end
                             else if (Copy(AreaName,1,3) = 'NZ_') then begin
                                ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '','2193',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));  //GDAL cannot do EPSG 1169
                             end
                             else if (Copy(AreaName,1,3) = 'NL_') then begin
                                ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5799','28992','32');
                             end
                             else begin
                                 {$IfDef RecordDEMIX} HighlightLineToDebugFile('Country not covered for  ' + SourceEGMName);  {$EndIf};
                                 exit;
                             end;
                             if (Not FileExists(SourceEGMName)) then begin
                                 {$IfDef RecordDEMIXDatumShift} HighlightLineToDebugFile('Failed to create ' + SourceEGMName);  {$EndIf};
                             end;
                         end;
                     end;
                 end
                 else begin
                     wmDEM.SetPanelText(3,'Datum shift source ref DEM',true);
                     fName := TheFiles.strings[i];
                     {$IfDef RecordDEMIXDatumShift} if (TilesInArea > 1) then WriteLineToDebugFile('Datum shift source DTM ' + ExtractFileNameNoExt(fName)); {$EndIf};
                     if (UpperCase(Copy(AreaName,1,3)) = 'ES_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(fName,SourceEGMName, '5782','ETRS89',AddDayMonthLeadingZero(Geotiff_UTMzone(fName)));
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'IC_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(fName,SourceEGMName,'9397','ETRS89',AddDayMonthLeadingZero(Geotiff_UTMzone(fName)));
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'NO_') then begin
                        UTMzone := Copy(ExtractFileNameNoExt(fName),6,2);
                        ShiftAFile_UTM_WGS84_EGM2008(fName,SourceEGMName,'5941','ETRS89',UTMzone);
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'OV_') then begin
                        //ShiftAFile_UTM_WGS84_EGM2008(fName,SourceEGMName, '','WGS84',AddDayMonthLeadingZero(Geotiff_UTMzone(fName)));
                        Petmar.CopyFile(fName,SourceEGMName);
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'CA_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(fName,SourceEGMName, IntToStr(VertCSNAVD88),'NAD83',AddDayMonthLeadingZero(Geotiff_UTMzone(fName)));
                     end
                     else begin //US data
                        ShiftAFile_UTM_WGS84_EGM2008(fName,SourceEGMName, IntToStr(VertCSNAVD88),'NAD83',AddDayMonthLeadingZero(Geotiff_UTMzone(fName)));
                     end;
                     if (Not FileExists(SourceEGMName)) then begin
                         {$IfDef RecordDEMIXDatumShift} HighlightLineToDebugFile('Failed to create ' + SourceEGMName); {$EndIf};
                     end;
                 end;
               end;
              {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('Finished datum shift: ' + UTM_Tile); {$EndIf};
           end;
          {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('MergeTo10x10andPerformDatumShift out, ' + AreaName); {$EndIf};
        end {procedure MergeTo10x10andPerformDatumShift};


        procedure MakeSourceHRDEM_WGS84_EGM2008;
        //this will nominally be 1 m resolution based on source data
        begin
           //WGS_EGMdir := MDDef.DEMIX_BaseDir + AreaName + '\wgs_egm\';
           {$IfDef RecordDEMIX} WriteLineToDebugFile(' MakeSourceHRDEM_WGS84_EGM2008 in: ' + AreaName); {$EndIf};
           if DEMIX_SpecialCaseRequiringMerge(AreaName) then begin
               SourceDir := MergesDirFName;
               TileSubDirs := GetSubDirsInDirectory(SourceDir);
               TilesInArea := TileSubDirs.Count;
               MergeTo10x10andPerformDatumShift(AreaName,TilesInArea,TileSubDirs);
           end
           else begin
              MoveDEMstoSourceDir;
              SourceDir := MDDef.DEMIX_BaseDir + AreaName + '\source\' ;
              theFiles := TstringList.Create;
              Petmar.FindMatchingFiles(SourceDir,'*.tif',TheFiles,1);
              TilesInArea := Thefiles.Count;
              MergeTo10x10andPerformDatumShift(AreaName,TilesInArea,TheFiles);
           end;
           {$IfDef RecordDEMIX} WriteLineToDebugFile(' MakeSourceHRDEM_WGS84_EGM2008 out: ' + AreaName); {$EndIf};
        end;



begin {procedure UTMbasedtilescreation}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('UTMbasedtilescreation Start: ' + AreaName); {$EndIf};
   HeavyDutyProcessing := true;
   NoDBPlots := true;
   theDEMs := GetListOfTestDEMsinUse;
   AreaDirectoryNames(AreaName);
   MakeSourceHRDEM_WGS84_EGM2008;

     if ValidPath(SourceDir) then begin
        MissingLandCover := tStringList.Create;
     end;

     if ((TilesInArea = 1) or ValidPath(WGS_EGMdir)) and FileExists(MapLibraryFName) then begin  //in case using second computer just to do hard part of work, like datum shift
         TileSubDirs := DEMIX_GetListOfTileSubDirsInArea(AreaName,false);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('Start test/ref for ' + AreaName +  '  tiles=' + IntToStr(TileSubDirs.Count)); {$EndIf};
         for i := 0 to pred(TileSubDirs.Count) do begin
            {$IfDef RecordDEMIXFull} WriteLineToDebugFile(TileSubDirs.Strings[i]); {$EndIf};
            UTM_Tile := DEMIX_TileNameFromSubDir(TileSubDirs.Strings[i]);
            TestDir := MDDef.DEMIX_BaseDir + AreaName + '\' + TileSubDirs.Strings[i] + '\';
            SafeMakeDir(TestDir);
            SourceDEM := 0;
            wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(TileSubDirs.Count) + '  ' + UTM_Tile,true);
            SourceEGMName := WGS_EGMdir + UTM_tile + '.tif';
            if FileExists(SourceEGMName) then begin
                for j := 0 to pred(TheDEMs.Count) do begin
                   HeavyDutyProcessing := true;
                   aDEM := TheDEMs.strings[j];
                   {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Start test/ref for ' + aDEM); {$EndIf};
                   RefDEM := 0;
                   TestDEM := 0;
                   TestName := TestDir + aDEM + '.tif';
                   DEMProgress := IntToStr(succ(j)) + '/' + IntToStr(TheDEMs.Count);
                   if (not FileExists(TestName)) or ((aDEM='ALOS') and (Geotiff_VerticalDatum(TestName) <> VertCSEGM2008)) then begin
                      wmDEM.SetPanelText(3,DEMProgress + ' Test DEM ' + aDEM,true);
                      MakeTestDEM(aDEM,TestDEM,SourceEGMName,TestName);
                   end;
                   MakeReferenceDEM(aDEM,TestDEM,RefDEM,SourceDEM,TestName,RefName,TestDir,SourceEGMName,DEMProgress);
                   CloseSingleDEM(TestDEM);
                   CloseSingleDEM(RefDEM);
                   {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Done ' + aDEM + '  open DEMs=' + IntToStr(NumDEMDataSetsOpen) + '  open maps=' +  IntToStr(NumOpenMaps)); {$EndIf}
                end;
               {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Finished DEMs ' + UTM_Tile); {$EndIf};
            end
            else begin
               {$IfDef RecordDEMIX} HighlightLineToDebugFile('Missing ' + SourceEGMName); {$EndIf};
            end;
         end;
         TileSubDirs.Destroy;
      end;
      MakeLandCoverGridForTile;
      CloseAllDEMs;
      wmDEM.ClearStatusBarPanelText;
      {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Done ' + UTM_Tile + '  open DEMs=' + IntToStr(NumDEMDataSetsOpen) + '  open maps=' +  IntToStr(NumOpenMaps)); {$EndIf}
end {procedure UTMbasedtilescreation};



procedure DEMIX_tile_statistics(AreaName : shortstring);
//Option = udTileStats
var
   TileStats,TileList : tStringList;
   TileDir,TileStatsFName : PathStr;
   Country,DEMIX_tile : shortstring;
   RefDTM,LC, k : integer;


         procedure InitializeStringLists;
         const
            TileStatsString = ',COUNTRY,LAT,LONG,AVG_ELEV,AVG_SLOPE,AVG_ROUGH,RELIEF,FOREST_PC,URBAN_PC,BARREN_PC,WATER_PC,MIN_ELEV,MAX_ELEV';
         begin
            TileStats := tStringList.Create;
            TileStats.Add('AREA,DEMIX_TILE' + TileStatsString);
         end;


         procedure FinalizeStringLists;
         begin
            {$IfDef TrackAverageStats} TileStats.SaveToFile(DEMIXSettingsDir + 'tilestats.csv'); AreaStats.SaveToFile(DEMIXSettingsDir + 'areastats.csv'); {$EndIf}
            if (TileStats.count > 1) then begin
               TileStats.SaveToFile(TileStatsfName);
               TileStats.Destroy;
            end
            else begin
               {$IfDef RecordDEMIX} HighlightLineToDebugFile('No Summary stats for ' + TileStatsfName); {$EndIf}
               TileStats.Destroy;
            end;
         end;


         procedure GetTileStatistics(RefDEM,LandCoverGrid : integer);
         var
           ElevMomentVar,SlopeMomentVar,RoughMomentVar : tMomentVar;
           zs : ^bfarray32;
           Lat,Long : float64;
           aLine : shortstring;
           ForestPC,UrbanPC,BarrenPC,WaterPC : float32;
           Relief,GridFull : float32;
         begin
            if ValidDEM(RefDEM) and ValidDEM(LandCoverGrid) then begin
               InitializeMomentVar(ElevMomentVar);
               InitializeMomentVar(SlopeMomentVar);
               InitializeMomentVar(RoughMomentVar);
               New(zs);
               DEMGlb[RefDEM].ElevationMomentsWithArray(DEMGlb[RefDEM].FullDEMGridLimits,ElevMomentVar,zs^);
               DEMGlb[RefDEM].DEMCenterPoint(Lat,Long);
               Relief := ElevMomentVar.Maxz - ElevMomentVar.Minz;
               if (Relief > 1) then begin
                  DEMGlb[RefDEM].SlopeMomentsWithArray(DEMGlb[RefDEM].FullDEMGridLimits,SlopeMomentVar,zs^);
                  DEMGlb[RefDEM].GetRoughnessInLongArray(DEMGlb[RefDEM].FullDEMGridLimits,RoughMomentVar.NPts,zs^);
                  Moment(zs^,RoughMomentVar,msAll);
                  {$IfDef TrackAverageStats} WriteLineToDebugFile(AreaName + '  ' + DEMIXTile +  '  ' + ShortMomentResultsToString(RoughMomentVar));  {$EndIf}
                  SimplifiedLandCoverPercentages(LandCoverGrid,DEMGlb[LandCoverGrid].FullDEMGridLimits,ForestPC,UrbanPC,BarrenPC,WaterPC);
                  GridFull := -999;
                  aline := AreaName + ',' + DEMIX_TileNameFromSubDir(DEMIX_Tile) + ',' + Country + ',' + RealToString(Lat,-12,-2) + ',' + RealToString(Long,-12,-2) + ',' +
                     RealToString(ElevMomentVar.Mean,-12,-2) + ',' + RealToString(SlopeMomentVar.Mean,-12,-2)  + ',' + RealToString(RoughMomentVar.Mean,-12,-2)  + ',' +
                     RealToString(Relief,-12,-2) + ',' + RealToString(ForestPC,-12,-2)  + ',' + RealToString(UrbanPC,-12,-2) + ',' +
                     RealToString(BarrenPC,-12,-2) + ',' + RealToString(WaterPC,-12,-2)  + ','  + RealToString(ElevMomentVar.MinZ,-12,-2)  + ','  +
                     RealToString(ElevMomentVar.MaxZ,-12,-2);
                  TileStats.Add(aLine);
               end;
               Dispose(zs);
            end;
         end;


var
   fName : PathStr;
begin {procedure DEMIX_tile_statistics}
   if (AreaName[3] = '_') then Country := Copy(AreaName,1,2)
   else Country := 'USA';
   {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMIX_tile_statistics for ' + MDDef.DEMIX_BaseDir); {$EndIf}
   SafeMakeDir(MDDef.DEMIX_BaseDir + 'aa_tile_stats\');
   wmDEM.SetPanelText(0,MDDef.DEMIX_BaseDir,true);
    TileStatsfName := MDDef.DEMIX_BaseDir + 'aa_tile_stats\' + AreaName + '_tile_stats.csv';
    if FileExists(TileStatsfName) then begin
       {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Already exists ' + TileStatsfName); {$EndIf}
    end
    else begin
      InitializeStringLists;
      TileList := DEMIX_GetListOfTileSubdirsInArea(AreaName);
      for k := 0 to pred(TileList.count) do begin
         DEMIX_tile := TileList.Strings[k];
         wmDEM.SetPanelText(2,IntToStr(succ(k)) + '/' + IntToStr(TileList.Count) + ' ' + DEMIX_tile,true);
         TileDir := MDDef.DEMIX_BaseDir + AreaName + '\' + DEMIX_tile + '\';
         fname := TileDir + 'landcover.tif';
         if FileExists(fName) then begin
            LC := OpenNewDEM(fName,false);
            fname := TileDir + 'ref_dtm_srtm.tif';
            if FileExists(fName) then begin
               RefDTM := OpenNewDEM(fName,false);
               GetTileStatistics(RefDTM,LC);
            end
            else begin
               {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMIX_tile_statistics missing ' + fName); {$EndIf}
            end;
         end
         else begin
           //{$IfDef RecordDEMIX} WriteLineToDebugFile('DEMIX_tile_statistics missing ' + fName); {$EndIf}
         end;
         CloseAllDEMs;
       end;
       FinalizeStringLists;
       TileList.Destroy;
    end;
    wmDEM.ClearStatusBarPanelText;
end {procedure DEMIX_tile_statistics};


procedure UTMbasedFUV(FUVmode : integer; AreaName : shortstring; DoAverageSlopeRefDTMs : tStringList = nil; DoAverageElevRefDTMs : tStringList = nil; Option : integer = 0);
//Option = udFUVclac
const
   OpenMaps = false;
   MaxRefDEM = 5;
type
   tManyMomentVar = array[1..15] of tMomentVar;
var
   TileDir,ResultsFName,ResultsDir,
   fName : PathStr;
   UTM_Tile,aDEM,aline,aline2,Criterion,Choice,DEMname : shortstring;
   TileList,TheDEMs,TheCriteria,FUVresults : tStringList;
   i,j,k,UseRef{,RefDEM} : integer;
   NPts,Fixed: int64;
   Lat,Long : float64;
   //z,zref : float32;
   LCGrid : integer;
   gl : tGridLimits;
   RefPresent,
   AllGridsPresent : boolean;
   RefLSPs,TestLSPs,RefDEMs,TestDEMs : array[1..10] of integer;
   Mean,Std : float32;

         function CheckIfDataPresent(gName : PathStr) : boolean;
         begin
            gName := TileDir + gName + '.tif';
            Result := FileExists(gName);
            if not Result then begin
               WriteLineToDebugFile('Missing ' + gName);
               AllGridsPresent := false;
            end;
         end;

         function OpenOneDEM(aDEM : PathStr) : integer;
         var
            gName : PathStr;
         begin
            gName := TileDir + aDEM + '.tif';
            if FileExists(gName) then begin
               LoadNewDEM(Result,gName,false);
               {$IfDef TrackOpenOneDEM} WriteLineToDebugFile('Initial ' + DEMglb[Result].AreaName + ' missing=' + RealToString(DEMGlb[Result].ComputeMissingDataPercentage(DEMGlb[Result].FullDEMGridLimits),-12,-2) + '%'); {$EndIf}
            end
            else Result := 0;
         end;


       procedure LoadAndMaskDEMs;
       var
          j : integer;
       begin
           RefDEMs[1] := OpenOneDEM('ref_dtm_srtm');
           RefDEMs[2] := OpenOneDEM('ref_dtm_alos');
           RefDEMs[3] := OpenOneDEM('ref_dtm_gedtm');
           RefDEMs[4] := OpenOneDEM('ref_dtm_cop');
           RefDEMs[5] := OpenOneDEM('ref_dtm_alos1');

          wmDEM.SetPanelText(3,'Open DEMs',true);
          LCgrid := OpenOneDEM('landcover');
          for j := 1 to theDEMs.Count do begin
             aDEM := theDEMs.Strings[pred(j)];
             TestDEMs[j] := OpenOneDEM(aDEM);
             {$IfDef TrackVerticalDatum} if ValidDEM(TestDEMs[j]) then WriteLineToDebugFile('FUV open ' + aDEM + ' vdatum=' + IntToStr(DEMglb[TestDEMs[j]].DEMheader.VerticalCSTypeGeoKey)); {$EndIf}
          end;

          wmDEM.SetPanelText(3,'Mask water',true);
          DEMGLb[lcGrid].MarkInRangeMissing(80-0.001,80+0.001,Fixed,false);
          for j := 1 to MaxRefDEM do begin
             if ValidDEM(RefDEMs[j]) then begin  //need to check in case high lat refs not needed for this area
                if (j <> 1) then MaskGridFromSecondGrid(TestDEMs[j],RefDEMs[1],msSecondMissing);
                MaskGridFromSecondGrid(RefDEMs[j],lcGrid,msSecondMissing);
                DEMGlb[RefDEMs[j]].CheckMaxMinElev;
                {$IfDef TrackOpenOneDEM} WriteLineToDebugFile('Masked ' + DEMglb[RefDEMs[j]].AreaName + ' missing=' + RealToString(DEMGlb[RefDEMs[j]].ComputeMissingDataPercentage(DEMGlb[RefDEMs[j]].FullDEMGridLimits),-12,-2) + '%'); {$EndIf}
             end;
          end;
          for j := 1 to theDEMs.Count do begin
             MaskGridFromSecondGrid(TestDEMs[j],RefDEMs[1],msSecondMissing);
             MaskGridFromSecondGrid(TestDEMs[j],lcGrid,msSecondMissing);
             DEMGlb[TestDEMs[j]].CheckMaxMinElev;
             {$IfDef TrackOpenOneDEM} WriteLineToDebugFile('Masked ' + DEMglb[TestDEMs[j]].AreaName + ' missing=' + RealToString(DEMGlb[TestDEMs[j]].ComputeMissingDataPercentage(DEMGlb[TestDEMs[j]].FullDEMGridLimits),-12,-2) + '%'); {$EndIf}
          end;

          DEMglb[RefDEMs[1]].DEMCenterPoint(Lat,Long);
        end;


         procedure ExpandLSPcalcFName(var fName : PathStr);
         begin
           fName := StringReplace(fName,'cos-a','cos_aspect',[rfReplaceAll, rfIgnoreCase]);
           fName := StringReplace(fName,'sin-a','sin_aspect',[rfReplaceAll, rfIgnoreCase]);
           fName := StringReplace(fName,'sin-sc','sin_sc',[rfReplaceAll, rfIgnoreCase]);
         end;


             function GetRefDTM(DEM : integer) : integer;
             var
                xspace : float64;
                DEMname : shortstring;
             begin
                 xSpace := DEMGlb[DEM].DEMHeader.DEMxSpacing * 3600;
                 DEMname := Uppercase(DEMGlb[DEM].AreaName);
                 if (DEMname = 'COP') then begin
                    if (xSpace > 1.01) then Result := 4 else Result := 1;
                 end
                 else if (DEMname = 'ALOS') then begin
                    if (xSpace > 1.01) then Result := 5 else Result := 2;
                 end
                 //else if (DEMname = 'GEDTMV1_1') or (DEMname = 'GEDTMV0') then Result := 3
                 else if (DEMname = 'GEDTMV1_2') or (DEMname = 'EDTM') or (DEMname = 'FATHOM') or (DEMname = 'FABDEM') then Result := 1
                 else MessageToContinue('Need to define reference DTM in code for ' + DEMGlb[DEM].AreaName);
                 {$If Defined(RecordRefDTM)} WriteLineToDebugFile('DEM: ' + DEMGlb[DEM].AreaName + '  reference: ' + DEMGlb[Result].AreaName); {$EndIf}
             end;

            procedure GetDifferenceDistributionStatistics;
            var
               {k,Col,Row,}xoffset,yoffset : integer;
               zs : ^bfarray32;
               SameGrid : boolean;
               DEMname : shortstring;

               procedure ProcessOne(Which : shortstring; MomentVar : tManyMomentVar);
               var
                  Decimals : integer;

                  procedure DoOneLine(Param : shortstring);
                  var
                     k : integer;
                     aLine : shortstring;
                  begin
                      aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ','  + which + Param;
                      for k := 1 to theDEMs.Count do begin
                          if Param = '_MEAN' then aline := aline + ',' + RealToString(MomentVar[k].Mean,-12,-Decimals);
                          if Param = '_MED' then  aline := aline + ',' + RealToString(MomentVar[k].Median,-12,-Decimals);
                          if Param = '_RMSE' then aline := aline + ',' + RealToString(MomentVar[k].RMSE,-12,-Decimals);
                          if Param = '_LE90' then aline := aline + ',' + RealToString(MomentVar[k].LE90,-12,-Decimals);
                          if Param = '_MAE' then  aline := aline + ',' + RealToString(MomentVar[k].MAE,-12,-Decimals);
                          if Param = '_AVD' then aline := aline + ',' + RealToString(MomentVar[k].avg_dev,-12,-Decimals);
                          if Param = '_STD' then aline := aline + ',' + RealToString(MomentVar[k].std_dev,-12,-Decimals);
                      end;
                      fuvResults.Add(aLine);
                  end;

               begin {procedure ProcessOne}
                  if which = 'TAND' then Decimals := 8 else Decimals := 2;
                  DoOneLine('_MEAN');
                  DoOneLine('_MED');
                  DoOneLine('_RMSE');
                  DoOneLine('_LE90');
                  DoOneLine('_MAE');
                  DoOneLine('_AVD');
                  DoOneLine('_STD');
               end {procedure ProcessOne};


               procedure DoOneDiffDist(which : shortstring);
               var
                  Col,Row,K : integer;
                  z,zref : float32;
                  MomentVar : tManyMomentVar;

                begin
                    wmDEM.SetPanelText(3,Which,true);
                    for k := 1 to theDEMs.Count do begin
                      InitializeMomentVar(MomentVar[k]);
                      DEMname := TheDEMs.Strings[pred(k)];
                      if SkipDEM(DEMname) then begin
                          aline := aLine + ',-9999';
                      end
                      else begin
                         UseRef := RefDEMs[GetRefDTM(TestDEMs[k])];
                         SameGrid := DEMGlb[UseRef].SecondGridJustOffset(TestDEMs[k],xoffset,yoffset);
                         for Col := 0 to pred(DEMglb[UseRef].DEMheader.NumCol) do begin
                            for Row := 0 to pred(DEMglb[UseRef].DEMheader.NumRow) do begin
                               if ((Which = 'ELVD') and DEMGlb[UseRef].GetElevMetersOnGrid(col,row,zref) and DEMGlb[TestDEMs[k]].GetElevMetersOnGrid(Col+xoffset,Row+yoffset,z)) or
                                  ((Which = 'SLPD') and DEMGlb[UseRef].GetElevMetersOnGrid(col,row,zref) and DEMGlb[TestDEMs[k]].GetElevMetersOnGrid(Col+xoffset,Row+yoffset,z)) or
                                  ((Which = 'RUFD') and DEMGlb[UseRef].RoughnessFromSlopeSTD(Col,Row,5,zref) and DEMGlb[TestDEMs[k]].RoughnessFromSlopeSTD(Col+xoffset,Row+yoffset,5,z)) or
                                  ((Which = 'TAND') and ComputeLSQCurvature(UseRef,eucurv_knc,Col+xoffset,Row+yoffset,zRef) and ComputeLSQCurvature(TestDEMs[k],eucurv_knc,Col+xoffset,Row+yoffset,z)) then begin
                                  inc(MomentVar[k].NPts);
                                  zs^[MomentVar[k].NPts] := z-zref;
                               end;
                            end;
                         end;
                         moment(zs^,MomentVar[k],msIncludeLE90);
                      end;
                    end;
                    ProcessOne(Which,MomentVar);
               end;

            begin
               New(zs);
                if DEMIX_NoVerticalShift(AreaName) then begin
                   //not valid becasue unable to shift reference to EGM2008
                end
                else begin
                   DoOneDiffDist('ELVD');
                end;
                DoOneDiffDist('TAND');
                DoOneDiffDist('SLPD');
                DoOneDiffDist('RUFD');

                Dispose(zs);
            end;


          procedure GetFUVStatistics;
          label
             MissingData;
          var
             Crit,k : integer;
             LSPvalue : float64;
             DEMname : shortstring;
             {$IfDef DEMIXtrackFUV} IsNANproblem : boolean; {$EndIf}
          begin
             {$IfDef DEMIXtrackFUV} WriteLineToDebugFile('GetFUVStatistics ' +  AreaName + '  ' + UTM_Tile);  {$EndIf}
             for Crit := 0 to pred(theCriteria.Count) do begin
                Criterion := theCriteria.Strings[Crit];

                if DEMIX_NoVerticalShift(AreaName) and ((Criterion = 'ELEV') or (Criterion = 'Z_FIT')) then begin
                   //skip this one, since we have mismatch with reference vertical datum
                end
                else begin
                    wmDEM.SetPanelText(3,IntToStr(succ(Crit)) + '/' + IntToStr(TheCriteria.Count) + '  ' + Criterion,true);
                    aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + Criterion;
                    if (Criterion <> 'ELEV') then begin
                       if FUVmode in [fuvmPartials,fuvmCurves] then begin
                          for k := 1 to MaxRefDEM do begin
                            if ValidDEM(RefDEMs[k]) then begin
                               fName := MDtempDir + DEMglb[RefDEMs[k]].AreaName + '_' + Criterion + '.tif';
                               ExpandLSPcalcFName(fName);
                               {$If Defined(RecordFUVcreateFull)} if not FileExists(fName) then WriteLineToDebugFile('RecordFUVcreateFull missing ' + fName); {$EndIf}
                               RefLSPs[k] := OpenNewDEM(fName,false);
                            end;
                          end;
                       end
                       else begin
                          for k := 1 to MaxRefDEM do begin
                             if ValidDEM(RefDEMs[k]) then begin
                                RefLSPs[k] := CreateSingleLSPGrid(OpenMaps,RefDEMs[k],Criterion);
                                {$IfDef DEMIXtrackFUV}
                                   WriteLineToDebugFile(DEMglb[RefDEMs[k]].AreaName + ' missing=' + RealToString(DEMGlb[RefDEMs[k]].ComputeMissingDataPercentage(DEMGlb[RefDEMs[k]].FullDEMGridLimits),-12,-2) + '%');
                                   WriteLineToDebugFile(DEMglb[RefLSPs[k]].AreaName + ' missing=' + RealToString(DEMGlb[RefLSPs[k]].ComputeMissingDataPercentage(DEMGlb[RefLSPs[k]].FullDEMGridLimits),-12,-2) + '%');
                                {$EndIf}
                             end;
                          end;
                          {$If Defined(RecordFUVcreateFull)} if not FileExists(fName) then WriteLineToDebugFile('Reference criteria created for ' + Criterion); {$EndIf}
                       end;
                    end;

                    for k := 1 to theDEMs.Count do begin
                      {$IfDef DEMIXtrackFUV} IsNANproblem := false; {$EndIf}
                      DEMname := TheDEMs.Strings[pred(k)];
                      if SkipDEM(DEMname) then begin
                          aline := aLine + ',-9999';
                      end
                      else begin
                         if ValidDEM(TestDEMs[k]) then begin
                             UseRef := GetRefDTM(TestDEMs[k]);
                             if (Criterion = 'ELEV') or ValidDEM(RefLSPs[UseRef]) then begin
                                 {$If Defined(RecordFUVcreateFull)} WriteLineToDebugFile('Start ' + DEMGlb[TestDEMs[k]].AreaName + '  Ref=' + DEMGlb[TestDEMs[UseRef]].AreaName); {$EndIf}
                                 gl := DEMglb[RefDEMs[UseRef]].FullDEMGridLimits;
                                 if (Criterion = 'ELEV') then begin
                                    aline := aLine + ',' + RealToString(GetFUVForPairGrids(gl,RefDEMs[UseRef],TestDEMs[k]),-12,8);
                                 end
                                 else begin
                                    if FUVmode in [fuvmPartials,fuvmCurves] then begin
                                      fName := MDtempDir + DEMglb[TestDEMs[k]].AreaName + '_' + Criterion + '.tif';
                                      ExpandLSPcalcFName(fName);
                                      TestLSPs[k] := OpenNewDEM(fName,false);
                                    end
                                    else begin
                                       TestLSPs[k] := CreateSingleLSPGrid(OpenMaps,TestDEMs[k],Criterion);
                                    end;
                                    if ValidDEM(TestLSPs[k]) then begin
                                        LSPvalue := GetFUVForPairGrids(gl,RefLSPs[UseRef],TestLSPs[k]);
                                        aline := aLine + ',' + RealToString(LSPvalue,-12,8);
                                    end
                                    else begin
                                       HighLightLineToDebugFile('Missing test LSP' + IntToStr(TestLSPs[k]) + '  criterion = ' + Criterion + '  ' + AreaName + '  ' + UTM_Tile );
                                       goto MissingData;
                                    end;
                                 end;
                             end
                             else begin
                                 HighLightLineToDebugFile('Missing ref dem=' + IntToStr(UseRef) + '  criterion = ' + Criterion);
                                 goto MissingData;
                             end;
                         end
                         else begin
                            HighLightLineToDebugFile('Missing testdem, ' + theDEMs[k] + '  criterion = ' + Criterion);
                            goto MissingData;
                         end;
                      end;
                    end;
                    fuvResults.Add(aLine);
                end;
                MissingData:;
             end {for j};
             CloseAllDEMs;
          end {procedure GetFUVStatistics};


begin {procedure UTMbasedFUV}
   wmDEM.SetPanelText(1,AreaProgress,true);
   if  (Option in [udPixelGeometrySlope,udAverageTileElevations]) then begin
      TheCriteria := Nil;
   end
   else begin
       DEMIX_CriteriaToleranceFNameFromMode(FUVmode);
       GetNamesForMode(FUVMode,AreaName,ResultsDir,ResultsfName);
       SafeMakeDir(ResultsDir);
       if FileExists(ResultsFName) then begin
          {$IfDef RecordDEMIXFull} WriteLineToDebugFile(ResultsfName + ' exists'); {$EndIf};
          exit;
       end;
       TheCriteria := GetListDEMIXOrderedCriteria(DEMIX_criteria_tolerance_fName);
   end;

   AreaDirectoryNames(AreaName);
   TileList := DEMIX_GetListOfTileSubdirsInArea(AreaName);
   theDEMs := GetListOfTestDEMsinUse;

   {$IfDef RecordDEMIXFull} HighlightLineToDebugFile(AreaProgress + ' Start UTMbasedFUV,' + ' mode=' + IntToStr(FUVMode) + ' tiles=' + IntToStr(TileList.Count) +  ' test DEMs=' + IntToStr(theDEMs.Count)); {$EndIf};

   fuvResults := nil;
   for i := 0 to pred(TileList.Count) do begin
      HeavyDutyProcessing := true;
      UTM_Tile := DEMIX_TileNameFromSubDir(TileList.Strings[i]);
      wmDEM.SetPanelText(0,TimeToStr(Now),true);
      wmDEM.SetPanelText(1,AreaProgress,true);
      wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(TileList.Count) + '  ' + UTM_tile,true);
      TileDir := AreaDir + TileList.Strings[i] + '\';
      for j := 1 to 5 do RefDEMs[j] := 0;
      for j := 1 to 10 do TestDEMs[j] := 0;

      AllGridsPresent := true;
      CheckIfDataPresent('landcover');
      for j := 1 to theDEMs.Count do begin
         CheckIfDataPresent(theDEMs.Strings[pred(j)]);
      end;
      {$IfDef AllowGEDTMgeometery}
         RefPresent := CheckIfDataPresent('ref_dtm_srtm') and CheckIfDataPresent('ref_dtm_alos') and CheckIfDataPresent('ref_dtm_gedtm');
      {$Else}
         RefPresent := CheckIfDataPresent('ref_dtm_srtm') and CheckIfDataPresent('ref_dtm_alos');
      {$EndIf}
      if AllGridsPresent and RefPresent then begin
          LoadAndMaskDEMs;
          if (Option in [udPixelGeometrySlope,udAverageTileElevations]) then begin
            wmDEM.SetPanelText(3,'Average tile slope',true);
            aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2);
            aline2 := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2);
            for j := 1 to 5 do begin
               if ValidDEM(RefDEMs[j]) then begin
                  aline := aline + ',' + RealToString(DEMglb[RefDEMs[j]].AverageDEMslope(DEMglb[RefDEMs[j]].FullDEMGridLimits),-8,-2);
                  DEMglb[RefDEMs[j]].ElevationStatistics(DEMglb[RefDEMs[j]].FullDEMGridLimits,Mean,Std,NPts);
                  aline2 := aline2 + ',' + RealToString(Mean,-8,-2);
               end
               else begin
                  aline := aline + ',-9999';
                  aline2 := aline2 + ',-9999';
               end;
            end;
            for j := 1 to theDEMs.Count do begin
                DEMName := theDEMs.Strings[pred(j)];
                if SkipDEM(DEMname) then begin
                    aline := aline + ',-9999';
                    aline2 := aline2 + ',-9999';
                end
                else begin
                    aline := aline + ',' + RealToString(DEMglb[TestDEMs[j]].AverageDEMslope(DEMglb[TestDEMs[j]].FullDEMGridLimits),-8,-2);
                    DEMglb[TestDEMs[j]].ElevationStatistics(DEMglb[TestDEMs[j]].FullDEMGridLimits,Mean,Std,NPts);
                    aline2 := aline2 + ',' + RealToString(Mean,-8,-2);
                end;
            end;
            DoAverageSlopeRefDTMs.Add(aline);
            DoAverageElevRefDTMs.Add(aline2);
          end
          else begin
              if (FUVresults = nil) then begin
                 fuvResults := tStringList.Create;
                 aLine := 'DEMIX_TILE,AREA,LAT,LONG,CRITERION';
                 for j := 1 to theDEMs.Count do aline := aLine + ',' + DEMGlb[TestDEMs[j]].AreaName;
                 fuvResults.Add(aLine);
              end;

             if FUVmode in [fuvmPartials,fuvmCurves] then begin
                if FUVmode in [fuvmPartials] then Choice := ' -p' else Choice := ' -a';
                for k := 1 to MaxRefDEM do if ValidDEM(RefDEMs[k]) then RUN_LSPcalculator(RefDEMs[k],Choice);
                for k := 1 to theDEMs.Count do RUN_LSPcalculator(TestDEMs[k],Choice);
             end;

             if FUVmode in [fuvmDiffDist] then begin
                GetDifferenceDistributionStatistics;
             end
             else begin
                GetFUVStatistics;
             end;
          end;
      end;

      CloseAllDEMs;
      CleanUpTempDirectory(false);
      wmDEM.SetPanelText(2,'',true);
      wmDEM.SetPanelText(3,'',true);
   end {for i};
   if (fuvResults <> Nil) and (fuvResults.Count > 1) then begin
      fuvResults.SaveToFile(ResultsfName);
   end;
   TileList.Destroy;
   theDEMs.Destroy;
   WMDEM.ClearStatusBarPanelText;
end {procedure UTMbasedFUV};


   procedure DEMIX_UTM_AreaTileSurvey(theAreas : tStringList);
   var
      j,db : integer;
      theDEMs,Report,TileSubDirs : tStringList;
      fName : PathStr;
      CSVthere,PartThere,CurveThere,StatsThere,DiffDistThere,
      aLine1,aline2,AreaName : shortstring;
   begin
      Report := tStringList.Create;
      theDEMs := GetListOfTestDEMsinUse;
      aline1 := 'Area,TILES,FUV_CSV,PART_CSV,CURVE_CSV,STATS_CSV,DIFF_DIST';
      Report.Add( aline1);
      StartProgress('File Status');
      for j := 0 to pred(theAreas.Count) do  begin
         if (j mod 10 = 0) then UpdateProgressBar(j/theAreas.Count);
         AreaName := theAreas.Strings[j];
         TileSubDirs := DEMIX_GetListOfTileSubDirsInArea(AreaName);
         AreaProgress := IntToStr(succ(j)) + '/' + IntToStr(theAreas.Count) + '  ' + AreaName;
         AreaDirectoryNames(AreaName);
          wmDEM.SetPanelText(1,AreaProgress,true);
           if FileExists(MDDef.DEMIX_BaseDir + 'aa_fuv_results\' + AreaName + '_fuv_results.csv') then CSVthere := 'Yes' else CSVthere := 'NO';
           if FileExists(MDDef.DEMIX_BaseDir + 'aa_partials_results\' + AreaName + '_fuv_partials.csv') then PartThere := 'Yes' else PartThere := 'NO';
           if FileExists(MDDef.DEMIX_BaseDir + 'aa_curvatures_results\' + AreaName + '_fuv_curvatures.csv') then CurveThere := 'Yes' else CurveThere := 'NO';
           if FileExists(MDDef.DEMIX_BaseDir + 'aa_tile_stats\' + AreaName + '_tile_stats.csv') then StatsThere := 'Yes' else StatsThere := 'NO';
           if FileExists(MDDef.DEMIX_BaseDir + 'aa_diff_dist_results\' + AreaName + '_diff_dist.csv') then DiffDistThere := 'Yes' else DiffDistThere := 'NO';
           aLine2 := AreaName + ',' + IntToStr(TileSubDirs.Count) + ',' + CSVthere + ',' + PartThere + ',' + CurveThere + ',' + StatsThere + ',' + DiffDistThere;
           Report.Add(aline2);
           TileSubDirs.Destroy;
       end;
       fName := NextFileNumber(MDtempDir,'UTM_area_tile_survey','.dbf');
       db := PetDBUtils.StringList2CSVtoDB(Report,fName);
       theDEMs.Destroy;
       EndProgress;
       GISdb[db].ApplyGISfilter('FUV_CSV=' + QuotedStr('No') + ' OR ' + 'PART_CSV=' + QuotedStr('No') + ' OR ' + 'CURVE_CSV=' + QuotedStr('No') + ' OR ' +
           'DIFF_DIST=' + QuotedStr('No') + ' OR ' + 'STATS_CSV=' + QuotedStr('No') );
    end {procedure DEMIX_UTM_AreaTileSurvey};

(*
procedure DEMIX_ZipSource(theAreas : tStringList);
//Option = udZipSourceFiles
//this unfortunately give negligble compression after a long time
var
   i : integer;
   cmd,AreaName : shortstring;
begin
   StartProgress('zipping');
   for i := 0 to pred(theAreas.count) do begin
      AreaName := theAreas.Strings[i];
      AreaDirectoryNames(AreaName);
      if ValidPath(SourceDir) then begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('zipping ' + SourceDir); {$EndIf};
         UpDateProgressBar(succ(i)/TheAreas.Count);
         cmd := 'tar -a -c -f ' + AreaDir + 'source.zip ' + AreaDir + 'source';
         if WinExecAndWait32(cmd) = 0 the begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('zip success ' + SourceDir); {$EndIf};
            CleanOutDirectory(SourceDir);
         end
         else begin
            {$IfDef RecordDEMIX} WriteLineToDebugFile('zip fail ' + SourceDir); {$EndIf};
         end;
      end
      else begin
         {$IfDef RecordDEMIX} WriteLineToDebugFile('missing ' + SourceDir); {$EndIf};
      end;
   end;
end;
*)


procedure DEMIX_SourceFileSurvey(theAreas : tStringList);
// Option = udSourceFileSurvey
var
   Report,TileList,MergeList : tStringList;
   aLine,AreaName,TileName,UTM_Tile : ShortString;
   i,j,k,DEM : integer;
   MP{,Missing,Lat,Long} : float64;
   fName : PathStr;
begin
   Report := tStringList.Create;
   Report.Add('AREA,DEMIX_TILE,SIZE_MB,MERGED,FILE_NAME');
   TileList := nil;
   for i := 0 to pred(theAreas.count) do begin
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,IntToStr(succ(i)) + '/' + IntToStr(TheAreas.Count) + '  ' + AreaName,true);
      if DEMIX_SpecialCaseRequiringMerge(AreaName) then begin
         TileList := DEMIX_GetListOfTileNamesInArea(AreaName);
         for j := 0 to pred(TileList.count) do begin
            SourceDir := MergesDirFName + TileList.Strings[j] + '\';
            Petmar.FindMatchingFiles(SourceDir,'*.tif',MergeList,1);
            if (MergeList.Count = 0) then Petmar.FindMatchingFiles(SourceDir,'*.asc',MergeList,1);
            if (MergeList.Count = 0) then begin
              {$IfDef RecordDEMIX} WriteLineToDebugFile('No HRDEM in ' + SourceDir); {$EndIf};
            end
            else begin
                for k := 0 to pred(MergeList.count) do begin
                   TileName := MergeList.Strings[k];
                   DEM := Petmar.GetFileSize(TileName);
                   MP := DEM / 1024 / 1024;
                   aline := AreaName + ',' + TileList.Strings[j] + ',' + RealToString(MP,-8,-2) + ',' + IntToStr(MergeList.Count) + ',' + ExtractFileName(TileName);
                   Report.Add(Aline);
                end;
            end;
         end;
      end
      else begin
          SourceDir := MDDef.DEMIX_BaseDir + AreaName + '\source\';
          Petmar.FindMatchingFiles(SourceDir,'*.tif',TileList,1);
          for j := 0 to pred(TileList.count) do begin
             TileName := TileList.Strings[j];
             UTM_Tile := DEMIX_UTM_tileName(AreaName,TileName);
             DEM := Petmar.GetFileSize(TileName);
             MP := DEM / 1024 / 1024;
             aline := AreaName + ',' + UTM_Tile + ',' + RealToString(MP,-8,-2) + ',1,' + ExtractFileName(TileName);
             Report.Add(Aline);
          end;
      end;
   end;
   fName := MDTempDir + 'HRDEM_source_files.dbf';
   PetDBUtils.StringList2CSVtoDB(Report,fName);

(*
   Report := tStringList.Create;
   Report.Add('AREA,SourceName,TILE_SIZE,LAT,LONG,MEGA_PIXELS,PC_FULL');
   TileList := nil;
   for i := 0 to pred(theAreas.count) do begin
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,IntToStr(succ(i)) + '/' + IntToStr(TheAreas.Count) + '  ' + AreaName,true);
      SourceDir := MDDef.DEMIX_BaseDir + AreaName + '\';
      Petmar.FindMatchingFiles(SourceDir,'*.tif',TileList,2);
      for j := 0 to pred(TileList.count) do begin
         TileName := TileList.Strings[j];
         aline := AreaName + ',' + ExtractFileNameNoExt(TileName) + ',';
         wmDEM.SetPanelText(2,IntToStr(succ(j)) + '/' + IntToStr(TileList.Count) + '  ' + TileName,true);
         DEM := OpenNewDEM(TileName,false);
         MP := (DEMGLb[DEM].DEMHeader.NumCol * DEMGLb[DEM].DEMHeader.NumRow) / 1024 / 1024;
         Missing := DEMglb[DEM].ComputeMissingDataPercentage(DEMGlb[DEM].FullDEMGridLimits);
         DEMglb[DEM].DEMCenterPoint(Lat,Long);
         aline := aline + DEMGlb[DEM].DEM_size_km + ',' + RealToString(Lat,-12,-3) + ',' + RealToString(Long,-12,-3) + ',' + RealToString(MP,-8,-4) + ',' + RealToString(100-Missing,-8,-2);
         CloseSingleDEM(DEM);
         Report.Add(Aline);
      end;
   end;
   fName := MDTempDir + 'source_files.dbf';
   PetDBUtils.StringList2CSVtoDB(Report,fName);
*)
end {procedure DEMIX_FileFill};


procedure DEMIX_FileFill(theAreas : tStringList; DEMtoUse : shortstring = 'ref_dtm_srtm.tif');
// Option = udFileFillReport
var
   Report,TileList : tStringList;
   aLine,AreaName,TileName,TStr : ShortString;
   i,j,DEM,LC : integer;
   Fixed : int64;
   MP,Missing,Missing2,Lat,Long : float64;
   fName,fName2 : PathStr;
begin
   Report := tStringList.Create;
   Report.Add('AREA,DEMIX_TILE,TILE_SIZE,LAT,LONG,MEGA_PIXELS,PC_FULL,PC_DRY,PTS_USED,PROBLEMS');
   for i := 0 to pred(theAreas.count) do begin
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,IntToStr(succ(i)) + '/' + IntToStr(TheAreas.Count) + '  ' + AreaName,true);
      TileList := DEMIX_GetListOfTileSubdirsInArea(AreaName);
      for j := 0 to pred(TileList.count) do begin
         TileName := Demix_TileNameFromSubDir(TileList.Strings[j]);
         aline := AreaName + ',' + TileName + ',';
         wmDEM.SetPanelText(2,IntToStr(succ(j)) + '/' + IntToStr(TileList.Count) + '  ' + TileName,true);
         fName := MDDef.DEMIX_BaseDir + AreaName + '\' + TileList.Strings[j] + '\' + DEMtoUse;
         fName2 := MDDef.DEMIX_BaseDir + AreaName + '\' + TileList.Strings[j] + '\landcover.tif';
         if FileExists(fName) and FileExists(fName2) then begin
            DEM := OpenNewDEM(fName,false);
            LC := OpenNewDEM(fName2,false);
            MP := (DEMGLb[DEM].DEMHeader.NumCol * DEMGLb[DEM].DEMHeader.NumRow) / 1024;
            Missing := DEMglb[DEM].ComputeMissingDataPercentage(DEMGlb[DEM].FullDEMGridLimits);
            DEMGLb[lc].MarkInRangeMissing(80-0.001,80+0.001,Fixed,false);
            MaskGridFromSecondGrid(DEM,lc,msSecondMissing);
            Missing2 := DEMglb[DEM].ComputeMissingDataPercentage(DEMGlb[DEM].FullDEMGridLimits);
            DEMglb[DEM].DEMCenterPoint(Lat,Long);
            aline := aline + DEMGlb[DEM].DEM_size_km + ',' + RealToString(Lat,-12,-3) + ',' + RealToString(Long,-12,-3) + ',' + RealToString(MP,-8,-4) + ',' + RealToString(100-Missing,-8,-2)  +
                ',' + RealToString(100-Missing2,-8,-2) + ',' + RealToString(0.01 * (100-Missing2)*MP,-8,-2)+  ',  ';
            CloseSingleDEM(DEM);
            CloseSingleDEM(LC);
         end
         else begin
            if FileExists(fName) then TStr := 'Landcover'
            else if FileExists(fName2) then TStr := 'Ref SRTM'
            else TStr := 'Ref SRTM and landcover';
            Aline := aline + '0,0,0,0,' + TStr + ' missing';
         end;
         Report.Add(Aline);
      end;
   end;
   fName := MDTempDir + 'tile_voids.dbf';
   PetDBUtils.StringList2CSVtoDB(Report,fName);
end {procedure DEMIX_FileFill};


procedure DEMIX_Tile_BoundingBox(theAreas : tStringList; DEMtoUse : shortstring = 'ref_dtm_srtm.tif');
// Option = udTileBoundingBoxes
var
   Report,TileList : tStringList;
   aLine,AreaName,TileName : ShortString;
   i,j : integer;
   fName : PathStr;
   bb : sfBoundBox;
begin
   Report := tStringList.Create;
   Report.Add('AREA,DEMIX_TILE,Lat_LOW,Long_Low,Lat_Hi,Long_Hi');
   for i := 0 to pred(theAreas.count) do begin
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,IntToStr(succ(i)) + '/' + IntToStr(TheAreas.Count) + '  ' + AreaName,true);
      TileList := DEMIX_GetListOfTileSubdirsInArea(AreaName);
      for j := 0 to pred(TileList.count) do begin
         TileName := Demix_TileNameFromSubDir(TileList.Strings[j]);
         fName := MDDef.DEMIX_BaseDir + AreaName + '\' + TileList.Strings[j] + '\' + DEMtoUse;
         if FileExists(fName) then begin
            if GeotiffBoundingBoxGeo(fName,bb) then begin
               aline := AreaName + ',' + TileName + ',' + RealToString(bb.ymin,-12,-6) + ',' + RealToString(bb.xmin,-12,-6) + ',' +
                  RealToString(bb.ymax,-12,-6) + ',' + RealToString(bb.xmax,-12,-6);
               Report.Add(Aline);
            end;
         end;
      end;
   end;
   fName := MDTempDir + 'tile_bounding_boxes.dbf';
   PetDBUtils.StringList2CSVtoDB(Report,fName);
end {procedure DEMIX_FileFill};


procedure DEMIX_CompareLandcovers(theAreas : tStringList; DEMtoUse : shortstring = 'ref_dtm_srtm.tif');
// Option = udCompareLandCover
var
   Report,TileList : tStringList;
   aLine,AreaName,TileName : ShortString;
   i,j,DEM,LC : integer;
   Lat,Long : float64;
   fName,fName2 : PathStr;
   ForestPC,UrbanPC,BarrenPC,WaterPC : float32;
begin
   Report := tStringList.Create;
   Report.Add('AREA,DEMIX_TILE,LAT,LONG,FOREST_10,BARREN_10,URBAN_10,FOREST_100,BARREN_100,URBAN_100');
   for i := 0 to pred(theAreas.count) do begin
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,IntToStr(succ(i)) + '/' + IntToStr(TheAreas.Count) + '  ' + AreaName,true);
      TileList := DEMIX_GetListOfTileSubdirsInArea(AreaName);
      for j := 0 to pred(TileList.count) do begin
         TileName := Demix_TileNameFromSubDir(TileList.Strings[j]);
         aline := AreaName + ',' + TileName + ',';
         wmDEM.SetPanelText(2,IntToStr(succ(j)) + '/' + IntToStr(TileList.Count) + '  ' + TileName,true);
         fName := MDDef.DEMIX_BaseDir + AreaName + '\' + TileList.Strings[j] + '\' + DEMtoUse;
         fName2 := MDDef.DEMIX_BaseDir + AreaName + '\' + TileList.Strings[j] + '\landcover.tif';
         if FileExists(fName) and FileExists(fName2) then begin
            DEM := OpenNewDEM(fName,false);
            DEMglb[DEM].DEMCenterPoint(Lat,Long);
            LC := OpenNewDEM(fName2,false);
            SimplifiedLandCoverPercentages(LC,DEMGlb[LC].FullDEMGridLimits,ForestPC,UrbanPC,BarrenPC,WaterPC);
            CloseSingleDEM(LC);
            aline := aline + RealToString(Lat,-12,-3) + ',' + RealToString(Long,-12,-3) + ',' + RealToString(ForestPC,-8,-2) + ',' + RealToString(BarrenPC,-8,-2)  +
                ',' + RealToString(UrbanPC,-8,-2) + ',';
            LC := LoadLC100LandCover('',DEMglb[DEM].DEMBoundBoxGeo,false);
            SimplifiedLandCoverPercentages(LC,DEMGlb[LC].FullDEMGridLimits,ForestPC,UrbanPC,BarrenPC,WaterPC);
            CloseSingleDEM(LC);
            aline := aline + RealToString(ForestPC,-8,-2) + ',' + RealToString(BarrenPC,-8,-2)  + ',' + RealToString(UrbanPC,-8,-2);
            CloseSingleDEM(DEM);
         end;
         Report.Add(Aline);
      end;
   end;
   fName := MDTempDir + 'compare_landcover.dbf';
   PetDBUtils.StringList2CSVtoDB(Report,fName);
end {procedure DEMIX_FileFill};



procedure DEMIX_SelectivelyDeleteCSVresults(theAreas : tStringList);
//Option = udDeleteResultsCSVsForAreas
var
   i : integer;
   AreaName : PathStr;
begin
   for i := 0 to pred(theAreas.count) do begin
      AreaName := theAreas.Strings[i];
      DeleteCSV_FilesForArea(AreaName);
   end;
end;


procedure TakeCareOfInvalidTiles(theAreas : tStringList);
const
   FixIssues = false;
var
   i,j,k,DEM,TestDEM,EGMref,lc : integer;
   AreaName,aLine : shortstring;
   SourceEGMName,fName,TileDirName,AreaLock : PathStr;
   theDEMs, Findings,TileDirs : tStringList;
   Lat1,Long1,Lat2,Long2,DistanceMeters,Bearing: float64;
   bb : sfBoundBox;
   HaveSRTMref,HaveALOSref,OneBad : boolean;


       function CheckCentroidDistance(BaseDEM : integer; TestName : shortstring) : boolean;
       var
          fName : PathStr;
          TestDEM : integer;
       begin
           Result := true;
           TestName := UpperCase(TestName);
           fName := TileDirName + TestName + '.tif';
           if GeotiffCentroidLatLong(fName,Lat2,Long2) then begin
               VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,DistanceMeters,Bearing);
               if (DistanceMeters > 5000) then begin
                  //DeleteFileIfExists(fName);
                  aLine := aline + ',' + RealToString(0.001 * DistanceMeters,-12,-2);
                  OneBad := true;
                  Result := false;
                  if (TestName = 'ALOS') then begin
                     LoadNewDEM(DEM,fName,false);
                     DEMGlb[DEM].SaveGridSubsetGeotiff(DEMGlb[DEM].sfBoundBox2tGridLimits(bb),fName);
                     CloseSingleDEM(TestDEM);
                     aLine := aLine + ' (subset)';
                  end;
               end
               else aLine := aline + ',OK' ;
           end
           else begin
              aLine := aLine + ',' + 'Missing';
              OneBad := true;
              if FixIssues and ((TestName = 'EDTM') or (TestName = 'GEDTMV0') or  (TestName = 'GEDTMV1_1') or  (TestName = 'GEDTM1_2')) then begin
                 wmDEM.SetPanelText(3,'DownLoad',true);
                 TestDEM := WebExtractGEDTMorEDTM(TestName,bb,fName,false);
                 if ValidDEM(TestDEM) then
                    aLine := aLine + ' (added)'
                 else Result := false;
                 CloseSingleDEM(TestDEM);
              end
              else if (TestName = 'LANDCOVER') then begin
                 if FixIssues then begin
                     wmDEM.SetPanelText(3,'DownLoad',true);
                     lc := LoadLC10LandCover(fName,bb,false,true);
                     if ValidDEM(lc) then
                        aLine := aLine + ' (added)'
                     else Result := false;
                 CloseSingleDEM(lc);
                 end;
              end
              else if (TestName = 'REF_DTM_GEDTM') or (TestName = 'REF_DTM_SRTM') or (TestName = 'REF_DTM_ALOS') then begin
                 Result := false;
                 //handle later, with one loading of SourceEGM
              end
              else begin
                 //does not yet handle the test DEMs loaded from the library
                 //MakeTestDEM(aDEM,SourceDEM,TestDEM,SourceEGMName,TestName);
              end;
           end;
           wmDEM.SetPanelText(3,'',true);
       end;


            procedure MakeOneRef(aDEM : shortstring);
            var
               TestName,RefName : PathStr;
               RefDEM : integer;
            begin
               TestName := TileDirName + aDEM + '.tif';
               if FileExists(TestName) then begin
                  LoadNewDEM(TestDEM,TestName,false);
                  RefDEM := 0;
                  MakeReferenceDEM(aDEM,TestDEM,RefDEM,EGMRef,TestName,RefName,TileDirName,SourceEGMName);
               end
               else WriteLineToDebugFile('Missing, cannot create ref:  ' + TestName);
               CloseSingleDEM(TestDEM);
               CloseSingleDEM(RefDEM);
           end;



begin {procedure TakeCareOfInvalidTiles}
   WriteLineToDebugFile('Identify problem data sets and start fixing, areas=' + IntToStr(theAreas.Count));
   theDEMs := GetListOfTestDEMsinUse;
   Findings := tStringList.Create;
   aLine := 'AREA,DEMIX_TILE,SOURCE_EGM,SRTM_REF,ALOS_REF,LANDCOVER';
   for i := 0 to pred(theDEMs.Count) do begin
       aline := aline + ',' + theDEMs.Strings[i];
   end;
   Findings.Add(aLine + ',' + 'EVERYTHING');
   for i := 0 to pred(theAreas.Count) do begin
      AreaName := theAreas.Strings[i];
      AreaLock := MDDef.DEMIX_BaseDir + AreaName + '\aa_arealocked.txt' ;
      if ClonedEXE and FileExists(AreaLock) then begin
         WriteLineToDebugFile('Area locked: ' + AreaName);
      end
      else begin
        theAreas.SaveToFile(AreaLock);
        WGS_EGMdir := MDDef.DEMIX_BaseDir + AreaName + '\wgs_egm\';
        wmDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(theAreas.Count) + '  ' + AreaName,true);
        if DEMIX_SpecialCaseRequiringMerge(AreaName) then begin
           WriteLineToDebugFile('Not yet doing special areas: ' + AreaName);
        end
        else begin
          TileDirs := DEMIX_GetListOfTileNamesInArea(AreaName,false);
          WriteLineToDebugFile('Start: ' + AreaName + '   tiles=' + IntToStr(TileDirs.Count));
          for j := 0 to pred(TileDirs.Count) do begin
             wmDEM.SetPanelText(2,IntToStr(j) + '/' + IntToStr(TileDirs.Count) + '  ' + TileDirs.Strings[j],true);
             OneBad := false;
             aLine := AreaName + ',' + TileDirs.Strings[j];
             SourceEGMName := WGS_EGMdir + TileDirs.Strings[j] + '.tif';
             if FileExists(SourceEGMName) then aline := aline + ',OK'
             else begin
                aLine := aline + ',missing';
                OneBad := true;
             end;

             TileDirName := MDDef.DEMIX_BaseDir + AreaName + '\' + TileDirs.Strings[j] + '_ref_test_dem\';
             if ValidPath(TileDirName) and FileExists(SourceEGMName) then begin
               GeotiffCentroidLatLong(SourceEGMName,Lat1,Long1);
               GeotiffBoundingBoxGeo(SourceEGMName,bb);
               HaveSRTMref := CheckCentroidDistance(DEM,'ref_dtm_srtm');
               HaveALOSref := CheckCentroidDistance(DEM,'ref_dtm_alos');
               CheckCentroidDistance(DEM,'landcover');
               for k := 0 to pred(theDEMs.Count) do begin
                  CheckCentroidDistance(DEM,theDEMs.Strings[k]);
               end;
             end;

             if FixIssues and FileExists(SourceEGMName) then begin
                if (not HaveSRTMref) or (not HaveALOSref) then begin
                   if FileExists(SourceEGMName) then begin
                       LoadNewDEM(EGMref,SourceEGMName,false);
                       if (not HaveSRTMref) then begin
                           MakeOneRef('FATHOM');
                           MakeOneRef('COP');  //makes high latitude version
                       end;
                       if (not HaveALOSref) then MakeOneRef('ALOS');
                       CloseSingleDEM(EGMref);
                   end
                   else WriteLineToDebugFile('Missing, cannot create all refs:  ' + SourceEGMName);
                end;
             end;
             if OneBad then Findings.Add(aLine + ',PROBLEM')
             else Findings.Add(aLine + ',OK');
          end {for j};
         end {OK for special area};
         DeleteFileIfExists(AreaLock);
      end {OK for area lock};
   end {for i};
   if (Findings.Count > 1) then begin
      fName := MDTempDir + 'tile_location_inventory.dbf';
      PetDBUtils.StringList2CSVtoDB(Findings,fName);
   end
   else MessageToContinue('No problems encountered');
   DeleteFileIfExists(AreaLock);


(*

   WriteLineToDebugFile('Fix GEDTM elevations off by factor 10');
   Done := 0;
   for i := 0 to pred(theAreas.Count) do begin
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(theAreas.Count) + '  ' + AreaName,true);
      TileDirs := DEMIX_GetListOfTileNamesInArea(AreaName);
      for j := 0 to pred(TileDirs.Count) do begin
         TileDirName := MDDef.DEMIX_BaseDir + AreaName + '\' + TileDirs.Strings[j] + '_ref_test_dem\';
         wmDEM.SetPanelText(2,IntToStr(j) + '/' + IntToStr(TileDirs.Count) + '  ' + TileDirs.strings[j],true);

         fName := TileDirName + 'cop.tif';
         fName2 := TileDirName + 'gedtmv1_2.tif';

         if FileExists(fName) and FileExists(fName2) then begin
            LoadNewDEM(COP,fName,false);
            LoadNewDEM(GEDTM,fName2,false);
            if DEMglb[Cop].DEMHeader.MaxElev / DEMglb[GEDTM].DEMHeader.MaxElev > 5 then begin
               DEMglb[GEDTM].MultiplyGridByConstant(10);
               DEMglb[GEDTM].CheckMaxMinElev;
               DEMglb[GEDTM].SaveAsGeotiff(fName2);
               inc(Done);
               wmDEM.SetPanelText(3,'Fixed=' + IntToStr(Done),true);
            end;
            CloseSingleDEM(Cop);
            CloseSingleDEM(GEDTM);
         end;
      end;
   end;



   WriteLineToDebugFile('Fix alos file sizes');
   Done := 0;
   for i := 0 to pred(theAreas.Count) do begin
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(theAreas.Count) + '  ' + AreaName,true);
      TileDirs := DEMIX_GetListOfTileNamesInArea(AreaName);
      for j := 0 to pred(TileDirs.Count) do begin
         TileDirName := MDDef.DEMIX_BaseDir + AreaName + '\' + TileDirs.Strings[j] + '_ref_test_dem\';
         fName := TileDirName + 'alos.tif';
         if (Petmar.GetFileSize(fName) > 2000000) or (Petmar.GetFileSize(fName) < 10000) then begin
            inc(done);
            WriteLineToDebugFile(IntToStr(Done) + '  ' + fName + '   ' + SmartMemorySizeBytes(Petmar.GetFileSize(fName)));
            fName2 := TileDirName + 'cop.tif';
            if FileExists(fName2) then begin
                LoadNewDEM(COP,fName2,false);
                bb := DEMglb[COP].DEMBoundBoxGeo;
                CloseSingleDEM(Cop);
                if (Petmar.GetFileSize(fName) > 2000000) then begin
                    LoadNewDEM(ALOS,fName,false);
                    if not ValidDEM(ALOS) then begin
                       WriteLineToDebugFile('Point 1, ALOS still bad');
                    end;
                    DEMGlb[ALOS].SaveGridSubsetGeotiff( DEMGlb[ALOS].sfBoundBox2tGridLimits(bb),fName);
                    WriteLineToDebugFile('Subset ' + fName + '   ' + SmartMemorySizeBytes(Petmar.GetFileSize(fName)));
                    CloseSingleDEM(ALOS);
                end;
                if (Petmar.GetFileSize(fName) < 10000) then begin
                   ALOS := LoadMapLibraryBox(true,bb,'AW3D30-FEB2025',false);
                   if ValidDEM(ALOS) then begin
                      DEMGlb[ALOS].SaveGridSubsetGeotiff( DEMGlb[ALOS].sfBoundBox2tGridLimits(bb),fName);
                      WriteLineToDebugFile('Library load ' + fName + '   ' + SmartMemorySizeBytes(Petmar.GetFileSize(fName)));
                      CloseSingleDEM(ALOS);
                   end;
                end;
            end
            else begin
               WriteLineToDebugFile('Missing COP: ' + '  ' + fName2);
            end;
         end;
      end;
   end;


   WriteLineToDebugFile('Identify gedtm ref DEMs missing or too much missing data');
   NakedMapOptions;
   Done := 0;
   for i := 0 to pred(theAreas.Count) do begin
      if (i mod 10 = 0) then UpdateProgressBar(i/theAreas.Count);
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,AreaName,true);
      if not DEMIX_SpecialCaseRequiringMerge(AreaName) then begin
        TileDirs := DEMIX_GetListOfTileNamesInArea(AreaName);
        for j := 0 to pred(TileDirs.Count) do begin
           TileDirName := MDDef.DEMIX_BaseDir + AreaName + '\' + TileDirs.Strings[j] + '_ref_test_dem\';
           if ValidPath(TileDirName) then begin
               fName := TileDirName + 'ref_dtm_gedtm.tif';
               if FileExists(fName) then begin
                   LoadNewDEM(GEDTMref,fName,false);
                   Missing := DEMglb[GEDTMref].ComputeMissingDataPercentage(DEMglb[GEDTMref].FullDEMgridLimits);
                   if (Missing > 50) then begin
                      WriteLineToDebugFile(fName + '   ' + RealToString(Missing,-12,-2));
                   end;
               end
               else WriteLineToDebugFile(fName + '  missing file');
               CloseAllDEMs;
           end;
        end;
      end;
   end;


   WriteLineToDebugFile('Fix landcovers that don't cover area');
   for i := 0 to pred(theAreas.Count) do begin
      if (i mod 10 = 0) then UpdateProgressBar(i/theAreas.Count);
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,AreaName,true);
      for j := 0 to pred(TileDirs.Count) do begin
         TileDirName := MDDef.DEMIX_BaseDir + AreaName + '\' + TileDirs.Strings[j] + '_ref_test_dem\';
         fName :=TileDirName + 'ref_dtm_srtm.tif';
         LCname := TileDirName + 'landcover.tif';

         if FileExists(fName) and FileExists(LCname) then begin
             LoadNewDEM(DEM,fName,false);
             LoadNewDEM(lc,lcName,false);
             DEMglb[DEM].DEMCenterPoint(Lat1,Long1);
             DEMglb[LC].DEMCenterPoint(Lat2,Long2);
             VincentyCalculateDistanceBearing(Lat1,Long2,Lat2,Long2,DistanceMeters,Bearing);
             if (DistanceMeters > 5000) then begin
                bb := DEMglb[DEM].DEMBoundBoxGeo;
                lc2 := LoadLC10LandCover(LCname,bb,false,true);
                WriteLineToDebugFile('rewrite ' + LCName);
             end;
             CloseAllDEMs;
         end;
      end;
   end;


//this option dealt with areas that had some bogus tiles, and removed them
   SourceFiles := nil;
   EGMFiles := nil;
   Findings := tStringList.Create;
   Findings.Add('AREA,SOURCE,WGS_EGM,TILES,EGM_ISSUE,TILE_ISSUE');
   StartProgress('Tile status');
   for i := 0 to pred(theAreas.Count) do begin
      if (i mod 10 = 0) then UpdateProgressBar(i/theAreas.Count);
      AreaName := theAreas.Strings[i];
      if not DEMIX_SpecialCaseRequiringMerge(AreaName) then begin
         SourceDir := MDDef.DEMIX_BaseDir + AreaName + '\source\' ;
         WGS_EGMdir := MDDef.DEMIX_BaseDir + AreaName + '\wgs_egm\';
         Petmar.FindMatchingFiles(SourceDir,'*.tif',SourceFiles,1);
         Petmar.FindMatchingFiles(WGS_EGMDir,'*.tif',EGMFiles,1);
         TileDirs := DEMIX_GetListOfTileNamesInArea(AreaName);
         if (SourceFiles.Count <> EGMFiles.Count) then EGMIssue := 'YES' else EGMIssue := '';
         if (SourceFiles.Count <> TileDirs.Count) then TileIssue := 'YES' else TileIssue := '';
         Findings.Add(AreaName + ',' + IntToStr(SourceFiles.Count) + ',' + IntToStr(EGMFiles.Count)  + ',' + IntToStr(TileDirs.Count) + ',' + EGMIssue + ',' + TileIssue);
         if (TileDirs.Count > EGMFiles.Count) then begin
            for j := 0 to pred(TileDirs.Count) do begin
               fName := WGS_EGMdir + TileDirs.Strings[j] + '.tif';
               if not FileExists(fName) then begin
                  TileDirName := MDDef.DEMIX_BaseDir + AreaName + '\' + TileDirs.Strings[j] + '_ref_test_dem\';
                  if ValidPath(TileDirName) then begin
                      WriteLineToDebugFile(TileDirName);
                      TDirectory.Delete(TileDirName, True);
                  end;
               end;
            end;
         end;
      end;
   end;
   EndProgress;
   fName := MDTempDir + 'tile_inventory.dbf';
   PetDBUtils.StringList2CSVtoDB(Findings,fName);


   //removes GEDTM from one area, redownloads, and then recreates GEDTM test DEMs
   if Option = udFixGEDTM then begin
      AreaName := 'ATLANTA_WEST';
      AreaDir := MDDef.DEMIX_BaseDir + AreaName + '\' ;
      theFiles := TstringList.Create;
      Petmar.FindMatchingFiles(AreaDir,'GEDTMV1_2.tif',TheFiles,1);
      for i := 0 to pred(theFiles.Count) do begin
          fName := theFiles.Strings[i];
          DeleteFileIfExists(fName);
      end;
      theFiles.Destroy;
      DeleteCSV_FilesForArea(AreaName);
      UTMbasedtilescreation(AreaName,MissingLandCover);
   end
*)
end {procedure TakeCareOfInvalidTiles};


procedure CreateLandCoverCatsInDEMIXTiles;
//(Option = udLandcoverCats)
const
   OpenMap = false;
var
  ForestGrid,BarrenGrid,UrbanGrid,
  x,y,lc : integer;
  UrbanName,BarrenName,ForestName,
  CopFilesFName : Pathstr;
  bb : sfBoundBox;
  CopFiles : tStringList;
  GridLimits : tGridLimits;
  ForestPC,UrbanPC,BarrenPC,WaterPC,BigLat,BigLong : float32;

       function OpenFile(fName : PathStr; var FullName : PathStr) : integer;
       begin
           FullName := DEMIX_final_DB_dir + fName + '_percent.tif';
           if FileExists(FullName) then begin
              LoadNewDEM(Result,FullName,OpenMap)
           end
           else begin
              Result := CreateNewGlobalGrid(PixelIsPoint,OpenMap, false,FloatingPointDEM, 0.1);
           end;
           DEMglb[Result].DEMHeader.ElevUnits := euPercentSlope;
       end;

begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateLandCoverCatsInDEMIXTiles in'); {$EndIf}
   CopFilesFName := DEMIX_final_DB_dir + 'cop_files_left.txt';
   if FileExists(CopFilesFName) then begin
      CopFiles := tStringList.Create;
      CopFiles.LoadFromFile(CopFilesFName);
   end
   else begin
      MessageToContinue('Create file and then retry: ' + CopFilesFName);
      exit;
   end;

   HeavyDutyProcessing := true;
   MDDef.ShowWinExec := false;
   WantShowProgress := false;

   ForestGrid := OpenFile('forest',ForestName);
   BarrenGrid := OpenFile('barren',BarrenName);
   UrbanGrid := OpenFile('urban',UrbanName);

   BigLat := -70;
   while BigLat < 70 do begin
      wmDEM.SetPanelText(2,'Lat=' + IntToStr(round(BigLat)),true);
      BigLong := -180;
      while BigLong < 180 do begin
          wmDEM.SetPanelText(3,'Long=' + IntToStr(round(BigLong)),true);
          bb.ymin := BigLat;
          bb.ymax := BigLat + 10;
          bb.xmin := BigLong;
          bb.xmax := BigLong + 10;
          lc := LoadLC100LandCover('',bb,false);
          if ValidDEM(lc) then begin

              for x := 0 to 99 do begin
                 bb.xmin := BigLong + x * 0.1;
                 bb.xmax := bb.xmin + 0.1;
                 for y := 0 to 99 do begin
                    bb.ymin := BigLat + y * 0.1;
                    bb.ymax := bb.ymin + 0.1;
                    GridLimits := DEMglb[lc].sfBoundBox2tGridLimits(bb);
                    SimplifiedLandCoverPercentages(lc, GridLimits,ForestPC,UrbanPC,BarrenPC,WaterPC);

                    if (BarrenPC > 0.01) or (ForestPC > 0.01) or (UrbanPC > 0.01) then begin
                       DEMglb[BarrenGrid].SetGridElevationLatLongDegree(0.05 + BigLat + 0.1 * y,0.05 + BigLong + 0.1 * x,BarrenPC);
                       DEMglb[ForestGrid].SetGridElevationLatLongDegree(0.05 + BigLat + 0.1 * y,0.05 + BigLong + 0.1 * x,ForestPC);
                       DEMglb[UrbanGrid].SetGridElevationLatLongDegree(0.05 + BigLat + 0.1 * y,0.05 + BigLong + 0.1 * x,UrbanPC);
                    end;
                 end;
              end;
          end
          else begin
          end;
          CloseSingleDEM(lc);
          BigLong := BigLong + 10;
      end;
      BigLat := BigLat + 10;
      DEMglb[BarrenGrid].CheckMaxMinElev;
      DEMglb[BarrenGrid].SaveAsGeotiff(BarrenName);
      DEMglb[ForestGrid].CheckMaxMinElev;
      DEMglb[ForestGrid].SaveAsGeotiff(ForestName);
      DEMglb[UrbanGrid].CheckMaxMinElev;
      DEMglb[UrbanGrid].SaveAsGeotiff(UrbanName);
   end;
   CleanUpTempDirectory(true);
   HeavyDutyProcessing := false;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateLandCoverCatsInDEMIXTiles out'); {$EndIf}
end {procedure CreateLandCoverCatsInDEMIXTiles};


procedure CreateTileCatBinMap;
// (Option = udTileCatBinMap)
var
   SlopeName,BarrenName,fName2 : PathStr;
   x,y,i,j,AvgSlopeGrid,BarrenGrid,BinMap,xoffset,yoffset,cat1,cat2 : integer;
   z1,z2 : float32;
   Filters1,Labels1,Filters2,Labels2,Lowers1,Uppers1,Lowers2,Uppers2 : tStringList;
   Hist : array[1..100] of int64;
   VAT : tStringList;
begin
   HeavyDutyProcessing := true;
   MDDef.ShowWinExec := false;
   WantShowProgress := false;

   MDDef.DEMIX_filter1_fName:= 'C:\microdem\demix_settings\filters_avg_slope_6_cat.dbf';
   MDDef.DEMIX_filter2_fName:= 'C:\microdem\demix_settings\filters_barren_pc.dbf';

   ImportLandParamFiltersLong(MDDef.DEMIX_filter1_fName,Filters1,Labels1,Lowers1,Uppers1);
   ImportLandParamFiltersLong(MDDef.DEMIX_filter2_fName,Filters2,Labels2,Lowers2,Uppers2);


   SlopeName := DEMIX_final_DB_dir + 'average_tile_slope.tif';
   BarrenName :=  DEMIX_final_DB_dir + 'barren_percent.tif';
   LoadNewDEM(AvgSlopeGrid,SlopeName,false);
   LoadNewDEM(BarrenGrid,BarrenName,false);

   BinMap := CreateNewGlobalGrid(PixelIsPoint,true, false,ByteDEM, 0.1);
   DEMglb[BinMap].DEMHeader.ElevUnits := euIntCode;

   DEMGlb[AvgSlopeGrid].SecondGridJustOffset(BarrenGrid,xoffset,yoffset);

   for i := 1 to 100 do Hist[i] := 0;

   StartProgress('Bin map');
   for x := 0 to pred(DEMglb[BinMap].DEMHeader.NumCol) do begin
      UpdateProgressBar(x/DEMglb[BinMap].DEMHeader.NumCol);
      for y := 0 to pred(DEMglb[BinMap].DEMHeader.NumRow) do begin
         if DEMglb[AvgSlopeGrid].GetElevMetersOnGrid(x,y,z1) then begin
            if DEMglb[BarrenGrid].GetElevMetersOnGrid(x+xoffset,y+yoffset,z2) then begin
               Cat1 := 0;
               for i := 0 to pred(Uppers1.Count) do begin
                  if (z1 >= StrToFloat(Lowers1.Strings[i])) and (z1 <= StrToFloat(Uppers1.Strings[i])) then Cat1 := succ(i);
               end;
               Cat2 := 0;
               for i := 0 to pred(Uppers2.Count) do begin
                  if (z2 >= StrToFloat(Lowers2.Strings[i])) and (z2 <= StrToFloat(Uppers2.Strings[i])) then Cat2 := succ(i);
               end;
               if (Cat1 > 0) and (Cat2 > 0) then begin
                  DEMglb[BinMap].SetGridElevation(x,y,10*Cat1 + Cat2);
                  inc(Hist[10*Cat1 + Cat2]);
               end;
            end;
         end;
      end;
   end;
   EndProgress;
   DEMglb[BinMap].CheckMaxMinElev;
   DEMGlb[BinMap].AreaName := 'dem_bins';
   Vat := tStringList.Create;
   Vat.add('VALUE,NAME,N,USE,COLOR');
   j := 0;
      for Cat1 := 0 to pred(Labels1.Count) do begin
         for Cat2 := 0 to pred(Labels2.Count) do begin
            i := 10*succ(Cat1) + succ(Cat2);
            Vat.Add(IntToStr(i) + ',' + Labels1.Strings[Cat1] + ' and ' + Labels2.Strings[Cat2] + ',' + IntToStr(Hist[i]) + ',Y,' + IntToStr(WinGraphColors(j)));
            inc(j);
         end;
      end;

    fName2 := NextFileNumber(MDTempDir,DEMGlb[BinMap].AreaName,'.tif');
    DEMGlb[BinMap].SaveAsGeotiff(fName2);

    fName2 := NextFileNumber(MDTempDir,DEMGlb[BinMap].AreaName,'.vat.dbf');
    StringList2CSVtoDB(vat,fName2,true);

   DEMGlb[BinMap].VATFileName := fName2;


   DEMglb[BinMap].SetUpMap(false);
end;

(*
type
   tHist = array[0..8] of int64;
const
   Colors : array[0..8] of tColor = (clBlue,clGreen,clAqua,
                                           clYellow,clLime,clTeal,
                                           clPurple,clFuchsia,clBrown);
 var
   i,Higher,Lower : integer;
   x,y : integer;
   HighHist,LowHist : tHist;


         procedure Finalize(Neigh : integer; Hist : tHist; aLabel : shortstring; ReverseColors : boolean);
         var
            VAT : tStringList;
            fName2 : PathStr;
            i : integer;
            aColor : tColor;
         begin
            Vat := tStringList.Create;
            Vat.add('VALUE,NAME,N,USE,COLOR');
            for i := 8 downto 0 do if (Hist[i] > 0) then begin
               if ReverseColors then aColor := Colors[8-i] else aColor := Colors[i];
               Vat.add(IntToStr(i) + ',' + IntToStr(i) + aLabel + ',' + IntToStr(Hist[i]) + ',Y,' + IntToStr(aColor));
            end;
            fName2 := NextFileNumber(MDTempDir,DEMGlb[Neigh].AreaName,'.vat.dbf');
            StringList2CSVtoDB(vat,fName2,true);
            DEMGlb[Neigh].VATFileName := fName2;
            DEMglb[Neigh].CheckMaxMinElev;
            DEMglb[Neigh].SetUpMap(true,mtDEMVATTable);
         end;


begin
   {$If Defined(RecordDEMCompare) or Defined(NewVATgrids)} WriteLineToDebugFile('TwoDEMHighLowMap in, DEM=' + IntToStr(DEM)); {$EndIf}
   if ValidDEM(DEM)then begin
      for i := 0 to 8 do begin
          HighHist[i] := 0;
          LowHist[i] := 0;
      end;
      HighNeigh := DEMGlb[DEM].CloneAndOpenGridSetMissing(ByteDEM,'higher_neighors_' + DEMGlb[DEM].AreaName + '_rad='+ IntToStr(Radius) + '_tol=' + RealToString(Tolerance,-4,-1),euIntCode);
      LowNeigh := DEMGlb[DEM].CloneAndOpenGridSetMissing(ByteDEM,'lower_neighors_' + DEMGlb[DEM].AreaName+ '_rad='+ IntToStr(Radius) + '_tol=' + RealToString(Tolerance,-4,-1),euIntCode);
      StartProgressAbortOption('High/low neighbors');
*)


procedure CreateAverageSlopeMapInDEMIXTiles;
// (Option = udAverageSlopeMap
//requires text file with files to use for the LSP computation, from the index (filter to Cop, then list of unique FILENAMEs
const
   OpenMap = true;
var
  AvgSlopeGrid,Lat,Long,x,y,Cop,i,ToDo,Done : integer;
  LSPunits : byte;
  LSPName : shortstring;
  Found,Fixed : int64;
  fName,CopFilesFName : Pathstr;
  Slope : float32;
  bb : sfBoundBox;
  CopFiles : tStringList;
  GridLimits : tGridLimits;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateAverageSlopeMapInDEMIXTiles in'); {$EndIf}
   LSPUnits := euPercentSlope;
   LSPname := 'average_tile_slope';

   CopFilesFName := DEMIX_final_DB_dir + 'cop_files_left.txt';
   if FileExists(CopFilesFName) then begin
      CopFiles := tStringList.Create;
      CopFiles.LoadFromFile(CopFilesFName);
   end
   else begin
      MessageToContinue('Create file and then retry: ' + CopFilesFName);
      exit;
   end;

   HeavyDutyProcessing := true;
   MDDef.ShowWinExec := false;
   WantShowProgress := false;
   fName := DEMIX_final_DB_dir + LSPName + '.tif';
   if FileExists(fName) then begin
      LoadNewDEM(AvgSlopeGrid,fName,OpenMap)
   end
   else begin
      AvgSlopeGrid := CreateNewGlobalGrid(PixelIsPoint,OpenMap, false,FloatingPointDEM, 0.1);
      DEMglb[AvgSlopeGrid].DEMHeader.ElevUnits := LSPUnits;
   end;

   Found := 0;
   ToDo := CopFiles.Count;
   Done := 0;
   for i := pred(CopFiles.Count) downto 0 do begin
       if (i mod 10 = 0) then begin
          wmDEM.SetPanelText(1,TimeToStr(Now),true);
          wmDEM.SetPanelText(2,'One degree COP ' +  IntToStr(Done) + '/' + IntToStr(ToDo),true);
       end;
       inc(Done);
       Cop := OpenNewDEM(CopFiles.Strings[i],false);
       if ValidDEM(Cop) then begin
          DEMGLb[Cop].MarkInRangeMissing(-0.01,0.01,Fixed,false);
          Lat := round(DEMglb[Cop].DEMBoundBoxGeo.YMin);
          Long := round(DEMglb[Cop].DEMBoundBoxGeo.XMin);
          for x := 0 to 9 do begin
             bb.xmin := Long + x * 0.1;
             bb.xmax := bb.xmin + 0.1;
             for y := 0 to 9 do begin
                bb.ymin := Lat + y * 0.1;
                bb.ymax := bb.ymin + 0.1;
                GridLimits := DEMglb[Cop].sfBoundBox2tGridLimits(bb);

                Slope := DEMglb[Cop].AverageDEMslope(GridLimits);
                if (Slope > -1) then begin
                   DEMglb[AvgSlopeGrid].SetGridElevationLatLongDegree(0.05 + Lat + 0.1 * y,0.05 + Long + 0.1 * x,Slope);
                   inc(Found);
                end;
             end;
          end;
          CloseSingleDEM(Cop);
          CopFiles.Delete(i);
       end;
       if (i mod 500 = 0) then begin
         wmDEM.SetPanelText(3,'n=' + IntToStr(Found),true);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('files left=' + IntToStr(i) + '  tiles done=' + IntToStr(Found)); {$EndIf}
         DEMglb[AvgSlopeGrid].CheckMaxMinElev;
         DEMglb[AvgSlopeGrid].SaveAsGeotiff(fName);
         DEMglb[AvgSlopeGrid].SelectionMap.DoBaseMapRedraw;
         CleanUpTempDirectory(true);
         if CopFiles.Count > 0 then CopFiles.SaveToFile(CopFilesFName)
         else DeleteFileIfExists(CopFilesFName);
       end;
   end;
   CopFiles.Destroy;
   HeavyDutyProcessing := false;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('CreateAverageSlopeMapInDEMIXTiles out'); {$EndIf}
end;


procedure DEMIX_UTM_based_processing(Option : integer);
var
   fName,AreaLock : PathStr;
   AreaName,aLine : ShortString;
   theFiles,theDEMs,MissingLandCover,TheAreas,Report,Report2 : tStringList;
   I,j,db : Integer;
   CanLimitAreas : boolean;
begin
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('DEMIX_UTM_based_processing in, option=' + IntToStr(Option) + AvailablePhysicalMemoryString); {$EndIf}
   {$If Defined(RecordFormResize) or Defined(RecordFormActivate)} WriteLineToDebugFile('Twmdem.FormActivate MDn set, width=' + IntToStr(wmdem.Width) + '  & height=' + IntToStr(wmdem.Height)); {$EndIf}
   SaveBackupDefaults;
   GetDEMIXpaths(True);

   GetDOSPath('Base data directory',MDDef.DEMIX_BaseDir);
   theAreas := nil;
   if (Option in [1]) then MissingLandCover := tStringList.Create;

   if (Option in [6,8,12,13,14,udMergeDTMDSMcompare,udMergeCopDTMDSMcompare]) then begin
       if (Option in [udMergeDTMDSMcompare]) then begin
           MDdef.DEMIX_tile_chars_fname := MDDef.DEMIX_BaseDir + 'DSM_DTM_compare_DB_' + CurrentTimeForFileName(false) + '.dbf';
           theFiles := nil;
           Petmar.FindMatchingFiles(MDDef.DEMIX_BaseDir + 'aa_dsm_dtm\','*.csv',TheFiles,2);
           MergeMultipleCSVorTextFiles(nil,theFiles,MDdef.DEMIX_tile_chars_fname);
       end;
       if (Option in [udMergeCopDTMDSMcompare]) then begin
           MDdef.DEMIX_tile_chars_fname := MDDef.DEMIX_BaseDir + 'COP_DSM_DTM_compare_DB_' + CurrentTimeForFileName(false) + '.dbf';
           theFiles := nil;
           Petmar.FindMatchingFiles(MDDef.DEMIX_BaseDir + 'aa_cop_dsm_dtm\','*.csv',TheFiles,2);
           MergeMultipleCSVorTextFiles(nil,theFiles,MDdef.DEMIX_tile_chars_fname);
       end;
       if (Option in [8]) then begin
           MDdef.DEMIX_tile_chars_fname := DEMIX_final_DB_dir + 'UTM_tile_stats_DB_' + CurrentTimeForFileName(false) + '.dbf';
           MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_tile_stats'),MDdef.DEMIX_tile_chars_fname);
       end;
       if (Option in [6]) then begin
           fName := DEMIX_final_DB_dir + 'UTM_FUV_DB_' + CurrentTimeForFileName(false) + '.dbf';
           db := MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_fuv_results'),fName);
           if ValidDB(db) then AddTileCharacteristicsToDB(db);
       end;
       if (Option in [12]) then begin
           fName := DEMIX_final_DB_dir + 'UTM_diff_distrib_DB_' + CurrentTimeForFileName(false) + '.dbf';
           db := MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_diff_dist_results'),fName);
           if ValidDB(db) then AddTileCharacteristicsToDB(db);
       end;
       if (Option in [13]) then begin
           fName := DEMIX_final_DB_dir + 'UTM_curvatures_DEMIX_DB_' + CurrentTimeForFileName(false) + '.dbf';
           db := MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_curvatures_results'),fName);
       end;
       if (Option in [14]) then begin
           fName := DEMIX_final_DB_dir + 'UTM_partials_DEMIX_DB_' + CurrentTimeForFileName(false) + '.dbf';
           db := MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_partials_results'),fName);
       end;
   end
   else if (Option = udAverageSlopeMap) then begin
      CreateAverageSlopeMapInDEMIXTiles;
   end
   else if (Option = udLandcoverCats) then begin
      CreateLandCoverCatsInDEMIXTiles;
   end
   else if (Option = udTileCatBinMap) then begin
      CreateTileCatBinMap;
   end
   else begin
      CanLimitAreas := not (Option in [3,udCSVReportByArea,udClearAreaLocks]);
      theAreas := DEMIX_AreasWanted(CanLimitAreas);
      if (TheAreas.Count = 0) then begin
          {$IfDef RecordDEMIX} WriteLineToDebugFile('No Selected areas='); {$EndIf}
      end
      else begin
          {$IfDef RecordDEMIX} WriteLineToDebugFile('Selected areas=' + IntToStr(theAreas.Count)); {$EndIf}
          if (Option in [udFileFillReport]) then DEMIX_FileFill(theAreas)
          else if (Option = udTileBoundingBoxes) then DEMIX_Tile_BoundingBox(theAreas)
          else if Option in [udInvalidTiles] then TakeCareOfInvalidTiles(theAreas)
          else if (Option in [3,udCSVReportByArea]) then DEMIX_UTM_AreaTileSurvey(theAreas)
          else if (Option in [udSourceFileSurvey]) then DEMIX_SourceFileSurvey(theAreas)
          else if (Option in [udCompareLandCover]) then DEMIX_CompareLandcovers(theAreas)
          else if (Option in [udDeleteResultsCSVsForAreas]) then begin
             DEMIX_SelectivelyDeleteCSVresults(theAreas);
             if AnswerIsYes('Reprocess CSV files') then Option := 20;
          end;
          Report := nil;
          Report2 := Nil;
          if (Option in [udPixelGeometrySlope,udAverageTileElevations]) then begin
             Report := tStringList.Create;
             Report2 := tStringList.Create;
             aline := 'DEMIX_TILE,AREA,LAT,LONG,REF_SRTM,REF_ALOS,REF_GEDTM,REF_COP,REF_ALOS1';
             theDEMs := GetListOfTestDEMsinUse;
             for j := 0 to pred(theDEMs.Count) do aLine := aline + ',' + theDEMs.Strings[j];
             Report.Add(aLine);
             Report2.Add(aLine);
             theDEMs.Destroy;
          end;
          for I := 0 to pred(theAreas.Count) do begin
            AreaName := theAreas.Strings[i];
            AreaProgress := IntToStr(succ(i)) + '/' + IntToStr(theAreas.Count) + '  ' + AreaName;
            AreaLock := MDDef.DEMIX_BaseDir + AreaName + '\aa_arealocked.txt';
            wmDEM.SetPanelText(1,AreaProgress,true);
            if (Option = udClearAreaLocks) then begin
               DeleteFileIfExists(AreaLock);
            end
            else if FileExists(AreaLock) then begin
                {$IfDef RecordDEMIX} HighlightLineToDebugFile(theAreas.Strings[i] + ' Area locked for ' + IntToStr(theAreas.Count)); {$EndIf}
            end
            else begin
                theAreas.SaveToFile(AreaLock);
                if (Option in [1]) then UTMbasedtilescreation(AreaName,MissingLandCover);
                if (Option in [udFUVCalc,20]) then UTMbasedFUV(fuvmMixed,AreaName);
                if (Option in [9,20]) then UTMbasedFUV(fuvmPartials,AreaName);
                if (Option in [10,20]) then UTMbasedFUV(fuvmCurves,AreaName);
                if (Option in [11,20]) then UTMbasedFUV(fuvmDiffDist,AreaName);
                if (Option in [udPixelGeometrySlope,udAverageTileElevations]) then UTMbasedFUV(0,AreaName,Report,Report2,Option);
                if (Option in [udTileStats,20]) then DEMIX_tile_statistics(AreaName);
                DeleteFileIfExists(AreaLock);
             end;
          end;
       end;

       if (Option = 1) then begin
          if (MissingLandCover.Count > 0) then begin
             StringListToContinue(true,MissingLandCover);
          end
          else MissingLandCover.Destroy;
       end;
       if (Option in [udPixelGeometrySlope,udAverageTileElevations]) then begin
           fName := DEMIX_final_DB_dir + 'Average_tile_slope_' + CurrentTimeForFileName(false) + '.dbf';
           PetDBUtils.StringList2CSVtoDB(Report,fName);
           fName := DEMIX_final_DB_dir + 'Average_tile_elevation_' + CurrentTimeForFileName(false) + '.dbf';
           PetDBUtils.StringList2CSVtoDB(Report2,fName);
       end;
   end;
   if (theAreas <> nil) then TheAreas.Destroy;
   EndDEMIXProcessing;
   NoDBPlots := false;
   RestoreBackupDefaults;
   {$IfDef RecordDEMIX} HighLightLineToDebugFile('DEMIX_UTM_based_processing out' + AvailablePhysicalMemoryString); {$EndIf}
end {procedure DEMIX_UTM_based_processing};




