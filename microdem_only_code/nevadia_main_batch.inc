var
   AreaDir,SourceDir,WGS_EGMdir : PathStr;
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



procedure MoveSmallTilesInto10KTiles;
var
   thePath,fName,TilePath,SourceDir : PathStr;
   theFiles : tStringList;
   CountryCode,
   Easting,Northing,AreaName : shortstring;
   East,North,i : Integer;

   function NZ_thirds(value : integer) : ANSIchar;
   begin
      if Value in [1..17] then Result := 'a'
      else if Value in [18..34] then Result := 'b'
      else Result := 'c';
   end;

begin
   GetDosPath('small tiles',ThePath);
   AreaName := LastSubDir(ThePath);
   CountryCode := copy(AreaName,1,3);
   TheFiles := tStringList.Create;
   Petmar.FindMatchingFiles(ThePath,'*.tif',TheFiles,1);
   StartProgress('Segregate 10x10 tiles');
   if (CountryCode = 'ch_') or (CountryCode = 'fr_') or (CountryCode = 'nz_') then begin
     for i := 0 to pred(TheFiles.Count) do begin
        if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);

        fName := theFiles.Strings[i];
         if (CountryCode = 'ch_') then begin
            {
            swissalti3d_2023_2762-1154_2_2056_5728.tif
            }
            Easting := Copy(ExtractFileNameNoExt(fName),18,3);
            Northing := Copy(ExtractFileNameNoExt(fName),23,3);
            TilePath := ThePath + AreaName + '_' + Easting + '-' + Northing + '\';
         end;

         if (CountryCode = 'fr_') then begin
            {
            LHD_FXX_0419_6703_MNT_O_0M50_LAMB93_IGN69.tif
            }
            Easting := Copy(ExtractFileNameNoExt(fName),9,3);
            Northing := Copy(ExtractFileNameNoExt(fName),14,3);
            TilePath := ThePath + AreaName + '_' + Easting + '-' + Northing + '\';
         end;

         if (CountryCode = 'nz_') then begin
            {
            DEM_BQ27_2025_1000_1701.tif
            //BQ27 is the 1:50K map quad, which covers about 860 square km
            //tile names at the four corners of 50K quads
            //  CE11_1000_5050     CE12_1000_5001
            //  CF11_1000_0150     CF12_1000_0101
            }
            East := StrToInt(Copy(ExtractFileNameNoExt(fName),22,2));
            North := StrToInt(Copy(ExtractFileNameNoExt(fName),20,2));
            TilePath := ThePath + AreaName + '_' + Nz_thirds(East) + '-' + Nz_thirds(North) + '\';
         end;
         SourceDir := TilePath + 'source\';
         SafeMakeDir(TilePath);
         SafeMakeDir(SourceDir);
         Petmar.MoveFile(fName,SourceDir + ExtractFileName(fName));
     end;
   end;
   TheFiles.Destroy;
   EndProgress;
end;


procedure MakeAreaDirectories(BaseDir : PathStr; AreaName : shortstring);
begin
   AreaDir := BaseDir + AreaName + '\';
   SourceDir := AreaDir + 'source\';
   WGS_EGMdir := AreaDir + 'wgs_egm\';
end;


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
       DEMGlb[Result].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
       DEMglb[Result].SaveAsGeotiff(gName);
       {$IfDef TrackVerticalDatum} WriteLineToDebugFile('DatumShiftALOS ' + aDEM + ' vdatum=' + IntToStr(DEMglb[Result].DEMheader.VerticalCSTypeGeoKey)); {$EndIf}
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


function DEMIX_GetListOfTileSubDirsInArea(AreaName : shortstring; ShouldHaveBeenCreated : boolean = true) : tStringList;
//these have '_ref_test_dem' plus the tile name
var
  i : integer;
  TiffsThere : tStringList;
  AreaDir,TileDir : PathStr;
begin
   AreaDir := MDDef.DEMIX_BaseDir + AreaName + '\';
   Result := GetSubDirsInDirectory(AreaDir);
   TiffsThere := tStringList.Create;
   for i := pred(Result.Count) downto 0 do begin
      if (UpperCase(Copy(Result.Strings[i],1,3)) = 'AA_') or (Result.Strings[i] = 'source') or (Result.Strings[i] = 'wgs_egm') then Result.Delete(i)
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

begin
(*
{ Rhineland-Palatinate
https://geobasis-rlp.de/data/dgm1/current/tif/dgm01_32_323_5516_1_rp.tif'}
// 32 is UTM zone
//323nnn is easting
//5516nnn is northing
   BaseLink := 'https://geobasis-rlp.de/data/dgm1/current/tif/dgm01_32_323_5516_1_rp.tif';
   ListName := 'de_rh_dgm01_32_323_5516.txt';
   Pos1 := 58;
   Pos2 := 63;


{Bavaria
https://download1.bayernwolke.de/a/dgm/dgm1/654_5260.tif  }
//654 is 654nnn easting
//5260 is 5260nnn northing
   BaseLink := 'https://download1.bayernwolke.de/a/dgm/dgm1/654_5260.tif';
   ListName := 'de_bv_dgm1_654_5260.txt';
   Pos1 := 47;
   Pos2 := 52;
*)
{Saxony
https://geocloud.landesvermessung.sachsen.de/index.php/s/9JHEbMApfuHxkiF/download?path=%2F&files=dgm1_33321_5598_1_sn_tiff.zip       }
   //33440 is zone 33, 440nnn easting
   //5633 is 5630nnn northing
   BaseLink := 'https://geocloud.landesvermessung.sachsen.de/index.php/s/9JHEbMApfuHxkiF/download?path=%2F&files=dgm1_33440_5633_1_sn_tiff.zip';
   ListName := 'de_sx_dgm1_3344_563';
   Pos1 := 107;
   Pos2 := 112;
   DoOne;
end;




      procedure PerformDatumShift(AreaName : shortstring; TilesInArea : integer; TheFiles : tStringList);
      var
         i{,SourceDEM} : integer;
         UTM_Tile : shortstring;
         MergefName,OutVRT,TestDir,SourceEGMName,fName : PathStr;
      begin
          {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('PerformDatumShift in, ' + AreaName); {$EndIf};
           for i := 0 to pred(TilesInArea) do begin
              wmDEM.SetPanelText(0,TimeToStr(Now),true);
              wmDEM.SetPanelText(1,AreaProgress,true);
              HeavyDutyProcessing := true;
              if DEMIX_SpecialCase(AreaName) then UTM_Tile := AreaName
              else UTM_Tile := DEMIX_UTM_tileName(AreaName,TheFiles.Strings[i]);
              wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(TilesInArea) + '  ' + UTM_Tile,true);
              TestDir := AreaDir + UTM_Tile + '_ref_test_dem\';
              SafeMakeDir(TestDir);
              SourceEGMName := WGS_EGMdir + UTM_Tile + '.tif';
              {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('Goal=' + SourceEGMName); {$EndIf}
              SafeMakeDir(WGS_EGMdir);
              if (Not FileExists(SourceEGMName)) then begin
                 MergefName := MDtempDir + AreaName + '_merge.tif';
                 {$IfDef RecordDEMIXDatumShift} if TilesInArea = 1 then WriteLineToDebugFile('Datum shift source DTM ' + AreaName + ' to ' + MergefName ); {$EndIf};
                 wmDEM.SetPanelText(3,'Datum shift source ref DTM',true);
                 if DEMIX_SpecialCase(AreaName) then begin
                     UseGDAL_VRT_to_merge(MergefName,OutVRT,TheFiles);
                     if (UpperCase(Copy(AreaName,1,3)) = 'MX_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5703','NAD83',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'DK_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5799','ETRS89',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'EE_') then begin
                       ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '','3301','34');    //true vertical datum 9663 not currently supported by GDAL
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'FI_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '3900','ETRS89',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'DE_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '7837','ETRS89',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));   //vertical datum DHHN2016
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'UK_') or (UpperCase(Copy(AreaName,1,3)) = 'GB_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5701','27700','30');
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'CH_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5728','2056','32');
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'FR_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5720','2154',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'NZ_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '','2193',AddDayMonthLeadingZero(Geotiff_UTMzone(MergefName)));  //GDAL cannot do EPSG 1169
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'NL_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(MergefName,SourceEGMName, '5799','28992','32');
                     end
                 end
                 else begin
                     fName := TheFiles.strings[i];
                     {$IfDef RecordDEMIXDatumShift} if (TilesInArea > 1) then WriteLineToDebugFile('Datum shift source DTM ' + ExtractFileNameNoExt(fName)); {$EndIf};
                     if (UpperCase(Copy(AreaName,1,3)) = 'ES_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(fName,SourceEGMName, '5782','ETRS89',Copy(UTM_Tile,6,2));
                     end
                     else if (UpperCase(Copy(AreaName,1,3)) = 'IC_') then begin
                        ShiftAFile_UTM_WGS84_EGM2008(fName,SourceEGMName, '9397','ETRS89',Copy(UTM_Tile,6,2));
                     end
                     else begin //US data
                        ShiftAFile_UTM_WGS84_EGM2008(fName,SourceEGMName, IntToStr(VertCSNAVD88),'NAD83',AddDayMonthLeadingZero(Geotiff_UTMzone(fName)));
                     end;
                 end;
                 if (Not FileExists(SourceEGMName)) then begin
                     MessageToContinue('ShiftAFile_UTM_WGS84_EGM2008 failed to create ' + SourceEGMName);
                 end;
                 {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('Finished datum shift: ' + UTM_Tile); {$EndIf};
              end
             else begin
                {$IfDef RecordDEMIXDatumShiftFull} WriteLineToDebugFile('Skipped datum shift: ' + UTM_Tile + '  tiles=' + IntToStr(TilesInArea)); {$EndIf};
             end;
           end;
          {$IfDef RecordDEMIXDatumShiftFull}WriteLineToDebugFile('PerformDatumShift out, ' + AreaName); {$EndIf};
        end {procedure PerformDatumShift};


 procedure MakeTestDEM(aDEM: shortstring; var TestDEM : integer; SourceEGMName,TestName : PathStr);
 var
    {$If Defined(TimeMakeMaps)} Stopwatch : TStopwatch; {$EndIf}
    bb : sfBoundBox;
 begin
    //if not ValidDEM(SourceDEM) then begin
      //LoadNewDEM(SourceDEM,SourceEGMName,false);
    //end;
    if FileExists(SourceEGMName) then begin
        {$IfDef RecordDEMIXMakeTestFull} WriteLineToDebugFile('Create test DEM ' + TestName); {$EndIf};
        wmDEM.SetPanelText(0,TimeToStr(Now),true);
        //bb := DEMglb[SourceDEM].DEMBoundBoxGeo;
        GeotiffBoundingBoxGeo(SourceEGMName,bb);

        {$If Defined(TimeMakeMaps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
        if (aDEM = 'GEDTMV0') or (aDEM = 'EDTM') or (aDEM = 'GEDTMV1_1') or (aDEM = 'GEDTMV1_2') then begin
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
            RefDEM := DEMGlb[TestDEM].ResaveNewElevationPrecision(fcSaveFloatingPoint);
            if ValidDEM(RefDEM) then begin
               DEMglb[RefDEM].MarkAboveMissing(-9999,NPts,false);            //entire DEM missing to start with
               DEMGlb[SourceDEM].ResampleByAveraging(false,RefName,RefDEM); //mean aggregate new reference DEM
            end
            else begin
               {$IfDef RecordDEMIXMakeRef} WriteLineToDebugFile('Missing ref DEM for' + RefName); {$EndIf};
            end;
        end;
    end;

begin  {procedure MakeReferenceDEM}
   if (aDEM = 'COP') or (aDEM= 'ALOS') or (aDEM = 'GEDTMV1_1') or (aDEM = 'FATHOM') then begin
      SecondMode := '';
      if (aDEM = 'FATHOM') or (aDEM = 'FABDEM') then Mode := 'srtm'
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
      else if (aDEM = 'GEDTMV1_1') then mode := 'gedtm';

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
   UTM_Tile,aDEM,Mode,SecondMode,TStr,DEMProgress : shortstring;
   TheFiles,TheDEMs,TileSubDirs : tStringList;

   i,j,SourceDEM,TestDEM,RefDEM,lc,TilesInArea : integer;
   Lat,Long : float64;
   NPts : int64;
   bb : sfBoundBox;


   procedure MoveDEMstoSourceDir;
   var
      i : integer;
      TileSubDirs : tStringList;
   begin
       TileSubDirs := DEMIX_GetListOfTileSubDirsInArea(AreaName);
       if (TileSubDirs.Count = 0) then begin
           {$IfDef RecordDEMIX} WriteLineToDebugFile('Start file movement for ' + AreaName); {$EndIf};
           if not(ValidPath(SourceDir)) then begin
              //to make it easier to download data, it does not have to be manually put into SourceDir, just into the AreaDir
              //only works if the files are close to the 10x10 km target size for tiles
              MakeAreaDirectories(MDDef.DEMIX_BaseDir,AreaName);

              theFiles := TstringList.Create;
              Petmar.FindMatchingFiles(AreaDir,'*.tif',TheFiles,1);
              if (theFiles.Count > 0) then begin
                 SafeMakeDir(SourceDir);
                 for i := 0 to pred(TheFiles.Count) do begin
                    Petmar.MoveFile(theFiles.Strings[i],SourceDir + ExtractFileName(theFiles.Strings[i]));
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


   procedure GetLandCover;
   var
      i : integer;
      TileSubDirs : tStringList;
   begin
       TileSubDirs := DEMIX_GetListOfTileSubDirsInArea(AreaName);
       for i := 0 to pred(TileSubDirs.Count) do begin
          TestDir := MDDef.DEMIX_BaseDir + AreaName + '\' + TileSubDirs.Strings[i] + '\';
          LCname := TestDir + 'landcover.tif';
          if not FileExists(LCname) then begin
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
                        //LCavailable := false;
                        DEMglb[SourceDEM].DEMCenterPoint(Lat,Long);
                        TStr := 'Land cover not available for ' + AreaName + ' ' + LatLongDegreeToString(Lat,Long) + '  ' + GetLC10_fileName(Lat,Long);
                        {$IfDef RecordDEMIX} HighLightLineToDebugFile(TStr); {$EndIf};
                        MissingLandCover.Add(TStr);
                     end;
                    {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Finished land cover: ' + UTM_Tile); {$EndIf};
                 end;
             end;
          end;
       end;
       TileSubDirs.Destroy;
   end;


begin {procedure UTMbasedtilescreation}
   {$IfDef RecordDEMIX} WriteLineToDebugFile('UTMbasedtilescreation Start: ' + AreaName); {$EndIf};
   HeavyDutyProcessing := true;
   NoDBPlots := true;
   theDEMs := GetListOfTestDEMsinUse;

   AreaDir := MDDef.DEMIX_BaseDir + AreaName + '\' ;
   SourceDir := MDDef.DEMIX_BaseDir + AreaName + '\source\' ;
   WGS_EGMdir := MDDef.DEMIX_BaseDir + AreaName + '\wgs_egm\';

   MoveDEMstoSourceDir;

   if ValidPath(AreaDir + 'source\') then begin
      theFiles := TstringList.Create;
      Petmar.FindMatchingFiles(AreaDir + 'source\','*.tif',TheFiles,1);
      MissingLandCover := tStringList.Create;
      if DEMIX_SpecialCase(AreaName) then begin
         TilesInArea := 1;
      end
      else begin
         TilesInArea := Thefiles.Count;
      end;
      PerformDatumShift(AreaName,TilesInArea,TheFiles);
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
               {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Missing ' + SourceEGMName); {$EndIf};
            end;
         end;
         TileSubDirs.Destroy;
      end;

      GetLandCover;

      //CloseAllMaps;
      CloseAllDEMs;
      wmDEM.ClearStatusBarPanelText;
      {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Done ' + UTM_Tile + '  open DEMs=' + IntToStr(NumDEMDataSetsOpen) + '  open maps=' +  IntToStr(NumOpenMaps)); {$EndIf}
end {procedure UTMbasedtilescreation};



procedure DEMIX_tile_statistics(AreaName : shortstring);
//Option = udTileStats
var
   TileStats,{theAreas,theTiles,}TileList : tStringList;
   TileDir,TileStatsFName : PathStr;
   Country,DEMIX_tile : shortstring;
   RefDTM,LC, k : integer;


         procedure InitializeStringLists;
         const
            TileStatsString = ',COUNTRY,LAT,LONG,GRID_FULL,AVG_ELEV,AVG_SLOPE,AVG_ROUGH,RELIEF,FOREST_PC,URBAN_PC,BARREN_PC,WATER_PC,MIN_ELEV,MAX_ELEV';
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
           UrbanMomentVar,ForestMomentVar,BarrenMomentVar,WaterMomentVar,
           ElevMomentVar,SlopeMomentVar,RoughMomentVar : tMomentVar;
           NPts : integer;
           zs : ^bfarray32;
           Lat,Long : float64;
           aLine : shortstring;
           ForestPC,UrbanPC,BarrenPC,WaterPC,Relief,GridFull : float32;

                  procedure LandCover;
                  var
                     slc,Col,Row : integer;
                     Value : float32;
                  begin
                     ForestPC := -999;
                     UrbanPC := -999;
                     BarrenPC := -999;
                     WaterPC := -999;
                     if ValidDEM(LandCoverGrid) then begin
                         InitializeMomentVar(UrbanMomentVar);
                         InitializeMomentVar(ForestMomentVar);
                         InitializeMomentVar(BarrenMomentVar);
                         InitializeMomentVar(WaterMomentVar);
                         for Col := 0 to pred(DEMglb[LandCoverGrid].DEMheader.NumCol) do begin
                            for Row := 0 to pred(DEMglb[LandCoverGrid].DEMheader.NumRow) do begin
                               slc := SimplifiedLandCoverFromGrid(LandCoverGrid,Col,Row,Value);
                               if (slc <> slcUndefined) then begin
                                   if (slc = slcForest) then inc(ForestMomentVar.Npts)
                                   else if (slc = slcBarren) then inc(BarrenMomentVar.Npts)
                                   else if (slc = slcUrban) then inc(UrbanMomentVar.Npts)
                                   else if (slc = slcWater) then inc(WaterMomentVar.Npts);
                                   inc(NPts);
                               end;
                            end;
                         end;
                         if (Npts > 0) then begin
                            ForestPC := 100 * ForestMomentVar.Npts / NPts;
                            UrbanPC := 100 * UrbanMomentVar.Npts / NPts;
                            BarrenPC := 100 * BarrenMomentVar.Npts / NPts;
                            WaterPC := 100 * WaterMomentVar.Npts / NPts;
                         end;
                     end;
                  end;

         begin
            if ValidDEM(RefDEM) and ValidDEM(LandCoverGrid) then begin
               InitializeMomentVar(ElevMomentVar);
               InitializeMomentVar(SlopeMomentVar);
               InitializeMomentVar(RoughMomentVar);
               NPts := 0;
               New(zs);
               DEMGlb[RefDEM].ElevationMomentsWithArray(DEMGlb[RefDEM].FullDEMGridLimits,ElevMomentVar,zs^);
               DEMGlb[RefDEM].DEMCenterPoint(Lat,Long);
               Relief := ElevMomentVar.Maxz - ElevMomentVar.Minz;
               if (Relief > 1) then begin
                  DEMGlb[RefDEM].SlopeMomentsWithArray(DEMGlb[RefDEM].FullDEMGridLimits,SlopeMomentVar,zs^);
                  DEMGlb[RefDEM].GetRoughnessInLongArray(DEMGlb[RefDEM].FullDEMGridLimits,RoughMomentVar.NPts,zs^);
                  Moment(zs^,RoughMomentVar,msAll);
                  {$IfDef TrackAverageStats} WriteLineToDebugFile(AreaName + '  ' + DEMIXTile +  '  ' + ShortMomentResultsToString(RoughMomentVar));  {$EndIf}
                  LandCover;
                  GridFull := -999;
                  aline := AreaName + ',' + DEMIX_TileNameFromSubDir(DEMIX_Tile) + ',' + Country + ',' + RealToString(Lat,-12,-2) + ',' + RealToString(Long,-12,-2) + ',' +
                     RealToString(GridFull,-6,-2)   + ','  + RealToString(ElevMomentVar.Mean,-12,-2) + ',' +
                     RealToString(SlopeMomentVar.Mean,-12,-2)  + ',' + RealToString(RoughMomentVar.Mean,-12,-2)  + ',' +
                     RealToString(Relief,-12,-2) + ',' + RealToString(ForestPC,-12,-2)  + ',' + RealToString(UrbanPC,-12,-2) + ','
                     + RealToString(BarrenPC,-12,-2) + ',' + RealToString(WaterPC,-12,-2)  + ','  + RealToString(ElevMomentVar.MinZ,-12,-2)  + ','  + RealToString(ElevMomentVar.MaxZ,-12,-2);
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
           {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMIX_tile_statistics missing ' + fName); {$EndIf}
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
var
   TileDir,ResultsFName,ResultsDir,
   //SourceName,
   fName : PathStr;
   UTM_Tile,aDEM,{Mode,}aline,aline2,Criterion,Choice : shortstring;
   TileList,TheDEMs,TheCriteria,FUVresults : tStringList;
   i,j,k,UseRef,{SourceDEM,TestDEM,}RefDEM{x,y{,Col,Row} : integer;
   NPts,Fixed: int64;
   Lat,Long{,Sum} : float64;
   z,zref : float32;
   LCGrid : integer;
   MomentVar : array[1..15] of tMomentVar;
   gl: tGridLimits;
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
                 else if (DEMname = 'GEDTMV1_1') or (DEMname = 'GEDTMV0') then Result := 3
                 else if (DEMname = 'GEDTMV1_2') or (DEMname = 'EDTM') or (DEMname = 'FATHOM') or (DEMname = 'FABDEM') then Result := 1
                 else MessageToContinue('Need to define reference DTM in code for ' + DEMGlb[DEM].AreaName);
                 {$If Defined(RecordRefDTM)} WriteLineToDebugFile('DEM: ' + DEMGlb[DEM].AreaName + '  reference: ' + DEMGlb[Result].AreaName); {$EndIf}
             end;

             procedure ProcessOne(Which : shortstring);
             var
                k : integer;
             begin
                aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + '-999' + ',' + which + '_MEAN';
                for k := 1 to theDEMs.Count do aline := aline + ',' + RealToString(MomentVar[k].Mean,-12,-2);
                fuvResults.Add(aLine);

                aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + '-999' + ',' + which + '_MED';
                for k := 1 to theDEMs.Count do aline := aline + ',' + RealToString(MomentVar[k].Median,-12,-2);
                fuvResults.Add(aLine);

                aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + '-999' + ',' + which + '_RMSE';
                for k := 1 to theDEMs.Count do aline := aline + ',' + RealToString(MomentVar[k].RMSE,-12,-2);
                fuvResults.Add(aLine);

                aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + '-999' + ',' + which + '_LE90';
                for k := 1 to theDEMs.Count do aline := aline + ',' + RealToString(MomentVar[k].LE90,-12,-2);
                fuvResults.Add(aLine);

                aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + '-999' + ',' + which + '_MAE';
                for k := 1 to theDEMs.Count do aline := aline + ',' + RealToString(MomentVar[k].MAE,-12,-2);
                fuvResults.Add(aLine);

                aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + '-999' + ',' + which + '_AVD';
                for k := 1 to theDEMs.Count do aline := aline + ',' + RealToString(MomentVar[k].avg_dev,-12,-2);
                fuvResults.Add(aLine);

                aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + '-999' + ',' + which + '_STD';
                for k := 1 to theDEMs.Count do aline := aline + ',' + RealToString(MomentVar[k].std_dev,-12,-2);
                fuvResults.Add(aLine);
             end;


            procedure GetDifferenceDistributionStatistics;
            var
               k,Col,Row,xoffset,yoffset : integer;
               zs : ^bfarray32;
               SlopeAsp1,SlopeAsp2 : tSlopeAspectRec;
               Ruff1,Ruff2 : float32;
               SameGrid : boolean;
            begin
               New(zs);
                if DEMIX_NoVerticalShift(AreaName) then begin
                   //not valid becasue unable to shift reference to EGM2008
                end
                else begin
                    wmDEM.SetPanelText(3,'ELVD',true);
                    for k := 1 to theDEMs.Count do begin
                       UseRef := RefDEMs[GetRefDTM(TestDEMs[k])];
                       InitializeMomentVar(MomentVar[k]);
                       SameGrid := DEMGlb[UseRef].SecondGridJustOffset(TestDEMs[k],xoffset,yoffset);

                       for Col := 0 to pred(DEMglb[UseRef].DEMheader.NumCol) do begin
                          for Row := 0 to pred(DEMglb[UseRef].DEMheader.NumRow) do begin
                             if DEMGlb[UseRef].GetElevMetersOnGrid(col,row,zref) and DEMGlb[TestDEMs[k]].GetElevMetersOnGrid(Col+xoffset,Row+yoffset,z) then begin
                                inc(MomentVar[k].NPts);
                                zs^[MomentVar[k].NPts] := z-zref;
                             end;
                          end;
                       end;
                       moment(zs^,MomentVar[k],msIncludeLE90);
                    end;
                    ProcessOne('ELVD');
                end;

                wmDEM.SetPanelText(3,'SLPD',true);
                for k := 1 to theDEMs.Count do begin
                   UseRef := RefDEMs[GetRefDTM(TestDEMs[k])];
                   InitializeMomentVar(MomentVar[k]);
                   SameGrid := DEMGlb[UseRef].SecondGridJustOffset(TestDEMs[k],xoffset,yoffset);
                   for Col := 0 to pred(DEMglb[UseRef].DEMheader.NumCol) do begin
                      for Row := 0 to pred(DEMglb[UseRef].DEMheader.NumRow) do begin
                         if DEMGlb[UseRef].QuickEvansSlopeAndAspect(Col,Row,SlopeAsp1) and DEMGlb[TestDEMs[k]].QuickEvansSlopeAndAspect(Col+xoffset,Row+yoffset,SlopeAsp2) then begin
                            inc(MomentVar[k].NPts);
                            zs^[MomentVar[k].NPts] := SlopeAsp2.SlopePercent - SlopeAsp1.SlopePercent;
                         end;
                      end;
                   end;
                   moment(zs^,MomentVar[k],msIncludeLE90);
                end;
                ProcessOne('SLPD');

                wmDEM.SetPanelText(3,'RUFD',true);
                for k := 1 to theDEMs.Count do begin
                   UseRef := RefDEMs[GetRefDTM(TestDEMs[k])];
                   InitializeMomentVar(MomentVar[k]);
                    SameGrid := DEMGlb[UseRef].SecondGridJustOffset(TestDEMs[k],xoffset,yoffset);
                   for Col := 0 to pred(DEMglb[UseRef].DEMheader.NumCol) do begin
                      for Row := 0 to pred(DEMglb[UseRef].DEMheader.NumRow) do begin
                         if DEMGlb[UseRef].RoughnessFromSlopeSTD(Col,Row,5,ruff1) and DEMGlb[TestDEMs[k]].RoughnessFromSlopeSTD(Col+xoffset,Row+yoffset,5,Ruff2) then begin
                            inc(MomentVar[k].NPts);
                            zs^[MomentVar[k].NPts] := ruff2-ruff1;
                         end;
                      end;
                   end;
                   moment(zs^,MomentVar[k],msIncludeLE90);
                end;
                ProcessOne('RUFD');
                Dispose(zs);
            end;


          procedure GetFUVStatistics;
          label
             MissingData;
          var
             Crit,k : integer;
             LSPvalue : float64;
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
                    aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + '-999' + ',' + Criterion;
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
                                      {$IfDef DEMIXtrackFUV}
                                          if IsNAN(LSPvalue) then begin
                                              HighLightLineToDebugFile(Criterion + '   LSPvalue=NAN  ' + UTM_Tile + '  ' + AreaName);
                                              WriteLineToDebugFile('Ref:  ' + DEMglb[RefLSPs[UseRef]].FullDEMParams);
                                              WriteLineToDebugFile('Test: ' + DEMglb[TestLSPs[k]].FullDEMParams);
                                              IsNANproblem := true;
                                              LSPvalue := GetFUVForPairGrids(gl,RefLSPs[UseRef],TestLSPs[k],true);
                                          end;
                                      {$EndIf}
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
                    fuvResults.Add(aLine);
                end;
                MissingData:;
             end {for j};
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

   MakeAreaDirectories(MDDef.DEMIX_BaseDir,AreaName);
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
      RefPresent := CheckIfDataPresent('ref_dtm_srtm') and CheckIfDataPresent('ref_dtm_alos') and CheckIfDataPresent('ref_dtm_gedtm');
      if AllGridsPresent and RefPresent then begin
          LoadAndMaskDEMs;
          if (Option in [udPixelGeometrySlope,udAverageTileElevations]) {and (DoAverageSlopeRefDTMs <> nil)} then begin
            wmDEM.SetPanelText(3,'Average tile slope',true);
            aline := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2);
            aline2 := UTM_Tile + ',' + AreaName + ',' + RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2);
            for j := 1 to 5 do begin
               if ValidDEM(RefDEMs[j]) then begin
                  aline := aline + ',' + RealToString(DEMglb[RefDEMs[j]].AverageDEMslope,-8,-2);
                  DEMglb[RefDEMs[j]].ElevationStatistics(DEMglb[RefDEMs[j]].FullDEMGridLimits,Mean,Std,NPts);
                  aline2 := aline2 + ',' + RealToString(Mean,-8,-2);
               end
               else begin
                  aline := aline + ',-9999';
                  aline2 := aline2 + ',-9999';
               end;
            end;
            for j := 1 to theDEMs.Count do begin
                aline := aline + ',' + RealToString(DEMglb[TestDEMs[j]].AverageDEMslope,-8,-2);
                DEMglb[TestDEMs[j]].ElevationStatistics(DEMglb[TestDEMs[j]].FullDEMGridLimits,Mean,Std,NPts);
                aline2 := aline2 + ',' + RealToString(Mean,-8,-2);
            end;
            DoAverageSlopeRefDTMs.Add(aline);
            DoAverageElevRefDTMs.Add(aline2);
          end
          else begin
              if (FUVresults = nil) then begin
                 fuvResults := tStringList.Create;
                 aLine := 'DEMIX_TILE,AREA,LAT,LONG,GRID_FULL,CRITERION';
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

         AreaDir := MDDef.DEMIX_BaseDir + AreaName + '\' ;
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


procedure DEMIX_FileFill(theAreas : tStringList; DEMtoUse : shortstring = 'ref_dtm_srtm.tif');
// Option = udFileFillReport
var
   Report,TileList : tStringList;
   aLine,AreaName,TileName : ShortString;
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
            aline := aline + DEMGlb[DEM].DEM_size_km + ',' + RealToString(Lat,-12,-3) + ',' + RealToString(Long,-12,-3) + ',' + RealToString(MP,-8,-4) + ',' + RealToString(100-Missing,-8,-2)  + ',' +
                 RealToString(100-Missing2,-8,-2) + ',' + RealToString(0.01 * (100-Missing2)*MP,-8,-2)+  ',  ';
            CloseSingleDEM(DEM);
         end
         else Aline := aline + '0,0,0,0,Ref SRTM missing';
         Report.Add(Aline);
      end;
   end;
   fName := MDTempDir + 'tile_voids.dbf';
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
   i,j,k,pfChange,TooBigDelete,DEM,lc,lc2,ALOS,COP,GEDTM,Done,GEDTMref,TestDEM,EGMref : integer;
   AreaName,EGMIssue,TileIssue,aLine : shortstring;
   SourceEGMName,fName,TileDirName,LCname,fName2,AreaLock : PathStr;
   theDEMs, Findings,TileDirs,SourceFiles,EGMfiles,TheFiles : tStringList;
   Lat1,Long1,Lat2,Long2,DistanceMeters,Bearing,Missing : float64;
   bb : sfBoundBox;
    HaveGEDTMref,HaveSRTMref,HaveALOSref,OneBad : boolean;
   //Action: TCloseAction;


       function CheckCentroidDistance(BaseDEM : integer; TestName : shortstring) : boolean;
       var
          fName : PathStr;
          TestDEM,SourceDEM,lc : integer;
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


            procedure MakeOnRef(aDEM : shortstring);
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



begin


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


exit;


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


   WriteLineToDebugFile('Identify problem data sets and start fixing, areas=' + IntToStr(theAreas.Count));
   theDEMs := GetListOfTestDEMsinUse;
   Findings := tStringList.Create;
   aLine := 'AREA,DEMIX_TILE,SOURCE_EGM,GEDTM_REF,SRTM_REF,ALOS_REF,LANDCOVER';
   for i := 0 to pred(theDEMs.Count) do begin
       aline := aline + ',' + theDEMs.Strings[i];
   end;
   Findings.Add(aLine + ',' + 'EVERYTHING');
   Done := 0;
   for i := 0 to pred(theAreas.Count) do begin
      AreaName := theAreas.Strings[i];
      AreaLock := MDDef.DEMIX_BaseDir + AreaName + '\aa_arealocked.txt' ;
      if FileExists(AreaLock) then begin
         WriteLineToDebugFile('Area locked: ' + AreaName);
      end
      else begin
        theAreas.SaveToFile(AreaLock);
        WGS_EGMdir := MDDef.DEMIX_BaseDir + AreaName + '\wgs_egm\';
        wmDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(theAreas.Count) + '  ' + AreaName,true);
        if DEMIX_SpecialCase(AreaName) then begin
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
               HaveGEDTMref := CheckCentroidDistance(DEM,'ref_dtm_gedtm');
               HaveSRTMref := CheckCentroidDistance(DEM,'ref_dtm_srtm');
               HaveALOSref := CheckCentroidDistance(DEM,'ref_dtm_alos');
               CheckCentroidDistance(DEM,'landcover');
               for k := 0 to pred(theDEMs.Count) do begin
                  CheckCentroidDistance(DEM,theDEMs.Strings[k]);
               end;
             end;

             if FixIssues and FileExists(SourceEGMName) then begin
                if (not HaveGEDTMref) or (not HaveSRTMref) or (not HaveALOSref) then begin
                   if FileExists(SourceEGMName) then begin
                       LoadNewDEM(EGMref,SourceEGMName,false);
                       if (not HaveGEDTMref) then MakeOnRef('GEDTMV1_1');
                       if (not HaveSRTMref) then begin
                           MakeOnRef('FATHOM');
                           MakeOnRef('COP');   //makes the high latitude version
                       end;
                       if (not HaveALOSref) then MakeOnRef('ALOS');
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
   WriteLineToDebugFile('Identify gedtm ref DEMs missing or too much missing data');
   NakedMapOptions;
   Done := 0;
   for i := 0 to pred(theAreas.Count) do begin
      if (i mod 10 = 0) then UpdateProgressBar(i/theAreas.Count);
      AreaName := theAreas.Strings[i];
      wmDEM.SetPanelText(1,AreaName,true);
      if not DEMIX_SpecialCase(AreaName) then begin
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


   TheFiles := Nil;
   DEMIX_FileFill(theAreas,'ref_dtm_gedtm.tif');


   //Petmar.FindMatchingFiles(MDDef.DEMIX_BaseDir,'ALOS.tif',TheFiles,4);
   Petmar.FindMatchingFiles(MDDef.DEMIX_BaseDir,'ref_dtm_gedtm.tif',TheFiles,4);
   for i := 0 to pred(theFiles.Count) do begin
       wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(TheFiles.Count),true);
       fName := theFiles.Strings[i];
       DeleteFileIfExists(fName);
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
      if not DEMIX_SpecialCase(AreaName) then begin
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

end;



procedure DEMIX_UTM_based_processing(Option : integer);
var
   fName,AreaLock{,TSfName} : PathStr;
   AreaName,aLine : ShortString;
   theDEMs,MissingLandCover,TheAreas,Report,Report2,theFiles : tStringList;
   I,j,db : Integer;
   CanLimitAreas : boolean;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('DEMIX_UTM_based_processing in, option=' + IntToStr(Option)); {$EndIf}
   {$If Defined(RecordFormResize) or Defined(RecordFormActivate)} WriteLineToDebugFile('Twmdem.FormActivate MDn set, width=' + IntToStr(wmdem.Width) + '  & height=' + IntToStr(wmdem.Height)); {$EndIf}

   GetDEMIXpaths(True);
   theAreas := nil;
   if (Option in [1]) then MissingLandCover := tStringList.Create;

   if (Option in [6,8,12,13,14,21]) then begin
       if (Option in [8,21]) then begin
           MDdef.DEMIX_tile_chars_fname := DEMIX_final_DB_dir + 'UTM_tile_stats_DB_' + CurrentTimeForFileName(false) + '.dbf';
           db := MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_tile_stats'),MDdef.DEMIX_tile_chars_fname);
       end;
       if (Option in [6,21]) then begin
           fName := DEMIX_final_DB_dir + 'UTM_FUV_DB_' + CurrentTimeForFileName(false) + '.dbf';
           db := MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_fuv_results'),fName);
           if ValidDB(db) and (Option = 21) then AddTileCharacteristicsToDB(db);
       end;
       if (Option in [12,21]) then begin
           fName := DEMIX_final_DB_dir + 'UTM_diff_distrib_DB_' + CurrentTimeForFileName(false) + '.dbf';
           db := MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_diff_dist_results'),fName);
           if ValidDB(db) and (Option = 21) then AddTileCharacteristicsToDB(db);
       end;
       if (Option in [13,21]) then begin
           fName := DEMIX_final_DB_dir + 'UTM_curvatures_DEMIX_DB_' + CurrentTimeForFileName(false) + '.dbf';
           db := MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_curvatures_results'),fName);
           if ValidDB(db) and (Option = 21) then AddTileCharacteristicsToDB(db);
       end;
       if (Option in [14,21]) then begin
           fName := DEMIX_final_DB_dir + 'UTM_partials_DEMIX_DB_' + CurrentTimeForFileName(false) + '.dbf';
           db := MergeMultipleCSVorTextFiles(nil,GetFUVresultsSpreadAcrossHardDrives('aa_partials_results'),fName);
           if ValidDB(db) and (Option = 21) then AddTileCharacteristicsToDB(db);
       end;
   end
   else begin
      CanLimitAreas := not (Option in [3,udCSVReportByArea]);
      theAreas := DEMIX_AreasWanted(CanLimitAreas);
      if TheAreas.Count > 0 then begin

          {$IfDef RecordDEMIX} WriteLineToDebugFile('Selected areas=' + IntToStr(theAreas.Count)); {$EndIf}
          if (Option in [udFileFillReport]) then DEMIX_FileFill(theAreas)
          else if Option in [udInvalidTiles] then TakeCareOfInvalidTiles(theAreas)
          else if (Option in [3,udCSVReportByArea]) then DEMIX_UTM_AreaTileSurvey(theAreas)
          else if (Option in [udDeleteResultsCSVsForAreas]) then begin
             DEMIX_SelectivelyDeleteCSVresults(theAreas);
             if AnswerIsYes('Reprocess CSV files') then Option := 20;
          end;
          //begin
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
                AreaLock := MDDef.DEMIX_BaseDir + AreaName + '\aa_arealocked.txt' ;
                wmDEM.SetPanelText(1,AreaProgress,true);
                if Option = udClearAreaLocks then begin
                   DeleteFileIfExists(AreaLock);
                end
                else if not FileExists(AreaLock) then begin
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
         //end;
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
   {$IfDef RecordDEMIX} WriteLineToDebugFile('Twmdem.UTMbasedtilescreation1Click out'); {$EndIf}
end;




