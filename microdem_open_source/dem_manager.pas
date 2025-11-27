unit dem_manager;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef DEBUG}
      //{$Define RecordCloseDEM}
      //{$Define RecordCleanUpTempDir}
      //{$Define TrackHorizontalDatum}
      //{$Define ShortRecordCloseDEM}
      //{$Define RecordClosingData}
      //{$Define RecordNewMaps}
      //{$Define LoadDEMsCovering}
      //{$Define RecordMetadata}
      //{$Define RecordProjects}
      //{$Define RecordDownload}
      //{$Define RecordGet2DEMs}
      //{$Define RecordWhatsOpen}
      //{$Define RecordStartup}
      //{$Define TimeLoadDEM}
      //{$Define RecordEdit}
      //{$Define TimeSatLoad}
      //{$Define RecordMenu}
      //{$Define RecordSatLoad}
      //{$Define RecordSimpleClose}
      //{$Define RecordSatDirOpen}
   {$Else}
      //{$Define TimeLoadDEM}
   {$EndIf}
{$EndIf}


interface

uses
//needed for inline of core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations

    {$IfDef VCL}
       forms,shlobj,vcl.controls,
    {$EndIf}
    sysutils,System.UITypes,StrUtils,Classes,
    Petmar_types,Petmar,DEMMapDraw,DEMDefs;


function OpenAndDisplaySatelliteScene(Files : tStringList; IndexFileName : PathStr; DisplayIt,NeedHist,ASatImage : boolean;  WhichSat : integer = 0; DEMtoAssociate : integer = 0) : integer;

procedure CloseAllDEMs;
procedure CloseSingleDEM(var DEMtoClose : integer; CloseMap : boolean = true; ResetMenus : boolean = true);

procedure CloseAllWindowsAndData;
procedure CloseEverything;
procedure CloseAllMaps;
procedure CloseSingleVectorMap(var I : integer);
procedure CloseAllDatabases;

procedure SaveMicrodemDesktop;
procedure RestoreSpecifiedDesktop(FName : PathStr);
procedure RestoreMicrodemDesktop(fName : PathStr = ''; CloseAll : boolean = true);

procedure CleanUpTempDirectory(IncudeDirs : boolean = false);

function GetWhatsOpen : tStringList;
procedure OpenDEMsToDebugFile(Why : shortstring);
function GetSatMaskList(ASatImage : boolean) : ANSIString;

function OpenDBString : shortstring;

procedure InitializeDEMsWanted(var DEMList : tDEMBooleanArray; Setting : boolean);
function DEMListForSingleDEM(CurDEM : integer) : tDEMBooleanArray;
function DEMListForAllOpenDEM: tDEMBooleanArray;
procedure WriteDEMListToDebug(Title : shortString; FileDEMList : tDEMBooleanArray);

procedure MakeDEMSummaryTable;
procedure DEMHeaderTable;

procedure MakeDBwithLC10FileInventory;

{$IfDef ExIndexes}
{$Else}
   const
      MaxCompare = 10;
   var
      CompareDEMIndexes : array[1..MaxCompare] of integer;
      CompareDEMNames : array[1..MaxCompare] of shortstring;

   function LoadDEMsCoveringPoint(Lat,long : float64; LoadMap : boolean = false) : integer;
   function LoadDEMsCoveringBox(bb : sfBoundBox; LoadMap : boolean = false) : integer;
   procedure CloseCompareDEMs;
   procedure InitCompareDEMs;
   function GetCompareNames : integer;
{$EndIf}

   procedure CheckGeoidNames;

   procedure CloseAllImagery;
   procedure CloseSingleSatelliteImage(var j : integer);
   function OpenSatImageFromDirectory(LastSatDir : PathStr) : integer;

   {$IfDef VCL}
      procedure PickAndOpenImagery(ImageType : tImageType);
      procedure PickSatDirToOpen;
   {$EndIf}

{$IfDef ExPointCloud}
{$Else}
    procedure OpenLidarMulti(theDir : PathStr = '');
{$EndIf}

procedure GeotiffMetadata(MDVersion : tMDVersion; fName : PathStr);

function LoadDatumShiftGrids(var LocalToWGS84,WGS84toEGM2008 : integer) : boolean;

{$IfDef VCL}
   function GetMultipleDEMsFromList(TheMessage : shortstring) : tDEMbooleanArray;  overload;
   procedure GetMultipleDEMsFromList(TheMessage : shortstring; var DEMsWanted : tDEMbooleanArray); overload;
   function GetDEM(var DEMWanted : integer; CanCancel : boolean = false; TheMessage : ShortString = '') : boolean;
   function OpenNewDEM(fName : PathStr = ''; LoadMap : boolean = true; WhatFor : shortstring = '') : integer;
   function GetTwoCompatibleGrids(WhatFor : shortString; CheckUnits : boolean; var DEM1,DEM2 : integer; WarnIfIncompatible : boolean = true;  AlwaysAsk : boolean = false) : boolean;

   function GetImage(var ImageWanted : integer; CanCancel : boolean = false; TheMessage : ShortString = '') : boolean;

   function PickMap(WhatFor : shortstring) : integer;
   function PickADifferentMap(WhatFor,ThisMapCaption : shortstring) : integer;
   function PickMaps(aCaption : ShortString) : tStringList;
   function GetListOfDEMs(DEMSwanted : tDEMbooleanArray) : tStringList;

   function EditHeaderRecord(DEM : integer; AllowChangeType : boolean) : boolean;
   procedure ViewHeaderRecord(DEM : integer);
   procedure EditDEMHeader;

   procedure FastRedrawAllMaps;

   function FileExistsErrorMessage(InName : PathStr) : boolean;

   {$IfDef ExAutoOpen}
   {$Else}
      procedure AutoOpenOptions;
   {$EndIf}

{$EndIf}


{$IfDef TrackElevationPointers}
   function DEMArrayElevationPointersDefined(Report : shortstring; DEMsWanted : tDEMbooleanArray) : boolean;
{$EndIf}


function ValidDEMExt(ext : extstr) : boolean;
function ValidImageryExt(ext : extstr) : boolean;

//function GetLC100_fileName(Lat,Long : float32) : PathStr;
function GetLC10_fileName(Lat,Long : float32) : PathStr;

function WebExtractGEDTMorEDTM(aDEM : shortstring;  bb : sfBoundBox; SaveName : PathStr; OpenMap : boolean) : integer;


const
   EGM96_grid : integer = 0;
   EGM2008_grid : integer = 0;
   EGMdiff_grid : integer = 0;

implementation

uses
   {$IfDef VCL}
      Nevadia_Main,
   {$EndIf}

   {$IfDef ExCartography}
   {$Else}
      DEM_cart_proj,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
       DEMEROS,
   {$EndIf}

   {$IfDef ExRedistrict}
   {$Else}
      demredistrict,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}

   {$IfDef VCL}
      petlistf,
      BaseGraf,
      PetEd32,
      DEMMapf,
      PetDBUtils,
      New_DEM_Headerf,
      {$IfDef ExPointCloud}
      {$Else}
         Point_cloud_options,
      {$EndIf}
   {$EndIf}

   {$IfDef ExVegDensity}
   {$Else}
      Veg_density,
   {$EndIf}

   {$IfDef ExSlicer3D}
   {$Else}
      Slicer_3D,
   {$EndIf}

   {$IfDef ExMultiGrid}
   {$Else}
      MultiGrid,
   {$EndIf}

   {$IfDef ExHyperspectral}
   {$Else}
      hyp_display,
   {$EndIf}

   {$IfDef ExPointCloud}
   {$Else}
      lidar_multiple_grid_display,
   {$EndIf}

   {$IfDef ExIndexes}
   {$Else}
      DEM_Indexes,
   {$EndIf}

   {$IfDef ExTools}
   {$Else}
      MD_use_tools,
      gdal_tools,
   {$EndIf}

   toggle_db_use,

   {$IfDef ExDEMIX}
   {$Else}
      demix_definitions,
      DEMIX_Control,
   {$EndIf}

   Make_Tables,
   Map_overlays,
   DEM_NLCD,
   DEMdbTable,
   DEMCoord,
   Compress_form,
   Geotiff,
   Petmar_ini_file,
   PetMath,
   DEMDef_routines,
   DEMhandW,
   BaseMap;


function WebExtractGEDTMorEDTM(aDEM : shortstring; bb : sfBoundBox; SaveName : PathStr; OpenMap : boolean) : integer;
var
   fName : PathStr;
begin
(*
    if (aDEM = 'GEDTMV1_2w') then      fName := 'https://s3.opengeohub.org/global/dtm/v1.2/gedtm_rf_m_30m_s_20060101_20151231_go_epsg.3857.3855_v1.2.tif'
    else if (aDEM = 'GEDTMV1_2f') then fName := 'https://s3.opengeohub.org/global/dtm/v1.2/gedtm_rf_m_30m_s_20060101_20151231_go_epsg.4326.3855_v1.2.tif'
    else if (aDEM = 'GEDTMV1_2') then  fName := 'https://s3.opengeohub.org/global/edtm/gedtm_rf_m_30m_s_20060101_20151231_go_epsg.4326.3855_v1.2.tif'
*)

    if (aDEM = 'GEDTMV1_2') then       fName := 'https://s3.opengeohub.org/global/dtm/v1.2/gedtm_rf_m_30m_s_20060101_20151231_go_epsg.4326.3855_v1.2.tif'
    else if (aDEM = 'GEDTMV1_1') then  fName := 'https://s3.opengeohub.org/global/edtm/gedtm_rf_m_30m_s_20060101_20151231_go_epsg.4326.3855_v20250611.tif'
    else if (aDEM = 'GEDTMV0') then    fName := 'https://s3.opengeohub.org/global/edtm/legendtm_rf_30m_m_s_20000101_20231231_go_epsg.4326_v20250130.tif'
    else if (aDEM = 'EDTM') then       fName := 'https://s3.eu-central-1.wasabisys.com/openlandmap/dtm/dtm.bareearth_ensemble_p10_30m_s_2018_go_epsg4326_v20230221.tif';
    Result := GDAL_WebExtractFromMonsterTIFFforBoundingBox(fName,bb,OpenMap,SaveName,SaveName);
    if ValidDEM(Result) then begin
       //if (aDEM = 'GEDTMV1_2') then DEMGlb[Result].MultiplyGridByConstant(0.1);   //because it lacks any indication it is decimeters
       SaveGEDTMFamilyDEM(Result,SaveName);  //with EGM2008 code added, MD elevation code for meters, and kill ASCII tag 42112
    end
    else MessageToContinue('At it again; download link broken');
end;




{$IfDef TrackElevationPointers}
   function DEMArrayElevationPointersDefined(Report : shortstring; DEMsWanted : tDEMbooleanArray) : boolean;
   var
      j : integer;
   begin
      Result := true;
      for j := 1 to MaxDEMDataSets do begin
         if ValidDEM(j) then begin
            if not DEMglb[j].ElevationStructuresAllocated then Result := false
         end;
      end;
      if Result then begin
         WriteLineToDebugFile(Report + ' DEM structure allocated');
      end
      else begin
         HighlightLineToDebugFile(Report + ' DEM structure not allocated');
      end;
   end;
{$EndIf}


function LandCoverTileBaseName(TileSize : integer; LatFirst : boolean; Lat,Long : float32) : shortstring;
var
   LatStr,LongStr : shortstring;
begin
   if (Long > 0) then begin
      Long := TileSize * trunc(Long / TileSize);
      LongStr := 'E' + RealToString(Long,3,0);
   end
   else begin
      Long := TileSize * trunc((abs(Long) + TileSize) / TileSize);
      LongStr := 'W' + RealToString(Long,3,0);
   end;
   if (Lat >= 0) then begin
      Lat := TileSize * trunc(Lat / TileSize);
      LatStr := 'N' + RealToString(Lat,2,0);
   end
   else begin
      Lat := TileSize * trunc((abs(Lat) + TileSize) / TileSize);
      LatStr := 'S' + RealToString(Lat,2,0);
   end;
   if LatFirst then Result := LatStr + LongStr
   else Result := LongStr + LatStr;
   ReplaceCharacter(Result,' ','0');
end;



function GetLC10_fileName(Lat,Long : float32) : PathStr;
const
   TileSize = 3;
var
   TileName : shortstring;
begin
   TileName := LandCoverTileBaseName(TileSize,True,Lat,Long);
   Result :=  'J:\landcover\esa_world_cover_10m\ESA_WorldCover_10m_2021_v200_' + TileName + '_Map\ESA_WorldCover_10m_2021_v200_' + TileName + '_Map.tif';
   FindDriveWithFile(Result);
end;


procedure MakeDBwithLC10FileInventory;
var
   Location,fName : PathStr;
   TheFiles,Results : tStringList;
   Lat,Long, i : Integer;
begin
   Location := 'J:\landcover\esa_world_cover_10m\';
   FindDriveWithPath(Location);
   TheFiles := Nil;
   FindMatchingFiles(Location,'*.tif',TheFiles,2);
   Results := tStringList.Create;
   Results.Add('LAT,LONG,FILE_NAME');
   for i := 0 to pred(theFiles.Count) do begin
      fName := ExtractFileNameNoExt(theFiles.Strings[i]);
      Lat := StrToInt(Copy(fName,31,2));
      if FName[30] = 'S' then Lat := -Lat;
      Long := StrToInt(Copy(fName,34,3));
      if FName[33] = 'W' then Long := -Long;
      Results.Add(IntToStr(Lat) + ',' + IntToStr(Long) + ',' + fName);
   end;
   fName := MdtempDir + 'LC_10_files_sw_corner.dbf';
   PetDBUtils.StringList2CSVtoDB(Results,fName);
   TheFiles.Destroy;
{
ESA_WorldCover_10m_2021_v200_N57E003_Map.tif
}
end;



function LoadDatumShiftGrids(var LocalToWGS84,WGS84toEGM2008 : integer) : boolean;
begin
   TemporaryNewGeotiff := false;  //so transformation grid will be untiled, uncompressed
   FindDriveWithFile(GeoidWGS84ellipsoidToLocalVDatum);
   LocalToWGS84 := OpenNewDEM(GeoidWGS84ellipsoidToLocalVDatum,false,'WGS84ellipsoid to local geoid');
   TemporaryNewGeotiff := false;  //so transformation grid will be untiled, uncompressed
   FindDriveWithFile(Geoid2008FName);
   WGS84toEGM2008 := OpenNewDEM(Geoid2008FName,false,'WGS84ellipsoid to EGM2008');
   Result := true;
end;


procedure DEMHeaderTable;
const
   NumParams = 28;
   Params : array[0..NumParams] of shortstring = ('DEM_NAME','DEM_USED','PRECISION','SPACE_UNIT','Z_UNITS','HEMISPHERE','NUM_COL','NUM_ROW',
        'PIXEL_IS','H_DATUM','VERT_DATUM','UTM_ZONE','MIN_Z','MAX_Z','X_SPACE','Y_SPACE','SW_CORNER_X','SW_CORNER_Y','WKT','GeoTIIF_3072',
        'GeoTIIF_2048','SPACING_M','DEM_SIZE','PIXEL_GEOMETRY','FILE_SIZE','VOID_PERCENT','GRID_ELEVS','SW_X_SEC','SW_Y_SEC');
var
   i,j,Decs : integer;
   DEMsWanted : tDEMbooleanArray;
   Results : tStringList;
   aLine,TStr : ANSIstring;
   fName : PathStr;
   Missing : float64;
 begin
   {$IfDef Record3d} WriteLineToDebugFile('TMapForm.Selectmultiplegrids1Click in'); {$EndIf}
   DEMsWanted := GetMultipleDEMsFromList('Grids to compare');
   Results := tStringList.Create;
   aLine := 'PARAMETER';
   for i := 1 to MaxDEMDataSets do begin
      if DEMsWanted[i] and ValidDEM(i) then begin
         aLine := aLine + ',' + 'DEM_' + IntToStr(i);
      end;
   end;
   Results.Add(aLine);
   for j := 0 to NumParams do begin
      aline := Params[j];
      for i := 1 to MaxDEMDataSets do begin
         if DEMsWanted[i] and ValidDEM(i) then begin
            if (DEMGlb[i].DEMHeader.DEMUsed = UTMbasedDEM) then Decs := -2 else Decs := -6;
            TStr := '';
            case j of
               0 : TStr := DEMGlb[i].AreaName;
               1 : TStr := DEMGlb[i].DEMmodel;
               2 : TStr := DEMGlb[i].GridPrecisionString;
               3 : TStr := SpacingUnits[DEMGlb[i].DEMHeader.DataSpacing];
               4 : TStr := ElevUnitsAre(DEMGlb[i].DEMHeader.ElevUnits) + ' (' + IntToStr(DEMGlb[i].DEMHeader.ElevUnits) + ')';
               5 : TStr := DEMGlb[i].DEMHeader.LatHemi;
               6 : TStr := IntToStr(DEMGlb[i].DEMHeader.NumCol);
               7 : TStr := IntToStr(DEMGlb[i].DEMHeader.NumRow);
               8 : TStr := RasterPixelIsString(DEMGlb[i].DEMHeader.RasterPixelIsGeoKey1025);
               9 : TStr := DEMGlb[i].DEMHeader.h_DatumCode;
               10 : TStr := VertDatumName(DEMGlb[i].DEMHeader.VerticalCSTypeGeoKey);
               11 : TStr := IntToStr(DEMGlb[i].DEMHeader.UTMZone);
               12 : TStr := RealToString(DEMGlb[i].DEMHeader.MinElev,-12,-4);
               13 : TStr := RealToString(DEMGlb[i].DEMHeader.MaxElev,-12,-4);
               14 : TStr := RealToString(DEMGlb[i].DEMHeader.DEMxSpacing,-12,Decs);
               15 : TStr := RealToString(DEMGlb[i].DEMHeader.DEMySpacing,-12,Decs);
               16 : TStr := RealToString(DEMGlb[i].DEMHeader.SWCornerX,-12,Decs);
               17 : TStr := RealToString(DEMGlb[i].DEMHeader.SWCornerY,-12,Decs);
               18 : begin
                        TStr := DEMGlb[i].DEMHeader.WKTString;
                        if (length(TStr) > 0) then TStr := IntToStr(length(TStr)) + ' chars'
                    end;
               19 : TStr := IntToStr(DEMGlb[i].DEMMapProj.ProjectedCSTypeGeoKey3072);
               20 : TStr := IntToStr(DEMGlb[i].DEMMapProj.GeographicTypeGeoKey2048);
               21 : TStr := DEMGlb[i].HorizontalDEMSpacing(true);
               22 : TStr := DEMGlb[i].DEM_size_km;
               23 : TStr := DEMGlb[i].GridCornerModel;
               24 : TStr := SmartMemorySizeBytes(GetFileSize(DEMGlb[i].DEMfileName));
               25 : TStr := RealToString(DEMglb[i].ComputeMissingDataPercentage(DEMglb[i].FullDEMGridLimits),-12,-2) + '%';
               26 : TStr := IntToStr(DEMglb[i].DEMheader.NumCol * DEMglb[i].DEMheader.NumRow);
               27 : if DEMGlb[i].DEMHeader.DEMUsed = ArcSecDEM then TStr := LongToString(DEMGlb[i].DEMHeader.SWCornerX,DecSeconds);
               28 : if DEMGlb[i].DEMHeader.DEMUsed = ArcSecDEM then TStr := LatToString(DEMGlb[i].DEMHeader.SWCornerY,DecSeconds);

            end;
            aLine := aline + ',' + tStr;
         end;
      end {for i};
      Results.Add(aLine);
   end {for j};
   fName := Petmar.NextFileNumber(MDTempDir, 'Compare_grid_headers_', DefaultDBExt);
   StringList2CSVtoDB(Results,fName);
end;


procedure MakeDEMsummaryTable;
var
   Results : tStringList;
   fName : PathStr;
   i,Decs : integer;
begin
   ShowHourglassCursor;
   Results := tStringList.Create;
   Results.Add('DEM,PIXEL_IS,NOM_CORNER,HORIZ_DATM,VERT_DATUM,LAT_CENT,LONG_CENT,MIN_Z,MAX_Z,NW_CornerX,NW_CornerY,SW_POINTX,SW_POINTY,SW_CornerX,SW_CornerY,' +
      'DX,DY,NUM_COL,NUM_ROW,AVG_X_M,AVG_Y_M,AVG_SP_M,DIAGONAL,VALID_PTS,LAT_LOW,LONG_LOW,LAT_HI,LONG_HI');
   for i := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
      if (DEMGlb[i].DEMheader.DEMUsed = UTMBasedDEM) then Decs := -2 else Decs := -8;
      Results.Add(DEMGlb[i].AreaName + ',' + PixelIsString(DEMGlb[i].DEMheader.RasterPixelIsGeoKey1025) + ',' + DEMGlb[i].GridCornerModel + ',' +
          DEMGlb[i].DEMMapProj.h_DatumCode + ',' + VertDatumName(DEMGlb[i].DEMheader.VerticalCSTypeGeoKey) + ',' +
          RealToString(DEMGlb[i].DEMSWcornerLat + 0.5 * DEMGlb[i].LatSizeMap,-12,-3) + ',' +
          RealToString(DEMGlb[i].DEMSWcornerLong + 0.5 * DEMGlb[i].LongSizeMap,-12,-3)  + ',' +
          RealToString(DEMGlb[i].DEMheader.MinElev,-12,2)  + ',' +  RealToString(DEMGlb[i].DEMheader.MaxElev,-12,2)  + ',' +
          RealToString(DEMGlb[i].GeotiffNWCornerX,-12,Decs)  + ',' +RealToString(DEMGlb[i].GeotiffNWCornerY,-12,Decs)  + ',' +
          RealToString(DEMGlb[i].CentroidSWCornerX,-12,Decs)  + ',' +RealToString(DEMGlb[i].CentroidSWCornerY,-12,Decs)  + ',' +
          RealToString(DEMGlb[i].DEMheader.SWCornerX,-12,Decs)  + ',' +RealToString(DEMGlb[i].DEMheader.SWCornerY,-12,Decs)  + ',' +
          RealToString(DEMGlb[i].DEMheader.DEMxSpacing,-12,Decs) + ',' + RealToString(DEMGlb[i].DEMheader.DEMySpacing,-12,Decs)  + ',' +
          IntToStr(DEMGlb[i].DEMheader.NumCol) + ',' + IntToStr(DEMGlb[i].DEMheader.NumRow) + ',' +
          RealToString(DEMGlb[i].AverageXSpace,-12,-2) + ',' + RealToString(DEMGlb[i].AverageYSpace,-12,-2)  + ',' + RealToString(DEMGlb[i].AverageSpace,-12,-2) + ',' +
          RealToString(Petmath.HeadingOfLine(DEMGlb[i].AverageXSpace,DEMGlb[i].AverageYSpace),-8,-1) + ',' +
          IntToStr(DEMGlb[i].ComputeNumberValidPoints(DEMGlb[i].FullDEMGridLimits)) + ',' + RealToString(DEMGlb[i].DEMBoundBoxGeo.Ymin,-12,-3) + ',' +
          RealToString(DEMGlb[i].DEMBoundBoxGeo.Xmin,-12,-3) + ',' + RealToString(DEMGlb[i].DEMBoundBoxGeo.Ymax,-12,-3) + ',' + RealToString(DEMGlb[i].DEMBoundBoxGeo.Xmax,-12,-3)  );
   end;
   fName := Petmar.NextFileNumber(MDTempDir,'dem_summary_','.dbf');
   StringList2CSVtoDB(Results,fName);
   ShowDefaultCursor;
end;


procedure CheckGeoidNames;
begin
   if not FileExists(Geoid2008FName) then begin
      Geoid2008FName := 'J:\gis_software\OSGeo4W\share\proj\us_nga_egm08_25.tif';
      if not FileExists(Geoid2008FName) then Geoid2008FName := MainMapData + 'geoid\egm2008-5.tif';
   end;
   if not FileExists(Geoid96FName) then begin
      Geoid96FName := MainMapData + 'geoid\us_nga_egm96_15.tif';
      if not FileExists(Geoid96FName) then Geoid96FName := MainMapData + 'geoid\egm96-5-idl.tif';
   end;
   GeoidDiffFName := MainMapData + 'geoid\egm96_to_egm2008.tif';
   if not FileExists(GeoidWGS84ellipsoidToLocalVDatum) then begin
      GeoidWGS84ellipsoidToLocalVDatum := 'J:\gis_software\OSGeo4W\share\proj\us_noaa_g2012bu0.tif';
   end;
end;


procedure InitializeDEMsWanted(var DEMList : tDEMBooleanArray; Setting : boolean);
var
   j : integer;
begin
   for j := 1 to MaxDEMDataSets do begin
      if ValidDEM(j) then DEMlist[j] := Setting
      else DEMlist[j] := false;
   end;
end;


function DEMListForAllOpenDEM : tDEMBooleanArray;
var
   j : integer;
begin
   for j := 1 to MaxDEMDataSets do Result[j] := ValidDEM(j);
end;

function DEMListForSingleDEM(CurDEM : integer) : tDEMBooleanArray;
begin
   InitializeDEMsWanted(Result,false);
   Result[CurDEM] := true;
end;


procedure WriteDEMListToDebug(Title : shortString; FileDEMList : tDEMBooleanArray);
var
   j : integer;
begin
   HighlightLineToDebugFile(Title);
   for j := 1 to MaxDEMDataSets do begin
      if FileDEMlist[j] then begin
         if ValidDEM(j) then WriteLineToDebugFile(IntegerToString(j,3) + '  ' + DEMglb[j].AreaName)
         else WriteLineToDebugFile(IntegerToString(j,3) + 'in list but not valid DEM or grid');
      end;
   end;
end;


function ValidDEMExt(ext : extstr) : boolean;
begin
   Result := ExtEquals(Ext, '.TIFF') or ExtEquals(Ext, '.TIF') or ExtEquals(Ext, '.DEM') or ExtEquals(Ext, '.FLT') or
             ExtEquals(Ext, '.BIL')  or ExtEquals(Ext, '.DT1') or ExtEquals(Ext, '.DT2') or ExtEquals(Ext, '.DT0') or
             ExtEquals(Ext, '.ASC')  or ExtEquals(Ext, '.PRU') or ExtEquals(Ext, '.NTF') or ExtEquals(Ext, '.HGT');
end;


function ValidImageryExt(ext : extstr) : boolean;
begin
   Ext := UpperCase(Ext);
   Result := ExtEquals(Ext, '.TIFF') or ExtEquals(Ext, '.TIF') or ExtEquals(Ext, '.SID') or ExtEquals(Ext, '.NTF');
end;


function FileExistsErrorMessage(InName : PathStr) : boolean;
begin
   Result := FileExists(InName);
   if Not Result then MessageToContinue('File missing: ' + InName);
end;


function OpenAndDisplaySatelliteScene(Files : tStringList; IndexFileName : PathStr; DisplayIt,NeedHist,ASatImage : boolean;  WhichSat : integer = 0; DEMtoAssociate : integer = 0) : integer;
var
   Masks   : ANSIString;
   Dir     : DirStr;
   bName   : NameStr;
   Ext     : ExtStr;
   TStr : shortstring;
   OpenSatView : tSatView;
   mt : integer;
   Success : boolean;
begin
   {$If Defined(BasicOpens) or Defined(RecordSatLoad) or Defined(TimeSatLoad)} WriteLineToDebugFile('OpenAndDisplayNewScene in ' + IndexFileName); {$EndIf}
   ShowHourglassCursor;
   if (Files = Nil) then begin
      if (IndexFileName = '') or (not FileExists(IndexFileName)) then begin
         if ASatImage then begin
            {$IfDef RecordSatLoad} WriteLineToDebugFile('ASatImage'); {$EndIf}
            if (IndexFileName = '') then begin
               if FileExists(LastImageName) or ValidPath(LastImageName) then IndexFilename := LastImageName
               else IndexFileName := WriteSatDir;
            end;
            if not GetFileMultipleMask('Satellite image',GetSatMaskList(true),IndexFileName,MDDef.DefaultSatFilter) then exit;
            LastImageName := IndexFilename;
         end
         else begin
            {$IfDef RecordSatLoad} WriteLineToDebugFile('Not ASatImage'); {$EndIf}
            if FileExists(LastScanMapName) then IndexFilename := LastScanMapName
            else IndexFileName := MainMapData;

            Masks := GetSatMaskList(false);
            if not GetFileMultipleMask('digitized map',Masks,IndexFileName,MDDef.DefaultDRGFilter) then exit;
            LastScanMapName := IndexFilename;
         end;
         FSplit(IndexFileName,Dir,BName,Ext);
         Ext := UpperCase(Ext);
         if ExtEquals(Ext,'.TFW') or ExtEquals(Ext, '.TIFW') then begin
            Ext := '.tif';
            IndexFileName := Dir + bName + Ext;
         end;
         if ExtEquals(Ext,'.FRQ') or (not ValidImageryExt(Ext)) then begin
            MessageToContinue('Invalid file selected');
            exit;
         end;
      end;
      {$If Defined(RecordSatLoad)} WriteLineToDebugFile('OpenAndDisplayNewScene fName=' + IndexFileName); {$EndIf}
      InsureFileIsNotReadOnly(IndexFileName);
   end;

   if CheckIfCompressedFile(IndexFileName) then exit;

   inc(NumSatImageOpen);
   if (WhichSat = 0) then begin
      Result := 0;
      repeat
         inc(Result);
      until (SatImage[Result] = Nil) or (Result = MaxSatAllowed);
      if (Result = MaxSatAllowed) and (SatImage[Result] <> Nil) then CloseAllImagery;
   end
   else Result := WhichSat;

   {$If Defined(RecordSatLoad) or Defined(TimeSatLoad)} WriteLineToDebugFile('Loading satellite image: ' + IntToStr(Result) + '  ' + IndexFileName); {$EndIf}

   SatImage[Result] := tSatImage.Create(OpenSatView,Files,IndexFileName,NeedHist,Success);

   {$IfDef RecordSatLoad} WriteLineToDebugFile('Back to OpenAndDisplayNewScene, result=' + IntToStr(Result)); {$EndIf}

   if (Result <> 0) and Success then begin
      SatImage[Result].AnImageMap := ASatImage;
      if (not ASatImage) then SatImage[Result].CanEnhance := false;

      if DisplayIt then begin
         {$If Defined(RecordSatLoad) or Defined(TimeSatLoad)} WriteLineToDebugFile('Sat display starting, registration=' + SatImage[Result].RegInfo); {$EndIf}
         if (SatImage[Result].RegVars.Registration <> RegNone) then begin
            if (SatImage[Result].RegVars.Registration in [RegProjection]) or ((not SatImage[Result].CanEnhance)) then begin
               {$IfDef RecordSatLoad} WriteLineToDebugFile('Call CreateNewSatWindow'); {$EndIf}

               if (SatImage[Result].LandsatNumber <> 0) or SatImage[Result].LandsatLook then TStr := ShortLandsatName(SatImage[Result].SceneBaseName)
               else if SatImage[Result].IsSentinel2 then begin
                  TStr := Copy(SatImage[Result].SceneBaseName,1,6) + '_' + Copy(SatImage[Result].SceneBaseName,8,4) + '-' + Copy(SatImage[Result].SceneBaseName,12,2) + '-' + Copy(SatImage[Result].SceneBaseName,14,2);
               end
               else TStr := SatImage[Result].SceneBaseName;

               if (SatImage[Result].NumBands < 3) then mt := mtSatImageGray
               else begin
                  if SatImage[Result].CanEnhance then begin
                     if MDDef.SatMultiBandTrueColor then mt := mtSatTrueColor else mt := mtSatFalseColor;
                  end
                  else mt := mtUnenhancedRGB;
               end;
               CreateNewSatWindow(OpenSatView,SatImage[Result].SelectionMap,Result,mt,{'Image: ' +} TStr,true,DEMToAssociate);
            end;
         end
         else begin
            MessageToContinue('Unregistered image; no map loaded');
         end;
         if MDDef.USOutlinesOnImagery then AddOverlay(SatImage[Result].SelectionMap,ovoUSOUtlines);
         SatImage[Result].SelectionMap.DoFastMapRedraw;
         {$If Defined(BasicOpens) or Defined(RecordSatLoad) or Defined(TimeSatLoad)} WriteLineToDebugFile('Sat display complete ' + SatImage[Result].SelectionMap.Caption); {$EndIf}
      end;
   end
   else begin
      if ReportErrors then MessageToContinue(SatImage[Result].LoadErrorMessage);
      SatImage[Result].Destroy;
      SatImage[Result] := Nil;
      Result := 0;
   end;
   StopSplashing;
   wmDEM.SetMenusForVersion;
end;


procedure GeotiffMetadata(MDVersion : tMDVersion; fName : PathStr);
var
   success : boolean;
   TiffImage : tTIFFImage;
   cmd : shortstring;
   OutName : PathStr;
begin
   if FileExists(fName) or GetFileFromDirectory('GeoTiff file','*.TIF;*.TIFF',FName) then begin
      {$IfDef RecordMetadata} WriteLineToDebugFile('GeotiffMetadata for ' + fName); {$EndIf}
      if (MDversion= mdMicrodem) then begin
         {$IfDef RecordMetadata} WriteLineToDebugFile('MICRODEM GeotiffMetadata for ' + fName); {$EndIf}
         TiffImage := tTiffImage.CreateGeotiff(true,false,fName,Success,true,false);
         {$IfDef RecordMetadata} WriteLineToDebugFile('tTiffImage.CreateGeotiff completed'); {$EndIf}
         TiffImage.Destroy;
      end
      else if (MDversion= mdWhiteBox) then WBT_GeotiffMetadata(fName)
      else if (MDversion= mdListGeo) then begin
          OutName := MDTempDir + extractFileNameNoExt(fname) + '_metadata.txt';
          cmd := ProgramRootDir + 'listgeo\listgeo.exe ' + fname + ' > ' + Outname;
          WinExecAndWait32(cmd);
          ShowInNotepadPlusPlus(OutName,ExtractFileName(OutName));
      end
      else if (MDversion= mdGDAL) then GDALGeotiffToWKT(fName);
      {$IfDef RecordMetadata} WriteLineToDebugFile('GeotiffMetadata out'); {$EndIf}
   end;
end;



{$IfDef ExIndexes}
{$Else}

   function GetCompareNames : integer;
   var
      Table : tMyData;
      fName : PathStr;
   begin
      CloseCompareDEMs;
      fName := SeriesIndexFileName;  //must pass var parameter
      Table := tMyData.Create(fName);
      Table.ApplyFilter('DATA_TYPE=' + QuotedStr('DEMS') + ' AND USE=' + QuotedStr('Y'));
      Result := 0;
      while not Table.eof do begin
         inc(Result);
         CompareDEMNames[Result] := Table.GetFieldByNameAsString('SERIES');
         Table.Next;
         if (Result=MaxCompare) then begin
            if not Table.EOF then MessageToContinue('Could not load all series');
            break;
         end;
      end;
      Table.Destroy;
      {$IfDef LoadDEMsCovering} WriteLineToDebugFile('GetCompareName, filter=' + Table.Filter + 'found=' + IntToStr(Result)); {$EndIf}
   end;


   function LoadDEMsCoveringBox(bb : sfBoundBox; LoadMap : boolean = false) : integer;
   var
      Table : tMyData;
      fName : PathStr;
      i : integer;
   begin
      Result := 0;
      if FileExists(SeriesIndexFileName) then begin
         bb.XMin := bb.XMin + 0.001;
         bb.YMin := bb.YMin + 0.001;
         bb.XMax := bb.XMax - 0.001;
         bb.YMax := bb.YMax - 0.001;

         SaveBackupDefaults;
         fName := SeriesIndexFileName;
         Table := tMyData.Create(fName);
         Table.ApplyFilter('DATA_TYPE=' + QuotedStr('DEMS') + ' AND USE=' + QuotedStr('Y'));
         {$IfDef LoadDEMsCovering} WriteLineToDebugFile('LoadDEMsCoveringBox, filter=' + Table.Filter + '  bbox=' + sfBoundBoxToString(bb)); {$EndIf}
         i := 0;
         while not Table.eof do begin
            inc(i);
            CompareDEMNames[i] := Table.GetFieldByNameAsString('SERIES');
            CompareDEMIndexes[i] := LoadMapLibraryBox(true,bb,CompareDEMNames[i],LoadMap);
            if ValidDEM(CompareDEMIndexes[i]) then begin
               if Table.FieldExists('SHORT_NAME') then CompareDEMNames[i] := Table.GetFieldByNameAsString('SHORT_NAME');
               DEMGlb[CompareDEMIndexes[i]].DEMheader.VerticalCSTypeGeoKey := Table.GetFieldByNameAsInteger('VERT_DATUM');
               {$IfDef LoadDEMsCovering} WriteLineToDebugFile('Series=' + CompareDEMNames[i] +  '   DEM=' + IntToStr(CompareDEMIndexes[i])); {$EndIf}
               inc(Result);
               DEMGlb[CompareDEMIndexes[i]].AreaName := CompareDEMNames[i];
               {$IfDef LoadDEMsCovering} WriteLineToDebugFile(DEMGlb[CompareDEMIndexes[i]].AreaName + ' ' + DEMGlb[CompareDEMIndexes[i]].PixelIsString); {$EndIf}
               if LoadMap then CreateDEMSelectionMap(CompareDEMIndexes[i],true,MDDef.DefElevsPercentile,MDdef.DefElevMap);
            end
            else begin
               dec(i);
            end;
            Table.Next;
          end;
          Table.Destroy;
         {$IfDef LoadDEMsCovering} WriteLineToDebugFile('LoadDEMsCoveringBox, dems loaded=' + IntToStr(Result)); {$EndIf}
         RestoreBackupDefaults;
      end;
    end;


   function LoadDEMsCoveringPoint(Lat,long : float64; LoadMap : boolean = false) : integer;
   var
      bb : sfBoundBox;
   begin
      bb.XMin := Long - 0.01;
      bb.XMax := Long + 0.01;
      bb.YMin := Lat - 0.01;
      bb.YMax := Lat + 0.01;
      Result := LoadDEMsCoveringBox(bb,LoadMap);
   end;


   procedure CloseCompareDEMs;
   var
      i : integer;
   begin
      DEMNowDoing := Calculating;
      for i := 1 to MaxCompare do CloseSingleDEM(CompareDEMIndexes[i]);
      InitCompareDEMs;
   end;

{$EndIf}


{$IfDef ExPointCloud}
{$Else}
   procedure OpenLidarMulti(theDir : PathStr = '');
   var
      i : integer;
      LidarMultipleDisplayForm : TLidarMultipleDisplayForm;
   begin
      i := 1;
      while (lmg[i] <> nil) do  inc(i);

      lmg[i] := tLidarMatchedGrids.Create(i,TheDir);
      lmg[i].lmgIndex := i;
      LidarMultipleDisplayForm := TLidarMultipleDisplayForm.Create(Application);
      LidarMultipleDisplayForm.Caption := lmg[i].MatchName + ' lidar grids';
      LidarMultipleDisplayForm.lmgonmap := lmg[i].lmgindex;
      LidarMultipleDisplayForm.SetGrids;
   end;
{$EndIf}


    procedure CloseSingleVectorMap(var I : integer);
    begin
    {$IfDef VCL}
       if (i > 0) and (i <= MaxVectorMap) and (VectorMap[i] <> Nil) then begin
          VectorMap[i].Close;
          VectorMap[i] := Nil;
          i := 0;
       end;
    {$EndIf}
    end;


procedure CloseSingleDEM(var DEMtoClose : integer; CloseMap : boolean = true; ResetMenus : boolean = true);


      procedure CloseYeDEM(DEMnowClosing : integer);
      begin
         if ValidDEM(DEMnowClosing) then try
            try
              //if DEMGlb[DEMnowClosing].SelectionMap <> Nil then DEMGlb[DEMnowClosing].SelectionMap.Destroy;

              {$IfDef RecordCloseDEM} WriteLineToDebugFile('Destroy DEMGlb=' + IntToStr(DEMnowClosing) + '  ' + DEMGlb[DEMnowClosing].AreaName); {$EndIf}
               DEMGlb[DEMnowClosing].Destroy(CloseMap);
               {$IfDef RecordCloseDEM} WriteLineToDebugFile('Destroy OK for DEMGlb=' + IntToStr(DEMnowClosing)); {$EndIf}
            except
                on Exception do ;
            end
         finally
            {$IfDef RecordCloseDEM} WriteLineToDebugFile('Nil DEMGlb=' + IntToStr(DEMnowClosing)); {$EndIf}
            DEMGlb[DEMnowClosing] := Nil;
         end;
      end;


var
   j : integer;
   CloseString : shortstring;
begin
   try
      ClosingIsHappening := true;
      if ValidDEM(DEMtoClose) then begin
         CloseString := 'CloseSingleDEM, dem=' + IntToStr(DEMtoClose) + '   ' + DEMGlb[DEMtoClose].AreaName;
         {$If Defined(RecordCloseDEM) or Defined(ShortRecordCloseDEM)} WriteLineToDebugFile('In ' + CloseString + '  Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

         if (DEMtoClose = PredAgesDEM) then PredAgesDEM := 0;
         if (DEMtoClose = SedThickDEM) then SedThickDEM := 0;
         if (DEMtoClose = SedTypeDEM) then SedTypeDEM := 0;

         for j := 1 to MaxDEMIXDEM do begin
            if (TestDEMs[j] = DEMtoClose) then TestDEMs[j] := 0;
         end;

         j := DEMtoClose;
         CloseYeDEM(j);
         {$If Defined(RecordCloseDEM) or Defined(ShortRecordCloseDEM)} WriteLineToDebugFile('Back from CloseYeDEM'); {$EndIf}

         DEMtoClose := 0;

         {$If Defined(RecordCloseDEM) or Defined(RecordSimpleClose) or Defined(ShortRecordCloseDEM)} WriteLineToDebugFile('CloseSingleDEM out OK ' + CloseString + '  Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
      end
      else begin
         {$If Defined(RecordCloseDEM) or Defined(ShortRecordCloseDEM)} WriteLineToDebugFile('No DEM to close, i=' + IntToStr(DEMtoClose)); {$EndIf}
      end;
   finally
      ApplicationProcessMessages;
      {$IfDef VCL}
         if ResetMenus and (not SkipMenuUpdating) then WmDem.SetMenusForVersion;
      {$EndIf}
      ClosingIsHappening := false;
   end;
end;


procedure CloseAllDEMs;
var
   i,j : integer;
begin
   {$IfDef RecordClosingData} WriteLineToDebugFile('CloseAllDEMs in'); {$EndIf}
   for i := MaxDEMDataSets downto 1 do begin
      j := i;
      if ValidDEM(j) then begin
         {$IfDef RecordClosingData} WriteLineToDebugFile('Try close DEM ' + IntToStr(j)); {$EndIf}
         CloseSingleDEM(j,true,false);
      end;
      //ApplicationProcessMessages;
   end;
   {$IfDef RecordClosingData} WriteLineToDebugFile('All DEMs now closed'); {$EndIf}
end;


function GetSatMaskList(ASatImage : boolean) : ANSIString;
var
   TStr : ShortString;
begin
    TStr :=  'All files|*.*|GEOTIFF|*.tif;*.tiff|' + 'Imagery with world files|*.jgw;*.tfw;*.tifw;*.gfw;*.pnw;*.sdw;*.bpw|' + 'BMP/JPEG/PNG, 3 pt reg|*.xy|JPEG2000|*.jp2|ECW|*.ecw|IMG|*.img';
    if ASatImage then begin
       Result := '|Landsat Look true color|*T1.TIF|Landsat Look TIR|*TIR.TIF|Likely images|*.tif;*.sid|' + 'Imagery|*.bmp;*.jpg;*.jpeg;*.png;*.gif|' + 'BIP file|*.BIP|';
       Result := TStr + Result;
    end
    else begin
       Result := 'Likely DRG|*.tif|NOAA BSB|*.KAP';
    end;
end;


{$IfDef VCL}

    procedure CloseAllMaps;
    var
       i : integer;
    begin
       {$IfDef RecordClosingData} WriteLineToDebugFile('CloseAllMaps in'); {$EndIf}
       if (WMDEM.MDIChildCount > 0) then begin
          DEMNowDoing := Calculating;
          for i := pred(WMDEM.MDIChildCount) downto 0 do begin
             if WMDEM.MDIChildren[i] is TMapForm then begin
                {$IfDef RecordClosingData} WriteLineToDebugFile('Close ' + (WMDEM.MDIChildren[i] as TMapForm).Caption); {$EndIf}
                (WMDEM.MDIChildren[i] as TMapForm).Close;
             end;
          end;
       end;
    end;

    procedure FastRedrawAllMaps;
    var
       i : integer;
    begin
       if (WMDEM.MDIChildCount > 0) then begin
          DEMNowDoing := Calculating;
          for i := pred(WMDEM.MDIChildCount) downto 0 do
             if WMDEM.MDIChildren[i] is TMapForm then begin
                (WMDEM.MDIChildren[i] as TMapForm).DoFastMapRedraw;
             end;
       end;
    end;

   procedure EditDEMHeader;
   var
      FileName : PathStr;
      ReadDEM  : integer;
   begin
      FileName := '';
      NewArea(false,ReadDEM,'DEM header to edit',FileName);
      if ValidDEM(ReadDEM) then begin
         StopSplashing;
         EditHeaderRecord(ReadDEM,true);
         CloseSingleDEM(ReadDEM);
      end;
   end;


   function EditHeaderRecord(DEM : integer; AllowChangeType : boolean) : boolean;
   var
      EditHeader : TDEMHeaderForm;
   begin
      {$IfDef RecordEdit} WriteLineToDebugFile('EditHeaderRecord in, DEM=' + IntToStr(DEM)); {$EndIf}
      EditHeader := TDEMHeaderForm.Create(Application);
      EditHeader.SetUpDEMHeaderForm(DEM);
      EditHeader.UpDateChoices;
      EditHeader.RadioGroup2.Enabled := AllowChangeType;
      EditHeader.ShowModal;
   end;


   procedure ViewHeaderRecord(DEM : integer);
   var
      EditHeader : TDEMHeaderForm;
      FileName : PathStr;
      ReadDEM  : integer;
   begin
      {$IfDef RecordEdit} WriteLineToDebugFile('ViewHeaderRecord in, DEM=' + IntToStr(DEM)); {$EndIf}
      if (DEM = 0) then begin
         FileName := '';
         NewArea(false,ReadDEM,'DEM header to edit',FileName);
         if ValidDEM(ReadDEM) then begin
            DEM := ReadDEM;
         end
         else exit;
      end;
      EditHeader := TDEMHeaderForm.Create(Application);
      EditHeader.SetUpDEMHeaderForm(DEM);
      EditHeader.DisableEdits;
      EditHeader.ShowModal;
      CloseSingleDEM(ReadDEM);
   end;


   function GetMultipleDEMsFromList(TheMessage : shortstring) : tDEMbooleanArray;  overload;
   var
      i : integer;
   begin
      for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) then Result[i] := true
         else Result[i] := false;
      end;
      GetMultipleDEMsFromList(TheMessage,Result);
   end;


   procedure GetMultipleDEMsFromList(TheMessage : shortstring; var DEMsWanted : tDEMbooleanArray); overload;
   //this allows the initial settings for the DEMs wanted to be set or not set depending on situation
   var
      fName : PathStr;
      table : tMyData;
      i : integer;
      ch : char;
   begin
      {$IfDef ShowToggle} WriteLineToDebugFile('GetMultipleDEMsFromList in'); {$EndIf}
      fName := MDTempDir + 'merge.dbf';
      MakePickUseTable(fName);
      Table := tMyData.Create(fName);
      for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) and (not DEMGlb[i].HiddenGrid) then begin
            Table.Insert;
            Table.SetFieldByNameAsString('MENU_OPTS',IntToStr(i) + '-' + DEMGlb[i].AreaName);
            if DEMsWanted[i] then ch := 'Y' else ch := 'N';
            Table.SetFieldByNameAsString('USE',ch);
         end;
      end;
      VerifyRecordsToUse(Table,'MENU_OPTS',TheMessage);
      {$IfDef ShowToggle} WriteLineToDebugFile('VerifyRecordsToUse success'); {$EndIf}
      Table.First;
      while not Table.eof do begin
         i := StrToInt(Petmar_Types.BeforeSpecifiedString(Table.GetFieldByNameAsString('MENU_OPTS'),'-'));
         DEMsWanted[i] := Table.GetFieldByNameAsString('USE') = 'Y';
         Table.Next;
      end;
      Table.Destroy;
      {$IfDef ShowToggle} WriteLineToDebugFile('GetMultipleDEMsFromList out'); {$EndIf}
   end;


   function OpenNewDEM(fName : PathStr = ''; LoadMap : boolean = true; WhatFor : shortstring = '') : integer;
   var
      FilesWanted : tStringList;
      i : integer;
   begin
      {$If Defined(RecordIniMemoryOverwrite) or Defined(TimeLoadDEM)} IniMemOverwriteCheck('start OpenNewDEM'); {$EndIf}
      Result := 0;
      try
         DEMMergeInProgress := true;   //defined for case of opening multiple DEMs with GetMultiple Files
         if (FName <> '') then begin
           {$If Defined(TimeLoadDEM)} WriteLineToDebugFile('OpenNewDEM passed DEM: ' + fName); {$EndIf}
           LoadNewDEM(Result,fName,LoadMap,'DEM/grid for ' + WhatFor);
         end
         else begin
           {$If Defined(TimeLoadDEM)} WriteLineToDebugFile('OpenNewDEM in'); {$EndIf}
           FilesWanted := tStringList.Create;
           FilesWanted.Add(LastDEMName);
           if GetMultipleFiles('DEM/grid ' + WhatFor,DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then begin
              {$If Defined(TimeLoadDEM))} WriteLineToDebugFile('Files picked ' + IntToStr(FilesWanted.Count)); {$EndIf}
              for i := 0 to pred(FilesWanted.Count) do begin
                 fName := FilesWanted.Strings[i];
                 {$If Defined(TimeLoadDEM))} WriteLineToDebugFile('User pick DEM: ' + IntToStr(i) + '  ' + fName); {$EndIf}
                 ShlObj.SHAddToRecentDocs(SHARD_PATH, PChar(FilesWanted.Strings[i]));
                 LoadNewDEM(Result,fName,LoadMap);
              end;
           end;
         end;
      finally
        {$If Defined(TimeLoadDEM)} WriteLineToDebugFile('OpenNewDEM final cleanup'); {$EndIf}
        LastDEMLoaded := Result;
        DEMMergeInProgress := false;
      end;
      {$If Defined(TimeLoadDEM)} if (Result = 0) then WriteLineToDebugFile('OpenNewDEM fail') else WriteLineToDebugFile('OpenNewDEM out  ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].DEMMapProjection.GetProjName); {$EndIf}
      {$If Defined(TrackHorizontalDatum)}
         WriteLineToDebugFile('exit OpenNewDEM, ' + DEMGlb[Result].AreaName + '  ' +  DEMGlb[Result].DEMMapProjection.h_DatumCode + '  ' +
           StringFromDatumCode(DEMGlb[Result].DEMheader.DigitizeDatum));
      {$EndIf}

      {$IfDef RecordIniMemoryOverwrite} IniMemOverwriteCheck('end OpenNewDEM'); {$EndIf}
   end;
{$EndIf}


{$IfDef ExAutoOpen}
{$Else}
   procedure AutoOpenOptions;
   begin
      StopSplashing;
      if (MDdef.AutoOpen = aoProject)then begin
         {$If Defined(RecordStartup) or Defined(RecordProjects)} WriteLineToDebugFile('Auto restore desktop ' + LastDesktop); {$EndIf}
         RestoreSpecifiedDesktop(LastDesktop);
      end
      else if (MDDef.AutoOpen = aoDEM)then begin
         {$If Defined(RecordStartup) or Defined(RecordProjects)} WriteLineToDebugFile('MDdef.AutoOpen = aoDEM ' + LastDEMName); {$EndIf}
         OpenNewDEM(LastDEMName);
      end
      {$IfDef ExMultiGrid}
      {$Else}
         else if (MDdef.AutoOpen = aoMultigrid) then begin
            {$If Defined(RecordStartup) or Defined(RecordProjects)} WriteLineToDebugFile('MDdef.AutoOpen = aoMultigrid'); {$EndIf}
            if (LastMultigrid1 <> '') then OpenTheMultigrid(1,GetParentDirectory(LastMultigrid1));
            if (LastMultigrid2 <> '') then OpenTheMultigrid(2,GetParentDirectory(LastMultigrid2));
            if (LastMultigrid3 <> '') then OpenTheMultigrid(3,GetParentDirectory(LastMultigrid3));
         end
      {$EndIf}
      {$IfDef ExPointCloud}
      {$Else}
         else if (MDdef.AutoOpen = aoLastPointCloud) then begin
            Slicer_3D.DB_3dSlices(Nil,Nil,Nil);
         end
         else if (MDdef.AutoOpen = aoLastLidarMulti) then begin
            OpenLidarMulti(LastLidarMulti);
         end
      {$EndIf}
      {$IfDef ExHyperSpectral}
      {$Else}
         else if MDdef.AutoOpen = aoHyper then begin
            OpenHyperspectralImage(true);
         end
      {$EndIf}
      else if (MDdef.AutoOpen = aoImage) then begin
         {$If Defined(RecordStartup) or Defined(RecordProjects)} WriteLineToDebugFile('MDdef.AutoOpen image=' + LastImageName); {$EndIf}
         OpenAndDisplaySatelliteScene(Nil,LastImageName,true,true,(not GlobalDRGMap));
      end
      else if MDdef.AutoOpen = aoShapeFile then begin
         DEMMapf.LoadBlankVectorMapAndOverlay(false,false,LastDataBase);
      end;
      {$IfDef LoadLastLOS}
         {$If Defined(RecordStartup) or Defined(RecordProjects)} WriteLineToDebugFile('Open Last LOS'); {$EndIf}
         LastSavedLOSfName := ProjectDir + 'last_los' + DefaultDBExt;
         if not FileExists(LastSavedLOSfName) then LastSavedLOSfName := ProjectDir + 'last_los.csv';
         if FileExists(LastSavedLOSfName) and ValidDEM(DEMGlb[1]) then begin
            DEMGlb[1].SelectionMap.LoadLOStopoprofile1Click(Nil);
         end;
     {$EndIf}
   end;
{$EndIf}


{$IfDef ExSat}
{$Else}

      function OpenSatImageFromDirectory(LastSatDir : PathStr) : integer;
      var
         fName : PathStr;
         i : integer;
         TheFiles,Files2 : tStringList;
      begin
         Result := 0;
         if WarnAboutSpaces(LastSatDir) then exit;

         {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('OpenSatImageFromDirectory in, ' + LastSatDir); {$EndIf}
         TheFiles := Nil;
         FindMatchingFiles(LastSatDir,'*.tif',TheFiles,6);
         if (TheFiles.Count = 0) then begin
            {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('found no TIF files'); {$EndIf}
            FreeAndNil(TheFiles);
            FindMatchingFiles(LastSatDir,'*.jp2',TheFiles,8);
            if (TheFiles.Count = 0) then begin
                {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('found no JP2 files'); {$EndIf}
                MessageToContinue('No JP2 files found in directory');
                TheFiles.Free;
                exit;
            end;
            {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('OpenSatImageFromDirectory, jp2 files=' + IntToStr(TheFiles.Count)); {$EndIf}
            for I := pred(TheFiles.Count) downto 0 do begin
               if AnsiContainsText(TheFiles.Strings[i],'IMG_DATA') then begin

               end
               else begin
                  TheFiles.Delete(i);
               end;
            end;
            GDAL_warp_multiple(TheFiles);
            //wmdem.SetPanelText(0,'');
            FindMatchingFiles(LastSatDir,'*.tif',TheFiles,6);
         end;

            //already have TIFFs, but maybe need to delete JP2 not originally deleted
            if MDDef.DeleteJP2 then begin
               {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('check JP2 delete'); {$EndIf}
               Files2 := Nil;
               FindMatchingFiles(LastSatDir,'*.jp2',Files2,8);
               for I := 0 to pred(Files2.Count) do DeleteFile(Files2.Strings[i]);
               Files2.Free;
            end;

            //  3/6/23--originally import of tar Landsat kept the original compreesed tif files
            //    at some point those were recycled, and the checking for them was deleted
            //    this will take care of legacy problem
            Files2 := Nil;
            FindMatchingFiles(LastSatDir,'ORIGINAL_*.tif',Files2,8);
            for I := 0 to pred(Files2.Count) do DeleteFile(Files2.Strings[i]);

         if IsThisSentinel2(TheFiles.Strings[0]) then begin
            //might be metadata or other TIFFS we don't want to open
            for i := pred(TheFiles.Count) downto 0 do begin
               if not AnsiContainsText(TheFiles.Strings[i],'IMG_DATA') then TheFiles.Delete(i);
            end;
         end;

         {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('OpenSatImageFromDirectory, TIF files=' + IntToStr(TheFiles.Count)); {$EndIf}
         if (TheFiles.Count > 0) then begin
            if (TheFiles.Count = 1) then i := 0 else i := 1;      //for Sentinel-2, where band 1 is low resolution
            {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('Open=' + TheFiles.Strings[i]); {$EndIf}
            Result := OpenAndDisplaySatelliteScene(Nil,TheFiles.Strings[i],true,true,true);
         end;
         TheFiles.Free;
         UpdateMenusForAllMaps;
         {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('OpenSatImageFromDirectory out'); {$EndIf}
      end;


      procedure PickSatDirToOpen;
      var
         Paths : tStringList;
         i : integer;
      begin
         {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('PickSatDirToOpen in'); {$EndIf}
         Paths := tStringList.Create;
         Paths.Add(LastSatDir);
         if GetMultipleDirectories('Landsat or Sentinel-2 image',Paths) then begin
            for i := 0 to pred(Paths.Count) do begin
               LastSatDir := Paths[i];
               {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('call OpenSatImageFromDirectory, ' + LastSatDir); {$EndIf}
               OpenSatImageFromDirectory(LastSatDir);
            end;
         end
         else begin
            if AnswerIsYes('Switch to file selection') then begin
               MDDef.ImageryIconDirs := false;
               PickAndOpenImagery(itSat);
            end;
         end;
         Paths.Free;
      end;


   procedure PickAndOpenImagery(ImageType : tImageType);
   var
      TheFiles : tStringList;
      Prompt,Masks : ANSIString;
      Filter : byte;
      LandCover : integer;
      fName : PathStr;
      i,NewSatImage : integer;
   begin
      {$If Defined(RecordSatLoad) or Defined(RecordMenu)} WriteLineToDebugFile('PickAndOpenImagery in'); {$EndIf}
      StopSplashing;
      Masks := GetSatMaskList(not GlobalDRGMap);
      TheFiles := tStringList.Create;
      if (ImageType = itDRG) then  begin
         TheFiles.Add(LastScanMapName);
         Prompt := 'DRG (scanned map)';
         Filter := MDDef.DefaultDRGFilter;
         GlobalDRGMap := true;
      end
      else begin
         TheFiles.Add(LastImageName);
         Prompt := 'Image';
         Filter := MDDef.DefaultSatFilter;
      end;

      {$If Defined(RecordSatLoad) or Defined(RecordMenu) or Defined(TimeSatLoad)} WriteLineToDebugFile('Set to GetMultipleFiles'); {$EndIf}
      if GetMultipleFiles(Prompt,Masks,TheFiles,Filter) then begin
         for I := 0 to pred(TheFiles.Count) do begin
            fName := TheFiles.Strings[i];
            if IsThisLandCover(fName,LandCover) and AnswerIsYes('This appears to be landcover; do you want to open as grid') then begin
                OpenNewDEM(fName);
            end
            else begin
               if StrUtils.AnsiContainsText(Uppercase(fname),'DRG') then begin
                  GlobalDRGMap := true;
                  ImageType := itDRG;
               end;
               ShowHourglassCursor;
               ShlObj.SHAddToRecentDocs(SHARD_PATH, PChar(TheFiles.Strings[i]));
               {$If Defined(RecordSatLoad) or Defined(RecordMenu) or Defined(TimeSatLoad)} WriteLineToDebugFile('call OpenAndDisplay ' + fName); {$EndIf}
               if not CheckFileNameForSpaces(fName) then NewSatImage := OpenAndDisplaySatelliteScene(Nil,fName,true,(ImageType <> itDRG),(not GlobalDRGMap));
            end;
         end;
         if (ImageType = itDRG) then begin
            LastScanMapName := TheFiles.Strings[0];
            GlobalDRGMap := false;
            MDDef.DefaultDRGFilter := Filter;
         end
         else begin
            LastImageName := TheFiles.Strings[0];
            MDDef.DefaultSatFilter := Filter;
         end;
      end
      else begin
         if AnswerIsYes('Switch to directory selection') then begin
            MDDef.ImageryIconDirs := true;
            PickSatDirToOpen;
            MDDef.DefaultSatFilter := Filter;
         end;
      end;
      TheFiles.Free;
      UpdateMenusForAllMaps;
      {$If Defined(RecordSatLoad) or Defined(RecordMenu) or Defined(TimeSatLoad)} WriteLineToDebugFile('PickAndOpenImagery out'); {$EndIf}
   end;

      procedure CloseSingleSatelliteImage(var j : integer);
      begin
         if ValidSatImage(j) then try
            {$IfDef RecordClosingData} WriteLineToDebugFile('CloseSingleSatelliteImage in, j=' + IntToStr(j)); {$EndIf}
            SkipMenuUpdating := true;
            FreeAndNil(SatImage[j]);
         finally
            SkipMenuUpdating := false;
            {$IfDef RecordClosingData} WriteLineToDebugFile('CloseSingleSatelliteImage out'); {$EndIf}
         end;
      end;

      procedure CloseAllImagery;
      var
         i,j : integer;
      begin
         {$IfDef RecordClosingData} WriteLineToDebugFile('Close all imagery in'); {$EndIf}
         DEMNowDoing := Calculating;
         for i := 1 to MaxSatAllowed do begin
            j := i;
            CloseSingleSatelliteImage(j);
         end;
         NumSatImageOpen := 0;
         {$IfDef VCL} wmDEM.SetMenusForVersion; {$EndIf}
         {$IfDef RecordClosingData} WriteLineToDebugFile('Close all imagery out'); {$EndIf}
      end;

      {$IfDef VCL}
         function GetImage(var ImageWanted : integer; CanCancel : boolean = false; TheMessage : ShortString = ''): boolean;
         var
            TheList : tStringList;
            i,Wanted,err  : integer;
            TStr : ShortString;
         begin
            if (not CanCancel) and (NumSatImageOpen = 1) then ImageWanted := 1
            else begin
               ImageWanted := 0;
               Wanted := 0;
               TheList := TStringList.Create;
               for i := 1 to MaxSatAllowed do begin
                  if (SatImage[i] <> Nil) then
                    TheList.Add('Image ' + IntToStr(i) +': ' + SatImage[i].SceneTitle + ' (' + SatImage[i].SceneBaseName + ')');
               end;
               {$IfDef VCL}
                  if (TheList.Count = 1) or MultiSelectSingleColumnStringList('Image for ' + TheMessage,Wanted,TheList,CanCancel) then begin
                     TStr := TheList.Strings[Wanted];
                     Val(Copy(TStr,7,2),ImageWanted,err);
                  end;
               {$EndIf}
               TheList.Free;
            end;
            Result := (ImageWanted <> 0);
         end;
      {$EndIf}
{$EndIf}


procedure CloseAllDatabases;
var
   i,j : integer;
begin
(* removed 3/2/2024, to see if it is causing shutdown problem
   for i := pred(WMDEM.MDIChildCount) downto 0 do
      if (WMDEM.MDIChildren[i] is Tdbtablef) then (WMDEM.MDIChildren[i] as Tdbtablef).Close;
*)

   for i := 1 to MaxDataBase do begin
      j := i;
      if ValidDB(j) then begin
         CloseAndNilNumberedDB(j);
      end;
   end;

   if not ClosingEverything then FastRedrawAllMaps;
end;


procedure CloseEverything;
begin
    {$IfDef RecordClosingData} WriteLineToDebugFile('CloseEverything in'); {$EndIf}
    try
       ClosingEveryThing := true;
       CloseAllDatabases;
       CloseAllMultigrids;
       CloseAllDEMs;
       {$IfDef ExSat}
       {$Else}
          CloseAllImagery;
       {$EndIf}
    finally
       ClosingEveryThing := false
    end;
    {$IfDef RecordClosingData} WriteLineToDebugFile('CloseEverything out'); {$EndIf}
end;


procedure CloseAllWindowsAndData;
{$IfDef VCL}
var
   j,i: integer;
{$EndIf}
begin
   {$IfDef RecordClosingData} WriteLineToDebugFile('DEM_Manager.CloseAllWindowsAndData enter'); {$EndIf}
   CloseEverything;

   {$IfDef VCL}
      if (WmDEM.MDIChildCount > 0) then begin
         for j := pred(WmDEM.MDIChildCount) downto 0 do begin
            {$IfDef RecordClosingData} WriteLineToDebugFile('Try to close window: '+  WmDEM.MDIChildren[j].Caption); {$EndIf}
            try
               WmDEM.MDIChildren[j].Close;
            except
               on E: Exception do i := -1;
            end;
         end;
      end;
      if (WMDEM <> Nil) then WMDEM.SetMenusForVersion;
      {$IfDef RecordClosingData} WriteLineToDebugFile('all child windows closed'); {$EndIf}
   {$EndIf}
   {$IfDef RecordClosingData} WriteLineToDebugFile('DEM_Manager.CloseAllWindowsAndData out'); {$EndIf}
end;


{$IfDef VCL}

      function GetTwoCompatibleGrids(WhatFor : shortString; CheckUnits : boolean; var DEM1,DEM2 : integer; WarnIfIncompatible : boolean = true;  AlwaysAsk : boolean = false) : boolean;
      //var
         //xoffset,yoffset : integer;
      begin
         if (NumDEMDataSetsOpen < 2) then begin
            Result := false;
            DEM1 := 0;
            DEM2 := 0;
         end
         else begin
            {$IfDef RecordGet2DEMs} WriteLineToDebugFile('GetTwoCompatibleGrids'); {$EndIf}
            DEM1 := 1;
            DEM2 := 2;
            repeat
               if AlwaysAsk or (NumDEMDataSetsOpen > 2) or (DEMGlb[1] = Nil) or (DEMGlb[2] = Nil) then begin
                  GetDEM(DEM1,false,'First grid (' + WhatFor + ')');
                  GetDEM(DEM2,false,'Second grid (' + WhatFor + ')');
               end;
               if (DEM1 = DEM2) then begin
                  if not AnswerIsYes('Cannot pick same grid for both; retry') then begin
                     Result := false;
                     exit;
                  end
               end;
            until (DEM1 <> DEM2);
            //Result := DEMGlb[DEM1].SecondGridJustOffset(DEM2,xoffset,yoffset);
            Result := DEMGlb[DEM1].SecondGridIdentical(DEM2);
            {$IfDef RecordGet2DEMs} WriteLineToDebugFile('Identical grids=' + TrueOrFalse(Result)); {$EndIf}
            if CheckUnits then Result := Result and (DEMGlb[DEM1].DEMheader.ElevUnits = DEMGlb[DEM2].DEMheader.ElevUnits);
            if WarnIfIncompatible and (not Result) then MessageToContinue('Incompatible grids');
            {$IfDef RecordGet2DEMs} WriteLineToDebugFile('Out of GetTwoCompatibleGrids=' + TrueOrFalse(Result)); {$EndIf}
         end;
      end;


      function GetDEM(var DEMWanted : integer; CanCancel : boolean = false; TheMessage : ShortString = '') : boolean;
      var
         i,err : integer;
         TheList : tStringList;
      begin
         DEMWanted := 0;
         TheList := TStringList.Create;
         for i := 1 to MaxDEMDataSets do if (ValidDEM(i)) and (not DEMGlb[i].HiddenGrid) then
            TheList.Add('DEM' + IntegerToString(i,4) +': ' + DEMGlb[i].AreaName);
         if (TheList.Count = 1) or MultiSelectSingleColumnStringList('DEM for ' + TheMessage,DEMWanted,TheList,CanCancel) then begin
            Val(Copy(theList.Strings[DEMWanted],5,3),DEMWanted,err);
         end;
         TheList.Free;
         Result := (DEMWanted <> 0);
      end;


      function PickMap(WhatFor : shortstring) : integer;
      var
         Maps : tStringList;
         i : integer;
      begin
         Maps := tStringList.Create;
         for i := 0 to pred(WMDEM.MDIChildCount) do
            if (WMDEM.MDIChildren[i] is tMapForm) then Maps.Add(WMDEM.MDIChildren[i].Caption);
         Result := 0;
         Petmar.MultiSelectSingleColumnStringList(WhatFor,Result,Maps);
      end;


      function PickMaps(aCaption : ShortString) : tStringList;
      var
         i : integer;
      begin
         Result := tStringList.Create;
         for i := 0 to pred(WMDEM.MDIChildCount) do
            if (WMDEM.MDIChildren[i] is tMapForm) then Result.Add(WMDEM.MDIChildren[i].Caption);
         PickSomeFromStringList(Result, aCaption);
      end;

      function GetListOfDEMs(DEMSwanted : tDEMbooleanArray) : tStringList;
      var
         i : integer;
      begin
         Result := tStringList.Create;
         for i := 1 to MaxDEMDataSets do
            if DEMsWanted[i] then Result.Add(DEMGlb[i].SelectionMap.Caption);
      end;


      function PickADifferentMap(WhatFor,ThisMapCaption : shortstring) : integer;
      var
         Maps : tStringList;
         i,j : integer;
      begin
         Maps := tStringList.Create;
         for i := 0 to pred(WMDEM.MDIChildCount) do
            if (WMDEM.MDIChildren[i] is tMapForm) and (WMDEM.MDIChildren[i].Caption <> ThisMapCaption) then Maps.Add(WMDEM.MDIChildren[i].Caption);
         Result := 0;

         if (Maps.Count = 1) then begin
            for i := 0 to pred(WMDEM.MDIChildCount) do
               if (WMDEM.MDIChildren[i] is tMapForm) and (WMDEM.MDIChildren[i].Caption = Maps.Strings[0]) then Result := i;
         end
         else if (Maps.Count > 1) then begin
            Petmar.MultiSelectSingleColumnStringList(WhatFor,j,Maps);
            for i := 0 to pred(WMDEM.MDIChildCount) do
               if (WMDEM.MDIChildren[i] is tMapForm) and (WMDEM.MDIChildren[i].Caption = Maps.Strings[j]) then Result := i;
         end;
      end;

{$If Defined(ExLabDownLoads)}
{$Else}

      procedure DownloadandUnzipDataFileIfNotPresent(pName : PathStr; Force : boolean = false);
      //pname is a subdirectory of c:\mapdata\, without the final backslash
      var
         pName2 : PathStr;
      begin
         {$IfDef RecordDownload} WriteLineToDebugFile('DownloadandUnzipDataFileIfNotPresent for ' + PName); {$EndIf}
         StopSplashing;
         if (not Force) and ValidPath(MainMapData + pname) then begin
            {$IfDef RecordDownload} WriteLineToDebugFile('DownloadandUnzipDataFileIfNotPresent, already there'); {$EndIf}
            exit;
         end;

         pName2 := pName + '.7z';
         {$IfDef RecordDownload} WriteLineToDebugFile('Try 7z'); {$EndIf}
         if not DownloadFileFromWeb(WebDataDownLoadDir + pName2,MainMapData + pName2,false) then begin
            {$IfDef RecordDownload} WriteLineToDebugFile('Try zip'); {$EndIf}
            pName2 := pName + '.zip';
            DownloadFileFromWeb(WebDataDownLoadDir + pName2,MainMapData + pName2,false);
         end;
         pName2 := MainMapData + pName2;
         if FileExists(pName2) then begin
            UnzipSingleFile(pName2);
            File2Trash(pName2);
            {$IfDef RecordDownload} WriteLineToDebugFile('success DownloadandUnzipDataFileIfNotPresent for ' + PName); {$EndIf}
         end
         else begin
            {$IfDef RecordDownload} WriteLineToDebugFile('failure DownloadandUnzipDataFileIfNotPresent for ' + PName); {$EndIf}
            MessageToContinue('Download fail, ' + pName);
         end;
      end;
{$EndIf}

{$EndIf}




function OpenDBString : shortstring;
begin
   Result := 'Open DBs: ' + IntToStr(NumOpenDB);
end;


procedure OpenDEMsToDebugFile(Why : shortstring);
var
   i : integer;
begin
   HighlightLineToDebugFile(why);
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         writeLineToDebugFile(IntegerToString(i,5) + '  ' + DEMGlb[i].AreaName + '  (' + DEMGlb[i].ColsRowsString + '  ' +  DEMGlb[i].DemSizeString + ')');
      end;
   end;
end;


function GetWhatsOpen : tStringList;
var
   i  : integer;

   {$IfDef VCL}
      procedure AddWindowType(wName : shortString; wType : tFormClass);
      var
         nf,i : integer;
         Extra : shortstring;
      begin
         nf := 0;
         for i := pred(WMDEM.MDIChildCount) downto 0 do
            if (WMDEM.MDIChildren[i] is wType) then inc(nf);
         if (nf > 0) then begin
            Result.Add('');
            Result.Add(wName + ' open');
            for i := pred(WMDEM.MDIChildCount) downto 0 do begin
               if WMDEM.MDIChildren[i] is wType then begin
                  if (wType = DEMMapf.tMapForm) then Extra := (WMDEM.MDIChildren[i] as TMapForm).MapDraw.MapSizeString
                  else Extra := '';
                  Result.Add('     ' + WMDEM.MDIChildren[i].Caption + Extra);
               end;
            end;
         end;
      end;
   {$EndIf}

begin
   {$IfDef RecordWhatsOpen} WriteLineToDebugFile('GetWhatsOpen enter'); {$EndIf}
   ApplicationProcessMessages;
   Result := tStringList.Create;

   if (NumDEMDataSetsOpen > 0) then begin
      Result.Add('DEMs/grids open');
      for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) then begin
            Result.Add(IntegerToString(i,5) + ' ' + DEMGlb[i].AreaName + ' (' + DEMGlb[i].ColsRowsString + ' ' +  DEMGlb[i].DemSizeString + ' ' + DEMGlb[i].zRange + DEMGlb[i].HorizontalDEMSpacing + ')');
         end;
      end;
   end;

   {$IfDef ExSat}
   {$Else}
      if (NumSatImageOpen > 0) then begin
         Result.Add('');
         Result.Add('Images open');
         for i := 1 to MaxSatAllowed do begin
            if (SatImage[i] <> Nil) then begin
               Result.Add(IntegerToString(i,5) + '  ' + SatImage[i].SceneBaseName);
            end;
         end;
      end;
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      if (NumOpenDB > 0) then begin
         Result.Add('');
         Result.Add('DB open');
         for i := 1 to MaxDataBase do begin
            if (GISdb[i] <> Nil) then begin
               Result.Add(IntegerToString(i,5) + '  ' + GISdb[i].dbName);
            end;
         end;
      end;
   {$EndIf}

   {$IfDef VCL}
      AddWindowType('Maps', DEMMapf.tMapForm);
      AddWindowType('Graphs', BaseGraf.TThisBaseGraph);
      AddWindowType('Text edit windows', PetEd32.TPetEditf);
   {$EndIf}

   {$IfDef RecordWhatsOpen}
      WriteLineToDebugFile('================================================================');
      Petmar.WriteStringListToDebugFile(OpenData);
      WriteLineToDebugFile('================================================================');
   {$EndIf}
end;


procedure CleanUpTempDirectory(IncudeDirs : boolean = false);
var
   bf : tstringlist;
begin
   {$If Defined(MessageStartup)} or Defined(CheckForNTLDR)} MessageToContinue('CleanUpTempDirectory ' + MDTempDir); {$EndIf}

   ShowHourglassCursor;
   {$IfDef VCL}
      if (CurrentProject <> '') then DeleteMultipleFiles(CurrentProject + '\','*.*');
   {$EndIf}

(*
   if ValidPath(MDTempDir + 'temp\grass1') then begin
      {$IfDef RecordCleanUpTempDir} WriteLineToDebugFile('CleanUpTempDirectory, start GRASS'); {$EndIf}
      bf := tstringlist.Create;
      bf.Add(ClearGRASSdirectory);
      EndBatchFile(MDTempdir + 'clear_grass.bat',bf);
   end;
*)

   if (MDTempDir <> '') then begin
      {$IfDef RecordCleanUpTempDir} WriteLineToDebugFile('CleanUpTempDirectory, start main'); {$EndIf}
      ChDir(MDTempDir);
      DeleteMultipleFiles(MDTempDir, '*.*');
      DeleteMultipleFiles(MDTempDir + 'ts\', '*.*');
      DeleteMultipleFiles(MDTempDir + 'db_aux\', '*.*');
      DeleteMultipleFiles(MainMapData + 'Icons\','beach_ball_*.*');
      if MDDef.CleanKMLDirOnClosing then CleanOutDirectory(MainMapData + 'kml\');
      if IncudeDirs then begin
         {$IfDef RecordCleanUpTempDir} WriteLineToDebugFile('CleanUpTempDirectory, call CleanOutDirectory'); {$EndIf}
         CleanOutDirectory(MDTempDir);
      end;
   end;
   ShowDefaultCursor;
   {$IfDef RecordCleanUpTempDir} WriteLineToDebugFile('CleanUpTempDirectory out'); {$EndIf}
end;


procedure RestoreSpecifiedDesktop(FName : PathStr);
var
   WantedDEM,WantedSat : integer;
   What     : ShortString;
   Table : tMyData;
   mt : tMapType;

       procedure LoadMapDetails(MapOwner : tMapForm);
       var
          bb : sfBoundBox;
       begin
          if Table.FieldExists('LAT_HI') then begin
             bb := Table.GetRecordBoundingBox;
             MapOwner.MapDraw.MaximizeLatLongMapCoverage(bb);
             MapOwner.FullMapSpeedButton.Enabled := true;
          end;
       end;


begin
   try
      HeavyDutyProcessing := true;
      if FileExists(FName) and FileExtEquals(fName,DefaultDBExt) then begin
         {$IfDef RecordProjects} WriteLineToDebugFile('RestoreSpecifiedDesktop in, fname=' + fName); {$EndIf}
         ProcessIniFile(iniRead,'',ChangeFileExt(fName,'_project.ini'));
         DeleteMultipleFiles(CurrentProject + '\','*.*');
         LastDeskTop := fName;
         {$IfDef RecordProjects} WriteLineToDebugFile('RestoreSpecifiedDesktop in, LastDeskTop=' + LastDeskTop); {$EndIf}
         WantedSat := 0;
         WantedDEM := 0;
         Table := tMyData.Create(FName);
         if Table.FieldExists('DATA_LAYER') then begin
            while not Table.Eof do begin
               What := Table.GetFieldByNameAsString('DATA_LAYER');
               fName := Table.GetFieldByNameAsString('FILENAME');

               {$IfDef RecordProjects} WriteLineToDebugFile(What + ' fname=' + fName); {$EndIf}

               if (What = 'DEM') then begin
                  if FileExists(fName) then begin
                     WantedSat := 0;
                     LoadNewDEM(WantedDem,fName,false,'new DEM','',false);
                     if (WantedDEM = 0) then break;
                     mt := MDdef.DefElevMap;
                     if Table.FieldExists('MAP_TYPE') then begin
                        mt := Table.GetFieldByNameAsInteger('MAP_TYPE');
                        if mt = 0 then mt := MDdef.DefElevMap;
                     end;
                     CreateDEMSelectionMap(WantedDEM,true,MDDef.DefElevsPercentile, mt);
                     LoadMapDetails(DEMGlb[WantedDEM].SelectionMap);
                  end
                  else MessageToContinue('Cannot find ' + fName);
               end;

               {$IfDef ExSat}
               {$Else}
                  if( What = 'SAT') then begin
                     if FileExists(fName) then begin
                        WantedDEM := 0;
                        WantedSat := OpenAndDisplaySatelliteScene(Nil,fname,true,true,true);
                        if Table.FieldExists('MAP_TYPE') then begin
                           mt := Table.GetFieldByNameAsInteger('MAP_TYPE');
                           if (mt <> 0) then SatImage[WantedSat].SelectionMap.MapDraw.MapType := mt;
                        end;
                        LoadMapDetails(SatImage[WantedSat].SelectionMap);
                        SatImage[WantedSat].SelectionMap.DoCompleteMapRedraw;
                     end
                     else MessageToContinue('Cannot find ' + fName);
                  end;
               {$EndIf}

               {$IfDef ExVegDensity}
               {$Else}
                  if What = 'VEG' then DEMGlb[WantedDem].OpenVegGrid(fName,1);
                  if What = 'VEG2' then DEMGlb[WantedDem].OpenVegGrid(fName,2);
                  if What = 'VOX1' then DEMGlb[WantedDem].VegDensityLayers[1] := tVegDensity.Create(fName,WantedDEM,false);
                  if What = 'VOX2' then DEMGlb[WantedDem].VegDensityLayers[2] := tVegDensity.Create(fName,WantedDEM,false);
               {$EndIf}

               if (What = 'DB') then begin
                  if FileExists(fName) then begin
                     if (WantedSat <> 0) then SatImage[WantedSat].SelectionMap.LoadDataBaseFile(fName);
                     if ValidDEM(WantedDEM) then DEMGlb[WantedDem].SelectionMap.LoadDataBaseFile(fName);
                  end
                  else MessageToContinue('Cannot find ' + fName);
               end;

               {$IfDef ExPointCloud}
               {$Else}
                  if (What = 'PC1') or (What = 'PC2') or (What = 'PC3') or (What = 'PC4') or (What = 'PC5') then begin
                     if (pt_cloud_opts_fm = Nil) then begin
                        pt_cloud_opts_fm := Tpt_cloud_opts_fm.Create(Application);
                        pt_cloud_opts_fm.Show;
                        pt_cloud_opts_fm.BaseMap := DEMGlb[WantedDem].SelectionMap;
                     end;
                     LastLidarDirectory := fName;
                     pt_cloud_opts_fm.GetFilesForPointCloud(StrToInt(Copy(What,3,1)),LastLidarDirectory,true);
                  end;
               {$EndIf}

               if (What = 'CART') then begin
                  DEMGlb[WantedDem].SelectionMap.LoadCartoDBoverlay(fName);
               end;
               if ValidDEM(WantedDEM) then DEMGlb[WantedDem].SelectionMap.CheckProperTix;
               Table.Next;
            end;
            Table.Destroy;
         end;
      end;
      {$IfDef RecordProjects} WriteLineToDebugFile('RestoreSpecifiedDesktop processing done, LastDESKtop=' + LastDesktop); {$EndIf}
   finally
      HeavyDutyProcessing := false;
      DEMNowDoing := JustWandering;
      wmDEM.SetMenusForVersion;
      wmDEM.Cascade;
      StopSplashing;
   end;
   {$IfDef RecordProjects} WriteLineToDebugFile('RestoreSpecifiedDesktop out, LastDESKtop=' + LastDesktop); {$EndIf}
end;

procedure RestoreMicrodemDesktop(fName : PathStr = ''; CloseAll : boolean = true);
begin
   try
      HeavyDutyProcessing := true;
      {$IfDef RecordProjects} WriteLineToDebugFile('RestoreMicrodemDesktop ' + fName); {$EndIf}
      if (fName = '') then  begin
         FName := LastDesktop;
         if not GetFileFromDirectory('Project to restore',DefaultDBMask,fName) then exit;
      end;
      LastDesktop := fName;
      {$IfDef RecordProjects} WriteLineToDebugFile('RestoreMicrodemDesktop, LastDesktop=' + LastDesktop); {$EndIf}
      if CloseAll then CloseAllWindowsAndData;
      {$IfDef RecordProjects} WriteLineToDebugFile('RestoreMicrodemDesktop moving on, LastDESKtop=' + LastDesktop); {$EndIf}
      RestoreSpecifiedDesktop(LastDesktop);
   finally
      HeavyDutyProcessing := false;
   end;
   {$IfDef RecordProjects} WriteLineToDebugFile('RestoreMicrodemDesktop out, LastDESKtop=' + LastDesktop); {$EndIf}
end;


procedure SaveMicrodemDesktop;
var
   i,j,k   : integer;
   FName : PathStr;
   Table : tMyData;

      procedure AddLine(theType : ShortString; theName : PathStr; MapOwner : tMapForm);
      begin
         Table.Insert;
         Table.SetFieldByNameAsString('DATA_LAYER',theType);
         Table.SetFieldByNameAsString('FILENAME',theName);
         if (MapOwner <> Nil) then begin
            Table.SetRecordBoundingBox(MapOwner.MapDraw.MapCorners.BoundBoxGeo);
            Table.SetFieldByNameAsInteger('MAP_TYPE',MapOwner.MapDraw.MapType);
         end;
         Table.Post;
      end;

begin
   FName := ProjectDir;
   if GetFileNameDefaultExt('saved project','MICRODEM project|*.dbf',FName,false) then begin
      LastDesktop := fname;
      ProcessIniFile(iniWrite,'',ChangeFileExt(fName,'_project.ini'));
      Make_Tables.CreateProjectTable(fName);
      Table := tMyData.Create(fName);
      for i := 1 to MaxDEMDataSets do if ValidDEM(i) and (DEMGlb[i].SelectionMap <> Nil) then begin
         if (DEMGlb[i].DEMFileName = '') then DEMGlb[i].WriteNewFormatDEM(DEMGlb[i].DEMFileName);
         AddLine('DEM',DEMGlb[i].DEMFileName,DEMGlb[i].SelectionMap);

         if (DEMGlb[i].SelectionMap.MapDraw.CartoGroupShapesUp <> '') then AddLine('CART',DEMGlb[i].SelectionMap.MapDraw.CartoGroupShapesUp,Nil);

         {$IfDef ExVegDensity}
         {$Else}
            if (DEMGlb[i].VegGrid[1] <> 0) then AddLine('VEG',DEMGlb[DEMGlb[i].VegGrid[1]].DEMFileName,Nil);
            if (DEMGlb[i].VegGrid[2] <> 0) then AddLine('VEG2',DEMGlb[DEMGlb[i].VegGrid[2]].DEMFileName,Nil);
            if (DEMGlb[i].VegDensityLayers[1] <> Nil) then AddLine('VOX1',LastVegDensity1fName,Nil);
            if (DEMGlb[i].VegDensityLayers[2] <> Nil) then AddLine('VOX2',LastVegDensity2fName,Nil);
         {$EndIf}

         for j := 1 to MaxDataBase do begin
            if ValidDB(j) and (GISdb[j].TheMapOwner <> Nil) and (GISdb[j].TheMapOwner.MapDraw.DEMonMap = i) then AddLine('DB',GISdb[j].dbFullName,Nil);
         end;

         {$IfDef ExPointCloud}
         {$Else}
            if (pt_cloud_opts_fm <> Nil) then begin
               for k := 1 to 5 do begin
                  if (pt_cloud_opts_fm .LasFiles[k] <> Nil) then begin
                     AddLine('PC' + IntToStr(k),pt_cloud_opts_fm.LasFiles[k].CloudDir ,Nil);
                  end;
               end;
            end;
         {$EndIf}

      end;
      {$IfDef ExSat}
      {$Else}
         for i := 1 to MaxSatAllowed do if (SatImage[i] <> Nil) then begin
            AddLine('SAT',SatImage[i].IndexFileName,SatImage[i].SelectionMap);
            for j := 1 to MaxDataBase do begin
               if ValidDB(j) and (GISdb[j].theMapOwner <> Nil) and (GISdb[j].TheMapOwner.MapDraw.SatonMap = i) then AddLine('DB',GISdb[j].dbFullName,Nil);
            end;
         end;
      {$EndIf}
      Table.Destroy;
   end;
end;


procedure InitCompareDEMs;
var
   i : integer;
begin
   for i := 1 to MaxCompare do begin
      CompareDEMIndexes[i] := 0;
      CompareDEMNames[i] := '';
   end;
end;


initialization
   {$IfDef MessageStartUpUnit} MessageToContinue('Startup dem_manager'); {$EndIf}
   InitCompareDEMs;
   ClosingEverything := false;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Finalized DEM_Manager'); {$EndIf}
end.

