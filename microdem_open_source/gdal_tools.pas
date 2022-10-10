unit gdal_tools;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2018 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}


//{$Define ConvertDBFtoDB}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$Define RecordGDALOpen}
   //{$Define RecordSubsetOpen}

   {$IFDEF DEBUG}
      //{$Define RecordSubsetOpen}
      //{$Define RecordGDALOpen}
      //{$Define RecordUseOtherPrograms}
      //{$Define RecordSaveProblems}
      //{$Define RecordGDAL}
      //{$Define RecordWBT}
      //{$Define RecordOGR}
      //{$Define RecordGeoPDF}
      //{$Define RecordReformat}
   {$Else}
   {$EndIf}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseBDETables}
      dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations

   {$IfDef MSWindows}
      Windows,Messages,
   {$EndIf}

   {$IfDef VCL}
      Forms, Graphics,ExtCtrls,Grids,Controls,
   {$EndIf}

   System.IOutils,System.UITypes,System.Math,System.UIConsts,
   SysUtils, Classes,StrUtils,ClipBrd,
   petmar,Petmar_types,
   DEMMapf,DEMMapDraw,DEMDefs,BaseMap,DEM_NLCD;


   type
      tGDALGeoPDF = (gdalOpenGeoPDFimagelayer1,gdalAllindividuallayers1,gdalOpenGeoPDF1,gdalMergeGeoPDF1);
   var
      SetGDALdataStr : ANSIString;

   procedure GetGDALFileNames;
   function GetGDALversion : ANSIstring;

   function IsGDALFilePresent(fName : PathStr) : boolean;
   procedure GDALcommand(BatchName : PathStr; cmd : ShortString; Log : boolean = true);
   procedure GDAL_netcdf(fName : PathStr ='');

   function GDALextentBoxUTM(xmin,ymin,xmax,ymax : float64) : shortstring; overload;
   function GDALextentBoxUTM(BoundBox : sfBoundBox) : shortstring; overload;

   function GDALextentBoxLatLong(BoundBox : sfBoundBox) : shortString;
   procedure ResaveAsGDALgeotiff(fName : PathStr);

   function GDALImageFormat(Ext : ExtStr; OnlyGDAL : boolean = false) :  boolean;
   function GDALGridFormat(Ext : ExtStr; OnlyGDAL : boolean = false) :  boolean;
   function GDALinfoOutput(fname : PathStr) : PathStr;
   procedure GetGDALinfo(fName : PathStr; var GDALinfo : tGDALinfo);
   procedure ZeroGDALinfo(var GDALinfo : tGDALinfo);
   function GetEPSG(var GDALinfo : tGDALinfo) : integer;
   procedure BatchGDALinfo(Infiles : tStringList; GetClipBox : boolean; var UTMZone : int16);
   procedure BatchGDALSRSinfo(Infiles : tStringList);
   procedure GDAL_Convert_JSON(var fName : pathStr);

   function GDAL_SlopeMap_ZT(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
   function GDAL_SlopeMap_Horn(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
   function GDAL_AspectMap_Horn(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
   function GDAL_AspectMap_ZT(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
   function GDAL_HillshadeMap_Horn(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
   function GDAL_TRI_Wilson(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
   function GDAL_TRI_Riley(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
   function GDAL_TPI(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
   function GDAL_Roughness(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;

   procedure GDAL_Fill_Holes(InName : PathStr);
   procedure GDAL_Upsample_DEM(DEM : integer);
   function GDAL_downsample_DEM_1sec(DEM : integer; OutName : PathStr) : integer;
   procedure GDALGeotiffToWKT(fName : PathStr);

   procedure StartGDALbatchFile(var BatchFile : tStringList);

   procedure GDAL_ConvertGPXToSHP(var fName : pathStr);
   procedure GDALreprojectshapefile;

   procedure CallGDALMerge(var MergefName : PathStr; OutNames : tStringList; MissingData : integer = 255);
   function GDAL_translateUTM(InName,OutName : PathStr; WGS84,NHemi : boolean;  UTMzone : int16) : shortstring;
   procedure GDALBandExtraction;
   procedure GDALcreatemultibandTIFF;
   procedure GDALGeodatabasetoshapefile;
   procedure GDAL_dual_UTM(DEM : integer);

   procedure GDAL_Convert_Shapefile_2_geopackage(fName : pathStr);

   procedure GDALAssignDEMProjection(DEMName,ProjName : PathStr);

   procedure GeneralConvertToWGS84Shapefile(fName : pathStr);
   function OGRrewriteShapefile(fName : pathStr) : PathStr;
   function ReprojectShapeFileToGeographic(var fName : Pathstr; aDir : PathStr) : boolean;
   function ExtractMapCoverageToWGS84Shapefile(fName : pathStr; BoundBox : sfBoundBox) : PathStr;

   procedure GDALConvertImagesToGeotiff(fName : PathStr = ''; Recycle : boolean = true);
   function GDAL_Translate_2_geotiff(fName : PathStr; OutName : PathStr = ''; ExtraOptions : ANSIString = ''; TrashOriginal : boolean = true) : PathStr;
   function GDAL_warp(var fName : PathStr) : PathStr;
   {$IfDef ExGeoPDF}
   {$Else}
      procedure GDALconvertGeoPDF(Option : tGDALGeoPDF);
   {$EndIf}
   procedure GDALregister(LatLong : boolean; GISNum : Integer; ImageName : PathStr; LatHemi : AnsiChar);

   procedure GDALreprojectLASfile(fName : PathStr; T_EPSG,a_EPSG : integer);

   procedure GDALSubsetSatImageToMatchMap(MapOwner : tMapForm; GDAL_program : PathStr);
   function GDALsubsetimageandopen(bb : sfBoundBox; LatLongBox : boolean; fName : PathStr; BaseOutPath : PathStr = '') : integer;
   procedure GDALConvert4BitGeotiff(fName : PathStr);
   procedure GDALConvertSingleImageToGeotiff(var fName : PathStr);

   procedure UseGDAL_VRT_to_merge(var MergefName : PathStr; OutNames : tStringList; Added : ShortString = '');
   procedure UseGDAL_Warp_to_merge(var MergefName : PathStr; OutNames : tStringList);

implementation


uses
   DEMDef_routines,
   DEMCoord,
   DEMeros,
   Geotiff,
   PetDButils,
   DEM_Manager,
   DEMDataBase,
   nevadia_main;

const
   GDAL_Geotiff_str = ' -of Gtiff -co TILED=NO -co COMPRESS=NONE ';
   AddWKT = ' -wkt_format WKT2';


   {$IfDef ExGeoPDF}
   {$Else}
      {$I geopdf.inc}
   {$EndIf}

function GetGDALversion : ANSIstring;
var
   cmd : shortstring;
begin
   SaveBackupDefaults;
   MDDef.ShowWinExec := false;
   cmd := GDAL_info_name + ' --version |clip';
   GDALCommand(MDTempDir + 'gdal_info.bat',cmd,false);
   Result := Clipboard.AsText;
   RestoreBackupDefaults;
end;


procedure GetGDALFileNames;
label
   NoMoreBugging;

     function SetGDALprogramName(fName : PathStr; var FullName : PathStr) : boolean;
     begin
        FullName := GDALtools_Dir + fName;
        Result := FileExists(FullName);
        {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} if Not Result then WriteLineToDebugFile('GDAL file missing, ' + FullName); {$EndIf}
     end;

     function SetRest : boolean;
     begin
         Result := SetGDALprogramName('gdal_translate.exe',GDAL_translate_name) and SetGDALprogramName('gdal_contour.exe',GDAL_contour_name) and SetGDALprogramName('gdalwarp.exe',GDAL_warp_name) and
                   SetGDALprogramName('gdaldem.exe',GDAL_dem_name) and SetGDALprogramName('ogr2ogr.exe',GDAL_ogr_name) and SetGDALprogramName('gdalinfo.exe',GDAL_info_name) and
                   SetGDALprogramName('gdalsrsinfo.exe',GDAL_srs_info_name);
         if (not Result) then begin
            MessageToContinue('GDAL files are missing; consider reinstalling OSGEO4W');
            {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('user recommended reinstalling'); {$EndIf}
         end;
         SetGDALdataStr := 'set GDAL_DATA=' + StringReplace(GDALtools_Dir, 'bin', 'share\gdal',[rfIgnoreCase]);
     end;

begin
   {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} HighlightLineToDebugFile('GetGDALFileNames in'); {$EndIf}

   if PathIsValid(GDALtools_Dir) {and PathIsValid(GDALtools_Data)} and SetRest then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('GDAL valid, ' + GDALtools_Dir + '  ' + GetGDALversion); {$EndIf}
      exit;
   end;

   if not PathIsValid('C:\OSGeo4W\') then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('No default GDAL dir'); {$EndIf}
      if not AnswerIsYes('No default GDAL installation present.  If you downloaded, do you want to try to find GDAL') then begin
         {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('User stopped GDAL manual search'); {$EndIf}
         goto NoMoreBugging;
      end
   end;

   if not PathIsValid(GDALtools_Dir) then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('GDAL problem tools dir, ' + GDALtools_Dir); {$EndIf}
      GDALtools_Dir := 'C:\OSGeo4W\bin\';
      GetDOSPath('GDAL binary directory, something like ' +  GDALtools_Dir,GDALtools_Dir);
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('User pick GDAL tools dir: ' + GDALtools_Dir); {$EndIf}
   end;

   (*
   if not PathIsValid(GDALtools_Data) then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('GDAL problem data dir, ' + GDALtools_Data); {$EndIf}
      GDALtools_Data := 'c:\OSGeo4W\share\gdal\';
      if not PathIsValid(GDALtools_Data) then GetDOSPath('GDAL tools data directory, something like ' + GDALtools_Data,GDALtools_Data);
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('User pick GDAL data dir: ' + GDALtools_Dir); {$EndIf}
   end;
   *)

   if PathIsValid(GDALtools_Dir) {and PathIsValid(GDALtools_Data)} and SetRest then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('User picked GDAL valid, ' + GDALtools_Dir + '  ' + GetGDALversion); {$EndIf}
      exit;
   end;

   {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)}
      if not PathIsValid(GDALtools_Dir) then WriteLineToDebugFile('Invalid: ' + GDALtools_Dir);
      //if not PathIsValid(GDALtools_Data) then WriteLineToDebugFile('Invalid: ' + GDALtools_Data);
   {$EndIf}

  NoMoreBugging:;
   MDdef.DontBugMeAboutGDAL := not AnswerIsYes('Do you want to be reminded about GDAL problems in the future');

   if MDdef.DontBugMeAboutGDAL then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('choose no more GDAL problem messages'); {$EndIf}
      SaveMDdefaults;
   end;
end;


procedure GDAL_netcdf(fName : PathStr = '');
var
   sl,BatchFile : tStringList;
   aLine : shortstring;
   i : integer;
   OutDir,LayerName,ShortName,InfoName : PathStr;
begin
   {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL_netcdf in ' + fName); {$EndIf}
   if IsGDALFilePresent(GDAL_translate_name) then begin
      StartGDALbatchFile(BatchFile);
      fName := 'C:\temp\output\L8_OLI_2015_08_17_15_46_10_015033_L1R_pan.nc';
      OutDir := ExtractFilePath(fName);
      InfoName := GDALinfoOutput(fName);
      {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL_netcdf in ' + InfoName); {$EndIf}

      //https://gdal.org/drivers/raster/netcdf.html
      //cmd := GDAL_info_name  + TStr + fName + ' >' + OutName;
      BatchFile.Add(GDAL_info_name  + ' ' + fName + AddWKT + ' >' + InfoName);
      EndBatchFile(MDTempDir + 'nc_info.bat',batchfile);

      StartGDALbatchFile(BatchFile);
      sl := tStringList.Create;
      sl.LoadFromFile(InfoName);
      //https://nsidc.org/support/how/how-convert-golive-netcdf-variables-geotiff
      //gdal_translate NETCDF:"Input_FileName.nc":variable_name Output_FileName.tif
      for i  := 0 to pred(sl.Count) do begin
         aline := sl.Strings[i];
         //SUBDATASET_1_NAME=NETCDF:"C:\temp\output\L8_OLI_2015_08_17_15_46_10_015033_L1R.nc":lon
         if StrUtils.AnsiContainsText(aline,'SUBDATASET_') then begin
            LayerName := AfterSpecifiedString(aline,'NAME=');
            ShortName := AfterSpecifiedString(aline,'":');   //InputString : shortString; SubString : shortstring)
            if (ShortName <> 'lon') and (ShortName <> 'lat') then begin
               BatchFile.Add(GDAL_translate_name + ' ' + LayerName + ' ' + OutDir + ShortName + '.tif');
            end;
         end;
      end;
     EndBatchFile(MDTempDir + 'netcdf.bat',batchfile);
   end;
end;


function GDAL_DEM_command(cmd : ANSIstring; OutName : PathStr; mt : byte = mtElevSpectrum) : integer;
begin
    WinExecAndWait32(cmd);
    if FileExists(OutName) then begin
       Result := OpenNewDEM(OutName,false);
       CreateDEMSelectionMap(Result,true,true,mt);
    end
    else begin
       Result := 0;
       MessageToContinue('GDAL failure, to see error messages try command in DOS window: ' + cmd);
    end;
end;


procedure GDALcommand(BatchName : PathStr; cmd : ShortString; Log : boolean = true);
var
   BatchFile : tStringList;
begin
   {$IfDef RecordOGR} if Log then WriteLineToDebugFile('GDALCommand, cmd = ' + cmd); {$EndIf}
   StartGDALbatchFile(BatchFile);
   BatchFile.Add(cmd);
   EndBatchFile(BatchName,batchfile,true,log);
end;


procedure GDALAssignDEMProjection(DEMName,ProjName : PathStr);
var
   bfile : PathStr;
   cmd : shortString;
   BatchFile : tStringList;
begin
   cmd := PythonEXEname + ' ' + PythonScriptDir + 'gdal_edit.py -a_srs ' + ProjName + ' ' + DEMName;
   StartGDALbatchFile(BatchFile);
   BatchFile.Add(cmd);
   bfile := Petmar.NextFileNumber(MDTempDir, 'gdal_assign_proj_','.bat');
   EndBatchFile(bfile ,batchfile);
   if FileExists(DEMName) then begin
      //OpenNewDEM(DEMName);
   end
   else MessageToContinue('GDALAssignProjection failure, to see error messages try batch file in DOS window: ' + bfile);
end;



procedure GDAL_Fill_Holes(InName : PathStr);
var
   OutName,bfile : PathStr;
   cmd : shortString;
   BatchFile : tStringList;
begin
   if FileExistsErrorMessage(InName) then begin
      OutName := MDTempDir + 'gdal_fill_holes_' + ExtractFileNameNoExt(InName) + '.tif';
      cmd := PythonEXEname + ' ' + PythonScriptDir + 'gdal_fillnodata.py -md 100 -si 2 -o ' + OutName + ' ' + InName;
      StartGDALbatchFile(BatchFile);
      BatchFile.Add(cmd);
      EndBatchFile(bfile,batchfile);
      if FileExists(OutName) then begin
         OpenNewDEM(OutName);
      end
      else MessageToContinue('GDAL_Fill_Holes failure, to see error messages try batch file in DOS window: ' + bfile);
  end;

//QGIS command looks like this
//C:\OSGeo4W64\bin\python3 C:\OSGeo4W64\apps\qgis\python\plugins\processing\algs\gdal\fillnodata.py -md 100 -si 2 -o c:\mapdata\temp\gdal_fill_holes_temp_dem.tif c:\mapdata\temp\temp_dem.tif

//python3 -m gdal_fillnodata -md 500 -si 1 -b 1 -of GTiff H:\00-alaska-test-area\alaska_elevation_geographic.tif C:/temp/OUTPUT.tif

(*
gdal_fillnodata.py [-q] [-md max_distance] [-si smooth_iterations]
                [-o name=value] [-b band]
                srcfile [-nomask] [-mask filename] [-of format] [dstfile]
*)
end;

function GDAL_TRI_Wilson(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
begin
   if FileExistsErrorMessage(InName) then begin
      if sf <> '' then sf := ' -s ' + sf + ' ';
      if (Outname = '') then OutName := MDTempDir + 'gdal_TRI_Wilson_' + ExtractFileNameNoExt(InName) + '.tif';
      GDAL_DEM_command(GDAL_dem_name + ' TRI ' + InName + ' ' + OutName + ' -alg Wilson' + sf, OutName,mtElevSpectrum);
   end;
end;


function GDAL_TRI_Riley(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
begin
   if FileExistsErrorMessage(InName) then begin
      if sf <> '' then sf := ' -s ' + sf + ' ';
      if (Outname = '') then OutName := MDTempDir + 'gdal_TRI_Riley_' + ExtractFileNameNoExt(InName) + '.tif';
      GDAL_DEM_command(GDAL_dem_name + ' TRI ' + InName + ' ' + OutName + ' -alg Riley' + sf, OutName,mtElevSpectrum);
   end;
end;

function GDAL_TPI(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
begin
   if FileExistsErrorMessage(InName) then begin
      if sf <> '' then sf := ' -s ' + sf + ' ';
      if (Outname = '') then OutName := MDTempDir + 'gdal_TPI_' + ExtractFileNameNoExt(InName) + '.tif';
      GDAL_DEM_command(GDAL_dem_name + ' TPI ' + InName + ' ' + OutName + sf, OutName,mtElevSpectrum);
   end;
end;


function GDAL_Roughness(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
begin
   if FileExistsErrorMessage(InName) then begin
      if sf <> '' then sf := ' -s ' + sf + ' ';
      if (Outname = '') then OutName := MDTempDir + 'gdal_roughness_' + ExtractFileNameNoExt(InName) + '.tif';
      GDAL_DEM_command(GDAL_dem_name + ' roughness ' + InName + ' ' + OutName + sf, OutName);
   end;
end;


function GDAL_SlopeMap_ZT(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
//https://gdal.org/programs/gdaldem.html
begin
   if FileExistsErrorMessage(InName) then begin
      if sf <> '' then sf := ' -s ' + sf + ' ';
      if (Outname = '') then OutName := MDTempDir + 'gdal_slope_zt_' + ExtractFileNameNoExt(InName) + '.tif';
      Result := GDAL_DEM_command(GDAL_dem_name + ' slope ' + InName + ' ' + OutName +  ' -p -alg ZevenbergenThorne' + sf, OutName);
      if (Result <> 0) then DEMGlb[Result].DEMHeader.ElevUnits := PercentSlope;
   end;
end;

function GDAL_SlopeMap_Horn(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
//https://gdal.org/programs/gdaldem.html
begin
   if FileExistsErrorMessage(InName) then begin
      if sf <> '' then sf := ' -s ' + sf + ' ';
      if (Outname = '') then OutName := MDTempDir + 'gdal_slope_horn_' + ExtractFileNameNoExt(InName) + '.tif';
      Result := GDAL_DEM_command(GDAL_dem_name + ' slope ' + InName + ' ' + OutName +  ' -p ' + sf, OutName);
      if Result <> 0 then DEMGlb[Result].DEMHeader.ElevUnits := PercentSlope;
   end;
end;

function GDAL_AspectMap_Horn(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
//https://gdal.org/programs/gdaldem.html
begin
   if FileExistsErrorMessage(InName) then begin
      if sf <> '' then sf := ' -s ' + sf + ' ';
      if (Outname = '') then OutName := MDTempDir + 'gdal_aspect_horn_' + ExtractFileNameNoExt(InName) + '.tif';
      Result := GDAL_DEM_command(GDAL_dem_name + ' aspect ' + InName + ' ' + OutName + sf, OutName);
      if Result <> 0 then DEMGlb[Result].DEMHeader.ElevUnits := AspectDeg;
   end;
end;


function GDAL_AspectMap_ZT(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
//https://gdal.org/programs/gdaldem.html
begin
   if FileExistsErrorMessage(InName) then begin
      if sf <> '' then sf := ' -s ' + sf + ' ';
      if (Outname = '') then OutName := MDTempDir + 'gdal_aspect_zt_' + ExtractFileNameNoExt(InName) + '.tif';
      Result := GDAL_DEM_command(GDAL_dem_name + ' aspect ' + InName + ' ' + OutName +  ' -alg ZevenbergenThorne' + sf, OutName);
      if Result <> 0 then DEMGlb[Result].DEMHeader.ElevUnits := AspectDeg;
   end;
end;


function GDAL_HillshadeMap_Horn(InName : PathStr; sf : shortstring = ''; outname : shortstring = '') : integer;
//gdaldem hillshade input_dem output_hillshade
//            [-z ZFactor (default=1)] [-s scale* (default=1)]
//            [-az Azimuth (default=315)] [-alt Altitude (default=45)]
//            [-alg Horn|ZevenbergenThorne] [-combined | -multidirectional | -igor]
//            [-compute_edges] [-b Band (default=1)] [-of format] [-co "NAME=VALUE"]* [-q]
//https://gdal.org/programs/gdaldem.html
begin
   if FileExistsErrorMessage(InName) then begin
      if sf <> '' then sf := ' -s ' + sf + ' ';
      if (Outname = '') then OutName := MDTempDir + 'gdal_aspect_zt_' + ExtractFileNameNoExt(InName) + '.tif';
      Result := GDAL_DEM_command(GDAL_dem_name + ' hillshade ' + InName + ' ' + OutName +  ' -alg Horn' + sf, OutName);
   end;
end;


procedure GDALGeotiffToWKT(fName : PathStr);
var
   OutName : PathStr;
   cmd : ANSIString;
   BatchFile: tStringList;
begin
   {$IfDef RecordGDAL} WriteLineToDebugFile('BatchGDALSRSinfo in'); {$EndIf}
   StartGDALbatchFile(BatchFile);
   OutName := MDTempDir + ExtractFileName(fName) + '.prj';
   cmd := GDAL_srs_info_name  + ' -o wkt ' + fName + ' >' + OutName;
   BatchFile.Add(Cmd);
   EndBatchFile(MDTempDir + 'gdal_srs_info.bat',BatchFile);
   ShowInNotepadPlusPlus(OutName,ExtractFileName(OutName) + 'SRS WKT');
end;



function GDAL_warp_DEM(DEM : integer; OutName : PathStr; xspace,yspace : float32; IntString : shortstring; TargetEPSG : shortstring = '') : integer;
var
   cmd : AnsiString;
   SpaceStr : shortString;
   InName : PathStr;
begin
   InName := DEMGlb[DEM].GeotiffDEMName;
   SpaceStr := ' -tr ' + RealToString(xSpace,-12,-8) + ' ' + RealToString(ySpace,-12,-8);
   cmd := GDAL_Warp_Name  + SpaceStr + IntString + InName + ' ' + OutName;
   Result := GDAL_DEM_command(cmd,OutName,DEMGlb[DEM].SelectionMap.MapDraw.MapType);
end;


function GDAL_downsample_DEM_1sec(DEM : integer; OutName : PathStr) : integer;
//tried a number of ways to change this, including having the input UTM EPSG
(*
C:\OSGeo4W\bin\gdalwarp.exe -te_srs EPSG:4326 -tr 0.000277778 0.000277778 -r average -te 46.095105 10.681662 46.201234 10.814749 H:\demix_wine_contest\italy\trentino_dtm.tif c:\mapdata\temp\gdal_downsample.dem
Creating output file that is 42211541P x 72564645L.
Using internal nodata values (e.g. -3.40282e+38) for image H:\demix_wine_contest\italy\trentino_dtm.tif.
Copying nodata values from source H:\demix_wine_contest\italy\trentino_dtm.tif to destination .
ERROR 2: usgsdem_create.cpp, 1291: cannot allocate 6126130975135890 bytes
*)
var
   cmd : AnsiString;
   IntString,
   TargetEPSG,
   TargetExtent,
   SpaceStr : shortString;
   LatSW,LatNE,LongSW,LongNE : float64;
   InName : PathStr;
begin
   IntString := ' -r average';
   TargetEPSG := ' -te_srs EPSG:4326';
   SpaceStr := ' -tr 0.000277778 0.000277778';
   DEMGlb[DEM].DEMGridToLatLongDegree(0,0,LatSW,LongSW);
   DEMGlb[DEM].DEMGridToLatLongDegree(pred(DEMGlb[DEM].DEMHeader.NumCol),pred(DEMGlb[DEM].DEMHeader.NumRow),LatNE,LongNE);
   TargetExtent := ' -te ' + RealToString(LatSW,-12,-6) + ' ' + RealToString(LongSW,-12,-6)  +  ' ' + RealToString(LatNE,-12,-6) + ' ' + RealToString(LongNE,-12,-6);
   InName := DEMGlb[DEM].GeotiffDEMName;
   cmd := GDAL_Warp_Name  + SpaceStr + IntString + TargetExtent + TargetEPSG + ' ' + InName + ' ' + OutName;
   Result := GDAL_DEM_command(cmd,OutName,DEMGlb[DEM].SelectionMap.MapDraw.MapType);
   //gdalwarp -t_srs EPSG:4326 -tr 0.3125 0.25 -r near -te 71.40625 24.875 84.21875 34.375 -te_srs EPSG:4326 -of GTiff foo.tiff bar.tiff
end;

procedure GDAL_upsample_DEM(DEM : integer);
var
   OutName : PathStr;
   SpaceStr : shortString;
   cmd : AnsiString;
begin
   {$If Defined(RecordSave) or Defined(RecordGDAL)} WriteLineToDebugFile('GDALresamplethin1Click'); {$EndIf}
   if IsGDALFilePresent(GDAL_Warp_Name) then begin
      ReadDefault('upsample factor (multiple spacing)',MDDef.GDALThinFactor);
      OutName := MDTempDir + DEMGlb[DEM].AreaName + '_upsample_linear.tif';
      GDAL_warp_DEM(DEM,OutName,MDDef.GDALThinFactor * DEMGlb[DEM].DEMheader.DEMxSpacing,MDDef.GDALThinFactor * DEMGlb[DEM].DEMheader.DEMxSpacing,' -r bilinear ');
      OutName := MDTempDir + DEMGlb[DEM].AreaName + '_upsample_cubic.tif';
      GDAL_warp_DEM(DEM,OutName,MDDef.GDALThinFactor * DEMGlb[DEM].DEMheader.DEMxSpacing,MDDef.GDALThinFactor * DEMGlb[DEM].DEMheader.DEMxSpacing,' -r cubic ');
   end;
end;


procedure GDAL_dual_UTM(DEM : integer);
var
   OutName : PathStr;
   UTMSpacing : float64;
   SpaceStr : shortString;
   Code : int32;
begin
   {$If Defined(RecordSave) or Defined(RecordGDAL)} WriteLineToDebugFile('GDALresamplethin1Click'); {$EndIf}
   if IsGDALFilePresent(GDAL_Warp_Name) then begin
      UTMSpacing := 30;
      ReadDefault('UTMSpacing (m)',UTMSpacing);

      Code := 32600;      {WGS84 default}
      if DEMGlb[DEM].DEMheader.LatHemi = 'S' then Code := Code + 100;
      Code := Code + DEMGlb[DEM].DEMheader.UTMZone;

      SpaceStr :=  ' -t_srs EPSG:' + IntToStr(Code) + ' ';  //-tr ' + RealToString(UTMSpacing,-12,-8) + ' ' + RealToString(UTMSpacing,-12,-8);

      OutName := MDTempDir + DEMGlb[DEM].AreaName + '_resamp_UTM_cubic.tif';
      GDAL_warp_DEM(DEM,OutName,UTMSpacing,UTMSpacing,' -r cubic ',SpaceStr);
      OutName := MDTempDir + DEMGlb[DEM].AreaName + '_resamp_UTM_linear.tif';
      GDAL_warp_DEM(DEM,OutName,UTMSpacing,UTMSpacing,' -r bilinear ',SpaceStr);
   end;
end;


procedure ResaveAsGDALgeotiff(fName : PathStr);
var
   OldName : PathStr;
begin
   OldName := ExtractFilePath(fName) + 'md_tiff_' + ExtractFileName(fName);
   SysUtils.RenameFile(fName,OldName);
   GDAL_Translate_2_geotiff(OldName,fName);
end;


function GDALextentBoxUTM(xmin,ymin,xmax,ymax : float64) : shortstring;
begin
   Result := IntToStr(round(xmin)) + ' ' + IntToStr(round(ymin)) + ' ' + IntToStr(round(xmax)) + ' ' +  IntToStr(round(ymax)) + ' ';
end;

function GDALextentBoxUTM(BoundBox : sfBoundBox) : shortstring;
begin
   Result := RealToString(BoundBox.xmin,-12,0) + ' ' + RealToString(BoundBox.ymax,-12,-0) + ' ' + RealToString(BoundBox.xmax,-12,-0) + ' ' +  RealToString(BoundBox.ymin,-12,-0) + ' ';
end;



function GDALextentBoxGeo(BoundBox : sfBoundBox) : shortstring;
begin
   Result := RealToString(BoundBox.xmin,-12,-8) + ' ' + RealToString(BoundBox.ymax,-12,-8) + ' ' + RealToString(BoundBox.xmax,-12,-8) + ' ' +  RealToString(BoundBox.ymin,-12,-8) + ' ';
end;


function GDALextentBoxLatLong(BoundBox : sfBoundBox) : shortString;
begin
   Result :=  ' -projwin ' + GDALextentBoxGeo(BoundBox) + ' ';
end;



     procedure ZeroGDALinfo(var GDALinfo : tGDALinfo);
     begin
        GDALinfo.inEPSG := -9999;
        GDALinfo.utmEPSG := -9999;
        GDALinfo.HDatum := '';
     end;

      function GDALImageFormat(Ext : ExtStr; OnlyGDAL : boolean = false) : boolean;
      begin
         Ext := UpperCase(Ext);
         Result := (Ext = '.JP2') or (Ext = '.KAP') or (Ext = '.HDF') or (Ext = '.ECW') or (Ext = '.IMG') or (Ext = '.COS') or (Ext = '.PDF') or (Ext = '.KAP');  // or (Ext = '.NC');
         if not(OnlyGDAL) then Result := Result or (Ext = '.TIF') or (Ext = '.TIFF');
      end;

      function GDALGridFormat(Ext : ExtStr; OnlyGDAL : boolean = false) : boolean;
      begin
         //Ext := UpperCase(Ext);
         Result := ExtEquals(Ext, '.ADF') or ExtEquals(Ext, '.IMG') or ExtEquals(Ext, '.CDF') or ExtEquals(Ext, '.BT') or ExtEquals(Ext, '.NC') or
             ExtEquals(Ext, '.E00') or ExtEquals(Ext, '.PDF') or ExtEquals(Ext, '.ASC');
         if not(OnlyGDAL) then begin
            Result := Result or ExtEquals(Ext,'.TIF') or ExtEquals(Ext,'.TIFF');
         end;
      end;


      function GetEPSG(var GDALinfo : tGDALinfo) : integer;
      begin
         if (GDALinfo.utmEPSG > 1) then Result := GDALinfo.utmEPSG
         else begin
            if (GDALinfo.HDatum = 'ETR89') then Result := 25800
            else if (GDALinfo.HDatum = 'NAD83') or (GDALinfo.HDatum = '') then Result := 26900
            else if (GDALinfo.HDatum = 'WGS84') then Result := 32600;

            if (GDALinfo.UTMZone < 0) then GDALinfo.UTMZone := GetUTMzone(GDALInfo.cLong);
            Result := Result + GDALinfo.UTMZone;
            if (GDALInfo.cLat < 0) then Result := Result  + 100;
            GDALinfo.utmEPSG := Result;
            if (GDALinfo.inEPSG < 0) then GDALinfo.inEPSG := Result;
         end;
      end;


      function GDAL_warp(var fName : PathStr) : PathStr;
      var
         BatchFile : tStringList;
         cmd,InProj,outProj : shortString;
         GDALinfo : tGDALinfo;
         Ext : ExtStr;
      begin
         if IsGDALFilePresent(GDAL_warp_name) then begin
            Ext := UpperCase(ExtractFileExt(fName));
            if (Ext = '.TIF') then  Result := ExtractFilePath(fName) + 'warp_utm_' + ExtractFileNameNoExt(fName) + '.tif'
            else Result := ChangeFileExt(fName,'.tif');
            if not FileExists(Result) then begin
               {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL_warp in ' + fName); {$EndIf}
               GetGDALinfo(fName,GDALinfo);
               {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL_warp GetGDALinfo done'); {$EndIf}
               inProj := ' -s_srs EPSG:' + IntToStr(GDALinfo.inEPSG);
               OutProj := ' -t_srs EPSG:' + IntToStr(GDALinfo.utmEPSG);
               StartGDALbatchFile(BatchFile);
               cmd := GDAL_warp_name + ' -of Gtiff ' + inProj + OutProj + ' ' + fName + ' ' + Result;
               BatchFile.Add(cmd);
               EndBatchFile(MDTempDir + 'r2v.bat',batchfile);
               //File2Trash(fName);
               {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL_warp GetGDALinfo out, cmd=' + cmd); {$EndIf}
            end;
         end;
      end;


      procedure ParseForMapCenter(TStr: ANSIstring; var Lat,Long : float64);
      //input from GDALinfo
      var
         DegStr,MinStr,SecStr : shortstring;
         Mult : float64;
      begin
          if StrUtils.AnsiContainsText(TStr,'CENTER') and StrUtils.AnsiContainsText(TStr,'(') then begin
            //Center      (  723282.207, 4270968.445) ( 78d26'14.85"W, 38d33'32.88"N)
             TStr := AfterSpecifiedString(TStr,')');
             ReplaceCharacter(TStr,'(',' ');
             ReplaceCharacter(TStr,',',' ');
             ReplaceCharacter(TStr,')',' ');
             StripBlanks(TStr);
             DegStr := BeforeSpecifiedCharacterANSI(TStr,'D',true,true);
             MinStr := BeforeSpecifiedCharacterANSI(TStr,chr(39),true,true);
             SecStr := BeforeSpecifiedCharacterANSI(TStr,chr(34),true,true);
             if Tstr[1] in ['E','N'] then Mult := 1 else Mult := -1;
             Long := Mult * (StrToFloat(DegStr) + StrToFloat(MinStr) / 60 + StrToFloat(SecStr) / 3600);
             Delete(TStr,1,1);
             DegStr := BeforeSpecifiedCharacterANSI(TStr,'D',true,true);
             MinStr := BeforeSpecifiedCharacterANSI(TStr,chr(39),true,true);
             SecStr := BeforeSpecifiedCharacterANSI(TStr,chr(34),true,true);
             if Tstr[1] in ['E','N'] then Mult := 1 else Mult := -1;
             Lat := Mult * (StrToFloat(DegStr) + StrToFloat(MinStr) / 60 + StrToFloat(SecStr) / 3600);
          end;
      end;


      function ParseForFips(Tstr : ANSIstring) : int16;
      //input from GDALinfo
      begin
          if StrUtils.AnsiContainsText(TStr,'FIPS_') then begin
             TStr := AfterSpecifiedString(TStr,'FIPS_');
             TStr := BeforeSpecifiedString(TStr,'_');
             Result := StrToInt(TStr);
          end;
      end;

      procedure GDALregister(LatLong : boolean; GISNum : Integer; ImageName : PathStr; LatHemi : AnsiChar);
      var
         BatchFile : tStringList;
         Code : integer;
         cmd : ANSIString;
         OutName : PathStr;
      begin
         LatLong := true;
         if IsGDALFilePresent(GDAL_translate_name) then begin
            if LatLong then OutName := ChangeFileExt(ImageName,'_latlong.tif')
            else OutName := ChangeFileExt(ImageName,'_utm.tif');
            if FileExists(OutName) then  begin
               if AnswerIsYes('Overwrite existing ' + OutName) then
                  SysUtils.DeleteFile(OutName)
               else exit;
            end;
            StartGDALbatchFile(BatchFile);
            //Code := 32600;      {WGS84 default}
            //if LatHemi= 'S' then Code := Code + 100;
            //Code := Code + MdDEF.DefaultUTMZone;
            Code := 4326;
            cmd := GDAL_translate_name + ' -a_srs EPSG:' + IntToStr(Code);
            GISdb[GISNum].MyData.First;
            while not GISdb[GISNum].MyData.eof do begin
               cmd := cmd + ' -gcp ' + GISdb[GISNum].MyData.GetFieldByNameAsString('X_IMAGE') + ' ' + GISdb[GISNum].MyData.GetFieldByNameAsString('Y_IMAGE');
               if LatLong then cmd := cmd + ' ' + GISdb[GISNum].MyData.GetFieldByNameAsString('LONG') + ' ' + GISdb[GISNum].MyData.GetFieldByNameAsString('LAT')
               else cmd := cmd +' ' + GISdb[GISNum].MyData.GetFieldByNameAsString('X_UTM') + ' ' + GISdb[GISNum].MyData.GetFieldByNameAsString('Y_UTM');
               GISdb[GISNum].MyData.Next;
            end;
            cmd := cmd +  ' ' + ImageName + ' ' + OutName;
            BatchFile.Add(cmd);
            EndBatchFile(MDTempDir + 'gdal_register.bat',batchfile);
         end;
      end;


      procedure GDALreprojectLASfile(fName : PathStr; T_EPSG,a_EPSG : integer);
      var
         BatchFile : tStringList;
      begin
         StartGDALbatchFile(BatchFile);
         BatchFile.Add('C:\OSGeo4W64\bin\las2las --t_srs EPSG:' + IntToStr(T_EPSG)+ ' --a_srs EPSG:' + IntToStr(T_EPSG)+ ' -i ' + fName + '  -o ' +
         ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_utm.las');
         EndBatchFile(MDTempDir + 'r2v.bat',batchfile);
      end;


      procedure GDALcreatemultibandTIFF;
      var
         FileNames : tStringList;
         DefaultFilter : byte;
         flist,my_vrt,OutName : PathStr;
         cmd : shortstring;
      begin
      //gdalbuildvrt -separate -input_file_list my_files.txt my.vrt
      //gdal_translate -projwin ulx uly lrx lry my.vrt my.tif

         FileNames := tStringList.Create;
         FileNames.Add(ExtractFilePath(LastImageName));
         DefaultFilter := 0;
         if GetMultipleFiles('TIFF files to create multiband','Image|*.tif',FileNames,DefaultFilter) then begin
            Petmar.GetFileNameDefaultExt('one band tiff','*.tif',OutName);
            fList := MDtempDir + 'vrt_files.txt';
            FileNames.SaveToFile(fList);
            FileNames.Free;
            my_vrt := MDtempDir + 'vrt.txt';
            cmd := ExtractShortPathName(GDALtools_Dir) + 'gdalbuildvrt -separate -input_file_list ' + flist + ' ' + my_vrt;
            WinExecAndWait32(cmd);
            cmd := GDAL_translate_name + ' ' + my_vrt + ' ' + OutName;
            WinExecAndWait32(cmd);
         end;
      end;


      procedure GDALBandExtraction;
      var
         FilesWanted : tStringList;
         DefaultFilter : byte;
         fName,fName2 : PathStr;
         i,j,NumBands : integer;
         Extra : shortString;
      begin
         {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.GDALbandextraction1Click in'); {$EndIf}
         FilesWanted := tStringList.Create;
         FilesWanted.Add(MainMapData);
         DefaultFilter := 1;
         if GetMultipleFiles('Image for band extraction','TIFF|*.tif;*.tiff|IMG|*.img',FilesWanted,DefaultFilter) then begin
            for I := 0 to pred(FilesWanted.Count) do begin
               fName := FilesWanted.Strings[i];
               Numbands := BandsInGeotiff(fName);
               (*
               if StrUtils.AnsiContainsText(UpperCase(fName),'_SR_8B_') then NumBands := 8
               else if StrUtils.AnsiContainsText(UpperCase(fName),'_4B_') then NumBands := 4
               else if StrUtils.AnsiContainsText(UpperCase(fName),'_3B_') then NumBands := 3
               else begin
                  NumBands := 84;
                  ReadDefault('bands to extract',NumBands);
               end;
               *)
               {$IfDef RecordReformat} WriteLineToDebugFile(fName); {$EndIf}
               StartProgress('Extract');
               for j := 1 to NumBands do begin
                  UpDateProgressBar(j/NumBands);
                  fName2 := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_b' + IntToStr(j) + '.tif';
                  Extra := ' -b ' + IntToStr(j);
                  GDAL_Translate_2_geotiff(fName,fName2,Extra,False);
               end;
            end;
         end;
         EndProgress;
         {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.GDALbandextraction1Click out'); {$EndIf}
      end;


      procedure GDALSubsetSatImageToMatchMap(MapOwner : tMapForm; GDAL_program : PathStr);
      var
         bfile : tStringList;
         i : integer;
         OutPath : PathStr;
         xmin, ymin, xmax, ymax : float64;


             procedure AddBand(BandName : PathStr);
             begin
                if GDAL_program = GDAL_warp_Name then bfile.Add(GDAL_program + ' -r near -te ' +  GDALextentBoxUTM(xmin,ymin,xmax,ymax)  + ' ' + BandName + ' ' +  OutPath + ExtractFileName(BandName))
                else bfile.Add(GDAL_program + ' -r near -projwin ' +  GDALextentBoxUTM(xmin,ymax,xmax,ymin) + ' ' + BandName + ' ' +  OutPath + ExtractFileName(BandName));
             end;

      begin
      //gdal_translate -of GTiff  C:\Users\pguth_2\Downloads\world_climate2_30sec\wc2.0_30s_tavg\wc2.0_30s_tavg_01.tif  c:\temp\subset.tif  -projwin -80 45 -70 30
         if IsGDALFilePresent(GDAL_program) then begin
            {$IfDef RecordGDAL} WriteLineToDebugFile('GDALSubsetSatImageToMatchMap in ' + GDAL_program + '  ' + ExtractFilePath(SatImage[MapOwner.MapDraw.SATonMap].IndexFileName)); {$EndIf}
            OutPath := LastSatDir;
            OutPath := Copy(OutPath, 1,pred(length(OutPath)));
            OutPath := NextFilePath(OutPath + '_subset');
            SafeMakeDir(OutPath);

            MapOwner.MapDraw.ScreenToUTM(0,0, xmin,ymax);
            MapOwner.MapDraw.ScreenToUTM(MapOwner.MapDraw.MapXSize,MapOwner.MapDraw.MapYSize, xmax,ymin);
            {$IfDef RecordGDAL} WriteLineToDebugFile('Map limits from screen: ' + GDALextentBoxUTM(xmin,ymax,xmax,ymin)); {$EndIf}
            StartGDALbatchFile(bFile);
            for i := 1 to SatImage[MapOwner.MapDraw.SATonMap].NumBands do begin
               bfile.Add('REM  Band ' + IntToStr(i) + '/' + IntToStr(SatImage[MapOwner.MapDraw.SATonMap].NumBands));
               AddBand(SatImage[MapOwner.MapDraw.SATonMap].BFileName[i]);
            end;
            if (SatImage[MapOwner.MapDraw.SATonMap].LandsatNumber <> 0) then begin
               AddBand(SatImage[MapOwner.MapDraw.SATonMap].GetLandsatBQAName);
               CopyFile(SatImage[MapOwner.MapDraw.SATonMap].GetLandsatMetadataName, OutPath + ExtractFileName(SatImage[MapOwner.MapDraw.SATonMap].GetLandsatMetadataName));
            end;
            EndBatchFile(Petmar.NextFileNumber(MDTempDir, ExtractFileNameNoExt(GDAL_program) +'_subset_sat_','.bat'),bfile);
         end
         else begin
            {$IfDef RecordGDAL} WriteLineToDebugFile('GDALSubsetSatImageToMatchMap fails, missing ' + GDAL_program); {$EndIf}
         end;
      end;

      function GDALsubsetimageandopen(bb : sfBoundBox; LatLongBox : boolean; fName : PathStr; BaseOutPath : PathStr = '') : integer;
      var
         OutPath,OutName : PathStr;
         LandCover,TStr,ExtentBoxString   : shortstring;
         Imagebb : sfBoundBox;
         GDALinfo : tGDALinfo;
         Ext : ExtStr;
      begin
         if IsGDALFilePresent(GDAL_Translate_Name) then begin
            if (fName <> '') and FileExists(fname) then begin
            end
            else begin
               fName := ExtractFilePath(LastDataBase);
               if not Petmar.GetExistingFileName('image to subset and import','image|*.tif;*.ecw',fName) then exit;
            end;
            CheckFileNameForSpaces(fName);
            if (BaseOutPath = '') or (not PathIsValid(BaseOutPath)) then BaseOutPath := MDtempdir;
            OutPath := NextFilePath(BaseOutPath + ExtractFileNameNoExt(fName) + '_subset_');
            OutName := OutPath + ExtractFileName(fName);
            {$IfDef RecordSubsetOpen} WriteLineToDebugFile('GDALsubsetimageandopen ' + ExtractFileName(fname) + '  ' + sfBoundBoxToString(ImageBB,4)); {$EndIf}

            Ext := UpperCase(ExtractFileExt(fName));
            if (Ext = '.TIF') or (Ext = '.TIFF') then Imagebb := GeotiffBBox(fName)
            else begin
               GetGDALinfo(fName, GDALinfo);
            end;

            if LatLongBox then begin
               ExtentBoxString := GDALextentBoxLatLong(bb);
            end
            else begin
               ExtentBoxString := GDALextentBoxUTM(bb);
            end;

            if AtLeastPartOfBoxInAnotherBox(ImageBB, bb) then begin
               if IsThisLandCover(fName,LandCover) then TStr := ' -r nearest '
               else TStr := '';
               GDALcommand(MDTempDir + 'raster_subset.bat',GDAL_Translate_Name + ' ' + ExtentBoxString + TStr + fName + ' ' + OutName);
               if FileExists(OutName) then begin
                  Result := OpenNewDEM(OutName);
                  {$IfDef RecordSubsetOpen} WriteLineToDebugFile('Grid opened ' + IntToStr(Result)); {$EndIf}
               end
               else begin
                  GeotiffMetadata(mdMicrodem, fName);
                  if not HeavyDutyProcessing then MessageToContinue('Failure; does image really cover this map area?  Check Metadata for image extent');
               end;
            end
            else begin
               if (not HeavyDutyProcessing) then begin
                  MessageToContinue('Map area is not covered by ' + ExtractFileNameNoExt(fName) + '  ' + sfBoundBoxToString(ImageBB,4));
                  {$IfDef RecordSubsetOpen} WriteLineToDebugFile('Not covered on map'); {$EndIf}
               end;
            end;
         end;
      end;


      procedure GetFilesNamesForGDALtranslate(var InName,OutName : Pathstr; TempStorage : boolean = false);
      var
         tName : PathStr;
      begin
         {$IfDef RecordReformat} WriteLineToDebugFile('Enter GetFilesNamesForGDALtranslate, in=' + InName); {$EndIf}
         if (UpperCase(ExtractFileExt(InName)) = '.TIF') or (UpperCase(ExtractFileExt(InName)) = '.TIFF') then begin
             if TempStorage then begin
                OutName := MDtempdir + ExtractFileName(InName);
             end
             else begin
                tName := ExtractFilePath(InName) + 'original_' + ExtractFileNameNoExt(InName) + '.tif';
                SysUtils.RenameFile(InName,tName);
                OutName := ExtractFilePath(InName) + ExtractFileNameNoExt(InName) + '.tif';
                InName := tName;
             end;
         end
         else if (UpperCase(ExtractFileExt(InName)) = '.ADF') then begin
            OutName := ExtractFilePath(InName) + LastSubDir(InName) + '.tif';
         end
         else begin
            OutName := ExtractFilePath(InName) + ExtractFileNameNoExt(InName) + '.tif';
         end;
         {$IfDef RecordReformat} WriteLineToDebugFile('Exit GetFilesNamesForGDALtranslate, in=' + InName + '  out=' + OutName); {$EndIf}
      end;


      function GDAL_Translate_2_geotiff(fName : PathStr; OutName : PathStr = ''; ExtraOptions : ANSIString = ''; TrashOriginal : boolean = true) : PathStr;
      var
         BatchFile : tStringList;
         BatchName : PathStr;
      begin
         {$If Defined(RecordGDAL) or Defined(Reformat)} WriteLineToDebugFile(' GDAL_Translate_2_geotiff in with ' + GDAL_translate_name); {$EndIf}
         if IsGDALFilePresent(GDAL_translate_name) then begin
            if (OutName = '') then GetFilesNamesForGDALtranslate(fName,OutName);
            Result := OutName;
            StartGDALbatchFile(BatchFile);
            BatchFile.Add(GDAL_translate_name + ' -of Gtiff '  + ExtraOptions + ' ' + fName + ' ' + OutName);
            BatchName := Petmar.NextFileNumber(MDTempDir, 'gdal_translate_','.bat');
            EndBatchFile(BatchName,BatchFile);
            if TrashOriginal then File2Trash(fName);
            {$If Defined(RecordGDAL) or Defined(Reformat)} WriteLineToDebugFile(' GDAL_Translate_2_geotiff end'); {$EndIf}
         end;
      end;


      procedure GDALConvert4BitGeotiff(fName : PathStr);

            function BuildCommand(fName : PathStr) : shortstring;
            var
               InName,OutName : PathStr;
            begin
               InName := fName;
               {$IfDef RecordReformat} WriteLineToDebugFile('reformat: ' + InName); {$EndIf}
               GetFilesNamesForGDALtranslate(InName,OutName);
               Result := GDAL_translate_name + GDAL_Geotiff_str + ' -ot byte -co NBITS=8 ' + InName + ' ' + OutName;
            end;

      begin
         {$IfDef RecordReformat} WriteLineToDebugFile('GDALConvert4BitGeotiff in'); {$EndIf}
         if IsGDALFilePresent(GDAL_translate_name) then begin
            WinExecAndWait32(BuildCommand(fName));
            {$IfDef RecordReformat} WriteLineToDebugFile('GDALConvert4BitGeotiff out'); {$EndIf}
         end;
      end;


      procedure GDALConvertSingleImageToGeotiff(var fName : PathStr);
      //if you want to keep the compressed DEM and uncompress to temporary storage, use this
      var
         OutName : PathStr;
         cmd : shortstring;
         LandCover : ShortString;
      begin
         {$IfDef RecordReformat} WriteLineToDebugFile('GDALConvertSingeImageToGeotiff in ' + fName); {$EndIf}
         if IsGDALFilePresent(GDAL_translate_name) then begin
            GetFilesNamesForGDALtranslate(fName,OutName,true);
            if not IsThisLandCover(ExtractFileName(fName),LandCover) then begin
               if IsThisLandCover(fName,LandCover) then begin
                  Outname := ExtractFilePath(OutName) + LandCover + '_' + ExtractFileName(fName);
               end;
            end;
            cmd := GDAL_translate_name + GDAL_Geotiff_str + fName + ' ' + OutName;
            WinExecAndWait32(cmd);
            fName := OutName;
            {$IfDef RecordReformat} WriteLineToDebugFile('GDALConvertSingeImageToGeotiff out, cmd=' + cmd); {$EndIf}
         end;
      end;


      procedure GDALConvertImagesToGeotiff(fName : PathStr = ''; Recycle : boolean = true);
      var
         FilesWanted,BatchFile,RecycleNames : tStringList;
         DefaultFilter : byte;
         InName,OutName : PathStr;
         i : integer;

            function BuildCommand(i : integer) : shortstring;
            begin
               InName := FilesWanted.Strings[i];
               {$IfDef RecordReformat} WriteLineToDebugFile('reformat: ' + InName); {$EndIf}
               GetFilesNamesForGDALtranslate(InName,OutName);
               Result := GDAL_translate_name + GDAL_Geotiff_str + InName + ' ' + OutName;
               if Recycle then RecycleNames.Add(InName);
            end;

      begin
         {$IfDef RecordReformat} WriteLineToDebugFile('GDALConvertImagesToGeotiff in'); {$EndIf}
         if IsGDALFilePresent(GDAL_translate_name) then begin
            FilesWanted := tStringList.Create;
            if (FName <> '') then begin
               FilesWanted.Add(fName);
            end
            else begin
               FilesWanted.Add(MainMapData);
               DefaultFilter := 1;
               GetMultipleFiles('Images to convert to Geotiff','TIFF|*.tif;*.tiff;*.img;*.jp2',FilesWanted,DefaultFilter);
            end;
            RecycleNames := tStringList.Create;
            if (FilesWanted.Count = 1) then begin
               WinExecAndWait32(BuildCommand(0));
            end
            else if (FilesWanted.Count > 0) then begin
               StartGDALbatchFile(BatchFile);
               for I := 0 to pred(FilesWanted.Count) do begin
                  BatchFile.Add(BuildCommand(I));
               end;
               EndBatchFile(Petmar.NextFileNumber(MDTempDir,'gdal_translate_', '.bat'),BatchFile);
            end;
            for I := 0 to pred(RecycleNames.Count) do begin
               File2Trash(RecycleNames.Strings[i]);
            end;

            FilesWanted.Free;
            {$IfDef RecordReformat} WriteLineToDebugFile('GDALConvertImagesToGeotiff out'); {$EndIf}
         end;
      end;


      function ReprojectShapeFileToGeographic(var fName : Pathstr; aDir : PathStr) : boolean;
      var
         NewName,fName2 : PathStr;
      begin
         fName2 := ExtractShortPathName(GDAL_ogr_Name);
         NewName := aDir + ExtractFileName(fName);
         if not FileExists(ChangeFileExt(fName,'.prj')) then begin
            MessageToContinue('Cannot reproject shapefile without PRJ file, ' + fName);
            Result := false;
            exit;
         end;
         if FileExists(NewName) then begin
         end
         else if IsGDALFilePresent(fName2) then begin
            GDALcommand(MDTempDir + 'shp2geo.bat',fName2 + ' -t_srs EPSG:4326 ' +  aDir + ' ' + fName);
         end;
         fName := NewName;
         Result := FileExists(fName);
      end;


      procedure GDALreprojectshapefile;
      var
         fName,aDir : PathStr;
         DefaultFilter : byte;
         FileNames : tStringList;
         i: Integer;
         OK : boolean;
      begin
         {$IfDef RecordReformat} WriteLineToDebugFile('TDemHandForm.GDALreprojectshapefile1Click in'); {$EndIf}
         FileNames := tStringList.Create;
         FileNames.Add(DBDir);
         DefaultFilter := 0;
         aDir := DBDir;
         if GetMultipleFiles('Shape file to reproject','shape file|*.shp',FileNames,DefaultFilter) then begin
            fName := FileNames.Strings[0];
            repeat
               GetDOSPath('reprojected file ' + ExtractFileName(fName),aDir);
               OK := UpperCase(aDir) <> UpperCase(ExtractFilePath(fName));
               if Not OK then MessageToContinue('Cannot write in the same directory');
            until OK;
            if aDir[length(aDir)] = '\' then Delete(aDir,length(aDir),1);

            for i := 0 to pred(FileNames.Count) do begin
               fName := FileNames.Strings[i];
               {$IfDef RecordReformat} WriteLineToDebugFile('do=' + fName); {$EndIf}
               if StrUtils.AnsiContainsText(fName,' ') then fName := ExtractShortPathName(fName);
               ReprojectShapeFileToGeographic(fName,aDir);
           end;
        end;
        FileNames.Free;
      end;


      const
         ogr2ogr_params = ' -skipfailures -overwrite -progress -t_srs EPSG:4326';


      procedure GDALGeodatabasetoshapefile;
      var
         fName,aDir,OutDir : PathStr;
      begin
         aDir := DBDir;
         GetDOSPath('Geodatabase',aDir);
         if (aDir = '') then Exit;
         if StrUtils.AnsiContainsText(aDir,' ') then fName := ExtractShortPathName(aDir);

          if IsGDALFilePresent(GDAL_info_Name) then begin
            GDALCommand(MDTempDir + 'r2v.bat_part2.bat',GDAL_info_Name + ' '  + aDir);
          end;

          if IsGDALFilePresent(GDAL_ogr_Name) then begin
            OutDir := adir + '_shapefiles\';
            GDALCommand(MDTempDir + 'r2v_part2.bat',GDAL_ogr_Name + ' ' +  OutDir + ' ' + aDir + ogr2ogr_params);
          end;
      end;


      procedure GDAL_Convert_Shapefile_2_geopackage(fName : pathStr);
      var
         NewDir : PathStr;
         cmd : shortstring;
      begin
//convert a shapefile to geopackage
//$ ogr2ogr -f GPKG filename.gpkg abc.shp
//all the files (shapefile/geopackage) will be added to one geopackage.
//$ ogr2ogr -f GPKG filename.gpkg ./path/to/dir
          cmd := GDAL_ogr_Name + ' -f GPKG ' + ChangeFileExt(fName,'.gpkg') + ' ' + fName  + ' -progress -t_srs EPSG:4326';
          GDALCommand(MDTempDir + 'shp2gkpg.bat',cmd);
          //NewDir := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_shapefile_wgs84';
          //GDALCommand(MDTempDir + 'shp2wgs.bat',GDAL_ogr_Name + ' ' + NewDir + ' ' + fname + ogr2ogr_params);
      end;



      procedure GeneralConvertToWGS84Shapefile(fName : pathStr);
      var
         NewDir : PathStr;
      begin
          NewDir := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_shapefile_wgs84';
          GDALCommand(MDTempDir + 'shp2wgs.bat',GDAL_ogr_Name + ' ' + NewDir + ' ' + fname + ogr2ogr_params);
      end;


      function ExtractMapCoverageToWGS84Shapefile(fName : pathStr; BoundBox : sfBoundBox) : PathStr;
      var
         NewDir : PathStr;
         CMD : ANSIstring;
         i : integer;
      begin
         {$IfDef RecordOGR} WriteLineToDebugFile('ExtractMapCoverageToWGS84Shapefile ' + fName); {$EndIf}
          i := 1;
          repeat
             NewDir := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_shapefile_extract_' + intToStr(i);
             inc(i);
          until not PathIsValid(NewDir);
         {$IfDef RecordOGR} WriteLineToDebugFile('ExtractMapCoverageToWGS84Shapefile to' + NewDir); {$EndIf}

          cmd := GDAL_ogr_Name + ' ' + NewDir + ' ' + fname + ogr2ogr_params + ' -s_srs EPSG:4326' + ' -clipdst ' + GDALextentBoxGeo(BoundBox);
          Result := NewDir + '\' + ExtractFileName(fName);

          GDALCommand(MDTempDir + 'extract_shp2wgs.bat',cmd);
          {$IfDef RecordOGR} WriteLineToDebugFile('New file = ' + Result); {$EndIf}
      end;


      function OGRrewriteShapefile(fName : pathStr) : PathStr;
      var
         NewDir : PathStr;
         CMD : ANSIstring;
      begin
         NewDir := ExtractFilePath(fName);
         GetDOSPath('New shapefile',NewDir);
         cmd := GDAL_ogr_Name + ' ' + NewDir + ' ' + fname + ogr2ogr_params + ' -s_srs EPSG:4326';
         Result := NewDir + '\' + ExtractFileName(fName);
         GDALCommand(MDTempDir + 'extract_shp2wgs.bat',cmd);
      end;

      procedure GDAL_Convert_JSON(var fName : pathStr);
      var
         NewName : PathStr;
         CMD : ANSIstring;
      begin
          if IsGDALFilePresent(GDAL_ogr_Name) then begin
             NewName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '.shp';
             cmd := GDAL_ogr_Name + ' -f "ESRI Shapefile" ' + NewName + ' ' + fname;   // + ' OGRGeoJSON';
             GDALCommand(MDTempDir + 'gpx2shp.bat',cmd);
          end;
      end;


      procedure GDAL_ConvertGPXToSHP(var fName : pathStr);
      var
         NewName : PathStr;
         CMD : ANSIstring;

               function RecordsInDataBase(fName : PathStr) : integer;
               var
                 f : tMyData;
               begin
                  f := tMyData.Create(fName);
                  Result := f.RecordCount;
                  f.Destroy;
               end;

      begin
          if IsGDALFilePresent(GDAL_ogr_Name) then begin
             NewName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_shapefile';
             cmd := GDAL_ogr_Name + ' ' + NewName + ' ' + fname + ' -fieldTypeToString DateTime ' + ogr2ogr_params;
             GDALCommand(MDTempDir + 'gpx2shp.bat',cmd);
             fName := NewName + '\route_points.dbf';
             if (RecordsInDataBase(fName) = 0) then fName := NewName + '\track_points.dbf';
          end;
      end;


      function IsGDALFilePresent(fName : PathStr) : boolean;
      begin
         Result := FileExists(fName);
         if (not Result) and (fName <> '') then begin
            MessageToContinue('Missing GDAL file required ' + fName);
          end;
      end;


      function GDAL_translateUTM(InName,OutName : PathStr; WGS84,NHemi : boolean;  UTMzone : int16) : shortstring;
      var
         Code : integer;
      begin
         Code := 26900;      {NAD83}
         if WGS84 then Code := 32600;      {WGS84 default}
         if not NHemi then Code := Code + 100;
         Code := Code + UTMZone;
         Result := GDAL_translate_name + ' -of Gtiff' + ' -a_srs EPSG:' + IntToStr(Code) + ' ' + inName + ' ' + OutName;
      end;


      procedure StartGDALbatchFile(var BatchFile : tStringList);
      begin
         BatchFile := tStringList.Create;
         BatchFile.Add('call "C:\OSGeo4W\bin\o4w_env.bat"');
         BatchFile.Add(SetGDALdataStr);
      end;

      procedure UseGDAL_Warp_to_merge(var MergefName : PathStr; OutNames : tStringList);
      //gdalwarp --config GDAL_CACHEMAX 3000 -wm 3000 $(list_of_tiffs) merged.tiff
      //this option did not work (could not get the list of input files to be acceptable
      var
         aName,OutVRT : PathStr;
         cmd : shortstring;
         BatchFile : tStringList;
      begin
         aName :=  Petmar.NextFileNumber(MDTempDir, 'gdal_merge_file_list_','.txt');
         OutNames.SaveToFile(aName);

         OutVRT := Petmar.NextFileNumber(MDTempDir, 'gdal_vrt_','.vrt');
         cmd := GDALtools_Dir + 'gdalwarp --config GDAL_CACHEMAX 3000 -wm 3000 --input_file_list ' + aName + ' ' + ' ' + MergeFName;

         StartGDALbatchFile(BatchFile);
         BatchFile.Add('REM create VRT');
         BatchFile.Add(cmd);

         aName :=  Petmar.NextFileNumber(MDTempDir, 'gdal_warp2merge_','.bat');
         EndBatchFile(aName,BatchFile);
      end;


      procedure UseGDAL_VRT_to_merge(var MergefName : PathStr; OutNames : tStringList; Added : ShortString = '');
      var
         aName,OutVRT : PathStr;
         cmd : shortstring;
         BatchFile : tStringList;
      begin
         aName :=  Petmar.NextFileNumber(MDTempDir, 'gdal_merge_file_list_','.txt');
         OutNames.SaveToFile(aName);
         OutVRT := Petmar.NextFileNumber(MDTempDir, 'gdal_vrt_','.vrt');
         cmd := GDALtools_Dir + 'gdalbuildvrt ' + Added + ' -input_file_list ' + aName + ' ' + OutVRT;

         StartGDALbatchFile(BatchFile);
         BatchFile.Add('REM create VRT');
         BatchFile.Add(cmd);
         cmd := GDALtools_Dir + 'gdal_translate -of GTiff ' + OutVrt + ' ' + MergefName;
         BatchFile.Add(cmd);

         aName :=  Petmar.NextFileNumber(MDTempDir, 'vrt2merge_','.bat');
         EndBatchFile(aName,BatchFile);
      end;


      procedure CallGDALMerge(var MergefName : PathStr; OutNames : tStringList; MissingData : integer = 255);
      var
         BatchFile : tStringList;
         cmd : ANSIString;
         i : integer;
         DefFilter : byte;
         fName : PathStr;
      begin
         if (OutNames = Nil) then begin
            OutNames := tStringList.Create;
            OutNames.Add(MainMapData);
            DefFilter := 1;
            if not GetMultipleFiles('Files for GDAL merge','TIFF files|*.TIF',OutNames,DefFilter) then exit;
         end;
         if (MergefName = '') then begin
            MergefName := ExtractFilePath(OutNames[0]);
            Petmar.GetFileNameDefaultExt('Merged filename','*.tif',MergefName);
         end;

         fName :=  Petmar.NextFileNumber(MDTempDir, 'gdal_merge_file_list_','.txt');
         OutNames.SaveToFile(fName);

         StartGDALbatchFile(BatchFile);
         BatchFile.Add('REM Merge tiffs');
         cmd := PythonEXEname + ' ' + PythonScriptDir + 'gdal_merge.py -a_nodata ' + IntToStr(MissingData) + ' -o ' + MergefName
           +  ' --optfile ' + fName;
         for i := 0 to pred(OutNames.Count) do begin
            cmd := cmd + ' ' + OutNames[i];
         end;
         BatchFile.Add(cmd);
         fName :=  Petmar.NextFileNumber(MDTempDir, 'gdalmerge_','.bat');
         EndBatchFile(fName,BatchFile);
      end;


      function GDALinfoOutput(fname : PathStr) : PathStr;
      begin
         Result := ChangeFileExt(fName, '_gdal_info.txt');
      end;


      procedure GetGDALinfo(fName : PathStr; var GDALinfo : tGDALinfo);
      var
         OutName : PathStr;
         cmd,TStr : ANSIString;
         Metadata : tStringList;
         j : integer;
      begin
         {$IfDef RecordGDAL} WriteLineToDebugFile('GetGDALinfo in ' + fName); {$EndIf}
         ZeroGDALInfo(GDALinfo);
         OutName := GDALinfoOutput(fname);

         if not FileExists(outName) then begin
            cmd := GDAL_info_name  + ' ' + fName + AddWKT + ' >' + OutName;
            GDALCommand(MDTempDir + 'gdal_info.bat',cmd);
         end;

         Metadata := tStringList.Create;
         Metadata.LoadFromFile(OutName);
         for j := 0 to pred(Metadata.Count) do begin
             TStr := UpperCase(trim(MetaData.Strings[j]));
             StripBlanks(TStr);
             GDALinfo.FIPS := ParseForFips(TStr);
             if StrUtils.AnsiContainsText(TStr,'NAD83') then GDALInfo.HDatum := 'NAD83';
             if StrUtils.AnsiContainsText(TStr,'WGS84') then GDALInfo.HDatum := 'WGS84';
             if StrUtils.AnsiContainsText(TStr,'ERTS98') then GDALInfo.HDatum := 'ETR89';
             if StrUtils.AnsiContainsText(TStr,'PROJCS["WGS_1984_Web_Mercator_Auxiliary_Sphere"') then GDALInfo.inEPSG := 3857;
             FindUTMZone(TStr,GDALinfo.UTMzone,GDALInfo.Hemi);
             ParseForMapCenter(TStr,GDALInfo.cLat,GDALInfo.cLong);
         end;
         Metadata.Free;
         GetEPSG(GDALinfo);
         {$IfDef RecordGDAL} WriteLineToDebugFile('GetGDALinfo out ' + fName); {$EndIf}
      end;


      procedure BatchGDALSRSinfo(Infiles : tStringList);
      var
         fName,OutName : PathStr;
         cmd : ANSIString;
         BatchFile: tStringList;
         i : integer;
      begin
         {$IfDef RecordGDAL} WriteLineToDebugFile('BatchGDALSRSinfo in'); {$EndIf}
         StartGDALbatchFile(BatchFile);
         for i := 0 to pred(infiles.Count) do begin
             fName := Infiles.Strings[i];
             OutName := MDTempDir + ExtractFileName(fName) + '.prj';
             if not FileExists(OutName) then begin
                cmd := GDAL_srs_info_name  + ' -o wkt ' + fName + ' >' + OutName;
                BatchFile.Add(Cmd);
             end;
         end;
         EndBatchFile(MDTempDir + 'gdal_srs_info.bat',BatchFile);
      end;


      procedure BatchGDALinfo(Infiles : tStringList; GetClipBox : boolean; var UTMZone : int16);
      const
         QuadTickSize = 7.5/60;
      var
         fName,OutName,BatchName : PathStr;
         cmd,TStr,Coords : ANSIString;
         BatchFile,Metadata : tStringList;
         i,j,fNeed : integer;
         Hemi : ANSIchar;
         Lat,Long : float64;
         UTMProjection  : tMapProjection;
         xutmC,yutmC, LatC,LongC : array[1..4] of float64;
      begin
         {$IfDef RecordGDAL} WriteLineToDebugFile('BatchGDALinfo in'); {$EndIf}
         fNeed := 0;
         StartGDALbatchFile(BatchFile);
         for i := 0 to pred(infiles.Count) do begin
             fName := Infiles.Strings[i];
             OutName := GDALinfoOutput(fName);
             if ExtEquals(fName,'.pdf') then TStr := ' -mdd LAYERS '
             else TStr := ' ';
             if not FileExists(OutName) then begin
                cmd := GDAL_info_name  + TStr + fName + AddWKT + ' >' + OutName;
                BatchFile.Add(Cmd);
                inc(fNeed);
             end;
         end;
         if (fNeed = 0) then BatchFile.Free
         else begin
            EndBatchFile(MDTempDir + 'gdal_info.bat',BatchFile);
            if GetClipBox then begin
               UTMProjection := tMapProjection.Create('utm gdal');
               UTMZone := 0;
               for i := 0 to pred(infiles.Count) do begin
                   OutName := GDALinfoOutput(Infiles.Strings[i]);
                   Metadata := tStringList.Create;
                   Metadata.LoadFromFile(OutName);
                   for j := 0 to pred(Metadata.Count) do begin
                      TStr := UpperCase(trim(MetaData.Strings[j]));
                      StripBlanks(TStr);
                      if FindUTMZone(TStr,UTMZone,Hemi) then begin
                      end;
                      ParseForMapCenter(TStr,Lat,Long);
                  end;
                  Metadata.Free;
                  if (Lat > 0) then Hemi := 'N' else Hemi := 'S';

                  UTMProjection.DefineDatumFromUTMZone('NAD83',UTMZone,Hemi);
                  LatC[1] := Lat + 0.5 * QuadTickSize;  //NE
                  LongC[1] := Long + 0.5 * QuadTickSize;
                  LatC[2] := Lat - 0.5 * QuadTickSize;   //SE
                  LongC[2] := Long + 0.5 * QuadTickSize;
                  LatC[3] := Lat - 0.5 * QuadTickSize;   //SW
                  LongC[3] := Long - 0.5 * QuadTickSize;
                  LatC[4] := Lat + 0.5 * QuadTickSize;   //NW
                  LongC[4] := Long - 0.5 * QuadTickSize;
                  if GetClipBox then begin
                      BatchFile := tStringList.Create;
                      BatchFile.Add('id,WKT');
                      Coords := '1,"POLYGON((';
                      for j := 1 to 4 do begin
                         QuadTickNearestHere(LatC[j],LongC[j],QuadTickSize);
                         UTMProjection.ForwardProjectDegrees(LatC[j],LongC[j],XUTMC[j],YUTMc[j]);
                         Coords := Coords + RealToString(XUTMC[j],-12,4) + ' ' +  RealToString(YUTMC[j],-12,4) + ',';
                      end;
                      Coords := Coords + RealToString(XUTMC[1],-12,4) + ' ' +  RealToString(YUTMC[1],-12,4) + '))"';
                      BatchFile.Add(Coords);
                      BatchName := MDTempDir + ExtractFileNameNoExt(Infiles.Strings[i]) + '_clip.csv';
                      BatchFile.SaveToFile(BatchName);
                  end;
               end;
               UTMProjection.Destroy;
            end;
         end;
      end;


initialization
finalization
  {$IfDef RecordGeoPDF} WriteLineToDebugFile('RecordGeoPDF active in gdal_tools'); {$EndIf}
  {$IfDef RecordGDAL} WriteLineToDebugFile('RecordGDAL active in gdal_tools'); {$EndIf}
  {$IfDef RecordOGR} WriteLineToDebugFile('RecordOGR active in gdal_tools'); {$EndIf}
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('RecordUseOtherPrograms active in gdal_tools'); {$EndIf}
  {$IfDef RecordReformat} WriteLineToDebugFile('RecordReformat active in gdal_tools'); {$EndIf}
  {$IfDef RecordSaveProblems} WriteLineToDebugFile('RecordSaveProblems active in gdal_tools'); {$EndIf}
end.
