unit gdal_tools;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

//these use Python from OSGeo4W and its batch file to set up environment
//   gdal_merge
//   gdal_edit
//   gdal_fillnodata
//   gdal_calc


{$IfDef RecordProblems}  //normally only defined for debugging specific problems

   {$IFDEF DEBUG}
      //{$Define RecordGDALOpen}
      //{$Define RecordSubsetOpen}
      //{$Define RecordDatumShift}
      //{$Define RecordDEMIX}
      //{$Define RecordWKT}
      //{$Define RecordProjectionStrings}
      //{$Define RecordDEMIXCompositeDatum}
      //{$Define RecordSubsetGDAL}
      //{$Define RecordGDALinfo}
      //{$Define RecordGDALOpen}
      //{$Define RecordSaveProblems}
      //{$Define RecordGDAL}
      //{$Define RecordOGR}
      //{$Define RecordReformat}
   {$Else}
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

const
   GDAL_testScaleFactor : shortstring = '';
   BiLinearString = ' -r bilinear ';
   BiCubicString = ' -r cubic ';

   type
      tgdalWarpMethod = (gdaltoGeoWGS84,gdalviaEPSG,gdaltoUTMNAD83,gdaltoadjacentUTMzone);
   var
      SetGDALdataStr : ANSIString;

   function GetGDALFileNames : boolean;
   function GetGDALversion : ANSIstring;
   function IsGDALFilePresent(fName : PathStr) : boolean;
   function GDALImageFormat(Ext : ExtStr; OnlyGDAL : boolean = false) :  boolean;
   function GDALGridFormat(Ext : ExtStr; OnlyGDAL : boolean = false) :  boolean;

   procedure GDALcommand(BatchName : PathStr; cmd : ShortString; Log : boolean = true);
   procedure StartGDALbatchFile(var BatchFile : tStringList);

   procedure GDAL_netcdf(fName : PathStr ='');

   function GDALextentBoxUTM(xmin,ymin,xmax,ymax : float64) : shortstring; overload;
   function GDALextentBoxUTM(BoundBox : sfBoundBox) : shortstring; overload;
   function GDALextentBoxLatLong(BoundBox : sfBoundBox) : shortString;
   function GDALextentBoxGeo(BoundBox : sfBoundBox) : shortstring;

   procedure ResaveAsGDALgeotiff(fName : PathStr);

   function GDALinfoOutputFName(fname : PathStr) : PathStr;
   procedure GetGDALinfo(fName : PathStr; var GDALinfo : tGDALinfo);
   procedure ZeroGDALinfo(var GDALinfo : tGDALinfo);
   function GetEPSG(var GDALinfo : tGDALinfo) : integer;
   procedure BatchGDALinfo(Infiles : tStringList; GetClipBox : boolean; var UTMZone : int16);
   procedure BatchGDALSRSinfo(Infiles : tStringList);
   procedure GDAL_Convert_JSON(var fName : pathStr);
   procedure GDAL_Fill_Holes(InName : PathStr);
   function GDAL_upsample_DEM(OpenMap : boolean; DEM : integer; Bilinear : boolean; Spacing : float32 = -99; OutName : PathStr = '') : integer;
   function GDAL_warp_DEM(OpenMap : boolean; DEM : integer; OutName : PathStr; xspace,yspace : float32; IntString : shortstring = biLinearString) : integer;
   function GDAL_downsample_DEM_1sec(OpenMap : boolean; DEM : integer; OutName : PathStr) : integer;
   //procedure GDAL_dual_UTM(DEM : integer);

   procedure GDAL_replace_42112_tag(DEMName : PathStr; ReplaceStr : shortstring = '');

   function GDAL_translateUTM(InName,OutName : PathStr; WGS84,NHemi : boolean;  UTMzone : int16) : shortstring;

   procedure GDALConvertImagesToGeotiff(fName : PathStr = ''; Recycle : boolean = true);
   function GDAL_Translate_2_geotiff(fName : PathStr; OutName : PathStr = ''; ExtraOptions : ANSIString = ''; TrashOriginal : boolean = true) : PathStr;
   function GDAL_warp(var fName : PathStr) : PathStr;
   function GDAL_warp_Multiple(var theFiles : tStringList) : PathStr;
   procedure GDAL_warp_reproject(How : tgdalWarpMethod);

   procedure GDALreprojectLASfile(fName : PathStr; T_EPSG,a_EPSG : integer);

   function GDALsubsetGridAndOpen(bb : sfBoundBox; LatLongBox : boolean; fName : PathStr; OpenMap : boolean; BaseOutPath : PathStr = '') : integer;
   procedure GDALConvert4BitGeotiff(fName : PathStr);

   procedure GDAL_Raster_Calculator(Expression : shortstring);

   function ExtractFromMonsterTIFFforBoundingBox(InName : PathStr; bb : sfBoundBox; OpenMap : boolean; ShortName : shortstring; OutfName : PathStr = '') : integer;


   function GDAL_DEM_command(OpenMap : boolean; InputDEM : integer; cmd : ANSIstring; OutName : PathStr; mt : byte = mtElevSpectrum; ElevUnits : byte = euUndefined) : integer;


   //create grids from DEM
      function GDAL_SlopeMap_ZT(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
      function GDAL_SlopeMap_Horn(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
      function GDAL_AspectMap_Horn(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
      function GDAL_AspectMap_ZT(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
      function GDAL_HillshadeMap_Horn(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
      function GDAL_TRI_Wilson(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
      function GDAL_TRI_Riley(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
      function GDAL_Roughness(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
      function GDAL_TPI(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;

   //shapefiles
      procedure GDAL_ConvertGPXToSHP(var fName : pathStr);
      procedure GDALGeodatabasetoshapefile;
      procedure GDAL_Convert_Shapefile_2_geopackage(fName : pathStr);
      procedure GDALreprojectshapefile;
      procedure GeneralConvertToWGS84Shapefile(fName : pathStr);
      function OGRrewriteShapefile(fName : pathStr) : PathStr;
      function ReprojectShapeFileToGeographic(var fName : Pathstr; aDir : PathStr) : boolean;
      function ExtractMapCoverageToWGS84Shapefile(fName : pathStr; BoundBox : sfBoundBox) : PathStr;

   //merge files
      procedure CallGDALMerge(var MergefName : PathStr; InNames : tStringList; MissingData : integer = 255);
      procedure UseGDAL_VRT_to_merge(var MergefName,OutVRT : PathStr; OutNames : tStringList; Added : ShortString = '');

   //satellite imagery
      procedure GDALBandExtraction;
      procedure GDALcreatemultibandTIFF;
      procedure GDALSubsetSatImageToMatchMap(MapOwner : tMapForm; GDAL_program : PathStr);
      procedure ResampleSentinel_1(Path : PathStr; Recycle : boolean = false);
      function GDALConvertSingleImageToGeotiff(var fName : PathStr) : boolean;

   //projection and datum issues
      procedure GDALAssignProjectionViaWKTorEPSG(DEMName,ProjWKTFileNameOrEPSG : PathStr);
      procedure CompositeDatumShiftWithGDAL(var InName,SaveName : shortstring; s_SRSstring,t_srsstring : shortstring);
      procedure ShiftAFile_UTM_WGS84_EGM2008(fName,SaveName : PathStr; VertCode,HorizCode,UTMzone : shortstring);

      procedure GDALGeotiffToWKT(fName : PathStr);
      function CreateWKTfileForGeotiff(fName : PathStr) : PathStr;
      function CreateWKTStringForGeotiff(fName : PathStr) : ANSIstring;

      procedure GDALregister(LatLong : boolean; GISNum : Integer; ImageName : PathStr; LatHemi : AnsiChar);
      procedure ShiftToUTM_WGS84_EGM2008(inName,SaveName : PathStr; s_SRSstring : shortString; UTMzone : integer);
      procedure VerticalDatumShiftWithGDALtoEGM2008(DEM : integer; var SaveName : PathStr);
      procedure MultipleGDALcompositedatumshift;

   {$IfDef IncludePython}
      procedure TestPythonFile;
   {$EndIf}

   {$IfDef ExGeoPDF}
   {$Else}
      type
         tGDALGeoPDF = (gdalOpenGeoPDFimagelayer1,gdalAllindividuallayers1,gdalOpenGeoPDF1,gdalMergeGeoPDF1);
      procedure GDALconvertGeoPDF(Option : tGDALGeoPDF);
   {$EndIf}


    function GDAL_WebExtractFromMonsterTIFFforBoundingBox(WebName : PathStr; bb : sfBoundBox; OpenMap : boolean; ShortName : shortstring; OutfName : PathStr = '') : integer;


implementation


uses
   DEMDef_routines,
   DEMCoord,
   DEMeros,
   Geotiff,
   PetDButils,
   DEM_Manager,
   DEMDataBase,
   PetMath,
   nevadia_main;

const
   ogr2ogr_params = ' -skipfailures -overwrite -progress -t_srs EPSG:4326';
   GDAL_Geotiff_str = ' -of Gtiff -co TILED=NO -co COMPRESS=NONE ';
   AddWKT = ' -wkt_format WKT2';
   RunDOSwindow  = ' run batch file in DOS window to see error message: ';

   {$IfDef ExGeoPDF}
   {$Else}
      {$I geopdf.inc}
   {$EndIf}

   {$IfDef IncludePython}
      procedure TestPythonFile;
      var
         BatchFile : tStringList;
         fName : PathStr;
      begin
         StartGDALbatchFile(BatchFile);
         BatchFile.Add('REM Test');
         BatchFile.Add(PythonEXEname + 'c:\temp\dem_ssim.py');
         fName := Petmar.NextFileNumber(MDTempDir, 'pytest_','.bat');
         EndBatchFile(fName,BatchFile);
      end;
   {$EndIf}


    function GDAL_WebExtractFromMonsterTIFFforBoundingBox(WebName : PathStr; bb : sfBoundBox; OpenMap : boolean; ShortName : shortstring; OutfName : PathStr = '') : integer;
    var
       cmd : ANSIString;
    begin
       Result := 0;
       if (OutfName = '') then OutfName := NextFileNumber(MDTempDir,ShortName,'.tif');
       cmd := GDAL_translate_name + ' --debug on /vsicurl/' + WebName + ' ' + GDALextentBoxLatLong(bb) +  ' ' + OutFName;
       if WinExecAndWait32(cmd) <> -1 then begin
       end;
       if FileExists(OutFName) then begin
          Result := OpenNewDEM(OutFName,OpenMap);
       end
       else Result := 0;
    end;


    function ExtractFromMonsterTIFFforBoundingBox(InName : PathStr; bb : sfBoundBox; OpenMap : boolean; ShortName : shortstring; OutfName : PathStr = '') : integer;
    begin
       Result := GDALsubsetGridAndOpen(bb,true,InName,false);
       if ValidDEM(Result) then begin
          DEMGlb[Result].AreaName := ShortName;
          if (OutfName <> '') then DEMGlb[Result].WriteNewFormatDEM(OutfName);
          if OpenMap then CreateDEMSelectionMap(Result,true,true,MDDef.DefaultElevationColors);
       end;
     end;


procedure GDAL_warp_reproject(How : tgdalWarpMethod);
var
   fName,fName2,outName : PathStr;
   DefaultFilter : byte;
   BatchFile,FileNames : tStringList;
   UTMzone,
   i : Integer;
   ch : ANSIchar;
   TStr2,outer,InProj,OutEPSG : shortString;
   cmd : ANSIString;
   GDALinfo : tGDALinfo;

   procedure GetTargetEPSG(What : shortstring);
   begin
      ch := 't';
      OutEPSG := '4326';
      GetString(What + ' EPSG code',OutEPSG,false,['0'..'9']);
   end;

begin
  {$IfDef RecordGDAL} WriteLineToDebugFile('TDemHandForm.GDALreprojectimagetoUTMNAD831Click in'); {$EndIf}
   ZeroGDALinfo(GDALinfo);
   FileNames := tStringList.Create;
   FileNames.Add(ExtractFilePath(LastImageName));
   DefaultFilter := 0;
   if not GetMultipleFiles('TIF file to reproject','Image|*.tif;*.jp2;*.pdf',FileNames,DefaultFilter) then Exit;
   inProj := '';
   fName := FileNames.Strings[0];
   if (How = gdalviaEPSG) then begin
      fName2 := GDAL_warp_name;
      TStr2 := 'warp';
      GetTargetEPSG('warped');
   end
   else if (How = gdaltoGeoWGS84) then begin
       fName2 := GDAL_warp_name;
       TStr2 := 'warp';
       ch := 't';
       OutEPSG := '4326';
       outer := 'geo';
   end
   else if (How = gdaltoadjacentUTMzone) then begin
       fName2 := GDAL_warp_name;
       PickUTMZone(MDdef.DefaultUTMZone);
       TStr2 := 'warp';
       ch := 't';
       OutEPSG := '269' + AddDayMonthLeadingZero(MDdef.DefaultUTMZone);
       outer := 'utm_x_zone';
   end
   else if (How = gdaltoUTMNAD83) then begin
      GetGDALinfo(fName,GDALinfo);
      GetEPSG(GDALinfo);
      OutEPSG := IntToStr(GDALinfo.utmEPSG);
      outer := 'utm';
      //if (Sender = GDALwarpGeotifftoUTMNAD831) then begin
         fName2 := GDAL_warp_name;
         TStr2 := 'warp';
         ch := 't';
         inProj := ' -s_srs EPSG:' + IntToStr(GDALinfo.inEPSG);
      //end;
   end
   else begin
      MessageToContinue('Disabled');
      FileNames.Free;
      exit;
   end;

   if IsGDALFilePresent(fName2) then begin
      StartGDALbatchFile(BatchFile);
      for i := 0 to pred(FileNames.Count) do begin
         fName := FileNames.Strings[i];
         OutName := ExtractFilePath(fName) + Tstr2 + '_' + outer + '_' + ExtractFileNameNoExt(fName) + '.tif';
         cmd := fName2 + ' -of Gtiff ' + inProj + ' -' + ch + '_srs EPSG:' + OutEPSG + ' ' +  fName + ' ' + OutName;
         {$IfDef RecordGDAL} WriteLineToDebugFile(cmd); {$EndIf}
         BatchFile.Add(cmd);
      end;
      fName := NextFileNumber(MDtempDir,'GDAL+' + TStr2 + '_','.bat');
      EndBatchFile(fName,batchfile);
   end;
   FileNames.Free;
end;



function Expand(sf : shortString) : shortstring;
begin
  if (sf <> '') then Result := ' -s ' + sf + ' '
  else Result := '';
end;



procedure UseGDAL_VRT_to_merge(var MergefName,OutVRT : PathStr; OutNames : tStringList; Added : ShortString = '');
//GDAL_VRT was about three times faster than other options tested
//OutVRT has the VRT table if you want to look at it
//added allows adding a projection of the files are lacking them, say for ASC input DEMs
var
   aName : PathStr;
   cmd : shortstring;
   BatchFile : tStringList;
begin
   try
      ShowHourglassCursor;
      HeavyDutyProcessing := true;
      aName := Petmar.NextFileNumber(MDTempDir, 'gdal_merge_file_list_','.txt');
      OutNames.SaveToFile(aName);
      OutVRT := Petmar.NextFileNumber(MDTempDir, 'gdal_vrt_','.vrt');

      StartGDALbatchFile(BatchFile);
      BatchFile.Add('REM create VRT');
      cmd := GDALtools_Dir + 'gdalbuildvrt ' + Added + ' -input_file_list ' + aName + ' ' + OutVRT;
      BatchFile.Add(cmd);
      cmd := GDALtools_Dir + 'gdal_translate -of GTiff ' + OutVrt + ' ' + MergefName + ' ' + Added;
      BatchFile.Add(cmd);

      aName := Petmar.NextFileNumber(MDTempDir, 'vrt2merge_','.bat');
      EndBatchFile(aName,BatchFile);
   finally
      ShowDefaultCursor;
      HeavyDutyProcessing := false;
   end;
end;


procedure ResampleSentinel_1(Path : PathStr; Recycle : boolean = false);
// based on https://asf.alaska.edu/how-to/data-recipes/geocode-sentinel-1-with-gdal/
var
   fName,outName : PathStr;
   BatchFile,TheFiles: tStringList;
   UTMspace : float32;
   i,j : Integer;
   TStr2,OutEPSG : shortString;
   cmd : ANSIString;
   RecycleList : tStringList;
begin
   PickUTMZone(MDdef.DefaultUTMZone);
   OutEPSG := 'EPSG:326' + AddDayMonthLeadingZero(MDdef.DefaultUTMZone);
   UTMSpace := 10;
   ReadDefault('UTM spacing (m)',UTMspace);
   TStr2 := ' ' + RealToString(UTMSpace,-12,-2);
   StartGDALbatchFile(BatchFile);
   RecycleList := tStringList.Create;
   TheFiles := Nil;
   FindMatchingFiles(Path,'*.tiff',TheFiles,6);
   for j := 0 to pred(TheFiles.Count) do begin
      fName := TheFiles.Strings[j];
      if StrUtils.AnsiContainsText(UpperCase(fName),'GRD') and (not StrUtils.AnsiContainsText(UpperCase(fName),'utm')) then begin
         OutName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName) + '_' + RealToString(UTMSpace,-8,-2) + '_m_utm.tif';
         cmd := GDAL_warp_name + ' -tps ' + BilinearString + ' -tr' + TStr2 + Tstr2 + ' -srcnodata 0 -dstnodata 0 -t_srs ' + OutEPSG + ' ' + fName + ' ' + Outname;
         {$IfDef RecordGDAL} WriteLineToDebugFile(cmd); {$EndIf}
         BatchFile.Add('REM   file ' + IntToStr(succ(i)) + '/' + IntToStr(TheFiles.Count));
         BatchFile.Add(cmd);
         RecycleList.Add(fName);
      end;
   end;
   TheFiles.Free;
   EndBatchFile(MDTempDir + 'warp_sentinel-1.bat',batchfile,true);
   if Recycle then for i := 0 to pred(RecycleList.Count) do File2Trash(RecycleList.Strings[i]);
   RecycleList.Free;
end;


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


function GetGDALFileNames : boolean;
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
                   SetGDALprogramName('gdalsrsinfo.exe',GDAL_srs_info_Name) and SetGDALprogramName('gdal_rasterize.exe',GDAL_rasterize);
         if (not Result) then begin
            MessageToContinue('GDAL files are missing; consider reinstalling OSGEO4W');
            {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('user recommended reinstalling'); {$EndIf}
         end;
         SetGDALdataStr := 'set GDAL_DATA=' + StringReplace(GDALtools_Dir, 'bin', 'share\gdal',[rfIgnoreCase]);
     end;

begin
   {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} HighlightLineToDebugFile('GetGDALFileNames in'); {$EndIf}
   Result := true;
   if ValidPath(GDALtools_Dir) and SetRest then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen) or Defined(RecordProblems)} WriteLineToDebugFile('GDAL valid, ' + GDALtools_Dir + '  ' + GetGDALversion); {$EndIf}
      exit;
   end;

   GDALtools_Dir := 'C:\OSGeo4W\bin\';
   if ValidPath(GDALtools_Dir) and SetRest then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('GDAL valid, ' + GDALtools_Dir + '  ' + GetGDALversion); {$EndIf}
      exit;
   end;

   if not AnswerIsYes('No default GDAL installation present.  If you downloaded, do you want to try to find GDAL') then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('User stopped GDAL manual search'); {$EndIf}
      goto NoMoreBugging;
   end;

   GetDOSPath('GDAL binary directory, something like ' +  GDALtools_Dir,GDALtools_Dir);
   {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('User pick GDAL tools dir: ' + GDALtools_Dir); {$EndIf}

   if ValidPath(GDALtools_Dir) and SetRest then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen)} WriteLineToDebugFile('User picked GDAL valid, ' + GDALtools_Dir + '  ' + GetGDALversion); {$EndIf}
      exit;
   end;


  NoMoreBugging:;
  Result := false;
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
      InfoName := GDALinfoOutputFName(fName);
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


function OpenDEMfromGDAL(OpenMap : boolean; InputDEM : integer; OutName : PathStr; mt : byte = mtElevSpectrum; ElevUnits : byte = euUndefined) : integer;
begin
    if FileExists(OutName) then begin
       Result := OpenNewDEM(OutName,false);
       if ValidDEM(Result) then begin
          DEMGlb[Result].DEMheader.ElevUnits := ElevUnits;
          if DEMGlb[Result].ElevationDEM then DEMGlb[Result].DEMheader.VerticalCSTypeGeoKey := DEMGlb[InputDEM].DEMheader.VerticalCSTypeGeoKey;
          DEMGlb[Result].WriteNewFormatDEM(DEMGlb[Result].DEMFileName);
          if not PossibleElevationUnits(DEMGlb[Result].DEMheader.ElevUnits) then DEMGlb[Result].DEMHeader.VerticalCSTypeGeoKey := VertCSUndefined;
          if OpenMap then CreateDEMSelectionMap(Result,true,true,mt);
       end;
    end
    else begin
       Result := 0;
       //MessageToContinue('GDAL failure, ' + RunDOSwindow + cmd);
    end;
end;

function GDAL_DEM_command(OpenMap : boolean; InputDEM : integer; cmd : ANSIstring; OutName : PathStr; mt : byte = mtElevSpectrum; ElevUnits : byte = euUndefined) : integer;
begin
    if (GDAL_testScaleFactor <> '') then begin
       cmd := GDAL_dem_name + cmd + GDAL_testScaleFactor;
       GDAL_testScaleFactor := '';
    end
    else cmd := GDAL_dem_name + cmd + DEMGlb[InputDEM].GDAL_ScaleFactorString;
    if WinExecAndWait32(cmd) = -1 then begin
      {$IfDef RecordProblems} HighlightLineToDebugFile('Failure GDALCommand, cmd = ' + cmd); {$EndIf}
       Result := 0;
       MessageToContinue('GDAL failure, ' + RunDOSwindow + cmd);
    end
    else Result := OpenDEMfromGDAL(OpenMap,InputDEM,OutName,mt,ElevUnits);
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


procedure CallGDALMerge(var MergefName : PathStr; InNames : tStringList; MissingData : integer = 255);
var
   BatchFile : tStringList;
   cmd : ANSIString;
   DefFilter : byte;
   fName : PathStr;
begin
   if (InNames = Nil) then begin
      InNames := tStringList.Create;
      InNames.Add(MainMapData);
      DefFilter := 1;
      if not GetMultipleFiles('Files for GDAL merge','TIFF files|*.TIF',InNames,DefFilter) then exit;
   end;
   if (MergefName = '') then begin
      MergefName := ExtractFilePath(InNames[0]);
      Petmar.GetFileNameDefaultExt('Merged filename','*.tif',MergefName);
   end;

   fName := Petmar.NextFileNumber(MDTempDir, 'gdal_merge_file_list_','.txt');
   InNames.SaveToFile(fName);

   StartGDALbatchFile(BatchFile);
   BatchFile.Add('REM Merge tiffs');
   cmd := PythonEXEname + ' ' + PythonScriptDir + 'gdal_merge.py -a_nodata ' + IntToStr(MissingData) + ' -o ' + MergefName +  ' --optfile ' + fName;
   (*
   //removed 3/6/2024   This has not been used in a long time, and appears it would not work
   for i := 0 to pred(InNames.Count) do begin
      cmd := cmd + ' ' + OutNames[i];
   end;
   *)
   BatchFile.Add(cmd);
   fName := Petmar.NextFileNumber(MDTempDir, 'gdalmerge_','.bat');
   EndBatchFile(fName,BatchFile);
end;



procedure GDAL_replace_42112_tag(DEMName : PathStr; ReplaceStr : shortstring = '');
// not working, 3/17/2024
var
   bfile : PathStr;
   cmd : shortString;
   BatchFile : tStringList;
begin
   StartGDALbatchFile(BatchFile);
   cmd := PythonEXEname + ' ' + PythonScriptDir + 'gdal_edit.py -unsetmd 42112 -mo 42112="' + ReplaceStr + '" ' + DEMName;
   BatchFile.Add(cmd);
   bfile := Petmar.NextFileNumber(MDTempDir, 'gdal_remove_42112_','.bat');
   EndBatchFile(bfile ,batchfile);
   if FileExists(DEMName) then begin
   end
   else MessageToContinue('GDAL_remove_42112_tag failure,' + RunDOSwindow + bfile);
end;



procedure GDALAssignProjectionViaWKTorEPSG(DEMName,ProjWKTFileNameOrEPSG : PathStr);
var
   bfile : PathStr;
   cmd : shortString;
   BatchFile : tStringList;
begin
   StartGDALbatchFile(BatchFile);
   cmd := PythonEXEname + ' ' + PythonScriptDir + 'gdal_edit.py -a_srs ' + ProjWKTFileNameOrEPSG + ' ' + DEMName;
   BatchFile.Add(cmd);
   bfile := Petmar.NextFileNumber(MDTempDir, 'gdal_assign_proj_','.bat');
   EndBatchFile(bfile ,batchfile);
   if FileExists(DEMName) then begin
   end
   else MessageToContinue('GDALAssignProjection failure,' + RunDOSwindow + bfile);
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
      bfile := Petmar.NextFileNumber(MDTempDir, 'gdal_fill_holes_','.bat');
      EndBatchFile(bfile,batchfile);
      if FileExists(OutName) then begin
         OpenNewDEM(OutName);
      end
      else MessageToContinue('GDAL_Fill_Holes failure,' + RunDOSwindow + bfile);
   end;
end;


procedure GDAL_Raster_Calculator(Expression : shortstring);
var
   bfile : PathStr;
   cmd : shortString;
   BatchFile : tStringList;
begin
//gdal_calc.py -A input1.tif -B input2.tif -C input3.tif --outfile=result.tif --calc="A+B+C"
   cmd := PythonEXEname + ' ' + PythonScriptDir + 'gdal_calc.py ' + Expression;
   StartGDALbatchFile(BatchFile);
   BatchFile.Add(cmd);
   bfile := Petmar.NextFileNumber(MDTempDir, 'gdal_raster_calc_','.bat');
   EndBatchFile(bfile,batchfile);
end;


function GDAL_SlopeMap_ZT(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
//https://gdal.org/programs/gdaldem.html
var
   cmd : ANSIstring;
begin
   if (Outname = '') then OutName := MDTempDir + 'gdal_slope_zt_' + DEMGlb[DEM].AreaName  + '.tif';
   cmd := ' slope ' + DEMGlb[DEM].GeotiffDEMName + ' ' + OutName +  ' -p -alg ZevenbergenThorne';
   Result := GDAL_DEM_command(OpenMap,DEM,cmd, OutName,MDDef.DefSlopeMap,euPercentSlope);
end;

function GDAL_SlopeMap_Horn(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
//https://gdal.org/programs/gdaldem.html
var
   cmd : ANSIstring;
begin
   if (Outname = '') then OutName := MDTempDir + 'gdal_slope_horn_' + DEMGlb[DEM].AreaName  + '.tif';
   cmd := ' slope ' + DEMGlb[DEM].GeotiffDEMName + ' ' + OutName +  ' -p ';
   Result := GDAL_DEM_command(OpenMap,DEM,cmd , OutName,MDDef.DefSlopeMap,euPercentSlope);
end;


function GDAL_TRI_Wilson(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
var
   cmd : ANSIstring;
begin
   if (Outname = '') then OutName := MDTempDir + 'gdal_TRI_Wilson_' + DEMGlb[DEM].AreaName + '.tif';
   cmd := ' TRI ' + DEMGlb[DEM].GeotiffDEMName + ' ' + OutName + ' -alg Wilson';
   Result := GDAL_DEM_command(OpenMap,DEM,cmd,OutName,mtElevSpectrum);
end;


function GDAL_TRI_Riley(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
var
   cmd : ANSIstring;
begin
   if (Outname = '') then OutName := MDTempDir + 'gdal_TRI_Riley_' + DEMGlb[DEM].AreaName  + '.tif';
   cmd := ' TRI ' + DEMGlb[DEM].GeotiffDEMName + ' ' + OutName + ' -alg Riley';
   Result := GDAL_DEM_command(OpenMap,DEM,cmd,OutName,mtElevSpectrum);
end;

function GDAL_TPI(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
var
   cmd : ANSIstring;
begin
   if (Outname = '') then OutName := MDTempDir + 'gdal_TPI_' + DEMGlb[DEM].AreaName  + '.tif';
   cmd := ' TPI ' + DEMGlb[DEM].GeotiffDEMName + ' ' + OutName;
   Result := GDAL_DEM_command(OpenMap,DEM,cmd,OutName,mtElevSpectrum);
end;


function GDAL_Roughness(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
var
   cmd : ANSIstring;
begin
   if (Outname = '') then OutName := MDTempDir + 'gdal_roughness_' + DEMGlb[DEM].AreaName  + '.tif';
   cmd := ' roughness ' + DEMGlb[DEM].GeotiffDEMName + ' ' + OutName;
   Result := GDAL_DEM_command(OpenMap,0,cmd, OutName,mtElevSpectrum,eumeters);
end;


function GDAL_AspectMap_Horn(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
//https://gdal.org/programs/gdaldem.html
var
   cmd : ANSIstring;
begin
   if (Outname = '') then OutName := MDTempDir + 'gdal_aspect_horn_' + DEMGlb[DEM].AreaName  + '.tif';
   cmd := ' aspect ' + DEMGlb[DEM].GeotiffDEMName + ' ' + OutName;
   Result := GDAL_DEM_command(OpenMap,DEM,cmd, OutName,mtDEMAspect,euAspectDeg);
end;


function GDAL_AspectMap_ZT(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
//https://gdal.org/programs/gdaldem.html
var
   cmd : ANSIstring;
begin
   if (Outname = '') then OutName := MDTempDir + 'gdal_aspect_zt_' + DEMGlb[DEM].AreaName  + '.tif';
   cmd := ' aspect ' + DEMGlb[DEM].GeotiffDEMName + ' ' + OutName +  ' -alg ZevenbergenThorne';
   Result := GDAL_DEM_command(OpenMap,DEM,cmd,OutName,mtDEMAspect,euAspectDeg);
end;


function GDAL_HillshadeMap_Horn(OpenMap : boolean; DEM : integer; outname : shortstring = '') : integer;
//https://gdal.org/programs/gdaldem.html
var
   cmd : ANSIstring;
begin
   if (Outname = '') then OutName := MDTempDir + 'gdal_hillshade_' + DEMGlb[DEM].AreaName  + '.tif';
   cmd := ' hillshade ' + DEMGlb[DEM].GeotiffDEMName + ' ' + OutName +  ' -alg Horn';
   Result := GDAL_DEM_command(OpenMap,DEM,cmd,OutName,mtElevGray,euUndefined);
end;


function CreateWKTfileForGeotiff(fName : PathStr) : PathStr;
var
   cmd : ANSIString;
   BatchFile: tStringList;
begin
   {$If Defined(RecordGDAL) or Defined(RecordWKT)} WriteLineToDebugFile('CreateWKTfileForGeotiff in, ' + fName); {$EndIf}
   StartGDALbatchFile(BatchFile);
   Result := MDTempDir + ExtractFileName(fName) + '.wkt';
   cmd := GDAL_srs_info_name  + ' -o wkt ' + fName + ' >' + Result;
   BatchFile.Add(Cmd);
   EndBatchFile(MDTempDir + 'gdal_srs_info.bat',BatchFile);
   {$If Defined(RecordGDAL) or Defined(RecordWKT)} WriteLineToDebugFile('CreateWKTfileForGeotiff out'); {$EndIf}
end;

function CreateWKTStringForGeotiff(fName : PathStr) : ANSIstring;
var
   sl : tStringList;
   wktName : PathStr;
   i : integer;
begin
   {$If Defined(RecordGDAL) or Defined(RecordWKT)} WriteLineToDebugFile('CreateWKTStringForGeotiff in, ' + fName); {$EndIf}
   wktName := CreateWKTfileForGeotiff(fName);
   sl := tStringList.Create;
   sl.LoadFromFile(wktName);
   Result := '';
   for i := 0 to pred(sl.Count) do Result := Result + sl.strings[i];
   StripBlanks(Result);
   sl.Destroy;
   {$If Defined(RecordGDAL) or Defined(RecordWKT)} WriteLineToDebugFile('CreateWKTStringForGeotiff out'); {$EndIf}
end;

procedure GDALGeotiffToWKT(fName : PathStr);
var
   OutName : PathStr;
begin
   OutName := CreateWKTfileForGeotiff(fName);
   ShowInNotepadPlusPlus(OutName,ExtractFileName(OutName) + 'SRS WKT');
end;


function GDAL_downsample_DEM_1sec(OpenMap : boolean; DEM : integer; OutName : PathStr) : integer;
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
   TargetExtent := ' -te ' + RealToString(LatSW,-12,-8) + ' ' + RealToString(LongSW,-12,-8)  +  ' ' + RealToString(LatNE,-12,-8) + ' ' + RealToString(LongNE,-12,-8);
   InName := DEMGlb[DEM].GeotiffDEMName;
   cmd := GDAL_Warp_Name  + SpaceStr + IntString + TargetExtent + TargetEPSG + ' ' + InName + ' ' + OutName;
   Result := GDAL_DEM_command(OpenMap,DEM,cmd,OutName,DEMGlb[DEM].SelectionMap.MapDraw.MapType);
   //gdalwarp -t_srs EPSG:4326 -tr 0.3125 0.25 -r near -te 71.40625 24.875 84.21875 34.375 -te_srs EPSG:4326 -of GTiff foo.tiff bar.tiff
end;


function GDAL_warp_DEM(OpenMap : boolean; DEM : integer; OutName : PathStr; xspace,yspace : float32; IntString : shortstring = biLinearString) : integer;
var
   cmd : AnsiString;
   SpaceStr : shortString;
   InName : PathStr;
   MapType : tMapType;
begin
   InName := DEMGlb[DEM].GeotiffDEMName;
   SpaceStr := ' -tr ' + RealToString(xSpace,-12,-8) + ' ' + RealToString(ySpace,-12,-8);
   if DEMglb[DEM].SelectionMap <> Nil then MapType := DEMglb[DEM].SelectionMap.MapDraw.MapType
   else MapType := mtElevSpectrum;
   cmd := GDAL_Warp_Name  + SpaceStr + IntString + InName + ' ' + OutName;
    if WinExecAndWait32(cmd) = -1 then begin
      {$IfDef RecordProblems} HighlightLineToDebugFile('Failure GDALCommand, cmd = ' + cmd); {$EndIf}
       Result := 0;
       MessageToContinue('GDAL failure, ' + RunDOSwindow + cmd);
    end
    else Result := OpenDEMfromGDAL(OpenMap,DEM,OutName,MapType,DEMglb[DEM].DEMHeader.ElevUnits);
end;


function GDAL_upsample_DEM(OpenMap : boolean; DEM : integer; Bilinear : boolean; Spacing : float32 = -99; OutName : PathStr = '') : integer;
var
   SpaceStr : shortString;
begin
   {$If Defined(RecordSave) or Defined(RecordGDAL)} WriteLineToDebugFile('GDAL_upsample_DEM in'); {$EndIf}
   if IsGDALFilePresent(GDAL_Warp_Name) then begin
      if (Spacing < 0) then begin
         ReadDefault('upsample factor (multiple of grid spacing)',MDDef.GDALThinFactor);
         Spacing := MDDef.GDALThinFactor * DEMGlb[DEM].DEMheader.DEMxSpacing;
      end;
      //if (DEMGlb[DEM].DEMheader.DEMUsed = ArcSecDEM) then SpaceStr := RealToString(3600 * Spacing,-8,-2) + '_sec' else SpaceStr := RealToString(Spacing,-8,-2) + '_m';

      if Bilinear then begin
         if (OutName = '') then OutName := MDTempDir + DEMGlb[DEM].AreaName + '_bilinear_upsample' {+ SpaceStr} + '.tif';
         Result := GDAL_warp_DEM(OpenMap,DEM,OutName,Spacing,Spacing,BiLinearString);
      end
      else begin
         if (OutName = '') then OutName := MDTempDir + DEMGlb[DEM].AreaName + '_bicubic_upsample' {+ SpaceStr} + '.tif';
         Result := GDAL_warp_DEM(OpenMap,DEM,OutName,Spacing,Spacing,BiCubicString);
      end;
   end;
end;

(*
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
      if (DEMGlb[DEM].DEMheader.LatHemi = 'S') then Code := Code + 100;
      Code := Code + DEMGlb[DEM].DEMheader.UTMZone;

      SpaceStr := ' -t_srs EPSG:' + IntToStr(Code) + ' ';  //-tr ' + RealToString(UTMSpacing,-12,-8) + ' ' + RealToString(UTMSpacing,-12,-8);

      OutName := MDTempDir + DEMGlb[DEM].AreaName + '_resamp_UTM_cubic.tif';
      GDAL_warp_DEM(true,DEM,OutName,UTMSpacing,UTMSpacing,' -r cubic ',SpaceStr);
      OutName := MDTempDir + DEMGlb[DEM].AreaName + '_resamp_UTM_linear.tif';
      GDAL_warp_DEM(true,DEM,OutName,UTMSpacing,UTMSpacing,' -r bilinear ',SpaceStr);
   end;
end;
*)

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
   Result := ' -projwin ' + GDALextentBoxGeo(BoundBox) + ' ';
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
               {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL_warp GetGDALinfo out, cmd=' + cmd); {$EndIf}
            end;
         end;
      end;

      function GDAL_warp_Multiple(var theFiles : tStringList) : PathStr;
      var
         BatchFile : tStringList;
         cmd,InProj,outProj : shortString;
         GDALinfo : tGDALinfo;
         fName : PathStr;
         i : integer;
      begin
         if IsGDALFilePresent(GDAL_warp_name) then begin
            {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL_warp_Multiple, files=' + IntToStr(theFiles.Count)); {$EndIf}
            inProj := '';
            StartGDALbatchFile(BatchFile);
            for i := 0 to pred(theFiles.Count) do begin
               fName := TheFiles.Strings[i];
               Result := ChangeFileExt(fName,'.tif');
               if not FileExists(Result) then begin
                  if inProj = '' then begin
                     GetGDALinfo(fName,GDALinfo);
                     {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL_warp GetGDALinfo done'); {$EndIf}
                     inProj := ' -s_srs EPSG:' + IntToStr(GDALinfo.inEPSG);
                     OutProj := ' -t_srs EPSG:' + IntToStr(GDALinfo.utmEPSG);
                  end;
                  cmd := GDAL_warp_name + ' -of Gtiff ' + inProj + OutProj + ' ' + fName + ' ' + Result;
                  BatchFile.Add('REM ' + IntToStr(succ(i)) + '/' + IntToStr(theFiles.Count));
                  BatchFile.Add(cmd);
               end;
            end;
            fName := NextFileNumber(MDTempDir,'gdw_mult_','.bat');
            EndBatchFile(fName,batchfile);
            {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL_warp GetGDALinfo out, cmd=' + fName); {$EndIf}
         end;
         FreeAndNil(theFiles);
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

      procedure ParseForUTM(TStr, Corner: ANSIstring; var xutm,yutm : float64);
      begin
          if StrUtils.AnsiContainsText(TStr,Corner) and StrUtils.AnsiContainsText(TStr,'(') then begin
            //Center      (  723282.207, 4270968.445) ( 78d26'14.85"W, 38d33'32.88"N)
             TStr := AfterSpecifiedString(TStr,'(');
             xutm := StrToFloat(BeforeSpecifiedCharacterANSI(TStr,',',true,true));
             yutm := StrToFloat(BeforeSpecifiedCharacterANSI(TStr,')',true,true));
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
                if (GDAL_program = GDAL_warp_Name) then bfile.Add(GDAL_program + ' -r near -te ' +  GDALextentBoxUTM(xmin,ymin,xmax,ymax)  + ' ' + BandName + ' ' +  OutPath + ExtractFileName(BandName))
                else bfile.Add(GDAL_program + ' -r near -projwin ' +  GDALextentBoxUTM(xmin,ymax,xmax,ymin) + ' ' + BandName + ' ' +  OutPath + ExtractFileName(BandName));
             end;

      begin
         if IsGDALFilePresent(GDAL_program) then begin
            {$If Defined(RecordGDAL) or Defined(RecordSubsetGDAL)} WriteLineToDebugFile('GDALSubsetSatImageToMatchMap in ' + GDAL_program + '  ' + ExtractFilePath(SatImage[MapOwner.MapDraw.SATonMap].IndexFileName)); {$EndIf}
            OutPath := LastSatDir;
            OutPath := Copy(OutPath, 1,pred(length(OutPath)));
            OutPath := NextFilePath(OutPath + '_subset');
            SafeMakeDir(OutPath);

            MapOwner.MapDraw.ScreenToUTM(0,0, xmin,ymax);
            MapOwner.MapDraw.ScreenToUTM(MapOwner.MapDraw.MapXSize,MapOwner.MapDraw.MapYSize, xmax,ymin);
            {$If Defined(RecordGDAL) or Defined(RecordSubsetGDAL)} WriteLineToDebugFile('Map limits from screen: ' + GDALextentBoxUTM(xmin,ymax,xmax,ymin)); {$EndIf}
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
         end;
      end;


      function GDALsubsetGridAndOpen(bb : sfBoundBox; LatLongBox : boolean; fName : PathStr; OpenMap : boolean; BaseOutPath : PathStr = '') : integer;


        function DoOneGrid(fName : PathStr) : integer;
        var
           OutPath,OutName : PathStr;
           LandCover : integer;
           TStr,ExtentBoxString,Cmd   : shortstring;
           Imagebb : sfBoundBox;
           Ext : ExtStr;
           RegVars : tRegVars;
         begin
            CheckFileNameForSpaces(fName);
            if (BaseOutPath = '') or (not ValidPath(BaseOutPath)) then BaseOutPath := MDtempdir;
            OutPath := NextFilePath(BaseOutPath  {ExtractFileNameNoExt(fName)} + 'extract_subset');
            SafeMakeDir(OutPath);
            OutName := OutPath + ExtractFileName(fName);


            {$IfDef RecordSubsetOpen} WriteLineToDebugFile('GDALsubsetGridAndOpen ' + ExtractFileName(fname) + ' want out ' + sfBoundBoxToString(BB,4)); {$EndIf}

            Ext := UpperCase(ExtractFileExt(fName));
            GeotiffBoundingBoxProj(fName,Imagebb);
            if GeotiffRegVars(fName, RegVars) then begin
               if (RegVars.pName = PlateCaree) then begin
                  ExtentBoxString := ' -srcwin ' + IntToStr(trunc((bb.xMin - RegVars.UpLeftX) / RegVars.pr_deltax)) + ' ' +
                                                   IntToStr(trunc((RegVars.UpLeftY - bb.YMax) / RegVars.pr_deltaY)) + ' ' +
                                                   IntToStr(succ( round((bb.xMax - bb.xMin) / RegVars.pr_deltax))) + ' ' +
                                                   IntToStr(succ( round((bb.yMax - bb.yMin) / RegVars.pr_deltaY)));
                  TStr := ' -r nearest ';
               end
               else begin
                  if LatLongBox then begin
                     ExtentBoxString := GDALextentBoxLatLong(bb);
                  end
                  else begin
                     ExtentBoxString := GDALextentBoxUTM(bb);
                  end;
                  if IsThisLandCover(fName,LandCover) then TStr := ' -r nearest '
                  else TStr := ' ';
               end;
            end;

            if AtLeastPartOfBoxInAnotherBox(ImageBB, bb) then begin
               cmd := GDAL_Translate_Name + ' ' + ExtentBoxString + TStr + fName + ' ' + OutName;
               {$IfDef RecordSubsetOpen} WriteLineToDebugFile('cmd=' + cmd); {$EndIf}
               GDALcommand(MDTempDir + 'raster_subset.bat',cmd);
               if FileExists(OutName) then begin
                  Result := OpenNewDEM(OutName,OpenMap);
                  {$IfDef RecordSubsetOpen} WriteLineToDebugFile('Grid opened ' + IntToStr(Result)); {$EndIf}
               end
               else begin
                  GeotiffMetadata(mdMicrodem, fName);
                  if (not HeavyDutyProcessing) and ReportErrors then MessageToContinue('Failure; does image really cover this map area?  Check Metadata for image extent');
               end;
            end
            else begin
               if (not HeavyDutyProcessing) then begin
                  if (not HeavyDutyProcessing) and ReportErrors  then MessageToContinue('Map area is not covered by ' + ExtractFileNameNoExt(fName) + '  ' + sfBoundBoxToString(ImageBB,4));
                  {$IfDef RecordSubsetOpen} WriteLineToDebugFile('Not covered on map'); {$EndIf}
               end;
            end;
         end;


      var
         theFiles : tStringList;
         DefaultFilter : byte;
         i : integer;
      begin
         if IsGDALFilePresent(GDAL_Translate_Name) then begin
            if (fName <> '') and FileExists(fname) then begin
               Result := DoOneGrid(fName);
            end
            else begin
               fName := ExtractFilePath(LastImageName);
               theFiles := tStringList.Create;
               theFiles.Add(fName);
               DefaultFilter := 1;
               if GetMultipleFiles('image to subset and import','image|*.tif;*.ecw',theFiles,DefaultFilter) then begin
                  for i := 0 to pred(TheFiles.Count) do DoOneGrid(theFiles.Strings[i]);
               end;
               LastImageName := fName;
            end;
         end;
      end;


      procedure GetFilesNamesForGDALtranslate(var InName,OutName : Pathstr; TempStorage : boolean = false; TrashOriginal : boolean = false);
      var
         tName : PathStr;
      begin
         {$IfDef RecordReformat} WriteLineToDebugFile('Enter GetFilesNamesForGDALtranslate, in=' + InName); {$EndIf}
         if (UpperCase(ExtractFileExt(InName)) = '.TIF') or (UpperCase(ExtractFileExt(InName)) = '.TIFF') then begin
             if (not TrashOriginal) and TempStorage then begin
                if ExtractFilePath(InName) = MDtempDir then
                     OutName := MDtempdir + 'rewritten_' + ExtractFileName(InName)
                else OutName := MDtempdir + ExtractFileName(InName);
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
            if (OutName = '') then GetFilesNamesForGDALtranslate(fName,OutName,TrashOriginal);
            Result := OutName;
            StartGDALbatchFile(BatchFile);
            BatchFile.Add(GDAL_translate_name + ' -of Gtiff '  + ExtraOptions + ' ' + fName + ' ' + OutName);
            BatchName := Petmar.NextFileNumber(MDTempDir, 'gdal_translate_','.bat');
            EndBatchFile(BatchName,BatchFile);
            if TrashOriginal and (Uppercase(ExtractFileExt(fName)) <> '.GTX') then begin
               if ExtractFilePath(fName) = MDTempDir then DeleteFileIfExists(fName) else File2Trash(fName);
            end;
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


      function GDALConvertSingleImageToGeotiff(var fName : PathStr) : boolean;
      //if you want to keep compressed DEM and uncompress to temporary storage, use this
      var
         OutName : PathStr;
         cmd : shortstring;
         LandCover : integer;
      begin
         {$IfDef RecordReformat} WriteLineToDebugFile('GDALConvertSingeImageToGeotiff in ' + fName); {$EndIf}
         if IsGDALFilePresent(GDAL_translate_name) then begin
            GetFilesNamesForGDALtranslate(fName,OutName,true);
            if not IsThisLandCover(ExtractFileName(fName),LandCover) then begin
               if IsThisLandCover(fName,LandCover) then begin
                  Outname := ExtractFilePath(OutName) + 'LandCover_' + IntToStr(LandCover) + '_' + ExtractFileName(fName);
               end;
            end;
            cmd := GDAL_translate_name + GDAL_Geotiff_str + fName + ' ' + OutName;
            {$IfDef RecordReformat} WriteLineToDebugFile('GDALConvertSingeImageToGeotiff, cmd=   ' + cmd); {$EndIf}
            WinExecAndWait32(cmd);
            Result := FileExists(outName);
            if Result then fName := OutName
            else fName := '';;
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
               if ExtractFilePath(RecycleNames.Strings[i]) = MDtempDir then DeleteFileIfExists(RecycleNames.Strings[i])
               else File2Trash(RecycleNames.Strings[i]);
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
         cmd : shortstring;
      begin
//convert a shapefile to geopackage
//$ ogr2ogr -f GPKG filename.gpkg abc.shp
//all the files (shapefile/geopackage) will be added to one geopackage.
//$ ogr2ogr -f GPKG filename.gpkg ./path/to/dir
          cmd := GDAL_ogr_Name + ' -f GPKG ' + ChangeFileExt(fName,'.gpkg') + ' ' + fName  + ' -progress -t_srs EPSG:4326';
          GDALCommand(MDTempDir + 'shp2gkpg.bat',cmd);
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
          until not ValidPath(NewDir);
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
         if (not Result) then begin
            if AnswerIsYes('Missing GDAL file required ' + fName  + '  Locate it how') then begin
               Result := GetGDALFileNames;
            end;
         end;
      end;


      function GDAL_translateUTM(InName,OutName : PathStr; WGS84,NHemi : boolean;  UTMzone : int16) : shortstring;
      var
         Code : integer;
      begin
         Code := 26900; {NAD83}
         if WGS84 then Code := 32600; {WGS84 default}
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


procedure ShiftToUTM_WGS84_EGM2008(inName,SaveName : PathStr; s_SRSstring : shortString; UTMzone : integer);
var
   t_srsstring : shortstring;
begin
   if (s_SRSstring <> '') then s_SRSstring := ' -s_srs EPSG:' + s_SRSstring;
   t_srsstring := ' -t_srs EPSG:' + '326' + AddDayMonthLeadingZero(UTMzone) + '+3855';   //WGS84 + EGM2008
   CompositeDatumShiftWithGDAL(InName,SaveName,s_SRSstring,t_srsstring);
end;


procedure MultipleGDALcompositedatumshift;
var
   FilesWanted : tstringList;
   DefaultFilter : byte;
   InName,SaveName : PathStr;
   s_SRSstring,t_srsstring : shortstring;
   i : integer;
begin
   FilesWanted := tstringList.Create;
   DefaultFilter := 1;
   if GetMultipleFiles('Tiff file','Tiff files|*.tif;*.tiff',FilesWanted,DefaultFilter) then begin
      s_SRSstring := '';
      t_srsstring := ' -t_srs EPSG:32759+3855';
      GetString('s_SRSstring (blank if correct in Geotiff)',s_SRSstring,false,ReasonableTextChars);
      GetString('t_SRSstring',t_SRSstring,false,ReasonableTextChars);
      for I := 0 to pred(FilesWanted.Count) do begin
         InName := FilesWanted.Strings[i];
         SaveName := ExtractFilePath(InName) + 'datums_shifted_' + ExtractFileName(InName);
         CompositeDatumShiftWithGDAL(InName,SaveName,s_SRSstring,t_srsstring);
      end;
   end;
end;


procedure VerticalDatumShiftWithGDALtoEGM2008(DEM : integer; var SaveName : PathStr);
var
   InName : PathStr;
   UTMzone : shortstring;
   s_SRSstring,t_srsstring : shortstring;
begin
   {$If Defined(RecordDatumShift)} WriteLineToDebugFile('VerticalDatumShiftWithGDAL in, ' + DEMGlb[DEM].AreaName); {$EndIf}
   if (DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey = VertCSEGM2008) then begin
      MessageToContinue('DEM already EGM2008');
      exit;
   end;
   if (SaveName = '') then begin
      SaveName := DEMGlb[DEM].AreaName + '_wgs84_egm2008';
      if not GetFileNameDefaultExt('Vertical shifted DEM','*.tif',SaveName) then begin
         exit;
      end;
   end;
   if (DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey = VertCSEGM96) and (DEMGlb[DEM].DEMHeader.DEMUsed = ArcSecDEM) then begin
      //this goes from WGS84 EGM96 to WGS84 EGM2008
      s_SRSstring := ' -s_srs EPSG:4326+' + IntToStr(VertCSEGM96);
      t_srsstring := ' -t_srs EPSG:4326+' + IntToStr(VertCSEGM2008);
   end
   else begin
       if (DEMGlb[DEM].DEMMapProj.h_DatumCode = 'NAD83') or (DEMGlb[DEM].DEMMapProj.h_DatumCode = 'WGS84') or (DEMGlb[DEM].DEMMapProj.h_DatumCode = 'ETRS89') and (DEMGlb[DEM].DEMHeader.DEMUsed = UTMBasedDEM) then begin
          //this goes from NAD83 NAVD88 to WGS84 EGM2008
          UTMZone := AddDayMonthLeadingZero(DEMGlb[DEM].DEMHeader.UTMzone);
          s_SRSstring := ' -s_srs EPSG:269' + UTMzone + '+' + IntToStr(VertCSNAVD88);
          t_srsstring := ' -t_srs EPSG:326' + UTMzone + '+' + IntToStr(VertCSEGM2008);
       end
       else begin
          MessageToContinue('Not yet supported for ' + DEMGlb[DEM].DEMMapProj.h_DatumCode + '+' + IntToStr(DEMGlb[DEM].DEMheader.VerticalCSTypeGeoKey));
          exit;
       end;
   end;
   InName := DEMGlb[DEM].GeotiffDEMName;
   CompositeDatumShiftWithGDAL(InName,SaveName,s_SRSstring,t_srsstring);
   {$If Defined(RecordDatumShift)} WriteLineToDebugFile('VerticalDatumShiftWithGDAL out'); {$EndIf}
end;


 procedure ShiftAFile_UTM_WGS84_EGM2008(fName,SaveName : PathStr; VertCode,HorizCode,UTMzone : shortstring);
 var
    s_SRSstring,t_srsstring,OutVert : shortstring;
 begin
    //SaveName := ChangeFileExt(fName,'_egm2008.tif');
    if FileExists(SaveName) then begin
       {$If Defined(Record3DEPXfull)} WriteLineToDebugFile('Already shifted=' + fName); {$EndIf}
    end
    else begin
       {$If Defined(Record3DEPX)} WriteLineToDebugFile('ShiftAFile=' + fName + '  horiz code=' + HorizCode); {$EndIf}
          if HorizCode = 'NAD83' then begin
             HorizCode := '269' + UTMZone;
          end
          else if HorizCode = 'WGS84' then begin
             HorizCode := '269' + UTMZone;
          end
          else if HorizCode = 'ETRS89' then begin
             HorizCode := '258' + UTMZone;
          end
          else if (HorizCode = 'GCS_WGS') then begin
             HorizCode := '4326'
          end;
       if (VertCode = '') then begin
          OutVert := '';
       end
       else begin
          OutVert := '+3855';
          Vertcode := '+' + VertCode
       end;
       if (Length(UTMzone) = 3) and (UTMZone[3] = 'S') then UTMZone :=  '327' + Copy(UTMzone,1,2)
       else UTMZone := '326' + UTMzone;
       s_SRSstring := ' -s_srs EPSG:' + HorizCode + VertCode;
       t_srsstring := ' -t_srs EPSG:' + UTMZone + OutVert;  //WGS84 + EGM2008
       CompositeDatumShiftWithGDAL(fName,SaveName,s_SRSstring,t_srsstring);
    end;
 end;


procedure CompositeDatumShiftWithGDAL(var InName,SaveName : PathStr; s_SRSstring,t_srsstring : shortstring);
//  s_SRSstring can be empty is the source Geotiff has the corrrect projection assigned
//  s_SRSstring will overwrite existing source Geotiff, or assign it it undefined
const
   NoUnitShift = '-wo MULT_FACTOR_VERTICAL_SHIFT=1 ';
var
   BatchFile : tstringList;
   cmd : shortstring;
   aName : PathStr;
begin
   if UpperCase(ExtractFileExt(inName)) = '.TIF' then begin
      {$If Defined(RecordProjectionStrings)} WriteLineToDebugFile('CompositeDatumShiftWithGDAL ' + s_SRSstring + ' ' + t_srsstring); {$EndIf}
      StartGDALbatchFile(BatchFile);
      cmd := GDAL_warp_name + ' --config GDAL_CACHEMAX 1000 -wm 1000 --debug on -overwrite -multi -wo NUM_THREADS=8 -ot float32 ' +
          NoUnitShift + DoubleQuotedString(InName) + ' ' + DoubleQuotedString(SaveName) + s_SRSString + t_srsstring;
      {$If Defined(RecordDEMIXCompositeDatum)} WriteLineToDebugFile('VerticalDatumShiftWithGDAL cmd=' + cmd); {$EndIf}
      BatchFile.Add(cmd);
      aName := Petmar.NextFileNumber(MDTempDir, 'gdal_datumshift_','.bat');
      {$If Defined(RecordDEMIXCompositeDatum)} WriteLineToDebugFile('VerticalDatumShiftWithGDAL batch file=' + aName); {$EndIf}
      EndBatchFile(aName,BatchFile);
   end
   else begin
       MessageToContinue('Not a TIF file: ' + InName);
   end;
end;


      function GDALinfoOutputFName(fname : PathStr) : PathStr;
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
         {$If Defined(RecordGDALinfo)} WriteLineToDebugFile('GetGDALinfo in ' + fName); {$EndIf}
         ZeroGDALInfo(GDALinfo);
         OutName := GDALinfoOutputFName(fname);

         if (not FileExists(outName)) then begin
            cmd := GDAL_info_name  + ' ' + fName + AddWKT + ' >' + OutName;
            GDALCommand(MDTempDir + 'gdal_info.bat',cmd);
            {$If Defined(RecordGDALinfo)} WriteLineToDebugFile('GetGDALinfo in, cmd=   ' + cmd); {$EndIf}
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
             ParseForUTM(TStr, 'UPPERLEFT',GDALInfo.xutm_low,GDALInfo.yutm_high);
             ParseForUTM(TStr, 'LOWERRIGHT',GDALInfo.xutm_hi,GDALInfo.yutm_low);
         end;
         Metadata.Free;
         GetEPSG(GDALinfo);
         {$If Defined(RecordGDALinfo)}  WriteLineToDebugFile('GetGDALinfo out ' + fName); {$EndIf}
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
             OutName := GDALinfoOutputFName(fName);
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
                   OutName := GDALinfoOutputFName(Infiles.Strings[i]);
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
end.
