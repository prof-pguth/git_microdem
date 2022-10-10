unit dem_manager;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2015 Peter L. Guth   }
{____________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordCloseDEM}
      //{$Define ShortRecordCloseDEM}
      //{$Define RecordClosingData}
      //{$Define RecordNewMaps}
      //{$Define LoadDEMsCovering}
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
      {$Define TimeLoadDEM}
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

    {$IfDef VCL}
       forms,shlobj,
    {$EndIf}
    sysutils,System.UITypes,StrUtils,Classes,
    Petmar_types,Petmar,DEMMapDraw,DEMDefs;


function OpenAndDisplayNewScene(Files : tStringList; IndexFileName : PathStr; DisplayIt,NeedHist,ASatImage : boolean;  WhichSat : integer = 0; DEMtoAssociate : integer = 0) : integer;

procedure CloseAllDEMs;
procedure CloseSingleDEM(var DEMtoClose : integer; ResetMenus : boolean = true);

procedure CloseAllWindowsAndData;
procedure CloseEverything;
procedure CloseAllMaps;
procedure CloseSingleVectorMap(var I : integer);
procedure CloseAllDatabases;

procedure SaveMicrodemDesktop;
procedure RestoreSpecifiedDesktop(FName : PathStr);
procedure RestoreMicrodemDesktop(fName : PathStr = ''; CloseAll : boolean = true);

procedure CleanUpTempDirectory;

function GetWhatsOpen : tStringList;
procedure OpenDEMsToDebugFile(Why : shortstring);
function GetSatMaskList(ASatImage : boolean) : ANSIString;

{$IfDef Exgis}
{$Else}
   function OpenDBString : shortstring;
{$EndIf}


{$IfDef ExIndexes}
{$Else}
   const
      MaxCompare = 10;
   var
      CompareDEMIndexes : array[1..MaxCompare] of integer;
      CompareDEMNames : array[1..MaxCompare] of shortstring;
      LikeDTED : array[1..MaxCompare] of boolean;

   function LoadDEMsCoveringPoint(Lat,long : float64; LoadMap : boolean = false) : integer;
   function LoadDEMsCoveringBox(bb : sfBoundBox; LoadMap : boolean = false) : integer;
   procedure CloseCompareDEMs;
   procedure InitCompareDEMs;
   function GetCompareNames : integer;
{$EndIf}

procedure CheckGeoidNames;



//download data from USNA server

     procedure DownloadandUnzipDataFileIfNotPresent(pName : PathStr; Force : boolean = false);
     procedure GetNaturalEarthData(Force : boolean = false);
     procedure GetETOPO1(Force : boolean = false);
     procedure GetBlueMarble(Force : boolean = false);
     procedure GetGeoid;
    {$IfDef ExGeography}
    {$Else}
       procedure ClimateGetData(ForceDownload : boolean = false);
    {$EndIf}

    {$IfDef ExGeology}
    {$Else}
       procedure GeologyGetData(ForceDownload : boolean = false);
    {$EndIf}


{$IfDef ExSat}
{$Else}
   procedure CloseAllImagery;
   procedure CloseSingleSatelliteImage(var j : integer);
   procedure OpenSatImageFromDirectory(LastSatDir : PathStr);
   {$IfDef VCL}
      procedure PickAndOpenImagery(ImageType : tImageType);
      procedure PickSatDirToOpen;
   {$EndIf}
{$EndIf}

{$IfDef ExPointCloud}
{$Else}
    procedure OpenLidarMulti(theDir : PathStr = '');
{$EndIf}


procedure GeotiffMetadata(MDVersion : tMDVersion; fName : PathStr);
function GeotiffBBox(fName : PathStr) : sfBoundBox;


{$IfDef VCL}
   procedure GetMultipleDEMsFromList(TheMessage : shortstring; var DEMsWanted : tDEMbooleanArray);
   function OpenNewDEM(fName : PathStr = ''; LoadMap : boolean = true; WhatFor : shortstring = '') : integer;
   function GetTwoCompatibleGrids(WhatFor : shortString; CheckUnits : boolean; var DEM1,DEM2 : integer; WarnIfIncompatible : boolean = true;  AlwaysAsk : boolean = false) : boolean;
   function GetDEM(var DEMWanted : integer; CanCancel : boolean = false; TheMessage : ShortString = '') : boolean;
   procedure GetDEMorImage(DEM,Image : boolean; var DEMWanted,ImageWanted : integer; CanCancel : boolean = false; TheMessage : ShortString = ''; ExcludeDEM : integer = 0; ExcludeImage : integer = 0);
   function PickMap(WhatFor : shortstring) : integer;
   function PickADifferentMap(WhatFor,ThisMapCaption : shortstring) : integer;

   function EditHeaderRecord(DEM : integer; AllowChangeType : boolean{; AllowChangeDatum : boolean = true}) : boolean;
   procedure ViewHeaderRecord(DEM : integer);
   procedure EditDEMHeader;

   procedure FastRedrawAllMaps;
   function CreateNewGrid(DEMName : shortstring; What : tCreateGrid;  bb : sfBoundBox; Resolution : tDEMprecision = FloatingPointDEM; Spacing : float64 = -99) : integer;

   function FileExistsErrorMessage(InName : PathStr) : boolean;

   {$IfDef ExAutoOpen}
   {$Else}
      procedure AutoOpenOptions;
   {$EndIf}
   {$IfDef ExSat}
   {$Else}
      function GetImage(var ImageWanted : integer; CanCancel : boolean = false; TheMessage : ShortString = '') : boolean;
   {$EndIf}
{$EndIf}

//procedure AddOpenDEMsToDebugLog(what : shortstring);


function ValidDEMExt(ext : extstr) : boolean;
function ValidImageryExt(ext : extstr) : boolean;


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

   Make_Tables,
   Map_overlays,
   DEMdbTable,
   DEMCoord,
   Compress_form,
   Geotiff,
   Petmar_ini_file,
   DEMDef_routines,
   DEMhandW,
   BaseMap;


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


function OpenAndDisplayNewScene(Files : tStringList; IndexFileName : PathStr; DisplayIt,NeedHist,ASatImage : boolean;  WhichSat : integer = 0; DEMtoAssociate : integer = 0) : integer;
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
               if FileExists(LastImageName) or PathIsValid(LastImageName) then IndexFilename := LastImageName
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

               if (SatImage[Result].LandsatNumber in [1..9]) or SatImage[Result].LandsatLook then TStr := ShortLandsatName(SatImage[Result].SceneBaseName)
               else TStr := SatImage[Result].SceneBaseName;

               if (SatImage[Result].NumBands < 3) then mt := mtSatImageGray
               else begin
                  if SatImage[Result].CanEnhance then begin
                     if MDDef.SatMultiBandTrueColor then mt := mtSatTrueColor else mt := mtSatFalseColor;
                  end
                  else mt := mtUnenhancedRGB;
               end;
               CreateNewSatWindow(OpenSatView,SatImage[Result].SelectionMap,Result,mt,'Image: ' + TStr,true,DEMToAssociate);
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
      if ReportErrors then MessageToContinue(SatImage[Result].LoadErrorMessage);   //IndexFileName + MessLineBreak + 'Unacceptable Image');
      SatImage[Result].Destroy;
      SatImage[Result] := Nil;
      Result := 0;
   end;
   StopSplashing;
   wmDEM.SetMenusForVersion;
end;


function CreateNewGrid(DEMName : shortstring; What : tCreateGrid; bb : sfBoundBox; Resolution : tDEMprecision = FloatingPointDEM; Spacing : float64 = -99) : integer;
var
   Mult : int64;
   NewHeader : tDEMheader;
begin
   {$IfDef RecordNewMaps} WriteLineToDebugFile('CreateNewGrid in, UTM=' + IntToStr(MDDef.DefaultUTMZone) + MDDef.DefaultLatHemi + '  bb: ' + sfBoundBoxToString(bb,2)); {$EndIf}
   Result := 0;
   ZeroDEMHeader(NewHeader, (What in [cgUTM,cgSpecifyUTM]) );
   NewHeader.DEMPrecision := Resolution;
   NewHeader.UTMZone := MDDef.DefaultUTMZone;
   NewHeader.LatHemi := MDDef.DefaultLatHemi;
   NewHeader.DEMxSpacing := Spacing;
   NewHeader.DEMySpacing := Spacing;
   NewHeader.DEMSWCornerX := bb.XMin;
   NewHeader.DEMSWCornerY := bb.YMin;
   NewHeader.NumCol := round((bb.xmax - bb.xmin) / Spacing);
   NewHeader.NumRow := round((bb.ymax - bb.ymin) / Spacing);

   if (Resolution = FloatingPointDEM) then Mult := 4
   else if (Resolution = ByteDEM) then Mult := 1
   else Mult := 2;

   {$IfDef RecordNewMaps} WriteLineToDebugFile('CreateNewGrid call OpenAndZero'); {$EndIf}
   OpenAndZeroNewDEM(false,NewHeader,Result,DEMName,InitDEMmissing);
   {$IfDef RecordNewMaps} WriteLineToDebugFile('CreateNewGrid call CreateDEMSelectionMap'); {$EndIf}
   CreateDEMSelectionMap(Result,false,MDDef.DefElevsPercentile,mtDEMBlank);
   {$IfDef RecordNewMaps} WriteLineToDebugFile('CreateNewGrid out ' + DEMGlb[Result].FullDEMParams); {$EndIf}
   {$IfDef RecordNewMaps} WriteLineToDebugFile('CreateNewGrid out, map grid box:' + sfBoundBoxToString(DEMGlb[Result].SelectionMap.MapDraw.MapCorners.BoundBoxDataGrid,2)); {$EndIf}
end;


procedure GeotiffMetadata(MDVersion : tMDVersion; fName : PathStr);
var
   success : boolean;
   TiffImage : tTIFFImage;
   MapProjection : tMapProjection;
   RegVars : tRegVars;
   cmd : shortstring;
   OutName : PathStr;
begin
   if FileExists(fName) or GetFileFromDirectory('GeoTiff file','*.TIF;*.TIFF',FName) then begin
      if MDversion= mdMicrodem then begin
         MapProjection := tMapProjection.Create('geotiff metadata');
         TiffImage := tTiffImage.CreateGeotiff(MapProjection,RegVars,false,fName,Success,true,false);
         TiffImage.Destroy;
         MapProjection.Destroy;
      end
      else if MDversion= mdWhiteBox then WhiteBoxGeotiffMetadata(fName)
      else if MDversion= mdListGeo then begin
          OutName := MDTempDir + extractFileNameNoExt(fname) + '_metadata.txt';
          cmd := ProgramRootDir + 'listgeo\listgeo.exe ' + fname + ' > ' + Outname;
          WinExecAndWait32(cmd);
          ShowInNotepadPlusPlus(OutName,ExtractFileName(OutName));
      end
      else if MDversion= mdGDAL then GDALGeotiffToWKT(fName);
   end;
end;


function GeotiffBBox(fName : PathStr) : sfBoundBox;
var
   success : boolean;
   TiffImage : tTIFFImage;
   MapProjection : tMapProjection;
   RegVars : tRegVars;
begin
   if FileExists(fName) then begin
      MapProjection := tMapProjection.Create('geotiff metadata');
      TiffImage := tTiffImage.CreateGeotiff(MapProjection,RegVars,false,fName,Success,false,false);
      Result.XMin := RegVars.UpLeftX;
      Result.YMax := RegVars.UpLeftY;
      Result.XMax := RegVars.UpLeftX + RegVars.pr_deltaX * pred(TiffImage.TiffHeader.ImageWidth);
      Result.YMin := RegVars.UpLeftY - RegVars.pr_deltaY * pred(TiffImage.TiffHeader.ImageLength);
      TiffImage.Destroy;
      MapProjection.Destroy;
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
      WantImage : integer;
      Table : tMyData;
      fName : PathStr;
      i : integer;
   begin
      Result := 0;
      if FileExists(SeriesIndexFileName) then begin
         SaveBackupDefaults;
         fName := SeriesIndexFileName;
         Table := tMyData.Create(fName);
         Table.ApplyFilter('DATA_TYPE=' + QuotedStr('DEMS') + ' AND USE=' + QuotedStr('Y'));
         {$IfDef LoadDEMsCovering} WriteLineToDebugFile('LoadDEMsCoveringBox, filter=' + Table.Filter + '  bbox=' + sfBoundBoxToString(bb)); {$EndIf}
         i := 0;
         while not Table.eof do begin
            inc(i);
            CompareDEMNames[i] := Table.GetFieldByNameAsString('SERIES');
            LoadMapLibraryBox(CompareDEMIndexes[i],WantImage,true,bb.ymax,bb.xmin,bb.ymin,bb.xmax,CompareDEMNames[i],false);
            if (CompareDEMIndexes[i] = 0) then begin
               dec(i);
            end
            else begin
               if Table.FieldExists('SHORT_NAME') then CompareDEMNames[i] := Table.GetFieldByNameAsString('SHORT_NAME');
               DEMGlb[CompareDEMIndexes[i]].DEMheader.VerticalCSTypeGeoKey := Table.GetFieldByNameAsInteger('VERT_DATUM');
               {$IfDef LoadDEMsCovering} WriteLineToDebugFile('Series=' + CompareDEMNames[i] +  '   DEM=' + IntToStr(CompareDEMIndexes[i])); {$EndIf}
               inc(Result);
               DEMGlb[CompareDEMIndexes[i]].AreaName := CompareDEMNames[i];
               LikeDTED[i] := (DEMGlb[CompareDEMIndexes[i]].DEMHeader.RasterPixelIsGeoKey1025 = 2) or (DEMGlb[CompareDEMIndexes[i]].AreaName = 'ASTER');
               {$IfDef LoadDEMsCovering} WriteLineToDebugFile(DEMGlb[CompareDEMIndexes[i]].AreaName + ' ' + DEMGlb[CompareDEMIndexes[i]].PixelIsString); {$EndIf}
               if LoadMap then CreateDEMSelectionMap(CompareDEMIndexes[i],true,MDDef.DefElevsPercentile,MDdef.DefDEMMap);
            end;
            Table.Next;
            if (Result=MaxCompare) then begin
               if not Table.EOF then MessageToContinue('Could not load all series');
               break;
            end;
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
      bb.XMin := Long - 0.0001;
      bb.XMax := Long + 0.0001;
      bb.YMin := Lat - 0.0001;
      bb.YMax := Lat + 0.0001;
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


procedure CloseSingleDEM(var DEMtoClose : integer; ResetMenus : boolean = true);


      procedure CloseYeDEM(i : integer);
      begin
         if ValidDEM(i) then try
            try
               {$IfDef RecordCloseDEM} WriteLineToDebugFile('Destroy DEMGlb=' + IntToStr(i) + '  ' + DEMGlb[i].AreaName); {$EndIf}
               DEMGlb[i].Destroy;
               {$IfDef RecordCloseDEM} WriteLineToDebugFile('Destroy OK for DEMGlb=' + IntToStr(i)); {$EndIf}
            except
                  on Exception do ;
            end
         finally
            {$IfDef RecordCloseDEM} WriteLineToDebugFile('Have to Nil DEMGlb=' + IntToStr(i)); {$EndIf}
            DEMGlb[i] := Nil;
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
         {$If Defined(RecordCloseDEM)} WriteLineToDebugFile('In ' + CloseString); {$EndIf}
         if (DEMtoClose = PredAgesDEM) then PredAgesDEM := 0;
         if (DEMtoClose = SedThickDEM) then SedThickDEM := 0;
         if (DEMtoClose = SedTypeDEM) then SedTypeDEM := 0;

         j := DEMtoClose;
         DEMtoClose := 0;
         CloseYeDEM(j);
         ApplicationProcessMessages;
         {$IfDef VCL}
            if ResetMenus and (not SkipMenuUpdating) then WmDem.SetMenusForVersion;
         {$EndIf}
         {$If Defined(RecordCloseDEM) or Defined(RecordSimpleClose)} WriteLineToDebugFile('CloseSingleDEM out OK ' + CloseString); {$EndIf}
      end
      else begin
         {$IfDef RecordCloseDEM} WriteLineToDebugFile('No DEM to close, i=' + IntToStr(DEMtoClose)); {$EndIf}
      end;
   finally
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
         CloseSingleDEM(j,false);
      end;
      ApplicationProcessMessages;
   end;
   {$IfDef RecordClosingData} WriteLineToDebugFile('All DEMs now closed'); {$EndIf}
end;


function GetSatMaskList(ASatImage : boolean) : ANSIString;
var
   TStr : ShortString;
begin
    TStr :=  'All files|*.*|GEOTIFF|*.tif;*.tiff|' + 'Imagery with world files|*.jgw;*.tfw;*.tifw;*.gfw;*.pnw;*.sdw;*.bpw|' + 'BMP/JPEG/PNG, 3 pt reg|*.xy|JPEG2000|*.jp2|ECW|*.ecw|IMG|*.img|' + 'GeoPDF|*.pdf';
    if MrSidEnabled then TStr := TStr + 'MrSID|*.sid|';
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
          for i := pred(WMDEM.MDIChildCount) downto 0 do
             if WMDEM.MDIChildren[i] is TMapForm then begin
                {$IfDef RecordClosingData} WriteLineToDebugFile('Close ' + (WMDEM.MDIChildren[i] as TMapForm).Caption); {$EndIf}
                (WMDEM.MDIChildren[i] as TMapForm).Close;
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
   begin
      {$IfDef RecordEdit} WriteLineToDebugFile('ViewHeaderRecord in'); {$EndIf}
      EditHeader := TDEMHeaderForm.Create(Application);
      EditHeader.SetUpDEMHeaderForm(DEM);
      EditHeader.DisableEdits;
      EditHeader.ShowModal;
   end;


   procedure GetMultipleDEMsFromList(TheMessage : shortstring; var DEMsWanted : tDEMbooleanArray);
   var
      PetList : TPetList;
      i,DEMWanted,err     : integer;
   begin
      PetList := TPetList.Create(Application);
      with PetList do begin
         Caption := theMessage;
         for i := 1 to MaxDEMDataSets do DEMsWanted[i] := false;
         for i := 1 to MaxDEMDataSets do if ValidDEM(i) and (not DEMGlb[i].HiddenGrid) then
             ListBox1.Items.Add('DEM' + IntegerToString(i,4) +': ' + DEMGlb[i].AreaName);
         CancelBtn.Enabled := true;
         PetList.Width:= 400;
         PetList.ListBox1.MultiSelect := true;
         PetList.ShowModal;
         for I := 0 to pred(ListBox1.Items.Count) do begin
            if ListBox1.Selected[i] then begin
               Val(Copy(ListBox1.Items[i],5,3),DEMWanted,err);
               DEMsWanted[DEMWanted] := true;
            end;
         end;
      end;
      PetList.Destroy;
   end;


   function OpenNewDEM(fName : PathStr = ''; LoadMap : boolean = true; WhatFor : shortstring = '') : integer;
   var
      FilesWanted : tStringList;
      i : integer;
   begin
      {$If Defined(RecordIniMemoryOverwrite) or Defined(TimeLoadDEM)} IniMemOverwriteCheck('start OpenNewDEM'); {$EndIf}
      Result := 0;
      try
         DEMMergeInProgress := true;   //defined for the case of opening multiple DEMs with GetMultiple Files
         if (FName <> '') then begin
           {$If Defined(TimeLoadDEM)} WriteLineToDebugFile('OpenNewDEM passed DEM: ' + fName); {$EndIf}
           LoadNewDEM(Result,fName,LoadMap,'DEM/grid for ' + WhatFor);
         end
         else begin
           {$If Defined(TimeLoadDEM)} WriteLineToDebugFile('OpenNewDEM in'); {$EndIf}
           FilesWanted := tStringList.Create;
           FilesWanted.Add(LastDEMName);
           if GetMultipleFiles('DEM ' + WhatFor,DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then begin
              {$If Defined(TimeLoadDEM))} WriteLineToDebugFile('Files picked ' + IntToStr(FilesWanted.Count));   {$EndIf}
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
      {$If Defined(TimeLoadDEM)} if (Result = 0) then WriteLineToDebugFile('OpenNewDEM fail') else WriteLineToDebugFile('OpenNewDEM out  ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].DEMMapProjection.GetProjectionName); {$EndIf}
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
            {$If Defined(RecordStartup) or Defined(RecordProjects)} WriteLineToDebugFile('MDdef.AutoOpen = aoMultigrid');   {$EndIf}
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
         OpenAndDisplayNewScene(Nil,LastImageName,true,true,(not GlobalDRGMap));
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

      procedure OpenSatImageFromDirectory(LastSatDir : PathStr);
      var
         fName : PathStr;
         i,NewSatImage : integer;
         TheFiles,Files2 : tStringList;
      begin
         if WarnAboutSpaces(LastSatDir) then exit;

         {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('OpenSatImageFromDirectory in, ' + LastSatDir);   {$EndIf}
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
            {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('OpenSatImageFromDirectory, jp2 files=' + IntToStr(TheFiles.Count));   {$EndIf}
            for I := 0 to pred(TheFiles.Count) do begin
               if AnsiContainsText(TheFiles.Strings[i],'IMG_DATA') then begin
                  wmdem.SetPanelText(0,'Convert JP2: ' + IntToStr(succ(i)) + '/' + IntToStr(TheFiles.Count));
                  fName := TheFiles.Strings[i];
                  GDAL_warp(fName);
                  if MDDef.DeleteJP2 then File2Trash(fName);
               end;
            end;
            FreeAndNil(TheFiles);
            wmdem.SetPanelText(0,'');
            FindMatchingFiles(LastSatDir,'*.tif',TheFiles,6);
         end
         else begin
            //already have TIFFs, but maybe need to delete JP2 that were not originally deleted
            if MDDef.DeleteJP2 then begin
               Files2 := Nil;
               FindMatchingFiles(LastSatDir,'*.jp2',Files2,8);
               for I := 0 to pred(Files2.Count) do DeleteFile(Files2.Strings[i]);
               Files2.Free;
            end;
         end;

         if IsThisSentinel2(TheFiles.Strings[0]) then begin
            //there might be metadata or other TIFFS we don't want to open
            for i := pred(TheFiles.Count) downto 0 do begin
               if not AnsiContainsText(TheFiles.Strings[i],'IMG_DATA') then TheFiles.Delete(i);
            end;
         end;

         {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('OpenSatImageFromDirectory, TIF files=' + IntToStr(TheFiles.Count));   {$EndIf}
         if (TheFiles.Count > 0) then begin
            if (TheFiles.Count = 1) then i := 0 else i := 1;      //for Sentinel-2, where band 1 is low resolution
            OpenAndDisplayNewScene(Nil,TheFiles.Strings[i],true,true,true);
         end;
         TheFiles.Free;
         UpdateMenusForAllMaps;
         {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('OpenSatImageFromDirectory out');   {$EndIf}
      end;


      procedure PickSatDirToOpen;
      var
         Paths : tStringList;
         i : integer;
      begin
         {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('PickSatDirToOpen in');   {$EndIf}
         Paths := tStringList.Create;
         Paths.Add(LastSatDir);
         if GetMultipleDirectories('Landsat or Sentinel-2 image',Paths) then begin
            for i := 0 to pred(Paths.Count) do begin
               LastSatDir := Paths[i];
               {$If Defined(RecordSatLoad) or Defined(RecordSatDirOpen)} WriteLineToDebugFile('call OpenSatImageFromDirectory, ' + LastSatDir);   {$EndIf}
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
            if StrUtils.AnsiContainsText(Uppercase(fname),'DRG') then begin
               GlobalDRGMap := true;
               ImageType := itDRG;
            end;
            ShowHourglassCursor;
            ShlObj.SHAddToRecentDocs(SHARD_PATH, PChar(TheFiles.Strings[i]));
            {$If Defined(RecordSatLoad) or Defined(RecordMenu) or Defined(TimeSatLoad)} WriteLineToDebugFile('call OpenAndDisplay ' + fName); {$EndIf}
            NewSatImage := OpenAndDisplayNewScene(Nil,fName,true,(ImageType <> itDRG),(not GlobalDRGMap));
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
         if ValidSatImage(j) then begin
            {$IfDef RecordClosingData} WriteLineToDebugFile('CloseSingleSatelliteImage in, j=' + IntToStr(j));   {$EndIf}
            {$IfDef VCL} SkipMenuUpdating := true; {$EndIf}
            FreeAndNil(SatImage[j]);
            {$IfDef VCL} SkipMenuUpdating := false; {$EndIf}
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
            DEMWanted : integer;
         begin
            if (not CanCancel) and (NumSatImageOpen = 1) then ImageWanted := 1
            else GetDEMorImage(false,true,DEMWanted,ImageWanted,CanCancel,TheMessage);
            Result := (ImageWanted <> 0);
         end;
      {$EndIf}
{$EndIf}


procedure CloseAllDatabases;
var
   i,j : integer;
begin
   for i := pred(WMDEM.MDIChildCount) downto 0 do
      if WMDEM.MDIChildren[i] is Tdbtablef then (WMDEM.MDIChildren[i] as Tdbtablef).Close;

   for i := 1 to MaxDataBase do begin
      j := i;
      if ValidDB(j) then begin
         CloseAndNilNumberedDB(j);
      end;
   end;

   FastRedrawAllMaps;
end;


procedure CloseEverything;
begin
    {$IfDef RecordClosingData} WriteLineToDebugFile('CloseEverything in'); {$EndIf}
    CloseAllDEMs;
    {$IfDef ExSat}
    {$Else}
       CloseAllImagery;
    {$EndIf}
    CloseAllDatabases;
    {$IfDef RecordClosingData} WriteLineToDebugFile('CloseEverything out'); {$EndIf}
end;


procedure CloseAllWindowsAndData;
{$IfDef VCL}
var
   j,i: integer;
{$EndIf}
begin
   {$IfDef RecordClosingData} WriteLineToDebugFile('DEM_Manager.CloseAllWindowsAndData enter'); {$EndIf}
   CloseAllMultigrids;
   CloseEverything;

   {$IfDef VCL}
      if (WmDEM.MDIChildCount > 0) then begin
         for j := WmDEM.MDIChildCount-1 downto 0 do begin
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
                  if not AnswerIsYes('Cannot pick the same grid for both; retry') then begin
                     Result := false;
                     exit;
                  end
               end;
            until (DEM1 <> DEM2);
            Result := DEMGlb[DEM1].SecondGridIdentical(DEM2);
            {$IfDef RecordGet2DEMs} WriteLineToDebugFile('Identical grids=' + TrueOrFalse(Result)); {$EndIf}
            if CheckUnits then Result := Result and (DEMGlb[DEM1].DEMheader.ElevUnits = DEMGlb[DEM2].DEMheader.ElevUnits);
            if WarnIfIncompatible and (not Result) then MessageToContinue('Incompatible grids');
            {$IfDef RecordGet2DEMs} WriteLineToDebugFile('Out of GetTwoCompatibleGrids=' + TrueOrFalse(Result)); {$EndIf}
         end;
      end;


      function GetDEM(var DEMWanted : integer; CanCancel : boolean = false; TheMessage : ShortString = '') : boolean;
      var
         ImageWanted : integer;
      begin
         if (NumDEMDataSetsOpen = 1)  and ValidDEM(1) then DEMWanted := 1
         else GetDEMorImage(true,false,DEMWanted,ImageWanted,CanCancel,TheMessage);
         Result := (DEMWanted <> 0);
      end;


      procedure GetDEMorImage(DEM,Image : boolean; var DEMWanted,ImageWanted : integer; CanCancel : boolean = false; TheMessage : ShortString = ''; ExcludeDEM : integer = 0; ExcludeImage : integer = 0);
      var
         TheList : tStringList;
         i,Wanted,err  : integer;
         TStr,TStr2 : ShortString;
      begin
         DEMWanted := 0;
         ImageWanted := 0;
         Wanted := 0;
         TheList := TStringList.Create;
         if DEM then begin
            TStr2 := 'DEM';
            for i := 1 to MaxDEMDataSets do if (ValidDEM(i)) and (not DEMGlb[i].HiddenGrid) and (ExcludeDEM <> i) then
               TheList.Add('DEM' + IntegerToString(i,4) +': ' + DEMGlb[i].AreaName);
         end;
         {$IfDef ExSat}
         {$Else}
            if Image then begin
               TStr2 := 'Image';
               for i := 1 to MaxSatAllowed do begin
                  if (SatImage[i] <> Nil) and (ExcludeImage <> i) then
                    TheList.Add('Image ' + IntToStr(i) +': ' + SatImage[i].SceneTitle + ' (' + SatImage[i].SceneBaseName + ')');
               end;
            end;
         {$EndIf}
         {$IfDef VCL}
            if (TheList.Count = 1) or GetFromListZeroBased(TStr2 + ' for ' + TheMessage,Wanted,TheList,CanCancel) then begin
               TStr := TheList.Strings[Wanted];
               if Copy(TStr,1,3) = 'DEM' then begin
                  Val(Copy(TStr,5,3),DEMWanted,err);
               end
               else begin
                  Val(Copy(TStr,7,2),ImageWanted,err);
               end;
            end;
         {$EndIf}
         TheList.Free;
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
         Petmar.GetFromListZeroBased(WhatFor,Result,Maps);
      end;


      function PickADifferentMap(WhatFor,ThisMapCaption : shortstring) : integer;
      var
         Maps : tStringList;
         i : integer;
      begin
         Maps := tStringList.Create;
         for i := 0 to pred(WMDEM.MDIChildCount) do
            if (WMDEM.MDIChildren[i] is tMapForm) then Maps.Add(WMDEM.MDIChildren[i].Caption);
         Result := 0;
         Petmar.GetFromListZeroBased(WhatFor,Result,Maps);
      end;


procedure DownloadandUnzipDataFileIfNotPresent(pName : PathStr; Force : boolean = false);
//pname is a subdirectory of c:\mapdata\, without the final backslash
var
   pName2 : PathStr;
begin
   {$IfDef RecordDownload} WriteLineToDebugFile('DownloadandUnzipDataFileIfNotPresent for ' + PName); {$EndIf}
   StopSplashing;
   if (not Force) and PathIsValid(MainMapData + pname) then begin
      {$IfDef RecordDownload} WriteLineToDebugFile('DownloadandUnzipDataFileIfNotPresent, already there'); {$EndIf}
      exit;
   end;

   pName2 := pName + '.zip';
   if not DownloadFileFromWeb(WebDataDownLoadDir + pName2,MainMapData + pName2) then begin
      pName2 := pName + '.7z';
      DownloadFileFromWeb(WebDataDownLoadDir + pName2,MainMapData + pName2);
   end;

(*
   if FileExists(pName2) then begin
      if AnswerIsYes('Try unzipping previous download instead of downloading again') then begin
         {$IfDef RecordDownload} WriteLineToDebugFile('DownloadandUnzipDataFileIfNotPresent, unzipping existing ZIP'); {$EndIf}
      end
      else begin
         {$IfDef RecordDownload} WriteLineToDebugFile('DownloadandUnzipDataFileIfNotPresent, download ZIP'); {$EndIf}
         DeleteFile(MainMapData + pName2);
         DownloadFileFromWeb(WebDataDownLoadDir + pName2,MainMapData + pName2);
      end;
   end
   else begin
      {$IfDef RecordDownload} WriteLineToDebugFile('DownloadandUnzipDataFileIfNotPresent, force overwrite directory'); {$EndIf}
      DownloadFileFromWeb(WebDataDownLoadDir + pName2,MainMapData + pName2);
   end;
*)
   pName2 := MainMapData + pName2;
   UnzipSingleFile(pName2);
   File2Trash(pName2);
end;


      {$IfDef ExGeography}
      {$Else}
         procedure ClimateGetData(ForceDownload : boolean = false);
         begin
            {$IfDef RecordDownload} WriteLineToDebugFile('ClimateGetData in'); {$EndIf}
            DownloadandUnzipDataFileIfNotPresent('climate',ForceDownload);
         end;
      {$EndIf}


      {$IfDef ExGeology}
      {$Else}
         procedure GeologyGetData(ForceDownload : boolean = false);
         begin
            {$IfDef RecordDownload} WriteLineToDebugFile('GeologyGetData in'); {$EndIf}
            DownloadandUnzipDataFileIfNotPresent('geology',ForceDownload);
         end;
      {$EndIf}


      procedure GetETOPO1;
      var
         dName : PathStr;
      begin
         ETOPODEMName := MainMapData + 'etopo1\etopo1.dem';
         if Force or (Not FileExists(ETOPODEMName)) then begin
            dName := ExtractFilePath(ETOPODEMName);
            SafeMakeDir(dName);
            dName := ChangeFileExt(ETOPODEMName,'.zip');
            DownloadFileFromWeb(WebDataDownLoadDir + ExtractFileName(dName),dName);
            ZipMasterUnzip(dName,ExtractFilePath(dName));
         end;
      end;


      procedure GetBlueMarble;
      var
         dName : PathStr;
      begin
         if Force or (Not FileExists(BlueMarbleFName)) then begin
            dName := ExtractFilePath(BlueMarbleFName);
            SafeMakeDir(dName);
            dName := ChangeFileExt(BlueMarbleFName,'.zip');
            DownloadFileFromWeb(WebDataDownLoadDir + 'blue_marble.zip',dName);
            ZipMasterUnzip(dName,ExtractFilePath(dName));
         end;
      end;


      procedure GetNaturalEarthData(Force : boolean = false);
      var
         dName,grDir : PathStr;
      begin
         if Force or (Not PathIsValid(DBDir + 'natural_earth_vector\10m_physical')) then begin
            dName := 'natural_earth_vector.zip';
            DownloadFileFromWeb(WebDataDownLoadDir + dName,DBDir + dName);
            ZipMasterUnzip(DBDir + dName,DBDir);
            grDir := MainMapData + 'natural_earth_vector\';
            LargeScaleWorldOutlines := DBDir + 'Natural_Earth_Vector\NE_10M_VECTORS' + DefaultDBExt;
            MedScaleWorldOutlines := DBDir + 'Natural_Earth_Vector\NE_50M_VECTORS' + DefaultDBExt;
            SmallScaleWorldOutlines := DBDir + 'Natural_Earth_Vector\NE_110M_VECTORS' + DefaultDBExt;
         end;
      end;


      procedure GetGeoid;
      var
         dName : PathStr;
      begin
         if (Not FileExists(Geoid2008FName)) then begin
            DownloadandUnzipDataFileIfNotPresent('geoid');
            CheckGeoidNames;
         end;
      end;


procedure CheckGeoidNames;
begin
   Geoid2008FName := MainMapData + 'geoid\egm2008-2.5.tif';
   if not FileExists(Geoid2008FName) then Geoid2008FName := MainMapData + 'geoid\egm2008-5.tif';
   Geoid96FName := MainMapData + 'geoid\egm96-15.tif';
   GeoidDiffFName := MainMapData + 'geoid\egm96_to_egm2008.tif';
end;


{$EndIf}


{$IfDef Exgis}
{$Else}
   function OpenDBString : shortstring;
   begin
      Result := 'Open DBs: ' + IntToStr(NumOpenDB);
   end;
{$EndIf}


procedure OpenDEMsToDebugFile(Why : shortstring);
var
   i : integer;
begin
   HighlightLineToDebugFile(why);
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         //writeLineToDebugFile(IntegerToString(i,5) + '  ' + DEMGlb[i].AreaName);
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
            Result.Add(IntegerToString(i,5) + '  ' + DEMGlb[i].AreaName + '  (' + DEMGlb[i].ColsRowsString + '  ' +  DEMGlb[i].DemSizeString + ')');
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


procedure CleanUpTempDirectory;
var
   bf : tstringlist;
begin
   {$If Defined(MessageStartup)} or Defined(CheckForNTLDR)} MessageToContinue('CleanUpTempDirectory ' + MDTempDir); {$EndIf}

   ShowHourglassCursor;
   {$IfDef VCL}
      if (CurrentProject <> '') then DeleteMultipleFiles(CurrentProject + '\','*.*');
   {$EndIf}

   if PathIsValid('c:\mapdata\temp\grass1') then begin
      bf := tstringlist.Create;
      bf.Add( 'rd /S /Q c:\mapdata\temp\grass1');
      EndBatchFile(MDTempdir + 'clear_grass.bat',bf);
   end;


   if (MDTempDir <> '') then begin
      ChDir(MDTempDir);
      DeleteMultipleFiles(MDTempDir, '*.*');
      DeleteMultipleFiles(MDTempDir + 'ts\', '*.*');
      DeleteMultipleFiles(MDTempDir + 'db_aux\', '*.*');
      DeleteMultipleFiles(MainMapData + 'Icons\','beach_ball_*.*');
      if MDDef.CleanKMLDirOnClosing then CleanOutDirectory(MainMapData + 'kml\');
      CleanOutDirectory(MDTempDir);
   end;
   ShowDefaultCursor;
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
                     mt := MDdef.DefDEMMap;
                     if Table.FieldExists('MAP_TYPE') then begin
                        mt := Table.GetFieldByNameAsInteger('MAP_TYPE');
                        if mt = 0 then mt := MDdef.DefDEMMap;
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
                        WantedSat := OpenAndDisplayNewScene(Nil,fname,true,true,true);
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
                     if (WantedDEM <> 0) then DEMGlb[WantedDem].SelectionMap.LoadDataBaseFile(fName);
                  end
                  else MessageToContinue('Cannot find ' + fName);
               end;

               {$IfDef ExPointCloud}
               {$Else}
                  if (What = 'PC1') or (What = 'PC2') or (What = 'PC3') or (What = 'PC4') then begin
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
               if (WantedDEM <> 0) then DEMGlb[WantedDem].SelectionMap.CheckProperTix;
               Table.Next;
            end;
            Table.Destroy;
         end;
      end;
      {$IfDef RecordProjects} WriteLineToDebugFile('RestoreSpecifiedDesktop processing done, LastDESKtop=' + LastDesktop); {$EndIf}
   finally
      HeavyDutyProcessing := false;
      wmDEM.SetMenusForVersion;
      wmDEM.Cascade;
      StopSplashing;
   end;
   {$IfDef RecordProjects} WriteLineToDebugFile('RestoreSpecifiedDesktop out, LastDESKtop=' + LastDesktop); {$EndIf}
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
            Table.SetBoundingBox(MapOwner.MapDraw.MapCorners.BoundBoxGeo);
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
            if DEMGlb[i].VegDensityLayers[1] <> Nil then AddLine('VOX1',LastVegDensity1fName,Nil);
            if DEMGlb[i].VegDensityLayers[2] <> Nil then AddLine('VOX2',LastVegDensity2fName,Nil);
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
finalization
   {$IfDef RecordClosingData} WriteLineToDebugFile('RecordClosingProblems in DEM_Manager'); {$EndIf}
   {$IfDef RecordWhatsOpen} WriteLineToDebugFile('RecordWhatsOpenProblems in DEM_Manager'); {$EndIf}
   {$IfDef RecordCloseDEM} WriteLineToDebugFile('RecordCloseDEM in DEM_Manager'); {$EndIf}
   {$IfDef RecordProjects} WriteLineToDebugFile('RecordProjects in DEM_Manager'); {$EndIf}
   {$IfDef TimeLoadDEM} WriteLineToDebugFile('TimeLoadDEM in DEM_Manager'); {$EndIf}
   {$IfDef TimeSatLoad} WriteLineToDebugFile('TimeSatLoad in DEM_Manager'); {$EndIf}
   {$IfDef RecordDownload} WriteLineToDebugFile('RecordDownload in DEM_Manager'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Finalized DEM_Manager'); {$EndIf}
end.

