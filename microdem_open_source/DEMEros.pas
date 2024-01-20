unit DEMEROS;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

//{$IfDef ConvertDNInline}


//{$Define AllowInlines}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define RecordLoadSat}
      {$Define RecordEOBrowser}
      //{$Define RecordKeyDraw}
      //{$Define RecordTMSat}
      //{$Define RecordShortLandsat}
      //{$Define RecordDrawSatOnMap}
      //{$Define RecordSatRegistration}
      //{$Define RecordSatColor}
      //{$Define RecordByteLookup}
      //{$Define RecordTMSatFull}
      //{$Define RecordHistogram}
      //{$Define RecordDetailedSatColor}
      //{$Define RecordDefineDatum}
      //{$Define RecordWorldFile}
      //{$Define RecordNewSat}
      //{$Define RecordClosing}
      //{$Define RecordLoadClass}
      //{$Define RecordScattergram}
      //{$Define RecordGDAL}
      //{$Define RecordSatTimeSeries}
      //{$Define RecordSpectralLibraryGraph}
      //{$DefIne RecordSatFrame}
      //{$Define RecordSat}
      //{$Define RecordSatInterpolateColor}
      //{$Define RecordGetSatRow}              //serious slowdown
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

   System.UITypes,
   Classes, StrUtils,Math, SysUtils,

   {$IfDef MSWindows}
      Winapi.Windows,
   {$EndIf}

   {$IfDef VCL}
      Graphics, Forms,Controls,ExtCtrls,Menus,
      BaseGraf,
      DEMMapf,
   {$EndIf}

   {$If Defined(ExSat) or Defined(ExNLCD)}
   {$Else}
      DEM_NLCD,
   {$EndIf}

   {$IfDef ExAdvancedSats}
   {$Else}
      Multigrid,
      {$IfDef ExHyperSpectral}
      {$Else}
         HyperSpectral_Image,
      {$EndIf}
   {$EndIf}

   {$IfDef ExTIN}
   {$Else}
      DEM_Tin,
   {$EndIf}

   DEMCoord,
   DEMDefs,
   BaseMap,
   DEMMapDraw,
   GEOTIFF,
   PETMAR,Petmar_types,PETMath;

{$A-}

const
   MaxTiffsInImage = 15;
   shOneBand  = 1;
   shAllBands = 2;
   shRGBBands = 3;

type
   tSatImage = class
      protected
      private
         Data16Bit : boolean;
         ImageOffsetInFile    : integer;
         Row16Bit : ^tWordRow16bit;
         SatRow : array[1..4] of ^tWordRow16bit;
         procedure SetUpColors(MapDraw : tMapDraw; SatView : tSatView);
         function YOffset(Band,y : integer) : LongInt;
         procedure AssignClusterColors(ShowClasses : integer; var Colors : tColors256);
         procedure ReadBandData(SatName: shortstring; var SatView : tSatView);
         {$If Defined(RecordSatColor) or Defined(RecordByteLookup)}
            function ContrastName(SatView : tSatview) : shortstring;
         {$EndIf}
         function BandRange(Band : integer) : shortstring;
         procedure DefineImageFromTiff(WhichOne: integer);
         procedure CloseTiffImages(SatView : tSatView);
      public
         {$IfDef NoBMPFileImagery}
         {$Else}
            BMPFile : boolean;               //must be public
            BMPFileName : PathStr;
         {$EndIf}
         FullWorld,
         CanEnhance,
         UTMValidImage,
         HistogramNeedsInitialization,
         BandNamesRead,
         IsSentinel2,
         LandsatLook,
         EOBrowserExport,
         Is4band,
         MultiTiff,
         SingleTiff,
         WV2Image,
         AnImageMap       : boolean;
         LandsatNumber    : byte;      //0 if not Landsat
         LoadErrorMessage,
         SatelliteName,
         SceneTitle       : shortstring;
         SceneBaseName    : AnsiString;
         CurrentSidName,
         OriginalFileName,
         LandsatDir,
         IndexFileName    : PathStr;
         BandLongName,
         BandShortName    : array[1..MaxBands] of ShortString;
         BFileName        : array[1..MaxBands] of PathStr;
         SatBandFile      : array[1..MaxBands] of file;
         BandRows,
         BandColumns      : array[1..MaxBands] of Int32;
         BandXSpace,
         BandYSpace       : array[1..MaxBands] of float32;
         MinRef,MaxRef    : array[1..MaxBands] of word;
         Distrib          : array[1..MaxBands] of ^tDistFreq;
         BandWavelengths  : tMaxBandsArray;
         Grays            : tRGBWordLookUp;
         GraysLookUp      : tColorIndex;
         ColorLookUp      : tColorLookUp;
         RegVars          : tRegVars;
         ImageMapProjection : tMapProjection;
         DefRedTrue,DefGreenTrue,DefBlueTrue,DefIR2,
         DefRedFalse,DefGreenFalse,DefBlueFalse,
         BandForSize,MissingDataValue,SatRecSize,
         NumSatRow,NumSatCol,NumBands : integer;
         NumPoints  : array[1..MaxBands] of int64;
         MetersPerPixel,
         xMetersPerPixel,
         yMetersPerPixel  : float64;
         DefinedImageColors : ^TRGBLookUp;
         TiffImage       : array[1..MaxTiffsInImage] of tTIFFImage;
         LandsatMetadata : tLandsatMetadata;
         {$IfDef VCL}
            SelectionMap  : tMapForm;
         {$EndIf}

         {$IfDef ExTIN}
             SatTINRegistration : tMyBitmap;
         {$Else}
             SatTINRegistration : DEM_TIN.tTIN;
         {$EndIf}

         {$IfDef ExHyperSpectral}
            HypersectralImage : tMyBitmap;
         {$Else}
            HypersectralImage : tHypersectralImage;
         {$EndIf}

         constructor Create(var SatView : tSatView; Files : tStringList; var ReadFileName : PathStr;  NeedHist : boolean; var Success : boolean);
         destructor Destroy; override;
         procedure InitializeHistogramDistributions;
         function BandSpacingChange(var SatView : tSatView) : boolean;

         procedure DisplayImage(SatView : tSatView; MapDraw : tMapDraw; Bitmap : tMyBitmap);

         procedure GetSatRow(Band,Row : integer; var TheRow : tImageRow);  {$IfDef AllowInlines} inline; {$EndIf}
         procedure GetSatRow16bit(Band,Row : integer; var TheRow : tWordRow16bit); {$IfDef AllowInlines} inline; {$EndIf}
         function GetSatPointValue(Band,Col,Row : integer) : integer;
         function ConvertDN(DN : word; Band : integer; HowConvert : tDNconvert = dncMDDefault) : float64; {$IfDef ConvertDNInline} inline; {$EndIf}

         function IsLandsatImageAnalysisBand(Band : integer) : boolean;
         function FindTMBand(TMBandName : integer; var BandPresent : integer) : boolean;

         function ImageDateString : shortstring;

         function GetLandsatBQAName : shortstring;
         function GetLandsatMetadataName : shortstring;

         function SatelliteImageLimits : string;
         function ImageKeyInfo : AnsiString;
         function RegInfo : AnsiString;

         procedure SatGridToUTM(Band : integer; x,y : float64; var XUTM,YUTM : float64);
         procedure UTMtoSatGrid(Band : integer; X,Y : float64; var Xu,Yu : float64);
         procedure SatGridToProjectedCoords(Band : integer; x,y : float64; var Xproj,Yproj : float64);   {$IfDef AllowInlines} inline; {$EndIf}

         procedure LatLongDegreeToSatGrid(Band : integer; Lat,Long: float64;  var Xu,Yu : float32);
         procedure SatGridToLatLongDegree(Band : integer; XGrid,YGrid : float32; var Lat,Long : float64);

         function SatGridInDataSet(Band : integer; XGrid,YGrid : float64) : boolean;
         function LatLongDegreeInDataSet(Lat,Long : float64) : boolean;

         procedure ClipSatGrid(Band : integer; var x,y : float64); overload;
         procedure ClipSatGrid(Band : integer; var x,y : float32); overload;
         procedure ClipSatGrid(Band : integer; var x,y : integer); overload;

         function SatelliteBoundBoxGeo(Band : integer) : sfBoundBox;
         function SatelliteBoundBoxProj(Band : integer) : sfBoundBox;
         function SatelliteBoundBoxDataGrid(Band : integer) : sfBoundBox;

         procedure LoadHistogram;

         function BandAxisValue(Band : integer) : float64;

         function FullImageGridLimits : tGridLimits;

         procedure CreateDBfromSatImage(var fName : PathStr; bb : sfBoundbox);

         procedure SatSceneStatistics(var n : integer; var Correlations,VarCoVar : tTrendMatrix; var Mean,Min,Max,StdDev : tMaxBandsArray);

         {$IfDef VCL}
            procedure PickBand(aMessage : ShortString; var WantedBand : integer);
            procedure PickMultipleBands(aMessage : ShortString; var UseBands : tUseBands);
            procedure OptimumIndexFactor;
            function MakeNewBand(NewBand : tNewSatBand; OpenMap : boolean = true) : integer;
            {$IfDef NoDBGrafs}
            {$Else}
               function GraphHistogram(Desired : integer; WhichBand : integer) : TThisBaseGraph;
               procedure SatHistograms(xlo,ylo,xhi,yhi : integer);
               function EnhancementGraph : TThisBaseGraph;
            {$EndIf}
         {$EndIf}

         {$IfDef ExLandsatQA}
         {$Else}
            function GetLandsatQA(fName : PathStr) : tStringList;
            procedure GetLandsatQAMap(What : ShortString;fName : PathStr; LowBitV,HighBitV : integer);
         {$EndIf}
   end;


var
   NumSatImageOpen : integer;
   SatImage : array[1..MaxSatAllowed] of tSatImage;
   TreatThisAsSingleTif : boolean;

   function ValidSatImage(i : integer) : boolean;

   procedure InitializeSatView(var SatView : tSatView);

   function LandsatSceneMetadata(fName : PathStr; var LandsatNumber : byte; var MPath : PathStr) : tStringList;
   procedure GetLandsatMetadata(fName : PathStr; var LandsatMetadata : tLandsatMetadata);
   function ShortLandsatName(fName : PathStr) : ShortString;
   function HistogramLandsatName(fName : PathStr) : ShortString;
   function IsThisSentinel2(fName : PathStr) : boolean;
   function IsEOBrowserExport(fName : PathStr) : boolean;
   function IsLandsat(fName : PathStr; var MPath : PathStr) : boolean;

   {$IfDef ExLandsatQA}
   {$Else}
      function GetLandsatQA(fName : PathStr) : tStringList;
      procedure GetLandsatQAMap(What : ShortString;fName : PathStr; LowBitV,HighBitV : integer);
   {$EndIf}

   {$IfDef VCL}
      function SpectralLibraryGraph(fName : PathStr = ''; MaxVert : float64 = 100) : TThisBaseGraph;
   {$EndIf}


implementation

Uses
   {$IfDef VCL}
      Nevadia_Main,
      DEM_Indexes,
      Toggle_db_use,
      GetLatLn,
   {$EndIf}

   {$IfDef ExGeoStats}
   {$Else}
      DEMStat,
   {$EndIf}

   {$IfDef ExAdvancedSats}
   {$Else}
      DEMSatContrast,
      Petimage_form,
   {$EndIf}

   {$IfDef ExDataManip}
   {$Else}
      DEMHandW,
   {$EndIf}

   {$IfDef ExcludeExternalTools}
   {$Else}
      MD_Use_tools,
   {$EndIf}

   DEMDef_routines,
   DEMdatabase,
   PetImage,
   PetDBUtils,
   DEM_Manager,
   gdal_tools,
   Make_tables;


  {$I demeros_landsat.inc}


function IsThisSentinel2(fName : PathStr) : boolean;
//  S2B_MSIL1C_20220602T082559_N0400_R021_T37TCN_20220602T121719.SAFE
//  L1C_T37TCN_A027360_20220602T082807           {inside granule folder}
begin
   fName := UpperCase(fName);
   Result := {StrUtils.AnsiContainsText(UpperCase(fName),'SENTINEL') or}
            (StrUtils.AnsiContainsText(fName,'_B') and (StrUtils.AnsiContainsText(fName,'L1C_T') or StrUtils.AnsiContainsText(fName,'L 1C_'))) or
            StrUtils.AnsiContainsText(fName,'S2A_MSI_') or
            StrUtils.AnsiContainsText(fName,'S2B_MSI_');
end;


function IsEOBrowserExport(fName : PathStr) : boolean;
begin
   fName := UpperCase(fName);

//2022-05-03-00_00_2022-05-03-23_59_Sentinel-1_AWS-IW-VVVH_RGB_ratio
//2022-05-03-00_00_2022-05-03-23_59_Sentinel-2_L2A_Scene_classification_map

   Result := StrUtils.AnsiContainsText(fName,'_SENTINEL-1_AWS') or StrUtils.AnsiContainsText(fName,'_SENTINEL-2_L2A_') or StrUtils.AnsiContainsText(fName,'_SENTINEL-2_L1C_');
end;


function tSatImage.ImageDateString : shortstring;
begin
   if IsSentinel2 then Result := Copy(SceneBaseName,8,4) + '-' + Copy(SceneBaseName,12,2) + '-' + Copy(SceneBaseName,14,2)
   else Result := '';
end;


function ValidSatImage(i : integer) : boolean;
begin
   Result := (i in [1..MaxSatAllowed]) and (SatImage[i] <> nil);
end;


procedure InitializeSatView(var SatView : tSatView);
begin
   {$IfDef RecordSatForm} WriteLineToDebugFile('InitializeSatView in'); {$EndIf}
   Satview.WindowContrast := MDdef.ContrastEnhancement;
   Satview.WindowContrastLowTailSize := MDdef.LowTailSize;
   Satview.WindowContrastHighTailSize := MDdef.HighTailSize;
   Satview.BandInWindow := 1;
   SatView.BandInWindow:= 3;
   SatView.RedBand := 3;
   SatView.GreenBand := 2;
   SatView.BlueBand := 1;
   SatView.PanSharpenImage := false;
   {$IfDef RecordSatForm} WriteLineToDebugFile('InitializeSatView out'); {$EndIf}
end;


function tSatImage.BandSpacingChange(var SatView : tSatView) : boolean;
var
   OldSpacing : float64;
begin
   OldSpacing := BandXSpace[BandForSize];
   if IsSatelliteColorImage(SelectionMap.MapDraw.MapType) then begin
      BandForSize := SelectionMap.MapDraw.SatView.RedBand;
   end
   else begin
      BandForSize := SelectionMap.MapDraw.SatView.BandInWindow;
   end;

   Result := abs(OldSpacing - BandXSpace[BandForSize]) > 0.01;

   if (Result) then with SelectionMap.MapDraw do begin
      {$If Defined(RecordPickBand) or Defined(RecordPixelSize)}
         WriteLineToDebugFile('tEROSContrastForm.BandSpacingChange change detected, new size band=' + IntToStr(SatImage[Sat].BandForSize));
         WriteLineToDebugFile(' old projected limits,  ' + sfBoundBoxToString(MapCorners.BoundBoxProj,1));
         WriteLineToDebugFile(' old data grid limits,  ' + sfBoundBoxToString(MapCorners.BoundBoxDataGrid,1));
         WriteLineToDebugFile(' old geographic limits, ' + sfBoundBoxToString(MapCorners.BoundBoxGeo,6));
      {$EndIf}
      NumSatCol := BandColumns[BandForSize];
      NumSatRow := BandRows[BandForSize];
      ProjectedCoordsToDataGrid(MapCorners.BoundBoxProj.xmin,MapCorners.BoundBoxProj.ymin,MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymax);
      ProjectedCoordsToDataGrid(MapCorners.BoundBoxProj.xmax,MapCorners.BoundBoxProj.ymax,MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymin);
      {$If Defined(RecordPickBand) or Defined(RecordPixelSize)}
         WriteLineToDebugFile(' new projected limits,  ' + sfBoundBoxToString(MapCorners.BoundBoxProj,1));
         WriteLineToDebugFile(' new data grid limits,  ' + sfBoundBoxToString(MapCorners.BoundBoxDataGrid,1));
         WriteLineToDebugFile(' new geographic limits, ' + sfBoundBoxToString(MapCorners.BoundBoxGeo,6));
      {$EndIf}
   end;
end;


procedure tSatImage.InitializeHistogramDistributions;
var
   i : integer;
begin
   if HistogramNeedsInitialization then begin
      for i := 1 to NumBands do begin
         New(Distrib[i]);
         FillChar(Distrib[i]^,SizeOf(tDistFreq),0);
         if MDdef.IgnoreHistogramZero then MinRef[i] := 1
         else MinRef[i] := 0;
         MaxRef[i] := MaxRefCount;
      end {for i};
   end;
   HistogramNeedsInitialization := false;
end;


function tSatImage.SatelliteBoundBoxGeo(Band : integer)  : sfBoundBox;
begin
   SatGridToLatLongDegree(Band,0,0,Result.ymax,Result.xmin);
   SatGridToLatLongDegree(Band,pred(BandColumns[Band]),pred(BandRows[Band]),Result.ymin,Result.xmax);
end;


function tSatImage.SatelliteBoundBoxProj(Band : integer) : sfBoundBox;
begin
   Result.xmin := RegVars.UpleftX;
   Result.xmax := RegVars.UpleftX + pred(BandColumns[Band]) * BandXSpace[Band];
   Result.ymin := RegVars.UpleftY - pred(BandRows[Band]) * BandYSpace[Band];
   Result.ymax := RegVars.UpleftY;
end;


function tSatImage.SatelliteBoundBoxDataGrid(Band : integer) : sfBoundBox;
begin
   Result.ymax := pred(BandRows[Band]);
   Result.ymin := 0;
   Result.xmax := pred(BandColumns[Band]);
   Result.xmin := 0;
end;



{$IfDef VCL}

function SpectralLibraryGraph(fName : PathStr = ''; MaxVert : float64 = 100) : TThisBaseGraph;
var
   i,j       : integer;
   x,y       : float64;
   Line      : AnsiString;
   DefFilt   : byte;
   Data,
   FilesWanted : tStringList;
   rfile : file;
   v     : tGraphPoint32;
begin
   {$IfDef AllowUSNAdataDownloads} DownloadandUnzipDataFileIfNotPresent('spectral_library'); {$EndIf}
   Result := Nil;
   FilesWanted := tStringList.Create;
   if (fName = '') then begin
      FilesWanted.Add(MainMapData  + 'spectral_library\');
      DefFilt:= 0;
      if not GetMultipleFiles('reflectance spectra','Spectral libary|*.SPC;*.ASC;*.txt',FilesWanted,DefFilt) then exit;
   end
   else FilesWanted.Add(FName);

   Result := tThisBaseGraph.Create(Application);
   Result.GraphDraw.MinHorizAxis := 200;
   Result.GraphDraw.MaxHorizAxis := 3000;
   Result.GraphDraw.MaxVertAxis := MaxVert;
   Result.GraphDraw.HorizLabel := 'Wavelength (microns)';
   Result.GraphDraw.VertLabel := 'Albedo (%)';
   Result.GraphDraw.SatBands := 'TM8';
   Result.Caption := 'Reflectance Spectrum';
   Result.SetUpGraphForm;
   Result.GraphDraw.LegendList := tStringList.Create;
   for i := 0 to pred(FilesWanted.Count) do begin
      fName := FilesWanted.Strings[i];
      Data := tStringList.Create;
      Data.LoadFromFile(fName);
      {$IfDef RecordSpectralLibraryGraph} WriteLineToDebugFile('Spectral Library file: ' + fName,true); WriteStringListToDebugFile(Data); {$EndIf}

      fName := ExtractFileNameNoExt(FilesWanted.Strings[i]);
      Result.Caption := Result.Caption + '  ' + fName;
      Result.GraphDraw.LegendList.Add(fName);
      Result.OpenDataFile(rfile);
      for j := 16 to pred(Data.Count) do begin
         Line := ptTrim(Data.Strings[j]);
         x := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(Line,' ',true,true));
         y := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(Line,' ',true,true));
         if (abs(y) > 1e-30) then begin
            v[1] := 1000 * x;
            v[2] := 100 * y;
            BlockWrite(rfile,v,1);
         end;
      end;
      Data.Free;
      CloseFile(rfile);
   end;
   Result.RedrawDiagram11Click(Nil);
end;

{$IfDef NoDBGrafs}
{$Else}

      function tSatImage.GraphHistogram(Desired : integer; WhichBand : integer) : TThisBaseGraph;
      var
         Band : integer;
         rfile      : file;
         MaxCount : float32;
         v          : array[1..2] of float32;


               procedure AddBand(Band : integer);
               var
                  J : integer;
                  Factor : float32;
               begin
                    Factor := BandXSpace[1] / BandXSpace[Band] * BandYSpace[1] / BandYSpace[Band];
                    Result.GraphDraw.LegendList.Add(BandLongName[Band]);
                    Result.OpenDataFile(rfile);
                    for j := 0 to MaxRefCount do begin
                       if Distrib[Band]^[j] > 0 then begin
                          v[1] := j;
                          v[2] := Distrib[Band]^[j] / Factor;
                          if ((LandsatNumber <> 0) and (Band = 9)) or (IsSentinel2 and (Band = 11)) then begin
                          end
                          else if (v[2] > MaxCount) then MaxCount := v[2];
                          BlockWrite(rfile,v,1);
                       end;
                    end;
                    CloseFile(rfile);
               end;


      begin
         {$IfDef RecordHistogram} WriteLineToDebugFile('tSatImage.GraphHistogram enter ' + OriginalFileName); {$EndIf}
         InitializeHistogramDistributions;
            MaxCount := 0;
            LoadHistogram;
            {$IfDef RecordHistogram} WriteLineToDebugFile('MaxCount found'); {$EndIf}

            Result := TThisBaseGraph.Create(Application);
            Result.GraphDraw.LegendList := tStringList.Create;
            with Result,GraphDraw do begin
               if Desired in [shAllBands,shRGBBands] then begin
                  HorizLabel := 'Band Reflectance';
                  Result.Caption := SceneTitle + ' Reflectance Histograms';
               end
               else begin
                  HorizLabel := BandLongName[WhichBand] + ' Reflectance';
                  Result.Caption := BandLongName[WhichBand] + ' Histogram';
               end;
               VertLabel := 'Points';
               Result.SetUpGraphForm;
               if Desired in [shAllBands,shRGBBands] then begin
                    if Desired in [shAllBands] then begin
                       for Band := 1 to NumBands do AddBand(Band);
                    end
                    else begin
                        AddBand(SelectionMap.MapDraw.SatView.RedBand);
                        AddBand(SelectionMap.MapDraw.SatView.GreenBand);
                        AddBand(SelectionMap.MapDraw.SatView.BlueBand);
                    end;
                    FileColors256[1] := claRed;
                    FileColors256[2] := claLime;
                    FileColors256[3] := claBlue;
                    FileColors256[4] := claFuchsia;
                    FileColors256[5] := claMaroon;
                    FileColors256[6] := claOlive;
                 end
                 else begin
                     AddBand(WhichBand);
                 end;
               {$IfDef RecordHistogram} WriteLineToDebugFile('Graph set up'); {$EndIf}
               MaxHorizAxis := MaxRefCount;
               MaxVertAxis := MaxCount;
               PadAxis(MinVertAxis,MaxVertAxis);
               ForceAxisFit(HorizAxisFunctionType,HorizCycleCuts,NumHorizCycles,MinHorizAxis,MaxHorizAxis,XWindowSize,50);
               ForceAxisFit(VertAxisFunctionType,VertCycleCuts,NumVertCycles,MinVertAxis,MaxVertAxis,YWindowSize,25);
               Result.SetUpGraphForm;
               RedrawDiagram11Click(Nil);
            end {with};
         {$IfDef RecordHistogram} WriteLineToDebugFile('tSatImage.GraphHistogram out'); {$EndIf}
      end;


      function tSatImage.EnhancementGraph : TThisBaseGraph;
      var
         rfile      : file;
         v          : tGraphPoint32;

               procedure AddBand(Band : integer; Colors : tColorIndex);
               var
                  J : integer;
               begin
                    Result.GraphDraw.LegendList.Add(BandLongName[Band]);
                    Result.OpenDataFile(rfile);
                    for j := MinRef[Band] to MaxRef[Band] do begin
                          v[1] := j;
                          v[2] := Colors[j];
                          BlockWrite(rfile,v,1);
                    end;
                    CloseFile(rfile);
               end;

      begin
         Result := TThisBaseGraph.Create(Application);
         Result.ClientWidth := 450;
         Result.ClientHeight := 300;

         with Result,GraphDraw do begin
            HorizLabel := 'Band DN';
            Result.Caption := SceneTitle + ' Enhancement graph';
            VertLabel := 'Ouput intensity';
            Result.SetUpGraphForm;
            Result.GraphDraw.LeftMargin := 80;
            Result.GraphDraw.LegendList := tStringList.Create;

            if SelectionMap.MapDraw.MapType = mtSatImageGray then begin
                AddBand(SelectionMap.MapDraw.SatView.BandInWindow,GraysLookUp);
                FileColors256[1] := claDarkGrey;
            end
            else begin
               AddBand(SelectionMap.MapDraw.SatView.RedBand,ColorLookUp[1]^);
               AddBand(SelectionMap.MapDraw.SatView.GreenBand,ColorLookUp[2]^);
               AddBand(SelectionMap.MapDraw.SatView.BlueBand,ColorLookUp[3]^);
               FileColors256[1] := claRed;
               FileColors256[2] := claLime;
               FileColors256[3] := claBlue;
            end;
            MaxHorizAxis := MaxRefCount;
            MaxVertAxis := 255;
            RedrawDiagram11Click(Nil);
         end;
      end;


      procedure tSatImage.SatHistograms(xlo,ylo,xhi,yhi : integer);
      var
         i,j,k    : integer;

               procedure SetHistogramLimits;
               var
                  i : integer;
               begin
                  {$IfDef RecordHistogram} WriteLineToDebugFile('tSatImage.SetHistogramLimits in'); {$EndIf}
                  for i := 1 to NumBands do begin
                     if MDdef.IgnoreHistogramZero and (not Data16bit) then Distrib[i]^[0] := 0;
                     MinRef[i] := 0;
                     while (Distrib[i]^[MinRef[i]] = 0) and (MinRef[i] < MaxRefCount) do inc(MinRef[i]);
                     MaxRef[i] := MaxRefCount;
                     while (Distrib[i]^[MaxRef[i]] = 0) and (MaxRef[i] > 0) do dec(MaxRef[i]);
                     {$IfDef RecordHistogram} WriteLineToDebugFile('Band ' + IntToStr(i) + '   min=' + IntToStr(MinRef[i]) + '   max=' + IntToStr(MaxRef[i])); {$EndIf}
                  end {for i};
                  {$IfDef RecordHistogram} WriteLineToDebugFile('tSatImage.SetHistogramLimits out'); {$EndIf}
               end;

      begin
         {$IfDef RecordSat} WriteLineToDebugFile('tSatImage.SatHistograms in'); {$EndIf}
         InitializeHistogramDistributions;
         LoadHistogram;

         for i := 1 to NumBands do begin
            {$IfDef RecordSat} WriteLineToDebugFile('Histogram band: ' + IntToStr(i)); {$EndIf}
            StartProgress('Histogram ' + IntToStr(i));
            FillChar(Distrib[i]^,SizeOf(Distrib[i]^),0);
            for j := ylo to yhi do begin
               GetSatRow16bit(i,j,Row16Bit^);
               for k := xlo to xhi do inc(Distrib[i]^[Row16Bit^[k]]);
               if (j mod 50 = 0) then UpDateProgressBar((j-ylo)/(yhi-ylo));
            end;
            SetHistogramLimits;
         end;
         EndProgress;
         {$IfDef RecordSat} WriteLineToDebugFile('tSatImage.SatHistograms out'); {$EndIf}
      end;

{$EndIf}



procedure tSatImage.OptimumIndexFactor;
var
   i,j,k,n,db : integer;
   Correlations,VarCoVar   : ^tTrendMatrix;
   Mean,Min,Max,StdDev : tMaxBandsArray;
   Results : tStringList;
   fName : PathStr;
   OIF : float64;
begin
   New(Correlations);
   New(VarCoVar);
   SatSceneStatistics(n,Correlations^,VarCoVar^,Mean,Min,Max,StdDev);
   Results := tStringList.Create;
   Results.Sorted := true;
   for i := 1 to (NumBands - 2) do begin
      for j := succ(i) to (NumBands - 1) do
         for k := succ(j) to NumBands do if (i <> j) and (j <> k) and (i <> k) then begin
            OIF := (StdDev[i] + StdDev[j] + StdDev[k]) / ( abs(Correlations^[i,j]) + abs(Correlations^[i,k]) + abs(Correlations^[j,k]));
            Results.Add(RealToString(OIF,12,3) + ',' + BandShortName[i] + ',' + BandShortName[j] + ',' + BandShortName[k]);
         end;
   end;
   fName := MDTempDir + 'oif.csv';
   Results.Sorted := false;
   Results.Insert(0,'OIF,BAND_1,BAND_2,BAND_3');
   Results.SaveToFile(fName);
   Results.Free;
   OpenNumberedGISDataBase(db,fName);
   Dispose(Correlations);
   Dispose(VarCoVar);
end;


procedure tSatImage.PickBand(aMessage : ShortString; var WantedBand : integer);
var
   i : integer;
   OK : boolean;
   TheList : TStringList;
begin
   if (NumBands = 1) then WantedBand := 1
   else begin
      if (WantedBand < 1) then WantedBand := 1;
      if (WantedBand > NumBands) then WantedBand := NumBands;
      TheList := TStringList.Create;
      for i := 1 to NumBands do begin
         TheList.Add(BandLongName[i]);
         {$IfDef RecordTMSat} WriteLineToDebugFile('Pick band ' + IntToStr(i) + '  ' + BandShortName[i]); {$EndIf}
      end;
      repeat
         GetFromList('Band for ' +aMessage,WantedBand,TheList);
         Inc(WantedBand);
         OK := UpperCase(Copy(BandLongName[WantedBand],1,7)) <> 'MISSING';
      until OK;
      {$IfDef RecordTMSat} WriteLineToDebugFile('Picked band ' + IntToStr(WantedBand)); {$EndIf}
      TheList.Free;
   end;
end;

procedure tSatImage.PickMultipleBands(aMessage : ShortString; var UseBands : tUseBands);
var
   i : integer;
   TheList : TStringList;
begin
   TheList := TStringList.Create;
   for i := 1 to NumBands do TheList.Add(BandLongName[i]);
   {$IfDef RecordTMSat} WriteLineToDebugFile('tSatImage.PickMultipleBands full list'); WriteStringListToDebugFile(TheList,true); {$EndIf}
   Toggle_db_use.PickSomeFromStringList(TheList,aMessage);
   {$IfDef RecordTMSat} WriteLineToDebugFile('tSatImage.PickMultipleBands picked files'); WriteStringListToDebugFile(TheList,true); {$EndIf}
   for i := 1 to MaxBands do UseBands[i] := false;
   for i := 1 to NumBands do begin
      UseBands[i] := TheList.IndexOf(BandLongName[i]) <> -1;
   end;
   TheList.Free;
end;


function tSatImage.MakeNewBand(NewBand : tNewSatBand; OpenMap : boolean = true) : integer;
var
   NIRBand,RedBand,GreenBand,IR2Band,SWIRBand,
   ThinFactor,
   i,j,k,Band3,Band1,Band2  : integer;
   numvalsf,denvalsf,thirdvalsf,Emissivity,
   Ratio,FirstFactor,SecondFactor          : float64;
   NewBandTitle : shortstring;
   NewHeadRecs : tDEMheader;

      function RatName : ANSIstring;
      begin
         Result := ', (' + BandShortName[Band1] + '-' + BandShortName[Band2] + ') / (' + BandShortName[Band1] + '+' + BandShortName[Band2] + ')';
      end;

      procedure PickBands;
      begin
         PickBand('b1 for (b1-b2)/(b1+b2)',Band1);
         PickBand('b2 for (b1-b2)/(b1+b2)',Band2);
      end;

begin
   {$IfDef RecordNewSat} WriteLineToDebugFile('tSatImage.MakeNewBand ' + SceneTitle); {$EndIf}
   Result := 0;
   LoadHistogram;
   Band1 := 1;
   Band2 := 0;
   Band3 := 0;
   ThinFactor := 1;

   NIRBand := 0;
   IR2Band := 0;
   RedBand := 0;
   GreenBand := 0;
   SWIRBand := 0;

   if (LandsatNumber in [4,5,6,7]) or (StrUtils.AnsiContainsText(IndexFileName,'NAIP') and (NumBands = 4)) or Is4band then begin
      NIRBand := 4;
      RedBand := 3;
      GreenBand := 2;
      IR2Band := 5;
      SWIRBand := 7;
   end
   else if (LandsatNumber in [8,9]) then begin
      SWIRBand := 7;
      IR2Band := 6;
      NIRBand := 5;
      RedBand := 4;
      GreenBand := 3;
   end
   else if (SatelliteName = 'Sentinel-2') then begin
      SWIRBand := 13;  //Band 12
      IR2Band := 12;  //Band 11
      if (NewBand = nsbNBRNormalizedburnindex) then NIRBand := 9  //8A
      else NIRBand := 8;
      RedBand := 4;
      GreenBand := 3;
   end
   else if SatelliteName = 'PlanetScope8' then begin
      NIRBand := 8;
      RedBand := 6;
      GreenBand := 4;
   end
   else if WV2Image then begin
      NIRBand := 8;
      RedBand := 5;
      GreenBand := 4;
   end;

   if (NewBand = nsbAddimages) then begin
      FirstFactor := 0.5;
      SecondFactor := 0.5;
      PickBand('first band',Band1);
      repeat
         ReadDefault('first band factor',FirstFactor);
      until abs(FirstFactor) > 0.01;
      PickBand('second band',Band2);
      repeat
         ReadDefault('second band factor',SecondFactor);
      until abs(SecondFactor) > 0.01;
      NewBandTitle := 'Sum ' + BandShortName[Band1] + ' and ' + BandShortName[Band2];
   end
   else if (NewBand = nsbNDVI) then begin
      Band1 := NIRBand;
      Band2 := RedBand;
      if (Band1 = 0) or (Band2 = 0) then PickBands;
      NewBandTitle := 'NDVI' + RatName;
   end
   else if (NewBand = nsbGDVI) then begin
      Band1 := NIRBand;
      Band2 := GreenBand;

      if (Band1 = 0) or (Band2 = 0) then PickBands;
      NewBandTitle := 'GDVI' + RatName;
   end
   else if (NewBand = nsbPickEm) then begin
      PickBands;
      NewBandTitle := RatName;
   end
   else if (NewBand = nsbNDSIsoil) or (NewBand = nsbNDBIbuilding) {Dozier, 1989, Remote Sensing Environment} then begin
      Band1 := IR2Band;
      Band2 := NIRBand;
      if (Band1 = 0) or (Band2 = 0) then PickBands;
      if (NewBand = nsbNDSIsoil) then NewBandTitle := 'NDSI (soil)' + RatName
      else NewBandTitle := 'NDBI (building)' + RatName;
   end
   else if (NewBand = nsbNDSIsnow) then begin  //Dozier, 1989, Remote Sensing Environment
      Band1 := RedBand;
      Band2 := IR2Band;
      if (Band1 = 0) or (Band2 = 0) then PickBands;
      NewBandTitle := 'NDSI (snow)' + RatName;
   end
   else if (NewBand = nsbNDWI) then begin
      Band1 := GreenBand;
      Band2 := NIRBand;
      if (Band1 = 0) or (Band2 = 0) then PickBands;
      NewBandTitle := 'NDWI' + RatName;
   end
   else if (NewBand in [nsbNBRNormalizedburnindex]) then begin
      Band1 := NIRBand;
      Band2 := SWIRBand;
      if (Band1 = 0) or (Band2 = 0) then PickBands;
      NewBandTitle := 'NBR' + RatName;
   end
   else if (NewBand in [nsbNBRNormalizedburnindex2]) then begin
      Band1 := IR2Band;
      Band2 := SWIRBand;
      if (Band1 = 0) or (Band2 = 0) then PickBands;
      NewBandTitle := 'NBR-2' + RatName;
   end
   else if (NewBand = nsbSentinelReflectance) then begin
      PickBand('band for Sentinel reflectance',Band1);
      NewBandTitle := 'TOA_reflectance_sun_angle_' + BandShortName[Band1];
   end
   else if (NewBand = nsbTOARefSolar) then begin
      PickBand('band for TOA reflectance with terrain effects',Band1);
      NewBandTitle := 'TOA_reflectance_sun_angle_' + BandShortName[Band1];
   end
   else if (NewBand = nsbTOARef) then begin
      PickBand('band for TOA reflectance',Band1);
      NewBandTitle := 'TOA_reflectance_' + BandShortName[Band1];
   end
   else if (NewBand = nsbTOARadiance) then begin
      PickBand('band for surface radiance',Band1);
      NewBandTitle := 'TOA_radiance_' + BandShortName[Band1];
   end
   else if (NewBand in [nsbBrightness]) then begin
      PickBand('thermal band for brightness temperature',Band1);
      NewBandTitle := 'brightness_temperature_' + BandShortName[Band1];
   end
   else if (NewBand in [nsbLST]) then begin
      PickBand('thermal band for land surface temperature',Band1);
      Emissivity := 0.984;
      ReadDefault('Emissivity',Emissivity);
      NewBandTitle := 'LST_emissivity_' + RealToString(Emissivity,-5,3) + '_' + BandShortName[Band1];
   end
   else if (NewBand = nsbVARI) or (NewBand = nsbGrayscale) then begin
      if (LandsatNumber in [4,5,6,7]) then begin
         Band1 := 2;  //green
         Band2 := 1;  //blue
         Band3 := 3;  //red
      end
      else if (LandsatNumber in [8]) then begin
         Band1 := 3;  //green
         Band2 := 2;  //blue
         Band3 := 4;  //red
      end
      else if WV2Image then begin
         Band1 := 3;  //green
         Band2 := 2;  //blue
         Band3 := 5;  //red
      end
      else begin
         PickBand('red band',Band3);
         PickBand('green band',Band1);
         PickBand('blue band',Band2);
      end;
      if (NewBand = nsbVARI) then NewBandTitle := 'VARI'
      else NewBandTitle := 'RGB grayscale';
   end
   else if (NewBand = nsbRatio) then begin
      PickBand('ratio numerator',Band1);
      PickBand('ratio denominator',Band2);
      NewBandTitle := 'Ratio ' + BandShortName[Band1] + ' over ' + BandShortName[Band2];;
   end;
   if (Band2 <> 0) and (BandXSpace[Band1] <> BandXSpace[Band2]) then begin
      ThinFactor := round(BandXSpace[Band1]/BandXSpace[Band2]);
      if not (NewBand = nsbNBRNormalizedburnindex) then begin
         if Not AnswerIsYes('Use thinning factor of ' + IntToStr(ThinFactor) + ' on bands with different spatial resolution, band1=' +
             RealToString(BandXSpace[Band1],-8,-2) + ' band2=' + RealToString(BandXSpace[Band2],-8,-2)) then exit;
      end;
   end;
   ShowHourglassCursor;

   ZeroDEMHeader(NewHeadRecs,true);
   NewHeadRecs.NumCol := BandColumns[Band1];
   NewHeadRecs.NumRow := BandRows[Band1];

   NewHeadRecs.DEMSWCornerX := SatelliteBoundBoxProj(Band1).xmin;
   NewHeadRecs.DEMSWCornerY := SatelliteBoundBoxProj(Band1).ymin;

   NewHeadRecs.DEMxSpacing := BandXSpace[Band1];
   NewHeadRecs.DEMySpacing := BandYSpace[Band1];
   NewHeadRecs.UTMZone := ImageMapProjection.projUTMZone;
   NewHeadRecs.LatHemi := ImageMapProjection.LatHemi;
   OpenAndZeroNewDEM(true,NewHeadRecs,Result,NewBandTitle,InitDEMmissing);

   if (NewBand = nsbNDVI) then DEMGlb[Result].DEMheader.ElevUnits := euNDVI
   else if (NewBand = nsbNBRNormalizedburnindex) then DEMGlb[Result].DEMheader.ElevUnits := euNBR
   else DEMGlb[Result].DEMheader.ElevUnits := Undefined;

   {$IfDef RecordNewSat} WriteLineToDebugFile('band1=' + IntToStr(Band1) + '  band2=' + IntToStr(Band2) + '  band3=' + IntToStr(Band3)); {$EndIf}

   StartProgress('Calculate ' + NewBandTitle);
   for i := 0 to pred(DEMGlb[Result].DEMheader.NumRow) do begin
      if (i mod 10 = 0) then UpdateProgressBar( i / DEMGlb[Result].DEMheader.NumRow);
      GetSatRow16bit(Band1,i,SatRow[1]^);
      if (Band2 <> 0) then GetSatRow16bit(Band2,i * ThinFactor,SatRow[2]^);
      if (Band3 <> 0) then GetSatRow16bit(Band3,i * ThinFactor,SatRow[3]^);
      for k := 0 to pred(DEMGlb[Result].DEMheader.NumCol) do begin
        j := k * ThinFactor;
        if MDdef.IgnoreHistogramZero and ((SatRow[1]^[k] = 0) or  ( (Band2 <> 0) and (SatRow[2]^[j] = 0)) or ((Band3 <> 0) and (SatRow[3]^[j] = 0)) ) then begin
           DEMGlb[Result].SetGridMissing(k,DEMGlb[Result].DEMheader.NumRow-i);
        end
        else begin
           numvalsf := ConvertDN(SatRow[1]^[k],Band1);
           if (Band2 <> 0) then denvalsf  := ConvertDN(SatRow[2]^[j],Band2);
           if (Band3 <> 0) then ThirdValsf := ConvertDN(SatRow[3]^[j],Band3);
           if (NewBand in [nsbPickEm,nsbNDVI, nsbNDSIsoil, nsbNDSIsnow, nsbNDWI, nsbVARI,nsbNBRNormalizedburnindex,nsbNBRNormalizedburnindex2,nsbNDBIbuilding,nsbGDVI]) then begin
              if (NumValsf + DenValsf = 0) then Ratio := 0
              else begin
                 Ratio := (NumValsf - DenValsf) / (NumValsf + DenValsf);
              end;
           end
           else if (NewBand = nsbVARI) then begin
              Ratio := (NumValsf - ThirdValsf) / (NumValsf + ThirdValsf - DenValsf);
           end
           else if (NewBand = nsbAddimages) then begin
              Ratio := FirstFactor * NumValsf + SecondFactor * DenValsf;
           end
           else if (NewBand = nsbRatio) then begin
              Ratio := ArcTan(NumValsf / DenValsf);
           end
           else if (NewBand = nsbSentinelReflectance) then begin
              Ratio := SatRow[1]^[k] / 10000;
           end
           else if (NewBand in [nsbTOARefSolar,nsbTOARef,nsbTOARadiance,nsbBrightness]) then begin
              Ratio := NumValsf;
           end
           else if (NewBand in [nsbLST]) then begin
              Ratio := NumValsf / ( 1 + BandWavelengths[Band1] * 1e-6 * NumValsf / 1.4388e-2 * system.ln(Emissivity));
           end;
           DEMGlb[Result].SetGridElevation(k,DEMGlb[Result].DEMheader.NumRow-i,Ratio);
         end;
      end {for j};
      {$IfDef RecordBandMod} WriteLineToDebugFile('   ' +  IntegerToString(RatVals^[1],5) +  IntegerToString(RatVals^[2],5) + IntegerToString(RatVals^[3],5) + IntegerToString(NumVals^[4],5) ); {$EndIf}
   end {for i};

   EndProgress;

   if (LandsatNumber in [1..9]) then DEMGlb[Result].AreaName := NewBandTitle + '_' + ShortLandsatName(SceneBaseName)
   else if IsSentinel2 then DEMGlb[Result].AreaName := NewBandTitle + '_' + ImageDateString + '_Sentinel-2'
   else DEMGlb[Result].AreaName := NewBandTitle + ' ' + SceneBaseName;
   DEMGlb[Result].CheckMaxMinElev;
   {$IfDef RecordNewSat} WriteLineToDebugFile('Band range: ' + DEMGlb[Result].zRange); {$EndIf}

   {$IfDef VCL}
      if OpenMap then begin
         CreateDEMSelectionMap(Result,true,true,mtDEMBlank);
      end;
   {$EndIf}
    WmDem.SetMenusForVersion;
end;

{$EndIf}


function tSatImage.RegInfo : AnsiString;
begin
    case RegVars.Registration of
       RegTIN      : Result := 'Delauney TIN registration';
       RegNone     : Result := 'No registration';
       RegProjection : Result := 'Data projection: ' + ImageMapProjection.GetProjectionName;
    end;
end;

function tSatImage.ImageKeyInfo : AnsiString;
begin
    Result := 'Image pixel size: ' + RealToString(MetersPerPixel,-8,2) + ' m' + MessLineBreak +
         'Image size (pixels): ' + IntToStr(NumSatCol) + 'x' + IntToStr(NumSatRow)  + MessLineBreak +
         'Bands in image: ' + IntToStr(NumBands) + MessLineBreak +
         'Image native datum: ' + DatumName(ImageMapProjection.h_DatumCode);
    if CanEnhance then Result := Result + MessLineBreak + 'Image contrast can be enhanced'
    else Result := Result + MessLineBreak + 'Image has color palette and cannot be enhanced';
    if RegVars.Registration in [RegProjection] then begin
       Result := Result +  MessLineBreak + 'Satellite ' + RegInfo +  MessLineBreak + 'Upper left corner, x=' + RealToString(RegVars.UpLeftX,-18,-6) + '  y=' + RealToString(RegVars.UpLeftY,-18,-6) +
           MessLineBreak + 'Image spacing, dx=' + RealToString(BandXSpace[DefRedTrue],-18,-6) + '  dy=' + RealToString(BandYSpace[DefRedTrue],-18,-6);
    end;
end;


function tSatImage.BandAxisValue(Band : integer) : float64;
begin
    if MDDef.BandsByWavelength and (BandWavelengths[Band] > -9) then Result := BandWavelengths[Band]
    else Result := Band;
end;


procedure tSatImage.AssignClusterColors(ShowClasses : integer; var Colors : tColors256);
var
   i : integer;
   fName : PathStr;
   Table : tMyData;
begin
    for i := 0 to 255 do Colors[i] := clWhite;
    fName := BFileName[ShowClasses] + '.vat.dbf';
    if FileExists(fName) then begin
        Table := tMyData.Create(fName);
        while not Table.eof do begin
           if Table.GetFieldByNameAsString('USE') = 'Y' then begin
              i := Table.GetFieldByNameAsInteger('VALUE');
              Colors[i] := Table.GetFieldByNameAsInteger('COLOR');
           end;
           Table.Next;
        end;
        Table.Destroy;
    end;
end;


procedure tSatImage.SatSceneStatistics(var n : integer; var Correlations,VarCoVar : tTrendMatrix; var Mean,Min,Max,StdDev : tMaxBandsArray);
var
   i,j,Row,Col,Band : integer;
   Vals : array[1..MaxBands] of ^tImageRow;
   Sum : tMaxBandsArray;
   SP : ^tTrendMatrix;
begin
   n := 0;
   New(SP);
   for i := 1 to NumBands do begin
      GetMem(Vals[i],BandColumns[i]*SatRecSize);
      for j := 1 to NumBands do SP^[i,j] := 0;
      Sum[i] := 0;
      Min[i] := 99999;
      Max[i] := -9999;
   end;
   StartProgress('Correlation');
   for Row := 0 to pred(NumSatRow) do begin
      if (Row mod 25 = 0) then UpDateProgressBar( Row / NumSatRow);
      for Band := 1 to NumBands do GetSatRow(Band,Row,Vals[Band]^);
      for Col := 0 to pred(NumSatCol) do begin
         if (Vals[1]^[Col] <> 0) or (not MDdef.IgnoreHistogramZero) then begin
            inc(n);
            for Band := 1 to NumBands do begin
               Petmath.CompareValueToExtremes(Vals[Band]^[Col],Min[Band],Max[Band]);
               Sum[Band] := Sum[Band] + Vals[Band]^[Col];
               for i := 1 to NumBands do SP^[Band,i] := SP^[Band,i] + 1.0 * Vals[i]^[Col] * Vals[Band]^[Col];
            end {for Band};
         end;
      end {for Col};
   end {for Row};
   for i := 1 to NumBands do FreeMem(Vals[i],NumSatCol*SatRecSize);
   EndProgress;

   for i := 1 to NumBands do begin
      Mean[i] := (Sum[i] / N);
      StdDev[i] := sqrt( ((SP^[i,i] * N) - (Sum[i] * Sum[i])) / N / (N-1));
   end {for i};

   for Band := 1 to NumBands do begin
      for i := 1 to NumBands do begin
         Correlations[Band,i] := (SP^[Band,i] - (Sum[i] * Sum[Band] / N) ) / pred(N) / StdDev[i] / StdDev[Band];
         VarCovar[Band,i] := (SP^[Band,i] - (Sum[i] * Sum[Band] / N) ) / pred(N);
      end;
   end;
   Dispose(SP);
end;


function tSatImage.FindTMBand(TMBandName : integer; var BandPresent : integer) : boolean;
var
  i : integer;
begin
   for i := 1 to NumBands do begin
      {$IfDef RecordFindTMBand} WriteLineToDebugFile(' check: ' + Copy(Bandtitle[i],1,6)); {$EndIf}
      if ('BAND ' + IntToStr(TMBandName)) = UpperCase(Copy(BandLongName[i],1,6)) then begin
         BandPresent := i;
         Result := true;
         exit;
      end;
   end;
   Result := false;
end;


function tSatImage.LatLongDegreeInDataSet(Lat,Long : float64) : boolean;
var
   xg,yg : float32;
begin
   LatLongDegreeToSatGrid(DefRedTrue,Lat,Long,xg,yg);
   Result := SatGridInDataSet(DefRedTrue,XG,YG);
end;


function tSatImage.SatGridInDataSet(Band : integer; XGrid,YGrid : float64) : boolean;
begin
   Result := (Xgrid >= 0) and (Xgrid < BandColumns[Band]) and (Ygrid >= 0) and (Ygrid < BandRows[Band]);
end;


procedure tSatImage.SetUpColors(MapDraw : tMapDraw; SatView : tSatView);
var
   i,j : integer;

      procedure SetByteLookUpTable(SatView : tSatView; Band : integer;  var SetColorChoices : tColorIndex);
      var
         i,MinUsed,MaxUsed,Limit : integer;
         Tot                     : LongInt;
      begin
         {$If Defined(RecordSatColor) or Defined(RecordByteLookup)} WriteLineToDebugFile('SetByteLookUpTable, band=' + IntToStr(Band) + ' contrast ' + ContrastName(SatView) +  '  ' + BandRange(Band)); {$EndIf}
         MinUsed := MinRef[Band];
         MaxUsed := MaxRef[Band];
         if (SatView.WindowContrast = NoEnhancement) or (not CanEnhance) then begin
            if (MaxUsed < 256) or (not CanEnhance) then begin
               for i := 0 to MaxUsed do SetColorChoices[i] := i;
            end
            else begin
               for i := MinUsed to MaxUsed do SetColorChoices[i] := round(255.0 * (i - MinUsed) / (MaxUsed - MinUsed));
            end;
         end
         else if SatView.WindowContrast in [StraightLinearStretch,TailLinearStretch,CloudOnlyTailStretch,DefinedLinearStretch] then begin
            if SatView.WindowContrast in [TailLinearStretch,CloudOnlyTailStretch] then begin
               {ignore % fringe for low reflectances, not used for cloud only}
               if SatView.WindowContrast in [TailLinearStretch] then begin
                  Tot := 0;
                  Limit := round(0.01 * NumPoints[Band] * SatView.WindowContrastLowTailSize);
                  while (Tot < Limit) do begin
                     inc(Tot,Distrib[band]^[MinUsed]);
                     inc(MinUsed);
                  end;
               end;

               {ignore % fringe for high reflectances}
               Tot := 0;
               Limit := round(0.01 * NumPoints[Band] * SatView.WindowContrastHighTailSize);
               while (Tot < Limit) do begin
                  inc(Tot,Distrib[band]^[MaxUsed]);
                  dec(MaxUsed);
               end;
            end
            else if SatView.WindowContrast in [DefinedLinearStretch] then begin
               MinUsed := MDDef.MinSatRange;
               MaxUsed := MDDef.MaxSatRange;
            end;

            if (MinUsed = MaxUsed) then begin
               for i := 0 to 255 do SetColorChoices[i] := 127;
            end
            else begin
               for i := MinUsed to MaxUsed do SetColorChoices[i] := round(255.0 * (i - MinUsed) / (MaxUsed - MinUsed));
            end;
         end
         else if (SatView.WindowContrast = MaskRange) then begin
            for i := 0 to 255 do SetColorChoices[i] := 0;
            for i := MDDef.MinSatRange to MDDef.MaxSatRange do SetColorChoices[i] := 255;
         end
         else if (SatView.WindowContrast = HistogramEqualization) then begin
            Tot := 0;
            for i := MinRef[Band] to MaxRef[Band] do begin
               SetColorChoices[i] := round(255.0 * (Tot + Distrib[band]^[i]) / NumPoints[Band]);
               inc(Tot,Distrib[Band]^[i]);
            end;
         end;
         {$If Defined(RecordSatColor) or Defined(RecordByteLookup)}WriteLineToDebugFile('After tails, MinUsed=' + IntToStr(MinUsed) + ' MaxUsed=' + IntToStr(MaxUsed)); {$EndIf}
         for i := 0 to pred(MinUsed) do SetColorChoices[i] := 0;
         for i := succ(MaxUsed) to MaxRefCount do SetColorChoices[i] := 255;
      end;

         procedure SetRGBLookupArray(BandInWindow : integer);
         var
            i : integer;
            fName : PathStr;
            Table : tMyData;
            Color : tColor;
         begin
            {$IfDef RecordSatColor} WriteLineToDebugFile('tSatImage.SetRGBLookupArray in'); {$EndIf}
            fName := BFileName[BandInWindow] + '.vat.dbf';
            if FileExists(fName) then begin
               {$IfDef RecordSatColor} WriteLineToDebugFile('SetRGBLookupArray use vat.dbf'); {$EndIf}
               for i := 0 to 255 do Grays[i] := GrayRGBtrip(255);
               Table := tMyData.Create(fName);
               while not Table.eof do begin
                  if Table.GetFieldByNameAsString('USE') = 'Y' then begin
                     i := Table.GetFieldByNameAsInteger('VALUE');
                     Color := Table.GetFieldByNameAsInteger('COLOR');
                     Grays[i] := ConvertTColorToPlatformColor(Color);
                  end;
                  Table.Next;
               end;
               Table.Destroy;
            end
            else if (DefinedImageColors = Nil) then begin
               {$IfDef FMX}
                  {$IfDef RecordSatColor} WriteLineToDebugFile('SetRGBLookupArray use DefinedImageColors'); {$EndIf}
                  for i := 0 to 255 do Grays[i] := GrayRGBtrip(i);
               {$Else}
                  if (Distrib[BandInWindow] = Nil) then begin
                     {$IfDef RecordSatColor} WriteLineToDebugFile('SetRGBLookupArray use Distrib'); {$EndIf}
                     for i := 0 to 255 do Grays[i] := GrayRGBtrip(i);
                  end
                  else begin
                    {$IfDef RecordSatColor} WriteLineToDebugFile('SetRGBLookupArray use SetByteLookUpTable'); {$EndIf}
                     SetByteLookUpTable(SatView,BandInWindow,GraysLookUp);
                     for i := 0 to MaxRefCount do begin
                        Grays[i] := GrayRGBtrip(GraysLookUp[i]);
                     end;
                     {$IfDef RecordSatColor}
                        WriteLineToDebugFile('GrayLookUp');
                        i := MinRef[BandInWindow];
                        while (i < MaxRef[BandInWindow]) do begin
                            if (GraysLookUp[i] > 0) and (GraysLookUp[i] < 255) then WriteLineToDebugFile(IntegerToString(i,8) + IntegerToString(GraysLookUp[i],8) );
                            inc(i,20);
                        end;
                     {$EndIf}
                  end;
               {$EndIf}
            end
            else begin
               for i := 0 to 255 do begin
                  Grays[i] := DefinedImageColors^[i];
               end;
            end;
         end;


begin
   {$IfDef NoBMPFileImagery} {$Else} if BMPFile then exit; {$EndIf}
   {$IfDef RecordSatColor} WriteLineToDebugFile('tSatImage.SetUpColors in, maptype=' + MapTypeName(MapDraw.MapType) + '  data grid: ' +  sfBoundBoxToString(MapDraw.MapCorners.BoundBoxDataGrid,1) );{$EndIf}
   for i := 1 to 3 do if (ColorLookUp[i] = Nil) then New(ColorLookUp[i]);
   if (MapDraw.MapType = mtUnenhancedRGB)  then begin
      for i := 1 to 3 do begin
         for j := 0 to 255 do ColorLookUp[i]^[j] := j;
      end;
   end
   else if (CanEnhance) or ((TIFFImage[1] <> Nil) and (TIFFImage[1].TiffHeader.PhotometricInterpretation = 2) ) then begin
       if IsSatelliteColorImage(MapDraw.MapType) then begin
         {$If Defined(RecordSatColor) or Defined(RecordKeyDraw)}
            if CanEnhance then begin
               WriteLineToDebugFile('Set multiband colors');
               WriteLineToDebugFile('  Red=' + IntToStr(SatView.RedBand) + '   ' + IntToStr(MinRef[SatView.RedBand]) + ' to ' + IntToStr(MaxRef[SatView.RedBand]));
               WriteLineToDebugFile('  Green=' + IntToStr(SatView.GreenBand)+ '   ' + IntToStr(MinRef[SatView.GreenBand]) + ' to ' + IntToStr(MaxRef[SatView.GreenBand]));
               WriteLineToDebugFile('  Blue=' + IntToStr(SatView.BlueBand)+ '   ' + IntToStr(MinRef[SatView.BlueBand]) + ' to ' + IntToStr(MaxRef[SatView.BlueBand]));
            end;
         {$EndIf}
         {$IfDef FMX}
         {$Else}
            if CanEnhance then begin
               SetByteLookUpTable(SatView,SatView.RedBand,ColorLookUp[1]^);
               SetByteLookUpTable(SatView,SatView.GreenBand,ColorLookUp[2]^);
               SetByteLookUpTable(SatView,SatView.BlueBand,ColorLookUp[3]^);

               {$IfDef RecordSatColor}
                  WriteLineToDebugFile('ColorLookUp');
                  WriteLineToDebugFile('DN         Band ' + IntToStr(SatView.RedBand) + '  Band ' + IntToStr(SatView.GreenBand) + '  Band ' + IntToStr(SatView.BlueBand) );
                  MinR := MinRef[SatView.RedBand];
                  MaxR := MaxRef[SatView.RedBand];
                  if MinRef[SatView.GreenBand] < MinR then MinR := MinRef[SatView.GreenBand];
                  if MaxRef[SatView.GreenBand] > MaxR then MaxR := MaxRef[SatView.GreenBand];
                  if MinRef[SatView.BlueBand] < MinR then MinR := MinRef[SatView.BlueBand];
                  if MaxRef[SatView.BlueBand] > MaxR then MaxR := MaxRef[SatView.BlueBand];
                  i := MinR;
                  while (i <= MaxR) do begin
                      if (ColorLookUp[1]^[i] < 255)  and (ColorLookUp[2]^[i] < 255) and (ColorLookUp[3]^[i] < 255) then
                         WriteLineToDebugFile(IntegerToString(i,8) + IntegerToString(ColorLookUp[1]^[i],8) + IntegerToString(ColorLookUp[2]^[i],8) + IntegerToString(ColorLookUp[3]^[i],8));
                      inc(i,(MaxR-MinR) div 20);
                  end;
               {$EndIf}
            end;
            if SatView.PanSharpenImage then SetRGBLookupArray(8);
         {$EndIf}
      end
      else begin
         {$IfDef RecordSatColor} WriteLineToDebugFile('Set grayscale colors, band=' + IntToStr(SatView.BandInWindow)); {$EndIf}
         SetRGBLookupArray(SatView.BandInWindow);
      end;
   end
   else begin
      if (DefinedImageColors <> Nil) then begin
         for i := 0 to 255 do begin
            Grays[i] := DefinedImageColors^[i];
         end;
         {$IfDef RecordSatColor}
            WriteLineToDebugFile('Use DefinedImageColors^');
            for i := 0 to 255 do WriteLineToDebugFile(IntToStr(i) + '  ' + RGBstring(Grays[i].rgbtRed,Grays[i].rgbtGreen,Grays[i].rgbtBlue) );
        {$EndIf}
      end;
   end;
end;

procedure tSatImage.CloseTiffImages(SatView : tSatView);

   procedure DoBand(Band : integer);
   begin
      if (Band <> 0) and (TiffImage[Band] <> nil) then TiffImage[Band].CloseTiffFile;
   end;

begin
   DoBand(SatView.RedBand);
   DoBand(SatView.BlueBand);
   DoBand(SatView.GreenBand);
   DoBand(8);
end;


procedure tSatImage.DisplayImage(SatView : tSatView; MapDraw : tMapDraw; Bitmap : tMyBitmap);
var
   DiffCols,DiffRows,DiffXOffset,DiffYOffset : array[1..4] of int32;


      {$IfDef NoBMPFileImagery}
      {$Else}
         procedure DoBMPimage;
         var
            Bitmap2,SmallBitMap : tMyBitmap;
         begin
            {$If Defined(RecordDrawSatOnMap) or Defined(RecordKeyDraw)} WriteLineToDebugFile('BMP file displaying'); {$EndIf}
            SmallBitmap := tMyBitmap.Create;
            SmallBitmap.LoadFromFile(BMPFileName);
            if (MapDraw.ColsDisplayed = NumSatCol) and (MapDraw.RowsDisplayed = NumSatRow) then
               Bitmap.Canvas.StretchDraw(Rect(0,0,Bitmap.Width,Bitmap.Height),SmallBitmap)
            else begin
               try
                  {$IfDef RecordDrawSatOnMap} WriteLineToDebugFile('Small Bitmap size: ' + BitmapSize(SmallBitmap) + '   Bitmap size: ' + BitmapSize(Bitmap)); {$EndIf}
                  CreateBitmap(Bitmap2,MapDraw.ColsDisplayed,MapDraw.RowsDisplayed);
                  Bitmap2.Canvas.CopyRect(Rect(0,0,Bitmap2.Width,Bitmap2.Height),SmallBitmap.Canvas,
                      Rect(round(MapDraw.MapCorners.BoundBoxDataGrid.xmin),round(MapDraw.MapCorners.BoundBoxDataGrid.ymin),Round(MapDraw.MapCorners.BoundBoxDataGrid.xmax),round(MapDraw.MapCorners.BoundBoxDataGrid.ymax)));
                  Bitmap.Canvas.StretchDraw(Rect(0,0,Bitmap.Width,Bitmap.Height),Bitmap2);
                  Bitmap2.Free;
               finally
               end;
            end;
            SmallBitmap.Free;
         end;
      {$EndIf}


        procedure GetBandParameters(Band,ColorBand : integer);
        begin
            DiffXOffset[Band] := round(MapDraw.MapCorners.BoundBoxDataGrid.xmin * BandXSpace[BandForSize]  / BandXSpace[ColorBand]);
            DiffYOffset[Band] := round(MapDraw.MapCorners.BoundBoxDataGrid.ymin * BandXSpace[BandForSize]  / BandXSpace[ColorBand]);
            DiffCols[Band] := round(MapDraw.ColsDisplayed * BandXSpace[BandForSize]  / BandXSpace[ColorBand]);
            DiffRows[Band] := round(MapDraw.RowsDisplayed * BandXSpace[BandForSize]  / BandXSpace[ColorBand]);
        end;


      procedure ColorAndDifferentSpacing;
      var
         x,y,j1,j2,j3 : integer;
         BMPMem : tBMPMemory;
      begin
         {$If Defined(RecordDrawSatOnMap) or Defined(RecordKeyDraw)} WriteLineToDebugFile('Sat display start loop different spacing data grid: ' +  sfBoundBoxToString(MapDraw.MapCorners.BoundBoxDataGrid,1)); {$EndIf}
          BMPMem := tBMPMemory.Create(Bitmap);
          for y := 0 to pred(Bitmap.Height) do begin
            if ShowSatProgress and (y mod 100 = 0) then UpdateProgressBar(y/Bitmap.Height);
            GetSatRow16bit(SatView.RedBand,DiffYOffset[1] + round(y/pred(Bitmap.Height)*pred(DiffRows[1])),SatRow[1]^);
            GetSatRow16bit(SatView.GreenBand,DiffYOffset[2] + round(y/pred(Bitmap.Height)*pred(DiffRows[2])),SatRow[2]^);
            GetSatRow16bit(SatView.BlueBand,DiffYOffset[3] + round(y/pred(Bitmap.Height)*pred(DiffRows[3])),SatRow[3]^);
            for x := 0 to pred(Bitmap.Width) do begin
               j1 := DiffXOffset[1] + round(x/pred(Bitmap.Width) * pred(DiffCols[1]));
               j2 := DiffXOffset[2] + round(x/pred(Bitmap.Width) * pred(DiffCols[2]));
               j3 := DiffXOffset[3] + round(x/pred(Bitmap.Width) * pred(DiffCols[3]));
               if MDdef.IgnoreHistogramZero and (SatRow[3][j3] = MissingDataValue) then begin
                  BMPMem.SetPixelColor(x,y,MDdef.MissingDataColor);
               end
               else begin
                  BMPMem.SetPixelColor(x,y, RGBtrip(ColorLookUp[1]^[SatRow[1]^[j1]],ColorLookUp[2]^[SatRow[2]^[j2]],ColorLookUp[3]^[SatRow[3]^[j3]]));
               end;
            end;
         end;
         BMPMem.Destroy;
      end;


      procedure DrawPanSharpened(var IntensityResultBitmap : tMyBitmap);
      var
         x,y,j1,j2,j3,j4 : integer;
         H2,S2,l2 : float32;
         BMPMem : tBMPMemory;
      begin
         {$If Defined(RecordDrawSatOnMap) or Defined(RecordKeyDraw)} WriteLineToDebugFile('DrawPanSharpened data grid: ' +  sfBoundBoxToString(MapDraw.MapCorners.BoundBoxDataGrid,1)); {$EndIf}
          BMPMem := tBMPMemory.Create(Bitmap);
          for y := 0 to pred(Bitmap.Height) do begin
            if ShowSatProgress and (y mod 100 = 0) then UpdateProgressBar(y/Bitmap.Height);

            GetSatRow16bit(SatView.RedBand,DiffYOffset[1] + round(y/pred(Bitmap.Height)*pred(DiffRows[1])),SatRow[1]^);
            GetSatRow16bit(SatView.GreenBand,DiffYOffset[2] + round(y/pred(Bitmap.Height)*pred(DiffRows[2])),SatRow[2]^);
            GetSatRow16bit(SatView.BlueBand,DiffYOffset[3] + round(y/pred(Bitmap.Height)*pred(DiffRows[3])),SatRow[3]^);
            GetSatRow16bit(8,DiffYOffset[4] + round(y/pred(Bitmap.Height)*pred(DiffRows[4])),SatRow[4]^);

            for x := 0 to pred(Bitmap.Width) do begin
               j1 := DiffXOffset[1] + round(x/pred(Bitmap.Width) * pred(DiffCols[1]));
               j2 := DiffXOffset[2] + round(x/pred(Bitmap.Width) * pred(DiffCols[2]));
               j3 := DiffXOffset[3] + round(x/pred(Bitmap.Width) * pred(DiffCols[3]));
               j4 := DiffXOffset[4] + round(x/pred(Bitmap.Width) * pred(DiffCols[4]));

               if MDdef.IgnoreHistogramZero and (SatRow[3][j3] = MissingDataValue) then begin
                  BMPMem.SetPixelColor(x,y,MDdef.MissingDataColor);
               end
               else begin
                  HSIfromRGBTrip(RGBtrip(ColorLookUp[1]^[SatRow[1]^[j1]],ColorLookUp[2]^[SatRow[2]^[j2]],ColorLookUp[3]^[SatRow[3]^[j3]]),H2,S2,l2);
                  l2 := (Grays[SatRow[4]^[j4]]).rgbtRed;
                  BMPMem.SetPixelColor(x,y,RGBtripFromHSI(H2,S2,l2));
               end;
            end;
         end;
         BMPMem.Destroy;

         CloseTiffImages(SatView);
      end;


      procedure SingleBandOrColorsSameSpacing(var Bitmap : tMyBitmap);
      var
         x,y,j,Row : integer;
         aRow,TheRow : ^tLongRGB;
         BMPMem : tBMPMemory;
      begin
        {$If Defined(RecordDrawSatOnMap) or Defined(RecordKeyDraw)}WriteLineToDebugFile('Sat display start loop same spacing data grid: ' +  sfBoundBoxToString(MapDraw.MapCorners.BoundBoxDataGrid,1)); {$EndIf}
            BMPMem := tBMPMemory.Create(Bitmap);
            New(TheRow);
            New(aRow);
            if IsSatelliteColorImage(MapDraw.MapType) then begin
               for y := 0 to pred(Bitmap.Height) do begin
                  if ShowSatProgress and (y mod 100 = 0) then UpdateProgressBar(y/pred(Bitmap.Height));
                  Row := round(MapDraw.MapCorners.BoundBoxDataGrid.ymin) + round(y/pred(Bitmap.Height)*pred(MapDraw.RowsDisplayed));
                  if (Row < NumSatRow) then begin
                     if (TiffImage[1] <> nil) and (TIFFImage[1].TiffHeader.PhotometricInterpretation in [1,2]) and (TIFFImage[1].TiffHeader.SamplesPerPixel > 1) then begin
                       TiffImage[1].GetTIFFRowRGB(Row,aRow^);
                        for j := 0 to pred(NumSatCol) do begin
                           {$IfDef FMX}
                              TheRow^[j] := RGBtrip(aRow[j*TiffImage[1].TiffHeader.SamplesPerPixel]+2, aRow[j*TiffImage[1].TiffHeader.SamplesPerPixel]+1,aRow[j*TiffImage[1].TiffHeader.SamplesPerPixel] );
                           {$Else}
                              TheRow^[j].rgbtRed := aRow^[j].rgbtRed;
                              TheRow^[j].rgbtGreen := aRow^[j].rgbtGreen;
                              TheRow^[j].rgbtBlue := aRow^[j].rgbtBlue;
                           {$EndIf}
                        end;
                     end
                     else begin
                        GetSatRow16bit(SatView.RedBand,Row,SatRow[1]^);
                        GetSatRow16bit(SatView.GreenBand,Row,SatRow[2]^);
                        GetSatRow16bit(SatView.BlueBand,Row,SatRow[3]^);
                        for j := 0 to pred(NumSatCol) do begin
                           if MDdef.IgnoreHistogramZero and (SatRow[3][j] = MissingDataValue) then begin
                              TheRow^[j] := MDdef.MissingDataColor;
                           end
                           else begin
                              {$IfDef FMX}
                                 TheRow^[j] := RGBtrip(ColorLookUp[3]^[TRow[3]^[j]],ColorLookUp[2]^[TRow[2]^[j]],ColorLookUp[1]^[TRow[1]^[j]]);
                              {$Else}
                                 TheRow^[j] := RGBtrip(ColorLookUp[1]^[SatRow[1]^[j]],ColorLookUp[2]^[SatRow[2]^[j]],ColorLookUp[3]^[SatRow[3]^[j]]);
                              {$EndIf}
                           end;
                        end;
                     end;
                     for x := 0 to pred(Bitmap.Width) do begin
                        j := round(MapDraw.MapCorners.BoundBoxDataGrid.xmin) + round(x/pred(Bitmap.Width) * pred(MapDraw.ColsDisplayed));
                        if (j >= 0) and (j < NumSatCol) then begin
                           BMPMem.SetPixelColor(x,y,TheRow^[j]);
                        end;
                     end;
                  end;
                 if WantOut then break;
               end;
           end
           else begin
               for y := 0 to pred(Bitmap.Height) do begin
                  if ShowSatProgress and (y mod 100 = 0) then UpdateProgressBar(y/pred(Bitmap.Height));
                  GetSatRow16bit(SatView.BandInWindow, DiffYOffset[1] + round(y/pred(Bitmap.Height)*pred(DiffRows[1])),SatRow[1]^);
                  for x := 0 to pred(Bitmap.Width) do begin
                     j := DiffXOffset[1] + round(x/pred(Bitmap.Width) * pred(DiffCols[1]));
                     if MDdef.IgnoreHistogramZero and (SatRow[1]^[j] = MissingDataValue) then begin
                        TheRow^[j] := MDdef.MissingDataColor;
                     end
                     else begin
                        BMPMem.SetPixelColor(x,y,Grays[SatRow[1]^[j]]);
                     end;
                  end;
               end;
           end;
           CloseTiffImages(SatView);
           Dispose(TheRow);
           Dispose(aRow);
           BMPMem.Destroy;
      end;

var
   TStr : shortstring;
   DiffSpace : boolean;
begin
   {$If Defined(RecordDrawSatOnMap) or Defined(RecordKeyDraw)}
      if MapDraw.MapType = mtUnenhancedRGB then TStr := ''
      else if IsSatelliteColorImage(MapDraw.MapType) then TStr := RGBString(SatView.RedBand,SatView.GreenBand,SatView.BlueBand)
      else TStr :=  ' gray=' + IntToStr(SatView.BandInWindow);
      WriteLineToDebugFile('tSatImage.DisplayImage in, ' + TStr +  '  pix size=' + RealToString(MapDraw.ScreenPixelSize,-12,-2) + '  data grid: ' +  sfBoundBoxToString(MapDraw.MapCorners.BoundBoxDataGrid,1));
   {$EndIf}
   ShowHourglassCursor;
   if BMPfile then begin
      {$IfDef NoBMPFileImagery}{$Else} DoBMPimage; {$EndIf}
   end
   else begin
      if IsSatelliteColorImage(MapDraw.MapType) then begin
         GetBandParameters(1,SatView.RedBand);
         GetBandParameters(2,SatView.GreenBand);
         GetBandParameters(3,SatView.BlueBand);
      end
      else begin
         GetBandParameters(1,SatView.BandInWindow);
      end;
      SetUpColors(MapDraw,SatView);

      if SatView.PanSharpenImage and (LandsatNumber in [7..9]) then begin
         GetBandParameters(4,8); //pan band for pan sharpening
         DrawPanSharpened(Bitmap);
      end
      else begin
         ShowHourglassCursor;
         {$If Defined(RecordDrawSatOnMap) or Defined(RecordKeyDraw)} WriteLineToDebugFile('Start pre-loop, sat data grid: ' +  sfBoundBoxToString(MapDraw.MapCorners.BoundBoxDataGrid,1)); {$EndIf}
         if ShowSatProgress then StartProgressAbortOption('Display ' + SceneBaseName);

         {$If Defined(RecordDrawSatOnMap)} WriteLineToDebugFile('Band spacing, Red: ' + RealToString(BandXSpace[SatView.RedBand],-8,0) + ' Green: ' + RealToString(BandXSpace[SatView.GreenBand],-8,0) +
             ' Blue: ' + RealToString(BandXSpace[SatView.BlueBand],-8,0)); {$EndIf}

         DiffSpace := ((abs(BandXSpace[SatView.RedBand] - BandXSpace[SatView.BlueBand]) > 0.01) or (abs(BandXSpace[SatView.RedBand] - BandXSpace[SatView.GreenBand]) > 0.01));
         if IsSatelliteColorImage(MapDraw.MapType) and DiffSpace then begin
            ColorAndDifferentSpacing;
         end
         else begin
           SingleBandOrColorsSameSpacing(Bitmap);
         end;
      end;
   end;
   if ShowSatProgress then EndProgress;
   {$IfDef RecordDrawSatOnMap} WriteLineToDebugFile('tSatImage.DisplayImage exit data grid: ' +  sfBoundBoxToString(MapDraw.MapCorners.BoundBoxDataGrid,1)); {$EndIf}
end;


function tSatImage.SatelliteImageLimits : string;
begin
    Result := 'Satellite image limits, Geographic:' + sfBoundBoxToString(SatelliteBoundBoxGeo(1)) + MessLineBreak + 'Satellite image limits, UTM:' + sfBoundBoxToString(SatelliteBoundBoxProj(1)) +  MessLineBreak;
end;


procedure tSatImage.SatGridToUTM(Band : integer; x,y : float64; var XUTM,YUTM : float64);
var
   Lat,Long : float64;
begin
   if (RegVars.Registration in [RegProjection]) then begin
      SatGridToProjectedCoords(Band, x,y,Xutm,Yutm);
      if (ImageMapProjection.PName <> UTMellipsoidal) then begin
         ImageMapProjection.InverseProjectDegrees(xutm,yutm,Lat,Long);
         ImageMapProjection.ForwardProjectDegrees(Lat,Long,XUTM,YUTM);
      end;
   end
end;


procedure tSatImage.SatGridToProjectedCoords(Band : integer; x,y : float64; var Xproj,Yproj : float64);
begin
   if (RegVars.Registration in [RegProjection]) then  begin
      xproj := RegVars.UpLeftX + X * BandXSpace[Band];
      yproj := RegVars.UpLeftY - Y * BandYSpace[Band];
   end;
end;


procedure tSatImage.LatLongDegreeToSatGrid(Band : integer; Lat,Long: float64;  var Xu,Yu : float32);
var
   xims,yims : integer;
   x,y : float64;
begin
   if (RegVars.Registration <> RegTIN)  then begin
      if (ImageMapProjection.pName <> PlateCaree) then ImageMapProjection.ForwardProjectDegrees(Lat,Long,X,Y);
      xu := (x - RegVars.UpleftX) / BandXSpace[Band];
      yu := (RegVars.UpleftY - y) / BandYSpace[Band];
   end
   else begin
      {$IfDef ExTIN}
      {$Else}
         SatTinRegistration.InterpolateImageCoords(Long,Lat,xims,yims);
         xu := xims;
         yu := yims;
      {$EndIf}
   end;
end;


procedure tSatImage.SatGridToLatLongDegree(Band : integer; XGrid,YGrid : float32; var Lat,Long : float64);
var
   xp,yp : float64;
begin
   Lat := 0;
   Long := 0;
   if (ImageMapProjection.pName = PlateCaree) then begin
      SatGridToProjectedCoords(Band,xgrid,ygrid,Long,Lat);
   end
   else if (RegVars.Registration = regprojection) then begin
      SatGridToProjectedCoords(Band,xgrid,ygrid,Xp,Yp);
      ImageMapProjection.InverseProjectDegrees(xp,yp,Lat,Long);
   end
   {$IfDef ExTIN}
   {$Else}
      else if (RegVars.Registration = RegTIN) then begin
         SatTinRegistration.InterpolateXY(xgrid,ygrid,Long,Lat);
      end
   {$EndIf}
end;


procedure tSatImage.ClipSatGrid(Band : integer; var x,y : float32);
begin
   if (x < 0) then x := 0;
   if (y < 0) then y := 0;
   if (x > pred(NumSatCol)) then x := pred(NumSatCol);
   if (y > pred(NumSatRow)) then y := pred(NumSatRow);
end;

procedure tSatImage.ClipSatGrid(Band : integer; var x,y : float64);
begin
   if (x < 0) then x := 0;
   if (y < 0) then y := 0;
   if (x > pred(NumSatCol)) then x := pred(NumSatCol);
   if (y > pred(NumSatRow)) then y := pred(NumSatRow);
end;


procedure tSatImage.ClipSatGrid(Band : integer; var x,y : Integer);
begin
   if (x < 0) then x := 0;
   if (y < 0) then y := 0;
   if (x > pred(BandColumns[Band])) then x := pred(NumSatCol);
   if (y > pred(BandRows[Band])) then y := pred(NumSatRow);
end;


procedure tSatImage.UTMtoSatGrid(Band : integer; X,Y : float64;  var Xu,Yu : float64);
var
   lat,Long : float64;
begin
   if (ImageMapProjection.PName <> UTMellipsoidal) then   begin
      ImageMapProjection.UTMtoLatLongDegree(x,y,Lat,Long);
      ImageMapProjection.ForwardProjectRadians(Lat,Long,x,y);
   end;
   xu := (x - RegVars.UpleftX) / BandXSpace[Band];
   yu := (RegVars.UpleftY - y) / BandYSpace[Band];
end;


function tSatImage.GetSatPointValue(Band,Col,Row : integer) : integer;
begin
   GetSatRow16bit(Band,Row,Row16bit^);
   Result := Row16Bit[Col];
end;


procedure tSatImage.GetSatRow16bit(Band,Row : integer; var TheRow : tWordRow16bit);
var
   Row8bit : tImageRow;
   i : integer;
begin
   if Data16Bit then begin
      TiffImage[Band].GetTiffRow16bit(Band,Row,TheRow);
   end
   else begin
      GetSatRow(Band,Row,Row8Bit);
      for i := 0 to pred(NumSatCol) do TheRow[i] := Row8Bit[i];
   end;
end;


procedure tSatImage.GetSatRow(Band,Row : integer; var TheRow : tImageRow);
{$IfDef ExSat}
begin
{$Else}
var
   r,g,b : byte;
   x,xp   : integer;
   Bitmap : tMyBitmap;
   Color  : tColor;
begin
   {$IfDef RecordGetSatRow} WriteLineToDebugFile(' tSatImage.GetSatRow, band: ' + IntToStr(Band) + '   Row: ' + IntToStr(Row)); {$EndIf}
   if (Band <= NumBands) then begin
      if Data16Bit then begin
         new(Row16Bit);
         TiffImage[Band].GetTiffRow16bit(Band,Row,Row16bit^);
         for xp := 0 to pred(NumSatCol) do begin
            TheRow[xp] := ValidByteRange(round((Row16bit^[xp] - MinRef[Band]) / (MaxRef[Band] - MinRef[Band]) * 255));
         end;
         Dispose(Row16Bit);
      end
      else if (TIFFImage[Band] <> Nil) then begin
         TiffImage[Band].GetTiffRow(Band,Row,TheRow);
      end
     {$IfDef NoBMPFileImagery}
     {$Else}
          else if BMPFile then begin
            Bitmap := tMyBitmap.Create;
            Bitmap.LoadFromFile(BMPFilename);
            for x := 0 to pred(NumSatCol) do begin
               Color := Bitmap.Canvas.Pixels[x,Row];
               GetRGBfromTColor(Color,r,g,b);
               case Band of
                  1 : TheRow[x] := b;
                  2 : TheRow[x] := g;
                  3 : TheRow[x] := r;
               end;
            end;
            Bitmap.Free;
         end
      {$EndIf}
      else begin
         {$IfDef RecordGetSatRow} WriteLineToDebugFile(' offset: ' + IntToStr(YOffset(Band,Row))); {$EndIf}
         seek(SatBandFile[Band],YOffset(Band,Row));
         if not EOF(SatBandFile[Band]) then BlockRead(SatBandFile[Band],TheRow,NumSatCol*SatRecSize)
         else MessageToContinue('Seek beyond end of file, row ' + IntToStr(Row) + '/' + IntToStr(NumSatRow));
      end;
   end;
{$EndIf}
end;


function tSatImage.ConvertDN(DN : word; Band : integer; HowConvert : tDNconvert = dncMDDefault) : float64;
begin
   if (HowConvert = dncMDDefault) then HowConvert := MDDef.dnConvert;
   if (LandsatNumber = 0) or (HowConvert = dncDN) then Result := DN
   else if (HowConvert  in [dncRadiance,dncBrightness]) then begin
      Result := LandsatMetadata.RadianceMult[Band] * DN + LandsatMetadata.RadianceAdd[Band];
      if (HowConvert  in [dncBrightness]) and (Band in [10,11]) then begin
         Result := LandsatMetadata.K1Const[Band] / Result;
         Result := Math.lnXP1(Result);
         Result := (LandsatMetadata.K2Const[Band] / Result);
      end;
   end
   else begin
       Result := LandsatMetadata.ReflectanceMult[Band] * DN + LandsatMetadata.ReflectanceAdd[Band];
       if (HowConvert  = dncReflectSun) then begin
          Result := Result / cosDeg(LandsatMetadata.SunElevation);
       end;
   end;
   if (Result < 0) then Result := 0;
end;



procedure tSatImage.LoadHistogram;
var
   i,j,Start : integer;
   fName1 : PathStr;

         procedure HistFromTiffFile(fName : PathStr; BandsInFile,BandOffset : integer);
         var
            Table : tMyData;
            Band,x,dn,npts : integer;
            Hist    : tWordValues;
            bName : shortstring;
            SeekingLow : boolean;
         begin
            {$IfDef RecordHistogram} WriteLineToDebugFile(ExtractFileName(fName) + '  band ' + IntToStr(Band) + '/' + IntToStr(BandsInFile) + '   offset=' + IntToStr(BandOffset)); {$EndIf}
            if FileExists(fName) then begin
               Table := tMyData.Create(fName);
               for Band := 1 to BandsInFile do begin
                  for x := 0 to MaxWord16 do Hist[x] := 0;
                  bName := 'BAND_' + IntToStr(Band+BandOffset);
                  if not Table.FieldExists(bName) then bName := 'BAND_1';
                  if Table.FieldExists(bName) then begin
                     Table.ApplyFilter(bname +'>0');
                     SeekingLow := true;
                     while not Table.EOF do begin
                        dn := Table.GetFieldByNameAsInteger('DN');
                        npts := Table.GetFieldByNameAsInteger(bName);
                        if SeekingLow then begin
                           MinRef[Band+BandOffset] := dn;
                           SeekingLow := false;
                        end;
                        Hist[dn] := npts;
                        Table.Next;
                     end;
                    MaxRef[Band+BandOffset] := dn;
                    {$If Defined(RecordKeyDraw) or Defined(RecordHistogram)} WriteLineToDebugFile('band=' + IntToStr(Band+BandOffset) + ' minref=' + IntToStr(MinRef[Band+BandOffset]) + '   maxref=' + IntToStr(MaxRef[Band+BandOffset])); {$EndIf}

                    for x := MinRef[Band+BandOffset] to MaxRef[Band+BandOffset] do begin
                       Distrib[Band+BandOffset]^[x] := Hist[x];
                    end;
                  end;
               end {for Band};
               Table.Destroy;
            end
            else begin
               {$IfDef RecordHistogram} WriteLineToDebugFile(ExtractFileName(fName) + '  missing'); {$EndIf}
            end {if FileExists};
         end {procedure};


begin
   {$IfDef RecordHistogram} WriteLineToDebugFile('tSatImage.LoadHistogram in, ' + SceneBaseName + ' bands=' + IntToStr(NumBands)); {$EndIf}
   InitializeHistogramDistributions;
   for i := 1 to NumBands do begin
      if (TiffImage[i] <> Nil) then begin
         {$IfDef RecordHistogram} WriteLineToDebugFile(IntToStr(i) + '  ' + TiffImage[i].TIFFFileName); {$EndIf}
         fName1 := HistogramLandsatName(TiffImage[i].TIFFFileName);
         HistFromTiffFile(fName1,1,pred(i));
      end;
   end;

   if MDDef.IgnoreHistogramZero then Start := 1 else Start := 0;
   for j := 1 to NumBands do begin
      NumPoints[j] := 0;
      for I := Start to MaxWord16 do inc(NumPoints[j],Distrib[j]^[i]);
   end;
end;


function tSatImage.FullImageGridLimits : tGridLimits;
begin
   Result.XGridLow  := 0;
   Result.YGridLow  := 0;
   Result.XGridHigh := pred(NumSatCol);
   Result.YGridHigh := pred(NumSatRow);
end;


procedure tSatImage.ReadBandData(SatName : shortstring; var SatView : tSatView);
var
   BandNames : tMyData;
   i : integer;

      function GetBand(Field : ShortString; Value : char) : integer;
      begin
         BandNames.ApplyFilter('SATELLITE=' + QuotedStr(SatName) + ' AND ' + Field + '=' + QuotedStr(Value));
         if (BandNames.RecordCount = 1) then Result := BandNames.GetFieldByNameAsInteger('BAND')
         else Result := 1;
         {$IfDef RecordTMSat} WriteLineToDebugFile(BandNames.Filter + ' means band=' + IntToStr(Result)); {$EndIf}
      end;

begin
   {$IfDef RecordTMSat} WriteLineToDebugFile('tSatImage.ReadBandData in, NumBands=' + IntToStr(NumBands)); {$EndIf}
   SatelliteName := SatName;
   BandNames := tMyData.Create(SatBandNames);
   BandNames.ApplyFilter('SATELLITE=' + QuotedStr(SatName));
   NumBands := BandNames.RecordCount;
   i := 0;
   while not BandNames.Eof do begin
      inc(i);
      BandLongName[i] := BandNames.GetFieldByNameAsString('LONG_NAME');
      BandShortName[i] := BandNames.GetFieldByNameAsString('SHORT_NAME');
      BandWavelengths[i] := 0.001 * BandNames.GetFieldByNameAsFloat('WAVE_NM');
      {$IfDef RecordTMSat} WriteLineToDebugFile('Band ' + IntToStr(i) + '  ' + BandShortName[i]); {$EndIf}
      BandNames.Next;
   end;
   SatView.BandInWindow  := GetBand('GRAYSCALE','G');
   DefRedTrue  := GetBand('TRUE_CLR','R');
   DefGreenTrue := GetBand('TRUE_CLR','G');
   DefBlueTrue := GetBand('TRUE_CLR','B');
   DefRedFalse := GetBand('FALSE_CLR','R');
   DefGreenFalse := GetBand('FALSE_CLR','G');
   DefBlueFalse := GetBand('FALSE_CLR','B');

   if (LandsatNumber in [4,5,6,7]) or (StrUtils.AnsiContainsText(IndexFileName,'NAIP') and (NumBands = 4)) or Is4band then begin
      DefIR2 := 5;
   end
   else if (LandsatNumber in [8,9]) then begin
      DefIR2 := 6;
   end
   else if SatelliteName = 'Sentinel-2' then begin
      DefIR2 := 12;  //Band 11
   end;

   if MDDef.SatMultiBandTrueColor then begin
      SatView.RedBand := DefRedTrue;
      SatView.GreenBand := DefGreenTrue;
      SatView.BlueBand := DefBlueTrue;
   end
   else begin
      SatView.RedBand := DefRedFalse;
      SatView.GreenBand := DefGreenFalse;
      SatView.BlueBand := DefBlueFalse;
   end;

   BandForSize := SatView.BandInWindow;

   BandNames.Destroy;
   BandNamesRead := true;
   {$If Defined(RecordTMSat) or Defined(RecordShortLandsat)} WriteLineToDebugFile('tSatImage.ReadBandData out, true color ' + RGBString(DefRedTrue,DefGreenTrue,DefBlueTrue) + ', false color ' + RGBString(DefRedFalse,DefGreenFalse,DefBlueFalse) + ' NumBands=' + IntToStr(NumBands)); {$EndIf}
end;


procedure tSatImage.DefineImageFromTiff(WhichOne : integer);
begin
   if (TIFFImage[WhichOne] <> Nil) then begin
      {$IfDef RecordLoadSat} WriteLineToDebugFile('DefineImageFromTifF, band=' + IntToStr(WhichOne)); {$EndIf}
      NumSatCol := TIFFImage[WhichOne].TiffHeader.ImageWidth;
      NumSatRow := TIFFImage[WhichOne].TiffHeader.ImageLength;
      ImageOffsetInFile := TIFFImage[WhichOne].TiffHeader.FirstImageOffset;
   end
   else begin
      {$IfDef RecordLoadSat} WriteLineToDebugFile('Cannnot DefineImageFromTifF, band=' + IntToStr(WhichOne)); {$EndIf}
   end;
end;


constructor tSatImage.Create(var SatView : tSatView; Files : tStringList; var ReadFileName : PathStr; NeedHist : boolean; var Success : boolean);
var
   Dir   : DirStr;
   Ext   : ExtStr;
   FName : PathStr;
   i     : integer;
   Bitmap : tMyBitmap;
   LandCover : ShortString;
   ReadFailure : boolean;


      function TryToLoadRegistration(Dir : PathStr) : boolean;
      var
         LatSizeMap,LongSizeMap,az : float64;
      begin
         {$IfDef RecordSatRegistration} WriteLineToDebugFile('Enter try to load sat registration ' + Dir); {$EndIf}
         {$IfDef RecordSatRegistration} WriteLineToDebugFile('Image size (pixels): ' + IntToStr(NumSatCol) + 'x' + IntToStr(NumSatRow)); {$EndIf}

         RegVars.Registration := RegProjection;
         {$IfDef RecordSatRegistration} WriteLineToDebugFile('Pixel size: ' + RealToString(RegVars.Pr_DeltaX,-12,-6) + 'x' + RealToString(RegVars.Pr_DeltaY,-12,-6)); {$EndIf}

         if (ImageMapProjection.pName = PlateCaree) then begin
            {$IfDef RecordSatRegistration} WriteLineToDebugFile('lat/long sat registration start'); {$EndIf}
            LatSizeMap := pred(NumSatRow)*RegVars.pr_DeltaY;
            LongSizeMap := pred(NumSatCol)*RegVars.Pr_DeltaX;
            {$IfDef RecordSatRegistration} WriteLineToDebugFile('Deg in image, lat=' + RealToString(LatSizeMap,-12,-2) + '  long=' + RealToString(LongSizeMap,-12,-2)); {$EndIf}
            {$IfDef Android}
               // because CalculateDistanceBearing crashes
               YMetersPerPixel := abs(LatSizeMap * 111000 / pred(NumSatRow));
               XMetersPerPixel := abs(LongSizeMap * 111000 / pred(NumSatCol) * CosDeg(RegVars.UpLeftY-0.5*NumSatRow*RegVars.pr_DeltaY));
            {$Else}
               if (LatSizeMap > 15) or (GetUTMZone(RegVars.UpLeftX) <> GetUTMZone(RegVars.UpLeftX + LongSizeMap)) then begin
                  {$IfDef RecordSatRegistration} WriteLineToDebugFile('110000 formula'); {$EndIf}
                  YMetersPerPixel := abs(LatSizeMap * 111000 / pred(NumSatRow));
                  XMetersPerPixel := abs(LongSizeMap * 111000 / pred(NumSatCol) * CosDeg(RegVars.UpLeftY-0.5*NumSatRow*RegVars.pr_DeltaY));
               end
               else begin
                  {$IfDef RecordSatRegistration} WriteLineToDebugFile('Vincenty'); {$EndIf}
                  VincentyCalculateDistanceBearing(RegVars.UpLeftY-0.5*NumSatRow*RegVars.pr_DeltaY,RegVars.UpLeftX,RegVars.UpLeftY-0.5*NumSatRow*RegVars.pr_DeltaY,RegVars.UpLeftX+pred(NumSatCol)*RegVars.pr_DeltaX,XMetersPerPixel,Az);
                  XMetersPerPixel := XMetersPerPixel / pred(NumSatCol);
                  VincentyCalculateDistanceBearing(RegVars.UpLeftY,RegVars.UpLeftX,RegVars.UpLeftY-pred(NumSatRow)*RegVars.pr_DeltaY,RegVars.UpLeftX,YMetersPerPixel,Az);
                  YMetersPerPixel := YMetersPerPixel / pred(NumSatRow);
               end;
            {$EndIf}
         end
         else begin
            XMetersPerPixel := abs(RegVars.pr_deltax);
            YMetersPerPixel := abs(RegVars.pr_deltay);
            {$IfDef RecordSatRegistration} WriteLineToDebugFile('UTM/rect sat registration'); {$EndIf}
         end;
         MetersPerPixel := 0.5 * (XMetersPerPixel + YMetersPerPixel);
         {$IfDef RecordSatRegistration} WriteLineToDebugFile('MetersPerPixel, x=' + RealToString(XMetersPerPixel,-8,-2) + ' Y=' + RealToString(YMetersPerPixel,-8,-2) + ' avg=' + RealToString(MetersPerPixel,-8,-2)); {$EndIf}
      end;


      procedure FixJPEGS;
      begin
         if ExtEquals(Ext, '.JPEG') or ExtEquals(Ext, '.JPE') then begin
            SysUtils.RenameFile(IndexFileName,ChangeFileExt(IndexFileName,'.jpg'));
            IndexFileName := ChangeFileExt(IndexFileName,'.jpg');
            Ext := '.jpg';
         end;
      end;

      function ReadTiffBand(FileName : PathStr; BandNum : integer; SatName : ShortString) : boolean;
      begin
         Result := FileExists(FileName);
         if Result then begin
            TemporaryNewGeotiff := false;
            TiffImage[BandNum] := tTiffImage.CreateGeotiff(false,ImageMapProjection,RegVars,false,FileName,Success,false,true,BandNum);
            BandRows[BandNum] := TiffImage[BandNum].TiffHeader.ImageLength;
            BandColumns[BandNum] := TiffImage[BandNum].TiffHeader.ImageWidth;
            BandXSpace[BandNum] := TiffImage[BandNum].TiffHeader.ScaleX;
            BandYSpace[BandNum] := TiffImage[BandNum].TiffHeader.ScaleY;
            if (BandLongName[BandNum] = '') then begin
               BandLongName[BandNum] := SatName + ' ' + IntToStr(i);
               BandShortName[BandNum] := BandLongName[BandNum];
               inc(NumBands);
            end;
            {$IfDef RecordTMSatFull}
               WriteLineToDebugFile('ReadTiffBand: ' + IntToStr(BandNum) +  '  ' + ExtractFileName(IndexFileName) +  '  ModelX=' +  RealToString(TiffImage[BandNum].TiffHeader.ModelX,-12,-4) +
                  '   ModelY=' +  RealToString(TiffImage[BandNum].TiffHeader.ModelY,-12,-4) + ' ScaleX=' +  RealToString(TiffImage[BandNum].TiffHeader.ScaleX,-12,-4) +  ' ScaleY=' +  RealToString(TiffImage[BandNum].TiffHeader.ScaleY,-12,-4));
            {$EndIf}
         end
         else begin
             {$If Defined(RecordTMSatFull) or Defined(RecordTMSat) or Defined(RecordShortLandsat)} WriteLineToDebugFile('ReadTiffBand: ' + IntToStr(BandNum) +  ' missing ' + ExtractFileName(IndexFileName)); {$EndIf}
         end;
      end;

            procedure ReadOrdinaryGeoTiff;
            var
               i : integer;

                        procedure BothBandName(i : integer; Color : shortstring);
                        begin
                            BandLongName[i] := Color;
                            BandShortName[i] := Color;
                        end;

                        procedure RGBName;
                        begin
                           BothBandName(1,'Red');
                           BothBandName(2,'Green');
                           BothBandName(3,'Blue');
                        end;

            begin
               {$IfDef RecordLoadSat} WriteLineToDebugFile('ReadOrdinaryGeoTiff ' + IndexFileName); {$EndIf}
               TemporaryNewGeotiff := false;
               TiffImage[1] := tTiffImage.CreateGeotiff(false,ImageMapProjection,RegVars,false,IndexFileName,Success);
               if Success then begin
                  if (TIFFImage[1].TiffHeader.SampleFormat = sfIEEEfloat) then begin
                     ReadFailure := true;
                     MessageToContinue('Imagery does not support IEEE float data; open as a DEM/grid');
                     exit;
                  end;
                  NumBands := TIFFImage[1].TiffHeader.SamplesPerPixel;
                  Data16Bit := TIFFImage[1].TiffHeader.BitsPerSample in [15,16];
                  {$IfDef RecordLoadSat} WriteLineToDebugFile('NumBands=' + IntToStr(NumBands)); {$EndIf}
                  for i := 1 to NumBands do begin
                     BandRows[i] := TiffImage[1].TiffHeader.ImageLength;
                     BandColumns[i] := TiffImage[1].TiffHeader.ImageWidth;
                     BandXSpace[i] := TiffImage[1].TiffHeader.ScaleX;
                     BandYSpace[i] := TiffImage[1].TiffHeader.ScaleY;
                  end;
                  if LandsatLook or EOBrowserExport then DefineImageFromTiff(1);

                  {$IfDef RecordLoadSat} WriteLineToDebugFile('ReadOrdinaryGeoTiff tTiffImage.CreateGeotiff OK'); {$EndIf}
                  if (TIFFImage[1].TiffHeader.PhotometricInterpretation = 2) or (TIFFImage[1].TiffHeader.SamplesPerPixel in [3,4,8]) then begin
                     if (NumBands = 8) and StrUtils.AnsiContainsText(IndexFileName,'WV2') then begin
                        //this has not been tested recently, and it is unclear if it will currently work with all bands in a single file
                        WV2Image := true;
                        ReadBandData('WV2',SatView);
                     end
                     else begin
                        for i := 1 to NumBands do BandLongName[i] := 'Band_' + IntToStr(i);
                        if (NumBands = 1) then SatView.BandInWindow := 1
                        else if (NumBands = 3) then begin
                           RGBName;
                           SatView.RedBand := 1;
                           SatView.GreenBand := 2;
                           SatView.BlueBand := 3;
                        end
                        else begin
                           if (TIFFImage[1].TiffHeader.BitsPerSampleCount = 4) then begin
                              BothBandName(4,'Alpha');
                              RGBName;
                              SatView.RedBand := 1;
                              SatView.GreenBand := 2;
                              SatView.BlueBand := 3;
                           end
                           else begin
                              BothBandName(4,'Near_IR');
                              RGBName;
                              SatView.RedBand := 4;
                              SatView.GreenBand := 3;
                              SatView.BlueBand := 2;
                           end;
                        end;
                     end;
                  end;

                  if TiffImage[1].TiffImageColorDefined then begin
                     New(DefinedImageColors);
                     DefinedImageColors^ := TiffImage[1].TiffImageColor;
                     {$IfDef RecordLoadSat} WriteLineToDebugFile('ReadOrdinaryGeoTiff out, defined image colors'); {$EndIf}
                  end;
                  SingleTiff := true;
                  MultiTiff := false;
                  if LandSatLook then CanEnhance := false;
                  {$IfDef RecordLoadSat} WriteLineToDebugFile('ReadOrdinaryGeoTiff out, can enhance=' + TrueOrFalse(CanEnhance)); {$EndIf}
               end
               else begin
                  {$IfDef RecordLoadSat} WriteLineToDebugFile('Failed ' + IndexFileName); {$EndIf}
               end;
            end;


               procedure OpenIkonos;
               //this has not been tested in a very long time, and probably only works for very old Ikonos imagery
               begin
                  IndexFileName := Dir + Copy(SceneBaseName,1,9) + 'blu' + Copy(SceneBasename,13,8) + Ext;
                  Data16Bit := true;
                  ReadTiffBand(IndexFileName,1,'VIS_NIR');
                  ReadTiffBand( Dir + Copy(SceneBaseName,1,9) + 'grn' + Copy(SceneBasename,13,8) + Ext,2,'VIS_NIR');
                  ReadTiffBand( Dir + Copy(SceneBaseName,1,9) + 'red' + Copy(SceneBasename,13,8) + Ext,3,'VIS_NIR');
                  ReadTiffBand( Dir + Copy(SceneBaseName,1,9) + 'nir' + Copy(SceneBasename,13,8) + Ext,4,'VIS_NIR');
                  NumBands := 4;
               end;


               procedure OpenSpot;
               //this has not been tested in a very long time, and probably only works for very old SPOT imagery
               begin
                  if FileExists(Dir + Copy(SceneBaseName,1,27) + '1' + Copy(SceneBasename,29,6) + Ext) then
                     IndexFileName := Dir + Copy(SceneBaseName,1,27) + '1' + Copy(SceneBasename,29,6) + Ext;
                  ReadTiffBand(IndexFileName,1,'VIS_NIR');
                  ReadTiffBand(Dir + Copy(SceneBaseName,1,27) + '2' + Copy(SceneBasename,29,6) + Ext,2,'VIS_NIR');
                  ReadTiffBand(Dir + Copy(SceneBaseName,1,27) + '3' + Copy(SceneBasename,29,6) + Ext,3,'VIS_NIR');
                  ReadTiffBand(Dir + Copy(SceneBaseName,1,27) + '4' + Copy(SceneBasename,29,6) + Ext,4,'VIS_NIR');
               end;

               procedure OpenPlanet(PlanetScope : boolean);
               //8 band imagery added in Sept 2022
               var
                  bName : PathStr;
                  i : integer;
               begin
                  Data16Bit := true;
                  bName := ExtractFilePath(IndexFileName) + ExtractFileNameNoExt(IndexFileName);
                  if PlanetScope then begin
                     if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'_SR_8B_') then NumBands := 8
                     else if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'_3B_AnalyticMS') then NumBands := 4;
                     SatelliteName := 'PlanetScope' + IntToStr(NumBands);
                  end;
                  for i := 1 to NumBands do begin
                     Delete(bName,length(bName),1);
                     bName := bName + IntToStr(i);
                     ReadTiffBand(bName +  '.tif',i,SatelliteName);
                  end;
                  ReadBandData(SatelliteName,SatView);
                  MissingDataValue := 255;
               end;

               procedure OpenUserMultiband;
               var
                  i : integer;
               begin
                  for i := 0 to pred(Files.Count) do begin
                     inc(NumBands);
                     {$IfDef RecordProblems} WriteLineToDebugFile(Files.Strings[i]); {$EndIf}
                     TemporaryNewGeotiff := false;
                     TiffImage[NumBands] := tTiffImage.CreateGeotiff(false,ImageMapProjection,RegVars,false,Files.Strings[i],Success,false,true,NumBands);
                     BandLongName[NumBands] := ExtractFileNameNoExt(Files.Strings[i]);
                     BandShortName[NumBands] := BandLongName[NumBands];
                     MultiTiff := true;
                     Data16Bit := (TIFFImage[1].TiffHeader.BitsPerSample in [15,16]);
                  end;
               end;


               function OpenLandsat : integer;
               var
                  i,BandNumberOffset : integer;
                  fName : PathStr;
               begin
                  {$If Defined(RecordTMSat) or Defined(RecordShortLandsat)} WriteLineToDebugFile('OpenLandsat in, looking in ' + LandSatDir + ' for scene ' + SceneBaseName + ' NumBands = ' + IntToStr(NumBands)); {$EndIf}
                  if LandsatLook then ReadOrdinaryGeoTiff
                  else begin
                     if (SceneBaseName[7] = '2') then begin
                        MessageToContinue('Landsat Level 2 data likely to have problems');
                     end;

                     SingleTiff := false;
                     MultiTiff := true;
                     Result := 0;

                     BandNumberOffset := 0;
                     if IsMSS(IndexFilename) then begin
                        if LandsatNumber in [1..3] then BandNumberOffset := 3;
                     end;

                     for i := 1 to Numbands do begin
                         bFileName[i] := LandSatDir + LandsatMetadata.BandFileName[i + BandNumberOffset];

                         //added 2/25/2022 to resolve issues with GDAL incorrectly rewriting tiled/compressed files and losing the rewritten file
                         if not FileExists(bFileName[i]) then begin
                            fName := ExtractFilePath(bFileName[i]) + 'original_' + ExtractFileName(bFileName[i]);
                            if FileExists(fName) then SysUtils.RenameFile(fName,bFileName[i]);
                         end;

                         {$IfDef RecordTMSat} WriteLineToDebugFile('TM Band = ' + IntToStr(i) + '  ' + bFileName[i] ); {$EndIf}
                         if not ReadTiffBand(bFileName[i],i,SatelliteName) then inc(Result);
                     end;
                  end;
                 {$IfDef RecordTMSat} WriteLineToDebugFile('Landsat preLoad OK, NumBands = ' + IntToStr(NumBands)); {$EndIf}
               end;


               procedure OpenSentinel2;
               var
                  i : integer;
                  BaseName : PathStr;
               begin
                  {$IfDef RecordTMSat} WriteLineToDebugFile('OpenSentinel2 in ' + IndexFileName); {$EndIf}
                     IsSentinel2 := true;
                     Data16Bit := true;
                     MultiTiff := true;
                     LandsatDir := LastSatDir;
                     ReadBandData('Sentinel-2',SatView);
                     SingleTiff := false;
                     SatelliteName := 'Sentinel-2';
                     BaseName := ExtractFilePath(IndexFileName) + ExtractFileNameNoExt(IndexFileName);
                     while (BaseName[length(BaseName)] <> '_') do Delete(BaseName,Length(BaseName),1);
                     for i := 1 to NumBands do begin
                        NeedToLoadGeoTiffProjection := true;
                        bFileName[i] := BaseName + BandShortName[i] + '.tif';
                        {$IfDef RecordTMSat} WriteLineToDebugFile('IndexFileName='+bFileName[i]); {$EndIf}
                        ReadTiffBand(bFileName[i],i,SatelliteName);
                     end;
                     NumSatCol := BandColumns[2];
                     NumSatRow := BandRows[2];
                     RegVars.pr_deltaY  := BandYSpace[2];
                     RegVars.pr_deltaX  := BandXSpace[2];
                     NeedToLoadGeotiffProjection := true;
                 {$IfDef RecordTMSat} WriteLineToDebugFile('OpenSentinel2 out'); {$EndIf}
               end;


       {$IfDef NoBMPFileImagery}
       {$Else}
          procedure OpenBitmapWithImage;
          var
             Registered : boolean;
             DigitizeDatum    : ShortString;
          begin
             {$IfDef RecordLoadSat} WriteLineToDebugFile('OpenBitmapWithImage, ext= ' + Ext); {$EndIf}
             BMPFile := true;
             Bitmap := tMyBitmap.Create;
             Registered := PetImage.ValidImageFileExt(Ext) and ReadWorldFile(ImageMapProjection,DigitizeDatum,IndexFileName,RegVars);

             if not Registered then begin
                {$IfDef RecordLoadSat} WriteLineToDebugFile('World file fail'); {$EndIf}
                if (Ext = '.JPG') then begin
                   TryToLoadRegistration(Dir);
                end
                else if (Ext = '.XY') then begin
                   if TryToLoadRegistration(Dir) then begin
                      if FileExists(Dir+SceneBaseName+'.jpg') then Ext := '.jpg';
                      if FileExists(Dir+SceneBaseName+'.bmp') then Ext := '.bmp';
                      if FileExists(Dir+SceneBaseName+'.gif') then Ext := '.gif';
                      if FileExists(Dir+SceneBaseName+'.png') then Ext := '.png';
                   end;
                end
                else if (Ext[length(Ext)] = 'W') then Registered := ReadWorldFile(ImageMapProjection,DigitizeDatum,IndexFileName,RegVars);
             end;

             {$IfDef RecordLoadSat} WriteLineToDebugFile('Start Open image'); {$EndIf}
             ShowHourglassCursor;

             if (Ext = '.JGW') or (Ext = '.JPW') or (Ext = '.JPG') or (Ext = '.HTM') then begin
                Bitmap := Petimage.LoadBitmapFromFile(Dir+SceneBaseName+'.jpg');
                BMPFileName := MDTempDir + SceneBaseName + OverlayFExt;
                Bitmap.SaveToFile(BMPFileName);
             end
             else if (Ext = '.GFW') or (Ext = '.GIF') then begin
                Bitmap := Petimage.LoadBitmapFromFile(Dir+SceneBaseName+'.gif');
                BMPFileName := NextFileNumber(MDTempDir, 'gifimage', OverlayFExt);
                Bitmap.SaveToFile(BMPFileName);
             end
             else if (Ext = '.PNW') or (Ext = '.PGW') or (Ext = '.PNG') then begin
                Bitmap := Petimage.LoadBitmapFromFile(Dir+SceneBaseName + '.png');
                BMPFileName := NextFileNumber(MDTempDir, 'pngimage', OverlayFExt);
                Bitmap.SaveToFile(BMPFileName);
             end
             else begin
                BMPFileName := ChangeFileExt(IndexFileName,OverlayFExt);
                Bitmap.LoadFromFile(BMPFileName);
                if (Bitmap.PixelFormat <> pf24Bit) then MessageToContinue('Non 24 bit image may not work.');
             end;
             {$IfDef RecordLoadSat} WriteLineToDebugFile('End Open image'); {$EndIf}
             if Registered then begin
                {$IfDef RecordLoadSat} WriteLineToDebugFile('World file worked'); {$EndIf}
                BandColumns[1] := Bitmap.Width;
                BandXSpace[1] := abs(RegVars.pr_DeltaX);
                BandRows[1] := Bitmap.Height;
                BandYSpace[1] := abs(RegVars.pr_DeltaY);
             end;

             NumSatCol := Bitmap.Width;
             NumSatRow := Bitmap.Height;
             Bitmap.Free;
             NumBands := 1;
             BandLongName[1] := 'RGB';
             BandShortName[1] := 'RGB';
             NeedHist := false;
             CanEnhance := false;
          end;
       {$EndIf}

               procedure OpenWorldView3(sName : shortstring);
               var
                  i,j : integer;
                  fList : tStringList;
                  Flag,flag2 : shortstring;
               begin
                  {$IfDef RecordTMSat} WriteLineToDebugFile('OpenWorldView3 in ' + IndexFileName); {$EndIf}
                     Data16Bit := true;
                     MultiTiff := true;
                     LandsatDir := LastSatDir;
                     ReadBandData(sName,SatView);
                     SingleTiff := false;
                     SatelliteName := sName;
                     Flist := Nil;
                     Dir := GetParentDirectory(Dir);
                     Petmar.FindMatchingFiles(Dir,'*.tif',fList,6);
                     {$IfDef RecordTMSat} WriteLineToDebugFile('Tiff found=' + IntToStr(fList.Count)); {$EndIf}
                     if sName ='WV3-MS' then flag2 := '_MUL' else flag2 := '_SWR';
                     
                     for i := 1 to NumBands do begin
                        flag := '_P001_b' + IntToStr(i) +'.tif';

                        for j := 0 to pred(fList.Count) do begin
                           if StrUtils.AnsiContainsText(fList.Strings[j],flag) and StrUtils.AnsiContainsText(fList.Strings[j],flag2) then begin
                              bFileName[i] := fList.Strings[j];
                              {$IfDef RecordTMSat} WriteLineToDebugFile('IndexFileName='+bFileName[i]); {$EndIf}
                              ReadTiffBand(bFileName[i],i,sName);
                           end;
                        end;
                     end;
                  {$IfDef RecordTMSat} WriteLineToDebugFile('OpenWorldView3 out'); {$EndIf}
               end;


   function ProcessGDAL(Ext : ExtStr) : boolean;
   var
      MissingBands : integer;
   begin
      {$IfDef ExGDAL}
         Result := false;
      {$Else}
         if GDALImageFormat(Ext) then begin
            if GDALImageFormat(Ext,true) then begin
               {$IfDef RecordGDAL} WriteLineToDebugFile('GDAL for ' + IndexFileName); {$EndIf}
               if (Ext = '.JP2') then begin
                  IndexFileName := GDAL_warp(IndexFileName);
               end
               else begin
                  IndexFileName := GDAL_Translate_2_geotiff(IndexFileName);
               end;
               Ext := '.TIF';
               {$IfDef RecordGDAL} WriteLineToDebugFile('DEMeros GDAL over for ' + IndexFileName); {$EndIf}
            end;
            {$If Defined(RecordLoadSat) or Defined(RecordTMSat)} WriteLineToDebugFile('TIFF=' + IndexFileName + '  Scene base name = ' + SceneBaseName); {$EndIf}

            if FileExists(IndexFileName) then begin

               LandsatSceneMetadata(IndexFilename,LandsatNumber,SceneBaseName);
               if (LandsatNumber <> 0) then begin
                  if StrUtils.AnsiContainsText(IndexFileName,'BQA') then begin
                     Data16Bit := true;
                  end
                  else begin
                     GetLandsatMetadata(IndexFilename, LandsatMetadata);
                     {$IfDef RecordTMSat} WriteLineToDebugFile('Back from LandsatSceneMetadata, scenebasename= ' + SceneBaseName); {$EndIf}

                     if (LandsatNumber in [1..5]) and IsMSS(IndexFilename) then begin
                        if (LandsatNumber in [1..2]) then begin
                           ReadBandData('L1-L2',SatView);
                        end
                        else if (LandsatNumber in [3]) then begin
                           ReadBandData('L3',SatView);
                        end
                        else if (LandsatNumber in [4,5]) then begin
                           ReadBandData('L4-L5',SatView);
                        end;
                     end
                     else if LandsatNumber in [4..9] then begin
                        if (LandsatNumber in [8,9]) then begin
                           Data16Bit := true;
                           ReadBandData('TM8',SatView);
                        end
                        else begin
                           if (LandsatNumber in [4,5]) then ReadBandData('TM',SatView)
                           else ReadBandData('ETM',SatView);
                        end;
                        {$If Defined(RecordTMSat) or Defined(RecordShortLandsat)} WriteLineToDebugFile('Landsat read band data ok'); {$EndIf}
                     end;
                  end;
                  MissingBands := OpenLandsat;
               end
               else begin
                  if IsThisSentinel2(IndexFileName) then OpenSentinel2
                  else if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'IKONOS') then OpenIkonos
                  else if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'SPOT') then OpenSpot
                  else if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'_SR_8B_') then OpenPlanet(true)
                  else if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'_4B_') then OpenPlanet(true)
                  else if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'_3B_') then OpenPlanet(true)
                  else if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'PLANET') then OpenPlanet(false)
                  else if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'WV3-MS') then OpenWorldView3('WV3-MS')
                  else if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'WV3-SWIR') then OpenWorldView3('WV3-SWIR')
                  else if (Files <> Nil) then OpenUserMultiband
                  else begin
                     ReadOrdinaryGeoTiff;
                     MultiTiff := false;
                  end;
               end;
               CanEnhance := (NumBands >= 1) and (TiffImage[1] <> Nil) and TIFFImage[1].CanEnhance and (not LandsatLook);
               Success := not ReadFailure;

               if Success then begin
                  CanEnhance := (NumBands >= 1) and (TiffImage[1] <> Nil) and TIFFImage[1].CanEnhance and (not LandsatLook);
                  if StrUtils.AnsiContainsText(UpperCase(IndexFileName),'CASI') and (TIFFImage[1].TiffHeader.SamplesPerPixel in [144]) then begin
                     NumBands := TIFFImage[1].TiffHeader.SamplesPerPixel;
                     ReadBandData('CASI',SatView);
                  end;
                  if MultiTiff then DefineImageFromTiff(SatView.BandInWindow) else DefineImageFromTiff(1);
                  Result := true;
                 {$IfDef RecordLoadSat} WriteLineToDebugFile('Done TIFF portion can enhance = ' + TrueOrFalse(CanEnhance)); {$EndIf}
                end;
            end
            else begin
               Result := false;
               MessageToContinue('GDAL failure; run c:\mapdata\temp\rev2.bat in command window to see why');
            end;
         end;
         {$EndIf}
      end;


      procedure InitializeValues;
      var
         i : integer;
      begin
         SatelliteName := '';
         LoadErrorMessage := '';
         FullWorld := false;
         ReadFailure := false;
         Data16Bit := false;
         BandNamesRead := false;
         CanEnhance := true;
         LandsatLook := false;
         UTMValidImage := true;
         HistogramNeedsInitialization := true;
         DefRedTrue := 1;
         BandForSize := 1;
         DefRedTrue  := 1;
         DefGreenTrue := 1;
         DefBlueTrue := 1;
         DefRedFalse := 1;
         DefGreenFalse := 1;
         DefBlueFalse := 1;

         DefinedImageColors := Nil;
         for I := 1 to MaxTiffsInImage do TiffImage[i] := Nil;
         for i := 1 to 3 do ColorLookUp[i] := Nil;
         ImageMapProjection := tMapProjection.Create('image');

         InitializeSatView(SatView);
         MissingDataValue := 0;
         SatRecSize := 1;
         DEMStatsString := '';
         RegVars.Registration := RegNone;
         CurrentSidName := '';
         SceneTitle := '';
         ImageOffsetInFile := 0;
         LandsatNumber := 0;
         WV2Image := false;
         is4Band := false;
         MultiTiff := false;
         SingleTiff := false;
         for i := 1 to MaxBands do begin
            BFileName[i] := '';
            Distrib[i] := Nil;
            BandLongName[i] := 'Band_' + IntToStr(i);
            BandShortName[i] := BandLongName[i];
            BandWavelengths[i] := -99;
         end;
         New(Row16Bit);
         for i := 1 to 4 do New(SatRow[i]);

         {$IfDef ExTIN}
         {$Else}
            SatTINRegistration := Nil;
         {$EndIf}
         {$IfDef ExAdvancedSats}
         {$Else}
            HypersectralImage := Nil;
         {$EndIf}

         {$IfDef NoBMPFileImagery}
         {$Else}
            BMPFile := false;
         {$EndIf}
      end;


begin
   {$If Defined(BasicOpens) or Defined(RecordLoadSat)} WriteLineToDebugFile('tSatImage.Create, need hist=' + TrueOrFalse(NeedHist)); {$EndIf}
   InitializeValues;

   ShowHourglassCursor;
   if (Files <> Nil) then begin
      ReadFileName := Files[0];
      {$If Defined(RecordKeyDraw) or Defined(RecordLoadSat)} WriteLineToDebugFile('Loading sat file list, count=' + IntToStr(Files.Count)); {$EndIf}
      OriginalFileName := ReadFileName;
   end
   else begin
      OriginalFileName := ReadFileName;
      {$If Defined(RecordKeyDraw) or Defined(RecordLoadSat)} WriteLineToDebugFile('Load ' + ReadFileName); {$EndIf}
      FSplit(ReadFileName,Dir,fName,Ext);
      if (Ext = '.FRQ') then begin
         ChangeFileExt(ReadFileName,'.TIF');
         Ext := '.TIF';
      end;
   end;

   IF ValidDBfName(IndexFileName) then begin
      if not AnswerIsYes('File appears to be a vector dataset; are you sure you want to try to open it as a raster image') then begin
         Success := false;
         exit;
      end;
   end;

   {$IfDef RecordSat} WriteLineToDebugFile('Pt 2 NeedHist=' + TrueOrFalse(NeedHist)); {$EndIf}
   IndexFileName := ReadFileName;
   LandsatDir := ExtractFilePath(ReadFileName);

   Success := true;
   FSplit(IndexFileName,Dir,fName,Ext);
   SceneBaseName := fName;
   SceneTitle := SceneBaseName;

   Ext := UpperCase(Ext);
   FixJPEGS;
   {$IfDef ExMrSID}
   {$Else}
      if (Ext = '.SID') then begin
         {$IfDef RecordLoadSat} WriteLineToDebugFile('MrSid image identified'); {$EndIf}
         CurrentSidName := IndexFileName;
         MrSidImagery.ExtractTiffFromSID(IndexFileName);
         Ext := '.TIF';
      end;
   {$EndIf}
   LandSatLook := IsLandsatLook(IndexFileName);
   EOBrowserExport := IsEOBrowserExport(IndexFileName);
   if (TreatThisAsSingleTif and ((Ext = '.TIF') or (Ext = '.TIFF'))) or LandsatLook or EOBrowserExport then begin
      ReadOrdinaryGeoTiff;
      if LandsatLook or EOBrowserExport then begin
         {$If Defined(RecordEOBrowser)} WriteLineToDebugFile('tSatImage.Create, ' + IndexFileName + '  LandsatLook or EOBrowserExport'); {$EndIf}
         CanEnhance := false;
      end;
   end
   else if (Ext = '.JPG') or (Ext = '.JPEG') or (Ext = '.JPE') or (Ext = '.GIF') or (Ext = '.PNG') or
      (Ext = '.BMP') or (Ext = '.HTM') or (Ext = '.BPW') or (Ext = '.JGW') or (Ext = '.JPW') or
      (Ext = '.GFW') or (Ext = '.XY') or (Ext = '.PNW') or (Ext = '.PGW') then begin
         {$IfDef NoBMPFileImagery}
         {$Else}
            OpenBitmapWithImage;
         {$EndIf}
   end
   else if not ProcessGDAL(Ext) then begin
      Success := false;
      if (LoadErrorMessage = '') then LoadErrorMessage := SceneBaseName + ' Load failure';
      exit;
   end;

   if (NumBands < MaxBands) and (not SingleTiff) and (not MultiTiff) then begin
      for i := 1 to NumBands do begin
         {$IfDef RecordSat} WriteLineToDebugFile(IntToStr(i) + '  ' + BFileName[i]); {$EndIf}
         if (BFileName[i] <> '') then begin
            InsureFileIsNotReadOnly(BFileName[i]);
            AssignFile(SatBandFile[i],BFileName[i]);
            reset(SatBandFile[i],1);
         end;
      end;
   end;

    {$IfDef ExTIN}
    {$Else}
       fName := Dir + 'tin-' + SceneBaseName + DefaultDBExt;
       if FileExists(fName) then begin
          FName := Dir + SceneBaseName + DefaultDBExt;
          SatTINRegistration := tTin.Create(nil,fName,true,true);
       end
       else SatTINRegistration := nil;
    {$EndIf}

   TryToLoadRegistration(Dir);

   if NeedHist and (not IsThisLandCover(IndexFileName,LandCover)) and (not LandsatLook) then begin
      {$IfDef RecordLoadSat} WriteLineToDebugFile('Call load histogram, NumBands=' + IntToStr(NumBands)); {$EndIf}
      LoadHistogram;
   end;
   {$If Defined(RecordLoadSat) or Defined(RecordEOBrowser)} WriteLineToDebugFile('tSatImage.Create out ' + SceneBaseName + '  can enhance=' + TrueOrFalse(CanEnhance) + '  ' + ImageMapProjection.GetProjectionName); {$EndIf}
end;


procedure tSatImage.CreateDBfromSatImage(var fName : PathStr;  bb : sfBoundbox);
{$IfDef NoClustering}
begin
{$Else}
var
   Sampler,i,x,y : integer;
   Lat,Long : float64;
   Table   : tMyData;
   RowVals : array[1..MaxBands] of ^tImageRow;
begin
    fName := ExtractFilePath(OriginalFileName) + SceneBaseName + '_refs.dbf';
    Make_Tables.MakeSatelliteTable(fName,NumBands);
    Table := tMyData.Create(fName);
    Sampler := 1;
    while ((bb.xmax-bb.xmin) / Sampler) * ((bb.ymax-bb.ymin) / Sampler) > EdburgGeneralFuncsMaxObservations do inc(Sampler);

    for i := 1 to NumBands do GetMem(RowVals[i],NumSatCol*SatRecSize);

    StartProgress('Extract');
    y := round(bb.ymin);
    while y <= round(bb.ymax) do begin
       for i := 1 to NumBands do begin
          if (Copy(BandLongName[i],1,4) <> 'Miss') then begin
             GetSatRow(i,y,RowVals[i]^);
          end;
       end;
       UpdateProgressBar((y-bb.ymin)/(bb.ymax-bb.ymin));
       x := round(bb.xmin);
       while x <= round(bb.xmax) do begin
          if (not MDDef.IgnoreHistogramZero) or (RowVals[1]^[x] > 0) then begin
            SatGridToLatLongDegree(1,X,Y,Lat,Long);
            Table.Insert;
            Table.SetFieldByNameAsFloat('LAT',Lat);
            Table.SetFieldByNameAsFloat('LONG',Long);
            for i := 1 to NumBands do begin
               if (Copy(BandLongName[i],1,4) <> 'Miss') then begin
                  Table.SetFieldByNameAsInteger('BAND_' + IntToStr(i),RowVals[i]^[x]);
               end;
            end;
            Table.Post;
          end;
          inc(x,sampler);
       end;
       inc(y,sampler);
    end;
    for i := 1 to NumBands do FreeMem(RowVals[i],NumSatCol*SatRecSize);
    Table.Destroy;
    EndProgress;
{$EndIf}
end;


destructor tSatImage.Destroy;
var
   i : integer;
begin
   {$IfDef RecordSat} WriteLineToDebugFile('Close ' + SceneBaseName); {$EndIf}
   for i := 1 to MaxTiffsInImage do if (TiffImage[i] <> Nil) then begin
      TiffImage[i].Destroy;
      TiffImage[i] := Nil;
   end;

   if (DefinedImageColors <> Nil) then Dispose(DefinedImageColors);

   for i := 1 to MaxBands do begin
      if (Distrib[i] <> Nil) then Dispose(Distrib[i]);
      Distrib[i] := Nil;
   end;

   Dispose(Row16Bit);
   for i := 1 to 4 do Dispose(SatRow[i]);

   {$IfDef ExTIN}
   {$Else}
      if (SatTINRegistration <> Nil) then begin
         SatTINRegistration.Destroy;
      end;
   {$EndIf}

   {$IfDef VCL}
   if (SelectionMap <> Nil) then try
      {$IfDef RecordSat} WriteLineToDebugFile('Close image selection map'); {$EndIf}
      SelectionMap.MapDraw.ClosingMapNow := true;
      SelectionMap.Close;
   finally
      SelectionMap := Nil;
   end;
   {$EndIf}

   {$IfDef RecordSat} WriteLineToDebugFile('Closed image selection map'); {$EndIf}

   if (ImageMapProjection <> Nil) then FreeAndNil(ImageMapProjection);
   dec(NumSatImageOpen);
   inherited;
   {$IfDef RecordSat} WriteLineToDebugFile('Closed image ' + SceneBaseName); {$EndIf}
end;

{$If Defined(RecordSatColor) or Defined(RecordByteLookup)}
   function tSatImage.ContrastName(SatView : tSatview) : shortstring;
   begin
      case SatView.WindowContrast of
         NoEnhancement         : Result := 'none';
         StraightLinearStretch : Result := 'linear';
         TailLinearStretch     : Result := 'linear w/ tails at ' +  RealToString(Satview.WindowContrastLowTailSize,-12,-1) + '% and ' +  RealToString(Satview.WindowContrastHighTailSize,-2,-1) + '%';
         HistogramEqualization : Result := 'hist eq';
         Custom                : Result := 'custom';
         CloudOnlyTailStretch  : Result := 'cloud tail at ' + RealToString(Satview.WindowContrastHighTailSize,-2,-1) + '%';
         MaskRange             : Result := 'mask range';
      end;
   end;
{$EndIf}

function tSatImage.BandRange(Band : integer) : shortstring;
begin
   Result :=  'min=' + IntToStr(MinRef[band]) + '   max=' + IntToStr(MaxRef[band]);
end;


function tSatImage.YOffset(Band,y : integer) : LongInt;
var
   Offset : LongInt;
begin
   Offset := y;
   if (Band <= NumBands) then YOffset := Offset * NumSatCol * SatRecSize + ImageOffsetInFile
   else YOffset := Offset * NumSatCol * SatRecSize;
end;


initialization
   {$IfDef MessageStartUpUnit} MessageToContinue('Startup demeros'); {$EndIf}
   NumSatImageOpen := 0;
   TreatThisAsSingleTif := false;
finalization
   {$IfDef RecordGetSatRow} WriteLineToDebugFile('RecordGetSatRowProblems active in DEMeros, serious performance hit'); {$EndIf}
   {$IfDef RecordWorldFile} WriteLineToDebugFile('RecordWorldFileProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordHistogram} WriteLineToDebugFile('RecordHistogramProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordSat} WriteLineToDebugFile('RecordSatProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordSatColor} WriteLineToDebugFile('RecordSatColorProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordSatInterpolateColor} WriteLineToDebugFile('RecordSatInterpolateColorProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordSatFrame} WriteLineToDebugFile('RecordSatFrameProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordLoadSat} WriteLineToDebugFile('RecordLoadSatProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordTMSat} WriteLineToDebugFile('RecordTMSatProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordSatRegistration} WriteLineToDebugFile('RecordSatRegistrationProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordSatTimeSeries} WriteLineToDebugFile('RecordSatTimeSeries active in DEMeros'); {$EndIf}
   {$IfDef RecordGDAL} WriteLineToDebugFile('RecordGDAL active in DEMeros'); {$EndIf}
   {$IfDef RecordSpectralLibraryGraph} WriteLineToDebugFile('RecordSpectralLibraryGraph active in DEMeros'); {$EndIf}
   {$IfDef RecordScattergram} WriteLineToDebugFile('RecordSpectralLibraryGraph active in DEMeros'); {$EndIf}
   {$IfDef RecordLoadClass} WriteLineToDebugFile('RecordLoadClassProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosingProblems active in DEMeros'); {$EndIf}
   {$IfDef RecordDrawSatOnMap} WriteLineToDebugFile('RecordDrawSatOnMap active in DEMeros'); {$EndIf}
   {$IfDef RecordKeyDraw} WriteLineToDebugFile('RecordKeyDraw active in DEMEROS'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demeros out'); {$EndIf}
end {unit}.



