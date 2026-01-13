unit GeoTiff;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ References:                                                     }
{    http://partners.adobe.com/asn/developer/pdfs/tn/TIFF6.pdf    }
{    http://www.remotesensing.org/geotiff/spec/geotiffhome.html   }
{_________________________________________________________________}


{$I nevadia_defines.inc}

{$Define InlineGeotiff}    //turn off to debug inline functions

{$IfDef Recordproblems}  //normally only defined for debugging specific problems

   {$IFDEF DEBUG}
      {$Define RecordGeotiff}
      //{$Define RecordGeotiffRewrite}
      //{$Define GeotiffSave}
      //{$Define RecordJustMetadata}
      //{$Define RecordGeotiffFailures}
      //{$Define RecordFullGeotiff}
      //{$Define RecordUTM}
      //{$Define RecordWKT}
      {$Define RecordProjProgress}
      //{$Define TrackWKTstring}
      {$Define RecordInitDEM}
      //{$Define RecordInitDEMName}
      //{$Define Record_h_datum_code}
      //{$Define ReportKey258}  //happens with some Landsat, but does not appear to stop things
      //{$Define TrackPixelIs}
      //{$Define TrackDEMCorners}
      //{$Define GeotiffCorner}
      //{$Define RecordImageOffsets}
      //{$Define TrackHorizontalDatum}
      //{$Define TrackVerticalDatum}
      {$Define RecordDEMMapProj}
      {$Define RecordGeotiffProjection}
      {$Define RecordDefineDatum}
      //{$Define RecordGeotiffDestroy}
      //{$Define RecordUKOS}
      //{$Define TrackProjection}
      //{$Define ShowKeyDEM}
      //{$Define TrackZ}
      //{$Define RecordTiePoints}
      //{$Define RecordGeotiffRestart}
      //{$Define TrackModelType}
      //{$Define RecordDisplayBitmat}
      //{$Define RecordBitPerPixel}
      //{$Define TrackA}
      //{$Define RecordKeys}
      //{$Define LongCent}
      //{$Define RecordEntryInGeotiff}
      //{$Define RecordPlateCaree}
      //{$Define RecordGeotiffHistogram}
      //{$Define Record3076}
      //{$Define RecordProcessingHeader}
      //{$Define RecordGeotiffPalette}
      //{$Define RecordMultiGrids}
      //{$Define RecordMinMax}
      //{$Define RecordNLCD}
      //{$Define RecordGeotiffRow}    //potential major slowdown
      //{$Define FullDEMinit}         //potential major slowdown
      //{$Define NoGeotiffProjection}  //Jan 2024 added to track down problem opening a Geotiff
   {$ELSE}
   {$ENDIF}
{$EndIf}


interface

uses
   {$IfDef VCL}
      Graphics,VCL.Forms,WinAPI.Windows,Vcl.ExtCtrls,
   {$EndIf}
   {$IfDef FMX}
      FMX.Graphics,
   {$EndIf}
   Classes, SysUtils,StrUtils,
   DEMCoord,DEMmapF,DEMMapdraw,BaseMap,DEMDefs,DEMDef_routines,Petmar_types,PETMAR;

type
   //ResolutionUnitType = (None, inch, cm);
   tSampleFormat = (sfWord,sfInt16,sfIEEEFloat,sfUndefined);
   DoubleBytes = packed array[1..8] of byte;
   SingleBytes = packed array[1..4] of byte;

   tWordBigRow = array[0..pred(MaxSatCols)*MaxBands] of word;
   tOffsetArray = array[0..pred(MaxSatCols)] of int64;
   tSmallOffsetArray = array[0..pred(MaxSatCols)] of int32;

    tTiffHeader = record
      SamplesPerPixel,BitsPerSample,BytesPerSample,
      xResolutionOffset,yResolutionOffset,
      Orientation,PlanarConfiguration,SubFileType,FillOrder,
      PhotometricInterpretation,ExtraSample,
      Threshholding,NumEnt,
      Num,Den,NewSubfileType,VertDatum,
      StripsPerImage,
      BitsPerSampleCount,MDZtype : int32;

      FirstImageOffset,CellWidth,CellLength,StripByteCounts,
      RowsPerStrip,TileWidth,TileHeight,Compression,
      ImageWidth,ImageLength,StripOffsets : int64;
      SMin,SMax,Factor : float64;
      ResolutionUnit   : byte;
      MinSampleValue,MaxSampleValue : array[1..MaxBands] of int32;
      SampleFormat : tSampleFormat;
      OffsetArray : ^tOffsetArray;
      FootDEM : boolean;

      //tGeoTiffHeader unique values
         HemiChar   : AnsiChar;
         RasterPixelIs,ModelType  : int16;
         ScaleX,ScaleY,ScaleZ,RasterX,RasterY,RasterZ,ModelX,ModelY,ModelZ : double;
   end;


type
   tTIFFImage = class
      private
      protected
         Row16Bit : ^tWordRow16Bit;
         Row8Bit  : ^tRow8Bit;
         BigRow   : ^tWordBigRow;
         Tag42112 : shortstring;
         Tag42112Offset,Tag42112Length : int64;
         OffsetByteSize : word;
         OffsetArraySize,
         ImageBytesPerRow,
         FirstIFD : int64;
         BigTiff,
         TiffOpen,
         NeedWKTHailMary : boolean;
         OriginalFileName: PathStr;
         CurrentMissing : float32;

         function WordToByte(Band : integer; Row16bit : Word) : byte; inline;
         function MakeDouble : Double;   {$IfDef InlineGeotiff} inline; {$EndIf}
         function MakeSingle : Single;   {$IfDef InlineGeotiff} inline; {$EndIf}
         function MakeWord : Word;       {$IfDef InlineGeotiff} inline; {$EndIf}
         function MakeLongInt : int32;   {$IfDef InlineGeotiff} inline; {$EndIf}
         function MakeLongWord : LongWord; {$IfDef InlineGeotiff} inline; {$EndIf}
         function MakeUnsigned8Byte : int64; {$IfDef InlineGeotiff} inline; {$EndIf}
         function Makeoffset : int64;  {$IfDef InlineGeotiff} inline; {$EndIf}
         function ValidZ(z : float64) : boolean; {$IfDef InlineGeotiff} inline; {$EndIf}

         function TiledImage : boolean;
         function TiffMustBeRewritten : boolean;

         {$IfDef ExSat}
         {$Else}
            procedure GetHistogramDBF(SingleFileBand : integer);
         {$EndIf}
      public
         TiffHeader    : tTiffHeader;
         MapProjection : tMapProjection;
         RegVars : tRegVars;
         TIFFFileName   : PathStr;
         TiffHandle : THandle; //public to read in multigrid
         BigEndian,   //public to read in multigrid
         CanEnhance,
         TIFFImageColorDefined  : boolean;   //must be public
         TIFFImageColor : Petmar_types.TRGBLookUp;
         procedure OpenTiffFile;   //inline;
         procedure CloseTiffFile;  //inline;

         constructor CreateGeotiff(Metadataonly : boolean; NoGeo : boolean; inFileName : PathStr; var Success : boolean; ShowHeader : boolean = false; GetHistogram : boolean = true; BandNum : integer = 0);
         function CreateTiffDEM(WantDEM : tDEMDataSet) : boolean;
         destructor Destroy; override;
         procedure GetTiffRow(Band,Row : integer; var TheRow : tRow8Bit);
         procedure GetPointReflectances(Column,Row : integer; var Reflectances : tAllRefs);
         procedure GetTiffRow16bit(Band,Row : integer; var Row16bit : tWordRow16Bit);

         procedure SeekFileOffset(Row : int64);
         {$IfDef VCL}
            procedure GetTIFFRowRGB(Row : integer;  var TheRow : tLongRGB);
            function DisplayInBitmap : tMyBitmap;
         {$EndIf}
   end;

   function GeoTIFFTagName(Tag : integer) : ShortString;
   function GeotiffTypeSize(ftype : integer) : integer;


procedure CaptureBMPInGeoTIFF(MapDraw : tMapDraw; FileName : PathStr; Image1 : tImage; MonoImage : boolean = false);
function BandsInGeotiff(fName : PathStr) : integer;
function GeotiffImageSize(fName : PathStr; var Width,Height : integer) : boolean;
function GeotiffBoundingBoxProj(fName : PathStr; var bb : sfBoundBox) : boolean;
function GeotiffBoundingBoxGeo(fName : PathStr; var bb : sfBoundBox) : boolean;

function GeotiffRegVars(fName : PathStr; var RegVars : tRegVars) : boolean;
function GeotiffCentroidLatLong(fName : PathStr; var Lat,Long : float64) : boolean;

procedure RewriteGeotiffIfRequired(var TIFFFileName : PathStr);
function GetGeotiffTag42112(fName : PathStr; var Tag : shortstring; var Tag42112Offset,Tag42112Length : int64) : boolean;
function Geotiff_UTMzone(fName : PathStr) : integer;

function Geotiff_VerticalDatum(fName : PathStr) : integer;


{$IfDef VCL}
var
   GeotiffImageDesc : String[255];
{$Else}
var
   GeotiffImageDesc : string;
{$EndIf}
var
   NeedToLoadGeotiffProjection,
   TemporaryNewGeotiff : boolean;


implementation

uses
   {$IfDef VCL}
      PetImage,
      PetDBUtils,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
      DEMEros,
   {$EndIf}

   {$IfDef ExAdvancedSats}
   {$Else}
      MultiGrid,
   {$EndIf}

   {$IfDef ExcludeExternalTools}
   {$Else}
      MD_Use_tools,
   {$EndIf}

   dem_nlcd,
   demesrishapefile,
   gdal_tools,
   PETMath;


function Geotiff_UTMzone(fName : PathStr) : integer;
var
   Lat,Long : float64;
begin
   Result := -99;
   if GeotiffCentroidLatLong(fName,Lat,Long) then begin
      Result := GetUTMZone(Long);
   end;
   {$IfDef RecordUTM} WriteLineToDebugFile('Geotiff ' + ExtractFileName(fName) + '  UTM zone=' + IntToStr(Result)); {$EndIf}
end;


function Geotiff_VerticalDatum(fName : PathStr) : integer;
var
   success : boolean;
   TiffImage : tTIFFImage;
begin
   Result := -99;
   if FileExists(fName) then begin
      TiffImage := tTiffImage.CreateGeotiff(true,false,fName,Success,false,false);
      Result := TiffImage.TiffHeader.VertDatum;
      TiffImage.Destroy;
   end;
end;


procedure RewriteGeotiffIfRequired(var TIFFFileName : PathStr);
var
  TiffImage : tTiffImage;
  Success : boolean;
begin
   {$IfDef RecordGeotiffRewrite} WriteLineToDebugFile('RewriteGeotiffIfRequired in for  ' + TIFFFileName); {$EndIf}
   TiffImage := tTiffImage.CreateGeotiff(true,false,TIFFFileName,success);
   {$IfDef RecordGeotiffRewrite} WriteLineToDebugFile('RewriteGeotiffIfRequired image created'); {$EndIf}
   if TiffImage.TiffMustBeRewritten then begin
      if (TiffImage.TiffHeader.BitsPerSample in [4]) then begin
         GDALConvert4BitGeotiff(TIFFFileName);
      end
      else begin
         if TemporaryNewGeotiff then begin
            GDALConvertSingleImageToGeotiff(TIFFFileName);
         end
         else begin
            {$IfDef RecordGeotiffRewrite} WriteLineToDebugFile('Calling GDALConvertImagesToGeotiff'); {$EndIf}
            GDALConvertImagesToGeotiff(TIFFFileName,true);
         end;
      end;
   end;
   TiffImage.Destroy;
   {$IfDef RecordGeotiffRewrite} WriteLineToDebugFile('RewriteGeotiffIfRequired out for  ' + TIFFFileName); {$EndIf}
end;


function tTIFFImage.TiledImage : boolean;
begin
   Result := (TiffHeader.TileWidth > 0) and (TiffHeader.TileHeight > 0) and ((TiffHeader.TileWidth < TiffHeader.ImageWidth) or (TiffHeader.TileHeight < TiffHeader.ImageLength))
end;


function tTIFFImage.TiffMustBeRewritten : boolean;
begin
   Result := TiledImage or (TiffHeader.Compression <> 1) or (TiffHeader.BitsPerSample in [4]);
end;


function GetGeotiffTag42112(fName : PathStr; var Tag : shortstring; var Tag42112Offset,Tag42112Length : int64) : boolean;
var
   success : boolean;
   TiffImage : tTIFFImage;
begin
   Result := FileExists(fName);
   if Result then begin
      TiffImage := tTiffImage.CreateGeotiff(true,false,fName,Success,false,false);
      Tag := TiffImage.Tag42112;
      Tag42112Offset := TiffImage.Tag42112Offset;
      Tag42112Length := TiffImage.Tag42112Length;
      Result := Tag <> '';
      TiffImage.Destroy;
   end;
end;


function GeotiffRegVars(fName : PathStr; var RegVars : tRegVars) : boolean;
var
   success : boolean;
   TiffImage : tTIFFImage;
begin
   Result := FileExists(fName);
   if Result then begin
      TiffImage := tTiffImage.CreateGeotiff(true,false,fName,Success,false,false);
      RegVars := TiffImage.RegVars;
      TiffImage.Destroy;
   end;
end;


function GeotiffBoundingBoxProj(fName : PathStr; var bb : sfBoundBox) : boolean;
var
   TiffImage : tTIFFImage;
   Success : boolean;
begin
   Result := FileExists(fName);
   if Result then begin
      TiffImage := tTiffImage.CreateGeotiff(true,false,fName,Success,false,false);
      bb.xmin := TiffImage.RegVars.UpleftX;
      bb.xmax := TiffImage.RegVars.UpleftX + pred(TiffImage.TiffHeader.ImageWidth) * TiffImage.RegVars.pr_deltaX;
      bb.ymin := TiffImage.RegVars.UpleftY - pred(TiffImage.TiffHeader.ImageLength) * TiffImage.RegVars.pr_deltaY;
      bb.ymax := TiffImage.RegVars.UpleftY;
      TiffImage.Destroy;
   end;
end;


function GeotiffBoundingBoxGeo(fName : PathStr; var bb : sfBoundBox) : boolean;
var
   TiffImage : tTIFFImage;
   Success : boolean;
begin
   Result := FileExists(fName);
   if Result then begin
      TiffImage := tTiffImage.CreateGeotiff(true,false,fName,Success,false,false);
      bb.xmin := TiffImage.RegVars.UpleftX;
      bb.xmax := TiffImage.RegVars.UpleftX + pred(TiffImage.TiffHeader.ImageWidth) * TiffImage.RegVars.pr_deltaX;
      bb.ymin := TiffImage.RegVars.UpleftY - pred(TiffImage.TiffHeader.ImageLength) * TiffImage.RegVars.pr_deltaY;
      bb.ymax := TiffImage.RegVars.UpleftY;
      if TiffImage.TiffHeader.ModelType <> 2 then begin
         TiffImage.MapProjection.InverseProjectDegrees(bb.xmin,bb.ymin,bb.ymin,bb.xmin);
         TiffImage.MapProjection.InverseProjectDegrees(bb.xmax,bb.ymax,bb.ymax,bb.xmax);
      end;
      TiffImage.Destroy;
   end;
end;



function GeotiffCentroidLatLong(fName : PathStr; var Lat,Long : float64) : boolean;
var
   bb : sfBoundBox;
begin
   Result := GeotiffBoundingBoxGeo(fName,bb);
   if Result then begin
      Lat := 0.5 * (bb.ymax + bb.ymin);
      Long := 0.5 * (bb.xmax + bb.xmin);
   end;
end;


function GeotiffImageSize(fName : PathStr; var Width,Height : integer) : boolean;
var
   success : boolean;
   TiffImage : tTIFFImage;
begin
   Result := FileExists(fName);
   if Result then begin
      TiffImage := tTiffImage.CreateGeotiff(true,false,fName,Success,false,false);
      Width := TiffImage.TiffHeader.ImageWidth;
      Height := TiffImage.TiffHeader.ImageLength;
      TiffImage.Destroy;
   end;
end;


function BandsInGeotiff(fName : PathStr) : integer;
var
  TiffImage : tTiffImage;
  Success : boolean;
begin
   TiffImage := tTiffImage.CreateGeotiff(false,false,fName,success);
   if Success then Result := TiffImage.TiffHeader.SamplesPerPixel
   else Result := 0;
   TiffImage.Destroy;
end;


function GeotiffTypeSize(ftype : integer) : integer;
begin
   case Ftype of
      1 : Result := 1; {byte}
      2 : Result := 1; {ASCII char}
      3 : Result := 2; {short, 2 byte unsigned integer}
      4 : Result := 4; {long, 4 byte unsigned integer}
      5 : Result := 8; {rational, two 4 byte integers}
     11 : Result := 4; {single IEEE floating point}
     12,16,17 : Result := 8; {double IEEE floating point, 8 byte integers}
   end;
end;


function TIFFTypeName(FType : integer) : ShortString;
begin
   case FType of
      1 : TIFFTypeName := ' Byte               ';
      2 : TIFFTypeName := ' ASCII char         ';
      3 : TIFFTypeName := ' unsigned 2 byte int';
      4 : TIFFTypeName := ' unsigned 4 byte int';
      5 : TIFFTypeName := ' rational, 2 4 byte ';
     11 : TIFFTypeName := ' single IEEE 4 byte ';
     12 : TIFFTypeName := ' double IEEE 8 byte ';
     16 : TIFFTypeName := ' unsigned 8 byte int';
     17 : TIFFTypeName := ' signed 8 byte int  ';
     else TIFFTypeName := ' other' + IntegerToString(FType,14);
   end;
end;


   procedure WriteLongInt(var TIFFFile : file; Value : LongInt);  inline;
   begin
      BlockWrite(TiffFile,Value,4);
   end;

   procedure WriteWord(var TIFFFile : file; Value : Word); inline;
   begin
      BlockWrite(TiffFile,Value,2);
   end;

   procedure WriteFieldEntry(var TIFFFile : File; Tag,TypeField : word; Length,Offset : LongInt);
   begin
      WriteWord(TIFFFile,Tag);
      WriteWord(TIFFFile,TypeField);
      WriteLongInt(TIFFFile,Length);
      WriteLongInt(TIFFFile,Offset);
      {$IfDef GeotiffSave} WriteLineToDebugFile('tag=' + IntegerToString(Tag,5) + '  type=' + TiffTypeName(TypeField) + '  Length=' + IntegerToString(Length,4) + '  Value=' + IntToStr(Offset)); {$EndIf}
   end;

   procedure WriteWordFieldEntry(var TIFFFile : File; Tag,TypeField,Length,Offset : word);
   begin
      WriteWord(TIFFFile,Tag);
      WriteWord(TIFFFile,TypeField);
      WriteWord(TiffFile,Length);
      WriteWord(TIFFFile,Offset);
      {$IfDef GeotiffSave} WriteLineToDebugFile('tag=' + IntegerToString(Tag,5) + '  type=' + TiffTypeName(TypeField) + '  Length=' + IntegerToString(Length,4) + '  Value=' + IntToStr(Offset)); {$EndIf}
   end;

   procedure WriteFieldEntryIncrementOffset(var TIFFFile : File; Tag,TypeField : word; Length : longint; var Offset : LongInt);
   begin
      WriteWord(TIFFFile,Tag);
      WriteWord(TIFFFile,TypeField);
      WriteLongInt(TIFFFile,Length);
      WriteLongInt(TIFFFile,Offset);
      {$IfDef GeotiffSave} WriteLineToDebugFile('tag=' +  IntegerToString(Tag,5) + '  type=' + TiffTypeName(TypeField) + '  Length=' + IntegerToString(Length,4) +
            '  Offset=' + IntToStr(Offset) + '  Bytes=' + IntToStr(Length * GeotiffTypeSize(TypeField)));
      {$EndIf}
      Offset := Offset + Length * GeotiffTypeSize(TypeField);
      //{$IfDef GeotiffSave} WriteLineToDebugFile('    Offset increased to Offset=' + IntToStr(Offset)); {$EndIf}
   end;


function tTIFFImage.ValidZ(z : float64) : boolean; //inline;
begin
   Result := (abs(z) < 1e30) and (abs(z - MDDef.GeotiffMissingValue) > 0.01) and (abs(z - CurrentMissing) > 0.01) and (not DEMDef_routines.MissingData(z)) ;
end;


procedure CaptureBMPInGeoTIFF(MapDraw : tMapDraw; FileName : PathStr; Image1 : tImage; MonoImage : boolean = false);
label
   Bored,CleanUp;
type
   tLongRun = array[0..3*MaxScreenXMax] of byte;
var
   LongRun : ^tLongRun;
   xutm,yutm,Lat,Long,dx,dy,
   Lat2,Long2,d : double;
   WField,ProjDatCode : word;
   NumFields,Offset, ASCIIsize,
   WidthImage,HeightImage,BytesPer,
   x,y : integer;
   TIFFHeader : tTiffHeader;
   b  : array[1..4] of byte;
   TIFFFile : file;
   Str34737 : shortstring;
   P0 : PRGB;
   RowBitmap : tMyBitmap;
begin
   {$IfDef GeotiffSave} WriteLineToDebugFile('TMapDraw.CaptureBMPInGeoTIFF in, for ' + FileName); {$EndIf}

   with TiffHeader do begin
      if (MapDraw <> Nil) then begin
         BitsPerSample := 8;
         BytesPer := 1;
         if MonoImage then PhotometricInterpretation := 1 else PhotometricInterpretation := 2;
         if MonoImage then SamplesPerPixel := 1 else SamplesPerPixel := 3;
         NumFields := 14;
         WidthImage := Image1.Width;
         HeightImage := Image1.Height;
         if MapDraw.IsThisMapUTM then Str34737 := 'Map from MICRODEM UTM'{+ #0 }
         else Str34737 := 'Map from MICRODEM' + #$A + 'Projection Geographic (Lat/Lon)' + #$A + 'Units = dd|OK'{ + #0};
         ASCIIsize := length(Str34737);
      end;

      assignFile(TiffFile,FileName);
      rewrite(TiffFile,1);
      {Byte Order}     B[1] := 73;   B[2] := 73;  BlockWrite(TiffFile,B,2);
      {TIFF version number}  B[1] := 42;   B[2] := 0;   BlockWrite(TiffFile,B,2);
      {Offset to first Image File Directory} writeLongInt(TIFFFile,8);
      {Number of fields} WriteWord(TIFFFile,NumFields);
      {width image} WriteFieldEntry(TIFFFile,256,3,1,WidthImage);
      {Length image} writeFieldEntry(TIFFFile,257,3,1,HeightImage);
      {Bits per sample} WriteFieldEntry(TIFFFile,258,3,1,BitsPerSample);
      {Compression} WriteFieldEntry(TIFFFile,259,3,1,1);
      {Photometric interpretation} writeFieldEntry(TIFFFile,262,3,1,PhotometricInterpretation);
      {StripOffsets}
         StripOffsets := NumFields*12 + 8 {TIFF version plus first offset} + 2 {NumFields} + 4 {0 ending offset};
         WriteFieldEntry(TIFFFile,273,4,1,StripOffsets);
      {SamplesPerPixel} writeFieldEntry(TIFFFile,277,3,1,SamplesPerPixel);
      {RowsPerStrip} writeFieldEntry(TIFFFile,278,4,1,HeightImage);
      {StripByteCounts}
         StripByteCounts := (WidthImage) * SamplesPerPixel * HeightImage * BytesPer;
         WriteFieldEntry(TIFFFile,279,4,1,StripByteCounts);
      {PlanarConfiguration} WriteFieldEntry(TIFFFile,284,3,1,1);

       Offset := StripOffsets + StripByteCounts;

      {start Geotiff}
       WriteFieldEntry(TIFFFile,33550,12,3,Offset);
       Offset := Offset + 3 * SizeOf(Double);
       WriteFieldEntry(TIFFFile,33922,12,6,Offset);
       Offset := Offset + 6 * SizeOf(Double);
       WriteFieldEntry(TIFFFile,34735,3,20,Offset);
       Offset := Offset + 20 * 2;
       WriteFieldEntry(TIFFFile,34737,2, ASCIIsize,Offset);
      {end Geotiff}
      {0 offset marking end} WriteLongInt(TIFFFile,0);

      if ShowSatProgress then StartProgress('Save ' + ExtractFileName(FileName));

      New(LongRun);
      CopyImageToBitMap(Image1,RowBitmap);
      for y := 0 to pred(RowBitmap.Height) do begin
         FillChar(LongRun^,SizeOf(LongRun^),0);
         P0 := RowBitMap.ScanLine[y];
         if MonoImage then begin
            for x := 0 to pred(RowBitmap.Width) do LongRun^[x] := ValidByteRange(round(0.3 * P0[x].rgbtRed  + 0.59 * P0[x].rgbtGreen  + 0.11 * P0[x].rgbtBlue));
         end
         else begin
            for x := 0 to pred(RowBitmap.Width) do begin
               LongRun^[(x)*3] := P0[x].rgbtRed;
               LongRun^[(x)*3+1] := P0[x].rgbtGreen;
               LongRun^[(x)*3+2] := P0[x].rgbtBlue;
            end;
         end;

         BlockWrite(TiffFile,LongRun^,(WidthImage * SamplesPerPixel));
         if (y mod 50 = 0) and ShowSatProgress then UpdateProgressBar(y/RowBitmap.Height);
      end {for y};
      RowBitmap.Free;
      Dispose(LongRun);

      if ShowSatProgress then EndProgress;

      MapDraw.ScreenToLatLongDegree(0,0,Lat,Long);  //get NW corner of map
      if MapDraw.IsThisMapUTM  then begin
         dx := MapDraw.ScreenPixelSize;
         dy := MapDraw.ScreenPixelSize;
      end
      else begin
         MapDraw.ScreenToLatLongDegree(MapDraw.MapXSize,MapDraw.MapYSize,Lat2,Long2);
         dx := (Long2 - Long) / pred(MapDraw.MapXSize);
         dy := (Lat - Lat2) / pred(MapDraw.MapYSize);
      end;

      {$IfDef RecordGeotiff} WriteLineToDebugFile('x space: ' +  RealToString(dx,-12,6) + '   y space: ' +  RealToString(dy,-12,6)); {$EndIf}

      BlockWrite(TiffFile,dx,SizeOf(Double));
      BlockWrite(TiffFile,dy,SizeOf(Double));

      d := 0;
      BlockWrite(TiffFile,d,SizeOf(Double));
   {ModelTiePointOffset, 33922}
      d := 0;
      BlockWrite(TiffFile,d,SizeOf(Double));
      BlockWrite(TiffFile,d,SizeOf(Double));
      BlockWrite(TiffFile,d,SizeOf(Double));

      if MapDraw.IsThisMapUTM  then  begin
         MapDraw.ScreenToUTM(0,0,xutm,yutm);
         BlockWrite(TiffFile,xutm,SizeOf(Double));
         BlockWrite(TiffFile,yutm,SizeOf(Double));
         {$IfDef RecordGeotiff} WriteLineToDebugFile('UTM, NW corner x=' + RealToString(xutm,-12,0) + '   & y=' + RealToString(yutm,-12,0)); {$EndIf}
      end
      else begin
         BlockWrite(TiffFile,Long,SizeOf(Double));
         BlockWrite(TiffFile,Lat,SizeOf(Double));
         {$IfDef RecordGeotiff} WriteLineToDebugFile('Lat/Long, NW corner x=' + RealToString(long,-12,6) + '   & y=' + RealToString(Lat,-12,6)); {$EndIf}
      end;

      BlockWrite(TiffFile,d,SizeOf(Double));
      WriteWordFieldEntry(TIFFFile,1,1,0,4);
      if MapDraw.IsThisMapUTM then wfield := 1 else wField := 2;
      WriteWordFieldEntry(TIFFFile,1024,0,1,wField);  {1 is for projection coords, 2 for Lat/long}
      WriteWordFieldEntry(TIFFFile,1025,0,1,1);       {1 RasterPixelIsArea, 2=RasterPixelIsPoint}
      WriteWordFieldEntry(TIFFFile,1026,34737,29,0);

       if MapDraw.IsThisMapUTM  then begin
          ProjDatCode := GetEPSGforUTMDatumCode(MapDraw.PrimMapProj);
          WriteWordFieldEntry(TIFFFile,3072,0,1,ProjDatCode);
       end
       else begin
          ProjDatCode := GetEPSGforGeoDatumCode(MapDraw.PrimMapProj);
          WriteWordFieldEntry(TIFFFile,2048,0,1,ProjDatCode);
       end;

      BlockWrite(TiffFile,Str34737[1],ASCIIsize);
    Bored:;
      closeFile(TiffFile);
    CleanUp:;
   end {with};
   {$IfDef GeotiffSave} WriteLineToDebugFile('TMapDraw.CaptureBMPInGeoTIFF out'); {$EndIf}
end;



procedure tTIFFImage.OpenTiffFile;
begin
   if (not TiffOpen) then begin
      if not FileExists(TIFFFileName) then begin
         TIFFFileName := ChangeFileExt(TIFFFileName,'.tif');
         Application.ProcessMessages;
      end;
      InsureFileIsNotReadOnly(TIFFFileName);
      TiffHandle := FileOpen(TIFFFileName,fmOpenRead);
      FileSeek(TiffHandle,0,0);
      TiffOpen := true;
   end;
end;


procedure tTIFFImage.CloseTiffFile;
begin
   if TiffOpen then FileClose(Tiffhandle);
   TiffOpen := false;
end;


{$IfDef VCL}

      procedure tTIFFImage.GetTIFFRowRGB(Row : integer; var TheRow : tLongRGB);
      var
         x : integer;
         TheRawRow : ^tRow8Bit;
      begin
         SeekFileOffset(Row);
         if (TiffHeader.PhotometricInterpretation in [1,2]) and (TiffHeader.SamplesPerPixel > 1) then begin
             if (TiffHeader.BitsPerSample in [15,16]) then begin
             //this has problems 8/13/23
               FileRead(TiffHandle,Row16bit^,ImageBytesPerRow);
               for x := 0 to pred(TiffHeader.ImageWidth) do begin
                   TheRow[x].rgbtRed := Row16bit^[x*TiffHeader.SamplesPerPixel] div 256;
                   TheRow[x].rgbtGreen := Row16bit^[x*TiffHeader.SamplesPerPixel+1] div 256;
                   TheRow[x].rgbtBlue := Row16bit^[x*TiffHeader.SamplesPerPixel+2] div 256;
               end;
            end
            else begin
                FileRead(TiffHandle,Row8Bit^,ImageBytesPerRow);
                for x := 0 to pred(TiffHeader.ImageWidth) do begin
                   TheRow[x].rgbtRed := Row8Bit^[x*TiffHeader.SamplesPerPixel];
                   TheRow[x].rgbtGreen := Row8Bit^[x*TiffHeader.SamplesPerPixel+1];
                   TheRow[x].rgbtBlue := Row8Bit^[x*TiffHeader.SamplesPerPixel+2];
                end;
            end;
         end
         else begin
            New(TheRawRow);
            GetTiffRow(1,Row,TheRawRow^);
            for x := 0 to TiffHeader.ImageWidth do TheRow[x] := TIFFImageColor[TheRawRow^[x]];
            Dispose(TheRawRow);
         end;
      end;


      function tTIFFImage.DisplayInBitmap : tMyBitmap;
      var
         x,y : integer;
         TheRow : tLongRGB;
         BMPMemory : tBMPMemory;
      begin
         {$IfDef RecordDisplayBitmat} WriteLineToDebugFile('tTIFFImage.DisplayInBitmap in, ' + IntToStr(TiffHeader.ImageWidth) + 'x' + IntToStr(TiffHeader.ImageLength)); {$EndIf}
         PetImage.CreateBitmap(Result,TiffHeader.ImageWidth,TiffHeader.ImageLength);
         BMPMemory := tBMPMemory.Create(Result);
         OpenTiffFile;
         for y := 0 to pred(TiffHeader.ImageLength) do begin
            GetTIFFRowRGB(y,TheRow);
            for x := 0 to pred(TiffHeader.ImageWidth) do BMPMemory.SetPixelColor(x,y,TheRow[x]);
         end;
         BMPMemory.Destroy;
         CloseTiffFile;
         {$IfDef RecordDisplayBitmat} WriteLineToDebugFile('tTIFFImage.DisplayInBitmap out'); {$EndIf}
      end;

{$EndIf}

function tTIFFImage.WordToByte(Band : integer; Row16bit : Word) : byte;
begin
   Result := ValidByteRange(round((Row16bit - TiffHeader.MinSampleValue[Band]) / (TiffHeader.MaxSampleValue[Band] - TiffHeader.MinSampleValue[Band]) * 255));
end;


function tTIFFImage.MakeDouble : Double;
var
   Run : array[1..8] of byte;
   v : DoubleBytes;
   i : integer;
begin
   FileRead(TiffHandle,v,8);
   if BigEndian then for i := 1 to 8 do Run[i] := v[9-i]
   else for i := 1 to 8 do Run[i] := v[i];
   Move(Run,Result,8);
end;


function tTIFFImage.MakeSingle : Single;
var
   Run : array[1..4] of byte;
   v : DoubleBytes;
   i : integer;
begin
   FileRead(TiffHandle,v,4);
   if BigEndian then for i := 1 to 4 do Run[i] := v[5-i]
   else for i := 1 to 4 do Run[i] := v[i];
   Move(Run,Result,4);
end;


function tTIFFImage.MakeWord : Word;
var
   v : array[1..2] of byte;
begin
   FileRead(TiffHandle,v,2);
   if BigEndian then begin
      Result := v[2] + 256 * v[1];
   end
   else begin
      Result := v[1] + 256 * v[2];
   end;
end;


function tTIFFImage.MakeLongInt : int32;
var
   Run : array[1..4] of byte;
   v : DoubleBytes;
   i : integer;
begin
   FileRead(TiffHandle,v,4);
   if BigEndian then for i := 1 to 4 do Run[i] := v[5-i]
   else for i := 1 to 4 do Run[i] := v[i];
   Move(Run,Result,4);
end;


function tTIFFImage.MakeLongWord : LongWord;
var
   Run : array[1..4] of byte;
   v : DoubleBytes;
   i : integer;
begin
   FileRead(TiffHandle,v,4);
   if BigEndian then for i := 1 to 4 do Run[i] := v[5-i]
   else for i := 1 to 4 do Run[i] := v[i];
   Move(Run,Result,4);
end;


function tTIFFImage.MakeUnsigned8Byte : int64;
var
   Run : array[1..8] of byte;
   v : DoubleBytes;
   i : integer;
begin
   FileRead(TiffHandle,v,8);
   if BigEndian then for i := 1 to 8 do Run[i] := v[9-i]
   else for i := 1 to 8 do Run[i] := v[i];
   Move(Run,Result,8);
end;


function tTIFFImage.MakeOffset : int64;
begin
   if BigTiff then Result := MakeUnsigned8Byte
   else Result := MakeLongWord;
end;


procedure tTIFFImage.SeekFileOffset(Row : int64);
var
   TheOffset,LinesNeeded : int64;
begin
   OpenTiffFile;
   if (TiffHeader.PhotometricInterpretation = 2) then begin  //color image
      if (TiffHeader.StripOffsets <> 0) then begin
         TheOffset := TiffHeader.StripOffsets + (Row * TiffHeader.ImageWidth * TiffHeader.BytesPerSample * TiffHeader.SamplesPerPixel);
      end
      else begin
         TheOffset := TiffHeader.OffsetArray^[(Row div TiffHeader.RowsPerStrip)];
      end;
   end
   else if (TiffHeader.RowsPerStrip = TiffHeader.ImageLength) then begin
      TheOffset := TiffHeader.StripOffsets + (Row * TiffHeader.ImageWidth * TiffHeader.BytesPerSample * TiffHeader.SamplesPerPixel);
   end
   else if (TiffHeader.RowsPerStrip = 1) then begin
      TheOffset := TiffHeader.OffsetArray^[(Row div TiffHeader.RowsPerStrip)];
   end
   else begin
      LinesNeeded := (Row mod TiffHeader.RowsPerStrip);
      TheOffset := TiffHeader.OffsetArray^[Row div TiffHeader.RowsPerStrip] + LinesNeeded * TiffHeader.ImageWidth * TiffHeader.BytesPerSample;
   end;
   FileSeek(TiffHandle,TheOffset,0);
   {$IfDef RecordGeotiffRow} if (Row mod 100 = 0) then WriteLineToDebugFile('Seek file offset, Band=' + IntToStr(Band) + '   Row=' + IntToStr(Row) + '   Offset=' + IntToStr(TheOffset)); {$EndIf}
end;


procedure tTIFFImage.GetPointReflectances(Column, Row: integer;  var Reflectances : tAllRefs);
var
   x : integer;
begin
   if (TiffHeader.BitsPerSample in [15,16]) and (TiffHeader.SamplesPerPixel > 1) then begin
      SeekFileOffset({1,}Row);
      FileRead(TiffHandle,BigRow^,ImageBytesPerRow);
      for x := 1 to TiffHeader.SamplesPerPixel do Reflectances[x] := BigRow^[Column*TiffHeader.SamplesPerPixel+ pred(x)];
      if BigEndian then for x := 0 to pred(TiffHeader.ImageWidth) do Reflectances[x] := swap(Reflectances[x]);
   end;
end;


procedure tTIFFImage.GetTiffRow16bit(Band,Row : integer; var Row16bit : tWordRow16Bit);
var
   x : integer;
begin
   SeekFileOffset(Row);
   if (TiffHeader.BitsPerSample in [15,16]) then begin
      if (TiffHeader.SamplesPerPixel > 1) then begin
         FileRead(TiffHandle,BigRow^,ImageBytesPerRow);
         for x := 0 to pred(TiffHeader.ImageWidth) do begin
            Row16Bit[x] := BigRow^[x*(TiffHeader.SamplesPerPixel) + pred(Band)];
         end;
      end
      else begin
         FileRead(TiffHandle,Row16bit,ImageBytesPerRow);
      end;
      if BigEndian then for x := 0 to pred(TiffHeader.ImageWidth) do Row16bit[x] := swap(Row16bit[x]);
   end
   else begin
      GetTiffRow(Band,Row,Row8Bit^);
      for x := 0 to TiffHeader.ImageWidth do Row16Bit[x] := Row8Bit^[x];
   end;
end;


procedure tTIFFImage.GetTiffRow(Band,Row : integer; var TheRow : tRow8Bit);
var
   NumRead,x,MemNeed  : integer;
begin
   SeekFileOffset(Row);
   if (TiffHeader.PhotometricInterpretation = 2) or (TiffHeader.SamplesPerPixel > 1) then begin    //color image
      if (TiffHeader.BitsPerSample in [15,16]) then begin
         GetTiffRow16bit(Band,Row,Row16bit^);
         for x := 0 to TiffHeader.ImageWidth do TheRow[x] := WordToByte(Band,Row16bit^[x]);
      end
      else begin
         MemNeed := TiffHeader.ImageWidth*TiffHeader.SamplesPerPixel;
         GetMem(Row8Bit,TiffHeader.ImageWidth*TiffHeader.SamplesPerPixel);
         NumRead := FileRead(TiffHandle,Row8Bit^,TiffHeader.ImageWidth*TiffHeader.SamplesPerPixel);
         if (NumRead = MemNeed) then begin
            for x := 0 to TiffHeader.ImageWidth do begin
               TheRow[x] := Row8Bit^[x*TiffHeader.SamplesPerPixel{+TiffHeader.ExtraSample} + pred(Band)];
            end;
         end;
         FreeMem(Row8Bit,TiffHeader.ImageWidth*TiffHeader.SamplesPerPixel);
      end;
   end
   else begin  //single band image
      if (TiffHeader.Compression = 1) then begin {uncompressed}
         if (TiffHeader.BitsPerSample = 8) then begin
            FileRead(TiffHandle,TheRow,TiffHeader.ImageWidth);
         end
         else if (TiffHeader.BitsPerSample in [15,16,32]) then begin
            GetTiffRow16bit(Band,Row,Row16bit^);
            for x := 0 to TiffHeader.ImageWidth do begin
               TheRow[x] := WordToByte(Band,Row16bit^[x]);
            end;
         end
         else if (TiffHeader.BitsPerSample = 4) then begin
           FileRead(TiffHandle,Row8Bit^,TiffHeader.ImageWidth div 2);
           for x := 0 to TiffHeader.ImageWidth div 2 do begin
               TheRow[2*x] := (Row8Bit^[x]) div 16;
               TheRow[succ(2*x)] := (Row8Bit^[x]) mod 16;
            end;
         end;
      end;
   end;
end;


procedure tTIFFImage.GetHistogramDBF(SingleFileBand : integer);
label
   out;
var
   Line : ANSIString;
   i,x,y,Band,Band2,Value,Start : integer;
   TotalPts : int64;
   cum,PC : float64;
   Hist  : array[1..25] of ^tWordValues;
   Results : tStringList;
   fName1 : PathStr;
   TheRow : tRow8Bit;
begin
   {$IfDef RecordGeotiffHistogram} WriteLineToDebugFile('tTIFFImage.GetHistogramDBF enter ' + ExtractFileName(TiffFileName)); {$EndIf}

   if (TiffHeader.Compression <> 1) or (not(TiffHeader.BitsPerSample in [8,15,16])) then begin
      exit;
   end;

   if (SingleFileBand = 0) then SingleFileBand := 1;

   {$IfDef RecordGeotiffHistogram} WriteLineToDebugFile('tTIFFImage.GetHistogramDBF ranges'); {$EndIf}

   fName1 := HistogramLandsatName(TIFFFileName);

   if not (FileExists(fName1)) then begin
      for I := 1 to TiffHeader.SamplesPerPixel do Hist[i] := Nil;
      SafeMakeDir(ExtractFilePath(fName1));
       StartProgress('Histogram ' + ExtractFileNameNoExt(TIFFFileName));
       for I := 1 to TiffHeader.SamplesPerPixel do begin
          New(Hist[i]);
          for x := 0 to MaxWord16 do Hist[i]^[x] := 0;
       end;

       if (TiffHeader.BitsPerSample in [15,16]) then begin
           for y := 0 to pred(TiffHeader.ImageLength) do begin
              if (y mod 500 = 0) then UpdateProgressBar(y/TiffHeader.ImageLength);
              for Band := 1 to TiffHeader.SamplesPerPixel do begin
                 GetTiffRow16bit(Band,y, Row16bit^);
                 for x := 0 to pred(TiffHeader.ImageWidth) do begin
                    inc(Hist[Band]^[Row16bit[x]]);
                 end;
              end;
           end;
       end
       else begin
           for y := 0 to pred(TiffHeader.ImageLength) do begin
              if (y mod 500 = 0) then UpdateProgressBar(y/TiffHeader.ImageLength);
              for Band := 1 to TiffHeader.SamplesPerPixel do begin
                 GetTiffRow(Band,y, Row8bit^);
                 for x := 0 to pred(TiffHeader.ImageWidth) do begin
                    inc(Hist[Band]^[Row8Bit[x]]);
                 end;
              end;
           end;
       end;

       {$IfDef RecordGeotiffHistogram} WriteLineToDebugFile('tTIFFImage.GetHistogramDBF computed'); {$EndIf}

       for Band := 1 to TiffHeader.SamplesPerPixel do begin
          TiffHeader.MinSampleValue[Band] := 0;
          while Hist[Band]^[TiffHeader.MinSampleValue[Band]] = 0 do inc(TiffHeader.MinSampleValue[Band]);
          TiffHeader.MaxSampleValue[Band] := MaxWord16;
          while Hist[Band]^[TiffHeader.MaxSampleValue[Band]] = 0 do dec(TiffHeader.MaxSampleValue[Band]);
       end;

      TotalPts := 0;
      for x := 1 to MaxWord16 do begin
         TotalPts := TotalPts + Hist[1]^[x];
      end;

      Results := tStringList.Create;
      Line := 'DN';
      if (TiffHeader.SamplesPerPixel > 1) then begin
         for Band := 1 to TiffHeader.SamplesPerPixel do Line := Line + ',BAND_' + IntToStr(Band);
      end
      else Line := Line + ',BAND_' + IntToStr(SingleFileBand) + ',PERCENT,CUM_PC';
      Results.Add(Line);

      if MDdef.IgnoreHistogramZero then Start := 1 else Start := 0;
      Cum := 0;
      for x := Start to MaxWord16 do begin
         for Band := 1 to TiffHeader.SamplesPerPixel do begin
            if (Hist[Band]^[x] > 0) then begin
               Line := IntToStr(x);
               for Band2 := 1 to TiffHeader.SamplesPerPixel do begin
                  Value := Hist[Band2]^[x];
                  Line := Line + ',' + IntToStr(Value);
               end;
               PC := 100.0 * value / TotalPts;
               Cum := Cum + PC;
               if (TiffHeader.SamplesPerPixel = 1) then Line := Line + ',' + RealToString(PC,-12,-6) + ',' + RealToString(cum,-15,-6);
               Results.Add(Line);
               goto out;
            end;
         end;
         out:;
      end;
      EndProgress;
      StringList2CSVtoDB(Results,fName1,true);
      {$IfDef RecordGeotiffHistogram} WriteLineToDebugFile('tTIFFImage.GetHistogramDBF written'); {$EndIf}
      for I := 1 to TiffHeader.SamplesPerPixel do Dispose(Hist[i]);
   end;
  {$IfDef RecordGeotiffHistogram} WriteLineToDebugFile('tTIFFImage.GetHistogramDBF exit') {$EndIf}
end;


function tTIFFImage.CreateTiffDEM(WantDEM : tDEMDataSet) : boolean;

         function InitializeTiffDEM(WantDEM : tDEMDataSet; ForceType : boolean = false; TypeWanted : tDEMprecision = ByteDEM) : boolean;
         var
            LandCover : integer;


            procedure SetCornersAndSpacing;
            begin
              {$If Defined(RecordInitDEM) or Defined(GeotiffCorner)}
                 HighlightLineToDebugFile(WantDEM.AreaName);
                 WriteLineToDebugFile('ScaleX=' + RealToString(TiffHeader.ScaleX,-18,-6) + ' ScaleY=' + RealToString(TiffHeader.ScaleY,-18,-6) +
                      ' ModelX=' + RealToString(TiffHeader.ModelX,-18,-6) + ' ModelY=' + RealToString(TiffHeader.ModelY,-18,-6) +
                      ' RasterX=' + RealToString(TiffHeader.RasterX,-18,-6) + ' RasterY=' + RealToString(TiffHeader.RasterY,-18,-6) );
                 WriteLineToDebugFile('Cols=' + IntToStr(WantDEM.DEMheader.NumCol) + ' Rows=' + IntToStr(WantDEM.DEMheader.NumRow) + '  ModelType=' + IntToStr(TiffHeader.ModelType));
                 WriteLineToDebugFile('UTMzone=' + IntToStr(WantDEM.DEMheader.UTMzone));
              {$EndIf}

              WantDEM.DEMheader.DEMxSpacing := TiffHeader.ScaleX;
              WantDEM.DEMheader.DEMySpacing := TiffHeader.ScaleY;

              if ANSIcontainsText(UpperCase(OriginalFileName),'COAST') then begin
                 if abs(WantDEM.DEMheader.DEMxSpacing - 1/3600) < 0.001 then WantDEM.DEMheader.DEMxSpacing := 1/3600;
                 if abs(WantDEM.DEMheader.DEMySpacing - 1/3600) < 0.001 then WantDEM.DEMheader.DEMySpacing := 1/3600;
              end;

              WantDEM.DEMheader.SWCornerX := TiffHeader.ModelX;
              {$IfDef GeotiffCorner} WriteLineToDebugFile('Read Geotiff DEM,  NW Corner X=' + RealToString(TiffHeader.ModelX,-18,-6) + '  Y=' + RealToString(TiffHeader.ModelY,-18,-6)); {$EndIf}

              WantDEM.GeotiffNWCornerX := TiffHeader.ModelX;
              WantDEM.GeotiffNWCornerY := TiffHeader.ModelY;

              if (TiffHeader.Orientation in [1,4]) then begin
                 //as defined, it is NW corner of upper left cell; we want to transform to SW corner of lower left cell
                 WantDEM.DEMheader.SWCornerY := TiffHeader.ModelY - (pred(WantDEM.DEMheader.NumRow) * WantDEM.DEMheader.DEMySpacing);
                 //for pixel-is-area or undefined this makes the corner the SW corner of the pixel
              end
              else begin
                 // this is probably not completely correct, but I don't think it ever happens
                 WantDEM.DEMheader.SWCornerY := TiffHeader.ModelY;
              end;
              {$IfDef TrackDEMCorners} WantDEM.WriteDEMCornersToDebugFile('Defined the Geotiff'); {$EndIf}
              {$IfDef GeotiffCorner} WriteLineToDebugFile('Read Geotiff DEM,  SW corner  X=' + RealToString(WantDEM.DEMheader.DEMSWCornerX,-18,-6) + '  Y=' + RealToString(WantDEM.DEMheader.DEMSWCornerY,-18,-6)); {$EndIf}
            end {SetCornersAndSpacing};


               procedure NLCDOptions;
               begin
                  {$IfDef RecordNLCD} WriteLineToDebugFile('Geotiff DEM with NLCD=' + LandCover); {$EndIf}
                  WantDEM.DEMheader.ElevUnits := LandCover;
                  if (WantDEM.DEMheader.ElevUnits = euGLC2000) then begin
                     WantDEM.DEMheader.DEMUsed := ArcSecDEM;
                     TiffHeader.ModelType := 2;
                  end;
               end;

               procedure DefineProjectionParameters;
               begin
                  {$If Defined(RecordGeotiffProjection) or Defined(RecordProjProgress)} MapProjection.WriteProjectionSummaryToDebugFile('DefineProjectionParameters, TiffFile=' ); {$EndIf}
                  WantDEM.DEMMapProj.PName := MapProjection.pName;
                  WantDEM.DEMHeader.h_DatumCode := MapProjection.h_DatumCode;
                  {$If Defined(Record_h_datum_code)} if MapProjection.h_DatumCode = '' then MessageToContinue('DefineProjectionParameters h_datum_code blank'); {$EndIf}
                  WantDEM.DEMheader.UTMZone := MapProjection.projUTMZone;
                  WantDEM.DEMheader.LatHemi := MapProjection.LatHemi;
                  WantDEM.DEMheader.wktString := MapProjection.wktString;
                  WantDEM.DEMMapProj.wktString := MapProjection.wktString;
                  WantDEM.DEMMapProj.GeographicTypeGeoKey2048 := MapProjection.GeographicTypeGeoKey2048;
                  WantDEM.DEMMapProj.ProjectedCSTypeGeoKey3072 := MapProjection.ProjectedCSTypeGeoKey3072;
                  {$If Defined(TrackWKTstring)} WriteLineToDebugFile('DefineProjectionParameters, Map wkt=' + IntToStr(Length(MapProjection.wktString))); {$EndIf}
                  {$If Defined(TrackWKTstring)} WriteLineToDebugFile('DefineProjectionParameters, DEM wkt=' + IntToStr(Length(WantDEM.DEMMapProj.wktString))); {$EndIf}

                  if (WantDEM.DEMMapProj.PName = UK_OS) then begin
                     //this is likely not to work
                     {$IfDef RecordUKOS} WriteLineToDebugFile('DefineProjectionParameters Loading WKT for osgb_1936'); {$EndIf}
                     WantDEM.DEMMapProj.InitProjFromWKTfile(ProgramRootDir + 'wkt_proj\osgb_1936.wkt');
                     WantDEM.DEMheader.DEMUsed := UTMBasedDEM;
                     WantDEM.DEMheader.DataSpacing := SpaceMeters;
                     WantDEM.DEMheader.h_DatumCode := 'UK-OS';
                     {$IfDef RecordUKOS} WriteLineToDebugFile('DefineProjectionParameters Loading WKT, pName=' + WantDEM.DEMMapProj.GetProjName); {$EndIf}
                  end
                  else begin
                     if (WantDEM.DEMMapProj.wktString <> '') or  WantDEM.DEMMapProj.ProjectionUsingWKT then begin
                        {$If Defined(RecordGeotiffProjection) or Defined(RecordProjProgress)} WriteLineToDebugFile('DefineProjectionParameters, DEM map WKT string=' + IntToStr(length(WantDEM.DEMMapProj.wktString))); {$EndIf}
                        WantDEM.DEMheader.DEMUsed := WKTDEM;
                        WantDEM.DEMheader.DataSpacing := SpaceMeters;
                        WantDEM.DEMMapProj.InitProjFromWKTstring(WantDEM.DEMMapProj.wktString);
                        {$If Defined(RecordGeotiffProjection) or Defined(RecordProjProgress)} WantDEM.DEMMapProj.WriteProjectionSummaryToDebugFile('DefineProjectionParameters, after WKT string'); {$EndIf}
                     end
                     else if (WantDEM.DEMMapProj.PName = PlateCaree) or (TiffHeader.ModelType = 2) then begin
                        WantDEM.DEMheader.DEMUsed := ArcSecDEM;
                        if (WantDEM.DEMheader.SWCornerX > 180) then WantDEM.DEMheader.SWCornerX := WantDEM.DEMheader.SWCornerX - 360;
                        WantDEM.DEMheader.UTMZone := GetUTMZone(WantDEM.DEMheader.SWCornerX + 0.5 * WantDEM.DEMheader.NumCol * WantDEM.DEMheader.DEMxSpacing);
                     end
                     else if (WantDEM.DEMMapProj.PName = UTMEllipsoidal) then begin
                        WantDEM.DEMheader.DEMUsed := UTMBasedDEM;
                        WantDEM.DEMheader.DataSpacing := SpaceMeters;
                     end
                     else begin
                        WantDEM.DEMheader.DEMUsed := UTMBasedDEM;
                        WantDEM.DEMheader.DataSpacing := SpaceMeters;
                        WantDEM.DEMheader.h_DatumCode := 'Rect';
                        {$IfDef RecordInitDEM} WriteLineToDebugFile('DEM SW Corner: ' + RealToString(WantDEM.DEMheader.SWCornerX,-18,-6) + RealToString(WantDEM.DEMheader.SWCornerY,18,-6) + '  UTM zone:' + IntToStr(WantDEM.DEMheader.UTMzone)); {$EndIf}
                     end;
                     //if (WantDEM.DEMMapProj.h_DatumCode <> '') then WantDEM.DEMheader.aDigitizeDatum := DatumCodeFromString(WantDEM.DEMMapProj.h_DatumCode);
                     WantDEM.DEMMapProj.InitProjFomDEMHeader(WantDEM.DEMHeader);
                     WantDEM.DEMheader.UTMZone := WantDEM.DEMMapProj.projUTMZone;
                     WantDEM.DEMheader.LatHemi := WantDEM.DEMMapProj.LatHemi;
                  end;
                  {$If Defined(RecordGeotiffProjection) or Defined(RecordUKOS)} WriteLineToDebugFile('DefineProjectionParameters out, DEM proj=' + WantDEM.DEMMapProj.GetProjName); {$EndIf}
                  {$IfDef RecordprojProgress} WantDEM.DEMMapProj.WriteProjectionSummaryToDebugFile('DefineProjectionParameters out: '); {$EndIf}
                  {$If Defined(TrackWKTstring)} WriteLineToDebugFile('DefineProjectionParameters out, DEM wkt=' + IntToStr(Length(WantDEM.DEMMapProj.wktString))); {$EndIf}
              end {DefineProjectionParameters};


         begin {InitializeTiffDEM}
            {$IfDef TrackA} WriteLineToDebugFile('InitializeTiffDEM in, a=' + RealToString(WantDEM.DEMMapProj.a,-18,-2)); {$EndIf}
            WantDEM.AreaName := ExtractFileNameNoExt(TIFFFileName);
            if (Uppercase(ptCopy(WantDEM.AreaName,1,2)) = 'LF') and (WantDEM.Areaname[3] in ['0'..'9']) then begin
               WantDEM.AreaName := Petmar.LastSubDir(TIFFFileName);
            end;

            WantDEM.DEMheader.NumCol := TiffHeader.ImageWidth;
            WantDEM.DEMheader.NumRow := TiffHeader.ImageLength;
            WantDEM.DEMHeader.RasterPixelIsGeoKey1025 := TiffHeader.RasterPixelIs;
            WantDEM.DEMheader.VerticalCSTypeGeoKey := TiffHeader.VertDatum;
            {$IfDef TrackVerticalDatum} WriteLineToDebugFile('Geotiff read ' + WantDEM.AreaName + ' vdatum=' + IntToStr(WantDEM.DEMheader.VerticalCSTypeGeoKey)); {$EndIf}


            //if (WantDEM.AreaName = 'GEDTMV1_2') then TiffHeader.MDZtype := euDecimeters;

            if TiffHeader.MDZtype = euCentimeters then begin
               TiffHeader.MDZtype := euMeters;
               TiffHeader.Factor := 0.01;
            end;

            if TiffHeader.MDZtype = euDecimeters then begin
               TiffHeader.MDZtype := euMeters;
               TiffHeader.Factor := 0.1;
            end;

            WantDEM.DEMheader.ElevUnits := TiffHeader.MDZtype; //tElevUnit(TiffHeader.MDZtype);
            {$IfDef TrackZ} WriteLineToDebugFile('ElevUnits=' + ElevUnitsAre(WantDEM.DEMheader.ElevUnits) + '  TiffHeader.Factor=' + RealToString(TiffHeader.Factor,-8,-2) ); {$EndIf}

            {$IfDef RecordInitDEM} WriteLineToDebugFile('WantDEM.DEMHeader.RasterPixelIsGeoKey1025=' + IntToStr(WantDEM.DEMHeader.RasterPixelIsGeoKey1025) ); {$EndIf}

            if ForceType then begin
               WantDEM.DEMheader.DEMPrecision := TypeWanted;
            end
            else begin
               if (TiffHeader.BitsPerSample in [4,8]) then WantDEM.DEMheader.DEMPrecision := ByteDEM
               else if (TiffHeader.BitsPerSample = 16) then begin
                  if (TiffHeader.SampleFormat = sfWord) then WantDEM.DEMheader.DEMPrecision := WordDEM
                  else WantDEM.DEMheader.DEMPrecision := SmallIntDEM;
               end
               else if (TiffHeader.BitsPerSample in [32,64]) then begin
                  WantDEM.DEMheader.DEMPrecision := FloatingPointDEM;
               end;
            end;

            SetCornersAndSpacing;
            DefineProjectionParameters;

           {$If Defined(RecordInitDEM) or Defined(RecordUKOS)}
              WriteLineToDebugFile('after define proj, ' + WantDEM.GridDefinition + ' pName=' + WantDEM.DEMMapProj.GetProjName);
           {$EndIf}
           {$If Defined(RecordInitDEM)} WriteLineToDebugFile('tTIFFImage.CreateDEM Header set, ' + sfBoundBoxToString(WantDEM.DEMBoundBoxProjected,6)); {$EndIf}

            if IsThisLandCover(TIFFFileName,LandCover) then begin
               NLCDOptions;
            end;

            {$IfDef RecordInitDEM} WriteLineToDebugFile('Call define DEM variables ' + WantDEM.AreaName + '  ' + sfBoundBoxToString(WantDEM.DEMBoundBoxProjected,4)); {$EndIf}
            WantDEM.DefineDEMvariables(true);
            {$IfDef RecordInitDEM} WriteLineToDebugFile('Back from define DEM variables '  + WantDEM.AreaName + '  ' + sfBoundBoxToString(WantDEM.DEMBoundBoxProjected,4)); {$EndIf}
            {$If Defined(RecordUKOS)} WriteLineToDebugFile('After DefineDEMvariable, pName=' + WantDEM.DEMMapProj.GetProjName); {$EndIf}

            Result := true;
            if ReallyReadDEM and (not WantDEM.AllocateDEMMemory(InitDEMnone)) then begin
               {$IfDef RecordInitDEM} WriteLineToDebugFile('WantDEM.AllocateDEMMemory failed'); {$EndIf}
               Result := false;
            end;

            {$IfDef TrackA} WriteLineToDebugFile('tTIFFImage.CreateTiffDEM out, a=' + RealToString(WantDEM.DEMMapProj.a,-18,-2)); {$EndIf}
            {$If Defined(RecordInitDEM) or Defined(RecordDEMMapProj)} WantDEM.DEMMapProj.ShortProjInfo('tTIFFImage.InitializeDEM in'); {$EndIf}
            {$IfDef RecordNLCD} WriteLineToDebugFile('Initialize TIFF DEM out, ' + WantDEM.AreaName + '  data=' + ElevUnitsAre(WantDEM.DEMheader.ElevUnits)); {$EndIf}
            {$If Defined(RecordUKOS)} WriteLineToDebugFile('Initialize TIFF DEM out, pName=' + WantDEM.DEMMapProj.GetProjName); {$EndIf}
            {$IfDef RecordprojProgress} WantDEM.DEMMapProj.WriteProjectionSummaryToDebugFile('DefineProjectionParameters Initialize TIFF DEM out: '); {$EndIf}
            {$If Defined(TrackWKTstring)} WriteLineToDebugFile('Initialize TIFF DEM out, DEM wkt=' + IntToStr(Length(WantDEM.DEMMapProj.wktString))); {$EndIf}
         end {InitializeTiffDEM};

var
   FloatRow : ^tFloatRow;
   DoubleRow : ^tDoubleRow;
   IntRow : ^tIntRow;
   WordRow : ^tWordRow;
   ByteRow : ^tByteRow;
   Int32Row : ^tInt32Row;
   bs,dRow,Col,Row,RecsRead,rc : int32;
   zi : smallInt;
   zw : word;
   zb : byte;
   z : float32;
begin {tTIFFImage.CreateTiffDEM}
   {$If Defined(RecordGeotiff) or Defined(RecordInitDEM) or Defined(RecordModelType) or Defined(RecordDefineDatum) or Defined(RecordGeotiffProjection)} WriteLineToDebugFile('tTIFFImage.CreateDEM in, modelType=' + IntToStr(TiffHeader.ModelType)); {$EndIf}
   {$IfDef RecordInitDEM} WriteLineToDebugFile('ModelX=' + RealToString(TiffHeader.ModelX,-18,-6) + ' ModelY=' + RealToString(TiffHeader.ModelY,-18,-6) ); {$EndIf}
      if (TiffHeader.SamplesPerPixel > 1) then begin
         MessageToContinue('File has ' + IntToStr(TiffHeader.SamplesPerPixel) + ' bands and is not a DEM; open as an image');
         Result := false;
         exit;
      end;
      Result := InitializeTiffDEM(WantDEM);
      {$If Defined(RecordGeotiffProjection) or Defined(RecordUKOS)} WriteLineToDebugFile('After InitializeTiffDEM back, pname=' + WantDEM.DEMMapProj.GetProjName); {$EndIf}
      WantDEM.GeotiffImageDesc := GeotiffImageDesc;
      {$If Defined(TrackHorizontalDatum)} WriteLineToDebugFile('tTIFFImage.CreateDEM read DEM, ' + WantDEM.AreaName + '  ' + WantDEM.DEMMapProj.h_DatumCode);   {$EndIf}
      if Result and ReallyReadDEM then begin
         {$If Defined(RecordInitDEM)} WriteLineToDebugFile('tTIFFImage.CreateDEM initialization done and reading DEM'); {$EndIf}
         if (WantDEM.AreaName = 'EXISTING_VEGETATION_HEIGHT') or (WantDEM.AreaName = 'CANOPY_BASE_HEIGHT') or (WantDEM.AreaName = 'CANOPY_HEIGHT') then TiffHeader.Factor := 0.1;
         if (WantDEM.DEMheader.NumRow > 10000) then ShowDEMReadingProgress := true;
         if ShowDEMReadingProgress then StartProgress('Read grid: ' + ExtractFileNameNoExt(TIFFFileName));

         {$If Defined(TrackZ)} WriteLineToDebugFile('TiffHeader.Factor=' + RealToString(TiffHeader.Factor,-12,-2)); {$EndIf}

         bs := TiffHeader.BytesPerSample * WantDEM.DEMheader.NumCol;
         if (TiffHeader.BitsPerSample = 64) then New(DoubleRow)
         else if (TiffHeader.BitsPerSample = 32) then begin
            if (TiffHeader.SampleFormat = sfIEEEfloat) then New(FloatRow)
            else New(Int32Row);
         end
         else if (TiffHeader.BitsPerSample in [15,16]) then begin
            if WantDEM.DEMheader.DEMPrecision = SmallIntDEM then New(IntRow) else New(WordRow);
         end
         else if (TiffHeader.BitsPerSample in [4,8]) then begin
            New(ByteRow);
            if (TiffHeader.BitsPerSample in [4]) then bs := WantDEM.DEMheader.NumCol div 2;
         end;

         {$If Defined(TrackZ)} WantDEM.TrackElevationRange('start Geotiff read'); {$EndIf}

         OpenTiffFile;
         if (TiffHeader.OffsetArray = Nil) then FileSeek(TiffHandle,TiffHeader.StripOffsets,0);

         rc := ProgressIncrement(WantDEM.DEMheader.NumRow);
         for Row := 0 to pred(WantDEM.DEMheader.NumRow) do  begin
            if (Row mod rc = 0) and ShowDEMReadingProgress then begin
               UpdateProgressBar(Row/WantDEM.DEMheader.NumRow);
               {$If Defined(FullDEMinit)} WriteLineToDebugFile('Row: ' + IntToStr(Row) + '/' + IntToStr(WantDEM.DEMheader.NumRow)); {$EndIf}
            end;
            if (TiffHeader.Orientation = 1) then begin
               dRow := pred(WantDEM.DEMheader.NumRow) - Row;
            end
            else dRow := Row;

            if (TiffHeader.OffsetArray <> Nil) and (Row mod TiffHeader.RowsPerStrip = 0) then begin
               FileSeek(TiffHandle,TiffHeader.OffsetArray^[(Row div TiffHeader.RowsPerStrip)],0);
               {$If Defined(FullDEMinit)} WriteLineToDebugFile('Seek: ' + IntToStr(Row div TiffHeader.RowsPerStrip) + '/' + IntToStr(WantDEM.DEMheader.NumRow)); {$EndIf}
            end;

            try
               if (TiffHeader.BitsPerSample = 32) then begin
                  if (TiffHeader.SampleFormat = sfIEEEfloat) then begin
                     RecsRead := FileRead(TiffHandle,FloatRow^,bs);
                     for Col := 0 to pred(WantDEM.DEMheader.NumCol) do begin
                        z := FloatRow^[Col];
                        if BigEndian then SwapToShortFloat(z);
                        if ValidZ(z) then begin
                           z := z * TiffHeader.Factor;
                           WantDEM.SetGridElevation(Col,dRow,z);
                        end
                        else WantDEM.SetGridMissing(Col,dRow);
                     end;
                  end
                  else begin
                     RecsRead := FileRead(TiffHandle,Int32Row^,bs);
                     for Col := 0 to pred(WantDEM.DEMheader.NumCol) do begin
                        z := Int32Row^[Col];
                        if ValidZ(z) then begin
                           z := z * TiffHeader.Factor;
                           WantDEM.SetGridElevation(Col,dRow,z);
                        end
                        else WantDEM.SetGridMissing(Col,dRow);
                     end;
                  end;
               end
               else if (TiffHeader.BitsPerSample in [15,16]) then begin
                  if (WantDEM.DEMheader.DEMPrecision = SmallIntDEM) then begin
                     RecsRead := FileRead(TiffHandle,IntRow^,bs);
                     for Col := 0 to pred(WantDEM.DEMheader.NumCol) do begin
                        if BigEndian then zi := Swap(IntRow^[Col])
                        else zi := IntRow^[Col];
                        if ValidZ(zi) then begin
                           WantDEM.SetGridElevation(Col,dRow,zi);
                        end
                        else WantDEM.SetGridMissing(Col,dRow);
                     end;
                  end
                  else begin
                     RecsRead := FileRead(TiffHandle,WordRow^,bs);
                     for Col := 0 to pred(WantDEM.DEMheader.NumCol) do begin
                        if BigEndian then zw := Swap(WordRow^[Col])
                        else zw := WordRow^[Col];
                        if ValidZ(zw) then begin
                           WantDEM.SetGridElevation(Col,dRow,zw);
                        end
                        else WantDEM.SetGridMissing(Col,dRow);
                     end;
                  end;
               end
               else if (TiffHeader.BitsPerSample = 64) then begin
                  RecsRead := FileRead(TiffHandle,DoubleRow^,bs);
                  for Col := 0 to pred(WantDEM.DEMheader.NumCol) do begin
                     z := DoubleRow^[Col];
                     if ValidZ(z) then begin
                        z := z * TiffHeader.Factor;
                        WantDEM.SetGridElevation(Col,dRow,z);
                     end
                     else WantDEM.SetGridMissing(Col,dRow);
                  end;
               end
               else if (TiffHeader.BitsPerSample in [8]) then begin
                  RecsRead := FileRead(TiffHandle,ByteRow^,bs);
                  for Col := 0 to pred(WantDEM.DEMheader.NumCol) do begin
                     zb := ByteRow^[Col];
                     WantDEM.SetGridElevation(Col,dRow,zb);
                  end;
               end
               else if (TiffHeader.BitsPerSample in [4]) then begin
                  RecsRead := FileRead(TiffHandle,ByteRow^,bs div 2);
                  for Col := 0 to pred(WantDEM.DEMheader.NumCol) div 2 do begin
                     zb := ByteRow^[Col] div 16;
                     WantDEM.SetGridElevation(2*Col,dRow,zb);
                     zb := ByteRow^[Col] mod 16;
                     WantDEM.SetGridElevation(succ(2*Col),dRow,zb);
                  end;
               end;
            except
                on exception do Result := false;
            end;
         end;

         if (TiffHeader.BitsPerSample = 64) then Dispose(DoubleRow)
         else if (TiffHeader.BitsPerSample = 32) then begin
            if (TiffHeader.SampleFormat <> sfIEEEfloat) then Dispose(Int32Row);
            if (TiffHeader.SampleFormat = sfIEEEfloat) then Dispose(FloatRow);
         end
         else if (TiffHeader.BitsPerSample in [15,16]) then begin
            if (WantDEM.DEMheader.DEMPrecision = SmallIntDEM) then Dispose(IntRow) else Dispose(WordRow);
         end
         else Dispose(ByteRow);

         CloseTiffFile;
         if ShowDEMReadingProgress then EndProgress;

         if (TiffHeader.SMax > TiffHeader.SMin) then begin
            WantDEM.DEMheader.MaxElev := TiffHeader.SMax;
            WantDEM.DEMheader.MinElev := TiffHeader.SMin;
         end
         else WantDEM.CheckMaxMinElev;
         {$If Defined(RecordFullGeotiff) or Defined(ShowKeyDEM) or Defined(TrackZ) or Defined(RecordUKOS)} WantDEM.TrackElevationRange('Geotiff DEM CheckMaxMinElev over '); {$EndIf}
      end;
   {$If Defined(RecordGeotiff) or Defined(RecordInitDEM)} WriteLineToDebugFile('tTIFFImage.CreateDEM out, ' + sfBoundBoxToString(WantDEM.DEMBoundBoxProjected,4)); {$EndIf}
   {$If Defined(RecordDefineDatum) or Defined(RecordGeotiff) or Defined(RecordFullGeotiff) or Defined(RecordGeotiffProjection) or Defined(RecordUKOS)}
      WriteLineToDebugFile('tTIFFImage.CreateDEM out, DEM=' + WantDEM.DEMMapProj.GetProjName);
   {$EndIf}
   {$If Defined(RecordInitDEMName)} WriteLineToDebugFile('Done Geotiff DEM read ' + WantDEM.AreaName); {$EndIf}

   {$If Defined(RecordDEMMapProj) or Defined(RecordInitDEM) or Defined(TrackProjection)}
      WantDEM.DEMMapProj.ProjectionParamsToDebugFile('SetUpDefaultNewProjection out');
   {$EndIf}
   {$If Defined(TrackHorizontalDatum)} WriteLineToDebugFile('tTIFFImage.CreateDEM out, ' + WantDEM.AreaName + '  DEM=' + WantDEM.DEMMapProj.h_DatumCode); {$EndIf}
   {$If Defined(TrackWKTstring)} WriteLineToDebugFile('tTIFFImage.CreateDEM out, DEM wkt=' + IntToStr(Length(WantDEM.DEMMapProj.wktString))); {$EndIf}
end  {tTIFFImage.CreateTiffDEM};


function TIFFTagName(Tag : integer) : shortstring;
begin
   case Tag of
      254 : Result := 'NewSubfileType';
      255 : Result := 'SubfileType';
      256 : Result := 'ImageWidth';
      257 : Result := 'ImageLength';
      258 : Result := 'BitsPerSample';
      259 : Result := 'Compression';
      262 : Result := 'PhotometricInterpretation';
      263 : Result := 'ThreshHolding';
      264 : Result := 'CellWidth';
      265 : Result := 'CellLength';
      266 : Result := 'FillOrder';
      270 : Result := 'ImageDescription';
      273 : Result := 'StripOffsets';
      274 : Result := 'Orientation';
      277 : Result := 'SamplesPerPixel';
      278 : Result := 'RowsPerStrip';
      279 : Result := 'StripByteCounts';
      280 : Result := 'MinSampleValue';
      281 : Result := 'MaxSampleValue';
      282 : Result := 'xResolutionOffset';
      283 : Result := 'yResolutionOffset';
      284 : Result := 'PlanarConfiguration';
      286 : Result := 'XPosition';
      287 : Result := 'YPosition';
      296 : Result := 'ResolutionUnit';
      305 : Result := 'Software';
      306 : Result := 'DateTime';
      317 : Result := 'Predictor';
      320 : Result := 'ColorTableOffset';
      322 : Result := 'Tile width';
      323 : Result := 'Tile length';
      324 : Result := 'Tile offsets';
      325 : Result := 'Tile byte counts';
      338 : Result := 'Extra samples';
      339 : Result := 'Sample format';
      340 : Result := 'SMinSampleValue';
      341 : Result := 'SMaxSampleValue';
      33550 : Result := 'Geotiff ModelPixelScaleOffset';
      33922 : Result := 'Geotiff ModelTiePointOffset';
      34264 : Result := 'JPL Carto Geotiff ModelTransformationTag (2.6.1)';
      34735 : Result := 'Geotiff GeoKeyDirectoryOffset';
      34736 : Result := 'Geotiff GeoDoubleParamsTag';
      34737 : Result := 'Geotiff GeoASCIIParams';
      42112 : Result := 'Metadata (GDAL)';
      42113 : Result := 'Missing data (GDAL)';
      else Result := 'Unspecified';
   end {case};
end;

function GeoTIFFTagName(Tag : integer) : ShortString;
begin
   case Tag of
         1 : Result := 'GeoKeyHeader';
      1024 : Result := 'GTModelTypeGeoKey (6.3.1.1 codes)';
      1025 : Result := 'GTRasterTypeGeoKey (6.3.1.2 codes)';
      1026 : Result := 'GTCitationGeoKey (ASCII)';
      2048 : Result := 'GeographicTypeGeoKey (6.3.2.1 codes)';
      2049 : Result := 'GeogCitationGeoKey (ASCII)';
      2050 : Result := 'GeogGeodeticDatumGeoKey (6.3.2.2 Codes)';
      2051 : Result := 'GeogPrimeMeridianGeoKey (6.3.2.4 codes)';
      2052 : Result := 'GeogLinearUnitsGeoKey (6.3.1.3 codes)';
      2054 : Result := 'GeogAngularUnitGeoKey (6.3.1.4 codes)';
      2056 : Result := 'GeogEllipsoidGeoKey (6.3.2.3 codes)';
      2057 : Result := 'GeogSemiMajorAxisGeoKey';
      2058 : Result := 'GeogSemiMinorAxisGeoKey';
      2059 : Result := 'GeogInvFlatteningGeoKey';
      2061 : Result := 'PrimeMeridianLongitudeGeoKey';
      3072 : Result := 'ProjectedCSTypeGeoKey (6.3.3.1 codes)';
      3073 : Result := 'PCSCitationGeoKey (ASCII), not supported directly';
      3074 : Result := 'ProjectionGeoKey (6.3.3.2 codes)';
      3075 : Result := 'ProjCoordTransGeoKey (6.3.3.3 codes)';
      3076 : Result := 'ProjLinearUnitsGeoKey (6.3.1.3 codes)';
      3078 : Result := 'ProjStdParallelGeoKey';
      3079 : Result := 'ProjStdParallel2GeoKey';
      3080 : Result := 'ProjNatOriginLongGeoKey';
      3081 : Result := 'ProjOriginLatGeoKey';
      3082 : Result := 'ProjFalseEastingGeoKey';
      3083 : Result := 'ProjFalseNorthingGeoKey';
      3084 : Result := 'ProjFalseOriginLongGeoKey';
      3085 : Result := 'ProjFalseOriginLatGeoKey';
      3086 : Result := 'ProjFalseOriginEastingGeoKey';
      3087 : Result := 'ProjFalseOriginNorthingGeoKey';
      3088 : Result := 'ProjCenterLongGeoKey';
      3089 : Result := 'ProjCenterLatGeoKey';
      3092 : Result := 'ProjScaleAtNatOriginGeoKey';
      3095 : Result := 'ProjStraightVertPoleLongGeoKey';
      4096 : Result := 'VerticalGeoKey (Section 6.3.4.1 codes)';
      4097 : Result := 'VerticalCitationGeoKey';
      4098 : Result := 'VerticalDatumGeoKey (Section 6.3.4.2 codes)';
      4099 : Result := 'VerticalUnitsGeoKey (Section 6.3.1.3 codes)';
      5120 : Result := 'CoordinateEpochGeoKey';
      else Result := 'Unspecified';
   end {case};
end;

function IDProjectionMagic(TextLine,Magic : ANSIstring) : boolean;
begin
   StripCharacter(TextLine,' ');
   StripCharacter(TextLine,'_');
   Result := StrUtils.AnsiContainsText(TextLine,Magic);
end;


constructor tTIFFImage.CreateGeotiff(Metadataonly : boolean; NoGeo : boolean; inFileName : PathStr; var Success : boolean; ShowHeader : boolean = false; GetHistogram : boolean = true; BandNum : integer = 0);
label
   SkipThumbnail,
   RestartGeotiff;
var
   Dir    : DirStr;
   bName  : NameStr;
   Ext    : ExtStr;
   b  : DoubleBytes;
   Zone : byte;
   FileName,ProjFileName : PathStr;
   DatCode,Hemi,
   TiePoints,
   TypeSize : integer;
   tf  : float64;
   TFWFile,
   HeaderLogList    : tStringList;
   ASCIIStr,UTMString  : AnsiString;
   GeoSuccess,AllGray,
   FirstImage,
   GeoLatLong,
   ProjectionDefined : boolean;
   StripOffsetsOffset,
   GeoKeyDirectoryOffset : int64;
   ColorTableEntries,
   GeoKeyDirectorySize,
   EntriesRead,
   TileOffsets : Integer;
   JPLModelTransformation : packed array[1..16] of double;
   rx,ry,mx,my : array[1..4] of double;
   ALine,TStr,MenuStr : AnsiString;


        function LogASCIIdata(anOffset,aSize : int64) : ANSIString;
        var
           I : integer;
           TheBytes : array[0..62000] of byte;
        begin
            {$IfDef RecordProcessingHeader} WriteLineToDebugFile('LogASCIIdata, offset=' + IntToStr(anOffset) + '  size=' + IntToStr(aSize)); {$EndIf}
            FileSeek(TiffHandle,anOffset,0);
            FileRead(TiffHandle,TheBytes[1],aSize);
            Result := ByteArrayToString(TheBytes,asize);
            for I := 1 to length(Result) do if ((Result[i] = #0) or (Result[i] = #$A)) then Result[i] := ' ';
            Result := trim(Result);
            {$IfDef RecordProcessingHeader} WriteLineToDebugFile('LogASCIIdata, result=' + Result); {$EndIf}
        end;


         Procedure HandleTiePoints(ModelTiePointOffset,ModelTiePointSize : Int64);
         var
            i : integer;
            TieBoundBox : sfBoundBox;
         begin
            FileSeek(TiffHandle,ModelTiePointOffset,0);
            TiePoints := ModelTiePointSize div 6;
            InitializeBoundingBox(TieBoundBox);

            for i := 1 to TiePoints do begin
               TiffHeader.RasterX := MakeDouble;
               TiffHeader.RasterY := MakeDouble;
               TiffHeader.RasterZ := MakeDouble;
               TiffHeader.ModelX := MakeDouble;
               TiffHeader.ModelY := MakeDouble;
               TiffHeader.ModelZ := MakeDouble;
               PetMath.CompareValueToExtremes(TiffHeader.ModelX,TieBoundBox.xMin,TieBoundBox.xMax);
               PetMath.CompareValueToExtremes(TiffHeader.ModelY,TieBoundBox.yMin,TieBoundBox.yMax);
               {$If Defined(RecordFullGeotiff) or Defined(RecordInitDEM) or Defined(RecordTiePoints)}
                  WriteLineToDebugFile('Tie point ' + IntToStr(i) + ' rasterx=' + RealToString(TiffHeader.RasterX,-12,2) + ' rastery=' + RealToString(TiffHeader.RasterY,-12,2) +
                      ' modelx =' + RealToString(TiffHeader.ModelX,-18,-8) + ' modely=' + RealToString(TiffHeader.ModelY,-18,-8) + ' modelz=' + RealToString(TiffHeader.Modelz,-18,-2));
               {$EndIf}

               if (TiePoints = 4) then begin
                  rx[i] := TiffHeader.RasterX;
                  ry[i] := TiffHeader.RasterY;
                  mx[i] := TiffHeader.ModelX;
                  my[i] := TiffHeader.ModelY;
               end;
            end;
            TStr := ' Raster coords: ' + RealToString(TiffHeader.RasterX,-18,-8) + ' ' + RealToString(TiffHeader.RasterY,-18,-8) + ' Model coords: ' + RealToString(TiffHeader.ModelX,-12,-6) + '  ' + RealToString(TiffHeader.ModelY,-12,-6);

            if (TiePoints = 4) and (TiffHeader.ScaleX < 0.000001) then begin
               HeapSort(4,rx);
               HeapSort(4,ry);
               HeapSort(4,mx);
               HeapSort(4,my);

               TiffHeader.RasterX := rx[1];
               TiffHeader.RasterY := ry[1];
               TiffHeader.ModelX := mx[1];
               TiffHeader.ModelY := my[1];
               TiffHeader.ScaleX := (mx[4] - mx[1]) / (rx[4] - rx[1]);
               TiffHeader.ScaleY := (my[1] - my[4]) / (ry[4] - ry[1]);
               TStr := 'ModelTiePointOffset Scale factors (lat-long): ' + RealToString(TiffHeader.ScaleX,18,-8) +  ' &' + RealToString(TiffHeader.ScaleY,18,-8);
            end;
         end;

      procedure DealWithColorTable(ColorTableOffset,ColorTableSize : LongInt);
      var
         i : integer;
         ColorTable  : array[0..pred(256*6)] of byte;
      begin
         ColorTableEntries := ColorTableSize div 3;
         FileSeek(TiffHandle,ColorTableOffset,0);
         FileRead(TiffHandle,ColorTable,2*ColorTableSize);
         {$If Defined(RecordFullGeotiff) or Defined(RecordGeotiffPalette)} WriteLineToDebugFile('Color palette present'); {$EndIf}
         TIFFImageColorDefined := true;
         for i := 0 to pred(ColorTableEntries) do begin
            if BigEndian then begin
               TiffImageColor[i].rgbtRed   := ColorTable[(i*2)];
               TiffImageColor[i].rgbtGreen := ColorTable[2*ColorTableEntries + (i*2)];
               TiffImageColor[i].rgbtBlue  := ColorTable[4*ColorTableEntries + (i*2)];
            end
            else begin
               TiffImageColor[i].rgbtRed   := ColorTable[succ(i*2)];
               TiffImageColor[i].rgbtGreen := ColorTable[2*ColorTableEntries + succ(i*2)];
               TiffImageColor[i].rgbtBlue  := ColorTable[4*ColorTableEntries + succ(i*2)];
            end;
            {$IfDef RecordGeotiffPalette} WriteLineToDebugFile(IntegerToString(i,3) + '  ' + ColorStringFromPlatformColor(TiffImageColor[i])); {$EndIf}
         end;
         AllGray := true;
         for i := 0 to pred(ColorTableEntries) do begin
            if (TiffImageColor[i].rgbtRed <> TiffImageColor[i].rgbtGreen) or (TiffImageColor[i].rgbtRed <> TiffImageColor[i].rgbtBlue) then begin
               AllGray := false;
               Break;
            end;
         end;
         if AllGray then begin
            for i := 0 to pred(ColorTableEntries) do TiffImageColor[i] := RGBtrip(i,i,i);
            TiffHeader.PhotometricInterpretation := 1;
         end
         else CanEnhance := false;
      end;


            procedure ProcessASCIIstringForProjection(ASCIIStr : shortstring);
            var
               TStr : AnsiString;
            begin
               {$If Defined(RecordDefineDatum) or Defined(RecordPlateCaree)} WriteLineToDebugFile('ProcessASCIIstringForProjections in, Projection=' + MapProjection.GetProjName); {$EndIf}
               {$IfDef RecordDefineDatum} WriteLineToDebugFile('ASCII info 34737:' + ASCIIStr); {$EndIf}
               {$If Defined(LongCent)} WriteLineToDebugFile('ProcessASCIIstringForProjection in,  LongCent: ' + RadToDegString(MapProjection.Long0)); {$EndIf}

               TStr := ASCIIStr;
               if StrUtils.AnsiContainsText(TStr,'mdz=') then begin
                  UTMString := TStr;
                  while Copy(TStr,1,4) <> 'mdz=' do Delete(TStr,1,1);
                  Delete(TStr,1,4);
                  TStr := Petmar_types.BeforeSpecifiedCharacterANSI(Tstr,'/');
                  TiffHeader.MDZtype := StrToInt(TStr);
                  exit;
               end;

               if StrUtils.AnsiContainsText(TStr,MapFromMICRODEMstr) then exit;

               if (DatCode = -99) or (DatCode = 32767) or (MapProjection.H_datumCode = '') then begin

                  if ThisIsETRS89(Tstr) or ThisIsWGS84(TStr) then begin
                    {$IfDef RecordDefineDatum} WriteLineToDebugFile('ETRS89 or WGS84'); {$EndIf}
                     if (MapProjection.H_datumCode = '') or (MapProjection.H_datumCode = 'rect') then begin
                        if ThisIsETRS89(Tstr) then MapProjection.H_datumCode := 'ETR89'
                        else if ThisIsWGS84(TStr) then MapProjection.H_datumCode := 'WGS84'
                        else if StrUtils.AnsiContainsText(TStr,'NAD83') then MapProjection.H_datumCode := 'NAD83'
                        else if StrUtils.AnsiContainsText(TStr,'NAD27') then MapProjection.H_datumCode := 'NAD27';
                     end;

                     if MapProjection.PName in [AlbersEqAreaConicalEllipsoid,PolarStereographicEllipsoidal] then begin
                        MapProjection.GetProjectParameters;
                        ProjectionDefined := true;
                     end
                     else begin
                        if StrUtils.AnsiContainsText(TStr,'Plate Carree') or StrUtils.AnsiContainsText(TStr,'Geographic') then begin
                           MapProjection.PName := PlateCaree;
                           {$IfDef RecordPlateCaree} WriteLineToDebugFile('PlateCaree, from ASCII string'); {$EndIf}
                        end;

                        if FindUTMZone(TStr,MapProjection.projUTMZone,MapProjection.LatHemi) then begin
                           MapProjection.StartUTMProjection(MapProjection.projUTMZone);
                           ProjectionDefined := true;
                        end;
                     end;
                   end
                   else begin
                       if StrUtils.AnsiContainsText(TStr,'OSGB') and StrUtils.AnsiContainsText(TStr,'1936') then MapProjection.H_datumCode := 'OGB-M'
                       else if FindUTMZone(TStr,MapProjection.projUTMZone,MapProjection.LatHemi) then begin
                           MapProjection.StartUTMProjection(MapProjection.projUTMZone);
                           ProjectionDefined := true;
                        end;
                   end;
               end;

               if ProjectionDefined then begin
                  {$If Defined(RecordDefineDatum) or Defined(LongCent)} MapProjection.ShortProjInfo('calling SetDatumConstants'); {$EndIf}
                  MapProjection.SetDatumConstants;
                  {$If Defined(RecordDefineDatum) or Defined(LongCent)} MapProjection.ShortProjInfo('calling GetProjectParameters'); {$EndIf}
                  MapProjection.GetProjectParameters;
               end;
            {$If Defined(RecordDefineDatum) or Defined(LongCent)} MapProjection.ShortProjInfo('ProcessASCIIstringForProjection out'); {$EndIf}
            ASCIIStr := '';
         end;


      procedure TryWorldFile;
      var
         i : integer;
      begin
         ProjectionDefined := false;
         FSplit(TiffFileName,Dir,bName,Ext);

         FileName := Dir + bName + '.tfw';
         if not FileExists(FileName) then begin
            FileName := Dir + bName + '.tifw';
         end;
         if not FileExists(FileName) then begin
            FileName := Dir + 'all.world';
         end;

         if FileExists(FileName) then begin
            ReadWorldFile(MapProjection,MapProjection.H_datumCode,TiffFileName,RegVars);
            ProjectionDefined := true;
            TFWFile := TStringList.Create;
            TFWFile.LoadFromFile(FileName);
            HeaderLogList.Add('');
            HeaderLogList.Add('World file ' + FileName);
            for i := 0 to pred(TFWFile.Count) do begin
               HeaderLogList.Add(TFWfile.Strings[i]);
            end;
            TFWFile.Free;
         end
         else begin
            HeaderLogList.Add('');
            HeaderLogList.Add('No world file found');
         end;
      end;

        procedure DefineGCScoordinates;
        begin
            TiffHeader.ModelType := 2;
            ProjectionDefined := true;
            MapProjection.PName := PlateCaree;
        end;

         procedure GetUTMZoneNotAlreadyDefined;
         begin
            {$IfDef RecordDefineDatum} WriteLineToDebugFile('undefined UTM zone'); {$EndIf}
            if (TiffHeader.ModelType = 1) then begin
               Zone := MDDef.DefaultUTMZone;
               TiffHeader.HemiChar := MDDef.DefaultLatHemi;
               MapProjection.H_datumCode := MDDef.DefaultDigitizeDatum;
               {$IfDef RecordFullGeotiff} WriteLineToDebugFile('Need to get registration, ' + FileName); {$EndIf}
               UseDefaultDatumZone := StrUtils.AnsiContainsText(UpperCase(TIFFFileName),'GLC2000') or StrUtils.AnsiContainsText(UpperCase(TIFFFileName),'GLC-2000');
               if (abs(TiffHeader.ModelX) <= 360) and (abs(TiffHeader.ModelY) <= 90)  and (TiffHeader.ScaleX < 0.1) and (TiffHeader.ScaleY < 1) then GeoLatLong := true
               else begin
                  {$IfDef VCL}
                     GetMapParametersSPCSOption(ProjFileName,TiffHeader.HemiChar,MapProjection.projUTMZone,MapProjection.H_DatumCode,MapProjection.PName,GeoLatLong,false);
                  {$EndIf}
               end;
               if GeoLatLong then TiffHeader.ModelType := 2;
               if (TiffHeader.HemiChar = 'N') then Hemi := 45 else Hemi := -45;
               MapProjection.projUTMZone := Zone;
            end
            else begin
               MapProjection.projUTMZone := GetUTMZone(TiffHeader.ModelX);
               if (TiffHeader.ModelY > 0) then Hemi := 45 else Hemi := -45;
               MapProjection.Long0 := UTMZoneCentralLong(MapProjection.projUTMZone) * DegToRad;
            end;
         end;


   procedure ReadTiffTags;
   type
      tTiffKey = record
         Tag : Word;
         FType : word;
         LengthIm : int64;
         KeyOffset : int64;
         LittleString : shortstring;
      end;
   var
      i,j,k : integer;
      TiffKeys : array[1..100] of tTiffKey;
      SmallOffsetArray : ^tSmallOffsetArray;
      TStr,TStr34737 : ANSIstring;
      b : array[1..8] of byte;

      {$If Defined(RecordFullGeotiff) or Defined(RecordKeys)}
         procedure WriteKey(j : integer);
         begin
            try
               WriteLineToDebugFile(IntegerToString(j,5) + IntegerToString(TiffKeys[j].Tag,8) + IntegerToString(TiffKeys[j].ftype,8) +
                  IntegerToString(TiffKeys[j].LengthIm,12) + IntegerToString(TiffKeys[j].KeyOffset,14));
             except
                on ERangeError do WriteLineToDebugFile(IntegerToString(j,5) + IntegerToString(TiffKeys[j].Tag,8) + IntegerToString(TiffKeys[j].ftype,8));
             end;
         end;
     {$EndIf}


   begin {procedure ReadTiffTags}
      {$If Defined(RecordGeotiff) or Defined(RecordPlateCaree)} WriteLineToDebugFile('ReadTiffTags in, tags=' + IntToStr(TiffHeader.NumEnt)); {$EndIf}
      {$If Defined(LongCent)} WriteLineToDebugFile('ReadTIFFtags in,  LongCent: ' + RadToDegString(MapProjection.Long0)); {$EndIf}
      with TiffHeader do begin
         for j := 1 to TiffHeader.NumEnt do begin
            TiffKeys[j].Tag := MakeWord;
            TiffKeys[j].Ftype := MakeWord;
            TypeSize := GeotiffTypeSize(TiffKeys[j].Ftype);
            TiffKeys[j].LengthIm := MakeOffset;
            TiffKeys[j].LittleString := '';
            if (TiffKeys[j].LengthIm * TypeSize <= OffsetByteSize) and (TiffKeys[j].Ftype = 2) then begin
               FileRead(TiffHandle,b,OffsetByteSize);
               for i := 1 to OffsetByteSize do if (b[i] <> 0) then TiffKeys[j].LittleString := TiffKeys[j].LittleString + chr(b[i]);
               TiffKeys[j].KeyOffset := 0;
            end
            else begin
               TiffKeys[j].KeyOffset := MakeOffset;
            end;
           {$If Defined(RecordFullGeotiff) or Defined(RecordKeys)} WriteKey(j); {$EndIf}
         end;

         {$IfDef RecordGeotiff} WriteLineToDebugFile('ReadTiffTags first j loop done'); WriteLineToDebugFile(''); {$EndIf}

         EntriesRead := 0;
         for j := 1 to NumEnt do begin
            {$If Defined(RecordFullGeotiff) or Defined(RecordKeys)} WriteKey(j); {$EndIf}
            inc(EntriesRead);
            TStr := '';
            case TiffKeys[j].Tag of
               254 : begin
                        NewSubfileType := TiffKeys[j].KeyOffset;
                        if (EntriesRead > 1) then begin
                           FirstImage := false;
                           if Odd(NewSubfileType) then exit;
                        end;
                     end;
               255 : SubfileType := TiffKeys[j].KeyOffset;
               256 : ImageWidth := TiffKeys[j].KeyOffset;
               257 : ImageLength := TiffKeys[j].KeyOffset;
               258 : begin
                       {$IfDef RecordFullGeotiff} WriteKey(j); {$EndIf}
                        if TiffKeys[j].KeyOffset in [8,15,16,32,64] then begin
                           BitsPerSample := TiffKeys[j].KeyOffset;
                           TStr := IntToStr(BitsPerSample);
                        end
                        else if (TiffKeys[j].LengthIm > 1) then begin
                           BitsPerSampleCount := TiffKeys[j].LengthIm;
                           if (TiffKeys[j].KeyOffset > 0) then begin
                              FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                              TStr := '';
                              for i := 1 to BitsPerSampleCount do begin
                                 BitsPerSample := MakeWord;
                                 TStr := TStr + IntToStr(BitsPerSample);
                                 if (I < BitsPerSampleCount) then TStr := TStr + ',';
                              end;
                           end;
                        end
                        else begin
                           BitsPerSample := TiffKeys[j].KeyOffset;
                           TStr := IntToStr(BitsPerSample);
                        end;
                        TiffHeader.BytesPerSample := BitsPerSample div 8;
                        {$IfDef RecordBitPerPixel} WriteLineToDebugFile('Read from offsets, BitsPerSample=' + IntToStr(BitsPerSample)); {$EndIf}
                     end;
               259 : begin
                        Compression := TiffKeys[j].KeyOffset;
                        if (Compression <> 1) then begin
                           Success := false;
                           TStr := 'Unsupported compression (key 259)=' + IntToStr(Compression);
                           HeaderLogList.Insert(1,TStr);
                           {$If Defined(RecordGeotiffFailures)} WriteLineToDebugFile(TStr + '  ' + inFileName); {$EndIf}
                         end;
                     end;
               262 : begin
                        PhotometricInterpretation := TiffKeys[j].KeyOffset;
                        case PhotometricInterpretation of
                           0 : TStr := '  WhiteIsZero';
                           1 : TStr := '  BlackIsZero';
                           2 : TStr := '  RGB';
                           3 : TStr := '  Palette color';
                           else TStr := ' unsupported';
                        end;
                        if (PhotometricInterpretation in [0,1,2,3]) then begin
                        end
                        else begin
                           Success := false;
                           MenuStr := 'Unsupported Photo interp=' + IntToStr(PhotometricInterpretation);
                           {$If Defined(RecordGeotiffFailures)} WriteLineToDebugFile(MenuStr + '  ' + inFileName); {$EndIf}
                           HeaderLogList.Insert(1,MenuStr);
                        end;
                        {$IfDef VCL}
                           if (PhotometricInterpretation = 1) then begin
                              for i := 0 to 255 do TiffImageColor[i] := RGBtrip(i,i,i);
                           end
                           else if (PhotometricInterpretation = 0) then begin
                              for i := 0 to 255 do TiffImageColor[i] := RGBtrip(255-i,255-i,255-i);
                           end;
                        {$EndIf}
                     end;
               263 : ThreshHolding := TiffKeys[j].KeyOffset;
               264 : CellWidth := TiffKeys[j].KeyOffset;
               265 : CellLength := TiffKeys[j].KeyOffset;
               266 : FillOrder := TiffKeys[j].KeyOffset;
               270 : begin
                        GeotiffImageDesc := LogASCIIdata(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm);
                        TStr := GeotiffImageDesc;
                     end;
               273 : begin
                     //6     273       4          28         286
                       {$If Defined(RecordImageOffsets)} WriteLineToDebugFile('Geotiff key 273, strip offsets'); {$EndIf}
                        k := TiffKeys[j].LengthIm;
                        if (k > 1) then begin
                           FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                           OffsetArraySize := 8 * TiffKeys[j].LengthIm;
                           GetMem(OffsetArray,OffsetArraySize);

                           if (OffsetByteSize = 4) then begin
                              {$If Defined(RecordImageOffsets)} WriteLineToDebugFile('Geotiff key 273, 4 byte offsets'); {$EndIf}
                              GetMem(SmallOffsetArray,OffsetArraySize div 2);
                              FileRead(TiffHandle,SmallOffsetArray^[0],OffsetArraySize div 2);
                              for k := 0 to pred(TiffKeys[j].LengthIm) do begin
                                 OffsetArray^[k] := SmallOffsetArray^[k];
                              end;
                              FreeMem(SmallOffsetArray,OffsetArraySize div 2);
                           end
                           else begin
                              {$If Defined(RecordImageOffsets)} WriteLineToDebugFile('Geotiff key 273, 8 byte offsets'); {$EndIf}
                              FileRead(TiffHandle,OffsetArray^[0],OffsetArraySize);
                           end;
                           Tstr := '';
                           for k := 0 to 10 do TStr := TStr + IntToStr(OffsetArray^[k]) + ',';
                        end
                        else begin
                           StripOffsets := TiffKeys[j].KeyOffset;
                           TStr := IntToStr(StripOffsets);
                           {$If Defined(RecordImageOffsets)} WriteLineToDebugFile('Geotiff key 273, single offset=' + TStr); {$EndIf}
                        end;
                     end;
               274 : begin
                        Orientation := TiffKeys[j].KeyOffset;
                        if (Orientation <> 1) then begin
                           Success := false;
                           MenuStr := 'Unsupported orientation (unless DEM)=' + IntToStr(Orientation);
                           {$If Defined(RecordFullGeotiff) or Defined(RecordGeotiffFailures)} WriteLineToDebugFile(MenuStr + '  ' + inFileName); {$EndIf}
                           HeaderLogList.Insert(1,MenuStr);
                        end;
                     end;
               277 : SamplesPerPixel := TiffKeys[j].KeyOffset;
               278 : begin
                        RowsPerStrip := TiffKeys[j].KeyOffset;
                     end;
               279 : begin
                         if TiffKeys[j].LengthIm  * GeotiffTypeSize(TiffKeys[j].FType) <= 4 then begin
                            StripByteCounts := TiffKeys[j].KeyOffset div 256 div 256;
                         end
                         else begin
                            FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                            if (GeotiffTypeSize(TiffKeys[j].Ftype) = 2) then StripByteCounts := MakeWord
                            else StripByteCounts := MakeLongInt;
                            TStr := IntToStr(StripByteCounts);
                         end;
                     end;
               280 : begin
                       FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                       for i := 1 to SamplesPerPixel do begin
                           MinSampleValue[i] := MakeWord;
                       end;
                     end;
               281 :  begin
                        FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                        for i := 1 to SamplesPerPixel do begin
                           MaxSampleValue[i] := MakeWord;
                           {$IfDef RecordFullGeotiff} WriteLineToDebugFile('Band ' + IntToStr(I) + ' range: ' + IntToStr(MinSampleValue[i]) + '--' + IntToStr(MaxSampleValue[i]) ); {$EndIf}
                        end;
                     end;
               282 : begin  //type rational, probably two values and must divide, but not needed
                        //Seek(TiffFile,TiffKeys[j].KeyOffset);
                        //TStr := RealToString(MakeDouble,-12,-4);
                     end;
               283 : begin  //type rational, probably two values and must divide, but not needed
                        //Seek(TiffFile,TiffKeys[j].KeyOffset);
                        //TStr := RealToString(MakeDouble,-12,-4);
                      end;
               284 : PlanarConfiguration := TiffKeys[j].KeyOffset;
               285 : begin end;
               296 : begin
                         //case TiffKeys[j].KeyOffset of
                            //1 : ResolutionUnit := None;
                            //2 : ResolutionUnit := Inch;
                            //3 : ResolutionUnit := cm;
                         //end {case};
                     end;
               305 : TStr := LogASCIIdata(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm);
               306 : TStr := LogASCIIdata(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm);
               320 : DealWithColorTable(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm);
               322 : TileWidth := TiffKeys[j].KeyOffset;
               323 : TileHeight := TiffKeys[j].KeyOffset;
               //324 : TileOffsets := TiffKeys[j].KeyOffset;  //crashes, and not needed
               338 : begin
                        ExtraSample := TiffKeys[j].LengthIm;
                        TStr := '  ExtraSample ' + IntToStr(ExtraSample);
                     end;
               339 : SampleFormat := tSampleFormat(pred(TiffKeys[j].KeyOffset));
               340 : begin

     //11 : TIFFTypeName := ' single IEEE 4 byte ';
     //12 : TIFFTypeName := ' double IEEE 8 byte ';
                        FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                        if TiffKeys[j].FType = 11 then begin
                           SMin := MakeSingle;
                        end
                        else if TiffKeys[j].Ftype = 12  then begin
                           SMin := MakeDouble;
                        end;
                        TStr := RealToString(SMin,-12,-4);
                        //HighlightLineToDebugFile('smin=' + TStr);
                      end;
               341 : begin
                        FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                        if TiffKeys[j].FType = 11 then SMax := MakeSingle else if TiffKeys[j].FType = 12 then SMax := MakeDouble;
                        TStr := RealToString(SMax,-12,-4);
                        //HighlightLineToDebugFile('smax=' + TStr);
                      end;
              33432 : TStr := LogASCIIdata(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm); {ModelPixelScaleTag}
              33550 : begin {ModelPixelScaleTag}
                           FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                           TiffHeader.ScaleX := MakeDouble;
                           TiffHeader.ScaleY := MakeDouble;
                           TiffHeader.ScaleZ := MakeDouble;
                           if (TiffHeader.ModelType = 2) then begin
                              tf := TiffHeader.ScaleX;
                              TiffHeader.ScaleX := TiffHeader.ScaleY;
                              TiffHeader.ScaleY := tf;
                           end;
                           TStr := ' scalex=' + RealToString(TiffHeader.ScaleX,-18,-8) + '   scaley=' + RealToString(TiffHeader.ScaleY,-18,-8)+ '   scalez=' + RealToString(TiffHeader.Scalez,-18,-8);
                      end;
              33922 : begin {ModelTiePointTag}
                          {$IfDef RecordProcessingHeader} WriteLineToDebugFile('Key=33922   ' + IntToStr(TiffKeys[j].KeyOffset) + '  ' + IntToStr(TiffKeys[j].LengthIm) ); {$EndIf}
                          HandleTiePoints(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm);
                      end;
              34264 : begin {ModelTransformationTag, JPL Carto Group}
                           FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                           for I := 1 to TiffKeys[j].LengthIm do JPLModelTransformation[i] := MakeDouble;
                           TiffHeader.ScaleX := JPLModelTransformation[1];
                           TiffHeader.ScaleY := JPLModelTransformation[6];
                           TiffHeader.ScaleZ := 0;
                           TiffHeader.ScaleX := JPLModelTransformation[4];
                           TiffHeader.ScaleY := JPLModelTransformation[8] + TiffHeader.ScaleY * ImageLength;
                           TiffHeader.Scalez := 0;
                      end;
              34453 : begin
                         TStr := LogASCIIdata(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm);
                         if StrUtils.AnsiContainsText(UpperCase(inFileName),'PLATE CARREE') then begin
                            TiffHeader.ModelType := 2;
                            {$IfDef RecordPlateCaree} WriteLineToDebugFile('PlateCaree, from 34453'); {$EndIf}
                         end;
                         ProjectionDefined := true;
                      end;
              34454 : begin
                           TStr := '';
                           FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                           for i := 1 to 3 do begin
                              TStr := TStr + IntToStr(MakeWord);
                              if (i <> 3) then TStr := tStr + ', ';
                           end;
                      end;
              34735 : begin
                        GeoKeyDirectoryOffset := TiffKeys[j].KeyOffset;
                        GeoKeyDirectorySize := TiffKeys[j].LengthIm;
                      end;
              34736 : begin
                          FileSeek(TiffHandle,TiffKeys[j].KeyOffset,0);
                          TStr := '';
                          for i := 0 to pred(TiffKeys[j].LengthIm) do begin
                             MapProjection.GeotiffDoubles[i] := MakeDouble;
                             TStr := TStr + RealToString(MapProjection.GeotiffDoubles[i],-12,-2);
                             if (i <> pred(TiffKeys[j].LengthIm)) then TStr := tStr + ', ';
                          end;
                      end;
              34737 : begin
                         if (TiffKeys[j].LittleString <> '') then begin
                            TStr := TiffKeys[j].LittleString;
                            ASCIIStr := TStr;
                         end
                         else begin
                             TStr := LogASCIIdata(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm);
                             {$IfDef RecordProjProgress} WriteLineToDebugFile('In 34737 ' + TStr); {$EndIf}
                             if StrUtils.AnsiContainsText(UpperCase(TStr),'UTM') then begin
                                 //among possible others, this is for RDN2008, Italy one zone
                                 {$IfDef RecordPlateCaree} WriteLineToDebugFile('UTM, from 34737'); {$EndIf}
                                 ProcessASCIIstringForProjection(TStr);
                                 ProjectionDefined := true;
                             end
                             else if StrUtils.AnsiContainsText(UpperCase(TStr),'PCS NAME =') then begin
                                MapProjection.wktString := tstr;
                                StripBlanks(MapProjection.wktString);
                                if StrUtils.AnsiContainsText(UpperCase(MapProjection.wktString),'ESRIPESTRING') then begin
                                   MapProjection.wktString := AfterSpecifiedStringANSI(MapProjection.wktString, 'ESRIPEString=');
                                end;
                                ProjectionDefined := MapProjection.InitProjFromWKTstring(MapProjection.wktString);
                             end
                             else if StrUtils.AnsiContainsText(TStr,'OSGB36')  then begin
                                MapProjection.pName := UK_OS;
                                MapProjection.projUTMzone := 30;
                                MapProjection.LatHemi := 'N';
                                ProjectionDefined := true;
                             end
                             else if StrUtils.AnsiContainsText(TStr,'NZGD2000') then begin
                                MapProjection.InitProjFromWKTfile(ProgramRootDir + 'wkt_proj\nzgd2000_epsg_2193.wkt');
                                MapProjection.projUTMzone := 59;
                                MapProjection.LatHemi := 'S';
                                ProjectionDefined := true;
                             end
                             else if StrUtils.AnsiContainsText(UpperCase(TStr),'GCS_WGS_1984') then begin
                                 {$IfDef RecordPlateCaree} WriteLineToDebugFile('GCS, from 34737'); {$EndIf}
                                 DefineGCScoordinates;
                                 ProjectionDefined := true;
                             end
                             else ASCIIStr := TStr;
                             //not processing GDA94 / MGA zone 53|GDA94|, since it will be covered with tag 3072
                            {$IfDef RecordProjProgress} MapProjection.WriteProjectionSummaryToDebugFile('End key 34737: '); {$EndIf}
                            {$If Defined(TrackWKTstring)} WriteLineToDebugFile('end key 34737, wkt=' + IntToStr(length(MapProjection.wktString))); {$EndIf}
                          end;

                      end;
              42112 : begin
                         TStr := LogASCIIdata(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm);
                         {$If Defined(TrackZ)} WriteLineToDebugFile('Key 42112 ' + TStr); {$EndIf}
                         if StrUtils.AnsiContainsText(UpperCase(Tstr),'FOOT') then FootDEM := true;
                         if StrUtils.AnsiContainsText(UpperCase(Tstr),'EGM2008') then begin
                            TiffHeader.VertDatum := VertCSEGM2008;
                            {$IfDef TrackVerticalDatum} WriteLineToDebugFile('Geotiff tag 42112,  vdatum=' + IntToStr(TiffHeader.VertDatum)); {$EndIf}
                         end;
                         if StrUtils.AnsiContainsText(UpperCase(Tstr),'CENTIMETERS') then TiffHeader.MDZtype := euCentimeters;
                         if StrUtils.AnsiContainsText(UpperCase(Tstr),'"SCALE" SAMPLE="0" ROLE="SCALE">0.1') then TiffHeader.MDZtype := euDecimeters;
                         Tag42112 := TStr;
                         Tag42112Offset := TiffKeys[j].KeyOffset;
                         Tag42112Length := TiffKeys[j].LengthIm;
                      end;
              42113 : begin
                         if (TiffKeys[j].LittleString <> '') then begin
                            TStr := TiffKeys[j].LittleString;
                         end
                         else begin
                            TStr := LogASCIIdata(TiffKeys[j].KeyOffset,TiffKeys[j].LengthIm);
                         end;
                         if (UpperCase(TStr) <> 'NAN') and IsNumeric(TStr) then CurrentMissing := StrToFloat(Tstr);
                      end;
            end {case};
            TStr := IntegerToString(TiffKeys[j].Tag,8) + '   ' + TIFFTypeName(TiffKeys[j].fType) + IntegerToString(TiffKeys[j].LengthIm,8) + '  ' + IntToStr(TiffKeys[j].KeyOffset) + '  ' + TiffTagName(TiffKeys[j].Tag) + '  ' + TStr;
            HeaderLogList.Add(TStr);
            {$If Defined(RecordProcessingHeader) or Defined(RecordKeys)} WriteLineToDebugFile(TStr); {$EndIf}
         end {for j};

         if (SampleFormat = sfUndefined) then begin
            if (BitsPerSample = 16) then SampleFormat := sfWord;
         end;
      end;

      if StrUtils.AnsiContainsText(UpperCase(inFileName),'GMRT') then TiffHeader.ModelType := 2;
      if (TiffHeader.StripOffsets = 0) and (TileOffsets <> 0) then TiffHeader.StripOffsets := TileOffsets;
      {$If Defined(LongCent)} WriteLineToDebugFile('ReadTIFFtags done, LongCent: ' + RadToDegString(MapProjection.Long0)); {$EndIf}
      {$IfDef TrackModelType} WriteLineToDebugFile('ReadTIFFtags done, TiffHeader.ModelType=' + IntToStr(TiffHeader.ModelType) ); {$EndIf}
      {$IfDef RecordInitDEM} WriteLineToDebugFile('ReadTiffTags out, ModelX=' + RealToString(TiffHeader.ModelX,-18,-6) + ' ModelY=' + RealToString(TiffHeader.ModelY,-18,-6) ); {$EndIf}
      {$IfDef RecordBitPerPixel} WriteLineToDebugFile('ReadTiffTags out, BitsPerSample=' + IntToStr(TiffHeader.BitsPerSample)); {$EndIf}
      {$If Defined(RecordDefineDatum) or Defined(RecordGeotiffProjection)} WriteLineToDebugFile('ReadTiffTags out'); {$EndIf}
      {$IfDef RecordProjProgress} MapProjection.WriteProjectionSummaryToDebugFile('ReadTiffTags out, '); {$EndIf}
   end;


   procedure InitializeValues;
   begin
      ProjectionDefined := false;
      CurrentMissing := MDDef.GeotiffMissingValue;
      GeotiffImageDesc := '';
      ASCIIstr := '';
      FirstImage := true;
      TiffOpen := false;
      TIFFImageColorDefined := false;
      GeoSuccess := false;
      ProjectionDefined := false;
      GeoKeyDirectorySize := 0;
      GeoKeyDirectoryOffset := 0;
      StripOffsetsOffset := 0;
      TiffHeader.Factor := 1;
      TiffHeader.ModelType := -99;
      TiffHeader.Orientation := 1;
      TiffHeader.SamplesPerPixel := 1;
      TiffHeader.ExtraSample := 0;
      TiffHeader.RasterPixelIs := 0;
      TiffHeader.VertDatum := 0;
      TiffHeader.TileWidth := 0;
      TiffHeader.SMax := -9999;
      TiffHeader.SMin := 9999;
      TiffHeader.MDZtype := 0;
      TiffHeader.OffsetArray := Nil;
      TiffHeader.StripOffsets := 0;
      TiffHeader.FootDEM := false;
      Tag42112 := '';
      DatCode := -99;
   end;

   {$IfDef NoGeotiffProjection}
   {$Else}

      procedure ReadGeotiffTags;
      var
         i : integer;
         Tag,  ftype, LengthIm, TiffOffset : Word;
         Factor,xt : float64;

                   function SetADouble(DN,Decimals : integer) : float64;
                   begin
                      Result := MapProjection.GeotiffDoubles[TiffOffset];
                      TStr := RealToString(Result,-15,Decimals);
                      inc(MapProjection.GeoKeys.NumKeys);
                      MapProjection.GeoKeys.KeyCode[MapProjection.GeoKeys.NumKeys] := Tag;
                      MapProjection.GeoKeys.KeyVal[MapProjection.GeoKeys.NumKeys] := Result;
                   end;

      begin
         {$If Defined(RecordDefineDatum) or Defined(RecordPlateCaree) or Defined(TrackProjection)  or Defined(RecordGeotiffProjection)}
            WriteLineToDebugFile('ReadGeotiffTags in, Projection=' + MapProjection.GetProjName);
         {$EndIf}
         {$If Defined(LongCent)} WriteLineToDebugFile('ReadGeotiffTags in,  LongCent: ' + RadToDegString(MapProjection.Long0)); {$EndIf}
         {$IfDef RecordprojProgress} MapProjection.WriteProjectionSummaryToDebugFile('Start ReadGeotiffTags: '); {$EndIf}

         if (not ProjectionDefined) then begin
            MapProjection.PName := UndefinedProj;
            MapProjection.ProjMapScale := 1;
         end;

            HeaderLogList.Add('');
            HeaderLogList.Add('');
            HeaderLogList.Add('GEOTiff GeoKey Directory Tag');
            FileSeek(TiffHandle,GeoKeyDirectoryOffset,0);
            TiffHeader.ModelType := 1;
            for i := 1 to (GeoKeyDirectorySize div 4) do begin
               FileSeek(TiffHandle,GeoKeyDirectoryOffset + pred(i) * 8,0);
               Tag := MakeWord;
               ftype := MakeWord;
               LengthIm := MakeWord;
               TiffOffset := MakeWord;
               TStr := '';
               //{$If Defined(RecordDefineDatum) or Defined(RecordPlateCaree)} WriteLineToDebugFile('Key=' + IntToStr(i) + ', Tag=' + IntToStr(Tag) + '  Projection=' + MapProjection.GetProjName); {$EndIf}
               MapProjection.ProcessGeotiffKey(Tag,TiffOffset);

               case Tag of
                  1024 : begin
                            if TiffOffset in [0,1,2] then begin
                               TiffHeader.ModelType := TiffOffset;
                            end
                            else begin
                               GeoSuccess := false;
                               HeaderLogList.Insert(1,'Unsupported model type' + IntegerToString(TiffOffset,8));
                            end;
                         end;
                  1025 : begin
                            TiffHeader.RasterPixelIs := TiffOffset;
                            TStr := RasterPixelIsString(TiffOffset);
                            if StrUtils.AnsiContainsText(TIFFFileName,'ASTGTMV003') then begin
                               TiffHeader.RasterPixelIs := PixelIsArea;
                               if TiffOffset <> PixelIsArea then TStr := RasterPixelIsString(PixelIsPoint) + ' corrected by MICRODEM';
                            end;
                         end;
                  1026 : TStr := ASCIIStr;
                  2048 : begin //horizontal datum
                             TStr := MapProjection.ProcessTiff2048(TiffOffset);
                             if (TStr <> '') and (TStr <> 'User defined') then begin
                                DefineGCScoordinates;
                                TStr := MapProjection.H_datumCode;
                             end;
                         end;
                  2049 : TStr := ASCIIStr;
                  2050 : begin
                            case TiffOffset of
                               6326 : MapProjection.H_datumCode := 'WGS84';
                               32767 : TStr := 'User defined';
                               else begin
                                  HeaderLogList.Insert(1,'2050 Unsupported datum ' + IntToStr(TiffOffset));
                               end;
                            end;
                         end;
                  2056 : TStr := 'User defined';
                  2057 : MapProjection.a := SetADouble(TiffOffset,2);
                  2058 : SetADouble(TiffOffset,6);
                  2059 : MapProjection.h_f := SetADouble(TiffOffset,6);
                  2061 : SetADouble(TiffOffset,6);  //Prime Meridian longitude
                  3072,
                  3074 : begin
                             TStr := MapProjection.OpenFromTiff3072(TiffOffset);
                             ProjectionDefined :=  TStr <> 'Undefined';
                             NeedWKTHailMary := not ProjectionDefined;
                             {$If Defined(RecordFullGeotiff) or Defined(TrackProjection)} MapProjection.WriteProjParamsToDebugFile('Key 3072'); {$EndIf}
                         end;
                  3073 : begin end;
                  3075 : begin
                             TStr := MapProjection.ProcessTiff3075(TiffOffset);
                             {$If Defined(RecordFullGeotiff) or Defined(TrackProjection)} MapProjection.WriteProjParamsToDebugFile('Key 3075'); {$EndIf}
                         end;
                  3076 : begin
                            if (TiffOffset = 9001) or (TiffOffset = 9002) or (TiffOffset = 9003) then begin
                                 case TiffOffset of
                                    9002 {spcsIntFeet} : Factor := 0.3048;
                                    9003 {spcsUSFeet} : Factor := 1200/3937;
                                    else Factor := 1;
                                 end;
                                 TStr := RealToString(Factor,-12,-6);
                                {$If Defined(Record3076) or Defined(RecordInitDEM)}
                                   WriteLineToDebugFile('Got factor ScaleX=' + RealToString(TiffHeader.ScaleX,-18,-6) + ' ScaleY=' + RealToString(TiffHeader.ScaleY,-18,-6) +
                                        ' ModelX=' + RealToString(TiffHeader.ModelX,-18,-6) + ' ModelY=' + RealToString(TiffHeader.ModelY,-18,-6) +
                                        ' RasterX=' + RealToString(TiffHeader.RasterX,-18,-6) + ' RasterY=' + RealToString(TiffHeader.RasterY,-18,-6) );
                                {$EndIf}
                                 TiffHeader.ScaleX := TiffHeader.ScaleX * Factor;
                                 TiffHeader.ScaleY := TiffHeader.ScaleY * Factor;
                                 TiffHeader.ModelX := TiffHeader.ModelX * Factor;
                                 TiffHeader.ModelY := TiffHeader.ModelY * Factor;
                                {$If Defined(Record3076) or Defined(RecordInitDEM)}
                                    WriteLineToDebugFile('Used factor ScaleX=' + RealToString(TiffHeader.ScaleX,-18,-6) + ' ScaleY=' + RealToString(TiffHeader.ScaleY,-18,-6) +
                                        ' ModelX=' + RealToString(TiffHeader.ModelX,-18,-6) + ' ModelY=' + RealToString(TiffHeader.ModelY,-18,-6) +
                                        ' RasterX=' + RealToString(TiffHeader.RasterX,-18,-6) + ' RasterY=' + RealToString(TiffHeader.RasterY,-18,-6) );
                                {$EndIf}
                            end
                            else MessageToContinue('Unknown linear units (' + IntToStr(TiffOffset) + '); Problems ahead');
                         end;
                  3078 : MapProjection.Phi1 := SetADouble(TiffOffset,4) * DegToRad;
                  3079 : MapProjection.Phi2 := SetADouble(TiffOffset,4) * DegToRad;
                  3080,3084,3088 : MapProjection.Long0 := SetADouble(TiffOffset,4) * DegToRad;
                  3081,3085,3089 : MapProjection.Lat0 := SetADouble(TiffOffset,4) * DegToRad;
                  3082,3086 : MapProjection.False_east := SetADouble(TiffOffset,2) * Factor;
                  3083,3087 : MapProjection.False_north := SetADouble(TiffOffset,2) * Factor;
                  3092 : MapProjection.ProjMapScale := SetADouble(TiffOffset,4);
                  3095 : xt := SetADouble(TiffOffset,4);
                  4096 : begin
                            TiffHeader.VertDatum := TiffOffset;
                            {$IfDef TrackVerticalDatum} WriteLineToDebugFile('Geotiff tag 4096 vdatum=' + IntToStr(TiffHeader.VertDatum)); {$EndIf}
                            TStr := MapProjection.ProcessTiff4096(TiffOffset);
                         end;
                  5120 : begin
                            //Coordinate epoch is encoded in GeoTIFF GeoKey, CoordinateEpochGeoKey code 5120 type DOUBLE
                            MessageToContinue('found CoordinateEpochGeoKey');
                         end;
               end;
               Aline := IntegerToString(tag,7) + '  ' + TIFFTypeName(FType) + IntegerToString(LengthIm,7) + IntegerToString(TiffOffset,8) + '  ' + GeoTiffTagName(Tag) + '   ' + TStr;
               {$IfDef RecordProcessingHeader} WriteLineToDebugFile(ALine); {$EndIf}
               HeaderLogList.Add(aLine);
               {$If Defined(TrackProjection)} MapProjection.WriteProjectionParametersToDebugFile('After tag' + IntToStr(i) + '  key=' + IntToStr(tag)); {$EndIf}
            end {for i};

            if (MapProjection.PName = UTMEllipsoidal) then MapProjection.False_east := 500000;
            {$If Defined(RecordFullGeotiff) or Defined(TrackProjection)} MapProjection.WriteProjParamsToDebugFile('Keys read'); {$EndIf}

            if (WeKnowItsUTMZone <> -99) then begin
               MapProjection.StartUTMProjection(WeKnowItsUTMZone);
               MapProjection.H_datumcode := 'NAD83';  //vmDatum := MapProjNAD83;
            end;

            {$If Defined(RecordDefineDatum) or Defined(RecordPlateCaree)}
               WriteLineToDebugFile('Before (GeoASCIIParamsOffset <> 0) Datum code: ' + MapProjection.H_datumCode);
               WriteLineToDebugFile('Projection=' + MapProjection.GetProjName);
            {$EndIf}
            {$IfDef TrackA} WriteLineToDebugFile('Geotiff tags read out, a=' + RealToString(MapProjection.a,-18,-2) + '  datum=' + MapProjection.H_datumCode); {$EndIf}

            if (ASCIIstr <> '') and (Not ProjectionDefined) then ProcessASCIIstringForProjection(ASCIIStr);

         {$IfDef TrackProjection} MapProjection.ProjectionParamsToDebugFile('ReadGeotiffTags out'); {$EndIf}
         {$If Defined(RecordDefineDatum) or Defined(RecordPlateCaree) or Defined(RecordGeotiffProjection)} WriteLineToDebugFile(' ReadGeotiffTags out, Projection=' + MapProjection.GetProjName); {$EndIf}
         {$If Defined(Record3076) or Defined(RecordInitDEM)} WriteLineToDebugFile('Out geotiff tags ModelX=' + RealToString(TiffHeader.ModelX,-18,-6) + ' ModelY=' + RealToString(TiffHeader.ModelY,-18,-6) ); {$EndIf}
         {$IfDef RecordprojProgress} MapProjection.WriteProjectionSummaryToDebugFile('End ReadGeotiffTags: '); {$EndIf}
     end {ReadGeotiffTags};


      procedure LoadGeotiffProjection;
      begin
         if (GeoKeyDirectoryOffset = 0) then begin
            if TiffHeader.ModelType = -99 then begin
               if (TiffHeader.ScaleX < 0.01) and (TiffHeader.ScaleY < 0.01) and (abs(TiffHeader.ModelY) < 100) and (abs(TiffHeader.ModelX) < 200) then TiffHeader.ModelType := 2 else TiffHeader.ModelType := 1;
               MapProjection.GetProjectParameters;
            end
            else TryWorldFile;
         end
         else begin
            ReadGeotiffTags;
            {$If Defined(TrackHorizontalDatum) or Defined(RecordUTM)} WriteLineToDebugFile('ReadGeotiffTags done, datum=' + MapProjection.h_DatumCode + '  UTM zone=' + IntToStr(MapProjection.projUTMzone)); {$EndIf}
            {$If Defined(TrackWKTstring)} WriteLineToDebugFile('LoadGeotiffProjection in, wkt=' + IntToStr(length(MapProjection.wktString))); {$EndIf}
            GeoSuccess := ProjectionDefined;
           {$IfDef RecordDefineDatum} MapProjection.ShortProjInfo('UTM zone after first round:');     {$EndIf}
            if GeoSuccess and NeedToLoadGeotiffProjection then begin
               if (MapProjection.projUTMZone = -99) then begin
                  GetUTMZoneNotAlreadyDefined;
               end;
               {$IfDef RecordFullGeotiff} WriteLineToDebugFile('Call GetProjectParameters '+ MapProjection.GetProjName); {$EndIf}
               {$IfDef TrackA} MapProjection.ShortProjInfo('Geotiff Call MapProjection.GetProjectParameters'); {$EndIf}
               MapProjection.GetProjectParameters;

               if (MapProjection.PName in [SphericalStereographic,EquidistantCylindrical]) then begin
               end
               else if (MapProjection.H_datumCode = '') then begin
                  ProcessASCIIstringForProjection(ASCIIStr);
                  if (MapProjection.H_datumCode = '') then begin
                     MapProjection.H_datumCode := MDDef.DefaultDigitizeDatum;
                     if (ASCIIStr <> '') then ASCIIStr := MessLineBreak + ASCIIStr;

                     if AnswerIsYes('Does image have registration information ' + ASCIIStr) then begin
                        {$IfDef VCL}
                           GetMapParameters(TiffHeader.HemiChar,MapProjection.projUTMZone,MapProjection.H_DatumCode);
                        {$EndIf}
                        if (TiffHeader.ScaleX < 0.01) and (TiffHeader.ScaleY < 0.01) then TiffHeader.ModelType := 2 else TiffHeader.ModelType := 1;
                     end
                     else GeoSuccess := false;
                     if TiffHeader.HemiChar = 'N' then Hemi := 45 else Hemi := -45;
                     MapProjection.projUTMZone := Zone;
                  end;
               end;
               if (Hemi > 0) then TiffHeader.HemiChar := 'N' else TiffHeader.HemiChar := 'S';
            end;
            {$IfDef TrackHorizontalDatum} WriteLineToDebugFile('GeoSuccess and NeedToLoadGeotiffProjection out, datum=' + MapProjection.h_DatumCode); {$EndIf}
         end;

         GeoSuccess := (abs(TiffHeader.ScaleY) > 0.00000001);  // and not(IsNaN(TiffHeader.ModelX);

         if GeoSuccess then begin
            RegVars.UpLeftY := TiffHeader.ModelY + TiffHeader.RasterY*TiffHeader.ScaleY;
            RegVars.UpLeftX := TiffHeader.ModelX - TiffHeader.RasterX*TiffHeader.ScaleX;
            RegVars.pr_deltaY  := TiffHeader.ScaleY;
            RegVars.pr_deltaX  := TiffHeader.ScaleX;
            RegVars.Registration := RegProjection;
            RegVars.pName := MapProjection.pName;
            HeaderLogList.Add('GEOTIFF registration analyzed OK');
            {$IfDef RecordFullGeotiff} MapProjection.WriteProjParamsToDebugFile('Tiff Read Projection'); {$EndIf}
         end;
         {$IfDef RecordprojProgress} MapProjection.WriteProjectionSummaryToDebugFile('end LoadGeotiffProjection: '); {$EndIf}
         {$If Defined(TrackWKTstring)} WriteLineToDebugFile('LoadGeotiffProjection out, map wkt=' + IntToStr(length(MapProjection.wktString))); {$EndIf}
      end;
   {$EndIf}

   function ReadTiffFileHeader : boolean;
   var
      b : array[1..16] of byte;
   begin
      try
         FileRead(TiffHandle,B,4);
         if (B[3] = 43) or (B[4] = 43) then BigTiff := true
         else if ((B[3] = 42) or (B[4] = 42)) then BigTiff := false
         else begin
            {$IfDef RecordGeotiffFailures} WriteLineToDebugFile('Header ' + IntToStr(B[1]) + '/' +IntToStr(B[2]) + '/' + IntToStr(B[3]) + '/' + IntToStr(B[4]) + ' Not TIFF ' + inFileName); {$EndIf}
            if false then begin
               Success := false;
               CloseTiffFile;
               exit;
            end;
            BigTiff := true;
         end;

         BigEndian := (b[1] = 77);
         HeaderLogList.Add(inFileName);

         if BigTiff then begin
            OffsetByteSize := 8;
            MakeWord;  //OffsetByteSize, has to be 8
            MakeWord;  //has to be 0
         end
         else begin
            OffsetByteSize := 4;  //not in header for original TIFF specs
         end;

         FirstIFD := MakeOffset;
         //move to first IFD
         FileSeek(TiffHandle,FirstIFD,0);
         if BigTiff then TiffHeader.NumEnt := MakeUnsigned8Byte
         else TiffHeader.NumEnt := MakeWord;

         TStr := ',  entries=' + IntToStr(TiffHeader.NumEnt);
         if BigEndian then TStr := 'Big Endian' + TStr
         else TStr := 'Little Endian (Intel)' + TStr;
         HeaderLogList.Add(TStr);
         HeaderLogList.Add('First IFD offset ' + IntToStr(FirstIFD));
         TStr := '';
         Result := true;
      Except
         on Exception do Result := false;
      end;
   end;

var
   Lat,Long : float64;
begin
   Success := false;
   if (Not(FileExists(inFileName))) or (GetFileSize(inFileName) = 0) then begin
      {$If Defined(RecordGeotiff) or Defined(RecordGeotiffFailures)} WriteLineToDebugFile('Read GEOTIFF file missing: ' + inFileName); {$EndIf}
      exit;
   end;
   {$If Defined(RecordGeotiff) or Defined(RecordEntryInGeotiff)} WriteLineToDebugFile('Read GEOTIFF: ' + ExtractFileName(inFileName) + '  ' + SmartMemorySizeBytes(GetFileSize(inFileName))); {$EndIf}

   {$IfDef FMX}
      CanEnhance := false;
   {$Else}
      CanEnhance := (not StrUtils.AnsiContainsText(UpperCase(inFileName),'DRG')) and (not StrUtils.AnsiContainsText(UpperCase(inFileName),'USTOPO')) and (not StrUtils.AnsiContainsText(UpperCase(inFileName),'NAIP'));
   {$EndIf}
   if (not CanEnhance) then GetHistogram := false;
   TIFFFileName := inFileName; //outside the loop, since we might use GDAL and change the file name

   MapProjection := BaseMap.tMapProjection.Create('geotiff');
   MapProjection.ProjCoordTransGeoKey := -99;
   MapProjection.H_datumCode := '';
   {$IfDef RecordprojProgress} MapProjection.WriteProjectionSummaryToDebugFile('Projection created: '); {$EndIf}

   HeaderLogList := tStringList.Create;
   NeedWKTHailMary := false;
   BigRow := nil;
   Row16Bit := nil;
   Row8Bit := Nil;
   OriginalFileName := inFileName;  //for Coastal DEM, and we can correct the data spacing to 1" instead of 0.9997"
 RestartGeotiff:;
   //HeaderLogList.Clear;
   {$IfDef RecordGeotiffRestart} WriteLineToDebugFile('Start/RestartGeotiff ' + TIFFFileName); {$EndIf}
   InitializeValues;
   OpenTiffFile;

   if not ReadTiffFileHeader then begin
      exit;
   end;

   if (TiffHeader.NumEnt > 100) or (TiffHeader.NumEnt < 10) then begin
      Success := false;
      {$If Defined(RecordGeotiffFailures) or Defined(RecordProblems)} WriteLineToDebugFile('Geotiff read NumEnt problem=' + IntToStr(TiffHeader.NumEnt) + '  ' + inFileName); {$EndIf}
      Success := false;
      CloseTiffFile;
      if not HeavyDutyProcessing then MessageToContinue('Tiff read problem; check that file is not open in another program ' + InFileName);
      exit;
   end;

   HeaderLogList.Add('');
   HeaderLogList.Add('Tag no/TiffTypeName/LengthIm/Offset/TiffTagName');
   HeaderLogList.Add('');

   ReadTIFFtags;

   if MetadataOnly then begin
      //don't need to worry about tiled or compressed geotiffs and rewriting
   end
   else if TiffMustBeRewritten then begin
      HeaderLogList.Insert(1,'Problem image');
      Success := false;
      CloseTiffFile;
      if (TiffHeader.OffsetArray <> Nil) then FreeMem(TiffHeader.OffsetArray,OffsetArraySize);

      if ValidPath(GDALtools_Dir) then begin
         {$If Defined(RecordGeotiffFailures) or Defined(RecordGeotiffRestart)}
            if TiledImage then WriteLineToDebugFile('Try GDAL fix for tiled image problem ' + inFileName)
            else if (TiffHeader.BitsPerSample in [4]) then WriteLineToDebugFile('Try GDAL fix for 4 bit image problem ' + inFileName)
            else WriteLineToDebugFile('Try GDAL fix for compression problem (mode=' + IntToStr(TiffHeader.Compression) + ')   ' + inFileName);
         {$EndIf}
         //Jan 2024 this fails, and despite a lot of work, I cannot figure out why
         if (TiffHeader.BitsPerSample in [4]) then begin
            GDALConvert4BitGeotiff(TIFFFileName);
         end
         else begin
            if TemporaryNewGeotiff and (Uppercase(ExtractFilePath(TIFFFileName)) <> UpperCase(MDTempDir)) then begin
               if not GDALConvertSingleImageToGeotiff(TIFFFileName) then begin
                  exit;
               end;
            end
            else GDALConvertImagesToGeotiff(TIFFFileName,true);
         end;
         {$If Defined(RecordGeotiffFailures) or Defined(RecordGeotiffRestart)} WriteLineToDebugFile('GDAL done; restart Geotiff'); {$EndIf}
         goto RestartGeotiff;
      end
      else begin
         MessageToContinue('Geotiff requires GDAL to process.');
         exit;
      end;
   end;

  SkipThumbnail:;
     {$IfDef TrackModelType} WriteLineToDebugFile('Done J loop, TiffHeader.ModelType=' + IntToStr(TiffHeader.ModelType) ); {$EndIf}
     {$IfDef RecordFullGeotiff} WriteLineToDebugFile('Done J loop, skipthumbnail'); {$EndIf}
      if not FirstImage then begin
         MenuStr := 'Multiple images in the file; all but the first ignored';
         HeaderLogList.Add(MenuStr);
         MessageToContinue(MenuStr);
      end;
     {$IfDef RecordFullGeotiff} WriteLineToDebugFile('GEOTIFF Header traversed'); {$EndIf}

      if (TiffHeader.RowsPerStrip <> 0) then TiffHeader.StripsPerImage := succ(TiffHeader.ImageLength) div TiffHeader.RowsPerStrip;
      TiffHeader.FirstImageOffset := TiffHeader.StripOffsets;
      if (TiffHeader.RowsPerStrip = 0) then TiffHeader.RowsPerStrip := TiffHeader.ImageLength;
      ImageBytesPerRow := TiffHeader.BytesPerSample * TiffHeader.ImageWidth * TiffHeader.SamplesPerPixel;

      if (not (TiffHeader.BitsPerSample in [4,8,15,16,32])) and ((TiffHeader.BitsPerSample <> 3) and (not (TiffHeader.PhotometricInterpretation in [1,2]))) then begin
         Success := false;
         MenuStr := ExtractFileName(TiffFileName) +  ' Unsupported bits per sample (key 258)=' + IntToStr(TiffHeader.BitsPerSample);
         {$IfDef ReportKey258} MessageToContinue(MenuStr); {$Endif}
         {$If Defined(RecordFullGeotiff) or Defined(RecordGeotiffFailures)} WriteLineToDebugFile(MenuStr + ' in ' + inFileName); {$EndIf}
         HeaderLogList.Insert(1,MenuStr);
         CloseTiffFile;
         exit;
      end;

       {$IfDef RecordPlateCaree} WriteLineToDebugFile('Line 1950, '  + MapProjection.GetProjName); {$EndIf}
       if GeoSuccess then begin
          {$IfDef RecordFullGeotiff} WriteLineToDebugFile('GEOTIFF image keys analyzed OK ' + MapProjection.GetProjName) {$EndIf}
       end
       else begin
          {$If Defined(RecordFullGeotiff) or Defined(RecordModelType)} WriteLineToDebugFile('GEOTIFF image keys problem found'); {$EndIf}
       end;

      {$IfDef TrackModelType} WriteLineToDebugFile('Time for projection TiffHeader.ModelType=' + IntToStr(TiffHeader.ModelType) ); {$EndIf}

   {$IfDef NoGeotiffProjection}
   {$Else}
      if (not NoGeo) and NeedToLoadGeoTiffProjection then begin
         LoadGeotiffProjection;
      end;
   {$EndIf}

   CloseTiffFile;
   if ((not (MapProjection.PName in [PlateCaree,UTMellipsoidal])) and (MapProjection.wktString = '')) or NeedWKTHailMary then begin
      // must be after CloseTiffFile;
      MapProjection.wktString := CreateWKTstringForGeotiff(TIFFFileName);
      MapProjection.InitProjFromWKTstring(MapProjection.wktString);
   end;

   {$IfDef RecordInitDEM} WriteLineToDebugFile(' closed tiff file,  ModelX=' + RealToString(TiffHeader.ModelX,-18,-6) + ' ModelY=' + RealToString(TiffHeader.ModelY,-18,-6) ); {$EndIf}

   if (not MetadataOnly) then begin
      if (TiffHeader.BitsPerSample in [15,16]) and (TiffHeader.SamplesPerPixel > 1) then begin
         New(BigRow);
      end;
      (*
      else if (TiffHeader.PhotometricInterpretation = 2) or (TiffHeader.SamplesPerPixel > 1) then begin    //color image
         if (TiffHeader.BitsPerSample in [15,16]) then begin
            new(Row16Bit);
         end
         else begin
            New(Row8Bit);
         end;
      end
      else begin  //single band image
      *)
         if (TiffHeader.BitsPerSample in [15,16]) then begin
            new(Row16Bit);
         end
         else if (TiffHeader.BitsPerSample in [4,8]) then begin
            new(Row8Bit);
         end;
      //end;

      {$IfDef ExSat}
      {$Else}
         if (TiffHeader.BitsPerSample in [8,15,16]) and GetHistogram then begin
            {$IfDef RecordMinMax} WriteLineToDebugFile('Point 1,   Min sample='+IntToStr(MinSampleValue[1]) + ' Max sample='+IntToStr(MaxSampleValue[1])); {$EndIf}
            if (not FileExists(HistogramLandsatName(TIFFFileName))) then begin
               GetHistogramDBF(BandNum);
               {$IfDef RecordMinMax}
                  for i := 1 to SamplesPerPixel do begin
                     HeaderLogList.Add('Sample range after 16 bit histogram, Band ' + IntToStr(i)  + RealToString(MinSampleValue[i],12,-4) + RealToString(MaxSampleValue[i],12,-4));
                  end;
               {$EndIf}
            end;
         end;
      {$EndIf}
   end;

   TemporaryNewGeotiff := true;
   if (MapProjection.projUTMzone <= 0) then begin
      MapProjection.InverseProjectDegrees(TiffHeader.ModelX,TiffHeader.ModelY,Lat,Long);
      MapProjection.projUTMzone := GetUTMzone(long);
   end;

   {$IfDef RecordDefineDatum}
      MapProjection.ShortProjInfo('Point 2, utm=');
      MapProjection.WriteProjParamsToDebugFile('Projection');
      WriteLineToDebugFile('Geotiff done OK, datum=' + MapProjection.H_datumCode);
   {$EndIf}

   {$IfDef RecordGeotiff} WriteStringListToDebugFile(HeaderLogList); {$EndIf}
   {$IfDef RecordBitPerPixel} WriteLineToDebugFile('leaving read Geotiff, BitsPerSample=' + IntToStr(TiffHeader.BitsPerSample) + ' bytes=' + IntToStr(TiffHeader.BytesPerSample) + 'can enhance=' + TrueOrFalse(CanEnhance)); {$EndIf}
   {$IfDef TrackA} WriteLineToDebugFile('read Geotiff out, a=' + RealToString(MapProjection.a,-18,-2) + '  f=' + RealToString(MapProjection.h_f,-18,-6) + '  datum=' + MapProjection.H_datumCode); {$EndIf}
   {$IfDef TrackProjection} MapProjection.ProjectionParamsToDebugFile('ReadGeotiffTags out'); {$EndIf}
   {$IfDef TrackHorizontalDatum} WriteLineToDebugFile('GeoSuccess and NeedToLoadGeotiffProjection out, datum=' + MapProjection.h_DatumCode); {$EndIf}
   {$IfDef TrackPixelIs} WriteLineToDebugFile('read Geotiff ' + ExtractFileName(InFileName) + ' out, ' + RasterPixelIsString(TiffHeader.RasterPixelIs)); {$EndIf}
   {$IfDef RecordUTM} WriteLineToDebugFile('read Geotiff ' + ExtractFileName(InFileName) + ' out, UTM zone=' + IntToStr(MapProjection.projUTMzone)); {$EndIf}
   {$IfDef RecordprojProgress} MapProjection.WriteProjectionSummaryToDebugFile('Read done: '); {$EndIf}

   if ShowHeader then begin
      ShowInNotepadPlusPlus(HeaderLogList,'MD_metadata_' + ExtractFileName(InFileName));
      {$If Defined(RecordJustMetadata)} WriteLineToDebugFile('ShowHeader done'); {$EndIf}
   end
   else begin
      //HeaderLogList.Free;
      FreeAndNil(HeaderLogList);
      {$IfDef RecordGeotiff} WriteLineToDebugFile('read Geotiff out, HeaderLogList.Freed'); {$EndIf}
   end;
   Success := true;
   {$If Defined(RecordGeotiff) or Defined(RecordJustMetadata)} WriteLineToDebugFile('TiffImageCreate out'); {$EndIf}
end;


destructor tTIFFImage.Destroy;
begin
   {$IfDef RecordGeotiffDestroy} WriteLineToDebugFile('enter tTIFFImage.Destroy ' + TIFFFileName); {$EndIf}
   if (Row8Bit <> Nil) then Dispose(Row8bit);
   if (Row16Bit<> Nil) then Dispose(Row16bit);
   if (BigRow <> Nil) then Dispose(BigRow);

   if (MapProjection <> Nil) then MapProjection.Destroy;

   if (TiffHeader.OffsetArray <> Nil) then FreeMem(TiffHeader.OffsetArray,OffsetArraySize);
   CloseTiffFile;
   {$IfDef RecordGeotiffDestroy} WriteLineToDebugFile('exit tTIFFImage.Destroy ' + TIFFFileName); {$EndIf}
end;


initialization
   NeedToLoadGeotiffProjection := true;
   TemporaryNewGeotiff := true;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing geotiff out'); {$EndIf}
end.



