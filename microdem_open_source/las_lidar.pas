unit las_lidar;
   
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

//{$Define NoInLine}   {use this to be able to trace calls into these routines}


{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define RecordLASMemoryAlocations}
      //{$Define RecordLASfiles}
      {$Define RecordWKT}
      {$Define RecordVarLenRec}
      //{$Define RecordFirstPulse}
      //{$Define RecordReprojectLAS}
      //{$Define RecordLASplot}
      //{$Define RecordListFilesProcessed}
      //{$Define RecordLASKML}
      //{$Define RecordLASexport}
      //{$Define RecordMergeLASfiles}
      //{$Define RecordCreateLASfiles}
      //{$Define RecordCreateEveryFile}
      //{$Define RecordLASheader}
      //{$Define RecordLASheaderKeys}
      //{$Define RecordLASprojection}
      //{$Define RecordLASColors}
      //{$Define RecordLASHist}
      //{$Define RecordLAS_subset}
   {$Else}
   {$EndIf}
{$EndIf}


interface

uses
//needed for inline of core DB functions
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
//end for inline of the core DB functions

   {$IfDef VCL}
      Graphics,Forms,DEMMapf,DEMLOSW,Winapi.Windows,Vcl.StdCtrls,
   {$EndIf}

   {$IfDef FMX}
      FMX.Graphics,FMX.Controls, FMX.Types,System.Types,
   {$EndIf}

   SysUtils,StrUtils,Classes, System.Threading,System.SyncObjs, System.UITypes, System.UIConsts,
   Petmar_types,Petmar_db,PetImage,PetMath,Petmar,
   DEMESRIShapeFile,DEMDefs,BaseMap,DEMMapDraw;

const
   MaxLASPtsToRead = 1024000;  //256000;
   MaxGeoKeys = 8128;
   LASLatLong = 2;

type
   tLASAsciiOutput = (lasascXYYonly,lasascClassInt,lasascRGB);
   tLASHeader = packed record
      FileSignature : array[1..4] of ANSIchar;
      FileSourceID,
      GlobalEncoding : word;      //bit 0 is standard GPS time minus 10^9, and bit 4 is WKT georef
      GUID_data1 : cardinal;
      GUID_data2,
      GUID_data3 : word;
      GUID_data4 : array[1..8] of ANSIchar;
      VersionMajor,
      VersionMinor : ANSIchar;
      SystemID,
      GenerateSoftware : array[1..32] of ANSIchar;
      CreateDay,
      CreateYear,
      HeaderSize : word;
      OffsetToData,
      NumVarLenRecs : cardinal;
      PointDataFormat : ANSIchar;
      PointDataRecLen : word;
      NumPtRecs : cardinal;
      NumPtsByReturn : array[1..5] of cardinal;
      XscaleFac,YscaleFac,ZscaleFac,
      Xoffset,Yoffset,Zoffset,
      MaxX,MinX,
      MaxY,MinY,
      MaxZ,MinZ : float64;
   end;

   tLas14Header = packed record
      StartWaveform : UInt64;
      StartELVR1 : UInt64;
      NumELVR : UInt32;
      NumPtRecs : UInt64;
      NumPtsByReturn : array[1..15] of UInt64;
   end;

   tELVRHeader = packed record   //p.20 of Las 1.4 documentation
      Reserved : UInt16;
      UserID : array[1..16] of ANSIChar;
      RecordID : UInt16;
      RecLenAfter : UInt64;
      Description : array[1..32] of ANSIChar;
   end;

   tLidarPointType0 = packed record
      xPt,yPt,zPt : Int32;
      Intensity : word;
      ReturnFlags : byte;     //return number in bits 0-2, the number of returns from pulse in bits 3-5
      Classification : byte;
      ScanAngle : ShortInt;
      UserData : byte;        // should be ANSIchar:
      PointSourceID : word;
   end;

   tLidarPointType1 = packed record
      lp0     : tLidarPointType0;
      GPSTime : double;
   end;
   tLidarPointType2 = packed record
      lp0 : tLidarPointType0;
      Red,Green,Blue : Word;
   end;
   tLidarPointType3 = packed record
      lp0 : tLidarPointType0;
      GPSTime : double;
      Red,Green,Blue : Word;
   end;
   tLidarPointType4 = packed record
      lp0 : tLidarPointType0;
      GPSTime : float64;
      WavePacketDescIndex : ANSIchar;
      OffsetToWaveform    : int64;
      WaveformPacketSize  : int32;
      ReturnPointWaveformLocation,
      xt,yt,zt  : Float32;
   end;
   tLidarPointType5 = packed record
      lp0 : tLidarPointType0;
      GPSTime : double;
      Red,Green,Blue : byte;
      WavePacketDescIndex : ANSIchar;
      OffsetToWaveform    : int64;
      WaveformPacketSize  : int32;
      ReturnPointWaveformLocation : Float32;
      xt,yt,zt  : Float32;
   end;

   tLidarPointType6 = packed record    //LAS 1.4 baseline
      xPt,yPt,zPt : Int32;
      Intensity : word;
      ReturnFlags : byte;             //return number in bits 0-3, the number of returns from pulse in bits 4-7
      ClassificationFlags : byte;     //plus scanner channel, direction
      Classification : byte;
      UserData : byte;
      ScanAngle : Int16;
      PointSourceID : word;
      GPSTime       : double;
   end;
   tLidarPointType7 = packed record
      lp6 : tLidarPointType6;
      Red,Green,Blue : word;
   end;
   tLidarPointType8 = packed record
      lp6 : tLidarPointType6;
      Red,Green,Blue,NIR : word;
   end;

   tVarLenRecHeader = packed record
      Reserved : word;
      UserID : array[1..16] of ANSIchar;
      RecordId : word;
      RecLenAfterHeader : word;
      Description : array[1..32] of ANSIchar;
   end;
   tExtendedVarLenRecHeader = packed record
      Reserved : word;
      UserID : array[1..16] of ANSIchar;
      RecordId : word;
      RecLenAfterHeader : UInt64;
      Description : array[1..32] of ANSIchar;
   end;

   tsGeoKeys = packed record
      wKeyDirVersion,
      wKeyRev,
      wMinorRev,
      wNumberOfKeys : word;
   end;
   tsKeyEntry = packed record
      wKeyID,wValueOffset : word;
   end;

   tlasProjectionDefinition = record
       RawXYZFile : boolean;
       LASProjection : tMapProjection;
   end;

   tLidarPoints0 = array[1..MaxLASPtsToRead] of tLidarPointType0;
   tLidarPoints1 = array[1..MaxLASPtsToRead] of tLidarPointType1;
   tLidarPoints2 = array[1..MaxLASPtsToRead] of tLidarPointType2;
   tLidarPoints3 = array[1..MaxLASPtsToRead] of tLidarPointType3;
   tLidarPoints4 = array[1..MaxLASPtsToRead] of tLidarPointType4;
   tLidarPoints5 = array[1..MaxLASPtsToRead] of tLidarPointType5;
   tLidarPoints6 = array[1..MaxLASPtsToRead] of tLidarPointType6;
   tLidarPoints7 = array[1..MaxLASPtsToRead] of tLidarPointType7;
   tLidarPoints8 = array[1..MaxLASPtsToRead] of tLidarPointType8;

   tSFColorArray  = array[1..3] of float32;
   tCatSet = set of byte;
   tBigBuffer = array[1..MaxLASPtsToRead*50] of byte;

   tLAS_data = class
      private
         bb : ^tBigBuffer;
         VarLenRecHeader : array[1..MaxGeoKeys] of tVarLenRecHeader;
         FileGeoKeys : array[1..MaxGeoKeys] of tsKeyEntry;
         function GetHowManyReadsRequired : integer;
         procedure NilRecordMemory;
         procedure GetShotScreenCoordinatesUTM(BaseMapDraw : tMapDraw; j: integer; var x, y: integer);  {$IfDef NoInLine} {$Else} inline; {$EndIf}
         procedure FigureOffsets(CenterOnKM, CenterZ: boolean; var xoff, yoff,zoff: float64);
         procedure ExtraBytesPrep;
      public
         LasHeader : tLasHeader;
         Las14Header : tLas14Header;
         NumPointRecs : uInt64;
         StartWaveFormDataPackedRecord : int64;
         ELVRHeader : tELVRHeader;
         LasFile : file;
         LasFileName : PathStr;
         BaseLength,
         LidarPointType : byte;
         lasProjectionDefinition : tlasProjectionDefinition;
         HasProjection,
         Icesat2Cloud,
         DataHasExtraBytes,
         RGBPresent,
         WKT : boolean;
         SingleFilePointDensity,
         SingleFilePulseDensity,
         MeterXscaleFac,MeterYscaleFac,MeterZscaleFac,
         LAS_x_range,LAS_y_range,LAS_Z_range : float64;
         LAS_UTM_Box,
         LAS_LatLong_Box : sfboundBox;
         ASCIIProjectionData : shortString;
         UTMZone  : int16;
         ModelType,
         ReadsRequired,
         NumGeotiffKeys : int32;

         LidarPoints0 : ^tLidarPoints0;
         LidarPoints1 : ^tLidarPoints1;
         LidarPoints2 : ^tLidarPoints2;
         LidarPoints3 : ^tLidarPoints3;
         LidarPoints4 : ^tLidarPoints4;
         LidarPoints5 : ^tLidarPoints5;
         LidarPoints6 : ^tLidarPoints6;
         LidarPoints7 : ^tLidarPoints7;
         LidarPoints8 : ^tLidarPoints8;

         constructor Create(fileName : PathStr);
         destructor Destroy;
         procedure PrepDataRead;
         procedure FreeLASRecordMemory;

         function GetMetadata : tStringList;
         procedure PlotTileOnMap(Cloud: integer; BaseMapDraw : tMapDraw; var BMPMemory : tBMPMemory; MinAreaZ, MaxAreaZ : float64);
         function LASClassificationCategory(j: integer): tLASClassificationCategory;

         function FileOnMap(BaseMapDraw : tMapDraw) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function InBoundBoxGeo(BoundBox : sfBoundBox): boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function InBoundBoxUTM(BoundBox : sfBoundBox): boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function FileOnDEM(DEM: integer): boolean; inline;

         procedure GetColor(j: integer; MinAreaZ,MaxAreaZ : float64; var Color : tPlatformColor); //inline;
         procedure GetColorAndElevation(j : integer; MinAreaZ,MaxAreaZ : float64; var ColorRGB : tPlatformColor; var ze : float64);
         function GetRGBColor(j : integer) : tPlatformColor; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function GetRGBComponents(j : integer; var r,g,b : byte) : boolean;  {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function GetRGBWord(j : integer; var r,g,b : Word) : boolean;
         function GetNIRWord(j : integer; var n : Word) : boolean;

         procedure ReadPoints(var RecsRead : integer); {$IfDef NoInLine} {$Else} inline; {$EndIf}

         procedure GetShotCoordinatesLatLong(j : integer; var Lat,Long : float64); {$IfDef NoInLine} {$Else} inline; {$EndIf}
         procedure GetShotCoordinatesUTM(j : integer; var xUTM,yUTM : float64); {$IfDef NoInLine} {$Else} inline; {$EndIf}
         procedure GetShotCoordinatesAppropriate(GoUTM : boolean; j : integer; var xApp,yApp : float64); {$IfDef NoInLine} {$Else} inline; {$EndIf}
         procedure GetShotScreenCoordinatesAppropriate(BaseMapDraw : tMapDraw; j : integer; var x,y : integer); {$IfDef NoInLine} {$Else} inline; {$EndIf}
         procedure ReviseCoordinates(j : integer; x,y,z : float64; NewLasHeader : tLasHeader);
         function ExpandLAS_X(ShotNumber : integer) : float64; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function ExpandLAS_Y(ShotNumber : integer) : float64; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function ExpandLAS_Z(ShotNumber : integer) : float64; {$IfDef NoInLine} {$Else} inline; {$EndIf}

         function ColorFilterOut(j : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function UTMLikeFile(BaseMapDraw : tMapDraw) : boolean;

         function WithheldPoint(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function OverlapPoint(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function FirstReturn(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function AirReturn(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function NoiseReturn(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function GroundReturn(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function LastReturn(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function SoloReturn(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function ReturnNumber(ShotNumber : integer) : byte; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function UserDataRec(ShotNumber : integer) : byte; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function PointSourceID(ShotNumber : integer) : integer; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function ReturnsInPulse(ShotNumber : integer) : byte; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function LASClassification(ShotNumber : integer) : byte; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function GetShotIntensity(j : integer) : byte; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function GetShotMeasuredIntensity(j : integer) : word; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function GetGPSTime(j : integer) : float64; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function GetScanAngle(j : integer) : float64; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function GoodNoiseOverlap(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}

         procedure SetLASClassification(ShotNumber : integer; TheClass : byte); {$IfDef NoInLine} {$Else} inline; {$EndIf}

         function MeetsFilter(ShotNumber : integer) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}

         {$IfDef MSWindows}
            function OldExportBinary(Layer : integer; var GeometryFName,ColorsFName : PathStr; ExportFilter : tLASClassificationCategory = lccAll) : boolean;
            procedure SplitLASfile;
            procedure ExportXYZ(Output : tLASAsciiOutput; ScaleFactor : float64 = -99; CenterOutput : boolean = true);
            procedure ExportGeoJSON;
            procedure ExportXYZ_DB(LatLong : boolean; fName : PathStr; MaxPoint : int64);
            procedure ExportIcesat(MapOwner : tMapForm; Lat,Long : float64);
         {$EndIf}

         {$IfDef VCL}
            procedure OutlineOnMap(BaseMap : tMapForm);
         {$EndIf}
   end;

   tCreateLasFile = class
     private
         OutputBuffer0 : ^tLidarPoints0;
         OutputBuffer1 : ^tLidarPoints1;
         OutputBuffer2 : ^tLidarPoints2;
         OutputBuffer3 : ^tLidarPoints3;
         OutputBuffer4 : ^tLidarPoints4;
         OutputBuffer5 : ^tLidarPoints5;
         OutputBuffer6 : ^tLidarPoints6;
         OutputBuffer7 : ^tLidarPoints7;
         OutputBuffer8 : ^tLidarPoints8;
         procedure ShotOverheadOutputBuffer(lp0: tLidarPointType0);  overload; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         procedure ShotOverheadOutputBuffer(lp6: tLidarPointType6);  overload; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         procedure DefineHeader;
         procedure FlushBuffer;
     public
         NewLasFile : file;
         NewLasFileName : PathStr;
         NewPtType : integer;
         NumPointRecs,
         PointsInOutputBuffer : int64;
         NewLasHeader : tLasHeader;
         jXscaleFac,jYscaleFac,jZscaleFac,
         jXoffset,jYoffset,jZoffset : float64;
         FirstLAS : integer;

         constructor Create;
         destructor Destroy;
         procedure CreateNewLASfile(fName : PathStr; NewLasProjectionDefinition : tlasProjectionDefinition; PtType : byte);  overload;
         procedure CreateNewLASfile(fName : PathStr; NewLasProjectionDefinition : tlasProjectionDefinition; CopyLASHeader : tLASHeader);  overload;
         procedure AddShotToOutputBuffer(lf : tLAS_data; Pt : integer); overload; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         procedure AddShotToOutputBuffer(lp0: tLidarPointType0); overload;  //needed for vegetation density
         procedure AddShotToOutputBuffer(lp1: tLidarPointType1); overload;
         procedure ZeroHeaderRange;   //must be public
   end;

const
   MaxLasCat = 255;
   MaxReturns = 15;
var
   Las_rgb_colors : array[0..MaxLasCat] of tPlatformColor;
   Las_ogl_colors : array[0..MaxLasCat] of tSFColorArray;
   Las_ret_colors : array[1..MaxReturns] of tPlatformColor;
   LasCatName : array[0..MaxLasCat] of ShortString;
   LASCatMapped,
   LasCatUsed : array[0..MaxLASCat] of boolean;

procedure InitializeLASColors(LAS_Versn : shortstring = '1.3');
function NoFilterWanted : boolean;
procedure CreateNewLasFromOldHeader(NewName : PathStr; var NewLas : tCreateLasFile; lf : tLAS_data{; RemoveOffsets : boolean});

procedure ReprojectLasFileToUTM(fName : PathStr; UTMZone : integer; StartProjection : BaseMap.tMapProjection; StartDatum : shortString; zscaler : float64 = 1);
procedure ReprojectIrishLasFileToUTM(fName : PathStr);
procedure ReprojectOKUSLasFileToUTM(fName : PathStr);
procedure ReprojectDefinedLasFileToUTM(fName : PathStr);
procedure ZShiftDefinedLasFileToMeters(fName : PathStr);

function OffSetString(LasHeader : tLasHeader) : shortstring;
function ScaleFactorsString(LasHeader : tLasHeader) : shortstring;
function AllRangeString(LasHeader : tLasHeader)  : shortstring;
function AllFactorsString(LasHeader : tLasHeader)  : shortstring;

procedure ZeroLidarPointType0(lp0 : tLidarPointType0);
function LidarPointTypeRecordBaseLength(LidarPointType : byte) : int64;


{$IfDef MSWindows}
   procedure SubsetLASfiles(fName : PathStr; Memo1 : tMemo = nil);
   procedure LidarASCIIout(Output : tLASAsciiOutput); overload;
   procedure LidarASCIIout(fName : PathStr; Output : tLASAsciiOutput); overload;
   procedure LAS2GeoJSON; overload;
   procedure LAS2GeoJSON(fName : PathStr); overload;
{$EndIf}

var
   LasColorsDone,AllowNoProjectionLAS : boolean;

implementation

{ tLAS_data }

uses
   {$IfDef VCL}
      BaseGraf,
      Thread_Timers,
      Slicer_3D,
      KML_Creator,
   {$EndIf}
   {$IfDef OldOpenGL}
      //ogl_DEMOpenGLMain,
   {$EndIf}
   PetDBUtils,
   DEMCoord, DEMDef_routines,
   GeoTiff,
   Make_tables;


{$I ..\las_lidar\create_las.inc}
{$I ..\las_lidar\subset_export_las.inc}
{$I ..\las_lidar\reproject_las.inc}


      function OffSetString(LasHeader : tLasHeader) : shortstring;
      begin
         Result := 'Offsets: ' + RealToString(LasHeader.XOffset,-18,-6) + '  ' + RealToString(LasHeader.YOffset,-18,-6) + '  ' + RealToString(LasHeader.ZOffset,-18,-6);
      end;

      function ScaleFactorsString(LasHeader : tLasHeader) : shortstring;
      begin
         Result := 'Scaling factors: ' + RealToString(LasHeader.XscaleFac,-18,-12) + '  ' + RealToString(LasHeader.YscaleFac,-18,-12) + '  ' + RealToString(LasHeader.ZscaleFac,-18,-12);
      end;

      function XRangestring(LasHeader : tLasHeader) : shortstring;
      begin
         Result := 'X range: ' + RealToString(LasHeader.MinX,-18,-6) + '  ' + RealToString(LasHeader.MaxX,-18,-6) + ' ';
      end;

      function YRangeString(LasHeader : tLasHeader)  : shortstring;
      begin
         Result := 'Y range: ' + RealToString(LasHeader.MinY,-18,-6) + '  ' + RealToString(LasHeader.MaxY,-18,-6)  + ' ';
      end;

      function ZRangeString(LasHeader : tLasHeader)  : shortstring;
      begin
        Result := 'Z range: ' + RealToString(LasHeader.MinZ,-18,-6) + '  ' + RealToString(LasHeader.MaxZ,-18,-6) + ' ';
      end;

      function AllRangeString(LasHeader : tLasHeader)  : shortstring;
      begin
        Result := XRangeString(LASHeader) + YRangeString(LASHeader) + ZRangeString(LASHeader);
      end;

      function AllFactorsString(LasHeader : tLasHeader)  : shortstring;
      begin
         Result := AllRangeString(LasHeader) + OffsetString(LasHeader)  + ' ' + ScaleFactorsString(LasHeader);
      end;


procedure ZeroLidarPointType0(lp0 : tLidarPointType0);
begin
   lp0.Intensity := 0;
   lp0.ReturnFlags := 0;
   lp0.Classification := 0;
   lp0.ScanAngle := 0;
   lp0.UserData := 0;
   lp0.PointSourceID := 0;
end;



function LidarPointTypeRecordBaseLength(LidarPointType : byte) : int64;
begin
   if (LidarPointType = 0) then Result := SizeOf(tLidarPointType0);
   if (LidarPointType = 1) then Result := SizeOf(tLidarPointType1);
   if (LidarPointType = 2) then Result := SizeOf(tLidarPointType2);
   if (LidarPointType = 3) then Result := SizeOf(tLidarPointType3);
   if (LidarPointType = 4) then Result := SizeOf(tLidarPointType4);
   if (LidarPointType = 5) then Result := SizeOf(tLidarPointType5);
   if (LidarPointType = 6) then Result := SizeOf(tLidarPointType6);
   if (LidarPointType = 7) then Result := SizeOf(tLidarPointType7);
   if (LidarPointType = 8) then Result := SizeOf(tLidarPointType8);
end;


procedure tLAS_data.ExtraBytesPrep;
begin
   BaseLength := LidarPointTypeRecordBaseLength(LidarPointType);
   DataHasExtraBytes := BaseLength < LasHeader.PointDataRecLen;
end;


procedure InitializeLASColors(LAS_Versn : shortstring = '1.3');
const
   LidarTColor : array[1..15] of tColor = (clLime,clRed,clBlue,clGreen,clAqua,clNavy,clDkGray,clSilver,clFuchsia,clPurple,clTeal,clYellow,clOlive,clMaroon,ClBlack);
var
   Table : tMyData;
   Code,i : Integer;
   Color : integer;
   {$IfDef ExportColors}
      r,g,b : byte;
      rs,gs,bs : shortstring;
   {$EndIf}

      procedure DefineColor(var colors : tSFColorArray; var RGB_color : tPlatFormColor; color : integer);
      var
          r,g,b : byte;
      begin
         GetRGBfromTColor(Color,r,g,b);
         colors[1] := r / 255;
         colors[2] := g / 255;
         colors[3] := b / 255;
         rgb_color := RGBtrip(r,g,b);
      end;

begin
   if  MDDef.ls.AssumeLAS14classes then LAS_Versn := '1.4';

   for i := 0 to MaxLASCat do LasCatMapped[i] := true;
   if not LasColorsDone then begin
      {$IfDef RecordLASfiles} WriteLineToDebugFile('InitializeLASColors in');   {$EndIf}
      for Color := 0 to MaxLasCat do begin
         Las_ogl_colors[Color,1] := 1;
         Las_ogl_colors[Color,2] := 0;
         Las_ogl_colors[Color,3] := 0;
      end;

      {$IfDef ExportColors}
         rs := '[';
         gs := '[';
         bs := '[';
      {$EndIf}

      if FileExists(LasRulesName) then  begin
         Table := tMyData.Create(LasRulesName);
         Table.ApplyFilter('LAS_VERSN=' + QuotedStr(LAS_Versn));
         while not Table.Eof do begin
            Code := Table.GetFieldByNameAsInteger('CODE');
            Color := Table.GetFieldByNameAsInteger('COLOR');
            {$IfDef RecordLASColors} writeLineToDebugFile('code=' + IntToStr(Code)); {$EndIf}

            {$IfDef ExportColors}
               Petmar.GetRGBfromTColor(Color,r,g,b);
               rs := rs + IntToStr(r) + ',';
               gs := gs + IntToStr(g) + ',';
               bs := bs + IntToStr(b) + ',';
            {$EndIf}

            LasCatName[Code] := Table.GetFieldByNameAsString('NAME');
            DefineColor(Las_ogl_colors[Code],LAS_RGB_colors[Code],Color);
            Table.Next;
         end;
         Table.Destroy;
      end
      else begin
         {$IfDef RecordLASColors} WriteLineToDebugFile(LasRulesName + ' missing in InitializeLASColors'); {$EndIf}
         MessageToContinue(LasRulesName + ' missing');
      end;

      {$IfDef ExportColors} WriteLineToDebugFile('las_red = ' + rs + ']'); WriteLineToDebugFile('las_green = ' + gs + ']'); WriteLineToDebugFile('las_blue = ' + bs + ']'); {$EndIf}

      for i := 1 to 15 do Las_ret_colors[i] := ConvertTColorToPlatformColor(LidarTColor[i]);

      LasColorsDone := true;
      {$IfDef RecordLASColors}
         writelineToDebugFile('Colors in initial definition');
         for Color := 0 to MaxLASCat do writelineToDebugFile(IntegerToString(color,2) + IntegerToString(LAS_RGB_colors[Color].rgbtred,5) +
            IntegerToString(LAS_RGB_colors[Color].rgbtgreen,5) +  IntegerToString(LAS_RGB_colors[Color].rgbtblue,5));
      {$EndIf}
   end;
end;


procedure tLAS_data.PlotTileOnMap(Cloud : integer; BaseMapDraw : tMapDraw; var BMPMemory : tBMPMemory; MinAreaZ, MaxAreaZ : float64);
var
   I,RecsRead,x,y,pplot : integer;
   j : int64;
   NoFilter : boolean;
   ColorRGB : tPlatformColor;
begin
   if FileOnMap(BaseMapDraw) then begin
      {$IfDef RecordLASplot} writelineToDebugFile('tLAS_data.PlotOnMap ' + ExtractFileNameNoExt(LasFileName) + '   mode=' + IntToStr(Ord(MDDef.ls.ColorCoding))); {$EndIf}
      NoFilter := NoFilterWanted;
      pplot := 0;
      if (MDDef.ls.ColorCoding = lasccCloudID) then begin
         ColorRGB := MDDef.CloudMapSymbol[Cloud].Color;
      end;
      PrepDataRead;
      for i := 0 to ReadsRequired do begin
         ReadPoints(RecsRead);
         if (RecsRead > 0) then begin
            {$IfDef VCL}
               TInterlocked.Add(ThreadsNumDone,RecsRead);
               if (i mod 10 = 0) then UpdateProgressBar(ThreadsNumDone/ThreadsToDo);
            {$EndIf}
             j := 1;
             while (j <= RecsRead) do begin
                if NoFilter or MeetsFilter(j) then begin
                   GetShotScreenCoordinatesAppropriate(BaseMapDraw,j, x,y);
                   if BaseMapDraw.OnScreen(x,y) then begin
                      if (MDDef.ls.ColorCoding <> lasccCloudID) then GetColor(j,MinAreaZ,MaxAreaZ,ColorRGB);
                      {$IfDef RecordLASplot} inc(pplot); {$EndIf}
                      BMPMemory.SetPixelColorSize(x,y,MDDef.CloudMapSymbol[Cloud].Size,ColorRGB);
                  end;
                end;
               inc(j,MDDef.CloudMapThinFactor);
            end {while};
         end;
         if WantOut then break;
         {$IfDef VCL} if ShowSatProgress then ApplicationProcessMessages; {$EndIf}
      end {for i};
      FreeLASRecordMemory;
   {$IfDef RecordLASplot} WritelineToDebugFile('tLAS_data.PlotOnMap out, plot=' + IntToStr(pplot)); {$EndIf}
   end;
end;


function tLAS_data.GoodNoiseOverlap(ShotNumber : integer) : boolean;
begin
   if (MDDef.ls.BadPointFilter in [0]) then Result := true;
   if (MDDef.ls.BadPointFilter in [1,2]) then begin
      if (LASclassification(ShotNumber) in [7,18]) then Result := false;
      if (MDDef.ls.BadPointFilter = 2) and OverlapPoint(ShotNumber) then result := false;
   end;
end;


function tLAS_data.MeetsFilter(ShotNumber : integer) : boolean;
var
  Category,Beam : integer;
  z : float64;
begin
   Result := false;
   Category := LASclassification(ShotNumber);

   if (MDDef.ls.BadPointFilter in [1,2]) then begin
      if (Category in [7,18]) then exit;
      if (MDDef.ls.BadPointFilter = 2) and OverlapPoint(ShotNumber) then exit;
   end;

   if MDDef.ls.filters then begin
      if MDDef.LasElevChecks then begin
         z := ExpandLAS_Z(ShotNumber);
         if (z < MDDef.LowValidZinPointCloud) or (z > MDDef.MaxValidZinPointCloud) then exit;
      end;

      if MDDef.ls.GroundClassOnly and (Category <> 2) then exit;
      if MDDef.ls.FirstReturnsOnly and (ShotNumber <> 1) then exit;
      if MDDef.ls.DiscardLowPointsNoise and (Category = 7) then exit;
      if MDDef.ls.DiscardHighNoise and (Category = 18) then exit;
      if MDDef.ls.DiscardOverlap and OverlapPoint(ShotNumber) then exit;
      if MDDef.ls.SimpleBuildingFilter and (Category <> 1) then exit;   //only unclassified allowed
      if (MDDef.ls.CatFilter <> 0) and (MDDef.ls.CatFilter <> Category) then exit;
      if (MDDef.ls.RetFilter <> 0) and (MDDef.ls.RetFilter <> ReturnNumber(ShotNumber)) then exit;
      if MDDef.ls.AirReturnsOnly and (not AirReturn(ShotNumber)) then exit;
      if MDDef.ls.SingleReturnsOnly and (not SoloReturn(ShotNumber)) then exit;
      if MDDef.ls.LastReturnsOnly and (not LastReturn(ShotNumber)) then exit;
      if MDDef.ls.ScanAngleFiltered and (abs(GetScanAngle(ShotNumber)) > MDDef.ls.ScanAngleFilter) then exit;
      if (MDDef.ls.PointIDFiltered) and (PointSourceID(ShotNumber) <> MDDef.ls.PointIDFilter) then exit;
      if MDDef.ls.UserDataRecordFiltered and (UserDataRec(ShotNumber) <> MDDef.ls.UserDataRecordFilter) then exit;
   end;

   if Icesat2Cloud and MDDef.Icesat2.DoFilter then begin
      Beam := UserDataRec(ShotNumber);
      if not MDDef.Icesat2.UseBeam[Beam] then exit;
      if Category < MDDef.Icesat2.BeamConfidence[Beam] then exit;
   end;

   Result := true;
end;


procedure tLas_data.ReviseCoordinates(j : integer; x,y,z : float64; NewLasHeader : tLasHeader);
var
   xp,yp,zp : integer;
begin
   xp := round((x - NewLasHeader.Xoffset) / NewLasHeader.XscaleFac);
   yp := round((y - NewLasHeader.Yoffset) / NewLasHeader.YscaleFac);
   zp := round((z - NewLasHeader.Zoffset) / NewLasHeader.ZscaleFac);

   if LidarPointType = 2 then begin
      LidarPoints2^[j].lp0.xPt := xp;
      LidarPoints2^[j].lp0.yPt := yp;
      LidarPoints2^[j].lp0.zPt := zp;
   end
   else if LidarPointType = 1 then begin
      LidarPoints1^[j].lp0.xPt := xp;
      LidarPoints1^[j].lp0.yPt := yp;
      LidarPoints1^[j].lp0.zPt := zp;
   end
   else if LidarPointType = 3 then begin
      LidarPoints3^[j].lp0.xPt := xp;
      LidarPoints3^[j].lp0.yPt := yp;
      LidarPoints3^[j].lp0.zPt := zp;
   end
   else if LidarPointType = 0 then begin
      LidarPoints0^[j].xPt := xp;
      LidarPoints0^[j].yPt := yp;
      LidarPoints0^[j].zPt := zp;
   end
   else if (LidarPointType = 6) then begin
      LidarPoints6^[j].xPt := xp;
      LidarPoints6^[j].yPt := yp;
      LidarPoints6^[j].zPt := zp;
   end
   else if (LidarPointType = 7) then begin
      LidarPoints7^[j].lp6.xPt := xp;
      LidarPoints7^[j].lp6.yPt := yp;
      LidarPoints7^[j].lp6.zPt := zp;
   end
   else if (LidarPointType = 8) then begin
      LidarPoints8^[j].lp6.xPt := xp;
      LidarPoints8^[j].lp6.yPt := yp;
      LidarPoints8^[j].lp6.zPt := zp;
   end;
end;



procedure CreateNewLasFromOldHeader(NewName : PathStr; var NewLas : tCreateLasFile; lf : tLAS_data);
begin
    {$IfDef RecordLASheader} writeLineToDebugFile('CreateNewLasFromOldHeadder in, ' + NewName); {$EndIf}
    NewLas := tCreateLasFile.Create;
    NewLas.NewLasHeader := lf.LasHeader;
    NewLAS.CreateNewLASfile(NewName,lf.lasProjectionDefinition,NewLas.NewLasHeader);
    {$IfDef RecordLASheader} writeLineToDebugFile('CreateNewLasFromOldHeadder out'); {$EndIf}
end;


function tLAS_data.ReturnNumber(ShotNumber : integer) : byte;
//PointType 0 return number is is bits 0-2, number of returns from pulse are in bits 3-5
//PointType 6 return number is is bits 0-3, number of returns from pulse are in bits 4-7
begin
   if (LidarPointType in [0..5]) then Result := LidarPoints0^[ShotNumber].ReturnFlags mod 8;
   if (LidarPointType in [6..8]) then Result := LidarPoints6^[ShotNumber].ReturnFlags mod 16;
   if (Result = 0) then Result := 1;          //should not happen, but does enough that LasTools reports the problem
end;

function tLAS_data.ReturnsInPulse(ShotNumber : integer) : byte;
//PointType 0 return number is is bits 0-2 (1-2-4), number of returns from pulse are in bits 3-5 (8-16-32)
//PointType 6 return number is is bits 0-3, number of returns from pulse are in bits 4-7
begin
   if (LidarPointType in [0..5]) then Result := ((LidarPoints0^[ShotNumber].ReturnFlags mod 64) div 8);
   if (LidarPointType in [6..8]) then Result := (LidarPoints6^[ShotNumber].ReturnFlags div 16);
end;


function tLAS_data.PointSourceID(ShotNumber : integer) : integer;
begin
   if (LidarPointType in [0..5]) then Result := LidarPoints0^[ShotNumber].PointSourceID;
   if (LidarPointType in [6..8]) then Result := LidarPoints6^[ShotNumber].PointSourceID;
end;

function tLAS_data.UserDataRec(ShotNumber : integer) : byte;
begin
   if (LidarPointType in [0..5]) then Result := LidarPoints0^[ShotNumber].UserData;
   if (LidarPointType in [6..8]) then Result := LidarPoints6^[ShotNumber].UserData;
end;


function tLAS_data.AirReturn(ShotNumber : integer) : boolean;
var
   i,j : byte;
begin
    i := ReturnNumber(ShotNumber);
    j := ReturnsInPulse(ShotNumber);
    Result := i < j;
end;


function tLAS_data.NoiseReturn(ShotNumber : integer) : boolean;
begin
   Result := LASclassification(ShotNumber) in [7,18];
end;

function tLAS_data.GroundReturn(ShotNumber : integer) : boolean;
begin
   Result := LASclassification(ShotNumber) in [2];
end;


function tLAS_data.LastReturn(ShotNumber : integer) : boolean;
var
   i,j : byte;
begin
    i := ReturnNumber(ShotNumber);
    j := ReturnsInPulse(ShotNumber);
    Result := i = j;
end;


function tLAS_data.FirstReturn(ShotNumber : integer) : boolean;
begin
   Result := ReturnNumber(ShotNumber) = 1;
end;


function tLAS_data.SoloReturn(ShotNumber : integer) : boolean;
begin
    Result := ReturnsInPulse(ShotNumber) = 1;
end;


function tLAS_data.GetScanAngle(j : integer) : float64;
begin
   if (LidarPointType in [0..5]) then Result := LidarPoints0^[j].ScanAngle;
   if (LidarPointType in [6..8]) then Result := 0.006 * LidarPoints6^[j].ScanAngle;
end;


function tLAS_data.LASClassification(ShotNumber : integer) : byte;
begin
   if (LidarPointType in [0..5]) then Result := LidarPoints0^[ShotNumber].Classification mod 32;
   if (LidarPointType in [6..8]) then Result := LidarPoints6^[ShotNumber].Classification;
end;

procedure tLAS_data.SetLASClassification(ShotNumber : integer; TheClass : byte);
begin
   if (LidarPointType in [0..5]) then LidarPoints0^[ShotNumber].Classification := (LidarPoints0^[ShotNumber].Classification div 32) * 32;
   if (LidarPointType in [6..8]) then LidarPoints6^[ShotNumber].Classification := 0;
end;

function tLAS_data.OverlapPoint(ShotNumber : integer) : boolean;
begin
   if (LidarPointType in [0..5]) then Result := (LidarPoints0^[ShotNumber].Classification mod 32) = 12;
   if (LidarPointType in [6..8]) then Result := (LidarPoints6^[ShotNumber].ClassificationFlags and 00010000) = 00010000;
end;


function tLAS_data.WithheldPoint(ShotNumber : integer) : boolean;
begin
   if (LidarPointType in [0..5]) then Result := (LidarPoints0^[ShotNumber].Classification div 128) = 1;
   if (LidarPointType in [6..8]) then Result := (LidarPoints6^[ShotNumber].ClassificationFlags and 00100000) = 00100000;
end;


function tLAS_data.LASClassificationCategory(j : integer) : tLASClassificationCategory;
var
   Cat : byte;
begin
   Cat := LASClassification(j);
   if (Cat in [0,1]) then Result := lccUnclass
   else if (Cat = 2) then Result := lccGround
   else if (Cat in [3..5]) then Result := lccVeg
   else if (Cat = 6) then Result := lccBuilding
   else if (Cat = 9) then Result := lccWater
   else Result := lccOther;
end;


procedure tLAS_data.FigureOffsets(CenterOnKM,CenterZ : boolean; var xoff,yoff,zoff : float64);
begin
    xoff := 0.5 * (LasHeader.MaxX + LasHeader.MinX);
    yoff := 0.5 * (LasHeader.MaxY + LasHeader.MinY);
    if CenterOnKm then begin
       xoff := 1000 * round(0.001 * xoff);
       yoff := 1000 * round(0.001 * yoff);
    end;
    zoff := 0;
    if CenterZ then zoff := 0.5 * (LasHeader.MaxZ + LasHeader.MinZ);
end;


function tLAS_data.ColorFilterOut(j : integer) : boolean;
var
   r,g,b : byte;
begin
   GetRGBComponents(j,r,g,b);
   Result := (r >= MDDef.RedLow) and (r <= MDDef.RedHigh) and (g >= MDDef.GreenLow) and (g <= MDDef.GreenHigh) and (b >= MDDef.BlueLow) and (b <= MDDef.BlueHigh);
end;


constructor tLAS_data.Create;
const
   MaxInRec = MaxWord;
var
   GeoASCIIParamsOffset,GeoASCIIParamsSize,i,{FIPS_Zone}j{,tCode} : integer;
   WidthMeters,az,HeightMeters,vFactor,hfactor, Area : float64;
   GeoKeys : array[0..MaxGeoKeys,1..4] of word;
   ProjData : tStringList;
   ASCIIStr  : AnsiString;
   //SPCSZone : ShortString;
   Junk,
   GeotiffDoubles,
   GeotiffASCII  : array[1..MaxInRec] of byte;


         function CheckProjectionFile : boolean;
         var
            md : tStringList;
         begin
            lasProjectionDefinition.LasProjection.ThisIsUTMFile := FileExists(ExtractFilePath(FileName) + 'utm.txt');
            if (not AllowNoProjectionLAS) and (not lasProjectionDefinition.LasProjection.ThisIsUTMFile) and (not lasProjectionDefinition.RawXYZFile) then begin
                {$IfDef VCL}
                   if AnswerIsYes('No registration in LAS file, show metadata') then begin
                      md := GetMetadata;
                      DisplayAndPurgeStringList(md,'Metadata for ' + FileName);
                   end;
                {$EndIf}
                HasProjection := false;
                lasProjectionDefinition.RawXYZFile := true;
                Result := false;
                EndProgress;
            end
            else Result := true;
         end;


         function MakeDouble(offset : integer) : Double;
         var
            Run : array[1..8] of byte;
            i : integer;
         begin
            for i := 1 to 8 do Run[i] := GeotiffDoubles[i + 8*Offset];
            Move(Run,Result,8);
         end;

         procedure SetLasUTMBox;
         begin
            LAS_UTM_Box.ymax := LasHeader.MaxY;
            LAS_UTM_Box.xmax := LasHeader.MaxX;
            LAS_UTM_Box.ymin := LasHeader.MinY;
            LAS_UTM_Box.xmin := LasHeader.MinX;
         end;

         procedure SetLasGeoBox;
         begin
            LAS_LatLong_Box.ymax := LasHeader.MaxY;
            LAS_LatLong_Box.xmax := LasHeader.MaxX;
            LAS_LatLong_Box.ymin := LasHeader.MinY;
            LAS_LatLong_Box.xmin := LasHeader.MinX;
         end;

         procedure GetUTMboxFromLatLongBox;
         begin
            RedefineWGS84DatumConstants(0.5*(LAS_LatLong_Box.XMax + LAS_LatLong_Box.XMin),lasProjectionDefinition.LasProjection.LatHemi);
            WGS84DatumConstants.LatLongDegreetoUTM(LAS_LatLong_Box.YMax,LAS_LatLong_Box.XMax,LAS_UTM_Box.xmax,LAS_UTM_Box.ymax);
            WGS84DatumConstants.LatLongDegreetoUTM(LAS_LatLong_Box.YMin,LAS_LatLong_Box.XMin,LAS_UTM_Box.xmin,LAS_UTM_Box.ymin);
         end;

         procedure DoWKTfromVariableLengthRecord;
         begin
            if lasProjectionDefinition.LasProjection.DecodeWKTProjectionFromString(ASCIIProjectionData) then begin
               lasProjectionDefinition.LasProjection.InverseProjectDegrees(LasHeader.MaxX,LasHeader.MaxY,LAS_LatLong_Box.ymax,LAS_LatLong_Box.xmax);
               lasProjectionDefinition.LasProjection.InverseProjectDegrees(LasHeader.MinX,LasHeader.MinY,LAS_LatLong_Box.ymin,LAS_LatLong_Box.xmin);
               {$If Defined(RecordWKT)} WriteLineToDebugFile('DoWKTfromVariableLengthRecord geobox=' + sfBoundBoxToString(LAS_LatLong_Box)); {$EndIf}

               //if lasProjectionDefinition.LasProjection.VertFeet then begin
                  //MeterZscaleFac := LasHeader.ZscaleFac * lasProjectionDefinition.LasProjection.VertFootFactor;
                  LasHeader.ZscaleFac := LasHeader.ZscaleFac * lasProjectionDefinition.LasProjection.VertFootFactor;
                  LasHeader.MaxZ := LasHeader.MaxZ * lasProjectionDefinition.LasProjection.VertFootFactor;
                  LasHeader.MinZ := LasHeader.MinZ * lasProjectionDefinition.LasProjection.VertFootFactor;
                  LAS_Z_range := LasHeader.MaxZ - LasHeader.MinZ;
               //end;
               if lasProjectionDefinition.LasProjection.Pname = UTMEllipsoidal then SetLasUTMBox
               else begin
                  GetUTMboxFromLatLongBox;
               end;

               UTMZone := GetUTMZone(0.5 * (LAS_LatLong_Box.xmax + LAS_LatLong_Box.xmin));
               if LAS_LatLong_Box.ymin > 0 then lasProjectionDefinition.LASProjection.LatHemi := 'N'
               else lasProjectionDefinition.LASProjection.LatHemi := 'S';

               Area := (LasHeader.MaxX - LasHeader.MinX) * (LasHeader.MaxY - LasHeader.MinY);
            end;
         end;


var
   NumRead : integer;
   Hemi : ANSIchar;
begin
   {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)} WriteLineToDebugFile('tLAS_data.Create for ' + FileName); {$EndIf}
   if FileExists(FileName) then begin
      CheckFileNameForSpaces(FileName);
      NilRecordMemory;
      lasProjectionDefinition.LASProjection := Nil;
      ASCIIProjectionData := '';
      UTMZone := -99;
      HasProjection := true;
      InitializeLASColors;
      Icesat2Cloud := StrUtils.AnsiContainsText(UpperCase(ExtractFileName(FileName)),'ICESAT2');
      InsureFileIsNotReadOnly(FileName);
      AssignFile(LasFile,FileName);
      Reset(LasFile,1);

      BlockRead(LasFile,LasHeader,SizeOf(LasHeader),NumRead);
      if NumRead <> SizeOf(LasHeader) then begin
         MessageToContinue('LAS file problem ' + FileName);
         exit;
      end;

      NumPointRecs := LasHeader.NumPtRecs;
      if (LasHeader.GenerateSoftware[1] = 'M') and (LasHeader.CreateDay > 366) then begin
         LasHeader.CreateDay := 1;
         Reset(LasFile,1);
         BlockWrite(LasFile,LasHeader,SizeOf(LasHeader));
      end;

      if (LasHeader.VersionMinor = #3) then begin
         BlockRead(LasFile,StartWaveFormDataPackedRecord,8);
      end;

      if (LasHeader.VersionMinor = #4) then begin
         BlockRead(LasFile,Las14Header,SizeOf(tLas14Header));
         NumPointRecs := Las14Header.NumPtRecs;
      end;

      LidarPointType := ord(LasHeader.PointDataFormat);
      {$IfDef RecordCreateEveryFile} writeLineToDebugFile('Point type =' + IntToStr(LidarPointType)); {$EndIf}
      RGBPresent := LidarPointType in [2,3,5,7,8];
      ExtraBytesPrep;

       LAS_x_range := LasHeader.MaxX - LasHeader.MinX;
       LAS_y_range := LasHeader.MaxY - LasHeader.MinY;
       LAS_Z_range := LasHeader.MaxZ - LasHeader.MinZ;
       MeterXscaleFac := LasHeader.XscaleFac;
       MeterYscaleFac := LasHeader.YscaleFac;
       MeterZscaleFac := LasHeader.ZscaleFac;

      lasProjectionDefinition.LASProjection := tMapProjection.Create(ExtractFileName(FileName));
      lasProjectionDefinition.LasProjection.H_DatumCode := '';
      GeoASCIIParamsOffset := 0;
      GeoASCIIParamsSize := 0;
      vFactor := 1;
      hFactor := 1;

      WKT := (LasHeader.GlobalEncoding and 16 = 16);
      if WKT and (Las14Header.NumELVR > 0) then begin
         {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)} WriteLineToDebugFile('WKT, start at ' + IntToStr(Las14Header.StartELVR1)); {$EndIf}
         Seek(LasFile,Las14Header.StartELVR1);
         {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)} WriteLineToDebugFile('WKT, read= ' + IntToStr(SizeOf(tELVRHeader))); {$EndIf}
         BlockRead(LasFile,ELVRHeader,SizeOf(tELVRHeader));
         {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)} WriteLineToDebugFile('WKT, read ELVRHeader.RecLenAfter= ' + IntToStr(ELVRHeader.RecLenAfter)); {$EndIf}
         BlockRead(LasFile,GeotiffASCII[1],ELVRHeader.RecLenAfter);

         ASCIIProjectionData := '';
         for J := 1 to ELVRHeader.RecLenAfter do begin
            ASCIIProjectionData := ASCIIProjectionData + chr(GeotiffASCII[j]);
         end;
         {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)} WriteLineToDebugFile('WKT, ASCIIProjectionData= ' + ASCIIProjectionData);    {$EndIf}
         DoWKTfromVariableLengthRecord;
      end
      else begin
         {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)} writeLineToDebugFile('No WKT in ELVR'); {$EndIf}
         lasProjectionDefinition.RawXYZFile := FileExists(ExtractFilePath(FileName) + 'xyz_files.txt') or FileExists(ExtractFilePath(FileName) + 'rawxy.txt');
         if LasProjectionDefinition.RawXYZFile then begin
            SetLASGeoBox;
            Area := (LasHeader.MaxX - LasHeader.MinX) * (LasHeader.MaxY - LasHeader.MinY);
         end;
         LasFileName := FileName;
         {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)}  WriteLineToDebugFile('LasHeader.NumVarLenRecs=' + IntToStr(LasHeader.NumVarLenRecs)); {$EndIf}
         for i := 1 to LasHeader.NumVarLenRecs do begin
            BlockRead(LasFile,VarLenRecHeader[i],SizeOf(tVarLenRecHeader));
            {$IfDef RecordVarLenRec}
               WriteLineToDebugFile(ExtractFileNameNoExt(LasFileName) + ' VLR ' + IntToStr(i) + '  RecID=' + IntToStr(VarLenRecHeader[i].RecordID) + '   after header: ' + IntToStr(VarLenRecHeader[i].RecLenAfterHeader) +  '  ' +
                  ArrayOfCharToString(16,VarLenRecHeader[i].UserID) +  '  ' +   ArrayOfCharToString(32,VarLenRecHeader[i].Description) );
            {$EndIf}
            if (VarLenRecHeader[i].RecLenAfterHeader > MaxInRec) then begin
               MessageToContinue('NumVarLenRec too big=' + IntToStr(VarLenRecHeader[i].RecLenAfterHeader));
            end
            else begin
               if (VarLenRecHeader[i].RecordID = 34735) then begin
                  BlockRead(LasFile,GeoKeys,VarLenRecHeader[i].RecLenAfterHeader,NumRead);
               end
               else if (VarLenRecHeader[i].RecordID = 34736) then BlockRead(LasFile,GeotiffDoubles,VarLenRecHeader[i].RecLenAfterHeader,NumRead)
               else if (VarLenRecHeader[i].RecordID = 34737) or (VarLenRecHeader[i].RecordID = 2112) then BlockRead(LasFile,GeotiffASCII,VarLenRecHeader[i].RecLenAfterHeader,NumRead)
               else BlockRead(LasFile,Junk,VarLenRecHeader[i].RecLenAfterHeader,NumRead);
            end;
         end;

        //10375 is for Whitebox, but it has bigger problems

         for i := 1 to LasHeader.NumVarLenRecs do begin
            if (VarLenRecHeader[i].RecordID = 34735) then begin
               NumGeotiffKeys := GeoKeys[0,4];
               for j := 1 to NumGeotiffKeys do begin
                  with FileGeoKeys[j] do begin
                     wKeyID := GeoKeys[j,1];
                     wValueOffset := GeoKeys[j,4];
                     {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)}  writeLineToDebugFile(IntegerToString(j,4) + IntegerToString(wKeyID,8) + IntegerToString(wValueOffset,12)); {$EndIf}
                     if (wKeyID = 1024) then lasProjectionDefinition.LASProjection.ModelType := wValueOffset;
                     if (wKeyID = 2048) then lasProjectionDefinition.LasProjection.ProcessTiff2048(wValueOffset);
                    //if (wKeyID = 2054) then begin end; //GeogAngularUnitsGeoKey
                     if (wKeyID = 3072) then lasProjectionDefinition.LASProjection.OpenFromTiff3072(wValueOffset);
                     if (wKeyID = 3075) then lasProjectionDefinition.LasProjection.ProcessTiff3075(wValueOffset);
                     if (wKeyID = 3076) then begin
                        lasProjectionDefinition.LasProjection.ProjLinearUnitsGeoKey := wValueOffset;
                        hFactor := LengthConversion(wValueOffset);
                     end;

                     if (wKeyID = 3078) then lasProjectionDefinition.LasProjection.phi1 := MakeDouble(wValueOffset) * DegToRad;
                     if (wKeyID = 3079) then lasProjectionDefinition.LasProjection.phi2 := MakeDouble(wValueOffset) * DegToRad;
                     if (wKeyID = 3084) then lasProjectionDefinition.LasProjection.Long0 := MakeDouble(wValueOffset)* Petmar_types.DegToRad;
                     if (wKeyID = 3085) then lasProjectionDefinition.LasProjection.Lat0 := MakeDouble(wValueOffset) * Petmar_types.DegToRad;
                     if (wKeyID = 3086) then lasProjectionDefinition.LasProjection.false_east := hfactor * MakeDouble(wValueOffset);
                     if (wKeyID = 3087) then lasProjectionDefinition.LasProjection.false_north := hfactor * MakeDouble(wValueOffset);
                     if (wKeyID = 4096) then lasProjectionDefinition.LasProjection.ProcessTiff4096(wValueOffset);
                     if (wKeyID = 4099) then begin
                        lasProjectionDefinition.LasProjection.VerticalUnitsGeoKey := wValueOffset;
                        vFactor := LengthConversion(wValueOffset);
                     end;
                  end {with};
               end {for j};
            end;
            if (VarLenRecHeader[i].RecordID = 34737) or (VarLenRecHeader[i].RecordID = 2112) then begin
               ASCIIProjectionData := '';
               for J := 1 to VarLenRecHeader[i].RecLenAfterHeader do begin
                  if (GeotiffASCII[j] <> 0) then ASCIIProjectionData := ASCIIProjectionData + chr(GeotiffASCII[j]);
               end;
               {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)}  writeLineToDebugFile(ASCIIProjectionData); {$EndIf}
               if FindUTMZone(ASCIIProjectionData,UTMzone,Hemi) then begin
                  {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)} WriteLineToDebugFile('34737, UTM and not already defined'); {$EndIf}
                  lasProjectionDefinition.LASProjection.h_datumcode := 'NAD83';
                  if (Hemi = 'S') then lasProjectionDefinition.LASProjection.LatHemi := 'S';
                  lasProjectionDefinition.LASProjection.StartUTMProjection(UTMZone);
               end
               else DoWKTfromVariableLengthRecord;
               {$If Defined(RecordCreateEveryFile) or Defined(RecordWKT)}  writeLineToDebugFile('34737  ASCII projection data=' + ASCIIProjectionData); {$EndIf}
            end;
         end;
      end;

      MeterXscaleFac := LasHeader.XscaleFac * hFactor;
      MeterYscaleFac := LasHeader.YscaleFac * hFactor;
      MeterZscaleFac := LasHeader.ZscaleFac * vFactor;
      LasHeader.MaxX := LasHeader.MaxX * hFactor;
      LasHeader.MinX := LasHeader.MinX * hFactor;
      LasHeader.MaxY := LasHeader.MaxY * hFactor;
      LasHeader.MinY := LasHeader.MinY * hFactor;

      if not lasProjectionDefinition.RawXYZFile then begin
         if (lasProjectionDefinition.LasProjection.H_DatumCode = '') then begin
            if ThisIsETRS89(ASCIIProjectionData) then lasProjectionDefinition.LasProjection.H_DatumCode := 'ETR89'
            else if ThisIsWGS84(ASCIIProjectionData) then lasProjectionDefinition.LasProjection.H_DatumCode := 'WGS84'
            else if ThisIsNAD83(ASCIIProjectionData) then lasProjectionDefinition.LasProjection.H_DatumCode := 'NAR-C';
         end;

         if (lasProjectionDefinition.LASProjection.ModelType = LasLatLong) then begin
            VincentyCalculateDistanceBearing(LasHeader.MaxY,LasHeader.MaxX,LasHeader.MaxY,LasHeader.MinX,WidthMeters,az);
            VincentyCalculateDistanceBearing(LasHeader.MaxY,LasHeader.MaxX,LasHeader.MinY,LasHeader.MaxX,HeightMeters,az);
            Area := WidthMeters * HeightMeters;
            SetLasGeoBox;
            UTMZone := GetUTMZone(0.5*(LAS_LatLong_Box.XMax + LAS_LatLong_Box.XMin));
            if (LAS_LatLong_Box.YMax > 0) then lasProjectionDefinition.LasProjection.LatHemi := 'N' else lasProjectionDefinition.LasProjection.LatHemi := 'S';
            GetUTMboxFromLatLongBox;
         end
         else begin
            {$IfDef RecordCreateEveryFile} WriteLineToDebugFile('Set up UTM-like projection'); {$EndIf}
            if (lasProjectionDefinition.LASProjection.PName = UTMEllipsoidal) then begin
               lasProjectionDefinition.LasProjection.StartUTMProjection(lasProjectionDefinition.LasProjection.projUTMZone);
               UTMZone := lasProjectionDefinition.LasProjection.projUTMZone;
            end;
            lasProjectionDefinition.LasProjection.GetProjectParameters;
            {$IfDef RecordCreateEveryFile}
               WriteLineToDebugFile('Got lasProjectionDefinition.LasProjection.GetProjectParameters');
               WriteStringListToDebugFile(lasProjectionDefinition.LasProjection.ProjectionParametersList);
            {$EndIf}
            SetLasUTMBox;
            Area := (LasHeader.MaxX - LasHeader.MinX) * (LasHeader.MaxY - LasHeader.MinY);
            lasProjectionDefinition.LasProjection.InverseProjectDegrees(LasHeader.MaxX,LasHeader.MaxY,LAS_LatLong_Box.ymax,LAS_LatLong_Box.xmax);
            lasProjectionDefinition.LasProjection.InverseProjectDegrees(LasHeader.MinX,LasHeader.MinY,LAS_LatLong_Box.ymin,LAS_LatLong_Box.xmin);
         end;
      end;

      if (not WKT) then begin
         if (NumGeotiffKeys = 0) or (lasProjectionDefinition.LasProjection = Nil) and (not (lasProjectionDefinition.LASProjection.ModelType = LasLatLong)) then begin
            {$IfDef RecordCreateEveryFile} WriteLineToDebugFile('No luck on projection'); {$EndIf}
            FileName := ExtractFilePath(LASFileName) + 'all.prj';
            if FileExists(fileName) then begin
               lasProjectionDefinition.LASProjection.InitializeProjectionFromWKT(fileName);
            end
            else begin
               if not CheckProjectionFile then exit;
            end;
         end;
      end;

      SingleFilePointDensity := NumPointRecs / Area;

      //in case LAS files has no breakdown by return (e.g. OpenTopography Pleiades)
      if LasHeader.NumPtsByReturn[1] = 0 then SingleFilePulseDensity := SingleFilePointDensity;
      Seek(LasFile,LasHeader.OffsetToData);

       {$IfDef RecordCreateEveryFile}
          WriteLineToDebugFile('SingleFilePointDensity =' + RealToString(SingleFilePointDensity,-18,-5) + '  SingleFilePulseDensity =' + RealToString(SingleFilePulseDensity,-18,-5));
          WriteLineToDebugFile('Area =' + SmartAreaFormat(Area) + ' NumPts =' + IntToStr(NumPointRecs));
          if LasProjectionDefinition.RawXYZFile then WriteLineToDebugFile('raw XYZ file');
          if (lasProjectionDefinition.LASProjection.ModelType = LasLatLong) then WriteLineToDebugFile('Lat/Long file');
          WriteLineToDebugFile('MeterScaleXFactor =' + RealToString(MeterXscaleFac,-18,-5) +  ' MeterScaleYFactor =' + RealToString(MeterYscaleFac,-18,-5) + ' MeterScaleZFactor =' + RealToString(MeterZscaleFac,-18,-5));
       {$EndIf}
   end
   else begin
      {$IfDef RecordCreateEveryFile} writeLineToDebugFile('tLAS_data.Create abort with missing ' + FileName); {$EndIf}
   end;
end;


function tLAS_data.GetGPSTime(j : integer) : float64;
begin
   if LidarPoints1 <> nil then Result := LidarPoints1^[j].GPSTime
   else if LidarPoints3 <> nil then Result := LidarPoints3^[j].GPSTime
   else if LidarPoints4 <> nil then Result := LidarPoints4^[j].GPSTime
   else if LidarPoints5 <> nil then Result := LidarPoints5^[j].GPSTime
   else if LidarPoints6 <> nil then Result := LidarPoints6^[j].GPSTime
   else if LidarPoints7 <> nil then Result := LidarPoints7^[j].lp6.GPSTime
   else if LidarPoints8 <> nil then Result := LidarPoints8^[j].lp6.GPSTime
   else Result := 0;
end;


function tLAS_data.GetShotMeasuredIntensity(j : integer) : word;
begin
   if (LidarPointType in [0..5]) then Result := LidarPoints0^[j].Intensity;
   if (LidarPointType in [6..8]) then Result := LidarPoints6^[j].Intensity;
end;


function tLAS_data.GetShotIntensity(j : integer) : byte;
var
   rf : word;
begin
   if (LidarPointType in [0..5]) then rf := LidarPoints0^[j].Intensity;
   if (LidarPointType in [6..8]) then rf := LidarPoints6^[j].Intensity;
   Result := ValidByteRange(round( 255 * (rf - MDDef.MinIntensity) / (MDDef.MaxIntensity - MDDef.MinIntensity)));
end;

procedure tLAS_data.GetShotCoordinatesLatLong(j : integer; var Lat,Long : float64);
begin
   if (lasProjectionDefinition.LASProjection.ModelType = LasLatLong) then begin
       Long := ExpandLAS_X(j);
       Lat := ExpandLAS_Y(j);
    end
    else if (lasProjectionDefinition.LASProjection <> Nil) then begin
       lasProjectionDefinition.LasProjection.InverseProjectDegrees(ExpandLAS_X(j),ExpandLAS_Y(j),Lat,Long);
    end;
end;


procedure tLAS_data.GetShotCoordinatesUTM(j : integer; var xUTM,yUTM : float64);
begin
    if (lasProjectionDefinition.LASProjection.ModelType = LasLatLong) then begin
       WGS84DatumConstants.ForwardProjectDegrees(ExpandLAS_Y(j),ExpandLAS_X(j),Xutm,Yutm);
    end
    else begin
       xutm := ExpandLAS_X(j);
       yutm := ExpandLAS_Y(j);
    end;
   {$IfDef RecordFirstPulse} if (J=1) then WriteLineToDebugFile('tLAS_data.GetShotCoordinatesUTM,' + UTMString(xutm,yutm) ); {$EndIf}
end;


procedure tLAS_data.GetShotCoordinatesAppropriate(GoUTM : boolean; j : integer; var xApp,yApp : float64);
begin
   if GoUTM then GetShotCoordinatesUTM(j,xApp,yApp)
   else GetShotCoordinatesLatLong(j,yApp,xApp);
end;


procedure tLAS_data.GetShotScreenCoordinatesAppropriate(BaseMapDraw : tMapDraw; j : integer; var x,y : integer);
var
   Lat,Long : float64;
begin
   if BaseMapDraw.BasicProjection = bpUTM then GetShotScreenCoordinatesUTM(BaseMapDraw, j, x,y)
   else begin
       GetShotCoordinatesLatLong(j,Lat,Long);
       BaseMapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
   end;
end;

procedure tLAS_data.GetShotScreenCoordinatesUTM(BaseMapDraw : tMapDraw; j : integer; var x,y : integer);
var
   xf,yf : float64;
begin
  GetShotCoordinatesUTM(j,xf,yf);
  BaseMapDraw.UTMToScreen(xf,yf,x,y);
end;


function tLAS_data.GetHowManyReadsRequired : integer;
begin
   Result := succ(NumPointRecs div MaxLASPtsToRead);
end;

{$IfDef VCL}
procedure tLAS_data.OutlineOnMap(BaseMap: tMapForm);
var
   x1,y1,x2,y2,x,y : integer;
   TStr : shortstring;
begin
   BaseMap.MapDraw.LatLongDegreeToScreen(LAS_LatLong_Box.ymax,LAS_LatLong_Box.xmin,x1,y1);
   BaseMap.MapDraw.LatLongDegreeToScreen(LAS_LatLong_Box.ymin,LAS_LatLong_Box.xmax,x2,y2);
   if (x1=x2) or (y1=y2) then begin
      Petmar.ScreenSymbol(BaseMap.Image1.Canvas,x1,y1,Box,2,ConvertTColorToPlatformColor(clRed));
   end
   else begin
      BaseMap.Image1.Canvas.Rectangle(x1,y1,x2,y2);
      if MDDef.LabelLAStiles then begin
         x := (x1 + x2) div 2;
         y := (y1 + y2) div 2;
         if BaseMap.MapDraw.OnScreen(x,y) then begin
            TStr := ExtractFileNameNoExt(LasFileName);
            BaseMap.Image1.Canvas.TextOut(x - BaseMap.Image1.Canvas.TextWidth(TStr) div 2,y,TStr);
         end;
      end;
   end;
end;
{$EndIf}


function tLAS_data.FileOnMap(BaseMapDraw : tMapDraw): boolean;
begin
   Result := BaseMapDraw.AFullWorldMap or AtLeastPartOfBoxInAnotherBox(LAS_LatLong_Box,BaseMapDraw.MapCorners.BoundBoxGeo);
end;


function tLAS_data.FileOnDEM(DEM : integer): boolean;
begin
   Result := AtLeastPartOfBoxInAnotherBox(LAS_LatLong_Box,DEMGlb[DEM].DEMBoundBoxGeo);
end;


function tLAS_data.InBoundBoxGeo(BoundBox : sfBoundBox): boolean;
begin
   Result := AtLeastPartOfBoxInAnotherBox(LAS_LatLong_Box,BoundBox);
end;


function tLAS_data.InBoundBoxUTM(BoundBox : sfBoundBox): boolean;
begin
   Result := AtLeastPartOfBoxInAnotherBox(LAS_UTM_Box,BoundBox);
end;


procedure tLAS_data.GetColorAndElevation(j : integer; MinAreaZ,MaxAreaZ : float64; var ColorRGB : tPlatformColor; var ze : float64);
begin
   ze := ExpandLAS_Z(j);
   if (MDDef.ls.ColorCoding = lasccElevation) then begin
      ColorRGB := Petmar.TerrainRGBFunct(ze,MinAreaZ,MaxAreaZ);
   end
   else GetColor(j,MinAreaZ,MaxAreaZ,ColorRGB);
end;


function tLAS_data.GetNIRWord(j : integer; var n : Word) : boolean;
begin
   if (LidarPoints8 <> Nil) then begin
       n := LidarPoints8^[j].NIR;
       Result := true;
    end
    else Result := false;
end;


function tLAS_data.GetRGBWord(j : integer; var r,g,b : Word) : boolean;
begin
    Result := true;
    if (LidarPoints2 <> Nil) then begin
       r := LidarPoints2^[j].Red;
       g := LidarPoints2^[j].Green;
       b := LidarPoints2^[j].Blue;
    end
    else if (LidarPoints3 <> Nil) then begin
       r := LidarPoints3^[j].Red;
       g := LidarPoints3^[j].Green;
       b := LidarPoints3^[j].Blue;
    end
    else if (LidarPoints7 <> Nil) then begin
       r := LidarPoints7^[j].Red;
       g := LidarPoints7^[j].Green;
       b := LidarPoints7^[j].Blue;
    end
    else if (LidarPoints8 <> Nil) then begin
       r := LidarPoints8^[j].Red;
       g := LidarPoints8^[j].Green;
       b := LidarPoints8^[j].Blue;
    end
    else Result := false;
end;


function tLAS_data.GetRGBComponents(j : integer; var r,g,b : byte) : boolean;
var
   rw,gw,bw : word;
begin
    Result := GetRGBWord(j,rw,gw,bw);
    if Result then begin
       ValueInRange(rw,MDDef.MinRGB,MDDef.MaxRGB);
       ValueInRange(gw,MDDef.MinRGB,MDDef.MaxRGB);
       ValueInRange(bw,MDDef.MinRGB,MDDef.MaxRGB);

       r := round( 255 * (rw-MDDef.MinRGB) / (MDDef.MaxRGB-MDDef.MinRGB));
       g := round( 255 * (gw-MDDef.MinRGB) / (MDDef.MaxRGB-MDDef.MinRGB));
       b := round( 255 * (bw-MDDef.MinRGB) / (MDDef.MaxRGB-MDDef.MinRGB));
    end;
end;


function tLAS_data.GetRGBColor(j : integer) : tPlatformColor;
var
   r,g,b : byte;
begin
   if GetRGBComponents(j,r,g,b) then begin
       Result := RGBTrip(r,g,b);
    end
    else Result := RGBTrip(255,0,0);
end;


procedure tLAS_data.GetColor(j : integer; MinAreaZ,MaxAreaZ : float64; var Color : tPlatformColor);
var
   Category,rf,Return,Channel : integer;
   ze,ScanAngle : float64;
begin
    //Color := MDDef.MissingDataColor;
    try
       if (MDDef.ls.ColorCoding = lasccElevation) then begin
          ze := ExpandLAS_Z(j);
          Color := Petmar.TerrainRGBFunct(ze,MinAreaZ,MaxAreaZ);
       end
       else if (MDDef.ls.ColorCoding = lasccClass) then begin
          Category := LASclassification(j);
          LasCatUsed[Category] := true;
          Color := LAS_RGB_colors[Category];
       end
       else if (MDDef.ls.ColorCoding = lasccIntensity) then begin
          rf := GetShotIntensity(j);
          Color := Petmar.GrayRGBtrip(rf);
       end
       else if (MDDef.ls.ColorCoding = lasccRGB) then begin
          Color := GetRGBColor(j);
       end
       else if (MDDef.ls.ColorCoding = lasccReturnNumber) then begin
          Return := ReturnNumber(j);
         if (Return <> 0) then Color := LAS_Ret_colors[Return];
       end
       else if (MDDef.ls.ColorCoding = lasccReturnsPulse) then begin
          Return := ReturnsInPulse(j);
          if (Return <> 0) then Color := LAS_Ret_colors[Return];
       end
       else if (MDDef.ls.ColorCoding = lasccScanAngle) then begin
          ScanAngle := GetScanAngle(j);
          Color := Petmar.SpectrumRGBFunct(ScanAngle,-25,25);
       end
       else if (MDDef.ls.ColorCoding = lasccGPSTime) then begin
          Color := Petmar.SpectrumRGBFunct(GetGPSTime(j),MinAreaZ,MaxAreaZ);
       end
       else if (MDDef.ls.ColorCoding = lasccCloudID) then begin
          Color := MDDef.CloudSymbol[1].Color;
       end
       else if (MDDef.ls.ColorCoding = lasccPointSourceID) then begin
          Channel := PointSourceID(j);
          case Channel of
             1 : Color := ConvertTColorToPlatformColor(clRed);
             2 : Color := ConvertTColorToPlatformColor(clBlue);
             3 : Color := ConvertTColorToPlatformColor(clLime);
             else Color := ConvertTColorToPlatformColor(clGreen);
          end;
       end
       else if (MDDef.ls.ColorCoding = lasccUserData) then begin
          Channel := UserDataRec(j);
          case Channel of
             1 : Color := ConvertTColorToPlatformColor(clRed);
             2 : Color := ConvertTColorToPlatformColor(clBlue);
             3 : Color := ConvertTColorToPlatformColor(clLime);
             else Color := ConvertTColorToPlatformColor(clGreen);
          end;
       end;
    except
       on Exception do Color := MDDef.MissingDataColor;
    end;
end;


function NoFilterWanted : boolean;
begin
   Result := (not MDDef.ls.Filters) and (MDDef.ls.BadPointFilter = 0);
end;


procedure tLAS_data.FreeLASRecordMemory;
begin
   {$If Defined(RecordLASMemoryAlocations)} WriteLineToDebugFile('tLAS_data.FreeLASRecordMemory ' + ExtractFileNameNoExt(LasFileName) +  ' PtType=' + IntToStr(LidarPointType)); {$EndIf}
   if (LidarPoints0 <> Nil) then Dispose(LidarPoints0);
   if (LidarPoints1 <> Nil) then Dispose(LidarPoints1);
   if (LidarPoints2 <> Nil) then Dispose(LidarPoints2);
   if (LidarPoints3 <> Nil) then Dispose(LidarPoints3);
   if (LidarPoints4 <> Nil) then Dispose(LidarPoints4);
   if (LidarPoints5 <> Nil) then Dispose(LidarPoints5);
   if (LidarPoints6 <> Nil) then Dispose(LidarPoints6);
   if (LidarPoints7 <> Nil) then Dispose(LidarPoints7);
   if (LidarPoints8 <> Nil) then Dispose(LidarPoints8);
   if DataHasExtraBytes and (BB <> Nil) then Dispose(BB);
   NilRecordMemory;
end;


procedure tLAS_data.NilRecordMemory;
begin
   {$If Defined(RecordLASMemoryAlocations)} WriteLineToDebugFile('tLAS_data.NilRecordMemory ' + ExtractFileNameNoExt(LasFileName) +  ' PtType=' + IntToStr(LidarPointType)); {$EndIf}
   LidarPoints0 := Nil;
   LidarPoints1 := Nil;
   LidarPoints2 := Nil;
   LidarPoints3 := Nil;
   LidarPoints4 := Nil;
   LidarPoints5 := Nil;
   LidarPoints6 := Nil;
   LidarPoints7 := Nil;
   LidarPoints8 := Nil;
end;


procedure tLAS_data.PrepDataRead;
begin
   {$If Defined(RecordLASMemoryAlocations) or Defined(RecordLASheader)}
      WriteLineToDebugFile('tLAS_data.PrepDataRead ' + ExtractFileNameNoExt(LasFileName) +  ' offset=' + IntToStr(LasHeader.OffsetToData) + ' PtType=' + IntToStr(LidarPointType));
   {$EndIf}
   if (LidarPointType in [0..5]) then New(LidarPoints0);
   if (LidarPointType = 1) then New(LidarPoints1);
   if (LidarPointType = 2) then New(LidarPoints2);
   if (LidarPointType = 3) then New(LidarPoints3);
   if (LidarPointType = 4) then New(LidarPoints4);
   if (LidarPointType = 5) then New(LidarPoints5);
   if (LidarPointType in [6..8]) then New(LidarPoints6);
   if (LidarPointType = 7) then New(LidarPoints7);
   Seek(LasFile,LasHeader.OffsetToData);
   ReadsRequired := GetHowManyReadsRequired;
   if DataHasExtraBytes then New(BB);
end;


procedure tLAS_data.ReadPoints(var RecsRead: integer);
var
   i,j : integer;
begin
   if DataHasExtraBytes then begin
       BlockRead(LasFile,bb^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);
       RecsRead := RecsRead div LasHeader.PointDataRecLen;
       if (LidarPointType = 0) then for i := 1 to RecsRead do Move(bb^[1 + pred(i) * LasHeader.PointDataRecLen],LidarPoints0^[i],BaseLength);
       if (LidarPointType = 1) then for i := 1 to RecsRead do Move(bb^[1 + pred(i) * LasHeader.PointDataRecLen],LidarPoints1^[i],BaseLength);
       if (LidarPointType = 2) then for i := 1 to RecsRead do Move(bb^[1 + pred(i) * LasHeader.PointDataRecLen],LidarPoints2^[i],BaseLength);
       if (LidarPointType = 3) then for i := 1 to RecsRead do Move(bb^[1 + pred(i) * LasHeader.PointDataRecLen],LidarPoints3^[i],BaseLength);
       if (LidarPointType = 4) then for i := 1 to RecsRead do Move(bb^[1 + pred(i) * LasHeader.PointDataRecLen],LidarPoints4^[i],BaseLength);
       if (LidarPointType = 5) then for i := 1 to RecsRead do Move(bb^[1 + pred(i) * LasHeader.PointDataRecLen],LidarPoints5^[i],BaseLength);
       if (LidarPointType = 6) then for i := 1 to RecsRead do Move(bb^[1 + pred(i) * LasHeader.PointDataRecLen],LidarPoints6^[i],BaseLength);
       if (LidarPointType = 7) then for i := 1 to RecsRead do Move(bb^[1 + pred(i) * LasHeader.PointDataRecLen],LidarPoints7^[i],BaseLength);
       if (LidarPointType = 8) then for i := 1 to RecsRead do Move(bb^[1 + pred(i) * LasHeader.PointDataRecLen],LidarPoints8^[i],BaseLength);
   end
   else begin
       if (LidarPointType = 0) then BlockRead(LasFile,LidarPoints0^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);
       if (LidarPointType = 1) then BlockRead(LasFile,LidarPoints1^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);
       if (LidarPointType = 2) then BlockRead(LasFile,LidarPoints2^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);
       if (LidarPointType = 3) then BlockRead(LasFile,LidarPoints3^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);
       if (LidarPointType = 4) then BlockRead(LasFile,LidarPoints4^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);
       if (LidarPointType = 5) then BlockRead(LasFile,LidarPoints5^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);

   //Las 1.4 new types
       if (LidarPointType = 6) then BlockRead(LasFile,LidarPoints6^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);
       if (LidarPointType = 7) then BlockRead(LasFile,LidarPoints7^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);
       if (LidarPointType = 8) then BlockRead(LasFile,LidarPoints8^,MaxLASPtsToRead*LasHeader.PointDataRecLen,RecsRead);

       RecsRead := RecsRead div LasHeader.PointDataRecLen;
   end;

    if (LidarPointType in [1..5,7..8]) then begin
       if (LidarPointType = 1) then for j := 1 to RecsRead do LidarPoints0^[j] := LidarPoints1^[j].lp0;
       if (LidarPointType = 2) then for j := 1 to RecsRead do LidarPoints0^[j] := LidarPoints2^[j].lp0;
       if (LidarPointType = 3) then for j := 1 to RecsRead do LidarPoints0^[j] := LidarPoints3^[j].lp0;
       if (LidarPointType = 4) then for j := 1 to RecsRead do LidarPoints0^[j] := LidarPoints4^[j].lp0;
       if (LidarPointType = 5) then for j := 1 to RecsRead do LidarPoints0^[j] := LidarPoints5^[j].lp0;
       if (LidarPointType = 7) then for j := 1 to RecsRead do LidarPoints6^[j] := LidarPoints7^[j].lp6;
       if (LidarPointType = 8) then for j := 1 to RecsRead do LidarPoints6^[j] := LidarPoints8^[j].lp6;
    end;
   {$IfDef RecordFirstPulse} writeLineToDebugFile('tLAS_data.ReadPoints x=' + IntToStr(LidarPoints0^[1].xpt) + '  y=' + IntToStr(LidarPoints0^[1].ypt)); {$EndIf}
end;



destructor tLAS_data.Destroy;
begin
   {$IfDef RecordCreateEveryFile} writeLineToDebugFile('tLAS_data.Destroy for ' + LasFileName); {$EndIf}
   CloseFile(LasFile);
   if (lasProjectionDefinition.LasProjection <> Nil) then begin
      lasProjectionDefinition.LasProjection.Destroy;
   end;

end;

function tLAS_data.ExpandLAS_X(ShotNumber : integer): float64;
var
   x : float64;
begin
    if (LidarPointType in [0..5]) then x := LidarPoints0^[ShotNumber].xPt;
    if (LidarPointType in [6..8]) then x := LidarPoints6^[ShotNumber].xPt;
    Result := LasHeader.XscaleFac * x + LasHeader.Xoffset;
end;

function tLAS_data.ExpandLAS_Y(ShotNumber : integer): float64;
var
   y : float64;
begin
    if (LidarPointType in [0..5]) then y := LidarPoints0^[ShotNumber].yPt;
    if (LidarPointType in [6..8]) then y := LidarPoints6^[ShotNumber].yPt;
    Result := LasHeader.YscaleFac * y + LasHeader.Yoffset;
end;

function tLAS_data.ExpandLAS_Z(ShotNumber : integer): float64;
var
   z : float64;
begin
   if (LidarPointType in [0..5]) then z := LidarPoints0^[ShotNumber].zPt;
   if (LidarPointType in [6..8]) then z := LidarPoints6^[ShotNumber].zPt;
   Result := LasHeader.ZscaleFac * z + LasHeader.Zoffset;
end;


function tLAS_data.GetMetadata: tStringList;
var
   I,j  : integer;
   TStr : tStringList;
begin
   {$IfDef RecordLASfiles} WriteLineToDebugFile('tLAS_data.GetMetadata in'); {$EndIf}
   Result := tStringList.Create;
   Result.Add('LAS header ' + ExtractFileName(LasFileName));
   Result.Add('');
   if (LasHeader.VersionMajor <> #0) then Result.Add('Version: ' + IntToStr(Ord(LasHeader.VersionMajor)) + '.' + IntToStr(Ord(LasHeader.VersionMinor)));
   Result.Add('SystemID: ' + ArrayOfCharToString(32,LASHeader.SystemID));
   Result.Add('Software: ' + ArrayOfCharToString(32,LASHeader.GenerateSoftware));
   Result.Add('Created: ' + IntToStr(LasHeader.CreateDay) + '/' + IntToStr(LasHeader.CreateYear));
   Result.Add('Point records: ' + IntToStr(NumPointRecs));
   Result.Add('Point Density: ' + RealToString(SingleFilePointDensity,-12,2) + ' pts/m²');

   if (LasHeader.GlobalEncoding and 2 = 2) then Result.Add('Internal waveform packets');
   if (LasHeader.GlobalEncoding and 4 = 4) then Result.Add('External waveform packets');
   if (LasHeader.GlobalEncoding and 8 = 8) then Result.Add('Synthetic return numbers');
   if (LasHeader.GlobalEncoding and 16 = 16) then Result.Add('WKT');

   Result.Add('Variable length records: ' + IntToStr(LasHeader.NumVarLenRecs ));
   Result.Add('Point data format: ' + IntToStr(LidarPointType));
   Result.Add('Point data rec length: ' + IntToStr(LasHeader.PointDataRecLen));
   for I := 1 to 5 do
      if (LasHeader.NumPtsByReturn[i] > 0) then
         Result.Add('   Return ' + IntToStr(i) + ': ' + IntToStr(LasHeader.NumPtsByReturn[i])  +  RealToString(LasHeader.NumPtsByReturn[i]/NumPointRecs * 100,8,2) + '%');
   Result.Add(ScaleFactorsString(LasHeader));
   Result.Add(OffSetString(Lasheader));
   Result.Add(XRangeString(LASheader));
   Result.Add(YRangeString(LASheader));
   Result.Add(ZRangeString(LASheader));
   Result.Add('');
   Result.Add('');
   if (LasHeader.NumVarLenRecs = 0) then begin
      Result.Add('No variable length records');
   end
   else begin
       for i := 1 to LasHeader.NumVarLenRecs do begin
          Result.Add('');
          Result.Add('Variable length record ' + IntToStr(i));
          Result.Add('  UserID=' + ArrayOfCharToString(16,VarLenRecHeader[i].UserId));
          Result.Add('  Description=' + ArrayOfCharToString(32,VarLenRecHeader[i].Description));
          Result.Add('  RecID=' + IntToStr(VarLenRecHeader[i].RecordId));
          Result.Add('  Rec Length=' + IntToStr(VarLenRecHeader[i].RecLenAfterHeader));
          if (VarLenRecHeader[i].RecordId = 34737) then begin
             Result.Add('ASCII projection: ' + ASCIIProjectionData);
          end;
       end;
   end;
   if (NumGeotiffKeys = 0) then begin
      Result.Add('No Geotiff keys');
   end
   else begin
      Result.Add('');
      Result.Add('Geotiff keys:');
      for j := 1 to NumGeotiffKeys do begin
         with FileGeoKeys[j] do begin
            Result.Add(IntegerToString(wKeyID,8) + IntegerToString(wValueOffset,9) + '  ' + GeoTIFFTagName(wKeyID) );
         end;
      end;
   end;
   if (ASCIIProjectionData <> '') then Result.Add(ASCIIProjectionData);

   if (lasProjectionDefinition.LasProjection <> Nil) and (lasProjectionDefinition.LasProjection.H_DatumCode <> '') then begin
      Result.Add('');
      Result.Add('SW corner: ' + LatLongDegreeToString(LAS_LatLong_Box.ymin,LAS_LatLong_Box.xmin) + ' NE corner: ' + LatLongDegreeToString(LAS_LatLong_Box.ymax,LAS_LatLong_Box.xmax));
      Result.Add('');
      TStr := Nil;
      TStr := lasProjectionDefinition.LasProjection.ProjectionParametersList;
      for j := 0 to pred(TStr.Count) do Result.Add(TStr.Strings[j]);
      TStr.Free;
   end;
   {$IfDef RecordLASfiles} writeLineToDebugFile('tLAS_data.GetMetadata out'); {$EndIf}
end;


function tLas_data.UTMLikeFile(BaseMapDraw : tMapDraw) : boolean;
begin
   Result := lasProjectionDefinition.RawXYZfile or
             ((lasProjectionDefinition.LASProjection <> Nil) and BaseMapDraw.IsThisMapUTM and lasProjectionDefinition.LasProjection.ThisIsUTMfile and (BaseMapDraw.PrimMapProj.projUTMZone = lasProjectionDefinition.LASProjection.projUTMZone));
end;


initialization
    LasColorsDone := false;
    //FudgeLidarCoords := false;
    AllowNoProjectionLAS := false;
finalization
   {$IfDef NoInLine} writeLineToDebugFile('NoInLine active in las_lidar'); {$EndIf}
   {$IfDef RecordLASfiles} WriteLineToDebugFile('RecordLASfiles active in las_lidar'); {$EndIf}
   {$IfDef RecordLASheader} WriteLineToDebugFile('RecordLASheader active in las_lidar'); {$EndIf}
   {$IfDef RecordLASColors} WriteLineToDebugFile('RecordLAScolors active in las_lidar'); {$EndIf}
   {$IfDef RecordLASHist} WriteLineToDebugFile('RecordLASHist active in las_lidar'); {$EndIf}
   {$IfDef RecordLASheaderKeys} writeLineToDebugFile('RecordLASheaderKeys active in las_lidar'); {$EndIf}
   {$IfDef RecordCreateEveryFile} writeLineToDebugFile('RecordCreateEveryFile active in las_lidar'); {$EndIf}
   {$IfDef RecordMergeLASfiles} writeLineToDebugFile('RecordMergeLASfiles active in las_lidar'); {$EndIf}
   {$IfDef RecordLAS_subset} writeLineToDebugFile('RecordLAS_subset active in las_lidar'); {$EndIf}
   {$IfDef RecordLASMemoryAlocations} writeLineToDebugFile('RecordLASMemoryAlocations active in las_lidar'); {$EndIf}
   {$IfDef RecordLASKML} writeLineToDebugFile('RecordLASKML active in las_lidar'); {$EndIf}
   {$IfDef RecordCreateLASfiles} writeLineToDebugFile('RecordCreateLASfiles active in las_lidar'); {$EndIf}
   {$IfDef RecordLASexport} writeLineToDebugFile('RecordLASexport active in las_lidar'); {$EndIf}
   {$IfDef RecordLASprojection} writeLineToDebugFile('RecordLASprojection active in las_lidar'); {$EndIf}
   {$IfDef RecordLASplot} writeLineToDebugFile('RecordLASplot active in las_lidar'); {$EndIf}
   {$IfDef RecordListFilesProcessed} writeLineToDebugFile('RecordListFilesProcessed active in las_lidar'); {$EndIf}
 end.

