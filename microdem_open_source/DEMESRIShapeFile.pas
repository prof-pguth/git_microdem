unit demESRIshapefile;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

//{$Define UseMemoryStream}

//{$Define InlinePlots}

//{$Define NoDBMaps}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordMissingPolygons}
   //{$Define TimeShapefile}
   //{$Define TrackNoPlots}
   //{$Define RecordDonuts} //major slowdown
   //{$Define RecordShapeFileLine}   //major slowdown
   //{$Define RecordLineCoords}      //major slowdown
   //{$Define RecordFindPoint}
   //{$Define RecordMergeShapefiles}
   //{$Define RecordPointProblem}
   //{$Define RecordGetLineCoords}    //major slowdown
   //{$Define RecordLineLength}    //major slowdown
   //{$Define RecordShapeFile}
   //{$Define RecordCreateShapeFile}
   //{$Define RawProjectInverse}      //for tracking shapefile creation
   //{$Define RecordCreateShapeFileFull}
   //{$Define RecordRotation}
   //{$Define RecordShapeFileWrite}
   //{$Define RecordShapeFileWriteFull}
   //{$Define TrackShapeDigitization}
   //{$Define RecordShapefilesReprojection}
   //{$Define RecordPart}         //major degradation
   //{$Define RecordReproject}    //major degradation
   //{$Define RecordClosing}
   //{$Define RecordMemoryStream}
   //{$Define RecordTooManyPoints}
   //{$Define RecordShapeFileLine}
   //{$Define RecordLineWidth}
   //{$Define TimeDBPlot}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end


   System.Types,System.UIConsts,System.UITypes,System.Diagnostics,
   SysUtils,Classes,Math,StrUtils,

   {$IfDef MSWindows}
      Windows,
   {$EndIf}

   {$IfDef VCL}
      VCL.Graphics,VCL.Buttons,VCL.StdCtrls,Forms,
      dbGrids, Controls,
      GIS_Scaled_symbols,
   {$EndIf}

   {$IfDef FMX}
      FMX.Graphics,
   {$EndIf}

   {$IfDef NoDBMaps}
   {$Else}
      DEMMapF,
   {$EndIf}

   {$IfDef NoDBGrafs}
   {$Else}
      BaseGraf,
   {$EndIf}

   DEMMapDraw,
   DEMDefs,
   DEMCoord, BaseMap,
   Petmar_types,PETMAR,PETMath;



type
   tShapeFile = class
      protected
      private
         SHXindexArray :  pshxindexarray;
         function DrawDonuts(MapDraw : tMapDraw; var Bitmap : tMyBitmap; RecNum : int32) : boolean;  {$IfDef InlinePlots} inline; {$EndIf}
         procedure ReadSHXindex;
         procedure MoveToStartShapefile; inline;
      public
         ShapeFile,IndexFile : System.file;
         {$IfDef UseMemoryStream}
            ShapeMemoryStream,IndexMemoryStream : tMemoryStream;
         {$EndIf}
         ShapeFileName : PathStr;
         MainFileHeader : sfMainFileHeader;
         ShapefileDEM,
         NumRecs : int32;
         NotLatLongShapeFile,
         BadPointCheck,
         ShapefileRandomAccess,
         NoDBFFile,
         DBFFileOnly : boolean;
         Symbol : PETMAR_types.tFullSymbolDeclaration;
         XPlaneMin,XPlaneMax,YPlaneMin,YPlaneMax,ZPlaneMin,ZPlaneMax : float64;

         CurrentLineRecNum,
         CurrentCCWRecNum  : int32;
         CurrentLineHaveZs : boolean;
         CurrentLineCoords : ^tdCoords;
         CurrentLineZs     : ^tdElevs;
         CurrentLinePartSize : tPartSize;
         CurrentPolyLineHeader  : sfPolyLineHeader;
         CurrentLinePartsCCW : packed array[1..sfMaxParts] of boolean;
         CurrentLinePointsPerPart : packed array[1..sfMaxParts] of int32;
         CurrentLineZRange : array[1..2] of float64;

         constructor Create(FileName : PathStr; var Success : boolean);
         destructor Destroy; override;

         procedure OpenSHPandSHX;
         procedure CloseSHPandSHX;

         function KeyInfo : string;
         procedure AreasCounterClockwise;
         function AreaWithHoles(RecNum : int32) : boolean;

         procedure PlotAllRecords(MapDraw : tMapDraw; var Bitmap : tMyBitmap; OSMdata : boolean = false);
         procedure SafePolygonOrPolyline(MapDraw : tMapDraw; Bitmap : tMyBitmap; PtsPolyLine : int32; var PolyLinePts : tPolyLinePts; APolygon : boolean);

         function LineLength(RecNum : int32; GetZs : boolean = false) : float64;
         function PointInRecord(RecNum : int32; Lat,Long : float64) : boolean;

         procedure AddFields(WhatAdd : tFieldsToAdd;  MyTable : tMyData = Nil);
         function AreaAndCentroid(PrimaryMapDatum : tMapProjection; RecNum : int32; var LatCent,LongCent : float64) : float64;
         procedure LineCenter(RecNum : int32; var XCent,YCent : float64);
         function DistanceToLineOrPerimeter(PrimaryMapDatum : tMapProjection; RecNum : int32; Lat,Long : float64; var LatOnLine,LongOnLine : float64) : float64;
         function LineAreaBoundBox(RecNum : int32) :  sfBoundBox;
         function LineHeading(RecNum : integer) : float64;
         procedure LineEndPoints(RecNum : int32; var x1,y1,x2,y2 : float64);
         function ReadPointRecord(RecNo : int32; var PointsHeader : sfPointsWithHeader) : boolean;
         function ReadPointRecord3D(RecNo : int32; var PointsHeader : sfPointsZWithHeader) : boolean;
         function ReadPolyLineHeader(RecNum : int32; var PolyLineHeader : sfPolyLineHeader) : boolean; //overload;
         function NextPolyLineHeader(var PolyLineHeader : sfPolyLineHeader) : boolean; inline;

         procedure WriteEditCurrentRecordPolyLine(RecNum : int32);

         function GetLineCoords(RecNum : int32; GetZS : boolean = false) : boolean;
         function GetLineCoordsAndZsFromDEM(DEM, RecNum: int32): boolean;

         function OldGetLineCoords(RecNum : int32; var NumPts,NParts : int32; var Coords : tdCoords; var PartSize : tPartSize) : boolean;
         function OldGetLineCoordsWithZs(RecNum : int32; var NumPts,NParts : int32; var Coords : tdCoords; var PartSize : tPartSize; GetZs : boolean; var zs : array of float64) : boolean;
         function OldGetLineCoordsAndZsFromDEM(DEM, RecNum: int32; var NumPts,NParts: int32; var Coords: tdCoords; var PartSize: tPartSize; var zs: array of float64): boolean;

         procedure DropInBucketIntoDEM(LastDEM : integer; MaxDEM : integer = 0; MinDEM : integer = 0; MeanDEM : integer = 0; DensityDEM : integer = 0);
         procedure RepairBoundingBoxInSHPandSHXfiles;

         {$IfDef UseMemoryStream}
            procedure OpenMemoryStreams(FName1,FName2 : PathStr);
         {$EndIf}

         {$IfDef NoDBMaps}
         {$Else}
            procedure LabelNthPointOnMap(BaseMap : tMapForm; var Bitmap : tMyBitmap; N : int32; Symbol : tDrawingSymbol; SymSize : int32; Color  : tPlatformColor; ReallyLabel : boolean);
            procedure RotateAndPlotSingleRecord(MapForm : tMapDraw; Bitmap : tMyBitmap; RecNum : int32;  RotationMatrix : MatrixType);
            function PlotSingleRecordMap(MapForm : tMapDraw; var Bitmap : tMyBitmap; RecNum : int32) : boolean;  {$IfDef InlinePlots} inline; {$EndIf}
            procedure PlotBoundingBox(MapForm : tMapForm);
            procedure PlotPointCloudOnMap(BaseMap : tMapForm; var Bitmap : tMyBitmap; MinAreaZ : float64 = 99; MaxAreaZ : float64 = -99; SymSize : integer = 2);
            procedure ScreenLocationOfPointRecord(MapForm : tMapDraw; RecNo : int32; var xpic,ypic : int32);
         {$EndIf}

         {$IfDef NoDBGrafs}
         {$Else}
            procedure PlotAllRecordsOnGraph(GraphForm : TThisBaseGraph; var Bitmap : tMyBitmap);
            procedure PlotSingleRecordGraph(GraphForm : TThisBaseGraph; var Bitmap : tMyBitmap; RecNum : int32);
            procedure ScreenLocationOfPointRecordGraph(GraphForm : TThisBaseGraph; RecNo : int32; var xpic,ypic : int32);
         {$EndIf}

         {$IfDef VCL}
            procedure RotateToNewShapefile(fName : PathStr;  RotationMatrix : MatrixType);
         {$EndIf}
   end;

  tShapeFileCreation = class
      protected
      private
      public
         glMainFileHeader : sfMainFileHeader;
         glShapeFile,glIndexFile : System.file;
         AppendingShapeFile : tShapeFile;
         Table : tMyData;
         DoSHP : boolean;
         PartSize : tPartSize;
         NParts,
         RecsInShapeStream,
         PtsInShapeStream : int32;
         ShapeStreamCoords : ^tdCoords;
         ShapeStreamZs : ^tdElevs;
         BBString : shortstring;
         DataTableSL : tStringList;
         zMin,zMax : float64;
         ShapeFileDigitizing : PathStr;
         RecordName : shortstring;
         FromDatum : tMapProjection;

         constructor Create(var inFromDatum : tMapProjection; fName : PathStr; MakeDBase : boolean; TheShapeType : int32; InDoSHP : boolean = true; inAppendingShapeFile : tShapeFile = Nil);
         destructor Destroy;

         procedure ProcessRecordForShapeFile;
         procedure OldProcessRecordForShapeFile(var dCoord : tDcoords; NumParts,NumPts : int32; Parts : array of int32; var zs : array of float64);

         procedure ProcessPointForShapeFile(Lat,Long : float64); overload;
         procedure ProcessPointForShapeFile(Lat,Long,zin : float64); overload;
         function CloseShapeFiles : integer;
         procedure AddLineLatLongToShapeFile(Lat1,Long1,Lat2,Long2,delta : float64; Convert : boolean);
         procedure AddBoxLatLongToShapeFile(Lat1,Long1,Lat2,Long2 : float64);
         procedure AddBoundBoxToShapeStream(bb : sfBoundBox);
         procedure AddLineUTMToShapeFile(x1,y1,x2,y2,delta : float64);
         procedure AddPointToShapeStream(Lat,Long : float64);
         procedure AddPointWithZToShapeStream(Lat,Long,z : float64);
         procedure GetPolyLineHeader(var PolyLineHeader : sfPolyLineHeader; var zMin,zMax : float64);
         {$IfDef NoDBMaps}
         {$Else}
            procedure ProcessShapeDigitization(TheMap : tMapForm);
            procedure ProcessShapeFileRecord;
         {$EndIf}
   end;


function ShapeFileDump(FName : PathStr) : tStringList;
procedure ScreenShapeFileDump(FName : PathStr);

function ItsAShapeFile(FileName : PathStr; var NoDBF : boolean) : boolean;


{$IfDef MSWindows}
   procedure AddProjectionFile(fName : PathStr);
   procedure CopyShapeFile(FromName,ToName : PathStr);
   procedure RenameShapeFile(FromName,ToName : PathStr);
   procedure MoveShapeFile(FromName,ToName : PathStr);
   procedure RepairShapeFileBoundingBoxes;
   procedure CheckShapeFileNameForSpaces(var fName : PathStr);
{$EndIf}

procedure DeleteShapeFile(FName : PathStr);
procedure InitializeBoundingBox(var BoundBox : sfBoundBox);

procedure CheckMainFileBoundingBox(var MainFileHeader : sfMainFileHeader;  LatLow,LatHigh,LongLow,LongHigh : float64); overload;
procedure CheckMainFileBoundingBox(var MainFileHeader : sfMainFileHeader; Lat,Long : float64); overload;
procedure CheckMainFileBoundingBox(var MainFileHeader : sfMainFileHeader; Lat,Long,z : float64); overload;
procedure CheckMainFileBoundingBox(var MainFileHeader : sfMainFileHeader; RecBoundBox : sfBoundBox); overload;

procedure CheckIfMainFileBoundingBoxIsOutside(MainFileHeader : sfMainFileHeader; var LatLow,LatHigh,LongLow,LongHigh : float64);
procedure PutBoundBoxInTable(var MyTable : tMyData; BoundBox : sfBoundBox; Name : ShortString = '');

function PointShapeFile(ShapeType : int32) : boolean;  inline;
function LineShapeFile(ShapeType : int32) : boolean;   inline;
function AreaShapeFile(ShapeType : int32) : boolean;   inline;
function ShapeFile3D(ShapeType : int32) : boolean; inline;
function LineOrAreaShapeFile(ShapeType : int32) : boolean;  inline;

procedure LineKMLtoStringList(fName : PathStr);
function IsItAShapefile(fName : PathStr) : boolean;
procedure PrepOSMFiles(OSMPath : PathStr = '');

function DoAShapeFile(fName : PathStr; Trim : boolean = false) : integer;
procedure ZipShapefile(DBontable : integer; IncludeDebug,SHZit : boolean);


var
   LastRecPolyLineHeader : sfPolyLineHeader;
   DrawPolygonsAsPolygons,       //this is for PLSS, where it is false, otherwise it is true
   NeedCentroid : boolean;
   ShapeFileNewLeader : ShortString;
   {$IfDef NoDBMaps}
   {$Else}
      MapForShapeDigitizing : tMapForm;
   {$EndIf}


implementation

uses
   {$IfDef VCL}
      DataBaseAddRec,
      Thread_timers,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
      DEMEROS,
   {$EndIf}

   PETdbUtils,
   DEMDef_routines,
   PetImage,
   Make_Tables,
   Nevadia_Main,
   DataBaseCreate;

procedure ZipShapefile(DBontable : integer; IncludeDebug,SHZit : boolean);
var
   TheFiles : tStringList;
   fName : PathStr;
   TStr : shortstring;

   procedure DoFile(aName : PathStr);
   begin
      if FileExists(aName) then begin
         CopyFile(aname,MDTempDir + ExtractFileName(aname));
         TheFiles.Add(MDTempDir + ExtractFileName(aname));
      end;
   end;

begin
   {$If Defined( RecordSHZ) or Defined(RecordExports)} WriteLineToDebugFile('Zipshapefile in'); {$EndIf}
   GISdb[DBonTable].SaveDataBaseStatus;
   TheFiles := tStringList.Create;
   DoFile(ChangeFileExt(GISdb[DBonTable].dbFullName,'.shp'));
   DoFile(ChangeFileExt(GISdb[DBonTable].dbFullName,'.shx'));
   DoFile(ChangeFileExt(GISdb[DBonTable].dbFullName,'.dbf'));
   fName := ChangeFileExt(GISdb[DBonTable].DBfullName,'.prj');
   if FileExists(fName) then DoFile(FName)
   else if FileExists(WKT_GCS_Proj_fName) then begin
      Petmar.CopyFile(WKT_GCS_Proj_fName,fName);
      DoFile(fName);
   end
   else MessageToContinue('Missing projection file ' + fName);


   if IncludeDebug and (GISdb[DBonTable].IniFileName <> '') and FileExists(GISdb[DBonTable].IniFileName) then begin
      {$IfDef RecordSHZ} WriteLineToDebugFile('TdbTableF.Zipshapefile1Click db ini'); {$EndIf}
      TheFiles.Add(GISdb[DBonTable].IniFileName);
   end;

   fName := GISdb[DBonTable].dbOpts.LinkTableName;
   if (fName <> '') and FileExists(fName) then begin
      {$IfDef RecordSHZ} WriteLineToDebugFile('TdbTableF.Zipshapefile1Click link table'); {$EndIf}
      TheFiles.Add(fName);
   end;
   if IncludeDebug then begin
      {$IfDef RecordSHZ} WriteLineToDebugFile('TdbTableF.Zipshapefile1Click debug log'); {$EndIf}
      TheFiles.Add(DebugFileName);
   end;
   if SHZit then TStr := '_' + CurrentTimeForFileName else TStr := '';
   fName := MDTempDir + ExtractFileNameNoExt(GISdb[DBonTable].dbFullName) + TStr + '.zip';

    {$If Defined( RecordSHZ) or Defined(RecordExports)} WriteLineToDebugFile('TdbTableF.Zipshapefile1Click call zip, files=' + IntToStr(TheFiles.Count)); {$EndIf}
   ZipMasterZipFiles(fName,TheFiles);
   TheFiles.Free;
   //DeleteShapeFile(FName);
   if SHZit then SysUtils.RenameFile(fname,ChangeFileExt(fName,'.shz'));
   {$If Defined( RecordSHZ) or Defined(RecordExports)}  WriteLineToDebugFile('Zipshapefile1Click'); {$EndIf}
end;



function DoAShapeFile(fName : PathStr; Trim : boolean = false) : integer;
begin
   if OpenNumberedGISDataBase(Result,fName,false) then begin
      GISdb[Result].AddSequentialIndex(RecNoFName,false);
      GISdb[Result].EmpSource.Enabled := false;
      if (GISDB[Result].ShapeFileType in [3,5,13,15]) then begin
         if not GISdb[Result].LatLongCornersPresent then GISdb[Result].aShapefile.AddFields(afBoundingBox,GISdb[Result].MyData);
      end
      else begin
         if not GISdb[Result].LatLongFieldsPresent then GISdb[Result].aShapefile.AddFields(afLatLong,GISdb[Result].MyData);
      end;
      if Trim then GISdb[Result].TrimAllStringFields;
      CloseAndNilNumberedDB(Result);
   end;
end;

procedure PrepOSMFiles(OSMPath : PathStr = '');
var
   TheFiles : tStringList;
   fName : PathStr;
   i : integer;
begin
   if (OSMPath = '') then GetDOSPath('OSM/Natural Earth/GADM shapefiles',OSMPath);
   TheFiles := Nil;
   FindMatchingFiles(OSMPath,'*.shp',TheFiles,6);
   if TheFiles.Count > 0 then begin
      for I := 1 to TheFiles.Count do begin
         WMDEM.SetPanelText(0,'Shapefile ' + IntToStr(i) + '/' + IntToStr(TheFiles.Count));
         fName := TheFiles.Strings[pred(i)];
         DoAShapeFile(fName, true);
      end;
      WMDEM.SetPanelText(0,'');
   end;
   TheFiles.Free;
end;



type
  tPointsArray = array[0..sfMaxPoints] of DEMDefs.sfPoints;


function IsItAShapefile(fName : PathStr) : boolean;
begin
   Result := FileExists(ChangeFileExt(fName,'.shp')) and FileExists(ChangeFileExt(fName,'.shx')) and FileExists(ChangeFileExt(fName,'.shp'));
end;


function PointShapeFile(ShapeType : int32) : boolean;
begin
   Result := ShapeType in [1,8,11,18];
end;

function LineShapeFile(ShapeType : int32) : boolean;
begin
   Result := ShapeType in [3,13,23];
end;

function AreaShapeFile(ShapeType : int32) : boolean;
begin
   Result := ShapeType in [5,15,25];
end;

function LineOrAreaShapeFile(ShapeType : int32) : boolean;
begin
   Result := LineShapeFile(ShapeType) or AreaShapeFile(ShapeType);
end;

function ShapeFile3D(ShapeType : int32) : boolean;
begin
   Result := ShapeType in [13,15,23,25];
end;

function ItsAShapeFile(FileName : PathStr; var NoDBF : boolean) : boolean;
begin
   Result := FileExists(ChangeFileExt(FileName,'.shp')) and FileExists(ChangeFileExt(FileName,'.shx'));
   if Result then NoDBF := not FileExists(ChangeFileExt(FileName,'.dbf'));
end;


procedure PutBoundBoxInTable(var MyTable : tMyData; BoundBox : sfBoundBox; Name : ShortString = '');   inline;
begin
   LongitudeAngleInRange(BoundBox.XMax);
   LongitudeAngleInRange(BoundBox.XMin);
   MyTable.SetFieldByNameAsFloat('LAT_LOW',BoundBox.YMin);
   MyTable.SetFieldByNameAsFloat('LAT_HI',BoundBox.YMax);
   MyTable.SetFieldByNameAsFloat('LONG_LOW',BoundBox.XMin);
   MyTable.SetFieldByNameAsFloat('LONG_HI',BoundBox.XMax);
   if (Name <> '') then MyTable.SetFieldByNameAsString('NAME',Name);
 end;


{$I shapefile_creation.inc}


procedure RepairShapeFileBoundingBoxes;
var
   DefaultFilter : byte;
   FileNames : tStringList;
   sf : tShapeFile;
   i : integer;
   success : boolean;
begin
   FileNames := tStringList.Create;
   FileNames.Add('');
   if GetMultipleFiles('Shape file','shape file|*.shp',FileNames,DefaultFilter) then begin
      for i := 0 to pred(FileNames.Count) do begin
         sf.Create(FileNames[i],success);
         sf.RepairBoundingBoxInSHPandSHXfiles;
         sf.Destroy;
      end;
   end;
end;

 procedure tShapeFile.MoveToStartShapefile;
 begin
      {$IfDef UseMemoryStream}
         if (ShapeMemoryStream = Nil) then begin
            seek(ShapeFile,SizeOf(sfMainFileHeader));
         end
         else begin
            ShapeMemoryStream.Seek(Offset, soFromBeginning);
         end;
      {$Else}
          seek(ShapeFile,SizeOf(sfMainFileHeader));
      {$EndIf}
end;



{$IfDef NoDBMaps}
{$Else}

procedure tShapeFile.ScreenLocationOfPointRecord(MapForm : tMapDraw; RecNo : int32; var xpic,ypic : int32);
var
   PointsHeader : sfPointsWithHeader;
begin
   xpic := MaxSmallInt;
   ypic := MaxSmallInt;
   if ReadPointRecord(RecNo,PointsHeader) then begin
       {$IfDef RecordShapefileLine} WriteLineToDebugFile('RecNo: '+ IntToStr(RecNo) + '  Point location: ' + RealToString(PointsHeader.x,12,2) + RealToString(PointsHeader.y,18,2)); {$EndIf}
       MapForm.LatLongDegreeToScreen(PointsHeader.y,PointsHeader.x,xpic,ypic);
   end;
end;


procedure tShapeFile.PlotPointCloudOnMap(BaseMap : tMapForm; var Bitmap : tMyBitmap; MinAreaZ : float64 = 99; MaxAreaZ : float64 = -99; SymSize : integer = 2);
var
   I,j,RecsRead,x,y,Count,OnBlock,BlocksNeeded : int32;
   BMPMemory : tBMPMemory;
   ShapePoints : ^tLotsOfPoints3D;
begin
   BMPMemory := tBMPMemory.Create(Bitmap);
   if (MinAreaZ > MaxAreaZ) then begin
      MinAreaZ := MainFileHeader.BoundBoxZMin;
      MaxAreaZ := MainFileHeader.BoundBoxZMax;
   end;
   if PointShapeFile(MainFileHeader.ShapeType) then begin
      OnBlock := 0;
      New(ShapePoints);
      reset(ShapeFile,1);
      BlockRead(ShapeFile,MainFileHeader,SizeOf(MainFileHeader));
      Int4Swap(MainFileHeader.FileLength);
      BlocksNeeded := succ( ( (MainFileHeader.FileLength - 50) div Sizeof(sfPointsZWithHeader)) div sfMaxPoints);
      repeat
         BlockRead(ShapeFile,ShapePoints^[1],sfMaxPoints*Sizeof(sfPointsZWithHeader),RecsRead);
         RecsRead := RecsRead div SizeOf(sfPointsZWithHeader);
         Thread_timers.ThreadTimers.Gauge1.Progress := round(100 * OnBlock/BlocksNeeded);
         ApplicationProcessMessages;
         inc(OnBlock);
         for i := 1 to RecsRead do begin
            inc(Count);
            if (Count >= MDDef.CloudMapThinFactor) then begin
                BaseMap.MapDraw.LatLongDegreeToScreen(ShapePoints^[i].y,ShapePoints^[i].x,x,y);
                if BaseMap.MapDraw.OnScreen(x,y) then begin
                   BMPMemory.SetPixelColorSize(x,y,SymSize,Petmar.SpectrumRGBFunct(CurrentLineZs^[j],MinAreaZ,MaxAreaZ));
                end;
                Count := 0;
            end;
         end;
      until System.eof(ShapeFile);
      Dispose(ShapePoints);
   end
   else begin
       for i := 1 to NumRecs do begin
          GetLineCoords(i,true);
          for j := 0 to pred(CurrentPolyLineHeader.NumPoints) do begin
             BaseMap.MapDraw.LatLongDegreeToScreen(CurrentLineCoords^[j].Lat,CurrentLineCoords^[j].Long,x,y);
              if BaseMap.MapDraw.OnScreen(x,y) then begin
                 BMPMemory.SetPixelColorSize(x,y,SymSize,Petmar.SpectrumRGBFunct(CurrentLineZs^[j],MinAreaZ,MaxAreaZ));
              end;
          end;
       end;
   end;
end;


procedure tShapeFile.LabelNthPointOnMap(BaseMap : tMapForm; var Bitmap : tMyBitmap; N : int32; Symbol : tDrawingSymbol; SymSize : int32; Color  : tPlatformColor; ReallyLabel : boolean);
var
   i,RecsRead,x,y,Count,BlocksNeeded : int32;
   ShapePoints : ^tLotsOfPoints3D;
begin
   New(ShapePoints);
   reset(ShapeFile,1);
   BlockRead(ShapeFile,MainFileHeader,SizeOf(MainFileHeader));
   Int4Swap(MainFileHeader.FileLength);
   Count := 0;
   BlocksNeeded := succ( ( (MainFileHeader.FileLength - 50) div Sizeof(sfPointsZWithHeader)) div sfMaxPoints);
   repeat
      BlockRead(ShapeFile,ShapePoints^[1],sfMaxPoints*Sizeof(sfPointsZWithHeader),RecsRead);
      RecsRead := RecsRead div SizeOf(sfPointsZWithHeader);
      for i := 1 to RecsRead do begin
         inc(Count);
         if (Count mod n = 0) then begin
             BaseMap.MapDraw.LatLongDegreeToScreen(ShapePoints^[i].y,ShapePoints^[i].x,x,y);
             if BaseMap.MapDraw.OnScreen(x,y) then begin
                Petmar.ScreenSymbol(Bitmap.Canvas,x,y,Symbol,SymSize,Color);
                if ReallyLabel then Bitmap.Canvas.TextOut(x+5,y+5,IntToStr(Count));
             end;
         end;
      end;
   until System.eof(ShapeFile);
   Dispose(ShapePoints);
end;


procedure tShapeFile.SafePolygonOrPolyline(MapDraw : tMapDraw; Bitmap : tMyBitmap; PtsPolyLine : int32; var PolyLinePts : tPolyLinePts; APolygon : boolean);
var
   i : integer;
begin
   {$IfDef RecordShapeFileLine} WriteLineToDebugFile('enter SafePolygonOrPolyline width=' + IntToStr(Bitmap.Canvas.Pen.Width) + ' color=' + IntToStr(Bitmap.Canvas.Pen.Color)); {$EndIf}

   {$IfDef RecordLineCoords} for I := 0 to pred(PtsPolyLine) do writelinetodebugFile(IntegerToString(PolyLinePts[i].x,8) + IntegerToString(PolyLinePts[i].y,8)); {$EndIf}

   {$IfDef VCL}
      Bitmap.Canvas.Pixels[PolyLinePts[0].x,PolyLinePts[0].y] := Bitmap.Canvas.Pen.Color;
      if APolygon and DrawPolygonsAsPolygons then begin
         Bitmap.Canvas.Polygon(slice(PolyLinePts,PtsPolyLine));
      end
      else begin
         MapDraw.CarefulPolyLineDraw(Bitmap,PtsPolyLine,PolyLinePts);
      end;
   {$EndIf}
end;


function tShapeFile.DrawDonuts(MapDraw : tMapDraw; var Bitmap : tMyBitmap; RecNum : int32) : boolean;
const
   OffscreenBuffer = 256000;
var
   i,x,y,xp,yp,Pt,Lastxp,lastYP : int32;
   PlotIt : boolean;
   RecordBitmap,HolesBitmap : tMyBitmap;
   {$IfDef VCL}
      p0,p1,p2 : prgb;
      PolyLinePoints  : ^tPolyLinePts;
   {$EndIf}
   {$IfDef FMX}
      Path : FMX.Graphics.tPathData;
   {$EndIf}
begin
   Result := false;
   if GetLineCoords(RecNum) then begin
      if MapDraw.AFullWorldMap or MapDraw.LatLongBoxOnScreen(CurrentPolyLineHeader.BoundBox) then begin
         Result := true;
         {$IfDef RecordDonuts}
            WriteLineToDebugFile('tShapeFile.DrawDonuts, recnum=' + IntToStr(RecNum) +'  parts=' + IntToStr(CurrentPolyLineHeader.NumParts)+ '  points=' + IntToStr(CurrentPolyLineHeader.NumPoints));
            WriteLineToDebugFile('CurrentLinePartSize[1]=' + IntToStr(CurrentLinePartSize[1]) + '  CurrentLinePointsPerPart[1]=' + IntToStr(CurrentLinePointsPerPart[1]));
            WriteLineToDebugFile('SW corner=' + LatLongDegreeToString(CurrentPolyLineHeader.BoundBox.YMin,CurrentPolyLineHeader.BoundBox.YMax + '  NE corner=' + LatLongDegreeToString(CurrentPolyLineHeader.BoundBox.XMin,CurrentPolyLineHeader.BoundBox.XMax);
         {$EndIf}

         if AreaShapeFile(MainFileHeader.ShapeType) and (CurrentPolyLineHeader.NumParts > 1) then begin
            {$IfDef VCL}
               CloneBitmap(Bitmap,RecordBitmap);
               RecordBitmap.Canvas.Pen.Color := Bitmap.Canvas.Pen.Color;
               RecordBitmap.Canvas.Pen.Width := Bitmap.Canvas.Pen.Width;
               RecordBitmap.Canvas.Brush.Color := Bitmap.Canvas.Brush.Color;
               RecordBitmap.Canvas.Brush.Style := Bitmap.Canvas.Brush.Style;

               CloneBitmap(Bitmap,HolesBitmap);
               HolesBitmap.Canvas.Pen.Color := Bitmap.Canvas.Pen.Color;
               HolesBitmap.Canvas.Pen.Width := Bitmap.Canvas.Pen.Width;
               HolesBitmap.Canvas.Brush.Color := Petmar.clAlmostBlack;
               HolesBitmap.Canvas.Brush.Style := bsSolid;
            {$EndIf}
             AreasCounterClockwise;
         end;

         {$IfDef VCL}
            New(PolyLinePoints);
         {$EndIf}

         {$IfDef FMX}
            Path := tPathData.Create;
         {$EndIf}

         for i := 1 to CurrentPolyLineHeader.NumParts do begin
            for Pt := CurrentLinePartSize[i] to pred(CurrentLinePartSize[i] + (CurrentLinePointsPerPart[i])) do begin
               MapDraw.LatLongDegreeToScreen(CurrentLineCoords^[Pt].Lat,CurrentLineCoords^[Pt].Long,xp,yp);

               {$IfDef ExOSM}
               {$Else}
                  if BadPointCheck then begin
                     if (Pt = CurrentLinePartSize[i]) then begin
                        LastXP := xp;
                        LastYP := yp;
                     end
                     else begin
                        if (abs(LastXP-xp) > MDDef.OSMmaxLength) or (abs(LastYP-yp) > MDDef.OSMmaxLength) then begin
                           xp := LastXP;
                           yp := LastYP;
                        end;
                     end;
                  end;
               {$EndIf}

               {$IfDef VCL}
                  if AreaShapeFile(MainFileHeader.ShapeType) and ((abs(xp) > OffscreenBuffer) or (abs(yp) > OffscreenBuffer)) then begin
                     Dispose(PolyLinePoints);
                     exit;
                  end;
                  PolyLinePoints^[Pt-CurrentLinePartSize[i]].x := xp;
                  PolyLinePoints^[Pt-CurrentLinePartSize[i]].y := yp;
               {$EndIf}
               {$IfDef FMX}
                  if (Pt = CurrentLinePartSize[i]) then Path.MoveTo(PointF(xp,yp))
                  else Path.LineTo(PointF(xp,yp));
               {$EndIf}
               LastXP := xp;
               LastYP := yp;
            end;
            PlotIt := true;

            {$IfDef FMX}
               Bitmap.Canvas.DrawPath(Path,255);
            {$EndIf}

            {$IfDef VCL}
               if PlotIt then begin
                  if LineShapeFile(MainFileHeader.ShapeType) or (CurrentPolyLineHeader.NumParts = 1) then begin
                     SafePolygonOrPolyline(MapDraw,Bitmap,CurrentLinePointsPerPart[i],PolyLinePoints^,AreaShapeFile(MainFileHeader.ShapeType))
                  end
                  else begin
                     if (not CurrentLinePartsCCW[i]) then begin
                        SafePolygonOrPolyline(MapDraw,RecordBitmap,CurrentLinePointsPerPart[i],PolyLinePoints^,AreaShapeFile(MainFileHeader.ShapeType));
                     end
                     else begin //Holes are ccw
                        SafePolygonOrPolyline(MapDraw,HolesBitmap,CurrentLinePointsPerPart[i],PolyLinePoints^,AreaShapeFile(MainFileHeader.ShapeType));
                     end;
                  end;
               end;
            {$EndIf}
         end;

         {$IfDef VCL}
            Dispose(PolyLinePoints);
         {$EndIf}

         if AreaShapeFile(MainFileHeader.ShapeType) and (CurrentPolyLineHeader.NumParts > 1) then begin
            {$IfDef VCL}
                for y := 0 to pred(Bitmap.Height) do begin
                   p0 := Bitmap.ScanLine[y];
                   p1 := RecordBitmap.ScanLine[y];
                   p2 := HolesBitmap.ScanLine[y];
                   for x := 0 to pred(Bitmap.Width) do begin
                      if (not SameColor(p2[x],rgbTripleAlmostBlack)) and (not SameColor(p1[x],rgbTripleWhite)) then p0[x] := p1[x];
                   end;
                end;
                HolesBitmap.Free;
                RecordBitmap.Free;
             {$EndIf}
         end;
      end;
   end;
end;


function tShapeFile.PlotSingleRecordMap(MapForm : tMapDraw; var Bitmap : tMyBitmap; RecNum : int32) : boolean;
begin
   {$If Defined(RecordShapeFileLineProblems) or Defined (RecordLineWidth)} WriteLineToDebugFile('tShapeFile.PlotSingeRecord RecNo: '+ IntToStr(RecNum) + '   line width =' + IntToStr(Bitmap.Canvas.Pen.Width)); {$EndIf}
   if PointShapeFile(MainFileHeader.ShapeType) then begin
      ScreenLocationOfPointRecord(MapForm,RecNum,dbxpic,dbypic);
      Result := MapForm.OnScreen(dbxpic,dbypic);
      if Result then ScreenSymbol(Bitmap.Canvas,dbXPic,dbYPic,Symbol);
   end
   else begin //line or area
      Result := DrawDonuts(MapForm,Bitmap,RecNum);
   end;
end;


procedure tShapeFile.PlotBoundingBox(MapForm: tMapForm);
var
   x1,y1,x2,y2 : int32;
begin
   Mapform.MapDraw.LatLongDegreeToScreen(MainFileHeader.BoundBox.YMax,MainFileHeader.BoundBox.XMin,x1,y1);
   Mapform.MapDraw.LatLongDegreeToScreen(MainFileHeader.BoundBox.YMin,MainFileHeader.BoundBox.XMax,x2,y2);
   Mapform.Image1.Canvas.Rectangle(x1,y1,x2,y2);
end;


procedure tShapeFile.RotateAndPlotSingleRecord(MapForm : tMapDraw; Bitmap : tMyBitmap; RecNum : int32; RotationMatrix : MatrixType);  //RotationData : VectorType);
var
   PolyLinePoints  : ^tPolyLinePts;
   i,j,k,EndPt,NumPts   : int32;
   InitVec,FinalVec : VectorType;
   Lat,Long : float64;
begin
   {$If Defined(RecordShapeFileLineProblems) or Defined(RecordRotation)} writeLineToDebugFile('PlotSingeRecord RecNo: '+ IntToStr(RecNum)); {$EndIf}
   if not PointShapeFile(MainFileHeader.ShapeType) then begin
      CurrentLineRecNum := -99;  //needed because we put the rotated coordinates in the orignal line record
      if GetLineCoords(RecNum) then begin
         New(PolyLinePoints);
         for i := 0 to pred(CurrentPolyLineHeader.NumPoints) do begin
            {$If Defined(RecordRotation)} if (i < 5) then WriteLineToDebugFile(IntToStr(i) + RealtoString(CurrentLineCoords[i].Lat*DegToRad,12,5) + RealtoString(CurrentLineCoords[i].Long*DegToRad,12,5)); {$EndIf}
            LatLongToCartesian(CurrentLineCoords[i].Lat*DegToRad,CurrentLineCoords[i].Long*DegToRad,InitVec);
            RotatePoint(RotationMatrix,InitVec,FinalVec);
            CartesianToLatLong(FinalVec,CurrentLineCoords[i].Lat,CurrentLineCoords[i].Long);
            if (CurrentLineCoords[i].Long > 180 * DegToRad) then CurrentLineCoords[i].Long := CurrentLineCoords[i].Long - 360 * DegToRad;
            {$If Defined(RecordRotation)} if (i < 5) then WriteLineToDebugFile(IntToStr(i) + RealtoString(CurrentLineCoords[i].Lat*DegToRad,12,5) + RealtoString(CurrentLineCoords[i].Long*DegToRad,12,5)); {$EndIf}
         end;

         for j := 1 to CurrentPolyLineHeader.NumParts do begin
            if (j = CurrentPolyLineHeader.NumParts) then EndPt := pred(CurrentPolyLineHeader.NumPoints)
            else EndPt := pred(CurrentLinePartSize[succ(j)]);
            for i := CurrentLinePartSize[j] to EndPt do begin
               k := i - CurrentLinePartSize[j];
               MapForm.LatLongRadiansToScreen(CurrentLineCoords[i].Lat,CurrentLineCoords[i].Long,PolyLinePoints^[k].x,PolyLinePoints^[k].y);
            end;
            NumPts := succ(EndPt - CurrentLinePartSize[j]);
            SafePolygonOrPolyline(MapForm,Bitmap,NumPts,PolyLinePoints^,AreaShapeFile(MainFileHeader.ShapeType));
         end;
         Dispose(PolyLinePoints);
     end {if};
  end;
end;

procedure tShapeFile.RotateToNewShapefile(fName : PathStr;  RotationMatrix : MatrixType);
var
   i,j,k   : int32;
   InitVec,FinalVec : VectorType;
   Lat,Long : float64;
   fName2 : PathStr;
   ShapeFileCreator : tShapeFileCreation;
begin
   if not PointShapeFile(MainFileHeader.ShapeType) then begin
      ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,false,MainFileHeader.ShapeType);
      fName2 := ChangeFileExt(fname,DefaultDBExt);
      MakeBasicBoundingBoxTable(fName2);
      ShapeFileCreator.Table := tMyData.Create(fName2);

      for j := 1 to NumRecs do begin
         GetLineCoords(j);
         for k := 0 to pred(CurrentPolyLineHeader.NumPoints) do begin
            LatLongToCartesian(CurrentLineCoords^[k].Lat*DegToRad,CurrentLineCoords^[k].Long*DegToRad,InitVec);
            RotatePoint(RotationMatrix,InitVec,FinalVec);
            CartesianToLatLong(FinalVec,Lat,Long);
            Lat := Lat / DegToRad;
            Long := Long / DegToRad;
            Long := PetMath.FindCompassAngleInRange(Long);
            ShapeFileCreator.AddPointToShapeStream(Lat,Long);
         end;
         ShapeFileCreator.RecordName := IntToStr(j);
         ShapeFileCreator.ProcessRecordForShapeFile;
      end;
      ShapeFileCreator.CloseShapeFiles;
   end;
end;


procedure tShapeFileCreation.ProcessShapeDigitization(TheMap : tMapForm);
var
   Lat,Long : float64;
   z : float32;
begin
   if (DEMNowDoing in [ShapePoint]) or DEMMapf.PolygonDigitizing(DEMNowDoing) or DEMMapf.PolylineDigitizing(DEMNowDoing) then begin
      TheMap.MapDraw.ScreenToLatLongDegree(theMap.LastX,theMap.LastY,Lat,Long);
      if MDDef.Create3DShapefiles and (theMap.MapDraw.DEMonMap <> 0) then begin
         DEMGlb[theMap.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z);
         AddPointWithZToShapeStream(Lat,Long,z);
      end
      else AddPointToShapeStream(Lat,Long);

      {$IfDef TrackShapeDigitization} WritelineToDebugFile('Pt: ' + IntToStr(PtsInShapeStream) + '  ' + LatLongDegreeToString(Lat,Long));      {$EndIf}
      if (DEMNowDoing = ShapePoint) then exit;
      if (PtsInShapeStream = 1) then begin
         StartX := theMap.Lastx;
         StartY := theMap.LastY;
         MapScreenX1 := theMap.Lastx;
         MapScreenY1 := theMap.LastY;
      end
      else begin
         theMap.Image1.Canvas.MoveTo(MapScreenX1,MapScreenY1);
         theMap.Image1.Canvas.LineTo(theMap.Lastx,theMap.Lasty);
         theMap.MakeLastPointFirst;
      end;
   end;
end;

{$EndIf}


{$IfDef NoDBGrafs}
{$Else}

procedure tShapeFile.PlotAllRecordsOnGraph(GraphForm : TThisBaseGraph; var Bitmap : tMyBitmap);
var
   i : int32;
begin
   for i := 1 to NumRecs do begin
      PlotSingleRecordGraph(GraphForm,Bitmap,i);
   end;
end;


procedure tShapeFile.ScreenLocationOfPointRecordGraph(GraphForm : TThisBaseGraph; RecNo : int32; var xpic,ypic : int32);
var
   PointsHeader : sfPointsWithHeader;
   PointsHeader3D : sfPointsZWithHeader;
   xp,yp,zp : float64;
begin
   xpic := MaxSmallInt;
   ypic := MaxSmallInt;
   if (MainFileHeader.ShapeType = 1) then begin
      if not ReadPointRecord(RecNo,PointsHeader) then exit;
      xp := PointsHeader.x;
      yp := PointsHeader.y;
   end
   else begin
      if not ReadPointRecord3D(RecNo,PointsHeader3D) then exit;
      if GraphForm.GraphDraw.GraphPlane in [0,1] then begin
         xp := PointsHeader3D.x;
      end
      else begin
         xp := PointsHeader3D.y;
      end;
      if GraphForm.GraphDraw.GraphPlane in [0] then begin
         yp := PointsHeader3D.y;
      end
      else begin
         yp := PointsHeader3D.z;
      end;
      if GraphForm.GraphDraw.GraphPlane in [0] then begin
         zp := PointsHeader3D.z;
      end
      else if GraphForm.GraphDraw.GraphPlane in [1] then begin
         zp := PointsHeader3D.y;
      end
      else begin
         zp := PointsHeader3D.x;
      end;
   end;

   if abs(XPlaneMin - XPlaneMax) > 0.01 then begin
      if (xp > XPlaneMax) or (xp < XPlaneMin) then exit;
   end;

   if abs(YPlaneMin - YPlaneMax) > 0.01 then begin
      if (yp > YPlaneMax) or (yp < YPlaneMin) then exit;
   end;

   if abs(ZPlaneMin - ZPlaneMax) > 0.01 then begin
      if (zp > ZPlaneMax)  or (zp < ZPlaneMin) then exit;
   end;

    {$IfDef RecordShapefileLine} WriteLineToDebugFile('RecNo: '+ IntToStr(RecNo) + '  Point location: ' + RealToString(xp,12,2) + RealToString(yp,18,2)); {$EndIf}
    xpic := GraphForm.GraphDraw.GraphX(xp);
    ypic := GraphForm.GraphDraw.GraphY(yp);
end;


procedure tShapeFile.PlotSingleRecordGraph(GraphForm : TThisBaseGraph; var Bitmap : tMyBitmap; RecNum : int32);
var
   PolyLinePoints  : ^tPolyLinePts;
   i : int32;
begin
   {$IfDef RecordShapeFileLine} WriteLineToDebugFile('PlotSingeRecord RecNo: '+ IntToStr(RecNum)); {$EndIf}
   if (MainFileHeader.ShapeType = 0) then exit;

   if PointShapeFile(MainFileHeader.ShapeType) then begin  {Point shape file}
      ScreenLocationOfPointRecordGraph(GraphForm,RecNum,dbxpic,dbypic);
      ScreenSymbol(Bitmap.Canvas,dbXPic,dbYPic,Symbol);
   end
   else begin
      if ReadPolyLineHeader(RecNum,CurrentPolyLineHeader) and (MainFileHeader.ShapeType <> 0)  then begin
         if (CurrentPolyLineHeader.NumParts > 1) then begin
            //DrawDonuts(MapForm,Bitmap,RecNum,PolyLineHeader);
         end
         else begin
            if GetLineCoords(RecNum) then begin
                {$IfDef RecordShapeFileLine} WriteLineToDebugFile('Pts='+ IntToStr(CurrentPolyLineHeader.NumPoints)); {$EndIf}
                for i := 0 to pred(CurrentPolyLineHeader.NumPoints) do begin
                   PolyLinePoints^[i].x := GraphForm.GraphDraw.GraphX(CurrentLineCoords^[i].Long);
                   PolyLinePoints^[i].y := GraphForm.GraphDraw.GraphY(CurrentLineCoords^[i].Lat);
                end;
                if AreaShapeFile(MainFileHeader.ShapeType) then Bitmap.Canvas.Polygon(slice(PolyLinePoints^,CurrentPolyLineHeader.NumPoints))
                else Bitmap.Canvas.PolyLine(slice(PolyLinePoints^,CurrentPolyLineHeader.NumPoints));
            end;
            Dispose(PolyLinePoints);
         end;
      end;
  end {with};
end;
{$EndIf}


procedure LineKMLtoStringList(fName : PathStr);
var
   FileInMemory : tStringList;
   i,j,k : integer;
   Str, aLine : ANSIString;
   ZStr,NameStr,LatStr,LongStr : shortstring;
   Stop,  Adding : boolean;
   //Table : tMyData;
   ShapeFileCreator : tShapeFileCreation;
begin
   ShowHourglassCursor;
   FileInMemory := tStringList.Create;
   FileInMemory.LoadFromFile(fName);
   fName := ChangeFileExt(fName,DefaultDBExt);
   //Make_Tables.MakeBasicBoundingBoxTable(fName);
   //Table := tMyData.Create(FName);
   ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,true,3);
   Adding := false;
   i := 0;
   while I <= pred(FileInMemory.count) do begin
       Str := UpperCase(ptTrim(UpperCase(FileInMemory.Strings[i])));
        if copy(str,1,10) = '<PLACEMARK' then begin
           Delete(Str,1,10);
           Adding := true;
        end;
        if Adding then begin
           if Copy(Str,1,6) = '<NAME>' then begin
              Delete(Str,1,6);
              NameStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,'<');
           end;
           if (Copy(Str,1,13)) = '<COORDINATES>' then begin
              repeat
                  inc(i);
                  Str := UpperCase(ptTrim(UpperCase(FileInMemory.Strings[i])));
                  Stop := (Copy(Str,1,14)) = '</COORDINATES>';
                  if not stop then repeat
                     LongStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,',',true,true);
                     LatStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,',',true,true);
                     ZStr := Petmar_Types.BeforeSpecifiedCharacterANSI(Str,' ',true,true);
                     ShapeFileCreator.AddPointToShapeStream(StrToFloat(LatStr),StrToFloat(LongStr));
                  until length(Str) = 0;
              until stop;
           end;
           if str = '</PLACEMARK>' then begin
              Adding := false;
              //Table.Insert;
              ShapeFileCreator.RecordName := NameStr;
              ShapeFileCreator.ProcessRecordForShapeFile;
              //Table.SetFieldByNameAsString('NAME',NameStr);
              //Table.Post;
           end;
        end;
        inc(i);
      end;
    ShapeFileCreator.CloseShapeFiles;
    FileInMemory.Destroy;
end;


{$IfDef MSWindows}

      procedure AddProjectionFile(fName : PathStr);
      begin
         if FileExists(WKT_GCS_Proj_fName) then begin
            Petmar.CopyFile(WKT_GCS_Proj_fName,ChangeFileExt(fName,'.prj') );
         end;
      end;

      procedure CopyShapeFile(FromName,ToName : PathStr);
      begin
         Petmar.CopyFile(ChangeFileExt(FromName,'.shp'),ChangeFileExt(ToName,'.shp') );
         Petmar.CopyFile(ChangeFileExt(FromName,'.shx'),ChangeFileExt(ToName,'.shx') );
         Petmar.CopyFile(ChangeFileExt(FromName,'.dbf'),ChangeFileExt(ToName,'.dbf') );
         Petmar.CopyFile(ChangeFileExt(FromName,'.prj'),ChangeFileExt(ToName,'.prj') );
         AddProjectionFile(ToName);
      end;


      procedure RenameShapeFile(FromName,ToName : PathStr);
      begin
         SysUtils.RenameFile(ChangeFileExt(FromName,'.shp'),ChangeFileExt(ToName,'.shp') );
         SysUtils.RenameFile(ChangeFileExt(FromName,'.shx'),ChangeFileExt(ToName,'.shx') );
         SysUtils.RenameFile(ChangeFileExt(FromName,'.dbf'),ChangeFileExt(ToName,'.dbf') );
         SysUtils.RenameFile(ChangeFileExt(FromName,'.prj'),ChangeFileExt(ToName,'.prj') );
      end;


      procedure MoveShapeFile(FromName,ToName : PathStr);
      begin
         ToName := ToName + ExtractFileNameNoExt(FromName);
         CopyShapeFile(FromName,ToName);
         DeleteShapeFile(FromName);
      end;


      procedure CheckShapeFileNameForSpaces(var fName : PathStr);
      var
         oName : PathStr;
      begin
         if StrUtils.AnsiContainsText(fName,' ') then begin
            oName := ChangeFileExt(fName,'.shx');
            CheckFileNameForSpaces(oName);
            oName := ChangeFileExt(fName,'.dbf');
            CheckFileNameForSpaces(oName);
            oName := ChangeFileExt(fName,'.prj');
            CheckFileNameForSpaces(oName);
            oName := ChangeFileExt(fName,'.shp');
            CheckFileNameForSpaces(oName);
            fName := oName;
         end;
      end;

{$EndIf}


procedure DeleteShapeFile(FName : PathStr);
begin
   DeleteFileIfExists(ChangeFileExt(fName,'.shp'));
   DeleteFileIfExists(ChangeFileExt(fName,'.shx'));
   DeleteFileIfExists(ChangeFileExt(fName,'.dbf'));
   DeleteFileIfExists(ChangeFileExt(fName,'.prj'));
   DeleteFileIfExists(ChangeFileExt(fName,'.cpg'));
   DeleteFileIfExists(ChangeFileExt(fName,'.sbn'));
end;


procedure tShapeFile.AreasCounterClockwise;
var
   i : int32;

      function Clockwise(i : int32) : boolean;
      var
         lastx,lasty,asum : float64;
         j : int32;
      begin
         aSum := 0;
         for j := CurrentLinePartSize[i] to pred(CurrentLinePartSize[i] + (CurrentLinePointsPerPart[i])) do begin
            if (j > CurrentLinePartSize[i]) then aSum := aSum + Lastx * CurrentLineCoords^[j].Lat - CurrentLineCoords^[j].Long * Lasty;
            LastX := CurrentLineCoords^[j].Long;
            LastY := CurrentLineCoords^[j].Lat;
         end;
         Result := aSum <= 0; //negative means clockwise, positive CCW
      end;

begin
   if (CurrentLineRecNum = CurrentCCWRecNum) then exit;
   for i := 1 to CurrentPolyLineHeader.NumParts do CurrentLinePartsCCW[i] := not Clockwise(i);
   CurrentCCWRecNum := CurrentLineRecNum;
end;


function tShapeFile.AreaWithHoles(RecNum : int32): boolean;
var
   Holes,j : int32;
begin
    GetLineCoords(RecNum);
    Holes := 0;
    if (CurrentPolyLineHeader.NumParts > 1) then begin
         AreasCounterClockwise;
         with CurrentPolyLineHeader do begin
            for j := 1 to NumParts  do begin
               if CurrentLinePartsCCW[j] then inc(holes);
            end;
         end;
     end;
     Result :=  Holes > 0;
end;


procedure InitializeBoundingBox(var BoundBox : sfBoundBox);
begin
   BoundBox.XMin := 9999999.99;
   BoundBox.YMin := 9999999.99;
   BoundBox.XMax := -9999999.99;
   BoundBox.YMax := -9999999.99;
end;



procedure CheckMainFileBoundingBox(var MainFileHeader : sfMainFileHeader; RecBoundBox : sfBoundBox);
begin
   CheckMainFileBoundingBox(MainFileHeader, RecBoundBox.YMin,RecBoundBox.YMax,RecBoundBox.XMin,RecBoundBox.XMax);
end;

procedure CheckMainFileBoundingBox(var MainFileHeader : sfMainFileHeader; LatLow,LatHigh,LongLow,LongHigh : float64);
begin
   if (LongLow < MainFileHeader.BoundBox.XMin) then MainFileHeader.BoundBox.XMin := LongLow;
   if (LongHigh > MainFileHeader.BoundBox.XMax) then MainFileHeader.BoundBox.XMax := LongHigh;
   if (LatLow < MainFileHeader.BoundBox.YMin) then MainFileHeader.BoundBox.YMin := LatLow;
   if (LatHigh > MainFileHeader.BoundBox.YMax) then MainFileHeader.BoundBox.YMax := LatHigh;
end;


procedure CheckMainFileBoundingBox(var MainFileHeader : sfMainFileHeader; Lat,Long : float64);
begin
   PetMath.CompareValueToExtremes(Lat,MainFileHeader.BoundBox.YMin,MainFileHeader.BoundBox.YMax);
   PetMath.CompareValueToExtremes(Long,MainFileHeader.BoundBox.XMin,MainFileHeader.BoundBox.XMax);
end;


procedure CheckMainFileBoundingBox(var MainFileHeader : sfMainFileHeader; Lat,Long,z : float64);
begin
   PetMath.CompareValueToExtremes(Lat,MainFileHeader.BoundBox.YMin,MainFileHeader.BoundBox.YMax);
   PetMath.CompareValueToExtremes(Long,MainFileHeader.BoundBox.XMin,MainFileHeader.BoundBox.XMax);
   PetMath.CompareValueToExtremes(z,MainFileHeader.BoundBoxZMin,MainFileHeader.BoundBoxZMax);
end;


procedure CheckIfMainFileBoundingBoxIsOutside(MainFileHeader : sfMainFileHeader; var LatLow,LatHigh,LongLow,LongHigh : float64);
begin
   if (MainFileHeader.BoundBox.XMin < LongLow) then LongLow := MainFileHeader.BoundBox.XMin;
   if (MainFileHeader.BoundBox.XMax > LongHigh) then LongHigh := MainFileHeader.BoundBox.XMax;
   if (MainFileHeader.BoundBox.YMin < LatLow) then LatLow := MainFileHeader.BoundBox.YMin;
   if (MainFileHeader.BoundBox.YMax > LatHigh) then LatHigh := MainFileHeader.BoundBox.YMax;
end;


procedure ScreenShapeFileDump(FName : PathStr);
var
   Output : tStringList;
begin
   Output := ShapeFileDump(FName);
   {$IfDef VCL} DisplayAndPurgeStringList(Output,'Shape file ' + fName); {$EndIf}
end;


function ShapeFileDump(FName : PathStr) : tStringList;
var
   PolyLineHeader  : sfPolyLineHeader;
   TStr : ShortString;
   NumRecs,NumRead,i : int32;
   IndexRecords    : array[1..100] of sfIndexRecord;
   Dir             : DirStr;
   bName           : NameStr;
   Ext             : ExtStr;
   TableStruct,Prjfile     : tStringList;
   Table         : tMyData;
   FullSummary     : boolean;
   PointsHeader : sfPointsWithHeader;
   PointZsHeader : sfPointsZWithHeader;
   MenuStr : ShortString;
   glShapeFile,glIndexFile : file;
   glMainFileHeader : sfMainFileHeader;

         procedure OutputHeader(Which : shortString);
         var
            i : int32;
            MenuStr : ShortString;
         begin
             Result.add('');
             Result.add(Which);
             Result.add('');
             Int4Swap(glMainFileHeader.FileCode);
             Int4Swap(glMainFileHeader.FileLength);
             Result.Add('  SW corner: x=' + RealToString(glMainFileHeader.BoundBox.XMin,-18,-6) + '    y='  + RealToString(glMainFileHeader.BoundBox.YMin,-18,-6));
             Result.Add('  NE corner: x=' + RealToString(glMainFileHeader.BoundBox.XMax,-18,-6) + '    y='  + RealToString(glMainFileHeader.BoundBox.YMax,-18,-6));
             if glMainFileHeader.ShapeType in [11,13,15,23,25] then begin
                Result.Add('  Z limits, low=' + RealToString(glMainFileHeader.BoundBoxZMin,-18,-6) + '    high='  + RealToString(glMainFileHeader.BoundBoxZMax,-18,-6));
             end;
             Result.Add(' Shape Type: ' + IntToStr(glMainFileHeader.ShapeType));
             Result.Add(' FileLen: ' +  IntToStr(glMainFileHeader.FileLength));
             Result.Add(' FileCode: ' +  IntToStr(glMainFileHeader.FileCode));
             Result.Add(' Version: ' +  IntToStr(glMainFileHeader.Version));
             MenuStr := '';
             for i := 1 to 5 do MenuStr := MenuStr + '  ' + IntToStr(glMainFileHeader.Unused[i]);
             Result.Add(' Unused: ' + MenuStr);
         end;

begin
   if not FileExists(FName) then exit;
   FSplit(FName,Dir,bName,Ext);

   Result := tStringList.Create;
   Result.Add('Shape file ' + Dir + bName);

   fName := ChangeFileExt(fName,'.shx');
   if FileExists(fName) then begin
       InsureFileIsNotReadOnly(fName);
       assignFile(glIndexFile,fName);
       reset(glIndexFile,1);
       BlockRead(glIndexFile,glMainFileHeader,100);
       OutputHeader('Header in shx file');
       with glMainFileHeader do begin
          NumRecs := (FileLength - 50) div 4;
          Result.Add('NumRecs: ' + IntToStr(NumRecs));
          Result.add('');
       end;
   end;

   fName := ChangeFileExt(fName,'.shp');
   if FileExists(fName) then begin
       InsureFileIsNotReadOnly(fName);
       assignFile(glShapeFile,fName);
       reset(glShapeFile,1);
       BlockRead(glShapeFile,glMainFileHeader,100);
       OutputHeader('Header in shp file');
       Result.add('');
   end;

   fName := ChangeFileExt(fName,'.prj');
   if FileExists(fName) then begin
       PrjFile := tStringList.Create;
       PrjFile.LoadFromFile(fName);
       Result.Add('PRJ projection file');
       for i := 0 to pred(PrjFile.Count) do
          Result.Add(PrjFile.Strings[i]);
       Result.add('');
       PrjFile.Free;
   end;

   fName := ChangeFileExt(fName,'.dbf');
   {$IfDef VCL}
      if FileExists(fName) then begin
          CheckDBaseIndexes(fName);
          Result.Add('dBase File fields:');
          Table := tMyData.Create(fName);
          TableStruct := Table.GetTableStructure;
          for i := 0 to pred(TableStruct.Count) do Result.Add(TableStruct[i]);
          TableStruct.Free;
          Result.add('Records in DBF file: ' + IntToStr(Table.RecordCount));
          Table.Destroy;
      end
      else Result.add('DBF file missing');
      Result.add('');
   {$EndIf}

   if FileExists(ChangeFileExt(fName,'.shx')) then begin
       FullSummary := (NumRecs < 1000) or AnswerIsYes('Summarize all ' + IntToStr(NumRecs) + ' records');
       with glMainFileHeader do begin
          Result.add('');
          Result.add('');
          if PointShapeFile(glMainFileHeader.ShapeType) then begin
             if (glMainFileHeader.ShapeType = 11) then TStr := '            z  ' else TStr := '';
             Result.add('        Offset    Length Rec No       Content           x                y    ' + TStr);
             if glMainFileHeader.ShapeType = 11 then TStr := '---------------' else TStr := '';
             Result.add('------------------------------------------------------------------------------' + TStr);
          end
          else begin
             Result.add('   Offset  Content Length   | Rec No   Content   Parts   Points   ');
             Result.add('---------------------------------------------------------------------');
          end;

          repeat
             BlockRead(glIndexFile,IndexRecords[1],800,NumRead);
             for i := 1 to (NumRead div 8) do with IndexRecords[i] do begin
                Int4Swap(Offset);
                Int4Swap(ContentLength);
                seek(glShapeFile,(2*Offset));
                if not EOF(glShapeFile) then begin
                   MenuStr := IntegerToString(Offset,12) + IntegerToString(ContentLength,12);
                   if PointShapeFile(glMainFileHeader.ShapeType) then begin
                      if (ShapeType = 1) then begin
                         BlockRead(glShapeFile,PointsHeader,SizeOf(sfPointsWithHeader));
                         Int4Swap(PointsHeader.RecordNumber);
                         Int4Swap(PointsHeader.ContentLength);
                         Result.Add(MenuStr + '|' + IntegerToString(PointsHeader.RecordNumber,6) + IntegerToString(PointsHeader.ContentLength,12) + RealToString(PointsHeader.y,18,6) + RealToString(PointsHeader.x,18,6));
                      end
                      else begin
                         BlockRead(glShapeFile,PointZsHeader,SizeOf(sfPointsZWithHeader));
                         Int4Swap(PointZsHeader.RecordNumber);
                         Int4Swap(PointZsHeader.ContentLength);
                         Result.Add(MenuStr + '|' + IntegerToString(PointZsHeader.RecordNumber,6) + IntegerToString(PointZsHeader.ContentLength,12) +
                                    RealToString(PointZsHeader.y,18,6) + RealToString(PointZsHeader.x,18,6) +  RealToString(PointZsHeader.z,18,6));
                      end;
                   end
                   else begin
                      BlockRead(glShapeFile,PolyLineHeader,SizeOf(PolyLineHeader));
                      Int4Swap(PolyLineHeader.RecordNumber);
                      Int4Swap(PolyLineHeader.ContentLength);
                      Result.Add(MenuStr + '|' + IntegerToString(PolyLineHeader.RecordNumber,6) + IntegerToString(PolyLineHeader.ContentLength,12) + IntegerToString(PolyLineHeader.NumParts,8) + IntegerToString(PolyLineHeader.NumPoints,8));
                   end;
                end;
             end;
          until EOF(glIndexFile) or (not FullSummary);
          closeFile(glIndexFile);
       end;
   end;
end;


{ tShapeFile }

function tShapeFile.KeyInfo : string;
begin
   Result := 'Shape type: ' + IntToStr(MainFileHeader.ShapeType) + '  Records: ' +  IntToStr((MainFileHeader.FileLength - 50) div 4);
end;


function tShapeFile.DistanceToLineOrPerimeter(PrimaryMapDatum : tMapProjection; RecNum : int32;  Lat,Long : float64; var LatOnLine,LongOnLine : float64) : float64;
var
   dx1p,dx21,dy1p,dy21,frac,lambda,xsep,ysep,Seg,LineX,LineY,
   xp,yp,x1,y1,x2,y2: float64;
   i,j,EndPt : int32;
begin
   Result := -99;
   if LineOrAreaShapeFile(MainFileHeader.ShapeType) then begin
       if GetLineCoords(RecNum) then begin
          PrimaryMapDatum.DefineDatumFromUTMZone(PrimaryMapDatum.h_DatumCode,GetUTMZone(CurrentLineCoords^[1].Long),HemiFromLat(CurrentLineCoords^[1].Lat),'tShapeFile.DistanceToLineOrPerimeter');
          PrimaryMapDatum.ForwardProjectDegrees(Lat,Long,xp,yp);
          Result := 99e39;
          for j := 1 to CurrentPolyLineHeader.NumParts do begin
             if (j = CurrentPolyLineHeader.NumParts) then EndPt := pred(CurrentPolyLineHeader.NumPoints)
             else EndPt := pred(CurrentLinePartSize[succ(j)]);
             for i := CurrentLinePartSize[j] to EndPt do begin
                PrimaryMapDatum.ForwardProjectDegrees(CurrentLineCoords^[i].Lat,CurrentLineCoords^[i].Long,x1,y1);
                if (i - CurrentLinePartSize[j] > 0) then begin
                   dx1p := x1 - xp;
                   dx21 := x2 - x1;
                   dy1p := y1 - yp;
                   dy21 := y2 - y1;
                   frac := dx21*dx21 + dy21*dy21;
                   //Compute distance along line that normal intersects.
                   lambda := -(dx1p*dx21 + dy1p*dy21) / frac;
                   //Accept if along the line segment, else choose the correct end point.
                   if (lambda < 0) then lambda := 0
                   else if (lambda > 1) then lambda := 1;
                   //Compute x and y separations between point on the line closest to (xp,yp) and (xp,yp).
                   xsep := dx1p + lambda * dx21;
                   ysep := dy1p + lambda * dy21;
                   Seg := sqrt(xsep*xsep + ysep*ysep);
                   if (Seg < Result) then begin
                      Result := Seg;
                      LineX := x1 + lambda * dx21;
                      LineY := y1 + lambda * dy21;
                   end;
                end;
                X2 := x1;
                Y2 := Y1;
             end;
          end;
          PrimaryMapDatum.UTMtoLatLongDegree(LineX,LineY,LatOnLine,LongOnLine);
       end;
   end;
end;


function tShapeFile.AreaAndCentroid(PrimaryMapDatum : tMapProjection; RecNum : int32; var LatCent,LongCent : float64) : float64;
var
   asum,xsum,ysum,term,XCent,YCent : extended;
   x,y,lastx,lasty : float64;
   i,j,EndPt: int32;
begin
   if AreaShapeFile(MainFileHeader.ShapeType) then begin
      if GetLineCoords(RecNum) then begin
         PrimaryMapDatum.DefineDatumFromUTMZone(PrimaryMapDatum.h_DatumCode,GetUTMZone(CurrentLineCoords^[1].Long),HemiFromLat(CurrentLineCoords^[1].Lat),'tShapeFile.AreaAndCentroid');
         if (CurrentPolyLineHeader.NumParts > 1) then begin
            //MessageToContinue('Donuts not yet supported');
         end
         else begin
            xsum := 0;
            ysum := 0;
            aSum := 0;
            j := 1;
            if (j = CurrentPolyLineHeader.NumParts) then EndPt := pred(CurrentPolyLineHeader.NumPoints)
            else EndPt := pred(CurrentLinePartSize[succ(j)]);
            for i := CurrentLinePartSize[j] to EndPt do begin
               PrimaryMapDatum.LatLongDegreetoUTM(CurrentLineCoords^[i].Lat,CurrentLineCoords^[i].Long,x,y);
               if (i-CurrentLinePartSize[j] > 0) then begin
                  Term := Lastx * y - x * Lasty;
                  aSum := aSum + Term;
                  xSum := xsum + (x + Lastx) * Term;
                  ySum := ysum + (y + Lasty) * Term;
               end;
               LastX := x;
               LastY := Y;
            end;
            PrimaryMapDatum.LatLongDegreetoUTM(CurrentLineCoords^[CurrentLinePartSize[j]].Lat,CurrentLineCoords^[CurrentLinePartSize[j]].Long,x,y);
            if (abs(LastX - x) > 0.001) or (abs(Lasty - y) > 0.001) then begin
               Term := Lastx * y - x * Lasty;
               aSum := aSum + Term;
               xSum := xsum + (x + Lastx) * Term;
               ySum := ysum + (y + Lasty) * Term;
            end;
         end;
      end;
   end;
   Result := abs(0.5 * aSum);   //negative means clockwise, positive CCW
   if (abs(aSum) > 0.00001) then begin
      XCent := xsum / (3 * aSum);
      YCent := ysum / (3 * aSum);
      PrimaryMapDatum.UTMtoLatLongDegree(xcent,ycent,LatCent,LongCent);
      if (LatCent > CurrentPolyLineHeader.BoundBox.YMax) or (LatCent < CurrentPolyLineHeader.BoundBox.YMin) then begin
         //ugly, for NGA MGRS6x8 boxes just south of equator
         LatCent := 0.5 * (CurrentPolyLineHeader.BoundBox.YMax + CurrentPolyLineHeader.BoundBox.YMin);
         LongCent := 0.5 * (CurrentPolyLineHeader.BoundBox.XMax + CurrentPolyLineHeader.BoundBox.XMin);
      end;
   end
   else begin
      LatCent := 0;
      LongCent := 0;
   end;
end;


procedure tShapeFile.LineCenter(RecNum : int32; var XCent,YCent : float64);
var
   First : int32;
begin
   if LineShapeFile(MainFileHeader.ShapeType) then begin
      GetLineCoords(RecNum,False);
      First := CurrentPolyLineHeader.NumPoints div 2;
      if Odd(CurrentPolyLineHeader.NumPoints)then begin
         xCent := CurrentLineCoords^[First].Long;
         yCent := CurrentLineCoords^[First].Lat;
      end
      else begin
         xCent := 0.5* (CurrentLineCoords^[First].Long + CurrentLineCoords^[pred(First)].Long);
         yCent := 0.5* (CurrentLineCoords^[First].Lat + CurrentLineCoords^[pred(First)].Lat);
      end;
   end;
end;


function tShapeFile.GetLineCoordsAndZsFromDEM(DEM,RecNum : int32) : boolean;
var
   i : int32;
   z : float32;
begin
   result := GetLineCoords(RecNum);
   if Result then begin
      if (CurrentLineZs = Nil) then new(CurrentLineZs);
      for i := 0 to pred(CurrentPolyLineHeader.NumPoints) do begin
         DEMGlb[DEM].GetElevFromLatLongDegree(CurrentLineCoords^[i].Lat,CurrentLineCoords^[i].Long,z);
         CurrentLineZs^[i] := z;
      end;
   end;
end;

function tShapeFile.OldGetLineCoordsAndZsFromDEM(DEM,RecNum : int32; var NumPts,NParts : int32; var Coords : tdCoords; var PartSize : tPartSize; var zs : array of float64) : boolean;
var
   i : int32;
   z : float32;
begin
   result := OldGetLineCoordsWithZs(RecNum,NumPts,NParts,Coords,PartSize,false,zs);
   if Result then begin
      for i := 0 to pred(NumPts) do begin
         DEMGlb[DEM].GetElevFromLatLongDegree(Coords[i].Lat,Coords[i].Long,z);
         zs[i] := z;
      end;
   end;
end;


function tShapeFile.OldGetLineCoords(RecNum : int32; var NumPts,NParts : int32; var Coords : tdCoords; var PartSize : tPartSize) : boolean;
begin
   result := OldGetLineCoords(RecNum,NumPts,NParts,Coords,PartSize);
end;


function tShapeFile.GetLineCoords(RecNum : integer; GetZS : boolean = false) : boolean;
var
   j,EndPt,i : int32;
   z : float32;
begin
   if LineOrAreaShapeFile(MainFileHeader.ShapeType) then begin
      Result := false;
      if false and (CurrentLineRecNum = RecNum) and ((not GetZS) or CurrentLineHaveZs) then begin
         {$IfDef RecordGetLineCoords} writeLineToDebugFile('old rec=' + IntToStr(RecNum) + ' CurrentLinePartSize[1]=' + IntToStr(CurrentLinePartSize[1])); {$EndIf}
         Result := true;
      end
      else begin
         CurrentLineHaveZs := false;
         CurrentLineRecNum := RecNum;
         if ReadPolyLineHeader(RecNum,CurrentPolyLineHeader) then begin
           if (CurrentPolyLineHeader.NumParts <= sfMaxParts) then begin
               FillChar(CurrentLinePartSize,SizeOf(CurrentLinePartSize),0);
               {$IfDef UseMemoryStream}
                  if (ShapeMemoryStream = Nil) then BlockRead(ShapeFile,CurrentLinePartSize[1],4*CurrentPolyLineHeader.NumParts)
                  else ShapeMemoryStream.Read(CurrentLinePartSize[1],4*CurrentPolyLineHeader.NumParts);
               {$Else}
                  BlockRead(ShapeFile,CurrentLinePartSize[1],4*CurrentPolyLineHeader.NumParts);
               {$EndIf}

               for j := 1 to CurrentPolyLineHeader.Numparts do begin
                  if (j = CurrentPolyLineHeader.NumParts) then EndPt := CurrentPolyLineHeader.NumPoints
                  else EndPt := CurrentLinePartSize[succ(j)];
                  CurrentLinePointsPerPart[j] := EndPt - CurrentLinePartSize[j];
               end;

               {$IfDef RecordGetLineCoords} WriteLineToDebugFile('new rec=' + IntToStr(RecNum) + ' CurrentLinePartSize[1]=' + IntToStr(CurrentLinePartSize[1])); {$EndIf}
               if (CurrentPolyLineHeader.NumPoints > sfMaxPoints) then MessageToContinue('Record too large ' + IntToStr(CurrentPolyLineHeader.NumPoints ));

               {$IfDef UseMemoryStream}
                  if (ShapeMemoryStream = Nil) then begin
                     BlockRead(ShapeFile,CurrentLineCoords^,CurrentPolyLineHeader.NumPoints*SizeOf(sfPoints));
                  end
                  else begin
                     ShapeMemoryStream.Read(CurrentLineCoords^,CurrentPolyLineHeader.NumPoints*SizeOf(sfPoints));
                  end;
               {$Else}
                  BlockRead(ShapeFile,CurrentLineCoords^,CurrentPolyLineHeader.NumPoints*SizeOf(sfPoints));
               {$EndIf}
               Result := true;
           end;
         end;

         if Result and GetZS then begin
            if (CurrentLineZs = Nil) then New(CurrentLineZs);
            if ShapeFile3D(MainFileHeader.ShapeType) then begin
               {$IfDef UseMemoryStream}
               {$Else}
                  BlockRead(ShapeFile,CurrentLineZrange[1],16);
                  BlockRead(ShapeFile,CurrentLineZs^[0],CurrentPolyLineHeader.NumPoints*8);
               {$EndIf}
            end
            else begin
               if (ShapefileDEM <> 0) then begin
                  for i := 0 to pred(CurrentPolyLineHeader.NumPoints) do begin
                     DEMGlb[ShapefileDEM].GetElevFromLatLongDegree(CurrentLineCoords^[i].Lat,CurrentLineCoords^[i].Long,z);
                     CurrentLineZs^[i] := z;
                  end;
               end;
            end;
            if (abs(CurrentLineZRange[2] - CurrentLineZRange[1]) < 0.0001) then begin
               CurrentLineZRange[1] := CurrentLineZs^[0];
               CurrentLineZRange[2] := CurrentLineZs^[0];
               for i := 1 to pred(CurrentPolyLineHeader.NumPoints) do Petmath.CompareValueToExtremes(CurrentLineZs^[i],CurrentLineZRange[1],CurrentLineZRange[2]);
            end;
            CurrentLineHaveZs  := true;
         end {if};
      end;
   end {if};
   {$IfDef RecordGetLineCoords} WriteLineToDebugFile('exit GetLineCoords rec=' + IntToStr(RecNum) + ' CurrentLinePartSize[1]=' + IntToStr(CurrentLinePartSize[1])); {$EndIf}
end;


function tShapeFile.OldGetLineCoordsWithZs(RecNum : int32; var NumPts,NParts : int32; var Coords : tdCoords; var PartSize : tPartSize; GetZs : boolean; var zs : array of float64) : boolean;
var
   PolyLineHeader  : sfPolyLineHeader;
   NumRead,NumToRead : int32;
   zrange : array[1..2] of float64;
   {$IfDef RecordShapeFileLine}
   i : integer;
   {$EndIf}
begin
   Result := false;
   if LineOrAreaShapeFile(MainFileHeader.ShapeType) then begin
      FillChar(PartSize,SizeOf(PartSize),0);
      if ReadPolyLineHeader(RecNum,PolyLineHeader) then begin
         NParts := PolyLineHeader.NumParts;
         NumPts := PolyLineHeader.NumPoints;
         if (PolyLineHeader.NumParts <= sfMaxParts) then begin
             {$IfDef UseMemoryStream}
                if (ShapeMemoryStream = Nil) then BlockRead(ShapeFile,PartSize[1],4*PolyLineHeader.NumParts)
                else ShapeMemoryStream.Read(PartSize[1],4*PolyLineHeader.NumParts);
             {$Else}
                BlockRead(ShapeFile,PartSize[1],4*PolyLineHeader.NumParts);
             {$EndIf}
             NumToRead := PolyLineHeader.NumPoints;
             if (NumToRead > sfMaxPoints) then NumToRead := sfMaxPoints;
              {$IfDef UseMemoryStream}
              if (ShapeMemoryStream = Nil) then begin
                 BlockRead(ShapeFile,Coords,NumToRead*SizeOf(sfPoints),NumRead);
                 NumRead := (NumRead div SizeOf(sfPoints));
              end
              else begin
                 ShapeMemoryStream.Read(Coords,NumToRead*SizeOf(sfPoints));
                 NumRead := NumToRead;
              end;
              {$Else}
              BlockRead(ShapeFile,Coords,NumToRead*SizeOf(sfPoints),NumRead);
              NumRead := (NumRead div SizeOf(sfPoints));
              {$EndIf}

             if GetZs and ShapeFile3D(MainFileHeader.ShapeType) then begin
                 {$IfDef UseMemoryStream}
                 if (ShapeMemoryStream = Nil) then begin
                    BlockRead(ShapeFile,zrange[1],32);
                    BlockRead(ShapeFile,zs[0],NumToRead*16);
                 end
                 {$Else}
                 BlockRead(ShapeFile,zrange[1],32);
                 BlockRead(ShapeFile,zs[0],NumToRead*16);
                 {$EndIf}
             end;
          end;
     end {with};
     Result := true;
  end;

   {$IfDef RecordShapeFileLine}
      WriteLineToDebugFile('Read line rec: ' + IntToStr(NumPts) + ' points & ' + IntToStr(NParts) + ' parts');
      for i := 0 to 5 do if (i < NumPts) then WriteLineToDebugFile(LatLongDegreeToString(Coords[i].Lat,Coords[i].Long));
      if (NumPts > 5) then begin
         WriteLineToDebugFile('......');
         for i := (NumPts - 5) to pred(NumPts) do if (i > 5) then
            WriteLineToDebugFile(LatLongDegreeToString(Coords[i].Lat,Coords[i].Long));
      end;
   {$EndIf}
end;


function tShapeFile.LineAreaBoundBox(RecNum : int32) :  sfBoundBox;
var
   PolyLineHeader : sfPolyLineHeader;
begin
   ReadPolyLineHeader(RecNum,PolyLineHeader);
   Result := PolyLineHeader.BoundBox;
end;



procedure tShapeFile.ReadSHXindex;
var
   i : integer;
begin
   {$IfDef UseMemoryStream}
   {$Else}
      if (SHXindexArray = Nil) and (NumRecs <= MaxSHXinMemory) then begin
         GetMem(SHXindexArray,8 * NumRecs);
         FileMode := 0;
         seek(IndexFile,SizeOf(sfMainFileHeader));
         if EOF(IndexFile) then exit;
         BlockRead(IndexFile,SHXindexArray^,8 * NumRecs);
         for I := 1 to NumRecs do begin
            Int4Swap(SHXindexArray^[i].Offset);
            Int4Swap(SHXindexArray^[i].ContentLength);
            SHXindexArray^[i].Offset := 2 * SHXindexArray^[i].Offset;
         end;
      end;
  {$EndIf}
end;


function tShapeFile.NextPolyLineHeader(var PolyLineHeader : sfPolyLineHeader) : boolean;
var
   IndexRecord  : sfIndexRecord;
begin
   {$IfDef UseMemoryStream}
      if (ShapeMemoryStream = Nil) then begin
         BlockRead(ShapeFile,PolyLineHeader,SizeOf(PolyLineHeader));
      end
      else begin
         ShapeMemoryStream.Read(PolyLineHeader,SizeOf(PolyLineHeader));
      end;
   {$Else}
      BlockRead(ShapeFile,PolyLineHeader,SizeOf(PolyLineHeader));
   {$EndIf}
   Result := LineOrAreaShapeFile(PolyLineHeader.ShapeType);
   {$IfDef RecordShapeFileLine} WriteLineToDebugFile('Polyline header: ' + IntToStr(RecNum) + '  Offset: ' + IntToStr(IndexRecord.Offset) + '   Content len: ' + IntToStr(IndexRecord.ContentLength)); {$EndIf}
end;



function tShapeFile.ReadPolyLineHeader(RecNum : int32; var PolyLineHeader : sfPolyLineHeader) : boolean;
var
   Offset : int32;
   IndexRecord  : sfIndexRecord;
begin
   result := false;
   if SHXindexArray <> Nil then begin
      Offset := SHXindexArray^[RecNum].Offset;
      //   Int4Swap(SHXindexArray^[i].ContentLength);
   end
   else begin
      Offset := 8 * pred(RecNum) + SizeOf(sfMainFileHeader);
      {$IfDef UseMemoryStream}
         if (IndexMemoryStream = Nil) then begin
            FileMode := 0;
            seek(IndexFile,Offset);
            if EOF(IndexFile) then exit;
            BlockRead(IndexFile,IndexRecord,8);
         end
         else begin
            if (Offset > IndexMemoryStream.Size) then exit;
            IndexMemoryStream.Seek(Offset, soFromBeginning);
            IndexMemoryStream.Read(IndexRecord,8);
         end;
      {$Else}
          seek(IndexFile,Offset);
          if EOF(IndexFile) then exit;
          BlockRead(IndexFile,IndexRecord,8);
      {$EndIf}

      Int4Swap(IndexRecord.Offset);
      Int4Swap(IndexRecord.ContentLength);
      Offset := 2 * IndexRecord.Offset;
   end;

   {$IfDef RecordMemoryStream} Panel1Message('Offset: ' + IntToStr(IndexRecord.Offset)); {$EndIf}


   {$IfDef UseMemoryStream}
      if (ShapeMemoryStream = Nil) then begin
         if ShapeFileRandomAccess then seek(ShapeFile,Offset);
         BlockRead(ShapeFile,PolyLineHeader,SizeOf(PolyLineHeader));
      end
      else begin
         ShapeMemoryStream.Seek(Offset, soFromBeginning);
         ShapeMemoryStream.Read(PolyLineHeader,SizeOf(PolyLineHeader));
      end;
   {$Else}
      if ShapeFileRandomAccess then seek(ShapeFile,Offset);
      BlockRead(ShapeFile,PolyLineHeader,SizeOf(PolyLineHeader));
   {$EndIf}

   Result := LineOrAreaShapeFile(PolyLineHeader.ShapeType);

   {$IfDef RecordShapeFileLine} WriteLineToDebugFile('Polyline header: ' + IntToStr(RecNum) + '  Offset: ' + IntToStr(IndexRecord.Offset) + '   Content len: ' + IntToStr(IndexRecord.ContentLength)); {$EndIf}
end;


procedure tShapeFile.WriteEditCurrentRecordPolyLine(RecNum : int32);
var
   Offset,ContentLength : int32;
   IndexRecord  : sfIndexRecord;
begin
   Offset := 8*pred(RecNum) + SizeOf(sfMainFileHeader);
   seek(IndexFile,Offset);
   if EOF(IndexFile) then exit;
   BlockRead(IndexFile,IndexRecord,8);

   Int4Swap(IndexRecord.Offset);
   Int4Swap(IndexRecord.ContentLength);

   Offset := 2*IndexRecord.Offset;
   ContentLength := 4 + IndexRecord.ContentLength;

   seek(ShapeFile,Offset);

   Int4Swap(CurrentPolyLineHeader.RecordNumber);
   Int4Swap(CurrentPolyLineHeader.ContentLength);

   BlockWrite(ShapeFile,CurrentPolyLineHeader,SizeOf(CurrentPolyLineHeader));
   BlockWrite(ShapeFile,CurrentLinePartSize[1],4*CurrentPolyLineHeader.NumParts);
   BlockWrite(ShapeFile,CurrentLineCoords^,CurrentPolyLineHeader.NumPoints*SizeOf(sfPoints));

   if ShapeFile3D(MainFileHeader.ShapeType) then begin
      BlockWrite(ShapeFile,CurrentLineZrange[1],16);
      BlockWrite(ShapeFile,CurrentLineZs^[0],CurrentPolyLineHeader.NumPoints*8);
   end;

   Int4Swap(CurrentPolyLineHeader.RecordNumber);
   Int4Swap(CurrentPolyLineHeader.ContentLength);
end;



procedure tShapeFile.DropInBucketIntoDEM(LastDEM : integer; MaxDEM : integer = 0; MinDEM : integer = 0; MeanDEM : integer = 0; DensityDEM : integer = 0);
var
   I,RecsRead,OnBlock,BlocksNeeded : int32;
   NZDEM,xg,yg : integer;
   NewZ,OldZ : float32;
   ShapePoints : ^tLotsOfPoints3D;
begin
   if (LastDEM <> 0) then NZDEM := LastDEM else NZDEM := MaxDEM;
   OnBlock := 0;
   New(ShapePoints);
   reset(ShapeFile,1);
   BlockRead(ShapeFile,MainFileHeader,SizeOf(MainFileHeader));
   Int4Swap(MainFileHeader.FileLength);
   BlocksNeeded := succ( ( (MainFileHeader.FileLength - 50) div Sizeof(sfPointsZWithHeader)) div sfMaxPoints);
   repeat
      BlockRead(ShapeFile,ShapePoints^[1],sfMaxPoints*Sizeof(sfPointsZWithHeader),RecsRead);
      RecsRead := RecsRead div SizeOf(sfPointsZWithHeader);
      inc(OnBlock);
      for i := 1 to RecsRead do begin
         DEMGlb[NZDEM].LatLongDegreeToDEMGridInteger(ShapePoints^[i].y,ShapePoints^[i].x,xg,yg);
         if DEMGlb[NZDEM].GridInDataSet(xg,yg) then begin
            NewZ := ShapePoints^[i].z;
            if (LastDEM <> 0) then DEMGlb[LastDEM].SetGridElevation(xg,yg,NewZ);
            if (MaxDEM <> 0) then begin
               DEMGlb[MaxDEM].GetElevMeters(xg,yg,OldZ);
               if NewZ > OldZ then DEMGlb[MaxDEM].SetGridElevation(xg,yg,NewZ);
            end;
            if (MinDEM <> 0) then begin
               DEMGlb[MinDEM].GetElevMeters(xg,yg,OldZ);
               if NewZ < OldZ then DEMGlb[MinDEM].SetGridElevation(xg,yg,NewZ);
            end;
            if (MeanDEM <> 0) then begin
               DEMGlb[MeanDEM].GetElevMeters(xg,yg,OldZ);
               DEMGlb[MeanDEM].SetGridElevation(xg,yg,NewZ + OldZ);
            end;
            if (DensityDEM <> 0) then DEMGlb[DensityDEM].IncrementGridValue(xg,yg);
         end;
      end;
   until System.eof(ShapeFile);
   Dispose(ShapePoints);
end;


function tShapeFile.LineHeading(RecNum : integer) : float64;
var
   x1,y1,x2,y2,Dist : float64;
begin
   LineEndPoints(RecNum,x1,y1,x2,y2);
   VincentyCalculateDistanceBearing(y1,x1,y2,x2,Dist,Result);
end;


procedure tShapeFile.LineEndPoints(RecNum : int32; var x1,y1,x2,y2 : float64);
begin
   if LineShapeFile(MainFileHeader.ShapeType) then begin
      GetLineCoords(RecNum);
      x1 := CurrentLineCoords^[0].Long;
      y1 := CurrentLineCoords^[0].Lat;
      x2 := CurrentLineCoords^[pred(CurrentPolyLineHeader.NumPoints)].Long;
      y2 := CurrentLineCoords^[pred(CurrentPolyLineHeader.NumPoints)].Lat;
   end;
end;


{$IfDef UseMemoryStream}
procedure tShapeFile.OpenMemoryStreams(FName1,FName2 : PathStr);
begin
   if (ShapeMemoryStream = Nil) then begin
     ShapeMemoryStream := tMemoryStream.Create;
     ShapeMemoryStream.LoadFromFile(fName1);
   end;
   if (IndexMemoryStream = Nil) then begin
     IndexMemoryStream := tMemoryStream.Create;
     IndexMemoryStream.LoadFromFile(fName2);
   end;
end;
{$EndIf}


procedure tShapeFile.OpenSHPandSHX;
begin
  assignFile(ShapeFile,ChangeFileExt(ShapeFileName,'.shp'));
  reset(ShapeFile,1);
  assignFile(IndexFile,ChangeFileExt(ShapeFileName,'.shx'));
  reset(IndexFile,1);
end;


procedure tShapeFile.CloseSHPandSHX;
begin
  CloseFile(ShapeFile);
  CloseFile(IndexFile);
end;


constructor tShapeFile.Create(FileName: PathStr; var Success : boolean);
var
   fName1,fName2 : PathStr;


    procedure GetShapeFileHeader(var tfile : file; var MainFileHeader : sfMainFileHeader; var NumRecs : int32);
    begin
       BlockRead(tfile,MainFileHeader,SizeOf(MainFileHeader));
       with MainFileHeader do begin
          Int4Swap(FileCode);
          Int4Swap(FileLength);
          NumRecs := (FileLength - 50) div 4;
          if not (PointShapeFile(ShapeType) or LineShapeFile(ShapeType) or AreaShapeFile(ShapeType)) then begin
             MessageToContinue('Unsupported shape file type ' + IntToStr(ShapeType));
             exit;
          end;
       end;
    end;

begin
   {$IfDef RecordShapefile} WriteLineToDebugFile('tShapeFile.Create ' + FileName); {$EndIf}
   {$IfDef UseMemoryStream}
      ShapeMemoryStream := Nil;
      IndexMemoryStream := Nil;
   {$EndIf}
   DBFFileOnly := false;
   NoDBFFile := false;
   ShapefileRandomAccess := true;
   CurrentLineCoords := Nil;
   CurrentLineZs := Nil;
   SHXindexArray := Nil;
   CurrentLineRecNum := -999;
   CurrentCCWRecNum := -999;
   ShapefileDEM := 0;
   Symbol.Size := 2;
   Symbol.Color := claRed;
   Symbol.DrawingSymbol := Box;

   XPlaneMin := 0;
   XPlaneMax := 0;
   YPlaneMin := 0;
   YPlaneMax := 0;
   ZPlaneMin := 0;
   ZPlaneMax := 0;

   fName2 := ChangeFileExt(FileName,'.shx');
   fName1 := ChangeFileExt(FileName, '.shp');
   Success := true;
   ShapeFileName := FileName;
   if ItsAShapeFile(FName1,NoDBFFile) then begin
     {$IfDef RecordShapefile} writeLineToDebugFile('its a shapefile ' + ShapeFileName); {$EndIf}

     NoDBFFile := Not FileExists(ChangeFileExt(FileName, '.dbf'));

     InsureFileIsNotReadOnly(FName1);
     InsureFileIsNotReadOnly(FName2);

     {$IfDef RecordShapefile} WriteLineToDebugFile('read only status checked'); {$EndIf}

     {$IfDef UseMemoryStream}
        OpenMemoryStreams(fName1,fName2);
     {$Else}
        OpenSHPandSHX;
     {$EndIf}

     {$IfDef RecordShapefile} writeLineToDebugFile('off to get header'); {$EndIf}

     GetShapeFileHeader(IndexFile,MainFileHeader,NumRecs);
     ReadSHXindex;

     NotLatLongShapeFile := FileExists(ExtractFilePath(FileName) + 'xyz_files.txt') or (MainFileHeader.BoundBox.XMin < -720) or (MainFileHeader.BoundBox.XMax > 720) or
         (MainFileHeader.BoundBox.YMin < -180) or (MainFileHeader.BoundBox.YMax > 180);
     if LineOrAreaShapeFile(MainFileHeader.ShapeType) then begin
        new(CurrentLineCoords);
        if ShapeFile3D(MainFileHeader.ShapeType) then new(CurrentLineZs);
     end;
   end
   else if FileExtEquals(FileName,'.dbf') then DBFFileOnly := true
   else Success := false;
   BadPointCheck := (MainFileHeader.BoundBox.XMin < -179) and (MainFileHeader.BoundBox.XMax > 179) and AreaShapeFile(MainFileHeader.ShapeType);

   {$IfDef RecordShapefile} WriteLineToDebugFile('tShapeFile.Create out'); {$EndIf}
end;


destructor tShapeFile.Destroy;
begin
  if (not DBFFileOnly) then begin
     CloseFile(ShapeFile);
     closeFile(Indexfile);
  end;
  if (CurrentLineCoords <> Nil) then Dispose(CurrentLineCoords);
  if (CurrentLineZs <> Nil) then Dispose(CurrentLineZs);
  if (SHXindexArray <> Nil) then FreeMem(SHXindexArray,8 * NumRecs);;

  {$IfDef UseMemoryStream}
     if (IndexMemoryStream <> Nil) then IndexMemoryStream.Free;
     if (ShapeMemoryStream <> Nil) then ShapeMemoryStream.Free;
  {$EndIf}
  inherited;
end;


function tShapeFile.ReadPointRecord3D(RecNo : int32; var PointsHeader : sfPointsZWithHeader) : boolean;
var
   IndexRecord  : sfIndexRecord;
   NumRead : int32;
begin
   Result := false;
  {$IfDef UseMemoryStream}
   if (IndexMemoryStream = Nil) then begin
      seek(IndexFile,8*pred(RecNo) + SizeOf(sfMainFileHeader) );
      BlockRead(IndexFile,IndexRecord,8,NumRead);
   end
   else begin
      IndexMemoryStream.Seek(8*pred(RecNo) + SizeOf(sfMainFileHeader),soFromBeginning	);
      IndexMemoryStream.Read(IndexRecord,8);
   end;
   {$Else}
      seek(IndexFile,8*pred(RecNo) + SizeOf(sfMainFileHeader) );
      BlockRead(IndexFile,IndexRecord,8,NumRead);
   {$EndIf}

   with IndexRecord do begin
      Int4Swap(Offset);
      Int4Swap(ContentLength);
      {$IfDef RecordPointProblem} WriteLineToDebugFile('RecNo=' + IntToStr(RecNo) + '  Offset=' + IntToStr(Offset)); {$EndIf}
      {$IfDef UseMemoryStream}
      if (ShapeMemoryStream = Nil) then begin
         seek(ShapeFile,(2*Offset));
         if not eof(Shapefile) then begin
            BlockRead(ShapeFile,PointsHeader,SizeOf(sfPointsZWithHeader),NumRead);
            if NumRead = SizeOf(sfPointsZWithHeader) then Result := true;
         end;
      end
      else begin
         ShapeMemoryStream.Seek(2*Offset,soFromBeginning);
         ShapeMemoryStream.Read(PointsHeader,SizeOf(sfPointsZWithHeader));
         Result := true;
      end;
      {$Else}
         seek(ShapeFile,(2*Offset));
         if not eof(Shapefile) then begin
            BlockRead(ShapeFile,PointsHeader,SizeOf(sfPointsZWithHeader),NumRead);
            if NumRead = SizeOf(sfPointsZWithHeader) then Result := true;
         end;
      {$EndIf}
   end;
end;


function tShapeFile.ReadPointRecord(RecNo : int32; var PointsHeader : sfPointsWithHeader) : boolean;
var
   IndexRecord  : sfIndexRecord;
   NumRead : int32;
begin
   Result := false;

   {$IfDef UseMemoryStream}
   if (IndexMemoryStream = Nil) then begin
      seek(IndexFile,8*pred(RecNo) + SizeOf(sfMainFileHeader) );
      BlockRead(IndexFile,IndexRecord,8,NumRead);
   end
   else begin
      IndexMemoryStream.Seek(8*pred(RecNo) + SizeOf(sfMainFileHeader),soFromBeginning	);
      IndexMemoryStream.Read(IndexRecord,8);
   end;
   {$Else}
      seek(IndexFile,8*pred(RecNo) + SizeOf(sfMainFileHeader) );
      BlockRead(IndexFile,IndexRecord,8,NumRead);
   {$EndIf}

   with IndexRecord do begin
      Int4Swap(Offset);
      Int4Swap(ContentLength);
      {$IfDef RecordPointProblem} WriteLineToDebugFile('RecNo=' + IntToStr(RecNo) + '  Offset=' + IntToStr(Offset)); {$EndIf}
      {$IfDef UseMemoryStream}
      if (ShapeMemoryStream = Nil) then begin
         seek(ShapeFile,(2*Offset));
         if not eof(Shapefile) then begin
            BlockRead(ShapeFile,PointsHeader,SizeOf(sfPointsWithHeader),NumRead);
            if NumRead = SizeOf(sfPointsWithHeader) then Result := true;
         end;
      end
      else begin
         ShapeMemoryStream.Seek(2*Offset,soFromBeginning);
         ShapeMemoryStream.Read(PointsHeader,SizeOf(sfPointsWithHeader));
         Result := true;
      end;
      {$Else}
         seek(ShapeFile,(2*Offset));
         if not eof(Shapefile) then begin
            BlockRead(ShapeFile,PointsHeader,SizeOf(sfPointsWithHeader),NumRead);
            if NumRead = SizeOf(sfPointsWithHeader) then Result := true;
         end;
      {$EndIf}
   end;
end;



procedure tShapeFile.RepairBoundingBoxInSHPandSHXfiles;
var
   i,FileLength : int32;
   PolyLineHeader : sfPolyLineHeader;
begin
   if LineOrAreaShapeFile(MainFileHeader.ShapeType) then begin
      InitializeBoundingBox(MainFileHeader.BoundBox);
      for i  := 1 to NumRecs do begin
        ReadPolyLineHeader(i,PolyLineHeader);
        CheckMainFileBoundingBox(MainFileHeader,  PolyLineHeader.BoundBox);
      end;

      FileLength := NumRecs * 4 + 50;
      Int4Swap(FileLength);
      Seek(IndexFile,0);
      BlockWrite(IndexFile,MainFileHeader,SizeOf(MainFileHeader));
      FileLength := FileSize(ShapeFile) div 2;
      Int4Swap(FileLength);
      seek(ShapeFile,0);
      BlockWrite(ShapeFile,MainFileHeader,SizeOf(MainFileHeader));
   end;
end;


procedure tShapeFile.AddFields;
label
   BailOut;
var
   FName : PathStr;
   RecNum : int32;
   x1,y1,x2,y2 : float64;
   NewTable : boolean;
   PointsHeader3D : sfPointsZWithHeader;
   PointsHeader : sfPointsWithHeader;
   LatFieldName,LongFieldName : ShortString;
begin
   {$IfDef TimeShapefile} Stopwatch1 := TStopwatch.StartNew; {$EndIf}
   try
      MoveToStartShapefile;
      NewTable := (MyTable = Nil);
      if NewTable then begin
         fName := ChangeFileExt(ShapeFileName,'.dbf');
         MyTable := tMyData.Create(fName);
      end
      else MyTable.ApplyFilter('');

      LatFieldName := 'LAT';
      LongFieldName := 'LONG';

      if (WhatAdd = afBoundingBox) then begin
         if not MyTable.BoundingBoxPresent then MyTable.AddBoundingBox;
      end
      else if (WhatAdd = afXYZ) then begin
         MyTable.InsureFieldPresentAndAdded(ftFloat,'X',12,6);
         MyTable.InsureFieldPresentAndAdded(ftFloat,'Y',12,6);
         MyTable.InsureFieldPresentAndAdded(ftFloat,'Z',12,6);
      end
      else if (WhatAdd = afLatLong) then begin
         MyTable.InsureFieldPresentAndAdded(ftFloat,'LAT',11,7);
         MyTable.InsureFieldPresentAndAdded(ftFloat,'LONG',11,7);
      end
      else if (WhatAdd = afLineMerge) then begin
         MyTable.InsureFieldPresentAndAdded(ftInteger,'X1',10,0);
         MyTable.InsureFieldPresentAndAdded(ftInteger,'Y1',9,0);
         MyTable.InsureFieldPresentAndAdded(ftInteger,'X2',10,0);
         MyTable.InsureFieldPresentAndAdded(ftInteger,'Y2',9,0);
         MyTable.InsureFieldPresentAndAdded(ftString,'DONE',1,0);
      end;

      MyTable.First;
      RecNum := 0;
      {$IfDef VCL} StartProgressAbortOption('Add fields ' + ExtractFileName(MyTable.FullTableName)); {$EndIf}
      while not MyTable.EOF do begin
         MyTable.Edit;
         inc(RecNum);
         {$IfDef VCL} if (RecNum mod 250 = 0) then UpdateProgressBar(RecNum/MyTable.RecordCount); {$EndIf}

         if (WhatAdd = afXYZ) then begin
            if ReadPointRecord3D(RecNum,PointsHeader3d) then begin
               MyTable.SetFieldByNameAsFloat('X',PointsHeader3D.x);
               MyTable.SetFieldByNameAsFloat('Y',PointsHeader3D.y);
               MyTable.SetFieldByNameAsFloat('Z',PointsHeader3D.z);
            end;
         end;
         if (WhatAdd = afLatLong) then begin
            if ReadPointRecord(RecNum,PointsHeader) then begin
               MyTable.SetFieldByNameAsFloat('LONG',PointsHeader.x);
               MyTable.SetFieldByNameAsFloat('LAT',PointsHeader.y);
            end;
         end;
         if (WhatAdd = afLineMerge) then begin
            LineEndPoints(RecNum,x1,y1,x2,y2);
            MyTable.SetFieldByNameAsInteger('X1',round(100000 * x1));
            MyTable.SetFieldByNameAsInteger('Y1',round(100000 * y1));
            MyTable.SetFieldByNameAsInteger('X2',round(100000 * x2));
            MyTable.SetFieldByNameAsInteger('Y2',round(100000 * y2));
            MyTable.SetFieldByNameAsString('DONE','N');
         end;
         if (WhatAdd = afBoundingBox) then begin
            if ReadPolyLineHeader(RecNum,LastRecPolyLineHeader) then begin
               PutBoundBoxInTable(MyTable,LastRecPolyLineHeader.BoundBox);
            end;
         end;
         MyTable.Next;
         if WantOut then break;
      end;
      if NewTable then MyTable.Destroy;
   finally
      EndProgress;
   end;
   {$IfDef TimeShapefile} WriteLineToDebugFile('Shapefile AddFields took ' + RealToString(Stopwatch1.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
end;


procedure tShapeFile.PlotAllRecords(MapDraw : tMapDraw; var Bitmap : tMyBitmap; OSMdata : boolean = false);
var
   i,nDrawn,j,RecsRead,x,y,OnBlock,BlocksNeeded,Track : int32;
   ShapePoints3D : ^tLotsOfPoints3D;
   ShapePoints : ^tLotsOfPoints;
begin
   {$If Defined(TimeShapefile) or Defined(TimeDBPlot)}  WriteLineToDebugFile('tShapeFile.PlotAllRecords in ' + ExtractFileName(ShapeFileName)); {$EndIf}
   {$If Defined(TimeShapefile) or Defined(TimeDBPlot)} Stopwatch1 := TStopwatch.StartNew; {$EndIf}

   {$IfDef VCL} if ShowSatProgress and (not ThreadsWorking) then StartProgressAbortOption('Plot ' + ExtractFileName(ShapeFileName)); {$EndIf}

   if PointShapeFile(MainFileHeader.ShapeType) then begin
      {$IfDef TimeDBPlot} WriteLineToDebugFile('Point DB'); {$EndIf}
       OnBlock := 0;
       reset(ShapeFile,1);
       BlockRead(ShapeFile,MainFileHeader,SizeOf(MainFileHeader));
       Int4Swap(MainFileHeader.FileLength);

       if (MainFileHeader.ShapeType = 1) then begin
         New(ShapePoints);
         BlocksNeeded := succ( ( (MainFileHeader.FileLength - 50) div Sizeof(sfPointsWithHeader)) div sfMaxPoints);
         repeat
            BlockRead(ShapeFile,ShapePoints^[1],sfMaxPoints*Sizeof(sfPointsWithHeader),RecsRead);
            RecsRead := RecsRead div SizeOf(sfPointsWithHeader);
            {$IfDef VCL} if ShowSatProgress and (not ThreadsWorking) then UpdateProgressBar(OnBlock/BlocksNeeded); {$EndIf}
            inc(OnBlock);
            for i := 1 to RecsRead do begin
               MapDraw.LatLongDegreeToScreen(ShapePoints^[i].y,ShapePoints^[i].x,x,y);
               if MapDraw.OnScreen(x,y) then begin
                  {$IfDef VCL}
                     ScreenSymbol(Bitmap.Canvas,x,y,Symbol);
                  {$EndIf}
               end;
            end;
         until System.eof(ShapeFile);
         Dispose(ShapePoints);
       end
       else begin
         New(ShapePoints3D);
         BlocksNeeded := succ( ( (MainFileHeader.FileLength - 50) div Sizeof(sfPointsZWithHeader)) div sfMaxPoints);
         repeat
            BlockRead(ShapeFile,ShapePoints3D^[1],sfMaxPoints*Sizeof(sfPointsZWithHeader),RecsRead);
            RecsRead := RecsRead div SizeOf(sfPointsZWithHeader);
            {$IfDef VCL} if ShowSatProgress then UpdateProgressBar(OnBlock/BlocksNeeded); {$EndIf}
            inc(OnBlock);
            for i := 1 to RecsRead do begin
               MapDraw.LatLongDegreeToScreen(ShapePoints3D^[i].y,ShapePoints3D^[i].x,x,y);
               if MapDraw.OnScreen(x,y) then begin
                  {$IfDef VCL}
                     ScreenSymbol(Bitmap.Canvas,x,y,Symbol);  //,SymbolSize,Bitmap.Canvas.Pen.Color);
                  {$EndIf}
               end;
            end;
         until System.eof(ShapeFile);
         Dispose(ShapePoints3D);
       end;
   end
   else begin
     {$IfDef TimeDBPlot} writeLineToDebugFile('Line/area DB    Pen color: ' + IntToStr(Bitmap.Canvas.Pen.Color) + '  Pen width ' + IntToStr(Bitmap.Canvas.Pen.Width) + '  Brush color ' + IntToStr(Bitmap.Canvas.Brush.Color)); {$EndIf}
      nDrawn := 0;
      Track:= UpdateRate(NumRecs);
      MoveToStartShapefile;
      try
         ShapefileRandomAccess := false;
          for i := 1 to NumRecs do begin
             if OSMdata then begin
                Bitmap.Canvas.Pen.Width := Bitmap.Canvas.Pen.Width * 2;
             end;
             if PlotSingleRecordMap(MapDraw,Bitmap,i) then inc(NDrawn)
             else begin
                 {$IfDef TrackNoPlots} writeLineToDebugFile('tShapeFile.PlotAllRecords, fail to draw ' + IntToStr(i)); {$EndIf}
             end;
             if ShowSatProgress and (i mod Track = 0) and (not ThreadsWorking) then begin
                {$IfDef VCL} UpdateProgressBar(i/Numrecs); {$EndIf}
                if WantOut then Break;
             end;
             if OSMdata then begin
                Bitmap.Canvas.Pen.Width := Bitmap.Canvas.Pen.Width div 2;
             end;
          end;
       finally
         ShapefileRandomAccess := true;
       end;
   end;
   {$IfDef VCL} if ShowSatProgress and (not ThreadsWorking) then EndProgress; {$EndIf}
   {$If Defined(TimeShapefile) or Defined(TimeDBPlot)}  WriteLineToDebugFile('PlotAllRecords, n=' + IntToStr(nDrawn) + '  took ' + RealToString(Stopwatch1.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
end;



function tShapeFile.LineLength;
var
   i,j : int32;
   Dist,Bearing : float64;
begin
   {$IfDef RecordLineLength} writelineToDebugFile('tShapeFile.LineLength, recno=' + IntToStr(RecNum)); {$EndIf}
   Result := 0;
   if LineShapeFile(MainFileHeader.ShapeType) or AreaShapeFile(MainFileHeader.ShapeType) then begin
      GetLineCoords(RecNum,GetZs);
      if AreaShapeFile(MainFileHeader.ShapeType) and (CurrentPolyLineHeader.NumParts > 1) {and (MapForm <> Nil)} then AreasCounterClockwise;

      for j := 1 to CurrentPolyLineHeader.NumParts do begin
         if LineShapeFile(MainFileHeader.ShapeType) or (not CurrentLinePartsCCW[j]) then begin
            for i := CurrentLinePartSize[j] to pred(CurrentLinePartSize[j] + (CurrentLinePointsPerPart[j])) do begin
               if (i > CurrentLinePartSize[j]) then begin
                  VincentyCalculateDistanceBearing(CurrentLineCoords^[i].Lat,CurrentLineCoords^[i].Long,CurrentLineCoords^[pred(i)].Lat,CurrentLineCoords^[pred(i)].Long,Dist,Bearing);
                  {$IfDef RecordLineLength} writelineToDebugFile(RealToString(dist,12,2)); {$EndIf}
                  if not Math.IsNAN(Dist) then Result := Result + Dist;
               end;
            end;
         end;
      end;
   end;
end;


function tShapeFile.PointInRecord(RecNum : int32; Lat,Long : float64) : boolean;
var
   Part,Point,j : int32;
   InPart : array[1..sfMaxParts] of boolean;
begin
   {$IfDef RecordFindPoint} writeLineToDebugFile('tShapeFile.PointInRecord, rec=' + IntToStr(RecNum)); {$EndIf}

//http://delphi.about.com/cs/adptips2001/a/bltip0601_5.htm
//similar to http://www.daniweb.com/code/snippet216805.html
  Result := False;
  if GetLineCoords(RecNum) then begin
      if not PointInBoundingBox(Lat,Long,CurrentPolyLineHeader.BoundBox) then exit;
      AreasCounterClockwise;
      for Part := 1 to CurrentPolyLineHeader.NumParts do begin
         InPart[Part] := false;
         J := CurrentLinePartSize[Part] + pred(CurrentLinePointsPerPart[Part]);
         for Point := CurrentLinePartSize[Part] to CurrentLinePartSize[Part] + pred(CurrentLinePointsPerPart[Part]) do begin
             if ((CurrentLineCoords^[Point].Lat <=Lat) and (Lat < CurrentLineCoords^[J].Lat)) or ((CurrentLineCoords^[J].Lat <=Lat) and (Lat < CurrentLineCoords^[Point].Lat)) then begin
                if (Long < (CurrentLineCoords^[j].Long - CurrentLineCoords^[Point].Long) * (Lat - CurrentLineCoords^[Point].Lat) /
                   (CurrentLineCoords^[j].Lat - CurrentLineCoords^[Point].Lat) + CurrentLineCoords^[Point].Long) then
                   InPart[Part] := not InPart[Part];
             end;
             J := Point;
         end;
         if ((CurrentPolyLineHeader.NumParts > 1) and CurrentLinePartsCCW[Part]) and InPart[Part] then begin
            Result := false;
            exit;
         end;
      end;
      for Part := 1 to CurrentPolyLineHeader.NumParts do begin
         if ((CurrentPolyLineHeader.NumParts = 1) or (not CurrentLinePartsCCW[Part])) and InPart[Part] then begin
            Result := true;
            exit;
         end;
     end;
  end;
   {$IfDef RecordFindPoint} writeLineToDebugFile('Out'); {$EndIf}
end;


initialization
   DrawPolygonsAsPolygons := true;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demesrishapefile in'); {$EndIf}
   {$IfDef RecordShapeFile} writeLineToDebugFile('RecordShapeFileProblems active in demesrishapefile'); {$EndIf}
   {$IfDef RecordReproject} writeLineToDebugFile('RecordReprojectProblems active in demesrishapefile (degrades performance)'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosingProblems active in demesrishapefile'); {$EndIf}
   {$IfDef RecordCreateShapeFile} WriteLineToDebugFile('RecordCreateShapeFile active in demesrishapefile'); {$EndIf}
   {$IfDef RecordFindPoint} WriteLineToDebugFile('RecordFindPointProblems active in demesrishapefile'); {$EndIf}
   {$IfDef TrackShapeDigitization} WriteLineToDebugFile('TrackShapeDigitization active in demesrishapefile'); {$EndIf}
   {$IfDef RecordShapefilesReprojection} WriteLineToDebugFile('RecordShapefilesReprojection active in demesrishapefile'); {$EndIf}
   {$IfDef RecordShapeFileWriteFull} WriteLineToDebugFile('RecordShapeFileWriteFullProblems active in demesrishapefile'); {$EndIf}
   {$IfDef RecordShapeFileWrite} WriteLineToDebugFile('RecordShapeFileWrite active in demesrishapefile'); {$EndIf}
   {$IfDef RecordMemoryStream} WriteLineToDebugFile('RecordMemoryStreamProblems active in demesrishapefile'); {$EndIf}
   {$IfDef RecordPointProblem} WriteLineToDebugFile('RecordPointProblem active in demesrishapefile (slowdown)'); {$EndIf}
   {$IfDef RecordTooManyPoints} WriteLineToDebugFile('RecordTooManyPoints active in demesrishapefile (slowdown)'); {$EndIf}
   {$IfDef RecordPart} WriteLineToDebugFile('RecordPartProblems in demesrishapefile (slowdown)'); {$EndIf}
   {$IfDef RecordMergeShapefiles} WriteLineToDebugFile('RecordMergeShapefiles in demesrishapefile'); {$EndIf}
   {$IfDef RecordShapeFileLine} WriteLineToDebugFile('RecordShapeFileLineProblems in demesrishapefile (major slowdown)'); {$EndIf}
   {$IfDef RecordLineWidth} WriteLineToDebugFile('RecordLineWidth in demesrishapefile (major slowdown)'); {$EndIf}
   {$IfDef TimeDBPlot} WriteLineToDebugFile('TimeDBPlot in demesrishapefile'); {$EndIf}
   {$IfDef RecordLineLength} WriteLineToDebugFile('RecordLineLength in demesrishapefile'); {$EndIf}
end.


