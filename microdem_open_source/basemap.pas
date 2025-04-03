unit basemap;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

//{$Define NewWidthKM}

//{$Define UseExternalDLLRoutines}

//{$Define AdjustFalseCoords}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordWKTFull}
      //{$Define RecordWKT}
      //{$Define RecordUKOS}
      //{$Define RecordDEMprojection}
      //{$Define TrackWKTstring}
      //{$Define RawProjectInverse}
      //{$Define ForwardProject}
      //{$Define RecordGridSize}
      //{$Define Track_f}
      //{$Define RawProject}
      //{$Define ProjectionInfoAllowed}

      //{$Define RecordTMParameters}
      //{$Define RecordFalse}
      //{$Define LongCent}
      //{$Define RecordProjectionParameters}
      //{$Define DetailedProjParams}
      //{$Define RecordMapProjCreateDestroy}
      //{$Define RecordDatumProblems}
      //{$Define RecordDefineDatum}
      //{$Define RecordShortDefineDatum}
      //{$Define RecordMapRoamProblems}
      //{$Define RecordPickDatum}
      //{$Define RecordWGS84Projection}
      //{$Define RecordProjection}
      //{$Define RecordM}
      //{$Define RecordVectorMap}
      //{$Define RecordSPCS}
      //{$Define RecordOpenFromTiff3072}
      //{$Define RecordGeotiffCodes)
      //{$Define RecordGeodeticCalc}          //likely slowdown
      //{$Define RecordGeotdeticCalcDetailed} //likely slowdown
      //{$Define RecordMolodenskiy}           //likely slowdown
      //{$Define RawProject}                  //slowdown
      //{$Define RecordProjectionDetails}     //slowdown
   {$ELSE}
   {$ENDIF}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DataSnap.dbclient,
   {$EndIf}
//end DB declarations

   {$IfDef VCL}
      Graphics,Forms,StdCtrls,ComCtrls,Controls,Dialogs, Menus, Buttons,{Printers,} ToolWin,
   {$EndIf}

   {$IfDef MSWindows}
      Windows, Messages,
   {$EndIf}

   SysUtils,StrUtils, classes,Math,
   PETMAR,PETMath,Petmar_types,DEMDefs;


const
   DatumWGS84 = 0;
   SphericalEarthAkm = 6378.137; {Earth equatorial radius in Km; as used in most GPS}

type
   tGeoKeys = record
      NumKeys,
      Code3075 : integer;
      KeyCode  : array[1..25] of int32;
      KeyVal   : array[1..25] of float64;
   end;

   tMapProjection = class
      protected
          procedure ProtectedDefineDatum(DatumCode : ShortString; Why : shortstring);
          function fm(Phi : float64) : float64; inline;
          function fq(Phi : float64) : float64; inline;
          function ft(Lat : float64) : float64; inline;
          procedure SubtractFalseEastingsNorthings(var x, y: float64); {$IfDef AdjustFalseCoords} inline; {$EndIf}
          procedure AddFalseEastingsNorthings(var x,y : float64); {$IfDef AdjustFalseCoords} inline; {$EndIf}
          procedure TransverseMercatorInverseProjectionRadians(X,Y : float64; var Lat,Long : float64);
          procedure TransverseMercatorForwardProjectionRadians(Lat,Long : float64; var X,Y : float64);
          function M_3_21(LatRadians : float64) : float64;  //equation 3-21 of Synder, p.17
      public
         PName   : tProjType;
         projUTMZone  : int16;
         LatHemi      : Ansichar;
         h_DatumCode,
         v_DatumCode,
         h_EllipsCode,
         wktProjName,
         pNameModifier,
         ProjDebugName : shortstring;
         ProjectionGeoBox : sfBoundBox;
         GeoKeys : tGeoKeys;
         GeotiffDoubles : array[0..25] of double;
         ModelType : SmallInt;
         VertFootFactor,
         MultFactorForFeet : float64;
         ProjLinearUnitsGeoKey,
         VerticalUnitsGeoKey,
         ProjCoordTransGeoKey,
         VerticalCSTypeGeoKey,
         GeographicTypeGeoKey2048,
         ProjectedCSTypeGeoKey3072 : int16;

         wktString : ANSIstring;
         ThisIsUTMFile,
         FullWorld : boolean;

       //datum definitions
         h_XDat,
         h_YDat,
         h_ZDat       : int16;

       //Basic floating point parameters for the projection
         Lat0,    {in radians}
         Long0,   {central meridian in radians}
         false_east,false_north,ProjMapScale,a,e,h_f,
         Phi1,Phi2,TM_Phi2,

       //Derived floating point parameters for the projection
         E2,      {eccentricity squared}
         EP2,     { E2 / (1 - E2) }
         n,c,rho0,nalbers,n_lcc,k0,
         SinLatCent,CosLatCent, CosPhi1,
         t0,t2,Big_F,Beta1,rq,D,q2,q1,t1,
         E1,MUPRM,Phi4,Phi6,
         tc,mc,
         UTM_S1,S2,S4,S6,
         M0,M1,M2     : float64;

         constructor Create(DebugName : shortstring = '');
         destructor Destroy; override;

         function InitProjFomDEMHeader(var DEMHeader : tDEMHeader; DebugName : shortstring = '') : boolean;
         function InitProjFromWKTfile(fName : PathStr) : boolean;
         function InitProjFromWKTstring(TheProjectionString : Ansistring) : boolean;
         procedure StartUTMProjection(UTMZone : integer);

         procedure SetDatumConstants;
         procedure SetDatumAndTMConstants;
         procedure DefineWGS84;
         procedure DefineDatumFromUTMZone(DatumCode : ShortString; UTMZone : byte; inLatHemi : ANSIchar;  Why : shortstring = '');
         function CheckForAllInDirectoryWKT(thePath : PathStr) : boolean;

         procedure ProcessGeotiffKey(wKeyID,wValueOffset : word);
         function ProcessTiff2048(TiffOffset : integer) : shortString;
         function OpenFromTiff3072(TiffOffset : integer) : shortstring;
         function ProcessTiff3075(TiffOffset : int16) : shortString;
         function ProcessTiff4096(TiffOffset : integer) : shortstring;
         procedure SetProjectionParameterFromGeotiffKey(Key : integer; Value : float64);

         procedure InitialProjectedBoundingBox(var ProjBox : sfBoundBox);

         function PreferLocationString(Lat,Long : float64) : ShortString;

         procedure ShortProjInfo(Why : shortstring);

         procedure ForwardProjectDegrees(Lat,Long : float64; var x,y : float64; Check : boolean = true);
         procedure ForwardProjectRadians(Lat,Long : float64; var x,y : float64; Check : boolean = true);
         procedure InverseProjectDegrees(x,y : float64; var Lat,Long : float64);
         procedure InverseProjectRadians(x,y : float64; var Lat,Long : float64);
         function  GetMapScaleFactor(Lat,Long : float64; var h,k : float64; var Prime : boolean) : boolean;

         function MGRStoLatLong(MGRS : ShortString; var Lat,Long : float64) : boolean;
         procedure GetHundredKMeterSquareID(var SquareID : ShortString; XUTM,YUTM : float64); {calculates two digit 100,000 m square ID from longitude, easting and northing, and spheroid in use}

         procedure LatLongDegreetoUTM(Lat,Long : float64; var XUTM,YUTM : float64);
         procedure UTMtoLatLongDegree(XUTM,YUTM : float64; var Lat,Long : float64);

         function UTMStringFromLatLongDegree(Lat,Long : float64; IncludeDatum : boolean = true) : shortString;
         function CalculateMGRS(X,Y : float64; UTMLen : byte) : shortstring;
         function LatLongToMGRS(Lat,Long : float64; theLength : integer = 10) : shortstring;
         function MGRSvalid : boolean;
         function UTMLocationString(XUTM,YUTM : float64) : ShortString;
         function UTMZoneString : shortstring;

         procedure GetProjectParameters;

         function ProjParamsList(Full : boolean = false) : tStringList;
         procedure ProjectionParamsToDebugFile(Why : shortstring; Full : boolean = false);
         function GetProjName : shortstring;
         function KeyDatumParams : shortstring;
         procedure WriteProjectionSummaryToDebugFile(why : shortstring);

         function ConformalProjection : boolean;
         function UTMLikeProjection : boolean;
         function EllipsoidalProjection : boolean;
         function SeriousWorkingProjection : boolean;
         function ProjectionUsingWKT : boolean;


         function TissotEnabled : boolean;

         function ConvertUTMBoundBoxToGeoBoundBox(utmBox : sfBoundbox) : sfBoundBox;
         function ConvertGeoBoundBoxToUTMBoundBox(geoBox : sfBoundbox) : sfBoundBox;
         function ConvertProjectedBoundBoxToGeoBoundBox(projBox : sfBoundbox) : sfBoundBox;


         {$If Defined(RecordDefineDatum) or Defined(ProjectionInfoAllowed)}
            procedure LogDatumInfo(Why : shortstring);
         {$EndIf}

         {$IfDef RecordProblems}
            procedure WriteProjParamsToDebugFile(Title : shortString; Full : boolean = false);
            function FalseSettingsString : shortstring;
         {$EndIf}

         {$IfDef RecordTMParameters}
            procedure TMDetails(Why : shortstring);
         {$EndIf}

   end;

   tRegistration = (RegNone,RegProjection);  //,regTIN);

   tRegVars = record
      Registration         : tRegistration;
      pName             : tProjType;
      UpLeftX,UpLeftY      : float64;
      pr_deltaX,pr_deltaY  : float64;
   end;


procedure SetLatLongTickInterval(LatRange : float64; var TickSize,labelSize : float64; var OutMethod : tLatLongMethod);
procedure GetOutPutMethodFromTickSize(TickSize : float64; var OutMethod : tLatLongMethod);
procedure SetLatLongTickLabel(TickSize : float64; var LabelSize : float64; var OutMethod : tLatLongMethod);
procedure SetUpDefaultNewProjection(var inMapProjection : tMapProjection; SetCenter : boolean = true);

function CreateUKOSprojection : tMapProjection;
function CreateIrishProjection : tMapProjection;

procedure VincentyPointAtDistanceBearing(StartLat,StartLong,DistanceMeters,Bearing : float64; var Lat,Long : float64);
procedure VincentyCalculateDistanceBearing(StartLat,StartLong,EndLat,EndLong : float64; var DistanceMeters,Bearing : float64);
function DistanceInKMLawOfCosines(Lat1,Long1,Lat2, Long2 : float64) : float64;
function DistanceInKMHaversine(Lat1,Long1,Lat2, Long2 : float64) : float64;

procedure MolodenskiyTransformation(InLat,InLong : float64; var OutLat,OutLong : float64; FromMap,ToMap: tMapProjection);

procedure RedefineWGS84DatumConstants(NewLong : float64; LatHemi : ANSIchar = 'N'); overload;
procedure RedefineWGS84DatumConstants(NewLat,NewLong : float64); overload;

function BuildMGRSstring(GridZo : ShortString; x,y : float64; UTMLen : integer) : shortstring;
function GetUTMZone(Long : float64) : integer;
function UTMZoneExtent(UTMZone : byte) : shortstring;
function UTMZoneCentralLong(UTMZone : byte) : integer;
function GridZoneDes(Lat,Long : float64) : ShortString;
function UTMString(xutm,yutm : float64) : shortstring;

function DatumName(var DatumCode : ShortString) : ShortString;
function EllipsoidName(EllipCode : ShortString) : ShortString;

function GetEPSGforUTMZone(HDatum : shortstring; LatHemi : ANSIchar; UTMzone : integer) : integer;
function GetEPSGforUTMDatumCode(PrimaryMapDatum : tMapProjection) : word;
function GetEPSGforGeoDatumCode(PrimaryMapDatum : tMapProjection) : word;
function GetEPSGforVerticalDatumCode(PrimaryMapDatum : tMapProjection) : word;

function WGSEquivalentDatum(StartDatum : shortstring) : boolean;
function HemiFromLat(Lat : float64) : ANSIchar;

procedure MetersPerDegree(Lat,Long : float64; var DistanceNS,DistanceEW,DistanceAVG : float64);
function FindSingleWKTinDirectory(thePath : PathStr) : PathStr;

{$IfDef VCL}
   procedure PickDatum(WhatFor : shortstring; var DatumCode : ShortString);
   procedure PickUTMZone(var UTMZone : int16);
   procedure GetSecondaryDatum;
   procedure GetMapParameters(var Hemi : ANSIchar; var UTMZone : int16; var DatCode : ShortString; UTMZoneOnly : boolean = false);
   procedure GetMapParametersSPCSOption(var ProjFileName : PathStr; var Hemi : ANSIchar; var UTMZone : int16; var DatCode : ShortString; var  PName :  tProjType; var GeoLatLong : boolean; UTMZoneOnly : boolean = false; AreaName : shortString = '');
{$EndIf}


const
   PointConvertDebug : boolean = false;
   GeotiffDatumDefaultString = 'GeoTIFF save only works with NAD or WGS datums; default to WGS84';

var
   OppositeSideMapTolerance : integer;
var
   WGS84DatumConstants   : tMapProjection;
   ReportErrors,
   UseDefaultDatumZone,
   ReallyReadDEM  : boolean;
   SaveFileName : PathStr;

{$IfDef RawProjectInverse}
   DebugRawProjectInverse : boolean = false;
{$EndIf}


implementation

uses
   {$IfDef VCL}
      PetDBUtils,
      Nevadia_Main,
   {$EndIf}
   DEMPickDatum,
   DEMDef_routines;

const
   SphericalMercatorA : float64 = (6378137.0 * 30 * 360 / 2 / Pi);
   eeA1 = 1.340264;
   eeA2 = -0.081106;
   eeA3 = 0.000893;
   eeA4 = 0.003796;
   eeM  = 0.86602540378;  //(0.5 * sqrt(3));


{$I basemap_datum.inc}

{$I basemap_vincenty.inc}

procedure tMapProjection.WriteProjectionSummaryToDebugFile(why : shortstring);
begin
   WriteLineToDebugFile(Why + ' Pname=' + GetProjName + ' ' + KeyDatumParams);
end;



function tMapProjection.ProjectionUsingWKT : boolean;
begin
   Result := PName in [AlbersEqAreaConicalEllipsoid,PolarStereographicEllipsoidal,LambertConformalConicEllipse,LamAzEqAreaEllipsoidal,CylindricalEqualAreaEllipsoidal,AzimuthalEquidistantEllipsoidal];
end;


function tMapProjection.InitProjFomDEMHeader(var DEMHeader : tDEMHeader; DebugName : shortstring = '') : boolean;
begin
   {$IfDef RecordProjectionParameters} ProjectionParamsToDebugFile('tMapProjection.InitProjFromDEMHeader, after definition'); {$EndIf}
   {$IfDef RecordDEMprojection} WriteProjectionSummaryToDebugFile('tMapProjection.InitProjFomDEMHeader in, '); {$EndIf}
   {$If Defined(TrackWKTstring)} WriteLineToDebugFile('tMapProjection.InitProjFomDEMHeader, Map wkt=' + IntToStr(Length(wktString))); {$EndIf}
   {$If Defined(TrackWKTstring)} WriteLineToDebugFile('tMapProjection.InitProjFomDEMHeader, DEM wkt=' + IntToStr(Length(DEMHeader.wktString))); {$EndIf}

   Result := true;
   LatHemi := DEMheader.LatHemi;
   h_DatumCode := DEMHeader.h_DatumCode;
   V_DatumCode := VertDatumName(DEMHeader.VerticalCSTypeGeoKey);
   projUTMZone := DEMheader.UTMZone;
   if (h_DatumCode = 'UK-OS') then PName := UK_OS
   else if (DEMheader.DEMUsed = ArcSecDEM) then PName := PlateCaree;

   if (DEMheader.wktString <> '') then begin
      {$If Defined(RecordDEMprojection) or Defined(TrackWKTstring)} WriteLineToDebugFile('tMapProjection.InitProjFomDEMHeader DEM wkt=' + IntToStr(Length(DEMheader.wktString))); {$EndIf}
      DEMheader.DEMUsed := WKTDEM;
      InitProjFromWKTstring(DEMheader.wktString);
      //Result := DecodeWKTProjectionFromString(DEMheader.wktString);
   end
   else if (DEMheader.DEMUsed = UTMbasedDEM) then begin
      PName := UTMEllipsoidal;
      DefineDatumFromUTMZone(h_DatumCode,DEMheader.UTMZone,LatHemi,'tDEMDataSet.DefineDEMVariables');
      StartUTMProjection(DEMheader.UTMZone);
   end
   else begin
      GetProjectParameters;
   end;
   ProjDebugName := DebugName;
   {$IfDef RecordDEMprojection} WriteLineToDebugFile('tMapProjection.InitProjFromDEMHeader, map ' + GetProjName); {$EndIf}
   {$If Defined(RecordDEMprojection) or Defined(TrackWKTstring)} WriteLineToDebugFile('tMapProjection.InitProjFomDEMHeader out, Map wkt=' + IntToStr(Length(wktString))); {$EndIf}
end;


function tMapProjection.ConvertGeoBoundBoxToUTMBoundBox(geoBox : sfBoundbox) : sfBoundBox;

   procedure AddPoint(Long,Lat : float64);
   var
      xutm,yutm : float64;
   begin
      LatLongDegreetoUTM(Lat,Long,xutm,yutm);
      CompareValueToExtremes(yutm,Result.ymin,Result.ymax);
      CompareValueToExtremes(xutm,Result.xmin,Result.xmax);
   end;

begin
   InitializeBoundBox(Result);
   AddPoint(geobox.xmin,geobox.ymin);
   AddPoint(geobox.xmax,geobox.ymin);
   AddPoint(geobox.xmin,geobox.ymax);
   AddPoint(geobox.xmax,geobox.ymax);
end;



function tMapProjection.ConvertUTMBoundBoxToGeoBoundBox(utmBox : sfBoundbox) : sfBoundBox;

   procedure AddPoint(xutm,yutm : float64);
   var
      Lat,Long : float64;
   begin
      UTMtoLatLongDegree(xutm,yutm,Lat,Long);
      CompareValueToExtremes(yutm,Result.ymin,Result.ymax);
      CompareValueToExtremes(xutm,Result.xmin,Result.xmax);
   end;

begin
   InitializeBoundBox(Result);
   AddPoint(utmBox.xmin,utmBox.ymin);
   AddPoint(utmBox.xmax,utmBox.ymin);
   AddPoint(utmBox.xmin,utmBox.ymax);
   AddPoint(utmBox.xmax,utmBox.ymax);
end;


function tMapProjection.ConvertProjectedBoundBoxToGeoBoundBox(projBox : sfBoundbox) : sfBoundBox;

   procedure AddPoint(xproj,yproj : float64);
   var
      Lat,Long : float64;
   begin
      InverseProjectDegrees(xproj,yproj,Lat,Long);
      CompareValueToExtremes(Lat,Result.ymin,Result.ymax);
      CompareValueToExtremes(Long,Result.xmin,Result.xmax);
   end;

begin
   InitializeBoundBox(Result);
   AddPoint(projBox.xmin,projBox.ymin);
   AddPoint(projBox.xmax,projBox.ymin);
   AddPoint(projBox.xmin,projBox.ymax);
   AddPoint(projBox.xmax,projBox.ymax);
end;


procedure MetersPerDegree(Lat,Long : float64; var DistanceNS,DistanceEW,DistanceAVG : float64);
var
   Lat2,Long2,Lat1,Long1,Bearing : float64;
begin
   Lat1 := Lat + 0.5;
   Lat2 := Lat - 0.5;
   Long2 := Long + 0.5;
   Long1 := Long - 0.5;
   VincentyCalculateDistanceBearing(Lat1,Long,Lat2,Long,DistanceNS,Bearing);
   VincentyCalculateDistanceBearing(Lat,Long1,Lat,Long2,DistanceEW,Bearing);
   DistanceAVG := 0.5 * (DistanceNS + DistanceEW);
end;



function UTMString(xutm,yutm : float64) : shortstring;
begin
   Result := 'xutm=' + RealToString(xutm,-18,1) + '   yutm=' + RealToString(yutm,-18,1)
end;



function HemiFromLat(Lat : float64) : ANSIchar;
begin
    if (Lat > 0) then Result := 'N' else Result := 'S';
end;


function tMapProjection.FalseSettingsString : shortstring;
begin
   Result := 'Lat0: ' + RadToDegString(Lat0) + ' Long0: ' + RadToDegString(Long0) + ' false_east=' + RealToString(false_east,-12,-2) + ' false_north=' + RealToString(false_north,-12,-2);
end;


function FindSingleWKTinDirectory(thePath : PathStr) : PathStr;
var
   TheFiles : tStringList;
begin
   TheFiles := tStringList.Create;
   FindMatchingFiles(thePath,'*.wkt',TheFiles,0);
   if (TheFiles.Count = 1) then Result := TheFiles.Strings[0]
   else Result := '';
   TheFiles.Free;
end;

function tMapProjection.CheckForAllInDirectoryWKT(thePath : PathStr) : boolean;
var
   fName : PathStr;
begin
   fName := thePath + 'all.prj';
   if FileExists(fName) then Result := InitProjFromWKTfile(fName)
   else begin
      fName := thePath + 'all.wkt';
      if FileExists(fName) then Result := InitProjFromWKTfile(fName)
      else begin
         fName := FindSingleWKTinDirectory(thePath);
         if fName = '' then Result := false
         else Result := InitProjFromWKTfile(fName);
      end;
   end;
end;


procedure tMapProjection.SetProjectionParameterFromGeotiffKey(Key : integer; Value : float64);
begin
   case Key of
      2057 : a := Value;
      2058 : begin end; // b, EllipsoidSemiMinorAxisGeoKey, not currently used
      2059 : h_f := Value;
      2061 : begin end; // PrimeMeridianLongitudeGeoKey, not currently used
      3075 : ProcessTiff3075(round(value));
      3078 : Phi1 := Value * DegToRad;
      3079 : Phi2 := Value * DegToRad;
      3080 : Long0 := Value * DegToRad;
      3081 : Lat0 := Value * DegToRad;
      3082,3086 : False_east := Value;
      3083,3087 : False_north := Value;
      3084 : Long0 := Value * DegToRad;
      3085 : Lat0 := Value * DegToRad;
      3088 : Long0 := Value * DegToRad;
      3089 : Lat0 := Value * DegToRad;
      3092 : ProjMapScale := Value;
      else MessageToContinue('tMapProjection.SetProjectionParameterFromGeotiffKey Unhandled case=' + IntToStr(Key));
   end;
end;


procedure tMapProjection.InitialProjectedBoundingBox(var ProjBox : sfBoundBox);

   procedure SetBox(LatRadius,LongRadius : float64);
   var
      Dummy : float64;
   begin
      ProjectionGeoBox.xmin := Long0 - LongRadius;
      ProjectionGeoBox.xmax := Long0 + LongRadius;
      ProjectionGeoBox.ymin := Lat0 - LatRadius;
      ProjectionGeoBox.ymax := Lat0 + LatRadius;

      ForwardProjectRadians(Lat0 + LatRadius * DegToRad,Long0,Dummy,ProjBox.ymax);
      ForwardProjectRadians(Lat0 - LatRadius * DegToRad,Long0,Dummy,ProjBox.ymin);
      ForwardProjectRadians(Lat0,Long0 + LongRadius * DegToRad,ProjBox.xmax,Dummy);
      ForwardProjectRadians(Lat0,Long0 - LongRadius * DegToRad,ProjBox.xmin,Dummy);
   end;


begin
   if PName in [PolarStereographicEllipsoidal] then begin
      ProjBox.xmin := -2486818;
      ProjBox.xmax := 2486818;
      ProjBox.ymin := -2486818;
      ProjBox.ymax := 2486818;
   end
   else if PName in [LamAzEqArea,SphericalStereographic,OrthoProj,OldStereographic] then begin
      SetBox(89.95,89.95);
   end
   else if PName in [HammerProj,SinusProj] then begin
      SetBox(89.95,179.95);
   end
   else if PName in [Gnomonic] then begin
      SetBox(40,40);
   end
   else begin
      if (PName in [UTMEllipsoidal]) then begin
         ProjectionGeoBox.xmin := (-183 + MDDef.DefaultUTMZone * 6) -3;
         ProjectionGeoBox.xmax := (-183 + MDDef.DefaultUTMZone * 6) +3;
         ProjectionGeoBox.ymin := 34;
         ProjectionGeoBox.ymax := 42;
      end
      else if PName in [LambertConformalConicEllipse,AlbersEqAreaConicalEllipsoid] then begin
         //for the US
         ProjectionGeoBox.xmin := -120;
         ProjectionGeoBox.xmax := -58;
         ProjectionGeoBox.ymin := 20;
         ProjectionGeoBox.ymax := 50;
      end
      else if PName in [Cassini] then begin
         ProjectionGeoBox.xmin := -30;
         ProjectionGeoBox.xmax := 30;
         ProjectionGeoBox.ymin := 20;
         ProjectionGeoBox.ymax := 55;
      end
      else begin
         ProjectionGeoBox.xmin := -179.99;
         ProjectionGeoBox.xmax := 179.99;
         if PName in [WebMercator,Mercator,MercatorEllipsoid,MillerCylindrical,Mollweide,EquidistantCylindrical] then begin //projections that blow up at the pole
            ProjectionGeoBox.ymin := -75;
            ProjectionGeoBox.ymax := 75;
         end
         else begin
            ProjectionGeoBox.ymin := -90;
            ProjectionGeoBox.ymax := 90;
         end;
      end;
      {$IfDef RecordProjectionDetails} PointConvertDebug := true; {$EndIf}
      ForwardProjectDegrees(ProjectionGeoBox.ymin,ProjectionGeoBox.xmin,ProjBox.xmin,ProjBox.ymin);
      ForwardProjectDegrees(ProjectionGeoBox.ymax,ProjectionGeoBox.xmax,ProjBox.xmax,ProjBox.ymax);
      {$IfDef RecordProjectionDetails} PointConvertDebug := false; {$EndIf}
   end;
   {$IfDef RecordProjectionParameters} WriteLineToDebugFile('tMapProjection.InitialProjectedBoundingBox, projected=  ' + sfBoundBoxToString(ProjBox)); {$EndIf}
   {$IfDef RecordProjectionParameters} WriteLineToDebugFile('tMapProjection.InitialProjectedBoundingBox, geo=  ' + sfBoundBoxToString(ProjectionGeoBox)); {$EndIf}
   {$If Defined(LongCent)} WriteLineToDebugFile('tMapProjection.InitialProjectedBoundingBox Out, LongCent: ' + RadToDegString(Long0)); {$EndIf}
end;


function tMapProjection.ProcessTiff3075(TiffOffset : int16) : shortString;
begin
    ProjCoordTransGeoKey := TiffOffset;
    if (TiffOffset in [1,7,8,10,11,12,14,15,17]) then begin //(6.3.3.3 codes)
       projUTMZone := 0;
       if (TiffOffset = 1) then PName := GeneralTransverseMercator;
       if (TiffOffset = 7) then PName := MercatorEllipsoid;
       if (TiffOffset = 8) then PName := LambertConformalConicEllipse;
       if (TiffOffset = 11) then PName := AlbersEqAreaConicalEllipsoid;
       if (TiffOffset = 10) then PName := LamAzEqAreaEllipsoidal;
       if (TiffOffset = 12) then PName := AzimuthalEquidistantEllipsoidal;
       if (TiffOffset = 14) then PName := SphericalStereographic;
       if (TiffOffset = 15) then PName := PolarStereographicEllipsoidal;
       if (TiffOffset = 17) then PName := EquiDistantCylindrical;     // aka Equirectangular;
       GeoKeys.Code3075 := TiffOffset;
       Result := GetProjName;
    end
    else if (TiffOffset = 16) then begin
       PName := ObliqueStereographic;
       Result := 'Unsupported TIFF 3075 Projection Option, Oblique Stereographic';
    end
    else Result := 'Unsupported TIFF 3075 Projection Option, offset=' + IntToStr(TiffOffset);
    {$If Defined(RecordGeotiffCodes)} WriteLineToDebugFile('tMapProjection.ProcessTiff3075 out, Projection=' + Result); {$EndIf}
    {$If Defined(LongCent)} WriteLineToDebugFile('tMapProjection.ProcessTiff3075 Out,  LongCent: ' + RadToDegString(Long0)); {$EndIf}
 end;


function tMapProjection.ProcessTiff2048(TiffOffset : integer) : shortString;
begin
   GeographicTypeGeoKey2048 := TiffOffset;
   if (TiffOffset = 32767) then Result := 'User defined'
   else begin
      Result := '';
       case TiffOffset of
          4030,4326 : H_datumCode := 'WGS84';
          4267      : H_datumCode := 'NAD27';
          4269      : H_datumCode := 'NAR-C';   //NAD83
       end;
       PName := PlateCaree;
   end;
end;


function tMapProjection.OpenFromTiff3072(TiffOffset : integer) : shortstring;
var
   tCode : integer;
begin
   {$If Defined(RecordGeotiffCodes) or Defined(RecordOpenFromTiff3072)} WriteLineToDebugFile('OpenFromTiff3072 Code ' + IntToStr(TiffOffset)); {$EndIf}
   ProjectedCSTypeGeoKey3072 := TiffOffset;
   if (TiffOffset = 32767) then Result := 'User defined'
   else begin
      LatHemi := 'N';
      if (TiffOffset = 2193) then begin
         LatHemi := 'S';
         InitProjFromWKTfile(ProgramRootDir + 'wkt_proj\nzgd2000_epsg_2193.wkt');
         Result := wktProjName;
      end
      else if (TiffOffset = 27700) then begin
         PName := UK_OS;
      end
      else if (TiffOffset = 0) or (TiffOffset = 32662)  then begin
          PName := PlateCaree;
          H_datumCode := 'WGS84';
      end
      else if ((TiffOffset = 3067)) then begin
          StartUTMProjection(35);
          H_DatumCode := 'ETR89';
      end
      else if ((TiffOffset = 4283)) then begin
          StartUTMProjection(35);
          H_DatumCode := 'GDA94';
      end
      else if ((TiffOffset = 5070)) then begin
          PName := AlbersEqAreaConicalEllipsoid;
          H_datumCode := 'WGS84';
          GetProjectParameters;
      end
      else if ((TiffOffset >= 3708) and (TiffOffset <= 3726))  then begin
          StartUTMProjection(TiffOffset - 3707);
          H_DatumCode := 'NAD83';
      end
      else if ((TiffOffset >= 3731) and (TiffOffset <= 3750)) then begin
         StartUTMProjection(TiffOffset - 3730);
         H_DatumCode := 'NAD83';
      end
      else if ((TiffOffset >= 6330) and (TiffOffset <= 6348)) then begin
         StartUTMProjection(TiffOffset - 6329);
         H_DatumCode := 'NAD83';
      end
      else if (TiffOffset = 3294) then begin
         //http://www.gdal.org/gdalsrsinfo.html
      end
      else if (TiffOffset = 8693) then begin  //Guam and Marianas NAD83(MA11) UTM zone 55
          StartUTMProjection(55);
          H_DatumCode := 'NAD83';     //really GRS1980, but that has to be verified and modified if necessary
      end
      else begin
          tCode := TiffOffset div 100;
          if ((tCode = 230) or (tCode = 267) or (tCode = 269) or (tCode = 326) or (tCode = 258) or (tCode = 283) or (tCode = 322)) and (TiffOffset mod 100 <= 60) then begin
             {$IfDef RecordOpenFromTiff3072} WriteLineToDebugFile('OpenFromTiff3072 UTM Code 258, 267, 269, 326'); {$EndIf}
             case tCode of
                230 : H_DatumCode := 'EUR-A';
                258 : H_DatumCode := 'ETR89';
                267 : H_DatumCode := 'NAD27';
                269 : H_DatumCode := 'NAD83';
                283 : begin
                         H_DatumCode := 'GDA94';
                         LatHemi := 'S';
                      end;
                322 : H_DatumCode := 'WGS72';
                326 : H_DatumCode := 'WGS84';
             end;
             StartUTMProjection(TiffOffset mod 100);
          end
          else if (tCode = 323) or (tCode = 327) or (tCode=161) then begin
             case tCode of
                323 : H_datumCode := 'WGS72';
                327 : H_datumCode := 'WGS84';
             end;
             LatHemi := 'S';
             StartUTMProjection(TiffOffset mod 100);
          end
          else if (tCode = 269) or (tCode = 321) then begin  //US state plane, NAD83
             H_DatumCode := 'NAD83';
             {$IfDef RecordProblems} HighlightLineToDebugFile('SPCS only supported with WKT'); {$EndIf}  //Message, since there might also be good WKT in the file
          end
          else begin
             {$IfDef RecordOpenFromTiff3072} WriteLineToDebugFile('OpenFromTiff3072 Unhandled Code ' + IntToStr(TiffOffset)); {$EndIf}
          end;
      end;
      if Result = '' then Result := GetProjName;
      if (projUTMZone = -99) then ProjUTMzone := 0;
   end;
   {$If Defined(RecordGeotiffCodes) or Defined(RecordOpenFromTiff3072) or Defined(RecordProjection)} WriteLineToDebugFile('tMapProjection.RecordOpenFromTiff3072 out, Projection=' + Result); {$EndIf}
   {$If Defined(LongCent)} WriteLineToDebugFile('tMapProjection.RecordOpenFromTiff3072 Out,  LongCent: ' + RadToDegString(Long0)); {$EndIf}
end;


function tMapProjection.ProcessTiff4096(TiffOffset : integer) : shortstring;
//
//Geotiff 6.3.4.1 Vertical CS Type Codes
begin
   VerticalCSTypeGeoKey := TiffOffset;
   case TiffOffSet of
     5030,
     5031 : Result := 'VertCS_WGS_84_ellipsoid';
     5032 : Result := 'VertCS_OSU86F_ellipsoid';
     5033 : Result := 'VertCS_OSU91A_ellipsoid';
     5102 : Result := 'VertCS_North_American_Vertical_Datum_1929';
     5103,
     5703 : Result := 'VertCS_North_American_Vertical_Datum_1988';
     5714 : Result := 'msl';
     5773 : Result := 'Vert_CS_EGM96';
     3855 : Result := 'Vert_CS_EGM2008';
     else Result := 'other 6.3.4.1 code (' + IntToStr(TiffOffset) + ')';
   end;
end;

procedure tMapProjection.ProcessGeotiffKey(wKeyID,wValueOffset : word);
var
   hFactor,vFactor : float64;
begin
   if (wKeyID = 1024) then ModelType := wValueOffset;
   if (wKeyID = 2048) then ProcessTiff2048(wValueOffset);
   if (wKeyID = 2057) then a := GeotiffDoubles[wValueOffset] * DegToRad;
   if (wKeyID = 2059) then h_f := GeotiffDoubles[wValueOffset] * DegToRad;

   if (wKeyID = 3072) then OpenFromTiff3072(wValueOffset);
   if (wKeyID = 3074) then OpenFromTiff3072(wValueOffset);
   if (wKeyID = 3075) then ProcessTiff3075(wValueOffset);
   if (wKeyID = 3076) then begin
      ProjLinearUnitsGeoKey := wValueOffset;
      hFactor := LengthConversion(wValueOffset);
   end;

   if (wKeyID = 3078) then phi1 := GeotiffDoubles[wValueOffset] * DegToRad;
   if (wKeyID = 3079) then phi2 := GeotiffDoubles[wValueOffset] * DegToRad;
   if (wKeyID = 3084) then Long0 := GeotiffDoubles[wValueOffset]* Petmar_types.DegToRad;
   if (wKeyID = 3085) then Lat0 := GeotiffDoubles[wValueOffset] * Petmar_types.DegToRad;
   if (wKeyID = 3086) then false_east := hfactor * GeotiffDoubles[wValueOffset];
   if (wKeyID = 3087) then false_north := hfactor * GeotiffDoubles[wValueOffset];
   if (wKeyID = 4096) then ProcessTiff4096(wValueOffset);
   if (wKeyID = 4099) then begin
      VerticalUnitsGeoKey := wValueOffset;
      vFactor := LengthConversion(wValueOffset);
   end;
end;



function CreateUKOSprojection : tMapProjection;
begin
   Result := tMapProjection.Create;
   Result.PName := UK_OS;
   Result.GetProjectParameters;
end;


function CreateIrishProjection : tMapProjection;
begin
   Result := tMapProjection.Create;
   Result.PName := IrishGrid;
   Result.GetProjectParameters;
end;



procedure tMapProjection.StartUTMProjection(UTMZone : integer);
begin
    PName := UTMEllipsoidal;
    ThisIsUTMFile := true;
    projUTMZone := UTMZone;
    if (LatHemi = 'S') then ProjectedCSTypeGeoKey3072 := 32700 + projUTMZone
    else ProjectedCSTypeGeoKey3072 := 26900 + projUTMZone;
    Long0 := DegToRad * (-183 + 6*projUTMZone);
    GetProjectParameters;
    {$If Defined(RecordOpenFromTiff3072) or Defined(LongCent)} ShortProjInfo('StartUTMProjection'); {$EndIf}
end;


function tMapProjection.InitProjFromWKTfile(fName : PathStr) : boolean;
var
   ProjData : tStringList;
   i : integer;
begin
   Result := false;
   if FileExists(fName) then begin
     {$IfDef RecordWKT} WriteLineToDebugFile('tMapProjection.InitializeProjectionFromWKT in, ' + fName); {$EndIf}
      ProjData := tStringList.Create;
      ProjData.LoadFromFile(fName);
      {$IfDef RecordProjectionParameters} WriteLineToDebugFile('InitProjFromWKT, ' + ProjData.Strings[0]); {$EndIf}
      wktString := ptTrim(ProjData.Strings[0]);
      if (ProjData.Count > 1) then begin
         wktString := '';
         for i := 0 to pred(ProjData.Count) do
            wktString := wktString + ptTrim(ProjData.Strings[i]);
      end;
      ProjData.Free;
      Result := InitProjFromWKTstring(wktString);
      {$If Defined(RecordWKT) or Defined(LongCent)} ShortProjInfo('tMapProjection.InitProjFromWKT out'); {$EndIf}
   end
   else begin
      MessageToContinue('WKT file required: ' + fName);
   end;
end;


function tMapProjection.InitProjFromWKTstring(TheProjectionString : ANSIstring) : boolean;
var
   ftf : float64;

       procedure PrepPJstring(var pjString : ANSIstring; ParamName : shortstring; Skip : integer);
       var
          i : integer;
       begin
           ParamName := UpperCase(ParamName);
           i := Length(ParamName);
           repeat
              Delete(pjString,1,1);
           until Copy(pjstring,1,i) = ParamName;
           Delete(pjstring,1,i);
           while pjstring[1] in [','] do Delete(pjString,1,1);
           if (Skip > 0) then for i := 0 to pred(Skip) do Petmar_types.BeforeSpecifiedCharacterANSI(pjString,',',true,true);
       end;

       function FloatFromParameter(pjString : ANSIstring; ParamName : shortstring; Skip : integer; Ending : Ansichar) : float64;
       var
          te : ANSIChar;
          i : integer;
       begin
          ParamName := UpperCase(ParamName);
          if StrUtils.AnsiContainsText(UpperCase(pjString),UpperCase(ParamName)) then begin
              PrepPJstring(pjString,ParamName,Skip);
              //in case there are more fields that we designed for, it might be a comma instead of square end bracket
              i := 0;
              te := ' ';
              repeat
                  inc(i);
                  if pjString[i] = ',' then te := ',';
                  if pjString[i] = Ending then te := Ending;
              until (I = length(PjString)) or (te <> ' ');
              Result := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(pjString,te,true,true));
          end
          else Result := NaN;
          {$IfDef RecordWKTFull} WriteLineToDebugFile('Check for ' + ParamName + '=' + FloatToStr(Result)); {$EndIf}
       end;


       function StringFromParameter(pjString : ANSIstring; ParamName : shortstring; Skip : integer; Ending : Ansichar) : shortstring;
       begin
          {$IfDef RecordWKTFull} WriteLineToDebugFile('Check for ' + ParamName); {$EndIf}
          ParamName := UpperCase(ParamName);
          if StrUtils.AnsiContainsText(pjString,ParamName) then begin
              PrepPJstring(pjString,ParamName,Skip);
              Result := Petmar_types.BeforeSpecifiedCharacterANSI(pjString,Ending,true,true);
              StripCharacter(Result,'[');
              StripCharacter(Result,'"');
          end
          else Result := '';
       end;

       function TryDatum(Name : shortstring) : boolean;
       begin
          Result := StrUtils.AnsiContainsText(TheProjectionString,Name);
          if Result then begin
             h_DatumCode := StringFromParameter(TheProjectionString,Name,0,',');
          end;
       end;

       function TrySpheroid(Name : shortstring) : boolean;
       begin
          Result := StrUtils.AnsiContainsText(TheProjectionString,Name);
          if Result then begin
             a := FloatFromParameter(TheProjectionString,Name,1,',');
             h_f := FloatFromParameter(TheProjectionString,Name,2,']');
             e2 := 2 * 1 / h_f- sqr(1 / h_f);
          end;
       end;

       function ParameterInString(Name : shortstring) : boolean;
       begin
          Result := StrUtils.AnsiContainsText(UpperCase(TheProjectionString),UpperCase(Name));
       end;


(*
WKTProjectionFromString: COMPD_CS["NAD83(2011) / Illinois East (ftUS) + NAVD88 height - Geoid12B (ftUS)",PROJCS["NAD83(2011) / Illinois East (ftUS)",GEOGCS["NAD83(2011)",DATUM["NAD83_National_Spatial_Reference_System_2011",
   SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],AUTHORITY["EPSG","1116"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","6318"]],


PROJECTION["Transverse_Mercator"],PARAMETER["latitude_of_origin",36.66666666666666],PARAMETER["central_meridian",-88.33333333333333],PARAMETER["scale_factor",0.999975],PARAMETER["false_easting",984250.0000000002],
   PARAMETER["false_northing",0],UNIT["US survey foot",0.3048006096012192,AUTHORITY["EPSG","9003"]],AXIS["X",EAST],AXIS["Y",NORTH],AUTHORITY["EPSG","6455"]],


VERT_CS["NAVD88 height - Geoid12B (ftUS)",VERT_DATUM["North American Vertical Datum 1988",2005,AUTHORITY["EPSG","5103"]],UNIT["US survey foot",0.3048006096012192,AUTHORITY["EPSG","9003"]],AXIS["Gravity-related height",UP],AUTHORITY["EPSG","6360"]]]
*)

var
   HorizWKT,VertWKT : ANSIstring;
   i : integer;
begin
   {$IfDef RecordWKT} WriteLineToDebugFile('WKTProjectionFromString: ' + TheProjectionString); {$EndIf}
   Result := StrUtils.AnsiContainsText(TheProjectionString,'PROJCS') or StrUtils.AnsiContainsText(TheProjectionString,'PROJCRS') or StrUtils.AnsiContainsText(TheProjectionString,'GEOGCRS') or StrUtils.AnsiContainsText(TheProjectionString,'GEOGGCS');
   if Result then begin
      WKTString := TheProjectionString;
      wktProjName := BeforeSpecifiedCharacterANSI(WKTString,',',false,false);
      TheProjectionString := UpperCase(TheProjectionString);
      StripCharacter(TheProjectionString,' ');
      StripCharacter(TheProjectionString,'_');

      for i := 1 to Length(TheProjectionString) - 6 do begin
         if Copy(TheProjectionString,i,6) = 'VERTCS' then begin
            HorizWKT := Copy(TheProjectionString,1,pred(i));
            VertWKT := Copy(TheProjectionString,i,Length(TheProjectionString) - i);
            break;
         end
         else HorizWKT := TheProjectionString;
      end;
      TheProjectionString := HorizWKT;

      if ParameterInString('LATITUDEOFORIGIN') then Lat0 := DegToRad * FloatFromParameter(HorizWKT,'"LATITUDEOFORIGIN"',0,']')
      else if ParameterInString('Latitudeofnaturalorigin') then Lat0 := DegToRad * FloatFromParameter(HorizWKT,'"Latitudeofnaturalorigin"',0,']')
      else if ParameterInString('latitudeofcenter') then Lat0 := DegToRad * FloatFromParameter(HorizWKT,'"latitudeofcenter"',0,']')
      else if ParameterInString('Latitudeofstandardparallel') then Lat0 := DegToRad * FloatFromParameter(HorizWKT,'"Latitudeofstandardparallel"',0,']');

      if ParameterInString('CentralMeridian') then Long0 := DegToRad * FloatFromParameter(HorizWKT,'"CentralMeridian"',0,']')
      else if ParameterInString('Longitudeofnaturalorigin') then Long0 := DegToRad * FloatFromParameter(HorizWKT,'"Longitudeofnaturalorigin"',0,']')
      else if ParameterInString('longitudeofcenter') then Long0 := DegToRad * FloatFromParameter(HorizWKT,'"longitudeofcenter"',0,']')
      else if ParameterInString('Longitudeoforigin') then Long0 := DegToRad * FloatFromParameter(HorizWKT,'"Longitudeoforigin"',0,']');

      if ParameterInString('FALSEEASTING') then false_east := FloatFromParameter(HorizWKT,'"FALSEEASTING"',0,']');
      if ParameterInString('FALSENORTHING') then false_north := FloatFromParameter(HorizWKT,'"FALSENORTHING"',0,']');

      if ParameterInString('StandardParallel1') then phi1 := DegToRad * FloatFromParameter(HorizWKT,'"StandardParallel1"',0,']');
      if ParameterInString('StandardParallel2') then phi2 := DegToRad * FloatFromParameter(HorizWKT,'"StandardParallel2"',0,']');

      ProjMapScale := FloatFromParameter(HorizWKT,'"ScaleFactor"',0,']');
      if ParameterInString('UNIT["FOOT",') or ParameterInString('USSURVEYFOOT') then begin
          if ParameterInString('UNIT["FOOT",') then ftf := FloatFromParameter(HorizWKT,'"FOOT"',0,']')
          else ftf := FloatFromParameter(HorizWKT,'"USSURVEYFOOT"',0,']');
          false_east := ftf * false_east;
          false_north := ftf * false_north;
      end;

      TryDatum('DATUM');

      if not TrySpheroid('SPHEROID') then TrySpheroid('ELLIPSOID');

       if ParameterInString('NAD') and ParameterInString('83') then begin
          h_datumcode := 'NAD83';  //vmDatum := MapProjNAD83;
          LatHemi := 'N';
       end;

       if ParameterInString('AzimuthalEquidistant') then begin
          pName := AzimuthalEquidistantEllipsoidal;
       end
       else if ParameterInString('LAEA') then begin
          PName := LamAzEqAreaEllipsoidal;
       end
       else if ParameterInString('PolarStereographic') or StrUtils.AnsiContainsText(HorizWKT,'StereographicNorthPole') then begin
          PName := PolarStereographicEllipsoidal;
          Lat0 := Phi1;
       end
       else if ParameterInString('ALBERSCONICEQUALAREA') or ParameterInString('CONUSALBERS') or ParameterInString('AlbersEqualArea') then begin
          PName := AlbersEqAreaConicalEllipsoid;
       end
       else if ParameterInString('GCSOSGB1936') or ParameterInString('OSGB1936') then begin
          PName := UK_OS;
       end
       else if ParameterInString('54008') then begin
          PName := Mollweide;
       end
       else if ParameterInString('54009') then begin
          PName := SinusEllipsoidal;
       end
       else if ParameterInString('LambertConformalConic1SP') then begin
          PName := LambertConformalConicEllipse;
          phi2 := Lat0;
          phi1 := Lat0;
       end
       else if ParameterInString('LambertConformalConic') then begin
          PName := LambertConformalConicEllipse;
       end
       else if ParameterInString('UTM') then begin
          PName := UTMEllipsoidal;
          ProjUTMZone := GetUTMZone(Long0 / DegToRad);
       end
       else if ParameterInString('TransverseMercator') then begin
          PName := GeneralTransverseMercator;
       end
       else begin
          {$IfDef RecordDEMprojection} WriteLineToDebugFile('WKTProjectionFromString defined fallthrough, wkt=' + TheProjectionString); {$EndIf}
       end;

      TheProjectionString := VertWKT;
      if ParameterInString('UNIT["FOOT",') then VertFootFactor := FloatFromParameter(HorizWKT,'"FOOT"',0,']')
      else if  ParameterInString('USSURVEYFOOT') then VertFootFactor := FloatFromParameter(HorizWKT,'"USSURVEYFOOT"',0,']')
      else VertFootFactor := 1;
      if ParameterInString('NAVD88') then V_datumCode := 'NAVD88';

      {$IfDef RecordWKT} ShortProjInfo('finished WKT read'); {$EndIf}
      GetProjectParameters;
      {$If Defined(RecordDEMprojection) or Defined(RecordWKT)} WriteProjectionSummaryToDebugFile('WKTProjectionFromString out, '); {$EndIf}
   end
   else begin
      {$IfDef RecordWKT} WriteLineToDebugFile('WKTProjectionFromString failure'); {$EndIf}
   end;
end;


function tMapProjection.SeriousWorkingProjection : boolean;
begin
   Result := not (Pname in [CylindricalEqualArea]);
   //Result := Pname in [WebMercator,Mercator,MercatorEllipsoid,UTMEllipsoidal,AlbersEqAreaConicalEllipsoid,LambertConformalConic,CylindricalEqualAreaEllipsoidal,CylindricalEqualArea,EquiDistantCylindrical];
end;


procedure SetUpDefaultNewProjection(var inMapProjection : tMapProjection; SetCenter : boolean = true);
begin
   {$IfDef RecordProjectionParameters} WriteLineToDebugFile('SetUpDefaultNewProjection in ' + inMapProjection.GetProjName); {$EndIf}
   if SetCenter then inMapProjection.lat0 := 0;
   if SetCenter then inMapProjection.Long0 := 0;
   inMapProjection.False_East := 0;
   inMapProjection.False_North := 0;
   if (inMapProjection.h_datumcode = '') then inMapProjection.h_datumcode := 'WGS84';

   if (inMapProjection.PName in [UTMEllipsoidal]) then begin
      inMapProjection.Long0  := (-183 + MDDef.DefaultUTMZone * 6) * Petmar_types.DegToRad;
      inMapProjection.false_east := 500000.00;
   end
   else if (inMapProjection.PName in [LambertConformalConicEllipse]) then begin
       if SetCenter then inMapProjection.lat0 := 23 * Petmar_types.DegToRad;
       if SetCenter then inMapProjection.long0 := -96 * Petmar_types.DegToRad;
       inMapProjection.Phi1 := 33 * Petmar_types.DegToRad;
       inMapProjection.Phi2 := 45 * Petmar_types.DegToRad;
   end
   else if inMapProjection.PName in [AlbersEqAreaConicalEllipsoid] then begin
      if SetCenter then inMapProjection.long0 := (-96) * Petmar_types.DegToRad;
      inMapProjection.lat0 := 23 * Petmar_types.DegToRad;
      inMapProjection.Phi1 := 29.5 * Petmar_types.DegToRad;
      inMapProjection.Phi2 := 45.5 * Petmar_types.DegToRad;
   end
   else if inMapProjection.PName in [CylindricalEqualAreaEllipsoidal,CylindricalEqualArea] then begin
      inMapProjection.Phi1 := 5 * Petmar_types.DegToRad;
   end
   else if inMapProjection.PName in [PolarStereographicEllipsoidal] then begin
      inMapProjection.Lat0 := 75 * Petmar_types.DegToRad; {lat cent is standard parallel}
   end
   else if inMapProjection.PName in [PlateCaree,EquiDistantCylindrical,MillerCylindrical,Mollweide,Cassini,HammerProj,EquiDistantCylindrical,WebMercator,Mercator,MercatorEllipsoid,MillerCylindrical] then begin
   end;
   {$IfDef RecordProjectionParameters} WriteLineToDebugFile('SetUpDefaultNewProjection out '  + inMapProjection.GetProjName); {$EndIf}
   {$If Defined(LongCent)} WriteLineToDebugFile('SetUpDefaultNewProjection Out,  LongCent: ' + RadToDegString(inMapProjection.long0)); {$EndIf}
   {$IfDef DetailedProjParams} inMapProjection.ProjectionParamsToDebugFile('SetUpDefaultNewProjection out'); {$EndIf}
end;


procedure GetOutPutMethodFromTickSize(TickSize : float64; var OutMethod :tLatLongMethod);
begin
   if TickSize > 0.99999 then OutMethod := NearestDegree
   else if TickSize > (1/60-0.0001) then OutMethod := DecMinutes
   else OutMethod := DecSeconds;
end;


procedure SetLatLongTickLabel(TickSize : float64; var LabelSize : float64; var OutMethod : tLatLongMethod);
begin
   GetOutPutMethodFromTickSize(TickSize,OutMethod);
   LabelSize := TickSize;
   if TickSize > 4.99 then LabelSize := 10
   else if TickSize > 1.99 then LabelSize := 5
   else if TickSize > 0.99 then LabelSize := 2
   else if TickSize > 0.49 then LabelSize := 1
   else if TickSize > 0.19 then LabelSize := 0.5
   else LabelSize := TickSize;
end;


procedure SetLatLongTickInterval(LatRange : float64; var TickSize,LabelSize : float64; var OutMethod : tLatLongMethod);
begin
   TickSize := 30;
   if LatRange  < 180 then TickSize := 10;
   if LatRange  < 120 then TickSize := 15;
   if LatRange  < 50 then TickSize := 5;
   if LatRange  < 20 then TickSize := 2;
   if LatRange  < 10 then TickSize := 1;
   if LatRange  < 5  then TickSize := 0.5;
   if LatRange  < 2.5 then TickSize := 0.25;
   if LatRange  < 1  then TickSize := 1/6;    {10'}
   if LatRange  < 0.5 then TickSize := 1/12;  {5'}
   if LatRange  < 0.35 then TickSize := 2.5/60;  {2.5'}
   if LatRange  < 0.20 then TickSize := 1/60;  {1'}
   if LatRange  < 0.125 then TickSize := 1/120;  {30"}
   if LatRange  < 0.05  then TickSize := 1/240;   {15"}
   if LatRange  < 0.01  then TickSize := 1/600;   {10"}
   if LatRange  < 0.005 then TickSize := 1/1200;   {3"}
   if LatRange  < 0.002 then TickSize := 1/1800;   {2"}
   if LatRange  < 0.001 then TickSize := 1/3600;   {1"}
   SetLatLongTickLabel(TickSize,LabelSize,OutMethod);
   {$IfDef RecordGridSize} WriteLineToDebugFile('SetLatLongTickInterval, lat range='  + RealToString(LatRange,-12,-4) + '  ticks=' + RealToString(TickSize,-12,-4)); {$EndIf}
end;


procedure SphericalMercatorLatLongToUTM(Lat,Long : float64; var xutm,yutm : float64);
begin
    xutm := SphericalMercatorA * Long * Petmar_types.DegToRad;
    if abs(lat) > 89 then lat := 89;
    yutm := SphericalMercatorA * ln(Math.Tan(QuarterPi + 0.5 * Lat * Petmar_types.DegToRad))
end;



destructor tMapProjection.Destroy;
begin
   (*
   if ProjectionSharedWithDataset then begin
      {$If Defined(RecordMapProjCreateDestroy)} WriteLineToDebugFile('tMapProjection.Destroy shared projection in, ' + ProjDebugName); {$EndIf}
   end
   else begin
   *)
      {$If Defined(RecordMapProjCreateDestroy)} WriteLineToDebugFile('tMapProjection.Destroy in, ' + ProjDebugName); {$EndIf}
      try
         inherited;
      except
         on Exception do begin end;
      end;
   //end;
   {$If Defined(RecordMapProjCreateDestroy)} WriteLineToDebugFile('tMapProjection.Destroy out, ' + ProjDebugName); {$EndIf}
end;



constructor tMapProjection.Create;
begin
   {$If Defined(RecordProjectionParameters) or Defined(RecordMapProjCreateDestroy)} WriteLineToDebugFile('tMapProjection.Create in, ' + DebugName); {$EndIf}
   ProjDebugName := DebugName;
   //ProjectionSharedWithDataset := false;

   FullWorld := true;
   GeoKeys.NumKeys := 0;
   GeoKeys.Code3075 := 0;
   ProjUTMZone := -99;
   H_datumCode := '';
   V_datumCode := '';
   ProjMapScale := 1;
   wktString := '';
   PName := UndefinedProj;
   LatHemi := MDDef.DefaultLatHemi;
   ThisIsUTMFile := false;
   MultFactorForFeet := 1;
   pNameModifier := '';

    //Basic floating point parameters for projection
      Lat0 := 0;
      Long0 := 0;
      false_east := 0;
      false_north := 0;
      a := NaN;
      e := NaN;
      h_f := NaN;
      Phi1 := NaN;
      Phi2 := NaN;

   DefineWGS84;

   {$If Defined(LongCent) or Defined(track_f)} ShortProjInfo('tMapProjection.Create Out'); {$EndIf}
end;


procedure tMapProjection.ProjectionParamsToDebugFile(Why : shortstring; Full : boolean = false);
var
   sl : tStringList;
   i : integer;
begin
    sl := ProjParamsList(Full);
    if Full then begin
       WriteLineToDebugFile('');
       WriteLineToDebugFile(Why);
       WriteStringListToDebugFile(sl);
       WriteLineToDebugFile('');
    end
    else begin
       for I := 0 to pred(sl.count) do Why := Why + ' ' + sl.Strings[i];
       WriteLineToDebugFile(Why);
    end;
    sl.Destroy;
end;


function tMapProjection.ProjParamsList(Full : boolean = false) : tStringList;
begin
   Result := tStringList.Create;
   Result.Add('Map Projection parameters ' + GetProjName);
   if not (PName in [PlateCaree]) then begin
      Result.Add(FalseSettingsString + ' a=' + RealToString(a,-18,-2) + ' f=' + RealToString(h_f,-18,-6));
   end;
   if PName in [LambertConformalConicEllipse,AlbersEqAreaConicalEllipsoid] then begin
      if not(IsNAN(Phi1) and IsNAN(Phi2)) then Result.Add('Phi1: ' + RadToDegString(Phi1) + ' Phi2: ' + RadToDegString(Phi2));
   end;
   if PName in [OldStereographic,SphericalStereographic] then begin
      Result.Add('k0: '+ RealToString(ProjMapScale,-12,-8));
      Result.Add('Phi2: ' + RealToString(Phi2/Petmar_types.DegToRad,-12,-6));
   end;

   if Full then begin
      if pName = AlbersEqAreaConicalEllipsoid then begin
          Result.Add('m1=' + RealToString(m1,-12,-6) + '   m2=' + RealToString(m2,-12,-6));
          Result.Add('q1=' + RealToString(q1,-12,-6) + '   q2=' + RealToString(q2,-12,-6));
          Result.Add('nalbers=' + RealToString(nalbers,-12,-6));
          Result.Add('c=' + RealToString(c,-12,-6));
          Result.Add('rho0=' + RealToString(rho0,-12,-6));
      end;
      if PName in [LambertConformalConicEllipse] then begin
          if not(IsNAN(m1) and IsNAN(m2)) then Result.Add('m1=' + RealToString(m1,-12,-6) + '   m2=' + RealToString(m2,-12,-6));
          if not IsNAN(rho0) then Result.Add('rho0=' + RealToString(rho0,-12,-6));
          Result.Add('t0=' + RealToString(t0,-12,-6) + '   t1=' + RealToString(t1,-12,-6) + '   t2=' + RealToString(t2,-12,-6));
          if not IsNAN(n_lcc) then Result.Add('n_lcc=' + RealToString(n_lcc,-12,-6));
          if not IsNAN(Big_F) then Result.Add('Big_F=' + RealToString(Big_F,-12,-6));
      end;
      if PName in [LamAzEqAreaEllipsoidal] then begin
          Result.Add('q1=' + RealToString(q1,-12,-8) + '   q2=' + RealToString(q2,-12,-8));
          Result.Add('Beta1=' + RealToString(Beta1/DegToRad,-12,-8));
          Result.Add('rq=' + RealToString(rq,-12,-8));
          Result.Add('m1=' + RealToString(m1,-12,-8));
          Result.Add('D=' + RealToString(D,-12,-8));
      end;
   end;
end;



{$IfDef RecordProblems}
   procedure tMapProjection.WriteProjParamsToDebugFile(Title : shortString; Full : boolean = false);
   var
      TStr : tStringList;
   begin
      TStr := ProjParamsList(Full);
      WriteLineToDebugFile(Title);
      WriteStringListToDebugFile(Tstr);
      TStr.Free;
   end;
{$EndIf}


function tMapProjection.UTMZoneString : shortstring;
begin
   Result := IntToStr(projUTMZone) + LatHemi;
end;


procedure tMapProjection.GetProjectParameters;
var
  q0 : float64;
begin
   {$IfDef RecordProjectionParameters} ShortProjInfo(' GetProjectParameters in'); {$EndIf}
      if (a = NaN) then a := SphericalEarthAkm * 1000;

      if PName in [UTMEllipsoidal,UK_OS,Finn_GK,GeneralTransverseMercator,IrishGrid,AlbersEqAreaConicalEllipsoid] then begin
        {$IfDef RecordProjectionParameters} WriteLineToDebugFile('Transerve Mercator'); {$EndIf}
         FullWorld := false;
         if (Lat0 = NaN) then Lat0 := 0;
         if (false_north = NaN) then false_north := 0;
         if (PName = UTMEllipsoidal) then begin
            {$IfDef RecordProjectionParameters} WriteLineToDebugFile('UTMEllipsoidal'); {$EndIf}
            if (ProjMapScale = NaN) then ProjMapScale := 0.9996;
            false_east := 500000;
            if (LatHemi = 'S') then false_north := 10000000;
         end
         else if (PName in [AlbersEqAreaConicalEllipsoid]) then begin
//PROJCS["NAD83 / Conus Albers",GEOGCS["NAD83",DATUM["North_American_Datum_1983",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],
//TOWGS84[0,0,0,0,0,0,0],AUTHORITY["EPSG","6269"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4269"]],
//PROJECTION["Albers_Conic_Equal_Area"],PARAMETER["standard_parallel_1",29.5],PARAMETER["standard_parallel_2",45.5],PARAMETER["latitude_of_center",23],PARAMETER["longitude_of_center",-96],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["X",EAST],AXIS["Y",NORTH],AUTHORITY["EPSG","5070"]]
            lat0 := 23 * Petmar_types.DegToRad;
            long0 := (-96) * Petmar_types.DegToRad;
            Phi1 := 29.5 * Petmar_types.DegToRad;
            Phi2 := 45.5 * Petmar_types.DegToRad;
         end
         else if (PName = UK_OS) then begin
            {$If Defined(RecordProjectionParameters) or Defined(RecordUKOS)} WriteLineToDebugFile('GetProjectParameters, UK_OS'); {$EndIf}
            ProjMapScale := 0.9996012717;
            Lat0 := 49 * Petmar_types.DegToRad;
            Long0 := -2 * Petmar_types.DegToRad;
            false_east := 400000;
            false_north := -100000;
            h_datumcode  := 'OGB-M';
         end
         else if (PName = IrishGrid) then begin
            ProjMapScale := 1.0000357;
            Lat0 := 53.5 * Petmar_types.DegToRad;
            Long0 := -8 * Petmar_types.DegToRad;
            false_east := 200000;
            false_north := 250000;
            h_datumcode  := 'Airy';
         end
         else if (PName = Finn_GK) then begin
            h_datumcode  := 'Hayford';
            ProjMapScale := 1.00;
            Long0 := 27 * Petmar_types.DegToRad;
            false_east := 3500000;
         end;
         {$IfDef RecordFalse}  WriteLineToDebugFile(ProjDebugName + ' TM set, ' + FalseSettingsString); {$EndIf}
      end;

      if (ProjMapScale = Nan) then ProjMapScale := 0.9996;

     //{$IfDef RecordProjectionParameters} WriteLineToDebugFile('call SetTMConstants'); {$EndIf}
      SetDatumAndTMConstants;

      CosLatCent := cos(Lat0);
      SinLatCent := sin(Lat0);
      CosPhi1 := cos(Phi1);

      if (PName = CylindricalEqualAreaEllipsoidal) then begin
         k0 := cosPhi1 / sqrt(1 - e2 * sqr(sin(Phi1)));     //Snyder, equation 10-13
         //qp :=                                          //Snyder, equation 3-12
      end
      else if PName in [LambertConformalConicEllipse] then begin
         FullWorld := false;
         m1 := fm(Phi1);
         m2 := fm(Phi2);
         t0 := ft(Lat0);
         t1 := ft(Phi1);
         t2 := ft(Phi2);
         if abs(Phi1-Phi2) < 0.0001 then  n_lcc := 1
         else n_lcc := (ln(m1/m2)) / (ln(t1/t2));
         Big_F := m1 / n_lcc / Math.Power(t1,n_lcc);
         rho0 := A * Big_F * Math.Power(t0,n_lcc);
         {$IfDef DebugConformalConic} WriteLineToDebugFile('tMapProjection.GetProjectParameters  LCC LatCent=' + RealToString(Lat0/DegToRad,-12,-8) + '  LongCent=' + RealToString(Long0/DegToRad,-12,-8)); {$EndIf}
      end
      else if PName in [PolarStereographicEllipsoidal] then begin
           if (Lat0 >= 0) then begin
              tc := Math.Tan(QuarterPi - 0.5 * Lat0) / Math.Power( (1 - e * sin(Lat0)) / (1 + e * sin(Lat0)),0.5 * e);
              mc := cos(Lat0) / sqrt(1 - e2 * sqr(sin(Lat0)));
           end
           else begin
              tc := Math.Tan(QuarterPi + 0.5 * Lat0) / Math.Power( (1 - e * sin(-Lat0)) / (1 + e * sin(-Lat0)),0.5 * e);
              mc := cos(-Lat0) / sqrt(1 - e2 * sqr(sin(-Lat0)));
           end;
      end
      else if PName in [LamAzEqAreaEllipsoidal] then begin
         FullWorld := false;
         q1 := fq(Lat0);
         q2 := fq(90*DegToRad);
         beta1 := arcsin(q1/q2);
         rq := a * sqrt(q2/2);
         m1 := fm(Lat0);
         D := a * m1 / (rq * cos(beta1));
      end
      else if PName in [AlbersEqAreaConicalEllipsoid] then begin
         FullWorld := false;
         m1 := fm(Phi1);
         m2 := fm(Phi2);
         if abs(Phi1 - Phi2) < 0.0001 then begin
            q0 := 2 * sin(Phi1);
            q1 := q0;
            nalbers := sin(Phi1);
         end
         else begin
            q0 := fq(Lat0);
            q1 := fq(Phi1);
            q2 := fq(Phi2);
            nalbers := (sqr(m1) - sqr(m2)) / (q2 - q1);
         end;
         c := sqr(M1) + Nalbers * q1;
         Rho0 := a * sqrt(C - nalbers * q0) / nalbers;
      end;
     {$If Defined(track_f) or Defined(RecordProjectionParameters)} ShortProjInfo(' GetProjectParameters Out'); {$EndIf}
     {$IfDef RecordFalse} WriteLineToDebugFile(ProjDebugName + ' GetProjectParameters Out, ' + FalseSettingsString); {$EndIf}
end {proc GetProjectParameters};



function tMapProjection.TissotEnabled : boolean;
begin
   Result := PName in [AlbersEqAreaConicalEllipsoid,LambertConformalConicEllipse, Mercator,WebMercator,MercatorEllipsoid,UTMEllipsoidal,GeneralTransverseMercator,
         OrthoProj,LamAzEqArea,CylindricalEqualArea,SphericalStereographic,OldStereographic,MillerCylindrical,Gnomonic,SinusProj,Cassini,EquiDistantCylindrical];
  //disabled for Cassini,EquiDistantCylindrical,Mollweide
end;


function tMapProjection.ConformalProjection : boolean;
begin
   Result := UTMLikeProjection or (PName in [WebMercator,Mercator,MercatorEllipsoid,OldStereographic,SphericalStereographic,LambertConformalConicEllipse]);
end;


function tMapProjection.UTMLikeProjection : boolean;
begin
   Result := PName in [UTMEllipsoidal,UK_OS,Finn_GK,GeneralTransverseMercator,IrishGrid];
end;


function tMapProjection.EllipsoidalProjection : boolean;
begin
   Result := PName in [MercatorEllipsoid,AlbersEqAreaConicalEllipsoid,UTMEllipsoidal,SinusEllipsoidal,LambertConformalConicEllipse,UK_OS,Finn_GK,GeneralTransverseMercator,PolarStereographicEllipsoidal,IrishGrid];
end;

(*
function tMapProjection.CylindricalProjection : boolean;
begin
   Result := false;
end;
 *)

function tMapProjection.GetProjName : shortstring;
begin
   case PName of
      PlateCaree  : Result := 'Plate caree';
      Gnomonic    : Result := 'Gnomonic';
      OrthoProj   : Result := 'Orthographic';
      SinusProj   : Result := 'Sinusoidal';
      HammerProj  : Result := 'Hammer';
      LamAzEqArea : Result := 'Lambert Azimuthal Equal Area';
      LamAzEqAreaEllipsoidal : Result := 'Lambert Azimuthal Equal Area (Ellipse)';
      Mollweide   : Result := 'Mollweide';
      OldStereographic : Result := 'Stereographic';
      SphericalStereographic : Result := 'Spherical stereographic';
      WebMercator   : Result := 'Web Mercator';
      Mercator      : Result := 'Mercator';
      Cassini       : Result := 'Cassini';
      VanDerGrinten : Result := 'Van der Grinten';
      EquiDistantCylindrical : Result := 'Equidistant cylindrical, std parallel=' + RadToDegString(Phi1);
      AlbersEqAreaConicalEllipsoid : Result := 'Albers Equal Area Conic (ellipse)';
      MercatorEllipsoid : Result := 'Mercator (ellipsoidal)';
      PolarStereographicEllipsoidal : Result := 'Polar Stereographic (ellipse)';
      UTMEllipsoidal : Result := 'UTM (ellipse), zone=' + UTMZoneString;
      SinusEllipsoidal : Result := 'Sinusoidal (ellipsoidal)';
      MillerCylindrical :  Result := 'Miller cylindrical';
      CylindricalEqualArea :  Result := 'Cylindrical equal area ' + pNameModifier;
      CylindricalEqualAreaEllipsoidal :  Result := 'Cylindrical equal area (ellipsoidal) ' + pNameModifier;
      LambertConformalConicEllipse :  Result := 'Lambert Conformal Conic';
      AzimuthalEquidistantEllipsoidal : Result := 'Azimuthal Equidistant Ellipsoidal';
      UK_OS : Result := 'British National Grid';
      IrishGrid : Result := 'Irish Grid';
      Finn_GK : Result := 'Finnish Gauss-Kruger';
      GeneralTransverseMercator : Result := 'Transverse Mercator';
      EqualEarth : Result := 'Equal Earth';
      UndefinedProj : Result := 'Undefined';
      else Result := 'Undefined ' + IntToStr(ord(PName));
   end {Case};
   //Result := Result +  '  Datum=' + h_DatumCode;
end;

initialization
{$IfDef MessageStartUpUnitProblems} MessageToContinue('demdatum initialization'); {$EndIf}
   ReallyReadDEM := true;
   ReportErrors := true;
   DEMMergeInProgress := false;
   UseDefaultDatumZone := false;
   WGS84DatumConstants := tMapProjection.Create('WGS84 datum constants');;
   RedefineWGS84DatumConstants(-1);
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demdatum in'); {$EndIf}
   WGS84DatumConstants.Destroy;
end.


