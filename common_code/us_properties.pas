unit us_properties;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define USpropertiesTrack}
   //{$Define RecordDBClosing}
{$EndIf}


interface


uses
   SysUtils,
   {$IfDef VCL}
      Graphics,
   {$EndIf}
   DEMDataBase,
   DEMMapDraw,DEMMapf,
   Petmar_types,Petmar_db,Petmar,Petdbutils;


{$IfDef VCL}
   function GetCounty(Lat,Long : float64; var GeoName : ShortString) : boolean;
{$EndIf}


implementation

uses
   DEMDefs;

var
   Highways,
   County,
   Rivers,
   States : integer;


procedure DrawOutlines(var db : integer; fName : PathStr; dbMapOwner : DEMMapF.tMapForm; var Bitmap : tMyBitmap;  Color : tPlatformColor; Width : integer; FloodFill : boolean = false);
begin
   if not FileExists(fName) then fName := DBDir + fName;
   if not FileExists(fName) then exit;

   if (db <> 0) or OpenNumberedGISDataBase(db,fName) then with GISdb[db],dbOpts do begin
       {$IfDef VCL}
       if FloodFill then begin
          AreaSymbolFill := bsSolid;
          FillColor := Color;
          LineWidth := 1;
       end
       else begin
          AreaSymbolFill := bsClear;
          LineWidth := Width;
       end;
       LineColor := Color;
       theMapOwner := dbMapOwner;
       RedrawLayerOnMap;
       {$EndIf}
   end;
end;


{$IfDef VCL}
function GetUSPropertiesString(var db :integer; fName : PathStr; Field1,Field2,Field3 : ShortString; Label1,Label2 : ShortString; Lat,Long : float64; var USName : ShortString) : boolean;
begin
   {$IfDef USpropertiesTrack} WriteLineToDebugFile('GetUSPropertiesString ' + fName); {$EndIf}
   USName := 'Data base missing';
   Result := false;
   if not FileExists(fName) then fName := DBDir + fName;
   if not FileExists(fName) then exit;
   if (db <> 0) or  OpenNumberedGISDataBase(db,fName) then with GISdb[db],dbOpts do begin
      if FindAreaRecordWithPoint(Lat,Long,false) then begin
         USName := MyData.GetFieldByNameAsString(Field1) + Label1;
         if Field2 <> '' then USName := USName + '  ' + MyData.GetFieldByNameAsString(Field2) + Label2;
         if Field3 <> '' then USName := USName + '  ' + MyData.GetFieldByNameAsString(Field3);
         Result := true;
      end
      else USName := 'Point not in data base';
   end;
   {$IfDef USpropertiesTrack} WriteLineToDebugFile('GetUSPropertiesString got ' + USName); {$EndIf}
end;


function GetState(Lat,Long : float64; var GeoName : ShortString) : boolean;
begin
   Result := GetUSPropertiesString(States,StateGISFileName,'STATE','','','','',Lat,Long,GeoName);
end;


function GetCounty(Lat,Long : float64; var GeoName : ShortString) : boolean;
var
   n1,n2 : ShortString;
begin
   if (Uppercase(ExtractFileName(CountyGISFileName)) <> UpperCase('COUNTYP020' + DefaultDBExt)) then begin
      n1 := 'NAME';
      n2 := '';
   end
   else begin
      n1 := 'COUNTY';
      n2 := 'STATE';
   end;
   Result := GetUSPropertiesString(County,CountyGISFileName,n1,n2,'',',  ','',Lat,Long,GeoName);
end;

{$EndIf}


procedure CloseDataBases;
begin
   {$IfDef RecordDBClosing} WriteLineToDebugFile('Enter CloseDataBases in US_properties');{$EndIf}
   CloseSingleDB(County);
   CloseSingleDB(Rivers);
   CloseSingleDB(Highways);
   CloseSingleDB(States);
   {$IfDef RecordDBClosing} WriteLineToDebugFile('  exit CloseDataBases in US_properties'); {$EndIf}
end;


initialization
   County := 0;
   States := 0;
   Rivers := 0;
finalization
   CloseDataBases;
   {$IfDef USpropertiesTrack} WriteLineToDebugFile('USpropertiesTrack in US_properties'); {$EndIf}
   {$IfDef RecordDBClosing} WriteLineToDebugFile('RecordDBClosing in US_properties'); {$EndIf}
end.






