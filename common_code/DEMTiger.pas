{$F+,O+}

unit DEMTiger;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}
    {$IfDef Debug}
       //{$Define RecordTiger}
       //{$Define RecordTIGERMasking}
       //{$Define RecordTigerIndexAll}
       //{$Define RecordTigerIndex}
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

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations

   {$IfDef VCL}
      Forms,Graphics,
      DEMMapF,
   {$EndIf}

   Classes,SysUtils,StrUtils,
   PETMAR,Petmar_types,PetDBUtils,PetImage,
   DEMESRIShapeFile,
   DEMDataBase,
   DEMDefs,
   DEMMapDraw;

var
   TigerIndex : PathStr;
   //RoadMaskColor : tColor;

procedure FindTIGERinBox(bb : sfBoundBox;  var TigerNames : tStringList);
function GetTIGERCounty(var fNames : TStringList; var inLat1,inLong1,inLat2,inLong2 : float64; var CountyName : ShortString) : boolean;

procedure IndexTigerFiles;
procedure ShortenTigerName(var FEName : ShortString);


implementation

uses
   DataBaseCreate,
   Pick_county,
   US_properties,
   Make_Tables,
   DEM_Manager,
   DEMDef_routines,
   PetMath;


procedure ShortenTigerName(var FEName : ShortString);
begin
   if Copy(FEName,1,7) = 'BIA Rd ' then System.Delete(FEName,1,7);
   if Copy(FEName,1,25) = 'Natl Forest Develop Road ' then System.Delete(FEName,1,25);
   if Copy(FEName,1,21) = 'United States Highway' then begin
      System.Delete(FEName,1,22);
      FEName := 'US ' + FEName;
   end;
end;


procedure IndexTigerFiles;
var
   fname,DirStr : PathStr;
   Count, Year  : integer;
   TStr : ShortString;
   i,GISNum,IndexNum : integer;
   bBox : sfBoundBox;
   {$IfDef RecordTigerIndex}
      TheFiles : tStringList;
   {$EndIf}
begin
   {$IfDef RecordTigerIndex} WriteLinetoDebugFile('IndexTigerFiles in, ' + TigerIndex); {$EndIf}
   if FileExists(TigerIndex) then begin
      DirStr := MainMapData + 'tiger_shapes\';
      OpenNumberedGISDataBase(IndexNum,TigerIndex);
      GISdb[IndexNum].ClearGISFilter;
      StartProgress('Index');
      SaveBackupDefaults;
      Count := 0;
      i := 0;
      StartProgress('TIGER index');
      while not GISdb[IndexNum].MyData.EOF do begin
         TStr := 'N';
         if (i mod 25 = 0) then UpdateProgressBar(i/GISdb[IndexNum].MyData.FiltRecsInDB);
         inc(i);
         GISdb[IndexNum].MyData.Edit;
         GISdb[IndexNum].MyData.SetFieldByNameAsString('ON_HAND','N');
         GISdb[IndexNum].MyData.SetFieldByNameAsString('FILENAME','');
         for Year := 2020 downto 2007 do begin
            fName := DirStr + GISdb[IndexNum].MyData.GetFieldByNameAsString('STATE') + '\tl_' + IntToStr(Year) + '_' + GISdb[IndexNum].MyData.GetFieldByNameAsString('FIPS') + '_edges.shp';
            if FileExists(fName) then begin
               TStr := 'Y';
               DesiredDBMode := dbmForceDefault;
               {$IfDef RecordTigerIndex} WriteLinetoDebugFile('Found ' + ExtractFileName(fName)); {$EndIf}
               if OpenNumberedGISDataBase(GISNum,fName) then begin
                  if not GISdb[GISNum].LatLongCornersPresent then begin
                     {$IfDef RecordTigerIndex} WriteLinetoDebugFile('Add record bounding boxes ' + ExtractFileName(fName)); {$EndIf}
                     GISdb[GISNum].aShapefile.AddFields(afBoundingBox,GISdb[GISNum].MyData);
                  end;
                  bBox := GISdb[GISNum].aShapefile.MainFileHeader.BoundBox;
                  CloseSingleDB(GISNum);
                  inc(Count);
                  GISdb[IndexNum].MyData.SetFieldByNameAsString('ON_HAND',TStr);
                  GISdb[IndexNum].MyData.SetFieldByNameAsString('FILENAME',fName);
                  if (TStr = 'Y') then GISdb[IndexNum].MyData.SetRecordBoundingBox(bBox);
               end;
               break;
            end;
         end;
         GISdb[IndexNum].MyData.Next;
      end;
      CloseSingleDB(IndexNum);
   end
   else MessageToContinue('No Tiger index: ' + TigerIndex);
   EndProgress;
   RestoreBackupDefaults;
   {$IfDef RecordTigerIndex} WriteLinetoDebugFile('IndexTigerFiles out; found n=' + IntToStr(Count)); {$EndIf}
end;



function GetTIGERCounty(var fNames : TStringList; var inLat1,inLong1,inLat2,inLong2 : float64; var CountyName : ShortString) : boolean;
var
   pc : Tpickcounty;
   FIPS,err : integer;
begin
   {$IfDef RecordTiger} WriteLineToDebugFile('GetTIGERCounty in'); {$EndIf}
   if not FileExists(DEMTiger.TigerIndex) then begin
      Result := false;
      {$IfDef RecordTiger} WriteLineToDebugFile('Tiger Missing Index=' + DEMTiger.TigerIndex); {$EndIf}
      MessageToContinue('Tiger Missing Index=' + DEMTiger.TigerIndex);
      exit;
   end;
   CountyName := '';
   pc := Tpickcounty.Create(application);
   if (FNames = Nil) then begin
      if (PC.Table1.RecordCount = 1) then begin
         pc.TigerName := '';
         Result := true;
      end
      else begin
         pc.ShowModal;
         Result := pc.Picked;
      end;
   end
   else begin
      pc.TigerName := 'in';
      Result := true;
   end;

   if Result then begin
      if (pc.TigerName = '') then begin
         fNames := tStringList.Create;
         fNames.Add(MainMapData + 'tiger\' + pc.Table1.GetFieldByNameAsString('FILENAME'));
         CountyName := pc.Table1.GetFieldByNameAsString('NAME')
      end
      else begin
         if (FNames = Nil) then begin
            fNames := tStringList.Create;
            fNames.Add(pc.TigerName);
            val(Copy(ExtractFileName(pc.TigerName),4,5),FIPS,err);
         end
         else begin
            val(Copy(ExtractFileName(fNames[0]),4,5),FIPS,err);
         end;
      end;
      inlat1 := pc.Table1.GetFieldByNameAsFloat('LAT_LOW');
      inLong1 := pc.Table1.GetFieldByNameAsFloat('LONG_LOW');
      inLat2 := pc.Table1.GetFieldByNameAsFloat('LAT_HI');
      InLong2 := pc.Table1.GetFieldByNameAsFloat('LONG_HI');
   end;
   pc.Table1.Destroy;
   pc.Free;
   {$IfDef RecordTiger} WriteLineToDebugFile('GetTIGERCounty out, with files:'); WriteStringListToDebugFile(fNames); {$EndIf}
end;


procedure FindTIGERinBox(bb : sfBoundBox; var TigerNames : tStringList);
{using database of TIGER county files, determines counties within box defined by lat/long}
label
   Restart;
var
   TigerDB : integer;
   fName : PathStr;
begin
   {$IfDef RecordTigerIndex} WriteLineToDebugFile('Finding all TIGER in box Tiger index= '+ TigerIndex + ' ' + OpenDBString + '  ' + sfBoundBoxToString(bb)); {$EndIf}
   TigerNames := tStringList.Create;
   Restart:;
   if FileExists(TigerIndex) and OpenNumberedGISDataBase(TigerDB,TigerIndex) then begin
      {$IfDef RecordTigerIndex} WriteLineToDebugFile('Recs in TIGER database: ' + IntToStr(GISdb[TigerDB].MyData.RecordCount)); {$EndIf}

      GISdb[TigerDB].dbOpts.MainFilter := 'ON_HAND=' + QuotedStr('Y');
      GISdb[TigerDB].AssembleGISFilter;
      {$IfDef RecordTigerIndex} WriteLineToDebugFile('Onhand recs in TIGER database: ' + IntToStr(GISdb[TigerDB].MyData.RecordCount)); {$EndIf}

      GISdb[TigerDB].ApplyGISFilter(MakeGeoFilterFromBoundingBox(bB));
      {$IfDef RecordTigerIndex} WriteLineToDebugFile('Filter: ' + GISdb[TigerDB].MyData.Filter); {$EndIf}

      GISdb[TigerDB].MyData.First;
      while not GISdb[TigerDB].MyData.EOF do begin
         fName := GISdb[TigerDB].MyData.GetFieldByNameAsString('FILENAME');
         if FileExists(FName) then begin
            TigerNames.Add(fName);
            {$IfDef RecordTigerIndexAll} WriteLinetoDebugFile(fName); {$EndIf}
         end
         else begin
            GISdb[TigerDB].MyData.Edit;
            GISdb[TigerDB].MyData.SetFieldBynameAsString('FILENAME','');
         end;
         GISdb[TigerDB].MyData.Next;
      end;
      CloseSingleDB(TigerDB);
   end
   else begin
      {$IfDef RecordTigerIndex} WriteLineToDebugFile('No Tiger index ' + TigerIndex); {$EndIf}
   end;
   {$IfDef RecordTigerIndex} WriteLinetoDebugFile('Find all TIGER done ' + OpenDBString); {$EndIf}
end;


initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demtiger in'); {$EndIf}
end.



