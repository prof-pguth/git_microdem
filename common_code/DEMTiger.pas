{$F+,O+}

unit DEMTiger;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}
    {$IfDef Debug}
       //{$Define RecordTiger}
       //{$Define RecordRedistrict}
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

procedure RedistrictTigerFiles(fName : PathStr);
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
                  CloseAndNilNumberedDB(GISNum);
                  inc(Count);
                  GISdb[IndexNum].MyData.SetFieldByNameAsString('ON_HAND',TStr);
                  GISdb[IndexNum].MyData.SetFieldByNameAsString('FILENAME',fName);
                  if (TStr = 'Y') then GISdb[IndexNum].MyData.SetBoundingBox(bBox);
               end;
               break;
            end;
         end;
         GISdb[IndexNum].MyData.Next;
      end;
      CloseAndNilNumberedDB(IndexNum);
   end
   else MessageToContinue('No Tiger index: ' + TigerIndex);
   EndProgress;
   RestoreBackupDefaults;
   {$IfDef RecordTigerIndex} WriteLinetoDebugFile('IndexTigerFiles out; found n=' + IntToStr(Count)); {$EndIf}
end;


procedure RedistrictTigerFiles(fName : PathStr);
var
   fName1,fName2,
   BaseDir : PathStr;
   Line1,Line2 : AnsiString;
   FileList1,FileList2 : tStringList;
   Census2020,Census2010,Blocks : boolean;
   StateCode : shortstring;
   GISNum,Count : integer;
   f1,f2 : system.Text;

      procedure ProcessLine;
      var
         TStr : ShortString;
         Value,i : integer;
         Tract,Block,BlkGrp : ShortString;
         NeededFilter : ANSIString;
      begin
         with GISdb[GISNum],MyData do begin
            Filter := '';
            if Census2010 then begin
               Tract := ptTrim(Copy(Line1,55,6));
               BlkGrp := ptTrim(Copy(Line1,62,1));
               Block := ptTrim(Copy(Line1,62,4));
               if Blocks then NeededFilter := 'TRACTCE10 = ' + QuotedStr(Tract) + ' AND BLOCKCE10 = ' + QuotedStr(Block)
               else begin
                  if (Tract = '') and (BlkGrp = '') then NeededFilter := ''
                  else begin
                     if (Tract = '') then NeededFilter := 'BLKGRPCE10 = ' + QuotedStr(BlkGrp)
                     else if (BlkGrp = '') then NeededFilter := 'TRACTCE10= ' + QuotedStr(Tract)
                     else NeededFilter := 'TRACTCE10= ' + QuotedStr(Tract) + ' AND BLKGRPCE10 = ' + QuotedStr(BlkGrp);
                  end;
               end;
            end
            else begin
               Tract := Copy(Line1,56,6);
               Block := ptTrim(Copy(Line1,63,4));
               if Blocks then NeededFilter :=  'TRACTCE00 = ' + QuotedStr(Tract) + ' AND BLOCKCE00 = ' + QuotedStr(Block)
               else NeededFilter := 'TRACTCE00 = ' + QuotedStr(Tract);
            end;
            if (NeededFilter <> '') then begin
               MyData.ApplyFilter(NeededFilter);
               if (RecordCount = 1) then begin
                  Edit;
                  for i := 1 to 5 do TStr := BeforeSpecifiedCharacterANSI(Line2,',',true,true);
                  Value := StrToInt(BeforeSpecifiedCharacterANSI(Line2,',',true,true));  //there are 5 header fields, then total pop, P1-1
                  SetFieldByNameAsInteger('POP',GetFieldByNameAsInteger('POP') + Value);
                  for i := 1 to 1 do TStr := BeforeSpecifiedCharacterANSI(Line2,',',true,true);
                  Value := StrToInt(BeforeSpecifiedCharacterANSI(Line2,',',true,true));  //after population of just 1 race, we get whitle only, P1-3
                  SetFieldByNameAsInteger('WHITE1',GetFieldByNameAsInteger('WHITE1') + Value);
                  Value := StrToInt(BeforeSpecifiedCharacterANSI(Line2,',',true,true));                     //black only follows, P1-4
                  SetFieldByNameAsInteger('BLACK1',GetFieldByNameAsInteger('BLACK1') + Value);
                  for i := 1 to 1 do TStr := BeforeSpecifiedCharacterANSI(Line2,',',true,true);
                  Value := StrToInt(BeforeSpecifiedCharacterANSI(Line2,',',true,true));
                  SetFieldByNameAsInteger('ASIAN1',GetFieldByNameAsInteger('ASIAN1') + Value);
                  for i := 1 to 66 do TStr := BeforeSpecifiedCharacterANSI(Line2,',',true,true);
                  Value := StrToInt(BeforeSpecifiedCharacterANSI(Line2,',',true,true)); //skip P1-5 to P1-71,P2-1, and read P2-2, Hispanic
                  SetFieldByNameAsInteger('HISPANIC',GetFieldByNameAsInteger('HISPANIC') + Value);
                  Post;
               end;
            end;
         end;
      end;

var
   Table1,Table2 : tMyData;
   i : integer;
begin
   {$IfDef RecordRedistrict} WriteLineToDebugFile('RedistrictTigerFiles In'); {$EndIf}
   OpenNumberedGISDataBase(GISNum,fName);
   StateCode := 'md';
   PetMar.GetString('2 digit state code',StateCode,false,['a'..'z','A'..'Z']);

   Census2010 := StrUtils.AnsiContainsText(fName,'tl_2010');
   Census2020 := StrUtils.AnsiContainsText(fName,'tl_2020');

   with GISdb[GISNum] do begin
     AddFieldToDataBase(ftInteger,'POP',9);
     AddFieldToDataBase(ftInteger,'WHITE1',9);
     AddFieldToDataBase(ftInteger,'BLACK1',9);
     AddFieldToDataBase(ftInteger,'ASIAN1',9);
     AddFieldToDataBase(ftInteger,'HISPANIC',9);
   end;

   BaseDir := ExtractFilePath(fName);
   if Census2020 then begin
      FName1 := BaseDir + StateCode + 'geo2020.pl.dbf';
      FName2 := BaseDir + StateCode + '000012020.pl.dbf';
   end
   else if Census2010 then begin
      FName1 := BaseDir + StateCode + 'geo2010.pl';
      FName2 := BaseDir + StateCode + '000012010.pl';
   end
   else begin
      FName1 := BaseDir + StateCode + 'geo.upl';
      FName2 := BaseDir + StateCode + '00001.upl';
   end;

   if not (FileExists(fName1) and FileExists(fName2)) then begin
      MessageToContinue('Missing ' + ExtractFileName(fName1) + ' and ' + ExtractFileName(fName2));
   end
   else begin
       Blocks := StrUtils.AnsiContainsText(fName,'tabblock');
       GISdb[GISNum].MyData.First;
       while Not GISdb[GISNum].MyData.EOf do begin
          GISdb[GISNum].MyData.Edit;
          GISdb[GISNum].MyData.SetFieldByNameAsInteger('POP',0);
          GISdb[GISNum].MyData.SetFieldByNameAsInteger('WHITE1',0);
          GISdb[GISNum].MyData.SetFieldByNameAsInteger('BLACK1',0);
          GISdb[GISNum].MyData.SetFieldByNameAsInteger('ASIAN1',0);
          GISdb[GISNum].MyData.SetFieldByNameAsInteger('HISPANIC',0);
          GISdb[GISNum].MyData.Next;
       end;
       if Census2020 then begin
          Table2 := tMyData.Create(fName2);
          if not(Table2.FieldExists('TRACTCE20')) then begin
             {$IfDef RecordRedistrict} WriteLineToDebugFile('Add tracts/blocks'); {$EndIf}
             Table1 := tMyData.Create(fName1);
             Table2.InsureFieldPresentAndAdded(ftInteger,'TRACTCE20',8);
             Table2.InsureFieldPresentAndAdded(ftInteger,'BLOCKCE20',8);
             Table2.First;
             StartProgress('Add tracts and blocks');
             i := 0;
             while not Table2.eof do begin
                if i Mod 1000 = 0 then UpDateProgressBar(i/Table2.FiltRecsInDB);
                inc(i);
                Table2.Edit;
                Table2.SetFieldByNameAsString('TRACTCE20',Table1.GetFieldByNameAsString('TRACT'));
                Table2.SetFieldByNameAsString('BLOCKCE20',Table1.GetFieldByNameAsString('BLOCK'));
                Table1.Next;
                Table2.Next;
             end;
             Table1.Destroy;
          end;

          StartProgress('Add population');
          {$IfDef RecordRedistrict} WriteLineToDebugFile('Add population'); {$EndIf}
          GISdb[GISNum].MyData.First;
          i := 0;
          while not GISdb[GISNum].MyData.eof do begin
             if (i Mod 1000 = 0) then UpDateProgressBar(i/GISdb[GISNum].MyData.FiltRecsInDB);
             inc(i);
             Table2.ApplyFilter('TRACTCE20=' + IntToStr(GISdb[GISNum].MyData.GetFieldByNameAsInteger('TRACTCE20')) + ' AND BLOCKCE20=' + IntToStr(GISdb[GISNum].MyData.GetFieldByNameAsInteger('BLOCKCE20')));
             if (Table2.FiltRecsInDB > 0) then begin
                GISdb[GISNum].MyData.Edit;
                GISdb[GISNum].MyData.SetFieldByNameAsInteger('POP',Table2.GetFieldByNameAsInteger('P0010001'));
                GISdb[GISNum].MyData.SetFieldByNameAsInteger('WHITE1',Table2.GetFieldByNameAsInteger('P0010003'));
                GISdb[GISNum].MyData.SetFieldByNameAsInteger('BLACK1',Table2.GetFieldByNameAsInteger('P0010004'));
                GISdb[GISNum].MyData.SetFieldByNameAsInteger('ASIAN1',Table2.GetFieldByNameAsInteger('P0010006'));
                GISdb[GISNum].MyData.SetFieldByNameAsInteger('HISPANIC',Table2.GetFieldByNameAsInteger('P0020002'));
             end;
             GISdb[GISNum].MyData.Next;
          end;
          Table2.Destroy;
          EndProgress;
          {$IfDef RecordRedistrict} WriteLineToDebugFile('final act'); {$EndIf}
       end
       else begin
           GISdb[GISNum].MyData.First;
            if AnswerIsYes('Load data into memory (not for big states)') then begin
               FileList1 := tStringList.Create;
               FileList1.LoadFromFile(fName1);
               FileList2 := tStringList.Create;
               FileList2.LoadFromFile(fName2);
               StartProgress('Redistrict');
               for Count := 0 to pred(FileList1.Count) do begin
                  if (Count mod 100 = 0) then UpdateProgressBar(Count/FileList1.Count);
                  {$IfDef RecordRedistrict} if (Count mod 500 = 0) then  WriteLineToDebugFile('Recs=' + IntToStr(Count)); {$EndIf}
                  Line1 := FileList1.Strings[Count];
                  Line2 := FileList2.Strings[Count];
                  ProcessLine;
               end;
               EndProgress;
               FileList1.Free;
               FileList2.Free;
            end
            else begin
               assignFile(f1,fName1);
               reset(f1);
               assignFile(f2,fname2);
               reset(f2);
               count := 0;
               while not eof(f1) do begin
                  readln(f1,Line1);
                  readln(f2,Line2);
                  ProcessLine;
               end;
               CloseFile(f1);
               CloseFile(f2);
            end;
       end;
   end;
   CloseAndNilNumberedDB(GISNum);
   {$IfDef RecordRedistrict} WriteLineToDebugFile('RedistrictTigerFiles Out'); {$EndIf}
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
{using database of TIGER county files, determines counties within a box defined by lat/long}
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

      GISdb[TigerDB].ApplyGISFilter(MakeCornersGeoFilter(bB));
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
      CloseAndNilNumberedDB(TigerDB);
   end
   else begin
      MessageToContinue('No Tiger index ' + TigerIndex);
   end;
   {$IfDef RecordTigerIndex} WriteLinetoDebugFile('Find all TIGER done ' + OpenDBString); {$EndIf}
end;


initialization
   //RoadMaskColor := clBlack;
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demtiger in'); {$EndIf}
   {$IfDef RecordTiger} WriteLineToDebugFile('RecordTiger active in DEMTiger'); {$EndIf}
   {$IfDef RecordTigerIndex} WriteLineToDebugFile('RecordTigerIndex active in DEMTiger'); {$EndIf}
   {$IfDef RecordTIGERMasking} WriteLineToDebugFile('RecordTIGERMasking active in DEMTiger'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demtiger out'); {$EndIf}
end.



