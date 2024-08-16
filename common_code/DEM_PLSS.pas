unit dem_plss;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
  //{$Define RecordPLSSTiming}
  //{$Define PLSSOpening}
  //{$Define RecordPLSS}
  //{$Define PLSSClosing}
  //{$Define RecordFont}
  //{$Define RecordPLSSDraw}
  //{$Define RecordTownshipName}
  //{$Define RecordPLSSSectionLabel}
  //{$Define RecordPLSSFields}
  //{$Define RecordPLSSLocation}
  //{$Define RecordPLSSQuarter}
  //{$Define RecordClosing}
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
//end for inline of core DB functions

   SysUtils,Forms,Controls,Windows,Classes,Graphics,StrUtils,
   demdatabase,PETMAR,Petmar_types,DEMDefs,DEMMapf,DEMMapDraw;


type tPLSSDataBase = class
   private
      function PLSSExists(dir : PathStr) : boolean;
   public
      ThisPLSSDir : PathStr;
      QuarterDB,
      SectionDB,
      TownshipDB  : integer;
      SectKey,QSectKey  : ShortString;
      function OpenFiles(MapOwner : tMapForm; fName : PathStr; CleanSections : boolean = false) : boolean;
      procedure CloseFiles;
      procedure SetPLSSDBoptions;
      function SimplePLSSLocation(Lat,Long : float64) : ShortString;
      function PLSSTownshipName(Table : tMyData) : shortstring;
      function PLSSTownshipNameFromLNDKEY(Table : tMyData) : shortstring;
      procedure DrawPLSS(MapDraw : tMapDraw; var Bitmap : tMyBitmap);
end;


const
   MaxPLSS = 5;
var
   PLSS : array[1..MaxPLSS] of tPLSSDataBase;

function TryToOpenPLSS(MapOwner : tMapForm) : boolean;
function PLSSLocation(Lat,Long : float64) : shortstring;
procedure CloseAllPLSS;
function TryToOpenOnePLSS(MapOwner : tMapForm; I : integer; fName : PathStr; CleanSections : boolean = false) : boolean;
function PLSSOpen : boolean;
procedure AssignMapOwnerToPLSS(MapOwner : tMapForm);
procedure PrepPLSS;
procedure PLSStoDebugFile;
function DecodeLandKey(MenuStr : ShortString) : shortstring;

implementation


uses
{Main program MDI window for different programs that use this module}
   Nevadia_Main,
{End of the MDI parent declaration}
   DEMESRIShapeFile,
   DEMdef_routines,
   PETdbUtils,
   PetImage,
   PETMath;

procedure PLSStoDebugFile;
var
   i : integer;
begin
   for i := 1 to 5 do if PLSSFile[i] <> '' then WriteLineToDebugFile('PLSS' + IntToStr(i) + '=' + PLSSFile[i]);
end;



procedure PrepPLSS;
var
   Paths : tStringList;
   i   : integer;
begin
   {$If Defined(RecordPLSS)} WriteLineToDebugFile('PrepPLSS in'); {$EndIf}
   Paths := tStringList.Create;
   Paths.Add(MainMapData);
   if GetMultipleDirectories('PLSS dirs',Paths) then begin
      StartProgress('PLSS dirs');
      for i := 0 to pred(Paths.Count) do begin
         UpdateProgressBar(i/Paths.Count);
         {$If Defined(RecordPLSS)} WriteLineToDebugFile(Paths.Strings[i]);   {$EndIf}
         TryToOpenOnePLSS(Nil,succ(i),Paths.Strings[i],true);
         CloseAllPLSS;
      end;
      EndProgress;
   end;
   Paths.Free;
end;


function TryToOpenOnePLSS(MapOwner : tMapForm; I : integer; fName : PathStr; CleanSections : boolean = false) : boolean;
begin
   Result := false;
   if ValidPath(fName) then begin
     if (PLSS[i] = Nil) then begin
       PLSS[i] := tPLSSDataBase.Create;
       Result := PLSS[i].OpenFiles(MapOwner,fname,CleanSections);
       if not Result then begin
          PLSS[i].Destroy;
          PLSS[i] := Nil;
       end;
       {$If Defined(RecordPLSS) or Defined(PLSSOpening)}
          if Result then WriteLineToDebugFile('TryToOpen success ' + IntToStr(i) + '  ' + fName + ' owner=' + MapOwner.Caption)
          else WriteLineToDebugFile('TryToOpen failed ' + IntToStr(i) + '  ' + fName);
       {$EndIf}
     end
     else begin
        Result := true;
        {$IfDef RecordPLSS} WriteLineToDebugFile('TryToOpen already open ' + IntToStr(i) + '  ' + fName); {$EndIf}
     end;
   end;
end;


function TryToOpenPLSS(MapOwner : tMapForm)  : boolean;
begin
  {$If Defined(RecordPLSS) or Defined(PLSSOpening)} WriteLineToDebugFile('TryToOpenPLSS in, ' +  MapOwner.Caption); {$EndIf}
   Result := TryToOpenOnePLSS(MapOwner,1,PLSSfile[1]);
   if Result then begin
      if TryToOpenOnePLSS(MapOwner,2,PLSSFile[2]) then
        if TryToOpenOnePLSS(MapOwner,3,PLSSFile[3]) then
           if TryToOpenOnePLSS(MapOwner,4,PLSSFile[4]) then
              TryToOpenOnePLSS(MapOwner,5,PLSSFile[5]);
      //MapOwner.DoCompleteMapRedraw;
   end;
  {$If Defined(RecordPLSS) or Defined(PLSSOpening)} WriteLineToDebugFile('TryToOpenPLSS complete'); {$EndIf}
end;


procedure CloseAllPLSS;
var
  i : integer;
begin
  {$If Defined(RecordPLSS) or Defined(PLSSClosing)} WriteLineToDebugFile('CloseAllPLSS in'); {$EndIf}
   for I := 1 to MaxPLSS do if (PLSS[i] <> Nil) then begin
      {$If Defined(RecordPLSS) or Defined(PLSSClosing)} WriteLineToDebugFile('CloseAllPLSS close PLSS=' +  IntToStr(i)); {$EndIf}
      PLSS[i].CloseFiles;
      PLSS[i] := Nil;
   end;
end;


function PLSSOpen : boolean;
var
  i : integer;
begin
   Result := false;
   for I := 1 to MaxPLSS do if (PLSS[i] <> Nil) then begin
      Result := true;
   end;
end;


procedure AssignMapOwnerToPLSS(MapOwner : tMapForm);
var
  i : integer;
begin
   for I := 1 to MaxPLSS do if (PLSS[i] <> Nil) then begin
       if PLSS[i].SectionDB <> 0 then begin
          GISdb[PLSS[i].SectionDB].TheMapOwner := MapOwner;
       end;
       if PLSS[i].TownshipDB <> 0 then begin
          GISdb[PLSS[i].TownshipDB].TheMapOwner := MapOwner;
       end;
       if PLSS[i].QuarterDB <> 0 then begin
          GISdb[PLSS[i].QuarterDB].TheMapOwner := MapOwner;
       end;
   end;
end;


function PLSSLocation(Lat,Long : float64) : shortstring;
var
   i : integer;
   cmd : ansistring;
   code : tstringList;
begin
   {$IfDef RecordPLSS} WriteLineToDebugFile('PLSSLocation'); {$EndIf}
   Result := '';

   for I := 1 to MaxPLSS do begin
     if (PLSS[i] <> Nil) then begin
        {$IfDef RecordPLSS} WriteLineToDebugFile('PLSS Num=' + IntToStr(i) + '  Town db=' + IntToStr(PLSS[i].TownshipDB) + '  Sect db=' + IntToStr(PLSS[i].SectionDB)); {$EndIf}
        Result := PLSS[i].SimplePLSSLocation(Lat,Long);
        if (Result <> '') then exit;
     end;
   end;

   if (Result = '') then begin
      cmd := 'https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/GetTRS?lat=' + RealToString(Lat,-12,-8) + '&lon=' + RealToString(Long,-12,-8) + '&units=DD&returnlevel=&returnalllevels=&f=html';
      if DownloadFileFromWeb(cmd,MDTempDir + 'PLSS.html') then begin
        Code := tStringList.Create;
        Code.LoadFromFile(MDTempDir + 'PLSS.html');
        for i := 0 to pred(Code.Count) do begin
           cmd := Code.Strings[i];
           //<b>landdescription</b>: CA210060S0360E0SN350ASWSE<br/></ul>
          // OR330320S0170E0SN130ASWNE
           if StrUtils.ANSIContainsText(cmd,'<b>landdescription</b>') then begin
              cmd := Petmar_types.AfterSpecifiedCharacter(cmd,':');
              Result := Petmar_types.BeforeSpecifiedCharacterANSI(cmd,'<');
              Result := DecodeLandKey(Result);
           end;
        end;
        Code.Free;
      end;
   end;
end;


const
   s84name = 'section_84.shp';
   q84name = 'quarter_84.shp';
   t84name = 'twnshp_84.shp';


procedure tPLSSDataBase.DrawPLSS(MapDraw : tMapDraw; var Bitmap : tMyBitmap);
const
   GeofilterForPLSS = true;

         procedure ClearPLSSfilters;
         begin
            if (QuarterDB <> 0) then GISdb[QuarterDB].ClearGISFilter;
            GISdb[SectionDB].ClearGISFilter;
            GISdb[TownshipDB].ClearGISFilter;
         end;


//var
  //TStr : ShortString;
begin
   {$IfDef RecordPLSSDraw} if (SectionDB <> 0) then WriteLineToDebugFile('+++++ tPLSSDataBase.DrawPLSS in, ' + GISdb[SectionDB].dbFullName); {$EndIf}
   //if (TownshipDB = 0) or (SectionDB = 0) or (QuarterDB = 0) then exit;
   ClearPLSSfilters;
   DrawPolygonsAsPolygons := false;

   if (MDDef.PLSSDef.PLSSShowQuarters) and (QuarterDB <> 0) then begin
      if (MDDef.PLSSDef.PLSSsmartScaling) and (MapDraw.ScreenPixelSize > MDDef.PLSSDef.PLSSAppearQuarters) then begin
         {$IfDef RecordPLSSDraw} WriteLineToDebugFile('quarters do not appear at pixel size ' + RealToString(MapDraw.ScreenPixelSize,-12,1)); {$EndIf}
      end
      else begin
         if GeofilterForPLSS then GISdb[QuarterDB].QueryBox(MapDraw,0,0,MapDraw.MapXSize,MapDraw.MapYSize,true);
         {$IfDef RecordPLSSDraw} WriteLineToDebugFile('tPLSSDataBase.DrawPLSS quarters recs=' + IntToStr(GISdb[QuarterDB].MyData.RecordCount) + ' filter='+ GISdb[QuarterDB].MyData.Filter); {$EndIf}
         MapDraw.PlotDataBase(GISdb[QuarterDB].DBNumber,Bitmap);
      end
   end;

   if (SectionDB <> 0) and (MDDef.PLSSDef.PLSSShowSections) then begin
      if (MDDef.PLSSDef.PLSSsmartScaling) and (MapDraw.ScreenPixelSize > MDDef.PLSSDef.PLSSAppearSections) then begin
         {$IfDef RecordPLSSDraw} WriteLineToDebugFile('sections do not appear at pixel size ' + RealToString(MapDraw.ScreenPixelSize,-12,1)); {$EndIf}
      end
      else begin
         {$IfDef RecordPLSSDraw} WriteLineToDebugFile('+++++ tPLSSDataBase.DrawPLSS do sections'); {$EndIf}
         {$IfDef RecordFont} WriteLineToDebugFile('tPLSSDataBase.DrawPLSS, sect font: ' + MyFontToString(GISdb[SectionDB].dbOpts.GisLabelFont1)); {$EndIf}
         {$IfDef RecordFont} WriteLineToDebugFile('tPLSSDataBase.DrawPLSS, sect color: ' + ColorStringFromPlatformColor(GISdb[SectionDB].dbOpts.LineColor)); {$EndIf}
         if GeofilterForPLSS then GISdb[SectionDB].QueryBox(MapDraw,0,0,MapDraw.MapXSize,MapDraw.MapYSize,true);
         MapDraw.PlotDataBase(GISdb[SectionDB].DBNumber,Bitmap);
         if MDDef.PLSSDef.PLSSLabelSections then GISdb[SectionDB].LabelRecordsOnMap(Bitmap);
         {$IfDef RecordPLSSDraw} WriteLineToDebugFile('tPLSSDataBase.DrawPLSS s1, recs: ' + IntToStr(GISdb[SectionDB].MyData.RecordCount) + '   filter: ' + GISdb[SectionDB].MyData.Filter); {$EndIf}
      end;
   end;

   if (TownshipDB  <> 0) and (MDDef.PLSSDef.PLSSShowTowns) then begin
      if (MDDef.PLSSDef.PLSSsmartScaling) and (MapDraw.ScreenPixelSize > MDDef.PLSSDef.PLSSAppearTowns) then begin
         {$IfDef RecordPLSSDraw} WriteLineToDebugFile('townships do not appear at pixel size ' + RealToString(MapDraw.ScreenPixelSize,-12,1)); {$EndIf}
      end
      else begin
         {$IfDef RecordFont} WriteLineToDebugFile('tPLSSDataBase.DrawPLSS, town font: ' + MyFontToString(GISdb[TownShipDB].dbOpts.GisLabelFont1)); {$EndIf}
         {$IfDef RecordFont} WriteLineToDebugFile('tPLSSDataBase.DrawPLSS, town color: ' + ColorStringFromPlatformColor(GISdb[TownshipDB].dbOpts.LineColor)); {$EndIf}
         if GeofilterForPLSS then GISdb[TownshipDB].QueryBox(MapDraw,0,0,MapDraw.MapXSize,MapDraw.MapYSize,true);
         {$IfDef RecordPLSSDraw} WriteLineToDebugFile('+++++ tPLSSDataBase.DrawPLSS township, recs: ' + IntToStr(GISdb[TownshipDB].MyData.RecordCount)); {$EndIf}
         MapDraw.PlotDataBase(GISdb[TownshipDB].DBNumber,Bitmap);
         if MDDef.PLSSDef.PLSSLabelTowns then GISdb[TownshipDB].LabelRecordsOnMap(Bitmap);
         {$IfDef RecordPLSSDaw} WriteLineToDebugFile('----- tPLSSDataBase.DrawPLSS township done'); {$EndIf}
      end;
   end;

   ClearPLSSfilters;
   DrawPolygonsAsPolygons := true;
   {$IfDef RecordPLSSDraw} WriteLineToDebugFile('tPLSSDataBase.DrawPLSS out'); {$EndIf}
end;


procedure tPLSSDataBase.SetPLSSDBoptions;
begin
   {$IfDef RecordFont} WriteLineToDebugFile('tPLSSDataBase.SetPLSSDBoptions, town font: ' + MyFontToString(MDDef.PLSSDef.TownFont)); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('tPLSSDataBase.SetPLSSDBoptions, town color: ' + ColorStringFromPlatformColor(MDDef.PLSSDef.PLSSTownColor)); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('tPLSSDataBase.SetPLSSDBoptions, sect font: ' +  MyFontToString(MDDef.PLSSDef.SectFont)); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('tPLSSDataBase.SetPLSSDBoptions, sect color: ' + ColorStringFromPlatformColor(MDDef.PLSSDef.PLSSSectionColor)); {$EndIf}

    if QuarterDB <> 0 then begin
       GISdb[QuarterDB].DoNotTrackDBSettings := true;
       GISdb[QuarterDB].PLSSfile := true;
       GISdb[QuarterDB].dbOpts.LineColor := MDDef.PLSSDef.PLSSQuarterColor;
       GISdb[QuarterDB].dbOpts.LineWidth := MDDef.PLSSDef.PLSSQuarterWidth;
       GISdb[QuarterDB].dbOpts.AreaSymbolFill := bsClear;
       GISdb[QuarterDB].dbOpts.LabelField := SectKey;
    end;
    if SectionDB <> 0 then begin
       GISdb[SectionDB].DoNotTrackDBSettings := true;
       GISdb[SectionDB].PLSSfile := true;
       GISdb[SectionDB].dbOpts.LineColor := MDDef.PLSSDef.PLSSSectionColor;
       GISdb[SectionDB].dbOpts.LineWidth := MDDef.PLSSDef.PLSSSectionWidth;
       GISdb[SectionDB].dbOpts.AreaSymbolFill := bsClear;
       GISdb[SectionDB].dbOpts.LabelDBPlots := MDDef.PLSSDef.PLSSLabelSections;
       GISdb[SectionDB].dbOpts.LabelField := 'SECTN';
       GISdb[SectionDB].dbOpts.GisLabelFont1 := MDDef.PLSSDef.SectFont;
    end;

    if TownshipDB <> 0 then begin
       GISdb[TownshipDB].DoNotTrackDBSettings := true;
       GISdb[TownshipDB].PLSSfile := true;
       GISdb[TownshipDB].dbOpts.LineColor := MDDef.PLSSDef.PLSSTownColor;
       GISdb[TownshipDB].dbOpts.LineWidth := MDDef.PLSSDef.PLSSTownWidth;
       GISdb[TownShipDB].dbOpts.AreaSymbolFill := bsClear;
       GISdb[TownShipDB].dbOpts.LabelDBPlots := MDDef.PLSSDef.PLSSLabelTowns;
       GISdb[TownShipDB].dbOpts.GisLabelFont1 := MDDef.PLSSDef.TownFont;
       GISdb[TownShipDB].dbOpts.LabelField := 'TOWN_NAME';
    end;
end;


function tPLSSDataBase.OpenFiles(MapOwner : tMapForm; fName : PathStr; CleanSections : boolean = false) : boolean;
var
   TStr : shortstring;
   sl : tStringList;

         procedure CleanSectionNames(DB : integer);
         begin
             {$IfDef RecordPLSS} WriteLineToDebugFile('CleanSectionNames, DB=' + IntToStr(DB)); {$EndIf}
             while not GISdb[DB].MyData.eof do begin
                GISdb[DB].MyData.Edit;
                TStr := GISdb[DB].MyData.GetFieldByNameAsString('SECTN');
                if (TStr <> '') then begin
                   while (TStr[1] = '0') and (Length(TStr) > 1) do System.Delete(TStr,1,1);
                   if (TStr = '') then TStr := '0';
                   GISdb[DB].MyData.SetFieldByNameAsString('SECTN',TStr);
                end;
                GISdb[DB].MyData.Next;
             end;
             {$IfDef RecordPLSS} WriteLineToDebugFile('Done'); {$EndIf}
         end;


begin
   {$IfDef RecordPLSS} WriteLineToDebugFile('tPLSSDataBase.OpenFiles ' + fName + ' owner=' + MapOwner.Caption); {$EndIf}
    ThisPLSSDir := fName;
    TownshipDB := 0;
    SectionDB := 0;
    QuarterDB := 0;
    Result := PLSSExists(ThisPLSSDir);
    if Result then begin
       CleanSections := CleanSections or (not FileExists(ThisPLSSDir + 'july_2020_update.txt'));
       wmDEM.SetPanelText(0,'Open PLSS files');

       if MDDef.PLSSDef.PLSSQuartersInLabels or MDDef.PLSSDef.PLSSShowQuarters then begin
          QSectKey := '';
          SectKey := '';
          if MDDef.PLSSDef.PLSStoRAM and (not CleanSections) then DesiredDBMode := dbmCDS;
          if OpenNumberedGISDataBase(QuarterDB,ThisPLSSDir + q84name,false,false,MapOwner) then begin
             if GISdb[QuarterDB].MyData.FieldExists('QQSECTION') then QSectKey := 'QQSECTION'
             else if GISdb[QuarterDB].MyData.FieldExists('QSECTN') then QSectKey := 'QSECTN';
             if GISdb[QuarterDB].MyData.FieldExists('SECTIONKEY') then SectKey := 'SECTIONKEY'
             else if GISdb[QuarterDB].MyData.FieldExists('SECTN') then SectKey := 'SECTN';
          end;
          if CleanSections then CleanSectionNames(QuarterDB);
       end;

       if MDDef.PLSSDef.PLSStoRAM  and (not CleanSections) then DesiredDBMode := dbmCDS;
       OpenNumberedGISDataBase(SectionDB,ThisPLSSDir + s84name,false,false,MapOwner);
       if CleanSections then begin
          CleanSectionNames(SectionDB);
          GISdb[SectionDB].TrimAllStringFields;
          sl := tStringList.Create;
          sl.Add('created ' + DateToStr(Now) + '  ' + TimeToStr(Now));
          sl.SaveToFile(ThisPLSSDir + 'july_2020_update.txt');
          sl.Destroy;
       end;

       if MDDef.PLSSDef.PLSStoRAM then DesiredDBMode := dbmCDS;
       OpenNumberedGISDataBase(TownshipDB,ThisPLSSDir + t84name,false,false,MapOwner);
       GISdb[TownShipDB].dbOpts.LabelField := 'TOWN_NAME';
       if not GISdb[TownshipDB].MyData.FieldExists(GISdb[TownShipDB].dbOpts.LabelField) then begin
          GISdb[TownshipDB].AddFieldToDataBase(ftstring,GISdb[TownShipDB].dbOpts.LabelField,12);
          GISdb[TownshipDB].MyData.First;
          while not GISdb[TownshipDB].MyData.eof do begin
             GISdb[TownshipDB].MyData.Edit;
             GISdb[TownshipDB].MyData.SetFieldByNameAsString(GISdb[TownShipDB].dbOpts.LabelField, PLSSTownshipName(GISdb[TownshipDB].MyData));
             GISdb[TownshipDB].MyData.Next;
          end;
       end;

     {$IfDef RecordPLSS}
        WriteLineToDebugFile('TownshipDB ' + SmartMemorySizeBytes(GetFileSize(ThisPLSSDir + t84name)) + ' recs=' + IntToStr(GISdb[TownshipDB].MyData.RecordCount));
        WriteLineToDebugFile('SectionDB ' + SmartMemorySizeBytes(GetFileSize(ThisPLSSDir + s84name)) + '  recs=' + IntToStr(GISdb[SectionDB].MyData.RecordCount));
        if (QuarterDB <> 0) then WriteLineToDebugFile('QuarterDB ' + SmartMemorySizeBytes(GetFileSize(ThisPLSSDir + q84name)) + '  recs=' + IntToStr(GISdb[QuarterDB].MyData.RecordCount));
     {$EndIf}
      SetPLSSDBoptions;
   end;
   {$IfDef RecordPLSS} WriteLineToDebugFile('tPLSSDataBase.OpenFiles out'); {$EndIf}
   wmDEM.SetPanelText(0,'');
end;


procedure tPLSSDataBase.CloseFiles;
begin
   {$If Defined(RecordPLSS) or Defined(PLSSClosing)} WriteLineToDebugFile('tPLSSDataBase.CloseFiles'); {$EndIf}
   CloseAndNilNumberedDB(TownshipDB);
   {$If Defined(RecordPLSS) or Defined(PLSSClosing)} WriteLineToDebugFile('Done towns'); {$EndIf}
   CloseAndNilNumberedDB(SectionDB);
   {$If Defined(RecordPLSS) or Defined(PLSSClosing)} WriteLineToDebugFile('Done Section'); {$EndIf}
   CloseAndNilNumberedDB(QuarterDB);
   {$If Defined(RecordPLSS) or Defined(PLSSClosing)} WriteLineToDebugFile('Done Quarter'); {$EndIf}
end;


function tPLSSDataBase.PLSSTownshipName(Table : tMyData) : shortstring;
var
   tStr,TStr2 : shortstring;
begin
   if (Table.RecordCount = 0) then Result := ''
   else begin
      TStr := Table.GetFieldByNameAsString('TOWN');
      while (TStr[1] = '0') do Delete(TStr,1,1);
      TStr2 := Table.GetFieldByNameAsString('TWNFRT');
      if (TStr2 = '0') then TStr2 := '' else TStr2 :='½';
      Result := 'T' + TStr + TStr2 + Table.GetFieldByNameAsString('TWNDIR') + ' R';
      TStr := Table.GetFieldByNameAsString('RANGE');
      while (TStr[1] = '0') do Delete(TStr,1,1);
      TStr2 := Table.GetFieldByNameAsString('RNGFRT');
      if (TStr2 = '0') then TStr2 := ''
      else if (TStr2 = '2') then TStr2 := '½'
      else TStr2 := '-3/4';
      Result := Result+ TStr + TStr2 + Table.GetFieldByNameAsString('RNGDIR');
   end;
end;


function DecodeLandKey(MenuStr : ShortString) : shortstring;
var
   tStr,TStr2 : shortstring;
begin
(*
----5----0----5----0----5
OR330320S0170E0SN130ASWNE   key from BLM web service
OR330310S0170E0SN340L13     key from BLM web service
OR33T0220S0130E             key in BLM shape files for quarters, sections
*)

   {$IfDef RecordPLSSTiming} WriteLineToDebugFile('LNDKey=' + MenuStr); {$EndIf}
   TStr := Copy(MenuStr,6,3);
   while TStr[1] = '0' do Delete(TStr,1,1);
   TStr2 := Copy(MenuStr,9,1);
   if TStr2 = '0' then TStr2 := '' else TStr2 :='½';
   Result := 'T' + TStr + TStr2 + MenuStr[10] + ' R';
   TStr := Copy(MenuStr,11,3);
   while TStr[1] = '0' do Delete(TStr,1,1);
   TStr2 := Copy(MenuStr,14,1);
   if TStr2 = '0' then TStr2 := '' else TStr2 :='½';
   Result := Copy(MenuStr,1,4) + '  ' + Result + TStr + TStr2 + MenuStr[15];
   {$IfDef RecordPLSSTiming} WriteLineToDebugFile('Result=' + Result); {$EndIf}
end;

function tPLSSDataBase.PLSSTownshipNameFromLNDKEY(Table : tMyData) : shortstring;
//var
   //tStr,TStr2 : shortstring;
  // MenuStr : ShortString;
begin
   {$IfDef RecordPLSSTiming} WriteLineToDebugFile('Arrive tPLSSDataBase.PLSSTownshipNameFromLNDKEY, Recs=' + IntToStr(Table.RecordCount)); {$EndIf}
   if (Table.RecordCount = 0) then Result := ''
   else begin
      //MenuStr := Table.GetFieldByNameAsString('LNDKEY');
      Result := DecodeLandKey(Table.GetFieldByNameAsString('LNDKEY'));
   end;
end;


function tPLSSDataBase.PLSSExists(dir : PathStr) : boolean;
const
   ladname = 'ladesc.shp';
   firname = 'first.shp';
   twnname = 'twnshp.shp';
var
   f1,f2,f3 : PathStr;


      procedure AddBoundingBoxToShapefile(fName : PathStr);
      var
         GISNum : integer;
      begin
         if OpenNumberedGISDataBase(GISNum,fName) then begin
            GISdb[GISNum].aShapeFile.AddFields(afBoundingBox,GISdb[GISNum].MyData);
            CloseAndNilNumberedDB(GISNum);
         end;
      end;


      procedure ProcessFile(f1,Newf1 : PathStr);
      begin
          {$IfDef RecordPLSS} WriteLineToDebugFile('start fix ' + newF1); {$EndIf}
          RepairDBaseFieldNames(f1);
          AddBoundingBoxToShapefile(f1);
          RenameShapeFile(f1,NewF1);
          {$IfDef RecordPLSS} WriteLineToDebugFile('done'); {$EndIf}
      end;

begin
   {$IfDef RecordPLSS} WriteLineToDebugFile('Enter tPLSSDataBase.PLSSExists for ' + Dir); {$EndIf}

   Result := true;
    if FileExists(Dir + s84name) and FileExists(Dir + q84name) and  FileExists(Dir + t84name) then begin
       exit;
    end
    else begin
       {$IfDef RecordPLSS}
       if not FileExists(Dir + s84name) then WriteLineToDebugFile('Missing ' + Dir + s84name);
       if not FileExists(Dir + q84name) then WriteLineToDebugFile('Missing ' + Dir + q84name);
       if not FileExists(Dir + t84name) then WriteLineToDebugFile('Missing ' + Dir + t84name);
       {$EndIf}
    end;

    f1 := Dir + ladname;
    if FileExists(f1) then begin
       f2 := Dir + firname;
       f3 := Dir + twnname;
    end
    else begin
       f1 := Dir + 'alt_' + ladname;
       f2 := Dir + 'alt_' + firname;
       f3 := Dir + 'alt_' + twnname;
    end;

    if FileExists(f1) and FileExists(f2) and FileExists(f3) then begin
       ProcessFile(f1,Dir + q84name);
       ProcessFile(f2,Dir + s84name);
       ProcessFile(f3,Dir + t84name);
       {$IfDef RecordPLSS} WriteLineToDebugFile('PLSS moved and fixed'); {$EndIf}
    end
    else begin
       Result := false;
       {$IfDef RecordPLSS}
          if not FileExists(f1) then WriteLineToDebugFile('Missing ' + Dir + ladname);
          if not FileExists(f2) then WriteLineToDebugFile('Missing ' + Dir + firname);
          if not FileExists(f3) then WriteLineToDebugFile('Missing ' + Dir + twnname);
       {$EndIf}
    end;
end;


function tPLSSDataBase.SimplePLSSLocation(Lat,Long : float64) : ShortString;
var
   QuarterString,SectionString,TownshipString,TStr : shortstring;
   UseFilter : string;
   //i : integer;

      function RestrictIfNeeded(var DataBase : TGISdataBaseModule) : boolean;
      label
         ReallyDifferent;
      var
         qq : ShortString;
      begin
         if (DataBase.MyData.eof) then Result := false
         else begin
            {$IfDef RecordPLSSQuarter}
               WriteLineToDebugFile('RestrictIfNeeded, count=' + IntToStr(DataBase.MyData.RecordCount),true);
               WriteLineToDebugFile('Filter=' + DataBase.MyData.Filter);
               if DataBase.MyData.FieldExists(QSectKey) then begin
                  DataBase.MyData.First;
                  while not DataBase.MyData.EOF do begin
                     WriteLineToDebugFile('QuarterString: ' + DataBase.MyData.GetFieldByNameAsString(QSectKey));
                     DataBase.MyData.Next;
                  end;
               end;
            {$EndIf}

           if DataBase.MyData.FieldExists(QSectKey) {and (DataBase.MyData.RecordCount > 1)} then begin
               DataBase.MyData.First;
               qq := DataBase.MyData.GetFieldByNameAsString(QSectKey);
               DataBase.MyData.Next;
               while not DataBase.MyData.EOF do begin
                  if (qq <> DataBase.MyData.GetFieldByNameAsString(QSectKey)) then goto ReallyDifferent;
                  DataBase.MyData.Next;
               end;
               Result := true;
               exit;
           end;

           ReallyDifferent:;
            {$IfDef RecordPLSSLocation} WriteLineToDebugFile(DataBase.dbName + ' found: ' + IntToStr(DataBase.MyData.RecordCount)); {$EndIf}
            Result := DataBase.FindAreaRecordWithPoint(Lat,Long,true);
            {$IfDef RecordPLSSQuarter} WriteLineToDebugFile('RestrictIfNeeded, out'); {$EndIf}
         end;
      end;


      procedure TownshipAndSection(var DataBase : TGISdataBaseModule; SectionKey : ShortString);
      begin
         if (SectionKey <> '') then TStr := DataBase.MyData.GetFieldByNameAsString(SectionKey)
         else TStr := '';
         {$IfDef RecordPLSSTiming} WriteLineToDebugFile('SECTN field: ' + TStr); {$EndIf}
         while (Length(TStr) > 0) and (TStr[1] = '0') do Delete(TStr,1,1);
         if (TStr = '') then TStr := '0';
         SectionString := ' Sect ' + TStr;
         {$IfDef RecordPLSSTiming} WriteLineToDebugFile('Section string: ' + SectionString); {$EndIf}
         TownShipString := PLSSTownshipNameFromLNDKEY(DataBase.MyData);
      end;


begin
   {$IfDef RecordPLSS} WriteLineToDebugFile('tPLSSDataBase.SimplePLSSLocation in, town bb=' + sfBoundBoxToString(GISdb[TownshipDB].dbBoundBox));   {$EndIf}
   Result := '';

   if not PointInBoundingBox(Lat,Long,GISdb[TownshipDB].dbBoundBox) then begin
      {$IfDef RecordPLSS} WriteLineToDebugFile('tPLSSDataBase.SimplePLSSLocation out, not on this DB'); {$EndIf}
      exit;
   end;

   ShowHourglassCursor;
   SectionString := '';
   QuarterString := '';
   TownshipString := '';

   UseFilter := PointInBoxGeoFilter(Lat,Long);
   {$IfDef RecordPLSS} WriteLineToDebugFile('filter= '+ UseFilter); {$EndIf}

   {$IfDef RecordPLSSTiming} WriteLineToDebugFile('next stop in tPLSSDataBase.PLSSLocation ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod)); {$EndIf}

   if (MDDef.PLSSDef.PLSSQuartersInLabels) then begin
      {$IfDef RecordPLSSQuarter} WriteLineToDebugFile('Using Quarters'); {$EndIf}
      GISdb[QuarterDB].MyData.ApplyFilter(UseFilter);

      if GISdb[QuarterDB].MyData.eof then begin
         {$IfDef RecordPLSSQuarter} WriteLineToDebugFile('Unable to restrict quarters'); {$EndIf}
      end
      else begin
         QuarterString := GISdb[QuarterDB].MyData.GetFieldByNameAsString(QSectKey);
         if (QuarterString = 'ALL') then QuarterString := ''
         else if GISdb[QuarterDB].MyData.FieldExists('SURSYS') and (GISdb[QuarterDB].MyData.GetFieldByNameAsString('SURSYS') = 'L') then begin
            if not MDDef.PLSSDef.PLSSLotsInLabels then QuarterString := '  ';
            QuarterString := GISdb[QuarterDB].MyData.GetFieldByNameAsString('QSECTION') + '¼' + '  ' + QuarterString;;
         end
         else if (QuarterString <> '') then begin
            Insert('¼',QuarterString,5);
            Insert('¼',QuarterString,3);
         end;
         TownshipAndSection(GISdb[QuarterDB],SectKey);
      end;
   end;

   {$IfDef RecordPLSSTiming} WriteLineToDebugFile('quarters done'); {$EndIf}

   if (QuarterString = '') then begin
      {$IfDef RecordPLSSQuarter} WriteLineToDebugFile('(QuarterString = '')'); {$EndIf}
      GISdb[SectionDB].MyData.ApplyFilter(UseFilter);
      {$IfDef RecordPLSSTiming} WriteLineToDebugFile('Sections found: ' + IntToStr(GISdb[SectionDB].MyData.RecordCount )); {$EndIf}
      if (GISdb[SectionDB].MyData.RecordCount > 0) and (RestrictIfNeeded(GISdb[SectionDB])) then begin
         {$IfDef RecordPLSSTiming} WriteLineToDebugFile('Restricted OK'); {$EndIf}
         TownshipAndSection(GISdb[SectionDB],SectKey);
      end;
   end;

   case MDDef.PLSSDef.PLSSFormat of
      plssTRS : Result := TownShipString + ' ' + SectionString + ' ' + QuarterString;
      plssSTR : Result := QuarterString + ' ' + SectionString + ' ' + TownShipString;
   end;
   Result := ptTrim(Result);

   GISdb[SectionDB].ClearGISFilter;
   GISdb[TownshipDB].ClearGISFilter;
   ShowDefaultCursor;
   {$IfDef RecordPLSS} WriteLineToDebugFile('tPLSSDataBase.SimplePLSSLocation out, result=' + Result); {$EndIf}
end;


procedure ZeroPLSS;
var
  i : integer;
begin
   for I := 1 to MaxPLSS do PLSS[i] := Nil;
end;


initialization
   ZeroPLSS;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing dem_plss in'); {$EndIf}
   CloseAllPLSS;
   {$IfDef RecordPLSSTiming} WriteLineToDebugFile('RecordPLSSTimingProblems in dem_plss'); {$EndIf}
   {$IfDef RecordPLSSQuarter} WriteLineToDebugFile('RecordPLSSQuarterProblems in dem_plss'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosingProblems in dem_plss'); {$EndIf}
   {$IfDef RecordPLSSLocation} WriteLineToDebugFile('RecordPLSSLocationProblems in dem_plss'); {$EndIf}
   {$IfDef RecordPLSS} WriteLineToDebugFile('RecordPLSSProblems in dem_plss'); {$EndIf}
   {$IfDef RecordPLSSFields} WriteLineToDebugFile('RecordPLSSFields in dem_plss'); {$EndIf}
   {$IfDef PLSSOpening} WriteLineToDebugFile('PLSSOpening in dem_plss'); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('RecordFont in dem_plss'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing dem_plss out'); {$EndIf}
end.
