unit dem_nlcd;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM                }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordNLCDProblems}
   //{$Define RecordNLCDLegend}
   //{$Define RecordBarGraphs}
   {$Define RecordDEMIX}
   //{$Define RecordBatch}
   //{$Define RecordBarGraphsDetailed}
   //{$Define RecordPaletteProblems}
{$EndIf}

interface

uses
//needed for inline of the core DB functions
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

   Windows,Math,StrUtils,Forms, Menus, Graphics,SysUtils,Classes,
   Petmar_types,PETMAR,DEMDefs,DEMMapf;

type
   tCategory = record
      ShortName : ShortString;
      LongName  : string[128];
      Color     : tPlatformColor;
      UseCat    : boolean;
      UseStat   : boolean;
      Height    : float64;
   end;
   tNLCDarray = array[0..MaxNLCDCategories] of float64;
   tNLCDCats = array[0..MaxNLCDCategories] of tCategory;

   procedure SetUpNLCDCategories(AskLimit : boolean; LandCover : ShortString; var Categories : tNLCDCats);
   function IsThisLandCover(fName : PathStr;  var LandCover : ShortString) : boolean;
   procedure LandCoverBarGraphs(UseTable : boolean; Legend : boolean = true; MaxCat : boolean = true);

var
   LandCoverCatsUsed : array[0..MaxNLCDCategories] of boolean;

implementation


uses
  {$IfDef ExSat}
  {$Else}
      DEMEROS,
   {$EndIf}
   DEM_Manager, gdal_tools,DEMcoord,DEMdatabase,
   DEMIX_control,
   petimage_form,
   PetImage,PetMath,PetDBUtils,Toggle_db_use,nevadia_main,DEMDef_routines;


procedure LandCoverBarGraphs(UseTable : boolean; Legend : boolean = true; MaxCat : boolean = true);
const
   TopSize = 40;
   EntryHeight = 32;
   AreaBlank = 12;
   IncludeTileName : boolean = true;
   IncludeTiles : boolean = false;
   IncludeLegend : boolean = false;
   IncludeAreaName : boolean = true;
var
   dbName : PathStr;


   procedure ASeries(LandCoverfName : PathStr; aTitle : shortstring);
   var
      NumDrawn,AreaWidth,TileWidth,
      j,x,top,NumAreas : integer;
      singleleg,legbmp,bmp : tMyBitmap;
      Table : tMyData;
      Areas : tStringList;

         function DoOne(bbox : sfBoundBox; aLabel : shortstring) : boolean;
         var
            fName : PathStr;
            NewDEM,NewDB : integer;
         begin
            {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Do series=' + aLabel); {$EndIf}
            try
               Result := true;
               NewDEM := GDALsubsetimageandopen(bbox,true,LandCoverfName);
               {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Grid opened'); {$EndIf}
               if ValidDEM(NewDEM) then begin
                  NewDB := DEMGlb[NewDEM].SelectionMap.MakeNLCDLegend(aLabel);
                  {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Legend made'); {$EndIf}
                  inc(NumDrawn);  //outside loop in case nothing drawn
                  if ValidDB(NewDB) then begin
                     if not IncludeLegend then aLabel := '';
                     SingleLeg := GISDB[NewDB].BarGraphLegend(false,aLabel);
                     {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Bar graph legend made'); {$EndIf}
                     bmp.Canvas.Draw(AreaWidth + TileWidth,Top,SingleLeg);
                     SingleLeg.Destroy;
                     fName := GISDB[NewDB].dbFullName;
                  end
                  else begin
                     Result := false;
                     fName := '';
                  end;
               end
               else begin
                  {$IfDef RecordBatch} HighlightLineToDebugFile('No data for ' + aLabel); {$EndIf}
                  Result := false;
               end;
            finally
               CloseAndNilNumberedDB(NewDB);
               DeleteFileIfExists(fName);
               CloseSingleDEM(NewDEM);
               {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Do One Out'); {$EndIf}
            end;
         end;

       procedure AddTitle(Xloc : integer = 0);
       begin
          bmp.canvas.font.Size := 20;
          bmp.canvas.font.Style := [fsBold];
          bmp.Canvas.TextOut(5 + XLoc,0,aTitle);
          bmp.Canvas.Font.Size := 14;
       end;

   var
      Name,Area,AreaName : shortstring;
      ItsDEMIX : boolean;
      i,RefDEM : integer;
   begin
       {$If Defined(RecordBatch) or Defined(RecordDEMIX)} HighLightLineToDebugFile('Do series=' + LandCoverfName); {$EndIf}
       NumDrawn := 0;
       for j := 0 to MaxNLCDCategories do LandCoverCatsUsed[j] := false;
       FindDriveWithFile(LandCoverfName);
       if UseTable then begin
          Table := tMyData.Create(dbName);
          ItsDEMIX := Table.FieldExists('DEMIX_TILE') and Table.FieldExists('AREA');
          if ItsDEMIX then begin
             GetDEMIXpaths;
             CreateBitmap(bmp,2400,1200);  //100 + (Table.RecordCount * EntryHeight) + (pred(Areas.Count) * AreaBlank));
             bmp.Canvas.Font.Size := 14;
             AreaWidth := 0;
             TileWidth := 0;

             Areas := tStringList.Create;
             Table.First;
             while not Table.eof do begin
                Area := Table.GetFieldByNameAsString('AREA');
                if (Areas.IndexOf(Area) = -1) then begin
                   Areas.Add(Area);
                end;
                Table.Next;
             end;
             Top := 100;
             for i := 0 to pred(Areas.Count) do begin
                Table.ApplyFilter('AREA=' + QuotedStr(Areas.Strings[i]));
                if IncludeAreaName then begin
                   j := Bmp.Canvas.TextWidth(Table.GetFieldByNameAsString('AREA'));
                   if (j > AreaWidth) then AreaWidth := j;
                end;
                if IncludeTiles then begin
                   while not Table.Eof do begin
                      Top := Top + EntryHeight;
                      if IncludeTileName then begin
                         j := Bmp.Canvas.TextWidth(Table.GetFieldByNameAsString('DEMIX_TILE'));
                         if (j > TileWidth) then TileWidth := j;
                      end;
                      Table.Next;
                   end;
                end
                else Top := Top + EntryHeight;
                Top := Top + AreaBlank;
             end;
             bmp.Height := Top;
          end;
          {$If Defined(RecordBarGraphs) or Defined(RecordBatch)} WriteLineToDebugFile('Create bitmap ' + BitmapSize(bmp)); {$EndIf}
          Top := TopSize;
          if ItsDEMIX then begin
             AreaWidth := AreaWidth + 15;
             TileWidth := TileWidth + 15;

             {$IfDef RecordBatch} WriteLineToDebugFile('AreaWidth=' + IntToStr(AreaWidth) + '  TileWidth=' + IntToStr(TileWidth)); {$EndIf}

             AddTitle(AreaWidth + TileWidth);

             for i := 0 to pred(Areas.Count) do begin
                WMDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(Areas.Count) + ' ' + Areas.Strings[i] );
                Table.ApplyFilter('AREA=' + QuotedStr(Areas.Strings[i]));
                AreaName := Table.GetFieldByNameAsString('AREA');
                Name := RemoveUnderScores(AreaName);
                {$IfDef RecordBarGraphs} WriteLineToDebugFile('Area=' + Name + ' at x=' + IntToStr(5) + '  y=' + IntToStr(Top)); {$EndIf}
                if IncludeTiles then begin
                   if IncludeAreaName then Bmp.Canvas.TextOut(5,Top + Table.FiltRecsInDB div 2 * EntryHeight,Name);
                   while not Table.Eof do begin
                      Name := Table.GetFieldByNameAsString('DEMIX_TILE');
                      if DoOne(DEMIXtileBoundingBox(Name),'  ') then begin
                      end
                      else begin
                         {$IfDef RecordBatch} HighLightLineToDebugFile('No land type ' + Name); {$EndIf}
                      end;
                      if IncludeTileName then Bmp.Canvas.TextOut(AreaWidth + 5,Top,Name);
                   end;
                   {$IfDef RecordBarGraphs} WriteLineToDebugFile('Tile=' + Name + ' at x=' + IntToStr(AreaWidth + 5) + '  y=' + IntToStr(Top)); {$EndIf}
                   Top := Top + EntryHeight;
                   Table.Next;
                end
                else begin
                    if IncludeAreaName then Bmp.Canvas.TextOut(5,Top,Name);
                    RefDEM := DEMIXLoadRefDEMFromPath(AreaName,true);
                    DoOne(DEMGlb[RefDEM].DEMBoundBoxGeo,'  ');
                    CloseSingleDEM(RefDEM);
                    Top := Top + EntryHeight;
                end;
                Top := Top + AreaBlank;
             end;
             {$IfDef RecordBatch} WriteLineToDebugFile('End DEMIX tiles'); {$EndIf}
          end
          else begin
             AddTitle;
             while not Table.Eof do begin
                if DoOne(Table.GetRecordBoundingBox,Table.GetFieldByNameAsString('NAME')) then Top := Top + EntryHeight;
                Table.Next;
             end;
          end;
          Table.Destroy;
       end
       else begin
          CreateBitmap(bmp,1200,100 + NumDEMDataSetsOpen * 60);
          AddTitle;
          for j := 1 to MaxDEMDataSets do begin
            if ValidDEM(j) then begin
               if DoOne(DEMGlb[j].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,DEMGlb[j].AreaName) then Top := Top + EntryHeight;
            end;
          end;
       end;
      {$IfDef RecordBatch} WriteLineToDebugFile('Drawing done bitmap, ' + BitmapSize(bmp)); {$EndIf}
      PutBitmapInBox(bmp);
      {$IfDef RecordBatch} WriteLineToDebugFile('Got image part bitmap, ' + BitmapSize(bmp)); {$EndIf}
      DisplayBitmap(Bmp,'Land cover distribution');
   end;

begin
   {$If Defined(RecordBatch) or Defined(RecordDEMIX)} WriteLineToDebugFile('LandCoverBarGraphs in'); {$EndIf}
   try
      HeavyDutyProcessing := true;
      if UseTable and (not GetFileFromDirectory('bounding box data base','*.dbf',dbName)) then exit;
      ASeries('d:\landcover\lccs_300m\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif','LCCS 300 m 2015');
      IncludeAreaName := false;
      ASeries('d:\landcover\iwahashi\iwahashi.tif','Iwahashi and Pike');
      ASeries('d:\landcover\Meybeck\Meybeck_1km1.tif','Meybeck and others');
      ASeries('d:\landcover\Geomorphon\geomorphon_1KMmaj_GMTEDmd.tif','Geomorphons');
   finally
      CloseAllDatabases;
      HeavyDutyProcessing := false;
      {$If Defined(RecordBatch) or Defined(RecordDEMIX)} WriteLineToDebugFile('LandCoverBarGraphs out'); {$EndIf}
   end;
end;



function IsThisLandCover(fName : PathStr;  var LandCover : ShortString) : boolean;
begin
   Landcover := '';
   if (fName <> '') then begin
      fName := UpperCase(FName);
      if StrUtils.AnsiContainsText(fName,'CHANGE') and StrUtils.AnsiContainsText(fName,'NLCD') then LandCover := 'NLCD-change'
      else if UpperCase(Copy(ExtractFileName(fName),1,7)) = 'NLCD_20' then LandCover := 'NLCD-2001up'
      else if StrUtils.AnsiContainsText(fName,'GLC-2000') then LandCover := 'GLC-2000'
      else if (StrUtils.AnsiContainsText(fName,'NLCD') and StrUtils.AnsiContainsText(fName,'1990')) then LandCover := 'NLCD-1990'
      else if StrUtils.AnsiContainsText(fName,'NLCD') then LandCover := 'NLCD-2001up'
      else if StrUtils.AnsiContainsText(fName,'LANDFIRE') and (not StrUtils.AnsiContainsText(fName,'EVT')) then LandCover := 'LANDFIRE'
      else if StrUtils.AnsiContainsText(fName,'S2GLC') then LandCover := 'S2GLC'
      else if StrUtils.AnsiContainsText(fName,'ESRI2020') then LandCover := 'ESRI2020'
      else if StrUtils.AnsiContainsText(fName,'ESACCI-LC-L4-LCCS') then LandCover := 'CCI-LC'
      else if StrUtils.AnsiContainsText(fName,'LC100_GLOBAL') then LandCover := 'CGLS-LC100'
      else if StrUtils.AnsiContainsText(fName,'ESA_WORLDCOVER_10M') then LandCover := 'WorldCover10m'
      else if StrUtils.AnsiContainsText(fName,'GLOBCOVER') then LandCover := 'GLOBCOVER'
      else if StrUtils.AnsiContainsText(fName,'CCAP') then LandCover := 'CCAP'
      else if StrUtils.AnsiContainsText(fName,'GEOMORPHON') then LandCover := 'GEOMORPHON'
      else if StrUtils.AnsiContainsText(fName,'PENNOCK') then LandCover := 'PENNOCK'
      else if StrUtils.AnsiContainsText(fName,'IWAHASHI') then LandCover := 'IWAHASHI'
      else if StrUtils.AnsiContainsText(fName,'MEYBECK') then LandCover := 'MEYBECK'
      else if StrUtils.AnsiContainsText(fName,'LCMAP') then LandCover := 'LCMAP';
   end;
   Result := (LandCover <> '');
end;


procedure SetUpNLCDCategories(AskLimit : boolean; LandCover : ShortString; var Categories : tNLCDCats);
var
   CatTable : tMyData;

   procedure SetCategories(var Categories : tNLCDCats; LandCover : ShortString);
   var
      i : integer;
   begin
      for i := 0 to MaxNLCDCategories do begin
         Categories[i].ShortName := '';
         Categories[i].LongName  := '';
         Categories[i].Color     := claWhite;
         Categories[i].UseCat    := false;
         Categories[i].UseStat   := false;
      end;
      CatTable.ApplyFilter( 'SERIES = ' + QuotedStr(LandCover));
      if (CatTable.RecordCount > 0) then begin
         if AskLimit then VerifyRecordsToUse(CatTable,'LONG_NAME','Fields to mask');
         {$IfDef RecordNLCDLegend} WriteLineToDebugFile('CatTable.Filter: ' + CatTable.Filter); {$EndIf}
         repeat
            i := CatTable.GetFieldByNameAsInteger('CATEGORY');
            Categories[i].ShortName := CatTable.GetFieldByNameAsString('SHORT_NAME');
            Categories[i].LongName  := CatTable.GetFieldByNameAsString('LONG_NAME');
            Categories[i].Color     := CatTable.PlatformColorFromTable;
            Categories[i].UseCat    := CatTable.GetFieldByNameAsString('USE') = 'Y';
            Categories[i].UseStat   := CatTable.GetFieldByNameAsString('STAT') = 'Y';
            if CatTable.GetFieldByNameAsString('HEIGHT') = '' then Categories[i].Height := 0 else Categories[i].Height := CatTable.GetFieldByNameAsFloat('HEIGHT');
            {$IfDef RecordNLCDLegend} WriteLineToDebugFile(IntToStr(i) + '  ' + LongName +  '  ' + Petmar.ColorString(Color)); {$EndIf}
            CatTable.Next;
         until CatTable.eof;
      end
      else begin
          MessageToContinue('Land cover ' + LandCover + ' missing in ' + LandCoverFName);
      end;
   end;

begin
   {$IfDef RecordNLCDLegend}   WriteLineToDebugFile('tLandCoverImage.SetUpCategories, Landcover=' + LandCover); {$EndIf}
   CatTable := tMyData.Create(LandCoverSeriesFName);
   SetCategories(Categories,LandCover);
   CatTable.Destroy;
   {$IfDef RecordNLCDLegend} WriteLineToDebugFile('tLandCoverImage.SetUpCategories out'); {$EndIf}
end;

initialization
finalization
{$IfDef RecordNLCDProblems}    WriteLineToDebugFile('RecordNLCDProblems in DEM_NLCD'); {$EndIf}
{$IfDef RecordNLCDLegend}      WriteLineToDebugFile('RecordNLCDLegend in DEM_NLCD'); {$EndIf}
{$IfDef RecordPaletteProblems} WriteLineToDebugFile('RecordPaletteProblems in DEM_NLCD'); {$EndIf}
end.


