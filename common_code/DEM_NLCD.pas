unit dem_nlcd;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM                }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordNLCDProblems}
   //{$Define RecordNLCDLegend}
   {$Define RecordBatch}
   {$Define RecordBarGraphsDetailed}
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
   petimage_form,
   PetImage,PetMath,PetDBUtils,Toggle_db_use,nevadia_main,DEMDef_routines;


procedure LandCoverBarGraphs(UseTable : boolean; Legend : boolean = true; MaxCat : boolean = true);
const
   TopSize = 35;
var
   dbName : PathStr;

   procedure ASeries(LandCoverfName : PathStr; aTitle : shortstring);
   var
      NumDrawn : integer;
      j,x,top,Base : integer;
      singleleg,legbmp,bmp : tMyBitmap;
      Table : tMyData;

         function DoOne(bbox : sfBoundBox; aLabel : shortstring) : boolean;
         var
            fName : PathStr;
            NewDEM,NewDB : integer;
         begin
            {$IfDef RecordBatch} WriteLineToDebugFile('Do series=' + aLabel); {$EndIf}
            try
               Result := true;
               NewDEM := GDALsubsetimageandopen(bbox,true,LandCoverfName);
               {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Grid opened'); {$EndIf}
               if (NewDEM = 0) then begin
                  {$IfDef RecordBatch} HighlightLineToDebugFile('No data for ' + aLabel); {$EndIf}
                  Result := false;
               end
               else begin
                  NewDB := DEMGlb[NewDEM].SelectionMap.MakeNLCDLegend(aLabel);
                  {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Legend made'); {$EndIf}
                  inc(NumDrawn);  //outside loop in case nothing drawn
                  if (NewDB = 0) then begin
                     Result := false;
                     fName := '';
                  end
                  else begin
                     SingleLeg := GISDB[NewDB].BarGraphLegend(false,aLabel);
                     {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Bar graph legend made'); {$EndIf}
                     bmp.Canvas.Draw(0,TopSize + NumDrawn * singleleg.Height,SingleLeg);
                     Base := TopSize + NumDrawn * singleleg.Height;
                     SingleLeg.Destroy;
                     fName := GISDB[NewDB].dbFullName;
                  end;
               end;
            finally
               CloseAndNilNumberedDB(NewDB);
               DeleteFileIfExists(fName);
               CloseSingleDEM(NewDEM);
               {$IfDef RecordBarGraphsDetailed} WriteLineToDebugFile('Do One Out'); {$EndIf}
            end;
         end;

       procedure AddTitle;
       begin
          bmp.canvas.font.Size := 18;
          bmp.canvas.font.Style := [fsBold];
          bmp.Canvas.TextOut(5,5,aTitle);
       end;

   var
      Name : shortstring;
   begin
       {$IfDef RecordBatch} HighLightLineToDebugFile('Do series=' + LandCoverfName); {$EndIf}
       NumDrawn := 0;
      // NewDEM := 0;
       for j := 0 to MaxNLCDCategories do LandCoverCatsUsed[j] := false;
       if UseTable then begin
          Table := tMyData.Create(dbName);
          CreateBitmap(bmp,1200,100 + Table.RecordCount * 60);
          AddTitle;

          if Table.FieldExists('DEMIX_TILE') then begin
             {$IfDef RecordBatch} WriteLineToDebugFile('Start DEMIX tiles'); {$EndIf}
             while not Table.Eof do begin
                Name := Table.GetFieldByNameAsString('DEMIX_TILE');
                DoOne(DEMIXtileBoundingBox(Name),Name);
                Table.Next;
             end;
             {$IfDef RecordBatch} WriteLineToDebugFile('End DEMIX tiles'); {$EndIf}
          end
          else begin
             while not Table.Eof do begin
                DoOne(Table.GetRecordBoundingBox,Table.GetFieldByNameAsString('NAME'));
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
               DoOne(DEMGlb[j].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,DEMGlb[j].AreaName);
            end;
          end;
       end;

       (*
       if Legend then begin
         CreateBitmap(legbmp,1200,1800);
         legbmp.canvas.font.Size := 18;
         legbmp.canvas.font.Style := [fsBold];
         top := 0;
         for x := 1 to MaxNLCDCategories do if LandCoverCatsUsed[x] then begin
            legbmp.Canvas.Brush.Color := ConvertPlatformColorToTColor(DEMGlb[NewDEM].NLCDCats^[x].Color);
            legbmp.Canvas.Brush.Style := bsSolid;
            legbmp.Canvas.Rectangle(15,Top+5,35,Top+30);
            legbmp.Canvas.Brush.Style := bsClear;
            legbmp.Canvas.TextOut(55,Top+2,DEMGlb[NewDEM].NLCDCats^[x].LongName);
            inc(Top,30);
         end;
         LegBMP.Height := top + TopSize;
         if (NewDEM <> 0) then CloseSingleDEM(NewDEM);

         Bmp.Height := BMP.Height + top + 25;
         bmp.Canvas.Draw(0,Base + 20,LegBMP);
         LegBMP.Destroy;
       end;
       *)
      GetImagePartOfBitmap(bmp);
      DisplayBitmap(Bmp,'Land cover distribution');
   end;

begin
   {$IfDef RecordBatch} WriteLineToDebugFile('LandCoverBarGraphs in'); {$EndIf}
   try
      HeavyDutyProcessing := true;
      if UseTable then
         if not GetFileFromDirectory('bounding box data base','*.dbf',dbName) then exit;
      ASeries('h:\landcover\Geomorphon\geomorphon_1KMmaj_GMTEDmd.tif','Geomorphons');
      ASeries('h:\landcover\iwahashi\iwahashi.tif','Iwahashi and Pike');
      ASeries('h:\landcover\Meybeck\Meybeck_1km1.tif','Meybeck and others');
      ASeries('h:\landcover\lccs_300m\ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif','LCCS 300 m 2015');
   finally
      CloseAllDatabases;
      HeavyDutyProcessing := false;
      {$IfDef RecordBatch} WriteLineToDebugFile('LandCoverBarGraphs out'); {$EndIf}
   end;
end;




function IsThisLandCover(fName : PathStr;  var LandCover : ShortString) : boolean;
begin
   Landcover := '';
   if (fName <> '') then begin
      fName := UpperCase(FName);
      if StrUtils.AnsiContainsText(fName,'Change') and StrUtils.AnsiContainsText(fName,'NLCD') then LandCover := 'NLCD-change'
      else if UpperCase(Copy(ExtractFileName(fName),1,7)) = 'NLCD_20' then LandCover := 'NLCD-2001up'
      else if StrUtils.AnsiContainsText(fName,'GLC-2000') then LandCover := 'GLC-2000'
      else if (StrUtils.AnsiContainsText(fName,'NLCD') and StrUtils.AnsiContainsText(fName,'1990')) then LandCover := 'NLCD-1990'
      else if StrUtils.AnsiContainsText(fName,'NLCD') then LandCover := 'NLCD-2001up'
      else if StrUtils.AnsiContainsText(fName,'LANDFIRE') then LandCover := 'LANDFIRE'
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
      else if StrUtils.AnsiContainsText(fName,'MEYBECK') then LandCover := 'MEYBECK';
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
   CatTable := tMyData.Create(LandCoverFName);
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


