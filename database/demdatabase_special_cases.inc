{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{                                        }
{   include file for demdatabase         }
{________________________________________}



{$IfDef ExGeography}
{$Else}
   procedure tGISdataBaseModule.PlotKoppenStations(Bitmap : tMyBitmap);
   var
      xp,yp : integer;
      Color : tPlatformColor;
   begin
       if MyData.FieldExists('CLASS') then begin
           EmpSource.Enabled := false;
          MyData.First;
          while not MyData.EOF do begin
            TheMapOwner.MapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat(LatFieldName),MyData.GetFieldByNameAsFloat(LongFieldName),xp,yp);
            if TheMapOwner.MapDraw.OnScreen(xp,yp) and GetKoppenColor(MyData.GetFieldByNameAsString('CLASS'),Color) then
               ScreenSymbol(Bitmap.Canvas,xp,yp,dbOpts.Symbol.DrawingSymbol,dbOpts.Symbol.Size,Color);
            MyData.Next;
          end;
          EmpSource.Enabled := true;
       end;
   end;
{$EndIf}


procedure AddCanopyHeightToDEMIXDB(DBonTable : integer);
// intended for DEMIX 10x10 km tiles
// https://data.nasa.gov/dataset/global-vegetation-height-metrics-from-gedi-and-icesat2-71ab5
// uses 98th percentile canopy, 1000 m grids, and averages within bounding box
var
   fName : PathStr;
   CanHt,i : integer;
   Lat,Long : float64;
   bb : sfBoundBox;
   Mean,Std : float32;
   NPts : int64;
   FieldName : shortstring;
begin
    fName := 'J:\landcover\gedi_icesat_canopy\gedi_rh98_1000m.tif';
    FindDriveWithFile(fName);
    FieldName := 'CANOPY_HT';
    if LoadNewDEM(CanHt,fName,false) then begin
       ShowHourglassCursor;
       GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftFloat,FieldName,6,2);
       GISdb[DBonTable].MyData.First;
       StartProgress('Add from grid: ' + FieldName);
       i := 0;
       while not GISdb[DBonTable].MyData.eof do begin
          if (i mod 25 = 0) then UpdateProgressBar(i / GISdb[DBonTable].MyData.FiltRecsinDB);
          inc(i);
          GISdb[DBonTable].EmpSource.Enabled := false;
          if GISdb[DBonTable].MyData.ValidLatLongFromTable(Lat,Long) then begin
             bb.XMin := Long - 0.05;
             bb.YMin := Lat - 0.05;
             bb.XMax := Long + 0.05;
             bb.YMax := Lat + 0.05;
             DEMglb[CanHt].ElevationStatistics(DEMglb[CanHt].sfBoundBox2tGridLimits(bb), Mean,Std,NPts);
             if (NPts > 0) then begin
                GISdb[DBonTable].MyData.Edit;
                GISdb[DBonTable].MyData.SetFieldByNameAsFloat(FieldName,Mean);
             end;
          end;
          GISdb[DBonTable].MyData.Next;
       end;
    end;
    CloseSingleDEM(CanHt);
    EndProgress;
    GISdb[DBonTable].ShowStatus;
end;


procedure AddBiomesToDEMIXdb(DBonTable : integer);
//https://ecoregions.appspot.com
//Eric Dinerstein et al., 2017, https://doi.org/10.1093/biosci/bix014
var
   fName : PathStr;
   FieldName : shortstring;
   db,i : integer;
   Lat,Long : float64;
begin
   fName := 'J:\landcover\Ecoregions2017\Ecoregions2017.dbf';
   FindDriveWithFile(fName);
   FieldName := 'BIOME_NAME';
   if OpenNumberedGISDataBase(db,fName) then begin
       ShowHourglassCursor;
       StartProgress('Add from table: ' + FieldName);
       GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftString,FieldName,GISdb[DB].MyData.GetFieldLength(FieldName));
       GISdb[DBonTable].MyData.First;
       i := 0;
       while not GISdb[DBonTable].MyData.eof do begin
          if (i mod 25 = 0) then UpdateProgressBar(i / GISdb[DBonTable].MyData.FiltRecsinDB);
          inc(i);
          GISdb[DBonTable].EmpSource.Enabled := false;
          if GISdb[DBonTable].MyData.ValidLatLongFromTable(Lat,Long) then begin
             fName := GISdb[DB].GetStringFromRecordAtLatLong(Lat,Long,FieldName);
             GISdb[DBonTable].MyData.Edit;
             GISdb[DBonTable].MyData.SetFieldByNameAsString(FieldName,fName);
          end;
          GISdb[DBonTable].MyData.Next;
       end;
       CloseSingleDB(db);
       EndProgress;
       GISdb[DBonTable].ShowStatus;
   end
   else begin
      MessageToContinue('Missing: ' + fName);
   end;
end;


var
   NGAgazDir : PathStr;
   NGAplotoptions : PathStr;
   NGAcode : shortstring;

procedure SetWarCrimesSettings(InputDB : integer);
begin
   if InputDB = 1 then begin
      NGAgazDir := 'C:\mydocs\Papers\icmg_2026\data\gazetteer\';
      NGAplotoptions := 'C:\mydocs\Papers\icmg_2026\data\nga_feature_classes.dbf';
      NGAcode := 'DESIG_CD';
   end
   else if InputDB = 2 then begin
      NGAgazDir := 'C:\mydocs\Papers\icmg_2026\data\osm_geofabrik_points\';
      NGAplotoptions := 'C:\mydocs\Papers\icmg_2026\data\geofabrik_point_feature_classes.dbf';
      NGAcode := 'FCLASS';
   end
   else if InputDB = 3 then begin
      NGAgazDir := 'C:\mydocs\Papers\icmg_2026\data\osm_geofabrik_buildings\';
      NGAplotoptions := 'C:\mydocs\Papers\icmg_2026\data\geofabrik_building_types.dbf';
      NGAcode := 'TYPE';
   end;
end;


procedure StatsWarCrimesDB(MapOwner : tMapForm; InputDB : integer);
var
   i,j,k,PlotOptions,db : integer;
   TheFiles,TheGroups,Subgroups,Findings : tStringList;
   aFilter : shortstring;
   fName : PathStr;
begin
   {$IfDef WarCrimes} WriteLineToDebugFile('StatsWarCrimesDB in'); {$EndIf}
   SetWarCrimesSettings(InputDB);
   SetColorForProcessing;
   ShowHourglassCursor;
   Findings := tStringList.Create;
   Findings.Add('COUNTRY,GROUP,SUBGROUP,NUMBER');
   TheFiles := tStringList.Create;
   FindMatchingFiles(NGAgazDir,'*.dbf',TheFiles,0);
   PlotOptions := MapOwner.OpenDBonMap('',NGAPlotOptions,false,false);
   TheGroups := GISdb[PlotOptions].MyData.ListUniqueEntriesInDB('GROUP');
   for j := 0 to pred(theFiles.Count) do begin
      {$IfDef WarCrimes} WriteLineToDebugFile(IntToStr(succ(j)) + '/' + IntToStr(theFiles.Count) + ' '  + theFiles.Strings[j]); {$EndIf}
      db := MapOwner.OpenDBonMap('',theFiles.Strings[j],false,false);
      for i := 0 to pred(theGroups.Count) do begin
         {$IfDef WarCrimes} WriteLineToDebugFile(IntToStr(succ(i)) + '/' + IntToStr(theGroups.Count) +' '  + theGroups.Strings[i]); {$EndIf}
         GISdb[PlotOptions].ApplyGISFilter('USE=' + QuotedStr('Y') + ' AND GROUP=' + QuotedStr(TheGroups.Strings[i]));
         Subgroups := GISdb[PlotOptions].MyData.ListUniqueEntriesInDB(NGAcode);
         aFilter := MakeOrFilter(PlotOptions,NGAcode, GISdb[PlotOptions].MyData.ListUniqueEntriesInDB(NGAcode));
         GISdb[db].ApplyGISFilter(aFilter);

         Findings.Add(ExtractFileNameNoExt(theFiles.Strings[j]) + ',' + TheGroups.Strings[i] + ',  ,' + IntToStr(GISdb[db].MyData.FiltRecsInDB));
         for k := 0 to pred(Subgroups.Count) do begin
            aFilter := NGAcode + '=' + QuotedStr(SubGroups.Strings[k]);
            {$IfDef WarCrimes} WriteLineToDebugFile(IntToStr(succ(k)) + '/' + IntToStr(SubGroups.Count) +' '  + SubGroups.Strings[k] + ' ' + aFilter); {$EndIf}
            GISdb[db].ApplyGISFilter(aFilter);
            Findings.Add(ExtractFileNameNoExt(theFiles.Strings[j]) + ',' + TheGroups.Strings[i] + ',' + Subgroups.Strings[k] + ',' + IntToStr(GISdb[db].MyData.FiltRecsInDB));
         end;
         Subgroups.Destroy;
      end;
      CloseSingleDB(DB);
   end;
   TheGroups.Destroy;
   CloseSingleDB(PlotOptions);
   fName := MDtempDir + ExtractFileName(NGAplotoptions);
   StringList2CSVtoDB(Findings,fName);
   ShowDefaultCursor;
   SetColorForWaiting;
end;

procedure MapWarCrimesDB(MapOwner : tMapForm; InputDB : integer);
var
   TheFiles,TheGroups : tStringList;
   i,j,k,db,xp,yp,PlotOptions : integer;
   Sym   : tDrawingSymbol;
   Lat,Long : float64;
   Size  : byte;
   Color : tPlatformColor;
   Bitmap : tMyBitmap;
   aFilter : shortstring;
begin
   {$IfDef WarCrimes} WriteLineToDebugFile('MapWarCrimesDB in'); {$EndIf}
   SetWarCrimesSettings(InputDB);

   ShowHourglassCursor;
   CopyImageToBitmap(MapOwner.Image1,Bitmap);
   TheFiles := tStringList.Create;
   FindMatchingFiles(NGAgazDir,'*.dbf',TheFiles,0);
   PlotOptions := MapOwner.OpenDBonMap('',NGAPlotOptions,false,false);
   GISdb[PlotOptions].ApplyGISFilter('USE=' + QuotedStr('Y'));
   TheGroups := GISdb[PlotOptions].MyData.ListUniqueEntriesInDB('GROUP');
   Sym := FilledBox;
   Size := 3;
   for j := 0 to pred(theFiles.Count) do begin
      {$IfDef WarCrimes} WriteLineToDebugFile(IntToStr(succ(j)) + '/' + IntToStr(theFiles.Count) + ' '  + theFiles.Strings[j]); {$EndIf}
      db := MapOwner.OpenDBonMap('',theFiles.Strings[j],false,false);
      for i := 0 to pred(theGroups.Count) do begin
          {$IfDef WarCrimes} WriteLineToDebugFile(IntToStr(succ(i)) + '/' + IntToStr(theGroups.Count) +' '  + theGroups.Strings[i]); {$EndIf}
          GISdb[PlotOptions].ApplyGISFilter('USE=' + QuotedStr('Y') + ' AND GROUP=' + QuotedStr(TheGroups.Strings[i]));
          if GISdb[PlotOptions].MyData.FiltRecsInDB > 0 then begin
              Color := GISdb[PlotOptions].MyData.PlatformColorFromTable;
              aFilter := MakeOrFilter(PlotOptions,NGAcode, GISdb[PlotOptions].MyData.ListUniqueEntriesInDB(NGAcode));
              GISdb[db].ApplyGISFilter(aFilter);
              if GISdb[db].MyData.FiltRecsInDB > 0 then begin
                  GISdb[db].EmpSource.Enabled := false;
                  while not GISdb[db].MyData.eof do begin
                    if GISdb[db].ValidLatLongFromTable(Lat,Long) then begin
                       MapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
                       if MapOwner.MapDraw.OnScreen(xp,yp) then begin
                          PETMAR.ScreenSymbol(Bitmap.Canvas,xp,yp,Sym,Size,Color);
                       end;
                    end;
                    GISdb[db].MyData.Next;
                  end {while};
              end;
          end {if};
      end {for i};
      CloseSingleDB(db);
   end {for j};
   TheFiles.Destroy;
   CloseSingleDB(PlotOptions);
   MapOwner.Image1.Picture.Graphic := Bitmap;
   ShowDefaultCursor;
   {$IfDef WarCrimes} WriteLineToDebugFile('MapWarCrimesDB out'); {$EndIf}
end;


procedure ProcessBuildings(MapOwner : tMapForm);
const
   Countries : array[1..7] of shortstring = ('lebanon','afghanistan','iran','iraq','syria','israel-and-palestine','jordan');
var
   BaseDir,fName,OutDir : PathStr;
   TheFiles : tStringList;
   i,j,db,Rec : integer;
begin
   {$IfDef WarCrimes} WriteLineToDebugFile('ProcessBuildings in'); {$EndIf}
   SetColorForProcessing;
   BaseDir := 'C:\mydocs\Papers\icmg_2026\data\osm_geofabrik\';
   OutDir := 'C:\mydocs\Papers\icmg_2026\data\osm_geofabrik_buildings\';
   TheFiles := tStringList.Create;
   FindMatchingFiles(BaseDir,'*.dbf',TheFiles,5);
   for i := pred(TheFiles.Count) downto 0 do begin
      if (not AnsiContainsText(TheFiles.Strings[i],'buildings')) or AnsiContainsText(theFiles.Strings[i],'range')  or AnsiContainsText(theFiles.Strings[i],'bak') then TheFiles.Delete(i);
   end;

   ShowHourglassCursor;
   for i := 0 to pred(TheFiles.Count) do begin
      {$IfDef WarCrimes} WriteLineToDebugFile(IntToStr(i) + ' '  + theFiles.Strings[i]); {$EndIf}
      fName := '';
      for j := 1 to 7 do begin
         if AnsiContainsText(TheFiles.Strings[i],Countries[j]) then fName := OutDir + Countries[j] + '_buildings.dbf';
      end;
      if (fName = '') then begin
         MessageToContinue('country not included');
      end
      else begin
          if FileExists(fName) then begin
             {$IfDef WarCrimes} WriteLineToDebugFile('File exists=' + fName); {$EndIf}
          end
          else begin
             db := MapOwner.OpenDBonMap('',theFiles.Strings[i],false,false);
             if GISdb[db].CentroidPresent then begin
                {$IfDef WarCrimes} WriteLineToDebugFile('Centroid present=' + fName); {$EndIf}
             end
             else begin
                GISdb[db].AddCentroidsToFile(true);
             end;
             GISdb[db].ApplyGISfilter('TYPE<>' + QuotedStr(''));
             GISdb[db].SaveCurrentDBaseSubset(fName,1,true);
             CloseSingleDB(db);
          end;
      end;
   end;
   {$IfDef WarCrimes} WriteLineToDebugFile('ProcessBuildings out'); {$EndIf}
   SetColorForWaiting;
   ShowDefaultCursor;
end;




