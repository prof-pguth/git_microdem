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


const
   MaxCountries = 10;
   MaxLayers = 10;
var
   NGAgazDir : PathStr;
   NGAplotoptions : PathStr;
   NGAcode,LongName : shortstring;
   PlotOptions : integer;
   TheCountries,TheGroups : tStringList;
   db : array[0..MaxCountries] of integer;


procedure SetWarCrimesSettings(MapOwner : tMapForm; InputDB : integer);
var
   j : integer;
begin
   if InputDB = 1 then begin
      NGAgazDir := 'C:\mydocs\Papers\icmg_2026\data\gazetteer\';
      NGAplotoptions := 'C:\mydocs\Papers\icmg_2026\data\nga_feature_classes.dbf';
      NGAcode := 'DESIG_CD';
      LongName := 'FEATURE';
   end
   else if InputDB = 2 then begin
      NGAgazDir := 'C:\mydocs\Papers\icmg_2026\data\osm_geofabrik_points\';
      NGAplotoptions := 'C:\mydocs\Papers\icmg_2026\data\geofabrik_point_feature_classes.dbf';
      NGAcode := 'FCLASS';
      LongName := 'FCLASS';
   end
   else if InputDB = 3 then begin
      NGAgazDir := 'C:\mydocs\Papers\icmg_2026\data\osm_geofabrik_buildings\';
      NGAplotoptions := 'C:\mydocs\Papers\icmg_2026\data\geofabrik_building_types.dbf';
      NGAcode := 'TYPE';
      LongName := 'TYPE';
   end;
   PlotOptions := MapOwner.OpenDBonMap('',NGAPlotOptions,false,false);
   TheGroups := GISdb[PlotOptions].MyData.ListUniqueEntriesInDB('GROUP');
   TheCountries := tStringList.Create;
   FindMatchingFiles(NGAgazDir,'*.dbf',TheCountries,0);
   for j := 0 to pred(TheCountries.Count) do begin
      db[j] := MapOwner.OpenDBonMap('',TheCountries.Strings[j],false,false);
      GISdb[db[j]].LayerIsOn := false;
   end;
end;


procedure EndWarCrimes;
var
   j : integer;
begin
   CloseSingleDB(PlotOptions);
   for j := 0 to pred(TheCountries.Count) do begin
      CloseSingleDB(db[j]);
   end;

   TheCountries.Destroy;
   TheGroups.Destroy;
   ShowDefaultCursor;
   SetColorForWaiting;
   wmdem.ClearStatusBarPanelText;
end;


procedure StatsWarCrimesDB(MapOwner : tMapForm; InputDB : integer);
var
   i,j,k : integer;
   Findings : tStringList;
   aFilter,aline,country : shortstring;
   fName : PathStr;
   SubGroups : tStringList;
begin
   {$IfDef WarCrimes} WriteLineToDebugFile('StatsWarCrimesDB in'); {$EndIf}
   SetWarCrimesSettings(MapOwner,InputDB);
   SetColorForProcessing;
   ShowHourglassCursor;
   Findings := tStringList.Create;
   aline := 'GROUP,SUBGROUP';
   for j := 0 to pred(TheCountries.Count) do begin
      country := UpperCase(ExTractFileNameNoExt(TheCountries.Strings[j]));
      country := StringReplace(Country,'_BUILDINGS','',[rfReplaceAll, rfIgnoreCase]);
      country := StringReplace(Country,'_POIS_FREE','',[rfReplaceAll, rfIgnoreCase]);
      aline := aline + ',' + Country;
   end;
   Findings.Add(aline);

    for i := 0 to pred(theGroups.Count) do begin
       wmDEM.SetPanelText(2,IntToStr(succ(i)) + '/' + IntToStr(theGroups.Count) + '  ' + theGroups.Strings[i] ,true);
       {$IfDef WarCrimes} WriteLineToDebugFile(IntToStr(succ(i)) + '/' + IntToStr(theGroups.Count) +' '  + theGroups.Strings[i]); {$EndIf}
       GISdb[PlotOptions].ApplyGISFilter('USE=' + QuotedStr('Y') + ' AND GROUP=' + QuotedStr(TheGroups.Strings[i]));
       Subgroups := GISdb[PlotOptions].MyData.ListUniqueEntriesInDB(NGAcode);

       Aline := theGroups.Strings[i] + ',ALL';
       aFilter := MakeOrFilter(PlotOptions,NGAcode, GISdb[PlotOptions].MyData.ListUniqueEntriesInDB(NGAcode));
       for j := 0 to pred(TheCountries.Count) do begin
          GISdb[db[j]].ApplyGISFilter(aFilter);
          aline := aline + ',' + IntToStr(GISdb[db[j]].MyData.FiltRecsInDB);
       end;
       Findings.Add(aLine);
       if (SubGroups.Count > 0) then begin
         for k := 0 to pred(Subgroups.Count) do begin
            wmDEM.SetPanelText(3,IntToStr(succ(k)) + '/' + IntToStr(SubGroups.Count) + '  ' + SubGroups.Strings[k],true);
            {$IfDef WarCrimes} WriteLineToDebugFile(IntToStr(succ(k)) + '/' + IntToStr(SubGroups.Count) +' '  + SubGroups.Strings[k] + ' ' + aFilter); {$EndIf}
            GISdb[PlotOptions].ApplyGISfilter(NGAcode + '=' + QuotedStr(SubGroups.Strings[k]));
            Aline := theGroups.Strings[i] + ',' + GISdb[PlotOptions].MyData.GetFieldByNameAsString(LongName);
            for j := 0 to pred(TheCountries.Count) do begin
               aFilter := NGAcode + '=' + QuotedStr(SubGroups.Strings[k]);
               GISdb[db[j]].ApplyGISFilter(aFilter);
               aline := aline + ',' + IntToStr(GISdb[db[j]].MyData.FiltRecsInDB);
            end;
            Findings.Add(aLine);
         end;
       end;
       Subgroups.Destroy;
    end;
   {$IfDef WarCrimes} WriteLineToDebugFile('CSV created'); {$EndIf}
   fName := MDtempDir + GISdb[PlotOptions].dbName + '.dbf';
   StringList2CSVtoDB(Findings,fName);
   EndWarCrimes;
   {$IfDef WarCrimes} WriteLineToDebugFile('StatsWarCrimesDB out'); {$EndIf}
end;

procedure MapWarCrimesDB(MapOwner : tMapForm; InputDB : integer; CountryMaps : boolean = false);
var
   TheMaps : tStringList;
   i,j,k,xp,yp,n : integer;
   Sym   : tDrawingSymbol;
   Lat,Long : float64;
   Size  : byte;
   Color : tPlatformColor;
   aFilter,aTitle : shortstring;
   fName : PathStr;
   AllLayers : tMyBitmap;
   Bitmap : array[0..MaxLayers] of tMyBitmap;
   sName : array[0..MaxLayers] of PathStr;
begin
   {$IfDef WarCrimes} WriteLineToDebugFile('MapWarCrimesDB in'); {$EndIf}
   SetWarCrimesSettings(MapOwner,InputDB);
   SetColorForProcessing;
   ShowHourglassCursor;
   Sym := FilledBox;
   Size := 3;
   CopyImageToBitmap(MapOwner.Image1,AllLayers);
   if CountryMaps then begin
      theMaps := tStringList.Create;
   end;
   for i := 0 to pred(theGroups.Count) do begin
      aTitle := IntToStr(succ(i)) + '/' + IntToStr(theGroups.Count) + '  ' + theGroups.Strings[i];
      wmDEM.SetPanelText(2,aTitle,true);
      {$IfDef WarCrimes} WriteLineToDebugFile(aTitle); {$EndIf}

      if CountryMaps then CopyImageToBitmap(MapOwner.Image1,Bitmap[i]);
      GISdb[PlotOptions].ApplyGISFilter('USE=' + QuotedStr('Y') + ' AND GROUP=' + QuotedStr(TheGroups.Strings[i]));
      n := 0;
      for j := 0 to pred(TheCountries.Count) do begin
         ShowHourglassCursor;
         aTitle := IntToStr(succ(j)) + '/' + IntToStr(TheCountries.Count) + '  ' + TheCountries.Strings[j];
         wmDEM.SetPanelText(3,aTitle,true);
         {$IfDef WarCrimes} WriteLineToDebugFile(aTitle); {$EndIf}
          if (GISdb[PlotOptions].MyData.FiltRecsInDB > 0) then begin
              Color := GISdb[PlotOptions].MyData.PlatformColorFromTable;
              aFilter := MakeOrFilter(PlotOptions,NGAcode, GISdb[PlotOptions].MyData.ListUniqueEntriesInDB(NGAcode));
              {$IfDef WarCrimes} WriteLineToDebugFile(GISdb[db[j]].dbName + '  ' + aFilter); {$EndIf}
              GISdb[db[j]].ApplyGISFilter(aFilter);
              if GISdb[db[j]].MyData.FiltRecsInDB > 0 then begin
                  GISdb[db[j]].EmpSource.Enabled := false;
                  while not GISdb[db[j]].MyData.eof do begin
                    if GISdb[db[j]].ValidLatLongFromTable(Lat,Long) then begin
                       MapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
                       if MapOwner.MapDraw.OnScreen(xp,yp) then begin
                          inc(n);
                          if CountryMaps then PETMAR.ScreenSymbol(Bitmap[i].Canvas,xp,yp,Sym,Size,Color);
                          PETMAR.ScreenSymbol(AllLayers.Canvas,xp,yp,Sym,Size,Color);
                       end;
                    end;
                    GISdb[db[j]].MyData.Next;
                  end {while};
              end;
          end {if};
          fName := MDtempDir + TheGroups.Strings[i] + '.png';
          {$IfDef WarCrimes} WriteLineToDebugFile(TheGroups.Strings[i] + '  n=' + IntToStr(n)); {$EndIf}
      end {for j};
      if CountryMaps then begin
          Bitmap[i].Canvas.Font.Size := 36;
          Bitmap[i].Canvas.Font.Color := ConvertPlatformColorToTColor(Color);
          Bitmap[i].Canvas.Font.Style := [fsBold];
          aTitle := theGroups.Strings[i] + '  (n=' + IntToStr(n) + ')';
          Bitmap[i].Canvas.TextOut(5,Bitmap[i].Height - 5 - Bitmap[i].Canvas.TextHeight(aTitle),aTitle);
          sName[i] := MDtempDir + GISdb[PlotOptions].dbName + '_' + theGroups.Strings[i] + '.png';
          SaveBitmap(Bitmap[i],sName[i]);
          TheMaps.Add(sName[i]);
      end
      else begin
         {$IfDef WarCrimesLayerMaps}
            sName[i] := MDtempDir + GISdb[PlotOptions].dbName + '_' + theGroups.Strings[i] + '.png';
            SaveBitmap(AllLayers,sName[i]);
         {$EndIf}
      end;
   end {for i};
   if CountryMaps then MakeBigBitmap(theMaps,Uppercase(ExtractFileNameNoExt(NGAplotoptions)),'',3);
   CloseSingleDB(PlotOptions);
   EndWarCrimes;
   MapOwner.Image1.Picture.Graphic := AllLayers;
   //MessageToContinue('What am I doing');
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




