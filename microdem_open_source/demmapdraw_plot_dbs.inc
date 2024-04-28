{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{ include file for demmapdraw       }
{___________________________________}


procedure TMapDraw.LabelDataBases(var Bitmap : tMyBitmap);
var
   i,j : integer;
begin
   {$IfDef RecordDBPlots} WriteLineToDebugFile('TMapDraw.LabelDataBases in'); {$EndIf}
   for j := MaxDataBase downto 1 do begin
      i := DBPlotOrder[j];
      if (GISdb[i] <> Nil) and (GISdb[i].theMapOwner <> Nil) and (DBonthisMap[i]) and (GISdb[i].LayerIsOn) and (GISdb[i].dbOpts.LabelDBPlots) and (not GISdb[i].PLSSfile) then begin
         {$IfDef RecordDBPlots} WriteLineToDebugFile('Label db=' + IntToStr(i) + '  ' + GISDB[i].dbName + '   recs=' + IntToStr(GISDB[i].MyData.FiltRecsInDB)); {$EndIf}
         GISdb[i].LabelRecordsOnMap(Bitmap);
      end;
   end;
   {$IfDef RecordDBPlots} WriteLineToDebugFile('TMapDraw.LabelDataBases out'); {$EndIf}
end;


procedure TMapDraw.QuickShapeFileDisplay(var Bitmap : tMyBitmap; fName : PathStr; ColorFieldPresent : boolean = false; color : tColor = -1);
var
   GISNum : integer;

  function OpenTheDB : boolean;
  begin
      {$IfDef FMX}
         Result :=  OpenGISDataBase(GISNum,'',GISdb[GISNum],fName,false,Self);
      {$EndIf}
      {$IfDef VCL}
         Result :=  OpenNumberedGISDataBase(GISNum,fName,false,true,Nil);
      {$EndIf}
  end;


begin
   if OpenTheDB then begin
      {$IfDef RecordDBPlots} WriteLineToDebugFile('TMapDraw.QuickShapeFileDisplay  GIS=' + GISNum.ToString + '   ' + fName); {$EndIf}
      {$IfDef VCL}
         if (Color > -1) then begin
            Bitmap.Canvas.Pen.Color := Color;
            Bitmap.Canvas.Brush.Color := Color;
            Bitmap.Canvas.Brush.Style := bsSolid;
            GISDB[GISNum].dbOpts.LineColor := ConvertTColorToPlatformColor(Color);
            GISDB[GISNum].dbOpts.FillColor := ConvertTColorToPlatformColor(Color);
         end;
      {$EndIf}

      {$IfDef FMX}
         Bitmap.Canvas.BeginScene;
         Bitmap.Canvas.Stroke.Color := ConvertTColorToPlatformColor(Color);
      {$EndIf}

      if GISDB[GISNum].LatLongFieldsPresent then begin
         {$IfDef VCL}
            GISDB[GISNum].PlotAndLabelPoints(Bitmap,true);
         {$EndIf}
      end
      else begin
         GISDB[GISNum].aShapeFile.PlotAllRecords(Self,Bitmap);
      end;

      {$IfDef FMX} Bitmap.Canvas.EndScene; {$EndIf}
      CloseAndNilNumberedDB(GISNum);
   end;
end;


procedure TMapDraw.PlotDataBaseLegends(var Bitmap : tMyBitmap);
var
   i,j : integer;
begin
   for j := MaxDataBase downto 1 do begin
      i := DBPlotOrder[j];
      if ValidDB(i) and (GISdb[i].MyData.FiltRecsInDB > 0) then begin
         if (GISdb[i].LayerIsOn) and DBPlotNow[i] and ( (DBonThisMap[i]) or (MDdef.DBsOnAllMaps and ( not GISdb[i].ShowLocalMapOnly))) then begin
            PlotDBLegend(i,Bitmap);
         end;
      end;
   end;
end;


procedure TMapDraw.PlotDataBase(DBNum : integer; var Bitmap : tMyBitmap);
var
   PlotLayer : boolean;
begin
   if ValidDB(DBNum) then with GISDB[DBNum] do begin
      if CanPlot and (MyData <> Nil) then begin
         {$IfDef RecordDBPlot} WriteLineToDebugFile('TGISDataBase.PlotDataBase in ' + DBName + '  Recs=' + IntToStr(MyData.RecordCount) + '   Mode=' + IntToStr(dbOpts.DBAutoShow)); {$EndIf}
         if ItsAShapeFile and (not AFullWorldMap) and (not LatLongBoxOnScreen(aShapeFile.MainFileHeader.BoundBox)) then begin
            {$IfDef RecordDBPlot} WriteLineToDebugFile('TGISDataBase.PlotDataBase in ' + DBName + '  not on map'); {$EndIf}
            exit;
         end;

         {$IfDef VCL}
            if (dbOpts.ConnectUnderPoints) then ConnectSequentialPoints(Bitmap);
            if (dbOpts.ShowBuffers) then begin
                if AreaShapeFile(ShapeFileType) then begin
                   MaskIn := MDDef.ShapeMaskNearInsideAreas;
                   MaskingDistance := MDDef.ShapeAreaBufferDist;
                end
                else if LineShapeFile(ShapeFileType) then begin
                   MaskIn := MDDef.ShapeMaskNearLines;
                   MaskingDistance := MDDef.ShapeLineBufferDist;
                end
                else begin
                   MaskIn := MDDef.ShapeMaskNearPoints;
                   MaskingDistance := MDDef.ShapePointBufferDist;
                end;
                AddDatabaseToMask(DBNumber,Bitmap,true,MaskIn,MaskingDistance,MDDef.MapMaskColor);
                if MaskIn then ChangeBitmapBlackWhite(Bitmap,MDDef.MapMaskColor,rgbTripleWhite)
                else ChangeBitmapBlackWhite(Bitmap,rgbTripleWhite,MDDef.MapMaskColor);
                PlotLayer := dbOpts.SymbolsWithBuffers;
            end
            else PlotLayer := true;

            if PlotLayer then begin
               if (dbOpts.DBAutoShow in [dbasColorByNumeric,dbasMonthlyTemp,dbasMonthlyRain,dbasMultiFieldRGB,dbasColorPosNeg]) then PlotDBNumericField(Bitmap)
               {$IfDef ExTiger} {$Else} else if ItsTigerShapeFile or (ItsOSMShapeFile and StrUtils.AnsiContainsText(dbName,'roads')) then PlotTigerShapeFileOnMap(DBNum,Bitmap)  {$EndIf}
               else if (dbOpts.DBAutoShow = dbasVector) then PlotVectorsOnMap(Bitmap)
               else if (dbOpts.dbAutoShow in [dbasTTFontSymbol,dbasScaledSquares,dbasScaledCircles]) then PlotProportionalSymbolsOnMap(Bitmap)
               else if (dbOpts.DBAutoShow in [dbasIconAll,dbasIconField,dbasPointsInDB]) then PlotIcons(Bitmap)
               else if (dbOpts.DBAutoShow in [dbasZipaToneAll,dbasZipaToneField]) then PlotZipATone(Bitmap)
               else if (dbOpts.DBAutoShow = dbasColorByString) then PlotDBStringField(Bitmap)
               else if (dbOpts.DBAutoShow in [dbasColorField,dbasColorJoinField]) then PlotColorField(Bitmap)
               else if (dbOpts.DBAutoShow = dbasDefault) then PlotDefaultSymbols(Self,Bitmap)
               else if (dbOpts.DBAutoShow = dbasConnectTwoPointsInRec) then ConnectTwoPointsInSameRecord(Bitmap)
               else if (dbOpts.dbAutoShow in [dbasGaz]) then TheMapOwner.MapDraw.LabelGazetteerFeatures(Bitmap)
               else if (dbOpts.DBAutoShow = dbasZValues) then aShapeFile.PlotPointCloudOnMap(theMapOwner,Bitmap,dbOpts.ZColorMin,dbOpts.ZColorMax,dbOpts.LineWidth)
               else if (dbOpts.DBAutoShow = dbasConnectSeqPts) then ConnectSequentialPoints(Bitmap)
               {$IfDef ExGeography} {$Else} else if (dbOpts.DBAutoShow = dbasKoppen) then PlotKoppenStations(Bitmap) {$EndIf}
               {$IfDef ExGeostats} {$Else} else if (dbOpts.DBAutoShow = dbasTerrainFabric) then PlotFabric(Bitmap) {$EndIf}
               {$IfDef ExGeology}
               {$Else}
                  else if (DipStrikeFieldExists or DipAndStrikeFieldsExist) and (dbOpts.DBAutoShow = dbasDipStrike) then PlotDipsAndStrikes(Bitmap)
                  else if (dbOpts.DBAutoShow in [dbasBeachBall,dbasQuakeMechColor]) then PlotEarthQuakesByMechanism(Bitmap)
                  else if (dbOpts.DBAutoShow = dbasTernary) then PlotTernary(Bitmap)
               {$EndIf}
               else begin
               end;

               if dbOpts.Grayscale then MakeTheBitmapGrayScale(Bitmap);
               if dbOpts.Subdue then MakeTheBitmapSubdued(Bitmap);
            end;
         {$EndIf}
      end;
      ShowStatus;
   end;
   {$IfDef RecordDBPlot} WriteLineToDebugFile('TGISDataBase.PlotDataBase out, ' + DBName + '   Mode=' + IntToStr(dbOpts.DBAutoShow) + '  Recs=' + IntToStr(MyData.RecordCount)); {$EndIf}
end;


procedure TMapDraw.PlotDataBases(var Bitmap : tMyBitmap);
var
   i,j : integer;
   Overlay : tMyBitmap;
begin
   if ClosingIsHappening or (not MapDrawValid) or ClosingMapNow then exit;
   {$IfDef RecordDBPlots} WriteLineToDebugFile('TMapDraw.PlotDataBases in, on map ' + BaseTitle); {$EndIf}
   if (NumOpenDB > 0) then for j := MaxDataBase downto 1 do begin
      i := DBPlotOrder[j];
      if ValidDB(i) then begin
         if (GISdb[i].MyData.FiltRecsInDB > 0) and (not GISdb[i].PLSSfile) then begin
            if (GISdb[i].LayerIsOn) and DBPlotNow[i] and ( (DBonThisMap[i]) or (MDdef.DBsOnAllMaps and ( not GISdb[i].ShowLocalMapOnly))) then begin
               {$IfDef RecordDBPlots} WriteLineToDebugFile('  Plot db=' + IntToStr(i) + '  ' + GISDB[i].dbName + '   recs=' + IntToStr(GISDB[i].MyData.FiltRecsInDB)); {$EndIf}
               if (DBOverlayfName[i] <> '') then begin
                  Overlay := LoadBitmapFromFile(DBOverlayfName[i]);
                  {$IfDef RecordDBPlots} WriteLineToDebugFile('Loaded=' + DBOverlayfName[i]); {$EndIf}
               end
               else begin
                  CreateBitmap(Overlay,Bitmap.Width,Bitmap.Height);
                  PlotDataBase(i,Overlay);
                  DBOverlayfName[i] := NextFileNumber(MDTempDir, 'db_overlay', OverlayFExt);
                  PetImage.SaveBitmap(Overlay,DBOverlayfName[i]);
                  {$IfDef RecordDBPlots} WriteLineToDebugFile('Create & save=' + DBOverlayfName[i]); {$EndIf}
               end;
               DrawOverlayNoDelete(Bitmap,Overlay,GISdb[i].dbOpts.Opacity);
            end;
            if (GISdb[i].dbTablef <> Nil) and (GISdb[i].dbTablef.BaseMapBitmap <> Nil) then FreeAndNil(GISdb[i].dbTablef.BaseMapBitmap);
         end;
      end;
   end;
   {$IfDef RecordDBPlots} WriteLineToDebugFile('TMapDraw.PlotDataBases out'); {$EndIf}
end;


procedure TMapDraw.PlotLineShapeFileOnMap(fName : PathStr; var Bitmap : tMyBitmap; LineColor : tPlatformColor; LineWidth : byte);
var
   sf : tShapeFile;
   Success : boolean;
begin
   {$IfDef RecordDBPlot} WritelineToDebugFile('PlotLineShapeFileOnMap ' + fName); {$EndIf}
   if FileExists(fName) then begin
     try
       sf := tShapeFile.Create(fName,Success);
       if Success then begin
          Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(LineColor);
          Bitmap.Canvas.Pen.Width := LineWidth;
          Bitmap.Canvas.Brush.Style := bsClear;
          sf.PlotAllRecords(Self,Bitmap);
       end
       else begin
          {$IfDef RecordDBPlot} WritelineToDebugFile('Failed to open shp'); {$EndIf}
       end;
     finally
        sf.Destroy;
     end;
   end;
end;


procedure TMapDraw.PlotShapeFileGroupOnMap(IndexFName : PathStr; Bitmap : tMyBitmap; BaseDir : PathStr = '');
var
   x,y,i : integer;
   Label_Field : string10;
   fName2,BaseName : PathStr;
   fList : tStringList;
   LabelIt,
   TryLabels,
   PlotIt : boolean;
   inTheData : tMyData;
   GISNum : integer;
begin
    {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('PlotShapeFileGroupOnMap in ' + IndexFName + '  ' + SmartMemorySizeBytes(GetFileSize(IndexfName))); {$EndIf}
    inTheData := TMyData.Create(IndexFName);
    {$IfDef RecordFullShapeFileGroup} WriteLineToDebugFile('PlotShapeFileGroupOnMap loaded ' + IndexFName); {$EndIf}
    if (not ThreadsWorking) then ShowHourglassCursor;
    TryLabels := inTheData.FieldExists('LABEL');
    inTheData.ApplyFilter('PLOT =' + QuotedStr('Y'));
    if (inTheData.RecordCount = 0) then begin
       MessageToContinue('No used layers in ' + IndexFName);
       {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('No used layers in ' + IndexFName); {$EndIf}
    end
    else for i := 0 to 9 do begin
        inTheData.ApplyFilter('PLOT_ORDER = ' + IntToStr(i) + ' AND PLOT =' + QuotedStr('Y'));
        {$IfDef RecordFullShapeFileGroup} if (inTheData.FiltRecsInDB > 0) then WriteLineToDebugFile(inTheData.Filter + '  layers=' + IntToStr(inTheData.FiltRecsInDB)); {$EndIf}
        if (inTheData.FiltRecsInDB > 0) then begin
           inTheData.First;
           while not inTheData.EOF do begin
              BaseName := inTheData.GetFieldByNameAsString('FILENAME');
              fName2 := BaseDir + BaseName;
              if not FileExists(fName2) then begin
                 fList := nil;
                 Petmar.FindMatchingFiles(BaseDir,BaseName,fList,4);
                 if (flist.Count = 1) then fName2 := fList.Strings[0];
                 fList.Destroy;
              end;
              if FileExists(fName2) then begin
                 {$IfDef RecordFullShapeFileGroup} WriteLineToDebugFile('Plot: ' + fName2); {$EndIf}
                 LabelIt := TryLabels and (inTheData.GetFieldByNameAsString('LABEL') = 'Y');
                 if LabelIt then begin
                    inTheData.DefineFontFromTable(Bitmap.Canvas.Font);
                    Bitmap.Canvas.Brush.Style := bsClear;
                    Label_Field := inTheData.GetFieldByNameAsString('LAB_FIELD');
                 end;
                 {$IfDef RecordFullShapeFileGroup} WriteLineToDebugFile('Opening GIS module for ' + fName2); {$EndIf}

                 if OpenNumberedGISDataBase(GISNum,fName2) then begin
                     if inTheData.FieldExists('FILTER') and (inTheData.GetFieldByNameAsString('FILTER') <> '') then begin
                         GISDB[GISNum].MyData.ApplyFilter(inTheData.GetFieldByNameAsString('FILTER'));
                        {$IfDef RecordFullShapeFileGroup} WriteLineToDebugFile('Filter: ' +  GISDB[GISNum].MyData.Filter + '  recs=' + IntToStr( GISDB[GISNum].MyData.FiltRecsInDB)); {$EndIf}
                     end;
                     if inTheData.FieldExists('PIXEL_SIZE') and (inTheData.GetFieldByNameAsString('PIXEL_SIZE') <> '') then begin
                        PlotIt := (ScreenPixelSize < inTheData.GetFieldByNameAsInteger('PIXEL_SIZE'));
                     end
                     else PlotIt := true;

                     if PlotIt and (GISDB[GISNum].MyData.FiltRecsInDB > 0) then begin
                       {$IfDef RecordFullShapeFileGroup} WriteLineToDebugFile('Plot file'); {$EndIf}
                        GISDB[GISNum].SetColorsFromDB(inTheData);
                        GISDB[GISNum].PrepColors(Bitmap);
                        if (not ThreadsWorking) then ShowHourglassCursor;
                        if (GISDB[GISNum].aShapeFile <> Nil) then begin
                           {$IfDef ExOSM}
                           {$Else}
                              if GISDB[GISNum].ItsOSMShapeFile then GISDB[GISNum].aShapeFile.BadPointCheck := MDDef.OSMCheck;
                           {$EndIf}

                           if (GISDB[GISNum].MyData.Filter = '') then begin
                              {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('Use GISDB[GISNum].aShapeFile.PlotAllRecords'); {$EndIf}
                              GISDB[GISNum].aShapeFile.PlotAllRecords(Self,Bitmap,GISDB[GISNum].ItsOSMShapeFile);
                           end
                           else begin
                              {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('Use GISDB[GISNum].aShapeFile.PlotSingleRecord'); {$EndIf}
                              GISDB[GISNum].MyData.First;
                              while not GISDB[GISNum].MyData.EOF do begin
                                 GISDB[GISNum].aShapeFile.PlotSingleRecordMap(Self,Bitmap,GISDB[GISNum].MyData.RecNo);
                                 GISDB[GISNum].MyData.Next;
                              end;
                           end;
                        end
                        else begin
                           {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('Use GISDB[GISNum].DisplayCurrentRecordOnMap, not SHP'); {$EndIf}
                           GISDB[GISNum].MyData.First;
                           while not GISDB[GISNum].MyData.EOF do begin
                              GISDB[GISNum].DisplayCurrentRecordOnMap(GISDB[GISNum].TheMapOwner.MapDraw,Bitmap);
                              GISDB[GISNum].MyData.Next;
                           end;
                        end;
                        if LabelIt then begin
                          {$IfDef RecordFullShapeFileGroup} WriteLineToDebugFile('Label records'); {$EndIf}
                          GISDB[GISNum].MyData.First;
                          while not GISDB[GISNum].MyData.EOF do begin
                             if GISDB[GISNum].ValidScreenPositionFromTable(x,y) then begin
                                Bitmap.Canvas.TextOut(x+5,y+5,GISDB[GISNum].MyData.GetFieldByNameAsString(Label_Field));
                             end;
                             GISDB[GISNum].MyData.Next;
                          end;
                        end;
                     end
                     else begin
                        {$IfDef RecordFullShapeFileGroup} WriteLineToDebugFile('File does not plot at this scale'); {$EndIf}
                     end;
                     CloseAndNilNumberedDB(GISNum);
                 end
                 else begin
                    {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('File was not OK'); {$EndIf}
                 end;
              end
              else begin
                 {$IfDef RecordShapeFileGroup} WriteLineToDebugFile(fName2 + ' does not exist'); {$EndIf}
              end;
              inTheData.Next;
              if WantOut then break;
           end;
        end;
        if WantOut then break;
    end;
    ShowDefaultCursor;
    inTheData.Destroy;
    {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('PlotShapeFileGroupOnMap out'); {$EndIf}
end;


{$IfDef ExOSM}
{$Else}

      procedure TMapDraw.OSMthread(Layer : integer; fName : PathStr; var Bitmap : tMyBitmap);
      var
         adb : integer;
         fList : tstringlist;
      begin
         if (Layer = 1) then begin
            PlotShapeFileGroupOnMap(OSMGroupRules,Bitmap,fName);
         end
         else begin
             fList := nil;
             Petmar.FindMatchingFiles(fName,'roads.shp',fList,4);
             if (flist.Count = 1) then fName := fList.Strings[0];
             fList.Destroy;
             if OpenNumberedGISDataBase(adb,fName) then begin
               {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('start roads'); {$EndIf}
               if GISdb[aDB].ItsOSMShapeFile then GISdb[adb].aShapeFile.BadPointCheck := MDDef.OSMCheck;
               if Layer = 3 then HighlightOSMBridges := true;

               PlotDBOnMapFromRules(adb,Bitmap,OSMRoadRules);
               CloseAndNilNumberedDB(adb);
            end;
         end;
      end;

{$EndIf}

procedure TMapDraw.QuietPlotTIGERShapeFileOnMap(fName : PathStr; var Bitmap : tMyBitmap; RoadsOnly : boolean = false; MaskSize : integer = -1; StreamsOnly : boolean = false);
var
   GISNum : integer;
begin
   {$IfDef RecordTiger} WriteLineToDebugFile('PlotTIGERShapeFileOnMap ' + fName); {$EndIf}
   if OpenNumberedGISDataBase(GISNum,fName) then begin
      PlotTigerShapeFileOnMap(GISNum,Bitmap,RoadsOnly,MaskSize,StreamsOnly);
      CloseAndNilNumberedDB(GISNum);
   end;
end;


         procedure TMapDraw.PlotTigerShapeFileOnMap(DBNum : integer; var Bitmap : tMyBitmap; RoadsOnly : boolean = false; MaskSize : integer = -1; StreamsOnly : boolean = false);
         var
            x,y : integer;
            Lat,Long : float64;
            LabelBitmap : tMyBitmap;
            StreetsPlotted : tStringList;
            Labelled : boolean;
            FEname : ShortString;
         begin
            with GISdb[DBNum] do begin
               {$If Defined(RecordDBPlot)} WriteLineToDebugFile('tGISDataBase.PlotTigerShapeFileOnMap in, DrawLabels=' + TrueOrFalse(MDDef.TigrDef.DrawLabels)  + ' AppearLabels=' + IntToStr(MDDef.TigrDef.AppearLabels)); {$EndIf}
               {$IfDef ExOSM}
               {$Else}
                  if ItsOSMShapeFile then begin
                    {$IfDef RecordTiger} WriteLineToDebugFile('OSM shapefile, ' +  GISdb[DBNum].dbName); {$EndIf}
                    PlotDBOnMapFromRules(DBNum,Bitmap,OSMRoadRules);
                    HighlightOSMBridges := true;
                    PlotDBOnMapFromRules(DBNum,Bitmap,OSMRoadRules);
                 end;
               {$EndIf}

               if ItsTigerShapeFile then begin
                 {$IfDef RecordTiger} WriteLineToDebugFile('TIGER shapefile, ' +  GISdb[DBNum].dbName); {$EndIf}
                 if StreamsOnly then dbOpts.MainFilter := '(HYDROFLG=' + QuotedStr('Y') + 'OR MTFCC=' + QuotedStr('P0002') + 'OR MTFCC=' + QuotedStr('P0003') + ')'
                 else if RoadsOnly then dbOpts.MainFilter := 'ROADFLG=' + QuotedStr('Y');

                 PlotDBOnMapFromRules(DBNum,Bitmap,TigerShapeRules,MaskSize);
                 LabelBitmap := Nil;

                 if MDDef.TigrDef.DrawLabels and MDDef.TigrDef.AutoAppear and (ScreenPixelSize < MDDef.TigrDef.AppearLabels) then begin
                    ShowHourglassCursor;
                    EmpSource.Enabled := false;
                    CloneBitmap(Bitmap,LabelBitmap);
                    Bitmap.Canvas.Brush.Style := bsClear;
                    StreetsPlotted := tStringList.Create;
                    StreetsPlotted.Sorted := true;
                    StreetsPlotted.Duplicates := dupIgnore;

                    if MyData.FieldExists('ROADFLG') then dbOpts.MainFilter :=  'ROADFLG=' + QuotedStr('Y');
                    AssembleGISFilter;

                    {$IfDef RecordTiger} WriteLineToDebugFile('Text labels  Filter=' + MyData.Filter + '  recs=' + IntToStr(MyData.FiltRecsInDB)); {$EndIf}

                     while not MyData.EOF do begin
                       FEName := MyData.GetFieldByNameAsString('FULLNAME');
                       ShortenTigerName(FEName);
                       if (StreetsPlotted.IndexOf(FEName) = -1) or MDDef.LabelEveryTigerFeature then begin
                          try
                             aShapeFile.LineCenter(MyData.RecNo,Long,Lat);
                             LatLongDegreeToScreen(Lat,long,x,y);
                             if OnScreen(x,y) then begin
                                x := x - Bitmap.Canvas.TextWidth(FEName) div 2;
                                y := y - Bitmap.Canvas.TextHeight(FEName) div 2;
                                Labelled := true;
                                if MDDef.AvoidTextOverprints then Labelled := PetImage.SmartTextOut(Bitmap,LabelBitmap,x,y,FEName)
                                else Bitmap.Canvas.TextOut(x,y,FEName);
                                if Labelled then StreetsPlotted.Add(FEName);
                             end;
                          except
                             on Exception do begin end;
                          end;
                       end;
                       MyData.Next;
                     end;
                     if (LabelBitmap <> Nil) then FreeAndNil(LabelBitmap);
                     StreetsPlotted.Free;
                     ShowDefaultCursor;
                     EmpSource.Enabled := true;
                     GISdb[DBNum].ClearGISFilter;
                  end;
               end;
               if MDDef.MakeTigerMapGrayscale then MakeTheBitmapGrayScale(Bitmap);
               if MDDef.SubdueTigerBase then MakeTheBitmapSubdued(Bitmap);
               {$IfDef RecordTiger} WriteLineToDebugFile('out'); {$EndIf}
            end;
         end;


         procedure TMapDraw.PlotDBOnMapFromRules(DBNum : integer; var Bitmap : tMyBitmap; Rules : PathStr; MaskSize : integer = -1);
         var
            RulesForTheData : tMyData;
            LineSize : integer;
            Color : tColor;
            aFilter : ShortString;
         begin
            with GISdb[DBNum] do begin
               {$IfDef RecordPlotDBRules} if (theMapOwner <> nil) then WriteLineToDebugFile('tGISDataBase.PlotDBOnMapFromRules in, with rules=' + ExtractFileName(Rules) + '  map pixel size=' + IntToStr(round(TheMapOwner.MapDraw.ScreenPixelSize))); {$EndIf}
               if FileExists(Rules) then begin
                   RulesForTheData := tMyData.Create(Rules);
                   ShowHourglassCursor;
                   EmpSource.Enabled := false;
                   if (MaskSize > 0) then begin
                       LineSize := round(MaskSize * 2 / TheMapOwner.MapDraw.ScreenPixelSize);
                       if ItsAPointDB  then LineSize := LineSize div 2;
                       if (LineSize < 1) then LineSize := 1;
                       Bitmap.Canvas.Pen.Width := LineSize;
                   end;
                   while (not RulesForTheData.EOF) do begin
                      {$IfDef RecordPlotDBRules} WriteLineToDebugFile(RulesForTheData.GetFieldByNameAsString('NAME')+'  plot beyond pixel size=' + IntToStr(round(RulesForTheData.GetFieldByNameAsInteger('PIXEL_SIZE'))));                     {$EndIf}
                      if (RulesForTheData.GetFieldByNameAsString('NAME') <> '') then begin
                        if (RulesForTheData.GetFieldByNameAsString('PLOT') = 'Y') then begin
                           if (not(MDDef.UsePixelSizeRules) or (ScreenPixelSize <= RulesForTheData.GetFieldByNameAsInteger('PIXEL_SIZE'))) then begin
                               EmpSource.Enabled := false;
                               if (MaskSize <= 0) then begin
                                  if LineOrAreaShapeFile(ShapeFileType) then begin
                                     Bitmap.Canvas.Pen.Color := RulesForTheData.GetFieldByNameAsInteger('LINE_COLOR');
                                     Bitmap.Canvas.Pen.Width := RulesForTheData.GetFieldByNameAsInteger('LINE_WIDTH');
                                     if AreaShapeFile(ShapeFileType) then Bitmap.Canvas.Brush.Color := RulesForTheData.GetFieldByNameAsInteger('FILL_COLOR');
                                  end
                                  else begin
                                     RulesForTheData.DefinePointSymbol(dbOpts.Symbol.DrawingSymbol,dbOpts.Symbol.Size,dbOpts.Symbol.Color);
                                     Bitmap.Canvas.Pen.Color := Color;
                                  end;
                               end;
                               aFilter := PetDBUtils.AddAndIfNeeded(dbOpts.GeoFilter) + PetDBUtils.AddAndIfNeeded(dbOpts.MainFilter) + RulesForTheData.GetFieldByNameAsString('FILTER');
                               if (Rules = OSMRoadRules) then begin
                                  Bitmap.Canvas.Pen.Width := Bitmap.Canvas.Pen.Width * 2;
                                  if HighlightOSMBridges then begin
                                    afilter := afilter + ' AND BRIDGE=1';
                                    Bitmap.Canvas.Pen.Color := clBlack;
                                    Bitmap.Canvas.Pen.Width := 5;
                                  end;
                              end;
                               if (aFilter = '') then begin
                                  aShapeFile.PlotAllRecords(TheMapOwner.MapDraw,Bitmap);
                               end
                               else begin
                                  MyData.ApplyFilter(aFilter);
                                  {$IfDef RecordPlotDBRules} WriteLineToDebugFile(MyData.Filter + '  recs=' + IntToStr(MyData.FiltRecsInDB) + '   Color=' + IntToStr(Bitmap.Canvas.Pen.Color)+ '  Line width=' + IntToStr(Bitmap.Canvas.Pen.Width));                                 {$EndIf}
                                  MyData.First;
                                  while not MyData.EOF do begin
                                     aShapeFile.PlotSingleRecordMap(Self,Bitmap,MyData.RecNo);
                                     MyData.Next;
                                  end;
                               end;
                               if (Rules = OSMRoadRules) then Bitmap.Canvas.Pen.Width := Bitmap.Canvas.Pen.Width div 2;
                             end
                             else begin
                               {$IfDef RecordPlotDBRules} WriteLineToDebugFile('Not plot for size'); {$EndIf}
                             end;
                         end
                         else begin
                            {$IfDef RecordPlotDBRules} WriteLineToDebugFile('PLOT="N"'); {$EndIf}
                         end;
                      end
                      else begin
                         {$IfDef RecordPlotDBRules} WriteLineToDebugFile('NAME=""'); {$EndIf}
                      end;
                      RulesForTheData.Next;
                    end;
                    RulesForTheData.Destroy;
                    HighlightOSMBridges := false;
                    MyData.ApplyFilter(dbOpts.MainFilter);
                    ShowDefaultCursor;
                    EmpSource.Enabled := true;
                end
                else begin
                   MessageToContinue('Missing ' + Rules + MessLineBreak + 'Cannot plot ' + dbFullName);
                end;
            end;
         end;


            procedure TMapDraw.PlotShapeFileGroup(inBitmap : tMyBitmap; FName : PathStr; SaveLayerAllowed : boolean = true);
            var
               Bitmap : tMyBitmap;
               BMPname : PathStr;
               Opacity : byte;
            begin
                if PathIsValid(fName) then begin
                   {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('PlotShapeFileGroup in ' + fName); {$EndIf}
                   {$IfDef ExOSM}
                   {$Else}
                      BMPName := OSMOverlayFName;
                      Opacity := MDDef.OSMopacity;
                   {$EndIf}
                end
                else begin
                   BMPName := CartoDBfName;
                   Opacity := 100;
                end;

                Bitmap := Nil;
                if (BMPName <> '') then begin
                   {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('Reading ' + BMPName); {$EndIf}
                   Bitmap := PetImage.LoadBitmapFromFile(BMPName);
                end
                else if (FileExists(fName) or PathIsValid(fName)) then begin
                   CreateBitmap(Bitmap,inBitmap.Width,inBitmap.Height);
                   if PathIsValid(fName) then begin
                       {$IfDef ExOSM}
                       {$Else}
                          OSMthread(1,fname,Bitmap);   //everything except roads
                          OSMthread(2,fname,Bitmap);   //roads
                          OSMthread(3,fname,Bitmap);   //bridges
                          // 4/29/2018, threading did not increase speed
                          if SaveLayerAllowed then begin
                             OSMOverlayfName := NextFileNumber(MDTempDir, 'osm_overlay', OverlayFExt);
                             SaveBitmap(Bitmap,OSMOverlayfName);
                             {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('Created and saved: ' + OSMOverlayfName); {$EndIf}
                          end;
                       {$EndIf}
                   end
                   else begin
                      PlotShapeFileGroupOnMap(fName,Bitmap);
                      CartoDBfName := NextFileNumber(MDTempDir, 'Carto_overlay', OverlayFExt);
                      SaveBitmap(Bitmap,CartoDBfName);
                      {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('Created and saved: ' + CartoGroupShapesUp); {$EndIf}
                   end;
                   if (Bitmap <> Nil) then DrawAndDeleteOverlay(inBitmap,Bitmap,Opacity);
                   {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('PlotShapeFileGroup out');{$EndIf}
                end
                else begin
                   {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('PlotShapeFileGroup missing ' + fName); {$EndIf}
                end;
            end;



procedure TMapDraw.PlotDBLegend(DBNum : integer; var Bitmap : tMyBitmap);
begin
   with GISDB[DBNum] do begin
      if CanPlot and (MyData <> Nil) then begin
         {$IfDef RecordDBPlot} WriteLineToDebugFile('TGISDataBase.PlotDBLegend in ' + DBName + '  Recs=' + IntToStr(MyData.RecordCount) + '   Mode=' + IntToStr(dbOpts.DBAutoShow)); {$EndIf}
         if ItsAShapeFile and (not LatLongBoxOnScreen(aShapeFile.MainFileHeader.BoundBox)) then begin
            {$IfDef RecordDBPlot} WriteLineToDebugFile('TGISDataBase.PlotDataBase in ' + DBName + '  not on map'); {$EndIf}
            exit;
         end;
        {$IfDef VCL} DoTheLegend(Bitmap); {$EndIf}
      end;
   end;
   {$IfDef RecordDBPlot} WriteLineToDebugFile('TGISDataBase.PlotDBlegend out, ' + DBName + '   Mode=' + IntToStr(dbOpts.DBAutoShow) + '  Recs=' + IntToStr(MyData.RecordCount)); {$EndIf}
end;


procedure TMapDraw.AddDatabaseToMask(DBonTable : integer; var Bitmap : tMyBitmap; UseShapeFile,MaskShapesAreIn : boolean; MaskingDistance : float64; Color : tPlatformColor);
var
   oldDBSaveOptions : tDBSaveOptions;
   x,y,radius : integer;
   GazColors : tMyData;
   NameStr : ShortString;
   MenuStr : shortString;

     procedure GetLineSize(MaskDistance : float64);
     begin
        GISdb[DBonTable].dbopts.LineWidth := round(MaskDistance * 2 / ScreenPixelSize);
        if GISdb[DBonTable].ItsAPointDB then GISdb[DBonTable].dbopts.LineWidth := GISdb[DBonTable].dbopts.LineWidth div 2;
        if (GISdb[DBonTable].dbopts.LineWidth < 1) then GISdb[DBonTable].dbopts.LineWidth := 1;
     end;

     procedure PointMask(MaskingDistance : float64);
     var
        i : integer;
     begin
        GetLineSize(MaskingDistance);
        GISdb[DBonTable].MyData.First;
        StartProgress('Point mask');
        i := 0;
        While not GISdb[DBonTable].MyData.eof do begin
           if i Mod 500 = 0 then begin
              UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
           end;
           LatLongDegreeToScreen(GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LAT'),GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LONG'),x,y);
           Bitmap.Canvas.Ellipse(x-GISdb[DBonTable].dbOpts.LineWidth,y-GISdb[DBonTable].dbOpts.LineWidth,x+GISdb[DBonTable].dbOpts.LineWidth,y+GISdb[DBonTable].dbOpts.LineWidth);
           GISdb[DBonTable].MyData.Next;
        end;
        EndProgress;
     end;


begin
   oldDBSaveOptions := GISdb[DBonTable].dbOpts;
   SetOptionsForMapWithNoMarginalia;
   GISdb[DBonTable].dbopts.LineColor := Color;
   GISdb[DBonTable].dbopts.FillColor := Color;
   GISdb[DBonTable].EmpSource.Enabled := false;
   if GISdb[DBonTable].SimplePointFile then begin
     Bitmap.Canvas.Brush.Style := bsSolid;
     Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(color);
     Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(color);
     Bitmap.Canvas.Pen.Width := 1;
     if (not UseShapefile) then begin    //using gazetteer
        PickGazFeatures;
        SetUpGazFile(GazColors,NameStr);
        MenuStr := '';
        While not GazColors.Eof do begin
           MenuStr := MenuStr + GazColors.GetFieldByNameAsString('NAME') + '  (' + IntToStr(GazColors.GetFieldByNameAsInteger('RADIUS')) + ' m)' + MessLineBreak;
           GazColors.Next;
        end;

        if AnswerIsYes('Modify radius values' + MessLineBreak + MessLineBreak + MenuStr) then begin
           GazColors.First;
           while not GazColors.eof do begin
              Radius := GazColors.GetFieldByNameAsInteger('RADIUS');
              ReadDefault('Radius for ' + GazColors.GetFieldByNameAsString('NAME') + ' (m)',Radius);
              GazColors.Edit;
              GazColors.SetFieldByNameAsInteger('RADIUS',Radius);
              GazColors.Next;
           end;
        end;

        GazColors.First;
        while not GazColors.eof do begin
           GISdb[DBonTable].MyData.ApplyFilter(GISdb[DBonTable].MyData.GetFieldByNameAsString('FILTER'));
           Radius := GazColors.GetFieldByNameAsInteger('RADIUS');
           if (Radius > 0.0000001) then PointMask(Radius);
           GazColors.Next;
        end;
        GazColors.Destroy;
     end
     else begin
        PointMask(MaskingDistance);
     end;
   end
   else begin
     {$IfDef RecordMaskDEMShapeFile} WriteLineToDebugFile('MaskDEMfromshapefile1Click not point shape file'); {$EndIf}
     GetLineSize(MaskingDistance);
     Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(color);
     Bitmap.Canvas.Brush.Style := bsSolid;
     Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(color);
     Bitmap.Canvas.Pen.Width := GISdb[DBonTable].dbOpts.LineWidth;
     GISdb[DBonTable].aShapeFile.PlotAllRecords(GISdb[DBonTable].TheMapOwner.MapDraw,Bitmap);
   end;
   GISdb[DBonTable].EmpSource.Enabled := true;
   GISdb[DBonTable].dbOpts := oldDBSaveOptions;
end;


