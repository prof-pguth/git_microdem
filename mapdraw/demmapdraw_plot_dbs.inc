{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{     include file for demmapdraw        }
{________________________________________}


procedure TMapDraw.PlotDataBases(var Bitmap : tMyBitmap);
var
   i,j,db : integer;
   Overlay : tMyBitmap;
begin
   if NoDBPlots or ClosingIsHappening or (not MapDrawValid) or ClosingMapNow then exit;
   if (NumOpenDB = 0) then begin
   {$If Defined(RecordDBPlots) or Defined(RecordDBPlotsFull) or Defined(RecordDBsIndex)}
      WriteLineToDebugFile('TMapDraw.PlotDataBases in, on map ' + BaseTitle + '  but no open dbs');
   {$EndIf}
   end
   else begin
       {$If Defined(RecordDBPlots) or Defined(RecordDBPlotsFull) or Defined(RecordDBsIndex)}
          WriteLineToDebugFile('TMapDraw.PlotDataBases in, on map ' + BaseTitle + '  dbs=' + IntToStr(NumOpenDB));
       {$EndIf}
(*
   {$IfDef UseDBPlotOrder}
      for j := 1 to MaxDataBase do begin  //we want to plot in order
          for i := 1 to MaxDataBase do begin //go through DBs to find which plots in this order position
             if (DBPlotOrder[j] = i) and ValidDB(i) and (GISdb[i].theMapOwner <> nil) and (GISdb[i].LayerIsOn) then begin
                {$If Defined(RecordDBPlotsFull)} WriteLineToDebugFile('valid db=' + IntToStr(i) + '  ' + GISdb[i].dbName); {$EndIf}
                if (GISdb[i].MyData.FiltRecsInDB > 0) then begin
                  {$If Defined(RecordDBPlotsFull)} WriteLineToDebugFile('filtered records=' + IntToStr(GISdb[i].MyData.FiltRecsInDB)); {$EndIf}
                   if {$IfDef UseDBPlotNow} DBPlotNow[i] and {$EndIf} ( {$IfDef UseDBonThisMap} (DBonThisMap[i]) or {$EndIf} (MDdef.DBsOnAllMaps {and (not GISdb[i].ShowLocalMapOnly)})) then begin
                      {$IfDef Defined(RecordDBPlots) or RecordDBPlotsFull} WriteLineToDebugFile('  Plot db=' + IntToStr(i) + '  ' + GISDB[i].dbName + '   recs=' + IntToStr(GISDB[i].MyData.FiltRecsInDB)); {$EndIf}
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
      end;
*)
      if ValidDB(OpenDBsIndex) then begin
         {$If Defined(RecordDBPlots) or Defined(RecordDBsIndex)}
            WriteLineToDebugFile('OpenDBsIndex layers=' + IntToStr(GISdb[OpenDBsIndex].MyData.FiltRecsInDB) + ' filter=' + GISdb[OpenDBsIndex].MyData.Filter);
         {$EndIf}
         GISdb[OpenDBsIndex].MyData.First;
         while not GISdb[OpenDBsIndex].MyData.eof do begin
            db := GISdb[OpenDBsIndex].MyData.GetFieldByNameAsInteger('DB_NUM');
            if ValidDB(db) and (GISdb[db].theMapOwner <> nil) and (GISdb[db].LayerIsOn) and (GISdb[db].MyData.FiltRecsInDB > 0) then begin
                if (DBOverlayfName[db] <> '') then begin
                   Overlay := LoadBitmapFromFile(DBOverlayfName[db]);
                   {$If Defined(RecordDBPlots) or Defined(RecordDBsIndex)} WriteLineToDebugFile('Loaded=' + DBOverlayfName[db]); {$EndIf}
                end
                else begin
                   CreateBitmap(Overlay,Bitmap.Width,Bitmap.Height);
                   PlotDataBase(db,Overlay);
                   DBOverlayfName[db] := NextFileNumber(MDTempDir, 'db_overlay_', OverlayFExt);
                   PetImage.SaveBitmap(Overlay,DBOverlayfName[db]);
                   {$If Defined(RecordDBPlots) or Defined(RecordDBsIndex)} WriteLineToDebugFile('Created=' + DBOverlayfName[db]); {$EndIf}
                end;
                DrawOverlayNoDelete(Bitmap,Overlay,GISdb[db].dbOpts.Opacity);
            end
            else begin
               //GISdb[OpenDBsIndex].MyData.Delete;
            end;
            GISdb[OpenDBsIndex].MyData.Next;
         end;
      end;
   end;
   {$If Defined(RecordDBPlots) or Defined(RecordDBPlotsFull) or Defined(RecordDBsIndex)}  WriteLineToDebugFile('TMapDraw.PlotDataBases out'); {$EndIf}
end;



procedure TMapDraw.PlotDataBaseLegends(var Bitmap : tMyBitmap);
var
   i,j,db : integer;
begin
   {$IfDef UseDBPlotOrder}
       if (NumOpenDB > 0) then begin
          for j := MaxDataBase downto 1 do begin
             i := DBPlotOrder[j];
             if ValidDB(i) then begin
                {$IfDef RecordMapDrawLegends} WriteLineToDebugFile('TMapDraw.PlotDataBaseLegends ' + GISdb[i].DBName); {$EndIf}
                if (GISdb[i].MyData.FiltRecsInDB > 0) then begin
                   if (GISdb[i].LayerIsOn) {$IfDef UseDBPlotNow} and DBPlotNow[i] {$EndIf} and ( {$IfDef UseDBonThisMap} (DBonThisMap[i]) or {$EndIf} (MDdef.DBsOnAllMaps {and (not GISdb[i].ShowLocalMapOnly)})) then begin
                      PlotDBLegend(i,Bitmap);
                   end;
                end;
             end;
          end;
       end;
   {$Else}
      if ValidDB(OpenDBsIndex) then begin
         GISdb[OpenDBsIndex].MyData.First;
         while not GISdb[OpenDBsIndex].MyData.eof do begin
            db := GISdb[OpenDBsIndex].MyData.GetFieldByNameAsInteger('DB_NUM');
            if ValidDB(db) and (GISdb[db].theMapOwner <> nil) and (GISdb[db].LayerIsOn) and (GISdb[db].MyData.FiltRecsInDB > 0) then begin
               PlotDBLegend(db,Bitmap);
            end;
            GISdb[OpenDBsIndex].MyData.Next;
         end;
      end;
   {$EndIf}
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
               else if ItsTigerShapeFile or (ItsOSMShapeFile and StrUtils.AnsiContainsText(dbName,'roads')) then PlotTigerShapeFileOnMap(DBNum,Bitmap)
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


procedure TMapDraw.LabelDataBases(var Bitmap : tMyBitmap);
var
   i,j,db : integer;
begin
   {$IfDef RecordDBPlots} WriteLineToDebugFile('TMapDraw.LabelDataBases in'); {$EndIf}
   {$IfDef UseDBPlotOrder}
       for j := MaxDataBase downto 1 do begin
          i := DBPlotOrder[j];
          if (GISdb[i] <> Nil) and (GISdb[i].theMapOwner <> Nil) and {$IfDef UseDBonThisMap} (DBonthisMap[i]) and {$EndIf} (GISdb[i].LayerIsOn) and (GISdb[i].dbOpts.LabelDBPlots) and (not GISdb[i].PLSSfile) then begin
             {$IfDef RecordDBPlots} WriteLineToDebugFile('Label db=' + IntToStr(i) + '  ' + GISDB[i].dbName + '   recs=' + IntToStr(GISDB[i].MyData.FiltRecsInDB)); {$EndIf}
             GISdb[i].LabelRecordsOnMap(Bitmap);
          end;
       end;
   {$Else}
     if ValidDB(OpenDBsIndex) then begin
         GISdb[OpenDBsIndex].MyData.First;
         while not GISdb[OpenDBsIndex].MyData.eof do begin
            db := GISdb[OpenDBsIndex].MyData.GetFieldByNameAsInteger('DB_NUM');
            if ValidDB(db) and (GISdb[db].theMapOwner <> nil) and (GISdb[db].LayerIsOn) and (GISdb[db].MyData.FiltRecsInDB > 0) then begin
               GISdb[db].LabelRecordsOnMap(Bitmap);
            end;
            GISdb[OpenDBsIndex].MyData.Next;
         end;
      end;
   {$EndIf}

   {$IfDef RecordDBPlots} WriteLineToDebugFile('TMapDraw.LabelDataBases out'); {$EndIf}
end;


procedure TMapDraw.QuickShapeFileDisplay(var Bitmap : tMyBitmap; fName : PathStr; ColorFieldPresent : boolean = false; color : tColor = -1);
var
   GISNum : integer;
begin
   if OpenNumberedGISDataBase(GISNum,fName,false,Nil) then begin
      {$IfDef RecordDBPlots} WriteLineToDebugFile('TMapDraw.QuickShapeFileDisplay  GIS=' + GISNum.ToString + '   ' + fName); {$EndIf}
      {$IfDef VCL}
         if (Color > -1) then begin
            Bitmap.Canvas.Pen.Color := Color;
            Bitmap.Canvas.Brush.Color := Color;
            Bitmap.Canvas.Brush.Style := bsClear;   //bsSolid;
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
      CloseSingleDB(GISNum);
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
       {$If Defined(RecordPlotDBRules) or Defined(RecordOSM)}
          if (theMapOwner <> nil) then WriteLineToDebugFile('tGISDataBase.PlotDBOnMapFromRules in, with rules=' + ExtractFileName(Rules) + '  map pixel size=' + IntToStr(round(TheMapOwner.MapDraw.ScreenPixelSize)));
       {$EndIf}
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
                          if not MyData.FieldExists('TYPE') then aFilter := StringReplace(aFilter,'TYPE','FCLASS',[rfReplaceAll, rfIgnoreCase]);
                      end;
                       if (aFilter = '') then begin
                          aShapeFile.PlotAllRecords(TheMapOwner.MapDraw,Bitmap);
                       end
                       else begin
                          {$IfDef RecordOSM} WriteLineToDebugFile(MyData.Filter);  {$EndIf}
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


procedure TMapDraw.PlotShapeFileGroup(LayerType : integer; IndexFName,LayerSavedName : PathStr; var inBitmap : tMyBitmap; MultFiles : tStringList = nil);


      procedure PlotSingleShapeFileGroup(IndexFName : PathStr; var Bitmap : tMyBitmap; BaseDir : PathStr = '');
      //because OSM might have variables directory labels, it is handled with the BaseDir
      var
         x,y,i,j : integer;
         Label_Field : string10;
         fName2,BaseName : PathStr;
         fList : tStringList;
         LabelIt,
         TryLabels,
         PlotIt : boolean;
         inTheData : tMyData;
         GISNum : integer;
      begin
          if not FileExists(IndexFName) then begin
             {$If Defined(RecordShapeFileGroup) or Defined(RecordOSM)} HighlightLineToDebugFile('TMapDraw.PlotShapeFileGroupOnMap No file requested'); {$EndIf}
             exit;
          end;
          if BaseDir <> '' then begin
             fList := nil;
             Petmar.FindMatchingFiles(IndexFName,'*.shp',fList,4);
          end;
          {$If Defined(RecordShapeFileGroup) or Defined(RecordOSM)} WriteLineToDebugFile('PPlotShapeFileGroup in ' + IndexFName + '  ' + SmartMemorySizeBytes(GetFileSize(IndexfName))); {$EndIf}
          inTheData := TMyData.Create(IndexFName);
          {$IfDef RecordFullShapeFileGroup} WriteLineToDebugFile('PlotShapeFileGroup loaded ' + IndexFName); {$EndIf}
          if (not ThreadsWorking) then ShowHourglassCursor;
          TryLabels := inTheData.FieldExists('LABEL');
          inTheData.ApplyFilter('PLOT =' + QuotedStr('Y'));
          if (inTheData.RecordCount = 0) then begin
             MessageToContinue('No used layers in ' + IndexFName);
             {$If Defined(RecordShapeFileGroup) or Defined(RecordOSM)} WriteLineToDebugFile('No used layers in ' + IndexFName); {$EndIf}
          end
          else for i := 0 to 9 do begin
              inTheData.ApplyFilter('PLOT_ORDER = ' + IntToStr(i) + ' AND PLOT =' + QuotedStr('Y'));
              {$If Defined(RecordFullShapeFileGroup) or Defined(RecordFullOSM)} if (inTheData.FiltRecsInDB > 0) then WriteLineToDebugFile(inTheData.Filter + '  layers=' + IntToStr(inTheData.FiltRecsInDB)); {$EndIf}
              if (inTheData.FiltRecsInDB > 0) then begin
                 inTheData.First;
                 while not inTheData.EOF do begin
                    BaseName := inTheData.GetFieldByNameAsString('FILENAME');
                    if BaseDir <> '' then begin
                       for j := 0 to pred(flist.count) do begin
                          if  StrUtils.AnsiContainsText(Flist.Strings[j],BaseName) then fName2 := Flist.Strings[j];
                       end;
                    end
                    else fName2 := BaseName;
                    if FileExists(fName2) then begin
                       {$If Defined(RecordFullShapeFileGroup) or Defined(RecordFullOSM)}  WriteLineToDebugFile('Plot: ' + fName2); {$EndIf}
                       LabelIt := TryLabels and (inTheData.GetFieldByNameAsString('LABEL') = 'Y');
                       if LabelIt then begin
                          inTheData.DefineFontFromTable(Bitmap.Canvas.Font);
                          Bitmap.Canvas.Brush.Style := bsClear;
                          Label_Field := inTheData.GetFieldByNameAsString('LAB_FIELD');
                       end;
                       {$If Defined(RecordFullShapeFileGroup) or Defined(RecordOSM)} WriteLineToDebugFile('Opening GIS module for ' + fName2); {$EndIf}

                       if OpenNumberedGISDataBase(GISNum,fName2) then begin
                           if inTheData.FieldExists('FILTER') and (inTheData.GetFieldByNameAsString('FILTER') <> '') then begin
                              GISDB[GISNum].MyData.ApplyFilter(inTheData.GetFieldByNameAsString('FILTER'));
                              {$If Defined(RecordFullShapeFileGroup) or Defined(RecordOSM)}  WriteLineToDebugFile('Filter: ' +  GISDB[GISNum].MyData.Filter + '  recs=' + IntToStr( GISDB[GISNum].MyData.FiltRecsInDB)); {$EndIf}
                           end;
                           if inTheData.FieldExists('PIXEL_SIZE') and (inTheData.GetFieldByNameAsString('PIXEL_SIZE') <> '') then begin
                              PlotIt := (ScreenPixelSize < inTheData.GetFieldByNameAsInteger('PIXEL_SIZE'));
                           end
                           else PlotIt := true;

                           if PlotIt and (GISDB[GISNum].MyData.FiltRecsInDB > 0) then begin
                             {$If Defined(RecordFullShapeFileGroup) or Defined(RecordOSM)} WriteLineToDebugFile('Plot file'); {$EndIf}
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
                                    GISDB[GISNum].aShapeFile.PlotAllRecords(Self,Bitmap);
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
                           CloseSingleDB(GISNum);
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
          if BaseDir <> '' then fList.Destroy;
          inTheData.Destroy;
          {$If Defined(RecordFullShapeFileGroup) or Defined(RecordOSM)} WriteLineToDebugFile('PlotShapeFileGroup out'); {$EndIf}
      end;


      procedure OSMthread(fName : PathStr; var Bitmap : tMyBitmap);
      //cannot count on the number of directory layers with OSM
      var
         adb,j : integer;
         fList : tstringlist;
      begin
         fList := nil;
         Petmar.FindMatchingFiles(fName,'*.shp',fList,4);
         for j := 0 to pred(flist.count) do begin
            if StrUtils.AnsiContainsText(Flist.Strings[j],'roads') then fName := Flist.Strings[j];
         end;
         fList.Destroy;
         if OpenNumberedGISDataBase(adb,fName) then begin
           {$If Defined(RecordOSM)} WriteLineToDebugFile('start roads'); {$EndIf}
           GISdb[adb].aShapeFile.BadPointCheck := MDDef.OSMCheck;
           PlotDBOnMapFromRules(adb,Bitmap,OSMRoadRules);
           CloseSingleDB(adb);
        end;
      end;

var
   Bitmap : tMyBitmap;
   Inscale : boolean;
   i : integer;
begin {procedure TMapDraw.PlotShapeFileGroup}
    if FileExists(LayerSavedName) then begin
       {$If Defined(RecordShapeFileGroup) or Defined(RecordOSM)} WriteLineToDebugFile('Reading ' + LayerSavedName); {$EndIf}
       Bitmap := PetImage.LoadBitmapFromFile(LayerSavedName);
    end
    else begin
        CreateBitmap(Bitmap,inBitmap.Width,inBitmap.Height);
        if (LayerType = OSMlayer) then begin
           {$If Defined(RecordShapeFileGroup) or Defined(RecordOSM)} HighlightLineToDebugFile('PlotShapeFileGroup in ' + IndexFName); {$EndIf}
           for i := 0 to pred(OSMShapesUp.Count) do begin
               IndexFName := OSMShapesUp.Strings[i];
               {$If Defined(RecordOSM)} WriteLineToDebugFile('OSMthread all but roads, ' + IndexFName); {$EndIf}
               PlotSingleShapeFileGroup(OSMgroupRules,Bitmap,IndexFName);
               {$If Defined(RecordOSM)} WriteLineToDebugFile('OSMthread roads, ' + IndexFName); {$EndIf}
               OSMthread(IndexFName,Bitmap);   //roads
               {$If Defined(RecordOSM)} WriteLineToDebugFile('OSMthread bridges, ' + IndexFName); {$EndIf}
               HighlightOSMBridges := true;
               OSMthread(IndexFName,Bitmap);   //bridges
               {$If Defined(RecordOSM)} WriteLineToDebugFile('OSM done'); {$EndIf}
           end;
           if GrayscaleOSM then MakeTheBitmapGrayScale(Bitmap);
           if SubdueOSM then MakeTheBitmapSubdued(Bitmap);
        end
        else if (LayerType = NaturalEarthLayer) then begin
           //PlotShapeFileGroup(IndexFName,Bitmap);
           InScale := true;
           if (not MDDef.NEautoScale) or (ScreenPixelSize > MDDef.SmallScaleWorldOutlinePixelSize) then begin
              {$IfDef RecordWorldOutline} WriteLineToDebugFile('small scale ' + SmallScaleWorldOutlines); {$EndIf}
              IndexFName := SmallScaleWorldOutlines;
           end
           else if (ScreenPixelSize > MDDef.MedScaleWorldOutlinePixelSize) then begin
              {$IfDef RecordWorldOutline} WriteLineToDebugFile('medium scale ' + MedScaleWorldOutlines); {$EndIf}
              IndexFName := MedScaleWorldOutlines;
           end
           else if (ScreenPixelSize > MDDef.LargeScaleWorldOutlinePixelSize) then begin
              {$IfDef RecordWorldOutline} WriteLineToDebugFile('large scale ' + LargeScaleWorldOutlines); {$EndIf}
              IndexFName := LargeScaleWorldOutlines;
           end
           else begin
              {$IfDef RecordWorldOutline} WriteLineToDebugFile('no scale and not drawn'); {$EndIf}
              InScale := false;
           end;
           if Inscale then begin
              PlotSingleShapeFileGroup(IndexFName,Bitmap);
              if GrayscaleWorldOutline then MakeTheBitmapGrayScale(Bitmap);
              if SubdueWorldOutline then MakeTheBitmapSubdued(Bitmap);
           end;
        end
        else if LayerType = CartoLayer then begin
           PlotSingleShapeFileGroup(IndexFName,Bitmap);
        end;
       DrawAndDeleteOverlay(inBitmap,Bitmap);
    end;
    SaveBitmap(Bitmap,LayerSavedName);
    {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('PlotShapeFileGroup out');{$EndIf}
end {procedure TMapDraw.PlotShapeFileGroup};



 procedure TMapDraw.SmartWorldOutline(Bitmap : tMyBitmap);
 var
    //NEOverlay : tMyBitmap;
    //fName : PathStr;
    InScale : boolean;
 begin
     if NaturalEarthOverlayFName = '' then NaturalEarthOverlayFName := NextFileNumber(MDTempDir, 'NE_overlay', OverlayFExt);
     PlotShapeFileGroup(NaturalEarthLayer,'',NaturalEarthOverlayFName,bitmap);

(*
    {$IfDef RecordWorldOutline} WriteLineToDebugFile('TMapDraw.SmartWorldOutline in, pixel=' + RealToString(ScreenPixelSize,-12,-2) + ' m, smart scaling='+ TrueOrFalse(MDDef.NEautoScale)); {$EndIf}
    CreateBitmap(NEOverlay,Bitmap.Width,Bitmap.Height);
    if (NaturalEarthOverlayFName <> '') then begin
      NEOverlay.LoadFromFile(NaturalEarthOverlayFName);
    end
    else begin
       InScale := true;
       NaturalEarthOverlayFName := NextFileNumber(MDTempDir, 'NE_overlay', OverlayFExt);
       if (not MDDef.NEautoScale) or (ScreenPixelSize > MDDef.SmallScaleWorldOutlinePixelSize) {or AFullWorldMap} then begin
          {$IfDef RecordWorldOutline} WriteLineToDebugFile('small scale ' + SmallScaleWorldOutlines); {$EndIf}
          fName := SmallScaleWorldOutlines;
       end
       else if (ScreenPixelSize > MDDef.MedScaleWorldOutlinePixelSize) then begin
          {$IfDef RecordWorldOutline} WriteLineToDebugFile('medium scale ' + MedScaleWorldOutlines); {$EndIf}
          fName := MedScaleWorldOutlines;
       end
       else if (ScreenPixelSize > MDDef.LargeScaleWorldOutlinePixelSize) then begin
          {$IfDef RecordWorldOutline} WriteLineToDebugFile('large scale ' + LargeScaleWorldOutlines); {$EndIf}
          fName := LargeScaleWorldOutlines;
       end
       else begin
          {$IfDef RecordWorldOutline} WriteLineToDebugFile('no scale and not drawn'); {$EndIf}
          InScale := false;
       end;
       if Inscale then begin
          PlotShapeFileGroup(fName,NEOverlay);
          if GrayscaleWorldOutline then MakeTheBitmapGrayScale(NEOverlay);
          if SubdueWorldOutline then MakeTheBitmapSubdued(NEOverlay);
       end;

       NEOverlay.SaveToFile(NaturalEarthOverlayFName);
    end;
    DrawAndDeleteOverlay(Bitmap,NEOverlay,100); //,0,0,true);
*)
end;




procedure TMapDraw.QuietPlotTIGERShapeFileOnMap(fName : PathStr; var Bitmap : tMyBitmap; RoadsOnly : boolean = false; MaskSize : integer = -1; StreamsOnly : boolean = false);
var
   GISNum : integer;
begin
   {$IfDef RecordTiger} WriteLineToDebugFile('PlotTIGERShapeFileOnMap ' + fName); {$EndIf}
   if OpenNumberedGISDataBase(GISNum,fName) then begin
      PlotTigerShapeFileOnMap(GISNum,Bitmap,RoadsOnly,MaskSize,StreamsOnly);
      CloseSingleDB(GISNum);
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
                  if MDDef.MakeTigerMapGrayscale then MakeTheBitmapGrayScale(Bitmap);
                  if MDDef.SubdueTigerBase then MakeTheBitmapSubdued(Bitmap);
               end;
               {$IfDef RecordTiger} WriteLineToDebugFile('out'); {$EndIf}
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


