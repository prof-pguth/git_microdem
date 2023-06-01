{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{   include file for demdatabase     }
{____________________________________}


{$IfDef ExGeostats}
{$Else}

      procedure tGISdataBaseModule.PlotFabric(var Bitmap : tMyBitmap);
      var
         PlotColor : tPlatformColor;
         Lat,Long : float64;
         x,y,Len,i,j,rc : Integer;
      begin
         {$IfDef RecordDBPlot} WriteLineToDebugFile('PlotFabric in, ' + dbName); {$EndIf}
         PlotColor := MDDef.GrainColor;
         MyData.First;
         if MDDef.FabColorByField then begin
            MDDef.FabColorByField := MyData.FieldExists(dbOpts.FloatColorField);
         end;
         ApplyGISFilter('S1S2<=' + RealToString(MDdef.FlatnessCutoff,-12,-3) + ' AND S2S3 >=' + RealToString(MDDef.OrganizationCutoff,-12,-3));
         StartProgress('Fabric');
         j := 0;
         rc := ProgressIncrement(MyData.FiltRecsInDB);
         while not MyData.EOF do begin
           if j mod rc = 0 then begin
              UpdateProgressBar(j/MyData.FiltRecsInDB);
              EmpSource.Enabled := false;
           end;
           inc(j);

           if ValidLatLongFromTable(Lat,Long) then begin
              TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
              Len := round(MyData.GetFieldByNameAsFloat('S2S3')*MDDef.GrainLengthMultiple);
              if MDDef.FabColorByField then PlotColor := Petmar.PlatformRainbowColorFunct(MyData.GetFieldByNameAsFloat(dbOpts.FloatColorField),MDDef.FabColorMin,MDDef.FabColorMax);
              PlotOrientedLine(Bitmap,x,y,Len,MyData.GetFieldByNameAsFloat('FABRIC_DIR'),PlotColor,MDDef.GrainLineWidth);
           end;
           for i := 1 to MDDef.FabricCalcThin do MyData.Next;
           if WantOut then break;
         end;
         {$IfDef RecordDBPlot} WriteLineToDebugFile('PlotFabric out, ' + dbName); {$EndIf}
      end;

{$EndIf}


            procedure TGISdataBaseModule.RedrawLayerOnMap;
            var
               NoRedraw : boolean;
            begin
               if (TheMapOwner <> Nil) and dbPlotted then begin
                  NoRedraw := (theMapOwner <> Nil) and theMapOwner.MapDraw.ClosingMapNow;
                  TheMapOwner.MapDraw.ClearGISLayer(DBNumber);
                  if (not NoRedraw) then begin
                     theMapOwner.DoFastMapRedraw;
                  end;
                  if (TheMapOwner.MapDraw.CurrentFansTable = DBNumber) then TheMapOwner.MapDraw.CurrentFansTable := 0;
                  if (TheMapOwner.MapTOCIndex <> 0) then MapTOC[TheMapOwner.MapTOCIndex].LabelTheButtons;
                  ShowStatus;
               end;
            end;


            procedure TGISdataBaseModule.ConnectTwoPointsInSameRecord(var Bitmap : tMyBitmap);
            var
               Lat,Long,Lat2,Long2 : float64;
               Value : float32;
               rc,XP2,yp2,XPic,ypic,n : integer;
            begin
               Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(dbOpts.LineColor);
               Bitmap.Canvas.Pen.Width := dbOpts.LineWidth;
               MyData.First;
               StartProgressAbortOption('Point separation');
               n := 0;
               rc:= ProgressIncrement(MyData.FiltRecsInDB);
               while not MyData.EOF do begin
                   if (n mod rc = 0) then begin
                      UpdateProgressBar(n/MyData.FiltRecsInDB);
                      EmpSource.Enabled := false;
                   end;
                   inc(n);
                   if ValidLatLongFromTable(Lat,Long) and ValidLat2Long2FromTable(Lat2,Long2) then begin
                      if (MDDef.ConnectRecordColoring) then begin
                         if GetFloat32FromTableLinkPossible(dbOpts.SymSizeField,Value) then begin
                            if (Value > MaxComboBox1) then Value := MaxComboBox1;
                            Bitmap.Canvas.Pen.Width := round(dbOpts.ScaledSymMinSize + (dbOpts.ScaledSymMaxSize - dbOpts.ScaledSymMinSize) * ((Value-MinComboBox1)/(MaxComboBox1-MinComboBox1)));
                         end;
                      end;
                      TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat2,Long2,xp2,yp2);
                      TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,xpic,ypic);
                      DrawLine(Bitmap,xp2,yp2,xpic,ypic);
                   end;
                   MyData.Next;
                   if WantOut then break;
               end;
            end;

            procedure TGISdataBaseModule.ConnectSequentialPoints(Bitmap : tMyBitmap);
            var
               Lat,Long,Strike : float64;
               x,y,i,n,midx,midy,LastX,LastY : integer;
               FirstPoint : boolean;
               Sep,LastSep : shortstring;
            begin
               Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(dbOpts.LineColor);
               Bitmap.Canvas.Pen.Width := dbOpts.LineWidth;

               n := 0;
               i := 0;
               Sep := '';
               LastSep := '';
               StartProgressAbortOption('Connect points');
               FirstPoint := true;
               MyData.First;
               while not MyData.eof do begin
                   if (n mod 500 = 0) then begin
                      UpdateProgressBar(n/MyData.FiltRecsInDB);
                      EmpSource.Enabled := false;
                   end;
                   inc(n);
                  if ValidLatLongFromTable(Lat,Long) then begin
                     if MyData.FieldExists('COLOR') then Bitmap.Canvas.Pen.Color := MyData.GetFieldByNameAsInteger('COLOR');
                     if dbOpts.SegSepField <> '' then Sep := MyData.GetFieldByNameAsString(dbOpts.SegSepField);
                     if (Sep <> LastSep) then FirstPoint := true;
                     TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                     inc(i);
                     if FirstPoint then begin
                        Bitmap.Canvas.MoveTo(x,y);
                        FirstPoint := false;
                     end
                     else begin
                        if MDDef.ConnectArrows and (i >= MDDef.ConnectArrowSpacing) then begin
                           i := 0;
                           Strike := HeadingOfLine(x-LastX,Lasty-y);
                           midx := (x + lastX) div 2;
                           midy := (y + lasty) div 2;
                           PlotVector(Bitmap,LastX,LastY,midx,midy,dbOpts.LineColor,dbOpts.LineWidth,true);
                        end;
                        Bitmap.Canvas.LineTo(x,y);
                     end;
                     LastX := x;
                     LastY := y;
                     LastSep := Sep;
                  end;
                  MyData.Next;
                  if WantOut then break;
               end;
            end;


            procedure TGISdataBaseModule.PlotDBNumericField(var Bitmap : tMyBitmap);
            var
               x : float64;
               MinLog,MaxLog : float64;
               Plotted,Skip,UpdateLevel,i : integer;
               ShortName : ShortString;
               UseLink,GotIt : boolean;
            begin
               {$If Defined(RecordDBPlot) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('PlotDBNumericField in, ' + dbName +  ' field=' + dbOpts.FloatColorField); {$EndIf}
               if (dbOpts.dbColorMode = dbcmFieldLog) then begin
                  if (dbOpts.ColorMin <= 0.000000001) then dbOpts.dbColorMode := dbcmFieldLinear
                  else begin
                     MinLog := Ln(dbOpts.ColorMin);
                     MaxLog := Ln(dbOpts.ColorMax);
                  end;
               end;
               DefineColorTable;
               ShortName := dbOpts.FloatColorField;
               UseLink := LinkedField(ShortName) and LinkTable.IsNumericField(ShortName);
               if UseLink or MyData.IsNumericField(dbOpts.FloatColorField) or (dbOpts.DBAutoShow in [dbasMultiFieldRGB,dbasColorPosNeg]) then begin
                 {$If Defined(RecordDBPlot) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('PlotDBNumericField begin plot'); {$EndIf}
                 PrepColors(Bitmap);
                 {$If Defined(RecordDBPlot) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('PlotDBNumericField colors prepped'); {$EndIf}
                 Plotted := 0;

                 UpdateLevel := ProgressIncrement(MyData.FiltRecsInDB);
                 StartProgressAbortOption('Plot: ' + dbOpts.FloatColorField);
                 MyData.First;
                 {$If Defined(RecordDBPlotDetailed) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('PlotDBNumericField start loop'); {$EndIf}
                 while not MyData.eof do begin
                    if ((Plotted mod UpdateLevel) = 0) then begin
                       UpDateProgressBar(Plotted/MyData.FiltRecsInDB);
                       EmpSource.Enabled := false;
                       {$IfDef RecordDBPlotDetailed} WriteLineToDebugFile('PlotDBNumericField plotted=' + IntToStr(Plotted)); {$EndIf}
                    end;
                    inc(Plotted,ThinToPlot);
                    if (dbOpts.DBAutoShow in [dbasMultiFieldRGB]) then begin
                       Bitmap.Canvas.Brush.Color := RGBColorFromThreeNumericFields;
                       Bitmap.Canvas.Pen.Color := Bitmap.Canvas.Brush.Color;
                    end
                    else begin
                       if UseLink then begin
                          GotIt := GetFloatFromTableLink(ShortName,x);
                       end
                       else begin
                          GotIt := MyData.CarefullyGetFieldByNameAsFloat64(dbOpts.FloatColorField,x);
                       end;
                       if GotIt then begin
                           if (dbOpts.DBAutoShow in [dbasColorPosNeg]) then begin
                             if (Abs(x) < ZeroTol) then Bitmap.Canvas.Brush.Color := clYellow
                             else if (x > 0) then Bitmap.Canvas.Brush.Color := clGreen
                             else Bitmap.Canvas.Brush.Color := clRed;
                             Bitmap.Canvas.Pen.Color := Bitmap.Canvas.Brush.Color;
                          end
                          else begin
                             if (dbOpts.dbColorMode = dbcmFieldLinear) then Bitmap.Canvas.Brush.Color := SelectedColorSchemeColorFunct(dbOpts.DBColorScheme,ColorDefTable,x,dbOpts.ColorMin,dbOpts.ColorMax)
                             else if (dbOpts.dbColorMode = dbcmFieldQuantile) then Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(ColorFromZColorTable(ColorDefTable,x,i))
                             else if (dbOpts.dbColorMode = dbcmFieldLog) then Bitmap.Canvas.Brush.Color := SelectedColorSchemeColorFunct(dbOpts.DBColorScheme,ColorDefTable,ln(x),MinLog,MaxLog);
                             if MDDef.ClipZColors then begin
                                if (x < dbOpts.ColorMin) then Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDdef.LowOffscaleColor)
                                else if (x > dbOpts.ColorMax) then Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDdef.HighOffscaleColor);
                             end;
                             if XYZFile or (SimplePointFile and (LatFieldName <> '') and (LongFieldName <> '')) then begin
                                dbOpts.Symbol.Color := ConvertTColorToPlatformColor(Bitmap.Canvas.Brush.Color);
                             end
                             else if AreaShapeFile(ShapeFileType) then begin
                                if (not dbOpts.OutlinePolygons) then Bitmap.Canvas.Pen.Color := Bitmap.Canvas.Brush.Color;
                             end
                             else Bitmap.Canvas.Pen.Color := Bitmap.Canvas.Brush.Color;
                          end;
                          {$IfDef ExRedistrict}
                          {$Else}
                             if Redistricting then SetRedistrictPattern(Bitmap);
                          {$EndIf}
                          DisplayCurrentRecordOnMap(TheMapOwner.MapDraw,Bitmap);
                       end;
                    end;
                    for Skip := 1 to ThinToPlot do MyData.Next;
                    if WantOut then Break;
                 end;
                 {$If Defined(RecordDBPlot) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('PlotDBNumericField out, recs plotted = ' + IntToStr(Plotted)); {$EndIf}
               end;
            end;


            procedure TGISdataBaseModule.PlotDBStringField;
            var
               OldFilter : ANSIString;
               i : integer;
               TStr : shortString;
               ShortName : ShortString;
               UseLink : boolean;
            begin
               {$IfDef RecordDBPlot} WriteLineToDebugFile('TGISdataBaseModule.PlotDBStringField in, dbOpts.ColorField=' + dbOpts.FloatColorField); {$EndIf}
               if (WantField <> '') then dbOpts.FloatColorField := WantField;

               if (dbOpts.FloatColorField = '') then exit;
               ShortName := dbOpts.FloatColorField;
               UseLink := LinkedField(ShortName) and (LinkTable.IsStringField(ShortName) or LinkTable.IsIntegerField(ShortName));

               if UseLink or MyData.IsStringField(dbOpts.FloatColorField) or MyData.IsIntegerField(dbOpts.FloatColorField) then begin
                  EmpSource.Enabled := false;
                  if (StringCategories = Nil) then begin
                     PetDBUtils.FindUniqueEntriesLinkPossible(MyData,LinkTable,dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,dbOpts.FloatColorField,StringCategories);
                  end;
                  {$IfDef RecordDBPlot} WriteLineToDebugFile('strings in field=' + IntToStr(StringCategories.Count)); {$EndIf}

                  DefineColorTableValues(dbOpts.DBColorPaletteName,0,pred(StringCategories.Count),ColorDefTable);
                  {$IfDef RecordColorPalette} ShowColorPalette('after string define'); {$EndIf}

                   OldFilter := MyData.Filter;
                   StartProgressAbortOption('Plot ' + dbName);
                   Bitmap.Canvas.Pen.Width := dbOpts.LineWidth;
                   for I := 0 to pred(StringCategories.Count) do begin
                      if (StringCategories.Count > 1) then UpdateProgressBar(I/pred(StringCategories.Count));
                      EmpSource.Enabled := false;
                      SetBitmapColorsForStringFieldPlot(Bitmap,I,pred(StringCategories.Count));
                      TStr := StringCategories.Strings[i];

                      if (MyData.GetFieldType(dbOpts.FloatColorField) = ftString) then TStr := QuotedStr(TStr);
                      MyData.ApplyFilter( PetDBUtils.AddAndIfNeeded(OldFilter) + dbOpts.FloatColorField + '=' + TStr);

                      {$IfDef RecordDBPlot} WriteLineToDebugFile('i=' + i.ToString + '  '  + TStr + ' color=' + IntToStr(Bitmap.Canvas.Pen.Color) + ' filter=' + MyData.Filter + ' recs=' + IntToStr(MyData.FiltRecsInDB)); {$EndIf}

                      EmpSource.Enabled := false;
                      while not MyData.EOF do begin
                         EmpSource.Enabled := false;
                         DisplayCurrentRecordOnMap(TheMapOwner.MapDraw,Bitmap);
                         MyData.Next;
                         if WantOut then break;
                      end;
                      if WantOut then break;
                   end;
                   MyData.ApplyFilter(OldFilter);
               end;
            end;

            procedure TGISdataBaseModule.PlotIcons(Var Bitmap : tMyBitmap);
            var
               Lat,Long : float64;
               x,y,i,rc : integer;
               OK : boolean;
               fName,base : pathstr;
            begin
               MyData.First;
               rc:= ProgressIncrement(MyData.FiltRecsInDB);
               i := 0;
               repeat
                  if (i mod rc = 0) then begin
                     UpdateProgressBar(I/MyData.FiltRecsInDB);
                     EmpSource.Enabled := false;
                  end;
                  OK := true;
                   if LineOrAreaShapeFile(ShapeFileType) then aShapeFile.AreaAndCentroid(TheMapOwner.MapDraw.PrimMapProj,MyData.RecNo,Lat,Long)
                   else begin
                      OK := ValidLatLongFromTable(Lat,Long);
                   end;
                  if OK then begin
                     TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                     if TheMapOwner.MapDraw.OnScreen(x,y) then begin
                        if (dbOpts.DBAutoShow = dbasIconAll) then begin
                           TheMapOwner.MapDraw.PlotAnIcon(Bitmap,Lat,Long,dbOpts.AllIconFName,dbOpts.IconScalingFactor);
                        end
                        else if (dbOpts.DBAutoShow = dbasIconField) then begin
                           if (dbOpts.IconField <> '') then begin
                              fName := MyData.GetFieldByNameAsString(dbOpts.IconField);
                              if FileExists(fname) then begin
                              end
                              else begin
                                 Base := ExtractFilePath(dbFullName);
                                 if FileExists(Base + fName) then fName := Base + fName;
                              end;
                              TheMapOwner.MapDraw.PlotAnIcon(Bitmap,Lat,Long,fName,dbOpts.IconScalingFactor);
                           end
                        end
                        else if (dbOpts.DBAutoShow = dbasPointsInDB) then begin
                           Petmar.ScreenSymbol(Bitmap.Canvas,x,y,tDrawingSymbol(MyData.GetFieldByNameAsInteger('SYM_TYPE')),
                                 MyData.GetFieldByNameAsInteger('SYM_SIZE'),ConvertTColorToPlatformColor(MyData.GetFieldByNameAsInteger('SYM_COLOR')));
                        end;
                     end;
                  end;
                  MyData.Next;
                  if WantOut then break;
               until MyData.eof;
            end;


            procedure TGISdataBaseModule.PlotVectorsOnMap(var Bitmap : tMyBitmap);
            var
               Lat,Long,Lat2,Long2,Dir,Mag,Value,xf,yf,dLong,LongCent,ve,vn,cv : float64;
               Recs, err,i,j,x1,y1,x2,y2,Dist,rc : integer;
               Color : tPlatformColor;
               PlotIt : boolean;
            begin
               {$If Defined(RecordSymProblems) or Defined(RecordGISvectors)} WriteLineToDebugFile('tGISdataBaseModule.PlotVectorsOnMap ' + dbName + '  filter=' + MyData.Filter); {$EndIf}

               if MyData.FieldExists(dbOpts.DirField) then begin
                  {$If Defined(RecordSymProblems) or Defined(RecordGISvectors)} WriteLineToDebugFile('Mag=' + dbOpts.MagField + '  Dir=' + dbOpts.DirField); {$EndIf}
                  if dbOpts.GISVectorsByMaxSpeed then begin
                     dbOpts.ColorMax := dbopts.GISVectorsMaxSpeed;
                     dbOpts.ColorMin := 0;
                     {$If Defined(RecordSymProblems) or Defined(RecordGISvectors)} WriteLineToDebugFile('Color by speed range, ' + RealToString(dbOpts.ColorMax,-12,-4)); {$EndIf}
                  end;

                  if dbOpts.WindAutoSpace then begin
                     dbOpts.VectorThinFactor := 0;
                     MyData.First;
                     ValidLatLongFromTable(Lat,Long);
                     MyData.Next;
                     ValidLatLongFromTable(Lat2,Long2);
                     dLong := abs(Long2 - Long);
                     TheMapOwner.MapDraw.ScreenToLatLongDegree(TheMapOwner.MapDraw.MapXSize div 2,TheMapOwner.MapDraw.MapYSize div 2,Lat,LongCent);
                     repeat
                         inc(dbOpts.VectorThinFactor);
                         Long := LongCent + dbOpts.VectorThinFactor * dLong;
                         Long2 := LongCent - dbOpts.VectorThinFactor * dLong;
                         TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x1,y1);
                         TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long2,x2,y2);
                         Dist := round(Sqrt(sqr(x1-x2)+ sqr(y2-y1)));
                     until (Dist >= 2 * dbOpts.WindPixelSpace) or (dbOpts.VectorThinFactor > 25);
                  end;

                  {$IfDef RecordDataInsideLoopPlots} if NeedColorValue then WriteLineToDebugFile('NeedColorValue = true   GISColorSource = ' + IntToStr(MDDef.GISColorSource)); {$EndIf}
                  EmpSource.Enabled := false;
                  Recs := 0;
                  MyData.First;
                  rc := ProgressIncrement(MyData.FiltRecsInDB);
                  StartProgressAbortOption('Vectors ' + dbName);
                  Color := dbOpts.LineColor;
                  while not MyData.EOF do begin
                     inc(Recs);
                     if (recs mod rc = 0) then UpdateProgressBar(Recs*dbOpts.VectorThinFactor/MyData.FiltRecsInDB);
                     PlotIt := true;
                     if SimplePointFile then begin
                        PlotIt := ValidLatLongFromTable(Lat,Long);
                     end
                     else if LatLongCornersPresent then begin
                        Lat := 0.5*(MyData.GetFieldByNameAsFloat('LAT_HI')+MyData.GetFieldByNameAsFloat('LAT_LOW'));
                        Long := 0.5*(MyData.GetFieldByNameAsFloat('LONG_HI')+MyData.GetFieldByNameAsFloat('LONG_LOW'));
                     end;
                     TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,dbxpic,dbypic);
                     if TheMapOwner.MapDraw.OnScreen(dbxpic,dbypic) then begin
                        if VelNECompPresent then begin
                           ve := MyData.GetFieldByNameAsFloat(dbOpts.MagField);
                           vn := MyData.GetFieldByNameAsFloat(dbOpts.DirField);
                           Dir := HeadingOfLine(ve,vn);
                           Mag := sqrt(sqr(ve) + sqr(vn));
                        end
                        else if dbOpts.VectorsByPolar then begin
                           PlotIt := MyData.CarefullyGetFieldByNameAsFloat64(dbOpts.DirField,Dir);
                           if PlotIt then begin
                              if dbOpts.MagField = '' then Mag := 1
                              else PlotIt := MyData.CarefullyGetFieldByNameAsFloat64(dbOpts.MagField, Mag);
                           end;
                        end
                        else begin
                           PlotIt := MyData.CarefullyGetFieldByNameAsFloat64(dbOpts.DirField,xf) and MyData.CarefullyGetFieldByNameAsFloat64(dbOpts.MagField,yf);
                           if PlotIt then begin
                              Dir := HeadingOfLine(yf,xf);
                              Mag := sqrt(sqr(xf) + sqr(yf));
                           end;
                        end;

                        if PlotIt then begin
                           if dbOpts.GISVectorsByMaxSpeed then begin
                              Color := ConvertTColorToPlatformColor(SelectedColorSchemeColorFunct(dbOpts.DBColorScheme,ColorDefTable,Mag,dbOpts.ColorMin,dbOpts.ColorMax));
                            end
                            else begin
                               Color := dbOpts.LineColor;
                            end;
                            if PlotIt and (abs(Mag) > 0.01) then begin
                               PETImage.PlotOrientedLine(Bitmap,dbxpic,dbypic,round(dbOpts.VectorLineMult*Mag),round(Dir),Color,dbOpts.LineWidth,MDDef.PlotArrowHead,MDDef.ReverseArrow);
                            end;
                        end;
                     end;
                     for i := 1 to dbOpts.VectorThinFactor do MyData.Next;
                     if WantOut then break;
                  end;
                  EmpSource.Enabled := true;
               end;

               {$If Defined(RecordSymProblems) or Defined(RecordGISvectors)} WriteLineToDebugFile('tGISdataBaseModule.PlotVectorsOnMap out'); {$EndIf}
            end;

            procedure TGISdataBaseModule.PlotDefaultSymbols(MapDraw : tMapDraw; var Bitmap : tMyBitmap);
            var
               Recs,x,y,rc : integer;
            begin
               {$IfDef RecordSym} WriteLineToDebugFile('tGISdataBaseModule.PlotDefaultSymbols ' + dbName); {$EndIf}
               {$IfDef RecordSymbolColor} WriteLineToDebugFile('TGISdataBaseModule.PlotDefaultSymbols out, color = ' + RGBString(dbOpts.Symbol.Color.rgbtRed,dbOpts.Symbol.Color.rgbtGreen,dbOpts.Symbol.Color.rgbtBlue)); {$EndIf}

                Recs := 0;
                MyData.First;
                EmpSource.Enabled := false;
                PrepColors(Bitmap);
                StartProgressAbortOption('Default symbols ' + dbName);
                rc := ProgressIncrement(MyData.FiltRecsInDB);
                while not MyData.EOF do begin
                   inc(Recs);
                   if (Recs mod rc = 0) then begin
                      UpdateProgressBar(Recs/MyData.FiltRecsInDB);
                      EmpSource.Enabled := false;
                   end;
                   DisplayCurrentRecordOnMap(MapDraw,Bitmap);
                   MyData.Next;
                   if WantOut then break;
                end;
               {$IfDef RecordSym} WriteLineToDebugFile('tGISdataBaseModule.PlotDefaultSymbols out'); {$EndIf}
            end;

            procedure TGISdataBaseModule.PlotZipATone(var Bitmap : tMyBitmap);
            var
               Recs,x,y,rc : integer;
               ZipPatName,LastZipAtone : PathStr;
               BMP : tMyBitmap;

                  procedure FreeBrushIfNeeded;
                  begin
                     if Bitmap.Canvas.Brush.Bitmap <> Nil then begin
                        Bitmap.Canvas.Brush.Bitmap.Free;
                        Bitmap.Canvas.Brush.Bitmap := Nil;
                     end;
                  end;

                  procedure LoadZipATone(fName : PathStr);
                  begin
                     FreeBrushIfNeeded;
                     Bitmap.Canvas.Brush.Bitmap := tMyBitmap.Create;  //have to go this way
                     Bitmap.Canvas.Brush.Bitmap.PixelFormat := pfDevice;
                     if (not  FileExists(fName)) then begin
                        fName := MainMapData + 'zipatone\' + fName;
                     end;
                     Bmp := PetImage.LoadBitmapFromFile(fName);
                     Bitmap.Canvas.Brush.Bitmap.Assign(Bmp);
                     BMP.Free;
                  end;


            begin
               {$IfDef RecordSym} WriteLineToDebugFile('tGISdataBaseModule.PlotDefaultSymbols ' + dbName); {$EndIf}
                Recs := 0;
                MyData.First;
                EmpSource.Enabled := false;
                PrepColors(Bitmap);
                Bitmap.Canvas.Brush.Bitmap := Nil;
                if (dbOpts.dbAutoShow in [dbasZipAtoneAll]) then begin
                    if dbOpts.ZipaToneFName = '' then begin
                       if not GetGraphicsFileName('zipatone for db',dbOpts.ZipaToneFName) then exit;
                    end;
                    LoadZipATone(dbOpts.ZipaToneFName);
                end;
                rc:= ProgressIncrement(MyData.FiltRecsInDB);
                StartProgressAbortOption('Default symbols ' + dbName);
                while not MyData.EOF do begin
                   inc(Recs);
                   if (Recs mod rc = 0) then begin
                      UpdateProgressBar(Recs/MyData.FiltRecsInDB);
                      EmpSource.Enabled := false;
                   end;
                   if (dbOpts.dbAutoShow in [dbasZipAtoneField]) then begin
                      ZipPatName := MyData.GetFieldByNameAsString('ZIPATONE');
                      if (ZipPatName <> '') then begin
                         if (ZipPatName <> LastZipAtone) then LoadZipATone(ZipPatName);
                         DisplayCurrentRecordOnMap(TheMapOwner.MapDraw,Bitmap);
                         LastZipAtone := ZipPatName;
                      end;
                   end
                   else begin
                      DisplayCurrentRecordOnMap(TheMapOwner.MapDraw,Bitmap);
                   end;
                   MyData.Next;
                   if WantOut then break;
                end;
                FreeBrushIfNeeded;
               {$IfDef RecordSym} WriteLineToDebugFile('tGISdataBaseModule.PlotDefaultSymbols out'); {$EndIf}
            end;


            procedure TGISdataBaseModule.DisplayCurrentRecordOnMap(MapDraw : tMapDraw; Bitmap : tMyBitmap);
            var
               xc,yc : integer;
               LastDisplayed : boolean;
            begin
               if (MapDraw = Nil) and ItsAShapeFile then begin
                  {$IfDef NoDBGrafs}
                  {$Else}
                     aShapeFile.PlotSingleRecordGraph(theGraphOwner,Bitmap,MyData.RecNo);
                  {$EndIf}
               end
               else begin
                  if XYZFile or ((LatFieldName <> '') and (LongFieldName <> '')) then begin
                     if ValidScreenPositionFromTable(dbxpic,dbypic) then begin
                         ScreenSymbol(Bitmap.Canvas,dbXpic,dbYpic,dbOpts.Symbol);
                     end;
                  end
                  else if ItsAShapeFile then begin
                     {$IfDef RecordCurrentRecord} WriteLineToDebugFile('TGISdataBaseModule.DisplayCurrentRecordOnMap RecNo=' + IntToStr(MyData.RecNo));{$EndIf}
                     aShapeFile.PlotSingleRecordMap(MapDraw,Bitmap,MyData.RecNo);
                  end
                  else if LatLongCornersPresent then begin
                     MapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat('LAT_HI'),MyData.GetFieldByNameAsFloat('LONG_LOW'),xc,yc);
                     MapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat('LAT_LOW'),MyData.GetFieldByNameAsFloat('LONG_HI'),dbxpic,dbypic);
                     LastDisplayed := (xc < 32000) and (yc < 32000) and (dbxpic < 32000) and (dbypic < 32000);
                     if LastDisplayed then begin
                        if (xc > dbxpic) then begin
                        end
                        else begin
                           if (xc=dbxpic) or (yc=dbypic) then Bitmap.canvas.Pixels[xc,yc] := Bitmap.Canvas.Brush.Color
                           else Bitmap.canvas.Rectangle(xc,yc,dbxpic,dbypic);
                        end;
                     end;
                  end;
               end;
            end;


            procedure TGISdataBaseModule.LabelRecordsOnMap(var Bitmap : tMyBitmap);
            var
               LastFName : PathStr;
               xCentroid,yCentroid,xp,rc,
               NeedRecordCount,RecFound,
               y2,xt,yt,Count : integer;
               Lat,Long : float64;
               TStr,TStr2 : ShortString;
               TextMyBitmap : tMyBitmap;

                  function TryToDraw : boolean;
                  var
                     ys : integer;
                  begin
                     {$IfDef RecordEveryDataBaseLabel} WriteLineToDebugFile('Try to draw ' + TStr); {$EndIf}
                     ys := -1;
                     repeat
                        y2 := dbypic - yt + ys;
                        inc(ys,2);
                        Result := PetImage.SmartTextOut(Bitmap,TextMyBitmap,xp,y2,TStr);
                     until Result or (ys > yt div 2);
                     if not Result then begin
                        ys := 0;
                        repeat
                           y2 := dbypic + ys;
                           inc(ys,2);
                           Result := PetImage.SmartTextOut(Bitmap,TextMyBitmap,xp,y2,TStr);
                        until Result or (ys > 8);
                     end;
                     {$IfDef RecordEveryDataBaseLabel} if Result then WriteLineToDebugFile('Worked Try to draw ' + TStr) else WriteLineToDebugFile('Failed Try to draw ' + TStr); {$EndIf}
                  end;

                 procedure GetLabel(aLabelField : ShortString; var aTStr : ShortString);
                 begin
                     aTStr := '';
                     if (aLabelField <> '') then begin
                        if MyData.IsFloatField(aLabelField) then begin
                           aTStr := RealToString(MyData.GetFieldByNameAsFloat(aLabelField),-18,-MDDef.MaxLabelDecimals);
                        end
                        else begin
                           aTStr := GetStringFromTableLinkPossible(aLabelField);
                        end;
                        DEMTiger.ShortenTigerName(aTStr);
                     end;
                 end;

            begin
               if (MyData = Nil) or (not dbOpts.LabelDBPlots) then exit;

               {$IfDef RecordDataBaseLabels}
                  WriteLineToDebugFile('TGISDataBase.LabelRecordsOnMap in ' + dbName + ' Label Field: ' + DBOpts.LabelField + '   Second label: ' + DBOpts.SecondLabelField);
                  WriteLineToDebugFile('Filter=' + MyData.Filter +   '   Records=' + IntToStr(MyData.FiltRecsInDB));
               {$EndIf}

               TextMyBitmap := Nil;
               LastFName := '';
               LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont1,Bitmap.Canvas.Font);
               NeedCentroid := true;
               if MDDef.AvoidTextOverprints then CreateBitmap(TextMyBitmap,Bitmap.Width,Bitmap.Height);

               if ShowSatProgress then StartProgressAbortOption('Label DB ' + ExtractFileName(dbFullName));
               NeedRecordCount := MyData.FiltRecsInDB;
               MyData.First;
               RecFound := 0;
               rc := ProgressIncrement(MyData.FiltRecsInDB);
               try
                  Count := 0;
                  repeat
                     if (RecFound mod rc = 0) then begin
                        UpdateProgressBar(RecFound/MyData.FiltRecsInDB);
                        EmpSource.Enabled := false;
                     end;
                     Bitmap.Canvas.Brush.Style := bsClear;
                     GetLabel(dbOpts.LabelField,TStr);
                     GetLabel(dbOpts.SecondLabelField,TStr2);
                     yt := Bitmap.Canvas.TextHeight(TStr);
                     xt := Bitmap.Canvas.TextWidth(TStr);
                     inc(Count);
                     if (Count >= MDDef.GISLabelSkip) and ((TStr <> '') or (TStr2 <> '') )then begin
                        if AreaShapeFile(ShapeFileType) or LatLongCornersPresent then begin
                           if AreaShapeFile(ShapeFileType) then begin
                              aShapeFile.AreaAndCentroid(TheMapOwner.MapDraw.PrimMapProj,MyData.RecNo,Lat,Long);
                           end
                           else begin
                              Long := 0.5 *(MyData.GetFieldByNameAsFloat('LONG_HI') + MyData.GetFieldByNameAsFloat('LONG_LOW'));
                              Lat := 0.5*(MyData.GetFieldByNameAsFloat('LAT_HI') + MyData.GetFieldByNameAsFloat('LAT_LOW'));
                           end;
                           TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,xCentroid,yCentroid);
                           if TheMapOwner.MapDraw.OnScreen(xCentroid,yCentroid) then begin
                              xCentroid := xCentroid - Bitmap.Canvas.TextWidth(TStr) div 2;
                              yCentroid := yCentroid - yt div 2;
                              PetImage.SmartTextOut(Bitmap,TextMyBitmap,xCentroid,yCentroid,TStr);
                           end
                           else begin
                              {$IfDef RecordDataBaseLabels} WriteLineToDebugFile('Not Plot: ' + TStr + ' Centroid: ' + LatLongDegreeToString(Lat,Long) + '  ' + IntToStr(xcentroid) + '/' + IntToStr(yCentroid)); {$EndIf}
                           end;
                        end
                        else begin
                           if ValidScreenPositionFromTable(dbxpic,dbypic) then begin
                             {$IfDef RecordEveryDataBaseLabel} WriteLineToDebugFile('Label: ' + TStr + ' Second: ' + TStr2 + ' Plot a x=' + IntToStr(dbxpic) + '  y=' + IntToStr(dbypic)); {$EndIf}
                             if (TStr2 <> '') then begin
                                {$IfDef RecordEveryDataBaseLabel} WriteLineToDebugFile('Calling PetImage.SmartTextOut'); {$EndIf}
                                LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont2,Bitmap.Canvas.Font);
                                PetImage.SmartTextOut(Bitmap,TextMyBitmap,dbxpic+4,dbypic + 2,TStr2);
                                LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont1,Bitmap.Canvas.Font);
                             end;
                             xp := dbxpic+dbOpts.XlabelOff;
                             if not TryToDraw then begin
                                xp := dbxpic-xt-dbOpts.XlabelOff;
                                TryToDraw;
                             end;
                          end;
                        end;
                        Count := 0;
                     end;
                     inc(RecFound);
                     if WantOut then break;
                     MyData.Next;
                  until MyData.eof;
               finally
               end;
               FreeAndNil(TextMyBitmap);
               NeedCentroid := false;
               PointCGMName := '';
               {$IfDef MICRODEM} WmDEM.StatusBar1.Panels[0].Text := ''; {$EndIf}
            end;


            procedure TGISdataBaseModule.PlotColorField(var Bitmap : tMyBitmap);
            var
               Recs : integer;
               Color : tColor;
               c,rc : integer;
            begin
               {$IfDef RecordSym} WriteLineToDebugFile('tGISdataBaseModule.PlotColorField ' + dbName); {$EndIf}
                Recs := 0;
                MyData.First;
                rc := ProgressIncrement(MyData.FiltRecsInDB);
                Bitmap.Canvas.Pen.Width := dbOpts.LineWidth;
                StartProgressAbortOption('COLOR field plot');
                while not MyData.EOF do begin
                   if (Recs mod rc = 0) then begin
                      UpdateProgressBar(Recs/MyData.FiltRecsInDB);
                      EmpSource.Enabled := false;
                      {$IfDef RecordDBPlotDetailed} WriteLineToDebugFile('tGISdataBaseModule.PlotColorField plotted=' + IntToStr(Recs)); {$EndIf}
                   end;
                   inc(Recs);
                   if (dbOpts.DBAutoShow in [dbasColorField]) then begin
                      dbOpts.Symbol.Color := MyData.PlatformColorFromTable;
                      Color := ConvertPlatFormColorToTColor(dbOpts.Symbol.Color);
                   end
                   else begin
                      GetIntegerFromTableLink('COLOR',C);
                      Color := c;
                   end;
                   Bitmap.Canvas.Brush.Color := Color;
                   if (not dbOpts.OutlinePolygons) or LineShapeFile(ShapeFileType) or PointShapeFile(ShapeFileType) then begin
                      Bitmap.Canvas.Pen.Color := Color;
                   end
                   else begin
                      Bitmap.Canvas.Pen.Color := ConvertPlatFormColorToTColor(dbOpts.LineColor);
                   end;
                   {$IfDef ExRedistrict}
                   {$Else}
                      if Redistricting then SetRedistrictPattern(Bitmap);
                   {$EndIf}
                   DisplayCurrentRecordOnMap(TheMapOwner.MapDraw,Bitmap);
                   MyData.Next;
                   if WantOut then break;
                end;
               {$IfDef RecordSym} WriteLineToDebugFile('tGISdataBaseModule.PlotColorField out'); {$EndIf}
            end;


            procedure TGISdataBaseModule.PlotProportionalSymbolsOnMap(var Bitmap : tMyBitmap);
            label
               DoNext;
            var
               Color : tColor;
               Recs,x,y,Radius,rc : integer;
               XCent,Ycent : float64;
               z,Value : float32;


                     function GetCorrectColor(z : float64) : tColor;
                     var
                        i : integer;
                     begin
                         if (dbOpts.DBColorScheme = LegChloropleth) and (dbOpts.dbColorMode = 1) then begin
                            Result := ConvertPlatformColorToTColor(ColorFromZColorTable(ColorDefTable,z,i));
                         end
                         else begin
                            Result := SelectedColorSchemeColorFunct(dbOpts.DBColorScheme,ColorDefTable,z,dbOpts.ColorMin,dbOpts.ColorMax);
                         end;
                     end;


            begin
               {$IfDef RecordSym} WriteLineToDebugFile('TGISdataBaseModule.PlotProportionalSymbolsOnMap in mode=' + IntToStr(dbOpts.dbAutoShow)); {$EndIf}
                if (not dbOpts.ConstantSymSize) and (dbOpts.SymSizeField = '') then begin
                   {$If Defined(RecordSym) or Defined(RecordProblems)} WriteLineToDebugFile('Exit ' + dbName + ' SymSizeField = blank,  mode=' + IntToStr(dbOpts.dbAutoShow)); {$EndIf}
                   exit;
                end;
                if AreaShapeFile(ShapeFileType) and (dbOpts.OutlinePolygons) then begin
                   Bitmap.Canvas.Pen.Color := ConvertPlatFormColorToTColor(dbOpts.LineColor);
                   Bitmap.Canvas.Pen.Width := dbOpts.LineWidth;
                   aShapeFile.PlotAllRecords(TheMapOwner.MapDraw,Bitmap);
                end;
                DefineColorTable;
                Recs := 0;
                MyData.First;
                rc := ProgressIncrement(MyData.FiltRecsInDB);

                StartProgressAbortOption('Symbols');
                if (dbOpts.dbColorMode = 4) or (dbOpts.FloatColorField = '') then begin
                   Color := ConvertPlatFormColorToTColor(MDDef.ScaledSymbolColor);
                end;

                while not MyData.EOF do begin
                   inc(Recs);
                   if (Recs mod rc = 0) then begin
                      UpdateProgressBar(Recs/MyData.FiltRecsInDB);
                      EmpSource.Enabled := false;
                   end;
                   if (dbOpts.ConstantSymSize) or GetFloat32FromTableLinkPossible(dbOpts.SymSizeField,z) then begin
                      if LineOrAreaShapeFile(ShapeFileType) then aShapeFile.AreaAndCentroid(TheMapOwner.MapDraw.PrimMapProj,MyData.RecNo,YCent,XCent)
                      else begin
                         XCent := MyData.GetFieldByNameAsFloat(LongFieldName);
                         YCent := MyData.GetFieldByNameAsFloat(LatFieldname);
                      end;
                      TheMapOwner.MapDraw.LatLongDegreeToScreen(YCent,XCent,x,y);
                      if TheMapOwner.MapDraw.OnScreen(x,y) then begin
                         if (dbOpts.FloatColorField <> '') and (dbOpts.dbColorMode <> 4) then begin
                            if GetFloat32FromTableLinkPossible(dbOpts.FloatColorField,z) then begin
                               Color := GetCorrectColor(z);
                            end
                            else begin
                               goto DoNext;
                            end;
                         end;

                         Bitmap.Canvas.Brush.Color := Color;
                         if (not dbOpts.OutlinePolygons) or LineShapeFile(ShapeFileType) or PointShapeFile(ShapeFileType) then begin
                            Bitmap.Canvas.Pen.Color := Color;
                         end
                         else begin
                            Bitmap.Canvas.Pen.Color := ConvertPlatFormColorToTColor(dbOpts.lineColor);
                            Bitmap.Canvas.Pen.Width := dbOpts.LineWidth;
                         end;

                         if  dbOpts.ConstantSymSize then begin
                             Radius := dbOpts.ScaledSymMaxSize;
                         end
                         else begin
                            if GetFloat32FromTableLinkPossible(dbOpts.SymSizeField,Value) then begin
                               PetMath.ValueInRange(Value,dbOpts.SizeMin,dbOpts.SizeMax);
                               Radius := round(dbOpts.ScaledSymMinSize + (dbOpts.ScaledSymMaxSize-dbOpts.ScaledSymMinSize) * sqrt((Value-dbOpts.SizeMin)/(dbOpts.SizeMax-dbOpts.SizeMin)));
                            end
                            else begin
                               {$IfDef RecordDetailedSym} WriteLineToDebugFile('No value for record'); {$EndIf}
                               Radius := 0;
                            end;
                         end;

                         if (Radius > 0) then begin
                            If (dbOpts.dbAutoShow = dbasScaledSquares) then Bitmap.Canvas.Rectangle(x-Radius,y-Radius,x+Radius,y+Radius)
                            else If (dbOpts.dbAutoShow = dbasScaledCircles) then Bitmap.Canvas.Ellipse(x-Radius,y-Radius,x+Radius,y+Radius)
                            else If (dbOpts.dbAutoShow = dbasTTFontSymbol) then begin
                               Bitmap.Canvas.Font.Name := dbOpts.TTSymbolFontName;
                               Bitmap.Canvas.Brush.Style := bsClear;
                               Bitmap.Canvas.Font.Color := Color;
                               Bitmap.Canvas.Font.Size := dbOpts.ScaledSymMinSize;
                               while (Bitmap.Canvas.TextHeight(dbOpts.TTSymbolFontChar) < Radius div 2) do Bitmap.Canvas.Font.Size := Bitmap.Canvas.Font.Size + 1;
                               {$IfDef RecordDetailedSym} WriteLineToDebugFile('Radius=' + IntToStr(Radius) + '  font=' + IntToStr(Bitmap.Canvas.Font.Size)); {$EndIf}
                               Bitmap.Canvas.TextOut(x-Bitmap.Canvas.TextWidth(dbOpts.TTSymbolFontChar) div 2,y - Bitmap.Canvas.TextHeight(dbOpts.TTSymbolFontChar) div 2, dbOpts.TTSymbolFontChar);
                            end;
                         end
                         else begin
                            {$IfDef RecordDetailedSym} WriteLineToDebugFile('Zero radius'); {$EndIf}
                         end;
                      end
                      else begin
                         {$IfDef RecordDetailedSym} WriteLineToDebugFile('Off screen'); {$EndIf}
                      end;
                   end
                   else begin
                      {$IfDef RecordDetailedSym} WriteLineToDebugFile('No size value'); {$EndIf}
                   end;
                   DoNext:;
                   MyData.Next;
                   if WantOut then break;
               end {while};
               {$IfDef RecordSym} WriteLineToDebugFile('TGISdataBaseModule.PlotProportionalSymbolsOnMap out mode=' + IntToStr(dbOpts.dbAutoShow)); {$EndIf}
            end;


            procedure TGISdataBaseModule.GISProportionalSymbols(dbShow : byte);
            var
               i : integer;
               ShortName : ShortString;
               UseLink : boolean;

                  procedure FillComboBoxes;
                  var
                     FieldsInDB : tStringList;
                  begin
                     FieldsInDB := Nil;
                     GetFieldsLinkPossible(MyData,LinkTable,dbOpts.VisCols,NumericFieldTypes,FieldsInDB);
                     if (FieldsInDB <> Nil) and (FieldsInDB.Count > 0) then begin
                        gis_scaled_form.ComboBox1.Items := FieldsInDB;
                        gis_scaled_form.ComboBox1.Text := dbOpts.SymSizeField;
                        gis_scaled_form.ComboBox1Change(Nil);
                        gis_scaled_form.NumbersComboBox2.Items := FieldsInDB;
                        if NumericFieldLinkPossible(dbOpts.FloatColorField) then gis_scaled_form.NumbersComboBox2.Text := dbOpts.FloatColorField;
                        gis_scaled_form.ComboBox6.Items := FieldsInDB;
                        gis_scaled_form.ComboBox6.Text := dbOpts.MagField;
                        gis_scaled_form.ComboBox7.Items := FieldsInDB;
                        gis_scaled_form.ComboBox7.Text := dbOpts.DirField;
                        gis_scaled_form.ComboBox14.Items := FieldsInDB;
                        gis_scaled_form.ComboBox14.Text := dbOpts.RedField;
                        gis_scaled_form.ComboBox15.Items := FieldsInDB;
                        gis_scaled_form.ComboBox15.Text := dbOpts.GreenField;
                        gis_scaled_form.ComboBox16.Items := FieldsInDB;
                        gis_scaled_form.ComboBox16.Text := dbOpts.BlueField;
                     end;
                     FreeAndNil(FieldsInDB);

                     GetFieldsLinkPossible(MyData,LinkTable,dbOpts.VisCols,[ftString,ftInteger,ftSmallInt,ftLargeInt],FieldsInDB);
                     if (FieldsInDB <> Nil) and (FieldsInDB.Count > 0) then begin
                        gis_scaled_form.StringsComboBox2.Items := FieldsInDB;
                        gis_scaled_form.ComboBox3.Items := FieldsInDB;
                        if StringFieldLinkPossible(dbOpts.FloatColorField) then gis_scaled_form.StringsComboBox2.Text := dbOpts.FloatColorField;
                        gis_scaled_form.ComboBox10.Text := FieldsInDB.Strings[0];
                     end;
                     FreeAndNil(FieldsInDB);

                     GetFieldsLinkPossible(MyData,LinkTable,dbOpts.VisCols,[ftString,ftFloat,ftInteger,ftSmallInt],FieldsInDB);
                     if (FieldsInDB <> Nil) and (FieldsInDB.Count > 0) then begin
                        gis_scaled_form.ComboBox4.Items := FieldsInDB;
                        gis_scaled_form.ComboBox4.Text := dbOpts.LabelField;
                        gis_scaled_form.ComboBox5.Items := FieldsInDB;
                        gis_scaled_form.ComboBox5.Text := dbOpts.SecondLabelField;
                     end;
                     FreeAndNil(FieldsInDB);
                  end;


            begin
               {$If Defined(RecordSym) or Defined(RecordDBPlot)} WriteLineToDebugFile('GISProportionalSymbols in, dbOpts.dbAutoShow=' + IntToStr(dbShow)); {$EndIf}
               ShowHourglassCursor;
               if CanPlot then begin
                  ChangeDEMNowDoing(JustWandering);
                  if (TheMapOwner.MapDraw <> Nil) and (dbOpts.dbAutoShow <> dbShow) then TheMapOwner.MapDraw.DeleteSingleMapLayer(TheMapOwner.MapDraw.DBOverlayfName[DBNumber]);
                  if (dbShow = dbasTTFontSymbol) and (dbOpts.dbAutoShow in [dbasScaledSquares,dbasScaledCircles,dbasTTFontSymbol]) then begin
                  end
                  else dbOpts.dbAutoShow := dbShow;
                  if (dbOpts.dbAutoShow in [dbasColorByString,dbasColorByNumeric]) and (dbOpts.dbColorMode = dbcmConstant) then dbOpts.dbColorMode := dbcmFieldLinear;
               end;
               if (gis_scaled_form = Nil) then begin
                  {$IfDef RecordSym} WriteLineToDebugFile('(gis_scaled_form = Nil) so start create'); {$EndIf}
                  AddSequentialIndex(RecNoFName,false);
                  {$IfDef RecordSym} WriteLineToDebugFile('sequential index added'); {$EndIf}
                  gis_scaled_form := Tgis_scaled_form.Create(Application);
                  gis_scaled_form.TheDB := DBNumber;
                  gis_scaled_form.CheckBox11.Checked := dbOpts.WindAutoSpace;
                  gis_scaled_form.Edit18.Text := IntToStr(dbOpts.WindPixelSpace);
                  gis_scaled_form.Edit23.Text := IntToStr(dbOpts.xlabelOff);
                  gis_scaled_form.SetAutoSpacing;
                  {$IfDef RecordSym} WriteLineToDebugFile('autospacing set'); {$EndIf}
                  gis_scaled_form.Edit3.Text := IntToStr(dbOpts.IconScalingFactor);
                  FillComboBoxes;
                  if dbOpts.VectorsByPolar then gis_scaled_form.RadioGroup6.ItemIndex := 0
                  else gis_scaled_form.RadioGroup6.ItemIndex := 1;
                  LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont1,gis_scaled_form.FontButton.Font);
                  gis_scaled_form.Edit8.Text := IntToStr(MDDef.MaxLabelDecimals);
                  gis_scaled_form.CheckBox10.Enabled := AreaShapeFile(ShapeFileType);
                  gis_scaled_form.BitBtn24.Enabled := AreaShapeFile(ShapeFileType);
                  gis_scaled_form.CheckBox5.Checked := dbOpts.GISVectorsByMaxSpeed;
                  gis_scaled_form.CheckBox15.Checked := dbOpts.GrayScale;
                  gis_scaled_form.CheckBox16.Checked := dbOpts.Subdue;
                  gis_Scaled_form.qfcheck1.Checked := dbOpts.qfField1 <> '';
                  gis_Scaled_form.qfcheck2.Checked := dbOpts.qfField2 <> '';
                  gis_Scaled_form.qfcheck3.Checked := dbOpts.qfField3 <> '';
                  gis_Scaled_form.Panel14.Visible := (dbOpts.qfField1 <> '') or  (dbOpts.qfField2 <> '') or (dbOpts.qfField3 <> '');
                  gis_scaled_form.Edit24.Text := IntToStr(dbOpts.xlabelOff);
                  {$IfDef RecordSym} WriteLineToDebugFile('(gis_scaled_form = Nil) done create'); {$EndIf}
               end;

               if (dbOpts.dbAutoShow = dbasMonthlyTemp) then begin
                  gis_scaled_form.Edit20.Text := IntToStr(-20);
                  gis_scaled_form.Edit21.Text := IntToStr(30);
               end;
               if (dbOpts.dbAutoShow = dbasMonthlyRain) then begin
                  gis_scaled_form.Edit20.Text := IntToStr(0);
                  gis_scaled_form.Edit21.Text := IntToStr(50);
               end;

              if CanPlot then begin
                 EmpSource.Enabled := false;
                 AutoRedrawAllowed := false;
                 LayerIsOn := true;
                 if (theMapOwner <> Nil) then begin
                    if (dbOpts.dbAutoShow <> dbShow) and (dbOpts.dbAutoShow in [dbasTTFontSymbol,dbasColorByString,dbasColorByNumeric,dbasVector,dbasConnectTwoPointsInRec,dbasConnectSeqPts]) then
                       theMapOwner.Forceredraw1Click(Nil);
                    theMapOwner.BringToFront;
                 end;
                 Petmar.ColorBitBtn(gis_scaled_form.BitBtn5,MDDef.ScaledSymbolColor);

                 gis_scaled_form.Caption := 'Plots for ' + dbName;
                 if (dbOpts.dbAutoShow = dbasConnectTwoPointsInRec) then gis_scaled_form.Caption := DBName + ' point separation';
                 if (dbOpts.dbAutoShow in [dbasColorByString]) then gis_scaled_form.Caption := DBName + ' Color code from string field';
                 if (dbOpts.dbAutoShow in [dbasColorByNumeric]) then gis_scaled_form.Caption := DBName + ' Color code from numeric field';
                 if (dbOpts.dbAutoShow in [dbasColorPosNeg]) then gis_scaled_form.Caption := DBName + ' Color code positive/negative';
                 if (dbOpts.dbAutoShow in [dbasIconField]) then gis_scaled_form.Caption := DBName + ' db icon field';
                 if (dbOpts.dbAutoShow in [dbasIconAll]) then gis_scaled_form.Caption := DBName + ' single icon';
                 if (dbOpts.dbAutoShow in [dbasMonthlyTemp]) then gis_scaled_form.Caption := 'Monthly temperatures';
                 if (dbOpts.dbAutoShow in [dbasMonthlyRain]) then gis_scaled_form.Caption := 'Monthly rainfall';
                 gis_scaled_form.RadioGroup8.ItemIndex := 0;  //Jan MDDef.WindMonth0based;

                 if (dbOpts.dbAutoShow in [dbasAnimate]) then begin
                    {$IfDef RecordSym} WriteLineToDebugFile('[gisAnimate] setup'); {$EndIf}
                    gis_scaled_form.Caption := DBName + ' Animate';
                    gis_scaled_form.CheckBox8.Enabled := MyData.FieldExists('SEQUENCE');
                    gis_scaled_form.BitBtn13.Enabled := gis_scaled_form.CheckBox8.Enabled;
                    LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont1,gis_scaled_form.FontButton2.Font);
                    Petmar.ColorLineWidthBitBtn(gis_scaled_form.BitBtn13,dbOpts.LineColor,dbOpts.LineWidth);
                 end;

                 if (dbOpts.dbAutoShow in [dbasVector]) then begin
                    {$IfDef RecordSym} WriteLineToDebugFile('[gisVectors] setup'); {$EndIf}
                    gis_scaled_form.Caption := DBName + ' Vectors';
                     Petmar.ColorLineWidthBitBtn(gis_scaled_form.BitBtn11,GISdb[gis_scaled_form.theDB].dbOpts.LineColor,
                          GISdb[gis_scaled_form.theDB].dbOpts.LineWidth);
                     gis_scaled_form.Edit5.Text := RealToString(dbOpts.VectorLineMult,-12,-2);
                     gis_scaled_form.Edit6.Text := IntToStr(dbOpts.VectorThinFactor);
                     gis_scaled_form.CheckBox6.Checked := MDDef.PlotArrowHead;
                     if MyData.FieldExists('JAN_V_MS') or (MyData.FieldExists('VE')) then begin
                        gis_scaled_form.RadioGroup8.Visible := true;
                        gis_scaled_form.RadioGroup6.ItemIndex := 1;
                        gis_scaled_form.BitBtn25.Visible := false;
                     end
                     else begin
                        gis_scaled_form.RadioGroup8.Visible := MyData.FieldExists(MonthFieldName);
                        gis_scaled_form.RadioGroup6.ItemIndex := 0;
                     end;
                     gis_scaled_form.Edit19.Text := 'Speed';
                 end;

                 if (dbOpts.dbAutoShow in [dbasConnectSeqPts]) then begin
                    {$IfDef RecordSym} WriteLineToDebugFile('[gisConnectSeqPts] setup'); {$EndIf}
                    gis_scaled_form.ArrowCheckBox.Checked := MDDef.ConnectArrows;
                    gis_scaled_form.Edit4.Text := IntToStr(MDDef.ConnectArrowSpacing);
                    Petmar.ColorLineWidthBitBtn(gis_scaled_form.BitBtn9,dbOpts.LineColor,dbOpts.LineWidth);
                 end;

                 if dbOpts.dbAutoShow = dbasScaledSquares then gis_scaled_form.RadioGroup1.ItemIndex := 0;
                 if dbOpts.dbAutoShow = dbasScaledCircles then gis_scaled_form.RadioGroup1.ItemIndex := 1;
                 if dbOpts.dbAutoShow = dbasTTFontSymbol then gis_scaled_form.RadioGroup1.ItemIndex := 2;

                 gis_scaled_form.TrackBar1.Position := dbOpts.Opacity;
                 gis_scaled_form.Edit1.Text := IntToStr(dbOpts.ScaledSymMaxSize);
                 gis_scaled_form.Edit22.Text := IntToStr(dbOpts.ScaledSymMinSize);
                 gis_scaled_form.ComboBox1.Text := dbOpts.SymSizeField;

                 if DbOpts.ConstantSymSize then gis_scaled_form.RadioGroup3.ItemIndex := 1
                 else gis_scaled_form.RadioGroup3.ItemIndex := 0;

                 gis_scaled_form.RadioGroup4.Visible := (dbOpts.dbAutoShow in [dbasColorByNumeric]);

                 gis_scaled_form.SetPanels;
                 gis_scaled_form.FillInLegendPanel;

                 gis_scaled_form.BitBtn3.Enabled := gis_scaled_form.RadioGroup1.ItemIndex = 2;
                 gis_scaled_form.StillSettingUp := false;
                 gis_scaled_form.DrawSymbol;
                 gis_scaled_form.ShowFieldRanges(false);
              end
              else begin
                 gis_scaled_form.Caption := DBName + ' Quick Filters';
              end;
               gis_scaled_form.Show;
               {$IfDef RecordSym} WriteLineToDebugFile('GISProportionalSymbols out, dbAutoShow=' + IntToStr(dbOpts.dbAutoShow) + '  ' + gis_scaled_form.Caption); {$EndIf}
            end;


            procedure TGISdataBaseModule.DisplayTheRecord(j : integer; ShowItModal : boolean = true; CanEdit : boolean = false);
            begin
               AddSequentialIndex(RecNoFName,false);
               DEMShowDBRecord.FullDBName := dbFullName;
               DisplaySingleDataBaseRecord(DBNumber,CanEdit,dbName,j,ShowItModal,-999,1,dbOpts.LabelField);
            end;


            procedure TGISdataBaseModule.PlotFieldOnMap(aField : shortstring; Minx : float64 = 9999; MaxX : float64 = -9999);
            begin
               if NumericFieldLinkPossible(aField) then begin
                  {$IfDef RecordDBPlot} WriteLineToDebugFile('TGISdataBaseModule.PlotFieldOnMap in'); {$EndIf}
                  dbOpts.DBAutoShow := dbasColorByNumeric;
                  dbOpts.FloatColorField := afield;
                  dbOpts.dbColorMode := dbcmFieldLinear;
                  EmpSource.Enabled := false;
                  If (MinX > MaxX) then MyData.FindFieldRange(aField,MinX,MaxX);
                  dbOpts.ColorMin := MinX;
                  dbOpts.ColorMax := MaxX;
                  RedrawLayerOnMap;
                  {$IfDef RecordDBPlot} WriteLineToDebugFile('TGISdataBaseModule.PlotFieldOnMap out'); {$EndIf}
               end;
            end;


            procedure TGISdataBaseModule.PlotAndLabelCurrentRecord(Bitmap : tMyBitmap; Plot : boolean = true);
            var
               xp,yp,Angle,Loc,Len,High : integer;
               Size : byte;
               OldFont : tFont;
               TStr : ShortString;
               Sym : tDrawingSymbol;
               Color : tPlatformColor;
            begin
                if (not MyData.FieldExists('PLOT')) or (MyData.GetFieldByNameAsString('PLOT') = 'Y') then begin
                   if FontFieldExists then begin
                       TStr := MyData.GetFieldByNameAsString('NAME');
                       OldFont := tFont.Create;
                       OldFont.Name := Bitmap.Canvas.Font.Name;
                       OldFont.Size := Bitmap.Canvas.Font.Size;
                       OldFont.Color := Bitmap.Canvas.Font.Color;
                       MyData.DefineFontFromTable(Bitmap.Canvas.Font);
                       Bitmap.Canvas.Brush.Style := bsClear;
                   end;
                   if ValidScreenPositionFromTable(xp,yp) then begin
                      if Plot then begin
                         MyData.DefinePointSymbol(Sym,Size,Color);
                         if (Size > 0) then PETMAR.ScreenSymbol(Bitmap.Canvas,xp,yp,Sym,Size,Color);
                      end;
                      if FontFieldExists then begin
                          if MyData.GetFieldByNameAsString('ROT_ANGLE') = '' then Angle := 0
                          else Angle := MyData.GetFieldByNameAsInteger('ROT_ANGLE');
                          if (Angle <> 0) then PETMAR.CanvasTextOutAngle(Bitmap.Canvas,xp+4,yp+4,10*Angle,TStr)
                          else begin
                             Len := Bitmap.Canvas.TextWidth(TStr);
                             High := Bitmap.Canvas.TextHeight(TStr);

                             // 1   2   3
                             // 4       5
                             // 6   7   8
                             if MyData.FieldExists('PLACE_TEXT') then begin
                                Loc := MyData.GetFieldByNameAsInteger('PLACE_TEXT');
                             end
                             else Loc := 5;
                             if Loc in [3,5,8] then xp := xp + 4;
                             if Loc in [1,4,6] then xp := xp - 4 - Len;
                             if Loc in [2,7] then xp := xp - Len div 2;
                             if Loc in [1,2,3] then yp := yp - 4 - High;
                             if Loc in [4,5] then yp := yp - 4 - High div 2;
                             if Loc in [6,7,8] then yp := yp + 4;
                             Bitmap.Canvas.TextOut(xp,yp,TStr)
                          end;
                      end;
                   end;
                   if FontFieldExists then begin
                      Bitmap.Canvas.Font.Name := OldFont.Name;
                      Bitmap.Canvas.Font.Size := OldFont.Size;
                      Bitmap.Canvas.Font.Color := OldFont.Color;
                      OldFont.Free;
                   end;
                end;
            end;


            procedure TGISdataBaseModule.PlotAndLabelPoints(Bitmap : tMyBitmap; Plot : boolean = true);
            begin
               {$IfDef RecordDataBaseLabels} WriteLineToDebugFile('tGISDataBase.PlotAndLabelPoints'); {$EndIf}
               if LatLongFieldsPresent then begin
                  MyData.First;
                  repeat
                     EmpSource.Enabled := false;
                     PlotAndLabelCurrentRecord(Bitmap,Plot);
                     MyData.Next;
                     if WantOut then  break;
                  until MyData.EOF;
               end;
            end;


            procedure TGISdataBaseModule.PlotRecord1to1Map(var NewMap : tMapForm; var Bitmap : tMyBitmap);
            begin
               NewMap := TheMapOwner.DuplicateMap(false,false,true);
               NewMap.MapDraw.MapType := mtSatBlank;
               NewMap.Closable := true;
               NewMap.MapDraw.MaximizeLatLongMapCoverage(MyData.GetFieldByNameAsFloat('LAT_LOW'),MyData.GetFieldByNameAsFloat('LONG_LOW'),MyData.GetFieldByNameAsFloat('LAT_HI'),MyData.GetFieldByNameAsFloat('LONG_HI'));
               NewMap.N11view1Click(Nil);
               CloneImageToBitmap(NewMap.Image1,Bitmap);
               Bitmap.Canvas.Brush.Color := clBlack;
               Bitmap.Canvas.Brush.Style := bsSolid;
               aShapeFile.PlotSingleRecordMap(NewMap.MapDraw,Bitmap,MyData.RecNo);
            end;





