{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 9/27/2015       }
{                                 }
{ include file for demdatabase    }
{_________________________________}

  no longer used


function TGISdataBaseModule.ChloroplethLegend(aLabel : shortString) : tMyBitmap;
var
   i,fhi : integer;
begin
   fHi := 22;
   CreateBitmap(Result,100,25 + fHi*ColorDefTable.ZTableEntries);
   Result.Canvas.Brush.Style := bsSolid;

   for i := 1 to ColorDefTable.ZTableEntries do begin
      Result.Canvas.Brush.Style := bsSolid;
      Result.Canvas.Pen.Color := Petmar.RGBtoTColor(ColorDefTable.ZTableColors[i]);
      Result.Canvas.Brush.Color := Petmar.RGBtoTColor(ColorDefTable.ZTableColors[i]);
      Result.Canvas.Rectangle(5,25 + pred(i)*fHi,25,25 +i*fhi);
      if (i > 1) then begin
         Result.Canvas.Brush.Style := bsClear;
         Result.Canvas.Pen.Color := clBlack;
         Result.Canvas.MoveTo(5,25+pred(i)*fHi);
         Result.Canvas.LineTo(30,25+pred(i)*fHi);
         Result.Canvas.TextOut(35,25+pred(i)*fHi-5,RealToString(ColorDefTable.ZTableValue[i],-12,-2));
      end;
   end;
   if aLabel = '' then aLabel := dbOpts.ColorFieldName;

   Result.Canvas.Brush.Style := bsClear;
   Result.Canvas.TextOut(5,5,aLabel);
end;



function TGISdataBaseModule.StringFieldLegend : tMyBitmap;
var
   i,tl,l,fHi : integer;
   ColorDefTable : tColorTableDefinitions;
   StringCategories : tStringList;
begin
   {$IfDef RecordDataBaseProblems}
   WriteLineToDebugFile('Tgis_scaled_form.MakeStringFieldLegend',true);
   {$EndIf}
   EmpSource.Enabled := false;
   PetDBUtils.FindUniqueEntriesLinkPossible(MyData,LinkTable,
            dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,dbOpts.ColorFieldName,StringCategories);
   fHi := 22;

   CreateBitmap(Result,1000,fHi*StringCategories.Count);
   Result.Canvas.Brush.Color := clGray;
   Result.Canvas.Brush.Style := bsSolid;
   Result.Canvas.Pen.Color := clGray;

   //DefineColorTableValues(MDDef.GISChloroplethPalName,0,pred(StringCategories.Count),ColorDefTable);

   DefineColorTableValues(DBColorPaletteName,0,pred(StringCategories.Count),ColorDefTable);



   tl := 0;
   for I := 0 to pred(StringCategories.Count) do begin
      SetBitmapColorsForStringFieldPlot(Result,I,pred(StringCategories.Count));
      //LegendBitmap.Canvas.Pen.Color := SelectedColorSchemeColorFunct(DBColorScheme,ColorDefTable,i,0,pred(StringCategories.Count));
      //LegendBitmap.Canvas.Brush.Color := LegendBitmap.Canvas.Pen.Color;
      //LegendBitmap.Canvas.Rectangle(10,0+i*fHi,40,15 + i*fHi);
      Result.Canvas.Rectangle(10,0+i*fHi,40,15 + i*fHi);
   end;

   //Result.Canvas.Draw(0,0,LegendBitmap);
   Result.Canvas.Brush.Color := clWhite;
   Result.Canvas.Font.Size := 12;
   Result.Canvas.Font.Style := [fsBold];

   for i := 0 to pred(StringCategories.Count) do begin
      {$IfDef RecordDataBaseProblems}
      WriteLineToDebugFile(StringCategories.Strings[i]);
      {$EndIf}
      Result.Canvas.TextOut(50,i*fHi,StringCategories.Strings[i]);
      l := Result.Canvas.TextWidth(StringCategories.Strings[i]);
      if (l + 50 > tl) then tl := l + 50;
   end;
   Result.Canvas.Brush.Style := bsClear;
   Result.Canvas.Pen.Color := clBlack;
   Result.Canvas.Pen.Width := 1;
   Result.Canvas.Rectangle(0,0,tl+3,pred(Result.Height));
   Result.Width := tl + 3;
   StringCategories.Free;
end;


procedure TGISdataBaseModule.PlotDBNumericField(var Bitmap : tMyBitmap);
var
   x : float;
   i : integer;
begin
   with MyData do begin
      EmpSource.Enabled := false;
      i := 0;
      StartProgressAbortOption(dbOpts.ColorFieldName);

      PrepColors(Bitmap);
      MyData.First;
      while not MyData.eof do begin
         inc(i);
         if (i mod 1000 = 0) then begin
            UpDateProgressBar(i/MyData.RecordCount);
            EmpSource.Enabled := false;
         end;
         if GetFloatFromTableLinkPossible(MyData,LinkTable,dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,dbOpts.ColorFieldName,x) then begin
            if MDDef.ClipZColors and (x < dbOpts.ColorMin) then Bitmap.Canvas.Pen.Color := MDdef.LowOffscaleColor
            else if MDDef.ClipZColors and(x > dbOpts.ColorMax) then Bitmap.Canvas.Pen.Color := MDdef.HighOffscaleColor
            else Bitmap.Canvas.Pen.Color := SelectedColorSchemeColorFunct(DBColorScheme,ColorDefTable,x,dbOpts.ColorMin,dbOpts.ColorMax);
            Bitmap.Canvas.Brush.Color := Bitmap.Canvas.Pen.Color;
            DisplayCurrentRecordOnMap(Bitmap);
         end;
         MyData.Next;
         if WantOut then Break;
      end;
      EmpSource.Enabled := true;
      EndProgress;
   end;
end;


procedure TGISdataBaseModule.SetBitmapColorsForStringFieldPlot(var Bitmap : tMyBitmap; i,Total : integer);
var
   Color : tColor;
begin
    Color := SelectedColorSchemeColorFunct(DBColorScheme,ColorDefTable,i,0,Total);
    Bitmap.Canvas.Brush.Color := Color;
    Bitmap.Canvas.Pen.Color := Color;
    if AreaShapeFile(ShapeFileType) and (not MDDef.GISNoBorders) then begin
       Bitmap.Canvas.Pen.Color := MDDef.GISOutlineColor;
       Bitmap.Canvas.Pen.Width := MDDef.GISOutlineWidth;
    end;
end;


procedure TGISdataBaseModule.PlotDBStringField;
var
   OldFilter : ANSIString;
   StringCategories : tStringlist;
   i : integer;
   TStr : shortString;
begin
   {$IfDef RecordDBPlot}
   WriteLineToDebugFile('TGISdataBaseModule.PlotDBStringField in',true);
   {$EndIf}
   if (dbOpts.ColorFieldName = '') then exit;
   EmpSource.Enabled := false;
   PetDBUtils.FindUniqueEntriesLinkPossible(MyData,LinkTable,dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,
           dbOpts.ColorFieldName,StringCategories);
   OldFilter := MyData.Filter;
   DefineColorTableValues(DBColorPaletteName,0,pred(StringCategories.Count),ColorDefTable);
    {$IfDef RecordColorPalette}
    ShowColorPalette('after string define');
    {$EndIf}

    StartProgressAbortOption('Plot');
    Bitmap.Canvas.Pen.Width := dbOpts.LineSize;
    for I := 0 to pred(StringCategories.Count) do begin
       if (StringCategories.Count > 1) then UpdateProgressBar(I/pred(StringCategories.Count));
       EmpSource.Enabled := false;
       SetBitmapColorsForStringFieldPlot(Bitmap,I,pred(StringCategories.Count));
       TStr := StringCategories.Strings[i];
       {$IfDef RecordDBPlot}
       WriteLineToDebugFile('i=' + i.ToString + '    '  + TStr);
       {$EndIf}
       if (MyData.GetFieldType(dbOpts.ColorFieldName) = ftString) then TStr := QuotedStr(TStr);

       MyData.ApplyFilter( PetDBUtils.AddAndIfNeeded(OldFilter) + dbOpts.ColorFieldName + '=' + TStr);

       {$IfDef RecordDBPlot}
       WriteLineToDebugFile('filter=' + MyData.Filter + '   recs=' + IntToStr(MyData.RecordCount));
       {$EndIf}

       EmpSource.Enabled := false;
       while not MyData.EOF do begin
          EmpSource.Enabled := false;
          DisplayCurrentRecordOnMap(Bitmap);
          MyData.Next;
          if WantOut then break;
       end;
       if WantOut then break;
    end;
    EndProgress;
    MyData.ApplyFilter(OldFilter);
    StringCategories.Free;
   {$IfDef RecordDBPlot}
   Bitmamp.SaveToFile(MDTempDir + 'PlotDBStringField.png');
   {$EndIf}
end;


procedure AddBoundingBoxToShapefile(fName : PathStr);
var
   GISNum : integer;
begin
   if OpenNumberedGISDataBase(GISNum,fName) then begin
      GISdb[GISNum].aShapeFile.AddFields(afBoundingBox,GISdb[GISNum].MyData);
      CloseAndNilNumberedDB(GISNum);
   end;
end;

procedure OpenDBForModalEdit(fName : PathStr);
var
   DBNum : integer;
begin
   DEMDataBase.OpenNumberedGISDataBase(DBNum,fName,true);
   GISdb[DBNum].dbTablef.FormStyle := fsNormal;
   GISdb[DBNum].dbTablef.Hide;
   GISdb[DBNum].dbTablef.CheckBox1.Checked := true;
   GISdb[DBNum].dbTablef.EditSymbologyOnly := true;
   GISdb[DBNum].dbTablef.Panel7.Visible := true;
   GISdb[DBNum].dbTablef.ShowModal;
end;


procedure TGISdataBaseModule.AddAndFillFieldFromDEM(AddDEM : tAddDEM; fName : PathStr = '');
var
   Dir : tCompassDirection;
   Slope,AspectDir,z,Lat,Long : float;
   x,y,i,UseDEM,Col,Row : integer;
   eName : string12;
begin
   if (AddDEM = adPickNearest) then begin
      if not GetDEM(UseDEM, true,'Add field from') then exit;
      fName := GetFieldNameForDB('Field to add',True,fName);
      AddFieldToDataBase(ftFloat,fName,14,6);
   end;
   if (AddDEM = adElevDiff) then begin
      eName := 'ELEV';
      if not MyData.FieldExists(eName) then eName := 'Z';
      fName := 'DZ';
      fName := GetFieldNameForDB('Elev difference field name',True,fName);
      AddFieldToDataBase(ftFloat,fName,8,2);
   end;
   if (AddDEM = adElevInterp) or (AddDEM = adElevNearest) then begin
      if (fName = '') then begin
         fName := 'ELEV';
         if not MyData.FieldExists(fName) then fName := 'Z';

         if MyData.FieldExists(fName) then begin
            If not AnswerIsYes('Field ' + fName + ' exists; Overwrite') then begin
              Petmar.GetString('Elev field name',fName,true,DBaseFieldNameChars);
            end;
         end;
      end;

      if not MyData.FieldExists(fName) then begin
         if (AddDEM = adElevNearest) and (DEMGlb[theMapDraw.DEMonMap].Headrecs.DEMPrecision <> FloatingPointDEM)then begin
           AddFieldToDataBase(ftInteger,fName,6);
         end
         else begin
           AddFieldToDataBase(ftFloat,fName,14,6);
         end;
      end;
   end;
   if (AddDEM = adSlope) then begin
      AddFieldToDataBase(ftFloat,'SLOPE_PC',9,2);
      AddFieldToDataBase(ftFloat,'ASPECT',5,1);
   end;

   ShowHourglassCursor;
   EmpSource.Enabled := false;
   i := 0;
   StartProgress('DEM add');
   MyData.First;
   while not MyData.Eof do begin
      inc(i);
      if I mod 50 = 0 then begin
         UpdateProgressBar(i/MyData.RecordCount);
         EmpSource.Enabled := false;
      end;

      if ValidLatLongFromTable(Lat,Long) then begin
          MyData.Edit;
          if (AddDEM = adElevDiff) then begin
             if DEMGlb[theMapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then
                MyData.SetFieldByNameAsFloat(fName,MyData.GetFieldByNameAsFloat(eName) - z);
          end;
          if (AddDEM = adElevNearest) then begin
             DEMGlb[theMapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,Col,Row);
             if DEMGlb[theMapDraw.DEMonMap].GetElevMeters(Col,Row,z) then begin
                if (DEMGlb[theMapDraw.DEMonMap].Headrecs.DEMPrecision <> FloatingPointDEM) then
                    MyData.SetFieldByNameAsInteger(fName,round(z))
                else MyData.SetFieldByNameAsFloat(fName,z);
             end;
          end;
          if (AddDEM = adElevInterp) then begin
             if DEMGlb[theMapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
                MyData.SetFieldByNameAsFloat(fName,z);
             end;
          end;
          if (AddDEM = adSlope) then begin
             DEMGlb[theMapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,x,y);
             if DEMGlb[theMapDraw.DEMonMap].FindDownHillDirection(x,y,z,Dir,Slope,AspectDir) then begin
                MyData.SetFieldByNameAsFloat('SLOPE_PC',100 * Slope);
                MyData.CarefullySetFloat('ASPECT',AspectDir,0.1);
             end;
          end;
          if (AddDEM = adPickNearest) then begin
             if DEMGlb[UseDEM].GetElevFromLatLongDegree(Lat,Long,z) then
                MyData.SetFieldByNameAsFloat(fName,z);
          end;
      end;
      MyData.Next;
   end;
   dbTablef.ShowStatus;
end;


procedure TGISdataBaseModule.IntervisibilityFromPoint(FromLat,FromLong,CameraElevation,TowerHeight : float);
var
   i : integer;
   TStr : shortstring;
   ch : char;
   Lat,Long,Distance,BearingAngle,Value,TargetZ,Pitch,BlockDist : float;
begin
    AddFieldToDataBase(ftString,'VIS',1);
    AddFieldToDataBase(ftFloat,'DIST_MILES',10,2);
    AddFieldToDataBase(ftString,'AZ_DMS',12);
    AddFieldToDataBase(ftFloat,'PITCH',6,2);
    AddFieldToDataBase(ftFloat,'DIST_KM',10,2);
    AddFieldToDataBase(ftFloat,'AZIMUTH',5,1);
    AddFieldToDataBase(ftString,'PITCH_DMS',12);
    AddSymbolToDB;
    dbOpts.DBAutoShow := dbasSymbolInDB;

    i := 0;
    StartProgress('Intervisibility');
    MyData.First;
    while not MyData.eof do begin
       inc(i);
       if i mod 10 = 0 then UpdateProgressBar(i/ MyData.RecordCount);

       ValidLatLongFromTable(Lat,Long);
       DEMDatum.CalculateDistanceBearing(FromLat,FromLong,Lat,Long,Distance,BearingAngle);
       MyData.Edit;
       MyData.SetFieldByNameAsString('PITCH','');
       MyData.SetFieldByNameAsString('VIS','');
       try
          value := Distance * 0.001;
          MyData.CarefullySetFloat('DIST_KM',value,0.01);
          value := Distance * 0.62 * 0.001;
          MyData.CarefullySetFloat('DIST_MILES',value,0.01);
          MyData.CarefullySetFloat('AZIMUTH',BearingAngle,0.1);
          TStr := ConvertToDegreesString(BearingAngle,NearestSecond);
          MyData.SetFieldByNameAsString('AZ_DMS',TStr);
          if DEMglb[theMapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,TargetZ) then begin
             Pitch := ArcTan((TargetZ - CameraElevation - DropEarthCurve(Distance)) / Distance) / DegToRad;
             MyData.CarefullySetFloat('PITCH',Pitch,0.01);
             MyData.SetFieldByNameAsString('PITCH_DMS',ConvertToDegreesString(Pitch,NearestSecond));
             if DEMglb[theMapDraw.DEMonMap].LatLongDegreePointsIntervisible(FromLat,FromLong,TowerHeight,Lat,Long,1,Distance,BlockDist,Nil) then ch := 'Y'
             else ch := 'N';
             MyData.SetFieldByNameAsString('VIS',ch);
             if ch = 'Y' then MyData.PostPointSymbol(MDDef.VisPtSymbol.DrawingSymbol,MDDef.VisPtSymbol.Size,MDDef.VisPtSymbol.Color)
             else MyData.PostPointSymbol(MDDef.MaskPtSymbol.DrawingSymbol,MDDef.MaskPtSymbol.Size,MDDef.MaskPtSymbol.Color);
          end;
       except
         on Exception do writelinetodebugFile(TStr);
       end;
       MyData.Next;
    end;
    EndProgress;
    RedrawLayerOnMap;
end;


procedure TGISdataBaseModule.PlotIcons(Var Bitmap : tMyBitmap);
var
   Lat,Long : float;
   x,y : integer;
begin
   MyData.First;
   repeat
      EmpSource.Enabled := false;
       if LineOrAreaShapeFile(ShapeFileType) then aShapeFile.AreaAndCentroid(TheMapDraw.CurrentMapDatum,MyData.RecNo,Lat,Long)
       else begin
          Long := MyData.GetFieldByNameAsFloat(LongFieldName);
          Lat := MyData.GetFieldByNameAsFloat(LatFieldname);
       end;
      if (dbOpts.DBAutoShow = dbasIconAll) then begin
         theMapDraw.PlotAnIcon(Bitmap,Lat,Long,dbOpts.AllIconFName,dbOpts.IconScalingFactor);
      end
      else if (dbOpts.DBAutoShow = dbasIconField) then begin
         if (dbOpts.PreferIconFieldName <> '') then begin
            theMapDraw.PlotAnIcon(Bitmap,Lat,Long,MyData.GetFieldByNameAsString(dbOpts.PreferIconFieldName),dbOpts.IconScalingFactor);
         end
         else if PointSymbolFieldsPresent then begin
            TheMapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
            Petmar.ScreenSymbol(Bitmap.Canvas,x,y,tDrawingSymbol(MyData.GetFieldByNameAsInteger('SYM_TYPE')),
                  MyData.GetFieldByNameAsInteger('SYM_SIZE'),MyData.GetFieldByNameAsInteger('SYM_COLOR'));
         end;
      end;
      MyData.Next;
   until MyData.eof;
end;


procedure TGISdataBaseModule.PlotVectorsOnMap(var Bitmap : tMyBitmap);
var
   Lat,Long,Lat2,Long2,Dir,Mag,Value,xf,yf,dLong,LongCent,Distance,Bearing,ve,vn : float;
   Recs, err,i,j,x1,y1,x2,y2,Dist : integer;
   TStr : ShortString;
   Color : tColor;
   PlotIt : boolean;
begin
   {$IfDef RecordSymProblems}
   WriteLineToDebugFile('tGISdataBaseModule.PlotVectorsOnMap, mode=' + IntToStr(ord(GISWhatDraw)) + '  ' + TimeToStr(now),true);
   {$EndIf}

   with MyData do begin
      if FieldExists(dbOpts.DirField) then begin
         {$IfDef RecordSymProblems}
         WriteLineToDebugFile('Mag=' + dbOpts.MagField + '   dir=' + dbOpts.DirField);
         {$EndIf}
         ForceShow := true;
         if MDDef.GISVectorsByMaxSpeed then begin
            dbOpts.ColorMax := MDDef.GISVectorsMaxSpeed;
            dbOpts.ColorMin := 0;
         end;

         if dbOpts.WindAutoSpace then begin
            dbOpts.VectorThinFactor := 0;
            MyData.First;
            ValidLatLongFromTable(Lat,Long);
            MyData.Next;
            ValidLatLongFromTable(Lat2,Long2);
            dLong := abs(Long2 - Long);
            theMapDraw.ScreenToLatLongDegree(theMapDraw.MapXSize div 2,theMapDraw.MapYSize div 2,Lat,LongCent);
            repeat
                inc(dbOpts.VectorThinFactor);
                Long := LongCent + dbOpts.VectorThinFactor * dLong;
                Long2 := LongCent - dbOpts.VectorThinFactor * dLong;
                theMapDraw.LatLongDegreeToScreen(Lat,Long,x1,y1);
                theMapDraw.LatLongDegreeToScreen(Lat,Long2,x2,y2);
                Dist := round(Sqrt(sqr(x1-x2)+ sqr(y2-y1)));
            until (Dist >= 2 * dbOpts.WindPixelSpace) or (dbOpts.VectorThinFactor > 25);
         end;

         {$IfDef RecordDataInsideLoopPlots}
         if NeedColorValue then WriteLineToDebugFile('NeedColorValue = true');
         WriteLineToDebugFile('GISColorSource = ' + IntToStr(ord(MDDef.GISColorSource)));
         {$EndIf}
         EmpSource.Enabled := false;
         Recs := 0;
         MyData.First;
         StartProgressAbortOption('Vectors');
         Color := dbOpts.BasicColor;
         while not MyData.EOF do begin
            inc(Recs);
            if (recs mod 1000 = 0) then UpdateProgressBar(Recs*dbOpts.VectorThinFactor/MyData.RecordCount);

           {$IfDef RecordGISvectors}
           WriteLineToDebugFile('Start Vector Plot',true);
           {$EndIf}
            PlotIt := true;
            if SimplePointFile then begin
               PlotIt := ValidLatLongFromTable(Lat,Long);
            end
            else if LatLongCornersPresent then begin
               Lat := 0.5*(GetFieldByNameAsFloat('LAT_HI')+GetFieldByNameAsFloat('LAT_LOW'));
               Long := 0.5*(GetFieldByNameAsFloat('LONG_HI')+GetFieldByNameAsFloat('LONG_LOW'));
            end;
            theMapDraw.LatLongDegreeToScreen(Lat,Long,dbxpic,dbypic);
            if theMapDraw.OnScreen(dbxpic,dbypic) then begin
               if VelNECompPresent then begin
                  ve := GetFieldByNameAsFloat(dbOpts.MagField);
                  vn := GetFieldByNameAsFloat(dbOpts.DirField);
                  Dir := HeadingOfLine(ve,vn);
                  Mag := sqrt(sqr(ve) + sqr(vn));
               end
               else if dbOpts.VectorsByPolar then begin
                  PlotIt := CarefullyGetFieldByNameAsFloat(dbOpts.DirField,Dir);
                  if PlotIt then begin
                     if dbOpts.MagField = '' then Mag := 1
                     else PlotIt := CarefullyGetFieldByNameAsFloat(dbOpts.MagField, Mag);
                  end;
               end
               else begin
                  PlotIt := CarefullyGetFieldByNameAsFloat(dbOpts.DirField,xf) and CarefullyGetFieldByNameAsFloat(dbOpts.MagField,yf);
                  if PlotIt then begin
                     Dir := HeadingOfLine(yf,xf);
                     Mag := sqrt(sqr(xf) + sqr(yf));
                  end;
               end;

               if PlotIt then begin
                  {$IfDef RecordGISvectors}
                  WriteLineToDebugFile('PlotIt is good,  mag=' + RealToString(Mag,-8,1) + '  dir=' + RealToString(Dir,-8,1));
                  {$EndIf}
                  if  MDDef.GISVectorsByMaxSpeed then
                     Color := SelectedColorSchemeColorFunct(DBColorScheme,ColorDefTable,Mag,dbOpts.ColorMin,dbOpts.ColorMax)
                   else begin
                      //PlotIt := ComputeColorFromRecord(Color);
                   end;

                   if PlotIt and (abs(Mag) > 0.01) then begin
                      PETImage.PlotOrientedLine(Bitmap,dbxpic,dbypic,round(dbOpts.VectorLineMult*Mag),round(Dir),
                                  Color,dbOpts.VectorWidth,MDDef.PlotArrowHead,MDDef.ReverseArrow);
                   end;
               end;
            end;
            for i := 1 to dbOpts.VectorThinFactor do MyData.Next;
            if WantOut then break;
         end;
         EmpSource.Enabled := true;
         EndProgress;
      end;
   end;

   {$IfDef RecordSymProblems}
   WriteLineToDebugFile('tGISdataBaseModule.PlotVectorsOnMap out');
   {$EndIf}
end;


procedure TGISdataBaseModule.PlotProportionalSymbolsOnMap(var Bitmap : tMyBitmap);
var
   Color : tColor;
   Recs,x,y,Radius : integer;
   XCent,Ycent,Value : float;
begin
    if AreaShapeFile(ShapeFileType) and (MDDef.OutlineProportionalPolygons)  then begin
       Bitmap.Canvas.Pen.Color := MDDef.GISOutlineColor;
       Bitmap.Canvas.Pen.Width := MDDef.GISOutlineWidth;
       aShapeFile.PlotAllRecords(theMapDraw,Bitmap);
    end;


    Recs := 0;
    MyData.First;
    while not MyData.EOF do begin
       inc(Recs);
       if (Recs mod 1000 = 0) then begin
          UpdateProgressBar(Recs/MyData.RecordCount);
          EmpSource.Enabled := false;
       end;
       if ComputeColorFromRecord(Color) then begin
          Bitmap.Canvas.Brush.Color := Color;
          if MDDef.GISNoBorders or LineShapeFile(ShapeFileType) or PointShapeFile(ShapeFileType) then begin
             Bitmap.Canvas.Pen.Color := Color;
          end
          else begin
             Bitmap.Canvas.Pen.Color := MDDef.GISOutlineColor;
             Bitmap.Canvas.Pen.Width := MDDef.GISOutlineWidth;
          end;
          if LineOrAreaShapeFile(ShapeFileType) then aShapeFile.AreaAndCentroid(TheMapDraw.CurrentMapDatum,MyData.RecNo,YCent,XCent)
          else begin
             XCent := MyData.GetFieldByNameAsFloat(LongFieldName);
             YCent := MyData.GetFieldByNameAsFloat(LatFieldname);
          end;
          theMapDraw.LatLongDegreeToScreen(YCent,XCent,x,y);

          if (MDDef.GISSymbolSize = gisSizeDBfield) and (SymSizeField <> '') then begin
             if GetFloatFromTableLinkPossible(MyData,LinkTable,dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,SymSizeField,Value) then begin
                if (Value > MaxComboBox1) then Value := MaxComboBox1;
                Radius := round(MDDef.ScaledSymbolMaxSize * sqrt((Value-MinComboBox1)/(MaxComboBox1-MinComboBox1)));
             end
             else Radius := 0;
          end
          else Radius := MDDef.ScaledSymbolMaxSize;

          if (Radius > 0) then begin
             If (MDDef.ScaledSymbolType = gisSymSquare) then Bitmap.Canvas.Rectangle(x-Radius,y-Radius,x+Radius,y+Radius)
             else If (MDDef.ScaledSymbolType = gisSymCircle) then Bitmap.Canvas.Ellipse(x-Radius,y-Radius,x+Radius,y+Radius)
             else If (MDDef.ScaledSymbolType = gisSymTT) then begin
                Bitmap.Canvas.Font.Name := dbOpts.TTSymbolFontName;
                Bitmap.Canvas.Brush.Style := bsClear;
                Bitmap.Canvas.Font.Color := Color;
                Bitmap.Canvas.Font.Size := 4;
                while (Bitmap.Canvas.TextHeight(dbOpts.TTSymbolFontChar) < Radius) do Bitmap.Canvas.Font.Size := Bitmap.Canvas.Font.Size + 1;
                Bitmap.Canvas.TextOut(x-Bitmap.Canvas.TextWidth(dbOpts.TTSymbolFontChar) div 2,
                     y - Bitmap.Canvas.TextHeight(dbOpts.TTSymbolFontChar) div 2, dbOpts.TTSymbolFontChar);
             end;
          end;
          MyData.Next;
       end;
       EndProgress;
   end;
end;



procedure TGISdataBaseModule.GISProportionalSymbols(WhatDraw : tGISWhatDraw;  dbShow : tDBAutoShow = dbasDummy);
var
   FieldsInDB : tStringList;
   i : integer;
//   tGISWhatDraw = (gisPointSymbol,gisConnectPoints,gisColorCode,gisConnectSeqPts,gisPickLabels,gisVectors,gisAnimate,gisTimeSeq,
//        gisMonthlyTemp,gisMonthlyRain,gisMonthlyFilter,gisDefaultDraw);

// tDBAutoShow = (dbasNone,dbasDefault,dbasColorField,dbasIHSField,dbasIconField,dbasIconAll,dbasTTFontSymbol,
//       dbasLayerDefinition,dbasConnectPoints,dbasZValues,dbasSymbolInDB,dbasColorByString,dbasColorByNumeric,
//       dbasTerrainFabric,dbasVector);

begin
   {$IfDef RecordSymProblems}
   WriteLineToDebugFile('GISProportionalSymbols in');
   {$EndIf}
   ChangeDEMNowDoing(JustWandering);
   theMapDraw.DeleteSingleMapLayer(theMapDraw.DBOverlayfName[DataBaseNumber]);

   if (WhatDraw=gisDummy) then dbOpts.dbAutoShow := dbShow;

   if (gis_scaled_form = Nil) then begin
      AddSequentialIndex('REC_NO',false);
      gis_scaled_form := Tgis_scaled_form.Create(Application);
      gis_scaled_form.TheDB := DataBaseNumber;
      gis_scaled_form.CheckBox11.Checked := dbOpts.WindAutoSpace;
      gis_scaled_form.Edit18.Text := IntToStr(dbOpts.WindPixelSpace);
      gis_scaled_form.SetAutoSpacing;
      if dbOpts.VectorsByPolar then gis_scaled_form.RadioGroup6.ItemIndex := 0
      else gis_scaled_form.RadioGroup6.ItemIndex := 1;
   end;

   if (WhatDraw = gisMonthlyTemp) then begin
      gis_scaled_form.Edit20.Text := IntToStr(-20);
      gis_scaled_form.Edit21.Text := IntToStr(30);
   end;
   if (WhatDraw = gisMonthlyRain) then begin
      gis_scaled_form.Edit20.Text := IntToStr(0);
      gis_scaled_form.Edit21.Text := IntToStr(50);
   end;

   gis_scaled_form.Caption := 'Plots for ' + dbName;
   ColorButtonForSymbol(gis_scaled_form.BitBtn7);
   EmpSource.Enabled := false;

   AutoRedrawAllowed := false;
   LayerIsOn := true;
   if (theMapOwner <> Nil) then begin
      if (dbOpts.dbAutoShow in [dbasTTFontSymbol,dbasColorByString,dbasColorByNumeric,dbasVector]) or  (WhatDraw in [gisConnectPoints,gisConnectSeqPts]) then
         theMapOwner.Forceredraw1Click(Nil);
      theMapOwner.BringToFront;
   end;
   GISWhatDraw := WhatDraw;
   Petmar.ColorBitBtn(gis_scaled_form.BitBtn5,MDDef.ScaledSymbolColor);

   FieldsInDB := Nil;
   {$IfDef RecordSymProblems}
   WriteLineToDebugFile('Initial setup done');
   {$EndIf}

   if (dbOpts.dbAutoShow in [dbasTTFontSymbol,dbasColorByString,dbasColorByNumeric,dbasVector]) or (WhatDraw in [gisConnectPoints,gisAnimate]) then begin
      {$IfDef RecordSymProblems}
      WriteLineToDebugFile('First get Fields Present');
      {$EndIf}
      GetFieldsLinkPossible(MyData,LinkTable,dbOpts.VisCols,[ftFloat,ftInteger,ftSmallInt],FieldsInDB);
   end;

   if (FieldsInDB <> Nil) then begin
      gis_scaled_form.ComboBox14.Items := FieldsInDB;
      gis_scaled_form.ComboBox14.Text := dbOpts.RedField;
      gis_scaled_form.ComboBox15.Items := FieldsInDB;
      gis_scaled_form.ComboBox15.Text := dbOpts.GreenField;
      gis_scaled_form.ComboBox16.Items := FieldsInDB;
      gis_scaled_form.ComboBox16.Text := dbOpts.BlueField;
   end;

   if (IconPresent) then begin
      if (NumericFields = 0) then begin
         gis_scaled_form.RadioGroup1.ItemIndex := 3;
         gis_scaled_form.RadioGroup1.Enabled := false;
      end
      else begin
         for i  := 1 to 5 do if (IconFieldNames[i] <> '') then gis_scaled_form.ComboBox13.Items.Add(IconFieldNames[i]);
         gis_scaled_form.ComboBox13.Text := dbOpts.PreferIconFieldName;
      end;
   end
   else begin
      if (gis_scaled_form.RadioGroup1.Items.Count > 3) then gis_scaled_form.RadioGroup1.Items.Delete(3);
   end;

   if (dbOpts.dbAutoShow in [dbasTTFontSymbol]) or (WhatDraw in [gisConnectPoints,gisAnimate]) then begin
      {$IfDef RecordSymProblems}
      WriteLineToDebugFile('[gisPointSymbol,gisConnectPoints,gisAnimate] setup');
      {$EndIf}
      if (FieldsInDB.Count > 0) then begin
         gis_scaled_form.ComboBox1.Items := FieldsInDB;
         gis_scaled_form.ComboBox1.Text := FieldsInDB.Strings[DBFieldNotLatLong(FieldsInDB)];
         gis_scaled_form.ComboBox1Change(Nil);
      end;
      if (WhatDraw = gisConnectPoints) then gis_scaled_form.Caption := DBName + ' point separation';
   end;

   if (WhatDraw in [gisAnimate]) then begin
      {$IfDef RecordSymProblems}
      WriteLineToDebugFile('[gisAnimate] setup');
      {$EndIf}
      if (FieldsInDB <> Nil) then FreeAndNil(FieldsInDB);

      GetFields(MyData,dbOpts.VisCols,[ftInteger,ftSmallInt],FieldsInDB);
      if (FieldsInDB.Count > 0) then begin
         gis_scaled_form.ComboBox1.Items := FieldsInDB;
         gis_scaled_form.ComboBox1.Text := FieldsInDB.Strings[DBFieldNotLatLong(FieldsInDB)];
         gis_scaled_form.ComboBox1Change(nil);
      end;
      GetFields(MyData,dbOpts.VisCols,[ftString,ftInteger,ftSmallInt],FieldsInDB);
      gis_scaled_form.ComboBox10.Items := FieldsInDB;
      gis_scaled_form.ComboBox10.Text := FieldsInDB.Strings[0];
      gis_scaled_form.ComboBox10Change(Nil);
      gis_scaled_form.Caption := DBName + ' Animate';
      gis_scaled_form.CheckBox8.Enabled := MyData.FieldExists('SEQUENCE');
      gis_scaled_form.BitBtn13.Enabled := gis_scaled_form.CheckBox8.Enabled;
      LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont1,gis_scaled_form.FontButton2.Font);
      Petmar.ColorLineWidthBitBtn(gis_scaled_form.BitBtn13,MDDef.ConnectColor,MDDef.ConnectWidth);
      gis_scaled_form.BitBtn7.Enabled := false;
   end;

   if (WhatDraw in [gisPickLabels]) then with gis_scaled_form do begin
      {$IfDef RecordSymProblems}
      WriteLineToDebugFile('[gisPickLabels] setup');
      {$EndIf}
       LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont1,FontButton.Font);
       GetFields(MyData,dbOpts.VisCols,[ftString,ftFloat,ftInteger,ftSmallInt],FieldsInDB);
       ComboBox4.Items := FieldsInDB;
       ComboBox4.Text := dbOpts.LabelField;
       ComboBox5.Items := FieldsInDB;
       ComboBox5.Text := dbOpts.SecondLabelField;
       CheckBox1.Checked := MDDef.AvoidTextOverprints;
       //CheckBox2.Checked := LabelOnRedraw;
       gis_scaled_form.Caption := DBName + ' Label records';
       gis_scaled_form.Lengend.Enabled := false;
       gis_scaled_form.BitBtn7.Enabled := false;
       Edit8.Text := IntToStr(MDDef.MaxLabelDecimals);
   end;

   if (dbOpts.dbAutoShow in [dbasVector]) then with gis_scaled_form do begin
      {$IfDef RecordSymProblems}
      WriteLineToDebugFile('[gisVectors] setup');
      {$EndIf}
       ComboBox6.Items := FieldsInDB;
       ComboBox6.Text := dbOpts.MagField;
       ComboBox7.Items := FieldsInDB;
       ComboBox7.Text := dbOpts.DirField;
       Petmar.ColorLineWidthBitBtn(gis_scaled_form.BitBtn11,GISdb[theDB].dbOpts.BasicColor,GISdb[theDB].dbOpts.VectorWidth);
       Edit5.Text := RealToString(dbOpts.VectorLineMult,-12,-2);
       Edit6.Text := IntToStr(dbOpts.VectorThinFactor);
       CheckBox6.Checked := MDDef.PlotArrowHead;
       gis_scaled_form.Caption :=  DBName + ' Vectors';
       gis_scaled_form.BitBtn7.Enabled := false;
       if MyData.FieldExists('JAN_V_MS') or (MyData.FieldExists('VE')) then begin
          RadioGroup8.Visible := true;
          RadioGroup8.ItemIndex := MDDef.WindMonth;
          RadioGroup6.ItemIndex := 1;
          BitBtn25.Visible := false;
       end
       else begin
          RadioGroup8.Visible := MyData.FieldExists('MONTH');
          RadioGroup6.ItemIndex := 0;
       end;
       Edit19.Text := 'Speed';
   end;

   if (WhatDraw = gisConnectSeqPts) then begin
      {$IfDef RecordSymProblems}
      WriteLineToDebugFile('[gisConnectSeqPts] setup');
      {$EndIf}
      gis_scaled_form.ArrowCheckBox.Checked := MDDef.ConnectArrows;
      gis_scaled_form.Edit4.Text := IntToStr(MDDef.ConnectArrowSpacing);
      Petmar.ColorLineWidthBitBtn(gis_scaled_form.BitBtn9,MDDef.ConnectColor,MDDef.ConnectWidth);
   end;

   if (dbOpts.dbAutoShow in [dbasColorByString,dbasColorByNumeric]) then begin
      {$IfDef RecordSymProblems}
      WriteLineToDebugFile('[gisColorCode] setup');
      {$EndIf}
      gis_scaled_form.Caption :=  DBName + ' Color code';
   end;


   if (FieldsInDB <> Nil) then FieldsInDB.Free;

   {$IfDef RecordSymProblems}
   WriteLineToDebugFile('inital setup over');
   {$EndIf}

   gis_scaled_form.TrackBar1.Position := dbOpts.Opacity;
   gis_scaled_form.Edit3.Visible := LineShapeFile(ShapeFileType);
   gis_scaled_form.Label6.Visible := gis_scaled_form.Edit3.Visible;
   gis_scaled_form.Edit1.Text := IntToStr(MDDef.ScaledSymbolMaxSize);
   gis_scaled_form.RadioGroup1.ItemIndex := ord(MDDef.ScaledSymbolType);
   gis_scaled_form.RadioGroup2.ItemIndex := ord(dbOpts.GISColorSource);
   gis_scaled_form.RadioGroup4.Enabled := (DBColorScheme = LegChloropleth);
   gis_scaled_form.CheckBox9.Enabled:= AreaShapeFile(ShapeFileType);
   gis_scaled_form.BitBtn17.Enabled := AreaShapeFile(ShapeFileType);

   with gis_scaled_form do ColorLineWidthBitBtn(BitBtn17,MDDef.GISOutlineColor,MDDef.GISOutlineWidth);

   {$IfDef RecordSymProblems}
   WriteLineToDebugFile('intermediate setup over');
   {$EndIf}

   gis_scaled_form.SetPanels;
   gis_scaled_form.FillInLegendPanel;

   gis_scaled_form.BitBtn3.Enabled := gis_scaled_form.RadioGroup1.ItemIndex = 2;
   gis_scaled_form.DrawSymbol;
   gis_scaled_form.Show;
   if (dbOpts.dbAutoShow in [dbasVector]) then gis_scaled_form.PlotScaledSymbolsButtonClick(Nil);
   EmpSource.Enabled := true;
   {$IfDef RecordSymProblems}
   WriteLineToDebugFile('final setup over');
   {$EndIf}
end;


procedure TGISdataBaseModule.DisplayTheRecord(j : integer; ShowItModal : boolean = true; CanEdit : boolean = false);
begin
   DEMShowDBRecord.FullDBName := dbFullName;
   DisplaySingleDataBaseRecord(dbTablef,LinkTable,dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,CanEdit,dbOpts.VisCols,dbName,j,ShowItModal,-999,1,dbOpts.LabelField);
end;


procedure ConvertShapeFileToKML(fName : PathStr; MapOwner : tMapForm);
var
   DBonTable : integer;
begin
   if FileExists(fName) then begin
      OpenNumberedGISDataBase(DBonTable,fName,false,MapOwner);
      KML_opts.ConvertToKML(DBonTable,'');
      CloseAndNilNumberedDB(DBonTable);
   end;
end;

procedure PlotLineShapeFileOnMap(fName : PathStr; MapDraw : tMapDraw; var Bitmap : tMyBitmap; LineColor : tColor; LineWidth : byte);
var
   sf : tShapeFile;
   Success : boolean;
begin
   {$IfDef RecordDBPlot}
   WritelineToDebugFile('PlotLineShapeFileOnMap ' + fName);
   {$EndIf}
   if FileExists(fName) then begin
     try
       sf := tShapeFile.Create(fName,Success);
       if Success then begin
          Bitmap.Canvas.Pen.Color := LineColor;
          Bitmap.Canvas.Pen.Width := LineWidth;
          Bitmap.Canvas.Brush.Style := bsClear;
          sf.PlotAllRecords(MapDraw,Bitmap);
       end
       else begin
          {$IfDef RecordDBPlot}
          WritelineToDebugFile('Failed to open shp');
          {$EndIf}
       end;
     finally
        sf.Destroy;
     end;
   end;
end;

procedure PlotTIGERShapeFileOnMap(fName : PathStr; var Bitmap : tMyBitmap; MapDraw : tMapDraw; RoadsOnly : boolean = false; MaskSize : integer = -1; StreamsOnly : boolean = false);
var
   GISNum : integer;
begin
   if (MapDraw <> Nil) then begin
      {$IfDef RecordTiger}
      WriteLineToDebugFile('PlotTIGERShapeFileOnMap ' + fName);
      {$EndIf}
      OpenNumberedGISDataBase(GISNum,fName);
      GISdb[GISNum].theMapDraw := MapDraw;
      GISdb[GISNum].PlotTigerShapeFileOnMap(Bitmap,RoadsOnly,MaskSize,StreamsOnly);
      if MDDef.MakeTigerMapGrayscale then MakeBitmapGrayScale(Bitmap);
      if MDDef.SubdueTigerBase then MakeBitmapSubdued(Bitmap);
      CloseAndNilNumberedDB(GISNum);
   end;
end;


procedure AddDatabaseToMask(var Bitmap : tMyBitmap; MapOwner : tMapForm; DBonTable : integer; UseShapeFile,MaskShapesAreIn : boolean; MaskingDistance : float; Color : tColor);
var
   oldDBSaveOptions : tDBSaveOptions;
   x,y,radius : integer;
   c : tColor;
   GazColors : tMyData;
   NameStr : string12;
   MenuStr : shortString;

     procedure GetLineSize(MaskDistance : float);
     begin
        with GISdb[DBonTable],MyData,dbOpts do begin
           LineSize := round(MaskDistance * 2 / MapOwner.MapDraw.ScreenPixelSize);
           if ItsAPointDB then LineSize := LineSize div 2;
           if (LineSize < 1) then LineSize := 1;
        end;
     end;

     procedure PointMask(MaskingDistance : float);
     begin
        with GISdb[DBonTable],MyData,dbOpts do begin
           GetLineSize(MaskingDistance);
           First;
           While not eof do begin
              MapOwner.MapDraw.LatLongDegreeToScreen(GetFieldByNameAsFloat('LAT'),GetFieldByNameAsFloat('LONG'),x,y);
              Bitmap.Canvas.Ellipse(x-LineSize,y-LineSize,x+LineSize,y+LineSize);
              Next;
           end;
        end;
     end;


begin
   with GISdb[DBonTable],MyData,dbOpts do begin
     oldDBSaveOptions := dbOpts;

     DEMDef_routines.SaveBackupDefaults;
     SetOptionsForMapWithNoMarginalia;
     //MapOwner.DoFastMapRedraw;

     c := Color;
     BasicColor := C;
     FillColor := C;

     //CloneImageToBitmap(MapOwner.Image1,Bitmap);
     EmpSource.Enabled := false;
     if SimplePointFile then begin
        Bitmap.Canvas.Brush.Style := bsSolid;
        Bitmap.Canvas.Brush.Color := c;
        Bitmap.Canvas.Pen.Color := c;
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
              MyData.ApplyFilter(GetFieldByNameAsString('FILTER'));
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
        {$IfDef RecordMaskDEMShapeFile}
        WriteLineToDebugFile('MaskDEMfromshapefile1Click not point shape file');
        {$EndIf}
        GetLineSize(MaskingDistance);
        Bitmap.Canvas.Brush.Color := c;
        Bitmap.Canvas.Brush.Style := bsSolid;
        Bitmap.Canvas.Pen.Color := c;
        Bitmap.Canvas.Pen.Width := LineSize;
        aShapeFile.PlotAllRecords(TheMapDraw,Bitmap);
     end;
     EmpSource.Enabled := true;
     dbOpts := oldDBSaveOptions;
     DEMDef_routines.RestoreBackupDefaults;
   end;
end;


procedure MaskDEMFromShapeFile(DEM,DBonTable : integer; UseShapeFile,MaskShapesAreIn : boolean; SingleRecord  : integer; MaskingDistance : float; ProgressCount : shortstring = '');
var
   oldDBSaveOptions : tDBSaveOptions;
   x,y,Col,Row,radius,MaskSize : integer;
   xg,yg  : float;
   MapBMP,MaskBMP : tMyBitmap;
   p1 : pRGB;
   Color : Windows.TRGBTriple;
   c : tColor;
   GazColors : tMyData;
   NameStr : string12;
   MenuStr : shortString;


     procedure GetLineSize(MaskDistance : float);
     begin
        with GISdb[DBonTable],MyData,dbOpts do begin
           MaskSize := round(MaskDistance * 2 / DEMGlb[TheMapDraw.DEMonMap].SelectionMap.MapDraw.ScreenPixelSize);
           if ItsAPointDB then MaskSize := MaskSize div 2;
           if (MaskSize < 1) then MaskSize := 1;
        end;
     end;

     procedure PointMask(MaskingDistance : float);
     begin
        with GISdb[DBonTable],MyData,dbOpts do begin
           GetLineSize(MaskingDistance);
           First;
           While not eof do begin
              DEMGlb[DEM].SelectionMap.MapDraw.LatLongDegreeToScreen(GetFieldByNameAsFloat('LAT'),GetFieldByNameAsFloat('LONG'),x,y);
              MaskBMP.Canvas.Ellipse(x-MaskSize,y-MaskSize,x+MaskSize,y+MaskSize);
              Next;
           end;
        end;
     end;


begin
   with GISdb[DBonTable],MyData,dbOpts do begin
     oldDBSaveOptions := dbOpts;

     DEMDef_routines.SaveBackupDefaults;
     SetOptionsForMapWithNoMarginalia;
     DEMGlb[DEM].SelectionMap.DoFastMapRedraw;

     c := clBlack;
     BasicColor := C;
     FillColor := C;

     CopyImageToBitmap(DEMGlb[DEM].SelectionMap.Image1,MapBMP);
     CloneImageToBitmap(DEMGlb[DEM].SelectionMap.Image1,MaskBMP);

     {$IfDef RecordMaskDEMShapeFile}
     PetImage.SaveBitmap(MaskBMP,MDTempDir + 'cloned' + OverlayFExt);
     {$EndIf}
     EmpSource.Enabled := false;

     if SimplePointFile then begin
        MaskBMP.Canvas.Brush.Style := bsSolid;
        MaskBMP.Canvas.Brush.Color := clBlack;
        MaskBMP.Canvas.Pen.Color := clBlack;
        MaskBMP.Canvas.Pen.Width := 1;
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
              MyData.ApplyFilter(GazColors.GetFieldByNameAsString('FILTER'));
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
        {$IfDef RecordMaskDEMShapeFile}
        WriteLineToDebugFile('MaskDEMfromshapefile1Click not point shape file');
        {$EndIf}
        GetLineSize(MaskingDistance);
        MaskBMP.Canvas.Pen.Color := clBlack;
        MaskBMP.Canvas.Pen.Width := MaskSize;
        MaskBMP.Canvas.Brush.Color := clBlack;
        MaskBMP.Canvas.Brush.Style := bsSolid;
        if (SingleRecord <> 0) then begin
          {$IfDef RecordCurrentRecord}
          WriteLineToDebugFile('MaskDEMFromShapeFile, RecNo=' + IntToStr(SingleRecord));
          {$EndIf}
           aShapefile.PlotSingleRecord(theMapDraw,MaskBMP,SingleRecord);
        end
        else begin
           AreaSymbolFill := bsSolid;
           LineSize := MaskSize;
           BasicColor := clBlack;
           DisplayOnMap(MaskBMP);
        end;
     end;

     EmpSource.Enabled := true;

     {$IfDef RecordMaskDEMShapeFile}
     PetImage.SaveBitmap(MaskBMP,MDTempDir + 'Mask_dbdisplayed.bmp');
     {$EndIf}

     if MDDef.ShowMasks then Petimage_form.DisplayBitmap(MaskBMP,'Mask from ' + dbName);

     StartProgress('Mask ' + ProgressCount);
     if (TheMapDraw.MapZoomFactor < 0.99) then begin
        for Col := 0 to DEMGlb[DEM].HeadRecs.NumCol do begin
           if (col mod 250 = 0) then UpDateProgressBar(Col/DEMGlb[DEM].HeadRecs.NumCol);
           for Row := 0 to DEMGlb[DEM].HeadRecs.NumRow do begin
              TheMapDraw.DEMGridToScreen(Col,Row,x,y);
              if TheMapDraw.OnScreen(x,y) then begin
                p1 := MaskBMP.ScanLine[y];
                if (SameColor(p1[x],RGBTripleBlack)) then begin
                   if (not MaskShapesAreIn) then DEMGlb[DEM].SetGridMissing(Col,Row);
                end
                else begin
                   if (MaskShapesAreIn) then DEMGlb[DEM].SetGridMissing(Col,Row);
                end;
              end
              else DEMGlb[DEM].SetGridMissing(Col,Row);
           end;
        end;
     end
     else begin
         if MaskShapesAreIn then Color := RGBTripleWhite
         else Color := RGBTripleBlack;
         for y := 0 to pred(MaskBMP.Height) do begin
            p1 := MaskBMP.ScanLine[y];
            if (y mod 25 = 0) then UpDateProgressBar(y/MaskBMP.Height);
            for x := 0 to pred(MaskBMP.Width) do begin
               if SameColor(p1[x],Color) then begin
                  TheMapDraw.ScreenToDEMGrid(x,y,xg,yg);
                  DEMGlb[DEM].SetGridMissing(xg,yg);
               end;
            end;
         end;
      end;
      EndProgress;

      {$IfDef RecordMaskDEMShapeFile}
      PetImage.SaveBitmap(MaskBMP,MDTempDir + 'final_Mask.bmp');
      {$EndIf}

      FreeAndNil(MaskBMP);
      FreeAndNil(MapBMP);
      dbOpts := oldDBSaveOptions;
      DEMDef_routines.RestoreBackupDefaults;
      DEMGlb[DEM].CheckMaxMinElev;
      DEMGlb[DEM].DEMstatus := dsUnsaved;
      DEMGlb[DEM].SelectionMap.DoBaseMapRedraw;
   end;
   {$IfDef RecordMaskDEMShapeFile}
   WriteLineToDebugFile('MaskDEMfromshapefile1Click out');
   {$EndIf}
end;



procedure OpenDataBase(WhatFor : ANSIstring; fName : Pathstr = ''; ShowTable : boolean = true);
var
   FilesWanted : tStringList;
   i : integer;
begin
   {$IfDef RecordOpenDataBaseProblems}
   WriteLineToDebugFile('OpenADataBase,fName=' + fName);
   {$EndIf}
   if (fName = '')  then begin
      FilesWanted := tStringList.Create;
      FilesWanted.Add(ExtractFilePath(LastDataBase));
      if GetMultipleFiles('DB for ' + WhatFor,DBMaskString,FilesWanted,MDdef.DefDBFilter) then begin
         for I := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[i];
            LastDBLoaded := 0;
            OpenNumberedGISDataBase(LastDBLoaded,fName,ShowTable);
         end;
         LastDataBase := FilesWanted.Strings[0];
         FilesWanted.Free;
      end;
   end
   else begin
      LastDBLoaded := 0;
      OpenNumberedGISDataBase(LastDBLoaded,fName,ShowTable);
   end;
end;



function CopyDatabaseAndOpen(GIS : TGISdataBaseModule; var NewGIS : TGISdataBaseModule; OpenLink : boolean = true) : boolean;
var
   fName : PathStr;

   procedure CopyOneFile(fName : PathStr);
   begin
      if FileExists(fName) then begin
         CopyFile(fName,MDTempDir + ExtractFileName(fName));
         //Petmar.DeleteFilesOnClosing.Add(MDTempDir + ExtractFileName(fName));
      end;
   end;

begin
   {$IfDef ExSaveDBstatus}
   {$Else}
   GIS.SaveDataBaseStatus;
   {$EndIf}

   fName := GIS.dbFullName;
   CopyOneFile(ChangeFileExt(FName,'.shp'));
   CopyOneFile(ChangeFileExt(FName,'.shx'));
   CopyOneFile(ChangeFileExt(FName,DefaultDBExt));
   if OpenLink and (GIS.LinkTable <> nil) then CopyOneFile(GIS.LinkTable.TableName);

   NewGIS := Nil;
   Result := OpenGISDataBase(0,'',NewGIS,MDTempDir + ExtractFileName(fName));
   if OpenLink and (GIS.LinkTable <> nil) then begin
      fName := MDTempDir + ExtractFileName(GIS.LinkTable.TableName);
      NewGIS.LinkSecondaryTable(fName);
   end;
end;


function TotalNumOpenDatabase : integer;
var
   i : integer;
begin
   Result := 0;
   for i := 1 to MaxDataBase do if (GISdb[i] <> Nil) then inc(Result);
end;


function NumOpenDatabaseThisMap(MapOwner : tMapForm) : integer;
var
   i : integer;
begin
   {$IfDef RecordCloseDataBaseProblems}
   WritelineToDebugFile('NumOpenDatabaseThisMap in ' + MapOwner.Caption, true);
   {$EndIf}
   Result := 0;
   for i := 1 to MaxDataBase do begin
      if (GISdb[i] <> Nil) then begin
         {$IfDef RecordCloseDataBaseProblems}
         WritelineToDebugFile('Open GIS '+ IntToStr(i));
         {$EndIf}
         if (GISdb[i].dbName <> '') then begin
            {$IfDef RecordCloseDataBaseProblems}
            WritelineToDebugFile('     ' + GISdb[i].dbName);
            {$EndIf}

            if (GISdb[i].theMapOwner <> Nil) then begin
              if (GISdb[i].theMapOwner = MapOwner) then begin
                 inc(Result);
              end;
           end;
         end;
      end;
   end;
end;




function PickOpenGISDataBase(WhatFor : shortString; Ignore : integer = 0; Geometry : integer = 0; FieldNeeded : string12 = '') : integer;
var
   i      : integer;
   OpenDB : tStringList;

   function GoodGeometry(I : integer) : boolean;
   begin
      with GISdb[i] do begin
         if (Geometry = 0) then Result := true
         else if (Geometry = 1) then Result := ItsApointDB
         else if (Geometry = 3) then Result := LineShapeFile(ShapeFileType)
         else if (Geometry = 5) then Result := AreaShapeFile(ShapeFileType);
      end;
   end;

begin
   Result := 0;
   OpenDB := tStringList.Create;
   for i := 1 to MaxDataBase do begin
      if (I <> Ignore) and (GISdb[i] <> Nil) then begin
         if GoodGeometry(i) or ((FieldNeeded = '') or GISdb[i].MyData.FieldExists(FieldNeeded)) then
            OpenDB.Add(IntToStr(i) + '  ' + GISdb[i].dbName);
      end;
   end;
   if (OpenDB.Count > 0) and GetFromListZeroBased(WhatFor,Result,OpenDB,true) then
      Val(ptTrim(Copy(OpenDB.Strings[Result],1,2)),Result,i)
   else Result := -99;
   OpenDB.Free;
end;



function TGISdataBaseModule.GetFieldNameForDB(Prompt : ShortString; AllowOverWrite : boolean; SuggestedName: string12 = ''): string12;
var
   ItExists : boolean;
begin
   repeat
      GetString('New Field',SuggestedName,true,DBaseFieldNameChars);
      while length(SuggestedName) > 11 do Delete(SuggestedName,Length(SuggestedName),1);

      ItExists := MyData.FieldExists(SuggestedName);
      if AllowOverwrite and ItExists then begin
         ItExists := not AnswerIsYes('Field ' + SuggestedName + ' exists; Overwrite');
      end;
   until not ItExists;
   Result := SuggestedName;
end;

procedure TGISdataBaseModule.ChangeLatLongLocation(Lat,Long : float);
var
   z : float;
   fName : PathStr;
begin
   with MyData do begin
      SetFieldByNameAsFloat(LatFieldName, Lat);
      SetFieldByNameAsFloat(LongFieldName, Long);
      if MyData.FieldExists('MGR/S') then begin
         RedefineWGS84DatumConstants(Long);
         MyData.SetFieldByNameAsString('MGRS',LatLongToMGRS(Lat,Long,WGS84DatumConstants));
      end;
      if MyData.FieldExists('Z') and (theMapDraw.DEMonMap <> 0) then begin
         if DEMGlb[theMapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then
            SetFieldByNameAsFloat('Z',z);
      end;

      if ItsFanFile then begin
         if MyData.FieldExists('IMAGE') then begin
            fName := GetFieldByNameAsString('IMAGE');
            DeleteFileIfExists(fName);
            MyData.SetFieldByNameAsString('IMAGE','');
            DEMGlb[1].SelectionMap.MapDraw.DeleteSingleMapLayer(DEMGlb[1].SelectionMap.MapDraw.AllFansCoverageFName);
         end;
         if MyData.FieldExists('COVERS') then MyData.SetFieldByNameAsString('COVERS', '');
         if MyData.FieldExists('UNIQUE_COV') then MyData.SetFieldByNameAsString('UNIQUE_COV', '');
         if MyData.FieldExists('SEE_TARGET') then MyData.SetFieldByNameAsString('SEE_TARGET', '');
      end;
   end;
end;


function TGISdataBaseModule.GetMaskFieldName : string10;
var
   Pickins : tStringList;
   j : integer;
begin
   Result := 'MASK';
   if MyData.FieldExists(Result) then begin
      Pickins := tStringList.Create;
      Pickins.Add(Result);
      for j := 2 to 5 do begin
         Result := 'MASK' + IntToStr(j);
         if MyData.FieldExists(Result) then Pickins.Add(Result);
      end;
      j := 0;
      Petmar.GetFromListZeroBased('Mask field to use',j,Pickins);
      Result := Pickins[j];
      Pickins.Free;
      EmpSource.Enabled := false;
      MyData.ApplyFilter('');
      MyData.First;
      while not MyData.EOF do begin
          MyData.Edit;
          MyData.SetFieldByNameAsString(Result, '0');
          MyData.Next;
      end;
      EmpSource.Enabled := true;
   end
   else begin
      AddFieldToDataBase(ftString,'MASK',1,0);
   end;
end;


function TGISdataBaseModule.ValidScreenPositionFromTable(var x,y : integer) : boolean;
var
   Lat,Long,xgrid,ygrid : float;
begin
   Result := false;
   if ValidLatLongFromTable(Lat,Long) then begin
      if XYZfile then begin
         if (TheGraphOwner <> Nil) then begin
            x := TheGraphOwner.GraphDraw.GraphX(Long);
            y := TheGraphOwner.GraphDraw.GraphY(Lat);
            Result := TheGraphOwner.GraphDraw.OnGraph(x,y);
            exit;
         end;
         {$IfDef ExSidescan}
         {$Else}
         if (theMapDraw.ValidSatOnMap) then begin
            SatImage[theMapDraw.SATonMap].UTMtoSatelliteGrid(Long,Lat,xgrid,ygrid);
            theMapDraw.GridToScreen(xgrid,ygrid,x,y);
         end;
         {$EndIf}
      end
      else begin
         if theMapDraw.CurrentMapDatum.h_DatumCode <> 'WGS84' then
            MolodenskiyTransformation(Lat,Long,Lat,Long,WGS84DatumConstants,theMapDraw.CurrentMapDatum);
         theMapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
      end;
      Result := theMapDraw.OnScreen(x,y);
   end;
   {$IfDef RecordValidScreenPosition}
   if Result then WriteLineToDebugFile('ValidScreenPositionFromTable')
   else WriteLineToDebugFile('No ValidScreenPositionFromTable');
   {$EndIf}
end;


function TGISdataBaseModule.FindAreaRecordWithPoint(Lat,Long : float; AlreadyFiltered : boolean = true) : boolean;
var
   theMainFilter : string;
   i : integer;
begin
   {$IfDef RecordPointInAreaProblems}
   WriteLineToDebugFile('FindAreaRecordWithPoint in,  ' + DBName + ' ' + LatLongDegreeToString(Lat,Long,DecDegrees),true);
   {$EndIf}
   Result := false;
   if AreaShapeFile(ShapeFileType) or (LatLongCornersPresent) then begin
      if not AlreadyFiltered then begin
         if (MainFilter = '') then theMainFilter := '' else theMainFilter := MainFilter + ' AND ';
         MyData.ApplyFilter(theMainFilter + PointInBoxGeoFilter(Lat,Long));
      end;
      {$IfDef RecordPointInAreaProblems}
      WriteLineToDebugFile('FindAreaRecordWithPoint Rec filter=' + MyData.Filter);
      WriteLineToDebugFile('Found recs=' + IntToStr(MyData.RecordCount));
      {$EndIf}
      if (MyData.RecordCount > 0) then begin
         if (MyData.RecordCount = 1) or (TheMapOwner = Nil) then Result := true
         else begin
            if AreaShapeFile(ShapeFileType) then begin
               {$IfDef RecordPointInAreaProblems}
               WriteLineToDebugFile('Records found: ' + IntToStr(MyData.RecordCount));
               MyData.First;
               for i := 1 to MyData.RecordCount do begin
                  if aShapeFile.PointInRecord(theMapDraw,MyData.RecNo,Lat,Long) then
                     WriteLineToDebugFile('Rec found in box: ' + IntToStr(MyData.RecNo))
                  else WriteLineToDebugFile('Rec not in box: ' + IntToStr(MyData.RecNo));
                  MyData.Next;
               end;
               {$EndIf}

               MyData.First;
               for i := 1 to MyData.RecordCount do begin
                  if aShapeFile.PointInRecord({theMapDraw,}MyData.RecNo,Lat,Long) then begin
                     Result := true;
                     {$IfDef RecordPointInAreaProblems}
                     WriteLineToDebugFile('FindAreaRecordWithPoint out  ' + IntToStr(i));
                     {$EndIf}
                     exit;
                  end;
                  MyData.Next;
               end;
            end
            else begin
               {$IfDef RecordPointInAreaProblems}
               WriteLineToDebugFile('Not an area shape file');
               {$EndIf}
            end;
         end;
      end;
   end;
   {$IfDef RecordPointInAreaProblems}
   WriteLineToDebugFile('FindAreaRecordWithPoint out (drop through)');
   {$EndIf}
end;



procedure LoadShapeFileGroup(Map : tMapForm; fName : PathStr = '');
var
   TheIndex : tMyData;
   i,dbnum : integer;
   fName2 : PathStr;
begin
    if (FName = '') then begin
       fName := dbDirectory + 'groups\';
       if not Petmar.GetFileFromDirectory('shape file grouping',DefaultDBMask,fName) then exit;
    end;
    if FileExists(fName) then begin
       TheIndex := tMyData.Create(fName);
       for i := 0 to 9 do begin
           TheIndex.ApplyFilter('ORDER = ' + IntToStr(i) + ' AND PLOT =' + QuotedStr('Y'));
           while not TheIndex.EOF do begin
              dbNum := 0;
              fName2 := TheIndex.GetFieldByNameAsString('FILENAME');
              if FileExists(fName2) then begin
                 OpenNumberedGISDataBase(dbNum,fName2,true,Map);
                 GISdb[dbNum].SetColorsFromDB(TheIndex);
                 if TheIndex.GetFieldByNameAsString('PLOT') = 'Y' then GISdb[dbNum].dbOpts.DBAutoShow := dbasDefault
                 else GISdb[dbNum].dbOpts.DBAutoShow := dbasNone;
              end;
              TheIndex.Next;
           end;
       end;
       TheIndex.Destroy;
       Map.MapDraw.MapOverlays.ovShapeFileGroup := fName;
       Map_Overlays.AddOrSubtractOverlay(Map,ovoDataBases,True);
       Map.DoFastMapRedraw;
    end;
end;


procedure PlotShapeFileGroupOnMap(Map : tMapDraw; Bitmap : tMyBitmap; inTheData : tMyData; GISdb : TGISdataBaseModule);
var
   x,y,i : integer;
   Label_Field : string10;
   fName2 : PathStr;
   LabelIt,
   TryLabels,
   TheDataOpen,
   OK,PlotIt : boolean;
begin
    {$IfDef RecordShapeFileGroup}
    WriteLineToDebugFileWithTime('PlotShapeFileGroupOnMap in',true);
    {$EndIf}
    ShowHourglassCursor;
    TryLabels := inTheData.FieldExists('LABEL');
    TheDataOpen := (GisDB <> Nil);
    if TheDataOpen then GISdb.EmpSource.Enabled := false;

    for i := 0 to 9 do begin
        inTheData.ApplyFilter('PLOT_ORDER = ' + IntToStr(i) + ' AND PLOT =' + QuotedStr('Y'));
        {$IfDef RecordShapeFileGroup}
        WriteLineToDebugFile(inTheData.Filter, true);
        WriteLineToDebugFile('  recs=' + IntToStr(inTheData.RecordCount));
        {$EndIf}
        if (inTheData.RecordCount > 0) then begin
           while not inTheData.EOF do begin
              if (not TheDataOpen) then fName2 := inTheData.GetFieldByNameAsString('FILENAME');
              if TheDataOpen or FileExists(fName2) then begin
                 {$IfDef RecordShapeFileGroup}
                 WriteLineToDebugFile('Plot: ' + fName2);
                 {$EndIf}
                 LabelIt := TryLabels and (inTheData.GetFieldByNameAsString('LABEL') = 'Y');
                 if LabelIt then begin
                    inTheData.DefineFontFromTable(Bitmap.Canvas.Font);
                    Bitmap.Canvas.Brush.Style := bsClear;
                    Label_Field := inTheData.GetFieldByNameAsString('LAB_FIELD');
                 end;

                 if TheDataOpen then begin
                    OK := true;
                 end
                 else begin
                    {$IfDef RecordShapeFileGroup}
                    WriteLineToDebugFile('Opening GIS module for ' + fName2);
                    {$EndIf}
                    GISdb := TGISdataBaseModule.Create(Application);
                    OK := GISdb.InitializeTheTable('',fName2);
                    GISDB.TheMapDraw := Map;
                 end;

                 if OK then begin
                     if inTheData.FieldExists('FILTER') and (inTheData.GetFieldByNameAsString('FILTER') <> '') then begin
                        GISDB.MyData.ApplyFilter(inTheData.GetFieldByNameAsString('FILTER'));
                        {$IfDef RecordShapeFileGroup}
                        WriteLineToDebugFile('Filter: ' + GISDB.MyData.Filter);
                        {$EndIf}
                     end;
                     if inTheData.FieldExists('PIXEL_SIZE') and (inTheData.GetFieldByNameAsString('PIXEL_SIZE') <> '') then begin
                        PlotIt := (Map.ScreenPixelSize < inTheData.GetFieldByNameAsInteger('PIXEL_SIZE'));
                     end
                     else PlotIt := true;

                     if PlotIt and (GISDB.MyData.RecordCount > 0) then begin
                       {$IfDef RecordShapeFileGroup}
                       WriteLineToDebugFileWithTime('Plot file');
                       {$EndIf}
                        GisDB.SetColorsFromDB(inTheData);
                        GisDB.PrepColors(Bitmap);
                        ShowHourglassCursor;
                        if (GISdb.aShapeFile <> Nil) then begin
                          {$IfDef RecordShapeFileGroup}
                          WriteLineToDebugFileWithTime('Use GISdb.aShapeFile.PlotAllRecords');
                          {$EndIf}
                           GISdb.aShapeFile.PlotAllRecords(Map,Bitmap);
                        end
                        else begin
                           {$IfDef RecordShapeFileGroup}
                           WriteLineToDebugFileWithTime('Use GisDB.DisplayCurrentRecordOnMap');
                          {$EndIf}
                             GisDB.MyData.First;
                             while not GISdb.MyData.EOF do begin
                                GisDB.DisplayCurrentRecordOnMap(Bitmap);
                                GisDB.MyData.Next;
                             end;
                        end;
                        if LabelIt then begin
                          GisDB.MyData.First;
                          while not GISdb.MyData.EOF do begin
                             if GisDB.ValidScreenPositionFromTable(x,y) then begin
                                Bitmap.Canvas.TextOut(x+5,y+5,GISdb.MyData.GetFieldByNameAsString(Label_Field));
                             end;
                             GisDB.MyData.Next;
                          end;
                        end;
                     end
                     else begin
                        {$IfDef RecordShapeFileGroup}
                        WriteLineToDebugFile('File does not plot at this scale');
                        {$EndIf}
                     end;
                 end
                 else begin
                    {$IfDef RecordShapeFileGroup}
                    WriteLineToDebugFile('File was not OK');
                    {$EndIf}
                 end;
                 if not TheDataOpen then begin
                    if GISdb.CloseDataBase(false) then GISdb.Free;
                 end;
              end
              else begin
                 {$IfDef RecordShapeFileGroup}
                 WriteLineToDebugFile(fName2 + ' does not exist');
                 {$EndIf}
              end;
              inTheData.Next;
              if CeaseThreads or WantOut then break;
           end;
        end;
        if CeaseThreads or WantOut then break;
    end;
    ShowDefaultCursor;
    if TheDataOpen then GISdb.EmpSource.Enabled := true;
end;


procedure PlotShapeFileGroup(Map : tMapDraw; inBitmap : tMyBitmap; FName : PathStr);
var
   TheIndex : tMyData;
   Bitmap : tMyBitmap;
begin
   {$IfDef RecordShapeFileGroup}
   WriteLineToDebugFile('PlotShapeFileGroup in ' + fName,true);
   {$EndIf}

    Bitmap := Nil;
    if (Map.CartoDBfName <> '') then begin
       {$IfDef RecordShapeFileGroup}
       WriteLineToDebugFile('Reading ' + Map.CartoGroupShapesUp);
       {$EndIf}
       Bitmap := PetImage.LoadBitmapFromFile(Map.CartoDBfName);
    end
    else if (Fname <> '') and FileExists(fName) then begin
       CreateBitmap(Bitmap,inBitmap.Width,inBitmap.Height);
       TheIndex := TMyData.Create(fName);
       PlotShapeFileGroupOnMap(Map,Bitmap,TheIndex,Nil);
       TheIndex.Destroy;
       Map.CartoDBfName := NextFileNumber(MDTempDir, 'Carto_overlay', OverlayFExt);
       SaveBitmap(Bitmap,Map.CartoDBfName);
       {$IfDef RecordShapeFileGroup}
       WriteLineToDebugFile('Created and saved: ' + Map.CartoGroupShapesUp);
       {$EndIf}
    end;
    if (Bitmap <> Nil) then DrawAndDeleteOverlay(inBitmap,Bitmap);
   {$IfDef RecordShapeFileGroup}
   WriteLineToDebugFile('PlotShapeFileGroup out');
   {$EndIf}
end;


procedure LegendFromShapeFileGroup(Map : tMapDraw; FName : PathStr);
var
   TheIndex : tMyData;
   GISdb : TGISdataBaseModule;
   x,MaxX, i,OnY : integer;
   PlotIt : boolean;
   TStr : ShortString;
   Bitmap : tMyBitmap;
begin
    TheIndex := tMyData.Create(fName);
    OnY := 15;
    MaxX := 0;
    LoadMyFontIntoWindowsFont(MDDef.DefGisLabelFont1,Bitmap.Canvas.Font);
    for i := 9 downto 0 do begin
        TheIndex.ApplyFilter('ORDER = ' + IntToStr(i) + ' AND PLOT =' + QuotedStr('Y'));
        TheIndex.First;
        while not TheIndex.EOF do begin
           if (Map <> Nil) and TheIndex.FieldExists('PIXEL_SIZE') and (TheIndex.GetFieldByNameAsString('PIXEL_SIZE') <> '') then begin
              PlotIt := (Map.ScreenPixelSize < TheIndex.GetFieldByNameAsInteger('PIXEL_SIZE'));
           end
           else PlotIt := true;

           if PlotIt then begin
              GISdb := TGISdataBaseModule.Create(Application);
              if GISdb.InitializeTheTable('',TheIndex.GetFieldByNameAsString('FILENAME')) then with GISdb,MyData,dbOpts do begin
                  if TheIndex.FieldExists('FILTER') and (TheIndex.GetFieldByNameAsString('FILTER') <> '') then begin
                     GISDB.MyData.ApplyFilter(TheIndex.GetFieldByNameAsString('FILTER'));
                  end;
                  if (GISdb.MyData.RecordCount > 0) then begin
                     GISdb.SetColorsFromDB(TheIndex);
                     PrepColors(Bitmap);
                     case TheIndex.GetFieldByNameAsInteger('SHAPE_TYPE') of
                        1 : Petmar.ScreenSymbol(Bitmap.Canvas,MDDef.LegendGraphWidth div 2,OnY+ MDDef.LegendSingleHeight div 2,Symbol,SymbolSize,BasicColor);
                        3 : begin
                                Bitmap.Canvas.MoveTo(5,OnY + MDDef.LegendSingleHeight div 2);
                                Bitmap.Canvas.LineTo(MDDef.LegendGraphWidth-5,OnY + MDDef.LegendSingleHeight div 2);
                            end;
                        5 : Bitmap.Canvas.Rectangle(5,OnY+2,MDDef.LegendGraphWidth-5,OnY + MDDef.LegendSingleHeight-2);
                     end;
                     Bitmap.Canvas.Brush.Style := bsClear;
                     TStr := TheIndex.GetFieldByNameAsString('CAPTION');
                     Bitmap.Canvas.TextOut(MDDef.LegendGraphWidth,OnY+2,TStr);
                     x := MDDef.LegendGraphWidth + Bitmap.Canvas.TextWidth(TStr);
                     if x > MaxX then MaxX := x;

                     inc(onY,MDDef.LegendSingleHeight);
                  end;
              end;
              if GISdb.CloseDataBase(false) then GISdb.Free;
           end;
           TheIndex.Next;
        end;
    end;
    TheIndex.Destroy;
    TheIndex.Free;
    Bitmap.Height := onY + 5;
    Bitmap.Width := MaxX + 5;
    DisplayBitmap(Bitmap,'Legend');
    Bitmap.Free;
end;




function OpenGazetteer(var i : integer; fName : PathStr; MapOwner : tMapForm) : boolean;
begin
   Result := false;
   if (Fname = '') or (not FileExists(fName)) then begin
      {$IfDef RecordGAZProblems}
      WriteLineToDebugFile('OpenGazetteer and asking about file');
      {$EndIf}
      if not DEM_Gaz_Opts.GetGazFileName(LastGazFile) then exit;
   end
   else LastGazFile := fName;

   if FindOpenDataBase(i) and FileExists(LastGazFile) then begin

      {$IfDef RecordGAZProblems}
      WriteLineToDebugFile('OpenGazetteer=' + MDDef.GazOpt.LastGazFile);
      {$EndIf}

       GISdb[i] := TGISdataBaseModule.Create(Application);
       if GISdb[i].InitializeTheTable('',LastGazFile) then begin
          GISdb[i].DataBaseNumber := i;
          if (MapOwner <> Nil) then with MapOwner.MapDraw.MapCorners do begin
             GISdb[i].theMapOwner := MapOwner;
             GISdb[i].theMapDraw := MapOwner.MapDraw;
          end;

          GISdb[i].SetGazTableOptions;

          if (MapOwner <> Nil) then with MapOwner.MapDraw.MapCorners do begin
             if (CurScrLatHigh - CurScrLatLow < 15) then with GISdb[i] do begin
                MyData.ApplyFilter(MakePointGeoFilter(LatFieldName,LongFieldName,CurScrLatHigh,CurScrLongLow,CurScrLatLow,CurScrLongHigh));
             end;
             GISdb[i].dbTablef.ShowStatus;
          end;
          Result := true;
       end;
    end;
end;



procedure TGISdataBaseModule.PlotDBOnMapFromRules(var Bitmap : tMyBitmap; Rules : PathStr; MaskSize : integer = -1);
var
   RulesForTheData : tMyData;
   LineSize : integer;
   Color : tColor;
   aFilter : ShortString;
begin
  {$IfDef RecordPlotDBRules}
   WriteLineToDebugFile('tGISDataBase.PlotDBOnMapFromRules in with rules=' + Rules,true);
   WriteLineToDebugFile('  map pixel size=' + IntToStr(round(TheMapDraw.ScreenPixelSize)));
  {$EndIf}
   if FileExists(Rules) then begin
       RulesForTheData := tMyData.Create(Rules);
       ShowHourglassCursor;
       EmpSource.Enabled := false;
       if (MaskSize > 0) then begin
           LineSize := round(MaskSize * 2 / theMapDraw.ScreenPixelSize);
           if ItsAPointDB  then LineSize := LineSize div 2;
           if (LineSize < 1) then LineSize := 1;
           Bitmap.Canvas.Pen.Width := LineSize;
       end;
       while not RulesForTheData.EOF do begin
          {$IfDef RecordPlotDBRules}
          WriteLineToDebugFile(RulesForTheData.GetFieldByNameAsString('NAME'),true);
          WriteLineToDebugFile('  plot beyond pixel size=' + IntToStr(round(RulesForTheData.GetFieldByNameAsInteger('PIXEL_SIZE'))));
          {$EndIf}

          if (RulesForTheData.GetFieldByNameAsString('NAME') <> '') then begin
            if (RulesForTheData.GetFieldByNameAsString('PLOT') = 'Y') then begin
               if (not(MDDef.UsePixelSizeRules) or (TheMapDraw.ScreenPixelSize <= RulesForTheData.GetFieldByNameAsInteger('PIXEL_SIZE'))) then begin
                   EmpSource.Enabled := false;
                   if (MaskSize <= 0) then begin
                      if AreaShapeFile(ShapeFileType) then begin
                      end
                      else if LineShapeFile(ShapeFileType) then begin
                        Bitmap.Canvas.Pen.Color := RulesForTheData.GetFieldByNameAsInteger('LINE_COLOR');
                        Bitmap.Canvas.Pen.Width := RulesForTheData.GetFieldByNameAsInteger('LINE_WIDTH');
                      end
                      else begin
                         RulesForTheData.DefinePointSymbol(dbOpts.Symbol,dbOpts.SymbolSize,Color);
                         Bitmap.Canvas.Pen.Color := Color;
                      end;
                   end;

                   aFilter := PetDBUtils.AddAndIfNeeded(GeoFilter) + PetDBUtils.AddAndIfNeeded(MainFilter) + RulesForTheData.GetFieldByNameAsString('FILTER');
                   if (aFilter = '') then begin
                      aShapeFile.PlotAllRecords(theMapDraw,Bitmap);
                   end
                   else begin
                      MyData.ApplyFilter(aFilter,false);   // 9/3/2015 false option fails for Tiger, because RecNo is off?
                      {$IfDef RecordPlotDBRules}
                      WriteLineToDebugFile(MyData.Filter,true);
                      WriteLineToDebugFile('recs=' + IntToStr(MyData.RecordCount));
                      WriteLineToDebugFile('Color=' + IntToStr(Bitmap.Canvas.Pen.Color));
                      WriteLineToDebugFile('Line width=' + IntToStr(Bitmap.Canvas.Pen.Width));
                      {$EndIf}
                      MyData.First;
                      while not MyData.EOF do begin
                         aShapeFile.PlotSingleRecord(theMapDraw,Bitmap,MyData.RecNo);
                         MyData.Next;
                      end;
                   end;
                 end
                 else begin
                   {$IfDef RecordPlotDBRules}
                   WriteLineToDebugFile('Not plot for size');
                   {$EndIf}
                 end;
             end
             else begin
                {$IfDef RecordPlotDBRules}
                WriteLineToDebugFile('PLOT="N"');
                {$EndIf}
             end;
          end
          else begin
             {$IfDef RecordPlotDBRules}
             WriteLineToDebugFile('NAME=""');
             {$EndIf}
          end;
          RulesForTheData.Next;
        end;
        RulesForTheData.Destroy;
        MyData.ApplyFilter(MainFilter);
        ShowDefaultCursor;
        EmpSource.Enabled := true;
    end
    else begin
       MessageToContinue('Missing ' + Rules + MessLineBreak + 'Cannot plot ' + dbFullName);
    end;
end;



procedure TGISdataBaseModule.PlotTigerShapeFileOnMap(var Bitmap : tMyBitmap; RoadsOnly : boolean = false; MaskSize : integer = -1; StreamsOnly : boolean = false);
var
   x,y : integer;
   Lat,Long : float;
   LabelBitmap : tMyBitmap;
   StreetsPlotted : tStringList;
   Labelled : boolean;
   FEname,TStr : shortString;
   OldFilter : ANSIString;
begin
   {$IfDef RecordTiger}
   WriteLineToDebugFile('tGISDataBase.PlotTigerShapeFileOnMap in',true);
   if MDDef.TigrDef.DrawLabels then TStr := 'Yes' else TStr := 'No';
   WriteLineToDebugFile('MDDef.TigrDef.DrawLabels: ' + TStr);
   WriteLineToDebugFile('MDDef.TigrDef.AppearLabels=' + IntToStr(MDDef.TigrDef.AppearLabels));
   {$EndIf}
   OldFilter := MyData.Filter;
   if ItsOSMShapeFile then begin
     PlotDBOnMapFromRules(Bitmap,OSMShapeRules);
   end;

   if ItsTigerShapeFile then begin
     {$IfDef RecordTiger}
     WriteLineToDebugFile('TIGER shapefile');
     {$EndIf}

     if StreamsOnly then MainFilter := '(HYDROFLG=' + QuotedStr('Y') + 'OR MTFCC=' + QuotedStr('P0002') + ')'
     else if RoadsOnly then MainFilter := 'ROADFLG=' + QuotedStr('Y');

     PlotDBOnMapFromRules(Bitmap,TigerShapeRules,MaskSize);

     LabelBitmap := Nil;
     if MDDef.TigrDef.DrawLabels and MDDef.TigrDef.AutoAppear and (TheMapDraw.ScreenPixelSize < MDDef.TigrDef.AppearLabels) then begin
        ShowHourglassCursor;
        EmpSource.Enabled := false;

        CloneBitmap(Bitmap,LabelBitmap);

        Bitmap.Canvas.Brush.Style := bsClear;
        StreetsPlotted := tStringList.Create;
        StreetsPlotted.Sorted := true;
        StreetsPlotted.Duplicates := dupIgnore;

        if (Geofilter <> '') then TStr := Geofilter + ' AND '
        else TStr := '';

        if MyData.FieldExists('ROADFLG') then TStr := TStr + 'ROADFLG=' + QuotedStr('Y') + ' AND ';
        MyData.ApplyFilter( TStr + ' FULLNAME <>' + QuotedStr(''));
        {$IfDef RecordTiger}
        WriteLineToDebugFile('Text labels');
        WriteLineToDebugFile('Filter=' + MyData.Filter);
        WriteLineToDebugFile('  recs=' + IntToStr(MyData.RecordCount));
        {$EndIf}

         while not MyData.EOF do
         begin
           FEName := MyData.GetFieldByNameAsString('FULLNAME');
           ShortenTigerName(FEName);
           if (StreetsPlotted.IndexOf(FEName) = -1) or MDDef.LabelEveryTigerFeature then
           begin
              aShapeFile.LineCenter(MyData.RecNo,Long,Lat);
              TheMapDraw.LatLongDegreeToScreen(Lat,long,x,y);
              if TheMapDraw.OnScreen(x,y) then begin
                 x := x - Bitmap.Canvas.TextWidth(FEName) div 2;
                 y := y - Bitmap.Canvas.TextHeight(FEName) div 2;
                 Labelled := true;
                 if MDDef.AvoidTextOverprints then Labelled := PetImage.SmartTextOut(Bitmap,LabelBitmap,x,y,FEName)
                 else Bitmap.Canvas.TextOut(x,y,FEName);
                 if Labelled then StreetsPlotted.Add(FEName);
              end;
           end;
           MyData.Next;
         end;
         if (LabelBitmap <> Nil) then FreeAndNil(LabelBitmap);
         StreetsPlotted.Free;
         ShowDefaultCursor;
         EmpSource.Enabled := true;
      end;
   end;
   MyData.ApplyFilter(OldFilter);

   if MDDef.MakeTigerMapGrayscale then MakeBitmapGrayScale(Bitmap);
   if MDDef.SubdueTigerBase then MakeBitmapSubdued(Bitmap);
end;



procedure TGISdataBaseModule.ReplaceLayerBitmap(var Bitmap : tMyBitmap);
begin
   if (theMapDraw.DBOverlayfName[DataBaseNumber] = '') then
       theMapDraw.DBOverlayfName[DataBaseNumber] := NextFileNumber(MDTempDir, 'db_overlay', OverlayFExt);
   PetImage.SaveBitmap(Bitmap,theMapDraw.DBOverlayfName[DataBaseNumber]);

   TheMapOwner.DoFastMapRedraw;
   FreeAndNil(Bitmap);
end;


procedure TGISdataBaseModule.PlotRatioOnMap(NumField,DenField : shortstring);
var
   x,y : float;
   Bitmap : tMyBitmap;
begin
   EmpSource.Enabled := false;
   CloneImageToBitmap(TheMapOwner.Image1,Bitmap);
   MyData.First;
   while not MyData.eof do begin
      if GetFloatFromTableLinkPossible(MyData,LinkTable,dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,DenField,x) and (abs(x) > 0.00001) then begin
         if GetFloatFromTableLinkPossible(MyData,LinkTable,dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,NumField,y) then begin
            Bitmap.Canvas.Pen.Color := SelectedColorSchemeColorFunct(DBColorScheme,ColorDefTable,y/x,dbOpts.ColorMin,dbOpts.ColorMax);
            Bitmap.Canvas.Brush.Color := Bitmap.Canvas.Pen.Color;
            DisplayCurrentRecordOnMap(Bitmap);
         end;
      end;
      MyData.Next;
   end;
   ReplaceLayerBitmap(Bitmap);
   DrawLayerLegend(dbOpts.ColorMin,dbOpts.ColorMax,NumField + '/' + DenField);
   EmpSource.Enabled := true;
end;


procedure TGISdataBaseModule.PlotFieldOnMap(aField : shortstring; Minx : float = 9999; MaxX : float = -9999);
var
   x : float;
   i : integer;
   Bitmap : tMyBitmap;
begin
   with MyData,dbOpts do begin
      EmpSource.Enabled := false;
      CloneImageToBitmap(TheMapOwner.Image1,Bitmap);
      If (MinX > MaxX) then PetDBUtils.FindFieldRange(EmpSource,MyData,aField,MinX,MaxX);
      EmpSource.Enabled := false;
      i := 0;
      StartProgressAbortOption(afield);

      PrepColors(Bitmap);
      First;
      while not eof do begin
         inc(i);
         if (i mod 1000 = 0) then begin
            UpDateProgressBar(i/MyData.RecordCount);
            EmpSource.Enabled := false;
         end;
         if GetFloatFromTableLinkPossible(MyData,LinkTable,LinkFieldThisDB,LinkFieldOtherDB,aField,x) {and (abs(x) > 0.00001)} then begin
            Bitmap.Canvas.Pen.Color := SelectedColorSchemeColorFunct(DBColorScheme,ColorDefTable,x,MinX,MaxX);
            Bitmap.Canvas.Brush.Color := Bitmap.Canvas.Pen.Color;
            DisplayCurrentRecordOnMap(Bitmap);
         end;
         Next;
         if WantOut then Break;

      end;
      ReplaceLayerBitmap(Bitmap);
      DrawLayerLegend(MinX,MaxX,aField);
      EmpSource.Enabled := true;
      EndProgress;
   end;
end;


procedure TGISdataBaseModule.PlotIntegerFieldOnMap(aField : shortstring; fName : PathStr);
var
   x       : float;
   i,j : integer;
   Bitmap  : tMyBitmap;
   Table   : tMyData;
   Colors  : array[0..1024] of tColor;
begin
   for i := 0 to 1024 do Colors[i] := -99;
   Table := tMyData.Create(fName);
   while not Table.eof do begin
      if Table.GetFieldByNameAsString('USE') = 'Y' then begin
         i := Table.GetFieldByNameAsInteger('VALUE');
         Colors[i] := clRed;
         Table.CarefullyGetFieldByNameAsInteger('COLOR',j);
         Colors[i] := j;
      end;
      Table.Next;
   end;
   Table.Destroy;

   EmpSource.Enabled := false;
   CloneImageToBitmap(TheMapOwner.Image1,Bitmap);

   EmpSource.Enabled := false;
   i := 0;
   StartProgress(afield);

   PrepColors(Bitmap);
   MyData.First;
   while not MyData.eof do begin
      inc(i);
      if (i mod 1000 = 0) then UpDateProgressBar(i/MyData.RecordCount);
      if GetFloatFromTableLinkPossible(MyData,LinkTable,dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,aField,x) {and (abs(x) > 0.00001)} then begin
         j := round(x);
         if (j >= 0) and (j <= 1024) and (Colors[j] >= 0) then begin
            Bitmap.Canvas.Brush.Color := Colors[j];
            DisplayCurrentRecordOnMap(Bitmap);
         end;
      end;
      MyData.Next;
   end;
   ReplaceLayerBitmap(Bitmap);
   EmpSource.Enabled := true;
   EndProgress;
end;


procedure TGISdataBaseModule.PlotAndLabelCurrentRecord(Bitmap : tMyBitmap; Plot : boolean = true);
var
   xp,yp,Angle,Loc,Len,High : integer;
   Size : byte;
   OldFont : tFont;
   TStr : ShortString;
   Sym : tDrawingSymbol;
   Color : tColor;
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
   {$IfDef RecordDataBaseLabels}
   WriteLineToDebugFile('tGISDataBase.PlotAndLabelPoints');
   {$EndIf}
   if LatLongFieldsPresent then begin
      MyData.First;
      repeat
         EmpSource.Enabled := false;
         PlotAndLabelCurrentRecord(Bitmap,Plot);
         MyData.Next;
      until MyData.EOF;
   end;
   EmpSource.Enabled := true;
end;

procedure TGISdataBaseModule.IdentifyRecord(xpic,ypic : integer; Lat,Long : float; var RecsFound : integer;  ShowRec,LabelRec : boolean;
          var FeatureName : shortString; ShowItModal : boolean = true; JustFilterDB : boolean = false);
label
   AllDone;
var
   Tol,LatLow,LongLow,LatHigh,LongHigh : float;
   i,j : integer;
   OldFilter : string;
   Bitmap2,Bitmap : tMyBitmap;
   theMainFilter : string;
begin
   {$IfDef RecordIDProblems}
   WriteLineToDebugFile('tGISDataBase.IdentifyRecord enter ' + dbName,true);
   {$EndIf}
   PrevNextButtonsEnabled := false;
   EmpSource.Enabled := false;
   OldFilter := MyData.Filter;
   ClearGISFilter;
   {$IfDef RecordIDProblems}
   WriteLineToDebugFile('unfiltered record count =' + IntToStr(MyData.RecordCount));
   WriteLineToDebugFile('Old filter =' + OldFilter);
   WriteLineToDebugFile('Main filter =' + MainFilter);
   {$EndIf}

   if (MainFilter = '') then theMainFilter := '' else theMainFilter := MainFilter + ' AND ';
   if (OldFilter = '') then theMainFilter := '' else theMainFilter := OldFilter + ' AND ';

   if SimplePointFile or XYZFile then begin
      {$IfDef RecordIDProblems}
      WriteLineToDebugFile('Point DB');
      {$EndIf}
      i := 0;
      repeat
          inc(i,IDRecScreenIncr);
          if (TheMapOwner <> Nil) then begin
             theMapDraw.ScreenToLatLongDegree(xpic-i,ypic-i,LatHigh,LongLow);
             theMapDraw.ScreenToLatLongDegree(xpic+i,ypic+i,LatLow,LongHigh);
             PetMath.MinOfPairFirst(LatLow,LatHigh);
             PetMath.MinOfPairFirst(LongLow,LongHigh);
             MyData.ApplyFilter(TheMainFilter + '(' + MakePointGeoFilter(LatFieldName,LongFieldName,LatHigh,LongLow,LatLow,LongHigh) + ')',true);
          end
          else with theGraphOwner do begin
            LongLow := GraphDraw.InvGraphX(xpic-i);
            LongHigh := GraphDraw.InvGraphX(xpic+i);
            LatLow := GraphDraw.InvGraphY(ypic+i);
            LatHigh := GraphDraw.InvGraphY(ypic-i);
            MyData.ApplyFilter('X' + ' >= ' + RealToString(LongLow,-12,-4) +
                      ' AND ' + 'X' + ' <= ' + RealToString(LongHigh,-12,-4) +
                      ' AND ' + 'Y' + ' >= ' + RealToString(LatLow,-12,-4) +
                      ' AND ' + 'Y' + ' <= ' + RealToString(LatHigh,-12,-4), true);
          end;
      until (MyData.RecordCount > 0) or (i >= IDRecScreenTolerance);

      RecsFound := MyData.RecordCount;
      DBEditRecord := MyData.RecNo;
      if JustFilterDB then goto AllDone;

      {$IfDef RecordIDProblems}
      if (RecsFound = 0) then WriteLineToDebugFile('Final ID filter: ' + Filter);
      WriteLineToDebugFile('filtered record count =' + IntToStr(MyData.RecordCount));
      {$EndIf}

      if (RecsFound > 0) then begin
         if (ShowRec or LabelRec) then  begin
            if (RecsFound < 5) or AnswerIsYes('Show all ' + IntToStr(RecsFound) + ' records') then  begin
              MyData.First;
              for j := 0 to pred(MyData.RecordCount) do   begin
                 if ShowRec then  begin
                    if (dbTablef <> Nil) then dbTablef.Highlightrecordonmap1Click(Nil);
                    DisplayTheRecord(j,ShowItModal,DEMNowDoing = EditDBRecs);
                 end;
                 if LabelRec then begin
                    theMapOwner.Image1.Canvas.TextOut(xpic+4,ypic+4,MyData.GetFieldByNameAsString(dbOpts.LabelField));
                 end;
                 if not MyData.EOF then MyData.Next;
              end;
            end;
         end
         else begin
            {$IfDef RecordIDProblems}
            WriteLineToDebugFile('Bookmark set');
            {$EndIf}
         end;
      end
      else  begin
         {$IfDef RecordIDProblems}
         WriteLineToDebugFile('No recs found');
         {$EndIf}
      end;
   end;

   if LatLongCornersPresent then with theMapDraw,MyData,dbOpts do begin
      Tol := 0.00001;
      repeat
         MyData.ApplyFilter(theMainFilter + '(' + MakeCornersGeoFilter(Lat+Tol,Long-Tol,Lat-Tol,Long+Tol) + ')');
         {$IfDef RecordIDProblems}
         WriteLineToDebugFile('Filter tolerance =' + RealToString(Tol,-18,-6) + '   record count =' + IntToStr(MyData.RecordCount));
         {$EndIf}
         Tol := Tol * 5;
      until (Tol > 0.001) or (MyData.RecordCount > 1);
      {$IfDef RecordIDProblems}
      WriteLineToDebugFile('filtered record count =' + IntToStr(MyData.RecordCount));
      if (RecsFound = 0) then  begin
         WriteLineToDebugFile('ID filter: ' + Filter);
         WriteLineToDebugFile(LatLongDegreeToString(Lat,Long,DecDegrees));
      end;
      {$EndIf}
      RecsFound := 0;
      if (RecordCount > 0) then begin
           MyData.First;
           for j := 0 to pred(RecordCount) do begin
             {$IfDef RecordIDProblems}
             WriteLineToDebugFile('Rec: ' + IntToStr(RecNo));
             {$EndIf}
              CreateBitmap(Bitmap,MapXSize,MapYSize);

              if AreaShapeFile(ShapeFileType) then Bitmap.Canvas.Pen.Width := 1
              else if LineShapeFile(ShapeFileType) then Bitmap.Canvas.Pen.Width := IDRecScreenTolerance;

              Bitmap.Canvas.Pen.Color := clBlack;
              Bitmap.Canvas.Brush.Color := clBlack;
              Bitmap.Canvas.Brush.Style := bsSolid;
              DisplayCurrentRecordOnMap(Bitmap);

              if (Bitmap.Canvas.Pixels[xpic,ypic] <> clWhite) then begin
                 if ShowRec then DisplayTheRecord(j,ShowItModal,DEMNowDoing = EditDBRecs);
                 if LabelRec then begin
                    ScreenToLatLongDegree(xpic,ypic,LatHigh,LongLow);
                    theMapOwner.AddPointToDB(LatHigh,LongLow,MyData.GetFieldByNameAsString(LabelField));
                 end;
                 if (LabelField <> '') then FeatureName := MyData.GetFieldByNameAsString(LabelField);
                 inc(RecsFound);

                 {$IfDef ExRedistrict}
                 {$Else}
                 if (DEMNowDoing = RecolorDataBase) then  begin
                    MyData.Edit;
                    MyData.SetFieldByNameAsString('DISTRICT',RedistrictForm.ComboBox1.Text);
                    MyData.SetFieldByNameAsInteger('COLOR',RedistrictForm.CurrentColor);
                    CopyImageToBitmap(theMapOwner.Image1,BitMap2);
                    SetRedistrictColor(Bitmap2);
                    DisplayCurrentRecordOnMap(Bitmap2);
                    theMapOwner.Image1.Picture.Graphic := Bitmap2;
                    Bitmap2.Free;
                    RedistrictForm.UpdateTable;
                 end;
                 {$EndIf}
              end;
              MyData.Next;
              Bitmap.Free;
           end;
      end;
   end;
   MyData.ApplyFilter(OldFilter);
   {$IfDef RecordIDProblems}
   WriteLineToDebugFile('Leaving tGISDataBase.IdentifyRecord, ' + DBName + ' filter=' + MyData.Filter + ' and recs=' + IntToStr(MyData.RecordCount));
   {$EndIf}

  AllDone:;
   EmpSource.Enabled := true;
   if (DBTablef <> nil) then dbTablef.ShowStatus;
end;


procedure TGISdataBaseModule.ConnectSequentialPoints(Bitmap : tMyBitmap; ConnectTwoPoints : boolean);
var
   Lat,Long,Strike : float;
   x,y,i,midx,midy,LastX,LastY : integer;
   FirstPoint : boolean;
begin
   Bitmap.Canvas.Pen.Color := MDDef.ConnectColor;
   Bitmap.Canvas.Pen.Width := MDDef.ConnectWidth;
   i := 0;
   FirstPoint := true;
   MyData.First;
   while not MyData.eof do begin
      if ValidLatLongFromTable(Lat,Long) then
      begin
         if MyData.FieldExists('COLOR') then Bitmap.Canvas.Pen.Color := MyData.GetFieldByNameAsInteger('COLOR');

         TheMapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
         inc(i);
         if FirstPoint then
         begin
            Bitmap.Canvas.MoveTo(x,y);
            FirstPoint := false;
         end
         else
         begin
            if MDDef.ConnectArrows and (i >= MDDef.ConnectArrowSpacing) then
            begin
               i := 0;
               Strike := HeadingOfLine(x-LastX,Lasty-y);
               midx := (x + lastX) div 2;
               midy := (y + lasty) div 2;
               PlotVector(Bitmap,LastX,LastY,midx,midy,MDDef.ConnectColor,MDDef.ConnectWidth,true);
            end;
            Bitmap.Canvas.LineTo(x,y);
            if ConnectTwoPoints then
            begin
               FirstPoint := true;
            end;
         end;
         LastX := x;
         LastY := y;
      end;
      MyData.Next;
   end;
end;


procedure TGISdataBaseModule.OutlineCurrentViewOnMap;
var
   Lat,Long,Lat2,Long2,HFOV,Az : float;
   x,y : integer;
begin
    if (theMapOwner <> Nil) then
    begin
       if ValidLatLongFromTable(Lat,Long) and MyData.FieldExists('HFOV') and MyData.FieldExists('AZIMUTH') then begin
          theMapOwner.Image1.Canvas.Pen.Color := clRed;
          theMapOwner.Image1.Canvas.Pen.Width := 3;
          HFOV := MyData.GetFieldByNameAsFloat('HFOV');
          AZ := MyData.GetFieldByNameAsFloat('AZIMUTH');
          DEMDatum.PointAtDistanceBearing(Lat,Long,25000,Az-0.5*HFOV,Lat2,Long2);
          theMapOwner.MapDraw.LatLongDegreeToScreen(Lat2,Long2,x,y);
          theMapOwner.Image1.Canvas.MoveTo(x,y);
          theMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
          theMapOwner.Image1.Canvas.LineTo(x,y);
          DEMDatum.PointAtDistanceBearing(Lat,Long,25000,Az+0.5*HFOV,Lat2,Long2);
          theMapOwner.MapDraw.LatLongDegreeToScreen(Lat2,Long2,x,y);
          theMapOwner.Image1.Canvas.LineTo(x,y);
       end;
    end;
end;


procedure TGISdataBaseModule.PlotRecord1to1Map(var NewMap : tMapForm; var Bitmap : tMyBitmap);
begin
   NewMap := TheMapOwner.DuplicateMap(false,false,true);
   NewMap.MapDraw.MapType := mtSatBlank;
   NewMap.Closable := true;
   NewMap.MapDraw.MaximizeLatLongMapCoverage(MyData.GetFieldByNameAsFloat('LAT_LOW'),MyData.GetFieldByNameAsFloat('LONG_LOW'),
        MyData.GetFieldByNameAsFloat('LAT_HI'),MyData.GetFieldByNameAsFloat('LONG_HI'));
   NewMap.N11view1Click(Nil);
   CloneImageToBitmap(NewMap.Image1,Bitmap);
   Bitmap.Canvas.Brush.Color := clBlack;
   Bitmap.Canvas.Brush.Style := bsSolid;
   aShapeFile.PlotSingleRecord(NewMap.MapDraw,Bitmap,MyData.RecNo);
end;


procedure TGISdataBaseModule.ConnectTwoPointsInDB(var Bitmap : tMyBitmap; Color : tColor; Width : byte);
var
   Lat,Long,Lat2,Long2 : float;
   XP2,yp2,XPic,ypic : integer;
begin
   Bitmap.Canvas.Pen.Color := Color;
   Bitmap.Canvas.Pen.Width := Width;
   MyData.First;
   while not MyData.EOF do
   begin
       if ValidLatLongFromTable(Lat,Long) and ValidLat2Long2FromTable(Lat2,Long2) then
       begin
          theMapDraw.LatLongDegreeToScreen(Lat2,Long2,xp2,yp2);
          theMapDraw.LatLongDegreeToScreen(Lat,Long,xpic,ypic);
          Bitmap.Canvas.MoveTo(XP2,yp2);
          Bitmap.Canvas.LineTo(XPic,ypic);
       end;
       MyData.Next;
   end;
end;



procedure TGISdataBaseModule.AddGeometry(What : tAddGeometry);
var
   area,Length,xcent,ycent,Lat,Long,Lat2,Long2,Dist,Bearing : float;
   i : integer;
   TStr : shortString;
begin
   if (What = agCentroid) then begin
      AddFieldToDataBase(ftFloat,'LAT_CENTRD',12,6);
      AddFieldToDataBase(ftFloat,'LON_CENTRD',12,6);
   end;
   if (What = agAreaKM2) then begin
      AddFieldToDataBase(ftFloat,'AREA_KM2',12,4);
   end;
   if (What = agAreaM2) then begin
      AddFieldToDataBase(ftFloat,'AREA_M2',12,1);
   end;
   if (What = agPerimeter) then begin
      AddFieldToDataBase(ftFloat,'PERIMTR_KM',12,4);
   end;
   if (What = agLength) then begin
      AddFieldToDataBase(ftFloat,'LENGTH_KM',12,4);
   end;
   if (What = agZStats) then begin
      AddFieldToDataBase(ftFloat,'FIRST_Z',12,2);
      AddFieldToDataBase(ftFloat,'LAST_Z',12,2);
   end;
   if (What = asSinuousity) then begin
      AddFieldToDataBase(ftFloat,'SINUOSITY',8,3);
   end;
   if (What = agNumPts) then begin
      AddFieldToDataBase(ftInteger,'REC_PNTS',6);
   end;
   if (What = agElevationDeltas) then begin
      AddFieldToDataBase(ftFloat,'DELTA_Z',8,2);
   end;
   if (What = agDirection) then begin
      AddFieldToDataBase(ftFloat,'REC_DIR',6,2);
   end;
   if (What = agEndPoints) then begin
      AddFieldToDataBase(ftFloat,'LAT_START',11,7);
      AddFieldToDataBase(ftFloat,'LONG_START',12,7);
      AddFieldToDataBase(ftFloat,'LAT_END',11,7);
      AddFieldToDataBase(ftFloat,'LONG_END',12,7);
   end;
   i := 0;
   StartProgress('Geometry');
   MyData.First;
   while not MyData.eof do begin
      if (i mod 1000 = 0) then
      begin
         UpdateProgressBar(i/MyData.RecordCount);
         EmpSource.Enabled := false;
      end;
      inc(i);

      if (What = agLength) or (What = agPerimeter) or (What = asSinuousity) then
         Length := aShapeFile.LineLength({theMapDraw.CurrentMapDatum,}MyData.RecNo)
      else if LineShapeFile(ShapeFileType) and (What = agCentroid) then
      begin
         ycent := 0.5 * (MyData.GetFieldByNameAsFloat('LAT_HI') + MyData.GetFieldByNameAsFloat('LAT_LOW'));
         xcent := 0.5 * (MyData.GetFieldByNameAsFloat('LONG_HI') + MyData.GetFieldByNameAsFloat('LONG_LOW'));
      end
      else if (What = agAreaKM2) or (What = agCentroid) or (What = agAreaM2)  then
         Area := aShapeFile.AreaAndCentroid(theMapDraw.CurrentMapDatum,MyData.RecNo,ycent,xcent)
      else if (What = agElevationDeltas) or (What = agEndPoints) then aShapeFile.GetLineCoords(MyData.RecNo,true);

      MyData.Edit;
      if (What = agAreaKM2)  then begin
          MyData.SetFieldByNameAsFloat('AREA_KM2',0.000001*Area);
      end
      else if (What = agAreaM2) then begin
          MyData.SetFieldByNameAsFloat('AREA_M2',Area);
      end
      else if (What = agPerimeter) then begin
         MyData.SetFieldByNameAsFloat('PERIMTR_KM',0.001 * Length);
      end
      else if (What = agLength) then begin
         MyData.SetFieldByNameAsFloat('LENGTH_KM',0.001 * Length);
      end
      else if (What = agElevationDeltas) then begin
         if aShapeFile.CurrentPolyLineHeader.NumPoints > 2 then begin
            MyData.SetFieldByNameAsFloat('DELTA_Z',abs(aShapeFile.CurrentLineZs^[0]-aShapeFile.CurrentLineZs^[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)]));
         end;
      end
      else if (What = asSinuousity) or (What = agEndPoints) then begin
         if (aShapeFile.CurrentPolyLineHeader.NumPoints > 2) then begin
             Lat := aShapeFile.CurrentLineCoords^[0].Lat;
             Long := aShapeFile.CurrentLineCoords^[0].Long;
             Lat2 := aShapeFile.CurrentLineCoords^[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)].Lat;
             Long2 := aShapeFile.CurrentLineCoords^[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)].Long;
             if (What = agEndPoints) then begin
                MyData.SetFieldByNameAsFloat('LAT_START',Lat);
                MyData.SetFieldByNameAsFloat('LONG_START',Long);
                MyData.SetFieldByNameAsFloat('LAT_END',Lat2);
                MyData.SetFieldByNameAsFloat('LONG_END',Long2);
             end
             else begin
                DEMDatum.CalculateDistanceBearing(Lat,Long,Lat2,Long2,Dist,Bearing);
                if (Dist > 0.01) then MyData.SetFieldByNameAsFloat('SINUOSITY',Length/Dist);
             end;
         end;
      end
      else if (What = agDirection) then begin
         MyData.SetFieldByNameAsFloat('REC_DIR',aShapeFile.LineHeading(MyData.RecNo));
      end
      else if (What = agCentroid) then begin
         MyData.SetFieldByNameAsFloat('LAT_CENTRD',yCent);
         MyData.SetFieldByNameAsFloat('LON_CENTRD',xCent);
         theMapDraw.MapSymbolAtLatLongDegree(TheMapOwner.Image1.Canvas,ycent,xcent,TheMapOwner.DrSymbol);
      end
      else if (What = agZStats) or (What = agNumPts) then begin
         aShapeFile.GetLineCoords(MyData.RecNo,(What = agZStats));
         if (What = agZStats) then begin
           {$IfDef RecordMakeLineArea}
           if MyData.FieldExists('NAME') then TStr := '  ' + MyData.GetFieldByNameAsString('NAME')
           else TStr := '';
           WriteLineToDebugFile(IntToStr(MyData.RecNo) + RealToString(zs^[0],12,1) + RealToString(zs^[pred(NumPts)],12,1) + TStr);
           {$EndIf}
           MyData.CarefullySetFloat('FIRST_Z',aShapeFile.CurrentLineZs^[0], 0.01);
           MyData.CarefullySetFloat('LAST_Z',aShapeFile.CurrentLineZs^[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)],0.01);
         end;
         if (What = agNumPts) then begin
            MyData.SetFieldByNameAsInteger('REC_PNTS',aShapeFile.CurrentPolyLineHeader.NumPoints);
         end;
      end;
      MyData.Next;
   end;
   DBTablef.ShowStatus;
end;


procedure TGISdataBaseModule.DisplayCurrentRecordOnMap(Bitmap : tMyBitmap; ReallyDraw : boolean = true);
var
   xc,yc : integer;
   LastDisplayed,
   UsesFillPattern : boolean;
begin
   LastDisplayed := false;
   if (MyData <> Nil) then begin
      if SimplePointFile and (LatFieldName <> '') and (LongFieldName <> '') then begin
         if ValidScreenPositionFromTable(dbxpic,dbypic) then begin
            //LastDisplayed := true;
            if ReallyDraw then begin
                ScreenSymbol(Bitmap.Canvas,dbXpic,dbYpic,dbOpts.Symbol,dbOpts.SymbolSize,Bitmap.Canvas.Pen.Color);
            end;
         end;
      end
      else if ItsAShapeFile then begin
         {$IfDef RecordCurrentRecord}
         WriteLineToDebugFile('TGISdataBaseModule.DisplayCurrentRecordOnMap RecNo=' + IntToStr(MyData.RecNo));
         {$EndIf}
         if (theMapDraw <> Nil) then aShapeFile.PlotSingleRecord(theMapDraw,Bitmap,MyData.RecNo)
         else aShapeFile.PlotSingleRecord(theGraphOwner,Bitmap,MyData.RecNo);
         LastDisplayed := true;
      end
      else if LatLongCornersPresent then begin
         theMapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat('LAT_HI'),MyData.GetFieldByNameAsFloat('LONG_LOW'),xc,yc);
         theMapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat('LAT_LOW'),MyData.GetFieldByNameAsFloat('LONG_HI'),dbxpic,dbypic);
         LastDisplayed := (xc < 32000) and (yc < 32000) and (dbxpic < 32000) and (dbypic < 32000);
         if ReallyDraw and LastDisplayed then begin
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


procedure TGISdataBaseModule.DisplayOnMap(var Bitmap : tMyBitmap; HowRestrict : tHowRestrict = resNone;
    LabelEm : boolean = false; NeedMargins : boolean = true; UseNameField : boolean = false;
    AcceptDefaultRange : boolean = false; WantedFieldName : shortstring = ''; JustLabels : boolean = false);
const
   vth = ' value to highlight';
var
   xmin,xmax,ymin,ymax,x,y : integer;
   StoreWhere : tStringList;
   Plot : boolean;
   sBitmap : tMyBitmap;
   LastFName,
   fName : PathStr;
   SaveFillColor,
   SymColor : tColor;
   NeedRecordCount,RecFound,LineMult,
   y2,yt,i : integer;
   DeleteOnExit : boolean;
   OldFilter : string;
   Lat,Long : float;
begin
   {$IfDef RecordDBPlot}
   WriteLineToDebugFile('TGISDataBase.DisplayOnMap in, fname= ' + dbName + '  ' + TimeToStr(now),true);
   WriteLineToDebugFile('  Filter=' + MyData.Filter);
   WriteLineToDebugFile('  recs=' + IntToStr(MyData.RecordCount));
   {$EndIf}

   DeleteOnExit := (Bitmap = Nil);
   LastFName := '';

   if (TheMapDraw <> Nil) then theMapDraw.DeleteSingleMapLayer(theMapDraw.LegendOverlayfName);


   if (dbOpts.dbAutoShow in [dbasVector]) then begin
      PlotVectorsOnMap(Bitmap);
   end
   else if (dbOpts.DBAutoShow in [dbasIconAll,dbasIconField]) then PlotIcons(Bitmap)
   else begin
     if ItsAShapeFile and LineOrAreaShapeFile(ShapeFileType) and HighLightDBOnWorld then with aShapeFile do begin
        if (abs(MainFileHeader.BoundBox.YMax) < 85) and (MainFileHeader.BoundBox.XMax - MainFileHeader.BoundBox.XMin < 90) then begin
           theMapDraw.LatLongDegreeToScreen(MainFileHeader.BoundBox.YMin,MainFileHeader.BoundBox.XMin,xmin,ymin);
           theMapDraw.LatLongDegreeToScreen(MainFileHeader.BoundBox.YMax,MainFileHeader.BoundBox.XMax,xmax,ymax);
           if theMapDraw.OnScreen(xmax,ymax) and theMapDraw.OnScreen(xmin,ymin) and ((xmax - xmin) < 10)  and ((ymax - ymin) < 10) then begin
              {$IfDef RecordDBPlot}
              WriteLineToDebugFile('DB too tiny to see on world map');
              {$EndIf}
              ScreenSymbol(Bitmap.Canvas,(xmax+xmin) div 2,(ymax+ymin) div 2,FilledBox,8,clRed);
              exit;
           end;
        end;
     end;

     if (HowRestrict in [resUseColorFieldIHS]) then begin
        if (Bitmap <> Nil) then begin
           theMapOwner.Image1.Picture.Graphic := Bitmap;
           FreeAndNil(Bitmap);
        end;
        CloneImageToBitmap(theMapOwner.Image1,Bitmap);
     end;

     if (Bitmap = Nil) then CopyImageToBitmap(theMapOwner.CurrentMapImage,BitMap);

     if (UseLayerDefinition or (dbOpts.DBAutoShow = dbasLayerDefinition))  and (LayerTable <> Nil) then begin
        {$IfDef RecordDBPlot}
        WriteLineToDebugFile('LayerTable plot');
        {$EndIf}
        OldFilter := MyData.Filter;
        PlotShapeFileGroupOnMap(theMapDraw,Bitmap,LayerTable,Self);
        MyData.ApplyFilter(OldFilter);
        exit;
     end;

     {$IfDef ExGeostats}
     {$Else}
     if (dbOpts.DBAutoShow = dbasTerrainFabric) then begin
        {$IfDef RecordDBPlot}
        WriteLineToDebugFile('DBSaveOptions.DBAutoShow = dbasTerrainFabric');
        {$EndIf}
        PlotFabric(Bitmap);
        exit;
     end;
     {$EndIf}

     if (dbOpts.DBAutoShow = dbasBuffers) then begin
       //CloneImageToBitmap(GISdb[DBonTable].TheMapOwner.Image1,Bitmap);
       AddDatabaseToMask(Bitmap,TheMapOwner,DataBaseNumber,true,MaskIn,MaskingDistance,MDDef.MapMaskColor);
       if MaskIn then ChangeBitmapBlackWhite(Bitmap,Petmar.ConvertTColorToPlatFormColor(MDDef.MapMaskColor),rgbTripleWhite)
       else ChangeBitmapBlackWhite(Bitmap,rgbTripleWhite,Petmar.ConvertTColorToPlatFormColor(MDDef.MapMaskColor));
       //Bitmap.SaveToFile(theMapDraw.DBOverlayfName[DBonTable]);
       exit;
     end;

     if (dbOpts.DBAutoShow = dbasColorByString) then begin
        PlotDBStringField(Bitmap);
        exit;
     end;
     if (dbOpts.DBAutoShow = dbasColorByNumeric) then begin
        PlotDBNumericField(Bitmap);
        exit;
     end;
     if (dbOpts.DBAutoShow = dbasZValues) then begin
        {$IfDef RecordDBPlot}
        WriteLineToDebugFile('DBSaveOptions.DBAutoShow = dbasZValues');
        {$EndIf}
        StartThreadTimers('3D shapefile',1,true);

        aShapeFile.PlotPointCloudOnMap(theMapOwner,Bitmap,dbOpts.ZColorMin,dbOpts.ZColorMax);
        theMapOwner.Image1.Picture.Graphic := Bitmap;
        EndThreadTimers;
        exit;
     end;

     if (dbOpts.DBAutoShow = dbasSymbolInDB) or (FontFieldExists and PointSymbolFieldsPresent) then begin
        {$IfDef RecordDBPlot}
        WriteLineToDebugFile('PlotAndLabelPoints');
        {$EndIf}
        PlotAndLabelPoints(Bitmap);
        exit;
     end;

//MessageToContinue('point 1');


     with MyData do begin
        SymColor := dbOpts.BasicColor;
        SaveFillColor := dbOpts.FillColor;
        //SaveLineColor := LineColor;
        if (not SymbolizationIsCorrent) then Bitmap.Canvas.Pen.Color := dbOpts.BasicColor;
        {$IfDef RecordDBPlot}
        WriteLineToDebugFile('TGISDataBase.DisplayOnMap start:  ' + TimeToStr(Now),true);
        {$EndIf}
        //UpInt := 1;
        if ShowSatProgress then StartProgressAbortOption('Plot DB ' + ExtractFileName(dbFullName));
        NeedRecordCount := RecordCount;
        //if (GeoFilter = '') then LimitDBtoMapArea;
        First;
        RecFound := 0;
        EmpSource.Enabled := false;

        (*
        if (theMapDraw <> Nil) then begin
           theMapDraw.ScreenToLatLongDegree(0,0,Lat1,Long1);
           theMapDraw.ScreenToLatLongDegree(theMapDraw.MapXSize,theMapDraw.MapYSize,Lat2,Long2);
        end;
        *)

        if (dbOpts.DBAutoShow = dbasTTFontSymbol) then begin
           {$IfDef RecordDBPlot}
           WriteLineToDebugFile('DBAutoShow = dbasTTFontSymbol');
           {$EndIf}
           CreateBitmap(Sbitmap,500,500);
           sBitmap.Canvas.Font.Name := dbOpts.TTSymbolFontName;
           sBitmap.Canvas.Brush.Style := bsClear;
           sBitmap.Canvas.Font.Color := dbOpts.TTSymbolFontColor;
           sBitmap.Canvas.Font.Size := dbOpts.TTSymbolFontSize;
           sBitmap.Canvas.TextOut(250,250, dbOpts.TTSymbolFontChar);
           GetImagePartOfBitmap(sBitmap);
       end;

        try
           if (ItsAShapeFile or LatLongCornersPresent) and (not SymbolizationIsCorrent) then begin
              PrepColors(Bitmap);
              if ItsAShapeFile then begin
                 aShapeFile.SymbolSize := dbOpts.SymbolSize;
                 aShapeFile.Symbol:= dbOpts.Symbol;
              end;
           end;

           if (ItsAShapeFile and LineOrAreaShapeFile(ShapeFileType)) and (HowRestrict = resNone) and (not MyData.Filtered) and (dbOpts.DBAutoShow <> dbasTTFontSymbol) then begin
              {$IfDef RecordDBPlot}
              WriteLineToDebugFile('call aShapeFile.PlotAllRecords  ' + TimeToStr(now));
              {$EndIf}

              if (TheMapOwner <> Nil) then aShapeFile.PlotAllRecords(TheMapDraw,Bitmap);
              if (TheGraphOwner <> Nil) then aShapeFile.PlotAllRecordsOnGraph(TheGraphOwner,Bitmap);
           end
           else if (dbOpts.DBAutoShow = dbasConnectPoints) then begin
              {$IfDef RecordDBPlot}
              WriteLineToDebugFile('ConnectTwoPointsInDB');
              {$EndIf}
              ConnectTwoPointsInDB(Bitmap,dbOpts.BasicColor,dbOpts.LineSize);
           end
           else begin
              {$IfDef RecordDBPlot}
              WriteLineToDebugFile('start repeat loop  ' + TimeToStr(now),true);
              if (DBAutoShow = dbasTTFontSymbol) then WriteLineToDebugFile('DBAutoShow = dbasTTFontSymbol');
              if (DBAutoShow = dbasIconAll) then WriteLineToDebugFile('DBAutoShow = dbasIconAll');
              if (DBAutoShow = dbasIconField) then WriteLineToDebugFile('DBAutoShow = dbasIconField');
              {$EndIf}
               repeat
                  Plot := true;
                  EmpSource.Enabled := false;
                  if (dbOpts.DBAutoShow = dbasTTFontSymbol) then begin
                     if LineOrAreaShapeFile(ShapeFileType) then begin
                        aShapeFile.AreaAndCentroid(theMapDraw.CurrentMapDatum,MyData.RecNo,Lat,Long);
                     end
                     else begin
                       Lat := GetFieldByNameAsFloat(LatFieldName);
                       Long := GetFieldByNameAsFloat(LongFieldName);
                     end;
                     TheMapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                     Bitmap.Canvas.Draw(x-sBitmap.Width div 2, y - sBitmap.Height div 2, sBitmap);
                  end
                  else if (dbOpts.DBAutoShow = dbasIconAll) then begin
                     theMapDraw.PlotAnIcon(Bitmap,GetFieldByNameAsFloat(LatFieldName),GetFieldByNameAsFloat(LongFieldName),
                         dbOpts.AllIconFName,dbOpts.IconScalingFactor);
                  end
                  else if (dbOpts.DBAutoShow = dbasIconField) then begin
                     if (dbOpts.PreferIconFieldName <> '') then begin
                        theMapDraw.PlotAnIcon(Bitmap,MyData,LatFieldName,LongFieldName,dbOpts.PreferIconFieldName,dbOpts.IconScalingFactor);
                     end
                     else if PointSymbolFieldsPresent then begin
                        TheMapDraw.LatLongDegreeToScreen(GetFieldByNameAsFloat(LatFieldName),GetFieldByNameAsFloat(LongFieldName),x,y);
                        Petmar.ScreenSymbol(Bitmap.Canvas,x,y,tDrawingSymbol(GetFieldByNameAsInteger('SYM_TYPE')),
                              GetFieldByNameAsInteger('SYM_SIZE'),GetFieldByNameAsInteger('SYM_COLOR'));
                     end;
                  end
                  else begin
                     if (HowRestrict <> resNone) or (dbOpts.dbAutoShow = dbasColorField) then begin
                        if HowRestrict in [resLineColorField] then begin
                           SetColorsFromDB(MyData);
                           PrepColors(Bitmap);
                        end
                        else if (HowRestrict in [resUseColorField,resUseColorFieldIHS]) or (dbOpts.dbAutoShow = dbasColorField) then begin
                           if Plot then begin
                              {$IfDef ExRedistrict}
                              {$Else}
                              if (RedistrictForm <> Nil) then begin
                                  SetRedistrictColor(Bitmap);
                              end
                              else {$EndIf} begin
                                 Bitmap.Canvas.Brush.Color := dbOpts.BasicColor;
                                 if RGBColorPresent or ColorPresent then Bitmap.Canvas.Brush.Color := MyData.ColorFromRGBinTable;
                                 if LatLongFieldsPresent then Bitmap.Canvas.Pen.Color := Bitmap.Canvas.Brush.Color
                                 else begin
                                    Bitmap.Canvas.Pen.Width := 1;
                                    if MDDef.ShowRegionBoundaries then Bitmap.Canvas.Pen.Color := clBlack
                                    else Bitmap.Canvas.Pen.Color := Bitmap.Canvas.Brush.Color;
                                 end;
                              end;
                           end;
                        end;
                     end;
                     {$IfDef RecordDataBasePlotProblemsEveryPoint}
                     WriteLineToDebugFile('Plot: ' + IntToStr(RecFound));
                     {$EndIf}
                     if Plot then DisplayCurrentRecordOnMap(Bitmap,(not JustLabels));
                  end;

                  if (Bitmap.Canvas.Brush.Bitmap <> Nil) then begin
                     Bitmap.Canvas.Brush.Bitmap.Free;
                     Bitmap.Canvas.Brush.Bitmap := Nil;
                  end;
                  inc(RecFound);

                  if (RecFound mod 500 = 0) and ShowSatProgress then begin
                     UpdateProgressBar(RecFound/NeedRecordCount);
                     EmpSource.Enabled := false;
                     if WantOut then break;
                  end;
                  Next;
               until eof;
           end;
        finally
        end;
        UpdateProgressBar(1.0);
        if (dbOpts.DBAutoShow = dbasTTFontSymbol) then sBitmap.Free;
        if ShowSatProgress then EndProgress;
     end;
//MessageToContinue('point 2');

     if HowRestrict in [resUseColorFieldIHS] then begin
        theMapOwner.IHSmergeOntoMap(Bitmap);
        CopyImageToBitmap(theMapOwner.Image1,BitMap);
     end;

     if DeleteOnExit then begin
        if (theMapOwner <> Nil) then theMapOwner.CurrentMapImage.Picture.Graphic := Bitmap;
        FreeAndNil(Bitmap);
     end;

     {$IfDef RecordDBPlot}
     WriteLineToDebugFile('Picture set on main map');
     {$EndIf}

     //MyData.ApplyFilter(OldFilter);

     with dbOpts do begin
        BasicColor := SymColor;
        FillColor := SaveFillColor;
     end;
   end;

   if (dbTablef = Nil) then begin
      EmpSource.Enabled := true;
   end
   else begin
      dbTablef.ShowStatus;
   end;

   NeedCentroid := false;
   PointCGMName := '';
   {$IfDef MICRODEM}
   WmDEM.StatusBar1.Panels[0].Text := '';
   {$EndIf}

   {$IfDef RecordDBPlot}
   WriteLineToDebugFile('exit at bottom of  TGISDataBase.DisplayOnMap ' + TimeToStr(Now));
   {$EndIf}
end;


procedure TGISdataBaseModule.LabelRecordsOnMap(var Bitmap : tMyBitmap);   ///var fName : PathStr);
var
   LastFName : PathStr;
   xCentroid,yCentroid,xp,
   NeedRecordCount,RecFound,
   y2,xt,yt,Count : integer;
   Lat,Long : float;
   TStr,TStr2 : ShortString;
   TextMyBitmap : tMyBitmap;

      function TryToDraw : boolean;
      var
         ys : integer;
      begin
         {$IfDef RecordEveryDataBaseLabel}
          WriteLineToDebugFile('Try to draw ' + TStr);
         {$EndIf}
         ys := -1;
         repeat
            y2 := dbypic - yt + ys;
            inc(ys,2);
            Result := PetImage.SmartTextOut(Bitmap,TextMyBitmap,xp,y2,TStr);
         until Result or (ys > yt div 2);
         {$IfDef RecordEveryDataBaseLabel}
         if Result then WriteLineToDebugFile('Worked Try to draw ' + TStr)
         else WriteLineToDebugFile('Failed Try to draw ' + TStr);
         {$EndIf}
      end;

     procedure GetLabel(aLabelField : string12; var aTStr : shortString);
     begin
        aTStr := '';
        with MyData,dbOpts do begin
            if (aLabelField <> '') then begin
               if MyData.IsFloatField(aLabelField) then begin
                  aTStr := RealToString(GetFieldByNameAsFloat(aLabelField),-18,-MDDef.MaxLabelDecimals);
               end
               else aTStr := GetFieldByNameAsString(aLabelField);
               DEMTiger.ShortenTigerName(aTStr);
            end;
        end;
     end;

begin
   {$IfDef RecordDataBaseLabels}
   WriteLineToDebugFile('TGISDataBase.LabelRecordsOnMap in ' + dbName,true);
   WriteLineToDebugFile('Label Field: ' + DBSaveOptions.LabelField);
   WriteLineToDebugFile('Second label: ' + DBSaveOptions.SecondLabelField);
   if MyData.Filtered then WriteLineToDebugFile('Filter=' + MyData.Filter);
   WriteLineToDebugFile('Records=' + IntToStr(MyData.RecordCount));
   {$EndIf}

   {$IfDef RecordFontProblems}
   WriteLineToDebugFile('TGISDataBase.LabelRecordsOnMap in ' + dbName,true);
   WriteLineToDebugFile('Gis font 1: ' + MyFontToString(DBSaveOptions.GisLabelFont1));
   WriteLineToDebugFile('Gis font 2: ' + MyFontToString(DBSaveOptions.GisLabelFont2));
   {$EndIf}

   if (MyData = Nil) then exit;
   TextMyBitmap := Nil;
   LastFName := '';
   with MyData do begin
      LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont1,Bitmap.Canvas.Font);
      NeedCentroid := true;
      if MDDef.AvoidTextOverprints then CreateBitmap(TextMyBitmap,Bitmap.Width,Bitmap.Height);
   end;

   with MyData,dbOpts do begin
      if ShowSatProgress then StartProgressAbortOption('Label DB ' + ExtractFileName(dbFullName));
      NeedRecordCount := RecordCount;
      First;
      RecFound := 0;
      EmpSource.Enabled := false;

      (*
      with theMapDraw do begin
         ScreenToLatLongDegree(0,0,Lat1,Long1);
         ScreenToLatLongDegree(MapXSize,MapYSize,Lat2,Long2);
      end;
      *)

      if MDDef.UseDBColorForFont then begin
         if AreaShapeFile(ShapeFileType) or LatLongCornersPresent then Bitmap.Canvas.Font.Color := BasicColor
         else if LineShapeFile(ShapeFileType) then Bitmap.Canvas.Font.Color := BasicColor
         else Bitmap.Canvas.Font.Color := FillColor;
      end;

      try
         Count := 0;
         repeat
            Bitmap.Canvas.Brush.Style := bsClear;

            GetLabel(LabelField,TStr);
            GetLabel(SecondLabelField,TStr2);

            yt := Bitmap.Canvas.TextHeight(TStr);
            xt := Bitmap.Canvas.TextWidth(TStr);
            inc(Count);
            if (Count >= MDDef.GISLabelSkip) then begin
               if AreaShapeFile(ShapeFileType) or LatLongCornersPresent then begin
                  if AreaShapeFile(ShapeFileType) then aShapeFile.AreaAndCentroid(TheMapDraw.CurrentMapDatum,RecNo,Lat,Long)
                  else begin
                     Long := 0.5*(GetFieldByNameAsFloat('LONG_HI') + GetFieldByNameAsFloat('LONG_LOW'));
                     Lat := 0.5*(GetFieldByNameAsFloat('LAT_HI') + GetFieldByNameAsFloat('LAT_LOW'));
                  end;
                  theMapDraw.LatLongDegreeToScreen(Lat,Long,xCentroid,yCentroid);
                  xCentroid := xCentroid - Bitmap.Canvas.TextWidth(TStr) div 2;
                  yCentroid := yCentroid - yt div 2;
                  PetImage.SmartTextOut(Bitmap,TextMyBitmap,xCentroid,yCentroid,TStr);
               end
               else begin
                  if ValidScreenPositionFromTable(dbxpic,dbypic) then begin
                     {$IfDef RecordEveryDataBaseLabel}
                     WriteLineToDebugFile('Label: ' + TStr + '       Second: ' + TStr2,true);
                     WriteLineToDebugFile('Plot a x=' + IntToStr(dbxpic) + '   y=' + IntToStr(dbypic));
                    {$EndIf}
                    if (TStr2 <> '') then begin
                       {$IfDef RecordEveryDataBaseLabel}
                        WriteLineToDebugFile('Calling PetImage.SmartTextOut');
                       {$EndIf}
                       LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont2,Bitmap.Canvas.Font);
                       PetImage.SmartTextOut(Bitmap,TextMyBitmap,dbxpic+4,dbypic + 2,TStr2);
                       LoadMyFontIntoWindowsFont(dbOpts.GisLabelFont1,Bitmap.Canvas.Font);
                    end;

                    xp := dbxpic+4;
                    if not TryToDraw then begin
                       xp := dbxpic-xt-4;
                       TryToDraw;
                    end;
                 end;
               end;
               Count := 0;
            end;
            inc(RecFound);
            if (RecFound mod 1000 = 0) and ShowSatProgress then UpdateProgressBar(RecFound/NeedRecordCount);
            if WantOut then break;
            Next;
         until eof;
      finally
      end;
      EndProgress;
   end;

   (*
   if Savelayer then begin
      {$IfDef RecordDataBaseLabels}
      WriteLineToDebugFile('No valid position');
      {$EndIf}
      fName := NextFileNumber(MDTempDir, 'db_text', OverlayFExt);
      PetImage.SaveBitmap(Bitmap,fName);
      {$IfDef RecordDataBaseLabels}
      WriteLineToDebugFile('Save text layer to ' + fName);
      {$EndIf}
   end
   else begin
      fName := '';
   end;
   *)

   EmpSource.Enabled := true;
   FreeAndNil(TextMyBitmap);
   NeedCentroid := false;
   PointCGMName := '';
   {$IfDef MICRODEM}
   WmDEM.StatusBar1.Panels[0].Text := '';
   {$EndIf}
end;


procedure TGISdataBaseModule.QueryBox(x1,y1,x2,y2 : integer; DisplayOnTable : boolean);
var
   Lat1,Long1,Lat2,Long2 : float;
begin
   with theMapDraw,MapCorners do begin
      ScreenToLatLongDegree(x1,y1,Lat1,Long1);
      ScreenToLatLongDegree(x2,y2,Lat2,Long2);
      {$IfDef RecordDataBaseProblems}
      WriteLineToDebugFile('TGISDataBase.QueryBox',true);
      WriteLineToDebugFile('   '+ LatLongDegreeToString(Lat1,Long1,DecDegrees));
      WriteLineToDebugFile('   '+ LatLongDegreeToString(Lat2,Long2,DecDegrees));
      {$EndIf}
      QueryGeoBox(Lat1,Long1,Lat2,Long2,DisplayOnTable);
   end;
end;


procedure TGISdataBaseModule.LimitDBtoMapArea;
begin
   {$IfDef NoLimitsToMapAreaForDB}
     {if (theMapDraw.MapCorners.CurScrLongLow < 0) or (theMapDraw.MapCorners.CurScrLongHigh < 0) then} exit;
   {$EndIf}
   if (theMapDraw <> Nil) then with theMapDraw,MapCorners do begin
      QueryGeoBox(CurScrLatHigh,CurScrLongLow,CurScrLatLow,CurScrLongHigh,true);
   end;
end;

procedure TGISdataBaseModule.PlotSingleMonth(Month: integer);
begin
   if MyData.FieldExists('MONTH') then begin
      MyData.ApplyFilter('MONTH=' + IntegerToString(Month));
      RedrawLayerOnMap;
   end;
end;


procedure TGISdataBaseModule.RedrawLayerOnMap;
var
   NoRedraw : boolean;
begin
   if (theMapDraw <> Nil) and (DataBaseNumber <> 0) and dbPlotted then begin
      NoRedraw := (theMapOwner <> Nil) and theMapOwner.ClosingMapNow;
      theMapDraw.ClearGISLayer(DataBaseNumber);
      if (not NoRedraw) then theMapOwner.DoFastMapRedraw;
      if (theMapDraw.CurrentFansTable = DataBaseNumber) then theMapDraw.CurrentFansTable := 0;
   end;
end;



