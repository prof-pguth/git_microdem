{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}
{                                   }
{ include file for demmapdraw       }
{___________________________________}



{$IfDef ExTiger}
{$Else}
   procedure TMapDraw.DrawFullTigerCoverage(Bitmap : tMyBitmap; RoadsOnly : boolean = false; AllRoads : boolean = false; DrawMarginalia : boolean = true; RoadMaskWidth : integer = -1; StreamsOnly : boolean = false);
   var
      TigerNames : tStringList;
      i    : integer;
      SavedTigerLayer : PathStr;
      TigerOverlay : tMyBitmap;
      SaveRoadAutoAppear : boolean;
   begin
      if (ScreenPixelSize > 1000) then exit;
      {$If Defined(RecordTIGER)} WriteLineToDebugFile('TMapDraw.FullTigerCoverage in for ' + BaseTitle + ' TigerOverlayFName=' + TigerOverlayFName); {$EndIf}
      ShowHourglassCursor;
      CreateBitmap(TigerOverlay,Bitmap.Width,Bitmap.Height);
      if (DEMonMap = 0) then SavedTigerLayer := ''
      else SavedTigerLayer := TPath.Combine(ProjectDir, 'Map_Layers' + PathDelim + ptTrim(DEMGlb[DEMonMap].AreaName) + '_tiger' + OverlayFExt);
      {$IfDef RecordTIGER} WriteLineToDebugFile('Seeking SavedTigerLayer Overlay: ' + SavedTigerLayer); {$EndIf}

      if LoadExistingBaseMap(TigerOverlay,SavedTigerLayer) then begin
         {$IfDef RecordTIGER} WriteLineToDebugFile('SavedTigerLayer Overlay found: ' + SavedTigerLayer); {$EndIf}
         SaveLayerBitmap(TigerOverlay,TigerOverlayFName);
      end
      else if (TigerOverlayFName <> '') and FileExists(TigerOverlayFName) then begin
         {$IfDef RecordTIGER} WriteLineToDebugFile('Temp Overlay found ' + TigerOverlayFName); {$EndIf}
         TigerOverlay.LoadFromFile(TigerOverlayFName);
      end
      else begin
         {$If Defined(RecordTiming) or Defined(TigerTiming) or Defined(RecordTIGER)} WriteLineToDebugFile('TMapDraw.FullTigerCoverage drawing for ' + BaseTitle + ' TigerOverlayFName=' + TigerOverlayFName); {$EndIf}
         FindTIGERinBox(MapCorners.BoundBoxGeo,TigerNames);
         {$IfDef RecordTIGER} WriteLineToDebugFile('TMapDraw.FullTigerCoverage; counties on map: ' + IntToStr(TigerNames.Count)); {$EndIf}
         SaveRoadAutoAppear := MDDef.TigrDef.AutoAppear;
         if AllRoads then MDDef.TigrDef.AutoAppear := false;
         if (TigerNames.Count > 0) then begin
            if (TigerNames.Count <= MDdef.TigrDef.MaxAutoTigerCounties) or AllTIGEROK or AnswerIsYes('Map includes ' + IntToStr(TigerNames.Count) + ' counties;' + MessLineBreak + 'Continue with Tiger overlay') then begin
               if (TigerNames.Count > MDdef.TigrDef.MaxAutoTigerCounties) then AllTigerOK := true;
               //StartProgress('TIGER counties');
               StartThreadTimers('TIGER counties',0,true);

               for i := 0 to pred(TigerNames.Count) do begin
                  //UpdateProgressBar(i/TigerNames.Count);
                  ThreadTimers.OverallGauge9.Progress := round(100 * i/TigerNames.Count);
                  {$IfDef RecordTIGERFull} WriteLineToDebugFile(' ' + TigerNames.Strings[i]); {$EndIf}
                  //MessageToContinue('done');
                  //if (wmDEM <> Nil) then wmdem.SetPanelText(0, 'Tiger: ' + IntToStr(succ(i)) + '/' + IntToStr(TigerNames.Count));
                  if FileExists(TigerNames.Strings[i]) then QuietPlotTIGERShapeFileOnMap(TigerNames.Strings[i],TigerOverlay,RoadsOnly,RoadMaskWidth,StreamsOnly);
                  if WantOut then break;
               end;
               //EndProgress;
               EndThreadTimers;
               //i := TigerNames.Count;
               TigerNames.Free;
               MDDef.TigrDef.AutoAppear := SaveRoadAutoAppear;
               TigerOverlayFName := NextFileNumber(MDTempDir, 'tiger_ovelay', OverlayFExt);
               PetImage.SaveBitmap(TigerOverlay,TigerOverlayFName);
               if (DEMonMap <> 0) and FullDEMMap and (not FileExists(SavedTigerLayer)) then begin
                  {$IfDef Defined(RecordTiming) or Defined(TigerTiming)} WriteLineToDebugFile('Save SavedTigerLayer Overlay: ' + SavedTigerLayer); {$EndIf}
                  TigerOverlay.SaveToFile(SavedTigerLayer);
               end;
            end;
         end;
         if (wmDEM <> Nil) then wmdem.StatusBar1.Panels[0].Text := '';
         {$If Defined(RecordTiming) or Defined(TigerTiming)} WriteLineToDebugFile('TMapDraw.FullTigerCoverage out for ' + BaseTitle + ' TigerOverlayFName=' + TigerOverlayFName + ' files=' + IntToStr(i)); {$EndIf}
      end;
      DrawOverlayNoDelete(Bitmap,TigerOverlay);
      {$If Defined(RecordTiger)} WriteLineToDebugFile('TMapDraw.FullTigerCoverage out for ' + BaseTitle + ' TigerOverlayFName=' + TigerOverlayFName + ' files=' + IntToStr(i)); {$EndIf}
   end;
{$EndIf}


{$IfDef ExMilIcons}
{$Else}
   procedure TMapDraw.PlotMilIconsOnBitmap(Bitmap : tMyBitmap; fName : PathStr);
   var
      MilIcon : tMilIcon;
      WeaponsTable : tMyData;
      LastX,LastY : integer;
   begin
      WeaponsTable := tMyData.Create(fName);
      while not WeaponsTable.Eof do begin
         GetMilIcon(WeaponsTable,MilIcon);
         LatLongDegreeToScreen(MilIcon.Lat,MilIcon.Long,LastX,LastY);
         Bitmap.Canvas.CopyMode := cmSrcAnd;
         DrawMilIcon(Bitmap,LastX,LastY,MilIcon);
         WeaponsTable.Next;
      end;
      WeaponsTable.Destroy;
   end;
{$EndIf}


{$IfDef ExGazetteer}
{$Else}

   procedure TMapDraw.GazetteerLegend(var Bitmap : tMyBitmap);
   var
      Table,GazColors : tMyData;
      yi : integer;
      PlotColor : tPlatformColor;
      PlotSize : byte;
      PlotSym : tDrawingSymbol;
      NameStr : ShortString;
   begin
      {$IfDef VCL}
         if (not FileExists(LastGazFile)) then exit;
         Table := tMyData.Create(LastGazFile);
         SetUpGazFile(GazColors,NameStr,true);
         ShowHourglassCursor;
         CreateBitmap(Bitmap,150,25*GazColors.FiltRecsInDB);
         yi := 0;
         while not GazColors.EOF do  begin
            GazColors.DefinePointSymbol(PlotSym,PlotSize,PlotColor);
            GazColors.DefineFontFromTable(Bitmap.Canvas.Font);
            Petmar.ScreenSymbol(Bitmap.Canvas,10,yi+5,PlotSym,PlotSize,PlotColor);
            Bitmap.Canvas.TextOut(25,yi,GazColors.GetFieldByNameAsString('NAME'));
            inc(yi,25);
            GazColors.Next
         end;
         Table.Destroy;
         ShowDefaultCursor;
         PetImage_form.DisplayBitmap(Bitmap,'Gazetteer Legend');
         Bitmap.Free;
      {$EndIf}
   end;



   procedure TMapDraw.LabelGazetteerFeatures(var inBitmap : tMyBitmap);
   var
      GazColors : tMyData;
      xi,yi,IconScale : integer;
      Lat1,Long1,Lat2,Long2 : float64;
      FeatureName : shortString;
      PlotColor : tPlatformColor;
      PlotSize : byte;
      PlotSym : tDrawingSymbol;
      OldFilter, IconName,
      NameStr : ShortString;
      GazOverlay,TextMyBitmap : tMyBitmap;
      TempDB : boolean;
   begin
      {$IfDef RecordGazLabel} WriteLineToDebugFile('TMapDraw.LabelGazetteerFeatures in'); {$EndIf}
      if (not FileExists(LastGazFile)) then begin
         {$IfDef RecordGazLabel} WriteLineToDebugFile('Missing LastGazFile=' + LastGazFile + ' gazdb=' + IntToStr(MapGazDB)); {$EndIf}
         exit;
      end;
      TempDB := (MapGazDB = 0);
      if TempDB then begin
         if not OpenNumberedGISDataBase(MapGazDB, LastGazFile,false,false,Nil) then exit;
      end;

      CreateBitmap(GazOverlay,inBitmap.Width,inBitmap.Height);
      if (GazOverlayFName <> '') then begin
         GazOverlay.LoadFromFile(GazOverlayFName);
      end
      else begin
         {$IfDef RecordGazLabel} WriteLineToDebugFile('Gaz labels from ' + LastGazFile); {$EndIf}
         SetUpGazFile(GazColors,NameStr);
         ShowHourglassCursor;
         ScreenToLatLongDegree(0,0,Lat2,Long1);
         ScreenToLatLongDegree(MapXSize,MapYSize,Lat1,Long2);
         {$IfDef RecordGazLabel} WriteLineToDebugFile('Record count: ' + IntToStr(GISdb[MapGazDB].MyData.FiltRecsInDB)); {$EndIf}
         TextMyBitmap := Nil;

         if MDDef.AvoidTextOverprints then CreateBitmap(TextMyBitmap,inBitmap.Width,inBitmap.Height);
         OldFilter := GISdb[MapGazDB].dbOpts.MainFilter;
         while not GazColors.EOF do begin
             GazColors.DefinePointSymbol(PlotSym,PlotSize,PlotColor);
             GazColors.DefineFontFromTable(GazOverlay.Canvas.Font);
             IconName := GazColors.GetFieldByNameAsString('ICON');
             IconScale := GazColors.GetFieldByNameAsInteger('ICON_SCALE');

             GISdb[MapGazDB].dbOpts.MainFilter := GazColors.GetFieldByNameAsString('FILTER');
             if (OldFilter <> '') then GISdb[MapGazDB].dbOpts.MainFilter := GISdb[MapGazDB].dbOpts.MainFilter + ' AND ' + OldFilter;

             GISdb[MapGazDB].AssembleGISFilter;
             GISdb[MapGazDB].EmpSource.Enabled := false;

             {$IfDef RecordGazLabel} WriteLineToDebugFile('Gaz Filter: ' + GISdb[MapGazDB].MyData.Filter+ '  Filtered record count: ' + IntToStr(GISdb[MapGazDB].MyData.FiltRecsInDB)); {$EndIf}

             while not GISdb[MapGazDB].MyData.Eof do begin
                if GISdb[MapGazDB].ValidLatLongFromTable(Lat1,Long1) then begin
                   LatLongDegreeToScreen(Lat1,Long1,xi,yi);
                   if OnScreen(xi,yi) then begin
                      FeatureName := GISdb[MapGazDB].MyData.GetFieldByNameAsString(NameStr);
                      AdjustGazFeatureName(FeatureName);
                      if (IconName <> '') then begin
                         PlotAnIcon(GazOverlay,Lat1,Long1,IconName,IconScale);
                      end
                      else Petmar.ScreenSymbol(GazOverlay.Canvas,xi,yi,PlotSym,PlotSize,PlotColor);
                      if MDDef.LabelGazOnMap then PetImage.SmartTextOut(GazOverlay,TextMyBitmap,xi+5,yi+5,FeatureName);
                   end;
                end;
                GISdb[MapGazDB].MyData.Next;
             end;
            GazColors.Next
         end;
         if MDDef.AvoidTextOverprints then TextMyBitmap.Free;
         GazOverlayFName := NextFileNumber(MDTempDir, 'Gaz_overlay', OverlayFExt);
         GazOverlay.SaveToFile(GazOverlayFName);
         GISdb[MapGazDB].dbOpts.MainFilter := OldFilter;
         GISdb[MapGazDB].AssembleGISFilter;
         GISdb[MapGazDB].EmpSource.Enabled := true;
      end;
      DrawAndDeleteOverlay(inBitmap,GazOverlay,100,0,0,true);
      if TempDB then begin
         CloseAndNilNumberedDB(MapGazDB);
      end;
      {$IfDef RecordGazLabel} WriteLineToDebugFile('TMapDraw.LabelGazetteerFeatures out'); {$EndIf}
   end;

{$EndIf}


         function TMapDraw.MapSymbolAtLatLongDegree(Canvas : tCanvas; Lat,Long : float64; Symbol : tFullSymbolDeclaration) : boolean;
         var
            x,y : integer;
         begin
            LatLongDegreetoScreen(Lat,Long,X,Y);
            Result := OnScreen(x,y);
            if Result then Petmar.ScreenSymbol(Canvas,X,Y,Symbol);
         end;


         function TMapDraw.MapSymbolAtLatLongDegree(Canvas : tCanvas; Lat,Long : float64; Sym : tDrawingSymbol; Size : integer; Color : tPlatformColor) : boolean;
         var
            x,y : integer;
         begin
            LatLongDegreetoScreen(Lat,Long,X,Y);
            Result := OnScreen(x,y);
            if Result then Petmar.ScreenSymbol(Canvas,X,Y,Sym,Size,Color);
         end;


         (*
         procedure TMapDraw.DrawRoute(Bitmap : tMyBitmap; StreamProfileResults : tStringList);
         var
            FirstPoint,j : integer;
            Lat,Long : float64;
         begin
           if (StreamProfileResults <> Nil) and (StreamProfileResults.Count > 3) then begin
              FirstPoint := 2;
              for j := FirstPoint to pred(StreamProfileResults.Count) do begin
                  ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,j,Lat,Long);
                  LatLongDegreetoScreen(Lat,Long,MapScreenX1,MapScreenY1);
                  if j = FirstPoint then Bitmap.Canvas.MoveTo(MapScreenX1,MapScreenY1)
                  else begin
                     Bitmap.Canvas.Pen.Color := clRed;
                     Bitmap.Canvas.Pen.Width := 2;
                     Bitmap.Canvas.LineTo(MapScreenX1,MapScreenY1)
                  end;
              end;
           end;
         end;
         *)

         procedure TMapDraw.ComputeDatumShifts(Canvas : tCanvas; Lat,Long : float64; var TotalShiftUTM,TotalShiftGeo : float64; Mark : tMarkShift = msNone);
         var
            eastingshiftUTM,northingshiftUTM,
            eastingshiftGeo,northingshiftGeo,
            xutm1,yutm1,xutm2,yutm2,Bearing,
            lat1,long1,lat2,long2 : float64;
            x,y : integer;
            OriginalUTMZone : byte;

            procedure FindDifferenceInGeoCoords;
            begin
               //if the same lat/long were reported on the second datum where would it be on the current datum
               MolodenskiyTransformation(Lat,Long,Lat1,Long1,SecondaryMapProj,PrimMapProj);
               {$If Defined(RecordDatum) or Defined(RecordDatumShift)} WriteLineToDebugFile('Geo Primary datum: ' + LatLongDegreeToString(Lat,Long) + '  Secondary datum: ' + LatLongDegreeToString(Lat1,Long1));    {$EndIf}             //get distance between the two
               VincentyCalculateDistanceBearing(Lat,Long,Lat1,Long1,TotalShiftGeo,Bearing);
               EastingShiftGeo := TotalShiftGeo * CosDeg(Bearing);
               NorthingShiftGeo := TotalShiftGeo * SinDeg(Bearing);
               if (Mark in [msLatLong,msBoth]) then Canvas.TextOut(x+2,y+2,'Geo: ' + RealToString(TotalShiftGeo,-8,1));
            end;

            procedure FindDifferenceInUTMCoords;
            begin
               //find utm of the current point
               PrimMapProj.ForwardProjectDegrees(Lat,Long,xutm1,yutm1);
               //use that UTM, but on the other datum, and find lat/long on other datum
               SecondaryMapProj.UTMtoLatLongDegree(xutm1,yutm1,Lat2,Long2);
               //shift that lat,long point to find where it is on the primary datum
               MolodenskiyTransformation(Lat2,Long2,Lat1,Long1,SecondaryMapProj,PrimMapProj);

               PrimMapProj.ForwardProjectDegrees(Lat1,Long1,xutm2,yutm2);
               EastingShiftUTM := abs(xutm2 - xutm1);
               NorthingShiftUTM := abs(yutm2 - yutm1);
               TotalShiftUTM := sqrt(sqr(EastingShiftUTM ) + sqr(NorthingShiftUTM));
               if (Mark in [msUTM,msBoth]) then Canvas.TextOut(x+2,y-12,'UTM: ' + RealToString(TotalShiftUTM,-8,1));
            end;


         begin
            if SecondaryMapProj = nil then begin
               MessageToContinue('Secondary projection not set');
               exit;
            end;
            {$If Defined(RecordDatum) or Defined(RecordDatumShift)} WriteLineToDebugFile('TMapForm.ComputeDatumShifts, ' + LatLongDegreeToString(Lat,Long) + '  ' + PrimaryMapDatum.h_DatumCode + '  ' +SecondaryMapDatum.h_DatumCode);            {$EndIf}
            LatLongDegreeToScreen(Lat,Long,x,y);
            {$If Defined(RecordDatumShift)} WriteLineToDebugFile('Screen location: ' + IntToStr(x) + '/' + IntToStr(y)); {$EndIf}
            OriginalUTMZone := PrimMapProj.projUTMZone;
            AdjustProjectionForUTMZone('cds',PrimMapProj.h_DatumCode,Lat,Long);
            if (Mark <> msNone) then Petmar.ScreenSymbol(Canvas,x,y,Cross,3,claRed);
            FindDifferenceInGeoCoords;
            FindDifferenceInUTMCoords;

            if ValidDB(DatumShiftDB) then begin
               GISDB[DatumShiftDB].MyData.Insert;
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('LAT',Lat);
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('LONG',Long);
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsInteger('UTM_ZONE',PrimMapProj.projUTMZone);
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsString('DATUM_1',PrimMapProj.h_DatumCode);
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsString('DATUM_2',SecondaryMapProj.h_DatumCode);
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsString('MGRS',PrimMapProj.LatLongToMGRS(Lat,Long));
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('X_UTM',xutm1);
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('Y_UTM',yutm1);

               GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('DIFF_GEO',TotalShiftGeo);
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('DIFF_GEO_X',EastingShiftGeo);
               GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('DIFF_GEO_Y',NorthingShiftGeo);
               {$IfDef NoUTMDiffsDatumShift}
               {$Else}
                  GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('DIFF_UTM',TotalShiftUTM);
                  GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('DIFF_UTM_X',EastingShiftUTM);
                  GISDB[DatumShiftDB].MyData.SetFieldByNameAsFloat('DIFF_UTM_Y',NorthingShiftUTM);
               {$EndIf}

               GISDB[DatumShiftDB].MyData.Post;
               GISDB[DatumShiftDB].dbTablef.ShowStatus;
            end;
            AdjustProjectionForUTMZone('/cds',PrimMapProj.h_DatumCode,OriginalUTMZone,HemiFromLat(Lat));
         end;


         function TMapDraw.DrawScaleBarOnBitmap(PixelSize : float64 = -1) : tMyBitmap;
         var
            TFloat,Bar,LenKM,Lat,Long,h,k,x1,y1,x2,y2 : float64;
            BoxLong,i,Start,EndX,y : integer;
            Prime : boolean;
            DistStr : shortstring;
         begin
            Result := Nil;
            if (PixelSize > 0) then begin
               LenKM := 0.001 * MapXSize * PixelSize;
            end
            else if (VectorIndex = 0) then begin
               LenKM := 0.001 * MapXSize * ScreenPixelSize;
            end
            else with PrimMapProj do begin
               if (not PrimMapProj.EllipsoidalProjection) then exit;
               ScreenToLatLongDegree(MapXSize div 2, MapYSize div 2,Lat,Long);
               GetMapScaleFactor(Lat,Long,h,k,Prime);
               ScreenToLatLongDegree(MapXSize div 2, MapYSize div 2,Lat,Long);
               ForwardProjectDegrees(Lat,Long,x1,y1);
               ScreenToLatLongDegree(pred(MapXSize), MapYSize div 2,Lat,Long);
               ForwardProjectDegrees(Lat,Long,x2,y2);
               LenKM := 2 * 0.001 * abs(x2-x1) / k;
            end;

                 if (LenKM > 7500) or (LenKM < 0.001) then exit;
                 if MDDef.EnglishDistanceUnits = disEnglish then LenKM := LenKM * 0.62137
                 else if MDDef.EnglishDistanceUnits = disNautical then LenKM := LenKM * 0.539956803;

                 if (LenKM > 7500) or (LenKM < 0.001) then exit;

                 if LenKM >= 5000 then Bar := 2500
                 else if LenKM >= 2500 then Bar := 1000
                 else if LenKM >= 1000 then Bar := 500
                 else if LenKM >= 500 then Bar := 250
                 else if LenKM >= 200 then Bar := 100
                 else if LenKM >= 100 then Bar := 50
                 else if LenKM >= 50 then Bar := 25
                 else if LenKM >= 20 then Bar := 10
                 else if LenKM >= 10 then Bar := 5
                 else if LenKM >= 4 then Bar := 2.5
                 else if LenKM >= 2 then Bar := 1
                 else if LenKM >= 1 then Bar := 0.5
                 else if LenKM >= 0.5 then Bar := 0.25
                 else if LenKM >= 0.2 then Bar := 0.1
                 else if LenKM >= 0.1 then Bar := 0.05
                 else if LenKM >= 0.05 then Bar := 0.01
                 else if LenKM >= 0.01 then Bar := 0.005
                 else Bar := 0.001;

               Bar := 0.2 * Bar / LenKM * MapXSize;
               TFloat := 5 * Bar * LenKM / MapXSize;

               if (MDDef.EnglishDistanceUnits=disEnglish) then DistStr := RealToString(TFloat,-12,-4) + ' miles'
               else if (MDDef.EnglishDistanceUnits=disNautical) then DistStr := RealToString(TFloat,-12,-4) + ' nm'
               else begin
                  if (TFloat > 1) then DistStr := RealToString(TFloat,-12,-4) + ' km'
                  else DistStr := RealToString(1000 * TFloat,-12,-2) + ' m';
               end;
               Result := tMyBitmap.Create;
               Result.Canvas.Font.Size := 14;
               Result.Canvas.Font.Style := [fsBold];
               BoxLong := round(5*Bar)+Result.Canvas.TextWidth(DistStr)+15;
               Result.Height := 30;
               Result.Width := BoxLong;
               with Result.Canvas do begin
                  Pen.Width := 1;
                  Pen.Color := clBlack;
                  Brush.Color := clNearWhite;
                  Brush.Style := bsSolid;
                  Font.Color := clBlack;
                  Rectangle(0,0,Result.Width,Result.Height);
                  Rectangle(5,9,round(5*Bar)+5,24);

                  Brush.Color := clBlack;
                  for i := 1 to 5 do begin
                     Start := round(pred(i) * Bar);
                     EndX :=  round(i * Bar);
                     if odd(i) then y := 9 else y := 16;
                     Rectangle(5+Start,Y,EndX+5,Y+8);
                  end;
                  Brush.Style := bsClear;
                  TextOut(EndX+10,5,DistStr);
               end;
         end;


         procedure TMapDraw.DrawScaleBar(Canvas : tCanvas; xstart,Ystart : integer);
         var
            Bitmap : tMyBitmap;
         begin
            {$IfDef RecordMapDraw} WriteLineToDebugFile('tMapDraw.DrawScaleBar ' + IntToStr(xstart) + '/' + IntToStr(YStart)); {$EndIf}
            if (xstart < 0) then XStart := 0;
            if (ystart < 0) or (ystart > (MapYSize-50)) then ystart := MapYSize - 50;
            Bitmap := DrawScaleBarOnBitmap;
            Canvas.Draw(xstart,ystart,Bitmap);
            Bitmap.Free;
         end;



         procedure TMapDraw.NewDEMSatelliteMerge(Bitmap : tMyBitmap);
         var
            NewBitmap : tMyBitmap;
         begin
            {$IfDef RecordMapMerge} WriteLineToDebugFile('TMapDraw.NewDEMSatelliteMerge ' + MapSizeString); {$EndIf}
            if (abs(MinMapElev-MaxMapElev) < 0.001) then ScaleMapElevationsToDEM;
            if (TerrainShadowsDEM = 0) then TerrainShadowsDEM := DEMonMap;

            if (MDDef.GrayscaleMerges) then begin
               {$IfDef RecordMapMerge} WriteLineToDebugFile('Make grayscale'); {$EndIf}
               MakeTheBitmapGrayScale(Bitmap);
            end;

            if (MapMerge in [mmElevation,mmSlope,mmAspect]) then begin
               ShowHourglassCursor;
               CloneBitmap(Bitmap,NewBitmap);
               if (MapMerge in [mmSlope,mmAspect]) then DrawSlopeMap(NewBitmap)
               else DrawElevationMapForMerge(NewBitMap);
               IHSMergePurgeBitmaps(Bitmap,NewBitmap);
               ShowDefaultCursor;
               {$IfDef RecordMapMerge} WriteLineToDebugFile('elev/slope merge over ' + MapSizeString); {$EndIf}
            end;
         end;


         procedure tMapDraw.DrawOrientedLine(Bitmap : tMyBitmap; StartLat,StartLong,Bearing : float64);
         var
            IncrSize,Dist,Lat,Long,Az,FullDist : float64;
            x,y  : integer;
            Done : boolean;
         begin
            IncrSize := 5 * ScreenPixelSize;
            Dist := 0;
            LatLongDegreeToScreen(StartLat,StartLong,x,y);
            Bitmap.Canvas.MoveTo(x,y);
            Done := false;
            Lat := 0.5 * (MapCorners.BoundBoxGeo.ymin + MapCorners.BoundBoxGeo.ymax);
            Long := 0.5 * (MapCorners.BoundBoxGeo.xmin + MapCorners.BoundBoxGeo.xmax);

            VincentyCalculateDistanceBearing(StartLat,StartLong,Lat,Long,FullDist,Az);
            repeat
               Dist := Dist + IncrSize;
               VincentyPointAtDistanceBearing(StartLat,StartLong,Dist,Bearing, Lat,Long);
               LatLongDegreeToScreen(Lat,Long,x,y);
               if not OnScreen(x,y) then Done := Dist > FullDist;
               Bitmap.Canvas.LineTo(x,y);
            until Done;
         end;





         procedure TMapDraw.OverlaySRTMWaterBodies(Bitmap : tMyBitmap; WaterMask : boolean = false);

               function SRTMWaterBodyFileName(Lat,Long : integer) : PathStr;
               //these names came for the CD's with the data from the early 2000's
               //names for the files at https://earthexplorer.usgs.gov/ are different from these, w087n41n.shp
               //https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-water-body?qt-science_center_objects=0#qt-science_center_objects
               const
                  ContinentLetters : array[1..6] of char = ('n','s','a','e','f','1');
               var
                  i : integer;
                  TStr1,TStr2 : Shortstring;
               begin
                  if (Long < 0) then TStr2 := 'west\swbdwest\w' else TStr2 := 'east\swbdeast\e';
                  TStr2 := TStr2  + IntegerToString(abs(Long),3);
                  if (Lat < 0) then TStr1 := 's' else TStr1 := 'n';
                  TStr1 := MainMapData + 'srtm-swbd\' + TStr2 + TStr1 + IntegerToString(abs(lat),2);
                  for i := 1 to Length(TStr1) do if TStr1[i] = ' ' then TStr1[i] := '0';
                  for i := 1 to 6 do begin
                     Result := TStr1 + ContinentLetters[i] + '.zip';
                     if FileExists(Result) then exit;
                  end;
                  Result := '';
               end;


         var
            fName : PathStr;
            Dir   : DirStr;
            bName : NameStr;
            Ext   : ExtStr;
            Lat,Long,
            Lat1,Long1,Lat2,Long2 : integer;
         begin
            {$IfDef ExZip}
            {$Else}
               HighLightDBOnWorld := false;
               Lat1 := trunc(MapCorners.BoundBoxGeo.ymin + 0.01);
               if (Lat1 < 0) then dec(lat1);
               Lat2 := succ(trunc(MapCorners.BoundBoxGeo.ymax - 0.025));
               if (Lat2 < 0) then dec(lat2);
               Long1 := trunc(MapCorners.BoundBoxGeo.xmin + 0.01);
               if (Long1 < 0) then dec(Long1);
               Long2 := succ(trunc(MapCorners.BoundBoxGeo.xmax - 0.025));
               if (Long2 < 0) then dec(Long2);

               if ShowSatProgress then StartProgress('Water');
               for Long := Long1 to pred(Long2) do begin
                  if ShowSatProgress then UpdateProgressBar((Long - Long1) / (Long2 - Long1));
                  for Lat := Lat1 to pred(Lat2) do begin
                     fName := SRTMWaterBodyFileName(Lat,Long);
                     if (fName <> '')  then begin
                        if FileExists(fName) and ZipMasterUnzip(fName,MDTempDir) then begin
                           FSplit(Fname,dir,bname,ext);
                           fName := MDTempDir + bName + '.shp';
                           QuickShapeFileDisplay(Bitmap,fName,false,ConvertPlatformColorToTColor(MDDef.WaterColor));
                           Petmar.DeleteMultipleFiles(MDTempDir,bName + '*');
                        end;
                     end;
                  end;
               end;
               HighLightDBOnWorld := true;
               if ShowSatProgress then EndProgress;
            {$EndIf}
         end;


         procedure tMapDraw.PlotMapLibrary(var Bitmap : tMyBitmap);

               procedure PlotIntegratedDataBaseSingleSeries(Bitmap : tMyBitmap; Ext : ShortString; Fill : boolean = false);
               var
                  Lat1,Long1,Lat2,Long2 : float64;
                  xc,yc : integer;
                  SeriesTable : tMyData;
                  Series : shortstring;
               begin
                  OpenIndexDataOnline;
                  Bitmap.Canvas.Pen.Width := 2;
                  if Fill then begin
                     Bitmap.Canvas.Brush.Color := clLime;
                     Bitmap.Canvas.Brush.Style := bsSolid;
                  end
                  else begin
                     Bitmap.Canvas.Brush.Style := bsClear;
                  end;

                  OpenIndexedSeriesTable(SeriesTable);
                  repeat
                     Series := SeriesTable.GetFieldByNameAsString('SERIES');
                     if (UpperCase(SeriesTable.GetFieldByNameAsString('USE')) = 'Y') then begin
                        Bitmap.Canvas.Pen.Color := SeriesTable.GetFieldByNameAsInteger('COLOR');
                        IndexDataOnline.ApplyFilter('SERIES = ' + QuotedStr(Series));
                        {$If Defined(RecordOverlays) or Defined(RecordMapIndex)} WriteLineToDebugFile('Use Series=' + Series +  '  recs=' + IntToStr(IndexDataOnline.FiltRecsInDB)); {$EndIf}
                        while (not IndexDataOnline.eof) do begin
                           Lat2 := IndexDataOnline.GetFieldByNameAsFloat('LAT_HI');
                           Lat1 := IndexDataOnline.GetFieldByNameAsFloat('LAT_LOW');
                           Long2 := IndexDataOnline.GetFieldByNameAsFloat('LONG_HI');
                           Long1 := IndexDataOnline.GetFieldByNameAsFloat('LONG_LOW');
                           LatLongBox(Bitmap,Lat1,Long1,Lat2,Long2,10,xc,yc,True);
                           IndexDataOnline.Next;
                        end;
                     end
                     else begin
                        {$If Defined(RecordOverlays) or Defined(RecordMapIndex)} WriteLineToDebugFile('Not using Series=' + Series +  '  recs=' + IntToStr(IndexDataOnline.FiltRecsInDB)); {$EndIf}
                     end;
                     SeriesTable.Next;
                  until SeriesTable.eof;
                  SeriesTable.Destroy;
                  CloseIndexDataOnline;
               end;

         begin
            {$If Defined(RecordOverlays) or Defined(RecordMapIndex)} WriteLineToDebugFile('tMapDraw.PlotMapLibrary in'); {$EndIf}
            PlotIntegratedDataBaseSingleSeries(Bitmap,'');
            {$If Defined(RecordOverlays) or Defined(RecordMapIndex)} WriteLineToDebugFile('tMapDraw.PlotMapLibrary out'); {$EndIf}
         end;

         procedure tMapDraw.OutlineSectors(Canvas : tCanvas);
         var
            Table1 : tMyData;
            x,y : integer;
            Lat,Long,Lat2,Long2,Azimuth,Range : float64;
         begin
            if (SectorOutlines <> '') then begin
               Table1 := tMyData.Create(SectorOutlines);
               while not Table1.eof do begin
                  if Table1.GetFieldByNameAsString('USE') = 'Y' then begin
                     Canvas.Pen.Width := Table1.GetFieldByNameAsInteger('LINE_WIDTH');
                     Canvas.Pen.Color := Table1.GetFieldByNameAsInteger('LINE_COLOR');
                     Lat := Table1.GetFieldByNameAsFloat('LAT');
                     Long := Table1.GetFieldByNameAsFloat('LONG');
                     Range := Table1.GetFieldByNameAsFloat('RANGE');
                     if Table1.GetFieldByNameAsString('CIRCLE') = 'Y' then begin
                        DrawArc(Canvas,Lat,Long,Range,0,360,Canvas.Pen.Color,Canvas.Pen.Width);
                     end
                     else if Table1.GetFieldByNameAsString('VECTOR') = 'Y' then begin
                        Azimuth := Table1.GetFieldByNameAsFloat('AZIMUTH');
                        VincentyPointAtDistanceBearing(Lat,Long,Range,Azimuth,Lat2,Long2);
                        LatLongDegreeToScreen(Lat,Long,x,y);
                        Canvas.MoveTo(x,y);
                        LatLongDegreeToScreen(Lat2,Long2,x,y);
                        Canvas.LineTo(x,y);
                     end;
                  end;
                  Table1.Next;
               end;
               Table1.Destroy;
            end;
         end;


         procedure TMapDraw.SmartWorldOutline(Bitmap : tMyBitmap);
         var
            NEOverlay : tMyBitmap;
         begin
            {$IfDef RecordWorldOutline} WriteLineToDebugFile('TMapDraw.SmartWorldOutline in, pixel=' + RealToString(ScreenPixelSize,-12,-2) + ' m, smart scaling='+ TrueOrFalse(MDDef.NEautoScale)); {$EndIf}
            CreateBitmap(NEOverlay,Bitmap.Width,Bitmap.Height);
            if (NaturalEarthOverlayFName <> '') then begin
              NEOverlay.LoadFromFile(NaturalEarthOverlayFName);
            end
            else begin
               if (not MDDef.NEautoScale) or (ScreenPixelSize > MDDef.SmallScaleWorldOutlinePixelSize) {or AFullWorldMap} then begin
                  {$IfDef RecordWorldOutline} WriteLineToDebugFile('small scale ' + SmallScaleWorldOutlines); {$EndIf}
                  PlotShapeFileGroupOnMap(SmallScaleWorldOutlines,NEOverlay);
               end
               else if (ScreenPixelSize > MDDef.MedScaleWorldOutlinePixelSize) then begin
                  {$IfDef RecordWorldOutline} WriteLineToDebugFile('medium scale ' + MedScaleWorldOutlines); {$EndIf}
                  PlotShapeFileGroupOnMap(MedScaleWorldOutlines,NEOverlay);
               end
               else if (ScreenPixelSize > MDDef.LargeScaleWorldOutlinePixelSize) then begin
                  {$IfDef RecordWorldOutline} WriteLineToDebugFile('large scale ' + LargeScaleWorldOutlines); {$EndIf}
                  PlotShapeFileGroupOnMap(LargeScaleWorldOutlines,NEOverlay);
               end
               else begin
                  {$IfDef RecordWorldOutline} WriteLineToDebugFile('no scale and not drawn'); {$EndIf}
               end;
               if GrayscaleWorldOutline then MakeTheBitmapGrayScale(NEOverlay);
               if SubdueWorldOutline then MakeTheBitmapSubdued(NEOverlay);

               NaturalEarthOverlayFName := NextFileNumber(MDTempDir, 'NE_overlay', OverlayFExt);
               NEOverlay.SaveToFile(NaturalEarthOverlayFName);
            end;
            DrawAndDeleteOverlay(Bitmap,NEOverlay,100); //,0,0,true);
         end;


         procedure TMapDraw.CarefulPolyLineDraw(Bitmap : tMyBitmap; var PointsInPolyline : integer;  var PolyLinePoints : tPolyLinePts);
         var
            i,LastX : integer;
            FirstPoint : boolean;
         begin
            {$IfDef RecordCarefulPolyLineDraw} WriteLineToDebugFile('TMapDraw.CarefulPolyLineDraw in'); {$EndIf}
            FirstPoint := true;
            for i := 0 to pred(PointsInPolyline) do begin
                if FirstPoint then begin
                   FirstPoint := false;
                   Bitmap.Canvas.MoveTo(PolyLinePoints[i].x,PolyLinePoints[i].y);
                   {$IfDef RecordCarefulPolyLineDraw} WriteLineToDebugFile('First point drawn'); {$EndIf}
                end
                else begin
                   if abs(PolyLinePoints[i].x-LastX) > (MapXSize * 2 div 4) then begin
                      FirstPoint := true;
                      {$IfDef RecordCarefulPolyLineDraw} WriteLineToDebugFile('Long run'); {$EndIf}
                   end
                   else begin
                      {$IfDef RecordCarefulPolyLineDraw} WriteLineToDebugFile('Actual lineto'); {$EndIf}
                      Bitmap.Canvas.LineTo(PolyLinePoints[i].x,PolyLinePoints[i].y);
                   end;
                end;
                Lastx := PolyLinePoints[i].x;
            end;
            PointsInPolyLine := 0;
         end;

         procedure TMapDraw.ShowVegOnMap(var inBitmap : tMyBitmap; VegLayer : integer = 0);
         {$IfDef ExVegDensity}
         begin
         {$Else}
         var
            RealDEM : integer;
            NewBitmap : tMyBitmap;
         begin
            RealDEM := DEMonMap;
            CreateBitmap(NewBitmap,inBitmap.Width,inBitmap.Height);
            if MDDef.VegOptionMap = voVeg then begin
               DEMonMap := DEMGlb[DEMonMap].VegGrid[1];
               ColorTintedElevationMap(NewBitMap);
               PetImage.MakeBitmapGreen(NewBitmap);
            end
            else begin
               DEMGlb[DEMonMap].OpenDSMGrid;
               DEMonMap := DEMGlb[DEMonMap].DSMGrid;
               ColorTintedElevationMap(NewBitMap);
            end;
            inBitmap.Canvas.Draw(0,0,NewBitmap);
            NewBitmap.Free;
            DEMonMap := RealDEM;
         {$EndIf}
         end;


         procedure TMapDraw.DrawElevationMapForMerge(var Bitmap : tMyBitmap);
         var
            x,y,xg,yg : integer;
            BMPMem : tBMPMemory;
         begin
            {$IfDef RecordMapMerge} WriteLineToDebugFile('TMapDraw.DrawElevationMapForMerge  ' + MapSizeString); {$EndIf}
            if (DEMonMap = 0) or (DEMGlb[DEMonMap] = Nil) then exit;
            BMPMem := tBMPMemory.Create(Bitmap);
            SetUpElevationColorTable;
            for y := 0 to pred(MapYSize) do begin
               for x := 0 to pred(MapXSize) do begin
                  ScreenToDEMGrid(x,y,XG,yg);
                  BMPMem.SetPixelColor(x,y,GetElevColor(xg,yg));
               end;
            end {for j};
            BMPMem.Destroy;
         end;


         procedure tMapDraw.PositionBitmap(var Bitmap : tMyBitmap;  var Legend : tMyBitmap; LegendLocation : byte);
         var
            x,y : integer;
         begin
            if (Bitmap <> Nil) and (Legend <> Nil)  then begin
               {$IfDef RecordLegend} WriteLineToDebugFile('tMapDraw.PositionBitmap in');   {$EndIf}
               try
                  x := 0;
                  y := 0;
                  if (MDDef.SpecifyLegendX = 0) then begin
                     if LegendLocation in [lpNEMap,lpSEMap] then x := MapXSize - Legend.Width;
                     if LegendLocation in [lpNMap,lpSMap] then x := MapXSize div 2 - Legend.Width div 2;
                  end
                  else x := MDDef.SpecifyLegendX;
                  if LegendLocation in [lpSWMap,lpSMap,lpSEMap] then y := MapYSize - Legend.Height;
                  Bitmap.Canvas.Pen.Color := clNearWhite;   //RGB(254,254,254);
                  Bitmap.Canvas.Brush.Color := clWhite;
                  Bitmap.Canvas.Brush.Style := bsSolid;
                  Bitmap.Canvas.Rectangle(x,y,pred(x+Legend.Width),pred(y+Legend.Height));
                  Bitmap.Canvas.CopyMode := cmSrcCopy;   //so white overwrites
                  Bitmap.Canvas.Draw(x,y,Legend);
                  Legend.Free;
               except
                  on Exception do begin end;
               end;
               {$IfDef RecordLegend} WriteLineToDebugFile('tMapDraw.PositionBitmap out');   {$EndIf}
            end;
         end;


         procedure tMapDraw.DrawDeclinationDiagramOnBitmap(var Bitmap : tMyBitmap);
         var
            GridMagAngle: float64;
            x,y,xd,yd : integer;

             procedure LabelAngles(GridMagAngle,MapMagDec : float64);
             begin
               if (GridMagAngle > 180) then GridMagAngle := 360 - GridMagAngle;
               if (MapMagDec > 180) then MapMagDec := 360 - MapMagDec;
               with Bitmap.Canvas do begin
                  Pen.Color := clBlack;
                  Font.Name := 'Arial';
                  Font.Size := 8;
                  TextOut(95,yd+205,'GT:' + RealToString(-GridTrueAngle,5,2) + '°');
                  TextOut(95,yd+220,'TM:' + RealToString(MapMagDec,5,2) + '°');
                  TextOut(95,yd+235,'GM:' + RealToString(GridMagAngle,5,2) + '°');
               end;
             end;

         begin
            CreateBitmap(Bitmap,150,250);
            xd := 65;
            yd := 0;
            Bitmap.Canvas.Brush.Color := clWhite;
            Bitmap.Canvas.Rectangle(xd-65,yd,xd+85,yd+250);

            if DEMMap and (DEMGlb[DEMonMap].DEMheader.DigitizeDatum = Spherical) then exit;

            GridMagAngle := -GridTrueAngle+MapMagDec;

            if ( (VectorIndex <> 0) and (PrimMapProj.PName = UTMEllipsoidal)) or (DEMMap and (DEMGlb[DEMOnMap].DEMheader.DEMUsed = UTMBasedDEM))
               {$IfDef ExSat} {$Else} or (ValidSatOnMap and (SatImage[SatOnMap].ImageMapProjection.PName = UTMellipsoidal)) {$EndIf}
               then begin

               Bitmap.Canvas.MoveTo(xd+10,yd+250);   Bitmap.Canvas.LineTo(xd+10,yd+10);
               Bitmap.Canvas.Font.Size := 6;
               Bitmap.Canvas.TextOut(xd+5,yd+2,'GN');

               x := xd + 10 + round(sinDeg(GridMagAngle) * 250);
               y := yd + 250 - round(CosDeg(GridMagAngle) * 250);
               Bitmap.Canvas.MoveTo(xd+10,yd+250);  Bitmap.Canvas.LineTo(x,y);   Bitmap.Canvas.LineTo(x+5,y+15);

               x := xd + 10 - round(sinDeg(GridTrueAngle) * 225);
               y := yd + 250 - round(CosDeg(GridTrueAngle) * 225);
               Bitmap.Canvas.MoveTo(xd+10,yd+250);   Bitmap.Canvas.LineTo(x,y);
               Bitmap.Canvas.Font.Size := 12;
               if (GridTrueAngle < 0) then Bitmap.Canvas.TextOut(x+8,y,'*') else Bitmap.Canvas.TextOut(x-12,y,'*');
            end;

            if (DEMMap and (DEMGlb[DEMOnMap].DEMheader.DEMUsed = ArcSecDEM)) {$IfDef ExSat} {$Else} or (ValidSatOnMap and (SatImage[SatOnMap].ImageMapProjection.PName = PlateCaree)) {$EndIf}
                then begin
               {$IfDef RecordMagDecDiagram} WriteLineToDebugFile('Mag Dec, ArcSec Map'); {$EndIf}

                Bitmap.Canvas.MoveTo(xd+10,yd+250);
                Bitmap.Canvas.LineTo(xd+10,yd);
                Bitmap.Canvas.Font.Size := 12;
                Bitmap.Canvas.TextOut(xd+7,yd+5,'*');

                x := xd + 10 + round(sinDeg(MapMagDec) * 225);
                y := yd + 250 - round(CosDeg(MapMagDec) * 225);
                Bitmap.Canvas.MoveTo(xd+10,yd+250);
                Bitmap.Canvas.LineTo(x,y);
                Bitmap.Canvas.LineTo(x+5,y+15);

                x := xd + 10 + round(sinDeg(GridTrueAngle) * 225);
                y := yd + 250 - round(CosDeg(GridTrueAngle) * 225);
                Bitmap.Canvas.MoveTo(xd+10,yd+250);
                Bitmap.Canvas.LineTo(x,y);
                Bitmap.Canvas.Font.Size := 6;
                if (GridTrueAngle > 0) then Bitmap.Canvas.TextOut(x+8,y,'GN') else Bitmap.Canvas.TextOut(x-16,y,'GN');
            end;
            LabelAngles(GridMagAngle,MapMagDec);
         end;


         function TMapDraw.DrawLegendOnBitmap : tMyBitmap;
         var
            Colors : tColors256;
            Radius,x,i,l : integer;
            Hue : float64;
            Values      : array[0..255] of float64;
            SlopeBands,
            BinWidth,xc,yc,
            y : integer;
            Color        : tColor;
            TStr : ShortString;
            LegendGIS : integer;

                      procedure Box(Color : tColor; ylo : integer; Capt : shortstring);
                      begin
                         Result.Canvas.Brush.Color := Color;
                         Result.Canvas.Rectangle(4,ylo,25,ylo+20);
                         Result.Canvas.Brush.Color := clNearWhite;
                         Result.Canvas.TextOut(30,ylo+2,Capt);
                      end;

         begin
            {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapDraw.DrawLegendOnBitmap in'); {$EndIf}

            Result := Nil;
            if ((MaxMapElev - MinMapElev) < 0.0001) then exit;

            if (MapMerge in [mmElevation]) then begin
               if not DEMMap then ScaleMapElevationsToDEM;
               for i := 0 to 255 do begin
                  Hue := (360.0 - (MDDef.MergeHue/255) * (i / 255 * 360.0));
                  if (MapMerge = mmElevation) or (MapType in [mtIHSReflect]) then Values[i] := MinMapElev + i/255 * (MapZRange)
                  else Values[i] := i/255 * (100 * MDDef.MaxMergeSlope);
                  Colors[i] := ConvertPlatformColorToTColor(RGBtripFromHSI(Hue,MDDef.MergeSat,MDDef.MergeInt));
               end;
               if isSlopeMap(MapType) then TStr := '%'
               else TStr := LabelElevUnits[DEMGlb[DEMOnMap].DEMheader.ElevUnits];

               {$IfDef RecordMapDraw} WriteLineToDebugFile('Going to draw legend, case 1'); {$EndIf}
               if MDDef.GridLegendLocation.HorizontalLegend then begin
                  Result := HorizontalLegendOnBitmap(Colors,Values,TStr,'',MDDef.GridLegendLocation.LegendSize);
               end
               else begin
                  Result := VerticalLegendOnBitmap(Colors,Values,TStr);
               end;
            end;

            if (MapType = mtDEMVATTable) then begin
               Result := MakeVATLegend;
            end;

            if isElevationMap(MapType) or (MapType in [mtIHSReflect]) then begin
               if (MapType = mtElevFromTable) then begin
                  if OpenNumberedGISDataBase(LegendGIS,HardLimitColorPaletteFName) then begin
                     GISDB[LegendGIS].MyData.ApplyFilter('PALETTE=' + QuotedStr(ElevationFixedPalette));
                     Result := GISDB[LegendGIS].CreateDataBaseLegend(true);
                     CloseAndNilNumberedDB(LegendGIS);
                  end;
               end
               else begin
                   for i := 0 to 255 do begin
                      Colors[i] := ConvertPlatformColorToTColor(pPal[i]);
                      Values[i] := MinMapElev + i/255 * (MapZRange);
                   end;
                   if (MapType = mtElevIHS) then begin
                      for i := 0 to 255 do Colors[i] := ConvertPlatformColorToTColor(RGBtripFromHSI(TheHue(MDDef.MergeHue,i),MDDef.MergeSat,MDDef.MergeInt));
                   end;
                   TStr := LabelElevUnits[DEMGlb[DEMOnMap].DEMheader.ElevUnits];
                   if Log10Elev then TStr := 'log10(' + TStr + ')';
                   if (MapType = mtElevLandSea) then begin
                      for i := 0 to 255 do begin
                         if Values[i] > 0 then begin
                            l := round(253.0 * (Values[i] - 0) / (MaxMapElev - 0));
                            if (l > 254) then l := 254;
                            if (l < 1) then l := 1;
                            Colors[i] := ConvertPlatformColorToTColor(pPal[l]);
                         end
                         else begin
                            if (MinMapElev = 0) then l := round(253.0 * (Values[i] - MinMapElev))
                            else l := round(253.0 * (Values[i] - MinMapElev) / (0 - MinMapElev));
                            if (l > 254) then l := 254;
                            if (l < 1) then l := 1;
                            Colors[i] := ConvertPlatformColorToTColor(seaPal[l]);
                         end;
                      end;
                   end;
                   if MDDef.GridLegendLocation.HorizontalLegend then begin
                      Result := HorizontalLegendOnBitmap(Colors,Values,TStr,'',MDDef.GridLegendLocation.LegendSize);
                   end
                   else Result := VerticalLegendOnBitmap(Colors,Values,TStr);
               end;
            end;

            if (MapType in [mtDEMaspect,mtFlowDir360]) or (DEMGlb[DEMonMap].DEMHeader.ElevUnits = AspectDeg) then begin
               Radius := 22 * MDDef.GridLegendLocation.LegendSize;
               CreateBitmap(Result,2*Radius+15,2*Radius+15);
               LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
               Result.Height := 2 * Radius + 15 + Result.Canvas.TextHeight('Aspect');
               ClearBitmap(Result,clNearWhite);
               xc := 5+Radius;
               yc := 5+Radius;
               for i := 1 to 360 do begin
                  x := xc + round(Radius * SinDeg(i));
                  y := yc - round(Radius * CosDeg(i));
                  Color := ConvertPlatformColorToTColor(AspColor[i]);
                  Result.Canvas.Pen.Color := Color;
                  Result.Canvas.Pen.Width := 2;
                  Result.Canvas.MoveTo(xc,yc);
                  Result.Canvas.LineTo(x,y);
               end;
               Result.Canvas.Brush.Color := ClWhite;
               Result.Canvas.Brush.Style := bsClear;
               Result.Canvas.TextOut(10,2*Radius+8,'Aspect');
            end
            else if isSlopemap(MapType) or (MapMerge in [mmSlope]) then begin
               {$IfDef RecordMapDraw} WriteLineToDebugFile('Slope legend'); {$EndIf}
                if MapType in [mtSlopeGrayScale,mtSlopeGrayScaleReversed,mtSlopeRainbow,mtSlopePastel] then begin
                   for i := 0 to 254 do begin
                      Colors[i] := ConvertPlatformColorToTColor(SlopeColorChoices[i]);
                      Values[i] := i/254 * MDDef.MaxSlopeOnMaps;
                   end;
                   if MDDef.GridLegendLocation.HorizontalLegend then Result := HorizontalLegendOnBitmap(Colors,Values,'%','',MDDef.GridLegendLocation.LegendSize)
                   else Result := VerticalLegendOnBitmap(Colors,Values,'%');
                end
                else begin
                   if MapType in [mtSlopeGoNoGo,mtSlopeTrafficCats] then begin
                      if MapType in [mtSlopeGoNoGo] then CreateBitmap(Result,80,60)
                      else CreateBitmap(Result,80,80);
                      ClearBitmap(Result,clNearWhite);
                      Result.Canvas.Brush.Style := bsSolid;
                      Result.Canvas.Pen.Color := clBlack;
                      Box(clLime,0,'Go');
                      if MapType in [mtSlopeGoNoGo] then begin
                         Box(clRed,20,'No Go');
                         Box(clBlack,40,'Flat');
                         Result.Canvas.Brush.Style := bsClear;
                         Result.Canvas.Rectangle(0,0,79,60);
                      end
                      else begin
                         Box(clYellow,20,'Slow Go');
                         Box(clRed,40,'No Go');
                         Box(clBlack,60,'Flat');
                         Result.Canvas.Brush.Style := bsClear;
                         Result.Canvas.Rectangle(0,0,79,80);
                      end;
                   end
                   else begin
                     {$IfDef RecordMapDraw} WriteLineToDebugFile('slope legend drop through'); {$EndIf}
                      if MapType in [mtSlopeStandardCats] then begin
                         BinWidth := 25;
                         SlopeBands := MDDef.NumSlopeBands;
                      end
                      else begin
                         BinWidth := 50;
                         SlopeBands := 4;
                      end;
                      CreateBitmap(Result,succ(SlopeBands) * BinWidth + 5,50);
                      ClearBitmap(Result,clNearWhite);
                      for i := 0 to pred(SlopeBands) do begin
                         Result.Canvas.Brush.Style := bsSolid;
                         Result.Canvas.Brush.Color := SlopeColors[i];
                         Result.Canvas.Pen.Color := SlopeColors[i];
                         Result.Canvas.Rectangle(i*BinWidth,0,succ(i)*BinWidth,25);
                         Result.Canvas.Brush.Color := clNearWhite;
                         if (i < pred(SlopeBands)) then begin
                            Result.Canvas.Pen.Color := clBlack;
                            if i = pred(MDDef.NumSlopeBands) then TStr := '%' else TStr := '';
                            Result.Canvas.TextOut(succ(i)*BinWidth, 32,RealToString(SlopeCut[i],-5,0)+TStr);
                            Result.Canvas.MoveTo(succ(i)*BinWidth, 25);
                            Result.Canvas.LineTo(succ(i)*BinWidth, 30);
                         end;
                      end {for i}
                   end;
                end;
               {$IfDef RecordMapDraw} WriteLineToDebugFile('Slope legend OK');{$EndIf}
            end;
            {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapDraw.DrawLegendOnBitmap out');{$EndIf}
         end;


         procedure TMapDraw.DrawArc(Canvas : tCanvas; Lat,Long : float64; Range,StartAz,EndAz : float64; RangeCircleColor : tColor; RangeCircleWidth : Integer; AddToShape : boolean = false; DrawPie : boolean = false);
         var
            Lat2,Long2,Azimuth,AzIncr : float64;
            xl,yl,xr,yr,rad,NumPts : integer;
            dCoord : ^tDcoords;
            Parts : array[1..1] of integer;
         begin
            Canvas.Pen.Color := RangeCircleColor;
            Canvas.Pen.Width := RangeCircleWidth;

            Azimuth := StartAz;
            if (EndAz < StartAz) then EndAz := EndAz + 360;

            VincentyPointAtDistanceBearing(Lat,Long,Range,Azimuth,Lat2,Long2);
            LatLongDegreeToScreen(Lat2,Long2,xr,yr);
            If AddToShape then begin
               new(Dcoord);
               Parts[1] := 0;    //record in the stream where coordinates start
               LastRecPolyLineHeader.NumParts := 1;
               dCoord^[0].Lat := Lat2;
               dCoord^[0].Long := Long2;
               NumPts := 1;
            end;

            if DrawPie then begin
               LatLongDegreeToScreen(Lat,Long,xl,yl);
               Canvas.MoveTo(xl,yl);
               Canvas.LineTo(xr,yr);
            end
            else Canvas.MoveTo(xr,yr);
            LatLongDegreeToScreen(Lat,Long,xl,yl);
            rad := round(sqrt( sqr(xl-xr) + sqr(yl-yr)));
            if (rad < 50)  then AzIncr := 5
            else if (rad < 100) then AzIncr := 2.5
            else if (rad < 500) then AzIncr := 1
            else if (rad < 1000) then AzIncr := 0.5
            else AzIncr := 0.25;

            repeat
               Azimuth := Azimuth + AzIncr;
               if (Azimuth > EndAz) then Azimuth := EndAz;
               VincentyPointAtDistanceBearing(Lat,Long,Range,Azimuth,Lat2,Long2);
               LatLongDegreeToScreen(Lat2,Long2,xl,yl);
               Canvas.LineTo(xl,yl);
               If AddToShape then begin
                  dCoord^[NumPts].Lat := Lat2;
                  dCoord^[NumPts].Long := Long2;
                  inc(NumPts);
               end;
            until (Azimuth > EndAz-0.005);
            if DrawPie then begin
               LatLongDegreeToScreen(Lat,Long,xl,yl);
               Canvas.LineTo(xl,yl);
            end;

            Canvas.Pen.Width := 1;
            If AddToShape then Dispose(Dcoord);
         end;


         procedure TMapDraw.DrawRangeCircleLatLong(Canvas : tCanvas; Lat,Long : float64; RangeCirclesRadius : float64;
            RangeCircleColor : tColor; RangeCircleWidth : Integer; AddToShape : boolean = false);
         begin
            if (RangeCirclesRadius > 0.0001) then begin
               DrawArc(Canvas,Lat,Long,RangeCirclesRadius,0,360,RangeCircleColor,RangeCircleWidth,AddToShape);
            end;
         end;


         procedure TMapDraw.OutlineFans(Canvas : tCanvas; FanTable : tMyData; FanOutLineColor : tColor; FanOutLineWidth : Integer);
         var
            Lat2,Long2,Lat,Long,LeftAz,RightAz,Range,MinRange : float64;
            xl,yl,xr,yr : integer;

            procedure Line(Azimuth : float64);
            begin
               VincentyPointAtDistanceBearing(Lat,Long,Range,Azimuth,Lat2,Long2);
               LatLongDegreeToScreen(Lat2,Long2,xl,yl);
               VincentyPointAtDistanceBearing(Lat,Long,MinRange,Azimuth,Lat2,Long2);
               LatLongDegreeToScreen(Lat2,Long2,xr,yr);
               Canvas.MoveTo(xr,yr);
               Canvas.LineTo(xl,yl);
            end;

         begin
            if (FanTable <> Nil) then begin
               Canvas.Pen.Color := FanOutLineColor;
               Canvas.Pen.Width := FanOutLineWidth;
               with FanTable do begin
                  First;
                  while not EOF do begin
                     Lat := GetFieldByNameAsFloat('LAT');
                     Long := GetFieldByNameAsFloat('LONG');
                     LeftAz := GetFieldByNameAsFloat('LEFT_AZ');
                     RightAz := GetFieldByNameAsFloat('RIGHT_AZ');
                     Range := GetFieldByNameAsFloat('SENSOR_RNG');
                     MinRange := GetFieldByNameAsFloat('MIN_RNG');

                     DrawArc(Canvas,Lat,Long,Range,LeftAz,RightAz,FanOutLineColor,FanOutLineWidth);
                     if (MinRange > 0.01) then DrawArc(Canvas,Lat,Long,MinRange,LeftAz,RightAz,FanOutLineColor,FanOutLineWidth);
                     if RightAz < LeftAz then RightAz := RightAz + 360;
                     if (RightAz - LeftAz) < 359 then begin
                        Canvas.Pen.Width := FanOutlineWidth;
                        Line(LeftAz);
                        Line(RightAz);
                        Canvas.Pen.Width := 1;
                     end;
                     Next;
                  end;
               end;
            end;
         end;


         procedure TMapDraw.SmartUSOutline(var inBitmap : tMyBitmap);
         var
            Bitmap : tMyBitmap;
         begin
         {$IfDef VCL}
            {$IfDef RecordOverlays} WriteLineToDebugFile('TMapDraw.SmartUSOutline in'); {$EndIf}
            if (USOverlayfName <> '') then begin
               {$IfDef RecordOverlays} WriteLineToDebugFile('TMapDraw.SmartUSOutline reload bitmap ' + USOverlayfName);   {$EndIf}
               Bitmap := LoadBitmapFromFile(USOverlayfName);
            end
            else begin
               CreateBitmap(Bitmap,inBitmap.Width,inBitmap.Height);
               if MDDef.US_Show_Roads then PlotLineShapeFileOnMap(HighwayGISFileName,Bitmap,MDDef.US_Highway_Color,MDDef.US_Highway_Width);
               if MDDef.US_Show_Rivers then PlotLineShapeFileOnMap(RiversGISFileName,Bitmap,MDDef.US_River_Color,MDDef.US_River_Width);
               if MDDef.US_Show_Counties and (ScreenPixelSize < MDDef.PixelSizeToShowCounties) then begin
                   PlotLineShapeFileOnMap(CountyGISFileName,Bitmap,MDDef.US_CountyOutline_Color,MDDef.US_CountyOutline_Width)
               end;
               if MDDef.US_Show_States and (ScreenPixelSize < MDDef.PixelSizeToShowStates) then begin
                   PlotLineShapeFileOnMap(StateGISFileName,Bitmap,MDDef.US_StateOutline_Color,MDDef.US_StateOutline_Width)
               end;
               USOverlayfName := NextFileNumber(MDTempDir, 'US_overlay', OverlayFExt);
               PetImage.SaveBitmap(Bitmap,USOverlayfName);
               {$IfDef RecordOverlays} WriteLineToDebugFile('TMapDraw.SmartUSOutline drew new ' + USOverlayfName); {$EndIf}
            end;
            inBitmap.Canvas.CopyMode := cmSrcAnd;
            inBitmap.Canvas.Draw(0,0,Bitmap);
            Bitmap.Free;
            {$IfDef RecordOverlays} WriteLineToDebugFile('TMapDraw.SmartUSOutline out'); {$EndIf}
         {$EndIf}
         end;


         procedure TMapDraw.DrawContoursInArea(Bitmap : tMyBitmap; ContInt,DEMToUse : integer);
         {++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
         { Prototype for routine written by Cadet Karen R. Gorkowski '85 in }
         { UCSD Pascal for the Damn Fine Sage IV.  Modified and translated  }
         { into Turbo Pascal for IBM PC and clones by CPT Peter L. Guth.    }
         {++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
         {^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
         { Contouring Algorithm:                                 }
         { 1. Each four data points define a rectangle.          }
         { 2. Calculate where contour lines cross each side of   }
         {  the rectangle (linear interpolation between corners).}
         { 3. Connect same elevation points across rectangle.    }
         { 4. Move to the next rectangle (start in lower left and}
         {   move up, then right, doing one column at a time).   }
         {_______________________________________________________}
             label
               NoFullSquare,StartAgain;
             type
                RlSide    = array[1..4,1..MaxContoursPerGrid] of float64;
                IntSide   = array[1..4,1..MaxContoursPerGrid] of integer;
                IntCorner = array[1..4] of integer;
             var
                Elv                      : ElevArray;
                ContourZ,SideZ,XPix,YPix : IntSide;
                Point                    : RlSide;
                DifElv,ElvPer,Tested     : IntCorner;
                Side,DesiredContInt,
                ElevShift,Side4,
                NSGrid,EWGrid,cnt,Num,side1  : integer;
                IndexContours          : tStringList;
                Found : boolean;
                ContourOverlay : tMyBitmap;


             procedure CompareForContourLines;
             var
                Num : integer;
             begin
                Num := 1;
                while not(Found) and (Num <= ElvPer[Side]) do with ContourOverlay.Canvas do begin
                   if (SideZ[Side1,Cnt] = SideZ[Side,Num]) then begin
                      case MDdef.ContourColors of
                         ccSingle      : Pen.Color := ConvertPlatformColorToTColor(MDdef.OverlayContColor);
                         ccSpecified   : Pen.Color := ContColorFunct(SideZ[Side,Num]-ElevShift);
                         ccChromaDepth : Pen.Color := RainbowColorFunct(SideZ[Side,Num]-ElevShift,MinMapElev,MaxMapElev);
                         ccTerrain     : Pen.Color := TerrainTColor(SideZ[Side,Num]-ElevShift,MinMapElev,MaxMapElev);
                      end;

                      if (SideZ[Side1,Cnt] div ContInt) mod 5 = 0 then Pen.Width := MDDef.IndexContWidth
                      else Pen.Width := MDdef.ContourLineWidth;

                      MoveTo(XPix[Side1,Cnt],YPix[Side1,Cnt]);
                      LineTo(XPix[Side,Num],YPix[Side,Num]);

                      if MDDef.LabelContours and (SideZ[Side,Num] mod (5 * ContInt) = 0) then begin
                         if (XPix[Side1,Cnt] <> XPix[Side,Num]) or (YPix[Side1,Cnt] <> YPix[Side,Num]) then
                            IndexContours.Add(IntegerToString(SideZ[Side,Num]-ElevShift,8) + IntegerToString(XPix[Side1,Cnt],8) +
                                     IntegerToString(YPix[Side1,Cnt],8) + IntegerToString(XPix[Side,Num],8) +
                                     IntegerToString(YPix[Side,Num],8));
                      end;

                      {$IfDef RecordContourLines}
                      if SmallArea then begin
                         WriteLineToDebugFile(IntegerToString(SideZ[Side,Num],8) + IntegerToString(XPix[Side1,Cnt],8) +  IntegerToString(YPix[Side1,Cnt],8) + IntegerToString(XPix[Side,Num],8) + IntegerToString(YPix[Side,Num],8));
                      end;
                      {$EndIf}

                      SideZ[Side1,Cnt] := -1;
                      SideZ[Side,Num] := -1;
                      inc(Tested[Side1]);
                      inc(Tested[Side]);
                      Found := true;
                   end {if};
                   inc(Num);
                end {while};
             end {proc Compare};


         var
            MinAreaElev,MaxAreaElev   : integer;
            zmin,zmax,zAvg         : float32;
            FloatElev  : tElevFloatArray;
         begin
            {$IfDef RecordContour} WriteLineToDebugFile('TMapDraw.DrawContoursInArea, map=' + BaseTitle + '  contouring DEM=' + IntToStr(DEMToUse)); {$EndIf}
            if (ContInt <> 0) then with MapCorners do begin
               if (DEMToUse = DEMonMap) and (ContourOverlayfName <> '')  then begin
                  ContourOverlay := LoadBitmapFromFile(ContourOverlayfName);
               end
               else if (ContourOverlayfName2 <> '')  then begin
                  ContourOverlay := LoadBitmapFromFile(ContourOverlayfName2);
               end
               else begin
                  CreateBitmap(ContourOverlay,Bitmap.Width,Bitmap.Height);
                  {$IfDef RecordContour} WriteLineToDebugFile('Starting Contour map with contour interval ' + IntToStr(ContInt) + '   Contour color method: ' + IntToStr(ord(MDdef.ContourColors))); {$EndIf}
                  if MDDef.LabelContours then begin
                     {$IfDef RecordContour} WriteLineToDebugFile('MDDef.LabelContours active'); {$EndIf}
                     IndexContours := tStringList.Create;
                     IndexContours.Sorted := true;
                     IndexContours.Duplicates := dupIgnore;
                  end;

                  MDdef.DefaultContourInterval := ContInt;
                  DEMGlb[DEMtoUse].BoxAreaExtremeElevations(MapAreaDEMGridLimits,zMin,ZMax,zAvg);

                  MinAreaElev := round(zMin);
                  MaxAreaElev := round(ZMax);

                  if (MDdef.DefaultContourInterval <> 0) then begin
                     BotCont := (MinAreaElev div MDdef.DefaultContourInterval div 5) * MDdef.DefaultContourInterval * 5;
                     if (BotCont <> MinAreaElev) and (BotCont > 0) then inc(BotCont,MDdef.DefaultContourInterval * 5);
                     TopCont := (MaxAreaElev div MDdef.DefaultContourInterval div 5) * MDdef.DefaultContourInterval * 5;
                  end {if};

                  if ShowSatProgress then StartProgressAbortOption('Contour');
                  ContourOverlay.Canvas.Pen.Width := MDdef.ContourLineWidth;
                  DesiredContInt := ContInt;
                 {if user wants too many contour lines, for affected squares it will drop down, but will use his interval where possible}
                 {4 data points.  Starts in lower left corner, and does 1 column at a time}
                 {arrays defined both in terms of corners and sides:}
                 {   corners     sides }
                 {  4 ----- 3    --3-- }
                 {  |       |    4   2 }
                 { 1=5 ---- 2    --1-- }
                   if (MinAreaElev = 0) then ElevShift := 5 * ContInt
                   else if (MinAreaElev < 0) then ElevShift := 5 * ContInt * succ(abs(MinAreaElev) div 5 div ContInt)
                   else ElevShift := 0;
                   for EWGrid := pred(MapAreaDEMGridLimits.XGridLow) to (MapAreaDEMGridLimits.XGridHigh) do if (EWGrid > 0) and (EWGrid < pred(DEMGlb[DEMtoUse].DEMheader.NumCol)) then  begin
                      if ShowSatProgress and (EWGrid mod 10 = 0) then UpdateProgressBar((EWGrid - MapAreaDEMGridLimits.XGridLow)/(MapAreaDEMGridLimits.XGridHigh-MapAreaDEMGridLimits.XGridLow));
                      if WantOut then break;
                      for NSGrid := MapAreaDEMGridLimits.YGridLow to MapAreaDEMGridLimits.YGridHigh do begin
                         if not DEMGlb[DEMtoUse].GetElevSquareMeters(EWGrid,NSGrid,FloatElev) then goto NoFullSquare;
                         for Cnt := 1 to 5 do Elv[Cnt] := round(FloatElev[Cnt]) + ElevShift;
                         ContInt := DesiredContInt;
                        StartAgain:;
                         For Cnt := 1 to 4 do begin
                            DifElv[Cnt] := Elv[succ(Cnt)] - Elv[Cnt];
                              {difference in elevation along each side of block}
                              { if > 0, higher elevation is Counterclockwise--Cnt+1}
                              { if < 0, higher elevation is clockwise--Cnt}
                            ElvPer[Cnt] :=  Abs(Elv[succ(Cnt)] div ContInt - Elv[Cnt] div ContInt);
                            {number of contour lines per block side, calculated by}
                            {figuring # of contour lines between sea level and each}
                            {corner point  }
                            if ElvPer[Cnt] > MaxContoursPerGrid then begin
                               ContInt := ContInt * 2;
                               goto StartAgain;
                            end {if};
                            if (ElvPer[Cnt]  > 0) then begin
                               {    grid units, from Cnt corner}
                               { SideZ = elevation of first contour line from corner}
                               if DifElv[Cnt] > 0 then begin
                                  {higher elevation is counterclockwise}
                                  Point[Cnt,1] := ( ContInt - Elv[Cnt] mod ContInt) / abs(DifElv[Cnt]);
                                  SideZ[Cnt,1] := succ(Elv[Cnt] div ContInt) * ContInt;
                               end
                               else begin
                                  {higher elevation is clockwise}
                                  Point[Cnt,1] := (Elv[Cnt] mod ContInt) / abs(DifElv[Cnt]);
                                  SideZ[Cnt,1] := (Elv[Cnt] div ContInt) * ContInt;
                               end;
                               //ContourZ[Cnt,1] := (Elv[Cnt] div ContInt) * ContInt - ElevShift;
                               for Num := 2 to ElvPer[Cnt] do Begin
                                  Point[Cnt,Num] := Point[Cnt,pred(Num)] + ContInt / abs(DifElv[Cnt]);
                                  if (DifElv[Cnt] > 0) then begin
                                     SideZ[Cnt,Num] := SideZ[Cnt,pred(Num)] + ContInt;
                                     ContourZ[Cnt,1] := ContourZ[Cnt,pred(Num)] + ContInt;
                                  end
                                  else begin
                                     SideZ[Cnt,Num] := SideZ[Cnt,pred(Num)] - ContInt;
                                     ContourZ[Cnt,1] := ContourZ[Cnt,pred(Num)] - ContInt;
                                  end;
                               end {for Num};
                             end {if};
                         end {Cnt Loop};

                       {calculate screen coordinates where contour lines cross grid squares}
                          For Num := 1 to ElvPer[1] do DEMGridToScreen(EWGrid+Point[1,Num],NSGrid,XPix[1,Num],YPix[1,Num]);
                          For Num := 1 to ElvPer[2] do DEMGridToScreen(succ(EWGrid),NSGrid + Point[2,Num],XPix[2,num],YPix[2,Num]);
                          For Num := 1 to ElvPer[3] do DEMGridToScreen(succ(EWGrid)-Point[3,Num],succ(NSGrid),XPix[3,num],YPix[3,Num]);
                          For Num := 1 to ElvPer[4] do DEMGridToScreen(EWGrid,succ(NSGrid) - Point[4,Num],XPix[4,num],YPix[4,Num]);

                          {$IfDef RecordContourLines}
                          if SmallArea then begin
                             For Num := 1 to ElvPer[1] do WriteLineToDebugFile(RealToString(EWGrid+Point[1,Num],12,2) + RealToString(NSGrid,12,2));
                             For Num := 1 to ElvPer[2] do WriteLineToDebugFile(RealToString(succ(EWGrid),12,2) + RealToString(NSGrid + Point[2,Num],12,2));
                             For Num := 1 to ElvPer[3] do WriteLineToDebugFile(RealToString(succ(EWGrid)-Point[3,Num],12,2) + RealToString(succ(NSGrid),12,2));
                             For Num := 1 to ElvPer[4] do WriteLineToDebugFile(RealToString(EWGrid,12,2) + RealToString(succ(NSGrid) - Point[4,Num],12,2));
                          end;
                          {$EndIf}

                        For Num := 1 to 4 do Tested[Num] := 0;
                           {Tested is number of contour lines drawn to a side}
                        If ((Elv[1] > Elv[2]) and (Elv[1] > Elv[4])) or
                             ((Elv[3] > Elv[2]) and (Elv[3] > Elv[4])) then begin {**** Algorithm for south to west *******}
                             for Side1 := 1 to 3 do
                                While (ElvPer[Side1] > Tested[Side1]) do begin
                                   for Cnt := 1 to ElvPer[Side1] do begin
                                      Found := (SideZ[Side1,Cnt] = -1);
                                      Side4 := succ(Side1);
                                      Side := 4;
                                      while (not Found) and (Side >= Side4) do begin
                                         CompareForContourLines;
                                         dec(Side);
                                      end {while};
                                   End {Cnt loop};
                                end {while};
                          end
                          else begin  {*** Algorithm for South to East ***}
                             for Side1 := 1 to 3 do
                                While (ElvPer[Side1] > Tested[Side1]) do begin
                                   for Cnt := 1 to ElvPer[Side1] do begin
                                      Found := (SideZ[Side1,Cnt] = -1);
                                      Side4 := 4;
                                      Side := succ(Side1);
                                      while (not Found) and (Side <= Side4) do begin
                                         CompareForContourLines;
                                         inc(Side);
                                      end {while};
                                   End {Cnt loop};
                                end {while};
                           end {if};
                           NoFullSquare:;  {happens due to jaggies on 1:24K DEM edge, when area selected exends beyond DEM, for water}
                        end {NSGrid loop};
                        if WantOut then break;
                    end {EWGrid loop};
                  if ShowSatProgress then EndProgress;

                  if MDDef.LabelContours and (IndexContours.Count > 0) then ThreadContours(ContourOverlay,IndexContours);
                  if (DEMToUse = DEMonMap) then SaveLayerBitmap(ContourOverlay,ContourOverlayFName)
                  else SaveLayerBitmap(ContourOverlay,ContourOverlayFName2);
               end {with};
            end;
            DrawOverlayNoDelete(Bitmap,ContourOverlay);
            {$IfDef RecordContour} WriteLineToDebugFile('End Contour map'); {$EndIf}
         end {proc};


         procedure TMapDraw.ThreadContours(Bitmap : tMyBitmap; Contours : tStringList; ExportContourShapeFile : boolean = false);
         var
            NewTable : tMyData;
            fName : PathStr;
            i : integer;
            daAngle : word;
            TStr : ShortString;
            WorkingLine : tStringList;
            angle,MaxLen,
            LineLen,SumLen : float64;
            Lastxe,LastYe,xe,ye,IndexHt,Total,
            xp,yp,err,IndexLen,StartP,EndP,
            cont,x1,y1,x2,y2 : integer;

                 procedure CoordsFromString(i : integer; var xp,yp : integer); //inline;
                 begin
                     val(ptTrim(Copy(WorkingLine.Strings[i],1,8)),xp,err);
                     val(ptTrim(Copy(WorkingLine.Strings[i],9,8)),yp,err);
                 end;

         begin
            {$IfDef RecordContour} WriteLineToDebugFile('TMapDraw.ThreadContours in, Contours.count=' + IntToStr(Contours.Count));    {$EndIf}
            WorkingLine := Nil;
            ShowHourglassCursor;
            fName := MDTempDir + 'contours' + DefaultDBExt;
            CreateContourLineTable(fName);
            NewTable := tMyData.Create(fName,dbmCDS);

            for i := 0 to pred(Contours.Count) do begin
               NewTable.Insert;
               TStr := Contours.Strings[i];
               NewTable.SetFieldByNameAsString('CONTOUR',ptTrim(Copy(TStr,1,8)));
               NewTable.SetFieldByNameAsString('X1',ptTrim(Copy(TStr,9,8)));
               NewTable.SetFieldByNameAsString('Y1',ptTrim(Copy(TStr,17,8)));
               NewTable.SetFieldByNameAsString('X2',ptTrim(Copy(TStr,25,8)));
               NewTable.SetFieldByNameAsString('Y2',ptTrim(Copy(TStr,33,8)));
               NewTable.Post;
            end;
            NewTable.FiltRecsInDB := Contours.Count;
            Contours.Free;

            {$IfDef RecordContour} WriteLineToDebugFile('Start labelf, recs='  + IntToStr(NewTable.FiltRecsInDB)); {$EndIf}
            Total := NewTable.FiltRecsInDB;
            if (Total > 1) then begin
               StartProgressAbortOption('Labels');
               WorkingLine := tStringList.Create;
               repeat
                  if ShowSatProgress then UpdateProgressBar((Total-NewTable.FiltRecsInDB)/Total);
                  if WantOut then break;
                  NewTable.First;
                  Cont := NewTable.GetFieldByNameAsInteger('CONTOUR');
                  x1 := NewTable.GetFieldByNameAsInteger('X1');
                  y1 := NewTable.GetFieldByNameAsInteger('Y1');
                  x2 := NewTable.GetFieldByNameAsInteger('X2');
                  y2 := NewTable.GetFieldByNameAsInteger('Y2');
                  WorkingLine.Add(IntegerToString(x1,8) + IntegerToString(y1,8));
                  WorkingLine.Add(IntegerToString(x2,8) + IntegerToString(y2,8));
                  NewTable.Delete;
                  NewTable.ApplyFilter( 'CONTOUR = ' + IntToStr(Cont) + ' AND ((X1=' + IntToStr(x1) + ' AND Y1=' + IntToStr(y1) + ') OR (X2=' + IntToStr(x1) + ' AND Y2=' + IntToStr(y1) + '))');
                  while (NewTable.FiltRecsInDB > 0) do begin
                     if (NewTable.FiltRecsInDB >= 1) then begin
                        if (NewTable.GetFieldByNameAsInteger('X1') = x1) and (NewTable.GetFieldByNameAsInteger('Y1') = y1) then begin
                           x1 := NewTable.GetFieldByNameAsInteger('X2');
                           y1 := NewTable.GetFieldByNameAsInteger('Y2');
                        end
                        else begin
                           x1 := NewTable.GetFieldByNameAsInteger('X1');
                           y1 := NewTable.GetFieldByNameAsInteger('Y1');
                        end;
                        WorkingLine.Insert(0,IntegerToString(x1,8) + IntegerToString(y1,8));
                        NewTable.Delete;
                     end;
                     NewTable.ApplyFilter( 'CONTOUR = ' + IntToStr(Cont) + ' AND ((X1=' + IntToStr(x1) + ' AND Y1=' + IntToStr(y1) + ') OR (X2=' + IntToStr(x1) + ' AND Y2=' + IntToStr(y1) + '))');
                  end;
                  NewTable.ApplyFilter( 'CONTOUR = ' + IntToStr(Cont) + ' AND ((X1=' + IntToStr(x2) + ' AND Y1=' + IntToStr(y2) + ') OR (X2=' + IntToStr(x2) + ' AND Y2=' + IntToStr(y2) + '))');
                  while (NewTable.FiltRecsInDB > 0) do begin
                     if NewTable.FiltRecsInDB >= 1 then begin
                        if (NewTable.GetFieldByNameAsInteger('X1') = x2) and (NewTable.GetFieldByNameAsInteger('Y1') = y2) then begin
                           x2 := NewTable.GetFieldByNameAsInteger('X2');
                           y2 := NewTable.GetFieldByNameAsInteger('Y2');
                        end
                        else  begin
                           x2 := NewTable.GetFieldByNameAsInteger('X1');
                           y2 := NewTable.GetFieldByNameAsInteger('Y1');
                        end;
                        WorkingLine.Add(IntegerToString(x2,8) + IntegerToString(y2,8));
                        NewTable.Delete;
                     end;
                     NewTable.ApplyFilter( 'CONTOUR = ' + IntToStr(Cont) + ' AND ((X1=' + IntToStr(x2) + ' AND Y1=' + IntToStr(y2) + ') OR (X2=' + IntToStr(x2) + ' AND Y2=' + IntToStr(y2) + '))');
                  end;

                  Bitmap.Canvas.Pen.Width := MDDef.IndexContWidth;
                  Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDdef.IndexColor);
                  Bitmap.Canvas.Font.Color := ConvertPlatformColorToTColor(MDdef.IndexColor);
                  for i := 0 to pred(WorkingLine.Count) do begin
                     CoordsFromString(i,xp,yp);
                     if (i = 0) then Bitmap.Canvas.MoveTo(xp,yp) else Bitmap.Canvas.LineTo(xp,yp);
                  end;

                  LoadMyFontIntoWindowsFont(MDDef.ContourLabelFont,Bitmap.Canvas.Font);
                  IndexLen := Bitmap.Canvas.TextWidth(IntToStr(Cont));
                  IndexHt := Bitmap.Canvas.TextHeight(IntToStr(Cont));
                  StartP := -1;
                  repeat
                     inc(StartP);
                     CoordsFromString(StartP,xp,yp);
                     EndP := StartP;
                     SumLen := 0;
                     MaxLen := 0;
                     Lastxe := xp;
                     Lastye := yp;
                     repeat
                        inc(EndP);
                        CoordsFromString(EndP,xe,ye);
                        LineLen := sqrt(sqr(xp-xe) + sqr(yp-ye));
                        SumLen := SumLen + sqrt(sqr(Lastxe-xe) + sqr(LastYe-ye));
                        if LineLen > MaxLen  then MaxLen := LineLen;
                        Lastxe := xe;
                        Lastye := ye;
                     until (LineLen >= IndexLen) or (EndP >= pred(WorkingLine.Count));

                     if (LineLen >= IndexLen) and (LineLen * 1.08 < SumLen) and (MaxLen < 1.08 * SumLen) then begin
                        angle := HeadingOfLine(xp-xe,ye-yp);
                        daAngle := round(10 * CompassAngleToMathAngle(Angle));
                        {$IfDef RecordThreadContour} WriteLineToDebugFile(IntegerToString(Cont) + RealToString(angle,8,1) + RealToString(AspDir,8,1) + IntegerToString(xe,8) + IntegerToString(ye,8)); {$EndIf}
                        TStr := IntToStr(Cont);
                        xe := xe + round(sinDeg(Angle-90) * IndexHt * 0.5);
                        ye := ye - round(CosDeg(Angle-90) * IndexHt * 0.5);
                        PETMAR.CanvasTextOutAngle(Bitmap.Canvas,xe,ye,daAngle,TStr);
                        Break;
                     end;
                  until (EndP >= pred(WorkingLine.Count));
                  WorkingLine.Clear;
                  NewTable.ApplyFilter('');
               until (NewTable.FiltRecsInDB = 0);
               if (WorkingLine <> Nil) then WorkingLine.Free;
            end;
            {$IfDef RecordContour} WriteLineToDebugFile('contour labels loop over'); {$EndIf}
            NewTable.Destroy;
            EndProgress;
         end;


         procedure TMapDraw.OverlayContours(Bitmap : tMyBitmap);
         begin
            SingleContourColor := true;
            DrawContoursInArea(BitMap,MapOverlays.ConInt,DEMonMap);
         end;


         procedure TMapDraw.PlotAnIcon(Bitmap : tMyBitmap; Lat,Long : float64; fName : PathStr; ScalingFactor : byte = 100);
         var
            sBitmap,sBitmap2 : tMyBitmap;
            x,y,LastX,LastY,sx,sy : integer;
         begin
            LatLongDegreeToScreen(Lat,Long,LastX,LastY);
            ExpandIconFileName(fName);
            if FileExists(fName) then begin
               if ScalingFactor <> 100 then  begin
                  sBitmap2 := LoadBitmapFromFile(fName);
                  sBitmap := CreateThumbNailBMP(sBitmap2, (ScalingFactor * sBitmap2.Height div 100));
                  sBitmap2.Free;
               end
               else sBitmap := LoadBitmapFromFile(fName);

               if MDDef.TransparentIcons then begin
                  Bitmap.Canvas.CopyMode := cmSrcAnd;
                  Bitmap.Canvas.Draw(LastX-sbitmap.Width div 2,LastY-sbitmap.Height div 2,sBitmap);
               end
               else begin
                 sx := LastX-sbitmap.Width div 2;
                 sy := LastY-sbitmap.Height div 2;
                 if OnScreen(sx,sy) then begin
                    for y := 0 to pred(sbitmap.Height) do
                       for x := 0 to pred(sbitmap.Width) do begin
                          if (sBitmap.Canvas.Pixels[x,y] <> clWhite) then
                             Bitmap.Canvas.Pixels[x+sx,y+sy] := sBitmap.Canvas.Pixels[x,y];
                       end;
                 end;
               end;
               sBitmap.free;
            end;
         end;


         procedure TMapDraw.PlotAnIcon(Bitmap : tMyBitmap; Table : tMyData; LatFieldName,LongFieldName : ShortString; FieldName : ShortString = 'ICONNAME'; ScalingFactor : byte = 100);
         {$IfDef ExMovie}
         begin
         {$Else}
         var
            Lat,Long : float64;
            Lastx,lasty : integer;
            MenuStr : ShortString;
         begin
            if Table.ValidLatLongFromTable(Lat,Long)  then begin
               LatLongDegreeToScreen(Lat,Long,LastX,LastY);
               if ExpandIconName(Table,FieldName,MenuStr) then begin
                  MenuStr := Table.GetFieldByNameAsString(FieldName);
                  if FileExists(ExtractFilePath(Table.TableName) + MenuStr) then MenuStr := ExtractFilePath(Table.TableName) + MenuStr
                  else MenuStr := MainMapData + 'Icons\' + MenuStr;
                  if FileExists(MenuStr) then PlotAnIcon(Bitmap,Lat,Long,MenuStr,ScalingFactor);
               end;
            end;
         {$EndIf}
         end;


         procedure TMapDraw.PlotVectorOverlay(Bitmap : tMyBitmap; FName : PathStr);
         var
            Ext : ExtStr;
            i : integer;
            sBitmap : tMyBitmap;
         begin
            Ext := ExtractFileExt(fName);
            if ExtEquals(Ext,'.KML') or  ExtEquals(Ext,'.PNG') or ExtEquals(Ext,'.JPG') or ExtEquals(Ext,'.GIF') or ExtEquals(Ext,'.BMP') then begin
               DrawWorldFileImageOnMap(Bitmap,FName);
            end
            else if ExtEquals(Ext,'.CAT') then begin
               {$IfDef AllowDEMGeomorph}
                  CloneBitmap(Bitmap,sBitmap);
                  for i := 0 to pred(MapOverlays.ovTerrainCat.Count) do begin
                     OverlayCategories(sBitMap,StringToTerrainCategory(MapOverlays.ovTerrainCat[i]));
                  end;
                  if MDDef.IHSTerrainCategory then IHSMergePurgeBitmaps(Bitmap,sBitmap)
                  else begin
                     Bitmap.Canvas.CopyMode := cmSrcAnd;
                     Bitmap.Canvas.Draw(0,0,sBitmap);
                     sBitmap.Free;
                  end;
               {$EndIf}
            end

         {$IfDef ExMilIcons}
         {$Else}
            else if (ExtractFilePath(fName) = ProjectDir + 'MILICONS\') then PlotMilIconsOnBitmap(Bitmap,fName)
         {$EndIf}
            else if ExtEquals(Ext,'.shp') or ExtEquals(Ext,DefaultDBExt) then begin
               QuickShapeFileDisplay(bitmap,fName);
            end;
         end;


         procedure tMapDraw.DrawVectors(var inBitmap : tMyBitmap);
         var
            Overlays : tBitmap;
            i : integer;
         begin
            if (VectorOverlayFName <> '') and FileExists(VectorOverlayFName) then begin
               Overlays := PetImage.LoadBitmapFromFile(VectorOverlayFName);
            end
            else begin
               {$IfDef RecordOverlays} WriteLineToDebugFile('Draw overlays: ' + IntToStr(MapOverlays.ovVectorFiles.Count)); {$EndIf}
               {$IfDef RecordOverlays}  PetImage.SaveBitmap(inBitmap,MDTempDir + 'before_vectors.bmp'); {$EndIf}
               CreateBitmap(Overlays,inBitmap.Width,inBitmap.Height);
               StartProgress('image overlays');
               for i := 0 to pred(MapOverlays.ovVectorFiles.Count) do begin
                  {$IfDef RecordOverlays} WriteLineToDebugFile('   ' + MapOverlays.ovVectorFiles.Strings[i]); {$EndIf}
                  UpdateProgressBar(i/pred(MapOverlays.ovVectorFiles.Count));
                  PlotVectorOverlay(Overlays,MapOverlays.ovVectorFiles.Strings[i]);
                  if WantOut then break;
               end;
               EndProgress;
               VectorOverlayFName := NextFileNumber(MDTempDir, 'Vector_overlay', OverlayFExt);
               PetImage.SaveBitmap(Overlays,VectorOverlayFName);
            end;
            DrawAndDeleteOverlay(inBitmap,Overlays,MDDef.OverlayOpacity);
         end;


         procedure tMapDraw.DrawMapOverlays(var inBitmap : tMyBitmap);
         var
            i : integer;
            DBsDone : boolean;
         begin
            {$If Defined(RecordFullMapDraw) or Defined(RecordTiming) or Defined(RecordOverlays)} WriteLineToDebugFile('tMapDraw.DrawMapOverlays in ' + BaseTitle  + '  ' + MapSizeString); {$EndIf}
            DBSdone := false;
            for i := MaxOverlays downto 1 do if (OverLayOrder[i] <> ovoUnused) then begin
               {$IfDef RecordOverlays} WriteLineToDebugFile(' Map layer: ' + LayerName[OverLayOrder[i]]); {$EndIf}

               case OverLayOrder[i] of
                   ovoSRTMWater   : OverlaySRTMWaterBodies(inBitmap);
                   ovoContours    : OverlayContours(inBitmap);
                   ovoContoursDEM2 : DrawContoursInArea(inBitMap,MapOverlays.ConInt,DEM2onMap);
                   ovoGrids       : DrawGridLines(inBitmap);
                   ovoGazetteer   : LabelGazetteerFeatures(inBitmap);
                   ovoSecondGrid  : DrawSecondGrid(inBitmap,DEM2onMap);
                   ovoVectors     : DrawVectors(inBitmap);
                   ovoCartoDB     : PlotShapeFileGroup(inBitmap,CartoGroupShapesUp);
                   ovoDatabases   : begin
                                       PlotDataBases(inBitmap);
                                       DBSdone := true;
                                    end;
                   {$IfDef ExTiger}
                   {$Else}
                      ovoTiger       : DrawFullTigerCoverage(inBitmap);
                   {$EndIf}
                   {$IfDef ExTissot}
                   {$Else}
                      ovoTissot      : if PrimMapProj.TissotEnabled then DrawTissotIndicatrixOverlay(inBitmap);
                   {$EndIf}

                   {$IfDef ExPLSS}
                   {$Else}
                      ovoPLSS        : DrawPLSSGrid(inBitmap);
                   {$EndIf}

                   {$IfDef VCL}
                      ovoWorldOutlines : SmartWorldOutline(inBitmap);
                      ovoUSOUtlines  : SmartUSOutline(inBitmap);
                   {$EndIf}
                   {$IfDef ExOSM}
                   {$Else}
                      ovoOSM      : begin
                                       SaveBackupDefaults;
                                       MDDef.OSMtoCDS := false;
                                       PlotShapeFileGroup(inBitmap,OSMShapesUp);
                                       if GrayscaleOSM then MakeTheBitmapGrayScale(inBitmap);
                                       if SubdueOSM then MakeTheBitmapSubdued(inBitmap);
                                       RestoreBackupDefaults;
                                    end;
                   {$EndIf}
                   {$IfDef ExViewshed}
                   {$Else}
                      ovoFans     : if (CurrentFansTable <> 0) and (GISdb[CurrentFansTable] <> Nil) then begin
                                       {$IfDef FanDrawProblems} SaveBitmap(inBitmap,MDTempDir + 'aa_before_fans.png');  {$EndIf}
                                       DrawAllFans(GISdb[CurrentFansTable].MyData,inBitmap);
                                       {$IfDef FanDrawProblems} SaveBitmap(inBitmap,MDTempDir + 'aa_after_fans.png');  {$EndIf}
                                    end;
                   {$EndIf}
               end {case};
               ApplicationProcessMessages;
            end {for i};

            {$IfDef RecordTiming} WriteLineToDebugFile('tMapDraw.DrawMapOverlays overlays done'); {$EndIf}
            if GrayscaleSubdueOverlays then begin
               if MakeMapGrayscale then begin
                  {$IfDef RecordOverlays} WriteLineToDebugFile('MakeMapGrayscale'); {$EndIf}
                  MakeTheBitmapGrayScale(inBitmap);
               end;
               if SubdueBase then begin
                  {$IfDef RecordOverlays} WriteLineToDebugFile('SubdueBase'); {$EndIf}
                  MakeTheBitmapSubdued(inBitmap);
               end;
               if not SubdueGrids then begin
                  DrawGridLines(inBitmap);
               end;
            end;

            if MDdef.DBsOnAllMaps and (not DBSdone) and AllowDataBaseDrawing then begin
               {$IfDef RecordOverlays} WriteLineToDebugFile('PlotDataBases catch up'); {$EndIf}
               PlotDataBases(inBitmap);
            end;

            {$IfDef RecordTiming} WriteLineToDebugFile('tMapDraw.DrawMapOverlays out'); {$EndIf}
         end;

         function tMapDraw.MaxScaleDistortionOnMap : float64;
         var
            s1,s2,s3,r1,r2,r3 : float64;
         begin
            s1 := SizeOfPixel(MapXsize div 2,0);
            s2 := SizeOfPixel(MapXsize div 2,MapYSize div 2);
            s3 := SizeOfPixel(MapXsize div 2,MapYSize);
            r1 := MaxFloat(s1/s2, s2/s1);
            r2 := MaxFloat(s1/s3, s3/s1);
            r3 := MaxFloat(s2/s3, s3/s2);
            Result := 100 * abs(1 - MaxFloat(r1,r2,r3));
         end;


         procedure tMapDraw.DrawLegendsOnMap(var Bitmap : tMyBitmap);
         var
            bmp,overlay : tMyBitmap;
            TStr : ANSIstring;
         begin
             if (Bitmap = Nil) then exit;
             if (LegendOverlayFName <> '') and FileExists(LegendOverlayFName) then begin
                Overlay := PetImage.LoadBitmapFromFile(LegendOverlayFName);
             end
             else begin
                CreateBitmap(Overlay,Bitmap.Width,Bitmap.Height);
                if MDDef.ScaleBarLocation.DrawItem and DrawScaleBarThisMap then begin
                   if MaxScaleDistortionOnMap < MDDef.ScalebarDistortionTolerable then begin
                      bmp := DrawScaleBarOnBitmap;
                      PositionBitmap(Overlay,bmp,MDDef.ScaleBarLocation.MapPosition);
                   end;
                end;

                if (DEMMap) and (DEMGlb[DEMonMap].LandCoverGrid) then begin
                end
                else begin
                   {$IfDef ExPointCloud}
                   {$Else}
                      if MDDef.LasLegend and LASlayerOnMap and (pt_cloud_opts_fm <> Nil) then begin
                         bmp := pt_cloud_opts_fm.LegendForPointCloudLayers;
                         if bmp <> Nil then PositionBitmap(Overlay,bmp,lpNEMap);
                      end;
                   {$EndIf}
                   if MDDef.GridLegendLocation.DrawItem and DrawLegendsThisMap then begin
                      bmp := DrawLegendOnBitmap;
                      PositionBitmap(Overlay,bmp,MDDef.GridLegendLocation.MapPosition);
                   end;
                   if MDDef.MapNameLocation.DrawItem then begin
                      CreateBitmap(bmp,500,50);
                      Petmar.LoadMyFontIntoWindowsFont(MDDef.TitleLabelFont,bmp.Canvas.Font);
                      bmp.Canvas.Brush.Color := clNearWhite;
                      bmp.Canvas.Brush.Style := bsSolid;
                      bmp.Canvas.Rectangle(0,0,bmp.Width,bmp.Height);

                       if (SatOnMap <> 0) then begin
                           if (SatImage[SatonMap].LandsatNumber in [1..9]) or SatImage[SatonMap].LandsatLook then begin
                              TStr := ShortLandsatName(SatImage[SatonMap].SceneBaseName);
                           end
                           else if SatImage[SatonMap].IsSentinel2 then begin
                              TStr := 'Sentinel-2  ' + Copy(SatImage[SatonMap].SceneBaseName,12,2) + '/' + Copy(SatImage[SatonMap].SceneBaseName,14,2) +  '/' + Copy(SatImage[SatonMap].SceneBaseName,8,4);
                           end
                           else begin
                              TStr := SatImage[SatonMap].SceneBaseName;
                              if copy(TStr,1,7) = 'Image: ' then Delete(TStr,1,7);
                           end;
                       end
                       else begin
                         TStr := BaseTitle;
                         if copy(TStr,1,6) = 'Grid: ' then Delete(TStr,1,6);
                       end;
                       {$IfDef RecordLegend} WriteLineToDebugFile('tMapDraw.DrawLegendsOnMap, title=' + TStr); {$EndIf}

                      TStr := RemoveUnderScores(TStr);
                      bmp.Canvas.TextOut(5,5,TStr);
                      bmp.Width := 10 + Bmp.Canvas.TextWidth(TStr);
                      bmp.Height := 10 + Bmp.Canvas.TextHeight(TStr);
                      bmp.Canvas.Brush.Style := bsClear;
                      bmp.Canvas.Rectangle(0,0,pred(bmp.Width),pred(bmp.Height));
                      PositionBitmap(Overlay,Bmp,MDDef.MapNameLocation.MapPosition);
                   end;
                   {$IfDef AllowDEMGeomorph}
                      if (MapOverlays.ovTerrainCat.Count > 0) and DrawLegendsThisMap and (MDDef.TerrainCatLegend.DrawItem) then begin
                         bmp := TerrainCategoryLegend;
                         PositionBitmap(Overlay,bmp,MDDef.TerrainCatLegend.MapPosition);
                      end;
                   {$EndIf}
                end;
                SaveLayerBitmap(Overlay,LegendOverlayFName);
             end;
             PlotDataBaseLegends(Bitmap);
             DrawAndDeleteOverlay(Bitmap,Overlay);
         end;

