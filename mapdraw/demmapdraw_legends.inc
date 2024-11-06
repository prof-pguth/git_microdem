{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for demmapdraw     }
{___________________________________}


function MakeChangeMapLegend(DEMonMap : integer) : tMyBitmap;
var
   TStr,MStr : shortstring;
   CatHeight,Cat : integer;
begin
   CreateBitmap(Result,1200,800);
   LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
   Result.Canvas.Font.Style := [fsBold];
   ClearBitmap(Result,clNearWhite);
   CatHeight := 6 * Result.Canvas.TextHeight('Wy') div 5;

   if true then begin
      Cat := 0;
   end
   else begin
      Cat := 1;
      Result.Canvas.TextOut(5,1,RemoveUnderscores(DEMGlb[DEMonMap].AreaName));
      Result.Canvas.TextOut(5,Cat*CatHeight + 4,'       % of area      Category');
      inc(Cat);
   end;
   MStr := ElevUnitsAre(DEMGlb[DEMonMap].DEMHeader.ElevUnits);
   Delete(MStr,1,4);

      Result.Canvas.Pen.Color := clLime;
      Result.Canvas.Brush.Color := clLime;
      Result.Canvas.Brush.Style := bsSolid;
      Result.Canvas.Rectangle(5,Cat*CatHeight,40,succ(Cat)*CatHeight);
      Result.Canvas.Brush.Style := bsClear;
      Tstr := RealToString(100-DEMGlb[DEMonMap].PercentileOfElevation(MDDef.TopCutLevel),9,2) + '%   Diff > ' + RealToString(MDDef.TopCutLevel,-8,-2) + MStr;
      Result.Canvas.TextOut(45,Cat*CatHeight + 4, TStr);

      inc(Cat);
      Result.Canvas.Pen.Color := clRed;
      Result.Canvas.Brush.Color := clRed;
      Result.Canvas.Brush.Style := bsSolid;
      Result.Canvas.Rectangle(5,Cat*CatHeight,40,succ(Cat)*CatHeight);
      Result.Canvas.Brush.Style := bsClear;
      TStr := RealToString(DEMGlb[DEMonMap].PercentileOfElevation(MDDef.BottomCutLevel),9,2) + '%  Diff < ' + RealToString(MDDef.BottomCutLevel,-8,-2) + MStr;
      Result.Canvas.TextOut(45,Cat*CatHeight + 4,TStr);

   PutBitmapInBox(Result);
end;


function tMapDraw.ChangeMapLegend : tMyBitmap;
begin
   Result := MakeChangeMapLegend(DEMonMap);
end;


function tMapDraw.MakeVATLegend : tMyBitmap;
var
   db,Cat,x,y,Cats,Total,Needed,z,i,CatHeight : integer;
   xg,fTotal : float64;
   fName : PathStr;
   zf : float32;
   aColor : tColor;
   CodeField : ShortString;
   Missing : int64;
   Hist : array[MinVATValue..MaxVatCats] of integer;
begin
   fName := DEMGlb[DEMOnMap].VATFileName;
   if FileExists(fName) then begin
      {$IfDef RecordVAT} WriteLineToDebugFile('tMapDraw.MakeVATLegend found=' + fName); {$EndIf}
   end
   else begin
      fName := DEMGlb[DEMOnMap].DEMFileName + '.vat.dbf';
      if not FileExists(fName) then fName := ChangeFileExt(DEMGlb[DEMOnMap].DEMFileName,'.vat.dbf');
      if not FileExists(fName) then fName := DEMGlb[DEMOnMap].DEMFileName + '.vat.csv';
      if not FileExists(fName) then begin
         {$IfDef RecordVAT} WriteLineToDebugFile('tMapDraw.MakeVATLegend did not find VAT file'); {$EndIf}
         exit;
      end;
      DEMGlb[DEMOnMap].VATFileName := fName;
   end;
   {$IfDef RecordVAT} WriteLineToDebugFile('tMapDraw.MakeVATLegend enter for=' + fName); {$EndIf}
   OpenNumberedGISDataBase(db, fName);

   if ValidDB(db) and (GISdb[db].MyData.RecordCount > 0) then begin
      if GISdb[db].MyData.FieldExists('CODE') then CodeField := 'CODE' else CodeField := 'VALUE';

      ShowHourglassCursor;
      for x := MinVATValue to MaxVATCats do Hist[x] := 0;
      Missing := 0;
      Total := 0;
      fTotal := 0;
      if not GISdb[db].MyData.FieldExists(GISdb[db].dbOpts.LabelField) then begin
         GISdb[db].dbOpts.LabelField := OrigPickField(GISdb[db].MyData,'Label',[ftString]);
      end;

      if true then begin
         for y := round(MapCorners.BoundBoxDataGrid.ymin) to round(MapCorners.BoundBoxDataGrid.ymax) do begin
            for x := round(MapCorners.BoundBoxDataGrid.xmin) to round(MapCorners.BoundBoxDataGrid.xmax) do begin
               if DEMGlb[DEMonMap].GetElevMetersOnGrid(x,y,zf) then begin
                  z := round(zf);
                  if (z >= MinVATValue) and (z <= MaxVatCats) then begin
                     Inc(Hist[z]);
                     Inc(Total);
                  end;
               end
               else inc(Missing);
            end;
         end;
         fTotal := Total;
         GISdb[db].MyData.First;
         while not GISdb[db].MyData.eof do begin
            i := GISdb[db].MyData.GetFieldByNameAsInteger(CodeField);
            GISdb[db].MyData.Edit;
            if (i >= 0) and (i <= MaxVATCats) then GISdb[db].MyData.SetFieldByNameAsInteger(GISdb[db].MyData.NCountField,Hist[i])
            else GISdb[db].MyData.SetFieldByNameAsInteger(GISdb[db].MyData.NCountField,0);
            GISdb[db].MyData.Next;
         end;
      end
      else begin
         while not GISdb[db].MyData.eof do begin
            fTotal := fTotal + GISdb[db].MyData.GetFieldByNameAsInteger(GISdb[db].MyData.NCountField);
            GISdb[db].MyData.Next;
         end;
      end;

      Needed := round(0.01 * GISdb[db].dbOpts.CatPCforLegend * fTotal);
      GISdb[db].MyData.ApplyFilter(GISdb[db].MyData.NCountField + ' > ' + IntToStr(Needed));

      Cats := GISdb[db].MyData.FiltRecsInDB;
      CreateBitmap(Result,1200,50*(Cats+2) + 4);
      LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
      CatHeight := 6 * Result.Canvas.TextHeight('Wy') div 5;
      Cat := 1;
      ClearBitmap(Result,clNearWhite);

      Result.Canvas.TextOut(5,1,RemoveUnderscores(BaseTitle));
      Result.Canvas.TextOut(5,Cat*CatHeight + 4,'    % Area     ' + GISdb[db].dbOpts.LabelField);
      while not GISdb[db].MyData.eof do begin
         xg := 100.0*GISdb[db].MyData.GetFieldByNameAsInteger(GISdb[db].MyData.NCountField) / fTotal;
         inc(Cat);
         aColor := GISdb[db].MyData.tColorFromTable;
         Result.Canvas.Pen.Color := aColor;
         Result.Canvas.Brush.Color := aColor;
         Result.Canvas.Brush.Style := bsSolid;
         Result.Canvas.Rectangle(5,Cat*CatHeight,40,(succ(Cat)*CatHeight) - 4);
         Result.Canvas.Brush.Style := bsClear;
         Result.Canvas.TextOut(45,Cat*CatHeight + 4, RealToString(xg,10,2) + '  ' + GISdb[db].MyData.GetFieldByNameAsString(GISdb[db].dbOpts.LabelField));
         GISdb[db].MyData.Next;
      end;
      PutBitmapInBox(Result);
   end;
   ShowDefaultCursor;
   CloseAndNilNumberedDB(db);
   {$IfDef RecordVAT} WriteLineToDebugFile('tMapDraw.MakeVATLegend out=' + fName); {$EndIf}
end;


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
         while not GazColors.EOF do begin
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

{$EndIf}


function TMapDraw.DrawLegendOnBitmap : tMyBitmap;
var
   Colors : tColors256;
   Radius,x,i,l,rad,SlopeBin : integer;
   AspectBin : integer;
   Hue,Angle : float64;
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
   {$If Defined(RecordMapDraw) or Defined(RecordMapType)} WriteLineToDebugFile('TMapDraw.DrawLegendOnBitmap in, maptype='+ IntToStr(MapType)); {$EndIf}

   Result := Nil;
   if ((MaxMapElev - MinMapElev) < 0.0001) and (MapType <> mtDEMVATTable) then exit;

   if (MapMerge in [mmElevation]) then begin
      if not DEMMap then ScaleMapElevationsToDEM;
      for i := 0 to 255 do begin
         Hue := (360.0 - (MDDef.MergeHue/255) * (i / 255 * 360.0));
         if (MapMerge = mmElevation) or (MapType in [mtIHSReflect]) then Values[i] := MinMapElev + i/255 * (MapZRange)
         else Values[i] := i/255 * (100 * MDDef.MaxMergeSlope);
         Colors[i] := ConvertPlatformColorToTColor(RGBtripFromHSI(Hue,MDDef.MergeSat,MDDef.MergeInt));
      end;
      if isSlopeMap(MapType) then TStr := '%'
      else TStr := ElevUnitsAre(DEMGlb[DEMOnMap].DEMheader.ElevUnits);

      {$IfDef RecordMapDraw} WriteLineToDebugFile('Going to draw legend, case 1'); {$EndIf}
      if MDDef.GridLegendLocation.HorizontalLegend then begin
         Result := HorizontalLegendOnBitmap(Colors,Values,TStr,'',MDDef.GridLegendLocation.LegendSize);
      end
      else begin
         Result := VerticalLegendOnBitmap(Colors,Values,TStr);
      end;
   end;

   if (MapType = mtGGRReflect) then begin
      Result := ChangeMapLegend;
   end
   else if (MapType = mtDEMVATTable) then begin
      {$If Defined(RecordVAT) or Defined(RecordMapType)} WriteLineToDebugFile('TMapDraw.DrawLegendOnBitmap (MapType = mtDEMVATTable)'); {$EndIf}
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
          TStr := ElevUnitsAre(DEMGlb[DEMOnMap].DEMheader.ElevUnits);
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

   if (MapType in [mtDEMaspect,mtFlowDir360,mtDEMAspectSlope]) or (DEMGlb[DEMonMap].DEMHeader.ElevUnits = euAspectDeg) then begin
      if (MapType in [mtDEMAspectSlope]) then TStr := 'Slope/aspect' else TStr := 'Aspect';
      Radius := 50 * MDDef.GridLegendLocation.LegendSize;
      CreateBitmap(Result,2*Radius+15,2*Radius+15);
      LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
      while Result.Canvas.TextWidth(TStr) > Result.Width do Result.Canvas.Font.Size := Result.Canvas.Font.Size - 1;

      Result.Height := 2 * Radius + 15 + Result.Canvas.TextHeight(TStr);
      ClearBitmap(Result,clNearWhite);
      xc := 5+Radius;
      yc := 5+Radius;
      if (MapType in [mtDEMAspectSlope]) then begin
         angle := 0;
         while Angle <= 360 do begin
            AspectBin := succ(ord(AspectDir8FromAspect(Angle)));
            //two times multiple to avoid holes
            for rad := 0 to 2 * Radius do begin
               if (Rad > 3 * 2 * Radius div 4) then Color := Rgb(161,161,161)
               else begin
                  if (Rad < 2 * Radius div 4) then SlopeBin := 3
                  else if (Rad < 2 * Radius div 2) then SlopeBin := 2
                  else if (Rad < 3 * 2 * Radius div 4) then SlopeBin := 1;
                  Color := SlopeAspectColor[SlopeBin,AspectBin];
               end;
               x := xc + round(Rad * SinDeg(Angle)) div 2;
               y := yc - round(Rad * CosDeg(Angle)) div 2;
               Result.Canvas.Pixels[x,y] := Color;
            end;
            Angle := Angle + 1 / MDDef.GridLegendLocation.LegendSize;
         end;
      end
      else begin
         for i := 1 to 360 do begin
            x := xc + round(Radius * SinDeg(i));
            y := yc - round(Radius * CosDeg(i));
            Color := ConvertPlatformColorToTColor(AspColor[i]);
            Result.Canvas.Pen.Color := Color;
            Result.Canvas.Pen.Width := 2;
            Result.Canvas.MoveTo(xc,yc);
            Result.Canvas.LineTo(x,y);
         end;
      end;
      Result.Canvas.Brush.Color := ClWhite;
      Result.Canvas.Brush.Style := bsClear;
      Result.Canvas.TextOut(5,2*Radius+8,TStr);
      {$IfDef RecordLegendDraw} Result.SaveToFile(MDTempDir + 'legend_step_1.bmp'); {$EndIf}
      PutBitmapInBox(Result);
      {$IfDef RecordLegendDraw} Result.SaveToFile(MDTempDir + 'legend_step_2.bmp'); {$EndIf}
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
                BinWidth := 25  * MDDef.GridLegendLocation.LegendSize;
                SlopeBands := MDDef.NumSlopeBands;
             end
             else begin
                BinWidth := 50 * MDDef.GridLegendLocation.LegendSize;
                SlopeBands := 4;
             end;
             CreateBitmap(Result,1500,500);
             ClearBitmap(Result,clNearWhite);
             LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
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
             end; {for i}
             GetImagePartOfBitmap(Result);
          end;
       end;
      {$IfDef RecordMapDraw} WriteLineToDebugFile('Slope legend OK');{$EndIf}
   end;
   {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapDraw.DrawLegendOnBitmap out');{$EndIf}
end;


procedure tMapDraw.DrawLegendsOnMap(var Bitmap : tMyBitmap);
var
   bmp,overlay : tMyBitmap;
   NewSize : integer;
   TStr : ANSIstring;
begin
    {$IfDef RecordMapDrawLegends} WriteLineToDebugFile('tMapDraw.DrawLegendsOnMap in');{$EndIf}
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
                if (bmp <> Nil) then PositionBitmap(Overlay,bmp,lpNEMap);
             end;
          {$EndIf}
          if MDDef.GridLegendLocation.DrawItem and DrawLegendsThisMap then begin
             bmp := DrawLegendOnBitmap;
             PositionBitmap(Overlay,bmp,MDDef.GridLegendLocation.MapPosition);
          end;
          if MDDef.MapNameLocation.DrawItem then begin
             bmp := nil;
             NewSize := 0;
             repeat
                if bmp <> Nil then BMP.Destroy;
                CreateBitmap(bmp,1500,50);
                Petmar.LoadMyFontIntoWindowsFont(MDDef.TitleLabelFont,bmp.Canvas.Font);
                bmp.Canvas.Font.Size := bmp.Canvas.Font.Size + NewSize;
                bmp.Canvas.Brush.Color := clNearWhite;
                bmp.Canvas.Brush.Style := bsSolid;
                bmp.Canvas.Rectangle(0,0,bmp.Width,bmp.Height);

                 if ValidSatImage(SatOnMap) then begin
                     if (SatImage[SatonMap].LandsatNumber in [1..9]) or SatImage[SatonMap].LandsatLook then begin
                        TStr := ShortLandsatName(SatImage[SatonMap].SceneBaseName) + '  ' + CurrentSatelliteColors;
                     end
                     else if SatImage[SatonMap].IsSentinel2 then begin
                        TStr := 'Sentinel-2  ' + SatImage[SatonMap].ImageDateString + '  ' + CurrentSatelliteColors;
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
                if Bmp.Width >= MapXSize then NewSize := NewSize - 1;
             until Bmp.Width < MapXSize;
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
    {$IfDef RecordMapDrawLegends} WriteLineToDebugFile('tMapDraw.DrawLegendsOnMap out');{$EndIf}
end;

