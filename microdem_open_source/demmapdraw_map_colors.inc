{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{ include file for demmapdraw        }
{____________________________________}


procedure TMapDraw.TerrainShadows(Bitmap : tMyBitmap);
var
   x,y : integer;
   Lat,Long: float64;
   BMPMem : tBMPMemory;
   Hue,Sat,Intens,   xdem,ydem  : float32;
begin
   {$IfDef RecordDrawLayers} WriteLineToDebugFile('TMapDraw.TerrainShadows in'); {$EndIf}
   if (TerrainShadowsDEM = 0) then TerrainShadowsDEM := DEMonMap;
   if ShowSatProgress then StartProgress('Terrain Shadows');
   BMPMem := tBMPMemory.Create(Bitmap);
   y := 0;
   while y < pred(MapYSize) do begin
      if ShowSatProgress and (y mod 200 = 0) then UpdateProgressBar(y/MapYSize);
      for x := 0 to pred(MapXSize) do begin
          if (TerrainShadowsDEM <> DEMonMap) then begin
             ScreenToLatLongDegree(x,y,Lat,Long);
             DEMGLB[TerrainShadowsDEM].LatLongDegreeToDEMGrid(Lat,Long,XDEM,YDEM);
          end
          else begin
             ScreenToDEMGrid(X,Y,XDEM,YDEM);
          end;
          HSIfromRGBTrip(BMPMem.GetPixelColor(x,y),Hue,Sat,Intens);
          if MDDef.ShadeOpts in [soReflectance] then Intens := DEMGlb[TerrainShadowsDEM].ReflectanceValueFloat(xdem,Ydem)
          else begin
             DEMGlb[TerrainShadowsDEM].GetElevMeters(xdem,Ydem, Intens);
             Intens := DEMGlb[TerrainShadowsDEM].ScaleZtoByte(Intens);
          end;
          BMPMem.SetPixelColor(x,y,RGBtripFromHSI(Hue,Sat,Intens));
     end;
     inc(y);
   end;
   EndProgress;
   BMPMem.Destroy;
end;


procedure TMapDraw.AnaglyphBitmap(var RightImage,LeftImage : PathStr; var Bitmap : tBitmap);
label
   EndRow;
var
   sbm : tMyBitmap;
   PixelShift,LastShift : integer;
   z : float32;
   i,x,y : integer;
   BMPMemOriginal,BMPMemShifted : tBMPMemory;
   {$IfDef FMX}
      Color : tAlphaColor;
      Region : tRectF;
   {$EndIf}
begin
   {$IfDef RecordAnaglyph} WriteLineToDebugFile('TMapDraw.AnaglyphBitmap in'); {$EndIf}
   RightImage := NextFileNumber(MDTempDir, 'right-image_', '.png');
   LeftImage := NextFileNumber(MDTempDir, 'left-image_','.png');
   {$IfDef RecordAnaglyph} if (MdDef.StereoMode in [smAnaglyph,smPair]) then WriteLineToDebugFile(LeftImage + ' and ' + RightImage); {$EndIf}
   if (MdDef.StereoMode in [smAnaglyph,smPair]) then begin
      {$IfDef VCL}
         Bitmap.Width := Bitmap.Width + MDDef.MaxAnaglyphShift;
         Bitmap.Canvas.Brush.Color := clBlack;
         Bitmap.Canvas.Rectangle(Bitmap.Width - MDDef.MaxAnaglyphShift,0,Bitmap.Width,Bitmap.Height);
      {$EndIf}

      {$IfDef FMX}
         CreateBitmap(sbm,Bitmap.Width + MDDef.MaxAnaglyphShift,Bitmap.Height);
         sbm.Canvas.BeginScene;
         Region.Create(0,0,pred(Bitmap.Width),pred(Bitmap.Height));
         sbm.Canvas.DrawBitmap(Bitmap,region,region,1);
         sbm.Canvas.EndScene;
         Bitmap.Assign(sbm);
         sbm.Free;
      {$EndIf}
   end;
   PetImage.SaveBitmap(Bitmap,LeftImage);
   CreateBitmap(sbm,Bitmap.Width,Bitmap.Height);

   {$IfDef VCL}
      PetImage.ClearBitmap(sbm,clBlack);
   {$EndIf}

   {$IfDef FMX}
      PetImage.ClearBitmap(sbm,claBlack);
   {$EndIf}

   {$IfDef RecordAnaglyph} WriteLineToDebugFile('MapZRange: ' + RealToString(MapZRange,-12,-2) + '   Anaglyph prep done'); {$EndIf}
   {$IfDef VCL} if ShowSatProgress then StartProgress('3D'); {$EndIf}

   if (MdDef.StereoMode in [smAnaglyph,smPair]) then begin
     BMPMemOriginal := tBMPMemory.Create(Bitmap);
     BMPMemShifted  := tBMPMemory.Create(sbm);

      for y := 0 to pred(MapYSize) do begin
         {$IfDef RecordAnaglyphLoop} WriteLineToDebugFile('y=' + y.ToString);   {$EndIf}
         if ShowSatProgress then UpdateProgressBar(y/MapYSize);

         LastShift := 999;
         for x := 0 to  pred(MapXSize) do begin
            if ScreenToElev(x,y,z) then begin
               PixelShift := round((z-MinMapElev) * MDDef.MaxAnaglyphShift / MapZRange);
               if MDDef.EWAnaglyphShift then begin
                  BMPMemOriginal.CurrentColor := BMPMemOriginal.GetPixelColor(x,y);
                  if (LastShift < PixelShift) then begin
                     for i := LastShift to PixelShift do BMPMemShifted.SetPixelColor(x+i,y, BMPMemOriginal.CurrentColor);
                  end
                  else begin
                     BMPMemShifted.SetPixelColor(x+PixelShift,y, BMPMemOriginal.CurrentColor);
                 end;
               end;
               LastShift := PixelShift;
            end;
         end;
         {$IfDef RecordAnaglyphLoop} WriteLineToDebugFile('Loop Done'); {$EndIf}
      end;
   end;

   BMPMemOriginal.Destroy;
   BMPMemShifted.Destroy;

   {$IfDef RecordAnaglyph} WriteLineToDebugFile('Two images created'); {$EndIf}

   if ShowSatProgress then EndProgress;

   if (MdDef.StereoMode in [smAnaglyph,smPair]) then begin
      PetImage.SaveBitmap(sbm,RightImage);
      if (MdDef.StereoMode in [smAnaglyph]) then begin
         FreeAndNil(Bitmap);
         Bitmap := AnaglyphFromTwoBitmaps(LeftImage,RightImage);
      end;
   end;
   FreeAndNil(sbm);
   {$IfDef RecordAnaglyph} WriteLineToDebugFile('Anaglyph out'); {$EndIf}
end;


procedure TMapDraw.DrawElevationMapStrip(Limits : tGridLimits; var BMPMemory :  tBMPMemory);
var
   i,j   : integer;
   xr,yr : float32;
   Color : tPlatFormColor;
   RefMap : boolean;
begin
   RefMap := IsReflectanceMap(MapType);
   for j := Limits.YGridLow to Limits.YGridHigh do begin
      for i := Limits.XGridLow to Limits.XGridHigh do begin
         ScreenToDEMGrid(i,j,xr,yr);
         if DEMGlb[DEMonMap].GridInDataSet(xr,yr) then begin
            if RefMap then Color := DEMGlb[DEMonMap].RGBReflectanceColor(MapType,round(xr),round(yr))
            else Color := GetElevColor(xr,yr);
            BMPMemory.SetPixelColor(i,j,Color);
         end;
      end;
      {$IfDef VCL}
         TInterlocked.Increment(BMPMemory.NumDone);
         if (CountInStrips mod 500 = 0) and ShowSatProgress then UpdateProgressBar(BMPMemory.NumDone/BMPMemory.NumToDo);
      {$EndIf}
   end {for j};
end;


procedure TMapDraw.ColorTintedElevationMap(var inBitmap : tMyBitmap);
var
   BMPMemory : tBMPMemory;
   {$IfDef NoParallelFor}
   {$Else}
      Limits : tGridLimitsArray;
   {$EndIf}
begin
   {$If Defined(RecordElevationScaling)} WriteLineToDebugFile('TMapDraw.SetFullMapCoverage ' + MapZRangeString); {$EndIf}
   (*
   if (MapZRange = 0) then begin
      {$If Defined(RecordTiming) or Defined(RecordKeyMap)} WriteLineToDebugFile('TMapDraw.ColorTintedElevationMap out for pancake grid'); {$EndIf}
      exit;
   end;
   *)
   {$If Defined(RecordFullMapDraw) or Defined(RecordMapDraw) or Defined(RecordTiming) or Defined(RecordElevationScaling) or Defined(RecordMapLayers)}
      WriteLineToDebugFile('TMapDraw.ColorTintedElevationMap in, maptype=' + IntToStr(Ord(MapType)) + MapZRangeString + '  ' +  MapSizeString);
   {$EndIf}

   SetUpElevationColorTable;
   {$If Defined(RecordTiming)} WriteLineToDebugFile('TMapDraw.ColorTintedElevationMap Elev table set up'); {$EndIf}
   BMPMemory := tBMPMemory.Create(inBitmap);
   BMPMemory.NumToDo := MapYSize;

   {$IfDef NoParallelFor}
      DrawElevationMapStrip(MapLimits,BMPMemory);
   {$Else}
      {$If Defined(RecordTiming) or Defined(RecordKeyMap)} WriteLineToDebugFile('TMapDraw.ColorTintedElevationMap start parallel loops, using threads=' + IntToStr(MDdef.MaxThreadsForPC)); {$EndIf}
      Limits := GetLimitsForParallelLoops(MapLimits);
      TParallel.For(1,MDdef.MaxThreadsForPC,
          procedure (Value: Integer)
          begin
            DrawElevationMapStrip(Limits[Value],BMPMemory);
          end);
      ThreadsWorking := false;
   {$EndIf}
   BMPMemory.Destroy;
   {$If Defined(RecordFullMapDraw) or Defined(RecordMapDraw) or Defined(RecordTiming) or Defined(RecordElevationScaling) or Defined(RecordMapLayers)} WriteLineToDebugFile('TMapDraw.ColorTintedElevationMap out'); {$EndIf}
end {proc ColorTintedElevationMap};


procedure TMapDraw.DrawSlopeMapStrip(Limits : tGridLimits; var BMPMemory :  tBMPMemory);
var
   SlopeVal,i,j,xg,yg,SlopeBin,AspectBin : integer;
   GrayRange : integer;
   SlopeAspectRec : tSlopeAspectRec;
   SlopeRange,z : float32;
   Color : tColor;
begin
   GrayRange := MDDef.SlopeMaxGray - MDDef.SlopeMinGray;
   SlopeRange := MDDef.MaxSlopeForGray-MDDef.MinSlopeForGray;
   for j := Limits.YGridLow to Limits.YGridHigh do begin
      for i := 0 to pred(MapXSize) do begin
         if OnScreen(i,j) then begin
            ScreenToDEMGrid(i,j,XG,yg);
            if (DEMGlb[DEMonMap].DEMheader.ElevUnits in [AspectDeg]) or (MapType = mtFlowDir360)  then begin
               if DEMGlb[DEMonMap].GetElevMeters(xg,yg,z) then BMPMemory.SetPixelColor(i,j,AspColor[round(z)]);
            end
            else if DEMGlb[DEMonMap].GetSlopeAndAspect(xg,yg,SlopeAspectRec) then begin
               if (MapType in [mtDEMaspectSlope]) then begin
                  Color := Rgb(161,161,161);
                  if (SlopeAspectRec.SlopePercent > 5) then begin
                     if (SlopeAspectRec.SlopePercent < 20) then SlopeBin := 1
                     else if SlopeAspectRec.SlopePercent < 40 then SlopeBin := 2
                     else SlopeBin := 3;
                     AspectBin := succ(ord(SlopeAspectRec.Dir));
                     if AspectBin in [1..8] then Color := SlopeAspectColor[SlopeBin,AspectBin];
                  end;
                  BMPMemory.SetPixelColor(i,j,ConvertTColorToPlatformColor(Color));
               end
               else if isSlopeMap(MapType) or (MapMerge = mmSlope) then begin
                  if (MapType = mtSlopeGrayScaleReversed) then begin
                     ValueInRange(SlopeAspectRec.SlopePercent,MDDef.MinSlopeForGray,MDDef.MaxSlopeForGray);
                     SlopeVal := ValidByteRange254(MDDef.SlopeMaxGray - round(GrayRange * (SlopeAspectRec.SlopePercent-MDDef.MinSlopeForGray) / SlopeRange));
                     BMPMemory.SetPixelColor(i,j,GrayRGBtrip(SlopeVal));
                  end
                  else begin
                     if MapType in [mtSlopeStandardCats,mtSlopeTrafficCats,mtSlopeGoNoGo] then SlopeVal := round(SlopeAspectRec.SlopePercent)
                     else SlopeVal := round(SlopeAspectRec.SlopePercent / MDDef.MaxSlopeOnMaps * 254);
                     BMPMemory.SetPixelColor(i,j,SlopeColorChoices[ValidByteRange254(SlopeVal)]);
                  end;
               end
               else if (MapType = mtDEMaspect) or (MapMerge = mmAspect) then begin
                  if (SlopeAspectRec.AspectDir < pred(MaxSmallInt)) then begin
                     BMPMemory.SetPixelColor(i,j,AspColor[round(SlopeAspectRec.AspectDir)]);
                  end;
               end;
            end;
         end;
      end {for i};
      {$IfDef VCL}
         TInterlocked.Increment(BMPMemory.NumDone);
         if (CountInStrips mod 500 = 0) and ShowSatProgress then begin
            UpdateProgressBar(BMPMemory.NumDone/BMPMemory.NumToDo);
         end;
      {$EndIf}
   end {for j};
end;


procedure TMapDraw.DrawSlopeMap(var Bitmap : tMyBitmap);
var
   BMPMemory :  tBMPMemory;
   {$IfDef NoParallelFor}
   {$Else}
      Limits : tGridLimitsArray;
   {$EndIf}
begin
   {$If Defined(RecordMapDraw) or Defined(RecordMapLayers)} WriteLineToDebugFile('enter TMapDraw.DrawSlopeMap, bmp=' +  MapSizeString); {$EndIf}
   DefineSlopeColors;
   BMPMemory := tBMPMemory.Create(Bitmap);
   BMPMemory.NumToDo := MapYSize;

   {$IfDef NoParallelFor}
      DrawSlopeMapStrip(MapLimits,BMPMemory);
   {$Else}
      Limits := GetLimitsForParallelLoops(MapLimits);
      TParallel.For(1,MDdef.MaxThreadsForPC,
          procedure (Value: Integer)
          begin
            DrawSlopeMapStrip(Limits[Value],BMPMemory);
         end);
      ThreadsWorking := false;
   {$EndIf}
   BMPMemory.Destroy;
   {$IfDef RecordMapDraw} WriteLineToDebugFile('exit DrawSlopeMap, bmp=' +  MapSizeString); {$EndIf}
end;


procedure TMapDraw.DrawOpennessMapStrip(Limits : tGridLimits; var BMPMemory :  tBMPMemory);
var
   Val,i,j,xg,yg : integer;
   GrayRange : integer;
   UpRange,
   Upward,Downward : float64;
begin
   GrayRange := MDDef.OpenMaxGray - MDDef.OpenMinGray;
   UpRange := MDDef.MaxUpward-MDDef.MinUpward;
   for j := Limits.YGridLow to Limits.YGridHigh do begin
      for i := 0 to pred(MapXSize) do begin
         if OnScreen(i,j) then  begin
            ScreenToDEMGrid(i,j,XG,yg);
            if DEMGlb[DEMonMap].FigureOpenness(xg,yg,round(5*DEMGlb[DEMonMap].AverageSpace),Upward,Downward) then begin
               ValueInRange(Upward,MDDef.MinUpward,MDDef.MaxUpward);
               Val := ValidByteRange254(MDDef.OpenMinGray + round(GrayRange * (Upward-MDDef.MinUpward) / UpRange));
               BMPMemory.SetPixelColor(i,j,GrayRGBtrip(Val));
            end;
         end;
      end {for i};
      {$IfDef VCL}
      TInterlocked.Increment(BMPMemory.NumDone);
      if (CountInStrips mod 500 = 0) and ShowSatProgress then begin
         UpdateProgressBar(BMPMemory.NumDone/BMPMemory.NumToDo);
      end;
      {$EndIf}
   end {for j};
end;


procedure TMapDraw.DrawOpennessMap(var Bitmap : tMyBitmap);
var
   BMPMemory :  tBMPMemory;
   {$If Defined(NoParallelFor) or Defined(NoParallelOpenness)} {$Else} Limits : tGridLimitsArray;  {$EndIf}
begin
   {$If Defined(RecordMapDraw) or Defined(RecordMapLayers)} WriteLineToDebugFile('enter TMapDraw.DrawOpennessMap, bmp=' +  MapSizeString); {$EndIf}
   BMPMemory := tBMPMemory.Create(Bitmap);
   BMPMemory.NumToDo := MapYSize;

   {$If Defined(NoParallelFor) or Defined(NoParallelOpenness)}
      DrawOpennessMapStrip(MapLimits,BMPMemory);
   {$Else}
      Limits := GetLimitsForParallelLoops(MapLimits);
      TParallel.For(1,MDdef.MaxThreadsForPC,
          procedure (Value: Integer)
          begin
            DrawOpennessMapStrip(Limits[Value],BMPMemory);
         end);
      ThreadsWorking := false;
   {$EndIf}
   BMPMemory.Destroy;
   {$IfDef RecordMapDraw} WriteLineToDebugFile('exit DrawOpennessMap, bmp=' +  MapSizeString);   {$EndIf}
end;


procedure TMapDraw.DrawLASclassMap(var Bitmap : tMyBitmap);
{$IfDef ExPointCloud}
begin
{$Else}
var
   BMPMemory :  tBMPMemory;
   i,j,zi : integer;
   xg,yg : integer;
   z : float32;
begin
   {$If Defined(RecordMapDraw) or Defined(RecordMapLayers)} WriteLineToDebugFile('enter TMapDraw.DrawOpennessMap, bmp=' +  MapSizeString); {$EndIf}
   BMPMemory := tBMPMemory.Create(Bitmap);
   BMPMemory.NumToDo := MapYSize;
   for j := 0 to pred(MapYSize) do begin
      for i := 0 to pred(MapXSize) do begin
         if OnScreen(i,j) then  begin
            ScreenToDEMGrid(i,j,XG,yg);
            if DEMGlb[DEMonMap].GetElevMeters(xg,yg,z) then begin
               zi :=round(z);
               if LasCatMapped[zi] then BMPMemory.SetPixelColor(i,j,Las_rgb_colors[zi]);
             end;
         end;
      end {for i};
   end;
   BMPMemory.Destroy;
   {$IfDef RecordMapDraw} WriteLineToDebugFile('exit DrawOpennessMap, bmp=' +  MapSizeString); {$EndIf}
{$EndIf}
end;


procedure TMapDraw.DrawRGBMap(var Bitmap : tMyBitmap);
var
   BMPMemory :  tBMPMemory;
   i,j,xg,yg : integer;
   r,g,b : byte;
begin
   {$If Defined(RecordMapDraw) or Defined(RecordMapLayers)} WriteLineToDebugFile('enter TMapDraw.DrawOpennessMap, bmp=' +  MapSizeString + '   DEM=' + IntToStr(DEMonMap)); {$EndIf}
   BMPMemory := tBMPMemory.Create(Bitmap);
   BMPMemory.NumToDo := MapYSize;
   for j := 0 to pred(MapYSize) do begin
      for i := 0 to pred(MapXSize) do begin
         if OnScreen(i,j) then  begin
            ScreenToDEMGrid(i,j,XG,yg);
            if DEMGlb[DEMonMap].GridInDataSet(xg,yg) then begin
              DEMGlb[DEMonMap].RGBfromLongWord(xg,yg,r,g,b);
              BMPMemory.SetPixelRGB(i,j,r,g,b);
            end;
         end;
      end {for i};
   end;
   BMPMemory.Destroy;
   {$IfDef RecordMapDraw} WriteLineToDebugFile('exit DrawOpennessMap, bmp=' +  MapSizeString); {$EndIf}
end;


 procedure TMapDraw.SecondGridStrip(Limits : tGridLimits; BMPMemory :  tBMPMemory);
var
   x,y : integer;
   xg,yg,Lat,Long : float64;
   z : float32;
   pUse : tRGBTriple;
begin
   for y := Limits.YGridLow to Limits.YgridHigh do begin
      {$IfDef VCL}
         TInterlocked.Increment(BMPMemory.NumDone);
         if (CountInStrips mod 100 = 0) and ShowSatProgress then begin
            UpdateProgressBar(BMPMemory.NumDone/BMPMemory.NumToDo);
         end;
      {$EndIf}

      for x := Limits.XGridLow to Limits.XGridHigh do begin
         ScreenToLatLongDegree(x,y,Lat,Long);
         if DEMGlb[DEM2onMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
            if MDDef.FuzzyMatches then begin
               DEMGlb[DEM2onMap].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
               pUse := DEMGlb[DEM2onMap].SelectionMap.MapDraw.GetElevColor(xg,yg);
               BMPMemory.SetPixelColor(x,y,pUse);
            end
            else begin
               BMPMemory.SetPixelColor(x,y,MDDef.MapMaskColor);
            end;
         end;
      end;
   end;
end;


 procedure TMapDraw.DrawSecondGrid(var Bitmap : tMyBitmap; Grid2 : integer);
 {$IfDef ExAdvancedGIS}
 begin
 {$Else}

      function GetClassColor(xg,yg : float64; var puse : tPlatformColor) : boolean;
      var
         zi : integer;
         z : float32;
      begin
         Result := false;
         if DEMGlb[DEM2onMap].GetElevMeters(round(xg),round(yg),z) then begin
            zi := round(z);
            if ((not GroundOnly) or (zi=2)) then begin
               pUse := Las_rgb_colors[zi];
               Result := true;
            end;
          end;
      end;


    function ChangeColor(xg,yg : float64; var pUse :  tRGBTriple) : boolean;
    var
       z : float32;
    begin
       Result := false;
       if DEMGlb[DEM2onMap].GetElevMeters(xg,yg,z) then begin
          if (z > MDDef.ChangeMinGreenValue) then begin
              ValueInRange(z,MDDef.ChangeMinGreenValue,MDDef.ChangeMaxGreenValue);
              pUse.rgbtRed := 0;
              pUse.rgbtBlue := 0;
              pUse.rgbtGreen := ValidByteRange254(MDDef.ChangeMinGreenColor - round((MDDef.ChangeMaxGreenColor-MDDef.ChangeMinGreenColor) * (z-MDDef.ChangeMinGreenValue) / (MDDef.ChangeMaxGreenValue-MDDef.ChangeMinGreenValue)));
              Result := true;
          end
          else begin
             z := abs(z);
             if (z > MDDef.ChangeMinRedValue) then begin
                 ValueInRange(z,MDDef.ChangeMinRedValue,MDDef.ChangeMaxRedValue);
                 pUse.rgbtRed := ValidByteRange254(MDDef.ChangeMinRedColor - round((MDDef.ChangeMaxRedColor-MDDef.ChangeMinRedColor) * (z-MDDef.ChangeMinRedValue) / (MDDef.ChangeMaxRedValue-MDDef.ChangeMinRedValue)));
                 pUse.rgbtBlue := 0;
                 pUse.rgbtGreen := 0;
                 Result := true;
             end;
          end;
       end;
    end;


          function MapLayerFromSecondGrid : tMyBitmap;
          var
             x,y : integer;
             Lat,Long,xg,yg : float64;
             z : float32;
             pUse : tRGBTriple;
             BMPMemory : tBMPMemory;
             {$IfDef NoParallelFor}
             {$Else}
                Limits : tGridLimitsArray;
             {$EndIf}
          begin
             {$If Defined(RecordDrawSecondGrid) or Defined(RecordMapDraw) or Defined(RecordMapLayers)} WriteLineToDebugFile('TMapDraw.MapLayerFromSecondGrid, dem=' + IntToStr(DEM2onMap)); {$EndIf}
                CreateBitmap(Result,MapXSize,MapYSize);
                pUse := MDDef.MapMaskColor;
                BMPMemory := tBMPMemory.Create(Result);
                BMPMemory.NumToDo := MapYSize;

                if false and MDDef.SinglePixel and (DEMGlb[DEM2onMap].AverageSpace < ScreenPixelSize) then begin
                   {$IfDef NoParallelFor}
                      SecondGridStrip(MapLimits,BMPMemory);
                   {$Else}
                      Limits := GetLimitsForParallelLoops(MapLimits);
                      TParallel.For(1,MDdef.MaxThreadsForPC,
                          procedure (Value: Integer)
                          begin
                             SecondGridStrip(Limits[Value],BMPMemory);
                          end);
                      ThreadsWorking := false;
                   {$EndIf}
                end
                else begin
                   pUse.rgbtRed := 0;
                   pUse.rgbtBlue := 0;
                   pUse.rgbtGreen := 0;
                   if false and DEMGlb[DEMonMap].SecondGridIdentical(DEM2onMap) then begin
                   end
                   else begin
                     for y := 0 to pred(MapYSize) do begin
                        if (y mod 10 = 0) then UpdateProgressBar( y / MapYSize);
                        for x := 0 to pred(MapXSize) do begin
                           ScreenToLatLongDegree(x,y,Lat,Long);
                           if DEMGlb[DEM2onMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
                              DEMGlb[DEM2onMap].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
                              if (DEM2onMap = LasClassGrid) then begin
                                 if GetClassColor(xg,yg,puse) then begin
                                    BMPMemory.SetPixelColor(x,y,pUse);
                                 end;
                              end
                              else if (DEM2onMap = FeatureGrid) then begin
                                 pUse := DEMGlb[DEM2onMap].SelectionMap.MapDraw.GetElevColor(xg,yg);
                                 BMPMemory.SetPixelColor(x,y,pUse);
                              end
                              else if (DEM2onMap = CHMGrid) then begin
                                  if DEMGlb[DEM2onMap].GetElevMeters(xg,yg,z) and (z > 2) then begin
                                     pUse.rgbtGreen := ValidByteRange(100 + round(155 * (z-2) / (25-2)));
                                     BMPMemory.SetPixelColor(x,y,pUse);
                                  end;
                              end
                              else if (DEM2onMap = ChangeGrid) then begin
                                  if ChangeColor(xg,yg,pUse) then begin
                                     BMPMemory.SetPixelColor(x,y,pUse);
                                  end;
                              end
                              else begin
                                 pUse := DEMGlb[DEM2onMap].SelectionMap.MapDraw.GetElevColor(xg,yg);
                                 BMPMemory.SetPixelColor(x,y,pUse);
                              end;
                           end;
                        end;
                      end;
                   end;
                end;
                BMPMemory.Destroy;
                {$IfDef RecordDrawSecondGrid} Result.SaveToFile(MDtempDir + 'drawsecondgrid.bmp'); {$EndIf}
          end;

          procedure ColorizeInPlace;
          var
             x,y{,zi} : integer;
             Lat,Long,xg,yg : float64;
             pUse : tRGBTriple;
             BMPMemory : tBMPMemory;
          begin
             {$If Defined(RecordDrawSecondGrid) or Defined(RecordMapDraw) or Defined(RecordMapLayers)} WriteLineToDebugFile('TMapDraw.MapLayerFromSecondGrid, dem=' + IntToStr(DEM2onMap)); {$EndIf}
             if (DEM2onMap <> DEMonMap) then begin
                BMPMemory := tBMPMemory.Create(Bitmap);
                BMPMemory.NumToDo := MapYSize;
                for y := 0 to pred(MapYSize) do begin
                  if (y mod 10 = 0) then UpdateProgressBar( y / MapYSize);
                  for x := 0 to pred(MapXSize) do begin
                     ScreenToLatLongDegree(x,y,Lat,Long);
                     DEMGlb[DEM2onMap].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
                     if (DEM2onMap = LasClassGrid) then begin
                        if GetClassColor(xg,yg,puse) then begin
                           BMPMemory.ColorizePixelRGB(x,y,pUse);
                        end;
                     end;
                     if DEM2onMap = ChangeGrid then begin
                         if ChangeColor(xg,yg,pUse) then begin
                            BMPMemory.ColorizePixelRGB(x,y,pUse);
                         end;
                     end;
                   end;
                end;
                BMPMemory.Destroy;
                {$IfDef RecordDrawSecondGrid} Result.SaveToFile(MDtempDir + 'drawsecondgrid.bmp'); {$EndIf}
             end;
          end;

 var
    Overlay : tMyBitmap;
 begin
    {$If Defined(RecordDrawSecondGrid) or Defined(RecordMapDraw) or Defined(RecordMapLayers)} WriteLineToDebugFile('TMapDraw.DrawSecondGrid, opacity=' + IntToStr(MDDef.SecondGridOpacity) + ' fname=' + SecondGridfName); {$EndIf}

    DEM2onMap := Grid2;
    if not ValidDEM(DEM2onMap) then exit;
    StartProgress('Grid 2 overlay');
    if MDdef.ColorizeInPlace and ((DEM2onMap = LasClassGrid) or (DEM2onMap = ChangeGrid) )then begin
       ColorizeInPlace;
    end
    else begin
       if (SecondGridfName <> '') and (FileExists(SecondGridfName))then begin
          Overlay := PetImage.LoadBitmapFromFile(SecondGridfName);
       end
       else begin
          Overlay := MapLayerFromSecondGrid;
          SecondGridfName := NextFileNumber(MDTempDir, 'grid2_ovelay', OverlayFExt);
          PetImage.SaveBitmap(Overlay,SecondGridfName);
       end;
       {$If Defined(RecordDrawSecondGrid) or Defined(RecordMapDraw) or Defined(RecordMapLayers)} Overlay.SaveToFile(MDtempDir + 'draw_2nd_grid_overlay.bmp'); {$EndIf}
       DrawAndDeleteOverlay(Bitmap,Overlay,MDDef.SecondGridOpacity);
    end;
    EndProgress;
{$EndIf}
 end;



procedure TMapDraw.SetUpElevationColorTable;
var
   i,tf : integer;
   r,g,b : byte;
   ColorTable : tMyData;

         procedure ColorsForLegends;
         var
            i : integer;
         begin
            for i := 0 to 255 do pPal[i] := ElevColorFromZ(MinMapElev + i/255 * (MaxMapElev - MinMapElev));
         end;

begin
   if (DEMonMap = 0) then exit;
   {$IfDef VCL}
      if (MapType = mtDEMVATTable) then begin
         DEMGlb[DEMonMap].LoadColorVATTable(VATColors);
         exit;
      end;
   {$EndIf}
   pPal[0] := MDDef.MissingDataColor;

   if IsReflectanceMap(MapType) then begin
      {$If Defined(RecordFullMapDraw) or Defined(RecordKeyMap)} WriteLineToDebugFile('Set hillshade colors'); {$EndIf}
      {$If Defined(RecordRefMapColors)}
         WriteLineToDebugFile('Set hillshade colors, hue=' + IntToStr(MDDef.MergeHue) + ' sat=' + IntToStr(MDDef.MergeSat) + ' int=' + IntToStr(MDDef.MergeInt));
      {$EndIf}
      InterpolateMapColors := (ScreenPixelSize < DEMGlb[DEMonMap].AverageSpace);
      DEMGlb[DEMonMap].ReflectanceParams(MinMapElev,MaxMapElev);
      for i := 0 to 255 do pPal[i] := RGBtripFromHSI(TheHue(MDDef.MergeHue,i),MDDef.MergeSat,MDDef.MergeInt);
   end
   else if (MapType in [mtFlowDir360,mtFlowDirArc,mtFlowDirTau]) then begin
      for I := 0 to 255 do pPal[i] := MDdef.MissingDataColor;
      if (MapType = mtFlowDir360) then begin
         DefineSlopeColors;
      end
      else if (MapType = mtFlowDirArc) then begin
         pPal[1] := AspColorE;
         pPal[2] := AspColorSE;
         pPal[4] := AspColorS;
         pPal[8] := AspColorSW;
         pPal[16] := AspColorW;
         pPal[32] := AspColorNW;
         pPal[64] := AspColorN;
         pPal[128] := AspColorNE;
      end
      else if (MapType = mtFlowDirTau) then begin
         pPal[1] := AspColorE;
         pPal[8] := AspColorSE;
         pPal[7] := AspColorS;
         pPal[6] := AspColorSW;
         pPal[5] := AspColorW;
         pPal[4] := AspColorNW;
         pPal[3] := AspColorN;
         pPal[2] := AspColorNE;
      end;
   end

   {$IfDef ExNLCD}
   {$Else}
   else if (MapType = mtLandCover) then begin
      {$IfDef RecordNLCD} WriteLineToDebugFile('LandCoverGrid on DEM Map'); {$EndIf}
      for I := 1 to MaxLandCoverCategories do begin
         if DEMGlb[DEMonMap].NLCDCats^[i].UseCat then begin
            {$IfDef RecordNLCD} WriteLineToDebugFile(IntegerToString(i,3) + IntegerToString(DEMGlb[DEMonMap].NLCDCats^[i].Color,12)); {$EndIf}
            pPal[i] := DEMGlb[DEMonMap].NLCDCats^[i].Color;
         end
         else pPal[i] := MDDef.MissingDataColor;
      end;
   end
   {$EndIf}
   else if (MapType in [mtElevDefinedPalette]) then begin
      if not DefineColorTableValues(MDDef.ElevPalName,MinMapElev,MaxMapElev,ZColorTable) then exit;
      ColorsForLegends;
   end
   else if (MapType = mtElevFromTable) then begin
      {$IfDef RecordElevColorFromTable} WriteLineToDebugFile('MapType = mtElevFromTable starting maps w/ ' + ElevationDBaseTable); {$EndIf}
      if FileExists(HardLimitColorPaletteFName) then begin
         ColorTable := tMyData.Create(HardLimitColorPaletteFName);
         ColorTable.ApplyFilter('PALETTE=' + QuotedStr(ElevationFixedPalette));
         {$IfDef RecordElevColorFromTable} WriteLineToDebugFile('Table open, ' + ElevationFixedPalette); {$EndIf}
         tf := 1;
         if (ColorTable.FiltRecsInDB > 256) then begin
            New(ZColorTable.ZBigTableValue);
            New(ZColorTable.ZBigTableColors);
         end
         else begin
            ZColorTable.ZBigTableValue := Nil;
            ZColorTable.ZBigTableColors := Nil;
            while (ColorTable.FiltRecsInDB div tf > 255) do inc(tf);
         end;

         {$IfDef RecordElevColorFromTable} WriteLineToDebugFile('Parse table, recs=' + IntToStr(ColorTable.FiltRecordsInDB)); {$EndIf}

         ZColorTable.ZTableEntries := 0;
         while (not ColorTable.Eof) do begin
            inc(ZColorTable.ZTableEntries);
            if (ColorTable.FiltRecsInDB > 256) then begin
               ZColorTable.zBigTableValue^[ZColorTable.ZTableEntries] := ColorTable.GetFieldByNameAsFloat('MAX');
               if (ZColorTable.zBigTableValue^[ZColorTable.ZTableEntries] >= MinMapElev) then begin
                  ZColorTable.zBigTableColors^[ZColorTable.ZTableEntries] := ColorTable.PlatformColorFromTable;
               end
               else dec(ZColorTable.ZTableEntries);
            end
            else begin
               ZColorTable.zTableValue[ZColorTable.ZTableEntries] := ColorTable.GetFieldByNameAsFloat('MAX');
               if (ZColorTable.zTableValue[ZColorTable.ZTableEntries] >= MinMapElev) then begin
                  ZColorTable.zTableColors[ZColorTable.ZTableEntries] := ColorTable.PlatformColorFromTable;
                  ZColorTable.zTableLabels[ZColorTable.ZTableEntries] := ColorTable.GetFieldByNameAsString('NAME');
               end
               else dec(ZColorTable.ZTableEntries);
               {$IfDef RecordElevColorFromTable} WriteLineToDebugFile(IntToStr(ZTableEntries) + '  ' + IntToStr(zTableColors[ZTableEntries].rgbtRed) + '/' + IntToStr(zTableColors[ZTableEntries].rgbtGreen) + '/' + IntToStr(zTableColors[ZTableEntries].rgbtBlue)  ) ;  {$EndIf}
            end;
            for i := 1 to tf do ColorTable.Next;
         end;
         ColorTable.Destroy;
         ColorsForLegends;
         {$IfDef RecordElevColorFromTable} WriteLineToDebugFile('Complete table'); {$EndIf}
      end;
   end
   else if MapType in [mtElevGrayReversed] then begin
      for i := 0 to 255 do pPal[i] := GrayRGBTrip(255-i);
   end
   else if MapType in [mtElevGray] then begin
      for i := 0 to 255 do pPal[i] := GrayRGBTrip(i);
      //0=gray
      //1=red
      //2=green
      //3=blue
      //4=cyan  blue and green
      //5=yellow   red and green
      //6=magenta   red and blue
      for i := 0 to 255 do begin
         if MDDef.MonochromeColor in [0,1,5,6] then begin
            if MDDef.InvertGrayscale then r := 255 - i else r := i;
         end
         else r := 0;

         if MDDef.MonochromeColor in [0,2,4,5] then begin
            if MDDef.InvertGrayscale then g := 255 - i else g := i;
         end
         else g := 0;
         if MDDef.MonochromeColor in [0,3,4,6] then begin
            if MDDef.InvertGrayscale then b := 255 - i else b := i;
         end
         else b := 0;
         pPal[i] := RGBtrip(r,g,b);
      end;

   end
   else if (MapType = mtElevContrast) then begin
      {$IfDef VCL}
         for i := 0 to 255 do pPal[i] := ConvertTColorToPlatformColor(WinGraphColors[i mod 16]);
      {$EndIf}
   end
   {$IfDef ExGeology}
   {$Else}
      else if DEMGlb[DEMonMap].DEMFileName = PredAgesFile then begin
         for i := 0 to 253 do pPal[i] := SpectrumRGBFunct(254-i,1,254);
         pPal[254] := ConvertTColorToPlatFormColor(clWhite);
      end
   {$EndIf}
   else begin
      if (MapType = mtElevIHS) then for i := 0 to 255 do pPal[i] := RGBtripFromHSI(TheHue(MDDef.MergeHue,i),MDDef.MergeSat,MDDef.MergeInt);
      if (MapType = mtElevRainbow) then for i := 0 to 255 do pPal[i] := RainbowRGBFunct(i,1,254);
      if (MapType = mtElevSpectrum) then for i := 0 to 255 do pPal[i] := SpectrumRGBFunct(i,1,254);
      if MapType in [mtElevTerrain,mtElevLandSea] then for i := 0 to 255 do pPal[i] := TerrainRGBFunct(i,1,254);
      if MapType in [mtElevLandSea] then for i := 0 to 255 do seaPal[i] := OceanRGBFunct(i,1,254);
      pPal[255] := ConvertTColorToPlatformColor(clWhite);
      if (MapType = mtElevBands) then begin
         pPal[1] := ConvertTColorToPlatformColor(clLime);
         pPal[2] := ConvertTColorToPlatformColor(clGreen);
      end;
   end;
   {$IfDef RecordColorPalette}
      for i := 0 to 255 do WriteLineToDebugFile( IntegerToString(i,3) + '  ' + ColorStringFromPlatformColor(ppal[i]);
   {$EndIf}
end;


{$F+}
function ContColorFunct(z : integer) : TColor;
begin
   if (z=0) then ContColorFunct := ConvertPlatformColorToTColor(MDDef.ZeroColor)
   else begin
      if z = TopCont then ContColorFunct := ConvertPlatformColorToTColor(MDDef.TopContColor)
      else if z = BotCont then ContColorFunct := ConvertPlatformColorToTColor(MDDef.BotContColor)
      else if (z div MDdef.DefaultContourInterval) mod 5 = 0 then ContColorFunct := ConvertPlatformColorToTColor(MDDef.IndexColor)
      else ContColorFunct := ConvertPlatformColorToTColor(MDDef.ContourColor);
   end;
end {function};


procedure DefineAspectColors;
var
   i,Color : integer;
   ZColorTable : tColorTableDefinitions;
   Name : shortstring;
begin
   if (MDDef.AspectMapMode = 0) then begin
      for i := 0 to 360 do begin
         if i in [22.. 67] then AspColor[i] := AspColorNE
         else if i in [68..112] then AspColor[i] := AspColorE
         else if i in [113..157] then AspColor[i] := AspColorSE
         else if i in [158..202] then AspColor[i] := AspColorS
         else if i in [203..247] then AspColor[i] := AspColorSW
         else if (i >=248) and (i <= 292) then AspColor[i] := AspColorW
         else if (i >=293) and (i <= 337) then AspColor[i] := AspColorNW
         else AspColor[i] := AspColorN;
      end;
   end
   else if (MDDef.AspectMapMode = 1) then begin
      for i := 0 to 359 do AspColor[i] := ConvertTColorToPlatformColor(TerrainTColor(i,0,360));
   end
   else if (MDDef.AspectMapMode = 2) then begin
      for i := 0 to 359 do AspColor[i] := SpectrumRGBFunct(i,0,360);
   end
   else if (MDDef.AspectMapMode = 3) then begin
      for i := 0 to 359 do AspColor[i] := ConvertTColorToPlatformColor(RainbowColorFunct(i,0,360));
   end
   else begin
       case MDDef.AspectMapMode of
           4 : Name := 'CET-C1';
           5 : Name := 'CET-C2';
           6 : Name := 'CET-C6';
           7 : Name := 'CET-C7';
       end;
       if FileExists(ColorBrewerName) then begin
          if DefineColorArray(Name,ZColorTable.ZTableEntries,ZColorTable.zTableColors,false) then begin
            for i := 0 to 359 do begin
               Color := round(i/360 * ZColorTable.ZTableEntries);
               if Color < 1 then Color := 1;
               AspColor[i] := ZColorTable.zTableColors[Color];
            end;
            AspColor[360] := AspColor[0];
         end;
      end;
   end;
   AspColor[360] := AspColor[0];
end;


procedure TMapDraw.DefineSlopeColors;
var
   i,j,LowCut,HighCut  : integer;
begin
   DEMGlb[DEMonMap].ReflectanceParams;
   if (MapMerge = mmSlope) then begin
       for i := 0 to 255 do begin
          SlopeColorChoices[i] := RGBtripFromHSI((360.0 - (MDdef.MergeHue/255) * (((i*0.01 * MDdef.MaxMergeSlope) / MDdef.MaxMergeSlope) * 360.0)),MDdef.MergeSat,120);
       end;
   end
   else if isSlopeMap(MapType) then begin
       if (MapType = mtSlopeGrayScale) then begin
          for i := 0 to 255 do begin
             SlopeColorChoices[i] := GrayRGBtrip(i);
          end;
       end
       else if (MapType = mtSlopeGrayScaleReversed) then begin
          for i := 0 to 255 do begin
             SlopeColorChoices[i] := GrayRGBtrip(255-i);
          end;
       end
       else if MapType = mtSlopeRainbow then begin
          for i := 0 to 255 do SlopeColorChoices[i] := RainbowRGBFunct(i,0,254);
       end
       else if MapType = mtSlopePastel then begin
          for i := 0 to 255 do begin
             SlopeColorChoices[i] := RGBtripFromHSI((360.0 - (MDdef.MergeHue/255) * i/255 * 360.0),MDdef.MergeSat,120);
          end;
       end
       else if MapType = mtSlopeGoNoGo then begin
          for i := 0 to 29 do SlopeColorChoices[i] := RGBtrip(147,230,153);
          for i := 30 to 255 do SlopeColorChoices[i] := RGBtrip(252,113,106);
       end
       else if (MapType in [mtSlopeStandardCats]) then begin
          for i := 0 to pred(MDdef.NumSlopeBands) do begin
             if (i = 0) then LowCut := 0 else LowCut := round(SlopeCut[pred(i)]);
             if i = pred(MDdef.NumSlopeBands) then HighCut := 255 else HighCut := round(SlopeCut[i]);
             for j := LowCut to HighCut do SlopeColorChoices[j] := ConvertTColorToPlatformColor(SlopeColors[i]);
          end;
       end
       else begin
           for i := 1 to 29 do SlopeColorChoices[i] := RGBtrip(118,250,103);
           for i := 30 to 44 do SlopeColorChoices[i] := RGBtrip(222,235,63);
           for i := 45 to 255 do SlopeColorChoices[i] := RGBtrip(248,68,16);
       end {for i};
   end;
   DefineAspectColors;
  {$IfDef RecordMapDraw} WriteLineToDebugFile('slope colors defined'); {$EndIf}
end;


function TMapDraw.QuickElevColor(x,y : integer) : tPlatformColor;
var
   zj : float32;
begin
   if DEMGlb[DEMonMap].GetElevMeters(x,Y,zj) then begin
      Result := ElevColorFromZ(zj);
   end
   else begin
      Result := MDdef.MissingDataColor;
   end;
end;


function TMapDraw.GetElevColor(x,y : float64) : tPlatformColor;
var
   zj : float32;
   z : integer;
begin
  if (MapType in [mtFlowDir360,mtFlowDirArc,mtFlowDirTau,mtDEMVATTable,mtLASClass]) or (DEMGlb[DEMonMap].LandCoverGrid) then begin
     if DEMGlb[DEMonMap].GetElevMeters(round(x),round(y),zj) then begin
        z := round(zj);
        if (MapType in [mtLASclass]) then begin
            Result := Las_rgb_colors[z];
        end
        else if (DEMGlb[DEMonMap].LandCoverGrid) then begin
           Result := pPal[z];
        end
        else if (MapType in [mtDEMVATTable]) then begin
           Result := VATColors[z];
        end
        else if (MapType in [mtFlowDirArc,mtFlowDirTau]) then begin
           if (z in [1..128]) then Result := pPal[z];
        end
        else begin
           if (z >= 0) and (z <= 360) then Result := AspColor[z];
        end;
     end
     else Result := MDdef.MissingDataColor;
     exit;
   end
   else begin
      if DEMGlb[DEMonMap].GetElevMeters(x,Y,zj) then begin
         Result := MaskColor;
      end
      else begin
         Result := MDDef.MissingDataColor;
         exit;
      end;
   end;


   if (MaxMapElev - MinMapElev) < 0.001 then begin
      Result := RGBTrip(0,0,125);
   end
   else if DEMGlb[DEMonMap].ElevationDEM and (MapType in [mtElevRainbow,mtElevSpectrum]) and ( (abs(zj) < 0.0001) and MDdef.WaterCheck) or (MDdef.LakeCheck and DEMGlb[DEMonMap].LakePoint(round(x),round(Y)) ) then begin
      Result := MDdef.WaterColor;
   end
   else if (MapType = mtDEMMask) then begin
      Result := MaskColor;
   end
   else begin
      case ElevStretch of
         esPercentile : zj := DEMGlb[DEMonMap].PercentileOfElevation(zj);
         esSD : zj := (zj - DEMGlb[DEMonMap].Z_Mean) / DEMGlb[DEMonMap].Z_std;
      end;
      if Log10Elev then begin
         if zj <= 0 then begin
            Result := pPal[0];
            exit;
         end
         else zj := Math.Log10(zj);
      end;
      Result := ElevColorFromZ(zj);
   end;
end;


function TMapDraw.ElevColorFromZ(zj : float64) : tPlatformColor;
var
   i : integer;
begin
   if Math.IsNAN(zj) or (round(zj) = 65535) or (round(zj) = 32767) then Result := MDdef.MissingDataColor
   else if MDDef.ClipZColors and (zj < MinMapElev) then Result := MDdef.LowOffscaleColor
   else if MDDef.ClipZColors and (zj > MaxMapElev) then Result := MDdef.HighOffscaleColor
   else begin
      {$IfDef VCL}
      if MapType in [mtDEMVATTable] then Result := VATColors[round(zj)]
      else if MapType in [mtElevFromTable,mtElevDefinedPalette] then Result := ColorFromZColorTable(ZColorTable,zj,i)
      else if (MapType = mtElevContrast) then begin
         Result := ConvertTColorToPlatformColor(WinGraphColors[(round(zj) mod 16)]);
      end
      else
      {$EndIf}
      if (DEMGlb[DEMonMap] <> Nil) and (DEMGlb[DEMonMap].LandCoverGrid) then Result := pPal[Round(zj)]
      else if (MapType = mtElevBands) then begin
         if Odd(round(zj / MapOverlays.ConInt)) then Result := pPal[1]
         else Result := pPal[2];
      end
      else begin
         i := ClipValue(round(253.0 * (zj - MinMapElev) / ( MaxMapElev - MinMapElev)),1,254);
         if (MapType = mtElevLandSea) then begin
            if (zj > 0) or (abs(0 - MinMapElev) < 0.0001) then Result := pPal[i]
            else Result := seaPal[i];
         end
         else if MapType in [mtElevTerrain,mtElevRainbow,mtElevGray,mtElevGrayReversed,mtElevSpectrum,mtElevRainbow,mtElevIHS,mtIHSReflect] then begin
            Result := pPal[i];
         end;
      end;
   end;
end;




