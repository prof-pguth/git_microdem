{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{ include file for demmapdraw        }
{____________________________________}


procedure tMapDraw.DrawLatLine(Canvas : tCanvas; Lat : float64);
var
   LeftY,RightY : integer;
begin
   DrawLatLine(Canvas,Lat,LeftY,RightY);
end;

procedure tMapDraw.DrawLongLine(Canvas : tCanvas; Long : float64);
var
   TopX,BottomX : integer;
begin
   DrawLongLine(Canvas,Long,TopX,BottomX);
end;


procedure tMapDraw.DrawLatLine(Canvas : tCanvas; Lat : float64; var LeftY,RightY : integer);
var
 x,y : integer;
 First : boolean;
 dLong,Long : float64;
begin
   First := true;
   LeftY := -999;
   RightY := -999;
   Long := MapCorners.BoundBoxGeo.xmin;
   dLong := (MapCorners.BoundBoxGeo.xmax - MapCorners.BoundBoxGeo.xmin) / MapXSize;
   while Long <= MapCorners.BoundBoxGeo.xmax do begin
     LatLongDegreeToScreen(Lat,Long,x,y);
     if OnScreen(x,y) then begin
        if First then begin
           First := false;
           LeftY := y;
           Canvas.MoveTo(x,y);
        end
        else begin
          Canvas.LineTo(x,y);
          RightY := y;
        end;
     end
     else First := true;
     Long := Long + dLong;
   end;
end;

procedure tMapDraw.DrawLongLine(Canvas : tCanvas; Long : float64; var TopX,BottomX : integer);
var
 x,y : integer;
 First : boolean;
 dLat,Lat : float64;
begin
   TopX := -999;
   BottomX := -999;
   First := true;
   Lat := MapCorners.BoundBoxGeo.ymin;
   dLat := (MapCorners.BoundBoxGeo.ymax - MapCorners.BoundBoxGeo.ymin) / MapYSize;
   while Lat <= MapCorners.BoundBoxGeo.ymax do begin
      LatLongDegreeToScreen(Lat,Long,x,y);
      if OnScreen(x,y) then begin
        if First then begin
           BottomX := x;
           First := false;
           Canvas.MoveTo(x,y);
        end
        else begin
           Canvas.LineTo(x,y);
           TopX := x;
        end;
      end
      else First := true;
      Lat := Lat + dLat;
   end;
end;


procedure tMapDraw.SetUTMTickInt(var UTMTickInt : float64);
begin
   if (UTMTickInt < 1) then begin
     if ScreenPixelSize < 1 then UTMTickInt := 100
     else if ScreenPixelSize < 2 then UTMTickInt := 250
     else if ScreenPixelSize < 10 then UTMTickInt := 500
     else if ScreenPixelSize < 25 then UTMTickInt := 1000
     else if ScreenPixelSize < 50 then UTMTickInt := 2000
     else if ScreenPixelSize < 100 then UTMTickInt := 10000
     else if ScreenPixelSize < 250 then UTMTickInt := 20000
     else if ScreenPixelSize < 1000 then UTMTickInt := 50000
     else UTMTickInt := 100000;
   end;
end;


function UTMGridLabel(v : float64) : ShortString;
var
  TStr : ShortString;
begin
  if (abs(frac(0.001 * v + 0.0005)) < 0.001) then begin
     Str(round(0.001 * v):12,TStr);
     Result := Copy(TStr,11,2);
  end
  else Result := '';
end {proc};



procedure TMapDraw.DrawGridLines(var inBitmap : tMyBitmap);
var
   GridOverlay : tMyBitmap;
   ShapeFileCreator : tShapeFileCreation;

      {$IfDef VCL}
         procedure DrawNativeGrid(VP : tMapProjection);
         var
            Lat,Long, xlo,ylo,xhigh,yhigh : float64;
            x,y,i : integer;
            TStr : shortString;
         begin
            SetUTMTickInt(NativeTickInt);
            if (NativeTickInt > 1) then begin
               GridOverlay.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.NativeGridColor);
               GridOverlay.Canvas.Pen.Width := MDDef.NativeGridLineWidth;
               xlo := round(MapCorners.BoundBoxProj.xmin / NativeTickInt) * NativeTickInt;
               xhigh := round(MapCorners.BoundBoxProj.xmax / NativeTickInt) * NativeTickInt;
               ylo := round(MapCorners.BoundBoxProj.ymin / NativeTickInt) * NativeTickInt;
               yhigh :=  round(MapCorners.BoundBoxProj.YMax / NativeTickInt) * NativeTickInt;
               repeat
                  VP.InverseProjectDegrees(xlo,ylo,Lat,Long);
                  LatLongDegreeToScreen(Lat,Long,x,y);
                  DrawLine(GridOverlay,x,0,x,MapYSize);
                  if MDDef.LabelNativeGrid then begin
                     TStr := RealToString(xlo,-12,0);
                     if (TStr <> '') then begin
                        i := MapYSize - GridOverlay.Canvas.TextHeight(TStr);
                        if (x > 0) and (x < MapXSize - GridOverlay.Canvas.TextWidth(TStr)) then GridOverlay.Canvas.TextOut(x-GridOverlay.Canvas.TextWidth(TStr) div 2,i,TStr);
                     end;
                  end;
                  xlo := xlo + NativeTickInt;
               until (xlo > xhigh);

               repeat
                  VP.InverseProjectDegrees(xlo,ylo,Lat,Long);
                  LatLongDegreeToScreen(Lat,Long,x,y);
                  DrawLine(GridOverlay,0,y,MapXSize,y);
                  if MDDef.LabelNativeGrid then begin
                     TStr := RealToString(ylo,-12,0);
                     if (TStr <> '') then begin
                        i := 1;
                        if (y > GridOverlay.Canvas.TextWidth(TStr)) and (y < MapYSize - GridOverlay.Canvas.TextWidth(TStr)) then TextOutVertical(GridOverlay.Canvas,i,y+GridOverlay.Canvas.TextWidth(TStr) div 2,TStr,True);
                     end;
                  end;
                  ylo := ylo + NativeTickInt;
               until (ylo > yhigh);
            end;
        end;
     {$EndIf}


      procedure DrawGraticule(FromDatum : tMapProjection; Color : tPlatformColor; Width : byte);
      var
         LongSize,LabelInt,Lat2,Long2,Lat3,Long3 : float64;
         LeftY,RightY,TopX,BottomX,
         xp,yp      : integer;
         TickStyle           : tLatLongMethod;
         TStr : shortstring;
         table2 : tMyData;
         LabelsHere,Convert : boolean;
         OutMethod :tLatLongMethod;
         GridLimits : sfBoundBox;


                procedure AddPoint(Lat,Long : float64; LabelLat : boolean);
                var
                   Name : shortString;
                   Method : tLatLongMethod;
                begin
                   Table2.Insert;
                   Table2.SetFieldByNameAsFloat('LAT',Lat);
                   Table2.SetFieldByNameAsFloat('LONG',Long);
                   GetOutPutMethodFromTickSize(LatTickInt,Method);
                   if LabelLat then Name := LatToString(Lat,Method)
                   else Name := LongToString(Long,Method);
                   Table2.SetFieldByNameAsString('NAME',Name);
                   Table2.SetFieldByNameAsString('ICON','pt_sym' + OverlayFExt);
                   Table2.Post;
                end;


      begin
         {$IfDef RecordDrawGridLines} FromDatum.ShortProjInfo('DrawGraticule in'); {$EndIf}

         {$IfDef VCL}
            GridOverlay.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
            GridOverlay.Canvas.Pen.Width := Width;
            {$IfDef RecordDrawGridLines} writeLineToDebugFile('Lat long Grid lines ' + ColorString(GridOverlay.Canvas.Pen.Color)); {$EndIf}
         {$EndIf}

         if (VectorIndex <> 0) and (PrimMapProj.Pname in [Gnomonic,HammerProj,LamAzEqArea,Mollweide,OrthoProj,SinusProj,SinusEllipsoidal,OldStereographic,VanDerGrinten,SphericalStereographic,UndefinedProj,EqualEarth]) then begin
            GridLimits := PrimMapProj.ProjectionGeoBox;
            MapCorners.BoundBoxGeo := PrimMapProj.ProjectionGeoBox;
         end
         else begin
            GridLimits := MapCorners.BoundBoxGeo;
         end;

         LongSize := MapCorners.BoundBoxGeo.xmax - MapCorners.BoundBoxGeo.xmin;
         if (LongSize < 0.001) then exit;
         LabelsHere := (VectorIndex = 0) or (not (PrimMapProj.Pname in [Gnomonic,HammerProj,LamAzEqArea,Mollweide,OrthoProj,SinusProj,SinusEllipsoidal,OldStereographic,VanDerGrinten,SphericalStereographic,UndefinedProj,EqualEarth]));

         if (LatTickInt < 0.000001) then SetLatLongTickInterval(LongSize,LatTickInt,LabelInt,TickStyle)
         else SetLatLongTickLabel(LatTickInt,LabelInt,TickStyle);
         GetOutPutMethodFromTickSize(LatTickInt,OutMethod);

         if (GridLimits.ymin < 0) then GridLimits.ymin := LatTickInt * pred(trunc(GridLimits.ymin / LatTickInt))
         else GridLimits.ymin := LatTickInt * trunc(GridLimits.ymin / LatTickInt);
         if (GridLimits.xmin < 0) then GridLimits.xmin := LatTickInt * pred(trunc(GridLimits.xmin / LatTickInt))
         else GridLimits.xmin := LatTickInt * trunc(GridLimits.xmin / LatTickInt);

         Convert := not (WGSEquivalentDatum(FromDatum.h_DatumCode) );

         {$IfDef RecordDrawGridLines} WriteLineToDebugFile('DrawGraticule long limits, ' + RealToString(GridLimits.xmin,-11,-4) + ' to ' + RealToString(GridLimits.xmax,-11,-4)); {$EndIf}
         if MDDef.LatLongGridTicks then begin
            Lat2 := GridLimits.ymin;
            while Lat2 <= GridLimits.ymax do begin
               Long2 := GridLimits.xmin;
               while Long2 <= GridLimits.xmax do begin
                   if Convert then begin
                      MolodenskiyTransformation(Lat2,Long2,Lat3,Long3,FromDatum,WGS84DatumConstants);
                      LatLongDegreeToScreen(Lat3,Long3,xp,yp);
                   end
                   else LatLongDegreeToScreen(Lat2,Long2,xp,yp);
                   BitmapCross(GridOverlay,xp,yp,5);
                   Long2 := Long2 + LatTickInt;
               end;
               Lat2 := Lat2 + LatTickInt;
            end;
         end
         else begin
            {$IfDef GridGraticuleShapeFile}
               if MDdef.KMLLabelGraticule then begin
                   fName2 := MainMapData + 'Icons\' +  'pt_sym' + OverlayFExt;
                   if not FileExists(fName2) then begin
                      CreateBitmap(bmp,11,11);
                      {$IfDef FMX} Bmp.Canvas.BeginScene; {$EndIf}
                      PetImage.BitmapCross(bmp,5,5,5);
                      {$IfDef FMX} Bmp.Canvas.EndScene; {$EndIf}
                      Petimage.SaveBitmap(bmp,fName2);
                      bmp.Free;
                   end;
                   GraticuleEndsFName := Petmar.NextFileNumber(MDTempDir, 'graticule_ends_',DefaultDBExt);
                   DefineAndCreateANewTable(GraticuleEndsFName,false,true,false,true,true,true);
                   Table2 := tMyData.Create(GraticuleEndsFName);
               end;

               GraticuleFName := Petmar.NextFileNumber(MDTempDir, 'graticule_',DefaultDBExt);
               Make_Tables.MakeBasicBoundingBoxTable(GraticuleFName);
               Table := tMyData.Create(GraticuleFName);
               ShapeFileCreator := tShapeFileCreation.Create(GraticuleFName,3);
               {$IfDef RecordDrawGridLines} WriteLineToDebugFile('shapefile created=' + GraticuleFName); {$EndIf}
            {$EndIf}

            GridOverlay.Canvas.Brush.Style := bsSolid;
            GridOverlay.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDDef.InsideMapGridColor);
            LoadMyFontIntoWindowsFont(MDDef.InsideGridFont,GridOverlay.Canvas.Font);

            {$IfDef RecordDrawGridLines} writeLineToDebugFile('DrawGraticule lat limits, ' + RealToString(GridLimits.ymin,-11,-4) + ' to ' + RealToString(GridLimits.ymax,-11,-4)); {$EndIf}
            Lat2 := GridLimits.ymin;
            while (Lat2 <= GridLimits.ymax + LatTickInt) do begin
                if (Lat2 < 89.000001) and (Lat2 > -89.00001) then begin
                   DrawLatLine(GridOverlay.Canvas,Lat2,LeftY,RightY);
                   if MDDef.GridLabelsInsideMap and LabelsHere then begin
                      TStr := LatToString(Lat2,OutMethod);
                      if LeftY > -99 then begin
                        GridOverlay.Canvas.TextOut(5,LeftY-GridOverlay.Canvas.TextHeight(TStr) div 2,TStr);  //left side
                      end;
                      if RightY > -99 then begin
                          GridOverlay.Canvas.TextOut(MapXsize - 5 - GridOverlay.Canvas.TextWidth(TStr),RightY-GridOverlay.Canvas.TextHeight(TStr) div 2,TStr);  //right side
                      end;
                   end;

                   {$IfDef GridGraticuleShapeFile}
                      UseLat := Lat2;
                      Table.Insert;
                      Table.SetFieldByNameAsString('NAME',RealToString(UseLat,-12,-5));
                      ShapeFileCreator.AddLineLatLongToShapeFile(Table,UseLat,LowLong - LatTickInt,UseLat,GridLimits.xmax + LatTickInt,0.1 * LatTickInt,Convert,FromDatum);
                      Table.Post;

                      if MDdef.KMLLabelGraticule then begin
                         AddPoint(UseLat,LowLong,true);
                         AddPoint(UseLat,GridLimits.xmax,true);
                      end;
                   {$EndIf}
                end;
                Lat2 := Lat2 + LatTickInt;
            end;
            {$IfDef RecordDrawGridLines} WriteLineToDebugFile('lat part done'); {$EndIf}
            Long2 := GridLimits.xmin - LatTickInt;
            repeat
                if (Long2 <= GridLimits.xmax) and (Long2 >= GridLimits.xmin)  then begin
                   DrawLongLine(GridOverlay.Canvas,Long2,TopX,BottomX);

                   if MDDef.GridLabelsInsideMap and LabelsHere then begin
                      TStr := LongToString(Long2,OutMethod);
                      if TopX > -299 then begin
                         GridOverlay.Canvas.TextOut(TopX-GridOverlay.Canvas.TextWidth(TStr) div 2,5,TStr);
                      end;
                      if BottomX > -299 then begin
                         GridOverlay.Canvas.TextOut(BottomX-GridOverlay.Canvas.TextWidth(TStr) div 2,MapYSize - 5 - GridOverlay.Canvas.TextHeight(TStr),TStr);
                      end;
                   end;

                   {$IfDef GridGraticuleShapeFile}
                      Petmath.LongitudeAngleInRange(UseLong);
                      Table.Insert;
                      Table.SetFieldByNameAsString('NAME',RealToString(UseLong,-12,-5));
                      ShapeFileCreator.AddLineLatLongToShapeFile(Table,LowLat - LatTickInt,UseLong,HighLat + LatTickInt,UseLong,0.1 * LatTickInt,Convert,FromDatum);
                      Table.Post;
                      if MDdef.KMLLabelGraticule then begin
                         AddPoint(LowLat,UseLong,false);
                         AddPoint(HighLat,UseLong,false);
                      end;
                   {$EndIf}
                end;
                Long2 := Long2 + LatTickInt;
            until (Long2 > GridLimits.xmax);
            {$IfDef RecordDrawGridLines} WriteLineToDebugFile('all lines drawn'); {$EndIf}
            {$IfDef GridGraticuleShapeFile}
               ShapeFileCreator.CloseShapeFiles(Table);
               {$IfDef RecordDrawGridLines} WriteLineToDebugFile('shapefile closed'); {$EndIf}
               {$IfDef VCL}
                  GridOverlay.Canvas.Pen.Width := Width;
               {$EndIf}
               QuickShapeFileDisplay(GridOverlay,GraticuleFName,false,ConvertPlatformColorToTColor(Color));
               if MDdef.KMLLabelGraticule then Table2.Destroy;
            {$EndIf}
         end;
      end;


     procedure UTMTicksAsNeeded;


            procedure DrawUTMGrid(var  FromDatum : tMapProjection; Color : tPlatformColor; Width : byte);
            var
               x,y : float64;
               KML_Table : tMyData;
               Convert : boolean;
               TStr : shortstring;
               xp,yp,xp2,yp2 : integer;
               UTMlimits : sfBoundBox;

                      procedure AddPoint(xutm,yutm : float64; Labelxutm : boolean);
                      var
                         Name : shortString;
                         Lat,Long : float64;
                      begin
                         FromDatum.UTMtoLatLongDegree(xutm,yutm,Lat,Long);
                         if Convert then MolodenskiyTransformation(Lat,Long,Lat,Long,FromDatum,WGS84DatumConstants);
                         KML_Table.Insert;
                         KML_Table.SetFieldByNameAsFloat('LAT',Lat);
                         KML_Table.SetFieldByNameAsFloat('LONG',Long);
                         if Labelxutm then Name := RealToString(xutm,-12,0)
                         else Name := RealToString(yutm,-12,0);
                         if MDDef.ShortKML_UTM then Name := Copy(Name,Length(Name)-4,2);
                         KML_Table.SetFieldByNameAsString('NAME',Name);
                         KML_Table.SetFieldByNameAsString('ICON','pt_sym' + OverlayFExt);
                         KML_Table.Post;
                      end;


                     procedure LabelUTMinside;
                     begin
                        GridOverlay.Canvas.Brush.Style := bsSolid;
                        GridOverlay.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDDef.InsideMapGridColor);
                        LoadMyFontIntoWindowsFont(MDDef.InsideGridFont,GridOverlay.Canvas.Font);

                        y := UTMLimits.ymin {- UTMTickInt};
                        while (y < UTMLimits.ymax) do begin
                            {$IfDef RecordFullDrawGridLines} WriteLineToDebugFile(IntToStr(xp) + '/' + IntToStr(yp) + '    ' + IntToStr(xp2) + '/' + IntToStr(yp2)); {$EndIf}
                            UTMToScreen(UTMLimits.xmin,y,xp2,yp2);
                            UTMToScreen(UTMLimits.xmax,y,xp,yp);
                            TStr := UTMGridLabel(y);
                            if (TStr <> '') then begin
                               GridOverlay.Canvas.TextOut(5,yp2-GridOverlay.Canvas.TextHeight(TStr) div 2,TStr);   //labels left side of map
                               GridOverlay.Canvas.TextOut(MapXSize-GridOverlay.Canvas.TextWidth(TStr)-5,yp-GridOverlay.Canvas.TextHeight(TStr) div 2,TStr);   //labels right side of map
                            end;
                            y := y + UTMTickInt;
                        end {while};

                        //easting lines
                        x := UTMLimits.xmin;
                        while (x < UTMLimits.xmax) do begin
                            UTMToScreen(x,UTMLimits.ymax,xp,yp);
                            UTMToScreen(x,UTMLimits.ymin,xp2,yp2);
                            TStr := UTMGridLabel(x);
                            if (TStr <> '') then begin
                               GridOverlay.Canvas.TextOut(xp-GridOverlay.Canvas.TextWidth(TStr) div 2,5,TStr);    //labels top of map
                               GridOverlay.Canvas.TextOut(xp2-GridOverlay.Canvas.TextWidth(TStr) div 2,MapYSize - 5 - GridOverlay.Canvas.TextHeight(TStr),TStr);   //labels bottom of map
                            end;
                            x := x + UTMTickInt;
                        end {while};
                     end;


            begin
               {$If Defined(track_f) or Defined(RecordDrawGridLines)} FromDatum.ShortProjInfo('UTMTicksAsNeeded in'); {$EndIf}
               if AFullWorldMap or ((MapCorners.BoundBoxGeo.xmax - MapCorners.BoundBoxGeo.xmin) > 10) then exit;
               Convert := not (WGSEquivalentDatum(FromDatum.h_DatumCode) );
               SetUTMTickInt(UTMTickInt);
               if (UTMTickInt > 75000) then exit;
               if (FromDatum.PName <> UTMellipsoidal) then FromDatum.false_east := 500000.00;

               if MDdef.KMLLabelGraticule then begin
                  UTMgridEndsFName := Petmar.NextFileNumber(MDTempDir, 'utm_grid_ends_',DefaultDBExt);
                  DefineAndCreateANewTable(UTMgridEndsFName,false,true,false,true,true,true);
                  KML_Table := tMyData.Create(UTMgridEndsFName);
               end;

               UTMgridFName := Petmar.NextFileNumber(MDTempDir, 'utm_grid_',DefaultDBExt);
               ShapeFileCreator := tShapeFileCreation.Create(FromDatum,UTMgridFName,true,3);

               {$IfDef RecordDrawGridLines}
                  WriteLineToDebugFile('');
                  WriteLineToDebugFile('geo bbox: ' + sfBoundBoxToString(MapCorners.BoundBoxGeo,6));
                  WriteLineToDebugFile('utm bbox=' + sfBoundBoxToString(MapCorners.BoundBoxUTM,1));
                  WriteLineToDebugFile('proj bbox=' + sfBoundBoxToString(MapCorners.BoundBoxProj,4));
                  WriteLineToDebugFile('Tick interval=' + RealToString(UTMTickInt,-12,0));
               {$EndIf}

               UTMLimits := MapCorners.BoundBoxUTM;

               UTMLimits.yMin := UTMTickInt * pred(trunc(UTMLimits.yMin / UTMTickInt));
               UTMLimits.yMax := UTMTickInt * succ(succ(trunc(UTMLimits.yMax / UTMTickInt)));
               UTMLimits.xMin := UTMTickInt * pred(trunc(UTMLimits.xMin / UTMTickInt));
               UTMLimits.xMax := UTMTickInt * succ(succ(trunc(UTMLimits.xMax / UTMTickInt)));

             //northing lines run east west
               y := UTMLimits.yMin;
               while (y < UTMLimits.yMax) do begin
                  {$IfDef RecordFullDrawGridLines} WriteLineToDebugFile('y=' + RealToString(y,-12,0)); {$EndIf}
                  ShapeFileCreator.RecordName := RealToString(y,-12,0);
                  ShapeFileCreator.AddLineUTMToShapeFile(UTMLimits.xMin - UTMTickInt,y,UTMLimits.xMax + UTMTickInt,y,0.1 * UTMTickInt);
                  if MDdef.KMLLabelGraticule then begin
                     AddPoint(UTMLimits.xMin,y,false);
                     AddPoint(UTMLimits.xMax,y,false);
                  end;
                   y := y + UTMTickInt;
               end {while};

             //easting lines run north south
               x := UTMLimits.xMin;
               while (x < UTMLimits.xMax) do begin
                  {$IfDef RecordFullDrawGridLines} WriteLineToDebugFile('x=' + RealToString(x,-12,0)); {$EndIf}
                  ShapeFileCreator.RecordName := RealToString(x,-12,0);
                  ShapeFileCreator.AddLineUTMToShapeFile(x,UTMLimits.yMin - UTMTickInt,x,UTMLimits.yMax + UTMTickInt,0.1 * UTMTickInt);
                  if MDdef.KMLLabelGraticule then begin
                     AddPoint(x,UTMLimits.yMin,true);
                     AddPoint(x,UTMLimits.yMax,true);
                  end;
                  x := x + UTMTickInt;
               end {while};
               {$IfDef RecordDrawGridLines} WriteLineToDebugFile('UTM grid extent: ' + sfBoundBoxToString(ShapeFileCreator.glMainFileHeader.BoundBox,4)); {$EndIf}
               ShapeFileCreator.CloseShapeFiles;
               if MDdef.KMLLabelGraticule then KML_Table.Destroy;
               {$IfDef VCL}
                  GridOverlay.Canvas.Pen.Width := Width;
               {$EndIf}
               QuickShapeFileDisplay(GridOverlay,UTMgridFName,false,ConvertPlatformColorToTColor(Color));
               if MDDef.GridLabelsInsideMap then LabelUTMinside;
            end;


      begin
        {$IfDef RecordDrawGridLines} WriteLineToDebugFile('UTMTicksAsNeeded enter'); {$EndIf}
        if (ScreenPixelSize < MDDef.UTMGridMaxPixelSize) then begin
           if MDdef.ShowSecondaryGrid and (MDDef.PreferSecondaryDatum <> MDdef.PreferPrimaryDatum) then begin
              {$IfDef RecordDrawGridLines} SecMapProjection.ShortProjInfo('MDdef.ShowSecondaryGrid UTM'); {$EndIf}
              DrawUTMGrid(SecMapProj,MDdef.SecondaryUTMColor,MDDef.UTMSecGridLineWidth);
           end;
           if MDdef.ShowPrimaryGrid then begin
              {$IfDef RecordDrawGridLines} PrimMapProj.ShortProjInfo('MDdef.ShowPrimaryGrid UTM'); {$EndIf}
              RedefineWGS84DatumConstants(MapCorners.BoundBoxGeo.xmin, PrimMapProj.LatHemi);
              DrawUTMGrid(WGS84DatumConstants,MDdef.MapGridColor,MDdef.UTMGridLineWidth);
           end;
           {$IfDef VCL}
           GridOverlay.Canvas.Pen.Width := 1;
           {$EndIf}
        end
        else begin
           {$IfDef RecordDrawGridLines} WriteLineToDebugFile('UTMTicksAsNeeded skipped, ScreenPixelSize=' + RealToString(ScreenPixelSize,-12,-2) + ' only draw at=' + RealToString(MDDef.UTMGridMaxPixelSize,-12,-2)); {$EndIf}
        end;
        {$IfDef RecordDrawGridLines} WriteLineToDebugFile('UTMTicksAsNeeded out'); {$EndIf}
      end;


begin {DrawGridLines}
   if ( (MDDef.MapTicks = tixNone) and (not MDDef.ShowNativeGrid) ) or (inBitmap = Nil) or (DEMMap and ValidDEMonMap and (DEMGlb[DEMonMap].DEMheader.DigitizeDatum = Rectangular)) then begin
      exit;
   end;

   {$If Defined(RecordDrawGridLines) or Defined(track_f)} PrimMapProj.ShortProjInfo('DrawGridLines in' + MapSizeString); {$EndIf}

   GridOverlay := Nil;
   CreateBitmap(GridOverlay,inBitmap.Width,inBitmap.Height);
   if (GridOverlayFName <> '') and FileExists(GridOverlayFName) then begin
      {$IfDef RecordDrawGridLines} WriteLineToDebugFile('load ' + GridOverlayFName); {$EndIf}
      GridOverlay.LoadFromFile(GridOverlayFName);
   end
   else begin
      {$IfDef RecordDrawGridLines} writeLineToDebugFile('DrawGridLines new grid draw'); {$EndIf}

      {$IfDef VCL}
         if MDDef.ShowNativeGrid and NativeGridAllowedOnMap then begin
            DrawNativeGrid(PrimMapProj);
         end;
      {$EndIf}

      if (MDDef.MapTicks in [tixLatLong,tixBoth]) then begin
         if MDdef.ShowSecondaryGrid and (MDDef.PreferSecondaryDatum <> MDdef.PreferPrimaryDatum) and (ScreenPixelSize < 500) then begin
            {$IfDef RecordDrawGridLines} writeLineToDebugFile('Draw secondary graticule');    {$EndIf}
            DrawGraticule(Self.SecMapProj,MDdef.SecondaryGeoColor,MDdef.GeoSecGridLineWidth);
            {$IfDef SaveGridSequentially} GridOverlay.SaveToFile(MDTempDir + 'gridOverlay_part_1_secondary_graticule.bmp'); {$EndIf}
         end;
         if MDDef.ShowPrimaryGrid then begin
            DrawGraticule(Self.PrimMapProj,MDdef.MapLatLongGridColor,MDdef.MapLatLongLineWidth);
            {$IfDef SaveGridSequentially} GridOverlay.SaveToFile(MDTempDir + 'gridOverlay_part_2_primary_graticule.bmp'); {$EndIf}
         end;
      end;

      if (MDDef.MapTicks in [tixUTM,tixBoth]) then begin
         UTMTicksAsNeeded;
         {$IfDef SaveGridSequentially} GridOverlay.SaveToFile(MDTempDir + 'gridOverlay_part_3_both_utm.bmp'); {$EndIf}
      end;
      GridOverlayFName := NextFileNumber(MDTempDir, 'Grid_overlay', OverlayFExt);
      GridOverlay.SaveToFile(GridOverlayFName);
   end;
   DrawOverlayNoDelete(inBitmap,GridOverlay,100,0,0);
   GridOverlay.Free;
end {DrawGridLines};


         procedure tMapDraw.ShowDEMGrid(Bitmap : tMyBitmap);
         var
            z : float32;
            Col,Row,xp,yp : integer;
            TStr : ShortString;
         begin
            if ValidDEMonMap then begin
               Bitmap.Canvas.Font.Name := 'Verdana';
               Bitmap.Canvas.Font.Style := [fsBold];
               //if ShowSatProgress then StartProgressAbortOption('Grid');
               for Col := round(MapCorners.BoundBoxDataGrid.xmin) to round(MapCorners.BoundBoxDataGrid.xmax) do begin
                  //if (Col mod 50 = 0) and ShowSatProgress then UpdateProgressBar(Col/DEMGlb[DEMonMap].DEMheader.NumCol);
                  for Row := round(MapCorners.BoundBoxDataGrid.ymin) to round(MapCorners.BoundBoxDataGrid.ymax) do begin
                     if DEMGlb[DEMonMap].GetElevMeters(Col,Row,z) then begin
                        DEMGridToScreen(Col,Row,xp,yp);
                        ScreenSymbol(Bitmap.Canvas,xp,yp,DEMGridSym,DEMGridSymSize,DEMGridSymColor);
                        Bitmap.Canvas.Brush.Style := bsClear;
                        if DEMGridLabel then begin
                           TStr := RealToString(z,-18,MDDef.GridLabelDecimals);
                           if Length(TStr) > 4 then Bitmap.Canvas.Font.Size := 8
                           else Bitmap.Canvas.Font.Size := 10;
                           xp := xp - Bitmap.Canvas.TextWidth(TStr) div 2;
                           Bitmap.Canvas.TextOut(xp+3,yp+3,TStr);
                        end;
                     end;
                  end;
                  //if WantOut then break;
               end;
            end;
            //EndProgress;
         end;
		 

         procedure TMapDraw.UTMBox(Bitmap : tMyBitmap; LowLong,HighLong : float64; x1,y1,x2,y2,inc : integer; var xc,yc,xr : integer; UseDatum : tMapProjection);
         var
            PolyLinePoints : array[0..500] of tPoint;
            xmin,xmax : integer;
            Lat,Long,dx,dy : float64;
            i : integer;

            procedure ProcessBoxPoint(xutm,yutm : float64; var PolyLinePoint : tPoint);
            begin
               UseDatum.UTMtoLatLongDegree(xutm,yutm,Lat,Long);
               if (UseDatum.h_DatumCode  <> PrimMapProj.h_DatumCode) then MolodenskiyTransformation(Lat,Long,Lat,Long,UseDatum,PrimMapProj);
               if (Long < LowLong) then Long := LowLong
               else if (Long > HighLong) then Long := HighLong;
               LatLongDegreeToScreen(Lat,Long,PolyLinePoint.x,PolyLinePoint.y);
            end;

         begin
           {$IfDef RecordDrawGridLines}
              WriteLineToDebugFile('x1=' + IntToStr(x1) +  '  y1=' + IntToStr(y1) + '  x2=' + IntToStr(x2) + '  y2=' + IntToStr(y2));
              UseDatum.UTMtoLatLongDegree(x1,y1,Lat,Long); WriteLineToDebugFile('   ' + LatLongDegreeToString(lat,Long));
              UseDatum.UTMtoLatLongDegree(x2,y2,Lat,Long); WriteLineToDebugFile('   ' + LatLongDegreeToString(lat,Long));
           {$EndIf}
            dx := (x2 - x1) / inc;
            dy := (y2 - y1) / inc;
            for i := 0 to inc do begin
               ProcessBoxPoint(x1,y1 + i * dy,PolyLinePoints[i]);
               if (i > 0) then begin
                  ProcessBoxPoint(x1+i*dx,y2,PolyLinePoints[i+inc]);
                  ProcessBoxPoint(x2,y2 - i * dy,PolyLinePoints[i+2*inc]);
                  ProcessBoxPoint(x2-i*dx,y1,PolyLinePoints[i+3*inc]);
               end;
            end;
            Bitmap.Canvas.PolyLine(slice(PolyLinePoints,succ(4*inc)));

            xc := 0;
            yc := 0;
            xmin := 9999;
            xmax := -9999;
            for i := 0 to 4*inc do begin
               xc := xc + PolyLinePoints[i].x;
               yc := yc + PolyLinePoints[i].y;
               if (PolyLinePoints[i].x > xmax) then xmax := PolyLinePoints[i].x;
               if (PolyLinePoints[i].x < xmin) then xmin := PolyLinePoints[i].x;
            end;
            xc := xc div succ(4*inc);
            yc := yc div succ(4*inc);
            xr := xmax - xmin;
         end;


         procedure TMapDraw.LatLongBox(Bitmap : tMyBitmap; Lat1,Long1,Lat2,Long2 : float64; inc : integer; var xc,yc : integer; Fill : boolean = false);
                  {SW corner first, then NE corner}
         var
            PolyLinePoints : array[0..100] of tPoint;
            dx,dy : float64;
            i,x1,y1,x2,y2 : integer;
         begin
            {$IfDef RecordDrawGridLines} WriteLineToDebugFile('TMapDraw.LatLongBox, sw=' + LatLongDegreeToString(Lat1,Long1) + '  ne=' + LatLongDegreeToString(Lat2,Long2)); {$EndIf}
            LatLongDegreeToScreen(Lat1,Long1,x1,y1);
            LatLongDegreeToScreen(Lat2,Long2,x2,y2);
            if Fill then begin
               Bitmap.Canvas.Brush.Style := bsDiagCross;
               Bitmap.Canvas.Brush.Color := Bitmap.Canvas.Pen.Color;
            end
            else Bitmap.Canvas.Brush.Style := bsClear;
            if (x1 > x2) then begin
               Bitmap.Canvas.Rectangle(x1,y1,x2,y2);
            end
            else begin
               dx := (Long2-Long1) / inc;
               dy := (Lat2-Lat1) / inc;
               for i := 0 to inc do begin
                  LatLongDegreeToScreen(Lat1 + i * dy,Long1,PolyLinePoints[i].x,PolyLinePoints[i].y);
                  if (i > 0) then begin
                     LatLongDegreeToScreen(Lat2,Long1+i*dx,PolyLinePoints[i+inc].x,PolyLinePoints[i+inc].y);
                     LatLongDegreeToScreen(Lat2-i*dy,Long2,PolyLinePoints[i+2*inc].x,PolyLinePoints[i+2*inc].y);
                     LatLongDegreeToScreen(Lat1,Long2-i*dx,PolyLinePoints[i+3*inc].x,PolyLinePoints[i+3*inc].y);
                  end;
               end;
               if Fill then Bitmap.Canvas.Polygon(slice(PolyLinePoints,succ(4*inc)))
               else Bitmap.Canvas.PolyLine(slice(PolyLinePoints,succ(4*inc)));
               xc := 0;
               yc := 0;
               for i := 0 to 4*inc do begin
                  xc := xc + PolyLinePoints[i].x;
                  yc := yc + PolyLinePoints[i].y;
               end;
               xc := xc div succ(4*inc);
               yc := yc div succ(4*inc);
            end;
         end;


{$IfDef ExPLSS}
{$Else}

      procedure TMapDraw.DrawPLSSGrid(inBitmap : tMyBitmap);
      var
         i : integer;
         Bitmap : tMyBitmap;
      begin
         {$If Defined(RecordPLSS) or Defined(TimePLSS)} WriteLineToDebugFile('TMapForm.DrawPLSS enter'); {$EndIf}
         CreateBitmap(Bitmap,inBitmap.Width,inBitmap.Height);
         if (PLSSOverlayfName <> '') then begin
            Bitmap.LoadFromFile(PLSSOverlayfName);
         end
         else begin
            ShowHourglassCursor;
            WMDEM.StatusBar1.Panels[0].Text := 'Draw PLSS grid';
            for I := 1 to MaxPLSS do begin
               if (PLSS[i] <> Nil) then begin
                  {$If Defined(RecordPLSS) or Defined(TimePLSS)} writeLineToDebugFile('PLSS=' + IntToStr(i)); {$EndIf}
                  PLSS[i].DrawPLSS(Self,Bitmap);
               end;
            end;
            ShowDefaultCursor;
            WMDEM.StatusBar1.Panels[0].Text := '';
            PLSSOverlayfName := NextFileNumber(MDTempDir, 'PLSS_overlay', OverlayFExt);
            Bitmap.SaveToFile(PLSSOverlayfName);
         end;
         inBitmap.Canvas.CopyMode := cmSrcAnd;
         inBitmap.Canvas.Draw(0,0,Bitmap);
         Bitmap.Free;
         EndProgress;
         {$If Defined(RecordPLSS) or Defined(TimePLSS)} WriteLineToDebugFile('TMapForm.DrawPLSS exit'); {$EndIf}
      end;

{$EndIf}
