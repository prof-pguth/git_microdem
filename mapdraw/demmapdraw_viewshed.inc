
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{     include file for demmapdraw        }
{________________________________________}


      procedure TMapDraw.FanBitmap(Lat,Long : float64; var Bitmap : tMyBitmap);
      var
         wf : tWeaponsFan;
         ShowName,CoverName : PathStr;
      begin
         InitializeWeaponsFan(wf);
         wf.W_Lat := Lat;
         wf.W_Long := Long;
         DrawRadialFan(wf,ShowName,CoverName);
         if FileExists(BaseMapFName) then begin
            CreateBitmap(Bitmap,0,0);
            Bitmap := LoadBitmapFromFile(BaseMapFName);
            DrawWorldFileImageOnMap(Bitmap,ShowName);
         end;
      end;


      procedure tMapDraw.ComputeMultiSensorCoverage(var NumFansCoveringFName : PathStr; WeaponsTable : tMyData; ViewshedSummary : tStringList = Nil);
      const
         MaxColorsPossible = 255;
      label
         FoundColor;
      var
         VisColors : array[0..MaxColorsPossible] of tColor;
         StretchRGB,
         VisRGB : array[0..MaxColorsPossible] of tRGBTriple;
         MaxColors,Color,Target,
         i,x,y,SensorNum,MaxColorUsed : integer;
         NumCover : array[0..MaxColorsPossible] of integer;
         Cover,SumCover : float64;
         line1,line2 : AnsiString;
         FanBitmap,VisBitmap, TargetMaskBMP,MultCover  : tMyBitmap;
         fName : PathStr;
         TStr,FanName   : ShortString;
         VisBMP,MultBMP,MaskBMP : tBMPMemory;
         VisPlatformColors : tZTableColors255;


         procedure StretchTheColors(MaxColorUsed : integer; TargetMaskBMP : tMyBitmap);
         var
            Color,y,x : integer;
            p1 :  PRGB;
         begin
             {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('StretchTheColors in'); {$EndIf}
             if (MaxColorUsed > MaxColorsPossible) then MaxColorUsed := MaxColorsPossible;

              for Color := 1 to MaxColorUsed do begin
                 StretchRGB[Color] := RainbowRGBFunct(Color,0,MaxColorUsed);
                 {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile(IntToStr(Color) + '  ' + ColorString(StretchRGB[Color])); {$EndIf}
              end;

              {$IfDef VCL}
              for y := 0 to pred(TargetMaskBMP.Height) do begin
                 p1 := TargetMaskBMP.Scanline[y];
                 for x := 0 to pred(TargetMaskBMP.Width) do begin
                    if (not SameColor(p1[x],RGBTripleWhite)) then begin
                       for Color := 1 to pred(MaxColors) do begin
                          if SameColor(p1[x],VisRGB[Color]) then begin
                             p1[x] := StretchRGB[Color];
                             break;
                          end;
                       end;
                    end;
                 end;
              end;
              {$EndIf}
           end;

      begin
        {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('FigureMultiSensorCoverage in'); {$EndIf}

        Target := PickOpenGISDataBase('Target area');

        if (MDDef.ViewshedColorScheme = LegChloropleth) then begin
           DefineColorArray(MDDef.ViewshedPaletteName,MaxColors,VisPlatformColors);
           for i := 1 to MaxColors do VisColors[i] := ConvertPlatformColorToTColor(VisPlatformColors[i]);
        end
        else begin
           MaxColors := 255;
           case MDDef.ViewshedColorScheme of
               LegSpectrum : for i := 1 to 255 do VisColors[i] := ConvertPlatformColorToTColor(SpectrumRGBFunct(i,0,255));
               LegRainbows : for i := 1 to 255 do VisColors[i] := RainbowColorFunct(i,0,255);
               LegGrays :    for i := 1 to 255 do VisColors[i] := RGB(i,i,i);
               LegTerrain :  for i := 1 to 255 do VisColors[i] := TerrainTColor(i,0,255);
           end;
        end;

        for i := 1 to MaxColors do VisRGB[i] := ConvertTColorToPlatformColor(VisColors[i]);

        Line1 := '';
        Line2 := '';

        CreateBitmap(FanBitmap,DEMGlb[1].SelectionMap.MapDraw.MapXSize,DEMGlb[1].SelectionMap.MapDraw.MapYSize);
        if FileExists(AllFansCoverageFName) then begin
           {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('Loading ' + AllFansCoverageFName); {$EndIf}
           FanBitmap.LoadFromFile(AllFansCoverageFName);
        end
        else begin
           {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('Creating ' + AllFansCoverageFName); {$EndIf}
           if (AllFansCoverageFName = '') then AllFansCoverageFName := MDTempDir + 'all_fans' + OverlayFExt;
           DrawAllFans(WeaponsTable,FanBitmap);
        end;

         CreateBitmap(MultCover,FanBitmap.Width,FanBitmap.Height);

         CreateBitmap(TargetMaskBMP,MapXSize,MapYSize);
         if (Target > 0) then begin
            if FileExists(DBOverlayfName[Target]) then TargetMaskBMP.LoadFromFile(DBOverlayfName[Target]);
         end
         else begin
            TargetMaskBMP.Canvas.Pen.Color := clBlack;
            TargetMaskBMP.Canvas.Brush.Color := clBlack;
            TargetMaskBMP.Canvas.Brush.Style := bsSolid;
            TargetMaskBMP.Canvas.Rectangle(0,0,pred(MapXSize),pred(MapYSize));
         end;

           SensorNum := 0;
           {$IfDef TimeFigureMultiSensorCoverage}
              WriteLineToDebugFile('Start coloring map');
              WriteLineToDebugFile('  Weapons Table=' + WeaponsTable.TableName + '  Weapons Filter=' + WeaponsTable.Filter);
              WriteLineToDebugFile('  Fans in table=' + IntToStr(WeaponsTable.FiltRecsInDB));
           {$EndIf}

           MultBMP := tBMPMemory.Create(MultCover);
           MaskBMP := tBMPMemory.Create(TargetMaskBMP);

           MaxColorUsed := 0;
           WeaponsTable.First;
           while not WeaponsTable.eof do begin
              inc(SensorNum);
              wmDEM.StatusBar1.Panels[0].Text := IntToStr(SensorNum) + '/' + IntToStr(WeaponsTable.FiltRecsInDB);
              ApplicationProcessMessages;
              FanName := WeaponsTable.GetFieldByNameAsString('IMAGE');
              if (FanName <> '') and FileExists(FanName) then  begin
                 {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('Load OK ' + FanName); {$EndIf}
                 StretchWorldFileMap(VisBitmap,FanName);
                 VisBMP := tBMPMemory.Create(VisBitmap);
                 for y := 0 to pred(VisBitmap.Height) do  begin
                    for x := 0 to pred(VisBitmap.Width) do begin
                       if not MaskBMP.SameColor(x,y,RGBTripleWhite) then begin  //point in target area
                          if (not VisBMP.SameColor(x,y,RGBTripleWhite)) then begin  //point visible on this fan
                             if MultBMP.SameColor(x,y,RGBTripleWhite) then MultBMP.SetPixelColor(x,y,VisRGB[1])
                             else begin
                                for Color := 1 to pred(MaxColors) do begin
                                   if MultBMP.SameColor(x,y,VisRGB[Color]) then begin
                                      MultBMP.SetPixelColor(x,y,VisRGB[succ(Color)]);
                                      if (Color > MaxColorUsed) then MaxColorUsed := Color;
                                      break;
                                   end;
                                end;
                             end;
                          end;
                       end;
                    end;
                 end;
                 VisBMP.Free;
                 VisBitmap.Free;
              end;
              WeaponsTable.Next;
           end;

           {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('Done color map',true); {$EndIf}

           for y := 0 to pred(TargetMaskBMP.Height) do begin
              for x := 0 to pred(TargetMaskBMP.Width) do  begin
                 if not (MaskBMP.SameColor(x,y,RGBTripleWhite)) then begin  //point in target area
                    if (MultBMP.SameColor(x,y,RGBTripleWhite)) then inc(NumCover[0])
                    else begin
                       for i := 1 to MaxColors do begin
                          if (MultBMP.SameColor(x,y,VisRGB[i])) then begin
                             inc(NumCover[i]);
                             goto FoundColor;
                          end;
                       end;
                       inc(NumCover[MaxColors]);
                       FoundColor:;
                    end;
                 end;
              end;
           end;

           SumCover := NumCover[0] + NumCover[1] + NumCover[2] + NumCover[3];

           if (SumCover > 0) then  begin
              if (ViewShedSummary <> Nil) then begin
                 if (Target > 0) then TStr := GISdb[Target].dbName
                 else TStr := 'entire map';
                 ViewshedSummary.Add('SENSORS,COVERAGE,COLOR');
                 for i := 0 to MaxColors do if (NumCover[i] > 0) then begin
                    Cover := (100 * NumCover[i] / SumCover);
                    if (i=0) then  TStr := 'No coverage'
                    else if (i=1) then TStr := '1 sensor'
                    else begin
                       if (i=MaxColors) then TStr := '+' else TStr := '';
                       TStr := IntToStr(i) + TStr;
                    end;
                    ViewshedSummary.Add(TStr + ',' + RealToString(Cover,8,2) + '%' + ',' + IntToStr(ConvertPlatformColorToTColor(VisRGB[i])));
                 end;
              end;
           end;
           {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('Done single sensor stats'); {$EndIf}
           fName := System.IOUtils.TPath.Combine(ImageDir, 'fans\num_views' + OverlayFExt);
           PetImage.SaveBitmap(MultCover,fName);
           WriteMapsWorldFile(fName);
           {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('Save ' + FName); {$EndIf}
           SaveBitmap(MultCover,NumFansCoveringFName);

           StretchTheColors(WeaponsTable.RecordCount,MultCover);
           fName := System.IOUtils.TPath.Combine(ImageDir, 'fans\num_views_percent' + OverlayFExt);
           SaveBitmap(TargetMaskBMP,fName);
           WriteMapsWorldFile(fName);
           {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('Save ' + FName); {$EndIf}

           WriteMapsWorldFile(NumFansCoveringFName);
           {$IfDef TimeFigureMultiSensorCoverage} WriteLineToDebugFile('Save Multcover=' + NumFansCoveringFName); {$EndIf}

        FreeAndNil(TargetMaskBMP);
        FreeAndNil(MultCover);
        FreeAndNil(FanBitmap);

        MultBMP.Destroy;
        MaskBMP.Destroy;
        {$IfDef TimeFigureMultiSensorCoverage} writeLineToDebugFile('out at'); {$EndIf}
      end;


      procedure TargetAreaCoverage(var VisBitmap,MaskBitmap,TargetMaskBitmap : tMyBitmap; var Coverage : float64);
      var
         VisRoad,MaskRoad,x,y : integer;
         p0,p2,pm : PRGB;

         {$IfDef RecordTargetAreasCoverage}
               procedure RecordBitmap(Bitmap : tMyBitmap; Name : shortstring);
               begin
                  if (Bitmap = Nil) then WriteLineToDebugFile('   ' + Name + '=NIL')
                  else WriteLineToDebugFile('   ' + Name + '=' +  BitmapSizeString(Bitmap));
               end;
         {$EndIf}

      begin
         {$IfDef RecordTargetAreasCoverage}
            RecordBitmap(VisBitmap,'Visbitmap');
            RecordBitmap(MaskBitmap,'Maskbitmap');
            RecordBitmap(TargetMaskBitmap,'TargetMaskBitmap');
         {$EndIf}
         VisRoad:= 0;
         MaskRoad := 0;
         if (MaskBitmap = Nil) and (TargetMaskBitmap = Nil) then exit;

         for y := 0 to pred(VisBitmap.Height) do begin
            p0 := VisBitmap.Scanline[y];
            if (TargetMaskBitmap = Nil) then begin
              //for target mask, just use visible bitmap
              //if point is colored in target mask, it will be visible if in visible bitmap and masked otherwise
              //for this to be valid, the fan must have considered every point in the area
               p2 := MaskBitmap.Scanline[y];
               for x := 0 to pred(VisBitmap.Width) do begin
                  if (not SameColor(p0[x],RGBTripleWhite)) then inc(VisRoad)
                  else if not (SameColor(p2[x],RGBTripleWhite)) then inc(MaskRoad);
               end;
            end
            else begin
              //this option can use sampling, and will report percentage of points considered
               pm := TargetMaskBitmap.Scanline[y];
               for x := 0 to pred(VisBitmap.Width) do begin
                  if (not SameColor(pm[x],RGBTripleWhite)) then begin
                     if (not SameColor(p0[x],RGBTripleWhite)) then inc(VisRoad)
                     else inc(MaskRoad);
                  end;
               end;
            end;
         end;
         if (VisRoad + MaskRoad = 0) then Coverage := 0
         else Coverage := 100 * VisRoad / (VisRoad + MaskRoad);
      end;



procedure TMapDraw.AddFanToMap(var WeaponsFan : tWeaponsFan);

         procedure PutFanInTable;
         begin
            AddFanToWeaponsTable(PrimMapProj,false,true,GISdb[CurrentFansTable].MyData,WeaponsFan);
         end;

begin
   {$IfDef RecordFan} WriteLineToDebugFile('TMapForm.AddFanToMap in    method=' + FanMethodName[MDDef.wf.FanMethod] + '  algorithm=' + LOSAlgorithmDescription(MDDef.wf)); {$EndIf}
   if (MDDef.FanPickMode = fpSingle) then begin
      DeleteSingleMapLayer(AllFansCoverageFName);
      PutFanInTable;
   end
   else PutFanInTable;

   {$IfDef RecordFan} WriteLineToDebugFile('Fan computed at ' + LatLongDegreeToString(WeaponsFan.W_Lat,WeaponsFan.W_Long) + ' Fan name= ' + WeaponsFan.FanFileName); {$EndIf}
end;



procedure TMapDraw.DrawRadialFan(var WeaponsFan : tWeaponsFan;  var ShowName,CoverName : PathStr);
var
   Fan1 : tDrawFan;
   VisName,MaskName,MergeName : PathStr;
begin
    {$If Defined(RecordFan) {or Defined(AndroidProgress)} or Defined(RecordTime)}
       Stopwatch := TStopwatch.StartNew;
       WriteLineToDebugFile('TMapDraw.DrawRadialFan in, DEM=' + IntToStr(Self.DEMonMap) + ' fan at ' + LatLongDegreeToString(WeaponsFan.W_Lat,WeaponsFan.W_Long));
    {$EndIf}

    if (WeaponsFan.Fan_Name <> '') then begin
       VisName := System.IOUtils.TPath.Combine(SaveViewshedDir,WeaponsFan.Fan_Name + '_vis' + MDDef.FanSaveExt);
       MaskName := System.IOUtils.TPath.Combine(SaveViewshedDir, WeaponsFan.Fan_Name + '_mask' + MDDef.FanSaveExt);
       MergeName := System.IOUtils.TPath.Combine(SaveViewshedDir, WeaponsFan.Fan_Name + '_merge' + MDDef.FanSaveExt);
    end
    else begin
       VisName := NextFileNumber(SaveViewshedDir, 'Fan_vis_',MDDef.FanSaveExt);
       MaskName := NextFileNumber(SaveViewshedDir, 'Fan_mask_',MDDef.FanSaveExt);
       MergeName := NextFileNumber(SaveViewshedDir, 'Fan_merge_',MDDef.FanSaveExt);
    end;

    if (WeaponsFan.FanShowWhat = fsMasked) then WeaponsFan.FanFileName := MaskName
    else if (WeaponsFan.FanShowWhat = fsVisible) then WeaponsFan.FanFileName := VisName
    else if (WeaponsFan.FanShowWhat = fsBoth) then WeaponsFan.FanFileName := MergeName;
    ShowName := WeaponsFan.FanFileName;
    CoverName := ChangeFileExt(ShowName,'.txt');

    {$IfDef RecordFan} writeLineToDebugFile('TMapDraw.DrawRadialFan setup over'); {$EndIf}
    if (not FileExists(ShowName)) or (not FileExists(CoverName)) then begin
       {$IfDef RecordFan} writeLineToDebugFile('TMapDraw.DrawRadialFan Fan1 creating ' + ShowName); {$EndIf}
       RedefineWGS84DatumConstants(WeaponsFan.W_Lat,WeaponsFan.W_Long);
       Fan1 := TDrawFan.Create(MDDef.UseVegInFans,self,WeaponsFan,VisName,MaskName,MergeName,CoverName);
       Fan1.ExecuteFan;
       Fan1.Free;
       {$IfDef RecordFan} writeLineToDebugFile('TMapDraw.DrawRadialFan Fan1 executed'); {$EndIf}
    end;
   {$If Defined(RecordFan) or Defined(AndroidProgress) or Defined(RecordTime)} writeLineToDebugFile('Draw radial fan out, ' + RealToString(Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
end;


procedure TMapDraw.DrawFanOnMap(WeaponsFan : tWeaponsFan; ShowName,CoverName : PathStr);
begin
   {$IfDef RecordFan} WriteLineToDebugFile('TMapDraw.DrawFanOnMap, FanMethod = ' + FanMethodName[MDDef.wf.FanMethod]); {$EndIf}
   DrawRadialFan(WeaponsFan,ShowName,CoverName);
   {$IfDef RecordFan} writeLineToDebugFile('DrawFanOnMap Done'); {$EndIf}
end;


procedure tMapDraw.InsureAllFansDrawn(Table : tMyData; var SummaryBitmap : tMyBitmap; Memo1 : tMemo = nil);
var
   i : integer;
   fName,VisName,MaskName,CoverName,ShowName : PathStr;
   wf : tWeaponsFan;
   Results : tStringList;
   TStr : shortstring;
begin
   {$IfDef RecordFan} WriteLineToDebugFile('tMapDraw.InsureAllFansDrawn in  Filter=' + Table.Filter + '  Write fans to ' + SaveViewshedDir); {$EndIf}
   ShowHourglassCursor;
   Table.First;
   I := 0;
   repeat
      inc(i);
      fName := Table.GetFieldByNameAsString('IMAGE');
      if (fName = '') or (not FileExists(fName)) then begin
         {$IfDef RecordFan} if (fName = '') then WriteLineToDebugFile('No viewshed file') else WriteLineToDebugFile('Could not find ' + fName); {$EndIf}
         if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' Draw viewshed for tower ' + IntToStr(i));
         wf := WeaponsTableBasicParametersToFan(PrimMapProj,Table);
         VisName := NextFileNumber(SaveViewshedDir, 'Fans_vis' + IntToStr(i),MDDef.FanSaveExt);
         MaskName := NextFileNumber(SaveViewshedDir, 'Fans_mask' + IntToStr(i),MDDef.FanSaveExt);
         if (wf.FanShowWhat = fsBoth) then ShowName := NextFileNumber(SaveViewshedDir, 'Fans_merge' + IntToStr(i),MDDef.FanSaveExt)
         else ShowName := '';
         CoverName := ChangeFileExt(ShowName,'.txt');
         DrawRadialFan(wf,ShowName,CoverName);
         Table.Edit;
         Table.SetFieldByNameAsString('IMAGE',ShowName);
         {$IfDef RecordFan} WriteLineToDebugFile('had to create ' + ShowName); {$EndIf}
      end;
      Table.Next;
   until (Table.eof);
   {$IfDef RecordFan} WriteLineToDebugFile('tMapDraw.InsureAllFansDrawn all fans created'); {$EndIf}

   if  (SummaryBitmap <> Nil) then begin
      {$IfDef FanDrawProblems} WriteLineToDebugFile('SummaryBitmap <> Nil, ' + DEMGlb[DEMonMap].KeyParams(True)); {$EndIf}
      ShowHourglassCursor;
      Table.First;
      ShowSatProgress := false;
      I := 0;
      repeat
         inc(i);
         if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' Merge viewshed ' + IntToStr(i));
         fName := Table.GetFieldByNameAsString('IMAGE');
         {$IfDef FanDrawProblems} WriteLineToDebugFile('tMapDraw.InsureAllFansDrawn Overlay ' + fname + BitmapSizeString(SummaryBitmap)); {$EndIf}
         DrawWorldFileImageOnMap(SummaryBitmap,fName);
         CoverName := ChangeFileExt(fName,'.txt');
         if FileExists(CoverName) then begin
            Results := tStringList.Create;
            Results.LoadFromFile(CoverName);
            TStr := ptTrim(Results.Strings[0]);
            SysUtils.DeleteFile(CoverName);
            Results.Free;
         end;
         Table.Next;
      until (Table.eof);

      {$IfDef VCL}
         if MDDef.OutlineFans then OutlineFans(SummaryBitmap.Canvas,Table,ConvertPlatformColorToTColor(MDDef.FanOutLineColor),MDDef.FanOutLineWidth);
      {$EndIf}
      ShowSatProgress := true;
   end;
   ShowDefaultCursor;
   if (Memo1 <> Nil) then Memo1.Lines.Add(TimeToStr(Now) + ' Composite viewshed complete');
   {$IfDef RecordFan} WriteLineToDebugFile('tMapDraw.InsureAllFansDrawn out, ' + BitmapSizeString(SummaryBitmap)); {$EndIf}
end;


procedure tMapDraw.DrawAllFans(Table : tMyData; var MapBitmap : tMyBitmap);
var
   SummaryBitmap : tMyBitmap;
begin
   {$IfDef RecordFan} WriteLineToDebugFile('tMapDraw.DrawAllFans in, filter=' + Table.Filter); {$EndIf}
   Table.First;
   if ((Table <> Nil) and (Table.RecordCount > 0)) then begin
      CreateBitmap(SummaryBitmap,MapBitmap.Width,MapBitmap.Height);
      if (AllFansCoverageFName <> '') and FileExists(AllFansCoverageFName) then begin
         {$IfDef RecordFan} WriteLineToDebugFile('tMapDraw.DrawAllFans Load ' + AllFansCoverageFName); {$EndIf}
         SummaryBitmap := PetImage.LoadBitmapFromFile(AllFansCoverageFName);
      end
      else begin
         InsureAllFansDrawn(Table,SummaryBitmap);
         SaveLayerBitmap(SummaryBitmap,AllFansCoverageFName);
         {$IfDef RecordFan} WriteLineToDebugFile('tMapDraw.DrawAllFans Created ' + AllFansCoverageFName); {$EndIf}
      end;
      DrawOverlayNoDelete(MapBitmap,SummaryBitmap,MDDef.FanOpacity);
      EndProgress;
   end;
   {$IfDef RecordFan} WriteLineToDebugFile('tMapDraw.DrawAllFans out, opacity=' + IntToStr(MDDef.FanOpacity) + BitmapSizeString(MapBitmap) ); {$EndIf}
end;




