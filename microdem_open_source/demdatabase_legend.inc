{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{____________________________________}
{                                    }
{ include file for demdatabase       }
{____________________________________}


function TGISdataBaseModule.BarGraphLegend(Display : boolean = true; theLabel : shortstring = '') : tMyBitmap;
var
   Top : integer;
   MaxStr : shortstring;
   xstart,xend,total,maxstart : Integer;
   Percent,Cum,MaxPC  : float64;
begin
   {$IfDef RecordGlobalDEM} WriteLineToDebugFile('LandCoverSummary in'); {$EndIf}
   CreateBitmap(Result,1400,30);
   Top := 5;

    Total := 0;
    EmpSource.Enabled := false;
    MyData.First;
    xstart := 10;
    Cum := 0;
    MaxPC := 0;
    while not MyData.eof do begin
       Percent := MyData.GetFieldByNameAsFloat('PERCENT');
       if (Percent > MaxPC) then begin
          MaxPC := Percent;
          MaxStr := RealToString(MaxPC,6,1) + '% ' + MyData.GetFieldByNameAsString('NAME');
       end;
       Cum := Cum + Percent;
       xend := 10 + round(Cum / 100 * 500);
       Result.Canvas.Brush.Style := bsSolid;
       Result.Canvas.Brush.Color := MyData.GetFieldByNameAsInteger('COLOR');
       Result.Canvas.Rectangle(xstart,Top,xend,Top + 25);
       xstart := xend;
       MyData.Next;
    end;
    if (theLabel <> '') then begin
       Result.Canvas.Font.Size := 14;
       Result.Canvas.Font.Style := [fsBold];
       Result.Canvas.Brush.Style := bsClear;
       Result.Canvas.TextOut(xend + 10,Top+5,RemoveUnderscores(theLabel));
       Maxstart := xend + 10 + Result.Canvas.TextWidth(RemoveUnderscores(theLabel));
       Result.Canvas.Font.Size := 10;
       Result.Canvas.TextOut(MaxStart,Top+10,MaxStr);
    end;
   if Display then DisplayBitmap(Result,'Land cover distribution');
   {$IfDef RecordGlobalDEM} WriteLineToDebugFile('LandCoverSummary out'); {$EndIf}
end;


function TGISdataBaseModule.CreateDataBaseLegend(SimpleLegend : boolean = false) : tMyBitmap;
const
   BitmapMaxWidth = 1500;
var
   i,ItemHeight,TextStart : integer;
   Lat,Long,Lat2,Long2 : float64;
   PCpresent : boolean;
   PCname,theFilter : ShortString;
   FieldsInDB : tStringList;

   fName : PathStr;
   BaseFilter,UseFilter : AnsiString;
   TStr : ShortString;
   bmp : tMyBitmap;

      procedure DrawSymbol(i : integer);
      var
         Color : tColor;
      begin
         fName := '';
         if IconPresent then begin
            fName := MyData.GetFieldByNameAsString(dbOpts.IconField);
            if (fName <> '') then fName := MainMapData + 'icons\' + fName;
         end;
         if IconPresent and (fName <> '') and (FileExists(fName)) then begin
            bmp := PetImage.LoadBitmapFromFile(fName);
            Result.Canvas.Draw(1,1+i*ItemHeight,bmp);
            TextStart := bmp.Width + 5;
            bmp.Free;
         end
         else begin
             Color := MyData.TColorFromTable;
             Result.Canvas.Brush.Color := Color;
             Result.Canvas.Brush.Style := bsSolid;
             Result.Canvas.Pen.Color := Color;
             Result.Canvas.Pen.Width := 2;
             if LineShapeFile(ShapeFileType)  then begin
                if MyData.FieldExists('LINE_WIDTH') then Result.Canvas.Pen.Width := MyData.GetFieldByNameAsInteger('LINE_WIDTH');
                Result.Canvas.MoveTo(10,10+i*ItemHeight);
                Result.Canvas.LineTo(30,10+i*ItemHeight);
             end
             else begin
                Result.Canvas.Pen.Width := 1;
                Result.Canvas.Rectangle(10,1+i*ItemHeight,30,succ(i)*ItemHeight-4);
            end;
            TextStart := 35;
         end;
      end;


begin
   {$IfDef RecordLegend} WriteLineToDebugFile('TGISDataBase.CreateDataBaseLegend in'); {$EndIf}
   Result := Nil;
   ShowHourglassCursor;
   if (dbOpts.DBAutoShow = dbasKoppen) then Result := KoppenLegend
   else if (dbOpts.dbAutoShow in [dbasQuakeMechColor]) then Result := FocalMechLegend
   else begin
       PCpresent := false;
       if MyData.FieldExists('PERCENT') then begin
          PCpresent := true;
          pcName := 'PERCENT';
       end
       else if MyData.FieldExists('PER_CENT') then begin
          PCpresent := true;
          pcName := 'PER_CENT';
       end;

       if LatLongFieldsPresent and SecondLatLongFieldsPresent then begin
          MyData.First;
          CreateBitmap(Result,BitmapMaxWidth,20*MyData.RecordCount+15);
          ClearBitmap(Result,clNearWhite);
          i := 0;
          repeat
             if ValidLatLongFromTable(Lat,Long) and ValidLat2Long2FromTable(Lat2,Long2) then begin
                Result.Canvas.Pen.Color := Petmar.RainbowColorFunct(i,0,MyData.RecordCount);
                Result.Canvas.Pen.Width := 3;
                Result.Canvas.MoveTo(10,10+i*20);
                Result.Canvas.LineTo(30,10+i*20);
                Result.Canvas.TextOut(40,3+i*20,ExtractFileName(MyData.GetFieldByNameAsString('FILENAME')));
                inc(i);
             end;
             MyData.Next;
          until MyData.EOF;
       end;

       if ColorPresent or RGBColorPresent or LineColorPresent or IconPresent then begin
          ShowHourglassCursor;
          EmpSource.Enabled := false;
          if SimpleLegend or PCpresent then begin
             CreateBitmap(Result,BitmapMaxWidth,20*MyData.RecordCount+5);
             ClearBitmap(Result,clNearWhite);

             Petmar.LoadMyFontIntoWindowsFont(MDDef.DefGISLegendFont,Result.Canvas.Font);
             ItemHeight := Result.Canvas.TextHeight('My') + 5;
             Result.Height := ItemHeight*MyData.RecordCount+5;
             i := 0;
             MyData.First;
             while not MyData.eof do begin
                DrawSymbol(i);
                Result.Canvas.Brush.Style := bsClear;
                if SimpleLegend then TStr := MyData.GetFieldByNameAsString('NAME')
                else begin
                   TStr := RealToString(MyData.GetFieldByNameAsFloat(PCname),-8,2) + '%';
                   TStr := MyData.GetFieldByNameAsString(dbOpts.LabelField) +  '  (' + TStr + ')';
                end;
                Result.Canvas.TextOut(TextStart,3+i*ItemHeight,RemoveUnderscores(TStr));
                MyData.Next;
                inc(i);
             end;
          end
          else begin
             BaseFilter := MyData.Filter;
             if (BaseFilter = '') then UseFilter := ''
             else UseFilter := BaseFilter +  ' AND ';
             if (dbOpts.LabelField = '') or (MyData.GetFieldType(dbOpts.LabelField) <> ftstring) then dbOpts.LabelField := PickField('unique values' ,[ftString,ftInteger,ftSmallInt]);
             FieldsInDB := MyData.UniqueEntriesInDB(dbOpts.LabelField);
             SaveFilterStatus(false);

             CreateBitmap(Result,BitmapMaxWidth,20*FieldsInDB.Count+5);
             Petmar.LoadMyFontIntoWindowsFont(MDDef.DefGISLegendFont,Result.Canvas.Font);
             ItemHeight := Result.Canvas.TextHeight('My') + 5;
             Result.Height := ItemHeight * FieldsInDB.Count + 5;

             for I := 0 to pred(FieldsInDB.Count) do begin
                theFilter := UseFilter + dbOpts.LabelField + '=' + QuotedStr(FieldsInDB.Strings[i]);
                MyData.ApplyFilter(theFilter);
                DrawSymbol(i);
                Result.Canvas.Brush.Style := bsClear;
                TStr := RemoveUnderscores(MyData.GetFieldByNameAsString(dbOpts.LabelField));
                if MDDef.DBLegendN then TStr := TStr + ' (n=' + IntToStr(MyData.RecordCount) +  ')';
                Result.Canvas.TextOut(TextStart,3+i*ItemHeight, TStr);
                MyData.Next;
             end;
             MyData.ApplyFilter(BaseFilter);
          end;
          if (Result <> Nil) then begin
             PutBitmapInBox(Result);
          end;
          RestoreFilterStatus;
          EmpSource.Enabled := true;
          ShowDefaultCursor;
       end;
   end;
end;


procedure TGISdataBaseModule.DrawLayerLegend(MinX,MaxX : float64; aField: ShortString);
var
   Bitmap,cbmp : tMyBitmap;
begin
   if dbOpts.DBLegendLocation.DrawItem then begin
      CopyImageToBitmap(TheMapOwner.Image1,cbmp);
      if dbOpts.DBLegendLocation.HorizontalLegend then Bitmap := DefaultHorizontalLegendOnBitmap(MinX,MaxX,'',aField,dbOpts.DBColorScheme)
      else Bitmap := DefaultVerticalLegendOnBitmap(MinX,MaxX,'',aField,dbOpts.DBColorScheme);
      TheMapOwner.MapDraw.PositionBitmap(cbmp,Bitmap,dbOpts.DBLegendLocation.MapPosition);
      TheMapOwner.Image1.Picture.Graphic := cbmp;
      cbmp.Free;
   end;
end;


function TGISdataBaseModule.FocalMechLegend : tMyBitmap;
begin
    CreateBitmap(Result,225,100);
    Result.Canvas.Font.Name := 'Verdana';
    Result.Canvas.Font.Style := [fsBold];
    Result.Canvas.Font.Size := 12;
    Petmar.ScreenSymbol(Result.Canvas,25,10,FilledBox,dbOpts.Symbol.Size,ConvertTColorToPlatformColor(clRed));
    Result.Canvas.TextOut(45,5,'Thrust fault');
    Petmar.ScreenSymbol(Result.Canvas,25,35,FilledBox,dbOpts.Symbol.Size,ConvertTColorToPlatformColor(clLime));
    Result.Canvas.TextOut(45,30,'Normal fault');
    Petmar.ScreenSymbol(Result.Canvas,25,60,FilledBox,dbOpts.Symbol.Size,ConvertTColorToPlatformColor(clBlue));
    Result.Canvas.TextOut(45,55,'Strike-slip fault');
    Petmar.ScreenSymbol(Result.Canvas,25,85,FilledBox,dbOpts.Symbol.Size,ConvertTColorToPlatformColor(clOlive));
    Result.Canvas.TextOut(45,80,'Oblique-slip fault');
    Result.Canvas.Rectangle(0,0,pred(Result.Width),pred(Result.Height));
end;


function TGISdataBaseModule.ChloroplethLegend(aLabel : shortString) : tMyBitmap;
begin
   if (aLabel = '') then aLabel := dbOpts.FloatColorField;
   if copy(aLabel,1,5) = 'LINK-' then Delete(aLabel,1,5);
   Result := DefaultHorizontalLegendOnBitmap(dbOpts.ColorMin,dbOpts.ColorMax,'',aLabel,dbOpts.DBColorScheme,dbOpts.DBColorPaletteName,dbOpts.ReverseColorTable);
end;


function TGISdataBaseModule.StringFieldLegend : tMyBitmap;
var
   i,tl,l,fHi : integer;
   TStr : shortstring;
begin
   {$IfDef RecordLegend} WriteLineToDebugFile('Tgis_scaled_form.MakeStringFieldLegend'); {$EndIf}
   EmpSource.Enabled := false;
   if (StringCategories = Nil) then PetDBUtils.FindUniqueEntriesLinkPossible(MyData,LinkTable,dbOpts.LinkFieldThisDB,dbOpts.LinkFieldOtherDB,dbOpts.FloatColorField,StringCategories);
   fHi := 22;
   DefineDBColorTable;
   CreateBitmap(Result,1000,fHi*StringCategories.Count);
   Result.Canvas.Brush.Color := clGray;
   Result.Canvas.Brush.Style := bsSolid;
   Result.Canvas.Pen.Color := clGray;

   tl := 0;
   for I := 0 to pred(StringCategories.Count) do begin
      SetBitmapColorsForStringFieldPlot(Result,I,pred(StringCategories.Count));
      Result.Canvas.Rectangle(10,0+i*fHi,40,15 + i*fHi);
   end;

   Result.Canvas.Brush.Color := clWhite;
   Result.Canvas.Font.Size := 12;
   Result.Canvas.Font.Style := [fsBold];

   for i := 0 to pred(StringCategories.Count) do begin
      {$IfDef RecordLegend} WriteLineToDebugFile(StringCategories.Strings[i]); {$EndIf}
      TStr := StringCategories.Strings[i];
      Result.Canvas.TextOut(50,i*fHi,TStr);
      l := Result.Canvas.TextWidth(TStr);
      if (l + 50 > tl) then tl := l + 50;
   end;
   Result.Canvas.Brush.Style := bsClear;
   Result.Canvas.Pen.Color := clBlack;
   Result.Canvas.Pen.Width := 1;
   Result.Canvas.Rectangle(0,0,tl+3,pred(Result.Height));
   Result.Width := tl + 3;
   {$IfDef RecordLegend} WriteLineToDebugFile('Tgis_scaled_form.MakeStringFieldLegend, size =' + IntToStr(Result.width) + 'x' + intToStr(Result.Height)); {$EndIf}
end;


procedure TGISdataBaseModule.CreatePopupLegend(Title : shortstring = ''; SaveName : PathStr = '');
var
   bmp : tMyBitmap;
   x,y : integer;
begin
{$IfDef RecordLegend} WriteLineToDebugFile('TGISdataBaseModule.CreatePopupLegend in'); {$EndIf}
   bmp := nil;
   EmpSource.Enabled := false;
   if (dbOpts.DBAutoShow = dbasColorByString) then begin
       bmp := StringFieldLegend;
   end
   else if (dbOpts.DBAutoShow = dbasColorByNumeric) then begin
      if dbOpts.DBLegendLocation.HorizontalLegend then bmp := DefaultHorizontalLegendOnBitmap(dbOpts.ColorMin,dbOpts.ColorMax,'',dbOpts.FloatColorField,dbOpts.DBColorScheme)
      else bmp := DefaultVerticalLegendOnBitmap(dbOpts.ColorMin,dbOpts.ColorMax,'',dbOpts.FloatColorField,dbOpts.DBColorScheme);
   end
   else begin
      bmp := CreateDataBaseLegend;
   end;
   if (Title <> '') then begin
      bmp.Canvas.Font.Size := 14;
      x := bmp.Canvas.TextWidth(Title);
      y := bmp.Canvas.TextHeight(Title);
      bmp.Height := Bmp.Height + 25;
      if (bmp.Width < x) then bmp.Width := x + 20;
      bmp.Canvas.TextOut(15,Bmp.Height - y,Title);
   end;
   if (SaveName <> '') then SaveBitmap(bmp,SaveName);

   if (bmp <> Nil) then begin
      Petimage_form.DisplayBitmap(BMP,'Legend ' + dbName,true,true);
      FreeAndNil(Bmp);
   end;
   ShowStatus;
end;


procedure TGISdataBaseModule.DoTheLegend(var Bitmap : tMyBitmap);
var
   i : integer;
   Legend : tMyBitmap;
   Values  : array[0..255] of float64;
   Colors : TColors256;
   LabelStr : shortstring;
begin
   {$IfDef RecordLegend} WriteLineToDebugFile('TGISdataBaseModule.DoTheLegend in'); {$EndIf}
   if (dbOpts.DBLegendLocation.DrawItem) and (theMapOwner <> nil) and (Bitmap <> Nil) then begin
       {$IfDef RecordLegend} WriteLineToDebugFile('Legend for ' + dbName + ' on ' + TheMapOwner.Caption); {$EndIf}

       Legend := Nil;
       if (dbOpts.dbAutoShow in [dbasColorByString]) then  begin
          Legend := StringFieldLegend;
       end
       else if (dbOpts.dbAutoShow in [dbasQuakeMechColor]) then begin
          Legend := FocalMechLegend;
       end
       else if (dbOpts.dbAutoShow in [dbasTTFontSymbol,dbasColorByNumeric,dbasVector,dbasMonthlyTemp,dbasMonthlyRain]) then begin
         if (dbOpts.DBColorScheme = LegChloropleth) and (ColorDefTable.ZTableEntries < 75) then begin
            Legend := ChloroplethLegend(LegendCaption);
         end
         else begin
            {$IfDef RecordLegend} WriteLineToDebugFile('Compute legend'); {$EndIf}
            if (dbOpts.ColorMax-dbOpts.ColorMin < 0.00000001) then exit;
             for i := 0 to 255 do begin
                Values[i] := dbOpts.ColorMin + i/255 * (dbOpts.ColorMax-dbOpts.ColorMin);
                Colors[i] := SelectedColorSchemeColorFunct(dbOpts.DBColorScheme,ColorDefTable,Values[i],dbOpts.ColorMin,dbOpts.ColorMax);
             end;
             if (dbOpts.dbAutoShow in [dbasVector]) and dbOpts.GISVectorsByMaxSpeed then begin
                if (LegendCaption = '') then LabelStr := 'Speed'
                else LabelStr := LegendCaption;
             end
             else begin
                if (LegendCaption = '') then LabelStr := dbOpts.FloatColorField
                else LabelStr := LegendCaption;
                if copy(LabelStr,1,5) = 'LINK-' then Delete(LabelStr,1,5);
             end;
             if dbOpts.DBLegendLocation.HorizontalLegend then begin
                Legend := HorizontalLegendOnBitmap(Colors,Values,'',LabelStr,dbOpts.DBLegendLocation.LegendSize);
             end
             else begin
                Legend := VerticalLegendOnBitmap(Colors,Values,'',LabelStr);
             end;
             {$IfDef RecordLegend} WriteLineToDebugFile('Legend drawn'); {$EndIf}
         end;
      end;
      if (Legend <> Nil) then begin
         if dbOpts.Grayscale then MakeTheBitmapGrayScale(Legend);
         if dbOpts.Subdue then MakeTheBitmapSubdued(Legend);
         TheMapOwner.MapDraw.PositionBitmap(Bitmap,Legend,dbOpts.DBLegendLocation.MapPosition);
      end;
   end;
end;


procedure LegendFromShapeFileGroup(MapOwner : tMapForm; FName : PathStr);
var
   TheIndex : tMyData;
   db,x,MaxX, i,OnY : integer;
   PlotIt : boolean;
   TStr : ShortString;
   Bitmap : tMyBitmap;
begin
    if (MapOwner = Nil) then exit;
                
    TheIndex := tMyData.Create(fName);
    OnY := 15;
    MaxX := 0;
    LoadMyFontIntoWindowsFont(MDDef.DefGisLabelFont1,Bitmap.Canvas.Font);
    for i := 9 downto 0 do begin
        TheIndex.ApplyFilter('ORDER = ' + IntToStr(i) + ' AND PLOT =' + QuotedStr('Y'));
        TheIndex.First;
        while not TheIndex.EOF do begin
           if  TheIndex.FieldExists('PIXEL_SIZE') and (TheIndex.GetFieldByNameAsString('PIXEL_SIZE') <> '') then begin
              PlotIt := (MapOwner.MapDraw.ScreenPixelSize < TheIndex.GetFieldByNameAsInteger('PIXEL_SIZE'));
           end
           else PlotIt := true;

           if PlotIt then begin
              fName := TheIndex.GetFieldByNameAsString('FILENAME');
              if OpenNumberedGISDataBase(db,fName) then with GISdb[db] do begin
                  if TheIndex.FieldExists('FILTER') and (TheIndex.GetFieldByNameAsString('FILTER') <> '') then begin
                     GISdb[db].MyData.ApplyFilter(TheIndex.GetFieldByNameAsString('FILTER'));
                  end;
                  if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
                     GISdb[db].SetColorsFromDB(TheIndex);
                     GISdb[db].PrepColors(Bitmap);
                     case TheIndex.GetFieldByNameAsInteger('SHAPE_TYPE') of
                        1 : Petmar.ScreenSymbol(Bitmap.Canvas,MDDef.LegendGraphWidth div 2,OnY+ MDDef.LegendSingleHeight div 2,GISdb[db].dbOpts.Symbol);
                        3 : PetImage.DrawLine(Bitmap,5,OnY + MDDef.LegendSingleHeight div 2,  MDDef.LegendGraphWidth-5,OnY + MDDef.LegendSingleHeight div 2);
                        5 : Bitmap.Canvas.Rectangle(5,OnY+2,MDDef.LegendGraphWidth-5,OnY + MDDef.LegendSingleHeight-2);
                     end;
                     Bitmap.Canvas.Brush.Style := bsClear;
                     TStr := TheIndex.GetFieldByNameAsString('CAPTION');
                     Bitmap.Canvas.TextOut(MDDef.LegendGraphWidth,OnY+2,TStr);
                     x := MDDef.LegendGraphWidth + Bitmap.Canvas.TextWidth(TStr);
                     if (x > MaxX) then MaxX := x;

                     inc(onY,MDDef.LegendSingleHeight);
                  end;
              end;
              if GISdb[db].CloseDataBase then CloseAndNilNumberedDB(db);
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


