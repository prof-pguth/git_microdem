
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for petmar              }
{________________________________________}


        function DoLegendOnBitmap(Horizontal : boolean; Colors : tColors256; Values : array of float64; Units : shortstring; LegendTitle : shortstring = ''; LegendSize : integer = 1) : tMyBitmap;
         var
           Width,Height,Size,i,j,LastLabel,Dec,xloc,yloc,TopText,LabelsDrawn : integer;
           Min,Max,Range,Tick,FirstTick  : float64;
           First          : boolean;
           TStr           : ShortString;

            procedure DoCenterAndEnds(DoMiddle : boolean = true);
            begin
               with Result.Canvas do begin
                   TopText := LegendSize * MDDef.LegendBarWidth + MDDef.LegendTickSize + 3;
                   xloc := 5 + LegendSize * (255) * Size;
                   MoveTo(xloc,LegendSize * MDDef.LegendBarWidth);
                   LineTo(xloc,LegendSize * MDDef.LegendBarWidth + MDDef.LegendTickSize);
                   TStr := RealToString(Min,-12,-Dec);
                   TextOut(xloc - TextWidth(TStr),TopText,TStr);

                   if DoMiddle then begin
                     xloc := 5 + LegendSize * (127) * Size;
                     MoveTo(xloc,LegendSize * MDDef.LegendBarWidth);
                     LineTo(xloc,LegendSize * MDDef.LegendBarWidth + MDDef.LegendTickSize);
                     TStr := '0';
                     TextOut(xloc - TextWidth(TStr) div 2,TopText,TStr);
                   end;

                   xloc := 5 + LegendSize * (0) * Size;
                   MoveTo(xloc,LegendSize * MDDef.LegendBarWidth);
                   LineTo(xloc,LegendSize * MDDef.LegendBarWidth + MDDef.LegendTickSize);
                   TStr := RealToString(Max,-12,-Dec);
                   TextOut(xloc,TopText,TStr);
               end;
            end;

         begin {function DoLegendOnBitmap(}
            {$IfDef RecordLegends} if Horizontal then TStr := 'horizontal'else TStr := 'vertical'; WriteLineToDebugFile('DoLegendOnBitmap ' + TStr); {$EndIf}

            {$IfDef RecordDetailedLegends}
               WriteLineToDebugFile('title=' + LegendTitle + '  units=' + Units);
               WriteLineToDebugFile('MDDef.LegendBarWidth=' + IntToStr(MDDef.LegendBarWidth) + '  MDDef.LegendTickSize=' + IntToStr(MDDef.LegendTickSize));
            {$EndIf}

            Size := 1;
            PetImage.CreateBitmap(Result,10,10);
            LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);

            if Horizontal then begin
               if (LegendTitle <> '') then i := 2 else i := 1;
               Height := LegendSize * MDDef.LegendBarWidth + MDDef.LegendTickSize + 5 + i * Result.Canvas.TextHeight('Mj');
               Width := LegendSize * (256) + 20 + Result.Canvas.TextWidth(Units) + 1;
               {$IfDef RecordLegends} WriteLineToDebugFile('Legend: ' + IntToStr(Width) + 'x' + IntToStr(Height)); {$EndIf}
            end
            else begin
               if (LegendTitle <> '') then i := 45 else i := 20;
               Height := LegendSize * (256) + 20 +i;
               Width := 600;
            end;
            Result.Width := Width;
            Result.Height := Height;
            ClearBitmap(Result,clNearWhite);

            if (LegendTitle <> '') then  begin
               i := (Result.Width - Result.Canvas.TextWidth(LegendTitle)) div 2;
               j := Result.Height - Result.Canvas.TextHeight(LegendTitle) - 3;
               Result.Canvas.TextOut(i,j,RemoveUnderscores(LegendTitle));
               {$IfDef RecordLegends} WriteLineToDebugFile('Legend Title at: ' + IntToStr(i) + '--' + IntToStr(j)); {$EndIf}
            end;

            with Result.Canvas do begin
               Brush.Style := bsSolid;
               Brush.Color := clWhite;
               Brush.Style := bsClear;
               Min := 99e45;
               Max := -99e45;
               for i := 0 to 255 do begin
                  if abs(Values[i]) < pred(MaxInt) then begin
                     Pen.Color := Colors[i];
                     Pen.Width := LegendSize;
                     if Horizontal then begin
                        MoveTo(5 + Size * LegendSize * (255-i),1);
                        LineTo(5 + Size * LegendSize * (255-i),LegendSize * MDDef.LegendBarWidth);
                     end
                     else begin
                        MoveTo(1,5 + Size * LegendSize *(255-i));
                        LineTo(MDDef.LegendBarWidth,5+Size * LegendSize *(255-i));
                     end;
                     if (Min > Values[i]) then Min := Values[i];
                     if (Max < Values[i]) then Max := Values[i];
                  end;
               end;

               Range := Max - Min;
               Tick := PetMath.GetTickInt(256,25,Range);
               if (Range < 0.00002) or (abs(Max) < 0.002) then Dec := 6
               else if (Range < 0.0002) or (abs(Max) < 0.002) then Dec := 5
               else if (Range < 0.002) or (abs(Max) < 0.002) then Dec := 4
               else if (Range < 0.02) or (abs(Max) < 0.02) then Dec := 3
               else if (Range < 0.2) or (abs(Max) < 0.2) then Dec := 2
               else if (Range < 2) or (abs(Max) < 2) then Dec := 1
               else Dec := 0;

               if Min <= 0 then FirstTick := (trunc(Min / Tick)) * Tick
               else FirstTick := succ(trunc((Min-0.0001) / Tick)) * Tick;

               if Horizontal and (abs(Max + Min) < 0.0001) then begin
                   DoCenterAndEnds;
               end
               else begin
                  First := true;
                  LastLabel := MaxInt;
                  LabelsDrawn := 0;
                  repeat
                     i := 0;
                     while (abs(Values[i] - FirstTick) > abs(Values[succ(i)] - FirstTick)) and (i < 254) do inc(i);
                     if (i < 256) then begin
                        TStr := RealToString(FirstTick,-12,-Dec);
                        Pen.Color := clBlack;
                        if First then begin
                           First := false;
                           if Horizontal then begin
                              TopText := LegendSize * MDDef.LegendBarWidth + MDDef.LegendTickSize + 3;
                              LastLabel := LegendSize * (256) + 5;
                              TextOut(LastLabel,TopText,Units);
                           end
                           else TStr := TStr + Units;
                        end;
                        if Horizontal then begin //draw the ticks
                           xloc := 5 + LegendSize * (256-i)*Size;
                           if LastLabel > (xloc -5 + TextWidth(TStr)) then begin
                              LastLabel := xloc - TextWidth(TStr) div 2;
                              if (LastLabel < 0) then LastLabel := 0;
                              TextOut(LastLabel,TopText,TStr);
                              inc(LabelsDrawn);
                              {$IfDef RecordLegends} WriteLineToDebugFile('Tick label at: ' + IntToStr(LastLabel) + '--' + IntToStr(j)); {$EndIf}
                              Pen.Width := 3;
                           end
                           else Pen.Width := 1;
                           MoveTo(xloc,LegendSize * MDDef.LegendBarWidth);
                           LineTo(xloc,LegendSize * MDDef.LegendBarWidth + MDDef.LegendTickSize);
                        end
                        else begin
                           yloc := 5+LegendSize *256*Size-Size*i;
                           MoveTo(15,yloc);
                           LineTo(25,yloc);
                           yloc := yloc - TextHeight(TStr) div 2;
                           if yloc < 1 then yloc := 1;

                           if yloc < LastLabel - TextHeight(TStr) then begin
                              TextOut(28,yloc,TStr);
                              LastLabel := yloc;
                              inc(LabelsDrawn);
                           end;
                        end;
                        FirstTick := FirstTick + Tick;
                     end;
                  until (FirstTick > Max) or (i = 255);
                  if LabelsDrawn = 1 then begin
                     if Horizontal then begin
                        DoCenterAndEnds(false);
                     end
                     else begin
                     end;
                  end;
               end;
            end;
            PutBitmapInBox(Result);
         end {function DoLegendOnBitmap(};


         function HorizontalLegendOnBitmap(Colors : tColors256; Values : array of float64; Units,LegendTitle : shortstring; LegendSize : integer = 1): tMyBitmap;
         begin
            Result := DoLegendOnBitmap(true,Colors,Values,' ' + Units,LegendTitle,LegendSize);
         end;


         function VerticalLegendOnBitmap(Colors : tColors256; Values : array of float64; Units,LegendTitle : shortstring): tMyBitmap;
         begin
            Result := DoLegendOnBitmap(false,Colors,Values,Units,LegendTitle);
         end;


         procedure MakeValuesForLegend(var Colors : tColors256; var Vals : array of float64; Min,Max : float64; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false);
         var
            ZColorTable : tColorTableDefinitions;
            i : integer;
            x : float64;
         begin
            {$IfDef RecordLegends} WriteLineToDebugFile('MakeValuesForLegend in'); {$EndIf}
            if (Legend = LegChloropleth) then begin
               {$IfDef RecordLegends} WriteLineToDebugFile('Chloropheth ' + ChloroplethScheme); {$EndIf}
              {$IfDef ExGIS}
              {$Else}
              if DefineColorTableValues(ChloroplethScheme,Min,Max,ZColorTable,Reverse) then begin
                 for i := 0 to 255 do begin
                    Colors[i] := ConvertPlatformColorToTColor(ZColorTable.ZTableColors[1 + round(i/255 * pred(ZColorTable.ZTableEntries))]);
                 end;
              end;
              {$EndIf}
            end
            else begin
               if (Min > Max) then begin
                  x := Min;
                  Min := Max;
                  Max := x;
                  case legend of
                     LegSpectrum : for i := 0 to 255 do Colors[255-i] := ConvertPlatformColorToTColor(SpectrumRGBFunct(i,0,255));
                     LegRainbows : for i := 0 to 255 do Colors[255-i] := RainbowColorFunct(i,0,255);
                     LegGrays : for i := 0 to 255 do Colors[255-i] := RGB(i,i,i);
                     LegTerrain : for i := 0 to 255 do Colors[255-i] := TerrainTColor(i,0,255);
                  end;
               end
               else begin
                  case legend of
                     LegSpectrum : for i := 0 to 255 do Colors[i] := ConvertPlatformColorToTColor(SpectrumRGBFunct(i,0,255));
                     LegRainbows : for i := 0 to 255 do Colors[i] := RainbowColorFunct(i,0,255);
                     LegGrays : for i := 0 to 255 do Colors[i] := RGB(i,i,i);
                     LegTerrain :  for i := 0 to 255 do Colors[i] := TerrainTColor(i,0,255);
                  end;
               end;
            end;
            for i := 0 to 255 do Vals[i] := Min + (i / 255) * (Max - Min);
         end;


         function DefaultHorizontalLegendOnBitmap(Min,Max : float64; Units,LegendTitle : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false): tMyBitmap;
         var
            Colors : tColors256;
            Vals : array[0..255] of float64;
         begin
            MakeValuesForLegend(Colors,Vals,Min,Max,Legend,ChloroplethScheme,Reverse);
            Result := DoLegendOnBitmap(true,Colors,vals,Units,LegendTitle);
         end;


         function DefaultVerticalLegendOnBitmap(Min,Max : float64; Units,LegendTitle : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false): tMyBitmap;
         var
            Colors : tColors256;
            Vals : array[0..255] of float64;
         begin
            {$IfDef RecordLegends} WriteLineToDebugFile('DefaultVerticalLegendOnBitmap, range ' + RealToString(Min,-12,-4) + ' to ' + RealToString(Max,-12,-4)); {$EndIf}
            MakeValuesForLegend(Colors,Vals,Min,Max,Legend,ChloroplethScheme,Reverse);
            Result := DoLegendOnBitmap(false,Colors,vals,Units,LegendTitle);
         end;



function MakeLegend(DEMsUsed,ColorsUsed : tstringList; Horizontal : boolean = true; MaxWide : integer = 2400) : tMyBitmap;
var
   i,Left,Top,StartFontSize : integer;

         procedure AnEntry(DEM : shortString; Color : tColor);
         begin
            Result.Canvas.Pen.Color := Color;
            Result.Canvas.Brush.Color := Result.Canvas.Pen.Color;
            Result.Canvas.Brush.Style := bsSolid;
            Result.Canvas.Rectangle(Left,Top,Left + Result.Canvas.TextHeight(DEM),Top + Result.Canvas.TextHeight(DEM));
            Left := Left + Result.Canvas.TextHeight(DEM) + 10;
            Result.Canvas.Brush.Style := bsClear;
            Result.Canvas.TextOut(Left,Top,DEM);
            if Horizontal then Left := Left + Result.Canvas.TextWidth(DEM + '   ')
            else begin
               Top := Top + 10 + Result.Canvas.TextHeight(DEM);
               Left := 15;
            end;
         end;

begin
   StartFontSize := MDDef.DEMIXlegendFontSize;
   Result := Nil;
   repeat
      if (Result <> Nil) then Result.Destroy;
      CreateBitmap(Result,MaxWide,2000);
      LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
      Result.Canvas.Font.Size := StartFontSize;
      Result.Canvas.Font.Style := [fsBold];
      Left := 15;
      Top := 10;
      for i := 0 to pred(DEMsUsed.Count) do begin
         AnEntry(DEMsUsed.Strings[i],StrToInt(ColorsUsed[i]));
      end;
      PutBitmapInBox(Result);
      Dec(StartFontSize);
   until (Result.Width < MaxWide);
end;

function MakeColorScaleBitmap(width,height : integer; ColorScheme : tLegendColors; ColorTable : tColorTableDefinitions) : tMyBitmap;
var
   i,j : integer;
begin
   CreateBitmap(Result,width,height);
   for i := 0 to pred(width) do begin
      j := succ(trunc(ColorTable.ZTableEntries * i / Width));
      Result.Canvas.Pen.Color := ConvertPlatformColorToTColor(ColorTable.ZTableColors[j]);
      DrawLine(Result,i,0,i,pred(height));
   end;
end;



