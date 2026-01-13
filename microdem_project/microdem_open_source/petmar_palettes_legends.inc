
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for petmar              }
{________________________________________}


function SelectedColorSchemeColorFunct(ColorScheme: tLegendColors; ColorTable : tColorTableDefinitions; z : float64; Min,Max : float64) : TColor;
var
   k : integer;
begin
   {$IfDef VCL}
      case ColorScheme of
         LegGrays      : Result := GrayColorFunct(z,Min,Max);
         LegRainbows   : Result := RainbowColorFunct(z,Min,Max);
         LegTerrain    : Result := TerrainTColor(z,Min,Max);
         LegSpectrum   : Result := SpectrumColorFunct(z,Min,Max);
         LegChloropleth : Result := ConvertPlatformColorToTColor(ColorFromZColorTable(ColorTable,z,k));
         else begin end;
      end;
   {$EndIf}
end;


function ColorFromZColorTable(ZColorTable : tColorTableDefinitions; zj : float64; var k : integer) : tRGBTriple;
var
   i : integer;
begin
   {$IfDef VCL}
      with ZColorTable do begin
         if (ZTableEntries > 256) then begin
            for i := 1 to ZTableEntries do begin
               if (zj <= zBigTableValue^[i]) or (i = ZTableEntries) then begin
                  Result := zBigTableColors^[i];
                  k := i;
                  exit;
               end;
            end;
         end
         else begin
            for i := 1 to ZTableEntries do begin
               {$IfDef RecordColorFromZColorTable} WriteLineToDebugFile('   ' + IntToStr(i) + '  ' + RealToString(zTableValue[i],-18,-4)); {$EndIf}
               if (zj <= zTableValue[i]) or (i = ZTableEntries) then begin
                  Result := zTableColors[i];
                  k := i;
                  exit;
               end;
            end;
         end;
      end;
   {$EndIf}
end;


function GrayColorFunct(i : integer) : TColor;
begin
   Result := RGB(i,i,i);
end;

function GrayColorFunct(z,Min,Max : float64) : TColor;
var
   zi : integer;
begin
   zi := ValidByteRange(round(255.0 * (z - Min) / (Max - Min)));
   Result := RGB(zi,zi,zi);
end;

		 
procedure GetTerrainRGBTrip(z,Min,Max : float64; var r,g,b : byte);
var
   zi : integer;

      procedure InterpolateColor(c1,c2 : integer; Dist : float64);
      begin
         r := TerrainCuts[c1,1] + round(Dist * (TerrainCuts[c2,1] - TerrainCuts[c1,1])) ;
         g := TerrainCuts[c1,2] + round(Dist * (TerrainCuts[c2,2] - TerrainCuts[c1,2])) ;
         b := TerrainCuts[c1,3] + round(Dist * (TerrainCuts[c2,3] - TerrainCuts[c1,3])) ;
      end;

begin
   zi := ValidByteRange(round(255.0 * (z - Min) / (Max - Min)));
   case zi of
      0..80 : InterpolateColor(0,1,zi/80);
      81..170 : InterpolateColor(1,2,(zi-81)/90);
      171..200 : InterpolateColor(2,3,(zi-171)/29);
      201..255 : InterpolateColor(3,4,(zi-201)/54);
      else begin
         r := 0;
         g := 0;
         b := 0;
      end;
   end;
end;


function RainbowRGBFunct(z,MinV,MaxV : float64) : tPlatformColor;
const
   ColorStep = 2;
   FirstColor = 136;
var
   zi : integer;
begin
   if (abs(MaxV-MinV) < 0.0001) then begin
      Result := RGBTrip(255,0,0);
   end
   else begin
      zi := ValidByteRange(round(255.0 * (z - MinV) / (MaxV - MinV)));
      case zi of
         0..42 : Result := RGBTrip(0, 0,FirstColor + ColorStep * zi); {blues}
         43..85 : Result := RGBTrip(0,FirstColor + ColorStep * (zi-43),FirstColor + ColorStep * (zi-43)); {Cyans}
         86..128 : Result := RGBTrip(0,FirstColor + ColorStep * (zi-86),0); {greens}
         129..171: Result := RGBTrip(FirstColor + ColorStep * (zi-129), FirstColor + ColorStep * (zi-129),0);  {yellows}
         172..214 : Result := RGBTrip(FirstColor + ColorStep * (zi-172), 0,0);  {reds}
         215..255 : Result := RGBTrip(FirstColor + ColorStep * (zi-215), 0,FirstColor + ColorStep * (zi-215));  {magentas}
         else Result := RGBTrip(0,0,0);
      end;
   end;
end {function};

         PROCEDURE WavelengthToRGB(CONST Wavelength :  float64;  VAR R,G,B:  BYTE);
         // Adapted from www.isc.tamu.edu/~astro/color.html
         // SpectraLibrary
         // Copyright (C) 1998, 2001, Earl F. Glynn, Overland Park, KS.
         // Updated June 2001 for "FrequencyToRGB" and conversion routines.
         // May be copied freely for non-commercial use.
         // E-Mail:  EarlGlynn@att.net
            CONST
              Gamma        =   0.60;
              IntensityMax = 255;
            VAR
              Blue  :  DOUBLE;
              factor:  DOUBLE;
              Green :  DOUBLE;
              Red   :  DOUBLE;

            FUNCTION Adjust(CONST Color, Factor:  DOUBLE):  INTEGER;
            BEGIN
              IF   Color = 0.0 THEN RESULT := 0     // Don't want 0^x = 1 for x <> 0
              ELSE RESULT := ROUND(IntensityMax * Math.Power(Color * Factor, Gamma))
            END {Adjust};

          BEGIN

            CASE TRUNC(Wavelength) OF
              380..439:
                BEGIN
                  Red   := -(Wavelength - 440) / (440 - 380);
                  Green := 0.0;
                  Blue  := 1.0
                END;

              440..489:
                BEGIN
                  Red   := 0.0;
                  Green := (Wavelength - 440) / (490 - 440);
                  Blue  := 1.0
                END;

              490..509:
                BEGIN
                  Red   := 0.0;
                  Green := 1.0;
                  Blue  := -(Wavelength - 510) / (510 - 490)
                END;

              510..579:
                BEGIN
                  Red   := (Wavelength - 510) / (580 - 510);
                  Green := 1.0;
                  Blue  := 0.0
                END;

              580..644:
                BEGIN
                  Red   := 1.0;
                  Green := -(Wavelength - 645) / (645 - 580);
                  Blue  := 0.0
                END;

              645..780:
                BEGIN
                  Red   := 1.0;
                  Green := 0.0;
                  Blue  := 0.0
                END;

              ELSE
                Red   := 0.0;
                Green := 0.0;
                Blue  := 0.0
            END;

            // Let intensity fall off near vision limits
            CASE TRUNC(Wavelength) OF
              380..419:  factor := 0.3 + 0.7*(Wavelength - 380) / (420 - 380);
              420..700:  factor := 1.0;
              701..780:  factor := 0.3 + 0.7*(780 - Wavelength) / (780 - 700)
              ELSE       factor := 0.0
            END;

            R := Adjust(Red,   Factor);
            G := Adjust(Green, Factor);
            B := Adjust(Blue,  Factor)
          END {WavelengthToRGB};




function SpectrumRGBFunct(z,Min,Max : float64) : tPlatformColor;
var
   r,g,b : byte;
begin
   if z < Min then z := Min;
   if z > Max then z := Max;
   z := 380 + 400 * (z - Min) / (Max - Min);
   WavelengthToRGB(z,r,g,b);
   Result := RGBtrip(r,g,b);
end;


function TerrainRGBFunct(z,Min,Max : float64) : tPlatformColor;
var
   r,g,b : byte;
begin
   GetTerrainRGBTrip(z,Min,Max,r,g,b);
   Result := RGBtrip(r,g,b);
end;


function OceanRGBFunct(z,Min,Max : float64) : tPlatformColor;
var
   zi : integer;
begin
   zi := ValidByteRange(round(255.0 * (z - Min) / (Max - Min)));
   if (zi < 130) then Result := RGBTrip(0,0,round(90 + zi))
   else Result := RGBTrip(0,0,round(90 + (zi-130)));
end;


         function TerrainTColor(z,aMin,aMax : float64) : TColor;
         var
            r,g,b : byte;
         begin
            GetTerrainRGBTrip(z,aMin,aMax,r,g,b);
            Result := RGB(r,g,b);
         end;


         function PlatformRainbowColorFunct(z,Min,Max : float64) : TPlatformColor;
         begin
            Result := ConvertTColorToPlatformColor(RainbowColorFunct(z,Min,Max));
         end;


         function RainbowColorFunct(z,Min,Max : float64) : TColor;
         const
            FirstColor = 136;
            ColorStep = 2;
         var
            zi : integer;
         begin
            if abs(Max-Min) < 0.0001 then Result := clRed
            else begin
               zi := ValidByteRange(round(255.0 * (z - Min) / (Max - Min)));
               case zi of
                  0..42 : RainbowColorFunct := RGB(0,0,FirstColor + ColorStep * zi); {blues}
                  43..85 : RainbowColorFunct := RGB(0,FirstColor + ColorStep * (zi-43),FirstColor + ColorStep * (zi-43)); {Cyans}
                  86..128 : RainbowColorFunct := RGB(0,FirstColor + ColorStep * (zi-86),0); {greens}
                  129..171: RainbowColorFunct := RGB(FirstColor + ColorStep * (zi-129),FirstColor + ColorStep * (zi-129),0);  {yellows}
                  172..214 : RainbowColorFunct := RGB(FirstColor + ColorStep * (zi-172),0,0);  {reds}
                  215..255 : RainbowColorFunct := RGB(FirstColor + ColorStep * (zi-215),0,FirstColor + ColorStep * (zi-215));  {magentas}
                  else RainBowColorFunct := clBlack;
               end;
            end;
         end {function};


         function DefineColorArray(Palette : shortstring; var NumColors : integer; var ColorsTable : tZTableColors255; Reverse : boolean = false) : boolean;
         var
            ColorTable : tMyData;
            i : integer;
         begin
            NumColors := 0;
            if FileExists(ColorBrewerName) then begin
               ColorTable := TMyData.Create(ColorBrewerName);
               ColorTable.ApplyFilter('PALETTE=' + QuotedStr(Palette));
               if (ColorTable.RecordCount = 0) then begin
                  Palette := 'ColorBrewer Spectral-9';
                  ColorTable.ApplyFilter('PALETTE=' + QuotedStr(Palette));
               end;
               NumColors := ColorTable.RecordCount;
               if NumColors > 255 then NumColors := 255;

               for i := 1 to NumColors do begin
                  if (i in [1..255]) then begin
                     if Reverse then ColorsTable[NumColors - pred(i)] := ColorTable.PlatformColorFromTable
                     else ColorsTable[i] := ColorTable.PlatformColorFromTable;
                  end;
                  ColorTable.Next;
               end;
               ColorTable.Destroy;
            end;
            Result := (NumColors > 0);
         end;


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

         begin
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
               if (Range < 0.002) or (abs(Max) < 0.002) then Dec := 4
               else if (Range < 0.02) or (abs(Max) < 0.02) then Dec := 3
               else Dec := 2;

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
         end;


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
   Color : tColor;

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
      Left := 15;
      Top := 10;
      for i := 0 to pred(DEMsUsed.Count) do begin
         Color := StrToInt(ColorsUsed[i]);
         AnEntry(DEMsUsed.Strings[i],Color);
      end;
      PutBitmapInBox(Result);
      Dec(StartFontSize);
   until Result.Width < MaxWide;
end;



