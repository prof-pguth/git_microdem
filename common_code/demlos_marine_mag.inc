
{$IfDef ExMagAnom}
{$Else}

procedure TDEMLOSF.StartMagModel;
begin
   {$IfDef RecordMGTProblems} WriteLineToDebugFile('TDEMLOSF.StartMagModel in'); {$EndIf}
   {$IfDef ExGeology}
   {$Else}
    LOSdraw.RidgeLoc := 0.001 * LOSdraw.FormSectLenMeters * 0.5;
    TimeLeft := LOSdraw.RidgeLoc / SpreadRidge;
    TimeRight := (LOSdraw.RidgeLoc - 0.001 * LOSdraw.FormSectLenMeters) / SpreadRidge;
    if (PickMagProfVars = Nil) then begin
       {$IfDef RecordMGTProblems}  WriteLineToDebugFile('TPickMagProfVars.Create'); {$EndIf}
       PickMagProfVars := TPickMagProfVars.Create(Application);
       PickMagProfVars.RadioGroup1.ItemIndex := pred(MDdef.MagLineWidth);

       PickMagProfVars.LOSForm := Self;
       PickMagProfVars.ShowValues;
       PickMagProfVars.Show;
       PickMagProfVars.Label8.Caption := 'Profile length: ' + RealToString(LOSdraw.FormSectLenMeters * 0.001,-12,-2) + ' km';
       {$IfDef RecordMGTProblems} WriteLineToDebugFile('Done TPickMagProfVars.Create'); {$EndIf}
    end;
    {$EndIf}
   {$IfDef RecordMGTProblems} WriteLineToDebugFile('TDEMLOSF.StartMagModel out'); {$EndIf}
end;


procedure TDEMLOSF.CreateMarineMagneticAnomaliesBitmap(var Bitmap : tMyBitmap; Wide,High : integer);
var
   Time,m,r3,x,z,dB,dH,dZ : float64;
   i,jTime,LastY : integer;
   LabStr : ANSIString;
begin
   {$IfDef RecordMGTProblems}
      WriteLineToDebugFile('TDEMLOSF.CreateMarineMagneticAnomaliesBitmap in');
      WriteLineToDebugFile('Bitmap size: ' + IntToStr(wide) + 'x' + IntToStr(High));
      WriteLineToDebugFile('Time: ' + RealToString(TimeLeft,-12,-2) + ' to ' + RealToString(TimeRight,-12,-2) + ' Ma');
   {$EndIf}
   CreateBitmap(Bitmap,Wide,High);
   Bitmap.Canvas.Pen.Width := MDdef.MagLineWidth;
   for High := LOSdraw.StartLosLeft to LOSdraw.StartLOSLeft+LOSdraw.PixLong do begin
      Time := TimeLeft + (TimeRight-TimeLeft) * (High-LOSdraw.StartLOSLeft) / LOSdraw.PixLong;
      dB := 0;
      for jTime := -50 to +50 do begin
         if GeologicTimeScale.NormalMagneticTime(Time + 0.01 * jTime) then m := BlockIntensity
         else m := -BlockIntensity;
         z := 2.5 + 0.3 * sqrt(abs(Time));
         if (z > 5.5) then z := 5.5;
         x := 0.001 * jTime * LOSdraw.FormSectLenMeters / LOSdraw.PixLong;
         r3 := sqrt(sqr(x) + sqr(z));
         r3 := Math.Power(r3,3);
         dH :=  m * x / r3;
         dZ := -m * z / r3;
         dB := dB + (dZ * SinDeg(MagI) + dH * cosDeg(MagI) * cosDeg(MagAlpha));
         if (jTime = 0) and (MagTimeScale or MagAnomNames) and (m > 0) then begin
            {$IfDef RecordMGTProblems} WriteLineToDebugFile(IntToStr(High) + '  ' + RealToString(Time,-12,2)); {$EndIf}
            DrawLine(Bitmap,High,Bitmap.Height-38,High,Bitmap.Height-28);
         end;
      end;
      if (High > LOSdraw.StartLOSLeft) then begin
         DrawLine(Bitmap,High,round(dB + Bitmap.Height div 2),pred(High),LastY);
      end;
      Lasty := round(db + Bitmap.Height div 2);
   end;

   {$IfDef RecordMGTProblems} WriteLineToDebugFile('Now for the time scale'); {$EndIf}

   if MagTimeScale then begin
      jTime := Round(TimeLeft);
      Bitmap.Canvas.Font.Size := 14;
      Bitmap.Canvas.Font.Style := [fsBold];
      repeat
         High := round((jTime - TimeLeft) / (TimeRight - TimeLeft) * LOSdraw.PixLong) + LOSdraw.StartLOSLeft;
         if (High >= 0) and (High <= LOSdraw.PixLong+LOSdraw.StartLOSLeft) then begin
            {$IfDef RecordMGTProblems} WriteLineToDebugFile(IntToStr(High) + '  ' + IntToStr(JTime)); {$EndIf}
            Bitmap.Canvas.MoveTo(High,Bitmap.Height-28);
            Bitmap.Canvas.LineTo(High,Bitmap.Height-23);
            Bitmap.Canvas.TextOut(High,Bitmap.Height-21,IntToStr(abs(JTime)));
         end;
         if (TimeLeft > TimeRight) then dec(JTime) else inc(JTime);
      until (High > LOSdraw.PixLong);
      Bitmap.Canvas.TextOut(LOSdraw.PixLong+LOSdraw.StartLOSLeft,Bitmap.Height-21,'Ma');
   end;

   if MagAnomNames then begin
      Bitmap.Canvas.Font.Size := 14;
      for i := 1 to MaxChrons do begin
         if (i=0) then Time := 0
         else Time := (StartTime[i] + EndTime[i]) * 0.5;
         LabStr := ChronName[i];
         LabStr := Petmar_Types.BeforeSpecifiedCharacterANSI(LabStr,'n',false,false);
         if (not StrUtils.AnsiContainsText(LabStr,'r'))  then begin
            High := round((Time - TimeLeft) / (TimeRight - TimeLeft) * LOSdraw.PixLong) + LOSdraw.StartLOSLeft;
            if (High > (LOSdraw.PixLong + LOSdraw.StartLOSLeft)) and (High < 0) then break;
            Bitmap.Canvas.TextOut(High-Bitmap.Canvas.TextWidth(LabStr) div 2,Bitmap.Height-65,LabStr);
            High := round((-Time - TimeLeft) / (TimeRight - TimeLeft) * LOSdraw.PixLong) + LOSdraw.StartLOSLeft;
            Bitmap.Canvas.TextOut(High-Bitmap.Canvas.TextWidth(LabStr) div 2,Bitmap.Height-65,LabStr);
         end;
      end;
   end;

   {$IfDef RecordMGTProblems}
      Bitmap.SaveToFile(NextFileNumber(MDTempDir, 'mag_model_',OverlayFExt));
      WriteLineToDebugFile('TDEMLOSF.CreateMarineMagneticAnomaliesBitmap out');
   {$EndIf}
end;

{$EndIf}



procedure TDEMLOSF.MagneticModel1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      LOSdraw.LOSVariety := losSimpleMagModel;
      FormResize(Nil);
      StartMagModel;
      MarineMagneticAnomalies;
   {$EndIf}
end;


procedure TDEMLOSF.MarineMagneticAnomalies;
{$IfDef ExMagAnom}
begin
{$Else}
label
   NewProfile;
var
   j,DivFactor : integer;
   Bitmap       : tMyBitmap;
begin {proc MarineMagneticAnomalies}
   {$IfDef RecordMGTProblems} WriteLineToDebugFile('TDEMLOSF.MarineMagneticAnomalies in'); {$EndIf}

   ShowHourglassCursor;
   if (LOSdraw.LOSVariety = losSimpleMagModel) then DivFactor := 2 else DivFactor := 3;
   j := (ClientHeight div DivFactor);
   if (j > 50) then dec(j,50);
   CreateMarineMagneticAnomaliesBitmap(Bitmap,ClientWidth,j);
   Image1.Picture.Graphic := Bitmap;

   Bitmap.Free;
   ShowDefaultCursor;
   Caption := 'Bathymetry, Magnetics, and Model for ' + RealToString(SpreadRidge,6,1) + ' mm/yr';
   {$IfDef RecordMGTProblems} WriteLineToDebugFile('TDEMLOSF.MarineMagneticAnomalies out');  {$EndIf}
{$EndIf}
end {proc MarineMagneticAnomalies};

