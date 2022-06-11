

function SpectralLibraryGraph(fName : PathStr = ''; MaxVert : float = 100) : TThisBaseGraph;
var
   i,j       : integer;
   x,y       : float;
   Line      : AnsiString;
   DefFilt   : byte;
   Data, FilesWanted : tStringList;
   rfile : file;
   v     : array[1..2] of float;
begin
   Result := Nil;
   FilesWanted := tStringList.Create;
   if (fName = '') then begin
      FilesWanted.Add(MainMapData  + 'spectral_library\');
      DefFilt:= 0;
      if not GetMultipleFiles('reflectance spectra','Spectral libary|*.SPC;*.ASC',FilesWanted,DefFilt) then exit;
   end
   else FilesWanted.Add(FName);

   Result := tThisBaseGraph.Create(Application);
   Result.GraphDraw.MinHorizAxis := 200;
   Result.GraphDraw.MaxHorizAxis := 3000;
   Result.GraphDraw.MaxVertAxis := MaxVert;
   Result.GraphDraw.HorizLabel := 'Wavelength (microns)';
   Result.GraphDraw.VertLabel := 'Albedo (%)';
   Result.Caption := 'Reflectance Spectrum';
   Result.SetUpGraphForm;
   Result.GraphDraw.LegendList := tStringList.Create;
   for i := 0 to pred(FilesWanted.Count) do begin
      fName := FilesWanted.Strings[i];
      Data := tStringList.Create;
      Data.LoadFromFile(fName);
      {$IfDef RecordSpectralLibraryGraph}
      WriteLineToDebugFile('Spectral Library file: ' + fName,true);
      WriteStringListToDebugFile(Data);
      {$EndIf}

      fName := ExtractFileNameNoExt(FilesWanted.Strings[i]);
      Result.Caption := Result.Caption + '  ' + fName;
      Result.GraphDraw.LegendList.Add(fName);
      Result.OpenDataFile(rfile);
      for j := 16 to pred(Data.Count) do begin
         Line := ptTrim(Data.Strings[j]);
         x := StrToFloat(Petmar_types.BeforeSpecifiedCharacter(Line,' ',true,true));
         y := StrToFloat(Petmar_types.BeforeSpecifiedCharacter(Line,' ',true,true));
         if (abs(y) > 1e-30) then begin
            v[1] := 1000 * x;
            v[2] := 100 * y;
            BlockWrite(rfile,v,1);
         end;
      end;
      Data.Free;
      CloseFile(rfile);
   end;
   Result.RedrawDiagram11Click(Nil);
end;

procedure tSatImage.CreateAllScattergrams(ShowAllPoints : boolean; ShowClasses : integer = 0; StatSample : integer = 0);
var
   gr : BaseGraf.tThisBaseGraph;
   xband,yband : integer;
   Bitmap,bm2 : tMyBitmap;
begin
   {$IfDef RecordScattergramProblems}
   WriteLineToDebugFile('Enter tSatImage.CreateAllScattergrams: ' + SceneBaseName,true);
   WriteLineToDebugFile('Image size:' + IntToStr(NumSatCol) + ' x' + IntToStr(NumSatRow),true);
   {$EndIf}
   Bitmap := Nil;
   StartProgressAbortOption('Scattergram');
    for xband  := 1 to NumBands do if UpperCase(Copy(BandTitle[xband],1,7)) <> 'MISSING' then begin
      for Yband := xband to Numbands do if UpperCase(Copy(BandTitle[yband],1,7)) <> 'MISSING' then begin
         gr := BandScattergram(true,true,xband,yband,StatSample,ShowAllPoints,ShowClasses);
         if (Bitmap = Nil) then CreateBitmap(Bitmap,NumBands * gr.Image1.Width,NumBands * gr.Image1.Height);
         CopyImageToBitmap(gr.Image1,bm2);
         Bitmap.Canvas.Draw(Pred(xBand) *  bm2.Width,pred(YBand) * bm2.Height,bm2);
         if (xband <> yband) then Bitmap.Canvas.Draw(Pred(yBand) * bm2.Width,pred(xBand) * bm2.Height,bm2);
         gr.Close;
         bm2.Free;
         UpdateProgressBar((xBand*NumBands+Yband)/sqr(NumBands));
         if WantOut then break;
      end;
      if WantOut then break;
   end;
  EndProgress;
   Petimage_form.DisplayBitmap(Bitmap,'Classification categories');
   Bitmap.Free;
end;


function tSatImage.BandScattergram(ShowTrainingSets,ShowBoxes : boolean; xband : integer = 0; yband : integer = 0; StatSample : integer = 0; PlotPoints : boolean = true; ShowClasses : integer = 0) :  tThisBaseGraph;
label
   Bored;
type
   varray = array[0..MaxSatCols,1..3] of float;
var
   xlow,xhigh,ylow,yhigh : float;
   c,i,j,k             : integer;
   MRowVals1,MRowVals2,ClassVals : ^tImageRow;
   rfile               : file;
   v                   : ^varray;
   Colors : Petmar_types.tColors256;
begin
   {$IfDef RecordScattergramProblems}
   WriteLineToDebugFileWithTime('Enter Satellite scattergram: ' + SceneBaseName,true);
   WriteLineToDebugFileWithTime('Image size:' + IntToStr(NumSatCol) + ' x' + IntToStr(NumSatRow));
   {$EndIf}
   if (XBand = 0) then begin
      xBand := 1;
      PickBand('x axis',xBand);
   end;
   if (yBand = 0) then begin
      yBand := succ(xBand);
      repeat
         PickBand('y axis',yBand);
      until (yBand <> XBand);
   end;

   if (StatSample = 0) then begin
      StatSample := 1;
      while (NumSatCol div StatSample) * (NumSatRow div StatSample) > MDDef.MaxPtsBandHist do inc(StatSample);
      if MDDef.CanOverrideMaxPtsBandHist then ReadDefault('Row sample factor',StatSample);
   end;
   {$IfDef RecordScattergramProblems}
   WriteLineToDebugFile('   Bands: ' + IntToStr(xband) + ' and ' + IntToStr(yBand));
   {$EndIf}

   Result := TThisBaseGraph.Create(Application);
   with Result,GraphDraw do begin
      ClientWidth := MaxRefCount + LeftMargin - 10;
      ClientHeight := MaxRefCount + BottomMargin - 10;
      MaxHorizAxis := MaxRefCount;
      MaxVertAxis := MaxRefCount;
      HorizLabel := 'Band ' + IntToStr(xband);
      VertLabel := 'Band ' + IntToStr(yband);

      Caption := SceneBaseName + ' Scattergram Bands ' + IntToStr(XBand) + '/' + IntToStr(Yband);
      SetUpGraphForm;
      Symbol[1].Size := 1;
      Symbol[1].Color := clRed;
      Symbol[1].DrawingSymbol := FilledBox;
      LineSize256[1] := 0;

      New(MRowVals1);
      New(MRowVals2);

      if (ShowClasses > 0) then begin
         AssignClusterColors(ShowClasses,Colors);
         New(ClassVals);
      end;

      if PlotPoints then begin
         {$IfDef RecordScattergramProblems}
         WriteLineToDebugFile('Plot points');
         {$EndIf}
         OpenXYColorFile(rfile);
         New(v);
         i := 0;
         while (i <= pred(NumSatRow)) do begin
            GetSatRow(xBand,i,MRowVals1^);
            GetSatRow(yBand,i,MRowVals2^);
            if (ShowClasses > 0) then GetSatRow(ShowClasses,i,ClassVals^);
             j := 0;
             while j <= pred(NumSatCol) do begin
                k := j div StatSample;
                v^[k,1] := MRowVals1^[j];
                v^[k,2] := MRowVals2^[j];
                if ShowClasses > 0 then v^[k,3] := Colors[ClassVals^[j]]
                else v^[k,3] := clSilver;
                inc(j,StatSample);
             end;
             BlockWrite(rfile,v^,k);
            inc(i,StatSample);
         end {while};
         CloseFile(rfile);
         Dispose(v);
         {$IfDef RecordScattergramProblems}
         WriteLineToDebugFile('Plot points done');
         {$EndIf}
      end;

     Dispose(MRowVals1);
     Dispose(MRowVals2);
     if (ShowClasses > 0) then Dispose(ClassVals);

     {$IfDef RecordScattergramProblems}
     WriteLineToDebugFile('Call redraw diagram');
     {$EndIf}
     RedrawDiagram11Click(Nil);

     if ShowBoxes and (SelectionMap.NumClass > 0)  and (SelectionMap.Classes <> Nil) then begin
         {$IfDef RecordScattergramProblems}
         WriteLineToDebugFile('ShowBoxes, Numclass=' + IntToStr(NumClass));
         {$EndIf}
         for c := 1 to SelectionMap.NumClass do with SelectionMap.Classes^[c] do begin
            xlow := Mean[xband] - MDDef.SatTrainStdDev * StdDev[xband];
            xhigh := Mean[xband] + MDDef.SatTrainStdDev * StdDev[xband];
            ylow := Mean[yband] - MDDef.SatTrainStdDev * StdDev[yband];
            yhigh := Mean[yband] + MDDef.SatTrainStdDev * StdDev[yband];
            Result.Image1.Canvas.Pen.Color := ClassColor;
            Result.Image1.Canvas.Brush.Style := bsClear;
            result.Image1.Canvas.Rectangle(GraphDraw.GraphX(xlow),GraphDraw.GraphY(yHigh),GraphDraw.GraphX(xhigh),GraphDraw.GraphY(yLow));
         end;
         {$IfDef RecordScattergramProblems}
         WriteLineToDebugFile('end ShowBoxes');
         {$EndIf}
      end;
   end {with};
   {$IfDef RecordScattergramProblems}
   WriteLineToDebugFile('Exit Satellite scattergram');
   {$EndIf}
end;


procedure tSatImage.ShowPreview(Image1 : tImage; Multicolor : boolean; Gray,Red,Green,Blue : integer);
var
   XoffsetinWindow,YOffsetInWindow,ColsDisplayed,RowsDisplayed : integer;
   Bitmap : tMyBitmap;
begin
     if (DefinedImageColors <> Nil) then Dispose(DefinedImageColors);
     DefinedImageColors := Nil;
     CreateBitmap(Bitmap,128,128);
     XoffsetinWindow := (NumSatCol div 2) - 64;
     YOffsetInWindow := (NumSatRow div 2) - 64;
     ColsDisplayed := 128;
     RowsDisplayed := 128;
     DisplayImage(Nil,Bitmap,Multicolor,MDdef.ContrastEnhancement,MDdef.TailSize,
        Gray,Red,Green,Blue,XoffsetinWindow,YOffsetInWindow,ColsDisplayed,RowsDisplayed);
     Image1.Picture.Graphic := Bitmap;
     Bitmap.Free;
     ShowDefaultCursor;
end;


procedure tSatImage.GraphHistogram(CombinedGraph : boolean; WhichBand : integer; ClustersBand : integer = 0);
var
   i,j,Band,MaxCount : integer;
   rfile      : file;
   ThisGraph  : TThisBaseGraph;
   v          : array[1..2] of float;

   procedure MakeHistogram(Band : integer);
   var
      fName1 : PathStr;
      GIS : TGISdataBaseModule;
   begin
      fName1 := HistogramLandsatName(TiffImage[Band].TIFFFileName);
      if FileExists(fName1) then begin
         GIS := TGISdataBaseModule.Create(Application);
         if GIS.InitializeTheTable('',fName1) then begin
            GIS.CreateScatterGram('DN','BAND_' + IntToStr(Band));
         end;
         GIS.Destroy;
      end;
   end;


begin
   {$IfDef RecordHistogramProblems}
   WriteLineToDebugFile('tSatImage.GraphHistogram enter ' + OriginalFileName,true);
   {$EndIf}

   if (LandsatNumber > 0) then begin
      if CombinedGraph then begin
         for Band := 1 to AvailableBands do MakeHistogram(Band);
      end
      else begin
         MakeHistogram(WhichBand);
      end;
   end
   else begin
      MaxCount := 0;
      if not RealHistogram then LoadHistogram;
      for i := 1 to AvailableBands do begin
         if (Distrib[i] = Nil) then begin
            {$IfDef RecordHistogramProblems}
            WriteLineToDebugFile('No distribution in memory');
            {$EndIf}
            exit;
         end
         else begin
            for j := 0 to MaxRefCount do if Distrib[i]^[j] > MaxCount then
               MaxCount := Distrib[i]^[j];
         end;
      end;

      {$IfDef RecordHistogramProblems}
      WriteLineToDebugFile('MaxCount found');
      {$EndIf}

      ThisGraph := TThisBaseGraph.Create(Application);
      ThisGraph.ClientWidth := 450;
      ThisGraph.ClientHeight := 300;

      with ThisGraph,GraphDraw do begin
         if CombinedGraph then begin
            HorizLabel := 'Band Reflectance';
            ThisGraph.Caption := SceneTitle + ' Reflectance Histogram';
         end
         else begin
            HorizLabel := BandTitle[WhichBand] + ' Reflectance';
            ThisGraph.Caption := BandTitle[WhichBand] + ' Histogram';
         end;
         VertLabel := '% of Points';
         ThisGraph.SetUpGraphForm;

           if CombinedGraph then begin
              for Band := 1 to AvailableBands do if (Distrib[Band] <> Nil) then begin
                 ThisGraph.OpenDataFile(rfile);
                 for j := 0 to MaxRefCount do begin
                    if Distrib[Band]^[j] > 0 then begin
                       v[1] := j;
                       v[2] := 100.0 * Distrib[Band]^[j] / NumPoints;
                       BlockWrite(rfile,v,1);
                    end;
                 end;
                 CloseFile(rfile);
              end;
              FileColors256[1] := clBlue;
              FileColors256[2] := clLime;
              FileColors256[3] := clRed;
              FileColors256[4] := clFuchsia;
              FileColors256[5] := clMaroon;
              FileColors256[6] := clOlive;
           end
           else begin
              ThisGraph.OpenDataFile(rfile);
              FileColors256[1] := clRed;
              for j := 0 to MaxRefCount do begin
                 if Distrib[WhichBand]^[j] > 0 then begin
                    v[1] := j;
                    v[2] := 100.0 * Distrib[WhichBand]^[j] / NumPoints;
                    BlockWrite(rfile,v,1);
                 end;
              end;
              CloseFile(rfile);
           end;
         {$IfDef RecordHistogramProblems}
         WriteLineToDebugFile('Graph set up');
         {$EndIf}
         MaxHorizAxis := MaxRefCount;
         MaxVertAxis := 100.0 * MaxCount / NumPoints;
         PadAxis(MinVertAxis,MaxVertAxis);
         ForceAxisFit(HorizAxisFunctionType,HorizCycleCuts,NumHorizCycles,MinHorizAxis,MaxHorizAxis,XSize,50);
         ForceAxisFit(VertAxisFunctionType,VertCycleCuts,NumVertCycles,MinVertAxis,MaxVertAxis,YSize,25);
         RedrawDiagram11Click(Nil);
      end {with};
   end;
   {$IfDef RecordHistogramProblems}
   WriteLineToDebugFile('tSatImage.GraphHistogram out');
   {$EndIf}
end;

procedure tSatImage.SatHistograms(xlo,ylo,xhi,yhi : integer);
var
   RowVals  : ^tImageRow;
   i,j,k    : integer;
begin
   {$IfDef RecordSatProblems}
   WriteLineToDebugFile('tSatImage.SatHistograms in');
   {$EndIf}
   if not RealHistogram then LoadHistogram;

   for i := 1 to NumBands do begin
      New(RowVals);
      {$IfDef RecordSatProblems}
      WriteLineToDebugFile('Histogram band: ' + IntToStr(i));
      {$EndIf}
      StartProgress('Histogram ' + IntToStr(i));
      if (NumXTiles = 1) and (NumYTiles = 1) then begin
         FillChar(Distrib[i]^,SizeOf(Distrib[i]^),0);
         for j := ylo to yhi do begin
            GetSatRow(i,j,RowVals^);
            for k := xlo to xhi do inc(Distrib[i]^[RowVals^[k]]);
            if (j mod 50 = 0) then UpDateProgressBar(j/(yhi-ylo));
         end;
         SetHistogramLimits;
      end;
     EndProgress;
      Dispose(RowVals);
   end;
end;


procedure tSatImage.InterpolateColorImage(Image : TImage; WindowContrast : tContrastEnhancement;
        WindowContrastTailSize,Red,Green,Blue : integer; xul,yul,xlr,ylr,xinc,yinc : float);
var
   ImageXSize,ImageYSize,
   xp,yp,
   xs,ys, i,j : integer;
   ColorLookUp               : tColorLookUp;
   Grays                     : TRGBLookUp;
   P0 : PRGB;
   yc,xc : float;
   Rows : array[1..3,1..2] of ^tImageRow;
   Bitmap : tMyBitmap;


    function InterpolateColor(Color : integer; xgrid,ygrid : float) : integer;
    var
       xincr,yincr : float;
       x : integer;
    begin
       xgrid := xgrid;
       ygrid := ygrid;
       x := trunc(Xgrid);
       YIncr := frac(YGrid);
       XIncr := frac(XGrid);
       result := ValidByteRange(round(  (1-XIncr) * (1-YIncr) * Rows[Color,1]^[x]
                    +    (1-XIncr) *    YIncr  * Rows[Color,2]^[x]
                    +       XIncr  * (1-YIncr) * Rows[Color,1]^[succ(x)]
                    +       XIncr  *    YIncr  * Rows[Color,2]^[succ(x)]));
    end;


begin
   {$IfDef RecordSatInterpolateColorProblems}
   WriteLineToDebugFile('tSatImage.InterpolateColorImage');
   {$EndIf}

   CopyImageToBitmap(Image,BitMap);

   ImageXSize := Bitmap.Width;
   ImageYSize := Bitmap.Height;

   SetUpColors(true,WindowContrast,WindowContrastTailSize,1,Red,Green,Blue,Grays,ColorLookUp);
   for i := 1 to 3 do
      for j := 1 to 2 do
         New(Rows[i,j]);

   xs := trunc(xul);
   ys := trunc(yul);

   GetSatRow(Red,ys,Rows[1,1]^);
   GetSatRow(Green,ys,Rows[2,1]^);
   GetSatRow(Blue,ys,Rows[3,1]^);

   GetSatRow(Red,succ(ys),Rows[1,2]^);
   GetSatRow(Green,succ(ys),Rows[2,2]^);
   GetSatRow(Blue,succ(ys),Rows[3,2]^);

   StartProgress('Interpolate');
   for yp := 0 to pred(ImageYSize) do begin
      if (yp mod 25 = 0) then begin
         Image.Picture.Graphic := Bitmap;
         UpdateprogressBar(yp/ImageYSize);
      end;

      P0 := BitMap.ScanLine[yp];
      yc := yul + yp * yinc + 0.00001;
      if (yc > ys + 1) then begin
         for i := 1 to 3 do Rows[i,1]^ := Rows[i,2]^;
         inc(ys);
         GetSatRow(Red,succ(ys),Rows[1,2]^);
         GetSatRow(Green,succ(ys),Rows[2,2]^);
         GetSatRow(Blue,succ(ys),Rows[3,2]^);
      end;

      for xp := 0 to pred(ImageXSize) do begin
         xc := xul + xp * xinc + 0.00001;
         P0[xp].rgbtRed := ColorLookUp[1]^[InterpolateColor(1,xc,yc)];
         P0[xp].rgbtGreen := ColorLookUp[2]^[InterpolateColor(2,xc,yc)];
         P0[xp].rgbtBlue := ColorLookUp[3]^[InterpolateColor(3,xc,yc)];
      end {for xp};
   end {for yp};

   for i := 1 to 3 do
      for j := 1 to 2 do
         Dispose(Rows[i,j]);
   Image.Picture.Graphic := Bitmap;
   Bitmap.Free;
  EndProgress;
end;


procedure tSatImage.VerifyImagePosition(Mess : Ansistring; var xg,yg : float);
var
   lat,Long : float;
begin
   SatGridToLatLongDegree(xg,yg,Lat,Long);
   GetLatLn.GetLatLongDefault(RegVars.SatDatumConstants,Mess,Lat,Long);
   LatLongDegreeToSatGrid(Lat,Long,xg,yg);
end;


procedure tSatImage.OptimumIndexFactor;
var
   i,j,k,n : integer;
   Correlations,VarCoVar   : ^tTrendMatrix;
   Mean,Min,Max,StdDev : tMaxBandsArray;
   Results : tStringList;
   fName : PathStr;
   OIF : float;
begin
   New(Correlations);
   New(VarCoVar);
   SatSceneStatistics(n,Correlations^,VarCoVar^,Mean,Min,Max,StdDev);
   Results := tStringList.Create;
   Results.Sorted := true;
   for i := 1 to (NumBands - 2) do begin
      for j := succ(i) to (NumBands - 1) do
         for k := succ(j) to NumBands do if (i <> j) and (j <> k) and (i <> k) then begin
            OIF := (StdDev[i] + StdDev[j] + StdDev[k]) / ( abs(Correlations^[i,j]) + abs(Correlations^[i,k]) + abs(Correlations^[j,k]));
            Results.Add(RealToString(OIF,12,3) + ',' + ShortBandName[i] + ',' + ShortBandName[j] + ',' + ShortBandName[k]);
         end;
   end;
   fName := MDTempDir + 'oif.csv';
   Results.Sorted := false;
   Results.Insert(0,'OIF,BAND_1,BAND_2,BAND_3');
   Results.SaveToFile(fName);
   Results.Free;
   OpenDataBase('',fName);
   Dispose(Correlations);
   Dispose(VarCoVar);
end;

(*
procedure tSatImage.SetThreeBandColors(BaseSat : integer; MulticolorAllowed : boolean; var Multicolor : boolean;
    var SelectBand,RedBand,GreenBand,BlueBand : integer; MapOn : tMapForm = Nil);
{$IfDef ExSats}
begin
{$Else}
var
   PickThreeBandForm : TPickThreeBandForm;
   i : integer;
   OK : boolean;
begin
   PickThreeBandForm := TPickThreeBandForm.Create(Application);
   with PickThreeBandForm do begin
      Caption := 'Bands for ' + SceneBaseName;
      Multiband := Multicolor;
      BaseMap := MapOn;
      Sat := BaseSat;
      UpDown1.Max := AvailableBands;
      UpDown1.Position := SelectBand;
      for i := 1 to AvailableBands do begin
         ComboBox1.Items.Add(BandTitle[i]);
         ComboBox2.Items.Add(BandTitle[i]);
         ComboBox3.Items.Add(BandTitle[i]);
         ComboBox4.Items.Add(BandTitle[i]);
      end;
      UpDown1.Max := ComboBox4.Items.Count;
      if MulticolorAllowed then begin
         ComboBox1.Text := BandTitle[RedBand];
         ComboBox2.Text := BandTitle[GreenBand];
         ComboBox3.Text := BandTitle[BlueBand];
      end;
      ComboBox4.Text := BandTitle[SelectBand];
      RadioGroup1.Enabled := MulticolorAllowed;
      BitBtn1.Enabled := MulticolorAllowed;
      BitBtn2.Enabled := MulticolorAllowed;
      if Multiband then RadioGroup1.ItemIndex := 1 else RadioGroup1.ItemIndex := 0;
      FormSetUp := true;
      ColorPreview;
      ToggleBands;
      repeat
         PickThreeBandForm.ShowModal;
         for i := 1 to AvailableBands do if UpperCase(Copy(BandTitle[i],1,7)) <> 'MISSING' then begin
            if ComboBox1.Text = BandTitle[i] then RedBand := i;
            if ComboBox2.Text = BandTitle[i] then GreenBand := i;
            if ComboBox3.Text = BandTitle[i] then BlueBand := i;
            if ComboBox4.Text = BandTitle[i] then SelectBand := i;
         end;
         OK := ((RedBand <> GreenBand) and (RedBand <> BlueBand) and (BlueBand <> GreenBand)) or
                     AnswerIsYes('Same band multiple colors');
      until OK;
      MultiColor := Multiband;
   end;
   PickThreeBandForm.Free;
{$EndIf}
end;
*)


procedure tSatImage.PickBand(Message : ShortString; var WantedBand : integer);
var
   i : integer;
   OK : boolean;
   TheList : TStringList;
begin
   if (NumBands = 1) then WantedBand := 1
   else begin
      if (WantedBand < 1) then WantedBand := 1;
      if (WantedBand > AvailableBands) then WantedBand := AvailableBands;
      TheList := TStringList.Create;
      for i := 1 to AvailableBands do TheList.Add(BandTitle[i]);
      repeat
         GetFromList('Band for ' + Message,WantedBand,TheList);
         Inc(WantedBand);
         OK := UpperCase(Copy(BandTitle[WantedBand],1,7)) <> 'MISSING';
      until OK;
      TheList.Free;
   end;
end;

procedure tSatImage.PickMultipleBands(Message : ShortString; var UseBands : tUseBands);
var
   i : integer;
   TheList : TStringList;
begin
   TheList := TStringList.Create;
   for i := 1 to AvailableBands do if (UpperCase(Copy(BandTitle[i],1,7)) <> 'MISSING') then TheList.Add(BandTitle[i]);
   {$IfDef RecordTM7SatProblems}
   WriteLineToDebugFile('tSatImage.PickMultipleBands full list',true);
   WriteStringListToDebugFile(TheList,true);
   {$EndIf}
   Toggle_db_use.PickSomeFromStringList(TheList,Message);
   {$IfDef RecordTM7SatProblems}
   WriteLineToDebugFile('tSatImage.PickMultipleBands picked files',true);
   WriteStringListToDebugFile(TheList,true);
   {$EndIf}
   for i := 1 to MaxBands do UseBands[i] := false;
   for i := 1 to AvailableBands do begin
      UseBands[i] := TheList.IndexOf(BandTitle[i]) <> -1;
   end;
   TheList.Free;
end;


function tSatImage.MakeNewBand(NewBand : tNewSatBand; OpenMap : boolean = true) : integer;
var
   RatMult,
   i,j,ThirdBand,NumBand,DenBand  : integer;
   Ratio,FirstFactor,SecondFactor          : float;
   ThirdVals,DenVals,NumVals : ^tImageRow;
   RatVal : byte;
   GraysLookUp: tColorIndex;

      procedure FreeBandMemory;
      begin
         Dispose(NumVals);
         if (DenBand <> 0) then Dispose(DenVals);
         if (ThirdBand <> 0) then Dispose(ThirdVals);
      end;

      function RatName : ANSIstring;
      begin
         if True then RatName := ''
         else Result := ', (' + ShortBandName[NumBand] + '-' + ShortBandName[DenBand] + ' / (' + ShortBandName[NumBand] + '+' + ShortBandName[DenBand] + ')';
      end;

begin
   Result := 0;
   if not RealHistogram then LoadHistogram;
   NumBand := 1;
   DenBand := 2;
   ThirdBand := 0;
   inc(AvailableBands);
   if (NewBand = nsbAddimages) then begin
      FirstFactor := 0.5;
      SecondFactor := 0.5;
      PickBand('first band',NumBand);
      repeat
         ReadDefault('first band factor',FirstFactor);
      until abs(FirstFactor) > 0.01;
      PickBand('second band',DenBand);
      repeat
         ReadDefault('second band factor',SecondFactor);
      until abs(SecondFactor) > 0.01;
      BandTitle[AvailableBands] := 'Sum ' + ShortBandName[NumBand] + ' and ' + ShortBandName[DenBand];
   end
   else if (NewBand = nsbBandNegative) then begin
      PickBand('band for negative',NumBand);
      DenBand := 0;
      BandTitle[AvailableBands] := 'Negative' + ShortBandName[NumBand];
   end
   else if (NewBand = nsbMaxDiff) then begin
      ThirdBand := 3;
      BandTitle[AvailableBands] := 'Band max difference';
   end
   else if (NewBand = nsbNDVI) then begin
      if (LandsatNumber in [4,5,6,7]) or (StrUtils.AnsiContainsText(IndexFileName,'NAIP') and (NumBands = 4)) then begin
         NumBand := 4;
         DenBand := 3;
      end
      else if (LandsatNumber = 8) then begin
         NumBand := 5;
         DenBand := 4;
      end
      else if WV2Image then begin
         NumBand := 8;
         DenBand := 5
      end
      else begin
         PickBand('IR band',NumBand);
         PickBand('red band',DenBand);
      end;
      BandTitle[AvailableBands] := 'NDVI' + RatName;
   end
   else if (NewBand = nsbPickEm) then begin
      PickBand('band 1 for (b1-b2)/(b1+b2)',NumBand);
      PickBand('band 2 for (b1-b2)/(b1+b2)',DenBand);
      BandTitle[AvailableBands] :=  RatName;
   end
   else if (NewBand = nsbNDSI) then begin
      if (LandsatNumber in [4,5,6,7]) then begin
         NumBand := 5;
         DenBand := 4;
      end
      else if (LandsatNumber in [8]) then begin
         NumBand := 6;
         DenBand := 5;
      end;
      BandTitle[AvailableBands] := 'NDSI (soil)' + RatName;
   end
   else if (NewBand = nsbNDSIsnow) then begin  //Dozier, 1989, Remote Sensing Environment
      if (LandsatNumber in [4,5,6,7]) then begin
         NumBand := 2;
         DenBand := 5;
      end
      else if (LandsatNumber in [8]) then begin
         NumBand := 3;
         DenBand := 6;
      end;
      BandTitle[AvailableBands] := 'NDSI (snow)' + RatName;
   end
   else if (NewBand = nsbNDWI) then begin
      if (LandsatNumber in [4,5,6,7]) then begin
         NumBand := 3;  //red
         DenBand := 5;   //NIR
      end
      else if (LandsatNumber in [8]) then begin
         NumBand := 4;    //red
         DenBand := 6;    //IR-2
      end
      else if WV2Image then begin
         NumBand := 5;    //red
         DenBand := 8;    //IR-2
      end
      else begin
         PickBand('red band',NumBand);
         PickBand('IR band',DenBand);
      end;
      BandTitle[AvailableBands] := 'NDWI' + RatName;
   end
   else if (NewBand = nsbContrastStretch) then begin
      PickBand('band for contrast stretch',NumBand);
      BandTitle[AvailableBands] := 'Contrast stretch  ' + ShortBandName[NumBand];
      DEMDef_routines.SaveBackupDefaults;
      {$IfDef VCL}
      DEMSatContrast.GetContrast(Nil,true,0,false,NumBand,NumBand,NumBand,NumBand);
      {$EndIf}
      SetByteLookUpTable(MDDef.ContrastEnhancement,MDDef.TailSize,MinRef[NumBand],MaxRef[NumBand],Distrib[NumBand]^,GraysLookUp);
      DEMDef_routines.RestoreBackupDefaults;
   end
   else if (NewBand = nsbNBRNormalizedburnindex) then begin
      NumBand := 4;
      FindTMBand(7,DenBand);
      BandTitle[AvailableBands] := 'NBR,  ' + ShortBandName[NumBand] + ' to ' + ShortBandName[DenBand];
   end
   else if (NewBand = nsbVARI) or (NewBand = nsbGrayscale) then begin
      if (LandsatNumber in [4,5,6,7]) then begin
         NumBand := 2;    //green
         DenBand := 1;    //blue
         ThirdBand := 3;  //red
      end
      else if (LandsatNumber in [8]) then begin
         NumBand := 3;    //green
         DenBand := 2;    //blue
         ThirdBand := 4;  //red
      end
      else if WV2Image then begin
         NumBand := 3;    //green
         DenBand := 2;    //blue
         ThirdBand := 5;  //red
      end
      else begin
         PickBand('red band',ThirdBand);
         PickBand('green band',NumBand);
         PickBand('blue band',DenBand);
      end;
      if (NewBand = nsbVARI) then BandTitle[AvailableBands] := 'VARI'
      else BandTitle[AvailableBands] := 'RGB grayscale';
   end
   else if (NewBand = nsbDifference) then begin
      PickBand('first band',NumBand);
      PickBand('second band',DenBand);
      BandTitle[AvailableBands] := 'Norm diff,  ' + ShortBandName[NumBand] + ' to ' + ShortBandName[DenBand];
   end
   else  if (NewBand = nsbRatio) then begin
      PickBand('ratio numerator',NumBand);
      PickBand('ratio denominator',DenBand);
      BandTitle[AvailableBands] := 'Ratio ' + ShortBandName[NumBand] + ' over ' + ShortBandName[DenBand];;
   end;

   Read_DEM.OpenDEMDataStructures(Result);
   RatMult := 1;
   if MDDef.ByteRatios then TiffImage[1].InitializeDEM(DEMGlb[Result],true,ByteDEM)
   else begin
      TiffImage[1].InitializeDEM(DEMGlb[Result],true,FloatingPointDEM);
      if MDDef.BandRatiosX100 then RatMult := 100;
   end;
   DEMGlb[Result].HeadRecs.ElevUnits := Undefined;

   New(NumVals);
   if (DenBand <> 0) then New(DenVals);
   if (ThirdBand <> 0) then New(ThirdVals);

   StartProgress('Calculate');
   for i := 0 to pred(NumSatRow) do begin
      if (i mod 10 = 0) then UpdateProgressBar( i / NumSatRow);
      GetSatRow(NumBand,i,NumVals^);
      if (DenBand <> 0) then GetSatRow(DenBand,i,DenVals^);
      if (ThirdBand <> 0) then GetSatRow(ThirdBand,i,ThirdVals^);
      for j := 0 to pred(NumSatCol) do begin

         if MDdef.IgnoreHistogramZero and (NumVals^[j] = 0) then RatVal := 0
         else begin
           if (NewBand = nsbPickEm) or (NewBand = nsbDifference) or (NewBand = nsbNDVI)  or (NewBand = nsbNDSI) or (NewBand = nsbNDSIsnow) or (NewBand = nsbNDWI) or (NewBand = nsbVARI) then begin
              if NumVals^[j] + DenVals^[j] = 0 then RatVal := 0
              else begin
                 Ratio := (NumVals^[j] - DenVals^[j]) / (NumVals^[j] + DenVals^[j]);
                 if MDDef.ByteRatios then Ratio := ValidByteRange(128 + round(256*Ratio));
              end;
           end
           else if (NewBand = nsbVARI) then begin
              Ratio := (NumVals^[j] - ThirdVals^[j]) / (NumVals^[j] + ThirdVals^[j] - DenVals^[j]);
           end
           else if (NewBand = nsbGrayscale) then begin
              Ratio := GreenMultGrayscale * NumVals^[j] + RedMultGrayscale * ThirdVals^[j] + BlueMultGrayscale * DenVals^[j];
           end
           else if (NewBand = nsbAddimages) then begin
              Ratio := FirstFactor * NumVals^[j] + SecondFactor * DenVals^[j];
           end
           else if (NewBand = nsbBandNegative) then begin
              Ratio := 255 - NumVals^[j];
           end
           else if (NewBand = nsbRatio) then begin
              if DenVals^[j] = 0 then Ratio := 1.571
              else Ratio := ArcTan(NumVals^[j] / DenVals^[j]);
              if MDDef.ByteRatios then Ratio := round(162.3*Ratio);
           end
           else if (NewBand = nsbContrastStretch) then begin
              Ratio := GraysLookUp[NumVals^[j]];
           end
           else if (NewBand = nsbMaxDiff) then begin
              FirstFactor := Petmath.MaxFloat(NumVals^[j],DenVals^[j],ThirdVals^[j]);
              SecondFactor := Petmath.MinFloat(NumVals^[j],DenVals^[j],ThirdVals^[j]);
              Ratio := FirstFactor-SecondFactor;
           end;
           if MDDef.ByteRatios then begin
              Ratio := ValidByteRange(round(Ratio));
              if (Ratio < 1) then Ratio := 1;
              if (Ratio > 254) then Ratio := 254;
           end;
           DEMGlb[Result].SetGridElevation(j,DEMGlb[Result].Headrecs.NumRow-i,RatMult * Ratio);
         end;
      end {for j};

      {$IfDef RecordBandModProblems}
      WriteLineToDebugFile('   ' +  IntegerToString(RatVals^[1],5) +
           IntegerToString(RatVals^[2],5) + IntegerToString(RatVals^[3],5) + IntegerToString(NumVals^[4],5) );
      {$EndIf}
   end {for i};
   FreeBandMemory;
   EndProgress;

   DEMGlb[Result].AreaName := BandTitle[AvailableBands] + ' ' + SceneBaseName;
   if (LandsatNumber in [4..8]) then DEMGlb[Result].AreaName := BandTitle[AvailableBands] + '_' + ShortLandsatName(SceneBaseName);

   DEMGlb[Result].CheckMaxMinElev;
   {$IfDef VCL}
   if OpenMap then begin
      SetUpDEMSelectionMap(Result,'',false);
      DEMGlb[Result].SelectionMap.MapDraw.MapType := mtElevGray;
      DEMGlb[Result].SelectionMap.DoFastMapRedraw;
   end;
   {$EndIf}
   Dec(AvailableBands);
end;






