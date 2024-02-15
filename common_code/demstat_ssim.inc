{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for demstat        }
{___________________________________}


procedure NormalizeDEMforSSIM(DEM : integer; What : shortstring);
var
   Min,Max : float32;
begin
   if ValidDEM(DEM) then begin
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM in for ' + DEMGlb[DEM].AreaName); {$EndIf};
      What := UpperCase(What);
      if (What = 'ELEV') then begin
         Min := -5;
         Max := 4000;
      end
      else if (What = 'RRI') then begin
         Min := 0;
         Max := 200;
      end
      else if (What = 'HILL') then begin
         Min := 0;
         Max := 255;
      end
      else if (What = 'SLOPE') then begin
         Min := 0;
         Max := 1000;
      end
      else if (What = 'TPI') then begin
         Min := -50;
         Max := 50;
      end
      else if (What = 'RUFF') then begin
         Min := 0;
         Max := 125;
      end;
      WantShowProgress := false;
      DEMGlb[DEM].ClipToVerticalRange(Min,Max);
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM z range clipped'); {$EndIf};
      DEMGlb[DEM].AddConstantToGrid(-Min);
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM min set to 0'); {$EndIf};
      DEMGlb[DEM].MultiplyGridByConstant(1/(Max-Min));
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM range set to 1'); {$EndIf};
      DEMGlb[DEM].CheckMaxMinElev;
      WantShowProgress := true;
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM out'); {$EndIf};
   end;
end;


function ComputeSSIM(DEM1,DEM2 : integer; gl1,gl2 : tGridLimits; var SSIM,Luminance,Contrast,Structure : float64) : boolean;
const
  MaxIntensity = 1;
  c1 = 0.01;    //C1 := sqr(0.01 * MaxIntensity);
  c2 = 0.03;    //C2 := sqr(0.03 * MaxIntensity);
  c3 = 0.015;   //C3 := c2 / 2;
var
  r,covar,Mean1,Mean2,StdDev1,StdDev2 : float64;
begin
   {$IfDef RecordSSIMFull} WriteLineToDebugFile('ComputeSSIM '  + DEMGlb[DEM1].AreaName + ',' + DEMGlb[DEM2].AreaName); {$EndIf}
   Result := CovariancesFromTwoGrids(gl1, DEM1,DEM2, r,covar,Mean1,Mean2,StdDev1,StdDev2);
   if Result then begin
      {$IfDef RecordSSIMFull} WriteLineToDebugFile('Covariance done, covar=' + RealToString(Covar,-12,2)); {$EndIf}
      Luminance := (2 * Mean1 * Mean2 + C1) / (Sqr(Mean1) + Sqr(Mean2) + C1);
      Contrast := (2 * stddev1 * stddev2 + C2) / (sqr(StdDev1) + sqr(StdDev2) + C2);
      Structure := (Covar + c3) / (stddev1 * stddev2 + C3);
      SSIM := Luminance * Contrast * Structure;
      {$IfDef RecordSSIMFull} WriteLineToDebugFile('ComputeSSIM '  + DEMGlb[DEM1].AreaName + ',' +
          DEMGlb[DEM2].AreaName + ',' + RealToString(ssim,-12,8) + ',' + RealToString(luminance,-12,8) + ',' +
          RealToString(contrast,-12,8) + ',' + RealToString(structure,-12,8)); {$EndIf}
   end;
end;


procedure MakeSSIMMaps(DEM1,DEM2 : integer);
const
  MaxIntensity = 1;
  c1 = 0.01;    //C1 := sqr(0.01 * MaxIntensity);
  c2 = 0.03;    //C2 := sqr(0.03 * MaxIntensity);
  c3 = 0.015;   //C3 := c2 / 2;
var
  r,covar,Mean1,Mean2,StdDev1,StdDev2,SSIM,Luminance,Contrast,Structure : float64;
  xoffset,yoffset,x,y,NewGrid,xl,yl,ThinFactor : integer;
  gl1,gl2 : tGridLimits;
  AreaName : shortString;
begin
   AreaName := 'SSIM_' + DEMGlb[DEM1].AreaName + '_' + DEMGlb[DEM2].AreaName;
   {$IfDef RecordSSIMFull} WriteLineToDebugFile('ComputeSSIM in, '  + AreaName); {$EndIf}
   if DEMglb[DEM1].SecondGridJustOffset(DEM2,xoffset,yoffset) then begin
      HeavyDutyProcessing := true;
      NormalizeDEMforSSIM(DEM1,'Elev');
      NormalizeDEMforSSIM(DEM2,'Elev');

      ThinFactor := 4;
      if (ThinFactor > 1) then NewGrid := DEMGlb[DEM1].ThinAndOpenGridSetMissing(ThinFactor,FloatingPointDEM,AreaName,Undefined)
      else NewGrid := DEMGlb[DEM1].CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName,Undefined);

      StartProgress('Make SSIM Map');
      x := 0;
      while x <= pred(DEMglb[NewGrid].DEMHeader.NumCol) do begin
         gl1.XGridLow := ThinFactor * x - 5;
         gl1.XGridHigh := ThinFactor * x + 5;
         gl2.XGridLow := ThinFactor * x + yoffset - 5;
         gl2.XGridHigh := ThinFactor * x +yoffset + 5;

         {if (x mod 10 = 0) then} UpdateProgressBar(x/DEMglb[DEM1].DEMHeader.NumCol);
         y := 0;
         while y <= pred(DEMglb[NewGrid].DEMHeader.NumRow) do begin
             gl1.YGridLow := ThinFactor * y - 5;
             gl1.YGridHigh := ThinFactor * y + 5;
             gl2.YGridLow := ThinFactor * y + yoffset - 5;
             gl2.YGridHigh := ThinFactor * y + yoffset + 5;
             if ComputeSSIM(DEM1,DEM2,gl1,gl2,SSIM,Luminance,Contrast,Structure) then begin
                DEMGlb[NewGrid].SetGridElevation(x,y,SSIM);
             end;
             inc(y,1);
         end;
         inc(x,1);
      end;
      DEMglb[NewGrid].CheckMaxMinElev;
      DEMglb[NewGrid].SetUpMap(NewGrid,true,mtElevSpectrum);
      DEMGlb[DEM1].ReloadDEM(true);
      DEMGlb[DEM2].ReloadDEM(true);
      HeavyDutyProcessing := false;
   end;
   {$IfDef RecordSSIMFull} WriteLineToDebugFile('ComputeSSIM out, '  + AreaName); {$EndIf}
end;



   procedure DoSSIMandR2ForAnArea(AreaName : shortstring; Overwrite : boolean);
   var
      i : integer;
   const
      DOSSIM = true;
      DoR2 = true;
   var
      DEMIXtileDB,NumRef,j : integer;
      Results : tStringList;
      fName,SavefName : PathStr;
      Area,aLine,TileName : shortstring;
      bb : sfBoundBox;
      MissingFiles : boolean;
      AreaGrids,PointGrids,AreaGrids2,PointGrids2 : tDEM_int_array;


      function ProcessDEMTileSSIM(Ref,Test : integer; bb : sfBoundBox) : float32;
      var
         SSIM,Luminance,Contrast,Structure : float64;
         RefGridLimits,TestGridLimits : tGridLimits;
      begin
          if ValidDEM(Ref) and ValidDEM(Test) then begin
             RefGridLimits := DEMglb[Ref].sfBoundBox2tGridLimits(bb);
             TestGridLimits := DEMglb[Test].sfBoundBox2tGridLimits(bb);
             ComputeSSIM(Ref,Test,RefGridLimits,TestGridLimits,SSIM,Luminance,Contrast,Structure);
             Result := 1-SSIM;
          end
          else begin
             Result := -999;
             {$IfDef RepeatProblematicComputations}
                MessageToContinue('Check debugger');
                ComputeSSIM(Ref,Test,RefGridLimits,TestGridLimits,SSIM,Luminance,Contrast,Structure);
             {$EndIf}
          end;
      end;

             function DoCovarianceForPair(RefGridLimits : tGridLimits; Grid1,Grid2 : integer) : float64;
             var
                Mean1,Mean2,StdDev1,StdDev2,r,covar : float64 ;
             begin
                 {$IfDef TrackSWCornerForComputations} DEMGlb[Grid1].WriteToDebugSWCornerForComputations('AreaSSIMComputations'); {$EndIf}
                 {$IfDef TrackSWCornerForComputations} DEMGlb[Grid2].WriteToDebugSWCornerForComputations('AreaSSIMComputations'); {$EndIf}
                if CovariancesFromTwoGrids(RefGridLimits,Grid1,Grid2, r,covar,Mean1,Mean2,StdDev1,StdDev2) then begin
                   Result := 1-sqr(r);
                end
                else begin
                   Result := -999;
                   {$IfDef RepeatProblematicComputations}
                      MessageToContinue('Check debugger');
                      CovariancesFromTwoGrids(RefGridLimits,Grid1,Grid2, r,covar,Mean1,Mean2,StdDev1,StdDev2);
                   {$EndIf}
                end;
             end;

      procedure DoCriterion(Criterion,Count : ANSIString; PointGrids,AreaGrids : tDEM_int_array);
      var
         i : integer;
         Criterion2 : shortstring;
         RefGridLimits,TestGridLimits : tGridLimits;
         What : shortstring;
      begin
          {$IfDef RecordDEMIX} HighLightLineToDebugFile('Criterion=' + Criterion); {$EndIf}
          wmdem.SetPanelText(3, Criterion + ' ' + Count,true);

          if DoR2 then begin
             GISdb[DEMIXtileDB].MyData.First;
             while not GISdb[DEMIXtileDB].MyData.eof do begin
               //do correlation coefficients before normalizing grids
                bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
                TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
                wmdem.SetPanelText(2, TileName + ' R2',true);
                Criterion2 := StringReplace(Criterion,'SSIM','R2',[rfReplaceAll, rfIgnoreCase]);
                aLine := TileName  + ',' + Area + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + Criterion2;

                RefGridLimits := DEMglb[PointGrids[0]].sfBoundBox2tGridLimits(bb);
                for i := 1 to NumPt do aLine := aline + ',' + RealToString(DoCovarianceForPair(RefGridLimits,PointGrids[0],PointGrids[i]),-12,6);

                RefGridLimits := DEMglb[AreaGrids[0]].sfBoundBox2tGridLimits(bb);
                for i := 1 to NumArea do aLine := aline + ',' + RealToString(DoCovarianceForPair(RefGridLimits,AreaGrids[0],AreaGrids[i]),-12,6);
                Results.Add(aline);

                GISdb[DEMIXtileDB].MyData.Next;
             end;
          end;;

          if DoSSIM then begin
              //normalize grids for SSIM
              What := BeforeSpecifiedCharacterAnsi(Criterion,'_');
              for i := 0 to NumPt do NormalizeDEMforSSIM(PointGrids[i],What);
              for i := 0 to NumArea do NormalizeDEMforSSIM(AreaGrids[i],What);

             GISdb[DEMIXtileDB].MyData.First;
             while not GISdb[DEMIXtileDB].MyData.eof do begin
                bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
                TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
                wmdem.SetPanelText(2, TileName + ' SSIM',true);
                aLine := TileName + ',' + Area + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + Criterion;
                for i := 1 to NumPt do aLine := aline + ',' + RealToString(ProcessDEMTileSSIM(PointGrids[0],PointGrids[i],bb),-12,6);
                for i := 1 to NumArea do aLine := aline + ',' + RealToString(ProcessDEMTileSSIM(AreaGrids[0],AreaGrids[i],bb),-12,6);
                Results.Add(aline);
                GISdb[DEMIXtileDB].MyData.Next;
             end;
          end;
      end;

      procedure ClearDerivedGrids;
      var
         i : integer;
      begin
          for i := 0 to NumPt do CloseSingleDEM(PointGrids[i]);
          for i := 0 to NumArea do CloseSingleDEM(AreaGrids[i]);
          wmdem.SetPanelText(2, '',true);
      end;

   begin {procedure DoSSIMandR2ForAnArea}
       {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMComputations area=' + AreaName); {$EndIf}
       SaveFName := SSIMresultsDir + AreaName + '_ssim_results.csv';
       if (not Overwrite) and FileExists(SaveFName) then begin
          {$IfDef RecordDEMIXFull} HighLightLineToDebugFile('Area=' + AreaName + '  file exists=' + SaveFName); {$EndIf}
       end
       else begin
          {$IfDef RecordDEMIXFull} HighLightLineToDebugFile('AreaSSIMComputations area=' + AreaName); {$EndIf}
          wmdem.SetPanelText(3, 'Load DEMs',true);
          if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,false) then begin
             Results := tStringList.Create;
             aline := 'DEMIX_TILE,AREA,LAT,LONG,CRITERION';
             for i := 1 to NumPt do aline := aLine + ',' + PointNames[i];
             for i := 1 to NumArea do aline := aLine + ',' + AreaNames[i];
             Results.Add(aLine);

             DEMIXtileDB := DEMIXtileFill(PointDEMs[0],DEMGlb[PointDEMs[0]].DEMBoundBoxGeo,true);
             GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>' + IntToStr(MDDef.DEMIX_Full));

             wmdem.SetPanelText(3, 'Compute TPI',true);
             for i := 0 to NumPt do PointGrids[i] := DEMGlb[PointDEMs[i]].BoxCarDetrendDEM(false,DEMGlb[PointDEMs[i]].FullDEMGridLimits,3);
             for i := 0 to NumArea do AreaGrids[i] := DEMGlb[AreaDEMs[i]].BoxCarDetrendDEM(false,DEMGlb[AreaDEMs[i]].FullDEMGridLimits,3);
             DoCriterion('TPI_SSIM','1/8',PointGrids,AreaGrids);
             ClearDerivedGrids;

             wmdem.SetPanelText(3, 'Compute slope/roughness',true);
             for i := 0 to NumPt do PointGrids2[i] := 0;    //so slope grids are returned
             for i := 0 to NumArea do AreaGrids2[i] := 0;  //so slope grids are returned
             for i := 0 to NumPt do PointGrids[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(PointDEMs[i],5,PointGrids2[i],false);
             for i := 0 to NumArea do AreaGrids[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(AreaDEMs[i],5,AreaGrids2[i],false);
             DoCriterion('SLOPE_SSIM','2/8',PointGrids2,AreaGrids2);
             DoCriterion('RUFF_SSIM','3/8',PointGrids,AreaGrids);
             ClearDerivedGrids;
             for i := 0 to NumPt do CloseSingleDEM(PointGrids2[i]);
             for i := 0 to NumArea do CloseSingleDEM(AreaGrids2[i]);

             wmdem.SetPanelText(3, 'Compute Hillshade',true);
             for i := 0 to NumPt do PointGrids[i] := MakeSingleNewDerivativeMap('R',PointDEMs[i],0,false);
             for i := 0 to NumArea do AreaGrids[i] := MakeSingleNewDerivativeMap('R',AreaDEMs[i],0,false);
             DoCriterion('HILL_SSIM','4/8',PointGrids,AreaGrids);
             ClearDerivedGrids;

             wmdem.SetPanelText(3, 'Compute RRI',true);
             for i := 0 to NumPt do PointGrids[i] := MakeTRIGrid(PointDEMs[i],nmRRI,false);
             for i := 0 to NumArea do AreaGrids[i] := MakeTRIGrid(AreaDEMs[i],nmRRI,false);
             DoCriterion('RRI_SSIM','5/8',PointGrids,AreaGrids);
             ClearDerivedGrids;

             wmdem.SetPanelText(3, 'Compute flow accumulation',true);
             for i := 0 to NumPt do PointGrids[i] := WBT_FlowAccumulation(False,True,True,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := 0 to NumArea do AreaGrids[i] := WBT_FlowAccumulation(False,True,True,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
             DoCriterion('ACCUM_SSIM','6/8',PointGrids,AreaGrids);
             ClearDerivedGrids;

             wmdem.SetPanelText(3, 'Compute wetness index',true);
             for i := 0 to NumPt do PointGrids[i] := WBT_WetnessIndex(false,true,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := 0 to NumArea do AreaGrids[i] := WBT_WetnessIndex(false,true,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
             DoCriterion('WETIN_SSIM','7/8',PointGrids,AreaGrids);
             ClearDerivedGrids;

             wmdem.SetPanelText(3, 'Compute LS factor',true);
             for i := 0 to NumPt do PointGrids[i] := SAGA_LSFactor(false,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := 0 to NumArea do AreaGrids[i] := SAGA_LSFactor(false,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
             DoCriterion('LS_SSIM','8/8',PointGrids,AreaGrids);
             ClearDerivedGrids;

             //Elevation is done last, so we no longer need elevation to create derived grids, and can normalize it
             DoCriterion('ELEV_SSIM','9/8',PointDEMs,AreaDEMs);

             Results.SaveToFile(SavefName);
             Results.Free;
             CloseAllDEMs;
             CloseAndNilNumberedDB(DEMIXtileDB);
          end
          else begin
             {$IfDef RecordDEMIX} HighLightLineToDebugFile('Missing files for area ' + AreaName); {$EndIf}
          end;
       end;
   end {procedure DoSSIMandR2ForAnArea};



procedure AreaSSIMComputations(Overwrite : boolean);
var
   Areas : tStringList;
   Area  : shortstring;
   i     : integer;
begin {procedure AreaSSIMComputations}
   {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMComputations in'); {$EndIf}
   try
      GetDEMIXPaths(true);
      Areas := DEMIX_AreasWanted(true,Area);
      SafeMakeDir(SSIMresultsDir);
      LockStatusBar := true;
      wmdem.SetPanelText(0, 'Started ' + TimeToStr(Now),true);
      for i := 0 to pred(Areas.Count) do begin
         Area := Areas.Strings[i];
         wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + Area,true);
         DoSSIMandR2ForAnArea(Area,Overwrite);
      end;
   finally
      Areas.Free;
      EndDEMIXProcessing;
      LockStatusBar := false;
   end;
   {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMComputations out'); {$EndIf}
end {procedure AreaSSIMComputations};








(*
program SSIM;

//created by Bard, 27 Nov 2023  https://bard.google.com/chat/31239af044003953

{
  Calculate the structural similarity index (SSIM) between two images.
  The SSIM is a measure of image quality that considers luminance, contrast,
  and structure.

  @param img1 The first image.
  @param img2 The second image.
  @return The SSIM between the two images.
}

function ssim(img1: array of array of byte; img2: array of array of byte): real;
var
  mu1, mu2: real;
  sigma1^2, sigma2^2, sigma12: real;
  C1, C2: real;
  SSIM: real;

begin
  mu1 := mean(img1);
  mu2 := mean(img2);

  sigma1^2 := meanSquare(img1, mu1);
  sigma2^2 := meanSquare(img2, mu2);
  sigma12 := covariance(img1, img2, mu1, mu2);

  C1 := (0.01 * 255)^2;
  C2 := (0.03 * 255)^2;

  SSIM := (2 * mu1 * mu2 + C1) / (mu1^2 + mu2^2 + C1) *
         (2 * sigma12 + C2) / (sigma1^2 + sigma2^2 + C2);

end;

function mean(img: array of array of byte): real;
var
  sum: real;
  i, j: integer;

begin
  sum := 0;

  for i := 0 to high(img) do
  begin
    for j := 0 to high(img[i]) do
    begin
      sum := sum + img[i][j];
    end;
  end;

  mean := sum / (length(img) * length(img[0]));
end;

function meanSquare(img: array of array of byte; mu: real): real;
var
  sum: real;
  i, j: integer;

begin
  sum := 0;

  for i := 0 to high(img) do
  begin
    for j := 0 to high(img[i]) do
    begin
      sum := sum + square(img[i][j] - mu);
    end;
  end;

  meanSquare := sum / (length(img) * length(img[0]));
end;

function covariance(img1: array of array of byte; img2: array of array of byte; mu1: real; mu2: real): real;
var
  sum: real;
  i, j: integer;

begin
  sum := 0;

  for i := 0 to high(img1) do
  begin
    for j := 0 to high(img1[i]) do
    begin
      sum := sum + (img1[i][j] - mu1) * (img2[i][j] - mu2);
    end;
  end;

  covariance := sum / (length(img1) * length(img1[0]));
end;

function square(x: real): real;
begin
  square := x * x;
end;

begin
  writeln('SSIM:', ssim(img1, img2));
end.
Use code with caution. Learn more
content_copy
This code will calculate the SSIM between two images and print the result to the console. To use the code, you will need to replace img1 and img2 with the actual images you want to compare. The images should be represented as two-dimensional arrays of bytes, where each byte represents the intensity of a pixel.
I hope this helps!


==================================================================



Bing 1, 12/14/2023




Here is an example of Pascal code for calculating the Structural Similarity Index (SSIM) between two images:

```
program SSIM;

uses
  Math;

type
  TDoubleArray = array of Double;

function Mean(Arr: TDoubleArray): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr) do
    Result := Result + Arr[i];
  Result := Result / Length(Arr);
end;

function Variance(Arr: TDoubleArray; Mean: Double): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr) do
    Result := Result + Sqr(Arr[i] - Mean);
  Result := Result / Length(Arr);
end;

function Covariance(Arr1, Arr2: TDoubleArray; Mean1, Mean2: Double): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr1) do
    Result := Result + (Arr1[i] - Mean1) * (Arr2[i] - Mean2);
  Result := Result / Length(Arr1);
end;

function SSIM(Arr1, Arr2: TDoubleArray): Double;
const
  C1 = 0.01 * 255 * 0.01 * 255;
  C2 = 0.03 * 255 * 0.03 * 255;
var
  Mean1, Mean2, Var1, Var2, Cov, SSIM: Double;
begin
  Mean1 := Mean(Arr1);
  Mean2 := Mean(Arr2);
  Var1 := Variance(Arr1, Mean1);
  Var2 := Variance(Arr2, Mean2);
  Cov := Covariance(Arr1, Arr2, Mean1, Mean2);
  SSIM := ((2 * Mean1 * Mean2 + C1) * (2 * Cov + C2)) /
    ((Sqr(Mean1) + Sqr(Mean2) + C1) * (Var1 + Var2 + C2));
  Result := SSIM;
end;

var
  Img1, Img2: TDoubleArray;
  i: Integer;
begin
  SetLength(Img1, 10);
  SetLength(Img2, 10);
  for i := 0 to 9 do
  begin
    Img1[i] := Random;
    Img2[i] := Random;
  end;
  WriteLn(SSIM(Img1, Img2));
end.
```

This code calculates the SSIM between two arrays of doubles, which represent the pixel intensities of two images.
The SSIM is a measure of the structural similarity between the two images,
taking into account luminance, contrast, and structure ¹.
The code uses the mean, variance, and covariance of the pixel intensities to calculate the SSIM ¹.

I hope this helps!

Source: Conversation with Bing, 12/14/2023
(1) Structural similarity index — skimage 0.22.0 documentation - scikit-image. https://scikit-image.org/docs/stable/auto_examples/transform/plot_ssim.html.
(2) structural-similarity-index · GitHub Topics · GitHub. https://github.com/topics/structural-similarity-index.
(3) Correlation Maximized Structural Similarity Loss for Semantic Segmentation. https://arxiv.org/pdf/1910.08711v1.pdf.
(4) An Overview Of Structural Similarity - GameDev Academy. https://gamedevacademy.org/structural-similarity-tutorial/.
(5) Zhou Wang - University of Waterloo. https://ece.uwaterloo.ca/~z70wang/research/ssim/.
(6) undefined. https://www.youtube.com/watch?v=-i3NQ-by2b8.





Bing 2

Yes, it is possible to compute the luminance, contrast, and structure separately in SSIM using Pascal. The code I provided earlier calculates the SSIM between two arrays of doubles, which represent the pixel intensities of two images. To compute the luminance, contrast, and structure separately, you can modify the code to calculate the mean, variance, and covariance of the pixel intensities for each component separately. Here is an example of how to do this:

```
function Luminance(Arr: TDoubleArray): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr) do
    Result := Result + Arr[i];
  Result := Result / Length(Arr);
end;

function Contrast(Arr: TDoubleArray; Mean: Double): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr) do
    Result := Result + Sqr(Arr[i] - Mean);
  Result := Sqrt(Result / Length(Arr));
end;


function Structure(Arr1, Arr2: TDoubleArray; Mean1, Mean2: Double): Double;
var
  i: Integer;
  Cov: Double;
begin
  Cov := 0;
  for i := 0 to High(Arr1) do
    Cov := Cov + (Arr1[i] - Mean1) * (Arr2[i] - Mean2);
  Result := (Cov / Length(Arr1)) / (Variance(Arr1, Mean1) * Variance(Arr2, Mean2));
end;


var
  Img1, Img2: TDoubleArray;
  Mean1, Mean2, Var1, Var2, Cov, L, C, S: Double;
  i: Integer;
begin
  SetLength(Img1, 10);
  SetLength(Img2, 10);
  for i := 0 to 9 do
  begin
    Img1[i] := Random;
    Img2[i] := Random;
  end;
  Mean1 := Mean(Img1);
  Mean2 := Mean(Img2);
  Var1 := Variance(Img1, Mean1);
  Var2 := Variance(Img2, Mean2);
  Cov := Covariance(Img1, Img2, Mean1, Mean2);
  L := Luminance(Img1) / Luminance(Img2);
  C := Contrast(Img1, Mean1) / Contrast(Img2, Mean2);
  S := Structure(Img1, Img2, Mean1, Mean2);
  WriteLn('Luminance: ', L:0.000);
  WriteLn('Contrast: ', C:0.000);
  WriteLn('Structure: ', S:0.000);
end.
```





This code calculates the luminance, contrast, and structure components of the SSIM between two arrays of doubles,
which represent the pixel intensities of two images.
The `Luminance` function calculates the mean pixel intensity,
the `Contrast` function calculates the standard deviation of the pixel intensities, and
the `Structure` function calculates the covariance of the pixel intensities ¹.
The `Luminance`, `Contrast`, and `Structure` values can be used to compute the SSIM using the same formula as before ¹.

I hope this helps!

Source: Conversation with Bing, 12/14/2023
(1) Structural similarity - Wikipedia. https://en.wikipedia.org/wiki/Structural_similarity.
(2) Structural similarity (SSIM) index for measuring image quality - MathWorks. https://www.mathworks.com/help/images/ref/ssim.html.
(3) [2006.13846] Understanding SSIM - arXiv.org. https://arxiv.org/abs/2006.13846.






*)
