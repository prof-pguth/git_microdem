{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{   include file for demstat             }
{________________________________________}




procedure CompareUpsampling(SlopeOption : boolean = false);
var
   HRD_DEM,LowResDEM,
   NewDEM1,NewDEM2,NewDEM3,NewDEM4,NewDEM5,
   LowResUpsample,
   LowSlopeUpsample,
   SlopeLowUpsample,
   HiSlope,LowSlope : integer;
   spacingInDegrees,SpacingInSeconds : float32;
begin
   {$IfDef RecordComparisons} WriteLineToDebugFile('CompareUpsampling, SlopeOption=' + TrueOrFalse(SlopeOption));  {$EndIf}

   if (NumDEMDataSetsOpen = 2) and ValidDEM(1) and ValidDEM(2) then begin
      if DEMGlb[1].AverageSpace < DEMGlb[2].AverageSpace then begin
         HRD_DEM := 1;
         LowResDEM := 2;
      end
      else begin
         HRD_DEM := 2;
         LowResDEM := 1;
      end;
      LowResUpsample := UpsampleDEM(true,HRD_DEM,LowResDEM);
      if SlopeOption then begin
          HiSlope := CreateEvansSlopeMapPercent(true,HRD_DEM,MDtempDir + 'Slope_HRD.tif');
          LowSlope := CreateEvansSlopeMapPercent(true,LowResDEM,MDtempDir + 'Slope_LowRes_DEM.tif');
          LowSlopeUpsample := UpsampleDEM(true,HRD_DEM,LowSlope,MDtempDir + 'Upsampled_LowRes_Slope.tif');
          SlopeLowUpsample := CreateEvansSlopeMapPercent(true,LowResUpsample,MDtempDir + 'Slope_Upsampled_LowRes_DEM.tif');
      end
      else begin

        SpacingInDegrees := {3600 *} DEMGlb[HRD_DEM].DEMHeader.DEMxSpacing;

        NewDEM4 := GDAL_upsample_DEM(True,LowResDEM,false,SpacingInDegrees,MDTempDir + 'GDAL_Upsampled_' + DEMGlb[LowResDEM].AreaName + '_bilinear.tif');
        NewDEM5 := GDAL_upsample_DEM(True,LowResDEM,true,SpacingInDegrees,MDTempDir + 'GDAL_Upsampled_' + DEMGlb[LowResDEM].AreaName + '_bicubic.tif');

        SpacingInSeconds := 3600 * DEMGlb[HRD_DEM].DEMHeader.DEMxSpacing;
        MDDef.ElevInterpolation := piBicubicVT;
        NewDEM1 := ReinterpolateLatLongDEM(True,LowResDEM,SpacingInSeconds,MDTempDir + 'Upsampled_' + DEMGlb[LowResDEM].AreaName + '_bicubicVT.dem');
        MDDef.ElevInterpolation := piBicubicVT;
        NewDEM3 := ReinterpolateLatLongDEM(True,LowResDEM,SpacingInSeconds,MDTempDir + 'Upsampled_' + DEMGlb[LowResDEM].AreaName + '_bicubicNR.dem');
        MDDef.ElevInterpolation := piBilinear;
        NewDEM2 := ReinterpolateLatLongDEM(True,LowResDEM,SpacingInSeconds,MDTempDir + 'Upsampled_' + DEMGlb[LowResDEM].AreaName + '_bilinear.dem');

      end;
   end
   else begin
      //to be done later
   end;
end;

procedure CompareLSPthenupsampletoUpsamplethenLSP;
begin
   CompareUpsampling(false);
end;


procedure CompareLSQ(DoSlopes : boolean; DEM : integer);

         function Duplicate(HowCompute : tSlopeCurveCompute) : boolean;
         begin
            Result := ((HowCompute.LSQorder = 1) and (HowCompute.WindowRadius = 1)) or
                      ((HowCompute.LSQorder = 2) and (HowCompute.WindowRadius = 1) and (not HowCompute.RequireFullWindow)) or
                      ((HowCompute.LSQorder in [3,4]) and (HowCompute.WindowRadius = 1)) or
                      ((HowCompute.LSQorder in [3,4]) and (not HowCompute.RequireFullWindow)) or
                      ((HowCompute.LSQorder = 4) and (HowCompute.WindowRadius = 2));
         end;


var
  i,j,k,Grid,TheCurvature : integer;
begin
   SetColorForProcessing;
   SaveBackupDefaults;
   MDDef.CurveCompute.AlgorithmName := smLSQ;

   if DoSlopes then begin
      for i := 1 to 4 do begin
         MDDef.SlopeCompute.LSQorder := i;
         for j := 1 to 4 do begin
            MDDef.SlopeCompute.WindowRadius := j;
            for k := 1 to 3 do begin
               MDDef.SlopeCompute.UsePoints := k;
               if not Duplicate(MDDef.SlopeCompute) then begin
                  Grid := CreateSlopeMapPercentAlgorithm(MDDef.SlopeCompute,true,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute));
               end;
            end;
         end;
      end;
   end
   else begin
      TheCurvature := eucurv_kns;
      for i := 1 to 4 do begin
         MDDef.CurveCompute.LSQorder := i;
         for j := 1 to 4 do begin
            MDDef.CurveCompute.WindowRadius := j;
            for k := 1 to 3 do begin
               MDDef.CurveCompute.UsePoints := k;
               if not Duplicate(MDDef.CurveCompute) then begin
                  Grid := CreateCurvatureMap(TheCurvature,true,DEM,SlopeMethodName(MDDef.CurveCompute) );
               end;
            end;
         end;
      end;
   end;
   RestoreBackupDefaults;
   SetColorForWaiting;
end;


{$IfDef SlopeWindowEdgeEffect}
  procedure CompareLSQEdgeEffects(DEM : integer);

      procedure CompareOneOrder(Order,Window : integer);
      const
         OpenMap = false;
      var
        Grid1,Grid2,Diff : integer;
        Graph : tThisBaseGraph;
      begin
         MDDef.SlopeCompute.LSQorder := Order;
         MDDef.SlopeCompute.WindowRadius := Window;
         TestEdgeEffect := true;
         Grid2 := CreateSlopeMapPercent(OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute,true));
         TestEdgeEffect := false;
         Grid1 := CreateSlopeMapPercent(OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute,true));
         Diff := MakeDifferenceMap(Grid1,Grid2,Grid1,0,true,false,false,SlopeMethodName(MDDef.SlopeCompute,true));
         DEMGlb[Diff].SelectionMap.MapDraw.MapType := mtElevSpectrum;
         DEMGlb[Diff].SelectionMap.N11view1Click(Nil);

         MDDef.DefaultGraphXSize := DEMGlb[Diff].SelectionMap.Width-75;
         MDDef.DefaultGraphYSize := DEMGlb[Diff].SelectionMap.Width-75;

         Graph := GridScatterGram(DEMglb[Grid1].FullDEMGridLimits,Grid1,Grid2);
         Graph.GraphDraw.MaxHorizAxis := 150;
         Graph.GraphDraw.MaxVertAxis := 150;
         Graph.GraphDraw.MinHorizAxis := 0;
         Graph.GraphDraw.MinVertAxis := 0;
         Graph.GraphDraw.HorizLabel := 'Slope (%)--Full Window';
         Graph.GraphDraw.VertLabel := 'Slope (%)--Edge Window';

         Graph.RedrawDiagram11Click(Nil);

         CloseSingleDEM(Grid1);
         CloseSingleDEM(Grid2);
      end;

  begin
     SetColorForProcessing;
     //NakedMapOptions;
     SaveBackupDefaults;
     MDDef.EvansApproximationAllowed := false;
     MDDef.SlopeCompute.AlgorithmName := smLSQ;
     CompareOneOrder(2,1);
     CompareOneOrder(3,2);
     CompareOneOrder(4,2);

     RestoreBackupDefaults;
     SetColorForWaiting;
  end;
{$EndIf}


  procedure CompareLSQPointsUsedEffects(DEM : integer);

      procedure CompareOneOrder(Order,Window,Use : integer; title : shortstring);
      const
         OpenMap = false;
      var
        Grid1,Grid2,Diff : integer;
        Graph : tThisBaseGraph;
      begin
         MDDef.SlopeCompute.LSQorder := Order;
         MDDef.SlopeCompute.WindowRadius := Window;
         MDDef.SlopeCompute.UsePoints := useAll;
         Grid2 := CreateSlopeMapPercentAlgorithm(MDDef.SlopeCompute,OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute,true));

         MDDef.SlopeCompute.UsePoints := Use;

         Grid1 := CreateSlopeMapPercentAlgorithm(MDDef.SlopeCompute,OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute,true));
         Diff := MakeDifferenceMap(Grid1,Grid2,Grid1,0,true,false,false,SlopeMethodName(MDDef.SlopeCompute,true) + '_' + Title);
         DEMGlb[Diff].SelectionMap.MapDraw.MapType := mtElevSpectrum;
         DEMGlb[Diff].SelectionMap.N11view1Click(Nil);

         MDDef.DefaultGraphXSize := DEMGlb[Diff].SelectionMap.Width-65;
         MDDef.DefaultGraphYSize := DEMGlb[Diff].SelectionMap.Width-65;
         Graph := GridScatterGram(DEMglb[Grid1].FullDEMGridLimits,Grid1,Grid2);
         Graph.GraphDraw.MaxHorizAxis := 150;
         Graph.GraphDraw.MaxVertAxis := 150;
         Graph.GraphDraw.MinHorizAxis := 0;
         Graph.GraphDraw.MinVertAxis := 0;
         Graph.GraphDraw.HorizLabel := 'Slope (%)--' + Title + ' Points';
         Graph.GraphDraw.VertLabel := 'Slope (%)--All Points';
         Graph.Caption := 'Scattergram_' + SlopeMethodName(MDDef.SlopeCompute);
         Graph.RedrawDiagram11Click(Nil);

         CloseSingleDEM(Grid1);
         CloseSingleDEM(Grid2);
      end;

  begin
     SetColorForProcessing;
     SaveBackupDefaults;
     MDDef.EvansApproximationAllowed := false;
     MDDef.SlopeCompute.AlgorithmName := smLSQ;
     CompareOneOrder(2,2,useEdge,'Edge');
     CompareOneOrder(2,2,useQueens,'Queen');
     CompareOneOrder(3,2,useEdge,'Edge');
     CompareOneOrder(4,2,useEdge,'Edge');

     RestoreBackupDefaults;
     SetColorForWaiting;
  end;


procedure CompareThreeLSQ(DEM : integer);
const
   OpenMap = true;
var
   Evans : integer;


        procedure CleanupMap(Map : integer);
        begin
           DEMGlb[Map].SelectionMap.MapDraw.MapType := mtElevSpectrum;
           DEMGlb[Map].SelectionMap.N11view1Click(Nil);
        end;



    procedure CompareOneOrder(Order,Window : integer);
    var
      Grid1,Diff : integer;
      Graph : tThisBaseGraph;
    begin
       MDDef.SlopeCompute.LSQorder := Order;
       MDDef.SlopeCompute.WindowRadius := Window;
       Grid1 := CreateSlopeMapPercentAlgorithm(MDDef.SlopeCompute,OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute));
       Diff := MakeDifferenceMap(Grid1,Evans,Grid1,0,true,false,true,'Evans compared to ' + SlopeMethodName(MDDef.SlopeCompute));
       CleanUpMap(Grid1);
       CleanUpMap(Diff);

       MDDef.DefaultGraphXSize := DEMGlb[Diff].SelectionMap.Width-65;
       MDDef.DefaultGraphYSize := DEMGlb[Diff].SelectionMap.Width-65;
       Graph := GridScatterGram(DEMglb[Evans].FullDEMGridLimits,Grid1,Evans);
       Graph.GraphDraw.MaxHorizAxis := 150;
       Graph.GraphDraw.MaxVertAxis := 150;
       Graph.GraphDraw.MinHorizAxis := 0;
       Graph.GraphDraw.MinVertAxis := 0;
       Graph.GraphDraw.HorizLabel := 'Slope (%)--' + SlopeMethodName(MDDef.SlopeCompute);
       Graph.GraphDraw.VertLabel := 'Slope (%)--Evans';
       Graph.Caption := 'Scattergram_' + SlopeMethodName(MDDef.SlopeCompute);
       Graph.RedrawDiagram11Click(Nil);
    end;

begin
   SetColorForProcessing;
   SaveBackupDefaults;
   DEMGlb[DEM].SelectionMap.N11view1Click(Nil);
   MDDef.EvansApproximationAllowed := false;
   MDDef.SlopeCompute.AlgorithmName := smLSQ;
   MDDef.SlopeCompute.LSQorder := 2;
   MDDef.SlopeCompute.WindowRadius := 1;
   Evans := CreateSlopeMapPercentAlgorithm(MDDef.SlopeCompute,OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute));
   CleanUpMap(Evans);
   CompareOneOrder(3,2);
   CompareOneOrder(3,3);
   RestoreBackupDefaults;
   SetColorForWaiting;
end;


procedure CompareLSQSlopePointSelection(DEM : integer);
const
   OpenMap = true;
var
   i,j : integer;
begin
   SetColorForProcessing;
   SaveBackupDefaults;
   MDDef.EvansApproximationAllowed := false;
   MDDef.SlopeCompute.AlgorithmName := smLSQ;
   MDDef.SlopeCompute.WindowRadius := 2;
   for j := 2 to 3 do begin
     MDDef.SlopeCompute.LSQorder := j;
     for i := 1 to 3 do begin
        MDDef.SlopeCompute.UsePoints := i;
        if IsLSQslopeMethodAllowed(MDDef.SlopeCompute) then begin
           CreateSlopeMapPercentAlgorithm(MDDef.SlopeCompute,OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute));
        end;
     end;
   end;
   RestoreBackupDefaults;
   SetColorForWaiting;
end;


procedure CompareWindowSizesForSlopeMap(DEM : integer);
const
   NumWin = 9;
   Windows : array[1..NumWin] of integer = (1,2,3,5,10,15,20,25,30);
var
   i : integer;
   fName : PathStr;
   Findings : tStringList;
   Fixed : int64;

      procedure ProcessNewGrid;
      var
         Slope : integer;
         n : int64;
         sMean,sStd,rMean,rStd : float32;
      begin
         Slope := CreateSlopeMapPercentAlgorithm(MDDef.SlopeCompute,false,DEM,fName);
         DEMglb[Slope].ElevationStatistics(DEMglb[Slope].FullDEMGridLimits,sMean,sStd,n);
         Findings.Add(DEMglb[Slope].AreaName + ',UTM,' + RealToString(MDDef.SlopeCompute.WindowRadius * DEMglb[Slope].AverageSpace,-8,-2) + ',' + RealToString(sMean,-8,-2) + ',' + RealToString(sStd,-8,-2) );
         CloseSingleDEM(Slope);
      end;


begin
   SetColorForProcessing;
   SaveBackupDefaults;
   Findings := tStringList.Create;
   Findings.Add('NAME,SERIES,AVG_SPACE,AVG_SLOPE,STD_SLOPE');

   DEMGLb[DEM].MarkBelowMissing(1.0,Fixed,false);

   for i := 1 to NumWin do begin
      fName := 'window_' + IntToStr(Windows[i]) + '_m';
      wmdem.SetPanelText(3,'utm ' + IntToStr(i) + '/' + IntToStr(NumWin));
      MDDef.SlopeCompute.WindowRadius := Windows[i];
      ProcessNewGrid;
   end;

   wmdem.SetPanelText(3,'');
   SetColorForWaiting;

   fName := NextFileNumber(MDtempDir,'slope_window_sampler_','.dbf');
   Findings.SaveToFile(fName);
   StringList2CSVtoDB(Findings,fName);
   RestoreBackupDefaults;
end;


