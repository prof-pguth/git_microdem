{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{   include file for demstat             }
{________________________________________}


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
                  Grid := CreateSlopeMapPercent(true,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute));
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


procedure CompareLSQEdgeEffects(DEM : integer);

    procedure CompareOneOrder(Order,Window : integer);
    const
       OpenMap = false;
    var
      Grid1,Grid2,Diff : integer;
    begin
       MDDef.SlopeCompute.LSQorder := Order;
       MDDef.SlopeCompute.WindowRadius := Window;
       TestEdgeEffect := true;
       Grid2 := CreateSlopeMapPercent(OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute));
       TestEdgeEffect := false;
       Grid1 := CreateSlopeMapPercent(OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute));
       Diff := MakeDifferenceMap(Grid1,Grid2,Grid1,0,true,false,true,SlopeMethodName(MDDef.SlopeCompute));
       DEMGlb[Diff].SelectionMap.MapDraw.MapType := mtElevSpectrum;
       DEMGlb[Diff].SelectionMap.N11view1Click(Nil);
       CloseSingleDEM(Grid1);
       CloseSingleDEM(Grid2);
    end;

begin
   SetColorForProcessing;
   NakedMapOptions;
   //SaveBackupDefaults;
   MDDef.EvansApproximationAllowed := false;
   MDDef.SlopeCompute.AlgorithmName := smLSQ;
   CompareOneOrder(2,1);
   CompareOneOrder(3,2);
   CompareOneOrder(4,2);

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
      Grid1,Grid2,Diff : integer;
    begin
       MDDef.SlopeCompute.LSQorder := Order;
       MDDef.SlopeCompute.WindowRadius := Window;
       Grid1 := CreateSlopeMapPercent(OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute));
       Diff := MakeDifferenceMap(Grid1,Grid2,Grid1,0,true,false,true,'Evans compared to ' + SlopeMethodName(MDDef.SlopeCompute));
       CleanUpMap(Grid1);
       CleanUpMap(Diff);
       //CloseSingleDEM(Grid1);
       //CloseSingleDEM(Grid2);
    end;

begin
   SetColorForProcessing;
   SaveBackupDefaults;
   MDDef.EvansApproximationAllowed := false;
   MDDef.SlopeCompute.AlgorithmName := smLSQ;
   MDDef.SlopeCompute.LSQorder := 2;
   MDDef.SlopeCompute.WindowRadius := 1;
   Evans := CreateSlopeMapPercent(OpenMap,DEM,MDtempDir + SlopeMethodName(MDDef.SlopeCompute));
   CleanUpMap(Evans);
   CompareOneOrder(3,2);
   CompareOneOrder(4,3);

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
         Slope := CreateSlopeMapPercent(false,DEM,fName);
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


