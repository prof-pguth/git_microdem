{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for md_use_tools   }
{___________________________________}


//  https://sourceforge.net/p/saga-gis/wiki/Executing%20Modules%20with%20SAGA%20CMD/#:~:text=SAGA%20CMD%20is%20a%20command,application%20like%20a%20web%20server.


 function IsSagaCMDthere : boolean;
 begin
    Result := FileExists(MDDef.SagaCMD);
    if (not Result) then begin
       FindDriveWithFile(MDDef.SagaCMD);
       Result := FileExists(MDDef.SagaCMD);
       if (not Result) then begin
          Result := GetExistingFileName('saga_cmd.exe','*.exe',MDDef.SagaCMD);
       end;
       if Result then SaveMDdefaults;
    end;
 end;


 function SAGAMap(OpenMap : boolean; cmd : shortstring; OutName : shortstring; ZUnits : tElevUnit; MapType :tMapType = mtElevRainbow) : integer;
 var
    tName : PathStr;
 begin
    Result := 0;
    if IsSagaCMDthere then begin
       {$IfDef RecordSAGA} WriteLineToDebugFile('SAGAMap in, cmd= ' + CMD); {$EndIf}
       WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
       if FileExists(OutName) then begin
         //output file from SAGA must be converted to simple Geotiff
         tName := ExtractFilePath(OutName) + 'temp' + ExtractFileName(OutName);
         RenameFile(OutName,tName);
         GDAL_Translate_2_geotiff(tName,OutName);
         {$IfDef RecordSAGA} WriteLineToDebugFile('SAGAMap opening map ' + OutName); {$EndIf}
         Result := OpenNewDEM(OutName,false);
         if ValidDEM(Result) then begin
            {$IfDef RecordSAGARanges} WriteLineToDebugFile(DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].KeyDEMParams(true)); {$EndIf}
            DEMGlb[Result].CheckMaxMinElev;
            if (DEMGlb[Result].DEMHeader.DEMUsed = ArcSecDEM) then begin
               if (zUnits in [euPercentSlope,euPerMeter]) then begin
                  DEMGlb[Result].MultiplyGridByConstant(1/DEMGlb[Result].GDAL_ScaleFactor);
                  DEMGlb[Result].CheckMaxMinElev;
                  {$IfDef RecordSAGARanges} WriteLineToDebugFile('Geo corrections  ' + DEMGlb[Result].KeyDEMParams(true)); {$EndIf}
                  DEMGlb[Result].SaveAsGeotiff(OutName);
               end;
            end;
            if OpenMap then begin
               DEMGlb[Result].DEMheader.ElevUnits := zUnits;
               CreateDEMSelectionMap(Result,true,true,MapType);
               UpdateMenusForAllMaps;
            end;
         end;
       end;
       {$IfDef RecordSAGA} WriteLineToDebugFile('SAGAMap out, grid=' + IntToStr(Result)); {$EndIf}
    end;
 end;


function SAGA_HillValleyIndexes(OpenMap : boolean; DEMName : PathStr; ValleyIndexName : PathStr = ''; HillIndexName : PathStr = '') : integer;
 //https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_morphometry_24.html
 //Usage: saga_cmd ta_morphometry 24 [-DEM <str>] [-VALLEY <str>] [-HILL <str>] [-VALLEY_IDX <str>] [-HILL_IDX <str>] [-SLOPE_IDX <str>] [-RADIUS_VALLEY <double>] [-RADIUS_HILL <double>] [-THRESHOLD <double>] [-METHOD <str>]
 //both using the SAGA GUI and calling from MICRODEM leads to the program hanging (or it is just incredibly slow)
 var
   cmd : shortstring;
   RadValley,RadHill,Threshhold,
   DEM2 : integer;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
     (*
      //suggested in https://sourceforge.net/p/saga-gis/discussion/790705/thread/ca9ff9c8b7/?limit=25, but did not really help
      RadValley := 10;
      RadHill := 10;
      Threshhold := 2;

      //defaults divided by 10, still problem
      RadValley := 100;
      RadHill := 100;
      Threshhold := 10;

      //defaults testing  for arc second which hangs
      RadValley := 30;
      RadHill := 30;
      Threshhold := 30;  //3 did not work either
     *)
      //defaults which work for the gross differences for UTM grids
      RadValley := 1000;
      RadHill := 1000;
      Threshhold := 100;

      if (ValleyIndexName = '') then ValleyIndexName := MDTempDir + 'saga_valley_index_' + ExtractFileNameNoExt(DEMName) + '.tif';
      if (HillIndexName = '') then HillIndexName := MDTempDir + 'saga_hill_index_' + ExtractFileNameNoExt(DEMName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_morphometry 24  -DEM ' + DEMName + ' -VALLEY_IDX ' + ValleyIndexName + ' -HILL_IDX ' + HillIndexName +
           ' -RADIUS_VALLEY ' + IntToStr(RadValley) + ' -RADIUS_HILL  ' + IntToStr(RadHill) + ' -THRESHOLD ' + IntToStr(ThreshHold);
      {$If Defined(RecordSAGA) or Defined(SAGA_HillValley)} WriteLineToDebugFile('Hill valley index:   ' + cmd); {$EndIf}
      Result := SAGAMap(OpenMap,cmd,ValleyIndexName,euUndefined);
      if OpenMap then begin
         DEM2 := OpenNewDEM(HillIndexName,OpenMap);
         if ValidDEM(DEM2) then begin
            DEMGlb[DEM2].DEMheader.ElevUnits := euUndefined;
            CreateDEMSelectionMap(DEM2,true,true,mtElevSpectrum);
            UpdateMenusForAllMaps;
         end;
      end;
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Hill valley index created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;


function SAGA_LSFactor(OpenMap : boolean; InName : PathStr; LSgridName : PathStr = '') : integer;
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_compound_0.html
// this is very slow
var
   cmd : shortstring;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (LSGridName = '') then LSGridName := MDTempDir + 'saga_ls_factor_' + ExtractFileNameNoExt(InName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_compound 0 -ELEVATION ' + InName + ' -LSFACTOR ' + LSGridName;
      {$IfDef RecordSAGA} WriteLineToDebugFile('LS_factor, ' + cmd); {$EndIf}
      //WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
      Result := SAGAMap(OpenMap,cmd,LSGridName,euUndefined);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga ls created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;


function SAGA_ConvergenceIndex(OpenMap : boolean; InName : PathStr; ConIndexGridName : PathStr = '') : integer;
//Basic Terrain Analysis
//  https://saga-gis.sourceforge.io/saga_tool_doc/9.4.1/ta_compound_0.html
//  produces up to 14 grids, 2 vectors
//  only one parameter (channel density threshhold)
//  this can be very slow
//Convergence Index
//  https://saga-gis.sourceforge.io/saga_tool_doc/9.4.1/ta_morphometry_1.html
//  saga_cmd ta_morphometry 1 [-ELEVATION ] [-RESULT ] [-METHOD ] [-NEIGHBOURS ]
//  default is aspect method, 2x2 neighbors
//Convergence Index (Search Radius)
//  https://saga-gis.sourceforge.io/saga_tool_doc/9.4.1/ta_morphometry_2.html
//  saga_cmd ta_morphometry 2 [-ELEVATION ] [-CONVERGENCE ] [-SLOPE ] [-DIFFERENCE ] [-RADIUS ] [-DW_WEIGHTING ] [-DW_IDW_POWER ] [-DW_BANDWIDTH ]
var
   cmd : shortstring;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (ConIndexGridName = '') then ConIndexGridName := MDTempDir + 'saga_CONIN_' + ExtractFileNameNoExt(InName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}

      //this worked for almost all DEMIX, but hangs for lucarno point referece DTM; adding MAXITER (not in tool documentation) did not work and suggested by progress in command window
      //  cmd := MDDef.SagaCMD + ' ta_compound 0 -ELEVATION ' + InName + ' -CONVERGENCE ' + ConIndexGridName + ' -MAXITER 25';

      cmd := MDDef.SagaCMD + ' ta_morphometry 1 -ELEVATION ' + InName + ' -RESULT ' + ConIndexGridName;

      {$If Defined(RecordSAGALS)} WriteLineToDebugFile(cmd); {$EndIf}
      Result := SAGAMap(Openmap,cmd,ConIndexGridName,euUndefined);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga conin created ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;

function SAGA_CurvatureClassification(OpenMap : boolean; DEMName : PathStr; CurvatureClassFName : PathStr = '') : integer;
//https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_morphometry_4.html
//saga_cmd ta_morphometry 4 [-DEM <str>] [-CLASSES <str>]
var
   cmd : shortstring;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (CurvatureClassFName = '') then CurvatureClassFName := MDTempDir + 'saga_curvature_class_' + ExtractFileNameNoExt(DEMName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_morphometry 4 -DEM ' + DEMName + ' -CLASSES '  + CurvatureClassFName;
      Result := SAGAMap(OpenMap,cmd,CurvatureClassFName,euIntCode);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga curvature class  ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;


function SAGA_Geomorphons(OpenMap : boolean; DEMName : PathStr; GeomorphonsFName : PathStr = '') : integer;
//  https://saga-gis.sourceforge.io/saga_tool_doc/9.4.0/ta_lighting_8.html
//  saga_cmd ta_lighting 8 [-DEM ] [-GEOMORPHONS ] [-THRESHOLD ] [-RADIUS ] [-METHOD ] [-DLEVEL ]
//  https://sourceforge.net/p/saga-gis/discussion/790705/thread/08b8445420/   which recommends multiscale method 0, not the default, and looking at radius
var
   cmd : shortstring;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (GeomorphonsFName = '') then GeomorphonsFName := MDTempDir + 'saga_geomorphons_' + ExtractFileNameNoExt(DEMName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_lighting 8 -DEM ' + DEMName + ' -GEOMORPHONS '  + GeomorphonsFName + ' -RADIUS 50 -METHOD 0';
      Result := SAGAMap(Openmap,cmd,GeomorphonsFName,euGeomorphon,mtLandCover);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga geomorphoons  ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;


function SAGA_IwahashiAndPikeClassification(OpenMap : boolean; DEMName : PathStr; Classes : integer = 12; ClassFName : PathStr = '') : integer;
//https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_morphometry_22.html
//saga_cmd ta_morphometry 22 [-DEM <str>] [-SLOPE <str>] [-CONVEXITY <str>] [-CONV_RECALC <str>] [-TEXTURE <str>] [-TEXT_RECALC <str>] [-LANDFORMS <str>]
//MIOCRODEM has DB to reclassify the code
//   how to find what the codes mean, https://sourceforge.net/p/saga-gis/discussion/790705/thread/2f6e99e6b4/ ;
//   in the settings tab of the dataset properties, the "Colors > Type" parameter is set to "Classifed".
//      Just below this parameter you find the "Table" parameter. Click on it, then on the ellipsis button appearing in the parameter field.
//      This will open up the LUT. There you can use the "Save" button to save the LUT or the "Save > Workspace" button to add the LUT as dataobject in the "Data" tab.
var
   cmd : shortstring;
   ClassesCode : integer;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (Classes = 8) then ClassesCode := 0
      else if (Classes = 12) then ClassesCode := 1
      else if (Classes = 16) then ClassesCode := 2
      else ClassesCode := 1;

      if (ClassFName = '') then ClassFName := MDTempDir + 'saga_IwahashiAndPike_class_' + ExtractFileNameNoExt(DEMName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_morphometry 22 -DEM ' + DEMName + ' -LANDFORMS '  + ClassFName + 'TYPE ' + IntToStr(ClassesCode);
      Result := SAGAMap(OpenMap,cmd,ClassFName,euIntCode);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga curvature class  ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;


(*
Usage: saga_cmd ta_morphometry 0 [-ELEVATION <str>] [-SLOPE <str>] [-ASPECT <str>] [-C_GENE <str>] [-C_PROF <str>] [-C_PLAN <str>] [-C_TANG <str>] [-C_LONG <str>] [-C_CROS <str>] [-C_MINI <str>] [-C_MAXI <str>] [-C_TOTA <str>] [-C_ROTO <str>] [-METHOD <str>] [-UNIT_SLOPE <str>] [-UNIT_ASPECT <str>]
  -ELEVATION:<str>  	Elevation
	Grid, input
  -SLOPE:<str>      	Slope
	Grid, output
  -ASPECT:<str>     	Aspect
	Grid, output
  -C_GENE:<str>     	General Curvature
	Grid, output, optional
  -C_PROF:<str>     	Profile Curvature
	Grid, output, optional
  -C_PLAN:<str>     	Plan Curvature
	Grid, output, optional
  -C_TANG:<str>     	Tangential Curvature
	Grid, output, optional
  -C_LONG:<str>     	Longitudinal Curvature
	Grid, output, optional
  -C_CROS:<str>     	Cross-Sectional Curvature
	Grid, output, optional
  -C_MINI:<str>     	Minimal Curvature
	Grid, output, optional
  -C_MAXI:<str>     	Maximal Curvature
	Grid, output, optional
  -C_TOTA:<str>     	Total Curvature
	Grid, output, optional
  -C_ROTO:<str>     	Flow Line Curvature
	Grid, output, optional
  -METHOD:<str>     	Method
	Choice
	Available Choices:
	[0] maximum slope (Travis et al. 1975)
	[1] maximum triangle slope (Tarboton 1997)
	[2] least squares fitted plane (Horn 1981, Costa-Cabral & Burgess 1996)
	[3] 6 parameter 2nd order polynom (Evans 1979)
	[4] 6 parameter 2nd order polynom (Heerdegen & Beran 1982)
	[5] 6 parameter 2nd order polynom (Bauer, Rohdenburg, Bork 1985)
	[6] 9 parameter 2nd order polynom (Zevenbergen & Thorne 1987)
	[7] 10 parameter 3rd order polynom (Haralick 1983)
	[8] 10 parameter 3rd order polynom (Florinsky 2009)
	Default: 6
  -UNIT_SLOPE:<str> 	Unit
	Choice
	Available Choices:
	[0] radians
	[1] degree
	[2] percent rise
	Default: 0
  -UNIT_ASPECT:<str>	Unit
	Choice
	Available Choices:
	[0] radians
	[1] degree
	Default: 0
*)


function SAGA_PlanCurvature(OpenMap : boolean; InName : PathStr; SlopeMethod : char = '3'; PlanCurvatureFName : PathStr = '') : integer;
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_compound_0.html
var
   cmd : shortstring;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (PlanCurvatureFName = '') then PlanCurvatureFName := MDTempDir + 'saga_plan_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_morphometry 0 -ELEVATION ' + InName + ' -C_PLAN '  + PlanCurvatureFName + ' -METHOD ' + SlopeMethod;
      Result := SAGAMap(OpenMap,cmd,PlanCurvatureFName,euPerMeter,MDDef.DefCurveMap);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga plan curvature   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;


function SAGA_ProfileCurvature(OpenMap : boolean; InName : PathStr; SlopeMethod : char = '3'; OutName : PathStr = '') : integer;
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_compound_0.html
var
   cmd : shortstring;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (OutName = '') then OutName := MDTempDir + 'saga_profile_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_morphometry 0 -ELEVATION ' + InName + ' -C_PROF ' + OutName + ' -METHOD ' + SlopeMethod;
      Result := SAGAMap(OpenMap,cmd,OutName,euPerMeter,MDDef.DefCurveMap);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga profile curvature   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;

function SAGA_TangentialCurvature(OpenMap : boolean; InName : PathStr; SlopeMethod : char = '3'; OutName : PathStr = '') : integer;
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_compound_0.html
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_morphometry_0.html
var
   cmd : shortstring;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (OutName = '') then OutName := MDTempDir + 'saga_profile_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_morphometry 0 -ELEVATION ' + InName + ' -C_TANG ' + OutName + ' -METHOD ' + SlopeMethod;
      Result := SAGAMap(OpenMap,cmd,OutName,euPerMeter,MDDef.DefCurveMap);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga tangential curvature   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;


function SAGA_Slope_percent(OpenMap : boolean; SlopeMethod : char; InName : PathStr; SlopeFName : PathStr = '') : integer;
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_compound_0.html
//  https://saga-gis.sourceforge.io/saga_tool_doc/2.1.3/ta_morphometry_0.html
var
   cmd : shortstring;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (SlopeFName = '') then SlopeFName := MDTempDir + 'saga_slope_' + ExtractFileNameNoExt(InName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_morphometry 0 -ELEVATION ' + InName + ' -SLOPE '  + SlopeFName + ' -METHOD ' + SlopeMethod + ' -UNIT_SLOPE 2';
      Result := SAGAMap(OpenMap,cmd,SlopeFName,euPercentSlope,MDDef.DefSlopeMap);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga slope percent   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;

function SAGA_Aspect(OpenMap : boolean; InName : PathStr; AspectFName : PathStr = '') : integer;
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_compound_0.html
//  https://saga-gis.sourceforge.io/saga_tool_doc/2.1.3/ta_morphometry_0.html
var
   cmd : shortstring;
   {$If Defined(RecordSAGALS)} Stopwatch : tStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      if (AspectFName = '') then AspectFName := MDTempDir + 'saga_aspect_' + ExtractFileNameNoExt(InName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_morphometry 0 -ELEVATION ' + InName + ' -ASPECT ' + AspectFName + ' -UNIT_ASPECT 1';
      Result := SAGAMap(OpenMap,cmd,AspectFName,euAspectDeg,mtDEMAspect);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga aspect   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;


(*
function SAGA_LSFactor_FieldBased(OpenMap : boolean; InName : PathStr; LSGridName : PathStr = '') : integer;
//https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_hydrology_25.html
//this produced a blank grid
var
   cmd : shortstring;
begin
   if IsSagaCMDthere then begin
      if (LSGridName = '') then LSGridName := MDTempDir + 'saga_ls_factor_' + ExtractFileNameNoExt(InName) + '.tif';
      {$If Defined(RecordSAGALS)} Stopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_hydrology 25 -DEM ' + InName + ' -LS_FACTOR ' + LSGridName;
      {$IfDef RecordSAGA} WriteLineToDebugFile('LS_factor, catchment area ' + cmd); {$EndIf}
      Result := SAGAMap(OpenMap,cmd,LSGridName,euUndefined,OpenMap);

      //WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga ls_field based created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   end;
end;


function SAGA_LSFactor(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_hydrology_22.html
//  catchment area from wetness index, https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_hydrology_15.html


//  https://sourceforge.net/p/saga-gis/discussion/790705/thread/695ccc2180/
//  https://sourceforge.net/p/saga-gis/discussion/354013/thread/0d107bd8/
//  slope in radians,  https://gis.stackexchange.com/questions/313659/ls-factor-in-saga-slope-units

      { chain steps from https://gis.stackexchange.com/questions/419398/saga-or-grass-tools-for-rusle-ls-factor-in-qgis
      Slope, Aspect, Curvature tool  Input: DEM  Output: Slope (measured in radian)
      Fill Sink (Wang & Liu) tool  Input: DEM  Output: DEM without sink
      Catchment Area tool Input: DEM without sink  Output: Flow Accumulation (Total catchment area)
      Flow width and specific catchment area tool  Input: DEM without sink, Flow Accumulation (Total catchment area) Output: Specific catchment area
      LS-factor tool  Input: Slope, Specific catchment area  Output: LS-factor
      To speed up the process, you can try Basic terrain analysis tool, which is in the SAGA - Terrain Analysis - Morphometry group. It does all the necessary steps automatically to produce LS-factor.
       }

var
    cmd : shortstring;
    SlopeName,AreaName,TWIName : PathStr;
    {$If Defined(RecordSAGALS)} SagaStopwatch : TStopwatch; {$EndIf}
begin
   if IsSagaCMDthere then begin
      {$IfDef RecordSAGA} WriteLineToDebugFile('SAGA_LSFactor in, ' + InName); {$EndIf}
      if (OutName = '') then OutName := MDTempDir + 'saga_ls_factor_' + ExtractFileNameNoExt(InName) + '.tif';
      AreaName := MDTempDir + 'saga_catch_area_' + ExtractFileNameNoExt(InName) + '.tif';
      TWIName := MDTempDir + 'saga_twi_' + ExtractFileNameNoExt(InName) + '.tif';
      SlopeName := MDTempDir + 'saga_slope_rad_' + ExtractFileNameNoExt(InName) + '.tif';

      {$If Defined(RecordSAGALS)} SagaStopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_hydrology 15 -DEM ' + InName + ' -AREA ' + AreaName + ' TWI ' + TWIName;
      {$IfDef RecordSAGA} WriteLineToDebugFile('LS_factor, catchment area ' + cmd); {$EndIf}
      WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga TWI created   ' + RealToString(SAGAStopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}

      {$If Defined(RecordSAGALS)} SagaStopwatch := TStopwatch.StartNew; {$EndIf}
      cmd := MDDef.SagaCMD + ' ta_morphometry 0 -ELEVATION ' + InName + ' -SLOPE ' + SlopeName;
      {$IfDef RecordSAGA} WriteLineToDebugFile('LS_factor, slope, ' + cmd); {$EndIf}
      WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
      {$If Defined(RecordSAGALS)} WriteLineToDebugFile('Saga slope radians created   ' + RealToString(SAGAStopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}

      {$If Defined(RecordSAGALS)} SagaStopwatch := TStopwatch.StartNew; {$EndIf}
      CMD := MDDef.SagaCMD + ' ta_hydrology 22' + ' -SLOPE ' + SlopeName + ' -AREA ' + AreaName + ' -LS ' + OutName;
      {$IfDef RecordSAGA} WriteLineToDebugFile('LS_factor command itself, ' + cmd); {$EndIf}
      Result := SAGAMap(OpenMap,cmd,OutName,euUndefined,OpenMap);
      {$If Defined(RecordSAGALS)}  WriteLineToDebugFile('Saga LS created   ' + RealToString(SAGAStopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}

      {$If Defined(RecordSAGA) or Defined(RecordSAGA_JustResult)} WriteLineToDebugFile(ExtractFileNameNoExt(InName) + '  SAGA_LSFactor out grid=' + IntToStr(Result)); {$EndIf}
   end;
end;
*)


function SAGA_FlowAccumulationParallizeable(InName : PathStr; OutName : PathStr = '') : integer;
//https://saga-gis.sourceforge.io/saga_tool_doc/9.3.1/ta_hydrology_29.html
var
   cmd : shortstring;
begin
   if (OutName = '') then OutName := MDTempDir + 'saga_flow_accumulation_' + ExtractFileNameNoExt(InName) + '.tif';
   CMD := MDDef.SagaCMD + ' ta_hydrology 29' +
         ' -DEM ' + inName +
         ' -FLOW ' + OutName;
   Result := SAGAMap(true,cmd,OutName,euUndefined);
end;


function SAGA_WatershedBasinsWangLiu(InName : PathStr) : integer;
// https://saga-gis.sourceforge.io/saga_tool_doc/8.3.0/ta_preprocessor_4.html
 var
    cmd : shortstring;
    FilledGrid,FlowDirections,WaterShedBasins : PathStr;
 begin
    Result := 0;
    if IsSagaCMDthere and FileExists(InName) then begin
       {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaWatershedBasinsWangLiu in, InName= ' + InName); {$EndIf}
       FilledGrid      := MDTempDir + 'saga_wang_liu_filled_grid_' + ExtractFileNameNoExt(InName) + '.tif';
       FlowDirections  := MDTempDir + 'saga_wang_liu_flow_dir_' + ExtractFileNameNoExt(InName) + '.tif';
       WaterShedBasins := MDTempDir + 'saga_wang_liu_watershed_' + ExtractFileNameNoExt(InName) + '.tif';
       CMD := MDDef.SagaCMD + ' ta_preprocessor 4' +
            ' -ELEV ' + inName +
            ' -FILLED ' + FilledGrid +
            ' -FDIR ' + FlowDirections +
            ' -WSHED ' + WaterShedBasins;
       WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
       {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaWatershedBasinsWangLiu out'); {$EndIf}
       //Result := SAGAMap(OpenMap,cmd,InName,'',Undefined,false);
    end;
 end;


function SAGAedgeContaminationMap(InName : PathStr; OutName : PathStr = '') : integer;
//https://saga-gis.sourceforge.io/saga_tool_doc/2.1.3/ta_hydrology_13.html
//may not be useful, 2/1/2024
var
    cmd : shortstring;
begin
    if (OutName = '') then OutName := MDTempDir + 'saga_channel_' + ExtractFileNameNoExt(InName) + '.tif';
    CMD := MDDef.SagaCMD + ' ta_hydrology 13 -ELEVATION ' + inName + ' -CONTAMINATION ' + OutName;
    Result := SAGAMap(True,cmd,OutName,euUndefined);
end;


function SAGA_StrahlerOrderGrid(InName : PathStr; OutName : PathStr = '') : integer;
var
    cmd : shortstring;
begin
    if (OutName = '') then OutName := MDTempDir + 'saga_strahler_order_' + ExtractFileNameNoExt(InName) + '.tif';
    CMD := MDDef.SagaCMD + ' ta_channels 6 -DEM ' + inName + ' -STRAHLER ' + OutName;
    Result := SAGAMap(True,cmd,OutName,euUndefined);
end;


function SagaChannelShapefile(InName : PathStr; ChannelName : PathStr = '') : integer;
//https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_channels_0.html
//  2/26/24, both grids are empty: CHNLNTWRK and CHNLROUTE, reason we go vector
var
   cmd : shortstring;
begin
    Result := 0;
    if IsSagaCMDthere and FileExists(InName) then begin
      {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaChannelShapefile in, InName= ' + InName); {$EndIf}
       if (ChannelName = '') then ChannelName := MDTempDir + 'saga_channel_' + ExtractFileNameNoExt(InName) + '.shp';
       if not FileExists(ChannelName) then begin
          CMD := MDDef.SagaCMD + ' ta_channels 0' +
               ' -ELEVATION ' + inName +
               ' -INIT_GRID ' + inName +
               ' -CHNLNTWRK ' + MDTempDir + 'saga_channel_network_' + ExtractFileNameNoExt(InName) + '.tif' +
               //' -CHNLROUTE ' + MDTempDir + 'saga_channel_route_' + ExtractFileNameNoExt(InName) + '.tif' +
               ' -SHAPES ' + ChannelName;
          WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
          {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaChannelShapefile in, ChannelName= ' + ChannelName); {$EndIf}
       end;
       //Result := SAGAMap(OpenMap,cmd,InName,'',Undefined,false);
    end;
end;


function SagaWatershedBasins(InName : PathStr; BasinGrid : PathStr = ''; ChannelNetwork : PathStr = ''; OutName : PathStr = '') : integer;
//  https://hydrology.usu.edu/taudem/taudem5/help53/StreamReachAndWatershed.html
//  https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_channels_1.html  --attempts to use this led to problems with the CHANNELS parameter, 1/31/2024
   // issues with display of grids in both QGIS and MD
   // shapefiles work, but basins produces way too many basins
 var
    cmd : shortstring;
    OrderName,BasinShape,SegmentName : PathStr;
 begin
    Result := 0;
    if IsSagaCMDthere and FileExists(InName) then begin
       SagaChannelShapefile(InName);
       if (BasinGrid = '') then BasinGrid := MDTempDir + 'saga_basins_' + ExtractFileNameNoExt(InName) + '.tif';
       OrderName := MDTempDir + 'saga_strahler_order_' + ExtractFileNameNoExt(InName) + '.tif';
       BasinShape := MDTempDir + 'saga_basins_' + ExtractFileNameNoExt(InName) + '.shp';
       SegmentName := MDTempDir + 'saga_segments_' + ExtractFileNameNoExt(InName) + '.shp';

       CMD := MDDef.SagaCMD + ' ta_channels 5' +
            ' -DEM ' + inName +
            ' -ORDER ' + OrderName +
            ' -BASIN ' + BasinGrid +
            ' -SEGMENT ' + BasinShape +
            ' -BASINS ' + BasinShape;
       WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
       {$IfDef RecordSAGAFull} WriteLineToDebugFile('SagaChannelShapefile in, ChannelName= ' + ChannelName); {$EndIf}
    end;
    //Result := SAGAMap(OpenMap,cmd,InName,'',Undefined,false);
 end;


    function SagaChannelNetworkGrid(OpenMap : boolean; InName : PathStr; OutGridName : PathStr = '') : integer;
    //https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_channels_0.html
    var
       cmd : shortstring;
    begin
       if (OutGridName = '') then OutGridName := MDTempDir + 'saga_channel_' + ExtractFileNameNoExt(InName) + '.tif';
       //if (ShpName = '') then ShpName := MDTempDir + 'saga_channel_' + ExtractFileNameNoExt(InName) + '.shp';
       CMD := MDDef.SagaCMD + ' ta_channels "Channel Network" -ELEVATION ' + inName + ' -INIT_GRID ' + inName + ' -CHNLNTWRK ' + OutGridName ;
       Result := SAGAMap(OpenMap,cmd,OutGridName,euUndefined);
       //Result := 0;
    end;


    function SagaSinkRemoval(InName: PathStr; OutName : PathStr = '') : integer;
    var
       cmd : shortstring;
    begin
       if IsSagaCMDthere then begin
          {$IfDef RecordSAGA} WriteLineToDebugFile('SagaSinkRemoval in, InName= ' + InName); {$EndIf}
          if (OutName = '') then OutName := MDTempDir + 'saga_sinks_removed_' + ExtractFileNameNoExt(InName) + '.tif';
          CMD := MDDef.SagaCMD + ' ta_preprocessor "Sink Removal" -DEM ' + inName + ' -DEM_PREPROC ' + OutName;
          //TemporaryNewGeotiff := false;
          Result := SAGAMap(False,cmd,{InName,}outname,euMeters);
          {$IfDef RecordSAGA} WriteLineToDebugFile('SagaSinkRemoval out'); {$EndIf}
       end
       else Result := 0;;
    end;


    procedure SAGA_all_DEMs_remove_sinks;
    var
       i : integer;
       InName,OutName : PathStr;
    begin
        {$IfDef RecordSAGA} WriteLineToDebugFile('SagaSinkRemoval in, InName= ' + InName); {$EndIf}
        if IsSagaCMDthere then begin
           for i := 1 to MaxDEMDataSets do begin
             if ValidDEM(i) then begin
                InName := DEMGlb[i].SelectionMap.GeotiffDEMNameOfMap;
                OutName := ChangeFileExt(InName,'_no_sink.tif');
                SagaSinkRemoval(InName,OutName);
                GDALConvertImagesToGeotiff(OutName,true);
             end;
          end;
        end;
    end;


    function SagaTRIMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
    var
       cmd : shortstring;
    begin
       if OutName = '' then OutName := MDTempDir + 'saga_tri_' + ExtractFileNameNoExt(InName) + '.tif';
       CMD := MDDef.SagaCMD + ' ta_morphometry 16 -DEM ' + inName + ' -TRI ' + OutName + ' -MODE 0';
       Result := SAGAMap(OpenMap,cmd,outname,euMeters);
    end;


    function SagaTPIMap(OpenMap : boolean; InName : PathStr) : integer;
    var
       cmd : shortstring;
       OutName : shortstring;
    begin
       OutName := MDTempDir + 'saga_tpi_' + ExtractFileNameNoExt(InName) + '.tif';
       CMD := MDDef.SagaCMD + ' ta_morphometry 18 -DEM ' + inName + ' -TPI ' + OutName;
       Result := SAGAMap(OpenMap,cmd,outname,euMeters);
    end;


    function SagaVectorRuggednessMap(OpenMap : boolean; InName : PathStr; Radius : integer) : integer;
    var
       cmd : shortstring;
       OutName : shortstring;
    begin
       OutName := MDTempDir + 'saga_vrm_rad_' + FilterSizeStr(Radius) + '_' + ExtractFileNameNoExt(InName) + '.tif';
       CMD := MDDef.SagaCMD + ' ta_morphometry 17 -DEM ' + inName + ' -VRM ' + OutName + ' -RADIUS ' + IntToStr(Radius);
       Result := SAGAMap(OpenMap,cmd,outname,euUndefined);
    end;

(*
SAGA_valley_depth
//https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_channels_7.html
 Usage: saga_cmd ta_channels 7 [-ELEVATION <str>] [-VALLEY_DEPTH <str>] [-RIDGE_LEVEL <str>] [-THRESHOLD <double>] [-MAXITER <num>] [-NOUNDERGROUND <str>] [-ORDER <num>]
  -ELEVATION:<str>    	Elevation
	Grid, input
  -VALLEY_DEPTH:<str> 	Valley Depth
	Grid, output
  -RIDGE_LEVEL:<str>  	Ridge Level
	Grid, output, optional
  -THRESHOLD:<double> 	Tension Threshold
	Floating point
	Minimum: 0.000000
	Default: 1.000000
  -MAXITER:<num>      	Maximum Iterations
	Integer
	Minimum: 0
	Default: 0
  -NOUNDERGROUND:<str>	Keep Ridge Level above Surface
	Boolean
	Default: 1
  -ORDER:<num>        	Ridge Detection Threshold
	Integer
	Minimum: 1
	Maximum: 7
	Default: 4


SAGA_Convergence Index
https://saga-gis.sourceforge.io/saga_tool_doc/8.2.1/ta_morphometry_1.html
Usage: saga_cmd ta_morphometry 1 [-ELEVATION <str>] [-RESULT <str>] [-METHOD <str>] [-NEIGHBOURS <str>]
  -ELEVATION:<str> 	Elevation
	Grid, input
  -RESULT:<str>    	Convergence Index
	Grid, output
  -METHOD:<str>    	Method
	Choice
	Available Choices:
	[0] Aspect
	[1] Gradient
	Default: 0
  -NEIGHBOURS:<str>	Gradient Calculation
	Choice
	Available Choices:
	[0] 2 x 2
	[1] 3 x 3
	Default: 0
*)

