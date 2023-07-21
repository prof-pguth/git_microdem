unit dragon_plot_init;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}



{$I Nevadia_defines.inc}


{$IfDef RecordProblems}
   {$IfDef Debug}
      //{$Define RecordDP}
      //{$Define CheckAccessibility}
   {$EndIf}
{$EndIf}


interface

uses
  Windows, Messages,  iniFiles,StrUtils,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, Db,ExtCtrls, StdCtrls,
  {$IfDef ExRegisterView}
  {$Else}
     dp_control,
  {$EndIf}
  DEMDefs, PETMar, Petmar_ini_file, Petmar_types;

const
   MaxInstances = 4;

type
   tDragonPlot2Defaults = record
       NegLightningSymbol,
       PosLightningSymbol,AccuracySymbol,TowerSymbol,
       VisibleSymbol,MaskedSymbol,ResultSymbol : tFullSymbolDeclaration;

       PauseGPSDuringShot,
       ShowLOS,
       TargetBlowupMaps,
       PlotAccuracy,
       FireAccessMap,
       VisDefPlot,
       TerrainShadows,
       AdvancedOptions : boolean;

       ViewDepth,
       MaxPitch,MinPitch,
       CustomVFOV,CustomHFOV,
       KMLdelay,
       DPInstances,
       MapLevelIndex,
       FireAccessMapSize,
       FireAccessMapBlowUp,
       ShotLineWidth,
       TargetMapHeight,
       TargetMapWidth,
       TargetMapType : Integer;
       DefaultDEMname,
       DefaultDRGname,
       TowerFileName : PathStr;
       DefaultTowerName : array[1..MaxInstances] of string35;
       ShotLineColor : tPlatformColor;
       AzAccuracy,PitchAccuracy : float64;
       PanoramaPitch : float64;
       BlowUpSizeAspect : Float64;
       LightningWWW : shortstring;
   end;

{$IfDef AllowOldDPOptions}
   procedure UnCustomizeMDDefaultsForDragonPlot;
{$EndIf}

procedure ProcessDragonPlotIniFile(iniWhat : tIniWhat; SectionRestoreDefaults : ShortString = '');
procedure StartDragonPlot;
procedure LoadTheInstances;
function DPCaption : shortstring;
function IsThisDP : boolean;

var
  DragonPlotDef : tDragonPlot2Defaults;
  LightningDirectory,ArchiveDirectory : PathStr;
  RegionalDEM,
  LightningTable : integer;
  TowerTable : integer;
  MergeList : tStringList;

{$IfDef ExRegisterView}
{$Else}
var
   DragonPlotForm : array[1..MaxInstances] of TDragonPlotForm;
{$EndIf}



implementation

uses
   DEMDef_routines,
   DEMDatabase,
   DEM_manager,
   DEMCoord,
   DEM_PLSS,
   Nevadia_main;


function DPCaption : shortstring;
begin
   DPCaption := EXENameWithBuild + '  (c) GeodesyBase -- Powered by MICRODEM (c) PETMAR Trilobites';
end;

function IsThisDP : boolean;
begin
   Result := (UpperCase(ptTrim(ParamStr(1))) = '-DP') or
              StrUtils.AnsiContainsText(Application.ExeName,'DP') or
              StrUtils.AnsiContainsText(Application.ExeName,'DragonPlot');
end;


{$IfDef AllowOldDPOptions}

   procedure UnCustomizeMDDefaultsForDragonPlot;
   begin
      {$IfDef RecordDP} WriteLineToDebugFile('UnCustomizeMDDefaults in'); {$EndIf}
      SetGeologyOptions(true);
      SetTigerDefaults;
      //MDDef.TigertoCDS := true;
      MDDef.ShowConversionAndAnalyze := true;
      MDDef.EnglishDistanceUnits := disMetric;
      MDDef.DBMinimizeOnOpen := false;
      MDDef.UseMapPanButtons := true;
      MDdef.ShowMainToolbar := true;
   end;

{$EndIf}



procedure SetUpDPsettings;
begin
   WMDEM.StatusBar1.Panels[1].Width := 360;
   WMDEM.StatusBar1.Panels[2].Width := 300;
   WMDEM.StatusBar1.Panels[3].Width := 300;
   WMDEM.ProgramClosing := false;

   ProcessDragonPlotIniFile(iniRead);
   {$IfDef AllowOldDPOptions}
      if (MDDef.ProgramOption = DragonPlotProgram) and (not DragonPlotDef.AdvancedOptions) then begin
         CustomizeMDDefaultsForDragonPlot;
         MDDef.KeyLocationSymbol := DragonPlotDef.ResultSymbol;
      end;
   {$EndIf}
   {$IfDef RecordDP} WriteLineToDebugFile('MainMapData=' + MainMapData); {$EndIf}
   {$IfDef RecordDP} WriteLineToDebugFile('Gaz=' + LastGazFile); {$EndIf}
   {$IfDef RecordDP} PLSStoDebugFile; {$EndIf}
   {$If Defined(CheckAccessibility)} WriteLineToDebugFile('DragonPlotDef.FireAccessMap=' + TrueOrFalse(DragonPlotDef.FireAccessMap)); {$EndIf}

   WantShowProgress := false;
   LightningDirectory := MainMapData + 'DragonPlot_lightning\';
   SafeMakeDir(LightningDirectory);
   ArchiveDirectory := MainMapData + 'DragonPlot_archive\';
   SafeMakeDir(ArchiveDirectory);
   SaveViewshedDir := MainMapData + 'DragonPlot_viewshed\';
   SafeMakeDir(SaveViewshedDir);

   if (DragonPlotDef.TowerFileName = '') then DragonPlotDef.TowerFileName := ProgramRootDir + 'towers.dbf';

   if not FileExists(DragonPlotDef.TowerFileName) then begin
      DragonPlotDef.TowerFileName := ProgramRootDir;
      Petmar.GetFileFromDirectory('Towers','*.dbf',DragonPlotDef.TowerFileName);
   end;

   CurrentProject := ProjectDir;
   FormatSettings.ShortDateFormat := 'yyyy/mm/dd';
   {$IfDef RecordDP} WriteLineToDebugFile('SetUpDPsettings out'); {$EndIf}
end;


procedure StartDragonPlot;
begin
   SetUpDPsettings;
   StopSplashing;
   LoadDEM;
   if (DragonPlotDef.TargetMapType = 1) then LoadSatelliteImage;
   LoadTheInstances;
end;


procedure LoadTheInstances;
var
   i,Top : integer;
begin
   {$IfDef RecordDP} WriteLineToDebugFile('LoadTheInstances in, n='  + IntToStr(DragonPlotDef.DPInstances)); {$EndIf}
   Top := 0;
   for i := DragonPlotDef.DPInstances downto 1 do begin
      {$IfDef RecordDP} WriteLineToDebugFile('StartDragonPlot, instance=' + IntToStr(i)); {$EndIf}
      if (DragonPlotForm[i] = Nil) then begin
         {$IfDef RecordDP} WriteLineToDebugFile('Try creating, i='  + IntToStr(i)); {$EndIf}
         DragonPlotForm[i] := TDragonPlotForm.Create(Application);
         DragonPlotForm[i].SetUpInstance(i);
      end;
      DragonPlotForm[i].Top := Top;
      inc(Top,40);
   end;
   wmDEM.SetMenusForVersion;
   {$IfDef RecordDP} WriteLineToDebugFile('LoadTheInstances out'); {$EndIf}
end;


procedure ProcessDragonPlotIniFile(iniWhat : tIniWhat; SectionRestoreDefaults : ShortString = '');
var
   MDIniFile : tMDiniFile;
   I : integer;

   procedure ProcessMainDp;
   begin
      with MDIniFile,DragonPlotDef do begin
          AParameter('DragonPlot','ViewDepth', ViewDepth,25000);
          AParameter('DragonPlot','MaxPitch', MaxPitch,20) ;
          AParameter('DragonPlot','MinPitch', MinPitch,-25) ;
          AParameter('DragonPlot','CustomVFOV', DragonPlotDef.CustomVFOV,9) ;
          AParameter('DragonPlot','CustomHFOV', CustomHFOV,12) ;
          AParameter('DragonPlot','MapLevelIndex', MapLevelIndex,1) ;
          AParameter('DragonPlot','FireAccessMapSize', FireAccessMapSize,350) ;
          AParameter('DragonPlot','FireAccessMapBlowup', FireAccessMapBlowup,250) ;
          AParameter('DragonPlot','TerrainShadows', TerrainShadows,false);
          AParameter('DragonPlot','TargetMapType', TargetMapType,0) ;
          AParameter('DragonPlot','TargetMapHeight', TargetMapHeight,700) ;
          AParameter('DragonPlot','TargetMapWidth', TargetMapWidth,700) ;
          AColorParameter('DragonPlot','ShotLineColor', ShotLineColor,claRed);
          AParameter('DragonPlot','ShotLineWidth', ShotLineWidth,2);
          AParameter('DragonPlot','PauseGPSDuringShot',PauseGPSDuringShot,true);
          AParameter('DragonPlot','ShowLOS',ShowLOS,false);
          AParameter('DragonPlot','TargetBlowupMaps',TargetBlowupMaps,true);
          AParameter('DragonPlot','PlotAccuracy',PlotAccuracy,false);
          AParameter('DragonPlot','FireAccessMap',FireAccessMap,false);
          AParameter('DragonPlot','AdvancedOptions',AdvancedOptions,false);
          AParameter('DragonPlot','DPInstances',DPInstances,1);
          AParameter('DragonPlot','KMLdelay',KMLdelay,0);
          AParameterFloat('DragonPlot','AzAccuracy',AzAccuracy,0.03);
          AParameterFloat('DragonPlot','PitchAccuracy',PitchAccuracy,0.03);
          AParameterFloat('DragonPlot','PanoramaPitch',PanoramaPitch,0);
          ASymbol('DragonPlot','PosLightningSymbol',PosLightningSymbol,Ex,claLime,3);
          ASymbol('DragonPlot','NegLightningSymbol',NegLightningSymbol,Ex,claRed,3);
          ASymbol('DragonPlot','VisibleSymbol',VisibleSymbol, FilledDiamond,claBlack,3);
          ASymbol('DragonPlot','MaskedSymbol',MaskedSymbol,Diamond,claBlack,3);
          ASymbol('DragonPlot','AccuracySymbol',AccuracySymbol,Diamond,claBlack,2);
          ASymbol('DragonPlot','TowerSymbol',TowerSymbol,FilledUpTri,claBlack,3);
          ASymbol('DragonPlot','ResultSymbol',ResultSymbol, FilledCircle,claBlue,4);
          AParameter('DragonPlot','LightningWWW',LightningWWW, 'https://www.earthnetworks.com');
       end;
   end;

   procedure ProcessMDforDp;
   begin
      with MDIniFile,DragonPlotDef do begin
          AColorParameter('MDforDP','MDdef.TigrDef.MajorRoadColor',MDdef.TigrDef.MajorRoadColor,RGBtrip(128,128,64));
          AColorParameter('MDforDP','MDdef.TigrDef.RoadCat2Color', MDdef.TigrDef.RoadCat2Color, RGBtrip(128,128,64));
          AColorParameter('MDforDP','MDdef.TigrDef.RoadCat3Color',MDdef.TigrDef.RoadCat3Color,RGBtrip(128,128,64));
          AColorParameter('MDforDP','MDdef.TigrDef.RoadCat4Color',MDdef.TigrDef.RoadCat4Color,RGBtrip(128,128,64));
          AColorParameter('MDforDP','MDdef.TigrDef.RoadCat5Color',MDdef.TigrDef.RoadCat5Color,RGBtrip(128,128,64));
          AColorParameter('MDforDP','MDdef.TigrDef.RoadCat6Color',MDdef.TigrDef.RoadCat6Color,RGBtrip(128,128,64));
          AColorParameter('MDforDP','MDdef.TigrDef.RoadCat7Color',MDdef.TigrDef.RoadCat7Color,RGBtrip(128,128,64));
          AColorParameter('MDforDP','MDdef.TigrDef.WaterColor1',MDdef.TigrDef.WaterColor1,RGBtrip(0,255,255));

          AParameter('MDforDP','MDdef.TigrDef.MajorRoadWidth', MDdef.TigrDef.MajorRoadWidth ,3 );
          AParameter('MDforDP','MDdef.TigrDef.WaterWidth1',MDdef.TigrDef.WaterWidth1,1);
          AParameter('MDforDP','MDdef.TigrDef.DrawRailroad', MDdef.TigrDef.DrawRailroad, false);
          AParameter('MDforDP','MDdef.TigrDef.DrawPipeline', MDdef.TigrDef.DrawPipeline, false);
          AParameter('MDforDP','MDdef.TigrDef.MaxAutoTigerCounties', MDdef.TigrDef.MaxAutoTigerCounties,500);
          AParameter('MDforDP','MDdef.TigrDef.AppearMajorRoad', MDdef.TigrDef.AppearMajorRoad,500);
          AParameter('MDforDP','MDdef.TigrDef.AppearRoadCat3', MDdef.TigrDef.AppearRoadCat3,500);
          AParameter('MDforDP','MDDef.TigrDef.AutoShowTigerOnDEMs', MDDef.TigrDef.AutoTigerOnDEMs, true);
          AParameter('MDforDP','MDdef.TigrDef.MaxAutoTigerCounties', MDdef.TigrDef.MaxAutoTigerCounties,500);
          AParameter('MDforDP','MDdef.TigrDef.MaxAutoTigerCounties', MDdef.TigrDef.MaxAutoTigerCounties,500);
          AParameter('MDforDP','MDdef.TigrDef.MaxAutoTigerCounties', MDdef.TigrDef.MaxAutoTigerCounties,500);

          AParameter('MDforDP','MDDef.DoReqFlyHigh', MDDef.DoReqFlyHigh, false);
          AParameter('MDforDP','MDDef.DrawLOS', MDDef.DrawLOS, false);
          //AParameter('MDforDP','MDDef.TigertoCDS', MDDef.TigertoCDS, false);
          AParameter('MDforDP','MDDef.ShowConversionAndAnalyze', MDDef.ShowConversionAndAnalyze, false);
          AParameter('MDforDP','MDDef.UseMapPanButtons', MDDef.UseMapPanButtons, false);
          AParameter('MDforDP','MDdef.ShowMainToolbar', MDdef.ShowMainToolbar, false);
          AParameter('MDforDP','MDDef.ShowPLSS', MDDef.ShowPLSS, true);
          AParameter('MDforDP','MDdef.DualElevs', MDdef.DualElevs, true);
          AParameter('MDforDP','MDDef.DBMinimizeOnOpen', MDDef.DBMinimizeOnOpen, true);
          AParameter('MDforDP','MDDef.DefaultContourInterval', MDDef.DefaultContourInterval,10);
          AParameter('MDforDP','MDDef.IndexContWidth', MDDef.IndexContWidth, 2);
          AParameter('MapGrid','MDDef.MapTicks',MDDef.MapTicks,tixNone);
          AParameterShortFloat('MDforDP','MDdef.MaskObsRange',MDdef.MaskObsRange,50000);    //2500
          AParameterShortFloat('MDforDP','MDDef.BlowUpLatSize',MDDef.BlowUpLatSize,0.05);
          AParameterShortFloat('MDforDP','MDDef.BlowUpLongSize', MDDef.BlowUpLongSize,1.5 * 0.05);

          if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','OutPutLatLongMethod',ord(MDDef.OutPutLatLongMethod));
          if (IniWhat = iniRead) then MDDef.OutPutLatLongMethod := tLatLongMethod(IniFile.ReadInteger('Display','OutPutLatLongMethod',ord(DecSeconds)));
          if (iniWhat = iniInit) then MDDef.OutPutLatLongMethod := DecSeconds;

         if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','OutputAzimuthMethod',ord(MDDef.OutputAzimuthMethod));
         if (IniWhat = iniRead) then MDDef.OutputAzimuthMethod := tLatLongMethod(IniFile.ReadInteger('Display','OutputAzimuthMethod',ord(DecMinutes)));
         if (iniWhat = iniInit) then MDDef.OutputAzimuthMethod := DecMinutes;

         if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','EnglishDistanceUnits',ord(MDDef.EnglishDistanceUnits));
         if (IniWhat = iniRead) then MDDef.EnglishDistanceUnits := tDistanceUnits(IniFile.ReadInteger('Display','EnglishDistanceUnits',ord(disEnglish)));
         if (iniWhat = iniInit) then MDDef.EnglishDistanceUnits := disEnglish;

         if (IniWhat = iniWrite) then IniFile.WriteInteger('Contour','ContourColors',ord(MDDef.ContourColors));
         if (IniWhat = iniRead) then MDDef.ContourColors := tContourColors(IniFile.ReadInteger('Contour','ContourColors',ord(ccSingle)));
         if (iniWhat = iniInit) then MDDef.ContourColors := ccSingle;
      end;
   end;


begin
   MDIniFile := tMDiniFile.OpenMDiniFile(iniWhat,SectionRestoreDefaults);
   ProcessMainDp;
   if (iniWhat in [iniRead,iniWrite]) then begin
       with MDIniFile,DragonPlotDef do begin
          AParameter('DragonPlotFiles','TowerFileName',DragonPlotDef.TowerFileName,'');
          AParameter('DragonPlotFiles','DefaultDEMName',DragonPlotDef.DefaultDEMName,'');
          AParameter('DragonPlotFiles','DefaultDRGName',DragonPlotDef.DefaultDRGName,'');
          for i := 1 to MaxInstances do begin
             AParameter('DragonPlotFiles','DefaultTowerName' + IntToStr(i),DragonPlotDef.DefaultTowerName[i],'');
          end;
       end;
   end;
   ProcessMDforDp;
   MDiniFile.CloseMDiniFile;
end;



end.
