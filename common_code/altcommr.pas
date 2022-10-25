unit AltCommR;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


//http://podaac.jpl.nasa.gov/dataset/PODAAC_MGDR

{$I nevadia_defines.inc}
 
interface

uses
   System.Classes,
   Petmar_types,PETMAR,DEMMapF;

{$A-}

const
   MaxInt16 = MaxSmallInt;

type
   Byte18 =  array[1..18] of byte;
   MGDRRecordTypeVersionB = packed record
     {time group}
      Tim_Moy_1 : Int16;
      TimeGroup : Byte18;
     {location group}
      LatI,LongI  : Int32;
     {Altitude group}
      Sat_Alt,HP_Sat : Int32;
      AltitudeGroup : array[1..40] of byte;
     {Attitude group}
      AttitudeGroup : array[1..2] of byte;
     {altimeter range group}
      H_Alt         : Int32;
      AltimeterRangeGroup : array[1..32] of byte;
     {environmental correction group}
      Dry_Corr,Dry1_Corr,Dry2_Corr,
      Inv_Bar,Wet_Corr,
      Wet1_Corr,Wet2_Corr,
      Wet_H_Rad,Iono_Corr,
      Iono_Dor,Iono_Ben      : Int16;
     {significant wave height and backscatter coefficient group}
      SWH_K,SWH_C : Int16;
      WindAndWaveGroup : array[1..9] of byte;
      EM_Bias_Corr_K1,
      EM_Bias_Corr_K2   : Int16;
      WindAndWaveGroup2 : array[1..19] of byte;
     {Geophysical quantity group}
      H_MSS, H_Geo   : Int32;
      H_EOT_CR, H_EOT_SCH,H_LT_SCH,H_Set : Int16;
      H_Pol,Wind_Sp : byte;
      H_Ocs         : Int16;
     {TMR brightness temperature group}
      Tb18,Tb21,Tb37 : Int16;
     {flags group}
      AltOn          : byte;
      FlagsGroup : array[1..13] of byte;
      Alt_Bad_1,Alt_Bad_2 : byte;
      FlagsGroup1 : array[1..9] of byte;
      Geo_Bad_1,Geo_Bad_2 : byte;
      FlagsGroup2 : array[1..3] of byte;
   end;

   tAltParam = (CorrectedSeaSurface,MediumWavelengthAnomaly,DynamicTopography,TOPEXAverageSeaSurface,DifferencePrelaunchMeanSeaSurface,
       DifferencePrelaunchGeoid,WaterDepth,WaveHeight,WindSpeed,BrightnessTemps,DifferenceTOPEXMSS);

const
   MaxRecords = 1200;
   MaxProfLength = 1000;
   NumTopexCycles = 200;
   NumTopexOrbits = 254;
type
   tVersionBMGDRRecs  = array[1..MaxRecords] of MGDRRecordTypeVersionB;
   OneProfileType = array[0..MaxProfLength] of float32;
var
   GeoidGrid      : array[0..35,-9..9] of Int16;
   AltParam       : tAltParam;
   LocProf,GeoidProf,StdDevProf : ^OneProfileType;


procedure OpenMGDRFile(Infile : PathStr; var GFile : file);
procedure ReadRecords(var VersionBMGDRRecs : MGDRRecordTypeVersionB; var NumRead : int32);
function TopexFlagsOK(MGDRRec : MGDRRecordTypeVersionB) : boolean;
procedure RecordCorrections(Param  : tAltParam; MGDRRec : MGDRRecordTypeVersionB; var H : float64);
procedure ImportTopexCD(MapOwner : tMapForm);


implementation


uses
   SysUtils,
   PETMath,
   BaseMap,
   PetDButils,
   Nevadia_main;

var
   GFile : file;
   ShortRecordsUsed : boolean;



procedure ImportTopexCD(MapOwner : tMapForm);
var
   i       : int32;
   Output,
   Files   : tStringList;
   fName   : PathStr;
   DefFilter : byte;

      procedure PlotSingleFile(Infile : String);
      const
         TopexEquatorCrossings : array[1..NumTopexOrbits] of byte =(36,94,26,84,16,74,6,64,123,54,113,44,103,34,93,
             24,83,14,73,4,63,121,53,111,43,101,33,91,23,81,13,71,3,61,120,51,110,41,100,31,90,21,80,11,70,1,60,118,
             50,108,40,98,30,88,20,78,10,68,127,58,117,48,107,38,97,28,87,18,77,8,67,125,57,115,47,105,37,95,27,85,
             17,75,7,65,124,55,114,45,104,35,94,25,84,15,74,5,64,122,54,112,44,102,34,92,24,82,14,72,4,62,121,52,111,
             42,101,32,91,22,81,12,71,2,61,119,51,109,41,99,31,89,21,79,11,69,1,59,118,49,108,39,98,29,88,19,78,9,68,
             126,58,116,48,106,38,96,28,86,18,76,8,66,125,56,115,46,105,36,95,26,85,16,75,6,65,123,55,113,45,103,35,
             93,25,83,15,73,5,63,122,53,112,43,102,33,92,23,82,13,72,3,62,120,52,110,42,100,32,90,22,80,12,70,2,60,
             119,50,109,40,99,30,89,20,79,10,69,127,59,117,49,107,39,97,29,87,19,77,9,67,126,57,116,47,106,37,96,27,
             86,17,76,7,66,124,56,114,46,104);
      var
         Lat,Long,ht,dyn,Dist,LastLat,LastLong,Segment,Bearing : float64;
         i,j,x,y,RecsRead,Orbit  : int32;
         hstr,wave_Ht_K, Wave_Ht_C,Wind_Speed,DStr,DynStr,EQStr : shortstring;
         TStr   : ShortString;
         VersionBMGDRRecs : ^tVersionBMGDRRecs;
         gFile : file;
      begin
         {$IfDef RecordAltimeterProblems} WriteLineToDebugFile('TMapForm.PlotSingleFile for altimeter ' + Infile); {$EndIf}
         TStr := Copy(ExtractFileExt(Infile),2,3);
         while TStr[1] = '0' do Delete(TStr,1,1);
         Orbit := StrToInt(TStr);
         OpenMGDRFile(Infile,Gfile);
         New(VersionBMGDRRecs);
         j := 0;
         repeat
             BlockRead(GFile,VersionBMGDRRecs^,MaxRecords,RecsRead);
             for i := 1 to RecsRead do begin
                Lat := 0.000001 * VersionBMGDRRecs^[i].LatI;
                Long := 0.000001 * VersionBMGDRRecs^[i].LongI;
                while (Long > 180) do Long := Long - 360;
                inc(j);
                if (j = 1) then Dist := 0
                else begin
                   VincentyCalculateDistanceBearing(Lat,Long,LastLat,LastLong,segment,bearing);
                   Dist := Dist + 0.001 * segment;
                end;
                MapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                if MapOwner.MapDraw.OnScreen(x,y) then begin
                   if (OutPut <> Nil) then begin
                      if TopexFlagsOK(VersionBMGDRRecs^[i]) and (VersionBMGDRRecs^[i].H_Ocs < 32766) then begin
                         RecordCorrections(MediumWavelengthAnomaly,VersionBMGDRRecs^[i],Ht);
                         hstr := RealToString(0.001 * Ht,-12,3);
                         wave_Ht_k := RealToString(0.01 * VersionBMGDRRecs^[i].SWH_K,-8,1);
                         wave_Ht_C := RealToString(0.01 * VersionBMGDRRecs^[i].SWH_C,-8,1);
                         Wind_Speed := RealToString(0.1 * VersionBMGDRRecs^[i].Wind_Sp,-8,1);
                         RecordCorrections(DynamicTopography,VersionBMGDRRecs^[i],dyn);
                         dynStr := RealToString(0.001 * dyn,-8,3);
                         Dstr := RealToString(VersionBMGDRRecs^[i].H_Ocs,-12,-3);
                         if Odd(Orbit) then EqStr := IntToStr(TopexEquatorCrossings[Orbit]) + ',-9999'
                         else EqStr :=  '-9999,' + IntToStr(TopexEquatorCrossings[Orbit]);
                         Output.Add( RealToString(Lat,-12,-8) + ',' + RealToString(Long,-14,-8) + ',' + TStr + ',' + hStr + ',' + DStr + ',' +
                             wave_Ht_K + ',' + wave_Ht_C + ',' + Wind_Speed  + ',' + dynStr + ',' +  EqStr + ',' + RealToString(Dist,-12,2));
                      end;
                   end;
                end;
                LastLat := Lat;
                LastLong := Long;
             end {for i};
         until (RecsRead = 0);
         CloseFile(gFile);
         Dispose(VersionBMGDRRecs);
      end;


begin
   {$IfDef RecordAltimeterProblems}  WriteLineToDebugFile('TMapForm.All1Click for altimeter in ' + AltimeterDataPath); {$EndIf}
   repeat
      try
         ShowHourglassCursor;
         DefFilter := 1;
         Files := tStringList.Create;
         Petmar.GetMultipleFiles('Altimeter','*.*',Files,DefFilter);
         fName := ExtractFilePath(Files.Strings[0]) + '_altimeter.dbf';
         Output := tStringList.Create;
         Output.Add('LAT,LONG,ORBIT,GEOID_ANOM,ELEV_M,SWH_K,SWH_C,WIND_SP,DYN_TOPO,ASC_EQ_X,DES_EQ_X,DIST_KM');
         StartProgress('TOPEX');
         for i := 0 to pred(Files.Count) do begin
            UpdateProgressBar(i/Files.Count);
            PlotSingleFile(Files.Strings[i]);
         end;
         EndProgress;
         if (Output <> Nil) then begin
            Output.SaveToFile(MDTempDir + 'alt.txt');
            MapOwner.StringListToLoadedDatabase(Output,fName);
         end;
      finally
         ShowDefaultCursor;
         Files.Free;
      end;
   until not AnswerIsYes('Another CD');
end;



function GrossGeoidHeight(Lat,Long : float64) : float64;
var
   XLo,YLo,XHi : integer;
   xd,yd,cm    : float64;
begin
   if Long < 0 then Long := Long + 360;
   Lat := Lat * 0.1;
   Long := Long * 0.1;
   XLo := Trunc(Long);
   if XLo = 36 then XLo := 0;
   XHi := succ(XLo);
   if XHi = 36 then XHi := 0;
   if Lat < 0 then YLo := pred(Trunc(Lat)) else YLo := Trunc(Lat);
   xd  := Frac(Long);
   if Lat < 0 then yd := Lat - YLo else yd  := Frac(Lat);
   cm :=  (1 - xd) * (1 - yd) * GeoidGrid[XLo,YLo] + (1 - xd) * yd * GeoidGrid[XLo,succ(YLo)] + xd * (1 - yd) * GeoidGrid[XHi,YLo] + xd * yd * GeoidGrid[XHi,succ(YLo)];
   GrossGeoidHeight := 100 * cm;
end;


function TopexFlagsOK(MGDRRec : MGDRRecordTypeVersionB) : boolean;
begin
   {first relaxation: shallow water and rain allowed}
   with MGDRRec do
      TopexFlagsOK := (H_MSS < 2147483647) and (H_Alt < 2147483647) and (Geo_Bad_1 in [0,1]) and (Geo_Bad_2 in [0,1]) and (Alt_Bad_1 = 0) and (Alt_Bad_2 = 0);
end;


procedure RecordCorrections(Param : tAltParam; MGDRRec : MGDRRecordTypeVersionB; var H : float64);
var
   SeaSurfaceHeight,TideEffects,CorrectedRange  : Int32;
   Lat,Long : float64;
begin
    with MGDRRec do begin
       if Param = BrightnessTemps then H := 0.01 * Tb18
       else if Param = WaveHeight then H := SWH_K
       else if Param = WindSpeed then H := Wind_Sp
       else if Param = WaterDepth then H := H_Ocs
       else begin
          if Dry_Corr = MaxInt16 then Dry_Corr := 0;
          if Iono_Dor = MaxInt16 then Iono_Dor := 0;
          if Wet_H_Rad = MaxInt16 then Wet_H_Rad := 0;
          if EM_Bias_Corr_K1 = MaxInt16 then EM_Bias_Corr_K1 := EM_Bias_Corr_K2;
          if H_Set = MaxInt16 then H_Set := 0;
          if H_Pol = 127 then H_Pol := 0;
          if Inv_Bar = MaxInt16 then Inv_Bar := 0;
          CorrectedRange := H_Alt + Wet_H_Rad + Dry_Corr + Iono_Dor + EM_Bias_Corr_K1;
          SeaSurfaceHeight := Sat_Alt - CorrectedRange;
          if Param = CorrectedSeaSurface then H := SeaSurfaceHeight
          else begin
             if H_EOT_SCH < MaxInt16 then TideEffects := H_EOT_SCH + H_Set + H_Pol
             else if H_EOT_CR < MaxInt16 then TideEffects := H_EOT_CR + H_Set + H_Pol
             else TideEffects := H_Set + H_Pol;
             if Param = MediumWavelengthAnomaly then begin
                Lat :=  0.000001 * LatI;
                Long := 0.000001 * LongI;
                H := SeaSurfaceHeight-10*GrossGeoidHeight(Lat,Long) - TideEffects + Inv_Bar;
             end;
             if(Param = DifferencePrelaunchMeanSeaSurface) then H := SeaSurfaceHeight - H_Geo - TideEffects + Inv_Bar;
             if (Param in [DynamicTopography,DifferencePreLaunchGeoid]) then H := SeaSurfaceHeight - H_MSS - TideEffects + Inv_Bar;
          end;
       end;
    end;
end;


procedure OpenMGDRFile(Infile : PathStr; var GFile : file);
var
   VersionBMGDRRecs : ^tVersionBMGDRRecs;
begin
   InsureFileIsNotReadOnly(InFile);
   assignFile(GFile,Infile);
   New(VersionBMGDRRecs);
   reset(GFile,SizeOf(MGDRRecordTypeVersionB));
   ShortRecordsUsed := false;
   BlockRead(GFile,VersionBMGDRRecs^,33);  {skip header}
   Dispose(VersionBMGDRRecs);
end;


procedure CloseMGDRFile;
begin
   close(Gfile);
end;

procedure ReadRecords(var VersionBMGDRRecs : MGDRRecordTypeVersionB; var NumRead : int32);
begin
   BlockRead(GFile,VersionBMGDRRecs,MaxRecords,NumRead);
end;



procedure AltimeterInitialization;
var
   GeoidFile : file;
   fName     : PathStr;
begin
   fName := ProgramRootDir + 'GEOID.DEM';
   if FileExists(fName) then begin
      assignFile(GeoidFile,fName);
      reset(GeoidFile,SizeOf(GeoidGrid));
      BlockRead(GeoidFile,GeoidGrid,1);
      closeFile(GeoidFile);
   end
   else MessageToContinue('GEOID.DEM missing.');
   AltParam := WaveHeight;
end;


initialization
end.

