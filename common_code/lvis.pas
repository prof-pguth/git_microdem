unit lvis;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   SysUtils,Graphics,Forms,Classes,Math,
   Petmar_types,DEMmapF,DEMESRIShapeFile,DEMDefs,BaseGraf;

//LDS v 1.02

type
  tlce102 = packed record  //28 bytes, big-endian
     lfid,
     shotnumber : cardinal;
     time,
     tlong,
     tlat : float64;
     zt   : Float32;
  end;

  tlge102 = packed record   //44 bytes, big-endian
     lfid,
     shotnumber : cardinal;
     time,
     glon,
     glat        : float64;   //8 byte
     zg,
     rh25,
     rh50,
     rh75,
     rh100 : float32;
  end;

  tlgw102 = packed record  //484 bytes, big-endian
     lfid,
     shotnumber : cardinal;
     time,
     lon0,
     lat0   : float64;
     z0     : Float32;
     lon431,
     lat431 : float64;
     z431,
     sigmean : Float32;
     rxwave : array[0..431] of byte;
  end;


//LDS v 1.03


type
  tlce = packed record  //48 bytes, big-endian
     lfid,
     shotnumber : cardinal;
     azimuth,
     incidentangle,
     range       : Float32;
     time,

     tlong,
     tlat :  float64;
     zt   : Float32;
  end;

tlge = packed record   //64 bytes, big-endian
     lfid,
     shotnumber : cardinal;
     azimuth,
     incidentangle,
     range       : Float32;
     time        : float64;
     glon,
     glat        : float64;
     zg,
     rh25,
     rh50,
     rh75,
     rh100 : Float32;
end;

tlgw = packed record  //580 bytes, big-endian
     lfid,
     shotnumber : cardinal;
     incidentangle,
     range       : Float32;
     time,
     lon0,
     lat0   : float64;
     z0 : Float32;
     lon431,
     lat431 : float64;
     z431,
     sigmean : Float32;
     txwave : array[0..79] of byte;
     rxwave : array[0..431] of byte;
end;


tLVIS_Dataset = class
   public
      lceFName,
      lgeFName,
      lgwFName : PathStr;
      lcef,lgef,lgwf : file;
      BoundBox   : sfBoundBox;
      GroundZMin,
      GroundZMax : Float32;
      WaveformZMin,
      WaveformZMax,
      CanopyZMin,
      CanopyZMax : Float32;
      NumRecs : integer;
      MapOwner : tMapForm;
      Version103 : boolean;

      constructor Create(var fName : PathStr; inMapOwner : tMapForm);
      destructor Destroy;
      procedure FindBounds;
      procedure PlotCanopyElevationOnMap;
      procedure PlotGroundElevationOnMap;
      procedure PulseStats;
      procedure GraphReturn(var Graph : tThisBaseGraph; ReturnNumber : integer);
      procedure OpenLGWfile;
      procedure OpenLCEfile;
      procedure OpenLGEfile;
      function ReadLGWrecord : tlgw;
      function ReadLCErecord : tlce;
      function ReadLGErecord : tlge;
end;


implementation


uses
   Petmar,PetMath,PetImage,
   BaseMap;


{ tLVIS_Dataset }


constructor tLVIS_Dataset.Create(var fName: PathStr; inMapOwner : tMapForm);
var
   i : integer;
begin
   if (FName = '') then begin
      Petmar.GetFileFromDirectory('LVIS file','*.lce;*.lce.1.03',LastLVISFileName);
      if LastLVISFileName = '' then exit;
      fName := LastLVISFileName;
   end;
   if fName = '' then exit;

   MapOwner := inMapOwner;
   if ExtractFileExt(fName) = '.03' then begin
      i := length(fName);
      lceFName := fName;
      lceFName[i-6] := 'c';
      lceFName[i-5] := 'e';

      lgeFName := fName;
      lgeFName[i-6] := 'g';
      lgeFName[i-5] := 'e';

      lgwFName := fName;
      lgwFName[i-6] := 'g';
      lgwFName[i-5] := 'w';
      Version103 := true;
   end
   else begin
      lceFName := ChangeFileExt(fName,'.lce');
      lgeFName := ChangeFileExt(fName,'.lge');
      lgwFName := ChangeFileExt(fName,'.lgw');
      Version103 := false;
   end;

   InitializeBoundingBox(BoundBox);
   CanopyZMin := 99999;
   CanopyZMax := -99999;
   GroundZMin := 99999;
   GroundZMax := -99999;
   WaveformZMin := 99999;
   WaveformZMax := -99999;
   FindBounds;
end;

destructor tLVIS_Dataset.Destroy;
begin
   inherited;
end;


procedure tLVIS_Dataset.FindBounds;
var
   lce : tlce;
   lge : tlge;
   lgw : tlgw;
   i : integer;
begin
   StartProgress('LCE');
   OpenLCEfile;
   i := 0;
   while not eof(lcef) do begin
      lce := ReadLCErecord;
      inc(i);
      if i mod 100 = 0 then UpDateprogressBar(i/NumRecs);
      PetMath.CompareValueToExtremes(lce.zt,CanopyZMin,CanopyZMax);
      PetMath.CompareValueToExtremes(lce.tlat,BoundBox.YMin,BoundBox.YMax);
      PetMath.CompareValueToExtremes(lce.tlong,BoundBox.XMin,BoundBox.XMax);
   end;
   closeFile(lcef);

   WriteLineToDebugFile('LCE recs=' + IntToStr(NumRecs));

   StartProgress('LGE');
   OpenLGEfile;
   i := 0;
   while not eof(lgef) do begin
      inc(i);
      if (i mod 100 = 0) then UpDateprogressBar(i/NumRecs);
      lge := ReadLGErecord;
      PetMath.CompareValueToExtremes(lge.zg,GroundZMin,GroundZMax);
      PetMath.CompareValueToExtremes(lge.glat,BoundBox.YMin,BoundBox.YMax);
      PetMath.CompareValueToExtremes(lge.glon,BoundBox.XMin,BoundBox.XMax);
   end;
   closeFile(lgef);

   WriteLineToDebugFile('LGE recs=' + IntToStr(NumRecs));

   StartProgress('LGW');
   OpenLGWfile;
   i := 0;
   while not eof(lgwf) do begin
      lgw := ReadLGWrecord;
      inc(i);
      if (i mod 100 = 0) then UpDateprogressBar(i/NumRecs);

      PetMath.CompareValueToExtremes(lgw.z0,WaveformZMin,WaveformZMax);
      PetMath.CompareValueToExtremes(lgw.z431,WaveformZMin,WaveformZMax);
      PetMath.CompareValueToExtremes(lgw.lat0,BoundBox.YMin,BoundBox.YMax);
      PetMath.CompareValueToExtremes(lgw.lon0,BoundBox.XMin,BoundBox.XMax);
      PetMath.CompareValueToExtremes(lgw.lat431,BoundBox.YMin,BoundBox.YMax);
      PetMath.CompareValueToExtremes(lgw.lon431,BoundBox.XMin,BoundBox.XMax);
   end;
   closeFile(lgwf);

   LongitudeAngleInRange(BoundBox.XMin);
   LongitudeAngleInRange(BoundBox.XMax);

   WriteLineToDebugFile('LGW recs=' + IntToStr(NumRecs));
   EndProgress;
end;


procedure tLVIS_Dataset.PulseStats;
var
   lce : tlce;
   lge : tlge;
   I  : Integer;
   Dist,Az : float64;
   Findings : tStringList;
   fName : PathStr;
begin
   OpenLCEFile;
   OpenLGEfile;
   Findings := tStringList.Create;
   Findings.Add('Lat,Long,HAG,Grnd_dist,azimuth,pitch');
   i := 0;
   StartProgress('Pulse');
   while not eof(lcef) do begin
      inc(i);
      if i mod 100 = 0 then UpDateprogressBar(i/NumRecs);
      lce := ReadLCErecord;
      lge := ReadLGErecord;
      VincentyCalculateDistanceBearing(lce.tlat,lce.tlong,lge.glat,lge.glon,Dist,Az);
      Findings.Add(RealToString(0.5*(lce.tlat+lge.glat),-18,-8) + ',' +
                   RealToString(0.5*(lce.tlong+lge.glon),-18,-8) + ',' +
                   RealToString(lce.zt-lge.zg,-18,-4) + ',' +
                   RealToString(Dist,-18,-4) + ',' +
                   RealToString(Az,-18,-2) + ',' +
                   RealToString((arcTan((lce.zt-lge.zg)/dist))/DegToRad,-18,-2) );
   end;
   EndProgress;
   closeFile(lcef);
   closeFile(lgef);

   fname := MDTempDir + ExtractFileNameNoExt(lceFName) + '_shots.csv';
   MapOwner.DisplayAndPurgeStringListDB(Findings,fName);
end;


procedure tLVIS_Dataset.GraphReturn(var Graph: tThisBaseGraph; ReturnNumber: integer);
var
   lce : tlce;
   lge : tlge;
   lgw : tlgw;
   I,x,y  : Integer;
   rfile : file;
begin
   if (Graph = Nil) then begin
      Graph := TThisBaseGraph.Create(Application);
      Graph.GraphDraw.MaxHorizAxis := 255;
      Graph.GraphDraw.MinVertAxis := WaveformZMin;
      Graph.GraphDraw.MaxVertAxis := WaveformZMax;
      Graph.RedrawDiagram11Click(Nil);
   end
   else Graph.ClearDataOnGraph;
   Graph.OpenDataFile(rfile);

   OpenLGWfile;

   seek(lgwf,ReturnNumber);
   lgw := ReadLGWrecord;
   for I := 0 to 431 do begin
      Graph.AddPointToDataBuffer(rfile,lgw.rxWave[i],lgw.z0 + i/431 * (lgw.z431-lgw.z0));
   end;
   closeFile(lgwf);
   Graph.ClosePointDataFile(rfile);
   Graph.Symbol.Color := claLime;
   Graph.OpenPointFile(rfile,Graph.Symbol);
   OpenLCEfile;
   seek(lcef,ReturnNumber);
   lce := ReadLCErecord;
   closeFile(lcef);
   Graph.AddPointToDataBuffer(rfile,1,lce.zt);

   OpenLGEfile;
   seek(lgef,ReturnNumber);
   lge := ReadLGErecord;
   closeFile(lgef);
   Graph.AddPointToDataBuffer(rfile,1,lge.zg);
   Graph.ClosePointDataFile(rfile);
   Graph.Symbol.Color := claBlue;
   Graph.OpenPointFile(rfile,Graph.Symbol);
   Graph.AddPointToDataBuffer(rfile,5,lge.zg + lge.rh25);
   Graph.AddPointToDataBuffer(rfile,5,lge.zg + lge.rh50);
   Graph.AddPointToDataBuffer(rfile,5,lge.zg + lge.rh75);
   Graph.AddPointToDataBuffer(rfile,5,lge.zg + lge.rh100);
   Graph.ClosePointDataFile(rfile);
   Graph.RedrawDiagram11Click(Nil);

   MapOwner.MapDraw.LatLongDegreeToScreen(lge.glat,lge.glon,x,y);
   MapOwner.Image1.Canvas.MoveTo(x,y);
   MapOwner.MapDraw.LatLongDegreeToScreen(lce.tlat,lce.tlong,x,y);
   MapOwner.Image1.Canvas.Pen.Color := clBlack;
   MapOwner.Image1.Canvas.Pen.Width := 3;
   MapOwner.Image1.Canvas.LineTo(x,y);
end;


procedure tLVIS_Dataset.OpenLCEfile;
var
  rs : integer;
begin
   if Version103 then rs := SizeOf(tlce)
   else rs := SizeOf(tlce102);
   NumRecs := GetFileSize(lceFName) div rs;
   assignFile(lcef,lceFName);
   reset(lcef,rs);
end;

procedure tLVIS_Dataset.OpenLGEfile;
var
  rs : integer;
begin
   if Version103 then rs := SizeOf(tlge)
   else rs := SizeOf(tlge102);
   NumRecs := GetFileSize(lgeFName) div rs;
   assignFile(lgef,lgeFName);
   reset(lgef,rs);
end;

procedure tLVIS_Dataset.OpenLGWfile;
var
  rs : integer;
begin
   if Version103 then rs := SizeOf(tlgw)
   else rs := SizeOf(tlgw102);
   NumRecs := GetFileSize(lgwFName) div rs;
   assignFile(lgwf,lgwFName);
   reset(lgwf,rs);
end;

procedure tLVIS_Dataset.PlotCanopyElevationOnMap;
var
   lce : tlce;
   x,y,i : integer;
   Color : tPlatformColor;
begin
   i := 0;
   StartProgress('Canopy z');
   OpenLCEfile;
   while not eof(lcef) do begin
      inc(i);
      if (i mod 100 = 0) then UpDateprogressBar(i/NumRecs);
      lce := ReadLCErecord;
      MapOwner.MapDraw.LatLongDegreeToScreen(lce.tlat,lce.tlong,x,y);
      Color := PetMar.PlatformRainbowColorFunct(lce.zt,CanopyZMin,CanopyZMax);
      Petmar.ScreenSymbol(MapOwner.Image1.Canvas,x,y,FilledBox,2,color);
   end;
   closeFile(lcef);
   EndProgress;
end;

procedure tLVIS_Dataset.PlotGroundElevationOnMap;
var
   lge : tlge;
   x,y,i : integer;
   Color : tPlatformColor;
begin
   OpenLGEfile;
   i := 0;
   StartProgress('Ground z');
   while not eof(lgef) do begin
      inc(i);
      if i mod 100 = 0 then UpDateprogressBar(i/NumRecs);
      lge := ReadLGErecord;
      MapOwner.MapDraw.LatLongDegreeToScreen(lge.glat,lge.glon,x,y);
      Color := PetMar.PlatformRainbowColorFunct(lge.zg,GroundZMin,GroundZMax);
      Petmar.ScreenSymbol(MapOwner.Image1.Canvas,x,y,FilledBox,2,color);
   end;
   closeFile(lgef);
   EndProgress;
end;


function tLVIS_Dataset.ReadLCErecord : tlce;
var
   lce102 : tlce102;
begin
   if Version103 then BlockRead(lcef,Result,1)
   else begin
      BlockRead(lcef,lce102,1);
      Result.lfid := lce102.lfid;
      Result.ShotNumber := lce102.ShotNumber;
      Result.time := lce102.time;
      Result.lfid := lce102.lfid;
      Result.tlong := lce102.tlong;
      Result.tlat := lce102.tlat;
      Result.zt := lce102.zt;
   end;
   with Result do begin
     Int4Swap(lfid);
     Int4Swap(ShotNumber);
     if Version103 then begin
        SwapToShortFloat(azimuth);
        SwapToShortFloat(incidentangle);
        SwapToShortFloat(range);
     end
     else begin
       azimuth := 0;
       incidentangle := 0;
       range := 0;
     end;

     SwapToFloat8byte(time);
     SwapToFloat8byte(tlong);
     SwapToFloat8byte(tlat);
     SwapToShortFloat(zt);
   end;
   LongitudeAngleInRange(Result.tlong);
end;


function tLVIS_Dataset.ReadLGWrecord : tlgw;
var
   lgw102 : tlgw102;
   i : integer;
begin
   if Version103 then BlockRead(lgwf,Result,1)
   else begin
      BlockRead(lgwf,lgw102,1);
      Result.lfid := lgw102.lfid;
      Result.ShotNumber := lgw102.ShotNumber;
      Result.time := lgw102.time;
      Result.lon0 := lgw102.lon0;
      Result.lat0 := lgw102.lat0;
      Result.z0 := lgw102.z0;
      Result.lon431 := lgw102.lon431;
      Result.lat431 := lgw102.lat431;
      Result.z431 := lgw102.z431;
      Result.sigmean := lgw102.sigmean;
      for i := 0 to 431 do Result.rxwave[i] := lgw102.rxwave[i];
   end;
   with Result do begin
     Int4Swap(lfid);
     Int4Swap(ShotNumber);
     if Version103 then begin
        SwapToShortFloat(incidentangle);
        SwapToShortFloat(range);
     end
     else begin
       incidentangle := 0;
       range := 0;
     end;
     SwapToFloat8byte(time);
     SwapToFloat8byte(lon0);
     SwapToFloat8byte(lat0);
     SwapToShortFloat(z0);
     SwapToFloat8byte(lon431);
     SwapToFloat8byte(lat431);
     SwapToShortFloat(z431);
     SwapToShortFloat(sigmean);
   end;
   LongitudeAngleInRange(Result.lon0);
   LongitudeAngleInRange(Result.lon431);
end;


function tLVIS_Dataset.ReadLGErecord : tlge;
var
   lge102 : tlge102;
begin
   if Version103 then BlockRead(lgef,Result,1)
   else begin
      BlockRead(lgef,lge102,1);
      Result.lfid := lge102.lfid;
      Result.ShotNumber := lge102.ShotNumber;
      Result.time := lge102.time;
      Result.glon := lge102.glon;
      Result.glat := lge102.glat;
      Result.zg := lge102.zg;
      Result.rh25 := lge102.rh25;
      Result.rh50 := lge102.rh50;
      Result.rh75 := lge102.rh75;
      Result.rh100 := lge102.rh100;
   end;
   with Result do begin
     Int4Swap(lfid);
     Int4Swap(ShotNumber);
     SwapToFloat8byte(time);
     SwapToFloat8byte(glon);
     SwapToFloat8byte(glat);
     SwapToShortFloat(zg);
     SwapToShortFloat(rh25);
     SwapToShortFloat(rh50);
     SwapToShortFloat(rh75);
     SwapToShortFloat(rh100);
   end;
   LongitudeAngleInRange(Result.glon);
end;



end.
