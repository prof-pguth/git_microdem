unit sun_position;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define RecordSolarPosition}
      {$Define RecordHorizon}
      //{$Define RecordGetSunriseSet}
      {$Define TimeSunlightMaps}
      //{$Define RecordSunlightMaps}
      //{$Define RecordFullSunlightMaps}      //major slowdown
      //{$Define RecordBlockAngleDifference}  //major slowdown
   {$EndIf}
{$EndIf}


interface

uses
   Windows,Classes,Graphics,Controls,SysUtils,Math,Forms,
   BaseGraf,NetMainW,DEMMapf,DEMDefs,Petmar_types;


function SunTime(Lat,Long : float64; DifGren,Year,Month,Day : integer; Rising,WithSec : boolean; SunAngle : tSunriseSunsetAngle) : string35;
function ComputeSunPosition(lat,long,hrtime,tz : float64; day : integer; var az,alt,sunrise,sunset : float64) : boolean;
procedure SunRiseSunSet({TheSun : boolean;} Lat,Long : float64; DoGraphical,FullTable : boolean);
function SunAndHorizon(BaseMap : tMapForm; DEM : integer; Latitude,Longitude : float64; ShowToday : boolean = true; ShowKeyDays : boolean = true) : tNetForm;
procedure AnnualSunRiseGeometry(MapForm : tMapForm; Latitude,Longitude : float64; AddSunriseSunset: boolean = false);
procedure SunAtLocalNoon(BaseMap : tMapForm; Latitude,Longitude : float64);
function HorizonBlockingGraph(MapForm : tMapForm; Lat,Long : float64; AngleGraph,RangeGraph : boolean) : tNetForm;
function HoursSolarIlluminationGraph(DEM : integer; Lat,Long : float64) : TThisBaseGraph;
function HoursSolarIlluminationGrid(MapForm : tMapForm; JulianDay : integer) : integer;

var
   SunGraph : tThisBaseGraph;


implementation


uses
   Petmath,DEMCoord,DEMOptions,DEMMapDraw,BaseMap,GetLatLn,Petmar_db,Petmar,Petdbutils,
   DEMDataBase,DEMDef_routines,
   Nevadia_Main;


const
   ZenithDistance : array[1..4] of float64 = (90.83333,96.0,102.0,108.0);
type
   tBlockAngles = array[0..3600] of float64;


function GetSunriseSunSet(Latitude,Longitude : float64; tz,day : integer; var az,alt,sunrise,sunset,sunappears,sundisappears,DurationDayLight : float64; var BlockAngles : tBlockAngles) : boolean;
var
   BlockAngle : float64;
begin
   {$IfDef RecordGetSunriseSet} WriteLineToDebugFile('GetSunriseSunSet, day: ' + IntToStr(Day) + '   ' + LatLongDegreeToString(Latitude,Longitude)); {$EndIf}
   if Sun_Position.ComputeSunPosition(latitude,longitude,MDDef.ObsAboveGround,tz,day,az,alt,sunrise,sunset) then begin
      if MDDef.HorizonIncrement < 0.1 then MDDef.HorizonIncrement := 0.1;
      {$IfDef RecordGetSunriseSet} WriteLineToDebugFile('Sunrise: ' + RealToString(Sunrise,6,2) + '  Sunset: ' + RealToString(Sunset,6,2) + ' Az: ' + RealToString(Az,6,2) + '  Alt: ' + RealToString(Alt,6,2) );{$EndIf}
      SunAppears := Sunrise - MDDef.SunlightPrecision/60;
      SunDisappears := Sunset + MDDef.SunlightPrecision/60;
      DurationDayLight := SunSet - Sunrise;
      {$IfDef RecordGetSunriseSet} WriteLineToDebugFile('  Time    Azimuth Altitude Horizon');  {$EndIf}
      repeat
         SunAppears := SunAppears + MDDef.SunlightPrecision / 60;
         Sun_Position.ComputeSunPosition(latitude,longitude, SunAppears,tz,day,az,alt,sunrise,sunset);
         BlockAngle := BlockAngles[round(az / MDDef.HorizonIncrement)];
         {$IfDef RecordGetSunriseSet} WriteLineToDebugFile(RealToString(SunAppears,8,2) + RealToString(az,8,2) + RealToString(alt,8,2) + RealToString(BlockAngle,8,2));        {$EndIf}
      until (BlockAngle < Alt) or (SunAppears > SunDisappears);

      if (SunAppears < SunDisappears) then begin
         {$IfDef RecordGetSunriseSet} WriteLineToDebugFile('  Time    Azimuth Altitude Horizon'); {$EndIf}
         repeat
            SunDisappears := SunDisappears - MDDef.SunlightPrecision / 60;
            Sun_Position.ComputeSunPosition(latitude,  longitude, SunDisappears,tz,day,az,alt,sunrise,sunset);
            BlockAngle := BlockAngles[round(az/ MDDef.HorizonIncrement)];
            {$IfDef RecordGetSunriseSet}WriteLineToDebugFile(RealToString(SunAppears,8,2) + RealToString(az,8,2) + RealToString(alt,8,2) + RealToString(BlockAngle,8,2));           {$EndIf}
         until (BlockAngle <  Alt) or (SunAppears > SunDisappears);
      end;
      Result := true;
     {$IfDef RecordGetSunriseSet}WriteLineToDebugFile('Sun up: ' + RealToString(SunAppears,6,2) + '   Sun down: ' + RealToString(SunDisappears,6,2) );     {$EndIf}
   end
   else begin
      Result := false;
   end;
end;


procedure GetBlockAngles(DEM,WiderDEM : integer; Lat,Long : float64; var BlockAngles : tBlockAngles);
var
   Az,EndAz,BlockAngle,BlockLength,BlockLat,BlockLong,WiderBlockAngle,WiderBlockLength : float64;
begin
   {$IfDef TimeSunlightMaps} WriteLineToDebugFile('GetBlockAngles in');  {$EndIf}
   Az := 0;
   EndAz := 360;
   while (Az <= EndAz) do begin
       DEMGlb[DEM].HorizonBlocking(lat,long,Az,MDDef.HorizonLength,2,BlockAngle,BlockLength,BlockLat,BlockLong,saDEMGrid);
       BlockAngles[round(az / MDDef.HorizonIncrement)] := BlockAngle;
       if (WiderDEM <> 0) then begin
          DEMGlb[WiderDEM].HorizonBlocking(lat,long,Az,MDDef.HorizonLength,2,WiderBlockAngle,WiderBlockLength,BlockLat,BlockLong,saDEMGrid);
          if (WiderBlockLength > BlockLength) and (WiderBlockAngle > BlockAngle) then  BlockAngles[round(az / MDDef.HorizonIncrement)] := WiderBlockAngle;
       end;
       Az := Az + MDDef.HorizonIncrement;
   end;
   {$IfDef TimeSunlightMaps} WriteLineToDebugFile('GetBlockAngles out'); {$EndIf}
end;


procedure SolarZenithAndAzimuth(JDay : integer; Hours,Lat,Long : float64; var SolarZenith,SolarAzimuth : float64);
{Peddle, D.R., White, H.P., Soffer, R.J., Miller, J.R., and LeDraw, E.F., 2001, Reflectance processing of remote sensing spectroradiometer data: Computers & Geosciences, vo.27, no.2, p.203-213.}
var
   a,TU1, TM,L_theta,g,Le,d_theta,oe,RA_theta,Dec_theta,Dec_z,Tu2,GMST,Raz : float64;
begin
   TM := Hours / 24;
   TU1 := (JDay - 2451545) + (1.002738 * TM);
   L_theta := 280.46 + 0.9856474 * Tu1;
   while L_theta > 360 do L_theta := L_Theta - 360;
   g := 357.528 + 0.9856003 * Tu1;
   while g > 360 do g := g - 360;
   Le := L_theta + 1.915 * sinDeg(g) + 0.02 * sinDeg(2 * g);
   while Le > 360 do Le := Le - 360;
   d_theta := 1.00014 - 0.1671 * CosDeg(g) - 0.00014 * CosDeg(2 * g);
   oe := 23.439 - 0.0000004 * Tu1;
   a := SphericalEarthAkm * 1000;
   RA_theta := a * TanDeg(CosDeg(oe) * TanDeg(Le)) / 15;
   Dec_theta := a * SinDeg(SinDeg(oe) * SinDeg(Le));
   Dec_Z := Lat;
   Tu2 := (JDay - 2451545)  / 36525;
   GMST := (24110.54841 + 8640184.812866 * Tu2 + 0.093104 * sqr(Tu2) - 0.0000062 * Tu2 * sqr(Tu2)) / 3600 + 1.002738 * TM * 24;
   Raz := GMST + Long / 15;
   while Raz > 24 do Raz := Raz - 24;
   while Raz < 24 do Raz := Raz + 24;
   SolarZenith := a * cosDeg( (sinDeg(Dec_Z) * SinDeg(Dec_theta)) + CosDeg(Dec_Z) * CosDeg(Dec_theta) * CosDeg( (Raz - RA_theta) * 15));
   SolarAzimuth :=  a * SinDeg(SinDeg( (Raz - RA_theta) * 15) * CosDeg(Dec_theta) / SinDeg(SolarZenith));
end;


procedure SunCalculate(Lat,Long,z : float64; N : integer; Rising : boolean;  var UT : float64);
var
   sinD,cosD,x,RA,t,H,L,M   : float64;
begin
   if Rising then t := N + (6 - Long / 15) / 24
   else t := N + (18 - Long / 15) / 24;
   M := 0.9856 * t - 3.289;
   L := M + 1.916 * SinDeg(M) + 0.020 * SinDeg (2.0 * M) + 282.634;
   while L > 360 do L := L - 360;
   RA := ArcTan(0.9174 * TanDeg(L)) / Petmar_types.DegToRad;
   if RA < 0 then RA := RA + 360;
   if abs(RA - L) > 90 then RA := RA + 180;
   if RA > 360 then RA := RA - 360;
   sinD := 0.39782 * sinDeg(L);
   cosD := cos(Math.ArcSin(SinD));
   x := (cosDeg(z) - sinD * sinDeg(Lat)) / (cosD * CosDeg(Lat));
   H := ArcCos(x) / Petmar_types.DegToRad;
   if Rising then H := 360 - H;
   T := H / 15 + RA / 15 - 0.065710 * t - 6.622;
   if (T > 24) then T := T - 24;
   if (T < 0) then T := T + 24;
   UT := T - Long / 15;
end;


function SunTime(Lat,Long : float64; DifGren,Year,Month,Day : integer; Rising,WithSec : boolean; SunAngle : tSunriseSunsetAngle) : string35;
var
   Ns,JDay : integer;
   UT : float64;
begin
   Ns := (275 * 1 div 9) - ((1 + 9) div 12) * ((1 + trunc(Year - 4 * (Year div 4) + 2) div 3)) + 1 - 30;
   JDay := JulDay(Month,Day,Year) - JulDay(1,1,Year);
   SunCalculate(Lat,Long,ZenithDistance[succ(ord(SunAngle))],NS + JDay,Rising,UT);
   Result := HoursMinutesString(UT - DifGren,WithSec);
end;



function ComputeSunPosition(lat,long, hrtime, tz : float64; day : integer; var az,alt,sunrise, sunset : float64) : boolean;
//lat,long in degrees
//hrtime in hours (0-24)
//tz  -12 to 12
//day is Julian day of the year, 1-365
 {Copyright  © 2003-2005, Gary Darby,  www.DelphiForFun.org
  This program may be used or modified for any non-commercial purpose so long as this original notice remains in place.
  All other rights are reserved}
  {
  /*  Adapted from C code written by :
  /*                Joe Cychosz                                           */
  /*                Purdue University CADLAB                              */
  /*                Potter Engineering Center                             */
  /*                W. Lafayette, IN  47907                               */
  /*  http://jedi.ks.uiuc.edu/~johns/raytracer/rtn/rtnv7n2.html#art23     */
  /* -------------------------------------------------------------------- */
  }

     function eqoftime(jday:integer):float64;
     {Equations of time - hours that solar differs from mean time an any day}
     var
        b : float64;
     begin
        b := 360.0 * (day-81) / 364;
        result:=(9.87*sindeg(2.0*b) - 7.53*cosdeg(b) - 1.5*sindeg(b)) / 60.;  {eq of time}
     end;


 var
    lontz : float64;   { time zone longitude}
    e : float64;       { Time                         }
    phi : float64;     {altitude }
    gs,cws, ws,civilcws,
    x, EqOfTimeMinutes,sunriseangle, sunsetangle, civilsunrise,  civilSunset,
    midday,declination,Hourangle,middayaz,middayalt,tzhours : float64;
    errmsg,civilerrmsg : shortstring;
    DLShours : integer;

   procedure getRiseSet(wsx:float64; var sunrisex,sunsetx:float64);
   var x:float64;
   begin
     x:=(180.0 - wsx + lontz - long);
     while x<0 do x:=x+360;
     sunrisex:=  x/ 15.0 - e + DLSHours;
     {if sunrise<0 then sunrise:=sunrise+24;}
       x:=(180.0 + wsx + lontz - long);
       while x<0 do x:=x+360;
       sunsetx:=  x/ 15.0 - e + DLSHours;
      { result:=(hrtime >= sunrise) and (hrtime <= sunset);}
    end;


begin
   result:=false;
   errmsg:='';
   civilerrmsg:='';
   tzhours := tz;
   DLSHours := 0;
   lontz := tz*15; {time zone degrees}
   e:=eqoftime(day);
   midday:= 12-long/15+tzhours-e;
   while midday <0 do midday := midday+24.0;
   declination := 23.45 * sindeg (360.0 * (284+day) / 365.25);
   Hourangle:=180.0 + lontz - long - 15.0*(midday+e);
   phi:=arccos(sindeg(lat)*sindeg(declination) + cosdeg(lat)*cosdeg(declination)*cosdeg(Hourangle));{alt}
   if lat<0 then hourangle := hourangle + 180;
   gs:=arcsin(cosdeg(declination) * sindeg(HourAngle) / sin(phi)); {az}
   middayaz:=180-radtodeg(gs);
   middayalt:=90-radtodeg(phi);
   midday:=midday+dlshours;

   {Current position calculations}

   {Compute solar declination (23.45 deg on June 21) }
   declination := 23.45 * sindeg (360.0 * (284+day) / 365.25);
   {Compute the hour angle in degrees. }
   Hourangle:=180.0 + lontz - long - 15.0*(hrtime-dlshours+e);
   //sin(ALT) = sin(DEC)*sin(LAT)+cos(DEC)*cos(LAT)*cos(HA)
   //ALT = asin(ALT)
   Alt:=radtodeg(arcsin(sindeg(declination)*sindeg(lat)+cosdeg(declination)*cosdeg(lat)*cosdeg(hourangle)));
   gs:=arccos((sindeg(declination)-sindeg(alt)*sindeg(lat))/(cosdeg(alt)*cosdeg(lat)));
   az:=radtodeg(gs);
  //If sin(HA) is negative, then AZ = A, otherwise AZ = 360 - A
  If sindeg(Hourangle)<0 then az:=360-az;
   { Compute the position of the sun. }
   If sindeg(Hourangle)<0 then gs:=2*pi-gs;
   EqOfTimeMinutes := e*60;

   x:=-tandeg(Declination)*tandeg(Lat) ; {replacement}
   cws:=x+cosdeg(90.833)/(cosdeg(lat)*cosdeg(declination));
   Civilcws:=x+cosdeg(96)/(cosdeg(lat)*cosdeg(declination));
   if cws<-1 then begin
      errmsg:='Sun never sets';
      sunrise:=0;
      sunset:=24;
   end
   else if cws>1 then begin
      errmsg:='Sun never rises';
      sunrise:=0;
      sunset:=0;
   end
   else begin
     result:=true;
     {Compute sunrise, sunset times and angles}
     ws:=radtodeg(arccos(cws));
     getRiseSet(ws, sunrise,sunset);
     phi:=arccos(sindeg(declination)/cosdeg(lat));
     sunriseangle:=radtodeg(phi);
     sunsetangle:=360-sunriseangle;
   end;
   if civilcws<-1 then Civilerrmsg:='no Civil Sunset'
   else if civilcws>1 then Civilerrmsg:='no Civil Sunrise'
   else begin
     Ws:=radtodeg(arccos(civilcws));
     getriseset(ws, civilsunrise,  civilSunset);
   end;
end;

function HoursSolarIlluminationGrid(MapForm : tMapForm; JulianDay : integer) : integer;
var
   BlockAngle,BlockLength,BlockLat,BlockLong,Lat,Long, SunRise,SunSet, az,alt, hrtime : float64;
   z : float32;
   NewGrid,x,y, tz,SunUp,db : integer;
begin
   {$IfDef RecordHorizon} WriteLineToDebugFile('HoursSolarIlluminationGrid JDay=' + IntToStr(JulianDay)); {$EndIf}

   tz := round(MapForm.MapDraw.MapCorners.BoundBoxGeo.xmin / 15);
   NewGrid := MapForm.MakeTempGrid;
   DEMGlb[NewGrid].DEMheader.ElevUnits := Undefined;
   DEMGlb[NewGrid].AreaName := 'Hours_direct_illumination_Julian_day_' + IntToStr(JulianDay);
   {$IfDef RecordHorizon} WriteLineToDebugFile('New grid=' + DEMGlb[NewGrid].KeyDEMParams(true)); {$EndIf}
   StartProgressAbortOption('Hours daylight map, Julian day=' + IntToStr(JulianDay));
   for x := 0 to pred(DEMGlb[NewGrid].DEMheader.NumCol) do begin
      UpdateProgressBar(x/DEMGlb[NewGrid].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[NewGrid].DEMheader.NumRow) do begin
         hrtime := 0;
         SunUp := 0;
         DEMGlb[NewGrid].DEMGridToLatLongDegree(x,y,Lat,Long);
         if DEMGlb[MapForm.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
            repeat
               Sun_Position.ComputeSunPosition(lat,long,hrtime,tz,JulianDay,az,alt,sunrise,sunset);
               if (alt > 0) then begin
                  DEMGlb[MapForm.MapDraw.DEMonMap].HorizonBlocking(lat,long,Az,MDDef.HorizonLength,2,BlockAngle,BlockLength,BlockLat,BlockLong,saDEMGrid);
                  if (BlockAngle <  Alt) then begin
                     inc(SunUp);
                  end;
               end;
               hrtime := hrtime + MDDef.SunlightPrecision / 60;
            until ((alt < 0) and (hrTime > 12)) or (hrTime > 24);
            DEMGlb[NewGrid].SetGridElevation(x,y,SunUp * MDDef.SunlightPrecision / 60);
         end;
      end;
      if WantOut then break;
   end;
   EndProgress;
   DEMGlb[NewGrid].SetUpMap(NewGrid,true,mtElevSpectrum);
   Result := NewGrid;
end;

function HoursSolarIlluminationGraph(DEM : integer; Lat,Long : float64) : TThisBaseGraph;
var
   SunRise,SunSet,SunAppears,SunDisappears,DurationDirectLight,DurationDayLight,TerrainMasking,
   az,alt, hrtime,HoursDaylight : float64;
   i,Month,tz,SunUp,db : integer;
   SunResults : tStringList;
   fName : PathStr;

      function ComputeADay(Day : integer; var HoursDaylight : float64) : float64;
      var
         sunrisen : integer;
         BlockAngle,BlockLength,BlockLat,BlockLong : float64;
      begin
         {$IfDef RecordHorizon} WriteLineToDebugFile('Start JDay=' + IntToStr(Day)); {$EndIf}
         hrtime := 0;
         SunUp := 0;
         SunRisen := 0;
         repeat
            Sun_Position.ComputeSunPosition(lat,long,hrtime,tz,day,az,alt,sunrise,sunset);
            if (alt > 0) then begin
               inc(SunRisen);
               DEMGlb[DEM].HorizonBlocking(lat,long,Az,MDDef.HorizonLength,2,BlockAngle,BlockLength,BlockLat,BlockLong,saDEMGrid);
               if (BlockAngle <  Alt) then begin
                  inc(SunUp);
               end;
            end;
            hrtime := hrtime + MDDef.SunlightPrecision / 60;
         until ((alt < 0) and (hrTime > 12)) or (hrTime > 24);
         Result := SunUp * MDDef.SunlightPrecision / 60;
         HoursDaylight := SunRisen  * MDDef.SunlightPrecision / 60;
         {$IfDef RecordHorizon} WriteLineToDebugFile('End JDay=' + IntToStr(Day)); {$EndIf}
      end;


begin
   tz := round(long / 15);
   SunResults := tStringList.Create;
   SunResults.Add('JULIAN_DAY,HOURS_ILL,HOUR_LIGHT');
   StartProgress('Sun above horizon');
   i := 1;
   while I <= 365 do begin
      UpdateProgressBar(i/365);
      SunResults.Add(IntToStr(i) + ',' + RealToString(ComputeADay(i,HoursDaylight),-12,-4) + ',' + RealToString(HoursDaylight,-12,-4));
      inc(i,7);
   end;
   EndProgress;

   fName := Petmar.NextFileNumber(MDTempDir,'hours_sun_' + LatLongToStringForFileName(Lat,Long) + '_','.dbf');
   db := StringList2CSVtoDB(SunResults, fName);

   Result := GISdb[db].CreateScatterGram('JULIAN_DAY','HOURS_ILL',true);
   GISdb[db].AddSeriesToScatterGram(Result,clLime,'JULIAN_DAY','HOUR_LIGHT',true);

   Result.GraphDraw.LegendList := tStringList.Create;
   Result.GraphDraw.LegendList.Add('Hours direct illumination');
   Result.GraphDraw.LegendList.Add('Hours daylight');

   Result.GraphDraw.SetShowAllLines(true);
   Result.GraphDraw.SetShowAllPoints(false);
   Result.AutoScaleAndRedrawDiagram;
end;



function HorizonBlockingGraph(MapForm : tMapForm; Lat,Long : float64; AngleGraph,RangeGraph : boolean) : tNetForm;
var
   Results : TStringList;
   Location : ShortString;
   xdemg1,ydemg1,BlockAngle,BlockLength,Angle,BlockLat,BlockLong : float64;
   lastxd,lastyd, xd,yd,db : integer;
   fName : PathStr;
   Graph1 : tThisBaseGraph;


         procedure AddToGraph(JDay : integer);
         var
            hr,az,alt,sunrise,sunset : float64;
            rfile : file;
         begin
            hr := 0;
            Graph1.OpenDataFile(rfile);
            repeat
               Sun_Position.ComputeSunPosition(lat,long, hr,Long / 15,JDay,az,alt,sunrise,sunset);
               if (alt > 0) then begin
                   Graph1.AddPointToDataBuffer(rfile,az,alt);
               end;
               hr := hr + 0.5;
               if Sunrise > hr then hr := Sunrise;
            until (hr > 24);
            Graph1.ClosePointDataFile(rfile);
         end;



begin
   ShowHourglassCursor;
   if (MapForm.MapDraw.DEMonMap = 0) then exit;
   DEMGlb[MapForm.MapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xdemg1,ydemg1);
   Location := DEMGlb[MapForm.MapDraw.DEMonMap].DEMLocationString(xdemg1,ydemg1);
   Results := TStringList.Create;
   Results.Add('LAT,LONG,AZIMUTH,ELEV_DEG,BLOCK_M');

   {$IfDef RecordHorizon} WriteLineToDebugFile('TMapForm.DrawHorizonBlocking ' + DEMGlb[MapForm.MapDraw.DEMonMap].DEMLocationString(xdemg1,ydemg1)); {$EndIf}

    if MDDef.HorizonSkyMap then begin
       Result := Sun_Position.SunAndHorizon(MapForm,MapForm.MapDraw.DEMOnMap,Lat,Long);
       Result.Caption := 'Horizon at ' + Location;
       Result.nd.LLcornerText := LatLongDegreeToString(Lat,Long);
       Lastxd := -999;
      {$IfDef RecordHorizon} WriteLineToDebugFile('TMapForm.DrawHorizonBlocking sky map created'); {$EndIf}
    end;

    Angle := 0;
    while (Angle <= 360) do begin
       DEMGlb[MapForm.MapDraw.DEMonMap].HorizonBlocking(Lat,Long,Angle,MDDef.HorizonLength,MDDef.ObsAboveGround,BlockAngle,BlockLength,BlockLat,BlockLong,MDDef.wf.StraightAlgorithm);
       Results.Add(RealToString(BlockLat,-12,-8) + ',' + RealToString(BlockLong,-12,-8) + ',' + RealToString(Angle,6,2) + ',' +   RealToString(BlockAngle,16,2) + ',' +  RealToString(BlockLength,16,1));
       if MDDef.HorizonSkyMap then begin
          if MDDef.InvertSkyline then BlockAngle := -BlockAngle + 30;
          Result.nd.PlotPointOnNet(LinePlot,BlockAngle,Angle,ASymbol(Dot,MDDef.HorizonColor,MDDef.HorizonWidth),xd,yd);
          if (LastXD > -99) then begin
             Result.nd.WorkingBitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.HorizonColor);
             Result.nd.WorkingBitmap.Canvas.Pen.Width := MDDef.HorizonWidth;
             Result.nd.WorkingBitmap.Canvas.MoveTo(xd,yd);
             Result.nd.WorkingBitmap.Canvas.LineTo(Lastxd,Lastyd);
          end;
          lastxd := xd;
          lastyd := yd;
       end;
       Angle := Angle +  MDDef.HorizonIncrement;
    end;
   {$IfDef RecordHorizon} WriteLineToDebugFile('TMapForm.DrawHorizonBlocking horizon on net'); {$EndIf}

    fName := Petmar.NextFileNumber(MDTempDir,'horizon_block_' + LatLongToStringForFileName(Lat,Long) + '_','.dbf');
    db := MapForm.StringListToLoadedDatabase(Results, fName);
    GISdb[db].dbOpts.DBAutoShow := dbasConnectSeqPts;
    GISdb[db].dbOpts.LineColor := MDDef.HorizonColor;
    GISdb[db].dbOpts.LineWidth := MDDef.HorizonWidth;
    GISdb[db].RedrawLayerOnMap;

    if AngleGraph then begin
       Graph1 := GISdb[db].CreateScatterGram('AZIMUTH','ELEV_DEG',true);
       Graph1.GraphDraw.FileColors256[1] := MDDef.HorizonColor;
       if MDDef.ShowSolstices then begin
          Graph1.GraphDraw.LegendList := tStringList.Create;
          Graph1.GraphDraw.LegendList.Add('Horizon');
          Graph1.GraphDraw.LegendList.Add('Summer solstice');
          Graph1.GraphDraw.LegendList.Add('Winter solstice');
          AddToGraph(AnnualJulianDay(2019,6,21));
          AddToGraph(AnnualJulianDay(2019,12,21));
       end;
       Graph1.GraphDraw.SetShowAllLines(true);
       Graph1.GraphDraw.SetShowAllPoints(false);
       Graph1.AutoScaleAndRedrawDiagram;
    end;
    if RangeGraph then GISdb[db].CreateScatterGram('AZIMUTH','BLOCK_M',true);

    if MDDef.HorizonSkyMap then begin
       Result.UpdateDisplay;
       Result.BringToFront;
    end;
    ShowDefaultCursor;
   {$IfDef RecordHorizon} WriteLineToDebugFile('TMapForm.DrawHorizonBlocking horizon out'); {$EndIf}
end;


procedure SunAtLocalNoon(BaseMap : tMapForm; Latitude,Longitude : float64);
var
   SunRise,SunSet,az,alt : float64;
   tz,Day,db : integer;
   SunResults : tStringList;
   fName : PathStr;
   TheGraph : TThisBaseGraph;
begin
   tz := round(Longitude / 15);
   StartProgress('Sun position');
   SunResults := tStringList.Create;
   SunResults.Add('JULIAN_DAY,MAX_ALT');
   for day := 1 to 365 do begin
      UpdateProgressBar(Day/365);
      Sun_Position.ComputeSunPosition(latitude,tz*15,12,tz,day,az,alt,sunrise,sunset);
      SunResults.Add(IntegerToString(Day) + ',' + RealToString(alt,-8,-2));
   end;
   EndProgress;
   fName := Petmar.NextFileNumber(MDTempDir, 'sun_altitude_noon_',DefaultDBExt);
   db := BaseMap.StringListToLoadedDatabase(SunResults,fName);
   TheGraph := GISdb[db].CreateScatterGram('JULIAN_DAY','MAX_ALT',false,'Local noon solar altitude ' + LatLongDegreeToString(Latitude,Longitude,VeryShortDegrees));
   TheGraph.GraphDraw.MaxVertAxis := 90;
   TheGraph.GraphDraw.MinVertAxis := -15;
   TheGraph.GraphDraw.MaxHorizAxis := 365;
   TheGraph.SettingsChanged := true;
   TheGraph.LLcornerText := LatLongDegreeToString(Latitude,Longitude,VeryShortDegrees);
   TheGraph.RedrawDiagram11Click(Nil);
   if (MDdef.ProgramOption <> ExpertProgram) then CloseAndNilNumberedDB(db);
end;



procedure SunRiseSunSet(Lat,Long : float64; DoGraphical,FullTable : boolean);
{from pages B5-B7, Almanac for computers 1989, Nautical Almanac Office,  U.S. Naval Observatory.
 Valid for latitudes less than 65ø (deteriorates near poles when sun above or below horizon longer than 24 hours) at any time in the latter half of the 20th century.
 Accurate to about 2 min}
type
   YearArray = array[1..366] of float64;
var
   wMonth,wDay,wYear : word;
   NDays,D,i,j,Month,Day,
   Ns,Year,DifGren     : integer;
   UT                       : float64;
   JDay             : LongInt;
   OutF                                   : Text;

         procedure DrawBaseGraph;
         begin
            with SunGraph.GraphDraw do begin
               Year := wYear;
               MaxVertAxis := 24.0;
               ForceVertCycleSize := 4;
               ForceVertTickIncr := 1;
               Day1 := 1;
               Day2 := 31;
               Month1 := 1;
               Month2 := 12;
               Year1 := wYear;
               Year2 := wYear;
               SunGraph.GraphDraw.ShowYears := false;
               GraphAxes := XTimeYFullGrid;
               if FullTable then VertLabel := 'Sunrise, Daylight, Sunset (hrs)'
               else VertLabel := 'Daylight (hrs)';
               SunGraph.SetUpGraphForm;
               BottomMargin := 45;
            end {with};
         end {proc DrawBaseGraph};


         procedure DrawSunGraph;
         var
            i    : integer;
            Rise,
            Setting,Daylight : float64;
            rfile,rfile2,rfile3  : file;
            v                    : tGraphPoint32;
         begin
            SunGraph.OpenDataFile(rfile);
            if MDDef.KoppenOpts.ShowSunRiseSet then begin
               SunGraph.OpenDataFile(rfile2);
               SunGraph.OpenDataFile(rfile3);
            end;
            StartProgress('Sunrise');
            for i := 0 to 365 do begin
               if (i mod 10) = 0 then UpdateProgressBar( i / 365 );
               SunCalculate(Lat,Long,ZenithDistance[1],NS+i,true,Rise);
               SunCalculate(Lat,Long,ZenithDistance[1],NS+i,false,Setting);
               if Math.IsNAN(Rise) or Math.IsNan(Setting) then begin
               end
               else  begin
                  Rise := Rise - DifGren;
                  Setting := Setting - DifGren;
                  DayLight := abs(Setting - Rise);
                  v[1] := JulDay(1,1,wYear) + (i);
                  v[2] := Daylight;
                  BlockWrite(rfile,v,1);
                  if MDDef.KoppenOpts.ShowSunRiseSet then
                  begin
                     v[2] := Rise;
                     BlockWrite(rfile2,v,1);
                     v[2] := Setting;
                     BlockWrite(rfile3,v,1);
                  end;
               end {if};
            end {for i};
            CloseFile(rfile);
            SunGraph.GraphDraw.FileColors256[1] := claNavy;
            if MDDef.KoppenOpts.ShowSunRiseSet then begin
               CloseFile(rfile2);
               CloseFile(rfile3);
               SunGraph.GraphDraw.FileColors256[2] := claLime;
               SunGraph.GraphDraw.FileColors256[3] := claRed;
            end;
            EndProgress;
            SunGraph.RedrawDiagram11Click(Nil);
         end;


         procedure TableHeading;
         var
            MenuStr : ShortString;
         begin
            if FullTable then MenuStr :=
                    '               Morning Twilight                            Evening Twilight' + MessLineBreak + 'Date          Astron  Naut Civil    Sunrise     Sunset   Civil  Naut  Astron' + MessLineBreak
            else  MenuStr := LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod) + MessLineBreak +  'Date       Sunrise     Sunset' + MessLineBreak;
            writeln(outf);
            writeln(Outf,MenuStr);
            writeln(outf);
         end;


var
   WhatIsit,MenuStr : ShortString;
begin {proc SunRiseSunSet}
   DecodeDate(now,wYear,wmonth,wDay);
   WhatIsIt := 'Sunrise/sunset';
   //if TheSun then WhatIsIt := 'Sunrise/sunset' else WhatIsIt := 'Moonrise/Moonset';

   if (Lat > pred(MaxInt)) then GetLatLong(WGS84DatumConstants,WhatIsIt,Lat,Long);
   Year := wYear;
   Month := wMonth;
   Day := wDay;

   if MDDef.TZFromLong then  DifGren :=  round(-Long / 15.0)
   else DifGren := MDDef.UTCOffset;

   Ns := (275 * 1 div 9) - ((1 + 9) div 12) * ((1 + trunc(Year - 4 * (Year div 4) + 2) div 3)) + 1 - 30;

   if DoGraphical then begin
      SunGraph := tThisBaseGraph.Create(Application);
      with SunGraph.GraphDraw do begin
         if (not FullTable) then begin
            SunGraph.ClientHeight := MDDef.KoppenOpts.KopHeight;
            SunGraph.ClientWidth := MDDef.KoppenOpts.KopWidth;
         end;
         DrawBaseGraph;
         SunGraph.Caption := 'Daylight at ' + LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod);
         DrawSunGraph;
      end;
   end
   else begin
      assignFile(Outf,MDTempDir + 'Suntable.txt');
      rewrite(outf);
      writeln(OutF,LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod));
      writeln(Outf);
      write(Outf,' all times local standard time ( = UT ');
      if (DifGren >= 0) then WRITE(Outf,'+');
      writeln(Outf,DifGren:5,' h )');

      TableHeading;
      Month := 1;
      Day := 1;
      JDay := JulDay(Month,Day,Year);
      NDays := 366;
      ShowHourglassCursor;

      for D := Ns to pred(Ns + NDays) do begin
         if (D mod 25 = 0) then TableHeading;
         CalDat(JDay,Month,Day,Year);
         inc(JDay);
         MenuStr := AddDayMonthLeadingZero(Month) + '/' + AddDayMonthLeadingZero(Day) + '/'  + IntegerToString(Year,4);
         MenuStr := MenuStr + '   ';
         //if TheSun then begin
            if FullTable then j := 4 else j := 1;
            for i := j downto 1 do begin
               SunCalculate(Lat,Long,ZenithDistance[i],D,true,UT);
               MenuStr := MenuStr + HoursMinutesString(UT - DifGren) + '   ';
            end {for i};
            MenuStr := MenuStr + '   ';
            for i := 1 to j do begin
               SunCalculate(Lat,Long,ZenithDistance[i],D,false,UT);
               MenuStr := MenuStr + HoursMinutesString(UT - DifGren) + '   ';
            end {for i};
        // end;
         writeln(Outf,MenuStr);
      end {for D};
      closeFile(Outf);
      ShowDefaultCursor;
      if DoGraphical then SunGraph.Close;
      QuickOpenEditWindow(MDTempDir + 'Suntable.txt',WhatIsIt + ' times');
      DeleteFile(MDTempDir + 'Suntable.txt');
   end {with};
end  {proc SunRiseSunSet};



procedure AnnualSunRiseGeometry(MapForm : tMapForm; Latitude,Longitude : float64; AddSunriseSunset: boolean = false);

      function QuickGetSunriseSunSet(Latitude,Longitude : float64; tz,day : integer; var az,alt,sunrise,sunset,sunappears,sundisappears,DurationDayLight : float64) : boolean;
      begin
         {$IfDef RecordGetSunriseSet} WriteLineToDebugFile('GetSunriseSunSet, day: ' + IntToStr(Day) + '   ' + LatLongDegreeToString(Latitude,Longitude)); {$EndIf}
         if Sun_Position.ComputeSunPosition(latitude,longitude,MDDef.ObsAboveGround,tz,day,az,alt,sunrise,sunset) then begin
            Result := true;
            SunAppears := Sunrise - MDDef.SunlightPrecision/60;
            SunDisappears := Sunset + MDDef.SunlightPrecision/60;
            DurationDayLight := SunSet - Sunrise;
         end
         else begin
            Result := false;
         end;
      end;

var
   Month,Day,Year,db,tz,JDay : integer;
   wYear,wmonth,wDay : word;
   Results : tStringList;
   az,alt,SunRise,SunSet,
   SunAppears,SunDisappears,DurationDayLight : float64;
   fName : PathStr;
   TheGraph : TThisbasegraph;
begin
   ShowHourglassCursor;
   tz := round(Longitude / 15);
   if MDDef.VerifyTimeZone then ReadDefault('Time zone', tz);
   Results := tStringList.Create;
   Results.Add('JULIAN_DAY,DATE,SUNRISE,SUNSET,DAY_HOURS,SUN_ANGLE,SUNRISE_F,SUNSET_F');
   DecodeDate(now,wYear,wmonth,wDay);
   for JDay := 1 to 365 do begin
      if QuickGetSunRiseSunSet(Latitude,Longitude,tz,JDay,az,alt,sunrise,sunset,sunappears,sundisappears,DurationDayLight) then begin
         CalDat(JDay, Month,Day,Year);
         Results.Add(IntToStr(JDay) + ',' + IntToStr(Month) + '/' + IntToStr(Day) + '/' + IntToStr(wYear)   + ',' + HoursMinutesString(Sunrise) + ',' + HoursMinutesString(SunSet) + ',' +
             RealToString(DurationDayLight,-6,2) + ',' + RealToString(AZ,-10,2) + ',' + RealToString(Sunrise,-6,-2) + ',' + RealToString(Sunset,-6,-2));
      end;
   end;
   fName := Petmar.NextFileNumber(MDTempDir, 'sunrise_',DefaultDBExt);
   db := MapForm.StringListToLoadedDatabase(Results,fName);
   TheGraph := GISDB[db].CreateScatterGram('JULIAN_DAY','DAY_HOURS',false,'Daylight duration at ' + LatLongDegreeToString(Latitude,Longitude,VeryShortDegrees));
   TheGraph.GraphDraw.MaxVertAxis := 24;
   TheGraph.GraphDraw.MinVertAxis := 0;
   TheGraph.GraphDraw.MaxHorizAxis := 365;
   TheGraph.SettingsChanged := true;
   TheGraph.LLcornerText := LatLongDegreeToString(Latitude,Longitude,VeryShortDegrees);
   TheGraph.RedrawDiagram11Click(Nil);
   if AddSunriseSunset then begin
      GISdb[db].AddSeriesToScatterGram(TheGraph,clLime,'JULIAN_DAY','SUNRISE_F');
      GISdb[db].AddSeriesToScatterGram(TheGraph,clBlue,'JULIAN_DAY','SUNSET_F');
      TheGraph.GraphDraw.LegendList := tStringList.Create;
      TheGraph.GraphDraw.LegendList.Add('Hours daylight');
      TheGraph.GraphDraw.LegendList.Add('Time of sunrise');
      TheGraph.GraphDraw.LegendList.Add('Time of sunset');
   end;
   if (MDdef.ProgramOption = ExpertProgram) then GISDB[db].dbtablef.ShowStatus
   else CloseAndNilNumberedDB(db);
   ShowDefaultCursor;
end;


function SunAndHorizon(BaseMap : tMapForm; DEM : integer; Latitude,Longitude : float64; ShowToday : boolean = true; ShowKeyDays : boolean = true) : TNetForm;
var
   SunRise,SunSet,
   SunAppears,SunDisappears,DurationDirectLight,DurationDayLight,
   az,alt, hrtime : float64;
   Month,DayMonth,Year,Day,SymSize,NumDay,tz,xd,yd,SunUp,SunMasked,db : integer;
   TStr : ShortString;
   SunResults,
   Results : tStringList;
   BlockAngles : tBlockAngles;
   fName : PathStr;
   TheGraph : TThisBaseGraph;

      procedure ComputeADay(Day : integer; Color : tPlatformColor);
      var
         hr : integer;
         BlockAngle,BlockLength,BlockLat,BlockLong : float64;
      begin
         {$IfDef RecordHorizon} WriteLineToDebugFile('Start JDay=' + IntToStr(Day)); {$EndIf}
         Inc(NumDay);
         hrtime := 0;
         SunUp := 0;
         SunMasked := 0;
         repeat
            Sun_Position.ComputeSunPosition(latitude,longitude,hrtime,tz,day,az,alt,sunrise,sunset);
            if (alt > 0) then begin
               if (DEM <> 0) then begin
                  DEMGlb[DEM].HorizonBlocking(latitude,longitude,Az,MDDef.HorizonLength,2,BlockAngle,BlockLength,BlockLat,BlockLong,saDEMGrid);
                  if (BlockAngle <  Alt) then begin
                     TStr := 'above horizon';
                     inc(SunUp);
                  end
                  else begin
                     TStr := 'masked by topography';
                     inc(SunMasked);
                  end;
                  if (SunResults <> Nil) then SunResults.Add(RealToString(hrtime,-12,3) + ',' + HoursMinutesString(hrtime) + ',' + RealToString(az,13,2) + ',' + RealToString(alt,13,2) + ',' + TStr);
               end;
               if MDDef.SolarPathMap then Result.nd.PlotPointOnNet(LinePlot,Alt,Az,ASymbol(FilledBox,Color,SymSize),xd,yd);
            end;
            hrtime := hrtime + MDDef.SunlightPrecision / 60;
         until ((alt < 0) and (hrTime > 12)) or (hrTime > 24);

         if MDDef.SolarPathMap then begin
            hr := 0;
            repeat
               Sun_Position.ComputeSunPosition(latitude,longitude, hr,tz,day,az,alt,sunrise,sunset);
               if (alt > 0) then begin
                  Result.nd.LabelPointOnNet(Alt,Az,hr);
               end;
               hr := hr + 2;
            until (hr > 24);
         end;
         {$IfDef RecordHorizon} WriteLineToDebugFile('End JDay=' + IntToStr(Day)); {$EndIf}
      end;

      procedure DoKeyDaze;
      begin
         ComputeADay(AnnualJulianDay(2019,3,21),claGreen);
         ComputeADay(AnnualJulianDay(2019,6,21),claBlue);
         ComputeADay(AnnualJulianDay(2019,12,21),claLime);
      end;

var
  SaveHemi : tHemisphere;
begin
   {$IfDef RecordHorizon} WriteLineToDebugFile('SunAndHorizon in'); {$EndIf}
   SunResults := Nil;
   SaveHemi := MDDef.NetDef.HemiSphereUsed;
   MDDef.NetDef.HemiSphereUsed := Upper;
   if (MDDef.HorizonIncrement < 0.1) then MDDef.HorizonIncrement := 0.1;
   Day := MDDef.SingleJulianDay;
   tz := round(Longitude / 15);
   if MDDef.VerifyTimeZone then ReadDefault('Time zone', tz);
   Result := nil;
   NumDay := 0;
   if (DEM <> 0) then GetBlockAngles(DEM,0,Latitude,Longitude,BlockAngles);
   if (Result = nil) then begin
       Result := TNetForm.Create(Application);
       MDDef.NetDef.DrawGridCircles := ngPolar;
   end;
   Result.Caption := 'Sun Positions, equinox/solstice at ' + LatLongDegreeToString(Latitude,Longitude);
   Result.nd.LLcornerText := LatLongDegreeToString(Latitude,Longitude,VeryShortDegrees);
   Result.nd.NewNet;

   SymSize := 2;
   if ShowToday then ComputeADay(Day,claRed);
   if ShowKeyDays then DoKeyDaze;

   if (MDDef.SunlightSingleDay = 0) then begin
      {$IfDef RecordHorizon} WriteLineToDebugFile('SunAndHorizon SingleDay'); {$EndIf}
      Results := tStringList.Create;
      Results.Add(LatLongDegreeToString(Latitude,Longitude,MDDef.OutPutLatLongMethod));
      if (Results <> Nil) then begin
         Results.Add('');
         Results.Add('');
      end;
      CalDat(JulDay(1,1,2019) + pred(Day), Month,DayMonth,Year);

      ShowHourglassCursor;
      if (DEM <> 0) then begin
         SunResults := tStringList.Create;
         SunResults.Add('Hour,Time,Azimuth,Altitude,Blocking');
         ComputeADay(Day, claRed);
         Results.Add('Solar Positions, Julian day: ' + IntToStr(Day) + '  (' + IntToStr(Month) + '/' + IntToStr(DayMonth) + '/' + IntToStr(Year) + ')');
         Results.Add('');
         GetSunRiseSunSet(Latitude,Longitude,tz,Day,az,alt,sunrise,sunset,sunappears,sundisappears,DurationDayLight,BlockAngles);
         Results.Add( '');
         Results.Add('Sunrise:           ' + HoursMinutesString(SunRise));
         Results.Add('Sun above horizon: ' + HoursMinutesString(SunAppears));
         Results.Add('Sun below horizon: ' + HoursMinutesString(SunDisappears));
         Results.Add('Sunset:            ' + HoursMinutesString(SunSet));
         Results.Add('Duration daylight: ' + RealToString(DurationDayLight, -13,2) + ' hrs');
         Results.Add('Direct solar illumination: ' + RealToString(SunUp*(MDDef.SunlightPrecision / 60), -15,2) + ' hrs');
         Results.Add('Terrain masking: ' +  RealToString(SunMasked*(MDDef.SunlightPrecision / 60), -10,2) + ' hrs'  );
         hrtime := trunc(SunRise);
      end
      else begin
         hrtime := 0;
         ComputeADay(Day, claRed);
      end;
      if (Results <> Nil) then Petmar.DisplayAndPurgeStringList(Results,'Sun Position');
      if (DEM <> 0) then begin
         fName := Petmar.NextFileNumber(MDTempDir, 'terrain_mask_' + LatLongToStringForFileName(latitude,longitude)  + '_',DefaultDBExt);
         BaseMap.StringListToLoadedDatabase(SunResults,fName);
      end;
   end
   else if (MDDef.SunlightSingleDay = 1) then begin
      {$IfDef RecordHorizon} WriteLineToDebugFile('SunAndHorizon not SingleDay'); {$EndIf}
      StartProgress('Sun position');
      SunResults := tStringList.Create;
      SunResults.Add('JULIAN_DAY,SUNRISE,SUN_UP,SUN_DOWN,SUNSET,DAYLIGHT,DIRECT_ILL,TERR_MASK');
      for day := MDDef.FirstSunlightDay to MDDef.LastSunlightDay do begin
         UpdateProgressBar(Day/365);
         ComputeADay(Day, claRed);
         GetSunriseSunSet(Latitude,Longitude,tz,Day,az,alt,sunrise,sunset,sunappears,sundisappears,DurationDayLight,BlockAngles);
         SunResults.Add(IntegerToString(Day) + ',' +
                      HoursMinutesString(Sunrise) + ',' +
                      HoursMinutesString(SunAppears) + ',' +
                      HoursMinutesString(SunDisappears) + ',' +
                      HoursMinutesString(SunSet) +  ',' +
                      RealToString(DurationDayLight, 13,2) + ',' +
                      RealToString(SunUp*(MDDef.SunlightPrecision / 60), 11,2) + ',' +
                      RealToString(SunMasked*(MDDef.SunlightPrecision / 60), 10,2) );
      end;
      EndProgress;
      fName := Petmar.NextFileNumber(MDTempDir, 'terrain_mask_',DefaultDBExt);
      db := BaseMap.StringListToLoadedDatabase(SunResults,fName);
      {$IfDef RecordHorizon} WriteLineToDebugFile('DB created and loaded'); {$EndIf}
      TheGraph := GISdb[db].CreateScatterGram('JULIAN_DAY','DAYLIGHT',false,'Daylight duration at ' + LatLongDegreeToString(Latitude,Longitude,VeryShortDegrees));
      TheGraph.GraphDraw.MaxVertAxis := 24;
      TheGraph.GraphDraw.MinVertAxis := 0;
      TheGraph.GraphDraw.MaxHorizAxis := 365;
      TheGraph.SettingsChanged := true;
      TheGraph.LLcornerText := LatLongDegreeToString(Latitude,Longitude,VeryShortDegrees);
      {$IfDef RecordHorizon} WriteLineToDebugFile('Graph created'); {$EndIf}
      GISdb[db].AddSeriesToScatterGram(TheGraph,clLime,'JULIAN_DAY','DIRECT_ILL');
      GISdb[db].AddSeriesToScatterGram(TheGraph,clBlue,'JULIAN_DAY','TERR_MASK');
      TheGraph.GraphDraw.LegendList := tStringList.Create;
      TheGraph.GraphDraw.LegendList.Add('Hours direct illumination');
      TheGraph.GraphDraw.LegendList.Add('Hours of terrain masking');

      TheGraph.RedrawDiagram11Click(Nil);
      if (MDDef.ProgramOption <> ExpertProgram) then GISdb[db].Destroy;
   end;
   if (Result <> Nil) then Result.UpdateDisplay;
   ShowDefaultCursor;
   MDDef.NetDef.HemiSphereUsed := SaveHemi;
   {$IfDef RecordHorizon} WriteLineToDebugFile('SunAndHorizon out'); {$EndIf}
end;


initialization
finalization
    {$IfDef RecordSolarPosition} WriteLineToDebugFile('RecordSolarPosition active in sun_position');{$EndIf}
    {$IfDef RecordGetSunriseSet} WriteLineToDebugFile('RecordGetSunriseSet active in sun_position');{$EndIf}
    {$IfDef RecordSunlightMaps}  WriteLineToDebugFile('RecordSunlightMaps active in sun_position');{$EndIf}
    {$IfDef RecordBlockAngleDifference} WriteLineToDebugFile('RecordBlockAngleDifference active in sun_position  (major slowdown)');{$EndIf}
    {$IfDef RecordFullSunlightMaps} WriteLineToDebugFile('RecordFullSunlightMaps active in sun_position (major slowdown)');{$EndIf}
    {$IfDef TimeSunlightMap} WriteLineToDebugFile('TimeSunlightMap active in sun_position');{$EndIf}
end.



