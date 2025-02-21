{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{      include file for basemap          }
{________________________________________}


      function GetEPSGforUTMZone(HDatum : shortstring; LatHemi : ANSIchar; UTMzone : integer) : integer;
      begin
         if (HDatum = 'ETR89') then Result := 25800
         else if (HDatum = 'NAD83') then Result := 26900
         else if (HDatum = 'WGS84') then Result := 32600;
         Result := Result + UTMZone;
         if LatHemi = 'S' then Result := Result  + 100;
      end;



     function GetEPSGforVerticalDatumCode(PrimaryMapDatum : tMapProjection) : word;
     begin
        if PrimaryMapDatum.V_DatumCode = 'NAVD88' then Result := VertCSNAVD88
        else if PrimaryMapDatum.V_DatumCode = 'EGM96' then Result := VertCSEGM96
        else if PrimaryMapDatum.V_DatumCode = 'EGM2008' then Result := VertCSEGM2008
        else if PrimaryMapDatum.V_DatumCode = 'WGS84 ellipsoid' then Result := VertCSWGS84
        else Result := 0;
     end;


     function GetEPSGforUTMDatumCode(PrimaryMapDatum : tMapProjection) : word;
     begin
       Result := 32600; {WGS84 default}
       if (PrimaryMapDatum.h_DatumCode = 'WGS72') then Result := 32200
       else if (PrimaryMapDatum.h_DatumCode = 'NAS-C') or (PrimaryMapDatum.h_DatumCode = 'NAD83') then Result := 26900
       else if (PrimaryMapDatum.h_DatumCode = 'NAR-C') or (PrimaryMapDatum.h_DatumCode = 'NAD27') then Result := 26700
       else if (PrimaryMapDatum.h_DatumCode = 'ETRS89') or (PrimaryMapDatum.h_DatumCode = 'ETR89') then Result := 25800
       else if (PrimaryMapDatum.h_DatumCode <> 'WGS84') then MessageToContinue('This is ' + PrimaryMapDatum.h_DatumCode + ' ' + GeotiffDatumDefaultString);
       if (PrimaryMapDatum.LatHemi = 'S') then Result := Result + 100;
       Result := Result + PrimaryMapDatum.projUTMZone;
     end;


     function GetEPSGforGeoDatumCode(PrimaryMapDatum : tMapProjection) : word;
     begin
       Result := 4326; {WGS84 default}
       if (PrimaryMapDatum.h_DatumCode = 'WGS72') then Result := 4322
       else if (PrimaryMapDatum.h_DatumCode = 'NAS-C') or (PrimaryMapDatum.h_DatumCode = 'NAD83') then Result := 4267
       else if (PrimaryMapDatum.h_DatumCode = 'NAR-C') then Result := 4269
       else if (PrimaryMapDatum.h_DatumCode = 'ETRS89')  or (PrimaryMapDatum.h_DatumCode = 'ETR89') then Result := 4258
       else if (PrimaryMapDatum.h_DatumCode <> 'WGS84') then MessageToContinue('This is ' + PrimaryMapDatum.h_DatumCode + ' ' + GeotiffDatumDefaultString);
     end;

     function GetCombinedEPSG(PrimaryMapDatum : tMapProjection) : shortstring;
     begin
        Result := '';
        if PrimaryMapDatum.PName = UTMEllipsoidal then Result := IntToStr(GetEPSGforUTMDatumCode(PrimaryMapDatum) )
        else if PrimaryMapDatum.PName = PlateCaree then Result := IntToStr(GetEPSGforGeoDatumCode(PrimaryMapDatum) )
        else exit;
        if PrimaryMapDatum.V_DatumCode <> '' then Result := Result + '+' + IntToStr(GetEPSGforVerticalDatumCode(PrimaryMapDatum));
     end;



{$If Defined(RecordDefineDatum) or Defined(ProjectionInfoAllowed)}
   procedure tMapProjection.LogDatumInfo(Why : shortstring);
   begin
       WriteLineToDebugFile(Why);
       WriteLineToDebugFile('UTM zone=' + IntToStr(projUTMZone) + '  h_EllipsCode=' + h_EllipsCode);
       WriteLineToDebugFile('h_XDat=' + IntToStr(h_XDat));
       WriteLineToDebugFile('a=' + RealToString(a,-12,-2));
       WriteLineToDebugFile('h_f=' + RealToString(h_f,-12,-6));
       WriteLineToDebugFile('e2=' + RealToString(e2,-18,-8) + '  ep2=' + RealToString(ep2,-18,-8));
       WriteLineToDebugFile('s1=' + RealToString(UTM_S1,-18,-8) + '   s2=' + RealToString(s2,-18,-8));
       WriteLineToDebugFile('s4=' + RealToString(s4,-18,-8) +'  s6=' + RealToString(s6,-18,-8));
       WriteLineToDebugFile('M0=' + RealToString(M0,-18,-8) +'  Long0=' +  IntToStr(round(UTMZoneCentralLong(projUTMZone))));
   end;
{$EndIf}


    {$IfDef RecordTMParameters}
    procedure tMapProjection.TMdetails(Why : shortstring);
    begin
        WriteLineToDebugFile('');
        ShortProjInfo(Why);
        WriteLineToDebugFile('Long0=' + RadToDegString(Long0));
        WriteLineToDebugFile('M0=' + RealToString(M0,-18,-6));
        WriteLineToDebugFile('EP2=' + RealToString(EP2,-18,-6));
        WriteLineToDebugFile('E1=' + RealToString(E1,-18,-6));
        WriteLineToDebugFile('E2=' + RealToString(E2,-18,-6));
        WriteLineToDebugFile('UTM_s1=' + RealToString(UTM_s1,-18,-6));
        WriteLineToDebugFile('s2=' + RealToString(s2,-18,-6));
        WriteLineToDebugFile('s4=' + RealToString(s4,-18,-6));
        WriteLineToDebugFile('s6=' + RealToString(s6,-18,-6));
        WriteLineToDebugFile('MUPRM=' + RealToString(MUPRM,-18,-6));
        WriteLineToDebugFile('TM_Phi2=' + RealToString(TM_Phi2,-18,-6));
        WriteLineToDebugFile('Phi4=' + RealToString(Phi4,-18,-9));
        WriteLineToDebugFile('Phi6=' + RealToString(Phi6,-18,-9));
        WriteLineToDebugFile('');
    end;
    {$EndIf}



   procedure tMapProjection.ShortProjInfo(Why : shortstring);
   var
      TStr : shortstring;
   begin
       if PName = UTMEllipsoidal then TStr := ' UTM zone=' + IntToStr(projUTMZone) + LatHemi
       else TStr := '';
       WriteLineToDebugFile(ProjDebugName +  ' ' + Why + ' ' +
                       GetProjName + ' ' +
                       ' ProjMapScale=' + RealToString(ProjMapScale,-12,-4) +
                       ' ellipse=' + H_datumCode +
                       ' h_f=' + RealToString(h_f,-12,-6) +
                       ' a=' + RealToString(a,-18,-2) +
                       //' s2=' + RealToString(s2,-18,-6) +
                       //' TM_phi2=' + RealToString(TM_phi2,-18,-6) +
                       ' long0=' + RadToDegString(Long0) +
                       ' lat0=' + RadToDegString(Lat0) +
                       ' false_east=' + RealToString(False_east,-12,-1) +
                       ' false_north=' + RealToString(False_north,-12,-1) +
                       TStr ) ;
   end;



procedure tMapProjection.InverseProjectRadians(x,y : float64; var Lat,Long : float64);
{returns Lat-Long in radians for an x-y in map coordinates}
const
   LatDiffToStop = 0.0000002;
var
   Iteration : integer;
   e1_2,e1_3,e1_4 : float64;
   l,l2,l6,delta, fy, fpy,c1,
   Mu,C2,C3,a1,Theta1,m1,M,Diff,tf,tf1,
   Sign,OldLat,sinc,cosc,Theta,rho,q,t    : float64;
   Reps,i : integer;


   {$IfDef RecordProjectionDetails}  //these are intended (and were used) to debug the equations, using the test cases in the Snyder book
      procedure RecordPolarStereographic;
      begin
         WriteLineToDebugFile('PolarStereographic InverseProject' );
         WriteLineToDebugFile('   LatCent=' + RadToDegString(Lat0,-18,-8));
         WriteLineToDebugFile('   e=' + RealToString(e,-18,-8) + '   e2=' + RealToString(e2,-18,-8));
         WriteLineToDebugFile('   tc=' + RealToString(tc,-18,-8) + '   rho=' + RealToString(rho,-18,-8));
         WriteLineToDebugFile('   mc=' + RealToString(mc,-18,-8) + '   t=' + RealToString(t,-18,-8));
         WriteLineToDebugFile('   x,y=' + RealToString(x,-18,-2) + '/' + RealToString(y,-18,-2) + '   lat,long=' + LatLongRadToString(Lat,Long,MDdef.OutPutLatLongMethod));
      end;

      procedure AzimuthalEquidistantEllipsoidalDebug;
      begin
         WriteLineToDebugFile('AzimuthalEquidistantEllipsoidalDebug InverseProject, in  x,y=' + RealToString(x,-18,-2) + '/' + RealToString(y,-18,-2));
         WriteLineToDebugFile('Iteration ' + IntToStr(Iteration) + ', OldLat=' + LatToString(OldLat/DegToRad,DecDegrees) + ', NewLat=' + LatToString(Lat/DegToRad,DecDegrees) );
         WriteLineToDebugFile('M1=' + RealToString(M1,-18,-4));
         WriteLineToDebugFile('e1=' + RealToString(e1,-18,-9));
         WriteLineToDebugFile('e1_2=' + RealToString(e1_2,-18,-9));
         WriteLineToDebugFile('e1_3=' + RealToString(e1_3,-18,-9));
         WriteLineToDebugFile('e1_4=' + RealToString(e1_4,-18,-9));
         WriteLineToDebugFile('tf1=' + RealToString(tf1,-18,-8));
         WriteLineToDebugFile('tf=' + RealToString(tf,-18,-8));
         WriteLineToDebugFile('M=' + RealToString(M,-18,-4));
         WriteLineToDebugFile('Mu=' + RealToString(Mu,-18,-4));
         WriteLineToDebugFile('Lat=' + RealToString(Lat/Petmar_types.DegToRad,-18,-8));
      end;

   {$EndIf}


begin
   {$IfDef RecordProjectionDetails}
      if PointConvertDebug then begin
         WriteLineToDebugFile('');
         ProjectionParamsToDebugFile('InverseProjectRadians',true);
      end;
   {$EndIf}
   if (PName = PlateCaree) then begin
      Lat := y;
      Long := x;
   end
   else if PName in [UTMEllipsoidal,UK_OS,Finn_GK,GeneralTransverseMercator,IrishGrid] then begin   //Snyder, p.60-64, example p.270
       TransverseMercatorInverseProjectionRadians(X,Y,Lat,Long);
   end
   else if (PName in [LamAzEqAreaEllipsoidal]) then begin
      SubtractFalseEastingsNorthings(x,y);
      //qp is q2
      rho := sqrt(sqr(x/D) + sqr(D*y));
      //rq := a * sqrt(q2/2);
      c := 2 * arcsin(rho / ( 2 * rq));
      q := q2 * (cos(c) * sin(Beta1) + D * y * sin(c) * cos(Beta1) / rho);
      Lat := arcsin(q/2);  //first guess
      repeat
         OldLat := Lat;
         Lat := OldLat + ( sqr(1 - e2 * sqr(sin(OldLat))) / (2 * cos(OldLat)) ) * (q / (1 - e2) - sin(OldLat) / ( 1 - e2 * sqr(sin(OldLat))) + (1 / (2 * e) * ln((1 - e* sin(OldLat)) / (1 + e* sin(OldLat)))) );
      until abs(Lat - OldLat) < LatDiffToStop;
      Long := Long0 + arctan(x * sin(c) / (D * rho * cos(Beta1) * cos(c) - sqr(D)* y * sin(Beta1) * sin(c)));
   end
   else if (PName = AzimuthalEquidistantEllipsoidal) then begin {Snyder, 1987, pp.198-199, p.339 using the Guam code}
      {$IfDef RecordProjectionDetails} if PointConvertDebug then WriteLineToDebugFile('in x,y=' + RealToString(x,-18,-2) + '/' + RealToString(y,-18,-2)); {$EndIf}
      SubtractFalseEastingsNorthings(x,y);
      M1 := M_3_21(Lat0);
      e1 := (1 - sqrt(1 - e2)) / (1 + sqrt(1 - e2));   //Synder equation 3-24, p.17
      e1_2 := sqr(e1);
      e1_3 := e1 * e1_2;
      e1_4 := e1 * e1_3;
      OldLat := Lat0;
      iteration := 0;
      repeat
         inc(iteration);
         tf1 := sqrt(1 - sqr(e2) * sqr(sin(OldLat)));
         tf := sqr(x) * tan(OldLat) * tf1;
         M := M1 + y - (tf / (2 * a));     //Snyder equation 25-30, p.201

         mu := M / (a * (1 - e2 / 4 - 3 * e2 * e2 /64 - 5 * e2 * e2 * e2 / 256));  //Synder equation 7-19, p.47

         Lat := mu + (3 * e1 / 2 - 27 * e1_3 / 32) * sin(2 * mu) +
                     (21 * e1_2 / 16 - 55 * e1_4 / 32) * sin(4 * mu) +
                     (151 * e1_3 / 96) * sin(6 * mu) +
                     (1097 * e1_4 / 512) * sin(8 * mu);                     //Synder equation 3-26, p.17
         Diff := (abs(Lat - OldLat));
         {$IfDef RecordProjectionDetails} if PointConvertDebug then AzimuthalEquidistantEllipsoidalDebug; {$EndIf}
         Oldlat := Lat;
      until (Diff < 0.00001) or (iteration > 5);
      Long := Long0 + x * sqrt(1  - e2 * sqr(sin(Lat))) / ( a * cos(Lat));

      {$IfDef RecordProjectionDetails} if PointConvertDebug then WriteLineToDebugFile('Out: ' + LatLongDegreeToString(Lat/DegToRad,Long/DegToRad)); {$EndIf}
   end
   else if (PName in [LambertConformalConicEllipse]) then begin {Snyder, 1987, pp.109,297}
   //this will need fixes in the southern hemisphere
      SubtractFalseEastingsNorthings(x,y);
      x := x / ProjMapScale;
      y := y / ProjMapScale;
      rho := sqrt(sqr(x) + sqr(rho0-y));
      theta := arctan(x / (rho0-y));
      t := Math.Power(rho / (a * Big_F), 1/n_lcc);
      Long := Long0 + theta / n_lcc;
      Lat := HalfPi - 2 * arcTan(t);
      repeat
         OldLat := Lat;
         Lat := HalfPi - 2 * arcTan(t * Math.Power((1 - e * sin(OldLat)) / (1 + e* sin(OldLat)),0.5*e));
      until abs(Lat - OldLat) < LatDiffToStop;
   end
   else if PName in [AlbersEqAreaConicalEllipsoid] then begin {Snyder, 1987, pp.102, 293-294}
      Lat := 0;
      Long := 0;
      rho := sqrt(sqr(x) + sqr(Rho0 - y));
      q := (C - sqr(rho) * sqr(nalbers) / sqr(a)) / nalbers;
      if  nalbers > 0 then Theta := arctan(x / (Rho0 - y))
      else Theta := arctan(-x / (y - Rho0));

      Lat := ArcSin(0.5 * q);
      Reps := 0;
      repeat
         inc(Reps);
         if Reps > 500 then begin
            Lat := -99;
            Long := -990;
            exit;
         end;
         OldLat := Lat;
         Lat := 1 - e* sin(OldLat);
         Lat := Lat / ( 1 + e* sin(OldLat));
         Lat := ln(Lat);
         Lat := 0.5 / e* Lat;
         Lat := -sin(OldLat) / (1 - e2 * sqr(sin(OldLat))) + Lat;
         Lat := (q / (1 - e2)) + Lat;
         Lat := Lat / ( 2 * cos(OldLat)) ;
         Lat := sqr(1 - e2 * sqr(sin(OldLat))) * Lat;
         Lat := OldLat + Lat;
      until abs(Lat - OldLat) < LatDiffToStop;
      Long := Long0 + Theta / nalbers;
   end
   else if (PName = MercatorEllipsoid) then begin
     t := exp(-y / a);
     Lat := HalfPi - 2 * arcTan(t);
     repeat
        OldLat := Lat;
        Lat := HalfPi - 2 * ArcTan(t * Math.Power( ((1 - e* sin(Lat)) /
                 (1 + e* sin(Lat))),0.5 * e));
     until abs(Lat - OldLat) < LatDiffToStop;
     Long := Long0 + x / a;
   end
   else if (PName = PolarStereographicEllipsoidal) then begin {Snyder, 1987, p.162, p.318}
        {lat cent is standard parallel}
      if (Lat0 > 0) then Sign := 1
      else begin
         Sign := -1;  //per Snyder, 1987, p.161, used with x, y, Lat, Long, LatCent, LongCent
         x := -x;
         y := -y;
         Lat0 := -Lat0;
         Long0 := -Long0;
      end;
      rho := sqrt(sqr(x) + sqr(y));   //no need to use sign, since x and y are squared
      //tc := Math.Tan(QuarterPi - 0.5 * Lat0) / Math.Power( (1 - esinLatCent) / (1 + e * sinLatCent),0.5 * e);
      //mc := cosLatCent / sqrt(1 - e * sqr(sinLatCent));
      t := rho * tc / a / mc;

      Lat := HalfPi - 2 * ArcTan(t);
      repeat
         OldLat := Lat;
         Lat := HalfPi - 2 * ArcTan(t * Math.Power( ((1 - e * sin(Lat)) / (1 + e * sin(Lat))),0.5 * e));
      until abs(Lat - OldLat) < LatDiffToStop;

      if (abs(Y) > 0.0001) then begin
         Long := Long0 + ArcTan2(x, -y);
      end
      else begin
         if (x > 0) then Long := -HalfPi else Long := HalfPi;
      end;

      if (Sign < 0) then begin
         Long := -Long;
         Lat := -Lat;
         Lat0 := -Lat0;
         Long0 := -Long0;
      end;

      {$IfDef RecordProjectionDetails} if PointConvertDebug then RecordPolarStereographic; {$EndIf}
   end
   else if (PName = SinusEllipsoidal) then begin {Snyder, 1987, p.248, example p.366}
      //these two could be stored
      Mu := y / (a * (1 - e2/4 - 3 *sqr(e2)/64 - 5*e2*e2*e2/256));
      e1 := (1 - sqrt(1-e2)) / (1 + sqrt(1-e2));

      Lat := Mu + (3*e1/2 - 27*e1*sqr(e1)/32) * sin(2*mu)
                + (21 * sqr(e1)/16  - 55 * sqr(sqr(e1))/32) * sin(4*mu)
                + (151 * e1*sqr(e1)/96) * sin(6*mu)
                + (1097 * sqr(sqr(e1))/512) * sin(8*mu);
      Long := Long0 + x * sqrt(1 - e2 * sqr(sin(Lat))) / a / cos(Lat);
   end
   else if (PName = VanDerGrinten) then begin {Snyder, 1987, 241-242, 364-365}
      x := x / Pi / a;
      y := y / Pi / a;
      c1 := -abs(y) * (1 + sqr(x) + sqr(y));
      c2 := c1 - 2 * sqr(y) + sqr(x);
      c3 := -2 * c1 + 1 + 2 * sqr(y) + sqr(sqr(x) + sqr(y));
      d := sqr(y) / c3 + (2 * c2 * sqr(c2) / sqr(c3) / c3 - 9 * c1 * c2 / sqr(c3)) / 27;
      a1 := (c1 - sqr(c2) / 3 / c3) / c3;
      m1 := 2 * sqrt( - a1 / 3);
      if (abs(a1) < 0.00000001) or (abs(m1) < 0.000000001) then begin
         Lat := Lat0;
         Long := Long0;
         exit;
      end;
      Theta1 := 0.33333333 * arccos(3 * d / a1 / m1);
      Lat := Pi * (-m1 * cos(Theta1 + 0.3333333 * Pi) - c2 / 3 / c3);

      if Y < 0 then Lat := -Lat;
      if abs(x) < 0.00001 then Long := 0
      else Long := Pi * (sqr(x) + sqr(y) - 1 + sqrt(1 + 2 * (sqr(x) - sqr(y)) + sqr(sqr(x) + sqr(y)))) / (2 * x) + Long0;
   end
   else if (PName = EquidistantCylindrical) then begin {Snyder, 1987, p.91}
      Lat := y / a;
      Long := Long0 + x / (a * CosPhi1);
   end
   else if (PName = CylindricalEqualArea) then begin
      Lat := Lat0 + arcsin((y / a) * cosLatCent);
      Long := Long0 + x / a / CosLatCent;
   end
   else if (PName = CylindricalEqualAreaEllipsoidal) then begin {Snyder, 1987, p.81, 281}
//

   end
   else if (PName = Cassini) then begin  {Snyder, 1987, p.94-95, p.288}
      d := y / a + Lat0;
      Long := Long0 + ArcTan(Math.Tan(x / a) / Cos(d));
      Lat := ArcSin(Sin(D) * cos(x / a));
   end
   else if (PName = SinusProj) then begin  {Snyder, 1987, p.248}
      Lat := y / a;
      Long := Long0 + x / (a * cos(Lat));
   end
   else if (PName = Mollweide) then begin  {Snyder, 1987, p.251}
      Theta := arcsin(y / (sqrt_2 * a));
      Lat := arcsin( (2*Theta + sin(2*Theta)) / Pi);
      if abs(Cos(Theta)) < 0.0001 then Long := 0
      else Long := Long0 + Pi * x / (sqrt(8) * a * cos(Theta));
   end
   else if (PName in [Mercator,WebMercator]) then begin {Snyder, 1987, p.44}
      Lat := arctan(sinh(y / a));
      Long := Long0 + x / a;
   end
   else if (PName = MillerCylindrical) then begin {Snyder, 1987, p.88}
      Lat := arctan(sinh(0.8*y / a)) / 0.8;
      Long := Long0 + x / a;
   end
   else if (PName = EqualEarth) then begin
      x := x / a;
      y := y / a;

      l := y;
      l2 := l * l;
      l6 := l2 * l2 * l2;

      for i := 0 to 12 do begin
         fy := l * (eeA1 + eeA2 * l2 + l6 * (eeA3 + eeA4 * l2)) - y;
         fpy := eeA1 + 3 * eeA2 * l2 + l6 * (7 * eeA3 + 9 * eeA4 * l2);
         delta := fy / fpy;
         l2 := l * l;
         l6 := l2 * l2 * l2;
         if (abs(delta) < 0.00001) then break;
      end;
      Lat := eeM * x * (eeA1 + 3 * eeA2 * l2 + l6 * (7 * eeA3 + 9 * eeA4 * l2)) / cos(l);
      Long := Long0 + arcsin(sin(l) / eeM);
   end
   else begin
     if (PName = HammerProj) then d := 0.5 else d := 1;  //rho := sqrt(sqr(0.5 * x) + sqr(Y))
     rho := sqrt(sqr(d * x) + sqr(Y));
     if rho < 0.00001 then begin
        Lat := Lat0;
        Long := Long0;
     end
     else begin
         case PName of
            HammerProj    : c := ArcSin(rho / a);
            OrthoProj     : c := ArcSin(rho / a);
            LamAzEqArea   : c := 2 * ArcSin(rho / (2 * a));
            Gnomonic      : c := arcTan(rho / a);
            OldStereographic : c := 2 * arcTan(rho / (2 * a));
            SphericalStereographic : c := 2 * arcTan(rho / (2 * a * ProjMapScale));
         end {case};
         sinc := sin(c);
         cosc := cos(c);
         if ((PName in [LamAzEqArea]) and (rho > 1.5 * a))or ((PName in [OrthoProj,HammerProj,Gnomonic]) and (rho > a)) or ((PName in [OldStereographic]) and (rho > (2*a))) then begin
            Lat := -9999;
            Long := -9999;
         end
         else begin
            if (PName = Gnomonic) then begin  {Snyder, 1987, p.167}
               Lat := arcSin(cosc * sinLatCent + y * sinc * cosLatCent / rho);
               if round(Long0 / Petmar_types.DegToRad) = 90 then Long := Long0 + arcTan(x / -y)
               else if round(Long0 / Petmar_types.DegToRad) = -90 then Long := Long0 + arcTan(x / y)
               else Long := Long0 + arctan(x * sinc / (rho * CosLatCent * cosc - y * sinLatCent * sinc));
            end
            else begin
               Lat := arcsin(cosc*SinLatCent + (y * sinc * CosLatCent / rho));
               Sign := (rho * CosLatCent * cosc - y * SinLatCent * sinc);
               Long := Long0 + ArcTan(x * sinc / Sign) / d;
               if (Sign < 0) then Long := Long + Pi;
            end;
            while Long < Pi do Long := Long + TwoPi;
            while Long > Pi do Long := Long - TwoPi;
         end;
     end;
   end;
end;


procedure tMapProjection.ForwardProjectRadians(Lat,Long : float64; var x,y : float64; Check : boolean = true);
{computes x,y coordinates from Lat,Long}
var
   T,tf,rho,Beta,l,l2,l6,M,esinlat,
   Theta,DeltaTheta,ThetaPrime,G,Q,P,
   HalfLambdaDif,K,SinPhi,CosPhi,x1,y1,K1,
   LambdaDif,SinLambdaDif,CosLambdaDif,CosC,B       : float64;

           {$IfDef RecordProjectionDetails}   //these are intended (and were used) to debug the equations, using the test cases in the Snyder book
              procedure PolarStereographicDebug;
              begin
                 WriteLineToDebugFile('PolarStereographicDebug tMapProjection.ForwardProject ' + LatLongRadToString(Lat,Long));
                 WriteLineToDebugFile('LambdaDif=' + RealToString(LambdaDif/DegToRad,-18,-6));
                 WriteLineToDebugFile('t=' + RealToString(t,-18,-6));
                 WriteLineToDebugFile('tc=' + RealToString(tc,-18,-6));
                 WriteLineToDebugFile('mc=' + RealToString(mc,-18,-6));
                 WriteLineToDebugFile('rho=' + RealToString(rho,-18,-6));
                 WriteLineToDebugFile('x=' + RealToString(x,-12,-2) + '   y=' + RealToString(y,-12,-2));
              end;

               procedure LamAzEqAreaDebug;
               begin
                  WriteLineToDebugFile('tMapProjection.ForwardProject ' + LatLongRadToString(Lat,Long));
                  WriteLineToDebugFile('qp=' + RealToString(q2,-12,-8));
                  WriteLineToDebugFile('q=' + RealToString(q,-12,-8));
                  WriteLineToDebugFile('Beta1=' + RealToString(Beta1/DegToRad,-12,-8));
                  WriteLineToDebugFile('Beta=' + RealToString(Beta/DegToRad,-12,-8));
                  WriteLineToDebugFile('rq=' + RealToString(rq,-12,-8));
                  WriteLineToDebugFile('D=' + RealToString(D,-12,-8));
                  WriteLineToDebugFile('B=' + RealToString(B,-12,-8));
                  WriteLineToDebugFile('x=' + RealToString(x,-12,-2) + '  y=' + RealToString(y,-12,-2));
               end;

              procedure LCCDebug;
              begin
                  WriteLineToDebugFile('tMapProjection.ForwardProject ' + LatLongRadToString(Lat,Long));
                  WriteLineToDebugFile('t=' + RealToString(t,-18,-6));
                  WriteLineToDebugFile('theta=' + RealToString(theta/DegToRad,-12,-8));
                  WriteLineToDebugFile('rho=' + RealToString(rho,-18,-6));
                  WriteLineToDebugFile('x=' + RealToString(x,-12,-2) + '  y=' + RealToString(y,-12,-2));
              end;

              procedure CassiniDebug;
              begin
                  WriteLineToDebugFile('Cassini tMapProjection.ForwardProject ' + LatLongRadToString(Lat,Long));
                  WriteLineToDebugFile('CosPhi=' + RealToString(CosPhi,-18,-6));
                  WriteLineToDebugFile('LambdaDif=' + RealToString(LambdaDif/DegToRad,-18,-6));
                  WriteLineToDebugFile('SinLambdaDif=' + RealToString(SinLambdaDif,-18,-6));
                  WriteLineToDebugFile('Lat0=' + RealToString(Lat0/DegToRad,-18,-6));
                  WriteLineToDebugFile('B=' + RealToString(t,-18,-6));
                  WriteLineToDebugFile('x1=' + RealToString(x1,-12,-2));
                  WriteLineToDebugFile('y1=' + RealToString(y1,-18,-2));
                  WriteLineToDebugFile('x=' + RealToString(x,-12,-2) + '  y=' + RealToString(y,-12,-2));
              end;


              procedure OldStereoGraphicDebug;
              begin
                  WriteLineToDebugFile('Stereographic tMapProjection.ForwardProject ' + LatLongRadToString(Lat,Long));
                  WriteLineToDebugFile('CosPhi=' + RealToString(CosPhi,-18,-6));
                  WriteLineToDebugFile('SinPhi=' + RealToString(SinPhi,-18,-6));
                  WriteLineToDebugFile('LambdaDif=' + RealToString(LambdaDif/DegToRad,-18,-6));
                  WriteLineToDebugFile('SinLatCent=' + RealToString(SinLatCent,-18,-6));
                  WriteLineToDebugFile('SinLambdaDif=' + RealToString(SinLambdaDif,-18,-6));
                  WriteLineToDebugFile('CosLambdaDif=' + RealToString(CosLambdaDif,-18,-6));
                  WriteLineToDebugFile('Lat0=' + RealToString(Lat0/DegToRad,-18,-6));
                  WriteLineToDebugFile('K1=' + RealToString(K1,-18,-6));
                  WriteLineToDebugFile('K=' + RealToString(K,-18,-6));
                  WriteLineToDebugFile('x=' + RealToString(x,-12,-2) + '  y=' + RealToString(y,-12,-2));
              end;

              procedure AzimuthalEquidistantEllipsoidalDebug;
              begin
                  WriteLineToDebugFile('AzimuthalEquidistantEllipsoidalDebug tMapProjection.ForwardProject lat=' + RadToDegString(Lat,-18,9) + ' long=' + RadToDegString(Long,-18,9));
                  WriteLineToDebugFile('lat0=' + RadToDegString(Lat0,-18,9) + ' long0=' + RadToDegString(Long0,-18,9));
                  WriteLineToDebugFile('a=' + RealToString(a,-12,-2));
                  WriteLineToDebugFile('e2=' + RealToString(e2,-12,-9));
                  WriteLineToDebugFile('LambdaDif=' + RealToString(LambdaDif,-12,-9));
                  WriteLineToDebugFile('cos(Lat)=' + RealToString(cos(Theta),-12,-9));
                  WriteLineToDebugFile('M=' + RealToString(M,-12,-2));
                  WriteLineToDebugFile('tf=' + RealToString(tf,-12,-8));
                  WriteLineToDebugFile('x=' + RealToString(x,-12,-2) + '  y=' + RealToString(y,-12,-2));
              end;
            {$EndIf}



begin
   {$IfDef ForwardProject} WriteLineToDebugFile('tMapProjection.ForwardProjectRadians in'); {$EndIf}
   if Check then begin
      while (Long > Pi) do Long := Long - TwoPi;
      while (Long < -Pi) do Long := Long + TwoPi;
   end;
   {set defaults for case not visible on ViewProjection}
   x := MaxInt;
   y := MaxInt;

   LambdaDif := (Long - Long0);
   if PName in [WebMercator,Mercator,HammerProj,SinusProj,Mollweide,EquiDistantCylindrical,VanDerGrinten,MillerCylindrical,Cassini,GeneralTransverseMercator] then begin
      if (LambdaDif) > Pi then LambdaDif := LambdaDif - TwoPi;
      if (LambdaDif) < -Pi then LambdaDif := LambdaDif + TwoPi;
   end {if};
   if PName in [LamAzEqArea,OldStereographic,SphericalStereographic,OrthoProj,Gnomonic,Cassini] then begin
      CosLambdaDif := cos(LambdaDif);
      SinLambdaDif := sin(LambdaDif);
   end;

   CosPhi := cos(Lat);
   SinPhi := sin(Lat);

   case PName of
      PlateCaree : begin;
                      x := Long / DegToRad;
                      y := Lat / DegToRad;
                   end;
      UTMEllipsoidal,UK_OS,Finn_GK,GeneralTransverseMercator,IrishGrid : begin          //Snyder page 269
                              TransverseMercatorForwardProjectionRadians(Lat,Long,x,y);
                       end;
      MercatorEllipsoid : begin {Snyder, 1987, p.44, example p.267}
                            if abs(Lat) <= (HalfPi - 0.001) then begin
                               x := a * LambdaDif;
                               y := 0.5 * a * ln( (1 + SinPhi) / (1 - SinPhi) * Math.Power(((1 - e* SinPhi) / (1 + e* SinPhi)),e));
                            end {if};
                      end;
      PolarStereographicEllipsoidal : begin {Snyder, 1987, p.161, example p.315}
                                if Lat0 > 0 then begin
                                   esinlat := e * sin(Lat);
                                   t := Math.Tan(QuarterPi - 0.5 * Lat) / Math.Power( (1 - esinlat) / (1 + esinlat),0.5 * e);
                                   rho := a * mc *  t / tc;
                                   x := rho * sin(LambdaDif);
                                   y := -rho * cos(LambdaDif);
                                end
                                else begin
                                   esinlat := e * sin(-Lat);
                                   t := Math.Tan(QuarterPi + 0.5 * Lat) / Math.Power( (1 - esinlat) / (1 + esinlat),0.5 * e);
                                   rho := a * mc *  t / tc;
                                   LambdaDif := -Long + Long0;
                                   x := -rho * sin(LambdaDif);
                                   y := rho * cos(LambdaDif);
                                end;
                                {$IfDef RecordProjectionDetails} if PointConvertDebug then PolarStereographicDebug; {$EndIf}
                             end;
     AzimuthalEquidistantEllipsoidal : {Snyder, 1987, pp.198-199, p.339 using the Guam code} begin
                                      tf := sqrt(1 - e2 * sqr(SinPhi));
                                      x := a * LambdaDif * cos(Lat) / tf;
                                      M := M_3_21(Lat); //Snyder equation 3-21 for M
                                      M1 := M_3_21(Lat0);
                                      y := M - M1 + sqr(x) * tan(Lat) * tf / (2 * a);
                                      AddFalseEastingsNorthings(x,y);
                                      {$IfDef RecordProjectionDetails} if PointConvertDebug then AzimuthalEquidistantEllipsoidalDebug; {$EndIf}
                                    end;
      LambertConformalConicEllipse : begin   {Snyder, 1987, p.107, example p.296}
                                 try
                                    t := ft(Lat);
                                    if (t > 0.00001) then begin
                                       rho := a * Big_F * Math.Power(t,n_lcc);
                                       theta := n_lcc * (Long - Long0);
                                       x := ProjMapScale * rho * sin(Theta);
                                       y := ProjMapScale * Rho0 - rho * cos(Theta);
                                       AddFalseEastingsNorthings(x,y);
                                       {$IfDef RecordProjectionDetails} if PointConvertDebug then  LCCDebug; {$EndIf}
                                    end;
                                 except
                                    on Exception do ;
                                 end;
                              end;
      LamAzEqAreaEllipsoidal : begin
                                  q := fq(Lat);
                                  Beta := arcsin(q/q2);
                                  B := Rq * sqrt(2 / (1 + sin(beta1) * sin(beta) + cos(Beta1) * cos(Beta) * cos(LambdaDif)));
                                  x := B * D * cos(Beta) * sin(LambdaDif);
                                  y := B / D * ( cos(Beta1) * sin(Beta) - sin(Beta1) * cos(Beta) * cos(LambdaDif));
                                  AddFalseEastingsNorthings(x,y);
                                  {$IfDef RecordProjectionDetails} if PointConvertDebug then LamAzEqAreaDebug; {$EndIf}
                               end;
     CylindricalEqualAreaEllipsoidal : begin {Snyder, 1987, p.81, 281}
                                      q := (1 - e2) * (SinPhi / (1 - e2 *sqr(SinPhi)) - (1/(2 * e)) * ln((1 - e * SinPhi) / (1 + e * SinPhi)));  //Snyder, equation 3-12
                                      x := a * k0 * LambdaDif;
                                      y := a * q / ( 2 * k0);
                                  end;
      CylindricalEqualArea : begin
                                x := a * LambdaDif * cosLatCent;
                                y := a * sin(Lat) / CosLatCent;
                             end;
      EquidistantCylindrical : begin {Snyder, 1987, p.91}
                      x := a * LambdaDif * CosPhi1;
                      Y := a * (Lat{-Phi1});
                   end;
      MillerCylindrical  : begin {Snyder, 1987, p.88}
                      if abs(Lat) <= (HalfPi - 0.001) then begin
                         x := a * LambdaDif;
                         y := a * ln(Math.Tan(QuarterPi + 0.4 * Lat)) / 0.8;
                      end;
                   end;
      AlbersEqAreaConicalEllipsoid :  begin {Snyder, 1987, pp.101-102, 292-293}
                         q := fq(Lat);
                         rho := a * sqrt(C - Nalbers * q) / Nalbers;
                         Theta := Nalbers * LambdaDif;
                         x := rho * Sin(Theta);
                         y := Rho0 - rho * cos(theta);
                       end;
      WebMercator,
      Mercator  : begin {Snyder, 1987, p.43}
                      if abs(Lat) <= (HalfPi - 0.001) then begin
                         x := a * LambdaDif;
                         y := a * ln(Math.Tan(QuarterPi + 0.5 * Lat));
                      end;
                   end;
      SinusEllipsoidal : begin {Snyder, 1987, p.248, example p.366}
                            x := a * LambdaDif * cosPhi / sqrt(1 - e2 * sqr(sinPhi));
                            y := a * ( (1-e2/4 - 3*e2*e2/64 - 5*e2*e2*e2/256) * Lat - (3*e2/8 + 3*e2*e2/32 + 45*e2*e2*e2/1024) * sin(2*Lat) + (15*e2*e2/256+45*e2*e2*e2/1024) * sin(4*lat) - (35*e2*e2*e2/3072) * sin(6 * lat) );
                         end;
      EqualEarth    : begin
                         l := arcsin(eeM * sin(Lat));
                         l2 := l * l;
                         l6 := l2 * l2 * l2;
                         x := LambdaDif * cos(l) / (eeM * (eeA1 + 3 * eeA2 * l2 + l6 * (7 * eeA3 + 9 * eeA4 * l2))) * a;
                         y := l * (eeA1 + eeA2 * l2 + l6 * (eeA3 + eeA4 * l2)) * a;
                      end;
      VanDerGrinten : begin {Snyder, 1987, p.241}
                      Theta := Math.arcsin(abs(2*Lat/pi));
                      if abs(Lat) < 0.00001 then begin
                         x := a * LambdaDif;
                         y := 0;
                      end
                      else if (abs(LambdaDif) < 0.00001) or (abs(Lat/Petmar_types.DegToRad) > 89.999) then begin
                         x := 0;
                         y := Pi * a * Math.Tan(Theta * 0.5);
                         if Lat < 0 then y := - y;
                      end
                      else begin
                         A := 0.5 * abs(Pi/LambdaDif - LambdaDif/Pi);
                         G := cos(Theta) / (sin(Theta) + cos(Theta) - 1);
                         P := G * (2 / sin(Theta) - 1);
                         Q := sqr(A) + G;
                         x := sqr(A)*sqr(G-sqr(P));
                         x := x - (sqr(P)+sqr(A))*(Sqr(G)-sqr(P));
                         x := sqrt(x);
                         x := A * (G - sqr(P)) + x;
                         x := x / (sqr(P) + sqr(A));
                         x := Pi * a * x;
                         if (LambdaDif < 0) then x := -x;
                         y := (sqr(A) + 1) * (sqr(P) + sqr(A)) - sqr(Q);
                         y := P * Q - A * sqrt(y);
                         y := y / (sqr(P) + sqr(A));
                         y := pi * a * y;
                         if Lat < 0 then y := -y;
                      end;
                   end;
      Cassini    : begin    {Snyder, 1987, p.94, p.288}
                      if abs(LambdaDif) <= Pi then begin
                         B := CosPhi * SinLambdaDif;
                         x1 := arcSin(b);
                         y1 := (ArcTan(Math.Tan(Lat)/CosLambdaDif) - Lat0);
                         x := a * x1;
                         y := a * y1;
                         {$IfDef RecordProjectionDetails} if PointConvertDebug then CassiniDebug; {$EndIf}
                      end;
                   end;
      HammerProj : begin
                      HalfLambdaDif := 0.5 * LambdaDif;
                      K := a / sqrt(1 + CosPhi * cos(HalfLambdaDif));
                      x := (2 * K * CosPhi * Sin(HalfLambdaDif));
                      y := (K * SinPhi);
                   end;
      LamAzEqArea: begin
                      K := SinLatCent * SinPhi + CosLatCent * CosPhi * CosLambdaDif;
                      if K >= -0.001 then begin
                         K := sqrt(2 / (1 + K)) * a;
                         x := K * CosPhi * SinLambdaDif;
                         y := K * (CosLatCent * SinPhi - SinLatCent * CosPhi * CosLambdaDif);
                      end;
                   end;
      OldStereographic : begin
                    {Snyder, 1987, p.157}
                      K1 := (1.0 + SinLatCent * SinPhi + CosLatCent * CosPhi * CosLambdaDif);
                      if K1 >= 0.999999 then begin
                         K := 2.0 * a / K1;
                         x := K * CosPhi * SinLambdaDif;
                         y := K * (CosLatCent * SinPhi - SinLatCent * CosPhi * CosLambdaDif);
                         {$IfDef RecordProjectionDetails} if PointConvertDebug then OldStereoGraphicDebug; {$EndIf}
                      end {if};
                   end;
      SphericalStereographic : begin
                    {Snyder, 1987, p.157}
                      K := (1.0 + SinLatCent * SinPhi + CosLatCent * CosPhi * CosLambdaDif);
                      if K >= 0.999999 then begin
                         K := 2.0 * ProjMapScale * a / K;
                         x := ProjMapScale * K * CosPhi * SinLambdaDif;
                         y := K * (CosLatCent * SinPhi - SinLatCent * CosPhi * CosLambdaDif);
                      end {if};
                   end;
      OrthoProj  : begin   {Snyder, 1987, p.165}
                      if (SinLatCent * SinPhi + CosLatCent * cosLambdaDif * CosPhi) >= 0 then begin
                         x := a * CosPhi * SinLambdaDif;
                         y := a * (CosLatCent*SinPhi - SinLatCent*cosLambdaDif * CosPhi);
                      end;
                   end;
      SinusProj  : begin
                      x := CosPhi * LambdaDif * a;
                      y := Lat * a;
                   end;
      Mollweide  : begin   {Snyder, 1987, p.251}
                      ThetaPrime := Lat;
                      repeat
                         DeltaTheta := -1.0 * (ThetaPrime + sin(ThetaPrime) - Pi * sin(Lat)) / ( 1.0 + cos(ThetaPrime));
                         ThetaPrime := ThetaPrime + DeltaTheta;
                      until abs(DeltaTheta) < 0.01;
                      Theta := 0.5 * ThetaPrime;
                      x := sqrt(8) / Pi * a * LambdaDif * cos(Theta);
                      y := sqrt_2 * a * sin(Theta);
                   end;
      Gnomonic   : begin     {Snyder, 1987, p.165}
                      CosC := sinLatCent * SinPhi + CosLatCent * cosPhi * cosLambdaDif;
                      if CosC > 0.0001 then begin
                         CosC := a / CosC;
                         x := CosC * cosPhi * SinLambdaDif;
                         y := CosC * (CosLatCent * sinPhi - SinLatCent * cosPhi * cosLambdaDif);
                      end;
                   end;
   end {case};
   {$IfDef RawProject} WriteLineToDebugFile('tMapProjection.RawProjectRadians out'); {$EndIf}
end;



function tMapProjection.GetMapScaleFactor(Lat,Long : float64; var h,k : float64; var Prime : boolean) : boolean;
   {Lat, long in degrees; returns h,k scale factors}
var
   x,y,cosc : float64;
begin
   Prime := false;
   Result := true;
   h := 1;
   k := 1;
   case PName of
      AlbersEqAreaConicalEllipsoid :  begin {Snyder, 1987, pp.101-102, 292-293}
                     h := cosDeg(lat) / sqrt(C - 2 * nalbers * sinDeg(lat));
                     k := 1 / h;
                   end;
      LambertConformalConicEllipse : begin
                      k := m1 * Math.Power(ft(Lat*Petmar_types.DegToRad),n_lcc) / (fm(Lat*Petmar_types.DegToRad) * Math.Power(t1,n_lcc) );
                      h := k;
                   end;
      WebMercator,
      Mercator : begin {Snyder, 1987, p.44}
                     h := 1 / CosDeg(Lat);
                     k := h;
                   end;
      MercatorEllipsoid : begin {Snyder, 1987, p.44, example p.267}
                     h := sqrt(1 - e2 * sqr(SinDeg(Lat))) / CosDeg(Lat);
                     k := h;
                   end;
      UTMEllipsoidal,GeneralTransverseMercator : begin
                   ForwardProjectRadians(Lat*Petmar_types.DegToRad,Long*Petmar_types.DegToRad,x,y);
                   k := ProjMapScale * (1 + (1 + e2 /(1-e2)* sqr(cosDeg(Lat))) *  sqr(x-false_east) / (2 * sqr(ProjMapScale) * sqr(N)));
                   h := k;
                 end;
     OldStereographic : begin      {Snyder, 1987, p.157}
                     if (abs(Lat0 - 90*Petmar_types.DegToRad) < 0.0001) then K := 2 / (1 + sinDeg(Lat))
                     else if (abs(Lat0 + 90*Petmar_types.DegToRad) < 0.0001) then  K := 2 / (1 - sinDeg(Lat))
                     else k := 2 / (1 + Sin(Lat0) * SinDeg(Lat) + Cos(Long0) * CosDeg(Long) * Cos(Long * Petmar_types.DegToRad - Long0));
                     h := k;
                  end;
     SphericalStereographic : begin      {Snyder, 1987, p.157}
                     if (abs(Lat0 - 90*Petmar_types.DegToRad) < 0.0001) then K := 2 * ProjMapScale / (1 + sinDeg(Lat))
                     else if (abs(Lat0 + 90*Petmar_types.DegToRad) < 0.0001) then  K := 2 * ProjMapScale / (1 - sinDeg(Lat))
                     else k := 2 * ProjMapScale / (1 + Sin(Lat0) * SinDeg(Lat) + Cos(Long0) * CosDeg(Long) * Cos(Long * Petmar_types.DegToRad - Long0));
                     h := k;
                  end;
     OrthoProj   : begin       {Snyder, 1987, p.149}
                     Prime := true;
                     k := 1.0;
                     h := Sin(Lat0) * SinDeg(Lat) + Cos(Long0) * CosDeg(Long) * Cos(Long * Petmar_types.DegToRad - Long0);
                  end;
     Gnomonic   : begin       {Snyder, 1987, p.165}
                     Prime := true;
                     Cosc := Sin(Lat0) * SinDeg(Lat) + Cos(Long0) * CosDeg(Long) * Cos(Long * Petmar_types.DegToRad - Long0);
                     k := 1 / CosC;
                     h := 1 / sqr(cosc);
                  end;
     LamAzEqArea  : begin       {Snyder, 1987, p.185}
                     Prime := true;
                     k := sqrt(2 / (1 + Sin(Lat0) * SinDeg(Lat) + Cos(Long0) * CosDeg(Long) * Cos(Long * Petmar_types.DegToRad - Long0)));
                     h := 1 / k;
                  end;
     SinusProj     : begin  {Snyder, 1987, p.247}
                       k := 1;
                       h := sqrt(1 + Cos(Long * Petmar_types.DegToRad - Long0) * sqr(sinDeg(Lat)));
                   end;
     CylindricalEqualArea : begin  {Snyder, 1987, p.77}
                       h := CosDeg(lat) / Cos(Lat0);
                       k := 1/h;
                   end;
     MillerCylindrical  : begin  {Snyder, 1987, p.88}
                       h := 1 / cosDeg(0.8*Lat);
                       k := 1 / cosDeg(Lat);
                    end;
     EquiDistantCylindrical  : begin  {Snyder, 1987, p.88}
                       h := 1;
                       k := CosPhi1 / cosDeg(Lat);
                   end;
     Cassini   : begin  {Snyder, 1987, p.94-95, p.288}
                       h := 1 / (1 - sqr(CosDeg(Lat) * sin(Long * Petmar_types.DegToRad - Long0)));
                       k := 1;
                       Prime := true;
                   end;
     PolarStereographicEllipsoidal: begin    //{Snyder, 1987, worked example p.314-315}
                       //{eq. 15-9, p.108} t := TanDeg(45-0.5 * Lat) / ( (1 - e * SinDeg(Lat)) / (1 + e * SinDeg(Lat)) );
                       {eq/21-33, p.161}
                       {eq 14-15}
                       {eq 21-32}
                       //k := rho / a / m;
                       //h := k;
                   end;
     VanDerGrinten : Result := false;  {Snyder, 1987, p.242, too involved to be given}
     Mollweide    : Result := false;  {Snyder, 1987, p.251, not given}
     HammerProj :  Result := false; {Snyder, 1987, p.182, not given}
     else Result := false;
   end {case};
end;



procedure tMapProjection.InverseProjectDegrees(x,y : float64; var Lat,Long : float64);
begin
   InverseProjectRadians(x,y,Lat,Long);
   Lat := Lat / DegToRad;
   Long := Long / DegToRad;
end;

procedure tMapProjection.ForwardProjectDegrees(Lat,Long : float64; var x,y : float64; Check : boolean = true);
begin
   ForwardProjectRadians(Lat*DegToRad,Long*DegToRad,x,y,Check);
end;


procedure tMapProjection.SubtractFalseEastingsNorthings(var x,y : float64);
begin
    X := X - false_east;
    Y := Y - false_north;
end;

procedure tMapProjection.AddFalseEastingsNorthings(var x,y : float64);
begin
    X := X + false_east;
    Y := Y + false_north;
end;


function tMapProjection.KeyDatumParams : shortString;
begin
    //Result := h_datumCode + '  h_f: ' + RealToString(h_f,-18,-6) + '  a: ' + RealToString(a,-18,-6) + '  h_xdat: ' + RealToString(h_xdat,-18,-6) + '  h_ydat: ' + RealToString(h_ydat,-18,-6) + '  h_zdat: ' + RealToString(h_zdat,-18,-6);
    Result := h_datumCode + ' UTM zone=' + IntToStr(ProjUTMZone) + Lathemi;
end;


procedure tMapProjection.DefineWGS84;
begin
   h_DatumCode := 'WGS84';
   h_EllipsCode := 'WE';
   h_XDat := 0;
   h_YDat := 0;
   h_ZDat := 0;
   a := 6378137;
   h_f := 298.2572236;
end;


function tMapProjection.fm(Phi : float64) : float64;
begin
   fm := Cos(Phi) / sqrt(1 - e2*sqr(sin(Phi)));
end;


function tMapProjection.fq(Phi : float64) : float64;
begin
   if abs(e) < 0.0001 then fq := 2 * sin(Phi)
   else fq := (1 - e2) * (Sin(Phi) / (1 - e2*sqr(sin(Phi))) - 1 / (2 * e) * ln( (1 - e * sin(Phi)) / (1 + e * sin(Phi))));
end;


function tMapProjection.ft(Lat : float64) : float64;
begin
   if Lat < -HalfPi then Lat := -HalfPi + 0.01;
   ft := Math.Tan(0.25*Pi - 0.5 * Lat) / Math.Power(( 1 - e * Sin(Lat)) / (1 + e * Sin(Lat)),0.5*e);
end;



{$IfDef VCL}

      procedure PickDatum(WhatFor : shortstring; var DatumCode : ShortString);
      var
         i,j : integer;
         Table : tMyData;
         DatumList : tStringList;
      begin
         {$IfDef RecordPickDatum} WriteLineToDebugFile('PickDatum in: "' + DatumCode + '"'); {$EndIf}
         DatumList := tStringList.Create;

         Table := tMyData.Create(GT_Datum_fName);
         i := 0;
         while not Table.Eof do begin
            inc(i);
            if UpperCase(Table.GetFieldByNameAsString('DATUM_CODE')) = UpperCase(DatumCode) then j := i;
            DatumList.Add(Table.GetFieldByNameAsString('DATUM_CODE') + '   ' + Table.GetFieldByNameAsString('DATUM_NAME'));
            Table.Next;
         end;
         Table.Destroy;
         GetFromList(WhatFor,j,DatumList);
         DatumCode := ptTrim(Copy(DatumList.Strings[j],1,5));
         DatumList.Free;
         {$IfDef RecordPickDatum} WriteLineToDebugFileWith('PickDatum out: "' + DatumCode + '"'); {$EndIf}
      end;


      procedure GetSecondaryDatum;
      begin
         PickDatum('Verify secondary datum',MDDef.PreferSecondaryDatum);
      end;


      procedure GetMapParametersSPCSOption(var ProjFileName : PathStr; var Hemi : ANSIchar; var UTMZone : int16; var DatCode : shortstring; var  PName :  tProjectType; var GeoLatLong : boolean; UTMZoneOnly : boolean = false; AreaName : shortString = '');
      var
         PickDatumParams : TPickDatumParams;
      begin
         PickDatumParams := TPickDatumParams.Create(Application);
         with PickDatumParams do begin
            if (AreaName <> '') then Caption := Caption + '  ' + AreaName;
            if (Hemi ='N') then RadioGroup1.ItemIndex := 0
            else RadioGroup1.ItemIndex := 1;
            if UTMZoneOnly then begin
               RadioGroup2.Visible := false;
               Button1.Visible := false;
               Button2.Visible := false;
               BitBtn3.Visible := false;
            end
            else begin
               Button1.Caption := DatumName(DatCode);
               FormDatumCode := DatCode;
            end;
            Edit1.Text := IntToStr(UTMZone);
            if (Not UseDefaultDatumZone) then ShowModal;

            if not UTMZoneOnly then begin
               DatCode := ptTrim(FormDatumCode);
            end;
            if (RadioGroup1.ItemIndex = 0) then Hemi := 'N' else Hemi := 'S';
            CheckEditString(Edit1.Text,UTMZone);
         end;
         GeoLatLong := PickDatumParams.GeoLatLong;
         PName := PickDatumParams.PName;
         ProjFileName := PickDatumParams.ProjFName;
         PickDatumParams.Free;
      end;


      procedure GetMapParameters(var Hemi : ANSIchar; var UTMZone : int16; var DatCode : ShortString; UTMZoneOnly : boolean = false);
      var
         PName     : tProjectType;
         ProjFileName : PathStr;
         GeoLatLong : boolean;
      begin
         GetMapParametersSPCSOption(ProjFileName,Hemi,UTMZone,DatCode,PName,GeoLatLong,UTMZoneOnly);
      end;

{$EndIf}



function DatumName(var DatumCode : ShortString) : ShortString;
var
   Table : tMyData;
begin
  Table := tMyData.Create(GT_Datum_fName);
  Table.ApplyFilter( 'DATUM_CODE=' + QuotedStr(DatumCode));
  Result := Table.GetFieldByNameAsString('DATUM_NAME');
  Table.Destroy;
end;


function EllipsoidName(EllipCode : ShortString) : ShortString;
var
   Table : tMyData;
begin
  Table := tMyData.Create(GT_Ellipse_fName);
  Table.ApplyFilter( 'ELLIPSE_C=' + QuotedStr(EllipCode));
  Result := Table.GetFieldByNameAsString('ELLIPSE_N');
  Table.Destroy;
end;


{ tMapProjection }


procedure tMapProjection.SetDatumConstants;
begin
   {$IfDef RecordProjectionParameters} WriteLineToDebugFile(ProjDebugName + ' tMapProjection.SetDatumConstants, datum=' + h_DatumCode); {$EndIf}
   if (h_datumCode = '') then h_datumcode := 'WGS84';

   if IsNAN(a) and IsNAN(h_f) then begin
      if h_datumCode = 'NAD27' then begin  {Clarke 1866 ellipsoid}
                           a := 6378206.4;
                           h_f := 1 / 294.978698214;
                           //e2 := 0.006768658;
     end
     else if WGSEquivalentDatum(h_datumCode) then begin
                           a := 6378137.0;
                           h_f := 1 / 298.257222101;
                           e2 := 0.00669438;
     end
     else if (h_datumCode = 'Airy') or (h_datumCode = 'OGB-M')  then begin
                           a := 6377563.396;
                           h_f := 1/299.3249646;
                           //e2 := 2*f-sqr(f);
     end
     else if (h_datumCode = 'Hayford') then begin
                           a := 6378388;
                           h_f := 1/297;
                           //e2 := 2*f-sqr(f);
     end
     else if (h_datumCode = 'GRS1980') then begin
                           a := 6378388;
                           h_f := 1/297;
                           //e2 := 2*f-sqr(f);
     end
     else begin
        MessageToContinue('Datum code not recognized: ' + h_DatumCode);
     end;
  end
  else begin
     if (h_f > 1) then h_f := 1 / h_f;
  end;

  e2 := 2*h_f-sqr(h_f);
  e := sqrt(e2);
  // h_f := 1 / f;
   {$If Defined(RecordProjectionParameters) or Defined(track_f)} ShortProjInfo('tMapProjection.SetDatumConstants Out'); {$EndIf}
end;


function tMapProjection.M_3_21(LatRadians : float64) : float64;  //equation 3-21 of Synder, p.17
//this could be used for M0 in the UTM, now only used for Equi7
begin
    Result := A * (1.0 - E2 / 4 - 3.0*E2*E2/64.0 - 5.0*E2*E2*E2/256.0) * LatRadians -
              A * (3.0 * E2 /8.0 + 3.0*E2*E2/32.0 + 45*E2*E2*E2/1024.0) * sin(2*LatRadians) +
              A * (15*E2*E2/256.0 + 45*E2*E2*E2/1024.0) * sin(4*LatRadians) -
              A * (35.0*E2*E2*E2/3072.0) * sin(4*LatRadians);
end;



procedure tMapProjection.SetDatumAndTMConstants;
begin
    {$IfDef RecordProjectionParameters} ShortProjInfo('tMapProjection.SetDatumAndTMConstants in'); {$EndIf}
    SetDatumConstants;
    UTM_S1 := A*(1.0 - E2 / 4 - 3.0*E2*E2/64.0 - 5.0*E2*E2*E2/256.0);
    S2 := A * (3.0/8.0*E2 + 3.0/32.0*E2*E2 + 45/1024.0*E2*E2*E2);
    S4 := A * ( 15/256.0*E2*E2 + 45/1024.0*E2*E2*E2);
    S6 := A * (35.0/3072.0*E2*E2*E2);
    M0 := UTM_S1*Lat0 - S2*Sin(2.0*Lat0) + S4*Sin(4.0*Lat0) - S6*Sin(6.0*Lat0);
    EP2 := E2 / (1 - E2);
    E1 := (1.0-Sqrt(1.0-E2)) / (1.0+Sqrt(1.0-E2));
    MUPRM := A*(1.0-E2/4.0-3.0*Sqr(E2)/64.0-5.0*E2*Sqr(E2)/256);        //Snyder equation 7-19
    TM_Phi2 := 3.0*E1/2.0 - 27.0*Math.IntPower(E1,3) / 32;
    Phi4 := 21.0*Sqr(E1)/16.0 - 55.0*Sqr(Sqr(E1)) / 32;
    Phi6 := 151.0 * Math.IntPower(E1,3) / 96;
    {$If Defined(RecordProjectionParameters) or Defined(track_f) or Defined(RecordTMParameters)} ShortProjInfo('tMapProjection.SetDatumAndTMConstants Out'); {$EndIf}


(*
    {$IfDef RecordProjectionParameters} ShortProjInfo('tMapProjection.SetDatumAndTMConstants restart old'); {$EndIf}
    SetDatumConstants;
    UTM_S1 := A*(1.0 - E2 / 4 - 3.0*E2*E2/64.0 - 5.0*E2*E2*E2/256.0);
    S2 := A * (3.0/8.0*E2 + 3.0/32.0*E2*E2 + 45/1024.0*E2*E2*E2);
    S4 := A * ( 15/256.0*E2*E2 + 45/1024.0*E2*E2*E2);
    S6 := A * (35.0/3072.0*E2*E2*E2);
    M0 := UTM_S1*Lat0 - S2*Sin(2.0*Lat0) + S4*Sin(4.0*Lat0) - S6*Sin(6.0*Lat0);
    EP2 := E2 / (1 - E2);
    E1 := (1.0-Sqrt(1.0-E2)) / (1.0+Sqrt(1.0-E2));
    MUPRM := A*(1.0-E2/4.0-3.0*Sqr(E2)/64.0-5.0*E2*Sqr(E2)/256);        //Snyder equation 7-19
    TM_Phi2 := 3.0*E1/2.0 - 27.0*Math.IntPower(E1,3) / 32;
    Phi4 := 21.0*Sqr(E1)/16.0 - 55.0*Sqr(Sqr(E1)) / 32;
    Phi6 := 151.0 * Math.IntPower(E1,3) / 96;
    {$If Defined(RecordProjectionParameters) or Defined(track_f) or Defined(RecordTMParameters)} ShortProjInfo('tMapProjection.SetDatumAndTMConstants Out after restart'); {$EndIf}
*)
end;



procedure tMapProjection.TransverseMercatorInverseProjectionRadians(X,Y : float64; var Lat,Long : float64);
//converted from Fortran-77 subroutine originally written by Dr. John Sharry of Sun Oil, using Snyder,  p.63-64, example p.270-271
var
   mu,dd,NewC1,Phi2forY,tanPhi2,cosPhi2,sinPhi2,C1,T1,N1,R1 : float64;
begin
  {$IfDef RawProjectInverse} if DebugRawProjectInverse then WriteLineToDebugFile('InverseProject TM restart start, x=' + RealToString(x,-12,-1)+ '  y=' + RealToString(y,-12,-1)); {$EndIf}
   SubtractFalseEastingsNorthings(x,y);
   MU := (M0 + Y / ProjMapScale) / MUPRM;
   {$IfDef RawProjectInverse} if DebugRawProjectInverse then WriteLineToDebugFile('long0=' + RadToDegString(Long0) + '  falsex=' + RealToString(x,-12,-1) + '  falsey=' + RealToString(y,-12,-1)); {$EndIf}
   Phi2forY := MU + TM_Phi2 * Sin(2.0*MU) + Phi4 * Sin(4.0*MU) + Phi6 * Sin(6.0*MU);
   cosPhi2 := Cos(Phi2forY);
   sinPhi2 := sin(Phi2forY);
   tanPhi2 := Math.Tan(Phi2forY);

   C1 := EP2*Sqr(cosPhi2);
   T1 := Sqr(TanPhi2);
   N1 := a/Sqrt(1.0-E2*Sqr(SinPhi2));
   R1 := a*(1.0-E2) / Sqrt((1.0 - E2 * Sqr(SinPhi2)) * Sqr(1.0-E2*Sqr(SinPhi2)));
   {$IfDef RawProjectInverse} if DebugRawProjectInverse then WriteLineToDebugFile('c1=' + RealToString(c1,-12,-6) + ' t1=' + RealToString(t1,-12,-6) + ' n1=' + RealToString(n1,-12,-6)  + ' r1=' + RealToString(r1,-12,-6)); {$EndIf}

   DD := X / (N1 * ProjMapScale);

   Lat := Phi2forY - (N1 * TanPhi2 / R1) * (Sqr(DD) / 2.0 - (5.0 + 3.0 * T1 + 10.0 * C1 - 4.0 * IntPower(C1,2) - 9.0 * EP2) *  Math.IntPower(DD,4) / 24.0 +
         (61.0 + 90.0 * T1 + 298.0 * C1 + 45.0 * Sqr(T1) - 25.0 *EP2 - 3.0 * Sqr(C1)) * IntPower(DD,6) / 720);

   NewC1 := (1.0 + (2.0 * T1) + C1) * Math.IntPower(DD,3) / 6.0;

   Long := 5.0 - (2.0 * C1) + (28.0 * T1) - (3.0 * Sqr(C1)) + (8.0 * EP2) + (24.0 * Sqr(T1));
   Long := Long * Math.IntPower(DD,5) / 120;
   Long := (DD - NewC1 + Long) / cosPhi2;
   Long := (Long0 + Long);
   {$IfDef RawProjectInverse} if DebugRawProjectInverse then WriteLineToDebugFile('Answer out:  ' + LatLongDegreeToString(Lat/DegToRad,Long/DegToRad)); {$EndIf}
end;


procedure tMapProjection.TransverseMercatorForwardProjectionRadians(Lat,Long : float64; var X,Y : float64);
//converted from Fortran-77 subroutine written by Dr. John Sharry of Sun Oil, using Snyder  p.60-61, 63, example page 269-270
var
   T,CC,BigA,M : float64;
begin
     {$IfDef ForwardProject} WriteLineToDebugFile('ForwardProject TM start'); {$EndIf}
     N := a / Sqrt(1 - e2*Sqr(sin(Lat)));
     {$IfDef ForwardProject} WriteLineToDebugFile('e2=' + RealToString(e2,-18,-6) + '  a=' + RealToString(A,-12,-2)); {$EndIf}
     T := Sqr(Math.Tan(Lat));
     CC := EP2* Sqr(cos(Lat));
     BigA := (Long - Long0) * Cos(Lat);
     {$IfDef ForwardProject} ShortProjInfo('ForwardProject TM mid projmapscale'); {$EndIf}
     {$IfDef ForwardProject} WriteLineToDebugFile('N=' + RealToString(N,-18,-6) + '  BigA=' + RealToString(BigA,-18,-6)); {$EndIf}
     {$IfDef ForwardProject} WriteLineToDebugFile('T=' + RealToString(T,-18,-6) + '  C=' + RealToString(C,-18,-6)); {$EndIf}
     {$IfDef ForwardProject} WriteLineToDebugFile('EP2=' + RealToString(EP2,-18,-6)); {$EndIf}
     X := ProjMapScale * N * (BigA + (1.0 - T + CC) / 6.0 * BigA * BigA * BigA + (5.0 - 18.0*T + Sqr(T) + 72.0*CC - 58.0*EP2) / 120.0 * Math.IntPower(BigA,5));
     M := (UTM_S1 * Lat) - S2 * Sin(2.0 * Lat) + S4 * SIN(4.0 * Lat) - S6 * sin(6.0 * Lat);
     {$IfDef ForwardProject} WriteLineToDebugFile('M=' + RealToString(m,-18,-2)); {$EndIf}
     Y := ProjMapScale * (M - M0 + N*Math.Tan(Lat)* (Sqr(BigA)/2 + (5.0 - T + 9.0*CC + 4.0*Sqr(CC)) * Math.IntPower(BigA,4)/24.0 + (61.0- 58.0*T + Sqr(T) + 600.0*CC - 330.0*EP2) * Math.IntPower(BigA,6)/720));
     {$IfDef ForwardProject} WriteLineToDebugFile('X=' + RealToString(x,-18,-2) + '   Y=' + RealToString(Y,-18,-2)); {$EndIf}
     AddFalseEastingsNorthings(x,y);
    {$If Defined(ForwardProject) or Defined(RecordGeodeticCalculations)} WriteLineToDebugFile('RawProject TM end X=' + RealToString(x,-18,-2) + '   Y=' + RealToString(Y,-18,-2)); {$EndIf}
end {proc};


procedure tMapProjection.LatLongDegreetoUTM(Lat,Long : float64; var XUTM,YUTM : float64);
begin
   {$IfDef RecordGeodeticCalculations} WriteLineToDebugFile('SpecifiedDatumLatLongDegreetoUTM in ' + LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod)); {$EndIf}
   TransverseMercatorForwardProjectionRadians(Lat * DegToRad,Long * DegToRad,XUTM,YUTM);
end;


procedure tMapProjection.UTMtoLatLongDegree(XUTM,YUTM : float64; var Lat,Long : float64);
begin
   {$IfDef RecordGeodeticCalculations} WriteLineToDebugFile('SpecifiedDatumUTMtoLatLongDegree in ' + UTMString(xutm,yutm)); {$EndIf}
   TransverseMercatorInverseProjectionRadians(XUTM,YUTM,Lat,Long);
   Lat := Lat / DegToRad;
   Long := Long / DegToRad;
   {$IfDef RecordGeodeticCalculations} WriteLineToDebugFile(LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod)); {$EndIf}
end;




//***********************************************Datum manipulation code ****************************************



procedure tMapProjection.ProtectedDefineDatum(DatumCode : ShortString; Why : shortstring);


      procedure SafeDefineDatum;
      var
         i     : integer;
         TStr  : ShortString;
         Table : tMyData;
      begin
         {$If Defined(RecordDefineDatum) or Defined(RecordLong0) or Defined(LongCent)} WriteLineToDebugFile(ProjDebugName + ' SafeDefineDatum in, ' + KeyDatumParams); {$EndIf}
         {$IfDef FMX}
            DefineWGS84;
         {$Else}
            if not WGSEquivalentDatum(DatumCode) then begin
              if FileExists(GT_Datum_fName) and FileExists(GT_Ellipse_fName) then begin
                  h_DatumCode := ptTrim(h_DatumCode);
                  for i := length(h_DatumCode) downto 1 do if not (h_DatumCode[i] in ['A'..'Z','0'..'9','-','-']) then Delete(H_DatumCode,i,1);
                  Table := tMyData.Create(GT_Datum_fName);
                  Table.ApplyFilter('DATUM_CODE=' + QuotedStr(h_DatumCode));
                  if (Table.RecordCount > 0) then begin
                     Table.ApplyFilter( 'DATUM_CODE=' + QuotedStr(h_DatumCode));
                     h_XDat := Table.GetFieldByNameAsInteger('DX');
                     h_YDat := Table.GetFieldByNameAsInteger('DY');
                     h_ZDat := Table.GetFieldByNameAsInteger('DZ');
                     h_EllipsCode := Table.GetFieldByNameAsString('ELLIPSE_C');

                     Table := tMyData.Create(GT_Ellipse_fName);
                     Table.ApplyFilter('ELLIPSE_C=' + QuotedStr(h_EllipsCode));
                     a := Table.GetFieldByNameAsFloat('ELLIPSE_A');
                     h_f := Table.GetFieldByNameAsFloat('ELLIPSE_F');
                     Table.Destroy;
                  end;
                 {$If Defined(RecordDefineDatum)} WriteLineToDebugFile('SafeDefineDatum after table lookup, ' + KeyDatumParams); {$EndIf}
              end
              else begin
                 TStr := 'Projection files missing: ' + GT_Datum_fName + ' and ' + GT_Ellipse_fName;
                 MessageToContinue(TStr);
                 {$IfDef RecordProblems} WriteLineToDebugFile(TStr); {$EndIf}
              end;
            end;
        {$EndIf}

        if PName in [UTMEllipsoidal] then begin
           //these are for UTM and defined here, as they differ from some other TM projections
            ProjMapScale := 0.9996;
            Lat0  := 0.0;
            Long0 := DegToRad * UTMZoneCentralLong(projUTMZone);
            if (LatHemi = 'S') then false_north := 10000000
            else false_north := 0;
            False_east := 500000;

            ep2 := (1 / h_f) * (2 - (1 / h_f));

            {$If Defined(RecordDefineDatum)} WriteLineToDebugFile('SafeDefineDatum mid, ' + KeyDatumParams); {$EndIf}
            SetDatumAndTMConstants;
         end;

         {$IfDef RecordDefineDatum} LogDatumInfo(ProjDebugName +  '  ' + Why + ' in SafeDefineDatum'); {$EndIf}
         {$If Defined(RecordDefineDatum) or Defined(RecordLong0) or Defined(LongCent)} WriteLineToDebugFile(ProjDebugName + ' SafeDefineDatum out, ' + KeyDatumParams); {$EndIf}
      end;


begin
   {$If Defined(RecordShortDefineDatum) or Defined(RecordDefineDatum)} WriteLineToDebugFile(ProjDebugName + '  ProtectedDefineDatum  in ' +  Why + ' ' + KeyDatumParams); {$EndIf}
   if (DatumCode = 'NAS-C') then DatumCode := 'NAD27';
   if (DatumCode = 'NAR-C') then DatumCode := 'NAD83';
   h_DatumCode := DatumCode;
   SafeDefineDatum;
   {$If Defined(RecordShortDefineDatum) or Defined(RecordDefineDatum)} WriteLineToDebugFile(ProjDebugName + '  ProtectedDefineDatum  out,' + KeyDatumParams); {$EndIf}
   {$If Defined(track_f)} ShortProjInfo('ProtectedDefineDatum Out'); {$EndIf}
end;



function WGSEquivalentDatum(StartDatum : shortstring) : boolean;
begin
   Result := (StartDatum = 'WGS84') or (StartDatum = 'ETR89') or (StartDatum = 'NAD83') or (StartDatum = 'NAR-C') or (StartDatum = 'GRS1980') or (StartDatum = 'GDA94');;
end;


procedure RedefineWGS84DatumConstants(NewLong : float64; LatHemi : ANSIchar = 'N');
var
   NewZone : integer;
begin
   NewZone := GetUTMZone(NewLong);
   if (NewZone <> WGS84DatumConstants.projUTMZone) then begin
      WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',NewZone,LatHemi,'RedefineWGS84DatumConstants');
   end;
   WGS84DatumConstants.StartUTMProjection(NewZone);
   {$If Defined(RecordWGS84Projection)} WriteLineToDebugFile('RedefineWGS84DatumConstants, lat=' + RealToString(NewLong,-8,2) + '  utmzone=' + IntToStr(NewZone)); {$EndIf}
end;


procedure RedefineWGS84DatumConstants(NewLat,NewLong : float64); overload;
var
   NewZone : integer;
   LatHemi : ANSIChar;
begin
   NewZone := GetUTMZone(NewLong);
   if NewLat > 0 then LatHemi := 'N' else LatHemi := 'S';
   
   if (NewZone <> WGS84DatumConstants.projUTMZone) then begin
      WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',NewZone,LatHemi,'RedefineWGS84DatumConstants');
   end;
   WGS84DatumConstants.StartUTMProjection(NewZone);
   {$If Defined(RecordWGS84Projection)} WriteLineToDebugFile('RedefineWGS84DatumConstants, lat=' + RealToString(NewLong,-8,2) + '  utmzone=' + IntToStr(NewZone)); {$EndIf}
end;


procedure MolodenskiyTransformation(InLat,InLong : float64; var OutLat,OutLong : float64; FromMap,ToMap : tMapProjection);
{Refs: chap. 2 DMA TM 8358.1;    chap. 7 DMA TR 8350.2 2d Ed.;    MADTRAN program}
const
   SinOneSecond = 0.4848136811e-5;
var
   BDat,da,SinLat,CosLat,SinLong,CosLong,
   ssqlat,tlat,tlon,rm,rn,esq,df,dx,dy,dz   : float64;
begin
   if (FromMap.h_datumCode = ToMap.h_datumCode) then begin
   {$If Defined( RecordGeotdeticCalc) or Defined(RecordDatumProblems) or Defined(RecordMolodenskiy)} WriteLineToDebugFile('MolodenskiyTransformation same datum: ' + FromMap.h_datumCode); {$EndIf}
      OutLat := InLat;
      OutLong := InLong;
      exit;
   end;
   {$If Defined( RecordGeotdeticCalc) or Defined(RecordDatumProblems) or Defined(RecordMolodenskiy)} WriteLineToDebugFile('MolodenskiyTransformation: ' + FromMap.h_datumCode + '  to  ' + ToMap.h_datumCode); {$EndIf}
   {$If Defined(RecordMolodenskiy)}
      WriteLineToDebugFile(FromMap.h_datumCode + '            h_f: ' + RealToString(FromMap.h_f,-18,-6) + '  to  ' + RealToString(ToMap.h_f,-18,-6) );
      WriteLineToDebugFile('            a: ' + RealToString(FromMap.a,-18,-6) + '  to  ' + RealToString(ToMap.a,-18,-6) );
      WriteLineToDebugFile('            h_xdat: ' + RealToString(FromMap.h_xdat,-18,-6) + '  to  ' + RealToString(ToMap.H_xdat,-18,-6) );
      WriteLineToDebugFile('            h_ydat: ' + RealToString(FromMap.h_ydat,-18,-6) + '  to  ' + RealToString(ToMap.H_ydat,-18,-6) );
      WriteLineToDebugFile('            h_zdat: ' + RealToString(FromMap.h_zdat,-18,-6) + '  to  ' + RealToString(ToMap.H_zdat,-18,-6) );
  {$EndIf}

   SinLat := sinDeg(InLat);
   CosLat := cosDeg(InLat);
   SinLong := sinDeg(InLong);
   CosLong := cosDeg(InLong);
   ssqlat := SinLat * SinLat;

   esq := 2 * (1 / FromMap.h_f) - sqr(1 / FromMap.h_f);
   da := ToMap.a - FromMap.a;
   df := (1 / ToMap.h_f) - (1 / FromMap.h_f);
   dx := ToMap.h_xdat - FromMap.h_xdat;
   dy := ToMap.h_ydat - FromMap.h_ydat;
   dz := ToMap.h_zdat - FromMap.h_zdat;
   BDat := (FromMap.a * 1 / FromMap.h_f) - FromMap.a;

   rn := FromMap.a / sqrt(1.0 - esq * ssqlat);
   rm := FromMap.a * (1.0 - esq) / Math.Power((1.0 - esq * ssqlat),1.5);

  {formula from chap. 7 DMA TR 8350.2 2d Ed.}
   tlat := -(dx * SinLat * CosLong) - (dy * SinLat * SinLong) + (dz * CosLat) - da * (rn * esq * SinLat * CosLat / FromMap.a) + df * (rm * FromMap.a / Bdat + rn * Bdat / FromMap.a) * SinLat * CosLat;
   OutLat := InLat - tlat / (rm * SinOneSecond) /3600;

   tlon := (-dx * SinLong + dy * CosLong) / (rn * CosLat * SinOneSecond);
   OutLong := InLong - tLon /3600;
  {$If Defined(RecordGeotdeticCalc) or Defined(RecordDatumProblems) or Defined(RecordMolodenskiy)} WriteLineToDebugFile('In: ' + LatLongDegreeToString(Inlat,InLong) + ' Out: ' + LatLongDegreeToString(OutLat,OutLong)); {$EndIf}
end;


//*********************************************** UTM and MGRS manipulation  ****************************************

type
   ZoneChars = array[1..8] of ANSIchar;
const
   LetUsed : array[0..34] of ANSIchar = ('A','B','C','D','E','F','G','H','J','K','L','M','N','P','Q','R','S','T','U','V',  'A','B','C','D','E','F','G','H','J','K','L','M','N','P','Q');
   Zone0 : ZoneChars = ('S','T','U','V','W','X','Y','Z');
   Zone1 : ZoneChars = ('A','B','C','D','E','F','G','H');
   Zone2 : ZoneChars = ('J','K','L','M','N','P','Q','R');


function tMapProjection.UTMStringFromLatLongDegree(Lat,Long : float64; IncludeDatum : boolean = true) : shortString;
var
   XUTM,YUTM : float64;
begin
   ForwardProjectDegrees(Lat,Long,XUTM,YUTM);
   if IncludeDatum then Result := h_DatumCode + '   '
   else Result := '';
   Result := Result + 'UTM zone ' + IntToStr(projUTMZone) + '  x=' + RealToString(XUTM,-18,0) + '  y=' + RealToString(YUTM,-18,0);
end;


procedure tMapProjection.DefineDatumFromUTMZone(DatumCode : ShortString; UTMZone : byte; inLatHemi : ANSIchar;  Why : shortstring = '');
begin
   {$If Defined(RecordShortDefineDatum) or Defined(RecordDefineDatum)} WriteLineToDebugFile('DefineDatumFromUTMZone=' + IntToStr(UTMZone) + '  ' + DatumCode + ' ' + Why); {$EndIf}
   {$If Defined(RecordLong0) or Defined(LongCent)} ShortProjInfo( 'tMapProjection.DefineDatumFromUTMZone in'); {$EndIf}

   if not (UTMZone in [1..60]) then begin
      {$If Defined(RecordShortDefineDatum) or Defined(RecordDefineDatum)} WriteLineToDebugFile('Reset UTM zone=' + IntToStr(MDDef.DefaultUTMZone) + '  ' + DatumCode + ' ' + Why); {$EndIf}
      if not (MDDef.DefaultUTMZone in [1..60]) then MDDef.DefaultUTMZone := 18;
      UTMZone := MDDef.DefaultUTMZone;
   end;
   LatHemi := inLatHemi;
   if not (LatHemi in ['N','S']) then LatHemi := 'N';
   h_datumCode := DatumCode;
   projUTMZone := UTMZone;
   ProtectedDefineDatum(DatumCode,Why);
   if (Pname = UTMellipsoidal) then begin
      if (LatHemi = 'N') then false_north := 0 else false_north := 10000000;
   end;
   {$If Defined(RecordShortDefineDatum) or Defined(RecordDefineDatum)} WriteLineToDebugFile(ProjDebugName + ' out DefineDatumFromUTMZone=' + KeyDatumParams + ' ' + Why); {$EndIf}
   {$If Defined(RecordLong0) or Defined(LongCent)} ShortProjInfo(ProjDebugName + ' tMapProjection.DefineDatumFromUTMZone out'); {$EndIf}
end;


function tMapProjection.MGRSvalid : boolean;
begin
   Result := (h_DatumCode = 'WGS84') or  (h_DatumCode = 'NAD83');
end;


function tMapProjection.LatLongToMGRS(Lat,Long : float64; TheLength : integer = 10) : shortstring;
var
   XUTM,YUTM : float64;
   tLong     : integer;
   TStr : ShortString;
begin
   Result := '';
   if (abs(Lat) < 0.0001) and (abs(Long) < 0.0001) then exit;
   if not MGRSvalid then exit;

   LatLongDegreetoUTM(Lat,Long,XUTM,YUTM);
   if (Lat > 0) then LatHemi := 'N' else LatHemi := 'S';
   DefineDatumFromUTMZone('WGS84',GetUTMZone(long),LatHemi);
   Result := WGS84DatumConstants.CalculateMGRS(Xutm,Yutm,TheLength);

   TStr := Copy(Result,1,3);
   if (TStr = '31V') or (TStr = '32V') or (TStr = '31X') or (TStr = '33X') or (TStr = '35X') or (TStr = '37X') then begin
   end
   else begin
      while (Long > 180) do long := long - 360;
      while (Long < -180) do Long := Long + 360;
      tLong := round(Long0 / DegToRad);
      if (Long < tlong - 4) or (Long > tlong + 4) then begin
         if ((tLong = 3) and (Long > 356)) or  ((tLong = 357) and (Long  < 4)) then begin
         end
         else Result := '';
      end;
   end;
end;


function tMapProjection.CalculateMGRS(X,Y : float64; UTMLen : byte) : shortstring;
var
   Lat,Long  : float64;
   Letter    : ANSIchar;
   GridZo    : ShortString;
begin
   Result := '';
   if not MGRSvalid then exit;
   if (x > 0.001) or (y > 0.001) then begin
      UTMtoLatLongDegree(X,Y,Lat,Long);
      GetHundredKMeterSquareID(GridZo,X,Y);
      Letter := AnsiChar((trunc(Lat + 80) div 8 + 67) {starts with "C"} );
      if (Letter >= 'I') and (Letter < 'X') then inc(Letter);   {I not used}
      if (Letter >= 'O') and (Letter < 'X') then inc(Letter);   {O not used}
      Str(projUTMZone,Result);
      Result := Result + Letter + BuildMGRSstring(GridZo,x,y,UTMLen);
   end;
end;

function UTMZoneCentralLong(UTMZone : byte) : integer;
begin
   Result := (-177+pred(UTMZone)*6);
end;


function UTMZoneExtent(UTMZone : byte) : shortstring;
begin
   Result := IntToStr(-180+pred(UTMZone)*6) + ' to ' + IntToStr(-180+UTMZone*6) + '°';
end;


procedure PickUTMZone(var UTMZone : int16);
var
   DatCode : ShortString;
begin
   {$IfDef VCL}
      GetMapParameters(MDDef.DefaultLatHemi,UTMZone,DatCode,true);
   {$EndIf}
end;


function GetUTMZone(Long : float64) : integer;
begin
   LongitudeAngleInRange(Long);
   Result := trunc((Long + 186) / 6);
end;


function EllipsoidXOffset(DatumCode, EllipsCode : ShortString) : integer;
{based on Fig. B-3 and B-4 in TM8358.1, in an unmarked and undated update/reprint that adds Clarke 1866 for NAD27 to the 10 character offsets, and leaves Clarke 1866 for the
    Phillipines and Marianas as 0 offset; Old Hawaiian not covered and assumed to have 0 offset as per results from GEOTRANS}
begin
   Result := 0;
   if (DatumCode = 'NAD27') or (copy(DatumCode,1,3) = 'NAS') {NAD27} or (EllipsCode = 'CD') {Clarke 1880 ellipsoid} or (EllipsCode = 'BR') {Bessel} then Result := 10;
end;


procedure ShiftZoneLetter(var c : ANSIchar);
begin
   case c of
      'L'..'N' : dec(c,11);  {A..C}
      'P'..'T' : dec(c,12);  {D..H}
      'U'..'V' : dec(c,11);  {J..K}
      'A'..'C' : inc(c,11);  {L..N}
      'D'..'H' : inc(c,12);  {P..T}
      'J'..'K' : inc(c,11);  {U..V}
   end;
end;


function GridZoneDes(Lat,Long : float64) : ShortString;
{  reference: DMA TM 8558.1, chapter 3;   Figure 8, page 3-2,  Army TM 5-241-1, 25 March 1983  }
var
   NumZone : integer;
   Letter  : ANSIchar;
begin
   if (Lat < -80) then
      if (Long < 0) then Result := 'A' else Result := 'B'
   else
   if (Lat > 84) then
      if (Long < 0) then Result := 'Y' else Result := 'Z'
   else begin
      NumZone := GetUTMZone(Long);
      Letter := Ansichar((trunc(Lat + 80) div 8 + 67) {starts with "C"} );
      if (Letter >= 'I') then inc(Letter);   {I not used}
      if (Letter >= 'O') then inc(Letter);   {O not used}
      if (Letter > 'X') then Letter := 'X';   {X zone is 12 degrees high}
      Str(NumZone,Result);
      Result := Result + Letter;

    {now check for exceptions}
      if (Result = '31V') and (Long >= 3) then Result := '32V';
      if (Letter = 'X') then begin
         if (Long >=  0) and (Long <  9) then Result := '31X';
         if (Long >=  9) and (Long < 21) then Result := '33X';
         if (Long >= 21) and (Long < 33) then Result := '35X';
         if (Long >= 33) and (Long < 42) then Result := '37X';
      end;
   end {if};
end {proc GridZoneDes};


function tMapProjection.PreferLocationString(Lat,Long : float64) : ShortString;
var
   XUTM,YUTM : float64;
begin
   {$IfDef RecordMapRoamProblems} WriteLineToDebugFile('Enter PreferLocationString for  ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
   LongitudeAngleInRange(Long);
   if (MDdef.CoordUse = coordLatLong) then begin
      Result := LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod);
   end
   else begin
      WGS84DatumConstants.LatLongDegreeToUTM(Lat,Long,XUTM,YUTM);
      Result := UTMLocationString(XUTM,YUTM);
   end;
   {$IfDef RecordMapRoamProblems} WriteLineToDebugFile('PreferLocationString for  ' + LatLongDegreeToString(Lat,Long) + '   ' + Result + '   Zone ' + IntegerToString(DatumConstants.UTMZone)); {$EndIf}
end;


function BuildMGRSstring(GridZo : ShortString; x,y : float64; UTMLen : integer) : shortstring;
var
   i         : integer;
   XUTM,YUTM,UTM  : ShortString;
begin
   UTMLen := UTMLen div 2;
   while (x >= 100000.0) do x := x - 100000.0;
   while (y >= 100000.0) do y := y - 100000.0;

   for i := 1 to (5 - UTMLen) do begin
      x := 0.1 * x;
      y := 0.1 * y;
   end;
   Str(x:UTMLen:0,XUTM);
   {take care of case where UTM rounds up to 10, 100, 1000...}
      if length(XUTM) > UTMLen then Delete(XUTM,1,1);
   Str(y:UTMLen:0,YUTM);
      if length(YUTM) > UTMLen then Delete(YUTM,1,1);
   UTM := XUTM + YUTM;
   for i := 1 to Length(UTM) do if (UTM[i] = ' ') then UTM[i] := '0';
   Result := GridZo + UTM;
end;


function tMapProjection.MGRStoLatLong(MGRS : ShortString; var Lat,Long : float64) : boolean;
{DMA TM 8358.1}
var
   i,Err,Err2,LatBand,Digits,UTMZone : integer;
   xutm,yutm : float64;
   TheZone : ShortString;
   OK   : boolean;
   ZoneCh : ZoneChars;
   ich    : ANSIchar;
begin
   Result := false;
   StripBlanks(MGRS);
   MGRS := UpperCase(MGRS);
   if MGRS[2] in ['A'..'Z'] then MGRS := '0' + MGRS;
   Val(Copy(MGRS,1,2),UTMZone,Err);
   if Err <> 0 then exit;
   {$IfDef RecordDefineDatum} WriteLineToDebugFile('MGRStoLatLong'); {$EndIf}
   DefineDatumFromUTMZone(h_DatumCode, UTMzone,HemiFromLat(Lat),'MGRSToLatLong');
   TheZone := Copy(MGRS,4,2);
   ich := MGRS[3];
   if not (ich in  ['A'..'Z']) then exit;
   if ich >= 'O' then dec(ich);   {O not used}
   if ich >= 'I' then dec(ich);   {I not used}
   LatBand := -80 + 8 * (ord(ich) - ord('C'));
   Delete(MGRS,1,5);
   if Odd(Length(MGRS)) then exit;
   Digits := Length(MGRS) div 2;
   Val(Copy(MGRS,1,Digits),XUTM,err);
   Val(Copy(MGRS,1+Digits,Digits),YUTM,err2);
   if (err <> 0) or (err2 <> 0) then exit;
   for i := 1 to 5-digits do begin
      XUTM := XUTM * 10;
      YUTM := YUTM * 10;
   end;
   case UTMZone mod 3 of
      0 : ZoneCh := Zone0;
      1 : ZoneCh := Zone1;
      2 : ZoneCh := Zone2;
   end;

   i := 0;
   while i <= 8 do begin
      inc(i);
      if i > 8 then exit;
      if ZoneCh[i] = TheZone[1] then break;
   end;
   XUTM := i * 100000 + XUTM;
   if EllipsoidXOffset(h_DatumCode,h_EllipsCode) = 10 then begin
      ich := TheZone[2];
      ShiftZoneLetter(ich);
      TheZone[2] := ich;
   end;

   if TheZone[2] >= 'O' then dec(TheZone[2]);
   if TheZone[2] >= 'I' then dec(TheZone[2]);

   if Odd(UTMZone) then YUTM := YUTM + 100000 * (Ord(TheZone[2]) - Ord('A'))
   else begin
      if TheZone[2] in ['A'..'E'] then  begin
         YUTM := YUTM + 100000 * (15 + Ord(TheZone[2]) - Ord('A'));
      end
      else YUTM := YUTM + 100000 * (Ord(TheZone[2]) - Ord('F'));
   end;

   repeat
      UTMtoLatLongDegree(XUTM,YUTM,Lat,Long);
      OK := (Lat >= (LatBand - 0.001)) and (Lat <= (LatBand + 8.001));
      if not OK then YUTM := YUTM + 2000000;
      if YUTM > 9999999 then exit;
   until OK;
   Result := true;
end;


procedure tMapProjection.GetHundredKMeterSquareID(var SquareID : ShortString; XUTM,YUTM : float64);
var
   NumK,NSoffset : integer;
begin
   SquareID := ' ';
   NSOffset := EllipsoidXOffset(H_DatumCode,H_EllipsCode);
   NumK := trunc(XUTM / 100000.0);
   if (NumK in [1..8]) then begin
      case projUTMZone mod 3 of
         0 : SquareID := Zone0[NumK];
         1 : SquareID := Zone1[NumK];
         2 : SquareID := Zone2[NumK];
      end {case};
      while YUTM > 2000000 do YUTM := YUTM - 2000000;
      NumK := trunc(abs(YUTM) / 100000.0) + NSoffset;
      if NumK in [0..29] then begin
         if odd(projUTMZone) then SquareID := SquareID + LetUsed[NumK]
         else SquareID := SquareID + LetUsed[5 + NumK];
      end;
   end;
end {proc HundredKMeterSquareID};


function tMapProjection.UTMLocationString(XUTM,YUTM : float64) : ShortString;
begin
   {$IfDef RecordMapRoamProblems} WriteLineToDebugFile('Enter PreferLocationString for  ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
   if (MDdef.CoordUse = coordFullUTM) then begin
      Result := 'Zone ' + IntToStr(projUTMZone) + LatHemi  + '  x=' + RealToString(XUTM,-12,0) + '  y=' + RealToString(YUTM,-12,0);
   end
   else begin
      Result := CalculateMGRS(XUTM,YUTM,10);
      if (Result <> '') and (MDdef.CoordUse = coordUTM ) then begin
         while Result[1] in ['0'..'9'] do Delete(Result,1,1);
         Delete(Result,1,1);
      end;
   end;
   {$IfDef RecordMapRoamProblems} WriteLineToDebugFile('PreferLocationString for  ' + LatLongDegreeToString(Lat,Long) + '   ' + Result + '   Zone ' + IntegerToString(DatumConstants.UTMZone)); {$EndIf}
end;



