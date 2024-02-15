
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{ include file for basemap}


procedure VincentyFullCalculateDistanceBearing(StartLat,StartLong,EndLat,EndLong : float64; EllipsoidConstants : tMapProjection;  var DistanceMeters,Bearing : float64);
//from T. Vincenty, Survey Review, 23, No 176, p 88-93,1975 to calculate lines ranging from a few cm to nearly 20,000 km, with millimetre accuracy
//verified with Excel spreadsheet from Geoscience Australia, http://www.auslig.gov.au/geodesy/datums/vincenty.xls  (http://www.auslig.gov.au/geodesy/datums/calcs.htm)
//requires a and f for the ellipsoid, for WGS84
//   a := 6378137;
//   h_f := 298.2572236;
//input in degrees


   {$IfDef RecordGeotdeticCalcDetailed}
         procedure RecordFirstValues;
         begin
            WriteLineToDebugFile('CalculateDistanceBearing');
            WriteLineToDebugFile('lambda1=' + realToString(lambda1,18,8));
            WriteLineToDebugFile('U1=' + realToString(u1,18,8));
            WriteLineToDebugFile('sinU1=' + realToString(sinu1,18,8));
            WriteLineToDebugFile('cosU1=' + realToString(cosu1,18,8));
            WriteLineToDebugFile('U2=' + realToString(u2,18,8));
            WriteLineToDebugFile('sinU2=' + realToString(sinu2,18,8));
            WriteLineToDebugFile('cosU2=' + realToString(cosu2,18,8));
            WriteLineToDebugFile('EllipsoidConstants.h_f=' + realToString(EllipsoidConstants.h_f,18,8));
         end;

         procedure RecordSecondValues
            WriteLineToDebugFile('');
            WriteLineToDebugFile('First lambda: ' + RealToString(Lambda1,18,-8));
            WriteLineToDebugFile('');
            WriteLineToDebugFile('sin_sq_sigma1=' + realToString(sin_sq_sigma1,18,8));
            WriteLineToDebugFile('cos_sigma1=' + realToString(cos_sigma1,18,8));
            WriteLineToDebugFile('tan_sigma1=' + realToString(tan_sigma1,18,8));
            WriteLineToDebugFile('sigma1=' + realToString(Sigma1,18,8));
            WriteLineToDebugFile('two_sigma_m1=' + realToString(two_sigma_m1,18,8));
            WriteLineToDebugFile('cos_two_sigma_m1=' + realToString(cos_two_sigma_m1,18,8));
            WriteLineToDebugFile('Sin_Alpha1=' + realToString(Sin_Alpha1,18,8));
            WriteLineToDebugFile('U_sq1=' + realToString(u_sq1,18,8));
            WriteLineToDebugFile('A1=' + realToString(A1,18,8));
            WriteLineToDebugFile('B1=' + realToString(B1,18,8));
            WriteLineToDebugFile('delta_sigma1=' + realToString(delta_sigma1,18,8));
            WriteLineToDebugFile('c1=' + realToString(c1,18,8));
         end;


         procedure RecordThirdValues;
         begin
            WriteLineToDebugFile('');
            WriteLineToDebugFile('sin_sq_sigma=' + realToString(sin_sq_sigma,18,12));
            WriteLineToDebugFile('cos_sigma=' + realToString(cos_sigma,18,12));
            WriteLineToDebugFile('tan_sigma=' + realToString(tan_sigma,18,12));
            WriteLineToDebugFile('sin_sigma=' + realToString(sin_sigma,18,12));
            WriteLineToDebugFile('sigma=' + realToString(Sigma,18,12));
            WriteLineToDebugFile('Sin_Alpha=' + realToString(Sin_Alpha,18,12));
            WriteLineToDebugFile('cos_two_sigma_m=' + realToString(cos_two_sigma_m,18,12));
            WriteLineToDebugFile('two_sigma_m=' + realToString(two_sigma_m,18,12));
            WriteLineToDebugFile('U_sq=' + realToString(u_sq,18,12));
            WriteLineToDebugFile('A=' + realToString(A,18,12));
            WriteLineToDebugFile('B=' + realToString(B,18,12));
            WriteLineToDebugFile('B_ell=' + realToString(B,18,12));
            WriteLineToDebugFile('delta_sigma=' + realToString(delta_sigma,18,12));
            WriteLineToDebugFile('c=' + realToString(c,18,12));
            WriteLineToDebugFile('lambda=' + realToString(lambda,18,12));
            WriteLineToDebugFile('delta_lambda=' + realToString(abs(Lambda - LastLambda),18,12));
            WriteLineToDebugFile('');
         end;

         procedure RecordFinalValues;
         begin
            WriteLineToDebugFile('Final');
            WriteLineToDebugFile('sin(lambda): ' + realToString(sin(Lambda),18,8)) ;
            WriteLineToDebugFile('Part 1: ' + realToString(cosU2 * sin(Lambda) * cosU2 * sin(Lambda),18,8)) ;
            WriteLineToDebugFile('Part 2: ' + realToString( sqr(cosU1 * sinU2 - sinU1 * cosU2 * cos(Lambda)),18,8) );
            WriteLineToDebugFile('sin_sq_sigma=' + realToString(sin_sq_sigma,18,8));
            WriteLineToDebugFile('cos_sigma=' + realToString(cos_sigma,18,8));
            WriteLineToDebugFile('tan_sigma=' + realToString(tan_sigma,18,8));
            WriteLineToDebugFile('sigma=' + realToString(Sigma,18,8));
            WriteLineToDebugFile('two_sigma_m=' + realToString(two_sigma_m,18,8));
            WriteLineToDebugFile('Sin_Alpha=' + realToString(Sin_Alpha,18,8));
            WriteLineToDebugFile('U_sq=' + realToString(u_sq,18,8));
            WriteLineToDebugFile('A=' + realToString(A,18,8));
            WriteLineToDebugFile('B=' + realToString(B,18,8));
            WriteLineToDebugFile('lambda=' + realToString(lambda,18,8));
            WriteLineToDebugFile('c=' + realToString(c,18,8));
            WriteLineToDebugFile('tan_alpha =' + realToString(tan_alpha ,18,8));
            WriteLineToDebugFile('');
         end;
   {$EndIf}


var
   sin_sq_sigma1,sin_sigma1,cos_sigma1,tan_sigma1,sigma1,sin_alpha1,alpha1,cos_two_sigma_m1,two_sigma_m1,u_sq1,
   A1,B1,delta_sigma1,c1,Lambda1,b_ell,u_sq,tanU1,U1,sinU1,cosU1,A,B,sigma,tanU2,U2,sinU2,cosU2,
   One_minus_f,Lambda,LastLambda,c,alpha,tan_sigma,sin_alpha,tan_alpha,Lat1,Lat2,Long1,Long2,sin_sq_sigma,
   cos_two_sigma_m,two_sigma_m,delta_sigma,cos_sigma,sin_sigma  : extended;
   i : integer;
begin
   {$IfDef RecordGeodeticCalc} WriteLineToDebugFile('FullCalculateDistanceBearing in  Start:  ' + LatLongDegreeToString(StartLat,StartLong) + ' End:    ' + LatLongDegreeToString(EndLat,EndLong));  {$EndIf}

   if (abs(StartLat-EndLat) < 0.0000001) and (abs(StartLong-EndLong) < 0.0000001) then begin
      DistanceMeters := 0;
      Bearing := 0;
      exit;
   end;

   Lat1 := StartLat * DegToRad;
   Long1 := StartLong * DegToRad;
   Lat2 := EndLat * DegToRad;
   Long2 := EndLong * DegToRad;

   //RealF := {1 / }EllipsoidConstants.h_f;
   One_Minus_f := 1 - EllipsoidConstants.h_f;
   b_ell := EllipsoidConstants.a * One_Minus_f;

   tanU1 := One_Minus_f * Math.tan(Lat1);
   U1 := arctan(tanU1);
   sinU1 := sin(u1);
   cosU1 := cos(U1);

   tanU2 := One_Minus_f * Math.tan(Lat2);
   U2 := arctan(tanU2);
   sinU2 := sin(u2);
   cosU2 := cos(U2);

   lambda1 := Long2 - Long1;

   {$IfDef RecordGeotdeticCalcDetailed} RecordFirstValues; {$EndIf}

   //formula 14
   sin_sq_sigma1 := (cosU2 * sin(Lambda1) * cosU2 * sin(Lambda1)) + sqr(cosU1 * sinU2 - sinU1 * cosU2 * cos(Lambda1));
   sin_sigma1 := sqrt(sin_sq_sigma1);
   //formula 15
   cos_sigma1 := sinU1 * sinU2 + cosU1 * cos(u2) * cos(Lambda1);
   //formula 16
   tan_sigma1 := sin_sigma1 / cos_sigma1;
   sigma1 := Math.ArcTan2(sin_sigma1, cos_sigma1);
   //formula 17
   if sin_sq_sigma1 < 0.00000000001 then sin_alpha1 := 0
   else sin_alpha1 := cosU1 * cosU2 * sin(lambda1) / sin_sigma1;
   alpha1 := Math.ArcSin(sin_alpha1);
   //formula 18
   cos_two_sigma_m1 := cos_sigma1 - 2 * sinU1 * sinU2 / sqr(cos(alpha1));
   two_sigma_m1 := ArcCos(cos_two_sigma_m1);

   u_sq1 := sqr(cos(Alpha1)) * (sqr(EllipsoidConstants.a) - sqr(b_ell)) / sqr(b_ell);

   //formula 3
   A1 := 1 + u_sq1 / 16384 * (4096 + u_sq1 * ( -768 + u_sq1 * ( 320 - 175 * u_sq1)));
   //formula 4
   B1 := u_sq1 / 1024 * ( 256 + u_sq1 * (-128 + u_sq1 * (74 - 47 * u_sq1)));
   //formula 6
   delta_sigma1 := B1 * sin(sigma1) * (cos(two_sigma_m1) + 0.25 * B1 * ( cos(sigma1) * (-1 + 2 * sqr(cos(two_sigma_m1))
               - 1 / 6 * B1 * cos(two_sigma_m1) * (-3 + 4 * sqr(sqr(sin(sigma1)))) * (-3 + 4 * sqr(sqr(cos(two_sigma_m1)))) )));
   //formula 10
   c1 := EllipsoidConstants.h_f  / 16 * sqr(cos(alpha1)) * (4 + EllipsoidConstants.h_f * (4 - 3 * sqr(cos(alpha1))));

   //formula 11
   Lambda1 := Lambda1 + (1 - c1) * EllipsoidConstants.h_f * sin_alpha1  * (sigma1 + C1 * sin_sigma1 * (cos_two_sigma_m1 + c1 * cos_sigma1 * (-1 + 2 * sqr(cos_two_sigma_m1))));

   {$IfDef RecordGeotdeticCalcDetailed}   RecordSecondValues; {$EndIf}

    Lambda := Lambda1;

   for i := 1 to 1 do begin
   //repeat
      LastLambda := Lambda;
      //formula 14
      sin_sq_sigma := (cosU2 * sin(Lambda) * cosU2 * sin(Lambda)) + sqr(cosU1 * sinU2 - sinU1 * cosU2 * cos(Lambda));
      sin_sigma := sqrt(sin_sq_sigma);
      //formula 15
      cos_sigma := sinU1 * sinU2 + cosU1 * cos(u2) * cos(Lambda);
      //formula 16
      tan_sigma := sin_sigma / cos_sigma;
      sigma := Math.ArcTan2(sin_sigma, cos_sigma);
      //formula 17
      if sin_sq_sigma < 0.00000000001 then sin_alpha := 0
      else sin_alpha := cosU1 * cosU2 * sin(lambda) / sin_sigma;
      alpha := Math.ArcSin(sin_alpha);
      //formula 18
      cos_two_sigma_m := cos_sigma - 2 * sinU1 * sinU2 / sqr(cos(alpha));
      two_sigma_m := ArcCos(cos_two_sigma_m);

      u_sq := sqr(cos(Alpha)) * (sqr(EllipsoidConstants.a) - sqr(b_ell)) / sqr(b_ell);

      //formula 3
      A := 1 + u_sq / 16384 * (4096 + u_sq * ( -768 + u_sq * ( 320 - 175 * u_sq)));
      //formula 4
      B := u_sq / 1024 * ( 256 + u_sq * (-128 + u_sq * (74 - 47 * u_sq)));
      //formula 6
      delta_sigma := B * sin(sigma) * (cos_two_sigma_m + 0.25 * B * (cos(sigma) * (-1 + 2 * sqr(cos_two_sigma_m)) - 1 / 6 * B * cos_two_sigma_m * (-3 + 4 * sqr(sqr(sin(sigma)))) * (-3 + 4 * sqr(sqr(cos_two_sigma_m)) )));
      //formula 10
      c := EllipsoidConstants.h_f  / 16 * sqr(cos(alpha)) * (4 + EllipsoidConstants.h_f * (4 - 3 * sqr(cos(alpha))));

      //formula 11
      Lambda := (Long2 - Long1) + (1 - c) * EllipsoidConstants.h_f * sin_alpha  * (sigma + C * sin_sigma * (cos_two_sigma_m + c * cos_sigma * (-1 + 2 * sqr(cos_two_sigma_m))));

      {$IfDef RecordGeotdeticCalcDetailed} RecordThirdValues; {$EndIf}
   end;

   //formula 20
   tan_alpha := cosU2 * sin(Lambda) / (cosU1*sinU2 - sinU1 * cosU2 * cos(Lambda));

   Bearing := Math.ArcTan2(cosU2 * sin(Lambda), (cosU1*sinU2 - sinU1 * cosU2 * cos(Lambda))) / DegToRad;
   if (Bearing < 0) then Bearing := Bearing + 360;

   {$IfDef RecordGeotdeticCalcDetailed} WriteLineToDebugFile('Call Formula 19'); {$EndIf}

   //formula 19
   DistanceMeters := b_ell * (sigma - delta_sigma) * A;
   DistanceMeters := abs(DistanceMeters);  //added  1/15/2022; not sure why it is coming out negative

   {$IfDef RecordGeotdeticCalcDetailed} RecordFinalValues; {$EndIf}
   {$IfDef RecordGeotdeticCalc} WriteLineToDebugFile('CalculateDistanceBearing out, Distance=' + RealToString(Distance,12,3) + '  Bearing=' + RealToString(Bearing,8,3)); {$EndIf}
end;


procedure VincentyCalculatePointAtDistanceBearing(StartLat,StartLong,Distance,Bearing : float64; EllipsoidConstants : tMapProjection; var Lat,Long : float64);
//from T. Vincenty, Survey Review, 23, No 176, p 88-93,1975 to calculate lines ranging from a few cm to nearly 20,000 km, with millimetre accuracy
//verified with Excel spreadsheet from Geoscience Australia, http://www.auslig.gov.au/geodesy/datums/vincenty.xls  (http://www.auslig.gov.au/geodesy/datums/calcs.htm)
var
   b_ell,u_sq,tanU1,tanSigma1,U1,sinU1,cosU1,sinAlpha,A,B,sigma,
   top,bott,One_minus_f,tan_Lambda,Lambda,c,BsinSigma,
   Cos2SigmaM,CosSigmaEtc,Bracket1,Bracket2,delta_sigma_left,delta_sigma_right,
   sinBearing,cosBearing,alpha,l,two_sigma_1,
   two_sigma_m,cos_sigma,sin_sigma  : extended;
begin
   {$IfDef RecordGeotdeticCalc}
   WriteLineToDebugFile('PointAtDistanceBearing in, Start: ' + LatLongDegreeToString(StartLat,StartLong) + '  Imput Bearing=' + RealToString(Bearing,-8,3) + '   Input Distance=' + RealToString(Distance,-18,3));
   {$EndIf}
   sinBearing := sin(Bearing * DegToRad);
   cosBearing := cos(Bearing * DegToRad);

   //RealF := 1 / EllipsoidConstants.h_f;
   One_Minus_f := 1 - EllipsoidConstants.h_f;
   b_ell := EllipsoidConstants.a * One_Minus_f;

   //formula 1
   tanU1 := One_Minus_f * Math.tan(StartLat*DegToRad);
   U1 := arctan(tanU1);
   tanSigma1 := tanU1 / cosBearing;

   {$IfDef RecordGeotdeticCalcDetailed}
      WriteLineToDebugFile('cosbearing=' + realToString(cosbearing,18,8));
      WriteLineToDebugFile('radius diff,sq=' + realToString( (sqr(EllipsoidConstants.a) - sqr(b_ell)) / sqr(b_ell),18,8));
      WriteLineToDebugFile('Lat=' + realToString(StartLat,18,8));
      WriteLineToDebugFile('a_ell=' + realToString(EllipsoidConstants.a,-18,8) + '   b_ell=' + realToString(b_ell,-18,8));
      WriteLineToDebugFile('One_Minus_f=' + realToString(One_Minus_f,18,8));
      WriteLineToDebugFile('Formula 1');
      WriteLineToDebugFile('tanU1=' + realToString(tanu1,18,8));
      WriteLineToDebugFile('U1=' + realToString(u1,18,8));
      WriteLineToDebugFile('tansigma1=' + realToString(tansigma1,18,8));
   {$EndIf}

   //formula 2
   sinU1 := sin(u1);
   cosU1 := cos(U1);
   sinAlpha := cosU1 * sinBearing;
   Alpha := Math.ArcSin(sinAlpha);
   u_sq := sqr(cos(Alpha)) * (sqr(EllipsoidConstants.a) - sqr(b_ell)) / sqr(b_ell);

   //formula 3  , CELL C24
   A := 1 + u_sq / 16384 * (4096 + u_sq * ( -768 + u_sq * ( 320 - 175 * u_sq)));
   //formula 4  , CELL C26
   B := u_sq / 1024 * ( 256 + u_sq * (-128 + u_sq * (74 - 47 * u_sq)));

   {$IfDef RecordGeotdeticCalcDetailed}
      WriteLineToDebugFile('Formula 2');
      WriteLineToDebugFile('sinu1=' + realToString(sinu1,-18,8) + '   cosu1=' + realToString(cosu1,-18,8));
      WriteLineToDebugFile('Alpha=' + realToString(Alpha,-18,8) +  '  SinAlpha=' + realToString(SinAlpha,-18,8));
      WriteLineToDebugFile('U_sq=' + realToString(u_sq,-18,8);
      WriteLineToDebugFile('Formula 3-4' + '  A=' + realToString(A,-18,8) + '   B=' + realToString(B,-18,8));
   {$EndIf}

   //first approximation;
   sigma := Distance / b_ell * A;

   {$IfDef RecordGeotdeticCalcDetailed}
    WriteLineToDebugFile('Start iteration',true);
   {$EndIf}

   two_sigma_1 := 2 * arcTan(tanSigma1);   //F28

   repeat
      //formula 5
      two_sigma_m := 2 * arcTan(tanSigma1) + sigma;   //C28

      BsinSigma :=  B * sin(sigma);                   //F29
      Cos2SigmaM := cos(two_sigma_m);                 //F30
      CosSigmaEtc :=  cos(sigma) * (-1 + 2 * sqr(Cos2SigmaM)); //F31
      Bracket1 := (-3 + 4 * sqr(sin(sigma)));                  //F33
      Bracket2 := (-3 + 4 * sqr(Cos2SigmaM));                  //F34

      //formula 6
      delta_sigma_Right := BsinSigma * (Cos2SigmaM + 0.25 * B * (CosSigmaEtc - B / 6  * Cos2SigmaM * Bracket1 * Bracket2));
      delta_sigma_left := BsinSigma * (Cos2SigmaM + 0.25 * B * (CosSigmaEtc - B / 6  * Cos2SigmaM * Bracket1 * Bracket2));

      //formula 7
      sigma := Distance / (b_ell * A) + Delta_sigma_Right;

      {$IfDef RecordGeotdeticCalc} WriteLineToDebugFile('sigma=' + RealToString(sigma,18,8) +' delta_sigma_left=' + RealToString(delta_sigma_Left,18,12) + ' delta_sigma_right=' + RealToString(delta_sigma_right,18,12)); {$EndIf}
   until abs(delta_sigma_left - delta_sigma_right) < 1e-14;

   {$IfDef RecordGeotdeticCalcDetailed} WriteLineToDebugFile('Done iteration');{$EndIf}

   cos_sigma := cos(sigma);
   sin_sigma := sin(sigma);

   //formula 8
   top := sin (U1)*cos_sigma + cos(U1)*sin_sigma*cosBearing;
   bott := One_Minus_f * sqrt(sqr(sinAlpha) + sqr(sinU1 * sin_sigma - cosU1 * cos_sigma * cosBearing));
   Lat := arctan(top / bott) / DegToRad;

   //formula 9
   tan_Lambda := sin_sigma * sinBearing / (cosU1 * cos_sigma - sinU1 * sin_sigma * cosBearing);
   Lambda := Math.Arctan2(sin_sigma * sinBearing , (cosU1 * cos_sigma - sinU1 * sin_sigma * cosBearing));

   //formula 10
   c := EllipsoidConstants.h_f / 16 * sqr(cos(alpha)) * (4 + EllipsoidConstants.h_f * (4 - 3 * sqr(cos(alpha))));

   //formula 11
   L := Lambda - (1 - c) * EllipsoidConstants.h_f * sinAlpha  * (sigma + C * sin_sigma * (cos(two_sigma_m) + c * cos_sigma * (-1 + 2 * sqr(cos(two_sigma_m)))));
   Long := StartLong + L / DegToRad;

   {$IfDef RecordGeotdeticCalcDetailed}
      WriteLineToDebugFile('sigma=' + realToString(Sigma,18,8));
      WriteLineToDebugFile('top=' + realToString(top,18,8));
      WriteLineToDebugFile('bott=' + realToString(bott,18,8));
      WriteLineToDebugFile('tan_Lambda =' + realToString(tan_Lambda,18,8),true);
      WriteLineToDebugFile('lambda=' + realToString(lambda,18,8),true);
      WriteLineToDebugFile('c=' + realToString(c,18,8),true);
      WriteLineToDebugFile('l=' + realToString(l,18,8),true);
   {$EndIf}
   {$IfDef RecordGeotdeticCalc} WriteLineToDebugFile('lat=' + realToString(lat,18,8) + '   long=' + realToString(long,18,8));  {$EndIf}
end;

function DistanceInKMLawOfCosines(Lat1,Long1,Lat2, Long2 : float64) : float64;
//https://gis.stackexchange.com/questions/4906/why-is-law-of-cosines-more-preferable-than-haversine-when-calculating-distance-b
//wikipedia
begin
  Result := arccos(SINDeg(lat1)*SINDeg(lat2)+COSDeg(lat1)*COSDeg(lat2)*COSDeg(long2-long1))*SphericalEarthAkm;
end;


function DistanceInKMHaversine(Lat1,Long1,Lat2, Long2 : float64) : float64;
var
   dLat,dLong,a : float64;
begin
//https://gis.stackexchange.com/questions/4906/why-is-law-of-cosines-more-preferable-than-haversine-when-calculating-distance-b
//wikipedia
  dLat := (lat2-lat1);
  dLong := (long2-long1);
  a := sqr(sinDeg(dLat/2)) + (cosDeg(lat1) * cosDeg(lat2) * sqr(sinDeg(dLong/2)));
  Result := SphericalEarthAkm * 2 * atan2(sqrt(a),sqrt(1-a));
end;



procedure VincentyPointAtDistanceBearing(StartLat,StartLong,DistanceMeters,Bearing : float64; var Lat,Long : float64);
begin
   VincentyCalculatePointAtDistanceBearing(StartLat,StartLong,DistanceMeters,Bearing,WGS84DatumConstants,Lat,Long);
end;


procedure VincentyCalculateDistanceBearing(StartLat,StartLong,EndLat,EndLong : float64; var DistanceMeters,Bearing : float64);
var
   IntDistance,IntBearing : float64;
begin
   VincentyFullCalculateDistanceBearing(StartLat,StartLong,EndLat,EndLong,WGS84DatumConstants, IntDistance,IntBearing);
   {$IfDef RecordGeotdeticCalc} WriteLineToDebugFile('CalculateDistanceBearing done   IntDistance=' + realToString(IntDistance,-18,2) + '  IntBearing=' + realToString(IntBearing,-18,2)); {$EndIf}

   //crash here in Android for the final conversion; could not fix, 10/18/2015
   DistanceMeters := IntDistance;
   Bearing := IntBearing;
end;



