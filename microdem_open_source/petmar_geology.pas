{$N+,E+,F+}

unit petmar_geology;


{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}



{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   Types,Math,Classes,Sysutils,
   Petmath,Petmar_types;

{ Dip and strike formats:                           }
{  1.  Quadrant N34E 69SE, strike, dip, dip direction}
{  2.  Right hand rule, 34 69, strike, dip; dip direction in 90ø clockwise from strike }
{  3.  Right hand rule with override, 34 69 SE, strike, dip, optional dip direction; right hand rule used if omitted}
{  4.  Dip-dip direction, 69 124 or 69 S56E }

procedure StrikeDirection(DipDirect : float32; var StrikeDirect : float32);
procedure StrikeFromNorth(var Strike : float32);
procedure StrikeIntoString(DipDirect : float32; var StrikeString : ShortString; NorthStrikesOnly : boolean = true);
function DipDirectionRHR(Strike : float32) : float32;
procedure StripDipAndStrike(WrkS : string10; var Dip,Strike,DipDir : float32; var OK : boolean);
procedure StripDirection(GeoData : string10;  var Direction : integer; var OK : boolean);
function CreateStrikeAndDipString(Strike,Dip : float32) : ShortString;
procedure QuadrantDipDirection(var DipDir : float32; Strike : float32; WrkS : string16);

procedure DipAndStrikeFromThreePoints(x1,y1,z1,x2,y2,z2,x3,y3,z3 : float32; var DipAndStrike,Slope : string16; var Dip,Strike,DipDir : float32);

procedure CalculateThickness(var XUTM1,YUTM1,Elv1,XUTM2,YUTM2,Elv2 : float32;  Dip,DipDir : integer; var Thick : float32);
procedure ApparentDip(Dip,Strike : integer; Azimuth : integer; var AppDip : float32);  {AppDip = apparent dip in plane of section}

procedure DipStrikeToCartesian(Dip,Strike : float32; var A : VectorType);
procedure CartesianToDipStrike(A : VectorType; var Dip,Strike : float32);

{$IfDef VCL}
   procedure GetStrikeAndDip(Message : shortstring; var DipAndStrike : shortstring; var Dip,Strike,DipDirect : float32);
   procedure GetDirection(var DirString : string10;  var Direction : integer);
{$EndIf}


implementation


uses
  DEMDefs,
  Petmar;


procedure DipStrikeToCartesian(Dip,Strike : float32; var A : VectorType);
{conventions from p.19, Fisher, Lewis, & Embleton}
var
   Lat,Long : float64;
begin
   Lat := Dip * DegToRad;
   Long := (360 - Strike) * DegToRad;
   LatLongToCartesian(Lat,Long,A);
end;


procedure CartesianToDipStrike(A : VectorType; var Dip,Strike : float32);
var
   Dip64,Strike64 : float64;
begin
   CartesianToLatLong(A,Dip64,Strike64);
   Dip := Dip64 / DegToRad;
   Strike := 360 - (Strike64 / DegToRad);
end;


procedure GetDirection(var DirString : string10;  var Direction : integer);
var
   OK  : boolean;
begin
   repeat
      GetString('Direction',DirString,true,['0'..'9','N','S','E','W','-','/',' ',#27]);
      if (DirString = '') or (DirString = #27) then OK := true
      else StripDirection(DirString,Direction,OK);
      if not OK then MessageToContinue('Valid: N45E, 69');
   until OK;
end;



procedure VectorToDipStrike(x : VectorType;  var DipDir,Dip : float64);
{input is vector normal to plane; procedure returns dip and dip direction of plane}
var
   Angle : float64;
begin
   {normalize x[1]}
      Angle := sqrt( sqr(x[1]) + sqr(x[2]) + sqr(x[3]) );
      x[1] := x[1] / Angle;
   DipDir := HeadingOfLine(x[3],x[2]);
   Dip :=  (Math.ArcCos(x[1]) / DegToRad);
   if (Dip > 90) then begin
      Dip := 180 - Dip;
      DipDir := DipDir + 180;
   end {if};
end {proc VectorToDipStrike};


procedure VectorToDipStrike32(x : VectorType32;  var DipDir,Dip : float32);
{input is vector normal to plane; procedure returns dip and dip direction of plane}
var
   Angle : float32;
begin
   {normalize x[1]}
      Angle := sqrt( sqr(x[1]) + sqr(x[2]) + sqr(x[3]) );
      x[1] := x[1] / Angle;
   DipDir := HeadingOfLine(x[3],x[2]);
   Dip :=  (Math.ArcCos(x[1]) / DegToRad);
   if (Dip > 90) then begin
      Dip := 180 - Dip;
      DipDir := DipDir + 180;
   end {if};
end {proc VectorToDipStrike};


procedure StrikeDirection(DipDirect : float32; var StrikeDirect : float32);
{ Strike direction must be 90 degrees counterclockwise from DipDirect }
begin
   StrikeDirect := CompassAngleInRangeFloat64(DipDirect - 90);
end;

procedure StrikeFromNorth(var Strike : float32);
{ gets strike that will correspond to N**E or N**W }
begin
   if (Strike > 90) and (Strike < 270) then Strike := Strike + 180;
   if Strike > 360 then Strike := Strike - 360;
end;


function CreateStrikeAndDipString(Strike,Dip : float32) : ShortString;
var
   DipDir : float32;
   StrikeString,DipValue : ShortString;
begin
   if Dip < 0.01 then Result := 'Horizontal'
   else begin
      DipDir := CompassAngleInRangeFloat64(Strike + 90);
      StrikeIntoString(DipDir,StrikeString);
      if (Dip <= 5) then Str(Dip:3:1,DipValue) else Str(Dip:2:0,DipValue);
      Result := StrikeString + ' ' + DipValue + AzimuthToDirection(DipDir);
   end;
end;

procedure StrikeIntoString(DipDirect : float32; var StrikeString : ShortString; NorthStrikesOnly : boolean = true);
{puts dip and strike values into format N**E or N**W }
var
   Strike  : float32;
   ch1,ch2 : AnsiChar;
begin
   StrikeDirection(DipDirect,Strike);
   if NorthStrikesOnly then StrikeFromNorth(Strike);
   if (abs(Strike) < 0.0001) or (abs(Strike - 360) < 0.0001) then
      StrikeString := 'N0E'
   else begin
      if (Strike > 90) and (Strike < 270) then begin
         ch1 := 'S';
         if Strike < 180 then begin
            ch2 := 'E';
            Str(round(180-Strike):2,StrikeString);
         end
         else begin
            ch2 := 'W';
            Str(round(Strike-180):2,StrikeString);
         end;
      end
      else begin
        ch1 := 'N';
        if Strike <= 90 then begin
            Str(round(Strike):2,StrikeString);
            ch2 := 'E';
        end
        else begin
            Str(round(360-Strike):2,StrikeString);
            ch2 := 'W';
        end;
      end;
      StrikeString := ch1 + StrikeString + ch2;
   end;
end;


function DipDirectionRHR(Strike : float32) : float32;
{ Dip direction must be 90 degrees clockwise from Strike Direct }
begin
   Result := CompassAngleInRangeFloat32(Strike + 90);
end;

procedure QuadrantDipDirection(var DipDir : float32; Strike : float32; WrkS : string16);
var
   i : integer;
begin
   DipDir := DipDirectionRHR(Strike);
   i := round(DipDir) div 45;
   if ((WrkS = 'N')  and (i in [2..5])) or ((WrkS = 'NE') and (i in [3..6])) or ((WrkS = 'E')  and (i in [4..7])) or ((WrkS = 'SE') and (i in [0,5..8])) or
      ((WrkS = 'S')  and (i in [0..1,6..8])) or ((WrkS = 'SW') and (i in [0..2,7..8])) or ((WrkS = 'W')  and (i in [0..3,8])) or ((WrkS = 'NW') and (i in [1..4])) then DipDir := DipDir + 180;
   DipDir := CompassAngleInRangeFloat32(DipDir);
end;


procedure DipAndStrikeFromThreePoints(x1,y1,z1,x2,y2,z2, x3,y3,z3 : float32; var DipAndStrike,Slope : string16; var Dip,Strike,DipDir : float32);
var
   Constant : float32;    {constants for equation of plane}
   Normal   : VectorType32;
begin
   PlaneEquationFromThreePoints(x1,y1,z1,x2,y2,z2,x3,y3,z3,Normal[3],Normal[2],Normal[1],Constant);
   VectorToDipStrike32(Normal,DipDir,Dip);
   Slope := ' (' + ptTrim(FloatToStrF(TanDeg(Dip) * 100,ffnumber,6,2)) + '%)';
   DipDir := CompassAngleInRangeFloat32(DipDir);
   Strike := CompassAngleInRangeFloat32(DipDir - 90);
   DipAndStrike := CreateStrikeAndDipString(Strike,Dip);
   {$IfDef RecordGeology} WriteLineToDebugFile('DipAndStrikeFromThreePoints=' + DipAndStrike + Slope); {$EndIf}
end {proc DipAndStrikeFromThreePoints};


{$IfDef VCL}

procedure GetStrikeAndDip(Message : shortstring; var DipAndStrike : shortstring;  var Dip,Strike,DipDirect : float32);
var
   OK  : boolean;
begin
   repeat
      GetString('Orientation for ' + Message,DipAndStrike,true,['0'..'9','N','S','E','W','-','/',' ','.']);
      if DipAndStrike = '' then OK := true
      else StripDipAndStrike(DipAndStrike,Dip,Strike,DipDirect,OK);
      if not OK then MessageToContinue('Valid: N45E 23E, 36 N63W, 34/356');
   until OK;
end;
{$EndIf}


procedure StripDipAndStrike(WrkS : string10; var Dip,Strike,DipDir : float32; var OK : boolean);
{strips user input of data entered as a dip and strike, such as N56E 45NW }
var
   i,code      : integer;
   StrikeDir   : ANSIchar;


      procedure StripDipDirection(WrkS : string10; var Dip,DipDir, Strike : float32; var OK : boolean);
      {strips user input of data entered as a pole, as 56 N45W }
      var
         code                    : integer;
      begin
         OK := false;
         if Length(WrkS) = 0 then exit;
         Wrks := ptTrim(UpperCase(Wrks));
         if Copy(Wrks,1,2) = 'NS' then begin
            Delete(Wrks,1,2);
            Wrks := 'N90E' + Wrks;
         end;
         val(copy(WrkS,1,2),Dip,code);
         if (code <> 0) then begin
            val(copy(WrkS,1,1),Dip,code);
            if (code <> 0) then exit;
            delete(WrkS,1,1);
         end
         else Delete(WrkS,1,2);
         if WrkS[1] in ['N','S'] then
            if WrkS[length(WrkS)] in ['E','W'] then begin
               val(copy(WrkS,2,length(WrkS)-2),DipDir,code);
               case WrkS[1] of
                  'N' : case WrkS[length(WrkS)] of
                           'E' : DipDir := DipDir;
                           'W' : DipDir := 360 - DipDir;
                         end {case};
                  'S' : case WrkS[length(WrkS)] of
                           'E' : DipDir := 180 - DipDir;
                           'W' : DipDir := 180 + DipDir;
                         end {case};
                end {case};
                StrikeDirection(DipDir,Strike);
            end
            else exit;
         OK := true;
      end {procedure StripDipDirection};


      procedure StripAllNumbers(WrkS : string10; var Dip,DipDir,Strike : float32; var OK :  boolean);
      {strips user input of data entered as 56/329  }
      {valid delimiters between dip and dip direction are [' ','-','/',',']}
      const
         Delimiters = [' ','-','/',','];
      var
         code  : integer;
         TStr  : ShortString;

            procedure StripForNumber;
            begin
               while (length(Wrks) > 0) and (WrkS[1] in Delimiters) do Delete(WrkS,1,1);
               TStr := '';
               while (length(Wrks) > 0) and (Wrks[1] in ['0'..'9','.']) do begin
                  TStr := TStr + Wrks[1];
                  Delete(WrkS,1,1);
               end {while};
            end;


      begin
         OK := false;
         if Length(WrkS) = 0 then exit;
         StripForNumber;
         if MDDef.AllowRightHandRule then val(TStr,Strike,code) else Val(TStr,Dip,code);
         if (code <> 0) then exit;
         StripForNumber;
         if MDDef.AllowRightHandRule then val(TStr,Dip,code) else Val(TStr,DipDir,code);
         if (code <> 0) then exit;
         if dip > 90 then exit;
         if MDDef.AllowRightHandRule then begin
            ptTrim(WrkS);
            if Length(WrkS) > 0 then QuadrantDipDirection(DipDir,Strike,WrkS)
            else DipDir := DipDirectionRHR(Strike);
         end
         else StrikeDirection(DipDir,Strike);
        OK := true;
      end {procedure StripAllNumbers};



begin {procedure StripDipAndStrike}
   OK := false;
   StripAllNumbers(WrkS,Dip,DipDir,Strike,OK);
   if OK then exit;
   Wrks := UpperCase(WrkS);
   ptTrim(WrkS);
   StripDipDirection(WrkS,Dip,DipDir,Strike,OK);
   if OK then exit;
   if Length(WrkS) = 0 then exit;
   if WrkS[1] in ['N','S'] then begin
      StrikeDir := WrkS[1];
      Delete(WrkS,1,1);
      i := 0;
      repeat
         inc(i);
      until (WrkS[succ(i)] in ['N','S','E','W']) or (i > Length(wrks));
      val(copy(WrkS,1,i),Strike,code);
      if (code <> 0) or (Strike < 0) or (Strike > 90) then exit;
      Delete(WrkS,1,i);
      if WrkS[1] = 'E' then
         if StrikeDir = 'N' then strike := Strike
         else {StrikeDir = 'S'}  strike := 180 - strike
      else if WrkS[1] = 'W' then
         if StrikeDir = 'N' then strike := 360 - Strike
         else {StrikeDir = 'S'}  strike := 180 + strike
      else exit;
      Delete(WrkS,1,1);
      if length(WrkS) = 0 then exit;
      if Wrks = '90' then Wrks := '90E';

      i := 0;
      repeat
         inc(i);
      until (WrkS[succ(i)] in ['N','S','E','W']) or (i > Length(wrks));
      val(copy(WrkS,1,i),Dip,code);
      if (code <> 0) or (Dip < 0) or (Dip > 90) then exit;
      Delete(WrkS,1,i);
      QuadrantDipDirection(DipDir,Strike,WrkS);
      OK := true;
   end;
end {procedure StripDipAndStrike};


procedure StripDirection(GeoData : string10;  var Direction : integer;  var OK : boolean);
{strips user input of data entered as a direction, such as N56E }
var
   i,code          : integer;
   StrikeDirection : ANSIchar;
begin
   OK := false;
   Val(GeoData,Direction,Code);
   if Code = 0 then begin
      OK := true;
      while Direction > 360 do dec(Direction, 360);
      while Direction < 0 do inc(Direction, 360);
      exit;
   end;
   Geodata := UpperCase(GeoData);
   if Length(GeoData) = 0 then exit;
   if GeoData[1] in ['N','S'] then begin
      StrikeDirection := GeoData[1];
      Delete(GeoData,1,1);
      if GeoData[2] in ['N','S','E','W'] then i := 1
      else if GeoData[3] in ['N','S','E','W'] then i := 2
           else exit;
      val(copy(GeoData,1,i),Direction,code);
      if (code <> 0) or (Direction < 0) or (Direction > 90) then exit;
      Delete(GeoData,1,i);
      if (GeoData[1] <> 'E') and (GeoData[1] <> 'W') then exit;
      if GeoData[1] = 'E' then
         if StrikeDirection = 'N' then Direction := Direction
         else {StrikeDirection = 'S'}  Direction := 180 - Direction;
      if GeoData[1] = 'W' then
         if StrikeDirection = 'N' then Direction := 360 - Direction
         else {StrikeDirection = 'S'}  Direction := 180 + Direction;
      OK := true;
   end;
end {procedure StripDirection};


procedure CalculateThickness(var XUTM1,YUTM1,Elv1,XUTM2,YUTM2,Elv2 : float32; Dip,DipDir : integer; var Thick : float32);
var
   h,v,DownHillDir  : float32;
begin
   if Elv1 < Elv2 then DownHillDir := HeadingOfLine(XUTM1-XUTM2,YUTM1-YUTM2)
   else DownHillDir := HeadingOfLine(XUTM2-XUTM1,YUTM2-YUTM1);
   h := sqrt( sqr(XUTM2-XUTM1) + sqr(YUTM2-YUTM1) );
   v := abs(Elv1 - Elv2);
   if Dip = 0 then Thick := v
   else if Dip = 90 then Thick := h
   else begin
      if abs(DownHillDir - DipDir) > 90 then
         {bedding and topography dip in opposite directions}
         Thick := h * sinDeg(Dip) + v * cosDeg(Dip)
      else
         {bedding and topography dip in the same direction}
         if ArcTan(v/h) > Dip then
            {topography dips more steeply than stratigraphy}
            Thick := v * cosDeg(Dip) - h * sinDeg(Dip)
         else
            {bedding dips more steeply than topography}
            Thick := h * sinDeg(Dip) - v * cosDeg(Dip);
   end {if Dip};
end;


procedure ApparentDip(Dip,Strike : integer; Azimuth : integer; var AppDip : float32);
   {Dip = true dip; strike = strike; Azimuth = heading of cross section; and AppDip = apparent dip in plane of section}
begin
   AppDip := abs( ArcTan( sinDeg( abs(Azimuth - Strike) ) * TanDeg(Dip) ) / DegToRad);
end;


initialization
finalization

end {unit}.


