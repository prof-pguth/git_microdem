Unit SGP_In;  (** This unit contains machine-specific code **)
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Jun 25 }
{ Current Revision:  1999 Nov 27 }
{          Version:  2.10 }
{        Copyright:  1992-1999, All Rights Reserved }


//constants from the original sgn_intf.pas and sgn_init.pas added here, PLG, August 2010
{$N+}

INTERFACE

Uses
   SGP_Math,
   //SGP_Init,
   Support;

const
  data_type : byte = 3;

var
  fsat,fobs : text;

const
  max_sats = 250;

type
  line_data = string[69];
  two_line  = array [1..2] of line_data;

var
  visible              : boolean;
  epoch                : double;
  catnr,elset          : string;
  obs_name             : string[25];
  selected             : array [1..max_sats] of boolean;
  sat_name             : array [1..max_sats] of string[24];
  sat_data             : array [1..max_sats] of two_line;
  data_drive,data_dir,
  work_drive,work_dir  : string;
  UTC_offset           : double;
  DST                  : boolean;

const
  ae       = 1;
  tothrd   = 2/3;
  xkmper   = 6378.135;        {Earth equatorial radius - kilometers (WGS '72)}
  f        = 1/298.26;        {Earth flattening (WGS '72)}
  ge       = 398600.8;        {Earth gravitational constant (WGS '72)}
  J2       = 1.0826158E-3;    {J2 harmonic (WGS '72)}
  J3       = -2.53881E-6;     {J3 harmonic (WGS '72)}
  J4       = -1.65597E-6;     {J4 harmonic (WGS '72)}
  ck2      = J2/2;
  ck4      = -3*J4/8;
  xj3      = J3;
  qo       = ae + 120/xkmper;
  s        = ae + 78/xkmper;
  e6a      = 1E-6;
  dpinit   = 1;               {Deep-space initialization code}
  dpsec    = 2;               {Deep-space secular code}
  dpper    = 3;               {Deep-space periodic code}

var
  iflag,ideep                : integer;
  xmo,xnodeo,omegao,eo,xincl,
  xno,xndt2o,xndd6o,bstar,
  julian_epoch,xke           : double;



//Function Checksum_Good(line : line_data) : boolean;
//Function Good_Elements(line : two_line) : boolean;
//Procedure Input_Satellite(index : word);
Function Input_Satellite_Data(fn : string) : word;
//Procedure Select_Satellites(title : string; x,y,w,h : byte; number : word);
//Procedure Input_Observer(var geodetic : vector);


IMPLEMENTATION

  Uses Math;

var
  i : byte;


Function Checksum_Good(line : line_data) : boolean;
  var
    i,checksum,check_digit : integer;
  begin
  checksum := 0;
  for i := 1 to 68 do
    case line[i] of
      '0'..'9' : checksum := checksum + Ord(line[i]) - Ord('0');
           '-' : checksum := checksum + 1;
      end; {case}
  checksum := checksum mod 10;
  check_digit := Ord(line[69]) - Ord('0');
  Checksum_Good := (checksum = check_digit);
  end; {Function Checksums_Good}


Function Good_Elements(line : two_line) : boolean;
  var
    result0 : boolean;
  begin
  result := Checksum_Good(line[1]) and Checksum_Good(line[2]);
  if (line[1,1] <> '1') or
     (line[2,1] <> '2') or
     (Copy(line[1],3,5) <> Copy(line[2],3,5)) then
    result0 := false;
  if (line[1,24] <> '.') or
     (line[1,35] <> '.') or
     (Copy(line[1],62,3) <> ' 0 ') or
     (line[2,12] <> '.') or
     (line[2,21] <> '.') or
     (line[2,38] <> '.') or
     (line[2,47] <> '.') or
     (line[2,55] <> '.') then
    result0 := false;
  Good_Elements := result0;
  end; {Function Good_Elements}


Procedure Input_Satellite(index : word);
  begin
  if not EOF(fsat) then begin
    if data_type = 3 then Readln(fsat,sat_name[index]);
    Readln(fsat,sat_data[index,1]);
    Readln(fsat,sat_data[index,2]);
    end; {if}
  end; {Procedure Input_Satellite}



Function Input_Satellite_Data(fn : string) : word;
  var
    count : word;
  begin
  if data_type in [2,3] then begin
    count := 0;
    Assign(fsat,data_drive + data_dir + fn);
    Reset(fsat);
    repeat
      count := count + 1;
      Input_Satellite(count);
    until EOF(fsat) or (count = max_sats);
    Close(fsat);
    Input_Satellite_Data := count;
    end {if}
  else
    begin
//    GotoXY(1,24);
//    Writeln('Invalid data type!');
//    Halt;
    end; {else}
  end; {Procedure Input_Satellite_Data}



Procedure Input_Observer(var geodetic : vector);
  begin
  if not EOF(fobs) then  begin
    Readln(fobs,obs_name,geodetic[1],geodetic[2],geodetic[3]);
    geodetic[1] := Radians(geodetic[1]);
    geodetic[2] := Radians(Modulus(geodetic[2],360));
    geodetic[3] := geodetic[3]*0.001;
    end; {if}
  end; {Procedure Input_Observer}



begin
  for i := 1 to max_sats do selected[i] := false;

end.
