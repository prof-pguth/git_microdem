Unit Support;  (** This unit contains machine-specific code **)
{           Author:  Dr TS Kelso }
{ Original Version:  1992 Jun 25 }
{ Current Revision:  1992 Sep 14 }
{          Version:  1.81 }
{        Copyright:  1992, All Rights Reserved }
{$N+}

INTERFACE


type
  options  = array [0..10] of string;
  time_set = record
       yr,mo,dy,hr,mi,se,hu : word;
  end; {record}

Procedure Zero_Time(var time : time_set);
Function TwoDigit(arg : integer) : string;
Function ThreeDigit(arg : integer) : string;
Procedure Convert_Blanks(var field : string);
Function Integer_Value(buffer : string; start,length : integer) : integer;
Function Real_Value(buffer : string; start,length : integer) : double;



IMPLEMENTATION

uses
   Math;



Procedure Zero_Time(var time : time_set);
  begin
  with time do begin
    yr := 0;
    mo := 0;
    dy := 0;
    hr := 0;
    mi := 0;
    se := 0;
    hu := 0;
    end; {with}
  end; {Procedure Zero_Time}


Function TwoDigit(arg : integer) : string;
  begin
  TwoDigit := Chr((arg div 10) + Ord('0'))
            + Chr((arg mod 10) + Ord('0'));
  end; {Function TwoDigit}


Function ThreeDigit(arg : integer) : string;
  var
    hundreds,barg : integer;
  begin
  hundreds := arg div 100;
  barg := arg - 100*hundreds;
  ThreeDigit := Chr(hundreds + Ord('0')) + TwoDigit(barg);
  end; {Function ThreeDigit}


Procedure Convert_Blanks(var field : string);
  var
    i : integer;
  begin
  for i := length(field) downto 1 do
    if field[i] = ' ' then
      field[i] := '0';
  end; {Procedure Convert_Blanks}

Function Integer_Value(buffer : string; start,length : integer) : integer;
  var
    answer,result0 : integer;
  begin
  buffer := Copy(buffer,start,length);
  Convert_Blanks(buffer);
  Val(buffer,answer,result0);
  if result0 = 0 then
    Integer_Value := answer
  else
    Integer_Value := 0;
  end; {Function Integer_Value}


Function Real_Value(buffer : string; start,length : integer) : double;
  var
    result0 : integer;
    answer : double;
  begin
  buffer := Copy(buffer,start,length);
  Convert_Blanks(buffer);
  if buffer = '' then
    buffer := '0';
  Val(buffer,answer,result0);
  if result0 = 0 then
    Real_Value := answer
  else
    Real_Value := 0.0;
  end; {Function Real_Value}



end.
