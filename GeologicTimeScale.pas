unit GeologicTimeScale;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
   Petmar_types,Petmar_db,PETMAR,PETMath;

{$IfDef ExGeology}
{$Else}
function NormalMagneticTime(Age : float64) : boolean;
{$EndIf}


const
   MaxChrons = 200;
var
  NumChrons : integer;
  StartTime,EndTime : array[1..MaxChrons] of float64;
  ChronName : array[1..MaxChrons] of shortstring;

implementation

uses
   DEMDefs,
   SysUtils;

{$IfDef ExGeology}
{$Else}
function NormalMagneticTime(Age : float64) : boolean;
var
   InTable : tMyData;
   i : integer;
begin
   result := false;
   if (NumChrons = 0) then begin
      if not FileExists(MagneticAnomalyTimeScale) then exit;
      InTable := tMyData.Create(MagneticAnomalyTimeScale);
      InTable.First;
      while (not InTable.Eof) do begin
         inc(NumChrons);
         StartTime[NumChrons] := Intable.GetFieldByNameAsFloat('TIME1');
         EndTime[NumChrons] := Intable.GetFieldByNameAsFloat('TIME2');
         ChronName[NumChrons] := Intable.GetFieldByNameAsString('NORMAL');
         InTable.Next;
      end;
      Intable.Destroy;
   end;
   age := abs(age);
   for i := 1 to NumChrons do begin
      if (Age >= StartTime[i]) and (Age <= EndTime[i]) then begin
         result := true;
         exit;
      end;
   end;
end;
{$EndIf}


initialization
   NumChrons := 0;
end.
