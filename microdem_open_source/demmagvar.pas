unit DEMmagvar;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordMagVarProblems}
{$EndIf}


interface

uses
   Math,Petmar_types;

procedure InitializeMagneticVariation(NextYear : boolean = false);
procedure MagVr1(ALT,GLAT,GLON,TIME : double; var DEC,DIP,TI,GV : float64);
function CurrentMagneticDeclination(Lat,Long : float64) : float64;

const
   MaxDeg = 12;
type
   tMagModel = record
      MagEpoch,a,b,re,a2,b2,c2,a4,b4,c4,FLNMJ,
      GNM,HNM,DGNM,DHNM : extended;
      c,cd,tc,p,dp,snorm,k : array[0..MaxDeg,0..MaxDeg] of extended;
      sp,cp,fn,fm,pp : array[0..MaxDeg] of extended;
   end;

var
   MagModel : ^tMagModel;
   CurMagYear : extended;


implementation


uses
   PETMAR,PetMath,SysUtils,DEMDefs;

const
   DTR = 0.01745329252;  {degree to radians}

function CurrentMagneticDeclination(Lat,Long : float64) : float64;
var
   DIP,TI,GV : float64;
begin
   MagVr1(0,Lat,Long,CurMagYear,Result,DIP,TI,GV);
end;


procedure InitializeMagneticVariation(NextYear : boolean = false);
var
   N,M,j : integer;
   wmmfile : textFile;
   wMonth,wDay,wYear : word;
begin
   {$If Defined( MessageStartUpUnitProblems) or Defined (RecordInitialization)} MessageToContinue('InitializeMagneticVariation in'); {$EndIf}
   MagModel := Nil;

   if FileExists(www_mag_mod_fName) then begin
      DecodeDate(now,wYear,wmonth,wDay);
      CurMagYear  :=  Wyear + (JulDay(wMonth,wDay,wYear) - JulDay(1,1,wYear)) / 365;
      if NextYear then CurMagYear := CurMagYear + 1;
      
      new(MagModel);
      with MagModel^ do begin
         assignFile(wmmfile,www_mag_mod_fName);
         reset(wmmfile);
         SP[0] := 0;
         CP[0] := 1;
         P[0,0] := 1;
         PP[0] := 1;
         DP[0,0] := 0;
         A := 6378.137;
         B := 6356.7523142;
         RE := 6371.2;
         A2 := A*a;
         B2 := B*b;
         C2 := A2-B2;
         A4 := A2*a2;
         B4 := B2*b2;
         C4 := A4-B4;

         C[0,0] := 0.0;
         CD[0,0] := 0.0;

         readln(wmmfile,MagEpoch);
         repeat
            readln(wmmfile,N,M,GNM,HNM,DGNM,DHNM);
            C[N,M] := GNM;
            CD[N,M] := DGNM;
            IF (M <> 0) THEN begin
               C[M-1,N] := HNM;
               CD[M-1,N] := DHNM;
            end;
         until (n=12) and (m =12);

      SNORM[0,0] := 1;
      for N  := 1 to MAXDEG do begin
         SNORM[N,0] := SNORM[N-1,0]*(2*N-1)/(N);
         J := 2;
         for M := 0 to N do begin
             K[N,M] := (sqr(N-1)-sqr(M))/((2*N-1)*(2*N-3));
             IF (M > 0) THEN begin
                 FLNMJ := ((N-M+1)*J)/ (N+M);
                 SNORM[N,M] := SNORM[N,M-1]*SQRT(FLNMJ);
                 J := 1;
                 C[M-1,N] := SNORM[N,M]*C[M-1,N];
                 CD[M-1,N] := SNORM[N,M]*CD[M-1,N];
             END {IF};
             C[N,M] := SNORM[N,M]*C[N,M];
             CD[n,M] := SNORM[N,M]*CD[N,M];
         end {for m};
         FN[N] := (N+1);
         FM[N] := (N);
      end {for n};
      K[1,1] := 0;
      end;
   end
   else begin
      {$IfDef RecordProblems} WriteLineToDebugFile('Missing ' + www_mag_mod_fName); {$EndIf}
   end;
end;


procedure MagVr1;
var
   q,q1,q2,ct,st,r2,r,d,ca,sa,par,parp,
   aor,ar,br,bt,bp,bpp,temp1,temp2,bx,by,bz,bh,
   rlon,rlat,srlon,srlat,crlon,crlat,
   srlon2,srlat2,crlon2,crlat2,DT : extended;
   m,n : integer;
begin
   if (MagModel = Nil) or (abs(Glat) > 90) or (abs(Glon) > 180) then exit;
   with MagModel^ do begin
      DT := TIME - MagEpoch;
      ALT := 0.001 * ALT;
      RLON  := GLON*DTR;
      RLAT  := GLAT*DTR;
      SRLON  := SIN(RLON);
      SRLAT  := SIN(RLAT);
      CRLON  := COS(RLON);
      CRLAT  := COS(RLAT);
      SRLON2  := sqr(SRLON);
      SRLAT2  := sqr(SRLAT);
      CRLON2  := sqr(CRLON);
      CRLAT2  := sqr(CRLAT);
      SP[1]  :=  SRLON;
      CP[1]  :=  CRLON;

//        CONVERT FROM GEODETIC COORDS. TO SPHERICAL COORDS.

      Q  := SQRT(A2-C2*SRLAT2);
      Q1  := ALT*Q;
      Q2  := sqr((Q1+A2)/(Q1+B2));
      CT := SRLAT/SQRT(Q2*CRLAT2+SRLAT2);
      ST := SQRT(1.0-sqr(CT));
      R2 := sqr(ALT)+2.0*Q1+(A4-C4*SRLAT2)/sqr(Q);
      R := SQRT(R2);
      D := SQRT(A2*CRLAT2+B2*SRLAT2);
      CA := (ALT+D)/R ;
      SA := C2*CRLAT*SRLAT/(R*D);

      for M := 2 to MAXDeg do begin
          SP[M] := SP[1]*CP[M-1]+CP[1]*SP[M-1];
          CP[M] := CP[1]*CP[M-1]-SP[1]*SP[M-1];
      end {for M};

      AOR := RE/R;
      AR := sqr(AOR);

      BR := 0;
      BT := 0;
      BP := 0;
      BPP := 0;

      for N  :=  1 to MaxDeg do begin
         AR := AR*AOR;
         for M := 0 to N do begin

//        COMPUTE UNNORMALIZED ASSOCIATED LEGENDRE POLYNOMIALS
//        AND DERIVATIVES VIA RECURSION RELATIONS

         IF (N = M) THEN begin
            P[N,M] := ST*P[N-1,M-1];
            DP[N,M] := ST*DP[N-1,M-1]+CT*P[N-1,M-1];
         end
         else IF (N = 1) and  (M = 0) THEN begin
            P[N,M] := CT*P[N-1,M];
            DP[N,M] := CT*DP[N-1,M]-ST*P[N-1,M];
         END
         else IF (N > 1) and ( N <> M) THEN begin
            IF (M > N-2) then P[N-2,M]  := 0.0;
            IF (M > N-2) then DP[N-2,M] := 0.0;
            P[N,M] := CT*P[N-1,M]-K[N,M]*P[N-2,M];
            DP[N,M] := CT*DP[N-1,M]-ST*P[N-1,M]-K[N,M]*DP[N-2,M];
         END {if};

//        TIME ADJUST THE GAUSS COEFFICIENTS

         TC[N,M] := C[N,M]+DT*CD[N,M];
         IF (M <> 0) THEN TC[M-1,N] := C[M-1,N]+DT*CD[M-1,N];

//        ACCUMULATE TERMS OF THE SPHERICAL HARMONIC EXPANSIONS

         PAR := AR*P[N,M];
         IF (M = 0) THEN begin
           TEMP1 := TC[N,M]*CP[M];
           TEMP2 := TC[N,M]*SP[M];
         end
         ELSE begin
           TEMP1 := TC[n,M]*CP[M]+TC[m-1,N]*SP[M];
           TEMP2 := TC[n,M]*SP[M]-TC[M-1,N]*CP[M];
        END {IF};
        BT := BT-AR*TEMP1*DP[N,M];
        BP := BP+FM[M]*TEMP2*PAR;
        BR := BR+FN[N]*TEMP1*PAR;

//        SPECIAL CASE:  NORTH/SOUTH GEOGRAPHIC POLES

       IF (ST = 0) AND (M = 1) THEN BEGIN
          IF (N = 1) THEN PP[N] := PP[N-1]
          ELSE PP[N] := CT*PP[N-1]-K[N,M]*PP[N-2];
          PARP := AR*PP[N];
          BPP := BPP+FM[M]*TEMP2*PARP;
       END {if};
      end {for m};
   end {for n};

      IF (ST = 0) then BP := BPP
      ELSE BP := BP/ST;

//        ROTATE MAGNETIC VECTOR COMPONENTS FROM SPHERICAL TO
//        GEODETIC COORDINATES

      BX := -BT*CA-BR*SA;
      BY := BP;
      BZ := BT*SA-BR*CA;


//        COMPUTE DECLINATION (DEC), INCLINATION (DIP) AND
//        TOTAL INTENSITY (TI)
      BH := SQRT(BX*BX+BY*BY);
      TI := SQRT(BH*BH+BZ*BZ);
      DEC := ATAN2(BY,BX)/DTR;
      if (Dec > 180) then Dec := Dec - 360;

      DIP := ATAN2(BZ,BH)/DTR;

//C        COMPUTE MAGNETIC GRID VARIATION IF THE CURRENT
//C        GEODETIC POSITION IS IN THE ARCTIC OR ANTARCTIC
//C        (I.E. GLAT > +55 DEGREES OR GLAT < -55 DEGREES)
//C        OTHERWISE, SET MAGNETIC GRID VARIATION TO -999.0

      GV := -999.0;
      IF (ABS(GLAT) > 55) THEN begin
          IF (GLAT > 0) and (GLON >= 0) then GV := DEC-GLON;
          IF (GLAT > 0) and (GLON < 0) then GV := DEC+ABS(GLON);
          IF (GLAT < 0) and (GLON >= 0) then GV := DEC+GLON;
          IF (GLAT < 0) and (GLON < 0) then GV := DEC-ABS(GLON);
          IF (GV > +180) then GV := GV-360;
          IF (GV < -180) then GV := GV+360;
      END {if};
   end;
END;


initialization
   InitializeMagneticVariation;
finalization
   Dispose(MagModel);
   {$IfDef RecordMagVarProblems} WriteLineToDebugFile('RecordMagVarProblems active in demmagvar'); {$EndIf}
end.




