unit DEMmagvar;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
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


(*
C***********************************************************************
C     SUBROUTINE MAGVAR (GEOMAGNETIC FIELD COMPUTATION)
C
C     MAGVAR IS A DEFENSE MAPPING AGENCY (DMA) STANDARD PRODUCT.	IT IS
C     COVERED UNDER DMA MILITARY SPECIFICATION: MIL-W-89500 (1993).
C     FOR INFORMATION ON THE USE AND APPLICABILITY OF THIS PRODUCT,
C     CONTACT DMA AT THE FOLLOWING ADDRESS:
C
C                     DIRECTOR
C                     DEFENSE MAPPING AGENCY/HEADQUARTERS
C                     ATTN: CODE PR
C                     8613 LEE HIGHWAY
C                     FAIRFAX, VA 22031-2137
C
C
C***********************************************************************
C
C     PROGRAMMED BY:  JOHN M. QUINN  7/19/90
C		      FLEET PRODUCTS DIVISION, CODE N342
C		      NAVAL OCEANOGRAPHIC OFFICE  (NAVOCEANO)
C                     STENNIS SPACE CENTER (SSC), MS
C		      PHONE:   COM:  (601) 688-5828
C			       DSN:	   485-5828
C			       FAX:  (601) 688-5221
C
C***********************************************************************
C
C     PURPOSE:  THIS ROUTINE COMPUTES THE DECLINATION (DEC),
C               INCLINATION (DIP), TOTAL INTENSITY (TI) AND
C		GRID VARIATION (GV - POLAR REGIONS ONLY, REFERENCED
C		TO GRID NORTH OF A STEREOGRAPHIC PROJECTION) OF THE
C               EARTH'S MAGNETIC FIELD IN GEODETIC COORDINATES
C               FROM THE COEFFICIENTS OF THE CURRENT OFFICIAL
C               DEPARTMENT OF DEFENSE (DOD) SPHERICAL HARMONIC WORLD
C		MAGNETIC MODEL (WMM-95).  THE WMM SERIES OF MODELS IS
C               UPDATED EVERY 5 YEARS ON JANUARY 1'ST OF THOSE YEARS
C               WHICH ARE DIVISIBLE BY 5 (I.E. 1980, 1985, 1990 ETC.)
C               BY THE U.S. NAVAL OCEANOGRAPHIC OFFICE IN COOPERATION
C		WITH THE BRITISH GEOLOGICAL SURVEY (BGS).  THE MODEL
C               IS BASED ON GEOMAGNETIC SURVEY MEASUREMENTS FROM
C               AIRCRAFT, SATELLITE AND GEOMAGNETIC OBSERVATORIES.
C
C***********************************************************************
C
C     MODEL:  THE WMM SERIES GEOMAGNETIC MODELS ARE COMPOSED
C             OF TWO PARTS:  THE MAIN FIELD MODEL, WHICH IS
C             VALID AT THE BASE EPOCH OF THE CURRENT MODEL AND
C             A SECULAR VARIATION MODEL, WHICH ACCOUNTS FOR SLOW
C             TEMPORAL VARIATIONS IN THE MAIN GEOMAGNETIC FIELD
C             FROM THE BASE EPOCH TO A MAXIMUM OF 5 YEARS BEYOND
C             THE BASE EPOCH.  FOR EXAMPLE, THE BASE EPOCH OF
C	      THE WMM-95 MODEL IS 1995.0.  THIS MODEL IS THEREFORE
C	      CONSIDERED VALID BETWEEN 1995.0 AND 2000.0. THE
C             COMPUTED MAGNETIC PARAMETERS ARE REFERENCED TO THE
C             WGS-84 ELLIPSOID.
C
C***********************************************************************
C
C     ACCURACY:  IN OCEAN AREAS AT THE EARTH'S SURFACE OVER THE
C                ENTIRE 5 YEAR LIFE OF A DEGREE AND ORDER 12
C		 SPHERICAL HARMONIC MODEL SUCH AS WMM-95, THE ESTIMATED
C		 RMS ERRORS FOR THE VARIOUS MAGNETIC COMPONENTS ARE:
C
C		 DEC  -   0.5 Degrees
C		 DIP  -   0.5 Degrees
C		 TI   - 280.0 nanoTeslas (nT)
C		 GV   -   0.5 Degrees
C
C		 OTHER MAGNETIC COMPONENTS THAT CAN BE DERIVED FROM
C		 THESE FOUR BY SIMPLE TRIGONOMETRIC RELATIONS WILL
C		 HAVE THE FOLLOWING APPROXIMATE ERRORS OVER OCEAN AREAS:
C
C		 X    - 140 nT (North)
C		 Y    - 140 nT (East)
C		 Z    - 200 nT (Vertical) Positive is down
C		 H    - 200 nT (Horizontal)
C
C		 OVER LAND THE RMS ERRORS ARE EXPECTED TO BE SOMEWHAT
C		 HIGHER, ALTHOUGH THE RMS ERRORS FOR DEC, DIP, AND GV
C		 ARE STILL ESTIMATED TO BE LESS THAN 1.0 DEGREE, FOR
C		 THE ENTIRE 5-YEAR LIFE OF THE MODEL AT THE EARTH's
C		 SURFACE.  THE OTHER COMPONENT ERRORS OVER LAND ARE
C		 MORE DIFFICULT TO ESTIMATE AND SO ARE NOT GIVEN.
C
C		 THE ACCURACY AT ANY GIVEN TIME FOR ALL OF THESE
C                GEOMAGNETIC PARAMETERS DEPENDS ON THE GEOMAGNETIC
C                LATITUDE.  THE ERRORS ARE LEAST AT THE EQUATOR AND
C                GREATEST AT THE MAGNETIC POLES.
C
C                IT IS VERY IMPORTANT TO NOTE THAT A DEGREE AND
C		 ORDER 12 MODEL, SUCH AS WMM-95, DESCRIBES ONLY
C                THE LONG WAVELENGTH SPATIAL MAGNETIC FLUCTUATIONS
C                DUE TO EARTH'S CORE.  NOT INCLUDED IN THE WMM SERIES
C                MODELS ARE INTERMEDIATE AND SHORT WAVELENGTH
C                SPATIAL FLUCTUATIONS OF THE GEOMAGNETIC FIELD
C                WHICH ORIGINATE IN THE EARTH'S MANTLE AND CRUST.
C                CONSEQUENTLY, ISOLATED ANGULAR ERRORS AT VARIOUS
C                POSITIONS ON THE SURFACE (PRIMARILY OVER LAND, IN
C                CONTINENTAL MARGINS AND OVER OCEANIC SEAMOUNTS,
C                RIDGES AND TRENCHES) OF SEVERAL DEGREES MAY BE
C                EXPECTED. ALSO NOT INCLUDED IN THE MODEL ARE
C                NONSECULAR TEMPORAL FLUCTUATIONS OF THE GEOMAGNETIC
C                FIELD OF MAGNETOSPHERIC AND IONOSPHERIC ORIGIN.
C                DURING MAGNETIC STORMS, TEMPORAL FLUCTUATIONS CAN
C                CAUSE SUBSTANTIAL DEVIATIONS OF THE GEOMAGNETIC
C                FIELD FROM MODEL VALUES.  IN ARCTIC AND ANTARCTIC
C                REGIONS, AS WELL AS IN EQUATORIAL REGIONS, DEVIATIONS
C                FROM MODEL VALUES ARE BOTH FREQUENT AND PERSISTENT.
C
C                IF THE REQUIRED DECLINATION ACCURACY IS MORE
C                STRINGENT THAN THE WMM SERIES OF MODELS PROVIDE, THEN
C                THE USER IS ADVISED TO REQUEST SPECIAL (REGIONAL OR
C                LOCAL) SURVEYS BE PERFORMED AND MODELS PREPARED BY
C                NAVOCEANO, WHICH OPERATES THE PROJECT MAGNET
C                AIRCRAFT AND THE POLAR ORBITING GEOMAGNETIC SURVEY
C                (POGS) SATELLITE.  REQUESTS OF THIS NATURE SHOULD
C                BE MADE TO DMA, CODE PR, AT THE ADDRESS ABOVE.
C
C***********************************************************************
C
C     USAGE:  THIS ROUTINE IS BROKEN UP INTO TWO PARTS:
C
C             A) AN INITIALIZATION MODULE, WHICH IS CALLED ONLY
C                ONCE AT THE BEGINNING OF THE MAIN (CALLING)
C                PROGRAM
C             B) A PROCESSING MODULE, WHICH COMPUTES THE MAGNETIC
C                FIELD PARAMETERS FOR EACH SPECIFIED GEODETIC
C                POSITION (ALTITUDE, LATITUDE, LONGITUDE) AND TIME
C
C**********************************************************************
C
C     REFERENCES:
C
C       JOHN M. QUINN, DAVID J. KERRIDGE AND DAVID R. BARRACLOUGH,
C            WORLD MAGNETIC CHARTS FOR 1985 - SPHERICAL HARMONIC
C            MODELS OF THE GEOMAGNETIC FIELD AND ITS SECULAR
C            VARIATION, GEOPHYS. J. R. ASTR. SOC. (1986) 87,
C            PP 1143-1157
C
C       DEFENSE MAPPING AGENCY TECHNICAL REPORT, TR 8350.2:
C            DEPARTMENT OF DEFENSE WORLD GEODETIC SYSTEM 1984,
C	     2nd ed. (1991)
C
C	JOSEPH C. CAIN, ET AL.; A PROPOSED MODEL FOR THE
C            INTERNATIONAL GEOMAGNETIC REFERENCE FIELD - 1965,
C            J. GEOMAG. AND GEOELECT. VOL. 19, NO. 4, PP 335-355
C            (1967) (SEE APPENDIX)
C
C       ALFRED J. ZMUDA, WORLD MAGNETIC SURVEY 1957-1969,
C            INTERNATIONAL ASSOCIATION OF GEOMAGNETISM AND
C	     AERONOMY (IAGA) BULLETIN #28, PP 186-188 (1971)
C
C	JOHN M. QUINN, RACHEL J. COLEMAN, MICHAEL R. PECK, AND
C	     STEPHEN E. LAUBER; THE JOINT US/UK 1990 EPOCH
C	     WORLD MAGNETIC MODEL, TECHNICAL REPORT NO. 304,
C	     NAVAL OCEANOGRAPHIC OFFICE (1991)
C
C	JOHN M. QUINN, RACHEL J. COLEMAN, DONALD L. SHIEL, AND
C	     JOHN M. NIGRO; THE JOINT US/UK 1995 EPOCH WORLD
C	     MAGNETIC MODEL, TECHNICAL REPORT NO. 314, NAVAL
C	     OCEANOGRAPHIC OFFICE (1995)
C
C***********************************************************************
C
C     PARAMETER DESCRIPTIONS:
C
C       A      - SEMIMAJOR AXIS OF WGS-84 ELLIPSOID (KM)
C       B      - SEMIMINOR AXIS OF WGS-84 ELLIPSOID (KM)
C       RE     - MEAN RADIUS OF IAU-66 ELLIPSOID (KM)
C       SNORM  - SCHMIDT NORMALIZATION FACTORS
C       C      - GAUSS COEFFICIENTS OF MAIN GEOMAGNETIC MODEL (NT)
C       CD     - GAUSS COEFFICIENTS OF SECULAR GEOMAGNETIC MODEL (NT/YR)
C       TC     - TIME ADJUSTED GEOMAGNETIC GAUSS COEFFICIENTS (NT)
C       OTIME  - TIME ON PREVIOUS CALL TO GEOMAG (YRS)
C       OALT   - GEODETIC ALTITUDE ON PREVIOUS CALL TO GEOMAG (YRS)
C       OLAT   - GEODETIC LATITUDE ON PREVIOUS CALL TO GEOMAG (DEG.)
C       OLON   - GEODETIC LONGITUDE ON PREVIOUS CALL TO GEOMAG (DEG.)
C       TIME   - COMPUTATION TIME (YRS)                        (INPUT)
C		 (EG. 1 JULY 1995 = 1995.500)
C       ALT    - GEODETIC ALTITUDE (KM)                        (INPUT)
C       GLAT   - GEODETIC LATITUDE (DEG.)                      (INPUT)
C       GLON   - GEODETIC LONGITUDE (DEG.)                     (INPUT)
C       EPOCH  - BASE TIME OF GEOMAGNETIC MODEL (YRS)
C       DTR    - DEGREE TO RADIAN CONVERSION
C       SP(M)  - SINE OF (M*SPHERICAL COORD. LONGITUDE)
C       CP(M)  - COSINE OF (M*SPHERICAL COORD. LONGITUDE)
C       ST     - SINE OF (SPHERICAL COORD. LATITUDE)
C       CT     - COSINE OF (SPHERICAL COORD. LATITUDE)
C       R      - SPHERICAL COORDINATE RADIAL POSITION (KM)
C       CA     - COSINE OF SPHERICAL TO GEODETIC VECTOR ROTATION ANGLE
C       SA     - SINE OF SPHERICAL TO GEODETIC VECTOR ROTATION ANGLE
C       BR     - RADIAL COMPONENT OF GEOMAGNETIC FIELD (NT)
C       BT     - THETA COMPONENT OF GEOMAGNETIC FIELD (NT)
C       BP     - PHI COMPONENT OF GEOMAGNETIC FIELD (NT)
C       P(N,M) - ASSOCIATED LEGENDRE POLYNOMIALS (UNNORMALIZED)
C       PP(N)  - ASSOCIATED LEGENDRE POLYNOMIALS FOR M=1 (UNNORMALIZED)
C       DP(N,M)- THETA DERIVATIVE OF P(N,M) (UNNORMALIZED)
C       BX     - NORTH GEOMAGNETIC COMPONENT (NT)
C       BY     - EAST GEOMAGNETIC COMPONENT (NT)
C       BZ     - VERTICALLY DOWN GEOMAGNETIC COMPONENT (NT)
C       BH     - HORIZONTAL GEOMAGNETIC COMPONENT (NT)
C       DEC    - GEOMAGNETIC DECLINATION (DEG.)                (OUTPUT)
C                  EAST=POSITIVE ANGLES
C                  WEST=NEGATIVE ANGLES
C       DIP    - GEOMAGNETIC INCLINATION (DEG.)                (OUTPUT)
C                  DOWN=POSITIVE ANGLES
C                    UP=NEGATIVE ANGLES
C       TI     - GEOMAGNETIC TOTAL INTENSITY (NT)              (OUTPUT)
C       GV     - GEOMAGNETIC GRID VARIATION (DEG.)             (OUTPUT)
C                REFERENCED TO GRID NORTH
C                GRID NORTH REFERENCED TO 0 MERIDIAN
C                OF A POLAR STEREOGRAPHIC PROJECTION
C                (ARCTIC/ANTARCTIC ONLY)
C       MAXDEG - MAXIMUM DEGREE OF SPHERICAL HARMONIC MODEL    (INPUT)
C       MOXORD - MAXIMUM ORDER OF SPHERICAL HARMONIC MODEL
C
C
C***********************************************************************
C
C
C     NOTE:  THIS VERSION OF MAGVAR USES THE WMM-95 GEOMAGNETIC
C            MODEL REFERENCED TO THE WGS-84 GRAVITY MODEL ELLIPSOID
C
C
C***********************************************************************
C
*)


procedure InitializeMagneticVariation(NextYear : boolean = false);
var
   N,M,j : integer;
   wmmfile : textFile;
   wMonth,wDay,wYear : word;
begin
   {$IfDef MessageStartUpUnitProblems} MessageToContinue('Startup demMagVar'); {$EndIf}
   {$IfDef RecordInitialization} WriteLineToDebugFile('Call InitializeMagneticVariation');   {$EndIf}
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
      {$IfDef RecordProblems} WriteLineToDebugFile('Missing ' + www_mag_mod_fName);  {$EndIf}
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




