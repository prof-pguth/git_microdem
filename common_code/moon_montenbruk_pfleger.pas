unit moon_montenbruk_pfleger;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{ from Montenbruck, O., and Pfleger, T., 1991, Astronomy on the personal computer: Springer-Verlag, 255 p.}

interface



procedure MoonRise(stMonth,stDay,stYear,Duration : integer; Lat,Long : float64);
function DaysSinceFullMoon(stMonth,stDay,stYear : integer; Lat,Long : float64) : integer;


implementation

uses
   SysUtils,Classes,
   Petmar_types,PETMAR,PETMath,DEMDefs,PETDBUtils;

CONST
   SEC=3600.0;
   RAD=0.0174532925199433;
   ARC=206264.8062;          //arcseconds per radian = 3600*180/pi


(*-----------------------------------------------------------------------*)
(* Unit MATLIB : mathematical functions and subroutines                  *)
(*-----------------------------------------------------------------------*)

  (* vectors and matrices *)

  TYPE INDEX  = (X,Y,Z);
       VECTOR = array[INDEX] OF float64;
       REAL3  = array[1.. 3] OF float64;
       REAL33 = array[1.. 3] OF REAL3;
       MAT3X  = array[1.. 3] OF VECTOR;

  (* Chebyshev polynomials *)

  CONST MAX_TP_DEG  = 13;                        (* maximum degree       *)
  TYPE  TPOLYNOM = RECORD                        (* Chebyshev polynomial *)
                     M  : INTEGER;                       (* degree       *)
                     A,B: float64;                          (* interval     *)
                     C  : array [0..MAX_TP_DEG] OF float64; (* coefficients *)
                   end;

  (* Vector and matrix for least squares systems:                    *)
  (* minimum dimensions for use with program FOTO are given below;   *)
  (* they may be increased as needed by the user.                    *)

  TYPE  LSQVEC =  array[1..3] OF float64;
        LSQMAT =  array[1..30,1..5] OF float64;


(*-----------------------------------------------------------------------*)
(* ATN2: arctangent of y/x for two arguments                             *)
(*       (correct quadrant; -180 deg <= ATN2 <= +180 deg)                *)
(*-----------------------------------------------------------------------*)
FUNCTION ATN2(Y,X:float64):float64;
var
   AX,AY,PHI: float64;
begin
    if (X=0.0) AND (Y=0.0) then ATN2:=0.0
    else begin
          AX:=ABS(X); AY:=ABS(Y);
          if (AX>AY) then PHI:=ARCTAN(AY/AX)/RAD
          else PHI:=90.0-ARCTAN(AX/AY)/RAD;
          if (X<0.0) then PHI:=180.0-PHI;
          if (Y<0.0) then PHI:=-PHI;
          ATN2:=PHI;
     end;
end;


(*-----------------------------------------------------------------------*)
(* POLAR: conversion of cartesian coordinates (x,y,z)                    *)
(*        into polar coordinates (r,theta,phi)                           *)
(*        (theta in [-90 deg,+90 deg]; phi in [-180 deg,+180 deg])       *)
(*-----------------------------------------------------------------------*)
PROCEDURE POLAR(X,Y,Z:float64;VAR R,THETA,PHI:float64);
  VAR
     RHO: float64;
  begin
    RHO:=X*X+Y*Y;
    R:=SQRT(RHO+Z*Z);
    PHI:=ATN2(Y,X);
    if PHI<0 then PHI:=PHI+360.0;
    RHO:=SQRT(RHO);
    THETA:=ATN2(Z,RHO);
  end;


(*-----------------------------------------------------------------------*)
(* CART: conversion of polar coordinates (r,theta,phi)                   *)
(*       into cartesian coordinates (x,y,z)                              *)
(*       (theta in [-90 deg,+90 deg]; phi in [-360 deg,+360 deg])        *)
(*-----------------------------------------------------------------------*)
PROCEDURE CART(R,THETA,PHI: float64; VAR X,Y,Z: float64);
  VAR
     RCST : float64;
  begin
    RCST := R*CosDeg(THETA);
    X    := RCST*CosDeg(PHI);
    Y := RCST*SinDeg(PHI);
    Z := R*SinDeg(THETA)
  end;


(*-----------------------------------------------------------------------*)
(* LSQFIT:                                                               *)
(*   solution of an overdetermined system of linear equations            *)
(*   A[i,1]*s[1]+...A[i,m]*s[m] - A[i,m+1] = 0   (i=1,..,n)              *)
(*   according to the method of least squares using Givens rotations     *)
(*   A: matrix of coefficients                                           *)
(*   N: number of equations  (rows of A)                                 *)
(*   M: number of unknowns   (columns of A, elemnts of S)                *)
(*   S: solution vector                                                  *)
(*-----------------------------------------------------------------------*)
PROCEDURE LSQFIT ( A: LSQMAT; N, M: INTEGER; VAR S: LSQVEC );

  CONST EPS = 1.0E-10;  (* machine accuracy *)

  VAR I,J,K: INTEGER;
      P,Q,H: float64;

begin
     for J:=1 to M do  (* loop over columns 1...M of A *)

      (* eliminate matrix elements A[i,j] with i>j from column j *)

      for I:=J+1 to N do
        if A[I,J]<>0.0 then
          begin
            (* calculate p, q and new A[j,j]; set A[i,j]=0 *)
            if ( ABS(A[J,J])<EPS*ABS(A[I,J]) ) then begin
                  P:=0.0; Q:=1.0; A[J,J]:=-A[I,J]; A[I,J]:=0.0;
            end
            else begin
                  H:=SQRT(A[J,J]*A[J,J]+A[I,J]*A[I,J]);
                  if A[J,J]<0.0 then H:=-H;
                  P:=A[J,J]/H; Q:=-A[I,J]/H; A[J,J]:=H; A[I,J]:=0.0;
            end;
            (*  calculate rest of the line *)
            for K:=J+1 to M+1 do begin
                H      := P*A[J,K] - Q*A[I,K];
                A[I,K] := Q*A[J,K] + P*A[I,K];
                A[J,K] := H;
              end;
          end;

    (* backsubstitution *)

    for I:=M DOWNTO 1 do begin
        H:=A[I,M+1];
        for K:=I+1 to M do H:=H+A[I,K]*S[K];
        S[I] := -H/A[I,I];
      end;
end;  (* LSQFIT *)


(*-----------------------------------------------------------------------*)
(* QUAD: finds a parabola through 3 points                               *)
(*       (-1,Y_MINUS), (0,Y_0) und (1,Y_PLUS),                           *)
(*       that do not lie on a straight line.                             *)
(*                                                                       *)
(*      Y_MINUS,Y_0,Y_PLUS: three y-values                               *)
(*      XE,YE   : x and y of the extreme value of the parabola           *)
(*      ZERO1   : first root within [-1,+1] (for NZ=1,2)                 *)
(*      ZERO2   : second root within [-1,+1] (only for NZ=2)             *)
(*      NZ      : number of roots within the interval [-1,+1]            *)
(*-----------------------------------------------------------------------*)
PROCEDURE QUAD(Y_MINUS,Y_0,Y_PLUS: float64;
               VAR XE,YE,ZERO1,ZERO2: float64; VAR NZ: INTEGER);
  VAR A,B,C,DIS,DX: float64;
  begin
    NZ := 0;
    A  := 0.5*(Y_MINUS+Y_PLUS)-Y_0; B := 0.5*(Y_PLUS-Y_MINUS); C := Y_0;
    XE := -B/(2.0*A); YE := (A*XE + B) * XE + C;
    DIS := B*B - 4.0*A*C; (* discriminant of y = axx+bx+c *)
    if (DIS >= 0) then    (* parabola intersects x-axis   *)
      begin
        DX := 0.5*SQRT(DIS)/ABS(A); ZERO1 := XE-DX; ZERO2 := XE+DX;
        if (ABS(ZERO1) <= +1.0) then NZ := NZ + 1;
        if (ABS(ZERO2) <= +1.0) then NZ := NZ + 1;
        if (ZERO1<-1.0) then ZERO1:=ZERO2;
      end;
    end;

(*-----------------------------------------------------------------------*)
(* T_EVAL: evaluates the approximation of a function by Chebyshev        *)
(*         polynomials of maximum order F.M over the interval [F.A,F.B]  *)
(*  F : record containing the Chebyshev coefficients                     *)
(*  X : argument                                                         *)
(*-----------------------------------------------------------------------*)
 FUNCTION T_EVAL(F: TPOLYNOM; X: float64): float64;
  VAR F1,F2,OLD_F1,XX,XX2 : float64;
      I                : INTEGER;
  begin
    if ( (X<F.A) or (F.B<X) ) then
      begin WRITELN(' T_EVAL : x not within [a,b]'); end;
    F1 := 0.0;  F2 := 0.0;
    XX := (2.0*X-F.A-F.B)/(F.B-F.A);  XX2 := 2.0*XX;
    for I := F.M DOWNTO 1 do  begin
       OLD_F1 := F1;
       F1 := XX2*F1-F2+F.C[I];
       F2 := OLD_F1;
    end;
    T_EVAL := XX*F1-F2+0.5*F.C[0]
  end;


type
   tPosProc = PROCEDURE(T:float64; VAR LL,BB,RR: float64);

(*-----------------------------------------------------------------------*)
(* T_FIT_LBR: expands lunar or planetary coordinates into series of      *)
(*            Chebyshev polynomials for longitude, latitude and radius   *)
(*            that are valid for a specified period of time              *)
(*                                                                       *)
(*  POSITION: routine for calculating the coordinates L,B,R              *)
(*  TA      : first date of desired period of time                       *)
(*  TB      : last date                                                  *)
(*  N       : highest order of Chebyshev polynomials (N<=MAX_TP_DEG)     *)
(*  L_POLY  : coefficients for longitude                                 *)
(*  B_POLY  : coefficients for latitude                                  *)
(*  R_POLY  : coefficients for radius                                    *)
(*                                                                       *)
(* note:                                                                 *)
(*  . the interval [TA,TB] must be shorter than one revolution!          *)
(*  . the routine will only work for heliocentric planetary or           *)
(*    geocentric lunar but not for geocentric planetary coordinates!     *)
(*-----------------------------------------------------------------------*)
PROCEDURE T_FIT_LBR ( PosProc : tPosProc;
                      TA,TB: float64; N: INTEGER;
                      VAR L_POLY,B_POLY,R_POLY: TPOLYNOM);
  CONST
        NDIM = 27;
  VAR   I,J,K          : INTEGER;
        FAC,BPA,BMA,PHI: float64;
        T,H,L,B,R      : array[0..NDIM] OF float64;
  begin
    if (NDIM<2*MAX_TP_DEG+1) then WRITELN(' NDIM too small in T_FIT_LBR');
    if (N>MAX_TP_DEG) then WRITELN(' N too large in T_FIT_LBR');
    L_POLY.M := N;    B_POLY.M := N;    R_POLY.M := N;
    L_POLY.A := TA;   B_POLY.A := TA;   R_POLY.A := TA;
    L_POLY.B := TB;   B_POLY.B := TB;   R_POLY.B := TB;
    BMA := (TB-TA)/2.0;   BPA := (TB+TA)/2.0;
    FAC := 2.0/(N+1);
    PHI:=PI/(2*N+2);                              (* h(k)=cos(pi*k/N/2)  *)
    H[0]:=1.0; H[1]:=COS(PHI);
    for I:=2 to (2*N+1) do H[I]:=2*H[1]*H[I-1]-H[I-2];
    for K:=1 to N+1 do T[K] := H[2*K-1]*BMA+BPA;  (* subdivison points   *)
    for K:=1 to N+1 do PosProc(T[K],L[K],B[K],R[K]);
    for K := 2 to N+1 do                          (* make L continuous   *)
      if (L[K-1]<L[K]) then L[K]:=L[K]-360.0;     (* in [-360,+360] !    *)
    for J := 0 to N do                            (* calculate Chebyshev *)
      begin                                       (* coefficients C(j)   *)
        PHI:=PI*J/(2*N+2); H[1]:=COS(PHI);
        for I:=2 to (2*N+1) do H[I] := 2*H[1]*H[I-1]-H[I-2];
        L_POLY.C[J]:=0.0; B_POLY.C[J]:=0.0; R_POLY.C[J]:=0.0;
        for K:=1 to N+1 do
          begin
            L_POLY.C[J] := L_POLY.C[J] + H[2*K-1]*L[K];
            B_POLY.C[J] := B_POLY.C[J] + H[2*K-1]*B[K];
            R_POLY.C[J] := R_POLY.C[J] + H[2*K-1]*R[K];
          end;
        L_POLY.C[J]:=L_POLY.C[J]*FAC; B_POLY.C[J]:=B_POLY.C[J]*FAC;
        R_POLY.C[J]:=R_POLY.C[J]*FAC;
      end;
  end;

(*-----------------------------------------------------------------------*)
(* TN: tangent function (degrees)                                        *)
(*-----------------------------------------------------------------------*)
FUNCTION TN(X : float64) : float64;
  VAR XX: float64;
  begin
    XX:=X*RAD;
    TN:=SIN(XX)/COS(XX);
  end;


(*-----------------------------------------------------------------------*)
(* Unit PNULIB: precession and nutation                                  *)
(*-----------------------------------------------------------------------*)


(*-----------------------------------------------------------------------*)
(* NUTEQU: transformation of mean to true coordinates                    *)
(*         (including terms >0.1" according to IAU 1980)                 *)
(*         T = (JD-2451545.0)/36525.0                                    *)
(*-----------------------------------------------------------------------*)
PROCEDURE NUTEQU(T:float64;VAR X,Y,Z:float64);
  VAR   LS,D,F,N,EPS : float64;
        DPSI,DEPS,C,S: float64;
        DX,DY,DZ     : float64;

  FUNCTION FRAC(X:float64):float64;
    (* with several compilers it may be necessary to replace TRUNC *)
    (* by LONG_TRUNC or INT if T<-24!                              *)
    begin
       FRAC:=X-TRUNC(X);
    end;

  begin
    LS  := TwoPi*FRAC(0.993133+  99.997306*T); (* mean anomaly Sun          *)
    D   := TwoPi*FRAC(0.827362+1236.853087*T); (* diff. longitude Moon-Sun  *)
    F   := TwoPi*FRAC(0.259089+1342.227826*T); (* mean argument of latitude *)
    N   := TwoPi*FRAC(0.347346-   5.372447*T); (* longit. ascending node    *)
    EPS := 0.4090928-2.2696E-4*T;           (* obliquity of the ecliptic *)
    DPSI := ( -17.200*SIN(N)   - 1.319*SIN(2*(F-D+N)) - 0.227*SIN(2*(F+N))
              + 0.206*SIN(2*N) + 0.143*SIN(LS) ) / ARC;
    DEPS := ( + 9.203*COS(N)   + 0.574*COS(2*(F-D+N)) + 0.098*COS(2*(F+N))
              - 0.090*COS(2*N)                 ) / ARC;
    C := DPSI*COS(EPS);  S := DPSI*SIN(EPS);
    DX := -(C*Y+S*Z); DY := (C*X-DEPS*Z); DZ := (S*X+DEPS*Y);
    X  :=  X + DX;        Y  := Y + DY;       Z  := Z + DZ;
  end;

(*-----------------------------------------------------------------------*)
(* PMATECL: calculates the precession matrix A[i,j] for                  *)
(*          transforming ecliptic coordinates from equinox T1 to T2      *)
(*          ( T=(JD-2451545.0)/36525 )                                   *)
(*-----------------------------------------------------------------------*)
PROCEDURE PMATECL(T1,T2:float64;VAR A: REAL33);
  VAR DT,PPI,PI,PA: float64;
      C1,S1,C2,S2,C3,S3: float64;
  begin
    DT:=T2-T1;
    PPI := 174.876383889 +( ((3289.4789+0.60622*T1)*T1) +((-869.8089-0.50491*T1) + 0.03536*DT)*DT )/SEC;
    PI  := ( (47.0029-(0.06603-0.000598*T1)*T1)+ ((-0.03302+0.000598*T1)+0.000060*DT)*DT )*DT/SEC;
    PA  := ( (5029.0966+(2.22226-0.000042*T1)*T1)+ ((1.11113-0.000042*T1)-0.000006*DT)*DT )*DT/SEC;
    C1:=CosDeg(PPI+PA);  C2:=CosDeg(PI);  C3:=CosDeg(PPI);
    S1:=SinDeg(PPI+PA);  S2:=SinDeg(PI);  S3:=SinDeg(PPI);
    A[1,1]:=+C1*C3+S1*C2*S3; A[1,2]:=+C1*S3-S1*C2*C3; A[1,3]:=-S1*S2;
    A[2,1]:=+S1*C3-C1*C2*S3; A[2,2]:=+S1*S3+C1*C2*C3; A[2,3]:=+C1*S2;
    A[3,1]:=+S2*S3;          A[3,2]:=-S2*C3;          A[3,3]:=+C2;
  end;

(*-----------------------------------------------------------------------*)
(* PMATEQU: calculates the precession matrix A[i,j] for                  *)
(*          transforming equatorial coordinates from equinox T1 to T2    *)
(*          (T=(JD-2451545.0)/36525 )                                    *)
(*-----------------------------------------------------------------------*)
PROCEDURE PMATEQU(T1,T2:float64;VAR A:REAL33);
  VAR DT,ZETA,Z,THETA: float64;
      C1,S1,C2,S2,C3,S3: float64;
  begin
   DT:=T2-T1;
    ZETA  := ( (2306.2181+(1.39656-0.000139*T1)*T1)+
                ((0.30188-0.000345*T1)+0.017998*DT)*DT )*DT/SEC;
    Z     := ZETA + ( (0.79280+0.000411*T1)+0.000205*DT)*DT*DT/SEC;
    THETA := ( (2004.3109-(0.85330+0.000217*T1)*T1)-
                ((0.42665+0.000217*T1)+0.041833*DT)*DT )*DT/SEC;
    C1:=CosDeg(Z);  C2:=CosDeg(THETA);  C3:=CosDeg(ZETA);
    S1:=SinDeg(Z);  S2:=SinDeg(THETA);  S3:=SinDeg(ZETA);
    A[1,1]:=-S1*S3+C1*C2*C3; A[1,2]:=-S1*C3-C1*C2*S3; A[1,3]:=-C1*S2;
    A[2,1]:=+C1*S3+S1*C2*C3; A[2,2]:=+C1*C3-S1*C2*S3; A[2,3]:=-S1*S2;
    A[3,1]:=+S2*C3;          A[3,2]:=-S2*S3;          A[3,3]:=+C2;
  end;

(*-----------------------------------------------------------------------*)
(* PN_MATRIX: combined precession and nutation matrix for transformation *)
(*            from mean equinox T0 to true equinox T                     *)
(*            T0,T in Julian cent. since J2000; T=(JD-2451545.0)/36525   *)
(*-----------------------------------------------------------------------*)
PROCEDURE PN_MATRIX ( T0,T:float64; VAR A: REAL33 );
  begin
    PMATEQU(T0,T,A);                  (* precession matrix T0->T;     *)
    NUTEQU(T,A[1,1],A[2,1],A[3,1]);   (* transform column vectors of  *)
    NUTEQU(T,A[1,2],A[2,2],A[3,2]);   (* matrix A from mean equinox T *)
    NUTEQU(T,A[1,3],A[2,3],A[3,3]);   (* to true equinox T            *)
  end;

(*-----------------------------------------------------------------------*)
(* PRECART: calculate change of coordinates due to precession            *)
(*          for given transformation matrix A[i,j]                       *)
(*          (to be used with PMATECL und PMATEQU)                        *)
(*-----------------------------------------------------------------------*)
PROCEDURE PRECART(A:REAL33; VAR X,Y,Z:float64);
  VAR U,V,W: float64;
  begin
    U := A[1,1]*X+A[1,2]*Y+A[1,3]*Z;
    V := A[2,1]*X+A[2,2]*Y+A[2,3]*Z;
    W := A[3,1]*X+A[3,2]*Y+A[3,3]*Z;
    X:=U; Y:=V; Z:=W;
  end;


(*-----------------------------------------------------------------------*)
(* Unit SPHLIB: spherical astronomy                                      *)
(*-----------------------------------------------------------------------*)

(*-----------------------------------------------------------------------*)
(* ABERRAT: velocity vector of the Earth in equatorial coordinates       *)
(*          (in units of the velocity of light)                          *)
(*-----------------------------------------------------------------------*)
PROCEDURE ABERRAT(T: float64; VAR VX,VY,VZ: float64);
  VAR L,CL: float64;
  FUNCTION FRAC(X:float64):float64;
    begin  X:=X-TRUNC(X); if (X<0) then X:=X+1; FRAC:=X  end;
  begin
    L := TwoPi*FRAC(0.27908+100.00214*T);  CL:=COS(L);
    VX := -0.994E-4*SIN(L); VY := +0.912E-4*CL; VZ := +0.395E-4*CL;
  end;


(*-----------------------------------------------------------------------*)
(* ECLEQU: Conversion of ecliptic into equatorial coordinates            *)
(*         (T: equinox in Julian centuries since J2000)                  *)
(*-----------------------------------------------------------------------*)
PROCEDURE ECLEQU(T:float64;VAR X,Y,Z:float64);
  VAR EPS,C,S,V: float64;
  begin
    EPS:=23.43929111-(46.8150+(0.00059-0.001813*T)*T)*T/3600.0;
    C:=CosDeg(EPS);  S:=SinDeg(EPS);
    V:=+C*Y-S*Z;  Z:=+S*Y+C*Z;  Y:=V;
  end;
(*-----------------------------------------------------------------------*)
(* ECLEQU: Conversion of ecliptic into equatorial coordinates            *)
(*         (T: equinox in Julian centuries since J2000)                  *)
(*-----------------------------------------------------------------------*)
PROCEDURE EQUECL(T:float64;VAR X,Y,Z:float64);
  VAR EPS,C,S,V: float64;
  begin
    EPS:=23.43929111-(46.8150+(0.00059-0.001813*T)*T)*T/3600.0;
    C:=CosDeg(EPS);  S:=SinDeg(EPS);
    V:=+C*Y+S*Z;  Z:=-S*Y+C*Z;  Y:=V;
  end;
(*-----------------------------------------------------------------------*)
(* EQUSTD: transformation of equatorial coordinates into                 *)
(*         standard coordinates                                          *)
(*   RA0,DEC0: right ascension and declination of the optical axis (deg) *)
(*   RA,DEC:   right ascension and declination (deg)                     *)
(*   XX,YY:    standard coordinates                                      *)
(*-----------------------------------------------------------------------*)
PROCEDURE EQUSTD ( RA0,DEC0,RA,DEC: float64; VAR XX,YY: float64);
  VAR C: float64;
  begin
    C  := CosDeg(DEC0)*CosDeg(DEC)*CosDeg(RA-RA0)+SinDeg(DEC0)*SinDeg(DEC);
    XX := - ( CosDeg(DEC)*SinDeg(RA-RA0) ) / C;
    YY := - ( SinDeg(DEC0)*CosDeg(DEC)*CosDeg(RA-RA0)-CosDeg(DEC0)*SinDeg(DEC) ) / C;
  end;


(*-----------------------------------------------------------------------*)
(* Unit SUNLIB: solar orbit                                              *)
(*-----------------------------------------------------------------------*)


(*-----------------------------------------------------------------------*)
(* MINI_SUN: low precision solar coordinates (approx. 1')                *)
(*           T  : time in Julian centuries since J2000                   *)
(*                ( T=(JD-2452545)/36525 )                               *)
(*           RA : right ascension (in h; equinox of date)                *)
(*           DEC: declination (in deg; equinox of date)                  *)
(*-----------------------------------------------------------------------*)
PROCEDURE MINI_SUN(T:float64; VAR RA,DEC: float64);
  CONST COSEPS=0.91748; SINEPS=0.39778;
  VAR   L,M,DL,SL,X,Y,Z,RHO: float64;
  FUNCTION FRAC(X:float64):float64;
    begin  X:=X-TRUNC(X); if (X<0) then X:=X+1; FRAC:=X  end;
  begin 
    M  := TwoPi*FRAC(0.993133+99.997361*T); 
    DL := 6893.0*SIN(M)+72.0*SIN(2*M);
    L  := TwoPi*FRAC(0.7859453 + M/TwoPi + (6191.2*T+DL)/1296E3);
    SL := SIN(L);
    X:=COS(L); Y:=COSEPS*SL; Z:=SINEPS*SL; RHO:=SQRT(1.0-Z*Z);
    DEC := (360.0/TwoPi)*ARCTAN(Z/RHO);
    RA  := ( 48.0/TwoPi)*ARCTAN(Y/(X+RHO)); if (RA<0) then RA:=RA+24.0;
  end;

(*-----------------------------------------------------------------------*)
(* SUN200: ecliptic coordinates L,B,R (in deg and AU) of the             *)
(*         Sun referred to the mean equinox of date                      *)
(*         (T: time in Julian centuries since J2000)                     *)
(*         (   = (JED-2451545.0)/36525             )                     *)
(*-----------------------------------------------------------------------*)
PROCEDURE SUN200(T:float64;VAR L,B,R:float64);
  VAR C3,S3:          array [-1..7] OF float64;
      C,S:            array [-8..0] OF float64;
      M2,M3,M4,M5,M6: float64;
      D,A,UU:         float64;
      U,V,DL,DR,DB:   float64;
      I:              INTEGER;

  FUNCTION FRAC(X:float64):float64;
    begin  X:=X-TRUNC(X); if (X<0) then X:=X+1.0; FRAC:=X  end;

  PROCEDURE ADDTHE(C1,S1,C2,S2:float64; VAR C,S:float64);
    begin  C:=C1*C2-S1*S2; S:=S1*C2+C1*S2; end;

  PROCEDURE TERM(I1,I,IT:INTEGER;DLC,DLS,DRC,DRS,DBC,DBS:float64);
    begin
      if IT=0 then ADDTHE(C3[I1],S3[I1],C[I],S[I],U,V)
              else begin U:=U*T; V:=V*T end;
      DL:=DL+DLC*U+DLS*V; DR:=DR+DRC*U+DRS*V; DB:=DB+DBC*U+DBS*V;
    end;


  PROCEDURE PERTVEN;  (* Keplerian terms and perturbations by Venus *)
    VAR I: INTEGER;
    begin
      C[0]:=1.0; S[0]:=0.0; C[-1]:=COS(M2); S[-1]:=-SIN(M2);
      for I:=-1 DOWNTO -5 do ADDTHE(C[I],S[I],C[-1],S[-1],C[I-1],S[I-1]);
      TERM(1, 0,0,-0.22,6892.76,-16707.37, -0.54, 0.00, 0.00);
      TERM(1, 0,1,-0.06, -17.35,    42.04, -0.15, 0.00, 0.00);
      TERM(1, 0,2,-0.01,  -0.05,     0.13, -0.02, 0.00, 0.00);
      TERM(2, 0,0, 0.00,  71.98,  -139.57,  0.00, 0.00, 0.00);
      TERM(2, 0,1, 0.00,  -0.36,     0.70,  0.00, 0.00, 0.00);
      TERM(3, 0,0, 0.00,   1.04,    -1.75,  0.00, 0.00, 0.00);
      TERM(0,-1,0, 0.03,  -0.07,    -0.16, -0.07, 0.02,-0.02);
      TERM(1,-1,0, 2.35,  -4.23,    -4.75, -2.64, 0.00, 0.00);
      TERM(1,-2,0,-0.10,   0.06,     0.12,  0.20, 0.02, 0.00);
      TERM(2,-1,0,-0.06,  -0.03,     0.20, -0.01, 0.01,-0.09);
      TERM(2,-2,0,-4.70,   2.90,     8.28, 13.42, 0.01,-0.01);
      TERM(3,-2,0, 1.80,  -1.74,    -1.44, -1.57, 0.04,-0.06);
      TERM(3,-3,0,-0.67,   0.03,     0.11,  2.43, 0.01, 0.00);
      TERM(4,-2,0, 0.03,  -0.03,     0.10,  0.09, 0.01,-0.01);
      TERM(4,-3,0, 1.51,  -0.40,    -0.88, -3.36, 0.18,-0.10);
      TERM(4,-4,0,-0.19,  -0.09,    -0.38,  0.77, 0.00, 0.00);
      TERM(5,-3,0, 0.76,  -0.68,     0.30,  0.37, 0.01, 0.00);
      TERM(5,-4,0,-0.14,  -0.04,    -0.11,  0.43,-0.03, 0.00);
      TERM(5,-5,0,-0.05,  -0.07,    -0.31,  0.21, 0.00, 0.00);
      TERM(6,-4,0, 0.15,  -0.04,    -0.06, -0.21, 0.01, 0.00);
      TERM(6,-5,0,-0.03,  -0.03,    -0.09,  0.09,-0.01, 0.00);
      TERM(6,-6,0, 0.00,  -0.04,    -0.18,  0.02, 0.00, 0.00);
      TERM(7,-5,0,-0.12,  -0.03,    -0.08,  0.31,-0.02,-0.01);
    end;

  PROCEDURE PERTMAR;  (* perturbations by Mars *)
    VAR I: INTEGER;
    begin
      C[-1]:=COS(M4); S[-1]:=-SIN(M4);
      for I:=-1 DOWNTO -7 do ADDTHE(C[I],S[I],C[-1],S[-1],C[I-1],S[I-1]);
      TERM(1,-1,0,-0.22,   0.17,    -0.21, -0.27, 0.00, 0.00);
      TERM(1,-2,0,-1.66,   0.62,     0.16,  0.28, 0.00, 0.00);
      TERM(2,-2,0, 1.96,   0.57,    -1.32,  4.55, 0.00, 0.01);
      TERM(2,-3,0, 0.40,   0.15,    -0.17,  0.46, 0.00, 0.00);
      TERM(2,-4,0, 0.53,   0.26,     0.09, -0.22, 0.00, 0.00);
      TERM(3,-3,0, 0.05,   0.12,    -0.35,  0.15, 0.00, 0.00);
      TERM(3,-4,0,-0.13,  -0.48,     1.06, -0.29, 0.01, 0.00);
      TERM(3,-5,0,-0.04,  -0.20,     0.20, -0.04, 0.00, 0.00);
      TERM(4,-4,0, 0.00,  -0.03,     0.10,  0.04, 0.00, 0.00);
      TERM(4,-5,0, 0.05,  -0.07,     0.20,  0.14, 0.00, 0.00);
      TERM(4,-6,0,-0.10,   0.11,    -0.23, -0.22, 0.00, 0.00);
      TERM(5,-7,0,-0.05,   0.00,     0.01, -0.14, 0.00, 0.00);
      TERM(5,-8,0, 0.05,   0.01,    -0.02,  0.10, 0.00, 0.00);
    end;

  PROCEDURE PERTJUP;  (* perturbations by Jupiter *)
    VAR I: INTEGER;
    begin
      C[-1]:=COS(M5); S[-1]:=-SIN(M5);
      for I:=-1 DOWNTO -3 do ADDTHE(C[I],S[I],C[-1],S[-1],C[I-1],S[I-1]);
      TERM(1,-1,0, 0.01,   0.07,     0.18, -0.02, 0.00,-0.02);
      TERM(0,-1,0,-0.31,   2.58,     0.52,  0.34, 0.02, 0.00);
      TERM(1,-1,0,-7.21,  -0.06,     0.13,-16.27, 0.00,-0.02);
      TERM(1,-2,0,-0.54,  -1.52,     3.09, -1.12, 0.01,-0.17);
      TERM(1,-3,0,-0.03,  -0.21,     0.38, -0.06, 0.00,-0.02);
      TERM(2,-1,0,-0.16,   0.05,    -0.18, -0.31, 0.01, 0.00);
      TERM(2,-2,0, 0.14,  -2.73,     9.23,  0.48, 0.00, 0.00);
      TERM(2,-3,0, 0.07,  -0.55,     1.83,  0.25, 0.01, 0.00);
      TERM(2,-4,0, 0.02,  -0.08,     0.25,  0.06, 0.00, 0.00);
      TERM(3,-2,0, 0.01,  -0.07,     0.16,  0.04, 0.00, 0.00);
      TERM(3,-3,0,-0.16,  -0.03,     0.08, -0.64, 0.00, 0.00);
      TERM(3,-4,0,-0.04,  -0.01,     0.03, -0.17, 0.00, 0.00);
    end;

  PROCEDURE PERTSAT;  (* perturbations by Saturn *)
    begin
      C[-1]:=COS(M6); S[-1]:=-SIN(M6);
      ADDTHE(C[-1],S[-1],C[-1],S[-1],C[-2],S[-2]);
      TERM(0,-1,0, 0.00,   0.32,     0.01,  0.00, 0.00, 0.00);
      TERM(1,-1,0,-0.08,  -0.41,     0.97, -0.18, 0.00,-0.01);
      TERM(1,-2,0, 0.04,   0.10,    -0.23,  0.10, 0.00, 0.00);
      TERM(2,-2,0, 0.04,   0.10,    -0.35,  0.13, 0.00, 0.00);
    end;

  PROCEDURE PERTMOO;  (* difference between the Earth-Moon      *)
    begin             (* barycenter and the center of the Earth *)
      DL := DL +  6.45*SIN(D) - 0.42*SIN(D-A) + 0.18*SIN(D+A)
                              + 0.17*SIN(D-M3) - 0.06*SIN(D+M3);
      DR := DR + 30.76*COS(D) - 3.06*COS(D-A)+ 0.85*COS(D+A)
                              - 0.58*COS(D+M3) + 0.57*COS(D-M3);
      DB := DB + 0.576*SIN(UU);
    end;

  begin  (* SUN200 *)

    DL:=0.0; DR:=0.0; DB:=0.0;
    M2:=TwoPi*FRAC(0.1387306+162.5485917*T);
    M3:=TwoPi*FRAC(0.9931266+99.9973604*T);
    M4:=TwoPi*FRAC(0.0543250+ 53.1666028*T);
    M5:=TwoPi*FRAC(0.0551750+ 8.4293972*T);
    M6:=TwoPi*FRAC(0.8816500+  3.3938722*T); D :=TwoPi*FRAC(0.8274+1236.8531*T);
    A :=TwoPi*FRAC(0.3749+1325.5524*T);      UU:=TwoPi*FRAC(0.2591+1342.2278*T);
    C3[0]:=1.0;     S3[0]:=0.0;
    C3[1]:=COS(M3); S3[1]:=SIN(M3);  C3[-1]:=C3[1]; S3[-1]:=-S3[1];
    for I:=2 to 7 do ADDTHE(C3[I-1],S3[I-1],C3[1],S3[1],C3[I],S3[I]);
    PERTVEN; PERTMAR; PERTJUP; PERTSAT; PERTMOO;
    DL:=DL + 6.40*SIN(TwoPi*(0.6983+0.0561*T))+1.87*SIN(TwoPi*(0.5764+0.4174*T))
           + 0.27*SIN(TwoPi*(0.4189+0.3306*T))+0.20*SIN(TwoPi*(0.3581+2.4814*T));
    L:= 360.0*FRAC(0.7859453 + M3/TwoPi + ((6191.2+1.1*T)*T+DL)/1296.0E3 );
    R:= 1.0001398 - 0.0000007*T  +  DR*1E-6;
    B:= DB/3600.0;

  end;   (* SUN200 *)

(*-----------------------------------------------------------------------*)
(* SUNEQU: apparent equatorial coordinates of the Sun                    *)
(*         (right ascension RA, declination DEC in deg, R in AU)         *)
(*         T in Julian centuries since J2000 (T=(JD-2451545.0)/36525)    *)
(*-----------------------------------------------------------------------*)
PROCEDURE SUNEQU(T:float64;VAR RA,DEC,R:float64);
  VAR DT,L,B,X,Y,Z: float64;
  begin
    DT := (8.32/1440.0)/36525.0; (* light-time correction of 8.32 min    *)
    SUN200(T-DT,L,B,R);          (* geocentric ecliptic coordinates      *)
    CART(R,B,L,X,Y,Z);           (* cartesian ecliptic coordinates       *)
    ECLEQU(T,X,Y,Z);             (* equatorial ecliptic coordinates      *)
    NUTEQU(T,X,Y,Z);             (* correction for nutation              *)
    POLAR(X,Y,Z,R,DEC,RA);       (* spherical coordinates;               *)
  end;                           (* true equinox of date                 *)

(*-----------------------------------------------------------------------*)
(* T_FIT_SUN: approximates the equatorial coordinates of the             *)
(*            Sun by Chebyshev expansions for a given period of time     *)
(*                                                                       *)
(*  TA     : first date (in Julian centuries since J2000)                *)
(*  TB     : last date ( TB < TA + 1 year )                              *)
(*  N      : highest order                                               *)
(*  RA_POLY: coefficients for right ascencion                            *)
(*  DE_POLY: coefficients for declination                                *)
(*  R_POLY : coefficients for geocentric distance                        *)
(*-----------------------------------------------------------------------*)
PROCEDURE T_FIT_SUN ( TA,TB: float64; N: INTEGER;  VAR RA_POLY,DE_POLY,R_POLY: TPOLYNOM);
  begin
    T_FIT_LBR (SUNEQU,TA,TB,N,RA_POLY,DE_POLY,R_POLY);
  end;


(*-----------------------------------------------------------------------*)
(* Unit MOOLIB: lunar orbit                                              *)
(*-----------------------------------------------------------------------*)


(*-----------------------------------------------------------------------*)
(* MINI_MOON: low precision lunar coordinates (approx. 5'/1')            *)
(*            T  : time in Julian centuries since J2000                  *)
(*                 ( T=(JD-2452545)/36525 )                              *)
(*            RA : right ascension (in h; equinox of date)               *)
(*            DEC: declination (in deg; equinox of date)                 *)
(*-----------------------------------------------------------------------*)
PROCEDURE MINI_MOON (T: float64; VAR RA,DEC: float64);
CONST
   COSEPS=0.91748;
   SINEPS=0.39778;  (* cos/sin(obliquity ecliptic)  *)
VAR
   L0,L,LS,F,D,H,S,N,DL,CB    : float64;
   L_MOON,B_MOON,V,W,X,Y,Z,RHO: float64;

  FUNCTION FRAC(X:float64):float64;
    begin  X:=X-TRUNC(X); if (X<0) then X:=X+1; FRAC:=X  end;

  begin
    (* mean elements of lunar orbit *)
    L0:= FRAC(0.606433+1336.855225*T); (* mean longitude Moon (in rev) *)
    L :=TwoPi*FRAC(0.374897+1325.552410*T); (* mean anomaly of the Moon     *)
    LS:=TwoPi*FRAC(0.993133+  99.997361*T); (* mean anomaly of the Sun      *)
    D :=TwoPi*FRAC(0.827361+1236.853086*T); (* diff. longitude Moon-Sun     *)
    F :=TwoPi*FRAC(0.259086+1342.227825*T); (* mean argument of latitude    *)
    DL := +22640*SIN(L) - 4586*SIN(L-2*D) + 2370*SIN(2*D) +  769*SIN(2*L)
          -668*SIN(LS)- 412*SIN(2*F) - 212*SIN(2*L-2*D) - 206*SIN(L+LS-2*D)
          +192*SIN(L+2*D) - 165*SIN(LS-2*D) - 125*SIN(D) - 110*SIN(L+LS)
          +148*SIN(L-LS) - 55*SIN(2*F-2*D);
    S := F + (DL+412*SIN(2*F)+541*SIN(LS)) / ARC;
    H := F-2*D;
    N := -526*SIN(H) + 44*SIN(L+H) - 31*SIN(-L+H) - 23*SIN(LS+H)
         + 11*SIN(-LS+H) -25*SIN(-2*L+F) + 21*SIN(-L+F);
    L_MOON := TwoPi * FRAC ( L0 + DL/1296E3 ); (* in rad *)
    B_MOON := ( 18520.0*SIN(S) + N ) / ARC; (* in rad *)
    (* equatorial coordinates *)
    CB:=COS(B_MOON);
    X:=CB*COS(L_MOON); V:=CB*SIN(L_MOON); W:=SIN(B_MOON);
    Y:=COSEPS*V-SINEPS*W; Z:=SINEPS*V+COSEPS*W; RHO:=SQRT(1.0-Z*Z);
    DEC := (360.0/TwoPi)*ARCTAN(Z/RHO);
    RA  := ( 48.0/TwoPi)*ARCTAN(Y/(X+RHO)); if RA<0 then RA:=RA+24.0;
  end;

(*-----------------------------------------------------------------------*)
(* MOON: analytical lunar theory by E.W.Brown (Improved Lunar Ephemeris) *)
(*       with an accuracy of approx. 1"                                  *)
(*                                                                       *)
(*       T:      time in Julian centuries since J2000 (Ephemeris Time)   *)
(*               (T=(JD-2451545.0)/36525.0)                              *)
(*       LAMBDA: geocentric ecliptic longitude (equinox of date)         *)
(*       BETA:   geocentric ecliptic latitude  (equinox of date)         *)
(*       R:      geocentric distance (in Earth radii)                    *)
(*                                                                       *)
(*-----------------------------------------------------------------------*)

PROCEDURE MOON ( T:float64; VAR LAMBDA,BETA,R: float64 );

  VAR DGAM,FAC           : float64;
      DLAM,N,GAM1C,SINPI : float64;
      L0, L, LS, F, D ,S : float64;
      DL0,DL,DLS,DF,DD,DS: float64;
      CO,SI: array[-6..6,1..4] OF float64;

  FUNCTION FRAC(X:float64):float64;
    begin  X:=X-TRUNC(X); if (X<0) then X:=X+1; FRAC:=X  end;

  (* calculate c=cos(a1+a2) and s=sin(a1+a2) from the addition theo-  *)
  (* rems for c1=cos(a1), s1=sin(a1), c2=cos(a2) and s2=sin(a2)       *)
  PROCEDURE ADDTHE(C1,S1,C2,S2:float64;VAR C,S:float64);
    begin C:=C1*C2-S1*S2; S:=S1*C2+C1*S2; end;

  (* calculate sin(phi); phi in units of 1 revolution = 360 degrees   *)
  FUNCTION SINE (PHI:float64):float64;
    begin  SINE:=SIN(TwoPi*FRAC(PHI));  end;

  (* calculate long-periodic changes of the mean elements             *)
  (* l,l',F,D and L0 as well as dgamma                                *)
  PROCEDURE LONG_PERIODIC ( T: float64; VAR DL0,DL,DLS,DF,DD,DGAM: float64 );
    VAR S1,S2,S3,S4,S5,S6,S7: float64;
    begin
      S1:=SINE(0.19833+0.05611*T); S2:=SINE(0.27869+0.04508*T);
      S3:=SINE(0.16827-0.36903*T); S4:=SINE(0.34734-5.37261*T);
      S5:=SINE(0.10498-5.37899*T); S6:=SINE(0.42681-0.41855*T);
      S7:=SINE(0.14943-5.37511*T);
      DL0:= 0.84*S1+0.31*S2+14.27*S3+ 7.26*S4+ 0.28*S5+0.24*S6;
      DL := 2.94*S1+0.31*S2+14.27*S3+ 9.34*S4+ 1.12*S5+0.83*S6;
      DLS:=-6.40*S1                                   -1.89*S6;
      DF := 0.21*S1+0.31*S2+14.27*S3-88.70*S4-15.30*S5+0.24*S6-1.86*S7;
      DD := DL0-DLS;
      DGAM  := -3332E-9 * SINE(0.59734-5.37261*T)
                -539E-9 * SINE(0.35498-5.37899*T)
                 -64E-9 * SINE(0.39943-5.37511*T);
    end;


  (* INIT: calculates the mean elements and their sine and cosine   *)
  (* l mean anomaly of the Moon     l' mean anomaly of the Sun      *)
  (* F mean distance from the node  D  mean elongation from the Sun *)

  PROCEDURE INIT;
    VAR I,J,MAX   : INTEGER;
        T2,ARG,FAC: float64;
    begin
      T2:=T*T;
      DLAM :=0; DS:=0; GAM1C:=0; SINPI:=3422.7000;
      LONG_PERIODIC ( T, DL0,DL,DLS,DF,DD,DGAM );
      L0 := TwoPi*FRAC(0.60643382+1336.85522467*T-0.00000313*T2) + DL0/ARC;
      L  := TwoPi*FRAC(0.37489701+1325.55240982*T+0.00002565*T2) + DL /ARC;
      LS := TwoPi*FRAC(0.99312619+  99.99735956*T-0.00000044*T2) + DLS/ARC;
      F  := TwoPi*FRAC(0.25909118+1342.22782980*T-0.00000892*T2) + DF /ARC;
      D  := TwoPi*FRAC(0.82736186+1236.85308708*T-0.00000397*T2) + DD /ARC;
      for I := 1 to 4 do
        begin
          case I OF
            1: begin ARG:=L;  MAX:=4; FAC:=1.000002208;               end;
            2: begin ARG:=LS; MAX:=3; FAC:=0.997504612-0.002495388*T; end;
            3: begin ARG:=F;  MAX:=4; FAC:=1.000002708+139.978*DGAM;  end;
            4: begin ARG:=D;  MAX:=6; FAC:=1.0;                       end;
          end;
          CO[0,I]:=1.0; CO[1,I]:=COS(ARG)*FAC;
          SI[0,I]:=0.0; SI[1,I]:=SIN(ARG)*FAC;
          for J := 2 to MAX do
            ADDTHE(CO[J-1,I],SI[J-1,I],CO[1,I],SI[1,I],CO[J,I],SI[J,I]);
          for J := 1 to MAX do
            begin CO[-J,I]:=CO[J,I]; SI[-J,I]:=-SI[J,I]; end;
        end;
    end;


  (* TERM calculates X=cos(p*arg1+q*arg2+r*arg3+s*arg4) and   *)
  (*                 Y=sin(p*arg1+q*arg2+r*arg3+s*arg4)       *)
  PROCEDURE TERM(P,Q,R,S:INTEGER;VAR X,Y:float64);
    VAR  I: array[1..4] OF INTEGER;  K: INTEGER;
    begin
      I[1]:=P; I[2]:=Q; I[3]:=R; I[4]:=S;  X:=1.0; Y:=0.0;
      for K:=1 to 4 do
        if (I[K]<>0) then  ADDTHE(X,Y,CO[I[K],K],SI[I[K],K],X,Y);
    end;

  PROCEDURE ADDSOL(COEFFL,COEFFS,COEFFG,COEFFP:float64;P,Q,R,S:INTEGER);
    VAR X,Y: float64;
    begin
      TERM(P,Q,R,S,X,Y);
      DLAM :=DLAM +COEFFL*Y; DS   :=DS   +COEFFS*Y;
      GAM1C:=GAM1C+COEFFG*X; SINPI:=SINPI+COEFFP*X;
    end;


  PROCEDURE SOLAR1;
    begin
      ADDSOL(   13.902,   14.06,-0.001,   0.2607,0, 0, 0, 4);
      ADDSOL(    0.403,   -4.01,+0.394,   0.0023,0, 0, 0, 3);
      ADDSOL( 2369.912, 2373.36,+0.601,  28.2333,0, 0, 0, 2);
      ADDSOL( -125.154, -112.79,-0.725,  -0.9781,0, 0, 0, 1);
      ADDSOL(    1.979,    6.98,-0.445,   0.0433,1, 0, 0, 4);
      ADDSOL(  191.953,  192.72,+0.029,   3.0861,1, 0, 0, 2);
      ADDSOL(   -8.466,  -13.51,+0.455,  -0.1093,1, 0, 0, 1);
      ADDSOL(22639.500,22609.07,+0.079, 186.5398,1, 0, 0, 0);
      ADDSOL(   18.609,    3.59,-0.094,   0.0118,1, 0, 0,-1);
      ADDSOL(-4586.465,-4578.13,-0.077,  34.3117,1, 0, 0,-2);
      ADDSOL(   +3.215,    5.44,+0.192,  -0.0386,1, 0, 0,-3);
      ADDSOL(  -38.428,  -38.64,+0.001,   0.6008,1, 0, 0,-4);
      ADDSOL(   -0.393,   -1.43,-0.092,   0.0086,1, 0, 0,-6);
      ADDSOL(   -0.289,   -1.59,+0.123,  -0.0053,0, 1, 0, 4);
      ADDSOL(  -24.420,  -25.10,+0.040,  -0.3000,0, 1, 0, 2);
      ADDSOL(   18.023,   17.93,+0.007,   0.1494,0, 1, 0, 1);
      ADDSOL( -668.146, -126.98,-1.302,  -0.3997,0, 1, 0, 0);
      ADDSOL(    0.560,    0.32,-0.001,  -0.0037,0, 1, 0,-1);
      ADDSOL( -165.145, -165.06,+0.054,   1.9178,0, 1, 0,-2);
      ADDSOL(   -1.877,   -6.46,-0.416,   0.0339,0, 1, 0,-4);
      ADDSOL(    0.213,    1.02,-0.074,   0.0054,2, 0, 0, 4);
      ADDSOL(   14.387,   14.78,-0.017,   0.2833,2, 0, 0, 2);
      ADDSOL(   -0.586,   -1.20,+0.054,  -0.0100,2, 0, 0, 1);
      ADDSOL(  769.016,  767.96,+0.107,  10.1657,2, 0, 0, 0);
      ADDSOL(   +1.750,    2.01,-0.018,   0.0155,2, 0, 0,-1);
      ADDSOL( -211.656, -152.53,+5.679,  -0.3039,2, 0, 0,-2);
      ADDSOL(   +1.225,    0.91,-0.030,  -0.0088,2, 0, 0,-3);
      ADDSOL(  -30.773,  -34.07,-0.308,   0.3722,2, 0, 0,-4);
      ADDSOL(   -0.570,   -1.40,-0.074,   0.0109,2, 0, 0,-6);
      ADDSOL(   -2.921,  -11.75,+0.787,  -0.0484,1, 1, 0, 2);
      ADDSOL(   +1.267,    1.52,-0.022,   0.0164,1, 1, 0, 1);
      ADDSOL( -109.673, -115.18,+0.461,  -0.9490,1, 1, 0, 0);
      ADDSOL( -205.962, -182.36,+2.056,  +1.4437,1, 1, 0,-2);
      ADDSOL(    0.233,    0.36, 0.012,  -0.0025,1, 1, 0,-3);
      ADDSOL(   -4.391,   -9.66,-0.471,   0.0673,1, 1, 0,-4);
    end;

  PROCEDURE SOLAR2;
    begin
      ADDSOL(    0.283,    1.53,-0.111,  +0.0060,1,-1, 0,+4);
      ADDSOL(   14.577,   31.70,-1.540,  +0.2302,1,-1, 0, 2);
      ADDSOL(  147.687,  138.76,+0.679,  +1.1528,1,-1, 0, 0);
      ADDSOL(   -1.089,    0.55,+0.021,   0.0   ,1,-1, 0,-1);
      ADDSOL(   28.475,   23.59,-0.443,  -0.2257,1,-1, 0,-2);
      ADDSOL(   -0.276,   -0.38,-0.006,  -0.0036,1,-1, 0,-3);
      ADDSOL(    0.636,    2.27,+0.146,  -0.0102,1,-1, 0,-4);
      ADDSOL(   -0.189,   -1.68,+0.131,  -0.0028,0, 2, 0, 2);
      ADDSOL(   -7.486,   -0.66,-0.037,  -0.0086,0, 2, 0, 0);
      ADDSOL(   -8.096,  -16.35,-0.740,   0.0918,0, 2, 0,-2);
      ADDSOL(   -5.741,   -0.04, 0.0  ,  -0.0009,0, 0, 2, 2);
      ADDSOL(    0.255,    0.0 , 0.0  ,   0.0   ,0, 0, 2, 1);
      ADDSOL( -411.608,   -0.20, 0.0  ,  -0.0124,0, 0, 2, 0);
      ADDSOL(    0.584,    0.84, 0.0  ,  +0.0071,0, 0, 2,-1);
      ADDSOL(  -55.173,  -52.14, 0.0  ,  -0.1052,0, 0, 2,-2);
      ADDSOL(    0.254,    0.25, 0.0  ,  -0.0017,0, 0, 2,-3);
      ADDSOL(   +0.025,   -1.67, 0.0  ,  +0.0031,0, 0, 2,-4);
      ADDSOL(    1.060,    2.96,-0.166,   0.0243,3, 0, 0,+2);
      ADDSOL(   36.124,   50.64,-1.300,   0.6215,3, 0, 0, 0);
      ADDSOL(  -13.193,  -16.40,+0.258,  -0.1187,3, 0, 0,-2);
      ADDSOL(   -1.187,   -0.74,+0.042,   0.0074,3, 0, 0,-4);
      ADDSOL(   -0.293,   -0.31,-0.002,   0.0046,3, 0, 0,-6);
      ADDSOL(   -0.290,   -1.45,+0.116,  -0.0051,2, 1, 0, 2);
      ADDSOL(   -7.649,  -10.56,+0.259,  -0.1038,2, 1, 0, 0);
      ADDSOL(   -8.627,   -7.59,+0.078,  -0.0192,2, 1, 0,-2);
      ADDSOL(   -2.740,   -2.54,+0.022,   0.0324,2, 1, 0,-4);
      ADDSOL(    1.181,    3.32,-0.212,   0.0213,2,-1, 0,+2);
      ADDSOL(    9.703,   11.67,-0.151,   0.1268,2,-1, 0, 0);
      ADDSOL(   -0.352,   -0.37,+0.001,  -0.0028,2,-1, 0,-1);
      ADDSOL(   -2.494,   -1.17,-0.003,  -0.0017,2,-1, 0,-2);
      ADDSOL(    0.360,    0.20,-0.012,  -0.0043,2,-1, 0,-4);
      ADDSOL(   -1.167,   -1.25,+0.008,  -0.0106,1, 2, 0, 0);
      ADDSOL(   -7.412,   -6.12,+0.117,   0.0484,1, 2, 0,-2);
      ADDSOL(   -0.311,   -0.65,-0.032,   0.0044,1, 2, 0,-4);
      ADDSOL(   +0.757,    1.82,-0.105,   0.0112,1,-2, 0, 2);
      ADDSOL(   +2.580,    2.32,+0.027,   0.0196,1,-2, 0, 0);
      ADDSOL(   +2.533,    2.40,-0.014,  -0.0212,1,-2, 0,-2);
      ADDSOL(   -0.344,   -0.57,-0.025,  +0.0036,0, 3, 0,-2);
      ADDSOL(   -0.992,   -0.02, 0.0  ,   0.0   ,1, 0, 2, 2);
      ADDSOL(  -45.099,   -0.02, 0.0  ,  -0.0010,1, 0, 2, 0);
      ADDSOL(   -0.179,   -9.52, 0.0  ,  -0.0833,1, 0, 2,-2);
      ADDSOL(   -0.301,   -0.33, 0.0  ,   0.0014,1, 0, 2,-4);
      ADDSOL(   -6.382,   -3.37, 0.0  ,  -0.0481,1, 0,-2, 2);
      ADDSOL(   39.528,   85.13, 0.0  ,  -0.7136,1, 0,-2, 0);
      ADDSOL(    9.366,    0.71, 0.0  ,  -0.0112,1, 0,-2,-2);
      ADDSOL(    0.202,    0.02, 0.0  ,   0.0   ,1, 0,-2,-4);
    end;

  PROCEDURE SOLAR3;
    begin
      ADDSOL(    0.415,    0.10, 0.0  ,  0.0013,0, 1, 2, 0);
      ADDSOL(   -2.152,   -2.26, 0.0  , -0.0066,0, 1, 2,-2);
      ADDSOL(   -1.440,   -1.30, 0.0  , +0.0014,0, 1,-2, 2);
      ADDSOL(    0.384,   -0.04, 0.0  ,  0.0   ,0, 1,-2,-2);
      ADDSOL(   +1.938,   +3.60,-0.145, +0.0401,4, 0, 0, 0);
      ADDSOL(   -0.952,   -1.58,+0.052, -0.0130,4, 0, 0,-2);
      ADDSOL(   -0.551,   -0.94,+0.032, -0.0097,3, 1, 0, 0);
      ADDSOL(   -0.482,   -0.57,+0.005, -0.0045,3, 1, 0,-2);
      ADDSOL(    0.681,    0.96,-0.026,  0.0115,3,-1, 0, 0);
      ADDSOL(   -0.297,   -0.27, 0.002, -0.0009,2, 2, 0,-2);
      ADDSOL(    0.254,   +0.21,-0.003,  0.0   ,2,-2, 0,-2);
      ADDSOL(   -0.250,   -0.22, 0.004,  0.0014,1, 3, 0,-2);
      ADDSOL(   -3.996,    0.0 , 0.0  , +0.0004,2, 0, 2, 0);
      ADDSOL(    0.557,   -0.75, 0.0  , -0.0090,2, 0, 2,-2);
      ADDSOL(   -0.459,   -0.38, 0.0  , -0.0053,2, 0,-2, 2);
      ADDSOL(   -1.298,    0.74, 0.0  , +0.0004,2, 0,-2, 0);
      ADDSOL(    0.538,    1.14, 0.0  , -0.0141,2, 0,-2,-2);
      ADDSOL(    0.263,    0.02, 0.0  ,  0.0   ,1, 1, 2, 0);
      ADDSOL(    0.426,   +0.07, 0.0  , -0.0006,1, 1,-2,-2);
      ADDSOL(   -0.304,   +0.03, 0.0  , +0.0003,1,-1, 2, 0);
      ADDSOL(   -0.372,   -0.19, 0.0  , -0.0027,1,-1,-2, 2);
      ADDSOL(   +0.418,    0.0 , 0.0  ,  0.0   ,0, 0, 4, 0);
      ADDSOL(   -0.330,   -0.04, 0.0  ,  0.0   ,3, 0, 2, 0);
    end;

  (* part N of the perturbations of ecliptic latitude                 *)
  PROCEDURE SOLARN(VAR N: float64);
    VAR X,Y: float64;
    PROCEDURE ADDN(COEFFN:float64;P,Q,R,S:INTEGER);
      begin TERM(P,Q,R,S,X,Y); N:=N+COEFFN*Y end;
    begin
      N := 0.0;
      ADDN(-526.069, 0, 0,1,-2); ADDN(  -3.352, 0, 0,1,-4);
      ADDN( +44.297,+1, 0,1,-2); ADDN(  -6.000,+1, 0,1,-4);
      ADDN( +20.599,-1, 0,1, 0); ADDN( -30.598,-1, 0,1,-2);
      ADDN( -24.649,-2, 0,1, 0); ADDN(  -2.000,-2, 0,1,-2);
      ADDN( -22.571, 0,+1,1,-2); ADDN( +10.985, 0,-1,1,-2);
    end;

  (* perturbations of ecliptic latitude by Venus and Jupiter          *)
  PROCEDURE PLANETARY(VAR DLAM:float64);
    begin
      DLAM  := DLAM
        +0.82*SINE(0.7736  -62.5512*T)+0.31*SINE(0.0466 -125.1025*T)
        +0.35*SINE(0.5785  -25.1042*T)+0.66*SINE(0.4591+1335.8075*T)
        +0.64*SINE(0.3130  -91.5680*T)+1.14*SINE(0.1480+1331.2898*T)
        +0.21*SINE(0.5918+1056.5859*T)+0.44*SINE(0.5784+1322.8595*T)
        +0.24*SINE(0.2275   -5.7374*T)+0.28*SINE(0.2965   +2.6929*T)
        +0.33*SINE(0.3132   +6.3368*T);
    end;

  begin
    INIT;  SOLAR1; SOLAR2; SOLAR3; SOLARN(N);  PLANETARY(DLAM);

    LAMBDA := 360.0*FRAC( (L0+DLAM/ARC) / TwoPi );

    S    := F + DS/ARC;
    FAC  := 1.000002708+139.978*DGAM;
    BETA := (FAC*(18518.511+1.189+GAM1C)*SIN(S)-6.24*SIN(3*S)+N) / 3600.0;

    SINPI := SINPI * 0.999953253;
    R     := ARC / SINPI;

  end;

(*-----------------------------------------------------------------------*)
(* MOONEQU: geocentric equatorial coordinates of the Moon                *)
(*          referred to the true equinox of date                         *)
(*   T   time in Julian centuries ephemeris time since J2000             *)
(*       ( T = (JD-2451545.0)/36525 )                                    *)
(*   RA  right ascension (deg)                                           *)
(*   DEC declination (deg)                                               *)
(*   R   distance (in earth radii)                                       *)
(*-----------------------------------------------------------------------*)
PROCEDURE MOONEQU(T:float64;VAR RA,DEC,R:float64);
  VAR L,B,X,Y,Z: float64;
  begin
    MOON(T,L,B,R);              (* ecliptic coordinates (mean equinox    *)
    CART(R,B,L,X,Y,Z);          (*                              of date) *)
    ECLEQU(T,X,Y,Z);            (* transform into equatorial coordinates *)
    NUTEQU(T,X,Y,Z);            (* nutation                              *)
    POLAR(X,Y,Z,R,DEC,RA);
  end;

(*-----------------------------------------------------------------------*)
(* T_FIT_MOON: approximates the equatorial coordinates                   *)
(*             of the Moon by Chebyshev expansions for a                 *)
(*             given period of time of at most one month                 *)
(*                                                                       *)
(*  TA     : first date (in Julian centuries since J2000)                *)
(*  TB     : last date ( TB < TA + 1 month )                             *)
(*  N      : highest order                                               *)
(*  RA_POLY: coefficients for right ascension                            *)
(*  DE_POLY: coefficients for declination                                *)
(*  R_POLY : coefficients for geocentric distance                        *)
(*-----------------------------------------------------------------------*)
PROCEDURE T_FIT_MOON ( TA,TB: float64; N: INTEGER; VAR RA_POLY,DE_POLY,R_POLY: TPOLYNOM);
  begin
    T_FIT_LBR ( MOONEQU, TA,TB,N, RA_POLY,DE_POLY,R_POLY );
  end;



(*-----------------------------------------------------------------------*)
(* Unit TIMLIB: time and calendar calculations                           *)
(*-----------------------------------------------------------------------*)


(*----------------------------------------------------------------------*)
(* CALDAT: Finds the civil calendar date for a given value              *)
(*         of the Modified Julian Date (MJD).                           *)
(*         Julian calendar is used up to 1582 October 4,                *)
(*         Gregorian calendar is used from 1582 October 15 onwards.     *)
(*----------------------------------------------------------------------*)
PROCEDURE CALDAT(MJD:float64; VAR DAY,MONTH,YEAR:INTEGER;VAR HOUR:float64);
VAR
   B,D,F     : INTEGER;
   JD,JD0,C,E: float64;
begin
    JD  := MJD + 2400000.5;
       JD0 := INT(JD+0.5);              (* TURBO Pascal     *)
    if (JD0<2299161.0)                            (* calendar:    *)
      then begin B:=0; C:=JD0+1524.0 end          (* -> Julian    *)
      else begin                                  (* -> Gregorian *)
             B:=TRUNC((JD0-1867216.25)/36524.25);
             C:=JD0+(B-TRUNC(B/4))+1525.0
           end;
    D    := TRUNC((C-122.1)/365.25);          E     := 365.0*D+TRUNC(D/4);
    F    := TRUNC((C-E)/30.6001);
    DAY  := TRUNC(C-E+0.5)-TRUNC(30.6001*F);  MONTH := F-1-12*TRUNC(F/14);
    YEAR := D-4715-TRUNC((7+MONTH)/10);       HOUR  := 24.0*(JD+0.5-JD0);
end;



(*-----------------------------------------------------------------------*)
(* LMST: local mean sidereal time                                        *)
(*-----------------------------------------------------------------------*)
FUNCTION LMST(MJD,LAMBDA:float64):float64;
var
   MJD0,T,UT,GMST: float64;

  FUNCTION FRAC(X:float64):float64;
  begin
     X:=X-TRUNC(X);
     if (X<0) then X:=X+1;
     FRAC:=X
  end;

begin
   MJD0:=INT(MJD);
   UT:=(MJD-MJD0)*24; T:=(MJD0-51544.5)/36525.0;
   GMST:=6.697374558 + 1.0027379093*UT
           +(8640184.812866+(0.093104-6.2E-6*T)*T)*T/3600.0;
   LMST:=24.0*FRAC( (GMST-LAMBDA/15.0) / 24.0 );
end;



(*-----------------------------------------------------------------------*)
(* MJD: Modified Julian Date                                             *)
(*      The routine is valid for any date since 4713 BC.                 *)
(*      Julian calendar is used up to 1582 October 4,                    *)
(*      Gregorian calendar is used from 1582 October 15 onwards.         *)
(*-----------------------------------------------------------------------*)
FUNCTION MJD(DAY,MONTH,YEAR:INTEGER;HOUR:float64):float64;
var
   A: float64; B: INTEGER;
begin
    A:=10000.0*YEAR+100.0*MONTH+DAY;
    if (MONTH<=2) then begin
       MONTH:=MONTH+12;
       YEAR:=YEAR-1
    end;
    if (A<=15821004.1)then B:=-2+TRUNC((YEAR+4716)/4)-1179
    else B:=TRUNC(YEAR/400)-TRUNC(YEAR/100)+TRUNC(YEAR/4);
    A:=365.0*YEAR-679004.0;
    MJD:=A+B+TRUNC(30.6001*(MONTH+1))+DAY+HOUR/24.0;
end;

PROCEDURE flmoon(n : LongInt; nph: integer; VAR jd: LongInt; VAR frac: float64);
VAR
   i : integer;
   xtra,t2,t,c,asf,am : float64;
BEGIN
   c := n + nph / 4.0;
   t := c / 1236.85;
   t2 := sqr(t);
   asf := 359.2242+29.105356*c;
   am := 306.0253+385.816918*c+0.010730*t2;
   jd := 2415020+28*n+7*nph;
   xtra := 0.75933+1.53058868*c+(1.178e-4-1.55e-7*t)*t2;
   IF nph in [0,2] THEN xtra := xtra+(0.1734-3.93e-4*t)*sinDeg(asf)-0.4068*sinDeg(am)
   ELSE IF nph in [1,3] THEN
      xtra := xtra+(0.1721-4.0e-4*t)*sinDeg(asf)-0.6280*sinDeg(am);
   IF (xtra >= 0.0) THEN i := trunc(xtra) ELSE i := trunc(xtra-1.0);
   jd := jd+i;
   frac := xtra-i;
END;


procedure FigurePhase(n,Code : integer; var Month,Day,Year : integer);
var
   jd : longint;
   Frac : float64;
begin
   flmoon(n,Code,jd,frac);
   frac := 24.0*(frac {+ (Zone*24)});
   IF (frac < 0.0) then begin
     dec(jd);
     frac := frac+24.0
   END;
   IF (frac > 12) then begin
     inc(jd);
     frac := frac-12.0
   END
   ELSE frac := frac + 12.0;
   PETMATH.CalDat(jd,Month,Day,Year);
end;




procedure MoonRise(stMonth,stDay,stYear,Duration : integer; Lat,Long : float64);
VAR
   ABOVE,RISE,SETT                      : BOOLEAN;
   DAY,MONTH,YEAR, I,IOBJ,NZ            : INTEGER;
   LAMBDA,ZONE,PHI,SPHI,CPHI            : float64;
   TSTART,DATE,HOUR,HH,UTRISE,UTSET     : float64;
   Y_MINUS,Y_0,Y_PLUS,ZERO1,ZERO2,XE,YE : float64;
   SINH0                                : array[1..3] of float64;
   aline : shortstring;
   sl : tStringList;
  // Outfile                              : TextFile;

         (*-----------------------------------------------------------------------*)
         (* SIN_ALT: sin(altitude)                                                *)
         (*         IOBJ:  1=moon, 2=sun                                          *)
         (*-----------------------------------------------------------------------*)

         FUNCTION SIN_ALT(IOBJ:INTEGER; MJD0,HOUR,LAMBDA,CPHI,SPHI : float64) : float64;
         VAR
           MJD,T,RA,DEC,TAU: float64;
         begin
            MJD := MJD0 + HOUR/24.0;
            T   := (MJD-51544.5)/36525.0;
            if (IOBJ=1) then MINI_MOON(T,RA,DEC) else  MINI_SUN (T,RA,DEC);
            TAU := 15.0 * (LMST(MJD,LAMBDA) - RA);
            SIN_ALT := SPHI*SinDeg(DEC) + CPHI*CosDeg(DEC)*CosDeg(TAU);
         end;


         procedure FigureThisPhase(n,Code : integer);
         var
            Month,Day,Year : integer;
         begin
            FigurePhase(n,Code,Month,Day,Year);
            aline := aline + IntegerToString(Month,8) + '/' + AddDayMonthLeadingZero(Day) + '/' + IntegerToString(Year,4);
         end;


var
   thisYear,n : integer;
begin (* SUNSET *)
   SINH0[1] := SinDeg ( +8.0/60.0); (* moonrise          at h= +8'        *)
   SINH0[2] := SinDeg (-50.0/60.0); (* sunrise           at h=-50'        *)
{
  SINH0[3] := SinDeg (   -12.0  ); (* nautical twilight at h=-12 degrees *)
}

   Phi := lat;
   Lambda := Long;

   Zone := round(Lambda /15);
   Lambda := - Lambda;

   ZONE := ZONE /24.0;
   tStart := MJD(stDay,StMonth,stYear,0) - ZONE;

   SPHI := SinDeg(PHI);
   CPHI := CosDeg(PHI);

   if MDDef.RiseSet then begin

     (*
     assignFile(Outfile,MDTempDir + 'Suntable.txt');
     rewrite(outfile);
     writeln(Outfile,LatLongDegreeToString(Phi,-Lambda,MDDef.OutPutLatLongMethod));
     *)
     sl := tStringList.Create;
     sl.add('DATA,MOON_RISE,MOON_SET,SUN_RISE,SUN_SET,LAT,LONG');
     for I:= 0 to Duration do  begin
          //if i mod 25 = 0 then Header;
          DATE := TSTART + I;
          CALDAT(DATE+ZONE,DAY,MONTH,YEAR,HH);
          aline := IntToStr(Month) + '/' + AddDayMonthLeadingZero(DAY) + '/' + IntToStr(Year);

          //WRITE(Outfile,MONTH:2,'/',AddDayMonthLeadingZero(DAY),'/',YEAR:4,'  '); (* print current date *)

          for IOBJ := 1 to 2 do  begin
              HOUR := 1.0;
              Y_MINUS := SIN_ALT(IOBJ,DATE,HOUR-1.0,LAMBDA,CPHI,SPHI) - SINH0[IOBJ];
              ABOVE := (Y_MINUS>0.0); RISE := FALSE; SETT := FALSE;

              (* loop over search intervals from [0h-2h] to [22h-24h]  *)
              repeat
                Y_0    := SIN_ALT(IOBJ,DATE,HOUR,LAMBDA,CPHI,SPHI) -  SINH0[IOBJ];
                Y_PLUS := SIN_ALT(IOBJ,DATE,HOUR+1.0,LAMBDA,CPHI,SPHI) -  SINH0[IOBJ];

                (* find parabola through three values Y_MINUS,Y_0,Y_PLUS *)
                QUAD( Y_MINUS,Y_0,Y_PLUS, XE,YE, ZERO1,ZERO2, NZ );

                case (NZ) OF
                  0: ;
                  1: if (Y_MINUS<0.0)then begin
                        UTRISE:=HOUR+ZERO1;
                        RISE:=TRUE;
                     end
                     else begin
                        UTSET :=HOUR+ZERO1;
                        SETT:=TRUE;
                     end;
                  2: begin
                       if (YE<0.0) then begin
                          UTRISE:=HOUR+ZERO2;
                          UTSET:=HOUR+ZERO1;
                       end
                       else begin
                          UTRISE:=HOUR+ZERO1;
                          UTSET:=HOUR+ZERO2;
                       end;
                       RISE:=TRUE;
                       SETT:=TRUE;
                     end;
                 end;

                 Y_MINUS := Y_PLUS;     (* prepare for next interval *)
                 HOUR := HOUR + 2.0;
              until ( (HOUR=25.0) or (RISE AND SETT) );

              if (RISE or SETT) then begin
                 //if RISE then WHM(UTRISE) else WRITE(Outfile,'----- ':9);
                 //if SETT then WHM(UTSET)  else WRITE(Outfile,'----- ':9);

                 if RISE then aline := aline +  ',' + HoursMinutesString(UTRISE)  else aline := aline + ', ';
                 if SETT then aline := aline +  ',' + HoursMinutesString(UTSET)  else aline := aline + ', ';
              end
              else begin
                 aline := aline +  ', , '
                 (*
                 if ABOVE then case IOBJ OF
                    1,2: WRITE (Outfile,'   always visible ');
                    3:   WRITE (Outfile,'    always bright ');
                 end
                 else case IOBJ OF
                     1,2: WRITE (Outfile,'  always invisible');
                     3:   WRITE (Outfile,'     always dark  ');
                 end;
                 *)
              end;
           end;
          //WRITELN(Outfile);
          sl.add(aline + ',' + RealToString(Lat,-10,-4) + ',' + RealToString(Long,-10,-4));
        end; (* end of loop over 10 days *)
        PetDBUtils.StringList2CSVtoDB(sl,NextFileNumber(MDTempDir,'moon_rise_','.dbf'));
      (*
      WRITELN(Outfile);
      WRITE(Outfile,' all times in local standard time ( = UT ');
      if ZONE >= 0 then WRITE(Outfile,'+');
      WRITELN(Outfile,ZONE*24.0:5:1,'h )');
      QuickOpenEditWindow(MDTempDir + 'Suntable.txt','Rise/set times');
      DeleteFileIfExists(MDTempDir + 'Suntable.txt');
      *)
   end;

   if MDDef.MoonPhase then  begin
     //assignFile(Outfile,MDTempDir + 'Suntable.txt');
     //rewrite(outfile);
     Sl := tStringList.Create;
     sl.Add('Day of moon phase (UTC)');
      Month := pred(stMonth);
      ThisYear := stYear;
      sl.Add('');
      sl.Add('      Moon Phases');
      sl.Add('');
      sl.Add('        New         First Quarter       Full         Last Quarter');
      sl.Add('-----------------------------------------------------------------');
      Duration := Duration div 30;
      n := trunc(12.37*(ThisYear-1900)) + Month;
      for i := 0 to succ(Duration) do begin
         aline := '';
         FigureThisPhase(n,0);
         FigureThisPhase(n,1);
         FigureThisPhase(n,2);
         FigureThisPhase(n,3);
         sl.Add(aline);
         inc(n);
      end;
      //closeFile(OutFile);
      //QuickOpenEditWindow(MDTempDir + 'Suntable.txt','Rise/set times');
      //DeleteFileIfExists(MDTempDir + 'Suntable.txt');
      DisplayAndPurgeStringList(sl,'Moon phases');
   end;
end;


function DaysSinceFullMoon(stMonth,stDay,stYear : integer; Lat,Long : float64) : integer;
VAR
   jDay,MoonJDay : float64;
   DAY,MONTH,YEAR,n : INTEGER;
begin
   jDay := (MJD(stDAY,stMONTH,stYEAR,12));
   n := pred(trunc(12.37*(stYear-1900)));
   repeat
      FigurePhase(n,2,Month,Day,Year);
      MoonJDay := (MJD(DAY,MONTH,YEAR,12));
      if (JDay >= MoonJDay) then Result := round(JDay - MoonJDay);
      inc(n);
   until MoonJDay >= JDay;
end;


end.
