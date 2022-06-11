unit OceanCal;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 11/26/2014      }
{_________________________________}



interface

uses
   PETMAR,Petmar_Types;


type
   SoundVelocityAlgorithm = (Medwin,Leroy,Simple);


function CalSigmaT(T,S : float64) : float64;
function SoundVelocity(T,S,Z : float64) : float64;
function CalSigmaTheta(Temp,Sal,Depth : float64) : float64;


implementation

var
   SoundVelAlg : SoundVelocityAlgorithm;


function CalSigmaTheta(Temp,Sal,Depth : float64) : float64;

      function Kappa(s,t,p : float64) : float64;
      var
         temp,p2,s3_2,t2,t3,t4 : float64;
      begin
         s3_2 := sqrt(s*s*s);
         t2   := sqr(t);
         t3   := t * t2;
         t4   := sqr(t2);
         p2   := sqr(p);
         temp := 19652.21 + 148.4206 * T         - 2.327105 * T2;
         temp := temp + 1.360477e-2 * t3         - 5.155288e-5 * T4;
         temp := temp + 3.239908 * p             + 1.43713e-3 * t * p;
         temp := temp + 1.16092e-4 * T2 * p      - 5.77905e-7 * t3 * p;
         temp := temp + 8.50935e-5 * p2          - 6.12293e-6 * t * p2;
         temp := temp + 5.2787e-8 * T2 * p2;
         temp := temp + 54.6746 * S              - 0.603459 * t * s;
         temp := temp + 1.09987e-2 * t2 * s      - 6.1670e-5 * t3 * s;
         temp := temp + 7.944e-2 * s3_2          + 1.6483e-2 * t * s3_2;
         temp := temp - 5.3009e-4 * t2 * s3_2    + 2.2838e-3 * p * s;
         temp := temp - 1.0981e-5 * t * p * s    - 1.6078e-6 * t2 * p * s;
         temp := temp + 1.91075e-4 * p * s3_2    - 9.9348e-7* P2 * s;
         temp := temp + 2.0816e-8 * t * P2 * S   + 9.1697e-10 * T2 * P2 * s;
         Kappa := temp;
      end;

begin
   Result := CalSigmaT(Temp,Sal);
   Result := 1 + 1000 * Result;
   Depth := 0.1 * Depth; {convert to bars}
   Result := Result / (1 - Depth / Kappa(Sal,Temp,Depth));
   Result := 0.001 * (Result - 1);
end;


function CalSigmaT(T,S : float64) : float64;
var
   T2,ssqrt,rho : float64;
begin
   T2 := sqr(T);
   Ssqrt := sqrt(S) * S;
   rho := 999.842594 + 6.793952e-2 * T;
   rho := rho - 9.095290e-3 * T2 + 1.001685e-4 * T * T2;
   rho := rho - 1.120083e-6 * sqr(T2) + 6.536322e-9 * T * sqr(T2);
   rho := rho + 8.24493e-1 * S -4.0899e-3 * T * S;
   rho := rho + 7.6438e-5 * T2 * S - 8.2467e-7 * T * T2 * S;
   rho := rho + 5.3875e-9 * sqr(T2) * S - 5.72466e-3 * Ssqrt;
   rho := rho + 1.0227e-4 * T * Ssqrt;
   rho := rho - 1.6546e-6 * T2 * Ssqrt;
   rho := rho + 4.8314e-4 * sqr(S);
   CalSigmaT := rho - 1000;
end;


function SoundVelocity(T,S,Z : float64) : float64;
begin
   case SoundVelAlg of
      Leroy :  {Leroy, J.Acoust.Soc.Am., 1969, 46:216}
         SoundVelocity := 1492.9 + 3.0 * (T-10) - 0.006 * sqr(T-10) - 0.04 * sqr(T-18) + 1.2 * (S-35) - 0.01 * (T-18) * (S-35) + (Z / 61.0);
      Medwin :  {Medwin, J.Acoust.Soc.Am., 1975, 58:1318}
          SoundVelocity := 1449.2 + 4.6 * T - 0.055 * sqr(T) + 0.00029 * sqr(T) * T + (1.34 - 0.01 * T) * (S - 35) + 0.016 * Z;
      Simple : {eq.5-9, p.73, Seawater: Its composition, properties, and behaviour}
          SoundVelocity := 1410.0 + 4.21 * T - 0.037 * sqr(T) + 1.14 * S + 0.018 * Z;
   end {case};
end;


begin
   SoundVelAlg := Medwin;
end.

(*

function OxySat(Temp,Sal : float64) : float64;
{from Application Note No. 13-1, rev B (April 1993) from SeaBird}
{units are ml/l}
var
   T : float64;
begin
   T := 0.01 * (Temp + 273.15);
   OxySat := exp(-173.4292 + 249.6339 / T + 143.3438 * ln(t) -21.8492 * t +
                 Sal * (-0.033096 + 0.014259 * T - 0.00170 * sqr(t)));
end;



function Salinity(Tm,Cn,Pr : float64) : float64;
const
   StdCondRdg = 42.9192;
var
   Temp,T0,T1,T2,T3,T4 : float64;
begin
{  Function returns salinity S from conductivity CN in mmho/cm, temperature TM in C, and pressure PR in decibars.
   Algorithm based on BASIC program S78TCRPI by D.W. Pritchard.  }
   If (Cn<=0) or (Cn>100) or (Tm<-5) or (Tm>100) or (Pr<-10) then begin
      Salinity := 0;
      exit;
    end;
   t0 := 1 + PR * ((3.989E-15 * PR - 6.37E-10) * PR + 0.0000207) /
      (1 + (0.00004464 * TM + 0.03426) * TM + (0.4215 - 0.003107 * TM) * CN
      / 42.9192);
   T1 := (1.0031E-9*TM - 6.9698E-7)*TM;
   T1 := 0.67661 + ((T1 + 1.10426E-4) * TM + 0.0200564) * TM;
   T2 := (TM - 15) / (1 + 0.0162 * (TM - 15));
   T3 := SQRT(ABS((CN/StdCondRdg) / (T0*T1)));
   if T3*T3 > 0.05 Then begin
      T4 := ((0.0636 - 0.0144 * T3) * T3 - 0.0375) * T3;
      T4 := T2 * (0.0005 + ((T4 - 0.0066) * T3 - 0.0056) * T3);
   end
   Else begin
      T4 := ((0.168176 * T3 - 0.338782) * T3 + 0.26076) * T3;
      T4 := T2 * ((T4 - 0.0958843) * T3 + 0.00573) * T3;
   end;
   Temp := ((2.7081 * T3 - 7.0261) * T3 + 14.0941) * T3;
   Salinity := 0.008 + ((Temp + 25.3851) * T3 - 0.1692) * T3 + T4;
end;
*)




