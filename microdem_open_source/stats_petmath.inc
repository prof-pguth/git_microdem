
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}

//include file for petmath



//  https://stackoverflow.com/questions/50394952/code-for-chi-square-distribution-function-in-delphi


         function LnGamma(x : float64) : float64;
         const
           a0 =  0.083333333096;
           a1 = -0.002777655457;
           a2 =  0.000777830670;
           c  =  0.918938533205;
         var
           r : float64;
         begin
           r := (a0 + (a1 + a2 / sqr(x)) / sqr(x)) / x;
           Result := (x - 0.5) * ln(x) - x + c + r;
         end;

         function LnFak(x : float64) : float64;
         begin
            Result := LnGamma(x + 1);
         end;

         function Reihe(chi : float64; f : float64) : float64;
         const
            MaxError = 0.0001;
         var
           chi2,Summe,Summand : float64;
           k    : longint;
         begin
           Summe := 1;
           k := 1;
           Summand := 1;
           chi2 := sqr(chi);
           repeat
             Summand := Summand * chi2 / (f + 2 * k);
             Summe := Summe + Summand;
             k := succ(k);
           until (Summand < MaxError);
           Result := Summe;
         end;

         function IntegralChi(chisqr : float64; f : longint) : float64;
         var
           s : float64;
         begin
           S := power((0.5 * chisqr), f/2) * Reihe(sqrt(chisqr), f) * exp((-chisqr/2) - LnGamma((f + 2) / 2));
           Result := 1 - s;
         end;

{==============================================================================================================================}



//http://delphiforfun.org/programs/delphi_techniques/ChiSquaredRNGTesting.htm   downloaded 9/3/2022
//unit U_ChiSquare_RNG_Testing;
{Copyright © 2013, Gary Darby,  www.DelphiForFun.org
 This program may be used or modified for any non-commercial purpose
 so long as this original notice remains in place.
 All other rights are reserved
 }

 const
  nbins=10;
  npts=2000;
TYPE
   barray = ARRAY [1..10] of extended;
   bExtArray = array of extended;
   //bint64array = array of int64;
VAR
   glinext,glinextp : integer;
   glma : ARRAY [1..55] OF extended;
   chsq, chsq64, df, prob, prob64, x : extended;
   ibin,idum : integer;

{******************************************************}
{      "Numerical Recipes in Pascal" Includes          }
{******************************************************}

FUNCTION gammln(xx: extended): float64;
//PLG note: p.157,p.704 "Numerical Recipes in Pascal"   retruns natural log of Gamma function
CONST
   stp = 2.50662827465;
   half = 0.5;
   one = 1.0;
   fpf = 5.5;
VAR
   x,tmp,ser: float64;
   j: integer;
   cof: ARRAY [1..6] OF float64;
BEGIN
   cof[1] := 76.18009173;
   cof[2] := -86.50532033;
   cof[3] := 24.01409822;
   cof[4] := -1.231739516;
   cof[5] := 0.120858003e-2;
   cof[6] := -0.536382e-5;
   x := xx-one;
   tmp := x+fpf;
   tmp := (x+half)*ln(tmp)-tmp;
   ser := one;
   FOR j := 1 TO 6 DO BEGIN
      x := x+one;
      ser := ser+cof[j]/x
   END;
   //gammln := sngl(tmp+ln(stp*ser))
   gammln := tmp+ln(stp*ser);
END;

PROCEDURE gcf(a,x: float64; VAR gammcf,gln: float64);
//PLG note: p.162,p.706 "Numerical Recipes in Pascal"
LABEL 1;
CONST
   itmax=100;
   eps=3.0e-7;
VAR
   n: integer;
   gold,g,fac,b1,b0,anf,ana,an,a1,a0: float64;
BEGIN
   gln := gammln(a);
   gold := 0.0;
   a0 := 1.0;
   a1 := x;
   b0 := 0.0;
   b1 := 1.0;
   fac := 1.0;
   g:=0.0; {GDD - ensure g is initialized}
   FOR n := 1 TO itmax DO BEGIN
      an := 1.0*n;
      ana := an-a;
      a0 := (a1+a0*ana)*fac;
      b0 := (b1+b0*ana)*fac;
      anf := an*fac;
      a1 := x*a0+anf*a1;
      b1 := x*b0+anf*b1;
      IF (a1 <> 0.0) THEN BEGIN
         fac := 1.0/a1;
         g := b1*fac;
         IF (abs((g-gold)/g) < eps) THEN GOTO 1;
         gold := g
      END
   END;
//   showmessage('pause in GCF - a too large, itmax too small'); //readln;                                                      *)adln;
1:   gammcf := exp(-x+a*ln(x)-gln)*g
END;


PROCEDURE gser(a,x: float64; VAR gamser,gln: float64);
//PLG note: p.162,p.706 "Numerical Recipes in Pascal"
LABEL 1;
CONST
   itmax=100;
   eps=3.0e-7;
VAR
   n: integer;
   sum,del,ap: float64;
BEGIN
   gln := gammln(a);
   IF (x <= 0.0) THEN BEGIN
      IF (x < 0.0) THEN BEGIN
         writeln('pause in GSER - x less than 0'); readln
      END;
      gamser := 0.0
   END ELSE BEGIN
      ap := a;
      sum := 1.0/a;
      del := sum;
      FOR n := 1 TO itmax DO BEGIN
         ap := ap+1.0;
         del := del*x/ap;
         sum := sum+del;
         IF (abs(del) < abs(sum)*eps) THEN GOTO 1
      END;
      //writeln('pause in GSER - a too large, itmax too small'); readln;
1:      gamser := sum*exp(-x+a*ln(x)-gln)
   END
END;


FUNCTION gammq(a,x: float64) : float64;
//PLG note: p.162,p.706 "Numerical Recipes in Pascal"
VAR
   gamser,gln: float64;
BEGIN
   IF ((x < 0.0) OR (a <= 0.0)) THEN BEGIN
      //showmessage('Pause in GAMMQ - invalid arguments');
   END;
   IF (x < a+1.0) THEN BEGIN
      gser(a,x,gamser,gln);
      gammq := 1.0-gamser;
   END
   ELSE BEGIN
      gcf(a,x,gamser,gln);
      gammq := gamser;
   END
END;

PROCEDURE chsone(bins,ebins: barray; nbins,knstrn: integer; VAR df,chsq,prob: float64 );
//PLG note: p.471, "Numerical Recipes in Pascal"
VAR
   j: integer;
BEGIN
   df := nbins-1-knstrn;
   chsq := 0.0;
   FOR j := 1 TO nbins DO BEGIN
      IF (ebins[j] <= 0.0) THEN BEGIN
         //writeln('pause in CHSONE - bad expected number')
      END;
      chsq := chsq+sqr(bins[j]-ebins[j])/ebins[j];
   END;
   prob := gammq(0.5*df,0.5*chsq);
END;


PROCEDURE chsoneExt(bins,ebins: bExtArray; nbins,knstrn: integer;  VAR df,chsq,prob: float64);
//PLG note: p.472, "Numerical Recipes in Pascal", where it is called chstwo
VAR
   j: integer;
BEGIN
   df := nbins-1-knstrn;
   chsq := 0.0;
   FOR j := low(bins) TO high(bins) do BEGIN
      IF (ebins[j] <= 0.0) THEN BEGIN
         //showmessage('Pause in CHSONE - bad expected number')
      END;
      chsq := chsq+sqr(bins[j]-ebins[j])/ebins[j];
   END;
   prob := gammq(0.5*df,0.5*chsq);
END;



{******************************************************}
{  End of "Numerical Recipes in Pascal" Includes       }
{******************************************************}

{==============================================================================================================================}

function Friedman(DBonTable : integer; DEMs : tStringList; Alpha : float64 = 95) : boolean;
var
   cf,sumr2,sumRi2,Rj,FriedmanVal,Rank,CritVal : float64;
   k,N,DEM : integer;
   R : array[1..10] of float32;
begin
   N := GISdb[DBonTable].MyData.FiltRecsInDB;
   k := DEMs.Count;
   CF := N * k * (k+1) * (k+1) / 4;

   sumr2 := 0;
   sumRi2 := 0;
   for DEM := 0 to pred(k) do  R[succ(DEM)] := 0;
      //R[succ(DEM)] := GISdb[DBonTable].MyData.FieldSum(DEMs[DEM] + '_SCR');
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin //was i loop
      for DEM := 0 to pred(k) do begin   //was j loop
         Rank := GISdb[DBonTable].MyData.FieldSum(DEMs[DEM] + '_SCR');
         R[succ(DEM)] := R[succ(DEM)] + Rank;
         sumr2 := sumr2  + sqr(Rank);
         sumRi2 := sumRi2 + sqr(R[succ(DEM)]);
      end;
      GISdb[DBonTable].MyData.next;
   end;

(*
%Compute the sum of rankings, and the sum of the squares of the rankings
sumr2=0;sumRi2=0;R=zeros(1,k);
for j=1:k   //dems loop
    R(j)=0;
    for i=1:N   //opinions loop
        R(j)=R(j)+rij(i,j);
        sumr2=sumr2+rij(i,j)*rij(i,j);
    end
    sumRi2=sumRi2+R(j)*R(j);
end;
*)


   FriedmanVal := (k-1) * (sumRi2 - N * CF) / (sumr2-CF);

//from scipy.stats.distributions import chi2
//chi2.ppf(0.975, df=2)

   critVal := IntegralChi(ALPHA,k-1);
   critVal := gammq(0.95,k-1);
   critVal := gammq(0.05,k-1);
   critVal := gammq(95,k-1);

   Result := FriedmanVal > critVal;
end;

//adaptation of Matlab code from Dr. Ing. Carlos López-Vázquez

(*
function [H,FriedmanVal,critVal,R]=friedman(rij,k,N,ALPHA);
%Inputs:
%        rij: we assume that rij is a matrix of k columns and N rows, with suitable rankings
%             according to the mid-rank rule. Thus, every row should add to a constant (k+1)*k/2
%          k: number of wines under consideration. Should match with the number of columns of rij
%          N: number of opinions. Should match with the number of rows of rij
%      ALPHA: confidence level. Depending on the routine at hand, this should be 0.95 or 95 or alike
%
%Outputs:
%          H: logical output, with two alternatives:
%             H = 0 => Do not reject the null hypothesis at significance level ALPHA.
%             H = 1 => Reject the null hypothesis at significance level ALPHA.
%FriedmanVal: Numerical value of the Friedman statistic
%    critVal: Critical value to compare
%          R: Sum of rankings along columns. Required by the Bonferroni-Dunn code
%
%Note: there is a numerical example at the end

%By CLV, 20220901

%Calculate some constant
CF=N*k*(k+1)*(k+1)/4;

%Compute the sum of rankings, and the sum of the squares of the rankings
sumr2=0;sumRi2=0;R=zeros(1,k);
for j=1:k   //dems loop
    R(j)=0;
    for i=1:N   //opinions loop
        R(j)=R(j)+rij(i,j);
        sumr2=sumr2+rij(i,j)*rij(i,j);
    end
    sumRi2=sumRi2+R(j)*R(j);
end;

%Compute the Friedman statistics
FriedmanVal=(k-1)*(sumRi2-N*CF)/(sumr2-CF);

%If N is large enough,this will be accurate. Otherwise, specialized tables will be required.
%Hint:
%  For k=3, "large enough" is N>149
%  For k=4, "large enough" is N>123
%  For k=5, "large enough" is N>110
%  For k=6, "large enough" is N>103
critVal=chi2inv(ALPHA,k-1);

%Finally, analyze if the Friedman statistics it is larger or not to the critical value
if FriedmanVal>critVal
    H=1;
else
    H=0;
end
return

%The numerical example
rij=[1.00	2.00	3.00
    2.00	1.00	3.00
    1.50	1.50	3.00
    2.00	1.00	3.00
    1.00	2.00	3.00
    2.00	1.00	3.00
    2.00	1.00	3.00
    2.00	1.00	3.00];
k=3;N=8;ALPHA=0.95;
[H,a,b,R]=friedman(rij,k,N,ALPHA)
H =
          1.00    //boolean result
a =
         12.97    //Friedman value
b =
          5.99   //critical value
R =
         13.50         10.50         24.00   //sum of rankings along columns

[YesNoTable,critVal]=BonferroniDunn(R,k,N,ALPHA)
YesNoTable =
         -1.00             0          1.00
         -1.00         -1.00          1.00
         -1.00         -1.00         -1.00
critVal =
          4.01

================================================================================================================



function [YesNoTable,critVal]=BonferroniDunn(R,k,N,ALPHA);
%Inputs:
%          R: sum of the rankings as computed by the Friedman routine
%          k: number of wines under consideration. Should match with the number of columns of rij
%          N: number of opinions. Should match with the number of rows of rij
%      ALPHA: confidence level. Depending on the routine you have at hand, this should be 0.95 or
%             95, or alike (0.99, etc.)
%
%Outputs:
% YesNoTable: square matrix of size k, each entry with three alternatives:
%               -1 Ignore this element
%                0 => Do not reject the null hypothesis at significance level ALPHA. This imply a tie.
%                1 => Reject the null hypothesis at significance level ALPHA.
%    critVal: Critical value used to compare

%By CLV, 20220901

%Calculate some constant
Quantile=1-ALPHA/k/(k-1);

%Compute the critical value to assess if there are significant discrepancies
zi=norminv(Quantile);%Inverse normal distribution of zero mean, unit variance
critVal=zi*sqrt(N*k*(k+1)/6);

%Compute the answer
for i=1:k
    for j=1:i
        YesNoTable(i,j)=-1;%Fill with non-sense values
    end
    for j=(i+1):k %Only the upper triangle is filled
        if abs(R(i)-R(j))>critVal
            YesNoTable(i,j)=1;
        else
            YesNoTable(i,j)=0;
        end
    end
end


*)

