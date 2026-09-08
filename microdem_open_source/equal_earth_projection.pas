unit Equal_Earth_projection;

//Chrome search, AI mode,  4 Sept 2026


interface

uses 
Math;

const
{ Equal Earth polynomial coefficients from the original Šavrič et al. paper }
A1 = 1.340264;
A2 = -0.081106;
A3 = 0.000893;
A4 = 0.003796;

{ Mathematical constants }
SQRT3 = 1.73205080756887729;

type
TPoint2D = record
   X, Y: Double;
end;

TGeoCoord = record
   Lon, Lat: Double; { In Radians }
end;

{ Forward Projection: Converts Lon/Lat (radians) to Cartesian Map X/Y }
function EqualEarthForward(const Geo: TGeoCoord; const Lon0: Double; const R: Double): TPoint2D;

{ Inverse Projection: Converts Cartesian Map X/Y back to Lon/Lat (radians) }
function EqualEarthInverse(const Pt: TPoint2D; const Lon0: Double; const R: Double): TGeoCoord;

implementation

function EqualEarthForward(const Geo: TGeoCoord; const Lon0: Double; const R: Double): TPoint2D;
var
DeltaLon, SinTheta, Theta, Theta2, Theta6: Double;
Denominator: Double;
begin
DeltaLon := Geo.Lon - Lon0;

{ 1. Calculate parametric latitude (Theta) }
SinTheta := (SQRT3 / 2.0) * Sin(Geo.Lat);

{ Clamp SinTheta to avoid floating point errors at the poles }
if SinTheta > 1.0 then SinTheta := 1.0
else if SinTheta < -1.0 then SinTheta := -1.0;

Theta := ArcSin(SinTheta);

{ Pre-calculate powers of Theta }
Theta2 := Theta * Theta;
Theta6 := Theta2 * Theta2 * Theta2;

{ 2. Compute Projected Coordinates }
Result.Y := R * Theta * (A1 + A2 * Theta2 + Theta6 * (A3 + A4 * Theta2));

Denominator := SQRT3 * (A1 + 3.0 * A2 * Theta2 + Theta6 * (7.0 * A3 + 9.0 * A4 * Theta2));
Result.X := (2.0 * R * DeltaLon * Cos(Theta)) / Denominator;
end;

function EqualEarthInverse(const Pt: TPoint2D; const Lon0: Double; const R: Double): TGeoCoord;
var
Theta, Theta2, Theta6, PrevTheta: Double;
f, fPrime, DeltaTheta: Double;
Iter: Integer;
SinLat: Double;
const
MAX_ITER = 100;
TOLERANCE = 1e-11;
begin
{ 1. Newton-Raphson iteration to find the parametric latitude (Theta) from Y }
{ Initial guess }
Theta := Pt.Y / R;

for Iter := 1 to MAX_ITER do
begin
Theta2 := Theta * Theta;
Theta6 := Theta2 * Theta2 * Theta2;

{ Compute function f(Theta) = Y_calculated - Y_target }
f := R * Theta * (A1 + A2 * Theta2 + Theta6 * (A3 + A4 * Theta2)) - Pt.Y;

{ Compute derivative f'(Theta) }
fPrime := R * (A1 + 3.0 * A2 * Theta2 + Theta6 * (7.0 * A3 + 9.0 * A4 * Theta2));

if Abs(fPrime) < 1e-14 then 
Break;

DeltaTheta := f / fPrime;
PrevTheta := Theta;
Theta := Theta - DeltaTheta;

{ Check convergence }
if Abs(Theta - PrevTheta) < TOLERANCE then
Break;
end;

{ 2. Compute Geodetic Latitude from Theta }
SinLat := (2.0 / SQRT3) * Sin(Theta);
if SinLat > 1.0 then SinLat := 1.0
else if SinLat < -1.0 then SinLat := -1.0;
Result.Lat := ArcSin(SinLat);

{ 3. Compute Geodetic Longitude from X and Theta }
Theta2 := Theta * Theta;
Theta6 := Theta2 * Theta2 * Theta2;

Result.Lon := Lon0 + (Pt.X * SQRT3 * (A1 + 3.0 * A2 * Theta2 + Theta6 * (7.0 * A3 + 9.0 * A4 * Theta2))) / (2.0 * R * Cos(Theta));
end;

end.

