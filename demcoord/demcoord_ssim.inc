

procedure SSIM(DEM1,DEM2 : integer; gl1,gl2 : tGridLimits; var Luminance,Contrast,Structure : float64);

const
  MaxIntensity = 1;
  C1 = sqr(0.01 * MaxIntensity);
  C2 = sqr(0.03 * MaxIntensity);

var
  //Img1, Img2: TDoubleArray;
  //Mean1, Mean2, Var1, Var2, Cov, L, C, S: Double;
  //i : Integer;
  Moment1,Moment2 : tMomentVar;

  r,covar : float64;
begin
   Moment1 := DEMGlb[DEM1].ElevationMoments(gl1);
   Moment2 := DEMGlb[DEM2].ElevationMoments(gl2);

   if CovariancesFromTwoGrids(gl1, DEM1,DEM2, r,covar) then begin
      Luminance := Moment1.Mean / Moment2.Mean;
      Contrast := Moment1.svar / Moment2.svar;
      Structure := (Covar / Moment1.Npts) / (Moment1.svar * Moment2.svar);

      SSIM := ((2 * Moment1.Mean * Moment2.Mean + C1) * (2 * Covar + C2)) /
           ((Sqr(Moment1.Mean) + Sqr(Moment2.Mean) + C1) * (Moment1.svar + Moment2.svar + C2));



   end;

end.


(*
program SSIM;

//created by Bard, 27 Nov 2023  https://bard.google.com/chat/31239af044003953

{
  Calculate the structural similarity index (SSIM) between two images.
  The SSIM is a measure of image quality that considers luminance, contrast,
  and structure.

  @param img1 The first image.
  @param img2 The second image.
  @return The SSIM between the two images.
}

function ssim(img1: array of array of byte; img2: array of array of byte): real;
var
  mu1, mu2: real;
  sigma1^2, sigma2^2, sigma12: real;
  C1, C2: real;
  SSIM: real;

begin
  mu1 := mean(img1);
  mu2 := mean(img2);

  sigma1^2 := meanSquare(img1, mu1);
  sigma2^2 := meanSquare(img2, mu2);
  sigma12 := covariance(img1, img2, mu1, mu2);

  C1 := (0.01 * 255)^2;
  C2 := (0.03 * 255)^2;

  SSIM := (2 * mu1 * mu2 + C1) / (mu1^2 + mu2^2 + C1) *
         (2 * sigma12 + C2) / (sigma1^2 + sigma2^2 + C2);

end;

function mean(img: array of array of byte): real;
var
  sum: real;
  i, j: integer;

begin
  sum := 0;

  for i := 0 to high(img) do
  begin
    for j := 0 to high(img[i]) do
    begin
      sum := sum + img[i][j];
    end;
  end;

  mean := sum / (length(img) * length(img[0]));
end;

function meanSquare(img: array of array of byte; mu: real): real;
var
  sum: real;
  i, j: integer;

begin
  sum := 0;

  for i := 0 to high(img) do
  begin
    for j := 0 to high(img[i]) do
    begin
      sum := sum + square(img[i][j] - mu);
    end;
  end;

  meanSquare := sum / (length(img) * length(img[0]));
end;

function covariance(img1: array of array of byte; img2: array of array of byte; mu1: real; mu2: real): real;
var
  sum: real;
  i, j: integer;

begin
  sum := 0;

  for i := 0 to high(img1) do
  begin
    for j := 0 to high(img1[i]) do
    begin
      sum := sum + (img1[i][j] - mu1) * (img2[i][j] - mu2);
    end;
  end;

  covariance := sum / (length(img1) * length(img1[0]));
end;

function square(x: real): real;
begin
  square := x * x;
end;

begin
  writeln('SSIM:', ssim(img1, img2));
end.
Use code with caution. Learn more
content_copy
This code will calculate the SSIM between two images and print the result to the console. To use the code, you will need to replace img1 and img2 with the actual images you want to compare. The images should be represented as two-dimensional arrays of bytes, where each byte represents the intensity of a pixel.
I hope this helps!


==================================================================



Bing 1, 12/14/2023




Here is an example of Pascal code for calculating the Structural Similarity Index (SSIM) between two images:

```
program SSIM;

uses
  Math;

type
  TDoubleArray = array of Double;

function Mean(Arr: TDoubleArray): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr) do
    Result := Result + Arr[i];
  Result := Result / Length(Arr);
end;

function Variance(Arr: TDoubleArray; Mean: Double): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr) do
    Result := Result + Sqr(Arr[i] - Mean);
  Result := Result / Length(Arr);
end;

function Covariance(Arr1, Arr2: TDoubleArray; Mean1, Mean2: Double): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr1) do
    Result := Result + (Arr1[i] - Mean1) * (Arr2[i] - Mean2);
  Result := Result / Length(Arr1);
end;

function SSIM(Arr1, Arr2: TDoubleArray): Double;
const
  C1 = 0.01 * 255 * 0.01 * 255;
  C2 = 0.03 * 255 * 0.03 * 255;
var
  Mean1, Mean2, Var1, Var2, Cov, SSIM: Double;
begin
  Mean1 := Mean(Arr1);
  Mean2 := Mean(Arr2);
  Var1 := Variance(Arr1, Mean1);
  Var2 := Variance(Arr2, Mean2);
  Cov := Covariance(Arr1, Arr2, Mean1, Mean2);
  SSIM := ((2 * Mean1 * Mean2 + C1) * (2 * Cov + C2)) /
    ((Sqr(Mean1) + Sqr(Mean2) + C1) * (Var1 + Var2 + C2));
  Result := SSIM;
end;

var
  Img1, Img2: TDoubleArray;
  i: Integer;
begin
  SetLength(Img1, 10);
  SetLength(Img2, 10);
  for i := 0 to 9 do
  begin
    Img1[i] := Random;
    Img2[i] := Random;
  end;
  WriteLn(SSIM(Img1, Img2));
end.
```

This code calculates the SSIM between two arrays of doubles, which represent the pixel intensities of two images.
The SSIM is a measure of the structural similarity between the two images,
taking into account luminance, contrast, and structure �.
The code uses the mean, variance, and covariance of the pixel intensities to calculate the SSIM �.

I hope this helps!

Source: Conversation with Bing, 12/14/2023
(1) Structural similarity index � skimage 0.22.0 documentation - scikit-image. https://scikit-image.org/docs/stable/auto_examples/transform/plot_ssim.html.
(2) structural-similarity-index � GitHub Topics � GitHub. https://github.com/topics/structural-similarity-index.
(3) Correlation Maximized Structural Similarity Loss for Semantic Segmentation. https://arxiv.org/pdf/1910.08711v1.pdf.
(4) An Overview Of Structural Similarity - GameDev Academy. https://gamedevacademy.org/structural-similarity-tutorial/.
(5) Zhou Wang - University of Waterloo. https://ece.uwaterloo.ca/~z70wang/research/ssim/.
(6) undefined. https://www.youtube.com/watch?v=-i3NQ-by2b8.





Bing 2

Yes, it is possible to compute the luminance, contrast, and structure separately in SSIM using Pascal. The code I provided earlier calculates the SSIM between two arrays of doubles, which represent the pixel intensities of two images. To compute the luminance, contrast, and structure separately, you can modify the code to calculate the mean, variance, and covariance of the pixel intensities for each component separately. Here is an example of how to do this:

```
function Luminance(Arr: TDoubleArray): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr) do
    Result := Result + Arr[i];
  Result := Result / Length(Arr);
end;

function Contrast(Arr: TDoubleArray; Mean: Double): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Arr) do
    Result := Result + Sqr(Arr[i] - Mean);
  Result := Sqrt(Result / Length(Arr));
end;


function Structure(Arr1, Arr2: TDoubleArray; Mean1, Mean2: Double): Double;
var
  i: Integer;
  Cov: Double;
begin
  Cov := 0;
  for i := 0 to High(Arr1) do
    Cov := Cov + (Arr1[i] - Mean1) * (Arr2[i] - Mean2);
  Result := (Cov / Length(Arr1)) / (Variance(Arr1, Mean1) * Variance(Arr2, Mean2));
end;


var
  Img1, Img2: TDoubleArray;
  Mean1, Mean2, Var1, Var2, Cov, L, C, S: Double;
  i: Integer;
begin
  SetLength(Img1, 10);
  SetLength(Img2, 10);
  for i := 0 to 9 do
  begin
    Img1[i] := Random;
    Img2[i] := Random;
  end;
  Mean1 := Mean(Img1);
  Mean2 := Mean(Img2);
  Var1 := Variance(Img1, Mean1);
  Var2 := Variance(Img2, Mean2);
  Cov := Covariance(Img1, Img2, Mean1, Mean2);
  L := Luminance(Img1) / Luminance(Img2);
  C := Contrast(Img1, Mean1) / Contrast(Img2, Mean2);
  S := Structure(Img1, Img2, Mean1, Mean2);
  WriteLn('Luminance: ', L:0.000);
  WriteLn('Contrast: ', C:0.000);
  WriteLn('Structure: ', S:0.000);
end.
```





This code calculates the luminance, contrast, and structure components of the SSIM between two arrays of doubles,
which represent the pixel intensities of two images.
The `Luminance` function calculates the mean pixel intensity,
the `Contrast` function calculates the standard deviation of the pixel intensities, and
the `Structure` function calculates the covariance of the pixel intensities �.
The `Luminance`, `Contrast`, and `Structure` values can be used to compute the SSIM using the same formula as before �.

I hope this helps!

Source: Conversation with Bing, 12/14/2023
(1) Structural similarity - Wikipedia. https://en.wikipedia.org/wiki/Structural_similarity.
(2) Structural similarity (SSIM) index for measuring image quality - MathWorks. https://www.mathworks.com/help/images/ref/ssim.html.
(3) [2006.13846] Understanding SSIM - arXiv.org. https://arxiv.org/abs/2006.13846.






*)
