{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 2.0 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    This unit holds color-related utilities                           }
{                                                                      }
{**********************************************************************}
unit FMXU.Colors;

{$i fmxu.inc}

interface

uses System.UITypes, System.Math.Vectors;

function ColorComposeAlpha(const color : TAlphaColor; const aOpacity : Single) : TAlphaColor;
function ColorPremultiplyAlpha(const color : TAlphaColor; const aOpacity : Single) : TAlphaColor;

function ColorToVector3D(const color : TAlphaColor) : TVector3D;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ColorComposeAlpha
//
function ColorComposeAlpha(const color : TAlphaColor; const aOpacity : Single) : TAlphaColor;
begin
   Result := color;
   if aOpacity < 1 then
      TAlphaColorRec(Result).A := Trunc(TAlphaColorRec(color).A * aOpacity);
end;

// ColorPremultiplyAlpha
//
function ColorPremultiplyAlpha(const color : TAlphaColor; const aOpacity : Single) : TAlphaColor;
begin
   if aOpacity < 1 then begin
      TAlphaColorRec(Result).R := Trunc(TAlphaColorRec(color).R * aOpacity);
      TAlphaColorRec(Result).G := Trunc(TAlphaColorRec(color).G * aOpacity);
      TAlphaColorRec(Result).B := Trunc(TAlphaColorRec(color).B * aOpacity);
      TAlphaColorRec(Result).A := Trunc(TAlphaColorRec(color).A * aOpacity);
   end else Result := color;
end;

// ColorToVector3D
//
function ColorToVector3D(const color : TAlphaColor) : TVector3D;
begin
   var f : Single := 1/255;
   Result.V[0] := TAlphaColorRec(color).R * f;
   Result.V[1] := TAlphaColorRec(color).G * f;
   Result.V[2] := TAlphaColorRec(color).B * f;
   Result.V[3] := TAlphaColorRec(color).A * f;
end;

end.
