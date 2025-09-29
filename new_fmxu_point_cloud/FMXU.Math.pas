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
{    Supplemental Math routines                                        }
{                                                                      }
{**********************************************************************}
unit FMXU.Math;

interface

uses
  System.SysUtils, System.Math.Vectors;

type
  TMatrix3DHelper = record helper for TMatrix3D

    // System.Math.Vectors compatibility
    const Identity : TMatrix3D = (
      m11: 1; m12: 0; m13: 0; m14: 0;
      m21: 0; m22: 1; m23: 0; m24: 0;
      m31: 0; m32: 0; m33: 1; m34: 0;
      m41: 0; m42: 0; m43: 0; m44: 1;
    );

    function IsSameMatrix(const m : TMatrix3D) : Boolean;
    function ToString : String; inline;
  end;

function Vector3DToString(const aVector : TVector3D) : String;
function Matrix3DToString(const aMatrix : TMatrix3D) : String;

const
  cPoint3D_X : TPoint3D = (X: 1; Y: 0; Z: 0);
  cPoint3D_Y : TPoint3D = (X: 0; Y: 1; Z: 0);
  cPoint3D_Z : TPoint3D = (X: 0; Y: 0; Z: 1);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// Vector3DToString
//
function Vector3DToString(const aVector : TVector3D) : String;
begin
   Result := Format('[ %f %f %f %f ]', [ aVector.X, aVector.Y, aVector.Z, aVector.W ]);
end;

// Matrix3DToString
//
function Matrix3DToString(const aMatrix : TMatrix3D) : String;
begin
   Result := '[ ' + Vector3DToString(aMatrix.M[0])
                  + Vector3DToString(aMatrix.M[1])
                  + Vector3DToString(aMatrix.M[2])
                  + Vector3DToString(aMatrix.M[3]) + ' ]';
end;

// ------------------
// ------------------ TMatrix3DHelper ------------------
// ------------------

// IsSameMatrix
//
function TMatrix3DHelper.IsSameMatrix(const m : TMatrix3D) : Boolean;
type
  T8Int64 = array [ 0 .. 7 ] of Int64;
  P8Int64 = ^T8Int64;
var
  pSelf, pOther : P8Int64;
begin
  pSelf := P8Int64(@Self);
  pOther := P8Int64(@m);
  Result :=     (pSelf[0] = pOther[0]) and (pSelf[1] = pOther[1])
            and (pSelf[2] = pOther[2]) and (pSelf[3] = pOther[3])
            and (pSelf[4] = pOther[4]) and (pSelf[5] = pOther[5])
            and (pSelf[6] = pOther[6]) and (pSelf[7] = pOther[7]);
end;

// ToString
//
function TMatrix3DHelper.ToString : String;
begin
   Result := Matrix3DToString(Self);
end;

end.
