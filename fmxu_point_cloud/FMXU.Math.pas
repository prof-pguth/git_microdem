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

    function SameMatrix(const m : TMatrix3D) : Boolean;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TMatrix3DHelper ------------------
// ------------------

// SameMatrix
//
function TMatrix3DHelper.SameMatrix(const m : TMatrix3D) : Boolean;
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

end.
