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
{    Scene related utilities                                           }
{                                                                      }
{**********************************************************************}
unit FMXU.Scene;

{$i fmxu.inc}

interface

uses
   System.Math, System.Math.Vectors, FMX.Types3D, FMX.Controls3D;

procedure MoveControl3DAroundTarget(
   anObject, aTarget : TControl3D;  // object to move and target around which to move
   pitchDelta, turnDelta : Single   // pitch & turn angles in degrees
   );
procedure AdjustDistanceToTarget(
   anObject, aTarget : TControl3D;  // object to move and target around which to move
   distanceScale : Single   // by how much to scale the distance to the target
   );

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

//  MoveControl3DAroundTarget
//
procedure MoveControl3DAroundTarget(
   anObject, aTarget : TControl3D;
   pitchDelta, turnDelta : Single   // pitch & turn angles in degrees
   );
begin
   // normalT2C points away from the direction the camera is looking
   var originalT2C := TPoint3D(anObject.AbsolutePosition - aTarget.AbsolutePosition);
   var normalT2C := originalT2C;
   var dist := normalT2C.Length;
   normalT2C := normalT2C / dist;

   var objectAbsoluteUp := TPoint3D(anObject.AbsoluteUp);

   // the camera is pitching around this axis.
   var normalCameraLeft := objectAbsoluteUp.CrossProduct(normalT2C);
   if normalCameraLeft.Length < 0.001 then
      normalCameraLeft := Point3D(-1, 0, 0) // arbitrary vector
   else normalCameraLeft := normalCameraLeft.Normalize;

   // calculate the current pitch.
   // 0 is looking down and PI is looking up
   var pitchNow := ArcCos(objectAbsoluteUp.DotProduct(normalT2C));
   pitchNow := EnsureRange(pitchNow+DegToRad(pitchDelta), 0+0.025, PI-0.025);

   // create a new vector pointing up and then rotate it down
   // into the new position
   normalT2C := objectAbsoluteUp;
   normalT2C := normalT2C.Rotate(normalCameraLeft, pitchNow);
   normalT2C := normalT2C.Rotate(objectAbsoluteUp, DegToRad(turnDelta));
   normalT2C := normalT2C * dist;

   var newPos := TPoint3D(anObject.AbsolutePosition) + (normalT2C - originalT2C);
   if anObject.Parent is TControl3D then
      newPos := TControl3D(anObject.Parent).AbsoluteToLocal3D(newPos);
   anObject.Position.Vector := newPos;
end;

// AdjustDistanceToTarget
//
procedure AdjustDistanceToTarget(
   anObject, aTarget : TControl3D;  // object to move and target around which to move
   distanceScale : Single   // by how much to scale the distance to the target
   );
var
  objectAbsolute, targetAbsolute : TPoint3D;
  originalTarget2Object, scaledTarget2Object, newPos : TPoint3D;
begin
  objectAbsolute := TPoint3D(anObject.AbsolutePosition);
  targetAbsolute := TPoint3D(aTarget.AbsolutePosition);
  originalTarget2Object := objectAbsolute - targetAbsolute;
  scaledTarget2Object := originalTarget2Object * distanceScale;
  newPos := scaledTarget2Object + targetAbsolute;
  if anObject.Parent is TControl3D then
     newPos := TControl3D(anObject.Parent).AbsoluteToLocal3D(newPos);
  anObject.Position.Vector := newPos;
end;

end.
