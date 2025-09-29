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
{    Implements "interposer classes" that for TForm & TViewport3D      }
{    - low-level paint timing                                          }
{    - before/after paint events                                       }
{    Place this unit as last of the interface uses clause              }
{                                                                      }
{**********************************************************************}
unit FMXU.Viewport3D;

{$i fmxu.inc}

interface

uses
   System.Classes, System.Diagnostics, System.Types, System.UITypes,
   System.Generics.Collections,
   FMX.Viewport3D, FMX.Forms, FMX.Forms3D, FMX.Controls3D, FMX.Objects3D,
   FMX.Types3D;

type
  TList_of_TControl3D = TList<TControl3D>;

   TForm = class (FMX.Forms.TForm)
      private
         FLastPaintSeconds : Single;
         FOnBeforePaint : TNotifyEvent;
         FOnAfterPaint : TNotifyEvent;

      protected
          procedure PaintRects(const UpdateRects: array of TRectF); override;

      public
         // Number of seconds the last Paint took - hopefully fractions of seconds :)
         property LastPaintSeconds : Single read FLastPaintSeconds;

         property OnBeforePaint : TNotifyEvent read FOnBeforePaint write FOnBeforePaint;
         property OnAfterPaint : TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
   end;

   TViewport3D = class (FMX.Viewport3D.TViewport3D)
      private
         FLastPaintSeconds : Single;
         FOnBeforePaint : TNotifyEvent;
         FOnAfterPaint : TNotifyEvent;
         FOnCustomPaint : TNotifyEvent;

      protected
         function GetDesignCamera : TCamera;
         function GetCurrentCamera : TCamera;
         function GetRenderingList : TList_of_TControl3D;

         procedure Paint; override;
         procedure AfterPaint; override;

         property OnCustomPaint : TNotifyEvent read FOnCustomPaint write FOnCustomPaint;

      public
         // Number of seconds the last Paint took - hopefully fractions of seconds :)
         property LastPaintSeconds : Single read FLastPaintSeconds;

         property OnBeforePaint : TNotifyEvent read FOnBeforePaint write FOnBeforePaint;
         property OnAfterPaint : TNotifyEvent read FOnAfterPaint write FOnAfterPaint;

         procedure MoveCameraAroundTarget(pitchDelta, turnDelta : Single);
   end;

   TForm3D = class (FMX.Forms3D.TForm3D)
      private
         FLastPaintSeconds : Single;
         FOnBeforePaint : TNotifyEvent;
         FOnAfterPaint : TNotifyEvent;

      protected
          procedure PaintRects(const UpdateRects: array of TRectF); override;

      public
         // Number of seconds the last Paint took - hopefully fractions of seconds :)
         property LastPaintSeconds : Single read FLastPaintSeconds;

         property OnBeforePaint : TNotifyEvent read FOnBeforePaint write FOnBeforePaint;
         property OnAfterPaint : TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses FMXU.Scene;

// ------------------
// ------------------ TForm ------------------
// ------------------

// PaintRects
//
procedure TForm.PaintRects(const UpdateRects: array of TRectF);
begin
   var stopWatch := TStopwatch.StartNew;
   if Assigned(FOnBeforePaint) then
      FOnBeforePaint(Self);
   inherited;
   if Assigned(FOnAfterPaint) then
      FOnAfterPaint(Self);
   FLastPaintSeconds := stopWatch.ElapsedTicks / stopWatch.Frequency;
end;

// ------------------
// ------------------ TViewport3D ------------------
// ------------------

type
  TViewPort3DFields = record
     FCamera: TCamera;
     FDesignCamera: TCamera;
     FDesignCameraZ: TDummy;
     FDesignCameraX: TDummy;
     FFill: TAlphaColor;
     FMultisample: TMultisample;
     FUsingDesignCamera: Boolean;
     FDrawing: Boolean;
     FRenderingList: TList<TControl3D>;
  end;
  PViewPort3DFields = ^TViewPort3DFields;

// Paint
//
procedure TViewport3D.Paint;
begin
   var stopWatch := TStopwatch.StartNew;
   if Assigned(FOnBeforePaint) then
      FOnBeforePaint(Self);
   if Assigned(FOnCustomPaint) then
      FOnCustomPaint(Self)
   else inherited Paint;
   FLastPaintSeconds := stopWatch.ElapsedTicks / stopWatch.Frequency;
end;

// AfterPaint
//
procedure TViewport3D.AfterPaint;
begin
   inherited;
   if Assigned(FOnAfterPaint) then
      FOnAfterPaint(Self);
end;

// MoveCameraAroundTarget
//
procedure TViewport3D.MoveCameraAroundTarget(pitchDelta, turnDelta : Single);
begin
   if (Camera <> nil) and (Camera.Target <> nil) then
      MoveControl3DAroundTarget(Camera, Camera.Target, pitchDelta, turnDelta);
end;

// GetDesignCamera
//
function TViewport3D.GetDesignCamera : TCamera;
var
  fieldsPtr : PViewPort3DFields;
begin
  fieldsPtr := PViewPort3DFields(@Camera);
  Result := fieldsPtr.FDesignCamera;
  Assert(Result.ClassType = TCamera);
end;

// GetCurrentCamera
//
function TViewport3D.GetCurrentCamera: TCamera;
begin
  if (Camera = nil) or UsingDesignCamera then
    Result := GetDesignCamera
  else Result := Camera;
end;

// GetRenderingList
//
function TViewport3D.GetRenderingList : TList<TControl3D>;
var
  fieldsPtr : PViewPort3DFields;
begin
  fieldsPtr := PViewPort3DFields(@Camera);
  Result := fieldsPtr.FRenderingList;
  Assert(Result.ClassType = TList<TControl3D>);
end;

// ------------------
// ------------------ TForm3D ------------------
// ------------------

// PaintRects
//
procedure TForm3D.PaintRects(const UpdateRects: array of TRectF);
begin
   var stopWatch := TStopwatch.StartNew;
   if Assigned(FOnBeforePaint) then
      FOnBeforePaint(Self);
   inherited;
   if Assigned(FOnAfterPaint) then
      FOnAfterPaint(Self);
   FLastPaintSeconds := stopWatch.ElapsedTicks / stopWatch.Frequency;
end;

end.
