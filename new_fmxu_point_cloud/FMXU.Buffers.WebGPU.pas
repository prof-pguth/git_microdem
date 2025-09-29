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
{    WebGPU hardware buffers                                           }
{                                                                      }
{**********************************************************************}
unit FMXU.Buffers.WebGPU;

{$i fmxu.inc}

interface

uses
   System.Classes,
   WebGPU, WebGPU.Interfaces,
   FMX.Types3D,
   FMXU.Buffers, FMXU.WebGPU.Utils;

type
   TGPUVertexBufferWebGPU = class (TGPUVertexBuffer)
      protected
         FMapped : IWebGPUMappedBuffer;

         procedure DoLock(context : TContext3D); override;
         procedure DoUnlock(context : TContext3D); override;

      public
         destructor Destroy; override;
   end;

   TGPUIndexBufferWebGPU = class (TGPUIndexBuffer)
      protected
         FMapped : IWebGPUMappedBuffer;

         procedure DoLock(context : TContext3D); override;
         procedure DoUnlock(context : TContext3D); override;

      public
         destructor Destroy; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses FMXU.Context, FMXU.Context.WebGPU;

type
   TVertexBufferCracker = class (TPersistent)
      protected
         FBuffer: Pointer;
   end;

   TIndexBufferCracker = class (TPersistent)
      protected
         FBuffer: Pointer;
   end;

// ------------------
// ------------------ TGPUVertexBufferWebGPU ------------------
// ------------------

// DoLock
//
procedure TGPUVertexBufferWebGPU.DoLock(context : TContext3D);
var
   buffer : IWGPUBuffer;
begin
   Assert(context <> nil);
   case BufferType of

      gpubtDynamic : begin

         Assert(False, 'not working :(');
         // for dynamic buffers, we map straight to WebGPU buffer
         var fmxuContext := (context as TFMXUContext3D_WebGPU);

         if FGPUBuffer = nil then begin
            // first access, allocate buffer
            buffer := fmxuContext.CreateBuffer(
               FVertexBuffer.Size, WGPUBufferUsage_CopyDst or WGPUBufferUsage_Vertex,
               'GPUVertexBuffer'
            );
            FGPUBuffer := buffer;
            // TODO: this is clumsy, the VB memory was allocated for nothing
            FreeMem(TVertexBufferCracker(FVertexBuffer).FBuffer);
         end else begin
            buffer := IWGPUBuffer(FGPUBuffer);
         end;
         FMapped := TIWebGPUMappedBuffer.Create(buffer, WGPUMapMode_Read or WGPUMapMode_Write, 0, FVertexBuffer.Size);

         TVertexBufferCracker(FVertexBuffer).FBuffer := FMapped.Data;

      end;

      gpubtStatic : begin

         // for static buffers, the content is copied on unlock in an immutable buffer
         FGPUBuffer := nil;

      end;
   else
      Assert(False);
   end;
end;

// DoUnlock
//
procedure TGPUVertexBufferWebGPU.DoUnlock(context : TContext3D);
var
   buffer : IWGPUBuffer;
begin
   var fmxuContext := (context as TFMXUContext3D_WebGPU);

   case BufferType of

      gpubtDynamic : begin

         Assert(
                (TVertexBufferCracker(FVertexBuffer).FBuffer = FMapped.Data)
            and (Cardinal(FVertexBuffer.Size) = FMapped.Size),
            'When using a TGPUVertexBuffer resizing or reallocating is NOT supported'
         );
         FMapped := nil;

      end;

      gpubtStatic : begin

         buffer := fmxuContext.CreateBufferFromData(
               FVertexBuffer.Size, WGPUBufferUsage_CopyDst or WGPUBufferUsage_Vertex,
               TVertexBufferCracker(FVertexBuffer).FBuffer,
               'GPUVertexBuffer'
            );
         FGPUBuffer := buffer;

      end;

   else
      Assert(False);
   end;
end;

// Destroy
//
destructor TGPUVertexBufferWebGPU.Destroy;
begin
   if BufferType = gpubtDynamic then
      TVertexBufferCracker(FVertexBuffer).FBuffer := nil;

   inherited;
end;

// ------------------
// ------------------ TGPUIndexBufferWebGPU ------------------
// ------------------

// DoLock
//
procedure TGPUIndexBufferWebGPU.DoLock(context : TContext3D);
var
   buffer : IWGPUBuffer;
begin
   Assert(context <> nil);
   case BufferType of

      gpubtDynamic : begin

         Assert(False, 'not working :(');

         // for dynamic buffers, we map straight to WebGPU buffer
         var fmxuContext := (context as TFMXUContext3D_WebGPU);

         if FGPUBuffer = nil then begin
            // first access, allocate buffer
            buffer := fmxuContext.CreateBuffer(
               FIndexBuffer.Size, WGPUBufferUsage_CopyDst or WGPUBufferUsage_Index,
               'GPUIndexBuffer'
            );
            FGPUBuffer := buffer;
            // TODO: this is clumsy, the VB memory was allocated for nothing
            FreeMem(TIndexBufferCracker(FIndexBuffer).FBuffer);
         end else begin
            buffer := IWGPUBuffer(FGPUBuffer);
         end;
         FMapped := TIWebGPUMappedBuffer.Create(buffer, WGPUMapMode_Write, 0, FIndexBuffer.Size);

         TVertexBufferCracker(FIndexBuffer).FBuffer := FMapped.Data;

      end;

      gpubtStatic : begin

         // for static buffers, the content is copied on unlock in an immutable buffer
         FGPUBuffer := nil;

      end;
   else
      Assert(False);
   end;
end;

// DoUnlock
//
procedure TGPUIndexBufferWebGPU.DoUnlock(context : TContext3D);
var
   buffer : IWGPUBuffer;
begin
   var fmxuContext := (context as TFMXUContext3D_WebGPU);

   case BufferType of

      gpubtDynamic : begin

         Assert(
                (TIndexBufferCracker(FIndexBuffer).FBuffer = FMapped.Data)
            and (Cardinal(FIndexBuffer.Size) = FMapped.Size),
            'When using a TGPUIndexBuffer resizing or reallocating is NOT supported'
         );
         FMapped := nil;

      end;

      gpubtStatic : begin

         buffer := fmxuContext.CreateBufferFromData(
            FIndexBuffer.Size, WGPUBufferUsage_CopyDst or WGPUBufferUsage_Index,
            TIndexBufferCracker(FIndexBuffer).FBuffer,
            'GPUIndexBuffer'
         );
         FGPUBuffer := buffer;

      end;

   else
      Assert(False);
   end;
end;

// Destroy
//
destructor TGPUIndexBufferWebGPU.Destroy;
begin
   if BufferType = gpubtDynamic then
      TIndexBufferCracker(FIndexBuffer).FBuffer := nil;

   inherited;
end;

end.
