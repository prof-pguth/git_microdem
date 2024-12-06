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
{    DirectX 11 hardware buffers                                       }
{                                                                      }
{**********************************************************************}
unit FMXU.Buffers.DX11;

{$i fmxu.inc}

interface

uses
   Winapi.D3D11,
   System.Classes,
   FMX.Types3D,
   FMXU.Buffers;

type
   TGPUVertexBufferDX11 = class (TGPUVertexBuffer)
      protected
         FMapped : TD3D11_MAPPED_SUBRESOURCE;

      protected
         procedure DoLock(context : TContext3D); override;
         procedure DoUnlock(context : TContext3D); override;

      public
         destructor Destroy; override;
   end;

   TGPUIndexBufferDX11 = class (TGPUIndexBuffer)
      protected
         FMapped : TD3D11_MAPPED_SUBRESOURCE;

      protected
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

uses FMXU.Context, FMXU.Context.DX11;

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
// ------------------ TGPUVertexBufferDX11 ------------------
// ------------------

// DoLock
//
procedure TGPUVertexBufferDX11.DoLock(context : TContext3D);
var
   buffer : ID3D11Buffer;
begin
   Assert(context <> nil);
   case BufferType of

      gpubtDynamic : begin

         // for dynamic buffers, we map straight to D3D11 buffer
         var fmxuContext := (context as TFMXUContext3D_DX11);

         if FGPUBuffer = nil then begin
            // first access, allocate buffer
            buffer := fmxuContext.CreateBuffer(
               FVertexBuffer.Size, D3D11_BIND_VERTEX_BUFFER,
               D3D11_USAGE_DYNAMIC, D3D11_CPU_ACCESS_WRITE
            );
            FGPUBuffer := buffer;
            // TODO: this is clumsy, the VB memory was allocated for nothing
            FreeMem(TVertexBufferCracker(FVertexBuffer).FBuffer);
         end else begin
            buffer := ID3D11Buffer(FGPUBuffer);
         end;
         FMapped := fmxuContext.MapBuffer(buffer, D3D11_MAP_WRITE_DISCARD);

         TVertexBufferCracker(FVertexBuffer).FBuffer := FMapped.pData;

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
procedure TGPUVertexBufferDX11.DoUnlock(context : TContext3D);
var
   buffer : ID3D11Buffer;
begin
   var fmxuContext := (context as TFMXUContext3D_DX11);

   case BufferType of

      gpubtDynamic : begin

         Assert(
                (TVertexBufferCracker(FVertexBuffer).FBuffer = FMapped.pData)
            and (Cardinal(FVertexBuffer.Size) = FMapped.RowPitch),
            'When using a TGPUVertexBuffer resizing or reallocating is NOT supported'
         );

         buffer := ID3D11Buffer(FGPUBuffer);
         fmxuContext.UnmapBuffer(buffer);

         FMapped := Default(TD3D11_MAPPED_SUBRESOURCE);

      end;

      gpubtStatic : begin

         buffer := fmxuContext.CreateBufferFromData(
            FVertexBuffer.Size, D3D11_BIND_VERTEX_BUFFER,
            D3D11_USAGE_IMMUTABLE, 0,
            TVertexBufferCracker(FVertexBuffer).FBuffer
         );
         FGPUBuffer := buffer;

      end;

   else
      Assert(False);
   end;
end;

// Destroy
//
destructor TGPUVertexBufferDX11.Destroy;
begin
   if BufferType = gpubtDynamic then
      TVertexBufferCracker(FVertexBuffer).FBuffer := nil;

   inherited;
end;

// ------------------
// ------------------ TGPUIndexBufferDX11 ------------------
// ------------------

// DoLock
//
procedure TGPUIndexBufferDX11.DoLock(context : TContext3D);
var
   buffer : ID3D11Buffer;
begin
   Assert(context <> nil);
   case BufferType of

      gpubtDynamic : begin

         // for dynamic buffers, we map straight to D3D11 buffer
         var fmxuContext := (context as TFMXUContext3D_DX11);

         if FGPUBuffer = nil then begin
            // first access, allocate buffer
            buffer := fmxuContext.CreateBuffer(
               FIndexBuffer.Size, D3D11_BIND_INDEX_BUFFER,
               D3D11_USAGE_DYNAMIC, D3D11_CPU_ACCESS_WRITE
            );
            FGPUBuffer := buffer;
            // TODO: this is clumsy, the VB memory was allocated for nothing
            FreeMem(TIndexBufferCracker(FIndexBuffer).FBuffer);
         end else begin
            buffer := ID3D11Buffer(FGPUBuffer);
         end;
         FMapped := fmxuContext.MapBuffer(buffer, D3D11_MAP_WRITE_DISCARD);

         TIndexBufferCracker(FIndexBuffer).FBuffer := FMapped.pData;

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
procedure TGPUIndexBufferDX11.DoUnlock(context : TContext3D);
var
   buffer : ID3D11Buffer;
begin
   var fmxuContext := (context as TFMXUContext3D_DX11);

   case BufferType of

      gpubtDynamic : begin

         Assert(
                (TIndexBufferCracker(FIndexBuffer).FBuffer = FMapped.pData)
            and (Cardinal(FIndexBuffer.Size) = FMapped.RowPitch),
            'When using a TGPUIndexBuffer resizing or reallocating is NOT supported'
         );

         buffer := ID3D11Buffer(FGPUBuffer);
         fmxuContext.UnmapBuffer(buffer);

         FMapped := Default(TD3D11_MAPPED_SUBRESOURCE);

      end;

      gpubtStatic : begin

         buffer := fmxuContext.CreateBufferFromData(
            FIndexBuffer.Size, D3D11_BIND_INDEX_BUFFER,
            D3D11_USAGE_IMMUTABLE, 0,
            FIndexBuffer.Buffer
         );
         FGPUBuffer := buffer;

      end;

   else
      Assert(False);
   end;
end;

// Destroy
//
destructor TGPUIndexBufferDX11.Destroy;
begin
   if BufferType = gpubtDynamic then
      TIndexBufferCracker(FIndexBuffer).FBuffer := nil;

   inherited;
end;

end.
