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
{    Vertex & Index buffer related utilities                           }
{                                                                      }
{**********************************************************************}
unit FMXU.Buffers;

{$i fmxu.inc}

interface

uses
   System.SysUtils, System.Math.Vectors, System.RTLConsts,
   FMX.Types3D;

type
   TDoubleArray = array [0..MaxInt div 8-1] of Double;
   PDoubleArray = ^TDoubleArray;

   TPoint3DArrayInfo = packed record
      Buffer : PPoint3D;
      Stride, Count : Integer;
      constructor CreateFromVertexBuffer(vb : TVertexBuffer);
   end;

   TGPUBufferType = (
      gpubtDynamic,  // maps to a shared CPU + GPU buffer
      gpubtStatic    // uses a CPU buffer on Lock that is copied on UnLock to a GPU buffer
   );

   TGPUBufferBase = class
      protected
         FGPUBuffer : IInterface;
         FLockCount : Integer;
         FBufferType : TGPUBufferType;

         procedure DoLock(context : TContext3D); virtual;
         procedure DoUnlock(context : TContext3D); virtual;

      public
         destructor Destroy; override;

         procedure Unlock(context : TContext3D);

         property BufferType : TGPUBufferType read FBufferType;
         property GPUBuffer : IInterface read FGPUBuffer;
         property LockCount : Integer read FLockCount;

   end;

   TGPUVertexBufferClass = class of TGPUVertexBuffer;
   TGPUVertexBuffer = class (TGPUBufferBase)
      private class var
         vTGPUVertexBufferClass : TGPUVertexBufferClass;

      protected
         FVertexBuffer : TVertexBuffer;
         FVertexDeclaration : TVertexDeclaration;

         constructor DoCreate(const aFormat: TVertexFormats; const aLength: Integer; aType : TGPUBufferType); virtual;

      public
         class function Create(const aFormat: TVertexFormats; const aLength: Integer; aType : TGPUBufferType) : TGPUVertexBuffer; virtual;
         class function CreateStaticCopy(aVertexBuffer : TVertexBuffer; aContext : TContext3D) : TGPUVertexBuffer;
         destructor Destroy; override;
         class procedure RegisterGPUVertexBufferClass(const aClass : TGPUVertexBufferClass);

         function Lock(context : TContext3D) : TVertexBuffer;

         function VertexSize : Cardinal;
         property VertexDeclarations : TVertexDeclaration read FVertexDeclaration;
         function Length : Integer;
         function Size : NativeUInt;
   end;

   TGPUIndexBufferClass = class of TGPUIndexBuffer;
   TGPUIndexBuffer = class (TGPUBufferBase)
      private class var
         vTGPUIndexBufferClass : TGPUIndexBufferClass;

      protected
         FIndexBuffer : TIndexBuffer;

         constructor DoCreate(const aFormat: TIndexFormat; const aLength: Integer; aType : TGPUBufferType); virtual;

      public
         class function Create(const aFormat: TIndexFormat; const aLength: Integer; aType : TGPUBufferType) : TGPUIndexBuffer;
         class function CreateStaticCopy(aIndexBuffer : TIndexBuffer; aContext : TContext3D) : TGPUIndexBuffer;
         destructor Destroy; override;

         class procedure RegisterGPUIndexBufferClass(const aClass : TGPUIndexBufferClass);

         function Lock(context : TContext3D) : TIndexBuffer;

         function IndexSize : Cardinal;
         function Length : Integer;
         function Size : NativeUInt;
   end;

//: Compute barycenter of the vertex buffer
function BufferBarycenter(const buf : TVertexBuffer) : TPoint3D;
//: Compute average distance of vertex buffer points to a given point
function BufferAverageDistance(const buf : TVertexBuffer; const point : TPoint3D) : Double;

//: Offset all vertices in the buffer
procedure BufferOffset(const buf : TVertexBuffer; const offset : TPoint3D);
//: Offset all vertices then apply scale
procedure BufferOffsetAndScale(const buf : TVertexBuffer; const offset : TPoint3D; scale : Single);

//: Fill the indexbuffer with a sequence
procedure IndexBufferSetSequence(buf : TIndexBuffer; base, increment : Integer);

procedure SetIndexQuadSequence_UInt16(buffer : Pointer; nbQuads : Integer);
procedure SetIndexQuadSequence_UInt32(buffer : Pointer; nbQuads : Integer);

{: Create and fill an index buffer with quad indexes for a triangles vertex buffer
   The vertex buffer should hold a sequence of the 4 vertices of each quad }
function CreateIndexBufferQuadSequence(nbQuads : Integer) : TIndexBuffer;

{: Fill a vertex buffer with integer X,Y grid coordinates
   buffer size MUST be a multiple of resolutionX, Z is set to zero }
procedure SetVertexBufferGridXY(buf : TVertexBuffer; resolutionX : Integer);

//: Performs double-precision dot product of an array
procedure Point3DotProductToDoubleArray(
   const points : TPoint3DArrayInfo;   // points array input
   const v : TPoint3D;                 // vector3 for dot product
   destResult : PDoubleArray           // destination result array
);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// CreateFromVertexBuffer
//
constructor TPoint3DArrayInfo.CreateFromVertexBuffer(vb : TVertexBuffer);
begin
   Buffer := vb.Buffer;
   Stride := vb.VertexSize;
   Count := vb.Length;
end;

// BufferBarycenter
//
function BufferBarycenter(const buf : TVertexBuffer) : TPoint3D;
begin
   Result := Default(TPoint3D);
   if (buf.Length = 0) or not (TVertexFormat.Vertex in buf.Format) then Exit;

   var aX : Double := 0;
   var aY := aX;
   var aZ := aX;
   var stride : UIntPtr := buf.VertexSize;
   var p := PPoint3D(buf.VerticesPtr[0]);
   for var i := 1 to buf.Length do begin
      aX := aX + p.X;
      aY := aY + p.Y;
      aZ := aZ + p.Z;
      p := Pointer(UIntPtr(p) + stride);
   end;
   Result.X := aX / buf.Length;
   Result.Y := aY / buf.Length;
   Result.Z := aZ / buf.Length;
end;

// BufferAverageDistance
//
function BufferAverageDistance(const buf : TVertexBuffer; const point : TPoint3D) : Double;
begin
   Result := 0;
   if (buf.Length = 0) or not (TVertexFormat.Vertex in buf.Format) then Exit;

   var pX := point.X;
   var pY := point.Y;
   var pZ := point.Z;
   var stride : UIntPtr := buf.VertexSize;
   var p := PPoint3D(buf.VerticesPtr[0]);
   if (pX = 0) and (pY = 0) and (pZ = 0) then begin
      for var i := 1 to buf.Length do begin
         Result := Result + Sqr(p.X) + Sqr(p.Y) + Sqr(p.Z);
         p := Pointer(UIntPtr(p) + stride);
      end;
   end else begin
      for var i := 1 to buf.Length do begin
         Result := Result + Sqr(p.X - pX) + Sqr(p.Y - py) + Sqr(p.Z - pz);
         p := Pointer(UIntPtr(p) + stride);
      end;
   end;
   Result := Sqrt(Result / buf.Length);
end;

// BufferOffset
//
procedure BufferOffset(const buf : TVertexBuffer; const offset : TPoint3D);
begin
   if (buf.Length = 0) or not (TVertexFormat.Vertex in buf.Format) then Exit;

   var dX : Single := offset.X;
   var dY : Single := offset.Y;
   var dZ : Single := offset.Z;
   var stride : UIntPtr := buf.VertexSize;
   var p := PPoint3D(buf.VerticesPtr[0]);
   for var i := 1 to buf.Length do begin
      p.X := p.X + dX;
      p.Y := p.Y + dY;
      p.Z := p.Z + dZ;
      p := Pointer(UIntPtr(p) + stride);
   end;
end;

// BufferOffsetAndScale
//
procedure BufferOffsetAndScale(const buf : TVertexBuffer; const offset : TPoint3D; scale : Single);
begin
   if (buf.Length = 0) or not (TVertexFormat.Vertex in buf.Format) then Exit;

   var dX : Single := offset.X;
   var dY : Single := offset.Y;
   var dZ : Single := offset.Z;
   var stride : UIntPtr := buf.VertexSize;
   var p := PPoint3D(buf.VerticesPtr[0]);
   for var i := 1 to buf.Length do begin
      p.X := (p.X + dX) * scale;
      p.Y := (p.Y + dY) * scale;
      p.Z := (p.Z + dZ) * scale;
      p := Pointer(UIntPtr(p) + stride);
   end;
end;

// IndexBufferSetSequence
//
procedure IndexBufferSetSequence(buf : TIndexBuffer; base, increment : Integer);
begin
   case buf.Format of
      TIndexFormat.UInt16: begin
         var v := Word(base);
         var d := Word(increment);
         var p := PWord(buf.Buffer);
         for var i := 0 to buf.Length-1 do begin
            p^ := v;
            Inc(p);
            Inc(v, d)
         end;
      end;
      TIndexFormat.UInt32: begin
         var v := UInt32(base);
         var d := UInt32(increment);
         var p := PUInt32(buf.Buffer);
         for var i := 0 to buf.Length-1 do begin
            p^ := v;
            Inc(p);
            Inc(v, d)
         end;
      end;
   else
      Assert(False);
   end;
end;

// SetIndexQuadSequence_UInt16
//
procedure SetIndexQuadSequence_UInt16(buffer : Pointer; nbQuads : Integer);
type
   TWord6 = array [0..5] of Word;
   PWord6 = ^TWord6;
begin
   Assert(nbQuads < 65536 div 4, 'Too many quads for UInt16');
   var pIndices : PWord6 := buffer;
   var k := 0;
   for var i := 1 to nbQuads do begin
      {$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
      pIndices[0] := k+0;
      pIndices[1] := k+1;
      pIndices[2] := k+2;
      pIndices[3] := k+0;
      pIndices[4] := k+2;
      pIndices[5] := k+3;
      {$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}
      Inc(pIndices);
      Inc(k, 4);
   end;
end;

// SetIndexQuadSequence_UInt32
//
procedure SetIndexQuadSequence_UInt32(buffer : Pointer; nbQuads : Integer);
type
   TDWord6 = array [0..5] of UInt32;
   PDWord6 = ^TDWord6;
begin
   var pIndices : PDWord6 := buffer;
   var k := 0;
   for var i := 1 to nbQuads do begin
      {$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
      pIndices[0] := k+0;
      pIndices[1] := k+1;
      pIndices[2] := k+2;
      pIndices[3] := k+0;
      pIndices[4] := k+2;
      pIndices[5] := k+3;
      {$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}
      Inc(pIndices);
      Inc(k, 4);
   end;
end;

// CreateIndexBufferQuadSequence
//
function CreateIndexBufferQuadSequence(nbQuads : Integer) : TIndexBuffer;
begin
   if nbQuads >= 65536 div 4 then begin
      Result := TIndexBuffer.Create(nbQuads*6, TIndexFormat.UInt32);
      SetIndexQuadSequence_UInt32(Result.Buffer, nbQuads);
   end else begin
      Result := TIndexBuffer.Create(nbQuads*6, TIndexFormat.UInt16);
      SetIndexQuadSequence_UInt16(Result.Buffer, nbQuads);
   end;
end;

// SetVertexBufferGridXY
//
procedure SetVertexBufferGridXY(buf : TVertexBuffer; resolutionX : Integer);
var
   ptr : PPoint3D;
   resolutionY, x, y : Integer;
   ySingle, zZero : Single;
begin
   resolutionY := buf.Length div resolutionX;
   Assert(resolutionX * resolutionY = buf.Length);
   ptr := buf.VerticesPtr[0];
   zZero := 0;
   for y := 0 to resolutionY-1 do begin
      ySingle := y;
      for x := 0 to resolutionX-1 do begin
         ptr.X := x;
         ptr.Y := ySingle;
         ptr.Z := zZero;
         Inc(ptr);
      end;
   end;
end;

// BufferDotProductToDoubleArray
//
procedure Point3DotProductToDoubleArray(
   const points : TPoint3DArrayInfo;
   const v : TPoint3D;
   destResult : PDoubleArray
);
{$ifdef WIN64_ASM}
asm          // x86-64 clang 18.1
   movsxd rax, dword ptr [rcx + 12]
   test rax, rax
   jle @@LBB0_5
   movss xmm0, dword ptr [rdx]
   cvtss2sd xmm1, xmm0
   cvtps2pd xmm0, qword ptr [rdx + 4]
   mov rdx, qword ptr [rcx]
   cmp eax, 1
   jne @@LBB0_6
   xor ecx, ecx
   jmp @@LBB0_3
@@LBB0_6:
   movsxd r9, dword ptr [rcx + 8]
   mov r10d, eax
   and r10d, 2147483646
   lea r11, [r9 + r9]
   xor ecx, ecx
@@LBB0_7:
   movss xmm2, dword ptr [rdx]
   cvtss2sd xmm2, xmm2
   mulsd xmm2, xmm1
   cvtps2pd xmm3, qword ptr [rdx + 4]
   mulpd xmm3, xmm0
   addsd xmm2, xmm3
   unpckhpd xmm3, xmm3
   addsd xmm3, xmm2
   movsd qword ptr [r8 + 8*rcx], xmm3
   movss xmm2, dword ptr [rdx + r9]
   cvtss2sd xmm2, xmm2
   mulsd xmm2, xmm1
   cvtps2pd xmm3, qword ptr [rdx + r9 + 4]
   mulpd xmm3, xmm0
   addsd xmm2, xmm3
   unpckhpd xmm3, xmm3
   addsd xmm3, xmm2
   movsd qword ptr [r8 + 8*rcx + 8], xmm3
   add rdx, r11
   add rcx, 2
   cmp r10, rcx
   jne @@LBB0_7
@@LBB0_3:
   test al, 1
   je @@LBB0_5
   movss xmm2, dword ptr [rdx]
   cvtss2sd xmm2, xmm2
   mulsd xmm2, xmm1
   cvtps2pd xmm1, qword ptr [rdx + 4]
   mulpd xmm1, xmm0
   addsd xmm2, xmm1
   unpckhpd xmm1, xmm1
   addsd xmm1, xmm2
   movsd qword ptr [r8 + 8*rcx], xmm1
@@LBB0_5:
end;
{$else}
begin
   var vX : Double := v.X;
   var vY : Double := v.Y;
   var vZ : Double := v.Z;
   var p := points.Buffer;
   var stride : NativeUInt := points.Stride;
   {$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
   for var i := 0 to points.Count-1 do begin
      destResult[i] := p.X * vX + p.Y * vY + p.Z * vZ;
      p := Pointer(UIntPtr(p) + stride);
   end;
   {$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}
end;
{$endif}

// ------------------
// ------------------ TGPUBufferBase ------------------
// ------------------

// DoLock
//
procedure TGPUBufferBase.DoLock(context : TContext3D);
begin
   // empty
end;

// DoUnlock
//
procedure TGPUBufferBase.DoUnlock(context : TContext3D);
begin
   // empty
end;

// Destroy
//
destructor TGPUBufferBase.Destroy;
begin
   Assert(FLockCount = 0, 'TGPUBuffer is still locked!');
   inherited;
end;

// Unlock
//
procedure TGPUBufferBase.Unlock(context : TContext3D);
begin
   Assert(FLockCount > 0);
   Dec(FLockCount);
   if FLockCount = 0 then
      DoUnlock(context);
end;

// ------------------
// ------------------ TGPUVertexBuffer ------------------
// ------------------

// DoCreate
//
constructor TGPUVertexBuffer.DoCreate(const aFormat: TVertexFormats; const aLength: Integer; aType : TGPUBufferType);
begin
   inherited Create;
   FVertexBuffer := TVertexBuffer.Create(aFormat, aLength);
   FBufferType := aType;
   FVertexDeclaration := FVertexBuffer.GetVertexDeclarations;
end;

// Create
//
class function TGPUVertexBuffer.Create(const aFormat: TVertexFormats; const aLength: Integer; aType : TGPUBufferType) : TGPUVertexBuffer;
begin
   if vTGPUVertexBufferClass <> nil then
      Result := vTGPUVertexBufferClass.DoCreate(aFormat, aLength, aType)
   else Result := TGPUVertexBuffer.DoCreate(aFormat, aLength, aType);
end;

// CreateStaticCopy
//
class function TGPUVertexBuffer.CreateStaticCopy(aVertexBuffer : TVertexBuffer; aContext : TContext3D) : TGPUVertexBuffer;
begin
   Result := Create(aVertexBuffer.Format, aVertexBuffer.Length, gpubtStatic);
   try
      var dest := Result.Lock(aContext);
      try
         Move(aVertexBuffer.Buffer^, dest.Buffer^, dest.Size);
      finally
         Result.Unlock(aContext);
      end;
   except
      Result.Free;
      raise;
   end;
end;

// Destroy
//
destructor TGPUVertexBuffer.Destroy;
begin
   inherited;
   FreeAndNil(FVertexBuffer);
end;

// RegisterGPUVertexBufferClass
//
class procedure TGPUVertexBuffer.RegisterGPUVertexBufferClass(const aClass : TGPUVertexBufferClass);
begin
   vTGPUVertexBufferClass := aClass;
end;

// Lock
//
function TGPUVertexBuffer.Lock(context : TContext3D) : TVertexBuffer;
begin
   Inc(FLockCount);
   if FLockCount = 1 then
      DoLock(context);
   Result := FVertexBuffer;
end;

// VertexSize
//
function TGPUVertexBuffer.VertexSize : Cardinal;
begin
   Result := FVertexBuffer.VertexSize;
end;

// Length
//
function TGPUVertexBuffer.Length : Integer;
begin
   Result := FVertexBuffer.Length;
end;

// Size
//
function TGPUVertexBuffer.Size : NativeUInt;
begin
   Result := FVertexBuffer.Size;
end;

// ------------------
// ------------------ TGPUIndexBuffer ------------------
// ------------------

// Create
//
constructor TGPUIndexBuffer.DoCreate(const aFormat: TIndexFormat; const aLength: Integer; aType : TGPUBufferType);
begin
   inherited Create;
   FIndexBuffer := TIndexBuffer.Create(aLength, aFormat);
   FBufferType := aType;
end;

// Create
//
class function TGPUIndexBuffer.Create(const aFormat: TIndexFormat; const aLength: Integer; aType : TGPUBufferType) : TGPUIndexBuffer;
begin
   if vTGPUIndexBufferClass <> nil then
      Result := vTGPUIndexBufferClass.DoCreate(aFormat, aLength, aType)
   else Result := TGPUIndexBuffer.DoCreate(aFormat, aLength, aType);
end;

// CreateStaticCopy
//
class function TGPUIndexBuffer.CreateStaticCopy(aIndexBuffer : TIndexBuffer; aContext : TContext3D) : TGPUIndexBuffer;
begin
   Result := Create(aIndexBuffer.Format, aIndexBuffer.Length, gpubtStatic);
   try
      var dest := Result.Lock(aContext);
      try
         Move(aIndexBuffer.Buffer^, dest.Buffer^, dest.Size);
      finally
         Result.Unlock(aContext);
      end;
   except
      Result.Free;
      raise;
   end;
end;

// Destroy
//
destructor TGPUIndexBuffer.Destroy;
begin
   inherited;
   FreeAndNil(FIndexBuffer);
end;

// RegisterGPUIndexBufferClass
//
class procedure TGPUIndexBuffer.RegisterGPUIndexBufferClass(const aClass : TGPUIndexBufferClass);
begin
   vTGPUIndexBufferClass := aClass;
end;

// Lock
//
function TGPUIndexBuffer.Lock(context : TContext3D) : TIndexBuffer;
begin
   Inc(FLockCount);
   if FLockCount = 1 then
      DoLock(context);
   Result := FIndexBuffer;
end;

// IndexSize
//
function TGPUIndexBuffer.IndexSize : Cardinal;
begin
   Result := FIndexBuffer.IndexSize;
end;

// Length
//
function TGPUIndexBuffer.Length : Integer;
begin
   Result := FIndexBuffer.Length;
end;

// Size
//
function TGPUIndexBuffer.Size : NativeUInt;
begin
   Result := FIndexBuffer.Size;
end;

end.
