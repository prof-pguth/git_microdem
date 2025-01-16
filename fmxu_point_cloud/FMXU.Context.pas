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
{    This unit holds context-related utilities                         }
{                                                                      }
{**********************************************************************}
unit FMXU.Context;

{$i fmxu.inc}
{$SCOPEDENUMS ON}

interface

uses
   System.Classes, System.SysUtils, System.UIConsts, System.SyncObjs,
   FMX.Types3D, FMX.Materials,
   FMXU.Buffers, FMXU.Colors;

{$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
const
   // extension to TContextShaderArch
   TContextShaderArch_WGSL : TContextShaderArch = Succ(High(TContextShaderArch));
{$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}

type
   PContextShaderVariable = ^TContextShaderVariable;

   TPrimitivesKindU = ( Points, Lines, Triangles, TrianglesStrip );

   TFMXUContext3D = class (TContext3D)
      private
         class var vResourceLock : TLightweightMREW;
         class var vResourceList : array of IInterface;

      protected
         class function AddResource(const aResource : IInterface) : THandle;
         class function GetResource(aResourceHandle : THandle) : IInterface;
         class procedure RemoveResource(aResourceHandle : THandle);

         function GetCurrentOpacity : Single;
         procedure SetCurrentOpacity(opacity : Single);

         function GetCurrentMaterial : TMaterial;
         procedure SetCurrentMaterial(material : TMaterial);

         function GetCurrentMaterialClass : TMaterialClass;
         procedure SetCurrentMaterialClass(materialClass : TMaterialClass);

         function StillValid(aContext3D : TContext3D) : Boolean; virtual; abstract;

      public
         // use in combination with DrawGPUPrimitives
         procedure ApplyMaterial(const material: TMaterial; const opacity: Single); virtual;
         procedure ResetMaterial; virtual;

         procedure DrawGPUPrimitives(
            const aKind : TPrimitivesKindU;
            const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer
         ); virtual; abstract;

         // jailbroken properties

         property CurrentOpacity : Single read GetCurrentOpacity write SetCurrentOpacity;
         property CurrentMaterial : TMaterial read GetCurrentMaterial write SetCurrentMaterial;
         property CurrentMaterialClass : TMaterialClass read GetCurrentMaterialClass write SetCurrentMaterialClass;
   end;

   TFMXUContext3DHelper = class helper for TContext3D
      public
         procedure ApplyMaterial(const material: TMaterial; const opacity: Single);
         procedure ResetMaterial;

         procedure DrawGPUPrimitives(
            const aKind : TPrimitivesKindU;
            const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer
         );

         // Helpers for classic methods

         procedure DrawPrimitives(const aKind : TPrimitivesKind; const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer; const material : TMaterial; const opacity : Single); overload;
         procedure DrawTriangles(const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer; const material : TMaterial; const opacity : Single); overload; inline;
         procedure DrawLines(const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer; const material : TMaterial; const opacity : Single); overload; inline;
         procedure DrawPoints(const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer; const material : TMaterial; const opacity : Single); overload; inline;

         function SupportsGPUPrimitives : Boolean;
         function GetFMXUContext : TFMXUContext3D;
   end;

   // wraps a context shader source in an interface
   TIContextShaderSource = class;
   IContextShaderSource = interface
      ['{2345E06D-A1F7-437B-97D9-60549EAD26CB}']
      function GetSelf : TIContextShaderSource;
      //: Per process unique ID
      function GetID : NativeUInt;
      function GetVariablesSize : Integer;
      function GetMaxTextureSlot : Integer;
      function IndexOfVariable(const name : String) : Integer;
      function GetVariablesCount : Integer;
      function GetSize(index : Integer) : Cardinal;
      function GetIndex(index : Integer) : Cardinal;
      function GetUserData : IInterface;

      function SpecializeForVertexDeclaration(const declaration : TVertexDeclaration) : IContextShaderSource;

      property Size[index : Integer] : Cardinal read GetSize;
      property Index[index : Integer] : Cardinal read GetIndex;
   end;
   TIContextShaderSource = class(TInterfacedObject, IContextShaderSource)
      protected
         class var vIDCounter : NativeUInt;
      protected
         FID : NativeUInt;
         FSource : TContextShaderSource;
         FVariablesSize : Integer;
         FMaxTextureSlot : Integer;
         FUserData : IInterface;

         function GetSelf : TIContextShaderSource;
         function GetID : NativeUInt;
         function GetVariablesSize : Integer;
         function GetMaxTextureSlot : Integer;
         function GetUserData : IInterface;

      public
         constructor Create(const aSource : TContextShaderSource; minVariableSlotSize : Integer);

         function IndexOfVariable(const name : String) : Integer;
         function GetVariablesCount : Integer;
         function GetSize(index : Integer) : Cardinal;
         function GetIndex(index : Integer) : Cardinal;

         function SpecializeForVertexDeclaration(const declaration : TVertexDeclaration) : IContextShaderSource; virtual;

         property ID : NativeUInt read FID;
         property Source : TContextShaderSource read FSource;
         property Code : TContextShaderCode read FSource.Code;
         property Variables : TContextShaderVariables read FSource.Variables;
         property VariablesSize : Integer read FVariablesSize;
         property MaxTextureSlot : Integer read FMaxTextureSlot;
         property UserData : IInterface read FUserData write FUserData;
   end;

// Compare two TVertexDeclaration and return True if they're identical
function SameVertexDeclaration(const a, b : TVertexDeclaration) : Boolean;

{: Returns current context shader architecture }
function ContextShaderArch : TContextShaderArch;
{: Returns context shader in simplified form (DX11, GLSL & Metal only) }
function ContextShaderArchSimplified : TContextShaderArch;

function CreateContextShader(
   kind : TContextShaderKind; const shaderCode : AnsiString;
   const variableNames : array of String;
   const variableKinds : array of TContextShaderVariableKind
   ) : TContextShader;

function MultisampleToSampleCount(const aMultisample : TMultisample) : Integer;
function SampleCountToMultisample(const count : Integer) : TMultisample;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses FMXU.D3DShaderCompiler;

var
   vPrepared : Boolean;
   vContextFinalized : Boolean;
   vContextShaderArch : TContextShaderArch;
   vContextShaderSimplifiedArch : TContextShaderArch;
   vShaderIndexBase : Integer;
   vD3D_VS_Target, vD3D_PS_Target : AnsiString;

// Prepare
//
function Prepare : TContextShaderArch;
begin
   vShaderIndexBase := 0;

   var contextClassName := TContextManager.DefaultContextClass.ClassName;
   if Pos('DX9', contextClassName) > 0 then begin
      vContextShaderArch := TContextShaderArch.DX9;
      vD3D_VS_Target := 'vs_3_0';
      vD3D_PS_Target := 'ps_3_0';
   end else if Pos('DX11', contextClassName) > 0 then begin
      vContextShaderArch := TContextShaderArch.DX11;
      vContextShaderSimplifiedArch := TContextShaderArch.DX11;
      vD3D_VS_Target := 'vs_4_0';
      vD3D_PS_Target := 'ps_4_0';
   end else if Pos('Android', contextClassName) > 0 then begin
      vContextShaderArch := TContextShaderArch.Android;
      vContextShaderSimplifiedArch := TContextShaderArch.GLSL;
      vShaderIndexBase := 1;
   end else if Pos('OpenGL', contextClassName) > 0 then begin
      vContextShaderArch := TContextShaderArch.GLSL;
      vContextShaderSimplifiedArch := TContextShaderArch.GLSL;
   end else if Pos('Metal', contextClassName) > 0 then begin
      vContextShaderArch := TContextShaderArch.Metal;
      vContextShaderSimplifiedArch := TContextShaderArch.Metal;
      vShaderIndexBase := 1;
   end else if Pos('WebGPU', contextClassName) > 0 then begin
      vContextShaderArch := TContextShaderArch_WGSL;
      vContextShaderSimplifiedArch := TContextShaderArch_WGSL;
      vShaderIndexBase := 0;
   end else begin
      raise EContext3DException.CreateFmt(
         'Unsupported context class "%s"', [ contextClassName ]
      );
   end;
   vPrepared := True;
   Result := vContextShaderArch;
end;

// ContextShaderArch
//
function ContextShaderArch : TContextShaderArch;
begin
   Result := vContextShaderArch;
   if Result = TContextShaderArch.Undefined then
      Result := Prepare;
end;

// ContextShaderArchSimplified
//
function ContextShaderArchSimplified : TContextShaderArch;
begin
   Result := vContextShaderSimplifiedArch;
   if Result = TContextShaderArch.Undefined then begin
      Prepare;
      Result := vContextShaderSimplifiedArch;
   end;
end;

// SameVertexDeclaration
//
function SameVertexDeclaration(const a, b : TVertexDeclaration) : Boolean;
begin
   var n := Length(a);
   if n <> Length(b) then Exit(False);

   Result := CompareMem(Pointer(a), Pointer(b), n * SizeOf(a[0]));
end;

// ShaderVariableSize
//
function ShaderVariableSize(kind : TContextShaderVariableKind) : Integer;
begin
   if not vPrepared then Prepare;
   // will eventually be replaced by a lookup table, but unsure of all the values
   // just yet, so what's unknown is protected by asserts
   case kind of
      TContextShaderVariableKind.Float2 :
         case vContextShaderSimplifiedArch of
            TContextShaderArch.GLSL, TContextShaderArch.Metal :
               Result := 1;
            TContextShaderArch.DX11 :
               Result := 8;
         else
            if vContextShaderSimplifiedArch = TContextShaderArch_WGSL then
               Result := 8
            else begin
               Result := 0;
               Assert(False, 'TODO');
            end;
         end;
      TContextShaderVariableKind.Matrix :
         case vContextShaderSimplifiedArch of
            TContextShaderArch.GLSL, TContextShaderArch.Metal :
               Result := 4;
            TContextShaderArch.DX11 :
               Result := 64;
         else
            if vContextShaderSimplifiedArch = TContextShaderArch_WGSL then
               Result := 64
            else begin
               Result := 0;
               Assert(False, 'TODO');
            end;
         end;
      TContextShaderVariableKind.Vector :
         case vContextShaderSimplifiedArch of
            TContextShaderArch.GLSL, TContextShaderArch.Metal :
               Result := 1;
            TContextShaderArch.DX11 :
               Result := 16;
         else
            if vContextShaderSimplifiedArch = TContextShaderArch_WGSL then
               Result := 16
            else begin
               Result := 0;
               Assert(False, 'TODO');
            end;
         end;
      TContextShaderVariableKind.texture :
         case vContextShaderSimplifiedArch of
            TContextShaderArch.GLSL, TContextShaderArch.Metal :
               Result := 0;
            TContextShaderArch.DX11 :
               Result := 0;
         else
            if vContextShaderSimplifiedArch = TContextShaderArch_WGSL then
               Result := 0
            else begin
               Result := 0;
               Assert(False, 'TODO');
            end;
         end;
   else
      Result := 0;
      Assert(False, 'TODO');
   end;
end;

// CreateContextShader
//
function CreateContextShader(
   kind : TContextShaderKind; const shaderCode : AnsiString;
   const variableNames : array of String;
   const variableKinds : array of TContextShaderVariableKind
   ) : TContextShader;
var
   variables : array of TContextShaderVariable;
   variableIndex : Integer;
   variablePtr : ^TContextShaderVariable;

   procedure AddSource(const name : String; kind : TContextShaderVariableKind);
   begin
      var size := ShaderVariableSize(kind);
      variablePtr^ := TContextShaderVariable.Create(name, kind, variableIndex, size);
      Inc(variablePtr);
      Inc(variableIndex, size);
   end;

begin
   if not vPrepared then Prepare;

   var nbVariables := Length(variableNames);
   if Length(variableKinds) <> nbVariables then begin
      raise EContext3DException.CreateFmt(
         'CreateContextShader: mismatched variable names & kinds arrays (%d elments vs %d)',
         [ nbVariables, Length(variableKinds) ]);
   end;

   // build variables array
   if kind = TContextShaderKind.VertexShader then begin
      variableIndex := vShaderIndexBase;
      SetLength(variables, nbVariables + 1);
      variablePtr := Pointer(variables);
      AddSource('MVPMatrix', TContextShaderVariableKind.Matrix);
   end else begin
      variableIndex := 0;
      SetLength(variables, nbVariables);
      variablePtr := Pointer(variables);
   end;
   for var i := 0 to nbVariables-1 do
      AddSource(variableNames[i], variableKinds[i]);

   // copy or compile shader
   var shaderData : TBytes;
   case vContextShaderArch of
      TContextShaderArch.DX9, TContextShaderArch.DX11 : begin
         case kind of
            TContextShaderKind.VertexShader:
               shaderData := CompileShaderFromSource(shaderCode, 'main', vD3D_VS_Target);
            TContextShaderKind.PixelShader:
               shaderData := CompileShaderFromSource(shaderCode, 'main', vD3D_PS_Target);
         else
            Assert(False);
         end;
      end;
      TContextShaderArch.Android, TContextShaderArch.GLSL, TContextShaderArch.Metal : begin
         SetLength(shaderData, Length(shaderCode));
         System.Move(Pointer(shaderCode)^, Pointer(shaderData)^, Length(shaderCode));
      end;
   else
      if vContextShaderArch = TContextShaderArch_WGSL then begin
         SetLength(shaderData, Length(shaderCode));
         System.Move(Pointer(shaderCode)^, Pointer(shaderData)^, Length(shaderCode));
      end else Assert(False);
   end;

   var source := TContextShaderSource.Create(vContextShaderArch, shaderData, variables);

   Result := TContextShader.Create;
   Result.LoadFromData('', kind, '', source);
end;

// MultisampleToSampleCount
//
function MultisampleToSampleCount(const aMultisample : TMultisample) : Integer;
begin
   case aMultisample of
      TMultisample.None: Result := 1;
      TMultisample.TwoSamples: Result := 2;
      TMultisample.FourSamples: Result := 4;
   else
      Result := 0;
      Assert(False);
   end;
end;

// SampleCountToMultisample
//
function SampleCountToMultisample(const count : Integer) : TMultisample;
begin
   case count of
      0, 1 : Result := TMultisample.None;
      2 : Result := TMultisample.TwoSamples;
      4 : Result := TMultisample.FourSamples;
   else
      Result := TMultisample.None;
      Assert(False);
   end;
end;

// ------------------
// ------------------ TFMXUContext3D ------------------
// ------------------

type
   TContext3DMaterial = record
      FCurrentOpacity: Single;
      FCurrentMaterial: TMaterial;
      FCurrentMaterialClass: TMaterialClass;
   end;
   PContext3DMaterial = ^TContext3DMaterial;

// GetCurrentOpacity
//
function TFMXUContext3D.GetCurrentOpacity : Single;
begin
   Result := inherited CurrentOpacity;
end;

// SetCurrentOpacity
//
procedure TFMXUContext3D.SetCurrentOpacity(opacity : Single);
begin
   PSingle(@inherited CurrentOpacity)^ := opacity;
end;

// GetCurrentMaterial
//
function TFMXUContext3D.GetCurrentMaterial : TMaterial;
begin
   Result := PContext3DMaterial(@inherited CurrentOpacity).FCurrentMaterial;
end;

// SetCurrentMaterial
//
procedure TFMXUContext3D.SetCurrentMaterial(material : TMaterial);
begin
   PContext3DMaterial(@inherited CurrentOpacity).FCurrentMaterial := material;
end;

// GetCurrentMaterialClass
//
function TFMXUContext3D.GetCurrentMaterialClass : TMaterialClass;
begin
   Result := PContext3DMaterial(@inherited CurrentOpacity).FCurrentMaterialClass;
end;

// SetCurrentMaterialClass
//
procedure TFMXUContext3D.SetCurrentMaterialClass(materialClass : TMaterialClass);
begin
   PContext3DMaterial(@inherited CurrentOpacity).FCurrentMaterialClass := materialClass;
end;

// ApplyMaterial
//
procedure TFMXUContext3D.ApplyMaterial(const material: TMaterial; const opacity: Single);

   procedure InnerApplyMaterial;
   begin
      var material := CurrentMaterial;
      CurrentMaterialClass := TMaterialClass(Material.ClassType);
      material.Apply(Self);
      if material.GetMaterialProperty(TMaterial.TProperty.ModelViewProjection) <> '' then
         SetShaderVariable(material.GetMaterialProperty(TMaterial.TProperty.ModelViewProjection), CurrentModelViewProjectionMatrix);
      if material.GetMaterialProperty(TMaterial.TProperty.ModelView) <> '' then
         SetShaderVariable(material.GetMaterialProperty(TMaterial.TProperty.ModelView), CurrentMatrix);
      if material.GetMaterialProperty(TMaterial.TProperty.ModelViewInverseTranspose) <> '' then begin
         var M := CurrentMatrix.Inverse.Transpose;
         SetShaderVariable(material.GetMaterialProperty(TMaterial.TProperty.ModelViewInverseTranspose), M);
      end;
   end;

begin
   CurrentOpacity := opacity;
   if material <> nil then
      CurrentMaterial := material
   else begin
      CurrentMaterial := DefaultMaterial;
      if CurrentMaterial is TColorMaterial then
         TColorMaterial(CurrentMaterial).Color := ColorComposeAlpha(claRed, opacity);
   end;
   if CurrentMaterial <> nil then
      InnerApplyMaterial;
end;

// ResetMaterial
//
procedure TFMXUContext3D.ResetMaterial;
begin
   if CurrentMaterial <> nil then
      CurrentMaterial.Reset(Self);
end;

// AddResource
//
class function TFMXUContext3D.AddResource(const aResource : IInterface) : THandle;
begin
   vResourceLock.BeginWrite;
   try
      // this implementation maps FMX behavior of the handle being an index in the list
      // should probably be changed to use meaningless handles, but I'm unsure of side-effects yet
      for var i := 1 to High(vResourceList) do begin
         if vResourceList[i] = nil then begin
            vResourceList[i] := aResource;
            Exit(i);
         end;
      end;

      Result := Length(vResourceList);
      if Result = 0 then
         Result := 1; // handle 0 is invalid, leave a hole
      SetLength(vResourceList, Result + 1);
      vResourceList[Result] := aResource;
   finally
      vResourceLock.EndWrite;
   end;
end;

// GetResource
//
class function TFMXUContext3D.GetResource(aResourceHandle : THandle) : IInterface;
begin
   vResourceLock.BeginRead;
   try
      if aResourceHandle > 0 then begin
         Assert(Cardinal(aResourceHandle) < Cardinal(Length(vResourceList)));
         Result := vResourceList[aResourceHandle];
         Assert(Result <> nil);
      end else Result := nil;
   finally
      vResourceLock.EndRead;
   end;
end;

// RemoveResource
//
class procedure TFMXUContext3D.RemoveResource(aResourceHandle : THandle);
begin
   vResourceLock.BeginWrite;
   try
      if (aResourceHandle > 0) and not vContextFinalized then begin
         Assert(Cardinal(aResourceHandle) < Cardinal(Length(vResourceList)));
         Assert(vResourceList[aResourceHandle] <> nil);
         vResourceList[aResourceHandle] := nil;
      end;
   finally
      vResourceLock.EndWrite;
   end;
end;

// ------------------
// ------------------ TFMXUContext3DHelper ------------------
// ------------------

// ApplyMaterial
//
procedure TFMXUContext3DHelper.ApplyMaterial(const material: TMaterial; const opacity: Single);
begin
   GetFMXUContext.ApplyMaterial(material, opacity);
end;

// ResetMaterial
//
procedure TFMXUContext3DHelper.ResetMaterial;
begin
   GetFMXUContext.ResetMaterial;
end;

// DrawGPUPrimitives
//
procedure TFMXUContext3DHelper.DrawGPUPrimitives(
   const aKind : TPrimitivesKindU;
   const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer
   );
begin
   GetFMXUContext.DrawGPUPrimitives(aKind, vertices, indices);
end;

// DrawPrimitives
//
procedure TFMXUContext3DHelper.DrawPrimitives(const aKind : TPrimitivesKind; const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer; const material : TMaterial; const opacity : Single);
begin
   if SupportsGPUPrimitives then begin
      ApplyMaterial(material, opacity);
      DrawGPUPrimitives(TPrimitivesKindU(aKind), vertices, indices);
      ResetMaterial;
   end else begin
      var vb := vertices.Lock(Self);
      try
         var ib := indices.Lock(Self);
         try
            case aKind of
               TPrimitivesKind.Points : inherited DrawPoints(vb, ib, material, opacity);
               TPrimitivesKind.Lines : inherited DrawLines(vb, ib, material, opacity);
               TPrimitivesKind.Triangles : inherited DrawTriangles(vb, ib, material, opacity);
            else
               Assert(False);
            end;
         finally
            indices.UnLock(Self);
         end;
      finally
         vertices.Unlock(Self);
      end;
   end;
end;

// DrawTriangles
//
procedure TFMXUContext3DHelper.DrawTriangles(const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer; const material : TMaterial; const opacity : Single);
begin
   DrawPrimitives(TPrimitivesKind.Triangles, vertices, indices, material, opacity);
end;

// DrawLines
//
procedure TFMXUContext3DHelper.DrawLines(const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer; const material : TMaterial; const opacity : Single);
begin
   DrawPrimitives(TPrimitivesKind.Lines, vertices, indices, material, opacity);
end;

// DrawPoints
//
procedure TFMXUContext3DHelper.DrawPoints(const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer; const material : TMaterial; const opacity : Single);
begin
   DrawPrimitives(TPrimitivesKind.Points, vertices, indices, material, opacity);
end;

// SupportsGPUPrimitives
//
function TFMXUContext3DHelper.SupportsGPUPrimitives : Boolean;
begin
   Result := Self is TFMXUContext3D;
end;

// GetFMXUContext
//
function TFMXUContext3DHelper.GetFMXUContext : TFMXUContext3D;
begin
   Assert(Self is TFMXUContext3D);
   Result := TFMXUContext3D(Self);
end;

// ------------------
// ------------------ TIContextShaderSource ------------------
// ------------------

// Create
//
constructor TIContextShaderSource.Create(const aSource : TContextShaderSource; minVariableSlotSize : Integer);
begin
   inherited Create;
   FID := AtomicIncrement(vIDCounter);
   FSource := aSource;
   FVariablesSize := 0;
   FMaxTextureSlot := -1;
   for var i := 0 to High(Variables) do begin
      case Variables[i].Kind of
         TContextShaderVariableKind.Float .. TContextShaderVariableKind.Matrix : begin
            var size := Variables[i].Size;
            if size < minVariableSlotSize then
               Inc(FVariablesSize, minVariableSlotSize)
            else Inc(FVariablesSize, size);
         end;
         TContextShaderVariableKind.Texture : begin
            if Variables[i].Index > FMaxTextureSlot then
               FMaxTextureSlot := Variables[i].Index;
         end;
      else
         Assert(False);
      end;
   end;
end;

// IndexOfVariable
//
function TIContextShaderSource.IndexOfVariable(const name : String) : Integer;
begin
   for var i := 0 to High(Variables) do begin
      if SameText(Variables[i].Name, name) then
         Exit(i);
   end;
   Result := -1;
end;

// GetVariablesCount
//
function TIContextShaderSource.GetVariablesCount : Integer;
begin
   Result := Length(Variables);
end;

// GetSize
//
function TIContextShaderSource.GetSize(index : Integer) : Cardinal;
begin
   Result := Variables[index].Size;
end;

// GetIndex
//
function TIContextShaderSource.GetIndex(index : Integer) : Cardinal;
begin
   Result := Variables[index].Index;
end;

// SpecializeForVertexDeclaration
//
function TIContextShaderSource.SpecializeForVertexDeclaration(const declaration : TVertexDeclaration) : IContextShaderSource;
begin
   Result := Self;
end;

// GetSelf
//
function TIContextShaderSource.GetSelf : TIContextShaderSource;
begin
   Result := Self;
end;

// GetID
//
function TIContextShaderSource.GetID : NativeUInt;
begin
   Result := FID;
end;

// GetVariablesSize
//
function TIContextShaderSource.GetVariablesSize : Integer;
begin
   Result := FVariablesSize;
end;

// GetMaxTextureSlot
//
function TIContextShaderSource.GetMaxTextureSlot : Integer;
begin
   Result := FMaxTextureSlot;
end;

// GetUserData
//
function TIContextShaderSource.GetUserData : IInterface;
begin
   Result := FUserData;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

   vContextFinalized := True;

end.

