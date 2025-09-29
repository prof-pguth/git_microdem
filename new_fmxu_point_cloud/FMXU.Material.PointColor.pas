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
{    Material & shaders for TPointCloud3D component                    }
{                                                                      }
{**********************************************************************}
unit FMXU.Material.PointColor;

{$i fmxu.inc}

interface

uses
   System.SysUtils, System.Math.Vectors,
   FMX.Types3D, FMX.Materials, FMX.MaterialSources;

type
   (*
   Shader support class for PointCloud component,
   not meant to be used independently at the moment
   *)

   TPointColorShape = ( pcsQuad, pcsPoint, pcsDisc, pcsGaussian );

   TPointColorMaterialSource = class(TMaterialSource)
      private
         FRightVector : TVector3D;
         FUpVector : TVector3D;
         FShape : TPointColorShape;
      protected
         function CreateMaterial: TMaterial; override;
         procedure SetRightVector(const val : TVector3D);
         procedure SetUpVector(const val : TVector3D);
         procedure SetShape(const val : TPointColorShape);
      public
         property RightVector : TVector3D read FRightVector write SetRightVector;
         property UpVector : TVector3D read FUpVector write SetUpVector;
         property Shape : TPointColorShape read FShape write SetShape;

   end;

   TPointColorMaterial = class(TMaterial)
      private
         FRightVector : TVector3D;
         FUpVector : TVector3D;
         FShape : TPointColorShape;

         FVertexShader: TContextShader;
         FPixelShader: TContextShader;

      protected
         procedure PrepareShaders;
         procedure PreparePointShaders;
         procedure PrepareQuadsShaders;
         procedure PrepareUVShaders;
         procedure ClearShaders;

         procedure DoInitialize; override;
         procedure DoApply(const Context: TContext3D); override;
         class function DoGetMaterialProperty(const Prop: TMaterial.TProperty): string; override;

         property UpVector : TVector3D read FUpVector write FUpVector;
         property RightVector : TVector3D read FRightVector write FRightVector;
         property Shape : TPointColorShape read FShape write FShape;

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

uses FMXU.D3DShaderCompiler, FMXU.Context;

// ------------------
// ------------------ TPointColorMaterialSource ------------------
// ------------------

// CreateMaterial
//
function TPointColorMaterialSource.CreateMaterial: TMaterial;
begin
   var vm := TPointColorMaterial.Create;
   vm.RightVector := RightVector;
   vm.UpVector := UpVector;
   vm.Shape := FShape;
   Result := vm;
end;

// SetRightVector
//
procedure TPointColorMaterialSource.SetRightVector(const val : TVector3D);
begin
   TPointColorMaterial(Material).RightVector := val;
end;

// SetUpVector
//
procedure TPointColorMaterialSource.SetUpVector(const val : TVector3D);
begin
   TPointColorMaterial(Material).UpVector := val;
end;

// SetShape
//
procedure TPointColorMaterialSource.SetShape(const val : TPointColorShape);
begin
   if FShape <> val then begin
      FShape := val;
      var vcm := TPointColorMaterial(Material);
      if vcm <> nil then begin
         vcm.Shape := val;
         vcm.ClearShaders;
      end;
   end;
end;

// ------------------
// ------------------ TPointColorMaterial ------------------
// ------------------

// Destroy
//
destructor TPointColorMaterial.Destroy;
begin
   inherited;
   ClearShaders;
end;

// DoInitialize
//
procedure TPointColorMaterial.PrepareShaders;
begin
   inherited;

   case Shape of
      pcsQuad : PrepareQuadsShaders;
      pcsPoint : PreparePointShaders;
      pcsDisc, pcsGaussian : PrepareUVShaders;
   else
      Assert(False);
   end;
end;

// PreparePointShaders
//
procedure TPointColorMaterial.PreparePointShaders;
begin
   var arch := ContextShaderArchSimplified;
   if arch = TContextShaderArch.DX11 then begin

      FVertexShader := CreateContextShader(TContextShaderKind.VertexShader,
         '''
         float4x4 MVPMatrix;
         void main(float4 inPos : POSITION0, float4 inColor : COLOR0,
            out float4 outPos : SV_POSITION0, out float4 outColor : COLOR0
         )
         {
            outPos = mul(MVPMatrix, inPos);
            outColor = float4(inColor.rgb, 1);
         }
         ''', [ ], [ ]
      );
      FPixelShader := CreateContextShader(TContextShaderKind.PixelShader,
         '''
         float4 main(float4 pos : SV_POSITION0, float4 color : COLOR0) : SV_Target
         {
            return color;
         }
         ''', [], []
      );

   end else if arch = TContextShaderArch.GLSL then begin

      FVertexShader := CreateContextShader(TContextShaderKind.VertexShader,
         '''
         uniform vec4 _MVPMatrix[4];

         attribute vec4 a_Position;
         attribute vec4 a_Color;

         varying vec4 outColor;

         void main()
         {
             gl_Position.x = dot(_MVPMatrix[0], a_Position);
             gl_Position.y = dot(_MVPMatrix[1], a_Position);
             gl_Position.z = dot(_MVPMatrix[2], a_Position);
             gl_Position.w = dot(_MVPMatrix[3], a_Position);

             outColor = vec4(a_Color.rgb, 1.0);
         }
         ''', [ ], [ ]
      );
      FPixelShader := CreateContextShader(TContextShaderKind.PixelShader,
         '''
         varying vec4 outColor;

         void main()
         {
             gl_FragColor = outColor;
         }
         ''', [], []
      );

   end else if arch = TContextShaderArch_WGSL then begin

      FVertexShader := CreateContextShader(TContextShaderKind.VertexShader,
         '''
         @group(0) @binding(0) var<uniform> MVPMatrix: mat4x4f;

         struct VertexInput {
            @location(0) pos: vec3f,
            @location(1) color: vec4f
         };

         struct VertexOutput {
            @builtin(position) pos: vec4f,
            @location(0) color: vec4f
         };

         @vertex
         fn main(in : VertexInput) -> VertexOutput {
            var out: VertexOutput;
            out.pos = MVPMatrix * vec4f(in.pos, 1.0);
            out.color = in.color;
            return out;
         }
         ''', [ ], [ ]
      );
      FPixelShader := CreateContextShader(TContextShaderKind.PixelShader,
         '''
         @fragment
         fn main(@location(0) color: vec4f) -> @location(0) vec4f  {
            return color;
         }
         ''', [], []
      );

   end else Assert(False);
end;

// PrepareQuadsShaders
//
procedure TPointColorMaterial.PrepareQuadsShaders;
begin
   var arch := ContextShaderArchSimplified;
   if arch = TContextShaderArch.DX11 then begin

      FVertexShader := CreateContextShader(TContextShaderKind.VertexShader,
         '''
         float4x4 MVPMatrix;
         float4 rightVector, upVector;
         void main(
            float4 inPos : POSITION0,         float4 inColor : COLOR0,
            out float4 outPos : SV_POSITION0, out float4 outColor : COLOR0
         )
         {
            if (inColor.a < 0.3) {
               if (inColor.a < 0.1) {
                  inPos = inPos - rightVector - upVector;
               } else {
                  inPos = inPos + rightVector - upVector;
               }
            } else {
               if (inColor.a < 0.6) {
                  inPos = inPos + rightVector + upVector;
               } else {
                  inPos = inPos - rightVector + upVector;
               }
            }
            outPos = mul(MVPMatrix, inPos);
            outColor = float4(inColor.rgb, 1);
         }
         ''', [
            'rightVector',                      'upVector'
         ], [
            TContextShaderVariableKind.Vector,  TContextShaderVariableKind.Vector
         ]
      );
      FPixelShader := CreateContextShader(TContextShaderKind.PixelShader,
         '''
         float4 main(float4 pos : SV_POSITION0, float4 color : COLOR0) : SV_Target
         {
            return color;
         }
         ''', [], []
      );

   end else if arch = TContextShaderArch.GLSL then begin

      FVertexShader := CreateContextShader(TContextShaderKind.VertexShader,
         '''
         uniform vec4 _MVPMatrix[4];
         uniform vec4 _rightVector;
         uniform vec4 _upVector;

         attribute vec4 a_Position;
         attribute vec4 a_Color;

         varying vec4 outColor;

         void main()
         {
             vec4 modifiedPos = a_Position;

             if (a_Color.a < 0.3) {
                 if (a_Color.a < 0.1) {
                     modifiedPos = modifiedPos - _rightVector - _upVector;
                 } else {
                     modifiedPos = modifiedPos + _rightVector - _upVector;
                 }
             } else {
                 if (a_Color.a < 0.6) {
                     modifiedPos = modifiedPos + _rightVector + _upVector;
                 } else {
                     modifiedPos = modifiedPos - _rightVector + _upVector;
                 }
             }

             gl_Position.x = dot(_MVPMatrix[0], modifiedPos);
             gl_Position.y = dot(_MVPMatrix[1], modifiedPos);
             gl_Position.z = dot(_MVPMatrix[2], modifiedPos);
             gl_Position.w = dot(_MVPMatrix[3], modifiedPos);

             outColor = vec4(a_Color.rgb, 1.0);
         }
         ''', [
            'rightVector',                      'upVector'
         ], [
            TContextShaderVariableKind.Vector,  TContextShaderVariableKind.Vector
         ]
      );
      FPixelShader := CreateContextShader(TContextShaderKind.PixelShader,
         '''
         varying vec4 outColor;

         void main()
         {
             gl_FragColor = outColor;
         }
         ''', [], []
      );

   end else if arch = TContextShaderArch_WGSL then begin

      FVertexShader := CreateContextShader(TContextShaderKind.VertexShader,
         '''
         @group(0) @binding(0) var<uniform> MVPMatrix: mat4x4f;
         @group(0) @binding(1) var<uniform> rightVector: vec4f;
         @group(0) @binding(2) var<uniform> upVector: vec4f;

         struct VertexInput {
            @location(0) pos: vec3f,
            @location(1) color: vec4f
         };

         struct VertexOutput {
            @builtin(position) pos: vec4f,
            @location(0) color: vec4f
         };

         @vertex
         fn main(in : VertexInput) -> VertexOutput {

            var modifiedPos = vec4f(in.pos, 1);

            if (in.color.a < 0.3) {
              if (in.color.a < 0.1) {
                  modifiedPos = modifiedPos - rightVector - upVector;
              } else {
                  modifiedPos = modifiedPos + rightVector - upVector;
              }
            } else {
              if (in.color.a < 0.6) {
                  modifiedPos = modifiedPos + rightVector + upVector;
              } else {
                  modifiedPos = modifiedPos - rightVector + upVector;
              }
            }

            var out: VertexOutput;
            out.pos = MVPMatrix * modifiedPos;
            out.color = vec4f(in.color.rgb, 1.0);
            return out;
         }
         ''', [
            'rightVector',                      'upVector'
         ], [
            TContextShaderVariableKind.Vector,  TContextShaderVariableKind.Vector
         ]
      );
      FPixelShader := CreateContextShader(TContextShaderKind.PixelShader,
         '''
         @fragment
         fn main(@location(0) color: vec4f) -> @location(0) vec4f  {
            return color;
         }
         ''', [], []
      );

   end else begin
      Assert(False);
   end;
end;

// PrepareUVShaders
//
procedure TPointColorMaterial.PrepareUVShaders;
begin
   var colorCode : AnsiString := '';
   if Shape = pcsGaussian then begin
      colorCode :=
         '''
         color.a = exp(- r2) * (1 - r2);
         color.rgb *= color.a;
         ''';
   end;
   var arch := ContextShaderArchSimplified;
   if arch = TContextShaderArch.DX11 then begin

      FVertexShader := CreateContextShader(TContextShaderKind.VertexShader,
         '''
         float4x4 MVPMatrix;
         float4 rightVector, upVector;
         void main(
            float4 inPos : POSITION0,
            float4 inColor : COLOR0,
            out float4 outPos : SV_POSITION0,
            out float4 outColor : COLOR0,
            out float2 uv : TEXCOORD0
         )
         {
            if (inColor.w < 0.3) {
               if (inColor.w < 0.1) {
                  inPos = inPos - rightVector - upVector;
                  uv = float2(-1.0, -1.0);
               } else {
                  inPos = inPos + rightVector - upVector;
                  uv = float2(+1.0, -1.0);
               }
            } else {
               if (inColor.w < 0.6) {
                  inPos = inPos + rightVector + upVector;
                  uv = float2(+1.0, +1.0);
               } else {
                  inPos = inPos - rightVector + upVector;
                  uv = float2(-1.0, +1.0);
               }
            }
            outPos = mul(MVPMatrix, inPos);
            outColor = float4(inColor.rgb, 1);
         }
         ''', [
            'rightVector',                      'upVector'
         ], [
            TContextShaderVariableKind.Vector,  TContextShaderVariableKind.Vector
         ]
      );
      FPixelShader := CreateContextShader(TContextShaderKind.PixelShader,
         '''
         float4 main(float4 pos : SV_POSITION0, float4 color : COLOR0, float2 uv : TEXCOORD0) : SV_Target
         {
            float r2 = dot(uv, uv);
            if (r2 >= 1.0)
               discard;
            else {
         '''
               + colorCode +
         '''
            }
            return color;
         }
         ''', [], []
      );

   end else if arch = TContextShaderArch.GLSL then begin

      FVertexShader := CreateContextShader(TContextShaderKind.VertexShader,
         '''
         uniform vec4 _MVPMatrix[4];
         uniform vec4 _rightVector;
         uniform vec4 _upVector;

         attribute vec4 a_Position;
         attribute vec4 a_Color;

         varying vec4 outColor;
         varying vec2 uv;

         void main()
         {
             vec4 modifiedPos = a_Position;

             if (a_Color.w < 0.3) {
                 if (a_Color.w < 0.1) {
                     modifiedPos = modifiedPos - _rightVector - _upVector;
                     uv = vec2(-1.0, -1.0);
                 } else {
                     modifiedPos = modifiedPos + _rightVector - _upVector;
                     uv = vec2(1.0, -1.0);
                 }
             } else {
                 if (a_Color.w < 0.6) {
                     modifiedPos = modifiedPos + _rightVector + _upVector;
                     uv = vec2(1.0, 1.0);
                 } else {
                     modifiedPos = modifiedPos - _rightVector + _upVector;
                     uv = vec2(-1.0, 1.0);
                 }
             }

             gl_Position.x = dot(_MVPMatrix[0], modifiedPos);
             gl_Position.y = dot(_MVPMatrix[1], modifiedPos);
             gl_Position.z = dot(_MVPMatrix[2], modifiedPos);
             gl_Position.w = dot(_MVPMatrix[3], modifiedPos);

             outColor = vec4(a_Color.rgb, 1.0);
         }
         ''', [
            'rightVector',                      'upVector'
         ], [
            TContextShaderVariableKind.Vector,  TContextShaderVariableKind.Vector
         ]
      );
      FPixelShader := CreateContextShader(TContextShaderKind.PixelShader,
         '''
         varying vec4 outColor;
         varying vec2 uv;

         void main()
         {
             float r2 = dot(uv, uv);
             if (r2 >= 1.0)
                 discard;
             else {
                 vec4 color = outColor;
         '''
                 + colorCode +
         '''
                 gl_FragColor = color;
             }
         }
         ''', [], []
      );

   end else if arch = TContextShaderArch_WGSL then begin

      FVertexShader := CreateContextShader(TContextShaderKind.VertexShader,
         '''
         @group(0) @binding(0) var<uniform> MVPMatrix: mat4x4f;
         @group(0) @binding(1) var<uniform> rightVector: vec4f;
         @group(0) @binding(2) var<uniform> upVector: vec4f;

         struct VertexInput {
            @location(0) pos: vec3f,
            @location(1) color: vec4f
         };

         struct VertexOutput {
            @builtin(position) pos: vec4f,
            @location(0) color: vec4f,
            @location(1) uv: vec2f
         };

         @vertex
         fn main(in : VertexInput) -> VertexOutput {

            var modifiedPos = vec4f(in.pos, 1);
            var uv: vec2f;

            if (in.color.a < 0.3) {
              if (in.color.a < 0.1) {
                  modifiedPos = modifiedPos - rightVector - upVector;
                  uv = vec2f(-1.0, -1.0);
              } else {
                  modifiedPos = modifiedPos + rightVector - upVector;
                  uv = vec2f(1.0, -1.0);
              }
            } else {
              if (in.color.a < 0.6) {
                  modifiedPos = modifiedPos + rightVector + upVector;
                  uv = vec2f(1.0, 1.0);
              } else {
                  modifiedPos = modifiedPos - rightVector + upVector;
                  uv = vec2f(-1.0, 1.0);
              }
            }

            var out: VertexOutput;
            out.pos = MVPMatrix * modifiedPos;
            out.color = vec4f(in.color.rgb, 1.0);
            out.uv = uv;
            return out;
         }
         ''', [
            'rightVector',                      'upVector'
         ], [
            TContextShaderVariableKind.Vector,  TContextShaderVariableKind.Vector
         ]
      );
      if Shape = pcsGaussian then begin
         colorCode :=
            '''
            color.a = exp(1 - r2) * (1 - r2);
            ''';
      end;
      FPixelShader := CreateContextShader(TContextShaderKind.PixelShader,
         '''
         @fragment
         fn main(@location(0) inColor: vec4f, @location(1) uv: vec2f) -> @location(0) vec4f {
            var r2 = dot(uv, uv);
            var color = inColor;
            if (r2 >= 1.0) {
               discard;
            } else {
         '''
               + colorCode +
         '''
            };
            return color;
         }
         ''', [], []
      );

   end else Assert(False);
end;

// ClearShaders
//
procedure TPointColorMaterial.ClearShaders;
begin
   FreeAndNil(FVertexShader);
   FreeAndNil(FPixelShader);
end;

// DoInitialize
//
procedure TPointColorMaterial.DoInitialize;
begin
   // nothing
end;

// DoGetMaterialProperty
//
class function TPointColorMaterial.DoGetMaterialProperty(const Prop: TMaterial.TProperty): string;
begin
   case Prop of
      TProperty.ModelViewProjection: Result := 'MVPMatrix';
   else
      Result := '';
   end;
end;

// DoApply
//
procedure TPointColorMaterial.DoApply(const Context: TContext3D);
begin
   if FVertexShader = nil then
      PrepareShaders;

   Context.SetShaders(FVertexShader, FPixelShader);
   if Shape <> pcsPoint then begin
      Context.SetShaderVariable('rightVector', [ FRightVector ]);
      Context.SetShaderVariable('upVector', [ FUpVector ]);
   end;
end;

end.
