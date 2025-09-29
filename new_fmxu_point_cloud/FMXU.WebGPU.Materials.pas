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
{    Compatibility shaders for WebGPU                                  }
{                                                                      }
{**********************************************************************}
unit FMXU.WebGPU.Materials;

interface

uses
   System.SysUtils, System.Math, System.Math.Vectors,
   FMX.Types3D, FMX.Materials, FMX.MaterialSources,
   FMXU.Context, FMXU.WebGPU.Utils, FMXU.Buffers, FMXU.Colors;

type
   //: Interposer class used to use TLightMaterialWebGPU
   TLightMaterialSource = class (FMX.MaterialSources.TLightMaterialSource)
      protected
          function CreateMaterial: TMaterial; override;
   end;

   //: Implementation of TLightMaterial with WGSL shaders
   TLightMaterialWebGPU = class (TLightMaterial)
      protected
         FLastNbLights : Integer;
         FVertexShaders : array [0..8] of TContextShader;
         FPixelShaderWebGPU : TContextShader;

         procedure DoApply(const aContext : TContext3D); override;
         procedure DoInitialize; override;
   end;

function GetWebGPUCompatibleShaderSource(const aShader : TContextShader) : IContextShaderSource;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// -----------------------------------------------------------------

uses FMXU.Context.WebGPU;

var
   // TColorMaterial replacement
   vColorVertexShaderSource : IContextShaderSource;
   vColorFragmentShaderSource : IContextShaderSource;
   // TextureMaterial replacement
   vTextureVertexShaderSource : IContextShaderSource;
   vTextureFragmentShaderSource : IContextShaderSource;
   // LightMaterial replacement
   vLightVertexShaderSource : array [0..7] of IContextShaderSource;
   vLightFragmentShaderSource : IContextShaderSource;

// Initialize
//
procedure Initialize;
begin
   vColorVertexShaderSource := TIWebGPUContextShaderSource.Create(TContextShaderSource.Create(TContextShaderArch_WGSL, TEncoding.UTF8.GetBytes(
      '''
      @group(0) @binding(0) var<uniform> MVPMatrix: mat4x4f;

      @vertex
      fn main(@location(0) vertex: vec3f) -> @builtin(position) vec4f {
          return MVPMatrix * vec4f(vertex, 1.0);
      }
      '''),
      [ TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4 * 4 * SizeOf(Single)) ]
   ), [ ]);
   vColorFragmentShaderSource := TIWebGPUContextShaderSource.Create(TContextShaderSource.Create(TContextShaderArch_WGSL, TEncoding.UTF8.GetBytes(
      '''
      @group(2) @binding(0) var<uniform> materialColor: vec4f;
      @group(2) @binding(1) var<uniform> opacity: f32;

      @fragment
      fn main() -> @location(0) vec4f {
          return materialColor * opacity;
      }
      '''),
      [  TContextShaderVariable.Create('materialColor', TContextShaderVariableKind.Vector, 0, 4 * SizeOf(Single)),
         TContextShaderVariable.Create('opacity', TContextShaderVariableKind.Float, 1, SizeOf(Single)) ]
   ), [ ]);

   vTextureVertexShaderSource := TIWebGPUContextShaderSource.Create(TContextShaderSource.Create(TContextShaderArch_WGSL, TEncoding.UTF8.GetBytes(
      '''
      @group(0) @binding(0) var<uniform> MVPMatrix: mat4x4f;

      struct VertexInput {
      <#VertexDeclaration#>
      };

      struct VertexOutput {
         @builtin(position) pos: vec4f,
         @location(0) texCoord0: vec2f
      };

      @vertex
      fn main(in : VertexInput) -> VertexOutput {
          var output: VertexOutput;
          output.pos = MVPMatrix * vec4f(in.vertex, 1.0);
          output.texCoord0 = in.texCoord0;
          return output;
      }
      '''),
      [ TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4 * 4 * SizeOf(Single)) ]
   ), [ ]);
   vTextureFragmentShaderSource := TIWebGPUContextShaderSource.Create(TContextShaderSource.Create(TContextShaderArch_WGSL, TEncoding.UTF8.GetBytes(
      '''
      @group(1) @binding(0) var texture0: texture_2d<f32>;
      @group(2) @binding(0) var<uniform> opacity: f32;
      @group(2) @binding(1) var sampler0: sampler;

      @fragment
      fn main(@location(0) texCoord: vec2f) -> @location(0) vec4f {
          //var texColor = textureLoad(texture0, vec2i(texCoord), 0);
          var texColor = textureSample(texture0, sampler0, texCoord);
          return texColor * opacity;
      }
      '''),
      [ TContextShaderVariable.Create('opacity', TContextShaderVariableKind.Float, 0, SizeOf(Single)),
        TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0) ]
   ), [ ]);
   vLightFragmentShaderSource := TIWebGPUContextShaderSource.Create(TContextShaderSource.Create(TContextShaderArch_WGSL, TEncoding.UTF8.GetBytes(
      '''
      @group(1) @binding(0) var texture0: texture_2d<f32>;
      @group(2) @binding(0) var<uniform> options: vec4f;
      @group(2) @binding(1) var sampler0: sampler;

      @fragment
      fn main(@location(0) texCoord: vec2f, @location(1) inColor: vec4f) -> @location(0) vec4f {
         if (options.y == 1) {
            return inColor * textureSample(texture0, sampler0, texCoord) * options.z;
         } else {
            return inColor * options.z;
         }
      }
      '''),
      [ TContextShaderVariable.Create('Options', TContextShaderVariableKind.Vector, 0, SizeOf(Single)*4),
        TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0) ]
    ), [ ]);
end;

function GetLightShaderSource(const nbLights : Integer): IContextShaderSource;
begin
   Assert(nbLights in [0..7]);
   if vLightVertexShaderSource[nbLights] <> nil then
      Exit(vLightVertexShaderSource[nbLights]);

   // Material: ( diffuse, specular, ambient, emission )
   // Options:  ( shininess, texture ? 1 : 0, opacity, unused )
   // LightsOptions: ( type, cos(spotCutOff), spotExponent, unused )
   // Light types: 1 directional, 2 point, 3 spot

   var code :=
      '''
      @group(0) @binding(0) var<uniform> MVPMatrix: mat4x4f;
      @group(0) @binding(1) var<uniform> ModelView: mat4x4f;
      @group(0) @binding(2) var<uniform> ModelViewIT: mat4x4f;
      @group(0) @binding(3) var<uniform> EyePos: vec4f;
      @group(0) @binding(4) var<uniform> Material: array<vec4f, 4>;
      @group(0) @binding(5) var<uniform> Options: vec4f;
      @group(0) @binding(6) var<uniform> LightsPos: array<vec4f, $nbLights$>;
      @group(0) @binding(7) var<uniform> LightsDir: array<vec4f, $nbLights$>;
      @group(0) @binding(8) var<uniform> LightsColor: array<vec4f, $nbLights$>;
      @group(0) @binding(9) var<uniform> LightsOptions: array<vec4f, $nbLights$>;

      struct VertexInput {
      <#VertexDeclaration#>
      };

      struct VertexOutput {
         @builtin(position) pos: vec4f,
         @location(0) texCoord0: vec2f,
         @location(1) color: vec4f
      };

      var<private> diffuse: f32;
      var<private> specular: f32;

      fn Lighting(normal: vec3f, eyeDir: vec3f, lightDir: vec3f, atten: f32) {
         var NdotL = dot(normal, lightDir);
         if (NdotL > 0) {
            diffuse += NdotL * atten;
            var halfVector = normalize(lightDir + eyeDir);
            var NdotH = max(0.0, dot(normal, halfVector));
            specular += pow(NdotH, Options.x) * atten;
         }
      };

      fn SpotLight(light: i32, normal : vec3f, vertexPos: vec3f, eyeDir: vec3f) {
         var lightDir = normalize(LightsPos[light].xyz - vertexPos);
         var spotDot = dot(-lightDir, LightsDir[light].xyz);
         if (spotDot > LightsOptions[light].y) {
            Lighting(
               normal, -eyeDir, lightDir,
               pow(spotDot, LightsOptions[light].z)
            );
         }
      };

      @vertex
      fn main(in : VertexInput) -> VertexOutput {
         var color : vec3f = Material[2].xyz + Material[3].xyz;

         var normal = normalize((ModelViewIT * vec4f(in.normal, 1)).xyz);
         var ecPosition = (ModelView * vec4f(in.vertex, 1)).xyz;
         var eye = normalize(ecPosition - EyePos.xyz);

         for (var light = 0; light < $nbLights$; light++) {
            diffuse = 0;
            specular = 0;

            var typ = LightsOptions[light].x;
            if (typ == 1) {
               Lighting(-normal, eye, LightsDir[light].xyz, 1);
            } else if (typ == 2) {
               var vp = LightsPos[light].xyz - ecPosition;
               Lighting(normal, -eye, normalize(vp), 1);
            } else if (typ == 3) {
               SpotLight(light, normal, ecPosition, eye);
            }

            color += diffuse * Material[0].xyz * LightsColor[light].xyz
                   + specular * Material[1].xyz;
         }

         var out : VertexOutput;
         out.color = vec4f(clamp(color, vec3f(0, 0, 0), vec3f(1, 1, 1)), 1);
         out.texCoord0 = in.texCoord0;
         out.pos = MVPMatrix * vec4f(in.vertex, 1);
         return out;
      };
      ''';
   vLightVertexShaderSource[nbLights] := TIWebGPUContextShaderSource.Create(TContextShaderSource.Create(TContextShaderArch_WGSL,
      TEncoding.UTF8.GetBytes(
         StringReplace(code, '$nbLights$', IntToStr(nbLights), [ rfReplaceAll ])
      ),
      [
         TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, SizeOf(Single)*4*4),
         TContextShaderVariable.Create('ModelView', TContextShaderVariableKind.Matrix, 1, SizeOf(Single)*4*4),
         TContextShaderVariable.Create('ModelViewIT', TContextShaderVariableKind.Matrix, 2, SizeOf(Single)*4*4),
         TContextShaderVariable.Create('EyePos', TContextShaderVariableKind.Vector, 3, SizeOf(Single)*4),
         TContextShaderVariable.Create('Material', TContextShaderVariableKind.Vector, 4, SizeOf(Single)*4*4),
         TContextShaderVariable.Create('Options', TContextShaderVariableKind.Vector, 5, SizeOf(Single)*4),
         TContextShaderVariable.Create('LightsPos', TContextShaderVariableKind.Vector, 6, SizeOf(Single)*4*nbLights),
         TContextShaderVariable.Create('LightsDir', TContextShaderVariableKind.Vector, 7, SizeOf(Single)*4*nbLights),
         TContextShaderVariable.Create('LightsColor', TContextShaderVariableKind.Vector, 8, SizeOf(Single)*4*nbLights),
         TContextShaderVariable.Create('LightsOptions', TContextShaderVariableKind.Vector, 9, SizeOf(Single)*4*nbLights)
      ]
    ), [ ]);
    Result := vLightVertexShaderSource[nbLights];
end;

// GetWebGPUCompatibleShaderSource
//
function GetWebGPUCompatibleShaderSource(const aShader : TContextShader) : IContextShaderSource;
begin
   case aShader.Kind of
      TContextShaderKind.VertexShader: begin
         if aShader.Name = 'color.fvs' then
            Result := vColorVertexShaderSource
         else if aShader.Name = 'texture.fvs' then
            Result := vTextureVertexShaderSource
         else if aShader.Name.StartsWith('gouraud') then
            Result := GetLightShaderSource(Ord(aShader.Name[8]) - Ord('0'));
      end;
      TContextShaderKind.PixelShader: begin
         if aShader.Name = 'color.fps' then
            Result := vColorFragmentShaderSource
         else if aShader.Name = 'texture.fps' then
            Result := vTextureFragmentShaderSource;
      end;
   end;
end;

// ------------------
// ------------------ TLightMaterialWebGPU ------------------
// ------------------

// DoApply
//
procedure TLightMaterialWebGPU.DoApply(const aContext : TContext3D);
var
   lights : array [0..7] of TLightDescription;
begin
   if not aContext.InheritsFrom(TFMXUContext3D_WebGPU) then begin
      inherited;
      Exit;
   end;

   // collect up to Min(8, MaxLightCount) enabled lights
   var nbLights := 0;
   for var i := 0 to aContext.Lights.Count-1 do begin
      // aContext.Lights is a generic list of records, each access is a record copy
      // so we copy the record locally to avoid further copies
      lights[nbLights] := aContext.Lights[i];
      if lights[nbLights].Enabled then begin
         Inc(nbLights);
         if (nbLights > Length(lights)) or (nbLights = aContext.MaxLightCount) then
            Break;
      end;
   end;

   aContext.SetShaders(FVertexShaders[nbLights], FPixelShaderWebGPU);

   var materialColors : array [0..3] of TVector3D;
   materialColors[0] := ColorToVector3D(Diffuse);
   materialColors[1] := ColorToVector3D(Specular);
   materialColors[2] := ColorToVector3D(Ambient);
   materialColors[3] := ColorToVector3D(Emissive);
   aContext.SetShaderVariable('Material', materialColors);

   var options := Vector3D(Shininess, 0, aContext.CurrentOpacity, 0);
   if (Texture <> nil) and not Texture.IsEmpty then
      options.Y := 1;
   aContext.SetShaderVariable('Options', options);

   aContext.SetShaderVariable('EyePos', [ aContext.CurrentCameraInvMatrix.M[3] ]);

   var lightPos, lightDir, lightColor, lightOptions : array [0..7] of TVector3D;

   for var i := 0 to nbLights-1 do begin
      lightPos[i] := lights[i].Position;
      lightDir[i] := lights[i].Direction;
      lightColor[i] := ColorToVector3D(lights[i].Color);
      lightOptions[i] := Vector3D(
         Ord(lights[i].LightType) + 1, Cos(DegToRad(lights[i].SpotCutoff)),
         lights[i].SpotExponent, 0
      );
   end;

   aContext.SetShaderVariable('LightsPos', lightPos);
   aContext.SetShaderVariable('LightsDir', lightDir);
   aContext.SetShaderVariable('LightsColor', lightColor);
   aContext.SetShaderVariable('LightsOptions', lightOptions);

   aContext.SetShaderVariable('texture0', Texture);
end;

// DoInitialize
//
procedure TLightMaterialWebGPU.DoInitialize;
begin
   inherited;

   // these are just placeholders here
   // lazy initialization magic happens in GetLightShaderSource
   for var i := 0 to 8 do
      FVertexShaders[i] := TShaderManager.RegisterShaderFromData('gouraud' + IntToStr(i) + '.fvs', TContextShaderKind.VertexShader, '', []);

   // Do not use FPixelShader field, as we may be in fallback mode (not a WebGPU context)
   FPixelShaderWebGPU := TShaderManager.RegisterShaderFromData(
      'gouraud.fps', TContextShaderKind.PixelShader, 'gouraud.fps',
      [ vLightFragmentShaderSource.GetSelf.Source ]
   );
end;

// ------------------
// ------------------ TLightMaterialSource ------------------
// ------------------

// CreateMaterial
//
function TLightMaterialSource.CreateMaterial: TMaterial;
begin
   Result := TLightMaterialWebGPU.Create;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   Initialize;

end.
