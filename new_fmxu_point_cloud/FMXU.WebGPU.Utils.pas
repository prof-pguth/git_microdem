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
{    DirectX 11 support utilities                                      }
{                                                                      }
{**********************************************************************}
unit FMXU.WebGPU.Utils;

{$i fmxu.inc}

interface

uses
   Winapi.Windows,
   WebGPU, WebGPU.Interfaces,
   System.Math, System.SysUtils, System.UITypes, System.Math.Vectors,
   System.Classes,
   FMX.Types3D, FMX.Types,
   FMXU.Context;

type

   TWebGPUDevice = class;

   TWebGPUViewPort = record
      X, Y, Width, Height : Single;
      MinDepth, MaxDepth : Single;
   end;

   IWebGPUCompiledShader = interface
      ['{CE653FC9-6993-4CC7-B6B9-B5EBFEE4602B}']
      //: Per process unique ID
      function GetID : NativeUInt;
      function GetKind : TContextShaderKind;
      function GetShaderSource : IContextShaderSource;
      function GetShaderModule : IWGPUShaderModule;
      function GetBindGroupLayout : IWGPUBindGroupLayout;
      function GetMaxTextureSlot : Integer;
      function GetUniformsSize : Cardinal;
      function GetVariableCount : Integer;
      function FindVariable(const name : String; var offset, size : UInt64) : Boolean;
      procedure GetOffsetAndSize(index : Integer; var offset, size : UInt64);
      function FindTextureVariable(const name : String; var slot : Integer) : Boolean;
      function GetSpecializedModule(const vertexDeclaration : TVertexDeclaration; const aDevice : TWebGPUDevice) : IWGPUShaderModule;
   end;

   TWebGPUSpecialization = record
      Declaration : TVertexDeclaration;
      Module : IWGPUShaderModule;
   end;

   TWebGPUCompiledShader = class (TInterfacedObject, IWebGPUCompiledShader)
      protected
         class var vIDCounter : NativeUInt;
      protected
         FID : NativeUInt;
         FKind : TContextShaderKind;
         FShaderSource : IContextShaderSource;
         FShaderModule : IWGPUShaderModule;
         FSpecializations : TArray<TWebGPUSpecialization>;
         FBindGroupLayout : IWGPUBindGroupLayout;
         FMaxTextureSlot : Integer;
         FUniformsSize : Cardinal;
         FPaddingSize : Cardinal;
         FOffsets, FSizes : TArray<Cardinal>;

         function CompileShaderModule(const aShader : IContextShaderSource; const aDevice : TWebGPUDevice) : IWGPUShaderModule;

      public
         constructor Create(
            aKind : TContextShaderKind;
            const aShader : IContextShaderSource;
            const aDevice : TWebGPUDevice;
            baseShader : Boolean
            );

         procedure GetOffsetAndSize(index : Integer; var offset, size : UInt64); inline;
         function GetVariableCount : Integer;
         function FindVariable(const name : String; var offset, size : UInt64) : Boolean;
         function FindTextureVariable(const name : String; var slot : Integer) : Boolean;
         function GetSpecializedModule(const vertexDeclaration : TVertexDeclaration; const aDevice : TWebGPUDevice) : IWGPUShaderModule;

         function GetID : NativeUInt;
         function GetKind : TContextShaderKind;
         function GetShaderSource : IContextShaderSource;
         function GetShaderModule : IWGPUShaderModule;
         function GetBindGroupLayout : IWGPUBindGroupLayout;
         function GetMaxTextureSlot : Integer;
         function GetUniformsSize : Cardinal;
   end;

   IWebGPUMappedBuffer = interface
      ['{8DBF7A6B-10E3-45A7-93F9-62EF2FC6179A}']
      function Status : TWGPUMapAsyncStatus;
      procedure Wait;
      function Data : Pointer;
      function Size : UInt64;
   end;

   TIWebGPUMappedBuffer = class (TInterfacedObject, IWebGPUMappedBuffer)
      private
         FBuffer : IWGPUBuffer;
         FStatus : TWGPUMapAsyncStatus;
         FData : Pointer;
         FOffset, FSize : NativeUInt;

      public
         class function Create(
            const aBuffer : IWGPUBuffer; const aMode: TWGPUMapMode;
            aOffset: NativeUInt; aSize: NativeUInt
         ) : IWebGPUMappedBuffer;
         destructor Destroy; override;

         function Status : TWGPUMapAsyncStatus;
         procedure Wait;
         function Data : Pointer;
         function Size : UInt64;
   end;

   // holds Instance, Device & Adapter, typically a singleton
   TWebGPUDevice = class
      protected
         class var vInstance : IWGPUInstance;

      protected
         FAdapter : IWGPUAdapter;
         FDevice : IWGPUDevice;
         FQueue : IWGPUQueue;

         FLog : TStrings;
         FMaxTextureSize : Integer;

         FTexturesBindGroupLayouts : TArray<IWGPUBindGroupLayout>;

      public
         constructor Create;
         destructor Destroy; override;

         class property Instance : IWGPUInstance read vInstance;

         property Adapter : IWGPUAdapter read FAdapter;
         property Device : IWGPUDevice read FDevice;
         property Queue : IWGPUQueue read FQueue;

         property Log : TStrings read FLog;

         property MaxTextureSize : Integer read FMaxTextureSize;

         // create a simple 2D texture, RGBA 32 bits, no mipmaps
         function CreateTexture2D_BGRA32(width, height : Cardinal; usage : TWGPUTextureUsage;
                                         const name : UTF8String = ''; sampleCount : Integer = 1) : IWGPUTexture; overload;
         function CreateTexture2D_BGRA32(width, height : Cardinal; const name : UTF8String = '') : IWGPUTexture; overload;
         // write a whole mip level of a 2D texture, RGBA 32bits
         procedure WriteToTexture2D_BGRA32(const texture : IWGPUTexture; width, height, mipLevel : Cardinal; data : Pointer);

         // create depth texture
         function CreateDepthStencilTexture(width, height : Cardinal; sampleCount : Integer) : IWGPUTexture;
         // create multisample texture for a render target texture
         function CreateMultiSample(width, height : Cardinal; format : TWGPUTextureFormat; sampleCount : Integer) : IWGPUTexture;

         // createa while texture view, for a 2D texture, base mip level
         function CreateBaseTextureView(const texture : IWGPUTexture; aspect : TWGPUTextureAspect; const name : UTF8String = '') : IWGPUTextureView;

         // create a buffer
         function CreateBuffer(dataSize : Cardinal; usage : TWGPUBufferUsage; const name : UTF8String = '') : IWGPUBuffer;
         function CreateBufferFromData(dataSize : Cardinal; usage : TWGPUBufferUsage; dataPointer : Pointer; const name : UTF8String = '') : IWGPUBuffer;

         function TextureBindGroupLayout(nbTextures : Integer) : IWGPUBindGroupLayout;

         // compile WGSL shader and raise exception in case of errors
         function CompileShaderModule(const wgslSource : UTF8String; const name : UTF8String = '') : IWGPUShaderModule; overload; inline;
         function CompileShaderModule(const wgslSourcePtr : PUTF8Char; wgslSourceLen : Integer; const name : UTF8String = '') : IWGPUShaderModule; overload;

         function CreateCommandEncoder(const name : UTF8String = '') : IWGPUCommandEncoder;
   end;

   // holds a particular pipeline configuration
   TWebGPUPipeline = class
      protected
         // State fields
         FVertexDeclaration : TVertexDeclaration;
         FVertexBufferStrideInBytes : Cardinal;
         ColorTargetState : TWGPUColorTargetState;
         BlendState : TWGPUBlendState;
         DepthStencilState : TWGPUDepthStencilState;
         StencilRef : Integer;
         Topology : TWGPUPrimitiveTopology;
         CullMode : TWGPUCullMode;
         VertexCompiled : IWebGPUCompiledShader;
         FFragmentCompiled : IWebGPUCompiledShader;
         TextureCount : Integer;
         FSampleCount : Integer;

         // Dynamic fields
         FVertexState : TWGPUVertexState;
         FVertexAttributes : TArray<TWGPUVertexAttribute>;
         FVertexBufferLayout : TWGPUVertexBufferLayout;

         FFragmentState : TWGPUFragmentState;

         PipelineLayout : IWGPUPipelineLayout;
         RenderPipeline : IWGPURenderPipeline;

         procedure Initialize;
         procedure SetContextState(aState : TContextState);
         procedure SetStencilOp(const aFail, aZFail, aZPass: TStencilOp);
         procedure SetStencilFunc(const aFunc : TStencilfunc; aRef, aMask : Cardinal);

         function SamePipeline(const aPipeline : TWebGPUPipeline) : Boolean;

         procedure SetVertexDeclaration(const aDeclaration : TVertexDeclaration; aVertexBufferStrideInBytes : Cardinal);
         procedure SetFragmentShader(const aShader : IWebGPUCompiledShader);

         property VertexDeclaration : TVertexDeclaration read FVertexDeclaration;

         constructor Instantiate(const ref : TWebGPUPipeline; const aDevice : TWebGPUDevice);
   end;

   // holds WebGPU pipeline states & layouts
   TWebGPUPipelineManager = class
      protected
         FDevice : TWebGPUDevice;

         FRenderPipeline : IWGPURenderPipeline;

         FPipelines : TArray<TWebGPUPipeline>;

         // working config, holds current states
         FCurrent : TWebGPUPipeline;

         FVertexBufferUniforms : IWGPUBuffer;

         // 0 is vertex uniforms, 1 is fragment textures, 2 is fragment uniforms
         FBindGroups : array [0..2] of IWGPUBindGroup;

         FFragmentBufferUniforms : IWGPUBuffer;

         FTextures : array [0..15] of IWGPUTexture;
         FTextureViews : array [0..15] of IWGPUTextureView;
         FSampler : IWGPUSampler;

         procedure SetStencilRef(ref : Integer);

      public
         constructor Create(const aDevice : TWebGPUDevice);
         destructor Destroy; override;

         procedure SetSampleCount(aCount : Integer);

         procedure SetContextState(aState : TContextState);

         property StencilRef : Integer write SetStencilRef;
         procedure SetStencilOp(const aFail, aZFail, aZPass: TStencilOp);
         procedure SetStencilFunc(const aFunc : TStencilfunc; aRef, aMask : Cardinal);

         procedure SetVertexDeclaration(const declaration : TVertexDeclaration;
                                        vertexBufferStrideInBytes : Cardinal);

         procedure SetTexture(slot : Integer; const aTexture : IWGPUTexture);
         procedure SetTextureVariable(const aName : String; const aTexture : IWGPUTexture);

         procedure Setup(const vertexShader, fragmentShader : IWebGPUCompiledShader);

         procedure SetShaderVariable(const aName : String; const data : array of TVector3D);

         procedure SetupRenderPipeline(topology : TPrimitivesKindU; indexFormat : TWGPUIndexFormat);

         procedure SetBindGroups(const renderPass : IWGPURenderPassEncoder);

         property RenderPipeline : IWGPURenderPipeline read FRenderPipeline;
   end;

   EFMXU_WebGPUException = class (Exception)
   end;

const
   cWebGPUWhite : TWGPUColor = ( r: 1.0; g: 1.0; b: 1.0; a: 1.0 );

function ColorToWGPUColor(const color : TColor) : TWGPUColor;

function PrimitiveKindUToWebGPUTopology(primitive : TPrimitivesKindU) : TWGPUPrimitiveTopology;
function PrimitiveKindToWebGPUTopology(primitive : TPrimitivesKind) : TWGPUPrimitiveTopology;

function StencilOpToWebGPUOp(stencilOp : TStencilOp) : TWGPUStencilOperation;
function StencilFuncToWebGPUComp(stencilFunc : TStencilFunc) : TWGPUCompareFunction;

function PixelFormatToWebGPUFormat(pf : TPixelFormat) : TWGPUTextureFormat;

function VertexDeclarationToWGSLVertexFields(const declaration : TVertexDeclaration) : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cDeviceLogMaxSize = 128;
   cDeviceLogTrimSize = 64;

   cLimits : TWGPULimits = (
      maxTextureDimension1D : 8192;
      maxTextureDimension2D : 8192;
      maxTextureDimension3D : 2048;
      maxTextureArrayLayers : 256;
      maxBindGroups : 4;
      maxBindingsPerBindGroup : 640;
      maxDynamicUniformBuffersPerPipelineLayout : 8;
      maxDynamicStorageBuffersPerPipelineLayout : 4;
      maxSampledTexturesPerShaderStage : 16;
      maxSamplersPerShaderStage : 16;
      maxStorageBuffersPerShaderStage : 8;
      maxStorageTexturesPerShaderStage : 4;
      maxUniformBuffersPerShaderStage : 12;
      maxUniformBufferBindingSize : 64 * 1024;  // 65536 bytes
      maxStorageBufferBindingSize : 128 * 1024 * 1024;  // 134217728 bytes (128 MB)
      minUniformBufferOffsetAlignment : 256;
      minStorageBufferOffsetAlignment : 256;
      maxVertexBuffers : 8;
      maxBufferSize : 256 * 1024 * 1024;  // 268435456 bytes (256 MB)
      maxVertexAttributes : 16;
      maxVertexBufferArrayStride : 2048;
      maxInterStageShaderVariables : 16;
      maxColorAttachments : 8;
      maxColorAttachmentBytesPerSample : 32;
      maxComputeWorkgroupStorageSize : 16384;  // 16384 bytes
      maxComputeInvocationsPerWorkgroup : 256;
      maxComputeWorkgroupSizeX : 256;
      maxComputeWorkgroupSizeY : 256;
      maxComputeWorkgroupSizeZ : 64;
      maxComputeWorkgroupsPerDimension : 65535;
   );

// RaiseIfFailed
//
procedure RaiseIfFailed(hr : HResult; const msg : String);
begin
   if Failed(hr) then
      raise EFMXU_WebGPUException.CreateFmt('Failed (0x%x), %s', [ hr, msg ]);
end;

// ColorToWGPUColor
//
function ColorToWGPUColor(const color : TColor) : TWGPUColor;
begin
   var f : Double := 1/255;
   Result.r := TAlphaColorRec(Color).R * f;
   Result.g := TAlphaColorRec(Color).G * f;
   Result.b := TAlphaColorRec(Color).B * f;
   Result.a := TAlphaColorRec(Color).A * f;
end;

// PrimitiveKindUToWebGPUTopology
//
function PrimitiveKindUToWebGPUTopology(primitive : TPrimitivesKindU) : TWGPUPrimitiveTopology;
const
   cMap : array [ TPrimitivesKindU ] of TWGPUPrimitiveTopology = (
      WGPUPrimitiveTopology_PointList,
      WGPUPrimitiveTopology_LineList,
      WGPUPrimitiveTopology_TriangleList,
      WGPUPrimitiveTopology_TriangleStrip
   );
begin
   Result := cMap[ primitive ];
end;

// PrimitiveKindToWebGPUTopology
//
function PrimitiveKindToWebGPUTopology(primitive : TPrimitivesKind) : TWGPUPrimitiveTopology;
begin
   Result := PrimitiveKindUToWebGPUTopology(TPrimitivesKindU(primitive));
end;

// StencilOpToWebGPUOp
//
function StencilOpToWebGPUOp(stencilOp : TStencilOp) : TWGPUStencilOperation;
const
   cMap : array [ TStencilOp ] of TWGPUStencilOperation = (
      WGPUStencilOperation_Keep,
      WGPUStencilOperation_Zero,
      WGPUStencilOperation_Replace,
      WGPUStencilOperation_IncrementClamp,
      WGPUStencilOperation_DecrementClamp,
      WGPUStencilOperation_Invert
   );
begin
   Result := cMap[ stencilOp ];
end;

// StencilFuncToWebGPUComp
//
function StencilFuncToWebGPUComp(stencilFunc : TStencilFunc) : TWGPUCompareFunction;
const
   cMap : array [ TStencilFunc ] of TWGPUCompareFunction = (
      WGPUCompareFunction_Never,
      WGPUCompareFunction_Less,
      WGPUCompareFunction_LessEqual,
      WGPUCompareFunction_Greater,
      WGPUCompareFunction_GreaterEqual,
      WGPUCompareFunction_Equal,
      WGPUCompareFunction_NotEqual,
      WGPUCompareFunction_Always
   );
begin
   Result := cMap[ stencilFunc ];
end;

// PixelFormatToWebGPUFormat
//
function PixelFormatToWebGPUFormat(pf : TPixelFormat) : TWGPUTextureFormat;
begin
   case pf of
      TPixelFormat.RGBA16 :   Result := WGPUTextureFormat_RGBA16Unorm;
      TPixelFormat.RGB10_A2 : Result := WGPUTextureFormat_RGB10A2Unorm;
      TPixelFormat.BGRA :     Result := WGPUTextureFormat_BGRA8Unorm;
      //TPixelFormat.BGR :      Result := WGPUTextureFormat_BGRA8Unorm;  // No direct match for BGR, using BGRA8Unorm.
      TPixelFormat.RGBA :     Result := WGPUTextureFormat_RGBA8Unorm;
      //TPixelFormat.BGR_565 :  Result := WGPUTextureFormat_Undefined;   // No direct match for BGR_565 in WebGPU.
      //TPixelFormat.BGR5_A1 :  Result := WGPUTextureFormat_Undefined;   // No direct match for BGR5_A1 in WebGPU.
      //TPixelFormat.LA :       Result := WGPUTextureFormat_RG8Unorm;    // Mapping to RG8Unorm as a close match.
      TPixelFormat.R16F :     Result := WGPUTextureFormat_R16Float;
      TPixelFormat.RG16F :    Result := WGPUTextureFormat_RG16Float;
      TPixelFormat.RGBA16F :  Result := WGPUTextureFormat_RGBA16Float;
      TPixelFormat.R32F :     Result := WGPUTextureFormat_R32Float;
      TPixelFormat.RG32F :    Result := WGPUTextureFormat_RG32Float;
      TPixelFormat.RGBA32F :  Result := WGPUTextureFormat_RGBA32Float;
      TPixelFormat.A :        Result := WGPUTextureFormat_R8Unorm;     // Mapping alpha channel to R8Unorm.
   else
      Assert(False, 'Unsupported');
      Result := WGPUTextureFormat_Undefined;
   end;
end;

// VertexDeclarationToWGSLVertexFields
//
function VertexDeclarationToWGSLVertexFields(const declaration : TVertexDeclaration) : String;
const
   cVertexFormatToDeclarationWGSL : array [TVertexFormat] of String = (
      'vertex: vec3f', 'normal: vec3f',
      'color0: vec4i', 'color1: vec4i', 'color2: vec4i', 'color3: vec4i',
      'colorF0: vec4f', 'colorF1: vec4f', 'colorF2: vec4f', 'colorF3: vec4f',
      'texCoord0: vec2f', 'texCoord1: vec2f', 'texCoord2: vec2f', 'texCoord3: vec2f',
      'biNormal: vec3f', 'tangent: vec3f'
   );
begin
   for var i := 0 to High(declaration) do begin
      Result := Result + '   @location(' + IntToStr(i) + ') '
                       + cVertexFormatToDeclarationWGSL[declaration[i].Format]
                       + ','#10;
   end;
end;

// ------------------
// ------------------ TWebGPUDevice ------------------
// ------------------

// AdapterCallback
//
procedure AdapterCallback(status: TWGPURequestAdapterStatus; adapter: TWGPUAdapter; const &message: TWGPUStringView; userdata1: Pointer; userdata2: Pointer); cdecl;
begin
   if (status = WGPURequestAdapterStatus_Success) and (adapter <> 0) then
      TWebGPUDevice(userdata1).FAdapter := WGPUFactory.WrapAdapter(adapter);
end;

// DeviceCallback
//
procedure DeviceCallback(status: TWGPURequestDeviceStatus; device: TWGPUDevice; const &message: TWGPUStringView; userdata1: Pointer; userdata2: Pointer); cdecl;
begin
   if (status = WGPURequestDeviceStatus_Success) and (device <> 0) then
      TWebGPUDevice(userdata1).FDevice := WGPUFactory.WrapDevice(device);
end;

// UncapturedErrorCallback
//
procedure UncapturedErrorCallback(
   const device: PWGPUDevice;
   &type: TWGPUErrorType; const &message: TWGPUStringView;
   userdata1: Pointer; userdata2: Pointer
   ); cdecl;
var
   msg : String;
begin
   var log := TWebGPUDevice(userdata1).FLog;
   if log = nil then Exit;

   case &type of
      WGPUErrorType_NoError : msg := 'NoError: ';
      WGPUErrorType_Validation : msg := 'Validation: ';
      WGPUErrorType_OutOfMemory : msg := 'OutOfMemory: ';
      WGPUErrorType_Internal : msg := 'Internal: ';
      WGPUErrorType_Unknown : msg := 'Unknown: ';
   else
      msg := 'Unknown: ';
   end;

   msg := msg + UTF8ToString(&message);
   Assert(False, msg);
   log.Add(msg);
   if log.Count > cDeviceLogMaxSize then begin
      log.BeginUpdate;
      while log.Count > cDeviceLogTrimSize  do
         log.Delete(0);
      log.EndUpdate;
   end;
end;

// CompilationCallback
// assumes userdata1 is a TStrings
procedure CompilationCallback(
   status: TWGPUCompilationInfoRequestStatus;
   const compilationInfo: PWGPUCompilationInfo;
   userdata1: Pointer; userdata2: Pointer
   ); cdecl;
begin
   if compilationInfo.messageCount = 0 then Exit;

   var p := compilationInfo.messages;
   for var i := 1 to compilationInfo.messageCount do begin
      TStrings(userdata1).Add(Format(
         'Line %d:%d %s',
         [ p.lineNum, p.linePos, p.message.data ]
      ));
      Inc(p);
   end;
end;

// Create
//
constructor TWebGPUDevice.Create;
begin
   FLog := TStringList.Create;

   if vInstance = nil then begin
      var instanceDescriptor := Default(TWGPUInstanceDescriptor);
      vInstance := WGPUFactory.CreateInstance(instanceDescriptor);
   end;

   var adapterOptions := Default(TWGPURequestAdapterOptions);
   var adapterCallbackInfo := Default(TWGPURequestAdapterCallbackInfo);
   adapterCallbackInfo.mode := WGPUCallbackMode_AllowSpontaneous;
   adapterCallbackInfo.callback := AdapterCallback;
   adapterCallbackInfo.userdata1 := Self;
   vInstance.RequestAdapter(adapterOptions, adapterCallbackInfo);
   Assert(FAdapter <> nil);

   // Limits based on https://developer.mozilla.org/en-US/docs/Web/API/GPUSupportedLimits
   FMaxTextureSize := cLimits.maxTextureDimension2D;
   var requiredLimits := Default(TWGPULimits);
   requiredLimits := cLimits;

   var deviceDescriptor := Default(TWGPUDeviceDescriptor);
   deviceDescriptor.&label := 'WebGPU Device';
   deviceDescriptor.requiredLimits := @requiredLimits;
   deviceDescriptor.uncapturedErrorCallbackInfo.callback := @UncapturedErrorCallback;
   deviceDescriptor.uncapturedErrorCallbackInfo.userdata1 := Self;
   var featuresArray : array of TWGPUFeatureName := [
      // WGPUFeatureName_TimestampQuery
   ];
   deviceDescriptor.requiredFeatureCount := Length(featuresArray);
   deviceDescriptor.requiredFeatures := Pointer(featuresArray);
   var deviceCallbackInfo := Default(TWGPURequestDeviceCallbackInfo);
   deviceCallbackInfo.mode := WGPUCallbackMode_AllowSpontaneous;
   deviceCallbackInfo.callback := DeviceCallback;
   deviceCallbackInfo.userdata1 := Self;
   FAdapter.RequestDevice(deviceDescriptor, deviceCallbackInfo);
   Assert(FDevice <> nil);

   FQueue := FDevice.GetQueue;
end;

// Destroy
//
destructor TWebGPUDevice.Destroy;
begin
   inherited;
   FreeAndNil(FLog);
end;

// CreateTexture2D_BGRA32
//
function TWebGPUDevice.CreateTexture2D_BGRA32(
   width, height : Cardinal; usage : TWGPUTextureUsage;
   const name : UTF8String = ''; sampleCount : Integer = 1
   ) : IWGPUTexture;
begin
   var textureDesc := Default(TWGPUTextureDescriptor);
   textureDesc.&label := name;
   textureDesc.usage := usage;
   textureDesc.dimension := WGPUTextureDimension_2D;
   textureDesc.size.width := width;
   textureDesc.size.height := height;
   textureDesc.size.depthOrArrayLayers := 1;
   textureDesc.format := WGPUTextureFormat_BGRA8Unorm;
   textureDesc.mipLevelCount := 1;
   textureDesc.sampleCount := sampleCount;
   Result := FDevice.CreateTexture(textureDesc);
end;

// CreateTexture2D_BGRA32
//
function TWebGPUDevice.CreateTexture2D_BGRA32(width, height : Cardinal; const name : UTF8String = '') : IWGPUTexture;
begin
   Result := CreateTexture2D_BGRA32(width, height, WGPUTextureUsage_TextureBinding or WGPUTextureUsage_CopyDst, name);
end;

// WriteToTexture2D_BGRA32
//
procedure TWebGPUDevice.WriteToTexture2D_BGRA32(const texture : IWGPUTexture; width, height, mipLevel : Cardinal; data : Pointer);
begin
   Assert(texture.GetWidth = width);
   Assert(texture.GetHeight = height);

   var mipLevelCount := texture.GetMipLevelCount;
   Assert(mipLevel < mipLevelCount);

   var destination := Default(TWGPUTexelCopyTextureInfo);
   destination.texture := texture.GetHandle;
   destination.mipLevel := mipLevel;
   destination.aspect := WGPUTextureAspect_All;

   var source := Default(TWGPUTexelCopyBufferLayout);
   source.bytesPerRow := 4 * width;
   source.rowsPerImage := height;

   var size := Default(TWGPUExtent3D);
   size.width := width;
   size.height := height;
   size.depthOrArrayLayers := mipLevelCount;
   FQueue.WriteTexture(@destination, data, 4 * width * height, @source, @size);
end;

// CreateDepthStencilTexture
//
function TWebGPUDevice.CreateDepthStencilTexture(width, height : Cardinal; sampleCount : Integer) : IWGPUTexture;
begin
   var textureDesc := Default(TWGPUTextureDescriptor);
   textureDesc.&label := 'Depth';
   textureDesc.usage := WGPUTextureUsage_RenderAttachment;
   textureDesc.dimension := WGPUTextureDimension_2D;
   textureDesc.size.width := width;
   textureDesc.size.height := height;
   textureDesc.size.depthOrArrayLayers := 1;
   textureDesc.format := WGPUTextureFormat_Depth24Plus;
   textureDesc.mipLevelCount := 1;
   textureDesc.sampleCount := sampleCount;
   textureDesc.viewFormatCount := 1;
   textureDesc.viewFormats := @textureDesc.format;
   Result := FDevice.CreateTexture(textureDesc);
end;

// CreateMultiSample
//
function TWebGPUDevice.CreateMultiSample(width, height : Cardinal; format : TWGPUTextureFormat; sampleCount : Integer) : IWGPUTexture;
begin
   var textureDesc := Default(TWGPUTextureDescriptor);
   textureDesc.&label := 'MultiSample';
   textureDesc.usage := WGPUTextureUsage_RenderAttachment;
   textureDesc.dimension := WGPUTextureDimension_2D;
   textureDesc.size.width := width;
   textureDesc.size.height := height;
   textureDesc.size.depthOrArrayLayers := 1;
   textureDesc.format := format;
   textureDesc.mipLevelCount := 1;
   textureDesc.sampleCount := sampleCount;
   textureDesc.viewFormatCount := 1;
   textureDesc.viewFormats := @textureDesc.format;
   Result := FDevice.CreateTexture(textureDesc);
end;

// CreateBaseTextureView
//
function TWebGPUDevice.CreateBaseTextureView(const texture : IWGPUTexture; aspect : TWGPUTextureAspect; const name : UTF8String = '') : IWGPUTextureView;
begin
   var viewDescriptor := Default(TWGPUTextureViewDescriptor);
   viewDescriptor.&label := name;
   viewDescriptor.format := texture.GetFormat;
   case texture.GetDimension of
      WGPUTextureDimension_2D : viewDescriptor.dimension := WGPUTextureViewDimension_2D;
   else
      Assert(False);
   end;
   viewDescriptor.baseMipLevel := 0;
   viewDescriptor.mipLevelCount := 1;
   viewDescriptor.baseArrayLayer := 0;
   viewDescriptor.arrayLayerCount := 1;
   viewDescriptor.aspect := aspect;
   Result := texture.CreateView(viewDescriptor);
end;

// CreateBuffer
//
function TWebGPUDevice.CreateBuffer(dataSize : Cardinal; usage : TWGPUBufferUsage; const name : UTF8String = '') : IWGPUBuffer;
begin
   var descriptor := Default(TWGPUBufferDescriptor);
   descriptor.&label := name;
   descriptor.usage := usage;
   descriptor.size := dataSize;
   Result := FDevice.CreateBuffer(descriptor);
end;

// CreateBufferFromData
//
function TWebGPUDevice.CreateBufferFromData(dataSize : Cardinal; usage : TWGPUBufferUsage; dataPointer : Pointer; const name : UTF8String = '') : IWGPUBuffer;
begin
   Result := CreateBuffer(dataSize, usage or WGPUBufferUsage_CopyDst , name);
   FQueue.WriteBuffer(Result, 0, dataPointer, dataSize);
end;

// TextureBindGroupLayout
//
function TWebGPUDevice.TextureBindGroupLayout(nbTextures : Integer) : IWGPUBindGroupLayout;

   procedure Prepare(nbTextures : Integer);
   begin
      var bindGroupLayoutEntries : array of TWGPUBindGroupLayoutEntry;
      SetLength(bindGroupLayoutEntries, nbTextures);

      for var i := 0 to nbTextures-1 do begin
         var entry : PWGPUBindGroupLayoutEntry := @bindGroupLayoutEntries[i];
         entry.binding := i;
         entry.visibility := WGPUShaderStage_Fragment;
         entry.texture.sampleType := WGPUTextureSampleType_Float;
         entry.texture.viewDimension := WGPUTextureViewDimension_2D;
      end;

      var bindGroupLayoutDescriptor := Default(TWGPUBindGroupLayoutDescriptor);
      bindGroupLayoutDescriptor.&label := 'Textures';
      bindGroupLayoutDescriptor.entryCount := nbTextures;
      bindGroupLayoutDescriptor.entries := Pointer(bindGroupLayoutEntries);

      SetLength(FTexturesBindGroupLayouts, nbTextures+1);
      FTexturesBindGroupLayouts[nbTextures] := Device.CreateBindGroupLayout(bindGroupLayoutDescriptor);
   end;

begin
   while nbTextures > High(FTexturesBindGroupLayouts) do
      Prepare(Length(FTexturesBindGroupLayouts));
   Result := FTexturesBindGroupLayouts[nbTextures];
end;

// CompileShaderModule (string)
//
function TWebGPUDevice.CompileShaderModule(const wgslSource : UTF8String; const name : UTF8String = '') : IWGPUShaderModule;
begin
   Result := CompileShaderModule(Pointer(wgslSource), Length(wgslSource), name);
end;

// CompileShaderModule (pointer, length)
//
function TWebGPUDevice.CompileShaderModule(const wgslSourcePtr : PUTF8Char; wgslSourceLen : Integer; const name : UTF8String = '') : IWGPUShaderModule;
begin
   var shaderSourceWGSL := Default(TWGPUShaderSourceWGSL);
   shaderSourceWGSL.chain.sType := WGPUSType_ShaderSourceWGSL;
   shaderSourceWGSL.code.data := wgslSourcePtr;
   shaderSourceWGSL.code.length := wgslSourceLen;

   var shaderModuleDescriptor := Default(TWGPUShaderModuleDescriptor);
   shaderModuleDescriptor.&label := name;
   shaderModuleDescriptor.nextInChain := @shaderSourceWGSL;

   Result := Device.CreateShaderModule(shaderModuleDescriptor);
   Assert(Result <> nil);

   var errors := TStringList.Create;
   try
      var compilationInfoCallbackInfo := Default(TWGPUCompilationInfoCallbackInfo);
      compilationInfoCallbackInfo.mode := WGPUCallbackMode_AllowProcessEvents;
      compilationInfoCallbackInfo.callback := CompilationCallback;
      compilationInfoCallbackInfo.userdata1 := errors;
      var future := Result.GetCompilationInfo(compilationInfoCallbackInfo);
      if errors.Count > 0 then
         raise EFMXU_WebGPUException.Create('Vertex Shader error:'#13#10 + errors.Text);
   finally
      FreeAndNil(errors);
   end;
end;

// CreateCommandEncoder
//
function TWebGPUDevice.CreateCommandEncoder(const name : UTF8String = '') : IWGPUCommandEncoder;
begin
   var commandEncoderDescriptor := Default(TWGPUCommandEncoderDescriptor);
   commandEncoderDescriptor.&label := name;
   Result := Device.CreateCommandEncoder(commandEncoderDescriptor);
end;

// ------------------
// ------------------ TWebGPUCompiledShader ------------------
// ------------------

// Create
//
constructor TWebGPUCompiledShader.Create(
   aKind : TContextShaderKind; const aShader : IContextShaderSource;
   const aDevice : TWebGPUDevice; baseShader : Boolean
   );
begin
   inherited Create;

   FID := AtomicIncrement(vIDCounter);
   FKind := aKind;
   FShaderSource := aShader;

   if (aKind <> TContextShaderKind.VertexShader) or not baseShader then
      FShaderModule := CompileShaderModule(aShader, aDevice);

   var contextShaderSource := aShader.GetSelf;

   var nbVariables := Length(contextShaderSource.Variables);
   var bindGroupLayoutEntries : array of TWGPUBindGroupLayoutEntry;
   SetLength(bindGroupLayoutEntries, nbVariables);

   SetLength(FOffsets, nbVariables);
   SetLength(FSizes, nbVariables);

   FPaddingSize := 256;
   FMaxTextureSlot := -1;
   FUniformsSize := 0;
   for var i := 0 to nbVariables-1 do begin
      var v := contextShaderSource.Variables[i];
      var entry : PWGPUBindGroupLayoutEntry := @bindGroupLayoutEntries[i];
      entry.binding := i;
      case aKind of
         TContextShaderKind.VertexShader:
            entry.visibility := WGPUShaderStage_Vertex;
         TContextShaderKind.PixelShader:
            entry.visibility := WGPUShaderStage_Fragment;
      else
         Assert(False);
      end;
      case v.Kind of
         TContextShaderVariableKind.Float : begin
            entry.buffer.&type := WGPUBufferBindingType_Uniform;
            entry.buffer.minBindingSize := SizeOf(Single);
         end;
         TContextShaderVariableKind.Float2 : begin
            entry.buffer.&type := WGPUBufferBindingType_Uniform;
            entry.buffer.minBindingSize := SizeOf(Single) * 2;
         end;
         TContextShaderVariableKind.Float3 : begin
            entry.buffer.&type := WGPUBufferBindingType_Uniform;
            entry.buffer.minBindingSize := SizeOf(Single) * 3;
         end;
         TContextShaderVariableKind.Vector : begin
            entry.buffer.&type := WGPUBufferBindingType_Uniform;
            entry.buffer.minBindingSize := SizeOf(Single) * 4;
         end;
         TContextShaderVariableKind.Matrix : begin
            entry.buffer.&type := WGPUBufferBindingType_Uniform;
            entry.buffer.minBindingSize := SizeOf(Single) * 4 * 4;
         end;
         TContextShaderVariableKind.Texture : begin
            entry.sampler.&type := WGPUSamplerBindingType_Filtering;
            FMaxTextureSlot := v.Index;
         end;
      else
         Assert(False);
      end;
      if v.Size > entry.buffer.minBindingSize then
         entry.buffer.minBindingSize := v.Size
      else Assert((v.Size = 0) or (entry.buffer.minBindingSize = v.Size));
      var bindingSize := Cardinal(v.Size);
      if bindingSize mod FPaddingSize > 0 then
         bindingSize := ((bindingSize div FPaddingSize) + 1) * FPaddingSize;
      FOffsets[i] := FUniformsSize;
      FSizes[i] := v.Size;
      Inc(FUniformsSize, bindingSize);
   end;

   var bindGroupLayoutDescriptor := Default(TWGPUBindGroupLayoutDescriptor);
   case aKind of
      TContextShaderKind.VertexShader:
         bindGroupLayoutDescriptor.&label := 'VertexUniforms';
      TContextShaderKind.PixelShader:
         bindGroupLayoutDescriptor.&label := 'PixelUniforms';
   end;
   bindGroupLayoutDescriptor.entryCount := nbVariables;
   bindGroupLayoutDescriptor.entries := Pointer(bindGroupLayoutEntries);
   FBindGroupLayout := aDevice.Device.CreateBindGroupLayout(bindGroupLayoutDescriptor);
end;

// CompileShaderModule
//
function TWebGPUCompiledShader.CompileShaderModule(
   const aShader : IContextShaderSource; const aDevice : TWebGPUDevice
   ) : IWGPUShaderModule;
begin
   var contextShaderSource := aShader.GetSelf;
   Assert(contextShaderSource.Source.Arch = TContextShaderArch_WGSL);

   case FKind of
      TContextShaderKind.VertexShader :
         Result := aDevice.CompileShaderModule(PUTF8Char(contextShaderSource.Code), Length(contextShaderSource.Code), 'VertexShader');
      TContextShaderKind.PixelShader :
         Result := aDevice.CompileShaderModule(PUTF8Char(contextShaderSource.Code), Length(contextShaderSource.Code), 'PixelShader');
   else
      Assert(False);
   end;
end;

// GetOffsetAndSize
//
procedure TWebGPUCompiledShader.GetOffsetAndSize(index : Integer; var offset, size : UInt64);
begin
   offset := FOffsets[index];
   size := FSizes[index];
end;

// GetVariableCount
//
function TWebGPUCompiledShader.GetVariableCount : Integer;
begin
   Result := FShaderSource.GetVariablesCount;
end;

// FindVariable
//
function TWebGPUCompiledShader.FindVariable(const name : String; var offset, size : UInt64) : Boolean;
begin
   var i := FShaderSource.IndexOfVariable(name);
   if i >= 0 then begin
      GetOffsetAndSize(i, offset, size);
      Result := True;
   end else Result := False;
end;

// FindTextureVariable
//
function TWebGPUCompiledShader.FindTextureVariable(const name : String; var slot : Integer) : Boolean;
begin
   Assert(FKind = TContextShaderKind.PixelShader);
   var i := FShaderSource.IndexOfVariable(name);
   if i >= 0 then begin
      Assert(FShaderSource.Size[i] = 0);
      slot := FShaderSource.Index[i];
      Result := True;
   end else Result := False;
end;

// GetSpecializedModule
//
function TWebGPUCompiledShader.GetSpecializedModule(const vertexDeclaration : TVertexDeclaration; const aDevice : TWebGPUDevice) : IWGPUShaderModule;
begin
   for var i := 0 to High(FSpecializations) do
      if SameVertexDeclaration(vertexDeclaration, FSpecializations[i].Declaration) then
         Exit(FSpecializations[i].Module);

   Result := CompileShaderModule(FShaderSource.SpecializeForVertexDeclaration(vertexDeclaration), aDevice);
   var n := Length(FSpecializations);
   SetLength(FSpecializations, n+1);
   FSpecializations[n].Declaration := Copy(vertexDeclaration);
   FSpecializations[n].Module := Result;
end;

// GetID
//
function TWebGPUCompiledShader.GetID : NativeUInt;
begin
   Result := FID;
end;

// GetKind
//
function TWebGPUCompiledShader.GetKind : TContextShaderKind;
begin
   Result := FKind;
end;

// GetShaderSource
//
function TWebGPUCompiledShader.GetShaderSource : IContextShaderSource;
begin
   Result := FShaderSource;
end;

// GetShaderModule
//
function TWebGPUCompiledShader.GetShaderModule : IWGPUShaderModule;
begin
   Result := FShaderModule;
end;

// GetBindGroupLayout
//
function TWebGPUCompiledShader.GetBindGroupLayout : IWGPUBindGroupLayout;
begin
   Result := FBindGroupLayout;
end;

// GetMaxTextureSlot
//
function TWebGPUCompiledShader.GetMaxTextureSlot : Integer;
begin
   Result := FMaxTextureSlot;
end;

// GetUniformsSize
//
function TWebGPUCompiledShader.GetUniformsSize : Cardinal;
begin
   Result := FUniformsSize;
end;

// ------------------
// ------------------ TWebGPUPipeline ------------------
// ------------------

// Initialize
//
procedure TWebGPUPipeline.Initialize;
begin
   BlendState.color.operation := WGPUBlendOperation_Add;
   BlendState.color.srcFactor := WGPUBlendFactor_SrcAlpha;
   BlendState.color.dstFactor := WGPUBlendFactor_OneMinusSrcAlpha;
   BlendState.alpha.operation := WGPUBlendOperation_Add;
   BlendState.alpha.srcFactor := WGPUBlendFactor_Zero;
   BlendState.alpha.dstFactor := WGPUBlendFactor_One;

   ColorTargetState.format := WGPUTextureFormat_BGRA8Unorm;
   ColorTargetState.writeMask := WGPUColorWriteMask_All;
   ColorTargetState.blend := @BlendState;

   DepthStencilState.format := WGPUTextureFormat_Depth24Plus;
   DepthStencilState.depthWriteEnabled := WGPUOptionalBool_True;
   DepthStencilState.depthCompare := WGPUCompareFunction_Less;
   DepthStencilState.stencilFront.compare := WGPUCompareFunction_Always;
   DepthStencilState.stencilFront.failOp := WGPUStencilOperation_Keep;
   DepthStencilState.stencilFront.depthFailOp := WGPUStencilOperation_Keep;
   DepthStencilState.stencilFront.passOp := WGPUStencilOperation_Keep;
   DepthStencilState.stencilBack := DepthStencilState.stencilFront;
   DepthStencilState.stencilReadMask := UInt32(-1);
   DepthStencilState.stencilWriteMask := UInt32(-1);

   CullMode := WGPUCullMode_None;
end;

// SetContextState
//
procedure TWebGPUPipeline.SetContextState(aState : TContextState);
begin
   case aState of
      TContextState.cs2DScene, TContextState.cs3DScene: ; // ignored

      TContextState.csZTestOn:
         DepthStencilState.depthCompare := WGPUCompareFunction_LessEqual;
      TContextState.csZTestOff:
         DepthStencilState.depthCompare := WGPUCompareFunction_Never;
      TContextState.csZWriteOn:
         DepthStencilState.depthWriteEnabled := WGPUOptionalBool_True;
      TContextState.csZWriteOff:
         DepthStencilState.depthWriteEnabled := WGPUOptionalBool_False;

      TContextState.csAlphaBlendOn: begin
         BlendState.color.operation := WGPUBlendOperation_Add;
         BlendState.color.srcFactor := WGPUBlendFactor_SrcAlpha;
         BlendState.color.dstFactor := WGPUBlendFactor_OneMinusSrcAlpha;
         BlendState.alpha.operation := WGPUBlendOperation_Add;
         BlendState.alpha.srcFactor := WGPUBlendFactor_One;
         BlendState.alpha.dstFactor := WGPUBlendFactor_OneMinusSrcAlpha;
      end;
      TContextState.csAlphaBlendOff: begin
         BlendState.color.operation := WGPUBlendOperation_Add;
         BlendState.color.srcFactor := WGPUBlendFactor_One;
         BlendState.color.dstFactor := WGPUBlendFactor_Zero;
         BlendState.alpha.operation := WGPUBlendOperation_Add;
         BlendState.alpha.srcFactor := WGPUBlendFactor_One;
         BlendState.alpha.dstFactor := WGPUBlendFactor_Zero;
      end;

      TContextState.csStencilOn:
         DepthStencilState.stencilWriteMask := UInt32(-1);
      TContextState.csStencilOff:
         DepthStencilState.stencilWriteMask := 0;

      TContextState.csColorWriteOn:
         ColorTargetState.writeMask := WGPUColorWriteMask_All;
      TContextState.csColorWriteOff:
         ColorTargetState.writeMask := WGPUColorWriteMask_None;

      TContextState.csScissorOn, TContextState.csScissorOff:
         Assert(False); // not handled here, shouldn't happen
      TContextState.csFrontFace:
         CullMode := WGPUCullMode_Front;
      TContextState.csBackFace:
         CullMode := WGPUCullMode_Back;
      TContextState.csAllFace:
         CullMode := WGPUCullMode_None;
   else
      Assert(False);
   end;
end;

// SetStencilOp
//
procedure TWebGPUPipeline.SetStencilOp(const aFail, aZFail, aZPass: TStencilOp);
begin
   DepthStencilState.stencilFront.failOp := StencilOpToWebGPUOp(aFail);
   DepthStencilState.stencilFront.depthFailOp := StencilOpToWebGPUOp(aZFail);
   DepthStencilState.stencilFront.passOp := StencilOpToWebGPUOp(aZPass);
   DepthStencilState.stencilBack.failOp := DepthStencilState.stencilFront.failOp;
   DepthStencilState.stencilBack.depthFailOp := DepthStencilState.stencilFront.depthFailOp;
   DepthStencilState.stencilBack.passOp := DepthStencilState.stencilFront.passOp;
end;

// SetStencilFunc
//
procedure TWebGPUPipeline.SetStencilFunc(const aFunc : TStencilfunc; aRef, aMask : Cardinal);
begin
   DepthStencilState.stencilFront.compare := StencilFuncToWebGPUComp(aFunc);
   StencilRef := aRef;
   // TODO: aMask is it read or write mask in FMX ?
end;

// SamePipeline
//
function TWebGPUPipeline.SamePipeline(const aPipeline : TWebGPUPipeline) : Boolean;
begin
   Result := CompareMem(@ColorTargetState, @aPipeline.ColorTargetState, SizeOf(ColorTargetState))
         and CompareMem(@BlendState, @aPipeline.BlendState, SizeOf(BlendState))
         and CompareMem(@DepthStencilState, @aPipeline.DepthStencilState, SizeOf(DepthStencilState))
         and (StencilRef = aPipeline.StencilRef)
         and (Topology = aPipeline.Topology)
         and (CullMode = aPipeline.CullMode)
         and (VertexCompiled.GetID = aPipeline.VertexCompiled.GetID)
         and (FFragmentCompiled.GetID = aPipeline.FFragmentCompiled.GetID)
         and SameVertexDeclaration(FVertexDeclaration, aPipeline.VertexDeclaration)
         and (FVertexBufferStrideInBytes = aPipeline.FVertexBufferStrideInBytes)
         and (TextureCount = aPipeline.TextureCount)
         and (FSampleCount = aPipeline.FSampleCount);
end;

// SetVertexDeclaration
//
procedure TWebGPUPipeline.SetVertexDeclaration(const aDeclaration : TVertexDeclaration; aVertexBufferStrideInBytes : Cardinal);
begin
   if     SameVertexDeclaration(FVertexDeclaration, aDeclaration)
      and (FVertexBufferStrideInBytes = aVertexBufferStrideInBytes) then Exit;

   FVertexDeclaration := Copy(aDeclaration);
   FVertexBufferStrideInBytes := aVertexBufferStrideInBytes;

   var n := Length(aDeclaration);
   SetLength(FVertexAttributes, n);

   for var i := 0 to n-1 do begin
      var attrib : PWGPUVertexAttribute := @FVertexAttributes[i];
      attrib.shaderLocation := i;
      attrib.offset := aDeclaration[i].Offset;
      case aDeclaration[i].Format of
         TVertexFormat.Vertex, TVertexFormat.Normal : begin
            attrib.format := WGPUVertexFormat_Float32x3;
         end;
         TVertexFormat.Color0 .. TVertexFormat.Color3 :
            attrib.format := WGPUVertexFormat_Unorm8x4;
         TVertexFormat.ColorF0 .. TVertexFormat.ColorF3 :
            attrib.format := WGPUVertexFormat_Float32x4;
         TVertexFormat.TexCoord0 .. TVertexFormat.TexCoord3 :
            attrib.format := WGPUVertexFormat_Float32x2;
         TVertexFormat.BiNormal, TVertexFormat.Tangent :
            attrib.format := WGPUVertexFormat_Float32x3;
      else
         Assert(False);
      end;
   end;

   FVertexBufferLayout := Default(TWGPUVertexBufferLayout);
   FVertexBufferLayout.arrayStride := aVertexBufferStrideInBytes;
   FVertexBufferLayout.stepMode := WGPUVertexStepMode_Vertex;
   FVertexBufferLayout.attributeCount := Length(FVertexAttributes);
   FVertexBufferLayout.attributes := Pointer(FVertexAttributes);

   FVertexState := Default(TWGPUVertexState);
   FVertexState.module := 0; // set in SetupRenderPipeline
   FVertexState.entryPoint := 'main';
   FVertexState.bufferCount := 1;
   FVertexState.buffers := @FVertexBufferLayout;

end;

// SetFragmentShader
//
procedure TWebGPUPipeline.SetFragmentShader(const aShader : IWebGPUCompiledShader);
begin
   if FFragmentCompiled = aShader then Exit;

   FFragmentCompiled := aShader;

   FFragmentState := Default(TWGPUFragmentState);
   FFragmentState.module := aShader.GetShaderModule.GetHandle;
   FFragmentState.entryPoint := 'main';
   FFragmentState.targetCount := 1;
   FFragmentState.targets := @ColorTargetState;

   TextureCount := aShader.GetMaxTextureSlot + 1;
end;

// Instantiate
//
constructor TWebGPUPipeline.Instantiate(const ref : TWebGPUPipeline; const aDevice : TWebGPUDevice);
begin
   Create;
   ColorTargetState := ref.ColorTargetState;
   BlendState := ref.BlendState;
   DepthStencilState := ref.DepthStencilState;
   StencilRef := ref.StencilRef;
   Topology := ref.Topology;
   CullMode := ref.CullMode;
   VertexCompiled := ref.VertexCompiled;
   SetFragmentShader(ref.FFragmentCompiled);
   SetVertexDeclaration(ref.FVertexDeclaration, ref.FVertexBufferStrideInBytes);
   TextureCount := ref.TextureCount;
   FSampleCount := ref.FSampleCount;

   var bindGroupLayouts : array [0..2] of TWGPUBindGroupLayout;

   bindGroupLayouts[0] := VertexCompiled.GetBindGroupLayout.GetHandle;
   bindGroupLayouts[1] := aDevice.TextureBindGroupLayout(TextureCount).GetHandle;
   bindGroupLayouts[2] := FFragmentCompiled.GetBindGroupLayout.GetHandle;

   var pipelineLayoutDescriptor := Default(TWGPUPipelineLayoutDescriptor);
   pipelineLayoutDescriptor.&label := 'PipelineLayout';
   pipelineLayoutDescriptor.bindGroupLayoutCount := Length(bindGroupLayouts);
   pipelineLayoutDescriptor.bindGroupLayouts := @bindGroupLayouts;

   PipelineLayout := aDevice.Device.CreatePipelineLayout(pipelineLayoutDescriptor);
   Assert(PipelineLayout <> nil);

   FVertexState.module := VertexCompiled.GetSpecializedModule(FVertexDeclaration, aDevice).GetHandle;

   var pipelineDescriptor := Default(TWGPURenderPipelineDescriptor);
   pipelineDescriptor.&label := 'RenderPipeline';
   pipelineDescriptor.layout := PipelineLayout.GetHandle;
   pipelineDescriptor.vertex := FVertexState;
   pipelineDescriptor.fragment := @FFragmentState;
   pipelineDescriptor.primitive.topology := Topology;
   pipelineDescriptor.primitive.stripIndexFormat := WGPUIndexFormat_Undefined;
   pipelineDescriptor.primitive.frontFace := WGPUFrontFace_CCW;
   pipelineDescriptor.primitive.cullMode := CullMode;
   pipelineDescriptor.depthStencil := @DepthStencilState;
   pipelineDescriptor.multisample.count := FSampleCount;
   pipelineDescriptor.multisample.mask := UInt32(-1);

   RenderPipeline := aDevice.Device.CreateRenderPipeline(pipelineDescriptor);
   Assert(RenderPipeline <> nil);
end;

// ------------------
// ------------------ TWebGPUPipelineManager ------------------
// ------------------

// Create
//
constructor TWebGPUPipelineManager.Create(const aDevice : TWebGPUDevice);
begin
   inherited Create;
   FDevice := aDevice;

   FCurrent := TWebGPUPipeline.Create;
   FCurrent.Initialize;

   var samplerDescriptor := Default(TWGPUSamplerDescriptor);
   samplerDescriptor.addressModeU := WGPUAddressMode_ClampToEdge;
   samplerDescriptor.addressModeV := WGPUAddressMode_ClampToEdge;
   samplerDescriptor.addressModeW := WGPUAddressMode_ClampToEdge;
   samplerDescriptor.magFilter := WGPUFilterMode_Linear;
   samplerDescriptor.minFilter := WGPUFilterMode_Linear;
   samplerDescriptor.mipmapFilter := WGPUMipmapFilterMode_Linear;
   samplerDescriptor.lodMinClamp := 0;
   samplerDescriptor.lodMaxClamp := 1;
   samplerDescriptor.maxAnisotropy := 1;
   FSampler := FDevice.Device.CreateSampler(samplerDescriptor);
end;

// Destroy
//
destructor TWebGPUPipelineManager.Destroy;
begin
   inherited;
   FreeAndNil(FCurrent);
   for var i := 0 to High(FPipelines) do
      FreeAndNil(FPipelines[i]);
end;

// SetSampleCount
//
procedure TWebGPUPipelineManager.SetSampleCount(aCount : Integer);
begin
   FCurrent.FSampleCount := aCount;
end;

// SetContextState
//
procedure TWebGPUPipelineManager.SetContextState(aState : TContextState);
begin
   FCurrent.SetContextState(aState);
end;

// SetStencilRef
//
procedure TWebGPUPipelineManager.SetStencilRef(ref : Integer);
begin
   FCurrent.StencilRef := ref;
end;

// SetStencilOp
//
procedure TWebGPUPipelineManager.SetStencilOp(const aFail, aZFail, aZPass: TStencilOp);
begin
   FCurrent.SetStencilOp(aFail, aZFail, aZPass);
end;

// SetStencilFunc
//
procedure TWebGPUPipelineManager.SetStencilFunc(const aFunc : TStencilfunc; aRef, aMask : Cardinal);
begin
   FCurrent.SetStencilFunc(aFunc, aRef, aMask);
end;

// Setup
//
procedure TWebGPUPipelineManager.Setup(const vertexShader, fragmentShader : IWebGPUCompiledShader);
begin
   FCurrent.VertexCompiled := vertexShader;
   FVertexBufferUniforms := nil;
   FVertexBufferUniforms := FDevice.CreateBuffer(
      vertexShader.GetUniformsSize,
      WGPUBufferUsage_Uniform or WGPUBufferUsage_CopyDst,
      'VertexUniforms'
   );

   FCurrent.SetFragmentShader(fragmentShader);
   FFragmentBufferUniforms := nil;
   if fragmentShader.GetUniformsSize > 0 then begin
      FFragmentBufferUniforms := FDevice.CreateBuffer(
         fragmentShader.GetUniformsSize,
         WGPUBufferUsage_Uniform or WGPUBufferUsage_CopyDst,
         'FragmentUniforms'
      );
   end;
end;

// SetShaderVariable
//
procedure TWebGPUPipelineManager.SetShaderVariable(const aName : String; const data : array of TVector3D);
var
   offset, size : UInt64;
begin
   offset := MaxInt;
   if FCurrent.VertexCompiled.FindVariable(aName, offset, size) then begin
      Assert(size <= SizeOf(data));
      FDevice.Queue.WriteBuffer(FVertexBufferUniforms, offset, @data[0], size);
   end;
   if FCurrent.FFragmentCompiled.FindVariable(aName, offset, size) then begin
      Assert(size <= SizeOf(data));
      FDevice.Queue.WriteBuffer(FFragmentBufferUniforms, offset, @data[0], size);
   end;
   if offset = MaxInt then
      raise EFMXU_WebGPUException.CreateFmt('Shader variable "%s" not found', [ aName ]);
end;

// SetVertexDeclaration
//
procedure TWebGPUPipelineManager.SetVertexDeclaration(
   const declaration : TVertexDeclaration;
   vertexBufferStrideInBytes : Cardinal
   );
begin
   FCurrent.SetVertexDeclaration(declaration, vertexBufferStrideInBytes);
end;

// SetTexture
//
procedure TWebGPUPipelineManager.SetTexture(slot : Integer; const aTexture : IWGPUTexture);
begin
   if FTextures[slot] <> aTexture then begin

      FTextures[slot] := aTexture;

      var textureViewDescriptor := Default(TWGPUTextureViewDescriptor);
      textureViewDescriptor.format := aTexture.GetFormat;
      textureViewDescriptor.dimension := WGPUTextureViewDimension_2D;
      textureViewDescriptor.baseMipLevel := 0;
      textureViewDescriptor.mipLevelCount := 1;
      textureViewDescriptor.baseArrayLayer := 0;
      textureViewDescriptor.arrayLayerCount := 1;
      textureViewDescriptor.aspect := WGPUTextureAspect_All;
      FTextureViews[slot] := aTexture.CreateView(textureViewDescriptor);
      Assert(FTextureViews[slot] <> nil);

   end;
end;

// SetTextureVariable
//
procedure TWebGPUPipelineManager.SetTextureVariable(const aName : String; const aTexture : IWGPUTexture);
var
   slot : Integer;
begin
   Assert(FCurrent.FFragmentCompiled <> nil);
   if FCurrent.FFragmentCompiled.FindTextureVariable(aName, slot) then
      SetTexture(slot, aTexture)
   else raise EFMXU_WebGPUException.CreateFmt('Texture Variable "%s" not found', [ aName ]);
end;

// SetupRenderPipeline
//
procedure TWebGPUPipelineManager.SetupRenderPipeline(topology : TPrimitivesKindU; indexFormat : TWGPUIndexFormat);
begin
   FCurrent.Topology := PrimitiveKindUToWebGPUTopology(topology);

   for var i := 0 to High(FPipelines) do begin
      if FCurrent.SamePipeline(FPipelines[i]) then begin
         FRenderPipeline := FPipelines[i].RenderPipeline;
         Exit;
      end;
   end;

   var newPipeline := TWebGPUPipeline.Instantiate(FCurrent, FDevice);
   Insert(newPipeline, FPipelines, 0);
   FRenderPipeline := newPipeline.RenderPipeline;
end;

// SetBindGroups
//
procedure TWebGPUPipelineManager.SetBindGroups(const renderPass : IWGPURenderPassEncoder);
begin
   // vertex bind group 0, only supports uniforms / variables
   var bindGroupEntries : TArray<TWGPUBindGroupEntry>;
   var nbVariables := FCurrent.VertexCompiled.GetVariableCount;
   SetLength(bindGroupEntries, nbVariables);
   for var i := 0 to nbVariables-1 do begin
      bindGroupEntries[i].binding := i;
      bindGroupEntries[i].buffer := FVertexBufferUniforms.GetHandle;
      FCurrent.VertexCompiled.GetOffsetAndSize(
         i,
         bindGroupEntries[i].offset,
         bindGroupEntries[i].size
      );
   end;

   var bindGroupDescriptor := Default(TWGPUBindGroupDescriptor);
   bindGroupDescriptor.&label := 'VertexBind';
   bindGroupDescriptor.layout := FCurrent.VertexCompiled.GetBindGroupLayout.GetHandle;
   bindGroupDescriptor.entryCount := Length(bindGroupEntries);
   bindGroupDescriptor.entries := Pointer(bindGroupEntries);
   FBindGroups[0] := FDevice.Device.CreateBindGroup(bindGroupDescriptor);

   renderPass.SetBindGroup(0, FBindGroups[0], 0, nil);

   // fragment bind group 1, fragment textures

   bindGroupEntries := nil;
   var textureCount := FCurrent.TextureCount;
   if textureCount > 0 then begin
      SetLength(bindGroupEntries, textureCount);
      for var i := 0 to textureCount-1 do begin
         bindGroupEntries[i].binding := i;
         if FTextureViews[i] <> nil then
            bindGroupEntries[i].textureView := FTextureViews[i].GetHandle;
      end;
   end;

   bindGroupDescriptor.&label := 'TextureBind';
   bindGroupDescriptor.layout := FDevice.TextureBindGroupLayout(textureCount).GetHandle;
   bindGroupDescriptor.entryCount := Length(bindGroupEntries);
   bindGroupDescriptor.entries := Pointer(bindGroupEntries);
   FBindGroups[1] := FDevice.Device.CreateBindGroup(bindGroupDescriptor);

   renderPass.SetBindGroup(1, FBindGroups[1], 0, nil);

   // fragment bind group 2, fragment uniforms

   bindGroupEntries := nil;
   if FFragmentBufferUniforms <> nil then begin
      var source := FCurrent.FFragmentCompiled.GetShaderSource.GetSelf;
      SetLength(bindGroupEntries, Length(source.Variables));
      for var i := 0 to High(bindGroupEntries) do begin
         bindGroupEntries[i].binding := i;
         if source.Variables[i].Kind = TContextShaderVariableKind.Texture then
            bindGroupEntries[i].sampler := FSampler.GetHandle
         else bindGroupEntries[i].buffer := FFragmentBufferUniforms.GetHandle;
         FCurrent.FFragmentCompiled.GetOffsetAndSize(i, bindGroupEntries[i].offset, bindGroupEntries[i].size);
      end;
   end;

   bindGroupDescriptor.&label := 'FragmentBind';
   bindGroupDescriptor.layout := FCurrent.FFragmentCompiled.GetBindGroupLayout.GetHandle;
   bindGroupDescriptor.entryCount := Length(bindGroupEntries);
   bindGroupDescriptor.entries := Pointer(bindGroupEntries);
   FBindGroups[2] := FDevice.Device.CreateBindGroup(bindGroupDescriptor);

   renderPass.SetBindGroup(2, FBindGroups[2], 0, nil);
end;

// ------------------
// ------------------ TIWebGPUMappedBuffer ------------------
// ------------------

procedure BufferMappedCallback(status: TWGPUMapAsyncStatus; const &message: TWGPUStringView; userdata1, userdata2: Pointer); cdecl;
begin
   var mapped := TIWebGPUMappedBuffer(userdata1);
   mapped.FStatus := status;
   try
      if status = WGPUMapAsyncStatus_Success then
         mapped.FData := mapped.FBuffer.GetConstMappedRange(mapped.FOffset, mapped.FSize);
   finally
      mapped._Release;
   end;
end;

// Create
//
class function TIWebGPUMappedBuffer.Create(
   const aBuffer : IWGPUBuffer; const aMode: TWGPUMapMode;
   aOffset: NativeUInt; aSize: NativeUInt
   ) : IWebGPUMappedBuffer;
begin
   var instance := inherited Create;
   Result := instance;
   instance.FBuffer := aBuffer;
   instance.FOffset := aOffset;
   instance.FSize := aSize;

   var callbackInfo := Default(TWGPUBufferMapCallbackInfo);
   callbackInfo.mode := WGPUCallbackMode_AllowSpontaneous;
   callbackInfo.callback := BufferMappedCallback;
   callbackInfo.userdata1 := instance;

   instance._AddRef;

   aBuffer.MapAsync(aMode, aOffset, aSize, callbackInfo);
end;

// Destroy
//
destructor TIWebGPUMappedBuffer.Destroy;
begin
   inherited;
   FBuffer.Unmap;
end;

// Status
//
function TIWebGPUMappedBuffer.Status : TWGPUMapAsyncStatus;
begin
   Result := FStatus;
end;

// Wait
//
procedure TIWebGPUMappedBuffer.Wait;
begin
   if Ord(FStatus) <> 0 then Exit;

   TWebGPUDevice.Instance.ProcessEvents;

   while Ord(FStatus) = 0 do begin
      Sleep(0);
      TWebGPUDevice.Instance.ProcessEvents;
   end;
end;

// Data
//
function TIWebGPUMappedBuffer.Data : Pointer;
begin
   if Ord(FStatus) = 0 then
      Wait;
   Assert(FStatus = WGPUMapAsyncStatus_Success);
   Result := FData;
end;

// Size
//
function TIWebGPUMappedBuffer.Size : UInt64;
begin
   Result := FSize;
end;

end.
