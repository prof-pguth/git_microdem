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
unit FMXU.DX11.Utils;

{$i fmxu.inc}

interface

uses
   Winapi.Windows, Winapi.D3D11, Winapi.D3DCommon,
   Winapi.DXGI, Winapi.DxgiFormat, WinApi.DxgiType,
   System.Math, System.SysUtils, System.UITypes, System.Classes,
   FMX.Types3D, FMX.Types,
   FMXU.Context;

type
   TD3D11_INPUT_ELEMENT_DESC_Array = array of TD3D11_INPUT_ELEMENT_DESC;

   // holds Device & Context, typically a singleton
   TDX11Device = class
      protected
         FDevice : ID3D11Device;
         FDeviceContext : ID3D11DeviceContext;
         FDXGIFactory : IDXGIFactory1;
         FDriverType : D3D_DRIVER_TYPE;
         FFeatureLevel : D3D_FEATURE_LEVEL;

         FSamplerDescArray : array [0..D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT-1] of TD3D11_SAMPLER_DESC;
         FSamplerStateArray : array [0..D3D11_COMMONSHADER_SAMPLER_SLOT_COUNT-1] of ID3D11SamplerState;

         FVertexShaderSource : IContextShaderSource;

      public
         constructor Create(debug : Boolean; whichGPU : Integer = -1);
         destructor Destroy; override;

         property Device : ID3D11Device read FDevice;
         property DeviceContext : ID3D11DeviceContext read FDeviceContext;
         property DXGIFactory : IDXGIFactory1 read FDXGIFactory;
         property DriverType : D3D_DRIVER_TYPE read FDriverType;
         property FeatureLevel : D3D_FEATURE_LEVEL read FFeatureLevel;

         // these methods automatically raise on HResult error

         function CreateBuffer(const desc : TD3D11_BUFFER_DESC; const initialData : PD3D11_SUBRESOURCE_DATA) : ID3D11Buffer;
         function CreateTexture2D(const desc : TD3D11_TEXTURE2D_DESC; const initialData : PD3D11_SUBRESOURCE_DATA) : ID3D11Texture2D;
         function CreateRenderTargetView(const resource : ID3D11Resource; const desc : PD3D11_RENDER_TARGET_VIEW_DESC) : ID3D11RenderTargetView;

         function CreateDepthStencilView(width, height : Integer; const sampleDesc : DXGI_SAMPLE_DESC) : ID3D11DepthStencilView;

         procedure SetViewport(const viewport : TD3D11_Viewport);
         procedure ClearViewport;

         procedure SetRenderTarget(const targetView : ID3D11RenderTargetView; const depthStenciView : ID3D11DepthStencilView);
         procedure ClearRenderTarget;

         procedure ClearRenderTargetView(const targetView : ID3D11RenderTargetView; const aColor : TAlphaColor);
         procedure ClearDepthStencilView(const depthStencilView : ID3D11DepthStencilView; flags : TD3D11_CLEAR_FLAG; aDepth : Single; aStencil : Cardinal);

         function Map(const res : ID3D11Resource; mapType: D3D11_MAP) : TD3D11_MAPPED_SUBRESOURCE;
         procedure Unmap(const res : ID3D11Resource);

         function CreateVertexShader(const code : TContextShaderCode) : ID3D11VertexShader;
         function CreatePixelShader(const code : TContextShaderCode) : ID3D11PixelShader;

         procedure IASetVertexBuffers(startSlot, numBuffers: UINT; const buffers : ID3D11Buffer; const strides, offsets : PUINT);
         procedure IASetIndexBuffer(const buffer : ID3D11Buffer; format : DXGI_FORMAT; offset : UINT);

         procedure SetSamplerDesc(slot : UINT; const samplerDesc : TD3D11_SAMPLER_DESC);
         function GetSampleDesc(slot : UINT) : PD3D11_SAMPLER_DESC;

         procedure SetInputLayout(const aVertexDeclaration : TVertexDeclaration; const aVertexShaderSource : IContextShaderSource);

         procedure DrawIndexed(topology : TPrimitivesKindU; indexCount : Integer);
   end;

   // holds blending, rasterizer & stencil states
   TDX11DeviceStates = class
      private
         FBlendDesc : TD3D11_BLEND_DESC;
         FBlendState : ID3D11BlendState;
         FBlendStateModified : Boolean;

         FRasterizerDesc : TD3D11_RASTERIZER_DESC;
         FRasterizerState : ID3D11RasterizerState;
         FRasterizerStateModified : Boolean;

         FDepthStencilDesc : TD3D11_DEPTH_STENCIL_DESC;
         FDepthStencilState : ID3D11DepthStencilState;
         FDepthStencilModified : Boolean;
         FStencilRef : Cardinal;

      protected
         function GetZTest : Boolean; inline;
         procedure SetZTest(val : Boolean);
         function GetZWrite : Boolean; inline;
         procedure SetZWriteTest(val : Boolean);
         function GetAlphaBlend : Boolean; inline;
         procedure SetAlphaBlend(const val : Boolean);
         function GetStencil : Boolean; inline;
         procedure SetStencil(const val : Boolean);
         function GetColorWrite : Boolean; inline;
         procedure SetColorWrite(const val : Boolean);
         function GetScissor : Boolean; inline;
         procedure SetScissor(const val : Boolean);
         function GetCullMode : TD3D11_CULL_MODE; inline;
         procedure SetCullMode(const val : TD3D11_CULL_MODE);

      public
         constructor Create;

         property ZTest : Boolean read GetZTest write SetZTest;
         property ZWrite : Boolean read GetZWrite write SetZTest;
         property AlphaBlend : Boolean read GetAlphaBlend write SetAlphaBlend;
         property Stencil : Boolean read GetStencil write SetStencil;
         property ColorWrite : Boolean read GetColorWrite write SetColorWrite;
         property Scissor : Boolean read GetScissor write SetScissor;
         property CullMode : TD3D11_CULL_MODE read GetCullMode write SetCullMode;

         procedure SetStencilOp(aFail, aZFail, aZPass : TD3D11_STENCIL_OP);
         procedure SetStencilFunc(aFunc : TD3D11_COMPARISON_FUNC; aRef, aMask : Cardinal);

         procedure ApplyChanges(device : TDX11Device);
   end;

   TDX11DeviceInputHelper = class;
   IDX11InputLayoutHelper = interface
      ['{4CB7A2B5-DC4D-41F9-A218-A10D258379CF}']
      function GetSelf : TDX11DeviceInputHelper;
      function HasSameVertexDeclaration(const aVertexDeclaration : TVertexDeclaration) : Boolean;
   end;
   TDX11DeviceInputHelper = class (TInterfacedObject, IDX11InputLayoutHelper)
      protected
         FVertexDeclaration : TVertexDeclaration;
         FInputLayout : ID3D11InputLayout;

         function GetSelf : TDX11DeviceInputHelper;
         function HasSameVertexDeclaration(const aVertexDeclaration : TVertexDeclaration) : Boolean;

      public
         property VertexDeclaration : TVertexDeclaration read FVertexDeclaration write FVertexDeclaration;
         property InputLayout : ID3D11InputLayout read FInputLayout write FInputLayout;
   end;

   EFMXU_DX11Exception = class (Exception)
   end;

const
   cD3DWhite : TFourSingleArray = ( 1.0, 1.0, 1.0, 1.0 );

function VertexElementsToDX11Declaration(const elements : TVertexDeclaration) : TD3D11_INPUT_ELEMENT_DESC_array;

function PrimitiveKindUToDX11Topology(primitive : TPrimitivesKindU) : D3D11_PRIMITIVE_TOPOLOGY;
function PrimitiveKindToDX11Topology(primitive : TPrimitivesKind) : D3D11_PRIMITIVE_TOPOLOGY; inline;

function StencilOpToDX11Op(stencilOp : TStencilOp) : D3D11_STENCIL_OP;
function StencilFuncToDX11Comp(stencilFunc : TStencilFunc) : D3D11_COMPARISON_FUNC;

function PixelFormatToDXGIFormat(pf : TPixelFormat) : DXGI_FORMAT;

procedure EnumerateDX11Adapters(adapterList : TStrings);

procedure RaiseIfFailed(hr : HResult; const msg : String);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RaiseIfFailed
//
procedure RaiseIfFailed(hr : HResult; const msg : String);
begin
   if Failed(hr) then
      raise EFMXU_DX11Exception.CreateFmt('Failed (0x%x), %s', [ hr, msg ]);
end;

// EnumerateDX11Adapters
//
procedure EnumerateDX11Adapters(adapterList : TStrings);
var
   factory : IDXGIFactory1;
   adapter : IDXGIAdapter1;
   adapterDesc : DXGI_ADAPTER_DESC;
begin
   if Succeeded(CreateDXGIFactory1(IDXGIFactory1, factory)) then begin
      var i := 0;
      while factory.EnumAdapters1(i, adapter) <> DXGI_ERROR_NOT_FOUND do begin
         if Succeeded(adapter.GetDesc(adapterDesc)) then
            adapterList.Add(String(adapterDesc.Description));
         adapter := nil;
         Inc(i);
      end;
   end;
end;

// VertexElementsToDX11Declaration
//
function VertexElementsToDX11Declaration(const elements : TVertexDeclaration) : TD3D11_INPUT_ELEMENT_DESC_array;
begin
   var n := Length(elements);
   SetLength(Result, n);
   for var i := 0 to n-1 do begin
      var desc : PD3D11_INPUT_ELEMENT_DESC := @Result[i];
      desc^ := Default(TD3D11_INPUT_ELEMENT_DESC);
      desc.AlignedByteOffset := elements[i].Offset;
      desc.InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
      var elementFormat := elements[i].Format;
      case elementFormat of
         TVertexFormat.Vertex : begin
            desc.SemanticName := 'POSITION';
            desc.Format := DXGI_FORMAT_R32G32B32_FLOAT;
         end;
         TVertexFormat.Normal : begin
            desc.SemanticName := 'NORMAL';
            desc.Format := DXGI_FORMAT_R32G32B32_FLOAT;
         end;
         TVertexFormat.Color0 .. TVertexFormat.Color3 : begin
            desc.SemanticName := 'COLOR';
            desc.SemanticIndex := Ord(elementFormat) - Ord(TVertexFormat.Color0);
            // for compatibility with FMX which supports feature level 9_1 which doesn't support BGRA
            desc.Format := DXGI_FORMAT_R8G8B8A8_UNORM;
         end;
         TVertexFormat.TexCoord0 .. TVertexFormat.TexCoord3 : begin
            desc.SemanticName := 'TEXCOORD';
            desc.SemanticIndex := Ord(elementFormat) - Ord(TVertexFormat.TexCoord0);
            desc.Format := DXGI_FORMAT_R32G32_FLOAT;
         end;
         TVertexFormat.BiNormal : begin
            desc.SemanticName := 'BINORMAL';
            desc.Format := DXGI_FORMAT_R32G32B32_FLOAT;
         end;
         TVertexFormat.Tangent : begin
            desc.SemanticName := 'TANGENT';
            desc.Format := DXGI_FORMAT_R32G32B32_FLOAT;
         end;
         TVertexFormat.ColorF0 .. TVertexFormat.ColorF3 : begin
            desc.SemanticName := 'COLOR';
            desc.SemanticIndex := Ord(elementFormat) - Ord(TVertexFormat.ColorF0);
            desc.Format := DXGI_FORMAT_R32G32B32A32_FLOAT;
         end;
      else
         Assert(False);
      end;
   end;
end;

// PrimitiveKindUToDX11Topology
//
function PrimitiveKindUToDX11Topology(primitive : TPrimitivesKindU) : D3D11_PRIMITIVE_TOPOLOGY;
const
   cMap : array [ TPrimitivesKindU ] of D3D11_PRIMITIVE_TOPOLOGY = (
      D3D11_PRIMITIVE_TOPOLOGY_POINTLIST,
      D3D11_PRIMITIVE_TOPOLOGY_LINELIST,
      D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST,
      D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP
   );
begin
   Result := cMap[ primitive ];
end;

// PrimitiveKindToDX11Topology
//
function PrimitiveKindToDX11Topology(primitive : TPrimitivesKind) : D3D11_PRIMITIVE_TOPOLOGY;
begin
   Result := PrimitiveKindUToDX11Topology(TPrimitivesKindU(primitive));
end;

// StencilOpToDX11Op
//
function StencilOpToDX11Op(stencilOp : TStencilOp) : D3D11_STENCIL_OP;
const
   cMap : array [ TStencilOp ] of D3D11_STENCIL_OP = (
      D3D11_STENCIL_OP_KEEP,
      D3D11_STENCIL_OP_ZERO,
      D3D11_STENCIL_OP_REPLACE,
      D3D11_STENCIL_OP_INCR_SAT,
      D3D11_STENCIL_OP_DECR_SAT,
      D3D11_STENCIL_OP_INVERT
   );
begin
   Result := cMap[ stencilOp ];
end;

// StencilFuncToDX11Comp
//
function StencilFuncToDX11Comp(stencilFunc : TStencilFunc) : D3D11_COMPARISON_FUNC;
const
   cMap : array [ TStencilFunc ] of D3D11_COMPARISON_FUNC = (
      D3D11_COMPARISON_NEVER,
      D3D11_COMPARISON_LESS,
      D3D11_COMPARISON_LESS_EQUAL,
      D3D11_COMPARISON_GREATER,
      D3D11_COMPARISON_GREATER_EQUAL,
      D3D11_COMPARISON_EQUAL,
      D3D11_COMPARISON_NOT_EQUAL,
      D3D11_COMPARISON_ALWAYS
   );
begin
   Result := cMap[ stencilFunc ];
end;

// PixelFormatToDXGIFormat
//
function PixelFormatToDXGIFormat(pf : TPixelFormat) : DXGI_FORMAT;
begin
   case pf of
      TPixelFormat.RGBA16 :   Result := DXGI_FORMAT_R16G16B16A16_UNORM;
      TPixelFormat.RGB10_A2 : Result := DXGI_FORMAT_R10G10B10A2_UNORM;
      TPixelFormat.BGRA :     Result := DXGI_FORMAT_B8G8R8A8_UNORM;
      TPixelFormat.BGR :      Result := DXGI_FORMAT_B8G8R8X8_UNORM;
      TPixelFormat.RGBA :     Result := DXGI_FORMAT_R8G8B8A8_UNORM;
      TPixelFormat.BGR_565 :  Result := DXGI_FORMAT_B5G6R5_UNORM;
      TPixelFormat.BGR5_A1 :  Result := DXGI_FORMAT_B5G5R5A1_UNORM;
      TPixelFormat.LA :       Result := DXGI_FORMAT_R8G8_UNORM;
      TPixelFormat.R16F :     Result := DXGI_FORMAT_R16_FLOAT;
      TPixelFormat.RG16F :    Result := DXGI_FORMAT_R16G16_FLOAT;
      TPixelFormat.RGBA16F :  Result := DXGI_FORMAT_R16G16B16A16_FLOAT;
      TPixelFormat.R32F :     Result := DXGI_FORMAT_R32_FLOAT;
      TPixelFormat.RG32F :    Result := DXGI_FORMAT_R32G32_FLOAT;
      TPixelFormat.RGBA32F :  Result := DXGI_FORMAT_R32G32B32A32_FLOAT;
      TPixelFormat.A :        Result := DXGI_FORMAT_A8_UNORM;
   else
      Result := DXGI_FORMAT_UNKNOWN;
   end;
end;

// ------------------
// ------------------ TDX11Device ------------------
// ------------------

// Create
//
constructor TDX11Device.Create(debug : Boolean; whichGPU : Integer = -1);
var
   dxgiDevice : IDXGIDevice;
   dxgiAdapter : IDXGIAdapter1;
   adapterDesc : DXGI_ADAPTER_DESC;
begin
   inherited Create;
   var flags := D3D11_CREATE_DEVICE_BGRA_SUPPORT;
   if debug then
      flags := flags or D3D11_CREATE_DEVICE_DEBUG;

   FDriverType := D3D_DRIVER_TYPE_NULL;
   FFeatureLevel := D3D_FEATURE_LEVEL_11_0;
   if not GlobalUseDX then Exit;

   var hD3D11 := LoadLibrary(D3D11dll);
   if hD3D11 = 0 then Exit;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      var requestedDriverType := D3D_DRIVER_TYPE_HARDWARE;
      if GlobalUseDXSoftware then begin
         requestedDriverType := D3D_DRIVER_TYPE_WARP;
         whichGPU := -1;
      end;

      if whichGPU >= 0 then begin

         // Create DXGI Factory
         var hr := CreateDXGIFactory1(IDXGIFactory1, FDXGIFactory);
         if Failed(hr) then
            Exit;

         // Enumerate adapters, preferring high-performance GPU
         hr := FDXGIFactory.EnumAdapters1(whichGPU, dxgiAdapter);
         if Failed(hr) then
            raise Exception.CreateFmt('Failed to enumerate adapter %d', [ whichGPU ]);

         hr := D3D11CreateDevice(
            dxgiAdapter, D3D_DRIVER_TYPE_UNKNOWN, 0, flags,
            nil, 0, D3D11_SDK_VERSION, FDevice, FFeatureLevel, FDeviceContext
         );
         if Failed(hr) or (FDeviceContext = nil) then begin
            dxgiAdapter.GetDesc(adapterDesc);
            raise Exception.CreateFmt('Failed with %x when creating device context for adapter %d (%s)',
                                      [ hr, whichGPU, String(adapterDesc.Description) ]);
        end;

        FDriverType := D3D_DRIVER_TYPE_HARDWARE;

      end else begin

        var hr := D3D11CreateDevice(
           nil, requestedDriverType, 0, flags,
           nil, 0, D3D11_SDK_VERSION, FDevice, FFeatureLevel, FDeviceContext
        );
        if Failed(hr) then Exit;

        // from this point on, everything should succeed

        FDriverType := requestedDriverType;

        hr := FDevice.QueryInterface(IDXGIDevice, dxgiDevice);
        RaiseIfFailed(hr, 'TDX11Device IDXGIDevice');

        hr := dxgiDevice.GetParent(IDXGIAdapter, dxgiAdapter);
        RaiseIfFailed(hr, 'TDX11Device IDXGIAdapter');

        hr := dxgiAdapter.GetParent(IDXGIFactory1, FDXGIFactory);
        RaiseIfFailed(hr, 'TDX11Device IDXGIFactory1');

      end;
   finally
      SetExceptionMask(fpuMask);
      FreeLibrary(hD3D11);
   end;
end;

// Destroy
//
destructor TDX11Device.Destroy;
begin
   inherited;
end;

// CreateBuffer
//
function TDX11Device.CreateBuffer(const desc : TD3D11_BUFFER_DESC; const initialData : PD3D11_SUBRESOURCE_DATA) : ID3D11Buffer;
begin
   var hr := Device.CreateBuffer(desc, initialData, Result);
   RaiseIfFailed(hr, 'CreateBuffer');
end;

// CreateTexture2D
//
function TDX11Device.CreateTexture2D(const desc : TD3D11_TEXTURE2D_DESC; const initialData : PD3D11_SUBRESOURCE_DATA) : ID3D11Texture2D;
begin
   var hr := Device.CreateTexture2D(desc, initialData, Result);
   RaiseIfFailed(hr, 'CreateTexture2D');
end;

// CreateRenderTargetView
//
function TDX11Device.CreateRenderTargetView(const resource : ID3D11Resource; const desc : PD3D11_RENDER_TARGET_VIEW_DESC) : ID3D11RenderTargetView;
begin
   var hr := Device.CreateRenderTargetView(resource, desc, Result);
   RaiseIfFailed(hr, 'CreateRenderTargetView');
end;

// CreateDepthStencilView
//
function TDX11Device.CreateDepthStencilView(width, height : Integer; const sampleDesc : DXGI_SAMPLE_DESC) : ID3D11DepthStencilView;
var
   tex2D : ID3D11Texture2D;
begin
   var textureDesc := Default(TD3D11_TEXTURE2D_DESC);
   textureDesc.Width := width;
   textureDesc.Height := height;
   textureDesc.MipLevels := 1;
   textureDesc.ArraySize := 1;
   textureDesc.Format := DXGI_FORMAT_D24_UNORM_S8_UINT;
   textureDesc.SampleDesc := sampleDesc;
   textureDesc.Usage := D3D11_USAGE_DEFAULT;
   textureDesc.BindFlags := D3D11_BIND_DEPTH_STENCIL;
   tex2D := CreateTexture2D(textureDesc, nil);
   var hr := Device.CreateDepthStencilView(tex2D, nil, Result);
   RaiseIfFailed (hr, 'CreateDepthStencilView');
end;

// SetViewport
//
procedure TDX11Device.SetViewport(const viewport : TD3D11_Viewport);
begin
   DeviceContext.RSSetViewports(1, @viewport);
end;

// ClearViewport
//
procedure TDX11Device.ClearViewport;
begin
   DeviceContext.RSSetViewports(0, nil);
end;

// SetRenderTarget
//
procedure TDX11Device.SetRenderTarget(const targetView : ID3D11RenderTargetView; const depthStenciView : ID3D11DepthStencilView);
begin
   DeviceContext.OMSetRenderTargets(1, targetView, depthStenciView);
end;

// ClearRenderTarget
//
procedure TDX11Device.ClearRenderTarget;
begin
   DeviceContext.OMSetRenderTargets(0, nil, nil);
end;

// ClearRenderTargetView
//
procedure TDX11Device.ClearRenderTargetView(const targetView : ID3D11RenderTargetView; const aColor : TAlphaColor);
begin
   DeviceContext.ClearRenderTargetView(targetView, TFourSingleArray(TAlphaColorF.Create(aColor)));
end;

// ClearDepthStencilView
//
procedure TDX11Device.ClearDepthStencilView(const depthStencilView : ID3D11DepthStencilView; flags : TD3D11_CLEAR_FLAG; aDepth : Single; aStencil : Cardinal);
begin
   DeviceContext.ClearDepthStencilView(depthStencilView, flags, aDepth, aStencil);
end;

// Map
//
function TDX11Device.Map(const res : ID3D11Resource; mapType: D3D11_MAP) : TD3D11_MAPPED_SUBRESOURCE;
begin
   var hr := DeviceContext.Map(res, 0, mapType, 0, Result);
   RaiseIfFailed(hr, 'MapBuffer');
end;

// Unmap
//
procedure TDX11Device.Unmap(const res : ID3D11Resource);
begin
   DeviceContext.Unmap(res, 0);
end;

// CreateVertexShader
//
function TDX11Device.CreateVertexShader(const code : TContextShaderCode) : ID3D11VertexShader;
begin
   var hr := Device.CreateVertexShader(Pointer(code), Length(code), nil, @Result);
   RaiseIfFailed(hr, 'CreateVertexShader');
end;

// CreatePixelShader
//
function TDX11Device.CreatePixelShader(const code : TContextShaderCode) : ID3D11PixelShader;
begin
   var hr := Device.CreatePixelShader(Pointer(code), Length(code), nil, Result);
   RaiseIfFailed(hr, 'CreatePixelShader');
end;

// IASetVertexBuffers
//
procedure TDX11Device.IASetVertexBuffers(startSlot, numBuffers: UINT; const buffers: ID3D11Buffer; const strides, offsets: PUINT);
begin
   DeviceContext.IASetVertexBuffers(startSlot, numBuffers, buffers, strides, offsets);
end;

// IASetIndexBuffer
//
procedure TDX11Device.IASetIndexBuffer(const buffer : ID3D11Buffer; format : DXGI_FORMAT; offset : UINT);
begin
   DeviceContext.IASetIndexBuffer(buffer, format, offset);
end;

// SetSamplerDesc
//
procedure TDX11Device.SetSamplerDesc(slot : UINT; const samplerDesc : TD3D11_SAMPLER_DESC);
begin
   Assert(slot < UINT(Length(FSamplerDescArray)));

   if not CompareMem(@samplerDesc, @FSamplerDescArray[slot], SizeOf(TD3D11_SAMPLER_DESC)) then begin
      var hr := Device.CreateSamplerState(samplerDesc, FSamplerStateArray[slot]);
      RaiseIfFailed(hr, 'CreateSamplerState');
      FSamplerDescArray[slot] := samplerDesc;
      DeviceContext.PSSetSamplers(slot, 1, FSamplerStateArray[slot]);
   end;
end;

// GetSampleDesc
//
function TDX11Device.GetSampleDesc(slot : UINT) : PD3D11_SAMPLER_DESC;
begin
   Assert(slot < UINT(Length(FSamplerDescArray)));
   Result := @FSamplerDescArray[slot];
end;

// SetInputLayout
//
procedure TDX11Device.SetInputLayout(const aVertexDeclaration : TVertexDeclaration; const aVertexShaderSource : IContextShaderSource);
var
   inputLayoutHelper : IDX11InputLayoutHelper;

   procedure ApplyNewLayout;
   var
      inputElements : TD3D11_INPUT_ELEMENT_DESC_array;
      inputLayout : ID3D11InputLayout;
   begin
      inputElements := VertexElementsToDX11Declaration(aVertexDeclaration);
      var source := aVertexShaderSource.GetSelf;
      var hr := Device.CreateInputLayout(
         Pointer(inputElements), Length(inputElements),
         Pointer(source.Code), Length(source.Code),
         inputLayout
      );
      RaiseIfFailed(hr, 'CreateInputLayout');

      var helper := TDX11DeviceInputHelper.Create;
      inputLayoutHelper := helper;
      helper.VertexDeclaration := Copy(aVertexDeclaration);
      helper.InputLayout := inputLayout;
      aVertexShaderSource.GetSelf.UserData := inputLayoutHelper;
      DeviceContext.IASetInputLayout(inputLayout);
   end;

begin
   if FVertexShaderSource = aVertexShaderSource then begin
      // same vertex shader, check if vertex declaration was changed
      inputLayoutHelper := IDX11InputLayoutHelper(FVertexShaderSource.GetUserData);
      if not inputLayoutHelper.HasSameVertexDeclaration(aVertexDeclaration) then
         ApplyNewLayout;
   end else begin
      // different vertex shader, check if vertex declaration can be applied
      FVertexShaderSource := aVertexShaderSource;
      inputLayoutHelper := IDX11InputLayoutHelper(FVertexShaderSource.GetUserData);
      if (inputLayoutHelper <> nil) and inputLayoutHelper.HasSameVertexDeclaration(aVertexDeclaration) then
         DeviceContext.IASetInputLayout(inputLayoutHelper.GetSelf.InputLayout)
      else ApplyNewLayout;
   end;
end;

// DrawIndexed
//
procedure TDX11Device.DrawIndexed(topology : TPrimitivesKindU; indexCount : Integer);
begin
   DeviceContext.IASetPrimitiveTopology(PrimitiveKindUToDX11Topology(topology));
   DeviceContext.DrawIndexed(indexCount, 0, 0);
end;

// ------------------
// ------------------ TDX11DeviceStates ------------------
// ------------------

// Create
//
constructor TDX11DeviceStates.Create;
begin
   inherited;
   FBlendDesc := Default(TD3D11_BLEND_DESC);
   FBlendDesc.AlphaToCoverageEnable := False;
   FBlendDesc.RenderTarget[0].BlendEnable := True;
   FBlendDesc.RenderTarget[0].SrcBlend := D3D11_BLEND_ONE;
   FBlendDesc.RenderTarget[0].DestBlend := D3D11_BLEND_INV_SRC_ALPHA;
   FBlendDesc.RenderTarget[0].BlendOp := D3D11_BLEND_OP_ADD;
   FBlendDesc.RenderTarget[0].SrcBlendAlpha := D3D11_BLEND_ONE;
   FBlendDesc.RenderTarget[0].DestBlendAlpha := D3D11_BLEND_INV_SRC_ALPHA;
   FBlendDesc.RenderTarget[0].BlendOpAlpha := D3D11_BLEND_OP_ADD;
   FBlendDesc.RenderTarget[0].RenderTargetWriteMask := UINT8(D3D11_COLOR_WRITE_ENABLE_ALL);
   FBlendStateModified := True;

   FRasterizerDesc := Default(TD3D11_RASTERIZER_DESC);
   FRasterizerDesc.FillMode := D3D11_FILL_SOLID;
   FRasterizerDesc.CullMode := D3D11_CULL_BACK;
   FRasterizerDesc.DepthClipEnable := True;
   FRasterizerDesc.MultisampleEnable := True;
   FRasterizerDesc.AntialiasedLineEnable := True;
   FRasterizerStateModified := True;

   FDepthStencilDesc := Default(TD3D11_DEPTH_STENCIL_DESC);
   FDepthStencilDesc.DepthWriteMask := D3D11_DEPTH_WRITE_MASK_ALL;
   FDepthStencilDesc.DepthFunc := D3D11_COMPARISON_LESS_EQUAL;
   FDepthStencilModified := True;
   FStencilRef := 0;
end;

// GetZTest
//
function TDX11DeviceStates.GetZTest : Boolean;
begin
   Result := FDepthStencilDesc.DepthEnable;
end;

// SetZTest
//
procedure TDX11DeviceStates.SetZTest(val : Boolean);
begin
   if val <> ZTest then begin
      FDepthStencilDesc.DepthEnable := val;
      FDepthStencilModified := True;
   end;
end;

// GetZWrite
//
function TDX11DeviceStates.GetZWrite : Boolean;
begin
   Result := (FDepthStencilDesc.DepthWriteMask = D3D11_DEPTH_WRITE_MASK_ALL);
end;

// SetZWriteTest
//
procedure TDX11DeviceStates.SetZWriteTest(val : Boolean);
begin
   if val <> ZTest then begin
      if val then
         FDepthStencilDesc.DepthWriteMask := D3D11_DEPTH_WRITE_MASK_ALL
      else FDepthStencilDesc.DepthWriteMask := D3D11_DEPTH_WRITE_MASK_ZERO;
      FDepthStencilModified := True;
   end;
end;

// GetAlphaBlend
//
function TDX11DeviceStates.GetAlphaBlend : Boolean;
begin
   Result := FBlendDesc.RenderTarget[0].BlendEnable;
end;

// SetAlphaBlend
//
procedure TDX11DeviceStates.SetAlphaBlend(const val : Boolean);
begin
   if val <> AlphaBlend then begin
      FBlendDesc.RenderTarget[0].BlendEnable := val;
      FBlendStateModified := True;
   end;
end;

// GetStencil
//
function TDX11DeviceStates.GetStencil : Boolean;
begin
   Result := FDepthStencilDesc.StencilEnable;
end;

// SetStencil
//
procedure TDX11DeviceStates.SetStencil(const val : Boolean);
begin
   if val <> Stencil then begin
      FDepthStencilDesc.StencilEnable := val;
      FDepthStencilModified := True;
   end;
end;

// GetColorWrite
//
function TDX11DeviceStates.GetColorWrite : Boolean;
begin
   Result := (FBlendDesc.RenderTarget[0].RenderTargetWriteMask = Byte(D3D11_COLOR_WRITE_ENABLE_ALL));
end;

// SetColorWrite
//
procedure TDX11DeviceStates.SetColorWrite(const val : Boolean);
begin
   if val <> ColorWrite then begin
      if val then
         FBlendDesc.RenderTarget[0].RenderTargetWriteMask := Byte(D3D11_COLOR_WRITE_ENABLE_ALL)
      else FBlendDesc.RenderTarget[0].RenderTargetWriteMask := 0;
      FBlendStateModified := True;
   end;
end;

// GetScissor
//
function TDX11DeviceStates.GetScissor : Boolean;
begin
   Result := FRasterizerDesc.ScissorEnable;
end;

// SetScissor
//
procedure TDX11DeviceStates.SetScissor(const val : Boolean);
begin
   if val <> Scissor then begin
      FRasterizerDesc.ScissorEnable := val;
      FRasterizerStateModified := True;
   end;
end;

// GetCullMode
//
function TDX11DeviceStates.GetCullMode : TD3D11_CULL_MODE;
begin
   Result := FRasterizerDesc.CullMode;
end;

// SetCullMode
//
procedure TDX11DeviceStates.SetCullMode(const val : TD3D11_CULL_MODE);
begin
   if CullMode <> val then begin
      FRasterizerDesc.CullMode := val;
      FRasterizerStateModified := True;
   end;
end;

// SetStencilOp
//
procedure TDX11DeviceStates.SetStencilOp(aFail, aZFail, aZPass : TD3D11_STENCIL_OP);
begin
   if    (FDepthStencilDesc.FrontFace.StencilFailOp <> aFail)
      or (FDepthStencilDesc.FrontFace.StencilDepthFailOp <> aZFail)
      or (FDepthStencilDesc.FrontFace.StencilPassOp <> aZPass) then begin
      FDepthStencilDesc.FrontFace.StencilFailOp := aFail;
      FDepthStencilDesc.FrontFace.StencilDepthFailOp := aZFail;
      FDepthStencilDesc.FrontFace.StencilPassOp := aZPass;
      FDepthStencilDesc.BackFace := FDepthStencilDesc.FrontFace;
      FDepthStencilModified := True;
   end;
end;

// SetStencilFunc
//
procedure TDX11DeviceStates.SetStencilFunc(aFunc : TD3D11_COMPARISON_FUNC; aRef, aMask : Cardinal);
begin
   if    (FDepthStencilDesc.FrontFace.StencilFunc <> aFunc)
      or (FDepthStencilDesc.StencilReadMask <> aMask)
      or (FStencilRef <> aRef) then begin
      FDepthStencilDesc.FrontFace.StencilFunc := aFunc;
      FDepthStencilDesc.BackFace.StencilFunc := aFunc;
      FDepthStencilDesc.StencilReadMask := aMask;
      FDepthStencilDesc.StencilWriteMask := aMask;
      FStencilRef := aRef;
      FDepthStencilModified := True;
   end;
end;

// ApplyChanges
//
procedure TDX11DeviceStates.ApplyChanges(device : TDX11Device);
begin
   if FBlendStateModified then begin
      FBlendState := nil;
      device.Device.CreateBlendState(FBlendDesc, FBlendState);
      device.DeviceContext.OMSetBlendState(FBlendState, cD3DWhite, $FFFFFFFF);
      FBlendStateModified := False;
   end;

   if FDepthStencilModified then begin
      FDepthStencilState := nil;
      device.Device.CreateDepthStencilState(FDepthStencilDesc, FDepthStencilState);
      device.DeviceContext.OMSetDepthStencilState(FDepthStencilState, FStencilRef);
      FDepthStencilModified := False;
   end;

   if FRasterizerStateModified then begin
      FRasterizerState := nil;
      device.Device.CreateRasterizerState(FRasterizerDesc, FRasterizerState);
      device.DeviceContext.RSSetState(FRasterizerState);
      FRasterizerStateModified := False;
   end;
end;

// ------------------
// ------------------ TDX11DeviceInputHelper ------------------
// ------------------

// GetSelf
//
function TDX11DeviceInputHelper.GetSelf : TDX11DeviceInputHelper;
begin
   Result := Self;
end;

// HasSameVertexDeclaration
//
function TDX11DeviceInputHelper.HasSameVertexDeclaration(const aVertexDeclaration : TVertexDeclaration) : Boolean;
begin
   Result := SameVertexDeclaration(FVertexDeclaration, aVertexDeclaration);
end;

end.
