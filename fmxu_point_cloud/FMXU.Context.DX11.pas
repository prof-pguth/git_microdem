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
{    DirectX 11 context, requires D3D_FEATURE_LEVEL_11_0 or more       }
{                                                                      }
{**********************************************************************}
unit FMXU.Context.DX11;

{$i fmxu.inc}

interface

uses
   Winapi.Windows, Winapi.DXTypes, Winapi.DXGI, Winapi.D3D11, Winapi.D3DCommon,
   Winapi.DxgiType, Winapi.DxgiFormat,
   System.Types, System.UITypes, System.SysUtils, System.Classes, System.Math,
   System.Math.Vectors,
   FMXU.Context, FMXU.Buffers, FMXU.DX11.Utils,
   FMX.Types3D, FMX.Types, FMX.Graphics;

type
   TDX11SharedBufferType = (
      sbtVertexBuffer, sbtIndexBuffer,
      sbtVertexVariables, sbtPixelVariables
   );
   TDX11SharedBuffer = record
      Buffer : ID3D11Buffer;
      Size, Offset : Cardinal;
   end;
   PDX11SharedBuffer = ^TDX11SharedBuffer;

   TFMXUContext3D_DX11 = class (TFMXUContext3D)
      protected class var
         vDevice : TDX11Device;
         vStates : TDX11DeviceStates;

         vBlankTex2D : ID3D11Texture2D;

         vVertexShaderModified, vPixelShaderModified : Boolean;
         vVertexShaderSource, vPixelShaderSource : IContextShaderSource;
         vVertexShaderVariables, vPixelShaderVariables : TBytes;

         // FMX limited to 16 instead of D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT ?
         vVertexShaderResourceViews : array [ 0..15 ] of ID3D11ShaderResourceView;
         vPixelShaderResourceViews : array [ 0..15 ] of ID3D11ShaderResourceView;

         vSharedBuffers : array [ TDX11SharedBufferType ] of TDX11SharedBuffer;

         class procedure ReleasePrivateVars;

      protected
         FVSync : Boolean;
         FSwapChain : IDXGISwapChain;

         FRenderTargetTex2D : ID3D11Texture2D;
         FRenderTargetView : ID3D11RenderTargetView;

         FDepthStencilTex2D : ID3D11Texture2D;
         FDepthStencilView : ID3D11DepthStencilView;

         FCopyToBitsTex2D : ID3D11Texture2D;

         // shared buffers methods

         procedure EnsureSharedBufferSize(typ : TDX11SharedBufferType; minByteSize : Cardinal);
         procedure MapToSharedBuffer(typ : TDX11SharedBufferType; data : Pointer; dataSize : Cardinal);
         procedure OffsetSharedBuffer(typ : TDX11SharedBufferType; dataSize : Cardinal);

         procedure SetSharedVertexBuffer(vertexSize : Cardinal; data : Pointer; dataSize : Cardinal);
         procedure SetSharedIndexBuffer(indexSize : Cardinal; data : Pointer; dataSize : Cardinal);

         procedure SetShaderVariables(typ : TDX11SharedBufferType; const variablesData : TBytes);

         // overrides of TContext3D virtual methods

         function GetIndexBufferSupport : TContext3D.TIndexBufferSupport; override;

         procedure DoCreateBuffer; override;
         procedure DoFreeBuffer; override;

         procedure DoResize; override;

         procedure DoCopyToBitmap(const aDest : FMX.Graphics.TBitmap; const aRect : TRect); override;
         procedure DoCopyToBits(const bits: Pointer; const pitchInBytes : Integer; const aRect : TRect); override;

         function DoBeginScene: Boolean; override;
         procedure DoEndScene; override;

         procedure DoClear(const aTarget: TClearTargets; const aColor: TAlphaColor;
                           const aDepth: Single; const aStencil: Cardinal); override;
         procedure DoSetContextState(AState: TContextState); override;
         procedure DoSetStencilOp(const aFail, aZFail, aZPass: TStencilOp); override;
         procedure DoSetStencilFunc(const aFunc : TStencilfunc; aRef, aMask : Cardinal); override;
         procedure DoSetScissorRect(const aScissorRect : TRect); override;


         procedure DoDrawPrimitivesBatch(
            const aKind : TPrimitivesKind;
            const vertices, indices: Pointer;
            const vertexDeclaration : TVertexDeclaration;
            const vertexSize, vertexCount, indexSize, indexCount : Integer
            ); override; final;
         procedure DoDrawPrimitivesBatchU(
            const aKind : TPrimitivesKindU;
            const vertices, indices: Pointer;
            const vertexDeclaration : TVertexDeclaration;
            const vertexSize, vertexCount, indexSize, indexCount : Integer
            ); virtual;

         class procedure DoInitializeTexture(const aTexture : TTexture); override;
         class procedure DoFinalizeTexture(const aTexture : TTexture); override;
         class procedure DoUpdateTexture(const aTexture : TTexture; const data : Pointer; const rowPitch : Integer); override;

         class function DoBitmapToTexture(const aBitmap : TBitmap): TTexture; override;

         class procedure DoInitializeShader(const aShader : TContextShader); override;
         class procedure DoFinalizeShader(const aShader : TContextShader); override;

         procedure DoSetShaders(const aVertexShader, aPixelShader : TContextShader); override;
         procedure DoSetShaderVariable(const aName : String; const data : array of TVector3D); override;
         procedure DoSetShaderVariable(const aName : String; const aTexture : TTexture); override;
         procedure DoSetShaderVariable(const aName : String; const matrix : TMatrix3D); override;

         constructor CreateFromWindow(const aParent : TWindowHandle; const aWidth, aHeight : Integer;
                                      const aMultisample : TMultisample; const aDepthStencil : Boolean); override;
         constructor CreateFromTexture(const aTexture : TTexture; const aMultisample : TMultisample;
                                       const aDepthStencil : Boolean); override;

      public
         class procedure TestDriverSupport(
            out aDriverType : D3D_DRIVER_TYPE;
            out aFeatureLevel : TD3D_FEATURE_LEVEL;
            whichGPU : Integer = -1);

         class function BlankTexture : ID3D11Texture2D;

         class function CreateBuffer(dataSize, bindType : Cardinal; usage : TD3D11_USAGE; cpuAccess : UINT) : ID3D11Buffer;
         class function CreateBufferFromData(dataSize, bindType : Cardinal; usage : TD3D11_USAGE; cpuAccess : UINT; dataPointer : Pointer) : ID3D11Buffer;
         class function MapBuffer(const buffer : ID3D11Buffer; mapType: D3D11_MAP) : TD3D11_MAPPED_SUBRESOURCE;
         class procedure UnmapBuffer(const buffer : ID3D11Buffer);

         function  MapToBits(const aRect : TRect) : TD3D11_MAPPED_SUBRESOURCE;
         procedure UnmapToBits;

         class function Valid : Boolean; override;
         class function PixelFormat : TPixelFormat; override;
         class function MaxTextureSize : Integer; override;

         procedure DrawGPUPrimitives(
            const aKind : TPrimitivesKindU;
            const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer
         ); override;

         // VSync control, only for window target, default false (FMLX d
         property VSync : Boolean read FVSync write FVSync;
   end;

procedure RegisterDX11ContextU(whichGPU : Integer = -1);
procedure UnregisterDX11ContextU;

// Set to True before registering the context if you want DX in debug mode
var vDX11_Debug : Boolean = False;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   FMX.Platform.Win, FMX.Platform, FMX.Canvas.GPU, FMX.Utils,
   FMXU.Buffers.DX11;

{$R-}

const
   cRGB32White : UINT32 = $FFFFFFFF;
   cMaxTextureSize = 16384; // DX11_0
   cMinShaderVariableSlotSize = 4*SizeOf(Single);

// RegisterDX11ContextU
//
procedure RegisterDX11ContextU(whichGPU : Integer = -1);
var
   driverType : D3D_DRIVER_TYPE;
   featureLevel : TD3D_FEATURE_LEVEL;
begin
   TFMXUContext3D_DX11.TestDriverSupport(driverType, featureLevel, whichGPU);
   if (driverType <> D3D_DRIVER_TYPE_NULL) and (featureLevel >= D3D_FEATURE_LEVEL_11_0) then begin
      TContextManager.RegisterContext(TFMXUContext3D_DX11, True);
      TGPUVertexBuffer.RegisterGPUVertexBufferClass(TGPUVertexBufferDX11);
      TGPUIndexBuffer.RegisterGPUIndexBufferClass(TGPUIndexBufferDX11);
   end;
end;

// UnregisterDX11ContextU
//
procedure UnregisterDX11ContextU;
begin
   TFMXUContext3D_DX11.ReleasePrivateVars;
end;

// ------------------
// ------------------ TFMXUContext3D_DX11 ------------------
// ------------------

// ReleasePrivateVars
//
class procedure TFMXUContext3D_DX11.ReleasePrivateVars;
begin
   for var i := Low(vSharedBuffers) to High(vSharedBuffers) do
      vSharedBuffers[i] := Default(TDX11SharedBuffer);
   for var i := Low(vVertexShaderResourceViews) to High(vVertexShaderResourceViews) do
      vVertexShaderResourceViews[i] := nil;
   for var i := Low(vPixelShaderResourceViews) to High(vPixelShaderResourceViews) do
      vPixelShaderResourceViews[i] := nil;

   vVertexShaderSource := nil;
   vPixelShaderSource := nil;
   vVertexShaderVariables := nil;
   vPixelShaderVariables := nil;

   vBlankTex2D := nil;

   FreeAndNil(TFMXUContext3D_DX11.vDevice);
   FreeAndNil(TFMXUContext3D_DX11.vStates);
end;

// TestDriverSupport
//
class procedure TFMXUContext3D_DX11.TestDriverSupport(
   out aDriverType : D3D_DRIVER_TYPE;
   out aFeatureLevel : TD3D_FEATURE_LEVEL;
   whichGPU : Integer = -1
   );
begin
   // note that we create an actual device and hold it,
   // if the Test method was really meant for testing, this would be problematic
   // but it's just to check for registration, and we assume here that if
   // the test is successfull, the device will be used, and it makes no sense
   // to discard it just to recreate it afterwards
   if vDevice = nil then
      vDevice := TDX11Device.Create(vDX11_Debug, whichGPU);

   aDriverType := vDevice.DriverType;
   aFeatureLevel := vDevice.FeatureLevel;
end;

// BlankTexture
//
class function TFMXUContext3D_DX11.BlankTexture : ID3D11Texture2D;

   procedure CreateBlankTexture;
   begin
      var textureDesc := Default(TD3D11_TEXTURE2D_DESC);
      textureDesc.Width := 1;
      textureDesc.Height := 1;
      textureDesc.MipLevels := 1;
      textureDesc.ArraySize := 1;
      textureDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      textureDesc.SampleDesc.Count := 1;
      textureDesc.Usage := D3D11_USAGE_IMMUTABLE;
      textureDesc.BindFlags := D3D11_BIND_SHADER_RESOURCE;

      var resourceData := Default(TD3D11_SUBRESOURCE_DATA);
      resourceData.pSysMem := @cRGB32White;
      resourceData.SysMemPitch := 4;

      var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
      try
         vBlankTex2D := vDevice.CreateTexture2D(textureDesc, @resourceData);
      finally
         SetExceptionMask(fpuMask);
      end;
   end;

begin
   if vBlankTex2D = nil then
      CreateBlankTexture;
   Result := vBlankTex2D;
end;

// CreateBuffer
//
class function TFMXUContext3D_DX11.CreateBuffer(dataSize, bindType : Cardinal; usage : TD3D11_USAGE; cpuAccess : UINT) : ID3D11Buffer;
begin
   Result := vDevice.CreateBuffer(TD3D11_BUFFER_DESC.Create(dataSize, bindType, usage, cpuAccess), nil);
end;

// CreateBufferFromData
//
class function TFMXUContext3D_DX11.CreateBufferFromData(dataSize, bindType : Cardinal; usage : TD3D11_USAGE; cpuAccess : UINT; dataPointer : Pointer) : ID3D11Buffer;
begin
   var resData := Default(TD3D11_SUBRESOURCE_DATA);
   resData.pSysMem := dataPointer;

   Result := vDevice.CreateBuffer(TD3D11_BUFFER_DESC.Create(dataSize, bindType, usage, cpuAccess), @resData);
end;

// MapBuffer
//
class function TFMXUContext3D_DX11.MapBuffer(const buffer : ID3D11Buffer; mapType: D3D11_MAP) : TD3D11_MAPPED_SUBRESOURCE;
begin
   Result := vDevice.Map(buffer, mapType);
end;

// UnmapBuffer
//
class procedure TFMXUContext3D_DX11.UnmapBuffer(const buffer : ID3D11Buffer);
begin
   vDevice.Unmap(buffer);
end;

// GetIndexBufferSupport
//
function TFMXUContext3D_DX11.GetIndexBufferSupport: TContext3D.TIndexBufferSupport;
begin
   Result := TIndexBufferSupport.Int32;
end;

// MaxTextureSize
//
class function TFMXUContext3D_DX11.MaxTextureSize: Integer;
begin
   Result := cMaxTextureSize;
end;

// CreateFromWindow
//
constructor TFMXUContext3D_DX11.CreateFromWindow(
   const aParent : TWindowHandle; const aWidth, aHeight : Integer;
   const aMultisample : TMultisample; const aDepthStencil : Boolean
   );
begin
   //inherited;
   vStates := TDX11DeviceStates.Create;
   inherited CreateBuffer;
end;

// CreateFromTexture
//
constructor TFMXUContext3D_DX11.CreateFromTexture(
   const aTexture : TTexture; const aMultisample : TMultisample;
   const aDepthStencil : Boolean
   );
begin
   inherited;
   vStates := TDX11DeviceStates.Create;
   inherited CreateBuffer;
end;

// Valid
//
class function TFMXUContext3D_DX11.Valid : Boolean;
begin
   Result := (vDevice.DeviceContext <> nil);
end;

// PixelFormat
//
class function TFMXUContext3D_DX11.PixelFormat: TPixelFormat;
begin
   Result := TPixelFormat.BGRA;
end;

// DoCreateBuffer
//
procedure TFMXUContext3D_DX11.DoCreateBuffer;

   function GetDXGISampleDesc(aMultisample : TMultisample; aFormat : DXGI_FORMAT) : DXGI_SAMPLE_DESC;
   begin
      Result.Count := 1;
      Result.Quality := 0;

      var maxSampleCount := MultisampleToSampleCount(aMultisample);

      Assert((aFormat <> DXGI_FORMAT_UNKNOWN) and Valid);

      var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
      try
         for var i := maxSampleCount downto 2 do begin
            var numQualityLevels : Cardinal := 0;
            var hr := vDevice.Device.CheckMultisampleQualityLevels(aFormat, i, numQualityLevels);
            if Succeeded(hr) and (numQualityLevels > 0) then begin
               Result.Count := i;
               Result.Quality := numQualityLevels - 1;
               Exit;
            end;
         end;
      finally
         SetExceptionMask(fpuMask);
      end;
   end;

   procedure CreateTextureRenderTarget;
   var
      textureDesc : TD3D11_TEXTURE2D_DESC;
      iTex2D : ID3D11Texture2D;
   begin
      var sampleDesc := GetDXGISampleDesc(Multisample, PixelFormatToDXGIFormat(Texture.PixelFormat));
      if sampleDesc.Count > 1 then begin
         // multisample texture render target
         textureDesc := Default(TD3D11_TEXTURE2D_DESC);
         textureDesc.Width := Texture.Width;
         textureDesc.Height := Texture.Height;
         textureDesc.MipLevels := 1;
         textureDesc.ArraySize := 1;
         textureDesc.Format := PixelFormatToDXGIFormat(Texture.PixelFormat);
         textureDesc.SampleDesc := sampleDesc;
         textureDesc.Usage := D3D11_USAGE_DEFAULT;
         textureDesc.BindFlags := D3D11_BIND_RENDER_TARGET;
         FRenderTargetTex2D := vDevice.CreateTexture2D(textureDesc, nil);
         if FRenderTargetTex2D <> nil then begin
            FRenderTargetView := vDevice.CreateRenderTargetView(FRenderTargetTex2D, nil);
            if DepthStencil then begin
               FRenderTargetTex2D.GetDesc(textureDesc);
               FDepthStencilView := vDevice.CreateDepthStencilView(textureDesc.Width, textureDesc.Height, sampleDesc);
            end;
         end;
      end else begin
         // normal texture render target
         iTex2D := GetResource(Texture.Handle) as ID3D11Texture2D;
         if iTex2D <> nil then begin
            iTex2D.GetDesc(textureDesc);
            FRenderTargetView := vDevice.CreateRenderTargetView(iTex2D, nil);
            if DepthStencil then begin
               sampleDesc.Count := 1;
               sampleDesc.Quality := 0;
               FDepthStencilView := vDevice.CreateDepthStencilView(textureDesc.Width, textureDesc.Height, sampleDesc);
            end;
         end;
      end;
   end;

   procedure CreateWindowRenderTarget;
   var
      backBuffer : ID3D11Texture2D;
      renderingSetupService : IFMXRenderingSetupService;
   begin
      var colorBits := 0;
      var depthBits := 24;
      var useStencil := DepthStencil;
      var sampleCount := MultisampleToSampleCount(Multisample);

      if TPlatformServices.Current.SupportsPlatformService(IFMXRenderingSetupService, renderingSetupService) then
         renderingSetupService.Invoke(colorBits, depthBits, useStencil, sampleCount);

      var bufferSize := WindowHandleToPlatform(Parent).WndClientSize;

      var swapDesc := Default(TDXGISwapChainDesc);
      swapDesc.BufferDesc.Width := bufferSize.Width;
      swapDesc.BufferDesc.Height := bufferSize.Height;
      swapDesc.BufferDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      swapDesc.SampleDesc := GetDXGISampleDesc(SampleCountToMultisample(sampleCount), swapDesc.BufferDesc.Format);
      swapDesc.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
      swapDesc.BufferCount := 1;
      swapDesc.OutputWindow := WindowHandleToPlatform(Parent).Wnd;
      swapDesc.Windowed := True;

      var hr := vDevice.DXGIFactory.CreateSwapChain(vDevice.Device, swapDesc, FSwapChain);
      RaiseIfFailed(hr, 'CreateWindowRenderTarget CreateSwapChain');

      vDevice.DXGIFactory.MakeWindowAssociation(WindowHandleToPlatform(Parent).Wnd, DXGI_MWA_NO_WINDOW_CHANGES);
      hr := FSwapChain.GetBuffer(0, ID3D11Texture2D, backBuffer);
      RaiseIfFailed(hr, 'CreateWindowRenderTarget GetBuffer');

      FRenderTargetView := vDevice.CreateRenderTargetView(backBuffer, nil);
      if (depthBits > 0) or useStencil then begin
         FDepthStencilView := vDevice.CreateDepthStencilView(swapDesc.BufferDesc.Width, swapDesc.BufferDesc.Height, swapDesc.SampleDesc);
      end;
   end;

begin
   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      if Texture <> nil then
         CreateTextureRenderTarget
      else CreateWindowRenderTarget;
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoFreeBuffer
//
procedure TFMXUContext3D_DX11.DoFreeBuffer;
begin
   FSwapChain := nil;
   FRenderTargetTex2D := nil;
   FRenderTargetView := nil;
   FDepthStencilTex2D := nil;
   FDepthStencilView := nil;
   FCopyToBitsTex2D := nil;
end;

// DoResize
//
procedure TFMXUContext3D_DX11.DoResize;
begin
   // nothing
end;

// DoClear
//
procedure TFMXUContext3D_DX11.DoClear(
   const aTarget : TClearTargets; const aColor : TAlphaColor;
   const aDepth : Single; const aStencil : Cardinal
   );
begin
   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      if (TClearTarget.Color in aTarget) and (FRenderTargetView <> nil) then
         vDevice.ClearRenderTargetView(FRenderTargetView, aColor);
      if DepthStencil then begin
         var flags : TD3D11_CLEAR_FLAG :=
              UInt(Ord(TClearTarget.Depth in aTarget)) * D3D11_CLEAR_DEPTH
            + UInt(Ord(TClearTarget.Stencil in aTarget)) * D3D11_CLEAR_STENCIL;
         if flags <> 0 then
            vDevice.ClearDepthStencilView(FDepthStencilView, flags, aDepth, aStencil);
      end;
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoCopyToBitmap
//
procedure TFMXUContext3D_DX11.DoCopyToBitmap(const aDest : FMX.Graphics.TBitmap; const aRect : TRect);
begin
   if (Texture <> nil) and (TCanvasStyle.NeedGPUSurface in aDest.CanvasClass.GetCanvasStyle) then begin

      var gpuCanvas := (aDest.Canvas as TCustomCanvasGpu);
      if not gpuCanvas.BeginScene then
         raise EFMXU_DX11Exception.Create('Failed to BeginScene in DoCopyToBitmap');
      try
         var copyRect := TRect.Intersect(aRect, TRect.Create(0, 0, Width, Height));

         gpuCanvas.Clear(0);

         var oldMatrix := gpuCanvas.Matrix;
         try
            var f : Single := 1 / aDest.BitmapScale;
            gpuCanvas.SetMatrix(TMatrix.CreateScaling(f, f));
            gpuCanvas.DrawTexture(
               copyRect, TRectF.Create(0, 0, copyRect.Width, copyRect.Height),
               cRGB32White, Texture
            );
         finally
            gpuCanvas.SetMatrix(oldMatrix);
         end;
      finally
         gpuCanvas.EndScene;
      end;

   end else inherited;
end;

// DoCopyToBits
//
procedure TFMXUContext3D_DX11.DoCopyToBits(const bits: Pointer; const pitchInBytes : Integer; const aRect: TRect);
begin
   var mappedSubres := MapToBits(aRect);
   try
      if     (aRect.Left = 0) and (aRect.Top = 0)
         and (aRect.Width = Width) and (aRect.Height = Height)
         and (pitchInBytes = Width * 4)
         and (mappedSubres.RowPitch = Cardinal(pitchInBytes)) then begin
         // same layout, whole rect, straight copy is possible
         Move(mappedSubres.pData^, bits^, pitchInBytes * Height)
      end else begin
         // layout does not match, copy by rows
         var pSource := PAlphaColorArray(mappedSubres.pData);  Inc(pSource, aRect.Left);
         var pDest   := PAlphaColorArray(bits);                Inc(pDest, aRect.Left);
         var w4 := aRect.Width * 4;
         var sourcePitchInPixels := mappedSubres.RowPitch div 4;
         var destPitchInPixels := UInt(pitchInBytes) div 4;
         for var i : Cardinal := aRect.Top to aRect.Bottom - 1 do
            Move(pSource[i * sourcePitchInPixels], pDest[i * destPitchInPixels], w4);
      end;
   finally
      UnmapToBits;
   end;
end;

// MapToBits
//
function TFMXUContext3D_DX11.MapToBits(const aRect : TRect) : TD3D11_MAPPED_SUBRESOURCE;

   procedure PrepareBuffer;
   begin
      var textureDesc := Default(TD3D11_TEXTURE2D_DESC);
      textureDesc.Width := Trunc(Width * Scale); // px
      textureDesc.Height := Trunc(Height * Scale); // px
      textureDesc.MipLevels := 1;
      textureDesc.ArraySize := 1;
      textureDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      textureDesc.SampleDesc.Count := 1;
      textureDesc.Usage := D3D11_USAGE_STAGING;
      textureDesc.CPUAccessFlags := D3D11_CPU_ACCESS_READ;
      FCopyToBitsTex2D := vDevice.CreateTexture2D(textureDesc, nil);
   end;

var
   backBuffer : ID3D11Texture2D;
begin
   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      if FCopyToBitsTex2D = nil then
         PrepareBuffer;

      if Texture = nil then begin
         var hr := FSwapChain.GetBuffer(0, ID3D11Texture2D, backBuffer);
         RaiseIfFailed(hr, 'DoCopyToBits GetBuffer');
      end else backBuffer := GetResource(Texture.Handle) as ID3D11Texture2D;

      vDevice.DeviceContext.CopySubresourceRegion(FCopyToBitsTex2D, 0, 0, 0, 0, backBuffer, 0, nil);

      Result := vDevice.Map(FCopyToBitsTex2D, D3D11_MAP_READ);
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// UnmapToBits
//
procedure TFMXUContext3D_DX11.UnmapToBits;
begin
   vDevice.Unmap(FCopyToBitsTex2D);
end;

// DoBeginScene
//
function TFMXUContext3D_DX11.DoBeginScene : Boolean;
begin
   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      vDevice.SetRenderTarget(FRenderTargetView, FDepthStencilView);

      // FMX texture rendering target doesn't honour Scale
      var f := Scale;
      if Texture <> nil then
         f := 1;

      vDevice.SetViewport(TD3D11_VIEWPORT.Create(
         0, 0, Width * f, Height * f, 0, 1
      ));

      Result := inherited;
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoEndScene
//
procedure TFMXUContext3D_DX11.DoEndScene;
begin
   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      if Texture <> nil then begin
         // copy render target texture to texture
         vDevice.DeviceContext.ResolveSubresource(
            GetResource(Texture.Handle) as ID3D11Texture2D,
            0, FRenderTargetTex2D,
            0, PixelFormatToDXGIFormat(Texture.PixelFormat)
         );
      end else begin
         // swap to present
         var hr := FSwapChain.Present(Ord(VSync), 0);
         RaiseIfFailed(hr, 'DoEndScene Present');
      end;

      vDevice.ClearRenderTarget;
      vDevice.ClearViewport;
   finally
      SetExceptionMask(fpuMask);
   end;
   inherited;
end;

// DoBitmapToTexture
//
class function TFMXUContext3D_DX11.DoBitmapToTexture(const aBitmap : TBitmap) : TTexture;
begin
   if aBitmap.CanvasClass.InheritsFrom(TCustomCanvasGpu) then
      Result := TBitmapCtx(aBitmap.Handle).PaintingTexture
   else Result := inherited DoBitmapToTexture(aBitmap);
end;

// DoSetContextState
//
procedure TFMXUContext3D_DX11.DoSetContextState(aState: TContextState);
begin
   case aState of
      TContextState.cs2DScene, TContextState.cs3DScene : ;// ignored here

      TContextState.csZTestOn  : vStates.ZTest := True;
      TContextState.csZTestOff : vStates.ZTest := False;

      TContextState.csZWriteOn  : vStates.ZWrite := True;
      TContextState.csZWriteOff : vStates.ZWrite := False;

      TContextState.csAlphaBlendOn  : vStates.AlphaBlend := True;
      TContextState.csAlphaBlendOff : vStates.AlphaBlend := False;

      TContextState.csStencilOn  : vStates.Stencil := True;
      TContextState.csStencilOff : vStates.Stencil := False;

      TContextState.csColorWriteOn  : vStates.ColorWrite := True;
      TContextState.csColorWriteOff : vStates.ColorWrite := False;

      TContextState.csScissorOn  : vStates.Scissor := True;
      TContextState.csScissorOff : vStates.Scissor := False;

      TContextState.csFrontFace : vStates.CullMode := D3D11_CULL_BACK;
      TContextState.csBackFace : vStates.CullMode := D3D11_CULL_FRONT;
      TContextState.csAllFace : vStates.CullMode := D3D11_CULL_NONE;
   else
      Assert(False);
   end;
end;

// DoSetStencilOp
//
procedure TFMXUContext3D_DX11.DoSetStencilOp(const aFail, aZFail, aZPass: TStencilOp);
begin
   vStates.SetStencilOp(StencilOpToDX11Op(aFail), StencilOpToDX11Op(aZFail), StencilOpToDX11Op(aZPass));
end;

// DoSetStencilFunc
//
procedure TFMXUContext3D_DX11.DoSetStencilFunc(const aFunc : TStencilfunc; aRef, aMask : Cardinal);
begin
   vStates.SetStencilFunc(StencilFuncToDX11Comp(aFunc), aRef, aMask);
end;

// DoDrawPrimitivesBatch
//
procedure TFMXUContext3D_DX11.DoDrawPrimitivesBatch(
   const aKind : TPrimitivesKind;
   const vertices, indices: Pointer;
   const vertexDeclaration : TVertexDeclaration;
   const vertexSize, vertexCount, indexSize, indexCount : Integer
   );
begin
   DoDrawPrimitivesBatchU(
      TPrimitivesKindU(aKind),
      vertices, indices, vertexDeclaration,
      vertexSize, vertexCount, indexSize, indexCount
   );
end;

// DoDrawPrimitivesBatchU
//
procedure TFMXUContext3D_DX11.DoDrawPrimitivesBatchU(
   const aKind : TPrimitivesKindU;
   const vertices, indices: Pointer;
   const vertexDeclaration : TVertexDeclaration;
   const vertexSize, vertexCount, indexSize, indexCount : Integer
   );
begin
   Assert(CurrentVertexShader <> nil, 'Vertex shader is missing');

   var vertexBufferByteSize := vertexSize * vertexCount;
   var indexBufferByteSize := indexSize * indexCount;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try

      if vertices <> nil then
         SetSharedVertexBuffer(vertexSize, vertices, vertexBufferByteSize);
      if indices <> nil then
         SetSharedIndexBuffer(indexSize, indices, indexBufferByteSize);

      if vVertexShaderModified then begin
         SetShaderVariables(sbtVertexVariables, vVertexShaderVariables);
         vVertexShaderModified := False;
      end;
      if vPixelShaderModified then begin
         SetShaderVariables(sbtPixelVariables, vPixelShaderVariables);
         vPixelShaderModified := False;
      end;

      vStates.ApplyChanges(vDevice);

      vDevice.SetInputLayout(vertexDeclaration, vVertexShaderSource);

      vDevice.DrawIndexed(aKind, indexCount);

      if vertices <> nil then
         OffsetSharedBuffer(sbtVertexBuffer, vertexBufferByteSize);
      if indices <> nil then
         OffsetSharedBuffer(sbtIndexBuffer, indexBufferByteSize);

   finally
      SetExceptionMask(fpuMask);
   end;
end;

// EnsureSharedBufferSize
//
procedure TFMXUContext3D_DX11.EnsureSharedBufferSize(typ : TDX11SharedBufferType; minByteSize : Cardinal);
const
   cBindBuffer : array [ TDX11SharedBufferType ] of UINT = (
      D3D11_BIND_VERTEX_BUFFER, D3D11_BIND_INDEX_BUFFER,
      D3D11_BIND_CONSTANT_BUFFER, D3D11_BIND_CONSTANT_BUFFER
   );
begin
   if minByteSize <= vSharedBuffers[typ].Size then Exit;

   if minByteSize < 32*1024 then
      minByteSize := 32*1024;

   vSharedBuffers[typ].Size := 0;
   vSharedBuffers[typ].Offset := 0;
   vSharedBuffers[typ].Buffer := nil;

   vSharedBuffers[typ].Buffer := CreateBuffer(
      minByteSize, cBindBuffer[typ],
      D3D11_USAGE_DYNAMIC, D3D11_CPU_ACCESS_WRITE
   );

   vSharedBuffers[typ].Size := minByteSize;
end;

// MapToSharedBuffer
//
procedure TFMXUContext3D_DX11.MapToSharedBuffer(typ : TDX11SharedBufferType; data : Pointer; dataSize : Cardinal);
var
   mapFlags : UINT;
begin
   EnsureSharedBufferSize(typ, dataSize);

   if vSharedBuffers[typ].Offset + dataSize > vSharedBuffers[typ].Size then begin
      vSharedBuffers[typ].Offset := 0;
      mapFlags := D3D11_MAP_WRITE_DISCARD;
   end else begin
      mapFlags := D3D11_MAP_WRITE_NO_OVERWRITE;
   end;

   var mappedRes := MapBuffer(vSharedBuffers[typ].Buffer, mapFlags);
   try
      var destPtr := Pointer(UIntPtr(mappedRes.pData) + vSharedBuffers[typ].Offset);
      System.Move(data^, destPtr^, dataSize);
   finally
     UnmapBuffer(vSharedBuffers[typ].Buffer);
   end;
end;

// OffsetSharedBuffer
//
procedure TFMXUContext3D_DX11.OffsetSharedBuffer(typ : TDX11SharedBufferType; dataSize : Cardinal);
begin
   Inc(vSharedBuffers[typ].Offset, dataSize);
end;

// SetSharedVertexBuffer
//
procedure TFMXUContext3D_DX11.SetSharedVertexBuffer(vertexSize : Cardinal; data : Pointer; dataSize : Cardinal);
begin
   MapToSharedBuffer(sbtVertexBuffer, data, dataSize);
   vDevice.IASetVertexBuffers(
      0, 1, vSharedBuffers[sbtVertexBuffer].Buffer,
      @vertexSize, @vSharedBuffers[sbtVertexBuffer].Offset
   );
end;

// SetSharedIndexBuffer
//
procedure TFMXUContext3D_DX11.SetSharedIndexBuffer(indexSize : Cardinal; data : Pointer; dataSize : Cardinal);
begin
   MapToSharedBuffer(sbtIndexBuffer, data, dataSize);
   var dxgiFormat := DXGI_FORMAT_R16_UINT;
   if indexSize = 4 then
      dxgiFormat := DXGI_FORMAT_R32_UINT;
   vDevice.IASetIndexBuffer(
      vSharedBuffers[sbtIndexBuffer].Buffer, dxgiFormat,
      vSharedBuffers[sbtIndexBuffer].Offset
   );
end;

// SetShaderVariables
//
procedure TFMXUContext3D_DX11.SetShaderVariables(typ : TDX11SharedBufferType; const variablesData : TBytes);
begin
   var p : PDX11SharedBuffer := @vSharedBuffers[typ];
   p.Buffer := nil;
   p.Offset := 0;
   p.Size := Length(variablesData);
   if p.Size > 0 then begin
      p.Buffer := CreateBufferFromData(
         p.Size, D3D11_BIND_CONSTANT_BUFFER,
         D3D11_USAGE_IMMUTABLE, 0, Pointer(variablesData)
      );
   end;
   case typ of
      sbtVertexVariables :
         vDevice.DeviceContext.VSSetConstantBuffers(0, 1, p.Buffer);
      sbtPixelVariables :
         vDevice.DeviceContext.PSSetConstantBuffers(0, 1, p.Buffer);
   else
      Assert(False, 'SetShaderVariables is only for variables');
   end;
end;

// DoInitializeTexture
//
class procedure TFMXUContext3D_DX11.DoInitializeTexture(const aTexture : TTexture);
var
   tex2D : ID3D11Texture2D;
begin
   Assert(aTexture.Handle = 0);

   // TTexture is created with None, assume BGRA for FMX compatibility
   if aTexture.PixelFormat = TPixelFormat.None then
      aTexture.PixelFormat := TPixelFormat.BGRA;

   // local vars to reduce verbosity of the scoped enums 'in' tests
   var isDynamic := (TTextureStyle.Dynamic in aTexture.Style);
   var isRenderTarget := (TTextureStyle.RenderTarget in aTexture.Style);
   var useMipMaps := (TTextureStyle.MipMaps in aTexture.Style);

   var textureDesc := Default(TD3D11_TEXTURE2D_DESC);
   textureDesc.Width := aTexture.Width;
   textureDesc.Height := aTexture.Height;
   if useMipMaps and not isDynamic then
      textureDesc.MipLevels := 0
   else textureDesc.MipLevels := 1;
   textureDesc.ArraySize := 1;
   textureDesc.Format := PixelFormatToDXGIFormat(aTexture.PixelFormat);
   textureDesc.SampleDesc.Count := 1;
   if isDynamic and not isRenderTarget then begin
      textureDesc.Usage := D3D11_USAGE_DYNAMIC;
      textureDesc.CPUAccessFlags := D3D11_CPU_ACCESS_WRITE;
   end else begin
      textureDesc.Usage := D3D11_USAGE_DEFAULT;
   end;
   if isRenderTarget then
      textureDesc.BindFlags := D3D11_BIND_SHADER_RESOURCE or D3D11_BIND_RENDER_TARGET
   else textureDesc.BindFlags := D3D11_BIND_SHADER_RESOURCE;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      tex2D := vDevice.CreateTexture2D(textureDesc, nil);
   finally
      SetExceptionMask(fpuMask);
   end;
   ITextureAccess(aTexture).Handle := AddResource(tex2D);
end;

// DoFinalizeTexture
//
class procedure TFMXUContext3D_DX11.DoFinalizeTexture(const aTexture : TTexture);
begin
   if aTexture.Handle = 0 then Exit;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      RemoveResource(aTexture.Handle);
      ITextureAccess(aTexture).Handle := 0;
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoUpdateTexture
//
class procedure TFMXUContext3D_DX11.DoUpdateTexture(
   const aTexture: TTexture;
   const data : Pointer; const rowPitch : Integer
   );
var
   mappedRes : D3D11_MAPPED_SUBRESOURCE;
   tex2DBuffer, tex2D : ID3D11Texture2D;

   procedure CopyToMappedResource;
   begin
      if UInt(rowPitch) = mappedRes.RowPitch then begin
         // copy at once
         Move(data^, mappedRes.pData^, aTexture.Height * rowPitch)
      end else begin
         /// copy by rows
         var rowByteSize := Min(rowPitch, mappedRes.RowPitch);
         var pSrc := PByte(data);
         var pDest := PByte(mappedRes.pData);
         for var i : Cardinal := 0 to aTexture.Height - 1 do begin
            Move(pSrc^, pDest^, rowByteSize);
            Inc(pSrc, rowPitch);
            Inc(pDest, mappedRes.RowPitch);
         end;
      end;
   end;

begin
   Assert((aTexture <> nil) and (aTexture.Handle <> 0));

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      tex2D := GetResource(aTexture.Handle) as ID3D11Texture2D;
      if TTextureStyle.RenderTarget in aTexture.Style then begin

         var textureDesc := Default(TD3D11_TEXTURE2D_DESC);
         textureDesc.Width := aTexture.Width;
         textureDesc.Height := aTexture.Height;
         textureDesc.MipLevels := 1;
         textureDesc.ArraySize := 1;
         textureDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
         textureDesc.SampleDesc.Count := 1;
         textureDesc.Usage := D3D11_USAGE_STAGING;
         textureDesc.CPUAccessFlags := D3D11_CPU_ACCESS_WRITE;
         tex2DBuffer := vDevice.CreateTexture2D(textureDesc, nil);

         mappedRes := vDevice.Map(tex2DBuffer, D3D11_MAP_WRITE);
         try
            CopyToMappedResource;
         finally
            vDevice.Unmap(tex2DBuffer);
         end;

         vDevice.DeviceContext.CopySubresourceRegion(tex2D, 0, 0, 0, 0, tex2DBuffer, 0, nil);

      end else begin

         mappedRes := vDevice.Map(tex2D, D3D11_MAP_WRITE_DISCARD);
         try
            CopyToMappedResource;
         finally
            vDevice.Unmap(tex2D);
         end;

      end;
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoInitializeShader
//
class procedure TFMXUContext3D_DX11.DoInitializeShader(const aShader : TContextShader);
const
   cArchPriority : array [0..3] of TContextShaderArch = (
      TContextShaderArch.DX11, TContextShaderArch.DX10,
      TContextShaderArch.DX11_level_9, TContextShaderArch.DX9
     );
var
   source : TContextShaderSource;
   iSource : IContextShaderSource;
   vertexShader : ID3D11VertexShader;
   pixelShader : ID3D11PixelShader;
begin
   Assert(aShader <> nil);

   for var i := 0 to High(cArchPriority) do begin
      source := aShader.GetSourceByArch(cArchPriority[i]);
      if source.IsDefined then Break;
   end;
   if not source.IsDefined then
      raise EFMXU_DX11Exception.CreateFmt('DoInitializeShader no source for shader "%s"', [ aShader.Name ]);

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      if aShader.Kind = TContextShaderKind.VertexShader then begin
         vertexShader := vDevice.CreateVertexShader(source.Code);
         iSource := TIContextShaderSource.Create(source, cMinShaderVariableSlotSize);
         vertexShader.SetPrivateDataInterface(IContextShaderSource, iSource);
         aShader.Handle := AddResource(vertexShader);
      end else begin
         pixelShader := vDevice.CreatePixelShader(source.Code);
         iSource := TIContextShaderSource.Create(source, cMinShaderVariableSlotSize);
         pixelShader.SetPrivateDataInterface(IContextShaderSource, iSource);
         aShader.Handle := AddResource(pixelShader);
      end;
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoFinalizeShader
//
class procedure TFMXUContext3D_DX11.DoFinalizeShader(const aShader : TContextShader);
begin
   Assert(aShader <> nil);

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      RemoveResource(aShader.Handle);
   finally
      SetExceptionMask(fpuMask);
   end;
   aShader.Handle := 0;
end;

// DoSetScissorRect
//
procedure TFMXUContext3D_DX11.DoSetScissorRect(const aScissorRect : TRect);
var
   scaledRect : TRectF;
begin
   var f := Scale;
   scaledRect.Left := aScissorRect.Left * f;
   scaledRect.Top := aScissorRect.Top * f;
   scaledRect.Right := aScissorRect.Right * f;
   scaledRect.Bottom := aScissorRect.Bottom * f;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      vDevice.DeviceContext.RSSetScissorRects(1, @scaledRect);
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoSetShaders
//
procedure TFMXUContext3D_DX11.DoSetShaders(const aVertexShader, aPixelShader : TContextShader);

   procedure GetIShaderSource(const aShader : ID3D11DeviceChild; out iSource : IContextShaderSource);
   begin
      var dataSize : UINT := SizeOf(iSource);
      aShader.GetPrivateData(IContextShaderSource, dataSize, @iSource);
      Assert(dataSize = SizeOf(iSource));
   end;

   procedure SetLengthAndClear(var buf : TBytes; size : Integer);
   begin
      SetLength(buf, size);
      if size > 0 then
         FillChar(buf[0], size, 0);
   end;

var
   iVertexShader : ID3D11VertexShader;
   iPixelShader : ID3D11PixelShader;
begin
   Assert((aVertexShader <> nil) and (aPixelShader <> nil));

   iVertexShader := GetResource(aVertexShader.Handle) as ID3D11VertexShader;
   iPixelShader := GetResource(aPixelShader.Handle) as ID3D11PixelShader;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      vDevice.DeviceContext.VSSetShader(iVertexShader, nil, 0);
      vDevice.DeviceContext.PSSetShader(iPixelShader, nil, 0);
   finally
      SetExceptionMask(fpuMask);
   end;

   GetIShaderSource(iVertexShader, vVertexShaderSource);
   SetLengthAndClear(vVertexShaderVariables, vVertexShaderSource.GetVariablesSize);
   vVertexShaderModified := True;

   GetIShaderSource(iPixelShader, vPixelShaderSource);
   SetLengthAndClear(vPixelShaderVariables, vPixelShaderSource.GetVariablesSize);
   vPixelShaderModified := True;
end;

// DoSetShaderVariable (array of TVector3D)
//
procedure TFMXUContext3D_DX11.DoSetShaderVariable(const aName : String; const data : array of TVector3D);

   function SetVariable(const iSource : IContextShaderSource; const variables : TBytes) : Boolean;
   begin
      var i := iSource.IndexOfVariable(aName);
      if i >= 0 then begin
         var size : NativeInt := SizeOf(data);
         var vSize : NativeInt := iSource.Size[i];
         if size > vSize then
            size := vSize;
         Move(data[0], variables[iSource.Index[i]], size);
         Result := True;
      end else Result := False;
   end;

begin
   if (CurrentVertexShader <> nil) and SetVariable(vVertexShaderSource, vVertexShaderVariables) then begin
      vVertexShaderModified := True;
      Exit;
   end;
   if (CurrentPixelShader <> nil) and SetVariable(vPixelShaderSource, vPixelShaderVariables) then begin
      vPixelShaderModified := True;
      Exit;
   end;
   raise EFMXU_DX11Exception.CreateFmt('Shader variable "%s" not found', [ aName ]);
end;

// DoSetShaderVariable (TTexture)
//
procedure TFMXUContext3D_DX11.DoSetShaderVariable(const aName : String; const aTexture : TTexture);

   procedure SetShaderResource(const slot : Integer; var tex2D : ID3D11Texture2D; const filter : D3D11_FILTER);
   begin
      var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
      try
         if vDevice.GetSampleDesc(slot).Filter <> filter then begin
            var samplerDesc := Default(TD3D11_SAMPLER_DESC);
            samplerDesc.Filter := filter;
            samplerDesc.AddressU := D3D11_TEXTURE_ADDRESS_CLAMP;
            samplerDesc.AddressV := D3D11_TEXTURE_ADDRESS_CLAMP;
            samplerDesc.AddressW := D3D11_TEXTURE_ADDRESS_CLAMP;
            samplerDesc.MaxAnisotropy := 1;
            samplerDesc.ComparisonFunc := D3D11_COMPARISON_NEVER;
            samplerDesc.MinLOD := -MaxSingle;
            samplerDesc.MaxLOD := MaxSingle;
            vDevice.SetSamplerDesc(slot, samplerDesc);
         end;

         if (aTexture <> nil) and (aTexture.Handle <> 0) then
            tex2D := GetResource(aTexture.Handle) as ID3D11Texture2D
         else tex2D := BlankTexture;

      finally
         SetExceptionMask(fpuMask);
      end;
   end;

   procedure SetVertexShaderResource(const slot : Integer);
   var

      tex2D : ID3D11Texture2D;
   begin
      SetShaderResource(slot, tex2D, D3D11_FILTER_MIN_MAG_MIP_LINEAR);

      vVertexShaderResourceViews[slot] := nil;
      var hr := vDevice.Device.CreateShaderResourceView(tex2D, nil, vVertexShaderResourceViews[slot]);
      RaiseIfFailed(hr, 'CreateShaderResourceView');
      vDevice.DeviceContext.VSSetShaderResources(slot, 1, vVertexShaderResourceViews[slot]);
   end;

   procedure SetPixelShaderResource(const slot : Integer);
   var
      tex2D : ID3D11Texture2D;
   begin
      SetShaderResource(slot, tex2D, D3D11_FILTER_MIN_MAG_MIP_LINEAR);

      vPixelShaderResourceViews[slot] := nil;
      var hr := vDevice.Device.CreateShaderResourceView(tex2D, nil, vPixelShaderResourceViews[slot]);
      RaiseIfFailed(hr, 'CreateShaderResourceView');
      vDevice.DeviceContext.PSSetShaderResources(slot, 1, vPixelShaderResourceViews[slot]);
   end;

begin
   var found := False;

   if CurrentVertexShader <> nil then begin
      var source : TIContextShaderSource := vVertexShaderSource.GetSelf;
      var index := source.IndexOfVariable(aName);
      if index >= 0 then begin
         SetVertexShaderResource(index);
         vVertexShaderModified := True;
         found := True;
      end;
   end;

   if CurrentPixelShader <> nil then begin
      var source : TIContextShaderSource := vPixelShaderSource.GetSelf;
      var index := source.IndexOfVariable(aName);
      if index >= 0 then begin
         SetPixelShaderResource(index);
         vPixelShaderModified := True;
         found := True;
      end;
   end;

   if not found then
      raise EFMXU_DX11Exception.CreateFmt('Shader variable "%s" not found', [ aName ]);
end;

// DoSetShaderVariable (TMatrix3D)
//
procedure TFMXUContext3D_DX11.DoSetShaderVariable(const aName : String; const matrix : TMatrix3D);
begin
   SetShaderVariable(aName, matrix.M);
end;

// DrawGPUPrimitives
//
procedure TFMXUContext3D_DX11.DrawGPUPrimitives(
   const aKind : TPrimitivesKindU;
   const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer
   );
begin
   var vertexSize : UInt := vertices.VertexSize;
   var offset : UInt := 0;
   vDevice.IASetVertexBuffers(
      0, 1, ID3D11Buffer(vertices.GPUBuffer),
      @vertexSize, @offset
   );

   var dxgiFormat := DXGI_FORMAT_R16_UINT;
   if indices.IndexSize = 4 then
      dxgiFormat := DXGI_FORMAT_R32_UINT;
   vDevice.IASetIndexBuffer(ID3D11Buffer(indices.GPUBuffer), dxgiFormat, 0);

   DoDrawPrimitivesBatchU(
      aKind, nil, nil, vertices.VertexDeclarations,
      0, 0, 0, indices.Length
   );

   vDevice.IASetVertexBuffers(0, 1, nil, @vertexSize, @offset);
   vDevice.IASetIndexBuffer(nil, dxgiFormat, 0);
end;

end.
