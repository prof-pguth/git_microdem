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
{    WebGPU context, requires Delphi-WebGPU                            }
{    https://github.com/EricGrange/Delphi-WebGPU                       }
{                                                                      }
{**********************************************************************}
unit FMXU.Context.WebGPU;

{$i fmxu.inc}

interface

(*
   Binding groups :

   0 is vertex uniforms
   1 is fragment textures
   2 is fragment uniforms
*)


uses
   Winapi.Windows,
   WebGPU, WebGPU.Interfaces,
   System.Types, System.UITypes, System.SysUtils, System.Classes, System.Math,
   System.Math.Vectors,
   FMX.Types3D, FMX.Types, FMX.Graphics,
   FMXU.Context, FMXU.Buffers, FMXU.WebGPU.Utils;

type
   TWebGPUSharedBufferType = (
      sbtVertexBuffer, sbtIndexBuffer,
      sbtVertexVariables, sbtPixelVariables
   );
   TWebGPUSharedBuffer = record
      Buffer : IWGPUBuffer;
      Size, Offset : Cardinal;
   end;
   PDX11SharedBuffer = ^TWebGPUSharedBuffer;

   TWebGPUMappedRange = record
      Data : Pointer;
      Size : Cardinal;
   end;

   TIWebGPUContextShaderSource = class (TIContextShaderSource)
      protected
         FSpecialized : TArray<IContextShaderSource>;
         FDeclaration : TVertexDeclaration;

      public
         constructor Create(const aSource : TContextShaderSource;
                            const vertexDeclaration : TVertexDeclaration);

         function SpecializeForVertexDeclaration(const aDeclaration : TVertexDeclaration) : IContextShaderSource; override;
   end;

   TFMXUContext3D_WebGPU = class (TFMXUContext3D)
      protected class var
         vDevice : TWebGPUDevice;

         vBlankTex2D : IWGPUTexture;

         vVertexShaderSource, vPixelShaderSource : IContextShaderSource;

         // FMX limited to 16 instead of D3D11_COMMONSHADER_INPUT_RESOURCE_SLOT_COUNT ?
         //vPixelShaderResourceViews : array [ 0..15 ] of ID3D11ShaderResourceView;

         vSharedBuffers : array [ TWebGPUSharedBufferType ] of TWebGPUSharedBuffer;
         vSharedIndexBufferFormat : TWGPUIndexFormat;

         class procedure ReleasePrivateVars;

      protected
         FPipeline : TWebGPUPipelineManager;

         FRenderPass : IWGPURenderPassEncoder;
         FCommandEncoder : IWGPUCommandEncoder;

         FViewPort : TWebGPUViewPort;
         FScissorRect : TRect;
         FScissorOn : Boolean;

         FTextureBufferPitch : Cardinal;
         FTextureBuffer : IWGPUBuffer;
         FTextureBufferMap : IWebGPUMappedBuffer;

         FRenderTexture: IWGPUTexture;
         FRenderSurface : IWGPUSurface;
         FRenderTargetView : IWGPUTextureView;
         FMultiSampleTexture : IWGPUTexture;
         FMultiSampleView : IWGPUTextureView;

         FDepthStencilTex2D : IWGPUTexture;
         FDepthStencilView : IWGPUTextureView;

         FCopyToBitsTex2D : IWGPUTexture;

         FVSync : Boolean;

         // shared buffers methods

         procedure EnsureSharedBufferSize(typ : TWebGPUSharedBufferType; minByteSize : Cardinal);
         procedure OffsetSharedBuffer(typ : TWebGPUSharedBufferType; dataSize : Cardinal);

         procedure SetSharedVertexBuffer(vertexSize : Cardinal; data : Pointer; dataSize : Cardinal);
         procedure SetSharedIndexBuffer(indexSize : Cardinal; data : Pointer; dataSize : Cardinal);

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
         procedure ApplyScissorRect;

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
         class property Device : TWebGPUDevice read vDevice;
         class function WebGPUSupported  : Boolean;

         class function BlankTexture : IWGPUTexture;

         property RenderTargetView : IWGPUTextureView read FRenderTargetView;
         property PipelineManager : TWebGPUPipelineManager read FPipeline;

         property CommandEncoder : IWGPUCommandEncoder read FCommandEncoder;
         property RenderPassEncoder : IWGPURenderPassEncoder read FRenderPass;

         procedure EndRenderPassEncoder;
         procedure BeginRenderPassEncoder(
            const aTarget : TClearTargets; const aColor : TAlphaColor;
            const aDepth : Single; const aStencil : Cardinal
         );

         class function CreateBuffer(
            dataSize : Cardinal; usage : TWGPUBufferUsage;
            const name : UTF8String = ''
            ) : IWGPUBuffer;
         class function CreateBufferFromData(
            dataSize : Cardinal; usage : TWGPUBufferUsage;
            dataPointer : Pointer; const name : UTF8String = ''
            ) : IWGPUBuffer;

         class function Valid : Boolean; override;
         class function PixelFormat : TPixelFormat; override;
         class function MaxLightCount : Integer; override;
         class function MaxTextureSize : Integer; override;

         procedure DrawGPUPrimitives(
            const aKind : TPrimitivesKindU;
            const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer
         ); override;

         // VSync control, only for window target, default false (FMX default)
         property VSync : Boolean read FVSync write FVSync;
   end;

procedure RegisterWebGPUContext(const webGPU_DLL_PathName : String = 'webgpu_dawn.dll');
procedure UnregisterWebGPUContext;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   FMX.Platform.Win, FMX.Platform, FMX.Canvas.GPU, FMX.Utils,
   FMXU.WebGPU.Materials, FMXU.Buffers.WebGPU;

{$R-}

const
   cRGB32White : UINT32 = $FFFFFFFF;
   cMinShaderVariableSlotSize = 4*SizeOf(Single);

// RegisterWebGPUContext
//
procedure RegisterWebGPUContext(const webGPU_DLL_PathName : String = 'webgpu_dawn.dll');
begin
   Load_webgpu_Library(webGPU_DLL_PathName);
   if TFMXUContext3D_WebGPU.WebGPUSupported then begin
      TContextManager.RegisterContext(TFMXUContext3D_WebGPU, True);
      TGPUVertexBuffer.RegisterGPUVertexBufferClass(TGPUVertexBufferWebGPU);
      TGPUIndexBuffer.RegisterGPUIndexBufferClass(TGPUIndexBufferWebGPU);
   end;
end;

// UnregisterWebGPUContext
//
procedure UnregisterWebGPUContext;
begin
   TFMXUContext3D_WebGPU.ReleasePrivateVars;
end;

// ------------------
// ------------------ TFMXUContext3D_WebGPU ------------------
// ------------------

// ReleasePrivateVars
//
class procedure TFMXUContext3D_WebGPU.ReleasePrivateVars;
begin
   for var i := Low(vSharedBuffers) to High(vSharedBuffers) do
      vSharedBuffers[i] := Default(TWebGPUSharedBuffer);

   vVertexShaderSource := nil;
   vPixelShaderSource := nil;

   vBlankTex2D := nil;

   FreeAndNil(TFMXUContext3D_WebGPU.vDevice);
end;

// WebGPUSupported
//
class function TFMXUContext3D_WebGPU.WebGPUSupported  : Boolean;
begin
   if vDevice = nil then
      vDevice := TWebGPUDevice.Create;
   Result := True;
end;

// BlankTexture
//
class function TFMXUContext3D_WebGPU.BlankTexture : IWGPUTexture;

   procedure CreateBlankTexture;
   begin
      vBlankTex2D := vDevice.CreateTexture2D_BGRA32(1, 1, 'BlankTexture');
      vDevice.WriteToTexture2D_BGRA32(vBlankTex2D, 1, 1, 0, @cRGB32White);
   end;

begin
   if vBlankTex2D = nil then
      CreateBlankTexture;
   Result := vBlankTex2D;
end;

// CreateBuffer
//
class function TFMXUContext3D_WebGPU.CreateBuffer(dataSize : Cardinal; usage : TWGPUBufferUsage; const name : UTF8String = '') : IWGPUBuffer;
begin
   Result := vDevice.CreateBuffer(dataSize, usage, name);
end;

// CreateBufferFromData
//
class function TFMXUContext3D_WebGPU.CreateBufferFromData(dataSize : Cardinal; usage : TWGPUBufferUsage; dataPointer : Pointer; const name : UTF8String = '') : IWGPUBuffer;
begin
   Result := vDevice.CreateBufferFromData(dataSize, usage, dataPointer, name);
end;

// GetIndexBufferSupport
//
function TFMXUContext3D_WebGPU.GetIndexBufferSupport: TContext3D.TIndexBufferSupport;
begin
   Result := TIndexBufferSupport.Int32;
end;

// MaxTextureSize
//
class function TFMXUContext3D_WebGPU.MaxTextureSize: Integer;
begin
   Result := vDevice.MaxTextureSize;
end;

// CreateFromWindow
//
constructor TFMXUContext3D_WebGPU.CreateFromWindow(
   const aParent : TWindowHandle; const aWidth, aHeight : Integer;
   const aMultisample : TMultisample; const aDepthStencil : Boolean
   );
begin
   inherited;
   FPipeline := TWebGPUPipelineManager.Create(vDevice);
   inherited CreateBuffer;
end;

// CreateFromTexture
//
constructor TFMXUContext3D_WebGPU.CreateFromTexture(
   const aTexture : TTexture; const aMultisample : TMultisample;
   const aDepthStencil : Boolean
   );
begin
   inherited;
   FPipeline := TWebGPUPipelineManager.Create(vDevice);
   inherited CreateBuffer;
end;

// Valid
//
class function TFMXUContext3D_WebGPU.Valid : Boolean;
begin
   Result := (vDevice <> nil);
end;

// PixelFormat
//
class function TFMXUContext3D_WebGPU.PixelFormat: TPixelFormat;
begin
   Result := TPixelFormat.BGRA;
end;

// MaxLightCount
//
class function TFMXUContext3D_WebGPU.MaxLightCount : Integer;
begin
   Result := 8;
end;

// DoCreateBuffer
//
procedure TFMXUContext3D_WebGPU.DoCreateBuffer;
var
   width, height, sampleCount : Integer;

   procedure CreateTextureRenderTarget;
   begin
      Assert(Self.Width = Texture.Width);
      Assert(Self.Height = Texture.Height);
      width := Texture.Width;
      height := Texture.Height;
      FRenderTexture := vDevice.CreateTexture2D_BGRA32(
         width, height,
         WGPUTextureUsage_RenderAttachment or WGPUTextureUsage_TextureBinding or WGPUTextureUsage_CopySrc,
         'TextureRenderTarget'
      );
      FRenderTargetView := vDevice.CreateBaseTextureView(
         FRenderTexture, WGPUTextureAspect_All,
         'TextureRenderTargetView'
      );
      Assert(FRenderTargetView <> nil);
   end;

   procedure CreateWindowRenderTarget;
   begin
      var winInfo := WindowHandleToPlatform(Parent);

      if FRenderSurface = nil then begin
         var fromWindowsHWND := Default(TWGPUSurfaceSourceWindowsHWND);
         fromWindowsHWND.chain.next := nil;
         fromWindowsHWND.chain.sType := WGPUSType_SurfaceSourceWindowsHWND;
         fromWindowsHWND.hinstance := Pointer(HInstance);
         fromWindowsHWND.hwnd := Pointer(winInfo.Wnd);

         var surfaceDescriptor := Default(TWGPUSurfaceDescriptor);
         surfaceDescriptor.&label := '';
         surfaceDescriptor.nextInChain := @fromWindowsHWND;

         FRenderSurface := vDevice.Instance.CreateSurface(surfaceDescriptor);
         Assert(FRenderSurface <> nil);
      end;

      width := Trunc(winInfo.WndClientSize.cx);
      height := Trunc(winInfo.WndClientSize.cy);

      var surfaceConfiguration := Default(TWGPUSurfaceConfiguration);
      surfaceConfiguration.nextInChain := nil;
      surfaceConfiguration.device := vDevice.Device.GetHandle;
      surfaceConfiguration.format := WGPUTextureFormat_BGRA8Unorm;
      surfaceConfiguration.usage := WGPUTextureUsage_RenderAttachment;
      surfaceConfiguration.alphaMode := WGPUCompositeAlphaMode_Auto;
      surfaceConfiguration.viewFormats := nil;
      surfaceConfiguration.viewFormatCount := 0;
      surfaceConfiguration.width := width;
      surfaceConfiguration.height := height;
      if VSync then
         surfaceConfiguration.presentMode := WGPUPresentMode_Fifo
      else surfaceConfiguration.presentMode := WGPUPresentMode_Immediate;

      FRenderSurface.Configure(surfaceConfiguration);
   end;

begin
   sampleCount := MultisampleToSampleCount(Multisample);

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      if Texture <> nil then
         CreateTextureRenderTarget
      else CreateWindowRenderTarget;
   finally
      SetExceptionMask(fpuMask);
   end;

   FPipeline.SetSampleCount(sampleCount);

   // note that the rest of the attachment planes are lazy initialized
   // in BeginScene, this is because at startup DoCreateBuffer is called
   // multiple times with bogus values
end;

// DoFreeBuffer
//
procedure TFMXUContext3D_WebGPU.DoFreeBuffer;
begin
   FPipeline.Free;
   FPipeline := TWebGPUPipelineManager.Create(vDevice);

   FRenderPass := nil;
   FCommandEncoder := nil;

   FRenderTexture := nil;
   FRenderTargetView := nil;
   FtextureBufferMap := nil;
   FTextureBuffer := nil;
   FMultiSampleTexture := nil;
   FMultiSampleView := nil;

   FDepthStencilTex2D := nil;
   FDepthStencilView := nil;

   FCopyToBitsTex2D := nil;
end;

// DoResize
//
procedure TFMXUContext3D_WebGPU.DoResize;
begin
   // nothing
end;

// DoBeginScene
//
function TFMXUContext3D_WebGPU.DoBeginScene : Boolean;

   procedure InitializeAttachmentPlanes;
   var
      w, h : Cardinal;
   begin
      var sampleCount := MultisampleToSampleCount(Multisample);
      if Texture <> nil then begin
         // no scaling for texture render target
         w := Width;
         h := Height;
      end else begin
         w := Trunc(Width * Scale);
         h := Trunc(Height * Scale);
      end;

      if (sampleCount > 1) and (FMultiSampleTexture = nil)  then begin
         var format : TWGPUTextureFormat;
         if FRenderSurface <> nil then
            format := FRenderSurface.GetCurrentTexture.GetFormat
         else format := FRenderTexture.GetFormat;
         FMultiSampleTexture := vDevice.CreateMultiSample(w, h, format, sampleCount);
         FMultiSampleView := vDevice.CreateBaseTextureView(FMultiSampleTexture, WGPUTextureAspect_All);
         Assert(FMultiSampleView <> nil);
      end;
      FDepthStencilTex2D := vDevice.CreateDepthStencilTexture(w, h, sampleCount);
      FDepthStencilView := vDevice.CreateBaseTextureView(FDepthStencilTex2D, WGPUTextureAspect_DepthOnly);

      if Texture <> nil then begin
         // prepare the BGRA buffer that will be use to update the Texture
         // align buffer pitch to multiple of 256
         FTextureBufferPitch := w * 4;
         if (FTextureBufferPitch and $FF) <> 0 then
            FTextureBufferPitch := (FTextureBufferPitch and not $FF) + $100;
         FTextureBuffer := vDevice.CreateBuffer(
            FTextureBufferPitch * h, WGPUBufferUsage_MapRead or WGPUBufferUsage_CopyDst
         );
      end;
   end;

begin
   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      // lazy initialization of attachment planes (see DoCreateBuffer)
      if FDepthStencilTex2D = nil then
         InitializeAttachmentPlanes;

      if FRenderSurface <> nil then begin
         // set to the current target in the swap chain
         FRenderTargetView := vDevice.CreateBaseTextureView(
            FRenderSurface.GetCurrentTexture, WGPUTextureAspect_All,
            'SurfaceTargetView'
         );
         Assert(FRenderTargetView <> nil);
      end else begin
         FTextureBufferMap := nil;
      end;

      var commandEncoderDescriptor := Default(TWGPUCommandEncoderDescriptor);
      commandEncoderDescriptor.&label := 'CommandEncoder';
      FCommandEncoder := vDevice.Device.CreateCommandEncoder(commandEncoderDescriptor);

      // FMX texture rendering target doesn't honour Scale
      var f := Scale;
      if Texture <> nil then
         f := 1;

      FViewPort.X := 0;
      FViewPort.Y := 0;
      FViewPort.Width := Trunc(Width * f);
      FViewPort.Height := Trunc(Height * f);
      FViewPort.MinDepth := 0;
      FViewPort.MaxDepth := 1;

      Result := inherited;
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoClear
//
procedure TFMXUContext3D_WebGPU.DoClear(
   const aTarget : TClearTargets; const aColor : TAlphaColor;
   const aDepth : Single; const aStencil : Cardinal
   );
begin
   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      EndRenderPassEncoder;

      BeginRenderPassEncoder(aTarget, aColor, aDepth, aStencil);
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// EndRenderPassEncoder
//
procedure TFMXUContext3D_WebGPU.EndRenderPassEncoder;
begin
   if FRenderPass <> nil then begin
      FRenderPass.&End;
      FRenderPass := nil;
   end;
end;

// BeginRenderPassEncoder
//
procedure TFMXUContext3D_WebGPU.BeginRenderPassEncoder(
   const aTarget : TClearTargets; const aColor : TAlphaColor;
   const aDepth : Single; const aStencil : Cardinal
   );
begin
   Assert(FRenderPass = nil);

   // TODO: stencil flags

   var renderPassColorAttachment := Default(TWGPURenderPassColorAttachment);
   if FMultiSampleTexture <> nil then begin
      // multisample rendering to a surface
      renderPassColorAttachment.view := FMultiSampleView.GetHandle;
      renderPassColorAttachment.resolveTarget := FRenderTargetView.GetHandle
   end else begin
      // no multisample
      renderPassColorAttachment.view := FRenderTargetView.GetHandle;
   end;

   if TClearTarget.Color in aTarget then
      renderPassColorAttachment.loadOp := WGPULoadOp_Clear
   else renderPassColorAttachment.loadOp := WGPULoadOp_Load;
   renderPassColorAttachment.storeOp := WGPUStoreOp_Store;
   renderPassColorAttachment.clearValue := ColorToWGPUColor(aColor);
   renderPassColorAttachment.depthSlice := WGPU_DEPTH_SLICE_UNDEFINED;

   var depthStencilAttachment := Default(TWGPURenderPassDepthStencilAttachment);
   depthStencilAttachment.view := FDepthStencilView.GetHandle;
   depthStencilAttachment.depthClearValue := aDepth;
   if TClearTarget.Depth in aTarget then
      depthStencilAttachment.depthLoadOp := WGPULoadOp_Clear
   else depthStencilAttachment.depthLoadOp := WGPULoadOp_Load;
   depthStencilAttachment.depthStoreOp := WGPUStoreOp_Store;
//   if TClearTarget.Stencil in aTarget then
//      depthStencilAttachment.stencilLoadOp := WGPULoadOp_Clear;
//   else depthStencilAttachment.stencilLoadOp := WGPULoadOp_Load;
//   depthStencilAttachment.stencilStoreOp := WGPUStoreOp_Store;
//   depthStencilAttachment.stencilClearValue := aStencil;
   depthStencilAttachment.stencilReadOnly := 1;

   var renderPassDescriptor := Default(TWGPURenderPassDescriptor);
   renderPassDescriptor.colorAttachmentCount := 1;
   renderPassDescriptor.colorAttachments := @renderPassColorAttachment;
   renderPassDescriptor.depthStencilAttachment := @depthStencilAttachment;

   FRenderPass := FCommandEncoder.BeginRenderPass(renderPassDescriptor);

   FRenderPass.SetViewport(
      FViewPort.X, FViewPort.Y, FViewPort.Width, FViewPort.Height,
      FViewPort.MinDepth, FViewPort.MaxDepth
   );
   ApplyScissorRect;
end;


// DoEndScene
//
procedure TFMXUContext3D_WebGPU.DoEndScene;
begin
   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      if FRenderPass <> nil then begin
         FRenderPass.&End;
         FRenderPass := nil;
      end;

      if FTextureBuffer <> nil then begin
         var imageCopyBuffer := Default(TWGPUImageCopyBuffer);
         imageCopyBuffer.layout.bytesPerRow := FTextureBufferPitch;
         imageCopyBuffer.layout.rowsPerImage := Height;
         imageCopyBuffer.buffer := FTextureBuffer.GetHandle;

         var imageCopyTexture := Default(TWGPUImageCopyTexture);
         imageCopyTexture.texture := FRenderTexture.GetHandle;
         imageCopyTexture.aspect := WGPUTextureAspect_All;

         var copySize := Default(TWGPUExtent3D);
         copySize.width := Width;
         copySize.height := Height;
         copySize.depthOrArrayLayers := 1;
         FCommandEncoder.CopyTextureToBuffer(@imageCopyTexture, @imageCopyBuffer, @copySize);
      end;

      var commandBuffer := FCommandEncoder.Finish(nil);
      vDevice.Queue.Submit(commandBuffer);

      if FRenderSurface <> nil then
         FRenderSurface.Present;

      if FTextureBuffer <> nil then begin
         FTextureBufferMap := TIWebGPUMappedBuffer.Create(
            FTextureBuffer, WGPUMapMode_Read, 0,
            FTextureBufferPitch * Cardinal(Height)
         );
      end;

      vDevice.Instance.ProcessEvents;
   finally
      SetExceptionMask(fpuMask);
   end;
   inherited;
end;

// DoCopyToBitmap
//
procedure BufferMappedCallback(status: TWGPUMapAsyncStatus; userdata: Pointer); cdecl;
begin
   PWGPUMapAsyncStatus(userdata)^ := status;
end;
procedure TFMXUContext3D_WebGPU.DoCopyToBitmap(const aDest : FMX.Graphics.TBitmap; const aRect : TRect);
begin
   Assert(aDest.PixelFormat = TPixelFormat.BGRA);
   var bitmapData := Default(TBitmapData);
   if aDest.Map(TMapAccess.Write, bitmapData) then try

      DoCopyToBits(
         bitmapData.Data, bitmapData.Pitch,
         Rect(0, 0, aDest.Width, aDest.Height)
      );

   finally
      aDest.Unmap(bitmapData);
   end else inherited;
end;

// DoCopyToBits
//
procedure TFMXUContext3D_WebGPU.DoCopyToBits(const bits: Pointer; const pitchInBytes : Integer; const aRect: TRect);
begin
   Assert((aRect.Left = 0) and (aRect.Top = 0) and (aRect.Width = Width) and (aRect.Height = Height));

   var bufferSize := FTextureBufferPitch * Cardinal(Height);
   var mappedMemory := FTextureBufferMap.Data;

   if FTextureBufferPitch = Cardinal(pitchInBytes) then
      Move(mappedMemory^, bits^, bufferSize)
   else begin
      Assert(Cardinal(pitchInBytes) < FTextureBufferPitch);
      var src : PByte := mappedMemory;
      var dest : PByte := bits;
      for var i := 1 to Height do begin
         Move(src^, dest^, pitchInBytes);
         Inc(src, FTextureBufferPitch);
         Inc(dest, pitchInBytes);
      end;
   end;
end;

// DoBitmapToTexture
//
class function TFMXUContext3D_WebGPU.DoBitmapToTexture(const aBitmap : TBitmap) : TTexture;
begin
   if aBitmap.CanvasClass.InheritsFrom(TCustomCanvasGpu) then
      Result := TBitmapCtx(aBitmap.Handle).PaintingTexture
   else Result := inherited DoBitmapToTexture(aBitmap);
end;

// DoSetContextState
//
procedure TFMXUContext3D_WebGPU.DoSetContextState(aState: TContextState);
begin
   case aState of
      TContextState.csScissorOn : begin
         FScissorOn := True;
         ApplyScissorRect;
      end;
      TContextState.csScissorOff : begin
         FScissorOn := False;
         ApplyScissorRect;
      end;
   else
      FPipeline.SetContextState(aState);
   end;
end;

// DoSetStencilOp
//
procedure TFMXUContext3D_WebGPU.DoSetStencilOp(const aFail, aZFail, aZPass: TStencilOp);
begin
   FPipeline.SetStencilOp(aFail, aZFail, aZPass);
end;

// DoSetStencilFunc
//
procedure TFMXUContext3D_WebGPU.DoSetStencilFunc(const aFunc : TStencilfunc; aRef, aMask : Cardinal);
begin
   FPipeline.SetStencilFunc(aFunc, aRef, aMask);
end;

// DoDrawPrimitivesBatch
//
procedure TFMXUContext3D_WebGPU.DoDrawPrimitivesBatch(
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
procedure TFMXUContext3D_WebGPU.DoDrawPrimitivesBatchU(
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
      FPipeline.SetVertexDeclaration(vertexDeclaration, vertexSize);

      if vertices <> nil then
         SetSharedVertexBuffer(vertexSize, vertices, vertexBufferByteSize);
      if indices <> nil then
         SetSharedIndexBuffer(indexSize, indices, indexBufferByteSize);

      FPipeline.SetBindGroups(FRenderPass);

      FPipeline.SetupRenderPipeline(aKind, vSharedIndexBufferFormat);

      FRenderPass.SetPipeline(FPipeline.RenderPipeline);

      FRenderPass.DrawIndexed(indexCount, 1, 0, 0, 0);

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
procedure TFMXUContext3D_WebGPU.EnsureSharedBufferSize(typ : TWebGPUSharedBufferType; minByteSize : Cardinal);
begin
   if vSharedBuffers[typ].Offset + minByteSize < vSharedBuffers[typ].Size then
      Exit;

   if minByteSize < 64*1024 then
      minByteSize := 64*1024;

   vSharedBuffers[typ].Size := 0;
   vSharedBuffers[typ].Offset := 0;
   vSharedBuffers[typ].Buffer := nil;

   var usage : TWGPUBufferUsage;
   case typ of
      sbtVertexBuffer:
         usage := WGPUBufferUsage_Vertex or WGPUBufferUsage_CopyDst;
      sbtIndexBuffer:
         usage := WGPUBufferUsage_Index or WGPUBufferUsage_CopyDst;
      sbtVertexVariables, sbtPixelVariables:
         usage := WGPUBufferUsage_Uniform or WGPUBufferUsage_CopyDst;
   else
      usage := 0;
      Assert(False);
   end;

   vSharedBuffers[typ].Buffer := CreateBuffer(minByteSize, usage);
   vSharedBuffers[typ].Size := minByteSize;
end;

// OffsetSharedBuffer
//
procedure TFMXUContext3D_WebGPU.OffsetSharedBuffer(typ : TWebGPUSharedBufferType; dataSize : Cardinal);
begin
   Inc(vSharedBuffers[typ].Offset, dataSize);
end;

// SetSharedVertexBuffer
//
procedure TFMXUContext3D_WebGPU.SetSharedVertexBuffer(vertexSize : Cardinal; data : Pointer; dataSize : Cardinal);
begin
   EnsureSharedBufferSize(sbtVertexBuffer, dataSize);
   vDevice.Queue.WriteBuffer(
      vSharedBuffers[sbtVertexBuffer].Buffer,
      vSharedBuffers[sbtVertexBuffer].Offset,
      data, dataSize
   );
   FRenderPass.SetVertexBuffer(
      0, vSharedBuffers[sbtVertexBuffer].Buffer,
      vSharedBuffers[sbtVertexBuffer].Offset, dataSize
   );
end;

// SetSharedIndexBuffer
//
procedure TFMXUContext3D_WebGPU.SetSharedIndexBuffer(indexSize : Cardinal; data : Pointer; dataSize : Cardinal);
begin
   EnsureSharedBufferSize(sbtIndexBuffer, dataSize);
   case indexSize of
      2 : vSharedIndexBufferFormat := WGPUIndexFormat_Uint16;
      4 : vSharedIndexBufferFormat := WGPUIndexFormat_Uint32;
   else
      Assert(False);
   end;
   vDevice.Queue.WriteBuffer(
      vSharedBuffers[sbtIndexBuffer].Buffer,
      vSharedBuffers[sbtIndexBuffer].Offset,
      data, dataSize
   );
   FRenderPass.SetIndexBuffer(
      vSharedBuffers[sbtIndexBuffer].Buffer, vSharedIndexBufferFormat,
      vSharedBuffers[sbtIndexBuffer].Offset, dataSize
   );
end;

// DoInitializeTexture
//
class procedure TFMXUContext3D_WebGPU.DoInitializeTexture(const aTexture : TTexture);
var
   tex2D : IWGPUTexture;
begin
   Assert(aTexture.Handle = 0);

   // TTexture is created with None, assume BGRA for FMX compatibility
   if aTexture.PixelFormat = TPixelFormat.None then
      aTexture.PixelFormat := TPixelFormat.BGRA;

   // local vars to reduce verbosity of the scoped enums 'in' tests
//   var isDynamic := (TTextureStyle.Dynamic in aTexture.Style);
//   var isRenderTarget := (TTextureStyle.RenderTarget in aTexture.Style);
//   var useMipMaps := (TTextureStyle.MipMaps in aTexture.Style);

   var textureDescriptor := Default(TWGPUTextureDescriptor);
   textureDescriptor.usage := WGPUTextureUsage_TextureBinding or WGPUTextureUsage_CopyDst;
   textureDescriptor.dimension := WGPUTextureDimension_2D;
   textureDescriptor.size.width := aTexture.Width;
   textureDescriptor.size.height := aTexture.Height;
   textureDescriptor.size.depthOrArrayLayers := 1;
   textureDescriptor.format := PixelFormatToWebGPUFormat(aTexture.PixelFormat);
   textureDescriptor.mipLevelCount := 1;
   textureDescriptor.sampleCount := 1;
   tex2D := vDevice.Device.CreateTexture(textureDescriptor);
   Assert(tex2D <> nil);

   ITextureAccess(aTexture).Handle := AddResource(tex2D);
end;

// DoFinalizeTexture
//
class procedure TFMXUContext3D_WebGPU.DoFinalizeTexture(const aTexture : TTexture);
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
class procedure TFMXUContext3D_WebGPU.DoUpdateTexture(
   const aTexture: TTexture;
   const data : Pointer; const rowPitch : Integer
   );
begin
   Assert((aTexture <> nil) and (aTexture.Handle <> 0));

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      var tex2D := GetResource(aTexture.Handle) as IWGPUTexture;

      var extent := Default(TWGPUExtent3D);
      extent.width := aTexture.Width;
      extent.height := aTexture.Height;
      extent.depthOrArrayLayers := 1;

      var destination := Default(TWGPUImageCopyTexture);
      destination.texture := tex2D.GetHandle;
      destination.mipLevel := 0;
      destination.aspect := WGPUTextureAspect_All;

      var source := Default(TWGPUTextureDataLayout);
      source.bytesPerRow := SizeOf(TColorRec) * extent.width;
      source.rowsPerImage := extent.height;

      vDevice.Queue.WriteTexture(
         @destination, data, aTexture.Height * rowPitch,
         @source, @extent
      );
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoInitializeShader
//
class procedure TFMXUContext3D_WebGPU.DoInitializeShader(const aShader : TContextShader);
var
   source : TContextShaderSource;
   iSource : IContextShaderSource;
   shader : IWebGPUCompiledShader;
begin
   Assert(aShader <> nil);

   source := aShader.GetSourceByArch(TContextShaderArch_WGSL);
   if source.IsDefined then begin
      iSource := TIContextShaderSource.Create(source, cMinShaderVariableSlotSize);
   end else begin
      iSource := GetWebGPUCompatibleShaderSource(aShader);
      if iSource = nil then
         raise EFMXU_WebGPUException.CreateFmt('DoInitializeShader no source for shader "%s"', [ aShader.Name ]);
   end;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      shader := TWebGPUCompiledShader.Create(aShader.Kind, iSource, vDevice, True);
      aShader.Handle := AddResource(shader);
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoFinalizeShader
//
class procedure TFMXUContext3D_WebGPU.DoFinalizeShader(const aShader : TContextShader);
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
procedure TFMXUContext3D_WebGPU.DoSetScissorRect(const aScissorRect : TRect);
begin
   FScissorRect := aScissorRect;
   if FScissorOn and (FRenderPass <> nil)  then begin
      var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
      try
         FRenderPass.SetScissorRect(
            FScissorRect.Left, FScissorRect.Top,
            FScissorRect.Width, FScissorRect.Height
         );
      finally
         SetExceptionMask(fpuMask);
      end;
   end;
end;

// ApplyScissorRect
//
procedure TFMXUContext3D_WebGPU.ApplyScissorRect;
begin
   if FRenderPass = nil then Exit;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      if FScissorOn then begin
         FRenderPass.SetScissorRect(
            FScissorRect.Left, FScissorRect.Top,
            FScissorRect.Width, FScissorRect.Height
         );
      end else begin
         FRenderPass.SetScissorRect(
            0, 0, Trunc(FViewPort.Width), Trunc(FViewPort.Height)
         );
      end;
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoSetShaders
//
procedure TFMXUContext3D_WebGPU.DoSetShaders(const aVertexShader, aPixelShader : TContextShader);
var
   iVertexShader : IWebGPUCompiledShader;
   iFragmentShader : IWebGPUCompiledShader;
begin
   Assert((aVertexShader <> nil) and (aPixelShader <> nil));

   iVertexShader := GetResource(aVertexShader.Handle) as IWebGPUCompiledShader;
   iFragmentShader := GetResource(aPixelShader.Handle) as IWebGPUCompiledShader;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      FPipeline.Setup(iVertexShader, iFragmentShader);
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoSetShaderVariable
//
procedure TFMXUContext3D_WebGPU.DoSetShaderVariable(const aName : String; const data : array of TVector3D);
begin
   FPipeline.SetShaderVariable(aName, data);
end;

// DoSetShaderVariable
//
procedure TFMXUContext3D_WebGPU.DoSetShaderVariable(const aName : String; const aTexture : TTexture);
var
   tex2D : IWGPUTexture;
begin
   if CurrentPixelShader = nil then Exit;

   var fpuMask := SetExceptionMask(exAllArithmeticExceptions);
   try
      if (aTexture <> nil) and (aTexture.Handle <> 0) then
         tex2D := GetResource(aTexture.Handle) as IWGPUTexture
      else tex2D := BlankTexture;

      FPipeline.SetTextureVariable(aName, tex2D);
   finally
      SetExceptionMask(fpuMask);
   end;
end;

// DoSetShaderVariable
//
procedure TFMXUContext3D_WebGPU.DoSetShaderVariable(const aName : String; const matrix : TMatrix3D);
begin
   SetShaderVariable(aName, matrix.M);
end;

// DrawGPUPrimitives
//
procedure TFMXUContext3D_WebGPU.DrawGPUPrimitives(
   const aKind : TPrimitivesKindU;
   const vertices : TGPUVertexBuffer; const indices : TGPUIndexBuffer
   );
begin
   FRenderPass.SetVertexBuffer(
      0, IWGPUBuffer(vertices.GPUBuffer),
      0, vertices.Size
   );

   case indices.IndexSize of
      2 : vSharedIndexBufferFormat := WGPUIndexFormat_Uint16;
      4 : vSharedIndexBufferFormat := WGPUIndexFormat_Uint32;
   else
      Assert(False);
   end;

   FRenderPass.SetIndexBuffer(
      IWGPUBuffer(indices.GPUBuffer), vSharedIndexBufferFormat,
      0, indices.Size
   );

   DoDrawPrimitivesBatchU(
      aKind, nil, nil, vertices.VertexDeclarations,
      vertices.VertexSize, 0, 0, indices.Length
   );
end;

// ------------------
// ------------------ TIWebGPUContextShaderSource ------------------
// ------------------

// Create
//
constructor TIWebGPUContextShaderSource.Create(const aSource : TContextShaderSource;
                                               const vertexDeclaration : TVertexDeclaration);
begin
   inherited Create(aSource, cMinShaderVariableSlotSize);
   FDeclaration := Copy(vertexDeclaration);
end;

// SpecializeForVertexDeclaration
//
function TIWebGPUContextShaderSource.SpecializeForVertexDeclaration(const aDeclaration : TVertexDeclaration) : IContextShaderSource;
begin
   if SameVertexDeclaration(FDeclaration, aDeclaration) then
      Exit(Self);

   for var i := 0 to High(FSpecialized) do begin
      var s := TIWebGPUContextShaderSource(FSpecialized[i].GetSelf);
      if SameVertexDeclaration(s.FDeclaration, aDeclaration) then
         Exit(FSpecialized[i]);
   end;

   var code := TEncoding.UTF8.GetString(Self.Code);

   Result := TIWebGPUContextShaderSource.Create(TContextShaderSource.Create(
      TContextShaderArch_WGSL,
      TEncoding.UTF8.GetBytes(
         StringReplace(code, '<#VertexDeclaration#>', VertexDeclarationToWGSLVertexFields(aDeclaration), [ ])
      ),
      Variables
   ), aDeclaration);

   var n := Length(FSpecialized);
   SetLength(FSpecialized, n+1);
   FSpecialized[n] := Result;
end;

end.
