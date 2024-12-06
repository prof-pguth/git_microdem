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
{    Support for compile DirectX HLSL shaders at runtime               }
{                                                                      }
{**********************************************************************}
unit FMXU.D3DShaderCompiler;

{$i fmxu.inc}

interface

uses
   System.SysUtils;

type
   ED3DShaderCompilerException = class (Exception);

(*
   Compile a pixel or fragment shader using D2DCompile2
   entryPoint specifies the main function name (e.g. "main")
   target specifies the shader model (e.g. "ps_5_0")
   returns compiled shader or raises an exception
*)
function CompileShaderFromSource(const shaderSource, entryPoint, target : AnsiString) : TBytes;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$ifdef MSWINDOWS}
uses Winapi.D3DCompiler, Winapi.D3DCommon, Winapi.Windows;

// CompileShaderFromSource
//
function CompileShaderFromSource(const shaderSource, entryPoint, target : AnsiString) : TBytes;
var
   errorBlob, shaderBlob : ID3DBlob;
   flags : UINT;
begin
   flags := D3DCOMPILE_ENABLE_STRICTNESS;

   {$IFDEF DEBUG}
   flags := flags or D3DCOMPILE_DEBUG;
   {$ENDIF}

   var err := D3DCompile2(
      PAnsiChar(shaderSource), // Shader source code
      Length(shaderSource),    // Length of the shader source
      nil,                     // Optional source name
      nil,                     // Optional macros
      nil,                     // Optional include handler
      PAnsiChar(entryPoint),   // Entry point function name
      PAnsiChar(target),       // Shader model (e.g., "ps_5_0" for pixel shader)
      flags,                   // Compile flags
      0,                       // Effect compile flags (not used for shaders)
      0,                       // Secondary data flags
      nil,                     // Secondary data
      0,                       // Secondary data size
      shaderBlob,              // Compiled shader
      errorBlob                // Error messages
   );

   if (err <> 0) or (shaderBlob = nil) then begin
      if errorBlob <> nil then
         raise ED3DShaderCompilerException.Create(String(PAnsiChar(errorBlob.GetBufferPointer)))
      else raise ED3DShaderCompilerException.CreateFmt('Failed with code %d', [ err ]);
   end;

   var n := shaderBlob.GetBufferSize;
   SetLength(Result, n);
   System.Move(shaderBlob.GetBufferPointer^, Pointer(Result)^, n);
end;

{$else} // MSWINDOWS

function CompileShaderFromSource(const shaderSource, entryPoint, target : AnsiString) : TBytes;
begin
   raise ED3DShaderCompilerException.Create('Not supported on this OS');
end;

{$endif}

end.
