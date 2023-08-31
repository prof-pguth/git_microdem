removed 20 Aug 2023


unit firemonkey_common;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}
   //{$Define Record3DProblems}
{$EndIf}


interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,System.RTLConsts,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects,FMX.Objects3D, FMX.Types3D,

  System.Math,System.UIConsts,System.Sensors, System.Sensors.Components,  System.Math.Vectors,

  DEMMapDraw, FireDAC.UI.Intf, FireDAC.FMXUI.Wait, FireDAC.Stan.Intf,
  FireDAC.Comp.UI, FMX.ExtCtrls,

  {$IfDef Android}
     Androidapi.Jni.Media,Androidapi.helpers,
  {$Endif}

  {$IfDef RecordTime}
     System.Diagnostics,System.TimeSpan,
  {$Endif}

  Petmar_types,
  DEMDefs;


procedure GenerateMesh(NewDEM : integer; TheMesh : tMesh; GridLimits : tGridLimits);

{$IfDef Android}
   procedure SetFileForViewing(sYourPath : PathStr);
{$EndIf}


implementation

uses
   PetImage, PetMath,
   DEMCoord,
   DEMDef_routines,
   Petmar;

var
  MinF : Double = 2000;
  MaxF : Double = -2000;

{$IfDef Android}
   procedure SetFileForViewing(sYourPath : PathStr);
   //without this, the file on the Android will not be visible on the PC until the Android reboots
   var
      c : Integer;
      JMediaScannerCon : Androidapi.Jni.Media.JMediaScannerConnection;
      JMediaScannerCon_Client : Androidapi.Jni.Media.JMediaScannerConnection_MediaScannerConnectionClient;
   begin
       JMediaScannerCon:=TJMediaScannerConnection.JavaClass.init(SharedActivityContext, JMediaScannerCon_Client);
       JMediaScannerCon.connect;
       c:=0;
       while not JMediaScannerCon.isConnected do begin
          Sleep(100);
          inc(c);
          if (c>20) then break;
       end;
       if (JMediaScannerCon.isConnected) then begin
          JMediaScannerCon.scanFile(StringToJString(sYourPath), nil);
          JMediaScannerCon.disconnect;
       end;
   end;
{$EndIf}



procedure GenerateMesh(NewDEM : integer; TheMesh : tMesh; GridLimits : tGridLimits);
//not recently tested
//FMX code, currently with a hard coded number of vertices to load
//plan is to load the largest number of triangles that will work
//this currently takes just the DEM, since I cannot programmatically load the draping bitmap texture
const
  MaxX = 30;
  MaxZ = 30;
var
  u, v, d : Double;
  xs,ys,zr : float64;
  P : array[0..3] of TPoint3D;
  NP,NI,Grids : Integer;

      procedure SetZ(u,v : float64; var z : single);
      var
         zf : float32;
      begin
         if not DEMGlb[NewDEM].GetElevMeters(Gridlimits.XGridLow + (u + MaxX)/(2*MaxX) * xs, Gridlimits.YGridLow + (v + MaxZ)/(2*MaxZ) * ys, zf) then zf := 0;
         z := ((zf - DEMGlb[NewDEM].DEMHeader.MinElev) / zr);
      end;

begin
  {$IfDef Record3DProblems} WriteLineToDebugFile('Generate mesh in'); {$EndIf}

  TheMesh.Data.Clear;
  d := 0.075;
  NP := 0;
  NI := 0;
  xs := GridLimits.XGridHigh - GridLimits.XGridLow;
  ys := GridLimits.YGridHigh - GridLimits.YGridLow;
  zr := (DEMGlb[NewDEM].DEMHeader.MaxElev-DEMGlb[NewDEM].DEMHeader.MinElev);

  // We have to set these up front. Buffers cleared every time Length is set.
  TheMesh.Data.VertexBuffer.Length := Round(2*MaxX*2*MaxZ/d/d)*4;
  TheMesh.Data.IndexBuffer.Length := Round(2*MaxX*2*MaxZ/d/d)*6;
  Grids := 0;
  u := -MaxX;
  while u < MaxX-d do begin
    v := -MaxZ;
    while v < MaxZ-d do begin
      // Set up points in XZ plane used by FMX
      P[0].z := 1*u;
      P[0].x := 1*v;
      P[1].z := 1*(u+d);
      P[1].x := 1*v;
      P[2].z := 1*(u+d);
      P[2].x := 1*(v+d);
      P[3].z := 1*u;
      P[3].x := 1*(v+d);

      // Calculate corresponding function values for Y = f(X,Z)
      SetZ(u,v,P[0].Y);
      SetZ(u+d,v,P[1].Y);
      SetZ(u+d,v+d,P[2].Y);
      SetZ(u,v+d,P[3].Y);

     // Map colors
       (*
       //this is for a color ramp, loaded at design time
       TheMesh.Data.VertexBuffer.TexCoord0[NP+0] := PointF(0,P[0].y);
       TheMesh.Data.VertexBuffer.TexCoord0[NP+1] := PointF(0,P[1].y);
       TheMesh.Data.VertexBuffer.TexCoord0[NP+2] := PointF(0,P[2].y);
       TheMesh.Data.VertexBuffer.TexCoord0[NP+3] := PointF(0,P[3].y);
       *)

       //we cannot load the bitmap programmatically
       TheMesh.Data.VertexBuffer.TexCoord0[NP+0] := PointF(P[0].x/30,P[0].z/30);
       TheMesh.Data.VertexBuffer.TexCoord0[NP+1] := PointF(P[1].x/30,P[1].z/30);
       TheMesh.Data.VertexBuffer.TexCoord0[NP+2] := PointF(P[2].x/30,P[2].z/30);
       TheMesh.Data.VertexBuffer.TexCoord0[NP+3] := PointF(P[3].x/30,P[3].z/30);

     // Set points
       TheMesh.Data.VertexBuffer.Vertices[NP+0] := P[0];
       TheMesh.Data.VertexBuffer.Vertices[NP+1] := P[1];
       TheMesh.Data.VertexBuffer.Vertices[NP+2] := P[2];
       TheMesh.Data.VertexBuffer.Vertices[NP+3] := P[3];

        // Map triangles
        TheMesh.Data.IndexBuffer[NI+0] := NP+1;
        TheMesh.Data.IndexBuffer[NI+1] := NP+2;
        TheMesh.Data.IndexBuffer[NI+2] := NP+3;
        TheMesh.Data.IndexBuffer[NI+3] := NP+3;
        TheMesh.Data.IndexBuffer[NI+4] := NP+0;
        TheMesh.Data.IndexBuffer[NI+5] := NP+1;

      NP := NP+4;
      NI := NI+6;
      v := v+d;
      inc(Grids);
    end;
    u := u+d;
  end;
  {$IfDef Record3DProblems} WriteLineToDebugFile('Generate mesh out; Grids=' + IntToStr(Grids)); {$EndIf}
end;



end.
