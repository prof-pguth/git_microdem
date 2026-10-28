unit view3d_main;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

//{$Define UseFPointCloud}

{$Define NoMouseMove}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      {$Define Record3d}
      {$Define AddLayer}
      {$Define Record3dRender}
      {$Define Record_scaling}
      {$Define Start3DView}
      {$Define RecordFormIssues}
      //{$Define ShortRecord}
      //{$Define RecordMoves}
      //{$Define Record3dDetailed}
   {$EndIf}
{$EndIf}


{------------------------------------------------------------------------------
Original code
   Visualizing mathematical functions by generating custom meshes using FireMonkey
   By: Anders Ohlsson
   now gone from the Delphi web pages

Modified for XE7                       Peter Larson 6th January 2016
Modified for Points instead of Mesh    7th January 2016
Removed all references to mesh         8th January 2016

Works with no problems in DX Seattle   Peter Guth 8 Jan 2016
Add import of random point cloud       Peter Guth 8 Jan 2016

Remove functions, so it only displays GIS data  Peter Guth 17 Jan 2017
Takes two kinds of data; file extension tells program which it is dealing with:
    1.  .XYZB: Any map in MICRODEM with an associated DEM, exported with XYZ points, and a drape texture with 2D bitmap (this but not the next could use a mesh, which might not show holes when zooming in)
    2.  .XYXIB: Point cloud, with one 1D bitmap for lidar texture of classification or intensity

Added to MICRODEM code base
------------------------------------------------------------------------------}


interface

uses
//needed for inline of core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end for inline of core DB functions
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,  System.RTLConsts,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects3D, FMX.Types3D,
  FMX.Layouts, FMX.Layers3D, System.Math.Vectors, FMX.Controls3D,
  FMX.MaterialSources, FMX.StdCtrls,FMX.ListBox,
  FMX.Forms3D, FMX.Graphics, System.UIConsts, FMX.Controls.Presentation,
  DEMMapDraw,Petmar_types;

const
   ViewerVersion = ' v.32';
   MaxClouds = 5;

type
  TView3DForm = class(TForm3D)
    Camera1: TCamera;
    //Light1: TLight;
    Layer3D1: TLayer3D;
    Layout3D1: TLayout3D;
    GridXY: TGrid3D;
    GridXZ: TGrid3D;
    GridYZ: TGrid3D;
    CheckBox1: TCheckBox;
    Timer1: TTimer;
    Plane1: TPlane;
    Plane2: TPlane;
    Plane3: TPlane;
    Plane4: TPlane;
    TextureMaterialSource1: TTextureMaterialSource;
    TextureMaterialSource2: TTextureMaterialSource;
    TextureMaterialSource3: TTextureMaterialSource;
    TextureMaterialSource4: TTextureMaterialSource;
    GroupBox1: TGroupBox;
    CornerButton3: TCornerButton;
    CornerButton4: TCornerButton;
    CornerButton11: TCornerButton;
    CornerButton12: TCornerButton;
    CornerButton13: TCornerButton;
    CornerButton14: TCornerButton;
    ComboBox1: TComboBox;
    ShowDataSetsToPickGroupBox2: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    GroupBox3: TGroupBox;
    CornerButton8: TCornerButton;
    CornerButton7: TCornerButton;
    CornerButton9: TCornerButton;
    CornerButton6: TCornerButton;
    CornerButton5: TCornerButton;
    CornerButton10: TCornerButton;
    GroupBox6: TGroupBox;
    TrackBar3: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar1: TTrackBar;
    GroupBox7: TGroupBox;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    ListBox1: TListBox;
    GroupBox5: TGroupBox;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    GroupBox8: TGroupBox;
    CornerButton1: TCornerButton;
    Label1: TLabel;
    CornerButton2: TCornerButton;
    Rotation: TGroupBox;
    RotationReset: TButton;
    RotateY: TArcDial;
    LabelY: TLabel;
    RotateX: TArcDial;
    LabelX: TLabel;
    RotateZ: TArcDial;
    LabelZ: TLabel;
    GroupBox9: TGroupBox;
    CornerButton15: TCornerButton;
    CornerButton16: TCornerButton;
    Button4: TButton;
    CheckBox6: TCheckBox;
    Button5: TButton;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure Layout3D1Render(Sender: TObject; Context: TContext3D);
    procedure CheckBox1Change(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);
    procedure CornerButton1Click(Sender: TObject);
    procedure CornerButton2Click(Sender: TObject);
    procedure RotationResetClick(Sender: TObject);
    procedure RotateXChange(Sender: TObject);
    procedure RotateYChange(Sender: TObject);
    procedure RotateZChange(Sender: TObject);
    procedure Form3DClose(Sender: TObject; var Action: TCloseAction);
    procedure CornerButton5Click(Sender: TObject);
    procedure CornerButton6Click(Sender: TObject);
    procedure CornerButton8Click(Sender: TObject);
    procedure CornerButton7Click(Sender: TObject);
    procedure CornerButton9Click(Sender: TObject);
    procedure CornerButton10Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure TrackBar6Change(Sender: TObject);
    procedure CornerButton14Click(Sender: TObject);
    procedure CornerButton11Click(Sender: TObject);
    procedure CornerButton12Click(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CornerButton3Click(Sender: TObject);
    procedure CornerButton4Click(Sender: TObject);
    procedure CornerButton13Click(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CornerButton16Click(Sender: TObject);
    procedure CornerButton15Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
     procedure MakeFramesForMovie(var MovieFiles : tStringList);
  public
     xRange,yRange,zRange,ScalingGISzAxis,ScalingGISyAxis,rMinX,rMaxX,rMinY,rMaxY : double;
     rMinZ,rMaxZ : float32;
     DrapeFile : array[1..MaxClouds] of PathStr;
     MouseIsDown,LinkZScaling,Grayscale,ViewOnlyOneLayerAtTime : boolean;
     NumSingle,CurCloud,ExtraPoints,PointRepeatFactor : integer;
     NPtsUsed,NPtsAllocated : array[1..MaxClouds] of integer;
     Material : array[1..MaxClouds] of TTextureMaterialSource;  // Texture to be used
     VertexBuffer : array[1..MaxClouds] of tVertexBuffer;       // Vertex buffer for points
     IndexBuffer : array[1..MaxClouds] of TIndexBuffer;         // Index buffer to point to verticies
     ShowCloud : array[1..MaxClouds] of boolean;
     CloudName : array[1..MaxClouds] of ShortString;
     procedure SetScaling;
     function ScaledGISX(inX : double) : single; inLine;
     function ScaledGISY(inY : double) : single; inLine;
     function ScaledGISZ(inZ : double) : single; inLine;
     procedure AddPointWithTextureBitmap(x,y,z : double); inline;
     procedure AddPointWithSpecificColor(x,y,z : double); inline;
     procedure SaveVertex(x,y,z : double); inline;
     procedure Initialize(PointsToAllocate : integer; GridName : PathStr);
     procedure ArrangePanels;
     procedure ScaleViewToMapExtent(MapDraw : tMapDraw);
     procedure AddMap(aMapDraw : tMapDraw);
     procedure AddPointCloudLayer(GridName : PathStr);
  end;

const
   MaxX = 30;
var
   View3DForm, Map3D      : TView3DForm;


function MapTo3DView(GeometryFileName : PathStr) : TView3DForm;
procedure FMX3dViewer(ViewSeveral : boolean; FileList : tStringList; LinkZScaling : boolean = true);


implementation

{$R *.fmx}

uses
   {$IfDef VCL}
      nevadia_main,
   {$EndIf}

   {$IfDef UseFPointCloud}
      FPointCloud,
   {$EndIf}

   Petmar,PetImage,PetImage_Form,PetMath,Math,
   DEMDefs,DEMCoord,DEMDef_routines,
   New_Petmar_Movie,
   BaseMap;

var
  Down : TPointF;



procedure FMX3dViewer(ViewSeveral : boolean; FileList : tStringList; LinkZScaling : boolean = true);
var
   fName : PathStr;

        function StartFMX3dViewer(ViewSeveral : boolean; FileList : tStringList; LinkZScaling : boolean = true) : TView3DForm;
        var
           i : integer;
        begin
           {$IfDef Record3d} HighlightInDebugFile('StartFMX3dViewer in,  layers=' + IntToStr(FileList.Count)); {$EndIf}
           Result := TView3DForm.Create(Application);
           Result.LinkZScaling := LinkZScaling;
           Result.ViewOnlyOneLayerAtTime := ViewSeveral;
           Result.ShowDataSetsToPickGroupBox2.Visible := ViewSeveral;
           Result.NumSingle := 0;
           Result.PointRepeatFactor := 1;
           for i := 0 to pred(FileList.Count) do begin
              {$IfDef Record3d} HighlightInDebugFile('AddPointCloudLayer ' + FileList.Strings[i]); {$EndIf}
              Result.AddPointCloudLayer(FileList.Strings[i]);
           end;
           Result.ListBox1.Visible := (Result.NumSingle > 1);
           Result.Button3.Visible := (Result.NumSingle > 1);
           Result.ArrangePanels;
           {$If Defined(Start3DView)} WriteLineToDebugFile('StartFMX3dViewer out, window at ' + IntToStr(Result.Left) + 'x' + IntToStr(Result.Top)); {$EndIf}
        end;

begin
    //ExplorexyziFile(FileList.Strings[0]);
    if MDDef.New3Dviewer then begin
       {$IfDef Record3d} HighlightInDebugFile('FMX3dViewer new viewer in'); {$EndIf}
       fName := NextFileNumber(MDtempDir,'3d_viewer_flist_','.txt');
       FileList.SaveToFile(fName);
       ExecuteFile(ProgramRootDir + 'MD_PointCloudViewer.exe',fName);
    end
    else begin
       {$IfDef Record3d} HighlightInDebugFile('FMX3dViewer old viewer in'); {$EndIf}
       View3DForm := StartFMX3dViewer(ViewSeveral,FileList,LinkZScaling);
    end;
    FileList.Destroy;
    {$IfDef VCL} wmDem.SetMenusForVersion; {$EndIf}
end;


procedure TView3DForm.AddPointCloudLayer(GridName : PathStr);


      procedure GeneratePoints;
      //PLG code to read real GIS/elevation data set
      var
        P : TPoint3D;
        NP,Pts : Integer;
        NeedRange : boolean;

            procedure LoadMapWithColorByCodes;
            var
               PointXYZColor : ^tPointXYZIArray;
               tfile : File;
               i,x,y,z,pfd2 : integer;
               Color2D : boolean;
               xf,yf : float64;
               c : TAlphaColorRec;
               {$IfDef Record_scaling} xsmin,xsmax,ysmin,ysmax,zsmin,zsmax : float32; {$Endif}
            begin
                 if FileExists(GridName) then begin
                    {$IfDef Record3d} WriteLineToDebugFile('LoadMapWithColorByCodes in ' + GridName); {$EndIf}
                    assignFile(tfile,GridName);
                    reset(tFile,sizeOf(tPointXYZI));
                    new(PointXYZColor);
                    BlockRead(tfile,PointXYZColor^,MaxPts,Pts);
                    CloseFile(tFile);

                    if NeedRange then begin
                       //get range of real world coordinates in data set
                       for i := 1 to Pts do begin
                          if (abs(PointXYZColor^[i].x) < 0.01) and (abs(PointXYZColor^[i].y) < 0.01) then begin
                             HighlightInDebugFile('Zero location, pt=' + IntToStr(i));
                          end
                          else begin
                             CompareValueToExtremes(PointXYZColor^[i].x, rMinX,rMaxX );
                             CompareValueToExtremes(PointXYZColor^[i].y, rMinY,rMaxY );
                             CompareValueToExtremes(PointXYZColor^[i].z, rMinZ,rMaxZ );
                          end;
                       end;
                       {$If Defined(Record3dRange) or Defined(Record_Scaling)}
                          WriteLineToDebugFile('In file, xRange=' + RealToString(rminX,-12,1) + ' to ' +  RealToString(rmaxX,-12,1) +
                                               '  yRange=' + RealToString(rminY,-12,1) + ' to ' +  RealToString(rmaxY,-12,1) +
                                               '  zRange=' + RealToString(rminZ,-12,1) + ' to ' +  RealToString(rmaxZ,-12,1) );
                       {$EndIf}
                    end;
                    {$IfDef Record_scaling}
                       xsmin := 99e38;
                       xsmax := -99e38;
                       ysmin := 99e38;
                       ysmax := -99e38;
                       zsmin := 99e38;
                       zsmax := -99e38;
                    {$Endif}


                   Initialize(Pts,GridName);
                   Color2D := (DrapeFile[CurCloud] = FullPaletteBitmap);
                   SetScaling;

                   NP := 0;
                   pfd2 := PointRepeatFactor div 2;
                   for i := 1 to Pts do begin
                     if (abs(PointXYZColor^[i].x) > 0.01) and (abs(PointXYZColor^[i].y) > 0.01) then begin
                        if (PointRepeatFactor = 1) then begin
                           //we use coordinates with x and y horizontal, and z vertical
                           P.x := ScaledGISX(PointXYZColor^[i].x);
                           P.y := -ScaledGISZ(PointXYZColor^[i].z);
                           P.z := ScaledGISY(PointXYZColor^[i].y);
                           VertexBuffer[CurCloud].Vertices[NP] := P*25;    // Save vertex
                           if Color2D then begin
                              RGBtoXYFloat(PointXYZColor^[i].Int1,PointXYZColor^[i].Int2,PointXYZColor^[i].Int3,xf,yf);
                              VertexBuffer[CurCloud].TexCoord0[NP] := PointF(xf,yf);
                           end
                           else VertexBuffer[CurCloud].TexCoord0[NP] := PointF(0,PointXYZColor^[i].Int1/255); // Set texture value from color ramp
                           IndexBuffer[CurCloud][NP] := NP;                                // Set index to vertex
                           NP := NP+1;
                            {$IfDef Record_scaling}
                               CompareValueToExtremes(P.x,xsmin,xsmax);
                               CompareValueToExtremes(P.y,ysmin,ysmax);
                               CompareValueToExtremes(P.z,zsmin,zsmax);
                            {$Endif}
                        end
                        else begin
                           //we use coordinates with x and y horizontal, and z vertical
                           for x := -pfd2 to pfd2 do begin
                              for y := -pfd2 to pfd2 do begin
                                 for z := -pfd2 to pfd2 do begin
                                    P.x := ScaledGISX(PointXYZColor^[i].x) + x * 0.05;
                                    P.z := -ScaledGISY(PointXYZColor^[i].y) + y * 0.05;
                                    P.y := ScaledGISZ(PointXYZColor^[i].z) + z * 0.05;
                                    VertexBuffer[CurCloud].Vertices[NP] := P*25;                   // Save vertex
                                    if Color2D then begin
                                       RGBtoXYFloat(PointXYZColor^[i].Int1,PointXYZColor^[i].Int2,PointXYZColor^[i].Int3,xf,yf);
                                       VertexBuffer[CurCloud].TexCoord0[NP] := PointF(xf,yf);
                                    end
                                    else VertexBuffer[CurCloud].TexCoord0[NP] := PointF(0,PointXYZColor^[i].Int1/255); // Set texture value from color ramp
                                    IndexBuffer[CurCloud][NP] := NP;                                // Set index to vertex
                                    NP := NP+1;
                                 end;
                              end;
                           end;
                         end;
                      end;
                   end;
                   Dispose(PointXYZColor);
                   {$IfDef Record3d} WriteLineToDebugFile('LoadMapWithColorByCodes out'); {$EndIf}
                   {$IfDef Record_scaling}
                       WriteLineToDebugFile('  P.x:' + RealToString(xsmin,-12,-2) + ' to ' + RealToString(xsmax,-12,-2) +
                                            '  P.y:' + RealToString(ysmin,-12,-2) + ' to ' + RealToString(ysmax,-12,-2) +
                                            '  P.z:' + RealToString(zsmin,-12,-2) + ' to ' + RealToString(zsmax,-12,-2));
                    {$Endif}
               end;
            end {procedure LoadMapWithColorByCodes};

      var
         Ext : shortstring;
      begin {procedure GeneratePoints}
         {$IfDef Record3d} WriteLineToDebugFile('TView3DForm.GeneratePoints in, CurCloud=' + IntToStr(CurCloud)); {$EndIf}
         {$IfDef Android} Layer3D1.Width := 80; {$EndIf}

         if (CurCloud = 0) then begin
            {$IfDef Record3d} WriteLineToDebugFile('set default range'); {$EndIf}
            rMinX := 99e38;
            rMaxX := -99e38;
            rMinY := 99e38;
            rMaxY := -99e38;
            rMinZ := 99e38;
            rMaxZ := -99e38;
            NeedRange := true;
         end
         else begin
            NeedRange := false;
         end;

         Ext := UpperCase(ExtractFileExt(GridName));
         if (Ext = '.XYZB') then begin
            MessageToContinue('did not think we needed this');
            //LoadPointsWithFullDrapeMap;
         end
         else if (Ext = '.XYZIB') or (Ext = '.XYZI') then begin
            LoadMapWithColorByCodes;
         end
         else begin
            MessageToContinue('File not supported, ' + GridName);
         end;
         NPtsUsed[CurCloud] := Pts;
         {$IfDef Record3d} WriteLineToDebugFile('TView3DForm.GeneratePoints out, pts=' + IntToStr(NPtsUsed[CurCloud])); {$EndIf}
      end {procedure GeneratePoints};

       procedure DoCheckBox(CheckBox : tCheckBox);
       begin
          CheckBox.Visible := true;
          CheckBox.Text := ExtractFileNameNoExt(GridName);
       end;

begin {procedure TView3DForm.AddPointCloudLayer}
   if FileExists(GridName) then begin
      {$If Defined(AddLayer)} writeLineToDebugFile('Add Layer Grid=' + GridName + ' now CurCloud=' + IntToStr(CurCloud)); {$EndIf}
      GeneratePoints;
      {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('Points generated for CurCloud=' + IntToStr(CurCloud)); {$EndIf}

      if ViewOnlyOneLayerAtTime then begin
         case CurCloud of
            1 : DoCheckBox(CheckBox2);
            2 : DoCheckBox(CheckBox3);
            3 : DoCheckBox(CheckBox4);
            4 : DoCheckBox(CheckBox5);
            5 : DoCheckBox(CheckBox6);
         end;
      end
      else begin
         {$If Defined(AddLayer)} writeLineToDebugFile('ListBox1.Items.Add ' + ExtractFileName(GridName)); {$EndIf}
         ListBox1.Items.Add(ExtractFileNameNoExt(GridName));
         ListBox1.ItemIndex := -1;
         inc(NumSingle);
      end;
   end;
end {procedure TView3DForm.AddPointCloudLayer};


procedure TView3DForm.SaveVertex(x,y,z : double);
var
   P : array[0..0] of TPoint3D;
begin
   P[0].x := ScaledGISX(x);
   P[0].z := ScaledGISY(y);
   P[0].y := ScaledGISZ(z);
   VertexBuffer[CurCloud].Vertices[NPtsUsed[CurCloud]] := P[0]*25;   // Save vertex
   inc(NPtsUsed[CurCloud]);
end;


procedure TView3DForm.AddPointWithTextureBitmap(x,y,z : double);
begin
   VertexBuffer[CurCloud].TexCoord0[NPtsUsed[CurCloud]] := PointF((x - rMinx)/ XRange,1-(y - rMiny)/ YRange);
   IndexBuffer[CurCloud][NPtsUsed[CurCloud]] := NPtsUsed[CurCloud];       // Set index to vertex
   SaveVertex(x,y,z);
end;


procedure TView3DForm.AddPointWithSpecificColor(x,y,z : double{; xc,yc : integer});
begin
   VertexBuffer[CurCloud].TexCoord0[NPtsUsed[CurCloud]] := System.Types.PointF(0,0);
   IndexBuffer[CurCloud][NPtsUsed[CurCloud]] := NPtsUsed[CurCloud];       // Set index to vertex
   SaveVertex(x,y,z);
end;


procedure TView3DForm.Initialize(PointsToAllocate : integer; GridName : PathStr);
var
   k : integer;
   NeededPoints : int64;
   BMP : TBitmap;
   Data : tBitmapData;
begin
   {$IfDef Record3d} writeLineToDebugFile('TView3DForm.Initialize in'); {$EndIf}
   if (CurCloud < MaxClouds) then begin
      inc(CurCloud);
      NeededPoints := PointsToAllocate * PointRepeatFactor * PointRepeatFactor * PointRepeatFactor;
      NPtsAllocated[CurCloud] := NeededPoints;
      ShowCloud[CurCloud] := true;
      NPtsUsed[CurCloud] := 0;
      {$IfDef Record3d} WriteLineToDebugFile('Cloud=' + IntToStr(CurCloud) + ' can have pts=' + IntToStr(PointsToAllocate)); {$EndIf}
      if DrapeFile[CurCloud] = '' then begin
         DrapeFile[CurCloud] := ChangeFileExt(GridName,'.bmp');
         if not FileExists(DrapeFile[CurCloud]) then DrapeFile[CurCloud] := FullPaletteBitmap;
      end;

      //##################### Create vertex and index buffers ###################
      // Create vertex buffer with vertex data and texture value for each vertex
      // Second parameter is number of verticies
      VertexBuffer[CurCloud] := TVertexBuffer.Create([tVertexFormat.Vertex,tVertexFormat.TexCoord0],NeededPoints);
      // Create index buffer, paramter 1 is number of indexes, the second i size of data type (32 bits)
      IndexBuffer[CurCloud] := TIndexBuffer.Create(NeededPoints,tIndexFormat.UInt32);  //## Points
      {$IfDef Record3dFull} WriteLineToDebugFile('TIndexBuffer.Create done'); {$EndIf}

      //--------------------- Create texture ------------------------------------

      //##################### Set up texture to be used #########################
      Material[CurCloud] := TTextureMaterialSource.Create(Self);
      Material[CurCloud].Texture.LoadFromFile(DrapeFile[CurCloud]);

      Caption := '3D viewer' + ViewerVersion + ' --' + SmartNumberPoints(PointsToAllocate) + ' points';
      {$IfDef Record3dFull} writeLineToDebugFile('call set scaling'); {$EndIf}
      if (CurCloud = 1) then SetScaling;
      {$IfDef Record3d} WriteLineToDebugFile('TView3DForm.Initialize out with ' + SmartNumberPoints(PointsToAllocate) + ' points'); {$EndIf}
   end;
end {TView3DForm.Initialize};



procedure TView3DForm.AddMap(aMapDraw : tMapDraw);
var
   x,y,Pts : integer;
   Lat,Long,xu,yu,xgrid,ygrid : float64;
   z : float32;
   Good : boolean;
begin
   if (CurCloud = MaxClouds) then begin
      MessageToContinue('Reached cloud Limit=' + IntToStr(MaxClouds));
      exit;
   end;

   //{$IfDef VCL} StartProgress('Export '+ aMapDraw.BaseTitle); {$EndIf}
   Pts := aMapDraw.MapXSize * aMapDraw.MapYSize + ExtraPoints;
   if (Pts >= MaxPts) then Pts := MaxPts;
   DrapeFile[succ(CurCloud)] := aMapDraw.FullMapfName;
   Initialize(Pts,'');

   for x := 0 to pred(aMapDraw.MapXSize) do begin
      //{$IfDef VCL} if (x mod 100 = 0) then UpDateProgressBar(x/aMapDraw.MapXSize); {$EndIf}
      for y := 0 to pred(aMapDraw.MapYSize) do begin
         if ValidDEM(aMapDraw.DEM2OnMap) then begin
            aMapDraw.ScreenToDEMGrid(x,y,xgrid,ygrid);
            DEMGlb[aMapDraw.DEMonMap].DEMGridToLatLongDegree(xgrid,ygrid,Lat,Long);
            Good := DEMGlb[aMapDraw.DEM2onMap].GetElevFromLatLongDegree(Lat,Long,z);
         end
         else begin
            Good := aMapDraw.ScreenToElev(x,y,z)
         end;
         if Good then begin
            aMapDraw.ScreenToUTM(x,y,xu,yu);
            AddPointWithTextureBitmap(xu,yu,z);
         end;
        if (NPtsUsed[CurCloud] >= NPtsAllocated[CurCloud]) then break;
      end;
   end;

   //{$IfDef VCL} EndProgress; {$EndIf}
   if ViewOnlyOneLayerAtTime then begin
      ListBox1.Visible := true;
      ListBox1.Items.Add(aMapDraw.BaseTitle);
   end
   else begin
      if (CurCloud = 1) then begin
         CheckBox2.Visible := true;
         CheckBox2.Text := aMapDraw.BaseTitle;
      end
      else begin
         ShowDataSetsToPickGroupBox2.Visible := true;
         Button3.Visible := true;
         CheckBox3.Visible := CurCloud >= 2;
         CheckBox4.Visible := CurCloud >= 3;
         CheckBox5.Visible := CurCloud >= 4;
         CheckBox6.Visible := CurCloud >= 5;
         case CurCloud of
            2 : CheckBox3.Text := aMapDraw.BaseTitle;
            3 : CheckBox4.Text := aMapDraw.BaseTitle;
            4 : CheckBox5.Text := aMapDraw.BaseTitle;
            5 : CheckBox6.Text := aMapDraw.BaseTitle;
         end;
      end;
   end;
   ArrangePanels;
   Show;
   {$If Defined(AddLayer)} WriteLineToDebugFile('TView3DForm.AddMap, CurCloud=' + IntToStr(CurCloud) + '  ' + aMapDraw.BaseTitle + '  pts=' + IntToStr(NPtsUsed[CurCloud] )); {$EndIf}
end {procedure TView3DForm.AddMap};


function MapTo3DView(GeometryFileName : PathStr) : TView3DForm;
var
   GeometryFName : PathStr;
   FileList : tStringList;
begin
   FileList := tStringList.Create;
   FileList.Add(GeometryFName);
   {$IfDef TrackMapColors} WriteStringListToDebugFile(ColorsUsed);  ColorsUsed.Destroy;  {$EndIf}
end;


procedure TView3DForm.MakeFramesForMovie(var MovieFiles : tStringList);

    procedure SaveFrame(fName : PathStr = '');
    begin
       PetImage.SaveScreenCapture(round(Layer3D1.Width),fName,false,true);
       MovieFiles.Add(fName);
       Delay(500);
    end;

    procedure DoCheckBox(theBox : tCheckBox; Cloud : integer);
    begin
       if theBox.Visible then begin
          theBox.IsChecked := true;
          ShowCloud[Cloud] := true;
          SaveFrame;
          theBox.IsChecked := false;
          ShowCloud[Cloud] := false;
       end;
    end;

var
   i : integer;
begin
   MovieFiles := tStringList.Create;
   if ViewOnlyOneLayerAtTime then begin
      for i := 0 to pred(ListBox1.Items.Count) do begin
         ListBox1.ItemIndex := i;
         ListBox1Change(Button3);
         SaveFrame(ListBox1.Items[i] + '.png');
      end;
   end
   else begin
      CheckBox2.IsChecked := false;
      CheckBox3.IsChecked := false;
      CheckBox4.IsChecked := false;
      CheckBox5.IsChecked := false;
      CheckBox6.IsChecked := false;
      DoCheckBox(CheckBox2,1);
      DoCheckBox(CheckBox3,2);
      DoCheckBox(CheckBox4,3);
      DoCheckBox(CheckBox5,4);
      DoCheckBox(CheckBox6,5);
      CheckBox2.IsChecked := true;
      ShowCloud[1] := true;
   end;
end;


procedure TView3DForm.Button5Click(Sender: TObject);
var
   MovieFiles : tstringlist;
begin
   {$IfDef VCL}
      MakeFramesForMovie(MovieFiles);
      MakeBigBitmap(MovieFiles,'');
   {$EndIf}
end;


procedure TView3DForm.CheckBox6Change(Sender: TObject);
begin
   if ViewOnlyOneLayerAtTime then begin
      CheckBox2.IsChecked := false;
      CheckBox3.IsChecked := false;
      CheckBox4.IsChecked := false;
      CheckBox5.IsChecked := false;
   end;
   ShowCloud[5] := CheckBox6.IsChecked;
end;


procedure TView3DForm.ArrangePanels;
var
   CurrentTop : integer;

         procedure CheckGroupBox(GB : tGroupBox);
         begin
            if GB.Visible then begin
               GB.Position.Y := CurrentTop;
               CurrentTop := CurrentTop + round(GB.Height + 5);
            end;
         end;

begin
   CurrentTop := 0;
   CheckGroupBox(Rotation);
   CheckGroupBox(GroupBox3);
   CheckGroupBox(GroupBox8);
   CheckGroupBox(GroupBox9);
   CheckGroupBox(ShowDataSetsToPickGroupBox2);
   if ListBox1.Visible then begin
      ListBox1.Position.Y := CurrentTop;
      CurrentTop := CurrentTop + round(ListBox1.Height + 5);
   end;
   CheckGroupBox(GroupBox5);
   CheckBox1.Position.Y := CurrentTop;
   Show;
end;

procedure TView3DForm.Button1Click(Sender: TObject);
var
   angle,endangle : integer;
   fname,MovieName : PathStr;
   MovieFiles : tstringlist;
begin
   {$IfDef VCL}
      angle := round(Layout3D1.RotationAngle.Y);
      endangle := angle + 360;
      MovieFiles := tStringList.Create;
      while angle <= endangle do begin
         RotateZ.Value := angle;
         RotateZChange(sender);
         PetImage.SaveScreenCapture(round(Layer3D1.Width),fName);
         MovieFiles.Add(ExtractFileName(fName));
         angle := angle + 15;
         Delay(500);
      end;
      MovieName := MovieDir + 'oglmovie.mov';
      MovieFiles.SaveToFile(MovieName);
      MovieFiles.Free;
      CreateNewMovie(MovieName);
   {$EndIf}
end;

procedure TView3DForm.Button2Click(Sender: TObject);
begin
   {$IfDef VCL}
      PetImage.CopyToWindowToClipBoard(round(Layer3D1.Width));
   {$EndIf}
end;


procedure TView3DForm.Button3Click(Sender: TObject);
var
   MovieName : PathStr;
   MovieFiles : tstringlist;
begin
   {$IfDef VCL}
      MakeFramesForMovie(MovieFiles);
      MovieName := MovieDir + 'oglmovie2.mov';
      MovieFiles.SaveToFile(MovieName);
      MovieFiles.Free;
      CreateNewMovie(MovieName);
   {$EndIf}
end;


procedure TView3DForm.Button4Click(Sender: TObject);
var
   fname : PathStr;
begin
   {$IfDef VCL}
      fName := '';
      PetImage.SaveScreenCapture(round(Layer3D1.Width),fName,true);
   {$EndIf}
end;

procedure TView3DForm.CornerButton15Click(Sender: TObject);
begin
   Camera1.Position.Vector := Camera1.Position.Vector + Vector3D(0, 0, 1) * ((-10 / 120) * 0.3)
end;

procedure TView3DForm.CornerButton16Click(Sender: TObject);
begin
   Camera1.Position.Vector := Camera1.Position.Vector + Vector3D(0, 0, 1) * ((10 / 120) * 0.3)
end;


procedure TView3DForm.ListBox1Change(Sender: TObject);
begin
   {$If Defined(Record3D)} writeLineToDebugFile('tView3DForm.ListBox1Change, ItemIndex=' + IntToStr(ListBox1.ItemIndex)); {$EndIf}
   ShowCloud[1] := ListBox1.ItemIndex = 0;
   ShowCloud[2] := ListBox1.ItemIndex = 1;
   ShowCloud[3] := ListBox1.ItemIndex = 2;
   ShowCloud[4] := ListBox1.ItemIndex = 3;
   ShowCloud[5] := ListBox1.ItemIndex = 4;
end;


procedure TView3DForm.CheckBox2Change(Sender: TObject);
begin
   if ViewOnlyOneLayerAtTime then begin
      CheckBox3.IsChecked := false;
      CheckBox4.IsChecked := false;
      CheckBox5.IsChecked := false;
      CheckBox6.IsChecked := false;
   end;
   Plane1.Visible := CheckBox2.IsChecked;
   ShowCloud[1] := CheckBox2.IsChecked;
end;

procedure TView3DForm.CheckBox3Change(Sender: TObject);
begin
   if ViewOnlyOneLayerAtTime then begin
      CheckBox2.IsChecked := false;
      CheckBox4.IsChecked := false;
      CheckBox5.IsChecked := false;
      CheckBox6.IsChecked := false;
   end;
   Plane2.Visible := CheckBox3.IsChecked;
   ShowCloud[2] := CheckBox3.IsChecked;
end;

procedure TView3DForm.CheckBox4Change(Sender: TObject);
begin
   if ViewOnlyOneLayerAtTime then begin
      CheckBox2.IsChecked := false;
      CheckBox3.IsChecked := false;
      CheckBox5.IsChecked := false;
      CheckBox6.IsChecked := false;
   end;
   Plane3.Visible := CheckBox4.IsChecked;
   ShowCloud[3] := CheckBox4.IsChecked;
end;

procedure TView3DForm.CheckBox5Change(Sender: TObject);
begin
   if ViewOnlyOneLayerAtTime then begin
      CheckBox2.IsChecked := false;
      CheckBox3.IsChecked := false;
      CheckBox4.IsChecked := false;
      CheckBox6.IsChecked := false;
   end;
   Plane4.Visible := CheckBox5.IsChecked;
   ShowCloud[4] := CheckBox5.IsChecked;
end;


procedure TView3DForm.CornerButton11Click(Sender: TObject);
begin  //y plane in real world coordinates
   if ComboBox1.ItemIndex = 0 then Plane1.Position.z := Plane1.Position.z - 1;
   if ComboBox1.ItemIndex = 1 then Plane2.Position.z := Plane2.Position.z - 1;
   if ComboBox1.ItemIndex = 2 then Plane3.Position.z := Plane3.Position.z - 1;
   if ComboBox1.ItemIndex = 3 then Plane4.Position.z := Plane4.Position.z - 1;
end;

procedure TView3DForm.CornerButton12Click(Sender: TObject);
begin  //z plane in real world coordinates
   if ComboBox1.ItemIndex = 0 then Plane1.Position.y := Plane1.Position.y - 1;
   if ComboBox1.ItemIndex = 1 then Plane2.Position.y := Plane2.Position.y - 1;
   if ComboBox1.ItemIndex = 2 then Plane3.Position.y := Plane3.Position.y - 1;
   if ComboBox1.ItemIndex = 3 then Plane4.Position.y := Plane4.Position.y - 1;
end;

procedure TView3DForm.CornerButton13Click(Sender: TObject);
begin //z plane in real world coordinates
   if ComboBox1.ItemIndex = 0 then Plane1.Position.y := Plane1.Position.y + 1;
   if ComboBox1.ItemIndex = 1 then Plane2.Position.y := Plane2.Position.y + 1;
   if ComboBox1.ItemIndex = 2 then Plane3.Position.y := Plane3.Position.y + 1;
   if ComboBox1.ItemIndex = 3 then Plane4.Position.y := Plane4.Position.y + 1;
end;

procedure TView3DForm.CornerButton14Click(Sender: TObject);
begin
   if ComboBox1.ItemIndex = 0 then Plane1.Position.X := Plane1.Position.X - 1;
   if ComboBox1.ItemIndex = 1 then Plane2.Position.X := Plane2.Position.X - 1;
   if ComboBox1.ItemIndex = 2 then Plane3.Position.X := Plane3.Position.X - 1;
   if ComboBox1.ItemIndex = 3 then Plane4.Position.X := Plane4.Position.X - 1;
end;


procedure TView3DForm.CornerButton3Click(Sender: TObject);
begin
   if ComboBox1.ItemIndex = 0 then Plane1.Position.X := Plane1.Position.X + 1;
   if ComboBox1.ItemIndex = 1 then Plane2.Position.X := Plane2.Position.X + 1;
   if ComboBox1.ItemIndex = 2 then Plane3.Position.X := Plane3.Position.X + 1;
   if ComboBox1.ItemIndex = 3 then Plane4.Position.X := Plane4.Position.X + 1;
end;

procedure TView3DForm.CornerButton4Click(Sender: TObject);
begin // y plane in real world coordinates
   if ComboBox1.ItemIndex = 0 then Plane1.Position.z := Plane1.Position.z + 1;
   if ComboBox1.ItemIndex = 1 then Plane2.Position.z := Plane2.Position.z + 1;
   if ComboBox1.ItemIndex = 2 then Plane3.Position.z := Plane3.Position.z + 1;
   if ComboBox1.ItemIndex = 3 then Plane4.Position.z := Plane4.Position.z + 1;
end;


procedure tView3DForm.ScaleViewToMapExtent(MapDraw : tMapDraw);
var
   AvgElev : float32;
begin
   if (MDdef.MercShiftLongLimit < MapDraw.MapCorners.BoundBoxGeo.xmax - MapDraw.MapCorners.BoundBoxGeo.xmin) then begin
      {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('tView3DForm.ScaleViewToMapExtent in, option 1'); {$EndIf}
      rMinX := MapDraw.MapCorners.BoundBoxProj.xmin;
      rMaxX := MapDraw.MapCorners.BoundBoxProj.xmax;
      rMinY := MapDraw.MapCorners.BoundBoxProj.ymin;
      rMaxY := MapDraw.MapCorners.BoundBoxProj.ymax;
   end
   else begin
      {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('tView3DForm.ScaleViewToMapExtent in, option 2'); {$EndIf}
      rMinX := MapDraw.MapCorners.BoundBoxUTM.xmin;
      rMaxX := MapDraw.MapCorners.BoundBoxUTM.xmax;
      rMinY := MapDraw.MapCorners.BoundBoxUTM.ymin;
      rMaxY := MapDraw.MapCorners.BoundBoxUTM.ymax;
   end;

   if ValidDEM(MapDraw.DEMonMap) then DEMGlb[MapDraw.DEMonMap].BoxAreaExtremeElevations(MapDraw.MapAreaDEMGridLimits,rMinZ,rMaxZ,AvgElev)
   else begin
      //This is for Chirps                 
      rMinZ := 0;
      rMaxZ := 15;
   end;
   {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('tView3DForm.ScaleViewToMapExtent out'); {$EndIf}
end;


procedure tView3DForm.SetScaling;
begin
   xRange := (rMaxx - rMinx);
   yRange := (rMaxy - rMiny);
   zRange := (rMaxz - rMinz);
   {if LinkZScaling then} ScalingGISZAxis := zRange / xRange;
   //else ScalingGISZAxis := 1;
   ScalingGISyAxis := yRange / xRange;

   {$IfDef Record_scaling}
      WriteLineToDebugFile('Set scaling x=' + RealToString(rminX,-12,2) + ' to ' +  RealToString(rmaxX,-12,2) + '  y=' + RealToString(rminY,-12,2) + ' to ' +  RealToString(rmaxY,-12,2) +
         ' z=' + RealToString(rminZ,-12,2) + ' to ' +  RealToString(rmaxZ,-12,2) + ' ScalingZ=' + RealToString(ScalingGISZAxis,-12,4) + ' ScalingY=' + RealToString(ScalingGISYAxis,-12,4));
   {$EndIf}
end;


procedure TView3DForm.Timer1Timer(Sender: TObject);
begin
   //Label3.Text := 'Rot: ' + RealToString(Layout3D1.RotationAngle.X,-6,1) + ' ' + RealToString(Layout3D1.RotationAngle.Z,-6,1) + ' ' + RealToString(Layout3D1.RotationAngle.Y,-6,1);
   //Label4.Text := 'Pos: ' + RealToString(Layout3D1.Position.X,-6,1) + ' ' + RealToString(Layout3D1.Position.Z,-6,1) + ' ' + RealToString(Layout3D1.Position.Y,-6,1);
   //Label5.Text := 'Cam: ' + RealToString(Camera1.Position.Vector.X,-6,1) + ' ' + RealToString(Camera1.Position.Vector.Z,-6,1) + ' ' +  RealToString(Camera1.Position.Vector.Y,-6,1);
end;

procedure TView3DForm.TrackBar1Change(Sender: TObject);
begin
   Camera.Position.X := TrackBar1.Value;
end;

procedure TView3DForm.TrackBar2Change(Sender: TObject);
begin
  Camera.Position.Z := TrackBar2.Value;
end;

procedure TView3DForm.TrackBar3Change(Sender: TObject);
begin
   Camera.Position.Y := TrackBar3.Value;
end;

procedure TView3DForm.TrackBar4Change(Sender: TObject);
begin
   Camera.RotationAngle.x := TrackBar4.Value;
end;

procedure TView3DForm.TrackBar5Change(Sender: TObject);
begin
   Camera.RotationAngle.z := TrackBar5.Value;
end;

procedure TView3DForm.TrackBar6Change(Sender: TObject);
begin
   Camera.RotationAngle.y := TrackBar6.Value;
end;

function tView3DForm.ScaledGISX(inX : double) : single;
begin
   Result := -1.0 * MaxX + (inx - rMinX) / XRange * 2 * MaxX;
end;

function tView3DForm.ScaledGISY(inY : double) : single;
begin
   Result := -1.0 * MaxX - ScalingGISYAxis * (inY - rMinY) / yRange * 2 * MaxX;
end;

function tView3DForm.ScaledGISZ(inZ : double) : single;
begin
   Result := -1.0 * MaxX - ScalingGISZAxis * (inz - rMinz) / ZRange * 2 * MaxX;
end;


{#############################################################################
   Code renders all of the points. Pretty simple!!!
#############################################################################}
procedure TView3DForm.Layout3D1Render(Sender: TObject; Context: TContext3D);
var
   i : integer;
begin
   {$IfDef Record3dRender} WriteLineToDebugFile('TView3DForm.Layout3D1Render in'); {$EndIf}
   for i := MaxClouds downto 1 do begin
      if (NPtsUsed[i] > 0) and ShowCloud[i] then begin
         {$IfDef Record3dRender} WriteLineToDebugFile('Draw cloud ' + IntToStr(i)); {$EndIf}
         Context.DrawPoints(VertexBuffer[i],IndexBuffer[i],Material[i].Material,1);
      end;
   end;
   {$IfDef Record3dRender} WriteLineToDebugFile('TView3DForm.Layout3D1Render out'); {$EndIf}
end;


procedure TView3DForm.RotateXChange(Sender: TObject);
begin
  Layout3D1.RotationAngle.X := RotateX.Value;
end;

procedure TView3DForm.RotateYChange(Sender: TObject);
begin
  //GIS Y is FMX graphics Z
  Layout3D1.RotationAngle.Z := RotateY.Value;
end;

procedure TView3DForm.RotateZChange(Sender: TObject);
begin
  //GIS Z is FMX graphics Y
  Layout3D1.RotationAngle.Y := RotateZ.Value;
end;

procedure TView3DForm.RotationResetClick(Sender: TObject);
begin
  RotateX.Value:=0;
  RotateY.Value:=0;
  RotateZ.Value:=0;
  Layout3D1.RotationAngle.X := RotateX.Value;
  Layout3D1.RotationAngle.Y := RotateY.Value;
  Layout3D1.RotationAngle.Z := RotateZ.Value;
end;


procedure TView3DForm.CheckBox1Change(Sender: TObject);
begin
   GridXY.Visible := CheckBox1.IsChecked;
   GridYZ.Visible := CheckBox1.IsChecked;
   GridXZ.Visible := CheckBox1.IsChecked;
end;


procedure TView3DForm.CornerButton10Click(Sender: TObject);
begin
   Layout3D1.Position.Z := Layout3D1.Position.Z + MDDef.OGLDefs.MoveIncr;
end;

procedure TView3DForm.CornerButton1Click(Sender: TObject);
begin
   Layout3D1.Scale.Y := Layout3D1.Scale.Y * 1.25;
   Label1.Text := floattostrf(10 * Layout3D1.Scale.Y, fffixed,6,2);
   //Layout3D1.Position.Y := Layout3D1.Position.Y + 2 * 1.25 * MDDef.OGLDefs.MoveIncr;
   {$If Defined(RecordMoves)} WriteLineToDebugFile('TView3DForm.CornerButton1Click (vert Exag up), position.Y=' + RealToString(Layout3D1.Position.Y,-8,-2) + '  new scale=' + RealToString(Layout3D1.Scale.Y,-8,-2) ); {$EndIf}
end;


procedure TView3DForm.CornerButton2Click(Sender: TObject);
begin
   Layout3D1.Scale.Y := Layout3D1.Scale.Y * 0.80;
   Label1.Text := floattostrf(10 * Layout3D1.Scale.Y, fffixed,6,2);
   //Layout3D1.Position.Y := Layout3D1.Position.Y - 0.80 * MDDef.OGLDefs.MoveIncr;
   {$If Defined(RecordMoves)} WriteLineToDebugFile('TView3DForm.CornerButton2Click (vert Exag down), position.Y=' + RealToString(Layout3D1.Position.Y,-8,-2) + '  new scale=' + RealToString(Layout3D1.Scale.Y,-8,-2) ); {$EndIf}
end;


procedure TView3DForm.CornerButton5Click(Sender: TObject);
begin
   Layout3D1.Position.Y := Layout3D1.Position.Y - MDDef.OGLDefs.MoveIncr;
   {$If Defined(RecordMoves)} WriteLineToDebugFile('TView3DForm.CornerButton5Click (Y down), new position.Y=' + RealToString(Layout3D1.Position.Y,-8,-2) + '  scale=' + RealToString(Layout3D1.Scale.Y,-8,-2) ); {$EndIf}
end;

procedure TView3DForm.CornerButton6Click(Sender: TObject);
begin
   Layout3D1.Position.Y := Layout3D1.Position.Y + MDDef.OGLDefs.MoveIncr;
   {$If Defined(RecordMoves)} WriteLineToDebugFile('TView3DForm.CornerButton5Click (Y up), new position.Y=' + RealToString(Layout3D1.Position.Y,-8,-2) + '  scale=' + RealToString(Layout3D1.Scale.Y,-8,-2) ); {$EndIf}
end;

procedure TView3DForm.CornerButton7Click(Sender: TObject);
begin
   Layout3D1.Position.X := Layout3D1.Position.X - MDDef.OGLDefs.MoveIncr;
end;

procedure TView3DForm.CornerButton8Click(Sender: TObject);
begin
   Layout3D1.Position.X := Layout3D1.Position.X + MDDef.OGLDefs.MoveIncr;
end;

procedure TView3DForm.CornerButton9Click(Sender: TObject);
begin
   Layout3D1.Position.Z := Layout3D1.Position.Z - MDDef.OGLDefs.MoveIncr;
end;

procedure TView3DForm.Form3DClose(Sender: TObject; var Action: TCloseAction);
var
   i : integer;
begin
   for I := 1 to MaxClouds do begin
      if (NPtsUsed[i] > 0) then begin
         VertexBuffer[i].Free;
         IndexBuffer[i].Free;
      end;
   end;
   MDDef.OGLDefs.OpenGLDefaultTopX := Top;
   MDDef.OGLDefs.OpenGLDefaultTopY := Left;
   MDDef.OGLDefs.OpenGLDefaultHeight := Height;
   MDDef.OGLDefs.OpenGLDefaultWidth := Width;
   Action := TCloseAction.caFree;
end;


procedure TView3DForm.Form3DCreate(Sender: TObject);
var
   i : integer;
begin
   CheckBox1.IsChecked := false;
   MouseIsDown := false;
   ViewOnlyOneLayerAtTime := false;
   LinkZScaling := true;
   MDDef.OGLDefs.MoveIncr := 7.5;

   Top := MDDef.OGLDefs.OpenGLDefaultTopX;
   Left := MDDef.OGLDefs.OpenGLDefaultTopY;
   Height := MDDef.OGLDefs.OpenGLDefaultHeight;
   Width := MDDef.OGLDefs.OpenGLDefaultWidth;
   PointRepeatFactor := 1;
   CurCloud := 0;
   for I := 1 to MaxClouds do begin
     ShowCloud[i] := false;
     NPtsAllocated[i] := 0;
     NPtsUsed[i] := 0;
   end;

   GroupBox6.Visible := false;
   GroupBox7.Visible := false;
   TrackBar1.Visible := false;
   TrackBar2.Visible := false;
   TrackBar3.Visible := false;
   TrackBar4.Visible := false;
   TrackBar5.Visible := false;
   TrackBar6.Visible := false;
   Timer1.Enabled := false;
end;


procedure TView3DForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   {$IfDef NoMouseMove} exit; {$EndIf}
   {$IfDef RecordFormIssues} WriteLineToDebugFile(' TView3DForm.FormMouseDown'); {$EndIf}
   Down := PointF(X, Y);
   MouseIsDown := true;
   MouseCapture;
end;

procedure TView3DForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   {$IfDef NoMouseMove} exit; {$EndIf}
   {$IfDef RecordFormIssues} WriteLineToDebugFile(' TView3DForm.FormMouseUp');   {$EndIf}
   ReleaseCapture;
   MouseIsDown := false;
end;


procedure TView3DForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
begin
   {$IfDef NoMouseMove} exit; {$EndIf}
   //{$IfDef RecordFormIssues} WriteLineToDebugFile(' TView3DForm.FormMouseMove x=' + RealToString(x,-12,-2) + ' y=' + RealToString(y,-12,-2));  {$EndIf}
   if MouseIsDown then begin
      Layout3D1.RotationAngle.X := Layout3D1.RotationAngle.X - ((Y - Down.Y) * 0.1);      //was 0.3
      Layout3D1.RotationAngle.Y := Layout3D1.RotationAngle.Y + ((X - Down.X) * 0.1);
      Down := PointF(X, Y);
   end
   else if (x < 0) and (y < 0) then begin
      MouseIsDown := false;
   end;
end;


procedure TView3DForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
   Camera1.Position.Vector := Camera1.Position.Vector + Vector3D(0, 0, 1) * ((WheelDelta / 120) * 0.3);
end;


initialization
   View3dForm := nil;
   Map3d := Nil;
finalization
end.




