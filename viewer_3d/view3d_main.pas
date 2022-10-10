unit view3d_main;

{------------------------------------------------------------------------------
Original code
   Visualizing mathematical functions by generating custom meshes using FireMonkey
   By: Anders Ohlsson

Modified for XE7                       Peter Larson 6th January 2016
Modified for Points instead of Mesh    7th January 2016
Removed all references to a mesh       8th January 2016

Works with no problems in DX Seattle   Peter Guth 8 Jan 2016
Add import of random point cloud       Peter Guth 8 Jan 2016

Remove functions, so it only displays GIS daa  Peter Guth 17 Jan 2017
Accepts two command line parameters, so it works with VCL MICRODEM which creates
     the data files and calls the FMX program
Takes two kinds of data; file extension tells program which it is dealing with:
    1.  .XYZB: Any map in MICRODEM with an associated DEM, exported with XYZ points,
           and a drape texture with 2D bitmap (this but not the next could use a mesh, which might not show holes when zooming in)
    2.  .XYXIB: Point cloud, with one 1D bitmap for lidar texture of classification or intensity

Could be added to the open source FMX version of MICRODEM
------------------------------------------------------------------------------}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define Record3d}
      //{$Define ShortRecord}
      //{$Define RecordMoves}
      //{$Define Record3dDetailed}
   {$EndIf}
{$EndIf}


interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,  System.RTLConsts,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects3D, FMX.Types3D,
  FMX.Layouts, FMX.Layers3D, System.Math.Vectors, FMX.Controls3D,
  FMX.MaterialSources, FMX.StdCtrls,FMX.ListBox,
  FMX.Forms3D, FMX.Graphics, System.UIConsts, FMX.Controls.Presentation,
  DEMMapDraw,Petmar_types,Petmar_db;

const
   ViewerVersion = ' v.32';
   MaxClouds = 5;

type
  TView3DForm = class(TForm3D)
    Camera1: TCamera;
    Light1: TLight;
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
    GroupBox4: TGroupBox;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
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
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure Layout3D1Render(Sender: TObject; Context: TContext3D);
    procedure Button6Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DActivate(Sender: TObject);
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
    procedure CheckBox6Change(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CornerButton16Click(Sender: TObject);
    //procedure Form3DMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure CornerButton15Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
  public
     xRange,yRange,zRange,
     ve1,ye1,rMinX,rMaxX,rMinY,rMaxY : double;
     rMinZ,rMaxZ : float32;
     DrapeFile : array[1..MaxClouds] of shortstring;
     MouseIsDown,FirstRun,LinkZScaling,Grayscale : boolean;
     CurCloud,ExtraPoints : integer;
     NPtsUsed,NPtsAllocated  : array[1..MaxClouds] of integer;
     Material : array[1..MaxClouds] of TTextureMaterialSource;  // Texture to be used, probably need to make this an array as well
     VertexBuffer : array[1..MaxClouds] of tVertexBuffer;       // Vertex buffer for points
     IndexBuffer : array[1..MaxClouds] of TIndexBuffer;         // Index buffer to point to verticies
     ShowCloud : array[1..MaxClouds] of boolean;
     CloudName : array[1..MaxClouds] of ShortString;
     procedure SetScaling;
     function ScaledX(inX : double) : single; inLine;
     function ScaledY(inY : double) : single; inLine;
     function ScaledZ(inZ : double) : single; inLine;
     procedure AddPointWithTextureBitmap(x,y,z : double); inline;
     procedure AddPointWithSpecificColor(x,y,z : double; xc,yc : integer); inline;
     procedure SaveVertex(x,y,z : double); inline;
     procedure GeneratePoints(PointsFile,DrapeFile : PathStr);
     procedure Initialize(PointsToAllocate : integer);
     procedure ScaleViewToMapExtent(MapDraw : tMapDraw);
     procedure ArrangePanels;
     procedure DoMap(aMapDraw : tMapDraw);    //; UseElevs : integer);
  end;


const
   MaxX = 30;
var
   View3DForm : TView3DForm;
   Map3D      : TView3DForm;


function MapTo3DView(MapDraw : tMapDraw;  ExtraPoints : integer = 0) : TView3DForm;

function SeismicTo3DView : TView3DForm;
procedure StartSeismicViewing;
procedure FMX3dViewer(ViewSeveral : boolean; GridName1,GridName2,GridName3,GridName4,GridName5,TextureName1,TextureName2,TextureName3,TextureName4,TextureName5 : PathStr; LinkZScaling : boolean = true);


implementation

{$R *.fmx}

uses
   {$IfDef VCL}
      nevadia_main,
   {$EndIf}
   Petmar,PetImage,PetMath,Math,DEMDefs,DEMCoord, New_Petmar_Movie,
   BaseMap;

var
  Down : TPointF;


      procedure TView3DForm.DoMap(aMapDraw : tMapDraw);
      var
         x,y,Pts : integer;
         Lat,Long,xu,yu,xgrid,ygrid : float64;
         z : float32;
         Good : boolean;
      begin
         if (CurCloud = MaxClouds) then begin
            MessageToContinue('Limit is ' + IntToStr(MaxClouds));
            exit;
         end;

         {$IfDef VCL} StartProgress('Export '+ aMapDraw.BaseTitle); {$EndIf}
         Pts := aMapDraw.MapXSize * aMapDraw.MapYSize + ExtraPoints;
         if (Pts >= MaxPts) then Pts := MaxPts;
         DrapeFile[succ(CurCloud)] := aMapDraw.FullMapfName;
         Initialize(Pts);

         for x := 0 to pred(aMapDraw.MapXSize) do begin
            {$IfDef VCL} if (x mod 100 = 0) then UpDateProgressBar(x/aMapDraw.MapXSize); {$EndIf}
            for y := 0 to pred(aMapDraw.MapYSize) do begin
               if (aMapDraw.DEM2OnMap <> 0) then begin
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
         {$IfDef VCL} EndProgress; {$EndIf}
         if (CurCloud > 1) then begin
            ShowDataSetsToPickGroupBox2.Visible := true;
            Button3.Visible := true;
            CheckBox2.Visible := true;
            CheckBox3.Visible := CurCloud >= 2;
            CheckBox4.Visible := CurCloud >= 3;
            CheckBox5.Visible := CurCloud >= 4;
            CheckBox6.Visible := CurCloud >= 5;
         end;
         case CurCloud of
            1 : CheckBox2.Text := aMapDraw.BaseTitle;
            2 : CheckBox3.Text := aMapDraw.BaseTitle;
            3 : CheckBox4.Text := aMapDraw.BaseTitle;
            4 : CheckBox5.Text := aMapDraw.BaseTitle;
            5 : CheckBox6.Text := aMapDraw.BaseTitle;
         end;

         ArrangePanels;
         Show;
         {$IfDef Record3d} WriteLineToDebugFile('TView3DForm DoMap, CurCloud=' + IntToStr(CurCloud) + '  ' + aMapDraw.BaseTitle); {$EndIf}
      end;


function MapTo3DView(MapDraw : tMapDraw;  ExtraPoints : integer = 0) : TView3DForm;
begin
   {$IfDef Record3d} WriteLineToDebugFile('MapTo3DView in, ' + MapDraw.BaseTitle); {$EndIf}
   Result := TView3DForm.Create(Application);
   Result.ShowDataSetsToPickGroupBox2.Visible := false;
   Result.Button3.Visible := false;
   Result.ExtraPoints := ExtraPoints;
   Result.ScaleViewToMapExtent(MapDraw);
   Result.DoMap(MapDraw);
   {$IfDef Record3d} WriteLineToDebugFile('MapTo3DView out'); {$EndIf}
end;


procedure FMX3dViewer(ViewSeveral : boolean; GridName1,GridName2,GridName3,GridName4,GridName5,TextureName1,TextureName2,TextureName3,TextureName4,TextureName5 : PathStr; LinkZScaling : boolean = true);
var
   NumMultiples,NumSingle : integer;

         procedure DoFile(GridName,TextureName : PathStr; CheckBox : tCheckBox);
         begin
            if (GridName <> '') then begin
               {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('OK, fname=' + GridName); {$EndIf}
               View3DForm.GeneratePoints(GridName,TextureName);
               {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('Points generated'); {$EndIf}
               CheckBox.Visible := true;
               if ViewSeveral then begin
                  CheckBox.Text := ExtractFileNameNoExt(GridName);
                  inc(NumMultiples);
               end
               else begin
                  {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('ListBox1.Items.Add ' + ExtractFileName(GridName)); {$EndIf}
                  View3DForm.ListBox1.Items.Add(ExtractFileNameNoExt(GridName));
                  View3DForm.ListBox1.ItemIndex := -1;
                  inc(NumSingle);
               end;
            end
            else begin
               CheckBox.Visible := false;
            end;
         end;


begin
   {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('FMX3dViewer in ' + ExtractFileName(GridName1)); {$EndIf}
   View3DForm := TView3DForm.Create(Application);
   View3DForm.LinkZScaling := LinkZScaling;
   View3DForm.ShowDataSetsToPickGroupBox2.Visible := ViewSeveral;
   NumMultiples := 0;
   NumSingle := 0;

   View3DForm.DrapeFile[1] := TextureName1;
   View3DForm.DrapeFile[2] := TextureName2;
   View3DForm.DrapeFile[3] := TextureName3;
   View3DForm.DrapeFile[4] := TextureName4;
   View3DForm.DrapeFile[5] := TextureName5;
   DoFile(GridName1,TextureName1,View3DForm.CheckBox2);
   DoFile(GridName2,TextureName2,View3DForm.CheckBox3);
   DoFile(GridName3,TextureName3,View3DForm.CheckBox4);
   DoFile(GridName4,TextureName4,View3DForm.CheckBox5);
   DoFile(GridName5,TextureName5,View3DForm.CheckBox6);
   View3DForm.ListBox1.Visible := (NumSingle > 1);

   View3DForm.Button3.Visible := NumSingle > 1;
   {$If Defined(Record3D) or Defined(ShortRecord)} WriteLineToDebugFile('call View3DForm.Show'); {$EndIf}
   View3DForm.ArrangePanels;
   {$IfDef VCL} wmDem.SetMenusForVersion; {$EndIf}
   {$If Defined(Record3D) or Defined(ShortRecord)} WriteLineToDebugFile('FMX3dViewer out, window at ' + IntToStr(View3dForm.Left) + 'x' + IntToStr(View3dForm.Top)); {$EndIf}
end;


procedure StartSeismicViewing;
begin
   {$IfDef VCL} StopSplashing; {$EndIf}
   SeismicTo3DView;
end;


function SeismicTo3DView  : TView3DForm;
var
   Table : tMyData;
   fName : PathStr;

   procedure SetUpPanel(CheckBox : tCheckBox; var thePlane : tPlane; tms : TTextureMaterialSource);
   var
      aName : NameStr;
      bmp   : tMyBitmap;
      fName : PathStr;
   begin
      if Table.eof then exit;
      FName := Table.GetFieldByNameAsString('FILENAME');
      if (fName <> '') then begin
         fName := ExtractFilePath(Table.FullTableName) + fName;
         aName := ExtractFileNameNoExt(fName);
         bmp := PetImage.LoadBitmapFromFile(fName);
         Result.ComboBox1.Items.Add(aName);
         CheckBox.Text := aName;
         ThePlane.Visible := true;
         ThePlane.Scale.X := 10;
         ThePlane.Scale.Y := 10;
         ThePlane.Scale.Z := 10;
         ThePlane.Width := 15 * bmp.Width/1500;
         ThePlane.Height := 15 * bmp.Height / 1500;
         ThePlane.TwoSide := true;
         tms := TTextureMaterialSource.Create(Result);
         tms.Texture.LoadFromFile(fName);
         thePlane.MaterialSource := tms;
         ThePlane.Position.x := Table.GetFieldByNameAsFLOAT('X');
         ThePlane.Position.y := Table.GetFieldByNameAsFLOAT('Y');
         ThePlane.Position.z := Table.GetFieldByNameAsFLOAT('Z');
         ThePlane.RotationAngle.Y := Table.GetFieldByNameAsFLOAT('Z_ROTATE');
         bmp.Free;
      end
      else begin
         CheckBox.Visible := false;
      end;
      Table.Next;
   end;

begin
   Result := TView3DForm.Create(Application);
   fName :=  'C:\mapdata\ca_offshore_v4\fence_102_103_200_202.dbf';
   if not FileExists(fName) then begin
      if not GetFileFromDirectory('Fence diagram',DefaultDBExt,fName) then exit;
   end;

   Table := tMyData.Create(fName);

   SetUpPanel(Result.CheckBox2,Result.Plane1,Result.TextureMaterialSource1);     //103
   SetUpPanel(Result.CheckBox3,Result.Plane2,Result.TextureMaterialSource2);     //202, offshore, far
   SetUpPanel(Result.CheckBox4,Result.Plane3,Result.TextureMaterialSource3);     //102
   SetUpPanel(Result.CheckBox5,Result.Plane4,Result.TextureMaterialSource4);     //200, offshore, close
   Table.Destroy;

   Result.CheckBox6.Visible := false;

   Result.ComboBox1.ItemIndex := 0;
   Result.Camera.Position.Y := -35;
   Result.Camera.Position.z := -35;
   Result.ArrangePanels;
   MDDef.OGLDefs.MoveIncr := 1;
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
      //Petimage.MakeMovie(ExtractFileName(MovieName));
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
   fname,MovieName : PathStr;
   MovieFiles : tstringlist;
   i : integer;


    procedure SaveFrame;
    begin
         PetImage.SaveScreenCapture(round(Layer3D1.Width),fName,false,true);
         MovieFiles.Add(ExtractFileName(fName));
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


begin
   {$IfDef VCL}
      MovieFiles := tStringList.Create;

      if ShowDataSetsToPickGroupBox2.Visible then begin
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
      end
      else begin
         for i := 0 to pred(ListBox1.Items.Count) do begin
            ListBox1.ItemIndex := i;
            ListBox1Change(Sender);
            SaveFrame;
         end;
      end;
      MovieName := MovieDir + 'oglmovie2.mov';
      MovieFiles.SaveToFile(MovieName);
      MovieFiles.Free;
      //Petimage.MakeMovie(ExtractFileName(MovieName));
      CreateNewMovie(MovieName);
   {$EndIf}
end;

procedure TView3DForm.Button4Click(Sender: TObject);
var
   fname : PathStr;
begin
   {$IfDef VCL}
      fName := '';
      PetImage.SaveScreenCapture(round(Layer3D1.Width),fName);
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
   ShowCloud[1] := ListBox1.ItemIndex = 0;
   ShowCloud[2] := ListBox1.ItemIndex = 1;
   ShowCloud[3] := ListBox1.ItemIndex = 2;
   ShowCloud[4] := ListBox1.ItemIndex = 3;
   ShowCloud[5] := ListBox1.ItemIndex = 4;
end;


procedure TView3DForm.CheckBox2Change(Sender: TObject);
begin
   Plane1.Visible := CheckBox2.IsChecked;
   ShowCloud[1] := CheckBox2.IsChecked;
end;

procedure TView3DForm.CheckBox3Change(Sender: TObject);
begin
   Plane2.Visible := CheckBox3.IsChecked;
   ShowCloud[2] := CheckBox3.IsChecked;
end;

procedure TView3DForm.CheckBox4Change(Sender: TObject);
begin
   Plane3.Visible := CheckBox4.IsChecked;
   ShowCloud[3] := CheckBox4.IsChecked;
end;

procedure TView3DForm.CheckBox5Change(Sender: TObject);
begin
   Plane4.Visible := CheckBox5.IsChecked;
   ShowCloud[4] := CheckBox5.IsChecked;
end;

procedure TView3DForm.CheckBox6Change(Sender: TObject);
begin
   //Plane5.Visible := CheckBox6.IsChecked;  //currently there is not Plane5
   ShowCloud[5] := CheckBox6.IsChecked;
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
      {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('tView3DForm.ScaleViewToMapExtent option 1'); {$EndIf}
      rMinX := MapDraw.MapCorners.BoundBoxProj.xmin;
      rMaxX := MapDraw.MapCorners.BoundBoxProj.xmax;
      rMinY := MapDraw.MapCorners.BoundBoxProj.ymin;
      rMaxY := MapDraw.MapCorners.BoundBoxProj.ymax;
   end
   else begin
      {$If Defined(Record3D) or Defined(ShortRecord)} writeLineToDebugFile('tView3DForm.ScaleViewToMapExtent option 2'); {$EndIf}
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


(*
   if MapDraw.DEMMap then begin
      rMinZ := MapDraw.MinMapElev;
      rMaxZ := MapDraw.MaxMapElev;
   end
   else if (MapDraw.DEMonMap <> 0) then begin
      rMinZ := DEMGlb[MapDraw.DEMonMap].DEMheader.MinElev;
      rMaxZ := DEMGlb[MapDraw.DEMonMap].DEMheader.MaxElev;
   end;
   *)
end;


procedure tView3DForm.SetScaling;
begin
   xRange := (rMaxx - rMinx);
   yRange := (rMaxy - rMiny);
   zRange := (rMaxz - rMinz);
   if LinkZScaling then ve1 := zRange / xRange
   else ve1 := 1;
   ye1 := yRange / xRange;
   {$IfDef Record3d}
      WriteLineToDebugFile('xRange=' + RealToString(rminX,12,2) + ' to' +  RealToString(rmaxX,12,2) + '  yRange=' + RealToString(rminY,12,2) + ' to' +  RealToString(rmaxY,12,2));
      WriteLineToDebugFile('zRange=' + RealToString(rminZ,12,2) + ' to' +  RealToString(rmaxZ,12,2) + '  vel=' + RealToString(ve1,12,4) + '   yel=' + RealToString(ye1,12,4));
   {$EndIf}
end;


procedure TView3DForm.Timer1Timer(Sender: TObject);
begin
   Label3.Text := 'Rot: ' + RealToString(Layout3D1.RotationAngle.X,-6,1) + ' ' + RealToString(Layout3D1.RotationAngle.Z,-6,1) + ' ' + RealToString(Layout3D1.RotationAngle.Y,-6,1);
   Label4.Text := 'Pos: ' + RealToString(Layout3D1.Position.X,-6,1) + ' ' + RealToString(Layout3D1.Position.Z,-6,1) + ' ' + RealToString(Layout3D1.Position.Y,-6,1);
   Label5.Text := 'Cam: ' + RealToString(Camera1.Position.Vector.X,-6,1) + ' ' + RealToString(Camera1.Position.Vector.Z,-6,1) + ' ' +  RealToString(Camera1.Position.Vector.Y,-6,1);
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

function tView3DForm.ScaledX(inX : double) : single;
begin
   Result := -1.0 * MaxX + (inx - rMinX) / XRange * 2 * MaxX;
end;

function tView3DForm.ScaledY(inY : double) : single;
begin
   Result := -1.0 * MaxX + ye1 * (inY - rMinY) / yRange * 2 * MaxX;
end;

function tView3DForm.ScaledZ(inZ : double) : single;
begin
   Result := 1.0 * MaxX - Ve1 * (inz - rMinz) / ZRange * 2 * MaxX;
end;


procedure TView3DForm.SaveVertex(x,y,z : double);
var
   P : array [0..0] of TPoint3D;
begin
   P[0].x := ScaledX(x);
   P[0].z := ScaledY(y);
   P[0].y := ScaledZ(z);
   VertexBuffer[CurCloud].Vertices[NPtsUsed[CurCloud]] := P[0]*25;   // Save vertex
   inc(NPtsUsed[CurCloud]);
end;


procedure TView3DForm.AddPointWithTextureBitmap(x,y,z : double);
begin
   VertexBuffer[CurCloud].TexCoord0[NPtsUsed[CurCloud]] := PointF((x - rMinx)/ (XRange),1-(y - rMiny)/ (YRange));
   IndexBuffer[CurCloud][NPtsUsed[CurCloud]] := NPtsUsed[CurCloud];                                // Set index to the vertex
   SaveVertex(x,y,z);
end;


procedure TView3DForm.AddPointWithSpecificColor(x,y,z : double; xc,yc : integer);
begin
   VertexBuffer[CurCloud].TexCoord0[NPtsUsed[CurCloud]] := System.Types.PointF(0,0);
   IndexBuffer[CurCloud][NPtsUsed[CurCloud]] := NPtsUsed[CurCloud];                                // Set index to the vertex
   SaveVertex(x,y,z);
end;


procedure TView3DForm.Initialize(PointsToAllocate : integer);
var
   k : integer;
   BMP : TBitmap;
   Data : tBitmapData;
begin
   {$IfDef Record3d} writeLineToDebugFile('TView3DForm.Initialize in'); {$EndIf}
   if (CurCloud = MaxClouds) then exit;
   inc(CurCloud);
   NPtsAllocated[CurCloud] := PointsToAllocate;
   ShowCloud[CurCloud] := true;
   NPtsUsed[CurCloud] := 0;
   {$IfDef Record3d} WriteLineToDebugFile('Cloud=' + IntToStr(CurCloud) + ' can have pts=' + IntToStr(PointsToAllocate)); {$EndIf}
   //##################### Create vertex and index buffers ###################
   // Create vertex buffer with vertex data and a texture value for each vertex
   // The second parameter is the number of verticies
     VertexBuffer[CurCloud] := TVertexBuffer.Create([tVertexFormat.Vertex,tVertexFormat.TexCoord0],PointsToAllocate);
   // Create index buffer, paramter 1 is the number of indexes, the second
   // is the size of the the data type (32 bits)
   {$IfDef Record3d} WriteLineToDebugFile('TVertexBuffer.Create done'); {$EndIf}

   IndexBuffer[CurCloud] := TIndexBuffer.Create(PointsToAllocate,tIndexFormat.UInt32);      //## Points
   {$IfDef Record3d} WriteLineToDebugFile('TIndexBuffer.Create done'); {$EndIf}

   //--------------------- Create the texture ------------------------------------

   //##################### Set up the texture to be used #########################
   Material[CurCloud] := TTextureMaterialSource.Create(Self);

   if (DrapeFile[CurCloud] = '') then begin
      //##################### Create the bitmap that will be used as the texture map
      //simple color ramp
           BMP := TBitmap.Create(1,360);
           BMP.Map(tMapAccess.ReadWrite,Data) ;    // Set read/write access to the bitmap

         //##################### Populate the bitmap with the texture values ###########
           for k := 0 to 359 do Begin
             Data.SetPixel(0,k,CorrectColor(HSLtoRGB(k/360,0.75,0.5)));
           End;
           BMP.Unmap(Data);
           Material[CurCloud].Texture := BMP;
    end
    else begin
       //can be color ramp, say for lidar intensity with grayscale, or point classification\
       //can be any map displayed in MICRODEM
       Material[CurCloud].Texture.LoadFromFile(DrapeFile[CurCloud]);
    end;
    Caption := '3D viewer' + ViewerVersion + ' --' + SmartNumberPoints(PointsToAllocate) + ' points';
   {$IfDef Record3d} writeLineToDebugFile('call set scaling'); {$EndIf}
    if (CurCloud = 1) then SetScaling;
   {$IfDef Record3d} WriteLineToDebugFile('TView3DForm.Initialize out with ' + SmartNumberPoints(PointsToAllocate) + ' points'); {$EndIf}
end;



procedure TView3DForm.GeneratePoints;
var
  P : TPoint3D;
  NP,Pts : Integer;
  NeedRange : boolean;
  Color2D : boolean;

      procedure LoadPointsWithFullDrapeMap;
      //PLG code to read real elevation data set
      type
         tPointXYZColor = record
            x,y,z : single;
         end;
         tPointXYZColorArray = array[1..MaxPts] of tPointXYZColor;
      var
         PointXYZColor : ^tPointXYZColorArray;
         tfile : File;
         i : integer;
      begin
           {$IfDef Record3d} WriteLineToDebugFile('LoadPointsWithFullDrapeMap in'); {$EndIf}
           assignFile(tfile,PointsFile);
           reset(tFile,sizeOf(tPointXYZColor));
           new(PointXYZColor);
           BlockRead(tfile,PointXYZColor^,MaxPts,Pts);
           if NeedRange then begin //get range of real world coordinates in data set
              for i := 1 to Pts do begin
                 CompareValueToExtremes(PointXYZColor^[i].x, rMinX,rMaxX );
                 CompareValueToExtremes(PointXYZColor^[i].y, rMinY,rMaxY );
                 CompareValueToExtremes(PointXYZColor^[i].z, rMinZ,rMaxZ );
              end;
              {$IfDef Record3d} WriteLineToDebugFile('LoadPointsWithFullDrapeMap, z range=' + RealToString(rMinz,-12,-2) +  ' to ' + RealToString(rMaxz,-12,-2)); {$EndIf}
           end;

          Initialize(Pts);
          NP := 0;
          for i := 1 to Pts do begin
            //use GIS coordinates with x and y horizontal, and z vertical, which are different from the
            P.x := ScaledX(PointXYZColor^[i].x);
            P.z := ScaledY(PointXYZColor^[i].y);
            P.y := ScaledZ(PointXYZColor^[i].z);
            VertexBuffer[1].Vertices[NP] := P*25;   // Save vertex
            if (DrapeFile[1] = '') then begin  // Set texture value from elevation color ramp
               VertexBuffer[1].TexCoord0[NP] := PointF(0,(PointXYZColor^[i].z - rMinz)/ (ZRange));
            end
            else begin // Set texture value from draped bitmap
               VertexBuffer[1].TexCoord0[NP] := PointF((PointXYZColor^[i].x - rMinx)/ (XRange),1-(PointXYZColor^[i].y - rMiny)/ (YRange));
            end;
            IndexBuffer[1][NP] := NP;      // Set index to vertex
            NP := NP+1;
          end;
          CloseFile(tFile);
          Dispose(PointXYZColor);
          {$IfDef Record3d} WriteLineToDebugFile('LoadPointsWithFullDrapeMap out'); {$EndIf}
      end;

      procedure LoadMapWithColorByCodes;
      //PLG code to read real elevation data set
      var
         PointXYZColor : ^tPointXYZIArray;
         tfile : File;
         i : integer;
         xf,yf : float64;
      begin
           {$IfDef Record3d} WriteLineToDebugFile('LoadMapWithColorByCodes in ' + PointsFile); {$EndIf}
           assignFile(tfile,PointsFile);
           reset(tFile,sizeOf(tPointXYZI));
           new(PointXYZColor);
           BlockRead(tfile,PointXYZColor^,MaxPts,Pts);

           if NeedRange then begin
              //get range of real world coordinates in data set
              for i := 1 to Pts do begin
                 CompareValueToExtremes(PointXYZColor^[i].x, rMinX,rMaxX );
                 CompareValueToExtremes(PointXYZColor^[i].y, rMinY,rMaxY );
                 CompareValueToExtremes(PointXYZColor^[i].z, rMinZ,rMaxZ );
              end;
              SetScaling;
              {$IfDef Record3d}
                 WriteLineToDebugFile('xRange=' + RealToString(rminX,12,2) + ' to' +  RealToString(rmaxX,12,2) + '  yRange=' + RealToString(rminY,12,2) + ' to' +  RealToString(rmaxY,12,2));
                 WriteLineToDebugFile('zRange=' + RealToString(rminZ,12,2) + ' to' +  RealToString(rmaxZ,12,2) );
              {$EndIf}
           end;

          Initialize(Pts);

          NP := 0;
          for i := 1 to Pts do begin
            //we use coordinates with x and y horizontal, and z vertical
            P.x := ScaledX(PointXYZColor^[i].x);
            P.z := ScaledY(PointXYZColor^[i].y);
            P.y := ScaledZ(PointXYZColor^[i].z);
            VertexBuffer[CurCloud].Vertices[NP] := P*25;                   // Save vertex
            if Color2D then begin
               RGBtoXYFloat(PointXYZColor^[i].Int,PointXYZColor^[i].Int2,PointXYZColor^[i].Int3,xf,yf);
               VertexBuffer[CurCloud].TexCoord0[NP] := PointF(xf,yf);
            end
            else VertexBuffer[CurCloud].TexCoord0[NP] := PointF(0,PointXYZColor^[i].Int/255); // Set texture value from color ramp
            IndexBuffer[CurCloud][NP] := NP;                                // Set index to vertex
            NP := NP+1;
          end;
          CloseFile(tFile);
          Dispose(PointXYZColor);
          {$IfDef Record3d} WriteLineToDebugFile('LoadMapWithColorByCodes out'); {$EndIf}
      end;

var
   Ext : shortstring;
begin
   {$IfDef Record3d} WriteLineToDebugFile('TView3DForm.GeneratePoints in, CurCloud=' + IntToStr(CurCloud)); {$EndIf}
   {$IfDef Android}
      Layer3D1.Width := 80;
   {$EndIf}

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
   else NeedRange := false;
   Color2D := (DrapeFile = FullPaletteBitmap);
   Ext := UpperCase(ExtractFileExt(PointsFile));
   if (Ext = '.XYZB') then LoadPointsWithFullDrapeMap
   else if (Ext = '.XYZIB') then LoadMapWithColorByCodes
   else begin
      MessageToContinue('File not supported, ' + PointsFile);
   end;
   NPtsUsed[CurCloud] := Pts;
   {$IfDef Record3d} WriteLineToDebugFile('TView3DForm.GeneratePoints out, pts=' + IntToStr(NPtsUsed[CurCloud])); {$EndIf}
end;

{#############################################################################
   Code renders all of the points. Pretty simple!!!
#############################################################################}
procedure TView3DForm.Layout3D1Render(Sender: TObject; Context: TContext3D);
var
   i : integer;
begin
   {$IfDef Record3dDetailed} WriteLineToDebugFile('TView3DForm.Layout3D1Render in'); {$EndIf}
   for i := MaxClouds downto 1 do begin
      if (NPtsUsed[i] > 0) and ShowCloud[i] then begin
         {$IfDef Record3dDetailed} WriteLineToDebugFile('Draw cloud ' + IntToStr(i)); {$EndIf}
         Context.DrawPoints(VertexBuffer[i],IndexBuffer[i],Material[i].Material,1);
      end;
   end;
   {$IfDef Record3dDetailed} WriteLineToDebugFile('TView3DForm.Layout3D1Render out'); {$EndIf}
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

procedure TView3DForm.Button6Click(Sender: TObject);
begin
   DrapeFile[1] := 'c:\temp\temp_cloud_1.bmp';
   GeneratePoints('c:\temp\vasa.xyzib',DrapeFile[1]);
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

procedure TView3DForm.Form3DActivate(Sender: TObject);
begin
   if FirstRun then begin
      FirstRun := false;
      DrapeFile[1] := ParamStr(2);
      GeneratePoints(ParamStr(1),DrapeFile[1]);
   end;
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
   FirstRun := true;
   MouseIsDown := false;
   LinkZScaling := true;
   MDDef.OGLDefs.MoveIncr := 7.5;

   Top := MDDef.OGLDefs.OpenGLDefaultTopX;
   Left := MDDef.OGLDefs.OpenGLDefaultTopY;
   Height := MDDef.OGLDefs.OpenGLDefaultHeight;
   Width := MDDef.OGLDefs.OpenGLDefaultWidth;

   CurCloud := 0;
   for I := 1 to MaxClouds do begin
     ShowCloud[i] := false;
     NPtsAllocated[i] := 0;
     NPtsUsed[i] := 0;
   end;

   GroupBox6.Visible := false;
   GroupBox7.Visible := false;
   GroupBox4.Visible := false;

   Label3.Visible := false;
   Label4.Visible := false;
   Label5.Visible := false;
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
   Down := PointF(X, Y);
   MouseIsDown := true;
end;

procedure TView3DForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   Down := PointF(X, Y);
   MouseIsDown := false;
end;


procedure TView3DForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
begin
   if false and MouseIsDown {(ssLeft in Shift))} then begin
      Layout3D1.RotationAngle.X := Layout3D1.RotationAngle.X - ((Y - Down.Y) * 0.3);
      Layout3D1.RotationAngle.Y := Layout3D1.RotationAngle.Y + ((X - Down.X) * 0.3);
      Down := PointF(X, Y);
   end;
end;


procedure TView3DForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
   Camera1.Position.Vector := Camera1.Position.Vector + Vector3D(0, 0, 1) * ((WheelDelta / 120) * 0.3)
end;


initialization
   View3dForm := nil;
   Map3d := Nil;
finalization
   {$IfDef Record3d} WriteLineToDebugFile('Record3d active in view3d_main'); {$EndIf}
   {$IfDef Record3dDetailed} WriteLineToDebugFile('Record3dDetailed active in view3d_main'); {$EndIf}
end.



