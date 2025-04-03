unit slicer_3d;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IfDef DEBUG}
      //{$Define NoSetColorOptions}
      //{$Define NoArrangeControls}
      //{$Define Slicer}
      //{$Define SliceCenter}
      //{$Define SlicePlot}
      //{$Define SliceControls}
      //{$Define TimeSlicer}
      //{$Define CloseSlicer}
      //{$Define SlicerDetailedDraw}
      //{$Define SlicerEdits}
      //{$Define SlicerOpenGL}
      //{$Define SlicerRange}
      //{$Define SlicerProblemsFull}   //major slowdown
      //{$Define SlicerBoxOutline}
   {$EndIf}
{$EndIf}


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

  {$IfDef ExFMX3D}
  {$Else}
     FMX.Types3D,
  {$EndIf}

  System.Math,System.UItypes,System.Math.Vectors,System.Types,System.RTLConsts,
  Vcl.StdCtrls,Vcl.Controls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Windows, Messages, SysUtils, Classes, Graphics,  Forms, Buttons,StrUtils,
  demdatabase,BaseGraf,Petmar_types,DEMMapf, DEMLOSW,point_cloud_memory,DEMDefs;

const
   MaxGISGraph = 5;
   MaxSliceClouds = 5;
type
   tSliceGraph = record
      GISGraf : tThisBaseGraph;
      ArtDBFilter : shortString;
      ArtDBShow : byte;
      SliceThickness,SliceCenter,SliceHi,SliceLo,gxlo,gxhi,gylo,gyhi : float64;
   end;

type
  TSlicerForm = class(TForm)
    BitBtn1: TBitBtn;
    RadioGroup1: TRadioGroup;
    BitBtn2: TBitBtn;
    HelpBtn: TBitBtn;
    RadioGroup2: TRadioGroup;
    BitBtn16: TBitBtn;
    BitBtn11: TBitBtn;
    RadioGroup4: TRadioGroup;
    CloudPickGroupBox1: TGroupBox;
    CheckBoxCloud1: TCheckBox;
    CheckBoxCloud2: TCheckBox;
    CheckBoxCloud3: TCheckBox;
    CheckBoxCloud4: TCheckBox;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Edit9: TEdit;
    Label2: TLabel;
    Label5: TLabel;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    SpeedButton1: TSpeedButton;
    Edit10: TEdit;
    SpeedButton2: TSpeedButton;
    GroupBox3: TGroupBox;
    Wudth: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    RadioGroup3: TRadioGroup;
    CheckBox7: TCheckBox;
    BitBtn42: TBitBtn;
    BitBtn43: TBitBtn;
    BitBtn44: TBitBtn;
    BitBtn45: TBitBtn;
    CheckBoxCorrectScaling1: TCheckBox;
    CheckBoxCloud5: TCheckBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure Edit10Change(Sender: TObject);
    procedure CheckBoxCloud1Click(Sender: TObject);
    procedure CheckBoxCloud2Click(Sender: TObject);
    procedure CheckBoxCloud3Click(Sender: TObject);
    procedure CheckBoxCloud4Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    //procedure Edit6Change(Sender: TObject);
    procedure BitBtn42Click(Sender: TObject);
    procedure BitBtn43Click(Sender: TObject);
    procedure BitBtn44Click(Sender: TObject);
    procedure BitBtn45Click(Sender: TObject);
    procedure CheckBoxCorrectScaling1Click(Sender: TObject);
    procedure CheckBoxCloud5Click(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeSliceCenter(Redraw : boolean);
    procedure SetSliceSettings;
    procedure GetRangesInXYZ;
    procedure CreateNewGraph;
    procedure SymbolXZPlane(x, z: float64; Color: tPlatformColor);
    procedure SingleYLine(x : float64; Color : tPlatformColor; Width : integer);
    {$IfDef IncludeVASAgis}
       procedure TryToGraphDatabase(bmp : tBitmap; i : integer);
    {$EndIf}
    procedure Set3DFilter(db : integer; xlo,xhi,ylo,yhi : float64);
    procedure DrawOneCloud(var bmp : tMyBitmap);
    procedure SetColorOptions;
    procedure ArrangeControls;
    procedure AdjustForLatLong;
   {$IfDef SlicerRange}
      procedure ShowDataRanges;
   {$EndIf}

  public
    { Public declarations }
    SliceGraph : array[1..MaxGISGraph] of tSliceGraph;
    PolarGraph : TThisBaseGraph;
    CoordDec,
    CurSlice,CloudInUse : integer;
    xlo,ylo,zlo,xhi,yhi,zhi : float64;
    ThisProject : PathStr;
    SlicerMapOwner : tMapForm;
    EditTable,Edit2Table : tMyData;
    EditResults : tStringList;
    EditName,Edit2Name : ShortString;
    LOSOwner : TDEMLOSF;
    XYZGIS : integer;
    {$IfDef IncludeVASAgis}
       GIS : TGISdataBaseModule;
       BoxOutlineGIS,ArtefactGIS : integer;
       EditGISDBs : array[1..15] of integer;
    {$EndIf}
    BaseName : shortString;
    SliceLoc : string[2];
    SlicePlane : string[3];
    LatLongCoords,NeedLegend,MatchMapSize,
    OpenGLAllPoints,SingleDBFFile,ReadyToRoll : boolean;

    MemoryPointCloud : array[1..MaxSliceClouds] of Point_Cloud_Memory.tMemoryPointCloud;
    CloudUsed : array[1..MaxSliceClouds] of boolean;
    CloudColorOpt : array[1..MaxSliceClouds] of tSliceColorOpt;

    procedure ZShift(NewX, NewZ: float64);
    procedure LoadMemoryPointCloud(Cloud : integer; fName : PathStr; XField : shortstring = 'X'; YField : shortstring = 'Y'; ZField : shortstring = 'Z'; InCloudName : shortstring = ''; inStayLatLong : boolean = false);
    procedure MaskPoints(x,y : float64);
    procedure DigitizePoint(x,y : float64);
    procedure SliceAtUTM(XUTM1,YUTM1 : float64);
    procedure SlicerPanorama(Lat,Long,z : float64);
    procedure RedrawSlicerPanorama(var bmp : tMyBitmap);
    procedure PointsAtLatLong(Lat,Long : float64);
    procedure MaskBoxPoints(xlo,xhi,ylo,yhi : float64);
    procedure DigitizeYAxis(NewX, NewY : float64);
    procedure RedrawClouds;
  end;


procedure DB_3dSlices(inMap : tMapForm; inLOS : TDEMLOSF; GIS : TGISdataBaseModule; XField : shortstring = 'X'; YField : shortstring = 'Y'; ZField : shortstring = 'Z'; InCloud : shortstring = ''; InLatLong : boolean = false); overload;
procedure DB_3dSlices(inMapOwner : tMapForm; inMemoryPointCloud : Point_Cloud_Memory.tMemoryPointCloud); overload;


const
   UseMemoryPointCloud = true;

var
  SlicerForm : TSlicerForm;


implementation

uses
   {$IfDef ExSat}
   {$Else}
      DEMEros,
   {$EndIf}

   {$IfDef ExFMX3D}
   {$Else}
      View3D_main,
   {$EndIf}

   Nevadia_Main,
   DEMDef_routines,
   DEM_Manager,
   DEMCoord,
   Color_Filter,
   DEMStat,
   Petimage_form,
   Thread_Timers,PetImage,PetMath,
   petmar, DEMESRIShapeFile,PetDBUtils, toggle_db_use,Make_tables,
   point_cloud_options,las_lidar;


{$R *.dfm}

const
   Slice_xy = 0;
   Slice_xz = 1;
   Slice_yz = 2;


procedure DB_3dSlices(inMapOwner : tMapForm; inMemoryPointCloud : Point_Cloud_Memory.tMemoryPointCloud); overload;
begin
   {$IfDef Slicer} WriteLineToDebugFile('DB_3dSlices starting '); {$EndIf}
   SlicerForm := TSlicerForm.Create(Application);
   SlicerForm.SlicerMapOwner := inMapOwner;
   SlicerForm.MemoryPointCloud[1] := inMemoryPointCloud;
   SlicerForm.LatLongCoords := SlicerForm.MemoryPointCloud[1].StayLatLong;
   SlicerForm.LoadMemoryPointCloud(1,'');
   {$IfDef Slicer} WriteLineToDebugFile('DB_3dSlices, point cloud loaded, center changed'); {$EndIf}
   SlicerForm.Caption := 'Slice viewer point cloud: ' + ExtractFileNameNoExt(SlicerForm.ThisProject) + ' pts=' + SmartNumberPoints(SlicerForm.MemoryPointCloud[1].NumMemPts);
   SlicerForm.AdjustForLatLong;
   SlicerForm.Show;
   SlicerForm.ChangeSliceCenter(True);
   {$IfDef Slicer} WriteLineToDebugFile('DB_3dSlices, out'); {$EndIf}
end;


procedure DB_3dSlices(inMap : tMapForm; inLOS : TDEMLOSF; GIS : TGISdataBaseModule; XField : shortstring = 'X'; YField : shortstring = 'Y'; ZField : shortstring = 'Z'; InCloud : shortstring = ''; InLatLong : boolean = false);
var
  ProjectTable : tMyData;
  fName : PathStr;
  GISNum : integer;
  Ext : ExtStr;

      procedure LoadAProject;
      begin
         SlicerForm.Basename := ExtractFilenameNoExt(SlicerForm.ThisProject);
         {$IfDef Slicer} WriteLinetoDebugFile('Starting up ' + SlicerForm.BaseName); {$EndIf}
         SlicerForm.GetRangesInXYZ;
         SlicerForm.SetSliceSettings;
         SaveBackupDefaults;

         if SlicerForm.SingleDBFFile then begin
            {$IfDef Slicer} WriteLineToDebugFile('SlicerForm.SingleDBFFile'); {$EndIf}
            if OpenNumberedGISDataBase(GISNum ,SlicerForm.ThisProject) then begin
               fName := ChangeFileExt(SlicerForm.ThisProject,'.shp');
               if not FileExists(fname) then GISdb[GISNum].SavePointShapeFile;
               DEMDefs.VasaProjectFName := fName;
               CloseAndNilNumberedDB(GISNum);
               SlicerForm.BitBtn2Click(Nil);
               SlicerForm.Close;
            end;
         end
         else begin
            {$IfDef ExSat}
            {$Else}
               ProjectTable := tMyData.Create(SlicerForm.ThisProject);
               ProjectTable.ApplyFilter( 'USE=' + QuotedStr('Y') + ' AND TYPE=' + QuotedStr('image'));
               while not ProjectTable.Eof do begin
                  OpenAndDisplaySatelliteScene(Nil,ProjectTable.GetFieldByNameAsString('FILENAME'),true,false,true);
                  ProjectTable.Next;
               end;
               FreeAndNil(ProjectTable);
            {$EndIf}
         end;
         SlicerForm.Caption := 'Slicer view: ' + SlicerForm.BaseName + ' pts=' + SmartNumberPoints(SlicerForm.MemoryPointCloud[1].NumMemPts);
         SlicerForm.BitBtn1Click(Nil);
         RestoreBackupDefaults;
      end;


begin
   {$IfDef Slicer} WriteLinetoDebugFile('DB_3dSlices in ' + DEMDefs.VasaProjectFName); {$EndIf}
   SlicerForm := TSlicerForm.Create(Application);
   {$IfDef IncludeVASAgis}
      if (GIS <> Nil) then begin
         SlicerForm.Gis := GIS;
         GIS.EmpSource.Enabled := false;
         SlicerForm.BaseName := GIS.dbName;
      end;
   {$EndIf}

   with SlicerForm do begin
     {$IfDef Slicer} WriteLinetoDebugFile('ThisProject=' + SlicerForm.ThisProject); {$EndIf}
      SlicerMapOwner := inMap;
      BitBtn16.Enabled := (inMap <> Nil);
      LOSOwner := inLOS;
      SlicerForm.LatLongCoords := InLatLong;

      Ext := ExtractFileExt(ThisProject);

      if (UpperCase(Copy(ExtractFileName(ThisProject),1,4)) = 'PROJ') and ExtEquals(Ext, DefaultDBExt) then begin
         LoadAProject;
      end
      else if ExtEquals(Ext, '.LAS') or ExtEquals(Ext,'.SHP') or ExtEquals(Ext, DefaultDBExt) or ExtEquals(Ext,'.CSV') or ExtEquals(Ext,'.TXT') then begin
         {$IfDef Slicer} WriteLineToDebugFile('Calling LoadMemoryPointClound with ' + ExtractFilePath(SlicerForm.ThisProject)); {$EndIf}
         BaseName := ExtractFileNameNoExt(ThisProject);
         LoadMemoryPointCloud(1,ThisProject,XField,YField,ZField,InCloud,InLatLong);
         xlo := MemoryPointCloud[1].pcMinX;
         xhi := MemoryPointCloud[1].pcMaxX;
         ylo := MemoryPointCloud[1].pcMinY;
         yhi := MemoryPointCloud[1].pcMaxY;
         zlo := MemoryPointCloud[1].pcMinZ;
         zhi := MemoryPointCloud[1].pcMaxZ;

         {$IfDef SlicerRange} ShowDataRanges; {$EndIf}

         ChangeSliceCenter(false);
         Caption := ExtractFileNameNoExt(ThisProject) + ' pts=' + SmartNumberPoints(MemoryPointCloud[1].NumMemPts);
         Show;
      end
      else begin
         {$IfDef Slicer} WriteLineToDebugFile('DB_3dSlices call BitBtn2Click'); {$EndIf}
         BaseName := '';
         BitBtn2Click(Nil);
         Close;
      end;
   end;

   if MDDef.AutoShowSlice then begin
      SlicerForm.RadioGroup2.ItemIndex := 0;
   end;
   SlicerForm.AdjustForLatLong;
   {$IfDef Slicer} WriteLineToDebugFile('DB_3dSlices out'); {$EndIf}
end;


function MakeSlicerFilter3D(xlo,xhi,ylo,yhi,zlo,zhi : float64) : string;
begin
   Result := 'X>=' + RealToString(xlo,-12,-6) + ' AND X<=' + RealToString(xhi,-12,-6) + ' AND ' +
             'Y>=' + RealToString(ylo,-12,-6) + ' AND Y<=' + RealToString(yhi,-12,-6) + ' AND ' +
             'Z>=' + RealToString(zlo,-12,-6) + ' AND Z<=' + RealToString(zhi,-12,-6);
end;


procedure TSlicerForm.AdjustForLatLong;
begin
   if LatLongCoords then begin
      Label1.Caption := 'Thick (sec)';
      MDDef.SlicerUseCorrectScaling := false;
      MDDef.CloudSliceJump := MDDef.CloudSliceJump / 3600;
      MDDef.CloudSliceThick := MDDef.CloudSliceThick / 3600;
      CoordDec := -5;
   end
   else begin
      CoordDec := -2;
   end;
end;


procedure TSlicerForm.ArrangeControls;
var
  xon : integer;

        procedure PlaceGroupBox(TheBox : tGroupBox);
        begin
           if TheBox.Visible then begin
              TheBox.Left := xon;
              xon := xon + theBox.Width + 4;
           end;
        end;

        procedure PlaceRadioGroup(TheBox : tRadioGroup);
        begin
           if TheBox.Visible then begin
              TheBox.Left := xon;
              xon := xon + theBox.Width + 4;
           end;
        end;

begin
   {$IfDef NoArrangeControls} exit; {$EndIf}

   {$IfDef SliceControls} WriteLinetoDebugFile('TSlicerForm.ArrangeControls in'); {$EndIf}
   SetColorOptions;
   xon := 4;
   PlaceGroupBox(GroupBox1);
   PlaceGroupBox(GroupBox2);
   PlaceGroupBox(GroupBox3);
   PlaceGroupBox(CloudPickGroupBox1);
   {$IfDef SliceControls} WriteLinetoDebugFile('TSlicerForm.ArrangeControls group boxes done'); {$EndIf}

   PlaceRadioGroup(RadioGroup3);
   PlaceRadioGroup(RadioGroup1);
   PlaceRadioGroup(RadioGroup2);
   {$IfDef SliceControls} WriteLinetoDebugFile('TSlicerForm.ArrangeControls radio groups done'); {$EndIf}
   ClientWidth := xon;
   wmDEM.FormPlacementInCorner(self);
   {$IfDef SliceControls} WriteLinetoDebugFile('TSlicerForm.ArrangeControls out'); {$EndIf}
end;


procedure TSlicerForm.SetColorOptions;
begin
   {$IfDef NoSetColorOptions} exit; {$EndIf}
   {$IfDef SliceControls} WriteLinetoDebugFile('TSlicerForm.SetColorOptions in'); {$EndIf}
   RadioGroup3.Items.Clear;
   RadioGroup3.Items.Add('Elevation');
   if (MemoryPointCloud[1].PtClass <> Nil) then RadioGroup3.Items.Add('Classification');
   if (MemoryPointCloud[1].PtReturnNumber <> Nil) then RadioGroup3.Items.Add('Return number');
   if (MemoryPointCloud[1].PtRGB <> Nil) then RadioGroup3.Items.Add('RGB');
   if (MemoryPointCloud[2] <> Nil) then RadioGroup3.Items.Add('Cloud ID');
   RadioGroup3.Items.Add('Grayscale');
   RadioGroup3.ItemIndex := 0;
   RadioGroup3.Visible := RadioGroup3.Items.Count > 1;
   {$IfDef SliceControls} WriteLinetoDebugFile('TSlicerForm.SetColorOptions out'); {$EndIf}
end;


procedure TSlicerForm.BitBtn11Click(Sender: TObject);
var
   dx,dy,RotAng : float64;
begin
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.BitBtn11Click (translate and rotate) in'); {$EndIf}
   if (MemoryPointCloud[CurSlice].CloudName = 'ascii_GS4') then begin
      dx := -462950.0 - 12.30 + 0.85;   {first term from Fred Hocker, second added to get 0 as the rear of the vessel }
      dy := -6472885.0 - 5.25 - 4;      {first term from Fred Hocker, second added to get 0 as the centerline }
      RotAng := -19;
   end
   else begin
      ReadDefault('x translation',dx);
      ReadDefault('y translation',dy);
      ReadDefault('Rotation angle',RotAng);
   end;
   MemoryPointCloud[CurSlice].TranslateAndRotate(dx,dy,RotAng);
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.BitBtn11Click LoadMemoryPointCloud'); {$EndIf}
   LoadMemoryPointCloud(CurSlice,'');
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.BitBtn11Click call BitBtn1Click'); {$EndIf}
   BitBtn1Click(Sender);
   BitBtn11.Enabled := false;
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.BitBtn11Click (translate and rotate) out'); {$EndIf}
end;


procedure TSlicerForm.BitBtn16Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PickSliceLocation);
end;

procedure TSlicerForm.Set3DFilter(db : integer; xlo,xhi,ylo,yhi : float64);
begin
   if RadioGroup1.ItemIndex in [0] then begin   //0  x-y
      GISdb[db].dbOpts.GeoFilter := 'X>=' + RealToString(xlo,-12,-6) + ' AND X<=' + RealToString(xhi,-12,-6) + ' AND Y>=' + RealToString(ylo,-12,-6) + ' AND Y<=' + RealToString(yhi,-12,-6);
   end
   else if RadioGroup1.ItemIndex in [1] then begin  //1 x-z
      GISdb[db].dbOpts.GeoFilter := 'X>=' + RealToString(xlo,-12,-6) + ' AND X<=' + RealToString(xhi,-12,-6) + ' AND Z>=' + RealToString(ylo,-12,-6) + ' AND Z<=' + RealToString(yhi,-12,-6);
   end
   else if RadioGroup1.ItemIndex in [2] then begin //2  y-z
      GISdb[db].dbOpts.GeoFilter := 'Y>=' + RealToString(xlo,-12,-6) + ' AND Y<=' + RealToString(xhi,-12,-6) + ' AND Z>=' + RealToString(ylo,-12,-6) + ' AND Z<=' + RealToString(yhi,-12,-6);
   end;

   GISdb[db].AssembleGISFilter;
   {$IfDef Slicer} WriteLineToDebugFile('TSlicerForm.Set3DFilter  filter=' + GISdb[db].MyData.Filter); {$EndIf}
end;

procedure TSlicerForm.MaskBoxPoints(xlo,xhi,ylo,yhi : float64);
var
   i : integer;
begin
   ShowHourglassCursor;
   with SliceGraph[CurSlice] do begin
      for i := 1 to 15 do begin
         if (GISdb[i] <> Nil) then begin
            Set3DFilter(i,xlo,xhi,ylo,yhi);
            GISdb[i].dbTablef.ShowStatus;
            if (GISdb[i].MyData.RecordCount > 0) and  (SlicerMaskField <> '') then begin
               while not GISdb[i].MyData.EOF do begin
                  GISdb[i].MyData.Edit;
                  GISdb[i].MyData.SetFieldByNameAsString(SlicerMaskField,'Y');
                  GISdb[i].MyData.Next;
               end;
            end;
         end;
      end;
   end;
   ShowDefaultCursor;
end;


procedure TSlicerForm.MaskPoints(x,y : float64);
const
   Tolerance = 0.05;
begin
   MaskBoxPoints(x-Tolerance,x+Tolerance,y-Tolerance,y+Tolerance);
end;


procedure TSlicerForm.DigitizePoint(x,y : float64);

    procedure AddAPoint(xf,yf,zf : float64);
    begin
       GISdb[XYZGIS].MyData.Insert;
       GISdb[XYZGIS].MyData.SetFieldByNameAsFloat('X',Xf);
       GISdb[XYZGIS].MyData.SetFieldByNameAsFloat('Y',yf);
       GISdb[XYZGIS].MyData.SetFieldByNameAsFloat('Z',zf);
       GISdb[XYZGIS].MyData.Post;
    end;

begin
   Case RadioGroup1.ItemIndex of
      0 : AddAPoint(x,y,SliceGraph[CurSlice].SliceCenter);  //xy
      1 : AddAPoint(x,SliceGraph[CurSlice].SliceCenter,y);  //xz
      2 : AddAPoint(SliceGraph[CurSlice].SliceCenter,x,y);  //yz
   end;
end;


procedure TSlicerForm.LoadMemoryPointCloud(Cloud : integer; fName : PathStr;  XField : shortstring = 'X'; YField : shortstring = 'Y'; ZField : shortstring = 'Z'; InCloudName : shortstring = ''; inStayLatLong : boolean = false);


     procedure SetCheckBox(Cloud : integer; var CheckBox : tCheckBox);
     begin
       CheckBox.Visible := true;
       if InCloudName = '' then CheckBox.Caption := ExtractFileNameNoExt(fName)
       else CheckBox.Caption := InCloudName;
       CheckBox.Checked := true;
       CloudUsed[Cloud] := true;
       CheckBox.Font.Color := ConvertPlatformColorToTColor(MDDef.CloudSymbol[Cloud].Color);
     end;


begin
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.LoadMemoryPointCloud, cloud=' + IntToStr(Cloud) + '  fName= ' + fName); {$EndIf}
   if (MemoryPointCloud[Cloud] = Nil) then begin
      {$IfDef Slicer} WriteLineToDebugFile('TSlicerForm.LoadMemoryPointCloud off to create cloud ' + IntToStr(Cloud)); {$EndIf}
      MemoryPointCloud[Cloud] := tMemoryPointCloud.Create(fName,SlicerMapOwner,XField,YField,ZField,InCloudName,InStayLatLong);
      {$IfDef Slicer} WriteLineToDebugFile('TSlicerForm.LoadMemoryPointCloud created ' + IntToStr(Cloud)); {$EndIf}
   end;
   CloudInUse := Cloud;
   if (Cloud = 1) then begin
       xlo := MemoryPointCloud[Cloud].pcMinX;
       xhi := MemoryPointCloud[Cloud].pcMaxX;
       ylo := MemoryPointCloud[Cloud].pcMinY;
       yhi := MemoryPointCloud[Cloud].pcMaxY;
       zlo := MemoryPointCloud[Cloud].pcMinZ;
       zhi := MemoryPointCloud[Cloud].pcMaxZ;
       SetCheckBox(1,CheckBoxCloud1);
       SetColorOptions;
       RadioGroup1Click(Nil);
       CloudPickGroupBox1.Visible := true;
   end
   else begin
      CloudPickGroupBox1.Visible := true;
      if MemoryPointCloud[Cloud].pcMinZ < zlo then zlo := MemoryPointCloud[Cloud].pcMinZ;
      if MemoryPointCloud[Cloud].pcMaxZ > zhi then zhi := MemoryPointCloud[Cloud].pcMaxZ;
      SetSliceSettings;
      if (Cloud = 2) then SetCheckBox(2,CheckBoxCloud2);
      if (Cloud = 3) then SetCheckBox(3,CheckBoxCloud3);
      if (Cloud = 4) then SetCheckBox(4,CheckBoxCloud4);
      if (Cloud = 5) then SetCheckBox(5,CheckBoxCloud5);
   end;
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.LoadMemoryPointCloud, cloud loaded'); {$EndIf}
   ArrangeControls;
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.LoadMemoryPointCloud, out'); {$EndIf}
end;


procedure TSlicerForm.CreateNewGraph;
begin
   {$IfDef Slicer} WriteLineToDebugFile('TSlicerForm.CreateNewGraph, Currentslice= ' + IntToStr(CurSlice)); {$EndIf}
   SliceGraph[CurSlice].GISGraf := tThisBaseGraph.Create(Application);
   SliceGraph[CurSlice].GISGraf.CanCloseGraph := false;
   SliceGraph[CurSlice].GISGraf.ScrollGraph := true;
   SliceGraph[CurSlice].GISGraf.Top := 0;
   SliceGraph[CurSlice].GISGraf.Left := 0;
   SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane := MDDef.PtSlicerDefView;
   SliceGraph[CurSlice].GISGraf.GraphDraw.PointCloudSlice := true;
   if LatLongCoords then SliceGraph[CurSlice].SliceThickness := MDDef.CloudSliceThick/ 3600
   else SliceGraph[CurSlice].SliceThickness := MDDef.CloudSliceThick;
   SetSliceSettings;
end;


{$IfDef IncludeVASAgis}
procedure TSlicerForm.TryToGraphDatabase(bmp : tBitmap; i : integer);

     procedure DrawPoint(xf,yf : ShortString; ItsLineTo : boolean);
     var
        x,y : integer;
        xr,yr : float64;
     begin
        xr := GISdb[i].MyData.GetFieldByNameAsFloat(xf);
        yr := GISdb[i].MyData.GetFieldByNameAsFloat(yf);
        x := SliceGraph[CurSlice].GISGraf.GraphDraw.GraphX(xr);
        y := SliceGraph[CurSlice].GISGraf.GraphDraw.GraphY(yr);
        if ItsLineTo then bmp.Canvas.LineTo(x,y)
        else bmp.Canvas.MoveTo(x,y);
     end;

begin
   if (GISdb[i] <> Nil) and GISdb[i].LayerIsOn then begin
      GISdb[i].EmpSource.Enabled := false;
      if GISdb[i].ItsBoxOutline then begin
          GISdb[i].MyData.First;
          bmp.Canvas.Pen.Color := clRed;
          bmp.Canvas.Pen.Width := 2;
          while not GISdb[i].MyData.EOF do begin
             DrawPoint('X1','Y1',false);
             DrawPoint('X2','Y2',true);
             DrawPoint('X3','Y3',true);
             DrawPoint('X4','Y4',true);
             DrawPoint('X1','Y1',true);
             GISdb[BoxOutlineGIS].MyData.Next;
          end;
          GISdb[BoxOutlineGIS].EmpSource.Enabled := true;
      end
      else if GISdb[i].XYZFile then begin
         (*
         if CheckBox8.Checked then begin
            if SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane in [0] then begin   //0  x-y
               GISdb[i].dbOpts.GeoFilter := 'Z>=' + RealToString(SliceGraph[CurSlice].SliceLo,-12,CoordDec) + ' AND Z<=' + RealToString(SliceGraph[CurSlice].SliceHi,-12,CoordDec);
            end
            else if SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane in [1] then begin  //1 x-z
               GISdb[i].dbOpts.GeoFilter := 'Y>=' + RealToString(SliceGraph[CurSlice].SliceLo,-12,CoordDec) + ' AND Y<=' + RealToString(SliceGraph[CurSlice].SliceHi,-12,CoordDec);
            end
            else if SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane in [2] then begin //2  y-z
               GISdb[i].dbOpts.GeoFilter := 'X>=' + RealToString(SliceGraph[CurSlice].SliceLo,-12,CoordDec) + ' AND X<=' + RealToString(SliceGraph[CurSlice].SliceHi,-12,CoordDec);
            end;
         end
         else begin
             GISdb[i].dbOpts.GeoFilter := '';
         end;
         GISdb[i].AssembleGISFilter;
         GraphDBFFile(i,Bmp,GISdb[i].dbOpts.Symbol);
         *)
      end;
      if (GISdb[i].DBTablef <> Nil) then GISdb[i].DBTablef.ShowStatus;
   end;
end;
{$EndIf}


{$IfDef SlicerRange}
   procedure TSlicerForm.ShowDataRanges;
   begin
      WriteLineToDebugFile('x range=' + RealToString(xlo,-12,CoordDec) + ' to ' + RealToString(xhi,-12,CoordDec) );
      WriteLineToDebugFile('y range=' + RealToString(ylo,-12,CoordDec) + ' to ' + RealToString(yhi,-12,CoordDec) );
      WriteLineToDebugFile('z range=' + RealToString(zlo,-12,CoordDec) + ' to ' + RealToString(zhi,-12,CoordDec) );
   end;
{$EndIf}


procedure TSlicerForm.BitBtn1Click(Sender: TObject);
begin
   ReadyToRoll := true;
   RedrawClouds;
end;


procedure TSlicerForm.PointsAtLatLong(Lat, Long: float64);
begin
   if (MemoryPointCloud[1] <> nil) then MemoryPointCloud[1].ExtractPointsNearHere(Lat,Long,SliceGraph[CurSlice].SliceThickness);
end;


procedure TSlicerForm.BitBtn2Click(Sender: TObject);
var
  GeometryFName,ColorsFName : PathStr;


      procedure ExportBinary;  //export to binary files for use in FMX viewer
      var
         Points : ^tPointXYZIArray;
         j,NPts : integer;
         Outf : file;
         zRange : float64;
      begin
         {$IfDef SlicerOpenGL}
            WriteLinetoDebugFile('ExportBinary in, slice=' + IntToStr(CloudInUse));
            ShowDataRanges;
         {$EndIf}
         ShowHourglassCursor;
         zRange := MemoryPointCloud[CloudInUse].pcMaxZ - MemoryPointCloud[CloudInUse].pcMinZ;
         NPts := 0;
         j := 1;
         New(Points);
         while (j <= MemoryPointCloud[CloudInUse].NumMemPts div MDDef.CloudOpenGLThinFactor) do begin
            inc(NPts);
            Points^[NPTs].x := MemoryPointCloud[CloudInUse].xyPts^[j,1];
            Points^[NPTs].y := MemoryPointCloud[CloudInUse].xyPts^[j,2];
            Points^[NPTs].z := MemoryPointCloud[CloudInUse].zPts^[j];
            if MDDef.SliceColorOpt = scoRGB then begin
               Points^[NPTs].Int := MemoryPointCloud[CloudInUse].PtRGB^[j].rgbtRed;
               Points^[NPTs].Int2 := MemoryPointCloud[CloudInUse].PtRGB^[j].rgbtGreen;
               Points^[NPTs].Int3 := MemoryPointCloud[CloudInUse].PtRGB^[j].rgbtBlue;
            end
            else begin
               Points^[NPTs].Int := round(255* ((Points^[NPTs].z-MemoryPointCloud[CloudInUse].pcMinZ)/zRange));
            end;
            inc(j,MDDef.CloudOpenGLThinFactor);
         end;

         GeometryFName := Petmar.NextFileNumber(MDTempDir, 'cloud_slicer_','.xyzib');
         if MDDef.SliceColorOpt = scoRGB then ColorsFName := FullPaletteBitmap
         else ColorsFName := Palette256Bitmap(p256Terrain);

         AssignFile(Outf,GeometryFName);
         rewrite(Outf,1);
         BlockWrite(OutF,Points^,NPTs * SizeOf(tPointXYZI));
         CloseFile(outf);
         Dispose(Points);

         ShowDefaultCursor;
         {$IfDef SlicerOpenGL} WriteLinetoDebugFile('ExportBinary out, NPTs=' + IntToStr(NPTs)); {$EndIf}
      end;


begin
   if (CloudInUse <> 0) then begin

      {$IfDef SlicerOpenGL} WriteLinetoDebugFile('TSlicerForm.BitBtn2Click (OpenGL) in, CloudInUse=' + IntToStr(CloudInUse) + '   OpenGLCloudThin=' + IntToStr(MDDef.CloudOpenGLThinFactor)); {$EndIf}

      {$IfDef ExOpenGL}
      {$Else}
         DEMDefs.VasaProjectFName := ThisProject;
         ShowHourglassCursor;
         ExportBinary;
         FMX3dViewer(True,GeometryFName,'','','','');
         ShowDefaultCursor;
      {$EndIf}
      {$IfDef SlicerOpenGL} WriteLinetoDebugFile('TSlicerForm.BitBtn2Click (OpenGL) out'); {$EndIf}
   end;
end;


procedure TSlicerForm.SingleYLine(x : float64; Color : tPlatformColor; Width : integer);
begin
    SliceGraph[CurSlice].GISGraf.Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
    SliceGraph[CurSlice].GISGraf.Image1.Canvas.Pen.Width := Width;
    SliceGraph[CurSlice].GISGraf.Image1.Canvas.MoveTo(SliceGraph[CurSlice].GISGraf.GraphDraw.GraphX(x),0);
    SliceGraph[CurSlice].GISGraf.Image1.Canvas.LineTo(SliceGraph[CurSlice].GISGraf.GraphDraw.GraphX(x),SliceGraph[CurSlice].GISGraf.GraphDraw.YWindowSize - SliceGraph[CurSlice].GISGraf.GraphDraw.BottomMargin);
end;


procedure TSlicerForm.SymbolXZPlane(x,z : float64; Color : tPlatformColor);
begin
   Petmar.ScreenSymbol(SliceGraph[CurSlice].GISGraf.Image1.Canvas,  SliceGraph[CurSlice].GISGraf.GraphDraw.GraphX(x),
      SliceGraph[CurSlice].GISGraf.GraphDraw.GraphY(Z), FilledBox,3,Color);
end;


procedure TSlicerForm.DigitizeYAxis(NewX,NewY : float64);
var
   x,z : float64;
   fName : PathStr;
   TStr : shortString;
begin
   {$IfDef SlicerEdits} WriteLinetoDebugFile('TSlicerForm.DigitizeYAxis in, NewY = ' + RealToString(NewY,-12,2)); {$EndIf}

   x := EditTable.GetFieldByNameAsFloat('X');
   z := EditTable.GetFieldByNameAsFloat('Z');
   tStr := RealToString(x,-12,-3) + ',' + RealToString(NewY,-12,-3) + ',' + RealToString(Z,-12,-3) + ',' + EditTable.GetFieldByNameAsString(EditName) + ',' + Edit2Table.GetFieldByNameAsString(Edit2Name);
   EditResults.Add(tStr);
   {$IfDef SlicerEdits} WriteLineToDebugFile('TSlicerForm.DigitizeYAxis:  ' + TStr); {$EndIf}
   Edit2Table.Next;
   if Edit2Table.eof then begin
      {$IfDef SlicerEdits} WriteLineToDebugFile('Beam done: ' + EditTable.GetFieldByNameAsString(EditName)); {$EndIf}
      x := EditTable.GetFieldByNameAsFloat('X');
      SingleYLine(x,ConvertTColorToPlatFormColor(clSilver),2);
      EditTable.Next;
      if EditTable.eof or (EditTable.GetFieldByNameAsString(EditName) = '') then begin
         {$IfDef SlicerEdits} WriteLineToDebugFile('Digitizing done'); {$EndIf}
         fName := ExtractFilePath(ThisProject);
         Petmar.GetFileNameDefaultExt('saved points','*.dbf',fName);
         StringList2CSVtoDB(EditResults,fName,true);
         EditTable.Destroy;
         Edit2Table.Destroy;
         GraphDoing := BaseGraf.gdDoingNothing;
         WMDem.StatusBar1.Panels[0].Text := '';
         exit;
      end
      else begin
         {$IfDef SlicerEdits} WriteLineToDebugFile('Next beam ' + EditTable.GetFieldByNameAsString(EditName)); {$EndIf}
         Edit2Table.First;
         x := EditTable.GetFieldByNameAsFloat('X');
         SingleYLine(x,ConvertTColorToPlatFormColor(clRed),2);
      end;
   end;
   WMDem.StatusBar1.Panels[0].Text := Edit2Table.GetFieldByNameAsString(Edit2Name);
   {$IfDef SlicerEdits} WriteLinetoDebugFile('Move to ' + Edit2Table.GetFieldByNameAsString(Edit2Name)); {$EndIf}
end;



procedure TSlicerForm.ZShift(NewX,NewZ : float64);
var
   x,z : float64;
begin
   x := EditTable.GetFieldByNameAsFloat('X');
   z := EditTable.GetFieldByNameAsFloat('Z');
   SymbolXZPlane(x,z,ConvertTColorToPlatformColor(clBlue));
   SymbolXZPlane(x,Newz,ConvertTColorToPlatformColor(clLime));
   {$IfDef SlicerEdits} WriteLineToDebugFile('TSlicerForm.ZShift in Old Point, x=' + RealToString(x,-12,2) + '  z=' + RealToString(z,-12,2) + ' Clicked:  x=' + RealToString(Newx,-12,2) + '  z=' + RealToString(Newz,-12,2)); {$EndIf}
   EditTable.Edit;
   EditTable.SetFieldByNameAsFloat('Z',NewZ);
   EditTable.Next;

   if EditTable.eof then begin
      {$IfDef SlicerEdits} WriteLineToDebugFile('All points done'); {$EndIf}
      EditTable.Destroy;
      MessageToContinue('z shift done');
      ChangeGraphDoing(BaseGraf.gdDoingNothing);
   end
   else begin
      x := EditTable.GetFieldByNameAsFloat('X');
      z := EditTable.GetFieldByNameAsFloat('Z');
      SymbolXZPlane(x,z,ConvertTColorToPlatformColor(clRed));
   end;
end;


procedure TSlicerForm.GetRangesInXYZ;
var
   ProjectTable : tMyData;
   z : float64;
begin
   ProjectTable := tMyData.Create(ThisProject);
   if ProjectTable.FieldExists('USE') and ProjectTable.FieldExists('TYPE') then begin
      ProjectTable.ApplyFilter('USE=' + QuotedStr('Y') + ' AND TYPE=' + QuotedStr('geometry'));
      ProjectTable.FindFieldRange('MIN_X',xlo,Z);
      ProjectTable.FindFieldRange('MIN_Y',ylo,Z);
      ProjectTable.FindFieldRange('MIN_Z',zlo,Z);
      ProjectTable.FindFieldRange('MAX_X',Z,xhi);
      ProjectTable.FindFieldRange('MAX_Y',Z,yhi);
      ProjectTable.FindFieldRange('MAX_Z',Z,Zhi);
      {$IfDef SlicerRange} WriteLinetoDebugFile('TSlicerForm.BitBtn3Click');  ShowDataRanges; {$EndIf}
   end
   else begin
      SingleDBFFile := true;
   end;
   ProjectTable.Destroy;
end;

procedure TSlicerForm.BitBtn42Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PickSlicePanorama);
end;


procedure TSlicerForm.BitBtn43Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PickPointCloudStats);
end;

procedure TSlicerForm.BitBtn44Click(Sender: TObject);
begin
   MemoryPointCloud[CloudInUse].HowHighUp;
end;


procedure TSlicerForm.BitBtn45Click(Sender: TObject);
begin
(*
var
   Bitmap : tMyBitmap;
   i : integer;


   procedure Cycle(IsItLikeDTED : boolean);
   var
      i,x,xgs,ygs,xge,yge : integer;
      z : float32;
      Lat,Long : float64;
      NeedReset : boolean;
      bmp : tMyBitmap;
      TStr : shortstring;
      Symbol : tFullSymbolDeclaration;
   begin
      NeedReset := true;
      for i := 1 to MaxCompare do begin
         if ValidDEM(CompareDEMIndexes[i]) and (IsItLikeDTED = LikeDTED[i]) {for ASTER} then begin
            DEMGlb[CompareDEMIndexes[i]].LatLongDegreeToDEMgridInteger(SliceGraph[CurSlice].SliceCenter,SliceGraph[CurSlice].gxlo,xgs,ygs);
            DEMGlb[CompareDEMIndexes[i]].LatLongDegreeToDEMgridInteger(SliceGraph[CurSlice].SliceCenter,SliceGraph[CurSlice].gxhi,xge,yge);
            if NeedReset then begin
               DEMGlb[CompareDEMIndexes[i]].DEMGridToLatLongDegree(xgs,ygs,Lat,Long);
               SliceGraph[CurSlice].SliceCenter := Lat;
               SliceGraph[CurSlice].SliceThickness := DEMGlb[CompareDEMIndexes[i]].DEMheader.DEMySpacing;
               ChangeSliceCenter(true);
               NeedReset := false;
            end;
            for x := xgs to xge do begin
               if DEMGlb[CompareDEMIndexes[i]].GetElevMetersOnGrid(x,ygs,z) then begin
                  DEMGlb[CompareDEMIndexes[i]].DEMGridToLatLongDegree(x,ygs,Lat,Long);
                  Symbol.DrawingSymbol := FilledBox;
                  Symbol.Size := 5;
                  Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors[i]);
                  SliceGraph[CurSlice].GISGraf.PlotPointOnGraph(Long,z,Symbol);
               end;
            end;
         end;
      end;
      if (not NeedReset) then begin
         CopyImageToBitmap(SliceGraph[CurSlice].GISGraf.Image1,bmp);
         if IsItLikeDTED then TStr := 'point' else TStr := 'area';
         DisplayBitmap(Bmp,'Pixel is ' + TStr,true);
      end;
   end;


begin
   if (LoadDEMsCoveringBox(SlicerMapOwner.MapDraw.MapCorners.BoundBoxGeo) > 0) then begin
      Cycle(True);
      Cycle(False);

      if NeedLegend then begin
         CreateBitmap(Bitmap,500,200);
         for i := 1 to MaxCompare do begin
            if ValidDEM(CompareDEMIndexes[i]) then begin
               Bitmap.Canvas.Pen.Color := WinGraphColors[i];
               Bitmap.Canvas.Brush.Color := WinGraphColors[i];
               Bitmap.Canvas.Brush.Style := bsSolid;
               Bitmap.Canvas.Rectangle(5,5+i*20,25,25 + i*20);
               Bitmap.Canvas.Brush.Style := bsClear;
               Bitmap.Canvas.Font.Size := 14;
               Bitmap.Canvas.Font.Style := [fsBold];
               Bitmap.Canvas.TextOut(30,5 + i*20,DEMGlb[CompareDEMIndexes[i]].AreaName);
            end;
         end;
         PutBitmapInBox(Bitmap);
         DisplayBitmap(Bitmap,'Global DEMs',true);
         Bitmap.Free;
         NeedLegend := false;
      end;
   end;
*)
end;



procedure TSlicerForm.SlicerPanorama(Lat,Long,z : float64);
var
   xutm,yutm : float64;
begin
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.SlicerPanorama in'); {$EndIf}
   MemoryPointCloud[CloudInUse].MapOwner.MapDraw.LatLongDegreeToUTM(Lat,Long,xutm,yutm);
   MemoryPointCloud[CloudInUse].SetPolarCoords(xutm,yutm,z);
   PolarGraph := TThisBaseGraph.Create(Application);
   PolarGraph.GraphDraw.MinHorizAxis := 0;
   PolarGraph.GraphDraw.MaxHorizAxis := 360;
   PolarGraph.GraphDraw.MinVertAxis := -10;
   PolarGraph.GraphDraw.MaxVertAxis := 90;
   PolarGraph.GraphDraw.HorizLabel := 'Compass azimuth (°)';
   PolarGraph.GraphDraw.VertLabel := 'Altitude (°)';
   PolarGraph.Caption := 'Lidar horizon at ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod);
   PolarGraph.GraphDraw.PointCloudPanorama := true;
   PolarGraph.SetUpGraphForm;
   PolarGraph.RedrawDiagram11Click(Nil);
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.SlicerPanorama out'); {$EndIf}
end;


procedure TSlicerForm.RedrawSlicerPanorama(var bmp : tMyBitmap);
var
   i,xi,yi : integer;
   BMPMemory : tBMPMemory;
begin
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.RedrawSlicerPanorama in, sym.size=' + IntToStr(MDDef.CloudSymbol[CloudInUse].Size)); {$EndIf}
   StartProgress('Polar graph');
   BMPMemory := tBMPMemory.Create(bmp);
   for i := 1 to MemoryPointCloud[CloudInUse].NumMemPts do begin
      if (i mod 1000 = 0) then UpdateProgressBar(i/MemoryPointCloud[CloudInUse].NumMemPts);
      xi := PolarGraph.GraphDraw.GraphX(MemoryPointCloud[CloudInUse].PolarCoords^[i,AzimCoord]);
      yi := PolarGraph.GraphDraw.GraphY(MemoryPointCloud[CloudInUse].PolarCoords^[i,ElevCoord]);
      if PolarGraph.GraphDraw.PtOnGraph(xi,yi) then
        if (MemoryPointCloud[CloudInUse].PolarCoords^[i,DistCoord] > MDdef.LidarPanClose) and (MemoryPointCloud[CloudInUse].PolarCoords^[i,DistCoord] <  MDdef.LidarPanFar) then
            BMPMemory.SetPixelColorSize(xi,yi,MDDef.CloudSymbol[CloudInUse].Size,MDDef.CloudSymbol[CloudInUse].Color);
   end;
   BMPMemory.Free;
   EndProgress;
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.RedrawSlicerPanorama out'); {$EndIf}
end;

procedure TSlicerForm.Edit10Change(Sender: TObject);
begin
   CheckEditString(Edit10.Text,MDDef.CloudSliceJump);
   if LatLongCoords then MDDef.CloudSliceJump := MDDef.CloudSliceJump / 3600;
end;

procedure TSlicerForm.Edit1Change(Sender: TObject);
begin
   {$IfDef SliceCenter} if CurSlice <> 0 then WriteLineToDebugFile('TSlicerForm.Edit1Change in, SliceGraph[CurSlice].SliceThickness=' + RealToString(SliceGraph[CurSlice].SliceThickness,-12,-2)); {$EndIf}
   Petmar.CheckEditString(Edit1.Text,MDDef.CloudSliceThick);
   if LatLongCoords then SliceGraph[CurSlice].SliceThickness := MDDef.CloudSliceThick / 3600
   else SliceGraph[CurSlice].SliceThickness := MDDef.CloudSliceThick;
   {$IfDef SliceCenter} WriteLineToDebugFile('TSlicerForm.Edit1Change mid, MDDef.CloudSliceThick=' + RealToString(MDDef.CloudSliceThick,-12,-2)); {$EndIf}
   ChangeSliceCenter(false);
   {$IfDef SliceCenter} WriteLineToDebugFile('TSlicerForm.Edit1Change out, MDDef.CloudSliceThick=' + RealToString(MDDef.CloudSliceThick,-12,-2)); {$EndIf}
end;


procedure TSlicerForm.Edit9Change(Sender: TObject);
begin
   CheckEditString(Edit9.Text,SliceGraph[CurSlice].SliceCenter);
   ChangeSliceCenter(false);
end;

procedure TSlicerForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
   {$IfDef CloseSlicer} WriteLinetoDebugFile('TSlicerForm.FormClose in'); {$EndIf}
   CloseAndNilNumberedDB(XYZGIS);
   {$IfDef IncludeVASAgis}
      CloseAndNilNumberedDB(BoxOutlineGIS);
   {$EndIf}
   for i := 1 to MaxSliceClouds do begin
      if (MemoryPointCloud[i] <> nil) then begin
         MemoryPointCloud[i].Destroy;
         {$IfDef CloseSlicer} WriteLineToDebugFile('TSlicerForm.FormClose closed cloud=' + IntToStr(i)); {$EndIf}
      end;
   end;
   for i := 1 to MaxGISGraph do begin
      if (SliceGraph[i].GISGraf <> Nil) then begin
         SliceGraph[i].GISGraf.CanCloseGraph := true;
         SliceGraph[i].GISGraf.Close;
         SliceGraph[i].GISGraf := Nil;
      end;
   end;
   {$IfDef CloseSlicer} WriteLineToDebugFile('TSlicerForm.FormClose out'); {$EndIf}
end;


procedure TSlicerForm.FormCreate(Sender: TObject);
var
   I : integer;
begin
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.FormCreate in'); {$EndIf}
   CloudInUse := 0;
   XYZGIS := 0;
   {$IfDef IncludeVASAgis}
      BoxOutlineGIS := 0;
      ArtefactGIS := 0;
      GIS := Nil;
   {$EndIf}
   //Height := 200;
   SingleDBFFile := false;
   ReadyToRoll := false;
   NeedLegend := true;

   SlicerMapOwner := Nil;
   EditTable := Nil;
   LOSOwner := Nil;
   CurSlice := 1;
   for i := 1 to MaxSliceClouds do MemoryPointCloud[i] := Nil;
   for i := 1 to MaxSliceClouds do CloudUsed[i] := false;
   for I := 1 to MaxSliceClouds do CloudColorOpt[i] := MDDef.SliceColorOpt;
   for I := 1 to MaxGISGraph do SliceGraph[i].GISGraf := Nil;

   if (MDDef.ClouderXSize > wmDEM.ClientWidth - 100) then MDDef.ClouderXSize := wmDEM.ClientWidth - 100;
   if (MDDef.ClouderYSize > wmDEM.ClientHeight - 50) then MDDef.ClouderYSize := wmDEM.ClientHeight - 50;

   Edit1.Text := RealToString(MDDef.CloudSliceThick,-12,-2);

   if MDDef.ClouderXSize < 0 then MDDef.ClouderXSize := wmDEM.ClientWidth - 200;;
   if MDDef.ClouderYSize < 0 then MDDef.ClouderYSize := wmDEM.ClientHeight div 3;
   Edit2.Text := IntToStr(MDDef.ClouderXSize);
   Edit4.Text := IntToStr(MDDef.ClouderYSize);
   Edit10.Text := RealToString(MDDef.CloudSliceJump,-8,-2);
   RadioGroup1.ItemIndex := MDDef.PtSlicerDefView;

   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.FormCreate color buttons used to be here'); {$EndIf}
   case MDDef.CloudSliceThinFactor of
      1 : RadioGroup4.ItemIndex := 0;
      2 : RadioGroup4.ItemIndex := 1;
      5 : RadioGroup4.ItemIndex := 2;
      10 : RadioGroup4.ItemIndex := 3;
      25 : RadioGroup4.ItemIndex := 4;
   end;

   CheckBoxCloud1.Visible := false;
   CheckBoxCloud2.Visible := false;
   CheckBoxCloud3.Visible := false;
   CheckBoxCloud4.Visible := false;
   CheckBoxCloud5.Visible := false;
   CheckBoxCorrectScaling1.Checked := MDDef.SlicerUseCorrectScaling;

   BitBtn11.Visible := MDDef.ExperimentalSliceOptions;
   BitBtn43.Visible := MDDef.ExperimentalSliceOptions;
   BitBtn44.Visible := MDDef.ExperimentalSliceOptions;
   BitBtn45.Visible := MDDef.ExperimentalSliceOptions;


   ThisProject := DEMDefs.VasaProjectFName;
   FormStyle := fsStayOnTop;
   wmDEM.FormPlacementInCorner(self);

   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.FormCreate out'); {$EndIf}
end;

procedure TSlicerForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html/pt_cloud_db_project.htm');
end;


procedure TSlicerForm.SetSliceSettings;
begin
  if (SliceGraph[CurSlice].GISGraf = Nil) then begin
     {$IfDef SlicerRange} WriteLinetoDebugFile('TSlicerForm.SetSliceSettings but graph undefined'); {$EndIf}
  end
  else begin
      {$IfDef SlicerRange} WriteLinetoDebugFile('TSlicerForm.SetSliceSettings in'); {$EndIf}
      with SliceGraph[CurSlice] do begin
         if GISGraf.GraphDraw.GraphPlane in [0,1] then begin
            gxlo := xlo;
            gxhi := xhi;
         end
         else begin
            gxlo := ylo;
            gxhi := yhi;
         end;
         if GISGraf.GraphDraw.GraphPlane in [0] then begin
            gylo := ylo;
            gyhi := yhi;
         end
         else begin
            gylo := zlo;
            gyhi := zhi;
         end;

         if not LatLongCoords then begin
            gxlo := round(gxlo-1);
            gxhi := round(gxhi+1);
            gylo := round(gylo-1);
            gyhi := round(gyhi+1);
         end;

         if GISGraf.GraphDraw.GraphPlane in [0] then begin
            SliceLo := zlo;
            SliceHi := zhi;
            SliceLoc := 'z=';
            SlicePlane := 'x-y';
         end
         else if GISGraf.GraphDraw.GraphPlane in [1] then begin
            SliceLo := ylo;
            SliceHi := yhi;
            SliceLoc := 'y=';
            SlicePlane := 'x-z';
         end
         else if GISGraf.GraphDraw.GraphPlane in [2] then begin
            SliceLo := xlo;
            SliceHi := xhi;
            SliceLoc := 'x=';
            SlicePlane := 'y-z';
         end;
         SliceGraph[CurSlice].SliceCenter := 0.5 * (SliceHi+SliceLo);
         ChangeSliceCenter(false);
         GISGraf.GraphDraw.GraphPlane := RadioGroup1.ItemIndex;
         GISGraf.GraphDraw.MinHorizAxis := gxlo;
         GISGraf.GraphDraw.MaxHorizAxis := gxhi;
         GISGraf.GraphDraw.MinVertAxis := gylo;
         GISGraf.GraphDraw.MaxVertAxis := gyhi;
      end;
      {$IfDef SlicerRange} ShowDataRanges; WriteLinetoDebugFile('TSlicerForm.SetSliceSettings out'); {$EndIf}
   end;
end;


procedure TSlicerForm.RadioGroup1Click(Sender: TObject);
begin
   {$IfDef Slicer} WriteLinetoDebugFile('TTSlicerForm.RadioGroup1Click (slice view) in, cloud=' + IntToStr(CloudInUse)); {$EndIf}
   if ReadyToRoll then begin
      {$IfDef Slicer} WriteLinetoDebugFile('TTSlicerForm.RadioGroup1Click (slice view) ready to roll'); {$EndIf}
      MDDef.PtSlicerDefView := RadioGroup1.ItemIndex;
      if (SliceGraph[CurSlice].GISGraf <> Nil) then begin
         SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane := MDDef.PtSlicerDefView;
      end;
      SetSliceSettings;
      BitBtn1Click(Nil);
   end;
   {$IfDef Slicer} WriteLinetoDebugFile('TTSlicerForm.RadioGroup1Click out, cloud=' + IntToStr(CloudInUse)); {$EndIf}
end;


procedure TSlicerForm.RadioGroup2Click(Sender: TObject);
begin
   {$IfDef IncludeVASAgis}
      if (ArtefactGIS <> 0) then begin
         SliceGraph[CurSlice].ArtDBFilter := GISdb[ArtefactGIS].dbOpts.MainFilter;
         SliceGraph[CurSlice].ArtDBShow := GISdb[ArtefactGIS].dbOpts.DBAutoShow;
      end;

      CurSlice := succ(RadioGroup2.ItemIndex);

      if (ArtefactGIS <> 0) then begin
         GISdb[ArtefactGIS].dbOpts.MainFilter := SliceGraph[CurSlice].ArtDBFilter;
         GISdb[ArtefactGIS].dbOpts.DBAutoShow := SliceGraph[CurSlice].ArtDBShow;
      end;
   {$EndIf}

   if (SliceGraph[CurSlice].GISGraf = Nil) then begin
      CreateNewGraph;
      SetSliceSettings;
      BitBtn1Click(Nil);
   end
   else begin
      RadioGroup1.ItemIndex := SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane;
      Edit9.Text := RealToString(SliceGraph[CurSlice].SliceCenter,-12,-2);
   end;
   SliceGraph[CurSlice].GISGraf.BringToFront;
end;


procedure TSlicerForm.RadioGroup3Click(Sender: TObject);
begin
   if RadioGroup3.Items[RadioGroup3.ItemIndex] = 'Elevation' then MDDef.SliceColorOpt := scoElevation
   else if RadioGroup3.Items[RadioGroup3.ItemIndex] = 'Classification' then MDDef.SliceColorOpt := scoClass
   else if RadioGroup3.Items[RadioGroup3.ItemIndex] = 'Return number' then MDDef.SliceColorOpt := scoRetNum
   else if RadioGroup3.Items[RadioGroup3.ItemIndex] = 'RGB' then MDDef.SliceColorOpt := scoRGB
   else if RadioGroup3.Items[RadioGroup3.ItemIndex] = 'Grayscale' then MDDef.SliceColorOpt := scoGray
   else MDDef.SliceColorOpt := scoCloudID;
   RedrawClouds;
end;


procedure TSlicerForm.DrawOneCloud(var bmp : tMyBitmap);
var
   x,y : float64;
   Cloud,RecsRead,NPts,i,Dec : integer;
   ProjectTable : tMyData;
   soloShapeFile : tShapeFile;
   fName : PathStr;
   Success : boolean;
   BMPMemory :  tBMPMemory;

         procedure GraphShapeFile(aShapeFile : tShapeFile);
         var
            i,Count,xi,yi,TotalRead : integer;
            v : array[1..3] of float64;
            ShapePoints : ^tLotsOfPoints3D;
         begin
            {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('GraphShapeFile in'); {$EndIf}
            new(ShapePoints);
            reset(aShapeFile.ShapeFile,1);
            BlockRead(aShapeFile.ShapeFile,aShapeFile.MainFileHeader,SizeOf(aShapeFile.MainFileHeader));
            Count := 0;
            TotalRead := 0;
            repeat
               BlockRead(aShapeFile.ShapeFile,ShapePoints[1],sfMaxPoints*Sizeof(sfPointsZWithHeader),RecsRead);
               RecsRead := RecsRead div SizeOf(sfPointsZWithHeader);
               ThreadTimers.UpdateThreadStats(1,Round(100 * TotalRead/aShapeFile.NumRecs));
               inc(TotalRead,RecsRead);
               i := 1;
               while (i < RecsRead) do  begin
                  v[1] := ShapePoints^[i].x;
                  v[2] := ShapePoints^[i].y;
                  v[3] := ShapePoints^[i].z;
                  {$IfDef SlicerProblemsFull} WriteLineToDebugFile(RealToString(v[1],12,2) + RealToString(v[2],12,2) + RealToString(v[3],12,2)); {$EndIf}
                  if (v[SliceGraph[CurSlice].GISGraf.GraphDraw.c3] >= SliceGraph[CurSlice].SliceLo) and (v[SliceGraph[CurSlice].GISGraf.GraphDraw.c3] <= SliceGraph[CurSlice].SliceHi) then begin
                     xi := SliceGraph[CurSlice].GISGraf.GraphDraw.GraphX(v[SliceGraph[CurSlice].GISGraf.GraphDraw.c1]);
                     yi := SliceGraph[CurSlice].GISGraf.GraphDraw.GraphY(v[SliceGraph[CurSlice].GISGraf.GraphDraw.c2]);
                     if SliceGraph[CurSlice].GISGraf.GraphDraw.PtOnGraph(xi,yi) then BMPMemory.SetPixelColorSize(xi,yi,MDDef.CloudSymbol[CloudInUse].Size,MDDef.CloudSymbol[CloudInUse].Color);
                  end;
                  inc(i,MDDef.CloudSliceThinFactor);
               end;
            until System.eof(aShapeFile.ShapeFile);
            Dispose(ShapePoints);
            {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('GraphShapeFile out'); {$EndIf}
         end;


         procedure PlotThisProject;
         begin
            {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('ThisProject <> ""'); {$EndIf}
            {$IfDef SlicerRange} WriteLineToDebugFile('Open project table'); ShowDataRanges; {$EndIf}
            ProjectTable := tMyData.Create(ThisProject);
            if ProjectTable.FieldExists('USE') then  begin
               ProjectTable.ApplyFilter('(USE=' + QuotedStr('Y') + ') AND (TYPE=' + QuotedStr('geometry') + ' OR TYPE=' + QuotedStr('objects') + ')');
            end;
            {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('loop=' + ExtractFileName(ThisProject));{$EndIf}
            StartThreadTimers('Draw',1,true);
            while not ProjectTable.Eof do begin
               ShowHourglassCursor;
               fName := ProjectTable.GetFieldByNameAsString('FILENAME');
               if not FileExists(fName) then begin
                  fName := ExtractFilePath(ThisProject) + fName;
               end;
               fName := ExpandFileName(fName);
               {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('file=' + ExtractFileName(fName)); {$EndIf}
               Color := ProjectTable.GetFieldByNameAsInteger('COLOR');

               MDDef.CloudSymbol[CloudInUse].Color := ConvertTColorToPlatformColor(Color);
               MDDef.CloudSymbol[CloudInUse].Size := ProjectTable.GetFieldByNameAsInteger('SYM_SIZE');

               if FileExists(fName) then begin
                  if (ProjectTable.GetFieldByNameAsString('FILTER') = '') then begin
                     {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('unfiltered'); {$EndIf}
                      soloShapeFile := tShapeFile.Create(fName,success);
                      if Success then begin
                         GraphShapeFile(soloShapeFile);
                      end;
                      FreeAndNil(soloShapeFile);
                  end;
               end
               else begin
                  {$IfDef Slicer} WriteLineToDebugFile('file missing');{$EndIf}
               end;
               ProjectTable.Next;
             end;
             EndThreadTimers;
             ProjectTable.Destroy;
         end;

         procedure ShowCurrentVertAxis(Where : shortstring);
         begin
             {$IfDef SlicerDetailedDraw} WriteLineToDebugFile(Where + RealToString(SliceGraph[CurrentSlice].GISBaseGraf.GraphDraw.AxisRange); {$EndIf}
         end;

begin
   {$If Defined(SlicerProblems) or Defined(SlicerDetailedDraw)} WriteLinetoDebugFile('BitBtn1Click (draw slice) in, cloud=' + IntToStr(CloudInUse)); {$EndIf}
   if not ReadyToRoll then exit;
   ReadyToRoll := false;
   {$IfDef IncludeVASAgis}
       if (GIS <> Nil) then GIS.EmpSource.Enabled := false;\
   {$EndIf}
   {$IfDef SlicerRange} ShowDataRanges; {$EndIf}

   CheckEditString(Edit2.Text,MDDef.ClouderXSize);
   CheckEditString(Edit4.Text,MDDef.ClouderYSize);
   if true or MatchMapSize then begin
      DefaultClientHeight := 400;
      DefaultClientWidth := SlicerMapOwner.MapDraw.MapXSize;
   end
   else begin
      DefaultClientHeight := MDDef.ClouderYSize;
      DefaultClientWidth := MDDef.ClouderXSize;
   end;
   if (SliceGraph[CurSlice].GISGraf = Nil) then begin
      {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('BitBtn1Click create new graph'); {$EndIf}
      CreateNewGraph;
      SliceGraph[CurSlice].GISGraf.SlicerOverlay := true;
      SliceGraph[CurSlice].GISGraf.GraphDraw.MinHorizAxis := SliceGraph[CurSlice].gxlo;
      SliceGraph[CurSlice].GISGraf.GraphDraw.MaxHorizAxis := SliceGraph[CurSlice].gxhi;
      SliceGraph[CurSlice].GISGraf.GraphDraw.MinVertAxis := SliceGraph[CurSlice].gylo - 50;
      SliceGraph[CurSlice].GISGraf.GraphDraw.MaxVertAxis := SliceGraph[CurSlice].gyhi + 50;
      if LatLongCoords then begin
         SliceGraph[CurSlice].GISGraf.GraphDraw.HorizLabel := 'Longitude';
         SliceGraph[CurSlice].GISGraf.GraphDraw.VertLabel := 'Elevation (m)';
         SliceGraph[CurSlice].GISGraf.SetUpGraphForm;
      end;
      ShowCurrentVertAxis('New graph created, ');
   end;

   SliceGraph[CurSlice].GISGraf.GraphDraw.CorrectScaling := MDDef.SlicerUseCorrectScaling;
   SliceGraph[CurSlice].GISGraf.ClearDataOnGraph;

   if MDDef.SlicerUseCorrectScaling then begin
       {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('MDDef.SlicerUseCorrectScaling, Call SetUpGraphForm'); {$EndIf}
       ShowHourglassCursor;
       SliceGraph[CurSlice].GISGraf.SetUpGraphForm;
       SliceGraph[CurSlice].GISGraf.GraphDraw.ForceNewSize := true;
       ShowCurrentVertAxis('Call FormResize Vert axis, ');
       SliceGraph[CurSlice].GISGraf.FormResize(nil);
   end
   else begin
      SliceGraph[CurSlice].GISGraf.RedrawDiagram11Click(Nil);
   end;

   {$IfDef SlicerDetailedDraw} ShowCurrentVertAxis('Graph redrawn'); {$EndIf}

   SliceGraph[CurSlice].GISGraf.MapOwner := SlicerMapOwner;
   SliceGraph[CurSlice].GISGraf.GraphDraw.ThirdPlaneConstant := SliceGraph[CurSlice].SliceCenter;
   SliceGraph[CurSlice].GISGraf.GraphDraw.ThirdPlaneThickness := SliceGraph[CurSlice].SliceThickness;
   SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane := RadioGroup1.ItemIndex;

   if SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane in [0] then  begin   //0  x-y
      SliceGraph[CurSlice].GISGraf.GraphDraw.c1 := 1;
      SliceGraph[CurSlice].GISGraf.GraphDraw.c2 := 2;
      SliceGraph[CurSlice].GISGraf.GraphDraw.c3 := 3;
   end
   else if SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane in [1] then  begin  //1 x-z
      SliceGraph[CurSlice].GISGraf.GraphDraw.c1 := 1;
      SliceGraph[CurSlice].GISGraf.GraphDraw.c2 := 3;
      SliceGraph[CurSlice].GISGraf.GraphDraw.c3 := 2;
   end
   else if SliceGraph[CurSlice].GISGraf.GraphDraw.GraphPlane in [2] then begin //2  y-z
      SliceGraph[CurSlice].GISGraf.GraphDraw.c1 := 2;
      SliceGraph[CurSlice].GISGraf.GraphDraw.c2 := 3;
      SliceGraph[CurSlice].GISGraf.GraphDraw.c3 := 1;
   end;

   NPts := 0;
   CopyImageToBitmap(SliceGraph[CurSlice].GISGraf.Image1,bmp);
   BMPMemory := tBMPMemory.Create(bmp);
   if (MemoryPointCloud[CloudInUse] <> Nil) then begin
     {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('(MemoryPointCloud[CloudInUse] <> Nil) or ExtEquals(Ext, .LAS)'); {$EndIf}
      for Cloud := MaxSliceClouds downto 1 do begin
         if CloudUsed[Cloud] then begin
            StartThreadTimers('Draw slice',1,True);
            MemoryPointCloud[Cloud].PlotSlice(Cloud,BMPMemory,SliceGraph[CurSlice].GISGraf,SliceGraph[CurSlice].GISGraf.GraphDraw.c1,SliceGraph[CurSlice].GISGraf.GraphDraw.c2,
               SliceGraph[CurSlice].GISGraf.GraphDraw.c3,SliceGraph[CurSlice].SliceLo,SliceGraph[CurSlice].SliceHi,NPTs,1);
            EndThreadTimers;
         end;
      end;
   end
   else if FileExtEquals(ThisProject,'.SHP') then begin
       soloShapeFile := tShapeFile.Create(fName,success);
       if Success then GraphShapeFile(soloShapeFile);
       FreeAndNil(soloShapeFile);
   end
   else if (ThisProject <> '') then begin
      PlotThisProject;
   end
   else begin
      {$IfDef IncludeVASAgis}
         if (GIS <> Nil) then begin
           {$IfDef SlicerDetailedDraw} WriteLineToDebugFile('(GIS <> Nil)'); {$EndIf}
            if (not GIS.MyData.Filtered) and GIS.ItsAShapeFile and (GIS.ShapeFileType = 11) then begin
               GraphShapeFile(GIS.aShapeFile);
            end
            else begin
               if not GIS.MyData.Filtered  then begin
                  GIS.MyData.ApplyFilter( 'y>=' + RealToString(SliceGraph[CurSlice].SliceLo,-12,-2) + ' AND y<=' + RealToString(SliceGraph[CurSlice].SliceHi,-12,-2));
                  NPts := GIS.MyData.RecordCount;
               end;

               while not GIS.MyData.EOF do begin
                  x := GIS.MyData.GetFieldByNameAsFloat('X');
                  y := GIS.MyData.GetFieldByNameAsFloat('Z');
                  if SliceGraph[CurSlice].GISGraf.GraphDraw.PtOnGraph(x,y) then begin
                     Petmar.ScreenSymbol(bmp.Canvas,SliceGraph[CurSlice].GISGraf.GraphDraw.GraphX(x),SliceGraph[CurSlice].GISGraf.GraphDraw.GraphY(Y), MDDef.CloudSymbol[CloudInUse]);
                  end;
                  GIS.MyData.Next;
               end;
            end;
            GIS.EmpSource.Enabled := true;
         end;
      {$EndIf}
   end;

   if LatLongCoords then Dec := -5 else Dec := -1;

   Label2.Caption := SliceLoc + Petmar_types.RealToString(SliceGraph[CurSlice].SliceCenter,-12,Dec) + '  pts=' + SmartNumberPoints(NPts);

   Edit9.Text := Petmar_types.RealToString(SliceGraph[CurSlice].SliceCenter,-12,-2);

   if CheckBox7.Checked then begin
      SliceGraph[CurSlice].GISGraf.Caption := SlicePlane + ' section ' + IntToStr(CurSlice) + ' at ' + SliceLoc + RealToString(SliceGraph[CurSlice].SliceCenter,-12,-2) + ' (thickness=' + Edit1.Text + ')';
   end
   else begin
      SliceGraph[CurSlice].GISGraf.Caption := '';
   end;

   {$IfDef IncludeVASAgis}
      for i := 1 to MaxDataBase do begin
         TryToGraphDatabase(bmp,i);
      end;
   {$EndIf}

   SliceGraph[CurSlice].GISGraf.Image1.Canvas.Font.Size := 14;
   SliceGraph[CurSlice].GISGraf.Image1.Canvas.Font.Color := clRed;
   SliceGraph[CurSlice].GISGraf.Image1.Canvas.Font.Style := [fsBold];
   SliceGraph[CurSlice].GISGraf.Image1.Canvas.TextOut(SliceGraph[CurSlice].GISGraf.GraphDraw.LeftMargin + 10,10, SliceGraph[CurSlice].GISGraf.Caption);

   {$IfDef IncludeVASAgis}
      if CheckBox7.Checked and (ArtefactGIS <> 0) and (GISdb[ArtefactGIS] <> nil) and (GISdb[ArtefactGIS].dbOpts.MainFilter <> '') then begin
         SliceGraph[CurSlice].GISGraf.Image1.Canvas.TextOut(SliceGraph[CurSlice].GISGraf.GraphDraw.LeftMargin + 10,
             SliceGraph[CurSlice].GISGraf.GraphDraw.YWindowSize - SliceGraph[CurSlice].GISGraf.GraphDraw.BottomMargin - 25, GISdb[ArtefactGIS].dbOpts.MainFilter);
      end;
   {$EndIf}

   BitBtn16.Enabled := (MDDef.PtSlicerDefView > 0) and (SlicerMapOwner <> Nil);
   ShowDefaultCursor;
   ReadyToRoll := true;
   {$If Defined(SlicerProblems) or Defined(SlicerDetailedDraw)}  ShowCurrentVertAxis('BitBtn1Click out, cloud=' + IntToStr(CloudInUse) + ' '); {$EndIf}
end;



procedure TSlicerForm.RedrawClouds;
var
   Cloud,NumUsed : integer;
   CloudBMP, bmp : tMyBitmap;
   First : boolean;
begin
   if ReadyToRoll then begin
      {$If Defined(Slicer) or Defined(TimeSlicer) } WriteLinetoDebugFile('TSlicerForm.RedrawClouds in'); {$EndIf}
      NumUsed := 0;
      BMP := nil;
      if (SlicerMapOwner <> Nil) then SlicerMapOwner.DoFastMapRedraw;
      First := true;

      for Cloud := MaxSliceClouds downto 1 do begin
         if CloudUsed[Cloud] then begin
            CloudInUse := Cloud;
            inc(NumUsed);
            if First then begin
               DrawOneCloud(CloudBMP);
               Caption := MemoryPointCloud[CloudInUse].CloudName + ' pts=' + SmartNumberPoints(MemoryPointCloud[CloudInUse].NumMemPts);
               First := false;
            end;
         end;
      end;
      if (NumUsed > 0) then begin
         if (NumUsed > 1) then Caption := 'Several clouds';
         SliceGraph[CurSlice].GISGraf.Image1.Picture.Graphic := CloudBmp;
         CloudBMP.Free;
      end;
      {$If Defined(Slicer) or Defined(TimeSlicer) } WriteLinetoDebugFile('TSlicerForm.RedrawClouds out'); {$EndIf}
   end;
end;


procedure TSlicerForm.RadioGroup4Click(Sender: TObject);
begin
   MDDef.CloudSliceThinFactor := StrToInt(RadioGroup4.Items[RadioGroup4.ItemIndex]);
   BitBtn1Click(Sender);
end;


procedure TSlicerForm.ChangeSliceCenter(Redraw : boolean);

      {$IfDef SliceCenter}
      procedure WriteLimits(what : shortstring);
      begin
         WriteLinetoDebugFile('TSlicerForm.ChangeSliceCenter ' + what + ' SliceHi=' + RealToString(SliceGraph[CurSlice].SliceHi,-12,6) + '  SliceCenter=' + RealToString(SliceGraph[CurSlice].SliceCenter,-12,6) +
                              '  SliceLo=' + RealToString(SliceGraph[CurSlice].SliceLo,-12,6) + '  Thickenss=' + RealToString(SliceGraph[CurSlice].SliceThickness,-12,6));
      end;
      {$EndIf}

begin
   if (CurSlice = 0) then exit;
   {$IfDef SliceCenter} WriteLimits('TSlicerForm.ChangeSliceCenter in'); {$EndIf}
   SliceGraph[CurSlice].SliceHi := SliceGraph[CurSlice].SliceCenter + 0.5 * SliceGraph[CurSlice].SliceThickness;
   SliceGraph[CurSlice].SliceLo := SliceGraph[CurSlice].SliceCenter - 0.5 * SliceGraph[CurSlice].SliceThickness;
   if Redraw then BitBtn1Click(Nil);

   {$IfDef SliceCenter} WriteLimits('TSlicerForm.ChangeSliceCenter out'); {$EndIf}
end;

procedure TSlicerForm.CheckBoxCorrectScaling1Click(Sender: TObject);
begin
   MDDef.SlicerUseCorrectScaling := CheckBoxCorrectScaling1.Checked;
end;

procedure TSlicerForm.CheckBoxCloud5Click(Sender: TObject);
begin
   CloudUsed[5] := CheckBoxCloud5.Checked;
   RedrawClouds;
end;

procedure TSlicerForm.CheckBoxCloud1Click(Sender: TObject);
begin
   CloudUsed[1] := CheckBoxCloud1.Checked;
   RedrawClouds;
end;

procedure TSlicerForm.CheckBoxCloud2Click(Sender: TObject);
begin
   CloudUsed[2] := CheckBoxCloud2.Checked;
   RedrawClouds;
end;

procedure TSlicerForm.CheckBoxCloud3Click(Sender: TObject);
begin
   CloudUsed[3] := CheckBoxCloud3.Checked;
   RedrawClouds;
end;

procedure TSlicerForm.CheckBoxCloud4Click(Sender: TObject);
begin
   CloudUsed[4] := CheckBoxCloud4.Checked;
   RedrawClouds;
end;

procedure TSlicerForm.SliceAtUTM(XUTM1, YUTM1: float64);
begin
   if (MDDef.PtSlicerDefView = 1) then begin
      SliceGraph[CurSlice].SliceCenter := yutm1;
      ChangeSliceCenter(true);
   end
   else if (MDDef.PtSlicerDefView = 2) then begin
      SliceGraph[CurSlice].SliceCenter := xutm1;
      ChangeSliceCenter(true);
   end;
end;

procedure TSlicerForm.SpeedButton1Click(Sender: TObject);
begin
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.SpeedButton1Click in'); {$EndIf}
   SliceGraph[CurSlice].SliceCenter := SliceGraph[CurSlice].SliceCenter - MDDef.CloudSliceJump;
   ChangeSliceCenter(true);
end;

procedure TSlicerForm.SpeedButton2Click(Sender: TObject);
begin
   {$IfDef Slicer} WriteLinetoDebugFile('TSlicerForm.SpeedButton2Click in'); {$EndIf}
   SliceGraph[CurSlice].SliceCenter := SliceGraph[CurSlice].SliceCenter + MDDef.CloudSliceJump;
   ChangeSliceCenter(true);
end;


initialization
finalization
end.



(*
procedure TSlicerForm.RecordBoxOutline(bmp : tMyBitmap);

     procedure DrawPoint(xf,yf : ShortString; ItsLineTo : boolean);
     var
        x,y : integer;
        xr,yr : float64;
     begin
        xr := GISdb[BoxOutlineGIS].MyData.GetFieldByNameAsFloat(xf);
        yr := GISdb[BoxOutlineGIS].MyData.GetFieldByNameAsFloat(yf);
        x := SliceGraph[CurSlice].GISBaseGraf.GraphDraw.GraphX(xr);
        y := SliceGraph[CurSlice].GISBaseGraf.GraphDraw.GraphY(yr);
        if ItsLineTo then bmp.Canvas.LineTo(x,y)
        else bmp.Canvas.MoveTo(x,y);
     end;

begin

    if (BoxOutlineGIS = 0) then exit;
    DrawPoint('X1','Y1',false);
    DrawPoint('X2','Y2',true);
    DrawPoint('X3','Y3',true);
    DrawPoint('X4','Y4',true);
    DrawPoint('X1','Y1',true);

end;


procedure TSlicerForm.PlotShapeFileOnSlice(fName : PathStr);
var
   Bitmap : tMyBitmap;
   db : integer;
begin
   {$IfDef SlicePlot} WriteLineToDebugFile('TSlicerForm.PlotShapeFileOnSlice, fname=' + fName); {$EndIf}
    OpenNumberedGISDataBase(db,fname);
    CopyImageToBitmap(SliceGraph[CurSlice].GISGraf.Image1,Bitmap);
    GISdb[db].aShapeFile.Symbol := GISdb[db].dbOpts.Symbol;
    GISdb[db].aShapeFile.ZPlaneMin := SliceGraph[CurSlice].SliceLo;
    GISdb[db].aShapeFile.ZPlaneMax := SliceGraph[CurSlice].SliceHi;
    {$IfDef Slicer} WriteLineToDebugFile('TSlicerForm.PlotShapeFileOnSlice, fname=' + fName + '  z filter ' + RealToString(GISdb[db].aShapeFile.ZPlaneMin,-12,CoordDec) + ' to ' + RealToString(GISdb[db].aShapeFile.ZPlaneMax,-12,CoordDec) ); {$EndIf}
    GISdb[db].aShapeFile.PlotAllRecordsOnGraph(SliceGraph[CurSlice].GISGraf,Bitmap);
    SliceGraph[CurSlice].GISGraf.Image1.Picture.Graphic := Bitmap;
    Bitmap.Free;
    CloseAndNilNumberedDB(db);
end;


procedure TSlicerForm.GraphDBFFile(dbNum : integer;  bmp : tBitmap; Symbol : tFullSymbolDeclaration);
var
   v : array[1..3] of float64;
   xi,yi : integer;
begin
   {$IfDef SlicePlot} WriteLineToDebugFile('GraphDBFFile in ' + GISdb[dbNum].MyData.TableName +  '   SymSize=' + IntToStr(Symbol.Size)); {$EndIf}
   GISdb[dbNum].MyData.First;
   while not GISdb[dbNum].MyData.eof do begin
      GISdb[dbNum].EmpSource.Enabled := false;
      if GISdb[dbNum].MyData.GetXYZFromTable(v[1],v[2],v[3]) then begin
         xi := SliceGraph[CurSlice].GISGraf.GraphDraw.GraphX(v[SliceGraph[CurSlice].GISGraf.GraphDraw.c1]);
         yi := SliceGraph[CurSlice].GISGraf.GraphDraw.GraphY(v[SliceGraph[CurSlice].GISGraf.GraphDraw.c2]);
         if SliceGraph[CurSlice].GISGraf.GraphDraw.PtOnGraph(xi,yi) then begin
            if GISdb[dbNum].MyData.FieldExists('COLOR') then Color := GISdb[dbNum].MyData.GetFieldByNameAsInteger('COLOR');
            ScreenSymbol(bmp.Canvas,xi,yi,Symbol);
            if GISdb[dbNum].dbOpts.LabelDBPlots then begin
               bmp.Canvas.TextOut(xi+5,yi-5,GISdb[dbNum].MyData.GetFieldByNameAsString(GISdb[dbNum].dbOpts.LabelField));
            end;
         end;
      end;
      GISdb[dbNum].MyData.Next;
   end;
   {$IfDef SlicePlot} WriteLineToDebugFile('GraphDBFFile out'); {$EndIf}
end;

procedure TSlicerForm.GetCloudSymbol(Cloud : integer; BitBtn : tBitBtn);
begin
    CloudColorOpt[Cloud] := MDDef.SliceColorOpt;
    PickSymbol(BitBtn,MDDef.CloudSymbol[Cloud],'cloud symbolization');
    RedrawClouds;
end;

 procedure TSlicerForm.LineYDirection(fName : PathStr; Color : tPlatformColor; Width : integer);
var
   Table : tMyData;
   X : float64;
begin
    Table := tMyData.Create(fName);
    while not Table.eof do begin
       x := Table.GetFieldByNameAsFloat('X');
       SingleYLine(x,Color,Width);
       Table.Next;
    end;
    Table.Destroy;
end;



*)




