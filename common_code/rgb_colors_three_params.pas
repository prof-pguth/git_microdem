unit rgb_colors_three_params;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordRGBIssues}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  petmar_types,demmapf, ExtCtrls;

type
  TRGB_form = class(TForm)
    Panel1: TPanel;
    Edit8: TEdit;
    Label11: TLabel;
    Edit7: TEdit;
    Label10: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    HelpBtn: TBitBtn;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    ComboBox1: TComboBox;
    Edit2: TEdit;
    Label5: TLabel;
    Edit1: TEdit;
    Label4: TLabel;
    Label1: TLabel;
    Edit3: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    ComboBox2: TComboBox;
    Edit4: TEdit;
    Label12: TLabel;
    ComboBox3: TComboBox;
    Edit6: TEdit;
    Label9: TLabel;
    Edit5: TEdit;
    Label8: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
      procedure ResetChannels;
  public
    { Public declarations }
    MapForm : tMapForm;
    RedBMP,BlueBMP,GreenBMP,CompositeBMP : tMyBitmap;
    RedChanged,BlueChanged,GreenChanged,DoingWork : boolean;
    procedure DrawGridRGBMap(DEM,Channel : integer; MinRange,MaxRange : float64; Bitmap : tMyBitmap);
    procedure CheckComboBox(ComboBox : tComboBox; MinEdit,MaxEdit : tEdit; var DEM : integer; var Min,Max : Petmar_types.float64; Bitmap : tMyBitmap);
  end;

var
   MinRedRange,MaxRedRange,MinGreenRange,MaxGreenRange,MinBlueRange,MaxBlueRange : Petmar_types.float64;
   RedDEM,BlueDEM,GreenDEM : integer;

function tColorFromRedGreen(RedField,GreenField : float64) : tColor;
function tColorFromRedGreenBlue(RedField,GreenField,BlueField : float64) : tColor;
function tPlatformColorFromRedGreen(RedField,GreenField : float64) : TRGBTriple;
function tPlatformColorFromRGB(RedField,GreenField,BlueField : float64) : TRGBTriple;

procedure ThreeGridRGBMap(aMapForm : tMapForm);

function NewThreeGridRGBMap : integer;

implementation

{$R *.dfm}

uses
   petmar, PetMath, PetImage,
   DEMDefs,
   Nevadia_main,
   demcoord,demmapdraw;

var
   RGB_form : TRGB_form;

const
  GrayChannel = 1;
  RedChannel = 2;
  GreenChannel = 3;
  BlueChannel = 4;



function tPlatformColorFromRedGreen(RedField,GreenField : float64) :  TRGBTriple;
begin
   ValueInRange(RedField,MinRedRange,MaxRedRange);
   ValueInRange(GreenField,MinGreenRange,MaxGreenRange);
   Result.rgbtRed := ValidByteRange(MDDef.MinRGBColor + round( MDDef.RangeRGBColor * (RedField - MinRedRange) / (MaxRedRange - MinRedRange)));
   Result.rgbtGreen := ValidByteRange(MDDef.MinRGBColor + round( MDDef.RangeRGBColor * (GreenField - MinGreenRange) / (MaxGreenRange - MinGreenRange)));
   Result.rgbtBlue := 0;
end;


function tColorFromRedGreen(RedField,GreenField : float64) : tColor;
begin
   Result := ConvertPlatformColorToTColor(tPlatformColorFromRedGreen(RedField,GreenField));
end;


function tPlatformColorFromRGB(RedField,GreenField,BlueField : float64) : TRGBTriple;
begin
   ValueInRange(RedField,MinRedRange,MaxRedRange);
   ValueInRange(GreenField,MinGreenRange,MaxGreenRange);
   ValueInRange(BlueField,MinBlueRange,MaxBlueRange);
   Result.rgbtRed := ValidByteRange(MDDef.MinRGBColor + round( MDDef.RangeRGBColor * (RedField - MinRedRange) / (MaxRedRange - MinRedRange)));
   Result.rgbtGreen := ValidByteRange(MDDef.MinRGBColor + round( MDDef.RangeRGBColor * (GreenField - MinGreenRange) / (MaxGreenRange - MinGreenRange)));
   Result.rgbtBlue := ValidByteRange(MDDef.MinRGBColor + round( MDDef.RangeRGBColor * (BlueField - MinBlueRange) / (MaxBlueRange - MinBlueRange)));
end;


function tColorFromRedGreenBlue(RedField,GreenField,BlueField : float64) : tColor;
begin
   Result := ConvertPlatformColorToTColor(tPlatformColorFromRGB(RedField,GreenField,BlueField));
end;


function NewThreeGridRGBMap : integer;
//var
   //i : Integer;
begin
   {$IfDef RecordRGBIssues} WriteLineToDebugFile('ThreeGridRGBMap in'); {$EndIf}
   RGB_form := TRGB_form.Create(Application);
   (*
   with RGB_form do begin
      MapForm := nil;
      for i := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
         ComboBox1.Items.Add(DEMGlb[i].AreaName);
         ComboBox2.Items.Add(DEMGlb[i].AreaName);
         ComboBox3.Items.Add(DEMGlb[i].AreaName);
         {$IfDef RecordRGBIssues} WriteLineToDebugFile('Accepted: ' + DEMGlb[i].FullDEMParams); {$EndIf}
      end;
      Edit7.Text := RealToString(MDDef.MinPercentile,-12,-1);
      Edit8.Text := RealToString(MDDef.MaxPercentile,-12,-1);

      ComboBox1.Text := ComboBox1.Items[0];
      if (ComboBox1.Items.Count > 1) then ComboBox2.Text := ComboBox1.Items[1];
      if (ComboBox1.Items.Count > 2) then ComboBox3.Text := ComboBox1.Items[2];
      ComboBox1Change(Nil);
      ComboBox2Change(Nil);
      ComboBox3Change(Nil);
      RGB_form.DoingWork := true;
      BitBtn1Click(Nil);
      Show;
   end;
   *)
   {$IfDef RecordRGBIssues} WriteLineToDebugFile('ThreeGridRGBMap out'); {$EndIf}
end;


procedure ThreeGridRGBMap(aMapForm : tMapForm);
//var
   //i : Integer;
begin
   {$IfDef RecordRGBIssues} WriteLineToDebugFile('ThreeGridRGBMap in'); {$EndIf}
   RGB_form := TRGB_form.Create(Application);
(*
   with RGB_form do begin
      MapForm := aMapForm;
      PetImage.CloneImageToBitmap(MapForm.Image1,RedBMP,true);
      PetImage.CloneImageToBitmap(MapForm.Image1,GreenBMP,true);
      PetImage.CloneImageToBitmap(MapForm.Image1,BlueBMP,true);
      for i := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
         ComboBox1.Items.Add(DEMGlb[i].AreaName);
         ComboBox2.Items.Add(DEMGlb[i].AreaName);
         ComboBox3.Items.Add(DEMGlb[i].AreaName);
         {$IfDef RecordRGBIssues} WriteLineToDebugFile('Accepted: ' + DEMGlb[i].FullDEMParams); {$EndIf}
      end;
      Edit7.Text := RealToString(MDDef.MinPercentile,-12,-1);
      Edit8.Text := RealToString(MDDef.MaxPercentile,-12,-1);

      ComboBox1.Text := ComboBox1.Items[0];
      if (ComboBox1.Items.Count > 1) then ComboBox2.Text := ComboBox1.Items[1];
      if (ComboBox1.Items.Count > 2) then ComboBox3.Text := ComboBox1.Items[2];
      ComboBox1Change(Nil);
      ComboBox2Change(Nil);
      ComboBox3Change(Nil);
      RGB_form.DoingWork := true;
      BitBtn1Click(Nil);
      Show;
   end;
*)
   {$IfDef RecordRGBIssues} WriteLineToDebugFile('ThreeGridRGBMap out'); {$EndIf}
end;


procedure TRGB_form.CheckBox1Click(Sender: TObject);
begin
   ResetChannels;
end;


procedure TRGB_form.CheckComboBox(ComboBox : tComboBox; MinEdit,MaxEdit : tEdit; var DEM : integer;  var Min,Max : Petmar_types.float64; Bitmap : tMyBitmap);
var
    i,chan : integer;
begin
   {$IfDef RecordRGBIssues} WriteLineToDebugFile('TRGB_form.CheckComboBox. DEM=' + IntToStr(DEM)); {$EndIf}
   for i := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
      if (ComboBox.Text = DEMGlb[i].AreaName) then begin
         DEM := i;

         if MDDef.DefElevsPercentile then begin
            Min := DEMGlb[DEM].FindPercentileElev(MDDef.MinPercentile);
            Max := DEMGlb[DEM].FindPercentileElev(MDDef.MaxPercentile);
         end
         else begin
            Min := DEMGlb[DEM].DEMHeader.MinElev;
            Max := DEMGlb[DEM].DEMHeader.MaxElev;
         end;
         {$IfDef RecordRGBIssues} WriteLineToDebugFile('Min=' + RealToString(Min,-12,-2) + ' Max=' + RealToString(Max,-12,-2) ); {$EndIf}

         MinEdit.Text := RealToString(Min,-18,-4);
         MaxEdit.Text := RealToString(Max,-18,-4);
         if (ComboBox = ComboBox1) then Chan := RedChannel
         else if (ComboBox = ComboBox2) then Chan := GreenChannel
         else if (ComboBox = ComboBox3) then Chan := BlueChannel;
         DrawGridRGBMap(DEM,chan,Min,Max,Bitmap);
         exit;
      end;
   end;
   DEM := 0;
   if (Bitmap <> Nil) then FreeAndNil(Bitmap);
   FreeAndNil(CompositeBMP);
end;


procedure TRGB_form.DrawGridRGBMap(DEM,Channel : integer; MinRange,MaxRange : float64; Bitmap : tMyBitmap);
var
   xp,yp : Integer;
   Lat,Long,xdem,ydem : float64;
   zr : float32;
   Val : byte;
   Found,IdenticalGrids : boolean;
   BMPMem : tBMPMemory;
begin
   {$IfDef RecordRGBIssues} WriteLineToDebugFile(' TRGB_form.DrawGridRGBMap in, DEM=' + IntToStr(DEM) + ' channel=' + IntToStr(Channel)); {$EndIf}
   StartProgress('RGB map ' + IntToStr(DEM));
   ClearBitmap(Bitmap,clBlack);
   BMPMem := tBMPMemory.Create(Bitmap);
   IdenticalGrids := DEMGlb[MapForm.MapDraw.DEMonMap].SecondGridIdentical(DEM);
   for yp := 0 to pred(Bitmap.Height) do begin
      if (yp mod 100 = 0) and ShowSatProgress then UpdateProgressBar(yp/MapForm.MapDraw.MapYSize);
      for xp := 0 to pred(Bitmap.Width) do begin
         if IdenticalGrids then begin
            MapForm.MapDraw.ScreenToDEMGrid(Xp,Yp,XDEM,YDEM);
            Found := DEMGlb[DEM].GetElevMeters(xdem,ydem,zr);
         end
         else begin
            MapForm.MapDraw.ScreenToLatLongDegree(Xp,Yp,Lat,Long);
            Found := DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,zr);
         end;
         if Found then begin
            Val := ValidByteRange(MDDef.MinRGBColor + round( MDDef.RangeRGBColor * (zr - MinRange) / (MaxRange - MinRange)));
            if (Channel = GrayChannel) or MDDef.GrayscaleChannels then BMPMem.SetPixelRGB(xp,yp,val,val,val)
            else if Channel = RedChannel then BMPMem.SetRedChannel(xp,yp,val)
            else if Channel = GreenChannel then BMPMem.SetGreenChannel(xp,yp,val)
            else if Channel = BlueChannel then BMPMem.SetBlueChannel(xp,yp,val)
         end;
      end;
   end;
   BMPMem.Destroy;
   EndProgress;
   {$IfDef RecordRGBIssues}  WriteLineToDebugFile(' TRGB_form.DrawGridRGBMap out, DEM=' + IntToStr(DEM) + ' channel=' + IntToStr(Channel)); {$EndIf}
end;

procedure TRGB_form.ResetChannels;
begin
   if DoingWork then begin
      MDDef.GrayscaleChannels := CheckBox1.Checked;
      RedChanged := true;
      BlueChanged := true;
      GreenChanged := true;
      BitBtn1Click(Nil);
   end;
end;

procedure TRGB_form.BitBtn1Click(Sender: TObject);
var
   x,y : Integer;
   r,g,b : byte;
   Composite,Red,Green,Blue : tBMPMemory;
begin
   {$IfDef RecordRGBIssues} WriteLineToDebugFile('TRGB_form.BitBtn1Click in'); {$EndIf}
   if (MapForm = nil) then  begin
      MapForm := DEMGlb[RedDEM].SelectionMap.DuplicateMap(false);
      MapForm.MapDraw.MapType := mtRGB3grids;
      MapForm.MatchThiscoverageareaandsamepixelsize1Click(Sender);
      PetImage.CloneImageToBitmap(MapForm.Image1,RedBMP,true);
      PetImage.CloneImageToBitmap(MapForm.Image1,GreenBMP,true);
      PetImage.CloneImageToBitmap(MapForm.Image1,BlueBMP,true);
   end;


(*
   if (RedBMP.Height <> MapForm.Image1.ClientHeight) or (RedBMP.Width <> MapForm.Image1.ClientWidth) then begin
      RedChanged := true;
      BlueChanged := true;
      GreenChanged := true;
   end;
   if (RedDEM <> 0) and RedChanged then DrawGridRGBMap(RedDEM,RedChannel,MinRedRange,MaxRedRange,RedBMP);
   if (BlueDEM <> 0) and BlueChanged then DrawGridRGBMap(BlueDEM,BlueChannel,MinBlueRange,MaxBlueRange,BlueBMP);
   if (GreenDEM <> 0) and GreenChanged then DrawGridRGBMap(GreenDEM,GreenChannel,MinGreenRange,MaxGreenRange,GreenBMP);

   RedChanged := false;
   BlueChanged := false;
   GreenChanged := false;
*)

   if (CompositeBMP <> Nil) then FreeAndNil(CompositeBMP);

   CloneImageToBitmap(MapForm.Image1,CompositeBMP,true);
   Composite := tBMPMemory.Create(CompositeBMP);
   if (RedBMP <> Nil) then Red := tBMPMemory.Create(RedBMP);
   if (GreenBMP <> Nil) then Green := tBMPMemory.Create(GreenBMP);
   if (BlueBMP <> Nil) then Blue := tBMPMemory.Create(BlueBMP);
   r := 0;
   g := 0;
   b := 0;

   StartProgress('RGB map composite');
   for y := 0 to pred(MapForm.MapDraw.MapYSize) do begin
      if (y mod 100 = 0) and ShowSatProgress then UpdateProgressBar(y/MapForm.MapDraw.MapYSize);
      for x := 0 to pred(CompositeBMP.Width) do begin
         if (RedBMP <> Nil) then r := Red.RedChannel(x,y);
         if (GreenBMP <> Nil) then g := Green.GreenChannel(x,y);
         if (BlueBMP <> Nil) then b := Blue.BlueChannel(x,y);
         Composite.SetPixelRGB(x,y,r,g,b);
      end;
   end;
   Composite.Destroy;
   if (RedBMP <> Nil) then Red.Destroy;
   if (GreenBMP <> Nil) then Green.Destroy;
   if (BlueBMP <> Nil) then Blue.Destroy;
   CompositeBMP.SaveToFile(MapForm.MapDraw.FullMapfName);
   EndProgress;
   RadioGroup2.Enabled := true;
   RadioGroup2.ItemIndex := 0;
   RadioGroup2Click(Sender);
   {$IfDef RecordRGBIssues} WriteLineToDebugFile('TRGB_form.BitBtn1Click out'); {$EndIf}
end;


procedure TRGB_form.BitBtn2Click(Sender: TObject);
begin
   ReadDefault('Min grayscale (0-255)',MDDef.MinRGBColor);
   ReadDefault('Max grayscale (0-255)',MDDef.MaxRGBColor);
   MDDef.RangeRGBColor := MDDef.MaxRGBColor - MDDef.MinRGBColor;
   ResetChannels;
end;

procedure TRGB_form.BitBtn3Click(Sender: TObject);
begin
   ResetChannels;
end;

procedure TRGB_form.ComboBox1Change(Sender: TObject);
begin
   CheckComboBox(ComboBox1,Edit2,Edit1,RedDEM,MinRedRange,MaxRedRange,RedBMP);
   RedChanged := false;
   BitBtn1Click(Sender);
end;

procedure TRGB_form.ComboBox2Change(Sender: TObject);
begin
   CheckComboBox(ComboBox2,Edit4,Edit3,GreenDEM,MinGreenRange,MaxGreenRange,GreenBMP);
   GreenChanged := false;
   BitBtn1Click(Sender);
end;

procedure TRGB_form.ComboBox3Change(Sender: TObject);
begin
   CheckComboBox(ComboBox3,Edit6,Edit5,BlueDEM,MinBlueRange,MaxBlueRange,BlueBMP);
   BlueChanged := false;
   BitBtn1Click(Sender);
end;

procedure TRGB_form.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MaxRedRange);
   RedChanged := true;
end;

procedure TRGB_form.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MinRedRange);
   RedChanged := true;
end;

procedure TRGB_form.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MaxGreenRange);
   GreenChanged := true;
end;

procedure TRGB_form.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MinGreenRange);
   GreenChanged := true;
end;

procedure TRGB_form.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,MaxBlueRange);
   BlueChanged := true;
end;

procedure TRGB_form.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,MinBlueRange);
   BlueChanged := true;
end;

procedure TRGB_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   FreeAndNil(RedBMP);
   FreeAndNil(GreenBMP);
   FreeAndNil(BlueBMP);
   FreeAndNil(CompositeBMP);
   Action := caFree;
end;

procedure TRGB_form.FormCreate(Sender: TObject);
var
   i : integer;
begin
   DoingWork := false;
   wmdem.FormPlacementInCorner(Self);
   CheckBox1.Checked := MDDef.GrayscaleChannels;
   Edit7.Text := RealToString(MDDef.MinPercentile,-12,-1);
   Edit8.Text := RealToString(MDDef.MaxPercentile,-12,-1);
   MapForm := nil;
   for i := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
      ComboBox1.Items.Add(DEMGlb[i].AreaName);
      ComboBox2.Items.Add(DEMGlb[i].AreaName);
      ComboBox3.Items.Add(DEMGlb[i].AreaName);
      {$IfDef RecordRGBIssues} WriteLineToDebugFile('Accepted: ' + DEMGlb[i].FullDEMParams); {$EndIf}
   end;

   ComboBox1.Text := ComboBox1.Items[0];
   if (ComboBox1.Items.Count > 1) then ComboBox2.Text := ComboBox1.Items[1];
   if (ComboBox1.Items.Count > 2) then ComboBox3.Text := ComboBox1.Items[2];
   ComboBox1Change(Nil);
   ComboBox2Change(Nil);
   ComboBox3Change(Nil);
   RGB_form.DoingWork := true;
   BitBtn1Click(Nil);
   Show;
end;

procedure TRGB_form.FormResize(Sender: TObject);
begin
   Panel2.Width := ClientWidth div 3;
   Panel4.Width := ClientWidth div 3;
end;

procedure TRGB_form.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\rgb_maps.htm');
end;

procedure TRGB_form.RadioGroup1Click(Sender: TObject);
begin
   MDDef.DefElevsPercentile := RadioGroup1.ItemIndex = 1;
   Edit7.Enabled := MDDef.DefElevsPercentile;
   Edit8.Enabled := MDDef.DefElevsPercentile;

   CheckComboBox(ComboBox1,Edit2,Edit1,RedDEM,MinRedRange,MaxRedRange,RedBMP);
   RedChanged := false;
   CheckComboBox(ComboBox2,Edit4,Edit3,GreenDEM,MinGreenRange,MaxGreenRange,GreenBMP);
   GreenChanged := false;
   CheckComboBox(ComboBox3,Edit6,Edit5,BlueDEM,MinBlueRange,MaxBlueRange,BlueBMP);
   BlueChanged := false;
   BitBtn1Click(Sender);
end;

procedure TRGB_form.RadioGroup2Click(Sender: TObject);
var
   c1,c2,c3 : shortstring;
begin
   case RadioGroup2.ItemIndex of
      0 : MapForm.Image1.Picture.Graphic := CompositeBMP;
      1 : MapForm.Image1.Picture.Graphic := RedBMP;
      2 : MapForm.Image1.Picture.Graphic := GreenBMP;
      3 : MapForm.Image1.Picture.Graphic := BlueBMP;
   end;

   MapForm.Caption := 'Three Grid RGB Map ';
   if RedDEM <> 0 then c1 := 'r=' + DEMGlb[RedDEM].AreaName;
   if GreenDEM <> 0 then c2 := 'g=' + DEMGlb[GreenDEM].AreaName;
   if BlueDEM <> 0 then c3 := 'b=' + DEMGlb[BlueDEM].AreaName;

   case RadioGroup2.ItemIndex of
      0 : MapForm.Caption := MapForm.Caption + '  ' + c1 + '  ' + c2 + '  ' + c3;
      1 : MapForm.Caption := MapForm.Caption + '  ' + c1;
      2 : MapForm.Caption := MapForm.Caption + '  ' + c2;
      3 : MapForm.Caption := MapForm.Caption + '  ' + c3;
   end;
end;


initialization
finalization
   {$IfDef RecordRGBIssues} WriteLineToDebugFile('RecordRGBIssues active in rgb_colors_three_params'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing rgb_colors_three_params'); {$EndIf}
end.
