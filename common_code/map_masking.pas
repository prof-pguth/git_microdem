unit map_masking;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordMapMasking}
   //{$Define RecordMapMaskBMPs}
   //{$Define RecordGeomorphFilter}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms,
  Petmar_Types,DEMMapf,DEMDefs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.Controls;

type
  TMapMaskForm = class(TForm)
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    BitBtn1: TBitBtn;
    Label11: TLabel;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    RadioGroup5: TRadioGroup;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label12: TLabel;
    Panel1: TPanel;
    BitBtn3: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox2: TCheckBox;
    Panel2: TPanel;
    CheckBox1: TCheckBox;
    BitBtn4: TBitBtn;
    Panel4: TPanel;
    CheckBox4: TCheckBox;
    Edit5: TEdit;
    Label5: TLabel;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn8: TBitBtn;
    CheckBox3: TCheckBox;
    Panel6: TPanel;
    BitBtn15: TBitBtn;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    Edit1: TEdit;
    Label2: TLabel;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    Panel5: TPanel;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox11: TCheckBox;
    BitBtn16: TBitBtn;
    Label3: TLabel;
    Label8: TLabel;
    Edit3: TEdit;
    Panel7: TPanel;
    Label4: TLabel;
    BitBtn9: TBitBtn;
    Edit2: TEdit;
    BitBtn17: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn10: TBitBtn;
    Panel8: TPanel;
    CancelBtn: TBitBtn;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    RedrawSpeedButton12: TSpeedButton;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox4Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
  private
    { Private declarations }
    procedure CheckSettings;
  public
    { Public declarations }
    MapUsed : tMapForm;
    DBUsed  : integer;
    UseZoom : Byte;
    Cancelled,Working : boolean;
    TerrainCategory : tTerrainCatDefinition;
    procedure GetRoadMask(var RoadMaskBMP : tMyBitmap);
    procedure GetStreamMask(var StreamMaskBMP : tMyBitmap);
    function MaskFromShapeFiles : tMyBitmap;
    function CreateCombinedMask : tMyBitmap;
  end;


function MaskMap(MapUsed : tMapForm; ChangeDEM,ShowModal : boolean; UseZoom : Byte; theDB : integer = 0; VerySimple : boolean = false) : boolean;


implementation

{$R *.dfm}

uses
   Nevadia_Main,
   Petmar,petmar_db,DEMCoord,
   geomorph_point_class,
   PETImage, demdef_routines,DEMMapDraw,
   us_properties, DEMDataBase, demterrc,DEMESRIShapeFile;


function MaskMap(MapUsed : tMapForm; ChangeDEM,ShowModal : boolean; UseZoom : Byte; theDB : integer = 0; VerySimple : boolean = false) : boolean;
var
   MapMaskForm : TMapMaskForm;
begin
   {$IfDef RecordMapMasking} WriteLineToDebugFile('MaskMap in'); {$EndIf}
   MapMaskForm := TMapMaskForm.Create(Application);
   MapMaskForm.MapUsed := MapUsed;
   MapMaskForm.UseZoom := UseZoom;
   MapMaskForm.DBUsed := theDB;
   MapMaskForm.CheckBox12.Checked := GISdb[theDB].dbOpts.SymbolsWithBuffers;
   MapMaskForm.CheckBox13.Checked := GISdb[theDB].dbOpts.ShowBuffers;

   if VerySimple then begin
        MapMaskForm.Panel1.Visible := false;
        MapMaskForm.Panel2.Visible := false;
        MapMaskForm.Panel4.Visible := false;
        MapMaskForm.Panel5.Visible := false;
        MapMaskForm.Panel6.Visible := false;
        MapMaskForm.OKBtn.Enabled := true;
        MapMaskForm.BitBtn1.Visible := false;
        MapMaskForm.BitBtn8.Visible := false;
        MapMaskForm.BitBtn9.Visible := false;
        MapMaskForm.BitBtn10.Visible := false;
        MapMaskForm.BitBtn11.Visible := false;
        MapMaskForm.BitBtn12.Visible := false;
        MapMaskForm.BitBtn16.Visible := false;
        MapMaskForm.RadioGroup3.Enabled := true;
        MapMaskForm.RadioGroup4.Enabled := true;
        MapMaskForm.RadioGroup5.Enabled := true;
        MapMaskForm.CheckBox3.Visible := false;
        MapMaskForm.GroupBox1.Top := 1;
        MapMaskForm.Panel7.Top := MapMaskForm.GroupBox1.Height + 2;
        MapMaskForm.Panel8.Top := MapMaskForm.GroupBox1.Height + MapMaskForm.Panel7.Height + 4;
        MapMaskForm.ClientHeight := MapMaskForm.Panel8.Top + MapMaskForm.Panel8.Height + 4;
        MapMaskForm.RadioGroup3.Enabled := false;
        MapMaskForm.RadioGroup4.Enabled := false;
        MapMaskForm.RadioGroup5.Enabled := false;
        if AreaShapeFile(GISdb[theDB].ShapeFileType) then begin
           MapMaskForm.RadioGroup5.Enabled := true;
        end
        else if LineShapeFile(GISdb[theDB].ShapeFileType) then begin
           MapMaskForm.RadioGroup4.Enabled := true;
        end
        else begin
           MapMaskForm.RadioGroup3.Enabled := true;
        end;
   end
   else begin
     MapMaskForm.BitBtn9.Enabled := ChangeDEM;
     MapMaskForm.BitBtn10.Enabled := ChangeDEM;
     MapMaskForm.BitBtn11.Enabled := ChangeDEM;

     MapMaskForm.BitBtn3.Enabled := ChangeDEM;
     MapMaskForm.BitBtn4.Enabled := ChangeDEM;
     MapMaskForm.BitBtn5.Enabled := ChangeDEM;
     MapMaskForm.BitBtn8.Enabled := ChangeDEM;

     MapMaskForm.OKBtn.Enabled := ChangeDEM;

     MapMaskForm.BitBtn12.Enabled := not ChangeDEM;
     if (MapUsed.MapDraw.DEMonMap = 0) then begin
        MapMaskForm.Panel1.Visible := false;
        MapMaskForm.Panel4.Visible := false;
        MapMaskForm.Panel5.Visible := false;
     end
     else begin
        DEMGlb[MapUsed.MapDraw.DEMonMap].InitializeTerrainCategory(MapMaskForm.TerrainCategory);
     end;
   end;
   Petmar.ColorBitBtn(MapMaskForm.BitBtn17,MDDef.MapMaskColor);

   if ShowModal then MapMaskForm.ShowModal
   else MapMaskForm.Show;
   Result := not MapMaskForm.Cancelled;
end;


procedure TMapMaskForm.CheckSettings;
begin
   MDDef.ShapeMaskNearPoints := RadioGroup3.ItemIndex = 0;
   MDDef.ShapeMaskNearLines := RadioGroup4.ItemIndex = 0;
   MDDef.ShapeMaskNearInsideAreas := RadioGroup5.ItemIndex = 0;
   CheckEditString(Edit1.Text,MDDef.StreamMaskDist);
   CheckEditString(Edit2.Text,MDDef.MaskOpacity);
   CheckEditString(Edit3.Text,MDDef.RoadMaskDistance);
   CheckEditString(Edit5.Text,MDDef.LocalOptimaOpts.MaxSlope);
   CheckEditString(Edit7.Text,MDDef.ShapePointBufferDist);
   CheckEditString(Edit8.Text,MDDef.ShapeLineBufferDist);
   CheckEditString(Edit9.Text,MDDef.ShapeAreaBufferDist);
   MDDef.LocalOptimaOpts.RoadMask := CheckBox1.Checked;
   MDDef.RegionsRidgeMask := CheckBox2.Checked;
   MDDef.RegionsShapeFileMask := CheckBox3.Checked;
   MDDef.LocalOptimaOpts.SlopeMask := CheckBox4.Checked;
end;


procedure TMapMaskForm.BitBtn10Click(Sender: TObject);
var
   BigMask : tMyBitmap;
begin
   CheckSettings;
   BigMask := CreateCombinedMask;
   MapUsed.Image1.Picture.Graphic := BigMask;
   BigMask.Free;
   MapUsed.EditGridViaColor(emvcAllButSelectedColor,clRed,-MaxSmallInt,true);
   MapUsed.DoCompleteMapRedraw;
end;


procedure TMapMaskForm.BitBtn11Click(Sender: TObject);
begin
   MapUsed.ReloadDEMClick(Nil);
   MapUsed.DoCompleteMapRedraw;
end;

procedure TMapMaskForm.BitBtn12Click(Sender: TObject);
var
   BigMask : tMyBitmap;
begin
   {$IfDef RecordMapMasking} WriteLineToDebugFile('TMapMaskForm.BitBtn12Click'); {$EndIf}
   BitBtn12.Enabled := false;
   CheckSettings;
   if MDDef.RegionsRidgeMask or MDDef.LocalOptimaOpts.RoadMask or MDDef.LocalOptimaOpts.SlopeMask or MDDef.RegionsShapeFileMask then begin
      MapUsed.MapDraw.MapType := mtDEMBlank;
      MapUsed.ResizeByPercentage(UseZoom);
      BigMask := CreateCombinedMask;
      BigMask.SaveToFile(MDTempDir + 'mask.bmp');
      BigMask.Free;
   end;

   MapUsed.MapDraw.MapType := MDDef.DefElevMap;
   BitBtn12.Enabled := true;
   if not OKBtn.Enabled then Close;
end;

procedure TMapMaskForm.BitBtn13Click(Sender: TObject);
var
   MaskBMP : tMyBitmap;
begin
   MapUsed.DoFastMapRedraw;
   MaskBMP := MapUsed.MapDraw.CreateCategoryOverlay(TerrainCategory);
   if MDDef.ExcludeTerrainCatInMask then MakeBitmapNegative(MaskBMP,ConvertTColorToPlatformColor(clRed));
   MapUsed.IHSmergeOntoMap(MaskBMP);
end;


procedure TMapMaskForm.BitBtn14Click(Sender: TObject);
begin
   DEMTerrC.GetTerrainCategory(tcNormal,DEMGlb[MapUsed.MapDraw.DEMonMap].SelectionMap,MapUsed.MapDraw.DEMonMap,TerrainCategory,true);
   {$IfDef RecordMapMasking} WriteLineToDebugFile('New terrain category: ' +  DEMGlb[MapUsed.MapDraw.DEMonMap].TerrainCategoryLabel(TerrainCategory)); {$EndIf}
end;

procedure TMapMaskForm.BitBtn15Click(Sender: TObject);
var
   StreamMaskBMP : tMyBitmap;
begin
   GetStreamMask(StreamMaskBMP);
   MapUsed.DoFastMapRedraw;
   MapUsed.IHSmergeOntoMap(StreamMaskBMP);
end;


procedure TMapMaskForm.BitBtn16Click(Sender: TObject);
begin
   CheckSettings;
   Petmar.GetFileFromDirectory('Single file to mask',DBNameMask + ';*.shp',ShapeFileMaskFile);
   Label3.Caption := ExtractFileName(ShapeFileMaskFile);
   FormCreate(Nil);
end;

procedure TMapMaskForm.BitBtn17Click(Sender: TObject);
begin
  Petmar.QueryColor(BitBtn17,MDDef.MapMaskColor);
end;

procedure TMapMaskForm.BitBtn1Click(Sender: TObject);
begin
   CheckSettings;
   GetDOSPath('Directory with masks',ShapeFileMaskDirectory);
   Label11.Caption := ShapeFileMaskDirectory;
   FormCreate(Nil);
end;

procedure TMapMaskForm.BitBtn2Click(Sender: TObject);
begin
   Geomorph_point_class.PointClassification(MapUsed.MapDraw.DEMonMap,true);
end;


procedure TMapMaskForm.BitBtn3Click(Sender: TObject);
begin
   MapUsed.DrawRidgeMask(clRed,true,true);
   CheckBox10.Checked := MDDef.InvertRidgeMask;
end;


procedure TMapMaskForm.BitBtn4Click(Sender: TObject);
var
   RoadMaskBMP : tMyBitmap;
begin
   MapUsed.DoFastMapRedraw;
   GetRoadMask(RoadMaskBMP);
   MapUsed.IHSmergeOntoMap(RoadMaskBMP);
end;


procedure TMapMaskForm.GetRoadMask(var RoadMaskBMP : tMyBitmap);
begin
   {$IfDef ExTiger}
   {$Else}
      {$IfDef RecordMapMasking} WriteLineToDebugFile('TMapMaskForm.GetRoadMask Mask distance: ' + IntToStr(MDDef.RoadMaskDistance)); {$EndIf}
      CheckSettings;
      CloneImageToBitmap(MapUsed.Image1,RoadMaskBMP);
      MapUsed.MapDraw.DeleteSingleMapLayer(MapUsed.MapDraw.TigerOverlayFName);
      MapUsed.MapDraw.DrawFullTigerCoverage(RoadMaskBMP,true,MDDef.FilterAllTigerRoads,false,MDDef.RoadMaskDistance,false);
      RecolorFan(RoadMaskBMP,claRed);
      if MDDef.ExcludeRoadsInMask then MakeBitmapNegative(RoadMaskBMP,claRed);
   {$EndIf}
end;

procedure TMapMaskForm.GetStreamMask(var StreamMaskBMP : tMyBitmap);
begin
   {$IfDef ExTiger}
   {$Else}
      {$IfDef RecordMapMasking} WriteLineToDebugFile('TMapMaskForm.GetStreamMask  Mask distance: ' + IntToStr(MDDef.MaskDistance)); {$EndIf}
      CheckSettings;
      CloneImageToBitmap(MapUsed.Image1,StreamMaskBMP);
      MapUsed.MapDraw.DeleteSingleMapLayer(MapUsed.MapDraw.TigerOverlayFName);
      MapUsed.MapDraw.DrawFullTigerCoverage(StreamMaskBMP,false,false,false,MDDef.StreamMaskDist,true);
      RecolorFan(StreamMaskBMP,claRed);
      if MDDef.ExcludeStreamsInMask then MakeBitmapNegative(StreamMaskBMP,ConvertTColorToPlatformColor(clRed));
   {$EndIf}
end;


procedure TMapMaskForm.BitBtn5Click(Sender: TObject);
begin
   CheckSettings;
   MapUsed.DrawSlopeMask(clRed,MDDef.LocalOptimaOpts.MaxSlope,true);
end;


procedure TMapMaskForm.BitBtn6Click(Sender: TObject);
begin
   DEMDef_routines.GetMaskingOptions(True,true);
end;

procedure TMapMaskForm.BitBtn8Click(Sender: TObject);
var
   MaskBMP : tMyBitmap;
begin
   CheckSettings;
   MapUsed.DoFastMapRedraw;
   MaskBMP := MaskFromShapeFiles;
   if (MaskBMP <> nil) then MapUsed.IHSmergeOntoMap(MaskBMP,false,MDDef.MaskOpacity);
end;


function TMapMaskForm.MaskFromShapeFiles : tMyBitmap;
var
   j,GISNum : integer;
   DoneOne : boolean;
   tName : PathStr;
   MaskIn : boolean;
   MaskingDistance : integer;
   ShapeFileMasks : tStringList;


      procedure AddFile(tName : PathStr);
      begin
         if OpenNumberedGISDataBase(GISNum,tName,false,false,MapUsed) then begin
            if AreaShapeFile(GISdb[GISNum].ShapeFileType) then begin
               MaskIn := MDDef.ShapeMaskNearInsideAreas;
               MaskingDistance := MDDef.ShapeAreaBufferDist;
            end
            else if LineShapeFile(GISdb[GISNum].ShapeFileType) then begin
               MaskIn := MDDef.ShapeMaskNearLines;
               MaskingDistance := MDDef.ShapeLineBufferDist;
            end
            else begin
               MaskIn := MDDef.ShapeMaskNearPoints;
               MaskingDistance := MDDef.ShapePointBufferDist;
            end;
            MapUsed.MapDraw.AddDatabaseToMask(GISNUM,Result,true,MaskIn,MaskingDistance,MDDef.MapMaskColor);
            CloseAndNilNumberedDB(GISNum);
            DoneOne := true;
         end;
      end;

begin
   with MapUsed do begin
      MapUsed.DoFastMapRedraw;
      DoneOne := false;
      CloneImageToBitmap(MapUsed.Image1,Result);
      if (ShapeFileMaskFile <> '') then begin
          AddFile(ShapeFileMaskFile);
      end;

      if (ShapeFileMaskDirectory <> '') then begin
         {$IfDef RecordGeomorphFilter} WriteLineToDebugFile('TGeomorphFilterForm.BitBtn9Click Shape file Mask'); {$EndIf}
         ShapeFileMasks := Nil;
         Petmar.FindMatchingFiles(ShapeFileMaskDirectory,DefaultDBMask,ShapeFileMasks);
         for j := 0 to pred(ShapeFileMasks.Count) do begin
            tName := ShapeFileMasks.Strings[j];
            {$IfDef RecordGeomorphFilter} WriteLineToDebugFile(tName); {$EndIf}
           AddFile(tName);
        end;
         {$IfDef RecordGeomorphFilter} WriteLineToDebugFile('TGeomorphFilterForm.BitBtn9Click Shape file Mask done'); {$EndIf}
      end;
      if not DoneOne then begin
         FreeAndNil(Result);
         MessageToContinue('No shapefiles selected for mask');
      end;
   end;
end;


function TMapMaskForm.CreateCombinedMask : tMyBitmap;
var
   bmp : tMyBitmap;
   First : boolean;

      procedure DrawAndDeleteOverlay(Result,bmp : tMyBitmap; fName : ShortString);
      var
         x,y : integer;
         p0,p1 : pRGB;
      begin
         {$IfDef RecordMapMasking} PetImage.SaveImageAsBMP(bmp,false,MDTempDir + fName + OverlayFExt); {$EndIf}
         ChangeBitmapBlackWhite(BMP,rgbTripleRed,rgbTripleWhite);

         if First then begin
            First := false;
            Result.Canvas.Draw(0,0,bmp);
         end
         else begin
            for y := 0 to pred(result.height) do begin
               p0 := result.ScanLine[y];
               p1 := bmp.ScanLine[y];
               for x := 0 to pred(Result.Width) do begin
                  if not SameColor(p1[x],p0[x]) then p0[x] := rgbTripleWhite;
               end;
            end;
         end;
         bmp.Free;
      end;


begin
   {$IfDef RecordMapMasking} WriteLineToDebugFile('TMapMaskForm.CreateCombinedMask in'); {$EndIf}

   CheckSettings;
   CloneImageToBitmap(MapUsed.Image1,Result);
   First := true;
   try
      if MDDef.RegionsRidgeMask then begin
         bmp := MapUsed.DrawRidgeMask(clRed,true,false);
         {$IfDef RecordMapMaskBMPs} bmp.SaveToFile(MDTempDir + 'RegionsRidgeMask_mask.bmp'); {$EndIf}
         DrawAndDeleteOverlay(Result,bmp,'regions');
         {$IfDef RecordMapMasking} WriteLineToDebugFile('Ridges done'); {$EndIf}
      end;

      if MDDef.RegionsStreamMask then begin
         GetStreamMask(BMP);
         {$IfDef RecordMapMaskBMPs} bmp.SaveToFile(MDTempDir + 'stream_mask.bmp'); {$EndIf}
         DrawAndDeleteOverlay(Result,bmp,'streams');
         {$IfDef RecordMapMasking} WriteLineToDebugFile('Ridges done'); {$EndIf}
      end;

      if MDDef.LocalOptimaOpts.RoadMask then begin
         GetRoadMask(bmp);
         {$IfDef RecordMapMaskBMPs} bmp.SaveToFile(MDTempDir + 'road_mask.bmp'); {$EndIf}
         DrawAndDeleteOverlay(Result,bmp,'roads');
         {$IfDef RecordMapMasking} WriteLineToDebugFile('Roads done'); {$EndIf}
      end;
      if MDDef.LocalOptimaOpts.SlopeMask then begin
         bmp := MapUsed.DrawSlopeMask(clRed,MDDef.LocalOptimaOpts.MaxSlope,false);
         {$IfDef RecordMapMaskBMPs}  bmp.SaveToFile(MDTempDir + 'slope_mask.bmp'); {$EndIf}
         DrawAndDeleteOverlay(Result,bmp,'slopes');
         {$IfDef RecordMapMasking} WriteLineToDebugFile('Slope done'); {$EndIf}
      end;
      if MDDef.RegionsShapeFileMask then begin
         bmp := MaskFromShapeFiles;
         if (BMP <> Nil) then begin
           {$IfDef RecordMapMaskBMPs} bmp.SaveToFile(MDTempDir + 'region_shape_mask.bmp'); {$EndIf}
           DrawAndDeleteOverlay(Result,bmp,'regions');
           {$IfDef RecordMapMasking} WriteLineToDebugFile('Shape files done'); {$EndIf}
         end;
      end;
      if CheckBox5.Checked then begin
         BMP := MapUsed.MapDraw.CreateCategoryOverlay(TerrainCategory);
         if MDDef.ExcludeTerrainCatInMask then MakeBitmapNegative(BMP,ConvertTColorToPlatformColor(clRed));
         DrawAndDeleteOverlay(Result,bmp,'terr-cats');
         {$IfDef RecordMapMasking} WriteLineToDebugFile('Terrain category done'); {$EndIf}
      end;
   except
      on Exception do ;
   end;
   {$IfDef RecordMapMaskBMPs} Result.SaveToFile(MDTempDir + 'combined_mask.bmp'); {$EndIf}
   {$IfDef RecordMapMasking} WriteLineToDebugFile('TMapMaskForm.CreateCombinedMask out'); {$EndIf}
end;


procedure TMapMaskForm.BitBtn9Click(Sender: TObject);
var
   BigMask : tMyBitmap;
begin
   CheckSettings;
   MapUsed.DoFastMapRedraw;
   BigMask := CreateCombinedMask;
   MapUsed.IHSmergeOntoMap(BigMask,false,MDDef.MaskOpacity);
end;


procedure TMapMaskForm.CancelBtnClick(Sender: TObject);
begin
   Cancelled := true;
   Close;
end;

procedure TMapMaskForm.CheckBox10Click(Sender: TObject);
begin
   MDDef.InvertRidgeMask := CheckBox10.Checked ;
end;

procedure TMapMaskForm.CheckBox11Click(Sender: TObject);
begin
   MDDef.ExcludeTerrainCatInMask := CheckBox11.Checked;
end;

procedure TMapMaskForm.CheckBox12Click(Sender: TObject);
begin
   GISdb[DBUsed].dbOpts.SymbolsWithBuffers := CheckBox12.Checked;
   GISdb[DBUsed].RedrawLayerOnMap;
end;

procedure TMapMaskForm.CheckBox13Click(Sender: TObject);
begin
   GISdb[DBUsed].dbOpts.ShowBuffers := CheckBox13.Checked;
   GISdb[DBUsed].RedrawLayerOnMap;
end;


procedure TMapMaskForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.LocalOptimaOpts.RoadMask := CheckBox1.Checked;
end;

procedure TMapMaskForm.CheckBox4Click(Sender: TObject);
begin
   Edit5.Enabled := CheckBox4.Checked;
end;


procedure TMapMaskForm.CheckBox7Click(Sender: TObject);
begin
  MDDef.RegionsStreamMask := CheckBox7.Checked;
end;

procedure TMapMaskForm.CheckBox8Click(Sender: TObject);
begin
   MDdef.ExcludeStreamsInMask := CheckBox9.Checked;
end;

procedure TMapMaskForm.CheckBox9Click(Sender: TObject);
begin
   MDDef.ExcludeRoadsInMask := CheckBox8.Checked;
end;

procedure TMapMaskForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TMapMaskForm.FormCreate(Sender: TObject);
begin
   with MDDef.LocalOptimaOpts do begin
      Working := false;
      if MDDef.ShapeMaskNearPoints then RadioGroup3.ItemIndex:= 0 else RadioGroup3.ItemIndex:= 1;
      if MDDef.ShapeMaskNearLines then RadioGroup4.ItemIndex:= 0 else RadioGroup4.ItemIndex:= 1;
      if MDDef.ShapeMaskNearInsideAreas then RadioGroup5.ItemIndex:= 0 else RadioGroup5.ItemIndex:= 1;
      Edit1.Text := IntToStr(MDDef.StreamMaskDist);
      Edit2.Text := IntToStr(MDDef.MaskOpacity);
      Edit3.Text := IntToStr(RoadMaskDistance);
      Edit5.Text := RealToString(MaxSlope,-12,-2);
      Edit7.Text := IntToStr(MDDef.ShapePointBufferDist);
      Edit8.Text := IntToStr(MDDef.ShapeLineBufferDist);
      Edit9.Text := IntToStr(MDDef.ShapeAreaBufferDist);
      CheckBox1.Checked := RoadMask;
      CheckBox2.Checked := MDDef.RegionsRidgeMask;
      CheckBox3.Checked := MDDef.RegionsShapeFileMask;
      CheckBox4.Checked := SlopeMask;
      CheckBox7.Checked := MDDef.RegionsStreamMask;
      CheckBox9.Checked := MDDef.ExcludeStreamsInMask;
      CheckBox8.Checked := MDDef.ExcludeRoadsInMask;
      CheckBox10.Checked := MDDef.InvertRidgeMask;
      CheckBox11.Checked := MDDef.ExcludeTerrainCatInMask;
   end;
  Label11.Caption := ShapeFileMaskDirectory;
  Label3.Caption := ExtractFileName(ShapeFileMaskFile);

  RadioGroup3.Enabled := (ShapeFileMaskDirectory <> '') or (ShapeFileMaskFile <> '');
  RadioGroup4.Enabled := RadioGroup3.Enabled;
  RadioGroup5.Enabled := RadioGroup3.Enabled;

  {$IfDef ExTiger}
     Panel2.Visible := false;
     Panel6.Visible := false;
  {$EndIf}
  PlaceFormAtMousePosition(Self);
end;

procedure TMapMaskForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\map_masking.htm');
end;

procedure TMapMaskForm.OKBtnClick(Sender: TObject);
begin
   CheckSettings;
   Close;
end;

procedure TMapMaskForm.RadioGroup3Click(Sender: TObject);
begin
   RedrawSpeedButton12Click(Sender);
end;

procedure TMapMaskForm.RadioGroup4Click(Sender: TObject);
begin
   RedrawSpeedButton12Click(Sender);
end;

procedure TMapMaskForm.RadioGroup5Click(Sender: TObject);
begin
   RedrawSpeedButton12Click(Sender);
end;

procedure TMapMaskForm.RedrawSpeedButton12Click(Sender: TObject);
begin
    if (Not Working) then begin
       Working := true;
       CheckSettings;
       if AreaShapeFile(GISdb[DBUsed].ShapeFileType) then begin
          GISdb[DBUsed].MaskIn := MDDef.ShapeMaskNearInsideAreas;
          GISdb[DBUsed].MaskingDistance := MDDef.ShapeAreaBufferDist;
       end
       else if LineShapeFile(GISdb[DBUsed].ShapeFileType) then begin
          GISdb[DBUsed].MaskIn := MDDef.ShapeMaskNearLines;
          GISdb[DBUsed].MaskingDistance := MDDef.ShapeLineBufferDist;
       end
       else begin
          GISdb[DBUsed].MaskIn := MDDef.ShapeMaskNearPoints;
          GISdb[DBUsed].MaskingDistance := MDDef.ShapePointBufferDist;
       end;
       GISdb[DBUsed].dbOpts.ShowBuffers := true;
       GISdb[DBUsed].dbOpts.Opacity := MDDef.MaskOpacity;
       GISdb[DBUsed].RedrawLayerOnMap;
    end;
    Working := false;
end;

initialization
finalization
   {$IfDef RecordMapMasking} WriteLineToDebugFile('RecordMapMasking active in map_masking'); {$EndIf}
   {$IfDef RecordGeomorphFilter} WriteLineToDebugFile('RecordGeomorphFilter active in map_masking'); {$EndIf}
   {$IfDef RecordMapMaskBMPs} WriteLineToDebugFile('RecordMapMaskBMPs active in map_masking'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing map_masking out'); {$EndIf}
end.


