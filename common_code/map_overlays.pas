unit map_overlays;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
  //{$Define RecordOverlay}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  DEMMapf,DEMDefs, Vcl.ComCtrls, Vcl.ExtCtrls;


type
  TMapOverlayForm = class(TForm)
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    Panel3: TPanel;
    BitBtn2: TBitBtn;
    Panel4: TPanel;
    BitBtn3: TBitBtn;
    Panel5: TPanel;
    BitBtn4: TBitBtn;
    Panel6: TPanel;
    CheckBox5: TCheckBox;
    BitBtn5: TBitBtn;
    Panel7: TPanel;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Panel8: TPanel;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    Panel10: TPanel;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    CheckBox7: TCheckBox;
    BitBtn16: TBitBtn;
    CheckBox8: TCheckBox;
    Panel11: TPanel;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    Panel13: TPanel;
    BitBtn21: TBitBtn;
    BitBtn22: TBitBtn;
    Panel14: TPanel;
    BitBtn23: TBitBtn;
    BitBtn24: TBitBtn;
    Panel12: TPanel;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    CheckBox13: TCheckBox;
    TrackBar1: TTrackBar;
    CheckBox6: TCheckBox;
    Panel9: TPanel;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    SpeedButton1: TSpeedButton;
    CheckBox14: TCheckBox;
    Panel17: TPanel;
    BitBtn29: TBitBtn;
    BitBtn30: TBitBtn;
    TrackBar2: TTrackBar;
    BitBtn31: TBitBtn;
    Panel18: TPanel;
    BitBtn32: TBitBtn;
    BitBtn33: TBitBtn;
    TrackBar3: TTrackBar;
    CheckBox15: TCheckBox;
    Panel19: TPanel;
    BitBtn34: TBitBtn;
    BitBtn35: TBitBtn;
    TrackBar4: TTrackBar;
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn30Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BitBtn31Click(Sender: TObject);
    procedure BitBtn35Click(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure CheckBox15Click(Sender: TObject);
    procedure BitBtn34Click(Sender: TObject);
  private
    { Private declarations }
     procedure CheckPanels(RedrawMap : boolean = true);
     procedure UpdateOverlayOrder(Layer : tOverlayOrder; Add : boolean);
     procedure ReorderOverlays(Layer : tOverlayOrder;  MoveUp : boolean);
  public
    { Public declarations }
    TheMapForm : tMapForm;
    SettingUp,
    TrackBarringAllowed : boolean;
  end;

procedure ManageMapOverlays(MapForm : tMapForm);
procedure AddOrSubtractOverlay(MapForm : tMapForm; Layer: tOverlayOrder; Add : boolean);
procedure MoveOverlayUp(MapForm : tMapForm; Layer: tOverlayOrder;  MoveUp : boolean);
procedure AddOverlay(MapForm : tMapForm; Layer: tOverlayOrder);
procedure SubtractOverlay(MapForm : tMapForm; Layer: tOverlayOrder);


implementation

{$R *.dfm}

uses
  {$IfDef ExCartography}
  {$Else}
     tissot,
  {$EndIf}

  {$IfDef ExNaturalEarth}
  {$Else}
     NE_outlines,
  {$EndIf}

  Petmar, DEMTigerOps,DEMDef_routines,db_display_options, dem_gaz_opts,DEMDataBase,
  dem_plss_op, petmar_types, Petmar_db,usoutlines,DEM_PLSS,
  BaseMap, DEM_indexes,DEMCoord;


procedure ManageMapOverlays(MapForm : tMapForm);
var
   MapOverlayForm : TMapOverlayForm;
begin
   {$IfDef RecordOverlay} WriteLineToDebugFile('ManageMapOverlays  Fan Opacity=' + IntToStr(MDDef.FanOpacity) + '  Grid 2 opacity=' + IntToStr(MDDef.SecondGridOpacity)); {$EndIf}
   MapOverlayForm := TMapOverlayForm.Create(Application);
   MapOverlayForm.TheMapForm := MapForm;

   MapOverlayForm.SettingUp := true;
   MapOverlayForm.Caption := 'Overlays--' + MapForm.Caption;
   ColorBitBtn(MapOverlayForm.BitBtn14,MDdef.WaterColor);
   MapOverlayForm.TrackBar1.Position := MDDef.FanOpacity;
   MapOverlayForm.TrackBar2.Position := MDDef.SecondGridOpacity;
   MapOverlayForm.TrackBar3.Position := MDDef.OverlayOpacity;
   {$IfDef ExOSM}
   {$Else}
      MapOverlayForm.TrackBar4.Position := MDDef.OSMOpacity;
   {$EndIf}

   MapOverlayForm.CheckBox1.Checked := MapForm.OverlayUp(ovoTiger);
   MapOverlayForm.CheckBox2.Checked := MapForm.OverlayUp(ovoContours);
   MapOverlayForm.CheckBox2.Enabled := ValidDEM(MapForm.MapDraw.DEMonMap);
   MapOverlayForm.CheckBox3.Checked := MapForm.OverlayUp(ovoGrids);
   MapOverlayForm.CheckBox4.Visible := (MDDef.ProgramOption in [ExpertProgram,DragonPlotProgram]) and MDDef.ShowPLSS;
   MapOverlayForm.CheckBox4.Checked := MapForm.OverlayUp(ovoPLSS);
   MapOverlayForm.CheckBox5.Checked := MapForm.OverlayUp(ovoGazetteer);
   MapOverlayForm.CheckBox7.Checked := MapForm.OverlayUp(ovoSRTMWater);
   MapOverlayForm.CheckBox7.Enabled := Petmar_types.ValidPath(MainMapData + 'srtm-swbd\');
   MapOverlayForm.CheckBox8.Checked := MapForm.OverlayUp(ovoCartoDB);
   MapOverlayForm.CheckBox9.Checked := MapForm.OverlayUp(ovoUSOutlines);
   MapOverlayForm.CheckBox10.Checked := MapForm.OverlayUp(ovoWorldOutlines);
   MapOverlayForm.CheckBox13.Checked := MapForm.OverlayUp(ovoFans);
   MapOverlayForm.CheckBox6.Checked := MapForm.OverlayUp(ovoTissot);
   MapOverlayForm.CheckBox14.Checked := MapForm.OverlayUp(ovoSecondGrid);
   MapOverlayForm.CheckBox15.Checked := MapForm.OverlayUp(ovoOSM);
   MapOverlayForm.CheckBox7.Visible := (MDDef.ProgramOption = ExpertProgram) and ValidPath(MainMapData + 'srtm-swbd\');
   MapOverlayForm.CheckBox5.Visible := (MDDef.ProgramOption in [ExpertProgram,DragonPlotProgram]) and MDDef.UseGazetteer;
   MapOverlayForm.CheckBox13.Visible := (MapForm.MapDraw.AllFansCoverageFName <> '');
   MapOverlayForm.CheckPanels(false);
   MapOverlayForm.TrackBarringAllowed := true;
   MapOverlayForm.SettingUp := false;

   MapOverlayForm.Show;
   {$IfDef RecordOverlay} WriteLineToDebugFile('ManageOverlays setup out'); {$EndIf}
end;


procedure AddOverlay(MapForm : tMapForm; Layer: tOverlayOrder);
begin
   AddOrSubtractOverlay(MapForm,Layer,true);
end;


procedure SubtractOverlay(MapForm : tMapForm; Layer: tOverlayOrder);
begin
   AddOrSubtractOverlay(MapForm,Layer,false);
end;


procedure AddOrSubtractOverlay(MapForm : tMapForm; Layer : tOverlayOrder;  Add : boolean);
var
   i,j : integer;
begin
   if (MapForm = nil) then exit;
   
   if Add then begin
      {$IfDef RecordOverlay} WriteLineToDebugFile('Add overlay=' + LayerName[Layer]); {$EndIf}
      for i := 1 to MaxOverlays do begin
         if (MapForm.MapDraw.OverLayOrder[i] = Layer) then break;
         if (MapForm.MapDraw.OverLayOrder[i] = ovoUnused) then begin
            MapForm.MapDraw.OverLayOrder[i] := Layer;
            if (i > 1) then for j := 1 to pred(i) do MoveOverlayUp(MapForm,Layer,true);
            if (MapForm.MapDraw.OverLayOrder[2] = ovoGrids) then MoveOverlayUp(MapForm,ovoGrids,true);
            exit;
         end;
      end;
   end
   else begin
      {$IfDef RecordOverlay}  WriteLineToDebugFile('Delete overlay=' + LayerName[Layer]); {$EndIf}
      for i := 1 to MaxOverlays do begin
         if(MapForm.MapDraw.OverLayOrder[i] = Layer) then begin
            for j := i to pred(MaxOverlays) do MapForm.MapDraw.OverLayOrder[j] := MapForm.MapDraw.OverLayOrder[succ(j)];
            MapForm.MapDraw.OverLayOrder[MaxOverlays] := ovoUnused;
            exit;
         end;
      end;
   end;
end;


procedure MoveOverlayUp(MapForm : tMapForm; Layer: tOverlayOrder;  MoveUp : boolean);
var
   i : integer;
begin
   if MoveUp then begin
      for i := 2 to MaxOverlays do begin
         if MapForm.MapDraw.OverLayOrder[i] = Layer then begin
            MapForm.MapDraw.OverLayOrder[i] := MapForm.MapDraw.OverlayOrder[pred(i)];
            MapForm.MapDraw.OverlayOrder[pred(i)] := Layer;
            exit;
         end;
      end;
   end;
end;


procedure TMapOverlayForm.ReorderOverlays(Layer : tOverlayOrder;  MoveUp : boolean);
begin
   MoveOverlayUp(TheMapForm,Layer,Moveup);
   CheckPanels;
end;


procedure TMapOverlayForm.SpeedButton1Click(Sender: TObject);
begin
   TheMapForm.DataBaseSpeedButton28Click(Sender);
   CheckPanels(false);
end;

procedure TMapOverlayForm.TrackBar1Change(Sender: TObject);
begin
   if TrackBarringAllowed then begin
      TrackBarringAllowed := false;
      MDDef.OverlayOpacity := TrackBar3.Position;
      MDDef.SecondGridOpacity := TrackBar2.Position;
      MDDef.FanOpacity := TrackBar1.Position;
      ShowSatProgress := false;
      TheMapForm.DoFastMapRedraw;
      ShowSatProgress := true;
      TrackBarringAllowed := true;
   end;
end;


procedure TMapOverlayForm.TrackBar4Change(Sender: TObject);
begin
   if TrackBarringAllowed then begin
      TrackBarringAllowed := false;
      {$IfDef ExOSM}
      {$Else}
         MDDef.OSMOpacity := TrackBar4.Position;
      {$EndIf}
      ShowSatProgress := false;
      TheMapForm.DoFastMapRedraw;
      ShowSatProgress := true;
      TrackBarringAllowed := true;
   end;
end;

procedure TMapOverlayForm.UpdateOverlayOrder(Layer: tOverlayOrder;   Add: boolean);
begin
   if not SettingUp then begin
      AddOrSubtractOverlay(TheMapForm,Layer,Add);
      CheckPanels;
      TheMapForm.DoFastMapRedraw;
   end;
end;


procedure TMapOverlayForm.CheckPanels(RedrawMap : boolean = true);
var
   i,Top : integer;
   NeedAdd : boolean;

   procedure SetPanel(Panel : tPanel);
   begin
      Panel.Top := Top;
      Top := Top + Panel.Height;
      Panel.Left := 0;
      Panel.Width := ClientWidth;
   end;

begin
   if SettingUp then exit;

   {$IfDef RecordOverlay}  WriteLineToDebugFile('TMapOverlayForm.CheckPanels in'); {$EndIf}
   BitBtn6.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoTiger;
   BitBtn7.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoGrids;
   BitBtn8.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoContours;
   BitBtn9.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoDatabases;
   BitBtn10.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoGazetteer;
   BitBtn11.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoPLSS;
   BitBtn13.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoTissot;
   BitBtn15.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoSRTMWater;
   BitBtn18.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoCartoDB;
   BitBtn20.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoFans;
   BitBtn30.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoSecondGrid;
   BitBtn35.Enabled := TheMapForm.MapDraw.OverlayOrder[1] <> ovoOSM;

   Panel2.Visible := CheckBox1.Checked;
   Panel3.Visible := CheckBox3.Checked;
   Panel4.Visible := CheckBox2.Checked;
   Panel6.Visible := CheckBox5.Checked;
   Panel8.Visible := CheckBox4.Checked;
   Panel9.Visible := CheckBox6.Checked;
   Panel10.Visible := CheckBox7.Checked;
   Panel11.Visible := CheckBox8.Checked;
   Panel12.Visible := CheckBox13.Checked;
   Panel13.Visible := CheckBox10.Checked;
   Panel14.Visible := CheckBox9.Checked;
   Panel17.Visible := CheckBox14.Checked;
   Panel18.Visible := TheMapForm.MapDraw.MapOverlays.ovVectorFiles.Count > 0;
   Panel19.Visible := CheckBox15.Checked;

   Panel5.Visible := (DEMDatabase.NumOpenDatabaseThisMap(TheMapForm) > 0);
   if Panel5.Visible then begin
      NeedAdd := true;
      for i := 1 to MaxOverlays do
         if (TheMapForm.MapDraw.OverLayOrder[i] = ovoDatabases) then NeedAdd := false;
      if NeedAdd then AddOrSubtractOverlay(TheMapForm,ovoDatabases,True);
   end;

   Top := Panel1.Height;
   for i := 1 to MaxOverlays do begin
      {$IfDef RecordOverlay} if (TheMapForm.MapDraw.OverLayOrder[i] <> ovoUnused) then WriteLineToDebugFile('TMapOverlayForm.CheckPanels Map layer: ' + LayerName[TheMapForm.MapDraw.OverLayOrder[i]]); {$EndIf}
      case TheMapForm.MapDraw.OverLayOrder[i] of
          ovoTiger       : SetPanel(Panel2);
          ovoGrids       : SetPanel(Panel3);
          ovoContours    : SetPanel(Panel4);
          ovoDatabases   : SetPanel(Panel5);
          ovoGazetteer   : SetPanel(Panel6);
          ovoPLSS        : SetPanel(Panel8);
          ovoFans        : SetPanel(Panel12);
          ovoSRTMWater   : SetPanel(Panel10);
          ovoCartoDB     : SetPanel(Panel11);
          ovoWorldOutlines : SetPanel(Panel13);
          ovoUSOutlines : SetPanel(Panel14);
          ovoTissot : SetPanel(Panel9);
          ovoSecondGrid : SetPanel(Panel17);
          ovoOSM : SetPanel(Panel19);
      end;
   end;
   if Panel18.Visible then SetPanel(Panel18);
   ClientHeight := Top + Panel7.height;
   ClientWidth := 254;

   If RedrawMap then begin
      {$IfDef RecordOverlay} WriteLineToDebugFile('TMapOverlayForm.CheckPanels redraw map'); {$EndIf}
      TheMapForm.DoFastMapRedraw;
   end;
   {$IfDef RecordOverlay} WriteLineToDebugFile('TMapOverlayForm.CheckPanels out'); {$EndIf}
end;


procedure TMapOverlayForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   TrackBarringAllowed := false;   //turn off until form is ready
end;

procedure TMapOverlayForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\map_overlay_manager.htm');
end;


procedure TMapOverlayForm.OKBtnClick(Sender: TObject);
begin
   {$IfDef RecordOverlay} WriteLineToDebugFile('TMapOverlayForm.OKBtnClick, Fan Opacity=' + IntToStr(MDDef.FanOpacity) + ' Grid 2 opacity=' + IntToStr(MDDef.SecondGridOpacity)); {$EndIf}
   Close;
end;

procedure TMapOverlayForm.BitBtn10Click(Sender: TObject);
begin
   ReorderOverlays(ovoGazetteer,true);
end;

procedure TMapOverlayForm.BitBtn11Click(Sender: TObject);
begin
   ReorderOverlays(ovoPLSS,true);
end;

procedure TMapOverlayForm.BitBtn12Click(Sender: TObject);
begin
  {$IfDef ExCartography}
  {$Else}
     Tissot.GetTissotOptions(TheMapForm);
  {$EndIf}
end;

procedure TMapOverlayForm.BitBtn13Click(Sender: TObject);
begin
   ReorderOverlays(ovoTissot,true);
end;

procedure TMapOverlayForm.BitBtn14Click(Sender: TObject);
begin
   QueryColor(BitBtn14,MDdef.WaterColor);
end;

procedure TMapOverlayForm.BitBtn15Click(Sender: TObject);
begin
   ReorderOverlays(ovoSRTMWater,true);
end;

procedure TMapOverlayForm.BitBtn16Click(Sender: TObject);
begin
   DEM_PLSS_op.SetPLSSPlot(TheMapForm);
end;

procedure TMapOverlayForm.BitBtn17Click(Sender: TObject);
begin
   if (TheMapForm.MapDraw.CartoGroupShapesUp = '') and CheckBox8.Checked then begin
      TheMapForm.MapDraw.CartoGroupShapesUp := DBDir + 'Groups\';
      Petmar.GetFileFromDirectory('cartographic shape file grouping',DefaultDBMask, TheMapForm.MapDraw.CartoGroupShapesUp);
   end;
   if (not CheckBox8.Checked) then TheMapForm.MapDraw.CartoGroupShapesUp := '';
   UpdateOverlayOrder(ovoCartoDB, CheckBox8.Checked and (TheMapForm.MapDraw.CartoGroupShapesUp <> ''));
   CheckPanels;
end;

procedure TMapOverlayForm.BitBtn18Click(Sender: TObject);
begin
   ReorderOverlays(ovoCartoDB,true);
end;
procedure TMapOverlayForm.BitBtn19Click(Sender: TObject);
begin
   TheMapForm.DoFastMapRedraw;
end;

procedure TMapOverlayForm.BitBtn1Click(Sender: TObject);
begin
   TheMapForm.ModifyTIGERdisplay1Click(Nil);
end;

procedure TMapOverlayForm.BitBtn20Click(Sender: TObject);
begin
   ReorderOverlays(ovoFans,true);
end;

procedure TMapOverlayForm.BitBtn21Click(Sender: TObject);
begin
  {$IfDef ExNaturalEarth}
  {$Else}
      SetNEOutlines(TheMapForm);
  {$EndIf}
end;

procedure TMapOverlayForm.BitBtn22Click(Sender: TObject);
begin
   ReorderOverlays(ovoWorldOutlines,true);
end;

procedure TMapOverlayForm.BitBtn23Click(Sender: TObject);
begin
   {$IfDef RecordOverlay} WriteLineToDebugFile('TMapOverlayForm.BitBtn23Click to SetUSOutlines'); {$EndIf}
   USOutlines.SetUSOutlines;
end;

procedure TMapOverlayForm.BitBtn24Click(Sender: TObject);
begin
   ReorderOverlays(ovoUSOutlines,true);
end;

procedure TMapOverlayForm.BitBtn27Click(Sender: TObject);
begin
   ReadDefault('Extend squares into next zone (' + DegSym + ')',MDDef.ExtendZoneSize);
end;

procedure TMapOverlayForm.BitBtn2Click(Sender: TObject);
begin
   TheMapForm.GridSpeedButton15Click(Nil);
end;

procedure TMapOverlayForm.BitBtn30Click(Sender: TObject);
begin
   UpdateOverlayOrder(ovoVectors, CheckBox2.Checked);
end;

procedure TMapOverlayForm.BitBtn31Click(Sender: TObject);
begin
   TheMapForm.DoFastMapRedraw;
end;

procedure TMapOverlayForm.BitBtn34Click(Sender: TObject);
begin
   if (TheMapForm.MapDraw.OSMShapesUp = Nil) and CheckBox15.Checked then begin
      TheMapForm.LoadOSMoverlay1Click(Nil);
   end;
   if (not CheckBox15.Checked) then FreeAndNil(TheMapForm.MapDraw.OSMShapesUp);
   UpdateOverlayOrder(ovoOSM, CheckBox15.Checked and (TheMapForm.MapDraw.OSMShapesUp <> Nil));
   CheckPanels;
end;

procedure TMapOverlayForm.BitBtn35Click(Sender: TObject);
begin
   ReorderOverlays(ovoOSM,true);
end;

procedure TMapOverlayForm.BitBtn3Click(Sender: TObject);
begin
   TheMapForm.OverlayUp(ovoContours);
   TheMapForm.Contourinterval1Click(Nil);
end;

procedure TMapOverlayForm.BitBtn4Click(Sender: TObject);
begin
   db_display_options.OpenMapTableOfContents(TheMapForm,false);
end;

procedure TMapOverlayForm.BitBtn5Click(Sender: TObject);
begin
   {$IfDef RecordOverlay} WriteLineToDebugFile('TMapOverlayForm.BitBtn5Click to SetGazOptions'); {$EndIf}
   with TheMapForm,MapDraw do DeleteSingleMapLayer(GazOverlayfName);
   DEM_Gaz_opts.SetGazOptions;
   TheMapForm.DoFastMapRedraw;
end;

procedure TMapOverlayForm.BitBtn6Click(Sender: TObject);
begin
   ReorderOverlays(ovoTiger,true);
end;

procedure TMapOverlayForm.BitBtn7Click(Sender: TObject);
begin
   ReorderOverlays(ovoGrids,true);
end;

procedure TMapOverlayForm.BitBtn8Click(Sender: TObject);
begin
   ReorderOverlays(ovoContours,true);
end;

procedure TMapOverlayForm.BitBtn9Click(Sender: TObject);
begin
   ReorderOverlays(ovoDatabases,true);
end;

procedure TMapOverlayForm.CheckBox10Click(Sender: TObject);
begin
   UpdateOverlayOrder(ovoWorldOutlines,CheckBox10.Checked);
end;

procedure TMapOverlayForm.CheckBox13Click(Sender: TObject);
begin
   if (TheMapForm.MapDraw.AllFansCoverageFName <> '') then UpdateOverlayOrder(ovoFans, CheckBox13.Checked);
end;

procedure TMapOverlayForm.CheckBox14Click(Sender: TObject);
begin
   UpdateOverlayOrder(ovoSecondGrid, CheckBox14.Checked);
   MDDef.FuzzyMatches := true;
   TheMapForm.MapDraw.SecondGridfName := '';
   if CheckBox14.Checked and (TheMapForm.MapDraw.DEM2onMap = 0) then begin
      TheMapForm.MapDraw.AssignSecondDEM(0);
   end
   else TheMapForm.MapDraw.DEM2onMap := 0;
   TheMapForm.DoFastMapRedraw;
end;

procedure TMapOverlayForm.CheckBox15Click(Sender: TObject);
begin
   BitBtn34Click(Sender);
end;

procedure TMapOverlayForm.CheckBox1Click(Sender: TObject);
begin
   UpdateOverlayOrder(ovoTiger, CheckBox1.Checked);
end;

procedure TMapOverlayForm.CheckBox2Click(Sender: TObject);
begin
   UpdateOverlayOrder(ovoContours, CheckBox2.Checked);
end;


procedure TMapOverlayForm.CheckBox3Click(Sender: TObject);
begin
   if not CheckBox3.Checked then MDDef.MapTicks := tixNone;
   UpdateOverlayOrder(ovoGrids, CheckBox3.Checked);
end;


procedure TMapOverlayForm.CheckBox4Click(Sender: TObject);
begin
   if CheckBox4.Checked then begin
      TryToOpenPLSS(TheMapForm);
      UpdateOverlayOrder(ovoPLSS, CheckBox4.Checked);
   end
   else begin
      CloseAllPLSS;
   end;
end;

procedure TMapOverlayForm.CheckBox5Click(Sender: TObject);
begin
   if CheckBox5.Checked and ((LastGazFile <> '') or GetGazFileName(LastGazFile)) then begin
      UpdateOverlayOrder(ovoGazetteer,CheckBox5.Checked);
   end;
   if not CheckBox5.Checked then begin
      AddOrSubtractOverlay(TheMapForm,ovoGazetteer,false);
      CheckPanels;
   end;
end;

procedure TMapOverlayForm.CheckBox6Click(Sender: TObject);
begin
   UpdateOverlayOrder(ovoTissot, CheckBox6.Checked);
end;

procedure TMapOverlayForm.CheckBox7Click(Sender: TObject);
begin
   UpdateOverlayOrder(ovoSRTMWater,CheckBox7.Checked);
end;

procedure TMapOverlayForm.CheckBox8Click(Sender: TObject);
begin
   BitBtn17Click(Sender);
end;


procedure TMapOverlayForm.CheckBox9Click(Sender: TObject);
begin
   UpdateOverlayOrder(ovoUSOutlines,CheckBox9.Checked);
end;


initialization
finalization
   {$IfDef RecordOverlay} WriteLineToDebugFile('RecordOverlay active in map_overlays'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing map_overlays'); {$EndIf}
end.
