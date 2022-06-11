unit dem_cart_proj;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordNewVectorMap}
      //{$Define RecordCartography}
   {$EndIf}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  System.Math,
  Petmar_types,
  DEMDefs,Petmath,Petmar;

type
  TProjectionDemForm = class(TForm)
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    BitBtn1: TBitBtn;
    GroupBox2: TGroupBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    GroupBox3: TGroupBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    GroupBox4: TGroupBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    GroupBox5: TGroupBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    Panel1: TPanel;
    Edit2: TEdit;
    Label2: TLabel;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox21: TCheckBox;
    HelpBtn: TBitBtn;
    BitBtn2: TBitBtn;
    Label5: TLabel;
    BitBtn4: TBitBtn;
    GroupBox6: TGroupBox;
    BitBtn3: TBitBtn;
    CheckBox16: TCheckBox;
    CheckBox24: TCheckBox;
    CheckBox23: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox28: TCheckBox;
    CheckBox22: TCheckBox;
    ComboBox2: TComboBox;
    Label3: TLabel;
    Edit3: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox20Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox21Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure CheckBox16Click(Sender: TObject);
    procedure CheckBox24Click(Sender: TObject);
    procedure CheckBox23Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    //procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox28Click(Sender: TObject);
    procedure CheckBox22Click(Sender: TObject);
  private
    { Private declarations }
   procedure UnCheckAllOthers(Sender: TObject);
  public
    { Public declarations }
    //LCCregion : tLCCregion;
  end;

procedure NewOrtographicMapForSeasonalTilt(aTitle : shortstring; Tilt : float64);
procedure PickProjections(SimpleMode : integer = 0);


implementation

{$R *.dfm}

uses
   DEMDataBase,PetImage, DEMdef_routines,petmar_ini_file,dem_manager,
   DEMMapf,BaseMap,map_overlays, tissot, nevadia_main;


procedure PickProjections(SimpleMode : integer = 0);
var
   ProjectionDemForm : TProjectionDemForm;
begin
   {$IfDef RecordCartography} WriteLineToDebugFile('Twmdem.PickProjections in'); {$EndIf}
   ProjectionDemForm := TProjectionDemForm.Create(Application);
   if (SimpleMode <> 0) then begin
      MDDef.OpenMultipleVectorMaps := true;
      MDDef.MapTicks := tixLatLong;
      GetNaturalEarthData;
      ProcessIniFile(iniInit,'Tissot');
      MDDef.DrawTissotDefault := true;
      ProjectionDemForm.CheckBox17.Checked := false;
      if (SimpleMode = 1) then begin
         ProjectionDemForm.CheckBox1.Checked := true;
         ProjectionDemForm.CheckBox2.Checked := true;
         ProjectionDemForm.CheckBox3.Checked := true;
      end
      else if (SimpleMode = 2) then begin
         ProjectionDemForm.CheckBox5.Checked := true;
         ProjectionDemForm.CheckBox6.Checked := true;
         ProjectionDemForm.CheckBox14.Checked := true;
         ProjectionDemForm.CheckBox17.Checked := true;
      end
      else if (SimpleMode = 3) then begin
         ProjectionDemForm.CheckBox7.Checked := true;
         ProjectionDemForm.CheckBox8.Checked := true;
      end
      else if (SimpleMode = 4) then begin
         ProjectionDemForm.CheckBox18.Checked := true;
      end;
      {$IfDef RecordCartography} WriteLineToDebugFile('Twmdem.PickProjections Call BitBtn1'); {$EndIf}
      ProjectionDemForm.BitBtn1Click(Nil);
      ProjectionDemForm.Close;
      {$IfDef RecordCartography} WriteLineToDebugFile('Twmdem.PickProjections Close projection'); {$EndIf}
      DisplayHTMLTopic('microdemtutorials/map_proj_distort.htm');
      wmdem.Tile;
   end
   else begin
      ProjectionDemForm.CheckBox9.Enabled := false;      //Cassini, major issues, p.92 in Snyder
      ProjectionDemForm.CheckBox15.Enabled := false;    //Hammer, problems

      (*
      ProjectionDemForm.CheckBox11.Enabled := false;    //Mollweide, works, could be improved
      ProjectionDemForm.CheckBox22.Enabled := false;    //Equal earth, works, graticule could be improved
      ProjectionDemForm.CheckBox12.Enabled := false;    //sinusoidal, works, but graticule could be improved

      ProjectionDemForm.CheckBox10.Enabled := false;    //Gnomonic, works
      ProjectionDemForm.CheckBox27.Enabled := false;    //Web Mercator, works
      ProjectionDemForm.CheckBox5.Enabled := false;    //EquiDistantCylindrical
      ProjectionDemForm.CheckBox13.Enabled := false;   //van der Grinten
      *)
      ProjectionDemForm.Show;
   end;
end;


procedure NewOrtographicMapForSeasonalTilt(aTitle : shortstring; Tilt : float64);
var
   LastVectorMap : integer;
begin
   LastVectorMap := SetUpVectorMap(false,true);
   VectorMap[LastVectorMap].MapDraw.PrimMapProj.PName := OrthoProj;
   SetUpDefaultNewProjection(VectorMap[LastVectorMap].MapDraw.PrimMapProj);
   VectorMap[LastVectorMap].MapDraw.BaseTitle := aTitle;
   VectorMap[LastVectorMap].MapDraw.PrimMapProj.Lat0 := tilt * DegtoRad;
   VectorMap[LastVectorMap].MapDraw.PrimMapProj.GetProjectParameters;
   VectorMap[LastVectorMap].DoCompleteMapRedraw;
   VectorMap[LastVectorMap].Keylatitudes1Click(nil);
   StopSplashing;
end;



procedure TProjectionDemForm.UnCheckAllOthers(Sender: TObject);
begin
   if not CheckBox20.Checked then begin
      if Sender <> CheckBox1 then CheckBox1.Checked := false;
      if Sender <> CheckBox2 then CheckBox2.Checked := false;
      if Sender <> CheckBox3 then CheckBox3.Checked := false;
      if Sender <> CheckBox4 then CheckBox4.Checked := false;
      if Sender <> CheckBox5 then CheckBox5.Checked := false;
      if Sender <> CheckBox6 then CheckBox6.Checked := false;
      if Sender <> CheckBox7 then CheckBox7.Checked := false;
      if Sender <> CheckBox8 then CheckBox8.Checked := false;
      if Sender <> CheckBox9 then CheckBox9.Checked := false;
      if Sender <> CheckBox10 then CheckBox10.Checked := false;
      if Sender <> CheckBox11 then CheckBox11.Checked := false;
      if Sender <> CheckBox12 then CheckBox12.Checked := false;
      if Sender <> CheckBox13 then CheckBox13.Checked := false;
      if Sender <> CheckBox14 then CheckBox14.Checked := false;
      if Sender <> CheckBox15 then CheckBox15.Checked := false;
      if Sender <> CheckBox17 then CheckBox17.Checked := false;
      if Sender <> CheckBox18 then CheckBox18.Checked := false;
      if Sender <> CheckBox19 then CheckBox19.Checked := false;
      if Sender <> CheckBox21 then CheckBox21.Checked := false;
      if Sender <> CheckBox27 then CheckBox27.Checked := false;
   end;
   ComboBox2.Enabled := CheckBox14.Checked;
   Edit3.Enabled := CheckBox18.Checked;
   Label3.Enabled := CheckBox18.Checked;
end;



procedure TProjectionDemForm.BitBtn1Click(Sender: TObject);
var
   LatC,LongC : float64;
   LastVectorMap : integer;


       procedure NewMap(Proj : tProjectType; fName : PathStr = '');
       var
          TStr : string;
       begin
           {$IfDef RecordNewVectorMap} WriteLineToDebugFile('New map in '); {$EndIf}
           LastVectorMap := SetUpVectorMap(false,true,Proj,fName);
           if (fName = '') then begin
              if CheckBox21.Checked then begin
                 VectorMap[LastVectorMap].MapDraw.PrimMapProj.Lat0 := LatC;
                 VectorMap[LastVectorMap].MapDraw.PrimMapProj.Long0 := LongC;
                 {$IfDef RecordNewVectorMap} WriteLineToDebugFile('New Map reset long cent=' + RadToDegString(VectorMap[LastVectorMap].MapDraw.PrimMapProj.long0)); {$EndIf}
              end;
           end;
           if (Proj = UTMEllipsoidal) then VectorMap[LastVectorMap].MapDraw.PrimMapProj.ProjUTMZone := MDDef.DefaultUTMZone;
           if (Proj = CylindricalEqualArea) then begin
              TStr := ComboBox2.Text;
              VectorMap[LastVectorMap].MapDraw.PrimMapProj.Phi1 := StrToFloat(Petmar_Types.BeforeSpecifiedCharacter(TStr,' ')) * Petmar_types.DegToRad;
              VectorMap[LastVectorMap].MapDraw.PrimMapProj.Lat0 := VectorMap[LastVectorMap].MapDraw.PrimMapProj.Phi1;
              VectorMap[LastVectorMap].MapDraw.PrimMapProj.GetProjectParameters;
              VectorMap[LastVectorMap].MapDraw.PrimMapProj.pNameModifier := Petmar_Types.AfterSpecifiedCharacter(TStr,' ');
              VectorMap[LastVectorMap].MapDraw.SetFullMapCoverage;
           end;
          {$IfDef RecordNewVectorMap} WriteLineToDebugFile('New Map=' + VectorMap[LastVectorMap].MapDraw.PrimMapProj.GetProjectionName); {$EndIf}
           SetUpDefaultNewProjection(VectorMap[LastVectorMap].MapDraw.PrimMapProj,false);
          {$IfDef RecordNewVectorMap} WriteLineToDebugFile('New Map after set default long cent=' + RadToDegString(VectorMap[LastVectorMap].MapDraw.PrimMapProj.long0)); {$EndIf}
           VectorMap[LastVectorMap].MapDraw.PrimMapProj.GetProjectParameters;
          {$IfDef RecordNewVectorMap} WriteLineToDebugFile('New Map after get parameters long cent=' + RadToDegString(VectorMap[LastVectorMap].MapDraw.PrimMapProj.long0)); {$EndIf}
           VectorMap[LastVectorMap].MapDraw.BaseTitle := VectorMap[LastVectorMap].MapDraw.PrimMapProj.GetProjectionName;
           VectorMap[LastVectorMap].DoCompleteMapRedraw;

           if MDDef.DrawTissotDefault then begin
              AddOrSubtractOverlay(VectorMap[LastVectorMap],ovoTissot,true);
              VectorMap[LastVectorMap].DoFastMapRedraw;
           end;
           {$IfDef RecordNewVectorMap} WriteLineToDebugFile('Created Map=' + VectorMap[LastVectorMap].MapDraw.PrimMapProj.GetProjectionName + '  size, ' + ImageSize(VectorMap[LastVectorMap].Image1) ); {$EndIf}
       end;

begin
   {$IfDef RecordNewVectorMap} WriteLineToDebugFile('TProjectionDemForm.BitBtn1Click'); {$EndIf}
   LongC := 0;
   LatC := 0;
   CheckEditString(Edit1.Text,LongC);
   CheckEditString(Edit2.Text,LatC);
   CheckEditString(Edit3.Text,MDDef.DefaultUTMZone);
   LongC := LongC * DegToRad;
   LatC := LatC * DegToRad;
   if CheckBox1.Checked then NewMap(OrthoProj);
   if CheckBox2.Checked then NewMap(OldStereographic);
   if CheckBox3.Checked then NewMap(LamAzEqArea);
   if CheckBox4.Checked then NewMap(Mercator);
   if CheckBox5.Checked then NewMap(EquiDistantCylindrical);
   if CheckBox6.Checked then NewMap(MillerCylindrical);
   if CheckBox7.Checked then NewMap(LambertConformalConicEllipse);
   if CheckBox8.Checked then NewMap(AlbersEqAreaConicalEllipsoid);
   if CheckBox9.Checked then NewMap(Cassini);
   if CheckBox10.Checked then NewMap(Gnomonic);
   if CheckBox11.Checked then NewMap(Mollweide);
   if CheckBox12.Checked then NewMap(SinusProj);
   if CheckBox13.Checked then NewMap(VanDerGrinten);
   if CheckBox14.Checked then NewMap(CylindricalEqualArea);
   if CheckBox15.Checked then NewMap(HammerProj);
   if CheckBox17.Checked then NewMap(MercatorEllipsoid);
   if CheckBox18.Checked then NewMap(UTMEllipsoidal);
   if CheckBox19.Checked then NewMap(PolarStereographicEllipsoidal);
   if CheckBox22.Checked then NewMap(EqualEarth);
   if CheckBox27.Checked then NewMap(WebMercator);
   if Label5.Enabled then NewMap(MercatorEllipsoid,VectorMapName);
   if MDDef.CloseCartFormOnOpen then Close;
   UpdateMenusForAllMaps;
end;


procedure TProjectionDemForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   MDDef.DefSubdueVectMap := CheckBox23.Checked;
   MDDef.DefGrayVectMap := CheckBox24.Checked;
   Action := caFree;
end;


procedure TProjectionDemForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   CheckBox20.Checked := MDDef.OpenMultipleVectorMaps;
   CheckBox16.Checked := MDDef.DrawTissotDefault;
   Label5.Caption := VectorMapName;
   Edit3.Text := IntToStr(MDDef.DefaultUTMZone);
   Label5.Enabled := false;
   CheckBox28.Checked := MDDef.CloseCartFormOnOpen;
   if (not MDDef.CloseCartFormOnOpen) then wmDEM.FormPlacementInCorner(self);
   CheckBox21Click(nil);
end;


procedure TProjectionDemForm.CheckBox20Click(Sender: TObject);
begin
   MDDef.OpenMultipleVectorMaps := CheckBox20.Checked;
end;


procedure TProjectionDemForm.CheckBox16Click(Sender: TObject);
begin
   MDDef.DrawTissotDefault := CheckBox16.Checked;
   BitBtn3.Enabled := MDDef.DrawTissotDefault;
end;


procedure TProjectionDemForm.CheckBox1Click(Sender: TObject);
begin
   UncheckAllOthers(Sender);
end;

procedure TProjectionDemForm.CheckBox21Click(Sender: TObject);
begin
   Edit1.Enabled := CheckBox21.Checked;
   Edit2.Enabled := CheckBox21.Checked;
   Label1.Enabled := CheckBox21.Checked;
   Label2.Enabled := CheckBox21.Checked;
end;


procedure TProjectionDemForm.CheckBox22Click(Sender: TObject);
begin
   UncheckAllOthers(Sender);
end;

procedure TProjectionDemForm.CheckBox23Click(Sender: TObject);
begin
   MDDef.DefSubdueVectMap := CheckBox23.Checked;
end;

procedure TProjectionDemForm.CheckBox24Click(Sender: TObject);
begin
   MDDef.DefGrayVectMap := CheckBox24.Checked;
end;


procedure TProjectionDemForm.CheckBox28Click(Sender: TObject);
begin
   MDDef.CloseCartFormOnOpen := CheckBox28.Checked;
end;


procedure TProjectionDemForm.Edit3Change(Sender: TObject);
begin
   Label3.Caption := 'UTM zone (' + UTMZoneExtent(StrToInt(Edit3.Text)) + ')';
end;

procedure TProjectionDemForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\vector_maps.htm');
end;

procedure TProjectionDemForm.BitBtn2Click(Sender: TObject);
begin
   if GetFileFromDirectory('Projection','*.prj',VectorMapName) then begin
      Label5.Caption := VectorMapName;
      Label5.Enabled := true;
      UnCheckAllOthers(Sender);
   end
   else Label5.Enabled := false;
end;

procedure TProjectionDemForm.BitBtn3Click(Sender: TObject);
begin
   Tissot.GetTissotOptions(Nil);
end;

initialization
finalization
   {$IfDef RecordNewVectorMap} WriteLineToDebugFile('RecordNewVectorMap in dem_cart_proj'); {$EndIf}
end.
