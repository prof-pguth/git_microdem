unit geomorph_point_class;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordPointClass}
   //{$Define RecordClosePointClass}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  DEMMapf,DEMDefs, ExtCtrls, Vcl.ComCtrls;

type
  TPointClassForm = class(TForm)
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    RadioGroup2: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Edit5: TEdit;
    Label5: TLabel;
    RadioGroup3: TRadioGroup;
    BitBtn6: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit4: TEdit;
    Edit3: TEdit;
    Edit6: TEdit;
    Label6: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Tabsheet2: TTabSheet;
    Edit17: TEdit;
    Label28: TLabel;
    Label27: TLabel;
    Edit16: TEdit;
    Edit15: TEdit;
    Label29: TLabel;
    Label26: TLabel;
    Edit14: TEdit;
    Edit13: TEdit;
    Label21: TLabel;
    RadioGroup5: TRadioGroup;
    BitBtn1: TBitBtn;
    Edit8: TEdit;
    Label8: TLabel;
    TabSheet3: TTabSheet;
    CheckBox2: TCheckBox;
    BitBtn7: TBitBtn;
    RadioGroup6: TRadioGroup;
    Panel1: TPanel;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    RadioGroup4: TRadioGroup;
    SpeedButton1: TSpeedButton;
    Label9: TLabel;
    Edit7: TEdit;
    BitBtn10: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
  private
    { Private declarations }
    procedure CheckValues;
    procedure CloseLastDEMCreated;
  public
    { Public declarations }
    MapOwner : tMapForm;
    SlopeGrid,RoughGrid,ConvexGrid,
    CurDEM,LastDEMCreated : integer;
    GridLimits : tGridLimits;
  end;


procedure PointClassification(CurDEM : integer; OptionsOnly : boolean = false);
procedure IwashishiandPikeclassificationMap(CurDEM : integer);


implementation

{$R *.dfm}


uses
   Petmar,Petmar_types,PetImage,Make_grid,
   DEMDef_Routines,DEMCoord,BaseMap,
   DEM_Manager,
   MD_use_tools,
   nevadia_main;


procedure PointClassification(CurDEM : integer; OptionsOnly : boolean = false);
var
   PointClassForm : TPointClassForm;
begin
   PointClassForm := TPointClassForm.Create(Application);
   PointClassForm.MapOwner := DEMGLB[CurDEM].SelectionMap;
   PointClassForm.CurDEM := CurDEM;
   if OptionsOnly then begin
      PointClassForm.BitBtn2.Enabled := false;
      PointClassForm.BitBtn4.Enabled := false;
      PointClassForm.FormStyle := fsStayOnTop;
      PointClassForm.Visible := false;
      PointClassForm.ShowModal;
   end;
   {$If Defined(RecordPointClass)} WriteLineToDebugFile('PointClassification out  ' + FormSize(PointClassForm)); {$EndIf}
end;


procedure IwashishiandPikeclassificationMap(CurDEM : integer);
var
   PointClassForm : TPointClassForm;
begin
   PointClassForm := TPointClassForm.Create(Application);
   PointClassForm.MapOwner := DEMGLB[CurDEM].SelectionMap;
   PointClassForm.CurDEM := CurDEM;
   PointClassForm.PageControl1.ActivePage := PointClassForm.Tabsheet2;
end;


procedure TPointClassForm.CloseLastDEMCreated;
begin
   if MDDef.OverWriteClassDEMs  and (LastDEMCreated <> 0) and (DEMGlb[LastDEMCreated] <> Nil) then begin
      CloseSingleDEM(LastDEMCreated);
      LastDEMCreated := 0;
   end;
end;


procedure TPointClassForm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.WoodRegionRadiusPixels);
end;

procedure TPointClassForm.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,MDdef.MinDeltaZToClassify);
end;

procedure TPointClassForm.Edit7Change(Sender: TObject);
begin
   CheckEditString(Edit7.Text,MDDef.SecondGridOpacity);
end;

procedure TPointClassForm.Edit8Change(Sender: TObject);
begin
   CheckEditString(Edit8.Text, MDDef.SSOBoxSizeMeters);
end;

procedure TPointClassForm.FormCreate(Sender: TObject);
begin
   {$If Defined(RecordPointClass)} WriteLineToDebugFile('TPointClassForm  ' + FormSize(Self)); {$EndIf}
   Edit1.Text := RealToString(MDDef.SlopeClassTolerance,-18,-2);
   Edit2.Text := RealToString(MDDef.ConvexClassTolerance,-18,-2);
   Edit3.Text := IntToStr(MDDef.WoodRegionRadiusPixels);
   Edit4.Text := RealToString(MDDef.MinSlopeToClassify,-18,-2);
   Edit5.Text := IntToStr(MDDef.RidgeMaskRadius);
   Edit6.Text := RealToString(MDDef.MinDeltaZToClassify,-18,-2);
   Edit7.Text := IntToStr(MDDef.SecondGridOpacity);
   Edit8.Text := IntToStr(MDDef.SSOBoxSizeMeters);

   RadioGroup1.ItemIndex := ord(MDDef.RidgePointClassify);
   CheckBox1.Checked := MDDef.InvertRidgeMask;
   CheckBox3.Checked := MDDef.ExaggeratePtClass;
   CheckBox4.Checked := MDDef.OverWriteClassDEMs;
   RadioGroup4.ItemIndex := MDDef.MaskMapShow;

   if MDDef.GeomorphMapsFullDEM then RadioGroup3.ItemIndex := 0
   else RadioGroup3.ItemIndex := 1;

   case MDDef.ValleyRidgeThreshold  of
      5 : RadioGroup2.ItemIndex := 2;
      6 : RadioGroup2.ItemIndex := 1;
      7 : RadioGroup2.ItemIndex := 0;
   end;

   Edit13.Text := RealToString(MDDef.SlopeCut1,-12,-1);
   Edit14.Text := RealToString(MDDef.SlopeCut2,-12,-1);
   Edit15.Text := RealToString(MDDef.SlopeCut3,-12,-1);
   Edit16.Text := RealToString(MDDef.ConvexCut,-12,-1);
   Edit17.Text := RealToString(MDDef.RoughnessCut,-12,-1);
   if (MDDef.IwashPikeCats = 8) then RadioGroup5.ItemIndex := 0
   else if (MDDef.IwashPikeCats = 12) then RadioGroup5.ItemIndex := 1
   else RadioGroup5.ItemIndex := 2;

   LastDEMCreated := 0;
   SlopeGrid := 0;
   RoughGrid := 0;
   ConvexGrid := 0;
   wmDEM.FormPlacementInCorner(self,lpNEMap);
   {$If Defined(RecordPointClass)} WriteLineToDebugFile('TPointClassForm  ' + FormSize(Self)); {$EndIf}
end;


procedure TPointClassForm.BitBtn10Click(Sender: TObject);
var
   n1,n2 : integer;
begin
   Make_grid.NumHighLowNeighborsMaps(CurDEM,MDDef.WoodRegionRadiusPixels,MDdef.MinDeltaZToClassify,n1,n2);
end;

procedure TPointClassForm.BitBtn1Click(Sender: TObject);
begin
   {$If Defined(RecordPointClass)} WriteLineToDebugFile('TPointClassForm.BitBtn1Click in (I&P grid)'); {$EndIf}
   CheckValues;
   if (RoughGrid = 0) then begin
      if RadioGroup6.ItemIndex = 0 then RoughGrid := CreateRoughnessMap2(CurDEM,false,true)
      else RoughGrid := CreateRoughnessMap(CurDEM,false);
   end;
   if ConvexGrid = 0 then ConvexGrid := CreateProfileConvexityMap(CurDEM,false);
   if SlopeGrid = 0 then SlopeGrid := CreateSlopeMap(CurDEM,false);

   {$IfDef RecordClosePointClass} WriteLineToDebugFile(''); {$EndIf}
   {$IfDef RecordClosePointClass} WriteLineToDebugFile('TPointClassForm.BitBtn1Click, RoughGrid=' + IntToStr(RoughGrid) + '  proj=' + DEMGlb[RoughGrid].DEMMapProjection.ProjDebugName); {$EndIf}
   {$IfDef RecordClosePointClass} WriteLineToDebugFile('TPointClassForm.BitBtn1Click, ConvexGrid=' + IntToStr(ConvexGrid) + '  proj=' + DEMGlb[ConvexGrid].DEMMapProjection.ProjDebugName); {$EndIf}
   {$IfDef RecordClosePointClass} WriteLineToDebugFile('TPointClassForm.BitBtn1Click, SlopeGrid=' + IntToStr(SlopeGrid) + '  proj=' + DEMGlb[SlopeGrid].DEMMapProjection.ProjDebugName); {$EndIf}
   {$IfDef RecordClosePointClass} WriteLineToDebugFile(''); {$EndIf}

   Make_grid.CreateIwashishiPikeMap(DEMGlb[CurDEM].AreaName,CurDEM,SlopeGrid,RoughGrid,ConvexGrid);

   {$If Defined(RecordPointClass)} WriteLineToDebugFile('TPointClassForm.BitBtn1Click out (I&P grid)'); {$EndIf}
end;

procedure TPointClassForm.BitBtn2Click(Sender: TObject);
var
   Color : tColor;
begin
   CheckValues;
   MapOwner.DoFastMapRedraw;
   CloseLastDEMCreated;
   if Sender=BitBtn2 then Color := clRed
   else Color := clBlue;
   MapOwner.DrawRidgeMask(Color,(Sender = BitBtn2),true);
end;


procedure TPointClassForm.BitBtn3Click(Sender: TObject);
begin
   BitBtn6Click(Sender);
end;


procedure TPointClassForm.BitBtn4Click(Sender: TObject);
begin
   BitBtn6Click(Sender);
end;


procedure TPointClassForm.BitBtn6Click(Sender: TObject);
var
   What : tRidgeTypemap;
begin
   if (CurDEM <> 0) then begin
      CheckValues;
      CloseLastDEMCreated;
      if Sender = BitBtn6 then What := rtmAllPoints;
      if Sender = BitBtn4 then What := rtmRidge;
      if Sender = BitBtn3 then What := rtmStream;
      if (RadioGroup3.ItemIndex = 0) then GridLimits := DEMGlb[CurDEM].FullDEMGridLimits
      else GridLimits := DEMGlb[CurDEM].SelectionMap.MapDraw.MapAreaDEMGridLimits;
      LastDEMCreated := CreateRidgeMap(CurDEM,GridLimits,What,Memo1);
   end;
end;


procedure TPointClassForm.BitBtn7Click(Sender: TObject);
begin
   WBT_PennockLandformClass(MapOwner.GeotiffDEMNameOfMap,CheckBox2.Checked);
end;


procedure TPointClassForm.CancelBtnClick(Sender: TObject);
begin
   Close;
end;


procedure TPointClassForm.CheckBox4Click(Sender: TObject);
begin
   MDDef.OverWriteClassDEMs := CheckBox4.Checked;
end;


procedure TPointClassForm.CheckValues;
begin
   CheckEditString(Edit1.Text,MDDef.SlopeClassTolerance);
   CheckEditString(Edit2.Text,MDDef.ConvexClassTolerance);
   CheckEditString(Edit4.Text,MDDef.MinSlopeToClassify);
   CheckEditString(Edit5.Text,MDDef.RidgeMaskRadius);

   MDDef.RidgePointClassify := tRidgeAlgorithm(RadioGroup1.ItemIndex);
   MDDef.InvertRidgeMask := CheckBox1.Checked;

   MDDef.ExaggeratePtClass := CheckBox3.Checked;
   case RadioGroup2.ItemIndex of
      0 : MDDef.ValleyRidgeThreshold := 7;
      1 : MDDef.ValleyRidgeThreshold := 6;
      2 : MDDef.ValleyRidgeThreshold := 5;
   end;

   CheckEditString(Edit13.Text,MDDef.SlopeCut1);
   CheckEditString(Edit14.Text,MDDef.SlopeCut2);
   CheckEditString(Edit15.Text,MDDef.SlopeCut3);
   CheckEditString(Edit16.Text,MDDef.ConvexCut);
   CheckEditString(Edit17.Text,MDDef.RoughnessCut);
end;


procedure TPointClassForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   CheckValues;
   {$IfDef RecordClosePointClass} WriteLineToDebugFile('TPointClassForm.FormClose, RoughGrid=' + IntToStr(RoughGrid) + '  ' + DEMGlb[RoughGrid].DEMMapProjection.ProjDebugName); {$EndIf}
   {$IfDef RecordClosePointClass} WriteLineToDebugFile('TPointClassForm.FormClose, ConvexGrid=' + IntToStr(ConvexGrid) + '  ' + DEMGlb[RoughGrid].DEMMapProjection.ProjDebugName); {$EndIf}
   {$IfDef RecordClosePointClass} WriteLineToDebugFile('TPointClassForm.FormClose, SlopeGrid=' + IntToStr(SlopeGrid) + '  ' + DEMGlb[SlopeGrid].DEMMapProjection.ProjDebugName); {$EndIf}
   CloseSingleDEM(RoughGrid);
   CloseSingleDEM(ConvexGrid);
   CloseSingleDEM(SlopeGrid);
   Action := caFree;
end;


procedure TPointClassForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\point_class_form.htm');
end;


procedure TPointClassForm.RadioGroup1Click(Sender: TObject);
begin
   Edit1.Enabled := RadioGroup1.ItemIndex = 0;
   Edit2.Enabled := RadioGroup1.ItemIndex = 0;
   Edit4.Enabled := RadioGroup1.ItemIndex = 0;
   Label1.Enabled := RadioGroup1.ItemIndex = 0;
   Label2.Enabled := RadioGroup1.ItemIndex = 0;
   Label4.Enabled := RadioGroup1.ItemIndex = 0;
   RadioGroup2.Enabled := RadioGroup1.ItemIndex = 2;
   CheckValues;
end;


procedure TPointClassForm.RadioGroup3Click(Sender: TObject);
begin
   MDDef.GeomorphMapsFullDEM := RadioGroup3.ItemIndex = 0
end;

procedure TPointClassForm.RadioGroup4Click(Sender: TObject);
begin
   MDDef.MaskMapShow := RadioGroup4.ItemIndex;
end;

procedure TPointClassForm.RadioGroup5Click(Sender: TObject);
begin
  Case RadioGroup5.ItemIndex of
     0 : MDDef.IwashPikeCats := 8;
     1 : MDDef.IwashPikeCats := 12;
     2 : MDDef.IwashPikeCats := 16;
  end
end;

procedure TPointClassForm.RadioGroup6Click(Sender: TObject);
begin
   CloseSingleDEM(RoughGrid);
end;

procedure TPointClassForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   MapOwner.DoFastMapRedraw;
end;

initialization
finalization
   {$If Defined(RecordPointClass)} WriteLineToDebugFile('RecordPointClass active in geomorph_point_class'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing geomorph_point_class out'); {$EndIf}
end.
