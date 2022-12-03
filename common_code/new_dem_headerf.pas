unit new_dem_headerf;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$Define RecordEditProblems}

interface

uses
  Buttons,StdCtrls, ExtCtrls,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Petmar_types,PETMAR,DEMDefs;


type
  TDEMHeaderForm = class(TForm)
    HelpBitBtn8: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn6: TBitBtn;
    RadioGroup3: TRadioGroup;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    xcoord: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn2: TBitBtn;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox3: TComboBox;
    Edit8: TEdit;
    Edit9: TEdit;
    BitBtn3: TBitBtn;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    Label15: TLabel;
    ComboBox4: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label11: TLabel;
    Edit7: TEdit;
    Label9: TLabel;
    Edit6: TEdit;
    Label8: TLabel;
    Edit5: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    BitBtn5: TBitBtn;
    Label13: TLabel;
    ComboBox2: TComboBox;
    RadioGroup2: TRadioGroup;
    RadioGroup1: TRadioGroup;
    RadioGroup4: TRadioGroup;
    Memo1: TMemo;
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure HelpBitBtn8Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure Edit3Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     EditHeadRec : DEMDefs.tDEMheader;
     Editable    : boolean;
     DEM : integer;
     procedure SetUpDEMHeaderForm(aDEM : integer);
     //procedure LabelSpecialProjection;
     procedure WriteValues;
     procedure UpdateChoices;
     procedure DisableEdits;
end;


var
  DEMHeaderForm : TDEMHeaderForm;

implementation

{$R *.dfm}

uses
   PETMath,//Get_SPCS,
   DEM_Manager,
   BaseMap,DEMCoord,GetLatLn;


procedure  TDEMHeaderForm.WriteValues;
begin
   Edit1.Text := RealToString(EditHeadRec.DEMxSpacing,-24,-12);
   Edit2.Text := RealToString(EditHeadRec.DEMySpacing,-24,-12);
   Edit3.Text := RealToString(EditHeadRec.DEMSWCornerX,-24,-12);
   Edit4.Text := RealToString(EditHeadRec.DEMSWCornerY,-24,-12);
   Edit8.Text := RealToString(EditHeadRec.MaxElev,-18,-6);
   Edit9.Text := RealToString(EditHeadRec.MinElev,-18,-6);
end;


procedure TDEMHeaderForm.DisableEdits;
begin
   ComboBox2.Enabled := false;
   ComboBox3.Enabled := false;
   ComboBox4.Enabled := false;
   RadioGroup1.Enabled := false;
   RadioGroup2.Enabled := false;
   RadioGroup3.Enabled := false;
   RadioGroup4.Enabled := false;
   //BitBtn1.Enabled := false;
   BitBtn2.Enabled := false;
   BitBtn3.Enabled := false;
   Edit1.Enabled := false;
   Edit2.Enabled := false;
   Edit3.Enabled := false;
   Edit4.Enabled := false;
   Edit5.Enabled := false;
   Edit6.Enabled := false;
   Edit7.Enabled := false;
   Edit8.Enabled := false;
   Edit9.Enabled := false;
   Editable := false;
end;



procedure TDEMHeaderForm.RadioGroup2Click(Sender: TObject);
begin
   EditHeadRec.DEMUsed := RadioGroup2.ItemIndex;
   Memo1.Visible := RadioGroup2.ItemIndex = 2;
   UpdateChoices;
end;


procedure TDEMHeaderForm.SetUpDEMHeaderForm;
var
   su : tSpacingUnit;
   zu : tElevUnit;
   TStr : shortstring;
begin
   {$IfDef RecordEditProblems} WriteLineToDebugFile('TDEMHeaderForm.SetUpForm, DEM=' + IntToStr(DEM)); {$EndIf}
   DEM := aDEM;
   Caption := DEMGlb[DEM].AreaName + ' Header';
   EditHeadRec := DEMGlb[DEM].DEMHeader;
   TStr := ByteArrayToString(EditHeadRec.DMAMapDefinition.h_DatumCode);
   Label6.Caption := DatumName(TStr);
   for zu := Low(ElevUnitsAre) to High(ElevUnitsAre) do ComboBox3.Items.Add(ElevUnitsAre[zu]);
   for su := SpaceMeters to SpaceUSFeet do ComboBox4.Items.Add(SpacingUnits[su]);
   ComboBox4.Text := ComboBox4.Items[ord(EditHeadRec.DataSpacing)];
   ComboBox2.Text := DatumName(TStr);                 //ComboBox2.Items[ord(EditHeadRec.DigitizeDatum)];
   ComboBox3.Text := ComboBox3.Items[ord(EditHeadRec.ElevUnits)];
   RadioGroup1.ItemIndex := 0;
   RadioGroup3.ItemIndex := ord(EditHeadRec.DEMprecision);
   RadioGroup4.ItemIndex := EditHeadRec.RasterPixelIsGeoKey1025;

   if (EditHeadRec.LatHemi = 'S') then RadioGroup1.ItemIndex := 1;
   //LabelSpecialProjection;
   RadioGroup2.ItemIndex := EditHeadRec.DEMUsed;
   WriteValues;

   Memo1.Lines.Add(EditHeadRec.wktString);
   Memo1.Visible := EditHeadRec.wktString <> '';
   Edit5.Text := IntToStr(EditHeadRec.UTMZone);
   Edit6.Text := IntToStr(EditHeadRec.NumCol);
   Edit7.Text := IntToStr(EditHeadRec.NumRow);
   Label11.Caption := UTMZoneExtent(EditHeadRec.UTMZone);
   UpdateChoices;
end;


procedure TDEMHeaderForm.BitBtn5Click(Sender: TObject);
var
   TStr : shortstring;
begin
   TStr := ByteArrayToString(EditHeadRec.DMAMapDefinition.h_DatumCode);
   PickDatum('DEM',TStr);
   StringToByteArray(TStr,EditHeadRec.DMAMapDefinition.h_DatumCode);
   Label6.Caption := DatumName(TStr);
end;

procedure TDEMHeaderForm.BitBtn6Click(Sender: TObject);
var
   i : integer;
begin
   {$IfDef RecordEditProblems} WriteLineToDebugFile('Close edit header (OK button)'); {$EndIf}
   if Editable and AnswerIsYes('Rewrite DEM ' + DEMGlb[DEM].AreaName) then begin
      DEMGlb[DEM].DEMheader := EditHeadRec;
      if (RadioGroup1.ItemIndex = 0) then DEMGlb[DEM].DEMheader.LatHemi := 'N'
      else DEMGlb[DEM].DEMheader.LatHemi := 'S';
      DEMGlb[DEM].DEMFileName := ChangeFileExt(DEMGlb[DEM].DEMFileName,'.dem');
      EditHeadRec.RasterPixelIsGeoKey1025 := RadioGroup4.ItemIndex;
      if Memo1.Visible then begin
         EditHeadRec.WKTString := '';
         for i := 0 to pred(Memo1.Lines.Count) do EditHeadRec.WKTString := EditHeadRec.WKTString + Memo1.Lines[i];
      end;

      DEMGlb[DEM].DEMHeader := EditHeadRec;
      DEMGlb[DEM].WriteNewFormatDEM(DEMGlb[DEM].DEMFileName);
   end;
   close;
end;

procedure TDEMHeaderForm.BitBtn7Click(Sender: TObject);
begin
   {$IfDef RecordEditProblems} WriteLineToDebugFile('Close edit header (cancel button)'); {$EndIf}
   Close;
end;

procedure TDEMHeaderForm.HelpBitBtn8Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\micr4vci.htm');
end;


procedure TDEMHeaderForm.ComboBox2Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox2.Items.Count) do begin
      {$IfDef RecordEditProblems} WriteLineToDebugFile(UpperCase(ComboBox2.Text) + '---' + UpperCase(ComboBox2.Items[i])); {$EndIf}
      if UpperCase(ComboBox2.Text) = UpperCase(ComboBox2.Items[i]) then begin
         EditHeadRec.DigitizeDatum := tDigitizeDatum(i);
         //LabelSpecialProjection;
      end;
   end;
end;

procedure TDEMHeaderForm.UpdateChoices;
begin
   BitBtn2.Enabled := Editable and (EditHeadRec.DEMUsed = ArcSecDEM);
   if (EditHeadRec.DEMUsed = ArcSecDEM) then begin
      Label7.Caption := LatLongDegreeToString(EditHeadRec.DEMSWCornerY,EditHeadRec.DEMSWCornerX,MDdef.OutPutLatLongMethod);
   end
   else Label7.Caption := '';
end;


procedure TDEMHeaderForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,EditHeadRec.DEMxSpacing)
end;

procedure TDEMHeaderForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,EditHeadRec.DEMySpacing)
end;


procedure TDEMHeaderForm.Edit3Click(Sender: TObject);
begin
   CheckEditString(Edit3.Text,EditHeadRec.DEMSWCornerX);
end;

procedure TDEMHeaderForm.Edit4Change(Sender: TObject);
begin
    CheckEditString(Edit4.Text,EditHeadRec.DEMSWCornerY);
end;

procedure TDEMHeaderForm.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,EditHeadRec.UTMZone);
   Label11.Caption := UTMZoneExtent(EditHeadRec.UTMZone);
end;

procedure TDEMHeaderForm.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,EditHeadRec.NumCol);
end;


procedure TDEMHeaderForm.Edit7Change(Sender: TObject);
begin
   CheckEditString(Edit7.Text,EditHeadRec.NumRow);
end;

procedure TDEMHeaderForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
   i : integer;
begin
   {$IfDef RecordEditProblems} WriteLineToDebugFile('TDEMHeaderForm.FormClose'); {$EndIf}
   Action := caFree;
   i := 1;
   if Editable then CloseSingleDEM(i);
end;

procedure TDEMHeaderForm.FormCreate(Sender: TObject);
begin
    Editable := true;
    Petmar.PlaceFormAtMousePosition(Self);
   {$IfDef HideHelpButtons} HelpBitBtn8.Visible := false; {$EndIf}
end;

procedure TDEMHeaderForm.BitBtn2Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   {$IfDef RecordEditProblems} WriteLineToDebugFile('Original SW corner: ' + LatLongDegreeToString(EditHeadRec.DEMSWCornerY,EditHeadRec.DEMSWCornerX,DecMinutes)); {$EndIf}
   Long := EditHeadRec.DEMSWCornerX;
   Lat := EditHeadRec.DEMSWCornerY;
   GetLatLongDefaultNoDatum('Lower left (SW) corner',Lat,Long);
   EditHeadRec.DEMSWCornerX := Long;
   EditHeadRec.DEMSWCornerY := Lat;
   Edit3.Text := RealToString(EditHeadRec.DEMSWCornerX,-24,-12);
   Edit4.Text := RealToString(EditHeadRec.DEMSWCornerY,-24,-12);
   {$IfDef RecordEditProblems}  WriteLineToDebugFile('Selected SW corner: ' + LatLongDegreeToString(EditHeadRec.DEMSWCornerY,EditHeadRec.DEMSWCornerX,DecMinutes));  {$EndIf}
   UpdateChoices;
end;


procedure TDEMHeaderForm.ComboBox3Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox3.Items.Count) do
      if ComboBox3.Text = ComboBox3.Items[i] then
         EditHeadRec.ElevUnits := tElevUnit(i);
end;

procedure TDEMHeaderForm.ComboBox4Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox4.Items.Count) do
      if ComboBox4.Text = ComboBox4.Items[i] then
          EditHeadRec.DataSpacing := tSpacingUnit(i);
end;


procedure TDEMHeaderForm.BitBtn3Click(Sender: TObject);
begin
   DEMGlb[DEM].CheckMaxMinElev;
   EditHeadRec.MinElev := DEMGlb[DEM].DEMheader.MinElev;
   EditHeadRec.MaxElev := DEMGlb[DEM].DEMheader.MaxElev;
   WriteValues;
end;


end.
