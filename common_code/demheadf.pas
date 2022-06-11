unit Demheadf;

form removed 8/8/2017

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordEditProblems}
{$EndIf}


interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,SysUtils,StdCtrls, ExtCtrls,
   Petmar_types,PETMAR,DEMDefs;

type
  TEditHeader = class(TForm)
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Edit6: TEdit;
    Edit7: TEdit;
    Label13: TLabel;
    ComboBox2: TComboBox;
    RadioGroup1: TRadioGroup;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    xcoord: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    BitBtn2: TBitBtn;
    GroupBox3: TGroupBox;
    ComboBox4: TComboBox;
    Label4: TLabel;
    Label15: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox4: TGroupBox;
    ComboBox3: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit8: TEdit;
    Edit9: TEdit;
    BitBtn3: TBitBtn;
    RadioGroup2: TRadioGroup;
    BitBtn1: TBitBtn;
    Label10: TLabel;
    Label11: TLabel;
    RadioGroup3: TRadioGroup;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure UpdateChoices;
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBox4Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
  private
    { Private declarations }
   public
    { Public declarations }
     EditHeadRec : DEMDefs.tHeaderRecord;
     Editable    : boolean;
     DEMfName    : PathStr;
     procedure SetUpForm;
     procedure LabelSpecialProjection;
     procedure WriteValues;
end;



implementation

{$R *.DFM}

uses
   PETMath,Get_SPCS,DEM_Manager,
   DEMDatum,DEMCoord,GetLatLn;


procedure TEditHeader.WriteValues;
begin
   Edit1.Text := RealToString(EditHeadRec.fLongInterval,-24,-12);
   Edit2.Text := RealToString(EditHeadRec.fLatInterval,-24,-12);
   Edit3.Text := RealToString(EditHeadRec.hdfSWCornerX,-24,-12);
   Edit4.Text := RealToString(EditHeadRec.hdfSWCornerY,-24,-12);
   Edit8.Text := RealToString(EditHeadRec.MaxElev,-18,-6);
   Edit9.Text := RealToString(EditHeadRec.MinElev,-18,-6);
end;


procedure TEditHeader.LabelSpecialProjection;
begin
   BitBtn1.Enabled := true;
   if ComboBox2.Text = 'Defined' then begin
      {$IfDef DisAllowV3DEMHeaders}
      {$Else}
      if (EditHeadRec.DefProjName = 'PRJ') then begin
         Label10.Caption := 'PRJ file';
         ComboBox2.Enabled := false;
      end
      else if FileExists(WriteDEMDir + EditHeadRec.DefProjName) or FileExists(ProgramRootDirectory + EditHeadRec.DefProjName) then
         Label10.Caption := EditHeadRec.DefProjName;
      {$EndIf}
   end
   else if ComboBox2.Text = 'SPCS' then begin
      Label10.Caption := 'FIPS zone ' + IntToStr(EditHeadRec.FIPS_Zone);
   end
   else begin
      Label10.Caption := '';
      BitBtn1.Enabled := false;
   end;
end;


procedure TEditHeader.SetUpForm;
var
   i  : integer;
   su : tSpacingUnit;
   zu : tElevUnit;
   TStr : shortstring;
begin
   TStr := ByteArrayToString(EditHeadRec.DMAMapDefinition.h_DatumCode);
   Label6.Caption := DatumName(TStr);

   if (EditHeadRec.FIPS_Zone <> 0) then begin
      LabelSpecialProjection;
   end;

   for zu := Meters to GLC2000 do ComboBox3.Items.Add(ElevUnitsAre[zu]);

   for su := SpaceMeters to SpaceUSFeet do ComboBox4.Items.Add(SpacingUnits[su]);
   ComboBox4.Text := ComboBox4.Items[ord(EditHeadRec.DataSpacing)];
   ComboBox2.Text := ComboBox2.Items[ord(EditHeadRec.DigitizeDatum)];
   ComboBox3.Text := ComboBox3.Items[ord(EditHeadRec.ElevUnits)];
   RadioGroup1.ItemIndex := 0;
   if (EditHeadRec.LatHemi = 'S') then RadioGroup1.ItemIndex := 1;
   LabelSpecialProjection;

   case EditHeadRec.DEMUsed of
      UTMBasedDEM  : RadioGroup2.ItemIndex := 1;
      ArcSecondDEM : RadioGroup2.ItemIndex := 0;
   end;
   WriteValues;

   Edit5.Text := IntToStr(EditHeadRec.UTMZone);
   Edit6.Text := IntToStr(EditHeadRec.NumCol);
   Edit7.Text := IntToStr(EditHeadRec.NumRow);
   Label11.Caption := DEMDatum.UTMZoneExtent(EditHeadRec.UTMZone);
   UpdateChoices;
   //Width := 427;
   //Height := 566;
end;

procedure TEditHeader.UpdateChoices;
var
   Lat,Long : float;
begin
   BitBtn2.Enabled := Editable and (EditHeadRec.DEMUsed = ArcSecondDEM);
   if (EditHeadRec.DEMUsed = ArcSecondDEM) then begin
      Long := EditHeadRec.hdfSWCornerX;
      Lat := EditHeadRec.hdfSWCornerY;
      Label7.Caption := LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod);
   end
   else Label7.Caption := '';
end;


procedure TEditHeader.RadioButton1Click(Sender: TObject);
begin
   EditHeadRec.DEMUsed := UTMBasedDEM;
   UpdateChoices;
end;

procedure TEditHeader.RadioButton2Click(Sender: TObject);
begin
   EditHeadRec.DEMUsed := ArcSecondDEM;
   UpdateChoices;
end;


procedure TEditHeader.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,EditHeadRec.fLongInterval)
end;

procedure TEditHeader.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,EditHeadRec.fLatInterval)
end;

procedure TEditHeader.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,EditHeadRec.UTMZone);
   Label11.Caption := DEMDatum.UTMZoneExtent(EditHeadRec.UTMZone);
end;

procedure TEditHeader.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,EditHeadRec.hdfSWCornerX);
end;

procedure TEditHeader.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,EditHeadRec.hdfSWCornerY);
end;


procedure TEditHeader.BitBtn2Click(Sender: TObject);
var
   Lat,Long : float;
begin
   {$IfDef RecordEditProblems}
   WriteLineToDebugFile('Original SW corner: ' + LatLongDegreeToString(EditHeadRec.hdfSWCornerY,EditHeadRec.hdfSWCornerX,DecMinutes),true);
   {$EndIf}
   Long := EditHeadRec.hdfSWCornerX;
   Lat := EditHeadRec.hdfSWCornerY;
   GetLatLongDefaultNoDatum('Lower left (SW) corner',Lat,Long);
   EditHeadRec.hdfSWCornerX := Long;
   EditHeadRec.hdfSWCornerY := Lat;
   Edit3.Text := RealToString(EditHeadRec.hdfSWCornerX,-24,-12);
   Edit4.Text := RealToString(EditHeadRec.hdfSWCornerY,-24,-12);

   {$IfDef RecordEditProblems}
   WriteLineToDebugFile('Selected SW corner: ' + LatLongDegreeToString(EditHeadRec.hdfSWCornerY,EditHeadRec.hdfSWCornerX,DecMinutes),true);
   {$EndIf}
   UpdateChoices;
end;


procedure TEditHeader.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,EditHeadRec.NumCol);
end;


procedure TEditHeader.Edit7Change(Sender: TObject);
begin
   CheckEditString(Edit7.Text,EditHeadRec.NumRow);
end;


procedure TEditHeader.BitBtn3Click(Sender: TObject);
begin
   DEMGlb[1].CheckMaxMinElev;
   EditHeadRec.MinElev := DEMGlb[1].HeadRecs.MinElev;
   EditHeadRec.MaxElev := DEMGlb[1].HeadRecs.MaxElev;
   WriteValues;
end;


procedure TEditHeader.ComboBox2Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox2.Items.Count) do begin
      {$IfDef RecordEditProblems}
      WriteLineToDebugFile(UpperCase(ComboBox2.Text) + '---' + UpperCase(ComboBox2.Items[i]));
      {$EndIf}
      if UpperCase(ComboBox2.Text) = UpperCase(ComboBox2.Items[i]) then begin
         EditHeadRec.DigitizeDatum := tDigitizeDatum(i);
         {$IfDef RecordEditProblems}
         WriteLineToDebugFile('Changed digitizing datum to : ' + DigitizeDatumName[EditHeadRec.DigitizeDatum]);
         {$EndIf}
         LabelSpecialProjection;
         exit;
      end;
   end;
end;


procedure TEditHeader.ComboBox3Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox3.Items.Count) do
      if ComboBox3.Text = ComboBox3.Items[i] then
         EditHeadRec.ElevUnits := tElevUnit(i);
end;


procedure TEditHeader.FormClose(Sender: TObject; var Action: TCloseAction);
var
   i : integer;
begin
   {$IfDef RecordEditProblems}
   WriteLineToDebugFileWithTime('TEditHeader.FormClose');
   {$EndIf}
   Action := caFree;
   i := 1;
   if Editable then CloseSingleDEM(i);
end;


procedure TEditHeader.ComboBox4Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox4.Items.Count) do
      if ComboBox4.Text = ComboBox4.Items[i] then
          EditHeadRec.DataSpacing := tSpacingUnit(i);
end;


procedure TEditHeader.BitBtn1Click(Sender: TObject);
var
   FileWanted : PathStr;
   SurveyUnits : tSPCSUnits;
begin
   if (ComboBox2.Text = 'SPCS') then begin
      EditHeadRec.FIPS_Zone := Get_SPCS.GetSPCSZoneFIPSNumber(SurveyUnits);
      case SurveyUnits of
          spcsMeters  : ComboBox4.Text := 'Meters';
          spcsIntFeet : ComboBox4.Text := 'Internation feet';
          spcsUSFeet  : ComboBox4.Text := 'US feet';
      end;
      EditHeadRec.DefProjName := '';
   end
   else begin
      FileWanted := WriteDEMDir;
      GetFileFromDirectory('Projection','*.prj',FileWanted);
      EditHeadRec.DefProjName := UpperCase(ExtractFileName(FileWanted));
   end;
   LabelSpecialProjection;
end;


procedure TEditHeader.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   {$IfDef HideHelpButtons}
   HelpBtn.Visible := false;
   {$EndIf}
end;


procedure TEditHeader.BitBtn5Click(Sender: TObject);
var
   TStr : shortstring;
begin
   TStr := ByteArrayToString(EditHeadRec.DMAMapDefinition.h_DatumCode);
   PickDatum('DEM',TStr);
   StringToByteArray(TStr,EditHeadRec.DMAMapDefinition.h_DatumCode);
   Label6.Caption := DatumName(TStr);
end;

procedure TEditHeader.BitBtn6Click(Sender: TObject);
begin
   {$IfDef RecordEditProblems}
   WriteLineToDebugFileWithTime('Close edit header (OK button)');
   {$EndIf}
   if Editable then begin
      if AnswerIsYes('Rewrite DEM') then begin
         CheckEditString(Edit8.Text,EditHeadRec.MaxElev);
         CheckEditString(Edit9.Text,EditHeadRec.MinElev);
         DEMGlb[1].HeadRecs := EditHeadRec;
         if (RadioGroup1.ItemIndex = 0) then DEMGlb[1].HeadRecs.LatHemi := 'N'
         else DEMGlb[1].HeadRecs.LatHemi := 'S';
         if (RadioGroup2.ItemIndex = 1) then DEMGlb[1].HeadRecs.DEMused := UTMBasedDEM
         else DEMGlb[1].HeadRecs.DEMused := ArcSecondDEM;
         DEMGlb[1].WriteNewFormatDEM(DEMGlb[1].DEMFileName);
      end;
   end;
   close;
end;

procedure TEditHeader.BitBtn7Click(Sender: TObject);
begin
   {$IfDef RecordEditProblems}
   WriteLineToDebugFileWithTime('Close edit header (cancel button)');
   {$EndIf}
   Close;
end;

procedure TEditHeader.BitBtn8Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\micr4vci.htm');
end;

initialization
finalization
   {$IfDef RecordEditProblems}
   WriteLineToDebugFile('RecordEditProblems active in demheadf');
   {$EndIf}
   {$IfDef RecordClosingProblems}
   WriteLineToDebugFile('Closing demheadf out');
   {$EndIf}
end.



