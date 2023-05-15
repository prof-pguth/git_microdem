unit sc_COLENTER;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
    //{$Define ColEntryProblems}
{$EndIf}


interface

uses
    Windows, Classes, Graphics, Forms, Controls, Buttons,
    StdCtrls,ExtCtrls, Spin;

type
  TFullUnitEntryDlg = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    ShortLabel: TLabel;
    Longlabel: TLabel;
    ShortNameEdit: TEdit;
    LongNameEdit: TEdit;
    ThickLabel: TLabel;
    BaseLabel: TLabel;
    ThicknessEdit: TEdit;
    BaseAgeEdit: TEdit;
    BasePanel1: TPanel;
    BaseLabell: TLabel;
    WhatIsBase: TLabel;
    BaseListBox: TListBox;
    BaseWidthLabel: TLabel;
    BaseDepthLabel: TLabel;     
    BaseWidthEdit: TEdit;
    BaseDepthEdit: TEdit;
    Panel1: TPanel;
    TopLabel: TLabel;
    WhatIsTop: TLabel;
    TopListBox: TListBox;
    TopWidthLabel: TLabel;
    TopWidthEdit: TEdit;
    TopDepthLabel: TLabel;
    TopDepthEdit: TEdit;
    LithPanel: TPanel;
    LithListBox: TListBox;
    LithLabel: TLabel;
    TopAgeLabel: TLabel;
    TopAgeEdit: TEdit;
    Image1: TImage;
    SpinButton1: TSpinButton;
    ComboBox1: TComboBox;
    ResistancePanel: TPanel;
    RadioGroup1: TRadioGroup;
    procedure BaseListBoxClick(Sender: TObject);
    procedure TopListBoxClick(Sender: TObject);
    procedure LithListBoxClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure BaseWidthEditChange(Sender: TObject);
    procedure BaseDepthEditChange(Sender: TObject);
    procedure TopDepthEditChange(Sender: TObject);
    procedure TopWidthEditChange(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure TopAgeEditChange(Sender: TObject);
    procedure BaseAgeEditChange(Sender: TObject);
    procedure ThicknessEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
  private
    { Private declarations }
     procedure UpdateEnabled;
  public
    { Public declarations }
     procedure ShortEntry;
     procedure RedrawPattern;
  end;

var
  SimpleEdit   : boolean;

implementation

{$R *.DFM}

uses
   Petmar_types,PETMar,PetImage,sc_COLMain,Zipatone;


procedure TFullUnitEntryDlg.UpdateEnabled;
begin
   BaseWidthLabel.Enabled := BaseListBox.ItemIndex in [1,2,7];
   BaseDepthLabel.Enabled := BaseListBox.ItemIndex in [1,2,3,4,7];
   BaseWidthEdit.Enabled := BaseListBox.ItemIndex in [1,2,7];
   BaseDepthEdit.Enabled := BaseListBox.ItemIndex in [1,2,3,4,7];
   TopWidthLabel.Enabled := TopListBox.ItemIndex in [1,2,7];
   TopDepthLabel.Enabled := TopListBox.ItemIndex in [1,2,3,4,7];
   TopWidthEdit.Enabled := TopListBox.ItemIndex in [1,2,7];
   TopDepthEdit.Enabled := TopListBox.ItemIndex in [1,2,3,4,7];
end;


procedure TFullUnitEntryDlg.ShortEntry;
begin
   {$IfDef ColEntryProblems} WriteLineToDebugFile('TFullUnitEntryDlg.ShortEntry in'); {$EndIf}
   TopAgeLabel.Enabled := false;
   TopAgeEdit.Enabled := false;
   BaseLabel.Enabled := false;
   BaseAgeEdit.Enabled := false;

   ResistancePanel.Visible := false;
   ResistancePanel.Enabled := false;
   Panel1.Enabled := false;
   BasePanel1.Enabled := false;
   RadioGroup1.Enabled := false;

   Panel1.Visible := false;
   BasePanel1.Visible := false;
   TopListBox.Enabled := false;
   BaseListBox.Enabled := false;
   TopLabel.Enabled := false;
   BaseLabell.Enabled := false;
   WhatIsTop.Enabled := false;
   WhatIsBase.Enabled := false;
   LithPanel.Top := 160;
   LithPanel.Left := 10;
   Width := 310;
end;


procedure TFullUnitEntryDlg.RedrawPattern;
var
  Bitmap: tMyBitmap;
  UnitLith : string3;
begin
   {$IfDef ColEntryProblems} WriteLineToDebugFile('TFullUnitEntryDlg.RedrawPattern in'); {$EndIf}
  CreateBitmap(Bitmap,150,80);
   try
     ThisUnitBase :=  BaseType(BaseListBox.ItemIndex);
   except
      ThisUnitBase := Conformable;
   end;
  ThisUnitTop :=  BaseType(TopListBox.ItemIndex);
  if Copy(LithLabel.Caption,1,5) = 'Other' then
       UnitLith := Copy(LithLabel.Caption,8,3)
  else if (LithListBox.ItemIndex <> -1) then UnitLith := NamedPatternShortName[LithListBox.ItemIndex];

  CurrentPattern := Zipatone.PatternFromString(UnitLith);

  ThisBaseParam2 := 5;
  CheckEditString(BaseWidthEdit.Text,ThisBaseParam2);
  ThisBaseParam1 := 5;
  CheckEditString(BaseDepthEdit.Text,ThisBaseParam1);
  ThisTopParam2 := 5;
  CheckEditString(TopWidthEdit.Text,ThisTopParam2);
  ThisTopParam1 := 5;
  CheckEditString(TopDepthEdit.Text,ThisTopParam1);

  FillInPattern(Bitmap.Canvas,5,20,140,140,60,0);
  Image1.Picture.Graphic := Bitmap;
  Bitmap.Free;
  {$IfDef ColEntryProblems} WriteLineToDebugFile('TFullUnitEntryDlg.RedrawPattern out'); {$EndIf}
end;


procedure TFullUnitEntryDlg.BaseListBoxClick(Sender: TObject);
begin
   WhatIsBase.Caption := BaseListBox.Items[BaseListBox.ItemIndex];
   UpdateEnabled;
   RedrawPattern;
   SimpleEdit := false;
end;


procedure TFullUnitEntryDlg.TopListBoxClick(Sender: TObject);
begin
   WhatIsTop.Caption := TopListBox.Items[TopListBox.ItemIndex];
   UpdateEnabled;
   RedrawPattern;
   SimpleEdit := false;
end;

procedure TFullUnitEntryDlg.LithListBoxClick(Sender: TObject);
begin
   LithLabel.Caption := LithListBox.Items[LithListBox.ItemIndex];
   RedrawPattern;
end;


procedure TFullUnitEntryDlg.FormPaint(Sender: TObject);
begin
   UpdateEnabled;
end;


procedure TFullUnitEntryDlg.BaseWidthEditChange(Sender: TObject);
begin
   RedrawPattern;
   SimpleEdit := false;
end;

procedure TFullUnitEntryDlg.BaseDepthEditChange(Sender: TObject);
begin
   RedrawPattern;
   SimpleEdit := false;
end;

procedure TFullUnitEntryDlg.TopDepthEditChange(Sender: TObject);
begin
   RedrawPattern;
   SimpleEdit := false;
end;

procedure TFullUnitEntryDlg.TopWidthEditChange(Sender: TObject);
begin
   RedrawPattern;
   SimpleEdit := false;
end;


procedure TFullUnitEntryDlg.HelpBtnClick(Sender: TObject);
begin
   Application.HelpJump('stratcol\Unit_edit_window');
end;

procedure TFullUnitEntryDlg.TopAgeEditChange(Sender: TObject);
begin
   SimpleEdit := false;
end;

procedure TFullUnitEntryDlg.BaseAgeEditChange(Sender: TObject);
begin
   SimpleEdit := false;
end;

procedure TFullUnitEntryDlg.ThicknessEditChange(Sender: TObject);
var
   MeterThick : float64;
begin
   SimpleEdit := false;
   CheckEditString(ThicknessEdit.Text,MeterThick);
   OKBtn.Enabled := MeterThick > 0.0001;
end;

procedure TFullUnitEntryDlg.FormCreate(Sender: TObject);
begin
   SimpleEdit := true;
   BaseListBox.ItemIndex := 0;
   TopListBox.ItemIndex := 0;
   LithListBox.ItemIndex := 0;
   Petmar.CheckFormPlacement(Self);
end;

procedure TFullUnitEntryDlg.SpinButton1DownClick(Sender: TObject);
var
   i : integer;
begin
   if (LithListBox.ItemIndex > 0) then begin
      i := LithListBox.ItemIndex;
      dec(i);
      LithListBox.ItemIndex := i;
   end;
   LithLabel.Caption := LithListBox.Items[LithListBox.ItemIndex];
   RedrawPattern;
end;

procedure TFullUnitEntryDlg.SpinButton1UpClick(Sender: TObject);
var
   i : integer;
begin
   if LithListBox.ItemIndex < pred(LithListBox.Items.Count) then begin
      i := LithListBox.ItemIndex;
      inc(i);
      LithListBox.ItemIndex := i;
   end;
  LithLabel.Caption := LithListBox.Items[LithListBox.ItemIndex];
  RedrawPattern;
end;


initialization
finalization
   {$IfDef ColEntryProblems} WriteLineToDebugFile('ColEntryProblems active in sc_colenter'); {$EndIf}
end.

