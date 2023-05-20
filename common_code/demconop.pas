unit Demconop;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define DrawContours}
{$EndIf}


{$I nevadia_defines.inc}


interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,SysUtils,
   StdCtrls, Dialogs, ExtCtrls, ComCtrls,
   DEMMapf;

type
  TContourOptions = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    ColorDialog1: TColorDialog;
    Label6: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    Label9: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    UpDown2: TUpDown;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BitBtn7: TBitBtn;
    Label5: TLabel;
    CheckBox2: TCheckBox;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    RedrawSpeedButton12: TSpeedButton;
    procedure BitBtn7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
      MapOwner : tMapForm;
      procedure SetUpForm;
      procedure ColorButtons;
      procedure ExpressContourOptions;
  end;


var
  ContourOptions: TContourOptions;


implementation

{$R *.DFM}


uses
   PETMAR,Petmar_types,DEMDefs,petdbutils, demdef_routines;


procedure TContourOptions.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text, MDdef.DefaultContourInterval);
end;

procedure TContourOptions.ExpressContourOptions;
begin
   RadioGroup1.Enabled := false;
   BitBtn1.Enabled := false;
   BitBtn3.Enabled := false;
   BitBtn4.Enabled := false;
   BitBtn5.Enabled := false;
   RadioGroup1.ItemIndex := 1;
   SetUpForm;
end;


procedure TContourOptions.ColorButtons;
begin
   ColorBitBtn(BitBtn1,MDDef.ContourColor);
   ColorBitBtn(BitBtn2,MDDef.IndexColor);
   ColorBitBtn(BitBtn3,MDDef.TopContColor);
   ColorBitBtn(BitBtn4,MDDef.BotContColor);
   ColorBitBtn(BitBtn5,MDDef.ZeroColor);
   ColorBitBtn(BitBtn6,MDDef.OverlayContColor);
end;


procedure TContourOptions.SetUpForm;
begin
   ColorButtons;
   BitBtn1.Enabled := RadioGroup1.ItemIndex = 0;
   BitBtn2.Enabled := (RadioGroup1.ItemIndex = 0) or (CheckBox1.Checked);
   BitBtn3.Enabled := RadioGroup1.ItemIndex = 0;
   BitBtn4.Enabled := RadioGroup1.ItemIndex = 0;
   BitBtn5.Enabled := RadioGroup1.ItemIndex = 0;
   BitBtn6.Enabled := RadioGroup1.ItemIndex = 1;
   CheckBox1.Checked := MDDef.LabelContours;

   Label9.Caption := IntToStr(MDDef.ContourLineWidth);
   Label3.Caption := IntToStr(MDdef.IndexContWidth);
   UpDown1.Position := MDDef.ContourLineWidth;
   UpDown2.Position := MDDef.IndexContWidth;
   Edit1.Text := IntToStr(MDDef.DefaultContourInterval);
end;

procedure TContourOptions.FormCreate(Sender: TObject);
begin
   {$IfDef HideHelpButtons}
   HelpBtn.Visible := false;
   {$EndIf}
   RadioGroup1.ItemIndex := ord(MDdef.ContourColors);
   CheckBox2.Checked := false; 

   LoadMyFontIntoWindowsFont(MDDef.ContourLabelFont,Label5.Font);
   BitBtn7.Enabled := CheckBox1.Checked;
   Label5.Enabled := CheckBox1.Checked;
   MapOwner := Nil;

   Petmar.PlaceFormAtMousePosition(Self);
   SetupForm;
end;

procedure TContourOptions.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme327n.htm');
end;

procedure TContourOptions.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
   MDdef.ContourLineWidth := UpDown1.Position;
   Label9.Caption := IntegerToString(MDdef.ContourLineWidth,-8);
end;

procedure TContourOptions.BitBtn6Click(Sender: TObject);
begin
   QueryColor(BitBtn6,MDdef.OverlayContColor);
end;

procedure TContourOptions.BitBtn7Click(Sender: TObject);
begin
   EditMyFont(MDDef.ContourLabelFont);
   LoadMyFontIntoWindowsFont(MDDef.ContourLabelFont,Label5.Font);
end;

procedure TContourOptions.BitBtn8Click(Sender: TObject);
begin
    QueryColor(BitBtn8,MDdef.ContourColor);
    MDdef.IndexColor := MDdef.ContourColor;
    MDdef.TopContColor  := MDdef.ContourColor;
    MDdef.BotContColor  := MDdef.ContourColor;
    MDdef.ZeroColor  := MDdef.ContourColor;
    ColorButtons;
end;


procedure TContourOptions.BitBtn9Click(Sender: TObject);
begin
   DemDef_routines.SetContourDefaults;
end;

procedure TContourOptions.BitBtn1Click(Sender: TObject);
begin
    QueryColor(BitBtn1,MDdef.ContourColor);
end;

procedure TContourOptions.BitBtn2Click(Sender: TObject);
begin
   QueryColor(BitBtn2,MDdef.IndexColor);
end;

procedure TContourOptions.BitBtn3Click(Sender: TObject);
begin
   QueryColor(BitBtn3,MDdef.TopContColor);
end;

procedure TContourOptions.BitBtn4Click(Sender: TObject);
begin
   QueryColor(BitBtn4,MDdef.BotContColor);
end;

procedure TContourOptions.BitBtn5Click(Sender: TObject);
begin
   QueryColor(BitBtn5,MDdef.ZeroColor);
end;

procedure TContourOptions.RadioGroup1Click(Sender: TObject);
begin
   SetUpForm;
   MDdef.ContourColors := tContourColors(RadioGroup1.ItemIndex);
end;


procedure TContourOptions.RedrawSpeedButton12Click(Sender: TObject);
begin
   if (MapOwner <> Nil) then begin
      MapOwner.MapDraw.DeleteSingleMapLayer(MapOwner.MapDraw.ContourOverlayfName);
      MapOwner.DoBaseMapRedraw;
   end;
end;

procedure TContourOptions.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
   MDdef.IndexContWidth := UpDown2.Position;
   Label3.Caption := IntToStr(MDdef.IndexContWidth);
end;


procedure TContourOptions.CheckBox1Click(Sender: TObject);
begin
   MDDef.LabelContours := CheckBox1.Checked;
   BitBtn7.Enabled := CheckBox1.Checked;
   Label5.Enabled := CheckBox1.Checked;
end;

procedure TContourOptions.CheckBox2Click(Sender: TObject);
begin
  {$IfDef DrawContours} writeLineToDebugFile('TContourOptions.CheckBox2Click');  {$EndIf}
   //MDDef.ExportContourShapeFile := CheckBox2.Checked;
end;

initialization
finalization
   {$IfDef DrawContours} writeLineToDebugFile('DrawContours active in DemConOp'); {$EndIf}

   {$IfDef RecordClosingProblems}  WriteLineToDebugFile('Closing demconop'); {$EndIf}
end.
