unit demfanparams;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordFanOptions}
{$EndIf}


interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, ExtCtrls,ColorGrd, Dialogs,
   DEMCurvature,
   PETMAR,DEMDefs, DEMCoord,DEMRange,
   PETMath,Petmar_types;

type
  TPickFanParams = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Button1: TButton;
    BitBtn1: TBitBtn;
    RadioGroup1: TRadioGroup;
    CheckBox2: TCheckBox;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox4: TCheckBox;
    BitBtn5: TBitBtn;
    CheckBox3: TCheckBox;
    RadioGroup2: TRadioGroup;
    procedure HelpBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
  private
    { Private declarations }
    procedure CheckColorBoxes;
  public
    { Public declarations }
    SetupDone : boolean;
    BaseBitmap : tMyBitmap;
  end;


function GetFanParameters : boolean;


implementation

{$R *.DFM}

uses
   DEM_fan_algorithm,Demdef_routines;


function GetFanParameters : boolean;
var
  PickFanParams: TPickFanParams;
begin
   {$IfDef RecordFanOptions} WriteLineToDebugFile('GetFanParams in'); {$EndIf}
   PickFanParams := TPickFanParams.Create(Application);
   with PickFanParams do begin
      SetupDone := false;
      Label1.Caption := IntervisiblityAlgorithmName(MDDef.wf);
      CheckBox2.Checked := MDDef.ShowFanLocation;
      CheckBox3.Checked := MDDef.ShowViewshedMixedPixels ;
      CheckBox4.Checked := MDDef.DisplayFanBitmaps;
      ColorBitBtn(BitBtn1,MDDef.FanColor);
      ColorBitBtn(BitBtn3,MDDef.MaskColor);
      ColorBitBtn(BitBtn5,MDDef.ViewshedMixedPixelColor);
      RadioGroup1.ItemIndex := ord(MDdef.wf.FanShowVisible);
      SetupDone := true;
      CheckColorBoxes;
      if MDDef.FanSaveExt = '.png' then RadioGroup2.ItemIndex := 0
      else if MDDef.FanSaveExt = '.gif' then RadioGroup2.ItemIndex := 2
      else RadioGroup2.ItemIndex := 1;

      if (ShowModal = mrCancel) then Result := false
      else
      begin
         Result := true;
         MDDef.ShowFanLocation := CheckBox2.Checked;
         MDDef.ShowViewshedMixedPixels := CheckBox3.Checked;
         MDDef.DisplayFanBitmaps := CheckBox4.Checked;
         case RadioGroup2.ItemIndex of
             0 : MDDef.FanSaveExt := '.png';
             2 : MDDef.FanSaveExt := '.gif';
             1 : MDDef.FanSaveExt := '.bmp';
         end;
      end;
      PickFanParams.Free;
   end;
   {$IfDef RecordFanOptions} WriteLineToDebugFile('GetFanParams in'); {$EndIf}
end;

procedure TPickFanParams.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme5lrn.htm');
end;

procedure TPickFanParams.Button1Click(Sender: TObject);
begin
   {$IfDef RecordFanOptions} WriteLineToDebugFile('Call EditRangeCircles'); {$EndIf}
   EditRangeCircles;
end;

procedure TPickFanParams.BitBtn1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,MDdef.FanColor);
end;

procedure TPickFanParams.CheckColorBoxes;
begin
   if SetupDone then begin
      MDDef.wf.FanShowVisible := tFanShow(RadioGroup1.ItemIndex);
      BitBtn1.Visible := MDDef.wf.FanMethod in [fmFanRadials,fmRadialIHS];
   end;
   BitBtn5.Enabled := CheckBox3.Checked;
end;

procedure TPickFanParams.CheckBox3Click(Sender: TObject);
begin
   CheckColorBoxes;
end;

procedure TPickFanParams.CheckBox4Click(Sender: TObject);
begin
   CheckColorBoxes;
end;

procedure TPickFanParams.RadioGroup2Click(Sender: TObject);
begin
   CheckColorBoxes;
end;

procedure TPickFanParams.RadioGroup1Click(Sender: TObject);
begin
   CheckColorBoxes;
end;

procedure TPickFanParams.BitBtn2Click(Sender: TObject);
begin
   GetFanAlgorithmParameters(MDDef.wf);
   Label1.Caption := IntervisiblityAlgorithmName(MDDef.wf);
end;

procedure TPickFanParams.BitBtn3Click(Sender: TObject);
begin
   QueryColor(BitBtn3,MDdef.MaskColor);       
end;


procedure TPickFanParams.FormCreate(Sender: TObject);
begin
    PlaceFormAtMousePosition(Self);
    Petmar.SymbolOnButton(BitBtn4,MDDef.DefaultCenterSymbol);
end;

procedure TPickFanParams.BitBtn4Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn4,MDDef.DefaultCenterSymbol,'Fan observer location');
end;

procedure TPickFanParams.BitBtn5Click(Sender: TObject);
begin
   QueryColor(BitBtn3,MDDef.ViewshedMixedPixelColor);
end;


initialization
finalization
   {$IfDef RecordFanOptions} WriteLineToDebugFile('RecordFanOptions active in demfanParams'); {$EndIf}
end.
