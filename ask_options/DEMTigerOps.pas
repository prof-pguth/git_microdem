unit demtigerops;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordTigerOps}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, Tabnotbk,
  PETMAR,Petmar_types,DEMdefs, ExtCtrls,
  DEMMapf;

type
  TTigerOverlayOptions = class(TForm)
    OKBtn: TBitBtn;
    Button1: TButton;
    CheckBox14: TCheckBox;
    HelpBtn: TBitBtn;
    CancelBtn: TBitBtn;
    CheckBox15: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    TabbedNotebook1: TTabbedNotebook;
    Label13: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    UpDown1: TUpDown;
    CheckBox8: TCheckBox;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    //BitBtn13: TBitBtn;
    CheckBox16: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Memo1: TMemo;
    Edit10: TEdit;
    Edit11: TEdit;
    Label12: TLabel;
    Edit12: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    //procedure BitBtn13Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RedrawSpeedButton12Click(Sender: TObject);
  private
    { Private declarations }
      procedure SetUpForm;
      procedure RoadLineWidths;
      procedure CheckCheckBoxes;
  public
    { Public declarations }
    Cancel : boolean;
    MapOwner : tMapForm;
  end;

function SetTigerOptions(inMapForm : tMapForm; PixelSize : float64) : boolean;


implementation

{$R *.DFM}

uses
   Nevadia_Main,
   Demdef_routines;


procedure TTigerOverlayOptions.RedrawSpeedButton12Click(Sender: TObject);
begin
   CheckCheckBoxes;
   MapOwner.RedrawTiger;
end;

procedure TTigerOverlayOptions.RoadLineWidths;
begin
   ColorLineWidthBitBtn(BitBtn1,MDdef.TigrDef.MajorRoadColor,MDdef.TigrDef.MajorRoadWidth);
   ColorLineWidthBitBtn(BitBtn2,MDdef.TigrDef.RoadCat2Color,MDdef.TigrDef.RoadCat2Width);
   ColorLineWidthBitBtn(BitBtn3,MDdef.TigrDef.RoadCat3Color,MDdef.TigrDef.RoadCat3Width);
   ColorLineWidthBitBtn(BitBtn4,MDdef.TigrDef.RoadCat4Color,MDdef.TigrDef.RoadCat4Width);
   ColorLineWidthBitBtn(BitBtn5,MDdef.TigrDef.RoadCat5Color,MDdef.TigrDef.RoadCat5Width);
   ColorLineWidthBitBtn(BitBtn6,MDdef.TigrDef.RoadCat6Color,MDdef.TigrDef.RoadCat6Width);
   ColorLineWidthBitBtn(BitBtn7,MDdef.TigrDef.RoadCat7Color,MDdef.TigrDef.RoadCat7Width);
end;


procedure TTigerOverlayOptions.SetUpForm;
begin
   {$IfDef RecordTigerOps} WriteLineToDebugFile('TTigerOverlayOptions.SetUpForm in'); {$EndIf}
     CheckBox1.Checked := MDdef.TigrDef.DrawMajorRoad;
     CheckBox2.Checked := MDdef.TigrDef.DrawRoadCat2;
     CheckBox3.Checked := MDdef.TigrDef.DrawRoadCat3;
     CheckBox4.Checked := MDdef.TigrDef.DrawRoadCat4;
     CheckBox5.Checked := MDdef.TigrDef.DrawRoadCat5;
     CheckBox6.Checked := MDdef.TigrDef.DrawRoadCat6;
     CheckBox7.Checked := MDdef.TigrDef.DrawRoadCat7;
     CheckBox16.Checked := MDdef.TigrDef.DrawLabels;
     CheckBox17.Checked := MDdef.MakeTigerMapGrayscale;
     CheckBox18.Checked := MDdef.SubdueTigerBase;
     CheckBox19.Checked := MDdef.LabelEveryTigerFeature;

     RoadLineWidths;

     CheckBox8.Checked := MDdef.TigrDef.DrawBoundary;
     ColorLineWidthBitBtn(BitBtn8,MDdef.TigrDef.BoundaryColor,MDdef.TigrDef.BoundaryWidth);

     CheckBox9.Checked := MDdef.TigrDef.DrawStreams;
     ColorLineWidthBitBtn(BitBtn9,MDdef.TigrDef.WaterColor1,MDdef.TigrDef.WaterWidth1);

     CheckBox10.Checked := MDdef.TigrDef.DrawRailroad;
     ColorLineWidthBitBtn(BitBtn10,MDdef.TigrDef.RailroadColor,MDdef.TigrDef.RailroadWidth);

     CheckBox11.Checked := MDdef.TigrDef.DrawPowerLine;
     ColorLineWidthBitBtn(BitBtn11,MDdef.TigrDef.PowerLineColor,MDdef.TigrDef.PowerLineWidth);

     CheckBox12.Checked := MDdef.TigrDef.DrawPipeLine;
     ColorLineWidthBitBtn(BitBtn12,MDdef.TigrDef.PipeLineColor,MDdef.TigrDef.PipeLineWidth);

     CheckBox14.Checked := MDdef.TigrDef.AutoAppear;
     Edit1.Text := IntToStr(MDdef.TigrDef.AppearMajorRoad);
     Edit2.Text := IntToStr(MDdef.TigrDef.AppearRoadCat2);
     Edit3.Text := IntToStr(MDdef.TigrDef.AppearRoadCat3);
     Edit4.Text := IntToStr(MDdef.TigrDef.AppearRoadCat4);
     Edit5.Text := IntToStr(MDdef.TigrDef.AppearRoadCat5);
     Edit6.Text := IntToStr(MDdef.TigrDef.AppearRoadCat6);
     Edit7.Text := IntToStr(MDdef.TigrDef.AppearRoadCat7);
     Edit8.Text := IntToStr(MDdef.TigrDef.AppearStream);
     Edit9.Text := IntToStr(MDdef.TigrDef.AppearCoast);
     Edit11.Text := IntToStr(MDdef.TigrDef.AppearLabels);

     CheckBox15.Checked := MDDef.TigrDef.AutoTigerOnDEMs;
     Edit12.Text := IntToStr(MDdef.TigrDef.MaxAutoTigerCounties);
   {$IfDef RecordTigerOps} WriteLineToDebugFile('TTigerOverlayOptions.SetUpForm out'); {$EndIf}
end;


function SetTigerOptions(inMapForm : tMapForm; PixelSize : float64) : boolean;
var
  TigerOverlayOptions: TTigerOverlayOptions;
begin
   {$IfDef RecordTigerOps} WriteLineToDebugFile('SetTigerOptions in'); {$EndIf}
    TigerOverlayOptions := TTigerOverlayOptions.Create(Application);
    TigerOverlayOptions.MapOwner := inMapForm;
    TigerOverlayOptions.RedrawSpeedButton12.Enabled := InMapForm <> Nil;
    TigerOverlayOptions.SetUpForm;
    if (PixelSize > 0) then begin
        TigerOverlayOptions.Edit10.Text := RealToString(PixelSize,-12,0);
    end
    else begin
       TigerOverlayOptions.Edit10.Visible := false;
       TigerOverlayOptions.Label10.Visible := false;
    end;
    TigerOverlayOptions.Cancel := false;
    TigerOverlayOptions.ShowModal;
    {$IfDef RecordTigerOps} WriteLineToDebugFile('ShowModal done');   {$EndIf}

    Result := not TigerOverlayOptions.Cancel;
    {$IfDef RecordTigerOps} WriteLineToDebugFile('Calling close'); {$EndIf}
    TigerOverlayOptions.Close;
   {$IfDef RecordTigerOps} WriteLineToDebugFile('SetTigerOptions out'); {$EndIf}
end;


procedure TTigerOverlayOptions.BitBtn1Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER major roads',BitBtn1,MajorRoadColor,MajorRoadWidth);
end;

procedure TTigerOverlayOptions.BitBtn2Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER roads 2',BitBtn2,RoadCat2Color,RoadCat2Width);
end;

procedure TTigerOverlayOptions.BitBtn3Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER roads 3',BitBtn3,RoadCat3Color,RoadCat3Width);
end;

procedure TTigerOverlayOptions.BitBtn4Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER roads 4',BitBtn4,RoadCat4Color,RoadCat4Width);
end;


procedure TTigerOverlayOptions.BitBtn5Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER roads 5',BitBtn5,RoadCat5Color,RoadCat5Width);
end;

procedure TTigerOverlayOptions.BitBtn6Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER roads 6',BitBtn6,RoadCat6Color,RoadCat6Width);
end;


procedure TTigerOverlayOptions.BitBtn7Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER roads 7',BitBtn7,RoadCat7Color,RoadCat7Width);
end;

procedure TTigerOverlayOptions.BitBtn8Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER boundaries',BitBtn8,BoundaryColor,BoundaryWidth);
end;


procedure TTigerOverlayOptions.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := true;
end;


procedure TTigerOverlayOptions.CheckCheckBoxes;
begin
   with MDdef.TigrDef do begin
       DrawMajorRoad := CheckBox1.Checked;
       DrawRoadCat2 := CheckBox2.Checked;
       DrawRoadCat3 := CheckBox3.Checked;
       DrawRoadCat4 := CheckBox4.Checked;
       DrawRoadCat5 := CheckBox5.Checked;
       DrawRoadCat6 := CheckBox6.Checked;
       DrawRoadCat7 := CheckBox7.Checked;

       DrawBoundary :=  CheckBox8.Checked;
       DrawStreams := CheckBox9.Checked;
       //DrawCoastline := CheckBox13.Checked;
       DrawRailroad := CheckBox10.Checked;
       DrawPowerLine := CheckBox11.Checked;
       DrawPipeLine  := CheckBox12.Checked;
       AutoAppear := CheckBox14.Checked;
       MDDef.TigrDef.AutoTigerOnDEMs := CheckBox15.Checked;
       DrawLabels := CheckBox16.Checked;
       MDDef.MakeTigerMapGrayscale := CheckBox17.Checked;
       MDDef.SubdueTigerBase := CheckBox18.Checked;
       MDDef.LabelEveryTigerFeature := CheckBox19.Checked;
       MDDef.TigrDef.AutoTigerOnImages := CheckBox20.Checked;

       CheckEditString(Edit1.Text,AppearMajorRoad);
       CheckEditString(Edit2.Text,AppearRoadCat2);
       CheckEditString(Edit3.Text,AppearRoadCat3);
       CheckEditString(Edit4.Text,AppearRoadCat4);
       CheckEditString(Edit5.Text,AppearRoadCat5);
       CheckEditString(Edit6.Text,AppearRoadCat6);
       CheckEditString(Edit7.Text,AppearRoadCat7);
       CheckEditString(Edit8.Text,AppearStream);
       CheckEditString(Edit9.Text,AppearCoast);
       CheckEditString(Edit11.Text,AppearLabels);
       CheckEditString(Edit12.Text,MDdef.TigrDef.MaxAutoTigerCounties);
       WriteTigerDefaults;
   end;
end;


procedure TTigerOverlayOptions.OKBtnClick(Sender: TObject);
begin
   {$IfDef RecordTigerOps} WriteLineToDebugFile('TTigerOverlayOptions.OKBtnClick in'); {$EndIf}
   CheckCheckBoxes;
   Cancel := false;
   {$IfDef RecordTigerOps} WriteLineToDebugFile('TTigerOverlayOptions.OKBtnClick out'); {$EndIf}
end;


procedure TTigerOverlayOptions.BitBtn9Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER water 1',BitBtn9,WaterColor1,WaterWidth1);
end;

procedure TTigerOverlayOptions.BitBtn10Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER railroad',BitBtn10,RailroadColor,RailroadWidth);
end;

procedure TTigerOverlayOptions.BitBtn11Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER power line',BitBtn11,PowerLineColor,PowerLineWidth);
end;

procedure TTigerOverlayOptions.BitBtn12Click(Sender: TObject);
begin
   with MDdef.TigrDef do PickLineSizeAndColor('TIGER pipeline',BitBtn12,PipeLineColor,PipeLineWidth);
end;


procedure TTigerOverlayOptions.Button1Click(Sender: TObject);
begin
   {$IfDef RecordTigerOps} WriteLineToDebugFile('TTigerOverlayOptions.Button1Click in (Restore defaults)'); {$EndIf}
   SetTigerDefaults;
   SetUpForm;
end;


procedure TTigerOverlayOptions.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tigerops.htm');
end;


procedure TTigerOverlayOptions.CancelBtnClick(Sender: TObject);
begin
   Cancel := true;
end;


procedure TTigerOverlayOptions.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
end;


procedure DecMin1(var value : byte);
begin
   if (Value > 1) then dec(Value);
end;

procedure TTigerOverlayOptions.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
   with MDdef.TigrDef do begin
      if (Button = btNext) then begin
         inc(MajorRoadWidth);
         inc(RoadCat2Width);
         inc(RoadCat3Width);
         inc(RoadCat4Width);
         inc(RoadCat5Width);
         inc(RoadCat6Width);
         inc(RoadCat7Width);
      end
      else begin
         DecMin1(MajorRoadWidth);
         DecMin1(RoadCat2Width);
         DecMin1(RoadCat3Width);
         DecMin1(RoadCat4Width);
         DecMin1(RoadCat5Width);
         DecMin1(RoadCat6Width);
         DecMin1(RoadCat7Width);
      end;
   end;
   RoadLineWidths;
end;


initialization
finalization
   {$IfDef RecordTigerOps} WriteLineToDebugFile('RecordTigerOps active in demtigerops'); {$EndIf}
end.
