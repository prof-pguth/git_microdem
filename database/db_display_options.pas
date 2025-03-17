unit db_display_options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordTOC}
{$EndIf}

interface

uses
  Vcl.Menus, Vcl.StdCtrls, Vcl.Buttons, Vcl.Controls, System.Classes, Vcl.ExtCtrls,
  Windows, Messages, SysUtils, Graphics, Forms, Dialogs,
  DEMMapf,  GIS_scaled_symbols;

type
  Tdb_display_opts = class(TForm)
    BottomPanel: TPanel;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    Panel2: TPanel;
    SpeedButton2: TSpeedButton;
    Panel3: TPanel;
    SpeedButton3: TSpeedButton;
    Panel4: TPanel;
    SpeedButton4: TSpeedButton;
    Panel5: TPanel;
    SpeedButton5: TSpeedButton;
    Panel6: TPanel;
    SpeedButton6: TSpeedButton;
    Panel7: TPanel;
    SpeedButton7: TSpeedButton;
    Panel8: TPanel;
    SpeedButton8: TSpeedButton;
    Panel9: TPanel;
    SpeedButton9: TSpeedButton;
    Panel11: TPanel;
    SpeedButton11: TSpeedButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Panel10: TPanel;
    SpeedButton10: TSpeedButton;
    Panel12: TPanel;
    SpeedButton12: TSpeedButton;
    Panel13: TPanel;
    SpeedButton13: TSpeedButton;
    Panel14: TPanel;
    SpeedButton14: TSpeedButton;
    Panel15: TPanel;
    SpeedButton15: TSpeedButton;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    SpeedButton16: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    PopupMenu1: TPopupMenu;
    Filter1: TMenuItem;
    CodebyDBfield1: TMenuItem;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn21: TBitBtn;
    BitBtn22: TBitBtn;
    BitBtn23: TBitBtn;
    BitBtn24: TBitBtn;
    BitBtn25: TBitBtn;
    BitBtn26: TBitBtn;
    BitBtn27: TBitBtn;
    BitBtn28: TBitBtn;
    BitBtn29: TBitBtn;
    BitBtn30: TBitBtn;
    BitBtn31: TBitBtn;
    BitBtn32: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure CheckBox15Click(Sender: TObject);
    procedure BitBtn1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Filter1Click(Sender: TObject);
    procedure CodebyDBfield1Click(Sender: TObject);
    procedure BitBtn2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BitBtn3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BitBtn4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BitBtn5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure BitBtn29Click(Sender: TObject);
    procedure BitBtn30Click(Sender: TObject);
    procedure BitBtn31Click(Sender: TObject);
    procedure BitBtn32Click(Sender: TObject);
  private
    { Private declarations }
    procedure Reorder(ol : integer);
    procedure ShowMenu(CallingDB : integer);
  public
    { Public declarations }
    theMapOwner : tMapForm;
    CurrentDB : integer;
    DoneSetup : boolean;
    gis_scaled_form : Tgis_scaled_form;
    procedure LabelTheButtons;
  end;

procedure OpenMapTableOfContents(MapOwner : tMapForm; FormStayAtop : boolean);
procedure CloseMapTableOfContents(MapOwner : tMapForm);


const
   MaxTOC = 10;
var
   MapTOC : array[1..MaxTOC] of Tdb_display_opts;


implementation

uses
   Petmar,PETMAR_types,DEMDefs,DEMDataBase;

{$R *.dfm}

procedure CloseMapTableOfContents(MapOwner : tMapForm);
begin
   if (MapOwner.MapTOCIndex <> 0) and (MapOwner <> Nil) then begin
      if (MapTOC[MapOwner.MapTOCIndex] <> Nil) then MapTOC[MapOwner.MapTOCIndex].Close;
      MapOwner.MapTOCIndex := 0;
   end;
end;

procedure OpenMapTableOfContents;
var
   NewTOC,i : integer;
begin
    {$IfDef RecordTOC} WriteLinetoDebugFile('Open TOC, map=' + MapOwner.Caption); {$EndIf}
    NewTOC := 0;
    for i := 1 to MaxTOC do begin
       if MapTOC[i] = Nil then begin
          NewTOC := i;
          break;
       end;
    end;
    if (NewTOC <> 0) then begin
        MapOwner.MapTOCIndex := NewTOC;
        MapTOC[NewTOC] := Tdb_display_opts.Create(Application);
        MapTOC[NewTOC].Caption := 'DB display options, ' + MapOwner.Caption;
        MapTOC[NewTOC].theMapOwner := MapOwner;
        if FormStayAtop then begin
           MapTOC[NewTOC].FormStyle := fsStayOnTop;
           MapTOC[NewTOC].Show;
        end
        else begin
           MapTOC[NewTOC].FormStyle := fsMDIchild;
        end;
    end;
end;


procedure MoveOverlayUp(ol : integer);
var
   i : integer;
   b : byte;
begin
   for i := 2 to 15 do begin
      if (DBPlotOrder[i] = ol) then begin
          b := DBPlotOrder[i];
          DBPlotOrder[i] := DBPlotOrder[pred(i)];
          DBPlotOrder[pred(i)] := b;
          exit;
      end;
   end;
end;


procedure Tdb_display_opts.Reorder(ol: integer);
begin
   MoveOverlayUp(ol);
   BitBtn16Click(Nil);
   LabelTheButtons;
   theMapOwner.DoFastMapRedraw;
end;


procedure Tdb_display_opts.SpeedButton10Click(Sender: TObject);
begin
   Reorder(10);
end;

procedure Tdb_display_opts.SpeedButton11Click(Sender: TObject);
begin
   Reorder(11);
end;

procedure Tdb_display_opts.SpeedButton12Click(Sender: TObject);
begin
   Reorder(12);
end;

procedure Tdb_display_opts.SpeedButton13Click(Sender: TObject);
begin
   Reorder(13);
end;

procedure Tdb_display_opts.SpeedButton14Click(Sender: TObject);
begin
   Reorder(14);
end;

procedure Tdb_display_opts.SpeedButton15Click(Sender: TObject);
begin
   Reorder(15);
end;

procedure Tdb_display_opts.SpeedButton16Click(Sender: TObject);
begin
   TheMapOwner.DataBaseSpeedButton28Click(Sender);
   LabelTheButtons;
end;

procedure Tdb_display_opts.SpeedButton1Click(Sender: TObject);
begin
   Reorder(1);
end;

procedure Tdb_display_opts.SpeedButton2Click(Sender: TObject);
begin
   Reorder(2);
end;

procedure Tdb_display_opts.SpeedButton3Click(Sender: TObject);
begin
   Reorder(3);
end;

procedure Tdb_display_opts.SpeedButton4Click(Sender: TObject);
begin
   Reorder(4);
end;

procedure Tdb_display_opts.SpeedButton5Click(Sender: TObject);
begin
   Reorder(5);
end;

procedure Tdb_display_opts.SpeedButton6Click(Sender: TObject);
begin
   Reorder(6);
end;

procedure Tdb_display_opts.SpeedButton7Click(Sender: TObject);
begin
   Reorder(7);
end;

procedure Tdb_display_opts.SpeedButton8Click(Sender: TObject);
begin
   Reorder(8);
end;

procedure Tdb_display_opts.SpeedButton9Click(Sender: TObject);
begin
   Reorder(9);
end;

procedure Tdb_display_opts.BitBtn10Click(Sender: TObject);
begin
   GISdb[10].GISProportionalSymbols(GISdb[10].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn11Click(Sender: TObject);
begin
   GISdb[11].GISProportionalSymbols(GISdb[11].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn12Click(Sender: TObject);
begin
   GISdb[12].GISProportionalSymbols(GISdb[12].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn13Click(Sender: TObject);
begin
   GISdb[13].GISProportionalSymbols(GISdb[13].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn14Click(Sender: TObject);
begin
   GISdb[14].GISProportionalSymbols(GISdb[14].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn15Click(Sender: TObject);
begin
   GISdb[15].GISProportionalSymbols(GISdb[15].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn16Click(Sender: TObject);
begin
   GISdb[1].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn17Click(Sender: TObject);
begin
   GISdb[2].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn18Click(Sender: TObject);
begin
   GISdb[3].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn19Click(Sender: TObject);
begin
   GISdb[4].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn1Click(Sender: TObject);
begin
   GISdb[1].GISProportionalSymbols(GISdb[1].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      ShowMenu(1);
   end;
end;

procedure Tdb_display_opts.ShowMenu(CallingDB : integer);
begin
   CurrentDB := CallingDB;
   PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Tdb_display_opts.BitBtn20Click(Sender: TObject);
begin
   GISdb[5].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn21Click(Sender: TObject);
begin
   GISdb[6].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn22Click(Sender: TObject);
begin
   GISdb[7].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn23Click(Sender: TObject);
begin
   GISdb[8].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn24Click(Sender: TObject);
begin
   GISdb[9].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn25Click(Sender: TObject);
begin
   GISdb[10].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn26Click(Sender: TObject);
begin
   GISdb[11].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn27Click(Sender: TObject);
begin
   GISdb[12].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn28Click(Sender: TObject);
begin
   GISdb[13].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn29Click(Sender: TObject);
begin
   GISdb[14].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn2Click(Sender: TObject);
begin
   GISdb[2].GISProportionalSymbols(GISdb[2].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      ShowMenu(2);
   end;
end;

procedure Tdb_display_opts.BitBtn30Click(Sender: TObject);
begin
   GISdb[15].dbtablef.BringToFront;
end;

procedure Tdb_display_opts.BitBtn31Click(Sender: TObject);
var
   Allcheck : boolean;
begin
   if (Sender = BitBtn31) then AllCheck := true else AllCheck := false;
   CheckBox1.Checked := AllCheck;
   CheckBox2.Checked := AllCheck;
   CheckBox3.Checked := AllCheck;
   CheckBox4.Checked := AllCheck;
   CheckBox5.Checked := AllCheck;
   CheckBox6.Checked := AllCheck;
   CheckBox7.Checked := AllCheck;
   CheckBox8.Checked := AllCheck;
   CheckBox9.Checked := AllCheck;
   CheckBox10.Checked := AllCheck;
   CheckBox12.Checked := AllCheck;
   CheckBox12.Checked := AllCheck;
   CheckBox13.Checked := AllCheck;
   CheckBox14.Checked := AllCheck;
   CheckBox15.Checked := AllCheck;
end;

procedure Tdb_display_opts.BitBtn32Click(Sender: TObject);
begin
   BitBtn31Click(Sender);
end;

procedure Tdb_display_opts.BitBtn3Click(Sender: TObject);
begin
   GISdb[3].GISProportionalSymbols(GISdb[3].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      ShowMenu(3);
   end;
end;

procedure Tdb_display_opts.BitBtn4Click(Sender: TObject);
begin
   GISdb[4].GISProportionalSymbols(GISdb[4].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn4MouseDown(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      ShowMenu(4);
   end;
end;

procedure Tdb_display_opts.BitBtn5Click(Sender: TObject);
begin
   GISdb[5].GISProportionalSymbols(GISdb[5].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn5MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      ShowMenu(5);
   end;
end;

procedure Tdb_display_opts.BitBtn6Click(Sender: TObject);
begin
   GISdb[6].GISProportionalSymbols(GISdb[6].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn7Click(Sender: TObject);
begin
   GISdb[7].GISProportionalSymbols(GISdb[7].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn8Click(Sender: TObject);
begin
   GISdb[8].GISProportionalSymbols(GISdb[8].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.BitBtn9Click(Sender: TObject);
begin
  GISdb[9].GISProportionalSymbols(GISdb[9].dbOpts.dbAutoShow);
end;

procedure Tdb_display_opts.CheckBox10Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(10) then GISdb[10].ToggleLayer(CheckBox10.Checked);
end;

procedure Tdb_display_opts.CheckBox11Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(11) then GISdb[11].ToggleLayer(CheckBox11.Checked);
end;

procedure Tdb_display_opts.CheckBox12Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(12) then GISdb[12].ToggleLayer(CheckBox12.Checked);
end;

procedure Tdb_display_opts.CheckBox13Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(13) then GISdb[13].ToggleLayer(CheckBox13.Checked);
end;

procedure Tdb_display_opts.CheckBox14Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(14) then GISdb[14].ToggleLayer(CheckBox14.Checked);
end;

procedure Tdb_display_opts.CheckBox15Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(15) then GISdb[15].ToggleLayer(CheckBox15.Checked);
end;

procedure Tdb_display_opts.CheckBox1Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(1) then GISdb[1].ToggleLayer(CheckBox1.Checked);
end;

procedure Tdb_display_opts.CheckBox2Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(2) then GISdb[2].ToggleLayer(CheckBox2.Checked);
end;

procedure Tdb_display_opts.CheckBox3Click(Sender: TObject);
begin
  if DoneSetup and ValidDB(3) then GISdb[3].ToggleLayer(CheckBox3.Checked);
end;

procedure Tdb_display_opts.CheckBox4Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(4) then GISdb[4].ToggleLayer(CheckBox4.Checked);
end;

procedure Tdb_display_opts.CheckBox5Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(5) then GISdb[5].ToggleLayer(CheckBox5.Checked);
end;

procedure Tdb_display_opts.CheckBox6Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(6) then GISdb[6].ToggleLayer(CheckBox6.Checked);
end;

procedure Tdb_display_opts.CheckBox7Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(7) then GISdb[7].ToggleLayer(CheckBox7.Checked);
end;

procedure Tdb_display_opts.CheckBox8Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(8) then GISdb[8].ToggleLayer(CheckBox8.Checked);
end;

procedure Tdb_display_opts.CheckBox9Click(Sender: TObject);
begin
   if DoneSetup and ValidDB(9) then GISdb[9].ToggleLayer(CheckBox9.Checked);
end;

procedure Tdb_display_opts.CodebyDBfield1Click(Sender: TObject);
begin
    GISdb[CurrentDB].GISProportionalSymbols(dbasColorByNumeric);
end;

procedure Tdb_display_opts.Filter1Click(Sender: TObject);
begin
   GISDB[CurrentDB].DisplayTable(GISdb[CurrentDB].MyData.Filter);
end;

procedure Tdb_display_opts.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   TheMapOwner.MapTOCIndex := 0;
   Action := caFree;
end;

procedure Tdb_display_opts.FormCreate(Sender: TObject);
begin
   TheMapOwner := Nil;
   DoneSetup := false;
   LabelTheButtons;
   DoneSetup := true;
end;


procedure Tdb_display_opts.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\multi_db.htm');
end;


Procedure Tdb_display_opts.LabelTheButtons;
var
   BaseTop,i : integer;

   Procedure apanel(ThePanel : TPanel; TheButton : tBitBtn; CheckBox : tCheckBox; i : integer);
   begin
      ThePanel.Visible := (GISdb[i] <> Nil) and GISDB[i].CanPlot;
      if ThePanel.Visible then begin
         GISdb[i].LayerIsOn := DBPlotNow[i];
         ThePanel.Top := BaseTop;
         GISdb[i].ColorButtonForSymbol(TheButton,GISdb[i].dbLegendLabel);
         BaseTop := BaseTop + ThePanel.Height;
         CheckBox.Checked := DBPlotNow[i];
      end;
   end;

begin
   {$IfDef RecordTOC} WriteLinetoDebugFile('Tdb_display_opts.LabelTheButtons in'); {$EndIf}
   BaseTop := 0;
   for I := 1 to 15 do begin
      case DBPlotOrder[i] of
         1 : APanel(Panel1,BitBtn1,CheckBox1,1);
         2 : APanel(Panel2,BitBtn2,CheckBox2,2);
         3 : APanel(Panel3,BitBtn3,CheckBox3,3);
         4 : APanel(Panel4,BitBtn4,CheckBox4,4);
         5 : APanel(Panel5,BitBtn5,CheckBox5,5);
         6 : APanel(Panel6,BitBtn6,CheckBox6,6);
         7 : APanel(Panel7,BitBtn7,CheckBox7,7);
         8 : APanel(Panel8,BitBtn8,CheckBox8,8);
         9 : APanel(Panel9,BitBtn9,CheckBox9,9);
         10 : APanel(Panel10,BitBtn10,CheckBox10,10);
         11 : APanel(Panel11,BitBtn11,CheckBox11,11);
         12 : APanel(Panel12,BitBtn12,CheckBox12,12);
         13 : APanel(Panel13,BitBtn13,CheckBox13,13);
         14 : APanel(Panel14,BitBtn14,CheckBox14,14);
         15 : APanel(Panel15,BitBtn15,CheckBox15,15);
      end;
   end;
   ClientHeight := BaseTop + BottomPanel.Height;
   {$IfDef RecordTOC} WriteLinetoDebugFile('Tdb_display_opts.LabelTheButtons out'); {$EndIf}
end;


procedure Tdb_display_opts.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure InitTOCs;
var
   i : integer;
begin
   for I := 1 to MaxTOC do MapTOC[i] := Nil;
end;


initialization
   InitTOCs;
finalization
   {$IfDef RecordTOC} WriteLineToDebugFile('RecordTOC active in db_display_options'); {$EndIf}
end.
