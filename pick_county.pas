unit pick_county;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB,
  Grids, DBGrids, StdCtrls, Buttons, ToolWin,
  ComCtrls,
  Petmar_types,PETMAR, Petmar_db,ExtCtrls;

type
  Tpickcounty = class(TForm)
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ToolBar1: TToolBar;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Table1    : tMyData;
    TigerName : PathStr;
    Picked    : boolean;
  end;


implementation

{$R *.dfm}


uses
   DEMTIGER,PetDBUtils,DEMDefs,Nevadia_Main;


procedure Tpickcounty.OKBtnClick(Sender: TObject);
begin
   TigerName := Table1.GetFieldByNameAsString('FILENAME');
   MDDef.DefaultState := Edit1.Text;
   MDDef.TigrDef.ShowNeighborsOnTIGERCounty := CheckBox1.Checked;
   Picked := true;
   Close;
end;


procedure Tpickcounty.Edit1Change(Sender: TObject);
var
   TStr : shortstring;
begin
   if ptTrim(Edit1.Text) = '' then Table1.ApplyFilter('ON_HAND = ' + QuotedStr('Y'))
   else begin
      TStr := Edit1.Text;
      Table1.ApplyFilter('STATE=' + QuotedStr(TStr) + ' AND ON_HAND = ' + QuotedStr('Y'));  //  + SeriesString);
   end;
end;


procedure Tpickcounty.FormCreate(Sender: TObject);
begin
   Picked := false;
   TigerName := '';
   PlaceFormAtMousePosition(Self);
   Caption := 'Pick county';
   Table1 := tMyData.Create(DEMTiger.TigerIndex);
   Table1.ApplyFilter('ON_HAND = ' + QuotedStr('Y'));  // + SeriesString);
   if (Table1.RecordCount = 0) then begin
      Table1.Destroy;
      IndexTigerFiles;
      Table1 := TMyData.Create(DEMTiger.TigerIndex);
      Table1.ApplyFilter('ON_HAND = ' + QuotedStr('Y'));
      if (Table1.RecordCount = 0) and AnswerIsYes('No TIGER data present; re-index now') then IndexTigerFiles;
   end;
   Table1.AssignEmpSource(DBGrid1.DataSource);
   CheckBox1.Checked := MDDef.TigrDef.ShowNeighborsOnTIGERCounty;
   FormActivate(nil);
   Table1.ApplyFilter('ON_HAND = ' + QuotedStr('Y')); // + SeriesString);
   Edit1.Text := MDDef.DefaultState;
end;



procedure Tpickcounty.DBGrid1DblClick(Sender: TObject);
begin
   OKBtnClick(Sender);
end;


procedure Tpickcounty.BitBtn1Click(Sender: TObject);
begin
   IndexTigerFiles;
end;

procedure Tpickcounty.FormActivate(Sender: TObject);
begin
   OKBtn.Height := 32;
   HelpBtn.Height := 32;
   Edit1.Height := 32;
   BitBtn1.Height := 32;
end;


procedure Tpickcounty.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tiger_county_map.htm');
end;


initialization
finalization
end.
