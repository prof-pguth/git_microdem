unit dem_grid_diffs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TGridDiffForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox3: TCheckBox;
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure SetGridDiffernceProperties;

implementation

{$R *.dfm}

uses
   Petmar,petmar_types,DEMDefs, nevadia_main;


procedure SetGridDiffernceProperties;
var
   GridDiffForm: TGridDiffForm;
begin
   GridDiffForm:= TGridDiffForm.Create(Application);
   GridDiffForm.CheckBox3.Checked := MDDef.ShowScatterplot;
   GridDiffForm.CheckBox2.Checked := MDDef.ShowGridDiffMap;
   GridDiffForm.CheckBox1.Checked := MDDef.ShowGridDiffHistogram;
   PlaceFormInCorner(WMDEM,GridDiffForm,lpCenterMap);
   GridDiffForm.ShowModal;
end;


procedure TGridDiffForm.OKBtnClick(Sender: TObject);
begin
   MDDef.ShowGridDiffMap := CheckBox2.Checked;
   MDDef.ShowGridDiffHistogram := CheckBox1.Checked;
   MDDef.ShowScatterplot := CheckBox3.Checked;
   Close;
end;


procedure TGridDiffForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TGridDiffForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
end;


procedure TGridDiffForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\dem_diff_map.htm');
end;


end.
