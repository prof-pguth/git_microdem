unit dem_grid_diffs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Vcl.ExtCtrls;

type
  TGridDiffForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox3: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox5: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    AutoClose : boolean;
  end;

procedure SetGridDiffernceProperties(DEM1,DEM2 : integer; var GridDefinition : integer;  inAutoClose : boolean = true);

implementation

{$R *.dfm}

uses
   Petmar,petmar_types,DEMDefs,DEMCoord, nevadia_main;


procedure SetGridDiffernceProperties(DEM1,DEM2 : integer; var GridDefinition : integer; inAutoClose : boolean = true);
var
   GridDiffForm: TGridDiffForm;
begin
   GridDiffForm:= TGridDiffForm.Create(Application);
   GridDiffForm.CheckBox3.Checked := MDDef.ShowScatterplot;
   GridDiffForm.CheckBox2.Checked := MDDef.ShowGridDiffMap;
   GridDiffForm.CheckBox1.Checked := MDDef.ShowGridDiffHistogram;
   //GridDiffForm.CheckBox4.Checked := MDDef.HighlightDiffMap;
   GridDiffForm.CheckBox5.Checked := MDDef.AutoMergeStartDEM;
   GridDiffForm.Edit1.Text := RealToString(abs(MDDef.TopCutLevel),-8,-2);
   GridDiffForm.Caption := 'Difference ' + DEMGlb[DEM1].AreaName + ' minus ' + DEMGlb[DEM2].AreaName;
   GridDiffForm.RadioGroup1.Items.Add(DEMGlb[DEM1].AreaName + '  ' + DEMGlb[DEM1].HorizontalDEMSpacing);
   GridDiffForm.RadioGroup1.Items.Add(DEMGlb[DEM2].AreaName + '  ' + DEMGlb[DEM2].HorizontalDEMSpacing);
   GridDiffForm.RadioGroup1.ItemIndex := 0;
   PlaceFormInCorner(WMDEM,GridDiffForm,lpCenterMap);
   GridDiffForm.AutoClose := inAutoClose;
   GridDiffForm.ShowModal;
   if GridDiffForm.RadioGroup1.ItemIndex = 0 then GridDefinition := DEM1 else GridDefinition := DEM2;
end;


procedure TGridDiffForm.OKBtnClick(Sender: TObject);
begin
   MDDef.ShowGridDiffMap := CheckBox2.Checked;
   MDDef.ShowGridDiffHistogram := CheckBox1.Checked;
   MDDef.ShowScatterplot := CheckBox3.Checked;
   MDDef.AutoMergeStartDEM := CheckBox5.Checked;

   CheckEditString(Edit1.Text,MDDef.TopCutLevel);
   MDDef.BottomCutLevel := -MDDef.TopCutLevel;
   if AutoClose then Close;
end;


procedure TGridDiffForm.RadioGroup2Click(Sender: TObject);
begin
   MDDef.HighlightDiffMap := RadioGroup2.ItemIndex;
   //Label1.Enabled := MDDef.HighlightDiffMap;
   //Edit1.Enabled := MDDef.HighlightDiffMap;
end;

procedure TGridDiffForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TGridDiffForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   RadioGroup2.ItemIndex := MDDef.HighlightDiffMap;
end;


procedure TGridDiffForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\dem_diff_map.htm');
end;


end.
