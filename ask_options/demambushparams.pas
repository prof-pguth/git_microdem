unit demambushparams;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,  StdCtrls, ExtCtrls, ColorGrd, Dialogs,

   Petmar_types,PETMAR,DEMDefs;


type
  TPickAmbushParams = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Button2: TButton;
    Label5: TLabel;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn4: TBitBtn;
    procedure HelpBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
  private                                                 { Private declarations }
  public
    { Public declarations }
    BaseBitmap : tMyBitmap;
  end;


  function GetAmbushParameters : boolean;



implementation

{$R *.DFM}

uses
   Nevadia_Main,
   DEMCoord,
   DEM_Fan_Algorithm,Demdef_routines,
   PETMath;


function GetAmbushParameters : boolean;
var
   PickAmbushParams : TPickAmbushParams;
begin
   PickAmbushParams := TPickAmbushParams.Create(Application);
   with PickAmbushParams do  begin
      Caption := 'Ambush Fan Parameters';
      Label5.Caption := IntervisiblityAlgorithmName(MDDef.wf);
      ColorBitBtn(BitBtn4,MDDef.FanColor);

      RadioGroup1.ItemIndex := ord(MDdef.AmbushFanShow);
      CheckBox1.Checked := MDDef.AmbushCoverage;
      CheckBox2.Checked := MDDef.DisplayAmbushRoute;

      if ShowModal = mrCancel then Result := false
      else begin
         Result := true;
         MDdef.AmbushFanShow := tFanShow(RadioGroup1.ItemIndex);
         MDDef.AmbushCoverage := CheckBox1.Checked;
         MDDef.DisplayAmbushRoute := CheckBox2.Checked;
      end;
   end;
end;


procedure TPickAmbushParams.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme5lrn.htm');
end;


procedure TPickAmbushParams.Button2Click(Sender: TObject);
begin
   GetFanAlgorithmParameters(MDDef.wf);
   Label5.Caption := IntervisiblityAlgorithmName(MDDef.wf);
end;


procedure TPickAmbushParams.FormCreate(Sender: TObject);
begin
   CheckFormPlacement(Self);
end;


procedure TPickAmbushParams.BitBtn4Click(Sender: TObject);
begin
   QueryColor(BitBtn4,MDDef.FanColor);
end;

initialization
finalization
end.
