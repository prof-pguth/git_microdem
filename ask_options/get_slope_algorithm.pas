unit get_slope_algorithm;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDrawMap}
{$EndIf}

interface

uses
   System.Diagnostics,
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, ExtCtrls, Dialogs,SysUtils,ComCtrls,
   DEMDefs,PETMar,PETMath,
   DEMMapf, DEMDef_routines;

type
  TGetSlopeAlgorithm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Label3: TLabel;
    Button11: TButton;
    Label6: TLabel;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    RadioGroup5: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
      SetupDone : boolean;
      theMethod : tSlopeCurveCompute;
      procedure SetUpForm;
  end;


function Get_Users_Slope_Algorithm(var Method : tSlopeCurveCompute) : boolean;

implementation

{$R *.DFM}

uses
   {$IfDef ExAdvancedGIS}
   {$Else}
      DEMSlpEd,
   {$EndIf}
   DEMCoord,Petmar_types,
   DEMMapDraw,
   Make_Grid,
   Grayscale_shift,
   PETImage, demslopeopts;


function Get_Users_Slope_Algorithm(var Method : tSlopeCurveCompute) : boolean;
var
   GetSlopeAlgorithm : TGetSlopeAlgorithm;
begin
   GetSlopeAlgorithm := TGetSlopeAlgorithm.Create(Application);
   GetSlopeAlgorithm.theMethod := Method;
   GetSlopeAlgorithm.SetUpForm;
   Result := not (GetSlopeAlgorithm.ShowModal = idCancel);
   Method := GetSlopeAlgorithm.theMethod;
   GetSlopeAlgorithm.Free;
end;


procedure TGetSlopeAlgorithm.SetUpForm;
begin
   Label6.Caption := SlopeMethodName(theMethod);
   Button11.Enabled := (MDDef.ProgramOption = ExpertProgram) or MDDef.ShowGeomorphometry;
   RadioGroup2.ItemIndex := pred(theMethod.LSQorder);
   RadioGroup3.ItemIndex := pred(theMethod.WindowRadius);
   RadioGroup5.ItemIndex := pred(theMethod.UsePoints);
   SetUpdone := true;
end;



procedure TGetSlopeAlgorithm.Button11Click(Sender: TObject);
begin
   PickSlopeAspectMethod('',theMethod.AlgorithmName);
   Label6.Caption := SlopeMethodName(theMethod);
   RadioGroup2.Enabled := theMethod.AlgorithmName = smLSQ;
   RadioGroup3.Enabled := theMethod.AlgorithmName = smLSQ;
end;


procedure TGetSlopeAlgorithm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   SetUpDone := false;
   {$IfDef ExAdvancedGIS} BitBtn1.Visible := false; {$Endif}
   {$IfDef HideHelpButtons} HelpBtn.Visible := false; {$EndIf}
end;


procedure TGetSlopeAlgorithm.RadioGroup2Click(Sender: TObject);
begin
    theMethod.LSQorder := succ(RadioGroup2.ItemIndex);
    //if user wants 3rd or 4th order, it has to be 5x5 window
    if (theMethod.LSQorder in [3,4]) and (theMethod.WindowRadius < 2) then RadioGroup3.ItemIndex := 1;
    Label6.Caption := SlopeMethodName(theMethod);
end;

procedure TGetSlopeAlgorithm.RadioGroup3Click(Sender: TObject);
begin
   theMethod.WindowRadius := succ(RadioGroup3.ItemIndex);
   //if user wants the 3x3 window, it has to be a 2d order
   if (theMethod.WindowRadius in [1]) and (theMethod.LSQorder in [3,4]) then RadioGroup2.ItemIndex := 0;
   Label6.Caption := SlopeMethodName(theMethod);
end;

procedure TGetSlopeAlgorithm.RadioGroup5Click(Sender: TObject);
begin
   theMethod.UsePoints := succ(RadioGroup5.ItemIndex);
end;


end.

