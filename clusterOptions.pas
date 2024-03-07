unit clusterOptions;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TClusterOptsForm = class(TForm)
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    Edit3: TEdit;
    Label3: TLabel;
    CancelBtn: TBitBtn;
    RadioGroup2: TRadioGroup;
    CheckBox5: TCheckBox;
    CheckBox8: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    CheckBox9: TCheckBox;
    Label6: TLabel;
    Edit4: TEdit;
    Label7: TLabel;
    Edit5: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Aborted : boolean;
  end;


function GetClusterOptions(var Sampling : integer; All : boolean = true) : boolean;


implementation

{$R *.dfm}

uses
   {$IfDef NoClustering}
   {$Else}
      MVClusterClientDataSet,
   {$EndIf}
   DEMDefs,Petmar,Petmar_types;


function GetClusterOptions;
{$IfDef NoClustering}
begin
{$Else}
var
   ClusterOptsForm : TClusterOptsForm;
begin
   ClusterOptsForm := TClusterOptsForm.Create(Application);
   with ClusterOptsForm do begin
      Label4.Caption := 'Max ' + IntToStr(EdburgGeneralFuncsMaxClusters);
      Edit1.Text := IntToStr(MDDef.NumClusters);
      Edit2.Text := IntToStr(MDDef.ClusterIterations);
      Label5.Caption := 'Min sampling=' + IntToStr(Sampling);
      Edit3.Text := IntToStr(Sampling);
      RadioGroup1.ItemIndex := ord(MDDef.ClusterInitialization);
      RadioGroup2.ItemIndex := pred(round(MDDef.ClassDistancePower * 2));

      CheckBox1.Checked := MDDef.ShowClusterScatterPlots;
      CheckBox2.Checked := MDDef.ShowMaskScatterPlots;
      CheckBox3.Checked := MDDef.ShowClusterHistograms;
      CheckBox4.Checked := MDDef.ShowMaskHistograms;

      //CheckBox6.Checked := MDDef.ShowClusterResults;
      //CheckBox7.Checked := MDDef.IncludeClusterStatistics;

      CheckBox5.Checked := MDDef.UnSupSamplesFullImage;
      CheckBox8.Checked := MDDef.UnSupClassFullImage;
      CheckBox9.Checked := MDDef.ClusterSensitivity;
      Edit4.Text := IntToStr(MDDef.ClustSensitiveMin);
      Edit5.Text := IntToStr(MDDef.ClustSensitiveMax);


      CheckBox1.Enabled := true;
      CheckBox2.Enabled := All;
      CheckBox3.Enabled := true;
      CheckBox4.Enabled := All;
      //CheckBox6.Enabled := All;
      //CheckBox7.Enabled := All;

      ShowModal;
      if Aborted then begin
         Result := false;
      end
      else begin
          Result := true;
          CheckEditString(Edit1.Text,MDDef.NumClusters);
          CheckEditString(Edit2.Text,MDDef.ClusterIterations);
          CheckEditString(Edit3.Text,Sampling);
          MDDef.ClusterInitialization := MVClusterClientDataSet.tInitializationOption(RadioGroup1.ItemIndex);
          MDDef.ShowClusterScatterPlots := CheckBox1.Checked;
          MDDef.ShowMaskScatterPlots := CheckBox2.Checked;

          MDDef.ShowClusterHistograms := CheckBox3.Checked;
          MDDef.ShowMaskHistograms := CheckBox4.Checked;
          //MDDef.ShowClusterResults := CheckBox6.Checked;
          //MDDef.IncludeClusterStatistics := CheckBox7.Checked;

          MDDef.UnSupSamplesFullImage := CheckBox5.Checked;
          MDDef.UnSupClassFullImage := CheckBox8.Checked;

          if (MDDef.NumClusters > EdburgGeneralFuncsMaxClusters) then begin
             MDDef.NumClusters := EdburgGeneralFuncsMaxClusters;
             MessageToContinue('Max ' + IntToStr(MDDef.NumClusters) + ' clusters');
          end;
      end;
   end;
   ClusterOptsForm.Free;
{$EndIf}
end;


procedure TClusterOptsForm.CancelBtnClick(Sender: TObject);
begin
   Aborted := true;
   Close;
end;

procedure TClusterOptsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html/cluster_opts.htm');
end;

procedure TClusterOptsForm.OKBtnClick(Sender: TObject);
begin
   Aborted := false;
   Close;
end;


procedure TClusterOptsForm.RadioGroup2Click(Sender: TObject);
begin
   MDDef.ClassDistancePower := 0.5 * succ(RadioGroup2.ItemIndex);
end;

initialization
finalization
end.
