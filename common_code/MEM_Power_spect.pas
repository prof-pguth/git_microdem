unit MEM_Power_spect;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordMemPower}
   //{$Define RecordFullMemPower}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  Petfouri,Petmar_types,DEMDefs;

type
  TMemForm = class(TForm)
    Edit1: TEdit;
    Profiles: TLabel;
    Edit3: TEdit;
    Label2: TLabel;
    OKBtn: TBitBtn;
    BitBtn3: TBitBtn;
    Edit2: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure GetMEMPowerSpectra(var NS_Slope,EW_Slope : float32; WhichDEM : integer; GridLimits: tGridLimits);
procedure GetMemOptions;


implementation

{$R *.dfm}


uses
   Petmar,Petmath,CrossCor,DEMCoord;



procedure GetMemOptions;
var
   MemForm : TMemForm;
begin
   with MDDef.MEMPowerDefaults do begin
      MemForm := TMemForm.Create(Application);
      MemForm.Edit1.Text := IntToStr(NumProfiles);
      MemForm.Edit3.Text := IntToStr(NumPoles);
      MemForm.Edit2.Text := RealToString(FirstFreq,-18,-3);
      MemForm.Edit4.Text := RealToString(LastFreq,-18,-3);
      MemForm.CheckBox1.Checked := LogLogPlot;
      MemForm.ShowModal;
      CheckEditString(MemForm.Edit1.Text,NumProfiles);
      CheckEditString(MemForm.Edit3.Text,NumPoles);
      CheckEditString(MemForm.Edit2.Text,FirstFreq);
      CheckEditString(MemForm.Edit4.Text,LastFreq);
      LogLogPlot := MemForm.CheckBox1.Checked;
      MemForm.Close;
   end;
end;


procedure GetMEMPowerSpectra;
var
   EWFFTGraph,NSFFTGraph : TFFTGraph;
   fNames,Line : tStringList;
   fName : PathStr;
   Prof,x,y : integer;
   z : float32;
begin
   {$IfDef RecordMemPower}
   WriteLineToDebugFile('GetMEMPowerSpectra in Col range: ' + IntToStr(GridLimits.XGridLow) + '/' + IntToStr(GridLimits.XGridHigh) +
       '  Row range: ' + IntToStr(GridLimits.YGridLow) + '/' + IntToStr(GridLimits.YGridHigh));
   {$EndIf}
   NS_Slope := -999;
   EW_Slope := -999;

   with MDDef.MEMPowerDefaults do begin
      Line := TStringList.Create;
      FNames := TStringList.Create;
      with DEMGlb[WhichDEM],MDDef.MEMPowerDefaults do begin
         For Prof := 0 to pred(NumProfiles) do begin
            Line.Clear;
            y := GridLimits.YGridLow + round( Prof * succ(GridLimits.YGridHigh - GridLimits.YGridLow) / pred(NumProfiles));
            {$IfDef RecordFullMemPower} WriteLineToDebugFile('y=' + IntToStr(y)); {$EndIf}
            for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
               if DEMGlb[WhichDEM].GetElevMeters(x,y,z) then Line.Add(RealToString(z,8,2));
            end;
            fName := MDTempDir + 'E-W' + IntToStr(y) + '.txt';
            Line.SaveToFile(fName);
            fNames.Add(fName);
         end;
         PowerSpectrumByMaximumEntropy(MDDef.MEMPowerDefaults,EW_Slope,EWFFTGraph,AreaName + ' E-W',fNames,succ(GridLimits.XGridHigh-GridLimits.XGridLow),AverageXSpace);   //Gallant and others, 1994, Math.Geol.

         FNames.Clear;
         For Prof := 0 to pred(NumProfiles) do begin
            Line.Clear;
            x := GridLimits.XGridLow + round( Prof * succ(GridLimits.XGridHigh - GridLimits.XGridLow) / pred(NumProfiles));
            {$IfDef RecordFullMemPower} WriteLineToDebugFile('x=' + IntToStr(x)); {$EndIf}
            for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
               if DEMGlb[WhichDEM].GetElevMeters(x,y,z) then Line.Add(RealToString(z,8,2));
            end;
            fName := MDTempDir + 'N-S' + IntToStr(x) + '.txt';
            Line.SaveToFile(fName);
            fNames.Add(fName);
         end;
         PowerSpectrumByMaximumEntropy(MDDef.MEMPowerDefaults,NS_Slope,NSFFTGraph,AreaName + ' N-S',fNames,succ(GridLimits.YGridHigh - GridLimits.YGridLow ),AverageYSpace);  //Gallant and others, 1994, Math.Geol.
      end;
      Line.Free;
      FNames.Free;
   end;
   {$IfDef RecordMemPower} WriteLineToDebugFile('  Slopes, E/W: ' + RealToString(EW_Slope,8,2) + '   N/S' + RealToString(NS_Slope,8,2)); {$EndIf}
end;


procedure TMemForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TMemForm.BitBtn3Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\mem_power_opts.htm');
end;


initialization
finalization
{$IfDef RecordMemPower}
   WriteLineToDebugFile('RecordMemPower active in mem_power_spect');
{$EndIf}
{$IfDef RecordFullMemPower}
   WriteLineToDebugFile('RecordFullMemPower active in mem_power_spect');
{$EndIf}
end.
