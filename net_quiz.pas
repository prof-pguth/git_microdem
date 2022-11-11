unit net_quiz;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordQuiz}
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  NetMainW;

type
  TNetQuizForm = class(TForm)
    Label1: TLabel;
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    BitBtn4: TBitBtn;
    BitBtn2: TBitBtn;
    Memo1: TMemo;
    Label2: TLabel;
    BitBtn3: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
     procedure RandomGreatCircle;
    procedure RestartScoring;
  public
    { Public declarations }
     Correct,False,
     Dip,DipDirect : integer;
     NetForm : TNetForm;
  end;

procedure MicronetQuiz;


implementation

{$R *.dfm}

uses
   Petmar,petmar_types,Petmar_geology,Petmath,DEMDefs,NetOpts,Nevadia_Main;


const
   Tolerance : integer = 6;

procedure MicronetQuiz;
var
   QuizForm : TNetQuizForm;
begin
   StopSplashing;
   MDDef.NetDef.CircleGridIncrement := 10;
   MDDef.NetDef.HemisphereUsed := Lower;
   QuizForm := TNetQuizForm.Create(Application);
   QuizForm.NetForm := TNetForm.Create(Application);
   QuizForm.NetForm.Top := 100;
   QuizForm.NetForm.Left := 100;
   QuizForm.RestartScoring;
   QuizForm.ShowModal;
end;


procedure TNetQuizForm.RandomGreatCircle;
begin
   Dip := 10 * succ(Random(8));
   DipDirect := 10 * Random(36);
   NetForm.nd.NewNet;
   NetForm.nd.GreatCircleOnNet(Dip,DipDirect,MDDef.NetDef.GreatCircleLineWidth,MDDef.NetDef.GreatCircleColor);
   NetForm.UpDateDisplay;
   Edit1.Text := '';
end;


procedure TNetQuizForm.RestartScoring;
var
   TStr : ShortString;
begin
   Correct := 0;
   False := 0;
   Memo1.Lines.Clear;
   if MDDef.NetDef.DrawGridCircles = ngNone then TStr := 'No grid' else TStr := 'Grid shown';
   Memo1.Lines.Add(TStr + ' tolerance=' + IntToStr(Tolerance));
   if (NetForm <> Nil) then RandomGreatCircle;
   NetForm.nd.GreatCircleOnNet(Dip,DipDirect,MDDef.NetDef.GreatCircleLineWidth,MDDef.NetDef.GreatCircleColor);
   NetForm.UpDateDisplay;
   Label2.Caption := '';
end;


procedure TNetQuizForm.BitBtn1Click(Sender: TObject);
var
   GuessDip,GuessStrike,GuessDipDirect : float32;
   StrikeString : ShortString;
   DipAndStrike : string16;
   OK : boolean;

         function Score : shortString;
         var
            n : integer;
         begin
            n := False + Correct;
            Result := RealToString(100 * Correct / n,-12,1) + '%   n=' + IntToStr(n) + '  ';
            Label2.Caption := Result;
         end;

begin
   DipAndStrike := UpperCase(Edit1.Text);
   StripDipAndStrike(DipAndStrike,GuessDip,GuessStrike,GuessDipDirect,OK);
   {$IfDef RecordQuiz}
   WriteLineToDebugFile('Actual:  Dip=' + IntToStr(Dip) + ' dip_direct=' + IntToStr(DipDirect));
   WriteLineToDebugFile('Input:   Dip=' + IntToStr(round(GuessDip)) + ' dip_direct=' + IntToStr(round(GuessDipDirect)));
   WriteLineToDebugFile(DipAndStrike + '   GuessStrike=' + IntToStr(round(GuessStrike)));
   {$EndIf}
   if OK then begin
      if (Round(abs(GuessDip-Dip)) < Tolerance) and (Round(abs(GuessDipDirect-DipDirect)) < Tolerance) then begin
         {$IfDef RecordQuiz} WriteLineToDebugFile('Correct'); {$EndIf}
         inc(Correct);
         Memo1.Lines.Add(Score + 'Right: ' + DipAndStrike + ',  really ' + StrikeString);
      end
      else begin
         {$IfDef RecordQuiz} WriteLineToDebugFile('Error'); {$EndIf}
         inc(false);
         StrikeIntoString(DipDirect,StrikeString);
         StrikeString := StrikeString + ' ' + IntToStr(Dip) + AzimuthToDirection(DipDirect);
         StrikeString := Score + 'Guess ' + DipAndStrike + ',  really ' + StrikeString;
         Memo1.Lines.Add(StrikeString);
         NetForm.nd.GreatCircleOnNet(GuessDip,GuessDipDirect,MDDef.NetDef.GreatCircleLineWidth,claLime);
         NetForm.UpDateDisplay;
         MessageToContinue(StrikeString);
      end;
      RandomGreatCircle;
   end
   else MessageToContinue('Invalid orientation');
end;


procedure TNetQuizForm.BitBtn2Click(Sender: TObject);
begin
   MicronetOptions(NetForm);
   RestartScoring;
end;

procedure TNetQuizForm.BitBtn3Click(Sender: TObject);
begin
   ReadDefault('Tolerance (degrees)',Tolerance);
   RestartScoring;
end;

procedure TNetQuizForm.BitBtn4Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Great circles',BitBtn4,MDDef.NetDef.GreatCircleColor,MDDef.NetDef.GreatCircleLineWidth);
   RestartScoring;
end;


procedure TNetQuizForm.Edit1Change(Sender: TObject);
begin
   BitBtn1.Enabled := Edit1.Text <> '';
end;

procedure TNetQuizForm.FormCreate(Sender: TObject);
begin
   ColorLineWidthBitBtn(BitBtn4,MDDef.NetDef.GreatCircleColor,MDDef.NetDef.GreatCircleLineWidth);
   wmDEM.FormPlacementInCorner(self);
end;

initialization
finalization
   {$IfDef RecordQuiz}WriteLineToDebugFile('RecordQuiz active in net_quiz'); {$EndIf}
end.
