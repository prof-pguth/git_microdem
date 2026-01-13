unit net_entry;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  NetMainW;

type
  TNetEntryForm = class(TForm)
    Label1: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Edit1: TEdit;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     NetForm : TNetForm;
  end;


procedure MicronetPlot(inNetForm : TNetForm);



implementation

{$R *.dfm}

uses
   Petmar,petmar_types,Petmar_geology,Petmath,DEMDefs;



procedure MicronetPlot(inNetForm : TNetForm);
var
   NetEntryForm : TNetEntryForm;
begin
   NetEntryForm := TNetEntryForm.Create(Application);
   NetEntryForm.NetForm := inNetForm;
   NetEntryForm.ShowModal;
end;

procedure TNetEntryForm.BitBtn1Click(Sender: TObject);
var
   Dip,Strike,DipDirect : float32;
   DipAndStrike : string16;
   xd,yd : integer;
   OK : boolean;
begin
   DipAndStrike := Edit1.Text;
   StripDipAndStrike(DipAndStrike,Dip,Strike,DipDirect,OK);
   if OK then with MDDef.NetDef do  begin
      if (Sender = BitBtn1) or (Sender = BitBtn3) then NetForm.nd.GreatCircleOnNet(Dip,DipDirect,GreatCircleLineWidth,GreatCircleColor);
      if (Sender = BitBtn2) or (Sender = BitBtn3) then NetForm.nd.PlotPointOnNet(PolePlot,Dip,DipDirect,ASymbol(FilledBox,claRed,3),xd,yd);
      NetForm.UpdateDisplay;
   end;
end;



procedure TNetEntryForm.BitBtn2Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TNetEntryForm.BitBtn3Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TNetEntryForm.BitBtn4Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Great circles',BitBtn4,MDDef.NetDef.GreatCircleColor,MDDef.NetDef.GreatCircleLineWidth);
end;


procedure TNetEntryForm.BitBtn5Click(Sender: TObject);
begin
(*
   ReadDefault('Bearing of line',DipDirect);
   ReadDefault('Dip of line',Dip);
   PlotPointOnNet(LinePlot,Dip,DipDirect,ASymbol(FilledBox,clRed,3),xd,yd);
*)
end;


procedure TNetEntryForm.FormCreate(Sender: TObject);
begin
   ColorLineWidthBitBtn(BitBtn4,MDDef.NetDef.GreatCircleColor,MDDef.NetDef.GreatCircleLineWidth);
end;


initialization
finalization
end.

