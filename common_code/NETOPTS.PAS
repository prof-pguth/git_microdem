unit Netopts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons, StdCtrls,
   NetMainW, Vcl.ExtCtrls;

type
  TNetOptFm = class(TForm)
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    CheckBox1: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    RadioGroup5: TRadioGroup;
    RadioGroup6: TRadioGroup;
    BitBtn1: TBitBtn;
    CancelBtn: TBitBtn;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    procedure Update;
    { Private declarations }
  public
    { Public declarations }
     NetForm : TNetForm;
     UpdateNow : boolean;
  end;

procedure MicronetOptions(inNetForm : TNetForm);



implementation

{$R *.DFM}

uses
   DEMDefs,PETMAR;


procedure MicronetOptions(inNetForm : TNetForm);
var
   NetOptFm : TNetOptFm;
   OldOptions : tMicronetDefaults;
begin
   NetOptFm := tNetOptFm.Create(Application);
   with MDDef.NetDef,NetOptFm do begin
       OldOptions := MDDef.NetDef;
       UpdateNow := false;
       NetForm := inNetForm;
       CheckBox1.Checked := FormLegend;
       CheckBox2.Checked := NorthTick;
       CheckBox3.Checked := CenterTick;
       RadioGroup1.ItemIndex := 2;
       ColorLineWidthBitBtn(BitBtn1,MDDef.NetDef.NetLineColor,MDDef.NetDef.NetLineWidth);

       if (NetScreenMult > 6) then NetScreenMult := 6;
       if (NetScreenMult < 1) then NetScreenMult := 1;

       RadioGroup2.ItemIndex := ord(DrawGridCircles);
       RadioGroup3.ItemIndex := ord(NetContourColors);
       RadioGroup4.ItemIndex := pred(NetScreenMult);
       RadioGroup5.ItemIndex := ord(HemisphereUsed);
       RadioGroup6.ItemIndex := ord(NetUsed);

       case MDDef.NetDef.CircleGridIncrement of
          30 : RadioGroup1.ItemIndex := 0;
          15 : RadioGroup1.ItemIndex := 1;
          10 : RadioGroup1.ItemIndex := 2;
           5 : RadioGroup1.ItemIndex := 3;
       end;
   end;
   NetOptFm.UpdateNow := true;
   if NetOptFm.ShowModal = idCancel then begin
       MDDef.NetDef := OldOptions;
   end;
   NetOptFm.Free;
end;


procedure TNetOptFm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\micr1d6b.htm');
end;

procedure TNetOptFm.Update;
begin
   if UpdateNow then begin
      MDDef.NetDef.DrawGridCircles := tNetGrid(RadioGroup2.ItemIndex);
      MDDef.NetDef.NetScreenMult := succ(RadioGroup4.ItemIndex);
      MDDef.NetDef.NetUsed := NetType(RadioGroup6.ItemIndex);
      MDDef.NetDef.HemisphereUsed := tHemisphere(RadioGroup5.ItemIndex);

      case RadioGroup1.ItemIndex of
          0 : MDDef.NetDef.CircleGridIncrement := 30;
          1 : MDDef.NetDef.CircleGridIncrement := 15;
          2 : MDDef.NetDef.CircleGridIncrement := 10;
          3 : MDDef.NetDef.CircleGridIncrement := 5;
       end;
      MDDef.NetDef.NetContourColors :=  tNetContourColors(RadioGroup3.ItemIndex);
      MDDef.NetDef.FormLegend := CheckBox1.Checked;
      MDDef.NetDef.NorthTick := CheckBox2.Checked;
      MDDef.NetDef.CenterTick := CheckBox3.Checked;
   end;
   if (NetForm <> Nil) then NetForm.nd.NewNet;
end;

procedure TNetOptFm.RadioGroup1Click(Sender: TObject);
begin
   Update;
end;

procedure TNetOptFm.RadioGroup2Click(Sender: TObject);
begin
   Update;
end;

procedure TNetOptFm.RadioGroup4Click(Sender: TObject);
begin
   Update;
end;

procedure TNetOptFm.RadioGroup5Click(Sender: TObject);
begin
   Update;
end;

procedure TNetOptFm.RadioGroup6Click(Sender: TObject);
begin
   Update;
end;

procedure TNetOptFm.BitBtn1Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Great circles',BitBtn1,MDDef.NetDef.NetLineColor,MDDef.NetDef.NetLineWidth);
end;

procedure TNetOptFm.CheckBox2Click(Sender: TObject);
begin
   Update;
end;

procedure TNetOptFm.CheckBox3Click(Sender: TObject);
begin
   Update;
end;

procedure TNetOptFm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
end;


end.
