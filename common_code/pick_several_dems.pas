unit pick_several_dems;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef DEBUG}
   {$Else}
   {$EndIf}
{$EndIf}



interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Petmar;

type
  TtPickGridsForm = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    HelpBitBtn8: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn6: TBitBtn;
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;



procedure PickSeveralExistingDEMs(l1,l2,l3,l4 : String; var DEM1,DEM2,DEM3,DEM4 : integer);


implementation


uses
   DEMCoord,DEM_Manager,DEMDefs,Petmar_types;

{$R *.dfm}

procedure PickSeveralExistingDEMs(l1,l2,l3,l4 : String; var DEM1,DEM2,DEM3,DEM4 : integer);
var
  PickGridsForm : TtPickGridsForm;
  Option : ANSIstring;
  i : integer;
begin
   PickGridsForm := TtPickGridsForm.Create(Application);
   with PickGridsForm do begin
      if l1 = '' then begin
          ComboBox1.Visible := false;
          Label1.Visible := false;
      end
      else begin
          Label1.Caption := l1;
      end;
      if l3 = '' then begin
          ComboBox3.Visible := false;
          Label3.Visible := false;
      end
      else begin
          Label2.Caption := l2;
      end;
      if l2 = '' then begin
          ComboBox2.Visible := false;
          Label2.Visible := false;
      end
      else begin
          Label3.Caption := l3;
      end;
      if l4 = '' then begin
          ComboBox4.Visible := false;
          Label4.Visible := false;
      end
      else begin
          Label4.Caption := l1;
      end;

      for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) and (not DEMGlb[i].HiddenGrid) then begin
            Option := IntToStr(i) + '-' + DEMGlb[i].AreaName;
            if ComboBox1.Visible then ComboBox1.Items.Add(Option);
            if ComboBox2.Visible then ComboBox2.Items.Add(Option);
            if ComboBox3.Visible then ComboBox3.Items.Add(Option);
            if ComboBox4.Visible then ComboBox4.Items.Add(Option);
         end;
      end;
      PickGridsForm.ShowModal;
      Option := ComboBox1.Text;
      if ComboBox1.Visible and (Option <> '') then DEM1 := StrToInt(Petmar_Types.BeforeSpecifiedCharacterANSI(Option,'-')) else DEM1 := 0;
      Option := ComboBox2.Text;
      if ComboBox2.Visible and (Option <> '') then DEM2 := StrToInt(Petmar_Types.BeforeSpecifiedCharacterANSI(Option,'-')) else DEM2 := 0;
      Option := ComboBox3.Text;
      if ComboBox3.Visible and (Option <> '') then DEM3 := StrToInt(Petmar_Types.BeforeSpecifiedCharacterANSI(Option,'-')) else DEM3 := 0;
      Option := ComboBox4.Text;
      if ComboBox4.Visible and (Option <> '') then DEM4 := StrToInt(Petmar_Types.BeforeSpecifiedCharacterANSI(Option,'-')) else DEM4 := 0;
   end;
end;


procedure TtPickGridsForm.BitBtn6Click(Sender: TObject);
begin
  Close;
end;


procedure TtPickGridsForm.BitBtn7Click(Sender: TObject);
begin
   Close;
end;

initialization
finalization
end.
