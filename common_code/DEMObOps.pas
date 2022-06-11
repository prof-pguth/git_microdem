unit Demobops;

no longer used

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,StdCtrls, ExtCtrls, Dialogs,
   DEMMapf;

type
  TObliqueOptions = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    Font: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Label3: TLabel;
    Button1: TButton;
    ComboBox3: TComboBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    procedure FontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     MapForm : tMapForm;
  end;


implementation

{$R *.DFM}

uses
   PETMAR,
   DEMDefs,
   DEMRefOp,

{Main program MDI window for different programs that use this module}
   Nevadia_Main;
{End of the MDI parent declaration}

procedure TObliqueOptions.FontClick(Sender: TObject);
begin
   EditTFont(wmDEM.Font);
end;


procedure TObliqueOptions.FormCreate(Sender: TObject);
begin
   Button1.Enabled := (MDdef.WhichOblique = ReflectanceOblique);
   ComboBox2.Text := ComboBox2.Items[ord(MDdef.WhichOblique)];
   ComboBox3.Text := ComboBox3.Items[MDdef.ObliqueSize];
   CheckBox1.Checked := MDDef.ShowOverlaysOnDrapes;
   CheckBox1.Checked := MDDef.ShowGridOnDrapes;
   MapForm := Nil;
   PlaceFormAtMousePosition(Self);
end;



procedure TObliqueOptions.ComboBox2Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox2.Items.Count) do
      if ComboBox2.Text = ComboBox2.Items[i] then
         MDdef.WhichOblique := tWhichOblique(i);
   Button1.Enabled := MDdef.WhichOblique = ReflectanceOblique;
end;


procedure TObliqueOptions.Button1Click(Sender: TObject);
begin
   ChangeReflectanceOptions(MapForm);
end;


procedure TObliqueOptions.ComboBox1Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox1.Items.Count) do
      if ComboBox1.Text = ComboBox1.Items[i] then exit;
  ComboBox1.Text := 'None';
end;


procedure TObliqueOptions.ComboBox3Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox3.Items.Count) do
      if ComboBox3.Text = ComboBox3.Items[i] then
         MDdef.ObliqueSize := i;
end;


procedure TObliqueOptions.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme9g4z.htm');
end;




procedure TObliqueOptions.OKBtnClick(Sender: TObject);
begin
    MDDef.ShowGridOnDrapes := CheckBox1.Checked;
    MDDef.ShowOverlaysOnDrapes := CheckBox1.Checked;
end;



end.

