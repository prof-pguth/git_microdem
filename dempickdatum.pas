unit dempickdatum;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 4/3/2016        }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define PickDatumProblems}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  Petmar_types,Petmar,DEMDefs, Buttons;

type
  TPickDatumParams = class(TForm)
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    //Label3: TLabel;
    Label4: TLabel;
    RadioGroup2: TRadioGroup;
    BitBtn3: TBitBtn;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Button2: TButton;
    CheckBox2: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     FormDatumCode : Shortstring;
     PName : tProjectType;
     GeoLatLong{,Mercator}: boolean;
     ProjFName : PathStr;
  end;


implementation

{$R *.DFM}

uses
   BaseMap;


procedure TPickDatumParams.Button1Click(Sender: TObject);
begin
   PickDatum('map',FormDatumCode);
   Button1.Caption := DatumName(FormDatumCode);
end;


procedure TPickDatumParams.Button2Click(Sender: TObject);
begin
   GeoLatLong := true;
   Close;
end;

procedure TPickDatumParams.CheckBox2Click(Sender: TObject);
begin
   MDDef.RememberUTM := CheckBox2.Checked;
end;

procedure TPickDatumParams.Edit1Change(Sender: TObject);
var
   UZ : integer;
begin
   {$IfDef PickDatumProblems} WriteLineToDebugFile('TPickDatumParams change UTM zone to ' + Edit1.Text); {$EndIf}
   CheckEditString(Edit1.Text,UZ);
   if not (UZ in [1..60]) then begin
      Edit1.Text := IntToStr(MDDef.DefaultUTMZone);
      UZ := MDDef.DefaultUTMZone;
   end;
   if MDdef.RememberUTM then MDDef.DefaultUTMZone := UZ;

   Label2.Caption := UTMZoneExtent(Uz);
end;


procedure TPickDatumParams.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   RadioGroup2.ItemIndex := ord(MDDef.DEMZunits);
   ProjFName := '';
   //Mercator := false;
   GeoLatLong := false;
   CheckBox2.Checked := MDDef.RememberUTM;
   BringToFront;
end;


procedure TPickDatumParams.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\spcs.htm');
end;

procedure TPickDatumParams.OKBtnClick(Sender: TObject);
begin
   {$IfDef PickDatumProblems} WriteLineToDebugFile('TPickDatumParams.OKBtnClick UTM zone set to ' + Edit1.Text); {$EndIf}
   Close;
end;

procedure TPickDatumParams.RadioGroup2Click(Sender: TObject);
begin
   MDDef.DEMZunits:= tDEMZunits(RadioGroup2.ItemIndex);
end;

procedure TPickDatumParams.BitBtn3Click(Sender: TObject);
begin
   ProjFName := ProgramRootDir;
   if GetFileFromDirectory('Projection','*.prj',ProjFName) then begin
      Close;
   end
   else ProjFName := '';
end;

initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing dempickdatum in'); {$EndIf}
   {$IfDef PickDatumProblems} WriteLineToDebugFile('PickDatumProblems active in dempickdatum'); {$EndIf}
end.



