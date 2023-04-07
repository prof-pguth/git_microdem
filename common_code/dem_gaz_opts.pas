unit dem_gaz_opts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,Petmar_Types;

type
  TGazOptsForm = class(TForm)
    BitBtn8: TBitBtn;
    Label1: TLabel;
    HelpBtn: TBitBtn;
    Features: TBitBtn;
    CheckBox1: TCheckBox;
    OKBtn: TBitBtn;
    BitBtn1: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn8Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FeaturesClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure SetGazOptions;
function GetGazFileName(var fName : PathStr) : boolean;


implementation

{$R *.dfm}

uses
   DEMDefs,PETMAR, PetDBUtils, DEMDataBase,toggle_db_use,DEMDef_routines;


function GetGazFileName(var fName : PathStr) : boolean;
begin
   fName := ExtractFilePath(fName);
   Result := GetFileMultipleMask('gazetteer','Any gazetteer|??_deci.dbf;??.dbf;??_geoname.dbf|USGS gazetter|??_deci.dbf;??_fea*.dbf|NGA gazetter|??.dbf|Canadian geobase|??_geoname.dbf',fName, MDDef.DefaultGazetteerType);
end;


procedure SetGazOptions;
var
   GazOptsForm : TGazOptsForm;
begin
   GazOptsForm := TGazOptsForm.Create(Application);
   with GazOptsForm do begin
      CheckBox1.Checked := MDDef.AvoidTextOverprints;
      Label1.Caption := ExtractFileName(LastGazFile);
      ShowModal;
   end;
end;


procedure TGazOptsForm.FeaturesClick(Sender: TObject);
begin
   PickGazFeatures;
end;

procedure TGazOptsForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TGazOptsForm.BitBtn1Click(Sender: TObject);
var
   GISNum : integer;
begin
   OpenDBForModalEdit(GazOptFName);
exit;
   if OpenNumberedGISDataBase(GISNum,GazOptFName,true) then begin
      GISdb[GISNUM].dbtablef.CheckBox1.Checked := true;
   end;
end;

procedure TGazOptsForm.BitBtn8Click(Sender: TObject);
begin
   if GetGazFileName(LastGazFile) then Label1.Caption := ExtractFileName(LastGazFile)
   else Label1.Caption := ' none set';
end;

procedure TGazOptsForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.AvoidTextOverprints := CheckBox1.Checked;
end;

procedure TGazOptsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\gaz_opts.htm');
end;

procedure TGazOptsForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TGazOptsForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   {$IfDef HideHelpButtons}
   HelpBtn.Visible := false;
   {$EndIf}
end;



end.
