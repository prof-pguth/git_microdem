unit tiger_address;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordTigerProblems}
{$EndIf}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TTigerAddressForm = class(TForm)
    Panel4: TPanel;
    Label3: TLabel;
    Edit3: TEdit;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    BitBtn3: TBitBtn;
    StreetNoEdit: TEdit;
    Memo1: TMemo;
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     DBonTable : integer;
  end;


procedure StartTigerAddress(DB : integer);


implementation

{$R *.dfm}


uses
   Petmar,Petmar_types, PetImage,
   DEMMapf,DEMDefs,DEMDataBase,
   Nevadia_Main;


procedure StartTigerAddress(DB : integer);
var
  TigerAddressForm : TTigerAddressForm;
begin
   TigerAddressForm := TTigerAddressForm.Create(Application);
   TigerAddressForm.DBonTable := DB;
   TigerAddressForm.Caption := 'Addresses ' + GISdb[DB].dbName;
   wmDEM.FormPlacementInCorner(TigerAddressForm,lpNEMap);
   TigerAddressForm.Show;
end;


procedure TTigerAddressForm.BitBtn3Click(Sender: TObject);
var
   TStr,ZipStr,OldFilter : shortString;
   fraction : float64;
   StreetNum,FromLeft,ToLeft,FromRight,ToRight : integer;
   Onleft,OnRight,FoundMatch : boolean;
   xp,yp : integer;
   Lat,Long : float64;
   FromLeftFName,ToLeftFName,FromRightFName,ToRightFName : ShortString;
begin
    TStr := '';
    if GISdb[DBonTable].ItsTigerShapeFile then begin
       GISdb[DBonTable].dbOpts.MainFilter := 'FULLNAME=' + QuotedStr(Edit3.Text + ' ' + ComboBox1.Text);
    end
    else begin
       if (ComboBox2.Text <> '') then TStr := 'FEDRPR=' + QuotedStr(ComboBox2.Text) + ' AND ';
       if (ComboBox3.Text <> '') then TStr := TStr + 'FEDRPS=' + QuotedStr(ComboBox3.Text) + ' AND ';
       GISdb[DBonTable].dbOpts.MainFilter := TStr + 'FENAME=' + QuotedStr(Edit3.Text) + ' AND FETYPE=' + QuotedStr(ComboBox1.Text);
    end;
    OldFilter := GISdb[DBonTable].MyData.Filter;
    GISdb[DBonTable].AssembleGISFilter;
    {$IfDef RecordTigerProblems} WriteLineToDebugFile('Tdbtablef.BitBtn3Click filter=' + MyData.Filter= + '   filtered record count=' + IntToStr(MyData.RecordCount)); {$EndIf}
    FoundMatch := false;
    if (GISdb[DBonTable].MyData.RecordCount > 0) then begin
       if (StreetNoEdit.Text = '')  then begin
          Memo1.Lines.Add(IntToStr(GISdb[DBonTable].MyData.RecordCount) + ' segments on ' + GISdb[DBonTable].dbOpts.MainFilter);
          GISdb[DBonTable].dbTablef.HighLightRecs(false);
       end
       else begin
          StreetNum := StrToInt(StreetNoEdit.Text);
          while not GISdb[DBonTable].MyData.eof do begin
             if GISdb[DBonTable].ItsTigerShapeFile then begin
                FromLeftFName  := 'LFROMADD';
                ToLeftFName    := 'LTOADD';
                FromRightFName := 'RFROMADD';
                ToRightFName   := 'RTOADD';
             end
             else begin
                FromLeftFName  := 'FRADDL';
                ToLeftFName    := 'TOADDL';
                FromRightFName := 'FRADDR';
                ToRightFName   := 'TOADDR';
             end;
             if (GISdb[DBonTable].MyData.GetFieldByNameAsString(FromLeftFName) <> '') and (GISdb[DBonTable].MyData.GetFieldByNameAsString(ToLeftFName) <> '') then begin
                  FromLeft := GISdb[DBonTable].MyData.GetFieldByNameAsInteger(FromLeftFName);
                  ToLeft := GISdb[DBonTable].MyData.GetFieldByNameAsInteger(ToLeftFName);
                  OnLeft := (StreetNum >= FromLeft) and (StreetNum <= ToLeft);
                  if OnLeft then begin
                     Fraction := (StreetNum - FromLeft) / (ToLeft - FromLeft);
                     ZipStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('ZIPL');
                  end;
             end
             else OnLeft := false;
             if (GISdb[DBonTable].MyData.GetFieldByNameAsString(FromRightFName) <> '') and (GISdb[DBonTable].MyData.GetFieldByNameAsString(ToRightFName) <> '') then begin
                  FromRight := GISdb[DBonTable].MyData.GetFieldByNameAsInteger(FromRightFName);
                  ToRight   := GISdb[DBonTable].MyData.GetFieldByNameAsInteger(ToRightFName);
                  OnRight := (StreetNum >= FromRight) and (StreetNum <= ToRight);
                  if OnRight then begin
                     Fraction := (StreetNum - FromRight) / (ToRight - FromRight);
                     ZipStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('ZIPR');
                  end;
             end
             else OnRight := false;

             if OnLeft or OnRight then begin
                GISdb[DBonTable].aShapeFile.GetLineCoords(GISdb[DBonTable].MyData.RecNo);
                Lat := GISdb[DBonTable].aShapeFile.CurrentLineCoords[0].Lat +
                   Fraction * (GISdb[DBonTable].aShapeFile.CurrentLineCoords[pred(GISdb[DBonTable].aShapeFile.CurrentPolyLineHeader.NumPoints)].Lat - GISdb[DBonTable].aShapeFile.CurrentLineCoords[0].Lat);
                Long := GISdb[DBonTable].aShapeFile.CurrentLineCoords[0].Long +
                   Fraction * (GISdb[DBonTable].aShapeFile.CurrentLineCoords[pred(GISdb[DBonTable].aShapeFile.CurrentPolyLineHeader.NumPoints)].Long - GISdb[DBonTable].aShapeFile.CurrentLineCoords[0].Long);

                GISdb[DBonTable].dbTablef.Highlightrecordonmap1Click(Nil);
                GISdb[DBonTable].TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
                Petmar.ScreenSymbol(GISdb[DBonTable].TheMapOwner.Image1.Canvas,xp,yp,FilledBox,3,ConvertTColorToPlatformColor(clLime));
                if (Memo1.Lines.Count > 1) then Memo1.Lines.Add('');
                Memo1.Lines.Add('Address: ' + StreetNoEdit.Text + ' ' + ComboBox2.Text + ' ' + Edit3.Text + ' ' + ComboBox1.Text + ' ' + ComboBox3.Text + MessLineBreak);
                Memo1.Lines.Add('Zip: ' +  ZipStr);
                Memo1.Lines.Add(LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
                FoundMatch := true;
             end;
             GISdb[DBonTable].MyData.Next;
          end;
          if not FoundMatch then MessageToContinue('No number' + StreetNoEdit.Text + ' on ' + GISdb[DBonTable].dbOpts.MainFilter);
       end;
    end
    else MessageToContinue('Invalid address  (' + StreetNoEdit.Text + ' ' + GISdb[DBonTable].dbOpts.MainFilter +')');
    GISdb[DBonTable].MyData.ApplyFilter(OldFilter);
    GISdb[DBonTable].dbTablef.ShowStatus;
end;


initialization
finalization
   {$IfDef RecordTigerProblems} WriteLineToDebugFile('RecordTigerProblems active in tiger_address'); {$EndIf}
end.


