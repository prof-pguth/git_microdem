unit get_plss;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //these are normally only defined for debugging specific problems
  //{$Define RecordPLSSProblems}
  //{$Define DefineOriginalPosition}    //only for very specific debugging
{$EndIf}

interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations

  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs,
  System.UITypes,Buttons, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  PETMAR, Petmar_types,DEMMapf;

type
  Tgetplssf = class(TForm)
    GroupBox1: TGroupBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    ComboBox1: TComboBox;
    Edit3: TEdit;
    RadioGroup4: TRadioGroup;
    RadioGroup5: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit5: TEdit;
    BitBtn2: TBitBtn;
    CheckBox7: TCheckBox;
    BitBtn1: TBitBtn;
    CheckBox3: TCheckBox;
    BitBtn16: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn14: TBitBtn;
    Label3: TLabel;
    Label5: TLabel;
    BitBtn3: TBitBtn;
    CheckBox4: TCheckBox;
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
  private
    { Private declarations }
     procedure UpdatePositionString;
  public
    { Public declarations }
    Lat,Long : float64;
    ReloadPLSS,AutoUpdate : boolean;
    MapOwner : tMapForm;
  end;


function GetPLSSLocation(var PLSSString : ShortString; var Lat,Long : float64; MapOwner : tMapForm) : boolean;


implementation

{$R *.dfm}


uses
   DEM_PLSS,PETMath,PetImage,DEMDefs,DEMDatabase,
   PetDBUtils,DEMCoord,BaseMap,Nevadia_Main;


const
   TownShipNumbers : array[0..35] of integer = (6,7,18,19,30,31,
                                                5,8,17,20,29,32,
                                                4,9,16,21,28,33,
                                                3,10,15,22,27,34,
                                                2,11,14,23,26,35,
                                                1,12,13,24,25,36);


function GetPLSSLocation(var PLSSString : ShortString; var Lat,Long : float64; MapOwner : tMapForm)  : boolean;
var
   getplssf : Tgetplssf;
   i : integer;
begin
   {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('GetPLSSLocation in'); {$EndIf}
   if PLSSOpen or TryToOpenPLSS(MapOwner) then begin
        getplssf := Tgetplssf.Create(Application);
        getplssf.MapOwner := MapOwner;

        {$IfDef DefineOriginalPosition}
           getplssf.RadioGroup1.ItemIndex := 1;
           getplssf.RadioGroup2.ItemIndex := 2;
           getplssf.RadioGroup3.ItemIndex := 18;
           getplssf.Edit1.Text := '15';
           getplssf.Edit2.Text := '10';
        {$EndIf}

        getplssf.ShowModal;
        PLSSString := getplssf.Edit3.Text;
        Lat := GetPLSSf.Lat;
        Long := GetPLSSf.Long;
        Getplssf.Free;
        Result := (Lat > -900) and (Long > -900);

        for I := 1 to MaxPLSS do begin
           if (PLSS[i] <> Nil) then begin
              GISdb[PLSS[i].TownshipDB].ClearGISFilter;
              GISdb[PLSS[i].SectionDB].ClearGISFilter;
              if (PLSS[i].QuarterDB <> 0) then GISdb[PLSS[i].QuarterDB].ClearGISFilter;
           end;
        end;
  end
  else begin
      {$IfDef RecordPLSSProblems} WriteLineToDebugFile('PLSS undefined'); {$EndIf}
      MessageToContinue('PLSS undefined');
      Result := false;
   end;
   {$IfDef RecordPLSSProblems} WriteLineToDebugFile('GetPLSSLocation out'); {$EndIf}
end;



procedure Tgetplssf.UpdatePositionString;


         procedure FillInEdit3(UseQuarterQuarters,UseQuarters : boolean);
         var
            s1,s2,s3,s4,s5,s6,s7 : shortstring;
         begin
            {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('Start fill in edit3'); {$EndIf}
            s6 := RadioGroup5.Items[RadioGroup5.ItemIndex];
            s7 := RadioGroup4.Items[RadioGroup4.ItemIndex];
            Edit3.Text := '';
            Edit5.Text := '';
            if RadioGroup1.Enabled and (RadioGroup1.ItemIndex <> 2) and UseQuarterQuarters then begin
               s1 := RadioGroup1.Items[RadioGroup1.ItemIndex] + '¼';
            end
            else s1 := '';

            if (RadioGroup2.ItemIndex <> 2) and UseQuarters then begin
               s2 := RadioGroup2.Items[RadioGroup2.ItemIndex] + '¼';
            end
            else s2 := '';

            if (RadioGroup3.ItemIndex <> -1) then begin
               s3 := ' Sect ' + IntToStr(TownShipNumbers[RadioGroup3.ItemIndex]);
            end
            else s3 := '';

            if CheckBox1.Checked then s4 := '½' else s4 := '';
            if CheckBox2.Checked then s5 := '½'
            else if CheckBox3.Checked then s5 := '-3/4'
            else s5 := '';

            case MDDef.PLSSdef.PLSSFormat of
               plssTRS : Edit3.Text := 'T' + Edit1.Text +  s4 + s7 + '  R' + Edit2.Text +  s5 + s6 + ' ' + s3 + ' ' + s1 + s2 +'  ' + ComboBox1.Text;
               plssSTR : Edit3.Text :=  s1 + s2 + s3 + ' T' + Edit1.Text + s4 + s7 + '  R' + Edit2.Text +  s5 + s6 + ' ' + ComboBox1.Text;
            end;

            {$IfDef RecordPLSSProblems}  WriteLinetoDebugFile('Edit3=' + Edit3.Text); {$EndIf}
         end;

            procedure ProcessPoint(Lat,Long : float64);
            begin
               Edit5.Text := LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod);
               {$IfDef RecordPLSSProblems}  WriteLinetoDebugFile('Process Point=' + Edit5.Text); {$EndIf}
               ShowHourglassCursor;
            end;


           procedure FindCenter(i : integer; var Lat,Long : float64);
           var
              Min,Max,aMinVal,aMaxVal : float64;
              Saved : boolean;
           begin
               {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('Seeking Center'); {$EndIf}

               if (GISdb[PLSS[i].QuarterDB].MyData.RecordCount = 1) then begin
                   {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('One rec');  {$EndIf}
                   Lat := 0.5 * (GISdb[PLSS[i].QuarterDB].MyData.GetFieldByNameAsFloat('LAT_HI') +  GISdb[PLSS[i].QuarterDB].MyData.GetFieldByNameAsFloat('LAT_LOW'));
                   Long := 0.5 * (GISdb[PLSS[i].QuarterDB].MyData.GetFieldByNameAsFloat('LONG_HI') + GISdb[PLSS[i].QuarterDB].MyData.GetFieldByNameAsFloat('LONG_LOW'));
               end
               else begin
                  Saved := ShowSatProgress;
                  ShowSatProgress := false;
                  GISdb[PLSS[i].QuarterDB].MyData.FindFieldRange('LAT_HI',aMinVal,Max);
                  GISdb[PLSS[i].QuarterDB].MyData.FindFieldRange('LAT_LOW',Min,aMaxVal);
                  Lat := 0.5 * (Min + Max);
                  GISdb[PLSS[i].QuarterDB].MyData.FindFieldRange('LONG_HI',aMinVal,Max);
                  GISdb[PLSS[i].QuarterDB].MyData.FindFieldRange('LONG_LOW',Min,aMaxVal);
                  Long := 0.5 * (Min + Max);
                  ShowSatProgress := saved;
                  ShowHourglassCursor;
               end;
               {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('Center found'); {$EndIf}
           end;

var
   s1,s2,s3,s4,s5,s6 : shortstring;
   i  : integer;
   Found : boolean;
begin
   {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('Tgetplssf.UpdatePositionString in');{$EndIf}
   ShowHourglassCursor;
   Edit3.Text := '';
   Edit5.Text := '';
   Label3.Caption := '';
   Label5.Caption := '';
   RadioGroup1.Enabled := RadioGroup2.ItemIndex <> 2;

   if (Edit1.Text <> '') and (Edit2.Text <> '') and (RadioGroup3.ItemIndex <> -1) then begin
      s2 := Edit1.Text;
      while(Length(s2) < 3) do s2 := '0' + s2;
      s3 := Edit2.Text;
      while(Length(s3) < 3) do s3 := '0' + s3;
      if CheckBox1.Checked then s4 := '2' else s4 := '0';
      if CheckBox2.Checked then s5 := '2' else s5 := '0';
      if CheckBox3.Checked then s5 := '3';

      s1 := Copy(ComboBox1.Text,1,4) + 'T' + s2 + s4 + RadioGroup4.Items[RadioGroup4.ItemIndex] + s3 + s5 + RadioGroup5.Items[RadioGroup5.ItemIndex];
      s6 := IntToStr(TownShipNumbers[RadioGroup3.ItemIndex]);
      //while(Length(s6) < 3) do s6 := '0' + s6;

      Edit5.Text := 'not in data base';
      Found := false;
      for i := 1 to MaxPLSS do begin
          if (PLSS[i] <> Nil) and (not Found) then begin
             Lat := -999;
             Long := -999;

             GISdb[PLSS[i].SectionDB].MyData.ApplyFilter('LNDKEY = ' + QuotedStr(s1) + ' and SECTN = ' + QuotedStr(s6));
             {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('Recs=' + IntToStr(GISdb[PLSS[i].SectionDB].MyData.RecordCount) + '  filter= ' + GISdb[PLSS[i].SectionDB].MyData.Filter); {$EndIf}
             if (GISdb[PLSS[i].SectionDB].MyData.RecordCount >= 1) then begin
                Lat := 0.5 * (GISdb[PLSS[i].SectionDB].MyData.GetFieldByNameAsFloat('LAT_HI') + GISdb[PLSS[i].SectionDB].MyData.GetFieldByNameAsFloat('LAT_LOW'));
                Long := 0.5 * (GISdb[PLSS[i].SectionDB].MyData.GetFieldByNameAsFloat('LONG_HI') + GISdb[PLSS[i].SectionDB].MyData.GetFieldByNameAsFloat('LONG_LOW'));
                FillInEdit3(false,false);
                ProcessPoint(Lat,Long);
                Found := true;
             end;
             GISdb[PLSS[i].SectionDB].MyData.ApplyFilter('');

             if (MDDef.PLSSDef.PLSSQuartersInLabels) and (RadioGroup2.ItemIndex <> 2) then begin
                if (RadioGroup1.ItemIndex <> 2) then begin  //doing 1/4  1/4
                   GISdb[PLSS[i].QuarterDB].MyData.ApplyFilter('LNDKEY = ' + QuotedStr(s1) + ' and SECTN = ' + QuotedStr(s6) +
                                 ' and ' + PLSS[i].QSectKey + ' = ' + QuotedStr(RadioGroup1.Items[RadioGroup1.ItemIndex] + RadioGroup2.Items[RadioGroup2.ItemIndex]));
                   {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('Quarter Quarter Recs=' + IntToStr(GISdb[PLSS[i].QuarterDB].MyData.RecordCount) + '  filter=' + GISdb[PLSS[i].QuarterDB].MyData.Filter); {$EndIf}

                   if (GISdb[PLSS[i].QuarterDB].MyData.RecordCount > 0) then begin
                      FindCenter(i,Lat,Long);
                      FillInEdit3(true,true);
                      ProcessPoint(Lat,Long);
                   end;
                end
                else begin
                   {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('Apply  quarter filter'); {$EndIf}
                   GISdb[PLSS[i].QuarterDB].MyData.ApplyFilter('LNDKEY = ' + QuotedStr(s1) + ' and SECTN = ' + QuotedStr(s6) + ' and ' + 'QSECTION = ' + QuotedStr(RadioGroup2.Items[RadioGroup2.ItemIndex]));
                   ApplicationProcessMessages;
                   {$IfDef RecordPLSSProblems} WriteLinetoDebugFile('Quarter Recs=' + IntToStr(GISdb[PLSS[i].QuarterDB].MyData.RecordCount) + '  filter=' + GISdb[PLSS[i].QuarterDB].MyData.Filter); {$EndIf}
                   if (GISdb[PLSS[i].QuarterDB].MyData.RecordCount > 0) then begin
                      FindCenter(i,Lat,Long);
                      FillInEdit3(false,true);
                      ProcessPoint(Lat,Long);
                   end
                   else Label3.Caption := 'No ¼ section';
                end;
             end;
          end;
      end;
   end;
   if Found and (MapOwner <> Nil) then begin
      MapOwner.DoFastMapRedraw;
      MapOwner.MapDraw.MapSymbolAtLatLongDegree(MapOwner.Image1.Canvas,Lat,Long,Box,3,claRed)
   end;

   ShowDefaultCursor;
  {$IfDef RecordPLSSProblems} WriteLineToDebugFile('Tgetplssf.UpdatePositionString out ' + Edit3.Text + '   ' + Edit5.Text); {$EndIf}
end;


procedure Tgetplssf.RadioGroup1Click(Sender: TObject);
begin
   UpdatePositionString;
end;

procedure Tgetplssf.RadioGroup2Click(Sender: TObject);
begin
   RadioGroup1.Enabled := true;
   if AutoUpdate then UpdatePositionString;
end;

procedure Tgetplssf.RadioGroup3Click(Sender: TObject);
begin
   if AutoUpdate then UpdatePositionString;
end;

procedure Tgetplssf.Edit1Change(Sender: TObject);
begin
   if AutoUpdate then UpdatePositionString;
end;

procedure Tgetplssf.Edit2Change(Sender: TObject);
begin
   if AutoUpdate then UpdatePositionString;
end;

procedure Tgetplssf.ComboBox1Change(Sender: TObject);
begin
   DefaultPLSSBaseline := ComboBox1.Text;
   if AutoUpdate then UpdatePositionString;
end;

procedure Tgetplssf.CheckBox1Click(Sender: TObject);
begin
   if AutoUpdate then UpdatePositionString;
end;


procedure Tgetplssf.CheckBox2Click(Sender: TObject);
begin
   if CheckBox2.Checked  then CheckBox3.Checked := false;
   if AutoUpdate then UpdatePositionString;
end;


procedure Tgetplssf.RadioGroup4Click(Sender: TObject);
begin
   MDDef.PLSSTownshipDef := RadioGroup4.ItemIndex;
   if AutoUpdate then UpdatePositionString;
end;

procedure Tgetplssf.RadioGroup5Click(Sender: TObject);
begin
   MDDef.PLSSRangeDef := RadioGroup5.ItemIndex;
   if AutoUpdate then UpdatePositionString;
end;


procedure Tgetplssf.FormCreate(Sender: TObject);
var
   Table : tMyData;
begin
   ComboBox1.Text := DefaultPLSSBaseline;
   Lat := -999;
   Long := -999;
   CheckBox7.Checked := MDDef.PLSSDef.PLSSFormat = plssTRS;
   Left := Screen.Width - 5 - Self.Width;
   Top := 30;
   RadioGroup4.ItemIndex := MDDef.PLSSTownshipDef;
   RadioGroup5.ItemIndex := MDDef.PLSSRangeDef;

   if FileExists(PLSSMerfName) then begin
     Table := tMyData.Create(PLSSMerfName);
     Table.ApplyFilter('USE=' + QuotedStr('Y'));
     while not Table.EOF  do begin
        ComboBox1.Items.Add(Table.GetFieldByNameAsString('MERIDIAN'));
        Table.Next;
     end;
     Table.Destroy;
   end;
   Autoupdate := false;
   wmDEM.FormPlacementInCorner(self);
end;


procedure Tgetplssf.BitBtn14Click(Sender: TObject);
begin
   if (MapOwner <> Nil) and (Lat > -990) and (Long > -990) then MapOwner.CenterMapOnLatLong(Lat,Long,hzZoomOut);
   UpdatePositionString;
end;

procedure Tgetplssf.BitBtn15Click(Sender: TObject);
begin
   if (MapOwner <> Nil)  and (Lat > -990) and (Long > -990) then MapOwner.CenterMapOnLatLong(Lat,Long,hzZoomIn);
   UpdatePositionString;
end;


procedure Tgetplssf.BitBtn16Click(Sender: TObject);
begin
   if (MapOwner <> Nil)  and (Lat > -990) and (Long > -990) then MapOwner.CenterMapOnLatLong(Lat,Long,hzNoZoom);
   UpdatePositionString;
end;


procedure Tgetplssf.BitBtn1Click(Sender: TObject);
begin
   RadioGroup1.ItemIndex := 2;
   RadioGroup2.ItemIndex := 2;
   RadioGroup3.ItemIndex := -1;
   Edit1.Text := '';
   Edit2.Text := '';
   CheckBox1.Checked := false;
   CheckBox2.Checked := false;
   CheckBox3.Checked := false;
end;


procedure Tgetplssf.BitBtn2Click(Sender: TObject);
begin
   Close;
end;


procedure Tgetplssf.BitBtn3Click(Sender: TObject);
begin
   UpdatePositionString;
end;

procedure Tgetplssf.CheckBox7Click(Sender: TObject);
begin
   if CheckBox7.Checked then MDDef.PLSSdef.PLSSFormat := plssTRS
   else MDDef.PLSSdef.PLSSFormat := plssSTR;
end;


procedure Tgetplssf.CheckBox3Click(Sender: TObject);
begin
   if CheckBox3.Checked then CheckBox2.Checked := false;
   UpdatePositionString;
end;


procedure Tgetplssf.CheckBox4Click(Sender: TObject);
begin
   Autoupdate := CheckBox4.Checked;
end;

initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing get_plss in'); {$EndIf}
   {$IfDef RecordPLSSProblems} WriteLineToDebugFile('RecordPLSSProblems in get_plss'); {$EndIf}
   {$IfDef DefineOriginalPosition} WriteLineToDebugFile('DefineOriginalPosition in get_plss'); {$EndIf}
end.



