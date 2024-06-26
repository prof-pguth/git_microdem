unit Getlatln;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordGetLatLong}
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons, StdCtrls,ExtCtrls, ComCtrls,SysUtils,
   TabNotBk,
   PETMAR,Petmar_types, BaseMap,DEMDefs;

type
  TGetLatLongDlg = class(TForm)
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    TabbedNotebook1: TTabbedNotebook;
    TabControl1: TTabControl;
    Label2: TLabel;
    Minutes: TLabel;
    Seconds: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Edit2: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    CheckBox1: TCheckBox;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit1: TEdit;
    Edit5: TEdit;
    RadioGroup3: TRadioGroup;
    Edit9: TEdit;
    Edit10: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    RadioGroup4: TRadioGroup;
    CheckBox4: TCheckBox;
    BitBtn1: TBitBtn;
    procedure HelpBtnClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
      { Private declarations }

  public
    { Public declarations }
     procedure ChangeInputMethod;
     procedure ReadLatLongFromBoxes(var Lat,Long : float64; var OK : boolean);
     procedure WriteLatLongToBoxes(Lat,Long : float64);
  end;


procedure GetLatLong(DatumConstants : tMapProjection; Prompt : ShortString; var Lat,Long : float64);
procedure GetLatLongDefault(DatumConstants : tMapProjection; Prompt : ShortString; var Lat,Long : float64);

procedure GetLatLongInRadiansNoDatum(Prompt : ShortString; var Lat,Long : float64);
procedure GetLatLongDefaultNoDatum(Prompt : ShortString; var Lat,Long : float64);
procedure GetLatLongNoDatum(Prompt : ShortString; var Lat,Long : float64);


implementation

{$R *.DFM}

uses
   PetMath,
   DEMMapf;


procedure tGetLatLongDlg.WriteLatLongToBoxes(Lat,Long : float64);
var
   DegString,MinString,SecString : string10;
begin
    if abs(Lat) > 90 then Lat := 45;
    ConvertToDegMinSecString(Lat,MDDef.OutPutLatLongMethod,DegString,MinString,SecString);
    Edit2.Text := ptTrim(DegString);
    Edit3.Text := ptTrim(MinString);
    Edit4.Text := ptTrim(SecString);
    if Lat >= 0 then RadioGroup1.ItemIndex := 0 else RadioGroup1.ItemIndex := 1;
    if abs(Long) > 180 then Long := -100;
    ConvertToDegMinSecString(Long,MDDef.OutPutLatLongMethod,DegString,MinString,SecString);
    Edit6.Text := ptTrim(DegString);
    Edit7.Text := MinString;
    Edit8.Text := SecString;
    if Long <= 0 then RadioGroup2.ItemIndex := 0 else RadioGroup2.ItemIndex := 1;
end;

procedure tGetLatLongDlg.ReadLatLongFromBoxes(var Lat,Long : float64; var OK : boolean);
var
   Deg,Min,Sec : float64;
   Error : integer;
begin
    MDdef.UseLongsto360 := CheckBox1.Checked;
    if Edit2.Text = '' then Edit2.Text := '0';
    if Edit3.Text = '' then Edit3.Text := '0';
    if Edit4.Text = '' then Edit4.Text := '0';
    if Edit6.Text = '' then Edit6.Text := '0';
    if Edit7.Text = '' then Edit7.Text := '0';
    if Edit8.Text = '' then Edit8.Text := '0';
    Val(ptTrim(Edit2.Text),Deg,Error);
    if (Error <> 0) or (Deg > 90) then OK := false;
    Val(ptTrim(Edit3.Text),Min,Error);
    if (Error <> 0) or (Min > 60) then OK := false;
    Val(ptTrim(Edit4.Text),Sec,Error);
    if (Error <> 0)  or (Sec > 60)then OK := false;
    Lat := Deg + Min / 60 + Sec / 3600;
    if RadioGroup1.ItemIndex = 1 then Lat := -Lat;

    Val(ptTrim(Edit6.Text),Deg,Error);
    if (Error <> 0) or ((abs(Deg) > 180) and (not MDdef.UseLongsto360)) or
       ((abs(Deg) > 360) and (MDdef.UseLongsto360)) then OK := false;

    Val(ptTrim(Edit7.Text),Min,Error);
    if (Error <> 0) or (Min > 60) then OK := false;
    Val(ptTrim(Edit8.Text),Sec,Error);
    if (Error <> 0)  or (Sec > 60)then OK := false;
    Long := Deg + Min / 60 + Sec / 3600;
    if RadioGroup2.ItemIndex = 0 then Long := -Long;
end;


procedure LatLongInBox(Prompt : ShortString; DatumConstants : tMapProjection;  var Lat,Long : float64; Default,CoordOption : boolean);
var
   XUTM,YUTM : float64;
   SquareID : shortstring;
   OK          : Boolean;
   GetLatLongDlg: TGetLatLongDlg;
   MenuStr : ShortString;
begin
    GetLatLongDlg := TGetLatLongDlg.Create(Application);
    with GetLatLongDlg do
    begin
       CheckBox4.Checked := MDDef.SaveDefaultHemis;
       if not (MDDef.OutPutLatLongMethod in [DecDegrees,DecMinutes,DecSeconds]) then MDDef.OutPutLatLongMethod := DecDegrees;
       RadioGroup4.ItemIndex := ord(MDDef.OutPutLatLongMethod);
       Edit2.Text := '';
       Edit6.Text := '';
       Edit3.Text := '';
       Edit7.Text := '';
       Edit4.Text := '';
       Edit8.Text := '';
       ChangeInputMethod;
       CheckBox1.Checked := MDdef.UseLongsto360;
       if DatumConstants.h_DatumCode = '' then Caption := Prompt
       else Caption := Prompt + ' (' + DatumName(DatumConstants.h_DatumCode) + ')';
       if Default then
       begin
          WriteLatLongToBoxes(Lat,Long);

          if CoordOption then
          begin
             Edit1.Text := DatumConstants.LatLongToMGRS(Lat,Long);
             case MDDef.CoordUse of
                coordLatLong       : GetLatLongDlg.TabbedNotebook1.PageIndex := 0;
                CoordMGRS,coordUTM : GetLatLongDlg.TabbedNotebook1.PageIndex := 1;
                coordFullUTM       : GetLatLongDlg.TabbedNotebook1.PageIndex := 2;
                else GetLatLongDlg.TabbedNotebook1.PageIndex := 0;
             end;
             DatumConstants.ForwardProjectDegrees(Lat,Long,XUTM,YUTM);
             Edit9.Text := RealToString(xutm,-18,0);
             Edit10.Text := RealToString(yutm,-18,0);
             Edit5.Text := IntegerToString(GetUTMZone(Long));
             Edit11.Text := GridZoneDes(Lat,Long);
             DatumConstants.GetHundredKMeterSquareID(SquareID,XUTM,YUTM);
             Edit12.Text := SquareID;
             while (xutm >= 100000.0) do xutm := xutm - 100000.0;
             while (yutm >= 100000.0) do yutm := yutm - 100000.0;
             Edit13.Text := RealToString(xutm,-18,0);
             Edit14.Text := RealToString(yutm,-18,0);
             if MDDef.CoordUse = CoordMGRS then CheckBox2Click(Nil);
             if MDDef.CoordUse = coordUTM then CheckBox3Click(Nil);
          end
          else
          begin
             Edit1.Enabled := false;
             Edit5.Enabled := false;
             Edit9.Enabled := false;
             Edit10.Enabled := false;
             Edit11.Enabled := false;
             Edit12.Enabled := false;
             Edit13.Enabled := false;
             Edit14.Enabled := false;
             GetLatLongDlg.TabbedNotebook1.PageIndex := 0;
          end;
       end
       else begin
          if MDDef.DefaultLatHemi = 'N' then
          begin
              RadioGroup1.ItemIndex := 0;
              RadioGroup3.ItemIndex := 0;
          end
          else
          begin
             RadioGroup1.ItemIndex := 1;
             RadioGroup3.ItemIndex := 1;
          end;
          {$IfDef MICRODEM}
          Edit5.Text := IntToStr(MDdef.DefaultUTMZone);
          {$EndIf}
          if MDDef.DefaultLongHemi = 'W' then RadioGroup2.ItemIndex := 0 else
          RadioGroup2.ItemIndex := 1;
       end;

       repeat
          GetLatLongDlg.ShowModal;
          Lat := 567;
          OK := true;
          if (GetLatLongDlg.TabbedNotebook1.PageIndex = 0) then begin
             ReadLatLongFromBoxes(Lat,Long,OK);
          end;

          {$ifDef Microdem}
          if CoordOption then
          begin
             if GetLatLongDlg.TabbedNotebook1.PageIndex = 1 then
             begin
                if CheckBox3.Checked then MenuStr := Edit11.Text + Edit12.Text + Edit13.Text + Edit14.Text
                else MenuStr := Edit1.Text;
                DatumConstants.MGRStoLatLong(MenuStr,Lat,Long);
             end;
             if GetLatLongDlg.TabbedNotebook1.PageIndex = 2 then  begin
                if RadioGroup3.ItemIndex = 0 then DatumConstants.LatHemi := 'N' else DatumConstants.LatHemi := 'S';
                CheckEditString(Edit5.Text,DatumConstants.projUTMZone);
                CheckEditString(Edit9.Text,xUTM);
                CheckEditString(Edit10.Text,yUTM);
                DatumConstants.DefineDatumFromUTMZone(DatumConstants.h_DatumCode,DatumConstants.projUTMZone,DatumConstants.LatHemi,'LatLongInBox');
                DatumConstants.UTMtoLatLongDegree(XUTM,YUTM,Lat,Long);
             end;
          end;
          {$EndIf}
          if OK then OK := (abs(Lat) <= 90) and
             ((abs(Long) <= 180) and (not MDdef.UseLongsto360)) or
                ((abs(Long) <= 360) and (MDdef.UseLongsto360)) ;
          if not OK then MessageToContinue('Invalid position');
        until OK;
        While Long > 180 do Long := Long - 360;
        While Long < -180 do Long := Long + 360;
    end;
end;


procedure GetLatLong;
begin
   LatLongInBox(Prompt,DatumConstants,Lat,Long,false,true);
end;


procedure GetLatLongDefault;
begin
   LatLongInBox(Prompt,DatumConstants,Lat,Long,true,true);
end;


procedure GetLatLongDefaultNoDatum(Prompt : ShortString; var Lat,Long : float64);
begin
   LatLongInBox(Prompt,WGS84DatumConstants,Lat,Long,true,false);
end;

procedure GetLatLongNoDatum(Prompt : ShortString; var Lat,Long : float64);
//var
   //DatumConstants : tBaseMap;
begin
   //DatumConstants := tBaseMap.Create;
  // DatumConstants.h_DatumCode := '';
   LatLongInBox(Prompt,WGS84DatumConstants,Lat,Long,false,true);
   //DatumConstants.Destroy;
end;


procedure GetLatLongInRadiansNoDatum(Prompt : ShortString; var Lat,Long : float64);
//var
   //DatumConstants : tBaseMap;
begin
   {$IfDef RecordGetLatLong}  WriteLineToDebugFile('GetLatLongInRadiansNoDatum, lat= ' + RealToString(Lat,-12,-4) + '  long=' + RealToString(Long,-12,-4)); {$EndIf}
   Lat := Lat / DegToRad;
   Long := Long / DegToRad;
   {$IfDef RecordGetLatLong} WriteLineToDebugFile('GetLatLongInRadiansNoDatum ' + LatLongDegreeToString(Lat,Long));  {$EndIf}
   GetLatLongNoDatum(Prompt,Lat,Long);
   Lat := Lat * DegToRad;
   Long := Long * DegToRad;
end;


procedure TGetLatLongDlg.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\enter_lat.htm');
end;

procedure TGetLatLongDlg.OKBtnClick(Sender: TObject);
begin
   if CheckBox4.Checked then begin
      if RadioGroup1.ItemIndex = 0 then MDDef.DefaultLatHemi := 'N' else MDDef.DefaultLatHemi := 'S';
      if RadioGroup2.ItemIndex = 0 then MDDef.DefaultLongHemi := 'W' else MDDef.DefaultLongHemi := 'E';
      MDDef.DefaultLatHemi := MDDef.DefaultLatHemi;
      MDDef.DefaultLongHemi := MDDef.DefaultLongHemi;
   end;
   MDDef.SaveDefaultHemis := CheckBox4.Checked;
end;

procedure TGetLatLongDlg.BitBtn1Click(Sender: TObject);
begin
   RadioGroup4.ItemIndex := 0;
   WriteLatLongToBoxes(DEMMapf.Clipboard_Lat,DEMMapf.Clipboard_Long);
end;

procedure TGetLatLongDlg.ChangeInputMethod;
begin
    Edit3.Enabled := (RadioGroup4.ItemIndex > 0);
    Edit7.Enabled := RadioGroup4.ItemIndex > 0;
    Minutes.Enabled := RadioGroup4.ItemIndex > 0;
    Edit4.Enabled := RadioGroup4.ItemIndex > 1;
    Edit8.Enabled := RadioGroup4.ItemIndex > 1;
    Seconds.Enabled := RadioGroup4.ItemIndex > 1;
    if (RadioGroup4.ItemIndex = 0) then begin
       Edit3.Text := '';
       Edit7.Text := '';
    end;
    if (RadioGroup4.ItemIndex in [0,1]) then begin
       Edit4.Text := '';
       Edit8.Text := '';
    end;
end;


procedure TGetLatLongDlg.CheckBox2Click(Sender: TObject);
begin
   Edit1.Enabled := true;
   Edit11.Enabled := false;
   Edit12.Enabled := false;
   Edit13.Enabled := false;
   Edit14.Enabled := false;
   CheckBox3.Checked := false;
end;


procedure TGetLatLongDlg.CheckBox3Click(Sender: TObject);
begin
   Edit1.Enabled := false;
   Edit11.Enabled := true;
   Edit12.Enabled := true;
   Edit13.Enabled := true;
   Edit14.Enabled := true;
   CheckBox2.Checked := false;
end;


procedure TGetLatLongDlg.RadioGroup4Click(Sender: TObject);
var
   Lat,Long : float64;
   OK : boolean;
begin
   ReadLatLongFromBoxes(Lat,Long,OK);
   ChangeInputMethod;
   MDDef.OutPutLatLongMethod := tLatLongMethod(RadioGroup4.ItemIndex);
   WriteLatLongToBoxes(Lat,Long);
end;

procedure TGetLatLongDlg.FormCreate(Sender: TObject);
begin
   BitBtn1.Visible := DEMMapf.ClipBoard_Coords;
   Petmar.PlaceFormAtMousePosition(Self);
end;

initialization
finalization
   {$IfDef RecordGetLatLong} WriteLineToDebugFile('RecordGetLatLong active in GetLatLn'); {$EndIf}
end.
