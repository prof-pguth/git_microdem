unit Demcnvrt;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordDEMconvert}
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,StdCtrls, ExtCtrls, Dialogs,SysUtils,
   Petmar_types,PETMAR,DEMDefs,Basemap;

type
  TCoordConverter = class(TForm)
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    GroupBox1: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button4: TButton;
    SaveDialog1: TSaveDialog;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Button6: TButton;
    CheckBox4: TCheckBox;
    Label11: TLabel;
    CheckBox5: TCheckBox;
    Label12: TLabel;
    Label16: TLabel;
    BitBtn1: TBitBtn;
    Label14: TLabel;
    Label13: TLabel;
    BitBtn2: TBitBtn;
    Label15: TLabel;
    BitBtn3: TBitBtn;
    Label17: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private { Private declarations }
    procedure Refigure;
    procedure ZeroOutput;
    procedure AddToMemo(Line : shortString);
    procedure InputCoords;
  public  { Public declarations }
    Working : boolean;
    InputMap,OutputMap  : tMapProjection;
    inxutm,inyutm,
    InLat,InLong,OutLat,OutLong : float64;
    HitGoodButton               : boolean;
    DatumList                   : tStringList;
    SecondMemo : tMemo;
  end;

  procedure ConvertCoordinates(SecondMemo : tMemo = Nil);

implementation

{$R *.DFM}

uses

{Main program MDI window for different programs that use this module}
   Nevadia_Main,
{End MDI parent declaration}

   Printers,
   PETMath,GetLatLn,
   DEMCoord;


procedure ConvertCoordinates;
var
  CoordConverter : TCoordConverter;
begin
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('ConvertCoordinates in'); {$EndIf}
   CoordConverter := TCoordConverter.Create(Application);
   CoordConverter.SecondMemo := SecondMemo;
   CoordConverter.ShowModal;
end;


procedure TCoordConverter.FormCreate(Sender: TObject);
begin
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('TCoordConverter.FormCreate in'); {$EndIf}
   InLat := 36;
   InLong := -117;

   Petmar.PlaceFormAtMousePosition(Self);
   InputMap  := tMapProjection.Create;
   InputMap.h_DatumCode := MDDef.PreferPrimaryDatum;  //'WGS84';
   InputMap.projUTMZone := MDDef.DefaultUTMZone;
   InputMap.projUTMZone := GetUTMZone(InLong);

   OutputMap := tMapProjection.Create;
   OutputMap.h_DatumCode := MDDef.PreferSecondaryDatum;  //  'NAD27';
   OutputMap.projUTMZone := MDDef.DefaultUTMZone;

   case MDdef.CoordUse of
      coordUTM      : CheckBox2.Checked := true;
      coordLatLong  : CheckBox1.Checked := true;
      //CoordMGRS     : CheckBox3.Checked := true;
   end;
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('initial definitions over'); {$EndIf}
   InputMap.ForwardProjectDegrees(InLat,InLong,inXUTM,inYUTM);
   BitBtn1.Enabled := true;
   ZeroOutput;
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('TCoordConverter.FormCreate out'); {$EndIf}
end;



function SpecialCaseUTM(UTMZone : integer; Lat : float64) : boolean;
begin
   Result := false;
   {check for Norway, Svalbard}
   if (UTMZone in [31,32]) and (Lat > 56) and (Lat < 64) then Result := true;
   if (UTMZone in [31..37]) and (Lat > 72) and (Lat < 84) then Result := true;
end;



procedure TCoordConverter.ZeroOutput;
begin
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('TCoordConverter.ZeroOutput in'); {$EndIf}
   Label11.Caption := '';
   Label10.Caption := '';
   Label3.Caption := '';
   Label4.Caption := '';
   Label5.Caption := '';
   Label12.Caption := '';
   Label16.Caption := '';
   Label7.Caption := '';
   Label8.Caption := '';
   Label9.Caption := '';
   Label13.Caption := DatumName(InputMap.h_DatumCode);
   Label14.Caption := 'Ellipsoid: ' + EllipsoidName(InputMap.h_EllipsCode);
   Label17.Caption := DatumName(OutputMap.h_DatumCode);
   Label15.Caption := 'Ellipsoid: ' + EllipsoidName(OutputMap.h_EllipsCode);
   Button1.Enabled := (InputMap.h_DatumCode <> OutputMap.h_DatumCode);
   if Button1.Enabled then begin
      Label1.Caption := 'dx=' + IntegerToString(-OutputMap.H_xdat + InputMap.h_xdat,-8);
      Label2.Caption := 'dy=' + IntegerToString(-OutputMap.H_ydat + InputMap.h_ydat,-8);
      Label6.Caption := 'dz=' + IntegerToString(-OutputMap.h_zdat + InputMap.h_zdat,-8);
      AddToMemo(InputMap.h_DatumCode + '  ======>  ' + OutputMap.h_DatumCode);
   end;
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('TCoordConverter.ZeroOutput out,' + InputMap.h_DatumCode + '  ======>  ' + OutputMap.h_DatumCode); {$EndIf}
end;


procedure TCoordConverter.InputCoords;
begin
   Label3.Caption := LatLongDegreeToString(InLat,InLong);
   Label4.Caption := LatLongDegreeToString(InLat,InLong,DecMinutes);
   Label5.Caption := LatLongDegreeToString(InLat,InLong,DecSeconds);
   Label12.Caption := InputMap.UTMStringFromLatLongDegree(inLat,inLong,false);
   if InputMap.MGRSvalid then Label16.Caption := 'MGRS: ' + InputMap.CalculateMGRS(inXUTM,inYUTM,10);
end;


procedure TCoordConverter.Refigure;
var
   MidLat,MidLong,
   XUTM,YUTM : float64;

      procedure InvalidateConversion;
      begin
         Label11.Caption := '';
         Label10.Caption := '';
      end;


begin
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('TCoordConverter.Refigure in, Input datum='  + InputMap.h_DatumCode + '  output datum='  + OutputMap.h_DatumCode); {$EndIf}
   InputCoords;
   Button6.Enabled := false;
   InputMap.ForwardProjectDegrees(InLat,(-186 + 6*InputMap.projUTMZone),XUTM,YUTM);
   if (inXUTM - XUTM) < 40000 then Button6.Enabled := true;
   InputMap.ForwardProjectDegrees(InLat,(-180 + 6*InputMap.projUTMZone),XUTM,YUTM);
   if (XUTM - inXUTM) < 40000 then Button6.Enabled := true;

  {transform to WGS84}
   if InputMap.h_DatumCode = 'WGS84' then begin
      MidLat := InLat;
      MidLong := InLong;
   end
   else begin
      MolodenskiyTransformation(inLat,inLong,MidLat,MidLong,InputMap,WGS84DatumConstants);
   end;

  {transform to desired output datum}
   if OutputMap.h_DatumCode = 'WGS84' then begin
      OutLat := MidLat;
      OutLong := MidLong;
   end
   else begin
      MolodenskiyTransformation(MidLat,MidLong,OutLat,OutLong,WGS84DatumConstants,OutputMap);
   end;

   Label7.Caption := LatLongDegreeToString(OutLat,OutLong);
   Label8.Caption := LatLongDegreeToString(OutLat,OutLong,DecMinutes);
   Label9.Caption := LatLongDegreeToString(OutLat,OutLong,DecSeconds);
   OutputMap.DefineDatumFromUTMZone(OutputMap.h_DatumCode,InputMap.projUTMZone,InputMap.LatHemi,'cc1');
   if SpecialCaseUTM(InputMap.projUTMZone,InLat) then begin
      MessageToContinue('Norway and Svalbard have unique UTM grids.' + MessLineBreak + 'Program will not correctly work there.');
      InvalidateConversion;
      Label10.Caption := 'Norway/Svalbard special grid';
   end
   else if (InLat < 84) and (InLat > -80) then begin
      Label10.Caption := OutputMap.UTMStringFromLatLongDegree(OutLat,OutLong,false);
      if OutputMap.MGRSvalid then Label11.Caption := 'MRGS: ' + OutputMap.LatLongToMGRS(OutLat,OutLong);
   end
   else begin
      InvalidateConversion;
      Label10.Caption := 'UTM invalid--use UPS grid';
   end;
end;


procedure TCoordConverter.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   DatumList.Free;
   InputMap.Destroy;
   OutputMap.Destroy;
end;


procedure TCoordConverter.BitBtn1Click(Sender: TObject);
begin
   GetLatLongDefault(InputMap,'Input coordinates',InLat,InLong);
   InputMap.DefineDatumFromUTMZone(InputMap.h_DatumCode,GetUTMZone(InLong),InputMap.LatHemi,'TCoordConverter.BitBtn1ClicK');
   InputMap.ForwardProjectDegrees(InLat,InLong,inXUTM,inYUTM);
   ZeroOutput;
   InputCoords;
end;


procedure TCoordConverter.BitBtn2Click(Sender: TObject);
begin
   if (Sender <> Nil) then PickDatum('Input',InputMap.h_DatumCode);
   InputMap.DefineDatumFromUTMZone(InputMap.h_DatumCode,InputMap.projUTMZone,InputMap.LatHemi,'TCoordConverter.BitBtn2ClicK');
   ZeroOutput;
end;


procedure TCoordConverter.BitBtn3Click(Sender: TObject);
begin
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('TCoordConverter.BitBtn3Click (get secondary datum) in,  old second datum=' + DatumName(OutputMap.h_DatumCode)); {$EndIf}
   if (Sender <> Nil) then PickDatum('output',OutputMap.h_DatumCode);
   OutputMap.DefineDatumFromUTMZone(OutputMap.h_DatumCode,OutputMap.projUTMZone,OutputMap.LatHemi,'TCoordConverter.BitBtn3ClicK');
   ZeroOutput;
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('TCoordConverter.BitBtn3Click out, new second datum=' + DatumName(OutputMap.h_DatumCode)); {$EndIf}
end;


procedure TCoordConverter.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme16k4.htm');
end;

procedure TCoordConverter.Button2Click(Sender: TObject);
begin
   Memo1.Lines.Clear;
end;

procedure TCoordConverter.Button4Click(Sender: TObject);
begin
  with SaveDialog1 do if Execute then Memo1.Lines.SaveToFile(Filename);
end;


procedure TCoordConverter.AddToMemo(Line : shortString);
begin
   Memo1.Lines.Add(Line);
   if (SecondMemo <> Nil) then SecondMemo.Lines.Add(Line);
end;


procedure TCoordConverter.Button1Click(Sender: TObject);
var
   InCoord : ShortString;
begin
   Refigure;
    case MDDef.CoordUse of
       coordLatLong       : InCoord := LatLongDegreeToString(InLat,InLong,MDdef.OutPutLatLongMethod);
       CoordMGRS,coordUTM : InCoord := Label16.Caption;
       coordFullUTM       : InCoord := Label12.Caption;
    end;

   InCoord := InCoord + '  ====>  ';
   if CheckBox1.Checked then begin
      if CheckBox5.Checked then begin
         AddToMemo(LatLongDegreeToString(InLat,InLong,DecDegrees) + '  ====>  ' + Label7.Caption);
         AddToMemo(LatLongDegreeToString(InLat,InLong,DecMinutes) + '  ====>  ' + Label8.Caption);
         AddToMemo(LatLongDegreeToString(InLat,InLong,DecSeconds) + '  ====>  ' + Label9.Caption);
      end
      else AddToMemo(InCoord +  LatLongDegreeToString(OutLat,OutLong,MDdef.OutPutLatLongMethod));
   end;
   if CheckBox2.Checked then AddToMemo(InCoord + Label10.Caption);
   //if CheckBox3.Checked then AddToMemo(InCoord + Label11.Caption);
   AddToMemo('');
   if CheckBox4.Checked then Button6Click(Nil);
end;


procedure TCoordConverter.Button6Click(Sender: TObject);
var
   OverlapMap : tMapProjection;
   xutm,yutm  : float64;
   ch         : AnsiChar;

   procedure DoOverlapMap(Add : integer);
   begin
      OverlapMap := InputMap;
      OverlapMap.projUTMZone := OverlapMap.projUTMZone + Add;
      if (OverlapMap.projUTMZone = 0) then OverlapMap.projUTMZone := 60;
      if (OverlapMap.projUTMZone = 61) then OverlapMap.projUTMZone := 1;
      OverlapMap.DefineDatumFromUTMZone(OverlapMap.h_DatumCode,OverlapMap.projUTMZone,OverlapMap.LatHemi,'TCoordConverter.Button6Click');
      OverlapMap.ForwardProjectDegrees(InLat,InLong,XUTM,YUTM);
      AddToMemo('Overlapping grid input datum:  x=' + RealToString(XUTM,-18,0) + '   y=' + RealToString(YUTM,-18,0) + '   ' + OverlapMap.CalculateMGRS(XUTM,YUTM,10));

      OverlapMap := OutputMap;
      OverlapMap.projUTMZone := OverlapMap.projUTMZone + Add;
      if (OverlapMap.projUTMZone = 0) then OverlapMap.projUTMZone := 60;
      if (OverlapMap.projUTMZone = 61) then OverlapMap.projUTMZone := 1;
      OverlapMap.DefineDatumFromUTMZone(OverlapMap.h_DatumCode,OverlapMap.projUTMZone,OverlapMap.LatHemi,'TCoordConverter.Button6Click');
      OverlapMap.ForwardProjectDegrees(OutLat,OutLong,XUTM,YUTM);
      AddToMemo('Overlapping grid output datum:  x=' + RealToString(XUTM,-18,0) + '   y=' + RealToString(YUTM,-18,0) + '   ' + OverlapMap.CalculateMGRS(XUTM,YUTM,10));
      AddToMemo('');
   end;

begin
   OverlapMap := tMapProjection.Create;
   if (InLat >= 0) then ch := 'N' else ch := 'S';
   InputMap.ForwardProjectDegrees(InLat,(-186 + 6*InputMap.projUTMZone),XUTM,YUTM);
   if (inXUTM - XUTM) < 40000 then begin
      OverlapMap := InputMap;
      dec(OverlapMap.projUTMZone);
      if (OverlapMap.projUTMZone = 0) then OverlapMap.projUTMZone := 60;
      OverlapMap.DefineDatumFromUTMZone(OverlapMap.h_DatumCode,OverlapMap.projUTMZone,OverlapMap.LatHemi,'TCoordConverter.Button6Click');
      OverlapMap.ForwardProjectDegrees(InLat,InLong,XUTM,YUTM);
      AddToMemo('Overlapping grid input datum:  x=' + RealToString(XUTM,-18,0) + '   y=' + RealToString(YUTM,-18,0) + '   ' + OverlapMap.CalculateMGRS(XUTM,YUTM,10));

      OverlapMap := OutputMap;
      dec(OverlapMap.projUTMZone);
      if (OverlapMap.projUTMZone = 0) then OverlapMap.projUTMZone := 60;
      OverlapMap.DefineDatumFromUTMZone(OverlapMap.h_DatumCode,OverlapMap.projUTMZone,OverlapMap.LatHemi,'TCoordConverter.Button6Click');
      OverlapMap.ForwardProjectDegrees(OutLat,OutLong,XUTM,YUTM);
      AddToMemo('Overlapping grid output datum:  x=' + RealToString(XUTM,-18,0) + '   y=' + RealToString(YUTM,-18,0) + '   ' + OverlapMap.CalculateMGRS(XUTM,YUTM,10));
      AddToMemo('');
   end;

   InputMap.ForwardProjectDegrees(InLat,(-180 + 6*InputMap.projUTMZone),XUTM,YUTM);
   if (XUTM - inXUTM) < 40000 then begin
      OverlapMap := InputMap;
      inc(OverlapMap.projUTMZone);
      if (OverlapMap.projUTMZone = 61) then OverlapMap.projUTMZone := 1;
      OverlapMap.DefineDatumFromUTMZone(OverlapMap.h_DatumCode,OverlapMap.projUTMZone,OverlapMap.LatHemi,'TCoordConverter.Button6Click');
      OverlapMap.ForwardProjectDegrees(InLat,InLong,XUTM,YUTM);
      AddToMemo('Overlapping grid input coordinates:  x=' + RealToString(XUTM,-18,0) + '   y=' + RealToString(YUTM,-18,0) + '   ' + OverlapMap.CalculateMGRS(XUTM,YUTM,10));

      OverlapMap := OutputMap;
      inc(OverlapMap.projUTMZone);
      if (OverlapMap.projUTMZone = 61) then OverlapMap.projUTMZone := 1;
      OverlapMap.DefineDatumFromUTMZone(OverlapMap.h_DatumCode,OverlapMap.projUTMZone,OverlapMap.LatHemi,'TCoordConverter.Button6Click');
      OverlapMap.ForwardProjectDegrees(OutLat,OutLong,XUTM,YUTM);
      AddToMemo('Overlapping grid output coordinates:  x=' + RealToString(XUTM,-18,0) +  '   y=' + RealToString(YUTM,-18,0) + '   ' + OverlapMap.CalculateMGRS(XUTM,YUTM,10));
      AddToMemo('');
   end;
   OverlapMap.Destroy;
end;


initialization
finalization
   {$IfDef RecordDEMconvert} WriteLineToDebugFile('RecordDEMconvert active in demcnvrt'); {$EndIf}
end.
