unit kml_opts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{ verified 1/24/2014              }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$Define KMLProblems}
   {$Define ShowKMLProofing}
   //{$Define SaveHTMLVersions}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,StrUtils,  Buttons,DB,
  KML_creator,Petmar_types, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  Tkml_opts_fm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Edit3: TEdit;
    Edit4: TEdit;
    Label4: TLabel;
    ComboBox2: TComboBox;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    Label5: TLabel;
    ComboBox4: TComboBox;
    BitBtn2: TBitBtn;
    Label8: TLabel;
    ComboBox5: TComboBox;
    Label9: TLabel;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    RadioGroup1: TRadioGroup;
    Edit6: TEdit;
    Label10: TLabel;
    Edit7: TEdit;
    Label11: TLabel;
    CheckBox1: TCheckBox;
    ComboBox8: TComboBox;
    Label12: TLabel;
    CheckBox2: TCheckBox;
    BitBtn3: TBitBtn;
    Image1: TImage;
    CheckBox3: TCheckBox;
    Edit8: TEdit;
    Label13: TLabel;
    CheckBox4: TCheckBox;
    Label6: TLabel;
    ComboBox3: TComboBox;
    CheckBox5: TCheckBox;
    CheckBox7: TCheckBox;
    procedure BitBtn3Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     IconFName : PathStr;
  end;

procedure ConvertToKML(DBonTable : integer; ThumbNailDir : PathStr; kml : KML_Creator.tKMLCreator = Nil; SingleRecordOnly : boolean = false; SepExpField : ShortString = '');

procedure CleanHTMLFile(var TStrl : tStringList; RemoveLinks : boolean = false);


var
   LastKMLDir : PathStr;

implementation

{$R *.dfm}

uses
   {$IfDef ExGeography} {$Else} KoppenGr, {$EndIf}
   Petmar,DEMCoord,DEMDefs,DEMDatabase,DEMESRIShapefile,PETDBUtils,
   PETImage, nevadia_main;


function ControlChar(Input : string; i : integer) : boolean;
begin
   Result := (Copy(Input,i,5) = '&amp;') or (Copy(Input,i,6) = '&apos;') or (Copy(Input,i,6) = '&quot;') or (Copy(Input,i,6) = '&nbsp;') or (Copy(Input,i,4) = '&gt;');
end;


procedure ExtractMainBodyOfHTML(var TStrl : tStringList);
var
  OneLine : ANSIstring;
  EndIt : boolean;
  i : Integer;
begin
   repeat
      OneLine := ptTrim(TStrl[0]);
      EndIt := UpperCase(Copy(oneLine,1,5)) = '<BODY';
      if not EndIt then TStrl.Delete(0);
      if (TStrl.Count = 0) then exit;
   until EndIt;

   i := 0;
   while i <= pred(TStrl.Count) do begin
      OneLine := TStrl[i];
      if UpperCase(Copy(oneLine,1,6)) = '</BODY' then break;
      inc(i);
   end;
   for I := pred(TStrl.Count) downto i do TStrl.Delete(i);
end;


function KML_proof_string(Input : string; var Intag : boolean) : string;
var
   i : integer;
   Add : shortstring;
begin
   Result := '';
   for i := 1 to Length(Input) do begin
      if Input[i] = '<' then InTag := true;
      if InTag then begin
         Add := Input[i];
         if Input[i] = '>' then InTag := false;
      end
      else begin
         case Input[i] of
            '&'  : if ControlChar(Input,i) then Result := Result + Input[i] else Result := Result + '&amp;';
            #39,#96,#146  : Add := '&acute;';
            '"'  : Add := '&quot;';
            //'>'  : Result := Result + '&gt;';
            else Add := Input[i];
         end;
      end;
      Result := Result + Add;
   end;
   {$IfDef ShowKMLProofing} WriteLineToDebugFile('KML_proof_string in, Input=' + Input +  'output=' + Result);   {$EndIf}
end;


procedure CleanHTMLTags(var TStrl : tStringList; start,Ending : shortstring);
var
   x,i : integer;
   s : AnsiString;
begin
   {$IfDef ShowKMLProofing} WriteLineToDebugFile('CleanHTMLTags, ' + Start + '  with  ' + Ending); {$EndIf}
   i := 0;
   while i <= pred(TStrl.Count) do begin
      x := 1;
      s := TStrl[i];
      if StrUtils.AnsiContainsText(UpperCase(s),Uppercase(Start)) then begin
         while not StrUtils.AnsiContainsText(UpperCase(s),Uppercase(Ending)) do begin
            TStrl[i] := '';
            inc(i);
            s := s + TStrl[i];
         end;

         repeat
            if UpperCase(Copy(s,x,length(Start))) = UpperCase(Start) then begin
               inc(x,length(Start));
               while not (UpperCase(Copy(s,x,length(Ending))) = UpperCase(Ending)) and (x < length(s)) do begin
                  Delete(s,x,1);
               end;
           end;
           inc(x);
         until (UpperCase(Copy(s,x,length(Ending))) = UpperCase(Ending));
      end;
      TStrl[i] := s;
      inc(i);
   end;
   {$IfDef ShowKMLProofing} WriteLineToDebugFile('done CleanHTMLTags, ' + Start + '  with  ' + Ending); {$EndIf}
end;


procedure RemoveHTMLTags(var TStrl : tStringList; Remove,Replace : shortstring);
var
   x,i : integer;
   s : AnsiString;
begin
   {$IfDef ShowKMLProofing} WriteLineToDebugFile('RemoveHTMLTags, ' + Remove + '  with  ' + Replace); {$EndIf}
   i := 0;
   while i <= pred(TStrl.Count) do begin
      x := 1;
      s := TStrl[i];
      repeat
         if Copy(s,x,length(Remove)) = Remove then begin
            Delete(s,x,length(Remove));
            Insert(Replace,s,x);
         end;
         inc(x);
      until (x >= length(s)) or (s = '');
      TStrl[i] := s;
      inc(i);
   end;
   {$IfDef ShowKMLProofing} WriteLineToDebugFile('done RemoveHTMLTags, ' + Remove + '  with  ' + Replace); {$EndIf}
end;


procedure CleanHTMLFile(var TStrl : tStringList; RemoveLinks : boolean = false);
const
   RemoveNBSP = true;
   RemovePtags = false;
   CleanTags = true;
var
   CopySTrl : tStringList;
   Str : string;
   i : integer;
begin
   {$IfDef ShowKMLProofing} WriteLineToDebugFile('CleanHTMLFile in'); {$EndIf}
   CopyStrl := tStringList.Create;
   Str := '';
   for i := 0 to pred(TStrl.Count) do begin
      Str := Str + ' ' + TStrl[i];
      if (i > 0) and (Str[length(Str)] = '>') or (i = pred(TStrl.Count)) then begin
         CopyStrl.Add(Str);
         Str := '';
      end;
   end;
   TStrl.Clear;
   for i := 0 to pred(CopyStrl.Count) do TStrl.Add(CopyStrl.Strings[i]);
   CopyStrl.Free;

   {$IfDef ShowKMLProofing}  WriteStringListToDebugFile(TStrl,true); {$EndIf}

   {$IfDef ShowKMLProofing} WriteLineToDebugFile('CleanHTMLFile, CopyStrl.Freed'); {$EndIf}

   if RemoveNBSP then RemoveHTMLTags(TStrl,'&nbsp;', ' ');

   if CleanTags then begin
      CleanHTMLTags(TStrl,'<p','>' );
      CleanHTMLTags(TStrl,'<table','>' );
      CleanHTMLTags(TStrl,'<li','>' );
      CleanHTMLTags(TStrl,'<tr','>' );
      CleanHTMLTags(TStrl,'<td','>' );
      CleanHTMLTags(TStrl,'<col','>' );
      CleanHTMLTags(TStrl,'<body','>' );
      CleanHTMLTags(TStrl,'<div','>' );
      CleanHTMLTags(TStrl,'<span','>' );
      CleanHTMLTags(TStrl,'<st1','>');
      CleanHTMLTags(TStrl,'</st1','>');
   end;

   {$IfDef ShowKMLProofing} WriteLineToDebugFile('CleanHTMLFile, start remove tags'); {$EndIf}
   RemoveHTMLTags(TStrl,'<st1>','');
   RemoveHTMLTags(TStrl,'</st1>','');

   if RemovePtags then begin
      RemoveHTMLTags(TStrl,'<p>','');
      RemoveHTMLTags(TStrl,'</p>','');
   end;

   if RemoveLinks then begin
      CleanHTMLTags(TStrl,'<a href="java','">');
      CleanHTMLTags(TStrl,'<a','>');
      RemoveHTMLTags(TStrl,'<a>','');
      RemoveHTMLTags(TStrl,'</a>','');
   end;
   {$IfDef ShowKMLProofing} WriteLineToDebugFile('CleanHTMLFile, done remove links'); {$EndIf}
   RemoveHTMLTags(TStrl,'<o:p>','');
   RemoveHTMLTags(TStrl,'</o:p>','');
   RemoveHTMLTags(TStrl,'<strong>','');
   RemoveHTMLTags(TStrl,'</strong>','');
   RemoveHTMLTags(TStrl,'<span>','');
   RemoveHTMLTags(TStrl,'</span>','');
   RemoveHTMLTags(TStrl,'<table>','<table border="2">');
   RemoveHTMLTags(TStrl,'<v:>','');
   RemoveHTMLTags(TStrl,'</v:>','');
end;


procedure ConvertImageLinksHTML(var TStrl : tStringList; fName,KMLOutPutPath : PathStr);
var
   ch : AnsiChar;
   i,j : integer;
   First : boolean;
   OneLine, Needed,p0,p1,p2,p3 : ANSIString;
begin
   Needed := 'src="';
   for i := 0 to pred(TStrl.Count) do begin
       OneLine := TStrl[i];
       if StrUtils.AnsiContainsText(OneLine,Needed) then begin
          j := 1;
          while Copy(OneLine,j,Length(Needed)) <> Needed do inc(j);
          p1 := Copy(OneLine,1,pred(J) + Length(Needed));
          p3 := OneLine;
          Delete(p3,1,pred(J) + Length(Needed));
          p2 := Petmar_types.BeforeSpecifiedCharacterANSI(p3,'"',true,true);
          for j := 1 to length(p2) do  if p2[j] = '/' then p2[j] := '\';

          p0 := p2;
          p2 := ExtractFileName(p2);
          ch := 'a';
          First := true;
          while FileExists(KMLOutputPath + p2) do begin
             if First or (ch = 'z') then p2 := ch + p2 else p2[1] := ch;
             First := false;
             if ch = 'z' then ch := 'a' else inc(ch);
          end;
          if (p2 <> '') then begin
             if FileExists(fName + p0) then CopyFile(fName + p0,KMLOutputPath + p2)
             else MessageToContinue('Image file missing: ' + fName + p0);
          end;
          TStrl[i] := p1 + p2 + '" ' + p3;
       end;
   end;
end;


procedure ConvertToKML(DBonTable : integer; ThumbNailDir : PathStr; kml : KML_Creator.tKMLCreator = Nil; SingleRecordOnly : boolean = false; SepExpField : ShortString = '');
var
   kml_opts_fm: Tkml_opts_fm;
   fname : PathStr;
   NameField,FolderName,FolderDesc,IconName,
   MenuStr,TStr : shortstring;
   Options,Desc : string;
   IconScaleFactor,Lat,Long : float64;
   z : float32;
   i : integer;
   AllFilters,AllIcons,TStrl,FieldsInDB : tStringList;
   LocalKML : boolean;


      function KML_FixDate(TheDate : string35) : string35;
      var
         Month,Day : shortstring;
         Year : AnsiString;
      begin
         if StrUtils.AnsiContainsText(TheDate,'/') then begin
            Year := TheDate;
            Month := Petmar_types.BeforeSpecifiedCharacterANSI(Year,'/',true,true);
            Day := Petmar_types.BeforeSpecifiedCharacterANSI(Year,'/',true,true);
            TheDate := Year + '-' + Month + '-' + Day;
         end;
         if TheDate[7] = '-' then Insert('0',TheDate,6);
         if TheDate[9] = '-' then Insert('0',TheDate,8);
         Result := TheDate;
      end;


      function TimeSpanString : string;
      var
         BeginTime,EndTime,TStr : string35;
         i : integer;
      begin
         Result := '';
         if (kml_opts_fm.ComboBox6.Text = '') or (not kml_opts_fm.CheckBox3.Checked) then exit;
         BeginTime := KML_FixDate(GISdb[DBonTable].MyData.GetFieldByNameAsString(kml_opts_fm.ComboBox6.Text));
         if (kml_opts_fm.ComboBox7.Text <> '') then EndTime := KML_FixDate(GISdb[DBonTable].MyData.GetFieldByNameAsString(kml_opts_fm.ComboBox7.Text))
         else EndTime := '';
         if (BeginTime <> '') or (EndTime <> '') then begin
            if (EndTime = '') then begin
               Result := '<TimeStamp> ' + BeginTime +  '</TimeStamp>';
            end
            else begin
               Result := '<TimeSpan> ';
               if (BeginTime <> '') then begin
                  i := AnsiPos('T',BeginTime);
                  if i > 0 then TStr := '' else TStr := JustAfterMidnight;
                  Result := Result + '<begin>' + BeginTime + TStr + '</begin>';
               end;
               if (EndTime <> '') then begin
                  i := AnsiPos('T',EndTime);
                  if i > 0 then TStr := '' else TStr := JustAfterMidnight;
                  Result := Result + '<end>' + EndTime + TStr + '</end>';
               end;
               Result := Result + '</TimeSpan>';
            end;
         end;
      end;


   procedure CreateDocument(ItsName : shortString);
   var
      i,ThinFactor   : integer;
      tName : PathStr;
      theName,fName : PathStr;
      InTag : boolean;
      Ender : shortstring;
      Color : tColor;
      r,g,b : byte;
      StyleString : AnsiString;
      ColorStr : shortString;
      AllLines : tStringList;
     {$IfDef ExGeography}
     {$Else}
        kGraph : KoppenGr.TKoppenGraph;
        Bitmap : tMyBitmap;
     {$EndIf}

       procedure EncodePoint(i : integer);
       begin
          Lat := GISdb[DBonTable].aShapeFile.CurrentLineCoords^[i].Lat;
          Long := GISdb[DBonTable].aShapeFile.CurrentLineCoords^[i].Long;
          z := 0;
          if (GISdb[DBonTable].TheMapOwner <> Nil) and (GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap <> 0) then DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z);
          KML.EncodeLatLongZ(Lat, Long, z);
       end;

       procedure ProcessDataBase;
       var
          i,j : integer;

             procedure CopyIconGraphicsFile;
             var
                bmp : tMyBitmap;
             begin
                 theName := ExtractFilePath(GISdb[DBonTable].dbFullName) + IconName;
                 if not FileExists(theName) then theName := MainMapData + 'icons\' + IconName;
                 if FileExists(theName) then begin
                    bmp := PetImage.LoadBitmapFromFile(theName);
                    PetImage.SaveBitmap(bmp,KML.KMLOutputPath + IconName);
                    BMP.Free;
                    KML.KMLStringList.Add(' <Style id="I' + IntToStr(DBonTable) + '-0' + '"><IconStyle><scale>' +
                       RealToString(IconScaleFactor,-12,-2) + '</scale><Icon><href>' + IconName + '</href> </Icon></IconStyle></Style>');
                 end
                 else MessageToContinue('Icon file missing: ' + theName);
             end;

             procedure DoAnIcon(IconFName : PathStr);
             var
                bmp : tMyBitmap;
             begin
                if FileExists(IconFName) then begin
                   IconName := ExtractFileName(IconFName);
                   bmp := PetImage.LoadBitmapFromFile(IconFName);
                   PetImage.SaveBitmap(bmp,KML.KMLOutputPath + IconName);
                   BMP.Free;
                   KML.KMLStringList.Add('<Style id="I' + IntToStr(DBonTable) + '-1' + '"><IconStyle><scale>' + RealToString(IconScaleFactor,-12,-2) + '</scale><Icon><href>' + IconName +  '</href> </Icon></IconStyle></Style>');
                end
                else MessageToContinue('Icon file missing: ' + IconFName);
             end;

             function ScaleString(IconScaleFactor : float64) : shortstring;
             begin
                Result := '<scale>' + RealToString(IconScaleFactor,-12,-2) + '</scale>';
             end;


       begin
         {$IfDef KMLProblems} WriteLinetoDebugFile('ProcessDataBase in'); {$EndIf}
          with GISdb[DBonTable] do begin
             KML.AddName(ItsName);
             AllIcons := Nil;
             AllLines := Nil;
             EmpSource.Enabled := false;
             if LineShapeFile(ShapeFileType) and MyData.FieldExists('LINE_COLOR') then begin
                AllLines := MyData.UniqueEntriesInDB('LINE_COLOR');
                for i := 0 to pred(AllLines.Count) do begin
                   Color := StrToInt(AllLines.Strings[i]);
                   GetRGBfromTColor(Color,r,g,b);
                   ColorStr := '7f' + IntToHex(r,2)+ IntToHex(g,2)+ IntToHex(b,2);
                   KML.KMLStringList.Add(' <Style id="Line' + IntToStr(DBonTable) + '-' + IntToStr(i) + '">' + '<LineStyle>' + '<color>' + ColorStr + '</color>' + '<width>4</width>' + '</LineStyle>' + '</Style>');
                end;
             end
             else if (kml_opts_fm.ComboBox5.Text <> '') or (FileExists(kml_opts_fm.IconFName)) or (GISdb[DBonTable].dbOpts.DBAutoShow =  dbasIconAll) then begin
                {$IfDef KMLProblems} WriteLinetoDebugFile('Copy Icons'); {$EndIf}
                IconScaleFactor := 1.1;
                CheckEditString(kml_opts_fm.Edit7.Text,IconScaleFactor);
                if (GISdb[DBonTable].dbOpts.DBAutoShow =  dbasIconAll) then begin
                   DoAnIcon('c:\mapdata\icons\' + GISdb[DBonTable].dbOpts.AllIconFName);
                end
                else if (not PathIsValid(kml_opts_fm.IconFName)) and (FileExists(kml_opts_fm.IconFName)) then begin
                   DoAnIcon(kml_opts_fm.IconFName);
                end
                else begin
                   if SingleRecordOnly then begin
                      if MyData.FieldExists('ICON') then begin
                          IconName := MyData.GetFieldByNameAsString('ICON');
                          if MyData.FieldExists('ICON_SCALE') then begin
                            IconScaleFactor := MyData.GetFieldByNameAsFloat('ICON_SCALE');
                          end;
                          CopyIconGraphicsFile;
                          theName := ExtractFilePath(GISdb[DBonTable].dbFullName) + IconName;
                          if not FileExists(theName) then theName := MainMapData + 'icons\' + IconName;
                          if FileExists(theName) then begin
                             KML.KMLStringList.Add(' <Style id="I' + IntToStr(DBonTable) + '-0' + '"><IconStyle>' + ScaleString(IconScaleFactor) + '<Icon><href>' + IconName + '</href> </Icon></IconStyle></Style>');
                          end
                          else MessageToContinue('Icon file missing: ' + theName);
                       end;
                   end
                   else begin
                      AllIcons := MyData.UniqueEntriesInDB(kml_opts_fm.ComboBox5.Text);
                      GISdb[DBonTable].EmpSource.Enabled := false;
                      for i := 0 to pred(AllIcons.Count) do begin
                         IconName := AllIcons.Strings[i];
                         if MyData.FieldExists('ICON_SCALE') then begin
                            MyData.First;
                            while (MyData.GetFieldByNameAsString(kml_opts_fm.ComboBox5.Text) <> IconName) do MyData.Next;
                            IconScaleFactor := MyData.GetFieldByNameAsFloat('ICON_SCALE');
                         end;
                         CopyIconGraphicsFile;
                         KML.KMLStringList.Add(' <Style id="I' + IntToStr(DBonTable) + '-' + IntToStr(i) + '"><IconStyle>' + ScaleString(IconScaleFactor) + '<Icon><href>' + IconName + '</href> </Icon></IconStyle></Style>');
                      end;
                   end;
                end;
            end;

            if (kml_opts_fm.ComboBox6.Text <> '') then KML.KMLStringList.Add(TimeSpanString);
            KML.KMLStringList.Add('');
            CheckEditString(kml_opts_fm.Edit6.Text,MDDef.ThumbSize);
            NameField := kml_opts_fm.ComboBox4.Text;
            ThinFactor := StrToInt(kml_opts_fm.Edit8.Text);
            EmpSource.Enabled := false;
            if (not SingleRecordOnly) then begin
               StartProgress('Export DB');
               MyData.First;
               j := 0;
            end;
            {$IfDef KMLProblems} WriteLinetoDebugFile('Start record processing'); {$EndIf}
            while SingleRecordOnly or (not MyData.eof) do begin
               if (not SingleRecordOnly) then begin
                  inc(j);
                  UpdateProgressBar(j/MyData.FiltRecsInDB);
                  EmpSource.Enabled := false;
               end;
               InTag := false;
               Desc := '';

               if MDDef.KML_DB_tables or (kml_opts_fm.ComboBox1.Enabled and (kml_opts_fm.ComboBox1.Text <> '')) or (kml_opts_fm.ComboBox2.Enabled and (kml_opts_fm.ComboBox2.Text <> '')) or (kml_opts_fm.ComboBox8.Enabled and (kml_opts_fm.ComboBox8.Text <> '')) then begin
                  Desc := '<![CDATA[' + Desc;

                 {$IfDef ExGeography}
                 {$Else}
                     if KoppenPresent and MyData.FieldExists('PLACE') then begin
                         kGraph := OpenKoppenGraph(400,300);
                         tName := ThumbNailDir + MyData.GetFieldByNameAsString('PLACE') + OverlayFExt;
                         Petimage.SaveImageAsBMP(kGraph.Image1,tName);
                         kGraph.Close;
                         Desc := Desc + ' <br> '  + MenuStr + '<img border="0" src="' + ExtractFileName(tName) +  '"><br>';
                     end;
                  {$EndIf}

                  if (kml_opts_fm.ComboBox1.Enabled and (kml_opts_fm.ComboBox1.Text <> '')) and (not MDDef.KML_DB_tables) then begin
                     Desc := Desc + MyData.MakeImageTag(ThumbnailDir, kml_opts_fm.ComboBox1.Text);
                  end;

                  if (kml_opts_fm.ComboBox2.Enabled and (kml_opts_fm.ComboBox2.Text <> '')) then begin
                     fName := MyData.GetFieldByNameAsString(kml_opts_fm.ComboBox2.Text);
                     if ExtEquals(ExtractFilePath(fName),'.PDF') then begin
                        CopyFile(FName,KML.KMLOutputPath + fName);
                     end;
                     Desc := Desc + '<a href="' + fName + '"><br />' + kml_opts_fm.Edit4.Text  + '</a><br />';
                  end;

                 if kml_opts_fm.ComboBox8.Enabled and (kml_opts_fm.ComboBox8.Text <> '') then begin
                     fName := MyData.GetFieldByNameAsString(kml_opts_fm.ComboBox8.Text);
                     if not FileExists(fName) then fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + fName;
                     CleanUpFileName(fname);
                     if FileExists(fName) then begin
                        TStrl := TStringList.Create;
                        TStrl.LoadFromFile(fName);
                        if FileExtEquals(fName,'.HTM') or FileExtEquals(fName, '.HTML') then begin
                           ExtractMainBodyOfHTML(TStrl);
                           if MDDef.CleanUpHTML then CleanHTMLFile(TStrl);
                           ConvertImageLinksHTML(TStrl,ExtractFilePath(ExpandFileName(fName)),KML.KMLOutPutPath);
                           Ender := #13;
                        end
                        else Ender := '<br>';
                      //now do something with the file
                        InTag := false;
                        for I := 0 to pred(TStrl.Count) do begin
                           Desc := Desc + Ender + KML_Proof_String(TStrl.Strings[i],InTag);
                        end;
                        TStrl.Free;
                     end
                     else MessageToContinue('Missing HTML file: ' + fName);
                  end;

                  if MDDef.KML_DB_tables then begin
                     Desc := Desc + '<br />' + SingleRecordToHTMLTable(MyData,dbOpts.VisCols,ThumbNailDir);
                  end;
                  Desc := Desc +  ']]>';
               end;

               if (NameField <> '') then TStr := MyData.GetFieldByNameAsString(NameField)
               else TStr := '';
               if SimplePointFile then begin
                  if ValidLatLongFromTable(Lat,Long) then begin
                     Options := '';
                     if (GISdb[DBonTable].dbOpts.DBAutoShow = dbasIconAll) or (FileExists(kml_opts_fm.IconFName)) then begin
                        Options := Options + '<styleUrl>#I' + IntToStr(DBonTable) + '-1' + '</styleUrl>';
                     end
                     else if (kml_opts_fm.ComboBox5.Text <> '') then begin
                        if SingleRecordOnly then i := 0
                        else i := AllIcons.IndexOf(MyData.GetFieldByNameAsString(kml_opts_fm.ComboBox5.Text));
                        Options := '<styleUrl>#I' + IntToStr(DBonTable) + '-' + IntToStr(i) + '</styleUrl>';
                     end;
                     KML.AddPlaceMark(Lat,Long, TStr,Desc,'',0,Options);
                  end;
               end
               else if LineOrAreaShapeFile(ShapeFileType) then begin
                  if aShapeFile.GetLineCoords(MyData.RecNo) then begin
                     StyleString := '';
                     if (AllLines <> Nil) then begin
                        ColorStr := MyData.GetFieldByNameAsString('LINE_COLOR');
                        if (ColorStr <> '') then begin
                           i := AllLines.IndexOf(ColorStr);
                           StyleString := '<styleUrl>#Line' + IntToStr(DBonTable) + '-' + IntToStr(i) + '</styleUrl>';
                        end;
                     end;

                     if LineShapeFile(ShapeFileType) then KML.AddLineString(TStr,Desc,StyleString)
                     else KML.AddPolygonString(TStr,Desc,StyleString);

                     for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
                         EncodePoint(i);
                     end;
                     if AreaShapeFile(ShapeFileType) then EncodePoint(0);
                     if LineShapeFile(ShapeFileType) then KML.EndLineString
                     else KML.EndPolygonString;
                  end;
               end;
               KML.KMLStringList.Add('');
               if SingleRecordOnly then break;
               for i := 1 to ThinFactor do MyData.Next;
            end {while};
            {$IfDef KMLProblems} WriteLinetoDebugFile('Done record processing');   {$EndIf}
          end;
       if (not SingleRecordOnly) then EndProgress;

        if (AllIcons <> Nil) then AllIcons.Free;
        KML.KMLStringList.Add('');
        {$IfDef KMLProblems}   WriteLinetoDebugFile('ProcessDataBase out'); {$EndIf}
       end;


   begin
     {$IfDef KMLProblems} WriteLinetoDebugFile('CreateDocument for KML in'); {$EndIf}
     {$IfDef ExGeography}
     {$Else}
         if GISdb[DBonTable].KoppenPresent then begin
            Bitmap := KoppenLegend(false);
            tName := ThumbNailDir + 'kop_leg' + OverlayFExt;
            Petimage.SaveBitmap(Bitmap,tName);
            Bitmap.Free;
            KML.AddLogo(tName,true);
         end;
      {$EndIf}

      if (kml_opts_fm.ComboBox3.Text = '') then begin
         ProcessDataBase;
      end
      else begin
         AllFilters := GISdb[DBonTable].MyData.UniqueEntriesInDB(kml_opts_fm.ComboBox3.Text);
         for i := 0 to pred(AllFilters.Count) do begin
            wmDEM.SetPanelText(0, IntToStr(i) + '/' + IntToStr(AllFilters.Count));
            ItsName := kml_opts_fm.ComboBox3.Text + '=' + QuotedStr(AllFilters.Strings[i]);
            GISdb[DBonTable].MyData.ApplyFilter(ItsName);
            ItsName := ItsName + '  n=' + IntToStr(GISdb[DBonTable].MyData.RecordCount);
            KML.AddFolder(ItsName,'');
            ProcessDataBase;
            KML.EndFolder;
         end;
         AllFilters.Free;
         GISdb[DBonTable].ClearGISFilter;
         wmDEM.SetPanelText(0, '');
      end;
      {$IfDef KMLProblems} WriteLinetoDebugFile('CreateDocument out'); {$EndIf}
   end;


begin
   {$IfDef KMLProblems} WriteLinetoDebugFile('ConvertToKML enter,  db=' + IntToStr(DBOnTable) + '  ThumbNailDir=' + ThumbNailDir); {$EndIf}

   kml_opts_fm := Tkml_opts_fm.Create(Application);
   kml_opts_fm.IconFName := ExtractFilePath(GISdb[DBonTable].dbFullName);
   GISdb[DBonTable].EmpSource.Enabled := false;
   FieldsInDB := Nil;
   with GISdb[DBonTable] do begin
      kml_opts_fm.Edit1.Text := dbName;
      GetFieldsLinkPossible(MyData,LinkTable,dbOpts.VisCols,[ftString],FieldsInDB);
      if (FieldsInDB.Count > 0) then begin
         kml_opts_fm.ComboBox3.Items := FieldsInDB;
         kml_opts_fm.ComboBox4.Items := FieldsInDB;
         kml_opts_fm.ComboBox6.Items := FieldsInDB;
         kml_opts_fm.ComboBox7.Items := FieldsInDB;
         kml_opts_fm.ComboBox3.Text := SepExpField;
         if MDDef.KMLTimeAnimations then begin
            if MyData.FieldExists('START_DATE') then kml_opts_fm.ComboBox6.Text := 'START_DATE';
            if MyData.FieldExists('END_DATE') then kml_opts_fm.ComboBox7.Text := 'END_DATE';
         end;
         kml_opts_fm.ComboBox8.Items := FieldsInDB;
         if MDDef.KMLLabelExports then begin
            if MyData.FieldExists('NAME') then kml_opts_fm.ComboBox4.Text := 'NAME'
            else kml_opts_fm.ComboBox4.Text := FieldsInDB.Strings[0];
         end;
         if MyData.FieldExists('TEXT') then kml_opts_fm.ComboBox8.Text := 'TEXT';
      end;
      FieldsInDB.Free;
      kml_opts_fm.ComboBox1.Enabled := false;
      kml_opts_fm.Edit3.Enabled := false;
      kml_opts_fm.Label3.Enabled := false;
      kml_opts_fm.ComboBox2.Enabled := false;
      kml_opts_fm.Edit4.Enabled := false;
      kml_opts_fm.Label4.Enabled := false;

      for i := 1 to 5 do if (ImageFieldNames[i] <> '') then begin
         kml_opts_fm.ComboBox1.Items.Add(ImageFieldNames[i]);
         kml_opts_fm.ComboBox1.Enabled := true;
         kml_opts_fm.Edit3.Enabled := true;
         kml_opts_fm.Label3.Enabled := true;
         kml_opts_fm.ComboBox1.Text := ImageFieldNames[i];
      end;
      for i := 1 to 5 do if (WWWFieldNames[i] <> '') then begin
         kml_opts_fm.ComboBox2.Items.Add(WWWFieldNames[i]);
         kml_opts_fm.ComboBox2.Enabled := true;
         kml_opts_fm.Edit4.Enabled := true;
         kml_opts_fm.Label4.Enabled := true;
         kml_opts_fm.ComboBox2.Text := WWWFieldNames[i];
      end;
      for i := 1 to 5 do if (IconFieldNames[i] <> '') then begin
         kml_opts_fm.ComboBox5.Items.Add(IconFieldNames[i]);
         kml_opts_fm.ComboBox5.Enabled := true;
         kml_opts_fm.ComboBox5.Text := IconFieldNames[i];
      end;
   end;

   LocalKML := (KML = Nil);
   if LocalKML then kml_opts_fm.BitBtn2.Visible := false
   else begin
      kml_opts_fm.OKBtn.Visible := false;
   end;

   if not MDDef.KMLDefaultDB then kml_opts_fm.ShowModal;

   {$IfDef KMLProblems} WriteLinetoDebugFile('Options selected'); {$EndIf}

   GISdb[DBonTable].EmpSource.Enabled := false;
   FolderName := kml_opts_fm.Edit1.Text;
   FolderDesc := kml_opts_fm.Edit2.Text;

   if LocalKML then begin
      kml := tKMLCreator.Create('');
      if MDDef.KMLTopLevelFolder and (FolderName <> '') then Kml.AddFolder(FolderName,FolderDesc);
   end;
   if (ThumbNailDir = '') then ThumbnailDir := KML.KMLOutputPath;
   LastKMLDir :=  KML.KMLOutputPath;

   GISdb[DBonTable].EmpSource.Enabled := false;
   if (kml_opts_fm.ComboBox6.Text = '') or (not kml_opts_fm.CheckBox3.Checked) then begin  //no time animateion
      CreateDocument(GISdb[DBonTable].dbName);
   end
   else begin  //time animation
      FieldsInDB := GISdb[DBonTable].MyData.UniqueEntriesInDB(kml_opts_fm.ComboBox6.Text);
      for i := 0 to pred(FieldsInDB.Count) do begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].MyData.ApplyFilter( kml_opts_fm.ComboBox6.Text + '=' + QuotedStr(FieldsInDB.Strings[i]));
         if GISdb[DBonTable].MyData.FieldExists('DATE_LABEL') then
            TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('DATE_LABEL')
         else TStr := FieldsInDB.Strings[i];
         CreateDocument(TStr);
      end;
      FieldsInDB.Free;
      GISdb[DBonTable].MyData.ApplyFilter('');
   end;

  if LocalKML then begin
     if MDDef.KMLTopLevelFolder then KML.EndFolder;
     fName := kml.KMLOutputPath + GISdb[DBonTable].dbName + '.KML';
     {$IfDef KMLProblems}   WriteLinetoDebugFile('Start LocalKML Will save as=' + fName);   {$EndIf}
     kml.CloseAndSaveFile(true,fName);
     kml.Destroy;
  end;
  GISdb[DBonTable].EmpSource.Enabled := true;
  kml_opts_fm.Free;
  {$IfDef KMLProblems} WriteLinetoDebugFile('ConvertToKML out'); {$EndIf}
end;


procedure Tkml_opts_fm.BitBtn3Click(Sender: TObject);
begin
  if GetGraphicsFileName('KML icon',IconFName) then PetImage.LoadBitmapInImage(Image1,IconFName);
end;

procedure Tkml_opts_fm.CheckBox1Click(Sender: TObject);
begin
   MDDef.KML_DB_tables := CheckBox1.Checked;
end;


procedure Tkml_opts_fm.CheckBox3Click(Sender: TObject);
begin
   MDDef.KMLTimeAnimations := CheckBox3.Checked;
   ComboBox6.Enabled := MDDef.KMLTimeAnimations;
   ComboBox7.Enabled := MDDef.KMLTimeAnimations;
   Label9.Enabled := MDDef.KMLTimeAnimations;
end;

procedure Tkml_opts_fm.CheckBox4Click(Sender: TObject);
begin
   MDDef.KMLLabelExports := CheckBox4.Checked;
   if not MDDef.KMLLabelExports  then ComboBox4.Text := '';
end;

procedure Tkml_opts_fm.CheckBox5Click(Sender: TObject);
begin
   MDDef.KMLTopLevelFolder := CheckBox5.Checked;
end;

procedure Tkml_opts_fm.CheckBox7Click(Sender: TObject);
begin
   MDDef.ZipKMLfiles := CheckBox7.Checked;
end;

procedure Tkml_opts_fm.FormCreate(Sender: TObject);
begin
   RadioGroup1.ItemIndex := MDDef.KMLImageOpts;
   CheckBox1.Checked := MDDef.KML_DB_tables;
   CheckBox3.Checked := MDDef.KMLTimeAnimations;
   CheckBox4.Checked := MDDef.KMLLabelExports;
   CheckBox5.Checked := MDDef.KMLTopLevelFolder;
   CheckBox7.Checked := MDDef.ZipKMLfiles;
   Edit6.Text := IntToStr(MDDef.ThumbSize);
   Edit8.Text := IntToStr(MDDef.DB2KMKLThin);
   IconFName := '';
end;

procedure Tkml_opts_fm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\kml_export.htm');
end;


procedure Tkml_opts_fm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure Tkml_opts_fm.RadioGroup1Click(Sender: TObject);
begin
   MDDef.KMLImageOpts := RadioGroup1.ItemIndex;
end;


initialization
finalization
   {$IfDef KMLProblems} WriteLineToDebugFile('KMLProblems active in kml_opts'); {$EndIf}
   {$IfDef ShowKMLProofing} WriteLineToDebugFile('ShowKMLProofing active in kml_opts'); {$EndIf}
   {$IfDef RecorClosing} WriteLineToDebugFile('Closing kml_opts out'); {$EndIf}
end.
