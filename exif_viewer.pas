{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

no longer used


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordReadEXIF}
{$EndIf}

//--------------------------------------------------------------------------
// Based on Test program: from Gerry McGuire, September 3, 2001
//    - Second Beta Release - 0.9
//--------------------------------------------------------------------------

unit exif_viewer;




//between the focal length f and the field of view (FOV) of a rectilinear lens.
//The formula that it implements is FOV = 2 arctan (x / (2 f)),
//where x is the diagonal of the film. The FOV is measured across the frame's
//diagonal, and is therefore smaller across the horizonal dimension, and
//even smaller across the vertical dimension.

(*
short side (x)                    long side (y)
e = 12 / ( tan(FOV / 2) )         e = 18 / ( tan(FOV / 2) )
FOV = 2 * tan-1 ( x / (2 * f) )   FOV = 2 * tan-1 ( y / (2 * f) )
e = 12 / ( x / (2 * f) )          e = 18 / ( y / (2 * f) )
e = (24 * f) / x                  e = (36 * f) / y
And of course, if
(24 * f) / x   <>   (36 * f) / y
then you didn't scan your film in 3:2 aspect ratio
*)


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, ExtDlgs, Dialogs, StdCtrls,Math,
  Jpeg, dExif,
  dIPTC,Petmar_types,
  petmar, ExtCtrls, ComCtrls;

type
  TExif_Form = class(TForm)
    btnLoad: TButton;
    pdlg: TOpenPictureDialog;
    Memo1: TMemo;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    PageControl1: TPageControl;
    Image: TTabSheet;
    Metadata: TTabSheet;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure dumpSections;
    procedure Memo(s: Ansistring);
    procedure dumpEXIF;
    procedure dumpMSpecific;
    procedure dumpThumb;
    { Private declarations }
  public
    { Public declarations }
    ImgData   : TImgData;
    exifBuffer: AnsiString;
    Verbose:boolean;
  end;

var
   Exif_Form : TExif_Form;


procedure ViewEXIFMetadata(dir : PathStr);
function GetEXIFFieldOfView(fName : PathStr; var fl,fov : float) : boolean;
function GetEXIFLatLongDegree(fName : PathStr; var Lat,Long,Altitude,ImageDir,fl,fov,AspectRatio : float) : boolean;


implementation


{$R *.DFM}

uses
   DEMMagvar;


procedure ViewEXIFMetadata(dir : PathStr);
var
  Exif_Form : TExif_Form;
begin
  Exif_Form := TExif_Form.Create(Application);
  Exif_Form.pdlg.InitialDir := Dir;
  //Exif_Form.ShowModal;
  //Exif_Form.Free;
end;


function GetEXIFLatLongDegree(fName : PathStr; var Lat,Long,Altitude,ImageDir,fl,fov,AspectRatio : float) : boolean;
var
    ImgData : TImgData;
    item :TTagEntry;
    err : integer;
    TStr,TStr2 : AnsiString;
    Dec : float;
    LatSign,LongSign : integer;
begin
    LatSign := 1;
    LongSign := 1;
    Altitude := -9999;
    ImageDir := -9999;

    ImgData := TimgData.Create;
    ExifTrace := 0;
    ImgData.BuildList := GenAll;  // on by default anyway
    ImgData.ProcessFile(fName);
    if (ImgData.ExifObj <> Nil) then begin
       ImgData.ExifObj.ResetIterator;
       while ImgData.ExifObj.IterateFoundTags(AllExif, {GenericEXIF,} item) do begin
          {$IfDef RecordReadEXIF}
          WriteLineToDebugFile(item.Desc + '/' + Item.Data);
          {$EndIf}
          if item.Desc = 'GPSLatitude' then begin
             Val(item.Data,Lat,err);
          end;
          if item.Desc = 'GPSLongitude' then begin
             Val(item.Data,Long,err);
          end;
          if item.Desc = 'GPSLatitudeRef' then begin
             if item.Data = 'S' then LatSign := -1;
          end;
          if item.Desc = 'GPSLongitudeRef' then begin
             if item.Data = 'W' then LongSign := -1;
          end;
          if item.Desc = 'GPSAltitude' then begin
              TStr := item.Data;
              TStr := Petmar_types.BeforeSpecifiedCharacter(TStr,'/');
              Val(TStr,Altitude,err);
              Altitude := 0.1 * Altitude;
          end;
          if item.Desc = 'GPSImageDirection' then begin
              TStr := item.Data;
              TStr := Petmar_types.BeforeSpecifiedCharacter(TStr,'/');
              Val(TStr,ImageDir,err);
              ImageDir := 0.01 * ImageDir;
          end;
          if item.Desc = 'Focal Length (35mm)' then begin
             Val(item.Data,fl,err);
          end;
          if item.Desc = 'Dimensions' then begin
             TStr := Item.Data;
             TStr2 := Trim(Petmar_types.BeforeSpecifiedCharacter(TStr,'x',true,true));
          end;
       end;
       Lat := Lat * LatSign;
       Long:= Long * LongSign;
       AspectRatio := 1.0 * ImgData.ExifObj.Width / ImgData.ExifObj.Height;
       FOV := AspectRatio / 1.33333 * 1632.6 / Math.Power(fl,0.952);
       Dec := CurrentMagneticDeclination(Lat,Long);
       ImageDir := ImageDir + Dec;
       //focalPixels = focalMM * pixelDensity / 25.4
       //fov = 2 * atan( (H - 1) / (2 * focalPixels) )

       Result := (abs(Lat) < 90) and (abs(Long) < 180) and (abs(Lat) > 0.0001) and (abs(Long) > 0.0001);
    end
    else begin
       Result := false;
    end;
    ImgData.Destroy;
end;


function GetEXIFFieldOfView(fName : PathStr; var fl,fov : float) : boolean;
var
    ImgData : TImgData;
    item : TTagEntry;
    err : integer;
    AspectRatio : float;
    TStr,TStr2 : AnsiString;
begin
    ImgData := TimgData.Create;
    ExifTrace := 0;
    ImgData.BuildList := GenAll;  // on by default anyway
    ImgData.ProcessFile(fName);
    if (ImgData.ExifObj <> Nil) then begin
       ImgData.ExifObj.ResetIterator;
       while ImgData.ExifObj.IterateFoundTags(GenericEXIF ,item) do begin
          {$IfDef RecordReadEXIF}
          WriteLineToDebugFile(item.Desc + '/' + Item.Data);
          {$EndIf}
          if item.Desc = 'Focal Length (35mm)' then begin
             Val(item.Data,fl,err);
          end;
          if item.Desc = 'Dimensions' then begin
             TStr := Item.Data;
             TStr2 := Trim(Petmar_types.BeforeSpecifiedCharacter(TStr,'x',true,true));
          end;
       end;
       AspectRatio := 1.0 * ImgData.ExifObj.Width / ImgData.ExifObj.Height;
       FOV := AspectRatio / 1.33333 * 1632.6 / Math.Power(fl,0.952);
       Result := true;
    end
    else begin
       Result := false;
    end;
    ImgData.Destroy;
end;


procedure TExif_Form.btnLoadClick(Sender: TObject);
var
    i : integer;
    ts : tstringlist;
    //tmp : AnsiString;
    Lat,Long,Alt,Az,AspRatio,
    FocalLength,FOV : float;
    //err : integer;
begin
  if pdlg.Execute then begin
    {$IfDef RecordReadEXIF}
    WriteLineToDebugFile('TExif_Form.btnLoadClick ' + pdlg.FileName,true);
    {$EndIf}
    Memo1.Clear;
    Image1.Picture.LoadFromFile(pdlg.FileName);
    GetEXIFFieldOfView(pdlg.FileName,FocalLength,FOV);
    Memo('fl=' + RealToString(FocalLength,12,2));
    Memo('FOV=' + RealToString(FOV,12,2));
    Memo('');

    ImgData := TimgData.Create;
    Caption := 'Info for '+ pdlg.FileName;
    if verbose then ExifTrace := 1
    else ExifTrace := 0;

    ImgData.BuildList := GenAll;  // on by default anyway
    ImgData.ProcessFile(pdlg.FileName);

    {$IfDef RecordReadEXIF}
    WriteLineToDebugFile('TExif_Form.btnLoadClick process file done');
    Memo('TExif_Form.btnLoadClick process file done');
    {$EndIf}

    if Verbose then begin
       dumpSections;
       {$IfDef RecordReadEXIF}
       WriteLineToDebugFile('TExif_Form.btnLoadClick dump sections done (verbose only)');
       Memo('TExif_Form.btnLoadClick dump sections done (verbose only)');
       {$EndIf}
    end;

    dumpExif;

    {$IfDef RecordReadEXIF}
    Memo('');
    Memo('TExif_Form.btnLoadClick dumpExif done');
    WriteLineToDebugFile('TExif_Form.btnLoadClick dumpExif done');
    {$EndIf}

    if not ImgData.HasMetaData() then exit;

    if ImgData.HasEXIF and ImgData.ExifObj.msAvailable then begin
       dumpMSpecific;
       {$IfDef RecordReadEXIF}
       Memo('');
       Memo('TExif_Form.btnLoadClick dump mspecfic done');
       WriteLineToDebugFile('TExif_Form.btnLoadClick dump mspecfic done');
       {$EndIf}
    end;

    if ImgData.HasThumbnail then begin
      ImgData.ExifObj.ProcessThumbnail;
      dumpThumb;
    end
    else Memo('No Thumbnail');

    if (ImgData.commentSegment <> nil) then begin
      Memo(' ');
      Memo('Comment Segment Available');
      Memo(ImgData.GetCommentStr());
    end;

    if (ImgData.IPTCSegment <> nil) then begin
      ts := ImgData.IptcObj.ParseIPTCStrings(ImgData.IPTCSegment^.Data);
      if ts.Count > 0 then begin
        Memo(crlf+' IPTC Segment Available!' + crlf);
        for i := 0 to ts.Count-1 do begin
           Memo(ts.strings[i]);
        end;
      end;
      ts.Free;
    end;

    if ImgData.HasEXIF then try
      Memo(' ');
      Memo(' -- EXIF Summary ----- ');
      Memo(ImgData.ExifObj.toLongString());
    finally
    end;

    if GetEXIFLatLongDegree(pdlg.FileName,Lat,Long,Alt,Az,focallength,fov,AspRatio) and (abs(Lat) > 0.0001) and (abs(Long) > 0.0001) then begin
       Memo(' ');
       Memo('Lat: ' + RealToString(Lat,-18,-6));
       Memo('Long: ' + RealToString(Long,-18,-6));
       Memo('Elev: ' + RealToString(Alt,-18,-1));
       Memo('Azimuth: ' + RealToString(Az,-18,-6));
    end;


    ImgData.Destroy;
  end;
end;


procedure TExif_Form.Memo(s:AnsiString);
begin
  Memo1.Lines.Add(s);
end;


procedure TExif_Form.dumpSections;
var
   i : integer;
   sh : AnsiString;
begin
  Memo(' --------------------------- ');
  Memo('File = '+ImgData.Filename);
  Memo('Section count = '+inttostr(ImgData.SectionCnt));
  for i := 1 to ImgData.SectionCnt do begin
    sh := '    Section['+inttostr(i)+']';
    Memo(sh+'.type = $'+IntToHex(ImgData.Sections[i].dtype,2)
           +' - '+LookupType(ImgData.Sections[i].dtype) +' ('
           +IntToStr(ImgData.Sections[i].size)+')');
  end;
end;


procedure TExif_Form.CheckBox1Click(Sender: TObject);
begin
   Verbose := CheckBox1.Checked;
end;



procedure TExif_Form.dumpEXIF;
var
   item : TTagEntry;
begin
  Memo('-- EXIF-Data --');
  Memo('ErrStr = '+ImgData.ErrStr);
  if not ImgData.HasEXIF() then exit;
  If ImgData.MotorolaOrder then Memo('Motorola Byte Order')
  else Memo('Intel Byte Order');
  if Verbose then Memo1.Lines.Add(ImgData.ExifObj.TraceStr)
  else begin
    ImgData.ExifObj.ResetIterator;
    while ImgData.ExifObj.IterateFoundTags(GenericEXIF, item) do begin
       {$IfDef RecordReadEXIF}
       WriteLineToDebugFile(item.Desc + '/' + Item.Data);
       {$EndIf}
       Memo(item.Desc+DexifDelim+item.Data);
    end;
  end;
end;

procedure TExif_Form.dumpMSpecific;
var
   item : TTagEntry;
begin
  Memo(' ');
  Memo(' -- Maker Specific Data ---- ');
  // verbose data is only available in the trace strings
  if Verbose then
    Memo1.Lines.Add(ImgData.ExifObj.msTraceStr)
  else begin
    ImgData.ExifObj.ResetIterator;
    while ImgData.ExifObj.IterateFoundTags(CustomEXIF,item) do Memo(item.Desc+DexifDelim+item.Data);
  end;
end;

procedure TExif_Form.dumpThumb;
var
   item : TTagEntry;
begin
  Memo(' ');
  Memo(' -- Thumbnail Data ---- ');
  Memo(' Start = ' +inttostr(ImgData.ExifObj.ThumbStart) +
       '  Length = '+inttostr(ImgData.ExifObj.ThumbLength));
  // verbose data is only available in the trace strings
  if Verbose then Memo1.Lines.Add(ImgData.ExifObj.ThumbTrace)
  else begin
    ImgData.ExifObj.ResetThumbIterator;
    while ImgData.ExifObj.IterateFoundThumbTags(GenericEXIF,item) do begin
       Memo(item.Desc+DexifDelim+item.Data);
    end;
  end;
end;


procedure TExif_Form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Close;
end;


procedure TExif_Form.FormCreate(Sender: TObject);
begin
  Verbose := false;
end;

initialization
finalization
    {$IfDef RecordReadEXIF}
    WriteLineToDebugFile('RecordReadEXIF active in exif_viewer');
    {$EndIf}
end.
