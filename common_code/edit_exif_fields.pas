unit edit_exif_fields;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Petmar_types,PETMAR;

type
  tAddToEXIFform = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    StartLat,StartLong : float64;
    PhotoDir : PathStr;
  end;


procedure AddEXIFfields(PhotoDir : PathStr = '');

implementation

{$R *.dfm}

uses
   GetLatLn,BaseMap, PetMath,
   ccr.exif;


procedure AddEXIFfields(PhotoDir : PathStr = '');
(*
var
   TheAuthor,TheSubject : shortstring;
   FilesWanted : tStringList;
   DefaultFilter : byte;
   f : integer;
   LatDeg,LatMin,LongDeg,LongMin :LongWord;
   LatSec,LongSec : Currency;

      procedure ProcessFile(FileName : PathStr);
      var
        ExifData: TExifData;
      begin
        ExifData := TExifData.Create;
        try
          ExifData.LoadFromGraphic(FileName);
          if (TheAuthor <> '') then ExifData.Author := TheAuthor;
          if (TheSubject <> '') then ExifData.Subject := TheSubject;
          //ExifData.SetKeyWords(['tennis', 'Wimbledon', 'match', 'SW19']);
          ExifData.GPSLatitude.Assign(LatDeg,LatMin,LatSec, ltNorth);
          ExifData.GPSLongitude.Assign(LongDeg,LongMin,LongSec, lnWest);
          ExifData.SaveToGraphic(FileName);
        finally
          ExifData.Free;
        end;
      end;

begin
   TheAuthor := 'Peter L. Guth DS69';
   TheSubject := 'DS Reunion 2024';

   LatDeg := 37;
   LatMin := 22;
   LatSec := 26.5;
   LongDeg := 117;
   LongMin := 58;
   LongSec := 47.8;

   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(PhotoDir);
   if GetMultipleFiles('Add Exif metadata','AnyFile|*.*',FilesWanted, DefaultFilter) then begin
       for f := 0 to pred(FilesWanted.Count) do begin
          ProcessFile(FilesWanted.Strings[f]);
       end;
   end;
   FilesWanted.Destroy;
*)

var
   AddToEXIFform : tAddToEXIFform;
begin
   AddToEXIFform := tAddToEXIFform.Create(Application);
   AddToEXIFform.StartLat := 0;
   AddToEXIFform.StartLong := 0;
   AddToEXIFform.PhotoDir := PhotoDir;
end;



(*
var
   TheAuthor,TheSubject : shortstring;
   FilesWanted : tStringList;
   DefaultFilter : byte;
   f : integer;
   LatDeg,LatMin,LongDeg,LongMin :LongWord;
   LatSec,LongSec : Currency;


begin
   TheAuthor := 'Peter L. Guth DS69';
   TheSubject := 'DS Reunion 2024';

   LatDeg := 37;
   LatMin := 22;
   LatSec := 26.5;
   LongDeg := 117;
   LongMin := 58;
   LongSec := 47.8;

*)


procedure tAddToEXIFform.BitBtn1Click(Sender: TObject);
begin
   GetLatLn.GetLatLongDefault(WGS84DatumConstants,'Photo locations',StartLat,StartLong);
   Label4.Caption := LatLongDegreeToString(StartLat,StartLong,DecSeconds);
end;

procedure tAddToEXIFform.BitBtn2Click(Sender: TObject);
var
   TheAuthor,TheSubject,TheComments : shortstring;
   FilesWanted : tStringList;
   DefaultFilter : byte;
   f : integer;
   LatDeg,LatMin,LongDeg,LongMin : LongWord;
   Sec : float64;
   Deg,Min : integer;
   LatSec,LongSec : Currency;


      procedure ProcessFile(FileName : PathStr);
      var
        ExifData: TExifData;
      begin
        ExifData := TExifData.Create;
        try
          ExifData.LoadFromGraphic(FileName);
          if (TheAuthor <> '') then ExifData.Author := TheAuthor;
          if (TheSubject <> '') then ExifData.Subject := TheSubject;
          if (TheComments <> '') then ExifData.Comments := TheComments;
          //ExifData.SetKeyWords(['tennis', 'Wimbledon', 'match', 'SW19']);
          ExifData.GPSLatitude.Assign(LatDeg,LatMin,LatSec, ltNorth);
          ExifData.GPSLongitude.Assign(LongDeg,LongMin,LongSec, lnWest);
          ExifData.SaveToGraphic(FileName);
        finally
          ExifData.Free;
        end;
      end;

begin
   TheAuthor := Edit1.Text;  // := 'Peter L. Guth DS69';
   TheSubject := Edit2.Text;
   TheComments := Edit3.Text;
   DegreesToDegMinSec(StartLat,Deg,Min,Sec);
   LatSec := Sec;
   LatMin := Min;
   LatDeg := Deg;

   DegreesToDegMinSec(StartLong,Deg,Min,Sec);
   LongSec := Sec;
   LongMin := Min;
   LongDeg := Deg;

   FilesWanted := tStringList.Create;
   FilesWanted.Add(PhotoDir);
   DefaultFilter := 1;
   if GetMultipleFiles('Add Exif metadata','AnyFile|*.*',FilesWanted, DefaultFilter) then begin
       for f := 0 to pred(FilesWanted.Count) do begin
          ProcessFile(FilesWanted.Strings[f]);
       end;
   end;
   FilesWanted.Destroy;
end;

end.
