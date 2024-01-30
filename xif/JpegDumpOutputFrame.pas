{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing image metadata               }
{ Version 1.5.1                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is JpegDumpOutputFrame.pas.                                        }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2011 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}


{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Modified as Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch             }
{ Released under the MIT Licences             }
{ Copyright (c) 2024 Peter L. Guth            }
{_____________________________________________}


unit JpegDumpOutputFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,StrUtils,
  CCR.Exif, CCR.Exif.BaseUtils, CCR.Exif.IPTC, CCR.Exif.TagIDs, CCR.Exif.XMPUtils,
  Petmar_types;

type
  TNewOutputFrame = class(TFrame)
    Memo: TMemo;
    grpExifThumbnail: TGroupBox;
    imgExifThumbnail: TImage;
  private
    procedure AddBlankLine;
    procedure AddLine(const S: string; const Args: array of const);
    procedure DoDefault(const Segment: IFoundJPEGSegment;const BlockName: string; LeaveBlankLine: Boolean = True; const Comment: string = '');
    procedure LoadAdobeApp13(const Segment: IFoundJPEGSegment);
    procedure LoadExif(const Segment: IFoundJPEGSegment);
    procedure LoadJFIF(const Segment: IFoundJPEGSegment);
    procedure LoadSOF(const Segment: IFoundJPEGSegment);
    procedure LoadSOS(const Segment: IFoundJPEGSegment);
    procedure LoadXMP(const Segment: IFoundJPEGSegment);
  public
    PhotoLat,PhotoLong,Speed : extended;
    Orient : integer;
    Verbose : boolean;
    Exposure,fStop, Focal35,Shutter,ISO,CameraName,
    DateTime,GPSDateStamp,PhotoAzimuth,PhotoAltitude,FocalLength : shortstring;
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const JPEGFile: string);
  end;

implementation

{$R *.dfm}


const
  SEndianness: array[TEndianness] of string = ('Small endian', 'Big endian');
  SectionNames: array[TExifSectionKind] of string = ('Main IFD', 'Exif sub-IFD','Interop sub-IFD', 'GPS sub-IFD', 'Thumbnail IFD', 'Maker note');
  SDataType: array[TExifDataType] of string = ('Byte', 'ASCII string', 'Word','LongWord', 'Fraction', 'ShortInt','Undefined', 'SmallInt', 'LongInt', 'Signed fraction', 'Single', 'Double', 'SubDirOffset');
  SDataOffsetsType: array[TExifDataOffsetsType] of string = ('From start of Exif data','From MakerNote start', 'From MakerNote IFD start', 'Not standard IFD format');


function TagIDToStr(const Tag: TExifTag): string;
begin
  if Tag.IsPadding then begin
    Result := 'WindowsPadding';
    Exit;
  end;
  FmtStr(Result, '$%.4x', [Tag.ID]);
  case Tag.Section.Kind of
    esGeneral, esThumbnail:
      case Tag.ID of
        ttImageDescription          : Result := 'ImageDescription';
        ttMake                      : Result := 'Make';
        ttModel                     : Result := 'Model';
        ttOrientation               : Result := 'Orientation';
        ttXResolution               : Result := 'XResolution';
        ttYResolution               : Result := 'YResolution';
        ttResolutionUnit            : Result := 'ResolutionUnit';
        ttSoftware                  : Result := 'Software';
        ttDateTime                  : Result := 'DateTime';
        ttArtist                    : Result := 'Artist';
        ttWhitePoint                : Result := 'WhitePoint';
        ttPrimaryChromaticities     : Result := 'PrimaryChromaticities';
        ttYCbCrCoefficients         : Result := 'YCbCrCoefficients';
        ttYCbCrPositioning          : Result := 'YCbCrPositioning';
        ttReferenceBlackWhite       : Result := 'ReferenceBlackWhite';
        ttCopyright                 : Result := 'Copyright';
        ttExifOffset                : Result := 'ExifOffset';
        ttGPSOffset                 : Result := 'GPSOffset';
        ttPrintIM                   : Result := 'PrintIM';
        ttWindowsTitle              : Result := 'WindowsTitle';
        ttWindowsComments           : Result := 'WindowsComments';
        ttWindowsAuthor             : Result := 'WindowsAuthor';
        ttWindowsKeywords           : Result := 'WindowsKeywords';
        ttWindowsSubject            : Result := 'WindowsSubject';
        ttWindowsRating             : Result := 'WindowsRating';
      else
        if Tag.Section.Kind = esThumbnail then
          case Tag.ID of
            ttImageWidth                : Result := 'ImageWidth';
            ttImageHeight               : Result := 'ImageHeight';
            ttBitsPerSample             : Result := 'BitsPerSample';
            ttCompression               : Result := 'Compression';
            ttPhotometricInterp         : Result := 'PhotometricInterp';
            ttStripOffsets              : Result := 'StripOffsets';
            ttSamplesPerPixel           : Result := 'SamplesPerPixel';
            ttRowsPerStrip              : Result := 'RowsPerStrip';
            ttStripByteCounts           : Result := 'StripByteCounts';
            ttPlanarConfiguration       : Result := 'PlanarConfiguration';
            ttJpegIFOffset              : Result := 'JpegIFOffset';
            ttJpegIFByteCount           : Result := 'JpegIFByteCount';
          end;
      end;
    esDetails:
      case Tag.ID of
        ttExposureTime              : Result := 'ExposureTime';
        ttFNumber                   : Result := 'FNumber';
        ttExposureProgram           : Result := 'ExposureProgram';
        ttSpectralSensitivity       : Result := 'SpectralSensitivity';
        ttISOSpeedRatings           : Result := 'ISOSpeedRatings';
        ttExifVersion               : Result := 'ExifVersion';
        ttDateTimeOriginal          : Result := 'DateTimeOriginal';
        ttDateTimeDigitized         : Result := 'DateTimeDigitized';
        ttComponentsConfiguration   : Result := 'ComponentsConfiguration';
        ttCompressedBitsPerPixel    : Result := 'CompressedBitsPerPixel';
        ttShutterSpeedValue         : Result := 'ShutterSpeedValue';
        ttApertureValue             : Result := 'ApertureValue';
        ttBrightnessValue           : Result := 'BrightnessValue';
        ttExposureBiasValue         : Result := 'ExposureBiasValue';
        ttMaxApertureValue          : Result := 'MaxApertureValue';
        ttSubjectDistance           : Result := 'SubjectDistance';
        ttMeteringMode              : Result := 'MeteringMode';
        ttLightSource               : Result := 'LightSource';
        ttFlash                     : Result := 'Flash';
        ttFocalLength               : Result := 'FocalLength';
        ttMakerNote                 : Result := 'MakerNote';
        ttUserComment               : Result := 'UserComment';
        ttSubsecTime                : Result := 'SubsecTime';
        ttSubsecTimeOriginal        : Result := 'SubsecTimeOriginal';
        ttSubsecTimeDigitized       : Result := 'SubsecTimeDigitized';
        ttFlashPixVersion           : Result := 'FlashPixVersion';
        ttColorSpace                : Result := 'ColorSpace';
        ttExifImageWidth            : Result := 'ExifImageWidth';
        ttExifImageHeight           : Result := 'ExifImageHeight';
        ttRelatedSoundFile          : Result := 'RelatedSoundFile';
        ttInteropOffset             : Result := 'InteropOffset';
        ttFlashEnergy               : Result := 'FlashEnergy';
        ttSpatialFrequencyResponse  : Result := 'SpatialFrequencyResponse';
        ttFocalPlaneXResolution     : Result := 'FocalPlaneXResolution';
        ttFocalPlaneYResolution     : Result := 'FocalPlaneYResolution';
        ttFocalPlaneResolutionUnit  : Result := 'FocalPlaneResolutionUnit';
        ttSubjectLocation           : Result := 'SubjectLocation';
        ttExposureIndex             : Result := 'ExposureIndex';
        ttSensingMethod             : Result := 'SensingMethod';
        ttFileSource                : Result := 'FileSource';
        ttSceneType                 : Result := 'SceneType';
        ttCFAPattern                : Result := 'CFAPattern';
        ttCustomRendered            : Result := 'CustomRendered';
        ttExposureMode              : Result := 'ExposureMode';
        ttWhiteBalance              : Result := 'WhiteBalance';
        ttDigitalZoomRatio          : Result := 'DigitalZoomRatio';
        ttFocalLengthIn35mmFilm     : Result := 'FocalLengthIn35mmFilm';
        ttSceneCaptureType          : Result := 'SceneCaptureType';
        ttGainControl               : Result := 'GainControl';
        ttContrast                  : Result := 'Contrast';
        ttSaturation                : Result := 'Saturation';
        ttSharpness                 : Result := 'Sharpness';
        ttDeviceSettingDescription  : Result := 'DeviceSettingDescription';
        ttSubjectDistanceRange      : Result := 'SubjectDistanceRange';
        ttImageUniqueID             : Result := 'ImageUniqueID';
        ttOffsetSchema              : Result := 'OffsetSchema';
        ttCameraOwnerName           : Result := 'CameraOwnerName';
        ttBodySerialNumber          : Result := 'BodySerialNumber';
        ttLensSpecification         : Result := 'LensSpecification';
        ttLensMake                  : Result := 'LensMake';
        ttLensModel                 : Result := 'LensModel';
        ttLensSerialNumber          : Result := 'LensSerialNumber';
      end;
    esInterop:
      case Tag.ID of
        ttInteropIndex              : Result := 'InteropIndex';
        ttInteropVersion            : Result := 'InteropVersion';
        ttRelatedImageFileFormat    : Result := 'RelatedImageFileFormat';
        ttRelatedImageWidth         : Result := 'RelatedImageWidth';
        ttRelatedImageLength        : Result := 'RelatedImageLength';
      end;
    esGPS:
      case Tag.ID of
        ttGPSVersionID              : Result := 'GPSVersionID';
        ttGPSLatitudeRef            : Result := 'GPSLatitudeRef';
        ttGPSLatitude               : Result := 'GPSLatitude';
        ttGPSLongitudeRef           : Result := 'GPSLongitudeRef';
        ttGPSLongitude              : Result := 'GPSLongitude';
        ttGPSAltitudeRef            : Result := 'GPSAltitudeRef';
        ttGPSAltitude               : Result := 'GPSAltitude';
        ttGPSTimeStamp              : Result := 'GPSTimeStamp';
        ttGPSSatellites             : Result := 'GPSSatellites';
        ttGPSStatus                 : Result := 'GPSStatus';
        ttGPSMeasureMode            : Result := 'GPSMeasureMode';
        ttGPSDOP                    : Result := 'GPSDOP';
        ttGPSSpeedRef               : Result := 'GPSSpeedRef';
        ttGPSSpeed                  : Result := 'GPSSpeed';
        ttGPSTrackRef               : Result := 'GPSTrackRef';
        ttGPSTrack                  : Result := 'GPSTrack';
        ttGPSImgDirectionRef        : Result := 'GPSImgDirectionRef';
        ttGPSImgDirection           : Result := 'GPSImgDirection';
        ttGPSMapDatum               : Result := 'GPSMapDatum';
        ttGPSDestLatitudeRef        : Result := 'GPSDestLatitudeRef';
        ttGPSDestLatitude           : Result := 'GPSDestLatitude';
        ttGPSDestLongitudeRef       : Result := 'GPSDestLongitudeRef';
        ttGPSDestLongitude          : Result := 'GPSDestLongitude';
        ttGPSDestBearingRef         : Result := 'GPSDestBearingRef';
        ttGPSDestBearing            : Result := 'GPSDestBearing';
        ttGPSDestDistanceRef        : Result := 'GPSDestDistanceRef';
        ttGPSDestDistance           : Result := 'GPSDestDistance';
        ttGPSProcessingMethod       : Result := 'GPSProcessingMethod';
        ttGPSAreaInformation        : Result := 'GPSAreaInformation';
        ttGPSDateStamp              : Result := 'GPSDateStamp';
        ttGPSDifferential           : Result := 'GPSDifferential';
      end;
  end;
end;


{ tNewOutputFrame }


constructor tNewOutputFrame.Create(AOwner: TComponent);
begin
  inherited;
  Memo.DoubleBuffered := True;
  Verbose := false;
end;

procedure tNewOutputFrame.AddBlankLine;
begin
  Memo.Lines.Add(' ');
end;

procedure tNewOutputFrame.AddLine(const S: string; const Args: array of const);
begin
  Memo.Lines.Add(Format(S, Args))
end;

procedure tNewOutputFrame.DoDefault(const Segment: IFoundJPEGSegment; const BlockName: string; LeaveBlankLine: Boolean = True; const Comment: string = '');
var
  S: string;
begin
  S := '--- ' + BlockName;
  if Comment <> '' then S := S + ' (' + Comment + ') ';
  S := S + ' ---';
  Memo.Lines.Add(S);
  AddLine('Segment offset'#9'$%.4x', [Segment.Offset]);
  if (Segment.MarkerNum in TJPEGSegment.MarkersWithNoData) and (Segment.Data.Size = 0) then
    Memo.Lines.Add('Segment has no data')
  else begin
    AddLine('Total size of segment'#9'%d bytes', [Segment.TotalSize]);
    if LeaveBlankLine and (Segment.Data.Size <= 128) then AddLine('Data'#9'%s', [BinToHexStr(Segment.Data)]);
  end;
  if LeaveBlankLine then AddBlankLine;
end;

procedure tNewOutputFrame.LoadAdobeApp13(const Segment: IFoundJPEGSegment);
const
  SYesNo: array[Boolean] of string = ('No', 'Yes');
var
  Block: IAdobeResBlock;
  Counter: Integer;
  FoundIPTC: Boolean;
  IPTCData: TIPTCData;
  S: string;
  Section: TIPTCSection;
  Tag: TIPTCTag;
begin
  DoDefault(Segment, 'Adobe Photoshop (APP13)', False);
  Counter := 0;
  FoundIPTC := False;
  for Block in Segment do begin
    Inc(Counter);
    FoundIPTC := FoundIPTC or Block.IsIPTCBlock;
  end;
  AddLine('Contains IPTC data'#9'%s', [SYesNo[FoundIPTC]]);
  AddLine('Number of data blocks'#9'%d', [Counter]);
  AddBlankLine;
  IPTCData := TIPTCData.Create;
  try
    Counter := 0;
    for Block in Segment do begin
      Inc(Counter);
      if Block.IsIPTCBlock then AddLine('Data block %d (IPTC):', [Counter])
      else AddLine('Data block %d:', [Counter]);
      AddLine('Signature'#9'%s', [string(Block.Signature)]);
      AddLine('Type ID'#9'$%.4x', [Block.TypeID]);
      if Block.Name <> '' then S := string(Block.Name)
      else S := '(none)';
      AddLine('Name'#9'%s', [S]);
      AddLine('Data size'#9'%d bytes', [Block.Data.Size]);
      if Block.IsIPTCBlock then begin
        IPTCData.LoadFromStream(Block.Data);
        for Section in IPTCData do
          for Tag in Section do
            AddLine('%d:%d'#9'%s', [Tag.Section.ID, Tag.ID, Tag.AsString]);
      end
      else if Block.Data.Size <= 128 then AddLine('Data'#9'%s', [BinToHexStr(Block.Data.Memory, Block.Data.Size)]);
      AddBlankLine;
    end;
  finally
    IPTCData.Free;
  end;
end;


function DecodePossibleFractionFloat(TheStr : ANSIstring) : float32;
var
   Part1,Part2 : float32;
begin
   if (TheStr <> '') then begin
      try
         if StrUtils.AnsiContainsText(TheStr,'/') then begin
            try
               Part1 := StrToFloat(petmar_types.BeforeSpecifiedCharacterANSI(TheStr,'/',true,true));
               Part2 := StrToFloat(TheStr);
               Result := Part1/Part2;
            except
                on Exception do Result := 0;
            end;
         end
         else begin
            Result := StrToFloat(TheStr);
         end;
      except
         on Exception do Result := 0;
      end;
   end
   else Result := 0;
end;


function DecodePossibleFloat(TheStr : ANSIstring) : shortstring;
begin
   if (TheStr <> '') then begin
      Result := RealToString(DecodePossibleFractionFloat(TheStr),-12,-2);
   end
   else Result := '';
end;


 function ParseStringToDegrees(TheStr : ANSIstring) : float32;
 begin
     Result := DecodePossibleFractionFloat(petmar_types.BeforeSpecifiedCharacterANSI(TheStr,',',true,true));
     Result := Result + DecodePossibleFractionFloat(petmar_types.BeforeSpecifiedCharacterANSI(TheStr,',',true,true)) / 60;
     Result := Result + DecodePossibleFractionFloat(TheStr) / 3600;
 end;


procedure tNewOutputFrame.LoadExif(const Segment: IFoundJPEGSegment);
var
  ExifData: TExifData;
  S: ANSIstring;
  Section: TExifSection;
  Tag: TExifTag;
  GPSImgDirection,
  CameraFocalLength,
  LatStr,LongStr : ANSIString;
  First,
  LatMult,LongMult : integer;
begin
  DoDefault(Segment, 'Exif', False);
  LatStr := '';
  LongStr := '';
  LatMult := 1;
  LongMult := 1;

  ExifData := TExifData.Create;
  GPSDateStamp := '';
  GPSImgDirection := '';
  CameraFocalLength := '';
  PhotoAltitude := '';
  Exposure := '';
  Fstop := '';
  Shutter := '';
  ISO := '';
  Focal35 := '';
  CameraName := '';

  try
    ExifData.LoadFromStream(Segment.Data);
    Memo.Lines.Add('Byte order of main structure'#9'' + SEndianness[ExifData.Endianness]);
    Memo.Lines.Add('');
    for Section in ExifData do
      if Section.Count > 0 then begin
        AddLine('%s (%d tags):', [SectionNames[Section.Kind], Section.Count]);
        if leBadOffset in Section.LoadErrors then Memo.Lines.Add('*** Bad offset ***');
        if leBadTagCount in Section.LoadErrors then Memo.Lines.Add('*** Claims to contain more tags than could be parsed ***');
        if leBadTagHeader in Section.LoadErrors then Memo.Lines.Add('*** Contains one or more malformed tag headers ***');
        if Section.Kind = esMakerNote then
          if ExifData.MakerNote is TUnrecognizedMakerNote then
            Memo.Lines.Add('Unrecognised type - couldn''t parse tag structure')
          else begin
            S := Copy(ExifData.MakerNote.ClassName, 2, MaxInt);
            Memo.Lines.Add('Type'#9'' + Copy(S, 1, Length(S) - 9));
            Memo.Lines.Add('Byte order'#9'' + SEndianness[ExifData.MakerNote.Endianness]);
            Memo.Lines.Add('Data offsets'#9'' + SDataOffsetsType[ExifData.MakerNote.DataOffsetsType]);
          end;
        for Tag in Section do begin
          if not Tag.WellFormed then S := '*** Malformed tag header ***'
          else if (Tag.ID = ttMakerNote) and (Section.Kind = esDetails) and (ExifData.MakerNote.Tags.Count <> 0) then S := '[Recognised - see below]'
          else S := Tag.AsString;

          if (TagIDToStr(Tag) <> 'MakerNote') then begin
             if Verbose or (Tag.ID = ttExposureTime) or (Tag.ID = ttFNumber) or (Tag.ID = ttISOSpeedRatings) or (Tag.ID = ttShutterSpeedValue) or (Tag.ID = ttFocalLength) or
              (Tag.ID = ttExifImageWidth) or (Tag.ID = ttExifImageHeight) or (Tag.ID = ttFocalLengthIn35mmFilm) or (Tag.ID = ttGPSLatitude) or (Tag.ID = ttGPSLongitude) or
              (Tag.ID = ttGPSAltitude) or (Tag.ID = ttGPSImgDirection)  or (Tag.ID = ttMake)  or (Tag.ID = ttModel) then begin
                 AddLine('%s'#9'%s (%d)'#9'%s', [TagIDToStr(Tag), SDataType[Tag.DataType],Tag.ElementCount, S]);
             end;
          end;

          if TagIDToStr(Tag) = 'GPSLatitude' then LatStr := s;
          if TagIDToStr(Tag) = 'GPSLongitude' then LongStr := s;
          if TagIDToStr(Tag) = 'GPSLatitudeRef' then if s = 'S' then LatMult := -1;
          if TagIDToStr(Tag) = 'GPSLongitudeRef' then if s = 'W' then LongMult := -1;
          if TagIDToStr(Tag) = 'GPSDateStamp' then GPSDateStamp := s;
          if TagIDToStr(Tag) = 'GPSImgDirection' then PhotoAzimuth := DecodePossibleFloat(s);
          if TagIDToStr(Tag) = 'GPSAltitude' then PhotoAltitude := DecodePossibleFloat(s);
          if TagIDToStr(Tag) = 'FocalLength' then FocalLength := DecodePossibleFloat(s);
          if TagIDToStr(Tag) = 'DateTime' then DateTime := s;
          if TagIDToStr(Tag) = 'Orientation' then Orient := strToInt(s);
          if TagIDToStr(Tag) = 'ExposureTime' then Exposure := 'Exp=' + s;
          //if TagIDToStr(Tag) = 'FNumber' then Fstop := 'f-stop=' + s;

           if (Tag.ID = ttExposureTime) then begin
              First := StrToInt(Petmar_types.BeforeSpecifiedCharacterANSI(s,'/',true,true));
              Shutter := '1/' + IntToStr(StrToint(s) div First);
              Speed := 1 / (StrToint(s) / First);
           end;
           if Tag.ID = ttFNumber then fstop := DecodePossibleFloat(s);
           if Tag.ID = ttISOSpeedRatings then ISO := s;
           if Tag.ID = ttFocalLengthIn35mmFilm then Focal35 := DecodePossibleFloat(s);
           if Tag.ID = ttModel then CameraName := s;
        end;
        AddBlankLine;
      end;

   if {false and} (LatStr <> '') and (LongStr <> '') and (LatStr[1] in ['0'..'9','-']) and (LongStr[1] in ['0'..'9','-']) then begin
      PhotoLat := LatMult * ParseStringToDegrees(LatStr);
      PhotoLong := LongMult * ParseStringToDegrees(LongStr);
   end
   else begin
      PhotoLat := -999;
      PhotoLong := -999;
   end;

    if not ExifData.Thumbnail.Empty then begin
      imgExifThumbnail.Picture.Assign(ExifData.Thumbnail);
      grpExifThumbnail.Height := (grpExifThumbnail.Height - imgExifThumbnail.Height) + ExifData.Thumbnail.Height;
    end;
  finally
    ExifData.Free;
  end;
end;

procedure tNewOutputFrame.LoadJFIF(const Segment: IFoundJPEGSegment);
var
  JFIFHeader: TJFIFData;
begin
  Segment.Data.ReadBuffer(JFIFHeader, SizeOf(JFIFHeader));
  DoDefault(Segment, 'JFIF header', False);
  AddLine('Ident'#9'' + JFIFHeader.Ident, []);
  AddLine('Version'#9'%d.%d', [JFIFHeader.MajorVersion, JFIFHeader.MinorVersion]);
  AddLine('Density units'#9'%d', [Ord(JFIFHeader.DensityUnits)]);
  AddLine('Horz density'#9'%d',  [MakeWord(JFIFHeader.HorzDensityLo, JFIFHeader.HorzDensityHi)]);
  AddLine('Vert density'#9'%d',  [MakeWord(JFIFHeader.VertDensityLo, JFIFHeader.VertDensityHi)]);
  { The JFIF thumbnail is not the same thing as the Exif thumbnail, so don't be surprised at the next two values being 0. }
  if (JFIFHeader.ThumbnailWidth <> 0) or (JFIFHeader.ThumbnailHeight <> 0) then begin
    AddLine('Thumbnail width'#9'%d', [JFIFHeader.ThumbnailWidth]);
    AddLine('Thumbnail height'#9'%d', [JFIFHeader.ThumbnailHeight]);
  end;
  AddBlankLine;
end;

procedure tNewOutputFrame.LoadSOF(const Segment: IFoundJPEGSegment);
var
  SOFData: PJPEGStartOfFrameData;
  Str: string;
begin
  case Segment.MarkerNum of
       jmStartOfFrame0: Str := '(baseline DCT)';
       jmStartOfFrame1: Str := '(extended sequential DCT)';
       jmStartOfFrame2: Str := '(progressive DCT)';
       jmStartOfFrame3: Str := '(lossless sequential)';
       jmStartOfFrame5: Str := '(differential sequential DCT)';
       jmStartOfFrame6: Str := '(differential progressive DCT)';
       jmStartOfFrame7: Str := '(differential lossless sequential)';
     else Str := IntToStr(Segment.MarkerNum - jmStartOfFrame0);
  end;
  DoDefault(Segment, 'Start of frame ' + Str, False);
  SOFData := Segment.Data.Memory;
  if Segment.Data.Size >= SizeOf(TJPEGStartOfFrameData) then begin
    AddLine('Sample precision'#9'%d', [SOFData.SamplePrecision]);
    AddLine('Width'#9'%d',[MakeWord(SOFData.ImageWidthLo, SOFData.ImageWidthHi)]);
    AddLine('Height'#9'%d', [MakeWord(SOFData.ImageHeightLo, SOFData.ImageHeightHi)]);
    AddLine('Component count'#9'%d', [SOFData.ComponentCount]);
  end;
  AddBlankLine;
end;

procedure tNewOutputFrame.LoadSOS(const Segment: IFoundJPEGSegment);
begin
  DoDefault(Segment, 'Start of scan (SOS)');
end;

function GetXMPNamespaceTitle(Schema: TXMPSchema): string;
begin
  case Schema.NamespaceInfo.Kind of
    xsCameraRaw: Result := 'Camera Raw';
    xsColorant: Result :='Dublin Core';
    xsDimensions: Result := 'Dimensions';
    xsDublinCore: Result := 'Dublin Core';
    xsExif: Result := 'Exif';
    xsExifAux: Result := 'Exif Auxiliary';
    xsMicrosoftPhoto: Result := 'Microsoft Photo';
    xsPDF: Result := 'PDF';
    xsPhotoshop: Result := 'Photoshop';
    xsResourceEvent: Result := 'Resource Event';
    xsResourceRef: Result := 'Resource Reference';
    xsTIFF: Result := 'TIFF';
    xsXMPBasic: Result := 'XMP Basic';
    xsXMPBasicJobTicket: Result := 'XMP Basic Job Ticket';
    xsXMPDynamicMedia: Result := 'XMP Dynamic Media';
    xsXMPMediaManagement: Result := 'XMP Media Management';
    xsXMPPagedText: Result := 'XMP Paged Text';
    xsXMPRights: Result := 'XMP Rights';
  else Result := Schema.NamespaceInfo.Prefix;
  end;
end;

procedure tNewOutputFrame.LoadXMP(const Segment: IFoundJPEGSegment);


        procedure DoIt(Level: Integer; const Name: string; const Props: IXMPPropertyCollection);
        const
          SPropKinds: array[TXMPPropertyKind] of string =  ('simple', 'structure', 'alternative (''alt'') array','unordered (''bag'') array', 'ordered (''seq'') array');
        var
          Counter: Integer;
          Prop: TXMPProperty;
          S: string;
        begin
          if Level <= 1 then
            Memo.Lines.Add(Name)
          else  begin
            S := StringOfChar(' ', Pred(Level) * 3) + Name;
            Memo.Lines.Add(S);
          end;
          Counter := 0;
          for Prop in Props do begin
            Inc(Counter);
            if (Prop.ParentProperty = nil) or not (Prop.ParentProperty.Kind in [xpBagArray, xpSeqArray]) then
              if Prop.Kind = xpSimple then S := Prop.Name
              else S := Format('%s'#9'[%s]', [Prop.Name, SPropKinds[Prop.Kind]])
            else
              S := Format('<item %d>', [Counter]);
            if Prop.SubPropertyCount = 0 then S := S + #9 + Prop.ReadValue;
            DoIt(Level + 1, S, Prop);
          end;
        end;


var
  I: Integer;
  Schema: TXMPSchema;
  S: string;
  SeekPtr: PAnsiChar;
  StartPos: Integer;
  UTF8Str: UTF8String;
  XMPPacket: TXMPPacket;
begin
  DoDefault(Segment, 'XMP', False);
  XMPPacket := TXMPPacket.Create;
  try
    StartPos := Segment.Data.Position;
    if not XMPPacket.TryLoadFromStream(Segment.Data) then begin
      AddBlankLine;
      Memo.Lines.Add('*** Failed to parse packet ***');
      AddBlankLine;
      SeekPtr := Segment.Data.Memory;
      for I := 0 to Segment.Data.Size - 1 do begin
        if SeekPtr^ in [#0..#8, #11..#12, #14..#31] then SeekPtr^ := ' ';
        Inc(SeekPtr);
      end;
      SeekPtr := Segment.Data.Memory;
      Inc(SeekPtr, StartPos);
      SetString(UTF8Str, SeekPtr, Segment.Data.Size - StartPos);
      Memo.Lines.Add(string(UTF8Str));
      AddBlankLine;
      Exit;
    end;
    S := XMPPacket.AboutAttributeValue;
    if S = '' then S := '(empty or not set)';
    AddLine('About attribute value'#9'%s', [S]);
    for Schema in XMPPacket do begin
      AddBlankLine;
      DoIt(0, GetXMPNamespaceTitle(Schema) + ':', Schema);
    end;
  finally
    XMPPacket.Free;
  end;
  AddBlankLine;
end;

procedure tNewOutputFrame.LoadFromFile(const JPEGFile: string);
var
  Segment: IFoundJPEGSegment;

        procedure DoUnrecognised;
        begin
          if Segment.MarkerNum in [jmAppSpecificFirst..jmAppSpecificLast] then DoDefault(Segment, Format('APP%d', [Segment.MarkerNum - jmAppSpecificFirst]))
          else DoDefault(Segment, Format('Unrecognised marker ($%.2x)', [Segment.MarkerNum]));
        end;

const
  TabStops: array[0..4] of UINT = (2, 4, 6, 96, 162);
var
  AnsiStr: AnsiString;
  Len: Integer;
begin
  grpExifThumbnail.Hide;
  imgExifThumbnail.Picture.Assign(nil);
  SendMessage(Memo.Handle, EM_SETTABSTOPS, Length(TabStops), LPARAM(@TabStops));
  Memo.Lines.BeginUpdate;
  try
    Memo.Lines.Clear;
    for Segment in JPEGHeader(JPEGFile) do
      case Segment.MarkerNum of
        jmStartOfScan:  if Verbose then LoadSOS(Segment);
        jmEndOfImage: if Verbose then DoDefault(Segment, 'End of image (EOI)');
        jmJFIF:     if Verbose then  LoadJFIF(Segment);
        jmStartOfFrame0..jmStartOfFrame3, jmStartOfFrame5..jmStartOfFrame7 :  if Verbose then LoadSOF(Segment);
        jmRestartInternal:
                 if Verbose then begin
                   DoDefault(Segment, 'Restart interval', False);
                   if Segment.Data.Size >= 2 then AddLine('Value'#9'%d', [PWord(Segment.Data.Memory)^]);
                   AddBlankLine;
                 end;
        jmComment:
                 if Verbose then begin
                   DoDefault(Segment, 'Comment', False);
                   //*should* be null terminated, but we'll play safe
                   Len := Segment.Data.Size;
                   if (Len > 0) and (PAnsiChar(Segment.Data.Memory)[Len - 1] = #0) then Dec(Len);
                   SetString(AnsiStr, PAnsiChar(Segment.Data.Memory), Len);
                   AddLine('Value'#9'%s', [string(AnsiStr)]);
                   AddBlankLine;
                 end;
        jmApp1:    if Segment.IsExifBlock then LoadExif(Segment)
                   else if Verbose then begin
                      if Segment.IsXMPBlock then LoadXMP(Segment)
                      else DoUnrecognised;
                   end;
        jmApp13:if Verbose then begin
                   if Segment.IsAdobeApp13 then LoadAdobeApp13(Segment)
                   else DoUnrecognised;
                end;
        jmQuantizationTable: if Verbose then DoDefault(Segment, 'Quantisation table definition(s)');
        jmHuffmanTable: if Verbose then DoDefault(Segment, 'Huffman table definition(s)');
      else
        if Verbose then DoUnrecognised;
      end;
  finally
    Memo.Lines.EndUpdate;
  end;
  if (imgExifThumbnail.Picture.Graphic <> nil) then begin
    grpExifThumbnail.Width := (grpExifThumbnail.Width - imgExifThumbnail.Width) + imgExifThumbnail.Picture.Width;
    grpExifThumbnail.Visible := True;
  end;
end;

end.
