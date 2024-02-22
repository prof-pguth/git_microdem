object petcompressform: Tpetcompressform
  Left = 245
  Top = 244
  BorderIcons = [biSystemMenu]
  Caption = 'Compress/Decompress'
  ClientHeight = 360
  ClientWidth = 840
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  Menu = MainMenu1
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 16
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 840
    Height = 341
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 341
    Width = 840
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 250
      end>
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 40
    object File1: TMenuItem
      Caption = '&File'
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object Uncompress1: TMenuItem
      Caption = 'Uncompress'
      object UncompressZIPfile1: TMenuItem
        Caption = 'ZIP file (unzip to this diretory, e.g. for Sentinel-2)'
        OnClick = UncompressZIPfile1Click
      end
      object ZIPfileseachtoitsowndirectory1: TMenuItem
        Caption = 'ZIP files (each to its own directory)'
        OnClick = ZIPfileseachtoitsowndirectory1Click
      end
      object Allzipsindirsubdirs1: TMenuItem
        Caption = 'All zips in dir/subdirs'
        OnClick = Allzipsindirsubdirs1Click
      end
      object Verylargezipusing7Zip1: TMenuItem
        Caption = '7Z or Very large zip using 7Zip'
        OnClick = Verylargezipusing7Zip1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object LandsatGeotiffsfromunixtar1: TMenuItem
        Caption = 'Landsat Geotiffs from unix tar'
        OnClick = LandsatGeotiffsfromunixtar1Click
      end
      object IFfromunixtar1: TMenuItem
        Caption = 'Copernicus DEM TIF from unix tar'
        OnClick = IFfromunixtar1Click
      end
      object HGTfromzip1: TMenuItem
        Caption = 'HGT from zip'
        OnClick = HGTfromzip1Click
      end
      object IFfromzip1: TMenuItem
        Caption = 'TIF from zip'
        OnClick = IFfromzip1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Uncompressgzfile1: TMenuItem
        Caption = 'Generic Unix GZ file'
        OnClick = Uncompressgzfile1Click
      end
      object ExpandTarFile1: TMenuItem
        Caption = 'Generic Unix Tar File if no special option'
        OnClick = ExpandTarFile1Click
      end
      object Unixtargzfile1: TMenuItem
        Caption = 'Generic Unix tar.gz file'
        OnClick = Unixtargzfile1Click
      end
    end
    object Zipcompress3: TMenuItem
      Caption = 'Zip compress'
      object ZIPcompress1: TMenuItem
        Caption = 'Single named archive'
        OnClick = ZIPcompress1Click
      end
      object Zipcompress2: TMenuItem
        Caption = 'Zip compress'
        object Mutliplesinglefilearchives1: TMenuItem
          Caption = 'Mutliple single file archives'
          OnClick = Mutliplesinglefilearchives1Click
        end
      end
      object BZIP21: TMenuItem
        Caption = 'BZIP2'
        OnClick = BZIP21Click
      end
    end
    object LASLAZ1: TMenuItem
      Caption = 'LAS/LAZ'
      object CompressLAS1: TMenuItem
        Caption = 'Compress LAS to LAZ'
        object CompressLASretain1: TMenuItem
          Caption = 'Retain LAS'
          OnClick = CompressLASretain1Click
        end
        object CompressLASpurge1: TMenuItem
          Caption = 'Recycle LAS'
          OnClick = CompressLASpurge1Click
        end
      end
      object UncompressLAZ1: TMenuItem
        Caption = 'Uncompress LAZ'
        object UncompressLAZretain1: TMenuItem
          Caption = 'Retain LAZ'
          OnClick = UncompressLAZretain1Click
        end
        object UncompressLAZPurge1: TMenuItem
          Caption = 'Recycle LAZ'
          OnClick = UncompressLAZPurge1Click
        end
        object FilesinallsubdirectoriespurgeLAZ1: TMenuItem
          Caption = 'Files in all subdirectories, recycle LAZ'
          OnClick = FilesinallsubdirectoriespurgeLAZ1Click
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object CompressLAStozLAS1: TMenuItem
        Caption = 'Compress LAS to zLAS'
        OnClick = CompressLAStozLAS1Click
      end
      object UncompresszLAStoLAS1: TMenuItem
        Caption = 'Uncompress zLAS to LAS'
        OnClick = UncompresszLAStoLAS1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      OnClick = Help1Click
    end
  end
end
