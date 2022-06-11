object wmdem: Twmdem
  Left = 213
  Top = 236
  BorderIcons = [biMinimize, biMaximize, biHelp]
  ClientHeight = 914
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  WindowState = wsMaximized
  WindowMenu = Windows1
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 895
    Width = 862
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 100
      end
      item
        Width = 200
      end
      item
        Width = 50
      end>
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 862
    Height = 895
    Align = alClient
    TabOrder = 1
    Visible = False
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 136
    object File1: TMenuItem
      Caption = 'File'
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
    object Convert1: TMenuItem
      Caption = 'Convert'
      GroupIndex = 2
      OnClick = Convert1Click
    end
    object Options1: TMenuItem
      Caption = 'Options'
      GroupIndex = 2
      object Defaults1: TMenuItem
        Caption = 'Reset defaults'
        OnClick = Defaults1Click
      end
      object Select1: TMenuItem
        Caption = 'Select options'
        OnClick = Select1Click
      end
      object ChangeDEM1: TMenuItem
        Caption = 'Change regional DEM'
        OnClick = ChangeDEM1Click
      end
      object Changeimagemap1: TMenuItem
        Caption = 'Change image map'
        OnClick = Changeimagemap1Click
      end
      object ChangeTowers1: TMenuItem
        Caption = 'Change Towers'
        OnClick = ChangeTowers1Click
      end
      object Automaticshapefileoverlay1: TMenuItem
        Caption = 'Automatic shape file overlay'
        OnClick = Automaticshapefileoverlay1Click
      end
      object Advancedoptions1: TMenuItem
        Caption = 'Advanced options'
        OnClick = Advancedoptions1Click
      end
      object Windowsizes1: TMenuItem
        Caption = 'Window sizes'
        OnClick = Windowsizes1Click
      end
      object Gazetteerfile1: TMenuItem
        Caption = 'Gazetteer file for target maps'
        OnClick = Gazetteerfile1Click
      end
      object Saveconfiguration1: TMenuItem
        Caption = 'Save configuration'
        OnClick = Saveconfiguration1Click
      end
      object INdexTIGER1: TMenuItem
        Caption = 'Index TIGER'
        OnClick = INdexTIGER1Click
      end
      object BackupEXE1: TMenuItem
        Caption = 'Backup EXE'
        OnClick = BackupEXE1Click
      end
    end
    object Windows1: TMenuItem
      Caption = 'Windows'
      GroupIndex = 2
      object Cascade1: TMenuItem
        Caption = 'Cascade'
        OnClick = Cascade1Click
      end
      object Tile1: TMenuItem
        Caption = 'Tile'
        OnClick = Tile1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      GroupIndex = 2
      object Debuglog1: TMenuItem
        Caption = 'View debug log'
        OnClick = Debuglog1Click
      end
      object Hardware1: TMenuItem
        Caption = 'Hardware '
        OnClick = Hardware1Click
      end
      object Whatsopen1: TMenuItem
        Caption = 'What'#39's open'
        OnClick = Whatsopen1Click
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 250
    OnTimer = Timer1Timer
    Left = 24
    Top = 80
  end
  object WebTimer: TTimer
    Enabled = False
    Interval = 100
    Left = 32
    Top = 16
  end
end
