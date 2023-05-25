object DEMLOSF: TDEMLOSF
  Left = 200
  Top = 454
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'LOS'
  ClientHeight = 189
  ClientWidth = 527
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  Menu = MainMenu1
  Position = poDefaultSizeOnly
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 16
  object TImage
    Left = 389
    Top = 165
    Width = 105
    Height = 105
    Visible = False
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 527
    Height = 112
    VertScrollBar.Visible = False
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 523
    ExplicitHeight = 91
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 523
      Height = 108
      Align = alClient
      OnDblClick = Image1DblClick
      OnMouseDown = Image1MouseDown
      OnMouseMove = Image1MouseMove
      ExplicitLeft = -3
      ExplicitTop = 2
      ExplicitWidth = 605
      ExplicitHeight = 591
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 148
    Width = 527
    Height = 41
    Align = alBottom
    Caption = ' '
    TabOrder = 1
    ExplicitTop = 127
    ExplicitWidth = 523
  end
  object Panel2: TPanel
    Left = 0
    Top = 112
    Width = 527
    Height = 36
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 91
    ExplicitWidth = 523
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 20
      Height = 16
      Caption = 'Az:'
    end
    object TrackBar1: TTrackBar
      Left = 40
      Top = 4
      Width = 150
      Height = 45
      Enabled = False
      Max = 360
      Frequency = 30
      TabOrder = 0
      OnChange = TrackBar1Change
      OnKeyUp = TrackBar1KeyUp
    end
    object Edit1: TEdit
      Left = 196
      Top = 6
      Width = 77
      Height = 24
      TabOrder = 1
      Text = 'Edit1'
      OnChange = Edit1Change
    end
    object BitBtn1: TBitBtn
      Left = 527
      Top = 6
      Width = 50
      Height = 25
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      TabOrder = 2
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 296
      Top = 4
      Width = 145
      Height = 25
      TabOrder = 3
      OnClick = BitBtn2Click
    end
  end
  object MainMenu1: TMainMenu
    AutoMerge = True
    Left = 57
    Top = 25
    object File1: TMenuItem
      Caption = '&File'
      object Openprofiledatabase1: TMenuItem
        Caption = 'Open profile database'
        OnClick = Openprofiledatabase1Click
      end
      object Saveprofileendpoints1: TMenuItem
        Caption = 'Save profile end points'
        OnClick = Saveprofileendpoints1Click
      end
      object Saveimage1: TMenuItem
        Caption = '&Save image'
        OnClick = Saveimage1Click
      end
      object Copytoclipboard1: TMenuItem
        Caption = 'Copy to clipboard'
        OnClick = Copytoclipboard1Click
      end
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object Modify1: TMenuItem
      Caption = '&Modify'
      GroupIndex = 1
      OnClick = Modify1Click
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 184
    Top = 16
    object LOSParameters1: TMenuItem
      Caption = 'Profile parameters'
      OnClick = LOSParameters1Click
    end
    object Magneticmodel1: TMenuItem
      Caption = 'Magnetic model'
      OnClick = Magneticmodel1Click
    end
    object Sensorobserversiteblowup1: TMenuItem
      Caption = 'Sensor/observer site blowup'
      OnClick = Sensorobserversiteblowup1Click
    end
    object Adjustazimuth1: TMenuItem
      Caption = 'Adjust azimuth'
      OnClick = Adjustazimuth1Click
    end
    object Adjustrange1: TMenuItem
      Caption = 'Adjust range'
      OnClick = Adjustrange1Click
    end
    object Linesizeandcolors1: TMenuItem
      Caption = 'Line size and colors'
    end
    object SetImagesize1: TMenuItem
      Caption = 'Set &Image size'
      OnClick = SetImagesize1Click
    end
    object Sethorizontalpixelsizem1: TMenuItem
      Caption = 'Set horizontal pixel size (m)'
      OnClick = Sethorizontalpixelsizem1Click
    end
    object Profiledropdown1: TMenuItem
      Caption = 'Profile drop down'
      OnClick = Profiledropdown1Click
    end
    object Profilenames1: TMenuItem
      Caption = 'Profile names'
      OnClick = Profilenames1Click
    end
    object Profilelegends1: TMenuItem
      Caption = 'Profile legends'
      OnClick = Profilelegends1Click
    end
    object Hideprofiles1: TMenuItem
      Caption = 'Hide profiles'
      OnClick = Hideprofiles1Click
    end
    object Intervisibilitysummary1: TMenuItem
      Caption = 'Intervisibility summary'
      OnClick = Intervisibilitysummary1Click
    end
    object OpenGLofPointCloud1: TMenuItem
      Caption = 'OpenGL of Point Cloud'
      OnClick = OpenGLofPointCloud1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Protractor1: TMenuItem
      Caption = 'Protractor'
      OnClick = Protractor1Click
    end
    object Averageelevation1: TMenuItem
      Caption = 'Average elevation'
      OnClick = Averageelevation1Click
    end
    object Measureslope1: TMenuItem
      Caption = 'Measure slope'
      OnClick = Measureslope1Click
    end
    object FrenelZoneencroachment1: TMenuItem
      Caption = 'Frenel Zone encroachment'
      OnClick = FrenelZoneencroachment1Click
    end
    object Frese1: TMenuItem
      Caption = 'Fresnel cross sections'
      OnClick = Frese1Click
    end
    object Wavelengthheight1: TMenuItem
      Caption = 'Wavelength/height crests'
      OnClick = Wavelengthheight1Click
    end
    object Grazingangles1: TMenuItem
      Caption = 'Grazing angles'
      OnClick = Grazingangles1Click
    end
    object Grainalongprofile1: TMenuItem
      Caption = 'Grain along profile'
      OnClick = Grainalongprofile1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Algorithmanalysis1: TMenuItem
      Caption = 'Algorithm analysis'
      OnClick = Algorithmanalysis1Click
    end
    object Spacinganalysis1: TMenuItem
      Caption = 'Spacing analysis'
      OnClick = Spacinganalysis1Click
    end
    object Parallelprofiles1: TMenuItem
      Caption = 'Parallel profiles'
      OnClick = Parallelprofiles1Click
    end
    object AllopenDEMs1: TMenuItem
      Caption = 'All open DEMs'
      OnClick = AllopenDEMs1Click
    end
    object ProfilesonotherDEMs1: TMenuItem
      Caption = 'Profiles on other DEMs'
      OnClick = ProfilesonotherDEMs1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Saveprofileendpoints2: TMenuItem
      Caption = 'Save profile end points'
      OnClick = Saveprofileendpoints2Click
    end
    object Saveimage2: TMenuItem
      Caption = '&Save image'
      OnClick = Saveimage2Click
    end
    object Copytoclipboard2: TMenuItem
      Caption = 'Copy to clipboard'
      OnClick = Copytoclipboard2Click
    end
    object Pastefromclipboard1: TMenuItem
      Caption = 'Paste from clipboard'
      OnClick = Pastefromclipboard1Click
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 304
    Top = 16
    object Saveimage3: TMenuItem
      Caption = 'Save image'
      OnClick = Saveimage3Click
    end
    object Copyimagetoclipboard1: TMenuItem
      Caption = 'Copy image to clipboard'
      OnClick = Copyimagetoclipboard1Click
    end
  end
end
