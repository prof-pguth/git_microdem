object NetForm: TNetForm
  Left = 201
  Top = 120
  Caption = 'NetForm'
  ClientHeight = 417
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'System'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00BBBB
    BBBBBBBB55550555BBBBBBBBBBBBBBBBBBBBB5550BBB0BBB055BBBBBBBBBBBBB
    BBB50BBB0BBB0BBB0BB55BBBBBBBBBBBBB50000000000000000005BBBBBBBBBB
    05BB0BBB0BBB0BBB0BBB0B5BBBBBBBBB0BBB0BBB0BBB0BBB0BBB0BB5BBBBBBB5
    0BBB0BBB0BBB0BBB0BBB0BBB0BBBBB5000000000000000000000000005BBBB5B
    0BBB0BBB0BBB0BBB0BBB0BBB05BBB5BB0BBB0BBB0BBB0BBB0BBB0BBB0B5BB5BB
    0BBB0BBB0BBB0BBB0BBB0BBB0B5BB500000000000000000000000000005B5BBB
    0BBB0BBB0BBB0BBB0BBB0BBB0BB55BBB0BBB0BBB0BBB0BBB0BBB0BBB0BB55BBB
    0BBB0BBB0BBB0BBB0BBB0BBB0BB5500000000000000000000000000000005BBB
    0BBB0BBB0BBB0BBB0BBB0BBB0BB55BBB0BBB0BBB0BBB0BBB0BBB0BBB0BB55BBB
    0BBB0BBB0BBB0BBB0BBB0BBB0BB550000000000000000000000000000000B5BB
    0BBB0BBB0BBB0BBB0BBB0BBB0B5BB5BB0BBB0BBB0BBB0BBB0BBB0BBB0B5BB5BB
    0BBB0BBB0BBB0BBB0BBB0BBB0B5BBB50000000000000000000000000000BBB5B
    0BBB0BBB0BBB0BBB0BBB0BBB05BBBBB50BBB0BBB0BBB0BBB0BBB0BBB5BBBBBBB
    0BBB0BBB0BBB0BBB0BBB0BB5BBBBBBBBB5000000000000000000005BBBBBBBBB
    BB5B0BBB0BBB0BBB0BBB05BBBBBBBBBBBBB55BBB0BBB0BBB0BB55BBBBBBBBBBB
    BBBBB5550BBB0BBB555BBBBBBBBBBBBBBBBBBBBB55555555BBBBBBBBBBBB0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Menu = MainMenu1
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 16
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 388
    Height = 376
    Align = alClient
    OnMouseDown = Image1MouseDown
    ExplicitWidth = 400
    ExplicitHeight = 340
  end
  object Panel1: TPanel
    Left = 0
    Top = 376
    Width = 388
    Height = 41
    Align = alBottom
    Caption = ' '
    TabOrder = 0
    ExplicitTop = 444
    ExplicitWidth = 400
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 256
    object File1: TMenuItem
      Caption = '&File'
      object Adddata1: TMenuItem
        Caption = '&Add data'
        Enabled = False
      end
      object SaveImage1: TMenuItem
        Caption = '&Save Image'
        OnClick = SaveImage1Click
      end
      object Zeronet1: TMenuItem
        Caption = '&Zero net'
        OnClick = Zeronet1Click
      end
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object Plot1: TMenuItem
      Caption = 'Plot'
      object Plane1: TMenuItem
        Caption = 'Plane (great circle)'
      end
      object Poletoplane1: TMenuItem
        Caption = 'Pole to plane'
      end
      object Planepolegreatcircle1: TMenuItem
        Caption = 'Plane (pole && great circle)'
      end
      object Line1: TMenuItem
        Caption = 'Line'
        OnClick = Line1Click
      end
    end
    object Modify1: TMenuItem
      Caption = '&Modify'
      HelpContext = 31
      Hint = 'Modify the stereo net'
      object Net1: TMenuItem
        Caption = '&Net'
        OnClick = Net1Click
      end
      object Title1: TMenuItem
        Caption = '&Title'
        OnClick = Title1Click
      end
      object Contourinterval1: TMenuItem
        Caption = '&Contour interval'
        OnClick = Contourinterval1Click
      end
    end
    object Statistics1: TMenuItem
      Caption = '&Statistics'
      HelpContext = 32
      Hint = 'Calculate statistics for stereo net'
      object AverageOrientation1: TMenuItem
        Caption = '&Average Orientation'
        OnClick = AverageOrientation1Click
      end
      object Betadiagram1: TMenuItem
        Caption = 'Beta diagram'
        OnClick = Betadiagram1Click
      end
      object Contour1: TMenuItem
        Caption = '&Contour'
        Visible = False
        OnClick = Contour1Click
      end
      object Intersectiontwoplanes1: TMenuItem
        Caption = 'Intersection two planes'
        OnClick = Intersectiontwoplanes1Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'BMP'
    Title = 'Save BMP Image'
    Left = 16
    Top = 32
  end
  object PopupMenu1: TPopupMenu
    Left = 128
    Top = 88
    object Netopitons1: TMenuItem
      Caption = 'Net options'
      OnClick = Netopitons1Click
    end
    object Countourlimits1: TMenuItem
      Caption = 'Countour limits'
      OnClick = Countourlimits1Click
    end
    object Enterandplot1: TMenuItem
      Caption = 'Enter and plot'
      OnClick = Enterandplot1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Saveimage2: TMenuItem
      Caption = 'Save image'
      OnClick = Saveimage2Click
    end
    object Copytoclipboard1: TMenuItem
      Caption = 'Copy to clipboard'
      OnClick = Copytoclipboard1Click
    end
  end
end
