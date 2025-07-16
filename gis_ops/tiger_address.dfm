object TigerAddressForm: TTigerAddressForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'TIGER address'
  ClientHeight = 51
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  TextHeight = 13
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 543
    Height = 36
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 537
    object Label3: TLabel
      Left = 5
      Top = 5
      Width = 39
      Height = 13
      Caption = 'Address'
    end
    object Edit3: TEdit
      Left = 186
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Main'
    end
    object ComboBox1: TComboBox
      Left = 313
      Top = 8
      Width = 57
      Height = 21
      TabOrder = 1
      Text = 'St'
      Items.Strings = (
        'Aly'
        'Ave'
        'Blvd'
        'Br'
        'Brg'
        'Byp'
        'Cir'
        'Ct'
        'Cv'
        'Dr'
        'Expy'
        'Fwy'
        'Hwy'
        'Ln'
        'Loop'
        'Mal'
        'Pass'
        'Path'
        'Pky'
        'Pl'
        'Plz'
        'Rd'
        'Row'
        'Run'
        'Sq'
        'St'
        'Ter'
        'Thwy'
        'Trl'
        'Walk'
        'Way'
        'Wkwy'
        'Xing')
    end
    object ComboBox2: TComboBox
      Left = 120
      Top = 8
      Width = 60
      Height = 21
      TabOrder = 2
      Items.Strings = (
        'E'
        'N'
        'NE'
        'NW'
        'S'
        'SE'
        'SW'
        'W')
    end
    object ComboBox3: TComboBox
      Left = 376
      Top = 8
      Width = 50
      Height = 21
      TabOrder = 3
      Items.Strings = (
        'E'
        'N'
        'NE'
        'NW'
        'S'
        'SE'
        'SW'
        'W')
    end
    object BitBtn3: TBitBtn
      Left = 432
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Find'
      TabOrder = 4
      OnClick = BitBtn3Click
    end
    object StreetNoEdit: TEdit
      Left = 56
      Top = 8
      Width = 58
      Height = 21
      Cursor = crIBeam
      TabOrder = 5
      Text = '1'
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 36
    Width = 543
    Height = 15
    Align = alClient
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
