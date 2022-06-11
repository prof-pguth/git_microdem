object TerraserverForm: TTerraserverForm
  Left = 444
  Top = 205
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'MSR Image Selection'
  ClientHeight = 280
  ClientWidth = 209
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 230
    Width = 55
    Height = 13
    Caption = 'Shift frames'
  end
  object BitBtn3: TBitBtn
    Left = 109
    Top = 247
    Width = 37
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = BitBtn3Click
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 79
    Width = 185
    Height = 57
    Caption = 'Shift map'
    Columns = 3
    ItemIndex = 4
    Items.Strings = (
      'NW'
      'W'
      'SW'
      'N'
      'None'
      'S'
      'NE'
      'E'
      'SE')
    TabOrder = 1
    OnClick = RadioGroup3Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 249
    Width = 33
    Height = 21
    TabOrder = 2
    Text = '1'
  end
  object RadioGroup4: TRadioGroup
    Left = 8
    Top = 136
    Width = 185
    Height = 57
    Caption = 'Tiles'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      '3x3'
      '5x5'
      '7x7'
      '11x11'
      '15x15'
      '25x25')
    TabOrder = 3
    OnClick = RadioGroup4Click
  end
  object BitBtn1: TBitBtn
    Left = 54
    Top = 249
    Width = 49
    Height = 25
    Caption = 'Unhide'
    TabOrder = 4
    OnClick = BitBtn1Click
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 199
    Width = 75
    Height = 25
    Caption = 'Center coords'
    TabOrder = 5
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 89
    Top = 199
    Width = 75
    Height = 25
    Caption = 'Open map'
    Enabled = False
    TabOrder = 6
    OnClick = BitBtn5Click
  end
  object HelpBtn: TBitBtn
    Left = 152
    Top = 246
    Width = 52
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 7
    OnClick = HelpBtnClick
    IsControl = True
  end
  object PageControl1: TPageControl
    Left = 5
    Top = 2
    Width = 196
    Height = 71
    ActivePage = TabSheet1
    TabOrder = 8
    object DOQQ: TTabSheet
      Caption = 'DOQQ'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object DOQQButton: TBitBtn
        Left = 108
        Top = 3
        Width = 57
        Height = 25
        Caption = 'Get DOQQ'
        TabOrder = 0
        OnClick = DOQQButtonClick
      end
      object ComboBox1: TComboBox
        Left = 3
        Top = 3
        Width = 78
        Height = 21
        ItemIndex = 3
        TabOrder = 1
        Text = '8 m'
        Items.Strings = (
          '1 m'
          '2 m'
          '4 m'
          '8 m'
          '16 m'
          '32 m'
          '64 m')
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'DRG'
      ImageIndex = 1
      object DRGButton: TBitBtn
        Left = 121
        Top = 11
        Width = 59
        Height = 25
        Caption = 'Get DRG'
        TabOrder = 0
        OnClick = DRGButtonClick
      end
      object ComboBox2: TComboBox
        Left = 3
        Top = 11
        Width = 95
        Height = 21
        TabOrder = 1
        Text = '16 m (100K)'
        Items.Strings = (
          '2 m (24K)'
          '4 m (24K)'
          '8 m (100K)'
          '16 m (100K)'
          '32 m (250k)'
          '64 m (250K)'
          '128 m (250K)'
          '256 m (250K)')
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Urban'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ComboBox3: TComboBox
        Left = 3
        Top = 3
        Width = 90
        Height = 21
        TabOrder = 0
        Text = '4 m'
        Items.Strings = (
          '0.25 m'
          '0.5 m'
          '1 m'
          '2 m'
          '4 m'
          '8 m'
          '16 m'
          '32 m'
          '64 m')
      end
      object BitBtn2: TBitBtn
        Left = 99
        Top = 3
        Width = 74
        Height = 25
        Caption = 'Get Urban'
        TabOrder = 1
        OnClick = BitBtn2Click
      end
    end
  end
  object CheckBox1: TCheckBox
    Left = 104
    Top = 226
    Width = 97
    Height = 17
    Caption = 'Image only'
    TabOrder = 9
  end
end
