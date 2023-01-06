object GetLatLongDlg: TGetLatLongDlg
  Left = 379
  Top = 185
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 235
  ClientWidth = 364
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object OKBtn: TBitBtn
    Left = 10
    Top = 210
    Width = 61
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 77
    Top = 209
    Width = 61
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object TabbedNotebook1: TTabbedNotebook
    Left = 8
    Top = 8
    Width = 360
    Height = 161
    TabFont.Charset = DEFAULT_CHARSET
    TabFont.Color = clBtnText
    TabFont.Height = -11
    TabFont.Name = 'MS Sans Serif'
    TabFont.Style = []
    TabOrder = 2
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Lat/Long'
      object TabControl1: TTabControl
        Left = 3
        Top = 4
        Width = 338
        Height = 125
        TabOrder = 0
        object Label2: TLabel
          Left = 120
          Top = 8
          Width = 45
          Height = 13
          Caption = ' Deg ('#176')'
        end
        object Minutes: TLabel
          Left = 213
          Top = 8
          Width = 36
          Height = 13
          Caption = 'Min ('#39')'
        end
        object Seconds: TLabel
          Left = 264
          Top = 8
          Width = 41
          Height = 13
          Caption = 'Sec (")'
        end
        object RadioGroup1: TRadioGroup
          Left = 8
          Top = 16
          Width = 105
          Height = 33
          Caption = 'Latitude'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'N (+)'
            'S (-)')
          TabOrder = 6
        end
        object RadioGroup2: TRadioGroup
          Left = 8
          Top = 56
          Width = 105
          Height = 33
          Caption = 'Longitude'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'W (-) '
            'E (+)')
          TabOrder = 7
        end
        object Edit2: TEdit
          Left = 120
          Top = 24
          Width = 87
          Height = 21
          TabOrder = 0
          Text = ' '
        end
        object Edit6: TEdit
          Left = 120
          Top = 64
          Width = 87
          Height = 21
          TabOrder = 3
          Text = ' '
        end
        object Edit7: TEdit
          Left = 216
          Top = 64
          Width = 49
          Height = 21
          TabOrder = 4
          Text = ' '
        end
        object Edit8: TEdit
          Left = 271
          Top = 64
          Width = 50
          Height = 21
          TabOrder = 5
          Text = ' '
        end
        object CheckBox1: TCheckBox
          Left = 8
          Top = 96
          Width = 113
          Height = 17
          Caption = 'Longitude 0-360'
          TabOrder = 8
        end
        object Edit3: TEdit
          Left = 216
          Top = 24
          Width = 49
          Height = 21
          TabOrder = 1
          Text = ' '
        end
        object Edit4: TEdit
          Left = 272
          Top = 24
          Width = 49
          Height = 21
          TabOrder = 2
          Text = ' '
        end
        object RadioGroup4: TRadioGroup
          Left = 136
          Top = 88
          Width = 153
          Height = 33
          Caption = 'Decimal'
          Columns = 3
          Items.Strings = (
            'Deg'
            'Min'
            'Sec')
          TabOrder = 9
          OnClick = RadioGroup4Click
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'MGRS'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label5: TLabel
        Left = 48
        Top = 16
        Width = 37
        Height = 13
        Caption = 'MGRS'
      end
      object Edit1: TEdit
        Left = 96
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object Edit11: TEdit
        Left = 56
        Top = 72
        Width = 49
        Height = 21
        Enabled = False
        TabOrder = 1
      end
      object Edit12: TEdit
        Left = 112
        Top = 72
        Width = 41
        Height = 21
        Enabled = False
        TabOrder = 2
      end
      object Edit13: TEdit
        Left = 160
        Top = 72
        Width = 65
        Height = 21
        Enabled = False
        TabOrder = 3
      end
      object Edit14: TEdit
        Left = 240
        Top = 72
        Width = 49
        Height = 21
        Enabled = False
        TabOrder = 4
      end
      object CheckBox2: TCheckBox
        Left = 8
        Top = 16
        Width = 25
        Height = 17
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = CheckBox2Click
      end
      object CheckBox3: TCheckBox
        Left = 8
        Top = 72
        Width = 25
        Height = 17
        TabOrder = 6
        OnClick = CheckBox3Click
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'UTM'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 8
        Top = 56
        Width = 30
        Height = 13
        Caption = 'Zone'
      end
      object Label3: TLabel
        Left = 8
        Top = 88
        Width = 43
        Height = 13
        Caption = 'Easting'
      end
      object Label4: TLabel
        Left = 8
        Top = 112
        Width = 49
        Height = 13
        Caption = 'Northing'
      end
      object Edit5: TEdit
        Left = 48
        Top = 48
        Width = 121
        Height = 21
        TabOrder = 0
        Text = ' '
      end
      object RadioGroup3: TRadioGroup
        Left = 8
        Top = 8
        Width = 145
        Height = 33
        Caption = 'Latitude'
        Columns = 2
        Items.Strings = (
          'N (+)'
          'S (-)')
        TabOrder = 1
      end
      object Edit9: TEdit
        Left = 64
        Top = 80
        Width = 121
        Height = 21
        TabOrder = 2
        Text = ' '
      end
      object Edit10: TEdit
        Left = 64
        Top = 104
        Width = 121
        Height = 21
        TabOrder = 3
        Text = ' '
      end
    end
  end
  object CheckBox4: TCheckBox
    Left = 27
    Top = 175
    Width = 170
    Height = 17
    Caption = 'Save default hemisphere'
    TabOrder = 3
  end
  object BitBtn1: TBitBtn
    Left = 292
    Top = 211
    Width = 61
    Height = 25
    Hint = 'Paste from clipboard'
    Caption = 'Paste'
    TabOrder = 4
    OnClick = BitBtn1Click
  end
end
