object PickDatumParams: TPickDatumParams
  Left = 118
  Top = 530
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Pick Projection Parameters'
  ClientHeight = 226
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 22
    Top = 39
    Width = 52
    Height = 13
    Caption = 'UTM Zone'
  end
  object Label2: TLabel
    Left = 80
    Top = 66
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 224
    Top = 100
    Width = 3
    Height = 13
  end
  object Label4: TLabel
    Left = 224
    Top = 120
    Width = 3
    Height = 13
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 88
    Width = 177
    Height = 37
    Caption = 'Hemisphere'
    Columns = 2
    Items.Strings = (
      'Northern'
      'Southern')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 80
    Top = 39
    Width = 57
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 273
    Height = 25
    TabOrder = 2
    OnClick = Button1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 16
    Top = 131
    Width = 177
    Height = 38
    Caption = 'DEM Z units'
    Columns = 2
    Items.Strings = (
      'Meters'
      'Feet')
    TabOrder = 3
    OnClick = RadioGroup2Click
  end
  object BitBtn3: TBitBtn
    Left = 208
    Top = 147
    Width = 75
    Height = 25
    Caption = 'PRJ file'
    TabOrder = 4
    OnClick = BitBtn3Click
  end
  object OKBtn: TBitBtn
    Left = 22
    Top = 200
    Width = 77
    Height = 27
    Caption = 'OK'
    Default = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    Margin = 2
    ModalResult = 1
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 128
    Top = 200
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Button2: TButton
    Left = 208
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Geo (lat/long)'
    TabOrder = 7
    OnClick = Button2Click
  end
  object CheckBox2: TCheckBox
    Left = 143
    Top = 39
    Width = 59
    Height = 17
    Caption = 'Default'
    TabOrder = 8
    OnClick = CheckBox2Click
  end
end
