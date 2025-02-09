object getplssf: Tgetplssf
  Left = 300
  Top = 172
  BorderIcons = [biSystemMenu]
  Caption = 'Get PLSS Position'
  ClientHeight = 351
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 1
    Width = 509
    Height = 313
    Caption = 'PLSS Location'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 144
      Width = 46
      Height = 13
      Caption = 'Township'
    end
    object Label2: TLabel
      Left = 112
      Top = 144
      Width = 32
      Height = 13
      Caption = 'Range'
    end
    object Label4: TLabel
      Left = 200
      Top = 152
      Width = 106
      Height = 13
      Caption = 'State && Prime Meridian'
    end
    object Label3: TLabel
      Left = 360
      Top = 248
      Width = 5
      Height = 16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 364
      Top = 275
      Width = 5
      Height = 16
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 24
      Width = 89
      Height = 105
      Caption = '1/4'
      Columns = 2
      Enabled = False
      ItemIndex = 2
      Items.Strings = (
        'NW'
        'SW'
        '--'
        'NE'
        'SE')
      TabOrder = 0
      OnClick = RadioGroup1Click
    end
    object RadioGroup2: TRadioGroup
      Left = 104
      Top = 24
      Width = 89
      Height = 105
      Caption = '1/4'
      Columns = 2
      ItemIndex = 2
      Items.Strings = (
        'NW'
        'SW'
        '--'
        'NE'
        'SE')
      TabOrder = 1
      OnClick = RadioGroup2Click
    end
    object RadioGroup3: TRadioGroup
      Left = 200
      Top = 24
      Width = 306
      Height = 113
      Caption = 'Section'
      Columns = 6
      Items.Strings = (
        '6'
        '7'
        '18'
        '19'
        '30'
        '31'
        '5'
        '8'
        '17'
        '20'
        '29'
        '32'
        '4'
        '9'
        '16'
        '21'
        '28'
        '33'
        '3'
        '10'
        '15'
        '22'
        '27'
        '34'
        '2'
        '11'
        '14'
        '23'
        '26'
        '35'
        '1'
        '12'
        '13'
        '24'
        '25'
        '36')
      TabOrder = 2
      OnClick = RadioGroup3Click
    end
    object Edit1: TEdit
      Left = 8
      Top = 168
      Width = 41
      Height = 21
      TabOrder = 3
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 104
      Top = 168
      Width = 33
      Height = 21
      TabOrder = 4
      OnChange = Edit2Change
    end
    object ComboBox1: TComboBox
      Left = 199
      Top = 171
      Width = 201
      Height = 21
      TabOrder = 5
      OnChange = ComboBox1Change
    end
    object Edit3: TEdit
      Left = 8
      Top = 240
      Width = 337
      Height = 21
      TabOrder = 6
    end
    object RadioGroup4: TRadioGroup
      Left = 8
      Top = 192
      Width = 65
      Height = 33
      Columns = 2
      ItemIndex = 1
      Items.Strings = (
        'N'
        'S')
      TabOrder = 7
      OnClick = RadioGroup4Click
    end
    object RadioGroup5: TRadioGroup
      Left = 104
      Top = 192
      Width = 73
      Height = 33
      Columns = 2
      ItemIndex = 1
      Items.Strings = (
        'W '
        'E')
      TabOrder = 8
      OnClick = RadioGroup5Click
    end
    object CheckBox1: TCheckBox
      Left = 56
      Top = 168
      Width = 41
      Height = 17
      Caption = '1/2'
      TabOrder = 9
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 144
      Top = 160
      Width = 41
      Height = 17
      Caption = '1/2'
      TabOrder = 10
      OnClick = CheckBox2Click
    end
    object Edit5: TEdit
      Left = 8
      Top = 272
      Width = 337
      Height = 21
      TabOrder = 11
    end
    object CheckBox3: TCheckBox
      Left = 144
      Top = 176
      Width = 49
      Height = 17
      Caption = '3/4'
      TabOrder = 12
      OnClick = CheckBox3Click
    end
    object BitBtn3: TBitBtn
      Left = 406
      Top = 156
      Width = 75
      Height = 25
      Caption = 'Locate'
      TabOrder = 13
      OnClick = BitBtn3Click
    end
    object CheckBox4: TCheckBox
      Left = 422
      Top = 198
      Width = 85
      Height = 17
      Caption = 'Autoupdate'
      TabOrder = 14
      OnClick = CheckBox4Click
    end
  end
  object BitBtn2: TBitBtn
    Left = 24
    Top = 320
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object CheckBox7: TCheckBox
    Left = 213
    Top = 199
    Width = 193
    Height = 17
    Caption = 'Township/Range/Section labels'
    TabOrder = 2
    OnClick = CheckBox7Click
  end
  object BitBtn1: TBitBtn
    Left = 168
    Top = 318
    Width = 67
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object BitBtn16: TBitBtn
    Left = 332
    Top = 318
    Width = 29
    Height = 25
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFF00000000000000000FFFFFFFFFFFFFF00FFFFFF9999FFFF00FFFFF99FF99
      FFF00FFFFF99FFFFFFF00FFFFF99FFFFFFF00FFFFF99FFFFFFF00FFFFF99FF99
      FFF00FFF0FF9999FFFF00FFF0FFFFFFFFFF00F00000FFFFFFFF00FFF0FFFFFFF
      FFF00FFF0FFFFFFFFFF00FFFFFFFFFFFFFF00000000000000000}
    TabOrder = 4
    OnClick = BitBtn16Click
  end
  object BitBtn15: TBitBtn
    Left = 367
    Top = 318
    Width = 29
    Height = 25
    Glyph.Data = {
      4E010000424D4E01000000000000760000002800000012000000120000000100
      040000000000D800000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFF000000FFFFFFFFFFFFFFF0FF000000FFFFFFFFFFFFFF000F000000FFFF
      FFFFFFFFF000FF000000FFFFFFFFFFFF000FFF000000FFFFFFFFFFF000FFFF00
      0000FFFF70007FB70FFFFF000000FFF07777700BFFFFFF000000FF07F8F8F70F
      FFFFFF000000F77F8F9F8F77FFFFFF000000F078F898F870FFFFFF000000F07F
      99999F70FFFFFF000000F078F898F870FFFFFF000000F77F8F9F8F77FFFFFF00
      0000FF07F8F8F70FFFFFFF000000FFF0777770FFFFFFFF000000FFFF70007FFF
      FFFFFF000000FFFFFFFFFFFFFFFFFF000000}
    TabOrder = 5
    OnClick = BitBtn15Click
  end
  object BitBtn14: TBitBtn
    Left = 407
    Top = 318
    Width = 29
    Height = 25
    Glyph.Data = {
      66010000424D6601000000000000760000002800000014000000140000000100
      040000000000F000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFF0FFF0000FFFF
      FFFFFFFFFFF000FF0000FFFFFFFFFFFFFF000FFF0000FFFFFFFFFFFFF000FFFF
      0000FFFFFFFFFFFF000FFFFF0000FFFFF70007FB70FFFFFF0000FFFF07777700
      BFFFFFFF0000FFF078F8F870FFFFFFFF0000FF778F8F8F877FFFFFFF0000FF07
      F8F8F8F70FFFFFFF0000FF07899999870FFFFFFF0000FF07F8F8F8F70FFFFFFF
      0000FF778F8F8F877FFFFFFF0000FFF078F8F870FFFFFFFF0000FFFF0777770F
      FFFFFFFF0000FFFFF70007FFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFF
      FFFFFFFFFFFFFFFF0000}
    TabOrder = 6
    OnClick = BitBtn14Click
  end
end
