object AlgCompareForm: TAlgCompareForm
  Left = 0
  Top = 0
  Caption = 'Algorithm/program comparisons'
  ClientHeight = 247
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 24
    Top = 8
    Width = 257
    Height = 184
    Caption = 'Comparisons to make'
    TabOrder = 0
    object CheckBox1: TCheckBox
      Left = 16
      Top = 24
      Width = 217
      Height = 17
      Caption = 'R--Pearson correlation coefficient'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 48
      Width = 209
      Height = 17
      Caption = 'MAbD--mean  absolute difference'
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 72
      Width = 238
      Height = 17
      Caption = 'MAvD--mean average diffference'
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 95
      Width = 97
      Height = 17
      Caption = 'Scattergrams'
      TabOrder = 3
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 117
      Width = 97
      Height = 17
      Caption = 'Histograms'
      TabOrder = 4
    end
    object CheckBox9: TCheckBox
      Left = 16
      Top = 140
      Width = 153
      Height = 17
      Caption = 'Difference maps'
      TabOrder = 5
    end
    object CheckBox10: TCheckBox
      Left = 16
      Top = 163
      Width = 97
      Height = 17
      Caption = 'Box plots'
      TabOrder = 6
    end
  end
  object OKBtn: TBitBtn
    Left = 22
    Top = 200
    Width = 52
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
    TabOrder = 1
    OnClick = OKBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 104
    Top = 200
    Width = 58
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    IsControl = True
  end
  object CheckBox6: TCheckBox
    Left = 312
    Top = 24
    Width = 169
    Height = 17
    Caption = 'Use colleague grids'
    TabOrder = 3
  end
  object CheckBox7: TCheckBox
    Left = 312
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Open maps'
    TabOrder = 4
  end
  object CheckBox8: TCheckBox
    Left = 312
    Top = 72
    Width = 217
    Height = 17
    Caption = 'GDAL + SAGA for arc sec DEMs'
    TabOrder = 5
  end
  object BitBtn38: TBitBtn
    Left = 168
    Top = 198
    Width = 113
    Height = 29
    Caption = 'Save defaults'
    TabOrder = 6
    OnClick = BitBtn38Click
  end
  object CheckBox11: TCheckBox
    Left = 312
    Top = 95
    Width = 193
    Height = 17
    Caption = 'Close grids after computations'
    TabOrder = 7
  end
end
