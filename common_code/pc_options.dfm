object pc_opts_form: Tpc_opts_form
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Principal components options'
  ClientHeight = 275
  ClientWidth = 241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 160
    Width = 68
    Height = 13
    Caption = 'Max PC bands'
  end
  object Label2: TLabel
    Left = 32
    Top = 200
    Width = 138
    Height = 13
    Caption = 'Min explanation to show (%)'
  end
  object HelpBtn: TBitBtn
    Left = 116
    Top = 230
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 230
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
    TabOrder = 1
    IsControl = True
  end
  object CheckBox2: TCheckBox
    Left = 32
    Top = 31
    Width = 113
    Height = 17
    Caption = 'Correlation matrix'
    TabOrder = 2
  end
  object CheckBox3: TCheckBox
    Left = 32
    Top = 55
    Width = 161
    Height = 17
    Caption = 'Variance-covariance matrix'
    TabOrder = 3
  end
  object CheckBox4: TCheckBox
    Left = 32
    Top = 80
    Width = 168
    Height = 17
    Caption = 'Eigen vectors and PC loadings'
    TabOrder = 4
  end
  object CheckBox5: TCheckBox
    Left = 32
    Top = 104
    Width = 168
    Height = 17
    Caption = 'Principal component results'
    TabOrder = 5
  end
  object Edit1: TEdit
    Left = 106
    Top = 160
    Width = 50
    Height = 21
    TabOrder = 6
    OnChange = Edit1Change
  end
  object CheckBox6: TCheckBox
    Left = 32
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Load PC images'
    TabOrder = 7
  end
  object Edit2: TEdit
    Left = 176
    Top = 197
    Width = 49
    Height = 21
    TabOrder = 8
    Text = 'Edit2'
  end
end
