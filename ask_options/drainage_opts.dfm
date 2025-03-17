object drain_opt_form: Tdrain_opt_form
  Left = 362
  Top = 243
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Drainage flow path options'
  ClientHeight = 455
  ClientWidth = 310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label2: TLabel
    Left = 79
    Top = 383
    Width = 129
    Height = 13
    Caption = 'Vector spacing (map pixels)'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 14
    Top = 422
    Width = 25
    Height = 25
    Hint = 'Force redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = RedrawSpeedButton12Click
  end
  object Edit2: TEdit
    Left = 8
    Top = 383
    Width = 65
    Height = 21
    TabOrder = 0
    OnChange = Edit2Change
  end
  object HelpBtn: TBitBtn
    Left = 175
    Top = 420
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 86
    Top = 420
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
    TabOrder = 2
    OnClick = OKBtnClick
    IsControl = True
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 208
    Width = 177
    Height = 17
    Caption = 'Show regionvector average'
    TabOrder = 3
    OnClick = CheckBox2Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 127
    Width = 260
    Height = 82
    Caption = 'Single point computation'
    TabOrder = 4
    object Label1: TLabel
      Left = 95
      Top = 55
      Width = 68
      Height = 13
      Caption = 'Vector lengths'
    end
    object BitBtn2: TBitBtn
      Left = 18
      Top = 24
      Width = 103
      Height = 25
      Caption = '2d order'
      TabOrder = 0
      OnClick = BitBtn2Click
    end
    object Edit1: TEdit
      Left = 24
      Top = 55
      Width = 65
      Height = 21
      TabOrder = 1
      OnChange = Edit1Change
    end
    object BitBtn1: TBitBtn
      Left = 127
      Top = 24
      Width = 103
      Height = 25
      Caption = '3d order'
      TabOrder = 2
      OnClick = BitBtn1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 231
    Width = 281
    Height = 82
    Caption = 'Vector average computation'
    TabOrder = 5
    object Label5: TLabel
      Left = 78
      Top = 55
      Width = 188
      Height = 13
      Caption = 'Avg aspect region size (DEM grid posts)'
    end
    object BitBtn3: TBitBtn
      Left = 16
      Top = 24
      Width = 161
      Height = 25
      Caption = 'Vector averages'
      TabOrder = 0
      OnClick = BitBtn3Click
    end
    object Edit4: TEdit
      Left = 9
      Top = 55
      Width = 63
      Height = 21
      TabOrder = 1
      OnChange = Edit4Change
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 19
    Top = 8
    Width = 185
    Height = 113
    Caption = 'Drainage directions'
    Items.Strings = (
      'LSQ 3x3 2d order polynomial'
      'LSQ 5x5 3d order polynomial'
      'Both LSQ polynomials'
      'None')
    TabOrder = 6
    OnClick = RadioGroup1Click
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 328
    Width = 103
    Height = 25
    Caption = 'Legend'
    TabOrder = 7
  end
end
