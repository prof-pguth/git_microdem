object drain_opt_form: Tdrain_opt_form
  Left = 362
  Top = 243
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Drainage flow path options'
  ClientHeight = 439
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 94
    Top = 85
    Width = 129
    Height = 13
    Caption = 'Vector spacing (map pixels)'
  end
  object Label3: TLabel
    Left = 125
    Top = 13
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 22
    Top = 408
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
  object BitBtn1: TBitBtn
    Left = 18
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Slope algorithm'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object Edit2: TEdit
    Left = 19
    Top = 82
    Width = 65
    Height = 21
    TabOrder = 1
    OnChange = Edit2Change
  end
  object HelpBtn: TBitBtn
    Left = 183
    Top = 406
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 94
    Top = 406
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
    TabOrder = 3
    OnClick = OKBtnClick
    IsControl = True
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 39
    Width = 97
    Height = 17
    Caption = 'Show point slope'
    TabOrder = 4
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 62
    Width = 177
    Height = 17
    Caption = 'Show regionvector average'
    TabOrder = 5
    OnClick = CheckBox2Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 109
    Width = 281
    Height = 113
    Caption = 'Single point computation'
    TabOrder = 6
    object Label1: TLabel
      Left = 89
      Top = 84
      Width = 68
      Height = 13
      Caption = 'Vector lengths'
    end
    object Label4: TLabel
      Left = 89
      Top = 55
      Width = 161
      Height = 13
      Caption = 'Slope region size (DEM grid posts)'
    end
    object BitBtn2: TBitBtn
      Left = 18
      Top = 24
      Width = 161
      Height = 25
      Caption = 'Point values'
      TabOrder = 0
      OnClick = BitBtn2Click
    end
    object Edit1: TEdit
      Left = 18
      Top = 84
      Width = 65
      Height = 21
      TabOrder = 1
      OnChange = Edit1Change
    end
    object Edit3: TEdit
      Left = 20
      Top = 57
      Width = 63
      Height = 21
      TabOrder = 2
      Text = 'Edit3'
      OnChange = Edit3Change
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 231
    Width = 281
    Height = 146
    Caption = 'Vector average computation'
    TabOrder = 7
    object Label5: TLabel
      Left = 72
      Top = 76
      Width = 188
      Height = 13
      Caption = 'Avg aspect region size (DEM grid posts)'
    end
    object Label6: TLabel
      Left = 72
      Top = 116
      Width = 68
      Height = 13
      Caption = 'Vector lengths'
    end
    object BitBtn3: TBitBtn
      Left = 20
      Top = 32
      Width = 161
      Height = 25
      Caption = 'Vector averages'
      TabOrder = 0
      OnClick = BitBtn3Click
    end
    object Edit4: TEdit
      Left = 3
      Top = 76
      Width = 63
      Height = 21
      TabOrder = 1
      Text = 'Edit3'
      OnChange = Edit4Change
    end
    object Edit5: TEdit
      Left = 3
      Top = 114
      Width = 63
      Height = 21
      TabOrder = 2
      Text = 'Edit3'
      OnChange = Edit5Change
    end
  end
end
