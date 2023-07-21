object RGB_form: TRGB_form
  Left = 0
  Top = 0
  Caption = 'RGB 3 parameter map coloring'
  ClientHeight = 258
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 128
    Width = 584
    Height = 130
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 95
    ExplicitWidth = 580
    object Label11: TLabel
      Left = 277
      Top = 69
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object Label10: TLabel
      Left = 175
      Top = 69
      Width = 16
      Height = 13
      Caption = 'Min'
    end
    object Edit8: TEdit
      Left = 303
      Top = 69
      Width = 65
      Height = 21
      TabOrder = 0
    end
    object Edit7: TEdit
      Left = 197
      Top = 69
      Width = 65
      Height = 21
      TabOrder = 1
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 52
      Width = 153
      Height = 33
      Caption = 'Initial ranges'
      Columns = 2
      ItemIndex = 1
      Items.Strings = (
        'Grids'
        'Percentiles')
      TabOrder = 2
      OnClick = RadioGroup1Click
    end
    object RadioGroup2: TRadioGroup
      Left = 8
      Top = 12
      Width = 313
      Height = 34
      Caption = 'Display'
      Columns = 4
      Enabled = False
      ItemIndex = 0
      Items.Strings = (
        'Composite'
        'Red'
        'Green'
        'Blue')
      TabOrder = 3
      OnClick = RadioGroup2Click
    end
    object BitBtn1: TBitBtn
      Left = 8
      Top = 91
      Width = 75
      Height = 25
      Caption = 'Plot'
      TabOrder = 4
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 89
      Top = 91
      Width = 118
      Height = 25
      Caption = 'Color range'
      TabOrder = 5
      OnClick = BitBtn2Click
    end
    object BitBtn3: TBitBtn
      Left = 218
      Top = 96
      Width = 63
      Height = 25
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      TabOrder = 6
      OnClick = BitBtn3Click
    end
    object HelpBtn: TBitBtn
      Left = 287
      Top = 96
      Width = 77
      Height = 25
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 7
      OnClick = HelpBtnClick
      IsControl = True
    end
    object CheckBox1: TCheckBox
      Left = 327
      Top = 24
      Width = 115
      Height = 17
      Caption = 'Grayscale channels'
      TabOrder = 8
      OnClick = CheckBox1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 207
    Height = 128
    Align = alLeft
    Caption = ' '
    TabOrder = 1
    ExplicitHeight = 95
    object Label5: TLabel
      Left = 8
      Top = 88
      Width = 16
      Height = 13
      Caption = 'Min'
    end
    object Label4: TLabel
      Left = 8
      Top = 54
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object Label1: TLabel
      Left = 5
      Top = 28
      Width = 19
      Height = 13
      Caption = 'Red'
    end
    object ComboBox1: TComboBox
      Left = 1
      Top = 1
      Width = 205
      Height = 21
      Align = alTop
      TabOrder = 0
      OnChange = ComboBox1Change
    end
    object Edit2: TEdit
      Left = 34
      Top = 85
      Width = 89
      Height = 21
      TabOrder = 1
      OnChange = Edit2Change
    end
    object Edit1: TEdit
      Left = 34
      Top = 54
      Width = 89
      Height = 21
      TabOrder = 2
      OnChange = Edit1Change
    end
  end
  object Panel3: TPanel
    Left = 207
    Top = 0
    Width = 176
    Height = 128
    Align = alClient
    Caption = ' '
    TabOrder = 2
    ExplicitWidth = 172
    ExplicitHeight = 95
    object Label13: TLabel
      Left = 24
      Top = 54
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object Label14: TLabel
      Left = 0
      Top = 35
      Width = 29
      Height = 13
      Caption = 'Green'
    end
    object Label12: TLabel
      Left = 24
      Top = 88
      Width = 16
      Height = 13
      Caption = 'Min'
    end
    object Edit3: TEdit
      Left = 50
      Top = 54
      Width = 89
      Height = 21
      TabOrder = 0
      OnChange = Edit3Change
    end
    object ComboBox2: TComboBox
      Left = 1
      Top = 1
      Width = 174
      Height = 21
      Align = alTop
      TabOrder = 1
      OnChange = ComboBox2Change
      ExplicitWidth = 170
    end
    object Edit4: TEdit
      Left = 50
      Top = 81
      Width = 89
      Height = 21
      TabOrder = 2
      OnChange = Edit4Change
    end
  end
  object Panel4: TPanel
    Left = 383
    Top = 0
    Width = 201
    Height = 128
    Align = alRight
    Caption = ' '
    TabOrder = 3
    ExplicitLeft = 379
    ExplicitHeight = 95
    object Label9: TLabel
      Left = 23
      Top = 88
      Width = 16
      Height = 13
      Caption = 'Min'
    end
    object Label8: TLabel
      Left = 23
      Top = 54
      Width = 20
      Height = 13
      Caption = 'Max'
    end
    object Label3: TLabel
      Left = 7
      Top = 35
      Width = 20
      Height = 13
      Caption = 'Blue'
    end
    object ComboBox3: TComboBox
      Left = 1
      Top = 1
      Width = 199
      Height = 21
      Align = alTop
      TabOrder = 0
      OnChange = ComboBox3Change
    end
    object Edit6: TEdit
      Left = 49
      Top = 85
      Width = 89
      Height = 21
      TabOrder = 1
      OnChange = Edit6Change
    end
    object Edit5: TEdit
      Left = 49
      Top = 54
      Width = 89
      Height = 21
      TabOrder = 2
      OnChange = Edit5Change
    end
  end
end
