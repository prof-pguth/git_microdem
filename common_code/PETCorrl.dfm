object CorrelationForm: TCorrelationForm
  Left = 644
  Top = 266
  ActiveControl = HelpBtn
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Variable Correlations'
  ClientHeight = 243
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Menu = MainMenu1
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 0
    Width = 321
    Height = 241
    Shape = bsFrame
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 240
    Top = 252
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    IsControl = True
  end
  object Button1: TButton
    Left = 24
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Histogram'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 128
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Scattergram'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 24
    Top = 8
    Width = 273
    Height = 21
    TabOrder = 3
    Text = 'ComboBox1'
  end
  object ComboBox2: TComboBox
    Left = 24
    Top = 40
    Width = 273
    Height = 21
    TabOrder = 4
    Text = 'ComboBox2'
  end
  object ComboBox3: TComboBox
    Left = 24
    Top = 96
    Width = 273
    Height = 21
    Enabled = False
    TabOrder = 5
    Text = 'ComboBox3'
  end
  object CheckBox1: TCheckBox
    Left = 48
    Top = 72
    Width = 217
    Height = 17
    Caption = 'Color Code Scattergrams'
    TabOrder = 6
    OnClick = CheckBox1Click
  end
  object MainMenu1: TMainMenu
    AutoMerge = True
    Left = 280
    Top = 136
    object Statistics1: TMenuItem
      Caption = '&View'
      GroupIndex = 1
      object Histogram1: TMenuItem
        Caption = '&Histogram'
        OnClick = Histogram1Click
      end
      object Scattergram1: TMenuItem
        Caption = '&Scattergram'
        OnClick = Scattergram1Click
      end
    end
  end
end
