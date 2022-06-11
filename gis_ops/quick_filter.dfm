object QuickFilterForm: TQuickFilterForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  ClientHeight = 60
  ClientWidth = 610
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 610
    Height = 60
    Align = alClient
    TabOrder = 0
    object qfLabel7: TLabel
      Left = 5
      Top = 9
      Width = 41
      Height = 13
      Caption = 'qfLabel7'
    end
    object qfLabel8: TLabel
      Left = 144
      Top = 9
      Width = 41
      Height = 13
      Caption = 'qfLabel8'
    end
    object qfCheckBox2: TCheckBox
      Left = 279
      Top = 8
      Width = 74
      Height = 17
      Caption = 'Show filter'
      TabOrder = 0
    end
    object qfCheckBox3: TCheckBox
      Left = 279
      Top = 31
      Width = 74
      Height = 17
      Caption = 'Show n='
      TabOrder = 1
    end
    object BitBtn24: TBitBtn
      Left = 359
      Top = 35
      Width = 33
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
      TabOrder = 2
      OnClick = BitBtn24Click
    end
    object BitBtn25: TBitBtn
      Left = 359
      Top = 4
      Width = 30
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333333333333333333333FFF33FFFFF33333300033000
        00333337773377777333333330333300033333337FF33777F333333330733300
        0333333377FFF777F33333333700000073333333777777773333333333033000
        3333333337FF777F333333333307300033333333377F777F3333333333703007
        33333333377F7773333333333330000333333333337777F33333333333300003
        33333333337777F3333333333337007333333333337777333333333333330033
        3333333333377333333333333333033333333333333733333333333333333333
        3333333333333333333333333333333333333333333333333333}
      NumGlyphs = 2
      TabOrder = 3
      OnClick = BitBtn25Click
    end
    object qfRadioGroup1: TRadioGroup
      Left = 398
      Top = 4
      Width = 92
      Height = 35
      Caption = 'Filter'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'AND'
        'OR')
      TabOrder = 4
    end
    object CheckBox4: TCheckBox
      Left = 406
      Top = 40
      Width = 65
      Height = 17
      Caption = 'All DBs'
      TabOrder = 5
      OnClick = CheckBox4Click
    end
    object qf1ComboBox10: TComboBox
      Left = 5
      Top = 28
      Width = 103
      Height = 21
      TabOrder = 6
      OnChange = qf1ComboBox10Change
    end
    object qf2ComboBox9: TComboBox
      Left = 144
      Top = 28
      Width = 101
      Height = 21
      TabOrder = 7
      OnChange = qf2ComboBox9Change
    end
    object GroupBox1: TGroupBox
      Left = 496
      Top = 4
      Width = 105
      Height = 52
      Caption = 'Cycle records'
      TabOrder = 8
      object BitBtn27: TBitBtn
        Left = 57
        Top = 24
        Width = 35
        Height = 25
        Caption = '>'
        TabOrder = 0
        OnClick = BitBtn27Click
      end
      object BitBtn26: TBitBtn
        Left = 16
        Top = 24
        Width = 35
        Height = 25
        Caption = '<'
        TabOrder = 1
        OnClick = BitBtn26Click
      end
    end
    object qfBitBtn1: TBitBtn
      Left = 114
      Top = 21
      Width = 17
      Height = 15
      Caption = '^'
      TabOrder = 9
      OnClick = qfBitBtn1Click
    end
    object qfBitBtn2: TBitBtn
      Left = 114
      Top = 42
      Width = 17
      Height = 15
      Caption = 'V'
      TabOrder = 10
      OnClick = qfBitBtn2Click
    end
    object qfBitBtn3: TBitBtn
      Left = 251
      Top = 24
      Width = 17
      Height = 15
      Caption = '^'
      TabOrder = 11
      OnClick = qfBitBtn3Click
    end
    object qfBitBtn4: TBitBtn
      Left = 251
      Top = 37
      Width = 17
      Height = 15
      Caption = 'V'
      TabOrder = 12
      OnClick = qfBitBtn4Click
    end
  end
end
