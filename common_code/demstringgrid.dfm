object GridForm: TGridForm
  Left = 100
  Top = 261
  Caption = 'GridForm'
  ClientHeight = 597
  ClientWidth = 1368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object StringGrid1: TStringGrid
    Left = 0
    Top = 41
    Width = 1368
    Height = 556
    Align = alClient
    DefaultColWidth = 100
    TabOrder = 0
    ColWidths = (
      100
      100
      100
      100
      100)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1368
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 785
      Top = 22
      Width = 45
      Height = 13
      Caption = 'Cell width'
    end
    object BitBtn1: TBitBtn
      Left = 526
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Highlight'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object Edit1: TEdit
      Left = 607
      Top = 14
      Width = 49
      Height = 21
      TabOrder = 1
      Text = '0.90'
    end
    object BitBtn2: TBitBtn
      Left = 8
      Top = 10
      Width = 41
      Height = 25
      Caption = 'Font'
      TabOrder = 2
      OnClick = BitBtn2Click
    end
    object BitBtn3: TBitBtn
      Left = 159
      Top = 10
      Width = 34
      Height = 25
      Caption = 'HTML'
      TabOrder = 3
      OnClick = BitBtn3Click
    end
    object BitBtn4: TBitBtn
      Left = 662
      Top = 10
      Width = 89
      Height = 25
      Caption = 'Text Highlights'
      TabOrder = 4
      OnClick = BitBtn4Click
    end
    object Edit2: TEdit
      Left = 844
      Top = 14
      Width = 28
      Height = 21
      TabOrder = 5
      Text = '100'
    end
    object BitBtn5: TBitBtn
      Left = 119
      Top = 10
      Width = 34
      Height = 25
      Caption = 'CSV'
      TabOrder = 6
      OnClick = BitBtn5Click
    end
    object BitBtn6: TBitBtn
      Left = 407
      Top = 10
      Width = 57
      Height = 25
      Caption = 'Diagram'
      TabOrder = 7
      OnClick = BitBtn6Click
    end
    object BitBtn7: TBitBtn
      Left = 470
      Top = 10
      Width = 50
      Height = 25
      Caption = 'Graph'
      TabOrder = 8
      OnClick = BitBtn7Click
    end
    object CheckBox1: TCheckBox
      Left = 878
      Top = 18
      Width = 97
      Height = 17
      Caption = 'Autosize'
      TabOrder = 9
      OnClick = CheckBox1Click
    end
    object BitBtn8: TBitBtn
      Left = 303
      Top = 10
      Width = 46
      Height = 25
      Caption = '123'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADA0EFE0ADADADDADAD0FEF0DADADAADADA0EFE0ADADADDADAD0FEF0DA
        DADAADADA0EFE0ADADADDADAD00000DADADAADADADA4ADADADADDADADAD4DADA
        DADAADADA44444ADADADDADADA444ADADADAADADADA4ADADADADD00000DAD000
        00DAA0FFF0ADA0FFF0ADD0FFF0DAD0FFF0DAA0FFF0ADA0FFF0AD}
      TabOrder = 10
      OnClick = BitBtn8Click
    end
    object BitBtn9: TBitBtn
      Left = 355
      Top = 10
      Width = 46
      Height = 25
      Caption = '123'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADADA4ADADADADDADADA444ADADADAADADA44444ADADADDADADAD4DADA
        DADAADADADA4ADADADADDADAD00000DADADAADADA0EFE0ADADADDADAD0FEF0DA
        DADAADADA0EFE0ADADADDADAD0FEF0DADADAA00000EFE00000ADD0FFF0FEF0FF
        F0DAA0FFF0EFE0FFF0ADD0FFF0FEF0FFF0DAADADADADADADADAD}
      TabOrder = 11
      OnClick = BitBtn9Click
    end
    object BitBtn19: TBitBtn
      Left = 199
      Top = 10
      Width = 46
      Height = 25
      Caption = 'XYZ'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADA0EFE0ADADADDADAD0FEF0DADADAADADA0EFE0ADADADDADAD0FEF0DA
        DADAADADA0EFE0ADADADDADAD00000DADADAADADADA4ADADADADDADADAD4DADA
        DADAADADA44444ADADADDADADA444ADADADAADADADA4ADADADADD00000DAD000
        00DAA0FFF0ADA0FFF0ADD0FFF0DAD0FFF0DAA0FFF0ADA0FFF0AD}
      TabOrder = 12
      OnClick = BitBtn8Click
    end
    object BitBtn20: TBitBtn
      Left = 251
      Top = 10
      Width = 46
      Height = 25
      Caption = 'ABC'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADADA4ADADADADDADADA444ADADADAADADA44444ADADADDADADAD4DADA
        DADAADADADA4ADADADADDADAD00000DADADAADADA0EFE0ADADADDADAD0FEF0DA
        DADAADADA0EFE0ADADADDADAD0FEF0DADADAA00000EFE00000ADD0FFF0FEF0FF
        F0DAA0FFF0EFE0FFF0ADD0FFF0FEF0FFF0DAADADADADADADADAD}
      TabOrder = 13
      OnClick = BitBtn9Click
    end
    object CheckBox2: TCheckBox
      Left = 943
      Top = 13
      Width = 97
      Height = 17
      Caption = 'r'#178
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 14
      OnClick = CheckBox2Click
    end
    object BitBtn10: TBitBtn
      Left = 999
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Decimals'
      TabOrder = 15
      OnClick = BitBtn10Click
    end
    object BitBtn11: TBitBtn
      Left = 55
      Top = 10
      Width = 58
      Height = 25
      Caption = 'DBF'
      TabOrder = 16
      OnClick = BitBtn11Click
    end
  end
end
