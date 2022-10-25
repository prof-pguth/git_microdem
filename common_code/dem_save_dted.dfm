object Dted_save_form: TDted_save_form
  Left = 385
  Top = 128
  BorderStyle = bsDialog
  Caption = 'DTED save parameters'
  ClientHeight = 241
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 110
    Height = 13
    Caption = 'Lat spacing (seconds): '
  end
  object Label2: TLabel
    Left = 16
    Top = 40
    Width = 116
    Height = 13
    Caption = 'Long spacing (seconds):'
  end
  object Label3: TLabel
    Left = 24
    Top = 104
    Width = 36
    Height = 13
    Caption = 'SW Lat'
  end
  object Label4: TLabel
    Left = 24
    Top = 136
    Width = 45
    Height = 13
    Caption = 'SW Long'
  end
  object Label5: TLabel
    Left = 16
    Top = 168
    Width = 47
    Height = 13
    Caption = 'Null value'
  end
  object Edit1: TEdit
    Left = 144
    Top = 16
    Width = 81
    Height = 21
    TabOrder = 0
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 144
    Top = 40
    Width = 81
    Height = 21
    Enabled = False
    TabOrder = 1
  end
  object FullCellCheckBox: TCheckBox
    Left = 16
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Force 1'#176' cell'
    TabOrder = 2
    OnClick = FullCellCheckBoxClick
  end
  object Edit3: TEdit
    Left = 88
    Top = 104
    Width = 65
    Height = 21
    Enabled = False
    TabOrder = 3
  end
  object Edit4: TEdit
    Left = 88
    Top = 136
    Width = 65
    Height = 21
    Enabled = False
    TabOrder = 4
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 215
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    IsControl = True
  end
  object Edit5: TEdit
    Left = 88
    Top = 168
    Width = 65
    Height = 21
    TabOrder = 6
  end
  object CheckBox2: TCheckBox
    Left = 144
    Top = 80
    Width = 97
    Height = 17
    Caption = 'Equal spacing'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = CheckBox2Click
  end
  object GroupBox1: TGroupBox
    Left = 320
    Top = 8
    Width = 225
    Height = 193
    Caption = 'Metadata'
    TabOrder = 8
    object StringGrid1: TStringGrid
      Left = 2
      Top = 15
      Width = 221
      Height = 176
      Align = alClient
      ColCount = 2
      DefaultColWidth = 98
      RowCount = 10
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
      ScrollBars = ssVertical
      TabOrder = 0
      ColWidths = (
        98
        98)
      RowHeights = (
        24
        24
        24
        24
        24
        24
        24
        24
        24
        24)
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 176
    Top = 104
    Width = 105
    Height = 105
    Caption = 'DTED Level '
    ItemIndex = 4
    Items.Strings = (
      '0'
      '1'
      '2'
      '3'
      'Arbitrary')
    TabOrder = 9
    OnClick = RadioGroup1Click
  end
end
