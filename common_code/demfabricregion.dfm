object FabricOptions: TFabricOptions
  Left = 1972
  Top = 233
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Fabric by Region Size'
  ClientHeight = 312
  ClientWidth = 355
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
  object Label3: TLabel
    Left = 11
    Top = 118
    Width = 56
    Height = 13
    Caption = 'First box (m)'
  end
  object Label4: TLabel
    Left = 11
    Top = 145
    Width = 57
    Height = 13
    Caption = 'Last box (m)'
  end
  object Label1: TLabel
    Left = 187
    Top = 120
    Width = 70
    Height = 13
    Caption = 'Points required'
  end
  object Label2: TLabel
    Left = 202
    Top = 185
    Width = 95
    Height = 13
    Caption = 'Point Separation (m)'
  end
  object Label5: TLabel
    Left = 202
    Top = 216
    Width = 59
    Height = 13
    Caption = 'Grid thinning'
  end
  object CheckBox1: TCheckBox
    Left = 192
    Top = 8
    Width = 161
    Height = 17
    Caption = 'Nets with grain fabric'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 8
    Width = 129
    Height = 17
    Caption = 'Graphs of flatness'
    TabOrder = 1
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 31
    Width = 161
    Height = 17
    Caption = 'Graphs of organization'
    TabOrder = 2
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 54
    Width = 153
    Height = 17
    Caption = 'Graphs of fabric orientation'
    TabOrder = 3
    OnClick = CheckBox4Click
  end
  object OKBtn: TButton
    Left = 121
    Top = 286
    Width = 75
    Height = 27
    Caption = 'OK (done)'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = OKBtnClick
  end
  object HelpBtn: TBitBtn
    Left = 220
    Top = 286
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CheckBox5: TCheckBox
    Left = 192
    Top = 31
    Width = 145
    Height = 17
    Caption = 'Aspect rose diagrams'
    TabOrder = 6
    OnClick = CheckBox5Click
  end
  object CheckBox6: TCheckBox
    Left = 8
    Top = 77
    Width = 113
    Height = 17
    Caption = 'Grain relief graph'
    TabOrder = 7
    OnClick = CheckBox6Click
  end
  object Edit2: TEdit
    Left = 83
    Top = 117
    Width = 41
    Height = 21
    TabOrder = 8
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 83
    Top = 144
    Width = 41
    Height = 21
    TabOrder = 9
    OnChange = Edit3Change
  end
  object BitBtn1: TBitBtn
    Left = 16
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    TabOrder = 10
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 16
    Top = 287
    Width = 75
    Height = 25
    Caption = 'New point'
    TabOrder = 11
    OnClick = BitBtn2Click
  end
  object Edit1: TEdit
    Left = 263
    Top = 115
    Width = 73
    Height = 21
    TabOrder = 12
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object BitBtn3: TBitBtn
    Left = 8
    Top = 180
    Width = 188
    Height = 25
    Caption = 'Most organized region size (DB)'
    TabOrder = 13
    OnClick = BitBtn3Click
  end
  object Edit5: TEdit
    Left = 310
    Top = 182
    Width = 43
    Height = 21
    TabOrder = 14
    Text = ' '
    OnChange = Edit5Change
  end
  object BitBtn4: TBitBtn
    Left = 8
    Top = 211
    Width = 188
    Height = 25
    Caption = 'Most organized region size (grids)'
    TabOrder = 15
    OnClick = BitBtn4Click
  end
  object Edit4: TEdit
    Left = 287
    Top = 209
    Width = 66
    Height = 21
    TabOrder = 16
    Text = 'Edit4'
    OnChange = Edit4Change
  end
end
