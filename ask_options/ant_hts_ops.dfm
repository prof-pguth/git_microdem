object ReqAntOptsForm: TReqAntOptsForm
  Left = 829
  Top = 253
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Required Antenna Height'
  ClientHeight = 238
  ClientWidth = 239
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 91
    Height = 13
    Caption = 'Antenna Height (m)'
  end
  object Label2: TLabel
    Left = 16
    Top = 32
    Width = 87
    Height = 13
    Caption = 'Antenna range (m)'
  end
  object Label3: TLabel
    Left = 16
    Top = 56
    Width = 119
    Height = 13
    Caption = 'Minimum vertical standoff'
  end
  object Label4: TLabel
    Left = 109
    Top = 172
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Edit1: TEdit
    Left = 152
    Top = 8
    Width = 65
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 152
    Top = 32
    Width = 65
    Height = 21
    TabOrder = 1
    Text = 'Edit2'
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 80
    Width = 193
    Height = 17
    Caption = 'Map of required antenna height'
    TabOrder = 2
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 96
    Width = 161
    Height = 17
    Caption = 'Map of required flying height'
    TabOrder = 3
  end
  object Edit3: TEdit
    Left = 152
    Top = 56
    Width = 65
    Height = 21
    TabOrder = 4
    Text = 'Edit3'
  end
  object BitBtn4: TBitBtn
    Left = 77
    Top = 204
    Width = 58
    Height = 25
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 5
    OnClick = BitBtn4Click
    IsControl = True
  end
  object BitBtn5: TBitBtn
    Left = 8
    Top = 204
    Width = 49
    Height = 25
    Caption = 'OK'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      555555555555555555555555555555555555555555FF55555555555559055555
      55555555577FF5555555555599905555555555557777F5555555555599905555
      555555557777FF5555555559999905555555555777777F555555559999990555
      5555557777777FF5555557990599905555555777757777F55555790555599055
      55557775555777FF5555555555599905555555555557777F5555555555559905
      555555555555777FF5555555555559905555555555555777FF55555555555579
      05555555555555777FF5555555555557905555555555555777FF555555555555
      5990555555555555577755555555555555555555555555555555}
    NumGlyphs = 2
    TabOrder = 6
    OnClick = BitBtn5Click
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 167
    Width = 95
    Height = 25
    Caption = 'Vert earth curve'
    TabOrder = 7
    OnClick = BitBtn1Click
  end
  object CheckBox3: TCheckBox
    Left = 16
    Top = 144
    Width = 97
    Height = 17
    Caption = 'LOS profile'
    TabOrder = 8
  end
  object CheckBox4: TCheckBox
    Left = 16
    Top = 112
    Width = 145
    Height = 17
    Caption = 'Map of grazing angle'
    TabOrder = 9
  end
  object CheckBox5: TCheckBox
    Left = 16
    Top = 128
    Width = 145
    Height = 17
    Caption = 'Map of earth curvature'
    TabOrder = 10
  end
end
