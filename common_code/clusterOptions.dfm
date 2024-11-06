object ClusterOptsForm: TClusterOptsForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Clustering Options'
  ClientHeight = 392
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 13
  object Label1: TLabel
    Left = 119
    Top = 16
    Width = 39
    Height = 13
    Caption = 'Clusters'
  end
  object Label2: TLabel
    Left = 119
    Top = 58
    Width = 47
    Height = 13
    Caption = 'Iterations'
  end
  object Label3: TLabel
    Left = 124
    Top = 85
    Width = 42
    Height = 13
    Caption = 'Sampling'
  end
  object Label4: TLabel
    Left = 135
    Top = 36
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 144
    Top = 112
    Width = 31
    Height = 13
    Caption = 'Label5'
  end
  object Label6: TLabel
    Left = 165
    Top = 132
    Width = 16
    Height = 13
    Caption = 'Min'
  end
  object Label7: TLabel
    Left = 226
    Top = 132
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object HelpBtn: TBitBtn
    Left = 183
    Top = 356
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
    Top = 356
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
    OnClick = OKBtnClick
    IsControl = True
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 97
    Height = 97
    Caption = 'Initialization'
    Items.Strings = (
      'MinMax'
      'StdDev'
      'Random')
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 172
    Top = 8
    Width = 69
    Height = 21
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 172
    Top = 55
    Width = 69
    Height = 21
    TabOrder = 4
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 175
    Width = 137
    Height = 17
    Caption = 'Scatterplots by cluster'
    TabOrder = 5
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 198
    Width = 137
    Height = 17
    Caption = 'Scatterplots by mask'
    TabOrder = 6
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 216
    Width = 137
    Height = 17
    Caption = 'Histograms by cluster'
    TabOrder = 7
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 235
    Width = 121
    Height = 17
    Caption = 'Histograms by mask'
    TabOrder = 8
  end
  object CheckBox6: TCheckBox
    Left = 165
    Top = 198
    Width = 97
    Height = 17
    Caption = 'Cluster results'
    Enabled = False
    TabOrder = 9
  end
  object CheckBox7: TCheckBox
    Left = 165
    Top = 217
    Width = 97
    Height = 17
    Caption = 'Cluster statistics'
    Enabled = False
    TabOrder = 10
  end
  object Edit3: TEdit
    Left = 172
    Top = 84
    Width = 69
    Height = 21
    TabOrder = 11
  end
  object CancelBtn: TBitBtn
    Left = 91
    Top = 356
    Width = 86
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 12
    OnClick = CancelBtnClick
    IsControl = True
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 297
    Width = 254
    Height = 40
    Caption = 'Classification distance power'
    Columns = 6
    Items.Strings = (
      '0.5'
      '1'
      '1.5'
      '2'
      '2.5'
      '3')
    TabOrder = 13
    OnClick = RadioGroup2Click
  end
  object CheckBox5: TCheckBox
    Left = 16
    Top = 264
    Width = 129
    Height = 17
    Caption = 'Samples from full map'
    TabOrder = 14
  end
  object CheckBox8: TCheckBox
    Left = 165
    Top = 264
    Width = 97
    Height = 17
    Caption = 'Classify full map'
    TabOrder = 15
  end
  object CheckBox9: TCheckBox
    Left = 8
    Top = 131
    Width = 161
    Height = 17
    Caption = 'Number cluster sensitivity'
    TabOrder = 16
  end
  object Edit4: TEdit
    Left = 187
    Top = 129
    Width = 33
    Height = 21
    TabOrder = 17
    Text = 'Edit4'
  end
  object Edit5: TEdit
    Left = 252
    Top = 129
    Width = 37
    Height = 21
    TabOrder = 18
    Text = 'Edit5'
  end
end
