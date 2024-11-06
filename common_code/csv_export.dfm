object CVSExportForm: TCVSExportForm
  Left = 839
  Top = 231
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Export CSV Grid'
  ClientHeight = 420
  ClientWidth = 362
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
    Left = 152
    Top = 112
    Width = 21
    Height = 13
    Caption = 'dLat'
  end
  object Label2: TLabel
    Left = 152
    Top = 144
    Width = 30
    Height = 13
    Caption = 'dLong'
  end
  object Label3: TLabel
    Left = 106
    Top = 303
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 32
    Top = 166
    Width = 5
    Height = 13
    Caption = 'z'
    Enabled = False
    FocusControl = BitBtn1
  end
  object Edit1: TEdit
    Left = 88
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '45'
  end
  object Edit2: TEdit
    Left = 88
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '44'
  end
  object Edit3: TEdit
    Left = 16
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '78'
  end
  object Edit4: TEdit
    Left = 168
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '79'
  end
  object Edit5: TEdit
    Left = 184
    Top = 112
    Width = 89
    Height = 21
    TabOrder = 4
    Text = '1'
  end
  object Edit6: TEdit
    Left = 188
    Top = 139
    Width = 89
    Height = 21
    TabOrder = 5
    Text = '1'
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 120
    Width = 97
    Height = 17
    Caption = 'Record number'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object BitBtn1: TBitBtn
    Left = 25
    Top = 329
    Width = 75
    Height = 25
    Caption = 'Write file'
    TabOrder = 7
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 25
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 8
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 168
    Top = 364
    Width = 77
    Height = 25
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 9
    OnClick = BitBtn3Click
    IsControl = True
  end
  object BitBtn4: TBitBtn
    Left = 25
    Top = 298
    Width = 75
    Height = 25
    Caption = 'File Name'
    TabOrder = 10
    OnClick = BitBtn4Click
  end
  object Edit7: TEdit
    Left = 43
    Top = 166
    Width = 57
    Height = 21
    Enabled = False
    TabOrder = 11
    Text = '0'
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 404
    Width = 362
    Height = 16
    Align = alBottom
    TabOrder = 12
    ExplicitTop = 403
    ExplicitWidth = 358
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 143
    Width = 121
    Height = 17
    Caption = 'Constant elevations'
    TabOrder = 13
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 16
    Top = 193
    Width = 157
    Height = 17
    Caption = 'Datum shift, geographic'
    TabOrder = 14
  end
  object CheckBox4: TCheckBox
    Left = 16
    Top = 216
    Width = 113
    Height = 17
    Caption = 'Datum shift, UTM'
    TabOrder = 15
  end
  object CheckBox5: TCheckBox
    Left = 16
    Top = 239
    Width = 137
    Height = 17
    Caption = 'Map distortion (h and k)'
    TabOrder = 16
  end
  object CheckBox6: TCheckBox
    Left = 156
    Top = 280
    Width = 141
    Height = 17
    Caption = 'Header line with labels'
    Checked = True
    State = cbChecked
    TabOrder = 17
  end
  object CheckBox7: TCheckBox
    Left = 16
    Top = 264
    Width = 134
    Height = 17
    Caption = 'Magnetic declination'
    TabOrder = 18
  end
end
