object MapRtForm: TMapRtForm
  Left = 0
  Top = 0
  Caption = 'Route calculator'
  ClientHeight = 259
  ClientWidth = 498
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 121
    Top = 11
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 121
    Top = 43
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 33
    Top = 84
    Width = 3
    Height = 13
  end
  object Label4: TLabel
    Left = 9
    Top = 160
    Width = 82
    Height = 13
    Caption = 'Point spacing (m)'
  end
  object Label5: TLabel
    Left = 9
    Top = 184
    Width = 82
    Height = 13
    Caption = 'Number of points'
  end
  object Label6: TLabel
    Left = 8
    Top = 103
    Width = 60
    Height = 13
    Caption = 'Distance (m)'
  end
  object Label7: TLabel
    Left = 136
    Top = 109
    Width = 41
    Height = 13
    Caption = 'Azimuth '
  end
  object BitBtn1: TBitBtn
    Left = 10
    Top = 6
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 10
    Top = 38
    Width = 75
    Height = 25
    Caption = 'End'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 91
    Top = 7
    Width = 24
    Height = 25
    Caption = '+'
    TabOrder = 2
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 91
    Top = 38
    Width = 24
    Height = 25
    Caption = '+'
    TabOrder = 3
    OnClick = BitBtn4Click
  end
  object Edit1: TEdit
    Left = 113
    Top = 152
    Width = 57
    Height = 21
    TabOrder = 4
    Text = '1000'
    OnChange = Edit1Change
  end
  object BitBtn5: TBitBtn
    Left = 8
    Top = 211
    Width = 75
    Height = 25
    Caption = 'Rhumb line'
    Enabled = False
    TabOrder = 5
    OnClick = BitBtn5Click
  end
  object Edit2: TEdit
    Left = 113
    Top = 184
    Width = 57
    Height = 21
    Enabled = False
    TabOrder = 6
    Text = '1000'
    OnChange = Edit2Change
  end
  object RadioGroup1: TRadioGroup
    Left = 193
    Top = 144
    Width = 121
    Height = 61
    Caption = 'Pick'
    ItemIndex = 0
    Items.Strings = (
      'Point spacing'
      'Number of points')
    TabOrder = 7
    OnClick = RadioGroup1Click
  end
  object CheckBox1: TCheckBox
    Left = 113
    Top = 211
    Width = 145
    Height = 17
    Caption = 'Topographic profile'
    TabOrder = 8
  end
  object Edit3: TEdit
    Left = 81
    Top = 103
    Width = 49
    Height = 21
    TabOrder = 9
    OnChange = Edit3Change
  end
  object Edit4: TEdit
    Left = 183
    Top = 101
    Width = 49
    Height = 21
    TabOrder = 10
    OnChange = Edit4Change
  end
  object BitBtn6: TBitBtn
    Left = 249
    Top = 98
    Width = 56
    Height = 25
    Caption = '+ End'
    Enabled = False
    TabOrder = 11
    OnClick = BitBtn6Click
  end
  object HelpBtn: TBitBtn
    Left = 237
    Top = 210
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 12
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Memo1: TMemo
    Left = 336
    Top = 8
    Width = 160
    Height = 189
    TabOrder = 13
  end
end
