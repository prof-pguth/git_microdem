object GridTimeSeriesControlForm: TGridTimeSeriesControlForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Monthly series'
  ClientHeight = 272
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 255
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Grids'
  end
  object Label2: TLabel
    Left = 255
    Top = 130
    Width = 51
    Height = 13
    Caption = 'Databases'
  end
  object BitBtn22: TBitBtn
    Left = 43
    Top = 106
    Width = 27
    Height = 20
    Caption = '>'
    TabOrder = 0
    OnClick = BitBtn22Click
  end
  object RadioGroup8: TRadioGroup
    Left = 5
    Top = 7
    Width = 244
    Height = 90
    Caption = 'Month'
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'Jan'
      'Feb'
      'Mar'
      'Apr'
      'May'
      'Jun'
      'Jul'
      'Aug'
      'Sep'
      'Oct'
      'Nov'
      'Dec')
    TabOrder = 1
    OnClick = RadioGroup8Click
  end
  object BitBtn21: TBitBtn
    Left = 11
    Top = 106
    Width = 26
    Height = 20
    Caption = '<'
    TabOrder = 2
    OnClick = BitBtn21Click
  end
  object CheckBox1: TCheckBox
    Left = 271
    Top = 20
    Width = 204
    Height = 17
    Caption = 'CheckBox1'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 271
    Top = 43
    Width = 204
    Height = 17
    Caption = 'CheckBox2'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 271
    Top = 66
    Width = 204
    Height = 17
    Caption = 'CheckBox3'
    TabOrder = 5
    OnClick = CheckBox3Click
  end
  object BitBtn1: TBitBtn
    Left = 152
    Top = 103
    Width = 97
    Height = 25
    Caption = 'Load another'
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object CheckBox4: TCheckBox
    Left = 271
    Top = 142
    Width = 204
    Height = 17
    Caption = 'CheckBox4'
    TabOrder = 7
  end
  object CheckBox5: TCheckBox
    Left = 271
    Top = 166
    Width = 204
    Height = 17
    Caption = 'CheckBox5'
    TabOrder = 8
  end
  object CheckBox6: TCheckBox
    Left = 271
    Top = 189
    Width = 204
    Height = 17
    Caption = 'CheckBox6'
    TabOrder = 9
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 134
    Width = 161
    Height = 49
    Caption = 'Thin DB to plot'
    Columns = 5
    ItemIndex = 0
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5')
    TabOrder = 10
  end
  object CheckBox7: TCheckBox
    Left = 175
    Top = 151
    Width = 74
    Height = 17
    Caption = 'Legends'
    Checked = True
    State = cbChecked
    TabOrder = 11
  end
  object BitBtn2: TBitBtn
    Left = 120
    Top = 212
    Width = 75
    Height = 25
    Caption = 'Annual sum'
    TabOrder = 12
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 201
    Top = 212
    Width = 75
    Height = 25
    Caption = 'Annual mean'
    TabOrder = 13
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 311
    Top = 212
    Width = 106
    Height = 25
    Caption = 'Close climatologies'
    TabOrder = 14
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 48
    Top = 212
    Width = 62
    Height = 25
    Caption = '>>'
    TabOrder = 15
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 8
    Top = 212
    Width = 34
    Height = 25
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000120B0000120B00001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADAD7000007
      DADAADAD019999910DADDAD09999999990DAAD0999999999990DD71999999999
      9917A0999FF999FF9990D09999FF9FF99990A099999FFF999990D099999FFF99
      9990A09999FF9FF99990D7199FF999FF9917AD0999999999990DDAD099999999
      90DAADAD019999910DADDADAD7000007DADAADADADADADADADAD}
    TabOrder = 16
    OnClick = BitBtn6Click
  end
  object TrackBar1: TTrackBar
    Left = 8
    Top = 243
    Width = 150
    Height = 45
    Max = 25
    Position = 5
    TabOrder = 17
    OnChange = TrackBar1Change
  end
  object BitBtn7: TBitBtn
    Left = 204
    Top = 243
    Width = 133
    Height = 25
    Caption = 'Annual Max/Min/Range'
    TabOrder = 18
    OnClick = BitBtn7Click
  end
  object BitBtn8: TBitBtn
    Left = 343
    Top = 243
    Width = 90
    Height = 25
    Caption = 'Month Min/Max'
    TabOrder = 19
    OnClick = BitBtn8Click
  end
  object CheckBox8: TCheckBox
    Left = 271
    Top = 89
    Width = 97
    Height = 17
    Caption = 'CheckBox8'
    TabOrder = 20
    OnClick = CheckBox8Click
  end
  object CheckBox9: TCheckBox
    Left = 272
    Top = 111
    Width = 97
    Height = 17
    Caption = 'CheckBox9'
    TabOrder = 21
    OnClick = CheckBox9Click
  end
  object BitBtn9: TBitBtn
    Left = 439
    Top = 242
    Width = 42
    Height = 25
    Caption = 'Missing'
    TabOrder = 22
    OnClick = BitBtn9Click
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 432
    Top = 184
  end
end
