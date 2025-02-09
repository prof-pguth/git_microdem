object supclasform: Tsupclasform
  Left = 0
  Top = 0
  Caption = 'Supervised classification'
  ClientHeight = 329
  ClientWidth = 548
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
  object CheckBox1: TCheckBox
    Left = 64
    Top = 160
    Width = 1
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 80
    Width = 548
    Height = 249
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 74
    ExplicitWidth = 517
    object Label1: TLabel
      Left = 8
      Top = 96
      Width = 38
      Height = 13
      Caption = 'Std Dev'
    end
    object Label2: TLabel
      Left = 8
      Top = 141
      Width = 102
      Height = 13
      Caption = 'Bands must be in box'
    end
    object Edit1: TEdit
      Left = 52
      Top = 94
      Width = 81
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
      OnChange = Edit1Change
    end
    object ClassifyButton: TBitBtn
      Left = 8
      Top = 209
      Width = 97
      Height = 25
      Caption = 'Classify'
      Enabled = False
      TabOrder = 1
      OnClick = ClassifyButtonClick
    end
    object HelpBtn: TBitBtn
      Left = 128
      Top = 209
      Width = 105
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 2
      OnClick = HelpBtnClick
      IsControl = True
    end
    object TrackBar1: TTrackBar
      Left = 301
      Top = 139
      Width = 125
      Height = 32
      Enabled = False
      Max = 100
      Frequency = 10
      TabOrder = 3
      Visible = False
      OnChange = TrackBar1Change
    end
    object CheckBox6: TCheckBox
      Left = 301
      Top = 23
      Width = 97
      Height = 17
      Caption = 'Show results DB'
      TabOrder = 4
      OnClick = CheckBox6Click
    end
    object RadioGroup2: TRadioGroup
      Left = 8
      Top = 163
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
      TabOrder = 5
      OnClick = RadioGroup2Click
    end
    object Edit2: TEdit
      Left = 116
      Top = 139
      Width = 49
      Height = 21
      TabOrder = 6
      Text = 'Edit2'
      OnChange = Edit2Change
    end
    object RadioGroup1: TRadioGroup
      Left = 156
      Top = 7
      Width = 121
      Height = 126
      Caption = 'Image class limits'
      ItemIndex = 0
      Items.Strings = (
        'Min/Max'
        '5/95'
        '10/90'
        '25/75'
        'Mean/Std'
        'User')
      TabOrder = 7
      OnClick = RadioGroup1Click
    end
    object BitBtn7: TBitBtn
      Left = 301
      Top = 177
      Width = 137
      Height = 25
      Caption = 'Classes included'
      Enabled = False
      TabOrder = 8
      OnClick = BitBtn7Click
    end
    object BitBtn9: TBitBtn
      Left = 301
      Top = 46
      Width = 140
      Height = 25
      Caption = 'Grid, dist class centroid'
      Enabled = False
      TabOrder = 9
      OnClick = BitBtn9Click
    end
    object BitBtn10: TBitBtn
      Left = 301
      Top = 77
      Width = 140
      Height = 25
      Caption = 'Grid, bands in class limits'
      Enabled = False
      TabOrder = 10
      OnClick = BitBtn10Click
    end
    object BitBtn1: TBitBtn
      Left = 301
      Top = 108
      Width = 140
      Height = 25
      Caption = 'Grids, slope and r-squared'
      Enabled = False
      TabOrder = 11
      OnClick = BitBtn1Click
    end
    object BitBtn3: TBitBtn
      Left = 301
      Top = 208
      Width = 137
      Height = 25
      Caption = 'Band scattergram'
      Enabled = False
      TabOrder = 12
      OnClick = BitBtn3Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 548
    Height = 76
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 517
    object Label3: TLabel
      Left = 280
      Top = 13
      Width = 63
      Height = 13
      Caption = 'Current class'
    end
    object Label4: TLabel
      Left = 368
      Top = 48
      Width = 70
      Height = 13
      Caption = 'Max pts in box'
    end
    object BitBtn2: TBitBtn
      Left = 8
      Top = 8
      Width = 102
      Height = 25
      Caption = 'New training set'
      TabOrder = 0
      OnClick = BitBtn2Click
    end
    object BitBtn4: TBitBtn
      Left = 8
      Top = 39
      Width = 102
      Height = 25
      Caption = 'Open training set'
      TabOrder = 1
      OnClick = BitBtn4Click
    end
    object BitBtn5: TBitBtn
      Left = 131
      Top = 9
      Width = 102
      Height = 25
      Caption = 'Add training class'
      TabOrder = 2
      OnClick = BitBtn5Click
    end
    object ComboBox1: TComboBox
      Left = 349
      Top = 10
      Width = 145
      Height = 21
      TabOrder = 3
      Text = 'ComboBox1'
      OnChange = ComboBox1Change
    end
    object BitBtn6: TBitBtn
      Left = 128
      Top = 40
      Width = 105
      Height = 25
      Caption = 'Train points'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8088888888888888808888888888888000008888808888777088888880777788
        8088888000008888888888888077888888888888808877808888888888888870
        8888888888888000008888888888778088888808877788808888880778888888
        8888000008888888888888088888888888888808888888888888}
      TabOrder = 4
      OnClick = BitBtn6Click
    end
    object BitBtn8: TBitBtn
      Left = 239
      Top = 39
      Width = 104
      Height = 25
      Caption = 'Train box'
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FF0000000000000000FF0000FF0F
        FFFFFFFFFFFFF0FF0000FF0FFFFFFFFFFFFFF0FF0000FF0FFFFFFFFFFFFFF0FF
        0000FF0FFFFFFFFFFFFFF0FF0000FF0FFFFFFFFFFFFFF0FF0000FF0FFFFFFFFF
        FFFFF0FF0000FF0FFFFFFFFFFFFFF0FF0000FF0FFFFFFFFFFFFFF0FF0000FF0F
        FFFFFFFFFFFFF0FF0000FF0FFFFFFFFFFFFFF0FF0000FF0FFFFFFFFFFFFFF0FF
        0000FF0FFFFFFFFFFFFFF0FF0000FF0FFFFFFFFFFFFFF0FF0000FF0FFFFFFFFF
        FFFFF0FF0000FF0000000000000000FF0000FFFFFFFFFFFFFFFFFFFF0000FFFF
        FFFFFFFFFFFFFFFF0000}
      TabOrder = 5
      OnClick = BitBtn8Click
    end
    object Edit3: TEdit
      Left = 444
      Top = 37
      Width = 65
      Height = 21
      TabOrder = 6
      Text = 'Edit3'
      OnChange = Edit3Change
    end
  end
end
