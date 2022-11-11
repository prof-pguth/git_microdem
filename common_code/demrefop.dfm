object RefOptFM: TRefOptFM
  Left = 363
  Top = 242
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Hillshade/Reflectance Map Options'
  ClientHeight = 425
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 421
    Height = 401
    Align = alTop
    Shape = bsFrame
    IsControl = True
    ExplicitWidth = 525
  end
  object Label1: TLabel
    Left = 25
    Top = 10
    Width = 87
    Height = 13
    Caption = 'Sun azimuth ('#176')'
  end
  object Label2: TLabel
    Left = 25
    Top = 33
    Width = 96
    Height = 13
    Caption = 'Sun elevation ('#176')'
  end
  object Label3: TLabel
    Left = 118
    Top = 8
    Width = 5
    Height = 13
  end
  object Label4: TLabel
    Left = 8
    Top = 56
    Width = 56
    Height = 13
    Caption = 'Vert Exag'
  end
  object Image1: TImage
    Left = 336
    Top = 104
    Width = 100
    Height = 100
  end
  object Image2: TImage
    Left = 336
    Top = 8
    Width = 100
    Height = 100
    OnDblClick = Image2DblClick
    OnMouseMove = Image2MouseMove
  end
  object Label5: TLabel
    Left = 296
    Top = 224
    Width = 59
    Height = 13
    Caption = 'Saturation'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 385
    Top = 407
    Width = 25
    Height = 25
    Hint = 'Force redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = RedrawSpeedButton12Click
  end
  object Label6: TLabel
    Left = 8
    Top = 158
    Width = 75
    Height = 13
    Caption = 'Sea level (m)'
  end
  object OKBtn: TBitBtn
    Left = 38
    Top = 407
    Width = 77
    Height = 27
    Kind = bkOK
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 0
    OnClick = OKBtnClick
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 121
    Top = 407
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = CancelBtnClick
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 204
    Top = 407
    Width = 76
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object Edit1: TEdit
    Left = 145
    Top = 8
    Width = 81
    Height = 21
    TabOrder = 3
    OnChange = Edit1Change
  end
  object Edit2: TEdit
    Left = 144
    Top = 33
    Width = 82
    Height = 21
    TabOrder = 4
    OnChange = Edit2Change
  end
  object Edit3: TEdit
    Left = 70
    Top = 52
    Width = 53
    Height = 21
    TabOrder = 5
    OnChange = Edit3Change
  end
  object Button1: TButton
    Left = 286
    Top = 407
    Width = 75
    Height = 27
    Caption = 'Default'
    TabOrder = 6
    OnClick = Button1Click
  end
  object CheckBox2: TCheckBox
    Left = 320
    Top = 311
    Width = 113
    Height = 17
    Caption = 'Sea level check'
    TabOrder = 7
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 320
    Top = 327
    Width = 97
    Height = 17
    Caption = 'Lake check'
    TabOrder = 8
    OnClick = CheckBox3Click
  end
  object BitBtn1: TBitBtn
    Left = 305
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Water'
    TabOrder = 9
    OnClick = BitBtn1Click
  end
  object RadioGroup1: TRadioGroup
    Left = 136
    Top = 141
    Width = 114
    Height = 183
    Caption = 'Colors'
    Items.Strings = (
      'Grays'
      'IHS elev '
      'Six colors'
      'Green/blue'
      'Gray/color'
      'Color/Gray'
      'Gray/blue'
      'Grn/Yel/red'
      'Grn/Gray/red'
      'Grn/Cyan/Blue')
    TabOrder = 10
    OnClick = RadioGroup1Click
  end
  object TrackBar1: TTrackBar
    Left = 296
    Top = 241
    Width = 137
    Height = 33
    Max = 255
    Frequency = 25
    TabOrder = 11
    OnChange = TrackBar1Change
  end
  object BitBtn2: TBitBtn
    Left = 8
    Top = 316
    Width = 75
    Height = 25
    Caption = 'z Range'
    TabOrder = 12
    OnClick = BitBtn2Click
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 353
    Width = 295
    Height = 36
    Caption = 'Directions to average'
    Columns = 8
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8')
    TabOrder = 13
    OnClick = RadioGroup2Click
  end
  object CheckBox5: TCheckBox
    Left = 136
    Top = 330
    Width = 131
    Height = 17
    Caption = 'Immediate redraw'
    TabOrder = 14
    OnClick = CheckBox5Click
  end
  object Edit4: TEdit
    Left = 89
    Top = 158
    Width = 41
    Height = 21
    TabOrder = 15
    Text = 'Edit4'
    OnChange = Edit4Change
  end
  object RadioGroup3: TRadioGroup
    Left = 8
    Top = 79
    Width = 242
    Height = 56
    Caption = 'Vert Exag'
    Columns = 5
    Items.Strings = (
      '1'
      '1.5'
      '2'
      '2.5'
      '3'
      '5'
      '7.5'
      '10'
      '12'
      '15')
    TabOrder = 16
    OnClick = RadioGroup3Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 185
    Width = 122
    Height = 68
    Caption = 'Color change maps'
    TabOrder = 17
    object Label8: TLabel
      Left = 17
      Top = 39
      Width = 50
      Height = 13
      Caption = 'Base (m)'
    end
    object Label7: TLabel
      Left = 17
      Top = 20
      Width = 44
      Height = 13
      Caption = 'Top (m)'
    end
    object Edit6: TEdit
      Left = 78
      Top = 41
      Width = 41
      Height = 21
      TabOrder = 0
      Text = 'Edit4'
      OnChange = Edit6Change
    end
    object Edit5: TEdit
      Left = 78
      Top = 14
      Width = 41
      Height = 21
      TabOrder = 1
      Text = 'Edit4'
      OnChange = Edit5Change
    end
  end
end
