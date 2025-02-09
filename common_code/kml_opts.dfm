object kml_opts_fm: Tkml_opts_fm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'KML database export options'
  ClientHeight = 388
  ClientWidth = 541
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 21
    Top = 32
    Width = 59
    Height = 13
    Caption = 'Folder name'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 85
    Height = 13
    Caption = 'Folder description'
  end
  object Label3: TLabel
    Left = 16
    Top = 144
    Width = 30
    Height = 13
    Caption = 'Image'
  end
  object Label4: TLabel
    Left = 16
    Top = 171
    Width = 42
    Height = 13
    Caption = 'Web site'
  end
  object Label5: TLabel
    Left = 19
    Top = 94
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object Label8: TLabel
    Left = 16
    Top = 203
    Width = 60
    Height = 13
    Caption = 'Icon DB field'
  end
  object Label9: TLabel
    Left = 21
    Top = 288
    Width = 22
    Height = 13
    Caption = 'Time'
  end
  object Label10: TLabel
    Left = 452
    Top = 250
    Width = 31
    Height = 13
    Caption = 'Height'
  end
  object Label11: TLabel
    Left = 312
    Top = 195
    Width = 80
    Height = 13
    Caption = 'Icon scale factor'
  end
  object Label12: TLabel
    Left = 24
    Top = 256
    Width = 22
    Height = 13
    Caption = 'Text'
  end
  object Image1: TImage
    Left = 353
    Top = 229
    Width = 35
    Height = 34
  end
  object Label13: TLabel
    Left = 410
    Top = 117
    Width = 52
    Height = 13
    Caption = 'Thin factor'
  end
  object Label6: TLabel
    Left = 16
    Top = 120
    Width = 76
    Height = 13
    Caption = 'Separate layers'
  end
  object Edit1: TEdit
    Left = 107
    Top = 37
    Width = 281
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 107
    Top = 64
    Width = 281
    Height = 21
    TabOrder = 1
  end
  object ComboBox1: TComboBox
    Left = 67
    Top = 141
    Width = 153
    Height = 21
    TabOrder = 2
  end
  object Edit3: TEdit
    Left = 226
    Top = 141
    Width = 162
    Height = 21
    TabOrder = 3
    Text = 'Image'
  end
  object Edit4: TEdit
    Left = 226
    Top = 168
    Width = 162
    Height = 21
    TabOrder = 4
    Text = 'Web site'
  end
  object ComboBox2: TComboBox
    Left = 67
    Top = 168
    Width = 153
    Height = 21
    TabOrder = 5
  end
  object HelpBtn: TBitBtn
    Left = 410
    Top = 351
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 6
    OnClick = HelpBtnClick
    IsControl = True
  end
  object OKBtn: TBitBtn
    Left = 8
    Top = 351
    Width = 93
    Height = 27
    Caption = 'Save KML'
    Default = True
    Glyph.Data = {
      8A010000424D8A01000000000000760000002800000017000000170000000100
      0400000000001401000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFFFF0FFFFFFFFFFFFFFFFFFFFFFF0FFFFFFF874404478FFFFFFF0FFFF
      F8474444444048FFFFF0FFFF848744777777448FFFF0FFF86888888888888848
      FFF0FFF68888887444444474FFF0FF87F8874444444444408FF0FF7477444444
      444444447FF0FF4444444444444447887FF0FF4444666664444788874FF0FF44
      44677776478F87440FF0FF44467777778FF864444FF0FF644677888FFF844447
      4FF0FF7846778FFFF84446884FF0FF888778FFFF844448878FF0FFF7FFFFFFF8
      44447F84FFF0FFF87FFFFF744447F848FFF0FFFF84777444448F868FFFF0FFFF
      F86447778FF768FFFFF0FFFFFFF877777778FFFFFFF0FFFFFFFFFFFFFFFFFFFF
      FFF0FFFFFFFFFFFFFFFFFFFFFFF0}
    Margin = 2
    ModalResult = 1
    Spacing = -1
    TabOrder = 7
    OnClick = OKBtnClick
    IsControl = True
  end
  object ComboBox4: TComboBox
    Left = 107
    Top = 91
    Width = 145
    Height = 21
    TabOrder = 8
  end
  object BitBtn2: TBitBtn
    Left = 317
    Top = 351
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
    TabOrder = 9
    OnClick = OKBtnClick
    IsControl = True
  end
  object ComboBox5: TComboBox
    Left = 94
    Top = 195
    Width = 156
    Height = 21
    TabOrder = 10
  end
  object ComboBox6: TComboBox
    Left = 64
    Top = 285
    Width = 156
    Height = 21
    TabOrder = 11
  end
  object ComboBox7: TComboBox
    Left = 232
    Top = 285
    Width = 156
    Height = 21
    TabOrder = 12
  end
  object RadioGroup1: TRadioGroup
    Left = 398
    Top = 160
    Width = 103
    Height = 81
    Caption = 'Images'
    ItemIndex = 0
    Items.Strings = (
      'Embed'
      'Thumbnail, link'
      'Thumbnail only')
    TabOrder = 13
    OnClick = RadioGroup1Click
  end
  object Edit6: TEdit
    Left = 398
    Top = 247
    Width = 41
    Height = 21
    TabOrder = 14
  end
  object Edit7: TEdit
    Left = 264
    Top = 195
    Width = 42
    Height = 21
    TabOrder = 15
    Text = '1.1'
  end
  object CheckBox1: TCheckBox
    Left = 408
    Top = 40
    Width = 135
    Height = 17
    Caption = 'Table with attributes'
    TabOrder = 16
    OnClick = CheckBox1Click
  end
  object ComboBox8: TComboBox
    Left = 64
    Top = 256
    Width = 156
    Height = 21
    TabOrder = 17
  end
  object CheckBox2: TCheckBox
    Left = 226
    Top = 262
    Width = 136
    Height = 17
    Caption = 'No changes to HTML'
    TabOrder = 18
  end
  object BitBtn3: TBitBtn
    Left = 264
    Top = 229
    Width = 75
    Height = 25
    Caption = 'Pick icon'
    TabOrder = 19
    OnClick = BitBtn3Click
  end
  object CheckBox3: TCheckBox
    Left = 408
    Top = 86
    Width = 97
    Height = 17
    Caption = 'Time animations'
    TabOrder = 20
    OnClick = CheckBox3Click
  end
  object Edit8: TEdit
    Left = 468
    Top = 114
    Width = 44
    Height = 21
    TabOrder = 21
    Text = '1'
  end
  object CheckBox4: TCheckBox
    Left = 408
    Top = 63
    Width = 121
    Height = 17
    Caption = 'Label point symbols'
    TabOrder = 22
    OnClick = CheckBox4Click
  end
  object ComboBox3: TComboBox
    Left = 107
    Top = 114
    Width = 145
    Height = 21
    TabOrder = 23
  end
  object CheckBox5: TCheckBox
    Left = 408
    Top = 17
    Width = 97
    Height = 17
    Caption = 'Top level folder'
    TabOrder = 24
    OnClick = CheckBox5Click
  end
  object CheckBox7: TCheckBox
    Left = 400
    Top = 328
    Width = 97
    Height = 17
    Caption = 'Zip KML to KMZ'
    TabOrder = 25
    OnClick = CheckBox7Click
  end
end
