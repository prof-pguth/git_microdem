object SS_opts_form: TSS_opts_form
  Left = 0
  Top = 0
  BorderIcons = [biMinimize]
  BorderStyle = bsDialog
  Caption = 'Sidescan Options'
  ClientHeight = 367
  ClientWidth = 644
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
  object Label7: TLabel
    Left = 104
    Top = 72
    Width = 75
    Height = 13
    Caption = 'Fisht height (m)'
    Enabled = False
  end
  object Label3: TLabel
    Left = 112
    Top = 48
    Width = 45
    Height = 13
    Caption = 'High Gain'
    Enabled = False
  end
  object Label2: TLabel
    Left = 112
    Top = 24
    Width = 43
    Height = 13
    Caption = 'Low Gain'
    Enabled = False
  end
  object Label5: TLabel
    Left = 16
    Top = 168
    Width = 55
    Height = 13
    Caption = 'First record'
  end
  object Label6: TLabel
    Left = 15
    Top = 200
    Width = 58
    Height = 13
    Caption = 'Last record:'
  end
  object Label14: TLabel
    Left = 18
    Top = 236
    Width = 100
    Height = 13
    Caption = 'Across track thinning'
  end
  object Label15: TLabel
    Left = 23
    Top = 262
    Width = 95
    Height = 13
    Caption = 'Along track thinning'
  end
  object Label10: TLabel
    Left = 176
    Top = 192
    Width = 58
    Height = 13
    Caption = 'Layback (m)'
  end
  object Label8: TLabel
    Left = 448
    Top = 289
    Width = 51
    Height = 13
    Caption = 'Low cutoff'
  end
  object Label9: TLabel
    Left = 448
    Top = 319
    Width = 53
    Height = 13
    Caption = 'High cutoff'
  end
  object Label11: TLabel
    Left = 34
    Top = 288
    Width = 60
    Height = 13
    Caption = 'Ping repeats'
  end
  object RedrawSpeedButton12: TSpeedButton
    Left = 240
    Top = 341
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
  object Label12: TLabel
    Left = 520
    Top = 273
    Width = 26
    Height = 13
    Caption = 'Value'
  end
  object Label13: TLabel
    Left = 584
    Top = 272
    Width = 47
    Height = 13
    Caption = 'Percentile'
  end
  object BitBtn2: TBitBtn
    Left = 319
    Top = 273
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
    TabOrder = 0
    OnClick = BitBtn2Click
    IsControl = True
  end
  object BitBtn3: TBitBtn
    Left = 319
    Top = 306
    Width = 77
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    OnClick = BitBtn3Click
    IsControl = True
  end
  object BitBtn4: TBitBtn
    Left = 319
    Top = 339
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = BitBtn4Click
    IsControl = True
  end
  object Edit1: TEdit
    Left = 184
    Top = 24
    Width = 81
    Height = 21
    Enabled = False
    TabOrder = 3
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 184
    Top = 48
    Width = 81
    Height = 21
    Enabled = False
    TabOrder = 4
    Text = 'Edit2'
  end
  object Edit7: TEdit
    Left = 184
    Top = 72
    Width = 81
    Height = 21
    Enabled = False
    TabOrder = 5
    Text = 'Edit7'
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 73
    Height = 73
    Caption = 'Frequency'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Low'
      'High'
      'Merge')
    TabOrder = 6
  end
  object Edit4: TEdit
    Left = 88
    Top = 168
    Width = 57
    Height = 21
    TabOrder = 7
    Text = 'Edit4'
  end
  object Edit5: TEdit
    Left = 88
    Top = 200
    Width = 57
    Height = 21
    TabOrder = 8
    Text = 'Edit5'
  end
  object Edit8: TEdit
    Left = 131
    Top = 232
    Width = 48
    Height = 21
    TabOrder = 9
    Text = 'Edit4'
  end
  object Edit9: TEdit
    Left = 131
    Top = 259
    Width = 47
    Height = 21
    TabOrder = 10
    Text = '1'
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 320
    Width = 173
    Height = 17
    Caption = 'Reverse image direction'
    TabOrder = 11
  end
  object CheckBox2: TCheckBox
    Left = 24
    Top = 344
    Width = 133
    Height = 17
    Caption = 'Reverse grayscale'
    TabOrder = 12
  end
  object BitBtn1: TBitBtn
    Left = 221
    Top = 278
    Width = 43
    Height = 25
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33033333333333333F8F3333333333333000333333333333F888333333333333
      000333333333333F888333333333333000333333333333F88833333333333300
      033333333FFF3F888333333800083B803333333F8883F8883333330FFFFF00B3
      3333338833388883333330FFFFFFF033333338F3333338F333338FFFF0FFFF83
      333338333333383F33330FFFF0FFFF0333338F333333338F33330FFFF0FFFF03
      33338F333333338F33330FFFF0FFFF03333383F33333338333338FFF00FFFF83
      333338F3333338F3333330FFF0FFF0333333383FF333F8333333330FFFFF0333
      333333883FF88333333333380008333333333333888333333333}
    NumGlyphs = 2
    TabOrder = 13
    OnClick = BitBtn1Click
  end
  object Edit12: TEdit
    Left = 240
    Top = 191
    Width = 49
    Height = 21
    TabOrder = 14
    Text = 'Edit12'
  end
  object CheckBox3: TCheckBox
    Left = 184
    Top = 318
    Width = 129
    Height = 17
    Caption = 'Custom color palette'
    TabOrder = 15
  end
  object GroupBox1: TGroupBox
    Left = 18
    Top = 88
    Width = 130
    Height = 74
    TabOrder = 16
    object Label1: TLabel
      Left = 3
      Top = 26
      Width = 78
      Height = 13
      Caption = 'Across track (m)'
    end
    object Label4: TLabel
      Left = 3
      Top = 53
      Width = 73
      Height = 13
      Caption = 'Along track (m)'
    end
    object Edit3: TEdit
      Left = 82
      Top = 26
      Width = 45
      Height = 21
      TabOrder = 0
      Text = 'Edit3'
    end
    object Edit6: TEdit
      Left = 82
      Top = 53
      Width = 45
      Height = 21
      TabOrder = 1
      Text = 'Edit6'
    end
    object CheckBox4: TCheckBox
      Left = 19
      Top = 3
      Width = 97
      Height = 17
      Caption = 'Scaled grid'
      TabOrder = 2
    end
  end
  object Memo1: TMemo
    Left = 311
    Top = 8
    Width = 337
    Height = 259
    ScrollBars = ssVertical
    TabOrder = 17
  end
  object Edit10: TEdit
    Left = 520
    Top = 289
    Width = 57
    Height = 21
    Enabled = False
    TabOrder = 18
    Text = 'Edit10'
  end
  object Edit11: TEdit
    Left = 520
    Top = 316
    Width = 57
    Height = 21
    Enabled = False
    TabOrder = 19
    Text = 'Edit11'
  end
  object BitBtn5: TBitBtn
    Left = 159
    Top = 109
    Width = 75
    Height = 25
    Caption = 'Grid'
    TabOrder = 20
    OnClick = BitBtn5Click
  end
  object Edit13: TEdit
    Left = 583
    Top = 289
    Width = 57
    Height = 21
    TabOrder = 21
    Text = 'Edit10'
    OnChange = Edit13Change
  end
  object Edit15: TEdit
    Left = 583
    Top = 316
    Width = 57
    Height = 21
    TabOrder = 22
    Text = 'Edit10'
    OnChange = Edit15Change
  end
  object Edit14: TEdit
    Left = 131
    Top = 286
    Width = 47
    Height = 21
    TabOrder = 23
    Text = '1'
  end
end
