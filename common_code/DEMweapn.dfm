object PickWeapon: TPickWeapon
  Left = 640
  Top = 256
  BorderStyle = bsDialog
  Caption = 'Viewshed parameters'
  ClientHeight = 495
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 380
    Width = 443
    Height = 74
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 348
    ExplicitWidth = 386
    object Label11: TLabel
      Left = 16
      Top = 8
      Width = 119
      Height = 13
      Caption = 'Frame separation (m)'
    end
    object Label10: TLabel
      Left = 216
      Top = 16
      Width = 46
      Height = 13
      Caption = 'Label10'
    end
    object Edit9: TEdit
      Left = 144
      Top = 8
      Width = 57
      Height = 21
      TabOrder = 0
      OnChange = Edit9Change
    end
    object BitBtn5: TBitBtn
      Left = 15
      Top = 35
      Width = 123
      Height = 33
      Hint = 'Color for composite viewsheds'
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 454
    Width = 443
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 422
    ExplicitWidth = 386
    object CancelBtn: TBitBtn
      Left = 144
      Top = 6
      Width = 78
      Height = 27
      Cancel = True
      Caption = 'Cancel'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = CancelBtnClick
      IsControl = True
    end
    object OKBtn: TBitBtn
      Left = 58
      Top = 6
      Width = 77
      Height = 27
      Caption = '&OK'
      Kind = bkOK
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      IsControl = True
    end
    object HelpBtn: TBitBtn
      Left = 237
      Top = 6
      Width = 77
      Height = 27
      Caption = 'Help'
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 2
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn4: TBitBtn
      Left = 320
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Fans Path'
      TabOrder = 3
      OnClick = BitBtn4Click
    end
  end
  object Panel3: TPanel
    Left = 8
    Top = 0
    Width = 427
    Height = 374
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 2
      Width = 46
      Height = 13
      Caption = 'location'
    end
    object Label5: TLabel
      Left = 15
      Top = 24
      Width = 56
      Height = 13
      Caption = 'Fan name'
    end
    object Label2: TLabel
      Left = 15
      Top = 48
      Width = 81
      Height = 13
      Caption = 'Max range (m)'
    end
    object Label3: TLabel
      Left = 8
      Top = 100
      Width = 51
      Height = 13
      Caption = 'Obs elev'
    end
    object Label4: TLabel
      Left = 8
      Top = 122
      Width = 39
      Height = 13
      Caption = 'Label4'
    end
    object Label8: TLabel
      Left = 16
      Top = 151
      Width = 83
      Height = 13
      Caption = 'Left boundary '
    end
    object Label9: TLabel
      Left = 16
      Top = 174
      Width = 87
      Height = 13
      Caption = 'Right boundary'
    end
    object Label6: TLabel
      Left = 15
      Top = 199
      Width = 86
      Height = 13
      Caption = 'Max inclination'
      FocusControl = BitBtn1
    end
    object Label7: TLabel
      Left = 15
      Top = 228
      Width = 83
      Height = 13
      Caption = 'Min inclination'
    end
    object Label12: TLabel
      Left = 16
      Top = 67
      Width = 78
      Height = 13
      Caption = 'Min range (m)'
    end
    object Label13: TLabel
      Left = 27
      Top = 262
      Width = 65
      Height = 13
      Caption = 'Opacity (%)'
    end
    object BitBtn1: TBitBtn
      Left = 216
      Top = 289
      Width = 113
      Height = 25
      Caption = 'Color && algorithm'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object Edit6: TEdit
      Left = 113
      Top = 16
      Width = 81
      Height = 21
      TabOrder = 1
      Text = ' fan name'
    end
    object Edit2: TEdit
      Left = 112
      Top = 43
      Width = 81
      Height = 21
      TabOrder = 2
    end
    object Edit3: TEdit
      Left = 113
      Top = 97
      Width = 80
      Height = 21
      TabOrder = 3
    end
    object Edit4: TEdit
      Left = 113
      Top = 124
      Width = 82
      Height = 21
      TabOrder = 4
    end
    object Edit1: TEdit
      Left = 115
      Top = 151
      Width = 81
      Height = 21
      TabOrder = 5
    end
    object Edit5: TEdit
      Left = 115
      Top = 178
      Width = 81
      Height = 21
      TabOrder = 6
    end
    object CheckBox4: TCheckBox
      Left = 216
      Top = 124
      Width = 121
      Height = 17
      Caption = 'Report coverage'
      TabOrder = 7
    end
    object CheckBox6: TCheckBox
      Left = 216
      Top = 140
      Width = 113
      Height = 17
      Caption = 'Graph coverage'
      TabOrder = 8
    end
    object CheckBox1: TCheckBox
      Left = 216
      Top = 156
      Width = 97
      Height = 17
      Caption = 'Save radials'
      TabOrder = 9
    end
    object RadioGroup3: TRadioGroup
      Left = 215
      Top = 250
      Width = 145
      Height = 33
      Caption = 'Range Units'
      Columns = 3
      Items.Strings = (
        'ft'
        'yds'
        'm')
      TabOrder = 10
      OnClick = RadioGroup3Click
    end
    object Edit10: TEdit
      Left = 116
      Top = 232
      Width = 81
      Height = 21
      TabOrder = 11
    end
    object Edit7: TEdit
      Left = 116
      Top = 205
      Width = 80
      Height = 21
      TabOrder = 12
    end
    object RadioGroup2: TRadioGroup
      Left = 216
      Top = 63
      Width = 137
      Height = 41
      Caption = 'Target'
      Columns = 2
      Items.Strings = (
        'ASL'
        'AGL')
      TabOrder = 13
      OnClick = RadioGroup2Click
    end
    object RadioGroup1: TRadioGroup
      Left = 216
      Top = 16
      Width = 137
      Height = 41
      Caption = 'Observer'
      Columns = 2
      Items.Strings = (
        'ASL'
        'AGL')
      TabOrder = 14
      OnClick = RadioGroup1Click
    end
    object Edit11: TEdit
      Left = 112
      Top = 70
      Width = 80
      Height = 21
      TabOrder = 15
    end
    object CheckBox3: TCheckBox
      Left = 216
      Top = 191
      Width = 97
      Height = 17
      Caption = 'Outline fans'
      TabOrder = 16
      OnClick = CheckBox3Click
    end
    object BitBtn2: TBitBtn
      Left = 252
      Top = 214
      Width = 85
      Height = 20
      Caption = 'Outline'
      TabOrder = 17
      OnClick = BitBtn2Click
    end
    object Edit12: TEdit
      Left = 115
      Top = 259
      Width = 80
      Height = 21
      TabOrder = 18
    end
    object CheckBox2: TCheckBox
      Left = 216
      Top = 174
      Width = 97
      Height = 17
      Caption = 'Use veg grid'
      TabOrder = 19
    end
    object RadioGroup4: TRadioGroup
      Left = 15
      Top = 320
      Width = 345
      Height = 41
      Caption = 'Fan selection'
      Columns = 3
      Items.Strings = (
        'Single'
        'Multiple (ask)'
        'Multiple (same)')
      TabOrder = 20
    end
    object CheckBox5: TCheckBox
      Left = 216
      Top = 105
      Width = 97
      Height = 17
      Caption = 'Range circles'
      TabOrder = 21
    end
    object BitBtn3: TBitBtn
      Left = 335
      Top = 289
      Width = 64
      Height = 25
      Caption = 'Full view'
      TabOrder = 22
      OnClick = BitBtn3Click
    end
  end
end
