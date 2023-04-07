object TigerOverlayOptions: TTigerOverlayOptions
  Left = 760
  Top = 298
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Tiger Overlay Options'
  ClientHeight = 312
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 13
  object RedrawSpeedButton12: TSpeedButton
    Left = 308
    Top = 253
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
  object OKBtn: TBitBtn
    Left = 16
    Top = 276
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
  object Button1: TButton
    Left = 258
    Top = 222
    Width = 75
    Height = 25
    Caption = 'Defaults'
    TabOrder = 1
    OnClick = Button1Click
  end
  object CheckBox14: TCheckBox
    Left = 8
    Top = 215
    Width = 113
    Height = 17
    Caption = 'Auto appear scaling'
    TabOrder = 2
  end
  object HelpBtn: TBitBtn
    Left = 209
    Top = 276
    Width = 77
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 3
    OnClick = HelpBtnClick
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 111
    Top = 276
    Width = 78
    Height = 27
    Kind = bkCancel
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 4
    OnClick = CancelBtnClick
    IsControl = True
  end
  object CheckBox15: TCheckBox
    Left = 8
    Top = 231
    Width = 145
    Height = 17
    Caption = 'Auto appear on DEMs'
    TabOrder = 5
  end
  object CheckBox17: TCheckBox
    Left = 143
    Top = 215
    Width = 97
    Height = 17
    Caption = 'Grayscale'
    TabOrder = 6
  end
  object CheckBox18: TCheckBox
    Left = 143
    Top = 231
    Width = 97
    Height = 17
    Caption = 'Subdue'
    TabOrder = 7
  end
  object CheckBox19: TCheckBox
    Left = 143
    Top = 253
    Width = 113
    Height = 17
    Caption = 'Label every record'
    TabOrder = 8
  end
  object CheckBox20: TCheckBox
    Left = 8
    Top = 253
    Width = 129
    Height = 17
    Caption = 'Auto appear on images'
    TabOrder = 9
  end
  object TabbedNotebook1: TTabbedNotebook
    Left = 0
    Top = 0
    Width = 326
    Height = 209
    Align = alTop
    TabFont.Charset = DEFAULT_CHARSET
    TabFont.Color = clBtnText
    TabFont.Height = -11
    TabFont.Name = 'MS Sans Serif'
    TabFont.Style = []
    TabOrder = 10
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Roads'
      ExplicitWidth = 315
      ExplicitHeight = 0
      object Label13: TLabel
        Left = 280
        Top = 160
        Width = 11
        Height = 13
        Caption = 'All'
      end
      object BitBtn1: TBitBtn
        Left = 136
        Top = 8
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 0
        OnClick = BitBtn1Click
      end
      object BitBtn2: TBitBtn
        Left = 136
        Top = 32
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 1
        OnClick = BitBtn2Click
      end
      object BitBtn3: TBitBtn
        Left = 136
        Top = 56
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 2
        OnClick = BitBtn3Click
      end
      object BitBtn4: TBitBtn
        Left = 136
        Top = 80
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 3
        OnClick = BitBtn4Click
      end
      object BitBtn5: TBitBtn
        Left = 136
        Top = 104
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 4
        OnClick = BitBtn5Click
      end
      object BitBtn6: TBitBtn
        Left = 136
        Top = 128
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 5
        OnClick = BitBtn6Click
      end
      object BitBtn7: TBitBtn
        Left = 136
        Top = 152
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 6
        OnClick = BitBtn7Click
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Major roads'
        TabOrder = 7
      end
      object CheckBox2: TCheckBox
        Left = 16
        Top = 40
        Width = 97
        Height = 17
        Caption = 'Level 2 roads'
        TabOrder = 8
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Level 3 roads'
        TabOrder = 9
      end
      object CheckBox4: TCheckBox
        Left = 16
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Level 4 roads'
        TabOrder = 10
      end
      object CheckBox5: TCheckBox
        Left = 16
        Top = 112
        Width = 97
        Height = 17
        Caption = 'Level 5 roads'
        TabOrder = 11
      end
      object CheckBox6: TCheckBox
        Left = 16
        Top = 136
        Width = 97
        Height = 17
        Caption = 'Level 6 roads'
        TabOrder = 12
      end
      object CheckBox7: TCheckBox
        Left = 16
        Top = 160
        Width = 97
        Height = 17
        Caption = 'Level 7 roads'
        TabOrder = 13
      end
      object UpDown1: TUpDown
        Left = 258
        Top = 155
        Width = 16
        Height = 24
        Position = 50
        TabOrder = 14
        OnClick = UpDown1Click
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Boundaries'
      ExplicitWidth = 315
      ExplicitHeight = 0
      object CheckBox8: TCheckBox
        Left = 16
        Top = 16
        Width = 121
        Height = 17
        Caption = 'Political boundaries'
        TabOrder = 0
      end
      object BitBtn8: TBitBtn
        Left = 176
        Top = 16
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 1
        OnClick = BitBtn8Click
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Others'
      ExplicitWidth = 315
      ExplicitHeight = 0
      object BitBtn9: TBitBtn
        Left = 160
        Top = 16
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 0
        OnClick = BitBtn9Click
      end
      object BitBtn10: TBitBtn
        Left = 160
        Top = 112
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 1
        OnClick = BitBtn10Click
      end
      object BitBtn11: TBitBtn
        Left = 160
        Top = 64
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 2
        OnClick = BitBtn11Click
      end
      object BitBtn12: TBitBtn
        Left = 160
        Top = 88
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 3
        OnClick = BitBtn12Click
      end
      object CheckBox9: TCheckBox
        Left = 40
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Streams'
        TabOrder = 4
      end
      object CheckBox10: TCheckBox
        Left = 40
        Top = 112
        Width = 97
        Height = 17
        Caption = 'Railroad'
        TabOrder = 5
      end
      object CheckBox11: TCheckBox
        Left = 40
        Top = 64
        Width = 97
        Height = 17
        Caption = 'Powerline'
        TabOrder = 6
      end
      object CheckBox12: TCheckBox
        Left = 40
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Pipeline'
        TabOrder = 7
      end
      object BitBtn13: TBitBtn
        Left = 160
        Top = 40
        Width = 75
        Height = 25
        Caption = ' '
        TabOrder = 8
      end
      object CheckBox16: TCheckBox
        Left = 40
        Top = 144
        Width = 97
        Height = 17
        Caption = 'Feature labels'
        TabOrder = 9
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Autoscaling'
      object Label1: TLabel
        Left = 0
        Top = 8
        Width = 50
        Height = 13
        Caption = 'Major road'
      end
      object Label2: TLabel
        Left = 0
        Top = 32
        Width = 60
        Height = 13
        Caption = 'Road level 2'
      end
      object Label3: TLabel
        Left = 0
        Top = 56
        Width = 60
        Height = 13
        Caption = 'Road level 3'
      end
      object Label4: TLabel
        Left = 0
        Top = 80
        Width = 60
        Height = 13
        Caption = 'Road level 4'
      end
      object Label5: TLabel
        Left = 0
        Top = 104
        Width = 60
        Height = 13
        Caption = 'Road level 5'
      end
      object Label6: TLabel
        Left = 0
        Top = 128
        Width = 60
        Height = 13
        Caption = 'Road level 6'
      end
      object Label7: TLabel
        Left = 0
        Top = 152
        Width = 60
        Height = 13
        Caption = 'Road level 7'
      end
      object Label8: TLabel
        Left = 152
        Top = 8
        Width = 60
        Height = 13
        Caption = 'Major stream'
      end
      object Label9: TLabel
        Left = 224
        Top = 8
        Width = 60
        Height = 13
        Caption = 'Minor stream'
      end
      object Label10: TLabel
        Left = 160
        Top = 136
        Width = 119
        Height = 13
        Caption = 'Current map pixel size (m)'
      end
      object Label11: TLabel
        Left = 160
        Top = 56
        Width = 67
        Height = 13
        Caption = 'Label features'
      end
      object Edit1: TEdit
        Left = 72
        Top = 8
        Width = 65
        Height = 21
        TabOrder = 0
        Text = ' '
      end
      object Edit2: TEdit
        Left = 72
        Top = 32
        Width = 65
        Height = 21
        TabOrder = 1
        Text = ' '
      end
      object Edit3: TEdit
        Left = 72
        Top = 56
        Width = 65
        Height = 21
        TabOrder = 2
        Text = ' '
      end
      object Edit4: TEdit
        Left = 72
        Top = 80
        Width = 65
        Height = 21
        TabOrder = 3
        Text = ' '
      end
      object Edit5: TEdit
        Left = 72
        Top = 104
        Width = 65
        Height = 21
        TabOrder = 4
        Text = ' '
      end
      object Edit6: TEdit
        Left = 72
        Top = 128
        Width = 65
        Height = 21
        TabOrder = 5
        Text = ' '
      end
      object Edit7: TEdit
        Left = 72
        Top = 152
        Width = 65
        Height = 21
        TabOrder = 6
        Text = ' '
      end
      object Edit8: TEdit
        Left = 160
        Top = 24
        Width = 65
        Height = 21
        TabOrder = 7
        Text = ' '
      end
      object Edit9: TEdit
        Left = 240
        Top = 24
        Width = 65
        Height = 21
        TabOrder = 8
        Text = ' '
      end
      object Memo1: TMemo
        Left = 160
        Top = 80
        Width = 137
        Height = 49
        Enabled = False
        Lines.Strings = (
          'Values are screen pixel size '
          '(in m) where features '
          'appear on the map display.')
        TabOrder = 9
      end
      object Edit10: TEdit
        Left = 168
        Top = 152
        Width = 121
        Height = 21
        Enabled = False
        TabOrder = 10
        Text = ' '
      end
      object Edit11: TEdit
        Left = 240
        Top = 48
        Width = 121
        Height = 21
        TabOrder = 11
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Files'
      ExplicitWidth = 315
      ExplicitHeight = 0
      object Label12: TLabel
        Left = 12
        Top = 21
        Width = 66
        Height = 13
        Caption = 'Max counties:'
      end
      object Edit12: TEdit
        Left = 84
        Top = 14
        Width = 89
        Height = 21
        TabOrder = 0
      end
    end
  end
end
