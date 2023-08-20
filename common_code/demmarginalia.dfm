object DemMarginaliaForm: TDemMarginaliaForm
  Left = 531
  Top = 313
  BorderIcons = []
  Caption = 'Legends/Marginalia'
  ClientHeight = 422
  ClientWidth = 607
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  TextHeight = 13
  object Image2: TImage
    Left = 0
    Top = 0
    Width = 105
    Height = 105
    OnMouseMove = Image2MouseMove
  end
  object Label1: TLabel
    Left = 320
    Top = 336
    Width = 3
    Height = 13
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 403
    Width = 607
    Height = 19
    Panels = <>
    ExplicitTop = 384
    ExplicitWidth = 550
  end
  object Panel1: TPanel
    Left = 443
    Top = 0
    Width = 164
    Height = 403
    Align = alRight
    TabOrder = 1
    ExplicitLeft = 386
    ExplicitHeight = 384
    object Label4: TLabel
      Left = 14
      Top = 204
      Width = 52
      Height = 13
      Caption = 'Left margin'
      Enabled = False
    end
    object Label5: TLabel
      Left = 6
      Top = 226
      Width = 67
      Height = 13
      Caption = 'Bottom margin'
      Enabled = False
    end
    object Label2: TLabel
      Left = 8
      Top = 162
      Width = 32
      Height = 13
      Caption = 'Label2'
    end
    object El: TBitBtn
      Left = 32
      Top = 7
      Width = 105
      Height = 25
      Caption = 'Elevation/grid value'
      TabOrder = 0
      OnClick = ElClick
    end
    object BitBtn3: TBitBtn
      Left = 32
      Top = 38
      Width = 105
      Height = 25
      Caption = 'Scale bar'
      TabOrder = 1
      OnClick = BitBtn3Click
    end
    object BitBtn5: TBitBtn
      Left = 32
      Top = 69
      Width = 105
      Height = 25
      Caption = 'Terrain categories'
      TabOrder = 2
      OnClick = BitBtn5Click
    end
    object Edit7: TEdit
      Left = 80
      Top = 199
      Width = 57
      Height = 21
      Enabled = False
      TabOrder = 3
      Text = ' '
      OnChange = CheckBox1Click
    end
    object Edit8: TEdit
      Left = 79
      Top = 226
      Width = 57
      Height = 21
      Enabled = False
      TabOrder = 4
      Text = ' '
      OnChange = CheckBox1Click
    end
    object BitBtn1: TBitBtn
      Left = 24
      Top = 284
      Width = 105
      Height = 25
      Caption = 'Redraw map'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
        FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
        CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
        FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
      TabOrder = 5
      OnClick = BitBtn1Click
    end
    object HelpBtn: TBitBtn
      Left = 25
      Top = 315
      Width = 104
      Height = 25
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 6
      OnClick = HelpBtnClick
      IsControl = True
    end
    object BitBtn4: TBitBtn
      Left = 24
      Top = 345
      Width = 105
      Height = 27
      Caption = 'OK (Close)'
      Kind = bkOK
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 7
      OnClick = BitBtn4Click
      IsControl = True
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 7
      Width = 12
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 8
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 38
      Width = 12
      Height = 17
      Caption = 'CheckBox2'
      TabOrder = 9
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 69
      Width = 12
      Height = 17
      Caption = 'CheckBox3'
      TabOrder = 10
      OnClick = CheckBox3Click
    end
    object BitBtn2: TBitBtn
      Left = 24
      Top = 253
      Width = 105
      Height = 25
      Caption = 'Grid                '
      Glyph.Data = {
        7E000000424D7E000000000000003E0000002800000010000000100000000100
        010000000000400000000000000000000000020000000200000000000000FFFF
        FF00EF7B0000EF7B000000000000EF7B0000EF7B0000EF7B0000EF7B00000000
        0000EF7B0000EF7B0000EF7B0000EF7B000000000000EF7B0000EF7B0000EF7B
        0000}
      TabOrder = 11
      OnClick = BitBtn2Click
    end
    object BitBtn6: TBitBtn
      Left = 32
      Top = 100
      Width = 105
      Height = 25
      Caption = 'N Arrow'
      TabOrder = 12
      OnClick = BitBtn6Click
    end
    object CheckBox5: TCheckBox
      Left = 8
      Top = 100
      Width = 17
      Height = 17
      TabOrder = 13
      OnClick = CheckBox5Click
    end
    object CheckBox6: TCheckBox
      Left = 8
      Top = 131
      Width = 97
      Height = 17
      TabOrder = 14
      OnClick = CheckBox6Click
    end
    object BitBtn7: TBitBtn
      Left = 32
      Top = 131
      Width = 105
      Height = 25
      Caption = 'Map name'
      TabOrder = 15
      OnClick = BitBtn7Click
    end
  end
end
