object CompForm: TCompForm
  Left = 0
  Top = 0
  Caption = 'Computations'
  ClientHeight = 372
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 145
    Width = 550
    Height = 227
    Align = alClient
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 550
    Height = 145
    Align = alTop
    TabOrder = 1
    object Label3: TLabel
      Left = 182
      Top = 85
      Width = 37
      Height = 13
      Caption = 'Dist (m)'
      Enabled = False
    end
    object Label4: TLabel
      Left = 183
      Top = 104
      Width = 36
      Height = 13
      Caption = 'Bearing'
      Enabled = False
    end
    object Label6: TLabel
      Left = 295
      Top = 104
      Width = 22
      Height = 13
      Caption = 'Year'
    end
    object Label5: TLabel
      Left = 295
      Top = 85
      Width = 39
      Height = 13
      Caption = 'Elev (m)'
    end
    object Label1: TLabel
      Left = 230
      Top = 13
      Width = 3
      Height = 13
    end
    object Label2: TLabel
      Left = 230
      Top = 45
      Width = 3
      Height = 13
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 0
      Width = 153
      Height = 117
      Caption = 'Computation'
      ItemIndex = 0
      Items.Strings = (
        'Distance and bearing'
        'Point at distance, bearing'
        'World Magnetic model'
        'Sunrise/Sunset graph'
        'Moonrise/Moonset table')
      TabOrder = 0
      OnClick = RadioGroup1Click
    end
    object BitBtn1: TBitBtn
      Left = 167
      Top = 8
      Width = 60
      Height = 25
      Caption = 'Point 1'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        5555555555FFFFF555555555544C4C5555555555F777775FF5555554C444C444
        5555555775FF55775F55554C4334444445555575577F55557FF554C4C334C4C4
        335557F5577FF55577F554CCC3334444335557555777F555775FCCCCC333CCC4
        C4457F55F777F555557F4CC33333CCC444C57F577777F5F5557FC4333333C3C4
        CCC57F777777F7FF557F4CC33333333C4C457F577777777F557FCCC33CC4333C
        C4C575F7755F777FF5755CCCCC3333334C5557F5FF777777F7F554C333333333
        CC55575777777777F755553333CC3C33C555557777557577755555533CC4C4CC
        5555555775FFFF77555555555C4CCC5555555555577777555555}
      NumGlyphs = 2
      TabOrder = 1
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 167
      Top = 39
      Width = 60
      Height = 25
      Caption = 'Point 2'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        5555555555FFFFF555555555544C4C5555555555F777775FF5555554C444C444
        5555555775FF55775F55554C4334444445555575577F55557FF554C4C334C4C4
        335557F5577FF55577F554CCC3334444335557555777F555775FCCCCC333CCC4
        C4457F55F777F555557F4CC33333CCC444C57F577777F5F5557FC4333333C3C4
        CCC57F777777F7FF557F4CC33333333C4C457F577777777F557FCCC33CC4333C
        C4C575F7755F777FF5755CCCCC3333334C5557F5FF777777F7F554C333333333
        CC55575777777777F755553333CC3C33C555557777557577755555533CC4C4CC
        5555555775FFFF77555555555C4CCC5555555555577777555555}
      NumGlyphs = 2
      TabOrder = 2
      OnClick = BitBtn2Click
    end
    object BitBtn3: TBitBtn
      Left = 0
      Top = 114
      Width = 57
      Height = 25
      Caption = 'Compute'
      TabOrder = 3
      OnClick = BitBtn3Click
    end
    object Edit1: TEdit
      Left = 225
      Top = 77
      Width = 64
      Height = 21
      Enabled = False
      TabOrder = 4
      Text = '25000'
    end
    object Edit2: TEdit
      Left = 225
      Top = 104
      Width = 64
      Height = 21
      Enabled = False
      TabOrder = 5
      Text = '45'
    end
    object Edit3: TEdit
      Left = 348
      Top = 77
      Width = 53
      Height = 21
      TabOrder = 6
    end
    object Edit4: TEdit
      Left = 348
      Top = 104
      Width = 53
      Height = 21
      TabOrder = 7
    end
    object HelpBtn: TBitBtn
      Left = 116
      Top = 114
      Width = 53
      Height = 25
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 8
      OnClick = HelpBtnClick
      IsControl = True
    end
    object CancelBtn: TBitBtn
      Left = 56
      Top = 114
      Width = 62
      Height = 25
      Kind = bkCancel
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 9
      IsControl = True
    end
    object DateTimePicker1: TDateTimePicker
      Left = 424
      Top = 77
      Width = 89
      Height = 21
      Date = 39165.000000000000000000
      Time = 0.637004120369965700
      Enabled = False
      TabOrder = 10
    end
    object Edit5: TEdit
      Left = 424
      Top = 104
      Width = 41
      Height = 21
      TabOrder = 11
      Text = '45'
    end
  end
end
