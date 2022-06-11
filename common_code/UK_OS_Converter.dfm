object UKOSConvertForm: TUKOSConvertForm
  Left = 508
  Top = 325
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 252
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object RichEdit1: TRichEdit
    Left = 0
    Top = 164
    Width = 333
    Height = 88
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BitBtn4: TBitBtn
    Left = 199
    Top = 127
    Width = 43
    Height = 25
    Caption = 'Clear'
    TabOrder = 1
    OnClick = BitBtn4Click
  end
  object HelpBtn: TBitBtn
    Left = 250
    Top = 127
    Width = 75
    Height = 27
    Kind = bkHelp
    Margin = 2
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 2
    OnClick = HelpBtnClick
    IsControl = True
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 0
    Width = 313
    Height = 121
    ActivePage = TabSheet1
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'x/y to Geo'
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 58
        Height = 13
        Caption = 'x coordinate'
      end
      object Label3: TLabel
        Left = 8
        Top = 40
        Width = 58
        Height = 13
        Caption = 'y coordinate'
      end
      object Edit1: TEdit
        Left = 88
        Top = 8
        Width = 89
        Height = 21
        TabOrder = 0
        Text = '429157'
      end
      object Edit2: TEdit
        Left = 88
        Top = 33
        Width = 89
        Height = 21
        TabOrder = 1
        Text = '623009'
      end
      object BitBtn1: TBitBtn
        Left = 32
        Top = 60
        Width = 75
        Height = 25
        Caption = 'Convert'
        TabOrder = 2
        OnClick = BitBtn1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Geo to x/y'
      ImageIndex = 1
      object Label6: TLabel
        Left = 8
        Top = 8
        Width = 15
        Height = 13
        Caption = 'Lat'
      end
      object Label7: TLabel
        Left = 8
        Top = 32
        Width = 24
        Height = 13
        Caption = 'Long'
      end
      object Edit3: TEdit
        Left = 48
        Top = 8
        Width = 81
        Height = 21
        TabOrder = 0
        Text = '60.00'
      end
      object Edit4: TEdit
        Left = 48
        Top = 32
        Width = 81
        Height = 21
        TabOrder = 1
        Text = '23.50'
      end
      object BitBtn2: TBitBtn
        Left = 128
        Top = 64
        Width = 75
        Height = 25
        Caption = 'Convert'
        TabOrder = 2
        OnClick = BitBtn2Click
      end
      object BitBtn3: TBitBtn
        Left = 24
        Top = 64
        Width = 51
        Height = 25
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
        TabOrder = 3
        OnClick = BitBtn3Click
      end
    end
    object Parameters: TTabSheet
      Caption = 'Parameters'
      ImageIndex = 2
      object Label1: TLabel
        Left = 0
        Top = 8
        Width = 57
        Height = 13
        Caption = 'Scale factor'
      end
      object Label4: TLabel
        Left = 0
        Top = 32
        Width = 62
        Height = 13
        Caption = 'False easting'
      end
      object Label5: TLabel
        Left = 0
        Top = 56
        Width = 66
        Height = 13
        Caption = 'False northing'
      end
      object Label9: TLabel
        Left = 160
        Top = 3
        Width = 6
        Height = 13
        Caption = 'a'
      end
      object Label10: TLabel
        Left = 160
        Top = 27
        Width = 3
        Height = 13
        Caption = 'f'
      end
      object Label11: TLabel
        Left = 160
        Top = 48
        Width = 6
        Height = 13
        Caption = 'e'
      end
      object Label12: TLabel
        Left = 216
        Top = 32
        Width = 3
        Height = 13
      end
      object Label13: TLabel
        Left = 0
        Top = 80
        Width = 56
        Height = 13
        Caption = 'Central long'
      end
      object Edit5: TEdit
        Left = 72
        Top = 0
        Width = 73
        Height = 21
        TabOrder = 0
      end
      object Edit6: TEdit
        Left = 72
        Top = 27
        Width = 73
        Height = 21
        TabOrder = 1
      end
      object Edit7: TEdit
        Left = 72
        Top = 48
        Width = 73
        Height = 21
        TabOrder = 2
      end
      object Edit8: TEdit
        Left = 184
        Top = 1
        Width = 75
        Height = 21
        TabOrder = 3
      end
      object Edit9: TEdit
        Left = 184
        Top = 24
        Width = 75
        Height = 21
        TabOrder = 4
      end
      object BitBtn5: TBitBtn
        Left = 200
        Top = 72
        Width = 59
        Height = 17
        Caption = 'Update'
        TabOrder = 5
        OnClick = BitBtn5Click
      end
      object Edit10: TEdit
        Left = 72
        Top = 72
        Width = 73
        Height = 21
        TabOrder = 6
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Units'
      ImageIndex = 4
      object RadioGroup1: TRadioGroup
        Left = 16
        Top = 3
        Width = 169
        Height = 73
        Caption = 'Geo display'
        Items.Strings = (
          'Decimal degrees'
          'Decimal minutes'
          'Decimal seconds')
        TabOrder = 0
        OnClick = RadioGroup1Click
      end
    end
  end
end
