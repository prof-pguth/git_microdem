object PickDEMIXmodeForm: TPickDEMIXmodeForm
  Left = 0
  Top = 0
  Caption = 'Pick DEMIX mode'
  ClientHeight = 186
  ClientWidth = 437
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 24
    Width = 401
    Height = 105
    Caption = 'DEMIX mode'
    Items.Strings = (
      'Not yet defined'
      'Classic--Cop, ALOS, SRTM, NASA, ASTER, FABDEM, TANDEM'
      'Add Dilumium (only  below 80 m elevation)'
      'Add Delta (only below 10 m elevation)')
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 168
    Top = 143
    Width = 57
    Height = 25
    Caption = 'OK'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      555555555555555555555555555555555555555555FF55555555555559055555
      55555555577FF5555555555599905555555555557777F5555555555599905555
      555555557777FF5555555559999905555555555777777F555555559999990555
      5555557777777FF5555557990599905555555777757777F55555790555599055
      55557775555777FF5555555555599905555555555557777F5555555555559905
      555555555555777FF5555555555559905555555555555777FF55555555555579
      05555555555555777FF5555555555557905555555555555777FF555555555555
      5990555555555555577755555555555555555555555555555555}
    NumGlyphs = 2
    TabOrder = 1
    OnClick = BitBtn1Click
  end
end
