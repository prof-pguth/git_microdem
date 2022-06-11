object get_angle_form: Tget_angle_form
  Left = 306
  Top = 174
  Caption = 'GMT gridding options'
  ClientHeight = 135
  ClientWidth = 176
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Edit5: TEdit
    Left = 8
    Top = 8
    Width = 97
    Height = 21
    TabOrder = 0
    Text = '0.00001'
    OnChange = Edit5Change
  end
  object RadioGroup4: TRadioGroup
    Left = 8
    Top = 40
    Width = 129
    Height = 41
    Columns = 3
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemIndex = 0
    Items.Strings = (
      #176
      #39
      '"')
    ParentFont = False
    TabOrder = 1
    OnClick = RadioGroup4Click
  end
  object BitBtn5: TBitBtn
    Left = 32
    Top = 87
    Width = 73
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
    TabOrder = 2
    OnClick = BitBtn5Click
  end
end
