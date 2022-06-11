object EyeDocF: TEyeDocF
  Left = 0
  Top = 0
  Caption = 'Eye Doc 3D'
  ClientHeight = 357
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object OpenGLSpeedButton: TSpeedButton
    Left = 296
    Top = 297
    Width = 49
    Height = 32
    Hint = 'OpenGL 3D view'
    Glyph.Data = {
      8A000000424D8A000000000000003E0000002800000020000000130000000100
      0100000000004C000000C40E0000C40E0000020000000000000000000000FFFF
      FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE003FFFF00707FFC0DFF9FF81DF
      FF7F02555400075855C30715D583074645F3077FFCD300FFFE1383FFFFFFC0FF
      F9FFF00207FFFE003FFFFFFFFFFF}
    OnClick = OpenGLSpeedButtonClick
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 435
    Height = 284
    Align = alTop
    ColCount = 11
    DefaultColWidth = 36
    RowCount = 11
    ScrollBars = ssVertical
    TabOrder = 0
    ColWidths = (
      36
      36
      36
      36
      36
      36
      36
      36
      36
      36
      36)
    RowHeights = (
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24)
  end
  object BitBtn1: TBitBtn
    Left = 24
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Open file'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 112
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Display'
    TabOrder = 2
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 200
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = BitBtn3Click
  end
end
