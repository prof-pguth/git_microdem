object splitter_form: Tsplitter_form
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'splitter_form'
  ClientHeight = 652
  ClientWidth = 993
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  Visible = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 105
    Height = 652
    Align = alLeft
    ExplicitLeft = 8
    ExplicitTop = 24
    ExplicitHeight = 105
  end
  object Splitter1: TSplitter
    Left = 105
    Top = 0
    Height = 652
    OnMoved = Splitter1Moved
    ExplicitLeft = 112
    ExplicitTop = 120
    ExplicitHeight = 100
  end
  object Image2: TImage
    Left = 108
    Top = 0
    Width = 885
    Height = 652
    Align = alClient
    ExplicitLeft = 111
    ExplicitWidth = 875
    ExplicitHeight = 642
  end
end
