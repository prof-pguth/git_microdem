object BrowserMainForm: TBrowserMainForm
  Left = 476
  Top = 199
  Width = 503
  Height = 377
  AutoScroll = True
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 319
    Width = 487
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object WebBrowser1: TWebBrowser
    Left = 0
    Top = 0
    Width = 487
    Height = 319
    Align = alClient
    TabOrder = 1
    OnDownloadComplete = WebBrowser1DownloadComplete
    ExplicitWidth = 495
    ExplicitHeight = 324
    ControlData = {
      4C00000055320000F82000000100000005000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126200000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Memo1: TMemo
    Left = 80
    Top = 56
    Width = 73
    Height = 33
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
    Visible = False
  end
end
