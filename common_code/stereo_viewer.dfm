object StereoViewerForm: TStereoViewerForm
  Left = 182
  Top = 356
  Caption = 'Stereo Viewer'
  ClientHeight = 645
  ClientWidth = 1090
  Color = cl3DLight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  WindowState = wsMaximized
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 40
    Top = 48
    Width = 105
    Height = 105
  end
  object Image2: TImage
    Left = 304
    Top = 48
    Width = 105
    Height = 105
  end
  object MainMenu1: TMainMenu
    Left = 200
    Top = 264
    object Flipimages1: TMenuItem
      Caption = 'Flip images'
      OnClick = Flipimages1Click
    end
    object Anaglyph1: TMenuItem
      Caption = 'Anaglyph'
      OnClick = Anaglyph1Click
    end
    object Twinview1: TMenuItem
      Caption = 'Twin view'
      Enabled = False
      OnClick = Twinview1Click
    end
    object Save2: TMenuItem
      Caption = 'Save'
      object Save1: TMenuItem
        Caption = 'Save anaglyph'
        Enabled = False
        OnClick = Save1Click
      end
      object SaveJPSstereo1: TMenuItem
        Caption = 'Save JPS stereo'
        OnClick = SaveJPSstereo1Click
      end
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 424
    Top = 296
  end
end
