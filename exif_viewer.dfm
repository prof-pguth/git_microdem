object Exif_Form: TExif_Form
  Left = 95
  Top = 291
  Caption = 'EXIF JPEG Viewer'
  ClientHeight = 631
  ClientWidth = 835
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 835
    Height = 590
    ActivePage = Image
    Align = alClient
    TabOrder = 0
    object Image: TTabSheet
      Caption = 'Image'
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 827
        Height = 562
        Align = alClient
        TabOrder = 0
        object Image1: TImage
          Left = -1
          Top = -1
          Width = 434
          Height = 314
          AutoSize = True
        end
      end
    end
    object Metadata: TTabSheet
      Caption = 'Metadata'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 827
        Height = 562
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 835
    Height = 41
    Align = alTop
    TabOrder = 1
    object btnLoad: TButton
      Left = 16
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Open file'
      TabOrder = 0
      OnClick = btnLoadClick
    end
    object CheckBox1: TCheckBox
      Left = 344
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Verbose'
      TabOrder = 1
      OnClick = CheckBox1Click
    end
  end
  object pdlg: TOpenPictureDialog
    Filter = 'JPEG Image File |*.jpg;*.jpeg'
    Left = 224
    Top = 8
  end
end
