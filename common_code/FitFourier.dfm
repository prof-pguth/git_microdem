inherited FitFourierForm: TFitFourierForm
  Left = 398
  Top = 235
  HorzScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Fit Fourier Curve'
  ClientHeight = 176
  ClientWidth = 354
  FormStyle = fsMDIChild
  Visible = True
  StyleElements = [seFont, seClient, seBorder]
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 370
  ExplicitHeight = 215
  TextHeight = 15
  inherited Bevel1: TBevel
    Left = 288
    Top = 0
    Width = 477
    Height = 171
    ExplicitLeft = 288
    ExplicitTop = 0
    ExplicitWidth = 477
    ExplicitHeight = 171
  end
  inherited OKBtn: TButton
    OnClick = OKBtnClick
  end
  inherited CancelBtn: TButton
    OnClick = CancelBtnClick
  end
  object HelpBtn: TButton
    Left = 300
    Top = 68
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 0
    Width = 289
    Height = 135
    Align = alLeft
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    PlainText = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 0
    Top = 135
    Width = 354
    Height = 41
    Align = alBottom
    TabOrder = 4
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 130
      Height = 25
      Caption = 'Show Harmonics'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 144
      Top = 6
      Width = 75
      Height = 25
      Caption = '# of terms'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
