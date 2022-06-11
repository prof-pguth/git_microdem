object AltGraphFm: TAltGraphFm
  Left = 200
  Top = 99
  Caption = 'Initializing . . . '
  ClientHeight = 273
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF009999
    99999999999999999999999999999FFFFFFFFFFFFF88888888888FFFFFF997FF
    FFFFFFFF888888888888888FFFF9977FFFFFFFF8444C444888888888FFF99777
    FFFFFF4444444444488888888FF997777FFF44444C4C4C4C4C48888888F99777
    77F44444443444444444888888F99777774C444C433C4C4C4C4C488888899777
    74444444C334C444C444C48888899777744C4C4C433C4C4C4C4C4C8888899777
    444444C43334CCC4C4C4C44888899777444C4C43333C4C4C4C4C4C3888899774
    4444C4433333CCCCC4CCC433888997744C4C4C4333333C4C4C4C4C3388899774
    4444C43333333CCCCCCCC43388899774444C4C333333CC4CCC4C4C3388899774
    4444C433333CCCCCCCC33333888997744C4C4C334C4C4CCCCCC3333388899774
    44444433CCCC3CCCCCC3333388F99777444C4C433C433C4CCC4C333888F99777
    4444343333333CCCCCCCC4C88FF99777744C333333333C4C4C433C88FFF99777
    7444333333333CCCCCC3348FFFF99777774C333333333C4C3C433FFFFFF99777
    7774333333C333CC3433FFFFFFF9977777774333334C333C4C377FFFFFF99777
    77777744433444C4477777FFFFF99777777777774C4C4C477777777FFFF99777
    777777777777777777777777FFF997777777777777777777777777777FF99777
    77777777777777777777777777F9999999999999999999999999999999990000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDefault
  PrintScale = poPrintToFit
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 16
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 427
    Height = 273
    Align = alClient
    OnDblClick = Image1DblClick
    OnMouseMove = Image1MouseMove
    ExplicitHeight = 254
  end
  object MainMenu1: TMainMenu
    Left = 368
    Top = 208
    object File1: TMenuItem
      Caption = '&File'
      object Plotdatacast1: TMenuItem
        Caption = 'Plot &Data'
        OnClick = Plotdatacast1Click
      end
      object Removedata1: TMenuItem
        Caption = '&Remove data'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Print'
        OnClick = Print1Click
      end
      object Saveimage1: TMenuItem
        Caption = '&Save image'
        OnClick = Saveimage1Click
      end
      object LoadImage1: TMenuItem
        Caption = '&Load Image'
        Visible = False
      end
      object Editimage1: TMenuItem
        Caption = '&Edit image'
        OnClick = Editimage1Click
      end
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object Rescale1: TMenuItem
      Caption = 'Res&cale'
      object Topgraph1: TMenuItem
        Caption = '&Top graph'
        OnClick = Topgraph1Click
      end
      object Bottomgraph1: TMenuItem
        Caption = '&Bottom graph'
        OnClick = Bottomgraph1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Bothlatitudes1: TMenuItem
        Caption = '&Both latitudes'
        OnClick = Bothlatitudes1Click
      end
    end
    object Parameter1: TMenuItem
      Caption = '&Parameter'
      Visible = False
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'BMP'
    Title = 'Save BMP Image'
    Left = 368
    Top = 160
  end
  object PrintDialog1: TPrintDialog
    Left = 312
    Top = 192
  end
end
