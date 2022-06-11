inherited IHSMergeForm: TIHSMergeForm
  Left = 537
  Top = 153
  Caption = 'IHS DEM/Image Merge'
  ClientHeight = 295
  ClientWidth = 366
  Position = poDefaultSizeOnly
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitLeft = 3
  ExplicitTop = 3
  ExplicitWidth = 384
  ExplicitHeight = 342
  PixelsPerInch = 96
  TextHeight = 20
  inherited Bevel1: TBevel
    Width = 345
    Height = 238
    ExplicitWidth = 345
    ExplicitHeight = 238
  end
  object TLabel [1]
    Left = 24
    Top = 16
    Width = 55
    Height = 20
    Caption = 'Intensity'
  end
  object Label1: TLabel [2]
    Left = 24
    Top = 64
    Width = 27
    Height = 20
    Caption = 'Hue'
  end
  object Label2: TLabel [3]
    Left = 24
    Top = 112
    Width = 68
    Height = 20
    Caption = 'Saturation'
  end
  object Label3: TLabel [4]
    Left = 232
    Top = 16
    Width = 44
    Height = 20
    Caption = 'Label3'
  end
  object Label4: TLabel [5]
    Left = 232
    Top = 64
    Width = 44
    Height = 20
    Caption = 'Label4'
  end
  object Label5: TLabel [6]
    Left = 232
    Top = 112
    Width = 44
    Height = 20
    Caption = 'Label5'
  end
  object Image1: TImage [7]
    Left = 16
    Top = 192
    Width = 256
    Height = 25
  end
  inherited OKBtn: TButton
    Left = 40
    Top = 252
    ExplicitLeft = 40
    ExplicitTop = 252
  end
  inherited CancelBtn: TButton
    Left = 136
    Top = 252
    ExplicitLeft = 136
    ExplicitTop = 252
  end
  object HelpBtn: TButton
    Left = 232
    Top = 252
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
  end
  object ScrollBar1: TScrollBar
    Left = 32
    Top = 40
    Width = 289
    Height = 16
    Max = 255
    PageSize = 0
    TabOrder = 3
    OnChange = ScrollBar1Change
  end
  object ScrollBar2: TScrollBar
    Left = 32
    Top = 90
    Width = 289
    Height = 16
    Max = 255
    PageSize = 0
    TabOrder = 4
    OnChange = ScrollBar2Change
  end
  object ScrollBar3: TScrollBar
    Left = 32
    Top = 138
    Width = 289
    Height = 16
    Max = 255
    PageSize = 0
    TabOrder = 5
    OnChange = ScrollBar3Change
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 163
    Width = 202
    Height = 17
    Caption = 'Saturation from reflectance'
    TabOrder = 6
    Visible = False
    OnClick = CheckBox1Click
  end
  object Button1: TButton
    Left = 232
    Top = 160
    Width = 113
    Height = 25
    Caption = 'Reflectance'
    TabOrder = 7
    Visible = False
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 280
    Top = 192
    Width = 65
    Height = 25
    Caption = 'Defaults'
    TabOrder = 8
    OnClick = Button2Click
  end
end
