object RefractionForm: TRefractionForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Refraction Model'
  ClientHeight = 242
  ClientWidth = 317
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 19
    Width = 101
    Height = 13
    Caption = 'Wave Period (T, sec)'
  end
  object Label2: TLabel
    Left = 8
    Top = 51
    Width = 73
    Height = 13
    Caption = 'Time step (sec)'
  end
  object Label3: TLabel
    Left = 8
    Top = 73
    Width = 104
    Height = 13
    Caption = 'Steps between crests'
  end
  object Label4: TLabel
    Left = 8
    Top = 99
    Width = 110
    Height = 13
    Caption = 'Number of orthogonals'
  end
  object Label5: TLabel
    Left = 8
    Top = 128
    Width = 112
    Height = 13
    Caption = 'Orthogonal spacing (m)'
  end
  object Label6: TLabel
    Left = 8
    Top = 155
    Width = 118
    Height = 13
    Caption = 'Wave propogation (deg)'
  end
  object Label7: TLabel
    Left = 184
    Top = 224
    Width = 24
    Height = 13
    Caption = 'Hrs='
  end
  object Edit1: TEdit
    Left = 128
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '2000'
  end
  object Edit2: TEdit
    Left = 128
    Top = 43
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '10'
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 187
    Width = 93
    Height = 25
    Caption = 'Wavelengths'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 107
    Top = 187
    Width = 93
    Height = 25
    Caption = 'Speed'
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object Refre: TBitBtn
    Left = 8
    Top = 218
    Width = 75
    Height = 25
    Caption = 'Refract'
    TabOrder = 4
    OnClick = RefreClick
  end
  object BitBtn3: TBitBtn
    Left = 240
    Top = 195
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 5
    OnClick = BitBtn3Click
  end
  object Edit3: TEdit
    Left = 128
    Top = 70
    Width = 121
    Height = 21
    TabOrder = 6
    Text = '30'
  end
  object Edit4: TEdit
    Left = 128
    Top = 96
    Width = 121
    Height = 21
    TabOrder = 7
    Text = '500'
  end
  object Edit5: TEdit
    Left = 126
    Top = 123
    Width = 121
    Height = 21
    TabOrder = 8
    Text = '100'
  end
  object Edit6: TEdit
    Left = 132
    Top = 150
    Width = 117
    Height = 21
    TabOrder = 9
    Text = '320'
  end
  object BitBtn4: TBitBtn
    Left = 89
    Top = 218
    Width = 75
    Height = 25
    Caption = 'Step'
    TabOrder = 10
    OnClick = BitBtn4Click
  end
end
