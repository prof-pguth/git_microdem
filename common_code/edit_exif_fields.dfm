object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Add EXIF fields'
  ClientHeight = 291
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Visible = True
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 37
    Height = 15
    Caption = 'Author'
  end
  object Label2: TLabel
    Left = 24
    Top = 48
    Width = 39
    Height = 15
    Caption = 'Subject'
  end
  object Label3: TLabel
    Left = 24
    Top = 79
    Width = 59
    Height = 15
    Caption = 'Comments'
  end
  object Label4: TLabel
    Left = 120
    Top = 120
    Width = 34
    Height = 15
    Caption = 'Label4'
  end
  object Edit1: TEdit
    Left = 80
    Top = 21
    Width = 121
    Height = 23
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 80
    Top = 50
    Width = 121
    Height = 23
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 32
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Location'
    TabOrder = 2
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 40
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Process files'
    TabOrder = 3
    OnClick = BitBtn2Click
  end
  object Edit3: TEdit
    Left = 104
    Top = 79
    Width = 97
    Height = 23
    TabOrder = 4
    Text = 'Edit3'
  end
end
