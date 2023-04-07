object XYZDisplayForm: TXYZDisplayForm
  Left = 614
  Top = 188
  Caption = 'XYZ data'
  ClientHeight = 346
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Menu = MainMenu1
  Position = poDefaultSizeOnly
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 35
    Width = 374
    Height = 292
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 327
    Width = 374
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 349
    ExplicitWidth = 402
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 374
    Height = 35
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 402
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 13
      Height = 13
      Caption = 'Z='
    end
    object Label2: TLabel
      Left = 96
      Top = 8
      Width = 21
      Height = 13
      Caption = 'Tol='
    end
    object Edit1: TEdit
      Left = 24
      Top = 3
      Width = 49
      Height = 21
      TabOrder = 0
      Text = '-9999'
    end
    object Edit2: TEdit
      Left = 120
      Top = 3
      Width = 49
      Height = 21
      TabOrder = 1
      OnChange = Edit2Change
    end
    object CheckBox1: TCheckBox
      Left = 192
      Top = 8
      Width = 97
      Height = 17
      Caption = 'feet'
      TabOrder = 2
    end
  end
  object DataSource1: TDataSource
    Left = 248
    Top = 80
  end
  object MainMenu1: TMainMenu
    Left = 144
    Top = 64
    object File1: TMenuItem
      Caption = 'File'
      object Opennew1: TMenuItem
        Caption = 'Open new '
        OnClick = Opennew1Click
      end
      object Append1: TMenuItem
        Caption = 'Append'
        OnClick = Append1Click
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
    object Digitizemode1: TMenuItem
      Caption = 'Digitize mode'
      Enabled = False
      object Stream1: TMenuItem
        Caption = 'Stream'
        OnClick = Stream1Click
      end
      object Point1: TMenuItem
        Caption = 'Point'
        OnClick = Point1Click
      end
    end
    object Zentry1: TMenuItem
      Caption = 'Z entry'
      Enabled = False
      object Editbox1: TMenuItem
        Caption = 'Edit box'
        OnClick = Editbox1Click
      end
      object DEM1: TMenuItem
        Caption = 'DEM'
        OnClick = DEM1Click
      end
      object DEMconstant1: TMenuItem
        Caption = 'DEM + constant'
        OnClick = DEMconstant1Click
      end
    end
    object Edit3: TMenuItem
      Caption = 'Edit'
      Enabled = False
      object Dragpoints1: TMenuItem
        Caption = 'Drag points'
        OnClick = Dragpoints1Click
      end
      object Deletepoints1: TMenuItem
        Caption = 'Delete points'
        OnClick = Deletepoints1Click
      end
      object Editzvalue1: TMenuItem
        Caption = 'Edit z value'
        OnClick = Editzvalue1Click
      end
    end
    object Plotpoints1: TMenuItem
      Caption = 'Plot points'
      Enabled = False
      OnClick = Plotpoints1Click
    end
  end
end
