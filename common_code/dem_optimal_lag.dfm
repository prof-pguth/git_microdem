object LagOptionsForm: TLagOptionsForm
  Left = 73
  Top = 256
  Caption = 'Grid shift/migration'
  ClientHeight = 364
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Memo1: TMemo
    Left = 217
    Top = 0
    Width = 301
    Height = 364
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 217
    Height = 364
    Align = alLeft
    TabOrder = 1
    object Label2: TLabel
      Left = 16
      Top = 240
      Width = 97
      Height = 13
      Caption = 'Lag sampling (pixels)'
    end
    object Label1: TLabel
      Left = 16
      Top = 216
      Width = 124
      Height = 13
      Caption = 'Correlation window (pixels)'
    end
    object Label4: TLabel
      Left = 13
      Top = 138
      Width = 32
      Height = 13
      Caption = 'Label4'
    end
    object Label3: TLabel
      Left = 13
      Top = 119
      Width = 32
      Height = 13
      Caption = 'Label3'
    end
    object HelpBtn: TBitBtn
      Left = 111
      Top = 329
      Width = 100
      Height = 27
      Kind = bkHelp
      Margin = 2
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      OnClick = HelpBtnClick
      IsControl = True
    end
    object OKBtn: TBitBtn
      Left = 8
      Top = 329
      Width = 77
      Height = 27
      Caption = 'Close'
      Default = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      Margin = 2
      ModalResult = 1
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 1
      OnClick = OKBtnClick
      IsControl = True
    end
    object BitBtn2: TBitBtn
      Left = 111
      Top = 291
      Width = 98
      Height = 32
      Caption = 'Radius sensitivity'
      TabOrder = 2
      OnClick = BitBtn2Click
    end
    object BitBtn1: TBitBtn
      Left = 8
      Top = 291
      Width = 97
      Height = 32
      Caption = 'Compute'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = BitBtn1Click
    end
    object CheckBox1: TCheckBox
      Left = 24
      Top = 264
      Width = 97
      Height = 17
      Caption = 'All open DEMs'
      TabOrder = 4
    end
    object Edit6: TEdit
      Left = 146
      Top = 240
      Width = 58
      Height = 21
      TabOrder = 5
      Text = 'Edit6'
    end
    object Edit5: TEdit
      Left = 146
      Top = 213
      Width = 58
      Height = 21
      TabOrder = 6
      Text = 'Edit5'
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 157
      Width = 165
      Height = 41
      Caption = 'Compute'
      Columns = 2
      DragKind = dkDock
      Items.Strings = (
        'Full DEM'
        'Map area')
      TabOrder = 7
      OnClick = RadioGroup1Click
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 185
      Height = 105
      Caption = 'Exploration window (pixels)'
      TabOrder = 8
      object Edit1: TEdit
        Left = 48
        Top = 20
        Width = 49
        Height = 21
        TabOrder = 0
        Text = '40'
      end
      object Edit3: TEdit
        Left = 3
        Top = 47
        Width = 57
        Height = 21
        TabOrder = 1
        Text = '-15'
      end
      object Edit4: TEdit
        Left = 74
        Top = 47
        Width = 42
        Height = 21
        TabOrder = 2
        Text = '60'
      end
      object Edit2: TEdit
        Left = 48
        Top = 74
        Width = 49
        Height = 21
        TabOrder = 3
        Text = '-15'
      end
    end
  end
end
