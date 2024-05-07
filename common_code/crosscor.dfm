inherited CrossCorrelationForm: TCrossCorrelationForm
  Left = 864
  Top = 235
  Caption = 'Cross Correlation Options'
  ClientHeight = 322
  ClientWidth = 431
  FormStyle = fsStayOnTop
  Visible = True
  StyleElements = [seFont, seClient, seBorder]
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  ExplicitWidth = 447
  ExplicitHeight = 361
  TextHeight = 15
  object Label1: TLabel [1]
    Left = 304
    Top = 112
    Width = 34
    Height = 15
    Caption = 'Label1'
  end
  object Label2: TLabel [2]
    Left = 304
    Top = 144
    Width = 34
    Height = 15
    Caption = 'Label2'
  end
  inherited OKBtn: TButton
    OnClick = OKBtnClick
  end
  inherited CancelBtn: TButton
    Visible = False
    OnClick = CancelBtnClick
  end
  object HelpBtn: TButton
    Left = 295
    Top = 81
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object Button1: TButton
    Left = 8
    Top = 175
    Width = 75
    Height = 25
    Caption = 'Show Lag'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 167
    Top = 175
    Width = 114
    Height = 25
    Caption = 'Correlations'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 96
    Top = 176
    Width = 65
    Height = 23
    TabOrder = 5
    Text = '0'
  end
  object BitBtn1: TBitBtn
    Left = 304
    Top = 176
    Width = 97
    Height = 25
    Caption = 'Scatter plot'
    TabOrder = 6
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 304
    Top = 208
    Width = 97
    Height = 25
    Caption = 'Offset time'
    TabOrder = 7
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 304
    Top = 240
    Width = 97
    Height = 25
    Caption = 'Correlogram'
    TabOrder = 8
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 304
    Top = 272
    Width = 97
    Height = 25
    Caption = 'Time series'
    TabOrder = 9
    OnClick = BitBtn4Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 281
    Height = 161
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 10
  end
  object CheckBox1: TCheckBox
    Left = 96
    Top = 223
    Width = 144
    Height = 17
    Caption = 'Close graphs'
    TabOrder = 11
  end
end
