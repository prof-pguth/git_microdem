object ChangeMapForm: TChangeMapForm
  Left = 0
  Top = 0
  Caption = 'Change Map'
  ClientHeight = 329
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  TextHeight = 15
  object RedrawSpeedButton12: TSpeedButton
    Left = 246
    Top = 274
    Width = 25
    Height = 25
    Hint = 'Force redraw'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFCFFFFFFFFFFFFFFCCFFFFFFFFFFFFCCCCFFFFFFFFFFFFCCCFFFFFFFF
      FFFFFCCFFFFFFFFFFFFFFCCFFFFFCCCCCCFFFCCFFFCCCCCCCCFFFCCFFFCCCCCC
      CCFFFCCFFFFFFFCCCCFFFCCCFFFFFFCCCCCFFCCCFFFFFCCCCCCFFFCCCCCCCCCC
      FCCFFFCCCCCCCCCFFCCFFFFFFFFFFFFFFCCFFFFFFFFFFFFFFFFF}
    OnClick = RedrawSpeedButton12Click
  end
  object Image1: TImage
    Left = 0
    Top = 111
    Width = 449
    Height = 105
    Align = alCustom
  end
  object OKBtn: TButton
    Left = 28
    Top = 278
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object HelpBtn: TButton
    Left = 156
    Top = 274
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 1
    OnClick = HelpBtnClick
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 441
    Height = 109
    ActivePage = Single
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 449
    object Single: TTabSheet
      Caption = 'Single cuttoff'
      object Label1: TLabel
        Left = 96
        Top = 3
        Width = 133
        Height = 15
        Caption = 'Change level to highlight'
      end
      object Edit1: TEdit
        Left = 3
        Top = 3
        Width = 81
        Height = 23
        TabOrder = 0
        Text = 'Edit1'
        OnChange = Edit1Change
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'High and low cutoffs'
      ImageIndex = 1
      object Label4: TLabel
        Left = 160
        Top = 11
        Width = 72
        Height = 15
        Caption = 'High cutoff >'
      end
      object Label5: TLabel
        Left = 160
        Top = 40
        Width = 68
        Height = 15
        Caption = 'Low cutoff <'
      end
      object Edit2: TEdit
        Left = 24
        Top = 8
        Width = 121
        Height = 23
        TabOrder = 0
        OnChange = Edit2Change
      end
      object Edit3: TEdit
        Left = 24
        Top = 40
        Width = 121
        Height = 23
        TabOrder = 1
        OnChange = Edit3Change
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Percentile cutoffs'
      ImageIndex = 2
      object Label6: TLabel
        Left = 152
        Top = 6
        Width = 85
        Height = 15
        Caption = 'High cutoff % >'
      end
      object Label7: TLabel
        Left = 152
        Top = 35
        Width = 81
        Height = 15
        Caption = 'Low cutoff % <'
      end
      object Edit4: TEdit
        Left = 16
        Top = 6
        Width = 121
        Height = 23
        TabOrder = 0
        OnChange = Edit4Change
      end
      object Edit5: TEdit
        Left = 16
        Top = 35
        Width = 121
        Height = 23
        TabOrder = 1
        OnChange = Edit5Change
      end
    end
  end
  object BitBtn1: TBitBtn
    Left = 277
    Top = 243
    Width = 124
    Height = 25
    Caption = 'Change category map'
    TabOrder = 3
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 28
    Top = 243
    Width = 75
    Height = 25
    Caption = 'Legend'
    TabOrder = 4
    OnClick = BitBtn2Click
  end
end
