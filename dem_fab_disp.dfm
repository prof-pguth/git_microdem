inherited SSOCalcDlg: TSSOCalcDlg
  Left = 325
  Top = 120
  Caption = 'Topographic Fabric Calculation'
  ClientHeight = 397
  ClientWidth = 594
  Position = poDefaultSizeOnly
  OnCreate = FormCreate
  ExplicitWidth = 600
  ExplicitHeight = 426
  PixelsPerInch = 96
  TextHeight = 13
  inherited Bevel1: TBevel
    Left = 252
    Top = 400
    Width = 224
    Height = 7
    ExplicitLeft = 252
    ExplicitTop = 400
    ExplicitWidth = 224
    ExplicitHeight = 7
  end
  inherited OKBtn: TButton
    Left = 142
    Top = 364
    Width = 45
    OnClick = OKBtnClick
    ExplicitLeft = 142
    ExplicitTop = 364
    ExplicitWidth = 45
  end
  inherited CancelBtn: TButton
    Left = 193
    Top = 364
    Width = 53
    OnClick = CancelBtnClick
    ExplicitLeft = 193
    ExplicitTop = 364
    ExplicitWidth = 53
  end
  object HelpBtn: TButton
    Left = 252
    Top = 364
    Width = 45
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpBtnClick
  end
  object PageControl1: TPageControl
    Left = 1
    Top = 0
    Width = 285
    Height = 318
    ActivePage = TabSheet1
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'Organization'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 277
        Height = 290
        Align = alClient
        TabOrder = 0
        object Label3: TLabel
          Left = 8
          Top = 177
          Width = 72
          Height = 13
          Caption = 'Length Multiple'
        end
        object Label5: TLabel
          Left = 1
          Top = 130
          Width = 112
          Height = 13
          Caption = 'Flatness cutoff, s1s2 <'
        end
        object Label4: TLabel
          Left = 7
          Top = 79
          Width = 88
          Height = 13
          Caption = 'Min points for SSO'
        end
        object Label6: TLabel
          Left = 160
          Top = 25
          Width = 3
          Height = 13
        end
        object Label7: TLabel
          Left = 160
          Top = 3
          Width = 3
          Height = 13
        end
        object Label12: TLabel
          Left = 1
          Top = 111
          Width = 130
          Height = 13
          Caption = 'Organization cutoff, s2s3>'
        end
        object Label18: TLabel
          Left = 160
          Top = 56
          Width = 3
          Height = 13
        end
        object Label21: TLabel
          Left = 112
          Top = 208
          Width = 16
          Height = 13
          Caption = 'Min'
        end
        object Label23: TLabel
          Left = 182
          Top = 205
          Width = 20
          Height = 13
          Caption = 'Max'
        end
        object RedrawSpeedButton12: TSpeedButton
          Left = 200
          Top = 257
          Width = 25
          Height = 25
          Hint = 'Force redraw'
          Enabled = False
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
        object Edit3: TEdit
          Left = 94
          Top = 177
          Width = 43
          Height = 21
          TabOrder = 0
          Text = ' '
          OnChange = Edit3Change
        end
        object Edit5: TEdit
          Left = 137
          Top = 127
          Width = 43
          Height = 21
          TabOrder = 1
          Text = ' '
          OnChange = Edit5Change
        end
        object Edit4: TEdit
          Left = 113
          Top = 76
          Width = 41
          Height = 21
          TabOrder = 2
        end
        object Edit9: TEdit
          Left = 137
          Top = 103
          Width = 42
          Height = 21
          TabOrder = 3
        end
        object CheckBox1: TCheckBox
          Left = 7
          Top = 204
          Width = 97
          Height = 17
          Caption = 'Color by field'
          TabOrder = 4
          OnClick = CheckBox1Click
        end
        object Edit17: TEdit
          Left = 134
          Top = 206
          Width = 42
          Height = 21
          TabOrder = 5
          Text = 'Edit17'
          OnChange = Edit17Change
        end
        object Edit18: TEdit
          Left = 208
          Top = 202
          Width = 51
          Height = 21
          TabOrder = 6
          Text = 'Edit18'
          OnChange = Edit18Change
        end
      end
    end
  end
end
