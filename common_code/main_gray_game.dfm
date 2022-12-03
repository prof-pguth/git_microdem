object GrayGameForm: TGrayGameForm
  Left = 221
  Top = 179
  Caption = 'The Grayscale Game'
  ClientHeight = 527
  ClientWidth = 566
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 566
    Height = 527
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    OnEnter = PageControl1Enter
    ExplicitWidth = 560
    ExplicitHeight = 518
    object TabSheet1: TTabSheet
      Caption = 'Grayscale game'
      object Label1: TLabel
        Left = 160
        Top = 8
        Width = 32
        Height = 13
        Caption = 'Label1'
      end
      object Image1: TImage
        Left = 48
        Top = 40
        Width = 209
        Height = 193
      end
      object Image2: TImage
        Left = 259
        Top = 40
        Width = 209
        Height = 193
      end
      object Label3: TLabel
        Left = 40
        Top = 336
        Width = 49
        Height = 13
        Caption = 'Difference'
      end
      object Label2: TLabel
        Left = 32
        Top = 368
        Width = 47
        Height = 13
        Caption = 'Gray level'
      end
      object RadioGroup1: TRadioGroup
        Left = 147
        Top = 248
        Width = 321
        Height = 57
        Caption = 'Is image on the right'
        Columns = 3
        Items.Strings = (
          'Darker'
          'Same'
          'Brighter')
        TabOrder = 0
        OnClick = RadioGroup1Click
      end
      object BitBtn1: TBitBtn
        Left = 40
        Top = 416
        Width = 75
        Height = 25
        Caption = 'BitBtn1'
        TabOrder = 1
        Visible = False
        OnClick = BitBtn1Click
      end
      object BitBtn2: TBitBtn
        Left = 128
        Top = 416
        Width = 75
        Height = 25
        Caption = 'Color'
        TabOrder = 2
        Visible = False
      end
      object Edit2: TEdit
        Left = 128
        Top = 360
        Width = 121
        Height = 21
        TabOrder = 3
        Text = '150'
        OnChange = Edit2Change
      end
      object Edit1: TEdit
        Left = 128
        Top = 328
        Width = 121
        Height = 21
        TabOrder = 4
        Text = '5'
        OnChange = Edit1Change
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Colors game'
      ImageIndex = 1
      object Image3: TImage
        Left = 16
        Top = 16
        Width = 209
        Height = 177
      end
      object Image4: TImage
        Left = 264
        Top = 16
        Width = 225
        Height = 177
      end
      object Label4: TLabel
        Left = 287
        Top = 256
        Width = 63
        Height = 24
        Caption = 'Label4'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label5: TLabel
        Left = 288
        Top = 312
        Width = 63
        Height = 24
        Caption = 'Label5'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label6: TLabel
        Left = 288
        Top = 376
        Width = 63
        Height = 24
        Caption = 'Label6'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = Label6Click
      end
      object Label7: TLabel
        Left = 16
        Top = 440
        Width = 149
        Height = 24
        Caption = 'Average score: '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object TrackBar1: TTrackBar
        Left = 16
        Top = 240
        Width = 265
        Height = 45
        Max = 255
        Frequency = 10
        TabOrder = 0
        OnChange = TrackBar1Change
      end
      object TrackBar2: TTrackBar
        Left = 16
        Top = 304
        Width = 265
        Height = 45
        Max = 255
        Frequency = 10
        TabOrder = 1
        OnChange = TrackBar2Change
      end
      object BitBtn3: TBitBtn
        Left = 384
        Top = 440
        Width = 75
        Height = 25
        Caption = 'Restart'
        TabOrder = 2
        OnClick = BitBtn3Click
      end
      object BitBtn4: TBitBtn
        Left = 465
        Top = 440
        Width = 75
        Height = 25
        Caption = 'Solve'
        TabOrder = 3
        OnClick = BitBtn4Click
      end
      object CheckBox1: TCheckBox
        Left = 432
        Top = 471
        Width = 97
        Height = 17
        Caption = 'Cheat mode'
        TabOrder = 4
        OnClick = CheckBox1Click
      end
      object TrackBar3: TTrackBar
        Left = 17
        Top = 355
        Width = 265
        Height = 45
        Max = 255
        Frequency = 10
        TabOrder = 5
        OnChange = TrackBar3Change
      end
      object Darker: TBitBtn
        Left = 328
        Top = 406
        Width = 75
        Height = 25
        Caption = 'Darker'
        TabOrder = 6
        OnClick = DarkerClick
      end
      object BitBtn5: TBitBtn
        Left = 414
        Top = 409
        Width = 75
        Height = 25
        Caption = 'Lighter'
        TabOrder = 7
        OnClick = BitBtn5Click
      end
      object Edit3: TEdit
        Left = 495
        Top = 413
        Width = 47
        Height = 21
        TabOrder = 8
        Text = '5'
        OnChange = Edit3Change
      end
    end
  end
end
