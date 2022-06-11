object OutputFrame: TOutputFrame
  Left = 0
  Top = 0
  Width = 544
  Height = 360
  TabOrder = 0
  object grpThumbnail: TGroupBox
    AlignWithMargins = True
    Left = 441
    Top = 0
    Width = 103
    Height = 360
    Margins.Left = 6
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alRight
    Caption = ' Thumbnail '
    TabOrder = 0
    Visible = False
    object imgThumbnail: TImage
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 93
      Height = 337
      Align = alClient
      ExplicitLeft = 4
      ExplicitTop = 16
      ExplicitWidth = 95
      ExplicitHeight = 331
    end
  end
  object grpTags: TGroupBox
    Left = 0
    Top = 0
    Width = 435
    Height = 360
    Align = alClient
    Caption = ' Tags '
    Padding.Left = 6
    Padding.Top = 3
    Padding.Right = 6
    TabOrder = 1
    object TabSet: TTabSet
      Left = 8
      Top = 337
      Width = 419
      Height = 21
      Align = alBottom
      DitherBackground = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      SelectedColor = clWindow
      SoftTop = True
      Style = tsSoftTabs
      Tabs.Strings = (
        '   &Standard '
        '   &MakerNote')
      TabIndex = 0
      UnselectedColor = clBtnFace
      OnChange = TabSetChange
    end
    object panListViewHost: TPanel
      Left = 8
      Top = 18
      Width = 419
      Height = 319
      Align = alClient
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 1
      object shpTop: TShape
        Left = 0
        Top = 0
        Width = 419
        Height = 1
        Align = alTop
        Pen.Color = clBtnShadow
        ExplicitWidth = 390
      end
      object shpLeft: TShape
        Left = 0
        Top = 1
        Width = 1
        Height = 318
        Align = alLeft
        Pen.Color = clBtnShadow
        ExplicitTop = 0
        ExplicitHeight = 390
      end
      object shpRight: TShape
        Left = 418
        Top = 1
        Width = 1
        Height = 318
        Align = alRight
        Pen.Color = clBtnShadow
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitHeight = 390
      end
      object lsvStandard: TListView
        AlignWithMargins = True
        Left = 1
        Top = 1
        Width = 417
        Height = 317
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 1
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Name'
            Width = 200
          end
          item
            Caption = 'Value'
            Width = 200
          end>
        ColumnClick = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
end
