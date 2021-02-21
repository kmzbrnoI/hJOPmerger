object F_DataCheck: TF_DataCheck
  Left = 0
  Top = 0
  Caption = 'V'#253'stup kontroly dat'
  ClientHeight = 570
  ClientWidth = 686
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LV_Errors: TListView
    Left = 0
    Top = 0
    Width = 686
    Height = 570
    Align = alClient
    Columns = <
      item
        Caption = 'Index'
      end
      item
        Caption = 'Hl'#225#353'ka'
        Width = 600
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = LV_ErrorsCustomDrawItem
  end
end
