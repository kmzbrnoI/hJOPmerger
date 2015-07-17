object F_Main: TF_Main
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Panel Merger'
  ClientHeight = 592
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MM_Main
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GB_InputFiles: TGroupBox
    Left = 0
    Top = 0
    Width = 645
    Height = 313
    Align = alTop
    Caption = ' F'#225'ze 1: Vstupn'#237' soubory '
    TabOrder = 0
    object LV_InputFiles: TListView
      Left = 2
      Top = 15
      Width = 641
      Height = 242
      Align = alTop
      Columns = <
        item
          Caption = 'Soubor'
          Width = 600
        end>
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = LV_InputFilesChange
      OnDblClick = LV_InputFilesDblClick
    end
    object B_FileAdd: TButton
      Left = 16
      Top = 272
      Width = 105
      Height = 25
      Action = F_FileOpen
      TabOrder = 1
    end
    object B_FileClose: TButton
      Left = 127
      Top = 272
      Width = 98
      Height = 25
      Action = A_FileClose
      TabOrder = 2
    end
  end
  object GB_Check: TGroupBox
    Left = 0
    Top = 313
    Width = 645
    Height = 105
    Align = alTop
    Caption = ' F'#225'ze 2: na'#269#237'st soubory a zkontrolovat data '
    TabOrder = 1
    object B_LoadAndCheck: TButton
      Left = 16
      Top = 24
      Width = 193
      Height = 25
      Action = A_Check
      TabOrder = 0
    end
  end
  object SB_Main: TStatusBar
    Left = 0
    Top = 573
    Width = 645
    Height = 19
    Panels = <>
  end
  object GB_Export: TGroupBox
    Left = 0
    Top = 418
    Width = 645
    Height = 105
    Align = alTop
    Caption = ' F'#225'ze 3: exportovat data pro server '
    TabOrder = 3
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 81
      Height = 13
      Caption = 'V'#253'stupn'#237' soubor:'
    end
    object E_OutputFileName: TEdit
      Left = 112
      Top = 24
      Width = 426
      Height = 21
      Color = cl3DLight
      ReadOnly = True
      TabOrder = 0
    end
    object B_OutputProchazet: TButton
      Left = 544
      Top = 24
      Width = 75
      Height = 22
      Caption = 'Proch'#225'zet'
      TabOrder = 1
      OnClick = B_OutputProchazetClick
    end
    object B_Export: TButton
      Left = 16
      Top = 64
      Width = 193
      Height = 25
      Action = A_Export
      TabOrder = 2
    end
  end
  object MM_Main: TMainMenu
    Left = 32
    Top = 160
    object MI_File: TMenuItem
      Caption = 'Soubor'
      object MI_FileOpen: TMenuItem
        Action = F_FileOpen
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MI_AppExit: TMenuItem
        Caption = 'Ukon'#269'it program'
        OnClick = MI_AppExitClick
      end
    end
    object MI_Help: TMenuItem
      Caption = 'N'#225'pov'#283'da'
      object MI_About: TMenuItem
        Caption = 'O programu'
        OnClick = MI_AboutClick
      end
    end
  end
  object OD_OpenOpnl: TOpenDialog
    Filter = 'Objektov'#233' soubory panelu (*.opnl)|*.opnl'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 32
    Top = 48
  end
  object SD_SaveSpnl: TSaveDialog
    Filter = 'Datab'#225'ze panel'#367' pro server (*.spnl)|*.spnl'
    Left = 32
    Top = 104
  end
  object AL_Main: TActionList
    Left = 112
    Top = 48
    object F_FileOpen: TAction
      Caption = 'Otev'#345#237't soubor...'
      ShortCut = 16463
      OnExecute = F_FileOpenExecute
    end
    object A_FileClose: TAction
      Caption = 'Zav'#345#237't soubor'
      Enabled = False
      OnExecute = A_FileCloseExecute
    end
    object A_Check: TAction
      Caption = 'Na'#269#237'st soubory a zkontrolovat data'
      Enabled = False
      OnExecute = A_CheckExecute
    end
    object A_Export: TAction
      Caption = 'Exportovat'
      Enabled = False
      OnExecute = A_ExportExecute
    end
  end
end
