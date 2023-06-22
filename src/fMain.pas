unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ActnList, Panel, StrUtils, System.Actions;

const
  _SUF_OBJECT = '.opnl';
  _SUF_SERVER = '.spnl';

type
  TF_Main = class(TForm)
    MM_Main: TMainMenu;
    MI_Help: TMenuItem;
    MI_About: TMenuItem;
    MI_File: TMenuItem;
    MI_FileOpen: TMenuItem;
    N1: TMenuItem;
    MI_AppExit: TMenuItem;
    OD_OpenOpnl: TOpenDialog;
    SD_SaveSpnl: TSaveDialog;
    GB_InputFiles: TGroupBox;
    LV_InputFiles: TListView;
    B_FileAdd: TButton;
    B_FileClose: TButton;
    GB_Check: TGroupBox;
    B_LoadAndCheck: TButton;
    AL_Main: TActionList;
    F_FileOpen: TAction;
    A_FileClose: TAction;
    A_Check: TAction;
    GB_Export: TGroupBox;
    Label1: TLabel;
    E_OutputFileName: TEdit;
    B_OutputProchazet: TButton;
    B_Export: TButton;
    A_Export: TAction;
    procedure MI_AboutClick(Sender: TObject);
    procedure LV_InputFilesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure F_FileOpenExecute(Sender: TObject);
    procedure A_FileCloseExecute(Sender: TObject);
    procedure A_CheckExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_OutputProchazetClick(Sender: TObject);
    procedure A_ExportExecute(Sender: TObject);
    procedure MI_AppExitClick(Sender: TObject);
    procedure LV_InputFilesDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    Relief: TRelief;

    procedure AddFile(filename: string);
  end;

var
  F_Main: TF_Main;

implementation

{$R *.dfm}

uses Verze, fDataCheck;

procedure TF_Main.A_CheckExecute(Sender: TObject);
var fnames: TStrings;
begin
  fnames := TStringList.Create();
  try
    for var i := 0 to Self.LV_InputFiles.Items.Count - 1 do
      fnames.Add(Self.LV_InputFiles.Items.Item[i].Caption);

    try
      Self.Relief.FilesLoad(fnames);
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Chyba při načítání souborů reliéfů:' + #13#10 + E.Message + #13#10 +
          'Opravte chyby v souboru panelu a načtětě soubor znovu!'), 'Chyba', MB_OK OR MB_ICONERROR);
        Exit();
      end;
    end;
  finally
    fnames.Free();
  end;

  try
    Self.Relief.Merge();
  except
    on E: Exception do
    begin
      Application.MessageBox(PChar('Chyba při spojování souborů reliéfů:' + #13#10 + E.Message), 'Chyba',
        MB_OK OR MB_ICONERROR);
      Exit();
    end;
  end;

  try
    fnames := Self.Relief.CheckValid();
    F_DataCheck.OpenForm(fnames);
  except
    on E: Exception do
    begin
      Application.MessageBox(PChar('Chyba při kontrole validity souborů reliéfů:' + #13#10 + E.Message), 'Chyba',
        MB_OK OR MB_ICONERROR);
      Exit();
    end;
  end;

  Self.A_Export.Enabled := true;
end;

procedure TF_Main.A_ExportExecute(Sender: TObject);
begin
  try
    Self.Relief.ExportData(Self.E_OutputFileName.Text);
    Application.MessageBox(PChar('Export proběhl úspěšně' + #13#10 + 'Vytvořen soubor ' + Self.E_OutputFileName.Text),
      'Info', MB_OK OR MB_ICONINFORMATION);
  except
    on E: Exception do
      Application.MessageBox(PChar('Export se nezdařil:' + #13#10 + E.Message), 'Varování', MB_OK OR MB_ICONWARNING);
  end;
end;

procedure TF_Main.A_FileCloseExecute(Sender: TObject);
begin
  Self.LV_InputFiles.Items.Delete(Self.LV_InputFiles.ItemIndex);
  if (Self.LV_InputFiles.Items.Count = 0) then
    Self.A_Check.Enabled := false;
  Self.A_Export.Enabled := false;
end;

procedure TF_Main.B_OutputProchazetClick(Sender: TObject);
begin
  if (Self.SD_SaveSpnl.Execute(Self.Handle)) then
  begin
    Self.E_OutputFileName.Text := Self.SD_SaveSpnl.filename;
    if (RightStr(Self.E_OutputFileName.Text, 5) <> '.spnl') then
      Self.E_OutputFileName.Text := Self.E_OutputFileName.Text + '.spnl';
  end;
end;

procedure TF_Main.FormCreate(Sender: TObject);
begin
  Self.Relief := TRelief.Create();
  Self.Caption := 'hJOPmerger – v'+Verze.GetVer(Application.ExeName);
end;

procedure TF_Main.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Self.Relief);
end;

procedure TF_Main.F_FileOpenExecute(Sender: TObject);
begin
  if (Self.OD_OpenOpnl.Execute(Self.Handle)) then
  begin
    for var i := 0 to Self.OD_OpenOpnl.Files.Count - 1 do
      Self.AddFile(Self.OD_OpenOpnl.Files[i]);
  end; // if
end;

procedure TF_Main.LV_InputFilesChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  Self.A_FileClose.Enabled := ((Sender as TListView).Selected <> nil);
end;

procedure TF_Main.LV_InputFilesDblClick(Sender: TObject);
begin
  if ((Sender as TListView).Selected = nil) then
  begin
    Self.F_FileOpenExecute(Self);
  end else begin
    Self.A_FileCloseExecute(Self);
  end;
end;

procedure TF_Main.MI_AboutClick(Sender: TObject);
begin
  Application.MessageBox(PChar('hJOPmerger' + #13#10 + 'Verze: ' + GetVer(Application.ExeName) + #13#10 +
    'Vytvořil Jan Horáček (c) 2013-2021 pro KMŽ Brno I'), 'Info', MB_OK OR MB_ICONINFORMATION);
end;

procedure TF_Main.MI_AppExitClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_Main.AddFile(filename: string);
var LI: TListItem;
begin
  LI := Self.LV_InputFiles.Items.Add;
  LI.Caption := filename;

  Self.A_Check.Enabled := true;
  if (Self.A_Export.Enabled) then
    Self.A_Export.Enabled := false;
end;

end.// unit
