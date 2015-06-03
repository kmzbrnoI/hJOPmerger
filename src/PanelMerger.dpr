program PanelMerger;

//tento program slucuje naeditovane panely (*.opnl) do 1 souboru, ktery potrebuje server (= technologie) (*.spnl)

uses
  Forms,
  Main in 'Main.pas' {F_Main},
  Verze in 'Verze.pas',
  Panel in 'Panel.pas',
  OblastRizeni in 'OblastRizeni.pas',
  frmDataCheck in 'frmDataCheck.pas' {F_DataCheck};

{$R *.res}

var i:Integer;

begin
  Application.Initialize;
  Application.Title := 'Panel Merger';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_DataCheck, F_DataCheck);

  if (ParamCount > 0) then
   begin
    //open the file
    for i := 1 to ParamCount do
    F_Main.AddFile(ParamStr(i));
   end;

  Application.Run;
end.
