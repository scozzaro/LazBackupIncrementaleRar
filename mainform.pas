unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FileUtil, Process, LazFileUtils, DateTimePicker, FileCtrl, StrUtils, ComCtrls,
  Menus, EditBtn, Buttons, AboutForm, fpjson, jsonparser, jsonconf;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    AboutMenuItem: TMenuItem;
    AddButton: TButton;
    AddExcludeButton: TButton;
    ArchiveNameEdit: TEdit;
    ArchiveNameLabel: TLabel;
    BackupNameLabel: TLabel;
    BrowseRARButton: TButton;
    Button1: TButton;
    chkSpegni: TCheckBox;
    chkStartTime: TCheckBox;
    Label1: TLabel;
    Visualizza: TMenuItem;
    PopupMenuTray: TPopupMenu;
    StartTime: TDateTimePicker;
    DestButton: TButton;
    DestinationEdit: TEdit;
    DestinationLabel: TLabel;
    ExcludeLabel: TLabel;
    ExcludeListbox: TListBox;
    FileMenu: TMenuItem;
    FoldersLabel: TLabel;
    FoldersListbox: TListBox;
    InfoMenu: TMenuItem;
    LicenseMenuItem: TMenuItem;
    LoadConfigMenuItem: TMenuItem;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    QuitMenuItem: TMenuItem;
    RARPathEdit: TEdit;
    RARPathLabel: TLabel;
    RemoveButton: TButton;
    RemoveExcludeButton: TButton;
    RunBackupButton: TButton;
    ProgressBar: TProgressBar;
    ProgressLabel: TLabel;
    SaveConfigMenuItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    ScrolledOutput: TMemo;
    StatusLabel: TLabel;
    TimerStartTime: TTimer;
    TrayIcon1: TTrayIcon;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure AddExcludeButtonClick(Sender: TObject);
    procedure BrowseRARButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkStartTimeChange(Sender: TObject);
    procedure DestButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadConfigMenuItemClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure RemoveExcludeButtonClick(Sender: TObject);
    procedure RunBackupButtonClick(Sender: TObject);
    procedure SaveConfigMenuItemClick(Sender: TObject);
    procedure TimerStartTimeTimer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure VisualizzaClick(Sender: TObject);
  private
    FProcess: TProcess;
    FTotalFiles: integer;
    FProcessedFiles: integer;

    procedure OnProcessOutput(const ALine: string);
    procedure PreCalculateFiles(const SourceDir: string);
    procedure LoadConfigFromFile(const AFileName: string);
  public
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}



{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
var
  ConfigPath: string;
begin
  FProcess := TProcess.Create(Self);
  FProcess.Options := [poUsePipes, poStderrToOutput];

  ConfigPath := GetUserDir + 'Documents' + PathDelim + 'backup_configLaz.rbak';

  if FileExists(ConfigPath) then
    LoadConfigFromFile(ConfigPath);
end;

procedure TFrmMain.AddButtonClick(Sender: TObject);
var
  Folder: string;
begin
  if SelectDirectory('Seleziona la cartella da salvare', '', Folder) then
  begin
    if (Folder <> '') and (FoldersListbox.Items.IndexOf(Folder) = -1) then
      FoldersListbox.Items.Add(Folder)
    else
      ShowMessage('La cartella è già stata aggiunta.');
  end;
end;

procedure TFrmMain.AboutMenuItemClick(Sender: TObject);
begin


    FrmAbout.ShowModal;


end;

procedure TFrmMain.RemoveButtonClick(Sender: TObject);
var
  Index: integer;
begin
  Index := FoldersListbox.ItemIndex;
  if Index <> -1 then
    FoldersListbox.Items.Delete(Index);
end;

procedure TFrmMain.AddExcludeButtonClick(Sender: TObject);
var
  Pattern: string;
begin
  Pattern := InputBox('Aggiungi esclusione',
    'Inserisci estensione/cartella da escludere (es: *.tmp):', '');
  if (Pattern <> '') and (ExcludeListbox.Items.IndexOf(Pattern) = -1) then
    ExcludeListbox.Items.Add(Pattern);
end;

procedure TFrmMain.RemoveExcludeButtonClick(Sender: TObject);
var
  Index: integer;
begin
  Index := ExcludeListbox.ItemIndex;
  if Index <> -1 then
    ExcludeListbox.Items.Delete(Index);
end;

procedure TFrmMain.DestButtonClick(Sender: TObject);
var
  DestFolder: string;
begin
  if SelectDirectory('Seleziona la cartella di destinazione', '', DestFolder) then
    DestinationEdit.Text := DestFolder;
end;

procedure TFrmMain.BrowseRARButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    RARPathEdit.Text := OpenDialog1.FileName;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
    // Nasconde la finestra principale
  Application.Minimize;
  Self.Hide;

  // Mostra icona nella barra di stato (tray)
  TrayIcon1.Visible := True;
  TrayIcon1.Hint := 'Backup App - in esecuzione';
  TrayIcon1.PopupMenu := PopupMenuTray; // collega il menu
end;

procedure TFrmMain.chkStartTimeChange(Sender: TObject);
begin
   // abilita/disabilita il timer a seconda dello stato della checkbox
  TimerStartTime.Enabled := chkStartTime.Checked;
end;

procedure TFrmMain.PreCalculateFiles(const SourceDir: string);
var
  FileList: TStringList;
begin
  FileList := TStringList.Create;
  try
    FindAllFiles(FileList, SourceDir, '*', True);
    Inc(FTotalFiles, FileList.Count);  // usa Inc invece di assegnare
  finally
    FileList.Free;
  end;
end;


procedure TFrmMain.OnProcessOutput(const ALine: string);
begin
  if ALine = '' then Exit;
  ScrolledOutput.Lines.Add(ALine);
  Inc(FProcessedFiles);

  if FTotalFiles > 0 then
  begin
    //ProgressBar.Max := FTotalFiles;
    //ProgressBar.Position := FProcessedFiles;
    ProgressLabel.Caption := Format('Avanzamento: %d%% (%d di %d file)',
      [Round((FProcessedFiles / FTotalFiles) * 100), FProcessedFiles, FTotalFiles]);
  end
  else
  begin
    ScrolledOutput.Lines.Add(IntToStr(FTotalFiles));
  end;
end;

procedure TFrmMain.RunBackupButtonClick(Sender: TObject);
function GetHomeDir: string;
begin
  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME');  // su Linux/macOS
  if Result = '' then
    Result := '/tmp'; // fallback se HOME non è impostata
  {$ELSE}
  Result := GetEnvironmentVariable('USERPROFILE'); // su Windows
  if Result = '' then
    Result := 'C:\'; // fallback se USERPROFILE non è impostata
  {$ENDIF}
end;
var
  Buffer: array[0..1023] of byte;
  BytesRead: LongInt;
  Line: string;
  i: Integer;
begin
  ScrolledOutput.Lines.Clear;
  FProcessedFiles := 0;
  FTotalFiles := 0;

  // Calcola il numero totale di file da processare
  for i := 0 to FoldersListbox.Items.Count - 1 do
    PreCalculateFiles(FoldersListbox.Items[i]);

  // Configurazione del processo RAR
  FProcess.Executable := RARPathEdit.Text;
  FProcess.Parameters.Clear;
  FProcess.Parameters.Add('a');                 // aggiungi file all'archivio
  FProcess.Parameters.Add('-u');                // aggiorna solo file modificati o nuovi
  FProcess.Parameters.Add('-r');                // include sottocartelle
  //FProcess.Parameters.Add('-o');                // include sottocartelle
  FProcess.Parameters.Add(DestinationEdit.Text + PathDelim + ArchiveNameEdit.Text);
  FProcess.Parameters.AddStrings(FoldersListbox.Items);

  FProcess.Options := [poUsePipes, poStderrToOutput, poNoConsole];

  FProcess.CurrentDirectory := GetHomeDir;


  FProcess.Execute;

  // Legge l'output mentre il processo è in esecuzione
  while FProcess.Running or (FProcess.Output.NumBytesAvailable > 0) do
  begin
    if FProcess.Output.NumBytesAvailable > 0 then
    begin
      BytesRead := FProcess.Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
      begin
        SetString(Line, PAnsiChar(@Buffer[0]), BytesRead);
        // Aggiunge l'output riga per riga
        ScrolledOutput.Lines.Text := ScrolledOutput.Lines.Text + Line;
        ScrolledOutput.SelStart := Length(ScrolledOutput.Text);
        //ScrolledOutput.Perform(EM_SCROLLCARET, 0, 0);
      end;
    end;
    Application.ProcessMessages;
    Sleep(50);
  end;

  // Assicura che l'ultimo output venga catturato
  while FProcess.Output.NumBytesAvailable > 0 do
  begin
    BytesRead := FProcess.Output.Read(Buffer, SizeOf(Buffer));
    if BytesRead > 0 then
    begin
      SetString(Line, PAnsiChar(@Buffer[0]), BytesRead);
      ScrolledOutput.Lines.Text := ScrolledOutput.Lines.Text + Line;
    end;
  end;

    ProgressLabel.Caption := Format('Avanzamento: %d%% (%d di %d file)',
    [Round(100), FTotalFiles, FTotalFiles]);

  // 🔹 Spegnimento del computer se chkSpegni è attivo
  // 🔹 Spegnimento del computer se chkSpegni è attivo
   if chkSpegni.Checked then
   begin
     FProcess.CloseInput; // chiude eventuali pipe precedenti
     FProcess.Parameters.Clear;

     {$IFDEF WINDOWS}
     FProcess.Executable := 'shutdown';
     FProcess.Parameters.Add('-s');
     FProcess.Parameters.Add('-t');
     FProcess.Parameters.Add('0');
     {$ENDIF}

     {$IFDEF LINUX}
     FProcess.Executable := '/sbin/shutdown';
     FProcess.Parameters.Add('-h');
     FProcess.Parameters.Add('now');
     {$ENDIF}

   {$IFDEF DARWIN} // macOS
FProcess.Executable := '/usr/bin/osascript';
FProcess.Parameters.Add('-e');
FProcess.Parameters.Add('tell application "System Events" to shut down');
{$ENDIF}

     FProcess.Options := [];
     FProcess.Execute;
   end;

end;


procedure TFrmMain.LoadConfigFromFile(const AFileName: string);
var
  JSONObj: TJSONObject;
  FoldersArray, ExcludesArray: TJSONArray;
  i: integer;
begin
  JSONObj := TJSONObject(GetJSON(ReadFileToString(AFileName)));
  try
    FoldersListbox.Items.Clear;
    ExcludeListbox.Items.Clear;

    FoldersArray := JSONObj.Arrays['folders'];
    if Assigned(FoldersArray) then
      for i := 0 to FoldersArray.Count - 1 do
        FoldersListbox.Items.Add(FoldersArray.Items[i].AsString);

    ExcludesArray := JSONObj.Arrays['excludes'];
    if Assigned(ExcludesArray) then
      for i := 0 to ExcludesArray.Count - 1 do
        ExcludeListbox.Items.Add(ExcludesArray.Items[i].AsString);

    DestinationEdit.Text := JSONObj.Get('destination_folder', '');
    ArchiveNameEdit.Text := JSONObj.Get('archive_name', '');
    RARPathEdit.Text := JSONObj.Get('rar_path', '');

          // Caricamento stato della checkbox chkSpegni
      chkSpegni.Checked := JSONObj.Get('chkSpegni', False);

      chkStartTime.Checked := JSONObj.Get('chkStartTime', False);
      StartTime.Time := StrToTimeDef(JSONObj.Get('start_time', ''), Now);
        TimerStartTime.Enabled := chkStartTime.Checked;

  finally
    JSONObj.Free;
  end;
end;



procedure TFrmMain.SaveConfigMenuItemClick(Sender: TObject);
var
  J: TJSONObject;
  Arr: TJSONArray;
  I: integer;
  F: TFileStream;
  SaveDlg: TSaveDialog;
  JSONText: utf8string;
begin
  SaveDlg := TSaveDialog.Create(Self);
  try
    SaveDlg.Filter := 'Backup Config (*.rbak)|*.rbak';
    SaveDlg.FileName := 'backup_configLaz.rbak';
    if not SaveDlg.Execute then Exit; // Se l'utente annulla, esce

    J := TJSONObject.Create;
    try
      // Salva le cartelle da includere
      Arr := TJSONArray.Create;
      for I := 0 to FoldersListbox.Items.Count - 1 do
        Arr.Add(FoldersListbox.Items[I]);
      J.Add('folders', Arr);

      // Salva le cartelle da escludere
      Arr := TJSONArray.Create;
      for I := 0 to ExcludeListbox.Items.Count - 1 do
        Arr.Add(ExcludeListbox.Items[I]);
      J.Add('excludes', Arr);

      // Salva cartella di destinazione
      J.Add('destination_folder', DestinationEdit.Text);

      // Salva anche archive_name e rar_path
      J.Add('archive_name', ArchiveNameEdit.Text);
      J.Add('rar_path', RARPathEdit.Text);

      // Salva stato della checkbox chkSpegni
      J.Add('chkSpegni', chkSpegni.Checked);

        J.Add('chkStartTime', chkStartTime.Checked);

        // Salva anche il valore di StartTime (solo ora, non data)
J.Add('start_time', TimeToStr(StartTime.Time));

      // Converti in UTF-8
      JSONText := UTF8Encode(J.AsJSON);

      // Scrivi su file in UTF-8 in modo sicuro
      F := TFileStream.Create(SaveDlg.FileName, fmCreate);
      try
        F.WriteBuffer(JSONText[1], Length(JSONText));
        // usa JSONText[1] per ottenere il primo byte
      finally
        F.Free;
      end;
    finally
      J.Free;
    end;
  finally
    SaveDlg.Free;
  end;
end;

procedure TFrmMain.TimerStartTimeTimer(Sender: TObject);
var
  CurrentTime, TargetTime: TTime;
begin
  if not chkStartTime.Checked then
  begin
    TimerStartTime.Enabled := False;
    Exit;
  end;

  CurrentTime := Time;
  TargetTime := StartTime.Time;

  // Mostra l'orario attuale per debug
  Label1.Caption := 'Ora attuale: ' + FormatDateTime('hh:nn:ss', CurrentTime) +
                    ' - Target: ' + FormatDateTime('hh:nn:ss', TargetTime);


  // Confronto con tolleranza di 1 minuto
  if Abs(Frac(CurrentTime) - Frac(TargetTime)) < (1 / (24 * 60)) then
  begin
    TimerStartTime.Enabled := False; // evita che riparta più volte
    //EseguiBackup; // chiama la tua procedura
  end;

end;

procedure TFrmMain.TrayIcon1Click(Sender: TObject);
begin
   // Ripristina la finestra
  Self.Show;
  Application.Restore;
  Application.BringToFront;

  // Nasconde l’icona dal tray
  TrayIcon1.Visible := False;
end;

procedure TFrmMain.VisualizzaClick(Sender: TObject);
begin
  // Ripristina la finestra
   Self.Show;
   Application.Restore;
   Application.BringToFront;

   // Nasconde l’icona dal tray
   TrayIcon1.Visible := False;
end;



procedure TFrmMain.LoadConfigMenuItemClick(Sender: TObject);
var
  JSONObj: TJSONObject;
  FoldersArray, ExcludesArray: TJSONArray;
  i: integer;
begin
  if OpenDialog1.Execute then
  begin
    JSONObj := TJSONObject(GetJSON(ReadFileToString(OpenDialog1.FileName)));
    try
      // Pulizia liste
      FoldersListbox.Items.Clear;
      ExcludeListbox.Items.Clear;

      // Caricamento folders
      FoldersArray := JSONObj.Arrays['folders'];
      if Assigned(FoldersArray) then
        for i := 0 to FoldersArray.Count - 1 do
          FoldersListbox.Items.Add(FoldersArray.Items[i].AsString);

      // Caricamento excludes
      ExcludesArray := JSONObj.Arrays['excludes'];
      if Assigned(ExcludesArray) then
        for i := 0 to ExcludesArray.Count - 1 do
          ExcludeListbox.Items.Add(ExcludesArray.Items[i].AsString);

      // Destination folder e archive name
      DestinationEdit.Text := JSONObj.Get('destination_folder', '');
      ArchiveNameEdit.Text := JSONObj.Get('archive_name', '');
      RARPathEdit.Text := JSONObj.Get('rar_path', '');

      // Caricamento stato della checkbox chkSpegni
      chkSpegni.Checked := JSONObj.Get('chkSpegni', False);

            chkStartTime.Checked := JSONObj.Get('chkStartTime', False);

             StartTime.Time := StrToTimeDef(JSONObj.Get('start_time', ''), Now);

             TimerStartTime.Enabled := chkStartTime.Checked;
    finally
      JSONObj.Free;
    end;
  end;
end;


end.
