unit mainform;

{$mode objfpc}{$H+}

interface

uses
    {$IFDEF WINDOWS}
  Windows,
        ShellApi,
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  dateutils,
  FileUtil, Process, LazFileUtils, DateTimePicker, FileCtrl, StrUtils, ComCtrls,
  Menus, EditBtn, Buttons, AboutForm, fpjson, jsonparser, jsonconf;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    // Dichiarazioni dei componenti dell'interfaccia utente (UI)
    // Questi sono gli elementi visivi che compongono la finestra principale
    AboutMenuItem: TMenuItem; // Voce di menu "About"
    AddButton: TButton; // Bottone per aggiungere cartelle
    AddExcludeButton: TButton; // Bottone per aggiungere esclusioni
    ArchiveNameEdit: TEdit; // Campo di testo per il nome dell'archivio
    ArchiveNameLabel: TLabel; // Etichetta per il nome dell'archivio
    BrowseRARButton: TButton; // Bottone per cercare il percorso di RAR
    btnTrayBar: TButton; // Bottone per minimizzare nella barra delle applicazioni
    chkEncrypt: TCheckBox;
    chkDayBak: TCheckBox; // Casella di spunta per il backup giornaliero
    chkMinTrayBar: TCheckBox;
    // Casella di spunta per minimizzare nella barra delle applicazioni dopo l'avvio
    chkSpegni: TCheckBox; // Casella di spunta per spegnere il PC dopo il backup
    chkChiudiApp: TCheckBox; // Casella di spunta per chiudere l'app dopo il backup
    chkStartTime: TCheckBox;
    // Casella di spunta per avviare il backup a un'ora specifica
    cmbCompressionLevel: TComboBox;
    cmbFrequency: TComboBox;
    cmbDayOfWeek: TComboBox;
    edtPassword: TEdit;
    Label1: TLabel;
    lblTimeLeft: TLabel; // Etichetta per visualizzare il conto alla rovescia del timer
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    RemoveAllFolder: TButton;
    RemoveEscludiAllFiles: TButton;
    StatusLabel1: TLabel; // Etichetta per lo stato
    Visualizza: TMenuItem; // Voce di menu per visualizzare la finestra
    PopupMenuTray: TPopupMenu;
    // Menu a comparsa per l'icona nella barra delle applicazioni
    StartTime: TDateTimePicker; // Componente per selezionare l'ora di avvio
    DestButton: TButton; // Bottone per selezionare la cartella di destinazione
    DestinationEdit: TEdit; // Campo di testo per la cartella di destinazione
    DestinationLabel: TLabel; // Etichetta per la cartella di destinazione
    ExcludeLabel: TLabel; // Etichetta per le esclusioni
    ExcludeListbox: TListBox; // Lista delle esclusioni
    FileMenu: TMenuItem; // Menu "File"
    FoldersLabel: TLabel; // Etichetta per le cartelle da includere
    FoldersListbox: TListBox; // Lista delle cartelle da includere
    InfoMenu: TMenuItem; // Menu "Info"
    LoadConfigMenuItem: TMenuItem; // Voce di menu per caricare la configurazione
    MainMenu1: TMainMenu; // Menu principale
    N1: TMenuItem; // Separatore di menu
    OpenDialog1: TOpenDialog; // Dialogo per l'apertura dei file
    Panel1: TPanel; // Pannello per raggruppare i controlli
    QuitMenuItem: TMenuItem; // Voce di menu "Esci"
    RARPathEdit: TEdit; // Campo di testo per il percorso di RAR
    RARPathLabel: TLabel; // Etichetta per il percorso di RAR
    RemoveButton: TButton; // Bottone per rimuovere cartelle
    RemoveExcludeButton: TButton; // Bottone per rimuovere esclusioni
    btnRunBackup: TButton; // Bottone per avviare il backup
    ProgressBar: TProgressBar; // Barra di avanzamento
    ProgressLabel: TLabel; // Etichetta per la percentuale di avanzamento
    SaveConfigMenuItem: TMenuItem; // Voce di menu per salvare la configurazione
    SaveDialog1: TSaveDialog; // Dialogo per il salvataggio dei file
    ScrolledOutput: TMemo; // Memo per visualizzare l'output del processo RAR
    TimerStartTime: TTimer; // Timer per l'avvio del backup a un'ora specifica
    TrayIcon1: TTrayIcon; // Icona nella barra delle applicazioni

    // Dichiarazione delle procedure che gestiscono gli eventi (click, change, ecc.)
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure AddExcludeButtonClick(Sender: TObject);
    procedure BrowseRARButtonClick(Sender: TObject);
    procedure btnTrayBarClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkEncryptChange(Sender: TObject);
    procedure chkStartTimeChange(Sender: TObject);
    procedure cmbFrequencyChange(Sender: TObject);
    procedure DestButtonClick(Sender: TObject);
    procedure ExcludeLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LicenseMenuItemClick(Sender: TObject);
    procedure LoadConfigMenuItemClick(Sender: TObject);
    procedure RemoveAllFolderClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure RemoveEscludiAllFilesClick(Sender: TObject);
    procedure RemoveExcludeButtonClick(Sender: TObject);
    procedure btnRunBackupClick(Sender: TObject);
    procedure SaveConfigMenuItemClick(Sender: TObject);
    procedure TimerStartTimeTimer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure VisualizzaClick(Sender: TObject);
  private
    // Variabili e procedure private, non accessibili dall'esterno
    FProcess: TProcess; // Oggetto per l'esecuzione del processo esterno (RAR)
    FTotalFiles: integer; // Numero totale di file da processare
    FProcessedFiles: integer; // Numero di file processati

    TrayMode: boolean;

    NomeFileConfigBakup: string;

    // Procedure interne per la gestione dei dati
    procedure OnProcessOutput(const ALine: string); // Gestisce l'output del processo
    procedure PreCalculateFiles(const SourceDir: string);
    // Precalcola il numero totale di file
    procedure LoadConfigFromFile(const AFileName: string);
    // Carica la configurazione da un file
  public
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

{ TFrmMain }


{$IFDEF MSWINDOWS}
function GetLocaleInformation(Flag: integer): string;
var
 pcLCA: array[0..20] of char;
begin
 if (GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, Flag, pcLCA, 19) <= 0) then
 begin
   pcLCA[0] := #0;
 end;
 Result := pcLCA;
end;

{$ENDIF}

function GetSystemLanguageCode: string;
begin
  {$IFDEF MSWINDOWS}
   Result := GetLocaleInformation(LOCALE_SENGLANGUAGE);
  {$ELSE}
   Result := SysUtils.GetEnvironmentVariable('LANG');
  {$ENDIF}
end;



procedure TFrmMain.FormCreate(Sender: TObject);



  procedure SetDefault;
  begin
    {$IFDEF WINDOWS}
    RARPathEdit.Text := 'C:\Program Files\WinRAR\Rar.exe';
    DestinationEdit.Text:= 'D:\';
    ArchiveNameEdit.text:='RarBackup.rar';

    ExcludeListbox.Clear;



        ExcludeLabelClick(Sender);

    {$ENDIF}

    {$IFDEF LINUX}
      FProcess.Executable := '/sbin/shutdown';
      FProcess.Parameters.Add('-h'); // Opzione per l'halt (spegnimento)
      FProcess.Parameters.Add('now'); // Spegni immediatamente
    {$ENDIF}

    {$IFDEF DARWIN}
 // Per macOS
      FProcess.Executable := '/usr/bin/osascript';
      FProcess.Parameters.Add('-e');
      FProcess.Parameters.Add('tell application "System Events" to shut down');
    {$ENDIF}
  end;

var
  ConfigPath: string;
  I: integer;
  Param: string;
  LoadFile: string;
begin
  LoadFile := '';
  TrayMode := False;

  // Analizza i parametri passati all'applicazione
  I := 1;
  while I <= ParamCount do
  begin
    Param := UpperCASE(ParamStr(I));
    if AnsiStartsText('/TRAY', Param) then
    begin
      TrayMode := True;
      Inc(I); // Passa al prossimo parametro
    end
    else if AnsiStartsText('/LOAD', Param) then
    begin
      Inc(I); // Passa al prossimo parametro, che dovrebbe essere il nome del file
      if I <= ParamCount then
      begin
        LoadFile := ParamStr(I);
        // Rimuovi eventuali doppi apici se il percorso era tra virgolette
        LoadFile := StringReplace(LoadFile, '"', '', [rfReplaceAll]);

      end;
      Inc(I); // Passa al parametro successivo dopo il nome del file
    end
    else
    begin
      // Se il parametro non è riconosciuto, passa al successivo
      Inc(I);
    end;
  end;

  // Crea l'oggetto per l'esecuzione del processo RAR
  FProcess := TProcess.Create(Self);
  FProcess.Options := [poUsePipes, poStderrToOutput];

  // Carica il file di configurazione specificato, se presente
  if LoadFile <> '' then
  begin
    if FileExists(LoadFile) then
    begin
      NomeFileConfigBakup := LoadFile;
      LoadConfigFromFile(LoadFile);
    end
    else
    begin
      SetDefault;
    end;

  end
  else
  begin
    // Se non è stato specificato un file, cerca quello predefinito
    ConfigPath := GetUserDir + 'Documents' + PathDelim + 'backup_configLaz.rbak';
    NomeFileConfigBakup := 'backup_configLaz.rbak';
    if FileExists(ConfigPath) then
    begin
      LoadConfigFromFile(ConfigPath);
    end
    else
    begin
      SetDefault;
    end;
  end;



    if GetSystemLanguageCode = 'Italian' then
  begin
    // Imposta le etichette e le caption in italiano
    // Form principale
    Self.Caption := 'Laz Backup Incrementale';

    // Pannello 1
    RARPathLabel.Caption := 'Percorso del programma RAR';
    BrowseRARButton.Caption := 'Trova Programma';
    DestinationLabel.Caption := 'Percorso di destinazione di salvataggio dell''archivio';
    DestButton.Caption := 'Trova Percorso';
    ArchiveNameLabel.Caption := 'Nome del file di backup';
    FoldersLabel.Caption := 'Cartelle da salvare';
    ExcludeLabel.Caption := 'Escludi file/cartelle (pattern *.tmp, *.zip, C:\temp\*)';
    ProgressLabel.Caption := 'Avanzamento: 0%';
    btnRunBackup.Caption := 'Esegui Backup';
    chkSpegni.Caption := 'Spegni computer al termine';
    chkStartTime.Caption := 'Auto Start';
    lblTimeLeft.Caption := 'Tempo rimanente';
    btnTrayBar.Caption := 'Minimizza nella tray bar';
    chkDayBak.Caption := 'Esegui backup giornaliero rotazionale';
    chkDayBak.Hint := 'Aggiunge al nome del file il giorno della settimana.'#13#10'Il backup aggiornerà, se esiste, il file della settimana precedente '#13#10'in modo da avere 7 backup incrementali.';
    chkMinTrayBar.Caption := 'Minimizza in Tray Bar';
    chkChiudiApp.Caption := 'Chiudi Applicazione';
    StatusLabel1.Caption := 'Dopo aver completato il backup esegui:';
    chkEncrypt.Caption := 'Proteggi con password';
    Label2.Caption := 'Compressione';
    Label4.Caption := 'Tipo';

    // ComboBox
    cmbCompressionLevel.Items.Clear;
    cmbCompressionLevel.Items.Add('Sola archiviazione');
    cmbCompressionLevel.Items.Add('Veloce');
    cmbCompressionLevel.Items.Add('Normale');
    cmbCompressionLevel.Items.Add('Buona');
    cmbCompressionLevel.Items.Add('Massima');

    cmbFrequency.Items.Clear;
    cmbFrequency.Items.Add('Giornaliero');
    cmbFrequency.Items.Add('Settimanale');

    cmbDayOfWeek.Items.Clear;
    cmbDayOfWeek.Items.Add('Lunedì');
    cmbDayOfWeek.Items.Add('Martedì');
    cmbDayOfWeek.Items.Add('Mercoledì');
    cmbDayOfWeek.Items.Add('Giovedì');
    cmbDayOfWeek.Items.Add('Venerdì');
    cmbDayOfWeek.Items.Add('Sabato');
    cmbDayOfWeek.Items.Add('Domenica');

    // Menu
    FileMenu.Caption := '&File';
    LoadConfigMenuItem.Caption := 'Carica Configurazione';
    SaveConfigMenuItem.Caption := 'Salva Configurazione';
    QuitMenuItem.Caption := 'Esci';
    InfoMenu.Caption := '&Info';
    AboutMenuItem.Caption := 'About';

    // Popup Menu Tray
    Visualizza.Caption := 'Visualizza';

    Label3.caption :='Esempio:'#13#10'LazBackup.exe /tray /load "C:\Configurazioni\backup_casa.rbak" '#13#10'I parametri non sono obligatori servono ad automatizzare la procedura';
  end else begin
     // Imposta le etichette e le caption in inglese
    // Main Form
    Self.Caption := 'Laz Incremental Backup';

    // Panel 1
    RARPathLabel.Caption := 'RAR Program Path';
    BrowseRARButton.Caption := 'Browse Program';
    DestinationLabel.Caption := 'Archive destination path';
    DestButton.Caption := 'Browse Path';
    ArchiveNameLabel.Caption := 'Backup file name';
    FoldersLabel.Caption := 'Folders to save';
    ExcludeLabel.Caption := 'Exclude files/folders (pattern *.tmp, *.zip, C:\temp\*)';
    ProgressLabel.Caption := 'Progress: 0%';
    btnRunBackup.Caption := 'Run Backup';
    chkSpegni.Caption := 'Shutdown computer on finish';
    chkStartTime.Caption := 'Auto Start';
    lblTimeLeft.Caption := 'Time left to start';
    btnTrayBar.Caption := 'Minimize to tray bar';
    chkDayBak.Caption := 'Perform rotational daily backup';
    chkDayBak.Hint := 'Adds the day of the week to the file name.'#13#10'The backup will update the previous week''s file if it exists,'#13#10'allowing for 7 incremental backups.';

    chkMinTrayBar.Caption := 'Minimize to Tray Bar';
    chkChiudiApp.Caption := 'Close Application';
    StatusLabel1.Caption := 'After completing the backup, execute:';
    chkEncrypt.Caption := 'Protect with password';
    Label2.Caption := 'Compression';
    Label4.Caption := 'Type';

    // ComboBox
    cmbCompressionLevel.Items.Clear;
    cmbCompressionLevel.Items.Add('Store only');
    cmbCompressionLevel.Items.Add('Fastest');
    cmbCompressionLevel.Items.Add('Normal');
    cmbCompressionLevel.Items.Add('Good');
    cmbCompressionLevel.Items.Add('Best');

    cmbFrequency.Items.Clear;
    cmbFrequency.Items.Add('Daily');
    cmbFrequency.Items.Add('Weekly');

    cmbDayOfWeek.Items.Clear;
    cmbDayOfWeek.Items.Add('Monday');
    cmbDayOfWeek.Items.Add('Tuesday');
    cmbDayOfWeek.Items.Add('Wednesday');
    cmbDayOfWeek.Items.Add('Thursday');
    cmbDayOfWeek.Items.Add('Friday');
    cmbDayOfWeek.Items.Add('Saturday');
    cmbDayOfWeek.Items.Add('Sunday');

    // Menu
    FileMenu.Caption := '&File';
    LoadConfigMenuItem.Caption := 'Load Configuration';
    SaveConfigMenuItem.Caption := 'Save Configuration';
    QuitMenuItem.Caption := 'Quit';
    InfoMenu.Caption := '&Info';
    AboutMenuItem.Caption := 'About';

    // Popup Menu Tray
    Visualizza.Caption := 'View';

    Label3.caption :='Example:'#13#10'LazBackup.exe /tray /load "C:\Configurations\home_backup.rbak"'#13#10'The parameters are not mandatory; they serve to automate the process.';

  end;


end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  // Se è stato passato il parametro /tray, minimizza l'applicazione nel tray
  if TrayMode then
  begin
    // Nasconde la finestra principale
    btnTrayBarClick(Sender);
    TrayMode := False;
  end;

end;

procedure TFrmMain.LicenseMenuItemClick(Sender: TObject);
begin
  // Questa procedura è un segnaposto per la licenza, attualmente non fa nulla
end;

procedure TFrmMain.AddButtonClick(Sender: TObject);
var
  Folder: string;
begin
  // Apre una finestra di dialogo per selezionare una cartella da includere nel backup
  if SelectDirectory('Seleziona la cartella da salvare', '', Folder) then
  begin
    // Controlla se la cartella è valida e non è già presente nella lista
    if (Folder <> '') and (FoldersListbox.Items.IndexOf(Folder) = -1) then
      FoldersListbox.Items.Add(Folder) // Aggiunge la cartella alla lista
    else
      ShowMessage('La cartella è già stata aggiunta.'); // Avvisa l'utente
  end;
end;

procedure TFrmMain.AboutMenuItemClick(Sender: TObject);
begin
  // Mostra la finestra "About" (Informazioni)
  FrmAbout.ShowModal;
end;

procedure TFrmMain.RemoveButtonClick(Sender: TObject);
var
  Index: integer;
begin
  // Rimuove la cartella selezionata dalla lista
  Index := FoldersListbox.ItemIndex;
  if Index <> -1 then // Controlla se un elemento è selezionato
    FoldersListbox.Items.Delete(Index);
end;

procedure TFrmMain.RemoveEscludiAllFilesClick(Sender: TObject);
begin
   ExcludeListbox.Clear;
end;

procedure TFrmMain.AddExcludeButtonClick(Sender: TObject);
var
  Pattern: string;
begin
  // Chiede all'utente di inserire un pattern di esclusione (es. *.tmp)
  Pattern := InputBox('Aggiungi esclusione',
    'Inserisci estensione/cartella da escludere (es: *.tmp):', '');
  // Se il pattern è valido e non è già presente, lo aggiunge alla lista
  if (Pattern <> '') and (ExcludeListbox.Items.IndexOf(Pattern) = -1) then
    ExcludeListbox.Items.Add(Pattern);
end;

procedure TFrmMain.RemoveExcludeButtonClick(Sender: TObject);
var
  Index: integer;
begin
  // Rimuove il pattern di esclusione selezionato dalla lista
  Index := ExcludeListbox.ItemIndex;
  if Index <> -1 then // Controlla se un elemento è selezionato
    ExcludeListbox.Items.Delete(Index);
end;

procedure TFrmMain.DestButtonClick(Sender: TObject);
var
  DestFolder: string;
begin
  // Apre una finestra di dialogo per selezionare la cartella di destinazione del backup
  if SelectDirectory('Seleziona la cartella di destinazione', '', DestFolder) then
    DestinationEdit.Text := DestFolder; // Imposta il percorso nella casella di testo
end;

procedure TFrmMain.ExcludeLabelClick(Sender: TObject);
begin
  ExcludeListbox.Items.Add('desktop.ini');

  // Fili infetti da ransomeware
  ExcludeListbox.Items.Add('*.locked');
  ExcludeListbox.Items.Add('*.encrypted');
  ExcludeListbox.Items.Add('*.wannacry');
  ExcludeListbox.Items.Add('*.phobos');
  ExcludeListbox.Items.Add('*.locked');
  ExcludeListbox.Items.Add('*.ryuk');

  // File temporanei generici
  ExcludeListbox.Items.Add('*.tmp');
  ExcludeListbox.Items.Add('*.~*');
  ExcludeListbox.Items.Add('*.bak');
  ExcludeListbox.Items.Add('*.log');
  ExcludeListbox.Items.Add('*.temp');

  // File di Microsoft Office e di sistema
  ExcludeListbox.Items.Add('*.xlsx~');
  ExcludeListbox.Items.Add('*.docx~');
  ExcludeListbox.Items.Add('*.pptx~');
  ExcludeListbox.Items.Add('Thumbs.db');
end;

procedure TFrmMain.BrowseRARButtonClick(Sender: TObject);
begin
  // Apre un dialogo per cercare il file eseguibile di RAR (rar.exe)
  if OpenDialog1.Execute then
    RARPathEdit.Text := OpenDialog1.FileName;
  // Imposta il percorso nella casella di testo
end;

procedure TFrmMain.btnTrayBarClick(Sender: TObject);
begin

  // Nasconde la finestra principale
  Self.ShowInTaskBar := stNever;


  // Nasconde la finestra principale
  //  Application.Minimize;
  //Self.Hide;
  frmMain.Hide;

  // Mostra l'icona nella barra delle applicazioni (tray)
  TrayIcon1.Visible := True;
  TrayIcon1.Hint := 'Backup App - in esecuzione'; // Suggerimento al passaggio del mouse
  // TrayIcon1.PopupMenu := PopupMenuTray; // collega il menu
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin

end;

procedure TFrmMain.chkEncryptChange(Sender: TObject);
begin
  edtPassword.Enabled := chkEncrypt.Checked;
end;

procedure TFrmMain.chkStartTimeChange(Sender: TObject);
begin
  // Abilita/disabilita il timer in base allo stato della checkbox
  TimerStartTime.Enabled := chkStartTime.Checked;
  lblTimeLeft.Visible:=chkStartTime.Checked;
end;

procedure TFrmMain.cmbFrequencyChange(Sender: TObject);
begin
  // Nasconde tutti i controlli aggiuntivi
  cmbDayOfWeek.Visible := False;


  // Mostra i controlli appropriati in base alla selezione
  case cmbFrequency.ItemIndex of
    0: // Giornaliero: nessun controllo aggiuntivo necessario
      ;
    1: // Settimanale
      cmbDayOfWeek.Visible := True;

  end;
end;

procedure TFrmMain.PreCalculateFiles(const SourceDir: string);
var
  FileList: TStringList;
begin
  // Calcola ricorsivamente il numero di file in una cartella
  FileList := TStringList.Create;
  try
    FindAllFiles(FileList, SourceDir, '*', True); // True per includere le sottocartelle
    Inc(FTotalFiles, FileList.Count); // Incrementa il contatore totale dei file
  finally
    FileList.Free; // Libera la memoria
  end;
end;


procedure TFrmMain.OnProcessOutput(const ALine: string);
begin
  // Aggiunge una riga di output dal processo RAR alla casella di testo
  if ALine = '' then Exit;
  ScrolledOutput.Lines.Add(ALine);
  Inc(FProcessedFiles); // Incrementa il contatore dei file processati

  // Aggiorna la barra di avanzamento e l'etichetta
  if FTotalFiles > 0 then
  begin
    ProgressLabel.Caption := Format('Avanzamento: %d%% (%d di %d file)',
      [Round((FProcessedFiles / FTotalFiles) * 100), FProcessedFiles, FTotalFiles]);
  end
  else
  begin
    ScrolledOutput.Lines.Add(IntToStr(FTotalFiles)); // Per debugging
  end;
end;

procedure TFrmMain.btnRunBackupClick(Sender: TObject);
// Funzione locale per ottenere il percorso della home directory in base al sistema operativo
  function GetHomeDir: string;
  begin
    {$IFDEF UNIX}
 // Se il sistema operativo è Unix (Linux/macOS)
      Result := GetEnvironmentVariable('HOME');
      if Result = '' then
        Result := '/tmp'; // Fallback
    {$ELSE}// Altrimenti (Windows)
    Result := GetEnvironmentVariable('USERPROFILE');
    if Result = '' then
      Result := 'C:\'; // Fallback
    {$ENDIF}
  end;

const
  // Array per i nomi dei giorni della settimana
  Giorni: array[0..6] of string = (
    'Lunedi', 'Martedi', 'Mercoledi', 'Giovedi', 'Venerdi', 'Sabato', 'Domenica'
    );
var
  // Variabili per la gestione dell'output del processo
  Buffer: array[0..1023] of byte;
  BytesRead: longint;
  Line: string;
  i, idx: integer;
  ArchiveName: string;
  CompressionParam: string;
begin
  // Prepara l'interfaccia per il nuovo backup
  ScrolledOutput.Lines.Clear;
  FProcessedFiles := 0;

  FTotalFiles := 0;

  // Calcola il numero totale di file da processare per aggiornare l'avanzamento
  for i := 0 to FoldersListbox.Items.Count - 1 do
    PreCalculateFiles(FoldersListbox.Items[i]);

  // Imposta il valore massimo della progress bar sul numero totale di file
  ProgressBar1.Min := 0;
  ProgressBar1.Max := FTotalFiles;
  ProgressBar1.Position := 0;


  // Gestione del nome dell'archivio
  ArchiveName := ArchiveNameEdit.Text;
  if LowerCase(ExtractFileExt(ArchiveName)) <> '.rar' then
  begin
    ArchiveName := ArchiveName + '.rar';
    ArchiveNameEdit.Text := ArchiveName;
  end;

  // Se la checkbox 'Backup giornaliero' è selezionata, aggiunge il giorno al nome dell'archivio
  if chkDayBak.Checked then
  begin
    idx := DayOfWeek(Now) - 2;
    // Mappa il giorno della settimana (1=Domenica) a un indice 0-6
    if idx < 0 then
      idx := 6;
    ArchiveName := ChangeFileExt(ArchiveName, '') + '_' + IntToStr(idx) +
      '_' + Giorni[idx] + '.rar';
  end;

  // Aggiungi questo blocco per gestire la compressione
  case cmbCompressionLevel.ItemIndex of
    0: CompressionParam := '-m0'; // Sola archiviazione
    1: CompressionParam := '-m1'; // Veloce
    2: CompressionParam := '-m3'; // Normale (default)
    3: CompressionParam := '-m4'; // Buona
    4: CompressionParam := '-m5'; // Massima
    else
      CompressionParam := ''; // Nessun parametro se l'opzione non è selezionata
  end;


  // Configurazione dei parametri per l'esecuzione del processo RAR
  FProcess.Executable := RARPathEdit.Text;
  FProcess.Parameters.Clear;
  FProcess.Parameters.Add('a');       // Aggiungi file all'archivio (opzione 'a')
  FProcess.Parameters.Add('-u');
  // Aggiorna solo file modificati o nuovi (opzione '-u')
  FProcess.Parameters.Add('-r');       // Include sottocartelle (opzione '-r')

  // Aggiungi il parametro di compressione se selezionato
  if CompressionParam <> '' then
    FProcess.Parameters.Add(CompressionParam);

  // Aggiungi questo blocco per gestire la password
  if chkEncrypt.Checked and (edtPassword.Text <> '') then
  begin
    FProcess.Parameters.Add('-p' + edtPassword.Text);
  end;


  FProcess.Parameters.Add(DestinationEdit.Text + PathDelim + ArchiveName);
  // Percorso di destinazione
  FProcess.Parameters.AddStrings(FoldersListbox.Items);
  // Aggiunge le cartelle da includere

  FProcess.Options := [poUsePipes, poStderrToOutput, poNoConsole];
  FProcess.CurrentDirectory := GetHomeDir;

  FProcess.Execute; // Avvia il processo RAR

  // Ciclo per leggere l'output del processo RAR in tempo reale




  while FProcess.Running or (FProcess.Output.NumBytesAvailable > 0) do
  begin
    if FProcess.Output.NumBytesAvailable > 0 then
    begin
      BytesRead := FProcess.Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
      begin
        SetString(Line, pansichar(@Buffer[0]), BytesRead);
        ScrolledOutput.Lines.Text := ScrolledOutput.Lines.Text + Line;
        ScrolledOutput.SelStart := Length(ScrolledOutput.Text);

        // La logica per aggiornare la progress bar in base all'output di RAR
        Inc(FProcessedFiles);
        ProgressBar1.Position := FProcessedFiles;

        ProgressLabel.Caption :=
          Format('Avanzamento: %d%% (%d di %d file)', [Round(
          (FProcessedFiles / FTotalFiles) * 100), FProcessedFiles, FTotalFiles]);

      end;
    end;
    Application.ProcessMessages;
    // Permette all'interfaccia utente di rimanere responsiva
    Sleep(50); // Mette in pausa l'esecuzione per evitare di sovraccaricare la CPU
  end;

  // Cattura l'eventuale output residuo dopo la fine del processo
  while FProcess.Output.NumBytesAvailable > 0 do
  begin
    BytesRead := FProcess.Output.Read(Buffer, SizeOf(Buffer));
    if BytesRead > 0 then
    begin
      SetString(Line, pansichar(@Buffer[0]), BytesRead);
      ScrolledOutput.Lines.Text := ScrolledOutput.Lines.Text + Line;
    end;
  end;

  // Aggiorna la barra di avanzamento e lo stato al 100% al termine del backup
  // Imposta la barra di avanzamento e l'etichetta al 100% al termine del backup
  ProgressBar1.Position := FTotalFiles;
  ProgressLabel.Caption := Format('Avanzamento: 100%% (%d di %d file)',
    [FTotalFiles, FTotalFiles]);

  // 🔹 Se la checkbox per lo spegnimento è attiva, esegue il comando di spegnimento
  if chkSpegni.Checked then
  begin
    FProcess.CloseInput;
    FProcess.Parameters.Clear;

    // Codice specifico per lo spegnimento in base al sistema operativo
    {$IFDEF WINDOWS}
      FProcess.Executable := 'shutdown';
      FProcess.Parameters.Add('-s'); // Opzione per spegnere
      FProcess.Parameters.Add('-t'); // Opzione per il timer
      FProcess.Parameters.Add('0'); // Spegni immediatamente
    {$ENDIF}

    {$IFDEF LINUX}
      FProcess.Executable := '/sbin/shutdown';
      FProcess.Parameters.Add('-h'); // Opzione per l'halt (spegnimento)
      FProcess.Parameters.Add('now'); // Spegni immediatamente
    {$ENDIF}

    {$IFDEF DARWIN}
 // Per macOS
      FProcess.Executable := '/usr/bin/osascript';
      FProcess.Parameters.Add('-e');
      FProcess.Parameters.Add('tell application "System Events" to shut down');
    {$ENDIF}

    FProcess.Options := [];
    FProcess.Execute;
  end;

  // Se la checkbox 'Minimizza dopo il backup' è attiva, minimizza l'app
  if chkMinTrayBar.Checked then
  begin
    btnTrayBarClick(Sender);
  end;

  // Se la checkbox 'Chiudi app dopo il backup' è attiva, chiude l'app
  if chkChiudiApp.Checked then
  begin
    halt(0); // Termina l'applicazione
  end;

end;


procedure TFrmMain.LoadConfigFromFile(const AFileName: string);
var
  JSONObj: TJSONObject;
  FoldersArray, ExcludesArray: TJSONArray;
  i: integer;
begin
  // Carica i dati di configurazione da un file JSON
  JSONObj := TJSONObject(GetJSON(ReadFileToString(AFileName)));
  try
    FoldersListbox.Items.Clear;


    // Carica le cartelle da includere
    FoldersArray := JSONObj.Arrays['folders'];
    if Assigned(FoldersArray) then
      for i := 0 to FoldersArray.Count - 1 do
        FoldersListbox.Items.Add(FoldersArray.Items[i].AsString);

    // Carica le esclusioni
    ExcludesArray := JSONObj.Arrays['excludes'];
    if Assigned(ExcludesArray) then
      for i := 0 to ExcludesArray.Count - 1 do
        ExcludeListbox.Items.Add(ExcludesArray.Items[i].AsString);

    // Carica i valori delle caselle di testo e delle checkbox
    DestinationEdit.Text := JSONObj.Get('destination_folder', '');
    ArchiveNameEdit.Text := JSONObj.Get('archive_name', '');
    RARPathEdit.Text := JSONObj.Get('rar_path', '');
    chkSpegni.Checked := JSONObj.Get('chkSpegni', False);
    chkStartTime.Checked := JSONObj.Get('chkStartTime', False);
    StartTime.Time := StrToTimeDef(JSONObj.Get('start_time', ''), Now);
    TimerStartTime.Enabled := chkStartTime.Checked;
    chkDayBak.Checked := JSONObj.Get('chkDayBak', False);
    chkChiudiApp.Checked := JSONObj.Get('chkChiudiApp', False);
    chkMinTrayBar.Checked := JSONObj.Get('chkMinTrayBar', False);

  finally
    JSONObj.Free; // Libera la memoria
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
  // Salva la configurazione corrente in un file JSON
  SaveDlg := TSaveDialog.Create(Self);
  try
    SaveDlg.Filter := 'Backup Config (*.rbak)|*.rbak';
    SaveDlg.FileName := NomeFileConfigBakup;
    if not SaveDlg.Execute then Exit; // Esce se l'utente annulla

    J := TJSONObject.Create;
    try
      // Salva le liste in array JSON
      Arr := TJSONArray.Create;
      for I := 0 to FoldersListbox.Items.Count - 1 do
        Arr.Add(FoldersListbox.Items[I]);
      J.Add('folders', Arr);

      Arr := TJSONArray.Create;
      for I := 0 to ExcludeListbox.Items.Count - 1 do
        Arr.Add(ExcludeListbox.Items[I]);
      J.Add('excludes', Arr);

      // Salva i valori delle caselle di testo
      J.Add('destination_folder', DestinationEdit.Text);
      J.Add('archive_name', ArchiveNameEdit.Text);
      J.Add('rar_path', RARPathEdit.Text);

      // Salva lo stato delle checkbox
      J.Add('chkSpegni', chkSpegni.Checked);
      J.Add('chkStartTime', chkStartTime.Checked);
      J.Add('start_time', TimeToStr(StartTime.Time));
      J.Add('chkDayBak', chkDayBak.Checked);
      J.Add('chkChiudiApp', chkChiudiApp.Checked);
      J.Add('chkMinTrayBar', chkMinTrayBar.Checked);

      // Scrive il JSON su file
      JSONText := UTF8Encode(J.AsJSON);
      F := TFileStream.Create(SaveDlg.FileName, fmCreate);
      try
        F.WriteBuffer(JSONText[1], Length(JSONText));
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
  CurrentDateTime: TDateTime;
  CurrentDayOfWeek: integer;
  CurrentDayOfMonth: integer;
  TargetDateTime: TDateTime;
  TargetDayOfWeek: integer;
  TargetDayOfMonth: integer;
  SecondsLeft: int64;
  Hours, Minutes, Seconds: word;
begin
  if not chkStartTime.Checked then
  begin
    TimerStartTime.Enabled := False;
    Exit;
  end;

  CurrentDateTime := Now;
  CurrentDayOfWeek := DayOfWeek(CurrentDateTime);
  CurrentDayOfMonth := DayOfTheMonth(CurrentDateTime);

  TargetDateTime := Int(CurrentDateTime) + Frac(StartTime.Time); // oggi all'ora scelta

  // Calcola se è il momento giusto in base alla frequenza selezionata
  case cmbFrequency.ItemIndex of
    0: // Giornaliero
    begin
      // Se l'orario target è già passato, sposta a domani
      if TargetDateTime <= CurrentDateTime then
        TargetDateTime := TargetDateTime + 1;
    end;
    1: // Settimanale
    begin
      TargetDayOfWeek := cmbDayOfWeek.ItemIndex + 1;
      // Se non è il giorno giusto, sposta il target al giorno corretto della settimana successiva
      while DayOfWeek(TargetDateTime) <> TargetDayOfWeek do
        TargetDateTime := TargetDateTime + 1;
      // Se l'orario target è già passato, sposta al giorno corretto della settimana successiva
      if TargetDateTime <= CurrentDateTime then
        TargetDateTime := TargetDateTime + 7;
    end;

  end;

  // Calcolo del tempo rimanente e aggiornamento del countdown
  SecondsLeft := Round((TargetDateTime - CurrentDateTime) * 24 * 60 * 60);
  if SecondsLeft < 0 then
    SecondsLeft := 0;

  Hours := SecondsLeft div 3600;
  Minutes := (SecondsLeft mod 3600) div 60;
  Seconds := SecondsLeft mod 60;

  lblTimeLeft.Caption :=
    'Ora attuale: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', CurrentDateTime) +
    ' - Target: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', TargetDateTime) +
    ' - Mancano: ' + Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);

  // Confronto con tolleranza di 1 secondo per avviare il backup
  if Abs(CurrentDateTime - TargetDateTime) < (1 / (24 * 60 * 60)) then
  begin
    TimerStartTime.Enabled := False;
    btnRunBackupClick(Sender);
    Sleep(110);
    TimerStartTime.Enabled := True;
  end;
end;


procedure TFrmMain.TrayIcon1Click(Sender: TObject);
begin
  // Ripristina la finestra
  Self.ShowInTaskBar := stAlways;
  // Aggiungi questa linea per mostrare l'icona nella taskbar

  // Ripristina la finestra dalla barra delle applicazioni
  Self.Show;
  Application.Restore;
  Application.BringToFront;
  // Nasconde l'icona dal tray
  TrayIcon1.Visible := False;
end;

procedure TFrmMain.TrayIcon1DblClick(Sender: TObject);
begin
  // Questa procedura è un segnaposto per un doppio click, attualmente non fa nulla
end;

procedure TFrmMain.VisualizzaClick(Sender: TObject);
begin
  // Ripristina la finestra dalla barra delle applicazioni
  Self.Show;
  Application.Restore;
  Application.BringToFront;
  // Nasconde l'icona dal tray
  TrayIcon1.Visible := False;
end;


procedure TFrmMain.LoadConfigMenuItemClick(Sender: TObject);
var
  JSONObj: TJSONObject;
  FoldersArray, ExcludesArray: TJSONArray;
  i: integer;
begin
  // Apre una finestra di dialogo per selezionare un file di configurazione e lo carica
  if OpenDialog1.Execute then
  begin
    NomeFileConfigBakup :=  OpenDialog1.FileName;
    JSONObj := TJSONObject(GetJSON(ReadFileToString(NomeFileConfigBakup)));
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
      chkSpegni.Checked := JSONObj.Get('chkSpegni', False);
      chkStartTime.Checked := JSONObj.Get('chkStartTime', False);
      StartTime.Time := StrToTimeDef(JSONObj.Get('start_time', ''), Now);
      chkDayBak.Checked := JSONObj.Get('chkDayBak', False);
      chkChiudiApp.Checked := JSONObj.Get('chkChiudiApp', False);
      chkMinTrayBar.Checked := JSONObj.Get('chkMinTrayBar', False);

      TimerStartTime.Enabled := chkStartTime.Checked;
    finally
      JSONObj.Free;
    end;
  end;
end;

procedure TFrmMain.RemoveAllFolderClick(Sender: TObject);
begin
   FoldersListbox.Items.Clear;
end;

end.
