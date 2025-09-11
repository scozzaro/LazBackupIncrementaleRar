unit AboutForm;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
        ShellApi,
  {$ENDIF}
  {$IFDEF LINUX}
  BaseUnix,
  {$ENDIF}
  fileinfo,
  Process,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    BtnClose1: TButton;
    ImgLogo: TImage;
    Label1: TLabel;
    LangRun: TLabel;
    LblAppName: TLabel;
    LblAuthor: TLabel;
    LblLicenza: TLabel;
    LblCopyright: TLabel;
    LblVersion: TLabel;
    LblWinRARWarning: TLabel;
    MemoTestoLicenza: TMemo;
    TimerBounce: TTimer;
    procedure BtnClose1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImgLogoClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure TimerBounceTimer(Sender: TObject);
  private
    fVelocityY: single;
    fGravity: single;
    fMinY: integer;
    // Variabili per il movimento orizzontale
    fVelocityX: single;
    fMinX: integer;
    fMaxX: integer;



  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }
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


procedure TfrmAbout.BtnClose1Click(Sender: TObject);
begin
  Close;
end;


function GetAppVersion: string;
var
  FileVerInfo: TFileVersionInfo;
  s: string;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    s := 'Company: ' + FileVerInfo.VersionStrings.Values['CompanyName'] +
      LineEnding + 'File description: ' +
      FileVerInfo.VersionStrings.Values['FileDescription'] + lineEnding +
      'File version: ' + FileVerInfo.VersionStrings.Values['FileVersion'] +
      LineEnding + 'Internal name: ' + FileVerInfo.VersionStrings.Values[
      'InternalName'] + LineEnding + 'Legal copyright: ' +
      FileVerInfo.VersionStrings.Values['LegalCopyright'] + LineEnding +
      'Original filename: ' + FileVerInfo.VersionStrings.Values['OriginalFilename'] +
      LineEnding + 'Product name: ' +
      FileVerInfo.VersionStrings.Values['ProductName'] + LineEnding +
      'Product version: ' + FileVerInfo.VersionStrings.Values['ProductVersion'];
    //   ShowMessage(s);
    GetAppVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;


procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Langrun.Caption := GetSystemLanguageCode;




  if GetSystemLanguageCode = 'Italiano' then
  begin
    // Imposta le caption e le stringhe in italiano
    Self.Caption := 'Informazioni';
    ImgLogo.Hint := 'Logo del programma';
    LblAppName.Caption := 'Lazbackup Incrementale';
    LblVersion.Caption := 'Versione ' + GetAppVersion;
    // Se la versione è dinamica, usa 'Versione ' + VersionNumber
    LblCopyright.Caption := 'Copyright © 2025 Vincenzo Scozzaro';
    LblAuthor.Caption := 'Autore: Vincenzo Scozzaro';
    LblLicenza.Caption := 'Licenza: Mozilla, gratuito anche per scopi commerciali';
    MemoTestoLicenza.Lines.Clear;
    MemoTestoLicenza.Lines.Add(
      'Questa applicazione è distribuita sotto licenza Mozilla Public License.');
    MemoTestoLicenza.Lines.Add(
      'È gratuita e può essere utilizzata anche per scopi commerciali.');
    MemoTestoLicenza.Lines.Add('Non è richiesta alcuna royalty o pagamento.');
    MemoTestoLicenza.Lines.Add(
      'Ulteriori dettagli sulla licenza possono essere consultati su https://www.mozilla.org/MPL/');
    LblWinRARWarning.Caption :=
      'Avvertenza: Questo programma richiede l''installazione di RAR o WinRAR. Questi strumenti non sono forniti e devono essere scaricati e installati separatamente dall''utente.';
    BtnClose1.Caption := 'Chiudi';
    LangRun.Caption := 'Lingua di sistema';
  end
  else
  begin
    // Imposta le caption e le stringhe in inglese
    Self.Caption := 'About';
    ImgLogo.Hint := 'Program logo';
    LblAppName.Caption := 'Lazbackup Incremental';
    LblVersion.Caption := 'Version ' + GetAppVersion;
    // If the version is dynamic, use 'Version ' + VersionNumber
    LblCopyright.Caption := 'Copyright © 2025 Vincenzo Scozzaro';
    LblAuthor.Caption := 'Author: Vincenzo Scozzaro';
    LblLicenza.Caption := 'License: Mozilla, free for commercial use';
    MemoTestoLicenza.Lines.Clear;
    MemoTestoLicenza.Lines.Add(
      'This application is distributed under the Mozilla Public License.');
    MemoTestoLicenza.Lines.Add(
      'It is free and can also be used for commercial purposes.');
    MemoTestoLicenza.Lines.Add('No royalty or payment is required.');
    MemoTestoLicenza.Lines.Add(
      'Further details on the license can be found at https://www.mozilla.org/MPL/');
    LblWinRARWarning.Caption :=
      'Warning: This program requires the installation of RAR or WinRAR. These tools are not provided and must be downloaded and installed separately by the user.';
    BtnClose1.Caption := 'Close';
    LangRun.Caption := 'System Language';
  end;

end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  // posiziona il logo in alto e al centro
  ImgLogo.Top := 10;
  ImgLogo.Left := 10;

  // inizializza parametri animazione
  TimerBounce.Enabled := False;

  TimerBounce.Interval := 10;

  fVelocityY := 0;
  fGravity := 0.5;
  fMinY := ImgLogo.Top;

  // Imposta i limiti orizzontali di rimbalzo (40 pixel dai bordi)
  fMinX := 40;
  fMaxX := Self.ClientWidth - ImgLogo.Width - 40;

  // Inizializza la velocità orizzontale con un valore casuale
  Randomize;
  // Assegna una velocità iniziale casuale tra -2.0 e 2.0

  fVelocityX := (Random(30) - 10) / 5; // Nuova spinta casuale orizzontale

end;

procedure TfrmAbout.ImgLogoClick(Sender: TObject);
begin
  // reset posizione in alto

  ImgLogo.Top := 10;

  // Inizializza i parametri dell'animazione
  fVelocityY := 0;
  // Inizializza la velocità orizzontale con un valore casuale
  Randomize;
  // Assegna una velocità iniziale casuale tra -2.0 e 2.0

  fVelocityX := (Random(30) - 10) / 5; // Nuova spinta casuale orizzontale

  // Attiva il timer per avviare l'animazione
  TimerBounce.Enabled := True;
end;

procedure TfrmAbout.Label1Click(Sender: TObject);
var
  BrowserProcess: TProcess;
begin
  BrowserProcess := TProcess.Create(nil);
  try
    {$IFDEF MSWINDOWS}
    BrowserProcess.Executable := 'cmd';
    BrowserProcess.Parameters.Add('/c');
    BrowserProcess.Parameters.Add('start');
    BrowserProcess.Parameters.Add('https://www.rarlab.com/');
    {$ENDIF}

    {$IFDEF LINUX}
    BrowserProcess.Executable := 'xdg-open';
    BrowserProcess.Parameters.Add('https://www.rarlab.com/');
    {$ENDIF}

    {$IFDEF DARWIN}
 // DARWIN è la direttiva per macOS e iOS
    BrowserProcess.Executable := 'open';
    BrowserProcess.Parameters.Add('https://www.rarlab.com/');
    {$ENDIF}

    BrowserProcess.Execute;
  finally
    BrowserProcess.Free;
  end;
end;

procedure TfrmAbout.TimerBounceTimer(Sender: TObject);
var
  NewY, NewX: integer;
begin
  // --- Logica di movimento verticale (uguale a prima) ---
  fVelocityY := fVelocityY + fGravity;
  NewY := ImgLogo.Top + Trunc(fVelocityY);

  if (NewY + ImgLogo.Height) > Self.ClientHeight then
  begin
    ImgLogo.Top := Self.ClientHeight - ImgLogo.Height;
    fVelocityY := fVelocityY * -0.6;

    // Ferma l'animazione se la velocità verticale è quasi nulla
    if Abs(fVelocityY) < 1 then
    begin
      TimerBounce.Enabled := False;
      fVelocityY := 0;
      fVelocityX := 0; // Ferma anche il movimento orizzontale
      ImgLogo.Top := Self.ClientHeight - ImgLogo.Height;
      Exit;
    end;
  end
  else
  begin
    ImgLogo.Top := NewY;
  end;

  // --- Nuova logica di movimento e rimbalzo orizzontale ---
  NewX := ImgLogo.Left + Trunc(fVelocityX);

  // Controlla se l'immagine colpisce il bordo sinistro
  if NewX < fMinX then
  begin
    fVelocityX := Abs(fVelocityX); // Inverti la direzione
    NewX := fMinX;
  end
  // Controlla se l'immagine colpisce il bordo destro
  else if (NewX + ImgLogo.Width) > fMaxX then
  begin
    fVelocityX := -Abs(fVelocityX); // Inverti la direzione
    NewX := fMaxX - ImgLogo.Width;
  end;

  ImgLogo.Left := NewX;
end;

end.
