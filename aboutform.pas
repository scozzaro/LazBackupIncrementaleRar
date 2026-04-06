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
  LCLIntf, LCLType, DynLibs,
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
    LibUse: TLabel;
    LblAppName: TLabel;
    LblAuthor: TLabel;
    LblLicenza: TLabel;
    LblCopyright: TLabel;
    LblVersion: TLabel;
    LblWinRARWarning: TLabel;
    MemoTestoLicenza: TMemo;
    TimerBounce: TTimer;
    TimerAutoClose: TTimer;  // Timer per la chiusura automatica
    procedure BtnClose1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ImgLogoClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure TimerBounceTimer(Sender: TObject);
    procedure TimerAutoCloseTimer(Sender: TObject);
  private
    fVelocityY: single;
    fGravity: single;
    fMinY: integer;
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
    pcLCA[0] := #0;
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

function GetAppVersion: string;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    Result := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

procedure TfrmAbout.TimerAutoCloseTimer(Sender: TObject);
begin
  TimerAutoClose.Enabled := False;
  Close;  // Chiude lo splash screen
end;

procedure TfrmAbout.BtnClose1Click(Sender: TObject);
begin

  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Application.ProcessMessages;
  BorderIcons := BorderIcons - [biMinimize, biMaximize];

  // Finestra non ridimensionabile
  BorderStyle := bsSingle;
end;

procedure TfrmAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrmAbout.FormShow(Sender: TObject);
var
  Info: string;

  function GetWidgetSet: string;
  begin
    Result := '';
    {$IFDEF LCLQT6} Result := 'Qt6'; {$ENDIF}
    {$IFDEF LCLQT5} Result := 'Qt5'; {$ENDIF}
    {$IFDEF LCLGTK2} Result := 'GTK2'; {$ENDIF}
    {$IFDEF LCLGTK3} Result := 'GTK3'; {$ENDIF}
    {$IFDEF LCLGTK4} Result := 'GTK4'; {$ENDIF}
    {$IFDEF LCLCARBON} Result := 'Carbon (macOS legacy)'; {$ENDIF}
    {$IFDEF LCLCOCOA} Result := 'Cocoa (macOS)'; {$ENDIF}
    {$IFDEF LCLWIN32} Result := 'Win32'; {$ENDIF}
    {$IFDEF LCLWIN64} Result := 'Win64'; {$ENDIF}
    if Result = '' then Result := 'Sconosciuto';
  end;

  function GetOS: string;
  begin
    {$IFDEF WINDOWS}
      Result := 'Windows';
    {$ELSE}
      {$IFDEF DARWIN}
        Result := 'macOS';
      {$ELSE}
        {$IFDEF UNIX}
          Result := 'Linux';
        {$ELSE}
          Result := 'Sconosciuto';
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  end;

  function GetArchitecture: string;
  begin
    {$IFDEF CPU32}
      Result := 'x86 (32-bit)';
    {$ELSE}
      {$IFDEF CPU64}
        Result := 'x86_64 (64-bit)';
      {$ELSE}
        {$IFDEF CPUARM}
          Result := 'ARM';
        {$ELSE}
          Result := 'Sconosciuta';
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  end;

  function GetCompileDate: string;
  begin
    Result := {$I %DATE%} + ' ' + {$I %TIME%};
  end;

  function GetTargetCPU: string;
  begin
    {$IFDEF CPU386}
      Result := 'i386';
    {$ELSE}
      {$IFDEF CPUX86_64}
        Result := 'x86_64';
      {$ELSE}
        {$IFDEF CPUARM}
          Result := 'ARM';
        {$ELSE}
          Result := 'Sconosciuta';
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  end;

begin
  // Imposta le informazioni tecniche
  Info := Format(
    'Compilato con: Free Pascal %s' + sLineBreak +
    'Target CPU: %s' + sLineBreak +
    'Architettura: %s' + sLineBreak +
    'WidgetSet: %s' + sLineBreak +
    'Sistema Operativo: %s' + sLineBreak +
    'Data compilazione: %s',
    [{$I %FPCVERSION%}, GetTargetCPU, GetArchitecture, GetWidgetSet, GetOS, GetCompileDate]
  );

  {$IFDEF LCLVERSION}
    Info := Info + sLineBreak + 'LCL: ' + LCLVERSION;
  {$ENDIF}

  LibUse.Caption := Info;
  Application.ProcessMessages;
  LangRun.Caption := GetSystemLanguageCode;

  // Impostazioni lingua
  if GetSystemLanguageCode = 'Italiano' then
  begin
    Self.Caption := 'Informazioni';
    ImgLogo.Hint := 'Logo del programma';
    LblAppName.Caption := 'Lazbackup Incrementale';
    LblVersion.Caption := 'Versione ' + GetAppVersion;
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
    Self.Caption := 'About';
    ImgLogo.Hint := 'Program logo';
    LblAppName.Caption := 'Lazbackup Incremental';
    LblVersion.Caption := 'Version ' + GetAppVersion;
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

  // Posiziona il logo
  ImgLogo.Top := 10;
  ImgLogo.Left := 10;

  // Inizializza parametri animazione
  TimerBounce.Enabled := False;
  TimerBounce.Interval := 10;

  fVelocityY := 0;
  fGravity := 0.5;
  fMinY := ImgLogo.Top;
  fMinX := 40;
  fMaxX := Self.ClientWidth - ImgLogo.Width - 40;

  Randomize;
  fVelocityX := (Random(30) - 10) / 5;

  Application.ProcessMessages;

  // === LOGICA DI CHIUSURA AUTOMATICA ===
  // Se il pulsante BtnClose1 non è visibile, avvia il timer per la chiusura automatica dopo 4 secondi
  if not BtnClose1.Visible then
  begin
    TimerAutoClose := TTimer.Create(Self);
    TimerAutoClose.Interval := 4000;  // 4 secondi
    TimerAutoClose.OnTimer := @TimerAutoCloseTimer;
    TimerAutoClose.Enabled := True;
  end;
   Self.Caption := 'Informazioni';
end;

procedure TfrmAbout.ImgLogoClick(Sender: TObject);
begin

  ImgLogo.Top := 10;
  fVelocityY := 0;
  Randomize;
  fVelocityX := (Random(30) - 10) / 5;
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
  fVelocityY := fVelocityY + fGravity;
  NewY := ImgLogo.Top + Trunc(fVelocityY);

  if (NewY + ImgLogo.Height) > Self.ClientHeight then
  begin
    ImgLogo.Top := Self.ClientHeight - ImgLogo.Height;
    fVelocityY := fVelocityY * -0.6;

    if Abs(fVelocityY) < 1 then
    begin
      TimerBounce.Enabled := False;
      fVelocityY := 0;
      fVelocityX := 0;
      ImgLogo.Top := Self.ClientHeight - ImgLogo.Height;
      Exit;
    end;
  end
  else
  begin
    ImgLogo.Top := NewY;
  end;

  NewX := ImgLogo.Left + Trunc(fVelocityX);

  if NewX < fMinX then
  begin
    fVelocityX := Abs(fVelocityX);
    NewX := fMinX;
  end
  else if (NewX + ImgLogo.Width) > fMaxX then
  begin
    fVelocityX := -Abs(fVelocityX);
    NewX := fMaxX - ImgLogo.Width;
  end;

  ImgLogo.Left := NewX;
end;

end.
