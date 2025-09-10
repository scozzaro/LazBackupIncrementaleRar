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
    procedure BtnClose1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private

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
  close;
end;


function GetAppVersion: string;
var
  FileVerInfo: TFileVersionInfo;
  s: String;
begin
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    s := 'Company: ' + FileVerInfo.VersionStrings.Values['CompanyName'] + LineEnding +
         'File description: ' + FileVerInfo.VersionStrings.Values['FileDescription'] + lineEnding +
         'File version: ' + FileVerInfo.VersionStrings.Values['FileVersion'] + LineEnding +
         'Internal name: ' +FileVerInfo.VersionStrings.Values['InternalName'] + LineEnding +
         'Legal copyright: ' + FileVerInfo.VersionStrings.Values['LegalCopyright'] + LineEnding +
         'Original filename: ' + FileVerInfo.VersionStrings.Values['OriginalFilename'] + LineEnding +
         'Product name: ' + FileVerInfo.VersionStrings.Values['ProductName'] + LineEnding +
         'Product version: ' + FileVerInfo.VersionStrings.Values['ProductVersion'];
//   ShowMessage(s);
    GetAppVersion := FileVerInfo.VersionStrings.Values['FileVersion'] ;
  finally
    FileVerInfo.Free;
  end;
end;


procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Langrun.Caption:= GetSystemLanguageCode;

   if GetSystemLanguageCode = 'Italiano' then
  begin
    // Imposta le caption e le stringhe in italiano
    Self.Caption := 'Informazioni';
    ImgLogo.Hint := 'Logo del programma';
    LblAppName.Caption := 'Lazbackup Incrementale';
    LblVersion.Caption := 'Versione '+GetAppVersion; // Se la versione è dinamica, usa 'Versione ' + VersionNumber
    LblCopyright.Caption := 'Copyright © 2025 Vincenzo Scozzaro';
    LblAuthor.Caption := 'Autore: Vincenzo Scozzaro';
    LblLicenza.Caption := 'Licenza: Mozilla, gratuito anche per scopi commerciali';
    MemoTestoLicenza.Lines.Clear;
    MemoTestoLicenza.Lines.Add('Questa applicazione è distribuita sotto licenza Mozilla Public License.');
    MemoTestoLicenza.Lines.Add('È gratuita e può essere utilizzata anche per scopi commerciali.');
    MemoTestoLicenza.Lines.Add('Non è richiesta alcuna royalty o pagamento.');
    MemoTestoLicenza.Lines.Add('Ulteriori dettagli sulla licenza possono essere consultati su https://www.mozilla.org/MPL/');
    LblWinRARWarning.Caption := 'Avvertenza: Questo programma richiede l''installazione di RAR o WinRAR. Questi strumenti non sono forniti e devono essere scaricati e installati separatamente dall''utente.';
    BtnClose1.Caption := 'Chiudi';
    LangRun.Caption := 'Lingua di sistema';
  end else begin
       // Imposta le caption e le stringhe in inglese
    Self.Caption := 'About';
    ImgLogo.Hint := 'Program logo';
    LblAppName.Caption := 'Lazbackup Incremental';
    LblVersion.Caption := 'Version '+GetAppVersion; // If the version is dynamic, use 'Version ' + VersionNumber
    LblCopyright.Caption := 'Copyright © 2025 Vincenzo Scozzaro';
    LblAuthor.Caption := 'Author: Vincenzo Scozzaro';
    LblLicenza.Caption := 'License: Mozilla, free for commercial use';
    MemoTestoLicenza.Lines.Clear;
    MemoTestoLicenza.Lines.Add('This application is distributed under the Mozilla Public License.');
    MemoTestoLicenza.Lines.Add('It is free and can also be used for commercial purposes.');
    MemoTestoLicenza.Lines.Add('No royalty or payment is required.');
    MemoTestoLicenza.Lines.Add('Further details on the license can be found at https://www.mozilla.org/MPL/');
    LblWinRARWarning.Caption := 'Warning: This program requires the installation of RAR or WinRAR. These tools are not provided and must be downloaded and installed separately by the user.';
    BtnClose1.Caption := 'Close';
    LangRun.Caption := 'System Language';
  end;

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

    {$IFDEF DARWIN} // DARWIN è la direttiva per macOS e iOS
    BrowserProcess.Executable := 'open';
    BrowserProcess.Parameters.Add('https://www.rarlab.com/');
    {$ENDIF}

    BrowserProcess.Execute;
  finally
    BrowserProcess.Free;
  end;
end;

end.

