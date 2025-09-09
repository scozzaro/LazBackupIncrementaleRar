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

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  Langrun.Caption:= GetSystemLanguageCode;
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

