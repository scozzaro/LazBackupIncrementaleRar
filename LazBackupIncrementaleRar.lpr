program lazbackupincrementalerar;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Interfaces,
  SysUtils, Forms, mainform, AboutForm;

{$R *.res}

var
  SplashForm: TfrmAbout;

begin
  RequireDerivedFormResource := True;
  Application.Title := 'lazbackupincrementalerar';
  Application.Scaled := True;
  Application.MainFormOnTaskbar := True;

  Application.Initialize;

  // === CREA LO SPLASH MANUALMENTE (NON COME FORM PRINCIPALE) ===
  SplashForm := TfrmAbout.Create(Application);
   SplashForm.BtnClose1.Visible:=false;
  SplashForm.ShowModal;

  // === IL CODICE ARRIVA QUI SOLO DOPO CHE LO SPLASH È CHIUSO ===
  SplashForm.Free;

  // === CREA IL FORM PRINCIPALE ===
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TfrmAbout, frmAbout);

  Application.Run;
end.
