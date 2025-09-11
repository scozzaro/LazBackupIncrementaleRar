program LazBackupIncrementaleRar;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, BaseUnix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  SysUtils,
  Forms,
  datetimectrls,
  mainform,
  AboutForm;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'Laz Backup Incrementale Rar';
  Application.Scaled := True;
  {$PUSH}
  {$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}

  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  frmAbout.Show;
  frmAbout.BtnClose1.Visible := False;

  // Forza il disegno dello splash screen prima del delay.
  Application.ProcessMessages;

  // Aggiungi un piccolo ritardo per permettere all'utente di vedere lo splash screen
  Sleep(3000);


  // Nascondi e libera la memoria dello splash screen.
  // Il programma non si chiuderà perché ora il form principale è frmMain.
  frmAbout.Hide;
  frmAbout.BtnClose1.Visible := True;


  // Avvia il ciclo principale dell'applicazione, mostrando frmMain.
  Application.Run;
end.
