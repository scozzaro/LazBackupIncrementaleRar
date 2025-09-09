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
  Forms, datetimectrls, mainform, AboutForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='Laz Backup Incrementale Rar';
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}

  Application.Initialize;
Application.CreateForm(TFrmMain, FrmMain);
Application.CreateForm(TfrmAbout, frmAbout);
  frmAbout.Show;
  frmAbout.BtnClose1.Visible:=false;

  // Forza il disegno dello splash screen prima del delay.
  Application.ProcessMessages;

  // Aggiungi un piccolo ritardo per permettere all'utente di vedere lo splash screen
  Sleep(3000);


  // Imposta esplicitamente frmMain come il form principale dell'applicazione.
  // Questo impedisce all'applicazione di terminare quando frmAbout viene chiuso.


  // Nascondi e libera la memoria dello splash screen.
  // Il programma non si chiuderà perché ora il form principale è frmMain.
  frmAbout.Hide;
  frmAbout.BtnClose1.Visible:=true;
  //frmAbout.Free;

  // Avvia il ciclo principale dell'applicazione, mostrando frmMain.
  Application.Run;
end.
