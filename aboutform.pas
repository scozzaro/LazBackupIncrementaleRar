unit AboutForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  RichMemo, ShellAPI;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    BtnClose1: TButton;
    ImgLogo: TImage;
    LblAppName: TLabel;
    LblAuthor: TLabel;
    LblLicenza: TLabel;
    LblCopyright: TLabel;
    LblVersion: TLabel;
    LblWinRARWarning: TLabel;
    MemoTestoLicenza: TMemo;
    RichMemo1: TRichMemo;
    procedure BtnClose1Click(Sender: TObject);
    procedure LblLicenzaClick(Sender: TObject);
    procedure LblWinRARWarningClick(Sender: TObject);
  private

  public

  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.BtnClose1Click(Sender: TObject);
begin
  close;
end;
procedure TfrmAbout.LblLicenzaClick(Sender: TObject);
begin
  if ShellExecute(0, PChar('open'),PChar('https://www.mozilla.org/MPL/2.0/'), nil, nil,1) =0 then;
end;

procedure TfrmAbout.LblWinRARWarningClick(Sender: TObject);


 var
  RTFStream: TFileStream;
begin
  // Crea un TFileStream per leggere il file RTF

  RTFStream := TFileStream.Create('licenza.rtf', fmOpenRead or fmShareDenyWrite);
  try
    // Carica il contenuto del flusso nel componente RichMemo
    RichMemo1.LoadRichText(RTFStream);
  finally
    // Libera la memoria del TFileStream
    RTFStream.Free;
  end;
end;


end.

