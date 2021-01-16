unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls,
  debug_srv;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnConnect: TButton;
    gbxControls: TGroupBox;
    gbxData: TGroupBox;
    edtLocalAddress: TLabeledEdit;
    edtRemoteAddress: TLabeledEdit;
    edtPort: TLabeledEdit;
    lblStatus: TLabel;
    stbNotifications: TStatusBar;
    procedure btnConnectClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  case btnConnect.Tag of
    0: begin
         btnConnect.Caption:= 'Stop';
         lblStatus.Font.Color:= clLime;
         lblStatus.Caption:= 'Running';
         btnConnect.Tag:= 1;
         stbNotifications.SimpleText:= 'Serving...';
     //    StartServer(Handle);
       end;
    1: begin
         btnConnect.Caption:= 'Start';
         lblStatus.Font.Color:= clRed;
         lblStatus.Caption:= 'Stopped';
         btnConnect.Tag:= 0;
         stbNotifications.SimpleText:= 'Waiting...';
      //   StopServer;
       end;
  end;
end;

end.

