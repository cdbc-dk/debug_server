unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls,
  LMessages,
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
    Memo1: TMemo;
    stbNotifications: TStatusBar;
    procedure btnConnectClick(Sender: TObject);
  private
    fDebugDaemon: TTCPDebugDaemon;
    procedure LMCreate(var Message: TLMessage); message LM_CREATE;
    procedure LMListen(var Message: TLMessage); message LM_LISTEN;
    procedure LMAccept(var Message: TLMessage); message LM_ACCEPT;
    procedure LMWorking(var Message: TLMessage); message LM_WORKING;
    procedure LMDone(var Message: TLMessage); message LM_DONE;
    procedure LMDestroy(var Message: TLMessage); message LM_DESTROY;
  public
    procedure StartServer;
    procedure StopServer;
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
         Application.ProcessMessages;
         StartServer;
       end;
    1: begin
         btnConnect.Caption:= 'Start';
         lblStatus.Font.Color:= clRed;
         lblStatus.Caption:= 'Stopped';
         btnConnect.Tag:= 0;
         stbNotifications.SimpleText:= 'Waiting...';
         Application.ProcessMessages;
         StopServer;
       end;
  end;
end;

procedure TfrmMain.LMCreate(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMListen(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMAccept(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMWorking(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMDone(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.LMDestroy(var Message: TLMessage);
begin
  Memo1.Lines.Add(inttostr(Message.WParam)+': '+string(pchar(Message.LParam)));
end;

procedure TfrmMain.StartServer;
begin
  fDebugDaemon:= TTCPDebugDaemon.Create(Handle);
end;

procedure TfrmMain.StopServer;
begin
  fDebugDaemon.Terminate;
  fDebugDaemon.WaitFor;
  FreeAndNil(fDebugDaemon);
end;

end.

