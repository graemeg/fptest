unit FPTestTestCaseOpts;

//Adapted from FPCUnit package

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, FPTestStrTestCaseOpts;

type

  { TFPTestTestCaseOptionsForm }

  TFPTestTestCaseOptionsForm = class(TForm)
    btnAccept: TButton;
    cbSetup: TCheckBox;
    cbTeardown: TCheckBox;
    edDefaultName: TEdit;
    gbFixture: TGroupBox;
    gbNames: TGroupBox;
    Label1: TLabel;
    procedure btnAcceptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 


implementation

{$R *.lfm}

{ TFPTestTestCaseOptionsForm }

procedure TFPTestTestCaseOptionsForm.btnAcceptClick(Sender: TObject);
begin
  Close;
end;

procedure TFPTestTestCaseOptionsForm.FormCreate(Sender: TObject);
begin
  Caption := sfrmTest;
  gbNames.Caption:= sgrpNames;
  gbFixture.Caption:= sgrpFixture;
  label1.Caption:= slblDefault;
  cbSetup.Caption:= schkSetup;
  cbTeardown.Caption:= schkTear;
  btnAccept.Caption:= sbtnCreate;
end;

end.

