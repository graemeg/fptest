program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  Forms, Interfaces, sample_tests, GuiTestRunner;

{$R *.res}

begin
  Application.Title := 'FPTest LCL GUI';
  // Register all tests
  sample_tests.RegisterTests;
  Application.Initialize;
  RunRegisteredTests;
end.

