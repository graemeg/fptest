program project1;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$Define TextRunner}
{$Define GUIRunner}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  Forms, Interfaces, sample_tests, GUITestRunner;


{$R *.res}

begin
  Application.Title := 'FPTest LCL GUI';
  // Register all tests
  sample_tests.RegisterTests;
  Application.Initialize;
  RunRegisteredTests;
end.

