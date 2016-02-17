program project1;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

{.$Define TextRunner}
{$Define GUIRunner}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sample_tests
  {$IFDEF TextRunner}
  ,TextTestRunner
  {$ENDIF}
  {$IFDEF GUIRunner}
  ,GuiTestRunner
  {$ENDIF}
  {$ifndef LCL}
  ,formimages
  {$endif}
  ;


begin
  // Register all tests
  sample_tests.RegisterTests;

  RunRegisteredTests;
end.

