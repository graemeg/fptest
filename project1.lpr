program project1;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

// Text or Gui project?
{$Define TextRunner}
{.$Define GUIRunner}

// Optional extra when using GUI Runner with fpGUI Toolkit
{.$Define FPGUI}

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
  {$ifdef FPGUI}
  ,formimages
  {$endif}
  ;


begin
  // Register all tests
  sample_tests.RegisterTests;

  RunRegisteredTests;
end.

