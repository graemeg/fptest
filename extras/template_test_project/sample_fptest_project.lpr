{ Sample FPTest project

  Lazarus IDE users
  -----------------
  If you are using the fpGUI GUI Test Runner (the only GUI Runner supported
  at the moment), remember to add fptest_fpgui.lpk to Project Inspector's
  Required Packages list. That way Lazarus IDE can find the FPTest source code.

  Any other users
  ---------------
  Remember to add the following paths to your Unit Search path settings:
     <fptest>/src
     <fptest>/src/fpGUI
     <fptest>/3rdparty/epiktimer
}
program sample_fptest_project;

{$Mode objfpc}{$H+}

// decide what test runner to use... GUI or Text
{.$Define TextRunner}
{$Define GUIRunner}


{$ifdef GuiRunner}
  {$apptype gui}
{$endif}


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF TextRunner}
  TextTestRunner,
  {$ENDIF}
  {$IFDEF GUIRunner}
  GUITestRunner,
  {$ENDIF}
  Classes, SysUtils,
  // the units containing the actual tests will be listed here
  SampleTests;

begin
  // Register all tests here
  SampleTests.RegisterTests;

  RunRegisteredTests;
end.


