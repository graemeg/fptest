program selftest;

{$mode objfpc}{$H+}

{.$Define TextRunner}
{$Define GUIRunner}

{$IFDEF TextRunner}
  {$IFDEF Windows}
    {$APPTYPE CONSOLE}
  {$ENDIF}
{$ENDIF}


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
TestFrameworkIfaces,
TestFramework,
TestFrameworkProxyIfaces,
TestListenerIface,
ProjectsManagerIface,
ProjectsManager,
TestFrameworkProxy,
RefTestFrameworkProxy,
RefTestFramework,
RefProjectsManager,
SharedTestClasses,
  UnitTestFramework,
//  UnitTestModules
  UnitTestFrameworkProxy,
  MiniTestSuite,
  MiniTestSuite2,
  UnitSingleTest,
  {$IFDEF TextRunner}
  TextTestRunner;
  {$ENDIF}
  {$IFDEF GUIRunner}
  RefGUITestRunner;
  {$ENDIF}


begin
  // Register all tests
//  sample_tests.RegisterTests;

  RunRegisteredTests;
end.

