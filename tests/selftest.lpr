program selftest;

{$mode objfpc}{$H+}

{$Define TextRunner}
{.$Define GUIRunner}

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
  MiniTestSuite,
  MiniTestSuite2,
  UnitSingleTest,
  {$IFDEF TextRunner}
  TextTestRunner,
  {$ENDIF}
  {$IFDEF GUIRunner}
  RefGUITestRunner,
  {$ENDIF}
  UnitTestFramework,
  UnitTestFrameworkProxy;


begin
  RunRegisteredTests;
end.

