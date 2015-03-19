program showoverrides;

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
  {$IFDEF TextRunner}
  TextTestRunner,
  {$ENDIF}
  {$IFDEF GUIRunner}
  RefGUITestRunner,
  {$ENDIF}
  CheckOverrides;


begin
  RunRegisteredTests;
end.

