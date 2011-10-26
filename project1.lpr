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
  ,GUITestRunner
  {$ENDIF}
  ;


{.$Define RunManually}

{$IFDEF RunManually}
var
  t: TTestCaseFirst;
begin
  t := TTestCaseFirst.Create;
  try
    t.TestWarning;
    t.TestOne;
    t.TestTwo;
    t.TestThree;
  finally
    t.free;
  end;
{$ELSE}
begin
  // Register all tests
  sample_tests.RegisterTests;

  RunRegisteredTests;
{$ENDIF}
end.

