program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sample_tests;



var
  t: TTestCaseFirst;
begin
  t := TTestCaseFirst.Create;
  try
    t.TestOne;
    t.TestTwo;
    t.TestThree;
  finally
    t.free;
  end;
end.

