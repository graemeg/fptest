unit SampleTests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework;

type
  TSampleTests = class(TTestCase)
  published
    procedure TestOne;
    procedure TestTwo;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TSampleTests.Suite);
end;

{ TSampleTests }

procedure TSampleTests.TestOne;
var
  b: boolean;
begin
  b := True;
  CheckEquals(True, b, 'Failed on 1');
  CheckNotEquals(False, b, 'Failed on 2');
end;

procedure TSampleTests.TestTwo;
var
  s: string;
begin
  s := 'hello';
  Check(s = 'hello', 'Failed on 1');
end;

end.

