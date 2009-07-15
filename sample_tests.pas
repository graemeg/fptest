unit sample_tests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework;

type
  TTestCaseFirst = class(TTestCase)
  published
    procedure TestOne;
    procedure TestTwo;
    procedure TestThree;
  end;

implementation

{ TTestCaseFirst }

procedure TTestCaseFirst.TestOne;
begin
  Check(1 + 1 = 3, 'Catastrophic arithmetic failure!');
end;

procedure TTestCaseFirst.TestTwo;
begin
  Check(1 + 1 = 2, 'Catastrophic arithmetic failure!');
end;

procedure TTestCaseFirst.TestThree;
var
  s: string;
begin
  s := 'hello';
  CheckEquals('Hello', s, 'Failed CheckEquals');
end;


initialization
  TestFramework.RegisterTest(TTestCaseFirst.Suite);

end.

