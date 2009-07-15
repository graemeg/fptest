unit sample_tests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework;

type
  TTestCaseFirst = class(TTestCase)
  published
    procedure TestWarning;
    procedure TestOne;
    procedure TestTwo;
    procedure TestThree;
  end;

  TClassA = class(TTestCase)
  published
    procedure TestClassA1;
    procedure TestClassA2; virtual;
  end;

  TClassB = class(TClassA)
  published
    procedure TestClassA2; override;
    procedure TestClassB1;
    procedure TestError;
  end;


procedure RegisterTests;


implementation

uses
  sysutils;


procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestCaseFirst.Suite);
  TestFramework.RegisterTest(TClassB.Suite);
end;

{ TTestCaseFirst }

procedure TTestCaseFirst.TestWarning;
begin
  // Do nothing here - should cause a Warning
end;

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

{ TClassA }

procedure TClassA.TestClassA1;
begin
  fail('TClassA.TestClassA1');
end;

procedure TClassA.TestClassA2;
begin
  Fail('This virtual method should never appear.');
end;

{ TClassB }

procedure TClassB.TestClassA2;
begin
  Fail('Test overridden method');
end;

procedure TClassB.TestClassB1;
begin
  sleep(2264);
  Fail('Test sleep() causing extra time');
end;

procedure TClassB.TestError;
var
  x, y: integer;
begin
  x := 10;
  y := 0;
  Check(x / y = 0, 'Failed on 1');
end;

end.

