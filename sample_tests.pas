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
  Fail('Just because I can!');
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
  Fail('TClassA.TestClassA1');
end;

procedure TClassA.TestClassA2;
begin
  Fail('TClassA.TestClassA2');
end;

{ TClassB }

procedure TClassB.TestClassA2;
begin
//  inherited TestClassA2;
  sleep(2264);
  Fail('TClassB.TestClassA2');
end;

procedure TClassB.TestClassB1;
begin
  Fail('TClassB.testClassB1');
end;

end.

