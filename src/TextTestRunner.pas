{ $Id: TextTestRunner.pas,v 1.28 2004/10/17 10:39:00 neuromancer Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.28 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2008.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 * Peter McNab <mcnabp@gmail.com>
 *
 *)

unit TextTestRunner;
{$IFDEF CLR}
  {$UNDEF FASTMM}
{$ENDIF}

{$UNDEF XMLLISTENER}
{$IFNDEF VER130}
  {$IFNDEF VER140}
    {$IFNDEF CLR}
      {$DEFINE XMLLISTENER}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

interface
uses
  Classes,
  TestFrameworkProxyIfaces;

const
  rcs_id :string = '#(@)$Id: TextTestRunner.pas,v 1.28 2004/10/17 10:39:00 neuromancer Exp $';

type
  TRunnerExitBehavior = (
    rxbContinue,
    rxbPause,
    rxbHaltOnFailures
    );

{ TTExtTestListener }
type
  TTextTestListener = class(TInterfacedObject, ITestListener, ITestListenerX)
  private
    FErrors: IInterfaceList;
    FFailures: IInterfaceList;
    FWarnings: IInterfaceList;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FRunTime: TDateTime;
    function FailedTestInfo(const AFailure: TTestFailure): string;
    class function IniFileName: string;
//    class function RunTest(Suite: ITestProxy; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult; overload;
//    class function RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
  protected
    property Errors: IInterfaceList read FErrors;
    property Failures: IInterfaceList read FFailures;
    property Warnings: IInterfaceList read FWarnings;
    // implement the ITestListener interface
    procedure AddSuccess(Test: ITestProxy); virtual;
    procedure AddError(Error: TTestFailure); virtual;
    procedure AddFailure(Failure: TTestFailure); virtual;
    procedure AddWarning(AWarning: TTestFailure); virtual;
    function  ShouldRunTest(const ATest :ITestProxy):boolean; virtual;
    procedure StartSuite(Suite: ITestProxy); virtual;
    procedure EndSuite(Suite: ITestProxy); virtual;
    procedure StartTest(Test: ITestProxy); virtual;
    procedure EndTest(Test: ITestProxy); virtual;
    procedure TestingStarts; virtual;
    procedure TestingEnds(TestResult: TTestResult); virtual;
    procedure Status(const ATest: ITestProxy; AMessage: string);
    function  Report(r: TTestResult): string;
    function  PrintErrors(r: TTestResult): string; virtual;
    function  PrintFailures(r: TTestResult): string; virtual;
    function  PrintWarnings(r: TTestResult): string; virtual;
    function  PrintHeader(r: TTestResult): string; virtual;
    function  PrintSettings(r: TTestResult): string; virtual;
    function  PrintWarningItems(r: TTestResult): string; virtual;
    function  PrintFailureItems(r :TTestResult): string; virtual;
    function  PrintErrorItems(r :TTestResult): string; virtual;
    property  StartTime: TDateTime read FStartTime write FStartTime;
    property  EndTime: TDateTime read FEndTime write FEndTime;
    property  RunTime: TDateTime read FRunTime write FRunTime;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  {: This type defines what the RunTest and RunRegisteredTests methods will do when
     testing has ended.
     @enum rxbContinue Just return the TestResult.
     @enum rxbPause    Pause with a ReadLn before returnng the TestResult.
     @enum rxbHaltOnFailures   Halt the program if errors or failures occurred, setting
                               the program exit code to FailureCount+ErrorCount;
                               behave like rxbContinue if all tests suceeded.
     @seeAlso <See Unit="TextTestRunner" Routine="RunTest">
     @seeAlso <See Unit="TextTestRunner" Routine="RunRegisteredTests">
     }

{: Run the given Test Suite
}
function RunTest(Suite: ITestProxy; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult; overload;
function RunRegisteredTests: TTestResult; overload;
function RunRegisteredTests(const AExitBehavior: TRunnerExitBehavior): TTestResult; overload;

implementation
uses
  {$IFDEF SELFTEST}
  RefTestFrameworkProxy,
  {$ELSE}
  TestFrameworkProxy,
  {$ENDIF}
  {$IFDEF XMLLISTENER}
    XMLListener,
  {$ENDIF}
  XPVistaSupport,
  SysUtils;

const
  CRLF = #13#10;

class function TTextTestListener.IniFileName: string;
const
  TEST_INI_FILE = 'dunit.ini';
begin
  result := LocalAppDataPath + TEST_INI_FILE;
end;

procedure TTextTestListener.AddSuccess(Test: ITestProxy);
begin
// No display for successes
end;

constructor TTextTestListener.Create;
begin
  inherited Create;
  FErrors := TInterfaceList.Create;
  FFailures := TInterfaceList.Create;
  FWarnings := TInterfaceList.Create;
end;

destructor TTextTestListener.Destroy;
begin
  FWarnings := nil;
  FErrors := nil;
  FFailures := nil;
  inherited;
end;

procedure TTextTestListener.AddError(Error: TTestFailure);
begin
  FErrors.Add(Error);
  write('E');
end;

procedure TTextTestListener.AddFailure(Failure: TTestFailure);
begin
  FFailures.Add(Failure);
  write('F');
end;

procedure TTextTestListener.AddWarning(AWarning: TTestFailure);
begin
  FWarnings.Add(AWarning);
  write('W');
end;

{:
   Prints failures to the standard output
 }
function TTextTestListener.Report(r: TTestResult): string;
var
  LHeader: string;
  LErrors: string;
  LFailures: string;
  LWarnings: string;
begin
  LHeader:= PrintHeader(r);
  LErrors:= PrintErrors(r);
  LFailures:= PrintFailures(r);
  LWarnings:= PrintWarnings(r);

  Result := LHeader +
            LErrors +
            LFailures +
            LWarnings;
end;

function TTextTestListener.FailedTestInfo(const AFailure: TTestFailure): string;
begin
  Result := format('%s: %s'#13#10'     at %s'#13#10'      "%s"',
                    [
                    AFailure.failedTest.ParentPath + '.' + AFailure.failedTest.name,
                    AFailure.thrownExceptionName,
                    AFailure.LocationInfo,
                    AFailure.thrownExceptionMessage
                    ]) + CRLF + CRLF;
end;

function TTextTestListener.PrintWarningItems(r :TTestResult): string;
var
  i: Integer;
  Failure: TTestFailure;
begin
  Result := '';
  for i := 0 to FWarnings.Count-1 do
  begin
    Failure := FWarnings.Items[i] as TTestFailure;
    Result := Result + format('%3d) ', [i+1]) + FailedTestInfo(Failure);
  end;
end;

function TTextTestListener.PrintFailureItems(r :TTestResult): string;
var
  i: Integer;
  Failure: TTestFailure;
begin
  Result := '';
  for i := 0 to FFailures.Count-1 do
  begin
    Failure := FFailures.Items[i] as TTestFailure;
    Result := Result + format('%3d) ', [i+1]) + FailedTestInfo(Failure);
  end;
end;

function TTextTestListener.PrintErrorItems(r :TTestResult): string;
var
  i: Integer;
  Failure: TTestFailure;
begin
  Result := '';
  for i := 0 to FErrors.Count-1 do
  begin
    Failure := FErrors.Items[i] as TTestFailure;
    Result := Result + format('%3d) ', [i+1]) + FailedTestInfo(Failure);
  end;
end;

{:
   Prints the errors to the standard output
 }
function TTextTestListener.PrintErrors(r: TTestResult): string;
begin
  Result := '';
  if (r.ErrorCount <> 0) then begin
    if (r.ErrorCount = 1) then
      Result := Result + format('There was %d error:', [r.ErrorCount]) + CRLF
    else
      Result := Result + format('There were %d errors:', [r.ErrorCount]) + CRLF;

    Result := Result + CRLF + PrintErrorItems(r);
    Result := Result + CRLF;
  end
end;

{:
   Prints failures to the standard output
}
function TTextTestListener.PrintFailures(r: TTestResult): string;
begin
  Result := '';
  if (r.FailureCount <> 0) then begin
    if (r.FailureCount = 1) then
      Result := Result + format('There was %d failure:', [r.FailureCount]) + CRLF
    else
      Result := Result + format('There were %d failures:', [r.FailureCount]) + CRLF;

    Result := Result + CRLF + PrintFailureItems(r);
    Result := Result + CRLF;
  end
end;

{:
   Prints warnings to the standard output
}
function TTextTestListener.PrintWarnings(r: TTestResult): string;
begin
  Result := '';
  if (r.WarningCount <> 0) then begin
    if (r.WarningCount = 1) then
      Result := Result + format('There was %d warning:', [r.WarningCount]) + CRLF
    else
      Result := Result + format('There were %d warnings:', [r.WarningCount]) + CRLF;

    Result := Result + CRLF + PrintWarningItems(r);
    Result := Result + CRLF;
  end
end;

{:
   Prints the setting used
 }
function TTextTestListener.PrintSettings(r: TTestResult): string;
begin
  Result := '';
  if RegisteredTests = nil then
    Exit;

  if r.FailsIfNoChecksExecuted then
    Result := Result + 'Test fails if Check() not executed in test' + CRLF;
{$IFDEF FASTMM}
  if r.FailsIfMemoryLeaked then
  begin
    if r.IgnoresMemoryLeakInSetUpTearDown then
      Result := Result + 'Test fails if memory leak detected in test method excluding SetUp and TearDown' + CRLF
    else
      Result := Result + 'Test fails if memory leak detected in test' + CRLF;
  end;
{$ENDIF}
  Result := Result + CRLF;
end;

{:
   Prints the header of the Report
 }
function TTextTestListener.PrintHeader(r: TTestResult): string;
begin
  Result := '';
  if r.wasSuccessful then
  begin
    Result := Result + CRLF;
    Result := Result + PrintSettings(r);
    Result := Result + format('OK: %d tests'+CRLF, [r.RunCount]);
  end
  else
  begin
    Result := Result + CRLF;
    Result := Result + PrintSettings(r);
    Result := Result + 'Test Results:'+CRLF;
    Result := Result + format('Run:      %8d'+CRLF+'Errors:   %8d'+CRLF+'Failures: %8d'+CRLF+'Warnings: %8d'+CRLF,
                      [r.RunCount, r.ErrorCount, r.FailureCount, r.WarningCount]
                      ) + CRLF;
  end
end;

procedure TTextTestListener.StartTest(Test: ITestProxy);
begin
  if Test.IsTestMethod then
    write('.');
end;

procedure TTextTestListener.EndTest(Test: ITestProxy);
begin
// Nothing to do here
end;

procedure TTextTestListener.TestingStarts;
begin
  writeln;
  writeln('DUnit2 / Testing');
  FStartTime := now;
end;

procedure TTextTestListener.TestingEnds(TestResult: TTestResult);
var
  H, M, S, L :Word;
begin
  FEndTime := now;
  FRunTime := FEndTime - FStartTime;
  writeln;
  DecodeTime(FRunTime, H,  M, S, L);
  writeln(Format('Time: %d:%2.2d:%2.2d.%d', [H, M, S, L]));
  writeln(Report(TestResult));
end;

(*
class function TTextTestListener.RunTest(Suite: ITestProxy; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Suite.LoadConfiguration(TTextTestListener.IniFileName, False, True);
  Result := Suite.Run([TTextTestListener.Create
    {$IFDEF XMLLISTENER}, TXMLListener.Create(ParamStr(0){, 'type="text/xsl" href="fpcunit2.xsl"'}) {$ENDIF}]);
  Result.ReleaseListeners;
  case exitBehavior of
    rxbPause:
      try
        writeln('Press <RETURN> to continue.');
        readln
      except
      end;
    rxbHaltOnFailures:
{$IFNDEF CLR}
      with Result do
      begin
        if not WasSuccessful then
          System.Halt(ErrorCount+FailureCount);
      end
{$ENDIF}
    // else fall through
  end;
end;

class function TTextTestListener.RunRegisteredTests(exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Result := RunTest(RegisteredTests, exitBehavior);
end;
*)

function RunTest(Suite: ITestProxy; exitBehavior: TRunnerExitBehavior = rxbContinue): TTestResult;
begin
  Result := nil;
  try
    if Suite = nil then
      writeln('No tests registered')
    else
    try
    Suite.LoadConfiguration(TTextTestListener.IniFileName, False, True);
      Result := RunTest(Suite,[TTextTestListener.Create
      {$IFDEF XMLLISTENER}, TXMLListener.Create(LocalAppDataPath + Suite.Name{, 'type="text/xsl" href="fpcunit2.xsl"'}) {$ENDIF}]);
    finally
      Result.ReleaseListeners;
      Suite.ReleaseTests;
    end;
  finally
    case exitBehavior of
      rxbPause:
        begin
          writeln('Press <RETURN> to continue.');
          readln;
        end;
      rxbHaltOnFailures:
      {$IFNDEF CLR}
        if Assigned(Result) then
        with Result do
        begin
          if not WasSuccessful then
            System.Halt(ErrorCount+FailureCount);
        end
      {$ENDIF}
    end;
  end;
end;

function RunRegisteredTests: TTestResult;
var
  LExitBehavior : TRunnerExitBehavior;
begin

  // To run with rxbPause, use -p switch
  // To run with rxbHaltOnFailures, use -h switch
  // No switch runs as rxbContinue
  if FindCmdLineSwitch('p', ['-', '/'], true) then
    LExitBehavior := rxbPause
  else if FindCmdLineSwitch('h', ['-', '/'], true) then
    LExitBehavior := rxbHaltOnFailures
  else
    LExitBehavior := rxbContinue;

  Result := RunTest(RegisteredTests, LExitBehavior);

end;

function RunRegisteredTests(const AExitBehavior: TRunnerExitBehavior): TTestResult;
begin
  Result := RunTest(RegisteredTests, AExitBehavior);
end;

{---------------}
procedure TTextTestListener.Status(const ATest: ITestProxy; AMessage: string);
begin
  writeln(Format('%s: %s', [ATest.Name, AMessage]));
end;

function TTextTestListener.ShouldRunTest(const ATest :ITestProxy):boolean;
begin
  Result := not ATest.Excluded ; // Call here for every enabled Test.
  if not Result then
    Write('x');
end;

procedure TTextTestListener.EndSuite(Suite: ITestProxy);
begin
// Nothing to do here
end;

procedure TTextTestListener.StartSuite(Suite: ITestProxy);
begin
// Nothing to do here
end;

end.
